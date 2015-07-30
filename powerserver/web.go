package main

import (
	"compress/gzip"
	"html/template"
	"io"
	"log"
	"net/http"
	"os"
	"path"
	"sort"
	"strings"
	"time"

	"github.com/dustin/powerlab"
)

var tmpl = template.Must(template.New("").Parse(`<html>
  <head>
    <title>Powerlab Log List</title>
  </head>

  <body>
    <h1>Powerlab Logs</h1>

    <table>
      <tr>
        <thead>
          <th>Logfile</th>
          <th>Size</th>
          <th>Date</th>
        </thead>

        <tbody>
          {{range .}}
            <tr>
              <td>
                <a href="/logs/{{.Name}}">{{.Name}}</a>
                <a href="/logs/{{.Name}}?fmt=csv">(csv)</a>
                <a href="/logs/{{.Name}}?fmt=csvlong">(csvlong)</a>
              </td>
              <td>{{.Size}}</td>
              <td>{{.ModTime}}</td>
            </tr>
          {{end}}
        </tbody>
      </tr>
    </table>
  </body>
</html>`))

type logHandler struct{}

func showLog(w http.ResponseWriter, r *http.Request) {
	log.Printf("Fetching log %v", r.URL.Path)

	f, err := os.Open(path.Join(*logpath, r.URL.Path))
	if err != nil {
		http.Error(w, err.Error(), 404)
		return
	}
	defer f.Close()

	gzr, err := gzip.NewReader(f)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	lr := io.ReadCloser(gzr)
	defer lr.Close()

	if strings.HasPrefix(r.FormValue("fmt"), "csv") {
		w.Header().Set("Content-type", "text/csv")

		ls, err := powerlab.NewLogReaderStream(gzr, *logFormat)
		if err != nil {
			http.Error(w, err.Error(), 500)
			return
		}
		defer ls.Close()

		switch r.FormValue("fmt") {
		case "csv":
			lr = powerlab.NewWideCSVReader(ls)
		case "csvlong":
			lr = powerlab.NewLongCSVReader(ls)
		}

	}

	g := newGzippingWriter(w, r)
	defer g.Close()

	io.Copy(g, lr)
}

type dsfio []os.FileInfo

func (d dsfio) Len() int           { return len(d) }
func (d dsfio) Less(i, j int) bool { return d[i].Name() > d[j].Name() }
func (d dsfio) Swap(i, j int)      { d[i], d[j] = d[j], d[i] }

type jfio struct {
	Name    string    `json:"name"`
	Size    int64     `json:"size"`
	ModTime time.Time `json:"mtime"`
}

func (logHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f, err := os.Open(*logpath)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	defer f.Close()

	if len(r.URL.Path) > 1 {
		showLog(w, r)
		return
	}

	o, err := f.Readdir(0)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	sort.Sort(dsfio(o))

	g := newGzippingWriter(w, r)
	defer g.Close()

	if r.FormValue("format") == "json" {
		oe := []jfio{}
		for _, e := range o {
			oe = append(oe, jfio{e.Name(), e.Size(), e.ModTime()})
		}
		currentLog := currentLog.String()
		currentLog = currentLog[1 : len(currentLog)-1]
		if currentLog != "" {
			currentLog = path.Base(currentLog)
		}
		serveJSON(w, r, map[string]interface{}{
			"current": currentLog,
			"entries": oe,
		})
		return
	}

	w.Header().Set("Content-type", "text/html")

	if err := tmpl.Execute(g, o); err != nil {
		log.Printf("Error rendering template: %v", err)
	}
}
