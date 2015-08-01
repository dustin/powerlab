package main

import (
	"encoding/json"
	"flag"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"path"
	"time"

	"github.com/cespare/wait"
	"github.com/dustin/httputil"
)

var (
	concurrency = flag.Int("concurrency", 4, "Maximum concurrent number of fetches.")
	httpTimeout = flag.Duration("timeout", time.Minute, "HTTP timeout")
)

func listRemote(baseurl string) (map[string]int64, error) {
	u, err := url.Parse(baseurl + "/logs/?format=json")
	if err != nil {
		return nil, err
	}

	res, err := http.Get(u.String())
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()
	r := struct {
		Current string
		Entries []struct {
			Name    string
			Size    int64
			ModTime time.Time
		}
	}{}

	if err := json.NewDecoder(res.Body).Decode(&r); err != nil {
		return nil, err
	}

	rv := map[string]int64{}
	for _, x := range r.Entries {
		if r.Current == x.Name {
			continue
		}
		rv[x.Name] = x.Size
	}

	return rv, nil
}

func listLocal(logdir string) (map[string]int64, error) {
	f, err := os.Open(logdir)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	stuff, err := f.Readdir(-1)
	if err != nil {
		return nil, err
	}

	rv := map[string]int64{}
	for _, x := range stuff {
		rv[x.Name()] = x.Size()
	}

	return rv, nil
}

func process(dest, baseurl string, local, remote map[string]int64) error {
	g := wait.Group{}

	sem := make(chan bool, *concurrency)

	client := &http.Client{
		Transport: &http.Transport{
			DisableCompression: true,
		},
		Timeout: *httpTimeout,
	}

	for k, v := range remote {
		if local[k] == v {
			continue
		}

		k := k
		v := v
		g.Go(func(<-chan struct{}) error {
			sem <- true
			defer func() { <-sem }()

			log.Printf("Fetching %v (we have %v, upstream has %v)", k, local[k], v)
			u, err := url.Parse(baseurl + "/logs/" + k)
			if err != nil {
				return err
			}

			req, err := http.NewRequest("GET", u.String(), nil)
			if err != nil {
				log.Printf("Error fetching %v: %v", u, err)
				return err
			}

			res, err := client.Do(req)
			if err != nil || res.StatusCode != 200 {
				if err == nil {
					err = httputil.HTTPError(res)
				}
				log.Printf("Error fetching %v: %v", u, err)
				return err
			}
			defer res.Body.Close()

			f, err := os.Create(path.Join(dest, k))
			if err != nil {
				log.Printf("Error creating output file %v: %v", k, err)
				return err
			}
			defer f.Close()

			_, err = io.Copy(f, res.Body)
			if err != nil {
				log.Printf("Error copying data for %v: %v", k, err)
			}
			return err
		})
	}

	return g.Wait()
}

func main() {
	flag.Parse()
	if flag.NArg() < 2 {
		log.Fatalf("Usage: %v baseurl destdir", os.Args[0])
	}

	http.DefaultClient.Timeout = *httpTimeout
	httputil.InitHTTPTracker(false)

	baseurl := flag.Arg(0)
	dest := flag.Arg(1)

	var local, remote map[string]int64

	g := wait.Group{}
	g.Go(func(<-chan struct{}) (err error) {
		remote, err = listRemote(baseurl)
		return err
	})
	g.Go(func(<-chan struct{}) (err error) {
		local, err = listLocal(dest)
		return err
	})
	if err := g.Wait(); err != nil {
		log.Fatalf("Error fetching stuff: %v", err)
	}

	if err := process(dest, baseurl, local, remote); err != nil {
		log.Fatalf("Error processing stuff: %v", err)
	}
}
