package main

import (
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/dustin/powerlab"
)

func BenchmarkJSONMarshaling(b *testing.B) {
	ls, err := powerlab.NewLogReader("../sample/2015-07-17T21:35:36-07:00.gob.gz")
	if err != nil {
		b.Fatal(err)
	}
	defer ls.Close()

	statuses := []*powerlab.Status{}
	for {
		e, err := ls.Next()
		if err == io.EOF {
			break
		} else if err != nil {
			b.Fatal(err)
		}
		statuses = append(statuses, e.Data)
	}

	out := statuses
	for b.N > len(out) {
		out = append(out, statuses...)
	}
	if len(out) > b.N {
		out = out[:b.N]
	}

	w := httptest.NewRecorder()
	req := &http.Request{
		Header: http.Header{
			"accept-encoding": []string{"gzip"},
		},
	}

	b.ResetTimer()
	if err := serveJSON(w, req, out); err != nil {
		b.Fatal(err)
	}
}
