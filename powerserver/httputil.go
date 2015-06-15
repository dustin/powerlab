package main

import (
	"compress/gzip"
	"encoding/json"
	"io"
	"net/http"
	"strings"
)

func canGzip(req *http.Request) bool {
	acceptable := req.Header.Get("accept-encoding")
	return strings.Contains(acceptable, "gzip")
}

type gzippingWriter struct {
	gz     *gzip.Writer
	output io.Writer
}

func newGzippingWriter(w http.ResponseWriter, req *http.Request) *gzippingWriter {
	rv := &gzippingWriter{output: w}
	if canGzip(req) {
		w.Header().Set("Content-Encoding", "gzip")
		rv.gz = gzip.NewWriter(w)
		rv.output = rv.gz
	}
	return rv
}

func (g *gzippingWriter) Write(b []byte) (int, error) {
	return g.output.Write(b)
}

func (g *gzippingWriter) Close() error {
	if g.gz != nil {
		return g.gz.Close()
	}
	return nil
}

func serveJSON(w http.ResponseWriter, req *http.Request, thing interface{}) error {
	w.Header().Set("Content-Type", "application/json")

	out := newGzippingWriter(w, req)
	defer out.Close()

	return json.NewEncoder(out).Encode(thing)
}
