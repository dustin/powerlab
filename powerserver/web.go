package main

import (
	"expvar"
	"log"
	"net/http"
	"time"

	"github.com/dustin/httputil"
	"github.com/dustin/seriesly/timelib"
)

func initWeb() {
	expvar.Publish("httpclients", httputil.InitHTTPTracker(false))

	http.HandleFunc("/status", func(w http.ResponseWriter, r *http.Request) {
		if err := serveJSON(w, r, getCurrent()); err != nil {
			log.Printf("Error serving status json: %v", err)
		}
	})
	http.HandleFunc("/statuses", func(w http.ResponseWriter, r *http.Request) {
		after, err := timelib.ParseTime(r.FormValue("after"))
		if err != nil {
			after = time.Time{}
		}
		rv := []markedStatus{}
		for _, ms := range allReadings() {
			if ms.ST != nil && ms.TS.After(after) {
				rv = append(rv, ms)
			}
		}
		if err := serveJSON(w, r, rv); err != nil {
			log.Printf("Error serving statuses json: %v", err)
		}
	})

	if *logpath != "" {
		http.Handle("/logs/", http.StripPrefix("/logs/", logHandler{}))
	}
	http.Handle("/", http.FileServer(http.Dir(*static)))

}
