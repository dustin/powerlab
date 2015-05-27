package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"log/syslog"
	"net/http"
	"os"
	"sync"
	"time"

	"github.com/dustin/powerlab"
)

type sample struct {
	t  time.Time
	st *powerlab.Status
}

var (
	bind         = flag.String("bind", ":8080", "addr:port to bind to")
	port         = flag.String("port", "/dev/ttyUSB0", "powerlab serial port")
	logpath      = flag.String("logpath", "log", "path to log files")
	static       = flag.String("static", "static", "path to static content")
	stateLogFreq = flag.Duration("logfreq", time.Minute, "state log frequency")
	logTimeout   = flag.Duration("logtimeout", time.Minute*5, "how long to log after a charge is complete")
	useSyslog    = flag.Bool("syslog", false, "Log to syslog")

	current = struct {
		st *powerlab.Status
		mu sync.Mutex
	}{}
)

func setCurrent(st *powerlab.Status) {
	current.mu.Lock()
	defer current.mu.Unlock()
	current.st = st
}

func getCurrent() *powerlab.Status {
	current.mu.Lock()
	defer current.mu.Unlock()
	return current.st
}

func logger(ch <-chan sample) {
	var w io.WriteCloser
	var logDeadline time.Time

	var prevMode powerlab.Mode
	prevComplete := false

	for s := range ch {
		mode := s.st.Mode()
		complete := s.st.ChargeComplete()

		if mode != prevMode {
			log.Printf("State change mode %v->%v", prevMode, mode)
			prevMode = mode
		}
		if complete != prevComplete {
			log.Printf("Completed charge/discharge %v->%v",
				prevComplete, complete)
			prevComplete = complete
		}

		if w == nil {
			if mode != powerlab.Ready && !complete {
				fn := fmt.Sprintf("%v/%v.json", *logpath,
					time.Now().Format(time.RFC3339))
				f, err := os.OpenFile(fn, os.O_RDWR|os.O_CREATE, 0666)
				if err != nil {
					log.Printf("Error creating log file: %v", err)
					continue
				}
				w = f
				logDeadline = time.Now().Add(*logTimeout)
				log.Printf("Starting log %v for charge", fn)
			}
		} else {
			if mode == powerlab.Ready || time.Now().After(logDeadline) {
				log.Printf("Closing logfile")
				if err := w.Close(); err != nil {
					log.Printf("Error closing logfile: %v", err)
				}
				w = nil
				logDeadline = time.Time{}
			}
		}
		if w != nil {
			if err := s.st.Log(s.t, w); err != nil {
				log.Printf("Error logging: %v", err)
			}
			// Stop logging a bit after we no longer see ourselves charging
			if (mode == powerlab.Charging || mode == powerlab.Discharging) && !complete {
				logDeadline = time.Now().Add(*logTimeout)
			}
		}
	}
}

func powerlabReader() {
	pl, err := powerlab.Open(*port)
	if err != nil {
		log.Fatalf("Error opening powerlab: %v", err)
	}

	ch := make(chan sample, 1)
	go logger(ch)

	for t := range time.Tick(time.Second) {
		st, err := pl.Status(0)
		if err != nil {
			if err != powerlab.ErrTimeout {
				log.Printf("Failed to read status: %v", err)
			}
			continue
		}

		setCurrent(st)

		select {
		case ch <- sample{t, st}:
		default:
		}
	}
}

func statusLogger() {
	loggedReady := false
	for range time.Tick(*stateLogFreq) {
		st := getCurrent()
		if st == nil || (st.Mode() == powerlab.Ready && loggedReady) {
			continue
		}
		volts := []string{}
		ir := []string{}
		for i := 0; i < st.DetectedCellCount(); i++ {
			volts = append(volts, fmt.Sprintf("%.2f", st.CellVoltage(i+1)))
			ir = append(ir, fmt.Sprintf("%.2f", st.IR(i+1)))
		}

		switch st.Mode() {
		case powerlab.Charging, powerlab.TrickleCharging:
			log.Printf("%v %.1f%%, amps=%.2fA, mah_in=%v, cells=%v, ir=%v, charge time=%v",
				st.Mode(), st.AvgCell(), st.AvgAmps(), st.MAhIn(),
				volts, ir, st.ChargeDuration())
		case powerlab.Discharging:
			log.Printf("%v %.1f%%, amps=%.2fA, mah_out=%v, cells=%v, charge time=%v",
				st.Mode(), st.AvgCell(), st.AvgAmps(), st.MAhOut(),
				volts, st.ChargeDuration())
		case powerlab.Monitoring:
			log.Printf("%v %.1f%%, volts=%v",
				st.Mode(), st.AvgCell(), volts)
		default:
			log.Printf("%v", st.Mode())
		}
		loggedReady = st.Mode() == powerlab.Ready
	}
}

func main() {
	flag.Parse()

	if *useSyslog {
		sl, err := syslog.New(syslog.LOG_INFO, "powerserver")
		if err != nil {
			log.Fatalf("Error initializing syslog")
		}
		log.SetOutput(sl)
		log.SetFlags(0)
	}

	go powerlabReader()
	go statusLogger()

	http.HandleFunc("/status", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(getCurrent())
	})

	http.Handle("/", http.FileServer(http.Dir(*static)))

	log.Fatal(http.ListenAndServe(*bind, nil))
}
