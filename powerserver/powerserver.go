package main

import (
	"compress/gzip"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"log/syslog"
	"net/http"
	"os"
	"strings"
	"sync"
	"time"

	"expvar"

	"github.com/dustin/go-nma"
	"github.com/dustin/httputil"
	"github.com/dustin/powerlab"
	"github.com/dustin/replaykit"
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
	nmaKey       = flag.String("nmakey", "", "notify my android key")
	replayFile   = flag.String("replayfile", "", "log to play back")
	replaySpeed  = flag.Float64("replayspeed", 1.0, "log playback time factor")

	current = struct {
		st *powerlab.Status
		mu sync.Mutex
	}{}

	statusErrors = expvar.NewInt("status_errors")
	currentLog   = expvar.NewString("current_log")
	currentState = expvar.NewString("state")
	stateChanges = expvar.NewMap("states")
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

func strfloats(fs []float64) []string {
	rv := []string{}
	for _, f := range fs {
		rv = append(rv, fmt.Sprintf("%.2f", f))
	}
	return rv
}

func notify(st *powerlab.Status) {
	if *nmaKey == "" {
		return
	}

	volts := strfloats(st.CellVoltages())
	ir := strfloats(st.IRs())

	msg := ""
	switch st.Mode() {
	case powerlab.Charging, powerlab.TrickleCharging:
		msg = fmt.Sprintf("%vS%vP %.1f%%, in=%vmA, cells=%v, ir=%v, charge time=%v",
			st.DetectedCellCount(), st.Packs(), st.AvgCell(),
			st.MAhIn(), volts, ir, st.ChargeDuration())
	case powerlab.Discharging:
		msg = fmt.Sprintf("%vS%vP %.1f%%, out=%vmA, cells=%v, discharge time=%v",
			st.DetectedCellCount(), st.Packs(), st.AvgCell(),
			st.MAhOut(), volts, st.ChargeDuration())
	}

	n := nma.New(*nmaKey)
	nmamsg := &nma.Notification{
		Application: "powerlab",
		Description: msg,
		Event:       st.Mode().String(),
	}

	if err := n.Notify(nmamsg); err != nil {
		log.Printf("Problem notifying: %v", err)
	}
}

type gzwriter struct {
	w io.WriteCloser
	g *gzip.Writer
}

func (g *gzwriter) Write(b []byte) (int, error) {
	return g.g.Write(b)
}

func (g *gzwriter) Close() error {
	err := g.g.Close()
	if err != nil {
		defer g.w.Close()
		return err
	}
	return g.w.Close()
}

func openGzWriter(fn string) (io.WriteCloser, error) {
	f, err := os.OpenFile(fn, os.O_RDWR|os.O_CREATE, 0666)
	if err != nil {
		return nil, err
	}
	return &gzwriter{
		w: f,
		g: gzip.NewWriter(f),
	}, nil
}

func logger(ch <-chan sample) {
	var w io.WriteCloser
	var logDeadline time.Time

	prevMode := powerlab.Unknown
	prevComplete := false

	log.Printf("Started logger")

	for s := range ch {
		mode := s.st.Mode()
		complete := s.st.ChargeComplete()

		if mode != prevMode {
			log.Printf("State change mode %v->%v", prevMode, mode)
			stateChanges.Add(mode.String(), 1)
			currentState.Set(mode.String())
			prevMode = mode
		}
		if complete != prevComplete {
			log.Printf("Completed charge/discharge %v->%v",
				prevComplete, complete)
			prevComplete = complete
			if complete {
				notify(s.st)
			}
		}

		if w == nil {
			if mode != powerlab.Ready && !complete && *logpath != "" {
				fn := fmt.Sprintf("%v/%v.json.gz", *logpath,
					time.Now().Format(time.RFC3339))
				f, err := openGzWriter(fn)
				if err != nil {
					log.Printf("Error creating log file: %v", err)
					continue
				}
				w = f
				logDeadline = time.Now().Add(*logTimeout)
				log.Printf("Starting log %v for mode %q", fn, mode)
				currentLog.Set(fn)
			}
		} else {
			if mode == powerlab.Ready || time.Now().After(logDeadline) {
				log.Printf("Closing logfile")
				if err := w.Close(); err != nil {
					log.Printf("Error closing logfile: %v", err)
				}
				currentLog.Set("")
				w = nil
				logDeadline = time.Time{}
			}
		}
		if w != nil {
			if err := s.st.Log(s.t, w); err != nil {
				log.Printf("Error logging: %v", err)
			}
			// Stop logging a bit after we no longer see ourselves charging
			if (mode == powerlab.Charging ||
				mode == powerlab.Discharging ||
				mode == powerlab.PackCoolDown ||
				mode == powerlab.TrickleCharging ||
				mode == powerlab.DetectingPack) && !complete {
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

	log.Printf("Started reader")

	for t := range time.Tick(time.Second) {
		st, err := pl.Status(0)
		if err != nil {
			statusErrors.Add(1)
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

type logEvent struct {
	le *powerlab.LogEntry
}

func (l logEvent) TS() time.Time { return l.le.Timestamp }

func powerlabPlayback() {
	lf, err := os.Open(*replayFile)
	if err != nil {
		log.Fatalf("Error opening replay file: %v", err)
	}

	r := io.Reader(lf)
	if strings.HasSuffix(*replayFile, ".gz") {
		gzr, err := gzip.NewReader(r)
		if err != nil {
			log.Fatalf("Error ungzipping: %v", err)
		}
		r = gzr
	}
	j := json.NewDecoder(r)

	src := replay.FunctionSource(func() replay.Event {
		le := &powerlab.LogEntry{}
		if err := j.Decode(le); err != nil {
			log.Printf("Error unmarshaling entry: %v", err)
			return nil
		}
		setCurrent(le.Data)
		return logEvent{le}
	})

	ch := make(chan sample)
	go logger(ch)

	log.Printf("Started replay on %v", *replayFile)

	rpl := replay.New(*replaySpeed)
	rpl.Run(src, replay.FunctionAction(func(event replay.Event) {
		le := event.(logEvent)
		ch <- sample{le.le.Timestamp, le.le.Data}
	}))

	log.Printf("Completed replay")
}

func statusLogger() {
	loggedReady := false
	for range time.Tick(*stateLogFreq) {
		st := getCurrent()
		if st == nil || (st.Mode() == powerlab.Ready && loggedReady) {
			continue
		}
		volts := strfloats(st.CellVoltages())
		ir := strfloats(st.IRs())

		complete := ""
		if st.ChargeComplete() {
			complete = " (complete)"
		}

		switch st.Mode() {
		case powerlab.Charging, powerlab.TrickleCharging:
			log.Printf("%v%s %vS%vP %.1f%%, current=%.2fA, in=%vmA, cells=%v, ir=%v, charge time=%v",
				st.Mode(), complete, st.DetectedCellCount(), st.Packs(), st.AvgCell(),
				st.AvgAmps(), st.MAhIn(), volts, ir, st.ChargeDuration())
		case powerlab.Discharging:
			log.Printf("%v%s %vS%vP %.1f%%, current=%.2fA, out=%vmA, cells=%v, discharge time=%v",
				st.Mode(), complete, st.DetectedCellCount(), st.Packs(), st.AvgCell(),
				st.AvgAmps(), st.MAhOut(), volts, st.ChargeDuration())
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

	expvar.Publish("httpclients", httputil.InitHTTPTracker(false))

	if *useSyslog {
		sl, err := syslog.New(syslog.LOG_INFO, "powerserver")
		if err != nil {
			log.Fatalf("Error initializing syslog")
		}
		log.SetOutput(sl)
		log.SetFlags(0)
	}

	if *replayFile == "" {
		go powerlabReader()
	} else {
		go powerlabPlayback()
	}
	go statusLogger()

	http.HandleFunc("/status", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(getCurrent())
	})

	http.Handle("/", http.FileServer(http.Dir(*static)))

	log.Fatal(http.ListenAndServe(*bind, nil))
}
