package main

import (
	"compress/gzip"
	"expvar"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"sync"
	"time"

	"github.com/dustin/go-nma"
	"github.com/dustin/powerlab"
	"github.com/dustin/replaykit"
)

type sample struct {
	t  time.Time
	st *powerlab.Status
}

const (
	maxReadings     = 3600
	maxCRCFails     = 5
	watchdogTimeout = time.Minute * 30
)

type markedStatus struct {
	TS time.Time
	ST *powerlab.Status
}

type readings struct {
	vals []markedStatus
	head int

	mu sync.Mutex
}

var (
	bind         = flag.String("bind", ":8080", "addr:port to bind to")
	port         = flag.String("port", "/dev/ttyUSB0", "powerlab serial port")
	logpath      = flag.String("logpath", "log", "path to log files")
	static       = flag.String("static", "static", "path to static content")
	stateLogFreq = flag.Duration("logfreq", time.Minute, "state log frequency")
	logTimeout   = flag.Duration("logtimeout", time.Minute*5, "how long to log after a charge is complete")
	sampleFreq   = flag.Duration("samplefreq", time.Second, "how often to sample status")
	useSyslog    = flag.Bool("syslog", false, "Log to syslog")
	nmaKey       = flag.String("nmakey", "", "notify my android key")
	replayFile   = flag.String("replayfile", "", "log to play back")
	replaySpeed  = flag.Float64("replayspeed", 1.0, "log playback time factor")
	logFormat    = flag.String("logformat", "json", "json|gob")

	current readings

	newLogger = powerlab.NewJSONStatusLogger

	statusErrors = expvar.NewInt("status_errors")
	currentLog   = expvar.NewString("current_log")
	currentState = expvar.NewString("state")
	stateChanges = expvar.NewMap("states")
)

func setCurrent(st *powerlab.Status) {
	current.mu.Lock()
	defer current.mu.Unlock()

	var d time.Duration
	if len(current.vals) > 0 {
		d = current.vals[current.head].ST.ChargeDuration()
	}

	if d > st.ChargeDuration() {
		log.Printf("Resetting stats.  Old duration=%v, new duration=%v", d, st.ChargeDuration())
		current.vals = nil
		current.head = 0
	}

	now := time.Now()
	if len(current.vals) >= maxReadings {
		current.head++
		if current.head >= maxReadings {
			current.head = 0
		}
		current.vals[current.head] = markedStatus{now, st}
	} else {
		current.vals = append(current.vals, markedStatus{now, st})
		current.head = len(current.vals) - 1
	}
}

func getCurrent() *powerlab.Status {
	current.mu.Lock()
	defer current.mu.Unlock()
	if len(current.vals) == 0 {
		return nil
	}
	return current.vals[current.head].ST
}

func resetCurrent() {
	current.mu.Lock()
	defer current.mu.Unlock()
	current.vals[0] = current.vals[current.head]
	current.vals = current.vals[:1]
	current.head = 0
}

func allReadings() []markedStatus {
	current.mu.Lock()
	defer current.mu.Unlock()

	if len(current.vals) == 0 {
		return nil
	}

	return append(current.vals[current.head:], current.vals[:current.head]...)
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
	g, err := gzip.NewWriterLevel(f, gzip.BestSpeed)
	if err != nil {
		// This can't reasonably fail.
		panic(err)
	}
	return &gzwriter{
		w: f,
		g: g,
	}, nil
}

func logger(ch <-chan sample) {
	var l powerlab.StatusLogger
	var logDeadline time.Time

	prevMode := powerlab.Unknown
	prevComplete := false

	wdch := make(chan sample)
	go func() {
		timeout := time.NewTimer(watchdogTimeout)
		for {
			select {
			case <-wdch:
				timeout.Reset(watchdogTimeout)
			case <-timeout.C:
				log.Printf("Watchdog failure:  too long since last log message")
				os.Exit(1)
			}
		}
	}()

	log.Printf("Started logger")

	for s := range ch {
		wdch <- s // feed the watchdog

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

		if l == nil {
			if mode != powerlab.Ready && !complete && *logpath != "" {
				resetCurrent()
				fn := fmt.Sprintf("%v/%v.%s.gz", *logpath,
					time.Now().Format(time.RFC3339), *logFormat)
				f, err := openGzWriter(fn)
				if err != nil {
					log.Printf("Error creating log file: %v", err)
					continue
				}
				l = newLogger(f)
				logDeadline = time.Now().Add(*logTimeout)
				log.Printf("Starting log %v for mode %q", fn, mode)
				currentLog.Set(fn)
			}
		} else {
			if mode == powerlab.Ready || time.Now().After(logDeadline) {
				log.Printf("Closing logfile")
				if err := l.Close(); err != nil {
					log.Printf("Error closing logfile: %v", err)
				}
				currentLog.Set("")
				l = nil
				logDeadline = time.Time{}
			}
		}
		if l != nil {
			if err := l.Log(s.st, s.t); err != nil {
				log.Printf("Error logging: %v", err)
			}
			// Stop logging a bit after we no longer see ourselves charging
			if (mode == powerlab.Charging ||
				mode == powerlab.Discharging ||
				mode == powerlab.PackCoolDown ||
				mode == powerlab.TrickleCharging ||
				mode == powerlab.Monitoring ||
				mode == powerlab.DetectingPack) && !complete {
				logDeadline = time.Now().Add(*logTimeout)
			}
		}
	}
}

func powerlabReader() {
	ch := make(chan sample, 1)
	go logger(ch)
	crcFails := 0

	// This prevents too many fast restarts on fast failure
	for range time.Tick(5 * time.Second) {
		err := powerLabReaderLoop(ch)
		if err == powerlab.ErrCRC {
			crcFails++
			if crcFails > maxCRCFails {
				log.Printf("Exiting after too many CRC failures")
				os.Exit(1)
			}
		} else {
			crcFails = 0
		}
		log.Printf("Closed reader loop (retrying): %v", err)
	}
}

func powerLabReaderLoop(ch chan sample) error {
	pl, err := powerlab.Open(*port)
	if err != nil {
		return err
	}
	defer pl.Close()

	log.Printf("Started reader")
	defer log.Printf("Closing reader")

	hardErrors := 0
	heart := time.NewTicker(*sampleFreq)
	defer heart.Stop()
	for t := range heart.C {
		st, err := pl.Status(0)
		if err != nil {
			statusErrors.Add(1)
			if err != powerlab.ErrTimeout {
				hardErrors++
				if hardErrors > 5 {
					log.Printf("Too many errors getting status.")
					return err
				}
				log.Printf("Failed to read status: %v", err)
			}
			continue
		}
		hardErrors = 0

		setCurrent(st)

		select {
		case ch <- sample{t, st}:
		default:
		}
	}
	panic("unreachable")
}

type logEvent struct {
	le *powerlab.LogEntry
}

func (l logEvent) TS() time.Time { return l.le.Timestamp }

func powerlabPlaybackOnce() {
	ls, err := powerlab.NewLogReader(*replayFile)
	if err != nil {
		log.Fatalf("Error opening replay file: %v", err)
	}
	defer ls.Close()

	src := replay.FunctionSource(func() replay.Event {
		le, err := ls.Next()
		if err != nil {
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
		if err := le.le.Data.ValidateCRC(); err != nil {
			log.Print(err)
			return
		}
		ch <- sample{le.le.Timestamp, le.le.Data}
	}))

	log.Printf("Completed replay")
}

func powerlabPlayback() {
	for {
		powerlabPlaybackOnce()
	}
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
	switch *logFormat {
	case "json":
		newLogger = powerlab.NewJSONStatusLogger
	case "gob":
		newLogger = powerlab.NewGobStatusLogger
	default:
		log.Fatalf("invalid log format: %v (must be either json or gob)", *logFormat)
	}

	initLogging(*useSyslog)

	log.Printf("Starting powerserver.")

	if *replayFile == "" {
		go powerlabReader()
	} else {
		go powerlabPlayback()
	}
	go statusLogger()

	initWeb()

	log.Fatal(http.ListenAndServe(*bind, nil))
}
