package main

import (
	"bytes"
	"encoding/hex"
	"encoding/json"
	"log"
	"os"
	"time"

	"github.com/dustin/powerlab"
)

type logEntry struct {
	Timestamp time.Time
	Raw       []byte
	Data      *powerlab.Status
}

func main() {
	log.SetFlags(log.Lmicroseconds)

	if len(os.Args) < 2 {
		log.Fatalf("Path to serial port required")
	}

	pl, err := powerlab.Open(os.Args[1])
	if err != nil {
		log.Fatalf("Error opening port: %v", err)
	}

	for t := range time.Tick(time.Second) {
		st, err := pl.Status(0)
		if err != nil {
			log.Printf("Error getting status: %v", err)
			continue
		}

		log.Printf("Mode: %v, Avg=%.1f%%, amps=%.2fA, charge time=%v",
			st.Mode(), st.AvgCell(), st.AvgAmps(), st.ChargeDuration())

		j, err := json.Marshal(st)
		if err != nil {
			log.Printf("Failed to marshal: %v", err)
			hex.Dumper(os.Stderr).Write(st[:])
			os.Stderr.Write([]byte{'\n'})
			log.Fatalf("exiting")
		}

		j2, err := json.Marshal(logEntry{t, j, st})
		if err != nil {
			log.Fatalf("Failed to marshal log entry: %v", err)
		}

		buf := &bytes.Buffer{}
		json.Indent(buf, j2, "", "  ")
		os.Stdout.Write(buf.Bytes())
		os.Stdout.Write([]byte{'\n'})
	}
}
