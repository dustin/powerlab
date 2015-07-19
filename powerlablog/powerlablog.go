package main

import (
	"encoding/hex"
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

	lw := powerlab.NewJSONStatusLogger(os.Stdout)

	for t := range time.Tick(time.Second) {
		st, err := pl.Status(0)
		if err != nil {
			log.Printf("Error getting status: %v", err)
			continue
		}

		log.Printf("%v %.1f%%, amps=%.2fA, mah_in=%v, charge time=%v",
			st.Mode(), st.AvgCell(), st.AvgAmps(), st.MAhIn(), st.ChargeDuration())

		if err := lw.Log(st, t); err != nil {
			log.Printf("Failed to log: %v", err)
			hex.Dumper(os.Stderr).Write(st[:])
			os.Stderr.Write([]byte{'\n'})
			log.Fatalf("exiting")
		}
	}
}
