package main

import (
	"flag"
	"io"
	"log"
	"os"

	"github.com/dustin/powerlab"
)

var (
	wideFormat = flag.Bool("wide", false, "emit wide format csv")
	logFmt     = flag.String("format", "gob", "log format -- (gob or json)")
)

func main() {
	flag.Parse()

	ls, err := powerlab.NewLogReaderStream(os.Stdin, *logFmt)
	if err != nil {
		log.Fatalf("Couldn't open log reader: %v", err)
	}

	convert := powerlab.NewLongCSVReader
	if *wideFormat {
		convert = powerlab.NewWideCSVReader
	}

	io.Copy(os.Stdout, convert(ls))
}
