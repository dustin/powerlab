package main

import (
	"compress/gzip"
	"flag"
	"io"
	"log"
	"os"
	"strings"

	"github.com/dustin/powerlab"
)

func main() {
	flag.Parse()

	if flag.NArg() != 2 {
		log.Fatalf("Need a source and dest")
	}

	lr, err := powerlab.NewLogReader(flag.Arg(0))
	if err != nil {
		log.Fatalf("Error opening source file: %v", err)
	}

	destfn := flag.Arg(1)

	f, err := os.Create(destfn)
	if err != nil {
		log.Fatalf("Error creating destination file: %v", err)
	}
	defer f.Close()

	w := io.WriteCloser(f)
	if strings.HasSuffix(destfn, ".gz") {
		gz := gzip.NewWriter(w)
		defer gz.Close()
		w = gz
	}

	neww := powerlab.NewGobStatusLogger
	if strings.Contains(destfn, ".json") {
		neww = powerlab.NewJSONStatusLogger
	}

	lw := neww(w)

	i := 0
	for {
		e, err := lr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatalf("Decode error: %v", err)
		}

		if err := lw.Log(e.Data, e.Timestamp); err != nil {
			log.Fatalf("Error logging: %v", err)
		}
		i++
	}

	log.Printf("Copied %v entries", i)
}
