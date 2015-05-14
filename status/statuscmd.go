package main

import (
	"encoding/json"
	"log"
	"os"

	"encoding/hex"

	"github.com/dustin/powerlab"
)

func main() {
	pl, err := powerlab.Open(os.Args[1])
	if err != nil {
		log.Fatalf("Error opening port: %v", err)
	}

	st, err := pl.Status(0)
	if err != nil {
		log.Fatalf("Error getting status: %v", err)
	}

	log.Printf("PL version %v", st.Version())

	err = json.NewEncoder(os.Stdout).Encode(st)
	if err != nil {
		log.Fatalf("Error writing JSON: %v", err)
	}

	hex.Dumper(os.Stdout).Write(st[:])
	os.Stdout.Write([]byte{'\n'})
}
