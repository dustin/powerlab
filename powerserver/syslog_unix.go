// +build !windows

package main

import (
	"log"
	"log/syslog"
)

func initLogging(useSyslog bool) {
	if !useSyslog {
		return
	}

	sl, err := syslog.New(syslog.LOG_INFO, "powerserver")
	if err != nil {
		log.Fatalf("Error initializing syslog")
	}
	log.SetOutput(sl)
	log.SetFlags(0)
}
