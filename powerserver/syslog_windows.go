package main

import "log"

func initLogging(useSyslog bool) {
	if useSyslog {
		log.Fatalf("Syslog not supported on Windows")
	}
}
