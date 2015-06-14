package powerlab

import (
	"bytes"
	"encoding/json"
	"io"
	"time"
)

// LogEntry represents a single entry in a powerlab Status log stream.
type LogEntry struct {
	Timestamp time.Time
	Raw       []byte
	Data      *Status
}

// Log this status to a stream.
func (s *Status) Log(t time.Time, w io.Writer) error {
	j, err := json.Marshal(LogEntry{t, (*s)[:], s})
	if err != nil {
		return err
	}

	buf := &bytes.Buffer{}
	json.Indent(buf, j, "", "  ")
	if _, err := w.Write(buf.Bytes()); err != nil {
		return err
	}
	if _, err := w.Write([]byte{'\n'}); err != nil {
		return err
	}

	return nil
}
