package powerlab

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"time"
)

// LogEntry represents a single entry in a powerlab Status log stream.
type LogEntry struct {
	Timestamp time.Time
	Raw       []byte
	Data      *Status
}

func (l *LogEntry) UnmarshalJSON(data []byte) error {
	led := &struct {
		Timestamp time.Time
		Raw       []byte
	}{}
	err := json.Unmarshal(data, led)
	if err != nil {
		return err
	}

	l.Timestamp = led.Timestamp
	l.Raw = led.Raw
	l.Data = &Status{}
	if len(l.Data) != len(l.Raw) {
		return fmt.Errorf("Mismatched data length. Wanted %v, got %v",
			len(l.Data), len(l.Raw))
	}
	copy((*l.Data)[:], l.Raw)
	return nil
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
