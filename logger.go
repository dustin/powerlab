package powerlab

import (
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

// UnmarshalJSON pulls the binary data and timestamp out of the log to
// recreate the log entry.
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

// MarshalBinary provides a compact binary marshaler for LogEntries.
func (l *LogEntry) MarshalBinary() (data []byte, err error) {
	tb, err := l.Timestamp.MarshalBinary()
	if err != nil {
		return nil, err
	}
	return append(tb, l.Raw...), nil
}

// Log this status to a stream.
func (s *Status) Log(t time.Time, w io.Writer) error {
	if err := json.NewEncoder(w).Encode(LogEntry{t, (*s)[:], s}); err != nil {
		return err
	}
	if _, err := w.Write([]byte{'\n'}); err != nil {
		return err
	}

	return nil
}
