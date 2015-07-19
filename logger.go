package powerlab

import (
	"encoding/gob"
	"encoding/json"
	"fmt"
	"io"
	"time"
)

const logEncodingMagic = 0x5b

// LogEntry represents a single entry in a powerlab Status log stream.
type LogEntry struct {
	Timestamp time.Time
	Raw       []byte
	Data      *Status
}

func init() {
	gob.Register(&LogEntry{})
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
	// magic + time encoding size + raw data
	res := make([]byte, 1+15+len(l.Raw))
	res[0] = logEncodingMagic

	tb, err := l.Timestamp.MarshalBinary()
	if err != nil {
		return nil, err
	}
	copy(res[1:], tb)
	copy(res[16:], l.Raw)
	return res, nil
}

// UnmarshalBinary reverses MarshalBinary's encoding.
func (l *LogEntry) UnmarshalBinary(data []byte) error {
	if data[0] != logEncodingMagic {
		return fmt.Errorf("Invalid encoding magic: %x", data[0])
	}
	if err := l.Timestamp.UnmarshalBinary(data[1:16]); err != nil {
		return err
	}

	l.Data = &Status{}
	if len(l.Data) != (len(data) - 16) {
		return fmt.Errorf("Mismatched data length. Wanted %v, got %v",
			len(l.Data), len(data)-16)
	}
	copy((*l.Data)[:], data[16:])
	l.Raw = (*l.Data)[:]
	return nil
}

// StatusLogger provides logging for logging status records.
type StatusLogger interface {
	Log(*Status, time.Time) error
	Close() error
}

// JSONStatusLogger logs status in JSON format.
type JSONStatusLogger struct {
	w io.WriteCloser
}

func NewJSONStatusLogger(w io.WriteCloser) StatusLogger {
	return &JSONStatusLogger{w}
}

func (l JSONStatusLogger) Close() error {
	return l.w.Close()
}

// Log this status to a stream.
func (l *JSONStatusLogger) Log(s *Status, t time.Time) error {
	if err := json.NewEncoder(l.w).Encode(LogEntry{t, (*s)[:], s}); err != nil {
		return err
	}
	if _, err := l.w.Write([]byte{'\n'}); err != nil {
		return err
	}

	return nil
}

// GobStatusLogger logs status in binary gob format.
type GobStatusLogger struct {
	w io.WriteCloser
	e *gob.Encoder
}

func NewGobStatusLogger(w io.WriteCloser) StatusLogger {
	return &GobStatusLogger{w, gob.NewEncoder(w)}
}

func (l GobStatusLogger) Close() error {
	l.e = nil
	return l.w.Close()
}

// Log this status to a stream.
func (l *GobStatusLogger) Log(s *Status, t time.Time) error {
	le := LogEntry{t, (*s)[:], s}
	return l.e.Encode(&le)
}
