package powerlab

import (
	"compress/gzip"
	"encoding/csv"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
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

// NewJSONStatusLogger creates a new StatusLogger that writes in JSON format.
func NewJSONStatusLogger(w io.WriteCloser) StatusLogger {
	return &JSONStatusLogger{w}
}

// Close closes.
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

// NewGobStatusLogger creates a new StatusLogger that writes in gob format.
func NewGobStatusLogger(w io.WriteCloser) StatusLogger {
	return &GobStatusLogger{w, gob.NewEncoder(w)}
}

// Close closes.
func (l GobStatusLogger) Close() error {
	l.e = nil
	return l.w.Close()
}

// Log this status to a stream.
func (l *GobStatusLogger) Log(s *Status, t time.Time) error {
	le := LogEntry{t, (*s)[:], s}
	return l.e.Encode(&le)
}

// LogSource is a source of Log entries.
type LogSource struct {
	io.Closer
	d interface {
		Decode(interface{}) error
	}
}

// Next returns the next log entry.
func (l *LogSource) Next() (*LogEntry, error) {
	rv := &LogEntry{}
	return rv, l.d.Decode(rv)
}

// NewLogReaderStream returns a log reader pulling log entries out of
// the given stream.
func NewLogReaderStream(r io.ReadCloser, f string) (*LogSource, error) {

	rv := &LogSource{Closer: r}
	switch f {
	case "gob":
		rv.d = gob.NewDecoder(r)
	case "json":
		rv.d = json.NewDecoder(r)
	default:
		return nil, fmt.Errorf("Unknown file format (not json or gob): %v", f)
	}

	return rv, nil
}

// NewLogReader opens the given filename as a powerlab log and
// commenses to emit reccords.
func NewLogReader(fn string) (*LogSource, error) {
	lf, err := os.Open(fn)
	if err != nil {
		log.Fatalf("Error opening replay file: %v", err)
	}

	var r io.ReadCloser = lf
	if strings.HasSuffix(fn, ".gz") {
		gzr, err := gzip.NewReader(r)
		if err != nil {
			log.Fatalf("Error ungzipping: %v", err)
		}
		r = gzr
	}

	switch {
	case strings.Contains(fn, ".gob"):
		return NewLogReaderStream(r, "gob")
	case strings.Contains(fn, ".json"):
		return NewLogReaderStream(r, "json")
	}

	lf.Close()
	return nil, fmt.Errorf("Unknown file format (not json or gob)")
}

var csvFields = []string{
	"avg_amps",
	"avg_cell",
	"bal_pwm_1",
	"bal_pwm_2",
	"bal_pwm_3",
	"bal_pwm_4",
	"bal_pwm_5",
	"bal_pwm_6",
	"bal_pwm_7",
	"bal_pwm_8",
	"balancing",
	"battery_neg",
	"battery_pos",
	"charge_complete",
	"charge_current",
	"charge_sec",
	"charging",
	"chemistry",
	"cpu_temp",
	"cycle_num",
	"detected_cell_count",
	"discharge_amps_set",
	"discharge_pwm",
	"discharging",
	"fast_amps",
	"high_temp",
	"ir_1",
	"ir_2",
	"ir_3",
	"ir_4",
	"ir_5",
	"ir_6",
	"ir_7",
	"ir_8",
	"mah_in",
	"mah_out",
	"max_cell",
	"mode",
	"nicd_fallback",
	"out_pos",
	"packs",
	"power_reduction_reason",
	"preset_set_amps",
	"reduce_amps",
	"regen_discharge",
	"regen_volt_set",
	"safety_charge",
	"screen_num",
	"slow_avg_amps",
	"start_avg_cell",
	"start_supply_volts",
	"supply_amps",
	"supply_volts",
	"supply_volts_with_current",
	"sync_pwm_drive",
	"version",
	"voltage_1",
	"voltage_2",
	"voltage_3",
	"voltage_4",
	"voltage_5",
	"voltage_6",
	"voltage_7",
	"voltage_8",
}

func sliceVal(i interface{}) []interface{} {
	rv := []interface{}{}

	switch v := i.(type) {
	case []interface{}:
		return v
	case []int:
		for _, x := range v {
			rv = append(rv, x)
		}
	case []float64:
		for _, x := range v {
			rv = append(rv, x)
		}
	default:
		log.Panicf("Unhandled type: %T", v)
	}

	return rv
}

func wideCSVFormat(c *csv.Writer, t time.Time, m map[string]interface{}) error {
	row := []string{t.Format(time.RFC3339)}

	for _, f := range csvFields {
		if val, ok := m[f]; ok {
			row = append(row, fmt.Sprint(val))
		} else {
			k := f[:len(f)-2]
			i := f[len(f)-1] - '0' - 1

			if val, ok := m[k]; ok {
				s := sliceVal(val)
				if len(s) > int(i) {
					row = append(row, fmt.Sprint(s[i]))
				} else {
					row = append(row, "0")
				}
			} else {
				log.Fatalf("Failed to find %q", k)
			}
		}
	}
	return c.Write(row)
}

func longCSVFormat(c *csv.Writer, t time.Time, m map[string]interface{}) error {
	for _, f := range csvFields {
		row := []string{t.Format(time.RFC3339)}

		if val, ok := m[f]; ok {
			row = append(row, f, "0", fmt.Sprint(val))
		} else {
			k := f[:len(f)-2]
			i := f[len(f)-1] - '0' - 1

			if val, ok := m[k]; ok {
				s := sliceVal(val)
				if len(s) > int(i) {
					row = append(row, k, fmt.Sprint(i+1), fmt.Sprint(s[i]))
				} else {
					row = append(row, k, fmt.Sprint(i+1), "0")
				}
			} else {
				log.Fatalf("Failed to find %q", k)
			}
		}
		if err := c.Write(row); err != nil {
			return err
		}
	}
	return nil
}

func csvReader(r *LogSource, hdr []string, conv func(c *csv.Writer, t time.Time, m map[string]interface{}) error) io.ReadCloser {
	pr, pw := io.Pipe()
	c := csv.NewWriter(pw)

	go func() {
		if err := c.Write(hdr); err != nil {
			pw.CloseWithError(err)
			return
		}

		for {
			e, err := r.Next()
			if err == io.EOF {
				c.Flush()
				pw.Close()
				return
			}
			if err != nil {
				log.Fatalf("Decode error: %v", err)
			}

			if err := conv(c, e.Timestamp, e.Data.Map()); err != nil {
				pw.CloseWithError(err)
				return
			}
		}
	}()

	return pr
}

// NewWideCSVReader provides a simple conversion from a LogSource to a CSV io.Reader.
//
// This version adds a column for every field.
func NewWideCSVReader(r *LogSource) io.ReadCloser {
	return csvReader(r, append([]string{"ts"}, csvFields...), wideCSVFormat)
}

// NewLongCSVReader provides a sipmle conversion from LogSource to a CSV io.Reader.
//
// This version has a fixed number of columns, with a row for each
// field at a given timestamp.  (This is really useful for long-form analysis in R).
func NewLongCSVReader(r *LogSource) io.ReadCloser {
	return csvReader(r, []string{"ts", "field", "cell", "value"}, longCSVFormat)
}
