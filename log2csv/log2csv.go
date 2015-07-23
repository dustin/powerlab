package main

import (
	"encoding/csv"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"time"

	"github.com/dustin/powerlab"
)

var (
	wideFormat = flag.Bool("wide", false, "emit wide format csv")
	logFmt     = flag.String("format", "gob", "log format -- (gob or json)")
)

var fields = []string{
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

func wide(c *csv.Writer, t time.Time, m map[string]interface{}) {
	row := []string{t.Format(time.RFC3339)}

	for _, f := range fields {
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
	c.Write(row)
}

func long(c *csv.Writer, t time.Time, m map[string]interface{}) {
	for _, f := range fields {
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
		c.Write(row)
	}
}

func main() {
	flag.Parse()

	ls, err := powerlab.NewLogReaderStream(os.Stdin, *logFmt)
	if err != nil {
		log.Fatalf("Couldn't open log reader: %v", err)
	}

	c := csv.NewWriter(os.Stdout)
	defer c.Flush()

	convert := long
	if *wideFormat {
		c.Write(append([]string{"ts"}, fields...))
		convert = wide
	} else {
		c.Write([]string{"ts", "field", "cell", "value"})
	}

	for {
		e, err := ls.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatalf("Decode error: %v", err)
		}

		convert(c, e.Timestamp, e.Data.Map())
	}
}
