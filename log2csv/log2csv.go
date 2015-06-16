package main

import (
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"time"
)

var wideFormat = flag.Bool("wide", false, "emit wide format csv")

type logEntry struct {
	Timestamp time.Time
	Raw       []byte
	Data      map[string]interface{}
}

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

func wide(c *csv.Writer, e logEntry) {
	row := []string{e.Timestamp.Format(time.RFC3339)}

	for _, f := range fields {
		if val, ok := e.Data[f]; ok {
			row = append(row, fmt.Sprint(val))
		} else {
			k := f[:len(f)-2]
			i := f[len(f)-1] - '0' - 1

			if val, ok := e.Data[k]; ok {
				row = append(row, fmt.Sprint((val.([]interface{}))[i]))
			} else {
				log.Fatalf("Failed to find %q", k)
			}
		}
	}
	c.Write(row)
}

func long(c *csv.Writer, e logEntry) {
	for _, f := range fields {
		row := []string{e.Timestamp.Format(time.RFC3339)}

		if val, ok := e.Data[f]; ok {
			row = append(row, f, "0", fmt.Sprint(val))
		} else {
			k := f[:len(f)-2]
			i := f[len(f)-1] - '0' - 1

			if val, ok := e.Data[k]; ok {
				s := val.([]interface{})
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
	j := json.NewDecoder(os.Stdin)

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
		e := logEntry{}
		err := j.Decode(&e)
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatalf("Decode error: %v", err)
		}

		convert(c, e)
	}
}
