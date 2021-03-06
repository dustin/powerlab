package powerlab

import (
	"bytes"
	"encoding/binary"
	"encoding/json"
	"fmt"
	"math"
	"strconv"
	"sync"
	"time"
)

const (
	statusLen = 149
)

// Status represents a Powerlab6 status response.
type Status [statusLen + 4]byte

var statusFieldMap = map[string]func(s *Status) interface{}{
	"version":                   func(s *Status) interface{} { return s.Version() },
	"sync_pwm_drive":            func(s *Status) interface{} { return s.SyncPWMDrive().String() },
	"charge_current":            func(s *Status) interface{} { return s.ChargeCurrent() },
	"supply_volts_with_current": func(s *Status) interface{} { return s.SupplyVoltsWithCurrent() },
	"supply_volts":              func(s *Status) interface{} { return s.SupplyVolts() },
	"cpu_temp":                  func(s *Status) interface{} { return s.CPUTemp() },
	"charge_sec":                func(s *Status) interface{} { return s.ChargeDuration().Seconds() },
	"charge_time":               func(s *Status) interface{} { return s.ChargeDuration().String() },
	"fast_amps":                 func(s *Status) interface{} { return s.FastAmps() },
	"out_pos":                   func(s *Status) interface{} { return s.OutPositive() },
	"mah_in":                    func(s *Status) interface{} { return s.MAhIn() },
	"avg_cell":                  func(s *Status) interface{} { return s.AvgCell() },
	"start_avg_cell":            func(s *Status) interface{} { return s.StartAvg() },
	"avg_amps":                  func(s *Status) interface{} { return s.AvgAmps() },
	"high_temp":                 func(s *Status) interface{} { return s.HighTemp() },
	"packs":                     func(s *Status) interface{} { return s.Packs() },
	"detected_cell_count":       func(s *Status) interface{} { return s.DetectedCellCount() },
	"nicd_fallback":             func(s *Status) interface{} { return s.NiCdFallbackV() },
	"max_cell":                  func(s *Status) interface{} { return s.MaxCell() },
	"supply_amps":               func(s *Status) interface{} { return s.SupplyAmps() },
	"battery_pos":               func(s *Status) interface{} { return s.BatteryPos() },
	"mah_out":                   func(s *Status) interface{} { return s.MAhOut() },
	"discharge_amps_set":        func(s *Status) interface{} { return s.DischAmpSet() },
	"discharge_pwm":             func(s *Status) interface{} { return s.DischargePWM() },
	"battery_neg":               func(s *Status) interface{} { return s.BatteryNeg() },
	"mode":                      func(s *Status) interface{} { return s.Mode().String() },
	"regen_volt_set":            func(s *Status) interface{} { return s.RegenVoltSet() },
	"start_supply_volts":        func(s *Status) interface{} { return s.StartSupplyVolts() },
	"slow_avg_amps":             func(s *Status) interface{} { return s.SlowAvgAmps() },
	"preset_set_amps":           func(s *Status) interface{} { return s.PresetSetAmps() },
	"preset_num":                func(s *Status) interface{} { return s.Preset() },
	"chemistry":                 func(s *Status) interface{} { return s.Chemistry().String() },
	"screen_num":                func(s *Status) interface{} { return s.ScreenNum() },
	"cycle_num":                 func(s *Status) interface{} { return s.CycleNum() },
	"power_reduction_reason":    func(s *Status) interface{} { return s.PowerReductionReason().String() },
	"voltage": func(s *Status) interface{} {
		var v []float64
		for i := 0; i < s.DetectedCellCount(); i++ {
			v = append(v, s.CellVoltage(i+1))
		}
		return v
	},
	"ir": func(s *Status) interface{} {
		var v []float64
		for i := 0; i < s.DetectedCellCount(); i++ {
			v = append(v, s.IR(i+1))
		}
		return v
	},
	"bal_pwm": func(s *Status) interface{} {
		var v []int
		for i := 0; i < s.DetectedCellCount(); i++ {
			v = append(v, s.BalancePWM(i+1))
		}
		return v
	},
	"slaves_found": func(s *Status) interface{} {
		if sl := s.SlavesFound(); len(sl) > 0 {
			return sl
		}
		return nil
	},
	"safety_charge":   func(s *Status) interface{} { x, _, _ := s.statusFlags(); return x },
	"charge_complete": func(s *Status) interface{} { _, x, _ := s.statusFlags(); return x },
	"reduce_amps":     func(s *Status) interface{} { _, _, x := s.statusFlags(); return x },
	"discharging":     func(s *Status) interface{} { x, _, _, _ := s.rxStatus(); return x },
	"regen_discharge": func(s *Status) interface{} { _, x, _, _ := s.rxStatus(); return x },
	"charging":        func(s *Status) interface{} { _, _, x, _ := s.rxStatus(); return x },
	"balancing":       func(s *Status) interface{} { _, _, _, x := s.rxStatus(); return x },
}

// Map converts the Status struture to a map[string]interface{}
// suitable for JSON encoding.
func (s *Status) Map() map[string]interface{} {
	m := map[string]interface{}{}
	for k, f := range statusFieldMap {
		if v := f(s); v != nil {
			m[k] = v
		}
	}
	return m
}

var bufferPool = &sync.Pool{New: func() interface{} { return &bytes.Buffer{} }}

// MarshalJSON satisfies json.Marshaler.
func (s *Status) MarshalJSON() ([]byte, error) {
	b := bufferPool.Get().(*bytes.Buffer)
	b.Reset()
	defer bufferPool.Put(b)
	b.WriteByte('{')
	needComma := false
	fbuf := []byte{}
	for k, f := range statusFieldMap {
		v := f(s)
		if v == nil {
			continue
		}
		if needComma {
			b.WriteByte(',')
		}
		needComma = true

		b.WriteByte('"')
		b.WriteString(k)
		b.WriteString(`": `)

		switch tv := v.(type) {
		case bool:
			b.WriteString(strconv.FormatBool(tv))
		case string:
			b.WriteString(strconv.Quote(tv))
		case float64:
			fbuf = strconv.AppendFloat(fbuf, tv, 'g', -1, 64)
			b.Write(fbuf)
		case []float64:
			b.WriteByte('[')
			nc := false
			for _, n := range tv {
				if nc {
					b.WriteByte(',')
				}
				nc = true
				fbuf = strconv.AppendFloat(fbuf, n, 'g', -1, 64)
				b.Write(fbuf)
				fbuf = fbuf[:0]
			}
			b.WriteByte(']')
		case int:
			fbuf = strconv.AppendInt(fbuf, int64(tv), 10)
			b.Write(fbuf)
		case []int:
			b.WriteByte('[')
			nc := false
			for _, n := range tv {
				if nc {
					b.WriteByte(',')
				}
				nc = true
				fbuf = strconv.AppendInt(fbuf, int64(n), 10)
				b.Write(fbuf)
				fbuf = fbuf[:0]
			}
			b.WriteByte(']')
		default:
			other, err := json.Marshal(v)
			if err != nil {
				return nil, err
			}
			b.Write(other)
		}

		fbuf = fbuf[:0]
	}
	b.WriteByte('}')
	return b.Bytes(), nil
}

func (s *Status) read1(o int) uint8 {
	o += 4
	return (*s)[o]
}

func (s *Status) read2(o int) uint16 {
	o += 4
	return binary.BigEndian.Uint16((*s)[o : o+2])
}

func (s *Status) read2s(o int) int16 {
	o += 4
	return int16(binary.BigEndian.Uint16((*s)[o : o+2]))
}

func (s *Status) read4(o int) uint32 {
	o += 4
	return binary.BigEndian.Uint32((*s)[o : o+4])
}

// Version of the powerlab firmware.
func (s *Status) Version() string {
	v := s.read2(0)
	return fmt.Sprintf("%d.%d", v/100, v%100)
}

// CellVoltage reports the voltage of the specified cell (1-8).
func (s *Status) CellVoltage(n int) float64 {
	if n < 0 || n > 8 {
		panic("invalid cell number")
	}

	return float64(s.read2(2*n)) * 5.12 / 65535
}

// CellVoltages reports voltages across all detected cells.
func (s *Status) CellVoltages() []float64 {
	volts := []float64{}
	for i := 0; i < s.DetectedCellCount(); i++ {
		volts = append(volts, s.CellVoltage(i+1))
	}
	return volts
}

// IRs returns the internal resistance of all detected cells.
func (s *Status) IRs() []float64 {
	irs := []float64{}
	for i := 0; i < s.DetectedCellCount(); i++ {
		irs = append(irs, s.IR(i+1))
	}
	return irs
}

// PWMType represents the type of PWMDrive being used.
type PWMType bool

const (
	// Buck PWM type.
	Buck = PWMType(false)
	// Boost PWM Type.
	Boost = PWMType(true)
)

func (p PWMType) String() string {
	if p == Buck {
		return "buck"
	}
	return "boost"
}

// SyncPWMDrive returns either Buck or Boost PWM drive.
func (s *Status) SyncPWMDrive() PWMType {
	return PWMType(s.read2(18) >= 8192)
}

// ChargeCurrent represents the current flowing towards the batteries.
func (s *Status) ChargeCurrent() float64 {
	return float64(s.read2(20)) / 1666
}

// SupplyVoltsWithCurrent is poorly documented in the PL6 documentation.
func (s *Status) SupplyVoltsWithCurrent() float64 {
	return float64(s.read2(22)) * 46.96 / 4095 / 16
}

// SupplyVolts provides the voltage being input to the charger.
func (s *Status) SupplyVolts() float64 {
	return float64(s.read2(24)) * 46.96 / 4095.0
}

// CPUTemp is the current CPU temperature in °C
func (s *Status) CPUTemp() float64 {
	return (2.5*float64(s.read2(26))/4095 - 0.986) / 0.00355
}

// chargeSec is the number of seconds in the current charge cycle.
func (s *Status) chargeSec() int {
	return int(s.read2(28))
}

// 78-79
// For <18hr use ChgSec
// For >=18hr Seconds = ChgSec – 64800 + ChgMin * 60
func (s *Status) chargeMin() int {
	return int(s.read2(78)) * 60
}

// ChargeDuration reports how long the most recent charge took (or has
// taken if still in progress).
func (s *Status) ChargeDuration() time.Duration {
	// TODO:  Include minutes
	return time.Duration(time.Second * time.Duration(s.chargeSec()))
}

// FastAmps is the number of amps currently being fed to the
// batteries.
func (s *Status) FastAmps() float64 {
	return float64(s.read2s(30)) / 600
}

// OutPositive is the positive output voltage towards the batteries.
func (s *Status) OutPositive() float64 {
	return float64(s.read2(32)) / 4095.0
}

// MAhIn is the number of milliamp hours sent into the batteries.
func (s *Status) MAhIn() int {
	return int(s.read4(34)) / 2160
}

// AvgCell represents the current average cell fuel percentage.
func (s *Status) AvgCell() float64 {
	return float64(s.read2(38)) / 10.0
}

// StartAvg represents the starting average cell fuel percentage.
func (s *Status) StartAvg() float64 {
	return float64(s.read2(40)) / 10.0
}

// AvgAmps is the average current being fed into batteries as
// displayed on the LCD.  Use this for reading pack current.
func (s *Status) AvgAmps() float64 {
	return float64(s.read2s(42)) / 600
}

// bit uses 1-based bit sequences because that's how the stuff is spec'd
func bit(b uint16, n int) bool {
	return b&(1<<uint(16-n)) != 0
}

// statusFlags returns three bits indicating three statuses:
// - Bit0 = Safety Charge
// - Bit8 = Charge/Discharge Complete
// - Bit11 = Reduce Amps
func (s *Status) statusFlags() (safetyCharge bool, chargeComplete bool, reduceAmps bool) {
	x := s.read2(44)
	return bit(x, 1), bit(x, 8), bit(x, 11)
}

// ChargeComplete is true if the current charge (or discharge) is complete.
func (s *Status) ChargeComplete() bool {
	_, rv, _ := s.statusFlags()
	return rv
}

// - Bit1 = Discharge Running
// - Bit4 = Regenerative Discharge
// - Bit6 = Charge Running
// - Bit7 = Balancers Running
func (s *Status) rxStatus() (discharging bool, regenDischarge bool, charging bool, balancing bool) {
	x := s.read2(46)
	return bit(x, 1), bit(x, 4), bit(x, 6), bit(x, 7)
}

// HighTemp is true if the temperature has exceeded the limit.
func (s *Status) HighTemp() bool {
	return bit(s.read2(50), 2)
}

// IR returns the internal resistance for the given cell in milliohms.
func (s *Status) IR(cell int) float64 {
	if cell < 0 || cell > 8 {
		panic("invalid cell number")
	}

	ir := float64(s.read2(50+(cell*2))) / 6.3984 / s.VRAmps()
	if math.IsNaN(ir) || math.IsInf(ir, 1) || math.IsInf(ir, -1) {
		ir = 0
	}
	return ir
}

// VRAmps is used as part of the internal resistance calculation.
func (s *Status) VRAmps() float64 {
	return float64(s.read2(68)) / 600.0
}

// Packs is the number of packs being charged.
func (s *Status) Packs() int {
	return int(s.read1(136))
}

// DetectedCellCount is the number of cells detected by the charger.
func (s *Status) DetectedCellCount() int {
	return int(s.read1(132))
}

// NiCdFallbackV is the fallback voltage for NiCad charging.
func (s *Status) NiCdFallbackV() float64 {
	return float64(s.read2(70))/12797 - s.MaxCell()
}

// MaxCell voltage.
func (s *Status) MaxCell() float64 {
	return float64(s.read2(74)) / 12797
}

// - Bit4 = Constant Voltage
// - Bit5 = Preset is Valid and Runable
// - Bit8 = Regenerative Discharge Failed
func (s *Status) status6() (constantVoltage bool, presetRunnable bool, regenFailed bool) {
	x := s.read2(76)
	return bit(x, 4), bit(x, 5), bit(x, 8)
}

// SupplyAmps is the current (in amps) being consumed from the power supply.
func (s *Status) SupplyAmps() float64 {
	return float64(s.read2(80)) / 150
}

// BatteryPos returns the battery pos field from the powerlab.
//
// TODO:  Understand this more.
func (s *Status) BatteryPos() float64 {
	return float64(s.read2(82)) / 12797
}

// MAhOut is the amount of power pulled from a battery during discharge.
func (s *Status) MAhOut() int {
	return int(s.read4(84)) / 2160
}

// DischAmpSet is the discharge current specified by the config.
func (s *Status) DischAmpSet() float64 {
	return float64(s.read2(92)) / 600
}

// DischargePWM returns the discharge PWM field from the powerlab.
//
// TODO:  Understand this more.
func (s *Status) DischargePWM() int {
	return int(s.read2(94))
}

// BatteryNeg returns the battery neg value from the powerlab.
//
// TODO:  Understand this more.
func (s *Status) BatteryNeg() float64 {
	return float64(s.read2(100)) * 46.96 / 4095
}

// BalancePWM returns the balance PWM field from the powerlab.
//
// TODO:  Understand this more.
func (s *Status) BalancePWM(cell int) int {
	if cell < 0 || cell > 8 {
		panic("invalid cell number")
	}
	return int(s.read1(124 + (cell * 2)))
}

// Mode represents an operating mode of the charger.
type Mode int

// Known operating modes.
const (
	Unknown         = Mode(-1)
	Ready           = Mode(0)
	DetectingPack   = Mode(1)
	Charging        = Mode(6)
	TrickleCharging = Mode(7)
	Discharging     = Mode(8)
	Monitoring      = Mode(9)
	HaltForSafety   = Mode(10)
	PackCoolDown    = Mode(11)
	SystemStopError = Mode(99)
)

var modeNames = map[Mode]string{
	Unknown:         "unknown",
	Ready:           "ready",
	DetectingPack:   "detecting pack",
	Charging:        "charging",
	TrickleCharging: "trickle charing",
	Discharging:     "discharging",
	Monitoring:      "monitoring",
	HaltForSafety:   "halt for safety",
	PackCoolDown:    "pack cool down",
	SystemStopError: "system stop",
}

// Mode returns the current mode the charger is in.
func (s *Status) Mode() Mode {
	return Mode(s.read1(133))
}

func (m Mode) String() string {
	if n, ok := modeNames[m]; ok {
		return n
	}
	return fmt.Sprintf("Unknown mode (#%d)", m)
}

// RegenVoltSet returns the set point for regenerative charging.
func (s *Status) RegenVoltSet() float64 {
	return float64(s.read2(90)) * 46.96 / 4095
}

// StartSupplyVolts returns the input voltage at the start of a cycle.
func (s *Status) StartSupplyVolts() float64 {
	return float64(s.read2(104)) * 46.96 / 4095
}

// SlowAvgAmps returns the average current over a longer period of time.
func (s *Status) SlowAvgAmps() float64 {
	return float64(s.read2s(116)) / 600
}

// PresetSetAmps returns the set point of the preset current.
func (s *Status) PresetSetAmps() float64 {
	return float64(s.read2(118)) / 600
}

// SlavesFound returns the IDs of slaves found.
func (s *Status) SlavesFound() []int {
	bits := s.read2(120)
	slaves := []int{}

	for i := 1; i <= 16; i++ {
		if bit(bits, i) {
			slaves = append(slaves, i)
		}
	}

	return slaves
}

// ErrorCode is only valid in mode 99.
func (s *Status) ErrorCode() int {
	return int(s.read1(134))
}

// Chemistry represents the type of a battery.
type Chemistry int

// Known chemistry types.
const (
	_ = Chemistry(iota)
	LiPo
	LiIon
	A123
	LiMang
	LiCo
	NiCd
	NiMh
	Pb
	LiFE
	Primary
	PowerSupply
)

var chemistryNames = []string{"",
	"Lithium Polymer",
	"Lithium Ion",
	"A123",
	"Lithium Manganese",
	"Lithium Cobalt",
	"NiCd",
	"NiMh",
	"Lead Acid",
	"LiFE",
	"Primary",
	"Power Supply",
}

func (c Chemistry) String() string {
	if int(c) >= len(chemistryNames) {
		return fmt.Sprintf("Unknown (#%d)", c)
	}
	return chemistryNames[c]
}

// Chemistry represents the type of battery being operated on.
func (s *Status) Chemistry() Chemistry {
	return Chemistry(s.read1(135))
}

// Preset is the preset value being executed.
func (s *Status) Preset() int {
	return int(s.read1(137))
}

// ScreenNum is the number of the screen being displayed on the UI.
func (s *Status) ScreenNum() int {
	return int(s.read1(139))
}

// CycleNum is the cycle number the powerlab is currently on during a
// multi-cycle teset.
func (s *Status) CycleNum() int {
	return int(s.read1(142))
}

// PowerReductionReason tells us why we are using less than full power.
type PowerReductionReason int

// Reasons we might reduce power.
const (
	FullPowerAllowed = PowerReductionReason(iota)
	InputCurrentLimit
	SixtyAmpInputCurrentLimitReached
	CellSumErrorCharge
	SupplyNoise
	HighTemp
	LowInputVoltage
	ConstantVoltageOutput
	InternalMax100WDischarge
	HighTempDischarge
	RegenMaxAmpsReached
	HighTempDischarge11
	CellSumErrorDischarge
	RegenVoltLimitReached
	DischargeReduced
	Reduce
	SupplyLow
)

var powerReductionReasons = []string{
	"Full Power Allowed",
	"Input Current Limit",
	"60A Input Current Limit Reached",
	"Cell Sum Error (Charge)",
	"Supply Noise",
	"High Temp",
	"Low Input Voltage",
	"Constant Voltage Output",
	"Internal Max 100W Discharge",
	"High Temp Discharge",
	"Regen. Max Amps Reached",
	"High Temp Discharge",
	"Cell Sum Error (Discharge)",
	"Regen. Volt Limit Reached",
	"Discharge Reduced (Below Average Charger)",
	"Reduce (Above Average Charger)",
	"Supply Low for High Power",
}

func (p PowerReductionReason) String() string {
	if int(p) >= len(powerReductionReasons) {
		return fmt.Sprintf("Unknown (#%d)", p)
	}
	return powerReductionReasons[p]
}

// PowerReductionReason tells us why we are using less than full power.
func (s *Status) PowerReductionReason() PowerReductionReason {
	return PowerReductionReason(s.read1(143))
}

// CRC returned for this status.
func (s *Status) CRC() uint16 {
	return s.read2(statusLen - 2)
}

// ComputeCRC computes a CRC for this status.
func (s *Status) ComputeCRC() uint16 {
	return crc16(s[4 : 4+statusLen-2])
}

// ValidateCRC validates the returned CRC is the same as what we'd calculate.
func (s *Status) ValidateCRC() error {
	if s.ComputeCRC() != s.CRC() {
		return ErrCRC
	}
	return nil
}
