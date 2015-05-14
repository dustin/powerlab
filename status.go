package powerlab

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"math"
	"time"
)

const (
	statusLen = 149
)

// Status represents a Powerlab6 status response.
type Status [statusLen + 4]byte

func (s *Status) MarshalJSON() ([]byte, error) {
	m := map[string]interface{}{
		"version":                   s.Version(),
		"sync_pwm_drive":            s.SyncPWMDrive().String(),
		"charge_current":            s.ChargeCurrent(),
		"supply_volts_with_current": s.SupplyVoltsWithCurrent(),
		"supply_volts":              s.SupplyVolts(),
		"cpu_temp":                  s.CPUTemp(),
		"charge_sec":                s.ChargeDuration().Seconds(),
		"charge_time":               s.ChargeDuration().String(),
		"fast_amps":                 s.FastAmps(),
		"out_pos":                   s.OutPositive(),
		"mah_in":                    s.MAHIn(),
		"avg_cell":                  s.AvgCell(),
		"start_avg_cell":            s.StartAvg(),
		"avg_amps":                  s.AvgAmps(),
		"high_temp":                 s.HighTemp(),
		"packs":                     s.Packs(),
		"detected_cell_count":       s.DetectedCellCount(),
		"nicd_fallback":             s.NiCdFallbackV(),
		"max_cell":                  s.MaxCell(),
		"supply_amps":               s.SupplyAmps(),
		"battery_pos":               s.BatteryPos(),
		"mah_out":                   s.MAhrOut(),
		"discharge_amps_set":        s.DischAmpSet(),
		"discharge_pwm":             s.DischargePWM(),
		"battery_neg":               s.BatteryNeg(),
		"mode":                      s.Mode().String(),
	}
	voltages := []float64{}
	ir := []float64{}
	balPwm := []int{}
	for i := 0; i < 8; i++ {
		voltages = append(voltages, s.CellVoltage(i+1))
		ir = append(ir, s.IR(i+1))
		balPwm = append(balPwm, s.BalancePWM(i+1))
	}
	m["voltage"] = voltages
	m["ir"] = ir
	m["bal_pwm"] = balPwm

	safetyCharge, chargeComplete, reduceAmps := s.statusFlags()
	m["safety_charge"] = safetyCharge
	m["charge_complete"] = chargeComplete
	m["reduce_amps"] = reduceAmps

	discharging, regenDischarge, charging, balancing := s.rxStatus()
	m["discharging"] = discharging
	m["regen_discharge"] = regenDischarge
	m["charging"] = charging
	m["balancing"] = balancing

	return json.Marshal(m)
}

func (s *Status) read1(o int) uint8 {
	o += 4
	return (*s)[o]
}

func (s *Status) read2(o int) uint16 {
	o += 4
	return binary.BigEndian.Uint16((*s)[o : o+2])
}

func (s *Status) read4(o int) uint32 {
	o += 4
	return binary.BigEndian.Uint32((*s)[o : o+4])
}

// The Version of the powerlab firmware.
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

type PWMType bool

const (
	Buck  = PWMType(false)
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
	return float64(s.read2(30)) / 600
}

// OutPositive is the positive output voltage towards the batteries.
func (s *Status) OutPositive() float64 {
	return float64(s.read2(32)) / 4095.0
}

// MAHIn is the number of milliamp hours sent into the batteries.
func (s *Status) MAHIn() int {
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
	return float64(s.read2(42)) / 600
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
	if math.IsNaN(ir) {
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

func (s *Status) SupplyAmps() float64 {
	return float64(s.read2(80)) / 150
}

func (s *Status) BatteryPos() float64 {
	return float64(s.read2(82)) / 12797
}

func (s *Status) MAhrOut() int {
	return int(s.read4(84)) / 2160
}

func (s *Status) DischAmpSet() float64 {
	return float64(s.read2(92)) / 600
}

func (s *Status) DischargePWM() int {
	return int(s.read2(94))
}

func (s *Status) BatteryNeg() float64 {
	return float64(s.read2(100)) * 46.96 / 4095
}

func (s *Status) BalancePWM(cell int) int {
	if cell < 0 || cell > 8 {
		panic("invalid cell number")
	}
	return int(s.read1(124 + (cell * 2)))
}

type Mode int

const (
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
	Ready:           "ready",
	DetectingPack:   "detecting pack",
	Charging:        "charging",
	TrickleCharging: "trickle charing",
	Discharging:     "discharging",
	Monitoring:      "monitorinng",
	HaltForSafety:   "halt for safety",
	PackCoolDown:    "pack cool down",
	SystemStopError: "system stop",
}

func (s *Status) Mode() Mode {
	return Mode(s.read1(133))
}

func (m Mode) String() string {
	return modeNames[m]
}

type internalStatus struct {
	// 90-91 -- Volts = 16bit * 46.96V / 4095

	RegenVoltSet uint16

	// 104-105 -- Volts = 16bit * 46.96V / 4095

	StartSupplyVolts uint16

	// 106-115

	unused7, unused8, unused9, unused10, unused11 uint16

	// 116-117 -- Amps = 16bit signed / 600

	SlowAvgAmps uint16

	// 118-119 -- Amps = 16bit / 600 (never changes with temp or power)

	PresetSetAmps uint16

	// 120-121 -- Each bit represents a slave charger that is found

	SlavesFound uint16

	// 122-123

	unused12 uint16

	// 134 -- (only valid in mode 99)

	ErrorCode uint8

	// 135
	// - 1 = Lithium Polymer
	// - 2 = Lithium Ion
	// - 3 = A123
	// - 4 = Lithium Manganese
	// - 5 = Lithium Cobalt
	// - 6 = NiCd
	// - 7 = NiMh
	// - 8 = Lead Acid
	// - 9 = LiFE
	// - 10 = Primary
	// - 11 = Power Supply

	Chemistry uint8

	// 136 -- Number of packs connected

	Preset uint8

	// 138

	unused13 uint8

	// 139 -- Screen showing on LCD

	ScreenNumber uint8

	// 140, 141

	unused14, unused15 uint8

	// 142 -- 0 – 255 (A complete Charge/Discharge is one cycle)

	CycleNumber uint8

	// 143
	// - 0 = Full Power Allowed
	// - 1 = Input Current Limit
	// - 2 = 60A Input Current Limit Reached
	// - 3 = Cell Sum Error (Charge)
	// - 4 = Supply Noise
	// - 5 = High Temp
	// - 6 = Low Input Voltage
	// - 7 = Constant Voltage Output
	// - 8 = Internal Max 100W Discharge
	// - 9 = High Temp Discharge
	// - 10 = Regen. Max Amps Reached
	// - 11 = High Temp Discharge
	// - 12 = Cell Sum Error (Discharge)
	// - 13 = Regen. Volt Limit Reached
	// - 14 = Discharge Reduced (Below Average Charger)
	// - 15 = Reduce (Above Average Charger)
	// - 16 = Supply Low for High Power

	PowerReductionReason uint8

	// 144-146

	unused16, unused17, unused18 uint8

	// 147-148

	Checksum uint16
}
