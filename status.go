package powerlab

const (
	statusLen = 147
)

// Status represents a Powerlab6 status response.
type Status struct {
}

type internalStatus struct {

	// 0,1 -- 0 - 655.35

	Version uint16

	// 2-17 -- Volts = 16bit * 5.12V / 65536

	Cell1, Cell2, Cell3, Cell4, Cell5, Cell6, Cell7, Cell8 uint16

	// 18-19 -- 0-8191 is Buck, 8192-16383 is Boost

	SyncPWMDrive uint16

	// 20-21 -- Amps = 16bit / 1666

	ChargeCurrent uint16

	// 22-23 -- Volts = 16bit * 46.96V / 4095 / 16

	SupplyVoltsWithCurrent uint16

	// 24-25 -- Volts = 16bit * 46.96V / 4095

	SupplyVolts uint16

	// 26-27 -- Tc = (2.5 * 16bit / 4095 - 0.986) / 0.00355

	CPUTemp uint16

	// 28-29 -- 0 to 18*3600 (Use with charge minutes)

	ChgSec uint16

	// 30-31 -- Amps = 16bit signed / 600

	FastAmps uint16

	// 32-33 -- Volts = 46.96V / 4095

	OutPositive uint16

	// 34-37 -- mAh = 32bit / 2160

	AHrIn uint32

	// 38-39 -- Fuel% = 16bit / 10

	AvgCell uint16

	// 40-41 -- Fuel% = 16bit / 10

	StartAvg uint16

	// 42-43 -- Amps = 16bit signed / 600 (Shows on LCD) USE THIS
	// READING FOR PACK CURRENT.

	AvgAmps uint16

	// 44-46
	// - Bit0 = Safety Charge
	// - Bit8 = Charge/Discharge Complete
	// - Bit11 = Reduce Amps

	StatusFlags uint16

	// 42-47
	// - Bit1 = Discharge Running
	// - Bit4 = Regenerative Discharge
	// - Bit6 = Charge Running
	// - Bit7 = Balancers Running

	RXStatus uint16

	// 48-49

	unused1 uint16

	// 50-51 -- Bit2 = High Temp (140 deg F)

	Status2 uint16

	// 52-67 -- mOhm = ( 16bit / 6.3984 ) / VRAmps

	IR1, IR2, IR3, IR4, IR5, IR6, IR7, IR8 uint16

	// 68-69 -- Amps = 16bit / 600

	VRAmps uint16

	// 70-71 -- Volts = 16bit / 12797 - MaxCell

	NiCdFallbackV uint16

	// 72-73

	unused2 uint16

	// 76-77
	// - Bit4 = Constant Voltage
	// - Bit5 = Preset is Valid and Runable
	// - Bit8 = Regenerative Discharge Failed

	Status6 uint16

	// 78-79
	// For <18hr use ChgSec
	// For >=18hr Seconds = ChgSec – 64800 + ChgMin * 60

	ChgMin uint16

	// 80-81 -- Amps = 16bit / 150

	SupplyAmps uint16

	// 82-83 -- Volts = 16bit / 12797

	BatteryPos uint16

	// 84-97 -- mAh = 32bit / 2160

	AhrOut uint16

	// 88-89

	unused3 uint16

	// 90-91 -- Volts = 16bit * 46.96V / 4095

	RegenVoltSet uint16

	// 92-93 -- Amps = 16bit / 600

	DischAmpsSet uint16

	// 94-95

	DischargePWM uint16

	// 96-99

	unused4, unused5 uint16

	// 100-101 -- Volts = 16bit * 46.96V / 4095

	BattNeg uint16

	// 102-103

	unused6 uint16

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

	// 124-131 -- 0-31

	Bal1PWM, Bal2PWM, Bal3PWM, Bal4PWM, Bal5PWM, Bal6PWM, Bal7PWM, Bal8PWM uint8

	// 132 -- 1 – 8 (0 = no cells detected)

	DetectedCellCount uint8

	// 133
	// - 0 = Charger Ready to Start
	// - 1 = Detecting Pack
	// - 6 = Charging
	// - 7 = Trickle Charging
	// - 8 = Discharging
	// - 9 = Monitoring
	// - 10 = Halt for Safety Screen
	// - 11 = Pack Cool Down (when cycling)
	// - 99 = System Stop Error Occurred

	Mode uint8

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

	Packs uint8

	// 137 -- 0 – 24 (Zero based number)

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
