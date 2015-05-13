package powerlab

import (
	"math"
	"testing"
)

var exemplar = &Status{
	1, 0x3a, // Version
	0xd1, 0xff, 0xb8, 0xff, // cells 1, 2
	0, 0, 0, 0, // cells 3, 4
	0, 0, 0, 0, // cells 5, 6
	0, 0, 0, 0, // cells 7, 8
	0x1f, 0xff, // pwm type
	0x32, 0xc2, // charge current
	0x41, 0x66, // supply volts with current
	0x04, 0x17, // supply volts
	0x07, 0x26, // CPU Temp
	0x07, 0x08, // charge sec
	0x12, 0x48, // fast amps
	0xc9, 0x8d, // output postive
	0, 0x26, 0xa0, 0xc0, // mAh in
	0x1, 0xf4, // avg cell
	0x1, 0x2c, // start avg
	0x12, 0x48, // avg amps
	0x80, 0x20, // status flags
	0x92, 0, // rx status
	0, 0, // unused
	0x40, 0, // status2
	0x2, 0x2, 0, 0, // ir 1, ir 2
	0, 0, 0, 0, // ir 3, ir 4
	0, 0, 0, 0, // ir 5, ir 6
	0, 0, 0, 0, // ir 7, ir 8
	0x12, 0x48, // VRamps -- 68
	0xee, 0xf5, // NiCdFallbackV
	0, 0, // unused
	0xcd, 0x74, // MaxCell volts -- 74-75 -- Volts = 16bit / 12797
	0x11, 0, // status6 -- 76-77
	0, 0, // ChgMin -- 78-79
	0, 0, // supply amps // 80-81
	0, 0, // battery pos // 82-83
	0, 0, 0, 0, // AHr Out // 84-87
	0, 0, // unused -- 88-89
	0, 0, // RegenVoltSet -- 90-91
	0, 0, // DischAmpsSet -- 92-93
	0, 0, // DischargePWM -- 94-95
	0, 0, 0, 0, // unused -- 96-99
	0, 0, // BatNeg -- 100-101
	0, 0, // unused -- 102-103
	0, 0, // start supply volts -- 104-105
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // unused -- 106-115
	0, 0, // SlowAvgAmps -- 116-117
	0, 0, // PresetSetAmps -- 118-119
	0, 0, // SlavesFound -- 120-121
	0, 0, // unused -- 122-123
	0, 0, 0, 0, 0, 0, 0, 0, // Bal*PWM -- 124-131
	3,    // DetectedCellCount -- 132
	0,    // Mode -- 133
	0,    // ErrorCode -- 134
	0,    // Chemistry -- 135
	6,    // Packs -- 136
	0,    // Preset
	0,    // unused
	0,    // Screen Number
	0, 0, // unused
	0,       // CycleNumber
	0,       // PowerReductionReason
	0, 0, 0, // unused
	0, 0, // Checksum
}

func assert(t *testing.T, name string, got interface{}, expected interface{}) {
	if got != expected {
		t.Errorf("Expected %v for %v, got %v", expected, name, got)
	}
}

func assertEpsilon(t *testing.T, field string, got, expected float64) {
	if math.Abs(got-expected) > 0.05 {
		t.Errorf("Expected %v for %v, got %v -- off by %v",
			expected, field, got, math.Abs(got-expected))
	}
}

func TestBit(t *testing.T) {
	val := uint16(0xaaaa)
	exp := []bool{
		true, false,
		true, false,
		true, false,
		true, false,
		true, false,
		true, false,
		true, false,
		true, false,
	}
	for i := 0; i <= 15; i++ {
		if bit(val, i+1) != exp[i] {
			t.Errorf("Expected %v for %v", exp[i], i)
		}
	}
}

func TestReading(t *testing.T) {
	s := exemplar

	assert(t, "version", s.Version(), "3.14")
	assertEpsilon(t, "cell(1)", s.CellVoltage(1), 4.2)
	assertEpsilon(t, "cell(2)", s.CellVoltage(2), 3.7)
	assertEpsilon(t, "cell(3)", s.CellVoltage(3), 0)
	assert(t, "pwm type", s.SyncPWMDrive(), Buck)
	assertEpsilon(t, "charge current", s.ChargeCurrent(), 1.3*6)
	assertEpsilon(t, "supply volts with current", s.SupplyVoltsWithCurrent(), 12)
	assertEpsilon(t, "supply volts", s.SupplyVolts(), 12)
	assertEpsilon(t, "cpu temp", s.CPUTemp(), 37)
	assert(t, "charge sec", s.ChargeSec(), 30*60)
	assertEpsilon(t, "fast amps", s.FastAmps(), 1.3*6)
	assertEpsilon(t, "output positive", s.OutPositive(), 4.2*3)
	assert(t, "mAh in", s.MAHIn(), 1172)
	assertEpsilon(t, "avg cell", s.AvgCell(), 50)
	assertEpsilon(t, "start avg", s.StartAvg(), 30)
	assertEpsilon(t, "avg amps", s.AvgAmps(), 1.3*6)

	safetyCharge, chargeComplete, reduceAmps := s.statusFlags()
	if !safetyCharge {
		t.Error("Expected safety charge")
	}
	if chargeComplete {
		t.Error("Expected charge not complete")
	}
	if !reduceAmps {
		t.Error("Expected reduce amps")
	}

	discharging, regenDischarge, charging, balancing := s.rxStatus()
	if !discharging {
		t.Errorf("Expected to be discharing")
	}
	if !regenDischarge {
		t.Errorf("Expecdted regenerative discharge")
	}
	if charging {
		t.Errorf("Expected not charging")
	}
	if !balancing {
		t.Errorf("Expected balancing.")
	}
	if !s.HighTemp() {
		t.Errorf("Expected high temperature")
	}

	assertEpsilon(t, "vr amps", s.VRAmps(), 1.3*6)
	assertEpsilon(t, "IR 1", s.IR(1), 10.3)
	assertEpsilon(t, "IR 2", s.IR(2), 0)

	assert(t, "detected cell count", s.DetectedCellCount(), 3)
	assert(t, "packs", s.Packs(), 6)

	assertEpsilon(t, "maxcell", s.MaxCell(), 4.11)
	assertEpsilon(t, "NiCad fallback voltage", s.NiCdFallbackV(), 0.67)

	constantVoltage, presetRunnable, regenFailled := s.status6()
	if !constantVoltage {
		t.Errorf("Expected constant voltage")
	}
	if presetRunnable {
		t.Errorf("Expected preset runnable")
	}
	if !regenFailled {
		t.Errorf("Expected regen to have failed")
	}
}
