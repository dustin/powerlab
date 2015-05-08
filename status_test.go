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
}

func assert(t *testing.T, name string, got interface{}, expected interface{}) {
	if got != expected {
		t.Errorf("Expected %v for %v, got %v", expected, name, got)
	}
}

func assertEpsilon(t *testing.T, field string, expected, got float64) {
	if math.Abs(got-expected) > 0.05 {
		t.Errorf("Expected %v for %v, got %v -- off by %v",
			expected, field, got, math.Abs(got-expected))
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
}
