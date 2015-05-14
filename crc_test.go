package powerlab

import "testing"

func TestCRC16Nil(t *testing.T) {
	const exp = 0x1286
	got := crc16(nil)
	if got != exp {
		t.Errorf("Expected %x, got %x", exp, got)
	}
}

func TestCRC16(t *testing.T) {
	input := []byte{0}
	const exp = 0xe12c
	got := crc16(input)
	if got != exp {
		t.Errorf("Expected %x, got %x", exp, got)
	}
}

func TestExemplarCRC(t *testing.T) {
	t.Skipf("Either this doesn't work, or I'm doing it wrong.")
	input := capturedExemplar[:statusLen+4-2]
	const exp = 0
	got := crc16(input)
	if got != exp {
		t.Errorf("Expected %x, got %x", exp, got)
	}
}
