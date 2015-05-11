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
