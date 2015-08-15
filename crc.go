package powerlab

import "errors"

// ErrCRC is returned when a packet fails to return a valid CRC.
var ErrCRC = errors.New("CRC failed")

func crc16(b []byte) uint16 {
	rv := uint32(4742)
	for _, x := range b {
		for i := 0; i < 8; i++ {
			tmp := uint32(x) ^ rv
			if tmp/2 == (tmp-1)/2 {
				rv = 33800 ^ (rv / 2)
			} else {
				rv = rv / 2
			}
			x = x >> 1
		}
	}
	return uint16(rv)
}
