package powerlab

func crc16(b []byte) uint16 {
	rv := uint16(4742)
	for _, x := range b {
		for i := 0; i < 8; i++ {
			tmp := uint16(x) ^ rv
			if tmp/2 == (tmp-1)/2 {
				rv = 33800 ^ (rv / 2)
			} else {
				rv = rv / 2
			}
			x = x >> 1
		}
	}
	return rv
}
