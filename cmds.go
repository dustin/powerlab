package powerlab

import (
	"errors"
	"io"
	"strings"
	"time"

	"github.com/dustin/go-rs232"
)

// Powerlab represents a connection to a Powerlab charger.
type Powerlab struct {
	port *rs232.SerialPort
}

// Open a connection to a powerlab.
func Open(port string) (*Powerlab, error) {
	ser, err := rs232.OpenPort(port, 19200, rs232.S_8N1)
	if err != nil {
		return nil, err
	}
	if err := ser.SetNonblock(); err != nil {
		ser.Close()
		return nil, err
	}
	if err := ser.SetInputAttr(1, time.Second*5); err != nil {
		ser.Close()
		return nil, err
	}
	return &Powerlab{ser}, nil
}

// Close the port.
func (p *Powerlab) Close() error {
	return p.port.Close()
}

// ErrTimeout is returned when a read times out.
var ErrTimeout = errors.New("timed out")

func readFullTimeout(r io.Reader, buf []byte, timeout time.Duration) (n int, err error) {
	until := time.Now().Add(timeout)
	for n < len(buf) && err == nil {
		if time.Now().After(until) {
			return n, ErrTimeout
		}
		var nn int
		nn, err = r.Read(buf[n:])
		n += nn
		if err == io.EOF ||
			(err != nil && strings.Contains(err.Error(), "resource temporarily unavailable")) {
			time.Sleep(time.Millisecond * 100)
			err = nil
		}
	}
	if n >= len(buf) {
		err = nil
	} else if n > 0 && err == io.EOF {
		err = io.ErrUnexpectedEOF
	}
	return
}

// Status requests status for the given powerlab on a bus (0 == master).
func (p *Powerlab) Status(id int) (*Status, error) {
	if _, err := p.port.Write([]byte{'R', 'a', 'm', byte(id)}); err != nil {
		return nil, err
	}

	rv := Status{}

	if _, err := readFullTimeout(p.port, rv[:], time.Second*5); err != nil {
		return nil, err
	}

	return &rv, nil
}
