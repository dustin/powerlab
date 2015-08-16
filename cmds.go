package powerlab

import (
	"errors"
	"io"
	"time"

	"github.com/tarm/serial"
)

// Powerlab represents a connection to a Powerlab charger.
type Powerlab struct {
	port *serial.Port
}

// Open a connection to a powerlab.
func Open(port string) (*Powerlab, error) {
	ser, err := serial.OpenPort(&serial.Config{
		Name:        port,
		Baud:        19200,
		ReadTimeout: time.Second * 5,
	})
	if err != nil {
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

func readPrefix(r io.Reader, cmd []byte) error {
	reading := cmd
	buf := []byte{0}
	timeout := time.Now().Add(time.Second * 5)

	for time.Now().Before(timeout) {
		_, err := r.Read(buf)
		if err != nil {
			return err
		}
		if buf[0] == reading[0] {
			reading = reading[1:]
		} else {
			reading = cmd
		}
		if len(reading) == 0 {
			return nil
		}
	}

	return ErrTimeout
}

// Status requests status for the given powerlab on a bus (0 == master).
func (p *Powerlab) Status(id int) (*Status, error) {
	cmd := []byte{'R', 'a', 'm', byte(id)}
	if _, err := p.port.Write(cmd); err != nil {
		return nil, err
	}

	rv := Status{}
	copy(rv[:], cmd)

	if err := readPrefix(p.port, cmd); err != nil {
		return nil, err
	}

	if _, err := io.ReadFull(p.port, rv[4:]); err != nil {
		if err == io.EOF {
			err = ErrTimeout
		}
		return nil, err
	}

	return &rv, rv.ValidateCRC()
}
