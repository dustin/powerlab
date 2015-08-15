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

// Status requests status for the given powerlab on a bus (0 == master).
func (p *Powerlab) Status(id int) (*Status, error) {
	if _, err := p.port.Write([]byte{'R', 'a', 'm', byte(id)}); err != nil {
		return nil, err
	}

	rv := Status{}

	if _, err := io.ReadFull(p.port, rv[:]); err != nil {
		if err == io.EOF {
			err = ErrTimeout
		}
		return nil, err
	}

	return &rv, rv.ValidateCRC()
}
