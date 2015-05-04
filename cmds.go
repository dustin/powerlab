package powerlab

import "github.com/dustin/go-rs232"
import "io"

type Powerlab struct {
	port *rs232.SerialPort
}

// Open a connection to a powerlab.
func Open(port string) (*Powerlab, error) {
	ser, err := rs232.OpenPort(port, 19200, rs232.S_8N1)
	if err != nil {
		return nil, err
	}
	return &Powerlab{ser}, nil
}

// Status requests status for the given powerlab on a bus (0 == master).
func (p *Powerlab) Status(id int) (*Status, error) {
	if _, err := p.port.Write([]byte{'R', 'a', 'm', byte(id)}); err != nil {
		return nil, err
	}

	buf := make([]byte, statusLen)
	if _, err := io.ReadFull(p.port, buf); err != nil {
		return nil, err
	}

	return &Status{}, nil
}
