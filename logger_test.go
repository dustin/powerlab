package powerlab

import (
	"bytes"
	"testing"
	"time"
)

func TestBinaryMarshaling(t *testing.T) {
	le := LogEntry{Timestamp: time.Now(), Raw: (*exemplar)[:], Data: exemplar}
	bs, err := le.MarshalBinary()
	if err != nil {
		t.Fatalf("Error marshaling binary: %v", err)
	}

	le2 := LogEntry{}
	if err := le2.UnmarshalBinary(bs); err != nil {
		t.Fatalf("Error unmarshaling: %v", err)
	}

	if le.Timestamp != le2.Timestamp {
		t.Errorf("Timestamps didn't match:  %v != %v", le.Timestamp, le2.Timestamp)
	}

	if !bytes.Equal(le.Raw, le2.Raw) {
		t.Errorf("Raw bytes were not equal:\n%v\n%v", le.Raw, le2.Raw)
	}

	if !bytes.Equal((*le.Data)[:], (*le2.Data)[:]) {
		t.Errorf("Data bytes were not equal:\n%v\n%v", le.Data, le2.Data)
	}
}
