# cuvave_midi

Parsing of SysEx MIDI messages used by the Cuvave Cube Baby.

### Memory
```
0x05 0x0000 - Settings (LSB bits 4 and 5 define the preset) (u8)
     ...
    =0x002c
...
0x00 0x3000 - USB loopback ON (u8)
0x04 0x0764 - IR data usable? (bool in a u8)
0x04 0x0768 - IR Distance (f32)
0x04 0x076c - ....
     ...            
    =0x0f68 - IR Data [RAM]
...
0x00 0x00069000
     ...
     0x00071000 - IR Data [ROM]
0x05 0x80000000
     ...
    =0x8000002c - Parameter data [RAM]
```
