# Sound synthesis from the scratch

Waveform based sound synthesis engine. It's a proof of concept helping implementing target synthesizer on STM32 ARM processor.

What's included:

1. BPM Clock with trigger and gate
2. Sequencer
3. ADSR envelope
4. State Variable Filter
5. Phase modulation
6. Waveform generators + collection imported from [Plaits](https://mutable-instruments.net/modules/plaits/)
7. Waveform oscillator

To play a sound load and run `sound.core` (use `toggle-playing` to start/stop sound generation). `sound.patch` defines whole configuration of the synthesizer you can change it live.

## Implementation notes

Structure of the code is created to make C++ translation as easy as possible. This requires semi-object programming - custom types (records) and functions operating on them.

Implementation is highly based on [Mutable Instruments](https://mutable-instruments.net/) source code.

## Sound quality

* Sample rate: 44100.0Hz
* Bit depth: 8 (-128 to 127)
* mono
* 3 independent tracks (sequencers) mixed together.

## TODO

* echo / delay
* resonator (implemented, not tested)
* waveshapers
* build full collection of waveforms
* write blog post about this implementation
* translate to C++

## License

Copyright © 2020 GenerateMe

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
