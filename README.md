# Sound synthesis

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

To allow easy translation to C++ every engine contains custom records (created by `defrecord`) separately for configuration and state. 

## License

Copyright © 2020 Generateme

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
