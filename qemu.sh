#!/bin/bash

qemu-system-x86_64 -hda build/ChoacuryOS.img -serial stdio -audiodev pa,id=snd0 -machine pcspk-audiodev=snd0