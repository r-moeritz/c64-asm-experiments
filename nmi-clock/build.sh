#!/usr/bin/env bash

NAMES='nmi-clock-chr nmi-clock-spr'

for name in $NAMES
do
    vasm6502_oldstyle -cbm-prg -Fbin -chklabels -nocase -dotdir \
                  src/$name.asm -o build/$name.prg -L build/$name.lst    
done
