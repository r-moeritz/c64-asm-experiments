#!/usr/bin/env bash

NAMES='multi multi2'

for name in $NAMES
do
    vasm6502_oldstyle -cbm-prg -Fbin -chklabels -nocase -dotdir \
                  src/$name.asm -o build/$name.bin -L build/$name.lst \
    && pucrunch -m5 -x4096 build/$name.bin build/$name.prg 
done
