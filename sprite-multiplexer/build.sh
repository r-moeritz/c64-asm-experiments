#!/usr/bin/env bash

NAME=multi2

vasm6502_oldstyle -cbm-prg -Fbin -chklabels -nocase -dotdir \
                  src/$NAME.asm -o build/$NAME.bin -L build/$NAME.lst \
    && pucrunch -x4096 build/$NAME.bin build/$NAME.prg 
