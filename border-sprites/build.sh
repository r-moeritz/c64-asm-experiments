#!/usr/bin/env bash

NAME=border-sprites

vasm6502_oldstyle -cbm-prg -Fbin -chklabels -nocase -dotdir \
                  src/$NAME.asm -o build/$NAME.prg -L build/$NAME.lst
