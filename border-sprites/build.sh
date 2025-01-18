#!/usr/bin/env bash

name=border-sprites

vasm6502_oldstyle "./$name.asm" -cbm-prg -Fbin -chklabels -nocase \
                  -o "./$name.prg" -L "./$name.txt"
