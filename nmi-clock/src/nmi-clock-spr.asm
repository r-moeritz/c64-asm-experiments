        ;; NMI-based clock/timer using sprites
        ;; ===================================
        ;;
        ;; CIA2 timers A+B cascaded -> 1 NMI/second (PAL)
        ;; HH:MM:SS clock (BCD sec/min/hour) displayed with
        ;; 8 sprites in the bottom-right corner of the screen.
        ;; The 8x8 glyphs are copied from the chargen ROM
        ;; ('0'-'9' and ':') into the left column of 11
        ;; 64-byte sprite blocks at $c800.
        ;; -----------------------------------------------
        ;; The VIC-II is switched to bank $c000-$ffff, so it
        ;; sees RAM there: screen at $c400, sprites at $c800,
        ;; sprite pointers at $c7f8-$c7ff.  Before the switch the
        ;; whole chargen charset is copied into the RAM at $d000
        ;; (the CPU reads the chargen ROM at $d000 while writes
        ;; hit the RAM underneath, which the VIC-II then displays
        ;; from bank offset $1000).
        ;; -----------------------------------------------
        ;; Timer A = 20455 (~50.008 Hz on PAL 1.022727 MHz
        ;; CIA clock); timer B counts 50 underflows -> ~1s

        ;; Macros
        ;; ------

        ;; BNE to distant address
jne:    .macro adr
        beq :+
        jmp \adr
:
        .endm

        ;; BCS to distant address
jcs:    .macro adr
        bcc :+
        jmp \adr
:
        .endm

        ;; CIA2 registers
        ;; --------------
ci2pra: .equ $dd00              ;.byte data port register A
c2ddra: .equ $dd02              ;.byte data direction register A
ci2tia: .equ $dd04              ;.word Timer A
ci2tib: .equ $dd06              ;.word Timer B
ci2icr: .equ $dd0d              ;.byte Interrupt ctrl register
ci2cra: .equ $dd0e              ;.byte Timer A ctrl register
ci2crb: .equ $dd0f              ;.byte Timer B ctrl register

        ;; VIC-II registers
        ;; -----------------
sprxlo: .equ $d000              ;.byte sprite X positions (low)
sprylo: .equ $d001              ;.byte sprite Y positions
sprxhi: .equ $d010              ;.byte sprite X positions (high)
spena:  .equ $d015              ;.byte sprite enable
vmcsb:  .equ $d018              ;.byte memory ctrl register
sp0col: .equ $d027              ;.byte sprite colors (0-7)

        ;; System Memory Locations
        ;; -----------------------
nmivec: .equ $0318              ;kernal indirect NMI vector
hibase: .equ $0288              ;high byte of KERNAL screen base
sprptr: .equ $c7f8              ;sprite pointers (end of screen RAM $c400+$3f8)
glyphs: .equ $c800              ;sprite glyph buffer (11 x 64 bytes)
sprbase:.equ $20                ;glyphs offset $0800 / 64 in VIC bank 3

        ;; Sprite anchor = top-left pixel of the first clock cell
        ;; (text row 24, column 32; screen RAM block at $c400).
        ;;
        ;; VIC-II sprite coordinates are measured from the top-left of
        ;; the border, NOT the visible screen.  The visible text area
        ;; starts at sprite coordinate X = 24, Y = 50, so a text cell
        ;; (col,row) maps to:
        ;;     sprite X = 24 + col*8     sprite Y = 50 + row*8
        ;;
        ;; Sprite 0 is the left-most column (32).
sprx0:  .equ 24 + (32*8)            ;sprite 0 X (col 32 -> sprite X 280)
spry0:  .equ 50 + (24*8)            ;sprite Y  (row 24 -> sprite Y 242)

        ;; OS routines
        ;; -----------
frmnum: .equ $ad8a              ;eval numeric expression
comma:  .equ $aefd              ;detect comma in BASIC line
facwrd: .equ $b7f7              ;convert FAC #1 to word at linnum
illqua: .equ $b248              ;raise illegal qty error
setpnt: .equ $e56c              ;refresh KERNAL screen/color pointers

        ;; OS memory
        ;; ---------
linnum: .ezp $14                ;.word BASIC line number

        ;; Temporary working memory (ZP)
        ;; -----------------------------
temp:     .ezp $fa                ;.byte Scratch pad
ptr1:     .ezp $5b                ;.word Scratch pointer
ptr2:     .ezp $5d                ;.word Scratch pointer

        .org $c000

        ;; Jump table
        ;; ----------
jtable: jmp settime
        jmp start
        jmp stop
        jmp restart

        ;; Routines
        ;; --------

        ;; Routine to set the time from user input
        ;; and start the clock.
        ;; Called from BASIC via jump table entry:
        ;;      SYS(49152) hrs (0-23), min (0-59), sec (0-59)
        ;;
        ;; E.g. SYS(49152) 11,51,20
settime:
        ;; Hours
        jsr getwrd$
        lda linnum+1
        jne illqua
        lda linnum
        cmp #24
        jcs illqua
        jsr bin2bcd
        sta hours
        ;; Minutes
        jsr comma
        jsr getwrd$
        lda linnum+1
        jne illqua
        lda linnum
        cmp #60
        jcs illqua
        jsr bin2bcd
        sta minutes
        ;; Seconds
        jsr comma
        jsr getwrd$
        lda linnum+1
        jne illqua
        lda linnum
        cmp #60
        jcs illqua
        jsr bin2bcd
        sta seconds
        jmp start

getwrd$:
        jsr frmnum
        jmp facwrd

        ;; Routine to start the NMI timer
        ;; Called from BASIC via jump table entry:
        ;;      SYS49155
start:
        jsr initspr      ;; build sprite glyphs, position/enable sprites

        lda #$e7         ;; timer A = 20,455 (PAL)
        sta ci2tia
        lda #$4f
        sta ci2tia+1
        lda #50          ;; timer B = 50 A-underflows
        sta ci2tib
        lda #0
        sta ci2tib+1

        lda #%00010001
        sta ci2cra       ;; start timer A
        lda #%01010001
        sta ci2crb       ;; B counts A underflows, start

        lda ci2icr
        ora #%10000010
        sta ci2icr       ;; enable timer B -> NMI

        lda #<isr        ;; hook NMI
        sta nmivec
        lda #>isr
        sta nmivec+1
        rts

        ;; Routine to stop the NMI timer
        ;; Called from BASIC via jump table entry:
        ;;      SYS49158
stop:
        lda #$7f
        sta ci2icr
        lda ci2icr
        rts

        ;; Routine to stop timer, reset time to 00:00:00
        ;; and then restart timer.
        ;; Called from BASIC via jump table entry:
        ;;      SYS49161
restart:
        jsr stop
        lda #0
        sta hours
        sta minutes
        sta seconds
        jmp start

        ;; Routine to convert byte in .A to packed BCD
bin2bcd:
        ldx #0
        tay
tens$:
        cpy #10
        bcc ones$
        tya
        sec
        sbc #10
        tay
        inx
        jmp tens$
ones$:
        txa
        asl
        asl
        asl
        asl
        sta temp
        tya
        ora temp
        rts

        ;; NMI service routine: called once per second
isr:
        ;; Save x,y,acc
        pha
        txa
        pha
        tya
        pha

        ldy ci2icr       ;; read clears pending flag
        tya
        and #$02
        beq exit$

        sed               ;; BCD +1 second, with 60/60/24 rollover
        clc
        lda seconds
        adc #1
        sta seconds
        cmp #$60
        bne done$         ;; seconds < 60: no carry

        lda #0            ;; seconds == 60 -> wrap to :00
        sta seconds
        clc
        lda minutes
        adc #1
        sta minutes
        cmp #$60
        bne done$         ;; minutes < 60: no carry

        lda #0            ;; minutes == 60 -> wrap to :00
        sta minutes
        clc
        lda hours
        adc #1
        sta hours
        cmp #$24
        bne done$         ;; hours < 24: no wrap

        lda #0            ;; hours == 24 -> wrap to 00
        sta hours
done$:
        cld
        jsr print
exit$:
        pla
        tay
        pla
        tax
        pla
        rti

        ;; Routine to build the '0'-'9' and ':' sprite shapes
        ;; from the chargen ROM, then position, color and
        ;; point the 8 clock sprites at them.
initspr:
        sei
        cld

        ;; Preserve the visible screen before moving it from $0400
        ;; to $c400 in the new VIC bank.
        lda #$00
        sta ptr1
        sta ptr2
        lda #$04
        sta ptr1+1
        lda #$c4
        sta ptr2+1
        ldx #4
scr$:
        ldy #0
scrbyte$:
        lda (ptr1),y
        sta (ptr2),y
        iny
        bne scrbyte$
        inc ptr1+1
        inc ptr2+1
        dex
        bne scr$

        ;; Copy all 2 KiB of the chargen ROM into the RAM beneath it.
        ;; Reads see ROM and writes to the same address reach RAM.
        lda $01
        pha                     ;save current CPU config
        lda #$33
        sta $01                 ;LORAM=1, HIRAM=1 CHAREN=0
        lda #$00
        sta ptr1
        lda #$d0
        sta ptr1+1
        ldx #8
chrpage$:
        ldy #0
chrbyte$:
        lda (ptr1),y
        sta (ptr1),y
        iny
        bne chrbyte$
        inc ptr1+1
        dex
        bne chrpage$

        ;; 11 glyphs: src = $d180 + g*8 ('0' = ASCII $30),
        ;;            dst = $c800 + g*64.
        lda #<glyphs
        sta ptr1
        lda #>glyphs
        sta ptr1+1
        ldx #0
gly$:
        txa
        asl
        asl
        asl
        clc
        adc #$80
        sta ptr2
        lda #$d1
        sta ptr2+1           ;; src = $d100 + $80 + g*8
        lda #0               ;; clear the 64-byte sprite block
        ldy #63
zro$:
        sta (ptr1),y
        dey
        bpl zro$
        ldy #0               ;; glyph row n -> sprite byte n*3
row$:
        lda (ptr2),y
        pha
        tya
        sta temp
        asl
        adc temp             ;; 3*n
        tay
        pla
        sta (ptr1),y
        ldy temp
        iny
        cpy #8
        bne row$
        clc                  ;; advance to the next sprite block
        lda ptr1
        adc #64
        sta ptr1
        bcc nxt$
        inc ptr1+1
nxt$:
        inx
        cpx #11
        bne gly$

        ;; Restore I/O visibility before touching CIA2 or VIC-II.
        pla
        sta $01

        ;; VIC bank $c000-$ffff: CIA2 PRA bits 0-1 = %00.
        lda #3                  ;configure the DDR so bits 0-1
        ora c2ddra              ;are output bits
        sta c2ddra
        lda ci2pra
        and #$fc                ;select VIC bank 3 ($c000-$ffff)
        sta ci2pra

        ;; Screen = $0400, charset = $1000 (bank offsets):
        ;; physical $c400 screen (pointers at $c7f8-$c7ff) and
        ;; the charset copied to RAM at $d000.
        lda #$0f
        and vmcsb
        ora #$10
        sta vmcsb
        lda #$f1
        and vmcsb
        ora #$04
        sta vmcsb

        ;; Tell the KERNAL screen editor about the relocated screen,
        ;; then refresh its pointers for the current cursor position.
        lda #$c4
        sta hibase
        jsr setpnt

        ;; sprite X positions (low bytes; hi bits set below):
        ;; cols 32-39 => X = sprx0 + i*8, i.e. 280,288,...,336.
        ldx #0
xlo$:
        txa
        asl
        asl
        clc
        adc #<sprx0
        sta sprxlo,x
        inx
        inx
        cpx #16
        bne xlo$
        ;; sprite Y positions: all on text row 24 (Y = 50+24*8 = 242)
        lda #spry0
        ldx #14
ylo$:
        sta sprylo,x
        dex
        dex
        bpl ylo$
        ;; colors: all white
        lda #$01
        ldx #7
col$:
        sta sp0col,x
        dex
        bpl col$
        ;; enable sprites, set all X high bits (cols 32-39)
        lda #$ff
        sta sprxhi
        sta spena
        ;; draw the current time immediately
        jsr print
        cli
        rts

        ;; Routine to update the 8 clock sprites
        ;; from the BCD time (HH:MM:SS).
print:
        ldx #0           ;; sprite index
        lda hours
        jsr digits$
        lda #10          ;; ':'
        jsr putdgt$
        lda minutes
        jsr digits$
        lda #10          ;; ':'
        jsr putdgt$
        lda seconds
        jmp digits$

        ;; Write sprite pointer for glyph index in .A
putdgt$:
        clc
        adc #sprbase
        sta sprptr,x
        inx
        rts

        ;; Write pointers for the two BCD digits of .A
digits$:
        pha
        lsr
        lsr
        lsr
        lsr
        jsr putdgt$       ;; tens digit
        pla
        and #$0f
        jmp putdgt$       ;; ones digit

        ;; Persistent state must not use zero page: BASIC and the
        ;; KERNAL use almost all of it as volatile working storage.
hours:   .byte 0          ;; Time in hours BCD
minutes: .byte 0          ;; Time in minutes BCD
seconds: .byte 0          ;; Time in seconds BCD
