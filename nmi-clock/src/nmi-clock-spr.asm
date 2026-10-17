        ;; NMI-based clock/timer using sprites
        ;; ===================================
        ;;
        ;; CIA2 timers A+B cascaded -> 1 NMI/second (PAL)
        ;; HH:MM:SS clock (BCD sec/min/hour) displayed with
        ;; 8 sprites in the bottom-right corner of the screen.
        ;; The 8x8 glyphs are copied from the C64 character
        ;; ROM into the top-left corner of 64-byte sprite
        ;; blocks (11 glyphs: digits 0-9 and ':').
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
sprenab:.equ $d015              ;.byte sprite enable
spcolor:.equ $d027              ;.byte sprite colors (0-7)

        ;; System Memory Locations
        ;; -----------------------
nmivec: .equ $0318              ;kernal indirect NMI vector
sprptr: .equ $07f8              ;sprite data pointers (top of screen RAM)
glyphs: .equ $3000              ;sprite glyph buffer (must be < $4000)

        ;; Sprite anchor = top-left pixel of the first clock cell at
        ;; screen RAM $07e0 (text row 24, column 32).
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

        ;; OS memory
        ;; ---------
linnum: .ezp $14                ;.word BASIC line number

        ;; Working memory (ZP)
        ;; -------------------
hours:    .ezp $f7                ;.byte Time in hours BCD
minutes:  .ezp $f8                ;.byte Time in minutes BCD
seconds:  .ezp $f9                ;.byte Time in seconds BCD
temp:     .ezp $fa                ;.byte Scratch pad
ptr1:     .ezp $5b                ;.word Scratch pointer
ptr2:     .ezp $5d                ;.word Scratch pointer
sprbase:  .ezp $5f                ;.byte Sprite pointer base (glyphs>>6)

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
        jsr initgfx      ;; build sprite glyphs, position/enable sprites

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
loop$:
        cpy #10
        bcc endloop$
        tya
        sec
        sbc #10
        tay
        inx
        jmp loop$
endloop$:
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
        beq endisr$

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
endisr$:
        pla
        tay
        pla
        tax
        pla
        rti

        ;; Routine to copy characters '0'-'9' and ':' from
        ;; the character ROM into the top-left 8x8 corner
        ;; of 11 sprite blocks at 'glyphs', then position
        ;; and enable the 8 clock sprites.
initgfx:
        sei
        lda $01
        pha
        and #%11111011   ;; CHAREN low -> chargen visible at $d000
        sta $01

        ;; Build 11 glyph blocks (digits 0-9, then ':')
        ldx #0           ;; glyph index
ig_loop$:
        ;; dest = glyphs + X*64
        lda #<glyphs
        sta ptr1
        lda #>glyphs
        sta ptr1+1
        txa
        beq ig_hav$
        stx temp
ig_add$:
        clc
        lda ptr1
        adc #64
        sta ptr1
        lda ptr1+1
        adc #0
        sta ptr1+1
        dec temp
        bne ig_add$
ig_hav$:
        ;; src = $d180 + X*8 ('0' is the first glyph)
        txa
        asl
        asl
        asl
        clc
        adc #$80
        sta ptr2
        lda #$d1
        adc #0
        sta ptr2+1
        ;; zero the whole 64-byte sprite block first
        ldy #0
ig_zero$:
        lda #0
        sta (ptr1),y
        iny
        cpy #64
        bne ig_zero$
        ;; Copy the 8 glyph rows into the left 8 columns of the
        ;; sprite.  A sprite row is 3 bytes wide (24 px), so glyph
        ;; row N must go to byte offset N*3 (not N): the glyph
        ;; occupies bytes 0,3,6,...,21 of the sprite block.
        ldy #0
ig_inner$:
        lda (ptr2),y     ;; glyph row Y (0..7)
        pha
        tya
        sta temp         ;; temp = Y (glyph row)
        asl              ;; Y*2
        clc
        adc temp         ;; Y*3 -> byte offset of sprite row Y
        tay
        pla
        sta (ptr1),y     ;; store glyph row in left column
        ldy temp         ;; restore glyph row counter
        iny
        cpy #8
        bne ig_inner$
        inx
        cpx #11
        bne ig_loop$

        ;; restore memory config
        pla
        sta $01
        cli

        ;; sprite X positions (low registers at $d000,$d002,...):
        ;; cols 32-39 => X = sprx0 + i*8, i.e. 280,288,...,336.
        ;; The low byte written here (24..80) is sprx0&$ff + i*8;
        ;; the high bit is provided by sprxhi=$ff below.
        ldx #0
ig_xl$:
        txa
        asl
        asl
        clc
        adc #<sprx0
        sta sprxlo,x
        inx
        inx
        cpx #16
        bne ig_xl$
        ;; sprite Y positions: all on the last text row 24
        ;; (sprite Y = 50 + 24*8 = 242)
        lda #spry0
        sta sprylo
        sta sprylo+2
        sta sprylo+4
        sta sprylo+6
        sta sprylo+8
        sta sprylo+10
        sta sprylo+12
        sta sprylo+14
        ;; colors = white
        lda #$01
        sta spcolor
        sta spcolor+1
        sta spcolor+2
        sta spcolor+3
        sta spcolor+4
        sta spcolor+5
        sta spcolor+6
        sta spcolor+7
        ;; enable all sprites, all X hi bits
        lda #$ff
        sta sprxhi
        sta sprenab
        ;; sprite pointer base = glyphs>>6
        lda #<glyphs
        lsr
        lsr
        lsr
        lsr
        lsr
        lsr
        sta temp
        lda #>glyphs
        asl
        asl
        ora temp
        sta sprbase
        ;; initial display: 00:00:00
        ldx #0
        lda sprbase
ig_ptr$:
        sta sprptr,x
        inx
        cpx #8
        bne ig_ptr$
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
        jsr digits$
        rts

        ;; Write sprite pointer for glyph index in .A
putdgt$:
        clc
        adc sprbase
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
