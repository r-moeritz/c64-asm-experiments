        ;; NMI-based clock/timer
        ;; =====================
        ;;
        ;; CIA2 timers A+B cascaded -> 1 NMI/second (PAL)
        ;; HH:MM:SS clock (BCD sec/min/hour), printed in
        ;; the bottom-right corner of the screen.
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

        ;; System Memory Locations
        ;; -----------------------
nmivec: .equ $0318              ;kernal indirect NMI vector
screen: .equ $07e0              ;screen memory location

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

        ;; Routine to print HH:MM:SS at screen (2 BCD digits each)
print:
        ldx #0
        lda hours
        jsr digits$
        lda #$3a          ;; ':'
        sta screen,x
        inx
        lda minutes
        jsr digits$
        lda #$3a          ;; ':'
        sta screen,x
        inx
        lda seconds
digits$:
        pha
        lsr
        lsr
        lsr
        lsr
        ora #$30          ;; BCD nibble -> screen code
        sta screen,x
        inx
        pla
        and #$0f
        ora #$30
        sta screen,x
        inx
        rts
