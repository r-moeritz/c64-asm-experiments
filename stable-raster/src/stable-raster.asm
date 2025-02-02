        ;;  Select the video timing (processor clock cycles per raster line)
;; CYCLES: .equ 65    ; 6567R8 and above, NTSC-M
;; CYCLES: .equ 64    ; 6567R5 6A, NTSC-M
CYCLES: .equ 63    ; 6569 (all revisions), PAL-B

cinv:   .equ $314
cnmi:   .equ $318
sp0ptr: .equ $07f8   ; sprite 0 pointer
raslin: .equ 52      ; start of raster interrupt
m:      .ezp $fb     ; zero page variable

        ;; VIC-II registers
vic:    .equ $d000
scroly: .equ vic + $11
raster: .equ vic + $12
spena:  .equ vic + $15
scrolx: .equ vic + $16
vicirq: .equ vic + $19
irqmsk: .equ vic + $1a

        ;; CIA 1 registers
cia1:   .equ $dc00
ci1pra: .equ cia1
ci1icr: .equ cia1 + $0d

        ;; CIA 2 registers
cia2:   .equ $dd00
ti2alo: .equ cia2 + $04
ti2ahi: .equ cia2 + $05
ci2icr: .equ cia2 + $0d
ci2cra: .equ cia2 + $0e
                
        .org $0801
        
basic:
        .word :+,10
        .byte $9e
        .string "2061"
:       .word 0

start:
    jmp install
    jmp deinstall

install:                ;install the raster routine
    jsr restore         ;disable the Restore key (disable NMI interrupts)
checkirq:
    lda cinv            ;check the original IRQ vector
    ldx cinv+1          ;(to avoid multiple installation)
    cmp #<irq1
    bne irqinit
    cpx #>irq1
    beq skipinit
irqinit:
    sei
    sta oldirq          ;store the old IRQ vector
    stx oldirq+1
    lda #<irq1
    ldx #>irq1
    sta cinv            ;set the new interrupt vector
    stx cinv+1
skipinit:
    lda #$1b
    sta scroly          ;set the raster interrupt location
    lda #raslin
    sta raster
    ldx #$e
    clc
    adc #3
    tay
    lda #0
    sta m
:
    lda m
    sta $d000,x         ;set the sprite X
    adc #24
    sta m
    tya
    sta $d001,x         ;and Y coordinates
    lda #sp0def/64
    sta sp0ptr,x        ;and sprite pointers
    dex
    dex
    bpl :-
    lda #$7f
    sta ci1icr          ;disable timer interrupts
    sta ci2icr
    ldx #1
    stx irqmsk          ;enable raster interrupt
    lda ci1icr          ;acknowledge CIA interrupts
    lsr vicirq          ;and video interrupts
    ldy #$ff
    sty spena           ;turn on all sprites
    cli
    rts

deinstall:
    sei                 ;disable interrupts
    lda #$1b
    sta scroly          ;restore text screen mode
    lda #$81
    sta ci1icr          ;enable timer A interrupts on CIA 1
    lda #0
    sta irqmsk          ;disable raster interrupts
    lda oldirq
    sta cinv            ;restore old IRQ vector
    lda oldirq+1
    sta cinv+1
    bit ci2icr          ;re-enable NMI interrupts
    cli
    rts

    ;;  Auxiliary raster interrupt (for synchronization)
irq1:
    ;;  irq (event)   ; > 7 + at least 2 cycles of last instruction (9 to 16 total)
    ;;  pha           ; 3
    ;;  txa           ; 2
    ;;  pha           ; 3
    ;;  tya           ; 2
    ;;  pha           ; 3
    ;;  tsx           ; 2
    ;;  lda $0104,x   ; 4
    ;;  and #xx       ; 2
    ;;  beq           ; 3
    ;;  jmp (cinv)    ; 5
    ;;  ---
    ;;  38 to 45 cycles delay at this stage
    lda #<irq2
    sta cinv
    lda #>irq2
    sta cinv+1
    nop                 ;waste at least 12 cycles
    nop                 ;(up to 64 cycles delay allowed here)
    nop
    nop
    nop
    nop
    inc raster          ;at this stage, raster has already been incremented by one
    lda #1
    sta vicirq          ;acknowledge the first raster interrupt
    cli                 ;enable interrupts (the second interrupt can now occur)
    ldy #9
:   dey
    bne :-              ;delay
    nop                 ;the second interrupt will occur while executing these
    nop                 ;two-cycle instructions
    nop
    nop
    nop
oldirq: .equ * + 1      ;placeholder for self-modifying code
    jmp *               ;return to the original interrupt

    ;;  Main raster interrupt
irq2:
    ;;  irq (event)   ; 7 + 2 or 3 cycles of last instruction (9 or 10 total)
    ;;  pha           ; 3
    ;;  txa           ; 2
    ;;  pha           ; 3
    ;;  tya           ; 2
    ;;  pha           ; 3
    ;;  tsx           ; 2
    ;;  lda $0104,x   ; 4
    ;;  and #xx       ; 2
    ;;  beq           ; 3
    ;;  jmp (cinv)    ; 5
    ;;  ---
    ;;  38 or 39 cycles delay at this stage
    lda #<irq1
    sta cinv
    lda #>irq1
    sta cinv+1
    ldx raster
    nop
    .if CYCLES - 63
    .if CYCLES - 64
    nop                 ;6567R8, 65 cycles/line
    bit $24
    .else
    nop                 ;6567R56A, 64 cycles/line
    nop
    .endif
    else
    bit $24             ;6569, 63 cycles/line
    .endif
    cpx raster          ;the comparison cycle is executed CYCLES or CYCLES+1 cycles
                        ;after the interrupt has occurred
    beq *+2             ;delay by one cycle if raster hadn't changed
    ;;  Now exactly CYCLES+3 cycles have passed since the interrupt
    dex
    dex
    stx raster          ;restore original raster interrupt position
    ldx #1
    stx vicirq          ;acknowledge the raster interrupt
    ldx #2
:   dex
    bne :-              ;delay
    nop
    nop
    lda #20             ;set the amount of raster lines-1 for the loop
    sta m
    ldx #$c8
irqloop:
    ldy #2
:   dey
    bne :-              ;delay
    dec scrolx          ;narrow the screen (exact timing required)
    ;; 3s4s5s6s7srrrrrgggggggggggggggggggggggggggggggggggggggg--||0s1s2s Phi-1 VIC-II
    ;; ssssssssss                                               ||ssssss Phi-2 VIC-II
    ;; ==========xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||XXX====== Phi-2 6510
    ;;           ^ now we are here
    stx scrolx          ;expand the screen
    .if CYCLES - 63
    .if CYCLES - 64
    bit $24             ;6567R8
    .else
    nop                 ;6567R56A
    .endif
    .else
    nop                 ;6569
    .endif
    dec m
    bmi endirq
    clc
    lda scroly
    sbc raster
    and #7
    bne irqloop         ;this instruction takes 4 cycles instead of 3
                        ;because the page boundary is crossed
badline:
    dec m
    nop
    nop
    nop
    nop
    dec scrolx
    ;; 3s4s5s6s7srrrrrgggggggggggggggggggggggggggggggggggggggg--||0s1s2s Phi-1 VIC-II
    ;; ssssssssss    cccccccccccccccccccccccccccccccccccccccc   ||ssssss Phi-2 VIC-II
    ;; ==========xXXX========================================||***====== Phi-2 6510
    ;;           ^ we are here
    stx scrolx
    ;; 3s4s5s6s7srrrrrgggggggggggggggggggggggggggggggggggggggg--||0s1s2s Phi-1 VIC-II
    ;; ssssssssss                                               ||ssssss Phi-2 VIC-II
    ;; ==========xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||XXX====== Phi-2 6510
    ;;           ^ ^^- we are here (6569)
    ;;           | \- or here (6567R56A)
    ;;           \- or here (6567R8)
    ldy #2
:   dey
    bne :-              ;delay
    nop
    nop
    .if CYCLES - 63
    .if CYCLES - 64
    nop                 ;6567R8, 65 cycles/line
    nop
    nop
    .else
    bit $24             ;6567R56A, 64 cycles/line
    .endif
    .else
    nop                 ;6569, 63 cycles/line
    .endif
    dec m
    bpl irqloop         ;this is a 4-cycle branch (page boundary crossed)
endirq:
    jmp $ea81           ;return to the auxiliary raster interrupt

restore:                ;disable the Restore key
    lda cnmi
    ldy cnmi+1
    pha
    lda #<nmi           ;set the NMI vector
    sta cnmi
    lda #>nmi
    sta cnmi+1
    ldx #$81
    stx ci2icr          ;enable CIA 2 Timer A interrupt
    ldx #0
    stx ti2ahi
    inx
    stx ti2alo          ;prepare Timer A to count from 1 to 0
    ldx #$dd
    stx ci2cra          ;cause an interrupt
nmi: .equ * + 1
    lda #$40            ;RTI placeholder
    pla
    sta cnmi
    sty cnmi+1          ;restore original NMI vector (although it won't be used)
    rts

        ;; Sprite shape definitions
        .align 8
sp0def: .blk 57,$ff
        .blk 6,$00
        .align 8
