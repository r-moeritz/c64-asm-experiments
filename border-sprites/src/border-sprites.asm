        ;; Sprites in the top & bottom borders
        
        ;; Observations:
        ;; - y coordinates  0-14: bottom border (15 px)
        ;;                 15-30: bottom border, repeats in top border (16 px)
        ;; - y coordinates 31-49: top border (19 px)

        ;; Symbol definitions
cinv:   .equ $0314
sp0ptr: .equ $07f8
vic:    .equ $d000
sp0x:   .equ vic
sp0y:   .equ vic + $01
sp1x:   .equ vic + $02
sp1y:   .equ vic + $03
sp2x:   .equ vic + $04
sp2y:   .equ vic + $05
sp3x:   .equ vic + $06
sp3y:   .equ vic + $07
sp4x:   .equ vic + $08
sp4y:   .equ vic + $09
sp5x:   .equ vic + $0a
sp5y:   .equ vic + $0b
sp6x:   .equ vic + $0c
sp6y:   .equ vic + $0d
sp7x:   .equ vic + $0e
sp7y:   .equ vic + $0f
scroly: .equ vic + $11
raster: .equ vic + $12
spena:  .equ vic + $15
vicirq: .equ vic + $19
irqmsk: .equ vic + $1a
xxpand: .equ vic + $1d
sp0col: .equ vic + $27
ci1icr: .equ $dc0d
sysirq: .equ $ea7e
linclr: .equ 249
linset: .equ 255
        
        ;; Start of code
        .org $0801

        ;; BASIC header                                                                        
        .word nxl,10
        .byte $9e
        .string "2061"
nxl:    .word 0

        ;; Initialization
        lda #0
        sta $3fff               ;ensure no garbage in borders        
        jsr setupspr            ;setup sprites
        jsr setupirq            ;setup IRQ handler
        
        ;; Busy loop
loop:   nop
        nop
        nop
        nop
        nop
        nop
        nop
        jmp loop

        ;; Setup sprites
setupspr:
        ;; enable all 8 sprites
        lda #$ff
        sta spena
        ;; set sprites 1,2 & 5,6 to double width
        lda #$66
        sta xxpand
        ;; set sprite colours
        ldy #7
        tya
        ldx #0
:       cpx #8
        beq :+
        tya
        sta sp0col,x
        iny
        inx        
        jmp :-
        ;; set sprite pointers
:       lda #sp0def/64
        sta sp0ptr
        sta sp0ptr + 1
        sta sp0ptr + 2
        sta sp0ptr + 3          ;first 4 sprites use shape 0
        lda #sp1def/64
        sta sp0ptr + 4
        sta sp0ptr + 5
        sta sp0ptr + 6
        sta sp0ptr + 7          ;last 4 sprites use shape 1
        ;; set sprite x coordinates
        clc
        lda #24
        sta sp0x
        sta sp4x
        adc #24
        sta sp1x
        sta sp5x
        adc #48
        sta sp2x
        sta sp6x
        adc #48
        sta sp3x
        sta sp7x
        ;; set sprite y coordinates
        lda #31
        sta sp0y
        sta sp1y
        sta sp2y
        sta sp3y                ;first 4 sprites at y=31 (top border)
        lda #0
        sta sp4y
        sta sp5y
        sta sp6y
        sta sp7y                ;last 4 sprites at y=0 (bottom border)
        rts
        
        ;; Setup IRQ handler
setupirq:
        sei
        lda #$7f
        sta ci1icr              ;enable IRQs
        lda #$01
        sta irqmsk              ;enable raster IRQ
        lda #$1b
        sta scroly              ;clear scroly high bit
        lda #linset
        sta raster              ;set scanline for next raster IRQ
        lda #<procirq
        sta cinv
        lda #>procirq
        sta cinv+1              ;set IRQ handler
        cli
        rts

        ;; IRQ handler
procirq:
        lda raster
        cmp #linset
        beq setbit
        ;; Clear bit 3 of scroly
        lda scroly
        and #%11110111
        sta scroly
        lda #linset
        sta raster              ;next IRQ at linset
        jmp finirq
        ;; Set bit 3 of scroly
setbit: lda scroly
        ora #%00001000
        sta scroly
        lda #linclr
        sta raster              ;next IRQ at linclr
finirq: asl vicirq              ;ACK raster IRQ
        jmp sysirq

        ;; Sprite shape definitions
        .align 5
sp0def: .blk 24,$ff
        .blk 39,$00
        .align 5
sp1def: .blk 45,$ff
        .blk 18,$00
