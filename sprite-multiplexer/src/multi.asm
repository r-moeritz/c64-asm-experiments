;------------------------------------------------------------------------------;
; Spritemultiplexing example V2.1                                              ;
; by Lasse Oorni (loorni@gmail.com)                                            ;
; Available at http://cadaver.github.io                                        ;
;                                                                              ;
; Quite easy (?) to understand example how to make a spritemultiplexer,        ;
; using 32 sprites. The routine is capable of more but the screen starts       ;
; to become very crowded, as they move randomly...                             ;
;                                                                              ;
; Uses a "new" more optimal sortmethod that doesn't take as much time          ;
; as bubblesort. This method is based on the idea of an orderlist that         ;
; is not recreated from scratch each frame; but instead modified every         ;
; frame to create correct top-bottom order of sprites.                         ;
;                                                                              ;
; Why sorted top-bottom order of sprites is necessary for multiplexing:        ;
; because raster interrupts are used to "rewrite" the sprite registers         ;
; in the middle of the screen and raster interrupts follow the                 ;
; top->bottom movement of the TV/monitor electron gun as it draws each         ;
; frame.                                                                       ;
;                                                                              ;
; Light grey color in the bottom of the screen measures the time taken         ;
; by sprite sorting.                                                           ;
;                                                                              ;
; What is missing from this tutorial for sake of simplicity:                   ;
; * 16-bit X coordinates (it's now multiplying the X-coord by 2)               ;
; * Elimination of "extra" (more than 8) sprites on a row                      ;
;                                                                              ;
; Original code by Lasse Oorni.                                                ;
;                                                                              ;
; Converted to vasm format & replaced register addresses with                  ;
; symbols by Ralph Moeritz.                                                    ;
;------------------------------------------------------------------------------;

        ;; VIC-II registers
        
vic:            .equ $d000
sp0x:           .equ vic
sp0y:           .equ vic + $01
sp1x:           .equ vic + $02
sp1y:           .equ vic + $03
sp2x:           .equ vic + $04
sp2y:           .equ vic + $05
sp3x:           .equ vic + $06
sp3y:           .equ vic + $07
sp4x:           .equ vic + $08
sp4y:           .equ vic + $09
sp5x:           .equ vic + $0a
sp5y:           .equ vic + $0b
sp6x:           .equ vic + $0c
sp6y:           .equ vic + $0d
sp7x:           .equ vic + $0e
sp7y:           .equ vic + $0f
msigx:          .equ vic + $10
scroly:         .equ vic + $11
raster:         .equ vic + $12
spena:          .equ vic + $15
vicirq:         .equ vic + $19
irqmsk:         .equ vic + $1a
extcol:         .equ vic + $20
sp0col:         .equ vic + $27
sp1col:         .equ vic + $28
sp2col:         .equ vic + $29
sp3col:         .equ vic + $2a
sp4col:         .equ vic + $2b
sp5col:         .equ vic + $2c
sp6col:         .equ vic + $2d
sp7col:         .equ vic + $2e
sp0ptr:         .equ $07f8
        
        ;; CIA 1 registers

cia1:           .equ $dc00
ci1icr:         .equ cia1 + $0d

        ;; System memory definitions
        
cinv:           .equ $0314      ;IRQ vector
sysirq:         .equ $ea81      ;Kernal IRQ handler

        ;; Constants

IRQ1LINE        .equ $fc           ;This is the place on screen where the sorting
                                   ;IRQ happens
IRQ2LINE        .equ $2a           ;This is where sprite displaying begins...

MAXSPR          .equ 32            ;Maximum number of sprites

        ;; Zero-Page
        
numsprites      .equ $02           ;Number of sprites that the main program wants
                                   ;to pass to the sprite sorter
sprupdateflag   .equ $03           ;Main program must write a nonzero value here
                                   ;when it wants new sprites to be displayed
sortedsprites   .equ $04           ;Number of sorted sprites for the raster
                                   ;interrupt
tempvariable    .equ $05           ;Just a temp variable used by the raster
                                   ;interrupt
sprirqcounter   .equ $06           ;Sprite counter used by the interrupt

sortorder       .equ $10           ;Order-table for sorting. Needs as many bytes
sortorderlast   .equ $2f           ;as there are sprites.

        ;; Sprite shape definition
        
                .org $0fc0

                .blk 64,$ff        ;Our sprite. Really complex design :-)

        ;; Main program
        
                .org $1000

start:          jsr initsprites             ;Init the multiplexing-system
                jsr initraster
                ldx #MAXSPR                 ;Use all sprites
                stx numsprites

                dex
initloop:       lda $e000,x                     ;Init sprites with some random
                sta sprx,x                      ;values from the KERNAL
                lda $e010,x
                sta spry,x
                lda #$3f
                sta sprf,x
                txa
                cmp #$06                        ;Blue is the default background
                bne colorok                     ;color, so sprite would look
                lda #$05                        ;invisible :-)
colorok:        sta sprc,x
                dex
                bpl initloop

mainloop:       inc sprupdateflag               ;Signal to IRQ: sort the
                                                ;sprites
waitloop:       lda sprupdateflag               ;Wait until the flag turns back
                bne waitloop                    ;to zero
                ldx #MAXSPR-1
moveloop:       lda $e040,x                     ;Move the sprites with some
                and #$03                        ;random speeds
                sec
                adc sprx,x
                sta sprx,x
                lda $e050,x
                and #$01
                sec
                adc spry,x
                sta spry,x
                dex
                bpl moveloop
                jmp mainloop                    ;Back to loop

        ;Routine to init the raster interrupt system

initraster:     sei
                lda #<irq1
                sta cinv
                lda #>irq1
                sta cinv+1
                lda #$7f                    ;CIA interrupt off
                sta ci1icr
                lda #$01                    ;Raster interrupt on
                sta irqmsk
                lda #27                     ;High bit of interrupt position = 0
                sta scroly
                lda #IRQ1LINE               ;Line where next IRQ happens
                sta raster
                lda ci1icr                  ;Acknowledge IRQ (to be sure)
                cli
                rts

        ;Routine to init the sprite multiplexing system

initsprites:    lda #$00
                sta sortedsprites
                sta sprupdateflag
                ldx #MAXSPR-1                   ;Init the order table with a
is_orderlist:   txa                             ;0,1,2,3,4,5... order
                sta sortorder,x
                dex
                bpl is_orderlist
                rts

        ;Raster interrupt 1. This is where sorting happens.

irq1:           dec vicirq                      ;Acknowledge raster interrupt
                lda #$ff                        ;Move all sprites
                sta sp0y                        ;to the bottom to prevent
                sta sp1y                        ;weird effects when sprite
                sta sp2y                        ;moves lower than what it
                sta sp3y                        ;previously was
                sta sp4y
                sta sp5y
                sta sp6y
                sta sp7y

                lda sprupdateflag               ;New sprites to be sorted?
                beq irq1_nonewsprites
                lda #$00
                sta sprupdateflag
                lda numsprites                  ;Take number of sprites given
                                                ;by the main program
                sta sortedsprites               ;If itïs zero, donït need to
                bne irq1_beginsort              ;sort

irq1_nonewsprites:
                ldx sortedsprites
                cpx #$09
                bcc irq1_notmorethan8
                ldx #$08
irq1_notmorethan8:
                lda spenatbl,x                   ;Now put the right value to
                sta spena                       ;$d015, based on number of
                beq irq1_nospritesatall         ;sprites
                                                ;Now init the sprite-counter
                lda #$00                        ;for the actual sprite display
                sta sprirqcounter               ;routine
                lda #<irq2                      ;Set up the sprite display IRQ
                sta cinv
                lda #>irq2
                sta cinv+1
                jmp irq2_direct                 ;Go directly; we might be late
irq1_nospritesatall:
                jmp sysirq

irq1_beginsort: ldx #MAXSPR
                dex
                cpx sortedsprites
                bcc irq1_cleardone
                lda #$ff                        ;Mark unused sprites with the
irq1_clearloop: sta spry,x                      ;lowest Y-coordinate ($ff);
                dex                             ;these will "fall" to the
                cpx sortedsprites               ;bottom of the sorted table
                bcs irq1_clearloop
irq1_cleardone: ldx #$00
irq1_sortloop:  ldy sortorder+1,x               ;Sorting code. Algorithm
                lda spry,y                      ;ripped from Dragon Breed :-)
                ldy sortorder,x
                cmp spry,y
                bcs irq1_sortskip
                stx irq1_sortreload+1
irq1_sortswap:  lda sortorder+1,x
                sta sortorder,x
                sty sortorder+1,x
                cpx #$00
                beq irq1_sortreload
                dex
                ldy sortorder+1,x
                lda spry,y
                ldy sortorder,x
                cmp spry,y
                bcc irq1_sortswap
irq1_sortreload:ldx #$00
irq1_sortskip:  inx
                cpx #MAXSPR-1
                bcc irq1_sortloop
                ldx sortedsprites
                lda #$ff                       ;$ff is the endmark for the
                sta sortspry,x                 ;sprite interrupt routine
                ldx #$00
irq1_sortloop3: ldy sortorder,x                ;Final loop:
                lda spry,y                     ;Now copy sprite variables to
                sta sortspry,x                 ;the sorted table
                lda sprx,y
                sta sortsprx,x
                lda sprf,y
                sta sortsprf,x
                lda sprc,y
                sta sortsprc,x
                inx
                cpx sortedsprites
                bcc irq1_sortloop3
                jmp irq1_nonewsprites

        ;Raster interrupt 2. This is where sprite displaying happens

irq2:           dec vicirq                      ;Acknowledge raster interrupt
irq2_direct:    ldy sprirqcounter               ;Take next sorted sprite number
                lda sortspry,y                  ;Take Y-coord of first new sprite
                clc
                adc #$10                        ;16 lines down from there is
                bcc irq2_notover                ;the endpoint for this IRQ
                lda #$ff                        ;Endpoint canït be more than $ff
irq2_notover:   sta tempvariable
irq2_spriteloop:lda sortspry,y
                cmp tempvariable                ;End of this IRQ?
                bcs irq2_endspr
                ldx physicalsprtbl2,y           ;Physical sprite number x 2
                sta sp0y,x                     ;for X & Y coordinate
                lda sortsprx,y
                asl
                sta sp0x,x
                bcc irq2_lowmsb
                lda msigx
                ora ortbl,x
                sta msigx
                jmp irq2_msbok
irq2_lowmsb:    lda msigx
                and andtbl,x
                sta msigx
irq2_msbok:     ldx physicalsprtbl1,y           ;Physical sprite number x 1
                lda sortsprf,y
                sta sp0ptr,x                    ;for color & frame
                lda sortsprc,y
                sta sp0col,x
                iny
                bne irq2_spriteloop
irq2_endspr:    cmp #$ff                        ;Was it the endmark?
                beq irq2_lastspr
                sty sprirqcounter
                sec                             ;That coordinate - $10 is the
                sbc #$10                        ;position for next interrupt
                cmp raster                      ;Already late from that?
                bcc irq2_direct                 ;Then go directly to next IRQ
                sta raster
                jmp sysirq
irq2_lastspr:   lda #<irq1                      ;Was the last sprite,
                sta cinv                       ;go back to irq1
                lda #>irq1                      ;(sorting interrupt)
                sta cinv+1
                lda #IRQ1LINE
                sta raster
                jmp sysirq

sprx:           .blk MAXSPR,0                   ;Unsorted sprite table
spry:           .blk MAXSPR,0
sprc:           .blk MAXSPR,0
sprf:           .blk MAXSPR,0

sortsprx:       .blk MAXSPR,0                   ;Sorted sprite table
sortspry:       .blk MAXSPR+1,0                 ;Must be one byte extra for the
                                                ;$ff endmark
sortsprc:       .blk MAXSPR,0
sortsprf:       .blk MAXSPR,0

spenatbl        .byte %00000000                  ;Table of sprites that are "on"
                .byte %00000001                  ;for $d015
                .byte %00000011
                .byte %00000111
                .byte %00001111
                .byte %00011111
                .byte %00111111
                .byte %01111111
                .byte %11111111

physicalsprtbl1:.byte 0,1,2,3,4,5,6,7            ;Indexes to frame & color
                .byte 0,1,2,3,4,5,6,7            ;registers
                .byte 0,1,2,3,4,5,6,7
                .byte 0,1,2,3,4,5,6,7
                .byte 0,1,2,3,4,5,6,7
                .byte 0,1,2,3,4,5,6,7
                .byte 0,1,2,3,4,5,6,7
                .byte 0,1,2,3,4,5,6,7

physicalsprtbl2:.byte 0,2,4,6,8,10,12,14
                .byte 0,2,4,6,8,10,12,14
                .byte 0,2,4,6,8,10,12,14
                .byte 0,2,4,6,8,10,12,14
                .byte 0,2,4,6,8,10,12,14
                .byte 0,2,4,6,8,10,12,14
                .byte 0,2,4,6,8,10,12,14
                .byte 0,2,4,6,8,10,12,14

andtbl:         .byte 255-1
ortbl:          .byte 1
                .byte 255-2
                .byte 2
                .byte 255-4
                .byte 4
                .byte 255-8
                .byte 8
                .byte 255-16
                .byte 16
                .byte 255-32
                .byte 32
                .byte 255-64
                .byte 64
                .byte 255-128
                .byte 128

