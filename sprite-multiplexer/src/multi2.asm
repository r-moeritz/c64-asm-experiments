        ;; Sprite multiplexer example 2 with doublebuffering and sprite
        ;; IRQ preparation
        ;;
        ;; Supports max. 24 sprites. Extracted from the Hessian game
        ;; https://github.com/cadaver/hessian
        ;; 
        ;; Original code by Lasse Oorni.
        ;; 
        ;; Converted to vasm format & replaced register addresses with
        ;; symbols by Ralph Moeritz.

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

        ;; CIA 1 registers

cia1:           .equ $dc00
ci1icr:         .equ cia1 + $0d

        ;; System memory definitions
        
cinv:           .equ $0314      ;IRQ vector
sysirq:         .equ $ea81      ;Kernal IRQ handler
        
        ;; Constant definitions

NUMSPR:         .equ 24         ;Number of sprites used by the main program
MAXSPR:         .equ 24         ;Maximum number of supported sprites

MINSPRY:        .equ 30         ;Minimum visible sprite Y-coordinate
MAXSPRY:        .equ 250        ;Maximum visible sprite Y-coordinate + 1

IRQ1LINE:       .equ $10        ;IRQ at the top of screen

SHOW_RASTIME:   .equ 0          ;Set to 1 to show rastertime used

        ;; Zeropage variable & memory definitions

temp1:          .ezp $02
temp2:          .ezp $03
temp3:          .ezp $04
sprupdateflag:  .ezp $05        ;Update flag for IRQ
sortsprstart:   .ezp $06        ;First used sorted table index (doublebuffered)
sortsprend:     .ezp $07        ;Last used sorted table index + 1

sprorder:       .ezp $40        ;Order table needs to be on zeropage due to addressing modes
                                ;& sorting speed. It also needs to contain 25 elements to contain
                                ;an endmark

screen1:        .equ $0400

        ;; Sprite data

                .org $0fc0
                .blk 64,$ff     ;The sprite frame we are using ($3f)

        ;; Main program

                .org $1000

start:          jsr initsprites                 ;Init the multiplexing-system
                jsr initraster                  ;Init raster interrupts

                ldx #NUMSPR-1
initloop:       lda $e000,x                     ;Init sprites with some random
                sta sprxl,x                     ;values from the KERNAL
                lda $e018,x
                and #$01
                sta sprxh,x
                lda $e030,x
                sta spry,x
                lda #$3f
                sta sprf,x
                txa
                and #$0f
                cmp #$06                        ;Avoid blue as sprite color
                bne colorok                     ;(would look invisible)
                lda #$05
colorok:        sta sprc,x
                dex
                bpl initloop

mainloop:       ldx #NUMSPR-1
moveloop:       lda $e048,x                     ;Move the sprites with some
                and #$03                        ;random speeds
                sec
                adc sprxl,x
                sta sprxl,x
                lda sprxh,x                     ;Update X coordinate high byte, wrap when coordinate
                adc #$00                        ;larger than $180 (arbitrary)
                and #$01
                sta sprxh,x
                beq moveloop_xnotover
                lda sprxl,x
                bpl moveloop_xnotover
                sec
                sbc #$80
                sta sprxl,x
                dec sprxh,x
moveloop_xnotover:
                lda $e060,x
                and #$01
                sec
                adc spry,x
                sta spry,x
                dex
                bpl moveloop
                jsr sortsprites             ;Sort sprites, build sprite IRQ lists and set the update flag
                jmp mainloop                ;Back to loop

        ;; Routine to init the raster interrupt system

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

        ;; Routine to init the sprite multiplexing system

initsprites:    lda #$00                    ;Reset update flag & doublebuffer side
                sta sprupdateflag
                sta sortsprstart
                ldx #MAXSPR                 ;Init the order table with a 0,1,2,3,4,5.. order.
is_orderlist:   txa                         ;Init all Y-coordinates with $ff (unused)
                sta sprorder,x
                lda #$ff
                sta spry,x
                dex
                bpl is_orderlist
                rts

        ;; Routine to sort the sprites, copy them to the sorted table, and
        ;; arrange the sprite IRQ's beforehand

sortsprites:    lda sprupdateflag           ;Wait until IRQ is done with current sprite update
                bne sortsprites
                .if SHOW_RASTIME>0
                inc extcol
                .endif
                lda sortsprstart            ;Switch sprite doublebuffer side
                eor #MAXSPR
                sta sortsprstart
                ldx #$00
                stx temp3                   ;msigx bits for first irq
                txa
sspr_loop1:     ldy sprorder,x              ;Check for Y-coordinates being in order
                cmp spry,y
                beq sspr_noswap2
                bcc sspr_noswap1
                stx temp1                   ;If not in order, begin insertion loop
                sty temp2
                lda spry,y
                ldy sprorder-1,x
                sty sprorder,x
                dex
                beq sspr_swapdone1
sspr_swap1:     ldy sprorder-1,x
                sty sprorder,x
                cmp spry,y
                bcs sspr_swapdone1
                dex
                bne sspr_swap1
sspr_swapdone1: ldy temp2
                sty sprorder,x
                ldx temp1
                ldy sprorder,x
sspr_noswap1:   lda spry,y
sspr_noswap2:   inx
                cpx #MAXSPR
                bne sspr_loop1
                ldx #$00
sspr_findfirst: ldy sprorder,x              ;Find upmost visible sprite
                lda spry,y
                cmp #MINSPRY
                bcs sspr_firstfound
                inx
                bne sspr_findfirst
sspr_firstfound:txa
                adc #<sprorder              ;Add one more, C=1 becomes 0
                sbc sortsprstart            ;subtract one more to cancel out
                sta sspr_copyloop1+1
                ldy sortsprstart
                tya
                adc #8-1                    ;C=1
                sta sspr_copyloop1end+1     ;Set endpoint for first copyloop
                bpl sspr_copyloop1

sspr_copyloop1skip:                         ;Copyloop for the first 8 sprites
                inc sspr_copyloop1+1
sspr_copyloop1: ldx sprorder,y
                lda spry,x                  ;If reach the maximum Y-coord, all done
                cmp #MAXSPRY
                bcs sspr_copyloop1done
                sta sortspry,y
                lda sprc,x                  ;Copy sprite's properties to sorted table
                sta sortsprc,y
                lda sprf,x
                sta sortsprf,y
                lda sprxl,x
                sta sortsprx,y
                lda sprxh,x                 ;Handle sprite X coordinate MSB
                beq sspr_copyloop1msblow
                lda temp3
                ora sprortbl,y
                sta temp3
sspr_copyloop1msblow:
                iny
sspr_copyloop1end:
                cpy #$00
                bcc sspr_copyloop1
                lda temp3
                sta sortsprmsigx-1,y
                lda sortsprc-1,y            ;Make first irq endmark
                ora #$80
                sta sortsprc-1,y
                lda sspr_copyloop1+1        ;Copy sortindex from first copyloop
                sta sspr_copyloop2+1        ;To second
                bcs sspr_copyloop2

sspr_copyloop1done:
                lda temp3
                sta sortsprmsigx-1,y
                sty temp1                   ;Store sorted sprite end index
                cpy sortsprstart            ;Any sprites at all?
                beq sspr_nosprites
                lda sortsprc-1,y            ;Make first (and final) IRQ endmark
                ora #$80                    ;(stored in the color table)
                sta sortsprc-1,y
                jmp sspr_finalendmark
sspr_nosprites: jmp sspr_alldone

sspr_copyloop2skip:                         ;Copyloop for subsequent sprites,
                inc sspr_copyloop2+1        ;with "9th sprite" (physical overlap) prevention
sspr_copyloop2: ldx sprorder,y
                lda spry,x
                cmp #MAXSPRY
                bcs sspr_copyloop2done
                sta sortspry,y
                sbc #21-1
                cmp sortspry-8,y            ;Check for physical sprite overlap
                bcc sspr_copyloop2skip
                lda sprc,x
                sta sortsprc,y
                lda sprf,x
                sta sortsprf,y
                lda sprxl,x
                sta sortsprx,y
                lda sprxh,x
                beq sspr_copyloop2msblow
                lda sortsprmsigx-1,y
                ora sprortbl,y
                bne sspr_copyloop2msbdone
sspr_copyloop2msblow:
                lda sortsprmsigx-1,y
                and sprandtbl,y
sspr_copyloop2msbdone:
                sta sortsprmsigx,y
                iny
                bne sspr_copyloop2

sspr_copyloop2done:
                sty temp1                   ;Store sorted sprite end index
                ldy sspr_copyloop1end+1     ;Go back to the second IRQ start
                cpy temp1
                beq sspr_finalendmark
sspr_irqloop:   sty temp2                   ;Store IRQ startindex
                lda sortspry,y              ;C=0 here
                sbc #21+12-1                ;First sprite of IRQ: store the y-coord
                sta sspr_irqycmp1+1         ;compare values
                adc #21+12+6-1
                sta sspr_irqycmp2+1
sspr_irqsprloop:iny
                cpy temp1
                bcs sspr_irqdone
                lda sortspry-8,y            ;Add next sprite to this IRQ?
sspr_irqycmp1:  cmp #$00                    ;(try to add as many as possible while
                bcc sspr_irqsprloop         ;avoiding glitches)
                lda sortspry,y
sspr_irqycmp2:  cmp #$00
                bcc sspr_irqsprloop
sspr_irqdone:   tya
                sbc temp2
                tax
                lda sprirqadvancetbl-1,x
                ldx temp2
                adc sortspry,x
                sta sprirqline-1,x          ;Store IRQ start line (with advance)
                lda sortsprc-1,y            ;Make endmark
                ora #$80
                sta sortsprc-1,y
                cpy temp1                   ;Sprites left?
                bcc sspr_irqloop
sspr_finalendmark:
                lda #$00                    ;Make final endmark
                sta sprirqline-1,y
sspr_alldone:   sty sortsprend              ;Index of last sorted sprite + 1
                inc sprupdateflag           ;Increment the update flag which will be read by IRQ's
                .if SHOW_RASTIME>0
                dec extcol
                .endif
                rts

        ;; IRQ code
        ;; IRQ at the top of the screen. Take sprite update from the main program and
        ;; start showing the sprites
        
irq1:           lda sprupdateflag           ;New sprites?
                beq irq1_nonewsprites
                lda #$00
                sta sprupdateflag
                lda sortsprstart
                sta irq1_sortsprstart+1     ;Copy sorted sprite start index for IRQ
                lda sortsprend
                sec
                sbc sortsprstart            ;Find out number of sprites
                cmp #$09                    ;More than 8?
                bcc irq1_notover8
                lda #$08
irq1_notover8:  tax
                lda spenatbl,x              ;Take the bit combination for spena
                sta irq1_d015value+1
irq1_nonewsprites:
irq1_d015value: lda #$00                    ;Any sprites?
                sta spena
                bne irq1_hassprites
                inc vicirq
                jmp sysirq                  ;If no sprites, can exit here
irq1_hassprites:
                lda #<irq2
                sta cinv
                lda #>irq2
                sta $0315
irq1_sortsprstart:
                ldx #$00                    ;Go through the first sprite IRQ immediately
                .if SHOW_RASTIME>0
                inc extcol
                .endif

        ;; IRQ for sprite displaying (repeated until done)

irq2_spr0:      lda sortspry,x
                sta sp0y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp0x
                sty msigx
                lda sortsprf,x
                sta screen1+$03f8
                lda sortsprc,x
                sta sp0col
                bmi irq2_sprirqdone2        ;Color high bit functions as IRQ endmark
                inx

irq2_spr1:      lda sortspry,x
                sta sp1y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp1x
                sty msigx
                lda sortsprf,x
                sta screen1+$03f9
                lda sortsprc,x
                sta sp1col
                bmi irq2_sprirqdone2
                inx

irq2_spr2:      lda sortspry,x
                sta sp2y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp2x
                sty msigx
                lda sortsprf,x
                sta screen1+$03fa
                lda sortsprc,x
                sta sp2col
                bmi irq2_sprirqdone2
                inx

irq2_spr3:      lda sortspry,x
                sta sp3y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp3x
                sty msigx
                lda sortsprf,x
                sta screen1+$03fb
                lda sortsprc,x
                sta sp3col
                bpl irq2_tospr4
irq2_sprirqdone2:
                jmp irq2_sprirqdone
irq2_tospr4:    inx

irq2_spr4:      lda sortspry,x
                sta sp4y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp4x
                sty msigx
                lda sortsprf,x
irq2_spr4frame: sta screen1+$03fc
                lda sortsprc,x
                sta sp4col
                bmi irq2_sprirqdone
                inx

irq2_spr5:      lda sortspry,x
                sta sp5y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp5x
                sty msigx
                lda sortsprf,x
irq2_spr5frame: sta screen1+$03fd
                lda sortsprc,x
                sta sp5col
                bmi irq2_sprirqdone
                inx

irq2_spr6:      lda sortspry,x
                sta sp6y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp6x
                sty msigx
                lda sortsprf,x
irq2_spr6frame: sta screen1+$03fe
                lda sortsprc,x
                sta sp6col
                bmi irq2_sprirqdone
                inx

irq2_spr7:      lda sortspry,x
                sta sp7y
                lda sortsprx,x
                ldy sortsprmsigx,x
                sta sp7x
                sty msigx
                lda sortsprf,x
irq2_spr7frame: sta screen1+$03ff
                lda sortsprc,x
                sta sp7col
                bmi irq2_sprirqdone
                inx
irq2_tospr0:    jmp irq2_spr0

irq2_sprirqdone:
                .if SHOW_RASTIME>0
                dec extcol
                .endif
                ldy sprirqline,x            ;Get startline of next IRQ
                beq irq2_alldone            ;(0 if was last)
                inx
                stx irq2_sprindex+1         ;Store next irq sprite start-index
                txa
                and #$07
                tax
                lda sprirqjumptbllo,x       ;Get the correct jump address for next sprite IRQ
                sta irq2_sprjump+1
                lda sprirqjumptblhi,x
                sta irq2_sprjump+2
                tya
                sta raster
                sec
                sbc #$03                    ;Already late from the next IRQ?
                cmp raster
                bcc irq2_direct             ;If yes, execute directly
                inc vicirq                  ;Acknowledge IRQ
                jmp sysirq                  ;Otherwise end IRQ

irq2:
irq2_direct:    .if SHOW_RASTIME>0
                inc extcol
                .endif
irq2_sprindex:  ldx #$00
irq2_sprjump:   jmp irq2_spr0

irq2_alldone:   lda #<irq1
                sta cinv
                lda #>irq1
                sta cinv+1
                lda #IRQ1LINE
                sta raster
                inc vicirq
                jmp sysirq                  ;All spriteIRQ's done, return to the top of screen IRQ

        ;; Tables

sprxl:          .blk MAXSPR,0               ;Unsorted sprite tables to be manipulated by the main program.
sprxh:          .blk MAXSPR,0
spry:           .blk MAXSPR+1,0             ;Y table needs an extra element due to endmark (maximum Y coord $ff)
sprc:           .blk MAXSPR,0
sprf:           .blk MAXSPR,0

sortsprx:       .blk MAXSPR*2,0             ;Sorted sprites are doublebuffered
sortsprmsigx    .blk MAXSPR*2,0
sortspry:       .blk MAXSPR*2,0
sortsprf:       .blk MAXSPR*2,0
sortsprc:       .blk MAXSPR*2,0
sprirqline:     .blk MAXSPR*2,0             ;Table used to control sprite IRQs

sprirqadvancetbl:
                .byte -4,-5,-6,-7,-7,-8,-9,-10   ;raster advance for raster IRQs based on number of sprites in the same IRQ

spenatbl:       .byte %00000000             ;Table of sprites that are "on"
                .byte %00000001             ;for spena
                .byte %00000011
                .byte %00000111
                .byte %00001111
                .byte %00011111
                .byte %00111111
                .byte %01111111
                .byte %11111111

sprortbl:       .byte $01,$02,$04,$08,$10,$20,$40,$80 ;Or table for msigx manipulation, repeated for 2x max sprites (doublebuffer)
                .byte $01,$02,$04,$08,$10,$20,$40,$80
                .byte $01,$02,$04,$08,$10,$20,$40,$80
                .byte $01,$02,$04,$08,$10,$20,$40,$80
                .byte $01,$02,$04,$08,$10,$20,$40,$80
                .byte $01,$02,$04,$08,$10,$20,$40,$80
                
sprandtbl:      .byte $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f  ;And table likewise repeated for 2x max sprites
                .byte $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f
                .byte $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f
                .byte $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f
                .byte $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f
                .byte $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f

sprirqjumptbllo:.byte <irq2_spr0            ;Jump table for starting the spriteIRQ at correct sprite
                .byte <irq2_spr1
                .byte <irq2_spr2
                .byte <irq2_spr3
                .byte <irq2_spr4
                .byte <irq2_spr5
                .byte <irq2_spr6
                .byte <irq2_spr7

sprirqjumptblhi:.byte >irq2_spr0
                .byte >irq2_spr1
                .byte >irq2_spr2
                .byte >irq2_spr3
                .byte >irq2_spr4
                .byte >irq2_spr5
                .byte >irq2_spr6
                .byte >irq2_spr7
