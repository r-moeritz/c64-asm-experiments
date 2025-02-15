        ;; GEOram output module for Profi-Ass
        ;; ----------------------------------
        ;; Called by Profi-Ass; writes object code to GEOram.
        ;; 
        ;; Format:
        ;; +-------------+---------------+-------------+
        ;; |    WORD     |     WORD      |             |
        ;; | data length | start address | object code |
        ;; +-------------+---------------+-------------+
        ;;
        ;; First initialize:
        ;;   SYS49152
        ;; Then assemble to GEOram:
        ;;   .OPT P,O=$C020

        ;; Constants
FIRST_BLOCK:    .equ 0          ;GEOram block at which to start writing data
PA_START:       .equ $80        ;pa_len value indicating start of assembly
PA_STOP:        .equ $c0        ;pa_len value indicating end of assembly
MAX_PAGE:       .equ 64         ;last GEOram page +1
MAX_BLOCK:      .equ 32         ;last GEOram block +1

        ;; OS routines
newline:        .equ $aad7      ;print CRLF        
strout:         .equ $ab1e      ;print 0 terminated string in A (lo) and Y (hi)
linprt:         .equ $bdcd      ;print 16-bit integer in X (lo) and A (hi)
        
        ;; GEOram registers
georam:         .equ $de00      ;first address mapped to GEOram
geoblock:       .equ $dfff      ;16K GEOram block selection register
geopage:        .equ $dffe      ;GEOram page selection register

        ;; Profi-Ass variables to read
pa_op:          .ezp $4b        ;BYTEx3 buffer containing first 3 bytes
pa_len:         .ezp $4e        ;BYTE number of bytes to emit -1
pa_adr:         .ezp $56        ;WORD object code starting address
pa_buf:         .equ $015b      ;buffer for remaining bytes beyond first 3

        ;; Working memory
datalen:        .ezp $a3        ;WORD total bytes written
curblock:       .ezp $a5        ;BYTE current GEOram block (0-31)
curpage:        .ezp $a6        ;BYTE current GEOram page (0-63)
offset:         .ezp $a7        ;BYTE offset within page ($00-$ff)

        ;; Initialize GEOram registers & working memory.
        ;; Must be called before each assembly!
        .org $c000
init:   lda #FIRST_BLOCK
        sta geoblock
        sta curblock
        lda #0
        sta geopage
        sta curpage
        sta datalen
        sta datalen+1        
        lda #2                  ;set offset to 2 to leave space for data length
        sta offset
        rts

        ;; Write object code to GEOram.
        ;; Called by Profi-Ass!
        .align 4
write:  lda pa_len
        cmp #PA_STOP
        beq finish
        cmp #PA_START
        beq start
        ldy #0
        ldx offset
out:    lda pa_op,y
out1:   sta georam,x
        jsr incdatalen          ;increment total bytes written
        inx
        bvs nextpage            ;overflow? next page!
        stx offset
chklen: cpy pa_len
        beq return
        iny
        cpy #3
        bcc out
        lda pa_buf-3,y
        jmp out1
nextpage:
        ldx #0
        stx offset
        inc curpage
        lda curpage
        cmp #MAX_PAGE
        beq nextblock           ;past page 63? next block!
        sta geopage
        jmp chklen
nextblock:
        lda #0
        sta curpage
        sta geopage
        inc curblock
        lda curblock
        cmp #MAX_BLOCK
        beq enomem
        sta geoblock
        jmp chklen
start:  ldx offset
        lda pa_adr
        sta georam,x
        inx
        lda pa_adr+1
        sta georam,x
        inx
        stx offset
return: rts
        ;; Write data length to first word in first block of GEOram
finish: lda #FIRST_BLOCK
        sta geoblock
        sta curblock
        lda #0
        sta geopage
        sta curpage
        ldx #0
        lda datalen,x
        sta georam,x
        inx
        lda datalen,x
        sta georam,x
        jsr printsummary
        rts
enomem: jsr newline
        lda #<outofmem
        ldy #>outofmem
        jsr strout
        rts

        ;; Routine to increment data length
incdatalen:
        clc
        lda datalen
        adc #1
        sta datalen
        lda datalen+1
        adc #0
        sta datalen+1
        rts

        ;; Routine to print summary of data written
printsummary:
        jsr newline
        ldx datalen
        lda datalen+1
        jsr linprt
        lda #<summary
        ldy #>summary
        jsr strout
        ldx #0
        lda #FIRST_BLOCK
        jsr linprt
        rts

summary:
        .string " BYTES WRITTEN TO GEORAM FROM BLOCK "        

outofmem:
        .string "ERROR: GEORAM OUT OF MEMORY"
