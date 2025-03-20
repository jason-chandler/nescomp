;;; Generated NES 6502 code for ca65 assembler

.segment "CODE"

.segment "HEADER"
    .byte "EXAMPLE"
ROMSPEC:
    .byte $30
    .byte 0
    .byte $07
    .byte 0, 0, 0, 0
    .word $AAAA, $5555
.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ
.segment "ZEROPAGE"
SCORE:
    .res 1
SAVE_CONTEXT:
    pla 
    sta RETURN_ADDR
    pla 
    sta RETURN_ADDR+1
    rts 
RESTORE_CONTEXT:
    lda RETURN_ADDR+1
    pha 
    lda RETURN_ADDR
    pha 
    rts 
ADD6502:
    clc 
    adc TEMP1
    rts 
SUB6502:
    sec 
    sbc TEMP1
    rts 
MUL6502:
    sta MULTIPLICAND
    sty MULTIPLIER
    lda #0
    sta PRODUCT
    ldx #%00001000
MUL_LOOP:
    lsr MULTIPLIER
    bcc MUL_SKIP
    clc 
    adc MULTIPLICAND
MUL_SKIP:
    asl MULTIPLICAND
    dex 
    bne MUL_LOOP
    rts 
INIT_HEAP:
    lda #<HEAP_START
    sta HEAP_PTR
    lda #>HEAP_START
    sta HEAP_PTR+1
    lda #0
    sta HEAP_USED
    rts 
ALLOC:
    sta ALLOC_SIZE
    lda HEAP_PTR
    sta ALLOC_RESULT
    lda HEAP_PTR+1
    sta ALLOC_RESULT+1
    lda HEAP_PTR
    clc 
    adc ALLOC_SIZE
    sta HEAP_PTR
    lda HEAP_PTR+1
    adc #0
    sta HEAP_PTR+1
    lda ALLOC_RESULT
    ldx ALLOC_RESULT+1
    rts 
CONS:
    sta CONS_CAR
    stx CONS_CDR
    sty CONS_CDR+1
    lda #4
    jsr ALLOC
    sta CONS_PTR
    stx CONS_PTR+1
    ldy #0
    lda CONS_CAR
    sta CONS_PTR, y
    iny 
    lda #0
    sta CONS_PTR, y
    iny 
    lda CONS_CDR
    sta CONS_PTR, y
    iny 
    lda CONS_CDR+1
    sta CONS_PTR, y
    lda CONS_PTR
    ldx CONS_PTR+1
    rts 
.segment "ZEROPAGE"
TEMP1:
    .res 1
TEMP2:
    .res 1
PTR:
    .res %00000010
RETURN_ADDR:
    .res %00000010
MULTIPLICAND:
    .res 1
MULTIPLIER:
    .res 1
PRODUCT:
    .res 1
HEAP_PTR:
    .res %00000010
HEAP_USED:
    .res 1
ALLOC_SIZE:
    .res 1
ALLOC_RESULT:
    .res %00000010
CONS_CAR:
    .res 1
CONS_CDR:
    .res %00000010
CONS_PTR:
    .res %00000010
ZP_PARAM0:
    .res 1
ZP_PARAM1:
    .res 1
ZP_PARAM2:
    .res 1
ZP_PARAM3:
    .res 1
ZP_LOCAL0:
    .res 1
ZP_LOCAL1:
    .res 1
ZP_LOCAL2:
    .res 1
ZP_LOCAL3:
    .res 1
.segment "RAM"
HEAP_START:
.segment "CODE"
RESET:
    sei 
    cld 
    ldx #$FF
    txs 
    jsr INIT_HEAP
    jsr CLEAR_RAM
    jsr INIT_PPU
    jsr MAIN_LOOP
    jmp RESET
;;; Function: FACTORIAL
;;; Parameters: (N)
FACTORIAL:
    jsr SAVE_CONTEXT
    lda ZP_PARAM0
    sta ZP_LOCAL0
    lda ZP_LOCAL0
    pha 
    lda #0
    pla 
    sta TEMP1
    pla 
    cmp TEMP1
    bne LABEL3
    lda #1
    jmp LABEL4
LABEL3:
    lda #0
LABEL4:
    cmp #0
    beq LABEL1
    lda #1
    jmp LABEL2
LABEL1:
    lda ZP_LOCAL0
    pha 
    lda ZP_LOCAL0
    sta TEMP1
    dec TEMP1
    lda TEMP1
    pla 
    sta ZP_PARAM0
    jsr FACTORIAL
    pla 
    sta ZP_PARAM1
    pla 
    sta ZP_PARAM0
    jsr *
LABEL2:
    jsr RESTORE_CONTEXT
    rts 

MAIN_LOOP:
    jsr WAIT_VBLANK
    lda #%00000101
    sta ZP_PARAM0
    jsr FACTORIAL
    sta SCORE
    jsr UPDATE_SPRITES
    jmp MAIN_LOOP
CLEAR_RAM:
    lda #0
    ldx #0
CLEAR_LOOP:
    sta $0000,X
    sta $0100,X
    sta $0200,X
    sta $0300,X
    sta $0400,X
    sta $0500,X
    sta $0600,X
    sta $0700,X
    inx 
    bne CLEAR_LOOP
    rts 
INIT_PPU:
    lda #%10000000
    sta $2000
    lda #%00011110
    sta $2001
    rts 
WAIT_VBLANK:
    bit $2002
WAIT_VBLANK_LOOP:
    bit $2002
    bpl WAIT_VBLANK_LOOP
    rts 
UPDATE_SPRITES:
    rts 
NMI:
    rti 
IRQ:
    rti 
    lda #0
