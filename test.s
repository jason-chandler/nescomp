;;; Generated NES 6502 code for ca65 assembler

.segment "CODE"

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
ADD-6502:
    clc 
    adc TEMP1
    rts 
SUB-6502:
    sec 
    sbc TEMP1
    rts 
MUL-6502:
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
    lda LO, HEAP_START
    sta HEAP_PTR
    lda HI, HEAP_START
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
    org ZEROPAGE
TEMP1:
    ds 1
TEMP2:
    ds 1
PTR:
    ds %00000010
RETURN_ADDR:
    ds %00000010
MULTIPLICAND:
    ds 1
MULTIPLIER:
    ds 1
PRODUCT:
    ds 1
HEAP_PTR:
    ds %00000010
HEAP_USED:
    ds 1
ALLOC_SIZE:
    ds 1
ALLOC_RESULT:
    ds %00000010
CONS_CAR:
    ds 1
CONS_CDR:
    ds %00000010
CONS_PTR:
    ds %00000010
ZP-PARAM0:
    ds 1
ZP-PARAM1:
    ds 1
ZP-PARAM2:
    ds 1
ZP-PARAM3:
    ds 1
ZP-LOCAL0:
    ds 1
ZP-LOCAL1:
    ds 1
ZP-LOCAL2:
    ds 1
ZP-LOCAL3:
    ds 1
    org RAM
HEAP_START:
    jsr FACTORIAL
