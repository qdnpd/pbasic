.segment "ZEROPAGE"
currentCodeline:        .res 2

.segment "BSS"
currentCodelineIndex:   .res 1

.segment "CODE"

.macro CODELINE_FIRST
    lda #<(code_table)
    sta currentCodeline
    lda #>(code_table)
    sta currentCodeline+1

    lda #0
    sta currentCodelineIndex

    jsr codeline_set_tokbase
.endmacro

.macro CODELINE_SAVE
    lda currentCodeline
    pha
    lda currentCodeline+1
    pha
    lda currentCodelineIndex
    pha
    lda tok_base
    pha
    lda tok_base+1
    pha
.endmacro

.macro CODELINE_LOAD
    pla
    sta tok_base+1
    pla
    sta tok_base
    pla
    sta currentCodelineIndex
    pla
    sta currentCodeline+1
    pla
    sta currentCodeline
.endmacro

.macro CODELINE_CLEAR_SAVED
    pla
    pla
    pla
    pla
    pla
.endmacro


.proc codeline_set_tokbase
    MEMORY_MOVE currentCodeline, ptr1

    ldy #2
    lda (ptr1),y
    sta tok_base

    iny
    lda (ptr1),y
    sta tok_base+1

    rts
.endproc

; accumulator stores 0 if current codeline is the last one
.proc codeline_next
    ; check if current codeline is the last one
    lda currentCodelineIndex
    cmp code_table_size
    bcc :+

    lda #0
    rts

    ; add 4 to pointer
:   clc
    lda currentCodeline
    adc #4
    sta currentCodeline
    bcc :+
    inc currentCodeline+1

    ; refresh tok_base
:   jsr codeline_set_tokbase
    inc currentCodelineIndex

    lda #1
    rts
.endproc


; arg1: codeline number
.proc codeline_set
    ; search for the codeline with the same num
codeline = ptr1

    ; make pointer point to the first codeline
    lda #<(code_table)
    sta codeline
    lda #>(code_table)
    sta codeline+1
    lda #0
    sta currentCodelineIndex

loop:
    lda currentCodelineIndex
    cmp code_table_size
    bne :+

    jmp error_codeline_wasnot_found

    ; compare nums
:   ldy #0
    lda (codeline),y
    sta arg2
    iny
    lda (codeline),y
    sta arg2+1
    jsr uint_cmp

    cmp #1
    beq finish

    cmp #0
    bne :+
    jmp error_codeline_wasnot_found

    ; next codeline
:   clc
    lda codeline
    adc #4
    sta codeline
    bcc :+
    inc codeline+1
:   jmp loop

finish:
    lda codeline
    sta currentCodeline
    lda codeline+1
    sta currentCodeline+1

    jsr codeline_set_tokbase

    rts
.endproc


.proc variables_clean
counter     = temp1
var_counter = temp1+1

    lda #0
    sta counter
    sta var_counter

loop:
    lda var_counter
    cmp var_table_size
    beq finish

    ; free variables name
    ldx counter
    lda var_table,x
    sta arg1
    inx
    lda var_table,x
    sta arg1+1
    inx
    stx counter
    jsr free

    ; free variables value pointer
    ldx counter
    lda var_table,x
    sta arg1
    inx
    lda var_table,x
    sta arg1+1
    stx counter
    jsr free

    inc var_counter
    jmp loop

finish:
    lda #0
    sta var_table_size
    rts
.endproc


.proc codelines_clean
    ; skip it if there is no codelines
    lda #0
    cmp code_table_size
    beq loop_codeline_finish

    CODELINE_FIRST

loop_codeline:

    ; clean codelines number
    ldy #0
    lda #0
    sta (currentCodeline),y
    iny
    sta (currentCodeline),y
    iny

    ; free token list
    lda (currentCodeline),y
    sta arg1
    lda #0
    sta (currentCodeline),y
    iny

    lda (currentCodeline),y
    sta arg1+1
    lda #0
    sta (currentCodeline),y

    jsr free

    jsr codeline_next

    ; if it was not the last one codeline, continue
    cmp #0
    bne loop_codeline

loop_codeline_finish:
    lda #0
    sta code_table_size
    rts
.endproc
