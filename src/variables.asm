.segment "BSS"
varSize:        .res 1

.segment "CODE"

; arg1: variables name
; accumulator: size
; returns address
.proc var_define
varPointer = return
name       = temp1
index      = temp2
offset     = temp2+1
string     = temp3
size       = varSize

    sta size
    VALUE_SAVE temp1
    VALUE_SAVE temp2
    VALUE_SAVE temp3
    MEMORY_MOVE arg1, name

    ; check if there no room for new variable in var_table
    lda var_table_size
    cmp #63
    bne :+
    jmp error_var_table_noroom

    ; copy variables name into allocated memory
    ; allocate memory
:   ldy #0
    lda (name),y
    tay
    iny
    tya
    pha                 ; save size
    jsr alloc
    MEMORY_MOVE return, string

    ; copy string
    MEMORY_MOVE name,   arg1
    MEMORY_MOVE string, arg2
    pla                 ; load size
    jsr memcpy

    ; get pointer to new entry
    lda var_table_size
    asl
    asl         ; offset = index * 4
    tax

    ; store pointer to variable name
    lda string
    sta var_table,x
    inx
    lda string+1
    sta var_table,x
    inx

    ; allocate memory for variable value
    txa
    pha
    lda size
    jsr alloc

    pla
    tax
    lda return
    sta var_table,x
    inx
    lda return+1
    sta var_table,x

    ; return variables entry
    ADDRESS_MOVE var_table, return
    dex
    dex
    dex
    stx offset

    clc
    lda return
    adc offset
    sta return
    bcc :+
    inc return+1

:   VALUE_RETURN temp3
    VALUE_RETURN temp2
    VALUE_RETURN temp1

    inc var_table_size
    rts
.endproc

; arg1: variables name
; accumulator will store variables index in var_table
; and return will store pointer to entry
.proc var_entry
counter = temp2
entry   = return
    ADDRESS_MOVE var_table, entry
    lda #0
    sta counter

loop:
    ldy #0
    lda (entry),y
    sta arg2
    iny
    lda (entry),y
    sta arg2+1
    jsr cmpstr

    cmp #1
    beq finish

    lda counter
    cmp var_table_size
    beq error

    inc counter

    clc
    lda entry
    adc #4
    sta entry
    bcc :+
    inc entry+1

:   jmp loop

error:
    lda #0
    rts
finish:
    lda #1
    rts
.endproc


; arg1: variables name
; returns variables value address
.proc var_address
    jsr var_entry
    cmp #0
    bne :+
    jmp error_var_wasnot_defined

:   ldy #2
    lda (return),y
    sta arg2
    iny
    lda (return),y
    sta arg2+1

    MEMORY_MOVE arg2, return
    rts
.endproc


; arg1: pointer to variable name
.proc var_index
name     = arg1
counter  = length

    VALUEB_SAVE counter
    ldx #0
    ldy #0
    lda #0
    sta counter


loop:
    lda var_table_size
    asl
    asl
    cmp counter
    beq novar

    ; compare names
    ldx counter
    lda var_table,x
    sta arg2
    inx
    lda var_table,x
    sta arg2+1
    jsr cmpstr

    cmp #1
    beq equal

    ; next variable name
    clc
    lda counter
    adc #4
    sta counter
    jmp loop

novar:
    lda #$ff
    sta counter
    jmp finish
equal:
    lsr counter
    lsr counter
finish:
    lda counter
    tax
    VALUEB_RETURN counter
    txa
    rts
.endproc
