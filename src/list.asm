.segment "BSS"
display_temp:   .res 1
display_first:  .res 2
display_last:   .res 2

.segment "CODE"

; displays codelines from arg1 to arg2
.proc display_codelines
first  = display_first
last   = display_last
number = display_number

    MEMORY_MOVE arg1, first
    MEMORY_MOVE arg2, last

    lda first
    cmp #0
    bne :+
    lda first+1
    cmp #0
    bne :+

    CODELINE_FIRST
    jmp :++

:   jsr codeline_set

:   lda last
    cmp #0
    bne loop
    lda last+1
    cmp #0
    bne loop

    ; get number of the last codeline
    lda code_table_size
    sta ptr1
    dec ptr1
    lda #0
    sta ptr1+1

    asl ptr1
    rol ptr1+1
    asl ptr1
    rol ptr1+1

    clc
    lda #<(code_table)
    adc ptr1
    sta ptr1
    lda #>(code_table)
    adc ptr1+1
    sta ptr1+1

    ldy #0
    lda (ptr1),y
    sta last
    iny
    lda (ptr1),y
    sta last+1

loop:
    jsr display_codeline

    ; if numbers of the current codeline and the last we need to display
    ; are equal, then return
    ldy #0
    lda (currentCodeline),y
    cmp last
    bne :+

    iny
    lda (currentCodeline),y
    cmp last+1
    bne :+
    jmp finish

:   jsr codeline_next
    jmp loop

finish:
    rts
.endproc

.proc display_codeline
string    = ptr2
codeline  = ptr3
stringLen = display_temp

    MEMORY_MOVE currentCodeline, codeline
    ADDRESS_MOVE str1, string
    inc string
    bne :+
    inc string+1

    ; insert codelines number
:   ldy #0
    lda (codeline),y
    sta arg1
    iny
    lda (codeline),y
    sta arg1+1
    ADDRESS_MOVE str2, arg2
    jsr numstr

    ; copy it into string
    ldx #0
    lda str2,x
    sta length
    sta stringLen

    ADDRESS_MOVE str2, arg1
    inc arg1
    bne :+
    inc arg1+1

:   MEMORY_MOVE string, arg2
    lda length
    jsr memcpy

    ; move string pointer
    clc
    lda string
    adc length
    sta string
    bne :+
    inc string+1

    ; add space
:   inc stringLen
    lda #$20
    ldy #0
    sta (string),y
    inc string
    bne loop
    inc string+1

loop:
    TOK_GET
    cmp #4
    bne :+++

    ldy #0
    lda #$20
    sta (string),y
    PURE_TOK_NEXT

    inc stringLen
    inc string
    bne :+
    inc string+1
:   jmp loop

:   jsr display_token

    clc
    lda string
    adc length
    sta string
    bne :+
    inc string+1

:   lda length
    clc
    lda stringLen
    adc length
    sta stringLen

    TOK_GET
    cmp TS_EOT
    bne loop

    ldx #0
    lda stringLen
    sta str1,x

    ADDRESS_MOVE str1, arg1
    jsr putstr

    rts
.endproc

.proc display_token
    TOK_GET

    cmp #$1
    bne :+
    jmp display_var

:   cmp #$2
    bne :+
    jmp display_number

:   cmp #$3
    bne :+
    jmp display_string

:   cmp #4
    bne :+
    jmp display_space

:   cmp #$30
    bcs :+
    jmp display_keyword

:   jmp display_symbol_token
.endproc

.proc display_space
    ldy #0
    lda #$20
    sta (ptr2),y

    iny
    sty length
    rts
.endproc

.proc display_var
    ADDRESS_MOVE str2, arg1
    jsr parse_var_name

    ADDRESS_MOVE str2, arg1
    inc arg1
    bne :+
    inc arg1+1

:   MEMORY_MOVE ptr2, arg2
    ldx #0
    lda str2,x
    sta length
    jsr memcpy

    ; parse_var_name uses TOK_NEXT instead of PURE_* one
    ; so, it can skip a space we need to display
    ; because of this, we need to do check by
    ; TOREWRITE
    dec tok_base
    bne :+
    dec tok_base+1

:   TOK_GET
    cmp #4
    beq :+

    inc tok_base
    bne :+
    inc tok_base+1

:   rts
.endproc

.proc display_number
    PURE_TOK_NEXT

    TOK_GET
    sta arg1
    PURE_TOK_NEXT
    TOK_GET
    sta arg1+1
    ADDRESS_MOVE str2, arg2
    jsr numstr

    ; get length
    ldx #0
    lda str2,x
    sta length

    ADDRESS_MOVE str2, arg1
    ; increment string pointer (skip length)
    inc arg1
    bne :+
    inc arg1+1

    ; copy string without
:   MEMORY_MOVE ptr2, arg2
    lda length
    jsr memcpy

    PURE_TOK_NEXT
    rts
.endproc

.proc display_string
    PURE_TOK_NEXT
    TOK_GET
    sta length
    PURE_TOK_NEXT

    ; add " char
    ldy #0
    lda #$22
    sta (ptr2),y

    ; increment string pointer
    inc ptr2
    bne :+
    inc ptr2+1

    ; copy string insides into ptr2
:   MEMORY_MOVE tok_base, arg1
    MEMORY_MOVE ptr2,     arg2
    lda length
    jsr memcpy

    ; move tok_base
    clc
    lda tok_base
    adc length
    sta tok_base
    bne :+
    inc tok_base+1

    ; decrement string pointer
:   dec ptr2
    bne :+
    dec ptr2+1

    ; add " char
:   ldy length
    iny
    sty length
    lda #$22
    sta (ptr2),y

    inc length
    rts
.endproc

.proc display_keyword
    TOK_GET
    sec
    sbc #$10

    ; get offset
    sta arg1
    lda #6
    sta arg2
    lda #0
    sta arg1+1
    sta arg2+1
    sta return
    sta return+1
    jsr byte_mul

    ; add offset
    ADDRESS_MOVE keywords, arg1

    clc
    lda arg1
    adc return
    sta arg1
    lda arg1+1
    adc return+1
    sta arg1+1

    ; get length
    ldy #0
    lda (arg1),y
    sta length

    ; increment pointer (skip length)
    inc arg1
    bne :+
    inc arg1+1

    ; copy
:   MEMORY_MOVE ptr2, arg2
    lda length
    jsr memcpy

    PURE_TOK_NEXT
    rts
.endproc

.proc display_symbol_token
    TOK_GET
    sec
    sbc #$30
    tax
    lda syms,x

    ldy #0
    sta (ptr2),y

    lda #1
    sta length

    PURE_TOK_NEXT
    rts
.endproc
