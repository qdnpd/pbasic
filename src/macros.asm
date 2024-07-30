.macro D value
    lda value+1
    ldx value
    jsr debug
.endmacro

.macro VALUE_SAVE value
    lda value
    pha
    lda value+1
    pha
.endmacro

.macro VALUE_RETURN value
    pla
    sta value+1
    pla
    sta value
.endmacro

.macro VALUEB_SAVE value
    lda value
    pha
.endmacro

.macro VALUEB_RETURN value
    pla
    sta value
.endmacro

.macro MEMORY_MOVE src, dst
    lda src
    sta dst
    lda src+1
    sta dst+1
.endmacro

.macro ADDRESS_MOVE adr, dst
    lda #<(adr)
    sta dst
    lda #>(adr)
    sta dst+1
.endmacro

.macro UINT_ADD num1, num2, res
    clc
    lda num1
    adc num2
    sta res

    lda num1+1
    adc num2+1
    sta res+1
.endmacro

.macro UINT_CONST_ADD const, num, res
    clc
    lda num
    adc #<(const)
    sta res

    lda num+1
    adc #>(const)
    sta res+1
.endmacro

.macro UINT_SUB num1, num2, res
    sec
    lda num1
    sbc num2
    sta res
    lda num1+1
    sbc num2+1
    sta res+1
.endmacro

.macro UINT_CONST_SUB const, num, res
    sec
    lda num
    sbc #<(const)
    sta res
    lda num+1
    sbc #>(const)
    sta res+1
.endmacro

.macro UINT_CMP num1, num2
.scope
    lda num1+1
    cmp num2+1
    beq cmplo_
    bmi lesser_
    bcs greater_

cmplo_:
    lda num1
    cmp num2
    beq equal_
    bmi lesser_
    jmp greater_

lesser_:
    lda #0
    jmp finish_
greater_:
    lda #2
    jmp finish_
equal_:
    lda #1

finish_:
.endscope
.endmacro

.macro UINT_CONST_CMP const, num
.scope
    lda #>(const)
    cmp num+1
    beq cmplo_
    bmi lesser_
    bcs greater_

cmplo_:
    lda #<(const)
    cmp num
    beq equal_
    bmi lesser_
    jmp greater_

lesser_:
    lda #0
    jmp finish_
greater_:
    lda #2
    jmp finish_
equal_:
    lda #1

finish_:
.endscope
.endmacro

.macro UINT_INC num
    inc num
    bne :+
    inc num+1
:
.endmacro

.macro INVERT num
    lda #$ff
    eor num
    sta num

    lda #$ff
    eor num+1
    sta num+1
.endmacro
