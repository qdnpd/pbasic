.segment "CODE"

.proc fun_inc
    ldx arg1
    ldy arg1+1

    inx
    bne :+
    iny

:   stx return
    sty return+1
    rts
.endproc

.proc fun_abs
    MEMORY_MOVE arg1, return

    jsr int_isnegative
    cmp #1
    bne :+
    INVERT return

:   rts
.endproc

.proc fun_hibyte
    lda arg1+1
    sta return
    lda #0
    sta return+1
    rts
.endproc

.proc fun_lobyte
    lda arg1
    sta return
    lda #0
    sta return+1
    rts
.endproc

.proc fun_input
input = str1

    ADDRESS_MOVE input, arg1
    jsr getstr
    jsr strnum
    rts
.endproc
