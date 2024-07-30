.segment "CODE"

.proc getstr
    lda #<(arg1)
    sta $fff0
    rts
.endproc

; .proc putstr
;     lda #<(arg1)
;     sta $fff1
;     rts
; .endproc

.proc getchar
    lda $fff1
    cmp #$0a
    beq newline
    sta $fff0
    rts
newline:
    sta $ffef
    rts
.endproc

.proc newline
    sta $ffef
    rts
.endproc

.proc putchar
    sta $fff0
    rts
.endproc

.proc end
    jsr debug
    sta $fff2
.endproc
