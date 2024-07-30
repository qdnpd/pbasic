.segment "CODE"

.proc debug
    sta $70
    stx $71
    sty $72
    sta $fff3
    rts
.endproc
