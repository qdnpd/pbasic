.segment "CODE"

; result = arg1 ^ arg2
; TOOPTIMIZE
.proc pow
base     = temp1
exponent = temp1+1
result   = temp2
toadd    = temp3
m        = temp4
counter  = counter1

    ; save registers and old values
    tya
    pha
    txa
    pha
    lda temp4
    pha
    lda temp4+1
    pha
    lda temp1
    pha
    lda temp1+1
    pha

    lda arg1
    sta base
    lda arg2
    sta exponent
    lda #0
    sta result+1
    lda #1
    sta result
    ldy #8

loop:
    tya
    tax
    lda exponent
    sta m
@shl:           ; m >> y
    cpx #0
    beq @skipshl
    lsr m
    dex
    jmp @shl

@skipshl:
    lda #1
    and m
    cmp #0      ; computing num^m
    beq @next   ; if it is one the result will not be changed
    tya
    pha
    lda base
    cpy #0
    beq @skipsqr
    sta arg1    ; accumulator stores num^m now
    jsr sqr     ; a^2
    jmp @skipnsqr

@skipsqr:
    sta arg1
    lda #0
    sta arg1+1
    jmp @common

@skipnsqr:
    lda return
    sta arg1
    lda return+1
    sta arg1+1

@common:
    lda result
    sta arg2
    lda result+1
    sta arg2+1
    jsr mul
    lda return
    sta result
    lda return+1
    sta result+1    ; result *= a

    pla
    tay
@next:
    cpy #0
    beq @end
    dey
    jmp loop

@end:
    lda result
    sta return
    lda result+1
    sta return+1

    pla
    sta temp1+1
    pla
    sta temp1
    pla
    sta temp4+1
    pla
    sta temp4
    pla
    tax
    pla
    tay

    rts
.endproc


; return = arg1 ^ 2
; TO REMAKE
.proc sqr
    lda arg1
    sta arg2
    lda arg1+1
    sta arg2+1
    jsr mul
    rts
.endproc


; return = a*32
.proc mul32
    ldy #0
    sta return
    lda #0
    sta return+1
    lda #1

loop:
    asl return+1
    clc
    asl return
    bcc :+
    ora return+1
    sta return+1
    lda #1
:

    iny
    cpy #5
    bne loop
    rts
.endproc


; return = arg1 * arg2
.proc mul

    tya
    pha
    txa
    pha

    lda #0
    sta return
    sta return+1
    ldy arg2

loop:
    cpy #0
    beq end
    lda return
    clc
    adc arg1
    bcc @skipadd
    inc return+1
@skipadd:
    sta return
    lda return+1
    clc
    adc arg1+1
    sta return+1
    dey
    jmp loop

end:
    pla
    tax
    pla
    tay

    rts
.endproc


; arithm = a*x
.proc mul_big

.endproc


; accumulator: char
; accumulator: 1 if true, 0 if false
.proc isnumber
char = temp4

    clc
    sbc #$30
    clc
    adc #1
    cmp #9
    beq true
    bcs false
    jmp true

false:
    lda #0
    jmp finish
true:
    lda #1
finish:
    rts
.endproc


; arg1: source pointer
; arg2: destination pointer
; a: size of data to move
.proc memcpy
source = arg1
dest   = arg2

    ; save length
    tay
    lda length
    pha
    tya

    ldy #0
    sta length

loop:
    cpy length
    beq finish

    lda (arg1),y
    sta (arg2),y
    iny
    jmp loop

finish:
    ; return length
    pla
    sta length

    rts
.endproc
