.segment "ZEROPAGE"
int_result:        .res 2
int_temp1:         .res 2
int_temp2:         .res 2
int_temp3:         .res 2
int_temp4:         .res 2
int_temp5:         .res 1

.segment "BSS"
int_num1:          .res 2
int_num2:          .res 2
int_sign:          .res 1

.segment "DATA"
pyth_table: .byte  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
            .byte  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15
            .byte  0,  2,  4,  6,  8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30
            .byte  0,  3,  6,  9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45
            .byte  0,  4,  8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60
            .byte  0,  5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75
            .byte  0,  6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90
            .byte  0,  7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 98,105
            .byte  0,  8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96,104,112,120
            .byte  0,  9, 18, 27, 36, 45, 54, 63, 72, 81, 90, 99,108,117,126,135
            .byte  0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100,110,120,130,140,150
            .byte  0, 11, 22, 33, 44, 55, 66, 77, 88, 99,110,121,132,143,154,165
            .byte  0, 12, 24, 36, 48, 60, 72, 84, 96,108,120,132,144,156,168,170
            .byte  0, 13, 26, 39, 52, 65, 78, 91,104,117,130,143,156,169,182,195
            .byte  0, 14, 28, 42, 56, 60, 84, 98,112,126,140,154,168,182,196,210
            .byte  0, 15, 30, 45, 60, 75, 90,105,120,135,150,165,180,195,210,225

tenpow_list:    .word 1, 10, 100, 1000, 10000


.segment "CODE"

; arg1: num. 1
; arg2: num. 2
; a = 0 if arg1 < arg2, 1 if arg1 = arg2, 2 if arg1 > arg2
.proc uint_cmp
    lda arg1+1
    cmp arg2+1
    beq cmplo
    bmi lesser
    bcs greater

cmplo:
    lda arg1
    cmp arg2
    beq equal
    bmi lesser
    jmp greater

lesser:
    lda #0
    rts
greater:
    lda #2
    rts
equal:
    lda #1
    rts
.endproc


; return = a << arg1
.proc shift
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
    cpy arg1
    bne loop
    rts
.endproc


; return = arg1 + arg2
.proc int_add
    clc
    lda arg1
    adc arg2
    sta return

    lda arg1+1
    adc arg2+1
    sta return+1

    lda return
    adc #0
    sta return
    bcc :+
    inc return+1
:   rts
.endproc

.proc uint_add
    clc
    lda arg1
    adc arg2
    sta return

    lda arg1+1
    adc arg2+1
    sta return+1
    rts
.endproc


; return = arg1 - arg2
.proc int_sub
    INVERT arg2
    jsr int_add
    rts
.endproc


; return = arg1 and arg2
.proc int_and
    lda arg1
    and arg2
    sta return

    lda arg1+1
    and arg2+1
    sta return+1

    rts
.endproc


; return = arg1 or arg2
.proc int_or
    lda arg1
    ora arg2
    sta return

    lda arg1+1
    ora arg2+1
    sta return+1

    rts
.endproc


; return = arg1 * arg2
; arg1 and arg2 must be lesser than $10 and be positive
.proc table_mul
    ; clean return
    lda #0
    sta return+1

    ; A*B = value at (table base + $AB) address
    lda arg1
    asl
    asl
    asl
    asl
    ora arg2

    tax
    lda pyth_table,x
    sta return

    rts
.endproc


; return = (arg1 * arg2) + return
.proc byte_mul
num1     = int_temp1
num2     = int_temp1+1
A1       = int_temp2
A2       = int_temp2+1
B1       = int_temp3
B2       = int_temp3+1
toadd    = int_temp4
result   = int_result
    ; let num1 and num2 be A2A1 and B2B1

    ; move arguments
    lda arg1
    sta num1
    lda arg2
    sta num2
    MEMORY_MOVE return, toadd

    ; clean result
    lda #0
    sta result+1

    ; get A1
    lda #$0f
    and num1
    sta A1

    ; get A2
    lda #$f0
    and num1
    lsr
    lsr
    lsr
    lsr
    sta A2

    ; get B1
    lda #$0f
    and num2
    sta B1

    ; get B2
    lda #$f0
    and num2
    lsr
    lsr
    lsr
    lsr
    sta B2

    ; compute A1*B1
    lda A1
    sta arg1
    lda B1
    sta arg2
    jsr table_mul
    lda return
    sta result

    ; compute A1*B2
    lda B2
    sta arg2
    jsr table_mul

    ; shift by 4
    lda #4
    sta arg1
    lda return
    jsr shift

    ; add
    lda return
    sta arg2
    lda return+1
    sta arg2+1
    lda result
    sta arg1
    jsr int_add

    ; save return
    lda return
    sta result
    lda return+1
    sta result+1

    ; compute A2*B1
    lda A2
    sta arg1
    lda B1
    sta arg2
    jsr table_mul

    ; shift by 4
    lda #4
    sta arg1
    lda return
    jsr shift

    ; add
    lda return
    sta arg2
    lda return+1
    sta arg2+1
    lda result
    sta arg1
    lda result+1
    sta arg1+1
    jsr int_add

    ; save  return
    lda return
    sta result
    lda return+1
    sta result+1

    ; compute A2*B2
    lda A2
    sta arg1
    lda B2
    sta arg2
    jsr table_mul

    ; shift by 8
    lda #8
    sta arg1
    lda return
    jsr shift

    ; add
    lda return
    sta arg2
    lda return+1
    sta arg2+1
    lda result
    sta arg1
    lda result+1
    sta arg1+1
    jsr int_add

    ; result + return
    lda return
    sta arg1
    lda return+1
    sta arg1+1
    lda toadd
    sta arg2
    lda toadd+1
    sta arg2+1
    jsr int_add

    rts
.endproc

.macro SIGN_FLAG_SET
    lda #0
    sta sign

    ; if result is negative, sign is $ff, else it is $0
    lda arg1+1
    jsr int_isnegative
    cmp #1
    bne :+

    lda #$ff
    eor sign
    sta sign
    lda #$ff
    eor arg1
    sta arg1
    lda #$ff
    eor arg1+1
    sta arg1+1

:   lda arg2+1
    jsr int_isnegative
    cmp #1
    bne :+

    lda #$ff
    eor sign
    sta sign
    lda #$ff
    eor arg2
    sta arg2
    lda #$ff
    eor arg2+1
    sta arg2+1
:
.endmacro

.macro SIGN_RETURN_SET
    lda sign
    cmp #$ff
    bne :+

    lda #$ff
    eor return
    sta return
    lda #$ff
    eor return+1
    sta return+1
:
.endmacro

; return = arg1*arg2
.proc int_mul
num1Hi  = int_num1
num2Hi  = int_num1+1
sign    = int_sign

    SIGN_FLAG_SET

    ; save hi bytes
    lda arg1+1
    sta num1Hi
    lda arg2+1
    sta num2Hi

    ; multiply lo bytes
    jsr byte_mul

    ; save return
    lda return
    pha
    lda return+1
    pha

    ; clean return
    lda #0
    sta return
    sta return+1

    ; multiply hi bytes
    lda num1Hi
    sta arg1
    lda num2Hi
    sta arg2
    lda #0
    sta arg1+1
    sta arg2+1
    jsr byte_mul

    ; shift return by 8
    lda return
    sta arg2+1

    ; add results
    pla
    sta arg1+1
    pla
    sta arg1
    lda #0
    sta arg2
    jsr int_add

    ; if result should be negative, invert result
    SIGN_RETURN_SET

    rts
.endproc


; return = remainder from arg1/arg2
.proc uint_mod
    jsr uint_div
    MEMORY_MOVE int_temp1, return
    rts
.endproc


; return = arg1 / arg2
.proc uint_div
numerator   = arg1
denominator = arg2
remainder   = int_temp1
temp        = int_temp2
toadd       = int_temp3

    ldy #15
    lda #0
    sta remainder
    sta remainder+1
    sta return
    sta return+1

loop:
    ; shift remainder by 1 left
    asl remainder
    rol remainder+1

    ; shift numerator by i and get first bit
    lda numerator
    sta temp
    lda numerator+1
    sta temp+1
    tya
    tax

    lda #1
    sta toadd
    lda #0
    sta toadd+1

shifting_loop:
    cpx #0
    beq :+

    lsr temp+1
    ror temp

    asl toadd
    rol toadd+1

    dex
    jmp shifting_loop

:   lda temp
    and #1

    ; merge it with remainder
    ora remainder
    sta remainder

    ; if remainder is greater than denominator
    lda remainder+1
    cmp denominator+1
    bcc next
    lda remainder
    cmp denominator
    bcc next

    ; r = r - d
    UINT_SUB remainder, denominator, remainder

    ; return |= 1 >> i
    lda return
    ora toadd
    sta return
    lda return+1
    ora toadd+1
    sta return+1

next:
    cpy #0
    bne :+
    rts

:   dey
    jmp loop
.endproc

.proc int_div
sign = int_sign

    SIGN_FLAG_SET
    jsr uint_div
    SIGN_RETURN_SET
    rts
.endproc

.proc int_mod
.endproc

; return = 10^arg1
.proc uint_tenpow
    ; if arg1 > 5 quit
    lda arg1
    cmp #5
    bcc :+
    lda #0
    sta return
    sta return+1
    rts

:   asl
    tax
    lda tenpow_list,x
    sta return
    inx
    lda tenpow_list,x
    sta return+1

    rts
.endproc

.proc int_cmp
.endproc

; checks if number, which hi byte stored in accumulator, is negative
.proc int_isnegative
    sta int_temp5
    lda #%10000000
    bit int_temp5
    bne :+
    lda #0
    rts

:   lda #1
    rts
.endproc
