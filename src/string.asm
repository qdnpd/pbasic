.segment "BSS"
cursor:      .res 1
str_temp1:   .res 2
str_temp2:   .res 2

.segment "CODE"


; arg1: number
; arg2: string buffer
.proc hexstr
number = arg1
buffer = arg2

    ldy #0
    lda #4
    sta (buffer),y
    D buffer

    lda number+1
    jsr hexstr_byte

    ldy #1
    lda return+1
    sta (buffer),y
    iny
    lda return
    sta (buffer),y

    lda number
    jsr hexstr_byte

    ldy #3
    lda return+1
    sta (buffer),y
    iny
    lda return
    sta (buffer),y

    rts
.endproc


; accumulator: byte number
; returns hex representation
.proc hexstr_byte
    tay

    and #$0f
    jsr numhex
    sta return

    tya
    lsr
    lsr
    lsr
    lsr
    jsr numhex
    sta return+1

    rts
.endproc


; accumulator: number (<= $0f)
; stores in accumulator hex representation of number
.proc numhex
    cmp #10
    bcc :+

    clc
    adc #$37
    rts

:   clc
    adc #$30
    rts
.endproc


; arg1: number
; arg2: pointer to buffer
.proc numstr
buffer      = ptr1
number      = temp1
pownum      = temp2
flag_first  = temp2+1

    VALUE_SAVE buffer
    VALUE_SAVE number
    VALUE_SAVE temp2
    lda length
    pha

    MEMORY_MOVE arg1, number
    MEMORY_MOVE arg2, buffer
    lda #0
    sta pownum+1
    lda #4
    sta pownum
    lda #1
    sta length
    lda #1
    sta flag_first

    lda number+1
    jsr int_isnegative
    cmp #1
    bne loop

    INVERT number
    ldy #1
    lda #$2D                ; char -
    sta (buffer),y
    inc length

loop:
    MEMORY_MOVE pownum, arg1
    jsr uint_tenpow

    MEMORY_MOVE number, arg1
    MEMORY_MOVE return, arg2
    jsr uint_div

    lda flag_first
    cmp #1
    bne :+
    lda #0
    cmp pownum
    beq :+

    cmp return+1
    bne :+
    cmp return
    bne :+
    jmp next

:   MEMORY_MOVE return, arg1
    lda #10
    sta arg2
    lda #0
    sta arg2+1
    jsr uint_mod

    lda return
    clc
    adc #$30
    ldy length
    sta (buffer),y
    inc length

    lda #0
    sta flag_first

next:
    lda pownum
    cmp #0
    beq finish

    dec pownum
    jmp loop

finish:
    ldy length
    dey
    tya
    ldy #0
    sta (buffer),y

    pla
    sta length
    VALUE_RETURN temp2
    VALUE_RETURN number
    VALUE_RETURN buffer
    rts
.endproc

; arg1:     pointer to string
; result:   number
.proc strnum
string = ptr1
result = temp4
negative = temp3

    VALUEB_SAVE negative
    VALUEB_SAVE length
    VALUE_SAVE temp4
    VALUE_SAVE ptr1

    MEMORY_MOVE arg1, string
    ldy #0
    lda (string),y
    sta length
    iny

    lda #0
    sta result
    sta result+1

    ; check if number is negative
    lda #0
    sta negative

    ldy #1
    lda (string),y
    cmp #$2D
    bne :+

    lda #1
    sta negative
    ldy #2
    jmp loop

:   ldy #1

loop:
    ; get length - index
    sty arg1
    dec arg1

    lda length
    clc
    sbc arg1

    ; calculate 10^a
    sta arg1
    jsr uint_tenpow

    ; get char and convert it to number
    lda (string),y
    clc
    sbc #$2f

    ; prepare arguments
    sta arg2
    lda #0
    sta arg2+1
    lda return
    sta arg1
    lda return+1
    sta arg1+1

    ; char *= 10^a
    jsr mul

    ; add to result
    lda result
    clc
    adc return
    bcc :+
    inc result+1
:   sta result
    lda result+1
    clc
    adc return+1
    sta result+1

    ; next char
    cpy length
    beq finish
    iny
    jmp loop

finish:
    lda #1
    cmp negative
    bne :+
    INVERT result

:   MEMORY_MOVE result, return

    VALUE_RETURN ptr1
    VALUE_RETURN temp4
    VALUEB_RETURN length
    VALUEB_RETURN negative

    rts
.endproc

; arg1: destination string
; arg2: source string
.proc catstr
dstlen = str_temp1
srclen = str_temp1+1
dstadr = str_temp2

    MEMORY_MOVE arg1, dstadr

    ldy #0
    lda (arg1),y
    sta dstlen
    lda (arg2),y
    sta srclen

    ; move source by 1 byte
    MEMORY_MOVE arg2, arg1
    UINT_INC arg1

    ; move dst to its end
    inc dstlen

    lda dstadr+1
    sta arg2+1
    clc
    lda dstadr
    adc dstlen
    sta arg2
    bcc :+
    inc arg2+1

:   ldx srclen
    ldy #0
    jsr kernel_memcpy

    MEMORY_MOVE dstadr, arg1
    clc
    dec dstlen
    lda dstlen
    adc srclen
    ldy #0
    sta (arg1),y

    rts
.endproc


; ; arg1: pointer to buffer
; .proc getstr
; buffer = ptr1
;
;     ; save argument
;     lda arg1
;     sta buffer
;     lda arg1+1
;     sta buffer+1
;
;     ; clear cursor
;     lda #0
;     sta cursor
;     ldy #1
;     ldx #0
;
; loop:
;     lda $fff1
;
;     ; if it is newline, finish
;     cmp #$0A
;     beq finish
;
;     ; if it is backspace, shift buffer left
;     cmp #$08
;     beq shiftleft
;
;     ; if it is arrows, move cursor
;     cmp #$00                        ; CHANGE!
;     beq movecursor
;
;     ; else store char into buffer
;     sta (buffer),y
;     iny
;     inx
;     jmp loop
;
; shiftleft:
;     ; if cursor > string length, just delete one char
;     tya
;     sta temp1
;     txa
;     cmp temp1
;     bpl :+
;
;     dex
;     dey
;     lda #0
;     sta (buffer),y
;     lda $fff2
;     jmp loop
;
;     sty temp1+1
;
;     ; move first argument to the same position, as cursors
;     clc
;     lda #<(buffer)
;     adc temp1+1
;     sta arg1
;     sta arg2
;     inc arg2
;
;     lda #>(buffer)
;     sta arg1
;     sta arg2
;
;     ; calculate size of right side of buffer relative to cursor position
; :   txa
;     sec
;     sbc temp1
;
;     ; perform shifting
;     jsr memcpy
;
;     dex
;     dey
;     lda $fff2
;     jmp loop
;
; movecursor:
;     lda $fff1
;
;     ; left
;     cmp #$44
;     bne :+
;     cpy #0
;     beq loop
;     dey
;     jmp :++
;
;     ; right
; :   cmp #$43
;     bne loop
;     tax
;     sta temp1
;     inc temp1
;     cpy temp1
;     beq loop
;     iny
;
;     ; refresh cursor display
; :   sty $fff0
;
;     jmp loop
;
; finish:
;     tax
;     ldy #0
;     sta (buffer),y
; .endproc

; ; arg1: pointer to string
; .proc putstr
;     ldx #0
;     lda (arg1,x)
;     tax
;     ldy #1
; loop:
;     lda (arg1),y
;     jsr putchar
;     stx temp4
;     cpy temp4
;     beq end
;     iny
;     jmp loop
; end:
;     rts
; .endproc

; arg1: pointer to string n.1
; arg2: pointer to string n.2
.proc cmpstr
char = temp4
length = temp4+1
    tya
    pha
    txa
    pha
    ldx #0
    lda (arg1,x)
    sta length
    lda (arg2,x)
    cmp length
    bne notequal
    ldy #1
loop:
    lda (arg1),y
    sta char
    lda (arg2),y
    cmp char
    bne notequal

    cpy length
    beq equal

    iny
    jmp loop

notequal:
    lda #0
    jmp end
equal:
    lda #1

end:
    sta char    ; save result
    pla
    tax
    pla
    tay
    lda char
    rts
.endproc
