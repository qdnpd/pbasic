.include "macros.asm"
.include "memory.asm"
.include "external_basic.asm"
.include "external_sim.asm"
.include "misc.asm"

.include "math_int.asm"
.include "math_fun.asm"
.include "string.asm"

.include "error.asm"
.include "variables.asm"
.include "token.asm"
.include "eval.asm"
.include "parse.asm"

.include "list.asm"
.include "interp.asm"

;.segment "VECTORS"
;.addr kernel_main, kernel_main, kernel_main

.segment "DATA"
tststr1:    .byte 3, "str     "
tststr2:    .byte 3, "ing"

.segment "CODE"

.proc main
    ; reserve memory
    lda #255
    jsr alloc           ; var_table
    lda #255
    jsr alloc           ; codelines
    lda #255
    jsr alloc
    lda #255
    jsr alloc
    lda #255
    jsr alloc
    lda #255
    jsr alloc
    lda #255
    jsr alloc
    lda #255            ; others
    jsr alloc

    jmp interactive
.endproc


.proc interactive
input = str1

loop:
    lda $fff3           ; display >
    ADDRESS_MOVE input, arg1
    jsr getstr

    ; if the first symbol is not number, interpet it
    ldx #1
    lda input,x
    jsr isnumber

    cmp #1
    beq :+

    jsr interpret
    jmp loop

    ; else make new codeline
:   jsr new_codeline
    jmp loop
.endproc
