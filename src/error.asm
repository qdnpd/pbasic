.segment "DATA"
error_str:          .byte 17, "too long var name   "
                    .byte 16, "var limit passed    "
                    .byte 17, "array not defined   "
                    .byte 17, "invalid statement   "
                    .byte 19, "wrong function name "
                    .byte 15, "var not defined     "
                    .byte 17, "wrong line number   "
                    .byte 20, "couldn't free memory"
                    .byte 20, "cond stmt not allowd"
                    .byte 20, "body                " ; TOREWRITE

error_num_str1:     .byte  7, "error #"
error_num_str2:     .byte  4, " at "
accept_error_str:   .byte  9, " expected"
free_error_str:     .byte 15, "couldn't free $"

token_names:        .byte $1,   8, "variable   "
                    .byte $2,   6, "number     "
                    .byte $3,   6, "string     "
                    .byte $30, 11, "op. bracket"
                    .byte $31, 11, "cl. bracket"
                    .byte $36, 10, "equal sign "
                    .byte $3c,  5, "comma      "
                    .byte $3f, 11, "cl. sq-brck"
                    .byte $14,  9, "word then  "
                    .byte $1b,  7, "word to    "
                    .byte $21,  7, "word do    "
                    .byte $20,  8, "word end   "
                    .byte $ff,  9, "operation  "

.segment "ZEROPAGE"
error_code:     .res 1
token_expected: .res 1

.segment "CODE"

.macro DEFINE_RUNTIME_ERROR label, num
label:
    lda #num
    jsr send_error_num
    lda #num
    jsr send_error
    jmp interactive
.endmacro

DEFINE_RUNTIME_ERROR error_var_table_noroom,        1
DEFINE_RUNTIME_ERROR error_array_wasnot_defined,    2
DEFINE_RUNTIME_ERROR error_invalid_statement,       3
DEFINE_RUNTIME_ERROR error_no_such_function,        4
DEFINE_RUNTIME_ERROR error_var_wasnot_defined,      5
DEFINE_RUNTIME_ERROR error_codeline_wasnot_found,   6
DEFINE_RUNTIME_ERROR error_dim_too_big_size,        7
DEFINE_RUNTIME_ERROR error_dim_defined_var,         8
DEFINE_RUNTIME_ERROR error_condst_in_colon_stmt,    9
DEFINE_RUNTIME_ERROR error_body,                   10

error_accept:
    sta token_expected

    lda #0
    jsr send_error_num
    jsr send_accept_error
    jmp interactive


; arg1: pointer
.proc free_error
pointer    = temp1
string     = str2
numstring  = str1

    MEMORY_MOVE arg1, pointer

    ADDRESS_MOVE free_error_str, arg1
    ADDRESS_MOVE str2, arg2
    ldy free_error_str
    iny
    tya
    jsr memcpy

    MEMORY_MOVE pointer, arg1
    ADDRESS_MOVE numstring, arg2
    jsr hexstr

    ADDRESS_MOVE string, arg1
    ADDRESS_MOVE numstring, arg2
    jsr catstr

    ADDRESS_MOVE string, arg1
    jsr putstr

    jmp interactive
.endproc


; accumulator: error's code
.proc send_error_num
code      = temp1
string    = str2
numstring = str1

    sta code

    ADDRESS_MOVE error_num_str1, arg1
    ADDRESS_MOVE string, arg2
    ldy error_num_str1
    iny
    tya
    jsr memcpy

    ; add error code
    MEMORY_MOVE code, arg1
    ADDRESS_MOVE numstring, arg2
    jsr numstr

    ADDRESS_MOVE string, arg1
    ADDRESS_MOVE numstring, arg2
    jsr catstr

    ; if error occured in interactive mode,
    ; skip "at" and codeline number adding
    lda param_numbered_codeline
    cmp #0
    beq skip

    ; add "at" string
    ADDRESS_MOVE string, arg1
    ADDRESS_MOVE error_num_str2, arg2
    jsr catstr

    ; add codelines number
    ldy #0
    lda (currentCodeline),y
    sta arg1
    iny
    lda (currentCodeline),y
    sta arg1+1
    ADDRESS_MOVE numstring, arg2
    jsr numstr

    ADDRESS_MOVE string, arg1
    ADDRESS_MOVE numstring, arg2
    jsr catstr

skip:
    ; display error
    ADDRESS_MOVE string, arg1
    jsr putstr

    rts
.endproc


; accumulator: error code
.proc send_error
    ; get offset
    sta arg1
    lda #21
    sta arg2
    lda #0
    sta arg1+1
    sta arg2+1
    sta return
    sta return+1
    jsr byte_mul

    ; add to base
    lda #<(error_str)
    sta arg1
    lda #>(error_str)
    sta arg1+1
    lda return
    sta arg2
    lda return+1
    sta arg2+1
    jsr uint_add

    ; print
    lda return
    sta arg1
    lda return+1
    sta arg1+1
    jsr putstr

    rts
.endproc

.proc send_accept_error
string  = str1
tok_str = ptr1

    ; find string of expected token
    ADDRESS_MOVE token_names, tok_str
loop:
    ldy #0
    lda (tok_str),y
    cmp token_expected
    beq :+

    clc
    lda tok_str
    adc #13
    sta tok_str
    bcc loop
    inc tok_str+1
    jmp loop

    ; move string by 1 byte to skip token
:   UINT_INC tok_str

    ; copy it
    MEMORY_MOVE tok_str, arg1
    ADDRESS_MOVE string, arg2
    ldy #0
    lda (tok_str),y
    tay
    iny
    tya
    jsr memcpy

    ; cat with error message
    ADDRESS_MOVE string, arg1
    ADDRESS_MOVE accept_error_str, arg2
    jsr catstr

    ADDRESS_MOVE string, arg1
    jsr putstr

    rts

    ldy #1
    lda (tok_str),y
    sta length
    inc length

    clc
    lda tok_str
    adc #1
    sta tok_str
    bcc :+
    inc tok_str+1

:   MEMORY_MOVE tok_str, arg1
    ADDRESS_MOVE string, arg2
    ldy length
    iny
    tya
    jsr memcpy

    MEMORY_MOVE tok_str, arg1
    ;ADDRESS_MOVE

    ADDRESS_MOVE string, arg2
    clc
    lda arg2
    adc length
    sta arg2
    bcc :+
    inc arg2+1
:   dec arg2

    ADDRESS_MOVE accept_error_str, arg1
    lda accept_error_str
    jsr memcpy

    clc
    lda length
    adc accept_error_str
    sta string

    ADDRESS_MOVE string, arg1
    jsr putstr

.endproc
