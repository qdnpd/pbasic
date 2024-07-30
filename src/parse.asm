.define TS_EOT      #0
.define TS_VAR      #1
.define TS_NUM      #2
.define TS_STR      #3
.define TS_SPACE    #4

.define TS_OPBR     #$30
.define TS_CLSBR    #$31
.define TS_MINUS    #$32
.define TS_PLUS     #$33
.define TS_GREATER  #$34
.define TS_LESSER   #$35
.define TS_EQUAL    #$36
.define TS_AND      #$37
.define TS_OR       #$38
.define TS_MOD      #$39
.define TS_MUL      #$3A
.define TS_DIV      #$3B
.define TS_COMMA    #$3C
.define TS_COLON    #$3D
.define TS_OPSQR    #$3E
.define TS_CLSSQR   #$3F

.define TS_DATA     #$10
.define TS_READ     #$11
.define TS_DIM      #$12
.define TS_IF       #$13
.define TS_THEN     #$14
.define TS_ELSE     #$15
.define TS_GOTO     #$16
.define TS_WHILE    #$17
.define TS_FOR      #$1A
.define TS_TO       #$1B
.define TS_PRINT    #$1D
.define TS_INSTR    #$1E
.define TS_REM      #$1F
.define TS_END      #$20
.define TS_DO       #$21
.define TS_BREAK    #$2B

.define TS_INC      #$22
.define TS_ABS      #$26
.define TS_HIBYTE   #$27
.define TS_LOBYTE   #$28
.define TS_INPUT    #$29

.define TS_RUN      #$23
.define TS_QUIT     #$24
.define TS_NEW      #$25
.define TS_LIST     #$2A
.define TS_REMOVE   #$2C

; used only in error messages
.define TS_OP       #$FF

.segment "BSS"
parse_another_stmt:         .res 1
parse_do_break:             .res 1
parse_else:                 .res 1
parse_cond_arg1:            .res 2
parse_cond_arg2:            .res 2
parse_temp_var_name:        .res 32

.segment "CODE"


; arg1: pointer to buffer
.proc parse_var_name
name    = arg1
counter = temp1

    VALUEB_SAVE counter
    VALUEB_SAVE length

    PURE_TOK_NEXT        ; accept VAR
    TOK_GET
    sta length
    inc length

    ; store variables name
    lda #0
    sta counter

loop:
    TOK_GET
    ldy counter
    sta (name),y

    cpy length
    beq :++
    inc counter
    TOK_NEXT
    jmp loop

:   VALUEB_RETURN length
    VALUEB_RETURN counter
    rts
.endproc

; returns variables entry
.proc parse_var_entry
name    = ptr3

    ADDRESS_MOVE parse_temp_var_name, name
    ADDRESS_MOVE parse_temp_var_name, arg1
    jsr parse_var_name

    MEMORY_MOVE name, arg1
    jsr var_entry

    cmp #0
    bne :+

    MEMORY_MOVE name, arg1
    lda #2
    jsr var_define

:   rts
.endproc

; returns variables address
.proc parse_var_address
name    = ptr3
counter = temp1

    VALUEB_SAVE counter
    VALUEB_SAVE length

    ADDRESS_MOVE parse_temp_var_name, name
    ADDRESS_MOVE parse_temp_var_name, arg1
    jsr parse_var_name

    MEMORY_MOVE name, arg1
    jsr var_address

    VALUEB_RETURN length
    VALUEB_RETURN counter
    rts
.endproc

; returns variables value
.proc parse_var_value
    ldy #0
    lda (return),y
    pha
    iny
    lda (return),y
    pha

    pla
    sta return+1
    pla
    sta return
    rts
.endproc

; parse_var_address with variable definition if it was not defined
.proc parse_var_address_ex
name = ptr3

    jsr parse_var_entry

    ldy #2
    lda (return),y
    pha
    iny
    lda (return),y
    pha

    pla
    sta return+1
    pla
    sta return

    rts
.endproc


; arg1: variables address
.proc parse_arr_address
offset = temp1

    VALUE_SAVE offset

    TOK_NEXT        ; accept (

    VALUE_SAVE arg1
    jsr parse_expr
    asl return
    rol return+1
    MEMORY_MOVE return, offset
    VALUE_RETURN return

    lda TS_CLSBR
    jsr tok_accept  ; accept )

    UINT_ADD offset, return, return
    VALUE_RETURN offset
    rts
.endproc

.proc parse_arr_value
    jsr parse_arr_address

    ldy #0
    lda (return),y
    pha
    iny
    lda (return),y
    pha

    pla
    sta return+1
    pla
    sta return
    rts
.endproc


; arg1: variables address
.proc parse_arr_byte_address
offset = temp1

    VALUE_SAVE offset

    TOK_NEXT        ; accept [

    VALUE_SAVE arg1
    jsr parse_expr
    MEMORY_MOVE return, offset
    VALUE_RETURN return

    lda TS_CLSSQR
    jsr tok_accept  ; accept ]

    UINT_ADD offset, return, return
    VALUE_RETURN offset
    rts
.endproc

.proc parse_arr_byte_value
    jsr parse_arr_byte_address

    ldy #0
    lda (return),y

    sta return
    lda #0
    sta return+1

    rts
.endproc


; result = arg1 op arg2
; op must be stored in accumulator
.proc do_op
    cmp TS_MINUS
    beq do_minus
    cmp TS_PLUS
    beq do_plus
    cmp TS_AND
    beq do_and
    cmp TS_OR
    beq do_or
    cmp TS_MOD
    beq do_mod
    cmp TS_MUL
    beq do_mul
    cmp TS_DIV
    beq do_div

    lda TS_OP
    jsr tok_accept

do_minus:
    jsr int_sub
    rts

do_plus:
    jsr int_add
    rts

do_and:
    jsr int_and
    rts

do_or:
    jsr int_or
    rts

do_mod:
    jsr int_mod
    rts

do_mul:
    jsr int_mul
    rts

do_div:
    jsr int_div
    rts
.endproc


; return = arg1 op arg2
; op must be stored in accumulator
.proc do_cond_op
    tay
    MEMORY_MOVE parse_cond_arg1, arg1
    MEMORY_MOVE parse_cond_arg2, arg2

    jsr uint_cmp            ; REPLACE WITH INT_CMP!!!!

    cpy TS_EQUAL
    bne :+
    cmp #1
    beq true
    bne false

:   cpy TS_LESSER
    bne :+
    cmp #0
    beq true
    bne false

:   cmp #2
    beq true
    bne false

false:
    lda #0
    sta return
    rts

true:
    lda #1
    sta return
    rts
.endproc


.proc parse_factor
value       = parse_temp1
token       = parse_temp3
sign        = parse_temp4

    ; save values
    VALUE_SAVE  value
    VALUEB_SAVE token
    VALUE_SAVE  sign

    lda #0
    sta sign

    ; if first token is -, then set invert flag
    TOK_GET
    cmp TS_MINUS
    bne :++
    lda #1
    sta sign
    TOK_NEXT

:   TOK_GET
    sta token

    lda TS_NUM
    cmp token
    beq num
    lda TS_OPBR
    cmp token
    beq expr
    lda TS_VAR
    cmp token
    bne :+
    jmp var

:   jsr parse_call
    jmp finish

num:
    PURE_TOK_NEXT
    TOK_GET
    sta return
    PURE_TOK_NEXT
    TOK_GET
    sta return+1
    TOK_NEXT
    jmp finish

expr:
    TOK_NEXT
    jsr parse_expr
    lda TS_CLSBR
    jsr tok_accept
    jmp finish

var:
    jsr parse_var_address
    MEMORY_MOVE return, arg1

    ; if next token is bracket, then it is an array
    TOK_GET
    cmp TS_OPBR
    beq array

    TOK_GET
    cmp TS_OPSQR
    beq array_sq

    jsr parse_var_value
    jmp finish

array:
    jsr parse_arr_value
    jmp finish

array_sq:
    jsr parse_arr_byte_value

finish:
    VALUE_RETURN  sign
    VALUEB_RETURN token
    VALUE_RETURN  value

    ; if sign = 1, invert return
    lda sign
    cmp #1
    bne :+
    INVERT return

:   rts
.endproc


.proc parse_term
factor1Val  = parse_temp1
op          = parse_temp2

    ; save values
    VALUE_SAVE  factor1Val
    VALUEB_SAVE op

    ; get factor value
    jsr parse_factor
    lda return
    sta factor1Val
    lda return+1
    sta factor1Val+1

    ; get op
    TOK_GET
    sta op

    ; check if it is not */div/%
    lda TS_MUL
    cmp op
    beq :+
    lda TS_DIV
    cmp op
    beq :+
    lda TS_MOD
    cmp op
    beq :+

    ; if it is not, return value
    jmp finish

    ; else get next factor's value
:   TOK_NEXT
    jsr parse_factor
    lda return
    sta arg2
    lda return+1
    sta arg2+1

    ; do op
    lda factor1Val
    sta arg1
    lda factor1Val+1
    sta arg1+1
    jsr do_op

    ; return
finish:
    VALUEB_RETURN op
    VALUE_RETURN  factor1Val
    rts
.endproc

.proc parse_expr
term1Val    = parse_temp1
op          = parse_temp2

    VALUE_SAVE  term1Val
    VALUEB_SAVE op

    jsr parse_term
    MEMORY_MOVE return, term1Val

    TOK_GET
    sta op

    ; check if it is not +/-/and/or
    lda TS_PLUS
    cmp op
    beq :+
    lda TS_MINUS
    cmp op
    beq :+
    lda TS_AND
    cmp op
    beq :+
    lda TS_OR
    cmp op
    beq :+

    jmp finish

:   TOK_NEXT
    jsr parse_term

    MEMORY_MOVE return,   arg2
    MEMORY_MOVE term1Val, arg1
    lda op
    jsr do_op

finish:
    VALUEB_RETURN op
    VALUE_RETURN  term1Val
    rts
.endproc


.proc parse_cond
cond1 = temp1

    ; save old value
    lda cond1
    pha

    ; if there is bracket, accept it
    TOK_GET
    cmp TS_OPBR
    bne :++
    TOK_NEXT

:   jsr parse_expr
    MEMORY_MOVE return, parse_cond_arg1

    TOK_GET
    cmp TS_EQUAL
    beq :+
    cmp TS_LESSER
    beq :+
    cmp TS_GREATER
    beq :+

    ; if there is no op, then it is condition with one argument
    ; it could return 0 if var store 0, otherwise 1
    lda parse_cond_arg1
    cmp #0
    bne one
    lda parse_cond_arg1+1
    cmp #0
    bne one

    lda #0
    sta return+1
    sta return
    jmp check_next_cond

one:
    lda #0
    sta return+1
    lda #1
    sta return
    jmp check_next_cond

:   pha         ; save op
    TOK_NEXT

    jsr parse_expr
    MEMORY_MOVE return, parse_cond_arg2

    pla
    jsr do_cond_op

check_next_cond:
    ; if there is bracket, accept it
    TOK_GET
    cmp TS_CLSBR
    bne :++
    TOK_NEXT

    ; check if there is another one condition
:   TOK_GET
    ldy TS_AND
    cmp TS_AND
    beq another_cond
    cmp TS_OR
    beq another_cond
    jmp finish

another_cond:
    ; if there is, save result and op
    pha
    lda return
    pha

    ; get value of cond
    TOK_NEXT
    jsr parse_cond
    sta cond1

    ; return result and op
    pla
    tax
    pla
    tay
    txa

    cpy TS_AND
    bne :+
    and cond1
    jmp restore

    ; or op
:   ora cond1
    jmp restore

finish:
    pla
    sta cond1
    lda return
    rts
restore:
    pla
    sta cond1
    rts
.endproc


.proc parse_letst
varPointer = parse_temp4
writeByte  = temp1

    lda #0
    sta writeByte

    jsr parse_var_address_ex

    ; check if it is an array
    TOK_GET
    cmp TS_OPBR
    beq array
    cmp TS_OPSQR
    beq array_sq

    jmp skip

array:
    MEMORY_MOVE return, arg1
    jsr parse_arr_address
    jmp skip

array_sq:
    MEMORY_MOVE return, arg1
    jsr parse_arr_byte_address
    lda #1
    sta writeByte

skip:
    MEMORY_MOVE return, varPointer

    lda TS_EQUAL
    jsr tok_accept

    jsr parse_expr

    lda writeByte
    cmp #0
    bne write_byte

    ; put expr's value into variable
    ldy #0
    lda return
    sta (varPointer),y
    iny
    lda return+1
    sta (varPointer),y
    rts

write_byte:
    lda return
    sta (varPointer),y
    rts
.endproc


.proc parse_printst
    ldx #0
    lda #0
    sta str1,x

    TOK_NEXT        ; accept print

loop:
    ; check if argument is string
    TOK_GET

    cmp TS_STR
    beq string

    cmp TS_OPBR
    beq array

    ; get exprs value
    jsr parse_expr

    ; convert into string
    MEMORY_MOVE return, arg1
    ADDRESS_MOVE str2, arg2
    jsr numstr

    ; print string
    ADDRESS_MOVE str1, arg1
    ADDRESS_MOVE str2, arg2
    jsr catstr

    jmp next_arg

array:
    jsr parse_var_address

    ADDRESS_MOVE str1, arg1
    MEMORY_MOVE return, arg2
    jsr catstr

    jmp next_arg

string:
    ; print string
    PURE_TOK_NEXT
    ADDRESS_MOVE str1, arg1
    MEMORY_MOVE tok_base, arg2
    jsr catstr

    D tok_base
    ; skip string
    clc
    TOK_GET
    adc tok_base
    sta tok_base

    lda tok_base+1
    adc #0
    sta tok_base+1

    TOK_NEXT
    D tok_base

next_arg:
    ; if current token is ',', then there is another one argument
    TOK_GET
    cmp TS_COMMA
    bne finish

    TOK_NEXT
    jmp loop

finish:
    ADDRESS_MOVE str1, arg1
    jsr putstr
    jsr newline
    rts
.endproc


.proc parse_ifst
    ;VALUEB_SAVE parse_else

    TOK_NEXT    ; accept "if"

    jsr parse_cond
    cmp #1
    beq cond_true

    TOK_GET
    cmp TS_EOT
    beq :+
    rts

    ; if there is a body, skip all its codelines
:   jsr parse_skipbody
    rts

cond_true:
    lda TS_THEN
    jsr tok_accept

    ; if next token is not EOT, then parse statement and quit
    TOK_GET
    cmp TS_EOT
    beq ifst_parsest

    jsr parse_stmt
    rts

ifst_parsest:
    jsr codeline_next

    ; if we are in the end, give error
    cmp #0
    bne :+
    lda TS_END
    jsr tok_accept

    ; check if there is end
:   TOK_GET
    cmp TS_END
    bne :+
    rts

:   jsr parse_stmt
    jmp ifst_parsest
.endproc


.proc parse_whilest
    lda TS_WHILE
    jsr tok_accept

loop:
    CODELINE_SAVE

    jsr parse_cond
    cmp #1
    bne finish

    lda TS_DO
    jsr tok_accept

    TOK_GET
    cmp TS_EOT
    beq until_not_end

    jsr parse_stmt
    jmp again

until_not_end:
    jsr codeline_next
    cmp #0
    bne :+
    lda TS_END
    jsr tok_accept

:   TOK_GET
    cmp TS_END
    beq again

    jsr parse_stmt

    lda #0
    cmp parse_do_break
    beq until_not_end

    sta parse_do_break
    jmp finish

again:
    CODELINE_LOAD
    jmp loop

finish:
    CODELINE_CLEAR_SAVED

    ; if while-body was in the same codeline as cond, do not do skipping
    TOK_GETNEXT
    cmp TS_EOT
    beq :+
    rts

:   jsr parse_skipbody
    rts
.endproc


.proc parse_forst
varPointer = ptr2
limit      = for_limit
flag       = for_flags

    ; save values
    VALUE_SAVE varPointer
    VALUE_SAVE limit
    VALUEB_SAVE flag

    lda TS_FOR
    jsr tok_accept

    ; parse statement
    CODELINE_SAVE
    jsr parse_letst
    CODELINE_LOAD

    ; get variables pointer
    jsr parse_var_address
    MEMORY_MOVE return, varPointer

    ; make sure ...
    TOK_NEXT
    lda TS_NUM
    jsr tok_accept
    PURE_TOK_NEXT
    TOK_NEXT

    ; accept to
    lda TS_TO
    jsr tok_accept

    ; save limit value
    lda TS_NUM
    jsr tok_accept

    TOK_GET
    sta limit
    PURE_TOK_NEXT
    TOK_GET
    sta limit+1
    TOK_NEXT

    ; accept do
    lda TS_DO
    jsr tok_accept

    ; save codeline for statement parsing
    CODELINE_SAVE

    ; recognize is for-body is one-line or not
    TOK_GET
    cmp TS_EOT
    beq until_not_endf_init

    ; if one-line
    lda #0
    sta flag

one_line:
    jsr parse_stmt
    jmp next

    ; if not
until_not_endf_init:
    lda #1
    sta flag

until_not_end:
    jsr codeline_next
    cmp #0
    bne :+
    lda TS_END
    jsr tok_accept

:   TOK_GET
    cmp TS_END
    beq next

    jsr parse_stmt

    lda #0
    cmp parse_do_break
    beq until_not_end

    sta parse_do_break
    CODELINE_LOAD
    jmp finish

next:
    ; restore first statements codeline
    CODELINE_LOAD

    ; increment variables value
    clc
    ldy #0
    lda (varPointer),y
    adc #1
    sta (varPointer),y

    bcc :+
    iny
    lda (varPointer),y
    adc #0
    sta (varPointer),y

    ; compare variables value with limit
:   ldy #0
    lda (varPointer),y
    sta arg1
    ldy #1
    lda (varPointer),y
    sta arg1+1

    lda limit
    sta arg2
    lda limit+1
    sta arg2+1

    jsr uint_cmp
    cmp #2
    beq finish

    ; if variable is not greater then limit, continue
    CODELINE_SAVE

    lda flag
    cmp #0
    bne :+
    jmp one_line
:   jmp until_not_end

    ; if it is, quit
finish:
    lda flag
    cmp #0
    bne :+
    jmp restore

    ; skip codelines
:   jsr parse_skipbody
    jmp restore

restore:
    pla
    sta flag
    pla
    sta limit+1
    pla
    sta limit
    pla
    sta varPointer+1
    pla
    sta varPointer
    rts
.endproc


.proc parse_call
function = call_temp
    ; save old values
    lda function
    pha

    ; store function
    TOK_GET
    sta function
    TOK_NEXT

    ; if it is input, skip argument passing
    lda TS_INPUT
    cmp function
    bne :+
    jsr fun_input
    jmp restore

    ; accept (
:   lda TS_OPBR
    jsr tok_accept

    ; get argument
    jsr parse_expr

    ; store argument
    lda return
    sta arg1
    lda return+1
    sta arg1+1

    ; accept )
    lda TS_CLSBR
    jsr tok_accept

    ; call function
    lda function
    cmp TS_INC
    bne :+
    jsr fun_inc
    jmp restore

:   cmp TS_ABS
    bne :+
    jsr fun_abs
    jmp restore

:   cmp TS_HIBYTE
    bne :+
    jsr fun_hibyte
    jmp restore

:   cmp TS_LOBYTE
    bne :+
    jsr fun_lobyte
    jmp restore

:   jmp error_no_such_function

restore:
    pla
    sta function
    rts
.endproc


.proc parse_dimstm
varPointer = ptr2
name       = ptr3
offset     = temp1

    lda TS_DIM
    jsr tok_accept

    ADDRESS_MOVE parse_temp_var_name, arg1
    ADDRESS_MOVE parse_temp_var_name, name
    jsr parse_var_name

    ; if variable was defined, send error
    MEMORY_MOVE name, arg1
    jsr var_entry
    cmp #0
    beq :+
    jmp error_dim_defined_var

    ; define variable
:   lda TS_COMMA
    jsr tok_accept
    jsr parse_expr

    ; if size > 255, send error
    UINT_CONST_CMP $00FF, return
    cmp #2
    bne :+
    jmp error_dim_too_big_size

:   MEMORY_MOVE name, arg1
    lda return
    jsr var_define

    rts
.endproc

.proc parse_instrst
input       = str1
varPointer  = parse_temp4

    TOK_NEXT

    jsr parse_var_address
    MEMORY_MOVE return, varPointer

    ADDRESS_MOVE input, arg1
    jsr getstr

    ADDRESS_MOVE input, arg1
    MEMORY_MOVE varPointer, arg2
    ldx #0
    lda input,x
    tax
    inx
    txa
    jsr memcpy

    rts
.endproc

.proc parse_datast
varPointer = parse_temp4
buffer     = str1
offset     = temp1
allocLen   = temp2
name       = temp3

    TOK_NEXT

    ADDRESS_MOVE parse_temp_var_name, arg1
    ADDRESS_MOVE parse_temp_var_name, name
    jsr parse_var_name

    ; if array was defined, send error
    MEMORY_MOVE name, arg1
    jsr var_entry
    cmp #0
    beq :+
    jmp error_dim_defined_var

:   lda TS_COMMA
    jsr tok_accept

    TOK_GET
    cmp TS_STR
    beq string
    jmp number

string:
    TOK_NEXT
    TOK_GET
    sta length
    inc length
    ldx #0

string_loop:
    cpx length
    beq string_finish

    TOK_GET
    sta buffer,x
    TOK_NEXT
    inx
    jmp string_loop

string_finish:
    ; if length is odd, replace it to the nearest greater even
    lda length
    lsr
    bcc :+
    tay
    iny
    tya

:   asl
    sta allocLen
    jmp finish

number:
    ldx #0
    sty length

number_loop:
    lda TS_NUM
    jsr pure_tok_accept

    TOK_GET
    sta buffer,x
    inx
    inc length
    PURE_TOK_NEXT

    TOK_GET
    sta buffer,x
    inx
    inc length
    TOK_NEXT

    TOK_GET
    cmp TS_COMMA
    bne number_finish

    TOK_NEXT
    jmp number_loop

number_finish:
    lda length
    sta allocLen

finish:
    MEMORY_MOVE name, arg1
    lda allocLen
    jsr var_define

    ; store pointer
    ldy #2
    lda (return),y
    sta varPointer
    iny
    lda (return),y
    sta varPointer+1

    ; copy data into array
    ADDRESS_MOVE buffer, arg1
    MEMORY_MOVE varPointer, arg2
    lda length
    jsr memcpy

    rts
.endproc

.proc parse_gotost
    TOK_NEXT
    jsr parse_expr

    MEMORY_MOVE return, arg1
    jsr codeline_set
    D currentCodeline
    rts
.endproc

.proc parse_stmt
again:
    TOK_GET
    cmp #4
    bne skip

    TOK_NEXT
    TOK_GET

skip:
    cmp TS_VAR
    bne :+
    jsr parse_letst
    jmp finish

:   cmp TS_PRINT
    bne :+
    jsr parse_printst
    jmp finish

:   cmp TS_DIM
    bne :+
    jsr parse_dimstm
    jmp finish

:   cmp TS_INSTR
    bne :+
    jsr parse_instrst
    jmp finish

:   cmp TS_DATA
    bne :+
    jsr parse_datast
    jmp finish

:   lda parse_another_stmt
    cmp #1
    bne :+
    jmp error_condst_in_colon_stmt

:   TOK_GET
    cmp TS_IF
    bne :+
    jsr parse_ifst
    jmp finish

:   cmp TS_GOTO
    bne :+
    jsr parse_gotost
    jmp finish

:   cmp TS_WHILE
    bne :+
    jsr parse_whilest
    jmp finish

:   cmp TS_FOR
    bne :+
    jsr parse_forst
    jmp finish

:   cmp TS_BREAK
    bne :+
    lda #1
    sta parse_do_break
    jmp finish

:   cmp TS_END
    bne :+
    lda #0
    rts

:   jmp error_invalid_statement

finish:
    ; check if there is another statement
    TOK_GET
    cmp TS_COLON
    bne :++

    lda #1
    sta parse_another_stmt
    TOK_NEXT
    jmp again

:   lda #0
    sta parse_another_stmt
    lda #1
    rts
.endproc


; skips codelines until the first token in one will be the same token, as was in accumulator
.proc parse_skipto
token = temp1

    ; save old value
    tay
    lda token
    pha

    ; save argument
    sty token

loop:
    jsr codeline_next
    TOK_GET
    cmp token
    bne loop

    ; return old value
    pla
    sta token

    rts
.endproc

; used in if, while and for parsing functions to skip their body
.proc parse_skipbody
counter = temp1

    VALUEB_SAVE counter
    lda #0
    sta counter

loop:
    jsr codeline_next
    TOK_GET

    cmp TS_END
    bne no_end

    ; if there was a body, decrement counter
    lda #0
    cmp counter
    beq no_body

    dec counter
    jmp loop

no_end:
    ; check if there is if/while/for
    cmp TS_IF
    beq new_body
    cmp TS_WHILE
    beq new_body
    cmp TS_FOR
    beq new_body

    ; TOREWRITE
    ; if it's the end of program and thus the body isn't closed
    cmp TS_EOT
    bne loop
    jmp error_body

new_body:
    inc counter
    jmp loop

no_body:
    VALUEB_RETURN counter
    rts
.endproc
