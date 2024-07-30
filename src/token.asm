.segment "DATA"
keywords:    .byte 4, "data "       ; code = entry num. + $10
             .byte 4, "read "
             .byte 3, "dim  "
             .byte 2, "if   "
             .byte 4, "then "
             .byte 4, "else "
             .byte 4, "goto "
             .byte 5, "while"
             .byte 4, "endw "
             .byte 5, "endif"
             .byte 3, "for  "
             .byte 2, "to   "
             .byte 4, "endf "
             .byte 5, "print"
             .byte 5, "instr"
             .byte 3, "rem  "
             .byte 3, "end  "
             .byte 2, "do   "
             .byte 3, "inc  "
             .byte 3, "run  "
             .byte 4, "quit "
             .byte 3, "new  "
             .byte 3, "abs  "
             .byte 5, "hibyt"
             .byte 5, "lobyt"
             .byte 5, "input"
             .byte 4, "list "
             .byte 5, "break"
             .byte 5, "remov"
             .byte 0
syms:        .byte "(", ")", "-", "+", ">", "<", "=", "&", "|", "%", "*", "/", ",", ":", "[", "]", 0

.segment "BSS"
temp_toks:          .res 32
isFirstToken:       .res  1

.segment "CODE"


; stores token in accumulator
.macro TOK_GET
    ldy #0
    lda (tok_base),y
.endmacro

.macro PURE_TOK_NEXT
.scope
    inc tok_base
    bne :+
    inc tok_base+1
:
.endscope
.endmacro

.macro TOK_NEXT
.scope
    again:
    PURE_TOK_NEXT
    TOK_GET
    cmp #4
    beq again

    lda #0
    sta isFirstToken
.endscope
.endmacro

; same as TOK_GET
.macro TOK_GETNEXT
.scope
    VALUE_SAVE tok_base
    TOK_NEXT
    TOK_GET
    tax
    VALUE_RETURN tok_base
    txa
.endscope
.endmacro


; in accumulator should be stored expection
.proc tok_accept
    sta temp4
    TOK_GET
    cmp temp4
    beq equal

    lda temp4
    jmp error_accept

equal:
    TOK_NEXT
    rts
.endproc

.proc pure_tok_accept
    sta temp4
    TOK_GET
    cmp temp4
    beq equal

    jmp error_accept

equal:
    PURE_TOK_NEXT
    rts
.endproc

; x and y are lo and hi bytes of address of new base
.macro TOK_SETBASE
    stx tok_base
    sty tok_base+1
.endmacro


; arg1: pointer to string
.proc new_codeline
codeline  = ptr2
num_length = temp1

    ; move argument
    MEMORY_MOVE arg1, codeline

    ; get size of codeline
    ldy #0
    lda (codeline),y
    sta length
    iny

numsize:
    lda (codeline),y
    jsr isnumber
    cmp #1
    bne numsize_end

    ; check if codeline have num, but no code
    cpy length
    bne :+
    jmp finish_error

    ; next char
:   iny
    jmp numsize

    ; y now contains length of num including space char
numsize_end:
    sty num_length

    ; make string contain num only
    dey         ; remove space char
    tya
    ldy #0
    sta (codeline),y

    ; convert number to int
    jsr strnum

    ; get pointer to codetable entry
    MEMORY_MOVE return, arg1
    jsr codetable_new_entry

    ; move pointer so that it points to string without num
    lda codeline
    ldy num_length
    clc
    adc num_length
    sta codeline

    ; store length of new string
    lda length
    sec
    sbc num_length
    ldy #0
    sta (codeline),y

    ; tokenize string
    MEMORY_MOVE codeline, arg1
    ADDRESS_MOVE temp_toks, arg2
    jsr tokenize

    ; allocate memory
    pha                 ; save size
    jsr alloc

    ; put pointer
    ldy #2
    lda return
    sta (ptr1),y
    iny
    lda return+1
    sta (ptr1),y

    ; copy tokens
    ADDRESS_MOVE temp_toks, arg1
    MEMORY_MOVE  return,    arg2
    pla                 ; return size
    jsr memcpy

    ; clean up temp_toks
    ADDRESS_MOVE temp_toks, arg1
    ldx #0
    ldy #32
    jsr memset

    rts

finish_error:
;   jmp error
.endproc


; arg1: num of line
; makes ptr1 point to entry
.proc codetable_new_entry
entry           = ptr1
entry_dst       = ptr2
number          = temp3
shifting_needed = temp2

    VALUE_SAVE ptr2
    VALUE_SAVE temp2
    VALUE_SAVE temp3

    MEMORY_MOVE arg1, number

    ; get pointer to entry
    jsr codetable_get_entry
    sta shifting_needed

    ; if new entry was created
    cpy #1
    bne :+
    inc code_table_size

    ; if shifting needed
:   lda shifting_needed
    cmp #1
    beq makenew

    ; in other case clean up returned entry
    jmp entry_cleanup

makenew:
    ; save received entry pointer
    MEMORY_MOVE entry, entry_dst

    ; get last entry
    jsr codetable_get_last_entry

    ; save it into arg1 (src)
    MEMORY_MOVE entry, arg1

    ; get next entry (dst)
    UINT_CONST_ADD 4, entry, arg2

mkn_loop:
    ; shift
    lda #4
    jsr memcpy

    ; check if current src entry was the last one we need to shift
    MEMORY_MOVE entry_dst, arg2
    jsr uint_cmp

    cmp #1
    beq mkn_loop_end

    ; shift previous entry to the current src entry
    ; another words, src -> dst, src-4 -> src
    MEMORY_MOVE arg1, arg2
    UINT_CONST_SUB 4, arg1, arg1
    jmp mkn_loop

mkn_loop_end:
    MEMORY_MOVE arg1, entry

    ; clean tokens pointer to prevent freeing active token list
    lda #0
    ldy #2
    sta (entry),y
    iny
    sta (entry),y

    ; clean up entry and move there number
entry_cleanup:
    ; move number
    ldy #0
    lda number
    sta (entry),y
    iny
    lda number+1
    sta (entry),y

    ; free code pointer
    ; (if it is not equal null)
    ldy #2
    lda (entry),y
    cmp #0
    beq :+
    iny
    lda (entry),y
    cmp #0
    beq :+

    ldy #2
    lda (entry),y
    sta arg1
    iny
    lda (entry),y
    sta arg1+1
    jsr free

:   VALUE_RETURN temp3
    VALUE_RETURN temp2
    VALUE_RETURN ptr2

    rts
.endproc


; arg1: num of line
; makes ptr1 point to entry
; a: if there is need of shifting 1, else 0
; y: contains 1 if entry was created, else 0
.proc codetable_get_entry
number          = temp1
mul32_result    = temp2
entry           = ptr1

    VALUE_SAVE temp1
    VALUE_SAVE temp2

    MEMORY_MOVE arg1, number
    ADDRESS_MOVE code_table, entry

    ; check if there is no entries
    ; and if it is, return first entry
    lda code_table_size
    cmp #0
    bne :+
    lda #0  ; don't shift
    ldy #1  ; new entry
    jmp finish

    ; get last entry
:   jsr codetable_get_last_entry

    ; check if num is greater than num of last entry
    MEMORY_MOVE number, arg1
    ldy #0
    lda (entry),y
    sta arg2
    iny
    lda (entry),y
    sta arg2+1
    jsr uint_cmp

    cmp #2      ; it's >
    beq pointon_top

    cmp #1      ; it's =
    beq pointon_last

    jmp entry_find

pointon_top:
    jsr codetable_next_entry
    lda #0  ; don't shift
    ldy #1  ; new entry
    jmp finish

pointon_last:
    lda #0  ; don't shift
    ldy #0  ; existing entry
    jmp finish

entry_find:
    ; make entry point to the first entry
    ADDRESS_MOVE code_table, entry

entry_find_loop:
    ; compare entrys num
    ldy #0
    lda (entry),y
    sta arg1
    iny
    lda (entry),y
    sta arg1+1
    lda number
    sta arg2
    lda number+1
    sta arg2+1
    jsr uint_cmp

    ; if it's equal, return it
    cmp #1
    bne :+
    lda #0  ; don't shift
    ldy #0  ; existing entry
    jmp finish

    ; if it's greater, return it
:
    cmp #2
    bne :+
    lda #1  ; shift
    ldy #1  ; new entry
    jmp finish

    ; else continue
:
    jsr codetable_next_entry
    jmp entry_find_loop

finish:
    tax
    VALUE_RETURN temp2
    VALUE_RETURN temp1
    txa

    rts
.endproc


; makes ptr1 point to the last codetable entry
.proc codetable_get_last_entry
    ADDRESS_MOVE code_table, ptr1

    ; check if there is no entries
    lda code_table_size
    cmp #0
    bne :+
    rts

    ; get offset
:   tay
    dey
    tya

    asl
    rol return+1
    asl
    rol return+1
    sta return

    ; add it to base address
    UINT_ADD ptr1, return, ptr1
    rts
.endproc


; makes ptr1 point to the previous codetable entry
.proc codetable_prev_entry
    UINT_CONST_SUB $0004, ptr1, ptr1
    rts
.endproc


; makes ptr1 point to the next codetable entry
.proc codetable_next_entry
    UINT_CONST_ADD $0004, ptr1, ptr1
    rts
.endproc


; arg1: pointer to string
; arg2: pointer to token buffer
; accumulator will store length of token list
.proc tokenize
codeline  = ptr1
tokens    = buffer
strtok    = str1
char      = temp1

codeline_i = counter1
tokens_i   = counter2
strtok_i   = counter3
codeline_len = length

    VALUE_SAVE ptr1

    ldy #0
    ldx #0

    ; move arguments
    lda arg1
    sta codeline
    lda arg1+1
    sta codeline+1
    lda arg2
    sta tokens
    lda arg2+1
    sta tokens+1
    lda (codeline),y
    sta codeline_len

    ; reseting counters
    sty tokens_i
    lda #1
    sta codeline_i
    sta strtok_i

loop:
    ldy codeline_i
    lda (codeline),y
    sta char

    ; if it's a "
    cmp #$22
    bne :++

    ; if it's the first char
    lda strtok_i
    cmp #1
    bne :+
    jsr charstore
    jmp nextchar

    ; if not, make string
:   jmp strmake

    ; if it is a string, store char
:   ldx #1
    lda strtok,x
    cmp #$22
    bne :+
    jsr charstore
    jmp nextchar

:   lda char

    ; if it is a space
    cmp #$20
    bne :+
    lda #4
    jsr tokenstore
    jmp nexttok

    ; if it is a symbol
:   ldy strtok_i
    cpy #1
    bne l_notsym
    jsr issym
    cmp #$ff
    beq l_notsym
    jmp symmake

l_notsym:
    ; storing char into strtok
    jsr charstore

    ; check next symbol
    ldy codeline_i
    iny
    lda (codeline),y

    ; check if the first char is " (and therefore it is a string)
    ; and skip other checks since string ends only with "
;     sta char
;     ldx #1
;     lda strtok,x
;     cmp #$22        ; is "
;     beq stringcheck
;     lda char

    ; if it is the end of codeline
    dey
    cpy codeline_len
    beq l_makekey

    ; if it is a space
    cmp #$20
    beq l_makekey

    ; if it is a symbol
    jsr issym
    cmp #$ff
    beq nextchar
    bne l_makekey

; stringcheck:
;     lda char
;     cmp #$22
;     bne nextchar
;     jmp strmake

l_makekey:
    ; make token
    ldy strtok_i
    dey
    sty strtok
    jmp keymake

nextchar:
    ; check if we are in the end of codeline
    ldy codeline_i
    cpy codeline_len
    beq finish

    inc codeline_i
    jmp loop

nexttok:
    lda #1
    sta strtok_i
    jmp nextchar

finish:
    ; store zero token (EOT)
    lda #0
    jsr tokenstore

    VALUE_RETURN ptr1
    lda tokens_i
    rts
finish_error:
;   jmp error
    jmp end

    ; in accumulator stored symbols index in syms table
symmake:
    clc
    adc #$30
    jsr tokenstore
    jmp nexttok

keymake:
    ; recognize if tokens is number
    ldx #1
    lda strtok,x
    jsr isnumber
    cmp #1
    bne :+
    jmp nummake

:
    ; prepare arguments for string comparasion
    lda #<(strtok)
    sta arg1
    lda #>(strtok)
    sta arg1+1
    lda #<(keywords)
    sta arg2
    lda #>(keywords)
    sta arg2+1
    ldx #0

kmk_loop:
    jsr cmpstr
    cmp #1
    beq kmk_finish

    ; go to the next keyword
    lda arg2
    clc
    adc #6
    bcc kmk_skipadd
    inc arg2+1
kmk_skipadd:
    sta arg2

    ; check if there is no more keywords
    ; in this case ...
    ldy #0
    lda (arg2),y
    cmp #0
    beq varmake

    inx
    jmp kmk_loop

kmk_finish:
    txa
    clc
    adc #$10
    jsr tokenstore
    jmp nexttok

varmake:
    lda #1
    jsr tokenstore
    ; name length
    dec strtok_i
    lda strtok_i
    jsr tokenstore

    ; store name
    ldx #1

:   lda strtok,x
    jsr tokenstore
    cpx strtok_i
    beq :+
    inx
    jmp :-
:   jmp nexttok

nummake:
    ; prepare arguments
    lda #<(strtok)
    sta arg1
    lda #>(strtok)
    sta arg1+1

    ; convert to number
    jsr strnum

    ; store tokens
    lda #2
    jsr tokenstore
    lda return
    jsr tokenstore
    lda return+1
    jsr tokenstore
    jmp nexttok

strmake:
    ; store key code
    lda #3
    jsr tokenstore

    ; store string length
    ldx strtok_i
    dex
    dex
    txa
    jsr tokenstore

    ; string contains extra " char. skiping it
    lda strtok_i
    sta strtok
    ldx #1

strmk_loop:
    inx
    cpx strtok_i
    bne :+
    jmp nexttok

:   lda strtok,x
    jsr tokenstore
    jmp strmk_loop

; in accumulator stored token
tokenstore:
    ldy tokens_i
    sta (tokens),y
    inc tokens_i
    lda codeline_i
    rts

charstore:
    ldx strtok_i
    lda char
    sta strtok,x
    inc strtok_i
    rts
.endproc


; a:    sym
; a:    number of symbol if it is a symbol
;       otherwise ff
.proc issym
sym = temp4

    sta sym
    txa
    pha
    ldx #0
loop:
    lda syms,x
    cmp #0
    beq false
    cmp sym
    beq true
    inx
    jmp loop

false:
    lda #$ff
    jmp finish
true:
    txa
finish:
    sta sym      ; save return value
    pla
    tax
    lda sym
    rts
.endproc
