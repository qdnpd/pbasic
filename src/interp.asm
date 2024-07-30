.segment "BSS"
interpret_toks:     .res 32

.segment "CODE"

.proc interpret
tokens = interpret_toks

    ; tokenize string
    lda #<(tokens)
    sta arg2
    lda #>(tokens)
    sta arg2+1
    jsr tokenize

    ; set token base and get first token
    ldx #<(tokens)
    ldy #>(tokens)
    TOK_SETBASE
    TOK_GET

    ; switch
    cmp TS_RUN
    bne :+
    jsr interpret_run
    jmp finish

:   cmp TS_LIST
    bne :+
    jsr interpret_list
    rts

:   cmp TS_NEW
    bne :+
    jsr interpret_new
    rts

:   cmp TS_REMOVE
    bne :+
    jsr interpret_remove
    rts

:   cmp TS_QUIT
    bne :+
    jmp end

:   jsr interpret_codeline

finish:
    ; clean up tokens and return
    ADDRESS_MOVE interpret_toks, arg1
    ldx #0
    ldy #32
    jsr memset
    rts
.endproc


.proc interpret_run
    lda #1
    sta param_numbered_codeline
    lda #0
    sta parse_do_break

    jsr variables_clean
    CODELINE_FIRST

eval_loop:
    jsr parse_stmt

    ; if exection ended up      TO REWRITE
    cmp #0
    bne :+
    rts

:   jsr codeline_next
    jmp eval_loop
.endproc


.proc interpret_remove
entry       = ptr1
entry_dst   = ptr2

    TOK_NEXT
    jsr parse_expr

    MEMORY_MOVE return, arg1
    jsr codetable_get_entry

    ; if there is no codeline with such number, quit
    cpy #1
    bne :+
    jmp error_codeline_wasnot_found

    ; free pointer to tokens
:   ldy #2
    lda (entry),y
    sta arg1
    iny
    lda (entry),y
    sta arg1+1
    jsr free

    ; find out codelines index
    MEMORY_MOVE entry, entry_dst
    CODELINE_FIRST

index_loop:
    UINT_CMP entry, entry_dst
    cmp #1
    beq :+
    jsr codeline_next
    jmp index_loop

    ; move other codelines
:   MEMORY_MOVE entry_dst, arg1

shifting_loop:
    lda currentCodelineIndex
    cmp code_table_size
    beq finish

    MEMORY_MOVE arg1, arg2       ; src -> dst
    UINT_CONST_ADD 4, arg1, arg1 ; src+4 -> src
    lda #4
    jsr memcpy

    inc currentCodelineIndex
    jmp shifting_loop

finish:
    dec code_table_size
    rts
.endproc


.proc interpret_codeline
    lda #0
    sta param_numbered_codeline

    TOK_GET
    cmp TS_OPBR
    beq :+
    jsr parse_stmt
    rts

:   jsr parse_expr

    MEMORY_MOVE return, arg1
    ADDRESS_MOVE str1, arg2
    jsr numstr

    ADDRESS_MOVE str1, arg2
    jsr putstr

    rts
.endproc

.proc interpret_list
    TOK_NEXT
    TOK_GET

    cmp TS_EOT
    bne :+
    jmp alllines

:   lda TS_NUM
    jsr tok_accept

    TOK_GET
    sta arg1
    TOK_NEXT
    TOK_GET
    sta arg1+1
    TOK_NEXT

    TOK_GET

    cmp TS_EOT
    beq oneline

    lda TS_COMMA
    jsr tok_accept
    lda TS_NUM
    jsr tok_accept

    TOK_GET
    sta arg2
    TOK_NEXT
    TOK_GET
    sta arg2+1
    TOK_NEXT

    jmp severallines

alllines:
    lda #0
    sta arg1
    sta arg1+1
    sta arg2
    sta arg2+1
    jmp display_codelines

oneline:
    MEMORY_MOVE arg1, arg2
    jmp display_codelines

severallines:
    jmp display_codelines
.endproc

.proc interpret_new
    jsr codelines_clean
    rts
.endproc
