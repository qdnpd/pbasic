.segment "ZEROPAGE"
arg1:        .res 2
arg2:        .res 2
return:      .res 2
temp1:       .res 2
temp2:       .res 2
temp3:       .res 2
temp4:       .res 2
counter1:    .res 1
counter2:    .res 1
counter3:    .res 1
counter4:    .res 1
buffer:      .res 2
ptr1:        .res 2
ptr2:        .res 2
ptr3:        .res 2
str1:        .res 64
str2:        .res 64
length:      .res 1
fun_adr:     .res 2

kernel_ptr1:        .res   2
kernel_ptr2:        .res   2
kernel_ptr3:        .res   2
kernel_temp1:       .res   2
kernel_temp2:       .res   2
kernel_temp3:       .res   2
kernel_src:         .res   2
kernel_dst:         .res   2
alloc_list_last:    .res   2
alloc_last_adr:     .res   2

parse_temp1:    .res 2
parse_temp2:    .res 2
parse_temp3:    .res 1
parse_temp4:    .res 2
tok_base:       .res 2

.segment "BSS"
code_table:  .res 256*4
var_table:   .res 4*64

code_table_size:    .res 1
var_table_size:     .res 1

for_var:            .res 2
for_limit:          .res 2
for_flags:          .res 1
call_temp:          .res 1

param_numbered_codeline:    .res 1

.segment "DATA"
dbgmsg_alloc:       .byte 16, "memory allocated"
dbgmsg_free:        .byte 15, "memory realised"

;alloc_list = $BFFF

.include "c64.asm"

.segment "CODE"

; arg1: pointer to memory
; x: data to fill by
; y: size
.proc memset
data = kernel_temp1
size = kernel_temp1+1

    VALUE_SAVE kernel_temp1
    stx data
    sty size
    ldy #0

loop:
    cpy size
    bne :+
    VALUE_RETURN kernel_temp1
    rts

:   lda data
    sta (arg1),y
    iny
    jmp loop
.endproc

.proc kernel_memcpy
source = kernel_src
dest   = kernel_dst
size   = kernel_temp3

    ; save size
    stx size
    sty size+1

    MEMORY_MOVE arg1, source
    MEMORY_MOVE arg2, dest

loop:
    lda #0
    cmp size
    bne :+
    cmp size+1
    bne :+
    jmp finish

:   ldy #0
    lda (source),y
    sta (dest),y

    UINT_INC source
    UINT_INC dest

    lda size
    cmp #0
    bne :+
    dec size+1
    jmp loop

:   dec size
    jmp loop

finish:
    rts
.endproc


; free memory (in arg1)
.proc free
memoryAdr    = kernel_ptr1
pointer      = kernel_ptr2

    MEMORY_MOVE arg1, memoryAdr
    ADDRESS_MOVE alloc_list-3, pointer

loop:
    ; compare pointer with alloc_list_last
    ; check if we did pass last entry
    UINT_CMP pointer, alloc_list_last
    cmp #0
    bne :+
    jmp return_error

    ; if addresses equal, remove entry
:   ldy #2
    lda (pointer),y
    cmp memoryAdr
    bne :+
    dey
    lda (pointer),y
    cmp memoryAdr+1
    bne :+
    jmp :++
:   jmp next

    ; shift list right
:   MEMORY_MOVE pointer, arg2
    UINT_CONST_SUB $0003, arg2, arg1

shift_loop:
    ; perform shift
    ldx #3
    ldy #0
    jsr kernel_memcpy

    ; if it was the last entry, quit
    UINT_CMP arg1, alloc_list_last
    cmp #1
    bne :+
    jmp shift_loop_finish

    ; next
:   MEMORY_MOVE arg1, arg2
    UINT_CONST_SUB $0003, arg2, arg1
    jmp shift_loop

shift_loop_finish:
    ; move alloc_list_last
    UINT_CONST_ADD $0003, alloc_list_last, alloc_list_last

    rts

next:
    UINT_CONST_SUB $0003, pointer, pointer
    jmp loop

return_error:
    MEMORY_MOVE pointer, arg1
    jmp free_error
.endproc

; allocates (accumulator) size memory and returns pointer to it
; accumulator set to 1 if memory was not allocated
.proc alloc
neededSize   = kernel_temp1
size         = kernel_temp2
prevAdr      = kernel_temp3
pointer      = kernel_ptr1
shiftSrc     = kernel_ptr2
shiftDst     = kernel_ptr3

    sta neededSize
    ADDRESS_MOVE alloc_list-3, pointer
    ADDRESS_MOVE $0300, prevAdr

loop:
    ; if pointer = alloc_list_last, make entry
    UINT_CMP pointer, alloc_list_last
    cmp #1
    bne next
    UINT_CONST_SUB $0003, alloc_list_last, alloc_list_last

    ; if distance between last allocated address and last allocated list address is lesser than 3 bytes,
    ; prevent allocation
;     UINT_SUB alloc_list_last, alloc_last_adr, shiftSrc
;     UINT_CONST_CMP $0003, shiftSrc
;     cmp #2
;     bne :+
;     jmp return_error
;:
    jmp make_entry

    ; if addresses equal, add size to prevAdr and go to the next one
next:
    ldy #2
    lda prevAdr
    cmp (pointer),y
    bne :++
    dey
    lda prevAdr+1
    cmp (pointer),y
    bne :++

    dey
    lda (pointer),y
    clc
    adc prevAdr
    sta prevAdr
    bcc :+
    inc prevAdr+1
:   jmp next_loop

    ; else find address difference
:   ldy #2
    sec
    lda (pointer),y
    sbc prevAdr
    sta size
    dey
    lda (pointer),y
    sbc prevAdr+1
    sta size+1

    ; if it >= neededSize
    UINT_CMP size, neededSize
    cmp #0
    bne :++

    ; else copy address and add size to it
    ldy #2
    lda (pointer),y
    sta prevAdr
    dey
    lda (pointer),y
    sta prevAdr+1

    dey
    clc
    lda (pointer),y
    adc prevAdr
    sta prevAdr
    bcc :+
    inc prevAdr+1
:   jmp next_loop

    ; shift list left
:   MEMORY_MOVE alloc_list_last, shiftSrc
    MEMORY_MOVE alloc_list_last, shiftDst
    UINT_CONST_SUB $0003, shiftDst, shiftDst

shift_loop:
    ldy #2

shift_cpy_loop:
    lda (shiftSrc),y
    sta (shiftDst),y
    cpy #0
    beq :+
    dey
    jmp shift_cpy_loop

    ; if shiftSrc = pointer, break
:   UINT_CMP shiftSrc, pointer
    cmp #1
    beq shift_finish

    ; move shiftSrc to shiftDst, shiftSrc += 3
    MEMORY_MOVE shiftSrc, shiftDst
    UINT_CONST_ADD $0003, shiftSrc, shiftSrc
    jmp shift_loop

next_loop:
    UINT_CONST_SUB $0003, pointer, pointer
    jmp loop

shift_finish:
    UINT_CONST_SUB $0003, alloc_list_last, alloc_list_last
make_entry:
    ; make entry
    ldy #2
    lda prevAdr
    sta (pointer),y
    dey
    lda prevAdr+1
    sta (pointer),y
    lda (pointer),y
    dey
    lda neededSize
    sta (pointer),y

    ; update alloc_last_adr
    UINT_ADD prevAdr, neededSize, alloc_last_adr

return_success:
;     ADDRESS_MOVE dbgmsg_alloc, arg1
;     jsr putstr

    MEMORY_MOVE prevAdr, return
    lda #0
    rts

return_error:
    lda #1
    rts
.endproc

.proc kernel_main
    ; init. alloclist
    ADDRESS_MOVE alloc_list-3, alloc_list_last

    jmp main
.endproc
