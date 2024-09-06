
ldi r14 239
ldi r15 239

ldi r2 250
str r2 r1 0
hlt
.mem_read
  lod r1 r1 0
  ret
.mem_write
  str r1 r2 0
  ret
:global
.mult
    str r14 r15 0
    adi r14 -1
    mov r14 r15
    adi r14 -5
    mov r1 r11
    str r15 r11 -1
    mov r2 r11
    str r15 r11 -2
    ldi r10 0
    mov r10 r11
    str r15 r11 -3
    .loop.0.continue
    lod r15 r10 -1
    ldi r11 0
    cmp r10 r11
    brh EQ .cL.false.0
    brh LT .cL.false.0
    ldi r10 1
    mov r10 r11
    str r15 r11 -4
    jmp .cL.end.1
    .cL.false.0
    ldi r10 0
    mov r10 r11
    str r15 r11 -4
    .cL.end.1
    lod r15 r10 -4
    ldi r11 0
    cmp r10 r11
    brh EQ .loop.0.break
    lod r15 r10 -3
    lod r15 r11 -2
    add r10 r11 r12
    str r15 r12 -5
    lod r15 r10 -5
    mov r10 r11
    str r15 r11 -3
    lod r15 r10 -1
    adi r10 -1
    str r15 r10 -1
    jmp .loop.0.continue
    .loop.0.break
    lod r15 r10 -3
    mov r10 r1
    mov r15 r14
    lod r14 r15 1
    adi r14 1
    ret
    ret
