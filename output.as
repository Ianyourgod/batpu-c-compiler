
ldi r14 239
ldi r15 239
cal .user_func_main
ldi r2 250
str r2 r1 0
hlt
.user_func_mem_read
  lod r1 r1 0
  ret
.user_func_mem_write
  str r1 r2 0
  ret
.user_func_main
    str r14 r15 0
    adi r14 -1
    mov r14 r15
    adi r14 -7
    ldi r10 3
    mov r10 r11
    str r15 r11 -1
    lod r15 r10 -1
    ldi r11 3
    sub r10 r11 r12
    str r15 r12 -2
    lod r15 r10 -2
    mov r10 r11
    str r15 r11 -3
    lod r15 r10 -1
    lod r15 r11 -3
    add r10 r11 r12
    str r15 r12 -4
    lod r15 r10 -4
    ldi r11 5
    sub r10 r11 r12
    str r15 r12 -5
    lod r15 r10 -5
    mov r10 r11
    str r15 r11 -6
    lod r15 r10 -1
    lod r15 r11 -6
    cmp r10 r11
    brh LT .cL.true.0
    ldi r10 0
    mov r10 r11
    str r15 r11 -7
    jmp .cL.end.1
    .cL.true.0
    ldi r10 1
    mov r10 r11
    str r15 r11 -7
    .cL.end.1
    lod r15 r10 -7
    mov r10 r1
    mov r15 r14
    lod r14 r15 0
    adi r14 1
    ret
