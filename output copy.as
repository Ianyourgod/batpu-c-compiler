# main
    ldi r14 239
    ldi r15 239
    str r14 r15 0
    adi r14 -1
    mov r14 r15
    adi r14 -23
    str r14 r12 0
    str r14 r13 -1
    adi r14 -2
    ldi r4 0
    .tmp.6
    ldi r11 17
    cmp r4 r11
    brh LT .cL.true.2
    ldi r6 0
    jmp .cL.end.3
    .cL.true.2
    ldi r6 1
    .cL.end.3
    cmp r6 r0
    brh EQ .loop.1.break
    mov r15 r6
    sub r6 r4 r1
    ldi r10 2
    str r1 r10 0
    .loop.1.continue
    adi r4 1
    jmp .tmp.6
    .loop.1.break
    ldi r10 0
    adi r15 26
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -26
    ldi r10 0
    adi r15 27
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -27
    ldi r10 1
    adi r15 28
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -28
    .tmp.10
    adi r15 28
    adi r15 -1
    lod r15 r10 7
    adi r15 1
    adi r15 -28
    ldi r11 5
    cmp r10 r11
    brh EQ .cL.true.4
    brh LT .cL.true.4
    ldi r6 0
    jmp .cL.end.5
    .cL.true.4
    ldi r6 1
    .cL.end.5
    cmp r6 r0
    brh EQ .loop.2.break
    ldi r12 0
    ldi r13 17
    .tmp.12
    cmp r13 r0
    brh EQ .cL.false.6
    brh LT .cL.false.6
    jmp .cL.end.7
    .cL.false.6
    jmp .loop.3.break
    .cL.end.7
    mov r15 r6
    ldi r11 1
    sub r13 r11 r4
    sub r6 r4 r1
    lod r1 r6 0
    mov r6 r6
    lsh r6 r6
    lsh r6 r6
    lsh r6 r6
    mov r15 r7
    ldi r11 1
    sub r13 r11 r4
    sub r7 r4 r1
    lod r1 r4 0
    mov r4 r4
    lsh r4 r4
    add r6 r4 r11
    adi r15 29
    adi r15 -1
    str r15 r11 7
    adi r15 1
    adi r15 -29
    mov r12 r1
    mov r13 r2
    cal .__mult
    adi r15 29
    adi r15 -1
    lod r15 r10 7
    adi r15 1
    adi r15 -29
    add r10 r1 r12
    mov r15 r6
    ldi r11 1
    sub r13 r11 r4
    sub r6 r4 r11
    adi r15 30
    adi r15 -1
    str r15 r11 7
    adi r15 1
    adi r15 -30
    ldi r1 2
    mov r13 r2
    cal .__mult
    ldi r11 1
    sub r1 r11 r2
    mov r12 r1
    cal .__div
    adi r15 30
    adi r15 -1
    lod r15 r1 7
    adi r15 1
    adi r15 -30
    str r1 r2 0
    ldi r1 2
    mov r13 r2
    cal .__mult
    ldi r11 1
    sub r1 r11 r2
    mov r12 r1
    cal .__div
    mov r1 r12
    .loop.3.continue
    adi r13 -1
    jmp .tmp.12
    .loop.3.break
    mov r15 r6
    sub r6 r0 r13
    mov r12 r1
    ldi r2 10
    cal .__div
    mov r13 r1
    str r1 r2 0
    mov r12 r1
    ldi r2 10
    cal .__div
    mov r1 r12
    ldi r11 9
    cmp r12 r11
    brh EQ .cL.true.8
    ldi r6 0
    jmp .cL.end.9
    .cL.true.8
    ldi r6 1
    .cL.end.9
    cmp r6 r0
    brh EQ .tmp.40
    adi r15 26
    adi r15 -1
    lod r15 r10 7
    adi r15 1
    adi r15 -26
    adi r10 1
    adi r15 26
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -26
    jmp .tmp.42
    .tmp.40
    ldi r11 10
    cmp r12 r11
    brh EQ .cL.true.10
    ldi r6 0
    jmp .cL.end.11
    .cL.true.10
    ldi r6 1
    .cL.end.11
    cmp r6 r0
    brh EQ .tmp.43
    adi r15 27
    adi r15 -1
    lod r15 r10 7
    adi r15 1
    adi r15 -27
    ldi r11 1
    add r10 r11 r1
    adi r15 28
    adi r15 -1
    lod r15 r2 7
    adi r15 1
    adi r15 -28
    cal .print_num
    ldi r12 0
    .tmp.46
    adi r15 26
    adi r15 -1
    lod r15 r11 7
    adi r15 1
    adi r15 -26
    cmp r12 r11
    brh LT .cL.true.12
    ldi r6 0
    jmp .cL.end.13
    .cL.true.12
    ldi r6 1
    .cL.end.13
    cmp r6 r0
    brh EQ .loop.4.break
    ldi r1 0
    adi r15 28
    adi r15 -1
    lod r15 r2 7
    adi r15 1
    adi r15 -28
    cal .print_num
    .loop.4.continue
    adi r12 1
    jmp .tmp.46
    .loop.4.break
    ldi r10 0
    adi r15 27
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -27
    ldi r10 0
    adi r15 26
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -26
    jmp .tmp.48
    .tmp.43
    adi r15 27
    adi r15 -1
    lod r15 r1 7
    adi r15 1
    adi r15 -27
    adi r15 28
    adi r15 -1
    lod r15 r2 7
    adi r15 1
    adi r15 -28
    cal .print_num
    adi r15 27
    adi r15 -1
    str r15 r12 7
    adi r15 1
    adi r15 -27
    adi r15 26
    adi r15 -1
    lod r15 r10 7
    adi r15 1
    adi r15 -26
    cmp r10 r0
    brh EQ .cL.false.14
    brh LT .cL.false.14
    ldi r6 1
    jmp .cL.end.15
    .cL.false.14
    ldi r6 0
    .cL.end.15
    cmp r6 r0
    brh EQ .tmp.49
    .tmp.51
    adi r15 26
    adi r15 -1
    lod r15 r10 7
    adi r15 1
    adi r15 -26
    cmp r10 r0
    brh EQ .cL.false.16
    brh LT .cL.false.16
    jmp .loop.5.break
    .cL.false.16
    ldi r1 9
    adi r15 28
    adi r15 -1
    lod r15 r2 7
    adi r15 1
    adi r15 -28
    cal .print_num
    .loop.5.continue
    adi r15 26
    adi r15 -1
    lod r15 r10 7
    adi r15 1
    adi r15 -26
    adi r10 -1
    adi r15 26
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -26
    jmp .tmp.51
    .loop.5.break
    .tmp.49
    .tmp.48
    .tmp.42
    .loop.2.continue
    adi r15 27
    lod r15 r10 7
    adi r15 1
    adi r15 -28
    adi r10 1
    adi r15 28
    adi r15 -1
    str r15 r10 7
    adi r15 1
    adi r15 -28
    jmp .tmp.10
    .loop.2.break
    adi r15 26
    lod r15 r1 7
    ldi r2 250
    str r2 r1 0
    hlt
.__mult
    LDI r3 0
    ..MULT_LOOP
    ADI r2 -1
    BRH LT ..MULT_END
    ADD r1 r3 r3
    JMP ..MULT_LOOP
    ..MULT_END
    MOV r3 r1
    RET
.__div
    ldi r3 0
    .__div..loop1
    cmp r1 r2
    brh lt .__div..end
    sub r1 r2 r1
    inc r3
    jmp .__div..loop1
    .__div..end
    mov r1 r2
    mov r3 r1
ret
.print_num
    ldi r10 247
    str r10 r1 3
    dec r2
    dec r2
    str r10 r2 -6
    ..pn.continue
    cmp r1 r0
    brh eq ..pn.break
    dec r1
    str r10 r1 -7
    str r10 r0 -5
    str r10 r0 -2
    jmp ..pn.continue
    ..pn.break
    ret