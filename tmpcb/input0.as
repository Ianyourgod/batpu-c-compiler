
ldi r14 239
ldi r15 239
cal .main:global
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
.main
    str r14 r15 0
    adi r14 -1
    mov r14 r15
    adi r14 -1
    ldi r10 3
    mov r10 r1
    ldi r10 4
    mov r10 r2
    cal .mult:global
    mov r1 r11
    str r15 r11 -1
    lod r15 r10 -1
    mov r10 r1
    mov r15 r14
    lod r14 r15 1
    adi r14 1
    ret
    ret
