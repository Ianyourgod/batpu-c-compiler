
ldi r14 239
ldi r15 239
cal .user_func_main
hlt
.user_func_mem_read
  lod r1 r1 0
  ret
.user_func_mem_write
  str r1 r2 0
  ret
.user_func_main
    ldi r1 4
    ret
