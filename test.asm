mov r0, 3
cmp r0, 8
je true
jmp false
true:
jmp exit

// loop
false:
jmp false


exit: