!
! sparc assembler for:
! int asm_probe(int i)
! RETURNS: i*3
!
.section        ".text"
        .align 4
        .global asm_probe
asm_probe:
        sll     %o0, 1, %o1
        add     %o1, %o0, %o0
        retl
        nop
