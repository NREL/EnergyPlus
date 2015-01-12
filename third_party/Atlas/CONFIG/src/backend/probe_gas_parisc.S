        .LEVEL 2.0
#ifdef ATL_OS_HPUX
        .SPACE $TEXT$
        .SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
#else
        .text
#endif
; PA-RISC assembler for:
; int asm_probe(int i)
; RETURNS: i*3
        .align 4
#ifdef ATL_OS_HPUX
        .SUBSPA $CODE$
        .EXPORT asm_probe,ENTRY,PRIV_LEV=3
asm_probe
#else
        .globl asm_probe
asm_probe:
#endif
        .PROC
        .CALLINFO FRAME=0,NO_CALLS
        .ENTRY
        depw,z  %r26, 29,30, %r28  ; r28 = 4 * i
        sub     %r28, %r26, %r28
        bve     (%r2)
        nop
        .EXIT
        .PROCEND
