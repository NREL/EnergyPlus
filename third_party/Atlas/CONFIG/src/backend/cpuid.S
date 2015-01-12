#include "atlas_asm.h"
        .text
        .globl ATL_asmdecor(do_cpuid)
ATL_asmdecor(do_cpuid):
#
#                rdi        rsi
# void cpuid(int *out, int what);
# NOTE: cpuid overwrites eax, ebx, ecx, and edx, takes eax as input
#       -- save ebx
#
#ifdef ATL_GAS_x8664
        movq    %rbx, -8(%rsp)
#
#       Load input reg, and call cpuid
#
        movl    %esi, %eax
        cpuid
#
#       Load address of output array, and fill in its entries
#
        movl    %eax, (%rdi)
        movl    %ebx, 4(%rdi)
        movl    %ecx, 8(%rdi)
        movl    %edx, 12(%rdi)

        movq    -8(%rsp), %rbx
        ret
 #else
#
#       Prologue
#
        subl    $8, %esp
        movl    %ebx, (%esp)
        movl    %edi, 4(%esp)
#
#       Load input reg, and call cpuid
#
        movl    16(%esp), %eax
        cpuid
#
#       Load address of output array, and fill in its entries
#
        movl    12(%esp), %edi
        movl    %eax, (%edi)
        movl    %ebx, 4(%edi)
        movl    %ecx, 8(%edi)
        movl    %edx, 12(%edi)
#
#       Epilogue
#
        movl    (%esp), %ebx
        movl    4(%esp), %edi
        addl    $8, %esp
        ret
#endif
