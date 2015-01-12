;              Automatically Tuned Linear Algebra Software v3.10.2
;                       (C) Copyright 2001 Julian Ruhe
;
;  Redistribution and use in source and binary forms, with or without
;  modification, are permitted provided that the following conditions
;  are met:
;    1. Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;    2. Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions, and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;    3. The name of the ATLAS group or the names of its contributers may
;       not be used to endorse or promote products derived from this
;       software without specific written permission.
;
;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
;  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;  POSSIBILITY OF SUCH DAMAGE.
;
;
;	ATL_sJIK48x48x48TN48x48x0_a1_b1.asm
;
;	ATLAS "Speed of Light" DGEMM() kernel for AMD Athlon
;	Code author: Julian Ruhe (ruheejih@linux.zrz.tu-berlin.de | Julian.Ruhe@t-online.de)
;
;	void ATL_sJIK48x48x48TN48x48x0_a1_b1(const int M, const int N, const int K, const float alpha,
;						const float *A, const int lda, const float *B, const int ldb,
;						const float beta, float *C, const int ldc)
;
;	Compile with "nasmw -f win32 -DWIN32 ATL_sJIK48x48x48TN48x48x0_a1_b1.asm" (Windows)
;	Compile with "nasm -f elf -DELF ATL_sJIK48x48x48TN48x48x0_a1_b1.asm" (LINUX)
;
;	See config file (ATL_sJIK48x48x48TN48x48x0_a1.cfg) for important macro definitions
;


%include "ATL_sJIK48x48x48TN48x48x0_a1.cfg"
%include "ATL_sJIK48x48x48TN48x48x0_a1.mcr"

%ifdef WIN32
global _ATL_sJIK48x48x48TN48x48x0_a1_b1

section .text

_ATL_sJIK48x48x48TN48x48x0_a1_b1:
%endif

%ifdef ELF
global ATL_sJIK48x48x48TN48x48x0_a1_b1

section .text

ATL_sJIK48x48x48TN48x48x0_a1_b1:
%endif

	push ebp
	mov ebp,esp

	push ebx
	push esi
	push edi

	femms

	mov eax,0				;temporary variable t1
	push eax				;t1->stack
	mov eax,[ebp+24]			;&A->eax
	add eax,NB*NB*4				;&(A+1)->eax
	mov ebx,[ebp+32]			;&B->ebx
	sub eax,ebx				;calculate offset
	push eax				;&A+1+offset->stack

	mov eax,[ebp+48]			;ldc->eax
	lea eax,[4*eax]
	push eax				;8*ldc->stack
	mov eax,NB
	push eax				;loop counter->stack

	mov eax,[ebp+24]			;&A->eax
	mov ebx,[ebp+32]			;&B->ebx
	mov ecx,[ebp+44]			;&C->ecx

	add ecx,byte 30*4			;calculate offsets
	add ebx,byte 30*4
	add eax,5*NB*4
	push eax				;&A+offset->stack
	push ebp				;ebp->stack

	mov edi,-1*NB*4				;calculate offsets for dot products
	mov esi,-3*NB*4
	mov ebp,-5*NB*4

	mov edx,6*NB*4-30*4			;offset for the next 6 dot products


						;stack dump
						;
						;[esp+20]:	t1 (temp)
						;[esp+16]:	&(A+1)+offset
						;[esp+12]:	ldc*4
						;[esp+08]:	loop counter
						;[esp+04]:	&A+offset
						;[esp+00]:	ebp

	align 16
loopj_
	fld dword [ebx+ELM1]			;01+1
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM2]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM3]
	fld dword [eax+DOTP1]
	fmul st0,st3

	fadd dword [ecx+ELM1]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM5]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM6]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM4]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+1
	OPERATION 3,4				;03+1
	OPERATION 4,5				;04+1
	OPERATION 5,6				;05+1
	OPERATION 6,7				;06+1
	OPERATION 7,8				;07+1
	OPERATION 8,9				;08+1
	OPERATION 9,10				;09+1
	OPERATION 10,11				;10+1
	OPERATION 11,12				;11+1
	OPERATION 12,13				;12+1
	OPERATION 13,14				;13+1
	OPERATION 14,15				;14+1
	OPERATION 15,16				;15+1
	OPERATION 16,17				;16+1
	OPERATION 17,18				;17+1
	OPERATION 18,19				;18+1
	OPERATION 19,20				;19+1
	OPERATION 20,21				;20+1
	OPERATION 21,22				;21+1
	OPERATION 22,23				;22+1
	OPERATION 23,24				;23+1
	OPERATION 24,25				;24+1
	OPERATION 25,26				;25+1
	OPERATION 26,27				;26+1
	OPERATION 27,28				;27+1
	OPERATION 28,29				;28+1
	OPERATION 29,30				;29+1
	OPERATION 30,31				;30+1
	OPERATION 31,32				;31+1
	OPERATION 32,33				;32+1
	OPERATION 33,34				;33+1
	OPERATION 34,35				;34+1
	OPERATION 35,36				;35+1
	OPERATION 36,37				;36+1
	OPERATION 37,38				;37+1
	OPERATION 38,39				;38+1
	OPERATION 39,40				;39+1
	OPERATION 40,41				;40+1
	OPERATION 41,42				;41+1
	OPERATION 42,43				;42+1
	OPERATION 43,44				;43+1
	OPERATION 44,45				;44+1
	OPERATION 45,46				;45+1
	OPERATION 46,47				;45+1
	OPERATION 47,48				;45+1
	fld dword [eax+DOTP1+ELM48]		;48+1
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREC_DST7
		mov [esp+20],ecx
		add ecx,[esp+12]

		prefetchw [ecx-2*64]
		prefetchw [ecx-1*64]

		prefetchw [ecx+0*64]
		prefetchw [ecx+1*64]
		nop

		prefetchw [ecx+2*64-1]
		mov ecx,[esp+20]
	%endif



	%ifdef PREB_DST7
		prefetch [ebx+48*4-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+48*4-1*64]
		nop

		prefetch [ebx+48*4+0*64]
		nop

		prefetch [ebx+48*4+1*64]
		nop

		prefetch [ebx+48*4+2*64]
		nop
	%endif



	fstp dword [ecx+ELM1]
	fxch st3
	fstp dword [ecx+ELM2]

	fxch st1
	fstp dword [ecx+ELM3]
	fstp dword [ecx+ELM4]

	fstp dword [ecx+ELM5]
	fstp dword [ecx+ELM6]
	add eax,edx



	fld dword [ebx+ELM1]			;01+2
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM8]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM9]
	fld dword [eax+DOTP1]
	fmul st0,st3

	fadd dword [ecx+ELM7]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM11]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM12]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM10]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+2
	OPERATION 3,4				;03+2
	OPERATION 4,5				;04+2
	OPERATION 5,6				;05+2
	OPERATION 6,7				;06+2
	OPERATION 7,8				;07+2
	OPERATION 8,9				;08+2
	OPERATION 9,10				;09+2
	OPERATION 10,11				;10+2
	OPERATION 11,12				;11+2
	OPERATION 12,13				;12+2
	OPERATION 13,14				;13+2
	OPERATION 14,15				;14+2
	OPERATION 15,16				;15+2
	OPERATION 16,17				;16+2
	OPERATION 17,18				;17+2
	OPERATION 18,19				;18+2
	OPERATION 19,20				;19+2
	OPERATION 20,21				;20+2
	OPERATION 21,22				;21+2
	OPERATION 22,23				;22+2
	OPERATION 23,24				;23+2
	OPERATION 24,25				;24+2
	OPERATION 25,26				;25+2
	OPERATION 26,27				;26+2
	OPERATION 27,28				;27+2
	OPERATION 28,29				;28+2
	OPERATION 29,30				;29+2
	OPERATION 30,31				;30+2
	OPERATION 31,32				;31+2
	OPERATION 32,33				;32+2
	OPERATION 33,34				;33+2
	OPERATION 34,35				;34+2
	OPERATION 35,36				;35+2
	OPERATION 36,37				;36+2
	OPERATION 37,38				;37+2
	OPERATION 38,39				;38+2
	OPERATION 39,40				;39+2
	OPERATION 40,41				;40+2
	OPERATION 41,42				;41+2
	OPERATION 42,43				;42+2
	OPERATION 43,44				;43+2
	OPERATION 44,45				;44+2
	OPERATION 45,46				;45+2
	OPERATION 46,47				;45+2
	OPERATION 47,48				;45+2
	fld dword [eax+DOTP1+ELM48]		;48+2
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREC_DST6
		mov [esp+20],ecx
		add ecx,[esp+12]

		prefetchw [ecx-2*64]
		prefetchw [ecx-1*64]

		prefetchw [ecx+0*64]
		prefetchw [ecx+1*64]
		nop

		prefetchw [ecx+2*64-1]
		mov ecx,[esp+20]
	%endif



	%ifdef PREB_DST6
		prefetch [ebx+48*4-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+48*4-1*64]
		nop

		prefetch [ebx+48*4+0*64]
		nop

		prefetch [ebx+48*4+1*64]
		nop

		prefetch [ebx+48*4+2*64]
		nop
	%endif



	fstp dword [ecx+ELM7]
	fxch st3
	fstp dword [ecx+ELM8]

	fxch st1
	fstp dword [ecx+ELM9]
	fstp dword [ecx+ELM10]

	fstp dword [ecx+ELM11]
	fstp dword [ecx+ELM12]
	add eax,edx



	fld dword [ebx+ELM1]			;01+3
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM14]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM15]
	fld dword [eax+DOTP1]
	fmul st0,st3

	fadd dword [ecx+ELM13]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM17]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM18]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM16]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+3
	OPERATION 3,4				;03+3
	OPERATION 4,5				;04+3
	OPERATION 5,6				;05+3
	OPERATION 6,7				;06+3
	OPERATION 7,8				;07+3
	OPERATION 8,9				;08+3
	OPERATION 9,10				;09+3
	OPERATION 10,11				;10+3
	OPERATION 11,12				;11+3
	OPERATION 12,13				;12+3
	OPERATION 13,14				;13+3
	OPERATION 14,15				;14+3
	OPERATION 15,16				;15+3
	OPERATION 16,17				;16+3
	OPERATION 17,18				;17+3
	OPERATION 18,19				;18+3
	OPERATION 19,20				;19+3
	OPERATION 20,21				;20+3
	OPERATION 21,22				;21+3
	OPERATION 22,23				;22+3
	OPERATION 23,24				;23+3
	OPERATION 24,25				;24+3
	OPERATION 25,26				;25+3
	OPERATION 26,27				;26+3
	OPERATION 27,28				;27+3
	OPERATION 28,29				;28+3
	OPERATION 29,30				;29+3
	OPERATION 30,31				;30+3
	OPERATION 31,32				;31+3
	OPERATION 32,33				;32+3
	OPERATION 33,34				;33+3
	OPERATION 34,35				;34+3
	OPERATION 35,36				;35+3
	OPERATION 36,37				;36+3
	OPERATION 37,38				;37+3
	OPERATION 38,39				;38+3
	OPERATION 39,40				;39+3
	OPERATION 40,41				;40+3
	OPERATION 41,42				;41+3
	OPERATION 42,43				;42+3
	OPERATION 43,44				;43+3
	OPERATION 44,45				;44+3
	OPERATION 45,46				;45+3
	OPERATION 46,47				;45+3
	OPERATION 47,48				;45+3
	fld dword [eax+DOTP1+ELM48]		;48+3
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREC_DST5
		mov [esp+20],ecx
		add ecx,[esp+12]

		prefetchw [ecx-2*64]
		prefetchw [ecx-1*64]

		prefetchw [ecx+0*64]
		prefetchw [ecx+1*64]
		nop

		prefetchw [ecx+2*64-1]
		mov ecx,[esp+20]
	%endif



	%ifdef PREB_DST5
		prefetch [ebx+48*4-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+48*4-1*64]
		nop

		prefetch [ebx+48*4+0*64]
		nop

		prefetch [ebx+48*4+1*64]
		nop

		prefetch [ebx+48*4+2*64]
		nop
	%endif



	fstp dword [ecx+ELM13]
	fxch st3
	fstp dword [ecx+ELM14]

	fxch st1
	fstp dword [ecx+ELM15]
	fstp dword [ecx+ELM16]

	fstp dword [ecx+ELM17]
	fstp dword [ecx+ELM18]
	add eax,edx



	fld dword [ebx+ELM1]			;01+4
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM20]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM21]
	fld dword [eax+DOTP1]
	fmul st0,st3

	fadd dword [ecx+ELM19]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM23]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM24]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM22]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+4
	OPERATION 3,4				;03+4
	OPERATION 4,5				;04+4
	OPERATION 5,6				;05+4
	OPERATION 6,7				;06+4
	OPERATION 7,8				;07+4
	OPERATION 8,9				;08+4
	OPERATION 9,10				;09+4
	OPERATION 10,11				;10+4
	OPERATION 11,12				;11+4
	OPERATION 12,13				;12+4
	OPERATION 13,14				;13+4
	OPERATION 14,15				;14+4
	OPERATION 15,16				;15+4
	OPERATION 16,17				;16+4
	OPERATION 17,18				;17+4
	OPERATION 18,19				;18+4
	OPERATION 19,20				;19+4
	OPERATION 20,21				;20+4
	OPERATION 21,22				;21+4
	OPERATION 22,23				;22+4
	OPERATION 23,24				;23+4
	OPERATION 24,25				;24+4
	OPERATION 25,26				;25+4
	OPERATION 26,27				;26+4
	OPERATION 27,28				;27+4
	OPERATION 28,29				;28+4
	OPERATION 29,30				;29+4
	OPERATION 30,31				;30+4
	OPERATION 31,32				;31+4
	OPERATION 32,33				;32+4
	OPERATION 33,34				;33+4
	OPERATION 34,35				;34+4
	OPERATION 35,36				;35+4
	OPERATION 36,37				;36+4
	OPERATION 37,38				;37+4
	OPERATION 38,39				;38+4
	OPERATION 39,40				;39+4
	OPERATION 40,41				;40+4
	OPERATION 41,42				;41+4
	OPERATION 42,43				;42+4
	OPERATION 43,44				;43+4
	OPERATION 44,45				;44+4
	OPERATION 45,46				;45+4
	OPERATION 46,47				;45+4
	OPERATION 47,48				;45+4
	fld dword [eax+DOTP1+ELM48]		;48+4
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREC_DST4
		mov [esp+20],ecx
		add ecx,[esp+12]

		prefetchw [ecx-2*64]
		prefetchw [ecx-1*64]

		prefetchw [ecx+0*64]
		prefetchw [ecx+1*64]
		nop

		prefetchw [ecx+2*64-1]
		mov ecx,[esp+20]
	%endif



	%ifdef PREB_DST4
		prefetch [ebx+48*4-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+48*4-1*64]
		nop

		prefetch [ebx+48*4+0*64]
		nop

		prefetch [ebx+48*4+1*64]
		nop

		prefetch [ebx+48*4+2*64]
		nop
	%endif



	fstp dword [ecx+ELM19]
	fxch st3
	fstp dword [ecx+ELM20]

	fxch st1
	fstp dword [ecx+ELM21]
	fstp dword [ecx+ELM22]

	fstp dword [ecx+ELM23]
	fstp dword [ecx+ELM24]
	add eax,edx



	fld dword [ebx+ELM1]			;01+5
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM26]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM27]
	fld dword [eax+DOTP1]
	fmul st0,st3

	fadd dword [ecx+ELM25]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM29]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM30]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM28]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+5
	OPERATION 3,4				;03+5
	OPERATION 4,5				;04+5
	OPERATION 5,6				;05+5
	OPERATION 6,7				;06+5
	OPERATION 7,8				;07+5
	OPERATION 8,9				;08+5
	OPERATION 9,10				;09+5
	OPERATION 10,11				;10+5
	OPERATION 11,12				;11+5
	OPERATION 12,13				;12+5
	OPERATION 13,14				;13+5
	OPERATION 14,15				;14+5
	OPERATION 15,16				;15+5
	OPERATION 16,17				;16+5
	OPERATION 17,18				;17+5
	OPERATION 18,19				;18+5
	OPERATION 19,20				;19+5
	OPERATION 20,21				;20+5
	OPERATION 21,22				;21+5
	OPERATION 22,23				;22+5
	OPERATION 23,24				;23+5
	OPERATION 24,25				;24+5
	OPERATION 25,26				;25+5
	OPERATION 26,27				;26+5
	OPERATION 27,28				;27+5
	OPERATION 28,29				;28+5
	OPERATION 29,30				;29+5
	OPERATION 30,31				;30+5
	OPERATION 31,32				;31+5
	OPERATION 32,33				;32+5
	OPERATION 33,34				;33+5
	OPERATION 34,35				;34+5
	OPERATION 35,36				;35+5
	OPERATION 36,37				;36+5
	OPERATION 37,38				;37+5
	OPERATION 38,39				;38+5
	OPERATION 39,40				;39+5
	OPERATION 40,41				;40+5
	OPERATION 41,42				;41+5
	OPERATION 42,43				;42+5
	OPERATION 43,44				;43+5
	OPERATION 44,45				;44+5
	OPERATION 45,46				;45+5
	OPERATION 46,47				;45+5
	OPERATION 47,48				;45+5
	fld dword [eax+DOTP1+ELM48]		;48+5
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREC_DST3
		mov [esp+20],ecx
		add ecx,[esp+12]

		prefetchw [ecx-2*64]
		prefetchw [ecx-1*64]

		prefetchw [ecx+0*64]
		prefetchw [ecx+1*64]
		nop

		prefetchw [ecx+2*64-1]
		mov ecx,[esp+20]
	%endif



	%ifdef PREB_DST3
		prefetch [ebx+48*4-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+48*4-1*64]
		nop

		prefetch [ebx+48*4+0*64]
		nop

		prefetch [ebx+48*4+1*64]
		nop

		prefetch [ebx+48*4+2*64]
		nop
	%endif



	fstp dword [ecx+ELM25]
	fxch st3
	fstp dword [ecx+ELM26]

	fxch st1
	fstp dword [ecx+ELM27]
	fstp dword [ecx+ELM28]

	fstp dword [ecx+ELM29]
	fstp dword [ecx+ELM30]
	add eax,edx



	fld dword [ebx+ELM1]			;01+6
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM32]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM33]
	fld dword [eax+DOTP1]
	fmul st0,st3

	rep
	fadd dword [ecx+ELM31]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM35]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM36]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM34]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+6
	OPERATION 3,4				;03+6
	OPERATION 4,5				;04+6
	OPERATION 5,6				;05+6
	OPERATION 6,7				;06+6
	OPERATION 7,8				;07+6
	OPERATION 8,9				;08+6
	OPERATION 9,10				;09+6
	OPERATION 10,11				;10+6
	OPERATION 11,12				;11+6
	OPERATION 12,13				;12+6
	OPERATION 13,14				;13+6
	OPERATION 14,15				;14+6
	OPERATION 15,16				;15+6
	OPERATION 16,17				;16+6
	OPERATION 17,18				;17+6
	OPERATION 18,19				;18+6
	OPERATION 19,20				;19+6
	OPERATION 20,21				;20+6
	OPERATION 21,22				;21+6
	OPERATION 22,23				;22+6
	OPERATION 23,24				;23+6
	OPERATION 24,25				;24+6
	OPERATION 25,26				;25+6
	OPERATION 26,27				;26+6
	OPERATION 27,28				;27+6
	OPERATION 28,29				;28+6
	OPERATION 29,30				;29+6
	OPERATION 30,31				;30+6
	OPERATION 31,32				;31+6
	OPERATION 32,33				;32+6
	OPERATION 33,34				;33+6
	OPERATION 34,35				;34+6
	OPERATION 35,36				;35+6
	OPERATION 36,37				;36+6
	OPERATION 37,38				;37+6
	OPERATION 38,39				;38+6
	OPERATION 39,40				;39+6
	OPERATION 40,41				;40+6
	OPERATION 41,42				;41+6
	OPERATION 42,43				;42+6
	OPERATION 43,44				;43+6
	OPERATION 44,45				;44+6
	OPERATION 45,46				;45+6
	OPERATION 46,47				;45+6
	OPERATION 47,48				;45+6
	fld dword [eax+DOTP1+ELM48]		;48+6
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREC_DST2
		mov [esp+20],ecx
		add ecx,[esp+12]

		prefetchw [ecx-2*64]
		prefetchw [ecx-1*64]

		prefetchw [ecx+0*64]
		prefetchw [ecx+1*64]
		nop

		prefetchw [ecx+2*64-1]
		mov ecx,[esp+20]
	%endif



	%ifdef PREB_DST2
		prefetch [ebx+48*4-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+48*4-1*64]
		nop

		prefetch [ebx+48*4+0*64]
		nop

		prefetch [ebx+48*4+1*64]
		nop

		prefetch [ebx+48*4+2*64]
		nop
	%endif



	rep
	fstp dword [ecx+ELM31]
	fxch st3
	fstp dword [ecx+ELM32]

	fxch st1
	fstp dword [ecx+ELM33]
	fstp dword [ecx+ELM34]

	fstp dword [ecx+ELM35]
	fstp dword [ecx+ELM36]
	add eax,edx



	fld dword [ebx+ELM1]			;01+7
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM38]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM39]
	fld dword [eax+DOTP1]
	fmul st0,st3

	fadd dword [ecx+ELM37]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM41]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM42]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM40]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+7
	OPERATION 3,4				;03+7
	OPERATION 4,5				;04+7
	OPERATION 5,6				;05+7
	OPERATION 6,7				;06+7
	OPERATION 7,8				;07+7
	OPERATION 8,9				;08+7
	OPERATION 9,10				;09+7
	OPERATION 10,11				;10+7
	OPERATION 11,12				;11+7
	OPERATION 12,13				;12+7
	OPERATION 13,14				;13+7
	OPERATION 14,15				;14+7
	OPERATION 15,16				;15+7
	OPERATION 16,17				;16+7
	OPERATION 17,18				;17+7
	OPERATION 18,19				;18+7
	OPERATION 19,20				;19+7
	OPERATION 20,21				;20+7
	OPERATION 21,22				;21+7
	OPERATION 22,23				;22+7
	OPERATION 23,24				;23+7
	OPERATION 24,25				;24+7
	OPERATION 25,26				;25+7
	OPERATION 26,27				;26+7
	OPERATION 27,28				;27+7
	OPERATION 28,29				;28+7
	OPERATION 29,30				;29+7
	OPERATION 30,31				;30+7
	OPERATION 31,32				;31+7
	OPERATION 32,33				;32+7
	OPERATION 33,34				;33+7
	OPERATION 34,35				;34+7
	OPERATION 35,36				;35+7
	OPERATION 36,37				;36+7
	OPERATION 37,38				;37+7
	OPERATION 38,39				;38+7
	OPERATION 39,40				;39+7
	OPERATION 40,41				;40+7
	OPERATION 41,42				;41+7
	OPERATION 42,43				;42+7
	OPERATION 43,44				;43+7
	OPERATION 44,45				;44+7
	OPERATION 45,46				;45+7
	OPERATION 46,47				;45+7
	OPERATION 47,48				;45+7
	fld dword [eax+DOTP1+ELM48]		;48+7
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREC_DST1
		mov [esp+20],ecx
		add ecx,[esp+12]

		prefetchw [ecx-2*64]
		prefetchw [ecx-1*64]

		prefetchw [ecx+0*64]
		prefetchw [ecx+1*64]
		nop

		prefetchw [ecx+2*64-1]
		mov ecx,[esp+20]
	%endif



	%ifdef PREB_DST1
		prefetch [ebx+48*4-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+48*4-1*64]
		nop

		prefetch [ebx+48*4+0*64]
		nop

		prefetch [ebx+48*4+1*64]
		nop

		prefetch [ebx+48*4+2*64]
		nop
	%endif



	fstp dword [ecx+ELM37]
	fxch st3
	fstp dword [ecx+ELM38]

	fxch st1
	fstp dword [ecx+ELM39]
	fstp dword [ecx+ELM40]

	fstp dword [ecx+ELM41]
	fstp dword [ecx+ELM42]
	add eax,edx



	fld dword [ebx+ELM1]			;01+8
	fld dword [eax+DOTP2]
	fmul st0,st1

	fadd dword [ecx+ELM44]
	fld dword [eax+DOTP3]
	fmul st0,st2

	fadd dword [ecx+ELM45]
	fld dword [eax+DOTP1]
	fmul st0,st3

	fadd dword [ecx+ELM43]
	fxch st0,st3
	fld dword [eax+DOTP5]

	rep
	fmul st0,st1
	fadd dword [ecx+ELM47]
	fld dword [eax+DOTP6]

	fmul st0,st2
	fadd dword [ecx+ELM48]
	fld dword [ebx+ELM2]

	fld dword [eax+DOTP4]
	fmulp st4,st0
	fld dword [ecx+ELM46]

	rep
	faddp st4,st0
	add eax,byte 30*4
	mov edx,edx



	OPERATION 2,3				;02+8
	OPERATION 3,4				;03+8
	OPERATION 4,5				;04+8
	OPERATION 5,6				;05+8
	OPERATION 6,7				;06+8
	OPERATION 7,8				;07+8
	OPERATION 8,9				;08+8
	OPERATION 9,10				;09+8
	OPERATION 10,11				;10+8
	OPERATION 11,12				;11+8
	OPERATION 12,13				;12+8
	OPERATION 13,14				;13+8
	OPERATION 14,15				;14+8
	OPERATION 15,16				;15+8
	OPERATION 16,17				;16+8
	OPERATION 17,18				;17+8
	OPERATION 18,19				;18+8
	OPERATION 19,20				;19+8
	OPERATION 20,21				;20+8
	OPERATION 21,22				;21+8
	OPERATION 22,23				;22+8
	OPERATION 23,24				;23+8
	OPERATION 24,25				;24+8
	OPERATION 25,26				;25+8
	OPERATION 26,27				;26+8
	OPERATION 27,28				;27+8
	OPERATION 28,29				;28+8
	OPERATION 29,30				;29+8
	OPERATION 30,31				;30+8
	OPERATION 31,32				;31+8
	OPERATION 32,33				;32+8
	OPERATION 33,34				;33+8
	OPERATION 34,35				;34+8
	OPERATION 35,36				;35+8
	OPERATION 36,37				;36+8
	OPERATION 37,38				;37+8
	OPERATION 38,39				;38+8
	OPERATION 39,40				;39+8
	OPERATION 40,41				;40+8
	OPERATION 41,42				;41+8
	OPERATION 42,43				;42+8
	OPERATION 43,44				;43+8
	OPERATION 44,45				;44+8
	OPERATION 45,46				;45+8
	OPERATION 46,47				;45+8
	OPERATION 47,48				;45+8
	fld dword [eax+DOTP1+ELM48]		;48+8
	fmul st0,st1
	faddp st7

	fld dword [eax+DOTP2+ELM48]
	fmul st0,st1
	faddp st6

	fld dword [eax+DOTP3+ELM48]
	fmul st0,st1
	faddp st5

	fld dword [eax+DOTP4+ELM48]
	fmul st0,st1
	faddp st4

	fld dword [eax+DOTP5+ELM48]
	fmul st0,st1
	faddp st3

	rep
	fmul dword [eax+DOTP6+ELM48]
	faddp st1
	fxch st5



	%ifdef PREA_EN
		mov [esp+20],edx		;save edx in t1
		mov edx,[esp+16]		;&A+1->edx

		lea edx,[edx+ebx]
		prefetch [edx-2*64]
		nop

		prefetch [edx-1*64]
		prefetch [edx+0*64]
		nop

		prefetch [edx+1*64]
		prefetch [edx+2*64-8]

		mov edx,[esp+20]		;restore edx
		mov eax,eax
		fnop
	%endif



	fstp dword [ecx+ELM43]
	fxch st3
	fstp dword [ecx+ELM44]

	fxch st1
	fstp dword [ecx+ELM45]
	fstp dword [ecx+ELM46]

	fstp dword [ecx+ELM47]
	fstp dword [ecx+ELM48]
	sub ebx,edi				;next column of B


	mov eax,[esp+4]				;reset eax
	add ecx,[esp+12]			;next column of C (+ldc*4)

	dec dword [esp+8]			;dec counter
	jnz near loopj_


end_
	femms

	pop ebp
	add esp,byte 5*4			;remove local variables

	pop edi					;restore registers
	pop esi
	pop ebx

	leave                			;mov esp,ebp / pop ebp
	ret
