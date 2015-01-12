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
;	ATL_dJIK30x30x30TN30x30x0_a1_bX.asm
;
;	ATLAS "Speed of Light" DGEMM() kernel for AMD Athlon
;	Code author: Julian Ruhe (ruheejih@linux.zrz.tu-berlin.de | Julian.Ruhe@t-online.de)
;
;	void ATL_dJIK30x30x30TN30x30x0_a1_bX(const int M, const int N, const int K, const double alpha,
;						const double *A, const int lda, const double *B, const int ldb,
;						const double beta, double *C, const int ldc)
;
;	Compile with "nasmw -f win32 -DWIN32 ATL_dJIK30x30x30TN30x30x0_a1_bX.asm" (Windows)
;	Compile with "nasm -f elf -DELF ATL_dJIK30x30x30TN30x30x0_a1_bX.asm" (LINUX)
;
;	See config file (ATL_dJIK30x30x30TN30x30x0_a1.cfg) for important macro definitions
;


%include "ATL_dJIK30x30x30TN30x30x0_a1.cfg"
%include "ATL_dJIK30x30x30TN30x30x0_a1.mcr"

%ifdef WIN32
global _ATL_dJIK30x30x30TN30x30x0_a1_bX

section .text

_ATL_dJIK30x30x30TN30x30x0_a1_bX:
%endif

%ifdef ELF
global ATL_dJIK30x30x30TN30x30x0_a1_bX

section .text

ATL_dJIK30x30x30TN30x30x0_a1_bX:
%endif

	push ebp
	mov ebp,esp

	and esp,-8				;align8 stack
	sub esp,8

	fld qword [ebp+44]			;beta->st0
	fstp qword [esp]			;st0->stack

	push ebx
	push esi
	push edi

	femms

	mov eax,0				;temporary variable t1
	push eax				;t1->stack
	mov eax,[ebp+28]			;&A->eax
	add eax,NB*NB*8				;&(A+1)->eax
	mov ebx,[ebp+36]			;&B->ebx
	sub eax,ebx				;calculate offset
	push eax				;&A+1+offset->stack

	mov eax,[ebp+56]			;ldc->eax
	lea eax,[8*eax]
	push eax				;8*ldc->stack
	mov eax,NB
	push eax				;loop counter->stack

	mov eax,[ebp+28]			;&A->eax
	mov ebx,[ebp+36]			;&B->ebx
	mov ecx,[ebp+52]			;&C->ecx

	add ecx,byte 15*8			;calculate offsets
	add ebx,byte 15*8
	add eax,5*NB*8+15*8
	push eax				;&A+offset->stack
	push ebp				;ebp->stack

	mov edi,-1*NB*8				;calculate offsets for dot products
	mov esi,-3*NB*8
	mov ebp,-5*NB*8

	mov edx,6*NB*8				;offset for the next 6 dot products


						;stack dump

						;[esp+36]:	beta
						;[esp+32]:	ebx
						;[esp+28]:	esi
						;[esp+24]:	edi
						;[esp+20]:	t1 (temp)
						;[esp+16]:	&(A+1)+offset
						;[esp+12]:	ldc*8
						;[esp+08]:	loop counter
						;[esp+04]:	&A+offset
						;[esp+00]:	ebp

	align 16
loopj_
	fld qword [esp+36]
	fld qword [ecx+ELM2]
	nop

	rep
	fmul st0,st1
	fld qword [ecx+ELM3]
	fmul st0,st2

	rep
	fld qword [ecx+ELM1]
	fmul st0,st3
	fxch st0,st3

	fld qword [ecx+ELM5]
	fmul st0,st1
	fld qword [ecx+ELM6]

	fmul st0,st2
	fld qword [ebx+ELM1]
	fld qword [ecx+ELM4]

	rep
	fmulp st4,st0
	rep
	mov edx,edx
	fnop


	OPERATION 1,2				;01+1
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
	fld qword [eax+DOTP1+ELM30]		;30+1
	fmul st0,st1
	faddp st7

	fld qword [eax+DOTP2+ELM30]
	fmul st0,st1
	faddp st6

	fld qword [eax+DOTP3+ELM30]
	fmul st0,st1
	faddp st5

	fld qword [eax+DOTP4+ELM30]
	fmul st0,st1
	faddp st4

	fld qword [eax+DOTP5+ELM30]
	fmul st0,st1
	faddp st3

	rep
	fmul qword [eax+DOTP6+ELM30]
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
		prefetch [ebx+30*8-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+30*8-1*64]
		nop

		prefetch [ebx+30*8+0*64]
		nop

		prefetch [ebx+30*8+1*64]
		nop

		prefetch [ebx+30*8+2*64]
		nop
	%endif



	fstp qword [ecx+ELM1]
	fxch st3
	fstp qword [ecx+ELM2]

	fxch st1
	fstp qword [ecx+ELM3]
	fstp qword [ecx+ELM4]

	fstp qword [ecx+ELM5]
	fstp qword [ecx+ELM6]
	add eax,edx



	fld qword [esp+36]
	fld qword [ecx+ELM8]
	nop

	rep
	fmul st0,st1
	fld qword [ecx+ELM9]
	fmul st0,st2

	rep
	fld qword [ecx+ELM7]
	fmul st0,st3
	fxch st0,st3

	fld qword [ecx+ELM11]
	fmul st0,st1
	fld qword [ecx+ELM12]

	fmul st0,st2
	fld qword [ebx+ELM1]
	fld qword [ecx+ELM10]

	rep
	fmulp st4,st0
	rep
	mov edx,edx
	fnop


	OPERATION 1,2				;01+2
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
	fld qword [eax+DOTP1+ELM30]		;30+2
	fmul st0,st1
	faddp st7

	fld qword [eax+DOTP2+ELM30]
	fmul st0,st1
	faddp st6

	fld qword [eax+DOTP3+ELM30]
	fmul st0,st1
	faddp st5

	fld qword [eax+DOTP4+ELM30]
	fmul st0,st1
	faddp st4

	fld qword [eax+DOTP5+ELM30]
	fmul st0,st1
	faddp st3

	rep
	fmul qword [eax+DOTP6+ELM30]
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
		prefetch [ebx+30*8-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+30*8-1*64]
		nop

		prefetch [ebx+30*8+0*64]
		nop

		prefetch [ebx+30*8+1*64]
		nop

		prefetch [ebx+30*8+2*64]
		nop
	%endif



	fstp qword [ecx+ELM7]
	fxch st3
	fstp qword [ecx+ELM8]

	fxch st1
	fstp qword [ecx+ELM9]
	fstp qword [ecx+ELM10]

	fstp qword [ecx+ELM11]
	fstp qword [ecx+ELM12]
	add eax,edx



	fld qword [esp+36]
	fld qword [ecx+ELM14]
	nop

	rep
	fmul st0,st1
	fld qword [ecx+ELM15]
	fmul st0,st2

	rep
	fld qword [ecx+ELM13]
	fmul st0,st3
	fxch st0,st3

	fld qword [ecx+ELM17]
	fmul st0,st1
	fld qword [ecx+ELM18]

	rep
	fmul st0,st2
	fld qword [ebx+ELM1]
	fld qword [ecx+ELM16]

	rep
	fmulp st4,st0
	rep
	mov edx,edx
	fnop


	OPERATION 1,2				;01+3
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
	fld qword [eax+DOTP1+ELM30]		;30+3
	fmul st0,st1
	faddp st7

	fld qword [eax+DOTP2+ELM30]
	fmul st0,st1
	faddp st6

	fld qword [eax+DOTP3+ELM30]
	fmul st0,st1
	faddp st5

	fld qword [eax+DOTP4+ELM30]
	fmul st0,st1
	faddp st4

	fld qword [eax+DOTP5+ELM30]
	fmul st0,st1
	faddp st3

	rep
	fmul qword [eax+DOTP6+ELM30]
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
		prefetch [ebx+30*8-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+30*8-1*64]
		nop

		prefetch [ebx+30*8+0*64]
		nop

		prefetch [ebx+30*8+1*64]
		nop

		prefetch [ebx+30*8+2*64]
		nop
	%endif



	fstp qword [ecx+ELM13]
	fxch st3
	fstp qword [ecx+ELM14]

	rep
	fxch st1
	fstp qword [ecx+ELM15]
	fstp qword [ecx+ELM16]

	fstp qword [ecx+ELM17]
	fstp qword [ecx+ELM18]
	add eax,edx



	fld qword [esp+36]
	fld qword [ecx+ELM20]
	nop

	rep
	fmul st0,st1
	fld qword [ecx+ELM21]
	fmul st0,st2

	rep
	fld qword [ecx+ELM19]
	fmul st0,st3
	fxch st0,st3

	fld qword [ecx+ELM23]
	fmul st0,st1
	fld qword [ecx+ELM24]

	fmul st0,st2
	fld qword [ebx+ELM1]
	fld qword [ecx+ELM22]

	rep
	fmulp st4,st0
	rep
	mov edx,edx
	fnop


	OPERATION 1,2				;01+4
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
	fld qword [eax+DOTP1+ELM30]		;30+4
	fmul st0,st1
	faddp st7

	fld qword [eax+DOTP2+ELM30]
	fmul st0,st1
	faddp st6

	fld qword [eax+DOTP3+ELM30]
	fmul st0,st1
	faddp st5

	fld qword [eax+DOTP4+ELM30]
	fmul st0,st1
	faddp st4

	fld qword [eax+DOTP5+ELM30]
	fmul st0,st1
	faddp st3

	rep
	fmul qword [eax+DOTP6+ELM30]
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
		prefetch [ebx+30*8-2*64]
		fnop
		mov edx,edx

		prefetch [ebx+30*8-1*64]
		nop

		prefetch [ebx+30*8+0*64]
		nop

		prefetch [ebx+30*8+1*64]
		nop

		prefetch [ebx+30*8+2*64]
		nop
	%endif



	fstp qword [ecx+ELM19]
	fxch st3
	fstp qword [ecx+ELM20]

	fxch st1
	fstp qword [ecx+ELM21]
	fstp qword [ecx+ELM22]

	fstp qword [ecx+ELM23]
	fstp qword [ecx+ELM24]
	add eax,edx



	fld qword [esp+36]
	fld qword [ecx+ELM26]
	nop

	rep
	fmul st0,st1
	fld qword [ecx+ELM27]
	fmul st0,st2

	rep
	fld qword [ecx+ELM25]
	fmul st0,st3
	fxch st0,st3

	fld qword [ecx+ELM29]
	fmul st0,st1
	fld qword [ecx+ELM30]

	fmul st0,st2
	fld qword [ebx+ELM1]
	fld qword [ecx+ELM28]

	rep
	fmulp st4,st0
	rep
	mov edx,edx
	fnop


	OPERATION 1,2				;01+5
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
	fld qword [eax+DOTP1+ELM30]		;30+5
	fmul st0,st1
	faddp st7

	fld qword [eax+DOTP2+ELM30]
	fmul st0,st1
	faddp st6

	fld qword [eax+DOTP3+ELM30]
	fmul st0,st1
	faddp st5

	fld qword [eax+DOTP4+ELM30]
	fmul st0,st1
	faddp st4

	fld qword [eax+DOTP5+ELM30]
	fmul st0,st1
	faddp st3

	rep
	fmul qword [eax+DOTP6+ELM30]
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



	fstp qword [ecx+ELM25]
	fxch st3
	fstp qword [ecx+ELM26]

	fxch st1
	fstp qword [ecx+ELM27]
	fstp qword [ecx+ELM28]

	fstp qword [ecx+ELM29]
	fstp qword [ecx+ELM30]
	sub ebx,edi				;next column of B


	mov eax,[esp+4]				;reset eax
	add ecx,[esp+12]			;next column of C (+ldc*8)

	dec dword [esp+8]			;dec counter
	jnz near loopj_


end_
	femms

	pop ebp
	add esp,byte 5*4			;remove local variables

	pop edi					;restore registers
	pop esi
	pop ebx

eend_	leave                			;mov esp,ebp / pop ebp
	ret

