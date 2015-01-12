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
;	ATL_sJIK48x48x48TN48x48x0_a1.mcr
;
;	ATLAS "Speed of Light" SGEMM() kernel for AMD Athlon
;	Code author: Julian Ruhe (ruheejih@linux.zrz.tu-berlin.de | Julian.Ruhe@t-online.de)
;


%define NB 48

%define ELM1 -30*4
%define ELM2 -29*4
%define ELM3 -28*4
%define ELM4 -27*4
%define ELM5 -26*4
%define ELM6 -25*4
%define ELM7 -24*4
%define ELM8 -23*4
%define ELM9 -22*4
%define ELM10 -21*4
%define ELM11 -20*4
%define ELM12 -19*4
%define ELM13 -18*4
%define ELM14 -17*4
%define ELM15 -16*4
%define ELM16 -15*4
%define ELM17 -14*4
%define ELM18 -13*4
%define ELM19 -12*4
%define ELM20 -11*4
%define ELM21 -10*4
%define ELM22 -9*4
%define ELM23 -8*4
%define ELM24 -7*4
%define ELM25 -6*4
%define ELM26 -5*4
%define ELM27 -4*4
%define ELM28 -3*4
%define ELM29 -2*4
%define ELM30 -1*4
%define ELM31 0*4
%define ELM32 1*4
%define ELM33 2*4
%define ELM34 3*4
%define ELM35 4*4
%define ELM36 5*4
%define ELM37 6*4
%define ELM38 7*4
%define ELM39 8*4
%define ELM40 9*4
%define ELM41 10*4
%define ELM42 11*4
%define ELM43 12*4
%define ELM44 13*4
%define ELM45 14*4
%define ELM46 15*4
%define ELM47 16*4
%define ELM48 17*4


%define DOTP1 ebp
%define DOTP2 4*edi
%define DOTP3 esi
%define DOTP4 2*edi
%define DOTP5 edi
%define DOTP6 0

%macro OPERATION 2
	%if ELM%1 == 0
	rep
	%endif
	fld dword [eax+DOTP1+ELM%1]
	fmul st0,st1
	faddp st7

	%if ELM%1 == 0
	rep
	%endif
	fld dword [eax+DOTP2+ELM%1]
	fmul st0,st1
	faddp st6

	%if ELM%1 == 0
	rep
	%endif
	fld dword [eax+DOTP3+ELM%1]
	fmul st0,st1
	faddp st5

	%if ELM%1 == 0
	rep
	%endif
	fld dword [eax+DOTP4+ELM%1]
	fmul st0,st1
	faddp st4

	%if ELM%1 == 0
	rep
	%endif
	fld dword [eax+DOTP5+ELM%1]
	fmul st0,st1
	faddp st3

	%if ELM%1 == 0
	rep
	%endif
	fmul dword [eax+DOTP6+ELM%1]
	faddp st1
	%if ELM%2 == 0
	rep
	%endif
	fld dword [ebx+ELM%2]



%endmacro
