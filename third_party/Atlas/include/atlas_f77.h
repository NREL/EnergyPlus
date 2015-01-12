/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
#ifndef ATLAS_F77_H
#define ATLAS_F77_H

   #ifndef ATL_F77_SUBROUTINE
      #define ATL_F77_SUBROUTINE void
   #endif
   #ifndef F77_INTEGER
      #define F77_INTEGER int
   #else
      #define ATL_FunkyInts
   #endif
   #if defined(CRAY)
      #define UseTransChar 1
      #include <fortran.h>
      #define F77_CHAR _fcd
      #define ATL_F2C_TransChar(c) (*(_fcdtocp(c) ))
      #define ATL_C2F_TransChar(c) (_cptofcd(&(c), 1))
   #elif defined(StringStructVal)
      typedef struct {char *cp; F77_INTEGER len;} F77_CHAR;
      #define ATL_F2C_TransChar(c) (*(c.cp))
      #define UseTransChar 2
   #elif defined(StringStructPtr)
      typedef struct {char *cp; F77_INTEGER len;} F77_CHAR;
      #define ATL_F2C_TransChar(c) (*(c->cp))
      #define UseTransChar 3
   #else
      #define ATL_DeclareSlens
      #define F77_CHAR char *
      #define ATL_F2C_TransChar(c) (*(c))
      #define ATL_C2F_TransChar(c) (&(c))
      #define ATL_STRLEN_1 ,F77_INTEGER ATL_Slen1
      #define ATL_STRLEN_2 ,F77_INTEGER ATL_Slen1, F77_INTEGER ATL_Slen2
      #define ATL_STRLEN_3 ,F77_INTEGER ATL_Slen1, F77_INTEGER ATL_Slen2, \
                           F77_INTEGER ATL_Slen3
      #define ATL_STRLEN_4 ,F77_INTEGER ATL_Slen1, F77_INTEGER ATL_Slen2, \
                           F77_INTEGER ATL_Slen3, F77_INTEGER ATL_Slen4
      #define ATL_STRLEN_1_para ,ATL_Slen1
      #define ATL_STRLEN_2_para ,ATL_Slen1, ATL_Slen2
      #define ATL_STRLEN_3_para ,ATL_Slen1, ATL_Slen2, ATL_Slen3
      #define ATL_STRLEN_4_para ,ATL_Slen1, ATL_Slen2, ATL_Slen3, ATL_Slen4
   #endif

   #ifndef ATL_STRLEN_1
      #define ATL_STRLEN_1
      #define ATL_STRLEN_2
      #define ATL_STRLEN_3
      #define ATL_STRLEN_4
      #define ATL_STRLEN_1_para
      #define ATL_STRLEN_2_para
      #define ATL_STRLEN_3_para
      #define ATL_STRLEN_4_para
   #endif

#endif
