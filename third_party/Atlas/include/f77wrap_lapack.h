/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
 *
 * Code contributers : R. Clint Whaley, Antoine P. Petitet
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

#ifndef F77WRAP_LAPACK_H
#define F77WRAP_LAPACK_H

#include "atlas_misc.h"
#include "atlas_f77.h"

#ifdef UpCase
   #define FW ATL_F77WRAP_
   #define PFW Mjoin(ATL_F77WRAP_,PREU)
#else
   #define FW atl_f77wrap_
   #define PFW Mjoin(atl_f77wrap_,PRE)
#endif

#ifdef Add_
   #define F77WRAP_ILAENV Mjoin(FW,ilaenv_)
   #define F77WRAP_GETRI Mjoin(PFW,getri_)
   #define F77WRAP_LAUUM Mjoin(PFW,lauum_)
   #define F77WRAP_TRTRI Mjoin(PFW,trtri_)
   #define F77WRAP_GETNB Mjoin(PFW,getnb_)
   #define F77WRAP_GETRS Mjoin(PFW,getrs_)
   #define F77WRAP_GETRF Mjoin(PFW,getrf_)
   #define F77WRAP_GESV Mjoin(PFW,gesv_)
   #define F77WRAP_POTRS Mjoin(PFW,potrs_)
   #define F77WRAP_POTRF Mjoin(PFW,potrf_)
   #define F77WRAP_POSV Mjoin(PFW,posv_)
   #define F77WRAP_GELS Mjoin(PFW,gels_)
   #define F77WRAP_LARFT Mjoin(PFW,larft_)
   #define F77WRAP_LARFB Mjoin(PFW,larfb_)
   #define F77WRAP_GELQF Mjoin(PFW,gelqf_)
   #define F77WRAP_GERQF Mjoin(PFW,gerqf_)
   #define F77WRAP_GEQLF Mjoin(PFW,geqlf_)
   #define F77WRAP_GEQRF Mjoin(PFW,geqrf_)
#elif defined(Add__)
   #define F77WRAP_ILAENV Mjoin(FW,ilaenv__)
   #define F77WRAP_GETRI Mjoin(PFW,getri__)
   #define F77WRAP_LAUUM Mjoin(PFW,lauum__)
   #define F77WRAP_TRTRI Mjoin(PFW,trtri__)
   #define F77WRAP_GETNB Mjoin(PFW,getnb__)
   #define F77WRAP_GETRS Mjoin(PFW,getrs__)
   #define F77WRAP_GETRF Mjoin(PFW,getrf__)
   #define F77WRAP_GESV Mjoin(PFW,gesv__)
   #define F77WRAP_POTRS Mjoin(PFW,potrs__)
   #define F77WRAP_POTRF Mjoin(PFW,potrf__)
   #define F77WRAP_POSV Mjoin(PFW,posv__)
   #define F77WRAP_GELS Mjoin(PFW,gels__)
   #define F77WRAP_LARFT Mjoin(PFW,larft__)
   #define F77WRAP_LARFB Mjoin(PFW,larfb__)
   #define F77WRAP_GELQF Mjoin(PFW,gelqf__)
   #define F77WRAP_GERQF Mjoin(PFW,gerqf__)
   #define F77WRAP_GEQLF Mjoin(PFW,geqlf__)
   #define F77WRAP_GEQRF Mjoin(PFW,geqrf__)
#elif defined(NoChange)
   #define F77WRAP_ILAENV Mjoin(FW,ilaenv)
   #define F77WRAP_GETRI Mjoin(PFW,getri)
   #define F77WRAP_LAUUM Mjoin(PFW,lauum)
   #define F77WRAP_TRTRI Mjoin(PFW,trtri)
   #define F77WRAP_GETNB Mjoin(PFW,getnb)
   #define F77WRAP_GETRS Mjoin(PFW,getrs)
   #define F77WRAP_GETRF Mjoin(PFW,getrf)
   #define F77WRAP_GESV Mjoin(PFW,gesv)
   #define F77WRAP_POTRS Mjoin(PFW,potrs)
   #define F77WRAP_POTRF Mjoin(PFW,potrf)
   #define F77WRAP_POSV Mjoin(PFW,posv)
   #define F77WRAP_GELS Mjoin(PFW,gels)
   #define F77WRAP_LARFT Mjoin(PFW,larft)
   #define F77WRAP_LARFB Mjoin(PFW,larfb)
   #define F77WRAP_GELQF Mjoin(PFW,gelqf)
   #define F77WRAP_GERQF Mjoin(PFW,gerqf)
   #define F77WRAP_GEQLF Mjoin(PFW,geqlf)
   #define F77WRAP_GEQRF Mjoin(PFW,geqrf)
#elif defined(UpCase)
   #define F77WRAP_ILAENV Mjoin(PF,ILAENV)
   #define F77WRAP_ILAENV Mjoin(PFW,ILAENV)
   #define F77WRAP_GETRI Mjoin(PFW,GETRI)
   #define F77WRAP_LAUUM Mjoin(PFW,LAUUM)
   #define F77WRAP_TRTRI Mjoin(PFW,TRTRI)
   #define F77WRAP_GETNB Mjoin(PFW,GETNB)
   #define F77WRAP_GETRS Mjoin(PFW,GETRS)
   #define F77WRAP_GETRF Mjoin(PFW,GETRF)
   #define F77WRAP_GESV Mjoin(PFW,GESV)
   #define F77WRAP_POTRS Mjoin(PFW,POTRS)
   #define F77WRAP_POTRF Mjoin(PFW,POTRF)
   #define F77WRAP_POSV Mjoin(PFW,POSV)
   #define F77WRAP_GELS Mjoin(PFW,GELS)
   #define F77WRAP_LARFT Mjoin(PFW,LARFT)
   #define F77WRAP_LARFB Mjoin(PFW,LARFB)
   #define F77WRAP_GELQF Mjoin(PFW,GELQF)
   #define F77WRAP_GERQF Mjoin(PFW,GERQF)
   #define F77WRAP_GEQLF Mjoin(PFW,GEQLF)
   #define F77WRAP_GEQRF Mjoin(PFW,GEQRF)
#endif

#endif
