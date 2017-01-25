// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef TARCOGGasses90_hh_INCLUDED
#define TARCOGGasses90_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TARCOGGasses90 {

	// Data
	//private doe2gas90

	// Functions

	void
	GASSES90(
		Real64 const tmean,
		Array1_int const & iprop,
		Array1< Real64 > const & frct,
		Real64 const pres,
		int const nmix,
		Array1< Real64 > const & xwght,
		Array2< Real64 > const & xgcon,
		Array2< Real64 > const & xgvis,
		Array2< Real64 > const & xgcp,
		Real64 & con,
		Real64 & visc,
		Real64 & dens,
		Real64 & cp,
		Real64 & pr,
		int const standard,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	GassesLow(
		Real64 const tmean,
		Real64 const mwght,
		Real64 const pressure,
		Real64 const gama,
		Real64 & cond,
		int & nperr,
		std::string & ErrorMessage
	);

	//  subroutine doe2gas90 (standard, iprop, frct, pres, nmix, con0, dcon, visc0, dvisc, dens0, ddens, pr0, dpr)
	//    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
	//    !  calculate gas properties using old doe2 format                              !
	//    !------------------------------------------------------------------------------!
	//    !  iprop(i)  vector of gas identifiers (for max of w5cog.fi::maxgas gasses)
	//    !  frct(i)   vector of fraction of gasses in a mixture (for max of w5cog.fi::maxgas gasses)
	//    !  pres(i)   pressure (default: pres = 1e5)[N/m^2]
	//    !  nmix(i)   number of gasses in a mixture
	//    !  con0(o)   thermal conductivity @ mean temperature of 0 C[W/m-K]
	//    !  dcon(o)   derivative of thermal conductivity wrt temperature x 10^5 [W/m-K^2 x 10^5]
	//    !  visc0(o)  dynamic viscosity @ mean temperature of 0 C x 10^5 [kg/m-s x 10^5]
	//    !  dvisc(o)  derivative of dynamic viscosity wrt temperature x 10^8 [kg/m-s-K x 10^8]
	//    !  dens0(o)  density @ mean temperature of 0 C [kg/m^3]
	//    !  ddens(o)  derivative of density wrt temperature [kg/m^3-K]
	//    !  pr0(o)    Prandl number @ mean temperature of 0 C [ - ]
	//    !  dpr(o)    derivative of Prandl number wrt temperature [ 1/K ]
	//    !  nperr(o)  error flag (if component fraction in a mixture is 0%)
	//    !
	//    !**import:
	//    !  w5cog.fi::maxgas
	//    !
	//    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

	//    ! Variables

	//    integer, intent(in) :: nmix, iprop(maxgas)
	//    REAL(r64), intent(in) :: pres, frct(maxgas)

	//    REAL(r64), intent(out) :: con0, dcon, visc0, dvisc, dens0, ddens, pr0, dpr

	//    REAL(r64) :: con, visc, dens, cp, pr
	//    integer :: standard, nperr
	//    character(len=2000) :: ErrMsg

	//    call GASSES90(273.15d0, iprop, frct, pres,nmix, wght, gcon, gvis, gcp, con, visc, dens, cp, pr, standard, nperr, ErrMsg)

	//    con0=con
	//    visc0=visc*10**5
	//    dens0=dens
	//    pr0=pr

	//    call GASSES90(283.15d0,iprop, frct, pres, nmix, wght, gcon, gvis, gcp, con, visc, dens, cp, pr, standard, nperr, ErrMsg)

	//    dcon=(con-con0)/10*10**5
	//    dvisc=(visc*10**5-visc0)/10*10**3
	//    ddens=(dens-dens0)/10
	//    dpr=(pr-pr0)/10

	//  end subroutine doe2gas90

} // TARCOGGasses90

} // EnergyPlus

#endif
