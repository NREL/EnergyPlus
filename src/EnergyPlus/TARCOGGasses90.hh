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
		Array1A_int const iprop,
		Array1A< Real64 > const frct,
		Real64 const pres,
		int const nmix,
		Array1A< Real64 > const xwght,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
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

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // TARCOGGasses90

} // EnergyPlus

#endif
