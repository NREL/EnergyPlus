#ifndef ThermalEN673Calc_hh_INCLUDED
#define ThermalEN673Calc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ThermalEN673Calc {

	// Functions

	void
	Calc_EN673(
		int const standard,
		int const nlayer,
		Real64 const tout,
		Real64 const tind,
		FArray1A< Real64 > gap,
		FArray1A< Real64 > thick,
		FArray1A< Real64 > scon,
		FArray1A< Real64 > const emis,
		Real64 const totsol,
		Real64 const tilt,
		Real64 const dir,
		FArray1A< Real64 > const asol,
		FArray1A< Real64 > const presure,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray1A_int const nmix,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		FArray1A< Real64 > const xwght,
		FArray1A< Real64 > theta,
		Real64 & ufactor,
		Real64 & hcin,
		Real64 & hin,
		Real64 & hout,
		Real64 & shgc,
		int & nperr,
		std::string & ErrorMessage,
		FArray1A_int const ibc,
		FArray1A< Real64 > hg,
		FArray1A< Real64 > hr,
		FArray1A< Real64 > hs,
		FArray1A< Real64 > Ra,
		FArray1A< Real64 > Nu
	);

	void
	EN673ISO10292(
		int const nlayer,
		Real64 const tout,
		Real64 const tind,
		FArray1A< Real64 > const emis,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		Real64 const tilt,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		FArray1A< Real64 > const xwght,
		FArray1A< Real64 > const presure,
		FArray1A_int const nmix,
		FArray1A< Real64 > theta,
		int const standard,
		FArray1A< Real64 > hg,
		FArray1A< Real64 > hr,
		FArray1A< Real64 > hs,
		Real64 & hin,
		Real64 const hout,
		Real64 & hcin,
		FArray1A_int const ibc,
		FArray1A< Real64 > rs,
		Real64 & ufactor,
		FArray1A< Real64 > Ra,
		FArray1A< Real64 > Nu,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	linint(
		Real64 const x1,
		Real64 const x2,
		Real64 const y1,
		Real64 const y2,
		Real64 const x,
		Real64 & y
	);

	void
	solar_EN673(
		Real64 const dir,
		Real64 const totsol,
		Real64 const rtot,
		FArray1A< Real64 > const rs,
		int const nlayer,
		FArray1A< Real64 > const absol,
		Real64 & sf,
		int const standard,
		int & nperr,
		std::string & ErrorMessage
	);

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // ThermalEN673Calc

} // EnergyPlus

#endif
