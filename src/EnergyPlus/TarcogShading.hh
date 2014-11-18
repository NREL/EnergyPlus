#ifndef TarcogShading_hh_INCLUDED
#define TarcogShading_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TarcogShading {

	// Functions

	void
	shading(
		FArray1A< Real64 > const theta,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > hgas,
		FArray1A< Real64 > hcgas,
		FArray1A< Real64 > hrgas,
		FArray2A< Real64 > const frct,
		FArray2A_int const iprop,
		FArray1A< Real64 > const pressure,
		FArray1A_int const nmix,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		int const nlayer,
		Real64 const width,
		Real64 const height,
		Real64 const angle,
		Real64 const Tout,
		Real64 const Tin,
		FArray1A< Real64 > const Atop,
		FArray1A< Real64 > const Abot,
		FArray1A< Real64 > const Al,
		FArray1A< Real64 > const Ar,
		FArray1A< Real64 > const Ah,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const tvent,
		FArray1A_int const LayerType,
		FArray1A< Real64 > Tgaps,
		FArray1A< Real64 > qv,
		int & nperr,
		std::string & ErrorMessage,
		FArray1A< Real64 > vfreevent
	);

	void
	forcedventilation(
		FArray1A_int const iprop,
		FArray1A< Real64 > const frct,
		Real64 const press,
		int const nmix,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		Real64 const s,
		Real64 const H,
		Real64 const hc,
		Real64 const forcedspeed,
		Real64 const Tinlet,
		Real64 & Toutlet,
		Real64 const Tav,
		Real64 & hcv,
		Real64 & qv,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	shadingin(
		FArray1A_int const iprop1,
		FArray1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		FArray1A_int const iprop2,
		FArray1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		Real64 & Atop,
		Real64 & Abot,
		Real64 const Al,
		Real64 const Ar,
		Real64 const Ah,
		Real64 const s1,
		Real64 const s2,
		Real64 const H,
		Real64 const L,
		Real64 const angle,
		Real64 const hc1,
		Real64 const hc2,
		Real64 & speed1,
		Real64 & speed2,
		Real64 & Tgap1,
		Real64 & Tgap2,
		Real64 const Tav1,
		Real64 const Tav2,
		Real64 & hcv1,
		Real64 & hcv2,
		Real64 & qv1,
		Real64 & qv2,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	shadingedge(
		FArray1A_int const iprop1,
		FArray1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		FArray1A_int const iprop2,
		FArray1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		FArray1A< Real64 > const xwght,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		Real64 & Atop,
		Real64 & Abot,
		Real64 const Al,
		Real64 const Ar,
		Real64 & Ah,
		Real64 const s,
		Real64 const H,
		Real64 const L,
		Real64 const angle,
		Real64 const forcedspeed,
		Real64 const hc,
		Real64 const Tenv,
		Real64 const Tav,
		Real64 & Tgap,
		Real64 & hcv,
		Real64 & qv,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & speed
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

} // TarcogShading

} // EnergyPlus

#endif
