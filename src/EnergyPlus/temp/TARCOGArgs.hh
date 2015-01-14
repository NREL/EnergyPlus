#ifndef TARCOGArgs_hh_INCLUDED
#define TARCOGArgs_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TARCOGArgs {

	// Functions

	int
	ArgCheck(
		int const nlayer,
		int const iwd,
		Real64 const tout,
		Real64 const tind,
		Real64 const trmin,
		Real64 const wso,
		Real64 const wsi,
		Real64 const dir,
		Real64 const outir,
		int const isky,
		Real64 const tsky,
		Real64 const esky,
		Real64 const fclr,
		Real64 const VacuumPressure,
		Real64 const VacuumMaxGapThickness,
		int const CalcDeflection,
		Real64 const Pa,
		Real64 const Pini,
		Real64 const Tini,
		FArray1A< Real64 > const gap,
		FArray1A< Real64 > const GapDef,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const scon,
		FArray1A< Real64 > const YoungsMod,
		FArray1A< Real64 > const PoissonsRat,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const emis,
		Real64 const totsol,
		Real64 const tilt,
		FArray1A< Real64 > const asol,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		FArray1A< Real64 > const presure,
		FArray2A_int const iprop,
		FArray2A< Real64 > const frct,
		FArray2A< Real64 > const xgcon,
		FArray2A< Real64 > const xgvis,
		FArray2A< Real64 > const xgcp,
		FArray1A< Real64 > const xwght,
		FArray1A< Real64 > const gama,
		FArray1A_int const nmix,
		FArray1A_int const SupportPillar, // Shows whether or not gap have support pillar
		FArray1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		FArray1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		Real64 & hin,
		Real64 & hout,
		FArray1A_int const ibc,
		FArray1A< Real64 > const Atop,
		FArray1A< Real64 > const Abot,
		FArray1A< Real64 > const Al,
		FArray1A< Real64 > const Ar,
		FArray1A< Real64 > const Ah,
		FArray1A< Real64 > const SlatThick,
		FArray1A< Real64 > const SlatWidth,
		FArray1A< Real64 > const SlatAngle,
		FArray1A< Real64 > const SlatCond,
		FArray1A< Real64 > const SlatSpacing,
		FArray1A< Real64 > const SlatCurve,
		FArray1A< Real64 > const vvent,
		FArray1A< Real64 > const tvent,
		FArray1A_int const LayerType,
		FArray1A_int const nslice,
		FArray1A< Real64 > const LaminateA,
		FArray1A< Real64 > const LaminateB,
		FArray1A< Real64 > const sumsol,
		int const standard,
		int const ThermalMod,
		Real64 const SDScalar,
		std::string & ErrorMessage
	);

	void
	PrepVariablesISO15099(
		int const nlayer,
		Real64 const tout,
		Real64 const tind,
		Real64 & trmin,
		int const isky,
		Real64 const outir, // IR radiance of window's exterior/interior surround (W/m2)
		Real64 const tsky,
		Real64 & esky,
		Real64 const fclr,
		FArray1A< Real64 > gap,
		FArray1A< Real64 > thick,
		FArray1A< Real64 > scon,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const emis,
		Real64 const tilt,
		Real64 & hin,
		Real64 & hout,
		FArray1A_int const ibc,
		FArray1A< Real64 > const SlatThick,
		FArray1A< Real64 > const SlatWidth,
		FArray1A< Real64 > const SlatAngle,
		FArray1A< Real64 > const SlatCond,
		FArray1A_int const LayerType,
		int const ThermalMod,
		Real64 const SDScalar,
		Real64 & ShadeEmisRatioOut,
		Real64 & ShadeEmisRatioIn,
		Real64 & ShadeHcRatioOut,
		Real64 & ShadeHcRatioIn,
		FArray1A< Real64 > Keff,
		FArray1A< Real64 > ShadeGapKeffConv,
		Real64 & sc,
		Real64 & shgc,
		Real64 & ufactor,
		Real64 & flux,
		FArray1A< Real64 > LaminateAU,
		FArray1A< Real64 > sumsolU,
		FArray1A< Real64 > sol0,
		Real64 & hint,
		Real64 & houtt,
		Real64 & trmout,
		Real64 & ebsky,
		Real64 & ebroom,
		Real64 & Gout,
		Real64 & Gin,
		FArray1A< Real64 > rir,
		FArray1A< Real64 > vfreevent,
		int & nperr,
		std::string & ErrorMessage
	);

	bool
	GoAhead( int const nperr );

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

} // TARCOGArgs

} // EnergyPlus

#endif
