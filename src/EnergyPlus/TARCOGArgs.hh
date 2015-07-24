#ifndef TARCOGArgs_hh_INCLUDED
#define TARCOGArgs_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

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
		Array1A< Real64 > const gap,
		Array1A< Real64 > const GapDef,
		Array1A< Real64 > const thick,
		Array1A< Real64 > const scon,
		Array1A< Real64 > const YoungsMod,
		Array1A< Real64 > const PoissonsRat,
		Array1A< Real64 > const tir,
		Array1A< Real64 > const emis,
		Real64 const totsol,
		Real64 const tilt,
		Array1A< Real64 > const asol,
		Real64 const height,
		Real64 const heightt,
		Real64 const width,
		Array1A< Real64 > const presure,
		Array2A_int const iprop,
		Array2A< Real64 > const frct,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Array1A< Real64 > const xwght,
		Array1A< Real64 > const gama,
		Array1A_int const nmix,
		Array1A_int const SupportPillar, // Shows whether or not gap have support pillar
		Array1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		Array1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		Real64 & hin,
		Real64 & hout,
		Array1A_int const ibc,
		Array1A< Real64 > const Atop,
		Array1A< Real64 > const Abot,
		Array1A< Real64 > const Al,
		Array1A< Real64 > const Ar,
		Array1A< Real64 > const Ah,
		Array1A< Real64 > const SlatThick,
		Array1A< Real64 > const SlatWidth,
		Array1A< Real64 > const SlatAngle,
		Array1A< Real64 > const SlatCond,
		Array1A< Real64 > const SlatSpacing,
		Array1A< Real64 > const SlatCurve,
		Array1A< Real64 > const vvent,
		Array1A< Real64 > const tvent,
		Array1A_int const LayerType,
		Array1A_int const nslice,
		Array1A< Real64 > const LaminateA,
		Array1A< Real64 > const LaminateB,
		Array1A< Real64 > const sumsol,
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
		Array1A< Real64 > gap,
		Array1A< Real64 > thick,
		Array1A< Real64 > scon,
		Array1A< Real64 > const tir,
		Array1A< Real64 > const emis,
		Real64 const tilt,
		Real64 & hin,
		Real64 & hout,
		Array1A_int const ibc,
		Array1A< Real64 > const SlatThick,
		Array1A< Real64 > const SlatWidth,
		Array1A< Real64 > const SlatAngle,
		Array1A< Real64 > const SlatCond,
		Array1A_int const LayerType,
		int const ThermalMod,
		Real64 const SDScalar,
		Real64 & ShadeEmisRatioOut,
		Real64 & ShadeEmisRatioIn,
		Real64 & ShadeHcRatioOut,
		Real64 & ShadeHcRatioIn,
		Array1A< Real64 > Keff,
		Array1A< Real64 > ShadeGapKeffConv,
		Real64 & sc,
		Real64 & shgc,
		Real64 & ufactor,
		Real64 & flux,
		Array1A< Real64 > LaminateAU,
		Array1A< Real64 > sumsolU,
		Array1A< Real64 > sol0,
		Real64 & hint,
		Real64 & houtt,
		Real64 & trmout,
		Real64 & ebsky,
		Real64 & ebroom,
		Real64 & Gout,
		Real64 & Gin,
		Array1A< Real64 > rir,
		Array1A< Real64 > vfreevent,
		int & nperr,
		std::string & ErrorMessage
	);

	bool
	GoAhead( int const nperr );

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

} // TARCOGArgs

} // EnergyPlus

#endif
