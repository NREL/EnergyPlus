#ifndef TARCOGCommon_hh_INCLUDED
#define TARCOGCommon_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TARCOGCommon {

	// Functions

	bool
	IsShadingLayer( int const layertype );

	Real64
	LDSumMax(
		Real64 const Width,
		Real64 const Height
	);

	Real64
	LDSumMean(
		Real64 const Width,
		Real64 const Height
	);

	void
	matrixQBalance(
		int const nlayer,
		FArray2A< Real64 > a,
		FArray1A< Real64 > b,
		FArray1A< Real64 > const scon,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const hcgas,
		Real64 const hcout,
		Real64 const hcin,
		FArray1A< Real64 > const asol,
		FArray1A< Real64 > const qv,
		Real64 const Tin,
		Real64 const Tout,
		Real64 const Gin,
		Real64 const Gout,
		FArray1A< Real64 > const theta,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const rir,
		FArray1A< Real64 > const emis
	);

	void
	EquationsSolver(
		FArray2A< Real64 > a,
		FArray1A< Real64 > b,
		int const n,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	ludcmp(
		FArray2A< Real64 > a,
		int const n,
		FArray1A_int indx,
		Real64 & d,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	lubksb(
		FArray2A< Real64 > const a,
		int const n,
		FArray1A_int const indx,
		FArray1A< Real64 > b
	);

	Real64
	pos( Real64 const x );

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // TARCOGCommon

} // EnergyPlus

#endif
