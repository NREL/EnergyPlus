#ifndef TARCOGDeflection_hh_INCLUDED
#define TARCOGDeflection_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TARCOGDeflection {

	// Functions

	void
	PanesDeflection(
		int const DeflectionStandard,
		Real64 const W,
		Real64 const H,
		int const nlayer,
		Real64 const Pa,
		Real64 const Pini,
		Real64 const Tini,
		Array1A< Real64 > const PaneThickness,
		Array1A< Real64 > const NonDeflectedGapWidth,
		Array1A< Real64 > DeflectedGapWidthMax,
		Array1A< Real64 > DeflectedGapWidthMean,
		Array1A< Real64 > const PanelTemps,
		Array1A< Real64 > const YoungsMod,
		Array1A< Real64 > const PoissonsRat,
		Array1A< Real64 > LayerDeflection,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	DeflectionTemperatures(
		int const nlayer,
		Real64 const W,
		Real64 const H,
		Real64 const Pa,
		Real64 const Pini,
		Real64 const Tini,
		Array1A< Real64 > const NonDeflectedGapWidth,
		Array1A< Real64 > DeflectedGapWidthMax,
		Array1A< Real64 > DeflectedGapWidthMean,
		Array1A< Real64 > const PanelTemps,
		Array1A< Real64 > DCoeff,
		Array1A< Real64 > LayerDeflection,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	DeflectionWidths(
		int const nlayer,
		Real64 const W,
		Real64 const H,
		Array1A< Real64 > DCoeff,
		Array1A< Real64 > const NonDeflectedGapWidth,
		Array1A< Real64 > const DeflectedGapWidthMax,
		Array1A< Real64 > DeflectedGapWidthMean,
		Array1A< Real64 > LayerDeflection
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

} // TARCOGDeflection

} // EnergyPlus

#endif
