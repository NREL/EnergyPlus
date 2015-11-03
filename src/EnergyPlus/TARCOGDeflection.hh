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

} // TARCOGDeflection

} // EnergyPlus

#endif
