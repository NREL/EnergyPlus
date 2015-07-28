// EnergyPlus Headers
#include <DataWindowEquivalentLayer.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataWindowEquivalentLayer {
	// MODULE INFORMATION:
	//       AUTHOR         Bereket Nigusse, FSEC/UCF
	//       DATE WRITTEN   May 2013
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module for equivalent layer window model.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// CFSTY: Complex Fenestration System
	int const CFSMAXNL( 6 ); // max # of glaze or shade layers
	// Long-wave (aka LW or thermal) layer properties
	// Short wave (aka SW or solar) layer properties
	// "black" room (no reflection)
	// Layer information

	// Gap Gas Properties
	// Gap information
	// Equivalent Layer Window Constructon
	// CFSLAYER: layer types
	int const ltyNONE( 0 ); // unused / empty layer
	int const ltyGLAZE( 1 ); // glazing layer i.e, purely specular
	int const ltyDRAPE( 2 ); // pleated drapes/curtains
	int const ltyROLLB( 3 ); // roller blind
	int const ltyVBHOR( 4 ); // venetian blinds - horizontal
	int const ltyVBVER( 5 ); // venetian blinds - vertical
	int const ltyINSCRN( 6 ); // insect screen
	int const ltyROOM( 7 ); // indoor space and/or make no adjustment
	int const ltyGZS( 8 ); // glazing with spectral data (read from aux file)
	// index for solar arrays
	int const isDIFF( 1 );
	int const isBEAM( 2 );
	// Defined CFSLayers and CFSs
	int TotWinEquivLayerConstructs( 0 ); // Number of constructions with Window equivalent Layer

	// Object Data
	CFSSWP SWP_ROOMBLK; // Solar reflectance, BEAM-BEAM, front | Solar reflectance, BEAM-BEAM, back | Solar transmittance, BEAM-BEAM, front | Solar transmittance, BEAM-BEAM, back | Solar reflectance, BEAM-DIFFUSE, front | Solar reflectance, BEAM-DIFFUSE, back | Solar transmittance, BEAM-DIFFUSE, front | Solar transmittance, BEAM-DIFFUSE, back | Solar reflectance, DIFFUSE-DIFFUSE, front | Solar reflectance, DIFFUSE-DIFFUSE, back | Solar transmittance, DIFFUSE-DIFFUSE
	Array1D< CFSLAYER > CFSLayers;
	Array1D< CFSTY > CFS;
	Array1D< CFSGAP > CFSGaps;

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

} // DataWindowEquivalentLayer

} // EnergyPlus
