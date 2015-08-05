// EnergyPlus Headers
#include <DataMoistureBalanceEMPD.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataMoistureBalanceEMPD {

	// MODULE INFORMATION:
	//       AUTHOR         Muthusamy V. Swami and Lixing Gu
	//       DATE WRITTEN   Aug. 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module should contain the information that is needed to calculate
	// moisture level at interior surfaces

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS

	// Parameters for the definition and limitation of arrays:

	Real64 const Lam( 2500000.0 ); // heat of adsorption for building materials

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	// Variables that are used in both the Surface Heat Balance and the Moisture Balance
	Array1D< Real64 > MoistEMPDOld; // Moisture level at interior surfaces at previous time step
	Array1D< Real64 > MoistEMPDInt; // Moisture level at interior surfaces at previous interation
	// and current time step
	Array1D< Real64 > MoistEMPDNew; // Moisture level at interior surfaces at current interation
	// and current time step
	Array1D< Real64 > MoistEMPDFlux; // Moisture flux at interior surfaces [W]

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

} // DataMoistureBalanceEMPD

} // EnergyPlus
