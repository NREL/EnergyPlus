// EnergyPlus Headers
#include <DataUCSDSharedData.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataUCSDSharedData {

	// Module containing the data shared between the UCSD models.

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   March 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// <description>

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// The Eplus surface numbers will be stored in the arrays Apos according to the
	// type of surface. The PosZ_Wall array has dimension 2 times the Number of Zones and
	// for each zone it has 2 positions: the start and end positions in the Apos_Wall array
	// for that specific zone.
	Array1D_int APos_Wall;
	Array1D_int APos_Floor;
	Array1D_int APos_Ceiling;
	Array1D_int PosZ_Wall;
	Array1D_int PosZ_Floor;
	Array1D_int PosZ_Ceiling;
	Array1D_int APos_Window;
	Array1D_int APos_Door;
	Array1D_int APos_Internal;
	Array1D_int PosZ_Window;
	Array1D_int PosZ_Door;
	Array1D_int PosZ_Internal;
	// Convection coeficients for the various surfaces
	Array1D< Real64 > HCeiling;
	Array1D< Real64 > HWall;
	Array1D< Real64 > HFloor;
	Array1D< Real64 > HInternal;
	Array1D< Real64 > HWindow;
	Array1D< Real64 > HDoor;

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

} // DataUCSDSharedData

} // EnergyPlus
