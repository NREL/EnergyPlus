#ifndef DataUCSDSharedData_hh_INCLUDED
#define DataUCSDSharedData_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataUCSDSharedData {

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
	extern FArray1D_int APos_Wall;
	extern FArray1D_int APos_Floor;
	extern FArray1D_int APos_Ceiling;
	extern FArray1D_int PosZ_Wall;
	extern FArray1D_int PosZ_Floor;
	extern FArray1D_int PosZ_Ceiling;
	extern FArray1D_int APos_Window;
	extern FArray1D_int APos_Door;
	extern FArray1D_int APos_Internal;
	extern FArray1D_int PosZ_Window;
	extern FArray1D_int PosZ_Door;
	extern FArray1D_int PosZ_Internal;
	// Convection coeficients for the various surfaces
	extern FArray1D< Real64 > HCeiling;
	extern FArray1D< Real64 > HWall;
	extern FArray1D< Real64 > HFloor;
	extern FArray1D< Real64 > HInternal;
	extern FArray1D< Real64 > HWindow;
	extern FArray1D< Real64 > HDoor;

} // DataUCSDSharedData

} // EnergyPlus

#endif
