// EnergyPlus Headers
#include <DataDefineEquip.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataDefineEquip {

	// MODULE INFORMATION
	//             AUTHOR:  Russ Taylor
	//       DATE WRITTEN:  Sept 1997

	// PURPOSE OF THIS MODULE:
	// This module  contains the essential coil information that is needed by water and air
	// loop managers as well as the coil simulations

	// METHODOLOGY EMPLOYED:
	// Needs description, as appropriate.

	// REFERENCES: none

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	//MODULE PARAMETER DEFINITIONS
	int const MaxZoneAirComponents( 1 );
	//INTEGER, PARAMETER :: MaxZoneAirControls = 4
	// Equipment Types covered by ZoneAirLoopEquipment:
	int const DualDuctConstVolume( 1 );
	int const DualDuctVAV( 2 );
	int const SingleDuctVAVReheat( 3 );
	int const SingleDuctConstVolReheat( 4 );
	int const SingleDuctVAVNoReheat( 5 );
	int const SingleDuct_SeriesPIU_Reheat( 6 );
	int const SingleDuct_ParallelPIU_Reheat( 7 );
	int const SingleDuct_ConstVol_4PipeInduc( 8 );
	int const SingleDuctVAVReheatVSFan( 9 );
	int const SingleDuctCBVAVReheat( 10 );
	int const SingleDuctCBVAVNoReheat( 11 );
	int const SingleDuctConstVolCooledBeam( 12 );
	int const DualDuctVAVOutdoorAir( 13 );
	int const SingleDuctUserDefined( 14 );
	int const SingleDuctInletATMixer( 15 );
	int const SingleDuctSupplyATMixer( 16 );
	int const SingleDuctConstVolFourPipeBeam( 17 );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	// components of air system
	int NumAirDistUnits( 0 );

	// Object Data
	Array1D< ZoneAirEquip > AirDistUnit; // Used to specify zone related

	// Clears the global data in DataDefineEquip.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumAirDistUnits = 0;
		AirDistUnit.deallocate();
	}

} // DataDefineEquip

} // EnergyPlus
