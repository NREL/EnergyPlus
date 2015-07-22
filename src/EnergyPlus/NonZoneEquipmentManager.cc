// EnergyPlus Headers
#include <NonZoneEquipmentManager.hh>
#include <DataGlobals.hh>
#include <InputProcessor.hh>
#include <WaterThermalTanks.hh>
#include <WaterUse.hh>

namespace EnergyPlus {

namespace NonZoneEquipmentManager {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   January 2004
	//       MODIFIED       Hudson, ORNL July 2007
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// METHODOLOGY EMPLOYED: na

	// REFERENCES: na
	// OTHER NOTES: na
	// USE STATEMENTS: na

	// Data
	// MODULE PARAMETER DEFINITIONS: na
	// MODULE VARIABLE DECLARATIONS: na

	// SUBROUTINE SPECIFICATIONS:

	// MODULE SUBROUTINES:

	// Functions

	void
	ManageNonZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimNonZoneEquipment // Simulation convergence flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 2000
		//       RE-ENGINEERED  Richard Liesen
		//       DATE MODIFIED  February 2003
		//       MODIFIED       Hudson, ORNL July 2007
		//       MODIFIED       B. Grifffith, NREL, April 2008,
		//                      added calls for just heat recovery part of chillers
		//       MODIFIED       Removed much for plant upgrade, 2011

		// PURPOSE OF THIS SUBROUTINE:
		// This routine checks the input file for any non-zone equipment objects and gets their input.
		// Zone equipment objects are generally triggered to "get input" when they are called for simulation
		// by the ZoneEquipmentManager because they are referenced by a Zone Equipment List.  In the case of
		// the NonZoneEquipmentManager, it does not yet have a list of non-zone equipment, so it must make
		// one here before it knows what to call for simulation.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using DataGlobals::ZoneSizingCalc;
		using InputProcessor::GetNumObjectsFound;
		using WaterThermalTanks::SimulateWaterHeaterStandAlone;
		using WaterUse::SimulateWaterUse;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterHeaterNum; // Water heater object number
		static int NumOfWaterHeater;
		static bool CountNonZoneEquip( true );

		// FLOW:
		if ( CountNonZoneEquip ) {
			NumOfWaterHeater = GetNumObjectsFound( "WaterHeater:Mixed" ) + GetNumObjectsFound( "WaterHeater:Stratified" );
			CountNonZoneEquip = false;
		}

		SimulateWaterUse( FirstHVACIteration ); // simulate non-plant loop water use.

		if ( ! ZoneSizingCalc ) {
			for ( WaterHeaterNum = 1; WaterHeaterNum <= NumOfWaterHeater; ++WaterHeaterNum ) {
				SimulateWaterHeaterStandAlone( WaterHeaterNum, FirstHVACIteration );
			}
		}

		if ( FirstHVACIteration ) {
			SimNonZoneEquipment = true;
		} else {
			SimNonZoneEquipment = false;
		}

	}

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

} // NonZoneEquipmentManager

} // EnergyPlus
