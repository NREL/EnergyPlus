// EnergyPlus Headers
#include <DataAirLoop.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataAirLoop {

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   November 2003
	//       MODIFIED       L. Gu, Jan. 24, 2007. Add more variables to get information on OnOff fan operation
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module contains type definitions and variables
	// associated with HVAC air loops (AirLoopHVAC objects).

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	int NumOASystems( 0 ); // Number of Outdoor Air Systems
	int LoopFanOperationMode( 0 ); // OnOff fan operation mode
	Real64 LoopSystemOnMassFlowrate( 0.0 ); // Loop mass flow rate during on cycle using an OnOff fan
	Real64 LoopSystemOffMassFlowrate( 0.0 ); // Loop mass flow rate during off cycle using an OnOff fan
	Real64 LoopOnOffFanPartLoadRatio( 0.0 ); // OnOff fan part load ratio
	Real64 LoopHeatingCoilMaxRTF( 0.0 ); // Maximum run time fraction for electric or gas heating coil in an HVAC Air Loop
	Real64 LoopOnOffFanRTF( 0.0 ); // OnOff fan run time fraction in an HVAC Air Loop
	Real64 LoopDXCoilRTF( 0.0 ); // OnOff fan run time fraction in an HVAC Air Loop
	Real64 LoopCompCycRatio( 0.0 ); // Loop compressor cycling ratio for multispeed heat pump
	bool AirLoopInputsFilled( false ); // Set to TRUE after first pass through air loop

	// Object Data
	Array1D< AirLoopZoneEquipConnectData > AirToZoneNodeInfo;
	Array1D< AirLoopOutsideAirConnectData > AirToOANodeInfo;
	Array1D< DefinePriAirSysAvailMgrs > PriAirSysAvailMgr;
	Array1D< AirLooptoZoneData > AirLoopZoneInfo;
	Array1D< AirLoopControlData > AirLoopControlInfo;
	Array1D< AirLoopFlowData > AirLoopFlow;
	Array1D< OAControllerData > OAControllerInfo;
	Array1D< OutsideAirSysProps > OutsideAirSys;

	// Clears the global data in DataAirLoop.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumOASystems = 0;
		LoopFanOperationMode = 0;
		LoopSystemOnMassFlowrate = 0.0;
		LoopSystemOffMassFlowrate = 0.0;
		LoopOnOffFanPartLoadRatio = 0.0;
		LoopHeatingCoilMaxRTF = 0.0;
		LoopOnOffFanRTF = 0.0;
		LoopDXCoilRTF = 0.0;
		LoopCompCycRatio = 0.0;
		AirLoopInputsFilled = false;
		AirToZoneNodeInfo.deallocate();
		AirToOANodeInfo.deallocate();
		PriAirSysAvailMgr.deallocate();
		AirLoopZoneInfo.deallocate();
		AirLoopControlInfo.deallocate();
		AirLoopFlow.deallocate();
		OAControllerInfo.deallocate();
		OutsideAirSys.deallocate();
	}

} // DataAirLoop

} // EnergyPlus
