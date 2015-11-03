#ifndef EMSManager_hh_INCLUDED
#define EMSManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

//note there are routines that lie outside of the Module at the end of this file

namespace EMSManager {

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const iTemperatureSetPoint; // integer for node setpoint control type
	extern int const iTemperatureMinSetPoint; // integer for node setpoint control type
	extern int const iTemperatureMaxSetPoint; // integer for node setpoint control type
	extern int const iHumidityRatioSetPoint; // integer for node setpoint control type
	extern int const iHumidityRatioMinSetPoint; // integer for node setpoint control type
	extern int const iHumidityRatioMaxSetPoint; // integer for node setpoint control type
	extern int const iMassFlowRateSetPoint; // integer for node setpoint control type
	extern int const iMassFlowRateMinSetPoint; // integer for node setpoint control type
	extern int const iMassFlowRateMaxSetPoint; // integer for node setpoint control type

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetEMSUserInput; // Flag to prevent input from being read multiple times
	extern bool ZoneThermostatActuatorsHaveBeenSetup;
	extern bool FinishProcessingUserInput; // Flag to indicate still need to process input

	// SUBROUTINE SPECIFICATIONS:

	// Functions
	void
	clear_state();
	
	void
	CheckIfAnyEMS();

	// MODULE SUBROUTINES:

	void
	ManageEMS(
		int const iCalledFrom, // indicates where subroutine was called from, parameters in DataGlobals.
		Optional_int_const ProgramManagerToRun = _ // specific program manager to run
	);

	void
	InitEMS( int const iCalledFrom ); // indicates where subroutine was called from, parameters in DataGlobals.

	void
	ReportEMS();

	void
	GetEMSInput();

	void
	ProcessEMSInput( bool const reportErrors ); // .  If true, then report out errors ,otherwise setup what we can

	void
	GetVariableTypeAndIndex(
		std::string const & VarName,
		std::string const & VarKeyName,
		int & VarType,
		int & VarIndex
	);

	void
	EchoOutActuatorKeyChoices();

	void
	EchoOutInternalVariableChoices();

	void
	SetupNodeSetPointsAsActuators();

	void
	UpdateEMSTrendVariables();

	void
	CheckIfNodeSetPointManagedByEMS(
		int const NodeNum, // index of node being checked.
		int const SetPointType,
		bool & ErrorFlag
	);

	void
	SetupPrimaryAirSystemAvailMgrAsActuators();

	void
	SetupWindowShadingControlActuators();

	void
	SetupThermostatActuators();

	void
	SetupSurfaceConvectionActuators();

	void
	SetupSurfaceConstructionActuators();

	void
	SetupSurfaceOutdoorBoundaryConditionActuators();

	void
	SetupZoneInfoAsInternalDataAvail();

} // EMSManager

//Moved these setup EMS actuator routines out of module to solve circular use problems between
//  ScheduleManager and OutputProcessor. Followed pattern used for SetupOutputVariable

void
SetupEMSActuator(
	std::string const & cComponentTypeName,
	std::string const & cUniqueIDName,
	std::string const & cControlTypeName,
	std::string const & cUnits,
	bool & lEMSActuated,
	Real64 & rValue
);

void
SetupEMSActuator(
	std::string const & cComponentTypeName,
	std::string const & cUniqueIDName,
	std::string const & cControlTypeName,
	std::string const & cUnits,
	bool & lEMSActuated,
	int & iValue
);

void
SetupEMSActuator(
	std::string const & cComponentTypeName,
	std::string const & cUniqueIDName,
	std::string const & cControlTypeName,
	std::string const & cUnits,
	bool & lEMSActuated,
	bool & lValue
);

void
SetupEMSInternalVariable(
	std::string const & cDataTypeName,
	std::string const & cUniqueIDName,
	std::string const & cUnits,
	Real64 & rValue
);

void
SetupEMSInternalVariable(
	std::string const & cDataTypeName,
	std::string const & cUniqueIDName,
	std::string const & cUnits,
	int & iValue
);

} // EnergyPlus

#endif
