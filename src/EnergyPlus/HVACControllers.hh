#ifndef HVACControllers_hh_INCLUDED
#define HVACControllers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataAirSystems.hh>
#include <DataGlobals.hh>
#include <DataHVACControllers.hh>
#include <DataRootFinder.hh>

namespace EnergyPlus {

namespace HVACControllers {

	// Using/Aliasing
	using DataAirSystems::DefinePrimaryAirSystem;
	using DataHVACControllers::ControllerSimple_Type;
	using DataHVACControllers::iFirstMode;
	using DataHVACControllers::iLastMode;
	using DataHVACControllers::iModeNone;
	using DataHVACControllers::iNoAction;
	using DataRootFinder::RootFinderDataType;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Number of significant digits to display in error messages for floating-point numbers
	extern Real64 const SomeFloatingPoint;
	extern int const NumSigDigits;

	// Parameters for controls used here
	extern int const iNoControlVariable;
	extern int const iTemperature;
	extern int const iHumidityRatio;
	extern int const iTemperatureAndHumidityRatio;
	extern int const iFlow;

	extern int const CoilType_Cooling;
	extern int const CoilType_Heating;

	extern Array1D_string const ControlVariableTypes;

	// DERIVED TYPE DEFINITIONS

	// Type describing a controller's properties

	// Type describing a controller's runtime statistics over the course of the simulation

	// Type describing an air loop's runtime statistics over the course of the simulation

	// MODULE VARIABLE DECLARATIONS:
	extern int NumControllers; // The number of controllers found in the Input
	extern int NumAirLoopStats; // Same size as NumPrimaryAirSys if controllers
	// are defined, 0 otherwise.
	// all controllers per air loop
	extern Array1D_bool CheckEquipName;

	// Flag set to make sure you get input once
	extern bool GetControllerInputFlag;

	// SUBROUTINE Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Algorithms for the Simple Controller

	// Statistics routines

	// Trace routines for all controllers on each air loop

	// Trace routines for each individual controller

	// Misc routines

	// Types

	struct SolutionTrackerType
	{
		// Members
		bool DefinedFlag; // Flag set to TRUE when tracker is up-to-date. FALSE otherwise.
		Real64 ActuatedValue; // Actuated value
		int Mode; // Operational model of controller

		// Default Constructor
		SolutionTrackerType() :
			DefinedFlag( true ),
			ActuatedValue( 0.0 ),
			Mode( iModeNone )
		{}

		// Member Constructor
		SolutionTrackerType(
			bool const DefinedFlag, // Flag set to TRUE when tracker is up-to-date. FALSE otherwise.
			Real64 const ActuatedValue, // Actuated value
			int const Mode // Operational model of controller
		) :
			DefinedFlag( DefinedFlag ),
			ActuatedValue( ActuatedValue ),
			Mode( Mode )
		{}

	};

	struct ControllerPropsType
	{
		// Members
		std::string ControllerName; // Name of the Controller
		std::string ControllerType; // Type of Controller
		int ControllerType_Num;
		int ControlVar; // The type of control variable being sensed
		int ActuatorVar; // The variable that the controller will act on ie. flow
		int Action; // Controller Action - Reverse or Normal
		// Controller must be initialized to set MinActuated and MaxActuated
		bool InitFirstPass;
		// --------------------
		// Internal data used for optimal restart across successive calls to SimAirLoop()
		// --------------------
		int NumCalcCalls; // Number of Calc() calls since last call to Reset()
		int Mode; // Operational model of controller at current iteration
		// Flag indicating whether the current controller simulation was performed from a cold start
		// or following a speculative warm restart. Set in the ResetController() routine.
		// Used in the CheckController() routine.
		bool DoWarmRestartFlag;
		// Flag used to decide whether or not it is allowed to reuse the intermediate solution from
		// solving the previous controller on the air loop (COLD_START mode only) as the initial guess for
		// the current controller.
		bool ReuseIntermediateSolutionFlag;
		// Flag used to decide whether or not it is possible to reuse the solution from
		// the last call to SimAirLoop() as a possible candidate.
		bool ReusePreviousSolutionFlag;
		// Array of solution trackers. Saved at last call to SimAirLoop() in ManageControllers(iControllerOpEnd)
		// The first tracker is used to track the solution when FirstHVACIteration is TRUE.
		// The second tracker is used to track the solution at FirstHVACIteration is FALSE.
		Array1D< SolutionTrackerType > SolutionTrackers;
		// --------------------
		// Operational limits at min/max avail values for actuated variable and the corresponding sensed values
		// --------------------
		Real64 MaxAvailActuated; // kg/s, The maximum actuated variable currently available.
		// Reset by simulation at each HVAC iteration
		Real64 MaxAvailSensed; // Sensed value at maximum available actuated variable
		Real64 MinAvailActuated; // kg/s, The minimum actuated variable currently available.
		// Reset by simulation at each HVAC iteration
		Real64 MinAvailSensed; // Sensed value at maximum available actuated variable
		// --------------------
		// User input min/max values for actuated variable
		// --------------------
		Real64 MaxVolFlowActuated; // m3/s, From User input the Max amount for the actuated variable
		Real64 MinVolFlowActuated; // m3/s, From User input the Min amount for the actuated variable
		Real64 MaxActuated; // kg/s, From User input the Max amount for the actuated variable
		Real64 MinActuated; // kg/s, From User input the Min amount for the actuated variable
		// --------------------
		// Actuated variable
		// --------------------
		int ActuatedNode; // The node that is acted upon by the controller
		Real64 ActuatedValue; // Value of actuated variable before change by the controller
		Real64 NextActuatedValue; // The new control actuated value
		int ActuatedNodePlantLoopNum; // the plant loop index for the actuated node DSU3
		int ActuatedNodePlantLoopSide; // the plant loop side for the actuated node DSU3
		int ActuatedNodePlantLoopBranchNum; // the plant loop branch num for actuated node DSU3
		// --------------------
		// Sensed variable
		// --------------------
		int SensedNode; // The sensed node number from the grid
		bool IsSetPointDefinedFlag; // If TRUE indicates that the setpoint has been defined and can
		// be used to compute DeltaSensed
		Real64 SetPointValue; // Desired setpoint; set in the SetPoint Manager or computed in Init() routine
		Real64 SensedValue; // The sensed control variable of any type
		Real64 DeltaSensed; // Difference of sensed to setpoint value for calculating proportional gain
		Real64 Offset; // This is the tolerance or droop from the error
		int HumRatCntrlType; // iCtrlVarType_HumRat=4,iCtrlVarType_MaxHumRat=5,iCtrlVarType_MinHumRat=6
		// --------------------
		// Other controller inputs, not yet used
		// --------------------
		std::string LimitType; // Limit type as in HIGH or LOW
		Real64 Range; // The range or hysteresis of the control limit
		Real64 Limit; // The Limit value for a Limit Controller
		// --------------------
		// Trace mechanism
		// --------------------
		int TraceFileUnit; // File unit for individual controller trace file to use if > 0
		bool FirstTraceFlag; // To detect first individual write operation to individual controller trace file
		int BadActionErrCount; // Counts number of incorrect action errors
		int BadActionErrIndex; // index to recurring error structure for bad action error

		// Default Constructor
		ControllerPropsType() :
			ControllerType_Num( ControllerSimple_Type ),
			ControlVar( iNoControlVariable ),
			ActuatorVar( 0 ),
			Action( iNoAction ),
			InitFirstPass( true ),
			NumCalcCalls( 0 ),
			Mode( iModeNone ),
			DoWarmRestartFlag( false ),
			ReuseIntermediateSolutionFlag( false ),
			ReusePreviousSolutionFlag( false ),
			SolutionTrackers( 2 ),
			MaxAvailActuated( 0.0 ),
			MaxAvailSensed( 0.0 ),
			MinAvailActuated( 0.0 ),
			MinAvailSensed( 0.0 ),
			MaxVolFlowActuated( 0.0 ),
			MinVolFlowActuated( 0.0 ),
			MaxActuated( 0.0 ),
			MinActuated( 0.0 ),
			ActuatedNode( 0 ),
			ActuatedValue( 0.0 ),
			NextActuatedValue( 0.0 ),
			ActuatedNodePlantLoopNum( 0 ),
			ActuatedNodePlantLoopSide( 0 ),
			ActuatedNodePlantLoopBranchNum( 0 ),
			SensedNode( 0 ),
			IsSetPointDefinedFlag( false ),
			SetPointValue( 0.0 ),
			SensedValue( 0.0 ),
			DeltaSensed( 0.0 ),
			Offset( 0.0 ),
			HumRatCntrlType( 0 ),
			Range( 0.0 ),
			Limit( 0.0 ),
			TraceFileUnit( 0 ),
			FirstTraceFlag( true ),
			BadActionErrCount( 0 ),
			BadActionErrIndex( 0 )
		{}

		// Member Constructor
		ControllerPropsType(
			std::string const & ControllerName, // Name of the Controller
			std::string const & ControllerType, // Type of Controller
			int const ControllerType_Num,
			int const ControlVar, // The type of control variable being sensed
			int const ActuatorVar, // The variable that the controller will act on ie. flow
			int const Action, // Controller Action - Reverse or Normal
			bool const InitFirstPass,
			int const NumCalcCalls, // Number of Calc() calls since last call to Reset()
			int const Mode, // Operational model of controller at current iteration
			bool const DoWarmRestartFlag,
			bool const ReuseIntermediateSolutionFlag,
			bool const ReusePreviousSolutionFlag,
			Array1< SolutionTrackerType > const & SolutionTrackers,
			Real64 const MaxAvailActuated, // kg/s, The maximum actuated variable currently available.
			Real64 const MaxAvailSensed, // Sensed value at maximum available actuated variable
			Real64 const MinAvailActuated, // kg/s, The minimum actuated variable currently available.
			Real64 const MinAvailSensed, // Sensed value at maximum available actuated variable
			Real64 const MaxVolFlowActuated, // m3/s, From User input the Max amount for the actuated variable
			Real64 const MinVolFlowActuated, // m3/s, From User input the Min amount for the actuated variable
			Real64 const MaxActuated, // kg/s, From User input the Max amount for the actuated variable
			Real64 const MinActuated, // kg/s, From User input the Min amount for the actuated variable
			int const ActuatedNode, // The node that is acted upon by the controller
			Real64 const ActuatedValue, // Value of actuated variable before change by the controller
			Real64 const NextActuatedValue, // The new control actuated value
			int const ActuatedNodePlantLoopNum, // the plant loop index for the actuated node DSU3
			int const ActuatedNodePlantLoopSide, // the plant loop side for the actuated node DSU3
			int const ActuatedNodePlantLoopBranchNum, // the plant loop branch num for actuated node DSU3
			int const SensedNode, // The sensed node number from the grid
			bool const IsSetPointDefinedFlag, // If TRUE indicates that the setpoint has been defined and can
			Real64 const SetPointValue, // Desired setpoint; set in the SetPoint Manager or computed in Init() routine
			Real64 const SensedValue, // The sensed control variable of any type
			Real64 const DeltaSensed, // Difference of sensed to setpoint value for calculating proportional gain
			Real64 const Offset, // This is the tolerance or droop from the error
			int const HumRatCntrlType, // iCtrlVarType_HumRat=4,iCtrlVarType_MaxHumRat=5,iCtrlVarType_MinHumRat=6
			std::string const & LimitType, // Limit type as in HIGH or LOW
			Real64 const Range, // The range or hysteresis of the control limit
			Real64 const Limit, // The Limit value for a Limit Controller
			int const TraceFileUnit, // File unit for individual controller trace file to use if > 0
			bool const FirstTraceFlag, // To detect first individual write operation to individual controller trace file
			int const BadActionErrCount, // Counts number of incorrect action errors
			int const BadActionErrIndex // index to recurring error structure for bad action error
		) :
			ControllerName( ControllerName ),
			ControllerType( ControllerType ),
			ControllerType_Num( ControllerType_Num ),
			ControlVar( ControlVar ),
			ActuatorVar( ActuatorVar ),
			Action( Action ),
			InitFirstPass( InitFirstPass ),
			NumCalcCalls( NumCalcCalls ),
			Mode( Mode ),
			DoWarmRestartFlag( DoWarmRestartFlag ),
			ReuseIntermediateSolutionFlag( ReuseIntermediateSolutionFlag ),
			ReusePreviousSolutionFlag( ReusePreviousSolutionFlag ),
			SolutionTrackers( 2, SolutionTrackers ),
			MaxAvailActuated( MaxAvailActuated ),
			MaxAvailSensed( MaxAvailSensed ),
			MinAvailActuated( MinAvailActuated ),
			MinAvailSensed( MinAvailSensed ),
			MaxVolFlowActuated( MaxVolFlowActuated ),
			MinVolFlowActuated( MinVolFlowActuated ),
			MaxActuated( MaxActuated ),
			MinActuated( MinActuated ),
			ActuatedNode( ActuatedNode ),
			ActuatedValue( ActuatedValue ),
			NextActuatedValue( NextActuatedValue ),
			ActuatedNodePlantLoopNum( ActuatedNodePlantLoopNum ),
			ActuatedNodePlantLoopSide( ActuatedNodePlantLoopSide ),
			ActuatedNodePlantLoopBranchNum( ActuatedNodePlantLoopBranchNum ),
			SensedNode( SensedNode ),
			IsSetPointDefinedFlag( IsSetPointDefinedFlag ),
			SetPointValue( SetPointValue ),
			SensedValue( SensedValue ),
			DeltaSensed( DeltaSensed ),
			Offset( Offset ),
			HumRatCntrlType( HumRatCntrlType ),
			LimitType( LimitType ),
			Range( Range ),
			Limit( Limit ),
			TraceFileUnit( TraceFileUnit ),
			FirstTraceFlag( FirstTraceFlag ),
			BadActionErrCount( BadActionErrCount ),
			BadActionErrIndex( BadActionErrIndex )
		{}

	};

	struct ControllerStatsType
	{
		// Members
		Array1D_int NumCalls; // Number of times this controller operated in each mode
		Array1D_int TotIterations; // Total number of iterations required to solve this controller
		Array1D_int MaxIterations; // Maximum number of iterations required to solve this controller

		// Default Constructor
		ControllerStatsType() :
			NumCalls( {iFirstMode,iLastMode}, 0 ),
			TotIterations( {iFirstMode,iLastMode}, 0 ),
			MaxIterations( {iFirstMode,iLastMode}, 0 )
		{}

		// Member Constructor
		ControllerStatsType(
			Array1_int const & NumCalls, // Number of times this controller operated in each mode
			Array1_int const & TotIterations, // Total number of iterations required to solve this controller
			Array1_int const & MaxIterations // Maximum number of iterations required to solve this controller
		) :
			NumCalls( {iFirstMode,iLastMode}, NumCalls ),
			TotIterations( {iFirstMode,iLastMode}, TotIterations ),
			MaxIterations( {iFirstMode,iLastMode}, MaxIterations )
		{}

	};

	struct AirLoopStatsType
	{
		// Members
		int TraceFileUnit; // File unit for trace file for all controllers on each air loop.
		// Used only if > 0. Same size as NumPrimaryAirSys
		bool FirstTraceFlag; // To detect first trace to air loop trace file
		int NumCalls; // Number of times air loop is simulated (number of calls to SimAirLoop)
		int NumFailedWarmRestarts; // Number of times speculative warm restart was attempted and failed
		int NumSuccessfulWarmRestarts; // Number of times speculative warm restart was attempted and succeeded
		int TotSimAirLoopComponents; // Total number of times the SimAirLoopComponents() routine has been invoked
		int MaxSimAirLoopComponents; // Maximum number of times the SimAirLoopComponents() routine has been invoked
		int TotIterations; // Total number of iterations required to solve the controllers on this air loop
		int MaxIterations; // Maximum number of iterations required to solve the controllers on this air loop
		Array1D< ControllerStatsType > ControllerStats; // Array of statistics for each controller
		// on this air loop

		// Default Constructor
		AirLoopStatsType() :
			FirstTraceFlag( true ),
			NumCalls( 0 ),
			NumFailedWarmRestarts( 0 ),
			NumSuccessfulWarmRestarts( 0 ),
			TotSimAirLoopComponents( 0 ),
			MaxSimAirLoopComponents( 0 ),
			TotIterations( 0 ),
			MaxIterations( 0 )
		{}

		// Member Constructor
		AirLoopStatsType(
			int const TraceFileUnit, // File unit for trace file for all controllers on each air loop.
			bool const FirstTraceFlag, // To detect first trace to air loop trace file
			int const NumCalls, // Number of times air loop is simulated (number of calls to SimAirLoop)
			int const NumFailedWarmRestarts, // Number of times speculative warm restart was attempted and failed
			int const NumSuccessfulWarmRestarts, // Number of times speculative warm restart was attempted and succeeded
			int const TotSimAirLoopComponents, // Total number of times the SimAirLoopComponents() routine has been invoked
			int const MaxSimAirLoopComponents, // Maximum number of times the SimAirLoopComponents() routine has been invoked
			int const TotIterations, // Total number of iterations required to solve the controllers on this air loop
			int const MaxIterations, // Maximum number of iterations required to solve the controllers on this air loop
			Array1< ControllerStatsType > const & ControllerStats // Array of statistics for each controller
		) :
			TraceFileUnit( TraceFileUnit ),
			FirstTraceFlag( FirstTraceFlag ),
			NumCalls( NumCalls ),
			NumFailedWarmRestarts( NumFailedWarmRestarts ),
			NumSuccessfulWarmRestarts( NumSuccessfulWarmRestarts ),
			TotSimAirLoopComponents( TotSimAirLoopComponents ),
			MaxSimAirLoopComponents( MaxSimAirLoopComponents ),
			TotIterations( TotIterations ),
			MaxIterations( MaxIterations ),
			ControllerStats( ControllerStats )
		{}

	};

	// Object Data
	extern Array1D< ControllerPropsType > ControllerProps;
	extern Array1D< RootFinderDataType > RootFinders;
	extern Array1D< AirLoopStatsType > AirLoopStats; // Statistics array to analyze computational profile for

	// Functions

	void
	ManageControllers(
		std::string const & ControllerName,
		int & ControllerIndex,
		bool const FirstHVACIteration,
		int const AirLoopNum, // unused1208
		int const AirLoopPass,
		int const Operation,
		bool & IsConvergedFlag,
		bool & IsUpToDateFlag,
		Optional_bool AllowWarmRestartFlag = _
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetControllerInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	ResetController(
		int const ControlNum,
		bool const FirstHVACIteration,
		bool const DoWarmRestartFlag,
		bool & IsConvergedFlag
	);

	void
	InitController(
		int const ControlNum,
		bool const FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
		bool & IsConvergedFlag
	);

	void
	SizeController( int const ControlNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcSimpleController(
		int const ControlNum,
		bool const FirstHVACIteration,
		bool & IsConvergedFlag,
		bool & IsUpToDateFlag,
		std::string const & ControllerName // used when errors occur
	);

	void
	FindRootSimpleController(
		int const ControlNum,
		bool const FirstHVACIteration,
		bool & IsConvergedFlag,
		bool & IsUpToDateFlag,
		std::string const & ControllerName // used when errors occur
	);

	void
	CheckSimpleController(
		int const ControlNum,
		bool & IsConvergedFlag
	);

	bool
	CheckMinActiveController( int const ControlNum );

	bool
	CheckMaxActiveController( int const ControlNum );

	void
	SaveSimpleController(
		int const ControlNum,
		bool const FirstHVACIteration,
		bool const IsConvergedFlag
	);

	void
	LimitController(
		int & ControlNum, // unused1208
		bool & IsConvergedFlag // unused1208
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Controller Module
	// *****************************************************************************

	void
	UpdateController( int const ControlNum );

	//        End of Update subroutines for the Controller Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Controller Module
	// *****************************************************************************

	void
	ReportController( int const ControlNum ); // unused1208

	//        End of Reporting subroutines for the Controller Module
	// *****************************************************************************

	void
	ExitCalcController(
		int const ControlNum,
		Real64 const NextActuatedValue,
		int const Mode,
		bool & IsConvergedFlag,
		bool & IsUpToDateFlag
	);

	// Beginning of Statistics subroutines for the Controller Module
	// *****************************************************************************

	void
	TrackAirLoopControllers(
		int const AirLoopNum,
		int const WarmRestartStatus,
		int const AirLoopIterMax,
		int const AirLoopIterTot,
		int const AirLoopNumCalls
	);

	void
	TrackAirLoopController(
		int const AirLoopNum, // Air loop index
		int const AirLoopControlNum // Controller index on this air loop
	);

	void
	DumpAirLoopStatistics();

	void
	WriteAirLoopStatistics(
		int const FileUnit,
		DefinePrimaryAirSystem const & ThisPrimaryAirSystem,
		AirLoopStatsType const & ThisAirLoopStats
	);

	// Beginning of Tracing subroutines for the Controller Module
	// *****************************************************************************

	void
	SetupAirLoopControllersTracer( int const AirLoopNum );

	void
	TraceAirLoopControllers(
		bool const FirstHVACIteration,
		int const AirLoopNum,
		int const AirLoopPass,
		bool const AirLoopConverged,
		int const AirLoopNumCalls
	);

	void
	TraceIterationStamp(
		int const TraceFileUnit,
		bool const FirstHVACIteration,
		int const AirLoopPass,
		bool const AirLoopConverged,
		int const AirLoopNumCalls
	);

	void
	TraceAirLoopController(
		int const TraceFileUnit,
		int const ControlNum
	);

	void
	SetupIndividualControllerTracer( int const ControlNum );

	void
	TraceIndividualController(
		int const ControlNum,
		bool const FirstHVACIteration,
		int const AirLoopPass,
		int const Operation, // Operation to execute
		bool const IsConvergedFlag
	);

	std::string
	CreateHVACTimeString();

	std::string
	CreateHVACStepFullString();

	std::string
	MakeHVACTimeIntervalString();

	//        End of Tracing subroutines for the Controller Module

	void
	CheckControllerListOrder();

	void
	CheckCoilWaterInletNode(
		int const WaterInletNodeNum, // input actuator node number
		bool & NodeNotFound // true if matching actuator node not found
	);

	void
	GetControllerActuatorNodeNum(
		std::string const & ControllerName, // name of coil controller
		int & WaterInletNodeNum, // input actuator node number
		bool & NodeNotFound // true if matching actuator node not found
	);

	// *****************************************************************************

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

} // HVACControllers

} // EnergyPlus

#endif
