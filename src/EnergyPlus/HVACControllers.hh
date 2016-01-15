// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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

	};

	// Object Data
	extern Array1D< ControllerPropsType > ControllerProps;
	extern Array1D< RootFinderDataType > RootFinders;
	extern Array1D< AirLoopStatsType > AirLoopStats; // Statistics array to analyze computational profile for

	// Functions

	// Needed for unit tests, should not be normally called.
	void
	clear_state();

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

} // HVACControllers

} // EnergyPlus

#endif
