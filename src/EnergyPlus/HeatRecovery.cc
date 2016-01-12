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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <HeatRecovery.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatRecovery {

	// Module containing the routines dealing with heat recovery from exhaust or relief air

	// MODULE INFORMATION:
	//       AUTHOR         Michael Wetter
	//       DATE WRITTEN   March 1999
	//       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003, R. Raustad April 2003
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and routines required to model heat
	// recovery components in the EnergyPlus HVAC simulation

	// METHODOLOGY EMPLOYED:
	// Heat exchanger effectiveness - NTU models are used.

	// REFERENCES:
	// M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger,LBNL Report 42354, 1999.
	// ARI Standard 1060-2001,Rating Air-to-Air Heat Exchangers for Energy Recovery Ventilation Equipment, www.ari.org
	// ASHRAE Standard 84, Method of Testing Air-To-Air Heat Exchangers, www.ashrae.org
	// U.S. Environmental Protection Agency software "SAVES" -
	//  School Advanced Ventilation Engineering Software http://www.epa.gov/iaq/schooldesign/saves.html

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataHVACGlobals;
	using DataGlobals::WarmupFlag;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::DisplayExtraWarnings;
	using namespace DataLoopNode;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::CurMnDy;
	using DataEnvironment::EnvironmentName;
	using DataEnvironment::StdRhoAir;
	using InputProcessor::SameString;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using General::SolveRegulaFalsi;
	using General::RoundSigDigits;
	using namespace Psychrometrics;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	Real64 const KELVZERO( 273.16 );
	Real64 const SMALL( 1.e-10 );

	// Heat exchanger performance data type
	int const BALANCEDHX_PERFDATATYPE1( 1 );

	// Heat exchanger configurations
	int const Counter_Flow( 1 );
	int const Parallel_Flow( 2 );
	int const Cross_Flow_Both_Unmixed( 3 );
	int const Cross_Flow_Other( 4 );

	// Heat exchanger configuration types
	int const Plate( 1 );
	int const Rotary( 2 );

	// Economizer lockout operation
	int const EconoLockOut_No( 0 );
	int const EconoLockOut_Yes( 1 );

	static std::string const BlankString;

	namespace {
		bool MyOneTimeAllocate( true );
	}

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumHeatExchangers( 0 ); // number of heat exchangers
	int NumAirToAirPlateExchs( 0 ); // number of air to air plate heat exchangers
	int NumAirToAirGenericExchs( 0 ); // number of air to air generic heat exchangers
	int NumDesiccantBalancedExchs( 0 ); // number of desiccant balanced heat exchangers
	int NumDesBalExchsPerfDataType1( 0 ); // number of desiccant balanced heat exchanger performance data maps
	Real64 FullLoadOutAirTemp( 0.0 ); // Used with desiccant HX empirical model, water coils use inlet node condition
	// DX coils use DXCoilFullLoadOutAirTemp when coil is ON otherwise inlet node
	Real64 FullLoadOutAirHumRat( 0.0 ); // Used with desiccant HX empirical model, water coils use inlet node condition
	// DX coils use DXCoilFullLoadOutAirHumRat when coil is ON otherwise inlet node
	bool GetInputFlag( true ); // First time, input is "gotten"
	bool CalledFromParentObject( true ); // Indicates that HX is called from parent object (this object is not on a branch)
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Sizing routine for the module

	// Update routines to check convergence and update nodes

	// Common routines

	// External function calls

	// Object Data
	Array1D< HeatExchCond > ExchCond;
	Array1D< BalancedDesDehumPerfData > BalDesDehumPerfData;

	// Functions

	void clear_state() {
		NumHeatExchangers = 0;
		NumAirToAirPlateExchs = 0;
		NumAirToAirGenericExchs = 0;
		NumDesiccantBalancedExchs = 0;
		NumDesBalExchsPerfDataType1 = 0;
		FullLoadOutAirTemp = 0.0;
		FullLoadOutAirHumRat = 0.0;
		GetInputFlag = true;
		CalledFromParentObject = true;
		CheckEquipName.deallocate();
		ExchCond.deallocate();
		BalDesDehumPerfData.deallocate();
		MyOneTimeAllocate = true;
	}

	void
	SimHeatRecovery(
		std::string const & CompName, // name of the heat exchanger unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex, // Pointer to Component
		int const FanOpMode, // Supply air fan operating mode
		Optional< Real64 const > HXPartLoadRatio, // Part load ratio requested of DX compressor
		Optional_bool_const HXUnitEnable, // Flag to operate heat exchanger
		Optional_int_const CompanionCoilIndex, // index of companion cooling coil
		Optional_bool_const RegenInletIsOANode, // flag to determine if supply inlet is OA node, if so air flow cycles
		Optional_bool_const EconomizerFlag, // economizer operation flag passed by airloop or OA sys
		Optional_bool_const HighHumCtrlFlag // high humidity control flag passed by airloop or OA sys
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000, R. Raustad FSEC - Feb 2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manage the simulation of a heat recovery unit

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// NA

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatExchNum; // index of unit being simulated
		bool HXUnitOn; // flag to enable heat exchanger
		//unused0509  INTEGER      :: FanModeOperation          ! supply air fan operating mode
		Real64 PartLoadRatio; // Part load ratio requested of DX compressor
		bool RegInIsOANode; // local variable to set RegenInletIsOANode optional argument
		int CompanionCoilNum; // Index to companion cooling coil

		if ( GetInputFlag ) {
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		// Find the correct unit index
		if ( CompIndex == 0 ) {
			HeatExchNum = FindItemInList( CompName, ExchCond );
			if ( HeatExchNum == 0 ) {
				ShowFatalError( "SimHeatRecovery: Unit not found=" + CompName );
			}
			CompIndex = HeatExchNum;
		} else {
			HeatExchNum = CompIndex;
			if ( HeatExchNum > NumHeatExchangers || HeatExchNum < 1 ) {
				ShowFatalError( "SimHeatRecovery:  Invalid CompIndex passed=" + TrimSigDigits( HeatExchNum ) + ", Number of Units=" + TrimSigDigits( NumHeatExchangers ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( HeatExchNum ) ) {
				if ( CompName != ExchCond( HeatExchNum ).Name ) {
					ShowFatalError( "SimHeatRecovery: Invalid CompIndex passed=" + TrimSigDigits( HeatExchNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + ExchCond( HeatExchNum ).Name );
				}
				CheckEquipName( HeatExchNum ) = false;
			}
		}

		if ( present( CompanionCoilIndex ) ) {
			CompanionCoilNum = CompanionCoilIndex;
		} else {
			CompanionCoilNum = 0;
		}

		if ( present( HXUnitEnable ) ) {
			HXUnitOn = HXUnitEnable;
			//   When CalledFromParentObject is TRUE, this SIM routine was called by a parent object that passed in HXUnitEnable.
			//   HX will use the DX coil part-load ratio (optional CompanionCoilIndex must be present) or PLR passed in if
			//   not used with DX coil (optional CompanionCoilIndex must not be present).
			CalledFromParentObject = true;
		} else {
			//   HX is placed on a BRANCH, optional arguments are not passed in from SimAirServingZones.
			//   HX will calculate its own part-load ratio if optional HXUnitEnable flag is not present
			HXUnitOn = true;
			CalledFromParentObject = false;
		}

		InitHeatRecovery( HeatExchNum, CompanionCoilNum );

		// call the correct heat exchanger calculation routine
		{ auto const SELECT_CASE_var( ExchCond( HeatExchNum ).ExchTypeNum );

		if ( SELECT_CASE_var == HX_AIRTOAIR_FLATPLATE ) {

			CalcAirToAirPlateHeatExch( HeatExchNum, HXUnitOn, EconomizerFlag, HighHumCtrlFlag );

		} else if ( SELECT_CASE_var == HX_AIRTOAIR_GENERIC ) {

			CalcAirToAirGenericHeatExch( HeatExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag, HXPartLoadRatio );

		} else if ( SELECT_CASE_var == HX_DESICCANT_BALANCED ) {

			if ( present( HXPartLoadRatio ) ) {
				PartLoadRatio = HXPartLoadRatio;
			} else {
				PartLoadRatio = 1.0;
			}

			if ( present( RegenInletIsOANode ) ) {
				RegInIsOANode = RegenInletIsOANode;
			} else {
				RegInIsOANode = false;
			}

			CalcDesiccantBalancedHeatExch( HeatExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, PartLoadRatio, CompanionCoilNum, RegInIsOANode, EconomizerFlag, HighHumCtrlFlag );

		}}

		UpdateHeatRecovery( HeatExchNum );

		ReportHeatRecovery( HeatExchNum );

	}

	void
	GetHeatRecoveryInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003, R. Raustad FSEC - Feb 2009 (EconoLockout inputs)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for heat recovery units and stores it in
		// appropriate data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ExchIndex; // loop index
		int ExchNum; // current heat exchanger number
		int PerfDataIndex; // desiccant balance heat exchanger performance data loop index
		int PerfDataNum; // current desiccant balanced heat exchanger performance data set number
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static std::string HeatExchPerfType; // Desiccant balanced heat exchanger performance data type
		static std::string const RoutineName( "GetHeatRecoveryInput: " ); // include trailing blank space

		NumAirToAirPlateExchs = GetNumObjectsFound( "HeatExchanger:AirToAir:FlatPlate" );
		NumAirToAirGenericExchs = GetNumObjectsFound( "HeatExchanger:AirToAir:SensibleAndLatent" );
		NumDesiccantBalancedExchs = GetNumObjectsFound( "HeatExchanger:Desiccant:BalancedFlow" );
		NumDesBalExchsPerfDataType1 = GetNumObjectsFound( "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1" );
		NumHeatExchangers = NumAirToAirPlateExchs + NumAirToAirGenericExchs + NumDesiccantBalancedExchs;

		// allocate the data array
		ExchCond.allocate( NumHeatExchangers );
		CheckEquipName.dimension( NumHeatExchangers, true );

		if ( NumDesBalExchsPerfDataType1 > 0 ) {
			BalDesDehumPerfData.allocate( NumDesBalExchsPerfDataType1 );
		}

		// loop over the air to air plate heat exchangers and load their input data
		for ( ExchIndex = 1; ExchIndex <= NumAirToAirPlateExchs; ++ExchIndex ) {
			cCurrentModuleObject = "HeatExchanger:AirToAir:FlatPlate";
			GetObjectItem( cCurrentModuleObject, ExchIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ExchNum = ExchIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ExchCond, ExchNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			ExchCond( ExchNum ).Name = cAlphaArgs( 1 );
			ExchCond( ExchNum ).ExchTypeNum = HX_AIRTOAIR_FLATPLATE;
			if ( lAlphaFieldBlanks( 2 ) ) {
				ExchCond( ExchNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				ExchCond( ExchNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( ExchCond( ExchNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
			{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );
			if ( SELECT_CASE_var == "COUNTERFLOW" ) {
				ExchCond( ExchNum ).FlowArr = Counter_Flow;
			} else if ( SELECT_CASE_var == "PARALLELFLOW" ) {
				ExchCond( ExchNum ).FlowArr = Parallel_Flow;
			} else if ( SELECT_CASE_var == "CROSSFLOWBOTHUNMIXED" ) {
				ExchCond( ExchNum ).FlowArr = Cross_Flow_Both_Unmixed;
			} else {
				ShowSevereError( cCurrentModuleObject + ": incorrect flow arrangement: " + cAlphaArgs( 3 ) );
				ErrorsFound = true;
			}}
			{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) );
			if ( SELECT_CASE_var == "YES" ) {
				ExchCond( ExchNum ).EconoLockOut = EconoLockOut_Yes;
			} else if ( SELECT_CASE_var == "NO" ) {
				ExchCond( ExchNum ).EconoLockOut = EconoLockOut_No;
			} else {
				if ( lAlphaFieldBlanks( 4 ) ) {
					ExchCond( ExchNum ).EconoLockOut = EconoLockOut_Yes;
				} else {
					ShowSevereError( cCurrentModuleObject + ": incorrect econo lockout: " + cAlphaArgs( 4 ) );
					ErrorsFound = true;
				}
			}}
			ExchCond( ExchNum ).hARatio = rNumericArgs( 1 );
			ExchCond( ExchNum ).NomSupAirVolFlow = rNumericArgs( 2 );
			ExchCond( ExchNum ).NomSupAirInTemp = rNumericArgs( 3 );
			ExchCond( ExchNum ).NomSupAirOutTemp = rNumericArgs( 4 );
			ExchCond( ExchNum ).NomSecAirVolFlow = rNumericArgs( 5 );
			ExchCond( ExchNum ).NomSecAirInTemp = rNumericArgs( 6 );
			ExchCond( ExchNum ).NomElecPower = rNumericArgs( 7 );
			ExchCond( ExchNum ).SupInletNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ExchCond( ExchNum ).SupOutletNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			ExchCond( ExchNum ).SecInletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			ExchCond( ExchNum ).SecOutletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			TestCompSet( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name, cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Process Air Nodes" );

		} // end of input loop over air to air plate heat exchangers

		// loop over the air to air generic heat exchangers and load their input data
		for ( ExchIndex = 1; ExchIndex <= NumAirToAirGenericExchs; ++ExchIndex ) {
			cCurrentModuleObject = "HeatExchanger:AirToAir:SensibleAndLatent";
			GetObjectItem( cCurrentModuleObject, ExchIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ExchNum = ExchIndex + NumAirToAirPlateExchs;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ExchCond, ExchNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			ExchCond( ExchNum ).Name = cAlphaArgs( 1 );
			ExchCond( ExchNum ).ExchTypeNum = HX_AIRTOAIR_GENERIC;
			if ( lAlphaFieldBlanks( 2 ) ) {
				ExchCond( ExchNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				ExchCond( ExchNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( ExchCond( ExchNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
			ExchCond( ExchNum ).NomSupAirVolFlow = rNumericArgs( 1 );
			ExchCond( ExchNum ).HeatEffectSensible100 = rNumericArgs( 2 );
			ExchCond( ExchNum ).HeatEffectLatent100 = rNumericArgs( 3 );
			ExchCond( ExchNum ).HeatEffectSensible75 = rNumericArgs( 4 );
			ExchCond( ExchNum ).HeatEffectLatent75 = rNumericArgs( 5 );
			if ( ExchCond( ExchNum ).HeatEffectSensible75 < ExchCond( ExchNum ).HeatEffectSensible100 ) {
				ShowWarningError( cCurrentModuleObject + " \"" + ExchCond( ExchNum ).Name + "\" sensible heating effectiveness at 75% rated flow is less than at 100% rated flow." );
				ShowContinueError( "Sensible heating effectiveness at 75% rated flow is usually greater than at 100% rated flow." );
			}
			if ( ExchCond( ExchNum ).HeatEffectLatent75 < ExchCond( ExchNum ).HeatEffectLatent100 ) {
				ShowWarningError( cCurrentModuleObject + " \"" + ExchCond( ExchNum ).Name + "\" latent heating effectiveness at 75% rated flow is less than at 100% rated flow." );
				ShowContinueError( "Latent heating effectiveness at 75% rated flow is usually greater than at 100% rated flow." );
			}
			ExchCond( ExchNum ).CoolEffectSensible100 = rNumericArgs( 6 );
			ExchCond( ExchNum ).CoolEffectLatent100 = rNumericArgs( 7 );
			ExchCond( ExchNum ).CoolEffectSensible75 = rNumericArgs( 8 );
			ExchCond( ExchNum ).CoolEffectLatent75 = rNumericArgs( 9 );
			if ( ExchCond( ExchNum ).CoolEffectSensible75 < ExchCond( ExchNum ).CoolEffectSensible100 ) {
				ShowWarningError( cCurrentModuleObject + " \"" + ExchCond( ExchNum ).Name + "\" sensible cooling effectiveness at 75% rated flow is less than at 100% rated flow." );
				ShowContinueError( "Sensible cooling effectiveness at 75% rated flow is usually greater than at 100% rated flow." );
			}
			if ( ExchCond( ExchNum ).CoolEffectLatent75 < ExchCond( ExchNum ).CoolEffectLatent100 ) {
				ShowWarningError( cCurrentModuleObject + " \"" + ExchCond( ExchNum ).Name + "\" latent cooling effectiveness at 75% rated flow is less than at 100% rated flow." );
				ShowContinueError( "Latent cooling effectiveness at 75% rated flow is usually greater than at 100% rated flow." );
			}
			ExchCond( ExchNum ).SupInletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ExchCond( ExchNum ).SupOutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			ExchCond( ExchNum ).SecInletNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			ExchCond( ExchNum ).SecOutletNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			ExchCond( ExchNum ).NomElecPower = rNumericArgs( 10 );

			if ( SameString( cAlphaArgs( 7 ), "Yes" ) ) {
				ExchCond( ExchNum ).ControlToTemperatureSetPoint = true;
			} else {
				if ( ! SameString( cAlphaArgs( 7 ), "No" ) ) {
					ShowSevereError( "Rotary HX Speed Modulation or Plate Bypass for Temperature Control for " );
					ShowContinueError( ExchCond( ExchNum ).Name + " must be set to Yes or No" );
					ErrorsFound = true;
				}
			}

			if ( SameString( cAlphaArgs( 8 ), "Plate" ) ) {
				ExchCond( ExchNum ).ExchConfigNum = Plate;
			} else if ( SameString( cAlphaArgs( 8 ), "Rotary" ) ) {
				ExchCond( ExchNum ).ExchConfigNum = Rotary;
			} else {
				ShowSevereError( cCurrentModuleObject + " configuration not found= " + cAlphaArgs( 8 ) );
				ShowContinueError( "HX configuration must be either Plate or Rotary" );
				ErrorsFound = true;
			}

			// Added additional inputs for frost control
			ExchCond( ExchNum ).FrostControlType = cAlphaArgs( 9 );
			if ( ! SameString( ExchCond( ExchNum ).FrostControlType, "None" ) ) {
				if ( ! SameString( ExchCond( ExchNum ).FrostControlType, "ExhaustOnly" ) ) {
					if ( ! SameString( ExchCond( ExchNum ).FrostControlType, "ExhaustAirRecirculation" ) ) {
						if ( ! SameString( ExchCond( ExchNum ).FrostControlType, "MinimumExhaustTemperature" ) ) {
							ShowSevereError( "Invalid Frost Control method for " + ExchCond( ExchNum ).Name + " =  " + cAlphaArgs( 9 ) );
							ErrorsFound = true;
						}
					}
				}
			}

			if ( ! SameString( cAlphaArgs( 9 ), "None" ) ) {
				ExchCond( ExchNum ).ThresholdTemperature = rNumericArgs( 11 );
				ExchCond( ExchNum ).InitialDefrostTime = rNumericArgs( 12 );
				ExchCond( ExchNum ).RateofDefrostTimeIncrease = rNumericArgs( 13 );
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 10 ) );
			if ( SELECT_CASE_var == "YES" ) {
				ExchCond( ExchNum ).EconoLockOut = EconoLockOut_Yes;
			} else if ( SELECT_CASE_var == "NO" ) {
				ExchCond( ExchNum ).EconoLockOut = EconoLockOut_No;
			} else {
				if ( lAlphaFieldBlanks( 10 ) ) {
					ExchCond( ExchNum ).EconoLockOut = EconoLockOut_Yes;
				} else {
					ShowSevereError( cCurrentModuleObject + ": incorrect econo lockout: " + cAlphaArgs( 10 ) );
					ErrorsFound = true;
				}
			}}

			TestCompSet( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name, cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Process Air Nodes" );
		} // end of input loop over air to air generic heat exchangers

		// loop over the desiccant balanced heat exchangers and load their input data
		for ( ExchIndex = 1; ExchIndex <= NumDesiccantBalancedExchs; ++ExchIndex ) {
			cCurrentModuleObject = "HeatExchanger:Desiccant:BalancedFlow";
			GetObjectItem( cCurrentModuleObject, ExchIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ExchNum = ExchIndex + NumAirToAirPlateExchs + NumAirToAirGenericExchs;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ExchCond, ExchNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			ExchCond( ExchNum ).Name = cAlphaArgs( 1 );
			ExchCond( ExchNum ).ExchTypeNum = HX_DESICCANT_BALANCED;
			if ( lAlphaFieldBlanks( 2 ) ) {
				ExchCond( ExchNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				ExchCond( ExchNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( ExchCond( ExchNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
			// desiccant HX's usually refer to process and regeneration air streams
			// In this module, Sup = Regeneration nodes and Sec = Process nodes
			// regeneration air inlet and outlet nodes
			ExchCond( ExchNum ).SupInletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ExchCond( ExchNum ).SupOutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			// process air inlet and outlet nodes
			ExchCond( ExchNum ).SecInletNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			ExchCond( ExchNum ).SecOutletNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			// Set up the component set for the process side of the HX (Sec = Process)
			TestCompSet( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name, NodeID( ExchCond( ExchNum ).SecInletNode ), NodeID( ExchCond( ExchNum ).SecOutletNode ), "Process Air Nodes" );

			HeatExchPerfType = cAlphaArgs( 7 );
			if ( SameString( HeatExchPerfType, "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1" ) ) {
				ExchCond( ExchNum ).HeatExchPerfTypeNum = BALANCEDHX_PERFDATATYPE1;
			} else {
				ShowSevereError( cCurrentModuleObject + " \"" + ExchCond( ExchNum ).Name + "\"" );
				ShowContinueError( "Invalid performance data type selected." );
				ShowContinueError( "...performance data type selected = " + HeatExchPerfType );
				ErrorsFound = true;
			}

			ExchCond( ExchNum ).HeatExchPerfName = cAlphaArgs( 8 );

			{ auto const SELECT_CASE_var( cAlphaArgs( 9 ) );
			if ( SELECT_CASE_var == "YES" ) {
				ExchCond( ExchNum ).EconoLockOut = EconoLockOut_Yes;
			} else if ( SELECT_CASE_var == "NO" ) {
				ExchCond( ExchNum ).EconoLockOut = EconoLockOut_No;
			} else {
				if ( lAlphaFieldBlanks( 9 ) ) {
					ExchCond( ExchNum ).EconoLockOut = EconoLockOut_No;
				} else {
					ShowSevereError( cCurrentModuleObject + ": incorrect econo lockout: " + cAlphaArgs( 9 ) );
					ErrorsFound = true;
				}
			}}

		} // end of input loop over desiccant balanced heat exchangers

		// get performance data set for balanced desiccant heat exchanger

		for ( PerfDataIndex = 1; PerfDataIndex <= NumDesBalExchsPerfDataType1; ++PerfDataIndex ) {
			cCurrentModuleObject = "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1";
			GetObjectItem( cCurrentModuleObject, PerfDataIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			PerfDataNum = PerfDataIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), BalDesDehumPerfData, PerfDataNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			BalDesDehumPerfData( PerfDataNum ).Name = cAlphaArgs( 1 );
			BalDesDehumPerfData( PerfDataNum ).PerfType = cCurrentModuleObject;
			BalDesDehumPerfData( PerfDataNum ).NomSupAirVolFlow = rNumericArgs( 1 );
			// check validity
			if ( BalDesDehumPerfData( PerfDataNum ).NomSupAirVolFlow <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Nominal air flow rate must be greater than zero." );
				ShowContinueError( "... value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).NomSupAirVolFlow, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).NomProcAirFaceVel = rNumericArgs( 2 );
			// check validity
			if ( BalDesDehumPerfData( PerfDataNum ).NomProcAirFaceVel <= 0.0 || BalDesDehumPerfData( PerfDataNum ).NomProcAirFaceVel > 6.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Nominal air face velocity cannot be less than or equal to zero or greater than 6 m/s." );
				ShowContinueError( "... value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).NomProcAirFaceVel, 6 ) );
				ErrorsFound = true;
			}
			BalDesDehumPerfData( PerfDataNum ).NomElecPower = rNumericArgs( 3 );
			// check validity
			if ( BalDesDehumPerfData( PerfDataNum ).NomElecPower < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Nominal electric power cannot be less than zero." );
				ShowContinueError( "... value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).NomElecPower, 6 ) );
				ErrorsFound = true;
			}

			// regen outlet temp variables
			BalDesDehumPerfData( PerfDataNum ).B1 = rNumericArgs( 4 );
			BalDesDehumPerfData( PerfDataNum ).B2 = rNumericArgs( 5 );
			BalDesDehumPerfData( PerfDataNum ).B3 = rNumericArgs( 6 );
			BalDesDehumPerfData( PerfDataNum ).B4 = rNumericArgs( 7 );
			BalDesDehumPerfData( PerfDataNum ).B5 = rNumericArgs( 8 );
			BalDesDehumPerfData( PerfDataNum ).B6 = rNumericArgs( 9 );
			BalDesDehumPerfData( PerfDataNum ).B7 = rNumericArgs( 10 );
			BalDesDehumPerfData( PerfDataNum ).B8 = rNumericArgs( 11 );

			//     Check that the minimum is not greater than or equal to the maximum for each of the following model boundaries
			BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInHumRat = rNumericArgs( 12 );
			BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInHumRat = rNumericArgs( 13 );
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInHumRat >= BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInHumRat ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of regeneration inlet air humidity ratio must be less than the maximum." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInHumRat, 6 ) );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInHumRat < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of regeneration inlet air humidity ratio must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInHumRat > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the maximum value of regeneration inlet air humidity ratio must be less than or equal to 1." );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInHumRat, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInTemp = rNumericArgs( 14 );
			BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInTemp = rNumericArgs( 15 );
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInTemp >= BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of regeneration inlet air temperature must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInTemp, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInTemp, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInHumRat = rNumericArgs( 16 );
			BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInHumRat = rNumericArgs( 17 );
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInHumRat >= BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInHumRat ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of process inlet air humidity ratio must be less than the maximum." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInHumRat, 6 ) );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInHumRat < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of process inlet air humidity ratio must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInHumRat > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the maximum value of process inlet air humidity ratio must be less than or equal to 1." );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInHumRat, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInTemp = rNumericArgs( 18 );
			BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInTemp = rNumericArgs( 19 );
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInTemp >= BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of process inlet air temperature must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInTemp, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInTemp, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).T_MinFaceVel = rNumericArgs( 20 );
			BalDesDehumPerfData( PerfDataNum ).T_MaxFaceVel = rNumericArgs( 21 );
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinFaceVel >= BalDesDehumPerfData( PerfDataNum ).T_MaxFaceVel ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of regen air velocity must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinFaceVel, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxFaceVel, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutTemp = rNumericArgs( 22 );
			BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutTemp = rNumericArgs( 23 );
			if ( BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutTemp >= BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of regen outlet air temperature must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutTemp, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutTemp, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInRelHum = rNumericArgs( 24 ) / 100.0;
			BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInRelHum = rNumericArgs( 25 ) / 100.0;
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInRelHum >= BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInRelHum ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of regen inlet air relative humidity must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInRelHum * 100.0, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInRelHum < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of regen inlet air relative humidity must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinRegenAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInRelHum > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the maximum value of regen inlet air relative humidity must be less than or equal to 100." );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxRegenAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInRelHum = rNumericArgs( 26 ) / 100.0;
			BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInRelHum = rNumericArgs( 27 ) / 100.0;
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInRelHum >= BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInRelHum ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of process inlet air relative humidity must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInRelHum * 100.0, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInRelHum < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the minimum value of process inlet air relative humidity must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MinProcAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInRelHum > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air temperature equation." );
				ShowContinueError( "... the maximum value of process inlet air relative humidity must be less than or equal to 100." );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).T_MaxProcAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}

			// regen outlet humidity ratio variables
			BalDesDehumPerfData( PerfDataNum ).C1 = rNumericArgs( 28 );
			BalDesDehumPerfData( PerfDataNum ).C2 = rNumericArgs( 29 );
			BalDesDehumPerfData( PerfDataNum ).C3 = rNumericArgs( 30 );
			BalDesDehumPerfData( PerfDataNum ).C4 = rNumericArgs( 31 );
			BalDesDehumPerfData( PerfDataNum ).C5 = rNumericArgs( 32 );
			BalDesDehumPerfData( PerfDataNum ).C6 = rNumericArgs( 33 );
			BalDesDehumPerfData( PerfDataNum ).C7 = rNumericArgs( 34 );
			BalDesDehumPerfData( PerfDataNum ).C8 = rNumericArgs( 35 );

			//     Check that the minimum is not greater than or equal to the maximum for each of the following model boundaries
			BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInHumRat = rNumericArgs( 36 );
			BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInHumRat = rNumericArgs( 37 );
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInHumRat >= BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInHumRat ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regeneration inlet air humidity ratio must be less than the maximum." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInHumRat, 6 ) );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInHumRat < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regeneration inlet air humidity ratio must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInHumRat > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the maximum value of regeneration inlet air humidity ratio must be less than or equal to 1." );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInHumRat, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInTemp = rNumericArgs( 38 );
			BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInTemp = rNumericArgs( 39 );
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInTemp >= BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regeneration inlet air temperature must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInTemp, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInTemp, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInHumRat = rNumericArgs( 40 );
			BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInHumRat = rNumericArgs( 41 );
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInHumRat >= BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInHumRat ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of process inlet air humidity ratio must be less than the maximum." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInHumRat, 6 ) );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInHumRat < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of process inlet air humidity ratio must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInHumRat > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the maximum value of process inlet air humidity ratio must be less than or equal to 1." );
				ShowContinueError( "... maximum value entered by user = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInHumRat, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInTemp = rNumericArgs( 42 );
			BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInTemp = rNumericArgs( 43 );
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInTemp >= BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of process inlet air temperature must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInTemp, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInTemp, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).H_MinFaceVel = rNumericArgs( 44 );
			BalDesDehumPerfData( PerfDataNum ).H_MaxFaceVel = rNumericArgs( 45 );
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinFaceVel >= BalDesDehumPerfData( PerfDataNum ).H_MaxFaceVel ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regen air velocity must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinFaceVel, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxFaceVel, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutHumRat = rNumericArgs( 46 );
			BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutHumRat = rNumericArgs( 47 );
			if ( BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutHumRat >= BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutHumRat ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regen outlet air humidity ratio must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutHumRat, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutHumRat < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regen outlet air humidity ratio must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).MinRegenAirOutHumRat, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutHumRat > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the maximum value of regen outlet air humidity ratio must be less or equal to 1." );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).MaxRegenAirOutHumRat, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInRelHum = rNumericArgs( 48 ) / 100.0;
			BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInRelHum = rNumericArgs( 49 ) / 100.0;
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInRelHum >= BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInRelHum ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regen inlet air relative humidity must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInRelHum * 100.0, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInRelHum < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of regen inlet air relative humidity must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinRegenAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInRelHum > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the maximum value of regen inlet air relative humidity must be less or equal to 100." );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxRegenAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}

			BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInRelHum = rNumericArgs( 50 ) / 100.0;
			BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInRelHum = rNumericArgs( 51 ) / 100.0;
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInRelHum >= BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInRelHum ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min/max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of process inlet air relative humidity must be less than the maximum." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInRelHum * 100.0, 6 ) );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInRelHum < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in min boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the minimum value of process inlet air relative humidity must be greater than or equal to 0." );
				ShowContinueError( "... minimum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MinProcAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}
			if ( BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInRelHum > 1.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + BalDesDehumPerfData( PerfDataNum ).Name + "\"" );
				ShowContinueError( "Error found in max boundary for the regen outlet air humidity ratio equation." );
				ShowContinueError( "... the maximum value of process inlet air relative humidity must be less than or equal to 100." );
				ShowContinueError( "... maximum value entered = " + RoundSigDigits( BalDesDehumPerfData( PerfDataNum ).H_MaxProcAirInRelHum * 100.0, 6 ) );
				ErrorsFound = true;
			}

		}
		// getting performance data set for balanced desiccant heat exchanger ends

		// match desiccant heat exchanger index to performance data index
		for ( ExchIndex = 1; ExchIndex <= NumDesiccantBalancedExchs; ++ExchIndex ) {
			ExchNum = ExchIndex + NumAirToAirPlateExchs + NumAirToAirGenericExchs;
			for ( PerfDataNum = 1; PerfDataNum <= NumDesBalExchsPerfDataType1; ++PerfDataNum ) {
				if ( SameString( ExchCond( ExchNum ).HeatExchPerfName, BalDesDehumPerfData( PerfDataNum ).Name ) ) {
					ExchCond( ExchNum ).PerfDataIndex = PerfDataNum;
					break;
				}
			}
			if ( ExchCond( ExchNum ).PerfDataIndex == 0 ) {
				ShowSevereError( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + " \"" + ExchCond( ExchNum ).Name + "\"" );
				ShowContinueError( "... Performance data set not found = " + ExchCond( ExchNum ).HeatExchPerfName );
				ErrorsFound = true;
			} else {
				if ( ! ErrorsFound ) {
					ExchCond( ExchNum ).FaceArea = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).NomSupAirVolFlow / ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).NomProcAirFaceVel );
				}
			}
		}
		// matching done

		// setup common report variables for heat exchangers
		for ( ExchIndex = 1; ExchIndex <= NumHeatExchangers; ++ExchIndex ) {
			ExchNum = ExchIndex;
			// CurrentModuleObject='HeatExchanger:AirToAir:FlatPlate/AirToAir:SensibleAndLatent/Desiccant:BalancedFlow')
			SetupOutputVariable( "Heat Exchanger Sensible Heating Rate [W]", ExchCond( ExchNum ).SensHeatingRate, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Sensible Heating Energy [J]", ExchCond( ExchNum ).SensHeatingEnergy, "System", "Sum", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Latent Gain Rate [W]", ExchCond( ExchNum ).LatHeatingRate, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Latent Gain Energy [J]", ExchCond( ExchNum ).LatHeatingEnergy, "System", "Sum", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Total Heating Rate [W]", ExchCond( ExchNum ).TotHeatingRate, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Total Heating Energy [J]", ExchCond( ExchNum ).TotHeatingEnergy, "System", "Sum", ExchCond( ExchNum ).Name, _, "ENERGYTRANSFER", "HEAT RECOVERY FOR HEATING", _, "System" );
			SetupOutputVariable( "Heat Exchanger Sensible Cooling Rate [W]", ExchCond( ExchNum ).SensCoolingRate, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Sensible Cooling Energy [J]", ExchCond( ExchNum ).SensCoolingEnergy, "System", "Sum", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Latent Cooling Rate [W]", ExchCond( ExchNum ).LatCoolingRate, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Latent Cooling Energy [J]", ExchCond( ExchNum ).LatCoolingEnergy, "System", "Sum", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Total Cooling Rate [W]", ExchCond( ExchNum ).TotCoolingRate, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Total Cooling Energy [J]", ExchCond( ExchNum ).TotCoolingEnergy, "System", "Sum", ExchCond( ExchNum ).Name, _, "ENERGYTRANSFER", "HEAT RECOVERY FOR COOLING", _, "System" );

			SetupOutputVariable( "Heat Exchanger Electric Power [W]", ExchCond( ExchNum ).ElecUseRate, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Electric Energy [J]", ExchCond( ExchNum ).ElecUseEnergy, "System", "Sum", ExchCond( ExchNum ).Name, _, "ELECTRICITY", "HEATRECOVERY", _, "System" );
		}

		// setup additional report variables for generic heat exchangers
		for ( ExchIndex = 1; ExchIndex <= NumAirToAirGenericExchs; ++ExchIndex ) {
			// generic heat exchangers are read in after flat plate heat exchanger objects (index needs to be set correctly)
			// CurrentModuleObject=HeatExchanger:AirToAir:SensibleAndLatent
			ExchNum = ExchIndex + NumAirToAirPlateExchs;
			SetupOutputVariable( "Heat Exchanger Sensible Effectiveness []", ExchCond( ExchNum ).SensEffectiveness, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Latent Effectiveness []", ExchCond( ExchNum ).LatEffectiveness, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Supply Air Bypass Mass Flow Rate [kg/s]", ExchCond( ExchNum ).SupBypassMassFlow, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Exhaust Air Bypass Mass Flow Rate [kg/s]", ExchCond( ExchNum ).SecBypassMassFlow, "System", "Average", ExchCond( ExchNum ).Name );
			SetupOutputVariable( "Heat Exchanger Defrost Time Fraction []", ExchCond( ExchNum ).DefrostFraction, "System", "Average", ExchCond( ExchNum ).Name );

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

	}

	void
	InitHeatRecovery(
		int const ExchNum, // number of the current heat exchanger being simulated
		int const CompanionCoilIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003
		//                      B Griffith May 2009, EMS setpoint check
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Heat Recovery Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DXCoils::DXCoilFullLoadOutAirTemp;
		using DXCoils::DXCoilFullLoadOutAirHumRat;
		//  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iHumidityRatioMaxSetPoint;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ExIndex; // do loop index
		int SupInNode; // supply air inlet node number
		int SecInNode; // secondary air inlet node number
		Real64 CMin0; // minimum capacity flow
		Real64 CMax0; // maximum capacity flow
		Real64 Eps0; // effectiveness at rated conditions
		Real64 NTU0; // NTU at rated conditions
		Real64 RhoAir; // air density at outside pressure & standard temperature and humidity
		Real64 CpAir; // heat capacity of air
		// of humidity ratio and temperature
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool MyOneTimeAllocate( true );
		////////////////////////////////////////////////////////////////////////////////////
		static Array1D_bool MySetPointTest;
		static Array1D_bool MySizeFlag;
		int ErrStat; // error status returned by CalculateNTUfromEpsAndZ
		bool FatalError; // fatal error flag
		bool LocalWarningError; // warning error flag
		Real64 Z; // Min/max flow ratio
		//  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .FALSE.  ! True after the Zone Equipment List has been checked for items

		if ( MyOneTimeAllocate ) {
			MySetPointTest.allocate( NumHeatExchangers );
			MySizeFlag.allocate( NumHeatExchangers );
			MySetPointTest = true;
			MySizeFlag = true;
			MyOneTimeAllocate = false;
		}

		if ( ! SysSizingCalc && MySizeFlag( ExchNum ) ) {

			SizeHeatRecovery( ExchNum );
			MySizeFlag( ExchNum ) = false;

		}

		FatalError = false;
		LocalWarningError = false;

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && ExchCond( ExchNum ).myEnvrnFlag ) {
			//I believe that all of these initializations should be taking place at the SCFM conditions
			RhoAir = StdRhoAir;
			//    RhoAir = PsyRhoAirFnPbTdbW(101325.0,20.0,0.0)  do we want standard air density at sea level for generic ERVs per ARI 1060?
			CpAir = PsyCpAirFnWTdb( 0.0, 20.0 );

			ExIndex = ExchNum; // this replaces the loop that went over multiple at once

				{ auto const SELECT_CASE_var( ExchCond( ExIndex ).ExchTypeNum );

				if ( SELECT_CASE_var == HX_AIRTOAIR_FLATPLATE ) {

					ExchCond( ExIndex ).NomSupAirMassFlow = RhoAir * ExchCond( ExIndex ).NomSupAirVolFlow;
					ExchCond( ExIndex ).NomSecAirMassFlow = RhoAir * ExchCond( ExIndex ).NomSecAirVolFlow;
					// Note: the capacity stream is here simply the mass flow
					//       since the thermal capacity can be assumed to be
					//       equal for both streams
					if ( ExchCond( ExIndex ).NomSupAirMassFlow > ExchCond( ExIndex ).NomSecAirMassFlow ) {
						CMin0 = ExchCond( ExIndex ).NomSecAirMassFlow;
						CMax0 = ExchCond( ExIndex ).NomSupAirMassFlow;
					} else {
						CMin0 = ExchCond( ExIndex ).NomSupAirMassFlow;
						CMax0 = ExchCond( ExIndex ).NomSecAirMassFlow;
					}

					Eps0 = ExchCond( ExIndex ).NomSupAirMassFlow * SafeDiv( ExchCond( ExIndex ).NomSupAirOutTemp - ExchCond( ExIndex ).NomSupAirInTemp, CMin0 * ( ExchCond( ExIndex ).NomSecAirInTemp - ExchCond( ExIndex ).NomSupAirInTemp ) );
					Z = CMin0 / CMax0;

					ErrStat = 0;
					CalculateNTUfromEpsAndZ( NTU0, ErrStat, Z, ExchCond( ExIndex ).FlowArr, Eps0 );

					if ( ErrStat == 1 ) {

						FatalError = true;
						ShowSevereError( "In the HeatExchanger:AirToAir:FlatPlate component " + ExchCond( ExIndex ).Name );
						ShowContinueError( "  the mass flow ratio is out of bounds" );
						ShowContinueError( "The mass flow ratio is (Min_Mass_Flow_Rate / Max_Mass_Flow_Rate) = " + RoundSigDigits( Z, 2 ) );
						ShowContinueError( "The mass flow ratio should be >= 0.0 and <= 1.0" );
						ShowContinueError( "Min_Mass_Flow_Rate = " + RoundSigDigits( RhoAir, 2 ) + " [air density] * " + RoundSigDigits( min( ExchCond( ExIndex ).NomSupAirVolFlow, ExchCond( ExIndex ).NomSecAirVolFlow ), 1 ) + " [Min_Vol_Flow_Rate]" );
						ShowContinueError( "Max_Mass_Flow_Rate = " + RoundSigDigits( RhoAir, 2 ) + " [air density] * " + RoundSigDigits( max( ExchCond( ExIndex ).NomSupAirVolFlow, ExchCond( ExIndex ).NomSecAirVolFlow ), 1 ) + " [Max_Vol_Flow_Rate]" );
					} else if ( ErrStat == 2 ) {
						FatalError = true;
						ShowSevereError( "In the HeatExchanger:AirToAir:FlatPlate component " + ExchCond( ExIndex ).Name );
						ShowContinueError( "  the calculated nominal effectiveness is out of bounds" );
						ShowContinueError( "The effectiveness is " + RoundSigDigits( Eps0, 3 ) );
						ShowContinueError( "The effectiveness should be >= 0.0 and <= " + RoundSigDigits( 1.0 / ( 1.0 + Z ), 3 ) );
						ShowContinueError( "Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)/(T_nom_sec_in-T_nom_sup_in)" );
						ShowContinueError( "The temperatures are user inputs. The mass flow rates are user input volume flow rates" );
						ShowContinueError( "  times the density of air [" + RoundSigDigits( RhoAir, 2 ) + " kg/m3]" );
						ShowContinueError( "Change these inputs to obtain a physically realizable heat exchanger effectiveness" );
					} else if ( ErrStat == 3 ) {
						FatalError = true;
						ShowSevereError( "In the HeatExchanger:AirToAir:FlatPlate component " + ExchCond( ExIndex ).Name );
						ShowContinueError( "  the calculated nominal effectiveness is out of bounds" );
						ShowContinueError( "The effectiveness is " + RoundSigDigits( Eps0, 3 ) );
						ShowContinueError( "The effectiveness should be >= 0.0 and <= " + RoundSigDigits( ( 1.0 - std::exp( -Z ) ) / Z, 3 ) );
						ShowContinueError( "Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)/(T_nom_sec_in-T_nom_sup_in)" );
						ShowContinueError( "The temperatures are user inputs. The mass flow rates are user input volume flow rates" );
						ShowContinueError( "  times the density of air [" + RoundSigDigits( RhoAir, 2 ) + " kg/m3]" );
						ShowContinueError( "Change these inputs to obtain a physically realizable heat exchanger effectiveness" );
					} else if ( ErrStat == 4 ) {
						FatalError = true;
						ShowSevereError( "In the HeatExchanger:AirToAir:FlatPlate component " + ExchCond( ExIndex ).Name );
						ShowContinueError( "  the quantity Eff_nom*(Min_Mass_Flow_Rate / Max_Mass_Flow_Rate) is out of bounds" );
						ShowContinueError( "The value is " + RoundSigDigits( Eps0 * Z, 3 ) );
						ShowContinueError( "The value should be >= 0.0 and <= " + RoundSigDigits( 1.0 - std::exp( Z * ( SMALL - 1.0 ) ), 3 ) );
						ShowContinueError( "Eff_nom = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate) * (T_nom_sup_out - T_nom_sup_in)/(T_nom_sec_in - T_nom_sup_in)" );
						ShowContinueError( "The temperatures are user inputs. The mass flow rates are user input volume flow rates" );
						ShowContinueError( "  times the density of air [" + RoundSigDigits( RhoAir, 2 ) + " kg/m3]" );
						ShowContinueError( "Change these inputs to obtain a physically realizable product of effectiveness times min/max mass ratio for this heat exchanger" );
					} else if ( ErrStat == 5 ) {
						FatalError = true;
						ShowSevereError( "In the HeatExchanger:AirToAir:FlatPlate component " + ExchCond( ExIndex ).Name );
						ShowContinueError( "  the calculated nominal effectiveness is out of bounds" );
						ShowContinueError( "The effectiveness is " + RoundSigDigits( Eps0, 3 ) );
						ShowContinueError( "The effectiveness should be >= 0.0 and <= 1.0" );
						ShowContinueError( "Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)/(T_nom_sec_in-T_nom_sup_in)" );
						ShowContinueError( "The temperatures are user inputs. The mass flow rates are user input volume flow rates" );
						ShowContinueError( "  times the density of air [" + RoundSigDigits( RhoAir, 2 ) + " kg/m3]" );
						ShowContinueError( "Change these inputs to obtain a physically realizable heat exchanger effectiveness" );

					}

					if ( FatalError ) {
						ShowFatalError( "Heat exchanger design calculation caused fatal error: program terminated." );
					}

					ExchCond( ExIndex ).UA0 = NTU0 * CMin0 * CpAir;
					ExchCond( ExIndex ).mTSup0 = ExchCond( ExIndex ).NomSupAirMassFlow * ( ExchCond( ExIndex ).NomSupAirInTemp + KELVZERO );
					ExchCond( ExIndex ).mTSec0 = ExchCond( ExIndex ).NomSecAirMassFlow * ( ExchCond( ExIndex ).NomSecAirInTemp + KELVZERO );

					// check validity
					if ( ExchCond( ExIndex ).NomSupAirMassFlow * ExchCond( ExIndex ).NomSecAirMassFlow < SmallMassFlow * SmallMassFlow ) {
						ShowFatalError( "Mass flow in HeatExchanger:AirToAir:FlatPlate too small in initialization." );
					}

					if ( ExchCond( ExIndex ).mTSup0 < SmallMassFlow ) {
						ShowFatalError( "(m*T)Sup,in in HeatExchanger:AirToAir:FlatPlate too small in initialization." );
					}

					if ( ExchCond( ExIndex ).mTSec0 < SmallMassFlow ) {
						ShowFatalError( "(m*T)Sec,in in HeatExchanger:AirToAir:FlatPlate too small in initialization." );
					}

					if ( CMin0 < SmallMassFlow ) {
						ShowFatalError( "CMin0 in HeatExchanger:AirToAir:FlatPlate too small in initialization." );
					}

				} else if ( SELECT_CASE_var == HX_AIRTOAIR_GENERIC ) {

					if ( ExchCond( ExIndex ).SupOutletNode > 0 && ExchCond( ExIndex ).ControlToTemperatureSetPoint ) {
						if ( Node( ExchCond( ExIndex ).SupOutletNode ).TempSetPoint == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( "Missing temperature setpoint for " + cHXTypes( ExchCond( ExIndex ).ExchTypeNum ) + " \"" + ExchCond( ExIndex ).Name + "\" :" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the supply air outlet node of the Heat Exchanger." );
								ShowFatalError( " Previous condition causes program termination." );
							} else {
								// need call to EMS to check node
								CheckIfNodeSetPointManagedByEMS( ExchCond( ExIndex ).SupOutletNode, iTemperatureSetPoint, FatalError );
								if ( FatalError ) {
									ShowSevereError( "Missing temperature setpoint for " + cHXTypes( ExchCond( ExIndex ).ExchTypeNum ) + " \"" + ExchCond( ExIndex ).Name + "\" :" );
									ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the supply air outlet node of the Heat Exchanger." );
									ShowContinueError( "  or use an EMS actuator to establish a setpoint at the supply air outlet node of the Heat Exchanger." );
									ShowFatalError( " Previous condition causes program termination." );
								}
							}
						}
					}

				} else if ( SELECT_CASE_var == HX_DESICCANT_BALANCED ) {

				} else {
					//       Will never get here

				}}

			ExchCond( ExchNum ).myEnvrnFlag = false;

		}

		if ( ! BeginEnvrnFlag ) {
			ExchCond( ExchNum ).myEnvrnFlag = true;
		}

		// Do these initializations every time step
		SupInNode = ExchCond( ExchNum ).SupInletNode;
		SecInNode = ExchCond( ExchNum ).SecInletNode;

		// Get information from inlet nodes
		ExchCond( ExchNum ).SupInTemp = Node( SupInNode ).Temp;
		ExchCond( ExchNum ).SupInHumRat = Node( SupInNode ).HumRat;
		ExchCond( ExchNum ).SupInEnth = Node( SupInNode ).Enthalpy;
		ExchCond( ExchNum ).SupInMassFlow = Node( SupInNode ).MassFlowRate;
		ExchCond( ExchNum ).SecInTemp = Node( SecInNode ).Temp;
		ExchCond( ExchNum ).SecInHumRat = Node( SecInNode ).HumRat;
		ExchCond( ExchNum ).SecInEnth = Node( SecInNode ).Enthalpy;
		ExchCond( ExchNum ).SecInMassFlow = Node( SecInNode ).MassFlowRate;

		// initialize the output variables
		ExchCond( ExchNum ).SensHeatingRate = 0.0;
		ExchCond( ExchNum ).SensHeatingEnergy = 0.0;
		ExchCond( ExchNum ).LatHeatingRate = 0.0;
		ExchCond( ExchNum ).LatHeatingEnergy = 0.0;
		ExchCond( ExchNum ).TotHeatingRate = 0.0;
		ExchCond( ExchNum ).TotHeatingEnergy = 0.0;
		ExchCond( ExchNum ).SensCoolingRate = 0.0;
		ExchCond( ExchNum ).SensCoolingEnergy = 0.0;
		ExchCond( ExchNum ).LatCoolingRate = 0.0;
		ExchCond( ExchNum ).LatCoolingEnergy = 0.0;
		ExchCond( ExchNum ).TotCoolingRate = 0.0;
		ExchCond( ExchNum ).TotCoolingEnergy = 0.0;
		ExchCond( ExchNum ).ElecUseRate = 0.0;
		ExchCond( ExchNum ).ElecUseEnergy = 0.0;
		ExchCond( ExchNum ).SensEffectiveness = 0.0;
		ExchCond( ExchNum ).LatEffectiveness = 0.0;
		ExchCond( ExchNum ).SupBypassMassFlow = 0.0;
		ExchCond( ExchNum ).SecBypassMassFlow = 0.0;

		//  Initialize inlet conditions

		{ auto const SELECT_CASE_var( ExchCond( ExchNum ).ExchTypeNum );

		if ( SELECT_CASE_var == HX_AIRTOAIR_FLATPLATE ) {

		} else if ( SELECT_CASE_var == HX_AIRTOAIR_GENERIC ) {

		} else if ( SELECT_CASE_var == HX_DESICCANT_BALANCED ) {

			if ( MySetPointTest( ExchNum ) ) {
				if ( ! SysSizingCalc && DoSetPointTest ) {
					if ( ! CalledFromParentObject ) {
						if ( Node( ExchCond( ExchNum ).SecOutletNode ).HumRatMax == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowWarningError( "Missing optional HumRatMax setpoint for " + cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + " \"" + ExchCond( ExchNum ).Name + "\"" );
								ShowContinueError( "...the simulation will continue without control of the desiccant heat exchanger to a maximum humidity ratio setpoint." );
								ShowContinueError( "...use a Setpoint Manager to establish a setpoint at the process air outlet node of the desiccant Heat Exchanger if control is desired." );
							} else {
								// need call to EMS to check node
								CheckIfNodeSetPointManagedByEMS( ExchCond( ExchNum ).SecOutletNode, iHumidityRatioMaxSetPoint, LocalWarningError );
								if ( LocalWarningError ) {
									ShowWarningError( "Missing optional HumRatMax setpoint for " + cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + " \"" + ExchCond( ExchNum ).Name + "\"" );
									ShowContinueError( "...the simulation will continue without control of the desiccant heat exchanger to a maximum humidity ratio setpoint." );
									ShowContinueError( "...use a Setpoint Manager to establish a setpoint at the process air outlet node of the desiccant Heat Exchanger if control is desired." );
									ShowContinueError( "...or use an EMS Actuator to establish a maximum humidity ratio setpoint at the process air outlet node of the desiccant Heat Exchanger if control is desired." );
								}
							}
						}
					}
					MySetPointTest( ExchNum ) = false;
				}
			}

			if ( CompanionCoilIndex > 0 ) {

				if ( DXCoilFullLoadOutAirTemp( CompanionCoilIndex ) == 0.0 || DXCoilFullLoadOutAirHumRat( CompanionCoilIndex ) == 0.0 ) {
					//       DX Coil is OFF, read actual inlet conditions
					FullLoadOutAirTemp = ExchCond( ExchNum ).SecInTemp;
					FullLoadOutAirHumRat = ExchCond( ExchNum ).SecInHumRat;
				} else {
					//       DX Coil is ON, read full load DX coil outlet conditions (conditions HX sees when ON)
					FullLoadOutAirTemp = DXCoilFullLoadOutAirTemp( CompanionCoilIndex );
					FullLoadOutAirHumRat = DXCoilFullLoadOutAirHumRat( CompanionCoilIndex );
				}

			} else {

				//     HX only (not used in conjunction with DX coil), read inlet conditions
				FullLoadOutAirTemp = ExchCond( ExchNum ).SecInTemp;
				FullLoadOutAirHumRat = ExchCond( ExchNum ).SecInHumRat;

			}

		} else {
			//   Will never get here

		}}

	}

	void
	SizeHeatRecovery( int const ExchNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   October 2007
		//       MODIFIED       February 2014 Daeho Kang, enable sizing multiple HX types and add additional sizing fields
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Heat Exchanger components for which flow rates have not been
		// specified in the input. Currently, only nominal supply air flow rate for the generic HX can be autosized.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the system or OA system sizing arrays

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataHVACGlobals::SmallAirVolFlow;
		using DataHVACGlobals::Main;
		using DataHVACGlobals::Cooling;
		using DataHVACGlobals::Heating;
		using DataHVACGlobals::Other;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool IsAutoSize;				// Indicator to autosize
		Real64 NomSupAirVolFlowDes;		// Autosized supply air flow rate for reproting
		Real64 NomSupAirVolFlowUser;	// Hard-sized supply air flow rate for reproting
		Real64 NomSecAirVolFlowDes;		// Autosized secondary air flow rate for reporting
		Real64 NomSecAirVolFlowUser;	// Hard-sized secondary air flow rate for reporting

		IsAutoSize = false;
		NomSupAirVolFlowDes = 0.0;
		NomSupAirVolFlowUser = 0.0;
		NomSecAirVolFlowDes = 0.0;
		NomSecAirVolFlowUser = 0.0;

		if ( ExchCond( ExchNum ).NomSupAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}

		if ( CurZoneEqNum > 0 ) {
			if ( !IsAutoSize && !ZoneSizingRunDone ) {
				if ( ExchCond( ExchNum ).NomSupAirVolFlow > 0.0 ) {
					ReportSizingOutput( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name,
					"User-Specified Nominal Supply Air Flow Rate [m3/s]", ExchCond( ExchNum ).NomSupAirVolFlow );
				}
			} else { // Sizing run done
				CheckZoneSizing( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name );
				NomSupAirVolFlowDes = std::max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
			}
		}

		if ( CurSysNum > 0 ) {
			if ( !IsAutoSize && !SysSizingRunDone ) {
				if ( ExchCond( ExchNum ).NomSupAirVolFlow > 0.0 ) {
					ReportSizingOutput( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name,
					"User-Specified Nominal Supply Air Flow Rate [m3/s]", ExchCond( ExchNum ).NomSupAirVolFlow );
				}
			} else { // Sizing run done

				CheckSysSizing( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name );

				if ( CurOASysNum > 0 ) {
					// size to outdoor air volume flow rate if available
					if ( FinalSysSizing( CurSysNum ).DesOutAirVolFlow > 0.0 ) {
						NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
					} else {
						// ELSE size to supply air duct flow rate
						{ auto const SELECT_CASE_var( CurDuctType );
						if ( SELECT_CASE_var == Main ) {
							NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						} else if ( SELECT_CASE_var == Cooling ) {
							NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
						} else if ( SELECT_CASE_var == Heating ) {
							NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
						} else if ( SELECT_CASE_var == Other ) {
							NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						} else {
							NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						}}
					}
				} else {
					{ auto const SELECT_CASE_var( CurDuctType );
					if ( SELECT_CASE_var == Main ) {
						NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else if ( SELECT_CASE_var == Cooling ) {
						NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
					} else if ( SELECT_CASE_var == Heating ) {
						NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
					} else if ( SELECT_CASE_var == Other ) {
						NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else {
						NomSupAirVolFlowDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					}}
				}

			}
		}

		if ( IsAutoSize ) {
			ExchCond( ExchNum ).NomSupAirVolFlow = NomSupAirVolFlowDes;
			ReportSizingOutput( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name,
				"Design Size Nominal Supply Air Flow Rate [m3/s]", NomSupAirVolFlowDes );
			if ( NomSupAirVolFlowDes < SmallAirVolFlow ) {
				NomSupAirVolFlowDes = 0.0;
 				// Generic HX will be turned off if nominal air flow rate is 0, even if simulated air flow through
				// HX is greater than 0. Avoids a divide by 0 in Sub CalcAirToAirGenericHeatExch.
				if ( ExchCond( ExchNum ).ExchTypeNum == HX_AIRTOAIR_GENERIC ) {
					ShowWarningError( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + ": \"" + ExchCond( ExchNum ).Name + "\"" );
					ShowContinueError( "... nominal supply air volume flow rate through the heat exchanger is sized to 0, see eio file for sizing results." );
					ShowContinueError( "... HX will not be enabled and the simulation continues." );
					ShowContinueError( "... To eliminate this warning, check sizing and HX inputs to correct HX sizing issue." );
				}
			}
		} else {
			if ( ExchCond( ExchNum ).NomSupAirVolFlow > 0.0 && NomSupAirVolFlowDes > 0.0 ) {
				NomSupAirVolFlowUser = ExchCond( ExchNum ).NomSupAirVolFlow;
				ReportSizingOutput( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name,
					"Design Size Nominal Supply Air Flow Rate [m3/s]", NomSupAirVolFlowDes,
					"User-Specified Nominal Supply Air Flow Rate [m3/s]", NomSupAirVolFlowUser );
				if ( DisplayExtraWarnings ) {
					if ( ( std::abs( NomSupAirVolFlowDes - NomSupAirVolFlowUser ) / NomSupAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
						ShowMessage( "Size:" + cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + ":Potential issue with equipment sizing for " +
							ExchCond( ExchNum ).Name );
						ShowContinueError( "User-Specified Nominal Supply Air Flow Rate of " +
							RoundSigDigits( NomSupAirVolFlowUser, 5 ) + " [m3/s]" );
						ShowContinueError( "differs from Design Size Nominal Supply Air Flow Rate of " +
							RoundSigDigits( NomSupAirVolFlowDes, 5 ) + " [m3/s]" );
						ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
						ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
					}
				}
			}
		}

		if ( ExchCond( ExchNum ).ExchTypeNum == HX_AIRTOAIR_FLATPLATE ) {
			IsAutoSize = false;

			if ( ExchCond( ExchNum ).NomSecAirVolFlow == AutoSize ) {
				IsAutoSize = true;
			}
			NomSecAirVolFlowDes = ExchCond( ExchNum ).NomSupAirVolFlow;

			if ( IsAutoSize) {
				ExchCond( ExchNum ).NomSecAirVolFlow = NomSecAirVolFlowDes;
				ReportSizingOutput( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name,
					"Design Size Nominal Secondary Air Flow Rate [m3/s]", NomSecAirVolFlowDes );
			} else {
				if ( ExchCond( ExchNum ).NomSecAirVolFlow > 0.0 && NomSecAirVolFlowDes > 0.0 ) {
					NomSecAirVolFlowUser = ExchCond( ExchNum ).NomSecAirVolFlow;
					ReportSizingOutput( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ), ExchCond( ExchNum ).Name,
						"Design Size Nominal Secondary Air Flow Rate [m3/s]", NomSecAirVolFlowDes,
						"User-Specified Nominal Secondary Air Flow Rate [m3/s]", NomSecAirVolFlowUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( NomSecAirVolFlowDes - NomSecAirVolFlowUser ) / NomSecAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "Size:" + cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + ":Potential issue with equipment sizing for " +
								ExchCond( ExchNum ).Name );
							ShowContinueError( "User-Specified Nominal Secondary Air Flow Rate of " + RoundSigDigits( NomSecAirVolFlowUser, 5 ) + " [m3/s]" );
							ShowContinueError( "differs from Design Size Nominal Secondary Air Flow Rate of " + RoundSigDigits( NomSecAirVolFlowDes, 5 ) + " [m3/s]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}
	}

	void
	CalcAirToAirPlateHeatExch(
		int const ExNum, // number of the current heat exchanger being simulated
		bool const HXUnitOn, // flag to simulate heat exchager heat recovery
		Optional_bool_const EconomizerFlag, // economizer flag pass by air loop or OA sys
		Optional_bool_const HighHumCtrlFlag // high humidity control flag passed by airloop or OA sys
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       F. Buhl Nov 2000, R. Raustad - FSEC, Feb 2009 - added economizer flags
		//                      Both the economizer and high humidity control flags can disable the HX
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the outlet conditions for an air to air plate heat
		// exchanger given the inlet conditions.

		// METHODOLOGY EMPLOYED:
		// This is a static heat exchanger model. No geometrical input data
		// is needed. No knowledge of h*A values is needed except the ratio
		// of the primary side to the secondary side convective heat transfer
		// coefficient times the exchanger surface area. Effectiveness - NTU
		// heat exchanger formulas are used.

		// The time varying load is calculated based on the variation of the
		// convective heat transfer coefficient.The variation is a function of
		// mass flow rate and inlet temperature. An iterative solution is only
		// required during initialization in one specific flow arrangement. During
		// the time steps the solution is explicit. The iteration is done with
		// the Regula Falsi algorithm. Convergence is always achieved.

		// REFERENCES:
		// M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
		// LBNL Report 42354, 1999.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool UnitOn; // unit on flag
		Real64 SupBypassMassFlow; // supply air mass flow rate bypassing unit [kg/s]
		Real64 UnitSupMassFlow; // supply air mass flow rate passing through the unit [kg/s]
		Real64 SecBypassMassFlow; // secondary air mass flow rate bypassing unit [kg/s]
		Real64 UnitSecMassFlow; // secondary air mass flow rate passing through the unit [kg/s]
		Real64 QuotSup; // ratio of supply nominal m*T to actual m*T
		Real64 QuotExh; // ratio of secondary nominal m*T to actual m*T
		Real64 Deno; // denominator of UA calculation
		Real64 CSup; // supply air capacitance rate [J/C/s]
		Real64 CSec; // secondary air capacitance rate [J/C/s]
		Real64 CMin; // minimum air capacitance rate [J/C/s]
		Real64 Z; // Ratio of minimum air capacitance rate to maximum air capacitance rate
		Real64 NTU; // Number of heat transfer units
		Real64 Eps; // epsilon, the unit effectiveness
		Real64 UA; // present UA
		Real64 TempSupOut; // unit supply outlet temperature [C]
		Real64 HumRatSupOut; // unit supply outlet humidity ratio [kg water / kg dry air]
		Real64 EnthSupOut; // unit supply outlet enthalpy [J/kg]
		Real64 TempSupOutSat; // unit supply outlet temperature at saturation (at EnthSupOut) [C]
		Real64 QTrans; // heat transferred in the heat exchanger [W]
		Real64 ElecCons; // electricity consumption rate [W]
		Real64 TempSecOut; // unit secondary outlet temperature [C]
		Real64 HumRatSecOut; // unit secondary outlet humidity ratio [kg water / kg dry air]
		Real64 EnthSecOut; // unit secondary outlet enthalpy [J/kgC]
		Real64 TempSecOutSat; // unit secondary outlet temperature at saturation (at EnthsSecOut) [C]
		Real64 SensHeatRecRate; // sensible heat recovery rate to supply air (heating +, cooling -)
		Real64 LatHeatRecRate; // latent heat recovery rate to supply air (heating [humidify] +, cooling [dehumidify] -)
		Real64 TotHeatRecRate; // total heat recovery rate to supply air (heating +, cooling -)
		bool EconomizerActiveFlag; // local representing the economizer status when PRESENT
		bool HighHumCtrlActiveFlag; // local representing high humidity control when PRESENT

		UnitOn = true;
		QTrans = 0.0;
		ElecCons = 0.0;

		if ( present( EconomizerFlag ) ) {
			EconomizerActiveFlag = EconomizerFlag;
		} else {
			EconomizerActiveFlag = false;
		}

		if ( present( HighHumCtrlFlag ) ) {
			HighHumCtrlActiveFlag = HighHumCtrlFlag;
		} else {
			HighHumCtrlActiveFlag = false;
		}

		if ( ( EconomizerActiveFlag || HighHumCtrlActiveFlag ) && ExchCond( ExNum ).EconoLockOut == EconoLockOut_Yes ) {
			UnitSupMassFlow = 0.0; // set HX supply flow to 0, all supply air will go through supply bypass
			UnitSecMassFlow = 0.0; // set HX secondary flow to 0, all secondary air will got through secondary bypass
			UnitOn = false; // turn off HX calculations when in economizer mode
		} else {
			// if economizer operation is not allowed, air always passes through HX
			// if CompanionCoilNum > 0, air always passes through HX (no economizer operation allowed)
			UnitSupMassFlow = min( ExchCond( ExNum ).NomSupAirMassFlow, ExchCond( ExNum ).SupInMassFlow );
			UnitSecMassFlow = min( ExchCond( ExNum ).NomSecAirMassFlow, ExchCond( ExNum ).SecInMassFlow );
		}

		SupBypassMassFlow = max( 0.0, ExchCond( ExNum ).SupInMassFlow - UnitSupMassFlow );
		SecBypassMassFlow = max( 0.0, ExchCond( ExNum ).SecInMassFlow - UnitSecMassFlow );
		if ( GetCurrentScheduleValue( ExchCond( ExNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( ExchCond( ExNum ).SupInMassFlow <= SmallMassFlow ) UnitOn = false;
		if ( ExchCond( ExNum ).SecInMassFlow <= SmallMassFlow ) UnitOn = false;
		if ( ! HXUnitOn ) UnitOn = false;

		if ( UnitOn ) {
			// unit is on
			// calculate the UA for this time step
			QuotSup = SafeDiv( ExchCond( ExNum ).mTSup0, UnitSupMassFlow * ( ExchCond( ExNum ).SupInTemp + KELVZERO ) );
			QuotExh = SafeDiv( ExchCond( ExNum ).mTSec0, UnitSecMassFlow * ( ExchCond( ExNum ).SecInTemp + KELVZERO ) );
			Deno = std::pow( QuotSup, 0.78 ) + ExchCond( ExNum ).hARatio * std::pow( QuotExh, 0.78 );
			UA = ExchCond( ExNum ).UA0 * ( ExchCond( ExNum ).hARatio + 1.0 ) / Deno;
			// calculate the NTU
			CSup = UnitSupMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SupInHumRat, ExchCond( ExNum ).SupInTemp );
			CSec = UnitSecMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SecInHumRat, ExchCond( ExNum ).SecInTemp );
			// note: no C can be zero since otherwise we wouldn't be here
			if ( CSup < CSec ) {
				CMin = CSup;
				Z = CMin / CSec;
			} else {
				CMin = CSec;
				Z = CMin / CSup;
			}
			NTU = UA / CMin;
			// Get the effectiveness
			CalculateEpsFromNTUandZ( NTU, Z, ExchCond( ExNum ).FlowArr, Eps );
			// use the effectiveness to calculate the unit outlet conditions
			TempSupOut = ExchCond( ExNum ).SupInTemp + Eps * CMin / CSup * ( ExchCond( ExNum ).SecInTemp - ExchCond( ExNum ).SupInTemp );
			QTrans = CSup * ( TempSupOut - ExchCond( ExNum ).SupInTemp );
			TempSecOut = ExchCond( ExNum ).SecInTemp - QTrans / CSec;
			HumRatSupOut = ExchCond( ExNum ).SupInHumRat;
			EnthSupOut = PsyHFnTdbW( TempSupOut, HumRatSupOut );
			// check for saturation in supply outlet
			TempSupOutSat = PsyTsatFnHPb( EnthSupOut, OutBaroPress );
			if ( TempSupOutSat > TempSupOut ) {
				TempSupOut = TempSupOutSat;
				HumRatSupOut = PsyWFnTdbH( TempSupOut, EnthSupOut );
			}
			HumRatSecOut = ExchCond( ExNum ).SecInHumRat;
			EnthSecOut = PsyHFnTdbW( TempSecOut, HumRatSecOut );
			// check for saturation in secondary outlet
			TempSecOutSat = PsyTsatFnHPb( EnthSecOut, OutBaroPress );
			if ( TempSecOutSat > TempSecOut ) {
				TempSecOut = TempSecOutSat;
				HumRatSecOut = PsyWFnTdbH( TempSecOut, EnthSecOut );
			}
			// calculate outlet conditions by mixing bypass air stream with air that went through the
			// heat exchanger core.
			ExchCond( ExNum ).SupOutEnth = ( UnitSupMassFlow * EnthSupOut + SupBypassMassFlow * ExchCond( ExNum ).SupInEnth ) / ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SupOutHumRat = ( UnitSupMassFlow * HumRatSupOut + SupBypassMassFlow * ExchCond( ExNum ).SupInHumRat ) / ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SupOutTemp = PsyTdbFnHW( ExchCond( ExNum ).SupOutEnth, ExchCond( ExNum ).SupOutHumRat );
			ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SecOutEnth = ( UnitSecMassFlow * EnthSecOut + SecBypassMassFlow * ExchCond( ExNum ).SecInEnth ) / ExchCond( ExNum ).SecInMassFlow;
			ExchCond( ExNum ).SecOutHumRat = ( UnitSecMassFlow * HumRatSecOut + SecBypassMassFlow * ExchCond( ExNum ).SecInHumRat ) / ExchCond( ExNum ).SecInMassFlow;
			ExchCond( ExNum ).SecOutTemp = PsyTdbFnHW( ExchCond( ExNum ).SecOutEnth, ExchCond( ExNum ).SecOutHumRat );
			ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;
			ElecCons = ExchCond( ExNum ).NomElecPower;

		} else {
			// the unit is off. Pass through the air streams with no change
			ExchCond( ExNum ).SupOutEnth = ExchCond( ExNum ).SupInEnth;
			ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupInHumRat;
			ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp;
			ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth;
			ExchCond( ExNum ).SecOutHumRat = ExchCond( ExNum ).SecInHumRat;
			ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp;
			ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;

		}
		CSup = ExchCond( ExNum ).SupInMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SupInHumRat, ExchCond( ExNum ).SupInTemp );
		SensHeatRecRate = CSup * ( ExchCond( ExNum ).SupOutTemp - ExchCond( ExNum ).SupInTemp );
		TotHeatRecRate = ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupOutEnth - ExchCond( ExNum ).SupInEnth );
		LatHeatRecRate = TotHeatRecRate - SensHeatRecRate;

		if ( SensHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).SensHeatingRate = SensHeatRecRate;
			ExchCond( ExNum ).SensCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).SensHeatingRate = 0.0;
			ExchCond( ExNum ).SensCoolingRate = std::abs( SensHeatRecRate );
		}
		if ( LatHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).LatHeatingRate = LatHeatRecRate;
			ExchCond( ExNum ).LatCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).LatHeatingRate = 0.0;
			ExchCond( ExNum ).LatCoolingRate = std::abs( LatHeatRecRate );
		}
		if ( TotHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).TotHeatingRate = TotHeatRecRate;
			ExchCond( ExNum ).TotCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).TotHeatingRate = 0.0;
			ExchCond( ExNum ).TotCoolingRate = std::abs( TotHeatRecRate );
		}

		ExchCond( ExNum ).ElecUseRate = ElecCons;

	}

	void
	CalcAirToAirGenericHeatExch(
		int const ExNum, // number of the current heat exchanger being simulated
		bool const HXUnitOn, // flag to simulate heat exchanger heat recovery
		bool const FirstHVACIteration, // first HVAC iteration flag
		int const FanOpMode, // Supply air fan operating mode (1=cycling, 2=constant)
		Optional_bool_const EconomizerFlag, // economizer flag pass by air loop or OA sys
		Optional_bool_const HighHumCtrlFlag, // high humidity control flag passed by airloop or OA sys
		Optional < Real64 const > HXPartLoadRatio //
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey
		//       DATE WRITTEN   February 2003
		//       MODIFIED       R. Raustad - FSEC, Feb 2009 - added economizer flags
		//                      Both the economizer and high humidity control flags can disable the HX
		//       RE-ENGINEERED  Richard Raustad, June 2003

		// PURPOSE OF THIS SUBROUTINE:
		//  Calculate the outlet conditions for an air to air generic heat
		//  exchanger given the inlet conditions.

		// METHODOLOGY EMPLOYED:
		//  This is a standard heat exchanger effectiveness model. No geometrical input data
		//  is needed. The model uses heat exchanger effectiveness performance data
		//  to calculate the air temperature and humidity ratio of the leaving
		//  supply and secondary air streams. Linear interpolation (or extrapolation)
		//  is assumed to obtain heat exchanger effectiveness at off-rated conditions.
		//  Economizer operation is allowed through the use of a Controller: Outside Air
		//  object.

		// REFERENCES:
		//  ARI Standard 1060-2001,Rating Air-to-Air Heat Exchangers for Energy Recovery Ventilation Equipment, www.ari.org
		//  ASHRAE Standard 84, Method of Testing Air-To-Air Heat Exchangers, www.ashrae.org
		//  U.S. Environmental Protection Agency software "SAVES" -
		//   School Advanced Ventilation Engineering Software http://www.epa.gov/iaq/schooldesign/saves.html

		// USE STATEMENTS:
		using DataHVACGlobals::CycFanCycCoil;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrorTol( 0.001 ); // error tolerence

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool UnitOn; // unit on flag
		bool FrostControlFlag; // unit is in frost control mode when TRUE
		int SupOutNode;
		Real64 Error; // iteration loop error variable
		Real64 Iter; // iteration counter
		Real64 ControlFraction; // fraction of effectiveness when rotary HX speed or plate bypass modulation is used for
		// temperature control
		Real64 RhoSup; // supply air density at actual pressure, temperature and humidity conditions [kg/m3]
		Real64 RhoSec; // secondary air density at actual pressure, temperature and humidity conditions [kg/m3]
		Real64 RhoStd; // standard air density at actual pressure, 20C dry-bulb temp and 0.0 absolute humidity [kg/m3]
		Real64 CSup; // supply air heat capacity rate [W/K]
		Real64 CSec; // secondary air heat capacity rate [W/K]
		Real64 CMin; // minimum air heat capacity rate [W/K]
		Real64 QSensTrans; // sensible heat transferred by the heat exchanger [W]
		Real64 QTotTrans; // total heat (sensible + latent) transferred by the heat exchanger [W]
		Real64 TempSecOutSat; // secondary air outlet temperature at saturation (at EnthsSecOut) [C]
		Real64 HXSecAirVolFlowRate; // air volume flow rate of the secondary air stream through the heat exchanger [m3/sec]
		Real64 HXSupAirVolFlowRate; // air volume flow rate of the supply air stream through the heat exchanger [m3/sec]
		Real64 HXAvgAirVolFlowRate; // average air volume flow rate through the heat exchanger [m3/sec]
		Real64 HXAirVolFlowRatio; // ratio of avg actual air volume flow through HX to nominal HX air volume flow [-]
		Real64 HXTempSetPoint; // setpoint temperature at supply outlet node of HX when ControlToTemperatureSetPoint = Yes
		Real64 MassFlowSecIn; // secondary air mass flow rate at HX inlet
		//  REAL(r64)    :: MassFlowSecOut      ! secondary air mass flow rate at HX outlet
		Real64 MassFlowSupIn; // supply air mass flow rate at HX inlet
		Real64 MassFlowSupOut; // supply air mass flow rate through HX core outlet
		Real64 MassFlowSupBypass; // supply air bypass mass flow rate around HX core
		Real64 TempSupIn; // supply side temperature of air entering HX
		Real64 TempSupOut; // supply side temperature of air leaving HX core
		Real64 HumRatSupIn; // supply side humidity ratio of air entering HX
		Real64 TempSecIn; // secondary side temperature of air entering HX
		Real64 SensHeatRecRate; // sensible heat recovery rate to supply air (heating +, cooling -)
		Real64 LatHeatRecRate; // latent heat recovery rate to supply air (heating [humidify] +, cooling [dehumidify] -)
		Real64 TotHeatRecRate; // total heat recovery rate to supply air (heating +, cooling -)
		bool EconomizerActiveFlag; // local representing the economizer status when PRESENT
		bool HighHumCtrlActiveFlag; // local representing high humidity control when PRESENT
		Real64 AirSidePLR;

		// Initialize local variables
		UnitOn = true;
		FrostControlFlag = false;
		QSensTrans = 0.0;
		QTotTrans = 0.0;
		ExchCond( ExNum ).DefrostFraction = 0.0;
		ExchCond( ExNum ).SensEffectiveness = 0.0;
		ExchCond( ExNum ).LatEffectiveness = 0.0;
		ExchCond( ExNum ).ElecUseRate = 0.0;
		ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp;
		ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp;
		ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupInHumRat;
		ExchCond( ExNum ).SecOutHumRat = ExchCond( ExNum ).SecInHumRat;
		ExchCond( ExNum ).SupOutEnth = ExchCond( ExNum ).SupInEnth;
		ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth;
		SupOutNode = ExchCond( ExNum ).SupOutletNode;
		HXTempSetPoint = Node( SupOutNode ).TempSetPoint;

		if ( present( EconomizerFlag ) ) {
			EconomizerActiveFlag = EconomizerFlag;
		} else {
			EconomizerActiveFlag = false;
		}

		if ( present( HighHumCtrlFlag ) ) {
			HighHumCtrlActiveFlag = HighHumCtrlFlag;
		} else {
			HighHumCtrlActiveFlag = false;
		}

		// Determine mass flow through heat exchanger and mass flow being bypassed (only flat plate bypasses flow)
		if ( ( ( EconomizerActiveFlag || HighHumCtrlActiveFlag ) && ExchCond( ExNum ).EconoLockOut == EconoLockOut_Yes ) && ExchCond( ExNum ).ExchConfigNum == Plate ) {
			ExchCond( ExNum ).SupBypassMassFlow = ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SecBypassMassFlow = ExchCond( ExNum ).SecInMassFlow;
			ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;
		} else { // No bypass mass flow
			ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;
			ExchCond( ExNum ).SupBypassMassFlow = 0.0;
			ExchCond( ExNum ).SecBypassMassFlow = 0.0;
		}
		// Unit is scheduled OFF, so bypass heat exchange calcs
		if ( GetCurrentScheduleValue( ExchCond( ExNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		//! Economizer is active, so bypass heat exchange calcs. This applies to both flat plate and rotary HX's
		if ( ( EconomizerActiveFlag || HighHumCtrlActiveFlag ) && ExchCond( ExNum ).EconoLockOut == EconoLockOut_Yes ) {
			UnitOn = false;
		}
		// Determine if unit is ON or OFF based on air mass flow through the supply and secondary airstreams and operation flag
		if ( ExchCond( ExNum ).SupInMassFlow <= SmallMassFlow ) UnitOn = false;
		if ( ExchCond( ExNum ).SecInMassFlow <= SmallMassFlow ) UnitOn = false;
		if ( ! HXUnitOn ) UnitOn = false;
		if ( ExchCond( ExNum ).NomSupAirVolFlow == 0.0 ) UnitOn = false;

		if ( UnitOn ) {
			// Unit is on.
			if( present( HXPartLoadRatio ) && FanOpMode == DataHVACGlobals::CycFanCycCoil ) {
				if( HXPartLoadRatio > 0 ) {
					AirSidePLR = HXPartLoadRatio;
				} else {
					AirSidePLR = 1.0;
				}
			} else {
				AirSidePLR = 1.0;
			}

			if( FanOpMode == DataHVACGlobals::CycFanCycCoil ) {
				ExchCond( ExNum ).SupInMassFlow /= AirSidePLR;
				ExchCond( ExNum ).SupOutMassFlow /= AirSidePLR;
				ExchCond( ExNum ).SecInMassFlow /= AirSidePLR;
				ExchCond( ExNum ).SecOutMassFlow /= AirSidePLR;
				ExchCond( ExNum ).SupBypassMassFlow /= AirSidePLR;
				ExchCond( ExNum ).SecBypassMassFlow /= AirSidePLR;
			}

			// In the future, use actual node pressures in the following air density calls
			RhoStd = PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );
			HXSupAirVolFlowRate = ExchCond( ExNum ).SupOutMassFlow / RhoStd; // volume flow using standard density
			HXSecAirVolFlowRate = ExchCond( ExNum ).SecOutMassFlow / RhoStd;
			// Limit unbalanced volumetric flow ratio to 2:1
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				if ( HXSupAirVolFlowRate != 0.0 && HXSecAirVolFlowRate != 0.0 ) {
					if ( ( ( HXSupAirVolFlowRate / HXSecAirVolFlowRate ) > 2.0 ) || ( ( HXSecAirVolFlowRate / HXSupAirVolFlowRate ) > 2.0 ) ) {
						++ExchCond( ExNum ).UnBalancedErrCount;
						if ( ExchCond( ExNum ).UnBalancedErrCount <= 2 ) {
							ShowSevereError( cHXTypes( ExchCond( ExNum ).ExchTypeNum ) + ": \"" + ExchCond( ExNum ).Name + "\" unbalanced air volume flow ratio through the heat exchanger is greater than 2:1." );
							ShowContinueErrorTimeStamp( "...HX Supply air to Exhaust air flow ratio = " + RoundSigDigits( HXSupAirVolFlowRate / HXSecAirVolFlowRate, 5 ) + '.' );
						} else {
							ShowRecurringWarningErrorAtEnd( cHXTypes( ExchCond( ExNum ).ExchTypeNum ) + " \"" + ExchCond( ExNum ).Name + "\":  Unbalanced air volume flow ratio exceeds 2:1 warning continues. HX flow ratio statistics follow.", ExchCond( ExNum ).UnBalancedErrIndex, HXSupAirVolFlowRate / HXSecAirVolFlowRate, HXSupAirVolFlowRate / HXSecAirVolFlowRate );
						}
					}
				}
			}
			// Calculate average volumetric flow rate of the two air streams
			HXAvgAirVolFlowRate = ( HXSecAirVolFlowRate + HXSupAirVolFlowRate ) / 2.0;
			HXAirVolFlowRatio = HXAvgAirVolFlowRate / ExchCond( ExNum ).NomSupAirVolFlow;
			// Average air volume flow rate must be between 50% and 130% of nominal supply air volume flow
			if ( HXAirVolFlowRatio > 1.3 || HXAirVolFlowRatio < 0.5 ) {
				if ( ! WarmupFlag && ! FirstHVACIteration ) {
					++ExchCond( ExNum ).LowFlowErrCount;
					if ( ExchCond( ExNum ).LowFlowErrCount == 1 ) {
						ShowWarningError( cHXTypes( ExchCond( ExNum ).ExchTypeNum ) + " \"" + ExchCond( ExNum ).Name + "\"" );
						ShowContinueError( "Average air volume flow rate is <50% or >130% of the nominal HX supply air volume flow rate." );
						ShowContinueErrorTimeStamp( "Air volume flow rate ratio = " + RoundSigDigits( HXAirVolFlowRatio, 3 ) + '.' );
					} else {
						ShowRecurringWarningErrorAtEnd( cHXTypes( ExchCond( ExNum ).ExchTypeNum ) + " \"" + ExchCond( ExNum ).Name + "\":  Average air volume flow rate is <50% or >130% warning continues. Air flow rate ratio statistics follow.", ExchCond( ExNum ).LowFlowErrIndex, HXAirVolFlowRatio, HXAirVolFlowRatio );
					}
				}
			}

			// Determine heat exchanger effectiveness using avg air volume flow rate based on actual inlet air density
			// Linearly interpolate and extrapolate (within limits) from effectiveness input values
			RhoSup = PsyRhoAirFnPbTdbW( OutBaroPress, ExchCond( ExNum ).SupInTemp, ExchCond( ExNum ).SupInHumRat );
			RhoSec = PsyRhoAirFnPbTdbW( OutBaroPress, ExchCond( ExNum ).SecInTemp, ExchCond( ExNum ).SecInHumRat );
			HXSupAirVolFlowRate = ExchCond( ExNum ).SupOutMassFlow / RhoSup;
			HXSecAirVolFlowRate = ExchCond( ExNum ).SecOutMassFlow / RhoSec;
			HXAvgAirVolFlowRate = ( HXSecAirVolFlowRate + HXSupAirVolFlowRate ) / 2.0;
			HXAirVolFlowRatio = HXAvgAirVolFlowRate / ExchCond( ExNum ).NomSupAirVolFlow;

			if ( ExchCond( ExNum ).SupInTemp < ExchCond( ExNum ).SecInTemp ) {
				// Use heating effectiveness values
				ExchCond( ExNum ).SensEffectiveness = ExchCond( ExNum ).HeatEffectSensible75 + ( ExchCond( ExNum ).HeatEffectSensible100 - ExchCond( ExNum ).HeatEffectSensible75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
				ExchCond( ExNum ).LatEffectiveness = ExchCond( ExNum ).HeatEffectLatent75 + ( ExchCond( ExNum ).HeatEffectLatent100 - ExchCond( ExNum ).HeatEffectLatent75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
			} else {
				// Use cooling effectiveness values
				ExchCond( ExNum ).SensEffectiveness = ExchCond( ExNum ).CoolEffectSensible75 + ( ExchCond( ExNum ).CoolEffectSensible100 - ExchCond( ExNum ).CoolEffectSensible75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
				ExchCond( ExNum ).LatEffectiveness = ExchCond( ExNum ).CoolEffectLatent75 + ( ExchCond( ExNum ).CoolEffectLatent100 - ExchCond( ExNum ).CoolEffectLatent75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
			}

			//     Keep effectiveness between 0 and 1.0 ??
			//     HXOpSensEffect = MAX(MIN(HXOpSensEffect,1.0),0.0)
			//     HXOpLatEffect =  MAX(MIN(HXOpLatEffect,1.0),0.0)

			//   The model should at least guard against negative numbers
			ExchCond( ExNum ).SensEffectiveness = max( 0.0, ExchCond( ExNum ).SensEffectiveness );
			ExchCond( ExNum ).LatEffectiveness = max( 0.0, ExchCond( ExNum ).LatEffectiveness );

			// Use the effectiveness to calculate the air conditions exiting the heat exchanger (all air flow through the HX)
			// Include EATR and OACF in the following calculations at some point

			CSup = ExchCond( ExNum ).SupOutMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SupInHumRat, ExchCond( ExNum ).SupInTemp );
			CSec = ExchCond( ExNum ).SecOutMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SecInHumRat, ExchCond( ExNum ).SecInTemp );
			CMin = min( CSup, CSec );

			ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp + ExchCond( ExNum ).SensEffectiveness * CMin / CSup * ( ExchCond( ExNum ).SecInTemp - ExchCond( ExNum ).SupInTemp );
			ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupInHumRat + ExchCond( ExNum ).LatEffectiveness * CMin / CSup * ( ExchCond( ExNum ).SecInHumRat - ExchCond( ExNum ).SupInHumRat );
			ExchCond( ExNum ).SupOutEnth = PsyHFnTdbW( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutHumRat );

			//   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
			if ( PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress ) > ExchCond( ExNum ).SupOutTemp ) {
				ExchCond( ExNum ).SupOutTemp = PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress );
				ExchCond( ExNum ).SupOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutEnth );
			}
			QSensTrans = CSup * ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp );
			ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp + QSensTrans / CSec;
			QTotTrans = ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupInEnth - ExchCond( ExNum ).SupOutEnth );
			ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth + QTotTrans / ExchCond( ExNum ).SecOutMassFlow;
			ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth );
			//   Control the supply air outlet temperature to a setpoint for Heating Mode only
			//   (ControlFraction = 0 HX fully bypassed, ControlFraction = 1 air passed entirely through HX)
			//   (supply air stream bypass mass flow rate proportional to ControlFraction except when frost control is active)
			if ( ExchCond( ExNum ).ControlToTemperatureSetPoint ) {
				if ( ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp ) != 0.0 ) {
					if ( ( ExchCond( ExNum ).SupInTemp < HXTempSetPoint && ExchCond( ExNum ).SupOutTemp > HXTempSetPoint ) ||
						( ExchCond( ExNum ).SupInTemp > HXTempSetPoint && ExchCond( ExNum ).SupOutTemp < HXTempSetPoint ) ) {
						ControlFraction = max( 0.0, min( 1.0, std::abs( ( ExchCond( ExNum ).SupInTemp - HXTempSetPoint ) / ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp ) ) ) );
					} else if ( ( ExchCond( ExNum ).SupInTemp < ExchCond( ExNum ).SupOutTemp && ExchCond( ExNum ).SupOutTemp < HXTempSetPoint ) ||
								( ExchCond( ExNum ).SupInTemp > ExchCond( ExNum ).SupOutTemp && ExchCond( ExNum ).SupOutTemp > HXTempSetPoint ) ) {
						ControlFraction = 1.0;
					} else {
						ControlFraction = 0.0;
					}
				} else {
					//     ELSE fully bypass HX to maintain supply outlet temp as high as possible
					ControlFraction = 0.0;
				}
				if ( ExchCond( ExNum ).ExchConfigNum == Rotary ) {
					//       Rotory HX's never get bypassed, rotational speed is modulated
					ExchCond( ExNum ).SensEffectiveness *= ControlFraction;
					ExchCond( ExNum ).LatEffectiveness *= ControlFraction;
				} else { // HX is a plate heat exchanger, bypass air to control SA temperature
					Error = 1.0;
					Iter = 0.0;
					MassFlowSupIn = ExchCond( ExNum ).SupInMassFlow;
					MassFlowSupOut = ExchCond( ExNum ).SupOutMassFlow;
					MassFlowSupBypass = ExchCond( ExNum ).SupBypassMassFlow;
					MassFlowSecIn = ExchCond( ExNum ).SecInMassFlow;
					TempSupIn = ExchCond( ExNum ).SupInTemp;
					TempSupOut = ExchCond( ExNum ).SupOutTemp;
					HumRatSupIn = ExchCond( ExNum ).SupInHumRat;
					TempSecIn = ExchCond( ExNum ).SecInTemp;
					while ( ( std::abs( Error ) > ErrorTol && Iter < 10 && ControlFraction < 1.0 ) || Iter == 1 ) {
						MassFlowSupOut = MassFlowSupIn * ControlFraction;
						MassFlowSupBypass = MassFlowSupIn * ( 1.0 - ControlFraction );
						HXSupAirVolFlowRate = MassFlowSupOut / RhoSup;
						HXAvgAirVolFlowRate = ( HXSecAirVolFlowRate + HXSupAirVolFlowRate ) / 2.0;
						HXAirVolFlowRatio = HXAvgAirVolFlowRate / ExchCond( ExNum ).NomSupAirVolFlow;
						CSup = MassFlowSupOut * PsyCpAirFnWTdb( HumRatSupIn, TempSupIn );
						CMin = min( CSup, CSec );
						if ( TempSupIn < TempSecIn ) {
							//          Use heating effectiveness values
							ExchCond( ExNum ).SensEffectiveness = ExchCond( ExNum ).HeatEffectSensible75 + ( ExchCond( ExNum ).HeatEffectSensible100 - ExchCond( ExNum ).HeatEffectSensible75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
							ExchCond( ExNum ).LatEffectiveness = ExchCond( ExNum ).HeatEffectLatent75 + ( ExchCond( ExNum ).HeatEffectLatent100 - ExchCond( ExNum ).HeatEffectLatent75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
						} else {
							//          Use cooling effectiveness values
							ExchCond( ExNum ).SensEffectiveness = ExchCond( ExNum ).CoolEffectSensible75 + ( ExchCond( ExNum ).CoolEffectSensible100 - ExchCond( ExNum ).CoolEffectSensible75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
							ExchCond( ExNum ).LatEffectiveness = ExchCond( ExNum ).CoolEffectLatent75 + ( ExchCond( ExNum ).CoolEffectLatent100 - ExchCond( ExNum ).CoolEffectLatent75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
						}
						if ( CSup == 0.0 ) {
							//          IF CSup = 0, then supply air mass flow rate = 0 and HX is fully bypassed. Fix divide by 0 error below DO loop.
							CSup = 1.0;
							CMin = 0.0;
							break;
						}
						TempSupOut = ( MassFlowSupOut * ( TempSupIn + ExchCond( ExNum ).SensEffectiveness * CMin / CSup * ( TempSecIn - TempSupIn ) ) + MassFlowSupBypass * TempSupIn ) / MassFlowSupIn;
						Error = ( TempSupOut - HXTempSetPoint );
						//         IF supply inlet temp = supply outlet temp, fully bypass HX - ELSE control to SP
						if ( TempSupIn != TempSupOut ) {
							ControlFraction = max( 0.0, min( 1.0, std::abs( ControlFraction * ( TempSupIn - HXTempSetPoint ) / ( TempSupIn - TempSupOut ) ) ) );
						} else if ( std::abs( TempSupOut - HXTempSetPoint ) < ErrorTol ) {
							//           IF TempSupIn = TempSupOut then TempSecIn = TempSupIn (ControlFraction = ?)
							//           Do nothing, variables in ELSE below have already been calculated
							break;
						} else {
							//           or HX is fully bypassed (ControlFraction = 0) which actually should be caught in IF(CSup .EQ. 0.0)THEN above.
							ControlFraction = 0.0;
							MassFlowSupOut = MassFlowSupIn * ControlFraction;
							MassFlowSupBypass = MassFlowSupIn * ( 1.0 - ControlFraction );
							CSup = 1.0;
							CMin = 0.0;
							break;
						}
						++Iter;
					}

					ExchCond( ExNum ).SupInMassFlow = MassFlowSupIn;
					ExchCond( ExNum ).SupOutMassFlow = MassFlowSupOut;
					ExchCond( ExNum ).SupBypassMassFlow = MassFlowSupBypass;
					ExchCond( ExNum ).SecInMassFlow = MassFlowSecIn;
					ExchCond( ExNum ).SupInTemp = TempSupIn;
					ExchCond( ExNum ).SupOutTemp = TempSupOut;
					ExchCond( ExNum ).SupInHumRat = HumRatSupIn;
					ExchCond( ExNum ).SecInTemp = TempSecIn;

				} // ENDIF for "IF (ExchCond(ExNum)%ExchConfig == 'ROTARY') THEN"
				ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp + ExchCond( ExNum ).SensEffectiveness * CMin / CSup * ( ExchCond( ExNum ).SecInTemp - ExchCond( ExNum ).SupInTemp );
				ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupInHumRat + ExchCond( ExNum ).LatEffectiveness * CMin / CSup * ( ExchCond( ExNum ).SecInHumRat - ExchCond( ExNum ).SupInHumRat );
				ExchCond( ExNum ).SupOutEnth = PsyHFnTdbW( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutHumRat );

				//     Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
				if ( PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress ) > ExchCond( ExNum ).SupOutTemp ) {
					ExchCond( ExNum ).SupOutTemp = PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress );
					ExchCond( ExNum ).SupOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutEnth );
				}

				QSensTrans = CSup * ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp );
				ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp + QSensTrans / CSec;
				QTotTrans = ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupInEnth - ExchCond( ExNum ).SupOutEnth );
				ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth + QTotTrans / ExchCond( ExNum ).SecOutMassFlow;
				ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth );

			} //ENDIF for "IF(ExchCond(ExNum)%ControlToTemperatureSetPoint .AND... THEN, ELSE"

			if( FanOpMode == DataHVACGlobals::CycFanCycCoil ) {
				ExchCond( ExNum ).SupInMassFlow *= AirSidePLR;
				ExchCond( ExNum ).SupOutMassFlow *= AirSidePLR;
				ExchCond( ExNum ).SecInMassFlow *= AirSidePLR;
				ExchCond( ExNum ).SecOutMassFlow *= AirSidePLR;
				ExchCond( ExNum ).SupBypassMassFlow *= AirSidePLR;
				ExchCond( ExNum ).SecBypassMassFlow *= AirSidePLR;
			} else if( FanOpMode == DataHVACGlobals::ContFanCycCoil ) {
				ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupOutTemp * AirSidePLR + ExchCond( ExNum ).SupInTemp * ( 1.0 - AirSidePLR );
				ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupOutHumRat * AirSidePLR + ExchCond( ExNum ).SupInHumRat * ( 1.0 - AirSidePLR );
				ExchCond( ExNum ).SupOutEnth = ExchCond( ExNum ).SupOutEnth * AirSidePLR + ExchCond( ExNum ).SupOutEnth * ( 1.0 - AirSidePLR );
				ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecOutTemp * AirSidePLR + ExchCond( ExNum ).SecInTemp * ( 1.0 - AirSidePLR );
				ExchCond( ExNum ).SecOutHumRat = ExchCond( ExNum ).SecOutHumRat * AirSidePLR + ExchCond( ExNum ).SecInHumRat * ( 1.0 - AirSidePLR );
				ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecOutEnth * AirSidePLR + ExchCond( ExNum ).SecOutEnth * ( 1.0 - AirSidePLR );
			}

			if( ( ExchCond( ExNum ).FrostControlType == "MINIMUMEXHAUSTTEMPERATURE" && ExchCond( ExNum ).SecOutTemp < ExchCond( ExNum ).ThresholdTemperature ) || ( ExchCond( ExNum ).FrostControlType == "EXHAUSTAIRRECIRCULATION" && ExchCond( ExNum ).SupInTemp <= ExchCond( ExNum ).ThresholdTemperature ) || ( ExchCond( ExNum ).FrostControlType == "EXHAUSTONLY" && ExchCond( ExNum ).SupInTemp <= ExchCond( ExNum ).ThresholdTemperature ) ) {
				FrostControl( ExNum );
				FrostControlFlag = true;
			}

			// check for saturation in secondary outlet
			TempSecOutSat = PsyTsatFnHPb( ExchCond( ExNum ).SecOutEnth, OutBaroPress );
			if ( TempSecOutSat > ExchCond( ExNum ).SecOutTemp ) {
				ExchCond( ExNum ).SecOutTemp = TempSecOutSat;
				ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth );
			}

			// calculate outlet conditions by mixing bypass air stream with air that went through the
			// heat exchanger core.  Perform this mixing only when no frost control is used or
			// heat exchanger is not in frost control mode.  Mixing similar to this is performed
			// in the frost control subroutine when in frost control mode.
			if ( ! FrostControlFlag ) {
				ExchCond( ExNum ).SupOutEnth = ( ExchCond( ExNum ).SupOutMassFlow * ExchCond( ExNum ).SupOutEnth + ExchCond( ExNum ).SupBypassMassFlow * ExchCond( ExNum ).SupInEnth ) / ExchCond( ExNum ).SupInMassFlow;
				ExchCond( ExNum ).SupOutHumRat = ( ExchCond( ExNum ).SupOutMassFlow * ExchCond( ExNum ).SupOutHumRat + ExchCond( ExNum ).SupBypassMassFlow * ExchCond( ExNum ).SupInHumRat ) / ExchCond( ExNum ).SupInMassFlow;
				ExchCond( ExNum ).SupOutTemp = PsyTdbFnHW( ExchCond( ExNum ).SupOutEnth, ExchCond( ExNum ).SupOutHumRat );
				ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
				ExchCond( ExNum ).SecOutEnth = ( ExchCond( ExNum ).SecOutMassFlow * ExchCond( ExNum ).SecOutEnth + ExchCond( ExNum ).SecBypassMassFlow * ExchCond( ExNum ).SecInEnth ) / ExchCond( ExNum ).SecInMassFlow;
				ExchCond( ExNum ).SecOutHumRat = ( ExchCond( ExNum ).SecOutMassFlow * ExchCond( ExNum ).SecOutHumRat + ExchCond( ExNum ).SecBypassMassFlow * ExchCond( ExNum ).SecInHumRat ) / ExchCond( ExNum ).SecInMassFlow;
				ExchCond( ExNum ).SecOutTemp = PsyTdbFnHW( ExchCond( ExNum ).SecOutEnth, ExchCond( ExNum ).SecOutHumRat );
				ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;
			}

			ExchCond( ExNum ).ElecUseRate = ExchCond( ExNum ).NomElecPower;

		} //ENDIF for "IF (UnitOn) THEN"

		// Calculate heat transfer from the unit using the final supply inlet and supply outlet air conditions
		CSup = ExchCond( ExNum ).SupOutMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SupInHumRat, ExchCond( ExNum ).SupInTemp );
		SensHeatRecRate = CSup * ( ExchCond( ExNum ).SupOutTemp - ExchCond( ExNum ).SupInTemp );
		TotHeatRecRate = ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupOutEnth - ExchCond( ExNum ).SupInEnth );
		LatHeatRecRate = TotHeatRecRate - SensHeatRecRate;

		// Set report variables based on sign of recovery rate
		if ( SensHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).SensHeatingRate = SensHeatRecRate;
			ExchCond( ExNum ).SensCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).SensHeatingRate = 0.0;
			ExchCond( ExNum ).SensCoolingRate = std::abs( SensHeatRecRate );
		}
		if ( LatHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).LatHeatingRate = LatHeatRecRate;
			ExchCond( ExNum ).LatCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).LatHeatingRate = 0.0;
			ExchCond( ExNum ).LatCoolingRate = std::abs( LatHeatRecRate );
		}
		if ( TotHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).TotHeatingRate = TotHeatRecRate;
			ExchCond( ExNum ).TotCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).TotHeatingRate = 0.0;
			ExchCond( ExNum ).TotCoolingRate = std::abs( TotHeatRecRate );
		}

	}

	void
	CalcDesiccantBalancedHeatExch(
		int const ExNum, // number of the current heat exchanger being simulated
		bool const HXUnitOn, // flag to simulate heat exchager heat recovery
		bool const FirstHVACIteration, // First HVAC iteration flag
		int const FanOpMode, // Supply air fan operating mode (1=cycling, 2=constant)
		Real64 const PartLoadRatio, // Part load ratio requested of DX compressor
		int const CompanionCoilIndex, // index of companion cooling coil
		bool const RegenInletIsOANode, // Flag to determine if regen side inlet is OANode, if so this air stream cycles
		Optional_bool_const EconomizerFlag, // economizer flag pass by air loop or OA sys
		Optional_bool_const HighHumCtrlFlag // high humidity control flag passed by airloop or OA sys
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar, FSEC
		//       DATE WRITTEN   January 2007
		//       MODIFIED       R. Raustad - FSEC, Feb 2009 - added economizer flags
		//                      Both the economizer and high humidity control flags can disable the HX
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Calculate the outlet conditions for a balanced air-to-air desiccant heat exchanger
		//  given the inlet conditions and face velocity. Performance map is provided by user.

		// METHODOLOGY EMPLOYED:
		//  This is an empirical heat exchanger model. The model uses heat exchanger performance data to
		//  calculate the air temperature and humidity ratio of the leaving upply and secondary air streams.
		//  Humidity control can enable/disable heat recovery through the use of the HXUnitOn Subroutine argument.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::DXCoilPartLoadRatio;
		using DataLoopNode::SensedNodeFlagValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool UnitOn; // unit on flag
		Real64 RhoStd; // standard air density at actual pressure, 20C dry-bulb temp and 0.0 absolute humidity [kg/m3]
		Real64 CSup; // supply air heat capacity rate [W/K]
		Real64 CSec; // secondary air heat capacity rate [W/K]
		Real64 TempSecOutSat; // secondary air outlet temperature at saturation (at EnthsSecOut) [C]
		Real64 SensHeatRecRate; // sensible heat recovery rate to supply air (heating +, cooling -)
		Real64 TotHeatRecRate; // total heat recovery rate to supply air (heating +, cooling -)
		Real64 ProcessSensHeatRecRate; // process sensible heat recovery rate (heating +, cooling -)
		Real64 ProcessTotHeatRecRate; // process total heat recovery rate (heating +, cooling -)
		Real64 ProcessLatHeatRecRate; // process latent heat recovery rate (heating [humidify] +, cooling [dehumidify] -)
		Real64 SupInMassFlow; // Supply side HX mass flow rate
		Real64 SecInMassFlow; // Secondary side HX mass flow rate

		Real64 Coeff1; // coefficient1 to empirical model (used for both temperature and humidity ratio equations)
		Real64 Coeff2; // coefficient2 to empirical model (used for both temperature and humidity ratio equations)
		Real64 Coeff3; // coefficient3 to empirical model (used for both temperature and humidity ratio equations)
		Real64 Coeff4; // coefficient4 to empirical model (used for both temperature and humidity ratio equations)
		Real64 Coeff5; // coefficient5 to empirical model (used for both temperature and humidity ratio equations)
		Real64 Coeff6; // coefficient6 to empirical model (used for both temperature and humidity ratio equations)
		Real64 Coeff7; // coefficient7 to empirical model (used for both temperature and humidity ratio equations)
		Real64 Coeff8; // coefficient8 to empirical model (used for both temperature and humidity ratio equations)
		Real64 BalFaceVelActual; // operating face velocity [m/s]
		Real64 FullLoadSupOutTemp( 0 ); // empirical model supply outlet temperature [C]
		Real64 FullLoadSupOutHumRat( 0 ); // empirical model supply outlet humidity ratio [kg/kg]
		Real64 FullLoadDeltaT; // empirical model heat exchanger delta temperature [C]
		Real64 FullLoadDeltaW; // empirical model heat exchanger delta humidity ratio [kg/kg]
		Real64 T_RegenInTemp; // empirical model supply (regen) inlet temperature for temperature equation [C]
		Real64 T_RegenInHumRat; // empirical model supply (regen) inlet humidity ratio for temperature equation [kg/kg]
		Real64 T_ProcInTemp; // empirical model secondary (process) inlet temperature for temperature equation [C]
		Real64 T_ProcInHumRat; // empirical model secondary (process) inlet humidity ratio for temperature equation [kg/kg]
		Real64 T_FaceVel; // empirical model face velocity for temperature equation [m/s]
		Real64 H_RegenInTemp; // empirical model supply (regen) inlet temperature for humidity ratio equation [C]
		Real64 H_RegenInHumRat; // empirical model supply (regen) inlet humidity ratio for humidity ratio equation [kg/kg]
		Real64 H_ProcInTemp; // empirical model secondary (process) inlet temperature for humidity ratio equation [C]
		Real64 H_ProcInHumRat; // empirical model secondary (process) inlet humidity ratio for humidity ratio equation [kg/kg]
		Real64 H_FaceVel; // empirical model face velocity for humidity ratio equation [m/s]
		Real64 MaxHumRatNeeded; // maximum humidity ratio setpoint for balanced desiccant HX [kg/kg]
		Real64 MinHumRatNeeded; // minimum humidity ratio setpoint for balanced desiccant HX [kg/kg]
		Real64 HXPartLoadRatio; // local heat exchanger part-load ratio
		Real64 TestSaturationEnthalpy; // enthalpy used to test for regeneration outlet condition over saturation curve (J/kg)
		static std::string const ThisSub( "CalcDesiccantBalancedHeatExch:  " ); // Used to pass to Psyc routines
		static std::string const ThisSubTSat( "CalcDesiccantBalancedHeatExch:   TSat" );
		static std::string const ThisSubTSatFullLoadOutTemp( "CalcDesiccantBalancedHeatExch:   TSat-FullLoadOutTemp" );
		static std::string const ThisSubTSatFullLoadOutHumRat( "CalcDesiccantBalancedHeatExch:   TSat-FullLoadOutHumRat" );
		static std::string const ThisSubSecOutHumRat( "CalcDesiccantBalancedHeatExch:   SecOutHumRat" );
		static std::string const ThisSubTestSatSec( "CalcDesiccantBalancedHeatExch:   TestSatSec" );
		static std::string const ThisSubTSatSecOutHumRat( "CalcDesiccantBalancedHeatExch:   TSat-SecOutHumRat" );

		Real64 AverageMassFlowRate; // average of supply (regen) and secondary (process) mass flow rates [kg/s]
		bool EconomizerActiveFlag; // local representing the economizer status when PRESENT
		bool HighHumCtrlActiveFlag; // local representing high humidity control when PRESENT

		// Initialize local variables
		UnitOn = true;
		SensHeatRecRate = 0.0;
		TotHeatRecRate = 0.0;
		HXPartLoadRatio = PartLoadRatio;
		ExchCond( ExNum ).DefrostFraction = 0.0;
		ExchCond( ExNum ).ElecUseRate = 0.0;
		ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp;
		ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp;
		ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupInHumRat;
		ExchCond( ExNum ).SecOutHumRat = ExchCond( ExNum ).SecInHumRat;
		ExchCond( ExNum ).SupOutEnth = ExchCond( ExNum ).SupInEnth;
		ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth;
		ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
		ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;
		AverageMassFlowRate = ( ExchCond( ExNum ).SupOutMassFlow + ExchCond( ExNum ).SecOutMassFlow ) / 2.0;

		if ( present( EconomizerFlag ) ) {
			EconomizerActiveFlag = EconomizerFlag;
		} else {
			EconomizerActiveFlag = false;
		}

		if ( present( HighHumCtrlFlag ) ) {
			HighHumCtrlActiveFlag = HighHumCtrlFlag;
		} else {
			HighHumCtrlActiveFlag = false;
		}

		// Unit is scheduled OFF, so bypass heat exchange calcs
		if ( GetCurrentScheduleValue( ExchCond( ExNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		// Determine if unit is ON or OFF based on air mass flow through the supply and secondary airstreams and operation flag
		if ( ExchCond( ExNum ).SupInMassFlow <= SmallMassFlow ) UnitOn = false;
		if ( ExchCond( ExNum ).SecInMassFlow <= SmallMassFlow ) UnitOn = false;
		if ( HXPartLoadRatio == 0.0 ) UnitOn = false;
		if ( ! HXUnitOn ) UnitOn = false;
		if ( ( EconomizerActiveFlag || HighHumCtrlActiveFlag ) && ExchCond( ExNum ).EconoLockOut == EconoLockOut_Yes ) UnitOn = false;

		if ( UnitOn ) {

			//   Use local variables to perform checks
			SecInMassFlow = ExchCond( ExNum ).SecInMassFlow;
			SupInMassFlow = ExchCond( ExNum ).SupInMassFlow;

			// In constant fan mode, process air mass flow rate is full flow and supply (regen) air cycles based on PLR.
			// If supply (regen) inlet is OA node, regen mass flow rate is proportional to PLR.
			// If both of the above is true then boost local variable up to full flow
			if ( ( FanOpMode == ContFanCycCoil ) && RegenInletIsOANode ) {
				SupInMassFlow /= HXPartLoadRatio;
			}
			// for cycling fan case, boost both local variables up to full flow
			if ( FanOpMode == CycFanCycCoil ) {
				SupInMassFlow /= HXPartLoadRatio; // supply = regen
				SecInMassFlow /= HXPartLoadRatio; // secondary = process
			}

			// Check for balanced flow condition
			CheckForBalancedFlow( ExNum, SecInMassFlow, SupInMassFlow, FirstHVACIteration );

			{ auto const SELECT_CASE_var( ExchCond( ExNum ).HeatExchPerfTypeNum );

			if ( SELECT_CASE_var == BALANCEDHX_PERFDATATYPE1 ) {

				Coeff1 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B1;
				Coeff2 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B2;
				Coeff3 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B3;
				Coeff4 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B4;
				Coeff5 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B5;
				Coeff6 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B6;
				Coeff7 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B7;
				Coeff8 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).B8;

				T_ProcInTemp = FullLoadOutAirTemp;
				T_ProcInHumRat = FullLoadOutAirHumRat;
				T_RegenInTemp = ExchCond( ExNum ).SupInTemp;
				T_RegenInHumRat = ExchCond( ExNum ).SupInHumRat;

				// Must use the same density used to convert volumetric flow rate to mass flow rate to get back to velocity
				RhoStd = StdRhoAir; //PsyRhoAirFnPbTdbW(StdBaroPress,20.0d0, 0.0d0)
				BalFaceVelActual = SupInMassFlow / ( RhoStd * ExchCond( ExNum ).FaceArea );

				T_FaceVel = BalFaceVelActual;

				//     Call model check routines only when HX is active, if coil is off these checks do not apply (no potential for heat transfer)
				//     Check RH limits and warn user if out of bounds (T_* not modified in subroutine)

				CheckModelBoundsRH_TempEq( ExNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, FirstHVACIteration );
				//     Check model boundaries and cap empirical model independent variables as needed (T_* may be modified on return from sub)
				CheckModelBoundsTempEq( ExNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, T_FaceVel, FirstHVACIteration );

				if ( T_ProcInTemp != 0.0 && T_RegenInTemp != 0.0 ) {
					FullLoadSupOutTemp = Coeff1 + Coeff2 * T_RegenInHumRat + Coeff3 * T_RegenInTemp + Coeff4 * ( T_RegenInHumRat / T_RegenInTemp ) + Coeff5 * T_ProcInHumRat + Coeff6 * T_ProcInTemp + Coeff7 * ( T_ProcInHumRat / T_ProcInTemp ) + Coeff8 * T_FaceVel;

					// Check model boundary for supply (regen) temp and do not cap value if out of bounds, check that supply in temp > out temp
					CheckModelBoundOutput_Temp( ExNum, ExchCond( ExNum ).SupInTemp, FullLoadSupOutTemp, FirstHVACIteration );
					FullLoadDeltaT = FullLoadSupOutTemp - ExchCond( ExNum ).SupInTemp;
				} else {
					FullLoadDeltaT = 0.0;
				}

				Coeff1 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C1;
				Coeff2 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C2;
				Coeff3 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C3;
				Coeff4 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C4;
				Coeff5 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C5;
				Coeff6 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C6;
				Coeff7 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C7;
				Coeff8 = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).C8;

				H_ProcInTemp = FullLoadOutAirTemp;
				H_ProcInHumRat = FullLoadOutAirHumRat;
				H_RegenInTemp = ExchCond( ExNum ).SupInTemp;
				H_RegenInHumRat = ExchCond( ExNum ).SupInHumRat;
				H_FaceVel = BalFaceVelActual;

				//     Call model check routines only when HX is active, if coil is off these checks do not apply (no potential for heat transfer)
				//     Check RH limits and warn user if out of bounds (H_* not modified in subroutine)

				CheckModelBoundsRH_HumRatEq( ExNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, FirstHVACIteration );
				//     Check model boundaries and cap empirical model independent variables as needed (H_* may be modified on return from sub)
				CheckModelBoundsHumRatEq( ExNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, H_FaceVel, FirstHVACIteration );

				//     Calc curve
				if ( H_ProcInTemp != 0.0 && H_RegenInTemp != 0.0 ) {
					FullLoadSupOutHumRat = Coeff1 + Coeff2 * H_RegenInHumRat + Coeff3 * H_RegenInTemp + Coeff4 * ( H_RegenInHumRat / H_RegenInTemp ) + Coeff5 * H_ProcInHumRat + Coeff6 * H_ProcInTemp + Coeff7 * ( H_ProcInHumRat / H_ProcInTemp ) + Coeff8 * H_FaceVel;

					// Check model boundary for supply (regen) hum rat and do not cap value if out of bounds, check that supply in HR < out HR
					CheckModelBoundOutput_HumRat( ExNum, ExchCond( ExNum ).SupInHumRat, FullLoadSupOutHumRat, FirstHVACIteration );
					FullLoadDeltaW = FullLoadSupOutHumRat - ExchCond( ExNum ).SupInHumRat;
				} else {
					FullLoadDeltaW = 0.0;
				}

				//     Check for saturation in the model's calculated supply outlet and reset temp, then humidity ratio at constant enthalpy
				//     Reset delta T and delta W such that the model does not allow an outlet condition over saturation
				TestSaturationEnthalpy = PsyHFnTdbW( FullLoadSupOutTemp, FullLoadSupOutHumRat );
				if ( PsyTsatFnHPb( TestSaturationEnthalpy, OutBaroPress, ThisSubTSat ) > FullLoadSupOutTemp ) {
					FullLoadSupOutTemp = PsyTsatFnHPb( TestSaturationEnthalpy, OutBaroPress, ThisSubTSatFullLoadOutTemp );
					FullLoadSupOutHumRat = PsyWFnTdbH( FullLoadSupOutTemp, TestSaturationEnthalpy, ThisSubTSatFullLoadOutHumRat );
					FullLoadDeltaT = FullLoadSupOutTemp - ExchCond( ExNum ).SupInTemp;
					FullLoadDeltaW = FullLoadSupOutHumRat - ExchCond( ExNum ).SupInHumRat;
				}

				if ( ! CalledFromParentObject ) {
					//       calculate part-load ratio for HX
					MaxHumRatNeeded = Node( ExchCond( ExNum ).SecOutletNode ).HumRatMax;
					MinHumRatNeeded = Node( ExchCond( ExNum ).SecOutletNode ).HumRatMin;
					// Calculate partload fraction of dehumidification capacity required to meet setpoint

					//       check the model output, if the regen delta W is positive, the process air stream is dehumidified
					if ( FullLoadDeltaW > 0 ) {
						//         check for a setpoint, if no setpoint then PLR remains at 1
						if ( MaxHumRatNeeded != SensedNodeFlagValue ) {
							if ( ExchCond( ExNum ).SecInHumRat > MaxHumRatNeeded && MaxHumRatNeeded > 0.0 ) {
								HXPartLoadRatio = ( ExchCond( ExNum ).SecInHumRat - MaxHumRatNeeded ) / FullLoadDeltaW;
							} else {
								HXPartLoadRatio = 0.0;
							}
						}
						//       check the model output, if the regen delta W is negative, the process air stream is humidified
					} else if ( FullLoadDeltaW < 0 ) {
						//         check for a setpoint, if no setpoint then PLR remains at 1
						if ( MinHumRatNeeded != SensedNodeFlagValue ) {
							if ( ExchCond( ExNum ).SecInHumRat < MinHumRatNeeded && MinHumRatNeeded > 0.0 ) {
								HXPartLoadRatio = ( ExchCond( ExNum ).SecInHumRat - MinHumRatNeeded ) / FullLoadDeltaW;
							} else {
								HXPartLoadRatio = 0.0;
							}
						}
					}

					HXPartLoadRatio = max( 0.0, HXPartLoadRatio );
					HXPartLoadRatio = min( 1.0, HXPartLoadRatio );

				} else if ( CompanionCoilIndex > 0 ) {
					HXPartLoadRatio = DXCoilPartLoadRatio( CompanionCoilIndex );
				}

				if ( FanOpMode == CycFanCycCoil || RegenInletIsOANode ) {
					//       Supply (regen) air stream mass flow rate is cycling and proportional to PLR, outlet conditions are full load conditions
					ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp + FullLoadDeltaT;
					ExchCond( ExNum ).SupOutHumRat = min( 1.0, max( 1.e-5, ExchCond( ExNum ).SupInHumRat + FullLoadDeltaW ) );
				} else {
					//       Supply (regen) air stream mass flow rate is constant and outlet conditions are averaged
					ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp + ( FullLoadDeltaT * HXPartLoadRatio );
					ExchCond( ExNum ).SupOutHumRat = min( 1.0, max( 1.e-5, ExchCond( ExNum ).SupInHumRat + ( FullLoadDeltaW * HXPartLoadRatio ) ) );
				}

				//     for a balanced flow HX, use average mass flow rate and actual node conditions to calculate CSup and CSec
				//     the mass flow rate on the process and secondary side of HX may be imbalanced when the HX is used in the OA branch
				//     use the average mass flow rate to avoid psych warnings, mass flow rates will converge at the end of the iteration
				//     if the air mass flow rates do not converge, this model should not be used
				CSup = AverageMassFlowRate * PsyCpAirFnWTdb( ExchCond( ExNum ).SupInHumRat, ExchCond( ExNum ).SupInTemp );
				CSec = AverageMassFlowRate * PsyCpAirFnWTdb( ExchCond( ExNum ).SecInHumRat, ExchCond( ExNum ).SecInTemp );

				ExchCond( ExNum ).SupOutEnth = PsyHFnTdbW( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutHumRat );

				SensHeatRecRate = CSup * ( ExchCond( ExNum ).SupOutTemp - ExchCond( ExNum ).SupInTemp );

				TotHeatRecRate = AverageMassFlowRate * ( ExchCond( ExNum ).SupOutEnth - ExchCond( ExNum ).SupInEnth );

				//     now calculate process side heat transfer

				ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth - TotHeatRecRate / AverageMassFlowRate;

				ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp - SensHeatRecRate / CSec;

				ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth, ThisSubSecOutHumRat );

				// check for saturation in process (secondary) outlet
				// The process outlet conditions should never be over the saturation curve for the balanced desiccant model
				// although this may occur during warmup. This check is included here for consistency.
				TempSecOutSat = PsyTsatFnHPb( ExchCond( ExNum ).SecOutEnth, OutBaroPress, ThisSubTestSatSec );
				if ( TempSecOutSat > ExchCond( ExNum ).SecOutTemp ) {
					ExchCond( ExNum ).SecOutTemp = TempSecOutSat;
					ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth, ThisSubTSatSecOutHumRat );
				}

				ExchCond( ExNum ).ElecUseRate = BalDesDehumPerfData( ExchCond( ExNum ).PerfDataIndex ).NomElecPower * HXPartLoadRatio;

			} else {

			}}

		} //ENDIF for "IF (UnitOn) THEN"

		// Report the process side heat transfer
		CSec = AverageMassFlowRate * PsyCpAirFnWTdb( ExchCond( ExNum ).SecInHumRat, ExchCond( ExNum ).SecInTemp );
		ProcessSensHeatRecRate = CSec * ( ExchCond( ExNum ).SecOutTemp - ExchCond( ExNum ).SecInTemp );

		ProcessTotHeatRecRate = ExchCond( ExNum ).SecOutMassFlow * ( ExchCond( ExNum ).SecOutEnth - ExchCond( ExNum ).SecInEnth );

		ProcessLatHeatRecRate = ProcessTotHeatRecRate - ProcessSensHeatRecRate;

		// Set report variables based on sign of recovery rate
		if ( ProcessSensHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).SensHeatingRate = ProcessSensHeatRecRate;
			ExchCond( ExNum ).SensCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).SensHeatingRate = 0.0;
			ExchCond( ExNum ).SensCoolingRate = std::abs( ProcessSensHeatRecRate );
		}
		if ( ProcessLatHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).LatHeatingRate = ProcessLatHeatRecRate;
			ExchCond( ExNum ).LatCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).LatHeatingRate = 0.0;
			ExchCond( ExNum ).LatCoolingRate = std::abs( ProcessLatHeatRecRate );
		}
		if ( ProcessTotHeatRecRate > 0.0 ) {
			ExchCond( ExNum ).TotHeatingRate = ProcessTotHeatRecRate;
			ExchCond( ExNum ).TotCoolingRate = 0.0;
		} else {
			ExchCond( ExNum ).TotHeatingRate = 0.0;
			ExchCond( ExNum ).TotCoolingRate = std::abs( ProcessTotHeatRecRate );
		}

	}

	void
	FrostControl( int const ExNum ) // number of the current heat exchanger being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates fraction of timestep necessary to eliminate frost on ERV surface
		// by comparing secondary outlet or outdoor temperature to a frost control threshold
		// temperature.  Supply air and secondary air outlet conditions are calculated
		// based on frost control method selected.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrorTol( 0.001 ); // error tolerence for iteration loop
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DFFraction; // fraction of timestep ERV is in frost control mode
		Real64 RhoSup; // density of supply air [kg/m3]
		Real64 RhoSec; // density of secondary air [kg/m3]
		Real64 Error; // iteration loop error variable
		Real64 Iter; // iteration counter
		Real64 CSup; // mdot Cp of supply air [W/K]
		Real64 CSec; // mdot Cp of secondary air [W/K]
		Real64 CMin; // minimum mdot Cp of supply or secondary air [W/K]
		Real64 QTotTrans; // total heat transfer by ERV [W]
		Real64 QSensTrans; // sensible heat transfer by ERV [W]
		Real64 HXSecAirVolFlowRate; // air volume flow rate of the secondary air stream through the heat exchanger [m3/sec]
		Real64 HXSupAirVolFlowRate; // air volume flow rate of the supply air stream through the heat exchanger [m3/sec]
		Real64 HXAvgAirVolFlowRate; // average air volume flow rate through the heat exchanger [m3/sec]
		Real64 HXAirVolFlowRatio; // nominal to actual air volume flow ratio
		Real64 MassFlowSupIn; // supply air mass flow rate at HX inlet
		Real64 MassFlowSupOut; // supply air mass flow rate through HX core outlet
		Real64 MassFlowSupBypass; // supply air bypass mass flow rate around HX core
		Real64 TempSupIn; // supply side temperature of air entering HX
		Real64 TempSupOut; // supply side temperature of air leaving HX core
		Real64 HumRatSupIn; // supply side humidity ratio of air entering HX
		Real64 TempSecIn; // secondary side temperature of air entering HX
		Real64 TempSecOut; // secondary side temperature of air leaving HX core
		Real64 TempThreshold; // threshold temperature below which frost control is active

		ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
		ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;
		ExchCond( ExNum ).SupBypassMassFlow = 0.0;
		ExchCond( ExNum ).SecBypassMassFlow = 0.0;
		RhoSup = PsyRhoAirFnPbTdbW( OutBaroPress, ExchCond( ExNum ).SupInTemp, ExchCond( ExNum ).SupInHumRat );
		RhoSec = PsyRhoAirFnPbTdbW( OutBaroPress, ExchCond( ExNum ).SecInTemp, ExchCond( ExNum ).SecInHumRat );
		CSup = ExchCond( ExNum ).SupOutMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SupInHumRat, ExchCond( ExNum ).SupInTemp );
		CSec = ExchCond( ExNum ).SecOutMassFlow * PsyCpAirFnWTdb( ExchCond( ExNum ).SecInHumRat, ExchCond( ExNum ).SecInTemp );
		CMin = min( CSup, CSec );
		TempThreshold = ExchCond( ExNum ).ThresholdTemperature;

		if ( ExchCond( ExNum ).ControlToTemperatureSetPoint ) {
			// Recalculate HX outlet conditions as if control to temperature setpoint was not activated,
			// because defrost will override those results

			HXSupAirVolFlowRate = ExchCond( ExNum ).SupOutMassFlow / RhoSup;
			HXSecAirVolFlowRate = ExchCond( ExNum ).SecOutMassFlow / RhoSec;
			HXAvgAirVolFlowRate = ( HXSecAirVolFlowRate + HXSupAirVolFlowRate ) / 2.0;
			HXAirVolFlowRatio = HXAvgAirVolFlowRate / ExchCond( ExNum ).NomSupAirVolFlow;
			ExchCond( ExNum ).SensEffectiveness = ExchCond( ExNum ).HeatEffectSensible75 + ( ExchCond( ExNum ).HeatEffectSensible100 - ExchCond( ExNum ).HeatEffectSensible75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
			ExchCond( ExNum ).LatEffectiveness = ExchCond( ExNum ).HeatEffectLatent75 + ( ExchCond( ExNum ).HeatEffectLatent100 - ExchCond( ExNum ).HeatEffectLatent75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
			ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp + ExchCond( ExNum ).SensEffectiveness * CMin / CSup * ( ExchCond( ExNum ).SecInTemp - ExchCond( ExNum ).SupInTemp );
			ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupInHumRat + ExchCond( ExNum ).LatEffectiveness * CMin / CSup * ( ExchCond( ExNum ).SecInHumRat - ExchCond( ExNum ).SupInHumRat );
			ExchCond( ExNum ).SupOutEnth = PsyHFnTdbW( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutHumRat );

			//   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
			if ( PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress ) > ExchCond( ExNum ).SupOutTemp ) {
				ExchCond( ExNum ).SupOutTemp = PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress );
				ExchCond( ExNum ).SupOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutEnth );
			}

			QSensTrans = CSup * ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp );
			ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp + QSensTrans / CSec;
			QTotTrans = ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupInEnth - ExchCond( ExNum ).SupOutEnth );
			ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth + QTotTrans / ExchCond( ExNum ).SecOutMassFlow;
			ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth );
		}

		// Check frost control by type

		if ( ExchCond( ExNum ).FrostControlType == "MINIMUMEXHAUSTTEMPERATURE" ) {
			//   A plate HX will bypass air on the supply side to keep exhaust temp above a
			//   threshold temperature and requires recalculating effectiveness based on
			//   the reduced air flow rate. A rotary HX modulates rotational speed to try to keep the
			//   exhaust air temperature above the threshold temperature. Assume that
			//   sensible and latent effectiveness decrease proportionally with rotary HX speed.

			DFFraction = max( 0.0, min( 1.0, SafeDiv( ( TempThreshold - ExchCond( ExNum ).SecOutTemp ), ( ExchCond( ExNum ).SecInTemp - ExchCond( ExNum ).SecOutTemp ) ) ) );
			if ( ExchCond( ExNum ).ExchConfigNum == Rotary ) {
				ExchCond( ExNum ).SensEffectiveness *= ( 1.0 - DFFraction );
				ExchCond( ExNum ).LatEffectiveness *= ( 1.0 - DFFraction );
			} else { // HX is a plate heat exchanger, bypass air to eliminate frost
				Error = 1.0;
				Iter = 0.0;
				MassFlowSupIn = ExchCond( ExNum ).SupInMassFlow;
				MassFlowSupOut = ExchCond( ExNum ).SupOutMassFlow;
				MassFlowSupBypass = ExchCond( ExNum ).SupBypassMassFlow;
				TempSupIn = ExchCond( ExNum ).SupInTemp;
				HumRatSupIn = ExchCond( ExNum ).SupInHumRat;
				TempSecIn = ExchCond( ExNum ).SecInTemp;

				while ( std::abs( Error ) > ErrorTol && Iter < 10 ) {
					MassFlowSupOut = MassFlowSupIn * ( 1.0 - DFFraction );
					MassFlowSupBypass = MassFlowSupIn * DFFraction;
					HXSupAirVolFlowRate = MassFlowSupOut / RhoSup;
					HXSecAirVolFlowRate = ExchCond( ExNum ).SecOutMassFlow / RhoSec;
					HXAvgAirVolFlowRate = ( HXSecAirVolFlowRate + HXSupAirVolFlowRate ) / 2.0;
					HXAirVolFlowRatio = HXAvgAirVolFlowRate / ExchCond( ExNum ).NomSupAirVolFlow;
					CSup = MassFlowSupOut * PsyCpAirFnWTdb( HumRatSupIn, TempSupIn );
					CMin = min( CSup, CSec );
					if ( TempSupIn < TempSecIn ) {
						//         Use heating effectiveness values
						ExchCond( ExNum ).SensEffectiveness = ExchCond( ExNum ).HeatEffectSensible75 + ( ExchCond( ExNum ).HeatEffectSensible100 - ExchCond( ExNum ).HeatEffectSensible75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
						ExchCond( ExNum ).LatEffectiveness = ExchCond( ExNum ).HeatEffectLatent75 + ( ExchCond( ExNum ).HeatEffectLatent100 - ExchCond( ExNum ).HeatEffectLatent75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
					} else {
						//         Use cooling effectiveness values
						ExchCond( ExNum ).SensEffectiveness = ExchCond( ExNum ).CoolEffectSensible75 + ( ExchCond( ExNum ).CoolEffectSensible100 - ExchCond( ExNum ).CoolEffectSensible75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
						ExchCond( ExNum ).LatEffectiveness = ExchCond( ExNum ).CoolEffectLatent75 + ( ExchCond( ExNum ).CoolEffectLatent100 - ExchCond( ExNum ).CoolEffectLatent75 ) * ( HXAirVolFlowRatio - 0.75 ) / ( 1.0 - 0.75 );
					}
					//         calculation of local variable Csup can be 0, gaurd against divide by 0.
					TempSupOut = TempSupIn + ExchCond( ExNum ).SensEffectiveness * SafeDiv( CMin, CSup ) * ( TempSecIn - TempSupIn );
					QSensTrans = CSup * ( TempSupIn - TempSupOut );
					//         Csec cannot be 0 in this subroutine
					TempSecOut = TempSecIn + QSensTrans / CSec;
					Error = ( TempSecOut - TempThreshold );
					//         recalculate DFFraction until convergence, gaurd against divide by 0 (unlikely).
					DFFraction = max( 0.0, min( 1.0, DFFraction * SafeDiv( ( TempSecIn - TempSecOut ), ( TempSecIn - TempThreshold ) ) ) );
					++Iter;
				}
				ExchCond( ExNum ).SupInMassFlow = MassFlowSupIn;
				ExchCond( ExNum ).SupOutMassFlow = MassFlowSupOut;
				ExchCond( ExNum ).SupBypassMassFlow = MassFlowSupBypass;
			}
			ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp + ExchCond( ExNum ).SensEffectiveness * SafeDiv( CMin, CSup ) * ( ExchCond( ExNum ).SecInTemp - ExchCond( ExNum ).SupInTemp );
			ExchCond( ExNum ).SupOutHumRat = ExchCond( ExNum ).SupInHumRat + ExchCond( ExNum ).LatEffectiveness * SafeDiv( CMin, CSup ) * ( ExchCond( ExNum ).SecInHumRat - ExchCond( ExNum ).SupInHumRat );
			ExchCond( ExNum ).SupOutEnth = PsyHFnTdbW( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutHumRat );

			//   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
			if ( PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress ) > ExchCond( ExNum ).SupOutTemp ) {
				ExchCond( ExNum ).SupOutTemp = PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress );
				ExchCond( ExNum ).SupOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutEnth );
			}

			QSensTrans = CSup * ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp );
			ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp + QSensTrans / CSec;
			QTotTrans = ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupInEnth - ExchCond( ExNum ).SupOutEnth );
			ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth + QTotTrans / ExchCond( ExNum ).SecOutMassFlow;
			ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth );

			//   Perform mixing of core air stream and bypass air stream and set mass flow rates at outlet nodes
			ExchCond( ExNum ).SupOutEnth = ( ExchCond( ExNum ).SupOutMassFlow * ExchCond( ExNum ).SupOutEnth + ExchCond( ExNum ).SupBypassMassFlow * ExchCond( ExNum ).SupInEnth ) / ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SupOutHumRat = ( ExchCond( ExNum ).SupOutMassFlow * ExchCond( ExNum ).SupOutHumRat + ExchCond( ExNum ).SupBypassMassFlow * ExchCond( ExNum ).SupInHumRat ) / ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SupOutTemp = PsyTdbFnHW( ExchCond( ExNum ).SupOutEnth, ExchCond( ExNum ).SupOutHumRat );
			ExchCond( ExNum ).SupOutMassFlow = ExchCond( ExNum ).SupInMassFlow;
			ExchCond( ExNum ).SecOutEnth = ( ExchCond( ExNum ).SecOutMassFlow * ExchCond( ExNum ).SecOutEnth + ExchCond( ExNum ).SecBypassMassFlow * ExchCond( ExNum ).SecInEnth ) / ExchCond( ExNum ).SecInMassFlow;
			ExchCond( ExNum ).SecOutHumRat = ( ExchCond( ExNum ).SecOutMassFlow * ExchCond( ExNum ).SecOutHumRat + ExchCond( ExNum ).SecBypassMassFlow * ExchCond( ExNum ).SecInHumRat ) / ExchCond( ExNum ).SecInMassFlow;
			ExchCond( ExNum ).SecOutTemp = PsyTdbFnHW( ExchCond( ExNum ).SecOutEnth, ExchCond( ExNum ).SecOutHumRat );
			ExchCond( ExNum ).SecOutMassFlow = ExchCond( ExNum ).SecInMassFlow;

		} // End of IF (Minimum Exhaust Temperature)

		if ( ExchCond( ExNum ).FrostControlType == "EXHAUSTAIRRECIRCULATION" ) {
			// Directing exhaust outlet air back across the HX core on the supply side
			// Assume no heat exchange when in frost control mode, full heat exchange otherwise
			DFFraction = max( 0.0, min( ( ExchCond( ExNum ).InitialDefrostTime + ExchCond( ExNum ).RateofDefrostTimeIncrease * ( TempThreshold - ExchCond( ExNum ).SupInTemp ) ), 1.0 ) );

			//    Calculate derated heat transfer using outlet air conditions assuming no defrost (calculated earlier)
			//    and (1-DefrostFraction)
			QSensTrans = ( 1.0 - DFFraction ) * CSup * ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp );
			QTotTrans = ( 1.0 - DFFraction ) * ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupInEnth - ExchCond( ExNum ).SupOutEnth );

			ExchCond( ExNum ).SupOutMassFlow = ( 1.0 - DFFraction ) * ExchCond( ExNum ).SupInMassFlow + DFFraction * ExchCond( ExNum ).SecInMassFlow;

			//    Blend supply outlet condition of HX core with exhaust air inlet to get final supply air outlet conditions
			ExchCond( ExNum ).SupOutTemp = ( ( 1.0 - DFFraction ) * ExchCond( ExNum ).SupInMassFlow * ExchCond( ExNum ).SupOutTemp + DFFraction * ExchCond( ExNum ).SecInMassFlow * ExchCond( ExNum ).SecInTemp ) / ExchCond( ExNum ).SupOutMassFlow;

			ExchCond( ExNum ).SupOutHumRat = ( ( 1.0 - DFFraction ) * ExchCond( ExNum ).SupInMassFlow * ExchCond( ExNum ).SupOutHumRat + DFFraction * ExchCond( ExNum ).SecInMassFlow * ExchCond( ExNum ).SecInHumRat ) / ExchCond( ExNum ).SupOutMassFlow;

			ExchCond( ExNum ).SupOutEnth = PsyHFnTdbW( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutHumRat );
			//    No need to check for saturation after SA out and EA inlet are blended

			//    Derate effectiveness based on frost control time fraction for reporting purposes
			ExchCond( ExNum ).SensEffectiveness *= ( 1.0 - DFFraction );
			ExchCond( ExNum ).LatEffectiveness *= ( 1.0 - DFFraction );

			//    Secondary air outlet conditions are previously calculated as the conditions when not
			//    in defrost, and this is what we want to report so no changes here.
			//    Average SupInMassFlow and SecOutMassFlow rates have been reduced due to frost control
			//      Equipment attached to the supply inlet node may have problems with our setting the
			//      mass flow rate in the next statement. This is done only to simulate exhaust air recirc.
			Node( ExchCond( ExNum ).SupInletNode ).MassFlowRate = ExchCond( ExNum ).SupInMassFlow * ( 1.0 - DFFraction );
			ExchCond( ExNum ).SecOutMassFlow *= ( 1.0 - DFFraction );

		} // End of IF (Exhaust Air Recirculation)

		if ( ExchCond( ExNum ).FrostControlType == "EXHAUSTONLY" ) {

			//   Perform frost control by bypassing the supply air around the HX core during the defrost
			//   time period. HX heat transfer is reduced proportionally to (1 - defrosttimefraction)

			DFFraction = max( 0.0, min( ( ExchCond( ExNum ).InitialDefrostTime + ExchCond( ExNum ).RateofDefrostTimeIncrease * ( TempThreshold - ExchCond( ExNum ).SupInTemp ) ), 1.0 ) );

			//   Calculate derated heat transfer based on defrost time
			QSensTrans = ( 1.0 - DFFraction ) * CSup * ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp );
			QTotTrans = ( 1.0 - DFFraction ) * ExchCond( ExNum ).SupOutMassFlow * ( ExchCond( ExNum ).SupInEnth - ExchCond( ExNum ).SupOutEnth );

			//   Calculate the air conditions leaving heat exchanger unit
			//   Heat exchanger effectiveness is not derated, HX is fully bypassed during frost control

			ExchCond( ExNum ).SupBypassMassFlow = ExchCond( ExNum ).SupInMassFlow * DFFraction;
			ExchCond( ExNum ).SupOutTemp = ExchCond( ExNum ).SupInTemp - QSensTrans / CSup;
			ExchCond( ExNum ).SupOutEnth = ExchCond( ExNum ).SupInEnth - QTotTrans / ExchCond( ExNum ).SupOutMassFlow;
			ExchCond( ExNum ).SupOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutEnth );

			if ( PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress ) > ExchCond( ExNum ).SupOutTemp ) {
				ExchCond( ExNum ).SupOutTemp = PsyTsatFnHPb( ExchCond( ExNum ).SupOutEnth, OutBaroPress );
				ExchCond( ExNum ).SupOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SupOutTemp, ExchCond( ExNum ).SupOutEnth );
				QSensTrans = CSup * ( ExchCond( ExNum ).SupInTemp - ExchCond( ExNum ).SupOutTemp );
				// Should we be updating the sensible and latent effectiveness values also?
			}

			ExchCond( ExNum ).SecOutEnth = ExchCond( ExNum ).SecInEnth + QTotTrans / ExchCond( ExNum ).SecOutMassFlow;
			ExchCond( ExNum ).SecOutTemp = ExchCond( ExNum ).SecInTemp + QSensTrans / CSec;
			ExchCond( ExNum ).SecOutHumRat = PsyWFnTdbH( ExchCond( ExNum ).SecOutTemp, ExchCond( ExNum ).SecOutEnth );
		} // End of IF (Exhaust Only)

		ExchCond( ExNum ).DefrostFraction = DFFraction;

	}

	void
	UpdateHeatRecovery( int const ExNum ) // number of the current heat exchanger being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Moves heat exchanger output to the outlet nodes.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SupInNode; // supply inlet node number
		int SupOutNode; // supply outlet node number
		int SecInNode; // secondary inlet node number
		int SecOutNode; // secondary outlet node number

		SupInNode = ExchCond( ExNum ).SupInletNode;
		SupOutNode = ExchCond( ExNum ).SupOutletNode;
		SecInNode = ExchCond( ExNum ).SecInletNode;
		SecOutNode = ExchCond( ExNum ).SecOutletNode;

		// Set the outlet air nodes of the heat exchanger
		Node( SupOutNode ).Temp = ExchCond( ExNum ).SupOutTemp;
		Node( SupOutNode ).HumRat = ExchCond( ExNum ).SupOutHumRat;
		Node( SupOutNode ).Enthalpy = ExchCond( ExNum ).SupOutEnth;
		Node( SupOutNode ).MassFlowRate = ExchCond( ExNum ).SupOutMassFlow;
		Node( SecOutNode ).Temp = ExchCond( ExNum ).SecOutTemp;
		Node( SecOutNode ).HumRat = ExchCond( ExNum ).SecOutHumRat;
		Node( SecOutNode ).Enthalpy = ExchCond( ExNum ).SecOutEnth;
		Node( SecOutNode ).MassFlowRate = ExchCond( ExNum ).SecOutMassFlow;

		// Set the outlet nodes for properties that just pass through & not used
		Node( SupOutNode ).Quality = Node( SupInNode ).Quality;
		Node( SupOutNode ).Press = Node( SupInNode ).Press;
		Node( SupOutNode ).MassFlowRateMin = Node( SupInNode ).MassFlowRateMin;
		Node( SupOutNode ).MassFlowRateMax = Node( SupInNode ).MassFlowRateMax;
		Node( SupOutNode ).MassFlowRateMinAvail = Node( SupInNode ).MassFlowRateMinAvail;
		Node( SupOutNode ).MassFlowRateMaxAvail = Node( SupInNode ).MassFlowRateMaxAvail;
		Node( SecOutNode ).Quality = Node( SecInNode ).Quality;
		Node( SecOutNode ).Press = Node( SecInNode ).Press;
		Node( SecOutNode ).MassFlowRateMin = Node( SecInNode ).MassFlowRateMin;
		Node( SecOutNode ).MassFlowRateMax = Node( SecInNode ).MassFlowRateMax;
		Node( SecOutNode ).MassFlowRateMinAvail = Node( SecInNode ).MassFlowRateMinAvail;
		Node( SecOutNode ).MassFlowRateMaxAvail = Node( SecInNode ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( SupOutNode ).CO2 = Node( SupInNode ).CO2;
			Node( SecOutNode ).CO2 = Node( SecInNode ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( SupOutNode ).GenContam = Node( SupInNode ).GenContam;
			Node( SecOutNode ).GenContam = Node( SecInNode ).GenContam;
		}

	}

	void
	ReportHeatRecovery( int const ExNum ) // number of the current heat exchanger being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       F Buhl Nov 2000, D Shirey Feb/June 2003
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fill remaining report variables

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::AirToAirHXElecPower;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;
		ExchCond( ExNum ).ElecUseEnergy = ExchCond( ExNum ).ElecUseRate * ReportingConstant;
		ExchCond( ExNum ).SensHeatingEnergy = ExchCond( ExNum ).SensHeatingRate * ReportingConstant;
		ExchCond( ExNum ).LatHeatingEnergy = ExchCond( ExNum ).LatHeatingRate * ReportingConstant;
		ExchCond( ExNum ).TotHeatingEnergy = ExchCond( ExNum ).TotHeatingRate * ReportingConstant;
		ExchCond( ExNum ).SensCoolingEnergy = ExchCond( ExNum ).SensCoolingRate * ReportingConstant;
		ExchCond( ExNum ).LatCoolingEnergy = ExchCond( ExNum ).LatCoolingRate * ReportingConstant;
		ExchCond( ExNum ).TotCoolingEnergy = ExchCond( ExNum ).TotCoolingRate * ReportingConstant;

		AirToAirHXElecPower = ExchCond( ExNum ).ElecUseRate;

	}

	Real64
	SafeDiv(
		Real64 const a,
		Real64 const b
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Returns a / b while preventing division by zero

		// METHODOLOGY EMPLOYED:
		// Check for small or zero values before performing division

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 c;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( std::abs( b ) < SMALL ) {
			c = a / sign( SMALL, b );
		} else {
			c = a / b;
		}

		return c;

	}

	void
	CalculateEpsFromNTUandZ(
		Real64 const NTU, // number of transfer units
		Real64 const Z, // capacity rate ratio
		int const FlowArr, // flow arrangement
		Real64 & Eps // heat exchanger effectiveness
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates eps, the exchanger effectiveness,
		// from NTU, the number of transfer units,
		// from Z, the capacity rate ratio, and
		// from the flow arrangement

		// METHODOLOGY EMPLOYED:
		// Uses the effectiveness - NTU heat exchanger formulas

		// REFERENCES:
		// M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
		// LBNL Report 42354, 1999.
		// Also see:
		// ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// 1: COUNTER FLOW
		// 2: PARALLEL FLOW
		// 3: CROSS FLOW BOTH UNMIXED
		// 4: CROSS FLOW, Cmax MIXED, Cmin UNMIXED
		//    (coil with one row)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Temp; // temporary variable

		// check input validity
		if ( Z < 0.0 || Z > 1.0 ) {
			ShowFatalError( "Variable Z (" + RoundSigDigits( Z, 2 ) + ") out of range [0.0,1.0] in CalculateEpsFromNTUandZ" );
		}

		// effectiveness
		if ( NTU < SMALL ) {
			Eps = 0.0;
		} else if ( Z < SMALL ) { // Eps independent of flow arrangement
			Eps = 1.0 - std::exp( -NTU );
		} else {
			{ auto const SELECT_CASE_var( FlowArr );
			if ( SELECT_CASE_var == Counter_Flow ) { // COUNTER FLOW
				if ( std::abs( Z - 1.0 ) < SMALL ) {
					Eps = NTU / ( NTU + 1.0 );
				} else {
					Temp = std::exp( -NTU * ( 1.0 - Z ) );
					Eps = ( 1.0 - Temp ) / ( 1.0 - Z * Temp );
				}
			} else if ( SELECT_CASE_var == Parallel_Flow ) { // PARALLEL FLOW
				Temp = ( 1.0 + Z );
				Eps = ( 1.0 - std::exp( -NTU * Temp ) ) / Temp;
			} else if ( SELECT_CASE_var == Cross_Flow_Both_Unmixed ) { // CROSS FLOW BOTH UNMIXED
				Temp = Z * std::pow( NTU, -0.22 );
				Eps = 1.0 - std::exp( ( std::exp( -NTU * Temp ) - 1.0 ) / Temp );
			} else if ( SELECT_CASE_var == Cross_Flow_Other ) { // CROSS FLOW, Cmax MIXED, Cmin UNMIXED
				Eps = ( 1.0 - std::exp( -Z * ( 1.0 - std::exp( -NTU ) ) ) ) / Z;
			} else {
				ShowFatalError( "HeatRecovery: Illegal flow arrangement in CalculateEpsFromNTUandZ, Value=" + RoundSigDigits( FlowArr ) );
			}}
		}

	}

	void
	CalculateNTUfromEpsAndZ(
		Real64 & NTU, // number of transfer units
		int & Err, // error indicator
		Real64 const Z, // capacity rate ratio
		int const FlowArr, // flow arrangement
		Real64 const Eps // heat exchanger effectiveness
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates NTU, the number of transfer units,
		// based on eps, the exchanger effectiveness,
		// Z, the capacity rate ratio, and
		// from the flow arrangement

		// METHODOLOGY EMPLOYED:
		// Uses the effectiveness - NTU heat exchanger formulas

		// REFERENCES:
		// M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
		// LBNL Report 42354, 1999.
		// Also see:
		// ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// 1: COUNTER FLOW
		// 2: PARALLEL FLOW
		// 3: CROSS FLOW BOTH UNMIXED
		// 4: CROSS FLOW, Cmax MIXED, Cmin UNMIXED
		//    (coil with one row)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		NTU = 0.0;
		// check input validity
		if ( Z < 0.0 || Z > 1.0 ) {
			Err = 1;
			return;
		}

		if ( FlowArr == Parallel_Flow ) {
			if ( Eps < 0.0 || Eps > 1.0 / ( 1.0 + Z ) ) {
				Err = 2;
				return;
			}
		} else if ( FlowArr == Cross_Flow_Other ) {
			if ( Eps < 0.0 || Eps > ( 1.0 - std::exp( -Z ) ) / Z ) {
				Err = 3;
				return;
			}
			// check product (Eps*Z)
			if ( Eps * Z < 0.0 || Eps * Z > 1.0 - std::exp( Z * ( SMALL - 1.0 ) ) ) {
				Err = 4;
				return;
			}
			// check product (Eps*Z)
		} else {
			if ( Eps < 0.0 || Eps > 1.0 ) {
				Err = 5;
				return;
			}
		}

		if ( Eps < SMALL ) { // no effectiveness. Set NTU = 0
			NTU = 0.0;
		} else if ( Z < SMALL ) { // Eps independent of flow arrangement
			NTU = - std::log( 1.0 - Eps );
		} else {
			// calculate based on configuration
			{ auto const SELECT_CASE_var( FlowArr );
			if ( SELECT_CASE_var == Counter_Flow ) { // COUNTER FLOW
				if ( std::abs( Z - 1.0 ) < SMALL ) {
					NTU = Eps / ( 1.0 - Eps );
				} else {
					NTU = 1.0 / ( Z - 1.0 ) * std::log( ( 1.0 - Eps ) / ( 1.0 - Eps * Z ) );
				}
			} else if ( SELECT_CASE_var == Parallel_Flow ) { // PARALLEL FLOW
				NTU = - std::log( -Eps - Eps * Z + 1.0 ) / ( Z + 1.0 );
			} else if ( SELECT_CASE_var == Cross_Flow_Both_Unmixed ) { // CROSS FLOW BOTH UNMIXED
				NTU = GetNTUforCrossFlowBothUnmixed( Eps, Z );
			} else if ( SELECT_CASE_var == Cross_Flow_Other ) { // CROSS FLOW, Cmax MIXED, Cmin UNMIXED
				NTU = - std::log( 1.0 + std::log( 1.0 - Eps * Z ) / Z );
			} else {
				ShowFatalError( "HeatRecovery: Illegal flow arrangement in CalculateNTUfromEpsAndZ, Value=" + RoundSigDigits( FlowArr ) );
			}}
		}

	}

	Real64
	GetNTUforCrossFlowBothUnmixed(
		Real64 const Eps, // heat exchanger effectiveness
		Real64 const Z // capacity rate ratio
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates the NTU value based on the exchanger effectiveness
		// and the capacity ratio for cross flow exchanger, both
		// streams unmixed

		// METHODOLOGY EMPLOYED:
		// Uses a Regula Falsi solver function to numerically invert the formula
		// giving effectiveness as a function of NTU and Z..

		// REFERENCES:
		// M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
		// LBNL Report 42354, 1999.
		// Also see:
		// ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

		// USE STATEMENTS:
		// na

		// Return value
		Real64 NTU; // result variable; number of transfer units

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const Acc( 0.0001 ); // Accuracy of result
		int const MaxIte( 500 ); // Maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		int SolFla; // Flag of solver
		static Real64 NTU0( 0.0 ); // lower bound for NTU
		static Real64 NTU1( 50.0 ); // upper bound for NTU
		Array1D< Real64 > Par( 2 );

		Par( 1 ) = Eps;
		Par( 2 ) = Z;

		SolveRegulaFalsi( Acc, MaxIte, SolFla, NTU, GetResidCrossFlowBothUnmixed, NTU0, NTU1, Par );

		if ( SolFla == -2 ) {
			ShowFatalError( "HeatRecovery: Bad initial bounds for NTU in GetNTUforCrossFlowBothUnmixed" );
		} else if ( SolFla == -1 ) {
			ShowFatalError( "HeatRecovery: No convergence in solving for NTU in GetNTUforCrossFlowBothUnmixed" );
		}

		return NTU;
	}

	Real64
	GetResidCrossFlowBothUnmixed(
		Real64 const NTU, // number of transfer units
		Array1< Real64 > const & Par // par(1) = Eps, par(2) = Z
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   March 1999
		//       MODIFIED       Fred Buhl November 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// From the formula Eps = f(NTU,Z) this function finds the
		// residual of f(NTU,Z) - Eps for a cross flow heat exchanger,
		// both streams unmixed.

		// METHODOLOGY EMPLOYED:
		// Uses the effectiveness - NTU heat exchanger formula for cross
		// flow, both streams unmixed.

		// REFERENCES:
		// M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
		// LBNL Report 42354, 1999.
		// Also see:
		// ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Residuum = 1.0 - std::exp( ( std::exp( -std::pow( NTU, 0.78 ) * Par( 2 ) ) - 1.0 ) / Par( 2 ) * std::pow( NTU, 0.22 ) ) - Par( 1 );

		return Residuum;

	}

	void
	CheckModelBoundsTempEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 & T_RegenInTemp, // current regen inlet temperature (C) for regen outlet temp eqn
		Real64 & T_RegenInHumRat, // current regen inlet hum rat for regen outlet temp eqn
		Real64 & T_ProcInTemp, // current process inlet temperature (C) for regen outlet temp eqn
		Real64 & T_ProcInHumRat, // current process inlet hum rat for regen outlet temp eqn
		Real64 & T_FaceVel, // current process and regen face velocity (m/s)
		bool const FirstHVACIteration // First HVAC iteration flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar, FSEC
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the empirical model's independent variables are within the limits used during the
		// developement of the empirical model.

		// METHODOLOGY EMPLOYED:
		// The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
		// Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
		// The range of each independent variable is provided by the user and are based on the limits of the
		// empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
		// routine.
		// REFERENCES:
		// na

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS
		// regen outlet temp equation

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string OutputChar; // character string for warning messages
		static std::string OutputCharLo; // character string for warning messages
		static std::string OutputCharHi; // character string for warning messages
		static std::string CharValue; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {

			// print error for variables of regeneration outlet temperature equation
			// Regen inlet temp for temp eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInTempMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempBuffer3 );
					ShowContinueError( "...Using regeneration inlet air temperatures that are outside the regeneration outlet air temperature equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air temp used in regen outlet air temperature equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempLast );
				}
			}
			// Regen inlet humidity ratio for temp eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInHumRatMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatBuffer3 );
					ShowContinueError( "...Using regeneration inlet air humidity ratios that are outside the regeneration outlet air temperature equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air humidity ratio used in regen outlet temperature equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatLast );
				}
			}
			// Process inlet temp for temp eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInTempMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempBuffer3 );
					ShowContinueError( "...Using process inlet air temperatures that are outside the regeneration outlet air temperature equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air temperature used in regen outlet temperature equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempLast );
				}
			}
			// Process inlet humidity ratio for temp eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInHumRatMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatBuffer3 );
					ShowContinueError( "...Using process inlet air humidity ratios that are outside the regeneratoin outlet air temperature equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air humidity ratio used in regen outlet temperature equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatLast );
				}
			}
			// Process and regeneration face velocity for temp eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_FaceVelMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelBuffer3 );
					ShowContinueError( "...Using process and regeneration face velocities that are outside the regeneration outlet air temperature equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process and regen inlet air face velocity used in regen outlet temperature equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelocityErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelLast );
				}
			}
		} // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//   If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
		if ( std::abs( T_RegenInTemp - T_ProcInTemp ) < SMALL ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInTempMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInHumRatMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInTempMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInHumRatMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_FaceVelMessage = false;
			return;
		}

		//   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
		// checking model bounds for variables of regeneration outlet temperature equation
		// Regen inlet temp
		if ( T_RegenInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInTemp || T_RegenInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInTemp ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempLast = T_RegenInTemp;
			OutputChar = RoundSigDigits( T_RegenInTemp, 2 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInTemp, 2 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInTemp, 2 );
			if ( T_RegenInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInTemp ) {
				T_RegenInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInTemp;
			}
			if ( T_RegenInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInTemp ) {
				T_RegenInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInTemp;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInTempMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air temperature used in regen outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( T_RegenInTemp, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInTempBuffer3 = "...Regeneration outlet air temperature equation: regeneration inlet air temperature passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInTempMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInTempMessage = false;
		}
		// regen inlet humidity ratio
		if ( T_RegenInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInHumRat || T_RegenInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInHumRat ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatLast = T_RegenInHumRat;
			OutputChar = RoundSigDigits( T_RegenInHumRat, 6 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInHumRat, 6 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInHumRat, 6 );
			if ( T_RegenInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInHumRat ) {
				T_RegenInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInHumRat;
			}
			if ( T_RegenInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInHumRat ) {
				T_RegenInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInHumRat;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInHumRatMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air humidity ratio used in regen outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( T_RegenInHumRat, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_RegenInHumRatBuffer3 = "...Regeneration outlet air temperature equation: regeneration inlet air humidity ratio passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInHumRatMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_RegenInHumRatMessage = false;
		}
		// process inlet temp
		if ( T_ProcInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInTemp || T_ProcInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInTemp ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempLast = T_ProcInTemp;
			OutputChar = RoundSigDigits( T_ProcInTemp, 2 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInTemp, 2 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInTemp, 2 );
			if ( T_ProcInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInTemp ) {
				T_ProcInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInTemp;
			}
			if ( T_ProcInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInTemp ) {
				T_ProcInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInTemp;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInTempMessage = true;
				//       Suppress warning message when process inlet temperature = 0 (DX coil is off)
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempLast == 0.0 ) BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInTempMessage = false;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air temperature used in regen outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ',' + CurMnDy + ' ' + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( T_ProcInTemp, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInTempBuffer3 = "...Regeneration outlet air temperature equation: process inlet air temperature passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInTempMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInTempMessage = false;
		}
		// process inlet humidity ratio
		if ( T_ProcInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInHumRat || T_ProcInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInHumRat ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatLast = T_ProcInHumRat;
			OutputChar = RoundSigDigits( T_ProcInHumRat, 6 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInHumRat, 6 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInHumRat, 6 );
			if ( T_ProcInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInHumRat ) {
				T_ProcInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInHumRat;
			}
			if ( T_ProcInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInHumRat ) {
				T_ProcInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInHumRat;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInHumRatMessage = true;
				//       Suppress warning message when process inlet humrat = 0 (DX coil is off)
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatLast == 0.0 ) BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInHumRatMessage = false;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air humidity ratio used in regen outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( T_ProcInHumRat, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatBuffer3 = "...Regeneration outlet air temperature equation: process inlet air humidity ratio passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInHumRatMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_ProcInHumRatMessage = false;
		}
		// regeneration and process face velocity
		if ( T_FaceVel < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinFaceVel || T_FaceVel > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxFaceVel ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelLast = T_FaceVel;
			OutputChar = RoundSigDigits( T_FaceVel, 6 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinFaceVel, 6 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxFaceVel, 6 );
			if ( T_FaceVel < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinFaceVel ) {
				T_FaceVel = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinFaceVel;
			}
			if ( T_FaceVel > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxFaceVel ) {
				T_FaceVel = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxFaceVel;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_FaceVelMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process and regen inlet air face velocity used in regen outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( T_FaceVel, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_FaceVelBuffer3 = "...Regeneration outlet air temperature equation: process and regen face velocity passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_FaceVelMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintT_FaceVelMessage = false;
		}

	}

	void
	CheckModelBoundsHumRatEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 & H_RegenInTemp, // current regen inlet temperature (C) for regen outlet hum rat eqn
		Real64 & H_RegenInHumRat, // current regen inlet hum rat for regen outlet hum rat eqn
		Real64 & H_ProcInTemp, // current process inlet temperature (C) for regen outlet hum rat eqn
		Real64 & H_ProcInHumRat, // current process inlet hum rat for regen outlet hum rat eqn
		Real64 & H_FaceVel, // current process and regen face velocity (m/s)
		bool const FirstHVACIteration // First HVAC iteration flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar, FSEC
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the empirical model's independent variables are within the limits used during the
		// developement of the empirical model.

		// METHODOLOGY EMPLOYED:
		// The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
		// Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
		// The range of each independent variable is provided by the user and are based on the limits of the
		// empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
		// routine.
		// REFERENCES:
		// na

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// regen outlet humidity ratio equation

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string OutputChar; // character string for warning messages
		static std::string OutputCharLo; // character string for warning messages
		static std::string OutputCharHi; // character string for warning messages
		static std::string CharValue; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {

			// print error for variables of regeneration outlet humidity ratio equation
			// Regen inlet temp for humidity ratio eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInTempMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempBuffer3 );
					ShowContinueError( "...Using regeneration inlet air temperatures that are outside the regeneration inlet air temperature equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air temperature used in regen outlet air humidity ratio equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempLast );
				}
			}
			// Regen inlet humidity ratio for humidity ratio eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInHumRatMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatBuffer3 );
					ShowContinueError( "...Using regeneration inlet air humidity ratios that are outside the regeneration outlet air humidity ratio equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air humidity ratio used in regen outlet air humidity ratio equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatLast );
				}
			}
			// Process inlet temp for humidity ratio eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInTempMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempBuffer3 );
					ShowContinueError( "...Using process inlet air temperatures that are outside the regeneration outlet air humidity ratio equation model may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air temperature used in regen outlet air humidity ratio equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempLast );
				}
			}
			// Process inlet humidity ratio for humidity ratio eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInHumRatMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatBuffer3 );
					ShowContinueError( "...Using process inlet air humidity ratios that are outside the regeneration outlet humidity ratio equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air humidity ratio used in regen outlet air humidity ratio equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_ProcInHumRatErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatLast );
				}
			}
			// Process and regeneration face velocity for humidity ratio eqn
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_FaceVelMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelBuffer3 );
					ShowContinueError( "...Using process and regeneration face velocities that are outside the regeneration outlet air humidity ratio equation model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process and regen face velocity used in regen outlet air humidity ratio equation is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelocityErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelLast );
				}
			}
		} // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//   If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
		if ( std::abs( H_RegenInTemp - H_ProcInTemp ) < SMALL ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInTempMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInHumRatMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInTempMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInHumRatMessage = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_FaceVelMessage = false;
			return;
		}

		//   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
		// checking model bounds for variables of regeneration outlet humidity ratio equation
		// Regen inlet temp
		if ( H_RegenInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInTemp || H_RegenInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInTemp ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempLast = H_RegenInTemp;
			OutputChar = RoundSigDigits( H_RegenInTemp, 2 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInTemp, 2 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInTemp, 2 );
			if ( H_RegenInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInTemp ) {
				H_RegenInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInTemp;
			}
			if ( H_RegenInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInTemp ) {
				H_RegenInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInTemp;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInTempMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air temperature used in regen outlet air humidity ratio equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + " , " + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( H_RegenInTemp, 2 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInTempBuffer3 = "...Regeneration outlet air humidity ratio equation: regeneration inlet air temperature passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInTempMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInTempMessage = false;
		}
		// regen inlet humidity ratio
		if ( H_RegenInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInHumRat || H_RegenInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInHumRat ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatLast = H_RegenInHumRat;
			OutputChar = RoundSigDigits( H_RegenInHumRat, 6 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInHumRat, 6 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInHumRat, 6 );
			if ( H_RegenInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInHumRat ) {
				H_RegenInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInHumRat;
			}
			if ( H_RegenInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInHumRat ) {
				H_RegenInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInHumRat;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInHumRatMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air humidity ratio used in regen outlet air humidity ratio equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( H_RegenInHumRat, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_RegenInHumRatBuffer3 = "...Regeneration outlet air humidity ratio equation: regeneration inlet air humidity ratio passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInHumRatMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_RegenInHumRatMessage = false;
		}
		// process inlet temp
		if ( H_ProcInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInTemp || H_ProcInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInTemp ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempLast = H_ProcInTemp;
			OutputChar = RoundSigDigits( H_ProcInTemp, 2 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInTemp, 2 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInTemp, 2 );
			if ( H_ProcInTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInTemp ) {
				H_ProcInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInTemp;
			}
			if ( H_ProcInTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInTemp ) {
				H_ProcInTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInTemp;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInTempMessage = true;
				//       Suppress warning message when process inlet temperature = 0 (DX coil is off)
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempLast == 0.0 ) BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInTempMessage = false;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air temperature used in regen outlet air humidity ratio equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( H_ProcInTemp, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInTempBuffer3 = "...Regeneration outlet air humidity ratio equation: process inlet air temperature passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInTempMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInTempMessage = false;
		}
		// process inlet humidity ratio
		if ( H_ProcInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInHumRat || H_ProcInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInHumRat ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatLast = H_ProcInHumRat;
			OutputChar = RoundSigDigits( H_ProcInHumRat, 6 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInHumRat, 6 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInHumRat, 6 );
			if ( H_ProcInHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInHumRat ) {
				H_ProcInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInHumRat;
			}
			if ( H_ProcInHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInHumRat ) {
				H_ProcInHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInHumRat;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInHumRatMessage = true;
				//       Suppress warning message when process inlet humrat = 0 (DX coil is off)
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatLast == 0.0 ) BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInHumRatMessage = false;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air humidity ratio used in regen outlet air humidity ratio equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( H_ProcInHumRat, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_ProcInHumRatBuffer3 = "...Regeneration outlet air humidity ratio equation: process inlet air humidity ratio passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInHumRatMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_ProcInHumRatMessage = false;
		}
		// regeneration and process face velocity
		if ( H_FaceVel < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinFaceVel || H_FaceVel > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxFaceVel ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelLast = H_FaceVel;
			OutputChar = RoundSigDigits( H_FaceVel, 6 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinFaceVel, 6 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxFaceVel, 6 );
			if ( H_FaceVel < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinFaceVel ) {
				H_FaceVel = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinFaceVel;
			}
			if ( H_FaceVel > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxFaceVel ) {
				H_FaceVel = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxFaceVel;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_FaceVelMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process and regen inlet air face velocity used in regen outlet air humidity ratio equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( H_FaceVel, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_FaceVelBuffer3 = "...Regeneration outlet air humidity ratio equation: process and regeneration face velocity passed to the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_FaceVelMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintH_FaceVelMessage = false;
		}

	}

	void
	CheckModelBoundOutput_Temp(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const RegenInTemp, // current regen inlet temp passed to eqn
		Real64 & RegenOutTemp, // current regen outlet temp from eqn
		bool const FirstHVACIteration // First HVAC iteration flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar, FSEC
		//       DATE WRITTEN   January 2007
		//       MODIFIED       June 2007, R. Raustad, changed requirement that regen outlet temp be less than inlet temp
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the empirical model's independent variables are within the limits used during the
		// developement of the empirical model.

		// METHODOLOGY EMPLOYED:
		// The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
		// Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
		// The range of each independent variable is provided by the user and are based on the limits of the
		// empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
		// routine.
		// REFERENCES:
		// na

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string OutputChar; // character string for warning messages
		static std::string OutputCharLo; // character string for warning messages
		static std::string OutputCharHi; // character string for warning messages
		static std::string CharValue; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {

			// print error when regeneration outlet temperature is greater than regen inlet temperature
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempFailedMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedBuffer2 );
					ShowContinueError( "...Regeneration outlet air temperature should always be less than or equal to regen inlet air temperature. Verify correct model coefficients." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air temperature above regen inlet air temperature error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedLast );
				}
			}

			// print error for variables of regeneration outlet temperature
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempBuffer3 );
					ShowContinueError( "...Regeneration outlet air temperature should always be less than or equal to regen inlet air temperature. Verify correct model coefficients." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air temperature should be less than regen inlet air temperature error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempLast );
				}
			}
		} // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		// checking model regeneration outlet temperature to always be less than or equal to regeneration inlet temperature
		if ( RegenOutTemp > RegenInTemp ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedLast = RegenOutTemp;
			OutputChar = RoundSigDigits( RegenOutTemp, 2 );
			OutputCharHi = RoundSigDigits( RegenInTemp, 2 );
			//      IF(RegenOutTemp .GT. RegenInTemp)THEN
			//        RegenOutTemp = RegenInTemp
			//      END IF
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempFailedMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air temperature is greater than inlet temperature at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedBuffer2 = "...Regen inlet air temperature = " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( RegenOutTemp, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempFailedBuffer3 = "...Regen outlet air temperature equation: regeneration outlet air temperature allowed from the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempMessage = false;
		}

		//   check boundaries of regen outlet temperature and post warnings to individual buffers to print at end of time step
		// checking model bounds for regeneration outlet temperature
		if ( RegenOutTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutTemp || RegenOutTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutTemp ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempLast = RegenOutTemp;
			OutputChar = RoundSigDigits( RegenOutTemp, 2 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutTemp, 2 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutTemp, 2 );
			if ( RegenOutTemp < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutTemp ) {
				RegenOutTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutTemp;
			}
			if ( RegenOutTemp > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutTemp ) {
				RegenOutTemp = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutTemp;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( RegenOutTemp, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutTempBuffer3 = "...Regen outlet air temperature equation: regeneration outlet air temperature allowed from the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutTempMessage = false;
		}

	}

	void
	CheckModelBoundOutput_HumRat(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const RegenInHumRat, // current regen inlet hum rat passed to eqn
		Real64 & RegenOutHumRat, // current regen outlet hum rat from eqn
		bool const FirstHVACIteration // First HVAC iteration flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar, FSEC
		//       DATE WRITTEN   January 2007
		//       MODIFIED       June 2007, R. Raustad, changed requirement that regen outlet temp be less than inlet temp
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the empirical model's independent variables are within the limits used during the
		// developement of the empirical model.

		// METHODOLOGY EMPLOYED:
		// The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
		// Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
		// The range of each independent variable is provided by the user and are based on the limits of the
		// empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
		// routine.
		// REFERENCES:
		// na

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string OutputChar; // character string for warning messages
		static std::string OutputCharLo; // character string for warning messages
		static std::string OutputCharHi; // character string for warning messages
		static std::string CharValue; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {

			// print error when regeneration outlet humidity ratio is less than regeneration inlet humidity ratio
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatFailedMess ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedBuffer2 );
					ShowContinueError( "...Regeneration outlet air humidity ratio should always be greater than or equal to regen inlet air humidity ratio. Verify correct model coefficients." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air humidity ratio should be greater than regen inlet air humidity ratio error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedLast );
				}
			}

			// print error for regeneration outlet humidity ratio
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatMessage ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatBuffer3 );
					ShowContinueError( "...Regeneration outlet air humidity ratio outside model boundaries may adversely affect desiccant model performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air humidity ratio is out of range error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatLast );
				}
			}
		} // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		// checking for regeneration outlet humidity ratio less than or equal to regeneration inlet humidity ratio
		if ( RegenOutHumRat < RegenInHumRat ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedLast = RegenOutHumRat;
			OutputChar = RoundSigDigits( RegenOutHumRat, 6 );
			OutputCharHi = RoundSigDigits( RegenInHumRat, 6 );
			//      IF(RegenOutHumRat .LT. RegenInHumRat)THEN
			//        RegenOutHumRat = RegenInHumRat
			//      END IF
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatFailedMess = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air humidity ratio is less than the inlet air humidity ratio at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedBuffer2 = "...Regen inlet air humidity ratio = " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( RegenOutHumRat, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatFailedBuffer3 = "...Regen outlet air humidity ratio equation: regeneration outlet air humidity ratio allowed from the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatFailedMess = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatFailedMess = false;
		}

		//   check boundaries of regen outlet humrat and post warnings to individual buffers to print at end of time step
		// checking model bounds for regeneration outlet humidity ratio
		if ( RegenOutHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutHumRat || RegenOutHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutHumRat ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatLast = RegenOutHumRat;
			OutputChar = RoundSigDigits( RegenOutHumRat, 6 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutHumRat, 6 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutHumRat, 6 );
			if ( RegenOutHumRat < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutHumRat ) {
				RegenOutHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MinRegenAirOutHumRat;
			}
			if ( RegenOutHumRat > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutHumRat ) {
				RegenOutHumRat = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).MaxRegenAirOutHumRat;
			}
			if ( ! WarmupFlag && ! FirstHVACIteration ) {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatMessage = true;
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration outlet air humidity ratio is outside model boundaries at " + OutputChar + '.';
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatBuffer2 = "...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
				CharValue = RoundSigDigits( RegenOutHumRat, 6 );
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenOutHumRatBuffer3 = "...Regen outlet air humidity ratio equation: regeneration outlet air humidity ratio allowed from the model = " + CharValue;
			} else {
				BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatMessage = false;
			}
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenOutHumRatMessage = false;
		}

	}

	void
	CheckModelBoundsRH_TempEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const T_RegenInTemp, // current regen inlet temperature passed to eqn
		Real64 const T_RegenInHumRat, // current regen inlet hum rat passed to eqn
		Real64 const T_ProcInTemp, // current process inlet temperature passed to eqn
		Real64 const T_ProcInHumRat, // current regen outlet hum rat from eqn
		bool const FirstHVACIteration // first HVAC iteration flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the empirical model's independent variables result in a relative humidity that is within the range
		// of relative humidities used when creating the empirical model. Both the regeneration and process inlet are tested.

		// METHODOLOGY EMPLOYED:
		// The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
		// Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
		// In addition, the range of relative humidities in the original data set may influence the output of the
		// empirical model. This subroutine tests the relative humidities passed to the empirical model and warns the
		// user if these relative humidities are out of bounds based on the limits set by the user.
		// REFERENCES:
		// na

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using Psychrometrics::PsyRhFnTdbWPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 RegenInletRH( 0.0 ); // Regeneration inlet air relative humidity
		static Real64 ProcInletRH( 0.0 ); // Process inlet air relative humidity
		static std::string OutputChar; // character string for warning messages
		static std::string OutputCharLo; // character string for warning messages
		static std::string OutputCharHi; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed

		if ( WarmupFlag || FirstHVACIteration ) return;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {

			// print error when regeneration inlet relative humidity is outside model boundaries
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumTempMess ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempBuffer3 );
					ShowContinueError( "...Using regeneration inlet air relative humidities that are outside the regeneration outlet temperature equation model boundaries may adversely affect desiccant model performance. Verify correct model coefficients." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air relative humidity related to regen outlet air temperature equation is outside model boundaries error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempLast );
				}
			}

			// print error when process inlet relative humidity is outside model boundaries
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumTempMess ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempBuffer3 );
					ShowContinueError( "...Using process inlet air relative humidities that are outside the regeneration outlet temperature equation model boundaries may adversely affect desiccant model performance. Verify correct model coefficients." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air relative humidity related to regen outlet air temperature equation is outside model boundaries error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempLast );
				}
			}

		} // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

		//     save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//     Check that condition is not above saturation curve prior to next calc (PsyRhFnTdbWPb) to avoid psyc routine errors
		//                           *
		//                          *
		//                  x------*---------- T_HumRat
		//                  |    *
		//                  |  *
		//                  *----------------- PsyWFnTdpPb(Tdp,Pb)
		//               *  |
		//                  |
		//                T_Temp
		if ( T_RegenInHumRat > PsyWFnTdpPb( T_RegenInTemp, OutBaroPress ) || T_ProcInHumRat > PsyWFnTdpPb( T_ProcInTemp, OutBaroPress ) ) {
			//       reset RH print flags just in case
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumTempMess = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumTempMess = false;
			return;
		}

		//     If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
		if ( std::abs( T_RegenInTemp - T_ProcInTemp ) < SMALL ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumTempMess = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumTempMess = false;
			return;
		}

		RegenInletRH = PsyRhFnTdbWPb( T_RegenInTemp, T_RegenInHumRat, OutBaroPress );
		ProcInletRH = min( 1.0, PsyRhFnTdbWPb( T_ProcInTemp, T_ProcInHumRat, OutBaroPress ) );

		// checking if regeneration inlet relative humidity is within model boundaries
		if ( RegenInletRH < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInRelHum || RegenInletRH > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInRelHum ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempLast = RegenInletRH * 100.0;
			OutputChar = RoundSigDigits( RegenInletRH * 100.0, 1 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinRegenAirInRelHum * 100.0, 1 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxRegenAirInRelHum * 100.0, 1 );
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumTempMess = true;

			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air relative humidity related to regen outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempBuffer2 = "...Model limit on regeneration inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumTempBuffer3 = "...Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumTempMess = false;
		}

		// checking if process inlet relative humidity is within model boundaries
		if ( ProcInletRH < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInRelHum || ProcInletRH > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInRelHum ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempLast = ProcInletRH * 100.0;
			OutputChar = RoundSigDigits( ProcInletRH * 100.0, 1 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MinProcAirInRelHum * 100.0, 1 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).T_MaxProcAirInRelHum * 100.0, 1 );
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumTempMess = true;

			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air relative humidity related to regen outlet air temperature equation is outside model boundaries at " + OutputChar + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempBuffer2 = "...Model limit on process inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumTempBuffer3 = "...Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumTempMess = false;
		}

	}

	void
	CheckModelBoundsRH_HumRatEq(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const H_RegenInTemp, // current regen inlet temperature passed to eqn
		Real64 const H_RegenInHumRat, // current regen inlet hum rat passed to eqn
		Real64 const H_ProcInTemp, // current process inlet temperature passed to eqn
		Real64 const H_ProcInHumRat, // current process inlet hum rat passed to eqn
		bool const FirstHVACIteration // first HVAC iteration flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the empirical model's independent variables result in a relative humidity that is within the range
		// of relative humidities used when creating the empirical model. Both the regeneration and process inlet are tested.

		// METHODOLOGY EMPLOYED:
		// The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
		// Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
		// In addition, the range of relative humidities in the original data set may influence the output of the
		// empirical model. This subroutine tests the relative humidities passed to the empirical model and warns the
		// user if these relative humidities are out of bounds based on the limits set by the user.
		// REFERENCES:
		// na

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using Psychrometrics::PsyRhFnTdbWPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 RegenInletRH( 0.0 ); // Regeneration inlet air relative humidity
		static Real64 ProcInletRH( 0.0 ); // Process inlet air relative humidity
		static std::string OutputChar; // character string for warning messages
		static std::string OutputCharLo; // character string for warning messages
		static std::string OutputCharHi; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed

		if ( WarmupFlag || FirstHVACIteration ) return;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {

			// print error when regeneration inlet relative humidity is outside model boundaries
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumHumRatMess ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatBuffer3 );
					ShowContinueError( "...Using regeneration inlet air relative humidities that are outside the regeneration outlet humidity ratio equation model boundaries may adversely affect desiccant model performance. Verify correct model coefficients." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air relative humidity related to regen outlet air humidity ratio equation is outside model boundaries error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatLast );
				}
			}

			// print error when process inlet relative humidity is outside model boundaries
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumHumRatMess ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatBuffer3 );
					ShowContinueError( "...Using process inlet air relative humidities that are outside the regeneration outlet humidity ratio equation model boundaries may adversely affect desiccant model performance. Verify correct model coefficients." );
				} else {
					ShowRecurringWarningErrorAtEnd( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air relative humidity related to regen outlet air humidity ratio equation is outside model boundaries error continues...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatLast, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatLast );
				}
			}

		} // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

		//     save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//     Check that condition is not above saturation curve prior to next calc (PsyRhFnTdbWPb) to avoid psyc routine errors
		//                           *
		//                          *
		//                  x------*---------- H_HumRat
		//                  |    *
		//                  |  *
		//                  *----------------- PsyWFnTdpPb(Tdp,Pb)
		//               *  |
		//                  |
		//                H_Temp
		if ( H_RegenInHumRat > PsyWFnTdpPb( H_RegenInTemp, OutBaroPress ) || H_ProcInHumRat > PsyWFnTdpPb( H_ProcInTemp, OutBaroPress ) ) {
			//       reset RH print flags just in case
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumHumRatMess = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumHumRatMess = false;
			return;
		}

		//     If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
		if ( std::abs( H_RegenInTemp - H_ProcInTemp ) < SMALL ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumHumRatMess = false;
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumHumRatMess = false;
			return;
		}

		RegenInletRH = PsyRhFnTdbWPb( H_RegenInTemp, H_RegenInHumRat, OutBaroPress );
		ProcInletRH = min( 1.0, PsyRhFnTdbWPb( H_ProcInTemp, H_ProcInHumRat, OutBaroPress ) );

		// checking if regeneration inlet relative humidity is within model boundaries
		if ( RegenInletRH < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInRelHum || RegenInletRH > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInRelHum ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatLast = RegenInletRH * 100.0;
			OutputChar = RoundSigDigits( RegenInletRH * 100.0, 1 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinRegenAirInRelHum * 100.0, 1 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxRegenAirInRelHum * 100.0, 1 );
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumHumRatMess = true;

			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Regeneration inlet air relative humidity related to regen outlet air humidity ratio equation is outside model boundaries at " + OutputChar + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatBuffer2 = "...Model limit on regeneration inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).RegenInRelHumHumRatBuffer3 = "...Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintRegenInRelHumHumRatMess = false;
		}

		// checking if process inlet relative humidity is within model boundaries
		if ( ProcInletRH < BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInRelHum || ProcInletRH > BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInRelHum ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatLast = ProcInletRH * 100.0;
			OutputChar = RoundSigDigits( ProcInletRH * 100.0, 1 );
			OutputCharLo = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MinProcAirInRelHum * 100.0, 1 );
			OutputCharHi = RoundSigDigits( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).H_MaxProcAirInRelHum * 100.0, 1 );
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumHumRatMess = true;

			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatBuffer1 = BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PerfType + " \"" + BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).Name + "\" - Process inlet air relative humidity related to regen outlet air humidity ratio equation is outside model boundaries at " + OutputChar + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatBuffer2 = "...Model limit on process inlet air relative humidity is " + OutputCharLo + " to " + OutputCharHi + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ProcInRelHumHumRatBuffer3 = "...Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintProcInRelHumHumRatMess = false;
		}

	}

	void
	CheckForBalancedFlow(
		int const ExchNum, // number of the current heat exchanger being simulated
		Real64 const ProcessInMassFlow, // current process inlet air mass flow rate (m3/s)
		Real64 const RegenInMassFlow, // current regeneration inlet air mass flow rate (m3/s)
		bool const FirstHVACIteration // first HVAC iteration flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the balanced flow desiccant heat exchanger has the same regeneration and process air flow rates.

		// METHODOLOGY EMPLOYED:
		// Check that the regeneration and process air mass flow rates are within 2%.
		// REFERENCES:
		// na

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string OutputCharProc; // character string for warning messages
		static std::string OutputCharRegen; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed
		Real64 ABSImbalancedFlow; // absolute value of process and regeneration air flow imbalance fraction

		if ( WarmupFlag || FirstHVACIteration ) return;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {

			// print error when regeneration inlet relative humidity is outside model boundaries
			if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintImbalancedMassFlowMess ) {
				++BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowErrorCount;
				if ( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowErrorCount < 2 ) {
					ShowWarningError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowBuffer1 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowBuffer2 );
					ShowContinueError( BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowBuffer3 );
					//           CALL ShowContinueError('...Using regeneration inlet air relative humidities that are outside the regeneration '&
					//                 //'outlet humidity ratio equation model boundaries may adversely affect desiccant model performance. '&
					//                 //'Verify correct model coefficients.')
				} else {
					ShowRecurringWarningErrorAtEnd( cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + " \"" + ExchCond( ExchNum ).Name + "\" - unbalanced air flow rate is limited to 2% error continues with the imbalanced fraction statistics reported...", BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedFlowErrIndex, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ABSImbalancedFlow, BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ABSImbalancedFlow );
				}
			}

		} // IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

		//     save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		// checking if regeneration inlet relative humidity is within model boundaries
		ABSImbalancedFlow = std::abs( RegenInMassFlow - ProcessInMassFlow ) / RegenInMassFlow;
		if ( ABSImbalancedFlow > 0.02 ) {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ABSImbalancedFlow = ABSImbalancedFlow;
			OutputCharRegen = RoundSigDigits( RegenInMassFlow, 6 );
			OutputCharProc = RoundSigDigits( ProcessInMassFlow, 6 );
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintImbalancedMassFlowMess = true;

			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowBuffer1 = cHXTypes( ExchCond( ExchNum ).ExchTypeNum ) + " \"" + ExchCond( ExchNum ).Name + "\" - unbalanced air flow rate is limited to 2%.";
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowBuffer2 = "...Regeneration air mass flow rate is " + OutputCharRegen + " and process air mass flow rate is " + OutputCharProc + '.';
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).ImbalancedMassFlowBuffer3 = "...Occurrence info = " + EnvironmentName + ", " + CurMnDy + ", " + CreateSysTimeIntervalString();
		} else {
			BalDesDehumPerfData( ExchCond( ExchNum ).PerfDataIndex ).PrintImbalancedMassFlowMess = false;
		}

	}

	int
	GetSupplyInletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   February 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given HX and returns the supply air inlet node number.
		// If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetSupplyInletNode; // node number returned

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichHX;

		// Obtains and Allocates heat exchanger related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		WhichHX = FindItemInList( HXName, ExchCond );
		if ( WhichHX != 0 ) {
			GetSupplyInletNode = ExchCond( WhichHX ).SupInletNode;
		} else {
			ShowSevereError( "GetSupplyInletNode: Could not find heat exchanger = \"" + HXName + "\"" );
			ErrorsFound = true;
			GetSupplyInletNode = 0;
		}

		return GetSupplyInletNode;

	}

	int
	GetSupplyOutletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   February 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given HX and returns the supply air outlet node number.
		// If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetSupplyOutletNode; // node number returned

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichHX;

		// Obtains and Allocates heat exchanger related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		WhichHX = FindItemInList( HXName, ExchCond );
		if ( WhichHX != 0 ) {
			GetSupplyOutletNode = ExchCond( WhichHX ).SupOutletNode;
		} else {
			ShowSevereError( "GetSupplyOutletNode: Could not find heat exchanger = \"" + HXName + "\"" );
			ErrorsFound = true;
			GetSupplyOutletNode = 0;
		}

		return GetSupplyOutletNode;

	}

	int
	GetSecondaryInletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   February 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given HX and returns the secondary air inlet node number.
		// If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetSecondaryInletNode; // node number returned

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichHX;

		// Obtains and Allocates heat exchanger related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		WhichHX = FindItemInList( HXName, ExchCond );
		if ( WhichHX != 0 ) {
			GetSecondaryInletNode = ExchCond( WhichHX ).SecInletNode;
		} else {
			ShowSevereError( "GetSecondaryInletNode: Could not find heat exchanger = \"" + HXName + "\"" );
			ErrorsFound = true;
			GetSecondaryInletNode = 0;
		}

		return GetSecondaryInletNode;

	}

	int
	GetSecondaryOutletNode(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   February 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given HX assisted cooling coil and returns the secondary air outlet node number.
		// If incorrect HX name is given, ErrorsFound is returned as true and node number as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetSecondaryOutletNode; // node number returned

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichHX;

		// Obtains and Allocates heat exchanger related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		WhichHX = FindItemInList( HXName, ExchCond );
		if ( WhichHX != 0 ) {
			GetSecondaryOutletNode = ExchCond( WhichHX ).SecOutletNode;
		} else {
			ShowSevereError( "GetSecondaryOutletNode: Could not find heat exchanger = \"" + HXName + "\"" );
			ErrorsFound = true;
			GetSecondaryOutletNode = 0;
		}

		return GetSecondaryOutletNode;

	}

	Real64
	GetSupplyAirFlowRate(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given Generic HX and the voluetric air flow rate.
		// If incorrect HX name is given, ErrorsFound is returned as true and air flow rate as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Real64 GetSupplyAirFlowRate; // air flow rate returned

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichHX;

		// Obtains and Allocates heat exchanger related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		WhichHX = FindItemInList( HXName, ExchCond );
		if ( WhichHX != 0 ) {
			GetSupplyAirFlowRate = ExchCond( WhichHX ).NomSupAirVolFlow;
		} else {
			ShowSevereError( "GetSupplyAirFlowRate: Could not find heat exchanger = \"" + HXName + "\"" );
			ShowContinueError( "... Supply Air Flow Rate returned as 0." );
			ErrorsFound = true;
			GetSupplyAirFlowRate = 0.0;
		}

		return GetSupplyAirFlowRate;

	}

	int
	GetHeatExchangerObjectTypeNum(
		std::string const & HXName, // must match HX names for the ExchCond type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given Generic HX and the voluetric air flow rate.
		// If incorrect HX name is given, ErrorsFound is returned as true and air flow rate as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetHeatExchangerObjectTypeNum; // object type parameter returned

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichHX;

		// Obtains and Allocates heat exchanger related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		WhichHX = FindItemInList( HXName, ExchCond );
		if ( WhichHX != 0 ) {
			GetHeatExchangerObjectTypeNum = ExchCond( WhichHX ).ExchTypeNum;
		} else {
			ShowSevereError( "GetHeatExchangerObjectTypeNum: Could not find heat exchanger = \"" + HXName + "\"" );
			ErrorsFound = true;
			GetHeatExchangerObjectTypeNum = 0;
		}

		return GetHeatExchangerObjectTypeNum;

	}

	void
	SetHeatExchangerData(
		int const HXNum, // Index of HX
		bool & ErrorsFound, // Set to true if certain errors found
		std::string const & HXName, // Name of HX
		Optional< Real64 > SupplyAirVolFlow, // HX supply air flow rate    [m3/s]
		Optional< Real64 > SecondaryAirVolFlow // HX secondary air flow rate [m3/s]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine was designed for to autosize the HeatExchanger:AirToAir:SensibleAndLatent using
		// information from the ZoneHVAC:EnergyRecoveryVentilator object.
		// This is an illustration of setting data from an outside source.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichHX; // index to generic HX

		// Obtains and Allocates heat exchanger related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetHeatRecoveryInput();
			GetInputFlag = false;
		}

		if ( HXNum == 0 ) {
			WhichHX = FindItemInList( HXName, ExchCond );
		} else {
			WhichHX = HXNum;
		}

		if ( WhichHX <= 0 || WhichHX > NumHeatExchangers ) {
			ShowSevereError( "SetHeatExchangerData: Could not find heat exchanger = \"" + HXName + "\"" );
			ErrorsFound = true;
			return;
		}

		if ( present( SupplyAirVolFlow ) ) {
			ExchCond( WhichHX ).NomSupAirVolFlow = SupplyAirVolFlow;
		}

		if ( present( SecondaryAirVolFlow ) ) {
			ExchCond( WhichHX ).NomSecAirVolFlow = SecondaryAirVolFlow;
		}

	}

} // HeatRecovery

} // EnergyPlus
