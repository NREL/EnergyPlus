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
#include <Fans.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirflowNetwork.hh>
#include <DataAirLoop.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <FaultsManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace Fans {
	// Module containing the fan simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   April 1998
	//       MODIFIED       Shirey, May 2001
	//                      Griffith, May 2009, EMS changes
	//                      Craig Wray 22Aug2010 Added Fan Component Model
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the Fan System Component

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using DataHVACGlobals::TurnFansOn; // cpw22Aug2010 Added FanType_ComponentModel
	using DataHVACGlobals::TurnFansOff;
	using DataHVACGlobals::Main;
	using DataHVACGlobals::Cooling;
	using DataHVACGlobals::Heating;
	using DataHVACGlobals::Other;
	using DataHVACGlobals::OnOffFanPartLoadFraction;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::UnbalExhMassFlow;
	using DataHVACGlobals::BalancedExhMassFlow;
	using DataHVACGlobals::NightVentOn;
	using DataHVACGlobals::cFanTypes;
	using DataHVACGlobals::FanType_SimpleConstVolume;
	using DataHVACGlobals::FanType_SimpleVAV;
	using DataHVACGlobals::FanType_SimpleOnOff;
	using DataHVACGlobals::FanType_ZoneExhaust;
	using DataHVACGlobals::FanType_ComponentModel;
	using DataHVACGlobals::MinFrac;
	using DataHVACGlobals::FixedMin;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::emsCallFromComponentGetInput;
	using DataGlobals::DisplayExtraWarnings;
	using EMSManager::ManageEMS;
	using DataEnvironment::StdRhoAir;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyTdbFnHW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using InputProcessor::SameString;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	//MODULE PARAMETER DEFINITIONS
	// parameters describing fan types are contained in DataHVACGlobals (see USE statement above)

	int const ExhaustFanCoupledToAvailManagers( 150 );
	int const ExhaustFanDecoupledFromAvailManagers( 151 );
	static std::string const BlankString;
	//na

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumFans( 0 ); // The Number of Fans found in the Input
	int NumNightVentPerf( 0 ); // number of FAN:NIGHT VENT PERFORMANCE objects found in the input
	bool GetFanInputFlag( true ); // Flag set to make sure you get input once
	bool LocalTurnFansOn( false ); // If True, overrides fan schedule and cycles ZoneHVAC component fans on
	bool LocalTurnFansOff( false ); // If True, overrides fan schedule and LocalTurnFansOn and cycles ZoneHVAC component fans off

	namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this module should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool MyOneTimeFlag( true ); // used for allocation in Init
		bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items

		Array1D_bool MySizeFlag;
		Array1D_bool MyEnvrnFlag;
		Array1D_bool CheckEquipName;
	}

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Utility routines for module

	// Object Data
	Array1D< FanEquipConditions > Fan;
	Array1D< NightVentPerfData > NightVentPerf;
	Array1D< FanNumericFieldData > FanNumericFields;

	// MODULE SUBROUTINES:
	//*************************************************************************

	void
	SimulateFanComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex,
		Optional< Real64 const > SpeedRatio,
		Optional_bool_const ZoneCompTurnFansOn, // Turn fans ON signal from ZoneHVAC component
		Optional_bool_const ZoneCompTurnFansOff, // Turn Fans OFF signal from ZoneHVAC component
		Optional< Real64 const > PressureRise // Pressure difference to use for DeltaPress
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       Chandan Sharma, March 2011 - FSEC: Added logic for ZoneHVAC sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Fan component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanNum; // current fan number

		// FLOW:

		// Obtains and Allocates fan related parameters from input file
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		if ( CompIndex == 0 ) {
			FanNum = FindItemInList( CompName, Fan, &FanEquipConditions::FanName );
			if ( FanNum == 0 ) {
				ShowFatalError( "SimulateFanComponents: Fan not found=" + CompName );
			}
			CompIndex = FanNum;
		} else {
			FanNum = CompIndex;
			if ( FanNum > NumFans || FanNum < 1 ) {
				ShowFatalError( "SimulateFanComponents: Invalid CompIndex passed=" + TrimSigDigits( FanNum ) + ", Number of Fans=" + TrimSigDigits( NumFans ) + ", Fan name=" + CompName );
			}
			if ( CheckEquipName( FanNum ) ) {
				if ( ! CompName.empty() && CompName != Fan( FanNum ).FanName ) {
					ShowFatalError( "SimulateFanComponents: Invalid CompIndex passed=" + TrimSigDigits( FanNum ) + ", Fan name=" + CompName + ", stored Fan Name for that index=" + Fan( FanNum ).FanName );
				}
				CheckEquipName( FanNum ) = false;
			}
		}

		LocalTurnFansOn = false;
		LocalTurnFansOff = false;
		// With the correct FanNum Initialize
		InitFan( FanNum, FirstHVACIteration ); // Initialize all fan related parameters

		if ( present( ZoneCompTurnFansOn ) && present( ZoneCompTurnFansOff ) ) {
			// Set module-level logic flags equal to ZoneCompTurnFansOn and ZoneCompTurnFansOff values passed into this routine
			// for ZoneHVAC components with system availability managers defined.
			// The module-level flags get used in the other subroutines (e.g., SimSimpleFan,SimVariableVolumeFan and SimOnOffFan)
			LocalTurnFansOn = ZoneCompTurnFansOn;
			LocalTurnFansOff = ZoneCompTurnFansOff;
		} else {
			// Set module-level logic flags equal to the global LocalTurnFansOn and LocalTurnFansOff variables for all other cases.
			LocalTurnFansOn = TurnFansOn;
			LocalTurnFansOff = TurnFansOff;
		}

		// Calculate the Correct Fan Model with the current FanNum
		if ( Fan( FanNum ).FanType_Num == FanType_SimpleConstVolume ) {
			SimSimpleFan( FanNum );
		} else if ( Fan( FanNum ).FanType_Num == FanType_SimpleVAV ) {
			if ( present( PressureRise ) ) {
				SimVariableVolumeFan( FanNum, PressureRise );
			} else {
				SimVariableVolumeFan( FanNum );
			}
		} else if ( Fan( FanNum ).FanType_Num == FanType_SimpleOnOff ) {
			SimOnOffFan( FanNum, SpeedRatio );
		} else if ( Fan( FanNum ).FanType_Num == FanType_ZoneExhaust ) {
			SimZoneExhaustFan( FanNum );
			// cpw22Aug2010 Add call for Component Model fan
		} else if ( Fan( FanNum ).FanType_Num == FanType_ComponentModel ) {
			SimComponentModelFan( FanNum );
		}

		// Update the current fan to the outlet nodes
		UpdateFan( FanNum );

		// Report the current fan
		ReportFan( FanNum );

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetFanInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   April 1998
		//       MODIFIED       Shirey, May 2001
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for fans and stores it in fan data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using CurveManager::GetCurveIndex;
		using BranchNodeConnections::TestCompSet;

		//    USE DataIPShortCuts
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::ScheduleAlwaysOn;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanNum; // The fan that you are currently loading input into
		int NumSimpFan; // The number of Simple Const Vol Fans
		int NumVarVolFan; // The number of Simple Variable Vol Fans
		int NumOnOff; // The number of Simple on-off Fans
		int NumZoneExhFan;
		int SimpFanNum;
		int OnOffFanNum;
		int VarVolFanNum;
		int ExhFanNum;
		int NVPerfNum;
		bool NVPerfFanFound;
		int NumCompModelFan; // cpw22Aug2010 The number of Component Model Fans
		int CompModelFanNum; // cpw22Aug2010 Component Model Fan index
		int NumAlphas;
		int NumNums;
		int checkNum;
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static std::string const RoutineName( "GetFanInput: " ); // include trailing blank space
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		int NumParams;
		int MaxAlphas;
		int MaxNumbers;

		// Flow
		MaxAlphas = 0;
		MaxNumbers = 0;
		NumSimpFan = GetNumObjectsFound( "Fan:ConstantVolume" );
		if ( NumSimpFan > 0 ) {
			GetObjectDefMaxArgs( "Fan:ConstantVolume", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}
		NumVarVolFan = GetNumObjectsFound( "Fan:VariableVolume" );
		if ( NumVarVolFan > 0 ) {
			GetObjectDefMaxArgs( "Fan:VariableVolume", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}
		NumOnOff = GetNumObjectsFound( "Fan:OnOff" );
		if ( NumOnOff > 0 ) {
			GetObjectDefMaxArgs( "Fan:OnOff", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}
		NumZoneExhFan = GetNumObjectsFound( "Fan:ZoneExhaust" );
		if ( NumZoneExhFan > 0 ) {
			GetObjectDefMaxArgs( "Fan:ZoneExhaust", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}
		NumNightVentPerf = GetNumObjectsFound( "FanPerformance:NightVentilation" );
		if ( NumNightVentPerf > 0 ) {
			GetObjectDefMaxArgs( "FanPerformance:NightVentilation", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}

		// cpw22Aug2010 Added get max alphas and numbers for ComponentModel fan
		NumCompModelFan = GetNumObjectsFound( "Fan:ComponentModel" );
		if ( NumCompModelFan > 0 ) {
			GetObjectDefMaxArgs( "Fan:ComponentModel", NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNumbers = max( MaxNumbers, NumNums );
		}

		cAlphaArgs.allocate( MaxAlphas );
		cAlphaFieldNames.allocate( MaxAlphas );
		lAlphaFieldBlanks.dimension( MaxAlphas, false );
		cNumericFieldNames.allocate( MaxNumbers );
		lNumericFieldBlanks.dimension( MaxNumbers, false );
		rNumericArgs.dimension( MaxNumbers, 0.0 );

		NumFans = NumSimpFan + NumVarVolFan + NumZoneExhFan + NumOnOff + NumCompModelFan; // cpw1Mar2010 Add NumCompModelFan
		if ( NumFans > 0 ) {
			Fan.allocate( NumFans );
			FanNumericFields.allocate( NumFans );
		}
		CheckEquipName.dimension( NumFans, true );

		for ( SimpFanNum = 1; SimpFanNum <= NumSimpFan; ++SimpFanNum ) {
			FanNum = SimpFanNum;
			cCurrentModuleObject = "Fan:ConstantVolume";
			GetObjectItem( cCurrentModuleObject, SimpFanNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			FanNumericFields( FanNum ).FieldNames.allocate( MaxNumbers );
			FanNumericFields( FanNum ).FieldNames = "";
			FanNumericFields( FanNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Fan, &FanEquipConditions::FanName, FanNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			Fan( FanNum ).FanName = cAlphaArgs( 1 );
			Fan( FanNum ).FanType = cCurrentModuleObject;
			Fan( FanNum ).AvailSchedName = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				Fan( FanNum ).AvailSchedPtrNum = ScheduleAlwaysOn;
			} else {
				Fan( FanNum ).AvailSchedPtrNum = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( Fan( FanNum ).AvailSchedPtrNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
			Fan( FanNum ).FanType_Num = FanType_SimpleConstVolume;

			Fan( FanNum ).FanEff = rNumericArgs( 1 );
			Fan( FanNum ).DeltaPress = rNumericArgs( 2 );
			Fan( FanNum ).MaxAirFlowRate = rNumericArgs( 3 );
			if ( Fan( FanNum ).MaxAirFlowRate == 0.0 ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + Fan( FanNum ).FanName + "\" has specified 0.0 max air flow rate. It will not be used in the simulation." );
			}
			Fan( FanNum ).MaxAirFlowRateIsAutosizable = true;
			Fan( FanNum ).MotEff = rNumericArgs( 4 );
			Fan( FanNum ).MotInAirFrac = rNumericArgs( 5 );
			Fan( FanNum ).MinAirFlowRate = 0.0;

			Fan( FanNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Fan( FanNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ( NumAlphas > 4 ) {
				Fan( FanNum ).EndUseSubcategoryName = cAlphaArgs( 5 );
			} else {
				Fan( FanNum ).EndUseSubcategoryName = "General";
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Air Nodes" );

		} // end Number of Simple FAN Loop

		for ( VarVolFanNum = 1; VarVolFanNum <= NumVarVolFan; ++VarVolFanNum ) {
			FanNum = NumSimpFan + VarVolFanNum;
			cCurrentModuleObject = "Fan:VariableVolume";
			GetObjectItem( cCurrentModuleObject, VarVolFanNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			FanNumericFields( FanNum ).FieldNames.allocate( MaxNumbers );
			FanNumericFields( FanNum ).FieldNames = "";
			FanNumericFields( FanNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Fan, &FanEquipConditions::FanName, FanNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			Fan( FanNum ).FanName = cAlphaArgs( 1 );
			Fan( FanNum ).FanType = cCurrentModuleObject;
			Fan( FanNum ).AvailSchedName = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				Fan( FanNum ).AvailSchedPtrNum = ScheduleAlwaysOn;
			} else {
				Fan( FanNum ).AvailSchedPtrNum = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( Fan( FanNum ).AvailSchedPtrNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
			Fan( FanNum ).FanType_Num = FanType_SimpleVAV;

			Fan( FanNum ).FanEff = rNumericArgs( 1 );
			Fan( FanNum ).DeltaPress = rNumericArgs( 2 );
			Fan( FanNum ).MaxAirFlowRate = rNumericArgs( 3 );
			if ( Fan( FanNum ).MaxAirFlowRate == 0.0 ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + Fan( FanNum ).FanName + "\" has specified 0.0 max air flow rate. It will not be used in the simulation." );
			}
			Fan( FanNum ).MaxAirFlowRateIsAutosizable = true;
			if ( SameString( cAlphaArgs( 3 ), "Fraction" ) ) {
				Fan( FanNum ).FanMinAirFracMethod = MinFrac;
			} else if ( SameString( cAlphaArgs( 3 ), "FixedFlowRate" ) ) {
				Fan( FanNum ).FanMinAirFracMethod = FixedMin;
			} else {
				ShowSevereError( cAlphaFieldNames( 3 ) + " should be either Fraction or FixedFlowRate." );
				ShowContinueError( "Occurs in " + Fan( FanNum ).FanName + " object." );
				ErrorsFound = true;
			}
			//        Fan(FanNum)%MinAirFlowRate= rNumericArgs(4)
			Fan( FanNum ).FanMinFrac = rNumericArgs( 4 );
			Fan( FanNum ).FanFixedMin = rNumericArgs( 5 );
			Fan( FanNum ).MotEff = rNumericArgs( 6 );
			Fan( FanNum ).MotInAirFrac = rNumericArgs( 7 );
			Fan( FanNum ).FanCoeff( 1 ) = rNumericArgs( 8 );
			Fan( FanNum ).FanCoeff( 2 ) = rNumericArgs( 9 );
			Fan( FanNum ).FanCoeff( 3 ) = rNumericArgs( 10 );
			Fan( FanNum ).FanCoeff( 4 ) = rNumericArgs( 11 );
			Fan( FanNum ).FanCoeff( 5 ) = rNumericArgs( 12 );
			if ( Fan( FanNum ).FanCoeff( 1 ) == 0.0 && Fan( FanNum ).FanCoeff( 2 ) == 0.0 && Fan( FanNum ).FanCoeff( 3 ) == 0.0 && Fan( FanNum ).FanCoeff( 4 ) == 0.0 && Fan( FanNum ).FanCoeff( 5 ) == 0.0 ) {
				ShowWarningError( "Fan Coefficients are all zero.  No Fan power will be reported." );
				ShowContinueError( "For " + cCurrentModuleObject + ", Fan=" + cAlphaArgs( 1 ) );
			}
			Fan( FanNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Fan( FanNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ( NumAlphas > 5 ) {
				Fan( FanNum ).EndUseSubcategoryName = cAlphaArgs( 6 );
			} else {
				Fan( FanNum ).EndUseSubcategoryName = "General";
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Air Nodes" );

		} // end Number of Variable Volume FAN Loop

		for ( ExhFanNum = 1; ExhFanNum <= NumZoneExhFan; ++ExhFanNum ) {
			FanNum = NumSimpFan + NumVarVolFan + ExhFanNum;
			cCurrentModuleObject = "Fan:ZoneExhaust";
			GetObjectItem( cCurrentModuleObject, ExhFanNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			FanNumericFields( FanNum ).FieldNames.allocate( MaxNumbers );
			FanNumericFields( FanNum ).FieldNames = "";
			FanNumericFields( FanNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Fan, &FanEquipConditions::FanName, FanNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			Fan( FanNum ).FanName = cAlphaArgs( 1 );
			Fan( FanNum ).FanType = cCurrentModuleObject;
			Fan( FanNum ).AvailSchedName = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				Fan( FanNum ).AvailSchedPtrNum = ScheduleAlwaysOn;
			} else {
				Fan( FanNum ).AvailSchedPtrNum = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( Fan( FanNum ).AvailSchedPtrNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else {
					if ( HasFractionalScheduleValue( Fan( FanNum ).AvailSchedPtrNum ) ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + Fan( FanNum ).FanName + "\" has fractional values in Schedule=" + cAlphaArgs( 2 ) + ". Only 0.0 in the schedule value turns the fan off." );
					}
				}
			}
			Fan( FanNum ).FanType_Num = FanType_ZoneExhaust;

			Fan( FanNum ).FanEff = rNumericArgs( 1 );
			Fan( FanNum ).DeltaPress = rNumericArgs( 2 );
			Fan( FanNum ).MaxAirFlowRate = rNumericArgs( 3 );
			Fan( FanNum ).MaxAirFlowRateIsAutosizable = false;
			Fan( FanNum ).MotEff = 1.0;
			Fan( FanNum ).MotInAirFrac = 1.0;
			Fan( FanNum ).MinAirFlowRate = 0.0;
			Fan( FanNum ).RhoAirStdInit = StdRhoAir;
			Fan( FanNum ).MaxAirMassFlowRate = Fan( FanNum ).MaxAirFlowRate * Fan( FanNum ).RhoAirStdInit;

			if ( Fan( FanNum ).MaxAirFlowRate == 0.0 ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + Fan( FanNum ).FanName + "\" has specified 0.0 max air flow rate. It will not be used in the simulation." );
			}

			Fan( FanNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Fan( FanNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ( NumAlphas > 4 && ! lAlphaFieldBlanks( 5 ) ) {
				Fan( FanNum ).EndUseSubcategoryName = cAlphaArgs( 5 );
			} else {
				Fan( FanNum ).EndUseSubcategoryName = "General";
			}

			if ( NumAlphas > 5 && ! lAlphaFieldBlanks( 6 ) ) {
				Fan( FanNum ).FlowFractSchedNum = GetScheduleIndex( cAlphaArgs( 6 ) );
				if ( Fan( FanNum ).FlowFractSchedNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 6 ) + " entered =" + cAlphaArgs( 6 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( Fan( FanNum ).FlowFractSchedNum > 0 ) {
					if ( ! CheckScheduleValueMinMax( Fan( FanNum ).FlowFractSchedNum, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 6 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Error found in " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
						ShowContinueError( "Schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}
			} else {
				Fan( FanNum ).FlowFractSchedNum = ScheduleAlwaysOn;
			}

			if ( NumAlphas > 6 && ! lAlphaFieldBlanks( 7 ) ) {
				{ auto const SELECT_CASE_var( cAlphaArgs( 7 ) );
				if ( SELECT_CASE_var == "COUPLED" ) {
					Fan( FanNum ).AvailManagerMode = ExhaustFanCoupledToAvailManagers;
				} else if ( SELECT_CASE_var == "DECOUPLED" ) {
					Fan( FanNum ).AvailManagerMode = ExhaustFanDecoupledFromAvailManagers;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 7 ) + " entered =" + cAlphaArgs( 7 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}}
			} else {
				Fan( FanNum ).AvailManagerMode = ExhaustFanCoupledToAvailManagers;
			}

			if ( NumAlphas > 7 && ! lAlphaFieldBlanks( 8 ) ) {
				Fan( FanNum ).MinTempLimitSchedNum = GetScheduleIndex( cAlphaArgs( 8 ) );
				if ( Fan( FanNum ).MinTempLimitSchedNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 8 ) + " entered =" + cAlphaArgs( 8 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			} else {
				Fan( FanNum ).MinTempLimitSchedNum = 0;
			}

			if ( NumAlphas > 8 && ! lAlphaFieldBlanks( 9 ) ) {
				Fan( FanNum ).BalancedFractSchedNum = GetScheduleIndex( cAlphaArgs( 9 ) );
				if ( Fan( FanNum ).BalancedFractSchedNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 9 ) + " entered =" + cAlphaArgs( 9 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( Fan( FanNum ).BalancedFractSchedNum > 0 ) {
					if ( ! CheckScheduleValueMinMax( Fan( FanNum ).BalancedFractSchedNum, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 9 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Error found in " + cAlphaFieldNames( 9 ) + " = " + cAlphaArgs( 9 ) );
						ShowContinueError( "Schedule values must be (>=0., <=1.)" );
						ErrorsFound = true;
					}
				}
			} else {
				Fan( FanNum ).BalancedFractSchedNum = 0;
			}

			// Component sets not setup yet for zone equipment
			// CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Air Nodes')

		} // end of Zone Exhaust Fan loop

		for ( OnOffFanNum = 1; OnOffFanNum <= NumOnOff; ++OnOffFanNum ) {
			FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum;
			cCurrentModuleObject = "Fan:OnOff";
			GetObjectItem( cCurrentModuleObject, OnOffFanNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			FanNumericFields( FanNum ).FieldNames.allocate( MaxNumbers );
			FanNumericFields( FanNum ).FieldNames = "";
			FanNumericFields( FanNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Fan, &FanEquipConditions::FanName, FanNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			Fan( FanNum ).FanName = cAlphaArgs( 1 );
			Fan( FanNum ).FanType = cCurrentModuleObject;
			Fan( FanNum ).AvailSchedName = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				Fan( FanNum ).AvailSchedPtrNum = ScheduleAlwaysOn;
			} else {
				Fan( FanNum ).AvailSchedPtrNum = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( Fan( FanNum ).AvailSchedPtrNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
			Fan( FanNum ).FanType_Num = FanType_SimpleOnOff;

			Fan( FanNum ).FanEff = rNumericArgs( 1 );
			Fan( FanNum ).DeltaPress = rNumericArgs( 2 );
			Fan( FanNum ).MaxAirFlowRate = rNumericArgs( 3 );
			if ( Fan( FanNum ).MaxAirFlowRate == 0.0 ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + Fan( FanNum ).FanName + "\" has specified 0.0 max air flow rate. It will not be used in the simulation." );
			}
			Fan( FanNum ).MaxAirFlowRateIsAutosizable = true;
			//       the following two structure variables are set here, as well as in InitFan, for the Heat Pump:Water Heater object
			//       (Standard Rating procedure may be called before BeginEnvirFlag is set to TRUE, if so MaxAirMassFlowRate = 0)
			Fan( FanNum ).RhoAirStdInit = StdRhoAir;
			Fan( FanNum ).MaxAirMassFlowRate = Fan( FanNum ).MaxAirFlowRate * Fan( FanNum ).RhoAirStdInit;

			Fan( FanNum ).MotEff = rNumericArgs( 4 );
			Fan( FanNum ).MotInAirFrac = rNumericArgs( 5 );
			Fan( FanNum ).MinAirFlowRate = 0.0;

			Fan( FanNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Fan( FanNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ( NumAlphas > 4 && ! lAlphaFieldBlanks( 5 ) ) {
				Fan( FanNum ).FanPowerRatAtSpeedRatCurveIndex = GetCurveIndex( cAlphaArgs( 5 ) );
			}

			if ( NumAlphas > 5 && ! lAlphaFieldBlanks( 6 ) ) {
				Fan( FanNum ).FanEffRatioCurveIndex = GetCurveIndex( cAlphaArgs( 6 ) );
			}

			if ( NumAlphas > 6 && ! lAlphaFieldBlanks( 7 ) ) {
				Fan( FanNum ).EndUseSubcategoryName = cAlphaArgs( 7 );
			} else {
				Fan( FanNum ).EndUseSubcategoryName = "General";
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Air Nodes" );

		} // end Number of Simple  ON-OFF FAN Loop

		cCurrentModuleObject = "FanPerformance:NightVentilation";
		NumNightVentPerf = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumNightVentPerf > 0 ) {
			NightVentPerf.allocate( NumNightVentPerf );
			for ( auto & e : NightVentPerf ) {
				e.FanName.clear();
				e.FanEff = 0.0;
				e.DeltaPress = 0.0;
				e.MaxAirFlowRate = 0.0;
				e.MotEff = 0.0;
				e.MotInAirFrac = 0.0;
				e.MaxAirMassFlowRate = 0.0;
			}
		}
		// input the night ventilation performance objects
		for ( NVPerfNum = 1; NVPerfNum <= NumNightVentPerf; ++NVPerfNum ) {
			GetObjectItem( cCurrentModuleObject, NVPerfNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), NightVentPerf, &NightVentPerfData::FanName, NVPerfNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			NightVentPerf( NVPerfNum ).FanName = cAlphaArgs( 1 );
			NightVentPerf( NVPerfNum ).FanEff = rNumericArgs( 1 );
			NightVentPerf( NVPerfNum ).DeltaPress = rNumericArgs( 2 );
			NightVentPerf( NVPerfNum ).MaxAirFlowRate = rNumericArgs( 3 );
			NightVentPerf( NVPerfNum ).MotEff = rNumericArgs( 4 );
			NightVentPerf( NVPerfNum ).MotInAirFrac = rNumericArgs( 5 );
			// find the corresponding fan
			NVPerfFanFound = false;
			for ( FanNum = 1; FanNum <= NumFans; ++FanNum ) {
				if ( NightVentPerf( NVPerfNum ).FanName == Fan( FanNum ).FanName ) {
					NVPerfFanFound = true;
					Fan( FanNum ).NVPerfNum = NVPerfNum;
					break;
				}
			}
			if ( ! NVPerfFanFound ) {
				ShowSevereError( cCurrentModuleObject + ", fan name not found=" + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

		}

		//cpw22Aug2010 Added get input for Component Fan Model
		for ( CompModelFanNum = 1; CompModelFanNum <= NumCompModelFan; ++CompModelFanNum ) {
			FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + NumOnOff + CompModelFanNum;

			cCurrentModuleObject = "Fan:ComponentModel";
			GetObjectItem( cCurrentModuleObject, CompModelFanNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			FanNumericFields( FanNum ).FieldNames.allocate( MaxNumbers );
			FanNumericFields( FanNum ).FieldNames = "";
			FanNumericFields( FanNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Fan, &FanEquipConditions::FanName, FanNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			Fan( FanNum ).FanName = cAlphaArgs( 1 ); // Fan name
			Fan( FanNum ).FanType = cCurrentModuleObject;

			Fan( FanNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent ); // Air inlet node name
			Fan( FanNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent ); // Air outlet node name

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Air Nodes" );

			Fan( FanNum ).AvailSchedName = cAlphaArgs( 4 ); // Availability schedule name
			if ( lAlphaFieldBlanks( 4 ) ) {
				Fan( FanNum ).AvailSchedPtrNum = 0;
			} else {
				Fan( FanNum ).AvailSchedPtrNum = GetScheduleIndex( cAlphaArgs( 4 ) );
				if ( Fan( FanNum ).AvailSchedPtrNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 4 ) + " entered =" + cAlphaArgs( 4 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			Fan( FanNum ).FanType_Num = FanType_ComponentModel;

			Fan( FanNum ).MaxAirFlowRate = rNumericArgs( 1 );
			if ( Fan( FanNum ).MaxAirFlowRate == 0.0 ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + Fan( FanNum ).FanName + "\" has specified 0.0 max air flow rate. It will not be used in the simulation." );
			}
			Fan( FanNum ).MaxAirFlowRateIsAutosizable = true;
			Fan( FanNum ).MinAirFlowRate = rNumericArgs( 2 );

			Fan( FanNum ).FanSizingFactor = rNumericArgs( 3 ); // Fan max airflow sizing factor [-] cpw31Aug2010
			Fan( FanNum ).FanWheelDia = rNumericArgs( 4 ); // Fan wheel outer diameter [m]
			Fan( FanNum ).FanOutletArea = rNumericArgs( 5 ); // Fan outlet area [m2]
			Fan( FanNum ).FanMaxEff = rNumericArgs( 6 ); // Fan maximum static efficiency [-]
			Fan( FanNum ).EuMaxEff = rNumericArgs( 7 ); // Euler number at Fan maximum static efficiency [-]
			Fan( FanNum ).FanMaxDimFlow = rNumericArgs( 8 ); // Fan maximum dimensionless airflow [-]
			Fan( FanNum ).PulleyDiaRatio = rNumericArgs( 9 ); // Motor/fan pulley diameter ratio [-]
			Fan( FanNum ).BeltMaxTorque = rNumericArgs( 10 ); // Belt maximum torque [N-m, autosizable]
			Fan( FanNum ).BeltSizingFactor = rNumericArgs( 11 ); // Belt sizing factor [-]
			Fan( FanNum ).BeltTorqueTrans = rNumericArgs( 12 ); // Belt fractional torque transition Region 1-2 [-]
			Fan( FanNum ).MotorMaxSpd = rNumericArgs( 13 ); // Motor maximum speed [rpm]
			Fan( FanNum ).MotorMaxOutPwr = rNumericArgs( 14 ); // Motor maximum output power [W, autosizable]
			Fan( FanNum ).MotorSizingFactor = rNumericArgs( 15 ); // Motor sizing factor [-]
			Fan( FanNum ).MotInAirFrac = rNumericArgs( 16 ); // Fraction of fan and motor losses to airstream [-]
			Fan( FanNum ).VFDEffType = cAlphaArgs( 5 ); // VFD efficiency type [Speed or Power]
			Fan( FanNum ).VFDMaxOutPwr = rNumericArgs( 17 ); // VFD maximum output power [W, autosizable]
			Fan( FanNum ).VFDSizingFactor = rNumericArgs( 18 ); // VFD sizing factor [-] cpw31Aug2010
			Fan( FanNum ).PressRiseCurveIndex = GetCurveIndex( cAlphaArgs( 6 ) ); // Fan pressure rise curve
			Fan( FanNum ).PressResetCurveIndex = GetCurveIndex( cAlphaArgs( 7 ) ); // Duct static pressure reset curve
			Fan( FanNum ).PLFanEffNormCurveIndex = GetCurveIndex( cAlphaArgs( 8 ) ); // Fan part-load eff (normal) curve
			Fan( FanNum ).PLFanEffStallCurveIndex = GetCurveIndex( cAlphaArgs( 9 ) ); // Fan part-load eff (stall) curve
			Fan( FanNum ).DimFlowNormCurveIndex = GetCurveIndex( cAlphaArgs( 10 ) ); // Fan dim airflow (normal) curve
			Fan( FanNum ).DimFlowStallCurveIndex = GetCurveIndex( cAlphaArgs( 11 ) ); // Fan dim airflow (stall) curve
			Fan( FanNum ).BeltMaxEffCurveIndex = GetCurveIndex( cAlphaArgs( 12 ) ); // Belt max eff curve
			Fan( FanNum ).PLBeltEffReg1CurveIndex = GetCurveIndex( cAlphaArgs( 13 ) ); // Belt part-load eff Region 1 curve
			Fan( FanNum ).PLBeltEffReg2CurveIndex = GetCurveIndex( cAlphaArgs( 14 ) ); // Belt part-load eff Region 2 curve
			Fan( FanNum ).PLBeltEffReg3CurveIndex = GetCurveIndex( cAlphaArgs( 15 ) ); // Belt part-load eff Region 3 curve
			Fan( FanNum ).MotorMaxEffCurveIndex = GetCurveIndex( cAlphaArgs( 16 ) ); // Motor max eff curve
			Fan( FanNum ).PLMotorEffCurveIndex = GetCurveIndex( cAlphaArgs( 17 ) ); // Motor part-load eff curve
			Fan( FanNum ).VFDEffCurveIndex = GetCurveIndex( cAlphaArgs( 18 ) ); // VFD eff curve

			if ( NumAlphas > 18 ) {
				Fan( FanNum ).EndUseSubcategoryName = cAlphaArgs( 19 );
			} else {
				Fan( FanNum ).EndUseSubcategoryName = "General";
			}

		} // end Number of Component Model FAN Loop

		cAlphaArgs.deallocate();
		cAlphaFieldNames.deallocate();
		lAlphaFieldBlanks.deallocate();
		cNumericFieldNames.deallocate();
		lNumericFieldBlanks.deallocate();
		rNumericArgs.deallocate();

		// Check Fans
		for ( FanNum = 1; FanNum <= NumFans; ++FanNum ) {
			for ( checkNum = FanNum + 1; checkNum <= NumFans; ++checkNum ) {
				if ( Fan( FanNum ).InletNodeNum == Fan( checkNum ).InletNodeNum ) {
					ErrorsFound = true;
					ShowSevereError( "GetFanInput, duplicate fan inlet node names, must be unique for fans." );
					ShowContinueError( "Fan=" + Fan( FanNum ).FanType + ':' + Fan( FanNum ).FanName + " and Fan=" + Fan( checkNum ).FanType + ':' + Fan( checkNum ).FanName + '.' );
					ShowContinueError( "Inlet Node Name=\"" + NodeID( Fan( FanNum ).InletNodeNum ) + "\"." );
				}
				if ( Fan( FanNum ).OutletNodeNum == Fan( checkNum ).OutletNodeNum ) {
					ErrorsFound = true;
					ShowSevereError( "GetFanInput, duplicate fan outlet node names, must be unique for fans." );
					ShowContinueError( "Fan=" + Fan( FanNum ).FanType + ':' + Fan( FanNum ).FanName + " and Fan=" + Fan( checkNum ).FanType + ':' + Fan( checkNum ).FanName + '.' );
					ShowContinueError( "Outlet Node Name=\"" + NodeID( Fan( FanNum ).OutletNodeNum ) + "\"." );
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

		for ( FanNum = 1; FanNum <= NumFans; ++FanNum ) {
			// Setup Report variables for the Fans  CurrentModuleObject='Fans'
			SetupOutputVariable( "Fan Electric Power [W]", Fan( FanNum ).FanPower, "System", "Average", Fan( FanNum ).FanName );
			SetupOutputVariable( "Fan Rise in Air Temperature [deltaC]", Fan( FanNum ).DeltaTemp, "System", "Average", Fan( FanNum ).FanName );
			SetupOutputVariable( "Fan Electric Energy [J]", Fan( FanNum ).FanEnergy, "System", "Sum", Fan( FanNum ).FanName, _, "Electric", "Fans", Fan( FanNum ).EndUseSubcategoryName, "System" );

			if ( ( Fan( FanNum ).FanType_Num == FanType_ZoneExhaust ) && ( Fan( FanNum ).BalancedFractSchedNum > 0 ) ) {
				SetupOutputVariable( "Fan Unbalanced Air Mass Flow Rate [kg/s]", Fan( FanNum ).UnbalancedOutletMassFlowRate, "System", "Average", Fan( FanNum ).FanName );
				SetupOutputVariable( "Fan Balanced Air Mass Flow Rate [kg/s]", Fan( FanNum ).BalancedOutletMassFlowRate, "System", "Average", Fan( FanNum ).FanName );
			}

			if ( AnyEnergyManagementSystemInModel ) {

				SetupEMSInternalVariable( "Fan Maximum Mass Flow Rate", Fan( FanNum ).FanName, "[kg/s]", Fan( FanNum ).MaxAirMassFlowRate );
				SetupEMSActuator( "Fan", Fan( FanNum ).FanName, "Fan Air Mass Flow Rate", "[kg/s]", Fan( FanNum ).EMSMaxMassFlowOverrideOn, Fan( FanNum ).EMSAirMassFlowValue );
				SetupEMSInternalVariable( "Fan Nominal Pressure Rise", Fan( FanNum ).FanName, "[Pa]", Fan( FanNum ).DeltaPress );
				SetupEMSActuator( "Fan", Fan( FanNum ).FanName, "Fan Pressure Rise", "[Pa]", Fan( FanNum ).EMSFanPressureOverrideOn, Fan( FanNum ).EMSFanPressureValue );
				SetupEMSInternalVariable( "Fan Nominal Total Efficiency", Fan( FanNum ).FanName, "[fraction]", Fan( FanNum ).FanEff );
				SetupEMSActuator( "Fan", Fan( FanNum ).FanName, "Fan Total Efficiency", "[fraction]", Fan( FanNum ).EMSFanEffOverrideOn, Fan( FanNum ).EMSFanEffValue );

				SetupEMSActuator( "Fan", Fan( FanNum ).FanName, "Fan Autosized Air Flow Rate", "[m3/s]", Fan( FanNum ).MaxAirFlowRateEMSOverrideOn, Fan( FanNum ).MaxAirFlowRateEMSOverrideValue );
			}
		}

		for ( OnOffFanNum = 1; OnOffFanNum <= NumOnOff; ++OnOffFanNum ) {
			FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum;
			SetupOutputVariable( "Fan Runtime Fraction []", Fan( FanNum ).FanRuntimeFraction, "System", "Average", Fan( FanNum ).FanName );
		}

		ManageEMS( emsCallFromComponentGetInput );
		MySizeFlag.dimension( NumFans, true );

	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitFan(
		int const FanNum,
		bool const EP_UNUSED( FirstHVACIteration ) // unused1208
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Fan Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::CurSysNum;
		using DataAirLoop::AirLoopControlInfo;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		int OutNode;
		int Loop;

		// FLOW:

		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.dimension( NumFans, true );

			MyOneTimeFlag = false;

		}

		// need to check all fans to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumFans; ++Loop ) {
				if ( ! SameString( Fan( Loop ).FanType, "Fan:ZoneExhaust" ) ) continue;
				if ( CheckZoneEquipmentList( Fan( Loop ).FanType, Fan( Loop ).FanName ) ) continue;
				ShowSevereError( "InitFans: Fan=[" + Fan( Loop ).FanType + ',' + Fan( Loop ).FanName + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( FanNum ) ) {

			SizeFan( FanNum );
			// Set the loop cycling flag
			if ( Fan( FanNum ).FanType_Num == FanType_SimpleOnOff ) {
				if ( CurSysNum > 0 ) {
					AirLoopControlInfo( CurSysNum ).CyclingFan = true;
				}
			}

			MySizeFlag( FanNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( FanNum ) ) {

			//For all Fan inlet nodes convert the Volume flow to a mass flow
			//unused0909    InNode = Fan(FanNum)%InletNodeNum
			OutNode = Fan( FanNum ).OutletNodeNum;
			Fan( FanNum ).RhoAirStdInit = StdRhoAir;

			//Change the Volume Flow Rates to Mass Flow Rates

			Fan( FanNum ).MaxAirMassFlowRate = Fan( FanNum ).MaxAirFlowRate * Fan( FanNum ).RhoAirStdInit;
			if ( Fan( FanNum ).FanMinAirFracMethod == MinFrac ) {
				Fan( FanNum ).MinAirFlowRate = Fan( FanNum ).MaxAirFlowRate * Fan( FanNum ).FanMinFrac;
				Fan( FanNum ).MinAirMassFlowRate = Fan( FanNum ).MinAirFlowRate * Fan( FanNum ).RhoAirStdInit;
			} else if ( Fan( FanNum ).FanMinAirFracMethod == FixedMin ) {
				Fan( FanNum ).MinAirFlowRate = Fan( FanNum ).FanFixedMin;
				Fan( FanNum ).MinAirMassFlowRate = Fan( FanNum ).MinAirFlowRate * Fan( FanNum ).RhoAirStdInit;
			}
			if ( Fan( FanNum ).NVPerfNum > 0 ) {
				NightVentPerf( Fan( FanNum ).NVPerfNum ).MaxAirMassFlowRate = NightVentPerf( Fan( FanNum ).NVPerfNum ).MaxAirFlowRate * Fan( FanNum ).RhoAirStdInit;
			}

			//Init the Node Control variables
			Node( OutNode ).MassFlowRateMax = Fan( FanNum ).MaxAirMassFlowRate;
			Node( OutNode ).MassFlowRateMin = Fan( FanNum ).MinAirMassFlowRate;

			//Initialize all report variables to a known state at beginning of simulation
			Fan( FanNum ).FanPower = 0.0;
			Fan( FanNum ).DeltaTemp = 0.0;
			Fan( FanNum ).FanEnergy = 0.0;

			MyEnvrnFlag( FanNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( FanNum ) = true;
		}

		// Do the Begin Day initializations
		// none

		// Do the begin HVAC time step initializations
		// none

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.

		// Do a check and make sure that the max and min available(control) flow is
		// between the physical max and min for the Fan while operating.

		InletNode = Fan( FanNum ).InletNodeNum;
		OutletNode = Fan( FanNum ).OutletNodeNum;

		Fan( FanNum ).MassFlowRateMaxAvail = min( Node( OutletNode ).MassFlowRateMax, Node( InletNode ).MassFlowRateMaxAvail );
		Fan( FanNum ).MassFlowRateMinAvail = min( max( Node( OutletNode ).MassFlowRateMin, Node( InletNode ).MassFlowRateMinAvail ), Node( InletNode ).MassFlowRateMaxAvail );

		// Load the node data in this section for the component simulation
		//First need to make sure that the MassFlowRate is between the max and min avail.
		if ( Fan( FanNum ).FanType_Num != FanType_ZoneExhaust ) {
			Fan( FanNum ).InletAirMassFlowRate = min( Node( InletNode ).MassFlowRate, Fan( FanNum ).MassFlowRateMaxAvail );
			Fan( FanNum ).InletAirMassFlowRate = max( Fan( FanNum ).InletAirMassFlowRate, Fan( FanNum ).MassFlowRateMinAvail );
		} else { // zone exhaust fans
			Fan( FanNum ).MassFlowRateMaxAvail = Fan( FanNum ).MaxAirMassFlowRate;
			Fan( FanNum ).MassFlowRateMinAvail = 0.0;
			if ( Fan( FanNum ).FlowFractSchedNum > 0 ) { // modulate flow
				Fan( FanNum ).InletAirMassFlowRate = Fan( FanNum ).MassFlowRateMaxAvail * GetCurrentScheduleValue( Fan( FanNum ).FlowFractSchedNum );
				Fan( FanNum ).InletAirMassFlowRate = max( 0.0, Fan( FanNum ).InletAirMassFlowRate );
			} else { // always run at max
				Fan( FanNum ).InletAirMassFlowRate = Fan( FanNum ).MassFlowRateMaxAvail;
			}
			if ( Fan( FanNum ).EMSMaxMassFlowOverrideOn ) Fan( FanNum ).InletAirMassFlowRate = min( Fan( FanNum ).EMSAirMassFlowValue, Fan( FanNum ).MassFlowRateMaxAvail );
		}

		//Then set the other conditions
		Fan( FanNum ).InletAirTemp = Node( InletNode ).Temp;
		Fan( FanNum ).InletAirHumRat = Node( InletNode ).HumRat;
		Fan( FanNum ).InletAirEnthalpy = Node( InletNode ).Enthalpy;

	}

	void
	SizeFan( int const FanNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2001
		//       MODIFIED       Craig Wray August 2010 - added fan, belt, motor, and VFD component sizing
		//                      August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing fans for which flow rates have not been
		// specified in the input, or when fan component sizes have not been specified

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace OutputReportPredefined;
		using CurveManager::CurveValue;
		using CurveManager::GetCurveIndex;
		using General::RoundSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeFan: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NVPerfNum; // Index to night ventialation performance object
		std::string equipName; // Equipment name
		Real64 RatedPower; // Rated fan power [W]
		Real64 RhoAir; // Air density [kg/m3]
		Real64 FanVolFlow; // Fan volumetric airflow [m3/s]
		Real64 DuctStaticPress; // Duct static pressure setpoint [Pa]
		Real64 DeltaPressTot; // Total pressure rise across fan [N/m2 = Pa]
		Real64 FanOutletVelPress; // Fan outlet velocity pressure [Pa]
		Real64 EulerNum; // Fan Euler number [-]
		Real64 NormalizedEulerNum; // Normalized Fan Euler number [-]
		Real64 FanDimFlow; // Fan dimensionless airflow [-]
		Real64 FanSpdRadS; // Fan shaft rotational speed [rad/s]
		Real64 MotorSpeed; // Motor shaft rotational speed [rpm]
		Real64 XbeltMax; // Factor for belt max eff curve [ln hp]
		Real64 FanTrqRatio; // Ratio of fan torque to max fan torque [-]
		Real64 BeltPLEff; // Belt normalized (part-load) efficiency [-]
		Real64 XmotorMax; // Factor for motor max eff curve [ln hp]
		Real64 MotorOutPwrRatio; // Ratio of motor output power to max motor output power [-]
		Real64 MotorPLEff; // Motor normalized (part-load) efficiency [-]
		static Real64 VFDSpdRatio( 0.0 ); // Ratio of motor speed to motor max speed [-]
		static Real64 VFDOutPwrRatio( 0.0 ); // Ratio of VFD output power to max VFD output power [-]
		std::string CompName; // component name
		std::string	CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		bool bPRINT = true; // TRUE if sizing is reported to output (eio)
		Real64 TempFlow; // autosized flow rate of fan [m3/s]
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int NumFansSized = 0; // counter used to deallocate temporary string array after all fans have been sized

		if ( Fan( FanNum ).FanType_Num == FanType_ComponentModel ) {
			FieldNum = 1;
		} else {
			FieldNum = 3;
		}
		TempFlow = Fan( FanNum ).MaxAirFlowRate;
		DataAutosizable = Fan( FanNum ).MaxAirFlowRateIsAutosizable;
		SizingString = FanNumericFields( FanNum ).FieldNames( FieldNum ) + " [m3/s]";
		CompType = Fan( FanNum ).FanType;
		CompName = Fan( FanNum ).FanName;
		DataEMSOverrideON = Fan( FanNum ).MaxAirFlowRateEMSOverrideOn;
		DataEMSOverride = Fan( FanNum ).MaxAirFlowRateEMSOverrideValue;
		RequestSizing( CompType, CompName, SystemAirflowSizing, SizingString, TempFlow, bPRINT, RoutineName );
		Fan( FanNum ).MaxAirFlowRate = TempFlow;
		DataAutosizable = true;
		DataEMSOverrideON = false;
		DataEMSOverride = 0.0;

		//cpw31Aug2010 Add fan, belt, motor and VFD component autosizing and maximum efficiency calculations
		FanVolFlow = Fan( FanNum ).MaxAirFlowRate; //Maximum volumetric airflow through fan [m3/s at standard conditions]
		if ( Fan( FanNum ).FanType_Num == FanType_ComponentModel ) {
			// Get air density at standard conditions and get mass airflow through fan
			// From WeatherManager:
			//   StdBaroPress=(101.325d0*(1.0d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
			//   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
			// From PsychRoutines:
			//   w=MAX(dw,1.0d-5)
			//   rhoair = pb/(287.d0*(tdb+KelvinConv)*(1.0d0+1.6077687d0*w))
			RhoAir = StdRhoAir;

			// Adjust max fan volumetric airflow using fan sizing factor
			FanVolFlow *= Fan( FanNum ).FanSizingFactor; //[m3/s at standard conditions]

			// Calculate max fan static pressure rise using max fan volumetric flow, std air density, air-handling system characteristics,
			//   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
			DuctStaticPress = CurveValue( Fan( FanNum ).PressResetCurveIndex, FanVolFlow ); //Duct static pressure setpoint [Pa]
			DeltaPressTot = CurveValue( Fan( FanNum ).PressRiseCurveIndex, FanVolFlow, DuctStaticPress ); //Max fan total pressure rise [Pa]
			FanOutletVelPress = 0.5 * RhoAir * pow_2( FanVolFlow / Fan( FanNum ).FanOutletArea ); //Max fan outlet velocity pressure [Pa]
			//Outlet velocity pressure cannot exceed total pressure rise
			FanOutletVelPress = min( FanOutletVelPress, DeltaPressTot );
			Fan( FanNum ).DeltaPress = DeltaPressTot - FanOutletVelPress; //Max fan static pressure rise [Pa]

			// Calculate max fan air power using volumetric flow abd corresponding fan static pressure rise
			Fan( FanNum ).FanAirPower = FanVolFlow * Fan( FanNum ).DeltaPress; //[W]

			// Calculate fan wheel efficiency at max fan volumetric flow and corresponding fan static pressure rise,
			//   using fan characteristics and Wray dimensionless fan static efficiency model
			EulerNum = ( Fan( FanNum ).DeltaPress * pow_4( Fan( FanNum ).FanWheelDia ) ) / ( RhoAir * pow_2( FanVolFlow ) ); //[-]
			NormalizedEulerNum = std::log10( EulerNum / Fan( FanNum ).EuMaxEff );
			if ( NormalizedEulerNum <= 0.0 ) {
				Fan( FanNum ).FanWheelEff = CurveValue( Fan( FanNum ).PLFanEffNormCurveIndex, NormalizedEulerNum );
			} else {
				Fan( FanNum ).FanWheelEff = CurveValue( Fan( FanNum ).PLFanEffStallCurveIndex, NormalizedEulerNum );
			}
			Fan( FanNum ).FanWheelEff *= Fan( FanNum ).FanMaxEff; // [-]
			Fan( FanNum ).FanWheelEff = max( Fan( FanNum ).FanWheelEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors

			// Calculate max fan shaft power using fan air power and fan efficiency
			// at max fan static pressure rise and max fan volumetric flow
			Fan( FanNum ).FanShaftPower = ( Fan( FanNum ).FanAirPower / Fan( FanNum ).FanWheelEff ); //[W]
			Fan( FanNum ).FanShaftPwrMax = Fan( FanNum ).FanShaftPower; //[W]

			// Calculate fan shaft speed, motor speed, and fan torque using Wray dimensionless fan airflow model
			if ( NormalizedEulerNum <= 0.0 ) {
				FanDimFlow = CurveValue( Fan( FanNum ).DimFlowNormCurveIndex, NormalizedEulerNum ); //[-]
			} else {
				FanDimFlow = CurveValue( Fan( FanNum ).DimFlowStallCurveIndex, NormalizedEulerNum ); //[-]
			}
			FanSpdRadS = FanVolFlow / ( FanDimFlow * Fan( FanNum ).FanMaxDimFlow * pow_3( Fan( FanNum ).FanWheelDia ) ); //[rad/s]
			Fan( FanNum ).FanSpd = FanSpdRadS * 9.549296586; //[rpm, conversion factor is 30/PI]

			if ( Fan( FanNum ).PulleyDiaRatio == AutoSize ) {
				//WRITE(*,*) 'Autosizing pulley drive ratio'
				Fan( FanNum ).PulleyDiaRatio = Fan( FanNum ).FanSpd / Fan( FanNum ).MotorMaxSpd; //[-]
			}

			// For direct-drive, should have PulleyDiaRatio = 1
			MotorSpeed = Fan( FanNum ).FanSpd / Fan( FanNum ).PulleyDiaRatio; //[rpm]

			// Check for inconsistent drive ratio and motor speed, and report design fan speed with warning cpw14Sep2010
			if ( MotorSpeed > ( Fan( FanNum ).MotorMaxSpd + 1.e-5 ) ) {
				ShowWarningError( "Drive ratio for " + Fan( FanNum ).FanType + ": " + Fan( FanNum ).FanName + " is too low at design conditions -- check motor speed and drive ratio inputs" );
				ShowContinueError( "...Design fan speed [rev/min]: " + RoundSigDigits( Fan( FanNum ).FanSpd, 2 ) );
			}

			Fan( FanNum ).FanTrq = Fan( FanNum ).FanShaftPower / FanSpdRadS; //[N-m]

			if ( Fan( FanNum ).BeltMaxTorque == AutoSize ) {
				//WRITE(*,*) 'Autosizing fan belt'
				Fan( FanNum ).BeltMaxTorque = Fan( FanNum ).FanTrq; //[N-m]
			}
			// Adjust max belt torque using belt sizing factor
			Fan( FanNum ).BeltMaxTorque *= Fan( FanNum ).BeltSizingFactor; //[N-m]

			// Check for undersized belt and report design size with warning cpw14Sep2010
			if ( Fan( FanNum ).FanTrq > ( Fan( FanNum ).BeltMaxTorque + 1.e-5 ) ) {
				ShowWarningError( "Belt for " + Fan( FanNum ).FanType + ": " + Fan( FanNum ).FanName + " is undersized at design conditions -- check belt inputs" );
				ShowContinueError( "...Design belt output torque (without oversizing) [Nm]: " + RoundSigDigits( Fan( FanNum ).FanTrq, 2 ) );
			}

			// Calculate belt max efficiency using correlations and coefficients based on AMCA data
			// Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
			XbeltMax = std::log( Fan( FanNum ).FanShaftPwrMax / 746.0 ); //Natural log of belt output power in hp
			if ( Fan( FanNum ).BeltMaxEffCurveIndex != 0 ) {
				Fan( FanNum ).BeltMaxEff = std::exp( CurveValue( Fan( FanNum ).BeltMaxEffCurveIndex, XbeltMax ) ); //[-]
			} else {
				Fan( FanNum ).BeltMaxEff = 1.0; //No curve specified - use constant efficiency
			}

			// Calculate belt part-load drive efficiency and input power using correlations and coefficients based on ACEEE data
			FanTrqRatio = Fan( FanNum ).FanTrq / Fan( FanNum ).BeltMaxTorque; //[-]
			if ( ( FanTrqRatio <= Fan( FanNum ).BeltTorqueTrans ) && ( Fan( FanNum ).PLBeltEffReg1CurveIndex != 0 ) ) {
				BeltPLEff = CurveValue( Fan( FanNum ).PLBeltEffReg1CurveIndex, FanTrqRatio ); //[-]
			} else {
				if ( ( FanTrqRatio > Fan( FanNum ).BeltTorqueTrans ) && ( FanTrqRatio <= 1.0 ) && ( Fan( FanNum ).PLBeltEffReg2CurveIndex != 0 ) ) {
					BeltPLEff = CurveValue( Fan( FanNum ).PLBeltEffReg2CurveIndex, FanTrqRatio ); //[-]
				} else {
					if ( ( FanTrqRatio > 1.0 ) && ( Fan( FanNum ).PLBeltEffReg3CurveIndex != 0 ) ) {
						BeltPLEff = CurveValue( Fan( FanNum ).PLBeltEffReg3CurveIndex, FanTrqRatio ); //[-]
					} else {
						BeltPLEff = 1.0; //Direct drive or no curve specified - use constant efficiency
					}
				}
			}
			Fan( FanNum ).BeltEff = Fan( FanNum ).BeltMaxEff * BeltPLEff; //[-]
			Fan( FanNum ).BeltEff = max( Fan( FanNum ).BeltEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors
			Fan( FanNum ).BeltInputPower = Fan( FanNum ).FanShaftPower / Fan( FanNum ).BeltEff; //[W]

			if ( Fan( FanNum ).MotorMaxOutPwr == AutoSize ) {
				//WRITE(*,*) 'Autosizing fan motor'
				Fan( FanNum ).MotorMaxOutPwr = Fan( FanNum ).BeltInputPower;
			}
			// Adjust max motor output power using motor sizing factor
			Fan( FanNum ).MotorMaxOutPwr *= Fan( FanNum ).MotorSizingFactor; //[W]

			// Check for undersized motor and report design size with warning cpw14Sep2010
			if ( Fan( FanNum ).BeltInputPower > ( Fan( FanNum ).MotorMaxOutPwr + 1.e-5 ) ) {
				ShowWarningError( "Motor for " + Fan( FanNum ).FanType + ": " + Fan( FanNum ).FanName + " is undersized at design conditions -- check motor inputs" );
				ShowContinueError( "...Design motor output power (without oversizing) [W]: " + RoundSigDigits( Fan( FanNum ).BeltInputPower, 2 ) );
			}

			// Calculate motor max efficiency using correlations and coefficients based on MotorMaster+ data
			XmotorMax = std::log( Fan( FanNum ).MotorMaxOutPwr / 746.0 ); //Natural log of motor output power in hp
			if ( Fan( FanNum ).MotorMaxEffCurveIndex != 0 ) {
				Fan( FanNum ).MotorMaxEff = CurveValue( Fan( FanNum ).MotorMaxEffCurveIndex, XmotorMax ); //[-]
			} else {
				Fan( FanNum ).MotorMaxEff = 1.0; //No curve specified - use constant efficiency
			}

			// Calculate motor part-load efficiency and input power using correlations and coefficients based on MotorMaster+ data
			MotorOutPwrRatio = Fan( FanNum ).BeltInputPower / Fan( FanNum ).MotorMaxOutPwr; //[-]
			if ( Fan( FanNum ).PLMotorEffCurveIndex != 0 ) {
				MotorPLEff = CurveValue( Fan( FanNum ).PLMotorEffCurveIndex, MotorOutPwrRatio ); //[-]
			} else {
				MotorPLEff = 1.0; //No curve specified - use constant efficiency
			}
			Fan( FanNum ).MotEff = Fan( FanNum ).MotorMaxEff * MotorPLEff; //[-]
			Fan( FanNum ).MotEff = max( Fan( FanNum ).MotEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors

			// Calculate motor input power using belt input power and motor efficiency
			Fan( FanNum ).MotorInputPower = Fan( FanNum ).BeltInputPower / Fan( FanNum ).MotEff; //[W]

			// Calculate max VFD efficiency and input power using correlations and coefficients based on VFD type
			if ( ( Fan( FanNum ).VFDEffType == "SPEED" ) && ( Fan( FanNum ).VFDEffCurveIndex != 0 ) ) {
				VFDSpdRatio = MotorSpeed / Fan( FanNum ).MotorMaxSpd; //[-]
				Fan( FanNum ).VFDEff = CurveValue( Fan( FanNum ).VFDEffCurveIndex, VFDSpdRatio ); //[-]
			} else {
				if ( ( Fan( FanNum ).VFDEffType == "POWER" ) && ( Fan( FanNum ).VFDEffCurveIndex != 0 ) ) {
					if ( Fan( FanNum ).VFDMaxOutPwr == AutoSize ) {
						//WRITE(*,*) 'Autosizing fan VFD'
						Fan( FanNum ).VFDMaxOutPwr = Fan( FanNum ).MotorInputPower;
					}
					// Adjust max VFD output power using VFD sizing factor
					Fan( FanNum ).VFDMaxOutPwr *= Fan( FanNum ).VFDSizingFactor; //[W]

					// Check for undersized VFD and report design size with warning cpw14Sep2010
					if ( Fan( FanNum ).MotorInputPower > ( Fan( FanNum ).VFDMaxOutPwr + 1.e-5 ) ) {
						ShowWarningError( "VFD for " + Fan( FanNum ).FanType + ": " + Fan( FanNum ).FanName + " is undersized at design conditions -- check VFD inputs" );
						ShowContinueError( "...Design VFD output power (without oversizing) [W]: " + RoundSigDigits( Fan( FanNum ).MotorInputPower, 2 ) );
					}

					VFDOutPwrRatio = Fan( FanNum ).MotorInputPower / Fan( FanNum ).VFDMaxOutPwr; //[-]
					Fan( FanNum ).VFDEff = CurveValue( Fan( FanNum ).VFDEffCurveIndex, VFDOutPwrRatio ); //[-]
				} else {
					// No curve specified - use constant efficiency
					Fan( FanNum ).VFDMaxOutPwr = 0.0;
					Fan( FanNum ).VFDEff = 0.97;
				}
			}
			Fan( FanNum ).VFDEff = max( Fan( FanNum ).VFDEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors

			// Calculate VFD "rated" input power using motor input power and VFD efficiency
			RatedPower = Fan( FanNum ).MotorInputPower / Fan( FanNum ).VFDEff; //[W]

			// Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
			// Equivalent to Fan(FanNum)%FanAirPower / Fan(FanNum)%FanPower
			Fan( FanNum ).FanEff = Fan( FanNum ).FanWheelEff * Fan( FanNum ).BeltEff * Fan( FanNum ).MotEff * Fan( FanNum ).VFDEff;

			// Report fan, belt, motor, and VFD characteristics at design condition to .eio file cpw14Sep2010
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Fan Airflow [m3/s]", FanVolFlow );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Fan Static Pressure Rise [Pa]", Fan( FanNum ).DeltaPress );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Fan Shaft Power [W]", Fan( FanNum ).FanShaftPower );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Motor Output Power [W]", Fan( FanNum ).MotorMaxOutPwr );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design VFD Output Power [W]", Fan( FanNum ).VFDMaxOutPwr );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Rated Power [W]", RatedPower );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Drive Ratio [-]", Fan( FanNum ).PulleyDiaRatio );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Belt Output Torque [Nm]", Fan( FanNum ).BeltMaxTorque );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Fan Efficiency  [-]", Fan( FanNum ).FanWheelEff );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Maximum Belt Efficiency [-]", Fan( FanNum ).BeltMaxEff );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Belt Efficiency [-]", Fan( FanNum ).BeltEff );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Maximum Motor Efficiency [-]", Fan( FanNum ).MotorMaxEff );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Motor Efficiency [-]", Fan( FanNum ).MotEff );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design VFD Efficiency [-]", Fan( FanNum ).VFDEff );
			ReportSizingOutput( Fan( FanNum ).FanType, Fan( FanNum ).FanName, "Design Combined Efficiency [-]", Fan( FanNum ).FanEff );

			//cpw31Aug2010 Temporary code for debugging fan component model
			//    WRITE(300,*) TRIM(RoundSigDigits(RhoAir,4))//','//TRIM(RoundSigDigits(FanVolFlow,4)) &
			//    //','//TRIM(RoundSigDigits(FanOutletVelPress,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%DeltaPress,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanAirPower,4))//','//TRIM(RoundSigDigits(EulerNum,4)) &
			//    //','//TRIM(RoundSigDigits(NormalizedEulerNum,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanWheelEff,4))
			//    WRITE(301,*) TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPower,4))//','//TRIM(RoundSigDigits(FanDimFlow,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanTrq,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanSpd,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPwrMax,4))//','//TRIM(RoundSigDigits(XbeltMax,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltMaxEff,4))//','//TRIM(RoundSigDigits(FanTrqRatio,4))
			//    WRITE(302,*) TRIM(RoundSigDigits(BeltPLEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%BeltEff,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltInputPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxOutPwr,4)) &
			//    //','//TRIM(RoundSigDigits(XmotorMax,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxEff,4)) &
			//    //','//TRIM(RoundSigDigits(MotorOutPwrRatio,4))//','//TRIM(RoundSigDigits(MotorPLEff,4))
			//    WRITE(303,*) TRIM(RoundSigDigits(Fan(FanNum)%MotEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorInputPower,4)) &
			//    //','//TRIM(RoundSigDigits(VFDOutPwrRatio,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%VFDEff,4)) &
			//    //','//TRIM(RoundSigDigits(RatedPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanEff,4)) &
			//    //','//TRIM(RoundSigDigits(0.0d0,4))//','//TRIM(RoundSigDigits(0.0d0,4))
			//    WRITE(304,*) TRIM("Fan")//','//TRIM("Sizing")

			//cpw31Aug2010 Temporary code to write headers for component fan model debug files
			//    WRITE(300,*) 'Rho,VolFLOW,dPvOut,dP,AirPwr,Eu,NrmEu,FWEff'
			//    WRITE(301,*) 'ShftPwr,DimFlow,Trq,FSpd,BPwrOut,XBmax,BMaxEff,TrqRat'
			//    WRITE(302,*) 'BPLEff,BEff,BPwrIn,MPwrOut,XMmax,MMaxEff,MPwrRat,MPLEff'
			//    WRITE(303,*) 'MEff,MPwrIn,VPwrRat,VEff,FanPwr,FanEff,PwrLoss,dEnthalpy'
			//    WRITE(304,*) 'Date,Period'
		} //End fan component sizing

		equipName = Fan( FanNum ).FanName;

		//cpw31Aug2010 Rearrange order to match table and use FanVolFlow to calculate RatedPower
		//ALSO generates values if Component Model fan, for which DeltaPress and FanEff vary with flow
		PreDefTableEntry( pdchFanType, equipName, Fan( FanNum ).FanType );
		PreDefTableEntry( pdchFanTotEff, equipName, Fan( FanNum ).FanEff );
		PreDefTableEntry( pdchFanDeltaP, equipName, Fan( FanNum ).DeltaPress );
		PreDefTableEntry( pdchFanVolFlow, equipName, FanVolFlow );
		RatedPower = FanVolFlow * Fan( FanNum ).DeltaPress / Fan( FanNum ).FanEff; // total fan power
		PreDefTableEntry( pdchFanPwr, equipName, RatedPower );
		if ( FanVolFlow != 0.0 ) {
			PreDefTableEntry( pdchFanPwrPerFlow, equipName, RatedPower / FanVolFlow );
		}
		PreDefTableEntry( pdchFanMotorIn, equipName, Fan( FanNum ).MotInAirFrac );
		PreDefTableEntry( pdchFanEndUse, equipName, Fan( FanNum ).EndUseSubcategoryName );

		NVPerfNum = Fan( FanNum ).NVPerfNum;
		if ( NVPerfNum > 0 ) {
			if ( NightVentPerf( NVPerfNum ).MaxAirFlowRate == AutoSize ) {
				NightVentPerf( NVPerfNum ).MaxAirFlowRate = Fan( FanNum ).MaxAirFlowRate;
			}
		}

		if ( ++NumFansSized == NumFans ) FanNumericFields.deallocate(); // remove temporary array for field names at end of sizing

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimSimpleFan( int const FanNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Unknown
		//       DATE WRITTEN   Unknown
		//       MODIFIED       Brent Griffith, May 2009, added EMS override
		//                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
		//                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple constant volume fan.

		// METHODOLOGY EMPLOYED:
		// Converts design pressure rise and efficiency into fan power and temperature rise
		// Constant fan pressure rise is assumed.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RhoAir;
		Real64 DeltaPress; // [N/m2]
		Real64 FanEff;
		Real64 MotInAirFrac;
		Real64 MotEff;
		Real64 MassFlow; // [kg/sec]
		//unused0909      REAL(r64) Tin         ! [C]
		//unused0909      REAL(r64) Win
		Real64 FanShaftPower; // power delivered to fan shaft
		Real64 PowerLossToAir; // fan and motor loss to air stream (watts)
		int NVPerfNum;

		NVPerfNum = Fan( FanNum ).NVPerfNum;

		if ( NightVentOn && NVPerfNum > 0 ) {
			DeltaPress = NightVentPerf( NVPerfNum ).DeltaPress;
			FanEff = NightVentPerf( NVPerfNum ).FanEff;
			MotEff = NightVentPerf( NVPerfNum ).MotEff;
			MotInAirFrac = NightVentPerf( NVPerfNum ).MotInAirFrac;
		} else {
			DeltaPress = Fan( FanNum ).DeltaPress;
			FanEff = Fan( FanNum ).FanEff;
			MotEff = Fan( FanNum ).MotEff;
			MotInAirFrac = Fan( FanNum ).MotInAirFrac;
		}

		// For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
		//unused0909   Tin        = Fan(FanNum)%InletAirTemp
		//unused0909   Win        = Fan(FanNum)%InletAirHumRat
		RhoAir = Fan( FanNum ).RhoAirStdInit;
		MassFlow = Fan( FanNum ).InletAirMassFlowRate;

		//Faulty fan operations_Jun. 2015, zrp
		//Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
		if ( Fan( FanNum ).FaultyFilterFlag && ( FaultsManager::NumFaultyAirFilter > 0 ) && ( ! WarmupFlag ) && ( ! DoingSizing ) && DoWeathSim ) {

			int iFault = Fan( FanNum ).FaultyFilterIndex;

			// Check fault availability schedules
			if ( GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).AvaiSchedPtr ) > 0.0 ) {
				Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

				FanDesignFlowRateDec = CalFaultyFanAirFlowReduction( Fan( FanNum ).FanName, Fan( FanNum ).MaxAirFlowRate, Fan( FanNum ).DeltaPress,
					( GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterPressFracSchePtr ) - 1 ) * Fan( FanNum ).DeltaPress,
					FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterFanCurvePtr );

				//Update MassFlow & DeltaPress of the fan
				MassFlow = min( MassFlow, Fan( FanNum ).MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir );
				DeltaPress = GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterPressFracSchePtr ) * Fan( FanNum ).DeltaPress;
			}
		}

		//EMS overwrite MassFlow, DeltaPress, and FanEff
		if ( Fan( FanNum ).EMSMaxMassFlowOverrideOn ) MassFlow = Fan( FanNum ).EMSAirMassFlowValue;
		if ( Fan( FanNum ).EMSFanPressureOverrideOn ) DeltaPress = Fan( FanNum ).EMSFanPressureValue;
		if ( Fan( FanNum ).EMSFanEffOverrideOn ) FanEff = Fan( FanNum ).EMSFanEffValue;

		MassFlow = min( MassFlow, Fan( FanNum ).MaxAirMassFlowRate );
		MassFlow = max( MassFlow, Fan( FanNum ).MinAirMassFlowRate );

		//Determine the Fan Schedule for the Time step
		if ( ( GetCurrentScheduleValue( Fan( FanNum ).AvailSchedPtrNum ) > 0.0 || LocalTurnFansOn ) && ! LocalTurnFansOff && MassFlow > 0.0 ) {
			//Fan is operating
			Fan( FanNum ).FanPower = MassFlow * DeltaPress / ( FanEff * RhoAir ); // total fan power
			FanShaftPower = MotEff * Fan( FanNum ).FanPower; // power delivered to shaft
			PowerLossToAir = FanShaftPower + ( Fan( FanNum ).FanPower - FanShaftPower ) * MotInAirFrac;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy + PowerLossToAir / MassFlow;
			// This fan does not change the moisture or Mass Flow across the component
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirMassFlowRate = MassFlow;
			Fan( FanNum ).OutletAirTemp = PsyTdbFnHW( Fan( FanNum ).OutletAirEnthalpy, Fan( FanNum ).OutletAirHumRat );

		} else {
			//Fan is off and not operating no power consumed and mass flow rate.
			Fan( FanNum ).FanPower = 0.0;
			FanShaftPower = 0.0;
			PowerLossToAir = 0.0;
			Fan( FanNum ).OutletAirMassFlowRate = 0.0;
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy;
			Fan( FanNum ).OutletAirTemp = Fan( FanNum ).InletAirTemp;
			// Set the Control Flow variables to 0.0 flow when OFF.
			Fan( FanNum ).MassFlowRateMaxAvail = 0.0;
			Fan( FanNum ).MassFlowRateMinAvail = 0.0;

		}

	}

	void
	SimVariableVolumeFan(
		int const FanNum,
		Optional< Real64 const > PressureRise
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Unknown
		//       DATE WRITTEN   Unknown
		//       MODIFIED       Phil Haves
		//                      Brent Griffith, May 2009 for EMS
		//                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
		//                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple variable volume fan.

		// METHODOLOGY EMPLOYED:
		// Converts design pressure rise and efficiency into fan power and temperature rise
		// Constant fan pressure rise is assumed.
		// Uses curves of fan power fraction vs. fan part load to determine fan power at
		// off design conditions.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RhoAir;
		Real64 DeltaPress; // [N/m2 = Pa]
		Real64 FanEff; // Total fan efficiency - combined efficiency of fan, drive train,
		// motor and variable speed controller (if any)
		Real64 MaxAirFlowRate;
		Real64 MaxAirMassFlowRate;
		Real64 MotInAirFrac;
		Real64 MotEff;
		Real64 MassFlow; // [kg/sec]
		//unused0909      REAL(r64) Tin         ! [C]
		//unused0909      REAL(r64) Win
		Real64 PartLoadFrac;
		//unused0909      REAL(r64) MaxFlowFrac   !Variable Volume Fan Max Flow Fraction [-]
		Real64 MinFlowFrac; // Variable Volume Fan Min Flow Fraction [-]
		static Real64 FlowFracForPower( 0.0 ); // Variable Volume Fan Flow Fraction for power calcs[-]
		static Real64 FlowFracActual( 0.0 ); // actual VAV fan flow fraction
		Real64 FanShaftPower; // power delivered to fan shaft
		Real64 PowerLossToAir; // fan and motor loss to air stream (watts)
		int NVPerfNum;

		// added to address the fan heat issue during low air flow conditions
		Real64 MinFlowFracLimitFanHeat; // Minimum Fan Flow Fraction Limit for Fan Heat at Low Airflow [-]
		Real64 FanPoweratLowMinimum; // Fan Power at Low Minimum Airflow [W]
		Real64 PartLoadFracatLowMin;
		Real64 DeltaTAcrossFan; // Air temperature rise across the fan due to fan heat [C]

		// Simple Variable Volume Fan - default values from DOE-2
		// Type of Fan          Coeff1       Coeff2       Coeff3        Coeff4      Coeff5
		// INLET VANE DAMPERS   0.35071223   0.30850535   -0.54137364   0.87198823  0.000
		// DISCHARGE DAMPERS    0.37073425   0.97250253   -0.34240761   0.000       0.000
		// VARIABLE SPEED MOTOR 0.0015302446 0.0052080574  1.1086242   -0.11635563  0.000

		NVPerfNum = Fan( FanNum ).NVPerfNum;
		MaxAirFlowRate = Fan( FanNum ).MaxAirFlowRate;

		if ( NightVentOn && NVPerfNum > 0 ) {
			DeltaPress = NightVentPerf( NVPerfNum ).DeltaPress;
			FanEff = NightVentPerf( NVPerfNum ).FanEff;
			MotEff = NightVentPerf( NVPerfNum ).MotEff;
			MotInAirFrac = NightVentPerf( NVPerfNum ).MotInAirFrac;
			MaxAirMassFlowRate = NightVentPerf( NVPerfNum ).MaxAirMassFlowRate;
		} else {
			if ( present( PressureRise ) ) {
				DeltaPress = PressureRise;
			} else {
				DeltaPress = Fan( FanNum ).DeltaPress;
			}
			FanEff = Fan( FanNum ).FanEff;
			MotEff = Fan( FanNum ).MotEff;
			MotInAirFrac = Fan( FanNum ).MotInAirFrac;
			MaxAirMassFlowRate = Fan( FanNum ).MaxAirMassFlowRate;
		}

		//unused0909  Tin         = Fan(FanNum)%InletAirTemp
		//unused0909  Win         = Fan(FanNum)%InletAirHumRat
		RhoAir = Fan( FanNum ).RhoAirStdInit;
		MassFlow = Fan( FanNum ).InletAirMassFlowRate;

		//Faulty fan operations_Apr. 2015, zrp
		//Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
		if ( Fan( FanNum ).FaultyFilterFlag && ( FaultsManager::NumFaultyAirFilter > 0 ) && ( ! WarmupFlag ) && ( ! DoingSizing ) && DoWeathSim && ( !Fan( FanNum ).EMSMaxMassFlowOverrideOn ) ) {

			int iFault = Fan( FanNum ).FaultyFilterIndex;

			// Check fault availability schedules
			if ( GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).AvaiSchedPtr ) > 0.0 ) {
				Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

				FanDesignFlowRateDec = CalFaultyFanAirFlowReduction( Fan( FanNum ).FanName, Fan( FanNum ).MaxAirFlowRate, Fan( FanNum ).DeltaPress,
					( GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterPressFracSchePtr ) - 1 ) * Fan( FanNum ).DeltaPress,
					FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterFanCurvePtr );

				//Update MassFlow & DeltaPress of the fan
				MaxAirFlowRate = Fan( FanNum ).MaxAirFlowRate - FanDesignFlowRateDec;
				MaxAirMassFlowRate = Fan( FanNum ).MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir;
				DeltaPress = GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterPressFracSchePtr ) * Fan( FanNum ).DeltaPress;
			}
		}

		//EMS overwrite MassFlow, DeltaPress, and FanEff
		if ( Fan( FanNum ).EMSFanPressureOverrideOn ) DeltaPress = Fan( FanNum ).EMSFanPressureValue;
		if ( Fan( FanNum ).EMSFanEffOverrideOn ) FanEff = Fan( FanNum ).EMSFanEffValue;
		if ( Fan( FanNum ).EMSMaxMassFlowOverrideOn ) MassFlow = Fan( FanNum ).EMSAirMassFlowValue;

		MassFlow = min( MassFlow, MaxAirMassFlowRate );

		//Determine the Fan Schedule for the Time step
		if ( ( GetCurrentScheduleValue( Fan( FanNum ).AvailSchedPtrNum ) > 0.0 || LocalTurnFansOn ) && ! LocalTurnFansOff && MassFlow > 0.0 ) {
			//Fan is operating - calculate power loss and enthalpy rise
			//  Fan(FanNum)%FanPower = PartLoadFrac*FullMassFlow*DeltaPress/(FanEff*RhoAir) ! total fan power
			// Calculate and check limits on fraction of system flow
			//unused0909    MaxFlowFrac = 1.0
			// MinFlowFrac is calculated from the ration of the volume flows and is non-dimensional
			MinFlowFrac = Fan( FanNum ).MinAirFlowRate / MaxAirFlowRate;
			// The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
			FlowFracActual = MassFlow / MaxAirMassFlowRate;

			// Calculate the part Load Fraction             (PH 7/13/03)

			FlowFracForPower = max( MinFlowFrac, min( FlowFracActual, 1.0 ) ); // limit flow fraction to allowed range
			if ( NightVentOn && NVPerfNum > 0 ) {
				PartLoadFrac = 1.0;
			} else {
				PartLoadFrac = Fan( FanNum ).FanCoeff( 1 ) + Fan( FanNum ).FanCoeff( 2 ) * FlowFracForPower + Fan( FanNum ).FanCoeff( 3 ) * pow_2( FlowFracForPower ) + Fan( FanNum ).FanCoeff( 4 ) * pow_3( FlowFracForPower ) + Fan( FanNum ).FanCoeff( 5 ) * pow_4( FlowFracForPower );
			}

			Fan( FanNum ).FanPower = PartLoadFrac * MaxAirMassFlowRate * DeltaPress / ( FanEff * RhoAir ); // total fan power (PH 7/13/03)

			FanShaftPower = MotEff * Fan( FanNum ).FanPower; // power delivered to shaft
			PowerLossToAir = FanShaftPower + ( Fan( FanNum ).FanPower - FanShaftPower ) * MotInAirFrac;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy + PowerLossToAir / MassFlow;
			// This fan does not change the moisture or Mass Flow across the component
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirMassFlowRate = MassFlow;
			Fan( FanNum ).OutletAirTemp = PsyTdbFnHW( Fan( FanNum ).OutletAirEnthalpy, Fan( FanNum ).OutletAirHumRat );

			// KHL/FB, 2/10/2011. NFP implemented as CR 8338.
			// When fan air flow is less than 10%, the fan power curve is linearized between the 10% to 0% to
			//  avoid the unrealistic high temperature rise across the fan.
			// TH, 2/15/2011
			// This change caused diffs for VAV systems when fan runs at less than 10% flow conditions.
			//  A potential way to improve is to check the temperature rise across the fan first,
			//  if it is too high (say > 20C) then applies the code.
			DeltaTAcrossFan = Fan( FanNum ).OutletAirTemp - Fan( FanNum ).InletAirTemp;
			if ( DeltaTAcrossFan > 20.0 ) {
				MinFlowFracLimitFanHeat = 0.10;
				if ( FlowFracForPower < MinFlowFracLimitFanHeat ) {
					PartLoadFracatLowMin = Fan( FanNum ).FanCoeff( 1 ) + Fan( FanNum ).FanCoeff( 2 ) * MinFlowFracLimitFanHeat + Fan( FanNum ).FanCoeff( 3 ) * pow_2( MinFlowFracLimitFanHeat ) + Fan( FanNum ).FanCoeff( 4 ) * pow_3( MinFlowFracLimitFanHeat ) + Fan( FanNum ).FanCoeff( 5 ) * pow_4( MinFlowFracLimitFanHeat );
					FanPoweratLowMinimum = PartLoadFracatLowMin * MaxAirMassFlowRate * DeltaPress / ( FanEff * RhoAir );
					Fan( FanNum ).FanPower = FlowFracForPower * FanPoweratLowMinimum / MinFlowFracLimitFanHeat;
				} else if ( FlowFracActual < MinFlowFracLimitFanHeat ) {
					PartLoadFracatLowMin = Fan( FanNum ).FanCoeff( 1 ) + Fan( FanNum ).FanCoeff( 2 ) * MinFlowFracLimitFanHeat + Fan( FanNum ).FanCoeff( 3 ) * pow_2( MinFlowFracLimitFanHeat ) + Fan( FanNum ).FanCoeff( 4 ) * pow_3( MinFlowFracLimitFanHeat ) + Fan( FanNum ).FanCoeff( 5 ) * pow_4( MinFlowFracLimitFanHeat );
					FanPoweratLowMinimum = PartLoadFracatLowMin * MaxAirMassFlowRate * DeltaPress / ( FanEff * RhoAir );
					Fan( FanNum ).FanPower = FlowFracActual * FanPoweratLowMinimum / MinFlowFracLimitFanHeat;
				}
				FanShaftPower = MotEff * Fan( FanNum ).FanPower; // power delivered to shaft
				PowerLossToAir = FanShaftPower + ( Fan( FanNum ).FanPower - FanShaftPower ) * MotInAirFrac;
				Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy + PowerLossToAir / MassFlow;
				// This fan does not change the moisture or Mass Flow across the component
				Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
				Fan( FanNum ).OutletAirMassFlowRate = MassFlow;
				Fan( FanNum ).OutletAirTemp = PsyTdbFnHW( Fan( FanNum ).OutletAirEnthalpy, Fan( FanNum ).OutletAirHumRat );
			}

		} else {
			//Fan is off and not operating no power consumed and mass flow rate.
			Fan( FanNum ).FanPower = 0.0;
			FanShaftPower = 0.0;
			PowerLossToAir = 0.0;
			Fan( FanNum ).OutletAirMassFlowRate = 0.0;
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy;
			Fan( FanNum ).OutletAirTemp = Fan( FanNum ).InletAirTemp;
			// Set the Control Flow variables to 0.0 flow when OFF.
			Fan( FanNum ).MassFlowRateMaxAvail = 0.0;
			Fan( FanNum ).MassFlowRateMinAvail = 0.0;
		}

	}

	void
	SimOnOffFan(
		int const FanNum,
		Optional< Real64 const > SpeedRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Unknown
		//       DATE WRITTEN   Unknown
		//       MODIFIED       Shirey, May 2001
		//                      R. Raustad - FSEC, Jan 2009 - added SpeedRatio for multi-speed fans
		//                      Brent Griffith, May 2009 for EMS
		//                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
		//                      Rongpeng Zhang, April 2015, added faulty fan operations due to fouling air filters
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple on/off fan.

		// METHODOLOGY EMPLOYED:
		// Converts design pressure rise and efficiency into fan power and temperature rise
		// Constant fan pressure rise is assumed.
		// Uses curves of fan power fraction vs. fan part load to determine fan power at
		// off design conditions.
		// Same as simple (constant volume) fan, except added part-load curve input

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RhoAir;
		Real64 DeltaPress; // [N/m2]
		Real64 FanEff;
		Real64 MassFlow; // [kg/sec]
		Real64 MaxAirMassFlowRate; // [kg/sec]
		//unused0909      REAL(r64) Tin         ! [C]
		//unused0909      REAL(r64) Win
		Real64 PartLoadRatio; // Ratio of actual mass flow rate to max mass flow rate
		Real64 FlowFrac; // Actual Fan Flow Fraction = actual mass flow rate / max air mass flow rate
		Real64 FanShaftPower; // power delivered to fan shaft
		Real64 PowerLossToAir; // fan and motor loss to air stream (watts)
		Real64 SpeedRaisedToPower; // Result of the speed ratio raised to the power of n (Curve object)
		Real64 EffRatioAtSpeedRatio; // Efficeincy ratio at current speed ratio (Curve object)
		static int ErrCount( 0 );

		//unused0909   Tin        = Fan(FanNum)%InletAirTemp
		//unused0909   Win        = Fan(FanNum)%InletAirHumRat
		MassFlow = Fan( FanNum ).InletAirMassFlowRate;
		MaxAirMassFlowRate = Fan( FanNum ).MaxAirMassFlowRate;
		DeltaPress = Fan( FanNum ).DeltaPress;
		FanEff = Fan( FanNum ).FanEff;
		RhoAir = Fan( FanNum ).RhoAirStdInit;

		//Faulty fan operations_Apr. 2015, zrp
		//Update MassFlow & DeltaPress if there are fouling air filters corresponding to the fan
		if ( Fan( FanNum ).FaultyFilterFlag && ( FaultsManager::NumFaultyAirFilter > 0 ) && ( ! WarmupFlag ) && ( ! DoingSizing ) && DoWeathSim && ( !Fan( FanNum ).EMSMaxMassFlowOverrideOn ) ) {

			int iFault = Fan( FanNum ).FaultyFilterIndex;

			// Check fault availability schedules
			if ( GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).AvaiSchedPtr ) > 0.0 ) {
				Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]

				FanDesignFlowRateDec = CalFaultyFanAirFlowReduction( Fan( FanNum ).FanName, Fan( FanNum ).MaxAirFlowRate, Fan( FanNum ).DeltaPress,
					( GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterPressFracSchePtr ) - 1 ) * Fan( FanNum ).DeltaPress,
					FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterFanCurvePtr );

				//Update MassFlow & DeltaPress of the fan
				MaxAirMassFlowRate = Fan( FanNum ).MaxAirMassFlowRate - FanDesignFlowRateDec * RhoAir;
				DeltaPress = GetCurrentScheduleValue( FaultsManager::FaultsFouledAirFilters( iFault ).FaultyAirFilterPressFracSchePtr ) * Fan( FanNum ).DeltaPress;
			}
		}

		//EMS overwrite MassFlow, DeltaPress, and FanEff
		if ( Fan( FanNum ).EMSMaxMassFlowOverrideOn ) MassFlow = Fan( FanNum ).EMSAirMassFlowValue;
		if ( Fan( FanNum ).EMSFanPressureOverrideOn ) DeltaPress = Fan( FanNum ).EMSFanPressureValue;
		if ( Fan( FanNum ).EMSFanEffOverrideOn ) FanEff = Fan( FanNum ).EMSFanEffValue;

		MassFlow = min( MassFlow, MaxAirMassFlowRate );
		MassFlow = max( MassFlow, Fan( FanNum ).MinAirMassFlowRate );
		Fan( FanNum ).FanRuntimeFraction = 0.0;

		// Determine the Fan Schedule for the Time step
		if ( ( GetCurrentScheduleValue( Fan( FanNum ).AvailSchedPtrNum ) > 0.0 || LocalTurnFansOn ) && ! LocalTurnFansOff && MassFlow > 0.0 && Fan( FanNum ).MaxAirMassFlowRate > 0.0 ) {
			// The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
			FlowFrac = MassFlow / MaxAirMassFlowRate;

			// Calculate the part load ratio, can't be greater than 1
			PartLoadRatio = min( 1.0, FlowFrac );
			// Fan is operating
			if ( OnOffFanPartLoadFraction <= 0.0 ) {
				ShowRecurringWarningErrorAtEnd( "Fan:OnOff, OnOffFanPartLoadFraction <= 0.0, Reset to 1.0", ErrCount );
				OnOffFanPartLoadFraction = 1.0; // avoid divide by zero or negative PLF
			}

			if ( OnOffFanPartLoadFraction < 0.7 ) {
				OnOffFanPartLoadFraction = 0.7; // a warning message is already issued from the DX coils or gas heating coil
			}

			// Keep fan runtime fraction between 0.0 and 1.0, and RTF >= PLR
			if ( OnOffFanPartLoadFraction >= 1.0 ) {
				Fan( FanNum ).FanRuntimeFraction = PartLoadRatio;
			} else {
				Fan( FanNum ).FanRuntimeFraction = max( 0.0, min( 1.0, PartLoadRatio / OnOffFanPartLoadFraction ) );
			}
			// The fan speed ratio (passed from parent) determines the fan power according to fan laws
			if ( present( SpeedRatio ) ) {
				//    Fan(FanNum)%FanPower = MassFlow*DeltaPress/(FanEff*RhoAir*OnOffFanPartLoadFraction)! total fan power
				Fan( FanNum ).FanPower = MaxAirMassFlowRate * Fan( FanNum ).FanRuntimeFraction * DeltaPress / ( FanEff * RhoAir );

				//    Do not modify fan power calculation unless fan power vs speed ratio curve is used.
				if ( Fan( FanNum ).FanPowerRatAtSpeedRatCurveIndex > 0 ) {

					//      adjust RTF to be in line with speed ratio (i.e., MaxAirMassFlowRate is not MAX when SpeedRatio /= 1)
					//      PLR = Mdot/MAXFlow => Mdot/(MAXFlow * SpeedRatio), RTF = PLR/PLF => PLR/SpeedRatio/PLF = RTF / SpeedRatio
					if ( SpeedRatio > 0.0 ) Fan( FanNum ).FanRuntimeFraction = min( 1.0, Fan( FanNum ).FanRuntimeFraction / SpeedRatio );

					SpeedRaisedToPower = CurveValue( Fan( FanNum ).FanPowerRatAtSpeedRatCurveIndex, SpeedRatio );
					if ( SpeedRaisedToPower < 0.0 ) {
						if ( Fan( FanNum ).OneTimePowerRatioCheck && ! WarmupFlag ) {
							ShowSevereError( cFanTypes( Fan( FanNum ).FanType_Num ) + " = " + Fan( FanNum ).FanName + "\"" );
							ShowContinueError( "Error in Fan Power Ratio curve. Curve output less than 0.0." );
							ShowContinueError( "Curve output = " + TrimSigDigits( SpeedRaisedToPower, 5 ) + ", fan speed ratio = " + TrimSigDigits( SpeedRatio, 5 ) );
							ShowContinueError( "Check curve coefficients to ensure proper power ratio as a function of fan speed ratio." );
							ShowContinueError( "Resetting Fan Power Ratio curve output to 0.0 and the simulation continues." );
							ShowContinueErrorTimeStamp( "Occurrence info:" );
							Fan( FanNum ).OneTimePowerRatioCheck = false;
						}
						SpeedRaisedToPower = 0.0;
					}
					if ( Fan( FanNum ).FanEffRatioCurveIndex > 0 && ! WarmupFlag ) {
						EffRatioAtSpeedRatio = CurveValue( Fan( FanNum ).FanEffRatioCurveIndex, SpeedRatio );
						if ( EffRatioAtSpeedRatio < 0.01 ) {
							if ( Fan( FanNum ).OneTimeEffRatioCheck && ! WarmupFlag ) {
								ShowSevereError( cFanTypes( Fan( FanNum ).FanType_Num ) + " = " + Fan( FanNum ).FanName + "\"" );
								ShowContinueError( "Error in Fan Efficiency Ratio curve. Curve output less than 0.01." );
								ShowContinueError( "Curve output = " + TrimSigDigits( EffRatioAtSpeedRatio, 5 ) + ", fan speed ratio = " + TrimSigDigits( SpeedRatio, 5 ) );
								ShowContinueError( "Check curve coefficients to ensure proper efficiency ratio as a function of fan speed ratio." );
								ShowContinueError( "Resetting Fan Efficiency Ratio curve output to 0.01 and the simulation continues." );
								ShowContinueErrorTimeStamp( "Occurrence info:" );
								Fan( FanNum ).OneTimeEffRatioCheck = false;
							}
							EffRatioAtSpeedRatio = 0.01;
						}
					} else {
						EffRatioAtSpeedRatio = 1.0;
					}
					Fan( FanNum ).FanPower *= SpeedRaisedToPower / EffRatioAtSpeedRatio;
				}
			} else {
				Fan( FanNum ).FanPower = MaxAirMassFlowRate * Fan( FanNum ).FanRuntimeFraction * DeltaPress / ( FanEff * RhoAir ); //total fan power
			}

			// OnOffFanPartLoadFraction is passed via DataHVACGlobals from the cooling or heating coil that is
			//   requesting the fan to operate in cycling fan/cycling coil mode
			OnOffFanPartLoadFraction = 1.0; // reset to 1 in case other on/off fan is called without a part load curve
			FanShaftPower = Fan( FanNum ).MotEff * Fan( FanNum ).FanPower; // power delivered to shaft
			PowerLossToAir = FanShaftPower + ( Fan( FanNum ).FanPower - FanShaftPower ) * Fan( FanNum ).MotInAirFrac;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy + PowerLossToAir / MassFlow;
			// This fan does not change the moisture or Mass Flow across the component
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirMassFlowRate = MassFlow;
			//   Fan(FanNum)%OutletAirTemp = Tin + PowerLossToAir/(MassFlow*PsyCpAirFnWTdb(Win,Tin))
			Fan( FanNum ).OutletAirTemp = PsyTdbFnHW( Fan( FanNum ).OutletAirEnthalpy, Fan( FanNum ).OutletAirHumRat );
		} else {
			// Fan is off and not operating no power consumed and mass flow rate.
			Fan( FanNum ).FanPower = 0.0;
			FanShaftPower = 0.0;
			PowerLossToAir = 0.0;
			Fan( FanNum ).OutletAirMassFlowRate = 0.0;
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy;
			Fan( FanNum ).OutletAirTemp = Fan( FanNum ).InletAirTemp;
			// Set the Control Flow variables to 0.0 flow when OFF.
			Fan( FanNum ).MassFlowRateMaxAvail = 0.0;
			Fan( FanNum ).MassFlowRateMinAvail = 0.0;
		}

	}

	void
	SimZoneExhaustFan( int const FanNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Jan 2000
		//       MODIFIED       Brent Griffith, May 2009 for EMS
		//                      Brent Griffith, Feb 2013 controls upgrade
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the Zone Exhaust Fan

		// METHODOLOGY EMPLOYED:
		// Converts design pressure rise and efficiency into fan power and temperature rise
		// Constant fan pressure rise is assumed.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RhoAir;
		Real64 DeltaPress; // [N/m2]
		Real64 FanEff;
		Real64 MassFlow; // [kg/sec]
		Real64 Tin; // [C]
		Real64 PowerLossToAir; // fan and motor loss to air stream (watts)
		bool FanIsRunning = false; // There seems to be a missing else case below unless false is assumed

		DeltaPress = Fan( FanNum ).DeltaPress;
		if ( Fan( FanNum ).EMSFanPressureOverrideOn ) DeltaPress = Fan( FanNum ).EMSFanPressureValue;

		FanEff = Fan( FanNum ).FanEff;
		if ( Fan( FanNum ).EMSFanEffOverrideOn ) FanEff = Fan( FanNum ).EMSFanEffValue;

		// For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
		Tin = Fan( FanNum ).InletAirTemp;
		RhoAir = Fan( FanNum ).RhoAirStdInit;
		MassFlow = Fan( FanNum ).InletAirMassFlowRate;

		//  When the AvailManagerMode == ExhaustFanCoupledToAvailManagers then the
		//  Exhaust Fan is  interlocked with air loop availability via global TurnFansOn and TurnFansOff variables.
		//  There is now the option to control if user wants to decouple air loop operation and exhaust fan operation
		//  (zone air mass balance issues). If in the future want to allow for zone level local availability manager
		//  then the optional arguments ZoneCompTurnFansOn and ZoneCompTurnFansOff will need
		//  to be passed to SimulateFanComponents, and TurnFansOn must be changed to LocalTurnFansOn
		//  and TurnFansOff to LocalTurnFansOff in the IF statement below.

		// apply controls to determine if operating
		if ( Fan( FanNum ).AvailManagerMode == ExhaustFanCoupledToAvailManagers ) {
			if ( ( ( GetCurrentScheduleValue( Fan( FanNum ).AvailSchedPtrNum ) > 0.0 ) || TurnFansOn ) && ! TurnFansOff && MassFlow > 0.0 ) { // available
				if ( Fan( FanNum ).MinTempLimitSchedNum > 0 ) {
					if ( Tin >= GetCurrentScheduleValue( Fan( FanNum ).MinTempLimitSchedNum ) ) {
						FanIsRunning = true;
					} else {
						FanIsRunning = false;
					}
				} else {
					FanIsRunning = true;
				}
			} else {
				FanIsRunning = false;
			}

		} else if ( Fan( FanNum ).AvailManagerMode == ExhaustFanDecoupledFromAvailManagers ) {
			if ( GetCurrentScheduleValue( Fan( FanNum ).AvailSchedPtrNum ) > 0.0 && MassFlow > 0.0 ) {
				if ( Fan( FanNum ).MinTempLimitSchedNum > 0 ) {
					if ( Tin >= GetCurrentScheduleValue( Fan( FanNum ).MinTempLimitSchedNum ) ) {
						FanIsRunning = true;
					} else {
						FanIsRunning = false;
					}
				} else {
					FanIsRunning = true;
				}
			} else {
				FanIsRunning = false;
			}
		}

		if ( FanIsRunning ) {
			//Fan is operating
			Fan( FanNum ).FanPower = MassFlow * DeltaPress / ( FanEff * RhoAir ); // total fan power
			PowerLossToAir = Fan( FanNum ).FanPower;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy + PowerLossToAir / MassFlow;
			// This fan does not change the moisture or Mass Flow across the component
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirMassFlowRate = MassFlow;
			Fan( FanNum ).OutletAirTemp = PsyTdbFnHW( Fan( FanNum ).OutletAirEnthalpy, Fan( FanNum ).OutletAirHumRat );

		} else {
			//Fan is off and not operating no power consumed and mass flow rate.
			Fan( FanNum ).FanPower = 0.0;
			PowerLossToAir = 0.0;
			Fan( FanNum ).OutletAirMassFlowRate = 0.0;
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy;
			Fan( FanNum ).OutletAirTemp = Fan( FanNum ).InletAirTemp;
			// Set the Control Flow variables to 0.0 flow when OFF.
			Fan( FanNum ).MassFlowRateMaxAvail = 0.0;
			Fan( FanNum ).MassFlowRateMinAvail = 0.0;
			Fan( FanNum ).InletAirMassFlowRate = 0.0;

		}

	}

	//cpw22Aug2010 Added Component Model fan algorithm

	void
	SimComponentModelFan( int const FanNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Craig Wray, LBNL
		//       DATE WRITTEN   Feb 2010
		//       MODIFIED       Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the component model fan.

		// METHODOLOGY EMPLOYED:
		// Calculate fan volumetric flow and corresponding fan static pressure rise,
		//    using air-handling system characteristics and Sherman-Wray system curve model
		// Calculate fan air power using volumetric flow and fan static pressure rise
		// Calculate fan wheel efficiency using fan volumetric flow, fan static pressure rise,
		//   fan characteristics, and Wray dimensionless fan static efficiency model
		// Calculate fan shaft power using fan air power and fan static efficiency
		// Calculate fan shaft speed and torque using Wray dimensionless fan airflow model
		// Calculate belt part-load efficiency using correlations and coefficients based on ACEEE data
		// Calculate belt input power using fan shaft power and belt efficiency
		// Calculate motor part-load efficiency using correlations and coefficients based on MotorMaster+ data
		// Calculate motor input power using belt input power and motor efficiency
		// Calculate VFD efficiency using correlations and coefficients based on DOE data
		// Calculate VFD input power using motor input power and VFD efficiency
		// Calculate combined efficiency of fan, belt, motor, and VFD
		// Calculate air temperature rise due to fan (and belt+motor if in airstream) power entering air-handler airflow
		// Calculate output node conditions

		// REFERENCES:
		// TBD

		// Using/Aliasing
		using CurveManager::CurveValue;
		using CurveManager::GetCurveIndex;
		using namespace OutputReportPredefined;
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NVPerfNum;

		Real64 MaxAirMassFlowRate; // Fan Max mass airflow [kg/s]
		//unused062011  REAL(r64) MinFlowFrac        ! Fan Min Volumetric airflow Fraction [-]
		//unused062011  REAL(r64) FlowFrac           ! Fan Volumetric airflow Fraction [-]

		//unused062011  REAL(r64) DeltaPress         ! Delta Pressure Across the Fan (Fan Static Pressure Rise) [N/m2 = Pa]
		//unused062011  REAL(r64) FanAirPower        ! Air power for Fan being Simulated [W]
		//unused062011  REAL(r64) FanSpd             ! Fan shaft rotational speed [rpm]
		//unused062011  REAL(r64) FanTrq             ! Fan shaft torque [N-m]
		//unused062011  REAL(r64) FanWheelEff        ! Fan efficiency (mechanical) [-]
		//unused062011  REAL(r64) FanShaftPower      ! Shaft input power for Fan being Simulated [W]
		//unused062011  REAL(r64) BeltEff            ! Belt efficiency (mechanical) [-]
		//unused062011  REAL(r64) BeltInputPower     ! Belt input power for Fan being Simulated [W]
		//unused062011  REAL(r64) MotEff             ! Fan motor efficiency [-]
		//unused062011  REAL(r64) MotorInputPower    ! Motor input power for Fan being Simulated [W]
		//unused062011  REAL(r64) VFDEff             ! VFD efficiency (electrical) [-]
		//unused062011  REAL(r64) VFDInputPower      ! VFD input power for Fan being Simulated [W]
		//unused062011  REAL(r64) FanEff             ! Fan total system efficiency (fan*belt*motor*VFD) [-]
		Real64 MotInAirFrac; // Fraction of fan power input to airstream

		// Local variables
		Real64 RhoAir; // Air density [kg/m3]
		Real64 MassFlow; // Fan mass airflow [kg/s]
		Real64 FanVolFlow; // Fan volumetric airflow [m3/s]
		Real64 DuctStaticPress; // Duct static pressure setpoint [Pa]
		Real64 DeltaPressTot; // Total pressure rise across fan [N/m2 = Pa]
		Real64 FanOutletVelPress; // Fan outlet velocity pressure [Pa]
		Real64 EulerNum; // Fan Euler number [-]
		Real64 NormalizedEulerNum; // Normalized Fan Euler number [-]
		Real64 FanDimFlow; // Fan dimensionless airflow [-]
		Real64 FanSpdRadS; // Fan shaft rotational speed [rad/s]
		Real64 MotorSpeed; // Motor shaft rotational speed [rpm]
		Real64 FanTrqRatio; // Ratio of fan torque to max fan torque [-]
		Real64 BeltPLEff; // Belt normalized (part-load) efficiency [-]
		Real64 MotorOutPwrRatio; // Ratio of motor output power to max motor output power [-]
		Real64 MotorPLEff; // Motor normalized (part-load) efficiency [-]
		static Real64 VFDSpdRatio( 0.0 ); // Ratio of motor speed to motor max speed [-]
		static Real64 VFDOutPwrRatio( 0.0 ); // Ratio of VFD output power to max VFD output power [-]
		Real64 PowerLossToAir; // Energy input to air stream (W)
		Real64 FanEnthalpyChange; // Air enthalpy change due to fan, belt, and motor losses [kJ/kg]

		// Get inputs for night ventilation option
		NVPerfNum = Fan( FanNum ).NVPerfNum;

		if ( NightVentOn && NVPerfNum > 0 ) {
			MotInAirFrac = NightVentPerf( NVPerfNum ).MotInAirFrac;
			MaxAirMassFlowRate = NightVentPerf( NVPerfNum ).MaxAirMassFlowRate;
		} else {
			MotInAirFrac = Fan( FanNum ).MotInAirFrac;
			MaxAirMassFlowRate = Fan( FanNum ).MaxAirMassFlowRate;
		}

		//  IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue
		//  IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

		// Get air density at standard conditions and get mass airflow through fan
		// From WeatherManager:
		//   StdBaroPress=(101.325d0*(1.0d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
		//   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
		// From PsychRoutines:
		//   w=MAX(dw,1.0d-5)
		//   rhoair = pb/(287.d0*(tdb+KelvinConv)*(1.0d0+1.6077687d0*w))
		RhoAir = Fan( FanNum ).RhoAirStdInit;
		MassFlow = min( Fan( FanNum ).InletAirMassFlowRate, Fan( FanNum ).MaxAirMassFlowRate );

		//  IF (Fan(FanNum)%EMSMaxMassFlowOverrideOn) MassFlow   = Fan(FanNum)%EMSAirMassFlowValue

		//Determine the Fan Schedule for the Time step
		if ( ( GetCurrentScheduleValue( Fan( FanNum ).AvailSchedPtrNum ) > 0.0 || LocalTurnFansOn ) && ! LocalTurnFansOff && MassFlow > 0.0 ) {
			//Fan is operating - calculate fan pressure rise, component efficiencies and power, and also air enthalpy rise

			// Calculate fan static pressure rise using fan volumetric flow, std air density, air-handling system characteristics,
			//   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
			FanVolFlow = MassFlow / RhoAir; //[m3/s at standard conditions]
			DuctStaticPress = CurveValue( Fan( FanNum ).PressResetCurveIndex, FanVolFlow ); //Duct static pressure setpoint [Pa]
			DeltaPressTot = CurveValue( Fan( FanNum ).PressRiseCurveIndex, FanVolFlow, DuctStaticPress ); //Fan total pressure rise [Pa]
			FanOutletVelPress = 0.5 * RhoAir * pow_2( FanVolFlow / Fan( FanNum ).FanOutletArea ); //Fan outlet velocity pressure [Pa]
			//Outlet velocity pressure cannot exceed total pressure rise
			FanOutletVelPress = min( FanOutletVelPress, DeltaPressTot );
			Fan( FanNum ).DeltaPress = DeltaPressTot - FanOutletVelPress; //Fan static pressure rise [Pa]

			//    IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue

			// Calculate fan static air power using volumetric flow and fan static pressure rise
			Fan( FanNum ).FanAirPower = FanVolFlow * Fan( FanNum ).DeltaPress; //[W]

			// Calculate fan wheel efficiency using fan volumetric flow, fan static pressure rise,
			//   fan characteristics, and Wray dimensionless fan static efficiency model
			EulerNum = ( Fan( FanNum ).DeltaPress * pow_4( Fan( FanNum ).FanWheelDia ) ) / ( RhoAir * pow_2( FanVolFlow ) ); //[-]
			NormalizedEulerNum = std::log10( EulerNum / Fan( FanNum ).EuMaxEff );
			if ( NormalizedEulerNum <= 0.0 ) {
				Fan( FanNum ).FanWheelEff = CurveValue( Fan( FanNum ).PLFanEffNormCurveIndex, NormalizedEulerNum );
			} else {
				Fan( FanNum ).FanWheelEff = CurveValue( Fan( FanNum ).PLFanEffStallCurveIndex, NormalizedEulerNum );
			}
			Fan( FanNum ).FanWheelEff *= Fan( FanNum ).FanMaxEff; // [-]
			Fan( FanNum ).FanWheelEff = max( Fan( FanNum ).FanWheelEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors

			// Calculate fan shaft power using fan static air power and fan static efficiency
			Fan( FanNum ).FanShaftPower = Fan( FanNum ).FanAirPower / Fan( FanNum ).FanWheelEff; //[W]

			// Calculate fan shaft speed, fan torque, and motor speed using Wray dimensionless fan airflow model
			if ( NormalizedEulerNum <= 0.0 ) {
				FanDimFlow = CurveValue( Fan( FanNum ).DimFlowNormCurveIndex, NormalizedEulerNum ); //[-]
			} else {
				FanDimFlow = CurveValue( Fan( FanNum ).DimFlowStallCurveIndex, NormalizedEulerNum ); //[-]
			}
			FanSpdRadS = FanVolFlow / ( FanDimFlow * Fan( FanNum ).FanMaxDimFlow * pow_3( Fan( FanNum ).FanWheelDia ) ); //[rad/s]
			Fan( FanNum ).FanTrq = Fan( FanNum ).FanShaftPower / FanSpdRadS; //[N-m]
			Fan( FanNum ).FanSpd = FanSpdRadS * 9.549296586; //[rpm, conversion factor is 30/PI]
			MotorSpeed = Fan( FanNum ).FanSpd * Fan( FanNum ).PulleyDiaRatio; //[rpm]

			// Calculate belt part-load drive efficiency using correlations and coefficients based on ACEEE data
			// Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
			FanTrqRatio = Fan( FanNum ).FanTrq / Fan( FanNum ).BeltMaxTorque; //[-]
			if ( ( FanTrqRatio <= Fan( FanNum ).BeltTorqueTrans ) && ( Fan( FanNum ).PLBeltEffReg1CurveIndex != 0 ) ) {
				BeltPLEff = CurveValue( Fan( FanNum ).PLBeltEffReg1CurveIndex, FanTrqRatio ); //[-]
			} else {
				if ( ( FanTrqRatio > Fan( FanNum ).BeltTorqueTrans ) && ( FanTrqRatio <= 1.0 ) && ( Fan( FanNum ).PLBeltEffReg2CurveIndex != 0 ) ) {
					BeltPLEff = CurveValue( Fan( FanNum ).PLBeltEffReg2CurveIndex, FanTrqRatio ); //[-]
				} else {
					if ( ( FanTrqRatio > 1.0 ) && ( Fan( FanNum ).PLBeltEffReg3CurveIndex != 0 ) ) {
						BeltPLEff = CurveValue( Fan( FanNum ).PLBeltEffReg3CurveIndex, FanTrqRatio ); //[-]
					} else {
						BeltPLEff = 1.0; //Direct drive or no curve specified - use constant efficiency
					}
				}
			}
			Fan( FanNum ).BeltEff = Fan( FanNum ).BeltMaxEff * BeltPLEff; //[-]
			Fan( FanNum ).BeltEff = max( Fan( FanNum ).BeltEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors

			// Calculate belt input power using fan shaft power and belt efficiency
			Fan( FanNum ).BeltInputPower = Fan( FanNum ).FanShaftPower / Fan( FanNum ).BeltEff; //[W]

			// Calculate motor part-load efficiency using correlations and coefficients based on MotorMaster+ data
			MotorOutPwrRatio = Fan( FanNum ).BeltInputPower / Fan( FanNum ).MotorMaxOutPwr; //[-]
			if ( Fan( FanNum ).PLMotorEffCurveIndex != 0 ) {
				MotorPLEff = CurveValue( Fan( FanNum ).PLMotorEffCurveIndex, MotorOutPwrRatio ); //[-]
			} else {
				MotorPLEff = 1.0; //No curve specified - use constant efficiency
			}
			Fan( FanNum ).MotEff = Fan( FanNum ).MotorMaxEff * MotorPLEff; //[-]
			Fan( FanNum ).MotEff = max( Fan( FanNum ).MotEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors

			// Calculate motor input power using belt input power and motor efficiency
			Fan( FanNum ).MotorInputPower = Fan( FanNum ).BeltInputPower / Fan( FanNum ).MotEff; //[W]

			// Calculate VFD efficiency using correlations and coefficients based on VFD type
			if ( ( Fan( FanNum ).VFDEffType == "SPEED" ) && ( Fan( FanNum ).VFDEffCurveIndex != 0 ) ) {
				VFDSpdRatio = MotorSpeed / Fan( FanNum ).MotorMaxSpd; //[-]
				Fan( FanNum ).VFDEff = CurveValue( Fan( FanNum ).VFDEffCurveIndex, VFDSpdRatio ); //[-]
			} else {
				if ( ( Fan( FanNum ).VFDEffType == "POWER" ) && ( Fan( FanNum ).VFDEffCurveIndex != 0 ) ) {
					VFDOutPwrRatio = Fan( FanNum ).MotorInputPower / Fan( FanNum ).VFDMaxOutPwr; //[-]
					Fan( FanNum ).VFDEff = CurveValue( Fan( FanNum ).VFDEffCurveIndex, VFDOutPwrRatio ); //[-]
				} else {
					// No curve specified - use constant efficiency
					Fan( FanNum ).VFDMaxOutPwr = 0.0;
					Fan( FanNum ).VFDEff = 0.97;
				}
			}
			Fan( FanNum ).VFDEff = max( Fan( FanNum ).VFDEff, 0.01 ); //Minimum efficiency is 1% to avoid numerical errors

			// Calculate VFD input power using motor input power and VFD efficiency
			Fan( FanNum ).VFDInputPower = Fan( FanNum ).MotorInputPower / Fan( FanNum ).VFDEff; //[W]
			Fan( FanNum ).FanPower = Fan( FanNum ).VFDInputPower; //[W]

			// Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
			// Equivalent to Fan(FanNum)%FanAirPower / Fan(FanNum)%FanPower
			Fan( FanNum ).FanEff = Fan( FanNum ).FanWheelEff * Fan( FanNum ).BeltEff * Fan( FanNum ).MotEff * Fan( FanNum ).VFDEff;

			//    IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

			// Calculate air enthalpy and temperature rise from power entering air stream from fan wheel, belt, and motor
			// Assumes MotInAirFrac applies to belt and motor but NOT to VFD
			PowerLossToAir = Fan( FanNum ).FanShaftPower + ( Fan( FanNum ).MotorInputPower - Fan( FanNum ).FanShaftPower ) * Fan( FanNum ).MotInAirFrac; //[W]
			FanEnthalpyChange = PowerLossToAir / MassFlow; //[kJ/kg]
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy + FanEnthalpyChange; //[kJ/kg]

			// This fan does not change the moisture or mass flow across the component
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat; //[-]
			Fan( FanNum ).OutletAirMassFlowRate = MassFlow; //[kg/s]
			Fan( FanNum ).OutletAirTemp = PsyTdbFnHW( Fan( FanNum ).OutletAirEnthalpy, Fan( FanNum ).OutletAirHumRat );

			//cpw31Aug2010 Temporary code for debugging fan component model
			//    WRITE(300,*) TRIM(RoundSigDigits(Fan(FanNum)%RhoAirStdInit,4))//','//TRIM(RoundSigDigits(FanVolFlow,4)) &
			//    //','//TRIM(RoundSigDigits(FanOutletVelPress,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%DeltaPress,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanAirPower,4))//','//TRIM(RoundSigDigits(EulerNum,4)) &
			//    //','//TRIM(RoundSigDigits(NormalizedEulerNum,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanWheelEff,4))
			//    WRITE(301,*) TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPower,4))//','//TRIM(RoundSigDigits(FanDimFlow,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanTrq,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanSpd,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPwrMax,4))//','//TRIM(" ") &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltMaxEff,4))//','//TRIM(RoundSigDigits(FanTrqRatio,4))
			//    WRITE(302,*) TRIM(RoundSigDigits(BeltPLEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%BeltEff,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltInputPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxOutPwr,4)) &
			//    //','//TRIM(" ")//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxEff,4)) &
			//    //','//TRIM(RoundSigDigits(MotorOutPwrRatio,4))//','//TRIM(RoundSigDigits(MotorPLEff,4))
			//    WRITE(303,*) TRIM(RoundSigDigits(Fan(FanNum)%MotEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorInputPower,4)) &
			//    //','//TRIM(RoundSigDigits(VFDOutPwrRatio,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%VFDEff,4)) &
			//    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanEff,4)) &
			//    //','//TRIM(RoundSigDigits(PowerLossToAir,4))//','//TRIM(RoundSigDigits(FanEnthalpyChange,4))
			//    WRITE(304,*) TRIM(CurMnDy)//','//TRIM(CreateSysTimeIntervalString())

		} else {
			//Fan is OFF and not operating -- no power consumed and zero mass flow rate
			Fan( FanNum ).FanPower = 0.0;
			Fan( FanNum ).FanShaftPower = 0.0;
			PowerLossToAir = 0.0;
			Fan( FanNum ).OutletAirMassFlowRate = 0.0;
			Fan( FanNum ).OutletAirHumRat = Fan( FanNum ).InletAirHumRat;
			Fan( FanNum ).OutletAirEnthalpy = Fan( FanNum ).InletAirEnthalpy;
			Fan( FanNum ).OutletAirTemp = Fan( FanNum ).InletAirTemp;
			// Set the Control Flow variables to 0.0 flow when OFF.
			Fan( FanNum ).MassFlowRateMaxAvail = 0.0;
			Fan( FanNum ).MassFlowRateMinAvail = 0.0;

			Fan( FanNum ).DeltaPress = 0.0;
			Fan( FanNum ).FanAirPower = 0.0;
			Fan( FanNum ).FanWheelEff = 0.0;
			Fan( FanNum ).FanSpd = 0.0;
			Fan( FanNum ).FanTrq = 0.0;
			Fan( FanNum ).BeltEff = 0.0;
			Fan( FanNum ).BeltInputPower = 0.0;
			Fan( FanNum ).MotEff = 0.0;
			Fan( FanNum ).MotorInputPower = 0.0;
			Fan( FanNum ).VFDEff = 0.0;
			Fan( FanNum ).VFDInputPower = 0.0;
			Fan( FanNum ).FanEff = 0.0;
		}

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Fan Module
	// *****************************************************************************

	void
	UpdateFan( int const FanNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   April 1998
		//       MODIFIED       L. Gu, Feb. 1, 2007, No unbalance airflow when Zone Exhaust Fans are used in the AirflowNetwork
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the fan outlet nodes.

		// METHODOLOGY EMPLOYED:
		// Data is moved from the fan data structure to the fan outlet nodes.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataAirflowNetwork::AirflowNetworkNumOfExhFan;
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode;
		int InletNode;

		OutletNode = Fan( FanNum ).OutletNodeNum;
		InletNode = Fan( FanNum ).InletNodeNum;

		// Set the outlet air nodes of the fan
		Node( OutletNode ).MassFlowRate = Fan( FanNum ).OutletAirMassFlowRate;
		Node( OutletNode ).Temp = Fan( FanNum ).OutletAirTemp;
		Node( OutletNode ).HumRat = Fan( FanNum ).OutletAirHumRat;
		Node( OutletNode ).Enthalpy = Fan( FanNum ).OutletAirEnthalpy;
		// Set the outlet nodes for properties that just pass through & not used
		Node( OutletNode ).Quality = Node( InletNode ).Quality;
		Node( OutletNode ).Press = Node( InletNode ).Press;

		// Set the Node Flow Control Variables from the Fan Control Variables
		Node( OutletNode ).MassFlowRateMaxAvail = Fan( FanNum ).MassFlowRateMaxAvail;
		Node( OutletNode ).MassFlowRateMinAvail = Fan( FanNum ).MassFlowRateMinAvail;

		if ( Fan( FanNum ).FanType_Num == FanType_ZoneExhaust ) {
			Node( InletNode ).MassFlowRate = Fan( FanNum ).InletAirMassFlowRate;
			if ( AirflowNetworkNumOfExhFan == 0 ) {
				UnbalExhMassFlow = Fan( FanNum ).InletAirMassFlowRate;
				if ( Fan( FanNum ).BalancedFractSchedNum > 0 ) {
					BalancedExhMassFlow = UnbalExhMassFlow * GetCurrentScheduleValue( Fan( FanNum ).BalancedFractSchedNum );
					UnbalExhMassFlow = UnbalExhMassFlow - BalancedExhMassFlow;
				} else {
					BalancedExhMassFlow = 0.0;
				}
			} else {
				UnbalExhMassFlow = 0.0;
				BalancedExhMassFlow = 0.0;
			}
			Fan( FanNum ).UnbalancedOutletMassFlowRate = UnbalExhMassFlow;
			Fan( FanNum ).BalancedOutletMassFlowRate = BalancedExhMassFlow;
		}

		if ( Contaminant.CO2Simulation ) {
			Node( OutletNode ).CO2 = Node( InletNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( OutletNode ).GenContam = Node( InletNode ).GenContam;
		}

	}

	//        End of Update subroutines for the Fan Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Fan Module
	// *****************************************************************************

	void
	ReportFan( int const FanNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   April 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variables for the fans.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::FanElecPower;
		using DataAirLoop::LoopOnOffFanRTF;
		using DataGlobals::SecInHour;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		Fan( FanNum ).FanEnergy = Fan( FanNum ).FanPower * TimeStepSys * SecInHour;
		Fan( FanNum ).DeltaTemp = Fan( FanNum ).OutletAirTemp - Fan( FanNum ).InletAirTemp;
		FanElecPower = Fan( FanNum ).FanPower;

		if ( Fan( FanNum ).FanType_Num == FanType_SimpleOnOff ) {
			LoopOnOffFanRTF = Fan( FanNum ).FanRuntimeFraction;
		}

	}

	//        End of Reporting subroutines for the Fan Module
	// *****************************************************************************

	// Beginning of Utility subroutines for the Fan Module
	// *****************************************************************************

	void
	GetFanIndex(
		std::string const & FanName,
		int & FanIndex,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given fan -- issues error message if that fan
		// is not legal fan.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		FanIndex = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
		if ( FanIndex == 0 ) {
			if ( present( ThisObjectType ) ) {
				ShowSevereError( ThisObjectType() + ", GetFanIndex: Fan not found=" + FanName );
			} else {
				ShowSevereError( "GetFanIndex: Fan not found=" + FanName );
			}
			ErrorsFound = true;
		}

	}

	void
	GetFanVolFlow(
		int const FanIndex,
		Real64 & FanVolFlow
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the fan volumetric flow for use by zone equipment (e.g. Packaged Terminal Heat Pump)
		// Zone equipment must ensure that a properly sized fan is used to meet the maximum supply air flow rate

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( FanIndex == 0 ) {
			FanVolFlow = 0.0;
		} else {
			FanVolFlow = Fan( FanIndex ).MaxAirFlowRate;
		}

	}

	void
	GetFanPower(
		int const FanIndex,
		Real64 & FanPower
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the fan power draw

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( FanIndex == 0 ) {
			FanPower = 0.0;
		} else {
			FanPower = Fan( FanIndex ).FanPower;
		}

	}

	void
	GetFanType(
		std::string const & FanName, // Fan name
		int & FanType, // returned fantype number
		bool & ErrorsFound, // error indicator
		Optional_string_const ThisObjectType, // parent object type (for error message)
		Optional_string_const ThisObjectName // parent object name (for error message)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an integer type for a given fan -- issues error message if that fan
		// is not a legal fan.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanIndex;

		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		FanIndex = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
		if ( FanIndex == 0 ) {
			if ( present( ThisObjectType ) && present( ThisObjectName ) ) {
				ShowSevereError( "GetFanType: " + ThisObjectType() + "=\"" + ThisObjectName() + "\", invalid Fan specified=\"" + FanName + "\"." );
			} else if ( present( ThisObjectType ) ) {
				ShowSevereError( ThisObjectType() + ", GetFanType: Fan not found=" + FanName );
			} else {
				ShowSevereError( "GetFanType: Fan not found=" + FanName );
			}
			FanType = 0;
			ErrorsFound = true;
		} else {
			FanType = Fan( FanIndex ).FanType_Num;
		}

	}

	Real64
	GetFanDesignVolumeFlowRate(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound, // set to true if problem
		Optional_int_const FanIndex // index to fan
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       R. Raustad, Aug 2007 - added optional fan index
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the design volume flow rate for the given fan and returns it.  If
		// incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Real64 DesignVolumeFlowRate; // returned flow rate of matched fan

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichFan;

		// Obtains and Allocates fan related parameters from input file
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		if ( present( FanIndex ) ) {
			DesignVolumeFlowRate = Fan( FanIndex ).MaxAirFlowRate;
		} else {
			WhichFan = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
			if ( WhichFan != 0 ) {
				DesignVolumeFlowRate = Fan( WhichFan ).MaxAirFlowRate;
			} else {
				ShowSevereError( "GetFanDesignVolumeFlowRate: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"" );
				ShowContinueError( "... Design Volume Flow rate returned as -1000." );
				ErrorsFound = true;
				DesignVolumeFlowRate = -1000.0;
			}
		}

		return DesignVolumeFlowRate;

	}

	int
	GetFanInletNode(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given fan and returns the inlet node.  If
		// incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned outlet node of matched fan

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichFan;

		// Obtains and Allocates fan related parameters from input file
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		WhichFan = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
		if ( WhichFan != 0 ) {
			NodeNumber = Fan( WhichFan ).InletNodeNum;
		} else {
			ShowSevereError( "GetFanInletNode: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetFanOutletNode(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given fan and returns the outlet node.  If
		// incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned outlet node of matched fan

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichFan;

		// Obtains and Allocates fan related parameters from input file
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		WhichFan = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
		if ( WhichFan != 0 ) {
			NodeNumber = Fan( WhichFan ).OutletNodeNum;
		} else {
			ShowSevereError( "GetFanOutletNode: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetFanAvailSchPtr(
		std::string const & FanType, // must match fan types in this module
		std::string const & FanName, // must match fan names for the fan type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   September 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given fan and returns the availability schedule pointer.  If
		// incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int FanAvailSchPtr; // returned availability schedule pointer of matched fan

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichFan;

		// Obtains and Allocates fan related parameters from input file
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		WhichFan = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
		if ( WhichFan != 0 ) {
			FanAvailSchPtr = Fan( WhichFan ).AvailSchedPtrNum;
		} else {
			ShowSevereError( "GetFanAvailSchPtr: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"" );
			ErrorsFound = true;
			FanAvailSchPtr = 0;
		}

		return FanAvailSchPtr;

	}

	int
	GetFanSpeedRatioCurveIndex(
		std::string & FanType, // must match fan types in this module (set if nonzero index passed)
		std::string & FanName, // must match fan names for the fan type (set if nonzero index passed)
		Optional_int IndexIn // optional fan index if fan type and name are unknown or index needs setting
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   September 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given fan and returns the fan speed curve pointer.  If
		// incorrect fan type or name is given, ErrorsFound is returned as true and value is returned
		// as zero. If optional index argument is passed along with fan type and name, the index is set.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int FanSpeedRatioCurveIndex; // index to fan speed ratio curve object

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichFan;

		// Obtains and Allocates fan related parameters from input file
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		if ( present( IndexIn ) ) {
			if ( IndexIn > 0 ) {
				WhichFan = IndexIn;
				FanType = Fan( WhichFan ).FanType;
				FanName = Fan( WhichFan ).FanName;
			} else {
				WhichFan = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
				IndexIn = WhichFan;
			}
		} else {
			WhichFan = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
		}

		if ( WhichFan != 0 ) {
			FanSpeedRatioCurveIndex = Fan( WhichFan ).FanPowerRatAtSpeedRatCurveIndex;
		} else {
			ShowSevereError( "GetFanSpeedRatioCurveIndex: Could not find Fan, Type=\"" + FanType + "\" Name=\"" + FanName + "\"" );
			FanSpeedRatioCurveIndex = 0;
		}

		return FanSpeedRatioCurveIndex;

	}

	void
	SetFanData(
		int const FanNum, // Index of fan
		bool & ErrorsFound, // Set to true if certain errors found
		std::string const & FanName, // Name of fan
		Optional< Real64 const > MaxAirVolFlow, // Fan air volumetric flow rate    [m3/s]
		Optional< Real64 const > MinAirVolFlow // Fan air volumetric flow rate    [m3/s]
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
		int WhichFan; // index to generic HX

		// Obtains and Allocates fan related parameters from input file
		if ( GetFanInputFlag ) { //First time subroutine has been entered
			GetFanInput();
			GetFanInputFlag = false;
		}

		if ( FanNum == 0 ) {
			WhichFan = FindItemInList( FanName, Fan, &FanEquipConditions::FanName );
		} else {
			WhichFan = FanNum;
		}

		if ( WhichFan <= 0 || WhichFan > NumFans ) {
			ShowSevereError( "SetFanData: Could not find fan = \"" + FanName + "\"" );
			ErrorsFound = true;
			return;
		}

		if ( present( MaxAirVolFlow ) ) {
			Fan( WhichFan ).MaxAirFlowRate = MaxAirVolFlow;
		}

		if ( present( MinAirVolFlow ) ) {
			Fan( WhichFan ).MinAirFlowRate = MinAirVolFlow;
		}

	}

	Real64
	FanDesDT(
		int const FanNum, // index of fan in Fan array
		Real64 const EP_UNUSED( FanVolFlow ) // fan volumetric flow rate [m3/s]
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2014
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates and returns the design fan delta T from the fan input data

		// METHODOLOGY EMPLOYED:
		// Simple fan:  Qdot,tot = (Vdot*deltaP)/Eff,tot
		//              Qdot,air = Eff,mot*Qdot,tot + (Qdot,tot - Eff,mot*Qdot,tot)*Frac,mot-in-airstream
		//              Qdot,air = cp,air*rho,air*Vdot*deltaT

		// REFERENCES: EnergyPlus Engineering Reference

		// Using/Aliasing

		// Return value
		Real64 DesignDeltaT; // returned delta T of matched fan [delta deg C]

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 RhoAir; // density of air [kg/m3]
		Real64 CpAir;  // specific heat of air [J/kg-K]
		Real64 DeltaP; // fan design pressure rise [N/m2]
		Real64 TotEff; // fan design total efficiency
		Real64 MotEff; // fan design motor efficiency
		Real64 MotInAirFrac; // fraction of motor in the air stream
		//
		if ( FanNum == 0 ) {
			DesignDeltaT = 0.0;
		} else if ( Fan( FanNum ).FanType_Num != FanType_ComponentModel ) {
			DeltaP = Fan( FanNum ).DeltaPress;
			TotEff = Fan( FanNum ).FanEff;
			MotEff = Fan( FanNum ).MotEff;
			MotInAirFrac = Fan( FanNum ).MotInAirFrac;
			RhoAir = StdRhoAir;
			CpAir = PsyCpAirFnWTdb( constant_zero, constant_twenty );
			DesignDeltaT = ( DeltaP / ( RhoAir * CpAir * TotEff ) ) * ( MotEff + MotInAirFrac * ( 1.0 - MotEff ) );
		} else {
			DesignDeltaT = 0.0;
		}

		return DesignDeltaT;

	} // FanDesDT

	Real64
	CalFaultyFanAirFlowReduction(
		std::string const & FanName,          // name of the fan
		Real64 const FanDesignAirFlowRate,    // Fan Design Volume Flow Rate [m3/sec]
		Real64 const FanDesignDeltaPress,     // Fan Design Delta Pressure [Pa]
		Real64 const FanFaultyDeltaPressInc,  // Increase of Fan Delta Pressure in the Faulty Case [Pa]
		int const FanCurvePtr                 // Fan Curve Index
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rongpeng Zhang
		//       DATE WRITTEN   Apr. 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the decrease of the fan air flow rate, given the fan curve
		// and the increase of fan pressure rise due to fouling air filters

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace CurveManager;
		using FaultsManager::CheckFaultyAirFilterFanCurve;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FanFaultyAirFlowRate;  // Fan Volume Flow Rate in the Faulty Case [m3/sec]
		Real64 FanCalDeltaPress;      // Calculated Fan Delta Pressure for temp use [Pa]
		Real64 FanCalDeltaPresstemp;  // Calculated Fan Delta Pressure for temp use [Pa]

		// FLOW

		// Check whether the fan curve covers the design operational point of the fan
		FanCalDeltaPress = CurveValue( FanCurvePtr, FanDesignAirFlowRate );
		if ( ( FanCalDeltaPress < 0.9 * FanDesignDeltaPress ) || ( FanCalDeltaPress > 1.1 * FanDesignDeltaPress ) ) {
			ShowWarningError( "The design operatinal point of the fan " + FanName + " does not fall " );
			ShowContinueError( "on the fan curve provided in the FaultModel:Fouling:AirFilter object. " );
			return 0.0;
		}

		// Calculate the Fan Volume Flow Rate in the Faulty Case
		FanFaultyAirFlowRate = FanDesignAirFlowRate;
		FanCalDeltaPresstemp = CurveValue( FanCurvePtr, FanFaultyAirFlowRate );
		FanCalDeltaPress = FanCalDeltaPresstemp;

		while ( FanCalDeltaPress < ( FanDesignDeltaPress + FanFaultyDeltaPressInc ) ) {
			FanFaultyAirFlowRate = FanFaultyAirFlowRate - 0.005;
			FanCalDeltaPresstemp = CurveValue( FanCurvePtr, FanFaultyAirFlowRate );

			if ( ( FanCalDeltaPresstemp <= FanCalDeltaPress ) || ( FanFaultyAirFlowRate <= PerfCurve( FanCurvePtr ).Var1Min ) ) {
			// The new operatinal point of the fan go beyond the fan selection range
				ShowWarningError( "The operatinal point of the fan " + FanName + " may go beyond the fan selection " );
				ShowContinueError( "range in the faulty fouling air filter cases" );
				break;
			}

			FanCalDeltaPress = FanCalDeltaPresstemp;
		}

		return FanDesignAirFlowRate - FanFaultyAirFlowRate;
	}

	Real64
	FanDesHeatGain(
		int const FanNum, // index of fan in Fan array
		Real64 const FanVolFlow // fan volumetric flow rate [m3/s]
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2014
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates and returns the design fan heat gain from the fan input data

		// METHODOLOGY EMPLOYED:
		// Simple fan:  Qdot,tot = (Vdot*deltaP)/Eff,tot
		//              Qdot,air = Eff,mot*Qdot,tot + (Qdot,tot - Eff,mot*Qdot,tot)*Frac,mot-in-airstream

		// REFERENCES: EnergyPlus Engineering Reference

		// Using/Aliasing
		using DataSizing::CurSysNum;
		using DataAirLoop::AirLoopControlInfo;

		// Return value
		Real64 DesignHeatGain; // returned heat gain of matched fan [W]

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaP; // fan design pressure rise [N/m2]
		Real64 TotEff; // fan design total efficiency
		Real64 MotEff; // fan design motor efficiency
		Real64 MotInAirFrac; // fraction of motor in the air stream
		Real64 FanPowerTot; // total fan power consumption [W]
		//
		if ( FanNum == 0 ) {
			DesignHeatGain = 0.0;
		} else if ( Fan( FanNum ).FanType_Num != FanType_ComponentModel ) {
			DeltaP = Fan( FanNum ).DeltaPress;
			TotEff = Fan( FanNum ).FanEff;
			MotEff = Fan( FanNum ).MotEff;
			MotInAirFrac = Fan( FanNum ).MotInAirFrac;
			FanPowerTot = ( FanVolFlow * DeltaP ) / TotEff;
			DesignHeatGain = MotEff*FanPowerTot + ( FanPowerTot - MotEff * FanPowerTot ) * MotInAirFrac;
		} else {
			if ( !SysSizingCalc && MySizeFlag( FanNum ) ) {
				SizeFan( FanNum );
				MySizeFlag( FanNum ) = false;
			}
			DesignHeatGain = Fan( FanNum ).FanShaftPower + ( Fan( FanNum ).MotorInputPower - Fan( FanNum ).FanShaftPower ) * Fan( FanNum ).MotInAirFrac;
		}

		return DesignHeatGain;

	} // FanDesHeatGain

	// Clears the global data in Fans.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumFans = 0;
		NumNightVentPerf = 0;
		GetFanInputFlag = true;
		LocalTurnFansOn = false;
		LocalTurnFansOff = false;
		MyOneTimeFlag = true;
		ZoneEquipmentListChecked = false;

		CheckEquipName.deallocate();
		MySizeFlag.deallocate();
		MyEnvrnFlag.deallocate();
		Fan.deallocate();
		NightVentPerf.deallocate();
		FanNumericFields.deallocate();

	}

	// End of Utility subroutines for the Fan Module
	// *****************************************************************************

} // Fans

} // EnergyPlus
