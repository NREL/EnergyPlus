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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <SteamCoils.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SteamCoils {

	// Module containing the SteamCoil simulation routines

	// MODULE INFORMATION:
	//   AUTHOR         Rahul Chillar
	//   DATE WRITTEN   Jan 2005
	//   MODIFIED       na
	//   RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the SteamCoil System Component.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using namespace Psychrometrics;
	using namespace FluidProperties;
	using DataEnvironment::StdBaroPress;
	using DataPlant::TypeOf_CoilSteamAirHeating;
	using DataPlant::ScanPlantLoopsForObject;
	using DataPlant::PlantLoop;
	using DataPlant::MyPlantSizingIndex;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const SteamCoil_AirHeating( 2 );
	int const TemperatureSetPointControl( 1 );
	int const ZoneLoadControl( 3 );
	static std::string const fluidNameSteam( "STEAM" );
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE DEFINITIONS
	// MODULE VARIABLE DECLARATIONS:
	int SteamIndex( 0 );
	int NumSteamCoils( 0 ); // The Number of SteamCoils found in the Input
	Array1D_bool MySizeFlag;
	Array1D_bool CoilWarningOnceFlag;
	Array1D_bool CheckEquipName;
	bool GetSteamCoilsInputFlag( true ); // Flag set to make sure you get input once

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Utility routines for module

	// Object Data
	Array1D< SteamCoilEquipConditions > SteamCoil;

	// MODULE SUBROUTINES:

	// Functions

	void
	SimulateSteamCoilComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex,
		Optional< Real64 const > QCoilReq, // coil load to be met
		Optional< Real64 > QCoilActual, // coil load actually delivered returned to calling component
		Optional_int_const FanOpMode,
		Optional< Real64 const > PartLoadRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//   AUTHOR         Rahul Chillar
		//   DATE WRITTEN   Jan 2005
		//   MODIFIED       na
		//   RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages SteamCoil component simulation.

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
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QCoilActualTemp; // coil load actually delivered returned to calling component
		int CoilNum; // The SteamCoil that you are currently loading input into
		int OpMode; // fan operating mode
		Real64 PartLoadFrac; // part-load fraction of heating coil

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		// Find the correct SteamCoilNumber with the Coil Name
		if ( CompIndex == 0 ) {
			CoilNum = FindItemInList( CompName, SteamCoil );
			if ( CoilNum == 0 ) {
				ShowFatalError( "SimulateSteamCoilComponents: Coil not found=" + CompName );
			}
			CompIndex = CoilNum;
		} else {
			CoilNum = CompIndex;
			if ( CoilNum > NumSteamCoils || CoilNum < 1 ) {
				ShowFatalError( "SimulateSteamCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Number of Steam Coils=" + TrimSigDigits( NumSteamCoils ) + ", Coil name=" + CompName );
			}
			if ( CheckEquipName( CoilNum ) ) {
				if ( CompName != SteamCoil( CoilNum ).Name ) {
					ShowFatalError( "SimulateSteamCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + SteamCoil( CoilNum ).Name );
				}
				CheckEquipName( CoilNum ) = false;
			}
		}

		// With the correct CoilNum Initialize
		InitSteamCoil( CoilNum, FirstHVACIteration ); // Initialize all SteamCoil related parameters

		if ( present( FanOpMode ) ) {
			OpMode = FanOpMode;
		} else {
			OpMode = ContFanCycCoil;
		}
		if ( present( PartLoadRatio ) ) {
			PartLoadFrac = PartLoadRatio;
		} else {
			PartLoadFrac = 1.0;
		}

		if ( SteamCoil( CoilNum ).SteamCoilType_Num == SteamCoil_AirHeating ) {
			CalcSteamAirCoil( CoilNum, QCoilReq, QCoilActualTemp, OpMode, PartLoadFrac ); //Autodesk:OPTIONAL QCoilReq used without PRESENT check
			if ( present( QCoilActual ) ) QCoilActual = QCoilActualTemp;
		}

		// Update the current SteamCoil to the outlet nodes
		UpdateSteamCoil( CoilNum );

		// Report the current SteamCoil
		ReportSteamCoil( CoilNum );

	}

	// Get Input Section of the Module

	void
	GetSteamCoilInput()
	{
		// SUBROUTINE INFORMATION:
		//   AUTHOR         Rahul Chillar
		//   DATE WRITTEN   Jan 2005
		//   MODIFIED       na
		//   RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for coils and stores it in coil data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::FindRefrigerant;
		using GlobalNames::VerifyUniqueCoilName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetSteamCoilInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoilNum; // The SteamCoil that you are currently loading input into
		int NumStmHeat;
		int StmHeatNum;
		int NumAlphas;
		int NumNums;
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file
		bool errFlag;

		CurrentModuleObject = "Coil:Heating:Steam";
		NumStmHeat = GetNumObjectsFound( CurrentModuleObject );
		NumSteamCoils = NumStmHeat;
		if ( NumSteamCoils > 0 ) {
			SteamCoil.allocate( NumSteamCoils );
			CheckEquipName.dimension( NumSteamCoils, true );
		}

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		AlphArray.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNums );
		NumArray.dimension( NumNums, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNums, true );

		// Get the data for steam heating coils
		for ( StmHeatNum = 1; StmHeatNum <= NumStmHeat; ++StmHeatNum ) {

			CoilNum = StmHeatNum;

			GetObjectItem( CurrentModuleObject, StmHeatNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SteamCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			SteamCoil( CoilNum ).Name = AlphArray( 1 );
			SteamCoil( CoilNum ).Schedule = AlphArray( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				SteamCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				SteamCoil( CoilNum ).SchedPtr = GetScheduleIndex( AlphArray( 2 ) );
				if ( SteamCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\", invalid data." );
					ShowContinueError( cAlphaFields( 2 ) + " not found=" + AlphArray( 2 ) );
					ErrorsFound = true;
				}
			}

			SteamCoil( CoilNum ).SteamCoilTypeA = "Heating";
			SteamCoil( CoilNum ).SteamCoilType_Num = SteamCoil_AirHeating;
			SteamCoil( CoilNum ).Coil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
			SteamCoil( CoilNum ).MaxSteamVolFlowRate = NumArray( 1 );
			SteamCoil( CoilNum ).DegOfSubcooling = NumArray( 2 );
			SteamCoil( CoilNum ).LoopSubcoolReturn = NumArray( 3 );

			SteamCoil( CoilNum ).SteamInletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Steam, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			SteamCoil( CoilNum ).SteamOutletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Steam, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			SteamCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SteamCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			{ auto const SELECT_CASE_var( MakeUPPERCase( AlphArray( 7 ) ) );
			//TEMPERATURE SETPOINT CONTROL or ZONE LOAD CONTROLLED Coils
			if ( SELECT_CASE_var == "TEMPERATURESETPOINTCONTROL" ) {
				SteamCoil( CoilNum ).TypeOfCoil = TemperatureSetPointControl;
				SteamCoil( CoilNum ).TempSetPointNodeNum = GetOnlySingleNode( AlphArray( 8 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
				if ( SteamCoil( CoilNum ).TempSetPointNodeNum == 0 ) {
					ShowSevereError( RoutineName + cAlphaFields( 8 ) + " not found for " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					ShowContinueError( "..required for Temperature Setpoint Controlled Coils." );
					ErrorsFound = true;
				}

			} else if ( SELECT_CASE_var == "ZONELOADCONTROL" ) {
				SteamCoil( CoilNum ).TypeOfCoil = ZoneLoadControl;

				if ( ! lAlphaBlanks( 8 ) ) {
					ShowWarningError( RoutineName + "ZoneLoad Controlled Coil, so " + cAlphaFields( 8 ) + " not needed" );
					ShowContinueError( "for " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					SteamCoil( CoilNum ).TempSetPointNodeNum = 0;
				}

			} else {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 7 ) + " [" + AlphArray( 7 ) + "] specified for " + CurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}}

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 3 ), AlphArray( 4 ), "Steam Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 5 ), AlphArray( 6 ), "Air Nodes" );

			if ( SteamIndex == 0 && CoilNum == 1 ) {
				SteamIndex = FindRefrigerant( "Steam" );
				if ( SteamIndex == 0 ) {
					ShowSevereError( RoutineName + "Steam Properties for " + AlphArray( 1 ) + " not found." );
					ShowContinueError( "Steam Fluid Properties should have been included in the input file." );
					ErrorsFound = true;
				}
			}

			SteamCoil( CoilNum ).FluidIndex = SteamIndex;
		}

		for ( CoilNum = 1; CoilNum <= NumStmHeat; ++CoilNum ) {

			//Setup the Simple Heating Coil reporting variables
			//CurrentModuleObject = "Coil:Heating:Steam"
			SetupOutputVariable( "Heating Coil Heating Energy [J]", SteamCoil( CoilNum ).TotSteamHeatingCoilEnergy, "System", "Sum", SteamCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Heating Rate [W]", SteamCoil( CoilNum ).TotSteamHeatingCoilRate, "System", "Average", SteamCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Steam Mass Flow Rate [Kg/s]", SteamCoil( CoilNum ).OutletSteamMassFlowRate, "System", "Average", SteamCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Steam Inlet Temperature [C]", SteamCoil( CoilNum ).InletSteamTemp, "System", "Average", SteamCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Steam Outlet Temperature [C]", SteamCoil( CoilNum ).OutletSteamTemp, "System", "Average", SteamCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Steam Trap Loss Rate [W]", SteamCoil( CoilNum ).LoopLoss, "System", "Average", SteamCoil( CoilNum ).Name );

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input." );
		}

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		NumArray.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	// End of Get Input subroutines for the HB Module

	// Beginning Initialization Section of the Module

	void
	InitSteamCoil(
		int const CoilNum,
		bool const FirstHVACIteration
	)
	{
		// SUBROUTINE INFORMATION:
		//   AUTHOR         Rahul Chillar
		//   DATE WRITTEN   Jan 2005
		//   MODIFIED       na
		//   RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the SteamCoil Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetSatEnthalpyRefrig;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitSteamCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirInletNode;
		int SteamInletNode;
		int ControlNode;
		int AirOutletNode;
		Real64 SteamDensity;
		Real64 StartEnthSteam;
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag;
		bool errFlag;

		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumSteamCoils );
			MySizeFlag.allocate( NumSteamCoils );
			CoilWarningOnceFlag.allocate( NumSteamCoils );
			MyPlantScanFlag.allocate( NumSteamCoils );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			CoilWarningOnceFlag = true;
			MyPlantScanFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyPlantScanFlag( CoilNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( SteamCoil( CoilNum ).Name, SteamCoil( CoilNum ).Coil_PlantTypeNum, SteamCoil( CoilNum ).LoopNum, SteamCoil( CoilNum ).LoopSide, SteamCoil( CoilNum ).BranchNum, SteamCoil( CoilNum ).CompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitSteamCoil: Program terminated for previous conditions." );
			}
			MyPlantScanFlag( CoilNum ) = false;
		}

		if ( ! SysSizingCalc && MySizeFlag( CoilNum ) ) {
			// for each coil, do the sizing once.
			SizeSteamCoil( CoilNum );
			MySizeFlag( CoilNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( CoilNum ) ) {
			//Initialize all report variables to a known state at beginning of simulation
			SteamCoil( CoilNum ).TotSteamHeatingCoilEnergy = 0.0;
			SteamCoil( CoilNum ).TotSteamCoolingCoilEnergy = 0.0;
			SteamCoil( CoilNum ).SenSteamCoolingCoilEnergy = 0.0;
			SteamCoil( CoilNum ).TotSteamHeatingCoilRate = 0.0;
			SteamCoil( CoilNum ).TotSteamCoolingCoilRate = 0.0;
			SteamCoil( CoilNum ).SenSteamCoolingCoilRate = 0.0;
			// Initialize other module level variables
			SteamCoil( CoilNum ).InletAirMassFlowRate = 0.0;
			SteamCoil( CoilNum ).OutletAirMassFlowRate = 0.0;
			SteamCoil( CoilNum ).InletAirTemp = 0.0;
			SteamCoil( CoilNum ).OutletAirTemp = 0.0;
			SteamCoil( CoilNum ).InletAirHumRat = 0.0;
			SteamCoil( CoilNum ).OutletAirHumRat = 0.0;
			SteamCoil( CoilNum ).InletAirEnthalpy = 0.0;
			SteamCoil( CoilNum ).OutletAirEnthalpy = 0.0;
			SteamCoil( CoilNum ).TotSteamCoilLoad = 0.0;
			SteamCoil( CoilNum ).SenSteamCoilLoad = 0.0;
			SteamCoil( CoilNum ).LoopLoss = 0.0;
			SteamCoil( CoilNum ).LeavingRelHum = 0.0;
			SteamCoil( CoilNum ).DesiredOutletTemp = 0.0;
			SteamCoil( CoilNum ).DesiredOutletHumRat = 0.0;
			SteamCoil( CoilNum ).InletSteamTemp = 0.0;
			SteamCoil( CoilNum ).OutletSteamTemp = 0.0;
			SteamCoil( CoilNum ).InletSteamMassFlowRate = 0.0;
			SteamCoil( CoilNum ).OutletSteamMassFlowRate = 0.0;
			SteamCoil( CoilNum ).InletSteamEnthalpy = 0.0;
			SteamCoil( CoilNum ).OutletWaterEnthalpy = 0.0;
			SteamCoil( CoilNum ).InletSteamPress = 0.0;
			SteamCoil( CoilNum ).InletSteamQuality = 0.0;
			SteamCoil( CoilNum ).OutletSteamQuality = 0.0;

			// More Environment initializations
			AirInletNode = SteamCoil( CoilNum ).AirInletNodeNum;
			SteamInletNode = SteamCoil( CoilNum ).SteamInletNodeNum;
			ControlNode = SteamCoil( CoilNum ).TempSetPointNodeNum;
			AirOutletNode = SteamCoil( CoilNum ).AirOutletNodeNum;

			Node( SteamInletNode ).Temp = 100.0;
			Node( SteamInletNode ).Press = 101325.0;
			SteamDensity = GetSatDensityRefrig( fluidNameSteam, Node( SteamInletNode ).Temp, 1.0, Node( SteamInletNode ).FluidIndex, RoutineName );
			StartEnthSteam = GetSatEnthalpyRefrig( fluidNameSteam, Node( SteamInletNode ).Temp, 1.0, Node( SteamInletNode ).FluidIndex, RoutineName );
			Node( SteamInletNode ).Enthalpy = StartEnthSteam;
			Node( SteamInletNode ).Quality = 1.0;
			Node( SteamInletNode ).HumRat = 0.0;
			SteamCoil( CoilNum ).MaxSteamMassFlowRate = SteamDensity * SteamCoil( CoilNum ).MaxSteamVolFlowRate;
			//     Node(SteamInletNode)%MassFlowRate         = SteamCoil(CoilNum)%MaxSteamMassFlowRate
			//     Node(SteamInletNode)%MassFlowRateMinAvail = 0.0
			//     Node(SteamInletNode)%MassFlowRateMaxAvail = SteamCoil(CoilNum)%MaxSteamMassFlowRate
			InitComponentNodes( 0.0, SteamCoil( CoilNum ).MaxSteamMassFlowRate, SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).SteamOutletNodeNum, SteamCoil( CoilNum ).LoopNum, SteamCoil( CoilNum ).LoopSide, SteamCoil( CoilNum ).BranchNum, SteamCoil( CoilNum ).CompNum );
			MyEnvrnFlag( CoilNum ) = false;
		} // End If for the Begin Environment initializations

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( CoilNum ) = true;
		}

		// Do the Begin Day initializations
		// NONE

		// Do the begin HVAC time step initializations
		// NONE

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.

		AirInletNode = SteamCoil( CoilNum ).AirInletNodeNum;
		SteamInletNode = SteamCoil( CoilNum ).SteamInletNodeNum;
		ControlNode = SteamCoil( CoilNum ).TempSetPointNodeNum;
		AirOutletNode = SteamCoil( CoilNum ).AirOutletNodeNum;

		// First set the conditions for the air into the coil model

		// If a temperature setpoint controlled coil must set the desired outlet temp everytime
		if ( ControlNode == 0 ) {
			SteamCoil( CoilNum ).DesiredOutletTemp = 0.0;
		} else if ( ControlNode == AirOutletNode ) {
			SteamCoil( CoilNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint;
		} else {
			SteamCoil( CoilNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint - ( Node( ControlNode ).Temp - Node( AirOutletNode ).Temp );
		}

		SteamCoil( CoilNum ).InletAirMassFlowRate = Node( AirInletNode ).MassFlowRate;
		SteamCoil( CoilNum ).InletAirTemp = Node( AirInletNode ).Temp;
		SteamCoil( CoilNum ).InletAirHumRat = Node( AirInletNode ).HumRat;
		SteamCoil( CoilNum ).InletAirEnthalpy = Node( AirInletNode ).Enthalpy;
		if ( FirstHVACIteration ) {
			SteamCoil( CoilNum ).InletSteamMassFlowRate = SteamCoil( CoilNum ).MaxSteamMassFlowRate;
		} else {
			SteamCoil( CoilNum ).InletSteamMassFlowRate = Node( SteamInletNode ).MassFlowRate;
		}
		SteamCoil( CoilNum ).InletSteamTemp = Node( SteamInletNode ).Temp;
		SteamCoil( CoilNum ).InletSteamEnthalpy = Node( SteamInletNode ).Enthalpy;
		SteamCoil( CoilNum ).InletSteamPress = Node( SteamInletNode ).Press;
		SteamCoil( CoilNum ).InletSteamQuality = Node( SteamInletNode ).Quality;
		SteamCoil( CoilNum ).TotSteamHeatingCoilRate = 0.0;
		SteamCoil( CoilNum ).TotSteamCoolingCoilRate = 0.0;
		SteamCoil( CoilNum ).SenSteamCoolingCoilRate = 0.0;
		//   Node(SteamInletNode)%MassFlowRateMaxAvail = MIN(Node(SteamInletNode)%MassFlowRateMaxAvail,&
		//                                                   SteamCoil(CoilNum)%MaxSteamMassFlowRate)

	}

	void
	SizeSteamCoil( int const CoilNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   Jan 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Steam Coil Components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSatDensityRefrig;
		//  USE BranchInputManager, ONLY: MyPlantSizingIndex
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeSteamCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNum; // do loop index for plant sizing
		int PltSizSteamNum; // index of plant sizing object for 1st steam loop
		bool ErrorsFound; // If errors detected in input
		Real64 CoilInTemp;
		Real64 CoilOutTemp;
		Real64 CoilOutHumRat;
		Real64 CoilInHumRat;
		Real64 DesCoilLoad;
		Real64 DesMassFlow;
		Real64 DesVolFlow;
		Real64 MinFlowFrac;
		Real64 OutAirFrac;
		Real64 TempSteamIn;
		Real64 EnthSteamInDry;
		Real64 EnthSteamOutWet;
		Real64 LatentHeatSteam;
		Real64 SteamDensity;
		Real64 RhoAirStd; // density of air at standard conditions
		Real64 CpAirStd; // specific heat of air at std conditions
		Real64 CpWater; // specific heat of water (condensed steam)

		std::string CompName; // component name
		std::string	CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		bool bPRINT = false; // TRUE if sizing is reported to output (eio)
		Real64 TempSize; // autosized value

		ErrorsFound = false;
		PltSizSteamNum = 0;
		PltSizNum = 0;
		CoilInTemp = 0.0;
		CoilInHumRat = 0.0;
		CoilOutTemp = 0.0;
		DesCoilLoad = 0.0;
		MinFlowFrac = 0.0;
		DesMassFlow = 0.0;
		CpWater = 0.0;
		RhoAirStd = PsyRhoAirFnPbTdbW( StdBaroPress, 20.0, 0.0 );
		CpAirStd = PsyCpAirFnWTdb( 0.0, 20.0 );

		// If this is a steam coil
		// Find the appropriate steam Plant Sizing object
		if ( SteamCoil( CoilNum ).MaxSteamVolFlowRate == AutoSize ) {
			PltSizSteamNum = MyPlantSizingIndex( "steam heating coil", SteamCoil( CoilNum ).Name, SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).SteamOutletNodeNum, ErrorsFound );
		}

		if ( PltSizSteamNum > 0 ) {
			// If this is a central air system heating coil
			if ( CurSysNum > 0 ) {
				// If the coil water volume flow rate needs autosizing, then do it
				if ( SteamCoil( CoilNum ).MaxSteamVolFlowRate == AutoSize ) {
					CheckSysSizing( "Coil:Heating:Steam", SteamCoil( CoilNum ).Name );

					if ( SteamCoil( CoilNum ).DesiccantRegenerationCoil ) {

						DataDesicRegCoil = true;
						DataDesicDehumNum = SteamCoil( CoilNum ).DesiccantDehumNum;
						CompType = SteamCoil( CoilNum ).SteamCoilType;
						CompName = SteamCoil( CoilNum ).Name;
						SizingString = "";
						bPRINT = false;
						TempSize = AutoSize;
						RequestSizing( CompType, CompName, HeatingCoilDesAirInletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
						DataDesInletAirTemp = TempSize;
						TempSize = AutoSize;
						RequestSizing( CompType, CompName, HeatingCoilDesAirOutletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
						DataDesOutletAirTemp = TempSize;
						if ( CurOASysNum > 0 ) {
							OASysEqSizing( CurOASysNum ).AirFlow = true;
							OASysEqSizing( CurOASysNum ).AirVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
						}
						TempSize = AutoSize; // reset back
					}

					// Set the duct flow rate
					{ auto const SELECT_CASE_var( CurDuctType );
					if ( SELECT_CASE_var == Main ) {
						DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else if ( SELECT_CASE_var == Cooling ) {
						DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesCoolVolFlow;
					} else if ( SELECT_CASE_var == Heating ) {
						DesVolFlow = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
					} else if ( SELECT_CASE_var == Other ) {
						DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else {
						DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					}}
					if ( DataDesicRegCoil ) {
						bPRINT = false;
						TempSize = AutoSize;
						RequestSizing( CompType, CompName, HeatingAirflowSizing, SizingString, TempSize, bPRINT, RoutineName );
						DesVolFlow = TempSize;
					}
					DesMassFlow = RhoAirStd * DesVolFlow;
					// get the outside air fraction
					if ( FinalSysSizing( CurSysNum ).HeatOAOption == MinOA ) {
						if ( DesVolFlow > 0.0 ) {
							OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / DesVolFlow;
						} else {
							OutAirFrac = 1.0;
						}
						OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
					} else {
						OutAirFrac = 1.0;
					}

					if ( DataDesicRegCoil ) {
						DesCoilLoad = CpAirStd * DesMassFlow * ( DataDesOutletAirTemp - DataDesInletAirTemp );
					} else {
						// mixed air temp
						CoilInTemp = OutAirFrac * FinalSysSizing( CurSysNum ).HeatOutTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).HeatRetTemp;
						// coil load
						DesCoilLoad = CpAirStd * DesMassFlow * ( FinalSysSizing( CurSysNum ).HeatSupTemp - CoilInTemp );
					}
					//AUTOSTEAMCOIL
					if ( DesCoilLoad >= SmallLoad ) {
						//TempSteamIn=SteamCoil(CoilNum)%InletSteamTemp
						//TempSteamIn=PlantSizData(PltSizSteamNum)%ExitTemp
						TempSteamIn = 100.0; // DSU? Should be from the PlantSizing object (ExitTemp) instead of hardwired to 100?
						// RefrigIndex is set during GetInput for this module
						EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
						EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
						LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
						// SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad/(SteamDensity * LatentHeatSteam)
						//            CpWater  =  GetSpecificHeatGlycol('WATER',  &
						//                                              TempSteamIn, &
						//                                              PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
						//                                             'SizeSteamCoil')
						CpWater = GetSatSpecificHeatRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );

						SteamCoil( CoilNum ).MaxSteamVolFlowRate = DesCoilLoad / ( SteamDensity * ( LatentHeatSteam + SteamCoil( CoilNum ).DegOfSubcooling * CpWater ) );
						//             PlantSizData(PltSizSteamNum)%DeltaT*CPHW(PlantSizData(PltSizSteamNum)%ExitTemp)))
					} else {
						SteamCoil( CoilNum ).MaxSteamVolFlowRate = 0.0;
						ShowWarningError( "The design coil load is zero for COIL:Heating:Steam " + SteamCoil( CoilNum ).Name );
						//CALL ShowContinueError('The autosize value for max Steam flow rate is zero')
						//CALL ShowContinueError('To change this, input a value for UA, change the heating design day, or lower')
						//CALL ShowContinueError('  the system heating design supply air temperature')
					}
					ReportSizingOutput( "Coil:Heating:Steam", SteamCoil( CoilNum ).Name, "Maximum Steam Flow Rate [m3/s]", SteamCoil( CoilNum ).MaxSteamVolFlowRate );
				}
				DataDesicRegCoil = false; // reset all globals to 0 to ensure correct sizing for other child components
				// if this is a zone coil
			} else if ( CurZoneEqNum > 0 ) {
				CheckZoneSizing( "Coil:Heating:Steam", SteamCoil( CoilNum ).Name );
				// autosize the coil steam volume flow rate if needed
				if ( SteamCoil( CoilNum ).MaxSteamVolFlowRate == AutoSize ) {
					// if coil is part of a terminal unit just use the terminal unit value
					if ( TermUnitSingDuct || TermUnitPIU || TermUnitIU ) {
						SteamCoil( CoilNum ).MaxSteamVolFlowRate = TermUnitSizing( CurZoneEqNum ).MaxSTVolFlow;
						// if coil is part of a zonal unit, calc coil load to get hot Steam flow rate
					} else {
						CoilInTemp = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp;
						CoilOutTemp = FinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
						CoilOutHumRat = FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
						DesMassFlow = FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow;
						DesCoilLoad = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) ) * DesMassFlow * ( CoilOutTemp - CoilInTemp );
						if ( DesCoilLoad >= SmallLoad ) {
							TempSteamIn = 100.0; // DSU? Should be from the PlantSizing object (ExitTemp) instead of hardwired to 100?
							// RefrigIndex is set during GetInput for this module
							EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
							EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
							LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
							// SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad/(SteamDensity * LatentHeatSteam)
							//           CpWater  =  GetSpecificHeatGlycol('WATER',  &
							//                                             TempSteamIn, &
							//                                             PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
							//                                            'SizeSteamCoil')
							CpWater = GetSatSpecificHeatRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );

							SteamCoil( CoilNum ).MaxSteamVolFlowRate = DesCoilLoad / ( SteamDensity * ( LatentHeatSteam + SteamCoil( CoilNum ).DegOfSubcooling * CpWater ) );
							//             PlantSizData(PltSizSteamNum)%DeltaT*CPHW(PlantSizData(PltSizSteamNum)%ExitTemp)))
						} else {
							SteamCoil( CoilNum ).MaxSteamVolFlowRate = 0.0;
						}
					}
					// issue warning if hw coil has zero flow
					if ( SteamCoil( CoilNum ).MaxSteamVolFlowRate == 0.0 ) {
						ShowWarningError( "The design coil load is zero for COIL:Heating:Steam " + SteamCoil( CoilNum ).Name );
						ShowContinueError( "The autosize value for max Steam flow rate is zero" );
						//CALL ShowContinueError('To change this, input a value for UA, change the heating design day, or lower')
						//CALL ShowContinueError('  the system heating design supply air temperature')
					}
					ReportSizingOutput( "Coil:Heating:Steam", SteamCoil( CoilNum ).Name, "Maximum Steam Flow Rate [m3/s]", SteamCoil( CoilNum ).MaxSteamVolFlowRate );
				}
			} // end zone coil ELSE - IF

		} else {
			// if there is no heating Plant Sizing object and autosizng was requested, issue an error message
			if ( SteamCoil( CoilNum ).MaxSteamVolFlowRate == AutoSize ) {
				ShowSevereError( "Autosizing of Steam coil requires a heating loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Steam coil object= " + SteamCoil( CoilNum ).Name );
				ErrorsFound = true;
			}
		} // end of heating Plant Sizing existence IF - ELSE

		// save the design Steam volumetric flow rate for use by the Steam loop sizing algorithms
		RegisterPlantCompDesignFlow( SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).MaxSteamVolFlowRate );

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding Steam coil sizing errors cause program termination" );
		}

	}

	// End Initialization Section of the Module

	// Begin Algorithm Section of the Module

	void
	CalcSteamAirCoil(
		int const CoilNum,
		Real64 const QCoilRequested, // requested coil load
		Real64 & QCoilActual, // coil load actually delivered
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	)
	{
		// SUBROUTINE INFORMATION:
		//   AUTHOR         Rahul Chillar
		//   DATE WRITTEN   Jan 2005
		//   MODIFIED       Sept. 2012, B. Griffith, add calls to SetComponentFlowRate for plant interactions
		//   RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simple Steam to air heat exchanger which,
		// serves as an interface for distributing heat from boiler to zones.

		// METHODOLOGY EMPLOYED:
		// Steam coils are different, All of steam condenses in heat exchanger
		// Steam traps allow only water to leave the coil,the degree of subcooling
		// desired is input by the user, which is used to calculate water outlet temp.
		// Heat exchange is = Latent Heat + Sensible heat,coil effectivness is 1.0

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TempControlTol;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSteamAirCoil" );
		static std::string const RoutineNameSizeSteamCoil( "SizeSteamCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 SteamMassFlowRate( 0.0 );
		static Real64 AirMassFlow( 0.0 ); // [kg/sec]
		static Real64 TempAirIn( 0.0 ); // [C]
		static Real64 TempAirOut( 0.0 ); // [C]
		static Real64 Win( 0.0 );
		static Real64 TempSteamIn( 0.0 );
		static Real64 TempWaterOut( 0.0 );
		static Real64 CapacitanceAir( 0.0 );
		static Real64 HeatingCoilLoad( 0.0 );
		static Real64 CoilPress( 0.0 );
		static Real64 EnthSteamInDry( 0.0 );
		static Real64 EnthSteamOutWet( 0.0 );
		static Real64 LatentHeatSteam( 0.0 );
		static Real64 SubcoolDeltaTemp( 0.0 );
		static Real64 TempSetPoint( 0.0 );
		static Real64 QCoilReq( 0.0 );
		static Real64 QCoilCap( 0.0 );
		static Real64 QSteamCoilMaxHT( 0.0 );
		static Real64 TempWaterAtmPress( 0.0 );
		static Real64 TempLoopOutToPump( 0.0 );
		static Real64 EnergyLossToEnvironment( 0.0 );
		static Real64 EnthCoilOutlet( 0.0 );
		static Real64 EnthPumpInlet( 0.0 );
		static Real64 EnthAtAtmPress( 0.0 );
		static Real64 CpWater( 0.0 );

		QCoilReq = QCoilRequested;
		TempAirIn = SteamCoil( CoilNum ).InletAirTemp;
		Win = SteamCoil( CoilNum ).InletAirHumRat;
		TempSteamIn = SteamCoil( CoilNum ).InletSteamTemp;
		CoilPress = SteamCoil( CoilNum ).InletSteamPress;
		SubcoolDeltaTemp = SteamCoil( CoilNum ).DegOfSubcooling;
		TempSetPoint = SteamCoil( CoilNum ).DesiredOutletTemp;

		//  adjust mass flow rates for cycling fan cycling coil operation
		if ( FanOpMode == CycFanCycCoil ) {
			if ( PartLoadRatio > 0.0 ) {
				AirMassFlow = SteamCoil( CoilNum ).InletAirMassFlowRate / PartLoadRatio;
				SteamMassFlowRate = min( SteamCoil( CoilNum ).InletSteamMassFlowRate / PartLoadRatio, SteamCoil( CoilNum ).MaxSteamMassFlowRate );
				QCoilReq /= PartLoadRatio;
			} else {
				AirMassFlow = 0.0;
				SteamMassFlowRate = 0.0;
			}
		} else {
			AirMassFlow = SteamCoil( CoilNum ).InletAirMassFlowRate;
			SteamMassFlowRate = SteamCoil( CoilNum ).InletSteamMassFlowRate;
		}

		if ( AirMassFlow > 0.0 ) { // If the coil is operating
			CapacitanceAir = PsyCpAirFnWTdb( Win, TempAirIn ) * AirMassFlow;
		} else {
			CapacitanceAir = 0.0;
		}

		// If the coil is operating there should be some heating capacitance
		//  across the coil, so do the simulation. If not set outlet to inlet and no load.
		//  Also the coil has to be scheduled to be available
		//  Control output to meet load QCoilReq. Load Controlled Coil.
		{ auto const SELECT_CASE_var( SteamCoil( CoilNum ).TypeOfCoil );

		if ( SELECT_CASE_var == ZoneLoadControl ) {
			if ( ( CapacitanceAir > 0.0 ) && ( ( SteamCoil( CoilNum ).InletSteamMassFlowRate ) > 0.0 ) && ( GetCurrentScheduleValue( SteamCoil( CoilNum ).SchedPtr ) > 0.0 || MySizeFlag( CoilNum ) ) && ( QCoilReq > 0.0 ) ) {

				// Steam heat exchangers would not have effectivness, since all of the steam is
				// converted to water and only then the steam trap allows it to leave the heat
				// exchanger, subsequently heat exchange is latent heat + subcooling.
				EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
				EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );

				LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;

				//          CpWater = GetSpecificHeatGlycol('WATER',  &
				//                                           TempSteamIn, &
				//                                           PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
				//                                           'CalcSteamAirCoil')

				CpWater = GetSatSpecificHeatRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineNameSizeSteamCoil );

				// Max Heat Transfer
				QSteamCoilMaxHT = SteamCoil( CoilNum ).MaxSteamMassFlowRate * ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );
				SteamCoil( CoilNum ).OperatingCapacity = QSteamCoilMaxHT;

				// Determine the Max coil capacity and check for the same.
				if ( QCoilReq > QSteamCoilMaxHT ) {
					QCoilCap = QSteamCoilMaxHT;
				} else {
					QCoilCap = QCoilReq;
				}

				// Steam Mass Flow Rate Required
				SteamMassFlowRate = QCoilCap / ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );

				SetComponentFlowRate( SteamMassFlowRate, SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).SteamOutletNodeNum, SteamCoil( CoilNum ).LoopNum, SteamCoil( CoilNum ).LoopSide, SteamCoil( CoilNum ).BranchNum, SteamCoil( CoilNum ).CompNum );

				// recalculate if mass flow rate changed in previous call.
				QCoilCap = SteamMassFlowRate * ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );

				// In practice Sensible & Superheated heat transfer is negligible compared to latent part.
				// This is required for outlet water temperature, otherwise it will be saturation temperature.
				// Steam Trap drains off all the Water formed.
				// Here Degree of Subcooling is used to calculate hot water return temperature.

				// Calculating Water outlet temperature
				TempWaterOut = TempSteamIn - SubcoolDeltaTemp;

				// Total Heat Transfer to air
				HeatingCoilLoad = QCoilCap;

				// Temperature of air at outlet
				TempAirOut = TempAirIn + QCoilCap / ( AirMassFlow * PsyCpAirFnWTdb( Win, TempAirIn ) );

				SteamCoil( CoilNum ).OutletSteamMassFlowRate = SteamMassFlowRate;
				SteamCoil( CoilNum ).InletSteamMassFlowRate = SteamMassFlowRate;

				//************************* Loop Losses *****************************
				// Loop pressure return considerations included in steam coil since the pipes are
				// perfect and do not account for losses.
				// Return water is condensate at atmoshperic pressure
				// Process is considered constant enthalpy expansion
				// No quality function in EnergyPlus hence no option left apart from
				// considering saturated state.
				//              StdBaroPress=101325

				TempWaterAtmPress = GetSatTemperatureRefrig( fluidNameSteam, StdBaroPress, SteamCoil( CoilNum ).FluidIndex, RoutineName );

				// Point 4 at atm - loop delta subcool during return journery back to pump
				TempLoopOutToPump = TempWaterAtmPress - SteamCoil( CoilNum ).LoopSubcoolReturn;

				// Actual Steam Coil Outlet Enthalpy
				EnthCoilOutlet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName ) - CpWater * SubcoolDeltaTemp;

				// Enthalpy at Point 4
				EnthAtAtmPress = GetSatEnthalpyRefrig( fluidNameSteam, TempWaterAtmPress, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );

				// Reported value of coil outlet enthalpy at the node to match the node outlet temperature
				CpWater = GetSatSpecificHeatRefrig( fluidNameSteam, TempLoopOutToPump, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineNameSizeSteamCoil );

				EnthPumpInlet = EnthAtAtmPress - CpWater * SteamCoil( CoilNum ).LoopSubcoolReturn;

				SteamCoil( CoilNum ).OutletWaterEnthalpy = EnthPumpInlet;

				// Point 3-Point 5,
				EnergyLossToEnvironment = SteamMassFlowRate * ( EnthCoilOutlet - EnthPumpInlet );

				// Loss to enviornment due to pressure drop
				SteamCoil( CoilNum ).LoopLoss = EnergyLossToEnvironment;
				//************************* Loop Losses *****************************
			} else { // Coil is not running.

				TempAirOut = TempAirIn;
				TempWaterOut = TempSteamIn;
				HeatingCoilLoad = 0.0;
				SteamCoil( CoilNum ).OutletWaterEnthalpy = SteamCoil( CoilNum ).InletSteamEnthalpy;
				SteamCoil( CoilNum ).OutletSteamMassFlowRate = 0.0;
				SteamCoil( CoilNum ).OutletSteamQuality = 0.0;
				SteamCoil( CoilNum ).LoopLoss = 0.0;
				TempLoopOutToPump = TempWaterOut;
			}

		} else if ( SELECT_CASE_var == TemperatureSetPointControl ) {
			// Control coil output to meet a Setpoint Temperature.
			if ( ( CapacitanceAir > 0.0 ) && ( ( SteamCoil( CoilNum ).InletSteamMassFlowRate ) > 0.0 ) && ( GetCurrentScheduleValue( SteamCoil( CoilNum ).SchedPtr ) > 0.0 || MySizeFlag( CoilNum ) ) && ( std::abs( TempSetPoint - TempAirIn ) > TempControlTol ) ) {

				// Steam heat exchangers would not have effectivness, since all of the steam is
				// converted to water and only then the steam trap allows it to leave the heat
				// exchanger, subsequently heat exchange is latent heat + subcooling.
				EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
				EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );
				LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;

				//          CpWater = GetSpecificHeatGlycol('WATER',  &
				//                                           TempSteamIn, &
				//                                           PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
				//                                           'CalcSteamAirCoil')
				CpWater = GetSatSpecificHeatRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineNameSizeSteamCoil );

				// Max Heat Transfer
				QSteamCoilMaxHT = SteamCoil( CoilNum ).MaxSteamMassFlowRate * ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );

				// Coil Load in case of temperature setpoint
				QCoilCap = CapacitanceAir * ( TempSetPoint - TempAirIn );

				// Check to see if setpoint above enetering temperature. If not, set
				// output to zero.
				if ( QCoilCap <= 0.0 ) {
					QCoilCap = 0.0;
					TempAirOut = TempAirIn;

					// Steam Mass Flow Rate Required
					SteamMassFlowRate = 0.0;
					SetComponentFlowRate( SteamMassFlowRate, SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).SteamOutletNodeNum, SteamCoil( CoilNum ).LoopNum, SteamCoil( CoilNum ).LoopSide, SteamCoil( CoilNum ).BranchNum, SteamCoil( CoilNum ).CompNum );
					// Inlet equal to outlet when not required to run.
					TempWaterOut = TempSteamIn;

					// Total Heat Transfer to air
					HeatingCoilLoad = QCoilCap;

					//The HeatingCoilLoad is the change in the enthalpy of the water
					SteamCoil( CoilNum ).OutletWaterEnthalpy = SteamCoil( CoilNum ).InletSteamEnthalpy;

					// Outlet flow rate set to inlet
					SteamCoil( CoilNum ).OutletSteamMassFlowRate = SteamMassFlowRate;
					SteamCoil( CoilNum ).InletSteamMassFlowRate = SteamMassFlowRate;

				} else if ( QCoilCap > QSteamCoilMaxHT ) {
					// Setting to Maximum Coil Capacity
					QCoilCap = QSteamCoilMaxHT;

					// Temperature of air at outlet
					TempAirOut = TempAirIn + QCoilCap / ( AirMassFlow * PsyCpAirFnWTdb( Win, TempAirIn ) );

					// In practice Sensible & Superheated heat transfer is negligible compared to latent part.
					// This is required for outlet water temperature, otherwise it will be saturation temperature.
					// Steam Trap drains off all the Water formed.
					// Here Degree of Subcooling is used to calculate hot water return temperature.

					// Calculating Water outlet temperature
					TempWaterOut = TempSteamIn - SubcoolDeltaTemp;

					// Steam Mass Flow Rate Required
					SteamMassFlowRate = QCoilCap / ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );
					SetComponentFlowRate( SteamMassFlowRate, SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).SteamOutletNodeNum, SteamCoil( CoilNum ).LoopNum, SteamCoil( CoilNum ).LoopSide, SteamCoil( CoilNum ).BranchNum, SteamCoil( CoilNum ).CompNum );

					// recalculate in case previous call changed mass flow rate
					QCoilCap = SteamMassFlowRate * ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );
					TempAirOut = TempAirIn + QCoilCap / ( AirMassFlow * PsyCpAirFnWTdb( Win, TempAirIn ) );

					// Total Heat Transfer to air
					HeatingCoilLoad = QCoilCap;

					//The HeatingCoilLoad is the change in the enthalpy of the water
					SteamCoil( CoilNum ).OutletWaterEnthalpy = SteamCoil( CoilNum ).InletSteamEnthalpy - HeatingCoilLoad / SteamMassFlowRate;
					SteamCoil( CoilNum ).OutletSteamMassFlowRate = SteamMassFlowRate;
					SteamCoil( CoilNum ).InletSteamMassFlowRate = SteamMassFlowRate;

				} else {
					// Temp air out is temperature Setpoint
					TempAirOut = TempSetPoint;

					// In practice Sensible & Superheated heat transfer is negligible compared to latent part.
					// This is required for outlet water temperature, otherwise it will be saturation temperature.
					// Steam Trap drains off all the Water formed.
					// Here Degree of Subcooling is used to calculate hot water return temperature.

					// Calculating Water outlet temperature
					TempWaterOut = TempSteamIn - SubcoolDeltaTemp;

					// Steam Mass Flow Rate Required
					SteamMassFlowRate = QCoilCap / ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );
					SetComponentFlowRate( SteamMassFlowRate, SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).SteamOutletNodeNum, SteamCoil( CoilNum ).LoopNum, SteamCoil( CoilNum ).LoopSide, SteamCoil( CoilNum ).BranchNum, SteamCoil( CoilNum ).CompNum );

					// recalculate in case previous call changed mass flow rate
					QCoilCap = SteamMassFlowRate * ( LatentHeatSteam + SubcoolDeltaTemp * CpWater );
					TempAirOut = TempAirIn + QCoilCap / ( AirMassFlow * PsyCpAirFnWTdb( Win, TempAirIn ) );

					// Total Heat Transfer to air
					HeatingCoilLoad = QCoilCap;

					SteamCoil( CoilNum ).OutletSteamMassFlowRate = SteamMassFlowRate;
					SteamCoil( CoilNum ).InletSteamMassFlowRate = SteamMassFlowRate;

					//************************* Loop Losses *****************************
					// Loop pressure return considerations included in steam coil since the pipes are
					// perfect and do not account for losses.

					// Return water is condensate at atmoshperic pressure
					// Process is considered constant enthalpy expansion
					// No quality function in EnergyPlus hence no option left apart from
					// considering saturated state.
					//              StdBaroPress=101325

					TempWaterAtmPress = GetSatTemperatureRefrig( fluidNameSteam, StdBaroPress, SteamCoil( CoilNum ).FluidIndex, RoutineName );

					// Point 4 at atm - loop delta subcool during return journery back to pump
					TempLoopOutToPump = TempWaterAtmPress - SteamCoil( CoilNum ).LoopSubcoolReturn;

					// Actual Steam Coil Outlet Enthalpy
					EnthCoilOutlet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName ) - CpWater * SubcoolDeltaTemp;

					// Enthalpy at Point 4
					EnthAtAtmPress = GetSatEnthalpyRefrig( fluidNameSteam, TempWaterAtmPress, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineName );

					CpWater = GetSatSpecificHeatRefrig( fluidNameSteam, TempLoopOutToPump, 0.0, SteamCoil( CoilNum ).FluidIndex, RoutineNameSizeSteamCoil );

					// Reported value of coil outlet enthalpy at the node to match the node outlet temperature
					EnthPumpInlet = EnthAtAtmPress - CpWater * SteamCoil( CoilNum ).LoopSubcoolReturn;

					SteamCoil( CoilNum ).OutletWaterEnthalpy = EnthPumpInlet;

					// Point 3-Point 5,
					EnergyLossToEnvironment = SteamMassFlowRate * ( EnthCoilOutlet - EnthPumpInlet );

					// Loss to enviornment due to pressure drop
					SteamCoil( CoilNum ).LoopLoss = EnergyLossToEnvironment;
					//************************* Loop Losses *****************************
				}

			} else { // If not running Conditions do not change across coil from inlet to outlet
				SteamMassFlowRate = 0.0;
				SetComponentFlowRate( SteamMassFlowRate, SteamCoil( CoilNum ).SteamInletNodeNum, SteamCoil( CoilNum ).SteamOutletNodeNum, SteamCoil( CoilNum ).LoopNum, SteamCoil( CoilNum ).LoopSide, SteamCoil( CoilNum ).BranchNum, SteamCoil( CoilNum ).CompNum );
				TempAirOut = TempAirIn;
				TempWaterOut = TempSteamIn;
				HeatingCoilLoad = 0.0;
				SteamCoil( CoilNum ).OutletWaterEnthalpy = SteamCoil( CoilNum ).InletSteamEnthalpy;
				SteamCoil( CoilNum ).OutletSteamMassFlowRate = 0.0;
				SteamCoil( CoilNum ).OutletSteamQuality = 0.0;
				SteamCoil( CoilNum ).LoopLoss = 0.0;
				TempLoopOutToPump = TempWaterOut;
			}

		}}

		if ( FanOpMode == CycFanCycCoil ) {
			HeatingCoilLoad *= PartLoadRatio;
		}

		// Set the outlet conditions
		SteamCoil( CoilNum ).TotSteamHeatingCoilRate = HeatingCoilLoad;
		SteamCoil( CoilNum ).OutletAirTemp = TempAirOut;
		SteamCoil( CoilNum ).OutletSteamTemp = TempLoopOutToPump;
		SteamCoil( CoilNum ).OutletSteamQuality = 0.0;
		QCoilActual = HeatingCoilLoad;

		// This SteamCoil does not change the moisture or Mass Flow across the component
		SteamCoil( CoilNum ).OutletAirHumRat = SteamCoil( CoilNum ).InletAirHumRat;
		SteamCoil( CoilNum ).OutletAirMassFlowRate = SteamCoil( CoilNum ).InletAirMassFlowRate;
		//Set the outlet enthalpys for air and water
		SteamCoil( CoilNum ).OutletAirEnthalpy = PsyHFnTdbW( SteamCoil( CoilNum ).OutletAirTemp, SteamCoil( CoilNum ).OutletAirHumRat );

	}

	// Beginning of Update subroutines for the SteamCoil Module

	void
	UpdateSteamCoil( int const CoilNum )
	{
		// SUBROUTINE INFORMATION:
		//   AUTHOR         Rahul Chillar
		//   DATE WRITTEN   Jan 2005
		//   MODIFIED       na
		//   RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the coil outlet nodes.

		// METHODOLOGY EMPLOYED:
		// Data is moved from the coil data structure to the coil outlet nodes.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirInletNode;
		int SteamInletNode;
		int AirOutletNode;
		int SteamOutletNode;

		AirInletNode = SteamCoil( CoilNum ).AirInletNodeNum;
		SteamInletNode = SteamCoil( CoilNum ).SteamInletNodeNum;
		AirOutletNode = SteamCoil( CoilNum ).AirOutletNodeNum;
		SteamOutletNode = SteamCoil( CoilNum ).SteamOutletNodeNum;

		// Set the outlet air nodes of the SteamCoil
		Node( AirOutletNode ).MassFlowRate = SteamCoil( CoilNum ).OutletAirMassFlowRate;
		Node( AirOutletNode ).Temp = SteamCoil( CoilNum ).OutletAirTemp;
		Node( AirOutletNode ).HumRat = SteamCoil( CoilNum ).OutletAirHumRat;
		Node( AirOutletNode ).Enthalpy = SteamCoil( CoilNum ).OutletAirEnthalpy;

		SafeCopyPlantNode( SteamInletNode, SteamOutletNode );

		// Set the outlet Steam nodes for the Coil
		//   Node(SteamOutletNode)%MassFlowRate = SteamCoil(CoilNum)%OutletSteamMassFlowRate
		Node( SteamOutletNode ).Temp = SteamCoil( CoilNum ).OutletSteamTemp;
		Node( SteamOutletNode ).Enthalpy = SteamCoil( CoilNum ).OutletWaterEnthalpy;
		Node( SteamOutletNode ).Quality = SteamCoil( CoilNum ).OutletSteamQuality;
		//Node(SteamInletNode)%MassFlowRate  = SteamCoil(CoilNum)%OutletSteamMassFlowRate

		// Set the outlet nodes for properties that just pass through & not used
		Node( AirOutletNode ).Quality = Node( AirInletNode ).Quality;
		Node( AirOutletNode ).Press = Node( AirInletNode ).Press;
		Node( AirOutletNode ).MassFlowRateMin = Node( AirInletNode ).MassFlowRateMin;
		Node( AirOutletNode ).MassFlowRateMax = Node( AirInletNode ).MassFlowRateMax;
		Node( AirOutletNode ).MassFlowRateMinAvail = Node( AirInletNode ).MassFlowRateMinAvail;
		Node( AirOutletNode ).MassFlowRateMaxAvail = Node( AirInletNode ).MassFlowRateMaxAvail;

		// Set the outlet nodes for properties that just pass through & not used

		//Node(SteamOutletNode)%Press              = Node(SteamInletNode)%Press
		//   Node(SteamOutletNode)%Press               = StdBaroPress  ! Water out at atm pressure
		//   Node(SteamOutletNode)%HumRat              = Node(SteamInletNode)%HumRat
		//   Node(SteamOutletNode)%MassFlowRateMin     = Node(SteamInletNode)%MassFlowRateMin
		//   Node(SteamOutletNode)%MassFlowRateMax     = Node(SteamInletNode)%MassFlowRateMax
		//   Node(SteamOutletNode)%MassFlowRateMinAvail= Node(SteamInletNode)%MassFlowRateMinAvail
		//   Node(SteamOutletNode)%MassFlowRateMaxAvail= Node(SteamInletNode)%MassFlowRateMaxAvail

		//   IF (SteamCoil(CoilNum)%InletSteamMassFlowRate.EQ.0.0) THEN
		//     Node(SteamInletNode)%MassFlowRate         = 0.0
		//     Node(SteamInletNode)%MassFlowRateMinAvail = 0.0
		//     Node(SteamOutletNode)%MassFlowRateMinAvail= 0.0
		//   END IF

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNode ).CO2 = Node( AirInletNode ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutletNode ).GenContam = Node( AirInletNode ).GenContam;
		}

	}

	// End of Update subroutines for the SteamCoil Module

	// Beginning of Reporting subroutines for the SteamCoil Module

	void
	ReportSteamCoil( int const CoilNum )
	{
		// SUBROUTINE INFORMATION:
		//   AUTHOR         Rahul Chillar
		//   DATE WRITTEN   Jan 2005
		//   MODIFIED       na
		//   RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variable for the coils.

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

		// Report the SteamCoil energy from this component
		SteamCoil( CoilNum ).TotSteamHeatingCoilEnergy = SteamCoil( CoilNum ).TotSteamHeatingCoilRate * TimeStepSys * SecInHour;

	}

	// End of Reporting subroutines for the SteamCoil Module

	// Utility subroutines for the SteamCoil Module

	int
	GetSteamCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   August 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the index for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int IndexNum; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilType == "COIL:HEATING:STEAM" ) {
			IndexNum = FindItemInList( CoilName, SteamCoil );
		} else {
			IndexNum = 0;
		}

		if ( IndexNum == 0 ) {
			ShowSevereError( "GetSteamCoilIndex: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
		}

		return IndexNum;

	}

	void
	CheckSteamCoilSchedule(
		std::string const & EP_UNUSED( CompType ),
		std::string const & CompName,
		Real64 & Value,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the correct schedule value for this coil

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

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoilNum;

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		// Find the correct Coil number
		if ( CompIndex == 0 ) {
			CoilNum = FindItemInList( CompName, SteamCoil );
			if ( CoilNum == 0 ) {
				ShowFatalError( "CheckSteamCoilSchedule: Coil not found=" + CompName );
			}
			CompIndex = CoilNum;
			Value = GetCurrentScheduleValue( SteamCoil( CoilNum ).SchedPtr ); // not scheduled?
		} else {
			CoilNum = CompIndex;
			if ( CoilNum > NumSteamCoils || CoilNum < 1 ) {
				ShowFatalError( "SimulateSteamCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Number of Steam Coils=" + TrimSigDigits( NumSteamCoils ) + ", Coil name=" + CompName );
			}
			if ( CompName != SteamCoil( CoilNum ).Name ) {
				ShowFatalError( "SimulateSteamCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + SteamCoil( CoilNum ).Name );
			}
			Value = GetCurrentScheduleValue( SteamCoil( CoilNum ).SchedPtr ); // not scheduled?
		}

	}

	Real64
	GetCoilMaxWaterFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the max water flow rate for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		Real64 MaxWaterFlowRate; // returned max water flow rate of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;
		static int ErrCount( 0 );

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:Steam" ) ) {
			WhichCoil = FindItem( CoilName, SteamCoil );
			if ( WhichCoil != 0 ) {
				// coil does not specify MaxWaterFlowRate
				MaxWaterFlowRate = 0.0;
				ShowRecurringWarningErrorAtEnd( "Requested Max Water Flow Rate from COIL:Heating:Steam N/A", ErrCount );
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilMaxWaterFlowRate: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			MaxWaterFlowRate = -1000.0;
		}

		return MaxWaterFlowRate;

	}

	Real64
	GetCoilMaxSteamFlowRate(
		int const CoilIndex, // must match coil types in this module
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   August 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the max steam flow rate for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and flow rate is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Real64 MaxSteamFlowRate; // returned max steam flow rate of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( "GetCoilMaxSteamFlowRate: Could not find CoilType = \"Coil:Heating:Steam\"" );
			ErrorsFound = true;
			MaxSteamFlowRate = 0.0;
		} else {
			MaxSteamFlowRate = SteamCoil( CoilIndex ).MaxSteamVolFlowRate;
		}

		return MaxSteamFlowRate;

	}

	int
	GetCoilAirInletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the air inlet node number for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( "GetCoilAirInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName );
			ErrorsFound = true;
			NodeNumber = 0;
		} else {
			NodeNumber = SteamCoil( CoilIndex ).AirInletNodeNum;
		}

		return NodeNumber;

	}

	int
	GetCoilAirOutletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the air outlet node number for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NodeNumber; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( "GetCoilAirOutletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName );
			ErrorsFound = true;
			NodeNumber = 0;
		} else {
			NodeNumber = SteamCoil( CoilIndex ).AirOutletNodeNum;
		}

		return NodeNumber;

	}

	int
	GetCoilAirOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & EP_UNUSED( ErrorsFound ) // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the air outlet node number for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		int NodeNumber; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int IndexNum; // returned air inlet node number of matched coil

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:Steam" ) ) {
			IndexNum = FindItem( CoilName, SteamCoil );
		} else {
			IndexNum = 0;
		}

		if ( IndexNum == 0 ) {
			NodeNumber = 0;
		} else {
			NodeNumber = SteamCoil( IndexNum ).AirOutletNodeNum;
		}

		return NodeNumber;

	}

	int
	GetCoilSteamInletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the steam inlet node number for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName );
			ErrorsFound = true;
			NodeNumber = 0;
		} else {
			NodeNumber = SteamCoil( CoilIndex ).SteamInletNodeNum;
		}

		return NodeNumber;

	}

	int
	GetCoilSteamInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         L. Lawrie (based on R. Raustad)
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the steam inlet node number for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		int NodeNumber; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int IndexNum; // returned air inlet node number of matched coil

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:Steam" ) ) {
			IndexNum = FindItem( CoilName, SteamCoil );
		} else {
			IndexNum = 0;
		}

		if ( IndexNum == 0 ) {
			ShowSevereError( "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName );
			ErrorsFound = true;
			NodeNumber = 0;
		} else {
			NodeNumber = SteamCoil( IndexNum ).SteamInletNodeNum;
		}

		return NodeNumber;

	}

	int
	GetCoilSteamOutletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the steam inlet node number for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName );
			ErrorsFound = true;
			NodeNumber = 0;
		} else {
			NodeNumber = SteamCoil( CoilIndex ).SteamOutletNodeNum;
		}

		return NodeNumber;

	}

	int
	GetCoilSteamOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         L. Lawrie (based on R. Raustad)
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the steam inlet node number for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		int NodeNumber; // returned air inlet node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int IndexNum; // returned air inlet node number of matched coil

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:Steam" ) ) {
			IndexNum = FindItem( CoilName, SteamCoil );
		} else {
			IndexNum = 0;
		}

		if ( IndexNum == 0 ) {
			ShowSevereError( "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName );
			ErrorsFound = true;
			NodeNumber = 0;
		} else {
			NodeNumber = SteamCoil( IndexNum ).SteamOutletNodeNum;
		}

		return NodeNumber;

	}

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the steam coils operating capacity and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		Real64 Capacity; // returned operating capacity of matched coil (W)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:Steam" ) ) {
			WhichCoil = FindItem( CoilName, SteamCoil );
			if ( WhichCoil != 0 ) {
				// coil does not specify MaxWaterFlowRate
				Capacity = SteamCoil( WhichCoil ).OperatingCapacity;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilSteamInletNode: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			Capacity = 0.0;
		}

		return Capacity;

	}

	int
	GetTypeOfCoil(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the steam coils operating capacity and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int TypeOfCoil; // returned coil type of matched coil (W)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// 1 = TemperatureSetPointControl
		// 3 = ZoneLoadControl

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName );
			ErrorsFound = true;
			TypeOfCoil = 0;
		} else {
			TypeOfCoil = SteamCoil( CoilIndex ).TypeOfCoil;
		}

		return TypeOfCoil;

	}

	int
	GetSteamCoilControlNodeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorFlag // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Nigusse, FSEC
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the steam coils and returns the steam control node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		int NodeNumber; // returned node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates SteamCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		WhichCoil = 0;
		NodeNumber = 0;
		if ( SameString( CoilType, "Coil:Heating:Steam" ) ) {
			WhichCoil = FindItem( CoilName, SteamCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = SteamCoil( WhichCoil ).TempSetPointNodeNum;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetSteamCoilControlNodeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorFlag = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetSteamCoilAvailScheduleIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the availability schedule index.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		int AvailSchIndex; // returned availability schedule of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetSteamCoilsInputFlag ) { //First time subroutine has been entered
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		WhichCoil = 0;
		AvailSchIndex = 0;

		if ( SameString( CoilType, "Coil:Heating:Steam" ) ) {
			WhichCoil = FindItem( CoilName, SteamCoil );
			if ( WhichCoil != 0 ) {
				AvailSchIndex = SteamCoil( WhichCoil ).SchedPtr;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilAvailScheduleIndex: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			AvailSchIndex = 0;
		}

		return AvailSchIndex;

	}

	void
	SetSteamCoilData(
		int const CoilNum, // Number of hot water heating Coil
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_bool DesiccantRegenerationCoil, // Flag that this coil is used as regeneration air heating coil
		Optional_int DesiccantDehumIndex // Index for the desiccant dehum system where this caoil is used 
		) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   February 2016
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function sets data to water Heating Coil using the coil index and arguments passed

		// Using/Aliasing
		using General::TrimSigDigits;

		if ( GetSteamCoilsInputFlag ) {
			GetSteamCoilInput();
			GetSteamCoilsInputFlag = false;
		}

		if ( CoilNum <= 0 || CoilNum > NumSteamCoils ) {
			ShowSevereError( "SetHeatingCoilData: called with heating coil Number out of range=" + TrimSigDigits( CoilNum ) + " should be >0 and <" + TrimSigDigits( NumSteamCoils ) );
			ErrorsFound = true;
			return;
		}

		if ( present( DesiccantRegenerationCoil ) ) {
			 SteamCoil( CoilNum ).DesiccantRegenerationCoil = DesiccantRegenerationCoil;
		}

		if ( present( DesiccantDehumIndex ) ) {
			 SteamCoil( CoilNum ).DesiccantDehumNum = DesiccantDehumIndex;
		}

	}
	// End of Utility subroutines for the SteamCoil Module

} // SteamCoils

} // EnergyPlus
