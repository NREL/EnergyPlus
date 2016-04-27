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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <Humidifiers.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace Humidifiers {

	// Module containing the routines dealing with humidifiers

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   September 2000
	//       MODIFIED       B Griffith, Aug. 2006 added water system interactions
	//						February 2015, B.Nigusse, FSEC, - transitioned the code
	//						to object oriented approach and Added gas fired humidifier
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and routines required to model humidifier
	// components in the EnergyPlus HVAC simulation

	// METHODOLOGY EMPLOYED:
	// The humidifier encompasses not just the component but also its
	// control. The humidifier adds moisture to its air inlet to meet
	// the HumRatMin setpoint at its exit node. The HumRatMin is set by
	// an external setpoint manager.

	// REFERENCES: ASHRAE HVAC 2 Toolkit, page 4-112

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::DisplayExtraWarnings;
	using namespace DataLoopNode;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SetPointErrorFlag;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const Humidifier_Steam_Electric( 1 );
	int const Humidifier_Steam_Gas( 2 );

	Array1D_string const HumidifierType( 2, { "Humidifier:Steam:Electric", "Humidifier:Steam:Gas" } );

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumHumidifiers( 0 ); // number of humidifiers of all types
	int NumElecSteamHums( 0 ); // number of electric steam humidifiers
	int NumGasSteamHums( 0 ); // number of electric steam humidifiers
	Array1D_bool CheckEquipName;

	// Humidifier normalized thermal efficiency curve types
	int const Linear( 1 );
	int const Quadratic( 2 );
	int const Cubic( 3 );
	int const FixedInletWaterTemperature( 1 );
	int const VariableInletWaterTemperature( 2 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< HumidifierData > Humidifier;

	// Functions

	// Clears the global data in Humidifiers.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumHumidifiers = 0;
		NumElecSteamHums = 0;
		NumGasSteamHums = 0;
		CheckEquipName.deallocate();
		Humidifier.deallocate();
	}

	void
	SimHumidifier(
		std::string const & CompName, // name of the humidifier unit
		bool const EP_UNUSED( FirstHVACIteration ), // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex // Pointer to Humidifier Unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       February 2015, B. Nigusse, FSEC, - Added gas fired humidifier
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manage the simulation of an air humidifier

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HumNum; // index of humidifier unit being simulated
		static bool GetInputFlag( true ); // First time, input is "gotten"
		Real64 WaterAddNeeded; // output in kg/s needed from humidifier to meet humidity setpoint

		if ( GetInputFlag ) {
			GetHumidifierInput();
			GetInputFlag = false;
		}

		// Get the humidifier unit index
		if ( CompIndex == 0 ) {
			HumNum = FindItemInList( CompName, Humidifier );
			if ( HumNum == 0 ) {
				ShowFatalError( "SimHumidifier: Unit not found=" + CompName );
			}
			CompIndex = HumNum;
		} else {
			HumNum = CompIndex;
			if ( HumNum > NumHumidifiers || HumNum < 1 ) {
				ShowFatalError( "SimHumidifier: Invalid CompIndex passed=" + TrimSigDigits( HumNum ) + ", Number of Units=" + TrimSigDigits( NumHumidifiers ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( HumNum ) ) {
				if ( CompName != Humidifier( HumNum ).Name ) {
					ShowFatalError( "SimHumidifier: Invalid CompIndex passed=" + TrimSigDigits( HumNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + Humidifier( HumNum ).Name );
				}
				CheckEquipName( HumNum ) = false;
			}
		}
		if ( HumNum <= 0 ) {
			ShowFatalError( "SimHumidifier: Unit not found=" + CompName );
		}

		auto & thisHum( Humidifier( HumNum ) );

		thisHum.InitHumidifier();

		thisHum.ControlHumidifier( WaterAddNeeded );

		// call the correct humidifier calculation routine
		{ auto const SELECT_CASE_var( thisHum.HumType_Code );

		if ( SELECT_CASE_var == Humidifier_Steam_Electric ) { // 'HUMIDIFIER:STEAM:ELECTRIC'

			thisHum.CalcElecSteamHumidifier( WaterAddNeeded );

		} else if ( SELECT_CASE_var == Humidifier_Steam_Gas ) { // 'HUMIDIFIER:STEAM:GAS'

			thisHum.CalcGasSteamHumidifier( WaterAddNeeded );

		} else {
			ShowSevereError( "SimHumidifier: Invalid Humidifier Type Code=" + TrimSigDigits( thisHum.HumType_Code ) );
			ShowContinueError( "...Component Name=[" + CompName + "]." );
			ShowFatalError( "Preceding Condition causes termination." );

		}}

		thisHum.UpdateReportWaterSystem();

		thisHum.UpdateHumidifier();

		thisHum.ReportHumidifier();

	}

	void
	GetHumidifierInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       February 2015, B. Nigusse, FSEC, - Added gas fired humidifier
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for humidifiers and stores it in humidifier data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using WaterManager::SetupTankDemandComponent;
		using WaterManager::SetupTankSupplyComponent;
		using namespace DataIPShortCuts;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetHumidifierInputs: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HumidifierIndex; // loop index
		int HumNum; // current humidifier number
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int MaxNums; // maximum Number of Numbers for each GetObjectItem call
		int MaxAlphas; // maximum Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file

		CurrentModuleObject = "Humidifier:Steam:Electric";
		NumElecSteamHums = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		MaxNums = NumNumbers;
		MaxAlphas = NumAlphas;
		CurrentModuleObject = "Humidifier:Steam:Gas";
		NumGasSteamHums = GetNumObjectsFound( CurrentModuleObject );
		NumHumidifiers = NumElecSteamHums + NumGasSteamHums;
		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		MaxNums = max( MaxNums, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		// allocate the data array
		Humidifier.allocate( NumHumidifiers );
		CheckEquipName.dimension( NumHumidifiers, true );

		Alphas.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		Numbers.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxAlphas, true );

		// loop over electric steam humidifiers and load the input data
		CurrentModuleObject = "Humidifier:Steam:Electric";
		for ( HumidifierIndex = 1; HumidifierIndex <= NumElecSteamHums; ++HumidifierIndex ) {
			GetObjectItem( CurrentModuleObject, HumidifierIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			HumNum = HumidifierIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Humidifier, HumNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Humidifier( HumNum ).Name = Alphas( 1 );
			//    Humidifier(HumNum)%HumType = TRIM(CurrentModuleObject)
			Humidifier( HumNum ).HumType_Code = Humidifier_Steam_Electric;
			Humidifier( HumNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Humidifier( HumNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Humidifier( HumNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( Humidifier( HumNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			Humidifier( HumNum ).NomCapVol = Numbers( 1 );
			Humidifier( HumNum ).NomPower = Numbers( 2 );
			Humidifier( HumNum ).FanPower = Numbers( 3 );
			Humidifier( HumNum ).StandbyPower = Numbers( 4 );
			Humidifier( HumNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Humidifier( HumNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			//  A5; \field Name of Water Storage Tank
			if ( lAlphaBlanks( 5 ) ) {
				Humidifier( HumNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( Alphas( 1 ), CurrentModuleObject, Alphas( 5 ), ErrorsFound, Humidifier( HumNum ).WaterTankID, Humidifier( HumNum ).WaterTankDemandARRID );
				Humidifier( HumNum ).SuppliedByWaterSystem = true;
			}

		}

		// loop over gas fired steam humidifiers and load the input data
		CurrentModuleObject = "Humidifier:Steam:Gas";
		for ( HumidifierIndex = 1; HumidifierIndex <= NumGasSteamHums; ++HumidifierIndex ) {
			GetObjectItem( CurrentModuleObject, HumidifierIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			HumNum = NumElecSteamHums + HumidifierIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Humidifier, HumNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Humidifier( HumNum ).Name = Alphas( 1 );
			Humidifier( HumNum ).HumType_Code = Humidifier_Steam_Gas;
			Humidifier( HumNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Humidifier( HumNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Humidifier( HumNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( Humidifier( HumNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			Humidifier( HumNum ).NomCapVol = Numbers( 1 );
			Humidifier( HumNum ).NomPower = Numbers( 2 ); // nominal gas use rate for gas fired steam humidifier
			Humidifier( HumNum ).ThermalEffRated = Numbers( 3 );
			Humidifier( HumNum ).FanPower = Numbers( 4 );
			Humidifier( HumNum ).StandbyPower = Numbers( 5 );
			Humidifier( HumNum ).AirInNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Humidifier( HumNum ).AirOutNode = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 4 ), Alphas( 5 ), "Air Nodes" );

			Humidifier( HumNum ).EfficiencyCurvePtr = GetCurveIndex( Alphas( 3 ) );
			if ( Humidifier( HumNum ).EfficiencyCurvePtr > 0 ) {
					{ auto const SELECT_CASE_var( GetCurveType( Humidifier( HumNum ).EfficiencyCurvePtr ) );
					if ( SELECT_CASE_var == "LINEAR" ) {
						Humidifier( HumNum ).EfficiencyCurveType = Linear;
					} else if ( SELECT_CASE_var == "QUADRATIC" ) {
						Humidifier( HumNum ).EfficiencyCurveType = Quadratic;
					} else if ( SELECT_CASE_var == "CUBIC" ) {
						Humidifier( HumNum ).EfficiencyCurveType = Cubic;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"," );
						ShowContinueError( "Invalid " + cAlphaFields( 3 ) + '=' + Alphas( 3 ) );
						ShowContinueError( "...Curve type for " + cAlphaFields( 3 ) + " = " + GetCurveType( Humidifier( HumNum ).EfficiencyCurvePtr ) );
						ErrorsFound = true;
					}}
			} else if ( !lAlphaBlanks( 3 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFields( 3 ) + '=' + Alphas( 3 ) );
				ShowSevereError( "..." + cAlphaFields( 3 ) + " not found." );
				ErrorsFound = true;
			}

			//  A6; \field Name of Water Storage Tank
			if ( lAlphaBlanks( 6 ) ) {
				Humidifier( HumNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( Alphas( 1 ), CurrentModuleObject, Alphas( 6 ), ErrorsFound, Humidifier( HumNum ).WaterTankID, Humidifier( HumNum ).WaterTankDemandARRID );
				SetupTankSupplyComponent( Alphas( 1 ), CurrentModuleObject, Alphas( 6 ), ErrorsFound, Humidifier( HumNum ).WaterTankID, Humidifier( HumNum ).TankSupplyID );
				Humidifier( HumNum ).SuppliedByWaterSystem = true;
			}

			// A7; \field Inlet Water Temperature Option
			if ( lAlphaBlanks( 7 ) ) {
				Humidifier( HumNum ).InletWaterTempOption = FixedInletWaterTemperature;
			} else { // water from storage tank
				if ( Alphas( 7 ) == "FixedInletWaterTemperature" ) {
					Humidifier( HumNum ).InletWaterTempOption = FixedInletWaterTemperature;
				} else if ( Alphas( 7 ) == "VariableInletWaterTemperature" ) {
					Humidifier( HumNum ).InletWaterTempOption = VariableInletWaterTemperature;
				} else {
					Humidifier( HumNum ).InletWaterTempOption = FixedInletWaterTemperature;
				}
			}

		}


		for ( HumNum = 1; HumNum <= NumHumidifiers; ++HumNum ) {
			// Setup Report variables for the Humidifiers
			if ( Humidifier( HumNum ).SuppliedByWaterSystem ) {
				SetupOutputVariable( "Humidifier Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).WaterConsRate, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Water Volume [m3]", Humidifier( HumNum ).WaterCons, "System", "Sum", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Storage Tank Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).TankSupplyVdot, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Storage Tank Water Volume [m3]", Humidifier( HumNum ).TankSupplyVol, "System", "Sum", Humidifier( HumNum ).Name, _, "Water", "HUMIDIFIER", _, "SYSTEM" );
				SetupOutputVariable( "Humidifier Starved Storage Tank Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).StarvedSupplyVdot, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Starved Storage Tank Water Volume [m3]", Humidifier( HumNum ).StarvedSupplyVol, "System", "Sum", Humidifier( HumNum ).Name, _, "Water", "HUMIDIFIER", _, "SYSTEM" );
				SetupOutputVariable( "Humidifier Mains Water Volume [m3]", Humidifier( HumNum ).StarvedSupplyVol, "System", "Sum", Humidifier( HumNum ).Name, _, "MainsWater", "HUMIDIFIER", _, "SYSTEM" );

			} else {
				SetupOutputVariable( "Humidifier Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).WaterConsRate, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Water Volume [m3]", Humidifier( HumNum ).WaterCons, "System", "Sum", Humidifier( HumNum ).Name, _, "WATER", "HUMIDIFIER", _, "System" );
				SetupOutputVariable( "Humidifier Mains Water Volume [m3]", Humidifier( HumNum ).WaterCons, "System", "Sum", Humidifier( HumNum ).Name, _, "MAINSWATER", "HUMIDIFIER", _, "System" );

			}
			if ( Humidifier( HumNum ).HumType_Code == Humidifier_Steam_Electric ) {
				SetupOutputVariable( "Humidifier Electric Power [W]", Humidifier( HumNum ).ElecUseRate, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Electric Energy [J]", Humidifier( HumNum ).ElecUseEnergy, "System", "Sum", Humidifier( HumNum ).Name, _, "ELECTRICITY", "HUMIDIFIER", _, "System" );
			} else if ( Humidifier( HumNum ).HumType_Code == Humidifier_Steam_Gas ) {
				SetupOutputVariable( "Humidifier Gas Use Thermal Efficiency []", Humidifier( HumNum ).ThermalEff, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Gas Use Rate [W]", Humidifier( HumNum ).GasUseRate, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Gas Use Energy [J]", Humidifier( HumNum ).GasUseEnergy, "System", "Sum", Humidifier( HumNum ).Name, _, "GAS", "HUMIDIFIER", _, "System" );
				SetupOutputVariable( "Humidifier Auxiliary Electric Power [W]", Humidifier( HumNum ).AuxElecUseRate, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Auxiliary Electric Energy [J]", Humidifier( HumNum ).AuxElecUseEnergy, "System", "Sum", Humidifier( HumNum ).Name, _, "ELECTRICITY", "HUMIDIFIER", _, "System" );
			}

		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input." );
		}

	}

	void
	HumidifierData::InitHumidifier() // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       February 2015, B. Nigusse, FSEC, - Added gas fired humidifier
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Humidifier Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::DoSetPointTest;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using EMSManager::iHumidityRatioMinSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CalledFrom( "Humidifier:InitHumidifier" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		//static bool MySetPointCheckFlag( true );

		// do sizing calculation once
		if ( MySizeFlag ) {
			SizeHumidifier();
			MySizeFlag = false;
		}

		if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
			if ( AirOutNode > 0 ) {
				if ( Node( AirOutNode ).HumRatMin == SensedNodeFlagValue ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						ShowSevereError( "Humidifiers: Missing humidity setpoint for " + HumidifierType( HumType_Code ) + " = " + Name );
						ShowContinueError( "  use a Setpoint Manager with Control Variable = \"MinimumHumidityRatio\" to establish a setpoint at the humidifier outlet node." );
						ShowContinueError( "  expecting it on Node=\"" + NodeID( AirOutNode ) + "\"." );
						SetPointErrorFlag = true;
					} else {
						CheckIfNodeSetPointManagedByEMS( AirOutNode, iHumidityRatioMinSetPoint, SetPointErrorFlag );
						if ( SetPointErrorFlag ) {
							ShowSevereError( "Humidifiers: Missing humidity setpoint for " + HumidifierType( HumType_Code ) + " = " + Name );
							ShowContinueError( "  use a Setpoint Manager with Control Variable = \"MinimumHumidityRatio\" to establish a setpoint at the humidifier outlet node." );
							ShowContinueError( "  expecting it on Node=\"" + NodeID( AirOutNode ) + "\"." );
							ShowContinueError( "  or use an EMS actuator to control minimum humidity ratio to establish a setpoint at the humidifier outlet node." );
						}
					}
				}
			}
			MySetPointCheckFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// do these initializations every HVAC time step
		HumRatSet = Node( AirOutNode ).HumRatMin;
		AirInTemp = Node( AirInNode ).Temp;
		AirInHumRat = Node( AirInNode ).HumRat;
		AirInEnthalpy = Node( AirInNode ).Enthalpy;
		AirInMassFlowRate = Node( AirInNode ).MassFlowRate;

		WaterAdd = 0.0;
		ElecUseEnergy = 0.0;
		ElecUseRate = 0.0;
		WaterCons = 0.0;
		WaterConsRate = 0.0;
		ThermalEff = 0.0;
		GasUseRate = 0.0;
		GasUseEnergy = 0.0;
		AuxElecUseRate = 0.0;
		AuxElecUseEnergy = 0.0;

	}

	void
	HumidifierData::SizeHumidifier() // number of the current humidifier being sized
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, UCF/FSEC,
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       May 2014, Daeho Kang, PNNL - Added additional sizing field
		//				        February 2015, B. Nigusse, FSEC, - Added gas fired humidifier
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for for sizing electric steam humidifier nominal electric power.

		// METHODOLOGY EMPLOYED:
		// Uses user sepecified nominal capacity in m3/s and water enthalpy change required to
		// vaporize water from a reference temperature of 20.0C. to steam at 100.0C.
		//  m_dot = Nominal Capacity [m3/s] * Density of water at 5.05 [kg/m3]
		//  Nominal Capacity =  m_dot [kg/s] * delta_enthalpy [J/kg]

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::RhoH2O;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::FindGlycol;
		using FluidProperties::FindRefrigerant;
		using General::RoundSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using DataSizing::AutoSize;
		using DataSizing::CurZoneEqNum;
		using DataSizing::ZoneSizingRunDone;
		using DataSizing::CurSysNum;
		using DataSizing::SysSizingRunDone;
		using DataSizing::CurOASysNum;
		using DataSizing::CurDuctType;
		using DataSizing::FinalZoneSizing;
		using DataSizing::FinalSysSizing;
		using DataSizing::AutoVsHardSizingThreshold;
		using DataHVACGlobals::Main;
		using DataHVACGlobals::Cooling;
		using DataHVACGlobals::Heating;
		using DataHVACGlobals::Other;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CalledFrom( "Humidifier:SizeHumidifier" );
		Real64 const Tref( 20.0 ); // Reference temp of water for rated capacity calac [C]
		Real64 const TSteam( 100.0 ); // saturated steam temperatur generated by Humidifier [C]

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string ModuleObjectType; // for ease in getting objects
		int RefrigerantIndex; // refiferant index
		int WaterIndex; // fluid type index
		Real64 NominalPower; // Nominal power input to humidifier, W
		Real64 WaterSpecHeatAvg; // specific heat of water, J/kgK
		Real64 SteamSatEnthalpy; // enthalpy of saturated steam at 100C, J/kg
		Real64 WaterSatEnthalpy; // enthalpy of saturated water at 100C, J/kg
		bool IsAutoSize;			// Indicator to autosize
		bool HardSizeNoDesRun;		// Indicator to a hard-sized field with no design sizing data
		static bool ErrorsFound( false ); // TRUE if errors detected in input
		Real64 NomPowerDes;			// Autosized nominal power for reporting
		Real64 NomPowerUser;		// Hardsized nominal power for reporting
		Real64 MassFlowDes;			// Design air mass flow rate
		Real64 InletHumRatDes;		// Design inlet humidity ratio
		Real64 OutletHumRatDes;		// Design outlet humidity ratio
		Real64 NomCapVolDes;		// Autosized Nominal capacity volume for reporting
		Real64 NomCapVolUser;		// HardSized nominal capacity volume for reporting
		Real64 AirVolFlow;			// Design air volume flow rate
		Real64 AirDensity;			// Density of air


		if ( HumType_Code == Humidifier_Steam_Electric || HumType_Code == Humidifier_Steam_Gas ) {
			IsAutoSize = false;
			HardSizeNoDesRun = false;
			NomPowerDes = 0.0;
			NomPowerUser = 0.0;

			if ( HumType_Code == Humidifier_Steam_Electric ) {
				ModuleObjectType = "electric";
			} else if ( HumType_Code == Humidifier_Steam_Gas ) {
				ModuleObjectType = "gas";
			}
			if ( NomCapVol == AutoSize ) {
				IsAutoSize = true;
			}
			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) {	// Hardsize with no sizing run
					HardSizeNoDesRun = true;
					if ( NomCapVol > 0.0 ) {
						ReportSizingOutput( HumidifierType( HumType_Code ), Name, "User-Specified Nominal Capacity Volume [m3/s]", NomCapVol );
					}
				} else {	// Sizing run done

					CheckZoneSizing( "Humidifier:SizeHumidifier", Name );
					AirDensity = FinalZoneSizing( CurZoneEqNum ).DesCoolDens;
					MassFlowDes = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow ) * AirDensity;
					InletHumRatDes = std::min( FinalZoneSizing( CurZoneEqNum ).OutHumRatAtHeatPeak, FinalZoneSizing( CurZoneEqNum ).OutHumRatAtCoolPeak );
					OutletHumRatDes = std::max( FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak, FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak );
				}
			} else if ( CurSysNum > 0 ) {
				if ( !IsAutoSize && !SysSizingRunDone ) {
					HardSizeNoDesRun = true;
					if ( NomCapVol > 0.0 ) {
						ReportSizingOutput( HumidifierType( HumType_Code ), Name, "User-Specified Nominal Capacity Volume [m3/s]", NomCapVol );
					}
				} else {
					CheckSysSizing( "Humidifier:SizeHumidifier", Name );
					if ( CurOASysNum > 0 ) {
						// size to outdoor air volume flow rate if available
						if ( FinalSysSizing( CurSysNum ).DesOutAirVolFlow > 0.0 ) {
							AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, OutDryBulbTemp, OutHumRat, CalledFrom );
							MassFlowDes = FinalSysSizing( CurSysNum ).DesOutAirVolFlow * AirDensity;
							InletHumRatDes = std::min( FinalSysSizing( CurSysNum ).OutHumRatAtCoolPeak, FinalSysSizing( CurSysNum ).HeatOutHumRat );
							OutletHumRatDes = std::max( FinalSysSizing( CurSysNum ).CoolSupHumRat, FinalSysSizing( CurSysNum ).HeatSupHumRat );
						} else {	// ELSE size to supply air duct flow rate
							auto const SELECT_CASE_var( CurDuctType );
							if ( SELECT_CASE_var == Main ) {
								AirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else if ( SELECT_CASE_var == Cooling ) {
								AirVolFlow = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
							} else if ( SELECT_CASE_var == Heating ) {
								AirVolFlow = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
							} else if ( SELECT_CASE_var == Other ) {
								AirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							} else {
								AirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							}
							AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, FinalSysSizing( CurSysNum ).MixTempAtCoolPeak, FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak, CalledFrom );
							MassFlowDes = AirVolFlow * AirDensity;
							InletHumRatDes = min( FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak, FinalSysSizing( CurSysNum ).HeatMixHumRat);
							OutletHumRatDes = max( FinalSysSizing( CurSysNum ).CoolSupHumRat, FinalSysSizing( CurSysNum ).HeatSupHumRat);
						}
					} else {
						auto const SELECT_CASE_var( CurDuctType );
						if ( SELECT_CASE_var == Main ) {
							AirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						} else if ( SELECT_CASE_var == Cooling ) {
							AirVolFlow = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
						} else if ( SELECT_CASE_var == Heating ) {
							AirVolFlow = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
						} else if ( SELECT_CASE_var == Other ) {
							AirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						} else {
							AirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						}
						AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, FinalSysSizing( CurSysNum ).MixTempAtCoolPeak, FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak, CalledFrom );
						MassFlowDes = AirVolFlow * AirDensity;
						InletHumRatDes = std::min( FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak, FinalSysSizing( CurSysNum ).HeatMixHumRat );
						OutletHumRatDes = std::max( FinalSysSizing( CurSysNum ).CoolSupHumRat, FinalSysSizing( CurSysNum ).HeatSupHumRat);
					}
				}
			}

			if ( !HardSizeNoDesRun ) {
				NomCapVolDes = MassFlowDes * ( OutletHumRatDes - InletHumRatDes ) / RhoH2O( InitConvTemp );
				if ( NomCapVolDes < 0.0 ) NomCapVolDes = 0.0;	// No humidity demand

				if ( IsAutoSize ) {
					 NomCapVol = NomCapVolDes;
					ReportSizingOutput( HumidifierType( HumType_Code ), Name, "Design Size Nominal Capacity Volume [m3/s]", NomCapVolDes );
				} else {
					if ( NomCapVol > 0.0) {
						NomCapVolUser = NomCapVol;
						ReportSizingOutput( HumidifierType( HumType_Code ), Name, "Design Size Nominal Capacity Volume [m3/s]", NomCapVolDes, "User-Specified Nominal Capacity Volume [m3/s]", NomCapVolUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( NomCapVolDes - NomCapVolUser )/NomCapVolUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeHumidifier: Potential issue with equipment sizing for " + HumidifierType( HumType_Code ) + " = \"" + Name + "\"." );
								ShowContinueError( "User-Specified Nominal Capacity Volume of " + RoundSigDigits( NomCapVolUser, 2 ) + " [Wm3/s]" );
								ShowContinueError( "differs from Design Size Nominal Capacity Volume of " + RoundSigDigits( NomCapVolDes, 2 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}

			NomCap = RhoH2O( InitConvTemp ) * NomCapVol;
			RefrigerantIndex = FindRefrigerant( fluidNameSteam );
			WaterIndex = FindGlycol( fluidNameWater );
			SteamSatEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, TSteam, 1.0, RefrigerantIndex, CalledFrom );
			WaterSatEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, TSteam, 0.0, RefrigerantIndex, CalledFrom );
			WaterSpecHeatAvg = 0.5 * ( GetSpecificHeatGlycol( fluidNameWater, TSteam, WaterIndex, CalledFrom ) + GetSpecificHeatGlycol( fluidNameWater, Tref, WaterIndex, CalledFrom ) );
			NominalPower = NomCap * ( ( SteamSatEnthalpy - WaterSatEnthalpy ) + WaterSpecHeatAvg * ( TSteam - Tref ) );

			if ( NomPower == AutoSize ) {
				IsAutoSize = true;
			}

			if ( HumType_Code == Humidifier_Steam_Gas ) {

				if ( !IsAutoSize ) {
					// override user specified rated thermal efficiency
					if ( NomPower >= NominalPower ) {
						ThermalEffRated = NominalPower / NomPower;
					} else {
						ShowMessage( CalledFrom + ": capacity and thermal efficiency mismatch for " + HumidifierType( HumType_Code ) + " =\"" + Name + "\"." );
						ShowContinueError( "User-Specified Rated Gas Use Rate of " + RoundSigDigits( NomPower, 2 ) + " [W]" );
						ShowContinueError( "User-Specified or Autosized Rated Capacity of " + RoundSigDigits( NomCapVol, 2 ) + " [m3/s]" );
						ShowContinueError( "Rated Gas Use Rate at the Rated Capacity of " + RoundSigDigits( NomCapVol, 2 ) + " [m3/s]" + " must be greater than the ideal, i.e., 100% thermal efficiency gas use rate of " + RoundSigDigits( NomPowerDes, 2 ) + " [W]" );
						ShowContinueError( "Resize the Rated Gas Use Rate by dividing the ideal gas use rate with expected thermal efficiency. " );
						// Changing this from a hard-stop condition to just a limiting condition of eta=1.0
						//ErrorsFound = true;
						ThermalEffRated = 1.0;
					}
				} else {
					if ( ThermalEffRated > 0.0 ) {
						NominalPower = NominalPower / ThermalEffRated;
					}
				}

				// gas fired steam humidifier's nominal gas use rate is always autosized
				IsAutoSize = true;
			}

			NomPowerDes = NominalPower;
			if ( IsAutoSize ) {
				NomPower = NomPowerDes;
				ReportSizingOutput( HumidifierType( HumType_Code ), Name, "Design Size Rated Power [W]", NomPowerDes );
			} else {
				if ( NomPower >= 0.0 && NomCap > 0.0 ) {
					NomPowerUser = NomPower;
					ReportSizingOutput( HumidifierType( HumType_Code ), Name, "Design Size Rated Power [W]", NomPowerDes, "User-Specified Rated Power [W]", NomPowerUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( NomPowerDes - NomPowerUser ) / NomPowerUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeHumidifier: Potential issue with equipment sizing for " + HumidifierType( HumType_Code ) + " =\"" + Name + "\"." );
							ShowContinueError( "User-Specified Rated Power of " + RoundSigDigits( NomPowerUser, 2 ) + " [W]" );
							ShowContinueError( "differs from Design Size Rated Power of " + RoundSigDigits( NomPowerDes, 2 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
					if ( NomPower < NominalPower ) {
						ShowWarningError( HumidifierType( HumType_Code ) + ": specified Rated Power is less than nominal Rated Power for " + ModuleObjectType + " steam humidifier = " + Name + ". " );
						ShowContinueError( " specified Rated Power = " + RoundSigDigits( NomPower, 2 ) );
						ShowContinueError( " while expecting a minimum Rated Power = " + RoundSigDigits( NominalPower, 2 ) );
					}
				} else {
					ShowWarningError( HumidifierType( HumType_Code ) + ": specified nominal capacity is zero for " + ModuleObjectType + " steam humidifier = " + Name + ". " );
					ShowContinueError( " For zero nominal capacity humidifier the rated power is zero." );
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( CalledFrom + ": Mismatch was found in the Rated Gas Use Rate and Thermal Efficiency for gas fired steam humidifier = " + Name + ". " );
		}
	}

	void
	HumidifierData::ControlHumidifier(
		Real64 & WaterAddNeeded // moisture addition rate needed to meet minimum humidity ratio setpoint [kg/s]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       February 2015, B. Nigusse, FSEC, - transitioned the code to OO approach
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets the output required from the humidifier

		// METHODOLOGY EMPLOYED:
		// Uses a minimum humidity setpoint and water mass balance to calculate moisture addition needed

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyWFnTdbRhPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		static std::string const RoutineName( "ControlHumidifier" );

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool UnitOn; // unit on flag
		Real64 HumRatSatIn; // humidity ratio at saturation at the inlet temperature [kg H2O / kg dry air]

		UnitOn = true;
		if ( HumRatSet <= 0.0 ) UnitOn = false;
		if ( AirInMassFlowRate <= SmallMassFlow ) UnitOn = false;
		if ( GetCurrentScheduleValue( SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( AirInHumRat >= HumRatSet ) UnitOn = false;
		HumRatSatIn = PsyWFnTdbRhPb( AirInTemp, 1.0, OutBaroPress, RoutineName );
		if ( AirInHumRat >= HumRatSatIn ) UnitOn = false;
		if ( UnitOn ) {
			// AirMassFlowRate*AirInHumRat + WaterAddNeeded = AirMassFlowRate*HumRatSet
			WaterAddNeeded = AirInMassFlowRate * ( HumRatSet - AirInHumRat );
		} else {
			WaterAddNeeded = 0.0;
		}

	}

	void
	HumidifierData::CalcElecSteamHumidifier(
		Real64 const WaterAddNeeded // moisture addition rate set by controller [kg/s]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       February 2015, B. Nigusse, FSEC, - transitioned the code to OO approach
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the electricity consumption and the outlet conditions for an electric steam
		// humidifier, given the inlet conditions and the steam addition rate.

		// METHODOLOGY EMPLOYED:
		// Uses energy and mass balance as well as pschrometric relations.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit, page 4-112
		// 1997 ASHRAE Handbook Fundamentals, page 6.18

		// Using/Aliasing
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::RhoH2O;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcElecSteamHumidifier" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 HumRatSatOut; // humidity ratio at saturation at the outlet temperature [kg H2O / kg dry air]
		Real64 HumRatSatIn; // humidity ratio at saturation at the inlet temperature [kg H2O / kg dry air]
		Real64 WaterAddNeededMax; // moisture addition rate set by controller, limited by humidifier capacity
		Real64 WaterInEnthalpy; // enthalpy of the inlet steam [J/kg]
		Real64 HumRatSatApp; // the approximate humidity ratio where the line drawn between inlet and desired outlet conditions
		// crosses the saturation line.
		Real64 WaterDens; // density of liquid water [kg/m3]

		HumRatSatIn = PsyWFnTdbRhPb( AirInTemp, 1.0, OutBaroPress, RoutineName );
		HumRatSatOut = 0.0;
		HumRatSatApp = 0.0;
		WaterInEnthalpy = 2676125.0; // At 100 C
		WaterDens = RhoH2O( InitConvTemp );
		WaterAddNeededMax = min( WaterAddNeeded, NomCap );
		if ( WaterAddNeededMax > 0.0 ) {
			//   ma*W1 + mw = ma*W2
			//   ma*h1 + mw*hw = ma*h2
			// where ma is air mass flow rate; h1,W1 are the inlet enthalpy and humidity ratio; h2 and W2 are
			// the outlet enthalpy and humidity ratio; mw is the steam mass flow rate; hw is the steam enthalpy.
			// Setting mw equal to the desired water addition rate, use the above 2 equations to calculate the
			// outlet conditions
			AirOutEnthalpy = ( AirInMassFlowRate * AirInEnthalpy + WaterAddNeededMax * WaterInEnthalpy ) / AirInMassFlowRate;
			AirOutHumRat = ( AirInMassFlowRate * AirInHumRat + WaterAddNeededMax ) / AirInMassFlowRate;
			AirOutTemp = PsyTdbFnHW( AirOutEnthalpy, AirOutHumRat );
			HumRatSatOut = PsyWFnTdbRhPb( AirOutTemp, 1.0, OutBaroPress, RoutineName );
			if ( AirOutHumRat <= HumRatSatOut ) {
				// If the outlet condition is below the saturation curve, the desired moisture addition rate can be met.
				WaterAdd = WaterAddNeededMax;
			} else {
				// The desired moisture addition rate results in an outlet state above the saturation curve. We need to
				// find the point where the line drawn between state 1 (inlet) and state 2 (our desired outlet) crosses
				// the saturation curve. This will be the new outlet condition. Rather than iterate to obtain this point,
				// we find it approximately by solving for the point where 2 lines cross: the first drawn from
				// state 1 to state 2, the second from T1, W1s to T2, W2s; where T1 is the inlet temperature, W1s is
				// the humidity ratio at saturation at temperature T1; and T2 is the desired outlet temperature, W2s
				// is the humidity ratio at saturation at temperature T2. The 2 lines are given by the equations:
				//   W = W1 + ((W2-W1)/(T2-T1))*(T-T1)
				//   W = W1s + ((W2s-W1s)/(T2-T1))*(T-T1)
				// Solving for the point where the line cross (T3,W3):
				//   W3 = W1 + ((W2-W1)*(W1s-W1))/(W2-W2s + W1s-W1)
				//   T3 = T1 + (W3-W1)*((T2-T1)/(W2-W1))  ! "T1 +" added by Shirey 8/12/04  That's correct! [WFB 9/29/2004]
				HumRatSatApp = AirInHumRat + ( AirOutHumRat - AirInHumRat ) * ( HumRatSatIn - AirInHumRat ) / ( AirOutHumRat - HumRatSatOut + HumRatSatIn - AirInHumRat );
				AirOutTemp = AirInTemp + ( HumRatSatApp - AirInHumRat ) * ( ( AirOutTemp - AirInTemp ) / ( AirOutHumRat - AirInHumRat ) );
				// This point isn't quite on the saturation curve since we made a linear approximation of the curve,
				// but the temperature should be very close to the correct outlet temperature. We will use this temperature
				// as the outlet temperature and move to the saturation curve for the outlet humidity and enthalpy
				AirOutHumRat = PsyWFnTdbRhPb( AirOutTemp, 1.0, OutBaroPress, RoutineName );
				AirOutEnthalpy = PsyHFnTdbW( AirOutTemp, AirOutHumRat );
				WaterAdd = AirInMassFlowRate * ( AirOutHumRat - AirInHumRat );
			}

		} else {
			WaterAdd = 0.0;
			AirOutEnthalpy = AirInEnthalpy;
			AirOutTemp = AirInTemp;
			AirOutHumRat = AirInHumRat;
		}
		if ( WaterAdd > 0.0 ) {
			ElecUseRate = ( WaterAdd / NomCap ) * NomPower + FanPower + StandbyPower;
		} else if ( GetCurrentScheduleValue( SchedPtr ) > 0.0 ) {
			ElecUseRate = StandbyPower;
		} else {
			ElecUseRate = 0.0;
		}
		WaterConsRate = WaterAdd / WaterDens;
		AirOutMassFlowRate = AirInMassFlowRate;
}

	void
	HumidifierData::CalcGasSteamHumidifier(
		Real64 const WaterAddNeeded // moisture addition rate set by controller [kg/s]
		) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   February 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the gas consumption and the outlet conditions for a gas fired steam
		// humidifier, given the inlet conditions and the steam addition rate.

		// METHODOLOGY EMPLOYED:
		// Uses energy and mass balance as well as pschrometric relations. Adopted
		// from routine CalcElecSteamHumidifier by Fred Buhl

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::RhoH2O;
		using CurveManager::CurveValue;

		using DataEnvironment::WaterMainsTemp;
		using DataWater::WaterStorage;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::FindGlycol;
		using FluidProperties::FindRefrigerant;


		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcGasSteamHumidifier" );
		Real64 const TSteam( 100.0 ); // saturated steam temperatur generated by Humidifier [C]

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 HumRatSatOut; // humidity ratio at saturation at the outlet temperature [kg H2O / kg dry air]
		Real64 HumRatSatIn; // humidity ratio at saturation at the inlet temperature [kg H2O / kg dry air]
		Real64 WaterAddNeededMax; // moisture addition rate set by controller, limited by humidifier capacity
		Real64 WaterInEnthalpy; // enthalpy of the inlet steam [J/kg]
		Real64 HumRatSatApp; // the approximate humidity ratio where the line drawn between inlet and desired outlet conditions
		// crosses the saturation line.
		Real64 WaterDens; // density of liquid water [kg/m3]
		Real64 ThermEffCurveOutput( 0 ); // thermal efficiency modifier normalized curve output value [-]
		Real64 PartLoadRatio; // gas fired humidifier part load ratio [-]
		Real64 GasUseRateAtRatedEff( 0 ); // gas use rate at rated thermal efficiency [W]
		Real64 WaterSpecHeatAvg; // specific heat of water [J/kgK]
		Real64 SteamSatEnthalpy; // enthalpy of saturated steam at 100C [J/kg]
		Real64 WaterSatEnthalpy; // enthalpy of saturated water at 100C [J/kg]
		Real64 Tref; // humidifier entering water temperature [C]
		int RefrigerantIndex; // refiferant index
		int WaterIndex; // fluid type index

		HumRatSatIn = PsyWFnTdbRhPb( AirInTemp, 1.0, OutBaroPress, RoutineName );
		HumRatSatOut = 0.0;
		HumRatSatApp = 0.0;
		WaterInEnthalpy = 2676125.0; // At 100 C
		WaterDens = RhoH2O( InitConvTemp );
		WaterAddNeededMax = min( WaterAddNeeded, NomCap );
		if ( WaterAddNeededMax > 0.0 ) {
			//   ma*W1 + mw = ma*W2
			//   ma*h1 + mw*hw = ma*h2
			// where ma is air mass flow rate; h1,W1 are the inlet enthalpy and humidity ratio; h2 and W2 are
			// the outlet enthalpy and humidity ratio; mw is the steam mass flow rate; hw is the steam enthalpy.
			// Setting mw equal to the desired water addition rate, use the above 2 equations to calculate the
			// outlet conditions
			AirOutEnthalpy = ( AirInMassFlowRate * AirInEnthalpy + WaterAddNeededMax * WaterInEnthalpy ) / AirInMassFlowRate;
			AirOutHumRat = ( AirInMassFlowRate * AirInHumRat + WaterAddNeededMax ) / AirInMassFlowRate;
			AirOutTemp = PsyTdbFnHW( AirOutEnthalpy, AirOutHumRat );
			HumRatSatOut = PsyWFnTdbRhPb( AirOutTemp, 1.0, OutBaroPress, RoutineName );
			if ( AirOutHumRat <= HumRatSatOut ) {
				// If the outlet condition is below the saturation curve, the desired moisture addition rate can be met.
				WaterAdd = WaterAddNeededMax;
			} else {
				// The desired moisture addition rate results in an outlet state above the saturation curve. We need to
				// find the point where the line drawn between state 1 (inlet) and state 2 (our desired outlet) crosses
				// the saturation curve. This will be the new outlet condition. Rather than iterate to obtain this point,
				// we find it approximately by solving for the point where 2 lines cross: the first drawn from
				// state 1 to state 2, the second from T1, W1s to T2, W2s; where T1 is the inlet temperature, W1s is
				// the humidity ratio at saturation at temperature T1; and T2 is the desired outlet temperature, W2s
				// is the humidity ratio at saturation at temperature T2. The 2 lines are given by the equations:
				//   W = W1 + ((W2-W1)/(T2-T1))*(T-T1)
				//   W = W1s + ((W2s-W1s)/(T2-T1))*(T-T1)
				// Solving for the point where the line cross (T3,W3):
				//   W3 = W1 + ((W2-W1)*(W1s-W1))/(W2-W2s + W1s-W1)
				//   T3 = T1 + (W3-W1)*((T2-T1)/(W2-W1))  ! "T1 +" added by Shirey 8/12/04  That's correct! [WFB 9/29/2004]
				HumRatSatApp = AirInHumRat + ( AirOutHumRat - AirInHumRat ) * ( HumRatSatIn - AirInHumRat ) / ( AirOutHumRat - HumRatSatOut + HumRatSatIn - AirInHumRat );
				AirOutTemp = AirInTemp + ( HumRatSatApp - AirInHumRat ) * ( ( AirOutTemp - AirInTemp ) / ( AirOutHumRat - AirInHumRat ) );
				// This point isn't quite on the saturation curve since we made a linear approximation of the curve,
				// but the temperature should be very close to the correct outlet temperature. We will use this temperature
				// as the outlet temperature and move to the saturation curve for the outlet humidity and enthalpy
				AirOutHumRat = PsyWFnTdbRhPb( AirOutTemp, 1.0, OutBaroPress, RoutineName );
				AirOutEnthalpy = PsyHFnTdbW( AirOutTemp, AirOutHumRat );
				WaterAdd = AirInMassFlowRate * ( AirOutHumRat - AirInHumRat );
			}

		} else {
			WaterAdd = 0.0;
			AirOutEnthalpy = AirInEnthalpy;
			AirOutTemp = AirInTemp;
			AirOutHumRat = AirInHumRat;
		}
		if ( WaterAdd > 0.0 ) {
			if ( InletWaterTempOption == FixedInletWaterTemperature ) {
				GasUseRateAtRatedEff = ( WaterAdd / NomCap ) * NomPower;
			} else if ( InletWaterTempOption == VariableInletWaterTemperature ) {
				if ( SuppliedByWaterSystem ) { // use water use storage tank supply temperaure
					 CurMakeupWaterTemp = WaterStorage( WaterTankID ).TwaterSupply( TankSupplyID );
				} else { // use water main temperaure
					 CurMakeupWaterTemp = WaterMainsTemp;
				}
				Tref = CurMakeupWaterTemp;
				RefrigerantIndex = FindRefrigerant( fluidNameSteam );
				WaterIndex = FindGlycol( fluidNameWater );
				SteamSatEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, TSteam, 1.0, RefrigerantIndex, RoutineName );
				WaterSatEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, TSteam, 0.0, RefrigerantIndex, RoutineName );
				WaterSpecHeatAvg = 0.5 * ( GetSpecificHeatGlycol( fluidNameWater, TSteam, WaterIndex, RoutineName ) + GetSpecificHeatGlycol( fluidNameWater, Tref, WaterIndex, RoutineName ) );
				GasUseRateAtRatedEff = WaterAdd * ( ( SteamSatEnthalpy - WaterSatEnthalpy ) + WaterSpecHeatAvg * ( TSteam - Tref ) ) / ThermalEffRated;
			}
			PartLoadRatio = GasUseRateAtRatedEff / NomPower;
			if ( EfficiencyCurvePtr > 0 ) { // calculate normalized thermal efficiency based on curve object type
				if ( EfficiencyCurveType == Linear || EfficiencyCurveType == Quadratic || EfficiencyCurveType == Cubic ) {
					ThermEffCurveOutput = CurveValue( EfficiencyCurvePtr, PartLoadRatio );
				}
			} else {
				ThermEffCurveOutput = 1.0;
			}
			ThermalEff = ThermalEffRated * ThermEffCurveOutput;
			if ( ThermEffCurveOutput != 0.0 ) {
				GasUseRate = GasUseRateAtRatedEff / ThermEffCurveOutput;
			}
			AuxElecUseRate = FanPower + StandbyPower;

		} else if ( GetCurrentScheduleValue( SchedPtr ) > 0.0 ) {
			 AuxElecUseRate = StandbyPower;
		} else {
			 AuxElecUseRate = 0.0;
		}
		WaterConsRate = WaterAdd / WaterDens;
		AirOutMassFlowRate = AirInMassFlowRate;
	}

	void
	HumidifierData::UpdateReportWaterSystem() // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Aug. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect water system calculations , update and report them

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataWater::WaterStorage;
		using DataGlobals::SecInHour;
		using DataGlobals::BeginTimeStepFlag;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AvailTankVdot;
		Real64 TankSupplyVdot;
		Real64 StarvedVdot;

		// set demand request in WaterStorage if needed.
		if ( SuppliedByWaterSystem ) {
			WaterStorage( WaterTankID ).VdotRequestDemand( WaterTankDemandARRID ) = WaterConsRate;

			AvailTankVdot = WaterStorage( WaterTankID ).VdotAvailDemand( WaterTankDemandARRID ); // check what tank can currently provide

			StarvedVdot = 0.0;
			TankSupplyVdot = WaterConsRate; // init
			if ( ( AvailTankVdot < WaterConsRate ) && ( ! ( BeginTimeStepFlag ) ) ) { // calculate starved flow
				StarvedVdot = WaterConsRate - AvailTankVdot;
				TankSupplyVdot = AvailTankVdot;
			}

			TankSupplyVol = TankSupplyVdot * ( TimeStepSys * SecInHour );
			StarvedSupplyVdot = StarvedVdot;
			StarvedSupplyVol = StarvedVdot * ( TimeStepSys * SecInHour );

		}

	}

	void
	HumidifierData::UpdateHumidifier() // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Moves humidifier output to the outlet nodes.

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Set the outlet air node of the humidifier
		Node( AirOutNode ).MassFlowRate = AirOutMassFlowRate;
		Node( AirOutNode ).Temp = AirOutTemp;
		Node( AirOutNode ).HumRat = AirOutHumRat;
		Node( AirOutNode ).Enthalpy = AirOutEnthalpy;

		// Set the outlet nodes for properties that just pass through & not used
		Node( AirOutNode ).Quality = Node( AirInNode ).Quality;
		Node( AirOutNode ).Press = Node( AirInNode ).Press;
		Node( AirOutNode ).MassFlowRateMin = Node( AirInNode ).MassFlowRateMin;
		Node( AirOutNode ).MassFlowRateMax = Node( AirInNode ).MassFlowRateMax;
		Node( AirOutNode ).MassFlowRateMinAvail = Node( AirInNode ).MassFlowRateMinAvail;
		Node( AirOutNode ).MassFlowRateMaxAvail = Node( AirInNode ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutNode ).CO2 = Node( AirInNode ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutNode ).GenContam = Node( AirInNode ).GenContam;
		}

	}

	void
	HumidifierData::ReportHumidifier() // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fill remaining report variables

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

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

		ElecUseEnergy = ElecUseRate * TimeStepSys * SecInHour;
		WaterCons = WaterConsRate * TimeStepSys * SecInHour;
		GasUseEnergy = GasUseRate * TimeStepSys * SecInHour;
		AuxElecUseEnergy = AuxElecUseRate * TimeStepSys * SecInHour;
	}

} // Humidifiers

} // EnergyPlus
