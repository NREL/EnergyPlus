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

// EnergyPlus Headers
#include <HVACDXHeatPumpSystem.hh>
#include <BranchNodeConnections.hh>
#include <DataAirLoop.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <VariableSpeedCoils.hh>

namespace EnergyPlus {

namespace HVACDXHeatPumpSystem {
	// Module containing the DXHeatPumpSystem simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Brent Griffith (derived from HVACDXSystem.cc by R.Liesen)
	//       DATE WRITTEN   May 2011
	//                      Feb 2013, Bo Shen, Oak Ridge National Lab
	//                      Add Coil:Heating:DX:VariableSpeed
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the DX Heat Pump System System Component
	// this wraps heat pump air-heating coils in coil-only wrapper with no fans.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	//MODULE PARAMETER DEFINITIONS
	Real64 const MinAirMassFlow( 0.001 );
	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumDXHeatPumpSystems( 0 ); // The Number of DXHeatPumpSystems found in the Input
	bool EconomizerFlag( false ); // holds air loop economizer status

	// Make this type allocatable
	Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Update routine to check convergence and update nodes

	// Object Data
	Array1D< DXHeatPumpSystemStruct > DXHeatPumpSystem;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	clear_state()
	{
		NumDXHeatPumpSystems = 0;
		EconomizerFlag = false;
		CheckEquipName.deallocate();
		DXHeatPumpSystem.deallocate();
	}

	void
	SimDXHeatPumpSystem(
		std::string const & DXHeatPumpSystemName, // Name of DXSystem:Airloop object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to CoilSystem:Heating:DX object
		Optional_int_const OAUnitNum, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
		Optional< Real64 > QTotOut // the total cooling output of unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith (derived from HVACDXSystem.cc by R.Liesen)
		//       DATE WRITTEN   May 2011
		//                      Feb 2013, Bo Shen, Oak Ridge National Lab
		//                      Add Coil:Heating:DX:VariableSpeed

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages DXHeatPumpSystem component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::SimDXCoil;
		using General::TrimSigDigits;
		using DataAirLoop::AirLoopControlInfo;
		using InputProcessor::FindItemInList;
		using VariableSpeedCoils::SimVariableSpeedCoils;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS:
		// na

		// FLOW:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of CoilSystem:Heating:DX object
		int DXSystemNum; // Index to CoilSystem:Heating:DX object
		static bool GetInputFlag( true ); // Flag to get input only once
		Real64 AirMassFlow; // DX System air mass flow rate
		int InletNodeNum; // DX System inlet node number
		int OutletNodeNum; // DX System outlet node number
		//local variables for calling variable speed coil
		static Real64 QZnReq( 0.001 ); // Zone load (W), input to variable-speed DX coil
		static Real64 QLatReq( 0.0 ); // Zone latent load, input to variable-speed DX coil
		static Real64 MaxONOFFCyclesperHour( 4.0 ); // Maximum cycling rate of heat pump [cycles/hr]
		static Real64 HPTimeConstant( 0.0 ); // Heat pump time constant [s]
		static Real64 FanDelayTime( 0.0 ); // Fan delay time, time delay for the HP's fan to
		static Real64 OnOffAirFlowRatio( 1.0 ); // ratio of compressor on flow to average flow over time step

		// Obtains and Allocates DX Cooling System related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			//Get the DXCoolingSystem input
			GetDXHeatPumpSystemInput();
			GetInputFlag = false;
		}

		// Find the correct DXSystemNumber
		if ( CompIndex == 0 ) {
			DXSystemNum = FindItemInList( DXHeatPumpSystemName, DXHeatPumpSystem );
			if ( DXSystemNum == 0 ) {
				ShowFatalError( "SimDXHeatPumpSystem: DXUnit not found=" + DXHeatPumpSystemName );
			}
			CompIndex = DXSystemNum;
		} else {
			DXSystemNum = CompIndex;
			if ( DXSystemNum > NumDXHeatPumpSystems || DXSystemNum < 1 ) {
				ShowFatalError( "SimDXHeatPumpSystem:  Invalid CompIndex passed=" + TrimSigDigits( DXSystemNum ) + ", Number of DX Units=" + TrimSigDigits( NumDXHeatPumpSystems ) + ", DX Unit name=" + DXHeatPumpSystemName );
			}
			if ( CheckEquipName( DXSystemNum ) ) {
				if ( DXHeatPumpSystemName != DXHeatPumpSystem( DXSystemNum ).Name ) {
					ShowFatalError( "SimDXHeatPumpSystem: Invalid CompIndex passed=" + TrimSigDigits( DXSystemNum ) + ", DX Unit name=" + DXHeatPumpSystemName + ", stored DX Unit Name for that index=" + DXHeatPumpSystem( DXSystemNum ).Name );
				}
				CheckEquipName( DXSystemNum ) = false;
			}
		}

		if ( present( OAUnitNum ) ) {
			InitDXHeatPumpSystem( DXSystemNum, AirLoopNum, OAUnitNum, OAUCoilOutTemp );
		} else {
			InitDXHeatPumpSystem( DXSystemNum, AirLoopNum );
		}

		//Call the series of components that simulate a DX Heating System
		// Control the DX Heating System
		ControlDXHeatingSystem( DXSystemNum, FirstHVACIteration );

		// simulate DX Heating System
		CompName = DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilName;

		{ auto const SELECT_CASE_var( DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

			SimDXCoil( CompName, On, FirstHVACIteration, DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex, DXHeatPumpSystem( DXSystemNum ).FanOpMode, DXHeatPumpSystem( DXSystemNum ).PartLoadFrac );

		} else if ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) { // Coil:Heating:DX:VariableSpeed
			SimVariableSpeedCoils( CompName, DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex, DXHeatPumpSystem( DXSystemNum ).FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, DXHeatPumpSystem( DXSystemNum ).PartLoadFrac, DXHeatPumpSystem( DXSystemNum ).SpeedNum, DXHeatPumpSystem( DXSystemNum ).SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

		} else {
			ShowFatalError( "SimDXCoolingSystem: Invalid DX Heating System/Coil=" + DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilType );

		}}
		// set econo lockout flag
		// set econo lockout flag
		if ( AirLoopNum != -1 ) { // IF the sysem is not an equipment of outdoor air unit

			if ( ( DXHeatPumpSystem( DXSystemNum ).PartLoadFrac > 0.0 ) && AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithCompressor ) {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithCompressor = true;
			} else {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithCompressor = false;
			}
		}

		if ( present( QTotOut ) ) {
			InletNodeNum = DXHeatPumpSystem( DXSystemNum ).DXHeatPumpCoilInletNodeNum;
			OutletNodeNum = DXHeatPumpSystem( DXSystemNum ).DXHeatPumpCoilOutletNodeNum;
			AirMassFlow = Node( OutletNodeNum ).MassFlowRate;
			QTotOut = AirMassFlow * ( Node( InletNodeNum ).Enthalpy - Node( OutletNodeNum ).Enthalpy );
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDXHeatPumpSystemInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith (derived from HVACDXSystem.cc by R.Liesen)
		//       DATE WRITTEN   May 2011
		//                      Feb 2013, Bo Shen, Oak Ridge National Lab
		//                      Add Coil:Heating:DX:VariableSpeed
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for system and stores it in System data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using BranchNodeConnections::TestCompSet;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using namespace DataIPShortCuts;
		using DXCoils::GetCoilInletNode;
		using DXCoils::GetCoilOutletNode;
		using DXCoils::SetCoilSystemHeatingDXFlag;
		using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
		using VariableSpeedCoils::GetCoilOutletNodeVariableSpeed;

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
		int NumAlphas;
		int NumNums;
		int IOStat;
		static std::string const RoutineName( "GetDXHeatPumpSystemInput: " ); // include trailing blank space
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int DXHeatSysNum;
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file

		// Flow

		CurrentModuleObject = "CoilSystem:Heating:DX";
		NumDXHeatPumpSystems = GetNumObjectsFound( CurrentModuleObject );

		DXHeatPumpSystem.allocate( NumDXHeatPumpSystems );
		CheckEquipName.dimension( NumDXHeatPumpSystems, true );

		GetObjectDefMaxArgs( "CoilSystem:Heating:DX", TotalArgs, NumAlphas, NumNums );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNums );
		Numbers.dimension( NumNums, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNums, true );

		// Get the data for the DX Cooling System
		for ( DXHeatSysNum = 1; DXHeatSysNum <= NumDXHeatPumpSystems; ++DXHeatSysNum ) {

			GetObjectItem( CurrentModuleObject, DXHeatSysNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXHeatPumpSystem, DXHeatSysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpSystemType = CurrentModuleObject; // push Object Name into data array
			DXHeatPumpSystem( DXHeatSysNum ).Name = Alphas( 1 );
			if ( lAlphaBlanks( 2 ) ) {
				DXHeatPumpSystem( DXHeatSysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXHeatPumpSystem( DXHeatSysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( DXHeatPumpSystem( DXHeatSysNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			if ( SameString( Alphas( 3 ), "Coil:Heating:DX:SingleSpeed" ) ) {

				DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType = Alphas( 3 );
				DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType_Num = CoilDX_HeatingEmpirical;

				DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName = Alphas( 4 );
			} else if ( SameString( Alphas( 3 ), "Coil:Heating:DX:VariableSpeed" ) ) {

				DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType = Alphas( 3 );
				DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType_Num = Coil_HeatingAirToAirVariableSpeed;

				DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName = Alphas( 4 );

			} else {
				ShowSevereError( "Invalid entry for " + cAlphaFields( 3 ) + " :" + Alphas( 3 ) );
				ShowContinueError( "In " + CurrentModuleObject + "=\"" + DXHeatPumpSystem( DXHeatSysNum ).Name + "\"." );
				ErrorsFound = true;
			}

			if ( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) {
				DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilInletNodeNum = GetCoilInletNodeVariableSpeed( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName, ErrorsFound );
				DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilOutletNodeNum = GetCoilOutletNodeVariableSpeed( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName, ErrorsFound );
			} else {
				DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilInletNodeNum = GetCoilInletNode( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName, ErrorsFound );

				DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilOutletNodeNum = GetCoilOutletNode( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName, ErrorsFound );
			}

			DXHeatPumpSystem( DXHeatSysNum ).DXSystemControlNodeNum = DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilOutletNodeNum;

			TestCompSet( CurrentModuleObject, DXHeatPumpSystem( DXHeatSysNum ).Name, NodeID( DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilInletNodeNum ), NodeID( DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilOutletNodeNum ), "Air Nodes" );

			ValidateComponent( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName, IsNotOK, CurrentModuleObject );
			if ( IsNotOK ) {
				ShowContinueError( "In " + CurrentModuleObject + " = \"" + DXHeatPumpSystem( DXHeatSysNum ).Name + "\"." );
				ErrorsFound = true;
			}

			SetUpCompSets( DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpSystemType, DXHeatPumpSystem( DXHeatSysNum ).Name, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName, NodeID( DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilInletNodeNum ), NodeID( DXHeatPumpSystem( DXHeatSysNum ).DXHeatPumpCoilOutletNodeNum ) );

			// Supply air fan operating mode defaulted to constant fan cycling coil/compressor
			DXHeatPumpSystem( DXHeatSysNum ).FanOpMode = ContFanCycCoil;

			if ( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType_Num != Coil_HeatingAirToAirVariableSpeed ) {
				SetCoilSystemHeatingDXFlag( DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilType, DXHeatPumpSystem( DXHeatSysNum ).HeatPumpCoilName );
			}

		} //End of the DX System Loop

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

		for ( DXHeatSysNum = 1; DXHeatSysNum <= NumDXHeatPumpSystems; ++DXHeatSysNum ) {
			// Setup Report variables for the DXHeatingSystem that is not reported in the components themselves
			SetupOutputVariable( "Coil System Part Load Ratio []", DXHeatPumpSystem( DXHeatSysNum ).PartLoadFrac, "System", "Average", DXHeatPumpSystem( DXHeatSysNum ).Name );
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning of Initialization subroutines for the Module
	// *****************************************************************************

	void
	InitDXHeatPumpSystem(
		int const DXSystemNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		Optional_int_const OAUnitNum, // number of the current outdoor air unit being simulated
		Optional< Real64 const > OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith (derived from HVACDXSystem.cc by R.Liesen)
		//       DATE WRITTEN   May 2011
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the DX heat pump Systems.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::DoSetPointTest;
		using DataAirLoop::AirLoopControlInfo;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ControlNode; // control node number
		int DXSysIndex;
		//  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.
		static bool MySetPointCheckFlag( true );
		int OutdoorAirUnitNum; // "ONLY" for ZoneHVAC:OutdoorAirUnit
		Real64 OAUCoilOutletTemp; // "ONLY" for zoneHVAC:OutdoorAirUnit
		// FLOW:

		//  IF (MyOneTimeFlag) THEN
		//    MyOneTimeFlag = .FALSE.
		//  END IF
		if ( present( OAUnitNum ) ) { // This Dx system is component of ZoneHVAC:OutdoorAirUnit
			OutdoorAirUnitNum = OAUnitNum;
			OAUCoilOutletTemp = OAUCoilOutTemp;
		}

		if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
			for ( DXSysIndex = 1; DXSysIndex <= NumDXHeatPumpSystems; ++DXSysIndex ) {
				ControlNode = DXHeatPumpSystem( DXSysIndex ).DXSystemControlNodeNum;
				if ( ControlNode > 0 ) {
					if ( AirLoopNum == -1 ) { // Outdoor Air Unit
						Node( ControlNode ).TempSetPoint = OAUCoilOutletTemp; // Set the coil outlet temperature
					} else if ( AirLoopNum != -1 ) { // Not an outdoor air unit

						if ( Node( ControlNode ).TempSetPoint == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( DXHeatPumpSystem( DXSysIndex ).DXHeatPumpSystemType + ": Missing temperature setpoint for DX unit= " + DXHeatPumpSystem( DXSysIndex ).Name );
								ShowContinueError( "  use a Set Point Manager to establish a setpoint at the unit control node." );
								SetPointErrorFlag = true;
							} else {
								CheckIfNodeSetPointManagedByEMS( ControlNode, iTemperatureSetPoint, SetPointErrorFlag );
								if ( SetPointErrorFlag ) {
									ShowSevereError( DXHeatPumpSystem( DXSysIndex ).DXHeatPumpSystemType + ": Missing temperature setpoint for DX unit= " + DXHeatPumpSystem( DXSysIndex ).Name );
									ShowContinueError( "  use a Set Point Manager to establish a setpoint at the unit control node." );
									ShowContinueError( "  or use an EMS actuator to establish a temperature setpoint at the unit control node." );
								}
							}
						}
					}
				}
			}
			MySetPointCheckFlag = false;
		}

		// These initializations are done every iteration
		if ( AirLoopNum == -1 ) { // This IF-Then routine is just for ZoneHVAC:OUTDOORAIRUNIT

			DXHeatPumpSystem( DXSystemNum ).DesiredOutletTemp = OAUCoilOutletTemp;

		} else if ( AirLoopNum != -1 ) { // Not Outdoor Air Unit
			ControlNode = DXHeatPumpSystem( DXSystemNum ).DXSystemControlNodeNum;
			EconomizerFlag = AirLoopControlInfo( AirLoopNum ).EconoActive;
			DXHeatPumpSystem( DXSystemNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint;

		}
	}

	// End of Initialization subroutines for the Module
	// *****************************************************************************

	// Beginning of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

	void
	ControlDXHeatingSystem(
		int const DXSystemNum, // index to DXSystem
		bool const FirstHVACIteration // First HVAC iteration flag
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith (derived from ControlDXSystem by Richard Liesen)
		//       DATE WRITTEN   Jan 2012
		//       MODIFIED       Richard Raustad, FSEC Nov 2003
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add dehumidification controls and support for multimode DX coil
		//                      Jan 2008 R. Raustad, FSEC. Added coolreheat to all coil types
		//                      Feb 2013, Bo Shen, Oak Ridge National Lab
		//                      Add Coil:Heating:DX:VariableSpeed
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine updates the System outlet nodes.

		// METHODOLOGY EMPLOYED:
		//  Data is moved from the System data structure to the System outlet nodes.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using namespace ScheduleManager;
		using DataHVACGlobals::TempControlTol;
		using InputProcessor::FindItemInList;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTdpFnWPb;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DXCoils::SimDXCoil;
		using DXCoils::DXCoilOutletTemp;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.e-3 ); // Accuracy of solver result

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of the DX cooling coil
		Real64 NoOutput; // Sensible capacity (outlet - inlet) when the compressor is off
		Real64 FullOutput; // Sensible capacity (outlet - inlet) when the compressor is on
		Real64 ReqOutput; // Sensible capacity (outlet - inlet) required to meet load or set point temperature
		int InletNode; // Inlet node number of the DX cooling coil
		int OutletNode; // Outlet node number of the DX cooling coil
		int ControlNode; // The node number where a set point is placed to control the DX cooling coil
		Real64 PartLoadFrac; // The part-load fraction of the compressor

		Real64 DesOutTemp; // Desired outlet temperature of the DX cooling coil
		Real64 OutletTempDXCoil; // Actual outlet temperature of the DX cooling coil

		int SolFla; // Flag of solver
		Array1D< Real64 > Par( 5 ); // Parameter array passed to solver
		bool SensibleLoad; // True if there is a sensible cooling load on this system
		int FanOpMode; // Supply air fan operating mode
		//added variables to call variable speed DX coils
		int SpeedNum; // speed number of variable speed DX cooling coil
		Real64 QZnReq; // Zone load (W), input to variable-speed DX coil
		Real64 QLatReq; // Zone latent load, input to variable-speed DX coil
		Real64 MaxONOFFCyclesperHour; // Maximum cycling rate of heat pump [cycles/hr]
		Real64 HPTimeConstant; // Heat pump time constant [s]
		Real64 FanDelayTime; // Fan delay time, time delay for the HP's fan to
		Real64 OnOffAirFlowRatio; // ratio of compressor on flow to average flow over time step
		Real64 TempSpeedOut; // output at one speed level
		Real64 TempSpeedReqst; // request capacity at one speed level
		int NumOfSpeeds; // maximum number of speed
		int VSCoilIndex; // variable-speed coil index
		int I; // interation increment
		Real64 SpeedRatio; // speed ratio between two neighboring speeds

		// Set local variables
		// Retrieve the load on the controlled zone
		OutletNode = DXHeatPumpSystem( DXSystemNum ).DXHeatPumpCoilOutletNodeNum;
		InletNode = DXHeatPumpSystem( DXSystemNum ).DXHeatPumpCoilInletNodeNum;
		ControlNode = DXHeatPumpSystem( DXSystemNum ).DXSystemControlNodeNum;
		DesOutTemp = DXHeatPumpSystem( DXSystemNum ).DesiredOutletTemp;
		CompName = DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilName;
		FanOpMode = DXHeatPumpSystem( DXSystemNum ).FanOpMode;

		PartLoadFrac = 0.0;

		SensibleLoad = false;

		SpeedNum = 1;
		QZnReq = 0.0;
		QLatReq = 0.0;
		MaxONOFFCyclesperHour = 4.0; //default number
		HPTimeConstant = 0.0;
		FanDelayTime = 0.0;
		OnOffAirFlowRatio = 1.0;
		TempSpeedOut = 0.0;
		TempSpeedReqst = 0.0;
		NumOfSpeeds = 0;
		VSCoilIndex = 0;
		I = 1;
		SpeedRatio = 0.0;

		// If DXHeatingSystem is scheduled on and there is flow
		if ( ( GetCurrentScheduleValue( DXHeatPumpSystem( DXSystemNum ).SchedPtr ) > 0.0 ) && ( Node( InletNode ).MassFlowRate > MinAirMassFlow ) ) {

			// Determine if there is a sensible load on this system
			if ( ( Node( InletNode ).Temp < Node( ControlNode ).TempSetPoint ) && ( Node( InletNode ).Temp < DesOutTemp ) && ( std::abs( Node( InletNode ).Temp - DesOutTemp ) > TempControlTol ) ) SensibleLoad = true;

			// If DXHeatingSystem runs with a heating load then set PartLoadFrac on Heating System
			if ( SensibleLoad ) {
				{ auto const SELECT_CASE_var( DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilType_Num );

				if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) { // Coil:Heating:DX:SingleSpeed

					// Get no load result
					PartLoadFrac = 0.0;
					SimDXCoil( CompName, On, FirstHVACIteration, DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex, FanOpMode, PartLoadFrac );
					NoOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					// Get full load result
					PartLoadFrac = 1.0;
					SimDXCoil( CompName, On, FirstHVACIteration, DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex, FanOpMode, PartLoadFrac );

					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

					ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXHeatPumpSystem( DXSystemNum ).DesiredOutletTemp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

					//         IF NoOutput is higher than (more heating than required) or very near the ReqOutput, do not run the compressor
					if ( ( NoOutput - ReqOutput ) > Acc ) {
						PartLoadFrac = 0.0;
						//         If the FullOutput is greater than (insufficient heating) or very near the ReqOutput,
						//         run the compressor at PartLoadFrac = 1.
					} else if ( ( FullOutput - ReqOutput ) < Acc ) {
						PartLoadFrac = 1.0;
						//         Else find the PLR to meet the load
					} else {
						//           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above. If this temp is
						//           greater than the desired outlet temp, then run the compressor at PartLoadFrac = 1, otherwise find the operating PLR.
						OutletTempDXCoil = DXCoilOutletTemp( DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex );
						if ( OutletTempDXCoil < DesOutTemp ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex );
							Par( 2 ) = DesOutTemp;
							Par( 3 ) = 1.0; //OnOffAirFlowFrac assume = 1.0 for continuous fan dx system
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, DXHeatingCoilResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIter < 1 ) {
										++DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIter;
										ShowWarningError( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXHeatPumpSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
										ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
									} else {
										ShowRecurringWarningErrorAtEnd( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " \"" + DXHeatPumpSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								}
							} else if ( SolFla == -2 ) {
								PartLoadFrac = ReqOutput / FullOutput;
								if ( ! WarmupFlag ) {
									if ( DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFail < 1 ) {
										++DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFail;
										ShowWarningError( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXHeatPumpSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
									} else {
										ShowRecurringWarningErrorAtEnd( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " \"" + DXHeatPumpSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}

							}
						}
					}

					if ( PartLoadFrac > 1.0 ) {
						PartLoadFrac = 1.0;
					} else if ( PartLoadFrac < 0.0 ) {
						PartLoadFrac = 0.0;
					}

				} else if ( SELECT_CASE_var == Coil_HeatingAirToAirVariableSpeed ) {
					//variable-speed air-to-air heating coil, begin -------------------------
					// Get no load result
					PartLoadFrac = 0.0;
					SpeedNum = 1;
					QZnReq = 0.0;
					QLatReq = 0.0;
					MaxONOFFCyclesperHour = 4.0; //default number
					HPTimeConstant = 0.0;
					FanDelayTime = 0.0;
					OnOffAirFlowRatio = 1.0;
					SpeedRatio = 0.0;

					SimVariableSpeedCoils( CompName, DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

					VSCoilIndex = DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilIndex;
					NumOfSpeeds = VarSpeedCoil( VSCoilIndex ).NumOfSpeeds;

					NoOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					// Get full load result
					PartLoadFrac = 1.0;

					SpeedNum = NumOfSpeeds;
					SpeedRatio = 1.0;
					QZnReq = 0.001; //to indicate the coil is running
					SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

					ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXHeatPumpSystem( DXSystemNum ).DesiredOutletTemp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
					//         IF NoOutput is higher than (more heating than required) or very near the ReqOutput, do not run the compressor
					if ( ( NoOutput - ReqOutput ) > Acc ) {
						PartLoadFrac = 0.0;
						SpeedNum = 1;
						SpeedRatio = 0.0;
						//         If the FullOutput is greater than (insufficient heating) or very near the ReqOutput,
						//         run the compressor at PartLoadFrac = 1.
					} else if ( ( FullOutput - ReqOutput ) < Acc ) {
						PartLoadFrac = 1.0;
						SpeedNum = NumOfSpeeds;
						SpeedRatio = 1.0;
						//         Else find the PLR to meet the load
					} else {
						//           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above. If this temp is
						//           greater than the desired outlet temp, then run the compressor at PartLoadFrac = 1, otherwise find the operating PLR.
						OutletTempDXCoil = VarSpeedCoil( VSCoilIndex ).OutletAirDBTemp;
						if ( OutletTempDXCoil < DesOutTemp ) {
							PartLoadFrac = 1.0;
							SpeedNum = NumOfSpeeds;
							SpeedRatio = 1.0;
						} else {
							PartLoadFrac = 1.0;
							SpeedNum = 1;
							SpeedRatio = 1.0;
							QZnReq = 0.001; //to indicate the coil is running
							SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

							TempSpeedOut = VarSpeedCoil( VSCoilIndex ).OutletAirDBTemp;

							if ( ( TempSpeedOut - DesOutTemp ) < Acc ) {
								// Check to see which speed to meet the load
								PartLoadFrac = 1.0;
								SpeedRatio = 1.0;
								for ( I = 2; I <= NumOfSpeeds; ++I ) {
									SpeedNum = I;
									SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

									TempSpeedOut = VarSpeedCoil( VSCoilIndex ).OutletAirDBTemp;

									if ( ( TempSpeedOut - DesOutTemp ) > Acc ) {
										SpeedNum = I;
										break;
									}
								}
								Par( 1 ) = double( VSCoilIndex );
								Par( 2 ) = DesOutTemp;
								Par( 5 ) = double( FanOpMode );
								Par( 3 ) = double( SpeedNum );
								SolveRegulaFalsi( Acc, MaxIte, SolFla, SpeedRatio, VSCoilSpeedResidual, 1.0e-10, 1.0, Par );

								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIter < 1 ) {
											++DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIter;
											ShowWarningError( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXHeatPumpSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										} else {
											ShowRecurringWarningErrorAtEnd( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " \"" + DXHeatPumpSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
										}
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = ReqOutput / FullOutput;
									if ( ! WarmupFlag ) {
										if ( DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFail < 1 ) {
											++DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFail;
											ShowWarningError( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXHeatPumpSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										} else {
											ShowRecurringWarningErrorAtEnd( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " \"" + DXHeatPumpSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
										}
									}

								}
							} else {
								Par( 1 ) = double( VSCoilIndex );
								Par( 2 ) = DesOutTemp;
								Par( 5 ) = double( FanOpMode );
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, VSCoilCyclingResidual, 1.0e-10, 1.0, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIter < 1 ) {
											++DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIter;
											ShowWarningError( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXHeatPumpSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										} else {
											ShowRecurringWarningErrorAtEnd( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " \"" + DXHeatPumpSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
										}
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = ReqOutput / FullOutput;
									if ( ! WarmupFlag ) {
										if ( DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFail < 1 ) {
											++DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFail;
											ShowWarningError( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXHeatPumpSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										} else {
											ShowRecurringWarningErrorAtEnd( DXHeatPumpSystem( DXSystemNum ).DXHeatPumpSystemType + " \"" + DXHeatPumpSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXHeatPumpSystem( DXSystemNum ).DXCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
										}
									}

								}
							}
						}
					}

					if ( PartLoadFrac > 1.0 ) {
						PartLoadFrac = 1.0;
					} else if ( PartLoadFrac < 0.0 ) {
						PartLoadFrac = 0.0;
					}

				} else {
					ShowFatalError( "ControlDXHeatingSystem: Invalid DXHeatPumpSystem coil type = " + DXHeatPumpSystem( DXSystemNum ).HeatPumpCoilType );

				}}
			} // End of cooling load type (sensible or latent) if block
		} // End of If DXheatingSystem is scheduled on and there is flow
		//Set the final results
		DXHeatPumpSystem( DXSystemNum ).PartLoadFrac = PartLoadFrac;
		DXHeatPumpSystem( DXSystemNum ).SpeedRatio = SpeedRatio;
		DXHeatPumpSystem( DXSystemNum ).SpeedNum = SpeedNum;

	}

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2006
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcDXHeatingCoil;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // Index of this coil
		Real64 OutletAirTemp; // Outlet air temperature [C]
		Real64 OnOffAirFlowFrac; // Ratio of compressor ON to compressor OFF air mass flow rate

		CoilIndex = int( Par( 1 ) );
		OnOffAirFlowFrac = Par( 3 );

		CalcDXHeatingCoil( CoilIndex, PartLoadFrac, ContFanCycCoil, OnOffAirFlowFrac );

		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	//******************************************************************************

	Real64
	VSCoilCyclingResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Feb, 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function, iterate part-load ratio
		//  compare the desired temperature value with exit temperature from a variable-speed heating coil

		// REFERENCES:

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		int FanOpMode; // Supply air fan operating mode
		static int SpeedNum( 1 ); // speed number of variable speed DX cooling coil
		static Real64 QZnReq( 0.001 ); // Zone load (W), input to variable-speed DX coil
		static Real64 QLatReq( 0.0 ); // Zone latent load, input to variable-speed DX coil
		static Real64 MaxONOFFCyclesperHour( 4.0 ); // Maximum cycling rate of heat pump [cycles/hr]
		static Real64 HPTimeConstant( 0.0 ); // Heat pump time constant [s]
		static Real64 FanDelayTime( 0.0 ); // Fan delay time, time delay for the HP's fan to
		static Real64 OnOffAirFlowRatio( 1.0 ); // ratio of compressor on flow to average flow over time step
		static Real64 SpeedRatio( 0.0 ); // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );

		SimVariableSpeedCoils( "", CoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadRatio, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

		OutletAirTemp = VarSpeedCoil( CoilIndex ).OutletAirDBTemp;
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;

	}

	//******************************************************************************

	Real64
	VSCoilSpeedResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Feb, 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function, iterate speed ratio
		//  compare the desired temperature value with exit temperature from a variable-speed heating coil

		// REFERENCES:

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		int FanOpMode; // Supply air fan operating mode
		static int SpeedNum( 1 ); // speed number of variable speed DX cooling coil
		static Real64 QZnReq( 0.001 ); // Zone load (W), input to variable-speed DX coil
		static Real64 QLatReq( 0.0 ); // Zone latent load, input to variable-speed DX coil
		static Real64 MaxONOFFCyclesperHour( 4.0 ); // Maximum cycling rate of heat pump [cycles/hr]
		static Real64 HPTimeConstant( 0.0 ); // Heat pump time constant [s]
		static Real64 FanDelayTime( 0.0 ); // Fan delay time, time delay for the HP's fan to
		static Real64 OnOffAirFlowRatio( 1.0 ); // ratio of compressor on flow to average flow over time step
		static Real64 PartLoadRatio( 1.0 ); // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );
		SpeedNum = int( Par( 3 ) );

		SimVariableSpeedCoils( "", CoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadRatio, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

		OutletAirTemp = VarSpeedCoil( CoilIndex ).OutletAirDBTemp;
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;

	}

} // HVACDXHeatPumpSystem

} // EnergyPlus
