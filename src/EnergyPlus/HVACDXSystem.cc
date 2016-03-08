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
#include <HVACDXSystem.hh>
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
#include <PackagedThermalStorageCoil.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <VariableSpeedCoils.hh>

namespace EnergyPlus {

// note that there are two modules in this file

//  HVACDXSystem is for cooling DX coils

//  HVACDXHeatPumpSystem is for heating DX coils

namespace HVACDXSystem {
	// Module containing the DXCoolingSystem simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard Liesen
	//       DATE WRITTEN   March 2001
	//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
	//                        Add dehumidification controls and support for multimode DX coil
	//                        Work supported by ASHRAE research project 1254-RP
	//                      Feb 2013 Bereket Nigusse, FSEC
	//                        Added DX Coil Model For 100% OA systems
	//                      Feb 2013 Bo Shen, Oak Ridge National Lab
	//                      Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the DXCoolingSystem System Component

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
	// Dehumidification control modes (DehumidControlMode)
	int const DehumidControl_None( 0 );
	int const DehumidControl_Multimode( 1 );
	int const DehumidControl_CoolReheat( 2 );
	bool GetInputFlag( true ); // Flag to get input only once

	//packaged TES modes
	int const OffMode( 0 );
	int const CoolingOnlyMode( 1 );
	int const CoolingAndChargeMode( 2 );
	int const CoolingAndDischargeMode( 3 );
	int const ChargeOnlyMode( 4 );
	int const DischargeOnlyMode( 5 );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumDXSystem( 0 ); // The Number of DXCoolingSystems found in the Input
	bool EconomizerFlag( false ); // holds air loop economizer status

	// Make this type allocatable
	Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Update routine to check convergence and update nodes

	// Object Data
	Array1D< DXCoolingConditions > DXCoolingSystem;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	clear_state()
	{
		NumDXSystem = 0;
		EconomizerFlag = false;
		GetInputFlag = true;
		CheckEquipName.deallocate();
		DXCoolingSystem.deallocate();
	}

	void
	SimDXCoolingSystem(
		std::string const & DXCoolingSystemName, // Name of DXSystem:Airloop object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to DXSystem:Airloop object
		Optional_int_const OAUnitNum, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
		Optional< Real64 > QTotOut // the total cooling output of unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Mar 2001
		//       MODIFIED       Richard Raustad, Sept 2003 (added HVACHXAssistedCoolingCoil)
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add support for multimode DX coil
		//                      Feb 2013 Bo Shen, Oak Ridge National Lab
		//                      Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages DXCoolingSystem component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DXCoils::SimDXCoil;
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::SimDXCoilMultiMode;
		using General::TrimSigDigits;
		using DataAirLoop::AirLoopControlInfo;
		using InputProcessor::FindItemInList;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using PackagedThermalStorageCoil::SimTESCoil;

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
		std::string CompName; // Name of DXSystem:Airloop object
		int DXSystemNum; // Index to DXSystem:Airloop object
		bool HXUnitOn; // Flag to control HX for HXAssisted Cooling Coil
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
			GetDXCoolingSystemInput();
			GetInputFlag = false;
		}

		// Find the correct DXSystemNumber
		if ( CompIndex == 0 ) {
			DXSystemNum = FindItemInList( DXCoolingSystemName, DXCoolingSystem );
			if ( DXSystemNum == 0 ) {
				ShowFatalError( "SimDXCoolingSystem: DXUnit not found=" + DXCoolingSystemName );
			}
			CompIndex = DXSystemNum;
		} else {
			DXSystemNum = CompIndex;
			if ( DXSystemNum > NumDXSystem || DXSystemNum < 1 ) {
				ShowFatalError( "SimulateDXCoolingSystem:  Invalid CompIndex passed=" + TrimSigDigits( DXSystemNum ) + ", Number of DX Units=" + TrimSigDigits( NumDXSystem ) + ", DX Unit name=" + DXCoolingSystemName );
			}
			if ( CheckEquipName( DXSystemNum ) ) {
				if ( DXCoolingSystemName != DXCoolingSystem( DXSystemNum ).Name ) {
					ShowFatalError( "SimulateDXCoolingSystem: Invalid CompIndex passed=" + TrimSigDigits( DXSystemNum ) + ", DX Unit name=" + DXCoolingSystemName + ", stored DX Unit Name for that index=" + DXCoolingSystem( DXSystemNum ).Name );
				}
				CheckEquipName( DXSystemNum ) = false;
			}
		}

		InitDXCoolingSystem( DXSystemNum, AirLoopNum, OAUnitNum, OAUCoilOutTemp );

		//Call the series of components that simulate a DX Cooling System
		// Control the DX Cooling System
		HXUnitOn = false;
		ControlDXSystem( DXSystemNum, FirstHVACIteration, HXUnitOn );

		// simulate DX Cooling System
		CompName = DXCoolingSystem( DXSystemNum ).CoolingCoilName;
		//Need a cooling System call here I think
		{ auto const SELECT_CASE_var( DXCoolingSystem( DXSystemNum ).CoolingCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

			SimDXCoil( CompName, On, FirstHVACIteration, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, DXCoolingSystem( DXSystemNum ).FanOpMode, DXCoolingSystem( DXSystemNum ).PartLoadFrac );

		} else if ( SELECT_CASE_var == CoilDX_CoolingHXAssisted ) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

			SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, DXCoolingSystem( DXSystemNum ).PartLoadFrac, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, DXCoolingSystem( DXSystemNum ).FanOpMode, HXUnitOn, _, EconomizerFlag );

		} else if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) { // Coil:Cooling:DX:TwoSpeed
			// formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL

			SimDXCoilMultiSpeed( CompName, DXCoolingSystem( DXSystemNum ).SpeedRatio, DXCoolingSystem( DXSystemNum ).CycRatio, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

		} else if ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode
			// formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL

			SimDXCoilMultiMode( CompName, On, FirstHVACIteration, DXCoolingSystem( DXSystemNum ).PartLoadFrac, DXCoolingSystem( DXSystemNum ).DehumidificationMode, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, DXCoolingSystem( DXSystemNum ).FanOpMode );
		} else if ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) { // Coil:Cooling:DX:VariableSpeed

			SimVariableSpeedCoils( CompName, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, DXCoolingSystem( DXSystemNum ).FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, DXCoolingSystem( DXSystemNum ).PartLoadFrac, DXCoolingSystem( DXSystemNum ).SpeedNum, DXCoolingSystem( DXSystemNum ).SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

		} else if ( SELECT_CASE_var == CoilDX_PackagedThermalStorageCooling ) {

			SimTESCoil( CompName, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, DXCoolingSystem( DXSystemNum ).FanOpMode, DXCoolingSystem( DXSystemNum ).TESOpMode, DXCoolingSystem( DXSystemNum ).PartLoadFrac );

		} else {
			ShowFatalError( "SimDXCoolingSystem: Invalid DX Cooling System/Coil=" + DXCoolingSystem( DXSystemNum ).CoolingCoilType );

		}}
		// set econo lockout flag
		// set econo lockout flag
		if ( AirLoopNum != -1 ) { // IF the sysem is not an equipment of outdoor air unit

			if ( ( DXCoolingSystem( DXSystemNum ).PartLoadFrac > 0.0 || DXCoolingSystem( DXSystemNum ).SpeedRatio > 0.0 || DXCoolingSystem( DXSystemNum ).CycRatio > 0.0 ) && AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithCompressor ) {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithCompressor = true;
			} else {
				AirLoopControlInfo( AirLoopNum ).ReqstEconoLockoutWithCompressor = false;
			}
		}

		if ( present( QTotOut ) ) {
			InletNodeNum = DXCoolingSystem( DXSystemNum ).DXCoolingCoilInletNodeNum;
			OutletNodeNum = DXCoolingSystem( DXSystemNum ).DXCoolingCoilOutletNodeNum;
			AirMassFlow = Node( OutletNodeNum ).MassFlowRate;
			QTotOut = AirMassFlow * ( Node( InletNodeNum ).Enthalpy - Node( OutletNodeNum ).Enthalpy );
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDXCoolingSystemInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Mar 2001
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add dehumidification controls and support for multimode DX coil
		//                      Feb 2013 Bo Shen, Oak Ridge National Lab
		//                      Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
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
		using HVACHXAssistedCoolingCoil::GetHXDXCoilIndex;
		using VariableSpeedCoils::GetCoilIndexVariableSpeed;
		using PackagedThermalStorageCoil::GetTESCoilIndex;
		using namespace DataIPShortCuts;
		using DXCoils::SetCoilSystemCoolingData;
		using DXCoils::SetDXCoilTypeData;
		using DXCoils::GetDXCoilIndex;

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
		int DXSystemNum; // The DXCoolingSystem that you are currently loading input into
		int NumAlphas;
		int NumNums;
		int IOStat;
		static std::string const RoutineName( "GetDXCoolingSystemInput: " ); // include trailing blank space
		static bool ErrorsFound( false ); // If errors detected in input
		static bool ErrFound( false ); // used for mining functions
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int DXCoolSysNum;
		bool FanErrorsFound; // flag returned on fan operating mode check
		bool DXErrorsFound; // flag returned on DX coil name check
		std::string HXDXCoolCoilName; // Name of DX cooling coil used with Heat Exchanger Assisted Cooling Coil
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

		CurrentModuleObject = "CoilSystem:Cooling:DX";
		NumDXSystem = GetNumObjectsFound( CurrentModuleObject );

		DXCoolingSystem.allocate( NumDXSystem );
		CheckEquipName.dimension( NumDXSystem, true );

		GetObjectDefMaxArgs( "CoilSystem:Cooling:DX", TotalArgs, NumAlphas, NumNums );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNums );
		Numbers.dimension( NumNums, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNums, true );

		// Get the data for the DX Cooling System
		for ( DXCoolSysNum = 1; DXCoolSysNum <= NumDXSystem; ++DXCoolSysNum ) {

			GetObjectItem( CurrentModuleObject, DXCoolSysNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoolingSystem, DXCoolSysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			DXCoolingSystem( DXCoolSysNum ).DXCoolingSystemType = CurrentModuleObject; // push Object Name into data array
			DXCoolingSystem( DXCoolSysNum ).Name = Alphas( 1 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoolingSystem( DXCoolSysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoolingSystem( DXCoolSysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( DXCoolingSystem( DXCoolSysNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			DXCoolingSystem( DXCoolSysNum ).DXCoolingCoilInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			DXCoolingSystem( DXCoolSysNum ).DXCoolingCoilOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			DXCoolingSystem( DXCoolSysNum ).DXSystemControlNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsParent );
			if ( DXCoolingSystem( DXCoolSysNum ).DXSystemControlNodeNum == 0 ) {
				ShowSevereError( CurrentModuleObject + ": control node must be input" );
				ShowContinueError( "Error occurred in " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
				ErrorsFound = true;
			}

			// Get Cooling System Information if available
			if ( SameString( Alphas( 6 ), "Coil:Cooling:DX:SingleSpeed" ) || SameString( Alphas( 6 ), "Coil:Cooling:DX:VariableSpeed" ) || SameString( Alphas( 6 ), "Coil:Cooling:DX:TwoSpeed" ) || SameString( Alphas( 6 ), "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) || SameString( Alphas( 6 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) || SameString( Alphas( 6 ), "Coil:Cooling:DX:SingleSpeed:ThermalStorage" ) ) {

				DXCoolingSystem( DXCoolSysNum ).CoolingCoilType = Alphas( 6 );
				DXCoolingSystem( DXCoolSysNum ).CoolingCoilName = Alphas( 7 );

				ErrFound = false;
				if ( SameString( Alphas( 6 ), "Coil:Cooling:DX:SingleSpeed" ) ) {
					DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num = CoilDX_CoolingSingleSpeed;
					GetDXCoilIndex( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, DXCoolingSystem( DXCoolSysNum ).CoolingCoilIndex, ErrFound, CurrentModuleObject );
					if ( ErrFound ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + DXCoolingSystem( DXCoolSysNum ).Name );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 6 ), "Coil:Cooling:DX:VariableSpeed" ) ) {
					DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
					DXCoolingSystem( DXCoolSysNum ).CoolingCoilIndex = GetCoilIndexVariableSpeed( "Coil:Cooling:DX:VariableSpeed", DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, ErrFound );
					if ( ErrFound ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + DXCoolingSystem( DXCoolSysNum ).Name );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 6 ), "Coil:Cooling:DX:TwoSpeed" ) ) {
					DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num = CoilDX_CoolingTwoSpeed;
					GetDXCoilIndex( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, DXCoolingSystem( DXCoolSysNum ).CoolingCoilIndex, ErrFound, CurrentModuleObject );
					if ( ErrFound ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + DXCoolingSystem( DXCoolSysNum ).Name );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 6 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
					DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num = CoilDX_CoolingHXAssisted;
					GetHXDXCoilIndex( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, DXCoolingSystem( DXCoolSysNum ).CoolingCoilIndex, ErrFound, CurrentModuleObject );
					if ( ErrFound ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + DXCoolingSystem( DXCoolSysNum ).Name );
						ErrorsFound = true;
					}

					DXErrorsFound = false;
					HXDXCoolCoilName = GetHXDXCoilName( Alphas( 6 ), Alphas( 7 ), DXErrorsFound );
					if ( DXErrorsFound ) {
						ShowWarningError( CurrentModuleObject + " = \"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"" );
						ShowContinueError( "CoilSystem:Cooling:DX:HeatExchangerAssisted \"" + Alphas( 7 ) + "\" not found." );
						ErrorsFound = true;
					}

				} else if ( SameString( Alphas( 6 ), "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {
					DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num = CoilDX_CoolingTwoStageWHumControl;
					GetDXCoilIndex( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, DXCoolingSystem( DXCoolSysNum ).CoolingCoilIndex, ErrFound, CurrentModuleObject );
					if ( ErrFound ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + DXCoolingSystem( DXCoolSysNum ).Name );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 6 ), "Coil:Cooling:DX:SingleSpeed:ThermalStorage" ) ) {
					DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num = CoilDX_PackagedThermalStorageCooling;
					GetTESCoilIndex( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, DXCoolingSystem( DXCoolSysNum ).CoolingCoilIndex, ErrFound, CurrentModuleObject );
					if ( ErrFound ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + DXCoolingSystem( DXCoolSysNum ).Name );
						ErrorsFound = true;
					}
				}

			} else {
				ShowSevereError( "Invalid entry for " + cAlphaFields( 6 ) + " :" + Alphas( 6 ) );
				ShowContinueError( "In " + CurrentModuleObject + "=\"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"." );
				ErrorsFound = true;
			}

			ValidateComponent( DXCoolingSystem( DXCoolSysNum ).CoolingCoilType, DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, IsNotOK, CurrentModuleObject );
			if ( IsNotOK ) {
				ShowContinueError( "In " + CurrentModuleObject + " = \"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"." );
				ErrorsFound = true;
			}

			SetUpCompSets( DXCoolingSystem( DXCoolSysNum ).DXCoolingSystemType, DXCoolingSystem( DXCoolSysNum ).Name, Alphas( 6 ), Alphas( 7 ), Alphas( 3 ), Alphas( 4 ) );

			FanErrorsFound = false;

			// Supply air fan operating mode defaulted to constant fan cycling coil/compressor
			DXCoolingSystem( DXCoolSysNum ).FanOpMode = ContFanCycCoil;

			// Dehumidification control mode
			if ( SameString( Alphas( 8 ), "None" ) ) {
				DXCoolingSystem( DXCoolSysNum ).DehumidControlType = DehumidControl_None;
			} else if ( SameString( Alphas( 8 ), "" ) ) {
				DXCoolingSystem( DXCoolSysNum ).DehumidControlType = DehumidControl_None;
			} else if ( SameString( Alphas( 8 ), "Multimode" ) ) {
				if ( DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					DXCoolingSystem( DXCoolSysNum ).DehumidControlType = DehumidControl_Multimode;
				} else if ( DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num == CoilDX_CoolingHXAssisted ) {
					DXCoolingSystem( DXCoolSysNum ).DehumidControlType = DehumidControl_Multimode;
				} else {
					ShowWarningError( "Invalid entry for " + cAlphaFields( 8 ) + " :" + Alphas( 8 ) );
					ShowContinueError( "In " + CurrentModuleObject + "=\"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"." );
					ShowContinueError( "Valid only with cooling coil type = Coil:Cooling:DX:TwoStageWithHumidityControlMode or CoilSystem:Cooling:DX:HeatExchangerAssisted." );
					ShowContinueError( "Setting " + cAlphaFields( 8 ) + " to None." );
					DXCoolingSystem( DXCoolSysNum ).DehumidControlType = DehumidControl_None;
				}
			} else if ( SameString( Alphas( 8 ), "CoolReheat" ) ) {
				DXCoolingSystem( DXCoolSysNum ).DehumidControlType = DehumidControl_CoolReheat;
			} else {
				ShowSevereError( "Invalid entry for " + cAlphaFields( 8 ) + " :" + Alphas( 8 ) );
				ShowContinueError( "In " + CurrentModuleObject + "=\"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"." );
			}

			// Run on sensible load
			if ( SameString( Alphas( 9 ), "Yes" ) ) {
				DXCoolingSystem( DXCoolSysNum ).RunOnSensibleLoad = true;
			} else if ( SameString( Alphas( 9 ), "" ) ) {
				DXCoolingSystem( DXCoolSysNum ).RunOnSensibleLoad = true;
			} else if ( SameString( Alphas( 9 ), "No" ) ) {
				DXCoolingSystem( DXCoolSysNum ).RunOnSensibleLoad = false;
			} else {
				ShowSevereError( "Invalid entry for " + cAlphaFields( 9 ) + " :" + Alphas( 9 ) );
				ShowContinueError( "In " + CurrentModuleObject + "=\"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"." );
				ShowContinueError( "Must be Yes or No." );
			}

			// Run on latent load
			if ( SameString( Alphas( 10 ), "Yes" ) ) {
				DXCoolingSystem( DXCoolSysNum ).RunOnLatentLoad = true;
			} else if ( SameString( Alphas( 10 ), "" ) ) {
				DXCoolingSystem( DXCoolSysNum ).RunOnLatentLoad = false;
			} else if ( SameString( Alphas( 10 ), "No" ) ) {
				DXCoolingSystem( DXCoolSysNum ).RunOnLatentLoad = false;
			} else {
				ShowSevereError( "Invalid entry for " + cAlphaFields( 10 ) + " :" + Alphas( 10 ) );
				ShowContinueError( "In " + CurrentModuleObject + "=\"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"." );
				ShowContinueError( "Must be Yes or No." );
			}

			// Run as 100% DOAS DX coil
			if ( lAlphaBlanks( 11 ) && NumAlphas <= 10 ) {
				DXCoolingSystem( DXCoolSysNum ).ISHundredPercentDOASDXCoil = false;
			} else {
				if ( SameString( Alphas( 11 ), "Yes" ) ) {
					DXCoolingSystem( DXCoolSysNum ).ISHundredPercentDOASDXCoil = true;
				} else if ( SameString( Alphas( 11 ), "" ) ) {
					DXCoolingSystem( DXCoolSysNum ).ISHundredPercentDOASDXCoil = false;
				} else if ( SameString( Alphas( 11 ), "No" ) ) {
					DXCoolingSystem( DXCoolSysNum ).ISHundredPercentDOASDXCoil = false;
				} else {
					ShowSevereError( "Invalid entry for " + cAlphaFields( 11 ) + " :" + Alphas( 11 ) );
					ShowContinueError( "In " + CurrentModuleObject + "=\"" + DXCoolingSystem( DXCoolSysNum ).Name + "\"." );
					ShowContinueError( "Must be Yes or No." );
				}
			}

			// considered as as 100% DOAS DX cooling coil
			if ( DXCoolingSystem( DXCoolSysNum ).ISHundredPercentDOASDXCoil ) {
				// set the system DX Coil application type to the child DX coil
				SetDXCoilTypeData( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName );
			}
			// DOAS DX Cooling Coil Leaving Minimum Air Temperature
			if ( NumNums > 0 ) {
				if ( ! lNumericBlanks( 1 ) ) {
					DXCoolingSystem( DXCoolSysNum ).DOASDXCoolingCoilMinTout = Numbers( 1 );
				}
			}
			if ( DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num == CoilDX_CoolingTwoSpeed ) {
				SetCoilSystemCoolingData( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName, DXCoolingSystem( DXCoolSysNum ).Name );
			}

		} //End of the DX System Loop

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

		for ( DXSystemNum = 1; DXSystemNum <= NumDXSystem; ++DXSystemNum ) {
			// Setup Report variables for the DXCoolingSystem that is not reported in the components themselves
			if ( SameString( DXCoolingSystem( DXSystemNum ).CoolingCoilType, "Coil:Cooling:DX:Twospeed" ) ) {
				SetupOutputVariable( "Coil System Cycling Ratio []", DXCoolingSystem( DXSystemNum ).CycRatio, "System", "Average", DXCoolingSystem( DXSystemNum ).Name );
				SetupOutputVariable( "Coil System Compressor Speed Ratio []", DXCoolingSystem( DXSystemNum ).SpeedRatio, "System", "Average", DXCoolingSystem( DXSystemNum ).Name );
			} else {
				SetupOutputVariable( "Coil System Part Load Ratio []", DXCoolingSystem( DXSystemNum ).PartLoadFrac, "System", "Average", DXCoolingSystem( DXSystemNum ).Name );
			}
			SetupOutputVariable( "Coil System Frost Control Status []", DXCoolingSystem( DXSystemNum ).FrostControlStatus, "System", "Average", DXCoolingSystem( DXSystemNum ).Name );
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
	InitDXCoolingSystem(
		int const DXSystemNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		Optional_int_const OAUnitNum, // number of the current outdoor air unit being simulated
		Optional< Real64 const > OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2001
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add dehumidification controls
		//                      May 2009, B. Griffith, NREL added EMS setpoint checks
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the DX Cooling Systems.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::DoSetPointTest;
		using DataAirLoop::AirLoopControlInfo;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iHumidityRatioMaxSetPoint;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataEnvironment::OutBaroPress;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutNode; // outlet node number
		int ControlNode; // control node number
		int DXSysIndex;
		static bool MyOneTimeFlag( true );
		static bool MySetPointCheckFlag( true );
		int OutdoorAirUnitNum; // "ONLY" for ZoneHVAC:OutdoorAirUnit
		Real64 OAUCoilOutletTemp; // "ONLY" for zoneHVAC:OutdoorAirUnit
		// FLOW:

		if ( MyOneTimeFlag ) {

			MyOneTimeFlag = false;
		}
		if ( AirLoopNum == -1 ) { // This Dx system is component of ZoneHVAC:OutdoorAirUnit
			OutdoorAirUnitNum = OAUnitNum;
			OAUCoilOutletTemp = OAUCoilOutTemp;
		}

		if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
			for ( DXSysIndex = 1; DXSysIndex <= NumDXSystem; ++DXSysIndex ) {
				ControlNode = DXCoolingSystem( DXSysIndex ).DXSystemControlNodeNum;
				if ( ControlNode > 0 ) {
					if ( AirLoopNum == -1 ) { // Outdoor Air Unit
						Node( ControlNode ).TempSetPoint = OAUCoilOutletTemp; // Set the coil outlet temperature
						if ( DXCoolingSystem( DXSystemNum ).ISHundredPercentDOASDXCoil ) {
							FrostControlSetPointLimit( DXSystemNum, DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( ControlNode ).HumRatMax, OutBaroPress, DXCoolingSystem( DXSystemNum ).DOASDXCoolingCoilMinTout, 1 );
						}
					} else if ( AirLoopNum != -1 ) { // Not an outdoor air unit

						if ( Node( ControlNode ).TempSetPoint == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( DXCoolingSystem( DXSysIndex ).DXCoolingSystemType + ": Missing temperature setpoint for DX unit= " + DXCoolingSystem( DXSysIndex ).Name );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the unit control node." );
								SetPointErrorFlag = true;
							} else {
								CheckIfNodeSetPointManagedByEMS( ControlNode, iTemperatureSetPoint, SetPointErrorFlag );
								if ( SetPointErrorFlag ) {
									ShowSevereError( DXCoolingSystem( DXSysIndex ).DXCoolingSystemType + ": Missing temperature setpoint for DX unit= " + DXCoolingSystem( DXSysIndex ).Name );
									ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the unit control node." );
									ShowContinueError( "  or use an EMS actuator to establish a temperature setpoint at the unit control node." );
								}
							}
						}
						if ( ( DXCoolingSystem( DXSysIndex ).DehumidControlType != DehumidControl_None ) && ( Node( ControlNode ).HumRatMax == SensedNodeFlagValue ) ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( DXCoolingSystem( DXSysIndex ).DXCoolingSystemType + ": Missing humidity ratio setpoint (HUMRATMAX) for DX unit= " + DXCoolingSystem( DXSysIndex ).Name );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the unit control node." );
								SetPointErrorFlag = true;
							} else {
								CheckIfNodeSetPointManagedByEMS( ControlNode, iHumidityRatioMaxSetPoint, SetPointErrorFlag );
								if ( SetPointErrorFlag ) {
									ShowSevereError( DXCoolingSystem( DXSysIndex ).DXCoolingSystemType + ": Missing maximum humidity ratio setpoint (HUMRATMAX) for DX unit= " + DXCoolingSystem( DXSysIndex ).Name );
									ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the unit control node." );
									ShowContinueError( "  or use an EMS actuator to establish a maximum humidity ratio setpoint." );
								}
							}

						}
					}
				}
			}
			MySetPointCheckFlag = false;
		}

		// These initializations are done every iteration
		if ( AirLoopNum == -1 ) { // This IF-TEHN routine is just for ZoneHVAC:OUTDOORAIRUNIT
			OutNode = DXCoolingSystem( DXSystemNum ).DXCoolingCoilOutletNodeNum;
			ControlNode = DXCoolingSystem( DXSystemNum ).DXSystemControlNodeNum;

			if ( ControlNode == 0 ) {
				DXCoolingSystem( DXSystemNum ).DesiredOutletTemp = 0.0;
				DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat = 1.0;
			} else if ( ControlNode == OutNode ) {
				DXCoolingSystem( DXSystemNum ).DesiredOutletTemp = OAUCoilOutletTemp;
				if ( DXCoolingSystem( DXSystemNum ).ISHundredPercentDOASDXCoil && DXCoolingSystem( DXSystemNum ).RunOnSensibleLoad ) {
					FrostControlSetPointLimit( DXSystemNum, DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( ControlNode ).HumRatMax, OutBaroPress, DXCoolingSystem( DXSystemNum ).DOASDXCoolingCoilMinTout, 1 );
				}
			}
			//  If the Dxsystem is an equipment of Outdoor Air Unit, the desiered coiloutlet humidity level is set to zero
			DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat = 1.0;

		} else if ( AirLoopNum != -1 ) { // Not Outdoor Air Unit

			OutNode = DXCoolingSystem( DXSystemNum ).DXCoolingCoilOutletNodeNum;
			ControlNode = DXCoolingSystem( DXSystemNum ).DXSystemControlNodeNum;
			EconomizerFlag = AirLoopControlInfo( AirLoopNum ).EconoActive;
			if ( ControlNode == 0 ) {
				DXCoolingSystem( DXSystemNum ).DesiredOutletTemp = 0.0;
				DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat = 1.0;
			} else if ( ControlNode == OutNode ) {
				if ( DXCoolingSystem( DXSystemNum ).ISHundredPercentDOASDXCoil && DXCoolingSystem( DXSystemNum ).RunOnSensibleLoad ) {
					FrostControlSetPointLimit( DXSystemNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, DXCoolingSystem( DXSystemNum ).DOASDXCoolingCoilMinTout, 1 );
				}
				DXCoolingSystem( DXSystemNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint;
				//  If HumRatMax is zero, then there is no request from SetpointManager:SingleZone:Humidity:Maximum
				if ( ( DXCoolingSystem( DXSystemNum ).DehumidControlType != DehumidControl_None ) && ( Node( ControlNode ).HumRatMax > 0.0 ) ) {
					if ( DXCoolingSystem( DXSystemNum ).ISHundredPercentDOASDXCoil && DXCoolingSystem( DXSystemNum ).RunOnLatentLoad ) {
						FrostControlSetPointLimit( DXSystemNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, DXCoolingSystem( DXSystemNum ).DOASDXCoolingCoilMinTout, 2 );
					}
					DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat = Node( ControlNode ).HumRatMax;
				} else {
					DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat = 1.0;
				}
			} else {
				if ( DXCoolingSystem( DXSystemNum ).ISHundredPercentDOASDXCoil && DXCoolingSystem( DXSystemNum ).RunOnSensibleLoad ) {
					FrostControlSetPointLimit( DXSystemNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, DXCoolingSystem( DXSystemNum ).DOASDXCoolingCoilMinTout, 1 );
				}
				DXCoolingSystem( DXSystemNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint - ( Node( ControlNode ).Temp - Node( OutNode ).Temp );
				if ( DXCoolingSystem( DXSystemNum ).DehumidControlType != DehumidControl_None ) {
					if ( DXCoolingSystem( DXSystemNum ).ISHundredPercentDOASDXCoil && DXCoolingSystem( DXSystemNum ).RunOnLatentLoad ) {
						FrostControlSetPointLimit( DXSystemNum, Node( ControlNode ).TempSetPoint, Node( ControlNode ).HumRatMax, OutBaroPress, DXCoolingSystem( DXSystemNum ).DOASDXCoolingCoilMinTout, 2 );
					}
					DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat = Node( ControlNode ).HumRatMax - ( Node( ControlNode ).HumRat - Node( OutNode ).HumRat );
				} else {
					DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat = 1.0;
				}
			}
		}
	}

	// End of Initialization subroutines for the Module
	// *****************************************************************************

	// Beginning of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

	void
	ControlDXSystem(
		int const DXSystemNum, // index to DXSystem
		bool const FirstHVACIteration, // First HVAC iteration flag
		bool & HXUnitOn // flag to enable heat exchanger heat recovery
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Feb 2001
		//       MODIFIED       Richard Raustad, FSEC Nov 2003
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add dehumidification controls and support for multimode DX coil
		//                      Jan 2008 R. Raustad, FSEC. Added coolreheat to all coil types
		//                      Feb 2013 Bo Shen, Oak Ridge National Lab
		//                      Add Coil:Cooling:DX:VariableSpeed, capable of both sensible and latent cooling
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine updates the System outlet nodes.

		// METHODOLOGY EMPLOYED:
		//  Data is moved from the System data structure to the System outlet nodes.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using namespace ScheduleManager;
		using DataEnvironment::OutBaroPress;
		using DataHVACGlobals::TempControlTol;
		using InputProcessor::FindItemInList;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTdpFnWPb;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DXCoils::SimDXCoil;
		using DXCoils::SimDXCoilMultiSpeed;
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::SimDXCoilMultiMode;
		using DXCoils::DXCoilOutletHumRat;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp;
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;
		using PackagedThermalStorageCoil::SimTESCoil;
		using PackagedThermalStorageCoil::ControlTESIceStorageTankCoil;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.e-3 ); // Accuracy of solver result
		Real64 const HumRatAcc( 1.e-6 ); // Accuracy of solver result

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // Name of the DX cooling coil
		Real64 NoOutput; // Sensible capacity (outlet - inlet) when the compressor is off
		Real64 FullOutput; // Sensible capacity (outlet - inlet) when the compressor is on
		Real64 ReqOutput; // Sensible capacity (outlet - inlet) required to meet load or setpoint temperature
		int InletNode; // Inlet node number of the DX cooling coil
		int OutletNode; // Outlet node number of the DX cooling coil
		int ControlNode; // The node number where a setpoint is placed to control the DX cooling coil
		Real64 PartLoadFrac; // The part-load fraction of the compressor
		Real64 SpeedRatio; // SpeedRatio = (CompressorSpeed - CompressorSpeedMin) /
		//              (CompressorSpeedMax - CompressorSpeedMin)
		// for variable speed or 2 speed compressors
		Real64 CycRatio; // Cycling part-load ratio for variable speed or 2 speed compressors
		Real64 DesOutTemp; // Desired outlet temperature of the DX cooling coil
		Real64 DesOutHumRat; // Desired outlet humidity ratio of the DX cooling coil
		Real64 OutletTempDXCoil; // Actual outlet temperature of the DX cooling coil
		Real64 OutletTempLS; // Actual outlet temperature of the variable speed DX cooling coil at low speed
		Real64 OutletTempHS; // Actual outlet temperature of the variable speed DX cooling coil at high speed
		Real64 OutletHumRatLS; // Actual outlet humrat of the variable speed DX cooling coil at low speed
		Real64 OutletHumRatHS; // Actual outlet humrat of the variable speed DX cooling coil at high speed
		Real64 OutletHumRatDXCoil; // Actual outlet humidity ratio of the DX cooling coil
		int SolFla; // Flag of solver
		Array1D< Real64 > Par( 5 ); // Parameter array passed to solver
		bool SensibleLoad; // True if there is a sensible cooling load on this system
		bool LatentLoad; // True if there is a latent   cooling load on this system
		int DehumidMode; // Dehumidification mode (0=normal, 1=enhanced)
		int FanOpMode; // Supply air fan operating mode
		Real64 TempMinPLR; // Used to find latent PLR when max iterations exceeded
		Real64 TempMaxPLR; // Used to find latent PLR when max iterations exceeded
		Real64 TempOutletTempDXCoil; // Used to find latent PLR when max iterations exceeded
		Real64 TempOutletHumRatDXCoil; // Used to find latent PLR when max iterations exceeded
		Real64 NoLoadHumRatOut; // DX coil outlet air humidity ratio with comprssor off
		Real64 FullLoadHumRatOut; // DX coil outlet air humidity ratio with comprssor full on
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

		// Set local variables
		// Retrieve the load on the controlled zone
		OutletNode = DXCoolingSystem( DXSystemNum ).DXCoolingCoilOutletNodeNum;
		InletNode = DXCoolingSystem( DXSystemNum ).DXCoolingCoilInletNodeNum;
		ControlNode = DXCoolingSystem( DXSystemNum ).DXSystemControlNodeNum;
		DesOutTemp = DXCoolingSystem( DXSystemNum ).DesiredOutletTemp;
		DesOutHumRat = DXCoolingSystem( DXSystemNum ).DesiredOutletHumRat;
		CompName = DXCoolingSystem( DXSystemNum ).CoolingCoilName;
		FanOpMode = DXCoolingSystem( DXSystemNum ).FanOpMode;
		SpeedRatio = 0.0;
		CycRatio = 0.0;
		PartLoadFrac = 0.0;
		DehumidMode = 0;
		SensibleLoad = false;
		LatentLoad = false;
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

		// If DXCoolingSystem is scheduled on and there is flow
		if ( ( GetCurrentScheduleValue( DXCoolingSystem( DXSystemNum ).SchedPtr ) > 0.0 ) && ( Node( InletNode ).MassFlowRate > MinAirMassFlow ) ) {

			// Determine if there is a sensible load on this system
			if ( ( Node( InletNode ).Temp > Node( ControlNode ).TempSetPoint ) && ( Node( InletNode ).Temp > DesOutTemp ) && ( std::abs( Node( InletNode ).Temp - DesOutTemp ) > TempControlTol ) ) SensibleLoad = true;

			// Determine if there is a latent load on this system - for future use to serve latent-only loads
			if ( ( Node( InletNode ).HumRat > Node( ControlNode ).HumRatMax ) && ( Node( InletNode ).HumRat > DesOutHumRat ) ) LatentLoad = true;

			// If DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
			// Multimode coil will switch to enhanced dehumidification if available and needed, but it
			// still runs to meet the sensible load. Multimode applies to Multimode or HXAssistedCooling coils.
			if ( ( SensibleLoad && DXCoolingSystem( DXSystemNum ).RunOnSensibleLoad ) || ( LatentLoad && DXCoolingSystem( DXSystemNum ).RunOnLatentLoad ) ) {
				// calculate sensible PLR, don't care if latent is true here but need to gaurd for
				// when LatentLoad=TRUE and SensibleLoad=FALSE
				{ auto const SELECT_CASE_var( DXCoolingSystem( DXSystemNum ).CoolingCoilType_Num );

				if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

					// Get no load result
					PartLoadFrac = 0.0;
					SimDXCoil( CompName, On, FirstHVACIteration, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, PartLoadFrac );
					NoOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );
					NoLoadHumRatOut = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

					// Get full load result
					PartLoadFrac = 1.0;
					SimDXCoil( CompName, On, FirstHVACIteration, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, PartLoadFrac );
					FullLoadHumRatOut = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					//         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
					if ( ( NoOutput - ReqOutput ) < Acc ) {
						PartLoadFrac = 0.0;
						//         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
						//         run the compressor at PartLoadFrac = 1.
					} else if ( ( FullOutput - ReqOutput ) > Acc ) {
						PartLoadFrac = 1.0;
						//         Else find the PLR to meet the load
					} else {
						//           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above. If this temp is
						//           greater than the desired outlet temp, then run the compressor at PartLoadFrac = 1, otherwise find the operating PLR.
						OutletTempDXCoil = DXCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
						if ( OutletTempDXCoil > DesOutTemp ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter < 1 ) {
										++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
										ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
								}
							} else if ( SolFla == -2 ) {
								PartLoadFrac = ReqOutput / FullOutput;
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail < 1 ) {
										++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
								}

							}
						}
					}

					//         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
					//         else use operating humidity ratio to test against humidity setpoint
					if ( PartLoadFrac == 0.0 ) {
						OutletHumRatDXCoil = NoLoadHumRatOut;
					} else {
						OutletHumRatDXCoil = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
					}

					// If humidity setpoint is not satisfied and humidity control type is CoolReheat,
					// then overcool to meet moisture load

					if ( ( OutletHumRatDXCoil > DesOutHumRat ) && ( PartLoadFrac < 1.0 ) && ( DXCoolingSystem( DXSystemNum ).DehumidControlType == DehumidControl_CoolReheat ) ) {

						//           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
						//           do not run the compressor
						if ( ( NoLoadHumRatOut - DesOutHumRat ) < HumRatAcc ) {
							//PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
							//           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
							//           run the compressor at PartLoadFrac = 1.
						} else if ( ( DesOutHumRat - FullLoadHumRatOut ) < HumRatAcc ) {
							PartLoadFrac = 1.0;
							//           Else find the PLR to meet the load
						} else {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutHumRat;
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilHumRatResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIter < 1 ) {
										++DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIter;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio   = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
										ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIterIndex, PartLoadFrac, PartLoadFrac );
								}
							} else if ( SolFla == -2 ) {
								//               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
								if ( NoLoadHumRatOut - FullLoadHumRatOut != 0.0 ) {
									PartLoadFrac = ( NoLoadHumRatOut - DesOutHumRat ) / ( NoLoadHumRatOut - FullLoadHumRatOut );
								} else {
									PartLoadFrac = 1.0;
								}
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFail < 1 ) {
										++DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFail;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFailIndex, PartLoadFrac, PartLoadFrac );
								}
							}
						}
					} // End if humidity ratio setpoint not met - CoolReheat humidity control

					if ( PartLoadFrac > 1.0 ) {
						PartLoadFrac = 1.0;
					} else if ( PartLoadFrac < 0.0 ) {
						PartLoadFrac = 0.0;
					}

				} else if ( SELECT_CASE_var == CoilDX_CoolingHXAssisted ) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

					//         Check the dehumidification control type. If it's multimode, turn off the HX to find the sensible PLR. Then check to
					//         see if the humidity load is met without the use of the HX. Always run the HX for the other modes.
					if ( DXCoolingSystem( DXSystemNum ).DehumidControlType != DehumidControl_Multimode ) {
						HXUnitOn = true;
					} else {
						HXUnitOn = false;
					}

					// Get no load result
					PartLoadFrac = 0.0;
					SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, PartLoadFrac, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
					NoOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );
					NoLoadHumRatOut = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

					// Get full load result
					PartLoadFrac = 1.0;
					SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, PartLoadFrac, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );
					FullLoadHumRatOut = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

					ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					//         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
					if ( ( NoOutput - ReqOutput ) < Acc ) {
						PartLoadFrac = 0.0;
						//         If the FullOutput is greater than or very near the ReqOutput, then run the compressor at PartLoadFrac = 1.
					} else if ( ( FullOutput - ReqOutput ) > Acc ) {
						PartLoadFrac = 1.0;
						//         Else find the PLR to meet the load
					} else {
						//           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
						//           If this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac = 1.
						//           (i.e. HX iterates to find solution, don't allow the tolerance in solution to trip up RegulaFalsi. So if
						//           solution is very near request, run compressor at PLR = 1)
						OutletTempDXCoil = HXAssistedCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
						if ( ( OutletTempDXCoil > DesOutTemp ) || std::abs( OutletTempDXCoil - DesOutTemp ) <= ( Acc * 2.0 ) ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							// FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
							if ( FirstHVACIteration ) {
								Par( 3 ) = 1.0;
							} else {
								Par( 3 ) = 0.0;
							}
							if ( HXUnitOn ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {

								//               RegulaFalsi may not find sensible PLR when the latent degradation model is used.
								//               If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
								TempMaxPLR = -0.1;
								TempOutletTempDXCoil = Node( InletNode ).Temp;
								while ( ( TempOutletTempDXCoil - DesOutTemp ) > 0.0 && TempMaxPLR <= 1.0 ) {
									//                 find upper limit of PLR
									TempMaxPLR += 0.1;
									SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMaxPLR, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
									TempOutletTempDXCoil = HXAssistedCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								}
								TempMinPLR = TempMaxPLR;
								while ( ( TempOutletTempDXCoil - DesOutTemp ) < 0.0 && TempMinPLR >= 0.0 ) {
									//                 pull upper limit of PLR down to last valid limit (i.e. outlet temp still exceeds DesOutTemp)
									TempMaxPLR = TempMinPLR;
									//                 find minimum limit of PLR
									TempMinPLR -= 0.01;
									SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMinPLR, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
									TempOutletTempDXCoil = HXAssistedCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								}
								//               Relax boundary slightly to assure a solution can be found using RegulaFalsi (i.e. one boundary may be
								//               very near the desired result)
								TempMinPLR = max( 0.0, ( TempMinPLR - 0.01 ) );
								TempMaxPLR = min( 1.0, ( TempMaxPLR + 0.01 ) );
								//               tighter boundary of solution has been found, call RegulaFalsi a second time
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, TempMinPLR, TempMaxPLR, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio   = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = ReqOutput / FullOutput;
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRFail < 1 ) {
											++DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRFail;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit sensible part-load ratio calculation unexpectedly failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation unexpectedly failed error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}

							} else if ( SolFla == -2 ) {
								PartLoadFrac = ReqOutput / FullOutput;
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRFail2 < 1 ) {
										++DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRFail2;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedSensPLRFailIndex2, PartLoadFrac, PartLoadFrac );
								}
							}
						}
					}

					//         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
					//         else use operating humidity ratio to test against humidity setpoint
					if ( PartLoadFrac == 0.0 ) {
						OutletHumRatDXCoil = NoLoadHumRatOut;
					} else {
						OutletHumRatDXCoil = HXAssistedCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
					}

					// If humidity setpoint is not satisfied and humidity control type is MultiMode,
					// then enable heat exchanger and run to meet sensible load

					if ( ( OutletHumRatDXCoil > DesOutHumRat ) && ( DXCoolingSystem( DXSystemNum ).DehumidControlType == DehumidControl_Multimode ) ) {

						// Determine required part load when heat exchanger is ON
						HXUnitOn = true;
						PartLoadFrac = 1.0;
						SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, PartLoadFrac, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );

						OutletTempDXCoil = HXAssistedCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

						//           FullOutput will be different than the FullOutput determined above during sensible PLR calculations
						FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

						ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

						//           Check to see if the system can meet the load with the compressor off
						//           IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
						if ( ( NoOutput - ReqOutput ) < Acc ) {
							PartLoadFrac = 0.0;
							//           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
							//           If this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac = 1.
						} else if ( ( OutletTempDXCoil > DesOutTemp ) || std::abs( OutletTempDXCoil - DesOutTemp ) <= ( Acc * 2.0 ) ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							// FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1.0 and FALSE = 0.0
							if ( FirstHVACIteration ) {
								Par( 3 ) = 1.0;
							} else {
								Par( 3 ) = 0.0;
							}
							if ( HXUnitOn ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).HXAssistedLatPLRIter < 1 ) {
										++DXCoolingSystem( DXSystemNum ).HXAssistedLatPLRIter;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated latent part-load ratio   = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
										ShowContinueError( "Calculated latent part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The calculated latent part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedLatPLRIterIndex, PartLoadFrac, PartLoadFrac );
								}
							} else if ( SolFla == -2 ) {
								PartLoadFrac = ReqOutput / FullOutput;
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).HXAssistedLatPLRFail < 1 ) {
										++DXCoolingSystem( DXSystemNum ).HXAssistedLatPLRFail;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedLatPLRFailIndex, PartLoadFrac, PartLoadFrac );
								}
							}
						}
					} // End if humidity ratio setpoint not met - Multimode humidity control

					// If humidity setpoint is not satisfied and humidity control type is CoolReheat, then calculate
					// a new latent PLR

					if ( OutletHumRatDXCoil > DesOutHumRat && PartLoadFrac < 1.0 && DXCoolingSystem( DXSystemNum ).DehumidControlType == DehumidControl_CoolReheat ) {

						//           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
						//           do not run the compressor
						if ( ( NoLoadHumRatOut - DesOutHumRat ) < HumRatAcc * 2.0 ) {
							//PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
							//           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
							//           run the compressor at PartLoadFrac = 1.
						} else if ( ( DesOutHumRat - FullLoadHumRatOut ) < HumRatAcc * 2.0 ) {
							PartLoadFrac = 1.0;
							//           Else find the PLR to meet the load
						} else {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutHumRat;
							// FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
							if ( FirstHVACIteration ) {
								Par( 3 ) = 1.0;
							} else {
								Par( 3 ) = 0.0;
							}
							if ( HXUnitOn ) {
								Par( 4 ) = 1.0;
							} else {
								Par( 4 ) = 0.0;
							}
							Par( 5 ) = double( FanOpMode );
							SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {

								//               RegulaFalsi may not find latent PLR when the latent degradation model is used.
								//               If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
								TempMaxPLR = -0.1;
								TempOutletHumRatDXCoil = OutletHumRatDXCoil;
								while ( ( OutletHumRatDXCoil - TempOutletHumRatDXCoil ) >= 0.0 && TempMaxPLR <= 1.0 ) {
									//                 find upper limit of LatentPLR
									TempMaxPLR += 0.1;
									SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMaxPLR, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
									OutletHumRatDXCoil = HXAssistedCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								}
								TempMinPLR = TempMaxPLR;
								while ( ( OutletHumRatDXCoil - TempOutletHumRatDXCoil ) <= 0.0 && TempMinPLR >= 0.0 ) {
									//                 pull upper limit of LatentPLR down to last valid limit (i.e. latent output still exceeds SystemMoisuterLoad)
									TempMaxPLR = TempMinPLR;
									//                 find minimum limit of Latent PLR
									TempMinPLR -= 0.01;
									SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, TempMaxPLR, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, HXUnitOn, _, EconomizerFlag );
									OutletHumRatDXCoil = HXAssistedCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								}
								//               tighter boundary of solution has been found, call RegulaFalsi a second time
								SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, TempMinPLR, TempMaxPLR, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated latent part-load ratio   = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated latent part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated latent part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}

								} else if ( SolFla == -2 ) {
									PartLoadFrac = ReqOutput / FullOutput;
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRFail < 1 ) {
											++DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRFail;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit latent part-load ratio calculation failed unexpectedly: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit latent part-load ratio calculation failed unexpectedly error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}
							} else if ( SolFla == -2 ) {
								PartLoadFrac = ReqOutput / FullOutput;
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRFail2 < 1 ) {
										++DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRFail2;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).HXAssistedCRLatPLRFailIndex2, PartLoadFrac, PartLoadFrac );
								}
							}
						}
					} // End if humidity ratio setpoint not met - CoolReheat humidity control

					if ( PartLoadFrac > 1.0 ) {
						PartLoadFrac = 1.0;
					} else if ( PartLoadFrac < 0.0 ) {
						PartLoadFrac = 0.0;
					}

				} else if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) { // Coil:Cooling:DX:TwoSpeed
					// formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL
					//         SUBROUTINE SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,CompIndex,SpeedNum,FanMode,CompOp)
					SimDXCoilMultiSpeed( CompName, 0.0, 1.0, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
					OutletTempLS = DXCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
					if ( OutletTempLS > DesOutTemp && SensibleLoad ) {
						CycRatio = 1.0;
						SimDXCoilMultiSpeed( CompName, 1.0, 1.0, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
						OutletTempHS = DXCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
						if ( OutletTempHS < DesOutTemp ) {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							SolveRegulaFalsi( Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).MSpdSensPLRIter < 1 ) {
										++DXCoolingSystem( DXSystemNum ).MSpdSensPLRIter;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible speed ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Calculated speed ratio = " + RoundSigDigits( SpeedRatio, 3 ) );
										ShowContinueErrorTimeStamp( "The calculated speed ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible speed ratio error continues. Sensible speed ratio statistics follow.", DXCoolingSystem( DXSystemNum ).MSpdSensPLRIterIndex, SpeedRatio, SpeedRatio );
								}
							} else if ( SolFla == -2 ) {
								if ( ! WarmupFlag ) ShowFatalError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - compressor speed calculation failed: speed limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
							}
						} else {
							SpeedRatio = 1.0;
						}
					} else if ( SensibleLoad ) {
						SpeedRatio = 0.0;
						Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
						Par( 2 ) = DesOutTemp;
						SolveRegulaFalsi( Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0, 1.0, Par );
						if ( SolFla == -1 ) {
							if ( ! WarmupFlag ) {
								if ( DXCoolingSystem( DXSystemNum ).MSpdCycSensPLRIter < 1 ) {
									++DXCoolingSystem( DXSystemNum ).MSpdCycSensPLRIter;
									ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible cycling ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
									ShowContinueError( "Calculated cycling ratio = " + RoundSigDigits( CycRatio, 3 ) );
									ShowContinueErrorTimeStamp( "The calculated cycling ratio will be used and the simulation continues. Occurrence info:" );
								}
								ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible cycling ratio error continues. Sensible cycling ratio statistics follow.", DXCoolingSystem( DXSystemNum ).MSpdCycSensPLRIterIndex, CycRatio, CycRatio );
							}
						} else if ( SolFla == -2 ) { // should never get here, if it does logic above to protect from this
							if ( ! WarmupFlag ) ShowFatalError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - cycling ratio calculation failed: cycling limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
						}
					} else {
						PartLoadFrac = 0.0;
						SpeedRatio = 0.0;
						CycRatio = 0.0;
						DehumidMode = 0;
					}

					if ( DXCoolingSystem( DXSystemNum ).DehumidControlType == DehumidControl_CoolReheat ) {

						//           Simulate MultiSpeed DX coil at sensible result
						SimDXCoilMultiSpeed( CompName, SpeedRatio, CycRatio, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

						OutletHumRatDXCoil = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
						// If humidity setpoint is not satisfied and humidity control type is CoolReheat,
						// then overcool to meet moisture load

						if ( OutletHumRatDXCoil > DesOutHumRat ) {

							CycRatio = 0.0;
							SpeedRatio = 0.0;

							//             SUBROUTINE SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,CompIndex,SpeedNum,FanMode,CompOp)
							SimDXCoilMultiSpeed( CompName, 0.0, 1.0, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							OutletHumRatLS = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							if ( OutletHumRatLS > DesOutHumRat ) {
								CycRatio = 1.0;
								SimDXCoilMultiSpeed( CompName, 1.0, 1.0, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								OutletHumRatHS = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								if ( OutletHumRatHS < DesOutHumRat ) {
									Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
									Par( 2 ) = DesOutHumRat;
									SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par );
									if ( SolFla == -1 ) {
										if ( ! WarmupFlag ) {
											if ( DXCoolingSystem( DXSystemNum ).MSpdLatPLRIter < 1 ) {
												++DXCoolingSystem( DXSystemNum ).MSpdLatPLRIter;
												ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit latent speed ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
												ShowContinueError( "Calculated speed ratio = " + RoundSigDigits( SpeedRatio, 3 ) );
												ShowContinueErrorTimeStamp( "The calculated speed ratio will be used and the simulation continues. Occurrence info:" );
											}
											ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating latent speed ratio error continues. Latent speed ratio statistics follow.", DXCoolingSystem( DXSystemNum ).MSpdLatPLRIterIndex, SpeedRatio, SpeedRatio );
										}
									} else if ( SolFla == -2 ) {
										if ( ! WarmupFlag ) ShowFatalError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - compressor speed calculation failed:speed limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
									}
								} else {
									SpeedRatio = 1.0;
								}
							} else {
								SpeedRatio = 0.0;
								Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								Par( 2 ) = DesOutHumRat;
								SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, CycRatio, DXCoilCyclingHumRatResidual, 0.0, 1.0, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).MSpdCycLatPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).MSpdCycLatPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit latent cycling ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Calculated cycling ratio = " + RoundSigDigits( CycRatio, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated cycling ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating latent cycling ratio error continues. Latent cycling ratio statistics follow.", DXCoolingSystem( DXSystemNum ).MSpdCycLatPLRIterIndex, CycRatio, CycRatio );
									}
								} else if ( SolFla == -2 ) { // should never get here, if it does logic above to protect from this
									if ( ! WarmupFlag ) ShowFatalError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - cycling ratio calculation failed: cycling limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
								}
							}

						}

					}

				} else if ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode
					// formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL)

					// Get no load result
					PartLoadFrac = 0.0;
					SimDXCoilMultiMode( CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode );
					NoOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );
					NoLoadHumRatOut = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

					// Get full load result
					PartLoadFrac = 1.0;
					SimDXCoilMultiMode( CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode );
					FullLoadHumRatOut = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					//         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
					if ( ( NoOutput - ReqOutput ) < Acc ) {
						PartLoadFrac = 0.0;
						//         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
						//         run the compressor at PartLoadFrac = 1.
					} else if ( ( FullOutput - ReqOutput ) > Acc ) {
						PartLoadFrac = 1.0;
						//         Else find the PLR to meet the load
					} else {
						OutletTempDXCoil = DXCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
						if ( OutletTempDXCoil > DesOutTemp ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutTemp;
							// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
							Par( 3 ) = double( DehumidMode );
							Par( 4 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).MModeSensPLRIter < 1 ) {
										++DXCoolingSystem( DXSystemNum ).MModeSensPLRIter;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
										ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).MModeSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
								}
							} else if ( SolFla == -2 ) {
								if ( ! WarmupFlag ) {
									ShowSevereError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
									ShowFatalError( "Program terminates due to previous condition." );
								}
							}
						}
					}

					OutletHumRatDXCoil = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );

					// If humidity setpoint is not satisfied and humidity control type is Multimode,
					// then turn on enhanced dehumidification mode 1

					if ( ( OutletHumRatDXCoil > DesOutHumRat ) && ( DXCoolingSystem( DXSystemNum ).DehumidControlType == DehumidControl_Multimode ) ) {

						// Determine required part load for enhanced dehumidification mode 1

						// Get full load result
						PartLoadFrac = 1.0;
						DehumidMode = 1;
						DXCoolingSystem( DXSystemNum ).DehumidificationMode = DehumidMode;
						SimDXCoilMultiMode( CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode );
						FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

						ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

						// Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
						// Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
						// Calculate the part load fraction
						if ( FullOutput >= 0 ) {
							PartLoadFrac = 0.0;
						} else {
							OutletTempDXCoil = DXCoilOutletTemp( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							OutletHumRatDXCoil = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							// if sensible load and setpoint cannot be met, set PLR = 1. If no sensible load and
							// latent load exists and setpoint cannot be met, set PLR = 1.
							if ( ( OutletTempDXCoil >= DesOutTemp && SensibleLoad && DXCoolingSystem( DXSystemNum ).RunOnSensibleLoad ) || ( OutletHumRatDXCoil >= DesOutHumRat && ! SensibleLoad && LatentLoad && DXCoolingSystem( DXSystemNum ).RunOnLatentLoad ) ) {
								PartLoadFrac = 1.0;
								// if no sensible load and latent load can be met, find PLR
							} else if ( ! SensibleLoad && ( OutletHumRatDXCoil < DesOutHumRat && LatentLoad && DXCoolingSystem( DXSystemNum ).RunOnLatentLoad ) ) {
								// is a latent load with no sensible load, iterate on humidity ratio
								Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								Par( 2 ) = DesOutHumRat;
								// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
								Par( 3 ) = double( DehumidMode );
								Par( 4 ) = double( FanOpMode );
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).MModeLatPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).MModeLatPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit multimode latent (no sensible) part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											if ( NoLoadHumRatOut - OutletHumRatDXCoil > 0.0 ) {
												TempMinPLR = ( DesOutHumRat - OutletHumRatDXCoil ) / ( NoLoadHumRatOut - OutletHumRatDXCoil );
											} else {
												TempMinPLR = PartLoadFrac + 0.001;
											}
											ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( TempMinPLR, 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating multimode latent (no sensible) part-load ratio error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).MModeLatPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									if ( ! WarmupFlag ) {
										ShowSevereError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
										ShowFatalError( "Program terminates due to previous condition." );
									}
								}

							} else { // must be a sensible load so find PLR
								PartLoadFrac = ReqOutput / FullOutput;
								Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
								Par( 2 ) = DesOutTemp;
								// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
								Par( 3 ) = double( DehumidMode );
								Par( 4 ) = double( FanOpMode );
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).MModeLatPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).MModeLatPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit multimode latent part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating multimode latent part-load ratio error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).MModeLatPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									if ( ! WarmupFlag ) {
										ShowSevereError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
										ShowFatalError( "Program terminates due to previous condition." );
									}
								}
							}
						}
					} // End if humidity ratio setpoint not met - multimode humidity control

					//         If humidity setpoint is not satisfied and humidity control type is CoolReheat, then run to meet latent load
					//         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
					//         else use operating humidity ratio to test against humidity setpoint
					if ( PartLoadFrac == 0.0 ) {
						OutletHumRatDXCoil = NoLoadHumRatOut;
					} else {
						OutletHumRatDXCoil = DXCoilOutletHumRat( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
					}

					if ( ( OutletHumRatDXCoil > DesOutHumRat ) && ( DXCoolingSystem( DXSystemNum ).DehumidControlType == DehumidControl_CoolReheat ) ) {

						//            CoolReheat operates cooling stage 1 and/or 2 to meet DesOutHumRat. Dehumidification mode is not active.
						DehumidMode = 0;

						//            IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
						//            do not run the compressor
						if ( ( NoLoadHumRatOut - DesOutHumRat ) < HumRatAcc ) {
							//PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
							//            If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
							//            run the compressor at PartLoadFrac = 1.
						} else if ( ( DesOutHumRat - FullLoadHumRatOut ) < HumRatAcc ) {
							PartLoadFrac = 1.0;
							//            Else find the PLR to meet the load
						} else {
							Par( 1 ) = double( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex );
							Par( 2 ) = DesOutHumRat;
							// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
							Par( 3 ) = double( DehumidMode );
							Par( 4 ) = double( FanOpMode );
							SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( ! WarmupFlag ) {
									if ( DXCoolingSystem( DXSystemNum ).MModeLatPLRIter2 < 1 ) {
										++DXCoolingSystem( DXSystemNum ).MModeLatPLRIter2;
										ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit coolreheat latent part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
										ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
										ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
										ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
									}
									ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating coolreheat latent part-load ratio error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).MModeLatPLRIterIndex2, PartLoadFrac, PartLoadFrac );
								}
							} else if ( SolFla == -2 ) {
								if ( ! WarmupFlag ) {
									ShowSevereError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " : part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" + DXCoolingSystem( DXSystemNum ).Name );
									ShowFatalError( "Program terminates due to previous condition." );
								}
							}
						}
					} // End if humidity ratio setpoint not met - CoolReheat humidity control

					if ( PartLoadFrac > 1.0 ) {
						PartLoadFrac = 1.0;
					} else if ( PartLoadFrac < 0.0 ) {
						PartLoadFrac = 0.0;
					}
				} else if ( SELECT_CASE_var == Coil_CoolingAirToAirVariableSpeed ) { // Coil:Cooling:DX:VariableSpeed
					//variable-speed air-to-air cooling coil, begin -------------------------
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

					SimVariableSpeedCoils( CompName, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

					VSCoilIndex = DXCoolingSystem( DXSystemNum ).CoolingCoilIndex;
					NumOfSpeeds = VarSpeedCoil( VSCoilIndex ).NumOfSpeeds;

					NoOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );
					NoLoadHumRatOut = VarSpeedCoil( VSCoilIndex ).OutletAirHumRat;

					// Get full load result
					PartLoadFrac = 1.0;

					SpeedNum = NumOfSpeeds;
					SpeedRatio = 1.0;
					QZnReq = 0.001; //to indicate the coil is running
					SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

					FullLoadHumRatOut = VarSpeedCoil( VSCoilIndex ).OutletAirHumRat;
					FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

					//         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
					if ( ( NoOutput - ReqOutput ) < Acc ) {
						PartLoadFrac = 0.0;
						SpeedNum = 1;
						SpeedRatio = 0.0;
						//         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
						//         run the compressor at PartLoadFrac = 1.
					} else if ( ( FullOutput - ReqOutput ) > Acc ) {
						PartLoadFrac = 1.0;
						SpeedNum = NumOfSpeeds;
						SpeedRatio = 1.0;
						//         Else find the PLR to meet the load
					} else {
						//           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above. If this temp is
						//           greater than the desired outlet temp, then run the compressor at PartLoadFrac = 1, otherwise find the operating PLR.
						OutletTempDXCoil = VarSpeedCoil( VSCoilIndex ).OutletAirDBTemp;
						if ( OutletTempDXCoil > DesOutTemp ) {
							PartLoadFrac = 1.0;
							SpeedNum = NumOfSpeeds;
							SpeedRatio = 1.0;
						} else {
							PartLoadFrac = 1.0;
							SpeedNum = 1;
							SpeedRatio = 1.0;
							QZnReq = 0.001; //to indicate the coil is running
							SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

							TempSpeedOut = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );
							TempSpeedReqst = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

							if ( ( TempSpeedOut - TempSpeedReqst ) > Acc ) {
								// Check to see which speed to meet the load
								PartLoadFrac = 1.0;
								SpeedRatio = 1.0;
								for ( I = 2; I <= NumOfSpeeds; ++I ) {
									SpeedNum = I;
									SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

									TempSpeedOut = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );
									TempSpeedReqst = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( DXCoolingSystem( DXSystemNum ).DesiredOutletTemp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( OutletNode ).HumRat ) );

									if ( ( TempSpeedOut - TempSpeedReqst ) < Acc ) {
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
										if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = TempSpeedReqst / TempSpeedOut;
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}
							} else {
								Par( 1 ) = double( VSCoilIndex );
								Par( 2 ) = DesOutTemp;
								Par( 5 ) = double( FanOpMode );
								SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadFrac, VSCoilCyclingResidual, 1.0e-10, 1.0, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = TempSpeedReqst / TempSpeedOut;
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}
							}
						}
					}

					//         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
					//         else use operating humidity ratio to test against humidity setpoint
					if ( PartLoadFrac == 0.0 ) {
						OutletHumRatDXCoil = NoLoadHumRatOut;
					} else {
						OutletHumRatDXCoil = VarSpeedCoil( DXCoolingSystem( DXSystemNum ).CoolingCoilIndex ).OutletAirHumRat;
					}

					// If humidity setpoint is not satisfied and humidity control type is CoolReheat,
					// then overcool to meet moisture load

					if ( ( OutletHumRatDXCoil > DesOutHumRat ) && ( PartLoadFrac < 1.0 ) && ( DXCoolingSystem( DXSystemNum ).DehumidControlType == DehumidControl_CoolReheat ) ) {

						//           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
						//           do not run the compressor
						if ( ( NoLoadHumRatOut - DesOutHumRat ) < HumRatAcc ) {
							//PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
							//           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
							//           run the compressor at PartLoadFrac = 1.
						} else if ( ( DesOutHumRat - FullLoadHumRatOut ) < HumRatAcc ) {
							PartLoadFrac = 1.0;
							//           Else find the PLR to meet the load
						} else {
							PartLoadFrac = 1.0;
							SpeedNum = 1;
							SpeedRatio = 1.0;
							QZnReq = 0.001; //to indicate the coil is running
							SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

							TempSpeedOut = VarSpeedCoil( VSCoilIndex ).OutletAirHumRat;

							if ( ( DesOutHumRat - FullLoadHumRatOut ) < HumRatAcc ) {
								// Check to see which speed to meet the load
								PartLoadFrac = 1.0;
								SpeedRatio = 1.0;
								for ( I = 2; I <= NumOfSpeeds; ++I ) {
									SpeedNum = I;
									SimVariableSpeedCoils( CompName, VSCoilIndex, FanOpMode, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, On, PartLoadFrac, SpeedNum, SpeedRatio, QZnReq, QLatReq, OnOffAirFlowRatio );

									TempSpeedOut = VarSpeedCoil( VSCoilIndex ).OutletAirHumRat;

									if ( ( DesOutHumRat - TempSpeedOut ) > HumRatAcc ) {
										SpeedNum = I;
										break;
									}
								}
								Par( 1 ) = double( VSCoilIndex );
								Par( 2 ) = DesOutHumRat;
								Par( 5 ) = double( FanOpMode );
								Par( 3 ) = double( SpeedNum );
								SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, SpeedRatio, VSCoilSpeedHumResidual, 1.0e-10, 1.0, Par );

								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio  = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									PartLoadFrac = TempSpeedReqst / TempSpeedOut;
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}
							} else {
								Par( 1 ) = double( VSCoilIndex );
								Par( 2 ) = DesOutHumRat;
								Par( 5 ) = double( FanOpMode );
								SolveRegulaFalsi( HumRatAcc, MaxIte, SolFla, PartLoadFrac, VSCoilCyclingHumResidual, 1.0e-10, 1.0, Par );
								if ( SolFla == -1 ) {
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIter < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIter;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio   = " + RoundSigDigits( ( ReqOutput / FullOutput ), 3 ) );
											ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIterIndex, PartLoadFrac, PartLoadFrac );
									}
								} else if ( SolFla == -2 ) {
									//               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
									if ( NoLoadHumRatOut - FullLoadHumRatOut != 0.0 ) {
										PartLoadFrac = ( NoLoadHumRatOut - DesOutHumRat ) / ( NoLoadHumRatOut - FullLoadHumRatOut );
									} else {
										PartLoadFrac = 1.0;
									}
									if ( ! WarmupFlag ) {
										if ( DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFail < 1 ) {
											++DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFail;
											ShowWarningError( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + DXCoolingSystem( DXSystemNum ).Name );
											ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
											ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
										}
										ShowRecurringWarningErrorAtEnd( DXCoolingSystem( DXSystemNum ).DXCoolingSystemType + " \"" + DXCoolingSystem( DXSystemNum ).Name + "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics follow.", DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFailIndex, PartLoadFrac, PartLoadFrac );
									}
								}
							}
						}
					} // End if humidity ratio setpoint not met - CoolReheat humidity control

					if ( PartLoadFrac > 1.0 ) {
						PartLoadFrac = 1.0;
					} else if ( PartLoadFrac < 0.0 ) {
						PartLoadFrac = 0.0;
					}
					//variable-speed air-to-air cooling coil, end -------------------------

				} else if ( SELECT_CASE_var == CoilDX_PackagedThermalStorageCooling ) {

					ControlTESIceStorageTankCoil( CompName, DXCoolingSystem( DXSystemNum ).CoolingCoilIndex, DXCoolingSystem( DXSystemNum ).DXCoolingSystemType,
						DXCoolingSystem( DXSystemNum ).FanOpMode, DesOutTemp, DesOutHumRat, PartLoadFrac,
						DXCoolingSystem( DXSystemNum ).TESOpMode, DXCoolingSystem( DXSystemNum ).DehumidControlType,
						DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIter, DXCoolingSystem( DXSystemNum ).DXCoilSensPLRIterIndex,
						DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFail, DXCoolingSystem( DXSystemNum ).DXCoilSensPLRFailIndex,
						DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIter, DXCoolingSystem( DXSystemNum ).DXCoilLatPLRIterIndex,
						DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFail, DXCoolingSystem( DXSystemNum ).DXCoilLatPLRFailIndex );

				} else {
					ShowFatalError( "ControlDXSystem: Invalid DXCoolingSystem coil type = " + DXCoolingSystem( DXSystemNum ).CoolingCoilType );

				}}
			} // End of cooling load type (sensible or latent) if block
		} // End of If DXCoolingSystem is scheduled on and there is flow
		//Set the final results
		DXCoolingSystem( DXSystemNum ).PartLoadFrac = PartLoadFrac;
		DXCoolingSystem( DXSystemNum ).SpeedRatio = SpeedRatio;
		DXCoolingSystem( DXSystemNum ).CycRatio = CycRatio;
		DXCoolingSystem( DXSystemNum ).DehumidificationMode = DehumidMode;
		DXCoolingSystem( DXSystemNum ).SpeedNum = SpeedNum;

	}

	Real64
	DXCoilVarSpeedResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp).
		// DX Coil output depends on the compressor speed which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcMultiSpeedDXCoil to get outlet temperature at the given compressor speed
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcMultiSpeedDXCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]

		CoilIndex = int( Par( 1 ) );
		CalcMultiSpeedDXCoil( CoilIndex, SpeedRatio, 1.0 );
		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DXCoilVarSpeedHumRatResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat).
		// DX Coil output depends on the compressor speed which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcMultiSpeedDXCoil to get outlet humidity ratio at the given compressor speed
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::CalcMultiSpeedDXCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]

		CoilIndex = int( Par( 1 ) );
		CalcMultiSpeedDXCoil( CoilIndex, SpeedRatio, 1.0 );
		OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	DXCoilCyclingResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the cycling ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcMultiSpeedDXCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]

		CoilIndex = int( Par( 1 ) );
		CalcMultiSpeedDXCoil( CoilIndex, 0.0, CycRatio );
		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DXCoilCyclingHumRatResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the cycling ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::CalcMultiSpeedDXCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]

		CoilIndex = int( Par( 1 ) );
		CalcMultiSpeedDXCoil( CoilIndex, 0.0, CycRatio );
		OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   November 2003
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
		using DXCoils::CalcDoe2DXCoil;

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

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );
		CalcDoe2DXCoil( CoilIndex, On, true, PartLoadRatio, FanOpMode );
		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DOE2DXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDoe2DXCoil to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::CalcDoe2DXCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		int FanOpMode; // Supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );
		CalcDoe2DXCoil( CoilIndex, On, true, PartLoadRatio, FanOpMode );
		OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         M. J. Witte, GARD Analytics, Inc.
		//       DATE WRITTEN   February 2005
		//                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::SimDXCoilMultiMode;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(3) = dehumidification mode (0=normal, 1=enhanced)
		// par(4) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		int DehumidMode; // dehumidification mode (par3)
		int FanOpMode; // supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		DehumidMode = int( Par( 3 ) );
		FanOpMode = int( Par( 4 ) );
		SimDXCoilMultiMode( "", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode );
		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	MultiModeDXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimDXCoilMultiMode to get outlet humidity ratio at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletHumRat;
		using DXCoils::SimDXCoilMultiMode;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(3) = dehumidification mode (0=normal, 1=enhanced)
		// par(4) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		int DehumidMode; // dehumidification mode (par3)
		int FanOpMode; // supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		DehumidMode = int( Par( 3 ) );
		FanOpMode = int( Par( 4 ) );
		SimDXCoilMultiMode( "", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode );
		OutletAirHumRat = DXCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	Real64
	HXAssistedCoolCoilTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   November 2003
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired outlet temp - actual outlet temp)
		//  DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcHXAssistedCoolingCoil to get outlet temperature at the given part load ratio
		//  and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp;
		using HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
		// par(4) = HX control (On/Off)
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		bool FirstHVACIteration; // FirstHVACIteration flag
		bool HXUnitOn; // flag to enable heat exchanger heat recovery
		int FanOpMode; // Supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		HXUnitOn = ( Par( 4 ) == 1.0 );
		FanOpMode = int( Par( 5 ) );
		CalcHXAssistedCoolingCoil( CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode );
		OutletAirTemp = HXAssistedCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;
		return Residuum;

	}

	Real64
	HXAssistedCoolCoilHRResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   January 2008
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired outlet humrat - actual outlet humrat)
		//  DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcHXAssistedCoolingCoil to get outlet humidity ratio at the given part load ratio
		//  and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat;
		using HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet humidity ratio [kg/kg]
		// par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
		// par(4) = HX control (On/Off)
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		bool FirstHVACIteration; // FirstHVACIteration flag
		bool HXUnitOn; // flag to enable heat exchanger heat recovery
		int FanOpMode; // Supply air fan operating mode

		CoilIndex = int( Par( 1 ) );
		// FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
		FirstHVACIteration = ( Par( 3 ) == 1.0 );
		HXUnitOn = ( Par( 4 ) == 1.0 );
		FanOpMode = int( Par( 5 ) );
		CalcHXAssistedCoolingCoil( CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode, _, EconomizerFlag );
		OutletAirHumRat = HXAssistedCoilOutletHumRat( CoilIndex );
		Residuum = Par( 2 ) - OutletAirHumRat;
		return Residuum;

	}

	Real64
	TESCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// TES Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls appropriate calculation routine depending on operating mode
		// to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using PackagedThermalStorageCoil::CalcTESCoilCoolingOnlyMode;
		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndChargeMode;
		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndDischargeMode;
		using PackagedThermalStorageCoil::CalcTESCoilDischargeOnlyMode;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(3) = TES coil operating mode
		// par(4) = outlet node number
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
		int TESOpMode;
		int OutletNodeNum;

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );
		OutletNodeNum = int( Par( 4 ) );
		TESOpMode = int( Par( 3 ) );

		{ auto const SELECT_CASE_var( TESOpMode );
		if ( SELECT_CASE_var == CoolingOnlyMode ) {
			CalcTESCoilCoolingOnlyMode( CoilIndex, FanOpMode, PartLoadRatio );
		} else if ( SELECT_CASE_var == CoolingAndChargeMode ) {
			CalcTESCoilCoolingAndChargeMode( CoilIndex, FanOpMode, PartLoadRatio );
		} else if ( SELECT_CASE_var == CoolingAndDischargeMode ) {
			CalcTESCoilCoolingAndDischargeMode( CoilIndex, FanOpMode, PartLoadRatio );
		} else if ( SELECT_CASE_var == DischargeOnlyMode ) {
			CalcTESCoilDischargeOnlyMode( CoilIndex, PartLoadRatio );
		}}

		OutletAirTemp = Node( OutletNodeNum ).Temp;
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	TESCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2013
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet humrat - actual outlet humrat)
		// TES Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls appropriate calculation routine depending on operating mode
		// to get outlet hum rat at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using PackagedThermalStorageCoil::CalcTESCoilCoolingOnlyMode;
		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndChargeMode;
		using PackagedThermalStorageCoil::CalcTESCoilCoolingAndDischargeMode;
		using PackagedThermalStorageCoil::CalcTESCoilDischargeOnlyMode;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet hum rat [kg_h20/kg_dryair]
		// par(3) = TES coil operating mode
		// par(4) = outlet node number
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg_H20/Kg_dryair]
		int FanOpMode; // Supply air fan operating mode
		int TESOpMode;
		int OutletNodeNum;

		CoilIndex = int( Par( 1 ) );
		FanOpMode = int( Par( 5 ) );
		OutletNodeNum = int( Par( 4 ) );
		TESOpMode = int( Par( 3 ) );

		{ auto const SELECT_CASE_var( TESOpMode );
		if ( SELECT_CASE_var == CoolingOnlyMode ) {
			CalcTESCoilCoolingOnlyMode( CoilIndex, FanOpMode, PartLoadRatio );
		} else if ( SELECT_CASE_var == CoolingAndChargeMode ) {
			CalcTESCoilCoolingAndChargeMode( CoilIndex, FanOpMode, PartLoadRatio );
		} else if ( SELECT_CASE_var == CoolingAndDischargeMode ) {
			CalcTESCoilCoolingAndDischargeMode( CoilIndex, FanOpMode, PartLoadRatio );
		} else if ( SELECT_CASE_var == DischargeOnlyMode ) {
			CalcTESCoilDischargeOnlyMode( CoilIndex, PartLoadRatio );
		}}

		OutletAirHumRat = Node( OutletNodeNum ).HumRat;
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;
	}

	void
	FrostControlSetPointLimit(
		int const DXSystemNum, // dx cooling coil system index
		Real64 & TempSetPoint, // temperature setpoint of the sensor node
		Real64 & HumRatSetPoint, // humidity ratio setpoint of the sensor node
		Real64 const BaroPress, // baromtric pressure, Pa [N/m^2]
		Real64 const TfrostControl, // minimum temperature limit for forst control
		int const ControlMode // temperature or humidity control mode
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED
		//       RE-ENGINEERED
		// PURPOSE OF THIS SUBROUTINE:
		// Controls the forst formation condition based on user specified minimum DX coil outlet
		// air temperature. Resets the cooling setpoint based on the user specified limiting
		// temperature for frost control.
		// METHODOLOGY EMPLOYED:
		//  na
		// REFERENCES:
		//  na
		// Using/Aliasing
		using Psychrometrics::PsyWFnTdpPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const RunOnSensible( 1 ); // identifier for temperature (sensible load) control
		int const RunOnLatent( 2 ); // identifier for humidity (latent load) control
		static std::string const RoutineName( "FrostControlSetPointLimit" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 HumRatioSat; // saturation humidity ratio at forst control temperature
		Real64 AirMassFlow; // air masss flow rate through the DX coil

		AirMassFlow = Node( DXCoolingSystem( DXSystemNum ).DXCoolingCoilInletNodeNum ).MassFlowRate;
		if ( ControlMode == RunOnSensible && AirMassFlow > MinAirMassFlow && TempSetPoint < Node( DXCoolingSystem( DXSystemNum ).DXCoolingCoilInletNodeNum ).Temp ) {
			if ( TempSetPoint < TfrostControl ) {
				TempSetPoint = TfrostControl;
				DXCoolingSystem( DXSystemNum ).FrostControlStatus = 1;
			}
		} else if ( ControlMode == RunOnLatent && AirMassFlow > MinAirMassFlow && HumRatSetPoint < Node( DXCoolingSystem( DXSystemNum ).DXCoolingCoilInletNodeNum ).HumRat ) {
			HumRatioSat = PsyWFnTdpPb( TfrostControl, BaroPress, RoutineName );
			if ( HumRatioSat > HumRatSetPoint ) {
				HumRatSetPoint = HumRatioSat;
				DXCoolingSystem( DXSystemNum ).FrostControlStatus = 2;
			}
		} else {
			DXCoolingSystem( DXSystemNum ).FrostControlStatus = 0;
		}
	}

	void
	CheckDXCoolingCoilInOASysExists( std::string const & DXCoilSysName )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   Feb 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// After making sure get input is done, checks if the Coil System DX coil is in the
		// OA System.  If exists then the DX cooling coil is 100% DOAS DX coil.
		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DXCoils::SetDXCoilTypeData;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoolSysNum;

		if ( GetInputFlag ) { //First time subroutine has been entered
			GetDXCoolingSystemInput();
			GetInputFlag = false;
		}

		DXCoolSysNum = 0;
		if ( NumDXSystem > 0 ) {
			DXCoolSysNum = FindItemInList( DXCoilSysName, DXCoolingSystem );
			if ( DXCoolSysNum > 0 && DXCoolingSystem( DXCoolSysNum ).ISHundredPercentDOASDXCoil ) {
				//DXCoolingSystem(DXCoolSysNum)%ISHundredPercentDOASDXCoil = .TRUE.
				SetDXCoilTypeData( DXCoolingSystem( DXCoolSysNum ).CoolingCoilName );
			}
		}

	}

	void
	GetCoolingCoilTypeNameAndIndex(
		std::string const & DXCoilSysName,
		int & CoolCoilType,
		int & CoolCoilIndex,
		std::string & CoolCoilName,
		bool & EP_UNUSED( ErrFound )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Aug 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// After making sure get input is done, checks if the Coil System DX coil is in the
		// OA System.  If exists then the DX cooling coil is 100% DOAS DX coil.
		// METHODOLOGY EMPLOYED:
		// na
		// REFERENCES:
		// na
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DXCoils::SetDXCoilTypeData;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoolSysNum;

		if ( GetInputFlag ) { //First time subroutine has been entered
			GetDXCoolingSystemInput();
			GetInputFlag = false;
		}

		DXCoolSysNum = 0;
		if ( NumDXSystem > 0 ) {
			DXCoolSysNum = FindItemInList( DXCoilSysName, DXCoolingSystem );
			if ( DXCoolSysNum > 0 && DXCoolSysNum <= NumDXSystem ) {
				CoolCoilType = DXCoolingSystem( DXCoolSysNum ).CoolingCoilType_Num;
				CoolCoilIndex = DXCoolingSystem( DXCoolSysNum ).CoolingCoilIndex;
				CoolCoilName = DXCoolingSystem( DXCoolSysNum ).CoolingCoilName;
			}
		}

	}

	//******************************************************************************

	Real64
	VSCoilCyclingResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Feb, 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (Temperature) by comparing with the output of variable-speed DX coil
		// interate part-load ratio

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
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Feb, 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (Temperature) by comparing with the output of variable-speed DX coil
		// interate speed ratio between two neighboring speeds
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

	Real64
	VSCoilCyclingHumResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Feb, 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (Humidity) by comparing with the output of variable-speed DX coil
		// interate part-load ratio
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
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
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

		OutletAirHumRat = VarSpeedCoil( CoilIndex ).OutletAirHumRat;
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;

	}

	//******************************************************************************

	Real64
	VSCoilSpeedHumResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Feb, 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (Humidity) by comparing with the output of variable-speed DX coil
		// interate speed ratio between two neighboring speeds

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
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
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

		OutletAirHumRat = VarSpeedCoil( CoilIndex ).OutletAirHumRat;
		Residuum = Par( 2 ) - OutletAirHumRat;

		return Residuum;

	}

	//        End of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

} // HVACDXSystem

} // EnergyPlus
