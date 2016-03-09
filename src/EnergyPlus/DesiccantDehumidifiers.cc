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
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DesiccantDehumidifiers.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HeatRecovery.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace DesiccantDehumidifiers {

	// Module containing the routines dealing with dehumidifiers

	// MODULE INFORMATION:
	//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
	//                      for Gas Research Institute
	//       DATE WRITTEN   March 2001
	//       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
	//                        Add new control type option:
	//                          NODE LEAVING HUMRAT SETPOINT:BYPASS
	//                        Change existing control type to:
	//                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
	//                        Work supported by ASHRAE research project 1254-RP
	//                      June 2007 R. Raustad, FSEC
	//                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
	//                      Jan 2012  B. Nigusse, FSEC
	//                        Added steam and hot water heating coils

	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and routines required to model desiccant dehumidifier
	// components in the EnergyPlus HVAC simulation

	// METHODOLOGY EMPLOYED:
	// The desiccant dehumidifier emcompasses not just the component but also its
	// control. The desiccant dehumidifier removes moisture from its air inlet to meet
	// the HumRatMax setpoint at its exit node. The HumRatMax is set by
	// an external setpoint manager or is a fixed user input.

	// REFERENCES: na

	// OTHER NOTES: This module is based substantially on the Humidifiers module.
	//              authored by Fred Buhl.
	//              Development of portions of this module was funded by the Gas Research Institute.
	//              (Please see copyright and disclaimer information at end of module)

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataLoopNode;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::StdRhoAir;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::OnOffFanPartLoadFraction;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::BlowThru;
	using DataHVACGlobals::DrawThru;
	using DataHVACGlobals::Coil_HeatingWater;
	using DataHVACGlobals::Coil_HeatingSteam;
	using DataHVACGlobals::Coil_HeatingGas;
	using DataHVACGlobals::Coil_HeatingElectric;
	using DataHeatBalance::HeatReclaimDXCoil;
	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using namespace HeatingCoils;
	using namespace Fans;
	using namespace CurveManager;
	using namespace Psychrometrics;
	using General::TrimSigDigits;
	using General::RoundSigDigits;
	using FluidProperties::GetSatDensityRefrig;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Desiccant dehumidifier type
	int const Solid( 1 ); // DESICCANT DEHUMIDIFIER:SOLID = 1
	int const Generic( 2 ); // DESICCANT DEHUMIDIFIER = 2
	//  Desiccant heat exchanger type
	int const BalancedHX( 1 ); // HeatExchanger:Desiccant:BalancedFlow = 1
	// Desiccant control type
	int const FixedHumratBypass( 1 ); // FIXED LEAVING HUMRAT SETPOINT:BYPASS = 1
	int const NodeHumratBypass( 2 ); // NODE LEAVING HUMRAT SETPOINT:BYPASS  = 2
	// Preheat selection
	int const No( 0 ); // Condenser waste heat NOT reclaimed for desiccant regeneration
	int const Yes( 1 ); // Condenser waste heat reclaimed for desiccant regeneration
	// Performance Model
	int const PM_Default( 1 ); // Performance Model = default
	int const PM_UserCurves( 2 ); // Performance Model = user curve
	static std::string const fluidNameSteam( "STEAM" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumDesicDehums; // number of desiccant dehumidifiers of all types
	int NumSolidDesicDehums; // number of solid desiccant dehumidifiers
	int NumGenericDesicDehums; // number of generic desiccant dehumidifiers
	Real64 TempSteamIn( 100.0 ); // steam coil steam inlet temperature

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>
	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool GetInputDesiccantDehumidifier( true ); // First time, input is "gotten"
		bool InitDesiccantDehumidifierOneTimeFlag( true );
	}

	// Name Public routines, optionally name Private routines within this module

	// Object Data
	Array1D< DesiccantDehumidifierData > DesicDehum;

	// Functions

	void
	SimDesiccantDehumidifier(
		std::string const & CompName, // name of the dehumidifier unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
		//                      for Gas Research Institute
		//       DATE WRITTEN   March 2001
		//       MODIFIED       June 2007, R. Raustad, Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manage the simulation of an air dehumidifier

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// NA

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
		int DesicDehumNum; // index of solid desiccant unit being simulated
		Real64 HumRatNeeded; // process air leaving humidity ratio set by controller [kg water/kg air]

		if ( GetInputDesiccantDehumidifier ) {
			GetDesiccantDehumidifierInput();
			GetInputDesiccantDehumidifier = false;
		}

		// Get the desiccant dehumidifier unit index
		if ( CompIndex == 0 ) {
			DesicDehumNum = FindItemInList( CompName, DesicDehum );
			if ( DesicDehumNum == 0 ) {
				ShowFatalError( "SimDesiccantDehumidifier: Unit not found=" + CompName );
			}
			CompIndex = DesicDehumNum;
		} else {
			DesicDehumNum = CompIndex;
			if ( DesicDehumNum > NumDesicDehums || DesicDehumNum < 1 ) {
				ShowFatalError( "SimDesiccantDehumidifier:  Invalid CompIndex passed=" + TrimSigDigits( DesicDehumNum ) + ", Number of Units=" + TrimSigDigits( NumDesicDehums ) + ", Entered Unit name=" + CompName );
			}
			if ( CompName != DesicDehum( DesicDehumNum ).Name ) {
				ShowFatalError( "SimDesiccantDehumidifier: Invalid CompIndex passed=" + TrimSigDigits( DesicDehumNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + DesicDehum( DesicDehumNum ).Name );
			}
		}

		InitDesiccantDehumidifier( DesicDehumNum, FirstHVACIteration );

		ControlDesiccantDehumidifier( DesicDehumNum, HumRatNeeded, FirstHVACIteration );

		// call the correct dehumidifier calculation routine
		{ auto const SELECT_CASE_var( DesicDehum( DesicDehumNum ).DehumTypeCode );

		if ( SELECT_CASE_var == Solid ) {

			CalcSolidDesiccantDehumidifier( DesicDehumNum, HumRatNeeded, FirstHVACIteration );

		} else if ( SELECT_CASE_var == Generic ) {

			CalcGenericDesiccantDehumidifier( DesicDehumNum, HumRatNeeded, FirstHVACIteration );

		} else {
			ShowFatalError( "Invalid type, Desiccant Dehumidifer=" + DesicDehum( DesicDehumNum ).DehumType );

		}}

		UpdateDesiccantDehumidifier( DesicDehumNum );

		ReportDesiccantDehumidifier( DesicDehumNum );

	}

	void
	GetDesiccantDehumidifierInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
		//                      for Gas Research Institute
		//       DATE WRITTEN   March 2001
		//       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add new control type option:
		//                          NODE LEAVING HUMRAT SETPOINT:BYPASS
		//                        Change existing control type to:
		//                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
		//                        Work supported by ASHRAE research project 1254-RP
		//                      June 2007 R. Raustad, FSEC
		//                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for humidifiers and stores it in dehumidifier data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using BranchNodeConnections::TestCompSet;
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		using DXCoils::GetCoilCondenserInletNode;
		using DXCoils::GetDXCoilBypassedFlowFrac;
		using DXCoils::GetDXCoilIndex;
		auto & GetDXCoilCapacity( DXCoils::GetCoilCapacity );
		using HeatRecovery::GetSupplyOutletNode;
		using HeatRecovery::GetSupplyInletNode;
		using HeatRecovery::GetSecondaryInletNode;
		using HeatRecovery::GetSecondaryOutletNode;
		auto & GetHeatingCoilInletNode( HeatingCoils::GetCoilInletNode );
		auto & GetHeatingCoilOutletNode( HeatingCoils::GetCoilOutletNode );
		auto & GetHeatReclaimSourceIndexNum( HeatingCoils::GetHeatReclaimSourceIndex );
		auto & GetHeatingCoilIndex( HeatingCoils::GetCoilIndex );
		auto & GetHeatingCoilControlNodeNum( HeatingCoils::GetCoilControlNodeNum );
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilMaxWaterFlowRate;
		using WaterCoils::GetWaterCoilIndex;
		auto & GetWaterCoilInletNode( WaterCoils::GetCoilInletNode );
		auto & GetWaterCoilOutletNode( WaterCoils::GetCoilOutletNode );
		auto & GetSteamCoilAirInletNode( SteamCoils::GetCoilAirInletNode );
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilAirOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		using SteamCoils::GetTypeOfCoil;
		using SteamCoils::GetSteamCoilControlNodeNum;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using WaterCoils::SetWaterCoilData;
		using SteamCoils::SetSteamCoilData;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetDesiccantDehumidifierInput: " ); // include trailing blank space
		static std::string const dehumidifierDesiccantNoFans( "Dehumidifier:Desiccant:NoFans" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DesicDehumIndex; // Loop index
		int DesicDehumNum; // Current desiccant dehumidifier number
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static bool ErrorsFound2( false ); // Set to true if errors in input, fatal at end of routine
		static bool ErrorsFoundGeneric( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool OANodeError; // Flag for check on outside air node
		std::string RegenFanInlet; // Desiccant system regeneration air fan inlet node
		std::string RegenFanOutlet; // Desiccant system regeneration air fan outlet node
		std::string RegenCoilInlet; // Desiccant system regeneration air heater inlet node
		std::string RegenCoilOutlet; // Desiccant system regeneration air heater outlet node
		std::string ProcAirInlet; // HX process air inlet node
		std::string ProcAirOutlet; // HX process air outlet node
		std::string RegenAirInlet; // HX regeneration air inlet node
		std::string RegenAirOutlet; // HX regeneration air outlet node
		std::string CurrentModuleObject; // for ease in getting objects
		int DesuperHeaterIndex; // Index of desuperheater heating coil
		int RegenCoilControlNodeNum; // Control node number of regen heating coil
		Real64 CoilBypassedFlowFrac; // Bypass air fraction for multimode DX coils
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file
		int RegenCoilAirInletNode; // regen heating coil air inlet node number
		int RegenCoilAirOutletNode; // regen heating coil air outlet node number
		bool errFlag; // local error flag
		std::string RegenCoilType; // Regen heating coil type
		std::string RegenCoilName; // Regen heating coil name
		static Real64 SteamDensity( 0.0 ); // density of steam at 100C
		int SteamIndex; // steam coil Index
		bool RegairHeatingCoilFlag( false ); // local error flag

		NumSolidDesicDehums = GetNumObjectsFound( dehumidifierDesiccantNoFans );
		NumGenericDesicDehums = GetNumObjectsFound( "Dehumidifier:Desiccant:System" );
		NumDesicDehums = NumSolidDesicDehums + NumGenericDesicDehums;
		// allocate the data array
		DesicDehum.allocate( NumDesicDehums );

		GetObjectDefMaxArgs( dehumidifierDesiccantNoFans, TotalArgs, NumAlphas, NumNumbers );
		MaxNums = max( MaxNums, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Dehumidifier:Desiccant:System", TotalArgs, NumAlphas, NumNumbers );
		MaxNums = max( MaxNums, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		Numbers.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		// loop over solid desiccant dehumidifiers and load the input data
		CurrentModuleObject = dehumidifierDesiccantNoFans;
		for ( DesicDehumIndex = 1; DesicDehumIndex <= NumSolidDesicDehums; ++DesicDehumIndex ) {
			RegenCoilAirInletNode = 0;
			RegenCoilAirOutletNode = 0;
			GetObjectItem( CurrentModuleObject, DesicDehumIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			DesicDehumNum = DesicDehumIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DesicDehum, DesicDehumNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			DesicDehum( DesicDehumNum ).Name = Alphas( 1 );
			DesicDehum( DesicDehumNum ).DehumType = CurrentModuleObject;
			DesicDehum( DesicDehumNum ).DehumTypeCode = Solid;
			DesicDehum( DesicDehumNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DesicDehum( DesicDehumNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DesicDehum( DesicDehumNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DesicDehum( DesicDehumNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			// For node connections, this object is both a parent and a non-parent, because the
			// Desiccant wheel is not called out as a separate component, its nodes must be connected
			// as ObjectIsNotParent.  But for the Regen fan, the nodes are connected as ObjectIsParent
			DesicDehum( DesicDehumNum ).ProcAirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DesicDehum( DesicDehumNum ).ProcAirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			DesicDehum( DesicDehumNum ).RegenAirInNode = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			DesicDehum( DesicDehumNum ).RegenFanInNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Internal, 2, ObjectIsParent );

			if ( SameString( Alphas( 7 ), "LEAVING HUMRAT:BYPASS" ) ) {
				ShowWarningError( RoutineName + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
				ShowContinueError( "Obsolete " + cAlphaFields( 7 ) + " = " + Alphas( 7 ) );
				ShowContinueError( "setting to LeavingMaximumHumidityRatioSetpoint" );
				DesicDehum( DesicDehumNum ).ControlType = FixedHumratBypass;
			}
			if ( SameString( Alphas( 7 ), "LeavingMaximumHumidityRatioSetpoint" ) ) DesicDehum( DesicDehumNum ).ControlType = FixedHumratBypass;
			if ( SameString( Alphas( 7 ), "SystemNodeMaximumHumidityRatioSetpoint" ) ) DesicDehum( DesicDehumNum ).ControlType = NodeHumratBypass;
			if ( DesicDehum( DesicDehumNum ).ControlType == 0 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
				ShowContinueError( "Invalid " + cAlphaFields( 7 ) + " = " + Alphas( 7 ) );
				ShowContinueError( "setting to LeavingMaximumHumidityRatioSetpoint" );
				DesicDehum( DesicDehumNum ).ControlType = FixedHumratBypass;
			}
			DesicDehum( DesicDehumNum ).HumRatSet = Numbers( 1 );
			DesicDehum( DesicDehumNum ).NomProcAirVolFlow = Numbers( 2 );
			DesicDehum( DesicDehumNum ).NomProcAirVel = Numbers( 3 );

			DesicDehum( DesicDehumNum ).RegenCoilType = Alphas( 8 );
			DesicDehum( DesicDehumNum ).RegenCoilName = Alphas( 9 );
			RegenCoilType = Alphas( 8 );
			RegenCoilName = Alphas( 9 );

			if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Electric" ) || SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Gas" ) ) {
				if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Electric" ) ) DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingElectric;
				if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Gas" ) ) DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingGas;
				ValidateComponent( DesicDehum( DesicDehumNum ).RegenCoilType, DesicDehum( DesicDehumNum ).RegenCoilName, ErrorsFound2, CurrentModuleObject + '=' + Alphas( 1 ) );
				if ( ErrorsFound2 ) ErrorsFound = true;
				GetHeatingCoilIndex( DesicDehum( DesicDehumNum ).RegenCoilName, DesicDehum( DesicDehumNum ).RegenCoilIndex, ErrorsFound2 );
				if ( ErrorsFound2 ) ErrorsFound = true;

			} else if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Water" ) ) {
				DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingWater;
				ValidateComponent( RegenCoilType, RegenCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				} else { // mine data from heating coil object
					errFlag = false;
					DesicDehum( DesicDehumNum ).RegenCoilIndex = GetWaterCoilIndex( "COIL:HEATING:WATER", RegenCoilName, errFlag );
					if ( DesicDehum( DesicDehumNum ).RegenCoilIndex == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + " illegal " + cAlphaFields( 9 ) + " = " + RegenCoilName );
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

					// Get the Heating Coil Hot water Inlet or control Node number
					errFlag = false;
					DesicDehum( DesicDehumNum ).CoilControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", RegenCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

					// Get the Regeneration Heating Coil hot water max volume flow rate
					errFlag = false;
					DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", RegenCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

					// Get the Regeneration Heating Coil Inlet Node
					errFlag = false;
					RegenCoilAirInletNode = GetWaterCoilInletNode( "Coil:Heating:Water", RegenCoilName, errFlag );
					DesicDehum( DesicDehumNum ).RegenCoilInletNode = RegenCoilAirInletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

					// Get the Regeneration Heating Coil Outlet Node
					errFlag = false;
					RegenCoilAirOutletNode = GetWaterCoilOutletNode( "Coil:Heating:Water", RegenCoilName, errFlag );
					DesicDehum( DesicDehumNum ).RegenCoilOutletNode = RegenCoilAirOutletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

				}
			} else if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Steam" ) ) {
				DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingSteam;
				ValidateComponent( Alphas( 8 ), RegenCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				} else { // mine data from the regeneration heating coil object

					errFlag = false;
					DesicDehum( DesicDehumNum ).RegenCoilIndex = GetSteamCoilIndex( "COIL:HEATING:STEAM", RegenCoilName, errFlag );
					if ( DesicDehum( DesicDehumNum ).RegenCoilIndex == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + " illegal " + cAlphaFields( 9 ) + " = " + RegenCoilName );
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

					// Get the regeneration Heating Coil steam inlet node number
					errFlag = false;
					DesicDehum( DesicDehumNum ).CoilControlNode = GetCoilSteamInletNode( "Coil:Heating:Steam", RegenCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

					// Get the regeneration heating Coil steam max volume flow rate
					DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate( DesicDehum( DesicDehumNum ).RegenCoilIndex, errFlag );
					if ( DesicDehum( DesicDehumNum ).MaxCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, dehumidifierDesiccantNoFans );
						DesicDehum( DesicDehumNum ).MaxCoilFluidFlow *= SteamDensity;
					}

					// Get the regeneration heating Coil Inlet Node
					errFlag = false;
					RegenCoilAirInletNode = GetSteamCoilAirInletNode( DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilName, errFlag );
					DesicDehum( DesicDehumNum ).RegenCoilInletNode = RegenCoilAirInletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

					// Get the regeneration heating Coil Outlet Node
					errFlag = false;
					RegenCoilAirOutletNode = GetCoilAirOutletNode( DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilName, errFlag );
					DesicDehum( DesicDehumNum ).RegenCoilOutletNode = RegenCoilAirOutletNode;
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
						ErrorsFound = true;
					}

				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + " = " + Alphas( 1 ) );
				ShowContinueError( "Illegal " + cAlphaFields( 8 ) + " = " + DesicDehum( DesicDehumNum ).RegenCoilType );
				ErrorsFound = true;
			}

			DesicDehum( DesicDehumNum ).NomRotorPower = Numbers( 4 );
			DesicDehum( DesicDehumNum ).RegenFanType = Alphas( 10 );
			DesicDehum( DesicDehumNum ).RegenFanName = Alphas( 11 );

			TestCompSet( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, Alphas( 3 ), Alphas( 4 ), "Process Air Nodes" );

			// Set up component set for regen coil
			SetUpCompSets( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, Alphas( 8 ), Alphas( 9 ), "UNDEFINED", "UNDEFINED" );

			// Set up component set for regen fan
			SetUpCompSets( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, Alphas( 10 ), Alphas( 11 ), Alphas( 6 ), "UNDEFINED" );

			if ( ( ! SameString( Alphas( 12 ), "Default" ) ) && ( SameString( Alphas( 12 ), "UserCurves" ) ) ) {
				ShowWarningError( RoutineName + CurrentModuleObject + ": Invalid" + cAlphaFields( 12 ) + " = " + Alphas( 12 ) );
				ShowContinueError( "resetting to Default" );
				DesicDehum( DesicDehumNum ).PerformanceModel_Num = PM_Default;
			}

			if ( SameString( Alphas( 12 ), "UserCurves" ) ) {
				DesicDehum( DesicDehumNum ).PerformanceModel_Num = PM_UserCurves;
				DesicDehum( DesicDehumNum ).ProcDryBulbCurvefTW = GetCurveIndex( Alphas( 13 ) );
				if ( DesicDehum( DesicDehumNum ).ProcDryBulbCurvefTW == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 13 ) + " not found." );
					ErrorsFound2 = true;
				}
				DesicDehum( DesicDehumNum ).ProcDryBulbCurvefV = GetCurveIndex( Alphas( 14 ) );
				if ( DesicDehum( DesicDehumNum ).ProcDryBulbCurvefV == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 14 ) + " not found." );
					ErrorsFound2 = true;
				}
				DesicDehum( DesicDehumNum ).ProcHumRatCurvefTW = GetCurveIndex( Alphas( 15 ) );
				if ( DesicDehum( DesicDehumNum ).ProcHumRatCurvefTW == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 15 ) + " not found." );
					ErrorsFound2 = true;
				}
				DesicDehum( DesicDehumNum ).ProcHumRatCurvefV = GetCurveIndex( Alphas( 16 ) );
				if ( DesicDehum( DesicDehumNum ).ProcHumRatCurvefV == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 16 ) + " not found." );
					ErrorsFound2 = true;
				}
				DesicDehum( DesicDehumNum ).RegenEnergyCurvefTW = GetCurveIndex( Alphas( 17 ) );
				if ( DesicDehum( DesicDehumNum ).RegenEnergyCurvefTW == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 17 ) + " not found." );
					ErrorsFound2 = true;
				}
				DesicDehum( DesicDehumNum ).RegenEnergyCurvefV = GetCurveIndex( Alphas( 18 ) );
				if ( DesicDehum( DesicDehumNum ).RegenEnergyCurvefV == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 18 ) + " not found." );
					ErrorsFound2 = true;
				}
				DesicDehum( DesicDehumNum ).RegenVelCurvefTW = GetCurveIndex( Alphas( 19 ) );
				if ( DesicDehum( DesicDehumNum ).RegenVelCurvefTW == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 19 ) + " not found." );
					ErrorsFound2 = true;
				}
				DesicDehum( DesicDehumNum ).RegenVelCurvefV = GetCurveIndex( Alphas( 20 ) );
				if ( DesicDehum( DesicDehumNum ).RegenVelCurvefV == 0 ) {
					ShowSevereError( RoutineName + "Curve object=" + Alphas( 20 ) + " not found." );
					ErrorsFound2 = true;
				}
				if ( ErrorsFound2 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "Errors found in getting performance curves." );
					ErrorsFound = true;
				}
				DesicDehum( DesicDehumNum ).NomRegenTemp = Numbers( 5 );
				// Validate regen fan type, for user defined curves, can be constant or variable volume
				if ( ( SameString( DesicDehum( DesicDehumNum ).RegenFanType, "FAN:CONSTANTVOLUME" ) ) || ( SameString( DesicDehum( DesicDehumNum ).RegenFanType, "FAN:VARIABLEVOLUME" ) ) ) {
					ValidateComponent( DesicDehum( DesicDehumNum ).RegenFanType, DesicDehum( DesicDehumNum ).RegenFanName, ErrorsFound2, CurrentModuleObject + " = " + Alphas( 1 ) );
					if ( ErrorsFound2 ) ErrorsFound = true;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "Illegal " + cAlphaFields( 10 ) + " = " + DesicDehum( DesicDehumNum ).RegenFanType );
					ErrorsFound = true;
				}
			} else {
				// If DEFAULT performance model, set operating limits curves.  Unit is off outside this range
				DesicDehum( DesicDehumNum ).PerformanceModel_Num = PM_Default;
				for ( auto & e : DesicDehum ) {
					e.MinProcAirInTemp = 1.67; //  35 F
					e.MaxProcAirInTemp = 48.89; // 120 F
					e.MinProcAirInHumRat = 0.002857; //  20 gr/lb
					e.MaxProcAirInHumRat = 0.02857; // 200 gr/lb
				}
				//  If DEFAULT performance model, warn if curve names and nominal regen temp have values
				if ( ( ! lAlphaBlanks( 13 ) ) || ( ! lAlphaBlanks( 14 ) ) || ( ! lAlphaBlanks( 15 ) ) || ( ! lAlphaBlanks( 16 ) ) || ( ! lAlphaBlanks( 17 ) ) || ( ! lAlphaBlanks( 18 ) ) || ( ! lAlphaBlanks( 19 ) ) || ( ! lAlphaBlanks( 20 ) ) ) {
					ShowWarningError( CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "DEFAULT performance selected, curve names and nominal regen temp will be ignored." );
				}
				if ( DesicDehum( DesicDehumNum ).NomProcAirVel > 4.064 ) {
					ShowWarningError( CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( cNumericFields( 3 ) + " > 4.064 m/s.; Value in input=" + RoundSigDigits( DesicDehum( DesicDehumNum ).NomProcAirVel, 3 ) );
					ShowContinueError( "DEFAULT performance curves not valid outside 2.032 to 4.064 m/s (400 to 800 fpm)." );
				}
				if ( DesicDehum( DesicDehumNum ).NomProcAirVel < 2.032 ) {
					ShowWarningError( CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( cNumericFields( 3 ) + " < 2.032 m/s.; Value in input=" + RoundSigDigits( DesicDehum( DesicDehumNum ).NomProcAirVel, 3 ) );
					ShowContinueError( "DEFAULT performance curves not valid outside 2.032 to 4.064 m/s (400 to 800 fpm)." );
				}
				// Validate regen fan type, for default curves, can only variable volume
				if ( DesicDehum( DesicDehumNum ).RegenFanType == "FAN:VARIABLEVOLUME" ) {
					ValidateComponent( DesicDehum( DesicDehumNum ).RegenFanType, DesicDehum( DesicDehumNum ).RegenFanName, ErrorsFound2, CurrentModuleObject + " = " + Alphas( 1 ) );
					if ( ErrorsFound2 ) ErrorsFound = true;
				} else {
					ShowSevereError( CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "Illegal " + cAlphaFields( 10 ) + " = " + DesicDehum( DesicDehumNum ).RegenFanType );
					ShowContinueError( "For DEFAULT performance model, the regen fan type must be Fan:VariableVolume" );
					ErrorsFound = true;
				}
			}
		}

		for ( DesicDehumIndex = 1; DesicDehumIndex <= NumGenericDesicDehums; ++DesicDehumIndex ) {
			RegenCoilAirInletNode = 0;
			RegenCoilAirOutletNode = 0;

			CurrentModuleObject = "Dehumidifier:Desiccant:System";

			DesicDehumNum = DesicDehumIndex + NumSolidDesicDehums;
			DesicDehum( DesicDehumNum ).DehumType = CurrentModuleObject;
			DesicDehum( DesicDehumNum ).DehumTypeCode = Generic;
			GetObjectItem( DesicDehum( DesicDehumNum ).DehumType, DesicDehumIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DesicDehum, DesicDehumNum - 1, IsNotOK, IsBlank, DesicDehum( DesicDehumNum ).DehumType + " Name" );

			if ( IsNotOK ) {
				ErrorsFoundGeneric = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			DesicDehum( DesicDehumNum ).Name = Alphas( 1 );

			ErrorsFound2 = false;
			ValidateComponent( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, ErrorsFound2, DesicDehum( DesicDehumNum ).DehumType + " = \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
			if ( ErrorsFound2 ) {
				ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\" is not unique" );
				ErrorsFoundGeneric = true;
			}

			DesicDehum( DesicDehumNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DesicDehum( DesicDehumNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DesicDehum( DesicDehumNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DesicDehum( DesicDehumNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			DesicDehum( DesicDehumNum ).HXType = Alphas( 3 );
			DesicDehum( DesicDehumNum ).HXName = Alphas( 4 );

			if ( ! SameString( DesicDehum( DesicDehumNum ).HXType, "HeatExchanger:Desiccant:BalancedFlow" ) ) {
				ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + " = \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ShowContinueError( "Invalid " + cAlphaFields( 3 ) + " = " + DesicDehum( DesicDehumNum ).HXType );
				ErrorsFoundGeneric = true;
			} else {
				DesicDehum( DesicDehumNum ).HXTypeNum = BalancedHX;
			}

			ErrorsFound2 = false;
			ValidateComponent( DesicDehum( DesicDehumNum ).HXType, DesicDehum( DesicDehumNum ).HXName, ErrorsFound2, DesicDehum( DesicDehumNum ).DehumType + " = \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
			if ( ErrorsFound2 ) ErrorsFoundGeneric = true;

			ErrorsFound2 = false;
			DesicDehum( DesicDehumNum ).HXProcInNode = GetSecondaryInletNode( DesicDehum( DesicDehumNum ).HXName, ErrorsFound2 );
			if ( ErrorsFound2 ) {
				ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ErrorsFoundGeneric = true;
			}

			ProcAirInlet = NodeID( DesicDehum( DesicDehumNum ).HXProcInNode );

			DesicDehum( DesicDehumNum ).ProcAirInNode = GetOnlySingleNode( ProcAirInlet, ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			ErrorsFound2 = false;
			DesicDehum( DesicDehumNum ).HXProcOutNode = GetSecondaryOutletNode( DesicDehum( DesicDehumNum ).HXName, ErrorsFound2 );
			if ( ErrorsFound2 ) {
				ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ErrorsFoundGeneric = true;
			}

			ProcAirOutlet = NodeID( DesicDehum( DesicDehumNum ).HXProcOutNode );

			DesicDehum( DesicDehumNum ).ProcAirOutNode = GetOnlySingleNode( ProcAirOutlet, ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			TestCompSet( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, ProcAirInlet, ProcAirOutlet, "Process Air Nodes" );

			ErrorsFound2 = false;
			DesicDehum( DesicDehumNum ).HXRegenInNode = GetSupplyInletNode( DesicDehum( DesicDehumNum ).HXName, ErrorsFound2 );
			if ( ErrorsFound2 ) {
				ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ErrorsFoundGeneric = true;
			}

			ErrorsFound2 = false;
			DesicDehum( DesicDehumNum ).HXRegenOutNode = GetSupplyOutletNode( DesicDehum( DesicDehumNum ).HXName, ErrorsFound2 );
			if ( ErrorsFound2 ) {
				ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ErrorsFoundGeneric = true;
			}

			DesicDehum( DesicDehumNum ).ControlNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			if ( DesicDehum( DesicDehumNum ).ControlNodeNum == 0 ) {
				ShowContinueError( DesicDehum( DesicDehumNum ).DehumType + " = \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ShowSevereError( cAlphaFields( 5 ) + " must be specified." );
				ErrorsFoundGeneric = true;
			}

			DesicDehum( DesicDehumNum ).RegenFanType = Alphas( 6 );
			DesicDehum( DesicDehumNum ).RegenFanName = Alphas( 7 );

			if ( SameString( DesicDehum( DesicDehumNum ).RegenFanType, "Fan:OnOff" ) || SameString( DesicDehum( DesicDehumNum ).RegenFanType, "Fan:ConstantVolume" ) ) {
				ErrorsFound2 = false;
				ValidateComponent( DesicDehum( DesicDehumNum ).RegenFanType, DesicDehum( DesicDehumNum ).RegenFanName, ErrorsFound2, DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				if ( ErrorsFound2 ) ErrorsFoundGeneric = true;
			} else {
				ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ShowContinueError( "Illegal " + cAlphaFields( 6 ) + " = " + DesicDehum( DesicDehumNum ).RegenFanType );
				ErrorsFoundGeneric = true;
			}

			if ( SameString( Alphas( 8 ), "DrawThrough" ) ) {
				DesicDehum( DesicDehumNum ).RegenFanPlacement = DrawThru;
			} else if ( SameString( Alphas( 8 ), "BlowThrough" ) ) {
				DesicDehum( DesicDehumNum ).RegenFanPlacement = BlowThru;
			} else {
				ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ShowContinueError( "Illegal " + cAlphaFields( 8 ) + " = " + Alphas( 8 ) );
				ShowContinueError( "...resetting to DEFAULT of DRAW THROUGH" );
				DesicDehum( DesicDehumNum ).RegenFanPlacement = DrawThru;
			}

			ErrorsFound2 = false;
			DesicDehum( DesicDehumNum ).RegenFanInNode = GetFanInletNode( DesicDehum( DesicDehumNum ).RegenFanType, DesicDehum( DesicDehumNum ).RegenFanName, ErrorsFound2 );
			if ( ErrorsFound2 ) {
				ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ErrorsFoundGeneric = true;
			}

			ErrorsFound2 = false;
			DesicDehum( DesicDehumNum ).RegenFanOutNode = GetFanOutletNode( DesicDehum( DesicDehumNum ).RegenFanType, DesicDehum( DesicDehumNum ).RegenFanName, ErrorsFound2 );
			GetFanIndex( DesicDehum( DesicDehumNum ).RegenFanName, DesicDehum( DesicDehumNum ).RegenFanIndex, ErrorsFound2, DesicDehum( DesicDehumNum ).RegenFanType );
			if ( ErrorsFound2 ) {
				ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ErrorsFoundGeneric = true;
			}

			DesicDehum( DesicDehumNum ).RegenCoilType = Alphas( 9 );
			DesicDehum( DesicDehumNum ).RegenCoilName = Alphas( 10 );
			RegenCoilType = Alphas( 9 );
			RegenCoilName = Alphas( 10 );
			DesicDehum( DesicDehumNum ).RegenSetPointTemp = Numbers( 1 );

			if ( ! lAlphaBlanks( 10 ) ) {
				if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Electric" ) || SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Gas" ) ) {
					if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Electric" ) ) DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingElectric;
					if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Gas" ) ) DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingGas;
					ErrorsFound2 = false;
					ValidateComponent( RegenCoilType, RegenCoilName, ErrorsFound2, DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
					if ( ErrorsFound2 ) ErrorsFoundGeneric = true;

					if ( DesicDehum( DesicDehumNum ).RegenSetPointTemp <= 0.0 ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( cNumericFields( 1 ) + " must be greater than 0." );
						ErrorsFoundGeneric = true;
					}

					ErrorsFound2 = false;
					DesicDehum( DesicDehumNum ).RegenCoilInletNode = GetHeatingCoilInletNode( RegenCoilType, RegenCoilName, ErrorsFound2 );
					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

					ErrorsFound2 = false;
					DesicDehum( DesicDehumNum ).RegenCoilOutletNode = GetHeatingCoilOutletNode( RegenCoilType, RegenCoilName, ErrorsFound2 );
					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

					ErrorsFound2 = false;
					GetHeatingCoilIndex( RegenCoilName, DesicDehum( DesicDehumNum ).RegenCoilIndex, ErrorsFound2 );
					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

					ErrorsFound2 = false;
					RegenCoilControlNodeNum = GetHeatingCoilControlNodeNum( RegenCoilType, RegenCoilName, ErrorsFound2 );
					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

					if ( RegenCoilControlNodeNum > 0 ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( cNumericFields( 1 ) + " is specified as " + RoundSigDigits( DesicDehum( DesicDehumNum ).RegenSetPointTemp, 3 ) + " C in this object." );
						ShowContinueError( " Do not specify a coil temperature setpoint node name in the regeneration air heater object." );
						ShowContinueError( "..." + cAlphaFields( 9 ) + " = " + DesicDehum( DesicDehumNum ).RegenCoilType );
						ShowContinueError( "..." + cAlphaFields( 10 ) + " = " + DesicDehum( DesicDehumNum ).RegenCoilName );
						ShowContinueError( "...heating coil temperature setpoint node = " + NodeID( RegenCoilControlNodeNum ) );
						ShowContinueError( "...leave the heating coil temperature setpoint node name blank in the regen heater object." );
						ErrorsFoundGeneric = true;
					}

					RegairHeatingCoilFlag = true;
					SetHeatingCoilData( DesicDehum( DesicDehumNum ).RegenCoilIndex, ErrorsFound2, RegairHeatingCoilFlag, DesicDehumNum );
					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

				} else if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Water" ) ) {
					DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingWater;
					ValidateComponent( RegenCoilType, RegenCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
						ErrorsFound = true;
					} else { // mine data from heating coil object
						errFlag = false;
						DesicDehum( DesicDehumNum ).RegenCoilIndex = GetWaterCoilIndex( "COIL:HEATING:WATER", RegenCoilName, errFlag );
						if ( DesicDehum( DesicDehumNum ).RegenCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 9 ) + " = " + RegenCoilName );
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						if ( DesicDehum( DesicDehumNum ).RegenSetPointTemp <= 0.0 ) {
							ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
							ShowContinueError( cNumericFields( 1 ) + " must be greater than 0." );
							ErrorsFoundGeneric = true;
						}

						// Get the Heating Coil Hot water Inlet or control Node number
						errFlag = false;
						DesicDehum( DesicDehumNum ).CoilControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", RegenCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						// Get the Regeneration Heating Coil hot water max volume flow rate
						errFlag = false;
						DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", RegenCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						// Get the Regeneration Heating Coil Inlet Node
						errFlag = false;
						RegenCoilAirInletNode = GetWaterCoilInletNode( "Coil:Heating:Water", RegenCoilName, errFlag );
						DesicDehum( DesicDehumNum ).RegenCoilInletNode = RegenCoilAirInletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						// Get the Regeneration Heating Coil Outlet Node
						errFlag = false;
						RegenCoilAirOutletNode = GetWaterCoilOutletNode( "Coil:Heating:Water", RegenCoilName, errFlag );
						DesicDehum( DesicDehumNum ).RegenCoilOutletNode = RegenCoilAirOutletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						RegairHeatingCoilFlag = true;
						SetWaterCoilData( DesicDehum( DesicDehumNum ).RegenCoilIndex, ErrorsFound2, RegairHeatingCoilFlag, DesicDehumNum );
						if ( ErrorsFound2 ) {
							ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
							ErrorsFoundGeneric = true;
						}

					}
				} else if ( SameString( DesicDehum( DesicDehumNum ).RegenCoilType, "Coil:Heating:Steam" ) ) {
					DesicDehum( DesicDehumNum ).RegenCoilType_Num = Coil_HeatingSteam;
					ValidateComponent( RegenCoilType, RegenCoilName, IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
						ErrorsFound = true;
					} else { // mine data from the regeneration heating coil object
						if ( DesicDehum( DesicDehumNum ).RegenSetPointTemp <= 0.0 ) {
							ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
							ShowContinueError( cNumericFields( 1 ) + " must be greater than 0." );
							ErrorsFoundGeneric = true;
						}

						errFlag = false;
						DesicDehum( DesicDehumNum ).RegenCoilIndex = GetSteamCoilIndex( "COIL:HEATING:STEAM", RegenCoilName, errFlag );
						if ( DesicDehum( DesicDehumNum ).RegenCoilIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 9 ) + " = " + RegenCoilName );
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						// Get the regeneration Heating Coil steam inlet node number
						errFlag = false;
						DesicDehum( DesicDehumNum ).CoilControlNode = GetCoilSteamInletNode( "Coil:Heating:Steam", RegenCoilName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						// Get the regeneration heating Coil steam max volume flow rate
						DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate( DesicDehum( DesicDehumNum ).RegenCoilIndex, errFlag );
						if ( DesicDehum( DesicDehumNum ).MaxCoilFluidFlow > 0.0 ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, dehumidifierDesiccantNoFans );
							DesicDehum( DesicDehumNum ).MaxCoilFluidFlow *= SteamDensity;
						}

						// Get the regeneration heating Coil Inlet Node
						errFlag = false;
						RegenCoilAirInletNode = GetSteamCoilAirInletNode( DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilName, errFlag );
						DesicDehum( DesicDehumNum ).RegenCoilInletNode = RegenCoilAirInletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}

						// Get the regeneration heating Coil Outlet Node
						errFlag = false;
						RegenCoilAirOutletNode = GetCoilAirOutletNode( DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilName, errFlag );
						DesicDehum( DesicDehumNum ).RegenCoilOutletNode = RegenCoilAirOutletNode;
						if ( errFlag ) {
							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + DesicDehum( DesicDehumNum ).Name );
							ErrorsFound = true;
						}
					}

					ErrorsFound2 = false;
					RegenCoilControlNodeNum = GetSteamCoilControlNodeNum( RegenCoilType, RegenCoilName, ErrorsFound2 );

					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

					if ( RegenCoilControlNodeNum > 0 ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( cNumericFields( 1 ) + " is specified as " + RoundSigDigits( DesicDehum( DesicDehumNum ).RegenSetPointTemp, 3 ) + " C in this object." );
						ShowContinueError( " Do not specify a coil temperature setpoint node name in the regeneration air heater object." );
						ShowContinueError( "..." + cAlphaFields( 9 ) + " = " + DesicDehum( DesicDehumNum ).RegenCoilType );
						ShowContinueError( "..." + cAlphaFields( 10 ) + " = " + DesicDehum( DesicDehumNum ).RegenCoilName );
						ShowContinueError( "...heating coil temperature setpoint node = " + NodeID( RegenCoilControlNodeNum ) );
						ShowContinueError( "...leave the heating coil temperature setpoint node name blank in the regen heater object." );
						ErrorsFoundGeneric = true;
					}

					RegairHeatingCoilFlag = true;
					SetSteamCoilData( DesicDehum( DesicDehumNum ).RegenCoilIndex, ErrorsFound2, RegairHeatingCoilFlag, DesicDehumNum );
					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

				} else {
					ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
					ShowContinueError( "Illegal " + cAlphaFields( 9 ) + " = " + DesicDehum( DesicDehumNum ).RegenCoilType );
					ErrorsFoundGeneric = true;
				}

			}

			RegenAirInlet = NodeID( DesicDehum( DesicDehumNum ).HXRegenInNode );

			RegenAirOutlet = NodeID( DesicDehum( DesicDehumNum ).HXRegenOutNode );

			RegenFanInlet = NodeID( DesicDehum( DesicDehumNum ).RegenFanInNode );

			RegenFanOutlet = NodeID( DesicDehum( DesicDehumNum ).RegenFanOutNode );

			if ( ! lAlphaBlanks( 10 ) ) {
				RegenCoilInlet = NodeID( DesicDehum( DesicDehumNum ).RegenCoilInletNode );

				RegenCoilOutlet = NodeID( DesicDehum( DesicDehumNum ).RegenCoilOutletNode );
			}

			SetUpCompSets( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, DesicDehum( DesicDehumNum ).HXType, DesicDehum( DesicDehumNum ).HXName, ProcAirInlet, ProcAirOutlet );

			SetUpCompSets( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, DesicDehum( DesicDehumNum ).RegenFanType, DesicDehum( DesicDehumNum ).RegenFanName, RegenFanInlet, RegenFanOutlet );

			if ( ! lAlphaBlanks( 10 ) ) {
				SetUpCompSets( DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, DesicDehum( DesicDehumNum ).RegenCoilType, DesicDehum( DesicDehumNum ).RegenCoilName, RegenCoilInlet, RegenCoilOutlet );
			}

			if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
				DesicDehum( DesicDehumNum ).RegenAirInNode = GetOnlySingleNode( RegenFanInlet, ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
				DesicDehum( DesicDehumNum ).RegenAirOutNode = GetOnlySingleNode( RegenAirOutlet, ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );
				if ( ! lAlphaBlanks( 10 ) ) {
					if ( DesicDehum( DesicDehumNum ).RegenFanOutNode != DesicDehum( DesicDehumNum ).RegenCoilInletNode ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( "Regen fan outlet node name and regen heater inlet node name do not match for fan placement: Blow Through" );
						ShowContinueError( "...Regen fan outlet node   = " + NodeID( DesicDehum( DesicDehumNum ).RegenFanOutNode ) );
						ShowContinueError( "...Regen heater inlet node = " + NodeID( DesicDehum( DesicDehumNum ).RegenCoilInletNode ) );
						ErrorsFoundGeneric = true;
					}
					if ( DesicDehum( DesicDehumNum ).RegenCoilOutletNode != DesicDehum( DesicDehumNum ).HXRegenInNode ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( "Regen heater outlet node name and desiccant heat exchanger regen inlet node name do not match for fan placement: Blow Through" );
						ShowContinueError( "...Regen heater outlet node = " + NodeID( DesicDehum( DesicDehumNum ).RegenCoilOutletNode ) );
						ShowContinueError( "...HX regen inlet node      = " + NodeID( DesicDehum( DesicDehumNum ).HXRegenInNode ) );
						ErrorsFoundGeneric = true;
					}
				} else {
					if ( DesicDehum( DesicDehumNum ).RegenFanOutNode != DesicDehum( DesicDehumNum ).HXRegenInNode ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( "Regen fan outlet node name and desiccant heat exchanger inlet node name do not match for fan placement: Blow Through" );
						ShowContinueError( "...Regen fan outlet node   = " + NodeID( DesicDehum( DesicDehumNum ).RegenFanOutNode ) );
						ShowContinueError( "...Desiccant HX inlet node = " + NodeID( DesicDehum( DesicDehumNum ).HXRegenInNode ) );
						ErrorsFoundGeneric = true;
					}
				}
			} else { // ELSE for IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
				DesicDehum( DesicDehumNum ).RegenAirOutNode = GetOnlySingleNode( RegenFanOutlet, ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );
				if ( ! lAlphaBlanks( 10 ) ) {
					DesicDehum( DesicDehumNum ).RegenAirInNode = GetOnlySingleNode( RegenCoilInlet, ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
					if ( DesicDehum( DesicDehumNum ).RegenCoilOutletNode != DesicDehum( DesicDehumNum ).HXRegenInNode ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( "Regen heater outlet node name and desiccant heat exchanger regen inlet node name do not match for fan placement: Draw Through" );
						ShowContinueError( "...Regen heater outlet node = " + NodeID( DesicDehum( DesicDehumNum ).RegenCoilOutletNode ) );
						ShowContinueError( "...HX regen inlet node      = " + NodeID( DesicDehum( DesicDehumNum ).HXRegenInNode ) );
						ErrorsFoundGeneric = true;
					}
				} else {
					DesicDehum( DesicDehumNum ).RegenAirInNode = GetOnlySingleNode( RegenAirInlet, ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
				}
				if ( DesicDehum( DesicDehumNum ).RegenFanInNode != DesicDehum( DesicDehumNum ).HXRegenOutNode ) {
					ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
					ShowContinueError( "Regen fan inlet node name and desiccant heat exchanger regen outlet node name do not match for fan placement: Draw Through" );
					ShowContinueError( "...Regen fan inlet node = " + NodeID( DesicDehum( DesicDehumNum ).RegenFanInNode ) );
					ShowContinueError( "...HX regen outlet node = " + NodeID( DesicDehum( DesicDehumNum ).HXRegenOutNode ) );
					ErrorsFoundGeneric = true;
				}
			}

			DesicDehum( DesicDehumNum ).CoolingCoilType = Alphas( 11 );
			DesicDehum( DesicDehumNum ).CoolingCoilName = Alphas( 12 );

			if ( ! lAlphaBlanks( 12 ) ) {
				if ( ( SameString( DesicDehum( DesicDehumNum ).CoolingCoilType, "COIL:COOLING:DX:SINGLESPEED" ) ) || ( SameString( DesicDehum( DesicDehumNum ).CoolingCoilType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ) ) ) {
					ErrorsFound2 = false;
					ValidateComponent( DesicDehum( DesicDehumNum ).CoolingCoilType, DesicDehum( DesicDehumNum ).CoolingCoilName, ErrorsFound2, DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
					if ( ErrorsFound2 ) ErrorsFoundGeneric = true;
				} else { //ELSE for IF (DesicDehum(DesicDehumNum)%CoolingCoilType == 'COIL:COOLING:DX:SINGLESPEED' or MultiMode)THEN
					ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + '=' + DesicDehum( DesicDehumNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( 11 ) + " = " + DesicDehum( DesicDehumNum ).CoolingCoilType );
					ErrorsFoundGeneric = true;
				}

				ErrorsFound2 = false;
				DesicDehum( DesicDehumNum ).CoolingCoilOutletNode = GetDXCoilOutletNode( DesicDehum( DesicDehumNum ).CoolingCoilType, DesicDehum( DesicDehumNum ).CoolingCoilName, ErrorsFound2 );
				DesicDehum( DesicDehumNum ).CompanionCoilCapacity = GetDXCoilCapacity( DesicDehum( DesicDehumNum ).CoolingCoilType, DesicDehum( DesicDehumNum ).CoolingCoilName, ErrorsFound2 );
				if ( ErrorsFound2 ) ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).CoolingCoilName + "\"" );

				ErrorsFound2 = false;
				GetDXCoilIndex( DesicDehum( DesicDehumNum ).CoolingCoilName, DesicDehum( DesicDehumNum ).DXCoilIndex, ErrorsFound2, DesicDehum( DesicDehumNum ).CoolingCoilType );
				if ( ErrorsFound2 ) ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).CoolingCoilName + "\"" );

			} //  (DesicDehum(DesicDehumNum)%CoolingCoilName /= Blank)THEN

			if ( SameString( Alphas( 13 ), "Yes" ) ) {
				DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide = Yes;
			} else if ( lAlphaBlanks( 13 ) ) {
				DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide = No;
			} else if ( SameString( Alphas( 13 ), "No" ) ) {
				DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide = No;
			} else {
				ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ShowContinueError( "Invalid choice for " + cAlphaFields( 13 ) + " = " + Alphas( 13 ) );
				ShowContinueError( "...resetting to the default value of No" );
				DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide = No;
			}

			if ( SameString( Alphas( 14 ), "Yes" ) ) {
				DesicDehum( DesicDehumNum ).Preheat = Yes;
			} else if ( SameString( Alphas( 14 ), "No" ) ) {
				DesicDehum( DesicDehumNum ).Preheat = No;
			} else if ( lAlphaBlanks( 14 ) ) {
				DesicDehum( DesicDehumNum ).Preheat = No;
			} else {
				ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ShowContinueError( "Invalid choice for " + cAlphaFields( 14 ) + " = " + Alphas( 14 ) );
				ShowContinueError( "...resetting to the default value of NO" );
				DesicDehum( DesicDehumNum ).Preheat = No;
			}

			if ( DesicDehum( DesicDehumNum ).DXCoilIndex > 0 ) {

				if ( DesicDehum( DesicDehumNum ).Preheat == Yes ) { // Companion coil waste heat used for regeneration of desiccant
					ErrorsFound2 = false;
					DesuperHeaterIndex = GetHeatReclaimSourceIndexNum( DesicDehum( DesicDehumNum ).CoolingCoilType, DesicDehum( DesicDehumNum ).CoolingCoilName, ErrorsFound2 );
					if ( ErrorsFound2 ) {
						ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ErrorsFoundGeneric = true;
					}

					if ( DesuperHeaterIndex > 0 ) {
						ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + '=' + DesicDehum( DesicDehumNum ).Name );
						ShowContinueError( "A Coil:Heating:Desuperheater object should not be used when condenser waste heat is reclaimed for desiccant regeneration." );
						ShowContinueError( "A Coil:Heating:Desuperheater object was found using waste heat from the " + DesicDehum( DesicDehumNum ).CoolingCoilType + " \"" + DesicDehum( DesicDehumNum ).CoolingCoilName + "\" object." );
						//          ErrorsFoundGeneric = .TRUE.
					}
				}

				ErrorsFound2 = false;
				DesicDehum( DesicDehumNum ).CondenserInletNode = GetCoilCondenserInletNode( DesicDehum( DesicDehumNum ).CoolingCoilType, DesicDehum( DesicDehumNum ).CoolingCoilName, ErrorsFound2 );
				if ( DesicDehum( DesicDehumNum ).CondenserInletNode == 0 && DesicDehum( DesicDehumNum ).Preheat == Yes ) {
					DesicDehum( DesicDehumNum ).CondenserInletNode = GetOnlySingleNode( DesicDehum( DesicDehumNum ).CoolingCoilName + " Condenser Inlet Node", ErrorsFound, DesicDehum( DesicDehumNum ).DehumType, DesicDehum( DesicDehumNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
					CheckAndAddAirNodeNumber( DesicDehum( DesicDehumNum ).CondenserInletNode, OANodeError );
					if ( ! OANodeError ) {
						ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( "The " + cAlphaFields( 14 ) + " input is specified as Yes and a condenser air inlet node name was not specified for the companion cooling coil." );
						ShowContinueError( "Adding condenser inlet air node for " + DesicDehum( DesicDehumNum ).CoolingCoilType + " \"" + DesicDehum( DesicDehumNum ).CoolingCoilName + "\"" );
						ShowContinueError( "...condenser inlet air node name = " + NodeID( DesicDehum( DesicDehumNum ).CondenserInletNode ) );
						ShowContinueError( "...this node name will be specified as an outdoor air node." );
					}
				} else if ( DesicDehum( DesicDehumNum ).Preheat == Yes ) {
					if ( ! CheckOutAirNodeNumber( DesicDehum( DesicDehumNum ).CondenserInletNode ) ) {
						ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
						ShowContinueError( "The regeneration air inlet node must be specified as an outdoor air node when " + cAlphaFields( 14 ) + " is specified as Yes." );
						ErrorsFoundGeneric = true;
					}
				}
			}

			if ( CheckOutAirNodeNumber( DesicDehum( DesicDehumNum ).RegenAirInNode ) ) {
				DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode = true;
			}

			if ( DesicDehum( DesicDehumNum ).DXCoilIndex == 0 && DesicDehum( DesicDehumNum ).Preheat == Yes ) {
				ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + '=' + DesicDehum( DesicDehumNum ).Name );
				ShowContinueError( "A valid " + cAlphaFields( 12 ) + " must be used when condenser waste heat is reclaimed for desiccant regeneration." );
				ShowContinueError( "... " + cAlphaFields( 11 ) + " = " + DesicDehum( DesicDehumNum ).CoolingCoilType );
				ShowContinueError( "... " + cAlphaFields( 12 ) + " = " + DesicDehum( DesicDehumNum ).CoolingCoilName );
				ErrorsFoundGeneric = true;
			}

			if ( DesicDehum( DesicDehumNum ).DXCoilIndex > 0 && DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide == Yes ) {
				ErrorsFound2 = false;
				CoilBypassedFlowFrac = GetDXCoilBypassedFlowFrac( DesicDehum( DesicDehumNum ).CoolingCoilType, DesicDehum( DesicDehumNum ).CoolingCoilName, ErrorsFound2 );
				if ( ErrorsFound2 ) ShowContinueError( "...occurs in " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).CoolingCoilName + "\"" );
				if ( CoilBypassedFlowFrac > 0.0 ) {
					ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + '=' + DesicDehum( DesicDehumNum ).Name );
					ShowContinueError( "A DX coil bypassed air flow fraction greater than 0 may not be used when the input for " + cAlphaFields( 13 ) + " is specified as Yes." );
					ShowContinueError( "A DX coil with a bypassed air flow fraction greater than 0 may be upstream of the process inlet however the input for " + cAlphaFields( 13 ) + " must be specified as No." );
					ShowContinueError( "... " + cAlphaFields( 11 ) + " = " + DesicDehum( DesicDehumNum ).CoolingCoilType );
					ShowContinueError( "... " + cAlphaFields( 12 ) + " = " + DesicDehum( DesicDehumNum ).CoolingCoilName );
					ErrorsFoundGeneric = true;
				}
			} else if ( DesicDehum( DesicDehumNum ).DXCoilIndex == 0 && DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide == Yes ) {
				ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
				ShowContinueError( "A valid companion coil must be specified when " + cAlphaFields( 13 ) + " is specified as Yes." );
				ErrorsFoundGeneric = true;
			}

			if ( ! DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode && DesicDehum( DesicDehumNum ).Preheat == Yes ) {
				ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + '=' + DesicDehum( DesicDehumNum ).Name );
				ShowContinueError( "The desiccant dehumidifier regeneration air inlet must be specified as an outdoor air node when " + cAlphaFields( 14 ) + " is specified as Yes." );
				ShowContinueError( "... desiccant dehumidifier regeneration air inlet node name = " + NodeID( DesicDehum( DesicDehumNum ).RegenAirInNode ) );
				ErrorsFoundGeneric = true;
			}

			if ( DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide == Yes ) {
				if ( DesicDehum( DesicDehumNum ).ProcAirInNode != DesicDehum( DesicDehumNum ).CoolingCoilOutletNode ) {
					ShowSevereError( "For " + DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
					ShowContinueError( "Node names are inconsistent in companion cooling coil and desiccant heat exchanger objects." );
					ShowContinueError( "For companion cooling coil = " + DesicDehum( DesicDehumNum ).CoolingCoilType + " \"" + DesicDehum( DesicDehumNum ).CoolingCoilName + "\"" );
					ShowContinueError( "The outlet node name in cooling coil = " + NodeID( DesicDehum( DesicDehumNum ).CoolingCoilOutletNode ) );
					ShowContinueError( "For desiccant heat exchanger = " + DesicDehum( DesicDehumNum ).HXType + " \"" + DesicDehum( DesicDehumNum ).HXName + "\"" );
					ShowContinueError( "The process air inlet node name = " + NodeID( DesicDehum( DesicDehumNum ).ProcAirInNode ) );
					ShowFatalError( "...previous error causes program termination." );
				}
			}

			//Exhaust Fan input
			DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate = Numbers( 2 );
			DesicDehum( DesicDehumNum ).ExhaustFanMaxPower = Numbers( 3 );
			DesicDehum( DesicDehumNum ).ExhaustFanCurveIndex = GetCurveIndex( Alphas( 15 ) );

			if ( ! SameString( GetCurveType( DesicDehum( DesicDehumNum ).ExhaustFanCurveIndex ), "" ) ) {
				{ auto const SELECT_CASE_var( GetCurveType( DesicDehum( DesicDehumNum ).ExhaustFanCurveIndex ) );
				if ( SELECT_CASE_var == "CUBIC" ) {

				} else if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else {
					ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + ", \"" + DesicDehum( DesicDehumNum ).Name + "\" illegal Part Load Fraction Correlation Curve (function of part-load ratio) type for this object = " + GetCurveType( DesicDehum( DesicDehumNum ).ExhaustFanCurveIndex ) );
					ErrorsFoundGeneric = true;
				}}
			}

			if ( DesicDehum( DesicDehumNum ).Preheat == Yes ) {
				ErrorsFound2 = false;
				if ( DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate <= 0 ) {
					ErrorsFound2 = true;
				}
				if ( DesicDehum( DesicDehumNum ).ExhaustFanMaxPower <= 0 ) {
					ErrorsFound2 = true;
				}
				if ( ErrorsFound2 ) {
					ShowSevereError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
					ShowContinueError( cNumericFields( 2 ) + " and " + cNumericFields( 3 ) + " must be defined if " + cAlphaFields( 14 ) + " field is \"Yes\"." );
				}
			} else if ( DesicDehum( DesicDehumNum ).Preheat == No ) {
				if ( DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate > 0.0 ) {
					ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\"" );
					ShowContinueError( cNumericFields( 2 ) + " should be 0 if " + cAlphaFields( 14 ) + " field is \"No\"." );
					ShowContinueError( "..." + cNumericFields( 2 ) + " will not be used and is reset to 0." );
					DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate = 0.0;
				}
			}

		}

		// SET UP OUTPUTS
		for ( DesicDehumNum = 1; DesicDehumNum <= NumSolidDesicDehums; ++DesicDehumNum ) {
			// Setup Report variables for the Desiccant Dehumidifiers
			SetupOutputVariable( "Dehumidifier Removed Water Mass [kg]", DesicDehum( DesicDehumNum ).WaterRemove, "System", "Sum", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Removed Water Mass Flow Rate [kg/s]", DesicDehum( DesicDehumNum ).WaterRemoveRate, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Part Load Ratio []", DesicDehum( DesicDehumNum ).PartLoad, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Electric Power [W]", DesicDehum( DesicDehumNum ).ElecUseRate, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Electric Energy [J]", DesicDehum( DesicDehumNum ).ElecUseEnergy, "System", "Sum", DesicDehum( DesicDehumNum ).Name, _, "Electricity", "Cooling", _, "System" );
			SetupOutputVariable( "Dehumidifier Regeneration Specific Energy [J/kgWater]", DesicDehum( DesicDehumNum ).SpecRegenEnergy, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Regeneration Rate [W]", DesicDehum( DesicDehumNum ).QRegen, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Regeneration Energy [J]", DesicDehum( DesicDehumNum ).RegenEnergy, "System", "Sum", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Regeneration Air Speed [m/s]", DesicDehum( DesicDehumNum ).RegenAirVel, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Regeneration Air Mass Flow Rate [kg/s]", DesicDehum( DesicDehumNum ).RegenAirInMassFlowRate, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Process Air Mass Flow Rate [kg/s]", DesicDehum( DesicDehumNum ).ProcAirInMassFlowRate, "System", "Average", DesicDehum( DesicDehumNum ).Name );
		}

		for ( DesicDehumNum = 1; DesicDehumNum <= NumGenericDesicDehums; ++DesicDehumNum ) {
			// Setup Report variables for the Desiccant Dehumidifiers
			SetupOutputVariable( "Dehumidifier Removed Water Mass [kg]", DesicDehum( DesicDehumNum ).WaterRemove, "System", "Sum", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Removed Water Mass Flow Rate [kg/s]", DesicDehum( DesicDehumNum ).WaterRemoveRate, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			SetupOutputVariable( "Dehumidifier Part Load Ratio []", DesicDehum( DesicDehumNum ).PartLoad, "System", "Average", DesicDehum( DesicDehumNum ).Name );
			if ( DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate > 0 ) {
				SetupOutputVariable( "Dehumidifier Exhaust Fan Electric Power [W]", DesicDehum( DesicDehumNum ).ExhaustFanPower, "System", "Average", DesicDehum( DesicDehumNum ).Name );
				SetupOutputVariable( "Dehumidifier Exhaust Fan Electric Energy [J]", DesicDehum( DesicDehumNum ).ExhaustFanElecConsumption, "System", "Sum", DesicDehum( DesicDehumNum ).Name, _, "Electricity", "Cooling", _, "System" );
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in getting Dehumidifier:Desiccant:NoFans input" );
		} else if ( ErrorsFoundGeneric ) {
			ShowFatalError( "Errors found in getting DESICCANT DEHUMIDIFIER input" );
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	void
	InitDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
		//                      for Gas Research Institute
		//       DATE WRITTEN   March 2001
		//       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add setpoint validation for new control type option:
		//                          NODE LEAVING HUMRAT SETPOINT:BYPASS
		//                        Work supported by ASHRAE research project 1254-RP
		//                      June 2007 R. Raustad, FSEC
		//                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
		//                      May 2009, B. Griffith, NREL. added EMS node setpoint checks
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the dehumidifier Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::DoSetPointTest;
		using DataHVACGlobals::SetPointErrorFlag;
		//unused  USE DataEnvironment, ONLY: StdBaroPress
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iHumidityRatioMaxSetPoint;
		using SteamCoils::SimulateSteamCoilComponents;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		using WaterCoils::GetCoilMaxWaterFlowRate;
		using WaterCoils::SimulateWaterCoilComponents;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::PlantLoop;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatDensityRefrig;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using DataGlobals::InitConvTemp;
		using DataGlobals::AnyPlantInModel;
		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitDesiccantDehumidifier" );
		static std::string const initCBVAV( "InitCBVAV" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ProcInNode; // inlet node number
		int RegenInNode; // inlet node number
		int ControlNode; // control node number
		static bool MySetPointCheckFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag; // Used for init plant component for heating coils

		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int SteamIndex; // steam coil index
		Real64 FluidDensity; // steam or water coil fluid density
		Real64 CoilMaxVolFlowRate; // water or steam max volumetric water flow rate
		Real64 QCoilActual; // actual CBVAV steam heating coil load met (W)
		bool ErrorFlag; // local error flag returned from data mining
		//unused  REAL(r64)                      :: mdot                 ! heating coil fluid mass flow rate, kg/s
		//unused  REAL(r64)                      :: QDelivered           ! regen heat actually delivered by regen coil [W]

		if ( InitDesiccantDehumidifierOneTimeFlag ) {

			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumDesicDehums );
			MyPlantScanFlag.allocate( NumDesicDehums );
			MyEnvrnFlag = true;

			InitDesiccantDehumidifierOneTimeFlag = false;
			MyPlantScanFlag = true;

		}

		if ( MyPlantScanFlag( DesicDehumNum ) && allocated( PlantLoop ) ) {
			if ( ( DesicDehum( DesicDehumNum ).RegenCoilType_Num == Coil_HeatingWater ) || ( DesicDehum( DesicDehumNum ).RegenCoilType_Num == Coil_HeatingSteam ) ) {
				if ( DesicDehum( DesicDehumNum ).RegenCoilType_Num == Coil_HeatingWater ) {
					ErrorFlag = false;
					ScanPlantLoopsForObject( DesicDehum( DesicDehumNum ).RegenCoilName, TypeOf_CoilWaterSimpleHeating, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum, _, _, _, _, _, ErrorFlag );
					if ( ErrorFlag ) {
						ShowFatalError( "InitDesiccantDehumidifier: Program terminated for previous conditions." );
					}

					ErrorFlag = false;
					DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", DesicDehum( DesicDehumNum ).RegenCoilName, ErrorFlag );
					if ( DesicDehum( DesicDehumNum ).MaxCoilFluidFlow > 0.0 ) {
						FluidDensity = GetDensityGlycol( PlantLoop( DesicDehum( DesicDehumNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( DesicDehum( DesicDehumNum ).LoopNum ).FluidIndex, initCBVAV );
						DesicDehum( DesicDehumNum ).MaxCoilFluidFlow *= FluidDensity;
					}

				} else if ( DesicDehum( DesicDehumNum ).RegenCoilType_Num == Coil_HeatingSteam ) {

					ErrorFlag = false;
					ScanPlantLoopsForObject( DesicDehum( DesicDehumNum ).RegenCoilName, TypeOf_CoilSteamAirHeating, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum, _, _, _, _, _, ErrorFlag );

					if ( ErrorFlag ) {
						ShowFatalError( "InitDesiccantDehumidifier: Program terminated for previous conditions." );
					}
					ErrorFlag = false;
					DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = GetCoilMaxSteamFlowRate( DesicDehum( DesicDehumNum ).RegenCoilIndex, ErrorFlag );

					if ( DesicDehum( DesicDehumNum ).MaxCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						FluidDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
						DesicDehum( DesicDehumNum ).MaxCoilFluidFlow *= FluidDensity;
					}

				}

				// fill outlet node for regenartion hot water or steam heating coil
				DesicDehum( DesicDehumNum ).CoilOutletNode = PlantLoop( DesicDehum( DesicDehumNum ).LoopNum ).LoopSide( DesicDehum( DesicDehumNum ).LoopSide ).Branch( DesicDehum( DesicDehumNum ).BranchNum ).Comp( DesicDehum( DesicDehumNum ).CompNum ).NodeNumOut;
				MyPlantScanFlag( DesicDehumNum ) = false;

			} else { // DesicDehum is not connected to plant
				MyPlantScanFlag( DesicDehumNum ) = false;
			}
		} else if ( MyPlantScanFlag( DesicDehumNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( DesicDehumNum ) = false;
		}

		{ auto const SELECT_CASE_var( ( DesicDehum( DesicDehumNum ).DehumTypeCode ) );

		if ( SELECT_CASE_var == Solid ) {

			if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
				if ( DesicDehum( DesicDehumNum ).ControlType == NodeHumratBypass ) {
					ControlNode = DesicDehum( DesicDehumNum ).ProcAirOutNode;
					if ( ControlNode > 0 ) {
						if ( Node( ControlNode ).HumRatMax == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( "Missing humidity ratio setpoint (HumRatMax) for " );
								ShowContinueError( "Dehumidifier:Desiccant:NoFans: " + DesicDehum( DesicDehumNum ).Name );
								ShowContinueError( "Node Referenced=" + NodeID( ControlNode ) );
								ShowContinueError( "use a Setpoint Manager to establish a setpoint at the process air outlet node." );
								SetPointErrorFlag = true;
							} else {
								CheckIfNodeSetPointManagedByEMS( ControlNode, iHumidityRatioMaxSetPoint, SetPointErrorFlag );
								if ( SetPointErrorFlag ) {
									ShowSevereError( "Missing humidity ratio setpoint (HumRatMax) for " );
									ShowContinueError( "Dehumidifier:Desiccant:NoFans: " + DesicDehum( DesicDehumNum ).Name );
									ShowContinueError( "Node Referenced=" + NodeID( ControlNode ) );
									ShowContinueError( "use a Setpoint Manager to establish a setpoint at the process air outlet node." );
									ShowContinueError( "Or use EMS Actuator to establish a setpoint at the process air outlet node." );
								}
							}
						}
					}
				}
				MySetPointCheckFlag = false;
			}
			// always do these initializations every iteration
			ProcInNode = DesicDehum( DesicDehumNum ).ProcAirInNode;
			DesicDehum( DesicDehumNum ).ProcAirInTemp = Node( ProcInNode ).Temp;
			DesicDehum( DesicDehumNum ).ProcAirInHumRat = Node( ProcInNode ).HumRat;
			DesicDehum( DesicDehumNum ).ProcAirInEnthalpy = Node( ProcInNode ).Enthalpy;
			DesicDehum( DesicDehumNum ).ProcAirInMassFlowRate = Node( ProcInNode ).MassFlowRate;

			//  Determine heating coil inlet conditions by calling it with zero load
			//  Not sure if this is really a good way to do this, should revisit for next release.
			CalcNonDXHeatingCoils( DesicDehumNum, FirstHVACIteration, 0.0 );

			RegenInNode = DesicDehum( DesicDehumNum ).RegenAirInNode;
			DesicDehum( DesicDehumNum ).RegenAirInTemp = Node( RegenInNode ).Temp;
			DesicDehum( DesicDehumNum ).RegenAirInHumRat = Node( RegenInNode ).HumRat;
			DesicDehum( DesicDehumNum ).RegenAirInEnthalpy = Node( RegenInNode ).Enthalpy;

			DesicDehum( DesicDehumNum ).WaterRemove = 0.0;
			DesicDehum( DesicDehumNum ).ElecUseEnergy = 0.0;
			DesicDehum( DesicDehumNum ).ElecUseRate = 0.0;

		} else if ( SELECT_CASE_var == Generic ) {

			//      Do the Begin Environment initializations
			if ( BeginEnvrnFlag && MyEnvrnFlag( DesicDehumNum ) ) {
				//Change the Volume Flow Rates to Mass Flow Rates
				DesicDehum( DesicDehumNum ).ExhaustFanMaxMassFlowRate = DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate * StdRhoAir;

				//   set fluid-side hardware limits
				if ( DesicDehum( DesicDehumNum ).CoilControlNode > 0 ) {
					//    If water coil max water flow rate is autosized, simulate once in order to mine max water flow rate
					if ( DesicDehum( DesicDehumNum ).MaxCoilFluidFlow == AutoSize ) {
						if ( DesicDehum( DesicDehumNum ).RegenCoilType_Num == Coil_HeatingWater ) {
							SimulateWaterCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenCoilIndex );
							ErrorFlag = false;
							CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", DesicDehum( DesicDehumNum ).RegenCoilName, ErrorFlag );
							if ( ErrorFlag ) {
								ErrorsFound = true;
							}
							if ( CoilMaxVolFlowRate != AutoSize ) {
								FluidDensity = GetDensityGlycol( PlantLoop( DesicDehum( DesicDehumNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( DesicDehum( DesicDehumNum ).LoopNum ).FluidIndex, RoutineName );
								DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity;
							}
						}
						if ( DesicDehum( DesicDehumNum ).RegenCoilType_Num == Coil_HeatingSteam ) {
							SimulateSteamCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenCoilIndex, 1.0, QCoilActual ); //simulate any load > 0 to get max capacity of steam coil
							ErrorFlag = false;
							CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( DesicDehum( DesicDehumNum ).RegenCoilIndex, ErrorFlag );
							if ( ErrorFlag ) {
								ErrorsFound = true;
							}
							if ( CoilMaxVolFlowRate != AutoSize ) {
								SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
								FluidDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
								DesicDehum( DesicDehumNum ).MaxCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity;
							}
						}
					}
					InitComponentNodes( 0.0, DesicDehum( DesicDehumNum ).MaxCoilFluidFlow, DesicDehum( DesicDehumNum ).CoilControlNode, DesicDehum( DesicDehumNum ).CoilOutletNode, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum );
				}

				MyEnvrnFlag( DesicDehumNum ) = false;
			}

			if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
				ControlNode = DesicDehum( DesicDehumNum ).ControlNodeNum;
				if ( ControlNode > 0 ) {
					if ( Node( ControlNode ).HumRatMax == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							ShowSevereError( "Missing maximum humidity ratio setpoint (MaxHumRat) for " );
							ShowContinueError( DesicDehum( DesicDehumNum ).DehumType + ": " + DesicDehum( DesicDehumNum ).Name );
							ShowContinueError( "Node Referenced=" + NodeID( ControlNode ) );
							ShowContinueError( "use a Setpoint Manager to establish a \"MaxHumRat\" setpoint at the process air control node." );
							SetPointErrorFlag = true;
						} else {
							CheckIfNodeSetPointManagedByEMS( ControlNode, iHumidityRatioMaxSetPoint, SetPointErrorFlag );
							if ( SetPointErrorFlag ) {
								ShowSevereError( "Missing maximum humidity ratio setpoint (MaxHumRat) for " );
								ShowContinueError( DesicDehum( DesicDehumNum ).DehumType + ": " + DesicDehum( DesicDehumNum ).Name );
								ShowContinueError( "Node Referenced=" + NodeID( ControlNode ) );
								ShowContinueError( "use a Setpoint Manager to establish a \"MaxHumRat\" setpoint at the process air control node." );
								ShowContinueError( "Or use EMS Actuator to establish a setpoint at the process air outlet node." );
							}
						}

					}
				}
				MySetPointCheckFlag = false;
			}
			RegenInNode = DesicDehum( DesicDehumNum ).RegenAirInNode;
			DesicDehum( DesicDehumNum ).RegenAirInTemp = Node( RegenInNode ).Temp;
			DesicDehum( DesicDehumNum ).RegenAirInMassFlowRate = Node( RegenInNode ).MassFlowRate;

			DesicDehum( DesicDehumNum ).ExhaustFanPower = 0.0;
			DesicDehum( DesicDehumNum ).WaterRemoveRate = 0.0;

		}}

	}

	void
	ControlDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 & HumRatNeeded, // process air leaving humidity ratio set by controller [kg water/kg air]
		bool const EP_UNUSED( FirstHVACIteration ) // TRUE if 1st HVAC simulation of system timestep !unused1208
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
		//                      for Gas Research Institute
		//       DATE WRITTEN   March 2001
		//       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add new control type option:
		//                          NODE LEAVING HUMRAT SETPOINT:BYPASS
		//                        Change existing control type to:
		//                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
		//                        Work supported by ASHRAE research project 1254-RP
		//                      June 2007 R. Raustad, FSEC
		//                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets the output required from the dehumidifier

		// METHODOLOGY EMPLOYED:
		// Uses a maximum humidity ratio setpoint to calculate required process
		// leaving humidity ratio

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
		bool UnitOn; // unit on flag
		Real64 ProcAirMassFlowRate; // process air mass flow rate [kg/s]
		Real64 RegenAirMassFlowRate; // regen air mass flow rate [kg/s]

		ProcAirMassFlowRate = 0.0;
		RegenAirMassFlowRate = 0.0;
		UnitOn = true;

		{ auto const SELECT_CASE_var( ( DesicDehum( DesicDehumNum ).DehumTypeCode ) );

		if ( SELECT_CASE_var == Solid ) {

			if ( DesicDehum( DesicDehumNum ).HumRatSet <= 0.0 ) UnitOn = false;
			ProcAirMassFlowRate = DesicDehum( DesicDehumNum ).ProcAirInMassFlowRate;
			if ( ProcAirMassFlowRate <= SmallMassFlow ) UnitOn = false;

			if ( GetCurrentScheduleValue( DesicDehum( DesicDehumNum ).SchedPtr ) <= 0.0 ) UnitOn = false;

			// If incoming conditions are outside valid range for curve fits, then shut unit off, do not issue warnings

			if ( UnitOn ) {
				if ( ( DesicDehum( DesicDehumNum ).ProcAirInTemp < DesicDehum( DesicDehumNum ).MinProcAirInTemp ) || ( DesicDehum( DesicDehumNum ).ProcAirInTemp > DesicDehum( DesicDehumNum ).MaxProcAirInTemp ) ) {
					UnitOn = false;
				}
				if ( ( DesicDehum( DesicDehumNum ).ProcAirInHumRat < DesicDehum( DesicDehumNum ).MinProcAirInHumRat ) || ( DesicDehum( DesicDehumNum ).ProcAirInHumRat > DesicDehum( DesicDehumNum ).MaxProcAirInHumRat ) ) {
					UnitOn = false;
				}
			}

			if ( UnitOn ) {

				// perform the correct dehumidifier control strategy
				{ auto const SELECT_CASE_var1( DesicDehum( DesicDehumNum ).ControlType );

				if ( SELECT_CASE_var1 == FixedHumratBypass ) {

					HumRatNeeded = DesicDehum( DesicDehumNum ).HumRatSet;
					if ( HumRatNeeded <= 0.0 ) {
						ShowSevereError( "Dehumidifier:Desiccant:NoFans: " + DesicDehum( DesicDehumNum ).Name );
						ShowContinueError( "Invalid Leaving Max Humidity Ratio Setpoint=" + TrimSigDigits( HumRatNeeded, 8 ) );
						ShowFatalError( "must be > 0.0" );
					}

				} else if ( SELECT_CASE_var1 == NodeHumratBypass ) {

					HumRatNeeded = Node( DesicDehum( DesicDehumNum ).ProcAirOutNode ).HumRatMax;

				} else {

					ShowFatalError( "Invalid control type in desiccant dehumidifier = " + DesicDehum( DesicDehumNum ).Name );

				}}

				// Setpoint of zero indicates no load from setpoint manager max hum
				if ( ( HumRatNeeded == 0.0 ) || ( DesicDehum( DesicDehumNum ).ProcAirInHumRat <= HumRatNeeded ) ) {
					UnitOn = false;
					HumRatNeeded = DesicDehum( DesicDehumNum ).ProcAirInHumRat;
				}
			} else {
				HumRatNeeded = DesicDehum( DesicDehumNum ).ProcAirInHumRat;
			}

		} else if ( SELECT_CASE_var == Generic ) {

			ProcAirMassFlowRate = Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).MassFlowRate;
			if ( ProcAirMassFlowRate <= SmallMassFlow ) UnitOn = false;

			if ( GetCurrentScheduleValue( DesicDehum( DesicDehumNum ).SchedPtr ) <= 0.0 ) UnitOn = false;

			if ( UnitOn ) {
				if ( DesicDehum( DesicDehumNum ).ControlNodeNum == DesicDehum( DesicDehumNum ).ProcAirOutNode ) {
					HumRatNeeded = Node( DesicDehum( DesicDehumNum ).ControlNodeNum ).HumRatMax;
				} else {
					if ( Node( DesicDehum( DesicDehumNum ).ControlNodeNum ).HumRatMax > 0.0 ) {
						HumRatNeeded = Node( DesicDehum( DesicDehumNum ).ControlNodeNum ).HumRatMax - ( Node( DesicDehum( DesicDehumNum ).ControlNodeNum ).HumRat - Node( DesicDehum( DesicDehumNum ).ProcAirOutNode ).HumRat );
					} else {
						HumRatNeeded = 0.0;
					}
				}

				// Setpoint of zero indicates no load from setpoint manager max hum
				if ( ( HumRatNeeded == 0.0 ) || ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat <= HumRatNeeded ) ) {
					HumRatNeeded = Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat;
				}
			} else {
				HumRatNeeded = Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat;
			}

		} else {

		}}

	}

	void
	CalcSolidDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 const HumRatNeeded, // process air leaving humidity ratio set by controller [kgWater/kgDryAir]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
		//                      for Gas Research Institute
		//       DATE WRITTEN   March 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the electricity consumption, regen heat requirements and the outlet
		// conditions for a solid desiccant dehumidifier, given the inlet conditions and
		// and the needed process leaving humidity ratio.

		// METHODOLOGY EMPLOYED:
		// Given the entering conditions, the full-load outlet conditions are calculated.
		// Adjust for part-load if required.
		// Caclulate required regen energy and call regen coil and regen fan.
		// Desiccant wheel leaving conditions and regen energy requirements are calculated
		// from empirical curve fits.  The user can select either default built-in
		// performance curves, or use custom user-defined curves.

		// REFERENCES:
		// The default performance curves represent a commerical-grade solid desiccant
		// wheel typical of HVAC applications in the early 1990's.  These curves were
		// developed for Gas Research Institute by William W. Worek, University of Illinois
		// at Chicago.

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		//unused  USE DataEnvironment, ONLY: StdBaroPress

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 ProcAirInHumRat; // process inlet air humidity ratio [kgWater/kgDryAir]
		Real64 ProcAirInTemp; // process inlet air temperature [C]
		Real64 ProcAirOutHumRat; // process outlet air humidity ratio [kgWater/kgDryAir]
		Real64 MinProcAirOutHumRat; // minimum available process outlet air humidity ratio [kgWater/kgDryAir]
		Real64 ProcAirOutTemp; // process outlet air temperature [C]
		Real64 ProcAirVel; // process air velocity [m/s]
		Real64 QRegen; // regen heat input rate requested from regen coil [W]
		Real64 QDelivered; // regen heat actually delivered by regen coil [W]
		//REAL(r64) :: RegenAirInHumRat        ! regen inlet air humidity ratio [kgWater/kgDryAir]
		Real64 RegenAirInTemp; // regen inlet air temperature [C]
		Real64 RegenAirVel; // regen air velocity [m/s]
		Real64 ProcAirMassFlowRate; // process air mass flow rate [kg/s]
		Real64 RegenAirMassFlowRate; // regen air mass flow rate [kg/s]
		Real64 SpecRegenEnergy; // specific regen energy [J/kg of water removed]
		Real64 NomRegenTemp; // nominal regen temperature for regen energy curve
		Real64 ElecUseRate; // electricity consumption rate [W]
		Real64 PartLoad; // fraction of dehumidification capacity required to meet setpoint
		bool UnitOn; // unit on flag

		static bool MyOneTimeFlag( true ); // one time flag
		static Real64 RhoAirStdInit;

		// Variables for hardwired coefficients for default performance model

		Real64 TC0;
		Real64 TC1;
		Real64 TC2;
		Real64 TC3;
		Real64 TC4;
		Real64 TC5;
		Real64 TC6;
		Real64 TC7;
		Real64 TC8;
		Real64 TC9;
		Real64 TC10;
		Real64 TC11;
		Real64 TC12;
		Real64 TC13;
		Real64 TC14;
		Real64 TC15;

		Real64 WC0;
		Real64 WC1;
		Real64 WC2;
		Real64 WC3;
		Real64 WC4;
		Real64 WC5;
		Real64 WC6;
		Real64 WC7;
		Real64 WC8;
		Real64 WC9;
		Real64 WC10;
		Real64 WC11;
		Real64 WC12;
		Real64 WC13;
		Real64 WC14;
		Real64 WC15;

		Real64 QC0;
		Real64 QC1;
		Real64 QC2;
		Real64 QC3;
		Real64 QC4;
		Real64 QC5;
		Real64 QC6;
		Real64 QC7;
		Real64 QC8;
		Real64 QC9;
		Real64 QC10;
		Real64 QC11;
		Real64 QC12;
		Real64 QC13;
		Real64 QC14;
		Real64 QC15;

		Real64 RC0;
		Real64 RC1;
		Real64 RC2;
		Real64 RC3;
		Real64 RC4;
		Real64 RC5;
		Real64 RC6;
		Real64 RC7;
		Real64 RC8;
		Real64 RC9;
		Real64 RC10;
		Real64 RC11;
		Real64 RC12;
		Real64 RC13;
		Real64 RC14;
		Real64 RC15;

		// Setup internal variables for calculations

		ProcAirInTemp = DesicDehum( DesicDehumNum ).ProcAirInTemp;
		ProcAirInHumRat = DesicDehum( DesicDehumNum ).ProcAirInHumRat;
		ProcAirMassFlowRate = DesicDehum( DesicDehumNum ).ProcAirInMassFlowRate;
		ProcAirVel = DesicDehum( DesicDehumNum ).NomProcAirVel;
		PartLoad = 0.0;

		RegenAirInTemp = DesicDehum( DesicDehumNum ).RegenAirInTemp;
		NomRegenTemp = DesicDehum( DesicDehumNum ).NomRegenTemp;

		// Calculate min available process out humrat
		UnitOn = false;
		MinProcAirOutHumRat = 0.0; // MAX(MinProcAirOutHumRat,0.000857)

		if ( HumRatNeeded < ProcAirInHumRat ) {

			UnitOn = true;

			{ auto const SELECT_CASE_var( DesicDehum( DesicDehumNum ).PerformanceModel_Num ); // Performance Model Part A

			if ( SELECT_CASE_var == PM_Default ) {

				WC0 = 0.0148880824323806;
				WC1 = -0.000283393198398211;
				WC2 = -0.87802168940547;
				WC3 = -0.000713615831236411;
				WC4 = 0.0311261188874622;
				WC5 = 1.51738892142485e-06;
				WC6 = 0.0287250198281021;
				WC7 = 4.94796903231558e-06;
				WC8 = 24.0771139652826;
				WC9 = 0.000122270283927978;
				WC10 = -0.0151657189566474;
				WC11 = 3.91641393230322e-08;
				WC12 = 0.126032651553348;
				WC13 = 0.000391653854431574;
				WC14 = 0.002160537360507;
				WC15 = 0.00132732844211593;

				MinProcAirOutHumRat = WC0 + WC1 * ProcAirInTemp + WC2 * ProcAirInHumRat + WC3 * ProcAirVel + WC4 * ProcAirInTemp * ProcAirInHumRat + WC5 * ProcAirInTemp * ProcAirVel + WC6 * ProcAirInHumRat * ProcAirVel + WC7 * ProcAirInTemp * ProcAirInTemp + WC8 * ProcAirInHumRat * ProcAirInHumRat + WC9 * ProcAirVel * ProcAirVel + WC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat + WC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel + WC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + WC13 * std::log( ProcAirInTemp ) + WC14 * std::log( ProcAirInHumRat ) + WC15 * std::log( ProcAirVel );

				// limit to 6 grains/lb (0.000857 kg/kg)

			} else if ( SELECT_CASE_var == PM_UserCurves ) {

				MinProcAirOutHumRat = CurveValue( DesicDehum( DesicDehumNum ).ProcHumRatCurvefTW, ProcAirInTemp, ProcAirInHumRat ) * CurveValue( DesicDehum( DesicDehumNum ).ProcHumRatCurvefV, ProcAirVel );

			} else {

				ShowFatalError( "Invalid performance model in desiccant dehumidifier = " + TrimSigDigits( DesicDehum( DesicDehumNum ).PerformanceModel_Num ) );

			}} // Performance Model Part A

			MinProcAirOutHumRat = max( MinProcAirOutHumRat, 0.000857 );

		}

		if ( MinProcAirOutHumRat >= ProcAirInHumRat ) UnitOn = false;

		if ( UnitOn ) {

			// Calculate partload fraction of dehumidification capacity required to meet setpoint
			PartLoad = 1.0;
			if ( MinProcAirOutHumRat < HumRatNeeded ) PartLoad = ( ProcAirInHumRat - HumRatNeeded ) / ( ProcAirInHumRat - MinProcAirOutHumRat );
			PartLoad = max( 0.0, PartLoad );
			PartLoad = min( 1.0, PartLoad );

			{ auto const SELECT_CASE_var( DesicDehum( DesicDehumNum ).PerformanceModel_Num ); // Performance Model Part B

			if ( SELECT_CASE_var == PM_Default ) {

				// Calculate leaving conditions
				TC0 = -38.7782841989449;
				TC1 = 2.0127655837628;
				TC2 = 5212.49360216097;
				TC3 = 15.2362536782665;
				TC4 = -80.4910419759181;
				TC5 = -0.105014122001509;
				TC6 = -229.668673645144;
				TC7 = -0.015424703743461;
				TC8 = -69440.0689831847;
				TC9 = -1.6686064694322;
				TC10 = 38.5855718977592;
				TC11 = 0.000196395381206009;
				TC12 = 386.179386548324;
				TC13 = -0.801959614172614;
				TC14 = -3.33080986818745;
				TC15 = -15.2034386065714;

				ProcAirOutTemp = TC0 + TC1 * ProcAirInTemp + TC2 * ProcAirInHumRat + TC3 * ProcAirVel + TC4 * ProcAirInTemp * ProcAirInHumRat + TC5 * ProcAirInTemp * ProcAirVel + TC6 * ProcAirInHumRat * ProcAirVel + TC7 * ProcAirInTemp * ProcAirInTemp + TC8 * ProcAirInHumRat * ProcAirInHumRat + TC9 * ProcAirVel * ProcAirVel + TC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat + TC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel + TC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + TC13 * std::log( ProcAirInTemp ) + TC14 * std::log( ProcAirInHumRat ) + TC15 * std::log( ProcAirVel );

				// Regen energy
				QC0 = -27794046.6291107;
				QC1 = -235725.171759615;
				QC2 = 975461343.331328;
				QC3 = -686069.373946731;
				QC4 = -17717307.3766266;
				QC5 = 31482.2539662489;
				QC6 = 55296552.8260743;
				QC7 = 6195.36070023868;
				QC8 = -8304781359.40435;
				QC9 = -188987.543809419;
				QC10 = 3933449.40965846;
				QC11 = -6.66122876558634;
				QC12 = -349102295.417547;
				QC13 = 83672.179730172;
				QC14 = -6059524.33170538;
				QC15 = 1220523.39525162;

				SpecRegenEnergy = QC0 + QC1 * ProcAirInTemp + QC2 * ProcAirInHumRat + QC3 * ProcAirVel + QC4 * ProcAirInTemp * ProcAirInHumRat + QC5 * ProcAirInTemp * ProcAirVel + QC6 * ProcAirInHumRat * ProcAirVel + QC7 * ProcAirInTemp * ProcAirInTemp + QC8 * ProcAirInHumRat * ProcAirInHumRat + QC9 * ProcAirVel * ProcAirVel + QC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat + QC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel + QC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + QC13 * std::log( ProcAirInTemp ) + QC14 * std::log( ProcAirInHumRat ) + QC15 * std::log( ProcAirVel );

				// Regen face velocity
				RC0 = -4.67358908091488;
				RC1 = 0.0654323095468338;
				RC2 = 396.950518702316;
				RC3 = 1.52610165426736;
				RC4 = -11.3955868430328;
				RC5 = 0.00520693906104437;
				RC6 = 57.783645385621;
				RC7 = -0.000464800668311693;
				RC8 = -5958.78613212602;
				RC9 = -0.205375818291012;
				RC10 = 5.26762675442845;
				RC11 = -8.88452553055039e-05;
				RC12 = -182.382479369311;
				RC13 = -0.100289774002047;
				RC14 = -0.486980507964251;
				RC15 = -0.972715425435447;

				RegenAirVel = RC0 + RC1 * ProcAirInTemp + RC2 * ProcAirInHumRat + RC3 * ProcAirVel + RC4 * ProcAirInTemp * ProcAirInHumRat + RC5 * ProcAirInTemp * ProcAirVel + RC6 * ProcAirInHumRat * ProcAirVel + RC7 * ProcAirInTemp * ProcAirInTemp + RC8 * ProcAirInHumRat * ProcAirInHumRat + RC9 * ProcAirVel * ProcAirVel + RC10 * ProcAirInTemp * ProcAirInTemp * ProcAirInHumRat * ProcAirInHumRat + RC11 * ProcAirInTemp * ProcAirInTemp * ProcAirVel * ProcAirVel + RC12 * ProcAirInHumRat * ProcAirInHumRat * ProcAirVel * ProcAirVel + RC13 * std::log( ProcAirInTemp ) + RC14 * std::log( ProcAirInHumRat ) + RC15 * std::log( ProcAirVel );

			} else if ( SELECT_CASE_var == PM_UserCurves ) {

				ProcAirOutTemp = CurveValue( DesicDehum( DesicDehumNum ).ProcDryBulbCurvefTW, ProcAirInTemp, ProcAirInHumRat ) * CurveValue( DesicDehum( DesicDehumNum ).ProcDryBulbCurvefV, ProcAirVel );

				SpecRegenEnergy = CurveValue( DesicDehum( DesicDehumNum ).RegenEnergyCurvefTW, ProcAirInTemp, ProcAirInHumRat ) * CurveValue( DesicDehum( DesicDehumNum ).RegenEnergyCurvefV, ProcAirVel );

				RegenAirVel = CurveValue( DesicDehum( DesicDehumNum ).RegenVelCurvefTW, ProcAirInTemp, ProcAirInHumRat ) * CurveValue( DesicDehum( DesicDehumNum ).RegenVelCurvefV, ProcAirVel );

			} else {

				ShowFatalError( "Invalid performance model in desiccant dehumidifier = " + TrimSigDigits( DesicDehum( DesicDehumNum ).PerformanceModel_Num ) );

				// Suppress uninitialized warnings
				ProcAirOutTemp = 0.0;
				SpecRegenEnergy = 0.0;
				RegenAirVel = 0.0;

			}} // Performance Model Part B

			ProcAirOutTemp = ( 1 - PartLoad ) * ProcAirInTemp + ( PartLoad ) * ProcAirOutTemp;

			ProcAirOutHumRat = ( 1 - PartLoad ) * ProcAirInHumRat + ( PartLoad ) * MinProcAirOutHumRat;

			// Calculate water removal
			DesicDehum( DesicDehumNum ).WaterRemoveRate = ProcAirMassFlowRate * ( ProcAirInHumRat - ProcAirOutHumRat );

			// Adjust for regen inlet temperature
			SpecRegenEnergy *= ( NomRegenTemp - RegenAirInTemp ) / ( NomRegenTemp - ProcAirInTemp );
			SpecRegenEnergy = max( SpecRegenEnergy, 0.0 );
			QRegen = SpecRegenEnergy * DesicDehum( DesicDehumNum ).WaterRemoveRate;

			// Above curves are based on a 90deg regen angle and 245deg process air angle
			RegenAirMassFlowRate = ProcAirMassFlowRate * 90.0 / 245.0 * RegenAirVel / ProcAirVel;

			ElecUseRate = DesicDehum( DesicDehumNum ).NomRotorPower;

		} else { // Unit is off

			ProcAirOutTemp = ProcAirInTemp;
			ProcAirOutHumRat = ProcAirInHumRat;
			SpecRegenEnergy = 0.0;
			QRegen = 0.0;
			ElecUseRate = 0.0;
			RegenAirVel = 0.0;
			RegenAirMassFlowRate = 0.0;
			DesicDehum( DesicDehumNum ).WaterRemoveRate = 0.0;
			PartLoad = 0.0;

		} // UnitOn/Off

		// Set regen mass flow
		Node( DesicDehum( DesicDehumNum ).RegenFanInNode ).MassFlowRate = RegenAirMassFlowRate;
		Node( DesicDehum( DesicDehumNum ).RegenFanInNode ).MassFlowRateMaxAvail = RegenAirMassFlowRate;
		// Call regen fan
		SimulateFanComponents( DesicDehum( DesicDehumNum ).RegenFanName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenFanIndex );
		// Call regen heating coil
		CalcNonDXHeatingCoils( DesicDehumNum, FirstHVACIteration, QRegen, QDelivered );

		// Verify is requestd flow was delivered (must do after heating coil has executed to pass flow to RegenAirInNode)
		if ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate != RegenAirMassFlowRate ) {
			// Initialize standard air density
			if ( MyOneTimeFlag ) {
				RhoAirStdInit = StdRhoAir;
			}
			ShowRecurringSevereErrorAtEnd( "Improper flow delivered by desiccant regen fan - RESULTS INVALID! Check regen fan capacity and schedule.", DesicDehum( DesicDehumNum ).RegenFanErrorIndex1 );
			ShowRecurringContinueErrorAtEnd( DesicDehum( DesicDehumNum ).DehumType + '=' + DesicDehum( DesicDehumNum ).Name, DesicDehum( DesicDehumNum ).RegenFanErrorIndex2 );
			RhoAirStdInit = StdRhoAir;
			ShowRecurringContinueErrorAtEnd( "Flow requested [m3/s] from " + DesicDehum( DesicDehumNum ).RegenFanType + '=' + DesicDehum( DesicDehumNum ).RegenFanName, DesicDehum( DesicDehumNum ).RegenFanErrorIndex3, ( RegenAirMassFlowRate / RhoAirStdInit ) );
			ShowRecurringContinueErrorAtEnd( "Flow request varied from delivered by [m3/s]", DesicDehum( DesicDehumNum ).RegenFanErrorIndex4, ( ( RegenAirMassFlowRate - Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate ) / RhoAirStdInit ), ( ( RegenAirMassFlowRate - Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate ) / RhoAirStdInit ) );
		}

		// Verify is requestd heating was delivered
		if ( QDelivered < QRegen ) {
			ShowRecurringSevereErrorAtEnd( "Inadequate heat delivered by desiccant regen coil - RESULTS INVALID! Check regen coil capacity and schedule.", DesicDehum( DesicDehumNum ).RegenCapErrorIndex1 );
			ShowRecurringContinueErrorAtEnd( DesicDehum( DesicDehumNum ).DehumType + '=' + DesicDehum( DesicDehumNum ).Name, DesicDehum( DesicDehumNum ).RegenCapErrorIndex2 );
			ShowRecurringContinueErrorAtEnd( "Load requested [W] from " + DesicDehum( DesicDehumNum ).RegenCoilType + '=' + DesicDehum( DesicDehumNum ).RegenCoilName, DesicDehum( DesicDehumNum ).RegenCapErrorIndex3, QRegen );
			ShowRecurringContinueErrorAtEnd( "Load request exceeded delivered by [W]", DesicDehum( DesicDehumNum ).RegenCapErrorIndex4, ( QRegen - QDelivered ) );
		}

		DesicDehum( DesicDehumNum ).SpecRegenEnergy = SpecRegenEnergy;
		DesicDehum( DesicDehumNum ).QRegen = QRegen;
		DesicDehum( DesicDehumNum ).ElecUseRate = ElecUseRate;
		DesicDehum( DesicDehumNum ).PartLoad = PartLoad;

		DesicDehum( DesicDehumNum ).ProcAirOutMassFlowRate = ProcAirMassFlowRate;
		DesicDehum( DesicDehumNum ).ProcAirOutTemp = ProcAirOutTemp;
		DesicDehum( DesicDehumNum ).ProcAirOutHumRat = ProcAirOutHumRat;
		DesicDehum( DesicDehumNum ).ProcAirOutEnthalpy = PsyHFnTdbW( ProcAirOutTemp, ProcAirOutHumRat );
		DesicDehum( DesicDehumNum ).RegenAirInMassFlowRate = RegenAirMassFlowRate;
		DesicDehum( DesicDehumNum ).RegenAirVel = RegenAirVel;

		//  DesicDehum(DesicDehumNum)%RegenAirOutTemp        = -999.
		//  DesicDehum(DesicDehumNum)%RegenAirOutHumRat      = -999.
		//  DesicDehum(DesicDehumNum)%RegenAirOutEnthalpy    = -999.

	}

	void
	CalcGenericDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 const HumRatNeeded, // process air leaving humidity ratio set by controller [kg water/kg air]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mangesh Basarkar, FSEC
		//       DATE WRITTEN   May 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the electricity consumption, regen heat requirements and the outlet
		// conditions for a desiccant dehumidifier, given the inlet conditions,
		// DX coil part-load ratio, and/or the needed process leaving humidity ratio.

		// METHODOLOGY EMPLOYED:
		// Given the entering conditions, the full-load outlet conditions are calculated.
		// Adjust for part-load if required.
		// Calculate the required regen energy and call the regen coil and the regen fan.

		// REFERENCES:
		// Kosar, D. 2006. Dehumidification Enhancements, ASHRAE Journal, Vol. 48, No. 2, February 2006.
		// Kosar, D. et al. 2006. Dehumidification Enhancement of Direct Expansion Systems Through Component
		//   Augmentation of the Cooling Coil. 15th Symposium on Improving Building Systems in Hot and Humid
		//   Climates, July 24-26, 2006.

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		//unused  USE DataEnvironment, ONLY: StdBaroPress
		using HeatRecovery::SimHeatRecovery;
		using DXCoils::DXCoilPartLoadRatio;
		using DXCoils::DXCoilFanOpMode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MinVolFlowPerRatedTotQ( 0.00002684 ); // m3/s per W = 200 cfm/ton,
		// min vol flow per rated evaporator capacity
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DDPartLoadRatio; // fraction of dehumidification capacity required to meet setpoint
		static Real64 QRegen( 0.0 ); // required coil load passed to sim heating coil routine (W)
		Real64 MassFlowRateNew; // new required mass flow rate calculated to keep regen setpoint temperature (kg/s)
		Real64 CondenserWasteHeat; // Condenser waste heat (W)
		Real64 CpAir; // Specific heat of air (J/kg-K)
		Real64 NewRegenInTemp; // new temp calculated from condenser waste heat (C)
		Real64 ExhaustFanMassFlowRate; // exhaust fan mass flow rate (kg/s)
		Real64 ExhaustFanPLR; // exhaust fan run time fraction calculated from new mass flow rate for regen side
		Real64 ExhaustFanPowerMod; // used to calculate exhaust fan power from flow fraction
		Real64 VolFlowPerRatedTotQ; // flow rate per rated total cooling capacity of the companion coil (m3/s/W)
		Real64 FanDeltaT; // used to account for fan heat when calculating regeneration heater energy (C)
		Real64 OnOffFanPLF; // save air loop fan part load fracton while calculating exhaust fan power
		Real64 RegenSetPointTemp; // regeneration temperature setpoint (C)
		int RegenCoilIndex; // index to regeneration heating coil, 0 when not used
		int CompanionCoilIndexNum; // index for companion DX cooling coil, 0 when DX coil is not used
		std::string MinVol; // character string used for error messages
		std::string VolFlowChar; // character string used for error messages
		static bool MyOneTimeFlag( true ); // one time flag
		static Real64 RhoAirStdInit; // standard air density (kg/m3)
		bool UnitOn; // unit on flag
		//  LOGICAL       :: SimFlag                    ! used to turn off additional simulation if DX Coil is off
		Real64 QRegen_OASysFanAdjust; // temporary variable used to adjust regen heater load during iteration

		UnitOn = false;
		DDPartLoadRatio = 0.0;
		RegenCoilIndex = DesicDehum( DesicDehumNum ).RegenCoilIndex;
		FanDeltaT = 0.0;
		RegenSetPointTemp = DesicDehum( DesicDehumNum ).RegenSetPointTemp;
		ExhaustFanMassFlowRate = 0.0;

		// Save OnOffFanPartLoadFraction while performing exhaust fan calculations
		OnOffFanPLF = OnOffFanPartLoadFraction;
		OnOffFanPartLoadFraction = 1.0;

		if ( DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide == Yes ) {
			// Cooling coil directly upstream of desiccant dehumidifier, dehumidifier runs in tandem with DX coil
			CompanionCoilIndexNum = DesicDehum( DesicDehumNum ).DXCoilIndex;
		} else {
			// desiccant dehumidifier determines its own PLR
			CompanionCoilIndexNum = 0;
		}

		if ( MyOneTimeFlag ) {
			RhoAirStdInit = StdRhoAir;
			MyOneTimeFlag = false;
		}

		if ( HumRatNeeded < Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat ) {
			UnitOn = true;
		}

		if ( DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide == Yes ) {
			if ( DXCoilPartLoadRatio( DesicDehum( DesicDehumNum ).DXCoilIndex ) == 0.0 ) {
				UnitOn = false;
			}
		}

		if ( UnitOn ) {

			if ( DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode ) {
				if ( DesicDehum( DesicDehumNum ).HXTypeNum == BalancedHX ) {
					Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate = Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).MassFlowRate;
					Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRateMaxAvail = Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).MassFlowRate;
				}
			}

			// Get conditions from DX Coil condenser if present (DXCoilIndex verified > 0 in GetInput)
			if ( DesicDehum( DesicDehumNum ).Preheat == Yes ) {

				//     condenser waste heat is proportional to DX coil PLR
				CondenserWasteHeat = HeatReclaimDXCoil( DesicDehum( DesicDehumNum ).DXCoilIndex ).AvailCapacity;
				HeatReclaimDXCoil( DesicDehum( DesicDehumNum ).DXCoilIndex ).AvailCapacity = 0.0;

				CpAir = PsyCpAirFnWTdb( Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).HumRat, Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).Temp );

				if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
					SimulateFanComponents( DesicDehum( DesicDehumNum ).RegenFanName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenFanIndex );
					FanDeltaT = Node( DesicDehum( DesicDehumNum ).RegenFanOutNode ).Temp - Node( DesicDehum( DesicDehumNum ).RegenFanInNode ).Temp;
					//       Adjust setpoint to account for fan heat
					RegenSetPointTemp -= FanDeltaT;
				}

				//     CompanionCoilIndexNum .GT. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes
				if ( CompanionCoilIndexNum > 0 ) {

					DDPartLoadRatio = DXCoilPartLoadRatio( DesicDehum( DesicDehumNum ).DXCoilIndex );

				}

				//     calculate actual condenser outlet node (regen inlet node) temperature
				if ( CompanionCoilIndexNum > 0 ) {
					if ( DXCoilFanOpMode( DesicDehum( DesicDehumNum ).DXCoilIndex ) == ContFanCycCoil ) {
						NewRegenInTemp = Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).Temp + CondenserWasteHeat / ( CpAir * ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate ) * DDPartLoadRatio );
						CondenserWasteHeat /= DDPartLoadRatio;
					} else {
						NewRegenInTemp = Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).Temp + CondenserWasteHeat / ( CpAir * ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate ) );
					}
				} else {
					NewRegenInTemp = Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).Temp + CondenserWasteHeat / ( CpAir * ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate ) );
				}

				Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp = NewRegenInTemp;
				Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Enthalpy = PsyHFnTdbW( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp, Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).HumRat );
				MassFlowRateNew = 0.0;

				if ( DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate > 0 ) {

					//       calculate mass flow rate required to maintain regen inlet setpoint temp
					if ( NewRegenInTemp > RegenSetPointTemp ) {
						if ( RegenSetPointTemp - Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).Temp != 0.0 ) {
							MassFlowRateNew = max( 0.0, CondenserWasteHeat / ( CpAir * ( RegenSetPointTemp - Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).Temp ) ) );
						} else {
							MassFlowRateNew = Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate;
						}
					}

					//       calculate exhaust fan mass flow rate and new regen inlet temperature (may not be at setpoint)
					if ( MassFlowRateNew > Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate ) {
						ExhaustFanMassFlowRate = MassFlowRateNew - Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate;
						ExhaustFanMassFlowRate = max( 0.0, min( ExhaustFanMassFlowRate, DesicDehum( DesicDehumNum ).ExhaustFanMaxMassFlowRate ) );

						Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp = Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).Temp + CondenserWasteHeat / ( CpAir * ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate + ExhaustFanMassFlowRate ) );
						Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).HumRat = Node( DesicDehum( DesicDehumNum ).CondenserInletNode ).HumRat;
						Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Enthalpy = PsyHFnTdbW( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp, Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).HumRat );
					}

				}

				if ( RegenCoilIndex > 0 ) {
					if ( NewRegenInTemp < RegenSetPointTemp ) {
						CpAir = PsyCpAirFnWTdb( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).HumRat, Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp );
					}
					QRegen = max( 0.0, ( CpAir * Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate * ( RegenSetPointTemp - Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp ) ) );
					if ( QRegen == 0.0 ) QRegen = -1.0;
				}

				//     CompanionCoilIndexNum .EQ. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == No
				if ( CompanionCoilIndexNum == 0 ) {

					if ( RegenCoilIndex > 0 ) {

						QRegen_OASysFanAdjust = QRegen;
						if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
							if ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate > 0.0 ) {
								//             For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot reduction through fan
								QRegen_OASysFanAdjust *= Node( DesicDehum( DesicDehumNum ).RegenFanOutNode ).MassFlowRate / Node( DesicDehum( DesicDehumNum ).RegenFanInNode ).MassFlowRate;
							}
						}

						CalcNonDXHeatingCoils( DesicDehumNum, FirstHVACIteration, QRegen_OASysFanAdjust );
					}

					SimHeatRecovery( DesicDehum( DesicDehumNum ).HXName, FirstHVACIteration, DesicDehum( DesicDehumNum ).CompIndex, ContFanCycCoil, 1.0, true, CompanionCoilIndexNum, DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode );

					//       calculate desiccant part-load ratio
					if ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat != Node( DesicDehum( DesicDehumNum ).ProcAirOutNode ).HumRat ) {
						DDPartLoadRatio = ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat - HumRatNeeded ) / ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat - Node( DesicDehum( DesicDehumNum ).ProcAirOutNode ).HumRat );
						DDPartLoadRatio = max( 0.0, min( 1.0, DDPartLoadRatio ) );
					} else {
						DDPartLoadRatio = 1.0;
					}

				}

				if ( ExhaustFanMassFlowRate > 0.0 ) {

					//       calculate exhaust fan mass flow rate due to desiccant system operation
					ExhaustFanMassFlowRate *= DDPartLoadRatio;

					//       calculate exhaust fan PLR due to desiccant system operation
					ExhaustFanPLR = ExhaustFanMassFlowRate / DesicDehum( DesicDehumNum ).ExhaustFanMaxMassFlowRate;

					//       find exhaust fan power multiplier using exhaust fan part-load ratio
					if ( DesicDehum( DesicDehumNum ).ExhaustFanCurveIndex > 0 ) {
						ExhaustFanPowerMod = min( 1.0, max( 0.0, CurveValue( DesicDehum( DesicDehumNum ).ExhaustFanCurveIndex, ExhaustFanPLR ) ) );
					} else {
						ExhaustFanPowerMod = 1.0;
					}

					//       calculate exhaust fan power due to desiccant operation
					DesicDehum( DesicDehumNum ).ExhaustFanPower = DesicDehum( DesicDehumNum ).ExhaustFanMaxPower * ExhaustFanPowerMod;

				}

			} else { // ELSE for IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN

				if ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat > HumRatNeeded ) {

					//       Get Full load output of desiccant wheel
					if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
						SimulateFanComponents( DesicDehum( DesicDehumNum ).RegenFanName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenFanIndex );
						FanDeltaT = Node( DesicDehum( DesicDehumNum ).RegenFanOutNode ).Temp - Node( DesicDehum( DesicDehumNum ).RegenFanInNode ).Temp;
						RegenSetPointTemp -= FanDeltaT;
					}

					if ( RegenCoilIndex > 0 ) {
						CpAir = PsyCpAirFnWTdb( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).HumRat, Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp );
						QRegen = max( 0.0, ( CpAir * Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate * ( RegenSetPointTemp - Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).Temp ) ) );

						QRegen_OASysFanAdjust = QRegen;
						if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
							if ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate > 0.0 ) {
								//             For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot reduction through fan
								QRegen_OASysFanAdjust *= Node( DesicDehum( DesicDehumNum ).RegenFanOutNode ).MassFlowRate / Node( DesicDehum( DesicDehumNum ).RegenFanInNode ).MassFlowRate;
							}
						}

						if ( QRegen_OASysFanAdjust == 0.0 ) QRegen_OASysFanAdjust = -1.0;
						CalcNonDXHeatingCoils( DesicDehumNum, FirstHVACIteration, QRegen_OASysFanAdjust );
					}

					//       CompanionCoilIndexNum .EQ. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == No
					if ( CompanionCoilIndexNum == 0 ) {
						SimHeatRecovery( DesicDehum( DesicDehumNum ).HXName, FirstHVACIteration, DesicDehum( DesicDehumNum ).CompIndex, ContFanCycCoil, 1.0, true, CompanionCoilIndexNum, DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode );

						//         calculate desiccant part-load ratio
						if ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat != Node( DesicDehum( DesicDehumNum ).ProcAirOutNode ).HumRat ) {
							DDPartLoadRatio = ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat - HumRatNeeded ) / ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat - Node( DesicDehum( DesicDehumNum ).ProcAirOutNode ).HumRat );
							DDPartLoadRatio = max( 0.0, min( 1.0, DDPartLoadRatio ) );
						} else {
							DDPartLoadRatio = 1.0;
						}
					} else {
						DDPartLoadRatio = DXCoilPartLoadRatio( DesicDehum( DesicDehumNum ).DXCoilIndex );
					}
				} else { // ELSE for IF(Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .GT. HumRatNeeded)THEN
					DDPartLoadRatio = 0.0;
				} // END IF for IF(Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .GT. HumRatNeeded)THEN

			} // END IF for IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN

			DesicDehum( DesicDehumNum ).PartLoad = DDPartLoadRatio;
			QRegen_OASysFanAdjust = QRegen;

			// set average regeneration air mass flow rate based on desiccant cycling ratio (DDPartLoadRatio)
			if ( DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode ) {
				Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate *= DDPartLoadRatio;

				// **RR moved to here, only adjust regen heater load if mass flow rate is changed
				//   adjust regen heating coil capacity based on desiccant cycling ratio (PLR)
				QRegen_OASysFanAdjust *= DDPartLoadRatio;

			}

			// Call regen fan, balanced desiccant HX and heating coil
			if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
				SimulateFanComponents( DesicDehum( DesicDehumNum ).RegenFanName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenFanIndex );
			}

			if ( RegenCoilIndex > 0 ) {

				//!   adjust regen heating coil capacity based on desiccant cycling ratio (PLR)
				//    QRegen_OASysFanAdjust = QRegen * DDPartLoadRatio

				if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
					if ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate > 0.0 ) {
						//       For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot reduction through fan
						QRegen_OASysFanAdjust *= Node( DesicDehum( DesicDehumNum ).RegenFanOutNode ).MassFlowRate / Node( DesicDehum( DesicDehumNum ).RegenFanInNode ).MassFlowRate;
					}
				}

				if ( QRegen_OASysFanAdjust == 0.0 ) QRegen_OASysFanAdjust = -1.0;
				CalcNonDXHeatingCoils( DesicDehumNum, FirstHVACIteration, QRegen_OASysFanAdjust );
			}

			SimHeatRecovery( DesicDehum( DesicDehumNum ).HXName, FirstHVACIteration, DesicDehum( DesicDehumNum ).CompIndex, ContFanCycCoil, DDPartLoadRatio, true, CompanionCoilIndexNum, DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode );

			if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == DrawThru ) {
				SimulateFanComponents( DesicDehum( DesicDehumNum ).RegenFanName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenFanIndex );
			}

			// Calculate water removal
			DesicDehum( DesicDehumNum ).WaterRemoveRate = Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).MassFlowRate * ( Node( DesicDehum( DesicDehumNum ).ProcAirInNode ).HumRat - Node( DesicDehum( DesicDehumNum ).ProcAirOutNode ).HumRat );

			// If preheat is Yes, exhaust fan is condenser fan, if CoilUpstreamOfProcessSide is No, DD runs an its own PLR
			if ( DesicDehum( DesicDehumNum ).Preheat == Yes && DesicDehum( DesicDehumNum ).CoilUpstreamOfProcessSide == No ) {
				//    should actually use DX coil RTF instead of PLR since fan power is being calculated
				DesicDehum( DesicDehumNum ).ExhaustFanPower += max( 0.0, ( DesicDehum( DesicDehumNum ).ExhaustFanMaxPower * ( DXCoilPartLoadRatio( DesicDehum( DesicDehumNum ).DXCoilIndex ) - DDPartLoadRatio ) ) );
			}

		} else { // unit must be off

			DesicDehum( DesicDehumNum ).PartLoad = 0.0;

			if ( DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode ) {
				Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate = 0.0;
				Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRateMaxAvail = 0.0;
			}

			if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == BlowThru ) {
				SimulateFanComponents( DesicDehum( DesicDehumNum ).RegenFanName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenFanIndex );
			}

			if ( RegenCoilIndex > 0 ) {
				CalcNonDXHeatingCoils( DesicDehumNum, FirstHVACIteration, -1.0 );
			}

			SimHeatRecovery( DesicDehum( DesicDehumNum ).HXName, FirstHVACIteration, DesicDehum( DesicDehumNum ).CompIndex, ContFanCycCoil, 0.0, false, CompanionCoilIndexNum, DesicDehum( DesicDehumNum ).RegenInletIsOutsideAirNode );

			if ( DesicDehum( DesicDehumNum ).RegenFanPlacement == DrawThru ) {
				SimulateFanComponents( DesicDehum( DesicDehumNum ).RegenFanName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenFanIndex );
			}

			// Turn on exhaust fan if DX Coil is operating
			if ( DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate > 0 ) {
				if ( DesicDehum( DesicDehumNum ).DXCoilIndex > 0 ) {
					DDPartLoadRatio = DXCoilPartLoadRatio( DesicDehum( DesicDehumNum ).DXCoilIndex );
					DesicDehum( DesicDehumNum ).ExhaustFanPower = DesicDehum( DesicDehumNum ).ExhaustFanMaxPower * DDPartLoadRatio;
					ExhaustFanMassFlowRate = DesicDehum( DesicDehumNum ).ExhaustFanMaxMassFlowRate * DDPartLoadRatio;
				}
			}

		} // UnitOn/Off

		// check condenser minimum flow per rated total capacity
		if ( DDPartLoadRatio > 0.0 && DesicDehum( DesicDehumNum ).ExhaustFanMaxVolFlowRate > 0.0 ) {
			VolFlowPerRatedTotQ = ( Node( DesicDehum( DesicDehumNum ).RegenAirInNode ).MassFlowRate + ExhaustFanMassFlowRate ) / max( 0.00001, ( DesicDehum( DesicDehumNum ).CompanionCoilCapacity * DDPartLoadRatio * RhoAirStdInit ) );
			if ( ! WarmupFlag && ( VolFlowPerRatedTotQ < MinVolFlowPerRatedTotQ ) ) {
				gio::write( VolFlowChar, fmtLD ) << VolFlowPerRatedTotQ;
				++DesicDehum( DesicDehumNum ).ErrCount;
				if ( DesicDehum( DesicDehumNum ).ErrCount < 2 ) {
					ShowWarningError( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\" - Air volume flow rate per watt of total condenser waste heat is below the minimum recommended at " + VolFlowChar + " m3/s/W." );
					ShowContinueErrorTimeStamp( "" );
					gio::write( MinVol, fmtLD ) << MinVolFlowPerRatedTotQ;
					ShowContinueError( "Expected minimum for VolumeFlowperRatedTotalCondenserWasteHeat = [" + MinVol + ']' );
					ShowContinueError( "Possible causes include inconsistent air flow rates in system components " );
					ShowContinueError( "on the regeneration side of the desiccant dehumidifier." );
				} else {
					ShowRecurringWarningErrorAtEnd( DesicDehum( DesicDehumNum ).DehumType + " \"" + DesicDehum( DesicDehumNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range error continues...", DesicDehum( DesicDehumNum ).ErrIndex1, VolFlowPerRatedTotQ, VolFlowPerRatedTotQ );
				}
			} // flow per rated total capacity check ends
		}

		// Reset OnOffFanPartLoadFraction for process side fan calculations
		OnOffFanPartLoadFraction = OnOffFanPLF;

	}

	void
	UpdateDesiccantDehumidifier( int const DesicDehumNum ) // number of the current dehumidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
		//                      for Gas Research Institute
		//       DATE WRITTEN   March 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Moves dehumidifier output to the outlet nodes.

		// METHODOLOGY EMPLOYED:
		// NA

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
		int ProcInNode; // process air inlet node number
		int ProcOutNode; // process air outlet node number

		{ auto const SELECT_CASE_var( DesicDehum( DesicDehumNum ).DehumTypeCode );

		if ( SELECT_CASE_var == Solid ) {
			ProcInNode = DesicDehum( DesicDehumNum ).ProcAirInNode;
			ProcOutNode = DesicDehum( DesicDehumNum ).ProcAirOutNode;
			// Set the process outlet air node of the dehumidifier
			Node( ProcOutNode ).Temp = DesicDehum( DesicDehumNum ).ProcAirOutTemp;
			Node( ProcOutNode ).HumRat = DesicDehum( DesicDehumNum ).ProcAirOutHumRat;
			Node( ProcOutNode ).Enthalpy = DesicDehum( DesicDehumNum ).ProcAirOutEnthalpy;

			// Set the process outlet nodes for properties that just pass through & not used
			Node( ProcOutNode ).Quality = Node( ProcInNode ).Quality;
			Node( ProcOutNode ).Press = Node( ProcInNode ).Press;
			Node( ProcOutNode ).MassFlowRate = Node( ProcInNode ).MassFlowRate;
			Node( ProcOutNode ).MassFlowRateMin = Node( ProcInNode ).MassFlowRateMin;
			Node( ProcOutNode ).MassFlowRateMax = Node( ProcInNode ).MassFlowRateMax;
			Node( ProcOutNode ).MassFlowRateMinAvail = Node( ProcInNode ).MassFlowRateMinAvail;
			Node( ProcOutNode ).MassFlowRateMaxAvail = Node( ProcInNode ).MassFlowRateMaxAvail;

			//   RegenInNode =DesicDehum(DesicDehumNum)%RegenAirInNode
			//   RegenOutNode = DesicDehum(DesicDehumNum)%RegenAirOutNode
			// Set the regen outlet air node of the dehumidifier
			//   Node(RegenOutNode)%Temp         = DesicDehum(DesicDehumNum)%RegenAirOutTemp
			//   Node(RegenOutNode)%HumRat       = DesicDehum(DesicDehumNum)%RegenAirOutHumRat
			//   Node(RegenOutNode)%Enthalpy     = DesicDehum(DesicDehumNum)%RegenAirOutEnthalpy

			// Set the regen outlet nodes for properties that just pass through & not used
			//   Node(RegenOutNode)%Quality             = Node(RegenInNode)%Quality
			//   Node(RegenOutNode)%Press               = Node(RegenInNode)%Press
			//   Node(RegenOutNode)%MassFlowRate        = Node(RegenInNode)%MassFlowRate
			//   Node(RegenOutNode)%MassFlowRateMin     = Node(RegenInNode)%MassFlowRateMin
			//   Node(RegenOutNode)%MassFlowRateMax     = Node(RegenInNode)%MassFlowRateMax
			//   Node(RegenOutNode)%MassFlowRateMinAvail= Node(RegenInNode)%MassFlowRateMinAvail
			//   Node(RegenOutNode)%MassFlowRateMaxAvail= Node(RegenInNode)%MassFlowRateMaxAvail

		} else if ( SELECT_CASE_var == Generic ) {

			return;

		}}

	}

	void
	ReportDesiccantDehumidifier( int const DesicDehumNum ) // number of the current dehumidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
		//                      for Gas Research Institute
		//       DATE WRITTEN   March 2001
		//       MODIFIED       June 2007, R. Raustad, Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
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
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;

		{ auto const SELECT_CASE_var( DesicDehum( DesicDehumNum ).DehumTypeCode );

		if ( SELECT_CASE_var == Solid ) {
			DesicDehum( DesicDehumNum ).WaterRemove = DesicDehum( DesicDehumNum ).WaterRemoveRate * ReportingConstant;
			DesicDehum( DesicDehumNum ).RegenEnergy = DesicDehum( DesicDehumNum ).QRegen * ReportingConstant;
			DesicDehum( DesicDehumNum ).ElecUseEnergy = DesicDehum( DesicDehumNum ).ElecUseRate * ReportingConstant;
		} else if ( SELECT_CASE_var == Generic ) {
			DesicDehum( DesicDehumNum ).WaterRemove = DesicDehum( DesicDehumNum ).WaterRemoveRate * ReportingConstant;
			DesicDehum( DesicDehumNum ).ExhaustFanElecConsumption = DesicDehum( DesicDehumNum ).ExhaustFanPower * ReportingConstant;

		}}

	}

	void
	CalcNonDXHeatingCoils(
		int const DesicDehumNum, // Desiccant dehumidifier unit index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 const RegenCoilLoad, // heating coil load to be met (Watts)
		Optional< Real64 > RegenCoilLoadmet // heating load met
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

		// METHODOLOGY EMPLOYED:
		// Simply calls the different heating coil component.  The hot water flow rate matching the coil load
		// is calculated iteratively.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetComponentFlowRate;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrTolerance( 0.001 ); // convergence limit for hotwater coil
		int const SolveMaxIter( 50 ); // Max iteration for SolveRegulaFalsi

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RegenCoilActual; // actual heating load met
		Real64 mdot; // heating coil steam or hot water mass flow rate
		Real64 MinWaterFlow; // minimum hot water mass flow rate
		//unused  REAL(r64)      :: PartLoadFraction  ! heating or cooling part load fraction
		Real64 MaxHotWaterFlow; // maximum hot water mass flow rate, kg/s
		Real64 HotWaterMdot; // actual hot water mass flow rate
		Array1D< Real64 > Par( 3 );
		int SolFlag;

		RegenCoilActual = 0.0;
		if ( RegenCoilLoad > SmallLoad ) {
			{ auto const SELECT_CASE_var( DesicDehum( DesicDehumNum ).RegenCoilType_Num );
			if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
				SimulateHeatingCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, RegenCoilLoad, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilActual );
			} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
				MaxHotWaterFlow = DesicDehum( DesicDehumNum ).MaxCoilFluidFlow;
				SetComponentFlowRate( MaxHotWaterFlow, DesicDehum( DesicDehumNum ).CoilControlNode, DesicDehum( DesicDehumNum ).CoilOutletNode, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum );
				RegenCoilActual = RegenCoilLoad;
				// simulate the regenerator hot water heating coil
				SimulateWaterCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilActual );

				if ( RegenCoilActual > ( RegenCoilLoad + SmallLoad ) ) {
					// control water flow to obtain output matching RegenCoilLoad
					SolFlag = 0;
					MinWaterFlow = 0.0;
					Par( 1 ) = double( DesicDehumNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = RegenCoilLoad;
					SolveRegulaFalsi( ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par );
					if ( SolFlag == -1 ) {
						if ( DesicDehum( DesicDehumNum ).HotWaterCoilMaxIterIndex == 0 ) {
							ShowWarningMessage( "CalcNonDXHeatingCoils: Hot water coil control failed for " + DesicDehum( DesicDehumNum ).DehumType + "=\"" + DesicDehum( DesicDehumNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "...Iteration limit [" + RoundSigDigits( SolveMaxIter ) + "] exceeded in calculating hot water mass flow rate" );
						}
						ShowRecurringWarningErrorAtEnd( "CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit [" + RoundSigDigits( SolveMaxIter ) + "]) for " + DesicDehum( DesicDehumNum ).DehumType + "=\"" + DesicDehum( DesicDehumNum ).Name + "\"", DesicDehum( DesicDehumNum ).HotWaterCoilMaxIterIndex );
					} else if ( SolFlag == -2 ) {
						if ( DesicDehum( DesicDehumNum ).HotWaterCoilMaxIterIndex2 == 0 ) {
							ShowWarningMessage( "CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for " + DesicDehum( DesicDehumNum ).DehumType + "=\"" + DesicDehum( DesicDehumNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "...Bad hot water maximum flow rate limits" );
							ShowContinueError( "...Given minimum water flow rate=" + RoundSigDigits( MinWaterFlow, 3 ) + " kg/s" );
							ShowContinueError( "...Given maximum water flow rate=" + RoundSigDigits( MaxHotWaterFlow, 3 ) + " kg/s" );
						}
						ShowRecurringWarningErrorAtEnd( "CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for " + DesicDehum( DesicDehumNum ).DehumType + "=\"" + DesicDehum( DesicDehumNum ).Name + "\"", DesicDehum( DesicDehumNum ).HotWaterCoilMaxIterIndex2, MaxHotWaterFlow, MinWaterFlow, _, "[kg/s]", "[kg/s]" );
					}

					RegenCoilActual = RegenCoilLoad;
					// simulate the regenerator hot water heating coil
					SimulateWaterCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilActual );
				}
			} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
				mdot = DesicDehum( DesicDehumNum ).MaxCoilFluidFlow;
				SetComponentFlowRate( mdot, DesicDehum( DesicDehumNum ).CoilControlNode, DesicDehum( DesicDehumNum ).CoilOutletNode, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum );
				// simulate the regenerator steam heating coil
				SimulateSteamCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilLoad, RegenCoilActual );
			}}
		} else {
			{ auto const SELECT_CASE_var( DesicDehum( DesicDehumNum ).RegenCoilType_Num );
			if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
				SimulateHeatingCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, RegenCoilLoad, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilActual );
			} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, DesicDehum( DesicDehumNum ).CoilControlNode, DesicDehum( DesicDehumNum ).CoilOutletNode, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum );
				RegenCoilActual = RegenCoilLoad;
				// simulate the regenerator hot water heating coil
				SimulateWaterCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilActual );
			} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, DesicDehum( DesicDehumNum ).CoilControlNode, DesicDehum( DesicDehumNum ).CoilOutletNode, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum );
				// simulate the regenerator steam heating coil
				SimulateSteamCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACIteration, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilLoad, RegenCoilActual );
			}}
		}
		if ( present( RegenCoilLoadmet ) ) RegenCoilLoadmet = RegenCoilActual;

	}

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   January 2012
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (RegenCoilActual - RegenCoilHeatLoad) / RegenCoilHeatLoad
		// coil actual output depends on the hot water flow rate which is varied to minimize the residual

		// METHODOLOGY EMPLOYED:
		// Calls HotWaterCoilResidual, and calculates the residual as defined above.

		// REFERENCES:

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;
		using PlantUtilities::SetComponentFlowRate;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int DesicDehumNum;
		bool FirstHVACSoln;
		Real64 RegenCoilActual; // delivered coild load, W
		Real64 RegenCoilHeatLoad; // requested coild load, W
		Real64 mdot;

		DesicDehumNum = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		RegenCoilHeatLoad = Par( 3 );
		RegenCoilActual = RegenCoilHeatLoad;
		mdot = HWFlow;
		SetComponentFlowRate( mdot, DesicDehum( DesicDehumNum ).CoilControlNode, DesicDehum( DesicDehumNum ).CoilOutletNode, DesicDehum( DesicDehumNum ).LoopNum, DesicDehum( DesicDehumNum ).LoopSide, DesicDehum( DesicDehumNum ).BranchNum, DesicDehum( DesicDehumNum ).CompNum );

		// simulate the hot water regenerator heating coil
		SimulateWaterCoilComponents( DesicDehum( DesicDehumNum ).RegenCoilName, FirstHVACSoln, DesicDehum( DesicDehumNum ).RegenCoilIndex, RegenCoilActual );
		if ( RegenCoilHeatLoad != 0.0 ) {
			Residuum = ( RegenCoilActual - RegenCoilHeatLoad ) / RegenCoilHeatLoad;
		} else { //Autodesk:Return ELSE added to assure return value is set
			Residuum = 0.0;
		}
		return Residuum;
	}

	// Clears the global data in HeatingCoils.
	// Needed for unit tests, should not be normally called.
	void
	clear_state() {
		NumDesicDehums = 0;
		NumSolidDesicDehums = 0;
		NumGenericDesicDehums = 0;
		GetInputDesiccantDehumidifier = true;
		InitDesiccantDehumidifierOneTimeFlag = true;	
		DesicDehum.deallocate();
	}

	//        End of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	//                                 COPYRIGHT NOTICE

	//     Portions Copyright (c) Gas Research Institute 2001.  All rights reserved.

	//     GRI LEGAL NOTICE
	//     Neither GRI, members of GRI nor any person or organization acting on behalf
	//     of either:

	//     A. Makes any warranty of representation, express or implied with respect to
	//        the accuracy, completness, or usefulness of the information contained in
	//        in this program, including any warranty of merchantability or fitness of
	//        any purpose with respoect to the program, or that the use of any
	//        information disclosed in this program may not infringe privately-owned
	//        rights, or

	//     B.  Assumes any liability with respoct to the use of, or for any and all
	//         damages resulting from the use of the program or any portion thereof or
	//         any information disclosed therein.

} // DesiccantDehumidifiers

} // EnergyPlus
