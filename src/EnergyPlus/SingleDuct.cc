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
#include <SingleDuct.hh>
#include <BranchNodeConnections.hh>
#include <DataAirflowNetwork.hh>
#include <DataAirLoop.hh>
#include <DataContaminantBalance.hh>
#include <DataConvergParams.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace SingleDuct {
	// Module containing the Single Duct Systems as a single component/ or really a single driver

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   January 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// simulate single duct systems as a single driver or inter-connecting controllers.

	// METHODOLOGY EMPLOYED:
	// Needs description, as appropriate.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::NumOfZones;
	using DataGlobals::DisplayExtraWarnings;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::TurnFansOn;
	using DataHVACGlobals::SingleCoolingSetPoint;
	using DataHVACGlobals::SingleHeatingSetPoint;
	using DataHVACGlobals::SingleHeatCoolSetPoint;
	using DataHVACGlobals::DualSetPointWithDeadBand;
	using DataHVACGlobals::ATMixer_InletSide;
	using DataHVACGlobals::ATMixer_SupplySide;
	using DataHeatBalFanSys::TempControlType;
	using BranchNodeConnections::SetUpCompSets;
	using BranchNodeConnections::TestCompSet;
	using namespace DataSizing;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using namespace FluidProperties;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using namespace SteamCoils;

	// Data
	//MODULE PARAMETER DEFINITIONS
	int const Normal( 1 );
	int const ReverseAction( 2 );
	// SysTypes represented here
	int const SingleDuctVAVReheat( 3 );
	int const SingleDuctConstVolReheat( 4 );
	int const SingleDuctVAVNoReheat( 5 );
	int const SingleDuctVAVReheatVSFan( 6 );
	int const SingleDuctCBVAVReheat( 10 );
	int const SingleDuctCBVAVNoReheat( 11 );
	// Reheat Coil Types used here
	int const HCoilType_None( 0 );
	int const HCoilType_Gas( 1 );
	int const HCoilType_Electric( 2 );
	int const HCoilType_SimpleHeating( 3 );
	int const HCoilType_SteamAirHeating( 4 );
	// Fan types used here
	int const FanType_None( 0 );
	int const FanType_VS( 1 );
	// Minimum Flow Fraction Input Method
	int const ConstantMinFrac( 1 );
	int const ScheduledMinFrac( 2 );
	int const FixedMin( 3 );
	int NumATMixers( 0 );

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	Array1D< Real64 > MassFlow1; // previous value of the terminal unit mass flow rate
	Array1D< Real64 > MassFlow2; // previous value of the previous value of the mass flow rate
	Array1D< Real64 > MassFlow3;
	Array1D< Real64 > MassFlowDiff;
	bool GetInputFlag( true ); // Flag set to make sure you get input once
	bool GetATMixerFlag( true ); // Flag set to make sure you get input once
	int NumConstVolSys( 0 );
	Array1D_bool CheckEquipName;

	// INTERFACE BLOCK SPECIFICATIONS

	int NumSys( 0 ); // The Number of Systems found in the Input

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Object Data
	Array1D< SysDesignParams > Sys;
	Array1D< SysFlowConditions > SysInlet;
	Array1D< SysFlowConditions > SysOutlet;
	Array1D< AirTerminalMixerData > SysATMixer;

	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool InitSysFlag( true ); // Flag set to make sure you do begin simulation initializaztions once
	}

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	clear_state()
	{
		GetInputFlag = true;
		GetATMixerFlag = true;
		InitSysFlag = true;
	}

	void
	SimulateSingleDuct(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   January 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Sys system simulation.
		// It is called from the ManageZoneEquip
		// at the system time step.

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
		int SysNum; // The Sys that you are currently loading input into

		// FLOW:

		// Obtains and Allocates Sys related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetSysInput();
			GetInputFlag = false;
		}

		// Find the correct SysNumber with the Component Name
		if ( CompIndex == 0 ) {
			SysNum = FindItemInList( CompName, Sys, &SysDesignParams::SysName );
			if ( SysNum == 0 ) {
				ShowFatalError( "SimulateSingleDuct: System not found=" + CompName );
			}
			CompIndex = SysNum;
		} else {
			SysNum = CompIndex;
			if ( SysNum > NumSys || SysNum < 1 ) {
				ShowFatalError( "SimulateSingleDuct: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Number of Systems=" + TrimSigDigits( NumSys ) + ", System name=" + CompName );
			}
			if ( CheckEquipName( SysNum ) ) {
				if ( CompName != Sys( SysNum ).SysName ) {
					ShowFatalError( "SimulateSingleDuct: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", System name=" + CompName + ", stored System Name for that index=" + Sys( SysNum ).SysName );
				}
				CheckEquipName( SysNum ) = false;
			}
		}

		TermUnitSingDuct = true;

		// With the correct SysNum Initialize the system
		InitSys( SysNum, FirstHVACIteration ); // Initialize all Sys related parameters

		// Calculate the Correct Sys Model with the current SysNum
		{ auto const SELECT_CASE_var( Sys( SysNum ).SysType_Num );

		if ( SELECT_CASE_var == SingleDuctConstVolReheat ) { // AirTerminal:SingleDuct:ConstantVolume:Reheat
			SimConstVol( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );

		} else if ( SELECT_CASE_var == SingleDuctVAVReheat ) { // SINGLE DUCT:VAV:REHEAT
			SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );

		} else if ( SELECT_CASE_var == SingleDuctVAVNoReheat ) { // SINGLE DUCT:VAV:NOREHEAT
			SimVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );

		} else if ( SELECT_CASE_var == SingleDuctVAVReheatVSFan ) { // SINGLE DUCT:VAV:REHEAT:VS FAN
			SimVAVVS( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );

		} else if ( SELECT_CASE_var == SingleDuctCBVAVReheat ) { // SINGLE DUCT:VAVHEATANDCOOL:REHEAT
			SimCBVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );

		} else if ( SELECT_CASE_var == SingleDuctCBVAVNoReheat ) { // SINGLE DUCT:VAVHEATANDCOOL:NOREHEAT
			SimCBVAV( SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum );

		}}

		// Report the current Sys
		ReportSys( SysNum );

		TermUnitSingDuct = false;

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetSysInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   April 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main routine to call other input routines and Get routines

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilAirOutletNode;
		auto & GetHeatingCoilCapacity( HeatingCoils::GetCoilCapacity );
		auto & GetHeatingCoilOutletNode( HeatingCoils::GetCoilOutletNode );
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using namespace DataIPShortCuts;
		using namespace DataHeatBalance;
		using DataSizing::OARequirements;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataGlobals::DoZoneSizing;
		using DataGlobals::ScheduleAlwaysOn;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetSysInput: " ); // include trailing blank

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static int SysNum( 0 ); // The Sys that you are currently loading input into
		static int SysIndex( 0 ); // The Sys that you are currently loading input into
		static int NumVAVSys( 0 );
		static int NumNoRHVAVSys( 0 );
		static int NumVAVVS( 0 );
		static int NumCBVAVSys( 0 );
		static int NumNoRHCBVAVSys( 0 );
		static int NumAlphas( 0 );
		static int NumNums( 0 );
		int NumZoneSiz;
		int ZoneSizIndex;
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int CtrlZone; // controlled zone do loop index
		int SupAirIn; // controlled zone supply air inlet index
		int ADUNum; // air distribution unit index
		std::string CurrentModuleObject; // for ease in getting objects
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
		std::string AirTermSysInletNodeName; // air terminal single duct system inlet node name
		std::string AirTermSysOutletNodeName; // air terminal single duct system outlet node name

		// Flow
		NumVAVSys = GetNumObjectsFound( "AirTerminal:SingleDuct:VAV:Reheat" );
		NumNoRHVAVSys = GetNumObjectsFound( "AirTerminal:SingleDuct:VAV:NoReheat" );
		NumConstVolSys = GetNumObjectsFound( "AirTerminal:SingleDuct:ConstantVolume:Reheat" );
		NumVAVVS = GetNumObjectsFound( "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan" );
		NumCBVAVSys = GetNumObjectsFound( "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat" );
		NumNoRHCBVAVSys = GetNumObjectsFound( "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat" );
		NumSys = NumVAVSys + NumConstVolSys + NumNoRHVAVSys + NumVAVVS + NumCBVAVSys + NumNoRHCBVAVSys;

		Sys.allocate( NumSys );
		SysInlet.allocate( NumSys );
		SysOutlet.allocate( NumSys );
		CheckEquipName.dimension( NumSys, true );

		MassFlow1.allocate( NumSys );
		MassFlow2.allocate( NumSys );
		MassFlow3.allocate( NumSys );
		MassFlowDiff.allocate( NumSys );

		MassFlow1 = 0.0;
		MassFlow2 = 0.0;
		MassFlow3 = 0.0;
		MassFlowDiff = 0.0;

		GetObjectDefMaxArgs( "AirTerminal:SingleDuct:VAV:Reheat", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "AirTerminal:SingleDuct:VAV:NoReheat", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "AirTerminal:SingleDuct:ConstantVolume:Reheat", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		Numbers.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		//Start Loading the System Input
		for ( SysIndex = 1; SysIndex <= NumVAVSys; ++SysIndex ) {

			CurrentModuleObject = "AirTerminal:SingleDuct:VAV:Reheat";

			GetObjectItem( CurrentModuleObject, SysIndex, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			SysNum = SysIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Sys, &SysDesignParams::SysName, SysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Sys( SysNum ).SysName = Alphas( 1 );
			Sys( SysNum ).SysType = CurrentModuleObject;
			Sys( SysNum ).SysType_Num = SingleDuctVAVReheat;
			Sys( SysNum ).ReheatComp = Alphas( 7 );
			if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Gas" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Gas;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Electric" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Electric;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Water" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SimpleHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Steam" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SteamAirHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
			} else if ( Sys( SysNum ).ReheatComp != "" ) {
				ShowSevereError( "Illegal " + cAlphaFields( 8 ) + " = " + Sys( SysNum ).ReheatComp + '.' );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).ReheatName = Alphas( 8 );
			ValidateComponent( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Sys( SysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Sys( SysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( Sys( SysNum ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " = " + Alphas( 2 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				}
			}
			// For node connections, this object is both a parent and a non-parent, because the
			// VAV damper is not called out as a separate component, its nodes must be connected
			// as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
			Sys( SysNum ).OutletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 3 ) );
			Sys( SysNum ).InletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );
			Sys( SysNum ).MaxAirVolFlowRate = Numbers( 1 );

			if ( SameString( Alphas( 5 ), "Constant" ) ) {
				Sys( SysNum ).ZoneMinAirFracMethod = ConstantMinFrac;
			} else if ( SameString( Alphas( 5 ), "FixedFlowRate" ) ) {
				Sys( SysNum ).ZoneMinAirFracMethod = FixedMin;
			} else if ( SameString( Alphas( 5 ), "Scheduled" ) ) {
				Sys( SysNum ).ZoneMinAirFracMethod = ScheduledMinFrac;
			} else {
				ShowSevereError( cAlphaFields( 5 ) + " = " + Alphas( 5 ) + " not found." );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}

			if ( ! lNumericBlanks( 2 ) ) {
				Sys( SysNum ).ZoneMinAirFrac = Numbers( 2 );
				Sys( SysNum ).ConstantMinAirFracSetByUser = true;
				Sys( SysNum ).DesignMinAirFrac = Numbers( 2 );
			}

			if ( ! lNumericBlanks( 3 ) ) {
				Sys( SysNum ).ZoneFixedMinAir = Numbers( 3 );
				Sys( SysNum ).FixedMinAirSetByUser = true;
				Sys( SysNum ).DesignFixedMinAir = Numbers( 3 );
			}

			Sys( SysNum ).ZoneMinAirFracSchPtr = GetScheduleIndex( Alphas( 6 ) );
			if ( ( Sys( SysNum ).ZoneMinAirFracSchPtr == 0 ) && ( Sys( SysNum ).ZoneMinAirFracMethod == ScheduledMinFrac ) ) {
				ShowSevereError( cAlphaFields( 6 ) + " = " + Alphas( 6 ) + " not found." );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ShowContinueError( "A valid schedule is required" );
				ErrorsFound = true;
			} else if ( ( Sys( SysNum ).ZoneMinAirFracSchPtr > 0 ) && ( Sys( SysNum ).ZoneMinAirFracMethod == ScheduledMinFrac ) ) {
				// check range of values in schedule
				if ( ! CheckScheduleValueMinMax( Sys( SysNum ).ZoneMinAirFracSchPtr, ">=", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( "Error found in " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ShowContinueError( "Schedule values must be (>=0., <=1.)" );
				}

			}

			// The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
			// electric or gas reheat.
			if ( Sys( SysNum ).ReheatComp_Num != HCoilType_Gas && Sys( SysNum ).ReheatComp_Num != HCoilType_Electric ) {
				if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilSteamInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					}
				} else {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilWaterInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					}
				}
			}
			Sys( SysNum ).ReheatAirOutletNode = GetOnlySingleNode( Alphas( 9 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFields( 9 ) );
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
				Sys( SysNum ).MaxReheatSteamVolFlow = Numbers( 4 );
				Sys( SysNum ).MinReheatSteamVolFlow = Numbers( 5 );
			} else {
				Sys( SysNum ).MaxReheatWaterVolFlow = Numbers( 4 );
				Sys( SysNum ).MinReheatWaterVolFlow = Numbers( 5 );
			}
			Sys( SysNum ).ControllerOffset = Numbers( 6 );
			// Set default convergence tolerance
			if ( Sys( SysNum ).ControllerOffset <= 0.0 ) {
				Sys( SysNum ).ControllerOffset = 0.001;
			}
			if ( SameString( Alphas( 10 ), "Reverse" ) ) {
				Sys( SysNum ).DamperHeatingAction = ReverseAction;
			} else {
				Sys( SysNum ).DamperHeatingAction = Normal;
			}

			// Register component set data
			TestCompSet( Sys( SysNum ).SysType, Sys( SysNum ).SysName, NodeID( Sys( SysNum ).InletNodeNum ), NodeID( Sys( SysNum ).ReheatAirOutletNode ), "Air Nodes" );

			// Fill the Zone Equipment data with the inlet node number of this unit.
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( Sys( SysNum ).ReheatAirOutletNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
							ShowSevereError( "Error in connecting a terminal unit to a zone" );
							ShowContinueError( NodeID( Sys( SysNum ).ReheatAirOutletNode ) + " already connects to another zone" );
							ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ShowContinueError( "Check terminal unit node names for errors" );
							ErrorsFound = true;
						} else {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Sys( SysNum ).InletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Sys( SysNum ).ReheatAirOutletNode;
						}

						Sys( SysNum ).CtrlZoneNum = CtrlZone;
						Sys( SysNum ).ActualZoneNum = ZoneEquipConfig( CtrlZone ).ActualZoneNum;
						Sys( SysNum ).ZoneFloorArea = Zone( Sys( SysNum ).ActualZoneNum ).FloorArea * Zone( Sys( SysNum ).ActualZoneNum ).Multiplier * Zone( Sys( SysNum ).ActualZoneNum ).ListMultiplier;

					}
				}
			}

			if ( ! lNumericBlanks( 7 ) ) {
				if ( Numbers( 7 ) == AutoCalculate ) {
					Sys( SysNum ).MaxAirVolFlowRateDuringReheat = Numbers( 7 );
				} else {
					Sys( SysNum ).MaxAirVolFlowRateDuringReheat = Numbers( 7 ) * Sys( SysNum ).ZoneFloorArea;
				}
			}

			if ( ! lNumericBlanks( 8 ) ) {
				Sys( SysNum ).MaxAirVolFractionDuringReheat = Numbers( 8 );
			}

			// Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
			if ( ! lNumericBlanks( 9 ) ) {
				Sys( SysNum ).MaxReheatTemp = Numbers( 9 );
				Sys( SysNum ).MaxReheatTempSetByUser = true;
			} else {
				// user does not specify maximum supply air temperature
				// Sys(SysNum)%MaxReheatTemp = 35.0D0 !C
				Sys( SysNum ).MaxReheatTempSetByUser = false;
			}

			if ( ! lAlphaBlanks( 11 ) ) {
				Sys( SysNum ).OARequirementsPtr = FindItemInList( Alphas( 11 ), OARequirements );
				if ( Sys( SysNum ).OARequirementsPtr == 0 ) {
					ShowSevereError( cAlphaFields( 11 ) + " = " + Alphas( 11 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				} else {
					Sys( SysNum ).NoOAFlowInputFromUser = false;
				}
			}

			ValidateComponent( Alphas( 7 ), Alphas( 8 ), IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}

			// Add reheat coil to component sets array
			SetUpCompSets( Sys( SysNum ).SysType, Sys( SysNum ).SysName, Alphas( 7 ), Alphas( 8 ), Alphas( 3 ), Alphas( 9 ) );

			//Setup the Average damper Position output variable
			SetupOutputVariable( "Zone Air Terminal VAV Damper Position []", Sys( SysNum ).DamperPosition, "System", "Average", Sys( SysNum ).SysName );
			SetupOutputVariable( "Zone Air Terminal Minimum Air Flow Fraction []", Sys( SysNum ).ZoneMinAirFracReport, "System", "Average", Sys( SysNum ).SysName );
			SetupOutputVariable( "Zone Air Terminal Outdoor Air Volume Flow Rate [m3/s]", Sys( SysNum ).OutdoorAirFlowRate, "System", "Average", Sys( SysNum ).SysName );

		} // end Number of Sys Loop

		for ( SysIndex = 1; SysIndex <= NumCBVAVSys; ++SysIndex ) {

			CurrentModuleObject = "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat";

			GetObjectItem( CurrentModuleObject, SysIndex, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			SysNum = SysIndex + NumVAVSys;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Sys, &SysDesignParams::SysName, SysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Sys( SysNum ).SysName = Alphas( 1 );
			Sys( SysNum ).SysType = CurrentModuleObject;
			Sys( SysNum ).SysType_Num = SingleDuctCBVAVReheat;
			Sys( SysNum ).ReheatComp = Alphas( 5 );
			if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Gas" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Gas;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Electric" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Electric;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Water" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SimpleHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Steam" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SteamAirHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
			} else if ( Sys( SysNum ).ReheatComp != "" ) {
				ShowSevereError( "Illegal " + cAlphaFields( 5 ) + " = " + Sys( SysNum ).ReheatComp + '.' );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).ReheatName = Alphas( 6 );
			ValidateComponent( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Sys( SysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Sys( SysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( Sys( SysNum ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " = " + Alphas( 2 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				}
			}
			// For node connections, this object is both a parent and a non-parent, because the
			// VAV damper is not called out as a separate component, its nodes must be connected
			// as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
			Sys( SysNum ).OutletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 3 ) );
			Sys( SysNum ).InletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );
			Sys( SysNum ).MaxAirVolFlowRate = Numbers( 1 );
			Sys( SysNum ).ZoneMinAirFrac = Numbers( 2 );
			if ( Sys( SysNum ).ZoneMinAirFrac < 0.0 ) {
				ShowWarningError( Sys( SysNum ).SysType + " \"" + Sys( SysNum ).SysName + "\"" );
				ShowContinueError( cNumericFields( 2 ) + " must be greater than or equal to 0. Resetting to 0 and the simulation continues." );
				Sys( SysNum ).ZoneMinAirFrac = 0.0;
			}
			if ( Sys( SysNum ).ZoneMinAirFrac > 1.0 ) {
				ShowWarningError( Sys( SysNum ).SysType + " \"" + Sys( SysNum ).SysName + "\"" );
				ShowContinueError( cNumericFields( 2 ) + " must be less than or equal to 1. Resetting to 1 and the simulation continues." );
				Sys( SysNum ).ZoneMinAirFrac = 1.0;
			}
			// The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
			// electric or gas reheat.
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_Gas || Sys( SysNum ).ReheatComp_Num == HCoilType_Electric ) {
				//          IF(.NOT. lAlphaBlanks(5)) THEN
				//            CALL ShowWarningError('In '//TRIM(Sys(SysNum)%SysType)//' = ' //TRIM(Sys(SysNum)%SysName) &
				//                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
				//            CALL ShowContinueError('  It is used for hot water and steam reheat coils only.')
				//          END IF
			} else {
				//          IF(lAlphaBlanks(5)) THEN
				//            CALL ShowSevereError('In '//TRIM(Sys(SysNum)%SysType)//' = ' //TRIM(Sys(SysNum)%SysName) &
				//                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
				//            ErrorsFound=.TRUE.
				//          ELSE
				if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilSteamInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					}
					//                GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
					//                              NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
				} else {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilWaterInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					}
					//                GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
					//                              NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
				}
				//  END IF
			}
			Sys( SysNum ).ReheatAirOutletNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFields( 7 ) );
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
				Sys( SysNum ).MaxReheatSteamVolFlow = Numbers( 3 );
				Sys( SysNum ).MinReheatSteamVolFlow = Numbers( 4 );
			} else {
				Sys( SysNum ).MaxReheatWaterVolFlow = Numbers( 3 );
				Sys( SysNum ).MinReheatWaterVolFlow = Numbers( 4 );
			}
			Sys( SysNum ).ControllerOffset = Numbers( 5 );
			// Set default convergence tolerance
			if ( Sys( SysNum ).ControllerOffset <= 0.0 ) {
				Sys( SysNum ).ControllerOffset = 0.001;
			}

			Sys( SysNum ).DamperHeatingAction = ReverseAction;

			// Register component set data
			TestCompSet( Sys( SysNum ).SysType, Sys( SysNum ).SysName, NodeID( Sys( SysNum ).InletNodeNum ), NodeID( Sys( SysNum ).ReheatAirOutletNode ), "Air Nodes" );

			// Fill the Zone Equipment data with the inlet node number of this unit
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( Sys( SysNum ).ReheatAirOutletNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
							ShowSevereError( "Error in connecting a terminal unit to a zone" );
							ShowContinueError( NodeID( Sys( SysNum ).ReheatAirOutletNode ) + " already connects to another zone" );
							ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ShowContinueError( "Check terminal unit node names for errors" );
							ErrorsFound = true;
						} else {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Sys( SysNum ).InletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Sys( SysNum ).ReheatAirOutletNode;
						}
					}
				}
			}

			if ( ! lNumericBlanks( 6 ) ) {
				Sys( SysNum ).MaxReheatTemp = Numbers( 6 );
				Sys( SysNum ).MaxReheatTempSetByUser = true;
			} else {
				// user does not specify maximum supply air temperature
				// Sys(SysNum)%MaxReheatTemp = 35.0D0 !C
				Sys( SysNum ).MaxReheatTempSetByUser = false;
			}

			ValidateComponent( Alphas( 5 ), Alphas( 6 ), IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}

			// Add reheat coil to component sets array
			SetUpCompSets( Sys( SysNum ).SysType, Sys( SysNum ).SysName, Alphas( 5 ), Alphas( 6 ), Alphas( 3 ), Alphas( 7 ) );

			//Setup the Average damper Position output variable
			SetupOutputVariable( "Zone Air Terminal VAV Damper Position []", Sys( SysNum ).DamperPosition, "System", "Average", Sys( SysNum ).SysName );

		} // end Number of VAVHeatandCool Sys Loop

		CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:Reheat";

		for ( SysIndex = 1; SysIndex <= NumConstVolSys; ++SysIndex ) {

			GetObjectItem( CurrentModuleObject, SysIndex, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			SysNum = SysIndex + NumVAVSys + NumCBVAVSys;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Sys, &SysDesignParams::SysName, SysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Sys( SysNum ).SysName = Alphas( 1 );
			Sys( SysNum ).SysType = CurrentModuleObject;
			Sys( SysNum ).SysType_Num = SingleDuctConstVolReheat;
			Sys( SysNum ).ReheatComp = Alphas( 5 );
			if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Gas" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Gas;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Electric" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Electric;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Water" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SimpleHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Steam" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SteamAirHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
			} else {
				ShowSevereError( "Illegal " + cAlphaFields( 5 ) + " = " + Sys( SysNum ).ReheatComp + '.' );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).ReheatName = Alphas( 6 );
			ValidateComponent( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Sys( SysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Sys( SysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( Sys( SysNum ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " = " + Alphas( 2 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				}
			}
			Sys( SysNum ).OutletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFields( 3 ) );
			Sys( SysNum ).InletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFields( 4 ) );
			// The reheat coil control node is necessary for hot water reheat, but not necessary for
			// electric or gas reheat.
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_Gas || Sys( SysNum ).ReheatComp_Num == HCoilType_Electric ) {
				//          IF(.NOT. lAlphaBlanks(5)) THEN
				//            CALL ShowWarningError('In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
				//                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
				//            CALL ShowContinueError('  It is used for hot water reheat coils only.')
				//          END IF
			} else {
				//          IF(lAlphaBlanks(5)) THEN
				//            CALL ShowSevereError('In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
				//                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
				//            ErrorsFound=.TRUE.
				//          END IF
				if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilSteamInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					}
					//                 GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
					//                               NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
				} else {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilWaterInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					}
					//                 GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
					//                               NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
				}
			}
			Sys( SysNum ).ReheatAirOutletNode = Sys( SysNum ).OutletNodeNum;
			Sys( SysNum ).MaxAirVolFlowRate = Numbers( 1 );
			Sys( SysNum ).ZoneMinAirFrac = 0.0;
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
				Sys( SysNum ).MaxReheatSteamVolFlow = Numbers( 2 );
				Sys( SysNum ).MinReheatSteamVolFlow = Numbers( 3 );
			} else {
				Sys( SysNum ).MaxReheatWaterVolFlow = Numbers( 2 );
				Sys( SysNum ).MinReheatWaterVolFlow = Numbers( 3 );
			}
			Sys( SysNum ).ControllerOffset = Numbers( 4 );
			// Set default convergence tolerance
			if ( Sys( SysNum ).ControllerOffset <= 0.0 ) {
				Sys( SysNum ).ControllerOffset = 0.001;
			}

			// Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
			if ( ! lNumericBlanks( 5 ) ) {
				Sys( SysNum ).MaxReheatTemp = Numbers( 5 );
				Sys( SysNum ).MaxReheatTempSetByUser = true;
			} else {
				// user does not specify maximum supply air temperature
				// Sys(SysNum)%MaxReheatTemp = 35.0D0 !C
				Sys( SysNum ).MaxReheatTempSetByUser = false;
			}
			// Register component set data
			TestCompSet( Sys( SysNum ).SysType, Sys( SysNum ).SysName, NodeID( Sys( SysNum ).InletNodeNum ), NodeID( Sys( SysNum ).OutletNodeNum ), "Air Nodes" );

			// Fill the Zone Equipment data with the inlet node number of this unit.
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( Sys( SysNum ).OutletNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
							ShowSevereError( "Error in connecting a terminal unit to a zone" );
							ShowContinueError( NodeID( Sys( SysNum ).OutletNodeNum ) + " already connects to another zone" );
							ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ShowContinueError( "Check terminal unit node names for errors" );
							ErrorsFound = true;
						} else {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Sys( SysNum ).InletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Sys( SysNum ).OutletNodeNum;
						}
					}
				}
			}

			ValidateComponent( Alphas( 5 ), Alphas( 6 ), IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}

			// Add reheat coil to component sets array
			SetUpCompSets( Sys( SysNum ).SysType, Sys( SysNum ).SysName, Alphas( 5 ), Alphas( 6 ), Alphas( 4 ), Alphas( 3 ) );

			//Setup the Average damper Position output variable
			// BG removed 9-10-2009 during work on CR 7770, constant volume has no damper
			//  CALL SetupOutputVariable('Damper Position', Sys(SysNum)%DamperPosition, &
			//                        'System','Average',Sys(SysNum)%SysName)

		} // End Number of Sys Loop

		for ( SysIndex = 1; SysIndex <= NumNoRHVAVSys; ++SysIndex ) {

			CurrentModuleObject = "AirTerminal:SingleDuct:VAV:NoReheat";

			GetObjectItem( CurrentModuleObject, SysIndex, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Sys, &SysDesignParams::SysName, SysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Sys( SysNum ).SysName = Alphas( 1 );
			Sys( SysNum ).SysType = CurrentModuleObject;
			Sys( SysNum ).SysType_Num = SingleDuctVAVNoReheat;
			Sys( SysNum ).ReheatComp = "";
			Sys( SysNum ).ReheatName = "";
			Sys( SysNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Sys( SysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Sys( SysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( Sys( SysNum ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " = " + Alphas( 2 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				}
			}
			Sys( SysNum ).OutletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 3 ) );
			Sys( SysNum ).InletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );
			Sys( SysNum ).MaxAirVolFlowRate = Numbers( 1 );

			if ( SameString( Alphas( 5 ), "Constant" ) ) {
				Sys( SysNum ).ZoneMinAirFracMethod = ConstantMinFrac;
			} else if ( SameString( Alphas( 5 ), "FixedFlowRate" ) ) {
				Sys( SysNum ).ZoneMinAirFracMethod = FixedMin;
			} else if ( SameString( Alphas( 5 ), "Scheduled" ) ) {
				Sys( SysNum ).ZoneMinAirFracMethod = ScheduledMinFrac;
			} else {
				ShowSevereError( cAlphaFields( 5 ) + " = " + Alphas( 5 ) + " not found." );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}

			if ( ! lNumericBlanks( 2 ) ) {
				Sys( SysNum ).ZoneMinAirFrac = Numbers( 2 );
				Sys( SysNum ).ConstantMinAirFracSetByUser = true;
				Sys( SysNum ).DesignMinAirFrac = Numbers( 2 );
			}

			if ( ! lNumericBlanks( 3 ) ) {
				Sys( SysNum ).ZoneFixedMinAir = Numbers( 3 );
				Sys( SysNum ).FixedMinAirSetByUser = true;
				Sys( SysNum ).DesignFixedMinAir = Numbers( 3 );
			}

			Sys( SysNum ).ZoneMinAirFracSchPtr = GetScheduleIndex( Alphas( 6 ) );
			if ( ( Sys( SysNum ).ZoneMinAirFracSchPtr == 0 ) && ( Sys( SysNum ).ZoneMinAirFracMethod == ScheduledMinFrac ) ) {
				ShowSevereError( cAlphaFields( 6 ) + " = " + Alphas( 6 ) + " not found." );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ShowContinueError( "A valid schedule is required" );
				ErrorsFound = true;
			} else if ( ( Sys( SysNum ).ZoneMinAirFracSchPtr > 0 ) && ( Sys( SysNum ).ZoneMinAirFracMethod == ScheduledMinFrac ) ) {
				// check range of values in schedule
				if ( ! CheckScheduleValueMinMax( Sys( SysNum ).ZoneMinAirFracSchPtr, ">=", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( "Error found in " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ShowContinueError( "Schedule values must be (>=0., <=1.)" );
				}

			}

			Sys( SysNum ).ReheatControlNode = 0;
			Sys( SysNum ).ReheatAirOutletNode = Sys( SysNum ).OutletNodeNum;
			Sys( SysNum ).MaxReheatWaterVolFlow = 0.0;
			Sys( SysNum ).MaxReheatSteamVolFlow = 0.0;
			Sys( SysNum ).MinReheatWaterVolFlow = 0.0;
			Sys( SysNum ).MinReheatSteamVolFlow = 0.0;
			Sys( SysNum ).ControllerOffset = 0.000001;
			Sys( SysNum ).DamperHeatingAction = Normal;

			// Register component set data
			TestCompSet( Sys( SysNum ).SysType, Sys( SysNum ).SysName, NodeID( Sys( SysNum ).InletNodeNum ), NodeID( Sys( SysNum ).OutletNodeNum ), "Air Nodes" );

			// Fill the Zone Equipment data with the inlet node number of this unit.
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( Sys( SysNum ).ReheatAirOutletNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
							ShowSevereError( "Error in connecting a terminal unit to a zone" );
							ShowContinueError( NodeID( Sys( SysNum ).ReheatAirOutletNode ) + " already connects to another zone" );
							ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ShowContinueError( "Check terminal unit node names for errors" );
							ErrorsFound = true;
						} else {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Sys( SysNum ).InletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Sys( SysNum ).ReheatAirOutletNode;
						}

						Sys( SysNum ).CtrlZoneNum = CtrlZone;
						Sys( SysNum ).ActualZoneNum = ZoneEquipConfig( CtrlZone ).ActualZoneNum;
						Sys( SysNum ).ZoneFloorArea = Zone( Sys( SysNum ).ActualZoneNum ).FloorArea * Zone( Sys( SysNum ).ActualZoneNum ).Multiplier * Zone( Sys( SysNum ).ActualZoneNum ).ListMultiplier;

					}
				}
			}

			if ( ! lAlphaBlanks( 7 ) ) {
				Sys( SysNum ).OARequirementsPtr = FindItemInList( Alphas( 7 ), OARequirements );
				if ( Sys( SysNum ).OARequirementsPtr == 0 ) {
					ShowSevereError( cAlphaFields( 7 ) + " = " + Alphas( 7 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				} else {
					Sys( SysNum ).NoOAFlowInputFromUser = false;
				}
			}

			//Setup the Average damper Position output variable
			SetupOutputVariable( "Zone Air Terminal VAV Damper Position []", Sys( SysNum ).DamperPosition, "System", "Average", Sys( SysNum ).SysName );
			SetupOutputVariable( "Zone Air Terminal Minimum Air Flow Fraction []", Sys( SysNum ).ZoneMinAirFracReport, "System", "Average", Sys( SysNum ).SysName );
			SetupOutputVariable( "Zone Air Terminal Outdoor Air Volume Flow Rate [m3/s]", Sys( SysNum ).OutdoorAirFlowRate, "System", "Average", Sys( SysNum ).SysName );

		} // end Number of Sys Loop

		for ( SysIndex = 1; SysIndex <= NumNoRHCBVAVSys; ++SysIndex ) {

			CurrentModuleObject = "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat";

			GetObjectItem( CurrentModuleObject, SysIndex, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys + NumNoRHVAVSys;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Sys, &SysDesignParams::SysName, SysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Sys( SysNum ).SysName = Alphas( 1 );
			Sys( SysNum ).SysType = CurrentModuleObject;
			Sys( SysNum ).SysType_Num = SingleDuctCBVAVNoReheat;
			Sys( SysNum ).ReheatComp = "";
			Sys( SysNum ).ReheatName = "";
			Sys( SysNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Sys( SysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Sys( SysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( Sys( SysNum ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " = " + Alphas( 2 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				}
			}
			Sys( SysNum ).OutletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 3 ) );
			Sys( SysNum ).InletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, Sys( SysNum ).SysType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );
			Sys( SysNum ).MaxAirVolFlowRate = Numbers( 1 );
			Sys( SysNum ).ZoneMinAirFrac = Numbers( 2 );
			if ( Sys( SysNum ).ZoneMinAirFrac < 0.0 ) {
				ShowWarningError( Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName );
				ShowContinueError( cNumericFields( 2 ) + " must be greater than or equal to 0. Resetting to 0 and the simulation continues." );
				Sys( SysNum ).ZoneMinAirFrac = 0.0;
			}
			if ( Sys( SysNum ).ZoneMinAirFrac > 1.0 ) {
				ShowWarningError( Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName );
				ShowContinueError( cNumericFields( 2 ) + " must be less than or equal to 1. Resetting to 1 and the simulation continues." );
				Sys( SysNum ).ZoneMinAirFrac = 1.0;
			}

			Sys( SysNum ).ReheatControlNode = 0;
			Sys( SysNum ).ReheatAirOutletNode = Sys( SysNum ).OutletNodeNum;
			Sys( SysNum ).MaxReheatWaterVolFlow = 0.0;
			Sys( SysNum ).MaxReheatSteamVolFlow = 0.0;
			Sys( SysNum ).MinReheatWaterVolFlow = 0.0;
			Sys( SysNum ).MinReheatSteamVolFlow = 0.0;
			Sys( SysNum ).ControllerOffset = 0.000001;
			Sys( SysNum ).DamperHeatingAction = ReverseAction;

			// Register component set data
			TestCompSet( Sys( SysNum ).SysType, Sys( SysNum ).SysName, NodeID( Sys( SysNum ).InletNodeNum ), NodeID( Sys( SysNum ).OutletNodeNum ), "Air Nodes" );

			// Fill the Zone Equipment data with the inlet node number of this unit.
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( Sys( SysNum ).ReheatAirOutletNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
							ShowSevereError( "Error in connecting a terminal unit to a zone" );
							ShowContinueError( NodeID( Sys( SysNum ).ReheatAirOutletNode ) + " already connects to another zone" );
							ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ShowContinueError( "Check terminal unit node names for errors" );
							ErrorsFound = true;
						} else {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Sys( SysNum ).InletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Sys( SysNum ).ReheatAirOutletNode;
						}
					}
				}
			}

			//Setup the Average damper Position output variable
			SetupOutputVariable( "Zone Air Terminal VAV Damper Position []", Sys( SysNum ).DamperPosition, "System", "Average", Sys( SysNum ).SysName );

		} // end Number of VAVHeatandCool:NoReheat Sys Loop

		// read in the SINGLE DUCT:VAV:REHEAT:VS FAN data
		for ( SysIndex = 1; SysIndex <= NumVAVVS; ++SysIndex ) {

			CurrentModuleObject = "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan";

			GetObjectItem( CurrentModuleObject, SysIndex, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys + NumNoRHVAVSys + NumNoRHCBVAVSys;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Sys, &SysDesignParams::SysName, SysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Sys( SysNum ).SysName = Alphas( 1 );
			Sys( SysNum ).SysType = CurrentModuleObject;
			Sys( SysNum ).SysType_Num = SingleDuctVAVReheatVSFan;
			Sys( SysNum ).ReheatComp = Alphas( 7 );
			Sys( SysNum ).ReheatName = Alphas( 8 );
			IsNotOK = false;
			if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Gas" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Gas;
				Sys( SysNum ).ReheatAirOutletNode = GetHeatingCoilOutletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
				Sys( SysNum ).ReheatCoilMaxCapacity = GetHeatingCoilCapacity( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
				if ( IsNotOK ) ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Electric" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_Electric;
				Sys( SysNum ).ReheatAirOutletNode = GetHeatingCoilOutletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
				Sys( SysNum ).ReheatCoilMaxCapacity = GetHeatingCoilCapacity( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
				if ( IsNotOK ) ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Water" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SimpleHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
			} else if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Steam" ) ) {
				Sys( SysNum ).ReheatComp_Num = HCoilType_SteamAirHeating;
				Sys( SysNum ).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
			} else if ( Sys( SysNum ).ReheatComp != "" ) {
				ShowSevereError( "Illegal " + cAlphaFields( 7 ) + " = " + Sys( SysNum ).ReheatComp + '.' );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			ValidateComponent( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).FanType = Alphas( 5 );
			if ( SameString( Sys( SysNum ).FanType, "Fan:VariableVolume" ) ) {
				Sys( SysNum ).Fan_Num = FanType_VS;
			} else if ( Sys( SysNum ).FanType != "" ) {
				ShowSevereError( "Illegal " + cAlphaFields( 5 ) + " = " + Sys( SysNum ).FanType + '.' );
				ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			Sys( SysNum ).FanName = Alphas( 6 );
			ValidateComponent( Sys( SysNum ).FanType, Sys( SysNum ).FanName, IsNotOK, Sys( SysNum ).SysType );
			if ( IsNotOK ) {
				ShowContinueError( "In " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}

			Sys( SysNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Sys( SysNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Sys( SysNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( Sys( SysNum ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " = " + Alphas( 2 ) + " not found." );
					ShowContinueError( "Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
					ErrorsFound = true;
				}
			}

			//  A5,     \field heating coil air inlet node
			//          \note same as fan outlet node
			//          \type alpha
			//          \required-field
			IsNotOK = false;

			Sys( SysNum ).OutletNodeNum = GetFanOutletNode( Sys( SysNum ).FanType, Sys( SysNum ).FanName, IsNotOK );
			if ( IsNotOK ) {
				ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			//               GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
			//                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
			//  A3,     \field Unit supply air inlet node
			//          \note same as fan inlet node
			//          \type alpha
			IsNotOK = false;
			Sys( SysNum ).InletNodeNum = GetFanInletNode( Sys( SysNum ).FanType, Sys( SysNum ).FanName, IsNotOK );
			if ( IsNotOK ) {
				ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ErrorsFound = true;
			}
			//               GetOnlySingleNode(Alphas(3),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
			//                           NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
			AirTermSysInletNodeName = NodeID( Sys( SysNum ).InletNodeNum );
			if ( ! SameString( Alphas( 3 ), AirTermSysInletNodeName ) ) {
				ShowWarningError( RoutineName + "Invalid air terminal object air inlet node name in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ShowContinueError( " Specified air inlet node name is = " + Alphas( 3 ) + "." );
				ShowContinueError( " Expected air inlet node name is = " + AirTermSysInletNodeName + "." );
				//ErrorsFound = true;
			}

			Sys( SysNum ).MaxAirVolFlowRate = Numbers( 1 );
			Sys( SysNum ).MaxHeatAirVolFlowRate = Numbers( 2 );
			Sys( SysNum ).ZoneMinAirFrac = Numbers( 3 );
			// The reheat coil control node is necessary for hot water reheat, but not necessary for
			// electric or gas reheat.
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_Gas || Sys( SysNum ).ReheatComp_Num == HCoilType_Electric ) {
				//          IF(.NOT. lAlphaBlanks(6)) THEN
				//            CALL ShowWarningError('In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
				//                                 // ' the '//TRIM(cAlphaFields(6))//' is not needed and will be ignored.')
				//            CALL ShowContinueError('  It is used for hot water reheat coils only.')
				//          END IF
			} else {
				//          IF(lAlphaBlanks(6)) THEN
				//            CALL ShowSevereError('In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
				//                                 // ' the '//TRIM(cAlphaFields(6))//' is undefined')
				//            ErrorsFound=.TRUE.
				//          END IF
				if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilSteamInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					} else {
						//  A4,     \field Unit supply air outlet node
						//          \note same as heating coil air outlet node
						//          \note same as zone inlet node
						//          \type alpha
						IsNotOK = false;
						Sys( SysNum ).ReheatAirOutletNode = GetCoilAirOutletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
						if ( IsNotOK ) {
							ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ErrorsFound = true;
						}
					}
					//               GetOnlySingleNode(Alphas(6),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
					//                                NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
				} else {
					IsNotOK = false;
					Sys( SysNum ).ReheatControlNode = GetCoilWaterInletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
						ErrorsFound = true;
					} else {
						//  A4,     \field Unit supply air outlet node
						//          \note same as heating coil air outlet node
						//          \note same as zone inlet node
						//          \type alpha
						IsNotOK = false;
						Sys( SysNum ).ReheatAirOutletNode = GetCoilOutletNode( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, IsNotOK );
						if ( IsNotOK ) {
							ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ErrorsFound = true;
						}
					}
					//               GetOnlySingleNode(Alphas(6),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
					//                                NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
				}
			}
			//  A4,     \field Unit supply air outlet node
			//          \note same as heating coil air outlet node
			//          \note same as zone inlet node
			//          \type alpha
			//        Sys(SysNum)%ReheatAirOutletNode  = &
			//               GetOnlySingleNode(Alphas(4),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
			//                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
			AirTermSysOutletNodeName = NodeID( Sys( SysNum ).ReheatAirOutletNode );
			if ( ! SameString( Alphas( 4 ), AirTermSysOutletNodeName ) ) {
				ShowWarningError( RoutineName + "Invalid air terminal object air outlet node name in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				ShowContinueError( " Specified air outlet node name is = " + Alphas( 4 ) + "." );
				ShowContinueError( " Expected air outlet node name is = " + AirTermSysOutletNodeName + "." );
				//ErrorsFound = true;
			}

			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
				Sys( SysNum ).MaxReheatSteamVolFlow = Numbers( 4 );
				Sys( SysNum ).MinReheatSteamVolFlow = Numbers( 5 );
			} else {
				Sys( SysNum ).MaxReheatWaterVolFlow = Numbers( 4 );
				Sys( SysNum ).MinReheatWaterVolFlow = Numbers( 5 );
			}
			Sys( SysNum ).ControllerOffset = Numbers( 6 );
			// Set default convergence tolerance
			if ( Sys( SysNum ).ControllerOffset <= 0.0 ) {
				Sys( SysNum ).ControllerOffset = 0.001;
			}
			Sys( SysNum ).DamperHeatingAction = ReverseAction;

			// Register component set data
			TestCompSet( Sys( SysNum ).SysType, Sys( SysNum ).SysName, NodeID( Sys( SysNum ).InletNodeNum ), NodeID( Sys( SysNum ).ReheatAirOutletNode ), "Air Nodes" );

			// Fill the Zone Equipment data with the inlet node number of this unit.
			// what if not found?  error?
			IsNotOK = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( Sys( SysNum ).ReheatAirOutletNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						IsNotOK = false;
						if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
							ShowSevereError( "Error in connecting a terminal unit to a zone" );
							ShowContinueError( NodeID( Sys( SysNum ).ReheatAirOutletNode ) + " already connects to another zone" );
							ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
							ShowContinueError( "Check terminal unit node names for errors" );
							ErrorsFound = true;
						} else {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Sys( SysNum ).InletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Sys( SysNum ).ReheatAirOutletNode;
						}
					}
				}
			}
			if ( IsNotOK ) {
				ShowWarningError( "Did not Match Supply Air Outlet Node to any Zone Node" );
				ShowContinueError( "..Occurs in " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
			}

			// Add reheat coil to component sets array
			SetUpCompSets( Sys( SysNum ).SysType, Sys( SysNum ).SysName, Alphas( 7 ), Alphas( 8 ), NodeID( Sys( SysNum ).OutletNodeNum ), NodeID( Sys( SysNum ).ReheatAirOutletNode ) );
			// Add fan to component sets array
			SetUpCompSets( Sys( SysNum ).SysType, Sys( SysNum ).SysName, Alphas( 5 ), Alphas( 6 ), NodeID( Sys( SysNum ).InletNodeNum ), NodeID( Sys( SysNum ).OutletNodeNum ) );

			//Setup the Average damper Position output variable
			SetupOutputVariable( "Zone Air Terminal VAV Damper Position []", Sys( SysNum ).DamperPosition, "System", "Average", Sys( SysNum ).SysName );

		}

		for ( SysIndex = 1; SysIndex <= NumSys; ++SysIndex ) {
			for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
				if ( Sys( SysIndex ).ReheatAirOutletNode == AirDistUnit( ADUNum ).OutletNodeNum ) {
					AirDistUnit( ADUNum ).InletNodeNum = Sys( SysIndex ).InletNodeNum;
					Sys( SysIndex ).ADUNum = ADUNum;
				}
			}
			// one assumes if there isn't one assigned, it's an error?
			if ( Sys( SysIndex ).ADUNum == 0 ) {
				ShowSevereError( RoutineName + "No matching Air Distribution Unit, for System = [" + Sys( SysIndex ).SysType + ',' + Sys( SysIndex ).SysName + "]." );
				ShowContinueError( "...should have outlet node = " + NodeID( Sys( SysIndex ).ReheatAirOutletNode ) );
				//          ErrorsFound=.TRUE.
			}
		}

		// Error check to see if a single duct air terminal is assigned to zone that has zone secondary recirculation
		// specified in the Sizing:Zone object

		NumZoneSiz = GetNumObjectsFound( "Sizing:Zone" );
		if ( NumZoneSiz > 0 ) {
			for ( SysIndex = 1; SysIndex <= NumSys; ++SysIndex ) {
				for ( ZoneSizIndex = 1; ZoneSizIndex <= NumZoneSiz; ++ZoneSizIndex ) {
					if ( DoZoneSizing ) {
						if ( FinalZoneSizing( ZoneSizIndex ).ActualZoneNum == Sys( SysIndex ).ActualZoneNum ) {
							if ( FinalZoneSizing( ZoneSizIndex ).ZoneSecondaryRecirculation > 0.0 ) {
								ShowWarningError( RoutineName + "A zone secondary recirculation fraction is specified for zone served by " );
								ShowContinueError( "...terminal unit \"" + Sys( SysIndex ).SysName + "\" , that indicates a single path system" );
								ShowContinueError( "...The zone secondary recirculation for that zone was set to 0.0" );
								FinalZoneSizing( ZoneSizIndex ).ZoneSecondaryRecirculation = 0.0;
								goto SizLoop_exit;
							}
						}
					}
				}
				SizLoop_exit: ;
			}
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Preceding condition(s) cause termination." );
		}

	}

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitSys(
		int const SysNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   January 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for  initializations of the Sys Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkFanActivated;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;
		using DataDefineEquip::AirDistUnit;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::ZoneEquipConfig;
		using InputProcessor::SameString;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using PlantUtilities::InitComponentNodes;
		using DataGlobals::AnyPlantInModel;
		auto & GetHeatingCoilCapacity( HeatingCoils::GetCoilCapacity );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitSys" );
		static std::string const RoutineNameFull( "InitHVACSingleDuct" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		int ADUNum;
		int SysIndex;
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MySizeFlag;
		static Array1D_bool GetGasElecHeatCoilCap; // Gets autosized value of coil capacity
		Real64 SteamTemp;
		Real64 SteamDensity;
		Real64 rho;
		bool errFlag;

		static Array1D_bool PlantLoopScanFlag;

		// FLOW:

		// Do the Begin Simulation initializations
		if ( InitSysFlag ) {

			MyEnvrnFlag.allocate( NumSys );
			MySizeFlag.allocate( NumSys );
			PlantLoopScanFlag.allocate( NumSys );
			GetGasElecHeatCoilCap.allocate( NumSys );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			PlantLoopScanFlag = true;
			GetGasElecHeatCoilCap = true;
			InitSysFlag = false;
		}

		if ( PlantLoopScanFlag( SysNum ) && allocated( PlantLoop ) ) {
			if ( ( Sys( SysNum ).ReheatComp_PlantType == TypeOf_CoilWaterSimpleHeating ) || ( Sys( SysNum ).ReheatComp_PlantType == TypeOf_CoilSteamAirHeating ) ) {
				// setup plant topology indices for plant fed heating coils
				errFlag = false;
				ScanPlantLoopsForObject( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp_PlantType, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, Sys( SysNum ).HWCompIndex, _, _, _, _, _, errFlag );

				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + Sys( SysNum ).SysName + "\", type=" + Sys( SysNum ).SysType );
					ShowFatalError( "InitSys: Program terminated for previous conditions." );
				}

				Sys( SysNum ).ReheatCoilOutletNode = PlantLoop( Sys( SysNum ).HWLoopNum ).LoopSide( Sys( SysNum ).HWLoopSide ).Branch( Sys( SysNum ).HWBranchIndex ).Comp( Sys( SysNum ).HWCompIndex ).NodeNumOut;

				PlantLoopScanFlag( SysNum ) = false;
			} else {
				PlantLoopScanFlag( SysNum ) = false;
			}
		} else if ( PlantLoopScanFlag( SysNum ) && ! AnyPlantInModel ) {
			PlantLoopScanFlag( SysNum ) = false;
		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			// Check to see if there is a Air Distribution Unit on the Zone Equipment List
			for ( SysIndex = 1; SysIndex <= NumSys; ++SysIndex ) {
				if ( Sys( SysIndex ).ADUNum == 0 ) continue;
				if ( CheckZoneEquipmentList( "ZoneHVAC:AirDistributionUnit", AirDistUnit( Sys( SysIndex ).ADUNum ).Name ) ) continue;
				ShowSevereError( "InitSingleDuctSystems: ADU=[Air Distribution Unit," + AirDistUnit( Sys( SysIndex ).ADUNum ).Name + "] is not on any ZoneHVAC:EquipmentList." );
				ShowContinueError( "...System=[" + Sys( SysIndex ).SysType + ',' + Sys( SysIndex ).SysName + "] will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( SysNum ) ) {

			SizeSys( SysNum );

			MySizeFlag( SysNum ) = false;
		}

		if ( GetGasElecHeatCoilCap( SysNum ) ) {
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_Electric || Sys( SysNum ).ReheatComp_Num == HCoilType_Gas ) {
				if ( Sys( SysNum ).ReheatCoilMaxCapacity == AutoSize ) {
					errFlag = false;
					Sys( SysNum ).ReheatCoilMaxCapacity = GetHeatingCoilCapacity( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, errFlag );
					if ( errFlag ) ShowContinueError( "Occurs for terminal unit " + Sys( SysNum ).SysType + " = " + Sys( SysNum ).SysName );
				}
				if ( Sys( SysNum ).ReheatCoilMaxCapacity != AutoSize ) {
					GetGasElecHeatCoilCap( SysNum ) = false;
				}
			} else {
				GetGasElecHeatCoilCap( SysNum ) = false;
			}
		}
		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( SysNum ) ) {

			// Set the outlet node max mass flow rate to the Max Air Flow specified for the Sys
			OutletNode = Sys( SysNum ).OutletNodeNum;
			InletNode = Sys( SysNum ).InletNodeNum;
			Node( OutletNode ).MassFlowRateMax = Sys( SysNum ).MaxAirVolFlowRate * StdRhoAir;
			Sys( SysNum ).AirMassFlowRateMax = Sys( SysNum ).MaxAirVolFlowRate * StdRhoAir;
			Sys( SysNum ).HeatAirMassFlowRateMax = Sys( SysNum ).MaxHeatAirVolFlowRate * StdRhoAir;
			Node( InletNode ).MassFlowRateMax = Sys( SysNum ).MaxAirVolFlowRate * StdRhoAir;
			MassFlowDiff( SysNum ) = 1.0e-10 * Sys( SysNum ).AirMassFlowRateMax;

			if ( Sys( SysNum ).HWLoopNum > 0 && Sys( SysNum ).ReheatComp_Num != HCoilType_SteamAirHeating ) { //protect early calls before plant is setup
				rho = GetDensityGlycol( PlantLoop( Sys( SysNum ).HWLoopNum ).FluidName, InitConvTemp, PlantLoop( Sys( SysNum ).HWLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = 1000.0;
			}

			Sys( SysNum ).MaxReheatWaterFlow = rho * Sys( SysNum ).MaxReheatWaterVolFlow;
			Sys( SysNum ).MinReheatWaterFlow = rho * Sys( SysNum ).MinReheatWaterVolFlow;

			Sys( SysNum ).AirMassFlowDuringReheatMax = Sys( SysNum ).MaxAirVolFlowRateDuringReheat * StdRhoAir;

			// set the upstream leakage flowrate
			ADUNum = Sys( SysNum ).ADUNum;
			if ( AirDistUnit( ADUNum ).UpStreamLeak ) {
				AirDistUnit( ADUNum ).MassFlowRateUpStrLk = Sys( SysNum ).AirMassFlowRateMax * AirDistUnit( ADUNum ).UpStreamLeakFrac;
			} else {
				AirDistUnit( ADUNum ).MassFlowRateUpStrLk = 0.0;
			}

			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
				SteamTemp = 100.0;
				SteamDensity = GetSatDensityRefrig( fluidNameSteam, SteamTemp, 1.0, Sys( SysNum ).FluidIndex, RoutineNameFull );
				Sys( SysNum ).MaxReheatSteamFlow = SteamDensity * Sys( SysNum ).MaxReheatSteamVolFlow;
				Sys( SysNum ).MinReheatSteamFlow = SteamDensity * Sys( SysNum ).MinReheatSteamVolFlow;
			}

			if ( SameString( Sys( SysNum ).SysType, "AirTerminal:SingleDuct:VAV:Reheat" ) || SameString( Sys( SysNum ).SysType, "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat" ) || SameString( Sys( SysNum ).SysType, "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat" ) || SameString( Sys( SysNum ).SysType, "AirTerminal:SingleDuct:VAV:NoReheat" ) ) {
				// need the lowest schedule value
				if ( Sys( SysNum ).ZoneMinAirFracMethod == ScheduledMinFrac ) {
					Sys( SysNum ).ZoneMinAirFrac = GetScheduleMinValue( Sys( SysNum ).ZoneMinAirFracSchPtr );
				}
				Node( OutletNode ).MassFlowRateMin = Node( OutletNode ).MassFlowRateMax * Sys( SysNum ).ZoneMinAirFrac;
				Node( InletNode ).MassFlowRateMin = Node( InletNode ).MassFlowRateMax * Sys( SysNum ).ZoneMinAirFrac;
			} else {
				Node( OutletNode ).MassFlowRateMin = 0.0;
				Node( InletNode ).MassFlowRateMin = 0.0;
			}
			if ( ( Sys( SysNum ).ReheatControlNode > 0 ) && ! PlantLoopScanFlag( SysNum ) ) {
				if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SteamAirHeating ) {
					InitComponentNodes( Sys( SysNum ).MinReheatSteamFlow, Sys( SysNum ).MaxReheatSteamFlow, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).ReheatCoilOutletNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, Sys( SysNum ).HWCompIndex );
				} else {
					InitComponentNodes( Sys( SysNum ).MinReheatWaterFlow, Sys( SysNum ).MaxReheatWaterFlow, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).ReheatCoilOutletNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, Sys( SysNum ).HWCompIndex );

				}
			}
			// Find air loop associated with terminal unit
			if ( Sys( SysNum ).SysType_Num == SingleDuctVAVReheat || Sys( SysNum ).SysType_Num == SingleDuctVAVNoReheat ) {
				if ( Sys( SysNum ).CtrlZoneNum > 0 ) {
					Sys( SysNum ).AirLoopNum = ZoneEquipConfig( Sys( SysNum ).CtrlZoneNum ).AirLoopNum;
				}
			}

			MyEnvrnFlag( SysNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( SysNum ) = true;
		}

		// Initialize the Inlet Nodes of the air side of air terminal
		InletNode = Sys( SysNum ).InletNodeNum;
		OutletNode = Sys( SysNum ).OutletNodeNum;

		if ( Sys( SysNum ).ZoneMinAirFracMethod == ScheduledMinFrac ) {
			Sys( SysNum ).ZoneMinAirFrac = GetCurrentScheduleValue( Sys( SysNum ).ZoneMinAirFracSchPtr );
			//now reset inlet node min avail
			Node( InletNode ).MassFlowRateMinAvail = Sys( SysNum ).AirMassFlowRateMax * Sys( SysNum ).ZoneMinAirFrac;
		}

		if ( FirstHVACIteration ) {
			//The first time through set the mass flow rate to the Max
			if ( ( Node( InletNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Sys( SysNum ).SchedPtr ) > 0.0 ) ) {
				if ( ! ( SimulateAirflowNetwork > AirflowNetworkControlMultizone && AirflowNetworkFanActivated ) ) {
					Node( InletNode ).MassFlowRate = Sys( SysNum ).AirMassFlowRateMax;
				}
			} else {
				Node( InletNode ).MassFlowRate = 0.0;
			}

			if ( ( Node( InletNode ).MassFlowRateMaxAvail > 0.0 ) && ( GetCurrentScheduleValue( Sys( SysNum ).SchedPtr ) > 0.0 ) ) {
				if ( ! ( SimulateAirflowNetwork > AirflowNetworkControlMultizone && AirflowNetworkFanActivated ) ) {
					Node( InletNode ).MassFlowRateMaxAvail = Sys( SysNum ).AirMassFlowRateMax;
				}
			} else {
				Node( InletNode ).MassFlowRateMaxAvail = 0.0;
			}

			if ( ( Node( InletNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Sys( SysNum ).SchedPtr ) > 0.0 ) ) {
				if ( ! ( SimulateAirflowNetwork > AirflowNetworkControlMultizone && AirflowNetworkFanActivated ) ) {
					Node( InletNode ).MassFlowRateMinAvail = Sys( SysNum ).AirMassFlowRateMax * Sys( SysNum ).ZoneMinAirFrac;
				}
			} else {
				Node( InletNode ).MassFlowRateMinAvail = 0.0;
			}
			// reset the mass flow rate histories
			MassFlow1( SysNum ) = 0.0;
			MassFlow2( SysNum ) = 0.0;
			MassFlow3( SysNum ) = 0.0;
			MassFlow3( SysNum ) = 0.0;

		}

		//Do a check and make sure that the max and min available(control) flow is
		//  between the physical max and min while operating.
		SysInlet( SysNum ).AirMassFlowRateMaxAvail = min( Sys( SysNum ).AirMassFlowRateMax, Node( InletNode ).MassFlowRateMaxAvail );
		SysInlet( SysNum ).AirMassFlowRateMinAvail = min( max( Node( OutletNode ).MassFlowRateMin, Node( InletNode ).MassFlowRateMinAvail ), SysInlet( SysNum ).AirMassFlowRateMaxAvail );

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.
		// Load the node data in this section for the component simulation
		SysInlet( SysNum ).AirMassFlowRate = Node( InletNode ).MassFlowRate;
		SysInlet( SysNum ).AirTemp = Node( InletNode ).Temp;
		SysInlet( SysNum ).AirHumRat = Node( InletNode ).HumRat;
		SysInlet( SysNum ).AirEnthalpy = Node( InletNode ).Enthalpy;

	}

	void
	SizeSys( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2001
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Sys Components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		using SteamCoils::GetCoilSteamOutletNode;
		//  USE BranchInputManager,  ONLY: MyPlantSizingIndex
		using General::SafeDivide;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		//unused  USE DataHeatBalance,     ONLY: Zone
		using DataGlobals::AutoCalculate;
		using ReportSizingManager::ReportSizingOutput;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeSys" );
		static std::string const RoutineNameFull( "SizeHVACSingleDuct" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		static Real64 CoilInTemp( 0.0 );
		static Real64 DesCoilLoad( 0.0 );
		static Real64 DesZoneHeatLoad( 0.0 );
		static Real64 ZoneDesTemp( 0.0 );
		static Real64 ZoneDesHumRat( 0.0 );
		Real64 DesMassFlow;
		Real64 TempSteamIn;
		Real64 EnthSteamOutWet;
		Real64 EnthSteamInDry;
		Real64 LatentHeatSteam;
		Real64 SteamDensity;
		static int CoilWaterInletNode( 0 );
		static int CoilWaterOutletNode( 0 );
		static int CoilSteamInletNode( 0 );
		static int CoilSteamOutletNode( 0 );

		bool ErrorsFound;
		bool PlantSizingErrorsFound;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		static int DummyWaterIndex( 1 );
		static Real64 UserInputMaxHeatAirVolFlowRate( 0.0 ); // user input for MaxHeatAirVolFlowRate
		bool IsAutoSize;
		Real64 MaxAirVolFlowRateDes; // Autosized maximum air flow rate for reporting
		Real64 MaxAirVolFlowRateUser; // Hardsized maximum air flow rate for reporting
		Real64 MaxHeatAirVolFlowRateDes; // Autosized maximum heating air flow rate for reporting
		Real64 MaxHeatAirVolFlowRateUser; // Hardsized maximum heating air flow rate for reporting
		Real64 MaxAirVolFlowRateDuringReheatDes; // Autosized maximum air flow durign reheat for reporting
		Real64 MaxAirVolFlowRateDuringReheatUser; // Hardsized maximum air flow durign reheat for reporting
		Real64 MaxAirVolFractionDuringReheatDes; // Autosized maximum air fraction durign reheat for reporting
		Real64 MaxAirVolFractionDuringReheatUser; // Hardsized maximum air flow durign reheat for reporting
		Real64 MaxReheatWaterVolFlowDes; // Autosized reheat water flow or reporting
		Real64 MaxReheatWaterVolFlowUser; // Hardsized reheat water flow for reporting
		Real64 MaxReheatSteamVolFlowDes; // Autosized reheat steam flow for reporting
		Real64 MaxReheatSteamVolFlowUser; // Hardsized reheat steam flow for reporting

		PltSizHeatNum = 0;
		DesMassFlow = 0.0;
		ErrorsFound = false;
		IsAutoSize = false;
		MaxAirVolFlowRateDes = 0.0;
		MaxAirVolFlowRateUser = 0.0;
		MaxHeatAirVolFlowRateDes = 0.0;
		MaxHeatAirVolFlowRateUser = 0.0;
		MaxAirVolFlowRateDuringReheatDes = 0.0;
		MaxAirVolFlowRateDuringReheatUser = 0.0;
		MaxAirVolFractionDuringReheatDes = 0.0;
		MaxAirVolFractionDuringReheatUser = 0.0;
		MaxReheatWaterVolFlowDes = 0.0;
		MaxReheatWaterVolFlowUser = 0.0;
		MaxReheatSteamVolFlowDes = 0.0;
		MaxReheatSteamVolFlowUser = 0.0;

		if ( Sys( SysNum ).MaxAirVolFlowRate == AutoSize ) {
			IsAutoSize = true;
		}

		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
				if ( Sys( SysNum ).MaxAirVolFlowRate > 0.0 ) {
					ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "User-Specified Maximum Air Flow Rate [m3/s]", Sys( SysNum ).MaxAirVolFlowRate );
				}
			} else { // Autosize or hard-size with sizing run

				CheckZoneSizing( Sys( SysNum ).SysType, Sys( SysNum ).SysName );

				MaxAirVolFlowRateDes = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );

				if ( MaxAirVolFlowRateDes < SmallAirVolFlow ) {
					MaxAirVolFlowRateDes = 0.0;
				}
				if ( IsAutoSize ) {
					Sys( SysNum ).MaxAirVolFlowRate = MaxAirVolFlowRateDes;
					ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateDes );
				} else { // Hard-size with sizing data
					if ( Sys( SysNum ).MaxAirVolFlowRate > 0.0 && MaxAirVolFlowRateDes > 0.0 ) {
						MaxAirVolFlowRateUser = Sys( SysNum ).MaxAirVolFlowRate;
						ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateDes, "User-Specified Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxAirVolFlowRateDes - MaxAirVolFlowRateUser ) / MaxAirVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeHVACSingleDuct: Potential issue with equipment sizing for " + Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName + "\"." );
								ShowContinueError( "User-Specified Maximum Air Flow Rate of " + RoundSigDigits( MaxAirVolFlowRateUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Air Flow Rate of " + RoundSigDigits( MaxAirVolFlowRateDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( Sys( SysNum ).MaxHeatAirVolFlowRate == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation should continue
				UserInputMaxHeatAirVolFlowRate = Sys( SysNum ).MaxHeatAirVolFlowRate;
				if ( Sys( SysNum ).MaxHeatAirVolFlowRate > 0.0 ) {
					ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "User-Specified Maximum Heating Air Flow Rate [m3/s]", Sys( SysNum ).MaxHeatAirVolFlowRate );
				}
			} else {
				CheckZoneSizing( Sys( SysNum ).SysType, Sys( SysNum ).SysName );
				MaxHeatAirVolFlowRateDes = TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
				if ( MaxHeatAirVolFlowRateDes < SmallAirVolFlow ) {
					MaxHeatAirVolFlowRateDes = 0.0;
				}
				if ( IsAutoSize ) {
					Sys( SysNum ).MaxHeatAirVolFlowRate = MaxHeatAirVolFlowRateDes;
					UserInputMaxHeatAirVolFlowRate = 0.0;
					ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Heating Air Flow Rate [m3/s]", MaxHeatAirVolFlowRateDes );
				} else { // Hard-size with sizing data
					if ( Sys( SysNum ).MaxHeatAirVolFlowRate > 0.0 && MaxHeatAirVolFlowRateDes > 0.0 ) {
						MaxHeatAirVolFlowRateUser = Sys( SysNum ).MaxHeatAirVolFlowRate;
						UserInputMaxHeatAirVolFlowRate = Sys( SysNum ).MaxHeatAirVolFlowRate;
						ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Heating Air Flow Rate [m3/s]", MaxHeatAirVolFlowRateDes, "User-Specified Maximum Heating Air Flow Rate [m3/s]", MaxHeatAirVolFlowRateUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxHeatAirVolFlowRateDes - MaxHeatAirVolFlowRateUser ) / MaxHeatAirVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeHVACSingleDuct: Potential issue with equipment sizing for " + Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName + "\"." );
								ShowContinueError( "User-Specified Maximum Heating Air Flow Rate of " + RoundSigDigits( MaxHeatAirVolFlowRateUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Heating Air Flow Rate of " + RoundSigDigits( MaxHeatAirVolFlowRateDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		if ( Sys( SysNum ).ZoneMinAirFracMethod == ScheduledMinFrac ) {
			// need a value for sizing.
			if ( Sys( SysNum ).ConstantMinAirFracSetByUser ) {
				Sys( SysNum ).ZoneMinAirFrac = Sys( SysNum ).DesignMinAirFrac;
				// if both inputs are defined, use the max
				if ( Sys( SysNum ).FixedMinAirSetByUser ) {
					Sys( SysNum ).ZoneMinAirFrac = min( 1.0, max( Sys( SysNum ).ZoneMinAirFrac, SafeDivide( Sys( SysNum ).DesignFixedMinAir, Sys( SysNum ).MaxAirVolFlowRate ) ) );
				}
				// if only fixed is defined, use the value
			} else if ( Sys( SysNum ).FixedMinAirSetByUser ) {
				Sys( SysNum ).ZoneMinAirFrac = min( 1.0, SafeDivide( Sys( SysNum ).DesignFixedMinAir, Sys( SysNum ).MaxAirVolFlowRate ) );
			} else {
				//use an average of min and max in schedule
				Sys( SysNum ).ZoneMinAirFrac = ( GetScheduleMinValue( Sys( SysNum ).ZoneMinAirFracSchPtr ) + GetScheduleMaxValue( Sys( SysNum ).ZoneMinAirFracSchPtr ) ) / 2.0;
			}

		}

		if ( Sys( SysNum ).ZoneMinAirFracMethod == FixedMin ) {
			// need a value for sizing.
			Sys( SysNum ).ZoneMinAirFrac = min( 1.0, SafeDivide( Sys( SysNum ).DesignFixedMinAir, Sys( SysNum ).MaxAirVolFlowRate ) );

		}

		IsAutoSize = false;
		if ( Sys( SysNum ).MaxAirVolFlowRateDuringReheat == AutoCalculate ) {
			IsAutoSize = true;
		}

		MaxAirVolFlowRateDuringReheatDes = min( 0.002032 * Sys( SysNum ).ZoneFloorArea, Sys( SysNum ).MaxAirVolFlowRate );
		// apply limit based on min stop
		MaxAirVolFlowRateDuringReheatDes = max( MaxAirVolFlowRateDuringReheatDes, ( Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac ) );
		if ( IsAutoSize ) {
			Sys( SysNum ).MaxAirVolFlowRateDuringReheat = MaxAirVolFlowRateDuringReheatDes;
			ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]", MaxAirVolFlowRateDuringReheatDes / Sys( SysNum ).ZoneFloorArea );
		} else { // Hard size with sizing data
			if ( Sys( SysNum ).MaxAirVolFlowRateDuringReheat > 0.0 && MaxAirVolFlowRateDuringReheatDes > 0.0 ) {
				MaxAirVolFlowRateDuringReheatUser = Sys( SysNum ).MaxAirVolFlowRateDuringReheat;
				ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]", MaxAirVolFlowRateDuringReheatDes / Sys( SysNum ).ZoneFloorArea, "User-Specified Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]", MaxAirVolFlowRateDuringReheatUser );
				if ( DisplayExtraWarnings ) {
					if ( ( std::abs( MaxAirVolFlowRateDuringReheatDes - MaxAirVolFlowRateDuringReheatUser ) / MaxAirVolFlowRateDuringReheatUser ) > AutoVsHardSizingThreshold ) {
						ShowMessage( "SizeHVACSingleDuct: Potential issue with equipment sizing for " + Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName + "\"." );
						ShowContinueError( "User-Specified Maximum Flow per Zone Floor Area during Reheat of " + RoundSigDigits( MaxAirVolFlowRateDuringReheatUser, 5 ) + " [m3/s-m2]" );
						ShowContinueError( "differs from Design Size Maximum Flow per Zone Floor Area during Reheat of " + RoundSigDigits( MaxAirVolFlowRateDuringReheatDes, 5 ) + " [m3/s-m2]" );
						ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
						ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
					}
				}
			}
		}

		IsAutoSize = false;
		if ( Sys( SysNum ).MaxAirVolFractionDuringReheat == AutoCalculate ) {
			IsAutoSize = true;
		}
		if ( Sys( SysNum ).MaxAirVolFlowRate > 0.0 ) {
			MaxAirVolFractionDuringReheatDes = min( 1.0, ( 0.002032 * Sys( SysNum ).ZoneFloorArea / Sys( SysNum ).MaxAirVolFlowRate ) );
			// apply limit based on min stop
			MaxAirVolFractionDuringReheatDes = max( MaxAirVolFractionDuringReheatDes, Sys( SysNum ).ZoneMinAirFrac );
		} else {
			MaxAirVolFractionDuringReheatDes = 0.0;
		}
		if ( IsAutoSize ) {
			Sys( SysNum ).MaxAirVolFractionDuringReheat = MaxAirVolFractionDuringReheatDes;
			ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Flow Fraction during Reheat []", MaxAirVolFractionDuringReheatDes );
		} else {
			if ( Sys( SysNum ).MaxAirVolFractionDuringReheat > 0.0 && MaxAirVolFractionDuringReheatDes > 0.0 ) {
				MaxAirVolFractionDuringReheatUser = Sys( SysNum ).MaxAirVolFractionDuringReheat;
				ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Flow Fraction during Reheat []", MaxAirVolFractionDuringReheatDes, "User-Specified Maximum Flow Fraction during Reheat []", MaxAirVolFractionDuringReheatUser );
				if ( DisplayExtraWarnings ) {
					if ( ( std::abs( MaxAirVolFractionDuringReheatDes - MaxAirVolFractionDuringReheatUser ) / MaxAirVolFractionDuringReheatUser ) > AutoVsHardSizingThreshold ) {
						ShowMessage( "SizeHVACSingleDuct: Potential issue with equipment sizing for " + Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName + "\"." );
						ShowContinueError( "User-Specified Maximum Flow Fraction during Reheat of " + RoundSigDigits( MaxAirVolFractionDuringReheatUser, 5 ) + " []" );
						ShowContinueError( "differs from Design Size Maximum Flow Fraction during Reheat of " + RoundSigDigits( MaxAirVolFractionDuringReheatDes, 5 ) + " []" );
						ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
						ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
					}
				}
			}
		}

		// use the larger of the two reheat flow rate methods for the simulated maximum flow during reheat
		Sys( SysNum ).MaxAirVolFlowRateDuringReheat = max( Sys( SysNum ).MaxAirVolFlowRateDuringReheat, Sys( SysNum ).MaxAirVolFractionDuringReheat * Sys( SysNum ).MaxAirVolFlowRate );
		Sys( SysNum ).MaxAirVolFlowRateDuringReheat = min( Sys( SysNum ).MaxAirVolFlowRateDuringReheat, Sys( SysNum ).MaxAirVolFlowRate );

		if ( CurZoneEqNum > 0 ) {
			TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = 1.0;
			TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = 1.0;
			if ( ZoneSizingRunDone ) {
				if ( Sys( SysNum ).SysType_Num == SingleDuctVAVReheatVSFan ) {
					TermUnitSizing( CurZoneEqNum ).AirVolFlow = max( UserInputMaxHeatAirVolFlowRate, CalcFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor, Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac );
				} else {
					TermUnitSizing( CurZoneEqNum ).AirVolFlow = max( CalcFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor, Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac );
				}
			} else {
				if ( Sys( SysNum ).SysType_Num == SingleDuctVAVReheatVSFan ) {
					TermUnitSizing( CurZoneEqNum ).AirVolFlow = max( Sys( SysNum ).MaxHeatAirVolFlowRate, Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac );
				} else if ( Sys( SysNum ).SysType_Num == SingleDuctConstVolReheat ) {
					TermUnitSizing( CurZoneEqNum ).AirVolFlow = Sys( SysNum ).MaxAirVolFlowRate;
				} else {
					if ( Sys( SysNum ).DamperHeatingAction == ReverseAction ) {
						if ( Sys( SysNum ).MaxAirVolFlowRateDuringReheat > 0.0 ) {
							TermUnitSizing( CurZoneEqNum ).AirVolFlow = max( Sys( SysNum ).MaxAirVolFlowRateDuringReheat, ( Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac ) );
						} else {
							TermUnitSizing( CurZoneEqNum ).AirVolFlow = Sys( SysNum ).MaxAirVolFlowRate;
						}
					} else {
						TermUnitSizing( CurZoneEqNum ).AirVolFlow = Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac;
					}
				}
			}
			if ( TermUnitSizing( CurZoneEqNum ).AirVolFlow > SmallAirVolFlow ) {
				if ( Sys( SysNum ).DamperHeatingAction == ReverseAction && Sys( SysNum ).MaxAirVolFlowRateDuringReheat > 0.0 ) {
					TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = min( Sys( SysNum ).MaxAirVolFlowRateDuringReheat, Sys( SysNum ).MaxAirVolFlowRate ) / TermUnitSizing( CurZoneEqNum ).AirVolFlow;
					TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult;
				} else if ( Sys( SysNum ).DamperHeatingAction == ReverseAction && Sys( SysNum ).MaxAirVolFlowRateDuringReheat == 0.0 ) {
					TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = Sys( SysNum ).MaxAirVolFlowRate / TermUnitSizing( CurZoneEqNum ).AirVolFlow;
					TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult;
				} else if ( Sys( SysNum ).DamperHeatingAction == Normal && Sys( SysNum ).MaxAirVolFlowRateDuringReheat > 0.0 ) {
					TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = min( Sys( SysNum ).MaxAirVolFlowRateDuringReheat, ( Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac ) ) / TermUnitSizing( CurZoneEqNum ).AirVolFlow;
					TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = 1.0;
				} else if ( Sys( SysNum ).DamperHeatingAction == Normal && Sys( SysNum ).MaxAirVolFlowRateDuringReheat == 0.0 ) {
					TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = ( Sys( SysNum ).MaxAirVolFlowRate * Sys( SysNum ).ZoneMinAirFrac ) / TermUnitSizing( CurZoneEqNum ).AirVolFlow;
					TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = 1.0;
				} else {
					TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = Sys( SysNum ).MaxAirVolFlowRate / TermUnitSizing( CurZoneEqNum ).AirVolFlow;
					TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult;
				}
				TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = max( 1.0, TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult );
				TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = max( 1.0, TermUnitSizing( CurZoneEqNum ).ReheatLoadMult );
			} else {
				TermUnitSizing( CurZoneEqNum ).ReheatAirFlowMult = 1.0;
				TermUnitSizing( CurZoneEqNum ).ReheatLoadMult = 1.0;
			}
		}

		IsAutoSize = false;
		if ( Sys( SysNum ).MaxReheatWaterVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) {
				if ( Sys( SysNum ).MaxReheatWaterVolFlow > 0.0 ) {
					ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "User-Specified Maximum Reheat Water Flow Rate [m3/s]", Sys( SysNum ).MaxReheatWaterVolFlow );
				}
			} else {
				CheckZoneSizing( Sys( SysNum ).SysType, Sys( SysNum ).SysName );
				if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Water" ) ) {
					CoilWaterInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", Sys( SysNum ).ReheatName, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( "Coil:Heating:Water", Sys( SysNum ).ReheatName, ErrorsFound );
					if ( IsAutoSize ) {
						PlantSizingErrorsFound = false;
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", Sys( SysNum ).ReheatName, CoilWaterInletNode, CoilWaterOutletNode, PlantSizingErrorsFound );
						if ( PlantSizingErrorsFound ) {
							ShowContinueError( "...Occurs in " + Sys( SysNum ).SysType + ':' + Sys( SysNum ).SysName );
							ErrorsFound = true;
						}
						if ( PltSizHeatNum > 0 ) {
							CoilInTemp = TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU;
							DesMassFlow = StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow;
							DesZoneHeatLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							ZoneDesTemp = CalcFinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
							ZoneDesHumRat = CalcFinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak;
							// the coil load is the zone design heating load plus (or minus!) the reheat load
							DesCoilLoad = DesZoneHeatLoad + PsyCpAirFnWTdb( ZoneDesHumRat, 0.5 * ( CoilInTemp + ZoneDesTemp ) ) * DesMassFlow * ( ZoneDesTemp - CoilInTemp );
							if ( DesCoilLoad >= SmallLoad ) {

								rho = GetDensityGlycol( PlantLoop( Sys( SysNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( Sys( SysNum ).HWLoopNum ).FluidIndex, RoutineName );

								Cp = GetSpecificHeatGlycol( PlantLoop( Sys( SysNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( Sys( SysNum ).HWLoopNum ).FluidIndex, RoutineName );

								MaxReheatWaterVolFlowDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
							} else {
								MaxReheatWaterVolFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in AirTerminal Object=" + Sys( SysNum ).SysName );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						Sys( SysNum ).MaxReheatWaterVolFlow = MaxReheatWaterVolFlowDes;
						ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Reheat Water Flow Rate [m3/s]", MaxReheatWaterVolFlowDes );
						ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Reheat Coil Sizing Air Volume Flow Rate [m3/s]", TermUnitSizing( CurZoneEqNum ).AirVolFlow );
						ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Reheat Coil Sizing Inlet Air Temperature [C]", TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU );
						ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Reheat Coil Sizing Inlet Air Humidity Ratio [kgWater/kgDryAir]", TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU );						
					} else { // Hard-size with sizing data
						if ( Sys( SysNum ).MaxReheatWaterVolFlow > 0.0 && MaxReheatWaterVolFlowDes > 0.0 ) {
							MaxReheatWaterVolFlowUser = Sys( SysNum ).MaxReheatWaterVolFlow;
							ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Reheat Water Flow Rate [m3/s]", MaxReheatWaterVolFlowDes, "User-Specified Maximum Reheat Water Flow Rate [m3/s]", MaxReheatWaterVolFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxReheatWaterVolFlowDes - MaxReheatWaterVolFlowUser ) / MaxReheatWaterVolFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeHVACSingleDuct: Potential issue with equipment sizing for " + Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName + "\"." );
									ShowContinueError( "User-Specified Maximum Reheat Water Flow Rate of " + RoundSigDigits( MaxReheatWaterVolFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Reheat Water Flow Rate of " + RoundSigDigits( MaxReheatWaterVolFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			Sys( SysNum ).MaxReheatWaterVolFlow = 0.0;
		}

		IsAutoSize = false;
		if ( Sys( SysNum ).MaxReheatSteamVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) {
				if ( Sys( SysNum ).MaxReheatSteamVolFlow > 0.0 ) {
					ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "User-Specified Maximum Reheat Steam Flow Rate [m3/s]", Sys( SysNum ).MaxReheatSteamVolFlow );
				}
			} else {
				CheckZoneSizing( Sys( SysNum ).SysType, Sys( SysNum ).SysName );
				if ( SameString( Sys( SysNum ).ReheatComp, "Coil:Heating:Steam" ) ) {
					CoilSteamInletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", Sys( SysNum ).ReheatName, ErrorsFound );
					CoilSteamOutletNode = GetCoilSteamOutletNode( "Coil:Heating:Steam", Sys( SysNum ).ReheatName, ErrorsFound );
					if ( IsAutoSize ) {
						PlantSizingErrorsFound = false;
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Steam", Sys( SysNum ).ReheatName, CoilSteamInletNode, CoilSteamOutletNode, PlantSizingErrorsFound );
						if ( PlantSizingErrorsFound ) {
							ShowContinueError( "...Occurs in " + Sys( SysNum ).SysType + ':' + Sys( SysNum ).SysName );
							ErrorsFound = true;
						}
						if ( PltSizHeatNum > 0 ) {
							CoilInTemp = TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU;
							DesMassFlow = StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow;
							DesZoneHeatLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							ZoneDesTemp = CalcFinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
							ZoneDesHumRat = CalcFinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak;
							// the coil load is the zone design heating load plus (or minus!) the reheat load
							DesCoilLoad = DesZoneHeatLoad + PsyCpAirFnWTdb( ZoneDesHumRat, 0.5 * ( CoilInTemp + ZoneDesTemp ) ) * DesMassFlow * ( ZoneDesTemp - CoilInTemp );
							if ( DesCoilLoad >= SmallLoad ) {
								TempSteamIn = 100.00;
								EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, Sys( SysNum ).FluidIndex, RoutineNameFull );
								EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, Sys( SysNum ).FluidIndex, RoutineNameFull );
								LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
								SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, Sys( SysNum ).FluidIndex, RoutineNameFull );

								Cp = GetSpecificHeatGlycol( fluidNameWater, PlantSizData( PltSizHeatNum ).ExitTemp, DummyWaterIndex, RoutineName );
								MaxReheatSteamVolFlowDes = DesCoilLoad / ( SteamDensity * ( LatentHeatSteam + PlantSizData( PltSizHeatNum ).DeltaT * Cp ) );
							} else {
								MaxReheatSteamVolFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of Steam flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in AirTerminal:SingleDuct:ConstantVolume:Reheat Object=" + Sys( SysNum ).SysName );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						Sys( SysNum ).MaxReheatSteamVolFlow = MaxReheatSteamVolFlowDes;
						ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Reheat Steam Flow Rate [m3/s]", MaxReheatSteamVolFlowDes );
					} else {
						if ( Sys( SysNum ).MaxReheatSteamVolFlow > 0.0 && MaxReheatSteamVolFlowDes > 0.0 ) {
							MaxReheatSteamVolFlowUser = Sys( SysNum ).MaxReheatSteamVolFlow;
							ReportSizingOutput( Sys( SysNum ).SysType, Sys( SysNum ).SysName, "Design Size Maximum Reheat Steam Flow Rate [m3/s]", MaxReheatSteamVolFlowDes, "User-Specified Maximum Reheat Steam Flow Rate [m3/s]", MaxReheatSteamVolFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxReheatSteamVolFlowDes - MaxReheatSteamVolFlowUser ) / MaxReheatSteamVolFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeHVACSingleDuct: Potential issue with equipment sizing for " + Sys( SysNum ).SysType + " = \"" + Sys( SysNum ).SysName + "\"." );
									ShowContinueError( "User-Specified Maximum Reheat Steam Flow Rate of " + RoundSigDigits( MaxReheatSteamVolFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Reheat Steam Flow Rate of " + RoundSigDigits( MaxReheatSteamVolFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			Sys( SysNum ).MaxReheatSteamVolFlow = 0.0;
		}

		if ( CurZoneEqNum > 0 ) {
			TermUnitSizing( CurZoneEqNum ).MinFlowFrac = Sys( SysNum ).ZoneMinAirFrac;
			TermUnitSizing( CurZoneEqNum ).MaxHWVolFlow = Sys( SysNum ).MaxReheatWaterVolFlow;
			TermUnitSizing( CurZoneEqNum ).MaxSTVolFlow = Sys( SysNum ).MaxReheatSteamVolFlow;
			if ( Sys( SysNum ).ReheatComp_Num == HCoilType_SimpleHeating ) {
				if ( Sys( SysNum ).DamperHeatingAction == Normal ) {
					SetCoilDesFlow( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, Sys( SysNum ).ZoneMinAirFrac * Sys( SysNum ).MaxAirVolFlowRate, ErrorsFound );
				} else {
					SetCoilDesFlow( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, TermUnitSizing( CurZoneEqNum ).AirVolFlow, ErrorsFound );
				}
			}
		}

		if ( Sys( SysNum ).MaxAirVolFlowRateDuringReheat > 0.0 ) {
			// check for inconsistent dual max input
			if ( Sys( SysNum ).MaxAirVolFlowRateDuringReheat < ( Sys( SysNum ).ZoneMinAirFrac * Sys( SysNum ).MaxAirVolFlowRate ) ) {
				// Only warn when really out of bounds
				if ( ( Sys( SysNum ).ZoneMinAirFrac * Sys( SysNum ).MaxAirVolFlowRate ) - Sys( SysNum ).MaxAirVolFlowRateDuringReheat > 1.e-8 ) {
					ShowWarningError( "SingleDuctSystem:SizeSys: Air Terminal Unit flow limits are not consistent, minimum flow limit is larger than reheat maximum" );
					ShowContinueError( "Air Terminal Unit name = " + Sys( SysNum ).SysName );
					ShowContinueError( "Maximum terminal flow during reheat = " + RoundSigDigits( Sys( SysNum ).MaxAirVolFlowRateDuringReheat, 6 ) + " [m3/s] or flow fraction = " + RoundSigDigits( ( Sys( SysNum ).MaxAirVolFlowRateDuringReheat / Sys( SysNum ).MaxAirVolFlowRate ), 4 ) );
					ShowContinueError( "Minimum terminal flow = " + RoundSigDigits( ( Sys( SysNum ).ZoneMinAirFrac * Sys( SysNum ).MaxAirVolFlowRate ), 6 ) + " [m3/s] or flow fraction = " + RoundSigDigits( Sys( SysNum ).ZoneMinAirFrac, 4 ) );
					ShowContinueError( "The reheat maximum flow limit will be replaced by the minimum limit, and the simulation continues" );
				}
				Sys( SysNum ).MaxAirVolFlowRateDuringReheat = ( Sys( SysNum ).ZoneMinAirFrac * Sys( SysNum ).MaxAirVolFlowRate );
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimVAV(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   January 2000
		//       MODIFIED       Fred Buhl: added reverse action damper heating action: August 2001
		//                      KHL/TH 7/2010: revise to support dual max
		//                      FB/KHL/TH 9/2010: added maximum supply air temperature leaving reheat coil
		//                      TH 3/2012: added supply air flow adjustment based on zone maximum outdoor
		//                                 air fraction - a TRACE feature
		//                      Brent Griffith, 5/2012, general cleanup, fix negatives CR 8767, fix phantom coil flows CR 8854
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple single duct volume VAV.

		// METHODOLOGY EMPLOYED:
		// There is method to this madness.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		//unused   USE DataHeatBalFanSys, ONLY: Mat
		using WaterCoils::SimulateWaterCoilComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataDefineEquip::AirDistUnit;
		//unused   USE DataAirLoop,       ONLY: AirLoopControlInfo
		using PlantUtilities::SetActuatedBranchFlowRate;
		using DataHVACGlobals::SmallLoad;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkFanActivated;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;
		using DataAirflowNetwork::VAVTerminalRatio;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlow; // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
		Real64 QTotLoad; // [Watts] Remaining load required for this zone
		Real64 QZnReq; // [Watts] Load calculated for heating coil
		Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
		int ADUNum; // index of air distribution unit for this terminal unit
		Real64 CpAirZn;
		Real64 CpAirSysIn;
		Real64 DeltaTemp;
		int SysOutletNode; // The node number of the terminal unit outlet node
		int SysInletNode; // the node number of the terminal unit inlet node
		int WaterControlNode; // This is the Actuated Reheat Control Node
		Real64 MaxFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 MinFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 QActualHeating; // the heating load seen by the reheat coil
		Real64 QHeatingDelivered; // the actual output from heating coil
		Real64 LeakLoadMult; // load multiplier to adjust for downstream leaks
		Real64 MinFlowFrac; // minimum flow fraction (and minimum damper position)
		static Real64 MinAirMassFlowRevAct( 0.0 ); // minimum air mass flow rate used in "reverse action" air mass flow rate calculation
		static Real64 MaxAirMassFlowRevAct( 0.0 ); // maximum air mass flow rate used in "reverse action" air mass flow rate calculation
		Real64 MassFlowBasedOnOA; // supply air mass flow rate based on zone OA requirements
		Real64 AirLoopOAFrac; // fraction of outside air entering air loop
		Real64 DummyMdot; // temporary mass flow rate argument

		static Real64 ZoneTemp( 0.0 ); // zone air temperature [C]
		static Real64 MaxHeatTemp( 0.0 ); // maximum supply air temperature [C]
		static Real64 MaxDeviceAirMassFlowReheat( 0.0 ); // air mass flow rate required to meet the coil heating load [W]
		static Real64 MassFlowReqToLimitLeavingTemp( 0.0 ); // air mass flow rate actually used [W]
		static Real64 QZoneMaxRHTempLimit( 0.0 ); // maximum zone heat addition rate given constraints of MaxHeatTemp and max
		// available air mass flow rate [W]
		static Real64 MinMassAirFlow( 0.0 ); // the air flow rate during heating for normal acting damper
		static Real64 QZoneMax2( 0.0 ); // temporary variable

		// Note to the perplexed
		// The SINGLE DUCT:VAV:REHEAT terminal unit originally contained 2 components: a damper
		// and a reheat coil. The damper has become a virtual component - it consists only of
		// an air inlet node and an air outlet node. The damper is upstream of the heating coil.
		// Sys(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
		// Sys(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
		// Sys(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

		// The calculated load from the Heat Balance
		ADUNum = Sys( SysNum ).ADUNum;
		LeakLoadMult = AirDistUnit( ADUNum ).LeakLoadMult;
		QTotLoad = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired * LeakLoadMult;
		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP * LeakLoadMult;
		SysOutletNode = Sys( SysNum ).ReheatAirOutletNode;
		SysInletNode = Sys( SysNum ).InletNodeNum;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
		MinFlowFrac = Sys( SysNum ).ZoneMinAirFrac;
		MassFlowBasedOnOA = 0.0;
		ZoneTemp = Node( ZoneNodeNum ).Temp;
		MinMassAirFlow = MinFlowFrac * StdRhoAir * Sys( SysNum ).MaxAirVolFlowRate;

		//Then depending on if the Load is for heating or cooling it is handled differently.  First
		// the massflow rate for cooling is determined to meet the entire load.  Then
		// if the massflow is below the minimum or greater than the Max it is set to either the Min
		// or the Max as specified for the VAV model.
		if ( ( QTotLoad < 0.0 ) && ( SysInlet( SysNum ).AirMassFlowRateMaxAvail > 0.0 ) && ( TempControlType( ZoneNum ) != SingleHeatingSetPoint ) && ( GetCurrentScheduleValue( Sys( SysNum ).SchedPtr ) > 0.0 ) ) {
			// Calculate the flow required for cooling
			CpAirSysIn = PsyCpAirFnWTdb( SysInlet( SysNum ).AirHumRat, SysInlet( SysNum ).AirTemp );
			DeltaTemp = CpAirSysIn * SysInlet( SysNum ).AirTemp - CpAirZn * ZoneTemp;

			//Need to check DeltaTemp and ensure that it is not zero
			if ( DeltaTemp != 0.0 ) {
				MassFlow = QTotLoad / DeltaTemp;
			} else {
				MassFlow = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
			}

			// Apply the zone maximum outdoor air fraction FOR VAV boxes - a TRACE feature
			if ( ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor > 1.0 ) {
				MassFlow *= ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor;
			}

			// calculate supply air flow rate based on user specified OA requirement
			CalcOAMassFlow( SysNum, MassFlowBasedOnOA, AirLoopOAFrac );
			MassFlow = max( MassFlow, MassFlowBasedOnOA );

			// used for normal acting damper
			MinMassAirFlow = max( MinMassAirFlow, MassFlowBasedOnOA );
			MinMassAirFlow = max( MinMassAirFlow, SysInlet( SysNum ).AirMassFlowRateMinAvail );
			MinMassAirFlow = min( MinMassAirFlow, SysInlet( SysNum ).AirMassFlowRateMaxAvail );

			// limit the OA based supply air flow rate based on optional user input
			//Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
			MassFlow = max( MassFlow, SysInlet( SysNum ).AirMassFlowRateMinAvail );
			MassFlow = min( MassFlow, SysInlet( SysNum ).AirMassFlowRateMaxAvail );

			if ( SimulateAirflowNetwork > AirflowNetworkControlMultizone && AirflowNetworkFanActivated && VAVTerminalRatio > 0.0 ) {
				MassFlow *= VAVTerminalRatio;
				if ( MassFlow > Node( Sys( SysNum ).InletNodeNum ).MassFlowRate ) {
					MassFlow = Node( Sys( SysNum ).InletNodeNum ).MassFlowRate;
				}
			}

		} else if ( ( SysInlet( SysNum ).AirMassFlowRateMaxAvail > 0.0 ) && ( QTotLoad >= 0.0 || TempControlType( ZoneNum ) == SingleHeatingSetPoint ) && ( GetCurrentScheduleValue( Sys( SysNum ).SchedPtr ) > 0.0 ) ) {
			//     IF (Sys(SysNum)%DamperHeatingAction .EQ. ReverseAction .AND. SysInlet(SysNum)%AirMassFlowRateMinAvail <= SmallMassFlow) THEN
			// special case for heating: reverse action and damper allowed to close - set the minimum flow rate to a small but nonzero value
			//       MassFlow = 0.01d0*SysInlet(SysNum)%AirMassFlowRateMaxAvail
			//     ELSE
			// usual case for heating: set the air mass flow rate to the minimum
			MassFlow = SysInlet( SysNum ).AirMassFlowRateMinAvail;
			//     END IF

			// Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
			if ( ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor > 1.0 ) {
				MassFlow *= ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor;
			}

			// calculate supply air flow rate based on user specified OA requirement
			CalcOAMassFlow( SysNum, MassFlowBasedOnOA, AirLoopOAFrac );
			MassFlow = max( MassFlow, MassFlowBasedOnOA );

			//Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
			if ( MassFlow <= SysInlet( SysNum ).AirMassFlowRateMinAvail ) {
				MassFlow = SysInlet( SysNum ).AirMassFlowRateMinAvail;
			} else if ( MassFlow >= SysInlet( SysNum ).AirMassFlowRateMaxAvail ) {
				MassFlow = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
			}

			// the AirflowNetwork model overrids the mass flow rate value
			if ( SimulateAirflowNetwork > AirflowNetworkControlMultizone && AirflowNetworkFanActivated && VAVTerminalRatio > 0.0 ) {
				MassFlow *= VAVTerminalRatio;
				if ( MassFlow > Node( Sys( SysNum ).InletNodeNum ).MassFlowRate ) {
					MassFlow = Node( Sys( SysNum ).InletNodeNum ).MassFlowRate;
				}
			}

		} else {
			// System is Off set massflow to 0.0
			MassFlow = 0.0;
			AirLoopOAFrac = 0.0;
		}

		// look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
		// equipment iteration. If detected, set flow rate to previous value.
		if ( ( ( std::abs( MassFlow - MassFlow2( SysNum ) ) < MassFlowDiff( SysNum ) ) || ( std::abs( MassFlow - MassFlow3( SysNum ) ) < MassFlowDiff( SysNum ) ) ) && ( std::abs( MassFlow - MassFlow1( SysNum ) ) >= MassFlowDiff( SysNum ) ) ) {
			if ( MassFlow > 0.0 ) MassFlow = MassFlow1( SysNum );
		}

		//Move data to the damper outlet node
		SysOutlet( SysNum ).AirTemp = SysInlet( SysNum ).AirTemp;
		SysOutlet( SysNum ).AirHumRat = SysInlet( SysNum ).AirHumRat;
		SysOutlet( SysNum ).AirMassFlowRate = MassFlow;
		SysOutlet( SysNum ).AirMassFlowRateMaxAvail = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
		SysOutlet( SysNum ).AirMassFlowRateMinAvail = SysInlet( SysNum ).AirMassFlowRateMinAvail;
		SysOutlet( SysNum ).AirEnthalpy = SysInlet( SysNum ).AirEnthalpy;

		//   ! Calculate the Damper Position when there is a Max air flow specified.
		//  If (MassFlow == 0.0D0) THEN
		//    Sys(SysNum)%DamperPosition = 0.0D0
		//  ELSE IF (SysInlet(SysNum)%AirMassFlowRateMaxAvail > SysInlet(SysNum)%AirMassFlowRateMinAvail) THEN
		//    Sys(SysNum)%DamperPosition = ((MassFlow-SysInlet(SysNum)%AirMassFlowRateMinAvail) / &
		//                                   (SysInlet(SysNum)%AirMassFlowRateMaxAvail-SysInlet(SysNum)%AirMassFlowRateMinAvail)) * &
		//                                  (1.0d0-MinFlowFrac) + MinFlowFrac
		//  ELSE
		//    Sys(SysNum)%DamperPosition = 1.0D0
		//  END IF

		if ( MassFlow == 0.0 ) {
			Sys( SysNum ).DamperPosition = 0.0;
			Sys( SysNum ).ZoneMinAirFracReport = 0.0;
		} else if ( ( MassFlow > 0.0 ) && ( MassFlow < Sys( SysNum ).AirMassFlowRateMax ) ) {
			Sys( SysNum ).DamperPosition = MassFlow / Sys( SysNum ).AirMassFlowRateMax;
			Sys( SysNum ).ZoneMinAirFracReport = Sys( SysNum ).ZoneMinAirFrac;
		} else if ( MassFlow == Sys( SysNum ).AirMassFlowRateMax ) {
			Sys( SysNum ).DamperPosition = 1.0;
			Sys( SysNum ).ZoneMinAirFracReport = Sys( SysNum ).ZoneMinAirFrac;
		}

		//Need to make sure that the damper outlets are passed to the coil inlet
		UpdateSys( SysNum );

		// At the current air mass flow rate, calculate heating coil load
		QActualHeating = QToHeatSetPt - MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp ); // reheat needed

		// do the reheat calculation if there's some air nass flow (or the damper action is "reverse action"), the flow is <= minimum ,
		// there's a heating requirement, and there's a thermostat with a heating setpoint
		// Reverse damper option is working only for water coils for now.
		if ( ( MassFlow > SmallMassFlow ) && ( QActualHeating > 0.0 ) && ( TempControlType( ZoneNum ) != SingleCoolingSetPoint ) ) {
			// At this point we know that there is a heating requirement: i.e., the heating coil needs to
			// be activated (there's a zone heating load or there's a reheat requirement). There are 3 possible
			// situations: 1) the coil load can be met by variable temperature air (below the max heat temp) at
			// the minimum air mass flow rate; 2) the coil load can be met by variable air flow rate with the air
			// temperature fixed at the max heat temp; 3) the load cannot be met (we will run at max air temp and
			// max air flow rate). We check for condition 2 by assuming the air temperatute is at the max heat temp
			// and solving for the air mass flow rate that will meet the load. If the flow rate is between the min and
			// max we are in condition 2.

			QZoneMax2 = QToHeatSetPt;

			// fill dual-max reheat flow limit, if any
			if ( Sys( SysNum ).DamperHeatingAction == ReverseAction ) {
				if ( Sys( SysNum ).AirMassFlowDuringReheatMax > 0.0 ) {
					MaxDeviceAirMassFlowReheat = Sys( SysNum ).AirMassFlowDuringReheatMax;
				} else {
					MaxDeviceAirMassFlowReheat = Sys( SysNum ).AirMassFlowRateMax;
				}
			} else {
				MaxDeviceAirMassFlowReheat = Sys( SysNum ).AirMassFlowRateMax;
			}

			// determine flow based on leaving reheat temperature limit
			if ( Sys( SysNum ).MaxReheatTempSetByUser ) {

				MaxHeatTemp = Sys( SysNum ).MaxReheatTemp;
				if ( QToHeatSetPt > SmallLoad ) { // zone has a postive load to heating setpoint
					MassFlowReqToLimitLeavingTemp = QToHeatSetPt / ( CpAirZn * ( MaxHeatTemp - ZoneTemp ) );
				} else {
					MassFlowReqToLimitLeavingTemp = 0.0;
				}
			}

			// (re)apply limits to find air mass flow
			MassFlow = max( MassFlow, MassFlowReqToLimitLeavingTemp );
			MassFlow = min( MassFlow, MaxDeviceAirMassFlowReheat );
			MassFlow = max( MassFlow, MassFlowBasedOnOA );
			MassFlow = min( MassFlow, SysInlet( SysNum ).AirMassFlowRateMaxAvail );
			MassFlow = max( MassFlow, SysInlet( SysNum ).AirMassFlowRateMinAvail );

			if ( SimulateAirflowNetwork > AirflowNetworkControlMultizone && AirflowNetworkFanActivated && VAVTerminalRatio > 0.0 ) {
				MassFlow *= VAVTerminalRatio;
				if ( MassFlow > Node( Sys( SysNum ).InletNodeNum ).MassFlowRate ) {
					MassFlow = Node( Sys( SysNum ).InletNodeNum ).MassFlowRate;
				}
			}

			// now make any corrections to heating coil loads
			if ( Sys( SysNum ).MaxReheatTempSetByUser ) {
				QZoneMaxRHTempLimit = CpAirZn * MassFlow * ( MaxHeatTemp - ZoneTemp );
				QZoneMax2 = min( QZoneMaxRHTempLimit, QToHeatSetPt );
			}

			SysOutlet( SysNum ).AirMassFlowRate = MassFlow;

			UpdateSys( SysNum );

			// Now do the heating coil calculation for each heating coil type
			{ auto const SELECT_CASE_var( Sys( SysNum ).ReheatComp_Num ); // Reverse damper option is working only for water coils for now.

			// hot water heating coil
			if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
				// Determine the load required to pass to the Component controller
				// Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
				// and is working as-is, temperature setpoints are maintained as expected.
				QZnReq = QZoneMax2 + MassFlow * CpAirZn * ZoneTemp;

				// Initialize hot water flow rate to zero.
				DummyMdot = 0.0;
				SetActuatedBranchFlowRate( DummyMdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, true );
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( FirstHVACIteration ) {
					MaxFlowWater = Sys( SysNum ).MaxReheatWaterFlow;
					MinFlowWater = Sys( SysNum ).MinReheatWaterFlow;
				} else {
					WaterControlNode = Sys( SysNum ).ReheatControlNode;
					MaxFlowWater = Node( WaterControlNode ).MassFlowRateMaxAvail;
					MinFlowWater = Node( WaterControlNode ).MassFlowRateMinAvail;
				}

				// Simulate the reheat coil at constant air flow. Control by varying the
				// hot water flow rate.
				//FB use QActualHeating, change ControlCompOutput to use new
				ControlCompOutput( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatComp_Index, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatControlNode, MaxFlowWater, MinFlowWater, Sys( SysNum ).ControllerOffset, Sys( SysNum ).ControlCompTypeNum, Sys( SysNum ).CompErrIndex, _, SysOutletNode, MassFlow, _, _, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex );

				// If reverse action damper and the hot water flow is at maximum, simulate the
				// hot water coil with fixed (maximum) hot water flow but allow the air flow to
				// vary up to the maximum (air damper opens to try to meet zone load)
				if ( Sys( SysNum ).DamperHeatingAction == ReverseAction ) {
					if ( Node( Sys( SysNum ).ReheatControlNode ).MassFlowRate == MaxFlowWater ) {
						// fill limits for air flow for controller
						MinAirMassFlowRevAct = Sys( SysNum ).AirMassFlowRateMax * Sys( SysNum ).ZoneMinAirFrac;
						MinAirMassFlowRevAct = min( MinAirMassFlowRevAct, SysInlet( SysNum ).AirMassFlowRateMaxAvail );
						MinAirMassFlowRevAct = max( MinAirMassFlowRevAct, SysInlet( SysNum ).AirMassFlowRateMinAvail );

						MaxAirMassFlowRevAct = Sys( SysNum ).AirMassFlowRateMax;
						MaxAirMassFlowRevAct = min( MaxAirMassFlowRevAct, MaxDeviceAirMassFlowReheat );
						MaxAirMassFlowRevAct = max( MaxAirMassFlowRevAct, MinAirMassFlowRevAct );
						MaxAirMassFlowRevAct = min( MaxAirMassFlowRevAct, SysInlet( SysNum ).AirMassFlowRateMaxAvail );

						Node( Sys( SysNum ).OutletNodeNum ).MassFlowRateMaxAvail = MaxAirMassFlowRevAct; // suspect, check how/if used in ControlCompOutput
						ControlCompOutput( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatComp_Index, FirstHVACIteration, QZoneMax2, Sys( SysNum ).OutletNodeNum, MaxAirMassFlowRevAct, MinAirMassFlowRevAct, Sys( SysNum ).ControllerOffset, Sys( SysNum ).ControlCompTypeNum, Sys( SysNum ).CompErrIndex, ZoneNodeNum, SysOutletNode ); // why not QZnReq  ?
						// air flow controller, not on plant, don't pass plant topology info
						// reset terminal unit inlet air mass flow to new value.
						Node( Sys( SysNum ).OutletNodeNum ).MassFlowRateMaxAvail = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
						MassFlow = Node( SysOutletNode ).MassFlowRate;

						//         ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
						//         ! equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
						if ( ( ( std::abs( MassFlow - MassFlow2( SysNum ) ) < MassFlowDiff( SysNum ) ) || ( std::abs( MassFlow - MassFlow3( SysNum ) ) < MassFlowDiff( SysNum ) ) ) && ( std::abs( MassFlow - MassFlow1( SysNum ) ) >= MassFlowDiff( SysNum ) ) ) {
							if ( MassFlow > 0.0 ) MassFlow = MassFlow1( SysNum );
							SysOutlet( SysNum ).AirMassFlowRate = MassFlow;
							UpdateSys( SysNum );

							// Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
							// and is working as-is, temperature setpoints are maintained as expected.
							QZnReq = QZoneMax2 + MassFlow * CpAirZn * ZoneTemp;
							ControlCompOutput( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatComp_Index, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatControlNode, MaxFlowWater, MinFlowWater, Sys( SysNum ).ControllerOffset, Sys( SysNum ).ControlCompTypeNum, Sys( SysNum ).CompErrIndex, _, SysOutletNode, MassFlow, _, _, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex );
						}

						SysOutlet( SysNum ).AirMassFlowRate = MassFlow;
						// reset OA report variable
						UpdateSys( SysNum );
					} // IF (Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate .EQ. MaxFlowWater) THEN
				} // IF (Sys(SysNum)%DamperHeatingAction .EQ. ReverseAction) THEN

				// Recalculate the Damper Position.
				if ( MassFlow == 0.0 ) {
					Sys( SysNum ).DamperPosition = 0.0;
					Sys( SysNum ).ZoneMinAirFracReport = 0.0;
				} else if ( ( MassFlow > 0.0 ) && ( MassFlow < Sys( SysNum ).AirMassFlowRateMax ) ) {
					Sys( SysNum ).DamperPosition = MassFlow / Sys( SysNum ).AirMassFlowRateMax;
					Sys( SysNum ).ZoneMinAirFracReport = Sys( SysNum ).ZoneMinAirFrac;
				} else if ( MassFlow == Sys( SysNum ).AirMassFlowRateMax ) {
					Sys( SysNum ).DamperPosition = 1.0;
					Sys( SysNum ).ZoneMinAirFracReport = Sys( SysNum ).ZoneMinAirFrac;
				}

			} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // ! COIL:STEAM:AIRHEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QZoneMax2 - MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp );

				// Simulate reheat coil for the VAV system
				SimulateSteamCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index, QZnReq );

			} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QZoneMax2 - MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp );

				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatComp_Index );

			} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QZoneMax2 - MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp );

				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatComp_Index, QHeatingDelivered );

			} else if ( SELECT_CASE_var == HCoilType_None ) { // blank
				// I no reheat is defined then assume that the damper is the only component.
				// If something else is there that is not a reheat coil or a blank then give the error message

			} else {
				ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
			}}

			//the COIL is OFF the properties are calculated for this special case.
		} else {
			{ auto const SELECT_CASE_var( Sys( SysNum ).ReheatComp_Num );

			if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
				// Simulate reheat coil for the Const Volume system
				// Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0  !DSU
				DummyMdot = 0.0;
				SetActuatedBranchFlowRate( DummyMdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, true );
				//call the reheat coil with the NO FLOW condition to make sure that the Node values
				// are passed through to the coil outlet correctly
				SimulateWaterCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index );
			} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // COIL:STEAM:AIRHEATING
				// Simulate reheat coil for the VAV system
				SimulateSteamCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index, 0.0 );

			} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, 0.0, Sys( SysNum ).ReheatComp_Index );

			} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, 0.0, Sys( SysNum ).ReheatComp_Index );
			} else if ( SELECT_CASE_var == HCoilType_None ) { // blank
				// If no reheat is defined then assume that the damper is the only component.
				// If something else is that is not a reheat coil or a blank then give the error message

			} else {
				ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
			}}

		}

		//  set OA report variable
		Sys( SysNum ).OutdoorAirFlowRate = ( MassFlow / StdRhoAir ) * AirLoopOAFrac;

		// push the flow rate history
		MassFlow3( SysNum ) = MassFlow2( SysNum );
		MassFlow2( SysNum ) = MassFlow1( SysNum );
		MassFlow1( SysNum ) = MassFlow;

	}

	void
	CalcOAMassFlow(
		int const SysNum, // index to terminal unit
		Real64 & SAMassFlow, // outside air based on optional user input
		Real64 & AirLoopOAFrac // outside air based on optional user input
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad (FSEC)
		//       DATE WRITTEN   Jan 2010
		//       MODIFIED       Mangesh Basarkar, 06/2011: Modifying outside air based on airloop DCV flag
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates the amount of outside air required based on optional user input.
		// Zone multipliers are included and are applied in GetInput.

		// METHODOLOGY EMPLOYED:
		// User input defines method used to calculate OA.

		// REFERENCES:

		// Using/Aliasing
		using DataAirLoop::AirLoopFlow;
		using DataAirLoop::AirLoopControlInfo;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
		using Psychrometrics::PsyRhoAirFnPbTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		bool const UseMinOASchFlag( true ); // Always use min OA schedule in calculations.

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int AirLoopNum; // Index to air loop
		Real64 RhoAir; // density of terminal unit inlet air
		Real64 OAVolumeFlowRate; // outside air volume flow rate (m3/s)
		Real64 OAMassFlow; // outside air mass flow rate (kg/s)

		// initialize OA flow rate and OA report variable
		SAMassFlow = 0.0;
		AirLoopOAFrac = 0.0;
		AirLoopNum = 0;

		if ( Sys( SysNum ).CtrlZoneNum > 0 ) AirLoopNum = ZoneEquipConfig( Sys( SysNum ).CtrlZoneNum ).AirLoopNum;

		// Calculate the amount of OA based on optional user inputs
		if ( AirLoopNum > 0 ) {
			AirLoopOAFrac = AirLoopFlow( AirLoopNum ).OAFrac;
			// If no additional input from user, RETURN from subroutine
			if ( Sys( SysNum ).NoOAFlowInputFromUser ) return;
			// Calculate outdoor air flow rate, zone multipliers are applied in GetInput
			if ( AirLoopOAFrac > 0.0 ) {
				OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( Sys( SysNum ).OARequirementsPtr, Sys( SysNum ).ActualZoneNum, AirLoopControlInfo( AirLoopNum ).AirLoopDCVFlag, UseMinOASchFlag );
				RhoAir = PsyRhoAirFnPbTdbW( Node( Sys( SysNum ).InletNodeNum ).Press, Node( Sys( SysNum ).InletNodeNum ).Temp, Node( Sys( SysNum ).InletNodeNum ).HumRat );
				OAMassFlow = OAVolumeFlowRate * RhoAir;

				// convert OA mass flow rate to supply air flow rate based on air loop OA fraction
				SAMassFlow = OAMassFlow / AirLoopOAFrac;

			}

		}

	}

	void
	SimCBVAV(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2006
		//       MODIFIED       KHL/TH 10/2010: added maximum supply air temperature leaving reheat coil
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the VAV box with varying airflow in heating and cooling.
		// Modified version of SimVAV.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataHVACGlobals::SmallLoad;
		//unused   USE DataHeatBalFanSys,    ONLY: Mat
		using WaterCoils::SimulateWaterCoilComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataDefineEquip::AirDistUnit;
		//unused   USE DataHeatBalFanSys,    ONLY: ZoneThermostatSetPointHi, ZoneThermostatSetPointLo
		using PlantUtilities::SetActuatedBranchFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlow; // Total Mass Flow Rate from Hot & Cold Inlets [kg/sec]
		Real64 QTotLoad; // Total load based on thermostat setpoint temperature [Watts]
		Real64 QZnReq; // Total load to be met by terminal heater [Watts]
		Real64 QToHeatSetPt; // Remaining load to heating setpoint [W]
		Real64 QSupplyAir; // Zone load met by VAVHeatandCool system
		Real64 CpAirZn; // Specific heat of zone air [J/kg-C]
		Real64 CpAirSysIn; // Specific heat of VAVHeatandCool box entering air [J/kg-C]
		Real64 DeltaTemp; // Temperature difference multiplied by specific heat [J/kg]
		Real64 MaxFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 MinFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 LeakLoadMult; // Load multiplier to adjust for downstream leaks
		int ADUNum; // Index of air distribution unit for this terminal unit
		int SysOutletNode; // The node number of the terminal unit outlet node
		int SysInletNode; // The node number of the terminal unit inlet node
		int WaterControlNode; // This is the Actuated Reheat Control Node
		Real64 DummyMdot;
		Real64 QActualHeating;
		Real64 MinFlowFrac; // minimum flow fraction (and minimum damper position)
		static Real64 ZoneTemp( 0.0 ); // zone air temperature [C]
		static Real64 MaxHeatTemp( 0.0 ); // maximum supply air temperature [C]
		static Real64 MassFlowReq( 0.0 ); // air mass flow rate required to meet the coil heating load [W]
		static Real64 MassFlowActual( 0.0 ); // air mass flow rate actually used [W]
		static Real64 QZoneMax( 0.0 ); // maximum zone heat addition rate given constraints of MaxHeatTemp and max
		// available air mass flow rate [W]
		static Real64 MinMassAirFlow( 0.0 ); // the air flow rate during heating for normal acting damper
		static Real64 QZoneMax2( 0.0 ); // temporary variable
		static Real64 QZoneMax3( 0.0 ); // temporary variable

		// Sys(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
		// Sys(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
		// Sys(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

		// The calculated load from the Heat Balance
		ADUNum = Sys( SysNum ).ADUNum;
		LeakLoadMult = AirDistUnit( ADUNum ).LeakLoadMult;
		QTotLoad = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired * LeakLoadMult;
		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP * LeakLoadMult;
		SysOutletNode = Sys( SysNum ).ReheatAirOutletNode;
		SysInletNode = Sys( SysNum ).InletNodeNum;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
		MinFlowFrac = Sys( SysNum ).ZoneMinAirFrac;
		MinMassAirFlow = MinFlowFrac * StdRhoAir * Sys( SysNum ).MaxAirVolFlowRate;
		ZoneTemp = Node( ZoneNodeNum ).Temp;

		//Then depending on if the Load is for heating or cooling it is handled differently.  First
		// the massflow rate for cooling is determined to meet the entire load.  Then
		// if the massflow is below the minimum or greater than the Max it is set to either the Min
		// or the Max as specified for the VAV model.
		if ( SysInlet( SysNum ).AirMassFlowRateMaxAvail > 0.0 ) {
			// Calculate the flow required for cooling
			CpAirSysIn = PsyCpAirFnWTdb( SysInlet( SysNum ).AirHumRat, SysInlet( SysNum ).AirTemp );
			DeltaTemp = CpAirSysIn * SysInlet( SysNum ).AirTemp - CpAirZn * ZoneTemp;

			//Need to check DeltaTemp and ensure that it is not zero
			if ( DeltaTemp != 0.0 ) {
				MassFlow = QTotLoad / DeltaTemp;
			} else {
				MassFlow = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
			}

			//Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
			MassFlow = max( MassFlow, SysInlet( SysNum ).AirMassFlowRateMinAvail );
			MassFlow = min( MassFlow, SysInlet( SysNum ).AirMassFlowRateMaxAvail );
		} else {
			// System is Off set massflow to 0.0
			MassFlow = 0.0;
		}
		// look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
		// equipment iteration. If detected, set flow rate to previous value.
		if ( ( ( std::abs( MassFlow - MassFlow2( SysNum ) ) < MassFlowDiff( SysNum ) ) || ( std::abs( MassFlow - MassFlow3( SysNum ) ) < MassFlowDiff( SysNum ) ) ) && ( std::abs( MassFlow - MassFlow1( SysNum ) ) >= MassFlowDiff( SysNum ) ) ) {
			MassFlow = MassFlow1( SysNum );
		}

		//Move data to the damper outlet node
		SysOutlet( SysNum ).AirTemp = SysInlet( SysNum ).AirTemp;
		SysOutlet( SysNum ).AirHumRat = SysInlet( SysNum ).AirHumRat;
		SysOutlet( SysNum ).AirMassFlowRate = MassFlow;
		SysOutlet( SysNum ).AirMassFlowRateMaxAvail = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
		SysOutlet( SysNum ).AirMassFlowRateMinAvail = SysInlet( SysNum ).AirMassFlowRateMinAvail;
		SysOutlet( SysNum ).AirEnthalpy = SysInlet( SysNum ).AirEnthalpy;

		// Calculate the Damper Position when there is a Max air flow specified.
		if ( Sys( SysNum ).AirMassFlowRateMax == 0.0 ) {
			Sys( SysNum ).DamperPosition = 0.0;
		} else {
			Sys( SysNum ).DamperPosition = MassFlow / Sys( SysNum ).AirMassFlowRateMax;
		}

		//Need to make sure that the damper outlets are passed to the coil inlet
		UpdateSys( SysNum );

		QActualHeating = QToHeatSetPt - MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp );

		if ( ( MassFlow > SmallMassFlow ) && ( QActualHeating > 0.0 ) && ( TempControlType( ZoneNum ) != SingleCoolingSetPoint ) ) {
			//   VAVHeatandCool boxes operate at varying mass flow rates when reheating, VAV boxes operate at min flow
			//      (MassFlow <= SysInlet(SysNum)%AirMassFlowRateMinAvail) .AND. &
			//   Per Fred Buhl, don't use DeadBandOrSetback to determine if heaters operate
			//      (.NOT. DeadBandOrSetback(ZoneNum))) Then

			// At this point we know that there is a heating requirement: i.e., the heating coil needs to
			// be activated (there's a zone heating load or there's a reheat requirement). There are 3 possible
			// situations: 1) the coil load can be met by variable temperature air (below the max heat temp) at
			// the minimum air mass flow rate; 2) the coil load can be met by variable air flow rate with the air
			// temperature fixed at the max heat temp; 3) the load cannot be met (we will run at max air temp and
			// max air flow rate). We check for condition 2 by assuming the air temperatute is at the max heat temp
			// and solving for the air mass flow rate that will meet the load. If the flow rate is between the min and
			// max we are in condition 2.

			QZoneMax2 = QToHeatSetPt;

			if ( Sys( SysNum ).MaxReheatTempSetByUser ) {

				MaxHeatTemp = Sys( SysNum ).MaxReheatTemp;
				if ( QToHeatSetPt > SmallLoad ) { // zone has a postive load to heating setpoint
					MassFlowReq = QToHeatSetPt / ( CpAirZn * ( MaxHeatTemp - ZoneTemp ) );
				} else {
					MassFlowReq = MassFlow;
				}

				QZoneMax3 = CpAirZn * ( MaxHeatTemp - ZoneTemp ) * MassFlow;

				MassFlowActual = MassFlow;

				if ( QZoneMax3 < QToHeatSetPt ) {
					MassFlowActual = MassFlowReq;
					// QZoneMax3 = CpAirZn * (MaxHeatTemp - ZoneTemp) * MassFlowActual
				}

				if ( MassFlowActual <= MinMassAirFlow ) {
					MassFlowActual = MinMassAirFlow;
				} else if ( MassFlowActual >= Sys( SysNum ).AirMassFlowRateMax ) {
					MassFlowActual = Sys( SysNum ).AirMassFlowRateMax;
				}

				QZoneMax = CpAirZn * MassFlowActual * ( MaxHeatTemp - ZoneTemp );

				// temporary variable
				QZoneMax2 = min( QZoneMax, QToHeatSetPt );

				MassFlow = MassFlowActual;

			} // IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN

			SysOutlet( SysNum ).AirMassFlowRate = MassFlow;

			UpdateSys( SysNum );

			{ auto const SELECT_CASE_var( Sys( SysNum ).ReheatComp_Num );

			// hot water heating coil
			if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
				// Determine the load required to pass to the Component controller
				// Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
				// and is working as-is, temperature setpoints are maintained as expected.
				QZnReq = QZoneMax2 + MassFlow * CpAirZn * Node( ZoneNodeNum ).Temp;
				if ( QZnReq < SmallLoad ) QZnReq = 0.0;

				// Initialize hot water flow rate to zero.
				// Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
				DummyMdot = 0.0;
				SetActuatedBranchFlowRate( DummyMdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, true );
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( FirstHVACIteration ) {
					MaxFlowWater = Sys( SysNum ).MaxReheatWaterFlow;
					MinFlowWater = Sys( SysNum ).MinReheatWaterFlow;
				} else {
					WaterControlNode = Sys( SysNum ).ReheatControlNode;
					MaxFlowWater = Node( WaterControlNode ).MassFlowRateMaxAvail;
					MinFlowWater = Node( WaterControlNode ).MassFlowRateMinAvail;
				}

				// Simulate the reheat coil at constant air flow. Control by varying the
				// hot water flow rate.
				ControlCompOutput( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatComp_Index, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatControlNode, MaxFlowWater, MinFlowWater, Sys( SysNum ).ControllerOffset, Sys( SysNum ).ControlCompTypeNum, Sys( SysNum ).CompErrIndex, _, SysOutletNode, MassFlow, _, _, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex );

				// If reverse action damper and the hot water flow is at maximum, simulate the
				// hot water coil with fixed (maximum) hot water flow but allow the air flow to
				// vary up to the maximum (air damper opens to try to meet zone load).
				if ( Sys( SysNum ).DamperHeatingAction == ReverseAction ) {
					if ( Node( Sys( SysNum ).ReheatControlNode ).MassFlowRate == Sys( SysNum ).MaxReheatWaterFlow ) {
						ControlCompOutput( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatComp_Index, FirstHVACIteration, QZoneMax2, Sys( SysNum ).OutletNodeNum, SysInlet( SysNum ).AirMassFlowRateMaxAvail, SysInlet( SysNum ).AirMassFlowRateMinAvail, Sys( SysNum ).ControllerOffset, Sys( SysNum ).ControlCompTypeNum, Sys( SysNum ).CompErrIndex, ZoneNodeNum, SysOutletNode );
						//                                   ! air flow controller, not on plant, don't pass plant topology info

						// reset terminal unit inlet air mass flow to new value.
						MassFlow = Node( SysOutletNode ).MassFlowRate;
						SysOutlet( SysNum ).AirMassFlowRate = MassFlow;
						UpdateSys( SysNum );
					}
					// look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
					// equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
					if ( ( ( std::abs( MassFlow - MassFlow2( SysNum ) ) < MassFlowDiff( SysNum ) ) || ( std::abs( MassFlow - MassFlow3( SysNum ) ) < MassFlowDiff( SysNum ) ) ) && ( std::abs( MassFlow - MassFlow1( SysNum ) ) >= MassFlowDiff( SysNum ) ) ) {
						MassFlow = MassFlow1( SysNum );
						SysOutlet( SysNum ).AirMassFlowRate = MassFlow;
						UpdateSys( SysNum );
						ControlCompOutput( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatComp_Index, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatControlNode, MaxFlowWater, MinFlowWater, Sys( SysNum ).ControllerOffset, Sys( SysNum ).ControlCompTypeNum, Sys( SysNum ).CompErrIndex, _, SysOutletNode, MassFlow, _, _, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex );

					}
					// recalculate damper position
					if ( Sys( SysNum ).AirMassFlowRateMax == 0.0 ) {
						Sys( SysNum ).DamperPosition = 0.0;
					} else {
						Sys( SysNum ).DamperPosition = MassFlow / Sys( SysNum ).AirMassFlowRateMax;
					}
				}
			} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // ! COIL:STEAM:AIRHEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QZoneMax2 - MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp );
				if ( QZnReq < SmallLoad ) QZnReq = 0.0;

				// Simulate reheat coil for the VAV system
				SimulateSteamCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index, QZnReq );

			} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
				// Determine the load required to pass to the Component controller
				QSupplyAir = MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp );
				QZnReq = QZoneMax2 - QSupplyAir;
				if ( QZnReq < SmallLoad ) QZnReq = 0.0;

				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatComp_Index );

			} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QZoneMax2 - MassFlow * CpAirZn * ( SysInlet( SysNum ).AirTemp - ZoneTemp );
				if ( QZnReq < SmallLoad ) QZnReq = 0.0;

				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatComp_Index );
			} else if ( SELECT_CASE_var == HCoilType_None ) { // blank
				// If no reheat is defined then assume that the damper is the only component.
				// If something else is there that is not a reheat coil then give the error message below.

			} else {
				ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
			}}

			//the COIL is OFF the properties are calculated for this special case.
		} else {
			{ auto const SELECT_CASE_var( Sys( SysNum ).ReheatComp_Num );

			if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
				// Simulate reheat coil for the Const Volume system
				// Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
				// Initialize hot water flow rate to zero.
				DummyMdot = 0.0;
				SetActuatedBranchFlowRate( DummyMdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, true );

				//call the reheat coil with the NO FLOW condition to make sure that the Node values
				// are passed through to the coil outlet correctly
				SimulateWaterCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index );
			} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // COIL:STEAM:AIRHEATING
				// Simulate reheat coil for the VAV system
				SimulateSteamCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index, 0.0 );

			} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, 0.0, Sys( SysNum ).ReheatComp_Index );

			} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, 0.0, Sys( SysNum ).ReheatComp_Index );
			} else if ( SELECT_CASE_var == HCoilType_None ) { // blank
				// If no reheat is defined then assume that the damper is the only component.
				// If something else is there that is not a reheat coil then give the error message

			} else {
				ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
			}}

		}
		// push the flow rate history
		MassFlow3( SysNum ) = MassFlow2( SysNum );
		MassFlow2( SysNum ) = MassFlow1( SysNum );
		MassFlow1( SysNum ) = MassFlow;

	}

	void
	SimVAVVS(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates a single duct VAV terminal unit with a variable-speed fan upstream
		// and a reheat coil on the downstream side.

		// METHODOLOGY EMPLOYED:
		// Define a compound component in CalcVAVVS. Break the heating/cooling load into 4 regions based
		// on equip on/off combinations. Assign the heating load to the appropriate region and iteratively
		// solve for the appropriate control variable value using Regula-Falsi solver.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataConvergParams::HVACFlowRateToler;
		using General::SolveRegulaFalsi;
		using SteamCoils::GetCoilCapacity;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const BigLoad( 1.0e+20 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlow; // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
		Real64 QTotLoad; // [Watts]
		//unused  REAL(r64) :: QZnReq      ! [Watts]
		Real64 CpAirZn;
		//unused  REAL(r64) :: CpAirSysIn
		//unused  REAL(r64) :: DeltaTemp
		int SysOutletNode; // The node number of the terminal unit outlet node
		int SysInletNode; // the node number of the terminal unit inlet node
		int WaterControlNode; // This is the Actuated Reheat Control Node
		int SteamControlNode;
		Real64 MaxFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 MinFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 MaxFlowSteam; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 MinFlowSteam; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 HWFlow; // the hot water flow rate [kg/s]
		Real64 QCoolFanOnMax; // max cooling - fan at max flow; note that cooling is always < 0. [W]
		Real64 QCoolFanOnMin; // min active cooling with fan on - fan at lowest speed. [W]
		Real64 QHeatFanOnMax; // max heating - fan at heat flow max, hot water flow at max [W]
		Real64 QHeatFanOnMin; // min heating - fan at min flow, hot water at max flow [W]
		Real64 QHeatFanOffMax; // max heating - fan off, hot water flow at max [W]
		Real64 QNoHeatFanOff; // min heating - fan off, hot water at min flow [W]
		int HCType; // heating coil type (as a number)
		int FanType; // fan type (as a number)
		Real64 HCLoad; // load passed to a gas or electric heating coil [W]
		int FanOp; // 1 if fan is on; 0 if off.
		Real64 MaxCoolMassFlow; // air flow at max cooling [kg/s]
		Real64 MaxHeatMassFlow; // air flow at max heating [kg/s]
		Real64 MinMassFlow; // minimum air flow rate [kg/s]
		Real64 UnitFlowToler; // flow rate tolerance
		Real64 QDelivered;
		Real64 FracDelivered;
		Array1D< Real64 > Par( 11 );
		int SolFlag;
		Real64 ErrTolerance;
		Real64 MaxSteamCap; // steam coil capacity at full load
		bool ErrorsFound; // returned from mining function call

		// The calculated load from the Heat Balance
		QTotLoad = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		SysOutletNode = Sys( SysNum ).ReheatAirOutletNode;
		SysInletNode = Sys( SysNum ).InletNodeNum;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
		HCType = Sys( SysNum ).ReheatComp_Num;
		FanType = Sys( SysNum ).Fan_Num;
		MaxCoolMassFlow = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
		MaxHeatMassFlow = min( Sys( SysNum ).HeatAirMassFlowRateMax, SysInlet( SysNum ).AirMassFlowRateMaxAvail );
		MinMassFlow = MaxCoolMassFlow * Sys( SysNum ).ZoneMinAirFrac;
		UnitFlowToler = 0.001 * HVACFlowRateToler;
		QDelivered = 0.0;
		HWFlow = 0.0;
		if ( SysInlet( SysNum ).AirMassFlowRateMaxAvail <= 0.0 || CurDeadBandOrSetback( ZoneNum ) ) {
			MassFlow = 0.0;
			FanOp = 0;
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, 0.0, FanType, MassFlow, FanOp, QDelivered );
			return;
		}

		if ( HCType == HCoilType_SimpleHeating ) {
			WaterControlNode = Sys( SysNum ).ReheatControlNode;
			HCLoad = 0.0;
			if ( FirstHVACIteration ) {
				MaxFlowWater = Sys( SysNum ).MaxReheatWaterFlow;
				MinFlowWater = Sys( SysNum ).MinReheatWaterFlow;
			} else {
				WaterControlNode = Sys( SysNum ).ReheatControlNode;
				MaxFlowWater = Node( WaterControlNode ).MassFlowRateMaxAvail;
				MinFlowWater = Node( WaterControlNode ).MassFlowRateMinAvail;
			}
		} else {
			WaterControlNode = 0;
			HCLoad = BigLoad;
			MaxFlowWater = 0.0;
			MinFlowWater = 0.0;
		}

		if ( HCType == HCoilType_SteamAirHeating ) {
			SteamControlNode = Sys( SysNum ).ReheatControlNode;
			HCLoad = 0.0;
			if ( FirstHVACIteration ) {
				MaxFlowSteam = Sys( SysNum ).MaxReheatSteamFlow;
				MinFlowSteam = Sys( SysNum ).MinReheatSteamFlow;
			} else {
				SteamControlNode = Sys( SysNum ).ReheatControlNode;
				MaxFlowSteam = Node( SteamControlNode ).MassFlowRateMaxAvail;
				MinFlowSteam = Node( SteamControlNode ).MassFlowRateMinAvail;
			}
		} else {
			SteamControlNode = 0;
			HCLoad = BigLoad;
			MaxFlowSteam = 0.0;
			MinFlowSteam = 0.0;
		}

		// define 3 load regions and assign the current load to the correct region.
		// region 1: active cooling with fan on
		FanOp = 1;
		if ( HCType == HCoilType_SteamAirHeating ) {
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MaxCoolMassFlow, FanOp, QCoolFanOnMax );
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QCoolFanOnMin );
			// region 2: active heating with fan on
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowSteam, BigLoad, FanType, MaxHeatMassFlow, FanOp, QHeatFanOnMax );
			MaxSteamCap = GetCoilCapacity( Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatName, ErrorsFound );
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QHeatFanOnMin );
			// region 3: active heating with fan off
			FanOp = 0;
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowSteam, BigLoad, FanType, MinMassFlow, FanOp, QHeatFanOffMax );
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QNoHeatFanOff );
		} else {
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MaxCoolMassFlow, FanOp, QCoolFanOnMax );
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MinMassFlow, FanOp, QCoolFanOnMin );
			// region 2: active heating with fan on
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, BigLoad, FanType, MaxHeatMassFlow, FanOp, QHeatFanOnMax );
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MinMassFlow, FanOp, QHeatFanOnMin );
			// region 3: active heating with fan off
			FanOp = 0;
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, BigLoad, FanType, MinMassFlow, FanOp, QHeatFanOffMax );
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MinMassFlow, FanOp, QNoHeatFanOff );
		}

		// Active cooling
		if ( QTotLoad < QCoolFanOnMin - SmallLoad && SysInlet( SysNum ).AirMassFlowRateMaxAvail > 0.0 && ! CurDeadBandOrSetback( ZoneNum ) ) {
			// check that it can meet the load
			FanOp = 1;
			if ( QCoolFanOnMax < QTotLoad - SmallLoad ) {
				Par( 1 ) = double( SysNum );
				if ( FirstHVACIteration ) {
					Par( 2 ) = 1.0;
				} else {
					Par( 2 ) = 0.0;
				}
				Par( 3 ) = double( ZoneNodeNum );
				Par( 4 ) = double( HCType );
				if ( HCType == HCoilType_SteamAirHeating ) {
					Par( 5 ) = MinFlowSteam;
				} else {
					Par( 5 ) = MinFlowWater;
				}
				Par( 6 ) = double( FanType );
				Par( 7 ) = double( FanOp );
				Par( 8 ) = QTotLoad;
				SolveRegulaFalsi( UnitFlowToler, 50, SolFlag, MassFlow, VAVVSCoolingResidual, MinMassFlow, MaxCoolMassFlow, Par );
				if ( SolFlag == -1 ) {
					if ( Sys( SysNum ).IterationLimit == 0 ) {
						ShowWarningError( "Supply air flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
						ShowContinueError( "  Iteration limit exceeded in calculating air flow rate" );
					}
					ShowRecurringWarningErrorAtEnd( "Supply air flow Iteration limit exceeded in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationLimit );
				} else if ( SolFlag == -2 ) {
					if ( Sys( SysNum ).IterationFailed == 0 ) {
						ShowWarningError( "Supply air flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
						ShowContinueError( "  Bad air flow limits" );
					}
					ShowRecurringWarningErrorAtEnd( "Supply air flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationFailed );
				}

			} else {

				MassFlow = MaxCoolMassFlow;

				if ( HCType == HCoilType_SteamAirHeating ) {
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MassFlow, FanOp, QDelivered );
				} else {
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered );
				}

			}

			// no active heating or cooling
		} else if ( ( QTotLoad >= QCoolFanOnMin - SmallLoad && QTotLoad <= QNoHeatFanOff + SmallLoad && SysInlet( SysNum ).AirMassFlowRateMaxAvail > 0.0 ) || ( SysInlet( SysNum ).AirMassFlowRateMaxAvail > 0.0 && CurDeadBandOrSetback( ZoneNum ) ) ) {
			MassFlow = MinMassFlow;
			FanOp = 0;
			if ( HCType == HCoilType_SteamAirHeating ) {
				CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, QTotLoad, FanType, MassFlow, FanOp, QNoHeatFanOff );
			} else {
				CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MassFlow, FanOp, QNoHeatFanOff );
			}

			// active heating
		} else if ( QTotLoad > QNoHeatFanOff + SmallLoad && SysInlet( SysNum ).AirMassFlowRateMaxAvail > 0.0 && ! CurDeadBandOrSetback( ZoneNum ) ) {
			// hot water coil
			if ( HCType == HCoilType_SimpleHeating ) {
				if ( QTotLoad < QHeatFanOffMax - SmallLoad ) {
					// vary HW flow, leave air flow at minimum
					MassFlow = MinMassFlow;
					FanOp = 0;
					Par( 1 ) = double( SysNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = double( ZoneNodeNum );
					Par( 4 ) = double( HCType );
					Par( 5 ) = MassFlow;
					Par( 6 ) = double( FanType );
					Par( 7 ) = double( FanOp );
					Par( 8 ) = QTotLoad;
					ErrTolerance = Sys( SysNum ).ControllerOffset;
					SolveRegulaFalsi( ErrTolerance, 500, SolFlag, HWFlow, VAVVSHWNoFanResidual, MinFlowWater, MaxFlowWater, Par );
					if ( SolFlag == -1 ) {
						ShowRecurringWarningErrorAtEnd( "Hot Water flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).ErrCount1 );
						ShowRecurringContinueErrorAtEnd( "...Iteration limit (500) exceeded in calculating the hot water flow rate", Sys( SysNum ).ErrCount1c );
						CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, HWFlow, 0.0, FanType, MassFlow, FanOp, QDelivered );
					} else if ( SolFlag == -2 ) {
						ShowRecurringWarningErrorAtEnd( "Hot Water flow control failed (bad air flow limits) in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).ErrCount2 );
					}
				} else if ( QTotLoad >= QHeatFanOffMax - SmallLoad && QTotLoad <= QHeatFanOnMin + SmallLoad ) {
					MassFlow = MinMassFlow;
					FanOp = 0;
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered );
				} else if ( QTotLoad > QHeatFanOnMin + SmallLoad && QTotLoad < QHeatFanOnMax - SmallLoad ) {
					// set hot water flow to max and vary the supply air flow rate
					FanOp = 1;
					Par( 1 ) = double( SysNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = double( ZoneNodeNum );
					Par( 4 ) = double( HCType );
					Par( 5 ) = MaxFlowWater;
					Par( 6 ) = double( FanType );
					Par( 7 ) = double( FanOp );
					Par( 8 ) = QTotLoad;
					SolveRegulaFalsi( UnitFlowToler, 50, SolFlag, MassFlow, VAVVSHWFanOnResidual, MinMassFlow, MaxHeatMassFlow, Par );
					if ( SolFlag == -1 ) {
						if ( Sys( SysNum ).IterationLimit == 0 ) {
							ShowWarningError( "Supply air flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
							ShowContinueError( "  Iteration limit exceeded in calculating air flow rate" );
						}
						ShowRecurringWarningErrorAtEnd( "Supply air flow Iteration limit exceeded in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationLimit );
					} else if ( SolFlag == -2 ) {
						if ( Sys( SysNum ).IterationFailed == 0 ) {
							ShowWarningError( "Supply air flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
							ShowContinueError( "  Bad air flow limits" );
						}
						ShowRecurringWarningErrorAtEnd( "Supply air flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationFailed );
					}
				} else {
					MassFlow = MaxHeatMassFlow;
					FanOp = 1;
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered );
				}
			} else if ( HCType == HCoilType_SteamAirHeating ) {
				//      IF (QTotLoad > QNoHeatFanOff + SmallLoad .AND. QTotLoad < QHeatFanOffMax - SmallLoad) THEN
				if ( QTotLoad < QHeatFanOffMax - SmallLoad ) {
					// vary steam flow, leave air flow at minimum
					MassFlow = MinMassFlow;
					FanOp = 0;
					Par( 1 ) = double( SysNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = double( ZoneNodeNum );
					Par( 4 ) = double( HCType );
					Par( 5 ) = MassFlow;
					Par( 6 ) = double( FanType );
					Par( 7 ) = double( FanOp );
					Par( 8 ) = QTotLoad;
					Par( 9 ) = MinFlowSteam;
					Par( 10 ) = MaxFlowSteam;
					Par( 11 ) = MaxSteamCap;
					ErrTolerance = Sys( SysNum ).ControllerOffset;
					SolveRegulaFalsi( ErrTolerance, 500, SolFlag, HWFlow, VAVVSHWNoFanResidual, MinFlowSteam, MaxFlowSteam, Par );
					if ( SolFlag == -1 ) {
						ShowRecurringWarningErrorAtEnd( "Steam flow control failed in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).ErrCount1 );
						ShowRecurringContinueErrorAtEnd( "...Iteration limit (500) exceeded in calculating the hot water flow rate", Sys( SysNum ).ErrCount1c );
						CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, HWFlow, 0.0, FanType, MassFlow, FanOp, QDelivered );
					} else if ( SolFlag == -2 ) {
						ShowRecurringWarningErrorAtEnd( "Steam flow control failed (bad air flow limits) in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).ErrCount2 );
					}
				} else if ( QTotLoad >= QHeatFanOffMax - SmallLoad && QTotLoad <= QHeatFanOnMin + SmallLoad ) {
					MassFlow = MinMassFlow;
					FanOp = 0;
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered );
				} else if ( QTotLoad > QHeatFanOnMin + SmallLoad && QTotLoad < QHeatFanOnMax - SmallLoad ) {
					FanOp = 1;
					Par( 1 ) = double( SysNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = double( ZoneNodeNum );
					Par( 4 ) = double( HCType );
					Par( 5 ) = MaxFlowSteam;
					Par( 6 ) = double( FanType );
					Par( 7 ) = double( FanOp );
					Par( 8 ) = QTotLoad;
					SolveRegulaFalsi( UnitFlowToler, 50, SolFlag, MassFlow, VAVVSHWFanOnResidual, MinMassFlow, MaxHeatMassFlow, Par );
					if ( SolFlag == -1 ) {
						if ( Sys( SysNum ).IterationLimit == 0 ) {
							ShowWarningError( "Steam heating coil control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
							ShowContinueError( "  Iteration limit exceeded in calculating air flow rate" );
						}
						ShowRecurringWarningErrorAtEnd( "Steam heating coil iteration limit exceeded in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationLimit );
					} else if ( SolFlag == -2 ) {
						if ( Sys( SysNum ).IterationFailed == 0 ) {
							ShowWarningError( "Steam heating coil control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
							ShowContinueError( "  Bad air flow limits" );
						}
						ShowRecurringWarningErrorAtEnd( "Steam heating coil control failed in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationFailed );
					}
				} else {
					MassFlow = MaxHeatMassFlow;
					FanOp = 1;
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, QTotLoad, QTotLoad, FanType, MassFlow, FanOp, QDelivered );
				}
			} else if ( HCType == HCoilType_Gas || HCType == HCoilType_Electric ) {
				if ( QTotLoad <= QHeatFanOnMin + SmallLoad ) {
					// vary heating coil power, leave mass flow at minimum
					MassFlow = MinMassFlow;
					FanOp = 0;
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, QTotLoad, FanType, MassFlow, FanOp, QDelivered );
				} else if ( QTotLoad > QHeatFanOnMin + SmallLoad && QTotLoad < QHeatFanOnMax - SmallLoad ) {
					FanOp = 1;
					Par( 1 ) = double( SysNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = double( ZoneNodeNum );
					Par( 4 ) = double( HCType );
					Par( 5 ) = Sys( SysNum ).ReheatCoilMaxCapacity;
					Par( 6 ) = double( FanType );
					Par( 7 ) = double( FanOp );
					Par( 8 ) = QTotLoad;
					SolveRegulaFalsi( UnitFlowToler, 50, SolFlag, FracDelivered, VAVVSHCFanOnResidual, 0.0, 1.0, Par );
					if ( SolFlag == -1 ) {
						if ( Sys( SysNum ).IterationLimit == 0 ) {
							ShowWarningError( "Heating coil control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
							ShowContinueError( "  Iteration limit exceeded in calculating air flow rate" );
						}
						ShowRecurringWarningErrorAtEnd( "Heating coil control iteration limit exceeded in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationLimit );
					} else if ( SolFlag == -2 ) {
						if ( Sys( SysNum ).IterationFailed == 0 ) {
							ShowWarningError( "Heating coil control failed in VS VAV terminal unit " + Sys( SysNum ).SysName );
							ShowContinueError( "  Bad air flow limits" );
						}
						ShowRecurringWarningErrorAtEnd( "Heating coil control failed in VS VAV terminal unit " + Sys( SysNum ).SysName, Sys( SysNum ).IterationFailed );
					}
				} else {
					MassFlow = MaxHeatMassFlow;
					FanOp = 1;
					CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, QTotLoad, FanType, MassFlow, FanOp, QDelivered );
				}
			} else {
				ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
			}

		} else {

			MassFlow = 0.0;
			FanOp = 0;
			CalcVAVVS( SysNum, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, 0.0, FanType, MassFlow, FanOp, QDelivered );

		}

	}

	void
	SimConstVol(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   February 2000
		//       MODIFIED       FB/KHL/TH 2/2011: added maximum supply air temperature leaving reheat coil
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple single duct constant volume systems.

		// METHODOLOGY EMPLOYED:
		// There is method to this madness.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		//unused   USE DataHeatBalFanSys, ONLY: Mat
		using WaterCoils::SimulateWaterCoilComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetActuatedBranchFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlow; // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
		Real64 QZnReq; // [Watts]
		Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
		Real64 CpAir;
		int WaterControlNode; // This is the Actuated Reheat Control Node
		Real64 MaxFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 MinFlowWater; // This is the value passed to the Controller depending if FirstHVACIteration or not
		Real64 QActualHeating; // the heating load seen by the reheat coil
		static Real64 TAirMax( 0.0 ); // Maximum zone supply air temperature [C]
		static Real64 QMax( 0.0 ); // Maximum heat addition rate imposed by the max zone supply air temperature [W]
		static Real64 ZoneTemp( 0.0 ); // Zone temperature [C]
		static Real64 QMax2( 0.0 );
		Real64 DummyMdot; // local fluid mass flow rate

		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP; // The calculated load from the Heat Balance
		MassFlow = SysInlet( SysNum ).AirMassFlowRateMaxAvail; // System massflow is set to the Available
		QMax2 = QToHeatSetPt;
		ZoneTemp = Node( ZoneNodeNum ).Temp;
		CpAir = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, ZoneTemp ); // zone air specific heat
		if ( Sys( SysNum ).MaxReheatTempSetByUser ) {
			TAirMax = Sys( SysNum ).MaxReheatTemp;
			QMax = CpAir * MassFlow * ( TAirMax - ZoneTemp );
			QMax2 = min( QToHeatSetPt, QMax );
		} // IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN

		if ( ( ( SysInlet( SysNum ).AirMassFlowRateMaxAvail == 0.0 ) && ( SysInlet( SysNum ).AirMassFlowRateMinAvail == 0.0 ) ) || ( SysInlet( SysNum ).AirMassFlowRate == 0.0 ) ) {
			// System is Off set massflow to 0.0
			MassFlow = 0.0;
		}

		// Calculate the Damper Position when there is a Max air flow specified.
		if ( Sys( SysNum ).AirMassFlowRateMax == 0.0 ) {
			Sys( SysNum ).DamperPosition = 0.0;
		} else {
			Sys( SysNum ).DamperPosition = MassFlow / Sys( SysNum ).AirMassFlowRateMax;
		}

		// make sure the inlet node flow rate is updated if the mass flow has been limited
		SysOutlet( SysNum ).AirMassFlowRate = MassFlow;
		SysOutlet( SysNum ).AirMassFlowRateMaxAvail = SysInlet( SysNum ).AirMassFlowRateMaxAvail;
		SysOutlet( SysNum ).AirMassFlowRateMinAvail = SysInlet( SysNum ).AirMassFlowRateMinAvail;
		UpdateSys( SysNum );

		QActualHeating = QToHeatSetPt - MassFlow * CpAir * ( SysInlet( SysNum ).AirTemp - ZoneTemp ); // reheat needed
		//Now the massflow for reheating has been determined. If it is zero, or in SetBack, or the
		// system scheduled OFF then not operational and shut the system down.
		if ( ( MassFlow > SmallMassFlow ) && ( QActualHeating > 0.0 ) && ( TempControlType( ZoneNum ) != SingleCoolingSetPoint ) ) {

			{ auto const SELECT_CASE_var( Sys( SysNum ).ReheatComp_Num );

			if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QMax2 + MassFlow * CpAir * ZoneTemp;

				//Before Iterating through the Reheat Coil and Controller set the flags for the
				// Do Loop to initialized conditions.
				// Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
				// Initialize hot water flow rate to zero.
				DummyMdot = 0.0;
				SetActuatedBranchFlowRate( DummyMdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, true );

				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( FirstHVACIteration ) {
					MaxFlowWater = Sys( SysNum ).MaxReheatWaterFlow;
					MinFlowWater = Sys( SysNum ).MinReheatWaterFlow;
				} else {
					WaterControlNode = Sys( SysNum ).ReheatControlNode;
					MaxFlowWater = Node( WaterControlNode ).MassFlowRateMaxAvail;
					MinFlowWater = Node( WaterControlNode ).MassFlowRateMinAvail;
				}

				// Simulate reheat coil for the Const Volume system
				// Set Converged to True & when controller is not converged it will set to False.
				ControlCompOutput( Sys( SysNum ).ReheatName, Sys( SysNum ).ReheatComp, Sys( SysNum ).ReheatComp_Index, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatControlNode, MaxFlowWater, MinFlowWater, Sys( SysNum ).ControllerOffset, Sys( SysNum ).ControlCompTypeNum, Sys( SysNum ).CompErrIndex, _, Sys( SysNum ).ReheatAirOutletNode, MassFlow, _, _, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex );

			} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // COIL:STEAM:STEAMAIRHEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QMax2 - MassFlow * CpAir * ( SysInlet( SysNum ).AirTemp - ZoneTemp );

				// Simulate reheat coil for the VAV system
				SimulateSteamCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index, QZnReq );
			} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QMax2 - MassFlow * CpAir * ( SysInlet( SysNum ).AirTemp - ZoneTemp );

				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatComp_Index );

			} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
				// Determine the load required to pass to the Component controller
				QZnReq = QMax2 - MassFlow * CpAir * ( SysInlet( SysNum ).AirTemp - ZoneTemp );

				// Simulate reheat coil for the VAV system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, QZnReq, Sys( SysNum ).ReheatComp_Index );
			} else {
				ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
			}}

			//the COIL is OFF the properties are calculated for this special case.
		} else {
			{ auto const SELECT_CASE_var( Sys( SysNum ).ReheatComp_Num );

			if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
				// Simulate reheat coil for the Const Volume system
				//Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
				// Initialize hot water flow rate to zero.
				DummyMdot = 0.0;
				SetActuatedBranchFlowRate( DummyMdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, true );

				//call the reheat coil with the NO FLOW condition to make sure that the Node values
				// are passed through to the coil outlet correctly
				SimulateWaterCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index );
			} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // COIL:STEAM:AIRHEATING
				// Simulate reheat coil for the Const Volume system
				SimulateSteamCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index, 0.0 );

			} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
				// Simulate reheat coil for the Const Volume system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, 0.0, Sys( SysNum ).ReheatComp_Index );

			} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
				// Simulate reheat coil for the Const Volume system
				SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, 0.0, Sys( SysNum ).ReheatComp_Index );
			} else {
				ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
			}}

		}

		//Debugging output for model
		//If((HourOfDay .ge. 8) .and. (HourOfDay .lt. 15)) Then
		//      Write(OutputFileDebug,*)  'Day of Sim     Hour of Day    Time'
		//      Write(OutputFileDebug,*)  DayOfSim, HourOfDay, TimeStep*TimeStepZone
		//      Write(OutputFileDebug,10)
		//      Write(OutputFileDebug,20)ZoneNum, SysInlet(SysNum)%AirMassFlowRate, &
		//                             SysInlet(SysNum)%AirMassFlowRate, &
		//                              Temperature, Mat(ZoneNum), Node(ZoneNodeNum)%Temp, QTotLoad, &
		//                             Enthalpy
		//End If
		//10 Format('ZoneNum    SysHot    SysCold   Temp  &
		//      &    MAT        NodeZoneTemp    QTotLoad  Enthalpy')
		//20 Format(1x,I3,3x, 5(2x, F9.4), 2(2x, F9.2))

	}

	void
	CalcVAVVS(
		int const SysNum, // Unit index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const ZoneNode, // zone node number
		int const EP_UNUSED( HCoilType ), // type of hot water coil !unused1208
		Real64 const HWFlow, // hot water flow (kg/s)
		Real64 const HCoilReq, // gas or elec coil demand requested
		int const FanType, // type of fan
		Real64 const AirFlow, // air flow rate (kg/s)
		int const FanOn, // 1 means fan is on
		Real64 & LoadMet // load met by unit (watts)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate the components making up the VAV terminal unit with variable speed fan.

		// METHODOLOGY EMPLOYED:
		// Simulates the unit components sequentially in the air flow direction.

		// REFERENCES:
		// na

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using Fans::SimulateFanComponents;
		using DataHVACGlobals::TurnFansOff;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanInNode; // unit air inlet node (fan inlet)
		int FanOutNode; // fan outlet node (heating coil inlet node)
		int HCOutNode; // unit air outlet node (heating coil air outlet node)
		int HotControlNode; // the hot water inlet node
		Real64 AirMassFlow; // total air mass flow rate [kg/s]
		Real64 CpAirZn; // zone air specific heat [J/kg-C]
		bool TurnFansOffSav; // save the fan off flag
		Real64 mdot;

		TurnFansOffSav = TurnFansOff;
		FanInNode = Sys( SysNum ).InletNodeNum;
		FanOutNode = Sys( SysNum ).OutletNodeNum;
		HCOutNode = Sys( SysNum ).ReheatAirOutletNode;
		HotControlNode = Sys( SysNum ).ReheatControlNode;
		AirMassFlow = AirFlow;
		Node( FanInNode ).MassFlowRate = AirMassFlow;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, Node( ZoneNode ).Temp );
		if ( FanType == FanType_VS && FanOn == 1 ) {
			SimulateFanComponents( Sys( SysNum ).FanName, FirstHVACIteration, Sys( SysNum ).Fan_Index );
		} else { // pass through conditions
			TurnFansOff = true;
			SimulateFanComponents( Sys( SysNum ).FanName, FirstHVACIteration, Sys( SysNum ).Fan_Index );
			TurnFansOff = TurnFansOffSav;
			Node( FanOutNode ).MassFlowRate = Node( FanInNode ).MassFlowRate;
			Node( FanOutNode ).MassFlowRateMaxAvail = Node( FanInNode ).MassFlowRateMaxAvail;
			Node( FanOutNode ).MassFlowRateMinAvail = Node( FanInNode ).MassFlowRateMinAvail;
		}
		{ auto const SELECT_CASE_var( Sys( SysNum ).ReheatComp_Num );
		if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
			mdot = HWFlow;
			if ( Sys( SysNum ).HWLoopNum > 0 ) {
				SetComponentFlowRate( mdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).ReheatCoilOutletNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, Sys( SysNum ).HWCompIndex );
			}

			SimulateWaterCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index );
		} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // HW Flow is steam mass flow here
			mdot = HWFlow;
			if ( Sys( SysNum ).HWLoopNum > 0 ) {
				SetComponentFlowRate( mdot, Sys( SysNum ).ReheatControlNode, Sys( SysNum ).ReheatCoilOutletNode, Sys( SysNum ).HWLoopNum, Sys( SysNum ).HWLoopSide, Sys( SysNum ).HWBranchIndex, Sys( SysNum ).HWCompIndex );
			}
			SimulateSteamCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, Sys( SysNum ).ReheatComp_Index, HCoilReq );
		} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
			SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, HCoilReq, Sys( SysNum ).ReheatComp_Index );
		} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
			SimulateHeatingCoilComponents( Sys( SysNum ).ReheatName, FirstHVACIteration, HCoilReq, Sys( SysNum ).ReheatComp_Index );
		} else {
			ShowFatalError( "Invalid Reheat Component=" + Sys( SysNum ).ReheatComp );
		}}

		LoadMet = AirMassFlow * CpAirZn * ( Node( HCOutNode ).Temp - Node( ZoneNode ).Temp );

	}

	Real64
	VAVVSCoolingResidual(
		Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
		// Unit Output depends on the supply air flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcVAVVS, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = FirstHVACIteration (1. or 0.)
		// Par(3) = REAL(ZoneNodeNum)
		// Par(4) = REAL(HCType)
		// Par(5) = minimum HW flow rate [kg/s]
		// Par(6) = REAL(FanType)
		// Par(7) = REAL(FanOp)
		// Par(8) = cooling demand [W] (negative)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitIndex;
		bool FirstHVACSoln;
		int ZoneNodeIndex;
		Real64 MinHWFlow; // min hot water flow rate
		int HCType; // heating coil type (integer)
		int FanType; // fan type (as an integer)
		int FanOp; // fan operation; 0=off, 1=on.
		Real64 UnitOutput; // cooling output [W] (cooling is negative)

		UnitIndex = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		ZoneNodeIndex = int( Par( 3 ) );
		HCType = int( Par( 4 ) );
		MinHWFlow = Par( 5 );
		FanType = int( Par( 6 ) );
		FanOp = int( Par( 7 ) );
		CalcVAVVS( UnitIndex, FirstHVACSoln, ZoneNodeIndex, HCType, MinHWFlow, 0.0, FanType, SupplyAirMassFlow, FanOp, UnitOutput );

		Residuum = ( Par( 8 ) - UnitOutput ) / Par( 8 );

		return Residuum;
	}

	Real64
	VAVVSHWNoFanResidual(
		Real64 const HWMassFlow, // hot water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
		// Unit Output depends on the hot water flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcVAVVS, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = FirstHVACIteration (1. or 0.)
		// Par(3) = REAL(ZoneNodeNum)
		// Par(4) = REAL(HCType)
		// Par(5) = air mass flow flow rate [kg/s]
		// Par(6) = REAL(FanType)
		// Par(7) = REAL(FanOp)
		// Par(8) = heating demand [W]
		// Par(9) = min steam flow rate [m3/s] - steam only
		// Par(10 = max steam flow rate [m3/s] - steam only

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitIndex;
		bool FirstHVACSoln;
		int ZoneNodeIndex;
		Real64 AirMassFlow; // supply air mass flow rate [kg/s]
		int HCType; // heating coil type (integer)
		int FanType; // fan type (as an integer)
		int FanOp; // fan operation; 0=off, 1=on.
		Real64 UnitOutput; // heating output [W]
		Real64 QSteamLoad; // proportional load to calculate steam flow [W]
		Real64 MinSteamFlow;
		Real64 MaxSteamFlow;
		Real64 MaxSteamCoilCapacity;

		UnitIndex = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		ZoneNodeIndex = int( Par( 3 ) );
		HCType = int( Par( 4 ) );
		AirMassFlow = Par( 5 );
		FanType = int( Par( 6 ) );
		FanOp = int( Par( 7 ) );
		QSteamLoad = 0.0;
		// vary the load to be met by the steam coil to converge on a steam flow rate to meet the load
		if ( HCType == HCoilType_SteamAirHeating ) {
			//   backwards way of varying steam flow rate. Steam coil calculates a flow rate to meet a load.
			MinSteamFlow = Par( 9 );
			MaxSteamFlow = Par( 10 );
			MaxSteamCoilCapacity = Par( 11 );
			if ( ( MaxSteamFlow - MinSteamFlow ) == 0.0 ) {
				QSteamLoad = Par( 8 ); // Use QTotLoad, bad starting value error for RegulaFalsi will occur
			} else {
				QSteamLoad = MaxSteamCoilCapacity * HWMassFlow / ( MaxSteamFlow - MinSteamFlow );
			}
		}
		CalcVAVVS( UnitIndex, FirstHVACSoln, ZoneNodeIndex, HCType, HWMassFlow, QSteamLoad, FanType, AirMassFlow, FanOp, UnitOutput );

		Residuum = ( Par( 8 ) - UnitOutput ) / Par( 8 );

		return Residuum;
	}

	Real64
	VAVVSHWFanOnResidual(
		Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
		// Unit Output depends on the supply air flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcVAVVS, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = FirstHVACIteration (1. or 0.)
		// Par(3) = REAL(ZoneNodeNum)
		// Par(4) = REAL(HCType)
		// Par(5) = hot water mass flow rate [kg/s]
		// Par(6) = REAL(FanType)
		// Par(7) = REAL(FanOp)
		// Par(8) = heating demand [W]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitIndex;
		bool FirstHVACSoln;
		int ZoneNodeIndex;
		Real64 HWMassFlow; // hot water mass flow rate [kg/s]
		int HCType; // heating coil type (integer)
		int FanType; // fan type (as an integer)
		int FanOp; // fan operation; 0=off, 1=on.
		Real64 UnitOutput; // heating output [W]

		UnitIndex = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		ZoneNodeIndex = int( Par( 3 ) );
		HCType = int( Par( 4 ) );
		HWMassFlow = Par( 5 );
		FanType = int( Par( 6 ) );
		FanOp = int( Par( 7 ) );
		CalcVAVVS( UnitIndex, FirstHVACSoln, ZoneNodeIndex, HCType, HWMassFlow, Par( 8 ), FanType, SupplyAirMassFlow, FanOp, UnitOutput );

		Residuum = ( Par( 8 ) - UnitOutput ) / Par( 8 );

		return Residuum;
	}

	Real64
	VAVVSHCFanOnResidual(
		Real64 const HeatingFrac, // fraction of maximum heating output
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
		// Unit Output depends on the heating coil output which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcVAVVS, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = FirstHVACIteration (1. or 0.)
		// Par(3) = REAL(ZoneNodeNum)
		// Par(4) = REAL(HCType)
		// Par(5) = max heating coil output [W]
		// Par(6) = REAL(FanType)
		// Par(7) = REAL(FanOp)
		// Par(8) = heating demand [W]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitIndex;
		bool FirstHVACSoln;
		int ZoneNodeIndex;
		Real64 MaxHeatOut; // maximum heating output [W]
		int HCType; // heating coil type (integer)
		int FanType; // fan type (as an integer)
		int FanOp; // fan operation; 0=off, 1=on.
		Real64 UnitOutput; // heating output [W]
		Real64 AirMassFlowRate; // [kg/s]
		Real64 HeatOut; // heating coil output [W]

		UnitIndex = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		ZoneNodeIndex = int( Par( 3 ) );
		HCType = int( Par( 4 ) );
		MaxHeatOut = Par( 5 );
		FanType = int( Par( 6 ) );
		FanOp = int( Par( 7 ) );
		HeatOut = HeatingFrac * MaxHeatOut;
		AirMassFlowRate = max( HeatingFrac * Sys( UnitIndex ).HeatAirMassFlowRateMax, SysInlet( UnitIndex ).AirMassFlowRateMaxAvail * Sys( UnitIndex ).ZoneMinAirFrac );

		CalcVAVVS( UnitIndex, FirstHVACSoln, ZoneNodeIndex, HCType, 0.0, HeatOut, FanType, AirMassFlowRate, FanOp, UnitOutput );

		Residuum = ( Par( 8 ) - UnitOutput ) / Par( 8 );

		return Residuum;
	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Sys Module
	// *****************************************************************************

	void
	UpdateSys( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   january 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the Syss.

		// METHODOLOGY EMPLOYED:
		// There is method to this madness.

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
		int OutletNode;
		int InletNode;

		OutletNode = Sys( SysNum ).OutletNodeNum;
		InletNode = Sys( SysNum ).InletNodeNum;

		if ( Sys( SysNum ).SysType_Num == SingleDuctVAVReheat || Sys( SysNum ).SysType_Num == SingleDuctCBVAVReheat || Sys( SysNum ).SysType_Num == SingleDuctCBVAVNoReheat || Sys( SysNum ).SysType_Num == SingleDuctVAVNoReheat ) {
			// Set the outlet air nodes of the Sys
			Node( OutletNode ).MassFlowRate = SysOutlet( SysNum ).AirMassFlowRate;
			Node( OutletNode ).Temp = SysOutlet( SysNum ).AirTemp;
			Node( OutletNode ).HumRat = SysOutlet( SysNum ).AirHumRat;
			Node( OutletNode ).Enthalpy = SysOutlet( SysNum ).AirEnthalpy;
			// Set the outlet nodes for properties that just pass through & not used
			Node( OutletNode ).Quality = Node( InletNode ).Quality;
			Node( OutletNode ).Press = Node( InletNode ).Press;

		}

		//After all of the Oulets are updated the mass flow information needs to be
		// passed back to the system inlet.
		Node( InletNode ).MassFlowRate = SysOutlet( SysNum ).AirMassFlowRate;
		Node( OutletNode ).MassFlowRateMaxAvail = min( SysOutlet( SysNum ).AirMassFlowRateMaxAvail, Node( OutletNode ).MassFlowRateMax );
		Node( OutletNode ).MassFlowRateMinAvail = SysOutlet( SysNum ).AirMassFlowRateMinAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( OutletNode ).CO2 = Node( InletNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( OutletNode ).GenContam = Node( InletNode ).GenContam;
		}

	}

	//        End of Update subroutines for the Sys Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Sys Module
	// *****************************************************************************

	void
	ReportSys( int const EP_UNUSED( SysNum ) ) // unused1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Unknown
		//       DATE WRITTEN   Unknown
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the Sys report variables.

		// METHODOLOGY EMPLOYED:
		// There is method to this madness.

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

		// Still needs to report the Sys power from this component

	}

	void
	GetHVACSingleDuctSysIndex(
		std::string const & SDSName,
		int & SDSIndex,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType,
		Optional_int DamperInletNode, // Damper inlet node number
		Optional_int DamperOutletNode // Damper outlet node number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given single duct system -- issues error message if that system
		// is not a legal system.

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
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetSysInput();
			GetInputFlag = false;
		}

		SDSIndex = FindItemInList( SDSName, Sys, &SysDesignParams::SysName );
		if ( SDSIndex == 0 ) {
			if ( present( ThisObjectType ) ) {
				ShowSevereError( ThisObjectType() + ", GetHVACSingleDuctSysIndex: Single duct system not found=" + SDSName );
			} else {
				ShowSevereError( "GetHVACSingleDuctSysIndex: Single duct system not found=" + SDSName );
			}
			ErrorsFound = true;
		} else {
			if ( ( Sys( SDSIndex ).SysType_Num != SingleDuctConstVolReheat ) && ( Sys( SDSIndex ).SysType_Num != SingleDuctVAVReheat ) ) {
				ShowSevereError( ThisObjectType() + ", GetHVACSingleDuctSysIndex: Could not find allowed types=" + SDSName );
				ShowContinueError( "The allowed types are: AirTerminal:SingleDuct:ConstantVolume:Reheat and AirTerminal:SingleDuct:VAV:Reheat" );
				ErrorsFound = true;
			}
			if ( Sys( SDSIndex ).SysType_Num == SingleDuctVAVReheat ) {
				if ( present( DamperInletNode ) ) DamperInletNode = Sys( SDSIndex ).InletNodeNum;
				if ( present( DamperOutletNode ) ) DamperOutletNode = Sys( SDSIndex ).OutletNodeNum;
			}
		}

	}

	void
	SimATMixer(
		std::string const & SysName,
		bool const FirstHVACIteration,
		int & SysIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Simulate an Air Terminal Mixer component

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int SysNum( 0 );

		if ( GetATMixerFlag ) {
			GetATMixers();
			GetATMixerFlag = false;
		}

		if ( SysIndex == 0 ) {
			SysNum = FindItemInList( SysName, SysATMixer );
			SysIndex = SysNum;
			if ( SysNum == 0 ) {
				ShowFatalError( "Object " + SysName + " not found" );
			}
		} else {
			SysNum = SysIndex;
		}

		InitATMixer( SysNum, FirstHVACIteration ); // Not being used, is placeholder

		CalcATMixer( SysNum );

		UpdateATMixer( SysNum );

	}

	void
	GetATMixers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Get input for inlet side air temrinal mixers and store it in the inlet side air terminal mixer array

		// METHODOLOGY EMPLOYED:
		// Use the Get routines from the InputProcessor module.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using NodeInputManager::GetOnlySingleNode;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::EquipmentData;
		using DataZoneEquipment::SubEquipmentData;
		using namespace DataLoopNode;
		using namespace DataIPShortCuts;
		using BranchNodeConnections::TestCompSet;
		using BranchNodeConnections::SetUpCompSets;
		using DataGlobals::NumOfZones;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumNums; // Number of REAL(r64) numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int InletATMixerNum; // Index of inlet side mixer air terminal unit
		int SupplyATMixerNum; // Index of supply side mixer air terminal unit
		int NumInletATMixers; // Number of inlet side mixer air terminal units
		int NumSupplyATMixers; // Number of supply side mixer air terminal units
		int IOStat;
		static std::string const RoutineName( "GetATMixers: " ); // include trailing blank space
		static bool ErrorsFound( false ); // Error flag
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NodeNum; // Index to node number
		int CtrlZone; // Index to control zone
		bool ZoneNodeNotFound; // Flag for error checking
		int SupAirIn; // Supply air inlet node index
		bool errFlag; // error flag from component validation

		NumInletATMixers = GetNumObjectsFound( "AirTerminal:SingleDuct:InletSideMixer" );
		NumSupplyATMixers = GetNumObjectsFound( "AirTerminal:SingleDuct:SupplySideMixer" );

		NumATMixers = NumInletATMixers + NumSupplyATMixers;
		SysATMixer.allocate( NumATMixers );

		cCurrentModuleObject = "AirTerminal:SingleDuct:InletSideMixer";

		for ( InletATMixerNum = 1; InletATMixerNum <= NumInletATMixers; ++InletATMixerNum ) {
			GetObjectItem( cCurrentModuleObject, InletATMixerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SysATMixer, InletATMixerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxxxxx";
			}
			SysATMixer( InletATMixerNum ).Name = cAlphaArgs( 1 );
			SysATMixer( InletATMixerNum ).MixerType = 1; // inlet side mixer
			if ( cAlphaArgs( 2 ) == "ZONEHVAC:WATERTOAIRHEATPUMP" ) {
				SysATMixer( InletATMixerNum ).ZoneHVACUnitType = 1;
			} else if ( cAlphaArgs( 2 ) == "ZONEHVAC:FOURPIPEFANCOIL" ) {
				SysATMixer( InletATMixerNum ).ZoneHVACUnitType = 2;
			}

			SysATMixer( InletATMixerNum ).ZoneHVACUnitName = cAlphaArgs( 3 );

			ValidateComponent( cAlphaArgs( 2 ), SysATMixer( InletATMixerNum ).ZoneHVACUnitName, errFlag, cCurrentModuleObject );

			SysATMixer( InletATMixerNum ).MixedAirOutNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFieldNames( 4 ) );

			SysATMixer( InletATMixerNum ).PriInNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFieldNames( 5 ) );
			SysATMixer( InletATMixerNum ).SecInNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFieldNames( 6 ) );
			// Check for dupes in the three nodes.
			if ( SysATMixer( InletATMixerNum ).SecInNode == SysATMixer( InletATMixerNum ).PriInNode ) {
				ShowSevereError( cCurrentModuleObject + " = " + SysATMixer( InletATMixerNum ).Name + ' ' + cAlphaArgs( 5 ) + " = " + NodeID( SysATMixer( InletATMixerNum ).PriInNode ) + " duplicates the " + cAlphaArgs( 4 ) + '.' );
				ErrorsFound = true;
			} else if ( SysATMixer( InletATMixerNum ).SecInNode == SysATMixer( InletATMixerNum ).MixedAirOutNode ) {
				ShowSevereError( cCurrentModuleObject + " = " + SysATMixer( InletATMixerNum ).Name + ' ' + cAlphaArgs( 6 ) + " = " + NodeID( SysATMixer( InletATMixerNum ).MixedAirOutNode ) + " duplicates the " + cAlphaArgs( 4 ) + '.' );
				ErrorsFound = true;
			}

			if ( SysATMixer( InletATMixerNum ).PriInNode == SysATMixer( InletATMixerNum ).MixedAirOutNode ) {
				ShowSevereError( cCurrentModuleObject + " = " + SysATMixer( InletATMixerNum ).Name + ' ' + cAlphaArgs( 6 ) + " = " + NodeID( SysATMixer( InletATMixerNum ).MixedAirOutNode ) + " duplicates the " + cAlphaArgs( 5 ) + '.' );
				ErrorsFound = true;
			}

			// Air Terminal inlet node must be the same as a zone exhaust node
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
					if ( SysATMixer( InletATMixerNum ).SecInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
						ZoneNodeNotFound = false;
						for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
							if ( SysATMixer( InletATMixerNum ).SecInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( SupAirIn ) ) {
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = SysATMixer( InletATMixerNum ).PriInNode;
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = SysATMixer( InletATMixerNum ).MixedAirOutNode;
								ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).InNode = SysATMixer( InletATMixerNum ).PriInNode;
								ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).OutNode = SysATMixer( InletATMixerNum ).MixedAirOutNode;
							}
						}
						goto ControlledZoneLoop_exit;
					}
				}
			}
			ControlledZoneLoop_exit: ;
			if ( ZoneNodeNotFound ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + SysATMixer( InletATMixerNum ).Name + "\". Inlet Side Air Terminal Mixer air inlet node name must be the same as a zone exhaust node name." );
				ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "..Inlet Side Air Terminal Mixer inlet node name = " + NodeID( SysATMixer( InletATMixerNum ).SecInNode ) );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, SysATMixer( InletATMixerNum ).Name, cAlphaArgs( 5 ), cAlphaArgs( 4 ), "Air Nodes" );

		}

		cCurrentModuleObject = "AirTerminal:SingleDuct:SupplySideMixer";

		for ( SupplyATMixerNum = NumInletATMixers + 1; SupplyATMixerNum <= NumInletATMixers + NumSupplyATMixers; ++SupplyATMixerNum ) {
			GetObjectItem( cCurrentModuleObject, SupplyATMixerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SysATMixer, SupplyATMixerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxxxxx";
			}
			SysATMixer( SupplyATMixerNum ).Name = cAlphaArgs( 1 );
			SysATMixer( SupplyATMixerNum ).MixerType = 2; // supply side mixer
			if ( cAlphaArgs( 2 ) == "ZONEHVAC:WATERTOAIRHEATPUMP" ) {
				SysATMixer( SupplyATMixerNum ).ZoneHVACUnitType = 1;
			} else if ( cAlphaArgs( 2 ) == "ZONEHVAC:FOURPIPEFANCOIL" ) {
				SysATMixer( SupplyATMixerNum ).ZoneHVACUnitType = 2;
			}

			SysATMixer( SupplyATMixerNum ).ZoneHVACUnitName = cAlphaArgs( 3 );

			ValidateComponent( cAlphaArgs( 2 ), SysATMixer( SupplyATMixerNum ).ZoneHVACUnitName, errFlag, cCurrentModuleObject );

			SysATMixer( SupplyATMixerNum ).MixedAirOutNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFieldNames( 4 ) );

			SysATMixer( SupplyATMixerNum ).PriInNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFieldNames( 5 ) );
			SysATMixer( SupplyATMixerNum ).SecInNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFieldNames( 6 ) );
			// Check for dupes in the three nodes.
			if ( SysATMixer( SupplyATMixerNum ).SecInNode == SysATMixer( SupplyATMixerNum ).PriInNode ) {
				ShowSevereError( cCurrentModuleObject + " = " + SysATMixer( SupplyATMixerNum ).Name + ' ' + cAlphaArgs( 5 ) + " = " + NodeID( SysATMixer( SupplyATMixerNum ).PriInNode ) + " duplicates the " + cAlphaArgs( 4 ) + '.' );
				ErrorsFound = true;
			} else if ( SysATMixer( SupplyATMixerNum ).SecInNode == SysATMixer( SupplyATMixerNum ).MixedAirOutNode ) {
				ShowSevereError( cCurrentModuleObject + " = " + SysATMixer( SupplyATMixerNum ).Name + ' ' + cAlphaArgs( 6 ) + " = " + NodeID( SysATMixer( SupplyATMixerNum ).MixedAirOutNode ) + " duplicates the " + cAlphaArgs( 4 ) + '.' );
				ErrorsFound = true;
			}

			if ( SysATMixer( SupplyATMixerNum ).PriInNode == SysATMixer( SupplyATMixerNum ).MixedAirOutNode ) {
				ShowSevereError( cCurrentModuleObject + " = " + SysATMixer( SupplyATMixerNum ).Name + ' ' + cAlphaArgs( 6 ) + " = " + NodeID( SysATMixer( SupplyATMixerNum ).MixedAirOutNode ) + " duplicates the " + cAlphaArgs( 5 ) + '.' );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, SysATMixer( SupplyATMixerNum ).Name, cAlphaArgs( 5 ), cAlphaArgs( 4 ), "Air Nodes" );

			// Air Terminal outlet node must be the same as a zone inlet node
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
					if ( SysATMixer( SupplyATMixerNum ).MixedAirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
						ZoneNodeNotFound = false;
						for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
							if ( SysATMixer( SupplyATMixerNum ).MixedAirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = SysATMixer( SupplyATMixerNum ).PriInNode;
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = SysATMixer( SupplyATMixerNum ).MixedAirOutNode;
								ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).InNode = SysATMixer( SupplyATMixerNum ).PriInNode;
								ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).OutNode = SysATMixer( SupplyATMixerNum ).MixedAirOutNode;
							}
						}
						goto ControlZoneLoop_exit;
					}
				}
			}
			ControlZoneLoop_exit: ;
			if ( ZoneNodeNotFound ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + SysATMixer( SupplyATMixerNum ).Name + "\". Supply Side Air Terminal Mixer air outlet node name must be the same as a zone inlet node name." );
				ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "..Inlet Side Air Terminal Mixer inlet node name = " + NodeID( SysATMixer( SupplyATMixerNum ).SecInNode ) );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

	}

	void
	InitATMixer(
		int const ATMixerNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Initialize the AirTerminalMixers data structure with node data

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int PriInNode;
		int MixedAirOutNode;

		InletNode = SysATMixer( ATMixerNum ).SecInNode;
		PriInNode = SysATMixer( ATMixerNum ).PriInNode;
		MixedAirOutNode = SysATMixer( ATMixerNum ).MixedAirOutNode;

		if ( FirstHVACIteration ) {
			//  SysATMixer(ATMixerNum)%ZoneAirMassFlowRate = SysATMixer(ATMixerNum)%MaxAirMassFlowRate
		}

		if ( BeginDayFlag ) {
		}

		if ( FirstHVACIteration ) {
		}

	}

	void
	CalcATMixer( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Calculate the mixed air flow and conditions in the air terminal mixer

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using Psychrometrics::PsyTdbFnHW;
		using DataEnvironment::StdRhoAir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Real64 PriMassFlowRate( 0.0 );
		static Real64 PriEnthalpy( 0.0 );
		static Real64 PriHumRat( 0.0 );
		static Real64 PriTemp( 0.0 );

		static Real64 SecAirMassFlowRate( 0.0 );
		static Real64 SecAirEnthalpy( 0.0 );
		static Real64 SecAirHumRat( 0.0 );
		static Real64 SecAirTemp( 0.0 );

		static Real64 MixedAirMassFlowRate( 0.0 );
		static Real64 MixedAirEnthalpy( 0.0 );
		static Real64 MixedAirHumRat( 0.0 );
		static Real64 MixedAirTemp( 0.0 );

		PriEnthalpy = Node( SysATMixer( SysNum ).PriInNode ).Enthalpy;
		PriHumRat = Node( SysATMixer( SysNum ).PriInNode ).HumRat;
		PriTemp = Node( SysATMixer( SysNum ).PriInNode ).Temp;
		PriMassFlowRate = Node( SysATMixer( SysNum ).PriInNode ).MassFlowRate;

		SecAirMassFlowRate = Node( SysATMixer( SysNum ).SecInNode ).MassFlowRate;
		SecAirEnthalpy = Node( SysATMixer( SysNum ).SecInNode ).Enthalpy;
		SecAirHumRat = Node( SysATMixer( SysNum ).SecInNode ).HumRat;
		SecAirTemp = Node( SysATMixer( SysNum ).SecInNode ).Temp;

		if ( SysATMixer( SysNum ).MixerType == ATMixer_SupplySide ) {
			MixedAirMassFlowRate = SecAirMassFlowRate + PriMassFlowRate;
		} else {
			// for inlet side mixer, the mixed air flow has been set, but we don't know the secondary flow
			MixedAirMassFlowRate = Node( SysATMixer( SysNum ).MixedAirOutNode ).MassFlowRate;
			SecAirMassFlowRate = max( MixedAirMassFlowRate - PriMassFlowRate, 0.0 );
			Node( SysATMixer( SysNum ).SecInNode ).MassFlowRate = SecAirMassFlowRate;
		}
		// now calculate the mixed (outlet) conditions
		if ( MixedAirMassFlowRate > 0.0 ) {
			MixedAirEnthalpy = ( SecAirMassFlowRate * SecAirEnthalpy + PriMassFlowRate * PriEnthalpy ) / MixedAirMassFlowRate;
			MixedAirHumRat = ( SecAirMassFlowRate * SecAirHumRat + PriMassFlowRate * PriHumRat ) / MixedAirMassFlowRate;
			// Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
			MixedAirTemp = PsyTdbFnHW( MixedAirEnthalpy, MixedAirHumRat );
		}

		SysATMixer( SysNum ).MixedAirMassFlowRate = MixedAirMassFlowRate;
		SysATMixer( SysNum ).MixedAirEnthalpy = MixedAirEnthalpy;
		SysATMixer( SysNum ).MixedAirHumRat = MixedAirHumRat;
		SysATMixer( SysNum ).MixedAirTemp = MixedAirTemp;

	}

	void
	UpdateATMixer( int const SysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Move the results of CalcATMixer to the affected nodes

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MixedAirOutNode;

		MixedAirOutNode = SysATMixer( SysNum ).MixedAirOutNode;

		// mixed air data
		Node( MixedAirOutNode ).Temp = SysATMixer( SysNum ).MixedAirTemp;
		Node( MixedAirOutNode ).HumRat = SysATMixer( SysNum ).MixedAirHumRat;
		Node( MixedAirOutNode ).Enthalpy = SysATMixer( SysNum ).MixedAirEnthalpy;
		Node( MixedAirOutNode ).Press = SysATMixer( SysNum ).MixedAirPressure;
		Node( MixedAirOutNode ).MassFlowRate = SysATMixer( SysNum ).MixedAirMassFlowRate;

	}

	void
	GetATMixerPriNode(
		std::string const & ZoneEquipName,
		int & ATMixerPriNode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the zone air inlet node for a given ATMixer

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
		int ATMixerIndex; // local air terminal mixer index

		bool ErrorsFound; // for error trapping

		if ( GetATMixerFlag ) {
			GetATMixers();
			GetATMixerFlag = false;
		}

		ATMixerIndex = FindItemInList( ZoneEquipName, SysATMixer );
		if ( ATMixerIndex > 0 ) {
			ATMixerPriNode = SysATMixer( ATMixerIndex ).PriInNode;
		}

		if ( ATMixerIndex == 0 ) {
			ShowSevereError( "GetATMixerPriNode: Air Terminal Mixer zone air inlet node not found for zone equipment=" + ZoneEquipName );
			ErrorsFound = true;
		}

	}

	void
	GetATMixerSecNode(
		std::string const & ZoneEquipName,
		int & ATMixerSecNode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the zone air inlet node for a given ATMixer

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
		int ATMixerIndex; // local air terminal mixer index

		bool ErrorsFound; // for error trapping

		if ( GetATMixerFlag ) {
			GetATMixers();
			GetATMixerFlag = false;
		}

		ATMixerIndex = FindItemInList( ZoneEquipName, SysATMixer );
		if ( ATMixerIndex > 0 ) {
			ATMixerSecNode = SysATMixer( ATMixerIndex ).SecInNode;
		}

		if ( ATMixerIndex == 0 ) {
			ShowSevereError( "GetATMixerSecNode: Air Terminal Mixer zone air inlet node not found for zone equipment=" + ZoneEquipName );
			ErrorsFound = true;
		}

	}

	void
	GetATMixerOutNode(
		std::string const & ZoneEquipName,
		int & ATMixerOutNode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the mixed air outlet node for a given ATMixer

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
		int ATMixerIndex; // local air terminal mixer index

		bool ErrorsFound; // for error trapping

		if ( GetATMixerFlag ) {
			GetATMixers();
			GetATMixerFlag = false;
		}

		ATMixerIndex = FindItemInList( ZoneEquipName, SysATMixer );
		if ( ATMixerIndex > 0 ) {
			ATMixerOutNode = SysATMixer( ATMixerIndex ).MixedAirOutNode;
		}

		if ( ATMixerIndex == 0 ) {
			ShowSevereError( "GetATMixerOutNode: Air Terminal Mixer outlet node not found for zone equipment=" + ZoneEquipName );
			ErrorsFound = true;
		}

	}

	void
	GetATMixer(
		std::string const & ZoneEquipName, // zone unit name name
		std::string & ATMixerName, // air terminal mixer name
		int & ATMixerNum, // air terminal mixer index
		int & ATMixerType, // air teminal mixer type
		int & ATMixerPriNode, // air terminal mixer primary air node number
		int & ATMixerSecNode, // air terminal mixer secondary air node number
		int & ATMixerOutNode // air terminal mixer outlet air node number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets: 1) the index of the named AT Mixer in the SysATMixer data array
		//                       2) the node number of the primary air inlet node of the AT Mixer

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		// USE ZoneAirLoopEquipmentManager, ONLY: GetZoneAirLoopEquipment

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ATMixerIndex; // local air terminal mixer index


		if ( GetATMixerFlag ) {
			// CALL GetZoneAirLoopEquipment
			GetATMixers();
			GetATMixerFlag = false;
		}

		if ( NumATMixers <= 0 ) {
			ATMixerNum = 0;
			ATMixerName = "";
			ATMixerPriNode = 0;
			ATMixerSecNode = 0;
			ATMixerOutNode = 0;
			ATMixerType = 0;
			return;
		}

		ATMixerIndex = FindItemInList( ZoneEquipName, SysATMixer, &AirTerminalMixerData::ZoneHVACUnitName );
		if ( ATMixerIndex > 0 ) {
			ATMixerNum = ATMixerIndex;
			ATMixerName = SysATMixer( ATMixerIndex ).Name;
			ATMixerPriNode = SysATMixer( ATMixerIndex ).PriInNode;
			ATMixerSecNode = SysATMixer( ATMixerIndex ).SecInNode;
			ATMixerOutNode = SysATMixer( ATMixerIndex ).MixedAirOutNode;
			ATMixerType = SysATMixer( ATMixerIndex ).MixerType;
		} else {
			ATMixerNum = 0;
			ATMixerName = "";
			ATMixerPriNode = 0;
			ATMixerSecNode = 0;
			ATMixerOutNode = 0;
			ATMixerType = 0;
		}

	}

	void
	SetATMixerPriFlow(
		int const ATMixerNum, // Air terminal mixer index
		Optional< Real64 const > PriAirMassFlowRate // Air terminal mixer primary air mass flow rate [kg/s]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This Subroutine sets the primary air mass flow rate on the primary air inlet
		// node of a terminal unit mixer component.

		// METHODOLOGY EMPLOYED:
		// The flow is set to either the input PriAirMassFlowRate if this optional input
		// parameter is present, or to the maximum available mass flow rate of the primary
		// air inlet node.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// none

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PriAirNode; // air terminal mixer primary air inlet node number

		if ( ATMixerNum <= 0 ) return;
		PriAirNode = SysATMixer( ATMixerNum ).PriInNode;
		if ( present( PriAirMassFlowRate ) ) {
			Node( PriAirNode ).MassFlowRate = PriAirMassFlowRate;
		} else {
			Node( PriAirNode ).MassFlowRate = Node( PriAirNode ).MassFlowRateMaxAvail;
		}

	}

	//        End of Reporting subroutines for the Sys Module
	// *****************************************************************************

} // SingleDuct

} // EnergyPlus
