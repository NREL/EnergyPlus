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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <DualDuct.hh>
#include <BranchNodeConnections.hh>
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
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
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

namespace DualDuct {
	// Module containing the DualDuct simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   February 2000
	//       MODIFIED       Clayton Miller, Brent Griffith Aug. 2010 - Added DualDuctOA Terminal Unit to Simulate Decoupled OA/RA
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the DualDuct Systems Simulation

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
	using DataGlobals::NumOfZones;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::ScheduleAlwaysOn;
	using DataEnvironment::StdRhoAir;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallAirVolFlow;
	using namespace DataSizing;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	//MODULE PARAMETER DEFINITIONS
	int const DualDuct_ConstantVolume( 1 );
	int const DualDuct_VariableVolume( 2 );
	int const DualDuct_OutdoorAir( 3 );
	std::string const cCMO_DDConstantVolume( "AirTerminal:DualDuct:ConstantVolume" );
	std::string const cCMO_DDVariableVolume( "AirTerminal:DualDuct:VAV" );
	std::string const cCMO_DDVarVolOA( "AirTerminal:DualDuct:VAV:OutdoorAir" );

	int const DD_OA_ConstantOAMode( 11 );
	int const DD_OA_ScheduleOAMode( 12 );
	int const DD_OA_DynamicOAMode( 13 );

	int const PerPersonModeNotSet( 20 );
	int const PerPersonDCVByCurrentLevel( 21 );
	int const PerPersonByDesignLevel( 22 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	Array1D_bool CheckEquipName;

	int NumDampers( 0 ); // The Number of Dampers found in the Input //Autodesk Poss used uninitialized in ReportDualDuctConnections
	int NumDualDuctConstVolDampers;
	int NumDualDuctVarVolDampers;
	int NumDualDuctVarVolOA;
	Real64 MassFlowSetToler;
	bool GetDualDuctInputFlag( true ); // Flag set to make sure you get input once

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Object Data
	Array1D< DamperDesignParams > Damper;
	Array1D< DamperFlowConditions > DamperInlet;
	Array1D< DamperFlowConditions > DamperHotAirInlet;
	Array1D< DamperFlowConditions > DamperColdAirInlet;
	Array1D< DamperFlowConditions > DamperOutlet;
	Array1D< DamperFlowConditions > DamperOAInlet; // VAV:OutdoorAir Outdoor Air Inlet
	Array1D< DamperFlowConditions > DamperRecircAirInlet; // VAV:OutdoorAir Recirculated Air Inlet

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SimulateDualDuct(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Damper component simulation.
		// It is called from the SimAirLoopComponent
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
		int DamperNum; // The Damper that you are currently loading input into

		// FLOW:

		// Obtains and Allocates Damper related parameters from input file
		if ( GetDualDuctInputFlag ) { //First time subroutine has been entered
			GetDualDuctInput();
			GetDualDuctInputFlag = false;
		}

		// Find the correct DamperNumber with the AirLoop & CompNum from AirLoop Derived Type
		if ( CompIndex == 0 ) {
			DamperNum = FindItemInList( CompName, Damper, &DamperDesignParams::DamperName );
			if ( DamperNum == 0 ) {
				ShowFatalError( "SimulateDualDuct: Damper not found=" + CompName );
			}
			CompIndex = DamperNum;
		} else {
			DamperNum = CompIndex;
			if ( DamperNum > NumDampers || DamperNum < 1 ) {
				ShowFatalError( "SimulateDualDuct: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Number of Dampers=" + TrimSigDigits( NumDampers ) + ", Damper name=" + CompName );
			}
			if ( CheckEquipName( DamperNum ) ) {
				if ( CompName != Damper( DamperNum ).DamperName ) {
					ShowFatalError( "SimulateDualDuct: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Damper name=" + CompName + ", stored Damper Name for that index=" + Damper( DamperNum ).DamperName );
				}
				CheckEquipName( DamperNum ) = false;
			}
		}

		if ( CompIndex > 0 ) {
			// With the correct DamperNum Initialize
			InitDualDuct( DamperNum, FirstHVACIteration ); // Initialize all Damper related parameters

			// Calculate the Correct Damper Model with the current DamperNum
			{ auto const SELECT_CASE_var( Damper( DamperNum ).DamperType );

			if ( SELECT_CASE_var == DualDuct_ConstantVolume ) { // 'AirTerminal:DualDuct:ConstantVolume'

				SimDualDuctConstVol( DamperNum, ZoneNum, ZoneNodeNum );

			} else if ( SELECT_CASE_var == DualDuct_VariableVolume ) { // 'AirTerminal:DualDuct:VAV'

				SimDualDuctVarVol( DamperNum, ZoneNum, ZoneNodeNum );

			} else if ( SELECT_CASE_var == DualDuct_OutdoorAir ) {

				SimDualDuctVAVOutdoorAir( DamperNum, ZoneNum, ZoneNodeNum ); // 'AirTerminal:DualDuct:VAV:OutdoorAir'

			}}

			// Update the current Damper to the outlet nodes
			UpdateDualDuct( DamperNum );

			// Report the current Damper
			ReportDualDuct( DamperNum );
		} else {
			ShowFatalError( "SimulateDualDuct: Damper not found=" + CompName );
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDualDuctInput()
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
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using NodeInputManager::GetOnlySingleNode;
		using DataZoneEquipment::ZoneEquipConfig;
		using BranchNodeConnections::TestCompSet;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using namespace DataIPShortCuts;
		using namespace DataHeatBalance;
		using General::RoundSigDigits;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetDualDuctInput: " ); // include trailing bla

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int DamperNum; // The Damper that you are currently loading input into
		int DamperIndex; // Loop index to Damper that you are currently loading input into
		int NumAlphas;
		int NumNums;
		int IOStat;
		static Array1D< Real64 > NumArray( 2, 0.0 );
		static Array1D_string AlphArray( 7 );
		static Array1D_string cAlphaFields( 7 ); // Alpha field names
		static Array1D_string cNumericFields( 2 ); // Numeric field names
		static Array1D_bool lAlphaBlanks( 7, true ); // Logical array, alpha field input BLANK = .TRUE.
		static Array1D_bool lNumericBlanks( 2, true ); // Logical array, numeric field input BLANK = .TRUE.
		std::string CurrentModuleObject; // for ease in getting objects
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int CtrlZone; // controlled zone do loop index
		int SupAirIn; // controlled zone supply air inlet index
		int ADUNum; // loop control to search Air Distribution Units
		static Real64 DummyOAFlow( 0.0 );

		// Flow
		NumDualDuctConstVolDampers = GetNumObjectsFound( cCMO_DDConstantVolume );
		NumDualDuctVarVolDampers = GetNumObjectsFound( cCMO_DDVariableVolume );
		NumDualDuctVarVolOA = GetNumObjectsFound( cCMO_DDVarVolOA );
		NumDampers = NumDualDuctConstVolDampers + NumDualDuctVarVolDampers + NumDualDuctVarVolOA;
		Damper.allocate( NumDampers );
		CheckEquipName.dimension( NumDampers, true );

		DamperInlet.allocate( NumDampers );
		DamperHotAirInlet.allocate( NumDampers );
		DamperColdAirInlet.allocate( NumDampers );
		DamperOutlet.allocate( NumDampers );

		DamperOAInlet.allocate( NumDampers );
		DamperRecircAirInlet.allocate( NumDampers );

		if ( NumDualDuctConstVolDampers > 0 ) {
			for ( DamperIndex = 1; DamperIndex <= NumDualDuctConstVolDampers; ++DamperIndex ) {

				CurrentModuleObject = cCMO_DDConstantVolume;

				GetObjectItem( CurrentModuleObject, DamperIndex, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				DamperNum = DamperIndex;
				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), Damper, &DamperDesignParams::DamperName, DamperNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				Damper( DamperNum ).DamperName = AlphArray( 1 );
				Damper( DamperNum ).DamperType = DualDuct_ConstantVolume;
				Damper( DamperNum ).Schedule = AlphArray( 2 );
				if ( lAlphaBlanks( 2 ) ) {
					Damper( DamperNum ).SchedPtr = ScheduleAlwaysOn;
				} else {
					Damper( DamperNum ).SchedPtr = GetScheduleIndex( AlphArray( 2 ) );
					if ( Damper( DamperNum ).SchedPtr == 0 ) {
						ShowSevereError( CurrentModuleObject + ", \"" + Damper( DamperNum ).DamperName + "\" " + cAlphaFields( 2 ) + " = " + AlphArray( 2 ) + " not found." );
						ErrorsFound = true;
					}
				}
				Damper( DamperNum ).OutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 3 ) );
				Damper( DamperNum ).HotAirInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );
				Damper( DamperNum ).ColdAirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 5 ) );

				Damper( DamperNum ).MaxAirVolFlowRate = NumArray( 1 );
				Damper( DamperNum ).ZoneMinAirFrac = 0.0;

				// Register component set data - one for heat and one for cool
				TestCompSet( CurrentModuleObject + ":HEAT", Damper( DamperNum ).DamperName, AlphArray( 4 ), AlphArray( 3 ), "Air Nodes" );
				TestCompSet( CurrentModuleObject + ":COOL", Damper( DamperNum ).DamperName, AlphArray( 5 ), AlphArray( 3 ), "Air Nodes" );

				// Fill the Zone Equipment data with the inlet node numbers of this unit.
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
						if ( Damper( DamperNum ).OutletNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
							if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
								ShowSevereError( "Error in connecting a terminal unit to a zone" );
								ShowContinueError( NodeID( Damper( DamperNum ).OutletNodeNum ) + " already connects to another zone" );
								ShowContinueError( "Occurs for terminal unit " + CurrentModuleObject + " = " + Damper( DamperNum ).DamperName );
								ShowContinueError( "Check terminal unit node names for errors" );
								ErrorsFound = true;
							} else {
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Damper( DamperNum ).ColdAirInletNodeNum;
								ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).InNode = Damper( DamperNum ).HotAirInletNodeNum;
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Damper( DamperNum ).OutletNodeNum;
								ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).OutNode = Damper( DamperNum ).OutletNodeNum;
							}
						}
					}
				}

				//Setup the Average damper Position output variable
				// CurrentModuleObject='AirTerminal:DualDuct:ConstantVolume'
				SetupOutputVariable( "Zone Air Terminal Cold Supply Duct Damper Position []", Damper( DamperNum ).ColdAirDamperPosition, "System", "Average", Damper( DamperNum ).DamperName );
				SetupOutputVariable( "Zone Air Terminal Hot Supply Duct Damper Position []", Damper( DamperNum ).HotAirDamperPosition, "System", "Average", Damper( DamperNum ).DamperName );

			} // end Number of Damper Loop
		}

		if ( NumDualDuctVarVolDampers > 0 ) {
			for ( DamperIndex = 1; DamperIndex <= NumDualDuctVarVolDampers; ++DamperIndex ) {

				CurrentModuleObject = cCMO_DDVariableVolume;

				GetObjectItem( CurrentModuleObject, DamperIndex, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				DamperNum = DamperIndex + NumDualDuctConstVolDampers;
				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), Damper, &DamperDesignParams::DamperName, DamperNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				Damper( DamperNum ).DamperName = AlphArray( 1 );
				Damper( DamperNum ).DamperType = DualDuct_VariableVolume;
				Damper( DamperNum ).Schedule = AlphArray( 2 );
				if ( lAlphaBlanks( 2 ) ) {
					Damper( DamperNum ).SchedPtr = ScheduleAlwaysOn;
				} else {
					Damper( DamperNum ).SchedPtr = GetScheduleIndex( AlphArray( 2 ) );
					if ( Damper( DamperNum ).SchedPtr == 0 ) {
						ShowSevereError( CurrentModuleObject + ", \"" + Damper( DamperNum ).DamperName + "\" " + cAlphaFields( 2 ) + " = " + AlphArray( 2 ) + " not found." );
						ErrorsFound = true;
					}
				}
				Damper( DamperNum ).OutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 3 ) );
				Damper( DamperNum ).HotAirInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );
				Damper( DamperNum ).ColdAirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 5 ) );

				Damper( DamperNum ).MaxAirVolFlowRate = NumArray( 1 );
				Damper( DamperNum ).ZoneMinAirFrac = NumArray( 2 );

				// Register component set data - one for heat and one for cool
				TestCompSet( CurrentModuleObject + ":HEAT", Damper( DamperNum ).DamperName, AlphArray( 4 ), AlphArray( 3 ), "Air Nodes" );
				TestCompSet( CurrentModuleObject + ":COOL", Damper( DamperNum ).DamperName, AlphArray( 5 ), AlphArray( 3 ), "Air Nodes" );

				// Fill the Zone Equipment data with the inlet node numbers of this unit.
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
						if ( Damper( DamperNum ).OutletNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Damper( DamperNum ).ColdAirInletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).InNode = Damper( DamperNum ).HotAirInletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Damper( DamperNum ).OutletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).OutNode = Damper( DamperNum ).OutletNodeNum;

							Damper( DamperNum ).CtrlZoneNum = CtrlZone;
							Damper( DamperNum ).ActualZoneNum = ZoneEquipConfig( CtrlZone ).ActualZoneNum;

						}
					}
				}

				if ( ! lAlphaBlanks( 6 ) ) {
					Damper( DamperNum ).OARequirementsPtr = FindItemInList( AlphArray( 6 ), OARequirements );
					if ( Damper( DamperNum ).OARequirementsPtr == 0 ) {
						ShowSevereError( cAlphaFields( 6 ) + " = " + AlphArray( 6 ) + " not found." );
						ShowContinueError( "Occurs in " + cCMO_DDVariableVolume + " = " + Damper( DamperNum ).DamperName );
						ErrorsFound = true;
					} else {
						Damper( DamperNum ).NoOAFlowInputFromUser = false;
					}
				}

				//Setup the Average damper Position output variable
				// CurrentModuleObject='AirTerminal:DualDuct:VAV'
				SetupOutputVariable( "Zone Air Terminal Cold Supply Duct Damper Position []", Damper( DamperNum ).ColdAirDamperPosition, "System", "Average", Damper( DamperNum ).DamperName );
				SetupOutputVariable( "Zone Air Terminal Hot Supply Duct Damper Position []", Damper( DamperNum ).HotAirDamperPosition, "System", "Average", Damper( DamperNum ).DamperName );
				SetupOutputVariable( "Zone Air Terminal Outdoor Air Volume Flow Rate [m3/s]", Damper( DamperNum ).OutdoorAirFlowRate, "System", "Average", Damper( DamperNum ).DamperName );
			} // end Number of Damper Loop
		}

		if ( NumDualDuctVarVolOA > 0 ) {
			for ( DamperIndex = 1; DamperIndex <= NumDualDuctVarVolOA; ++DamperIndex ) {

				CurrentModuleObject = cCMO_DDVarVolOA;

				GetObjectItem( CurrentModuleObject, DamperIndex, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				DamperNum = DamperIndex + NumDualDuctConstVolDampers + NumDualDuctVarVolDampers;
				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), Damper, &DamperDesignParams::DamperName, DamperNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				Damper( DamperNum ).DamperName = AlphArray( 1 );
				Damper( DamperNum ).DamperType = DualDuct_OutdoorAir;
				Damper( DamperNum ).Schedule = AlphArray( 2 );
				if ( lAlphaBlanks( 2 ) ) {
					Damper( DamperNum ).SchedPtr = ScheduleAlwaysOn;
				} else {
					Damper( DamperNum ).SchedPtr = GetScheduleIndex( AlphArray( 2 ) );
					if ( Damper( DamperNum ).SchedPtr == 0 ) {
						ShowSevereError( CurrentModuleObject + ", \"" + Damper( DamperNum ).DamperName + "\" " + cAlphaFields( 2 ) + " = " + AlphArray( 2 ) + " not found." );
						ErrorsFound = true;
					}
				}
				Damper( DamperNum ).OutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 3 ) );
				Damper( DamperNum ).OAInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );

				if ( ! lAlphaBlanks( 5 ) ) {
					Damper( DamperNum ).RecircAirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 5 ) );
				} else {
					// for this model, we intentionally allow not using the recirc side
					Damper( DamperNum ).RecircIsUsed = false;
				}

				Damper( DamperNum ).MaxAirVolFlowRate = NumArray( 1 );
				Damper( DamperNum ).MaxAirMassFlowRate = Damper( DamperNum ).MaxAirVolFlowRate * StdRhoAir;

				// Register component set data - one for OA and one for RA
				TestCompSet( CurrentModuleObject + ":OutdoorAir", Damper( DamperNum ).DamperName, AlphArray( 4 ), AlphArray( 3 ), "Air Nodes" );
				if ( Damper( DamperNum ).RecircIsUsed ) {
					TestCompSet( CurrentModuleObject + ":RecirculatedAir", Damper( DamperNum ).DamperName, AlphArray( 5 ), AlphArray( 3 ), "Air Nodes" );
				}

				{ auto const SELECT_CASE_var( AlphArray( 7 ) );
				if ( SELECT_CASE_var == "CURRENTOCCUPANCY" ) {
					Damper( DamperNum ).OAPerPersonMode = PerPersonDCVByCurrentLevel;

				} else if ( SELECT_CASE_var == "DESIGNOCCUPANCY" ) {
					Damper( DamperNum ).OAPerPersonMode = PerPersonByDesignLevel;
				}}
				// checks on this are done later

				// Fill the Zone Equipment data with the inlet node numbers of this unit.
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
						if ( Damper( DamperNum ).OutletNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
							if ( Damper( DamperNum ).RecircIsUsed ) {
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Damper( DamperNum ).RecircAirInletNodeNum;
							} else {
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = Damper( DamperNum ).OAInletNodeNum;
							}
							ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).InNode = Damper( DamperNum ).OAInletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = Damper( DamperNum ).OutletNodeNum;
							ZoneEquipConfig( CtrlZone ).AirDistUnitHeat( SupAirIn ).OutNode = Damper( DamperNum ).OutletNodeNum;

							Damper( DamperNum ).CtrlZoneNum = CtrlZone;
							Damper( DamperNum ).ActualZoneNum = ZoneEquipConfig( CtrlZone ).ActualZoneNum;
						}
					}
				}
				Damper( DamperNum ).OARequirementsPtr = FindItemInList( AlphArray( 6 ), OARequirements );
				if ( Damper( DamperNum ).OARequirementsPtr == 0 ) {
					ShowSevereError( cAlphaFields( 6 ) + " = " + AlphArray( 6 ) + " not found." );
					ShowContinueError( "Occurs in " + cCMO_DDVarVolOA + " = " + Damper( DamperNum ).DamperName );
					ErrorsFound = true;
				} else {
					Damper( DamperNum ).NoOAFlowInputFromUser = false;

					// now fill design OA rate
					CalcOAOnlyMassFlow( DamperNum, DummyOAFlow, Damper( DamperNum ).DesignOAFlowRate );

					if ( Damper( DamperNum ).MaxAirVolFlowRate != AutoSize ) {
						ReportSizingOutput( CurrentModuleObject, Damper( DamperNum ).DamperName, "Maximum Outdoor Air Flow Rate [m3/s]", Damper( DamperNum ).DesignOAFlowRate );

						if ( Damper( DamperNum ).RecircIsUsed ) {
							Damper( DamperNum ).DesignRecircFlowRate = Damper( DamperNum ).MaxAirVolFlowRate - Damper( DamperNum ).DesignOAFlowRate;
							Damper( DamperNum ).DesignRecircFlowRate = max( 0.0, Damper( DamperNum ).DesignRecircFlowRate );
							ReportSizingOutput( CurrentModuleObject, Damper( DamperNum ).DamperName, "Maximum Recirculated Air Flow Rate [m3/s]", Damper( DamperNum ).DesignRecircFlowRate );
						} else {
							if ( Damper( DamperNum ).MaxAirVolFlowRate < Damper( DamperNum ).DesignOAFlowRate ) {
								ShowSevereError( "The value " + RoundSigDigits( Damper( DamperNum ).MaxAirVolFlowRate, 5 ) + " in " + cNumericFields( 1 ) + "is lower than the outdoor air requirement." );
								ShowContinueError( "Occurs in " + cCMO_DDVarVolOA + " = " + Damper( DamperNum ).DamperName );
								ShowContinueError( "The design outdoor air requirement is " + RoundSigDigits( Damper( DamperNum ).DesignOAFlowRate, 5 ) );
								ErrorsFound = true;
							}
						}
					}
				}

				if ( Damper( DamperNum ).OAPerPersonMode == PerPersonModeNotSet ) {
					DummyOAFlow = OARequirements( Damper( DamperNum ).OARequirementsPtr ).OAFlowPerPerson;
					if ( ( DummyOAFlow == 0.0 ) && ( lAlphaBlanks( 7 ) ) ) { // no worries
						// do nothing, okay since no per person requirement involved
					} else if ( ( DummyOAFlow > 0.0 ) && ( lAlphaBlanks( 7 ) ) ) { // missing input
						ShowSevereError( cAlphaFields( 7 ) + " was blank." );
						ShowContinueError( "Occurs in " + cCMO_DDVarVolOA + " = " + Damper( DamperNum ).DamperName );
						ShowContinueError( "Valid choices are \"CurrentOccupancy\" or \"DesignOccupancy\"" );
						ErrorsFound = true;
					} else if ( ( DummyOAFlow > 0.0 ) && ! ( lAlphaBlanks( 7 ) ) ) { // incorrect input
						ShowSevereError( cAlphaFields( 7 ) + " = " + AlphArray( 7 ) + " not a valid key choice." );
						ShowContinueError( "Occurs in " + cCMO_DDVarVolOA + " = " + Damper( DamperNum ).DamperName );
						ShowContinueError( "Valid choices are \"CurrentOccupancy\" or \"DesignOccupancy\"" );
						ErrorsFound = true;
					}
				}

				//Setup the Average damper Position output variable
				SetupOutputVariable( "Zone Air Terminal Outdoor Air Duct Damper Position []", Damper( DamperNum ).OADamperPosition, "System", "Average", Damper( DamperNum ).DamperName );
				SetupOutputVariable( "Zone Air Terminal Recirculated Air Duct Damper Position []", Damper( DamperNum ).RecircAirDamperPosition, "System", "Average", Damper( DamperNum ).DamperName );
				SetupOutputVariable( "Zone Air Terminal Outdoor Air Fraction []", Damper( DamperNum ).OAFraction, "System", "Average", Damper( DamperNum ).DamperName );

			} // end Number of Damper Loop
		}

		for ( DamperIndex = 1; DamperIndex <= NumDampers; ++DamperIndex ) {
			for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
				if ( Damper( DamperIndex ).OutletNodeNum == AirDistUnit( ADUNum ).OutletNodeNum ) {
					//          AirDistUnit(ADUNum)%InletNodeNum = Damper(DamperIndex)%InletNodeNum
					Damper( DamperIndex ).ADUNum = ADUNum;
				}
			}
			// one assumes if there isn't one assigned, it's an error?
			if ( Damper( DamperIndex ).ADUNum == 0 ) {
				// convenient String
				if ( Damper( DamperIndex ).DamperType == DualDuct_ConstantVolume ) {
					CurrentModuleObject = "ConstantVolume";
				} else if ( Damper( DamperIndex ).DamperType == DualDuct_VariableVolume ) {
					CurrentModuleObject = "VAV";
				} else if ( Damper( DamperIndex ).DamperType == DualDuct_OutdoorAir ) {
					CurrentModuleObject = "VAV:OutdoorAir";
				} else {
					CurrentModuleObject = "*invalid*";
				}
				ShowSevereError( RoutineName + "No matching List:Zone:AirTerminal for AirTerminal:DualDuct = [" + CurrentModuleObject + ',' + Damper( DamperIndex ).DamperName + "]." );
				ShowContinueError( "...should have outlet node=" + NodeID( Damper( DamperIndex ).OutletNodeNum ) );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Preceding condition(s) cause termination." );
		}

	}

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitDualDuct(
		int const DamperNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for  initializations of the Damper Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataConvergParams::HVACFlowRateToler;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataDefineEquip::AirDistUnit;
		using InputProcessor::SameString;
		using DataHeatBalance::TotPeople;
		using DataHeatBalance::People;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HotInNode;
		int ColdInNode;
		int OAInNode; // Outdoor Air Inlet Node for VAV:OutdoorAir units
		int RAInNode; // Reciruclated Air Inlet Node for VAV:OutdoorAir units
		int OutNode;
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MySizeFlag;
		static Array1D_bool MyAirLoopFlag;
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop; // Loop checking control variable
		Real64 PeopleFlow; // local sum variable, m3/s
		// FLOW:

		// Do the Begin Simulation initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumDampers );
			MySizeFlag.allocate( NumDampers );
			MyAirLoopFlag.dimension( NumDampers, true );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MassFlowSetToler = HVACFlowRateToler * 0.00001;

			MyOneTimeFlag = false;

		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			// Check to see if there is a Air Distribution Unit on the Zone Equipment List
			for ( Loop = 1; Loop <= NumDampers; ++Loop ) {
				if ( Damper( Loop ).ADUNum == 0 ) continue;
				if ( CheckZoneEquipmentList( "ZONEHVAC:AIRDISTRIBUTIONUNIT", AirDistUnit( Damper( Loop ).ADUNum ).Name ) ) continue;
				ShowSevereError( "InitDualDuct: ADU=[Air Distribution Unit," + AirDistUnit( Damper( Loop ).ADUNum ).Name + "] is not on any ZoneHVAC:EquipmentList." );
				if ( Damper( Loop ).DamperType == DualDuct_ConstantVolume ) {
					ShowContinueError( "...Dual Duct Damper=[" + cCMO_DDConstantVolume + ',' + Damper( Loop ).DamperName + "] will not be simulated." );
				} else if ( Damper( Loop ).DamperType == DualDuct_VariableVolume ) {
					ShowContinueError( "...Dual Duct Damper=[" + cCMO_DDVariableVolume + ',' + Damper( Loop ).DamperName + "] will not be simulated." );
				} else if ( Damper( Loop ).DamperType == DualDuct_OutdoorAir ) {
					ShowContinueError( "...Dual Duct Damper=[" + cCMO_DDVarVolOA + ',' + Damper( Loop ).DamperName + "] will not be simulated." );
				} else {
					ShowContinueError( "...Dual Duct Damper=[unknown/invalid," + Damper( Loop ).DamperName + "] will not be simulated." );
				}
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( DamperNum ) ) {

			SizeDualDuct( DamperNum );

			MySizeFlag( DamperNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( DamperNum ) ) {

			if ( Damper( DamperNum ).DamperType == DualDuct_ConstantVolume || Damper( DamperNum ).DamperType == DualDuct_VariableVolume ) {
				OutNode = Damper( DamperNum ).OutletNodeNum;
				HotInNode = Damper( DamperNum ).HotAirInletNodeNum;
				ColdInNode = Damper( DamperNum ).ColdAirInletNodeNum;
				Node( OutNode ).MassFlowRateMax = Damper( DamperNum ).MaxAirVolFlowRate * StdRhoAir;
				if ( Damper( DamperNum ).DamperType == DualDuct_ConstantVolume ) {
					Node( OutNode ).MassFlowRateMin = 0.0;
				} else if ( Damper( DamperNum ).DamperType == DualDuct_VariableVolume ) {
					Node( OutNode ).MassFlowRateMin = Node( OutNode ).MassFlowRateMax * Damper( DamperNum ).ZoneMinAirFrac;
				} else {
					Node( OutNode ).MassFlowRateMin = 0.0;
				}
				DamperHotAirInlet( DamperNum ).AirMassFlowRateMax = Node( OutNode ).MassFlowRateMax;
				DamperColdAirInlet( DamperNum ).AirMassFlowRateMax = Node( OutNode ).MassFlowRateMax;
				Node( HotInNode ).MassFlowRateMax = Node( OutNode ).MassFlowRateMax;
				Node( ColdInNode ).MassFlowRateMax = Node( OutNode ).MassFlowRateMax;
				Node( HotInNode ).MassFlowRateMin = 0.0;
				Node( ColdInNode ).MassFlowRateMin = 0.0;
				MyEnvrnFlag( DamperNum ) = false;

			} else if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
				// Initialize for DualDuct:VAV:OutdoorAir
				OutNode = Damper( DamperNum ).OutletNodeNum;
				OAInNode = Damper( DamperNum ).OAInletNodeNum;
				if ( Damper( DamperNum ).RecircIsUsed ) RAInNode = Damper( DamperNum ).RecircAirInletNodeNum;
				Node( OutNode ).MassFlowRateMax = Damper( DamperNum ).MaxAirMassFlowRate;
				Node( OutNode ).MassFlowRateMin = 0.0;
				DamperOAInlet( DamperNum ).AirMassFlowRateMax = Damper( DamperNum ).DesignOAFlowRate * StdRhoAir;
				if ( Damper( DamperNum ).RecircIsUsed ) {
					DamperRecircAirInlet( DamperNum ).AirMassFlowRateMax = Damper( DamperNum ).MaxAirMassFlowRate - DamperOAInlet( DamperNum ).AirMassFlowRateMax;
					Node( RAInNode ).MassFlowRateMax = DamperRecircAirInlet( DamperNum ).AirMassFlowRateMax;
					Node( RAInNode ).MassFlowRateMin = 0.0;
					DamperRecircAirInlet( DamperNum ).AirMassFlowDiffMag = 1.0e-10 * DamperRecircAirInlet( DamperNum ).AirMassFlowRateMax;
				}
				Node( OAInNode ).MassFlowRateMax = DamperOAInlet( DamperNum ).AirMassFlowRateMax;
				Node( OAInNode ).MassFlowRateMin = 0.0;
				//figure per person by design level for the OA duct.
				PeopleFlow = 0.0;
				for ( Loop = 1; Loop <= TotPeople; ++Loop ) {
					if ( People( Loop ).ZonePtr != Damper( DamperNum ).ActualZoneNum ) continue;
					int damperOAFlowMethod = OARequirements( Damper( DamperNum ).OARequirementsPtr ).OAFlowMethod;
					if ( damperOAFlowMethod == OAFlowPPer || damperOAFlowMethod == OAFlowSum || damperOAFlowMethod == OAFlowMax ) {
						PeopleFlow += People( Loop ).NumberOfPeople * OARequirements( Damper( DamperNum ).OARequirementsPtr ).OAFlowPerPerson;
					}
				}
				Damper( DamperNum ).OAPerPersonByDesignLevel = PeopleFlow;

				MyEnvrnFlag( DamperNum ) = false;
			}
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( DamperNum ) = true;
		}

		// Find air loop associated with VAV dual duct or VAV:OutdoorAir terminal units
		if ( MyAirLoopFlag( DamperNum ) ) {
			if ( Damper( DamperNum ).DamperType == DualDuct_VariableVolume || Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
				if ( Damper( DamperNum ).AirLoopNum == 0 ) {
					if ( Damper( DamperNum ).CtrlZoneNum > 0 ) {
						Damper( DamperNum ).AirLoopNum = ZoneEquipConfig( Damper( DamperNum ).CtrlZoneNum ).AirLoopNum;
					}
				} else {
					MyAirLoopFlag( DamperNum ) = false;
				}
			} else {
				MyAirLoopFlag( DamperNum ) = false;
			}
		}

		// Initialize the Inlet Nodes of the Sys
		if ( Damper( DamperNum ).DamperType == DualDuct_ConstantVolume || Damper( DamperNum ).DamperType == DualDuct_VariableVolume ) {
			HotInNode = Damper( DamperNum ).HotAirInletNodeNum;
			ColdInNode = Damper( DamperNum ).ColdAirInletNodeNum;
			OutNode = Damper( DamperNum ).OutletNodeNum;
		} else if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
			OAInNode = Damper( DamperNum ).OAInletNodeNum;
			if ( Damper( DamperNum ).RecircIsUsed ) RAInNode = Damper( DamperNum ).RecircAirInletNodeNum;
			OutNode = Damper( DamperNum ).OutletNodeNum;
		}

		if ( FirstHVACIteration ) {
			//     CALL DisplayString('Init First HVAC Iteration {'//TRIM(Damper(DamperNum)%DamperName)//'}') !-For debugging - REMOVE
			//The first time through set the mass flow rate to the Max
			//Take care of the flow rates first. For Const Vol and VAV.
			if ( Damper( DamperNum ).DamperType == DualDuct_ConstantVolume || Damper( DamperNum ).DamperType == DualDuct_VariableVolume ) {
				if ( ( Node( HotInNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( HotInNode ).MassFlowRate = DamperHotAirInlet( DamperNum ).AirMassFlowRateMax;
				} else {
					Node( HotInNode ).MassFlowRate = 0.0;
				}
				if ( ( Node( ColdInNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( ColdInNode ).MassFlowRate = DamperColdAirInlet( DamperNum ).AirMassFlowRateMax;
				} else {
					Node( ColdInNode ).MassFlowRate = 0.0;
				}
				//Next take care of the Max Avail Flow Rates
				if ( ( Node( HotInNode ).MassFlowRateMaxAvail > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( HotInNode ).MassFlowRateMaxAvail = DamperHotAirInlet( DamperNum ).AirMassFlowRateMax;
				} else {
					Node( HotInNode ).MassFlowRateMaxAvail = 0.0;
				}
				if ( ( Node( ColdInNode ).MassFlowRateMaxAvail > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( ColdInNode ).MassFlowRateMaxAvail = DamperColdAirInlet( DamperNum ).AirMassFlowRateMax;
				} else {
					Node( ColdInNode ).MassFlowRateMaxAvail = 0.0;
				}
				//The last item is to take care of the Min Avail Flow Rates
				if ( ( Node( HotInNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( HotInNode ).MassFlowRateMinAvail = DamperHotAirInlet( DamperNum ).AirMassFlowRateMax * Damper( DamperNum ).ZoneMinAirFrac;
				} else {
					Node( HotInNode ).MassFlowRateMinAvail = 0.0;
				}
				if ( ( Node( ColdInNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( ColdInNode ).MassFlowRateMinAvail = DamperColdAirInlet( DamperNum ).AirMassFlowRateMax * Damper( DamperNum ).ZoneMinAirFrac;
				} else {
					Node( ColdInNode ).MassFlowRateMinAvail = 0.0;
				}

			} else if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
				//The first time through set the mass flow rate to the Max for VAV:OutdoorAir
				if ( ( Node( OAInNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( OAInNode ).MassFlowRate = DamperOAInlet( DamperNum ).AirMassFlowRateMax;
				} else {
					Node( OAInNode ).MassFlowRate = 0.0;
				}
				if ( Damper( DamperNum ).RecircIsUsed ) {
					if ( ( Node( RAInNode ).MassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
						Node( RAInNode ).MassFlowRate = DamperRecircAirInlet( DamperNum ).AirMassFlowRateMax;
					} else {
						Node( RAInNode ).MassFlowRate = 0.0;
					}
					// clear flow history
					DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist1 = 0.0;
					DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist2 = 0.0;
					DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist3 = 0.0;
				}
				//Next take care of the Max Avail Flow Rates
				if ( ( Node( OAInNode ).MassFlowRateMaxAvail > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
					Node( OAInNode ).MassFlowRateMaxAvail = DamperOAInlet( DamperNum ).AirMassFlowRateMax;
				} else {
					Node( OAInNode ).MassFlowRateMaxAvail = 0.0;
				}
				if ( Damper( DamperNum ).RecircIsUsed ) {
					if ( ( Node( RAInNode ).MassFlowRateMaxAvail > 0.0 ) && ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) ) {
						Node( RAInNode ).MassFlowRateMaxAvail = DamperRecircAirInlet( DamperNum ).AirMassFlowRateMax;
					} else {
						Node( RAInNode ).MassFlowRateMaxAvail = 0.0;
					}
				}
				//The last item is to take care of the Min Avail Flow Rates. VAV:OutdoorAir
				Node( OAInNode ).MassFlowRateMinAvail = 0.0;
				if ( Damper( DamperNum ).RecircIsUsed ) Node( RAInNode ).MassFlowRateMinAvail = 0.0;
			}
		}

		// Initialize the Inlet Nodes of the Dampers for Const. Vol and VAV
		if ( Damper( DamperNum ).DamperType == DualDuct_ConstantVolume || Damper( DamperNum ).DamperType == DualDuct_VariableVolume ) {

			DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail = min( Node( OutNode ).MassFlowRateMax, Node( HotInNode ).MassFlowRateMaxAvail );
			DamperHotAirInlet( DamperNum ).AirMassFlowRateMinAvail = min( max( Node( OutNode ).MassFlowRateMin, Node( HotInNode ).MassFlowRateMinAvail ), Node( HotInNode ).MassFlowRateMaxAvail );

			DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail = min( Node( OutNode ).MassFlowRateMax, Node( ColdInNode ).MassFlowRateMaxAvail );
			DamperColdAirInlet( DamperNum ).AirMassFlowRateMinAvail = min( max( Node( OutNode ).MassFlowRateMin, Node( ColdInNode ).MassFlowRateMinAvail ), Node( ColdInNode ).MassFlowRateMaxAvail );

			// Do the following initializations (every time step): This should be the info from
			// the previous components outlets or the node data in this section.
			// Load the node data in this section for the component simulation
			DamperHotAirInlet( DamperNum ).AirMassFlowRate = Node( HotInNode ).MassFlowRate;
			DamperHotAirInlet( DamperNum ).AirTemp = Node( HotInNode ).Temp;
			DamperHotAirInlet( DamperNum ).AirHumRat = Node( HotInNode ).HumRat;
			DamperHotAirInlet( DamperNum ).AirEnthalpy = Node( HotInNode ).Enthalpy;
			DamperColdAirInlet( DamperNum ).AirMassFlowRate = Node( ColdInNode ).MassFlowRate;
			DamperColdAirInlet( DamperNum ).AirTemp = Node( ColdInNode ).Temp;
			DamperColdAirInlet( DamperNum ).AirHumRat = Node( ColdInNode ).HumRat;
			DamperColdAirInlet( DamperNum ).AirEnthalpy = Node( ColdInNode ).Enthalpy;

			// Initialize the Inlet Nodes of the Dampers for VAV:OutdoorAir
		} else if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
			DamperOAInlet( DamperNum ).AirMassFlowRateMaxAvail = Node( OAInNode ).MassFlowRateMaxAvail;
			DamperOAInlet( DamperNum ).AirMassFlowRateMinAvail = Node( OAInNode ).MassFlowRateMinAvail;

			// Do the following initializations (every time step): This should be the info from
			// the previous components outlets or the node data in this section.
			// Load the node data in this section for the component simulation
			DamperOAInlet( DamperNum ).AirMassFlowRate = Node( OAInNode ).MassFlowRate;
			DamperOAInlet( DamperNum ).AirTemp = Node( OAInNode ).Temp;
			DamperOAInlet( DamperNum ).AirHumRat = Node( OAInNode ).HumRat;
			DamperOAInlet( DamperNum ).AirEnthalpy = Node( OAInNode ).Enthalpy;
			if ( Damper( DamperNum ).RecircIsUsed ) {
				DamperRecircAirInlet( DamperNum ).AirMassFlowRateMaxAvail = Node( RAInNode ).MassFlowRateMaxAvail;
				DamperRecircAirInlet( DamperNum ).AirMassFlowRateMinAvail = Node( RAInNode ).MassFlowRateMinAvail;
				DamperRecircAirInlet( DamperNum ).AirMassFlowRate = Node( RAInNode ).MassFlowRate;
				DamperRecircAirInlet( DamperNum ).AirTemp = Node( RAInNode ).Temp;
				DamperRecircAirInlet( DamperNum ).AirHumRat = Node( RAInNode ).HumRat;
				DamperRecircAirInlet( DamperNum ).AirEnthalpy = Node( RAInNode ).Enthalpy;
			}
		}

	}

	void
	SizeDualDuct( int const DamperNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Dual Duct air terminal units for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
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
		std::string DamperType;

		if ( Damper( DamperNum ).MaxAirVolFlowRate == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {
				if ( Damper( DamperNum ).DamperType == DualDuct_ConstantVolume ) {
					DamperType = cCMO_DDConstantVolume;
				} else if ( Damper( DamperNum ).DamperType == DualDuct_VariableVolume ) {
					DamperType = cCMO_DDVariableVolume;
				} else if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
					DamperType = cCMO_DDVarVolOA;
				} else {
					DamperType = "Invalid/Unknown";
				}
				CheckZoneSizing( DamperType, Damper( DamperNum ).DamperName );
				Damper( DamperNum ).MaxAirVolFlowRate = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
					if ( Damper( DamperNum ).RecircIsUsed ) {
						Damper( DamperNum ).DesignRecircFlowRate = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						Damper( DamperNum ).MaxAirVolFlowRate = Damper( DamperNum ).DesignRecircFlowRate + Damper( DamperNum ).DesignOAFlowRate;
					} else {
						Damper( DamperNum ).MaxAirVolFlowRate = Damper( DamperNum ).DesignOAFlowRate;
						Damper( DamperNum ).DesignRecircFlowRate = 0.0;
					}
					Damper( DamperNum ).MaxAirMassFlowRate = Damper( DamperNum ).MaxAirVolFlowRate * StdRhoAir;
				}

				if ( Damper( DamperNum ).MaxAirVolFlowRate < SmallAirVolFlow ) {
					Damper( DamperNum ).MaxAirVolFlowRate = 0.0;
					Damper( DamperNum ).MaxAirMassFlowRate = 0.0;
					Damper( DamperNum ).DesignOAFlowRate = 0.0;
					Damper( DamperNum ).DesignRecircFlowRate = 0.0;
				}
				ReportSizingOutput( DamperType, Damper( DamperNum ).DamperName, "Maximum Air Flow Rate [m3/s]", Damper( DamperNum ).MaxAirVolFlowRate );
				if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {
					ReportSizingOutput( DamperType, Damper( DamperNum ).DamperName, "Maximum Outdoor Air Flow Rate [m3/s]", Damper( DamperNum ).DesignOAFlowRate );
					if ( Damper( DamperNum ).RecircIsUsed ) {
						ReportSizingOutput( DamperType, Damper( DamperNum ).DamperName, "Maximum Recirculated Air Flow Rate [m3/s]", Damper( DamperNum ).DesignRecircFlowRate );
					}
				}

			}

		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimDualDuctConstVol(
		int const DamperNum,
		int const ZoneNum,
		int const ZoneNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   Jan 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple mixing damper.

		// METHODOLOGY EMPLOYED:
		// There is method to this madness.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		//unused0909   USE DataHeatBalFanSys, ONLY: Mat
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdbFnHW;
		using DataHVACGlobals::SmallTempDiff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlow; // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
		Real64 HumRat; // [Kg Moisture / Kg dry air]
		Real64 Enthalpy; // [Watts]
		Real64 Temperature; // [C]
		Real64 QTotLoad; // [W]
		Real64 QZnReq; // [W]
		Real64 CpAirZn;
		Real64 CpAirSysHot;
		Real64 CpAirSysCold;

		// Get the calculated load from the Heat Balance from ZoneSysEnergyDemand
		QTotLoad = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		// Need the design MassFlowRate for calculations
		if ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) {
			MassFlow = DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail / 2.0 + DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail / 2.0;
		} else {
			MassFlow = 0.0;
		}
		// If there is massflow then need to provide the correct amount of total
		//  required zone energy
		if ( MassFlow > SmallMassFlow ) {
			CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
			QZnReq = QTotLoad + MassFlow * CpAirZn * Node( ZoneNodeNum ).Temp;
			// If the enthalpy is the same for the hot and cold duct then there would be a
			//  divide by zero so for heating or cooling set the damper to one max flow
			//  or the other.
			if ( std::abs( DamperColdAirInlet( DamperNum ).AirTemp - DamperHotAirInlet( DamperNum ).AirTemp ) > SmallTempDiff ) {
				// CpAirSysHot = PsyCpAirFnWTdb(DamperHotAirInlet(DamperNum)%AirHumRat,DamperHotAirInlet(DamperNum)%AirTemp)
				// CpAirSysCold= PsyCpAirFnWTdb(DamperColdAirInlet(DamperNum)%AirHumRat,DamperColdAirInlet(DamperNum)%AirTemp)
				CpAirSysHot = CpAirZn;
				CpAirSysCold = CpAirZn;
				//Determine the Cold Air Mass Flow Rate
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = ( QZnReq - MassFlow * CpAirSysHot * DamperHotAirInlet( DamperNum ).AirTemp ) / ( CpAirSysCold * DamperColdAirInlet( DamperNum ).AirTemp - CpAirSysHot * DamperHotAirInlet( DamperNum ).AirTemp );
			} else if ( ( QTotLoad > 0.0 ) && ( DamperHotAirInlet( DamperNum ).AirMassFlowRate > 0.0 ) ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			} else {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = MassFlow;
			}
			// Check to make sure that the calculated flow is not greater than the available flows
			if ( DamperColdAirInlet( DamperNum ).AirMassFlowRate > DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
			} else if ( DamperColdAirInlet( DamperNum ).AirMassFlowRate < DamperColdAirInlet( DamperNum ).AirMassFlowRateMinAvail ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = DamperColdAirInlet( DamperNum ).AirMassFlowRateMinAvail;
			}
			// Using Mass Continuity to determine the other duct flow quantity
			DamperHotAirInlet( DamperNum ).AirMassFlowRate = MassFlow - DamperColdAirInlet( DamperNum ).AirMassFlowRate;
			if ( DamperHotAirInlet( DamperNum ).AirMassFlowRate > DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail ) {
				DamperHotAirInlet( DamperNum ).AirMassFlowRate = DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
			} else if ( DamperHotAirInlet( DamperNum ).AirMassFlowRate < DamperHotAirInlet( DamperNum ).AirMassFlowRateMinAvail ) {
				DamperHotAirInlet( DamperNum ).AirMassFlowRate = DamperHotAirInlet( DamperNum ).AirMassFlowRateMinAvail;
			}
			MassFlow = DamperColdAirInlet( DamperNum ).AirMassFlowRate + DamperHotAirInlet( DamperNum ).AirMassFlowRate;
		} else {
			// System is Off set massflow to 0.0
			MassFlow = 0.0;
		}
		if ( MassFlow > SmallMassFlow ) {
			// After flows are calculated then calculate the mixed air flow properties.
			HumRat = ( DamperHotAirInlet( DamperNum ).AirHumRat * DamperHotAirInlet( DamperNum ).AirMassFlowRate + DamperColdAirInlet( DamperNum ).AirHumRat * DamperColdAirInlet( DamperNum ).AirMassFlowRate ) / MassFlow;
			Enthalpy = ( DamperHotAirInlet( DamperNum ).AirEnthalpy * DamperHotAirInlet( DamperNum ).AirMassFlowRate + DamperColdAirInlet( DamperNum ).AirEnthalpy * DamperColdAirInlet( DamperNum ).AirMassFlowRate ) / MassFlow;

			// If there is no air flow than calculate the No Flow conditions
		} else {
			DamperColdAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			DamperHotAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			HumRat = ( DamperHotAirInlet( DamperNum ).AirHumRat + DamperColdAirInlet( DamperNum ).AirHumRat ) / 2.0;
			Enthalpy = ( DamperHotAirInlet( DamperNum ).AirEnthalpy + DamperColdAirInlet( DamperNum ).AirEnthalpy ) / 2.0;
		}
		Temperature = PsyTdbFnHW( Enthalpy, HumRat );

		// Load all properties in the damper outlet
		DamperOutlet( DamperNum ).AirTemp = Temperature;
		DamperOutlet( DamperNum ).AirHumRat = HumRat;
		DamperOutlet( DamperNum ).AirMassFlowRate = MassFlow;
		DamperOutlet( DamperNum ).AirMassFlowRateMaxAvail = MassFlow;
		DamperOutlet( DamperNum ).AirMassFlowRateMinAvail = min( DamperHotAirInlet( DamperNum ).AirMassFlowRateMinAvail, DamperColdAirInlet( DamperNum ).AirMassFlowRateMinAvail );
		DamperOutlet( DamperNum ).AirEnthalpy = Enthalpy;

		//Calculate the hot and cold damper position in %
		if ( ( DamperHotAirInlet( DamperNum ).AirMassFlowRateMax == 0.0 ) || ( DamperColdAirInlet( DamperNum ).AirMassFlowRateMax == 0.0 ) ) {
			Damper( DamperNum ).ColdAirDamperPosition = 0.0;
			Damper( DamperNum ).HotAirDamperPosition = 0.0;
		} else {
			Damper( DamperNum ).ColdAirDamperPosition = DamperColdAirInlet( DamperNum ).AirMassFlowRate / DamperColdAirInlet( DamperNum ).AirMassFlowRateMax;
			Damper( DamperNum ).HotAirDamperPosition = DamperHotAirInlet( DamperNum ).AirMassFlowRate / DamperHotAirInlet( DamperNum ).AirMassFlowRateMax;
		}

	}

	void
	SimDualDuctVarVol(
		int const DamperNum,
		int const ZoneNum,
		int const ZoneNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   Jan 2000
		//       MODIFIED       na
		//                      TH 3/2012: added supply air flow adjustment based on zone maximum outdoor
		//                                 air fraction - a TRACE feature
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple mixing damper.

		// METHODOLOGY EMPLOYED:
		// There is method to this madness.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		//unused0909   USE DataHeatBalFanSys, ONLY: Mat
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdbFnHW;
		using DataHVACGlobals::SmallTempDiff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlow; // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
		Real64 HumRat; // [Kg Moisture / Kg dry air]
		Real64 Enthalpy; // [Watts]
		Real64 Temperature; // [C]
		Real64 QTotLoad; // [W]
		Real64 QZnReq; // [W]
		Real64 CpAirZn; // specific heat of zone air
		Real64 CpAirSysHot;
		Real64 CpAirSysCold;
		Real64 MassFlowBasedOnOA; // Supply air flow rate based on minimum OA requirement
		Real64 AirLoopOAFrac; // fraction of outdoor air entering air loop outside air system

		// The calculated load from the Heat Balance
		QTotLoad = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		//Calculate all of the required Cp's
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
		// CpAirSysHot = PsyCpAirFnWTdb(DamperHotAirInlet(DamperNum)%AirHumRat,DamperHotAirInlet(DamperNum)%AirTemp)
		// CpAirSysCold= PsyCpAirFnWTdb(DamperColdAirInlet(DamperNum)%AirHumRat,DamperColdAirInlet(DamperNum)%AirTemp)
		CpAirSysHot = CpAirZn;
		CpAirSysCold = CpAirZn;

		// calculate supply air flow rate based on user specified OA requirement
		CalcOAMassFlow( DamperNum, MassFlowBasedOnOA, AirLoopOAFrac );

		//Then depending on if the Load is for heating or cooling it is handled differently.  First
		// the massflow rate of either heating or cooling is determined to meet the entire load.  Then
		// if the massflow is below the minimum or greater than the Max it is set to either the Min
		// or the Max as specified for the VAV model.
		if ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) == 0.0 ) {
			// System is Off set massflow to 0.0
			MassFlow = 0.0;

		} else if ( ( QTotLoad > 0.0 ) && ( DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail > 0.0 ) ) {
			// Then heating is needed
			// Next check for the denominator equal to zero
			if ( std::abs( ( CpAirSysHot * DamperHotAirInlet( DamperNum ).AirTemp ) - ( CpAirZn * Node( ZoneNodeNum ).Temp ) ) / CpAirZn > SmallTempDiff ) {
				MassFlow = QTotLoad / ( CpAirSysHot * DamperHotAirInlet( DamperNum ).AirTemp - CpAirZn * Node( ZoneNodeNum ).Temp );
			} else {
				// If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
				MassFlow = DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
			}
			//Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
			if ( MassFlow <= ( DamperHotAirInlet( DamperNum ).AirMassFlowRateMax * Damper( DamperNum ).ZoneMinAirFrac ) ) {
				MassFlow = DamperHotAirInlet( DamperNum ).AirMassFlowRateMax * Damper( DamperNum ).ZoneMinAirFrac;
				MassFlow = max( MassFlow, DamperHotAirInlet( DamperNum ).AirMassFlowRateMinAvail );
			} else if ( MassFlow >= DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail ) {
				MassFlow = DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
			}

			// Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
			if ( ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor > 1.0 ) {
				MassFlow *= ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor;
			}

			MassFlow = max( MassFlow, MassFlowBasedOnOA );
			MassFlow = min( MassFlow, DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail );

		} else if ( ( QTotLoad < 0.0 ) && ( DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail > 0.0 ) ) {
			// Then cooling is required
			// Next check for the denominator equal to zero
			if ( std::abs( ( CpAirSysCold * DamperColdAirInlet( DamperNum ).AirTemp ) - ( CpAirZn * Node( ZoneNodeNum ).Temp ) ) / CpAirZn > SmallTempDiff ) {
				MassFlow = QTotLoad / ( CpAirSysCold * DamperColdAirInlet( DamperNum ).AirTemp - CpAirZn * Node( ZoneNodeNum ).Temp );
			} else {
				// If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
				MassFlow = DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
			}

			//Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
			if ( ( MassFlow <= ( DamperColdAirInlet( DamperNum ).AirMassFlowRateMax * Damper( DamperNum ).ZoneMinAirFrac ) ) && ( MassFlow >= 0.0 ) ) {
				MassFlow = DamperColdAirInlet( DamperNum ).AirMassFlowRateMax * Damper( DamperNum ).ZoneMinAirFrac;
				MassFlow = max( MassFlow, DamperColdAirInlet( DamperNum ).AirMassFlowRateMinAvail );
			} else if ( MassFlow < 0.0 ) {
				MassFlow = DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
			} else if ( MassFlow >= DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail ) {
				MassFlow = DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
			}

			// Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
			if ( ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor > 1.0 ) {
				MassFlow *= ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor;
			}

			MassFlow = max( MassFlow, MassFlowBasedOnOA );
			MassFlow = min( MassFlow, DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail );

		} else if ( ( DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail > 0.0 ) || ( DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail > 0.0 ) ) {
			// No Load on Zone set to mixed condition
			MassFlow = ( DamperHotAirInlet( DamperNum ).AirMassFlowRateMax / 2.0 ) * Damper( DamperNum ).ZoneMinAirFrac + DamperColdAirInlet( DamperNum ).AirMassFlowRateMax / 2.0 * Damper( DamperNum ).ZoneMinAirFrac;

			// Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
			if ( ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor > 1.0 ) {
				MassFlow *= ZoneSysEnergyDemand( ZoneNum ).SupplyAirAdjustFactor;
			}

			MassFlow = max( MassFlow, MassFlowBasedOnOA );
			MassFlow = min( MassFlow, ( DamperHotAirInlet( DamperNum ).AirMassFlowRateMaxAvail + DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail ) );

		} else {
			// System is Off set massflow to 0.0
			MassFlow = 0.0;
		}

		//Now the massflow for heating or cooling has been determined and if the massflow was reset to the
		// Min or Max we will need to mix the hot and cold deck to meet the zone load.  Knowing the enthalpy
		// of the zone and the hot and cold air flows we can determine exactly by using the Energy and Continuity
		// Eqns.  Of course we have to make sure that we are within the Min and Max flow conditions.
		if ( MassFlow > SmallMassFlow ) {
			//Determine the enthalpy required from Zone enthalpy and the zone load.
			QZnReq = QTotLoad + MassFlow * CpAirZn * Node( ZoneNodeNum ).Temp;
			//Using the known enthalpies the cold air inlet mass flow is determined.  If the enthalpy of the hot and cold
			// air streams are equal the IF-Then block handles that condition.
			if ( std::abs( DamperColdAirInlet( DamperNum ).AirTemp - DamperHotAirInlet( DamperNum ).AirTemp ) > SmallTempDiff ) {
				//Calculate the Cold air mass flow rate
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = ( QZnReq - MassFlow * CpAirSysHot * DamperHotAirInlet( DamperNum ).AirTemp ) / ( CpAirSysCold * DamperColdAirInlet( DamperNum ).AirTemp - CpAirSysHot * DamperHotAirInlet( DamperNum ).AirTemp );
			} else if ( ( QTotLoad > 0.0 ) && ( DamperHotAirInlet( DamperNum ).AirMassFlowRate > 0.0 ) ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			} else {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = MassFlow;
			}

			//Need to make sure that the flows are within limits
			if ( DamperColdAirInlet( DamperNum ).AirMassFlowRate > DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = DamperColdAirInlet( DamperNum ).AirMassFlowRateMaxAvail;

				//These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
			} else if ( DamperColdAirInlet( DamperNum ).AirMassFlowRate < 0.0 ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			} else if ( DamperColdAirInlet( DamperNum ).AirMassFlowRate > MassFlow ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = MassFlow;
			}
			//Using Mass Continuity to determine the other duct flow quantity
			DamperHotAirInlet( DamperNum ).AirMassFlowRate = MassFlow - DamperColdAirInlet( DamperNum ).AirMassFlowRate;

			if ( DamperHotAirInlet( DamperNum ).AirMassFlowRate < MassFlowSetToler ) {
				DamperHotAirInlet( DamperNum ).AirMassFlowRate = 0.0;
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = MassFlow;
			} else if ( DamperColdAirInlet( DamperNum ).AirMassFlowRate < MassFlowSetToler ) {
				DamperColdAirInlet( DamperNum ).AirMassFlowRate = 0.0;
				DamperHotAirInlet( DamperNum ).AirMassFlowRate = MassFlow;
			}

			//After the flow rates are determined the properties are calculated.
			HumRat = ( DamperHotAirInlet( DamperNum ).AirHumRat * DamperHotAirInlet( DamperNum ).AirMassFlowRate + DamperColdAirInlet( DamperNum ).AirHumRat * DamperColdAirInlet( DamperNum ).AirMassFlowRate ) / MassFlow;
			Enthalpy = ( DamperHotAirInlet( DamperNum ).AirEnthalpy * DamperHotAirInlet( DamperNum ).AirMassFlowRate + DamperColdAirInlet( DamperNum ).AirEnthalpy * DamperColdAirInlet( DamperNum ).AirMassFlowRate ) / MassFlow;

			//IF the system is OFF the properties are calculated for this special case.
		} else {
			DamperColdAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			DamperHotAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			HumRat = ( DamperHotAirInlet( DamperNum ).AirHumRat + DamperColdAirInlet( DamperNum ).AirHumRat ) / 2.0;
			Enthalpy = ( DamperHotAirInlet( DamperNum ).AirEnthalpy + DamperColdAirInlet( DamperNum ).AirEnthalpy ) / 2.0;
		}
		Temperature = PsyTdbFnHW( Enthalpy, HumRat );

		DamperOutlet( DamperNum ).AirTemp = Temperature;
		DamperOutlet( DamperNum ).AirHumRat = HumRat;
		DamperOutlet( DamperNum ).AirMassFlowRate = MassFlow;
		DamperOutlet( DamperNum ).AirMassFlowRateMaxAvail = MassFlow;
		DamperOutlet( DamperNum ).AirMassFlowRateMinAvail = Damper( DamperNum ).ZoneMinAirFrac * DamperHotAirInlet( DamperNum ).AirMassFlowRateMax;
		DamperOutlet( DamperNum ).AirEnthalpy = Enthalpy;
		Damper( DamperNum ).OutdoorAirFlowRate = MassFlow * AirLoopOAFrac;

		//Calculate the hot and cold damper position in %
		if ( ( DamperHotAirInlet( DamperNum ).AirMassFlowRateMax == 0.0 ) || ( DamperColdAirInlet( DamperNum ).AirMassFlowRateMax == 0.0 ) ) {
			Damper( DamperNum ).ColdAirDamperPosition = 0.0;
			Damper( DamperNum ).HotAirDamperPosition = 0.0;
		} else {
			Damper( DamperNum ).ColdAirDamperPosition = DamperColdAirInlet( DamperNum ).AirMassFlowRate / DamperColdAirInlet( DamperNum ).AirMassFlowRateMax;
			Damper( DamperNum ).HotAirDamperPosition = DamperHotAirInlet( DamperNum ).AirMassFlowRate / DamperHotAirInlet( DamperNum ).AirMassFlowRateMax;
		}

	}

	void
	SimDualDuctVAVOutdoorAir(
		int const DamperNum,
		int const ZoneNum,
		int const ZoneNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Clayton Miller
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       B. Griffith, Dec 2010, major rework
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Designed to accommodate for systems with outdoor air (OA) and recirculated air (RA)
		// as two separate air streams to controlled at the zone level in a dual duct system.

		// METHODOLOGY EMPLOYED:
		// The terminal unit is be designed to set the airflow of the of the OA stream at the zone
		// level based on the zonal ventilation requirements and the RA stream flowrate of recirculated
		// cooling air stream in order to meet the remaining thermal load.
		// If the zone calls for cooling but the inlet air temperature is too warm, recirc side set to zero
		// if the zone calls for heating and the inlet air is warm enough, modulate damper to meet load
		// if the zone calls for heating and the inlet air is too cold, zero flow (will not control sans reheat)

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdbFnHW;
		using namespace DataGlobals;
		using General::TrimSigDigits;
		using DataHeatBalFanSys::ZoneThermostatSetPointLo;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;
		using DataHVACGlobals::SmallTempDiff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlowMax; // [kg/sec]   Maximum Mass Flow Rate from OA and Recirc Inlets
		Real64 HumRat; // [Kg Moisture / Kg dry air]
		Real64 Enthalpy; // [Watts]
		Real64 Temperature; // [C]
		Real64 QTotLoadRemain; // [W]
		Real64 QtoHeatSPRemain; // [W]
		Real64 QtoCoolSPRemain; // [W]
		//  REAL(r64) :: QTotRemainAdjust  ! [W]
		Real64 QtoHeatSPRemainAdjust; // [W]
		Real64 QtoCoolSPRemainAdjust; // [W]
		Real64 QOALoadToHeatSP; // [W]
		Real64 QOALoadToCoolSP; // [W]
		Real64 QOALoad; // Amount of cooling load accounted for by OA Stream [W]
		Real64 QRALoad; // Amount of cooling load accounted for by Recirc Stream [W]
		Real64 CpAirZn; // specific heat of zone air
		Real64 CpAirSysOA; // specific heat of outdoor air
		Real64 CpAirSysRA; // specific heat of recirculated air
		Real64 OAMassFlow; // Supply air flow rate based on minimum OA requirement - for printing
		Real64 TotMassFlow; // [kg/sec]   Total Mass Flow Rate from OA and Recirc Inlets
		int OAInletNodeNum;
		int RecircInletNodeNum;

		OAInletNodeNum = Damper( DamperNum ).OAInletNodeNum;
		if ( Damper( DamperNum ).RecircIsUsed ) {
			RecircInletNodeNum = Damper( DamperNum ).RecircAirInletNodeNum;
		}
		// Calculate required ventilation air flow rate based on user specified OA requirement
		CalcOAOnlyMassFlow( DamperNum, OAMassFlow );

		// The calculated load from the Heat Balance, adjusted for any equipment sequenced before terminal
		QTotLoadRemain = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		QtoHeatSPRemain = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		QtoCoolSPRemain = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;

		//Calculate all of the required Cp's
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
		CpAirSysOA = PsyCpAirFnWTdb( Node( OAInletNodeNum ).HumRat, Node( OAInletNodeNum ).Temp );
		if ( Damper( DamperNum ).RecircIsUsed ) CpAirSysRA = PsyCpAirFnWTdb( Node( RecircInletNodeNum ).HumRat, Node( RecircInletNodeNum ).Temp );

		// Set the OA Damper to the calculated ventilation flow rate
		DamperOAInlet( DamperNum ).AirMassFlowRate = OAMassFlow;
		//Need to make sure that the OA flows are within limits
		if ( DamperOAInlet( DamperNum ).AirMassFlowRate > DamperOAInlet( DamperNum ).AirMassFlowRateMaxAvail ) {
			DamperOAInlet( DamperNum ).AirMassFlowRate = DamperOAInlet( DamperNum ).AirMassFlowRateMaxAvail;
		} else if ( DamperOAInlet( DamperNum ).AirMassFlowRate < 0.0 ) {
			DamperOAInlet( DamperNum ).AirMassFlowRate = 0.0;
		}

		//..Find the amount of load that the OAMassFlow accounted for
		if ( std::abs( ( CpAirSysOA * DamperOAInlet( DamperNum ).AirTemp ) - ( CpAirZn * Node( ZoneNodeNum ).Temp ) ) / CpAirZn > SmallTempDiff ) {
			QOALoad = DamperOAInlet( DamperNum ).AirMassFlowRate * ( CpAirSysOA * DamperOAInlet( DamperNum ).AirTemp - CpAirZn * Node( ZoneNodeNum ).Temp );

			QOALoadToHeatSP = DamperOAInlet( DamperNum ).AirMassFlowRate * ( CpAirSysOA * DamperOAInlet( DamperNum ).AirTemp - CpAirZn * ZoneThermostatSetPointLo( ZoneNum ) );
			QOALoadToCoolSP = DamperOAInlet( DamperNum ).AirMassFlowRate * ( CpAirSysOA * DamperOAInlet( DamperNum ).AirTemp - CpAirZn * ZoneThermostatSetPointHi( ZoneNum ) );

		} else {
			QOALoad = 0.0;
			QOALoadToHeatSP = 0.0;
			QOALoadToCoolSP = 0.0;
		}

		if ( Damper( DamperNum ).RecircIsUsed ) {

			//correct load for recirc side to account for impact of OA side
			// QTotRemainAdjust      = QTotLoadRemain  - QOALoad
			QtoHeatSPRemainAdjust = QtoHeatSPRemain - QOALoadToHeatSP;
			QtoCoolSPRemainAdjust = QtoCoolSPRemain - QOALoadToCoolSP;

			if ( QtoCoolSPRemainAdjust < 0.0 ) {
				QRALoad = QtoCoolSPRemainAdjust;
			} else if ( QtoHeatSPRemainAdjust > 0.0 ) {
				QRALoad = QtoHeatSPRemainAdjust;
			} else {
				QRALoad = 0.0;
			}

			//  IF (QTotLoadRemain == 0.0d0) THEN  ! floating in deadband
			//    IF ((QTotRemainAdjust < 0.0d0) .AND. (QtoCoolSPRemainAdjust < 0.0d0)) THEN !really need cooling
			//      QRALoad = QtoCoolSPRemainAdjust
			//    ELSEIF ((QTotRemainAdjust > 0.0d0) .AND. (QtoHeatSPRemainAdjust > 0.0d0)) THEN ! really need heating
			//      QRALoad = QtoHeatSPRemainAdjust
			//    ELSE
			//      QRALoad = 0.0 ! still floating in deadband even with impact of OA side
			//    ENDIF
			//  ELSE
			//    QRALoad = QTotRemainAdjust
			//  ENDIF

			if ( QRALoad < 0.0 ) { // cooling
				if ( ( DamperRecircAirInlet( DamperNum ).AirTemp - Node( ZoneNodeNum ).Temp ) < -0.5 ) { // can cool
					//  Find the Mass Flow Rate of the RA Stream needed to meet the zone cooling load
					if ( std::abs( ( CpAirSysRA * DamperRecircAirInlet( DamperNum ).AirTemp ) - ( CpAirZn * Node( ZoneNodeNum ).Temp ) ) / CpAirZn > SmallTempDiff ) {
						DamperRecircAirInlet( DamperNum ).AirMassFlowRate = QRALoad / ( CpAirSysRA * DamperRecircAirInlet( DamperNum ).AirTemp - CpAirZn * Node( ZoneNodeNum ).Temp );
					}
				} else {
					DamperRecircAirInlet( DamperNum ).AirMassFlowRate = 0.0;
				}

			} else if ( QRALoad > 0.0 ) { // heating
				//    IF ((DamperRecircAirInlet(DamperNum)%AirTemp - Node(ZoneNodeNum)%Temp) > 2.0d0)  THEN ! can heat
				//      DamperRecircAirInlet(DamperNum)%AirMassFlowRate = QRALoad / &
				//                         (CpAirSysRA*DamperRecircAirInlet(DamperNum)%AirTemp - CpAirZn*Node(ZoneNodeNum)%Temp)
				//    ELSE
				DamperRecircAirInlet( DamperNum ).AirMassFlowRate = 0.0;
				//    ENDIF

			} else { // none needed.
				DamperRecircAirInlet( DamperNum ).AirMassFlowRate = 0.0;

			}

			//Need to make sure that the RA flows are within limits
			if ( DamperRecircAirInlet( DamperNum ).AirMassFlowRate > DamperRecircAirInlet( DamperNum ).AirMassFlowRateMaxAvail ) {
				DamperRecircAirInlet( DamperNum ).AirMassFlowRate = DamperRecircAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
				//These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
			} else if ( DamperRecircAirInlet( DamperNum ).AirMassFlowRate < 0.0 ) {
				DamperRecircAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			}

		} else {
			DamperRecircAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			DamperRecircAirInlet( DamperNum ).AirMassFlowRateMaxAvail = 0.0;
		} // recirc used

		// look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
		// equipment iteration. If detected, set flow rate to previous value.
		if ( ( ( std::abs( DamperRecircAirInlet( DamperNum ).AirMassFlowRate - DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist2 ) < DamperRecircAirInlet( DamperNum ).AirMassFlowDiffMag ) || ( std::abs( DamperRecircAirInlet( DamperNum ).AirMassFlowRate - DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist3 ) < DamperRecircAirInlet( DamperNum ).AirMassFlowDiffMag ) ) && ( std::abs( DamperRecircAirInlet( DamperNum ).AirMassFlowRate - DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist1 ) >= DamperRecircAirInlet( DamperNum ).AirMassFlowDiffMag ) ) {
			if ( DamperRecircAirInlet( DamperNum ).AirMassFlowRate > 0.0 ) {
				DamperRecircAirInlet( DamperNum ).AirMassFlowRate = DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist1;
			}
		}

		// Find the Max Box Flow Rate.
		MassFlowMax = DamperOAInlet( DamperNum ).AirMassFlowRateMaxAvail + DamperRecircAirInlet( DamperNum ).AirMassFlowRateMaxAvail;
		if ( GetCurrentScheduleValue( Damper( DamperNum ).SchedPtr ) > 0.0 ) {
			TotMassFlow = DamperOAInlet( DamperNum ).AirMassFlowRate + DamperRecircAirInlet( DamperNum ).AirMassFlowRate;
		} else {
			TotMassFlow = 0.0;
		}

		if ( TotMassFlow > SmallMassFlow ) {

			// If the sum of the two air streams' flow is greater than the Max Box Flow Rate then reset the RA Stream
			if ( TotMassFlow > MassFlowMax ) {
				DamperRecircAirInlet( DamperNum ).AirMassFlowRate = MassFlowMax - DamperOAInlet( DamperNum ).AirMassFlowRate;
			}
			//After the flow rates are determined the properties are calculated.
			TotMassFlow = DamperOAInlet( DamperNum ).AirMassFlowRate + DamperRecircAirInlet( DamperNum ).AirMassFlowRate;
			if ( TotMassFlow > SmallMassFlow ) {
				HumRat = ( DamperOAInlet( DamperNum ).AirHumRat * DamperOAInlet( DamperNum ).AirMassFlowRate + DamperRecircAirInlet( DamperNum ).AirHumRat * DamperRecircAirInlet( DamperNum ).AirMassFlowRate ) / TotMassFlow;
				Enthalpy = ( DamperOAInlet( DamperNum ).AirEnthalpy * DamperOAInlet( DamperNum ).AirMassFlowRate + DamperRecircAirInlet( DamperNum ).AirEnthalpy * DamperRecircAirInlet( DamperNum ).AirMassFlowRate ) / TotMassFlow;
			} else {
				HumRat = ( DamperRecircAirInlet( DamperNum ).AirHumRat + DamperOAInlet( DamperNum ).AirHumRat ) / 2.0;
				Enthalpy = ( DamperRecircAirInlet( DamperNum ).AirEnthalpy + DamperOAInlet( DamperNum ).AirEnthalpy ) / 2.0;
			}
		} else {

			// The Max Box Flow Rate is zero and the box is off.
			DamperRecircAirInlet( DamperNum ).AirMassFlowRate = 0.0;
			DamperOAInlet( DamperNum ).AirMassFlowRate = 0.0;
			HumRat = ( DamperRecircAirInlet( DamperNum ).AirHumRat + DamperOAInlet( DamperNum ).AirHumRat ) / 2.0;
			Enthalpy = ( DamperRecircAirInlet( DamperNum ).AirEnthalpy + DamperOAInlet( DamperNum ).AirEnthalpy ) / 2.0;
		}

		Temperature = PsyTdbFnHW( Enthalpy, HumRat );

		DamperOutlet( DamperNum ).AirTemp = Temperature;
		DamperOutlet( DamperNum ).AirHumRat = HumRat;
		DamperOutlet( DamperNum ).AirMassFlowRate = TotMassFlow;
		DamperOutlet( DamperNum ).AirMassFlowRateMaxAvail = MassFlowMax;
		DamperOutlet( DamperNum ).AirEnthalpy = Enthalpy;

		//Calculate the OA and RA damper position in %
		if ( Damper( DamperNum ).RecircIsUsed ) {
			if ( DamperRecircAirInlet( DamperNum ).AirMassFlowRateMax == 0.0 ) { //protect div by zero
				Damper( DamperNum ).RecircAirDamperPosition = 0.0;
			} else {
				Damper( DamperNum ).RecircAirDamperPosition = DamperRecircAirInlet( DamperNum ).AirMassFlowRate / DamperRecircAirInlet( DamperNum ).AirMassFlowRateMax;
			}
		}

		if ( DamperOAInlet( DamperNum ).AirMassFlowRateMax == 0.0 ) { //protect div by zero
			Damper( DamperNum ).OADamperPosition = 0.0;
		} else {
			Damper( DamperNum ).OADamperPosition = DamperOAInlet( DamperNum ).AirMassFlowRate / DamperOAInlet( DamperNum ).AirMassFlowRateMax;
		}

		//Calculate OAFraction of mixed air after the box
		if ( TotMassFlow > 0 ) {
			if ( Damper( DamperNum ).RecircIsUsed ) {
				if ( DamperOAInlet( DamperNum ).AirMassFlowRate == 0.0 ) {
					Damper( DamperNum ).OAFraction = 0.0;
				} else if ( DamperRecircAirInlet( DamperNum ).AirMassFlowRate == 0.0 ) {
					Damper( DamperNum ).OAFraction = 1.0;
				} else {
					Damper( DamperNum ).OAFraction = DamperOAInlet( DamperNum ).AirMassFlowRate / TotMassFlow;
				}
			} else {
				Damper( DamperNum ).OAFraction = 1.0;
			}
		} else {
			Damper( DamperNum ).OAFraction = 0.0;
		}

		DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist3 = DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist2;
		DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist2 = DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist1;
		DamperRecircAirInlet( DamperNum ).AirMassFlowRateHist1 = DamperRecircAirInlet( DamperNum ).AirMassFlowRate;

	}

	void
	CalcOAMassFlow(
		int const DamperNum, // index to terminal unit
		Real64 & SAMassFlow, // outside air based on optional user input
		Real64 & AirLoopOAFrac // outside air based on optional user input
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad (FSEC)
		//       DATE WRITTEN   Mar 2010
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
		if ( Damper( DamperNum ).CtrlZoneNum > 0 ) AirLoopNum = ZoneEquipConfig( Damper( DamperNum ).CtrlZoneNum ).AirLoopNum;

		// Calculate the amount of OA based on optional user inputs
		if ( AirLoopNum > 0 ) {
			AirLoopOAFrac = AirLoopFlow( AirLoopNum ).OAFrac;
			// If no additional input from user, RETURN from subroutine
			if ( Damper( DamperNum ).NoOAFlowInputFromUser ) return;
			// Calculate outdoor air flow rate, zone multipliers are applied in GetInput
			if ( AirLoopOAFrac > 0.0 ) {
				OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( Damper( DamperNum ).OARequirementsPtr, Damper( DamperNum ).ActualZoneNum, AirLoopControlInfo( AirLoopNum ).AirLoopDCVFlag, UseMinOASchFlag );
				RhoAir = PsyRhoAirFnPbTdbW( Node( Damper( DamperNum ).OutletNodeNum ).Press, Node( Damper( DamperNum ).OutletNodeNum ).Temp, Node( Damper( DamperNum ).OutletNodeNum ).HumRat );
				OAMassFlow = OAVolumeFlowRate * RhoAir;

				// convert OA mass flow rate to supply air flow rate based on air loop OA fraction
				SAMassFlow = OAMassFlow / AirLoopOAFrac;

			}

		}

	}

	void
	CalcOAOnlyMassFlow(
		int const DamperNum, // index to terminal unit
		Real64 & OAMassFlow, // outside air flow from user input kg/s
		Optional< Real64 > MaxOAVolFlow // design level for outside air m3/s
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         C. Miller (Mod of CaclOAMassFlow by R. Raustad (FSEC))
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       B. Griffith, Dec 2010 clean up, sizing optional, scheduled OA
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates the amount of outside air required based on optional user input. Returns
		// ONLY calculated OAMassFlow without consideration of AirLoopOAFrac. Used for
		// the DualDuct:VAV:OutdoorAir object which does not mix OA with RA

		// METHODOLOGY EMPLOYED:
		// User input defines method used to calculate OA.

		// REFERENCES:

		// Using/Aliasing
		using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
		using Psychrometrics::PsyRhoAirFnPbTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		bool const UseMinOASchFlag( true ); // Always use min OA schedule in calculations.
		static std::string const RoutineName( "HVACDualDuctSystem:CalcOAOnlyMassFlow" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 RhoAir; // density of terminal unit inlet air
		Real64 OAVolumeFlowRate; // outside air volume flow rate (m3/s)
		bool UseOccSchFlag; // TRUE = use actual occupancy, FALSE = use total zone people
		bool PerPersonNotSet;

		// Calculate the amount of OA based on optional user inputs
		OAMassFlow = 0.0;

		// If no additional input from user, RETURN from subroutine
		if ( Damper( DamperNum ).NoOAFlowInputFromUser ) {
			ShowSevereError( "CalcOAOnlyMassFlow: Problem in AirTerminal:DualDuct:VAV:OutdoorAir = " + Damper( DamperNum ).DamperName + ", check outdoor air specification" );
			if ( present( MaxOAVolFlow ) ) MaxOAVolFlow = 0.0;
			return;
		}

		if ( Damper( DamperNum ).OAPerPersonMode == PerPersonDCVByCurrentLevel ) {
			UseOccSchFlag = true;
			PerPersonNotSet = false;
		} else {
			UseOccSchFlag = false;
			PerPersonNotSet = false;
			if ( Damper( DamperNum ).OAPerPersonMode == PerPersonModeNotSet ) PerPersonNotSet = true;
		}

		OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( Damper( DamperNum ).OARequirementsPtr, Damper( DamperNum ).ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, PerPersonNotSet );

		RhoAir = PsyRhoAirFnPbTdbW( Node( Damper( DamperNum ).OutletNodeNum ).Press, Node( Damper( DamperNum ).OutletNodeNum ).Temp, Node( Damper( DamperNum ).OutletNodeNum ).HumRat, RoutineName );

		OAMassFlow = OAVolumeFlowRate * RhoAir;

		if ( present( MaxOAVolFlow ) ) {
			OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( Damper( DamperNum ).OARequirementsPtr, Damper( DamperNum ).ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, _, true );
			MaxOAVolFlow = OAVolumeFlowRate;
		}

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Damper Module
	// *****************************************************************************

	void
	UpdateDualDuct( int const DamperNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   February 2000
		//       MODIFIED       Aug 2010 Clayton Miller - Added DualDuctVAVOutdoorAir
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the dampers.

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
		int HotInletNode;
		int ColdInletNode;
		int OAInletNode; // Outdoor Air Duct Inlet Node - for DualDuctOutdoorAir
		int RAInletNode; // Recirculated Air Duct Inlet Node - for DualDuctOutdoorAir

		if ( Damper( DamperNum ).DamperType == DualDuct_ConstantVolume || Damper( DamperNum ).DamperType == DualDuct_VariableVolume ) {

			OutletNode = Damper( DamperNum ).OutletNodeNum;
			HotInletNode = Damper( DamperNum ).HotAirInletNodeNum;
			ColdInletNode = Damper( DamperNum ).ColdAirInletNodeNum;

			// Set the outlet air nodes of the Damper
			Node( HotInletNode ).MassFlowRate = DamperHotAirInlet( DamperNum ).AirMassFlowRate;
			Node( ColdInletNode ).MassFlowRate = DamperColdAirInlet( DamperNum ).AirMassFlowRate;
			Node( OutletNode ).MassFlowRate = DamperOutlet( DamperNum ).AirMassFlowRate;
			Node( OutletNode ).MassFlowRateMaxAvail = DamperOutlet( DamperNum ).AirMassFlowRate;
			Node( OutletNode ).MassFlowRateMinAvail = DamperOutlet( DamperNum ).AirMassFlowRateMinAvail;
			Node( OutletNode ).Temp = DamperOutlet( DamperNum ).AirTemp;
			Node( OutletNode ).HumRat = DamperOutlet( DamperNum ).AirHumRat;
			Node( OutletNode ).Enthalpy = DamperOutlet( DamperNum ).AirEnthalpy;
			// Set the outlet nodes for properties that just pass through & not used
			// FIX THIS LATER!!!!
			Node( OutletNode ).Quality = Node( HotInletNode ).Quality;
			Node( OutletNode ).Press = Node( HotInletNode ).Press;

			if ( Contaminant.CO2Simulation ) {
				if ( Node( OutletNode ).MassFlowRate > 0.0 ) {
					Node( OutletNode ).CO2 = ( Node( HotInletNode ).CO2 * Node( HotInletNode ).MassFlowRate + Node( ColdInletNode ).CO2 * Node( ColdInletNode ).MassFlowRate) / Node( OutletNode ).MassFlowRate;
				} else {
					Node( OutletNode ).CO2 = max( Node( HotInletNode ).CO2, Node( ColdInletNode ).CO2);
				}
			}
			if ( Contaminant.GenericContamSimulation ) {
				if ( Node( OutletNode ).MassFlowRate > 0.0) {
					Node( OutletNode ).GenContam = ( Node( HotInletNode ).GenContam * Node( HotInletNode ).MassFlowRate + Node( ColdInletNode ).GenContam * Node( ColdInletNode ).MassFlowRate ) / Node( OutletNode ).MassFlowRate;
				} else {
					Node( OutletNode ).GenContam = max( Node( HotInletNode ).GenContam, Node( ColdInletNode ).GenContam );
				}
			}
		} else if ( Damper( DamperNum ).DamperType == DualDuct_OutdoorAir ) {

			OutletNode = Damper( DamperNum ).OutletNodeNum;
			OAInletNode = Damper( DamperNum ).OAInletNodeNum;
			if ( Damper( DamperNum ).RecircIsUsed ) {
				RAInletNode = Damper( DamperNum ).RecircAirInletNodeNum;
				Node( RAInletNode ).MassFlowRate = DamperRecircAirInlet( DamperNum ).AirMassFlowRate;
			}
			// Set the outlet air nodes of the Damper
			Node( OAInletNode ).MassFlowRate = DamperOAInlet( DamperNum ).AirMassFlowRate;
			Node( OutletNode ).MassFlowRate = DamperOutlet( DamperNum ).AirMassFlowRate;
			Node( OutletNode ).MassFlowRateMaxAvail = DamperOutlet( DamperNum ).AirMassFlowRate;
			Node( OutletNode ).MassFlowRateMinAvail = DamperOutlet( DamperNum ).AirMassFlowRateMinAvail;
			Node( OutletNode ).Temp = DamperOutlet( DamperNum ).AirTemp;
			Node( OutletNode ).HumRat = DamperOutlet( DamperNum ).AirHumRat;
			Node( OutletNode ).Enthalpy = DamperOutlet( DamperNum ).AirEnthalpy;
			// Set the outlet nodes for properties that just pass through & not used
			// FIX THIS LATER!!!!
			Node( OutletNode ).Quality = Node( OAInletNode ).Quality;
			Node( OutletNode ).Press = Node( OAInletNode ).Press;

			if ( Damper( DamperNum ).RecircIsUsed ) {
				if ( Node( OutletNode ).MassFlowRate > 0.0 ) {
					if ( Contaminant.CO2Simulation ) {
						Node( OutletNode ).CO2 = ( Node( OAInletNode ).CO2 * Node( OAInletNode ).MassFlowRate + Node( RAInletNode ).CO2 * Node( RAInletNode ).MassFlowRate ) / Node( OutletNode ).MassFlowRate;
					}
					if ( Contaminant.GenericContamSimulation ) {
						Node( OutletNode ).GenContam = ( Node( OAInletNode ).GenContam * Node( OAInletNode ).MassFlowRate + Node( RAInletNode ).GenContam * Node( RAInletNode ).MassFlowRate ) / Node( OutletNode ).MassFlowRate;
					}
				} else {
					if ( Contaminant.CO2Simulation ) {
						Node( OutletNode ).CO2 = max( Node( OAInletNode ).CO2, Node( RAInletNode ).CO2 );
					}
					if ( Contaminant.GenericContamSimulation ) {
						Node( OutletNode ).GenContam = max( Node( OAInletNode ).GenContam, Node( RAInletNode ).GenContam );
					}
				}

			} else {
				if ( Contaminant.CO2Simulation ) {
					Node( OutletNode ).CO2 = Node( OAInletNode ).CO2;
				}
				if ( Contaminant.GenericContamSimulation ) {
					Node( OutletNode ).GenContam = Node( OAInletNode ).GenContam;
				}
			}

		}

	}

	//        End of Update subroutines for the Damper Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Damper Module
	// *****************************************************************************

	void
	ReportDualDuct( int const EP_UNUSED( DamperNum ) ) // unused1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Unknown
		//       DATE WRITTEN   Unknown
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the damper report variables.

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

		// Still needs to report the Damper power from this component

	}

	void
	ReportDualDuctConnections()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte
		//       DATE WRITTEN   February 2004
		//       MODIFIED       B. Griffith, DOAS VAV dual duct
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Report dual duct damper connections to the BND file.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::OutputFileBNDetails;
		using DataAirLoop::AirToZoneNodeInfo;
		using DataHVACGlobals::NumPrimaryAirSys;
		using DataZoneEquipment::SupplyAirPath;
		using DataZoneEquipment::NumSupplyAirPaths;

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
		int Count1;
		int Count2;
		int Count3;
		int Found;
		int SupplyAirPathNum; // Supply air path ID
		std::string ChrOut;
		std::string ChrName;
		std::string DamperType;

		// Formats
		static gio::Fmt Format_100( "('! <#Dual Duct Damper Connections>,<Number of Dual Duct Damper Connections>')" );
		static gio::Fmt Format_101( "(A)" );
		static gio::Fmt Format_102( "('! <Dual Duct Damper>,<Dual Duct Damper Count>,<Dual Duct Damper Name>,<Inlet Node>,','<Outlet Node>,<Inlet Node Type>,<AirLoopHVAC Name>')" );
		static gio::Fmt fmtLD( "*" );

		if ( ! allocated( Damper ) ) return; //Autodesk Bug: Can arrive here with Damper unallocated (SimulateDualDuct not yet called) with NumDampers either set >0 or uninitialized

		//Report Dual Duct Dampers to BND File
		gio::write( OutputFileBNDetails, Format_101 ) << "! ===============================================================";
		gio::write( OutputFileBNDetails, Format_100 );
		gio::write( ChrOut, fmtLD ) << NumDampers * 2;
		gio::write( OutputFileBNDetails, Format_101 ) << " #Dual Duct Damper Connections," + stripped( ChrOut );
		gio::write( OutputFileBNDetails, Format_102 );

		for ( Count1 = 1; Count1 <= NumDampers; ++Count1 ) {

			// Determine if this damper is connected to a supply air path
			Found = 0;
			for ( Count2 = 1; Count2 <= NumSupplyAirPaths; ++Count2 ) {
				SupplyAirPathNum = Count2;
				Found = 0;
				for ( Count3 = 1; Count3 <= SupplyAirPath( Count2 ).NumOutletNodes; ++Count3 ) {
					if ( Damper( Count1 ).HotAirInletNodeNum == SupplyAirPath( Count2 ).OutletNode( Count3 ) ) Found = Count3;
					if ( Damper( Count1 ).ColdAirInletNodeNum == SupplyAirPath( Count2 ).OutletNode( Count3 ) ) Found = Count3;
					if ( Damper( Count1 ).OAInletNodeNum == SupplyAirPath( Count2 ).OutletNode( Count3 ) ) Found = Count3;
					if ( Damper( Count1 ).RecircAirInletNodeNum == SupplyAirPath( Count2 ).OutletNode( Count3 ) ) Found = Count3;
				}
				if ( Found != 0 ) break;
			}
			if ( Found == 0 ) SupplyAirPathNum = 0;

			// Determine which air loop this dual duct damper is connected to
			Found = 0;
			for ( Count2 = 1; Count2 <= NumPrimaryAirSys; ++Count2 ) {
				ChrName = AirToZoneNodeInfo( Count2 ).AirLoopName;
				Found = 0;
				for ( Count3 = 1; Count3 <= AirToZoneNodeInfo( Count2 ).NumSupplyNodes; ++Count3 ) {
					if ( SupplyAirPathNum != 0 ) {
						if ( SupplyAirPath( SupplyAirPathNum ).InletNodeNum == AirToZoneNodeInfo( Count2 ).ZoneEquipSupplyNodeNum( Count3 ) ) Found = Count3;
					} else {
						if ( Damper( Count1 ).HotAirInletNodeNum == AirToZoneNodeInfo( Count2 ).ZoneEquipSupplyNodeNum( Count3 ) ) Found = Count3;
						if ( Damper( Count1 ).ColdAirInletNodeNum == AirToZoneNodeInfo( Count2 ).ZoneEquipSupplyNodeNum( Count3 ) ) Found = Count3;
						if ( Damper( Count1 ).OAInletNodeNum == AirToZoneNodeInfo( Count2 ).ZoneEquipSupplyNodeNum( Count3 ) ) Found = Count3;
						if ( Damper( Count1 ).RecircAirInletNodeNum == AirToZoneNodeInfo( Count2 ).ZoneEquipSupplyNodeNum( Count3 ) ) Found = Count3;
					}
				}
				if ( Found != 0 ) break;
			}
			if ( Found == 0 ) ChrName = "**Unknown**";

			gio::write( ChrOut, fmtLD ) << Count1;
			if ( Damper( Count1 ).DamperType == DualDuct_ConstantVolume ) {
				DamperType = cCMO_DDConstantVolume;
			} else if ( Damper( Count1 ).DamperType == DualDuct_VariableVolume ) {
				DamperType = cCMO_DDVariableVolume;
			} else if ( Damper( Count1 ).DamperType == DualDuct_OutdoorAir ) {
				DamperType = cCMO_DDVarVolOA;
			} else {
				DamperType = "Invalid/Unknown";
			}

			if ( ( Damper( Count1 ).DamperType == DualDuct_ConstantVolume ) || ( Damper( Count1 ).DamperType == DualDuct_VariableVolume ) ) {
				gio::write( OutputFileBNDetails, Format_101 ) << " Dual Duct Damper," + stripped( ChrOut ) + ',' + DamperType + ',' + Damper( Count1 ).DamperName + ',' + NodeID( Damper( Count1 ).HotAirInletNodeNum ) + ',' + NodeID( Damper( Count1 ).OutletNodeNum ) + ",Hot Air," + ChrName;

				gio::write( OutputFileBNDetails, Format_101 ) << " Dual Duct Damper," + stripped( ChrOut ) + ',' + DamperType + ',' + Damper( Count1 ).DamperName + ',' + NodeID( Damper( Count1 ).ColdAirInletNodeNum ) + ',' + NodeID( Damper( Count1 ).OutletNodeNum ) + ",Cold Air," + ChrName;
			} else if ( Damper( Count1 ).DamperType == DualDuct_OutdoorAir ) {
				gio::write( OutputFileBNDetails, Format_101 ) << "Dual Duct Damper, " + stripped( ChrOut ) + ',' + DamperType + ',' + Damper( Count1 ).DamperName + ',' + NodeID( Damper( Count1 ).OAInletNodeNum ) + ',' + NodeID( Damper( Count1 ).OutletNodeNum ) + ",Outdoor Air," + ChrName;
				gio::write( OutputFileBNDetails, Format_101 ) << "Dual Duct Damper, " + stripped( ChrOut ) + ',' + DamperType + ',' + Damper( Count1 ).DamperName + ',' + NodeID( Damper( Count1 ).RecircAirInletNodeNum ) + ',' + NodeID( Damper( Count1 ).OutletNodeNum ) + ",Recirculated Air," + ChrName;
			}
		}

	}

	void
	GetDualDuctOutdoorAirRecircUse(
		std::string const & EP_UNUSED( CompTypeName ),
		std::string const & CompName,
		bool & RecircIsUsed
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Aug 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get routine to learn if a dual duct outdoor air unit is using its recirc deck

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER :: DamperNum
		static bool FirstTimeOnly( true );
		static Array1D_bool RecircIsUsedARR;
		static Array1D_string DamperNamesARR;
		int DamperIndex; // Loop index to Damper that you are currently loading input into
		std::string CurrentModuleObject; // for ease in getting objects
		static Array1D< Real64 > NumArray( 2, 0.0 );
		static Array1D_string AlphArray( 7 );
		static Array1D_string cAlphaFields( 7 ); // Alpha field names
		static Array1D_string cNumericFields( 2 ); // Numeric field names
		static Array1D_bool lAlphaBlanks( 7, true ); // Logical array, alpha field input BLANK = .TRUE.
		static Array1D_bool lNumericBlanks( 2, true ); // Logical array, numeric field input BLANK = .TRUE.
		int NumAlphas;
		int NumNums;
		int IOStat;

		RecircIsUsed = true;

		// this doesn't work because it fires code that depends on things being further along
		//  IF (GetDualDuctInputFlag) THEN  !First time subroutine has been entered
		//    CALL GetDualDuctInput
		//    GetDualDuctInputFlag=.FALSE.
		//  END IF

		if ( FirstTimeOnly ) {
			NumDualDuctVarVolOA = GetNumObjectsFound( cCMO_DDVarVolOA );
			RecircIsUsedARR.allocate( NumDualDuctVarVolOA );
			DamperNamesARR.allocate( NumDualDuctVarVolOA );
			if ( NumDualDuctVarVolOA > 0 ) {
				for ( DamperIndex = 1; DamperIndex <= NumDualDuctVarVolOA; ++DamperIndex ) {

					CurrentModuleObject = cCMO_DDVarVolOA;

					GetObjectItem( CurrentModuleObject, DamperIndex, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
					DamperNamesARR( DamperIndex ) = AlphArray( 1 );
					if ( ! lAlphaBlanks( 5 ) ) {
						RecircIsUsedARR( DamperIndex ) = true;
					} else {
						RecircIsUsedARR( DamperIndex ) = false;
					}
				}
			}
			FirstTimeOnly = false;
		}

		DamperIndex = FindItemInList( CompName, DamperNamesARR, NumDualDuctVarVolOA );
		if ( DamperIndex > 0 ) {
			RecircIsUsed = RecircIsUsedARR( DamperIndex );
		}

	}

	//        End of Reporting subroutines for the Damper Module
	// *****************************************************************************

} // DualDuct

} // EnergyPlus
