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
#include <PoweredInductionUnits.hh>
#include <BranchNodeConnections.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
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
#include <MixerComponent.hh>
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

namespace PoweredInductionUnits {

	// Module containing routines dealing with Series and Parallel fan powered terminal boxes

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   August 2000
	//       MODIFIED       Brent Griffith, Sept 2010, plant upgrades, fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms needed to simulate Series and Parallel
	// fan powered induction terminal boxes.

	// METHODOLOGY EMPLOYED:
	// The terminal boxes are modeled as a collection of components: air mixer,
	// fan, and heating coil plus an integrated control
	// algorithm that adjusts the primary air flow and the heating coil output
	// to meet the zone load.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::SecInHour;
	using DataGlobals::NumOfZones;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::SmallTempDiff;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::SingleCoolingSetPoint;
	using DataHVACGlobals::SingleHeatingSetPoint;
	using DataHVACGlobals::PlenumInducedMassFlow;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyHFnTdbW;
	using SteamCoils::SimulateSteamCoilComponents;
	using namespace FluidProperties;
	using DataHeatBalFanSys::TempControlType;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const SingleDuct_SeriesPIU_Reheat( 6 );
	int const SingleDuct_ParallelPIU_Reheat( 7 );
	// coil types in this module
	int const HCoilType_Gas( 1 );
	int const HCoilType_Electric( 2 );
	int const HCoilType_SimpleHeating( 3 );
	int const HCoilType_SteamAirHeating( 4 );

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	Array1D_bool CheckEquipName;
	bool GetPIUInputFlag( true ); // First time, input is "gotten"

	int NumPIUs( 0 );
	int NumSeriesPIUs( 0 );
	int NumParallelPIUs( 0 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// PRIVATE UpdatePIU

	// Object Data
	Array1D< PowIndUnitData > PIU;

	// Functions

	void
	SimPIU(
		std::string const & CompName, // name of the PIU
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by PIU
		int const ZoneNodeNum, // zone node number of zone served by PIU
		int & CompIndex // PIU Index in PIU names
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a fan powered induction terminal unit.
		// Called from SimZoneAirLoopEquipmentin module ZoneAirLoopEquipmentManager.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataSizing::TermUnitPIU;
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
		int PIUNum; // index of powered induction unit being simulated

		// FLOW

		// First time SimPIU is called, get the input for all the fan coil units
		if ( GetPIUInputFlag ) {
			GetPIUs();
			GetPIUInputFlag = false;
		}

		// Get the powered induction unit index
		if ( CompIndex == 0 ) {
			PIUNum = FindItemInList( CompName, PIU );
			if ( PIUNum == 0 ) {
				ShowFatalError( "SimPIU: PIU Unit not found=" + CompName );
			}
			CompIndex = PIUNum;
		} else {
			PIUNum = CompIndex;
			if ( PIUNum > NumPIUs || PIUNum < 1 ) {
				ShowFatalError( "SimPIU: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Number of PIU Units=" + TrimSigDigits( NumPIUs ) + ", PIU Unit name=" + CompName );
			}
			if ( CheckEquipName( PIUNum ) ) {
				if ( CompName != PIU( PIUNum ).Name ) {
					ShowFatalError( "SimPIU: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", PIU Unit name=" + CompName + ", stored PIU Unit Name for that index=" + PIU( PIUNum ).Name );
				}
				CheckEquipName( PIUNum ) = false;
			}
		}

		// initialize the unit
		InitPIU( PIUNum, FirstHVACIteration );

		TermUnitPIU = true;

		// Select the correct unit type
		{ auto const SELECT_CASE_var( PIU( PIUNum ).UnitType_Num );

		if ( SELECT_CASE_var == SingleDuct_SeriesPIU_Reheat ) { //  'AirTerminal:SingleDuct:SeriesPIU:Reheat'

			CalcSeriesPIU( PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration );

		} else if ( SELECT_CASE_var == SingleDuct_ParallelPIU_Reheat ) { // 'AirTerminal:SingleDuct:ParallelPIU:Reheat'

			CalcParallelPIU( PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration );

		} else {
			ShowSevereError( "Illegal PI Unit Type used=" + PIU( PIUNum ).UnitType );
			ShowContinueError( "Occurs in PI Unit=" + PIU( PIUNum ).Name );
			ShowFatalError( "Preceding condition causes termination." );

		}}

		TermUnitPIU = false;

		// Update the current unit's outlet nodes
		// no update needed: reheat coil updates outlet node; inlet nodes' mass flow rate set by Calc routine

		// Fill the report variables
		ReportPIU( PIUNum );

	}

	void
	GetPIUs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for powered induction unit terminal boxes and stores it
		// in PIU data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using NodeInputManager::GetOnlySingleNode;
		using FluidProperties::FindRefrigerant;
		using DataZoneEquipment::ZoneEquipConfig;
		using BranchNodeConnections::SetUpCompSets;
		using BranchNodeConnections::TestCompSet;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using namespace DataIPShortCuts;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using WaterCoils::GetCoilWaterInletNode;
		using SteamCoils::GetCoilSteamInletNode;

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
		int PIUIndex; // loop index
		int PIUNum; // current fan coil number
		int NumAlphas; // Number of Alpha input fields for each GetObjectItem call
		int NumNumbers; // Number of Numeric input fields for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int CtrlZone; // controlled zome do loop index
		int SupAirIn; // controlled zone supply air inlet index
		bool AirNodeFound;
		int ADUNum;
		static std::string const RoutineName( "GetPIUs: " ); // include trailing blank space
		bool SteamMessageNeeded;

		// FLOW
		// find the number of each type of fan coil unit
		SteamMessageNeeded = true;
		NumSeriesPIUs = GetNumObjectsFound( "AirTerminal:SingleDuct:SeriesPIU:Reheat" );
		NumParallelPIUs = GetNumObjectsFound( "AirTerminal:SingleDuct:ParallelPIU:Reheat" );
		NumPIUs = NumSeriesPIUs + NumParallelPIUs;
		// allocate the data structures
		PIU.allocate( NumPIUs );
		CheckEquipName.dimension( NumPIUs, true );

		// loop over Series PIUs; get and load the input data
		for ( PIUIndex = 1; PIUIndex <= NumSeriesPIUs; ++PIUIndex ) {

			cCurrentModuleObject = "AirTerminal:SingleDuct:SeriesPIU:Reheat";

			GetObjectItem( cCurrentModuleObject, PIUIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			PIUNum = PIUIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PIU, PIUNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PIU( PIUNum ).Name = cAlphaArgs( 1 );
			PIU( PIUNum ).UnitType = cCurrentModuleObject;
			PIU( PIUNum ).UnitType_Num = SingleDuct_SeriesPIU_Reheat;
			PIU( PIUNum ).Sched = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				PIU( PIUNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				PIU( PIUNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) ); // convert schedule name to pointer
				if ( PIU( PIUNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			PIU( PIUNum ).MaxTotAirVolFlow = rNumericArgs( 1 );
			PIU( PIUNum ).MaxPriAirVolFlow = rNumericArgs( 2 );
			PIU( PIUNum ).MinPriAirFlowFrac = rNumericArgs( 3 );

			PIU( PIUNum ).HCoilType = cAlphaArgs( 9 ); // type (key) of heating coil
			if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:WATER" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_SimpleHeating;
				PIU( PIUNum ).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
			} else if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:GAS" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_Gas;
			} else if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:STEAM" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_SteamAirHeating;
				PIU( PIUNum ).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
				PIU( PIUNum ).HCoil_FluidIndex = FindRefrigerant( "Steam" );
				if ( PIU( PIUNum ).HCoil_FluidIndex == 0 ) {
					ShowSevereError( RoutineName + "Steam Properties for " + cAlphaArgs( 1 ) + " not found." );
					if ( SteamMessageNeeded ) ShowContinueError( "Steam Fluid Properties should have been included in the input file." );
					ErrorsFound = true;
					SteamMessageNeeded = false;
				}
			} else if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:ELECTRIC" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_Electric;
			} else {
				ShowSevereError( "Illegal " + cAlphaFieldNames( 9 ) + " = " + cAlphaArgs( 9 ) );
				ShowContinueError( "Occurs in " + cCurrentModuleObject + " = " + PIU( PIUNum ).Name );
				ErrorsFound = true;
			}

			PIU( PIUNum ).PriAirInNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, PIU( PIUNum ).UnitType, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFieldNames( 3 ) );

			PIU( PIUNum ).SecAirInNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, PIU( PIUNum ).UnitType, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFieldNames( 4 ) );

			PIU( PIUNum ).OutAirNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, PIU( PIUNum ).UnitType, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFieldNames( 5 ) );

			PIU( PIUNum ).HCoilInAirNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, PIU( PIUNum ).UnitType, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent, cAlphaFieldNames( 6 ) );
			// The reheat coil control node is necessary for hot water reheat, but not necessary for
			// electric or gas reheat.
			if ( PIU( PIUNum ).HCoilType_Num == HCoilType_SimpleHeating ) {
				PIU( PIUNum ).HotControlNode = GetCoilWaterInletNode( cAlphaArgs( 9 ), cAlphaArgs( 10 ), ErrorsFound );
			}
			if ( PIU( PIUNum ).HCoilType_Num == HCoilType_SteamAirHeating ) {
				PIU( PIUNum ).HotControlNode = GetCoilSteamInletNode( cAlphaArgs( 9 ), cAlphaArgs( 10 ), ErrorsFound );
			}
			PIU( PIUNum ).MixerName = cAlphaArgs( 7 ); // name of zone mixer object
			PIU( PIUNum ).FanName = cAlphaArgs( 8 ); // name of fan object
			PIU( PIUNum ).HCoil = cAlphaArgs( 10 ); // name of heating coil object
			ValidateComponent( PIU( PIUNum ).HCoilType, PIU( PIUNum ).HCoil, IsNotOK, cCurrentModuleObject + " - Heating Coil" );
			if ( IsNotOK ) {
				ShowContinueError( "In " + cCurrentModuleObject + " = " + PIU( PIUNum ).Name );
				ErrorsFound = true;
			}
			PIU( PIUNum ).MaxVolHotWaterFlow = rNumericArgs( 4 );
			PIU( PIUNum ).MinVolHotWaterFlow = rNumericArgs( 5 );
			PIU( PIUNum ).HotControlOffset = rNumericArgs( 6 );
			// Set default convergence tolerance
			if ( PIU( PIUNum ).HotControlOffset <= 0.0 ) {
				PIU( PIUNum ).HotControlOffset = 0.001;
			}

			// Add fan to component sets array
			SetUpCompSets( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "UNDEFINED", cAlphaArgs( 8 ), "UNDEFINED", cAlphaArgs( 6 ) );

			// Add reheat coil to component sets array
			SetUpCompSets( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, cAlphaArgs( 9 ), cAlphaArgs( 10 ), cAlphaArgs( 6 ), cAlphaArgs( 5 ) );

			// Register component set data
			TestCompSet( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, NodeID( PIU( PIUNum ).PriAirInNode ), NodeID( PIU( PIUNum ).OutAirNode ), "Air Nodes" );

			// Fill the Zone Equipment data with the supply air inlet node number of this unit.
			AirNodeFound = false;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( PIU( PIUNum ).OutAirNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = PIU( PIUNum ).PriAirInNode;
						ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = PIU( PIUNum ).OutAirNode;
						AirNodeFound = true;
						break;
					}
				}
			}
			if ( ! AirNodeFound ) {
				ShowSevereError( "The outlet air node from the " + cCurrentModuleObject + " Unit = " + PIU( PIUNum ).Name );
				ShowContinueError( "did not have a matching Zone Equipment Inlet Node, Node = " + cAlphaArgs( 5 ) );
				ErrorsFound = true;
			}

		}

		for ( PIUIndex = 1; PIUIndex <= NumParallelPIUs; ++PIUIndex ) {

			cCurrentModuleObject = "AirTerminal:SingleDuct:ParallelPIU:Reheat";

			GetObjectItem( cCurrentModuleObject, PIUIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			PIUNum = PIUIndex + NumSeriesPIUs;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PIU, PIUNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PIU( PIUNum ).Name = cAlphaArgs( 1 );
			PIU( PIUNum ).UnitType = cCurrentModuleObject;
			PIU( PIUNum ).UnitType_Num = SingleDuct_ParallelPIU_Reheat;
			PIU( PIUNum ).Sched = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				PIU( PIUNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				PIU( PIUNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) ); // convert schedule name to pointer
				if ( PIU( PIUNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
			PIU( PIUNum ).MaxPriAirVolFlow = rNumericArgs( 1 );
			PIU( PIUNum ).MaxSecAirVolFlow = rNumericArgs( 2 );
			PIU( PIUNum ).MinPriAirFlowFrac = rNumericArgs( 3 );
			PIU( PIUNum ).FanOnFlowFrac = rNumericArgs( 4 );
			PIU( PIUNum ).HCoilType = cAlphaArgs( 9 ); // type (key) of heating coil
			if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:WATER" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_SimpleHeating;
				PIU( PIUNum ).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
			} else if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:GAS" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_Gas;
			} else if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:STEAM" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_SteamAirHeating;
				PIU( PIUNum ).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
				PIU( PIUNum ).HCoil_FluidIndex = FindRefrigerant( "Steam" );
				if ( PIU( PIUNum ).HCoil_FluidIndex == 0 ) {
					ShowSevereError( RoutineName + "Steam Properties for " + cAlphaArgs( 1 ) + " not found." );
					if ( SteamMessageNeeded ) ShowContinueError( "Steam Fluid Properties should have been included in the input file." );
					ErrorsFound = true;
					SteamMessageNeeded = false;
				}
			} else if ( SameString( cAlphaArgs( 9 ), "COIL:HEATING:ELECTRIC" ) ) {
				PIU( PIUNum ).HCoilType_Num = HCoilType_Electric;
			} else {
				ShowSevereError( "Illegal " + cAlphaFieldNames( 9 ) + " = " + cAlphaArgs( 9 ) );
				ShowContinueError( "Occurs in " + cCurrentModuleObject + " = " + PIU( PIUNum ).Name );
				ErrorsFound = true;
			}

			PIU( PIUNum ).PriAirInNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFieldNames( 3 ) );

			PIU( PIUNum ).SecAirInNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFieldNames( 4 ) );

			PIU( PIUNum ).OutAirNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFieldNames( 5 ) );

			PIU( PIUNum ).HCoilInAirNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent, cAlphaFieldNames( 6 ) );
			// The reheat coil control node is necessary for hot water reheat, but not necessary for
			// electric or gas reheat.
			//  IF (PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Gas .OR. PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Electric) THEN
			//    IF(cAlphaArgs(11) /= '') THEN
			//      CALL ShowWarningError('In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
			//                             // ' the '//TRIM(cAlphaFieldNames(11))//' is not needed and will be ignored.')
			//      CALL ShowContinueError('  It is used for hot water reheat coils only.')
			//    END IF
			//  ELSE
			//    IF(cAlphaArgs(11) == '') THEN
			//      CALL ShowSevereError('In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
			//                           // ' the '//TRIM(cAlphaFieldNames(11))//' is undefined.')
			//      ErrorsFound=.TRUE.
			//    END IF
			//    PIU(PIUNum)%HotControlNode  = &
			//      GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
			//                        NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
			//  END IF
			if ( PIU( PIUNum ).HCoilType_Num == HCoilType_SimpleHeating ) {
				PIU( PIUNum ).HotControlNode = GetCoilWaterInletNode( cAlphaArgs( 9 ), cAlphaArgs( 10 ), ErrorsFound );
			}
			if ( PIU( PIUNum ).HCoilType_Num == HCoilType_SteamAirHeating ) {
				PIU( PIUNum ).HotControlNode = GetCoilSteamInletNode( cAlphaArgs( 9 ), cAlphaArgs( 10 ), ErrorsFound );
			}
			PIU( PIUNum ).MixerName = cAlphaArgs( 7 ); // name of zone mixer object
			PIU( PIUNum ).FanName = cAlphaArgs( 8 ); // name of fan object
			PIU( PIUNum ).HCoil = cAlphaArgs( 10 ); // name of heating coil object
			ValidateComponent( PIU( PIUNum ).HCoilType, PIU( PIUNum ).HCoil, IsNotOK, cCurrentModuleObject + " - Heating Coil" );
			if ( IsNotOK ) {
				ShowContinueError( "In " + cCurrentModuleObject + " = " + PIU( PIUNum ).Name );
				ErrorsFound = true;
			}
			PIU( PIUNum ).MaxVolHotWaterFlow = rNumericArgs( 5 );
			PIU( PIUNum ).MinVolHotWaterFlow = rNumericArgs( 6 );
			PIU( PIUNum ).HotControlOffset = rNumericArgs( 7 );
			// Set default convergence tolerance
			if ( PIU( PIUNum ).HotControlOffset <= 0.0 ) {
				PIU( PIUNum ).HotControlOffset = 0.001;
			}

			// Add fan to component sets array
			SetUpCompSets( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "UNDEFINED", cAlphaArgs( 8 ), cAlphaArgs( 4 ), "UNDEFINED" );

			// Add reheat coil to component sets array
			SetUpCompSets( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, cAlphaArgs( 9 ), cAlphaArgs( 10 ), cAlphaArgs( 6 ), cAlphaArgs( 5 ) );

			// Register component set data
			TestCompSet( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, NodeID( PIU( PIUNum ).PriAirInNode ), NodeID( PIU( PIUNum ).OutAirNode ), "Air Nodes" );

			// Fill the Zone Equipment data with the supply air inlet node number of this unit.
			AirNodeFound = false;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( PIU( PIUNum ).OutAirNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = PIU( PIUNum ).PriAirInNode;
						ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = PIU( PIUNum ).OutAirNode;
						AirNodeFound = true;
					}
				}
			}
			if ( ! AirNodeFound ) {
				ShowSevereError( "The outlet air node from the " + cCurrentModuleObject + " Unit = " + PIU( PIUNum ).Name );
				ShowContinueError( "did not have a matching Zone Equipment Inlet Node, Node = " + cAlphaArgs( 5 ) );
				ErrorsFound = true;
			}

		}

		for ( PIUNum = 1; PIUNum <= NumPIUs; ++PIUNum ) {
			for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
				if ( PIU( PIUNum ).OutAirNode == AirDistUnit( ADUNum ).OutletNodeNum ) {
					//      AirDistUnit(ADUNum)%InletNodeNum = PIU(PIUNum)%InletNodeNum
					PIU( PIUNum ).ADUNum = ADUNum;
				}
			}
			// one assumes if there isn't one assigned, it's an error?
			if ( PIU( PIUNum ).ADUNum == 0 ) {
				ShowSevereError( RoutineName + "No matching Air Distribution Unit, for PIU = [" + PIU( PIUNum ).UnitType + ',' + PIU( PIUNum ).Name + "]." );
				ShowContinueError( "...should have outlet node = " + NodeID( PIU( PIUNum ).OutAirNode ) );
				//          ErrorsFound=.TRUE.
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input.  Preceding conditions cause termination." );
		}

		for ( PIUNum = 1; PIUNum <= NumPIUs; ++PIUNum ) {
			// Setup Report variables for the Fan Coils
			SetupOutputVariable( "Zone Air Terminal Heating Rate [W]", PIU( PIUNum ).HeatingRate, "System", "Average", PIU( PIUNum ).Name );
			SetupOutputVariable( "Zone Air Terminal Heating Energy [J]", PIU( PIUNum ).HeatingEnergy, "System", "Sum", PIU( PIUNum ).Name );
			SetupOutputVariable( "Zone Air Terminal Sensible Cooling Rate [W]", PIU( PIUNum ).SensCoolRate, "System", "Average", PIU( PIUNum ).Name );
			SetupOutputVariable( "Zone Air Terminal Sensible Cooling Energy [J]", PIU( PIUNum ).SensCoolEnergy, "System", "Sum", PIU( PIUNum ).Name );

		}

	}

	void
	InitPIU(
		int const PIUNum, // number of the current fan coil unit being simulated
		bool const FirstHVACIteration // TRUE if first zone equip this HVAC step
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the powered induction unit
		// terminal boxe.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataDefineEquip::AirDistUnit;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using PlantUtilities::InitComponentNodes;
		using DataGlobals::AnyPlantInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitPIU" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PriNode; // primary air inlet node number
		int SecNode; // secondary air inlet node number
		int HotConNode; // hot water control node number in PIU
		int OutletNode; // unit air outlet node number
		Real64 RhoAir; // air density at outside pressure and standard temperature and humidity
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MySizeFlag;
		static Array1D_bool MyPlantScanFlag;
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop; // Loop checking control variable
		Real64 rho; // local plant fluid density
		bool errFlag;

		// FLOW:
		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumPIUs );
			MySizeFlag.allocate( NumPIUs );
			MyPlantScanFlag.allocate( NumPIUs );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;
			MyOneTimeFlag = false;

		}

		if ( MyPlantScanFlag( PIUNum ) && allocated( PlantLoop ) ) {
			if ( ( PIU( PIUNum ).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating ) || ( PIU( PIUNum ).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( PIU( PIUNum ).HCoil, PIU( PIUNum ).HCoil_PlantTypeNum, PIU( PIUNum ).HWLoopNum, PIU( PIUNum ).HWLoopSide, PIU( PIUNum ).HWBranchNum, PIU( PIUNum ).HWCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitPIU: Program terminated due to previous condition(s)." );
				}
				PIU( PIUNum ).HotCoilOutNodeNum = PlantLoop( PIU( PIUNum ).HWLoopNum ).LoopSide( PIU( PIUNum ).HWLoopSide ).Branch( PIU( PIUNum ).HWBranchNum ).Comp( PIU( PIUNum ).HWCompNum ).NodeNumOut;
			}
			MyPlantScanFlag( PIUNum ) = false;
		} else if ( MyPlantScanFlag( PIUNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( PIUNum ) = false;
		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			// Check to see if there is a Air Distribution Unit on the Zone Equipment List
			for ( Loop = 1; Loop <= NumPIUs; ++Loop ) {
				if ( PIU( Loop ).ADUNum == 0 ) continue;
				if ( CheckZoneEquipmentList( "ZoneHVAC:AirDistributionUnit", AirDistUnit( PIU( Loop ).ADUNum ).Name ) ) continue;
				ShowSevereError( "InitPIU: ADU=[Air Distribution Unit," + AirDistUnit( PIU( Loop ).ADUNum ).Name + "] is not on any ZoneHVAC:EquipmentList." );
				ShowContinueError( "...PIU=[" + PIU( Loop ).UnitType + ',' + PIU( Loop ).Name + "] will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( PIUNum ) && ! MyPlantScanFlag( PIUNum ) ) {

			SizePIU( PIUNum );

			HotConNode = PIU( PIUNum ).HotControlNode;
			if ( HotConNode > 0 ) {
				//plant upgrade note? why no separate handling of steam coil? add it ?
				rho = GetDensityGlycol( PlantLoop( PIU( PIUNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( PIU( PIUNum ).HWLoopNum ).FluidIndex, RoutineName );

				PIU( PIUNum ).MaxHotWaterFlow = rho * PIU( PIUNum ).MaxVolHotWaterFlow;
				PIU( PIUNum ).MinHotWaterFlow = rho * PIU( PIUNum ).MinVolHotWaterFlow;
				InitComponentNodes( PIU( PIUNum ).MinHotWaterFlow, PIU( PIUNum ).MaxHotWaterFlow, PIU( PIUNum ).HotControlNode, PIU( PIUNum ).HotCoilOutNodeNum, PIU( PIUNum ).HWLoopNum, PIU( PIUNum ).HWLoopSide, PIU( PIUNum ).HWBranchNum, PIU( PIUNum ).HWCompNum );

			}

			MySizeFlag( PIUNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( PIUNum ) ) {
			RhoAir = StdRhoAir;
			PriNode = PIU( PIUNum ).PriAirInNode;
			SecNode = PIU( PIUNum ).SecAirInNode;
			OutletNode = PIU( PIUNum ).OutAirNode;
			// set the mass flow rates from the input volume flow rates
			if ( PIU( PIUNum ).UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat" ) {
				// series
				PIU( PIUNum ).MaxTotAirMassFlow = RhoAir * PIU( PIUNum ).MaxTotAirVolFlow;
				PIU( PIUNum ).MaxPriAirMassFlow = RhoAir * PIU( PIUNum ).MaxPriAirVolFlow;
				PIU( PIUNum ).MinPriAirMassFlow = RhoAir * PIU( PIUNum ).MinPriAirFlowFrac * PIU( PIUNum ).MaxPriAirVolFlow;
				Node( PriNode ).MassFlowRateMax = PIU( PIUNum ).MaxPriAirMassFlow;
				Node( PriNode ).MassFlowRateMin = PIU( PIUNum ).MinPriAirMassFlow;
				Node( OutletNode ).MassFlowRateMax = PIU( PIUNum ).MaxTotAirMassFlow;
			} else {
				// parallel
				PIU( PIUNum ).MaxPriAirMassFlow = RhoAir * PIU( PIUNum ).MaxPriAirVolFlow;
				PIU( PIUNum ).MinPriAirMassFlow = RhoAir * PIU( PIUNum ).MinPriAirFlowFrac * PIU( PIUNum ).MaxPriAirVolFlow;
				PIU( PIUNum ).MaxSecAirMassFlow = RhoAir * PIU( PIUNum ).MaxSecAirVolFlow;
				PIU( PIUNum ).FanOnAirMassFlow = RhoAir * PIU( PIUNum ).FanOnFlowFrac * PIU( PIUNum ).MaxPriAirVolFlow;
				Node( PriNode ).MassFlowRateMax = PIU( PIUNum ).MaxPriAirMassFlow;
				Node( PriNode ).MassFlowRateMin = PIU( PIUNum ).MinPriAirMassFlow;
				Node( OutletNode ).MassFlowRateMax = PIU( PIUNum ).MaxPriAirMassFlow;
			}

			if ( ( ( PIU( PIUNum ).HCoilType_Num == HCoilType_SimpleHeating ) || ( PIU( PIUNum ).HCoilType_Num == HCoilType_SteamAirHeating ) ) && ! MyPlantScanFlag( PIUNum ) ) {
				InitComponentNodes( PIU( PIUNum ).MinHotWaterFlow, PIU( PIUNum ).MaxHotWaterFlow, PIU( PIUNum ).HotControlNode, PIU( PIUNum ).HotCoilOutNodeNum, PIU( PIUNum ).HWLoopNum, PIU( PIUNum ).HWLoopSide, PIU( PIUNum ).HWBranchNum, PIU( PIUNum ).HWCompNum );
			}
			MyEnvrnFlag( PIUNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( PIUNum ) = true;
		}

		PriNode = PIU( PIUNum ).PriAirInNode;
		SecNode = PIU( PIUNum ).SecAirInNode;

		// Do the start of HVAC time step initializations
		if ( FirstHVACIteration ) {
			// check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
			if ( GetCurrentScheduleValue( PIU( PIUNum ).SchedPtr ) > 0.0 && Node( PriNode ).MassFlowRate > 0.0 ) {
				if ( PIU( PIUNum ).UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat" ) {
					Node( PriNode ).MassFlowRate = PIU( PIUNum ).MaxPriAirMassFlow;
					Node( SecNode ).MassFlowRate = max( 0.0, PIU( PIUNum ).MaxTotAirMassFlow - PIU( PIUNum ).MaxPriAirMassFlow );
				} else {
					Node( PriNode ).MassFlowRate = PIU( PIUNum ).MaxPriAirMassFlow;
					Node( SecNode ).MassFlowRate = PIU( PIUNum ).MaxSecAirMassFlow;
				}
			} else {
				Node( PriNode ).MassFlowRate = 0.0;
				Node( SecNode ).MassFlowRate = 0.0;
			}
			// reset the max and min avail flows
			if ( GetCurrentScheduleValue( PIU( PIUNum ).SchedPtr ) > 0.0 && Node( PriNode ).MassFlowRateMaxAvail > 0.0 ) {
				if ( PIU( PIUNum ).UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat" ) {
					Node( PriNode ).MassFlowRateMaxAvail = PIU( PIUNum ).MaxPriAirMassFlow;
					Node( PriNode ).MassFlowRateMinAvail = PIU( PIUNum ).MinPriAirMassFlow;
					Node( SecNode ).MassFlowRateMaxAvail = max( 0.0, PIU( PIUNum ).MaxTotAirMassFlow - PIU( PIUNum ).MinPriAirMassFlow );
					Node( SecNode ).MassFlowRateMinAvail = max( 0.0, PIU( PIUNum ).MaxTotAirMassFlow - PIU( PIUNum ).MaxPriAirMassFlow );
				} else {
					Node( PriNode ).MassFlowRateMaxAvail = PIU( PIUNum ).MaxPriAirMassFlow;
					Node( PriNode ).MassFlowRateMinAvail = PIU( PIUNum ).MinPriAirMassFlow;
					Node( SecNode ).MassFlowRateMaxAvail = PIU( PIUNum ).MaxSecAirMassFlow;
					Node( SecNode ).MassFlowRateMinAvail = 0.0;
				}
			} else {
				Node( PriNode ).MassFlowRateMaxAvail = 0.0;
				Node( PriNode ).MassFlowRateMinAvail = 0.0;
				Node( SecNode ).MassFlowRateMaxAvail = 0.0;
				Node( SecNode ).MassFlowRateMinAvail = 0.0;
			}
		}

		// Do the following initializations every time step

		// None needed

	}

	void
	SizePIU( int const PIUNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing PIU terminal units for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		using SteamCoils::GetCoilSteamOutletNode;
		//  USE BranchInputManager, ONLY: MyPlantSizingIndex
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using ReportSizingManager::ReportSizingOutput;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizePIU" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		Real64 CoilInTemp;
		Real64 CoilOutTemp;
		Real64 CoilOutHumRat;
		Real64 DesCoilLoad;
		Real64 DesMassFlow;

		Real64 TempSteamIn;
		Real64 EnthSteamInDry;
		Real64 EnthSteamOutWet;
		Real64 LatentHeatSteam;
		Real64 SteamDensity;
		static int CoilWaterInletNode( 0 );
		static int CoilWaterOutletNode( 0 );
		static int CoilSteamInletNode( 0 );
		static int CoilSteamOutletNode( 0 );
		bool ErrorsFound;
		Real64 rho;
		Real64 Cp;
		static int DummyWaterIndex( 1 );
		bool IsAutoSize; // Indicator to autosize
		Real64 MaxPriAirVolFlowDes; // Autosized maximum primary air flow for reporting
		Real64 MaxPriAirVolFlowUser; // Hardsized maximum primary air flow for reporting
		Real64 MaxTotAirVolFlowDes; // Autosized maximum air flow for reporting
		Real64 MaxTotAirVolFlowUser; // Hardsized maximum air flow for reporting
		Real64 MaxSecAirVolFlowDes; // Autosized maximum secondary air flow for reporting
		Real64 MaxSecAirVolFlowUser; // Hardsized maximum secondary air flow for reporting
		Real64 MinPriAirFlowFracDes; // Autosized minimum primary air flow fraction for reporting
		Real64 MinPriAirFlowFracUser; // Hardsized minimum primary air flow fraction for reporting
		Real64 FanOnFlowFracDes; // Autosized fan on flow fraction for reporting
		Real64 FanOnFlowFracUser; // Hardsized fan on flow fraction for reporting
		Real64 MaxVolHotWaterFlowDes; // Autosized maximum hot water flow for reporting
		Real64 MaxVolHotWaterFlowUser; // Hardsized maximum hot water flow for reporting
		Real64 MaxVolHotSteamFlowDes; // Autosized maximum hot steam flow for reporting
		Real64 MaxVolHotSteamFlowUser; // Hardsized maximum hot steam flow for reporting

		PltSizHeatNum = 0;
		DesMassFlow = 0.0;
		ErrorsFound = false;
		IsAutoSize = false;
		MaxPriAirVolFlowDes = 0.0;
		MaxPriAirVolFlowUser = 0.0;
		MaxTotAirVolFlowDes = 0.0;
		MaxTotAirVolFlowUser = 0.0;
		MaxSecAirVolFlowDes = 0.0;
		MaxSecAirVolFlowUser = 0.0;
		MinPriAirFlowFracDes = 0.0;
		MinPriAirFlowFracUser = 0.0;
		FanOnFlowFracDes = 0.0;
		FanOnFlowFracUser = 0.0;
		MaxVolHotWaterFlowDes = 0.0;
		MaxVolHotWaterFlowUser = 0.0;
		MaxVolHotSteamFlowDes = 0.0;
		MaxVolHotSteamFlowUser = 0.0;

		if ( PIU( PIUNum ).MaxPriAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( PIU( PIUNum ).MaxPriAirVolFlow > 0.0 ) {
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "User-Specified Maximum Primary Air Flow Rate [m3/s]", PIU( PIUNum ).MaxPriAirVolFlow );
				}
			} else {
				CheckZoneSizing( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name );
				MaxPriAirVolFlowDes = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( MaxPriAirVolFlowDes < SmallAirVolFlow ) {
					MaxPriAirVolFlowDes = 0.0;
				}

				if ( IsAutoSize ) {
					PIU( PIUNum ).MaxPriAirVolFlow = MaxPriAirVolFlowDes;
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Primary Air Flow Rate [m3/s]", MaxPriAirVolFlowDes );
				} else {
					if ( PIU( PIUNum ).MaxPriAirVolFlow > 0.0 && MaxPriAirVolFlowDes > 0.0 ) {
						MaxPriAirVolFlowUser = PIU( PIUNum ).MaxPriAirVolFlow;
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Primary Air Flow Rate [m3/s]", MaxPriAirVolFlowDes, "User-Specified Maximum Primary Air Flow Rate [m3/s]", MaxPriAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxPriAirVolFlowDes - MaxPriAirVolFlowUser ) / MaxPriAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePIU: Potential issue with equipment sizing for " + PIU( PIUNum ).UnitType + ' ' + PIU( PIUNum ).Name );
								ShowContinueError( "User-Specified Primary Air Flow Rate of " + RoundSigDigits( MaxPriAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Primary Air Flow Rate of " + RoundSigDigits( MaxPriAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( PIU( PIUNum ).MaxTotAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( PIU( PIUNum ).MaxTotAirVolFlow > 0.0 ) {
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "User-Specified Maximum Air Flow Rate [m3/s]", PIU( PIUNum ).MaxTotAirVolFlow );
				}
			} else {
				CheckZoneSizing( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name );
				MaxTotAirVolFlowDes = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( MaxTotAirVolFlowDes < SmallAirVolFlow ) {
					MaxTotAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					PIU( PIUNum ).MaxTotAirVolFlow = MaxTotAirVolFlowDes;
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Air Flow Rate [m3/s]", MaxTotAirVolFlowDes );
				} else {
					if ( PIU( PIUNum ).MaxTotAirVolFlow > 0.0 && MaxTotAirVolFlowDes > 0.0 ) {
						MaxTotAirVolFlowUser = PIU( PIUNum ).MaxTotAirVolFlow;
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Air Flow Rate [m3/s]", MaxTotAirVolFlowDes, "User-Specified Maximum Air Flow Rate [m3/s]", MaxTotAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxTotAirVolFlowDes - MaxTotAirVolFlowUser ) / MaxTotAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePIU: Potential issue with equipment sizing for " + PIU( PIUNum ).UnitType + ' ' + PIU( PIUNum ).Name );
								ShowContinueError( "User-Specified Maximum Air Flow Rate of " + RoundSigDigits( MaxTotAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Air Flow Rate of " + RoundSigDigits( MaxTotAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( PIU( PIUNum ).MaxSecAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( PIU( PIUNum ).MaxSecAirVolFlow > 0.0 ) {
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "User-Specified Maximum Secondary Air Flow Rate [m3/s]", PIU( PIUNum ).MaxSecAirVolFlow );
				}
			} else {
				CheckZoneSizing( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name );
				MaxSecAirVolFlowDes = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( MaxSecAirVolFlowDes < SmallAirVolFlow ) {
					MaxSecAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					PIU( PIUNum ).MaxSecAirVolFlow = MaxSecAirVolFlowDes;
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Secondary Air Flow Rate [m3/s]", MaxSecAirVolFlowDes );
				} else {
					if ( PIU( PIUNum ).MaxSecAirVolFlow > 0.0 && MaxSecAirVolFlowDes > 0.0 ) {
						MaxSecAirVolFlowUser = PIU( PIUNum ).MaxSecAirVolFlow;
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Secondary Air Flow Rate [m3/s]", MaxSecAirVolFlowDes, "User-Specified Maximum Secondary Air Flow Rate [m3/s]", MaxSecAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxSecAirVolFlowDes - MaxSecAirVolFlowUser ) / MaxSecAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePIU: Potential issue with equipment sizing for " + PIU( PIUNum ).UnitType + ' ' + PIU( PIUNum ).Name );
								ShowContinueError( "User-Specified Maximum Secondary Air Flow Rate of " + RoundSigDigits( MaxSecAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Secondary Air Flow Rate of " + RoundSigDigits( MaxSecAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( PIU( PIUNum ).MinPriAirFlowFrac == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( PIU( PIUNum ).MinPriAirFlowFrac > 0.0 ) {
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "User-Specified Minimum Primary Air Flow Fraction", PIU( PIUNum ).MinPriAirFlowFrac );
				}
			} else {
				CheckZoneSizing( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name );
				if ( PIU( PIUNum ).MaxPriAirVolFlow >= SmallAirVolFlow && TermUnitFinalZoneSizing( CurZoneEqNum ).MinOA >= SmallAirVolFlow ) {
					MinPriAirFlowFracDes = TermUnitFinalZoneSizing( CurZoneEqNum ).MinOA / PIU( PIUNum ).MaxPriAirVolFlow;
				} else {
					MinPriAirFlowFracDes = 0.0;
				}
				if ( IsAutoSize ) {
					PIU( PIUNum ).MinPriAirFlowFrac = MinPriAirFlowFracDes;
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Minimum Primary Air Flow Fraction", MinPriAirFlowFracDes );
				} else {
					if ( PIU( PIUNum ).MinPriAirFlowFrac > 0.0 && MinPriAirFlowFracDes > 0.0 ) {
						MinPriAirFlowFracUser = PIU( PIUNum ).MinPriAirFlowFrac;
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Minimum Primary Air Flow Fraction", MinPriAirFlowFracDes, "User-Specified Minimum Primary Air Flow Fraction", MinPriAirFlowFracUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MinPriAirFlowFracDes - MinPriAirFlowFracUser ) / MinPriAirFlowFracUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePIU: Potential issue with equipment sizing for " + PIU( PIUNum ).UnitType + ' ' + PIU( PIUNum ).Name );
								ShowContinueError( "User-Specified Minimum Primary Air Flow Fraction of " + RoundSigDigits( MinPriAirFlowFracUser, 1 ) );
								ShowContinueError( "differs from Design Size Minimum Primary Air Flow Fraction of " + RoundSigDigits( MinPriAirFlowFracDes, 1 ) );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		if ( CurZoneEqNum > 0 ) {
			{ auto const SELECT_CASE_var( PIU( PIUNum ).UnitType_Num );
			if ( SELECT_CASE_var == SingleDuct_SeriesPIU_Reheat ) {
				TermUnitSizing( CurZoneEqNum ).AirVolFlow = PIU( PIUNum ).MaxTotAirVolFlow;
			} else if ( SELECT_CASE_var == SingleDuct_ParallelPIU_Reheat ) {
				TermUnitSizing( CurZoneEqNum ).AirVolFlow = PIU( PIUNum ).MaxSecAirVolFlow + PIU( PIUNum ).MinPriAirFlowFrac * PIU( PIUNum ).MaxPriAirVolFlow;
			}}
		}

		IsAutoSize = false;
		if ( PIU( PIUNum ).FanOnFlowFrac == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( PIU( PIUNum ).FanOnFlowFrac > 0.0 ) {
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "User-Specified Fan On Flow Fraction", PIU( PIUNum ).FanOnFlowFrac );
				}
			} else {
				CheckZoneSizing( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name );
				FanOnFlowFracDes = PIU( PIUNum ).MinPriAirFlowFrac;
				if ( IsAutoSize ) {
					PIU( PIUNum ).FanOnFlowFrac = FanOnFlowFracDes;
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Fan On Flow Fraction", FanOnFlowFracDes );
				} else {
					if ( PIU( PIUNum ).FanOnFlowFrac > 0.0 && FanOnFlowFracDes > 0.0 ) {
						FanOnFlowFracUser = PIU( PIUNum ).FanOnFlowFrac;
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Fan On Flow Fraction", FanOnFlowFracDes, "User-Specified Fan On Flow Fraction", FanOnFlowFracUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( FanOnFlowFracDes - FanOnFlowFracUser ) / FanOnFlowFracUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePIU: Potential issue with equipment sizing for " + PIU( PIUNum ).UnitType + ' ' + PIU( PIUNum ).Name );
								ShowContinueError( "User-Specified Fan On Flow Fraction of " + RoundSigDigits( FanOnFlowFracUser, 1 ) );
								ShowContinueError( "differs from Design Size Fan On Flow Fraction of " + RoundSigDigits( FanOnFlowFracDes, 1 ) );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( PIU( PIUNum ).MaxVolHotWaterFlow == AutoSize ) { //.or.()) THEN
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( PIU( PIUNum ).MaxVolHotWaterFlow > 0.0 ) {
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "User-Specified Maximum Reheat Water Flow Rate [m3/s]", PIU( PIUNum ).MaxVolHotWaterFlow );
				}
			} else {
				CheckZoneSizing( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name );
				if ( SameString( PIU( PIUNum ).HCoilType, "Coil:Heating:Water" ) ) {

					CoilWaterInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", PIU( PIUNum ).HCoil, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( "Coil:Heating:Water", PIU( PIUNum ).HCoil, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", PIU( PIUNum ).HCoil, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {

							if ( TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallAirVolFlow ) {
								CoilInTemp = TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU * PIU( PIUNum ).MinPriAirFlowFrac + TermUnitFinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak * ( 1.0 - PIU( PIUNum ).MinPriAirFlowFrac );
								CoilOutTemp = TermUnitFinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
								CoilOutHumRat = TermUnitFinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
								DesMassFlow = StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow;
								DesCoilLoad = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) ) * DesMassFlow * ( CoilOutTemp - CoilInTemp );

								rho = GetDensityGlycol( PlantLoop( PIU( PIUNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( PIU( PIUNum ).HWLoopNum ).FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( PlantLoop( PIU( PIUNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( PIU( PIUNum ).HWLoopNum ).FluidIndex, RoutineName );

								MaxVolHotWaterFlowDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
							} else {
								MaxVolHotWaterFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in" + PIU( PIUNum ).UnitType + " Object=" + PIU( PIUNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						PIU( PIUNum ).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Reheat Water Flow Rate [m3/s]", MaxVolHotWaterFlowDes );
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Reheat Coil Inlet Air Temperature [C]", TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU );
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Reheat Coil Inlet Air Humidity Ratio [kgWater/kgDryAir]", TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU );
					} else { // Hardsize with sizing data
						if ( PIU( PIUNum ).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0 ) {
							MaxVolHotWaterFlowUser = PIU( PIUNum ).MaxVolHotWaterFlow;
							ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Reheat Water Flow Rate [m3/s]", MaxVolHotWaterFlowDes, "User-Specified Maximum Reheat Water Flow Rate [m3/s]", MaxVolHotWaterFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser ) / MaxVolHotWaterFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizePIU: Potential issue with equipment sizing for " + PIU( PIUNum ).UnitType + ' ' + PIU( PIUNum ).Name );
									ShowContinueError( "User-Specified Maximum Reheat Water Flow Rate of " + RoundSigDigits( MaxVolHotWaterFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Reheat Water Flow Rate of " + RoundSigDigits( MaxVolHotWaterFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				} else {
					PIU( PIUNum ).MaxVolHotWaterFlow = 0.0;
				}
			}
		}

		IsAutoSize = false;
		if ( PIU( PIUNum ).MaxVolHotSteamFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( PIU( PIUNum ).MaxVolHotWaterFlow > 0.0 ) {
					ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "User-Specified Maximum Reheat Steam Flow Rate [m3/s]", PIU( PIUNum ).MaxVolHotWaterFlow );
				}
			} else {
				if ( SameString( PIU( PIUNum ).HCoilType, "Coil:Heating:Steam" ) ) {

					CoilSteamInletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", PIU( PIUNum ).HCoil, ErrorsFound );
					CoilSteamOutletNode = GetCoilSteamOutletNode( "Coil:Heating:Steam", PIU( PIUNum ).HCoil, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Steam", PIU( PIUNum ).HCoil, CoilSteamInletNode, CoilSteamOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {

							if ( TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallAirVolFlow ) {
								CoilInTemp = TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU * PIU( PIUNum ).MinPriAirFlowFrac + TermUnitFinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak * ( 1.0 - PIU( PIUNum ).MinPriAirFlowFrac );
								CoilOutTemp = TermUnitFinalZoneSizing( CurZoneEqNum ).HeatDesTemp;
								CoilOutHumRat = TermUnitFinalZoneSizing( CurZoneEqNum ).HeatDesHumRat;
								DesMassFlow = StdRhoAir * TermUnitSizing( CurZoneEqNum ).AirVolFlow;
								DesCoilLoad = PsyCpAirFnWTdb( CoilOutHumRat, 0.5 * ( CoilInTemp + CoilOutTemp ) ) * DesMassFlow * ( CoilOutTemp - CoilInTemp );
								TempSteamIn = 100.00;
								EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, PIU( PIUNum ).HCoil_FluidIndex, RoutineName );
								EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, PIU( PIUNum ).HCoil_FluidIndex, RoutineName );
								LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
								SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, PIU( PIUNum ).HCoil_FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( fluidNameWater, PlantSizData( PltSizHeatNum ).ExitTemp, DummyWaterIndex, RoutineName );
								MaxVolHotSteamFlowDes = DesCoilLoad / ( SteamDensity * ( LatentHeatSteam + PlantSizData( PltSizHeatNum ).DeltaT * Cp ) );
							} else {
								MaxVolHotSteamFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of Steam flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in" + PIU( PIUNum ).UnitType + " Object=" + PIU( PIUNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						PIU( PIUNum ).MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
						ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Reheat Steam Flow [m3/s]", MaxVolHotSteamFlowDes );
					} else {
						if ( PIU( PIUNum ).MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0 ) {
							MaxVolHotSteamFlowUser = PIU( PIUNum ).MaxVolHotSteamFlow;
							ReportSizingOutput( PIU( PIUNum ).UnitType, PIU( PIUNum ).Name, "Design Size Maximum Reheat Steam Flow [m3/s]", MaxVolHotSteamFlowDes, "User-Specified Maximum Reheat Steam Flow [m3/s]", MaxVolHotSteamFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser ) / MaxVolHotSteamFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizePIU: Potential issue with equipment sizing for " + PIU( PIUNum ).UnitType + ' ' + PIU( PIUNum ).Name );
									ShowContinueError( "User-Specified Maximum Reheat Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Reheat Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				} else {
					PIU( PIUNum ).MaxVolHotSteamFlow = 0.0;
				}
			}
		}

		if ( CurZoneEqNum > 0 ) {
			TermUnitSizing( CurZoneEqNum ).MinFlowFrac = PIU( PIUNum ).MinPriAirFlowFrac;
			TermUnitSizing( CurZoneEqNum ).MaxHWVolFlow = PIU( PIUNum ).MaxVolHotWaterFlow;
			TermUnitSizing( CurZoneEqNum ).MaxSTVolFlow = PIU( PIUNum ).MaxVolHotSteamFlow;
			TermUnitSizing( CurZoneEqNum ).InducesPlenumAir = PIU( PIUNum ).InducesPlenumAir;
			if ( PIU( PIUNum ).HCoilType_Num == HCoilType_SimpleHeating ) {
				SetCoilDesFlow( PIU( PIUNum ).HCoilType, PIU( PIUNum ).HCoil, TermUnitSizing( CurZoneEqNum ).AirVolFlow, ErrorsFound );
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcSeriesPIU(
		int const PIUNum, // number of the current PIU being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNode, // zone node number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a series powered induction unit; adjust its primary air flow
		// and reheat coil output to match the zone load.

		// METHODOLOGY EMPLOYED:
		// If unit is on and there is a cooling load:
		// (1) simulates mixer and fan at max secondary air flow and heating coil
		//     off. Obtains fan temperature increase.
		// (2) Calculates primary and secomdary air flow to meet zone load and
		//     resimulates mixer, fan, and (off) coil.
		// If unit is on and there is a heating load
		// (1) sets primary air flow to a minimum.
		// (2) simulates mixer and fan
		// (3) if reheat is hot water, calls ControlCompOutput to simulate hot
		//     water coil and adjust water flow to match coil output to the zone load.
		// (4) if reheat is electric or gas calls SimulateHeatingCoilComponents to
		//     simulate coil at coil output that matches the zone load

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using MixerComponent::SimAirMixer;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using Fans::SimulateFanComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QZnReq; // heating or cooling needed by zone [Watts]
		Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
		Real64 QActualHeating; // the heating load seen by the reheat coil [W]
		Real64 PowerMet; // power supplied
		bool UnitOn( true ); // TRUE if unit is on
		bool PriOn( true ); // TRUE if primary air available
		bool HCoilOn( true ); // TRUE if heating coil is on
		int ControlNode( 0 ); // the hot water or cold water inlet node
		Real64 ControlOffset; // tolerance for output control
		Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/s]
		Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/s]
		int OutletNode; // unit air outlet node
		int PriNode; // unit primary air inlet node
		int SecNode; // unit secondary air inlet node
		int HCoilInAirNode; // air inlet node of reheat coil
		Real64 QCoilReq; // required heating coil outlet to meet zone load
		Real64 PriAirMassFlow; // primary air mass flow rate [kg/s]
		Real64 PriAirMassFlowMax; // max primary air mass flow rate [kg/s]
		Real64 PriAirMassFlowMin; // min primary air mass flow rate [kg/s]
		Real64 SecAirMassFlow; // secondary air mass flow rate [kg/s]
		Real64 CpAirZn; // zone air specific heat [J/kg-C]
		Real64 FanDeltaTemp( 0.0 ); // fan temperature rise [C]
		Real64 OutletTempNeeded( 0.0 ); // unit outlet temperature needed to meet cooling load
		Real64 MixTempNeeded( 0.0 ); // mixer outlet temperature needed to meet cooling load
		Real64 MinSteamFlow;
		Real64 MaxSteamFlow;
		Real64 mdot; // local plant fluid flow rate kg/s

		// FLOW

		FanElecPower = 0.0;
		// initialize local variables
		ControlOffset = PIU( PIUNum ).HotControlOffset;
		OutletNode = PIU( PIUNum ).OutAirNode;
		PriNode = PIU( PIUNum ).PriAirInNode;
		SecNode = PIU( PIUNum ).SecAirInNode;
		HCoilInAirNode = PIU( PIUNum ).HCoilInAirNode;
		ControlNode = PIU( PIUNum ).HotControlNode;
		PriAirMassFlow = Node( PriNode ).MassFlowRate;
		PriAirMassFlowMax = Node( PriNode ).MassFlowRateMaxAvail;
		PriAirMassFlowMin = Node( PriNode ).MassFlowRateMinAvail;
		SecAirMassFlow = Node( SecNode ).MassFlowRate;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, Node( ZoneNode ).Temp );

		//On the first HVAC iteration the system values are given to the controller, but after that
		// the demand limits are in place and there needs to be feedback to the Zone Equipment
		if ( ControlNode > 0 ) {
			if ( FirstHVACIteration ) {
				MaxWaterFlow = PIU( PIUNum ).MaxHotWaterFlow;
				MinWaterFlow = PIU( PIUNum ).MinHotWaterFlow;
				MaxSteamFlow = PIU( PIUNum ).MaxHotWaterFlow; // Need TO change THESE******************************
				MinSteamFlow = PIU( PIUNum ).MinHotWaterFlow;
			} else {
				MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
				MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				MaxSteamFlow = Node( ControlNode ).MassFlowRateMaxAvail;
				MinSteamFlow = Node( ControlNode ).MassFlowRateMinAvail;
			}
		}
		if ( GetCurrentScheduleValue( PIU( PIUNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( PriAirMassFlow <= SmallMassFlow || PriAirMassFlowMax <= SmallMassFlow ) PriOn = false;
		// Set the mass flow rates
		if ( UnitOn ) {
			// unit is on
			if ( ! PriOn ) {
				// no primary air flow
				PriAirMassFlow = 0.0;
				SecAirMassFlow = PIU( PIUNum ).MaxTotAirMassFlow;
			} else if ( CurDeadBandOrSetback( ZoneNum ) || std::abs( QZnReq ) < SmallLoad ) {
				// in deadband or very small load: set primary air flow to the minimum
				PriAirMassFlow = PriAirMassFlowMin;
				SecAirMassFlow = max( 0.0, PIU( PIUNum ).MaxTotAirMassFlow - PriAirMassFlow );
			} else if ( QZnReq > SmallLoad ) {
				// heating: set primary air flow to the minimum
				PriAirMassFlow = PriAirMassFlowMin;
				SecAirMassFlow = max( 0.0, PIU( PIUNum ).MaxTotAirMassFlow - PriAirMassFlow );
			} else {
				// cooling: set the primary air flow rate to meet the load.
				// First calculate the fan temperature rise
				// use only secondary air for this calculation
				Node( PriNode ).MassFlowRate = 0.0;
				Node( SecNode ).MassFlowRate = PIU( PIUNum ).MaxTotAirMassFlow;
				SimAirMixer( PIU( PIUNum ).MixerName, PIU( PIUNum ).Mixer_Num ); // fire the mixer
				SimulateFanComponents( PIU( PIUNum ).FanName, FirstHVACIteration, PIU( PIUNum ).Fan_Index ); // fire the fan
				FanDeltaTemp = Node( HCoilInAirNode ).Temp - Node( SecNode ).Temp;
				// using the required zone load, calculate the air temperature needed to meet the load
				// PIU(PIUNum)%MaxTotAirMassFlow * CpAirZn * (OutletTempNeeded - Node(ZoneNodeNum)%Temp) = QZnReq
				OutletTempNeeded = Node( ZoneNode ).Temp + QZnReq / ( PIU( PIUNum ).MaxTotAirMassFlow * CpAirZn );
				MixTempNeeded = OutletTempNeeded - FanDeltaTemp;
				if ( MixTempNeeded <= Node( PriNode ).Temp ) {
					PriAirMassFlow = PriAirMassFlowMax;
				} else if ( MixTempNeeded >= Node( PriNode ).Temp && MixTempNeeded >= Node( SecNode ).Temp ) {
					PriAirMassFlow = PriAirMassFlowMin;
				} else {
					PriAirMassFlow = PIU( PIUNum ).MaxTotAirMassFlow * ( Node( SecNode ).Temp - MixTempNeeded ) / max( SmallTempDiff, Node( SecNode ).Temp - Node( PriNode ).Temp );
					PriAirMassFlow = min( max( PriAirMassFlow, PriAirMassFlowMin ), PriAirMassFlowMax );
				}
				SecAirMassFlow = max( 0.0, PIU( PIUNum ).MaxTotAirMassFlow - PriAirMassFlow );
			}
		} else {
			// unit is off ; no flow
			PriAirMassFlow = 0.0;
			SecAirMassFlow = 0.0;
		}
		// Set inlet node flowrates
		Node( PriNode ).MassFlowRate = PriAirMassFlow;
		Node( SecNode ).MassFlowRate = SecAirMassFlow;
		//now that inlet airflows have been set, the terminal bos components can be simulated.

		// fire the mixer
		SimAirMixer( PIU( PIUNum ).MixerName, PIU( PIUNum ).Mixer_Num );
		// fire the fan
		SimulateFanComponents( PIU( PIUNum ).FanName, FirstHVACIteration, PIU( PIUNum ).Fan_Index );
		// check if heating coil is off
		QActualHeating = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
		if ( ( ! UnitOn ) || ( QActualHeating < SmallLoad ) || ( TempControlType( ZoneNum ) == SingleCoolingSetPoint ) || ( PriAirMassFlow > PriAirMassFlowMin ) ) {
			HCoilOn = false;
		}
		//fire the heating coil

		{ auto const SELECT_CASE_var( PIU( PIUNum ).HCoilType_Num );

		if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
			if ( ! HCoilOn ) {
				//call the reheat coil with the NO FLOW condition
				mdot = 0.0;
				SetComponentFlowRate( mdot, PIU( PIUNum ).HotControlNode, PIU( PIUNum ).HotCoilOutNodeNum, PIU( PIUNum ).HWLoopNum, PIU( PIUNum ).HWLoopSide, PIU( PIUNum ).HWBranchNum, PIU( PIUNum ).HWCompNum );

				SimulateWaterCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, PIU( PIUNum ).HCoil_Index );
			} else {
				// control water flow to obtain output matching QZnReq
				ControlCompOutput( PIU( PIUNum ).HCoil, PIU( PIUNum ).UnitType, PIU( PIUNum ).HCoil_Index, FirstHVACIteration, QActualHeating, ControlNode, MaxWaterFlow, MinWaterFlow, ControlOffset, PIU( PIUNum ).ControlCompTypeNum, PIU( PIUNum ).CompErrIndex, HCoilInAirNode, OutletNode, _, _, _, PIU( PIUNum ).HWLoopNum, PIU( PIUNum ).HWLoopSide, PIU( PIUNum ).HWBranchNum );
			}
		} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // COIL:STEAM:AIRHEATING
			if ( ! HCoilOn ) {
				QCoilReq = 0.0;
			} else {
				QCoilReq = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
			}
			SimulateSteamCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, PIU( PIUNum ).HCoil_Index, QCoilReq );

		} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
			if ( ! HCoilOn ) {
				QCoilReq = 0.0;
			} else {
				QCoilReq = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
			}
			SimulateHeatingCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, QCoilReq, PIU( PIUNum ).HCoil_Index );

		} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
			if ( ! HCoilOn ) {
				QCoilReq = 0.0;
			} else {
				QCoilReq = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
			}
			SimulateHeatingCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, QCoilReq, PIU( PIUNum ).HCoil_Index );

		}}

		PowerMet = Node( OutletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
		PIU( PIUNum ).HeatingRate = max( 0.0, PowerMet );
		PIU( PIUNum ).SensCoolRate = std::abs( min( constant_zero, PowerMet ) );
		if ( Node( OutletNode ).MassFlowRate == 0.0 ) {
			Node( PriNode ).MassFlowRate = 0.0;
			Node( SecNode ).MassFlowRate = 0.0;
		}
		if ( PIU( PIUNum ).InducesPlenumAir ) {
			PlenumInducedMassFlow = Node( SecNode ).MassFlowRate;
		} else {
			PlenumInducedMassFlow = 0.0;
		}
		Node( OutletNode ).MassFlowRateMax = PIU( PIUNum ).MaxTotAirMassFlow;

	}

	void
	CalcParallelPIU(
		int const PIUNum, // number of the current PIU being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNode, // zone node number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a parallel powered induction unit; adjust its primary air flow
		// and reheat coil output to match the zone load.

		// METHODOLOGY EMPLOYED:
		// If unit is on and there is a cooling load:
		// (1) simulate fan at max secondary air flow and heating coil
		//     off. Obtains fan temperature increase.
		// (2) Calculates primary and secomdary air flow to meet zone load.
		//     (a) Assume fan is off and calculate primary air flow to meet cooling load.
		//     (b) If calculated primary air flow is above the fan turn on ratio, fan is off.
		//         Otherwise fan is on; calculate mixed secondary and primary air flow that
		//         will meet the zone load
		//  (3) Simulate fan, mixer, and (off) heating coil to obtain zone inlet conditions.
		// If unit is on and there is a heating load
		// (1) sets primary air flow to a minimum.
		// (2) simulates fan and mixer
		// (3) if reheat is hot water, calls ControlCompOutput to simulate hot
		//     water coil and adjust water flow to match coil output to the zone load.
		// (4) if reheat is electric or gas calls SimulateHeatingCoilComponents to
		//     simulate coil at coil output that matches the zone load

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using MixerComponent::SimAirMixer;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using Fans::SimulateFanComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QZnReq; // heating or cooling needed by zone [Watts]
		Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
		Real64 QActualHeating; // the heating load seen by the reheat coil [W]
		Real64 PowerMet; // power supplied
		bool UnitOn( true ); // TRUE if unit is on
		bool PriOn( true ); // TRUE if primary air available
		bool HCoilOn( true ); // TRUE if heating coil is on
		int ControlNode( 0 ); // the hot water or cold water inlet node
		Real64 ControlOffset; // tolerance for output control
		Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/s]
		Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/s]
		int OutletNode; // unit air outlet node
		int PriNode; // unit primary air inlet node
		int SecNode; // unit secondary air inlet node
		int HCoilInAirNode; // air inlet node of reheat coil
		Real64 QCoilReq; // required heating coil outlet to meet zone load
		Real64 PriAirMassFlow; // primary air mass flow rate [kg/s]
		Real64 PriAirMassFlowMax; // max primary air mass flow rate [kg/s]
		Real64 PriAirMassFlowMin; // min primary air mass flow rate [kg/s]
		Real64 SecAirMassFlow; // secondary air mass flow rate [kg/s]
		Real64 CpAirZn; // zone air specific heat [J/kg-C]
		Real64 FanDeltaTemp( 0.0 ); // fan temperature rise [C]
		//unusedREAL(r64)    :: MaxSteamFlow
		//unusedREAL(r64)    :: MinSteamFlow
		Real64 mdot; // local fluid flow rate kg/s

		// FLOW

		FanElecPower = 0.0;
		// initialize local variables
		ControlOffset = PIU( PIUNum ).HotControlOffset;
		OutletNode = PIU( PIUNum ).OutAirNode;
		PriNode = PIU( PIUNum ).PriAirInNode;
		SecNode = PIU( PIUNum ).SecAirInNode;
		HCoilInAirNode = PIU( PIUNum ).HCoilInAirNode;
		ControlNode = PIU( PIUNum ).HotControlNode;
		PriAirMassFlow = Node( PriNode ).MassFlowRate;
		PriAirMassFlowMax = Node( PriNode ).MassFlowRateMaxAvail;
		PriAirMassFlowMin = Node( PriNode ).MassFlowRateMinAvail;
		SecAirMassFlow = Node( SecNode ).MassFlowRate;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, Node( ZoneNode ).Temp );

		//On the first HVAC iteration the system values are given to the controller, but after that
		// the demand limits are in place and there needs to be feedback to the Zone Equipment
		if ( ControlNode > 0 ) {
			if ( FirstHVACIteration ) {
				MaxWaterFlow = PIU( PIUNum ).MaxHotWaterFlow;
				MinWaterFlow = PIU( PIUNum ).MinHotWaterFlow;
			} else {
				MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
				MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
			}
		}
		if ( GetCurrentScheduleValue( PIU( PIUNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( PriAirMassFlow <= SmallMassFlow || PriAirMassFlowMax <= SmallMassFlow ) PriOn = false;
		// Set the mass flow rates
		if ( UnitOn ) {
			// unit is on
			if ( ! PriOn ) {
				// no primary air flow
				PriAirMassFlow = 0.0;
				SecAirMassFlow = PIU( PIUNum ).MaxSecAirMassFlow;
			} else if ( CurDeadBandOrSetback( ZoneNum ) || std::abs( QZnReq ) < SmallLoad ) {
				// in deadband or very small load: set primary air flow to the minimum
				PriAirMassFlow = PriAirMassFlowMin;
				SecAirMassFlow = PIU( PIUNum ).MaxSecAirMassFlow;
			} else if ( QZnReq > SmallLoad ) {
				// heating: set primary air flow to the minimum
				PriAirMassFlow = PriAirMassFlowMin;
				SecAirMassFlow = PIU( PIUNum ).MaxSecAirMassFlow;
			} else {
				// cooling: set the primary air flow rate to meet the load.
				// First calculate the fan temperature rise
				Node( SecNode ).MassFlowRate = PIU( PIUNum ).MaxSecAirMassFlow;
				Node( SecNode ).MassFlowRateMaxAvail = PIU( PIUNum ).MaxSecAirMassFlow;
				Node( PriNode ).MassFlowRate = 0.0;
				SimulateFanComponents( PIU( PIUNum ).FanName, FirstHVACIteration, PIU( PIUNum ).Fan_Index ); // fire the fan
				SimAirMixer( PIU( PIUNum ).MixerName, PIU( PIUNum ).Mixer_Num ); // fire the mixer
				FanDeltaTemp = Node( HCoilInAirNode ).Temp - Node( SecNode ).Temp;
				// Assuming the fan is off, calculate the primary air flow needed to meet the zone cooling demand.
				// CpAir*PriAirMassFlow*(Node(PriNode)%Temp - Node(ZoneNodeNum)%Temp) = QZnReq
				PriAirMassFlow = QZnReq / ( CpAirZn * min( -SmallTempDiff, ( Node( PriNode ).Temp - Node( ZoneNode ).Temp ) ) );
				PriAirMassFlow = min( max( PriAirMassFlow, PriAirMassFlowMin ), PriAirMassFlowMax );
				// check for fan on or off
				if ( PriAirMassFlow > PIU( PIUNum ).FanOnAirMassFlow ) {
					SecAirMassFlow = 0.0; // Fan is off; no secondary air
				} else {
					// fan is on; recalc primary air flow
					// CpAir*PriAirMassFlow*(Node(PriNode)%Temp - Node(ZoneNodeNum)%Temp) +
					//   CpAir*SecAirMassFlow*(Node(SecNode)%Temp + FanDeltaTemp - Node(ZoneNodeNum)%Temp) = QZnReq
					PriAirMassFlow = ( QZnReq - CpAirZn * SecAirMassFlow * ( Node( SecNode ).Temp + FanDeltaTemp - Node( ZoneNode ).Temp ) ) / ( CpAirZn * min( -SmallTempDiff, ( Node( PriNode ).Temp - Node( ZoneNode ).Temp ) ) );
					PriAirMassFlow = min( max( PriAirMassFlow, PriAirMassFlowMin ), PriAirMassFlowMax );
					SecAirMassFlow = PIU( PIUNum ).MaxSecAirMassFlow;
				}
			}
		} else {
			// unit is off; no flow
			PriAirMassFlow = 0.0;
			SecAirMassFlow = 0.0;
		}
		// Set inlet node flowrates
		Node( PriNode ).MassFlowRate = PriAirMassFlow;
		Node( SecNode ).MassFlowRate = SecAirMassFlow;
		Node( SecNode ).MassFlowRateMaxAvail = SecAirMassFlow;
		//now that inlet airflows have been set, the terminal bos components can be simulated.
		// fire the fan
		SimulateFanComponents( PIU( PIUNum ).FanName, FirstHVACIteration, PIU( PIUNum ).Fan_Index );
		// fire the mixer
		SimAirMixer( PIU( PIUNum ).MixerName, PIU( PIUNum ).Mixer_Num );
		// check if heating coil is off
		QActualHeating = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
		if ( ( ! UnitOn ) || ( QActualHeating < SmallLoad ) || ( TempControlType( ZoneNum ) == SingleCoolingSetPoint ) || ( PriAirMassFlow > PriAirMassFlowMin ) ) {
			HCoilOn = false;
		}
		//fire the heating coil
		{ auto const SELECT_CASE_var( PIU( PIUNum ).HCoilType_Num );

		if ( SELECT_CASE_var == HCoilType_SimpleHeating ) { // COIL:WATER:SIMPLEHEATING
			if ( ! HCoilOn ) {
				//call the reheat coil with the NO FLOW condition
				mdot = 0.0;
				SetComponentFlowRate( mdot, PIU( PIUNum ).HotControlNode, PIU( PIUNum ).HotCoilOutNodeNum, PIU( PIUNum ).HWLoopNum, PIU( PIUNum ).HWLoopSide, PIU( PIUNum ).HWBranchNum, PIU( PIUNum ).HWCompNum );
				SimulateWaterCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, PIU( PIUNum ).HCoil_Index );
			} else {
				// control water flow to obtain output matching QZnReq
				ControlCompOutput( PIU( PIUNum ).HCoil, PIU( PIUNum ).UnitType, PIU( PIUNum ).HCoil_Index, FirstHVACIteration, QActualHeating, ControlNode, MaxWaterFlow, MinWaterFlow, ControlOffset, PIU( PIUNum ).ControlCompTypeNum, PIU( PIUNum ).CompErrIndex, HCoilInAirNode, OutletNode, _, _, _, PIU( PIUNum ).HWLoopNum, PIU( PIUNum ).HWLoopSide, PIU( PIUNum ).HWBranchNum );
			}
		} else if ( SELECT_CASE_var == HCoilType_SteamAirHeating ) { // COIL:STEAM:AIRHEATING
			if ( ! HCoilOn ) {
				QCoilReq = 0.0;
			} else {
				QCoilReq = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
			}
			SimulateSteamCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, PIU( PIUNum ).HCoil_Index, QCoilReq );
		} else if ( SELECT_CASE_var == HCoilType_Electric ) { // COIL:ELECTRIC:HEATING
			if ( ! HCoilOn ) {
				QCoilReq = 0.0;
			} else {
				QCoilReq = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
			}
			SimulateHeatingCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, QCoilReq, PIU( PIUNum ).HCoil_Index );

		} else if ( SELECT_CASE_var == HCoilType_Gas ) { // COIL:GAS:HEATING
			if ( ! HCoilOn ) {
				QCoilReq = 0.0;
			} else {
				QCoilReq = QToHeatSetPt - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( ZoneNode ).Temp );
			}
			SimulateHeatingCoilComponents( PIU( PIUNum ).HCoil, FirstHVACIteration, QCoilReq, PIU( PIUNum ).HCoil_Index );

		}}
		PowerMet = Node( OutletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
		PIU( PIUNum ).HeatingRate = max( 0.0, PowerMet );
		PIU( PIUNum ).SensCoolRate = std::abs( min( constant_zero, PowerMet ) );
		if ( Node( OutletNode ).MassFlowRate == 0.0 ) {
			Node( PriNode ).MassFlowRate = 0.0;
			Node( SecNode ).MassFlowRate = 0.0;
		}
		if ( PIU( PIUNum ).InducesPlenumAir ) {
			PlenumInducedMassFlow = Node( SecNode ).MassFlowRate;
		} else {
			PlenumInducedMassFlow = 0.0;
		}
		Node( OutletNode ).MassFlowRateMax = PIU( PIUNum ).MaxPriAirMassFlow;

	}

	void
	ReportPIU( int const PIUNum ) // number of the current fan coil unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fills some of the report variables for the PIU terminal boxes

		// METHODOLOGY EMPLOYED:
		// NA

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

		// FLOW

		PIU( PIUNum ).HeatingEnergy = PIU( PIUNum ).HeatingRate * TimeStepSys * SecInHour;
		PIU( PIUNum ).SensCoolEnergy = PIU( PIUNum ).SensCoolRate * TimeStepSys * SecInHour;

	}

	// ===================== Utilities =====================================

	bool
	PIUnitHasMixer( std::string const & CompName ) // component (mixer) name
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given a mixer name, this routine determines if that mixer is found on
		// PIUnits.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		bool YesNo; // True if found

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int ItemNum;

		if ( GetPIUInputFlag ) {
			GetPIUs();
			GetPIUInputFlag = false;
		}

		YesNo = false;
		if ( NumPIUs > 0 ) {
			ItemNum = FindItemInList( CompName, PIU, &PowIndUnitData::MixerName );
			if ( ItemNum > 0 ) YesNo = true;
		}

		return YesNo;

	}

	void
	PIUInducesPlenumAir( int const NodeNum ) // induced air node number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Marks a PIU air terminal unit as obtaining its induced air from
		// a plenum.

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

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PIUIndex;

		if ( GetPIUInputFlag ) {
			GetPIUs();
			GetPIUInputFlag = false;
		}

		for ( PIUIndex = 1; PIUIndex <= NumPIUs; ++PIUIndex ) {
			if ( NodeNum == PIU( PIUIndex ).SecAirInNode ) {
				PIU( PIUIndex ).InducesPlenumAir = true;
				break;
			}
		}

	}

} // PoweredInductionUnits

} // EnergyPlus
