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
#include <HVACSingleDuctInduc.hh>
#include <BranchNodeConnections.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
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
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace HVACSingleDuctInduc {

	// Module containing routines dealing terminal 4 pipe induction terminal units

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   June 15 2004
	//       MODIFIED       Brent Griffith, Sept 2010, plant upgrades, fluid props
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms needed to simulate 4 pipe induction terminal units

	// METHODOLOGY EMPLOYED:
	// The terminal boxes are modeled as compound components: heating coil, cooling coil and
	// mixer. The combined components are controlled to meet the zone load.

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::NumOfZones;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::DisplayExtraWarnings;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;
	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyHFnTdbW;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::SmallAirVolFlow;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const SingleDuct_CV_FourPipeInduc( 1 );
	int const SingleDuct_CV_2PipeInduc( 2 );
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	int NumIndUnits( 0 );
	int NumFourPipes( 0 );
	Array1D_bool CheckEquipName;
	bool GetIUInputFlag( true ); // First time, input is "gotten"

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACSingleDuctInduc:

	// PRIVATE UpdateIndUnit
	// PRIVATE ReportIndUnit

	// Object Data
	Array1D< IndUnitData > IndUnit;

	// Functions

	void
	SimIndUnit(
		std::string const & CompName, // name of the terminal unit
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the terminal unit
		int const ZoneNodeNum, // zone node number of zone served by the terminal unit
		int & CompIndex // which terminal unit in data structure
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 18 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a passive (no fan) induction terminal unit.
		// Called from SimZoneAirLoopEquipment in module ZoneAirLoopEquipmentManager.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataSizing::TermUnitIU;
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
		int IUNum; // index of terminal unit being simulated

		// First time SimIndUnit is called, get the input for all the passive terminal induction units
		if ( GetIUInputFlag ) {
			GetIndUnits();
			GetIUInputFlag = false;
		}

		// Get the induction unit index
		if ( CompIndex == 0 ) {
			IUNum = FindItemInList( CompName, IndUnit );
			if ( IUNum == 0 ) {
				ShowFatalError( "SimIndUnit: Induction Unit not found=" + CompName );
			}
			CompIndex = IUNum;
		} else {
			IUNum = CompIndex;
			if ( IUNum > NumIndUnits || IUNum < 1 ) {
				ShowFatalError( "SimIndUnit: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Number of Induction Units=" + TrimSigDigits( NumIndUnits ) + ", System name=" + CompName );
			}
			if ( CheckEquipName( IUNum ) ) {
				if ( CompName != IndUnit( IUNum ).Name ) {
					ShowFatalError( "SimIndUnit: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Induction Unit name=" + CompName + ", stored Induction Unit for that index=" + IndUnit( IUNum ).Name );
				}
				CheckEquipName( IUNum ) = false;
			}
		}

		// initialize the unit
		InitIndUnit( IUNum, FirstHVACIteration );

		TermUnitIU = true;

		// Select the correct unit type
		{ auto const SELECT_CASE_var( IndUnit( IUNum ).UnitType_Num );

		if ( SELECT_CASE_var == SingleDuct_CV_FourPipeInduc ) {

			SimFourPipeIndUnit( IUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration );

		} else {
			ShowSevereError( "Illegal Induction Unit Type used=" + IndUnit( IUNum ).UnitType );
			ShowContinueError( "Occurs in Induction Unit=" + IndUnit( IUNum ).Name );
			ShowFatalError( "Preceding condition causes termination." );

		}}

		TermUnitIU = false;

		// the tasks usually done by the Update and Report routines are not required in a compound terminal unit.

		// Update the current unit's outlet nodes. No update needed
		// CALL UpdateIndUnit(IUNum)

		// Fill the report variables. There are no report variables
		// CALL ReportIndUnit(IUNum)

	}

	void
	GetIndUnits()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 15 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for passive induction air terminal units and stores it in the
		// induction terminal unit data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using BranchNodeConnections::SetUpCompSets;
		using DataZoneEquipment::ZoneEquipConfig;
		using namespace DataSizing;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using WaterCoils::GetCoilWaterInletNode;
		using namespace DataIPShortCuts;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using MixerComponent::GetZoneMixerIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetIndUnits " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		int IUIndex; // loop index
		int IUNum; // current fan coil number
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		static int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int CtrlZone; // controlled zome do loop index
		int SupAirIn; // controlled zone supply air inlet index
		bool AirNodeFound;
		int ADUNum;
		bool errFlag;

		// find the number of each type of induction unit
		CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction";
		NumFourPipes = GetNumObjectsFound( CurrentModuleObject );
		NumIndUnits = NumFourPipes;
		// allocate the data structures
		IndUnit.allocate( NumIndUnits );
		CheckEquipName.dimension( NumIndUnits, true );

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// loop over Series PIUs; get and load the input data
		for ( IUIndex = 1; IUIndex <= NumFourPipes; ++IUIndex ) {

			GetObjectItem( CurrentModuleObject, IUIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IUNum = IUIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), IndUnit, IUNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			IndUnit( IUNum ).Name = Alphas( 1 );
			IndUnit( IUNum ).UnitType = CurrentModuleObject;
			IndUnit( IUNum ).UnitType_Num = SingleDuct_CV_FourPipeInduc;
			IndUnit( IUNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				IndUnit( IUNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				IndUnit( IUNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( IndUnit( IUNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			IndUnit( IUNum ).MaxTotAirVolFlow = Numbers( 1 );
			IndUnit( IUNum ).InducRatio = Numbers( 2 );
			if ( lNumericBlanks( 2 ) ) IndUnit( IUNum ).InducRatio = 2.5;

			IndUnit( IUNum ).PriAirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFields( 3 ) );
			IndUnit( IUNum ).SecAirInNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFields( 4 ) );
			IndUnit( IUNum ).OutAirNode = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFields( 5 ) );

			IndUnit( IUNum ).HCoilType = Alphas( 6 ); // type (key) of heating coil
			if ( SameString( IndUnit( IUNum ).HCoilType, "Coil:Heating:Water" ) ) {
				IndUnit( IUNum ).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
			}

			IndUnit( IUNum ).HCoil = Alphas( 7 ); // name of heating coil object
			IsNotOK = false;
			IndUnit( IUNum ).HWControlNode = GetCoilWaterInletNode( IndUnit( IUNum ).HCoilType, IndUnit( IUNum ).HCoil, IsNotOK );
			if ( IsNotOK ) {
				ShowContinueError( "In " + CurrentModuleObject + " = " + IndUnit( IUNum ).Name );
				ShowContinueError( "..Only Coil:Heating:Water is allowed." );
				ErrorsFound = true;
			}
			//      GetOnlySingleNode(Alphas(6),ErrorsFound,'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction',Alphas(1), &
			//                        NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
			IndUnit( IUNum ).MaxVolHotWaterFlow = Numbers( 3 );
			IndUnit( IUNum ).MinVolHotWaterFlow = Numbers( 4 );
			IndUnit( IUNum ).HotControlOffset = Numbers( 5 );

			IndUnit( IUNum ).CCoilType = Alphas( 8 ); // type (key) of cooling coil

			if ( SameString( IndUnit( IUNum ).CCoilType, "Coil:Cooling:Water" ) ) {
				IndUnit( IUNum ).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
			} else if ( SameString( IndUnit( IUNum ).CCoilType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
				IndUnit( IUNum ).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
			}

			IndUnit( IUNum ).CCoil = Alphas( 9 ); // name of cooling coil object
			IsNotOK = false;
			IndUnit( IUNum ).CWControlNode = GetCoilWaterInletNode( IndUnit( IUNum ).CCoilType, IndUnit( IUNum ).CCoil, IsNotOK );
			if ( IsNotOK ) {
				ShowContinueError( "In " + CurrentModuleObject + " = " + IndUnit( IUNum ).Name );
				ShowContinueError( "..Only Coil:Cooling:Water or Coil:Cooling:Water:DetailedGeometry is allowed." );
				ErrorsFound = true;
			}
			//      GetOnlySingleNode(Alphas(7),ErrorsFound,'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction',Alphas(1), &
			//                        NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
			IndUnit( IUNum ).MaxVolColdWaterFlow = Numbers( 6 );
			IndUnit( IUNum ).MinVolColdWaterFlow = Numbers( 7 );
			IndUnit( IUNum ).ColdControlOffset = Numbers( 8 );

			// Get the Zone Mixer name and check that it is OK
			errFlag = false;
			IndUnit( IUNum ).MixerName = Alphas( 10 );
			GetZoneMixerIndex( IndUnit( IUNum ).MixerName, IndUnit( IUNum ).Mixer_Num, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "...specified in " + CurrentModuleObject + " = " + IndUnit( IUNum ).Name );
				ErrorsFound = true;
			}

			// Add heating coil to component sets array
			SetUpCompSets( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, IndUnit( IUNum ).HCoilType, IndUnit( IUNum ).HCoil, Alphas( 4 ), "UNDEFINED" );
			// Add cooling coil to component sets array
			SetUpCompSets( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, IndUnit( IUNum ).CCoilType, IndUnit( IUNum ).CCoil, "UNDEFINED", "UNDEFINED" );

			// Register component set data
			TestCompSet( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, NodeID( IndUnit( IUNum ).PriAirInNode ), NodeID( IndUnit( IUNum ).OutAirNode ), "Air Nodes" );

			// Fill the Zone Equipment data with the supply air inlet node number of this unit.
			AirNodeFound = false;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( IndUnit( IUNum ).OutAirNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
							ShowSevereError( "Error in connecting a terminal unit to a zone" );
							ShowContinueError( NodeID( IndUnit( IUNum ).OutAirNode ) + " already connects to another zone" );
							ShowContinueError( "Occurs for terminal unit " + IndUnit( IUNum ).UnitType + " = " + IndUnit( IUNum ).Name );
							ShowContinueError( "Check terminal unit node names for errors" );
							ErrorsFound = true;
						} else {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = IndUnit( IUNum ).PriAirInNode;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = IndUnit( IUNum ).OutAirNode;
						}
						AirNodeFound = true;
						// save the induction ratio in the term unit sizing array for use in the system sizing calculation
						if ( ZoneSizingRunDone ) {
							TermUnitSizing( CtrlZone ).InducRat = IndUnit( IUNum ).InducRatio;
						}
						break;
					}
				}
			}
			if ( ! AirNodeFound ) {
				ShowSevereError( "The outlet air node from the " + CurrentModuleObject + " = " + IndUnit( IUNum ).Name );
				ShowContinueError( "did not have a matching Zone Equipment Inlet Node, Node =" + Alphas( 3 ) );
				ErrorsFound = true;
			}

		}

		for ( IUNum = 1; IUNum <= NumIndUnits; ++IUNum ) {
			for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
				if ( IndUnit( IUNum ).OutAirNode == AirDistUnit( ADUNum ).OutletNodeNum ) {
					//        AirDistUnit(ADUNum)%InletNodeNum = IndUnitIUNum)%InletNodeNum
					IndUnit( IUNum ).ADUNum = ADUNum;
				}
			}
			// one assumes if there isn't one assigned, it's an error?
			if ( IndUnit( IUNum ).ADUNum == 0 ) {
				ShowSevereError( RoutineName + "No matching Air Distribution Unit, for Unit = [" + IndUnit( IUNum ).UnitType + ',' + IndUnit( IUNum ).Name + "]." );
				ShowContinueError( "...should have outlet node=" + NodeID( IndUnit( IUNum ).OutAirNode ) );
				//          ErrorsFound=.TRUE.
			}
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();
		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input. Preceding conditions cause termination." );
		}

	}

	void
	InitIndUnit(
		int const IUNum, // number of the current induction unit being simulated
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 21 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initialization of the passive induction
		// terminal boxes

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataDefineEquip::AirDistUnit;
		using InputProcessor::SameString;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using DataGlobals::AnyPlantInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitIndUnit" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PriNode; // primary air inlet node number
		int SecNode; // secondary air inlet node number
		int OutletNode; // unit air outlet node
		int HotConNode; // hot water control node number
		int ColdConNode; // cold water control node  number
		Real64 IndRat; // unit induction ratio
		Real64 RhoAir; // air density at outside pressure and standard temperature and humidity
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MySizeFlag;
		static Array1D_bool MyPlantScanFlag;

		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop; // Loop checking control variable
		Real64 rho; // local fluid density
		int HWOutletNode; // local node index for hot water coil's outlet node
		int CWOutletNode; // local node index for cold water coil's outlet node
		bool errFlag( false );

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumIndUnits );
			MySizeFlag.allocate( NumIndUnits );
			MyPlantScanFlag.allocate( NumIndUnits );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyPlantScanFlag( IUNum ) && allocated( PlantLoop ) ) {
			if ( IndUnit( IUNum ).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating ) {
				errFlag = false;
				ScanPlantLoopsForObject( IndUnit( IUNum ).HCoil, IndUnit( IUNum ).HCoil_PlantTypeNum, IndUnit( IUNum ).HWLoopNum, IndUnit( IUNum ).HWLoopSide, IndUnit( IUNum ).HWBranchNum, IndUnit( IUNum ).HWCompNum, _, _, _, _, _, errFlag );
			}
			if ( errFlag ) {
				ShowContinueError( "Reference Unit=\"" + IndUnit( IUNum ).Name + "\", type=" + IndUnit( IUNum ).UnitType );
			}
			if ( IndUnit( IUNum ).CCoil_PlantTypeNum == TypeOf_CoilWaterCooling || IndUnit( IUNum ).CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling ) {
				errFlag = false;
				ScanPlantLoopsForObject( IndUnit( IUNum ).CCoil, IndUnit( IUNum ).CCoil_PlantTypeNum, IndUnit( IUNum ).CWLoopNum, IndUnit( IUNum ).CWLoopSide, IndUnit( IUNum ).CWBranchNum, IndUnit( IUNum ).CWCompNum, _, _, _, _, _, errFlag );
			}
			if ( errFlag ) {
				ShowContinueError( "Reference Unit=\"" + IndUnit( IUNum ).Name + "\", type=" + IndUnit( IUNum ).UnitType );
				ShowFatalError( "InitIndUnit: Program terminated for previous conditions." );
			}
			MyPlantScanFlag( IUNum ) = false;
		} else if ( MyPlantScanFlag( IUNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( IUNum ) = false;
		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			// Check to see if there is a Air Distribution Unit on the Zone Equipment List
			for ( Loop = 1; Loop <= NumIndUnits; ++Loop ) {
				if ( IndUnit( Loop ).ADUNum == 0 ) continue;
				if ( CheckZoneEquipmentList( "ZONEHVAC:AIRDISTRIBUTIONUNIT", AirDistUnit( IndUnit( Loop ).ADUNum ).Name ) ) continue;
				ShowSevereError( "InitIndUnit: ADU=[Air Distribution Unit," + AirDistUnit( IndUnit( Loop ).ADUNum ).Name + "] is not on any ZoneHVAC:EquipmentList." );
				ShowContinueError( "...Unit=[" + IndUnit( Loop ).UnitType + ',' + IndUnit( Loop ).Name + "] will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( IUNum ) ) {

			SizeIndUnit( IUNum );
			MySizeFlag( IUNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( IUNum ) ) {
			RhoAir = StdRhoAir;
			PriNode = IndUnit( IUNum ).PriAirInNode;
			SecNode = IndUnit( IUNum ).SecAirInNode;
			OutletNode = IndUnit( IUNum ).OutAirNode;
			IndRat = IndUnit( IUNum ).InducRatio;
			// set the mass flow rates from the input volume flow rates
			if ( SameString( IndUnit( IUNum ).UnitType, "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction" ) ) {
				IndUnit( IUNum ).MaxTotAirMassFlow = RhoAir * IndUnit( IUNum ).MaxTotAirVolFlow;
				IndUnit( IUNum ).MaxPriAirMassFlow = IndUnit( IUNum ).MaxTotAirMassFlow / ( 1.0 + IndRat );
				IndUnit( IUNum ).MaxSecAirMassFlow = IndRat * IndUnit( IUNum ).MaxTotAirMassFlow / ( 1.0 + IndRat );
				Node( PriNode ).MassFlowRateMax = IndUnit( IUNum ).MaxPriAirMassFlow;
				Node( PriNode ).MassFlowRateMin = IndUnit( IUNum ).MaxPriAirMassFlow;
				Node( SecNode ).MassFlowRateMax = IndUnit( IUNum ).MaxSecAirMassFlow;
				Node( SecNode ).MassFlowRateMin = IndUnit( IUNum ).MaxSecAirMassFlow;
				Node( OutletNode ).MassFlowRateMax = IndUnit( IUNum ).MaxTotAirMassFlow;
			}

			HotConNode = IndUnit( IUNum ).HWControlNode;
			if ( HotConNode > 0 && ! MyPlantScanFlag( IUNum ) ) {

				rho = GetDensityGlycol( PlantLoop( IndUnit( IUNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( IndUnit( IUNum ).HWLoopNum ).FluidIndex, RoutineName );
				IndUnit( IUNum ).MaxHotWaterFlow = rho * IndUnit( IUNum ).MaxVolHotWaterFlow;
				IndUnit( IUNum ).MinHotWaterFlow = rho * IndUnit( IUNum ).MinVolHotWaterFlow;
				// get component outlet node from plant structure
				HWOutletNode = PlantLoop( IndUnit( IUNum ).HWLoopNum ).LoopSide( IndUnit( IUNum ).HWLoopSide ).Branch( IndUnit( IUNum ).HWBranchNum ).Comp( IndUnit( IUNum ).HWCompNum ).NodeNumOut;
				InitComponentNodes( IndUnit( IUNum ).MinHotWaterFlow, IndUnit( IUNum ).MaxHotWaterFlow, HotConNode, HWOutletNode, IndUnit( IUNum ).HWLoopNum, IndUnit( IUNum ).HWLoopSide, IndUnit( IUNum ).HWBranchNum, IndUnit( IUNum ).HWCompNum );
			}

			ColdConNode = IndUnit( IUNum ).CWControlNode;
			if ( ColdConNode > 0 ) {
				rho = GetDensityGlycol( PlantLoop( IndUnit( IUNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( IndUnit( IUNum ).CWLoopNum ).FluidIndex, RoutineName );
				IndUnit( IUNum ).MaxColdWaterFlow = rho * IndUnit( IUNum ).MaxVolColdWaterFlow;
				IndUnit( IUNum ).MinColdWaterFlow = rho * IndUnit( IUNum ).MinVolColdWaterFlow;

				CWOutletNode = PlantLoop( IndUnit( IUNum ).CWLoopNum ).LoopSide( IndUnit( IUNum ).CWLoopSide ).Branch( IndUnit( IUNum ).CWBranchNum ).Comp( IndUnit( IUNum ).CWCompNum ).NodeNumOut;
				InitComponentNodes( IndUnit( IUNum ).MinColdWaterFlow, IndUnit( IUNum ).MaxColdWaterFlow, ColdConNode, CWOutletNode, IndUnit( IUNum ).CWLoopNum, IndUnit( IUNum ).CWLoopSide, IndUnit( IUNum ).CWBranchNum, IndUnit( IUNum ).CWCompNum );
			}

			MyEnvrnFlag( IUNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( IUNum ) = true;
		}

		PriNode = IndUnit( IUNum ).PriAirInNode;
		SecNode = IndUnit( IUNum ).SecAirInNode;

		// Do the start of HVAC time step initializations
		if ( FirstHVACIteration ) {
			// check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
			if ( GetCurrentScheduleValue( IndUnit( IUNum ).SchedPtr ) > 0.0 && Node( PriNode ).MassFlowRate > 0.0 ) {
				if ( SameString( IndUnit( IUNum ).UnitType, "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction" ) ) {
					Node( PriNode ).MassFlowRate = IndUnit( IUNum ).MaxPriAirMassFlow;
					Node( SecNode ).MassFlowRate = IndUnit( IUNum ).MaxSecAirMassFlow;
				}
			} else {
				Node( PriNode ).MassFlowRate = 0.0;
				Node( SecNode ).MassFlowRate = 0.0;
			}
			// reset the max and min avail flows
			if ( GetCurrentScheduleValue( IndUnit( IUNum ).SchedPtr ) > 0.0 && Node( PriNode ).MassFlowRateMaxAvail > 0.0 ) {
				if ( SameString( IndUnit( IUNum ).UnitType, "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction" ) ) {
					Node( PriNode ).MassFlowRateMaxAvail = IndUnit( IUNum ).MaxPriAirMassFlow;
					Node( PriNode ).MassFlowRateMinAvail = IndUnit( IUNum ).MaxPriAirMassFlow;
					Node( SecNode ).MassFlowRateMaxAvail = IndUnit( IUNum ).MaxSecAirMassFlow;
					Node( SecNode ).MassFlowRateMinAvail = IndUnit( IUNum ).MaxSecAirMassFlow;
				}
			} else {
				Node( PriNode ).MassFlowRateMaxAvail = 0.0;
				Node( PriNode ).MassFlowRateMinAvail = 0.0;
				Node( SecNode ).MassFlowRateMaxAvail = 0.0;
				Node( SecNode ).MassFlowRateMinAvail = 0.0;
			}
		}

	}

	void
	SizeIndUnit( int const IUNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 22 2004
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing induction terminal units for which flow rates have not been
		// specified in the input

		// METHODOLOGY EMPLOYED:
		// Accesses zone sizing array for air flow rates and zone and plant sizing arrays to
		// calculate coil water flow rates.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		//  USE BranchInputManager,  ONLY: MyPlantSizingIndex
		using ReportSizingManager::ReportSizingOutput;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeIndUnit" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
		Real64 DesCoilLoad;
		Real64 DesPriVolFlow;
		Real64 RhoAir;
		Real64 CpAir;
		static int CoilWaterInletNode( 0 );
		static int CoilWaterOutletNode( 0 );
		bool ErrorsFound;
		Real64 Cp; // local fluid specific heat
		Real64 rho; // local fluid density
		bool IsAutoSize;
		Real64 MaxTotAirVolFlowDes; // Desing size maximum air volume flow for reproting
		Real64 MaxTotAirVolFlowUser; // User hard-sized maximum air volume flow for reporting
		Real64 MaxVolHotWaterFlowDes; // Desing size maximum hot water flow for reproting
		Real64 MaxVolHotWaterFlowUser; // User hard-sized maximum hot water flow for reporting
		Real64 MaxVolColdWaterFlowDes; // Desing size maximum cold water flow for reproting
		Real64 MaxVolColdWaterFlowUser; // User hard-sized maximum cold water flow for reporting

		PltSizHeatNum = 0;
		PltSizCoolNum = 0;
		DesPriVolFlow = 0.0;
		CpAir = 0.0;
		RhoAir = StdRhoAir;
		ErrorsFound = false;
		IsAutoSize = false;
		MaxTotAirVolFlowDes = 0.0;
		MaxTotAirVolFlowUser = 0.0;
		MaxVolHotWaterFlowDes = 0.0;
		MaxVolHotWaterFlowUser = 0.0;
		MaxVolColdWaterFlowDes = 0.0;
		MaxVolColdWaterFlowUser = 0.0;

		if ( IndUnit( IUNum ).MaxTotAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}

		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
				if ( IndUnit( IUNum ).MaxTotAirVolFlow > 0.0 ) {
					ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "User-Specified Maximum Total Air Flow Rate [m3/s]", IndUnit( IUNum ).MaxTotAirVolFlow );
				}
			} else {
				CheckZoneSizing( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name );
				MaxTotAirVolFlowDes = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( MaxTotAirVolFlowDes < SmallAirVolFlow ) {
					MaxTotAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					IndUnit( IUNum ).MaxTotAirVolFlow = MaxTotAirVolFlowDes;
					ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Maximum Total Air Flow Rate [m3/s]", MaxTotAirVolFlowDes );
				} else {
					if ( IndUnit( IUNum ).MaxTotAirVolFlow > 0.0 && MaxTotAirVolFlowDes > 0.0 ) {
						MaxTotAirVolFlowUser = IndUnit( IUNum ).MaxTotAirVolFlow;
						ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Maximum Total Air Flow Rate [m3/s]", MaxTotAirVolFlowDes, "User-Specified Maximum Total Air Flow Rate [m3/s]", MaxTotAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxTotAirVolFlowDes - MaxTotAirVolFlowUser ) / MaxTotAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeHVACSingleDuctInduction: Potential issue with equipment sizing for " + IndUnit( IUNum ).UnitType + " = \"" + IndUnit( IUNum ).Name + "\"." );
								ShowContinueError( "User-Specified Maximum Total Air Flow Rate of " + RoundSigDigits( MaxTotAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Total Air Flow Rate of " + RoundSigDigits( MaxTotAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( IndUnit( IUNum ).MaxVolHotWaterFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
				if ( IndUnit( IUNum ).MaxVolHotWaterFlow > 0.0 ) {
					ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "User-Specified Maximum Hot Water Flow Rate [m3/s]", IndUnit( IUNum ).MaxVolHotWaterFlow );
				}
			} else {
				CheckZoneSizing( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name );

				if ( SameString( IndUnit( IUNum ).HCoilType, "Coil:Heating:Water" ) ) {

					CoilWaterInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", IndUnit( IUNum ).HCoil, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( "Coil:Heating:Water", IndUnit( IUNum ).HCoil, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", IndUnit( IUNum ).HCoil, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {

							if ( TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallAirVolFlow ) {
								DesPriVolFlow = IndUnit( IUNum ).MaxTotAirVolFlow / ( 1.0 + IndUnit( IUNum ).InducRatio );
								CpAir = PsyCpAirFnWTdb( TermUnitFinalZoneSizing( CurZoneEqNum ).HeatDesHumRat, TermUnitFinalZoneSizing( CurZoneEqNum ).HeatDesTemp );
								// the design heating coil load is the zone load minus whatever the central system does. Note that
								// DesHeatCoilInTempTU is really the primary air inlet temperature for the unit.
								if ( TermUnitFinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak > 0.0 ) {
									DesCoilLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor - CpAir * RhoAir * DesPriVolFlow * ( TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU - TermUnitFinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak );
								} else {
									DesCoilLoad = CpAir * RhoAir * DesPriVolFlow * ( ZoneSizThermSetPtLo( CurZoneEqNum ) - TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU );
								}
								IndUnit( IUNum ).DesHeatingLoad = DesCoilLoad;
								Cp = GetSpecificHeatGlycol( PlantLoop( IndUnit( IUNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( IndUnit( IUNum ).HWLoopNum ).FluidIndex, RoutineName );

								rho = GetDensityGlycol( PlantLoop( IndUnit( IUNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( IndUnit( IUNum ).HWLoopNum ).FluidIndex, RoutineName );

								MaxVolHotWaterFlowDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
								MaxVolHotWaterFlowDes = max( MaxVolHotWaterFlowDes, 0.0 );
							} else {
								MaxVolHotWaterFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in" + IndUnit( IUNum ).UnitType + " Object=" + IndUnit( IUNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						IndUnit( IUNum ).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
						ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Maximum Hot Water Flow Rate [m3/s]", MaxVolHotWaterFlowDes );
						ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Inlet Air Temperature [C]", TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU );
						ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Inlet Air Humidity Ratio [kgWater/kgDryAir]", TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU );
					} else {
						if ( IndUnit( IUNum ).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0 ) {
							MaxVolHotWaterFlowUser = IndUnit( IUNum ).MaxVolHotWaterFlow;
							ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Maximum Hot Water Flow Rate [m3/s]", MaxVolHotWaterFlowDes, "User-Specified Maximum Hot Water Flow Rate [m3/s]", MaxVolHotWaterFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser ) / MaxVolHotWaterFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeHVACSingleDuctInduction: Potential issue with equipment sizing for " + IndUnit( IUNum ).UnitType + " = \"" + IndUnit( IUNum ).Name + "\"." );
									ShowContinueError( "User-Specified Maximum Hot Water Flow Rate of " + RoundSigDigits( MaxVolHotWaterFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Hot Water Flow Rate of " + RoundSigDigits( MaxVolHotWaterFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				} else {
					IndUnit( IUNum ).MaxVolHotWaterFlow = 0.0;
				}
			}
		}

		IsAutoSize = false;
		if ( IndUnit( IUNum ).MaxVolColdWaterFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
				if ( IndUnit( IUNum ).MaxVolColdWaterFlow > 0.0 ) {
					ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "User-Specified Maximum Cold Water Flow Rate [m3/s]", IndUnit( IUNum ).MaxVolColdWaterFlow );
				}
			} else {
				CheckZoneSizing( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name );

				if ( SameString( IndUnit( IUNum ).CCoilType, "Coil:Cooling:Water" ) || SameString( IndUnit( IUNum ).CCoilType, "Coil:Cooling:Water:DetailedGeometry" ) ) {

					CoilWaterInletNode = GetCoilWaterInletNode( IndUnit( IUNum ).CCoilType, IndUnit( IUNum ).CCoil, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( IndUnit( IUNum ).CCoilType, IndUnit( IUNum ).CCoil, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizCoolNum = MyPlantSizingIndex( IndUnit( IUNum ).CCoilType, IndUnit( IUNum ).CCoil, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						if ( PltSizCoolNum > 0 ) {

							if ( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow >= SmallAirVolFlow ) {
								DesPriVolFlow = IndUnit( IUNum ).MaxTotAirVolFlow / ( 1.0 + IndUnit( IUNum ).InducRatio );
								CpAir = PsyCpAirFnWTdb( TermUnitFinalZoneSizing( CurZoneEqNum ).CoolDesHumRat, TermUnitFinalZoneSizing( CurZoneEqNum ).CoolDesTemp );
								// the design cooling coil load is the zone load minus whatever the central system does. Note that
								// DesCoolCoilInTempTU is really the primary air inlet temperature for the unit.
								if ( TermUnitFinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak > 0.0 ) {
									DesCoilLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor - CpAir * RhoAir * DesPriVolFlow * ( TermUnitFinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak - TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU );
								} else {
									DesCoilLoad = CpAir * RhoAir * DesPriVolFlow * ( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU - ZoneSizThermSetPtHi( CurZoneEqNum ) );
								}
								IndUnit( IUNum ).DesCoolingLoad = DesCoilLoad;
								Cp = GetSpecificHeatGlycol( PlantLoop( IndUnit( IUNum ).CWLoopNum ).FluidName, 5.0, PlantLoop( IndUnit( IUNum ).CWLoopNum ).FluidIndex, RoutineName );

								rho = GetDensityGlycol( PlantLoop( IndUnit( IUNum ).CWLoopNum ).FluidName, 5.0, PlantLoop( IndUnit( IUNum ).CWLoopNum ).FluidIndex, RoutineName );

								MaxVolColdWaterFlowDes = DesCoilLoad / ( PlantSizData( PltSizCoolNum ).DeltaT * Cp * rho );
								MaxVolColdWaterFlowDes = max( MaxVolColdWaterFlowDes, 0.0 );
							} else {
								MaxVolColdWaterFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
							ShowContinueError( "Occurs in" + IndUnit( IUNum ).UnitType + " Object=" + IndUnit( IUNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						IndUnit( IUNum ).MaxVolColdWaterFlow = MaxVolColdWaterFlowDes;
						ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Maximum Cold Water Flow Rate [m3/s]", MaxVolColdWaterFlowDes );
					} else {
						if ( IndUnit( IUNum ).MaxVolColdWaterFlow > 0.0 && MaxVolColdWaterFlowDes > 0.0 ) {
							MaxVolColdWaterFlowUser = IndUnit( IUNum ).MaxVolColdWaterFlow;
							ReportSizingOutput( IndUnit( IUNum ).UnitType, IndUnit( IUNum ).Name, "Design Size Maximum Cold Water Flow Rate [m3/s]", MaxVolColdWaterFlowDes, "User-Specified Maximum Cold Water Flow Rate [m3/s]", MaxVolColdWaterFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolColdWaterFlowDes - MaxVolColdWaterFlowUser ) / MaxVolColdWaterFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeHVACSingleDuctInduction: Potential issue with equipment sizing for " + IndUnit( IUNum ).UnitType + " = \"" + IndUnit( IUNum ).Name + "\"." );
									ShowContinueError( "User-Specified Maximum Cold Water Flow Rate of " + RoundSigDigits( MaxVolColdWaterFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Cold Water Flow Rate of " + RoundSigDigits( MaxVolColdWaterFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				} else {
					IndUnit( IUNum ).MaxVolColdWaterFlow = 0.0;
				}
			}
		}

		if ( CurZoneEqNum > 0 ) {
			// note we save the induced air flow for use by the hw and cw coil sizing routines
			TermUnitSizing( CurZoneEqNum ).AirVolFlow = IndUnit( IUNum ).MaxTotAirVolFlow * IndUnit( IUNum ).InducRatio / ( 1.0 + IndUnit( IUNum ).InducRatio );
			// save the max hot and cold water flows for use in coil sizing
			TermUnitSizing( CurZoneEqNum ).MaxHWVolFlow = IndUnit( IUNum ).MaxVolHotWaterFlow;
			TermUnitSizing( CurZoneEqNum ).MaxCWVolFlow = IndUnit( IUNum ).MaxVolColdWaterFlow;
			// save the design load used for reporting
			TermUnitSizing( CurZoneEqNum ).DesCoolingLoad = IndUnit( IUNum ).DesCoolingLoad;
			TermUnitSizing( CurZoneEqNum ).DesHeatingLoad = IndUnit( IUNum ).DesHeatingLoad;
			// save the induction ratio for use in subsequent sizing calcs
			TermUnitSizing( CurZoneEqNum ).InducRat = IndUnit( IUNum ).InducRatio;
			if ( SameString( IndUnit( IUNum ).HCoilType, "Coil:Heating:Water" ) ) {
				SetCoilDesFlow( IndUnit( IUNum ).HCoilType, IndUnit( IUNum ).HCoil, TermUnitSizing( CurZoneEqNum ).AirVolFlow, ErrorsFound );
			}
			if ( SameString( IndUnit( IUNum ).CCoilType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
				SetCoilDesFlow( IndUnit( IUNum ).CCoilType, IndUnit( IUNum ).CCoil, TermUnitSizing( CurZoneEqNum ).AirVolFlow, ErrorsFound );
			}
		}

	}

	void
	SimFourPipeIndUnit(
		int const IUNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 23 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a 4 pipe induction unit; adjust its heating or cooling
		// coil outputs to match the zone load.

		// METHODOLOGY EMPLOYED:
		// (1) From the zone load and the primary air inlet conditions calculate the coil load
		//     in the secondary air stream
		// (2) If there is a cooling coil load, set the heating coil off and control the cooling
		//     coil to meet the coil load
		// (3) If there is a heating coil load, control the heating coil to meet the load and keep
		//     the cooling coil off.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DataPlant::PlantLoop;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const SolveMaxIter( 50 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QZnReq; // heating or cooling needed by zone [Watts]
		Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
		Real64 QToCoolSetPt; // [W]  remaining load to cooling setpoint
		Real64 PowerMet; // power supplied
		bool UnitOn; // TRUE if unit is on
		Real64 MaxHotWaterFlow; // maximum water flow for heating [kg/s]
		Real64 MinHotWaterFlow; // minimum water flow for heating [kg/s]
		Real64 MaxColdWaterFlow; // maximum water flow for cooling [kg/s]
		Real64 MinColdWaterFlow; // minimum water flow for cooling [kg/s]
		Real64 HWFlow; // hot water flow [kg/s]
		Real64 CWFlow; // cold water flow [kg/s]
		int PriNode; // unit primary air inlet node
		int SecNode; // unit secondary air inlet node
		int OutletNode; // unit air outlet node
		int HotControlNode; // hot water coil inlet node
		int ColdControlNode; // cold water coil inlet node
		Real64 QPriOnly; // unit output with no zone coils active
		Real64 PriAirMassFlow; // primary air mass flow rate [kg/s]
		Real64 SecAirMassFlow; // secondary air mass flow rate [kg/s]
		Real64 InducRat; // Induction Ratio
		Array1D< Real64 > Par( 7 );
		int SolFlag;
		Real64 ErrTolerance;
		int HWOutletNode;
		int CWOutletNode;

		UnitOn = true;
		PowerMet = 0.0;
		InducRat = IndUnit( IUNum ).InducRatio;
		PriNode = IndUnit( IUNum ).PriAirInNode;
		SecNode = IndUnit( IUNum ).SecAirInNode;
		OutletNode = IndUnit( IUNum ).OutAirNode;
		HotControlNode = IndUnit( IUNum ).HWControlNode;
		HWOutletNode = PlantLoop( IndUnit( IUNum ).HWLoopNum ).LoopSide( IndUnit( IUNum ).HWLoopSide ).Branch( IndUnit( IUNum ).HWBranchNum ).Comp( IndUnit( IUNum ).HWCompNum ).NodeNumOut;
		ColdControlNode = IndUnit( IUNum ).CWControlNode;
		CWOutletNode = PlantLoop( IndUnit( IUNum ).CWLoopNum ).LoopSide( IndUnit( IUNum ).CWLoopSide ).Branch( IndUnit( IUNum ).CWBranchNum ).Comp( IndUnit( IUNum ).CWCompNum ).NodeNumOut;
		PriAirMassFlow = Node( PriNode ).MassFlowRateMaxAvail;
		SecAirMassFlow = InducRat * PriAirMassFlow;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		QToCoolSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		//On the first HVAC iteration the system values are given to the controller, but after that
		// the demand limits are in place and there needs to be feedback to the Zone Equipment

		MaxHotWaterFlow = IndUnit( IUNum ).MaxHotWaterFlow;
		SetComponentFlowRate( MaxHotWaterFlow, HotControlNode, HWOutletNode, IndUnit( IUNum ).HWLoopNum, IndUnit( IUNum ).HWLoopSide, IndUnit( IUNum ).HWBranchNum, IndUnit( IUNum ).HWCompNum );

		MinHotWaterFlow = IndUnit( IUNum ).MinHotWaterFlow;
		SetComponentFlowRate( MinHotWaterFlow, HotControlNode, HWOutletNode, IndUnit( IUNum ).HWLoopNum, IndUnit( IUNum ).HWLoopSide, IndUnit( IUNum ).HWBranchNum, IndUnit( IUNum ).HWCompNum );

		MaxColdWaterFlow = IndUnit( IUNum ).MaxColdWaterFlow;
		SetComponentFlowRate( MaxColdWaterFlow, ColdControlNode, CWOutletNode, IndUnit( IUNum ).CWLoopNum, IndUnit( IUNum ).CWLoopSide, IndUnit( IUNum ).CWBranchNum, IndUnit( IUNum ).CWCompNum );

		MinColdWaterFlow = IndUnit( IUNum ).MinColdWaterFlow;
		SetComponentFlowRate( MinColdWaterFlow, ColdControlNode, CWOutletNode, IndUnit( IUNum ).CWLoopNum, IndUnit( IUNum ).CWLoopSide, IndUnit( IUNum ).CWBranchNum, IndUnit( IUNum ).CWCompNum );

		if ( GetCurrentScheduleValue( IndUnit( IUNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( PriAirMassFlow <= SmallMassFlow ) UnitOn = false;

		// Set the unit's air inlet nodes mass flow rates
		Node( PriNode ).MassFlowRate = PriAirMassFlow;
		Node( SecNode ).MassFlowRate = SecAirMassFlow;
		// initialize the water inlet nodes to minimum
		// fire the unit at min water flow
		CalcFourPipeIndUnit( IUNum, FirstHVACIteration, ZoneNodeNum, MinHotWaterFlow, MinColdWaterFlow, QPriOnly );
		// the load to be met by the secondary air stream coils is QZnReq-PowerMet

		if ( UnitOn ) {

			if ( QToHeatSetPt - QPriOnly > SmallLoad ) {
				// heating coil
				// check that it can meet the load
				CalcFourPipeIndUnit( IUNum, FirstHVACIteration, ZoneNodeNum, MaxHotWaterFlow, MinColdWaterFlow, PowerMet );
				if ( PowerMet > QToHeatSetPt + SmallLoad ) {
					Par( 1 ) = double( IUNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = double( ZoneNodeNum );
					Par( 4 ) = MinColdWaterFlow;
					Par( 5 ) = QToHeatSetPt;
					Par( 6 ) = QPriOnly;
					Par( 7 ) = PowerMet;
					ErrTolerance = IndUnit( IUNum ).HotControlOffset;
					SolveRegulaFalsi( ErrTolerance, SolveMaxIter, SolFlag, HWFlow, FourPipeIUHeatingResidual, MinHotWaterFlow, MaxHotWaterFlow, Par );
					if ( SolFlag == -1 ) {
						if ( IndUnit( IUNum ).HWCoilFailNum1 == 0 ) {
							ShowWarningMessage( "SimFourPipeIndUnit: Hot water coil control failed for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "  Iteration limit [" + RoundSigDigits( SolveMaxIter ) + "] exceeded in calculating hot water mass flow rate" );
						}
						ShowRecurringWarningErrorAtEnd( "SimFourPipeIndUnit: Hot water coil control failed (iteration limit [" + RoundSigDigits( SolveMaxIter ) + "]) for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name + "\"", IndUnit( IUNum ).HWCoilFailNum1 );
					} else if ( SolFlag == -2 ) {
						if ( IndUnit( IUNum ).HWCoilFailNum2 == 0 ) {
							ShowWarningMessage( "SimFourPipeIndUnit: Hot water coil control failed (maximum flow limits) for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "...Bad hot water maximum flow rate limits" );
							ShowContinueError( "...Given minimum water flow rate=" + RoundSigDigits( MinHotWaterFlow, 3 ) + " kg/s" );
							ShowContinueError( "...Given maximum water flow rate=" + RoundSigDigits( MaxHotWaterFlow, 3 ) + " kg/s" );
						}
						ShowRecurringWarningErrorAtEnd( "SimFourPipeIndUnit: Hot water coil control failed (flow limits) for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name + "\"", IndUnit( IUNum ).HWCoilFailNum2, MaxHotWaterFlow, MinHotWaterFlow, _, "[kg/s]", "[kg/s]" );
					}
				}
			} else if ( QToCoolSetPt - QPriOnly < -SmallLoad ) {
				// cooling coil
				// check that it can meet the load
				CalcFourPipeIndUnit( IUNum, FirstHVACIteration, ZoneNodeNum, MinHotWaterFlow, MaxColdWaterFlow, PowerMet );
				if ( PowerMet < QToCoolSetPt - SmallLoad ) {
					Par( 1 ) = double( IUNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = double( ZoneNodeNum );
					Par( 4 ) = MinHotWaterFlow;
					Par( 5 ) = QToCoolSetPt;
					Par( 6 ) = QPriOnly;
					Par( 7 ) = PowerMet;
					ErrTolerance = IndUnit( IUNum ).ColdControlOffset;
					SolveRegulaFalsi( ErrTolerance, SolveMaxIter, SolFlag, CWFlow, FourPipeIUCoolingResidual, MinColdWaterFlow, MaxColdWaterFlow, Par );
					if ( SolFlag == -1 ) {
						if ( IndUnit( IUNum ).CWCoilFailNum1 == 0 ) {
							ShowWarningMessage( "SimFourPipeIndUnit: Cold water coil control failed for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "  Iteration limit [" + RoundSigDigits( SolveMaxIter ) + "] exceeded in calculating cold water mass flow rate" );
						}
						ShowRecurringWarningErrorAtEnd( "SimFourPipeIndUnit: Cold water coil control failed (iteration limit [" + RoundSigDigits( SolveMaxIter ) + "]) for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name, IndUnit( IUNum ).CWCoilFailNum1 );
					} else if ( SolFlag == -2 ) {
						if ( IndUnit( IUNum ).CWCoilFailNum2 == 0 ) {
							ShowWarningMessage( "SimFourPipeIndUnit: Cold water coil control failed (maximum flow limits) for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "...Bad cold water maximum flow rate limits" );
							ShowContinueError( "...Given minimum water flow rate=" + RoundSigDigits( MinColdWaterFlow, 3 ) + " kg/s" );
							ShowContinueError( "...Given maximum water flow rate=" + RoundSigDigits( MaxColdWaterFlow, 3 ) + " kg/s" );
						}
						ShowRecurringWarningErrorAtEnd( "SimFourPipeIndUnit: Cold water coil control failed (flow limits) for " + IndUnit( IUNum ).UnitType + "=\"" + IndUnit( IUNum ).Name + "\"", IndUnit( IUNum ).CWCoilFailNum2, MaxColdWaterFlow, MinColdWaterFlow, _, "[kg/s]", "[kg/s]" );
					}
				}
			} else {
				CalcFourPipeIndUnit( IUNum, FirstHVACIteration, ZoneNodeNum, MinHotWaterFlow, MinColdWaterFlow, PowerMet );
			}

		} else {
			// unit off
			CalcFourPipeIndUnit( IUNum, FirstHVACIteration, ZoneNodeNum, MinHotWaterFlow, MinColdWaterFlow, PowerMet );
		}
		Node( OutletNode ).MassFlowRateMax = IndUnit( IUNum ).MaxTotAirMassFlow;

		// At this point we are done. There is no output to report or pass back up: the output provided is calculated
		// one level up in the calling routine SimZoneAirLoopEquipment. All the inlet and outlet flow rates and
		// conditions have been set by CalcFourPipeIndUnit either explicitly or as a result of the simple component calls.

	}

	void
	CalcFourPipeIndUnit(
		int const IUNum, // Unit index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const ZoneNode, // zone node number
		Real64 const HWFlow, // hot water flow (kg/s)
		Real64 const CWFlow, // cold water flow (kg/s)
		Real64 & LoadMet // load met by unit (watts)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate the components making up the 4 pipe induction unit.

		// METHODOLOGY EMPLOYED:
		// Simulates the unit components sequentially in the air flow direction.

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixerComponent::SimAirMixer;
		using WaterCoils::SimulateWaterCoilComponents;
		using PlantUtilities::SetComponentFlowRate;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode; // unit air outlet node
		int PriNode; // unit primary air inlet node
		int HotControlNode; // the hot water inlet node
		int ColdControlNode; // the cold water inlet node
		Real64 PriAirMassFlow; // primary air mass flow rate [kg/s]
		Real64 SecAirMassFlow; // secondary air mass flow rate [kg/s]
		Real64 TotAirMassFlow; // total air mass flow rate [kg/s]
		Real64 InducRat; // induction ratio
		Real64 CpAirZn; // zone air specific heat [J/kg-C]
		Real64 mdotHW; // local temporary hot water flow rate [kg/s]
		Real64 mdotCW; // local temporary cold water flow rate [kg/s]
		int HWOutletNode;
		int CWOutletNode;

		// FLOW

		PriNode = IndUnit( IUNum ).PriAirInNode;
		OutletNode = IndUnit( IUNum ).OutAirNode;
		PriAirMassFlow = Node( PriNode ).MassFlowRateMaxAvail;
		InducRat = IndUnit( IUNum ).InducRatio;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, Node( ZoneNode ).Temp );
		SecAirMassFlow = InducRat * PriAirMassFlow;
		TotAirMassFlow = PriAirMassFlow + SecAirMassFlow;
		HotControlNode = IndUnit( IUNum ).HWControlNode;
		HWOutletNode = PlantLoop( IndUnit( IUNum ).HWLoopNum ).LoopSide( IndUnit( IUNum ).HWLoopSide ).Branch( IndUnit( IUNum ).HWBranchNum ).Comp( IndUnit( IUNum ).HWCompNum ).NodeNumOut;

		ColdControlNode = IndUnit( IUNum ).CWControlNode;
		CWOutletNode = PlantLoop( IndUnit( IUNum ).CWLoopNum ).LoopSide( IndUnit( IUNum ).CWLoopSide ).Branch( IndUnit( IUNum ).CWBranchNum ).Comp( IndUnit( IUNum ).CWCompNum ).NodeNumOut;

		mdotHW = HWFlow;
		SetComponentFlowRate( mdotHW, HotControlNode, HWOutletNode, IndUnit( IUNum ).HWLoopNum, IndUnit( IUNum ).HWLoopSide, IndUnit( IUNum ).HWBranchNum, IndUnit( IUNum ).HWCompNum );

		//  Node(HotControlNode)%MassFlowRate = HWFlow

		mdotCW = CWFlow;
		SetComponentFlowRate( mdotCW, ColdControlNode, CWOutletNode, IndUnit( IUNum ).CWLoopNum, IndUnit( IUNum ).CWLoopSide, IndUnit( IUNum ).CWBranchNum, IndUnit( IUNum ).CWCompNum );
		//  Node(ColdControlNode)%MassFlowRate = CWFlow

		SimulateWaterCoilComponents( IndUnit( IUNum ).HCoil, FirstHVACIteration, IndUnit( IUNum ).HCoil_Num );
		SimulateWaterCoilComponents( IndUnit( IUNum ).CCoil, FirstHVACIteration, IndUnit( IUNum ).CCoil_Num );
		SimAirMixer( IndUnit( IUNum ).MixerName, IndUnit( IUNum ).Mixer_Num );
		LoadMet = TotAirMassFlow * CpAirZn * ( Node( OutletNode ).Temp - Node( ZoneNode ).Temp );

	}

	Real64
	FourPipeIUHeatingResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested zone load
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Zone Load - Unit Output) / Requested Coil Load
		// Unit Output depends on the hot water flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcFourPipeIndUnit, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

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
		int IUIndex;
		bool FirstHVACSoln;
		int ZoneNodeIndex;
		Real64 MinCWFlow;
		Real64 UnitOutput;

		IUIndex = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		ZoneNodeIndex = int( Par( 3 ) );
		MinCWFlow = Par( 4 );
		CalcFourPipeIndUnit( IUIndex, FirstHVACSoln, ZoneNodeIndex, HWFlow, MinCWFlow, UnitOutput );
		Residuum = ( Par( 5 ) - UnitOutput ) / ( Par( 7 ) - Par( 6 ) );

		return Residuum;
	}

	Real64
	FourPipeIUCoolingResidual(
		Real64 const CWFlow, // cold water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested zone load
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Zone Load - Unit Output) / Requested Coil Load
		// Unit Output depends on the cold water flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcFourPipeIndUnit, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

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
		int IUIndex;
		bool FirstHVACSoln;
		int ZoneNodeIndex;
		Real64 MinHWFlow;
		Real64 UnitOutput;

		IUIndex = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		ZoneNodeIndex = int( Par( 3 ) );
		MinHWFlow = Par( 4 );
		CalcFourPipeIndUnit( IUIndex, FirstHVACSoln, ZoneNodeIndex, MinHWFlow, CWFlow, UnitOutput );
		Residuum = ( Par( 5 ) - UnitOutput ) / ( Par( 7 ) - Par( 6 ) );

		return Residuum;
	}

	// ========================= Utilities =======================

	bool
	FourPipeInductionUnitHasMixer( std::string const & CompName ) // component (mixer) name
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

		if ( GetIUInputFlag ) {
			GetIndUnits();
			GetIUInputFlag = false;
		}

		YesNo = false;
		if ( NumIndUnits > 0 ) {
			ItemNum = FindItemInList( CompName, IndUnit, &IndUnitData::MixerName );
			if ( ItemNum > 0 ) YesNo = true;
		}

		return YesNo;

	}

} // HVACSingleDuctInduc

} // EnergyPlus
