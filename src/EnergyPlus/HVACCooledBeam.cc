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
#include <HVACCooledBeam.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
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
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace HVACCooledBeam {

	// Module containing routines dealing with cooled beam units

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   February 2, 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms needed to simulate cooled beam units

	// METHODOLOGY EMPLOYED:
	// Cooled beam units are treated as terminal units. There is a fixed amount of supply air delivered
	// either directly through a diffuser or through the cooled beam units. Thermodynamically the
	// situation is similar to 4 pipe induction terminal units. The detailed methodology follows the
	// method in DOE-2.1E.

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
	using DataGlobals::Pi;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
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
	using DataHVACGlobals::TimeStepSys;
	using DataHVACGlobals::SmallWaterVolFlow;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const Passive_Cooled_Beam( 1 );
	int const Active_Cooled_Beam( 2 );
	Real64 const NomMassFlowPerBeam( 0.07 ); // nominal water mass flow rate per beam [kg/s]
	Real64 const MinWaterVel( 0.2 ); // minimum water velocity [m/s]
	Real64 const Coeff2( 10000.0 );
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	Array1D_bool CheckEquipName;

	// INTEGER :: NumPassiveCB = 0
	// INTEGER :: NumActiveCB = 0
	int NumCB( 0 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACCooledBeam:

	// Object Data
	Array1D< CoolBeamData > CoolBeam;

	// Functions

	void
	SimCoolBeam(
		std::string const & CompName, // name of the cooled beam unit
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the unit
		int const ZoneNodeNum, // zone node number of zone served by the unit
		int & CompIndex, // which cooled beam unit in data structure
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 3, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a cooled beam unit.
		// Called from SimZoneAirLoopEquipment in module ZoneAirLoopEquipmentManager.

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
		int CBNum; // index of cooled beam unit being simulated
		static bool GetInputFlag( true ); // First time, input is "gotten"

		// First time SimIndUnit is called, get the input for all the cooled beam units
		if ( GetInputFlag ) {
			GetCoolBeams();
			GetInputFlag = false;
		}

		// Get the  unit index
		if ( CompIndex == 0 ) {
			CBNum = FindItemInList( CompName, CoolBeam );
			if ( CBNum == 0 ) {
				ShowFatalError( "SimCoolBeam: Cool Beam Unit not found=" + CompName );
			}
			CompIndex = CBNum;
		} else {
			CBNum = CompIndex;
			if ( CBNum > NumCB || CBNum < 1 ) {
				ShowFatalError( "SimCoolBeam: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Number of Cool Beam Units=" + TrimSigDigits( NumCB ) + ", System name=" + CompName );
			}
			if ( CheckEquipName( CBNum ) ) {
				if ( CompName != CoolBeam( CBNum ).Name ) {
					ShowFatalError( "SimCoolBeam: Invalid CompIndex passed=" + TrimSigDigits( CompIndex ) + ", Cool Beam Unit name=" + CompName + ", stored Cool Beam Unit for that index=" + CoolBeam( CBNum ).Name );
				}
				CheckEquipName( CBNum ) = false;
			}
		}
		if ( CBNum == 0 ) {
			ShowFatalError( "Cool Beam Unit not found = " + CompName );
		}

		// initialize the unit
		InitCoolBeam( CBNum, FirstHVACIteration );

		ControlCoolBeam( CBNum, ZoneNum, ZoneNodeNum, FirstHVACIteration, NonAirSysOutput );

		// Update the current unit's outlet nodes. No update needed
		UpdateCoolBeam( CBNum );

		// Fill the report variables. There are no report variables
		ReportCoolBeam( CBNum );

	}

	void
	GetCoolBeams()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 3, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for cool beam units and stores it in the
		// cool beam unit data structures

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

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetCoolBeams " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		int CBIndex; // loop index
		int CBNum; // current fan coil number
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

		// find the number of cooled beam units
		CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:CooledBeam";
		NumCB = GetNumObjectsFound( CurrentModuleObject );
		// allocate the data structures
		CoolBeam.allocate( NumCB );
		CheckEquipName.dimension( NumCB, true );

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		NumAlphas = 7;
		NumNumbers = 16;
		TotalArgs = 23;

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// loop over cooled beam units; get and load the input data
		for ( CBIndex = 1; CBIndex <= NumCB; ++CBIndex ) {

			GetObjectItem( CurrentModuleObject, CBIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			CBNum = CBIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), CoolBeam, CBNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			CoolBeam( CBNum ).Name = Alphas( 1 );
			CoolBeam( CBNum ).UnitType = CurrentModuleObject;
			CoolBeam( CBNum ).UnitType_Num = 1;
			CoolBeam( CBNum ).CBType = Alphas( 3 );
			if ( SameString( CoolBeam( CBNum ).CBType, "Passive" ) ) {
				CoolBeam( CBNum ).CBType_Num = Passive_Cooled_Beam;
			} else if ( SameString( CoolBeam( CBNum ).CBType, "Active" ) ) {
				CoolBeam( CBNum ).CBType_Num = Active_Cooled_Beam;
			} else {
				ShowSevereError( "Illegal " + cAlphaFields( 3 ) + " = " + CoolBeam( CBNum ).CBType + '.' );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CoolBeam( CBNum ).Name );
				ErrorsFound = true;
			}
			CoolBeam( CBNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				CoolBeam( CBNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				CoolBeam( CBNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( CoolBeam( CBNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			CoolBeam( CBNum ).AirInNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFields( 4 ) );
			CoolBeam( CBNum ).AirOutNode = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFields( 5 ) );
			CoolBeam( CBNum ).CWInNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent, cAlphaFields( 6 ) );
			CoolBeam( CBNum ).CWOutNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent, cAlphaFields( 7 ) );
			CoolBeam( CBNum ).MaxAirVolFlow = Numbers( 1 );
			CoolBeam( CBNum ).MaxCoolWaterVolFlow = Numbers( 2 );
			CoolBeam( CBNum ).NumBeams = Numbers( 3 );
			CoolBeam( CBNum ).BeamLength = Numbers( 4 );
			CoolBeam( CBNum ).DesInletWaterTemp = Numbers( 5 );
			CoolBeam( CBNum ).DesOutletWaterTemp = Numbers( 6 );
			CoolBeam( CBNum ).CoilArea = Numbers( 7 );
			CoolBeam( CBNum ).a = Numbers( 8 );
			CoolBeam( CBNum ).n1 = Numbers( 9 );
			CoolBeam( CBNum ).n2 = Numbers( 10 );
			CoolBeam( CBNum ).n3 = Numbers( 11 );
			CoolBeam( CBNum ).a0 = Numbers( 12 );
			CoolBeam( CBNum ).K1 = Numbers( 13 );
			CoolBeam( CBNum ).n = Numbers( 14 );
			CoolBeam( CBNum ).Kin = Numbers( 15 );
			CoolBeam( CBNum ).InDiam = Numbers( 16 );

			// Register component set data
			TestCompSet( CurrentModuleObject, CoolBeam( CBNum ).Name, NodeID( CoolBeam( CBNum ).AirInNode ), NodeID( CoolBeam( CBNum ).AirOutNode ), "Air Nodes" );
			TestCompSet( CurrentModuleObject, CoolBeam( CBNum ).Name, NodeID( CoolBeam( CBNum ).CWInNode ), NodeID( CoolBeam( CBNum ).CWOutNode ), "Water Nodes" );

			//Setup the Cooled Beam reporting variables
			//CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:CooledBeam"
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Cooling Energy [J]", CoolBeam( CBNum ).BeamCoolingEnergy, "System", "Sum", CoolBeam( CBNum ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Air Terminal Beam Chilled Water Energy [J]", CoolBeam( CBNum ).BeamCoolingEnergy, "System", "Sum", CoolBeam( CBNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Cooling Rate [W]", CoolBeam( CBNum ).BeamCoolingRate, "System", "Average", CoolBeam( CBNum ).Name );
			SetupOutputVariable( "Zone Air Terminal Supply Air Sensible Cooling Energy [J]", CoolBeam( CBNum ).SupAirCoolingEnergy, "System", "Sum", CoolBeam( CBNum ).Name );
			SetupOutputVariable( "Zone Air Terminal Supply Air Sensible Cooling Rate [W]", CoolBeam( CBNum ).SupAirCoolingRate, "System", "Average", CoolBeam( CBNum ).Name );
			SetupOutputVariable( "Zone Air Terminal Supply Air Sensible Heating Energy [J]", CoolBeam( CBNum ).SupAirHeatingEnergy, "System", "Sum", CoolBeam( CBNum ).Name );
			SetupOutputVariable( "Zone Air Terminal Supply Air Sensible Heating Rate [W]", CoolBeam( CBNum ).SupAirHeatingRate, "System", "Average", CoolBeam( CBNum ).Name );

			// Fill the Zone Equipment data with the supply air inlet node number of this unit.
			AirNodeFound = false;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
					if ( CoolBeam( CBNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
						ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = CoolBeam( CBNum ).AirInNode;
						ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = CoolBeam( CBNum ).AirOutNode;
						AirNodeFound = true;
						break;
					}
				}
			}
			if ( ! AirNodeFound ) {
				ShowSevereError( "The outlet air node from the " + CurrentModuleObject + " = " + CoolBeam( CBNum ).Name );
				ShowContinueError( "did not have a matching Zone Equipment Inlet Node, Node =" + Alphas( 5 ) );
				ErrorsFound = true;
			}

		}

		for ( CBNum = 1; CBNum <= NumCB; ++CBNum ) {
			for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
				if ( CoolBeam( CBNum ).AirOutNode == AirDistUnit( ADUNum ).OutletNodeNum ) {
					CoolBeam( CBNum ).ADUNum = ADUNum;
				}
			}
			// one assumes if there isn't one assigned, it's an error?
			if ( CoolBeam( CBNum ).ADUNum == 0 ) {
				ShowSevereError( RoutineName + "No matching Air Distribution Unit, for Unit = [" + CurrentModuleObject + ',' + CoolBeam( CBNum ).Name + "]." );
				ShowContinueError( "...should have outlet node=" + NodeID( CoolBeam( CBNum ).AirOutNode ) );
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
	InitCoolBeam(
		int const CBNum, // number of the current cooled beam unit being simulated
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 6, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initialization of the cooled beam units

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
		using DataPlant::TypeOf_CooledBeamAirTerminal;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitCoolBeam" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InAirNode; // supply air inlet node number
		int OutAirNode; // unit air outlet node
		int InWaterNode; // unit inlet chilled water node
		int OutWaterNode; // unit outlet chilled water node
		Real64 RhoAir; // air density at outside pressure and standard temperature and humidity
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MySizeFlag;
		static Array1D_bool PlantLoopScanFlag;
		Real64 rho; // local fluid density
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop; // Loop checking control variable
		std::string CurrentModuleObject;
		bool errFlag;

		CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:CooledBeam";
		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumCB );
			MySizeFlag.allocate( NumCB );
			PlantLoopScanFlag.allocate( NumCB );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			PlantLoopScanFlag = true;
			MyOneTimeFlag = false;

		}

		if ( PlantLoopScanFlag( CBNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( CoolBeam( CBNum ).Name, TypeOf_CooledBeamAirTerminal, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitCoolBeam: Program terminated for previous conditions." );
			}
			PlantLoopScanFlag( CBNum ) = false;

		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			// Check to see if there is a Air Distribution Unit on the Zone Equipment List
			for ( Loop = 1; Loop <= NumCB; ++Loop ) {
				if ( CoolBeam( Loop ).ADUNum == 0 ) continue;
				if ( CheckZoneEquipmentList( "ZONEHVAC:AIRDISTRIBUTIONUNIT", AirDistUnit( CoolBeam( Loop ).ADUNum ).Name ) ) continue;
				ShowSevereError( "InitCoolBeam: ADU=[Air Distribution Unit," + AirDistUnit( CoolBeam( Loop ).ADUNum ).Name + "] is not on any ZoneHVAC:EquipmentList." );
				ShowContinueError( "...Unit=[" + CurrentModuleObject + ',' + CoolBeam( Loop ).Name + "] will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( CBNum ) && ! PlantLoopScanFlag( CBNum ) ) {

			SizeCoolBeam( CBNum );

			InWaterNode = CoolBeam( CBNum ).CWInNode;
			OutWaterNode = CoolBeam( CBNum ).CWOutNode;
			rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );
			CoolBeam( CBNum ).MaxCoolWaterMassFlow = rho * CoolBeam( CBNum ).MaxCoolWaterVolFlow;
			InitComponentNodes( 0.0, CoolBeam( CBNum ).MaxCoolWaterMassFlow, InWaterNode, OutWaterNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );
			MySizeFlag( CBNum ) = false;

		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( CBNum ) ) {
			RhoAir = StdRhoAir;
			InAirNode = CoolBeam( CBNum ).AirInNode;
			OutAirNode = CoolBeam( CBNum ).AirOutNode;
			// set the mass flow rates from the input volume flow rates
			CoolBeam( CBNum ).MaxAirMassFlow = RhoAir * CoolBeam( CBNum ).MaxAirVolFlow;
			Node( InAirNode ).MassFlowRateMax = CoolBeam( CBNum ).MaxAirMassFlow;
			Node( OutAirNode ).MassFlowRateMax = CoolBeam( CBNum ).MaxAirMassFlow;
			Node( InAirNode ).MassFlowRateMin = 0.0;
			Node( OutAirNode ).MassFlowRateMin = 0.0;

			InWaterNode = CoolBeam( CBNum ).CWInNode;
			OutWaterNode = CoolBeam( CBNum ).CWOutNode;
			InitComponentNodes( 0.0, CoolBeam( CBNum ).MaxCoolWaterMassFlow, InWaterNode, OutWaterNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );

			MyEnvrnFlag( CBNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( CBNum ) = true;
		}

		InAirNode = CoolBeam( CBNum ).AirInNode;
		OutAirNode = CoolBeam( CBNum ).AirOutNode;

		// Do the start of HVAC time step initializations
		if ( FirstHVACIteration ) {
			// check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
			if ( GetCurrentScheduleValue( CoolBeam( CBNum ).SchedPtr ) > 0.0 && Node( InAirNode ).MassFlowRate > 0.0 ) {
				Node( InAirNode ).MassFlowRate = CoolBeam( CBNum ).MaxAirMassFlow;
			} else {
				Node( InAirNode ).MassFlowRate = 0.0;
			}
			// reset the max and min avail flows
			if ( GetCurrentScheduleValue( CoolBeam( CBNum ).SchedPtr ) > 0.0 && Node( InAirNode ).MassFlowRateMaxAvail > 0.0 ) {
				Node( InAirNode ).MassFlowRateMaxAvail = CoolBeam( CBNum ).MaxAirMassFlow;
				Node( InAirNode ).MassFlowRateMinAvail = CoolBeam( CBNum ).MaxAirMassFlow;
			} else {
				Node( InAirNode ).MassFlowRateMaxAvail = 0.0;
				Node( InAirNode ).MassFlowRateMinAvail = 0.0;
			}
			//Plant should do this    InWaterNode = CoolBeam(CBNum)%CWInNode
			//    Node(InWaterNode)%MassFlowRateMaxAvail = CoolBeam(CBNum)%MaxCoolWaterMassFlow
			//    Node(InWaterNode)%MassFlowRateMinAvail = 0.0
		}

		// do these initializations every time step
		InWaterNode = CoolBeam( CBNum ).CWInNode;
		CoolBeam( CBNum ).TWIn = Node( InWaterNode ).Temp;
		CoolBeam( CBNum ).SupAirCoolingRate = 0.0;
		CoolBeam( CBNum ).SupAirHeatingRate = 0.0;

		// CoolBeam(CBNum)%BeamFlow = Node(InAirNode)%MassFlowRate / (StdRhoAir*CoolBeam(CBNum)%NumBeams)

	}

	void
	SizeCoolBeam( int const CBNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 10, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing cooled beam units for which flow rates have not been
		// specified in the input

		// METHODOLOGY EMPLOYED:
		// Accesses zone sizing array for air flow rates and zone and plant sizing arrays to
		// calculate coil water flow rates.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using DataGlobals::AutoCalculate;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		//  USE BranchInputManager,  ONLY: MyPlantSizingIndex
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "SizeCoolBeam" );
		static int PltSizCoolNum( 0 ); // index of plant sizing object for the cooling loop
		static int NumBeams( 0 ); // number of beams in the zone
		static int Iter( 0 ); // beam length iteration index
		static Real64 DesCoilLoad( 0.0 ); // total cooling capacity of the beams in the zone [W]
		static Real64 DesLoadPerBeam( 0.0 ); // cooling capacity per individual beam [W]
		static Real64 DesAirVolFlow( 0.0 ); // design total supply air flow rate [m3/s]
		static Real64 DesAirFlowPerBeam( 0.0 ); // design supply air volumetric flow per beam [m3/s]
		static Real64 RhoAir( 0.0 );
		static Real64 CpAir( 0.0 );
		static Real64 WaterVel( 0.0 ); // design water velocity in beam
		static Real64 IndAirFlowPerBeamL( 0.0 ); // induced volumetric air flow rate per beam length [m3/s-m]
		static Real64 DT( 0.0 ); // air - water delta T [C]
		static Real64 LengthX( 0.0 ); // test value for beam length [m]
		static Real64 Length( 0.0 ); // beam length [m]
		static Real64 ConvFlow( 0.0 ); // convective and induced air mass flow rate across beam per beam plan area [kg/s-m2]
		static Real64 K( 0.0 ); // coil (beam) heat transfer coefficient [W/m2-K]
		static Real64 WaterVolFlowPerBeam( 0.0 ); // Cooling water volumetric flow per beam [m3]
		bool ErrorsFound;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat

		PltSizCoolNum = 0;
		DesAirVolFlow = 0.0;
		CpAir = 0.0;
		RhoAir = StdRhoAir;
		ErrorsFound = false;
		// find the appropriate Plant Sizing object
		if ( CoolBeam( CBNum ).MaxAirVolFlow == AutoSize || CoolBeam( CBNum ).BeamLength == AutoSize ) {
			PltSizCoolNum = MyPlantSizingIndex( "cooled beam unit", CoolBeam( CBNum ).Name, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, ErrorsFound );
		}

		if ( CoolBeam( CBNum ).Kin == AutoCalculate ) {
			if ( CoolBeam( CBNum ).CBType_Num == Passive_Cooled_Beam ) {
				CoolBeam( CBNum ).Kin = 0.0;
			} else {
				CoolBeam( CBNum ).Kin = 2.0;
			}
			ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Coefficient of Induction Kin", CoolBeam( CBNum ).Kin );

		}

		if ( CoolBeam( CBNum ).MaxAirVolFlow == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {

				CheckZoneSizing( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name );
				CoolBeam( CBNum ).MaxAirVolFlow = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( CoolBeam( CBNum ).MaxAirVolFlow < SmallAirVolFlow ) {
					CoolBeam( CBNum ).MaxAirVolFlow = 0.0;
				}
				ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Supply Air Flow Rate [m3/s]", CoolBeam( CBNum ).MaxAirVolFlow );
			}

		}

		if ( CoolBeam( CBNum ).MaxCoolWaterVolFlow == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {

				CheckZoneSizing( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name );

				if ( PltSizCoolNum > 0 ) {

					if ( FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow >= SmallAirVolFlow ) {
						DesAirVolFlow = CoolBeam( CBNum ).MaxAirVolFlow;
						CpAir = PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat, FinalZoneSizing( CurZoneEqNum ).CoolDesTemp );
						// the design cooling coil load is the zone load minus whatever the central system does. Note that
						// DesCoolCoilInTempTU is really the primary air inlet temperature for the unit.
						if ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak > 0.0 ) {
							DesCoilLoad = FinalZoneSizing( CurZoneEqNum ).DesCoolLoad - CpAir * RhoAir * DesAirVolFlow * ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak - FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU );
						} else {
							DesCoilLoad = CpAir * RhoAir * DesAirVolFlow * ( FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU - ZoneSizThermSetPtHi( CurZoneEqNum ) );
						}

						rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

						Cp = GetSpecificHeatGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

						CoolBeam( CBNum ).MaxCoolWaterVolFlow = DesCoilLoad / ( ( CoolBeam( CBNum ).DesOutletWaterTemp - CoolBeam( CBNum ).DesInletWaterTemp ) * Cp * rho );
						CoolBeam( CBNum ).MaxCoolWaterVolFlow = max( CoolBeam( CBNum ).MaxCoolWaterVolFlow, 0.0 );
						if ( CoolBeam( CBNum ).MaxCoolWaterVolFlow < SmallWaterVolFlow ) {
							CoolBeam( CBNum ).MaxCoolWaterVolFlow = 0.0;
						}
					} else {
						CoolBeam( CBNum ).MaxCoolWaterVolFlow = 0.0;
					}

					ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Maximum Total Chilled Water Flow Rate [m3/s]", CoolBeam( CBNum ).MaxCoolWaterVolFlow );
				} else {
					ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
					ShowContinueError( "Occurs in" + CoolBeam( CBNum ).UnitType + " Object=" + CoolBeam( CBNum ).Name );
					ErrorsFound = true;
				}

			}

		}

		if ( CoolBeam( CBNum ).NumBeams == AutoSize ) {
			rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

			NumBeams = int( CoolBeam( CBNum ).MaxCoolWaterVolFlow * rho / NomMassFlowPerBeam ) + 1;
			CoolBeam( CBNum ).NumBeams = double( NumBeams );
			ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Number of Beams", CoolBeam( CBNum ).NumBeams );
		}

		if ( CoolBeam( CBNum ).BeamLength == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {

				CheckZoneSizing( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name );

				if ( PltSizCoolNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

					Cp = GetSpecificHeatGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );
					DesCoilLoad = CoolBeam( CBNum ).MaxCoolWaterVolFlow * ( CoolBeam( CBNum ).DesOutletWaterTemp - CoolBeam( CBNum ).DesInletWaterTemp ) * Cp * rho;
					if ( DesCoilLoad > 0.0 ) {
						DesLoadPerBeam = DesCoilLoad / NumBeams;
						DesAirFlowPerBeam = CoolBeam( CBNum ).MaxAirVolFlow / NumBeams;
						WaterVolFlowPerBeam = CoolBeam( CBNum ).MaxCoolWaterVolFlow / NumBeams;
						WaterVel = WaterVolFlowPerBeam / ( Pi * pow_2( CoolBeam( CBNum ).InDiam ) / 4.0 );
						if ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak > 0.0 ) {
							DT = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak - 0.5 * ( CoolBeam( CBNum ).DesInletWaterTemp + CoolBeam( CBNum ).DesOutletWaterTemp );
							if ( DT <= 0.0 ) {
								DT = 7.8;
							}
						} else {
							DT = 7.8;
						}
						LengthX = 1.0;
						for ( Iter = 1; Iter <= 100; ++Iter ) {
							IndAirFlowPerBeamL = CoolBeam( CBNum ).K1 * std::pow( DT, CoolBeam( CBNum ).n ) + CoolBeam( CBNum ).Kin * DesAirFlowPerBeam / LengthX;
							ConvFlow = ( IndAirFlowPerBeamL / CoolBeam( CBNum ).a0 ) * RhoAir;
							if ( WaterVel > MinWaterVel ) {
								K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( ConvFlow, CoolBeam( CBNum ).n2 ) * std::pow( WaterVel, CoolBeam( CBNum ).n3 );
							} else {
								K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( ConvFlow, CoolBeam( CBNum ).n2 ) * std::pow( MinWaterVel, CoolBeam( CBNum ).n3 ) * ( WaterVel / MinWaterVel );
							}
							Length = DesLoadPerBeam / ( K * CoolBeam( CBNum ).CoilArea * DT );
							if ( CoolBeam( CBNum ).Kin <= 0.0 ) break;
							// Check for convergence
							if ( std::abs( Length - LengthX ) > 0.01 ) {
								// New guess for length
								LengthX += 0.5 * ( Length - LengthX );
							} else {
								break; // convergence achieved
							}
						}
					} else {
						Length = 0.0;
					}
					CoolBeam( CBNum ).BeamLength = Length;
					CoolBeam( CBNum ).BeamLength = max( CoolBeam( CBNum ).BeamLength, 1.0 );
					ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Beam Length [m]", CoolBeam( CBNum ).BeamLength );
				} else {
					ShowSevereError( "Autosizing of cooled beam length requires a cooling loop Sizing:Plant object" );
					ShowContinueError( "Occurs in" + CoolBeam( CBNum ).UnitType + " Object=" + CoolBeam( CBNum ).Name );
					ErrorsFound = true;
				}

			}

		}

		// save the design water volumetric flow rate for use by the water loop sizing algorithms
		if ( CoolBeam( CBNum ).MaxCoolWaterVolFlow > 0.0 ) {
			RegisterPlantCompDesignFlow( CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).MaxCoolWaterVolFlow );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding cooled beam sizing errors cause program termination" );
		}

	}

	void
	ControlCoolBeam(
		int const CBNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const EP_UNUSED( FirstHVACIteration ), // TRUE if 1st HVAC simulation of system timestep
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 12, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a cooled beam unit;

		// METHODOLOGY EMPLOYED:
		// (1) From the zone load and the Supply air inlet conditions calculate the beam load
		// (2) If there is a beam load, vary the water flow rate to match the beam load

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using General::SolveRegulaFalsi;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QZnReq; // heating or cooling needed by zone [Watts]
		Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
		Real64 QToCoolSetPt; // [W]  remaining load to cooling setpoint
		static Real64 QMin( 0.0 ); // cooled beam output at minimum water flow [W]
		static Real64 QMax( 0.0 ); // cooled beam output at maximum water flow [W]
		static Real64 QSup( 0.0 ); // heating or cooling by supply air [W]
		static Real64 PowerMet( 0.0 ); // power supplied
		static Real64 CWFlow( 0.0 ); // cold water flow [kg/s]
		static Real64 AirMassFlow( 0.0 ); // air mass flow rate for the cooled beam system [kg/s]
		static Real64 MaxColdWaterFlow( 0.0 ); // max water mass flow rate for the cooled beam system [kg/s]
		static Real64 MinColdWaterFlow( 0.0 ); // min water mass flow rate for the cooled beam system [kg/s]
		static Real64 CpAirZn( 0.0 ); // specific heat of air at zone conditions [J/kg-C]
		static Real64 CpAirSys( 0.0 ); // specific heat of air at supply air conditions [J/kg-C]
		static Real64 TWOut( 0.0 ); // outlet water tamperature [C]
		int ControlNode; // the water inlet node
		int InAirNode; // the air inlet node
		bool UnitOn; // TRUE if unit is on
		Array1D< Real64 > Par( 5 );
		int SolFlag;
		Real64 ErrTolerance;

		UnitOn = true;
		PowerMet = 0.0;
		InAirNode = CoolBeam( CBNum ).AirInNode;
		ControlNode = CoolBeam( CBNum ).CWInNode;
		AirMassFlow = Node( InAirNode ).MassFlowRateMaxAvail;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		QToCoolSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
		CpAirSys = PsyCpAirFnWTdb( Node( InAirNode ).HumRat, Node( InAirNode ).Temp );
		MaxColdWaterFlow = CoolBeam( CBNum ).MaxCoolWaterMassFlow;
		SetComponentFlowRate( MaxColdWaterFlow, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );
		MinColdWaterFlow = 0.0;
		SetComponentFlowRate( MinColdWaterFlow, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );

		if ( GetCurrentScheduleValue( CoolBeam( CBNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( MaxColdWaterFlow <= SmallMassFlow ) UnitOn = false;

		// Set the unit's air inlet nodes mass flow rates
		Node( InAirNode ).MassFlowRate = AirMassFlow;
		// set the air volumetric flow rate per beam
		CoolBeam( CBNum ).BeamFlow = Node( InAirNode ).MassFlowRate / ( StdRhoAir * CoolBeam( CBNum ).NumBeams );
		// fire the unit at min water flow
		CalcCoolBeam( CBNum, ZoneNodeNum, MinColdWaterFlow, QMin, TWOut );
		// cooling by supply air
		QSup = AirMassFlow * ( CpAirSys * Node( InAirNode ).Temp - CpAirZn * Node( ZoneNodeNum ).Temp );
		// load on the beams is QToCoolSetPt-QSup
		if ( UnitOn ) {
			if ( ( QToCoolSetPt - QSup ) < -SmallLoad ) {
				// There is a cooling demand on the cooled beam system.
				// First, see if the system can meet the load
				CalcCoolBeam( CBNum, ZoneNodeNum, MaxColdWaterFlow, QMax, TWOut );
				if ( ( QMax < QToCoolSetPt - QSup - SmallLoad ) && ( QMax != QMin ) ) {
					// The cooled beam system can meet the demand.
					// Set up the iterative calculation of chilled water flow rate
					Par( 1 ) = double( CBNum );
					Par( 2 ) = double( ZoneNodeNum );
					Par( 3 ) = QToCoolSetPt - QSup; // load to be met by the beams
					Par( 4 ) = QMin;
					Par( 5 ) = QMax;
					ErrTolerance = 0.01;
					SolveRegulaFalsi( ErrTolerance, 50, SolFlag, CWFlow, CoolBeamResidual, MinColdWaterFlow, MaxColdWaterFlow, Par );
					if ( SolFlag == -1 ) {
						ShowWarningError( "Cold water control failed in cooled beam unit " + CoolBeam( CBNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating cold water mass flow rate" );
					} else if ( SolFlag == -2 ) {
						ShowWarningError( "Cold water control failed in cooled beam unit " + CoolBeam( CBNum ).Name );
						ShowContinueError( "  Bad cold water flow limits" );
					}
				} else {
					// unit maxed out
					CWFlow = MaxColdWaterFlow;
				}
			} else {
				// unit has no load
				CWFlow = MinColdWaterFlow;
			}
		} else {
			// unit Off
			CWFlow = MinColdWaterFlow;
		}
		// Get the cooling output at the chosen water flow rate
		CalcCoolBeam( CBNum, ZoneNodeNum, CWFlow, PowerMet, TWOut );
		CoolBeam( CBNum ).BeamCoolingRate = -PowerMet;
		if ( QSup < 0.0 ) {
			CoolBeam( CBNum ).SupAirCoolingRate = std::abs( QSup );
		} else {
			CoolBeam( CBNum ).SupAirHeatingRate = QSup;
		}
		CoolBeam( CBNum ).CoolWaterMassFlow = Node( ControlNode ).MassFlowRate;
		CoolBeam( CBNum ).TWOut = TWOut;
		CoolBeam( CBNum ).EnthWaterOut = Node( ControlNode ).Enthalpy + CoolBeam( CBNum ).BeamCoolingRate;
		//  Node(ControlNode)%MassFlowRate = CWFlow
		NonAirSysOutput = PowerMet;

	}

	void
	CalcCoolBeam(
		int const CBNum, // Unit index
		int const ZoneNode, // zone node number
		Real64 const CWFlow, // cold water flow [kg/s]
		Real64 & LoadMet, // load met by unit [W]
		Real64 & TWOut // chilled water outlet temperature [C]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a cooled beam given the chilled water flow rate

		// METHODOLOGY EMPLOYED:
		// Uses the cooled beam equations; iteratively varies water outlet  temperature
		// until air-side and water-side cooling outputs match.

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcCoolBeam" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int Iter( 0 ); // TWOut iteration index
		static Real64 TWIn( 0.0 ); // Inlet water temperature [C]
		static Real64 ZTemp( 0.0 ); // zone air temperature [C]
		static Real64 WaterCoolPower( 0.0 ); // cooling power from water side [W]
		static Real64 DT( 0.0 ); // approximate air - water delta T [C]
		static Real64 IndFlow( 0.0 ); // induced air flow rate per beam length [m3/s-m]
		static Real64 CoilFlow( 0.0 ); // mass air flow rate of air passing through "coil" [kg/m2-s]
		static Real64 WaterVel( 0.0 ); // water velocity [m/s]
		static Real64 K( 0.0 ); // coil heat transfer coefficient [W/m2-K]
		static Real64 AirCoolPower( 0.0 ); // cooling power from the air side [W]
		Real64 Diff; // difference between water side cooling power and air side cooling power [W]
		static Real64 CWFlowPerBeam( 0.0 ); // water mass flow rate per beam
		static Real64 Coeff( 0.0 ); // iteration parameter
		static Real64 Delta( 0.0 );
		static Real64 mdot( 0.0 );
		Real64 Cp; // local fluid specific heat
		Real64 rho; // local fluid density

		//test CWFlow against plant
		mdot = CWFlow;

		SetComponentFlowRate( mdot, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );

		CWFlowPerBeam = mdot / CoolBeam( CBNum ).NumBeams;
		TWIn = CoolBeam( CBNum ).TWIn;

		Cp = GetSpecificHeatGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, TWIn, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

		rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, TWIn, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

		TWOut = TWIn + 2.0;
		ZTemp = Node( ZoneNode ).Temp;
		if ( mdot <= 0.0 || TWIn <= 0.0 ) {
			LoadMet = 0.0;
			TWOut = TWIn;
			return;
		}
		for ( Iter = 1; Iter <= 200; ++Iter ) {
			if ( Iter > 50 && Iter < 100 ) {
				Coeff = 0.1 * Coeff2;
			} else if ( Iter > 100 ) {
				Coeff = 0.01 * Coeff2;
			} else {
				Coeff = Coeff2;
			}

			WaterCoolPower = CWFlowPerBeam * Cp * ( TWOut - TWIn );
			DT = max( ZTemp - 0.5 * ( TWIn + TWOut ), 0.0 );
			IndFlow = CoolBeam( CBNum ).K1 * std::pow( DT, CoolBeam( CBNum ).n ) + CoolBeam( CBNum ).Kin * CoolBeam( CBNum ).BeamFlow / CoolBeam( CBNum ).BeamLength;
			CoilFlow = ( IndFlow / CoolBeam( CBNum ).a0 ) * StdRhoAir;
			WaterVel = CWFlowPerBeam / ( rho * Pi * pow_2( CoolBeam( CBNum ).InDiam ) / 4.0 );
			if ( WaterVel > MinWaterVel ) {
				K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( CoilFlow, CoolBeam( CBNum ).n2 ) * std::pow( WaterVel, CoolBeam( CBNum ).n3 );
			} else {
				K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( CoilFlow, CoolBeam( CBNum ).n2 ) * std::pow( MinWaterVel, CoolBeam( CBNum ).n3 ) * ( WaterVel / MinWaterVel );
			}
			AirCoolPower = K * CoolBeam( CBNum ).CoilArea * DT * CoolBeam( CBNum ).BeamLength;
			Diff = WaterCoolPower - AirCoolPower;
			Delta = TWOut * ( std::abs( Diff ) / Coeff );
			if ( std::abs( Diff ) > 0.1 ) {
				if ( Diff < 0.0 ) {
					TWOut += Delta; // increase TWout
					if ( TWOut > ZTemp ) { // check that water outlet temperature is less than zone temperature
						WaterCoolPower = 0.0;
						TWOut = ZTemp;
						break;
					}
				} else {
					TWOut -= Delta; // Decrease TWout
					if ( TWOut < TWIn ) {
						TWOut = TWIn;
					}
				}
			} else {
				// water and air side outputs have converged
				break;
			}
		}
		LoadMet = -WaterCoolPower * CoolBeam( CBNum ).NumBeams;

	}

	Real64
	CoolBeamResidual(
		Real64 const CWFlow, // cold water flow rate in kg/s
		Array1< Real64 > const & Par
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2009
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Unit Load - Unit Output) / Max Unit Output
		// Unit Output depends on the cold water flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcCoolBeam, and calculates the residual as defined above.

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
		int CBIndex;
		int ZoneNodeIndex;
		static Real64 UnitOutput( 0.0 );
		static Real64 TWOut( 0.0 );

		CBIndex = int( Par( 1 ) );
		ZoneNodeIndex = int( Par( 2 ) );
		CalcCoolBeam( CBIndex, ZoneNodeIndex, CWFlow, UnitOutput, TWOut );
		Residuum = ( Par( 3 ) - UnitOutput ) / ( Par( 5 ) - Par( 4 ) );

		return Residuum;
	}

	void
	UpdateCoolBeam( int const CBNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the cooled beam unit outlet nodes

		// METHODOLOGY EMPLOYED:
		// Data is moved from the cooled beam unit data structure to the unit outlet nodes.

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
		int WaterInletNode;
		int AirOutletNode;
		int WaterOutletNode;

		AirInletNode = CoolBeam( CBNum ).AirInNode;
		WaterInletNode = CoolBeam( CBNum ).CWInNode;
		AirOutletNode = CoolBeam( CBNum ).AirOutNode;
		WaterOutletNode = CoolBeam( CBNum ).CWOutNode;

		// Set the outlet air nodes of the unit; note that all quantities are unchanged
		Node( AirOutletNode ).MassFlowRate = Node( AirInletNode ).MassFlowRate;
		Node( AirOutletNode ).Temp = Node( AirInletNode ).Temp;
		Node( AirOutletNode ).HumRat = Node( AirInletNode ).HumRat;
		Node( AirOutletNode ).Enthalpy = Node( AirInletNode ).Enthalpy;

		// Set the outlet water nodes for the unit
		//  Node(WaterOutletNode)%MassFlowRate = CoolBeam(CBNum)%CoolWaterMassFlow
		SafeCopyPlantNode( WaterInletNode, WaterOutletNode );

		Node( WaterOutletNode ).Temp = CoolBeam( CBNum ).TWOut;
		Node( WaterOutletNode ).Enthalpy = CoolBeam( CBNum ).EnthWaterOut;

		// Set the air outlet nodes for properties that just pass through & not used
		Node( AirOutletNode ).Quality = Node( AirInletNode ).Quality;
		Node( AirOutletNode ).Press = Node( AirInletNode ).Press;
		Node( AirOutletNode ).MassFlowRateMin = Node( AirInletNode ).MassFlowRateMin;
		Node( AirOutletNode ).MassFlowRateMax = Node( AirInletNode ).MassFlowRateMax;
		Node( AirOutletNode ).MassFlowRateMinAvail = Node( AirInletNode ).MassFlowRateMinAvail;
		Node( AirOutletNode ).MassFlowRateMaxAvail = Node( AirInletNode ).MassFlowRateMaxAvail;

		// Set the outlet nodes for properties that just pass through & not used
		//  Node(WaterOutletNode)%Quality             = Node(WaterInletNode)%Quality
		//  Node(WaterOutletNode)%Press               = Node(WaterInletNode)%Press
		//  Node(WaterOutletNode)%HumRat              = Node(WaterInletNode)%HumRat
		//  Node(WaterOutletNode)%MassFlowRateMin     = Node(WaterInletNode)%MassFlowRateMin
		//  Node(WaterOutletNode)%MassFlowRateMax     = Node(WaterInletNode)%MassFlowRateMax
		//  Node(WaterOutletNode)%MassFlowRateMinAvail= Node(WaterInletNode)%MassFlowRateMinAvail
		//  Node(WaterOutletNode)%MassFlowRateMaxAvail= Node(WaterInletNode)%MassFlowRateMaxAvail

		//  IF (CoolBeam(CBNum)%CoolWaterMassFlow.EQ.0.0) THEN
		//    Node(WaterInletNode)%MassFlowRateMinAvail= 0.0
		//    Node(WaterOutletNode)%MassFlowRateMinAvail= 0.0
		//  END IF

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNode ).CO2 = Node( AirInletNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutletNode ).GenContam = Node( AirInletNode ).GenContam;
		}

	}

	void
	ReportCoolBeam( int const CBNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variable for the cooled beam units

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// USE STATEMENTS:

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
		// report the WaterCoil energy from this component
		CoolBeam( CBNum ).BeamCoolingEnergy = CoolBeam( CBNum ).BeamCoolingRate * ReportingConstant;
		CoolBeam( CBNum ).SupAirCoolingEnergy = CoolBeam( CBNum ).SupAirCoolingRate * ReportingConstant;
		CoolBeam( CBNum ).SupAirHeatingEnergy = CoolBeam( CBNum ).SupAirHeatingRate * ReportingConstant;

	}

} // HVACCooledBeam

} // EnergyPlus
