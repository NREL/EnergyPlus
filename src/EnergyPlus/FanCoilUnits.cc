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
#include <FanCoilUnits.hh>
#include <BranchNodeConnections.hh>
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
#include <HVACHXAssistedCoolingCoil.hh>
#include <InputProcessor.hh>
#include <MixedAir.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SingleDuct.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>
#include <ZoneEquipmentManager.hh>

namespace EnergyPlus {

namespace FanCoilUnits {

	// Module containing the routines dealing with 2 and 4 pipe fan coil units

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   March 2000
	//       MODIFIED       October 2003 (FSEC added cooling coil type)
	//                      June 2010    Arnaud Flament LBNL added 3-speed and variables-speed fan capacity control;
	//                                   outside air schedule; and removed coil water inlet node inputs
	//                      Sept 2010    Brent Griffith, plant upgrades for water coils, fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms needed to simulate 2 and 4 pipe
	// fan coil units.

	// METHODOLOGY EMPLOYED:
	// Units are modeled as a collection of components: outside air mixer,
	// fan, heating coil and/or cooling coil plus an integrated control
	// algorithm that adjusts the hot or cold water flow to meet the zone
	// load. Or varies the air flow rate to meet the zone load. Or both.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataSizing;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::SecInHour;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::DisplayExtraWarnings;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutRelHum;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::SingleCoolingSetPoint;
	using DataHVACGlobals::SingleHeatingSetPoint;
	using DataHVACGlobals::SingleHeatCoolSetPoint;
	using DataHVACGlobals::DualSetPointWithDeadBand;
	using DataHVACGlobals::cFanTypes;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::ATMixer_InletSide;
	using DataHVACGlobals::ATMixer_SupplySide;
	using DataHVACGlobals::cATMixerTypes;
	using DataHVACGlobals::ATMixerExists;
	using DataHVACGlobals::CycFanCycCoil;
	using DataHVACGlobals::ContFanCycCoil;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	// MODULE PARAMETER DEFINITIONS

	std::string const cMO_FanCoil( "ZoneHVAC:FourPipeFanCoil" );

	// coil operation
	int const On( 1 ); // normal coil operation
	int const Off( 0 ); // signal coil shouldn't run

	// coil type units supported in this module
	int const FanCoilUnit_4Pipe( 1 );

	int const CCoil_Water( 1 );
	int const CCoil_Detailed( 2 );
	int const CCoil_HXAssist( 3 );

	int const HCoil_Water( 1 );
	int const HCoil_Electric( 2 );

	//capacity control method supported in this module
	int const CCM_ConsFanVarFlow( 1 );
	int const CCM_CycFan( 2 );
	int const CCM_VarFanVarFlow( 3 );
	int const CCM_VarFanConsFlow( 4 );
	int const CCM_MultiSpeedFan( 5 );
	int const CCM_ASHRAE( 6 );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:	
	namespace {
		bool InitFanCoilUnitsOneTimeFlag( true );
		bool InitFanCoilUnitsCheckInZoneEquipmentListFlag( false ); // True after the Zone Equipment List has been checked for items
	}

	int NumFanCoils( 0 );
	int Num4PipeFanCoils( 0 );
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;
	bool GetFanCoilInputFlag( true ); // First time, input is "gotten"
	Real64 FanFlowRatio( 0.0 );
	bool HeatingLoad( false ); // True when zone needs heating
	bool CoolingLoad( false ); // True when zone needs cooling
	Real64 const Small5WLoad( 5.0 ); // load threshold 5.0 W

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// look up functions for node numbers

	// Object Data
	Array1D< FanCoilData > FanCoil;
	Array1D< FanCoilNumericFieldData > FanCoilNumericFields;

	// Functions

	void
	clear_state()
	{
		NumFanCoils = 0;
		Num4PipeFanCoils = 0;
		MySizeFlag.deallocate();
		CheckEquipName.deallocate();
		GetFanCoilInputFlag = true;
		FanFlowRatio = 0.0;
		HeatingLoad = false;
		CoolingLoad = false;
		FanCoil.deallocate();
		FanCoilNumericFields.deallocate();
		InitFanCoilUnitsOneTimeFlag = true;
		InitFanCoilUnitsCheckInZoneEquipmentListFlag = false;
	}

	void
	SimFanCoilUnit(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		int const ControlledZoneNum, // index into ZoneEquipConfig array; may not be equal to ZoneNum
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a fan coil unit. Called from SimZone Equipment

		// METHODOLOGY EMPLOYED:
		// NA

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
		int FanCoilNum; // index of fan coil unit being simulated

		// FLOW

		// First time SimFanCoilUnit is called, get the input for all the fan coil units
		if ( GetFanCoilInputFlag ) {
			GetFanCoilUnits();
			GetFanCoilInputFlag = false;
		}

		// Find the correct Fan Coil Equipment
		if ( CompIndex == 0 ) {
			FanCoilNum = FindItemInList( CompName, FanCoil );
			if ( FanCoilNum == 0 ) {
				ShowFatalError( "SimFanCoil: Unit not found=" + CompName );
			}
			CompIndex = FanCoilNum;
		} else {
			FanCoilNum = CompIndex;
			if ( FanCoilNum > NumFanCoils || FanCoilNum < 1 ) {
				ShowFatalError( "SimFanCoil:  Invalid CompIndex passed=" + TrimSigDigits( FanCoilNum ) + ", Number of Units=" + TrimSigDigits( NumFanCoils ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( FanCoilNum ) ) {
				if ( CompName != FanCoil( FanCoilNum ).Name ) {
					ShowFatalError( "SimFanCoil: Invalid CompIndex passed=" + TrimSigDigits( FanCoilNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + FanCoil( FanCoilNum ).Name );
				}
				CheckEquipName( FanCoilNum ) = false;
			}
		}

		ZoneEqFanCoil = true;

		// Initialize the fan coil unit
		InitFanCoilUnits( FanCoilNum, ZoneNum );

		// Select the correct unit type
		{ auto const SELECT_CASE_var( FanCoil( FanCoilNum ).UnitType_Num );

		if ( SELECT_CASE_var == FanCoilUnit_4Pipe ) {

			Sim4PipeFanCoil( FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided );

		}}

		// Report the result of the simulation
		ReportFanCoilUnit( FanCoilNum );

		ZoneEqFanCoil = false;

	}

	void
	GetFanCoilUnits()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2000
		//       MODIFIED       Bereket Nigusse, FSEC, April 2011: eliminated input node names
		//                                                         added OA Mixer object type
		//                                                         and fan object type
		//                      Chandan Sharma, FSEC, July 2012: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for fan coil units and stores it in fan coil data structures

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
		using Fans::GetFanDesignVolumeFlowRate;
		using Fans::GetFanType;
		using General::TrimSigDigits;
		using namespace DataIPShortCuts;
		using WaterCoils::GetCoilWaterInletNode;
		auto & GetHXCoilWaterInletNode( HVACHXAssistedCoolingCoil::GetCoilWaterInletNode );
		auto & GetHeatingCoilCapacity( HeatingCoils::GetCoilCapacity );
		using HVACHXAssistedCoolingCoil::GetHXCoilTypeAndName;
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleVAV;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using DataPlant::TypeOf_CoilWaterCooling;
		using MixedAir::GetOAMixerIndex;
		using MixedAir::GetOAMixerNodeNumbers;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataGlobals::NumOfZones;
		using DataGlobals::ScheduleAlwaysOn;
		using SingleDuct::GetATMixer;
		using InputProcessor::FindItemInList;
		using DataSizing::ZoneHVACSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		static std::string const RoutineName( "GetFanCoilUnits: " ); // include trailing blank space

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilIndex; // loop index
		int FanCoilNum; // current fan coil number
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		Array1D_int OANodeNums( 4 ); // Node numbers of Outdoor air mixer (OA, EA, RA, MA)
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static bool errFlag( false ); // Local error flag for GetOAMixerNodeNums
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // Object type for getting and error messages
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		int CtrlZone; // index to loop counter
		int NodeNum; // index to loop counter
		static bool ZoneExNodeNotFound( false ); // used in error checking
		static bool ZoneInNodeNotFound( false ); // used in error checking
		static int ATMixerNum( 0 ); // index of air terminal mixer in the air terminal mixer data array
		static int ATMixerType( 0 ); // type of air terminal mixer (1=inlet side; 2=supply side)
		static int ATMixerPriNode( 0 ); // node number of the air terminal mixer primary air inlet
		static int ATMixerSecNode( 0 ); // node number of the air terminal mixer secondary air inlet
		static int ATMixerOutNode( 0 ); // node number of the air terminal mixer secondary air inlet
		std::string ATMixerName;

		// FLOW

		// find the number of each type of fan coil unit

		CurrentModuleObject = cMO_FanCoil;
		Num4PipeFanCoils = GetNumObjectsFound( CurrentModuleObject );
		NumFanCoils = Num4PipeFanCoils;
		// allocate the data structures
		FanCoil.allocate( NumFanCoils );
		FanCoilNumericFields.allocate( NumFanCoils );
		CheckEquipName.dimension( NumFanCoils, true );

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// loop over 4 pipe fan coil units; get and load the input data
		for ( FanCoilIndex = 1; FanCoilIndex <= Num4PipeFanCoils; ++FanCoilIndex ) {

			GetObjectItem( CurrentModuleObject, FanCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			FanCoilNum = FanCoilIndex;

			FanCoilNumericFields( FanCoilNum ).FieldNames.allocate( NumNumbers );
			FanCoilNumericFields( FanCoilNum ).FieldNames = "";
			FanCoilNumericFields( FanCoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), FanCoil, FanCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			FanCoil( FanCoilNum ).Name = Alphas( 1 );
			FanCoil( FanCoilNum ).UnitType = CurrentModuleObject;
			FanCoil( FanCoilNum ).UnitType_Num = FanCoilUnit_4Pipe;
			FanCoil( FanCoilNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				FanCoil( FanCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				FanCoil( FanCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( FanCoil( FanCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid" );
					ShowContinueError( "invalid-not found: " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			if ( SameString( Alphas( 3 ), "ConstantFanVariableFlow" ) || SameString( Alphas( 3 ), "CyclingFan" ) || SameString( Alphas( 3 ), "VariableFanVariableFlow" ) || SameString( Alphas( 3 ), "VariableFanConstantFlow" ) || SameString( Alphas( 3 ), "MultiSpeedFan" ) || SameString( Alphas( 3 ), "ASHRAE90VariableFan" ) ) {
				FanCoil( FanCoilNum ).CapCtrlMeth = Alphas( 3 );
				if ( SameString( Alphas( 3 ), "ConstantFanVariableFlow" ) ) FanCoil( FanCoilNum ).CapCtrlMeth_Num = CCM_ConsFanVarFlow;
				if ( SameString( Alphas( 3 ), "CyclingFan" ) ) FanCoil( FanCoilNum ).CapCtrlMeth_Num = CCM_CycFan;
				if ( SameString( Alphas( 3 ), "VariableFanVariableFlow" ) ) FanCoil( FanCoilNum ).CapCtrlMeth_Num = CCM_VarFanVarFlow;
				if ( SameString( Alphas( 3 ), "VariableFanConstantFlow" ) ) FanCoil( FanCoilNum ).CapCtrlMeth_Num = CCM_VarFanConsFlow;
				if ( SameString( Alphas( 3 ), "MultiSpeedFan" ) ) FanCoil( FanCoilNum ).CapCtrlMeth_Num = CCM_MultiSpeedFan;
				if ( SameString( Alphas( 3 ), "ASHRAE90VariableFan" ) ) {
					FanCoil( FanCoilNum ).CapCtrlMeth_Num = CCM_ASHRAE;
					FanCoil( FanCoilNum ).DesZoneCoolingLoad = AutoSize;
					FanCoil( FanCoilNum ).DesZoneHeatingLoad = AutoSize;
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\", invalid" );
				ShowContinueError( "illegal value: " + cAlphaFields( 3 ) + "=\"" + Alphas( 3 ) + "\"." );
				ErrorsFound = true;
			}

			FanCoil( FanCoilNum ).SchedOutAir = Alphas( 4 );
			FanCoil( FanCoilNum ).SchedOutAirPtr = GetScheduleIndex( Alphas( 4 ) ); // convert schedule name to pointer
			if ( FanCoil( FanCoilNum ).SchedOutAirPtr == 0 && ( ! lAlphaBlanks( 4 ) ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\", invalid" );
				ShowContinueError( "illegal value: " + cAlphaFields( 4 ) + "=\"" + Alphas( 4 ) + "\"." );
				ErrorsFound = true;
			}
			FanCoil( FanCoilNum ).MaxAirVolFlow = Numbers( 1 );
			FanCoil( FanCoilNum ).LowSpeedRatio = Numbers( 2 );
			FanCoil( FanCoilNum ).MedSpeedRatio = Numbers( 3 );
			// check if low speed ratio < medium speed ratio, if not : warning & set to default values
			if ( FanCoil( FanCoilNum ).LowSpeedRatio > FanCoil( FanCoilNum ).MedSpeedRatio ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"," );
				ShowContinueError( "... " + cNumericFields( 2 ) + " is greater than the medium speed supply air flow ratio." );
				ShowContinueError( "... Fan Coil Unit low speed supply air flow ratio = " + TrimSigDigits( FanCoil( FanCoilNum ).LowSpeedRatio, 5 ) + ' ' );
				ShowContinueError( "... Fan Coit Unit medium speed supply air flow ratio = " + TrimSigDigits( FanCoil( FanCoilNum ).MedSpeedRatio, 5 ) + ' ' );
				ShowContinueError( "... Fan Coil Unit low speed supply air flow ratio and medium speed supply air flow ratio set to default values" );
				FanCoil( FanCoilNum ).LowSpeedRatio = 1.0 / 3.0;
				FanCoil( FanCoilNum ).MedSpeedRatio = 2.0 / 3.0;
			}

			FanCoil( FanCoilNum ).OutAirVolFlow = Numbers( 4 );

			FanCoil( FanCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, FanCoil( FanCoilNum ).UnitType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent ); // air input node

			FanCoil( FanCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, FanCoil( FanCoilNum ).UnitType, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent ); // air outlet node

			FanCoil( FanCoilNum ).OAMixType = Alphas( 7 );
			FanCoil( FanCoilNum ).OAMixName = Alphas( 8 );
			// check to see if local OA mixer specified
			if ( ! lAlphaBlanks( 8 ) ) {
				errFlag = false;
				ValidateComponent( FanCoil( FanCoilNum ).OAMixType, FanCoil( FanCoilNum ).OAMixName, errFlag, CurrentModuleObject );
				if ( errFlag ) {
					ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\"." );
					ErrorsFound = true;
				} else {
					// Get outdoor air mixer node numbers
					OANodeNums = GetOAMixerNodeNumbers( FanCoil( FanCoilNum ).OAMixName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "that was specified in " + CurrentModuleObject + " = " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name." );
						ErrorsFound = true;
					} else {
						FanCoil( FanCoilNum ).OutsideAirNode = OANodeNums( 1 );
						FanCoil( FanCoilNum ).AirReliefNode = OANodeNums( 2 );
						FanCoil( FanCoilNum ).MixedAirNode = OANodeNums( 4 );
					}
				}
			}

			FanCoil( FanCoilNum ).CCoilName = Alphas( 12 );
			FanCoil( FanCoilNum ).MaxColdWaterVolFlow = Numbers( 5 );
			FanCoil( FanCoilNum ).MinColdWaterVolFlow = Numbers( 6 );
			FanCoil( FanCoilNum ).ColdControlOffset = Numbers( 7 );
			FanCoil( FanCoilNum ).HCoilName = Alphas( 14 );
			FanCoil( FanCoilNum ).HCoilType = Alphas( 13 );
			FanCoil( FanCoilNum ).MaxHotWaterVolFlow = Numbers( 8 );
			FanCoil( FanCoilNum ).MinHotWaterVolFlow = Numbers( 9 );
			FanCoil( FanCoilNum ).HotControlOffset = Numbers( 10 );

			if ( SameString( Alphas( 11 ), "Coil:Cooling:Water" ) || SameString( Alphas( 11 ), "Coil:Cooling:Water:DetailedGeometry" ) || SameString( Alphas( 11 ), "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
				FanCoil( FanCoilNum ).CCoilType = Alphas( 11 );
				if ( SameString( Alphas( 11 ), "Coil:Cooling:Water" ) ) {
					FanCoil( FanCoilNum ).CCoilType_Num = CCoil_Water;
					FanCoil( FanCoilNum ).CCoilPlantName = FanCoil( FanCoilNum ).CCoilName;
					FanCoil( FanCoilNum ).CCoilPlantTypeOfNum = TypeOf_CoilWaterCooling;
				}
				if ( SameString( Alphas( 11 ), "Coil:Cooling:Water:DetailedGeometry" ) ) {
					FanCoil( FanCoilNum ).CCoilType_Num = CCoil_Detailed;
					FanCoil( FanCoilNum ).CCoilPlantName = FanCoil( FanCoilNum ).CCoilName;
					FanCoil( FanCoilNum ).CCoilPlantTypeOfNum = TypeOf_CoilWaterDetailedFlatCooling;
				}
				if ( SameString( Alphas( 11 ), "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
					FanCoil( FanCoilNum ).CCoilType_Num = CCoil_HXAssist;
					GetHXCoilTypeAndName( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, ErrorsFound, FanCoil( FanCoilNum ).CCoilPlantType, FanCoil( FanCoilNum ).CCoilPlantName );
					if ( SameString( FanCoil( FanCoilNum ).CCoilPlantType, "Coil:Cooling:Water" ) ) {
						FanCoil( FanCoilNum ).CCoilPlantTypeOfNum = TypeOf_CoilWaterCooling;
					} else if ( SameString( FanCoil( FanCoilNum ).CCoilPlantType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
						FanCoil( FanCoilNum ).CCoilPlantTypeOfNum = TypeOf_CoilWaterDetailedFlatCooling;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\", invalid" );
						ShowContinueError( "For: " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
						ShowContinueError( "Invalid Coil Type=" + FanCoil( FanCoilNum ).CCoilPlantType + ", Name=" + FanCoil( FanCoilNum ).CCoilPlantName );
						ShowContinueError( "must be \"Coil:Cooling:Water\" or \"Coil:Cooling:Water:DetailedGeometry\"" );
						ErrorsFound = true;
					}
				}
				IsNotOK = false;
				ValidateComponent( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, IsNotOK, FanCoil( FanCoilNum ).UnitType );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"." );
					ErrorsFound = true;
				} else {
					if ( FanCoil( FanCoilNum ).CCoilType_Num != CCoil_HXAssist ) {
						// mine the cold water node from the coil object
						FanCoil( FanCoilNum ).ColdControlNode = GetCoilWaterInletNode( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, IsNotOK );
					} else {
						FanCoil( FanCoilNum ).ColdControlNode = GetHXCoilWaterInletNode( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, IsNotOK );
					}
					// Other error checks should trap before it gets to this point in the code, but including just in case.
					if ( IsNotOK ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"." );
						ErrorsFound = true;
					}
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\", invalid" );
				ShowContinueError( "illegal value: " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 13 ), "Coil:Heating:Water" ) ) {
				FanCoil( FanCoilNum ).HCoilType_Num = HCoil_Water;
				FanCoil( FanCoilNum ).HCoilPlantTypeOfNum = TypeOf_CoilWaterSimpleHeating;
				IsNotOK = false;
				ValidateComponent( FanCoil( FanCoilNum ).HCoilType, FanCoil( FanCoilNum ).HCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"." );
					ErrorsFound = true;
				} else {
					// mine the hot water node from the coil object
					FanCoil( FanCoilNum ).HotControlNode = GetCoilWaterInletNode( FanCoil( FanCoilNum ).HCoilType, FanCoil( FanCoilNum ).HCoilName, IsNotOK );
					if ( IsNotOK ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"." );
						ErrorsFound = true;
					}
				}
			} else if ( SameString( Alphas( 13 ), "Coil:Heating:Electric" ) ) {
				FanCoil( FanCoilNum ).HCoilType_Num = HCoil_Electric;
				IsNotOK = false;
				ValidateComponent( FanCoil( FanCoilNum ).HCoilType, FanCoil( FanCoilNum ).HCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"." );
					ErrorsFound = true;
				} else {
					FanCoil( FanCoilNum ).DesignHeatingCapacity = GetHeatingCoilCapacity( FanCoil( FanCoilNum ).HCoilType, FanCoil( FanCoilNum ).HCoilName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + FanCoil( FanCoilNum ).Name );
						ErrorsFound = true;
					}
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\", invalid" );
				ShowContinueError( "illegal value: " + cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\"." );
				ErrorsFound = true;
			}

			FanCoil( FanCoilNum ).FanType = Alphas( 9 );
			FanCoil( FanCoilNum ).FanName = Alphas( 10 );

			if ( ! lAlphaBlanks( 15 ) ) {
				FanCoil( FanCoilNum ).AvailManagerListName = Alphas( 15 );
			}

			FanCoil( FanCoilNum ).HVACSizingIndex = 0;
			if ( !lAlphaBlanks( 16 ) ) {
				FanCoil( FanCoilNum ).HVACSizingIndex = FindItemInList( Alphas( 16 ), ZoneHVACSizing );
				if ( FanCoil( FanCoilNum ).HVACSizingIndex == 0 ) {
					ShowSevereError( cAlphaFields( 16 ) + " = " + Alphas( 16 ) + " not found." );
					ShowContinueError( "Occurs in " + cMO_FanCoil + " = " + FanCoil( FanCoilNum ).Name );
					ErrorsFound = true;
				}
			}

			errFlag = false;
			ValidateComponent( FanCoil( FanCoilNum ).FanType, FanCoil( FanCoilNum ).FanName, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				GetFanType( FanCoil( FanCoilNum ).FanName, FanCoil( FanCoilNum ).FanType_Num, errFlag, CurrentModuleObject, FanCoil( FanCoilNum ).Name );
				{ auto const SELECT_CASE_var( FanCoil( FanCoilNum ).FanType_Num );
				if ( ( SELECT_CASE_var == FanType_SimpleConstVolume ) || ( SELECT_CASE_var == FanType_SimpleVAV ) || ( SELECT_CASE_var == FanType_SimpleOnOff ) ) {
					// Get fan air volume flow rate
					FanCoil( FanCoilNum ).FanAirVolFlow = GetFanDesignVolumeFlowRate( FanCoil( FanCoilNum ).FanType, FanCoil( FanCoilNum ).FanName, IsNotOK );
					// Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
					if ( FanCoil( FanCoilNum ).MaxAirVolFlow > FanCoil( FanCoilNum ).FanAirVolFlow && FanCoil( FanCoilNum ).FanAirVolFlow != AutoSize ) {
						ShowWarningError( RoutineName + FanCoil( FanCoilNum ).UnitType + ": " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "... " + cNumericFields( 1 ) + " is greater than the maximum fan flow rate." );
						ShowContinueError( "... Fan Coil Unit flow = " + TrimSigDigits( FanCoil( FanCoilNum ).MaxAirVolFlow, 5 ) + " m3/s." );
						ShowContinueError( "... Fan = " + cFanTypes( FanCoil( FanCoilNum ).FanType_Num ) + ": " + FanCoil( FanCoilNum ).FanName );
						ShowContinueError( "... Fan flow = " + TrimSigDigits( FanCoil( FanCoilNum ).FanAirVolFlow, 5 ) + " m3/s." );
						ShowContinueError( "... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues." );
						FanCoil( FanCoilNum ).MaxAirVolFlow = FanCoil( FanCoilNum ).FanAirVolFlow;
					}

					// Check that the fan type match with the capacity control method selected
					if( ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_ConsFanVarFlow && ( FanCoil( FanCoilNum ).FanType_Num == FanType_SimpleVAV ) ) || ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_CycFan && FanCoil( FanCoilNum ).FanType_Num != FanType_SimpleOnOff ) || ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_VarFanVarFlow && FanCoil( FanCoilNum ).FanType_Num != FanType_SimpleVAV ) || ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_VarFanConsFlow && FanCoil( FanCoilNum ).FanType_Num != FanType_SimpleVAV ) || ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_ASHRAE && FanCoil( FanCoilNum ).FanType_Num == FanType_SimpleConstVolume ) ) {
						ShowSevereError( RoutineName + FanCoil( FanCoilNum ).UnitType + ": " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "...the fan type of the object : " + FanCoil( FanCoilNum ).FanName + " does not match with the capacity control method selected : " + FanCoil( FanCoilNum ).CapCtrlMeth + " please see I/O reference" );
						ShowContinueError( "...for ConstantFanVariableFlow a Fan:OnOff or Fan:ConstantVolume is valid." );
						ShowContinueError( "...for CyclingFan a Fan:OnOff is valid." );
						ShowContinueError( "...for VariableFanVariableFlow or VariableFanConstantFlow a Fan:VariableVolume is valid." );
						ShowContinueError( "...for ASHRAE90.1 a Fan:OnOff or Fan:VariableVolume is valid." );
						ErrorsFound = true;
					}

				} else {
					ShowSevereError( CurrentModuleObject + " = \"" + Alphas( 1 ) + "\"" );
					ShowContinueError( "Fan Type must be Fan:OnOff, Fan:ConstantVolume or Fan:VariableVolume." );
					ErrorsFound = true;
				}}
			}

			// check low speed fan ratio when using ASHRAE90.1 capacity control method
			if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_ASHRAE ) {
				if ( FanCoil( FanCoilNum ).LowSpeedRatio > 0.5 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"," );
					ShowContinueError( "... " + cNumericFields( 2 ) + " is greater than the 50% of the supply air flow ratio." );
					ShowContinueError( "... Fan Coil Unit low speed supply air flow ratio = " + TrimSigDigits( FanCoil( FanCoilNum ).LowSpeedRatio, 5 ) + ' ' );
				} else if( FanCoil( FanCoilNum ).LowSpeedRatio == 0.0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"," );
					ShowContinueError( "... " + cNumericFields( 2 ) + " is equal to 0." );
					ShowContinueError( "... Fan Coil Unit low speed supply air flow ratio should be greater than 0 to comply with ASHRAE90.1." );
					ShowContinueError( "... Fan Coil Unit low speed supply air flow ratio set to 0.5" );
					FanCoil( FanCoilNum ).LowSpeedRatio = 0.5;
				}
			}

			// Set defaults for convergence tolerance
			if ( FanCoil( FanCoilNum ).ColdControlOffset <= 0.0 ) {
				FanCoil( FanCoilNum ).ColdControlOffset = 0.001;
			}
			if ( FanCoil( FanCoilNum ).HotControlOffset <= 0.0 ) {
				FanCoil( FanCoilNum ).HotControlOffset = 0.001;
			}

			//check for inlet side air mixer
			GetATMixer( FanCoil( FanCoilNum ).Name, ATMixerName, ATMixerNum, ATMixerType, ATMixerPriNode, ATMixerSecNode, ATMixerOutNode );
			if ( ATMixerType == ATMixer_InletSide ) {
				// save the air terminal mixer data in the fan coil data array
				FanCoil( FanCoilNum ).ATMixerExists = true;
				FanCoil( FanCoilNum ).ATMixerIndex = ATMixerNum;
				FanCoil( FanCoilNum ).ATMixerName = ATMixerName;
				FanCoil( FanCoilNum ).ATMixerType = ATMixer_InletSide;
				FanCoil( FanCoilNum ).ATMixerPriNode = ATMixerPriNode;
				FanCoil( FanCoilNum ).ATMixerSecNode = ATMixerSecNode;
				FanCoil( FanCoilNum ).ATMixerOutNode = ATMixerOutNode;
				// check that fan coil doesn' have local outside air
				if ( ! lAlphaBlanks( 8 ) ) {
					ShowSevereError( CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\". Fan coil unit has local as well as central outdoor air specified" );
				}
				// check that the air teminal mixer out node is the fan coil inlet node
				if ( FanCoil( FanCoilNum ).AirInNode != ATMixerOutNode ) {
					ShowSevereError( CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\". Fan coil unit air inlet node name must be the same as an air terminal mixer outlet node name." );
					ShowContinueError( "..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:InletSideMixer object." );
					ShowContinueError( "..Fan coil unit air inlet node name = " + NodeID( FanCoil( FanCoilNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check for supply side air terminal mixer
			} else if ( ATMixerType == ATMixer_SupplySide ) {
				// save the air terminal mixer data in the fan coil data array
				FanCoil( FanCoilNum ).ATMixerExists = true;
				FanCoil( FanCoilNum ).ATMixerIndex = ATMixerNum;
				FanCoil( FanCoilNum ).ATMixerName = ATMixerName;
				FanCoil( FanCoilNum ).ATMixerType = ATMixer_SupplySide;
				FanCoil( FanCoilNum ).ATMixerPriNode = ATMixerPriNode;
				FanCoil( FanCoilNum ).ATMixerSecNode = ATMixerSecNode;
				FanCoil( FanCoilNum ).ATMixerOutNode = ATMixerOutNode;
				// check that fan coil doesn' have local outside air
				if ( ! lAlphaBlanks( 8 ) ) {
					ShowSevereError( CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\". Fan coil unit has local as well as central outdoor air specified" );
				}
				// check that the air teminal mixer secondary air inlet node is the fan coil outlet node
				if ( FanCoil( FanCoilNum ).AirOutNode != ATMixerSecNode ) {
					ShowSevereError( CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\". Fan coil unit air outlet node name must be the same as the air terminal mixer secondary air inlet node name." );
					ShowContinueError( "..Air terminal mixer secondary inlet node name is specified in AirTerminal:SingleDuct:SupplySideMixer object." );
					ShowContinueError( "..Fan coil unit air outlet node name = " + NodeID( FanCoil( FanCoilNum ).AirOutNode ) );
					ErrorsFound = true;
				}
				// no air terminal mixer; do the normal connectivity checks
			} else {
				// check that the fan coil inlet node is the same as one of the zone exhaust nodes
				ZoneExNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( FanCoil( FanCoilNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneExNodeNotFound = false;
						}
					}
				}
				if ( ZoneExNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\". Fan coil unit air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Fan coil unit air inlet node name = " + NodeID( FanCoil( FanCoilNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check that the fan coil outlet node is the same as one of the zone inlet nodes
				ZoneInNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( FanCoil( FanCoilNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							FanCoil( FanCoilNum ).ZonePtr = CtrlZone;
							ZoneInNodeNotFound = false;
						}
					}
				}
				if ( ZoneInNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " = \"" + FanCoil( FanCoilNum ).Name + "\". Fan coil unit air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Fan coil unit air outlet node name = " + NodeID( FanCoil( FanCoilNum ).AirOutNode ) );

					ErrorsFound = true;
				}
			}
			if ( FanCoil( FanCoilNum ).CapCtrlMeth == "MULTISPEEDFAN" ) {
				if ( !lAlphaBlanks( 17 ) ) {
					FanCoil( FanCoilNum ).FanOpModeSchedPtr = GetScheduleIndex( Alphas( 17 ) );
					if ( FanCoil( FanCoilNum ).FanType_Num != FanType_SimpleOnOff ) {
						ShowSevereError( CurrentModuleObject + " = " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "For " + cAlphaFields( 17 ) + " = " + Alphas( 17 ) );
						ShowContinueError( "Illegal " + cAlphaFields( 9 ) + " = " + Alphas( 9 ) );
						ShowContinueError( "...fan operating schedule is allowed for on off fan type only )" );
						ErrorsFound = true;
					} else {
						if ( FanCoil( FanCoilNum ).FanOpModeSchedPtr == 0 ) {
							ShowSevereError( CurrentModuleObject + " = " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "Illegal " + cAlphaFields( 17 ) + " = " + Alphas( 17 ) );
							ErrorsFound = true;
						}
					}
				} else {
					if ( FanCoil( FanCoilNum ).FanType_Num == FanType_SimpleOnOff ) {
						FanCoil( FanCoilNum ).FanOpMode = CycFanCycCoil;
					}
				}
			}

			if ( ! lNumericBlanks( 11 ) ) {
				FanCoil( FanCoilNum ).MinSATempCooling = Numbers( 11 );
			} else if ( ! lNumericBlanks( 12 ) ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"," );
				ShowContinueError( "... " + cNumericFields( 11 ) + " and " + cNumericFields( 12 ) + " must be used in unison." );
				ErrorsFound = true;
			}

			if ( ! lNumericBlanks( 12 ) ) {
				FanCoil( FanCoilNum ).MaxSATempHeating = Numbers( 12 );
				if ( FanCoil( FanCoilNum ).MinSATempCooling != AutoSize && FanCoil( FanCoilNum ).MaxSATempHeating != AutoSize ) {
					if ( FanCoil( FanCoilNum ).MaxSATempHeating < FanCoil( FanCoilNum ).MinSATempCooling ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"," );
						ShowContinueError( "... " + cNumericFields( 11 ) + " is greater than " + cNumericFields( 12 ) + "." );
						ShowContinueError( "... " + cNumericFields( 11 ) + " = " + TrimSigDigits( FanCoil( FanCoilNum ).MinSATempCooling, 2 ) + " [C]." );
						ShowContinueError( "... " + cNumericFields( 12 ) + " = " + TrimSigDigits( FanCoil( FanCoilNum ).MaxSATempHeating, 2 ) + " [C]." );
						ErrorsFound = true;
					}
				}
			} else if( ! lNumericBlanks( 11 ) ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + FanCoil( FanCoilNum ).Name + "\"," );
				ShowContinueError( "... " + cNumericFields( 11 ) + " and " + cNumericFields( 12 ) + " must be used in unison." );
				ErrorsFound = true;
			}

			if ( FanCoil( FanCoilNum ).MinSATempCooling > 0.0 && FanCoil( FanCoilNum ).MaxSATempHeating > 0.0 ) {
				FanCoil( FanCoilNum ).ASHRAETempControl = true;
			} else if ( FanCoil( FanCoilNum ).MinSATempCooling == AutoSize || FanCoil( FanCoilNum ).MaxSATempHeating == AutoSize ) {
				FanCoil( FanCoilNum ).ASHRAETempControl = true;
			}

			// Set up component set for supply fan
			if ( FanCoil( FanCoilNum ).OutsideAirNode > 0 ) {
				SetUpCompSets( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).FanType, FanCoil( FanCoilNum ).FanName, NodeID( FanCoil( FanCoilNum ).MixedAirNode ), "UNDEFINED" );
			} else {
				SetUpCompSets( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).FanType, FanCoil( FanCoilNum ).FanName, NodeID( FanCoil( FanCoilNum ).AirInNode ), "UNDEFINED" );
			}
			// Set up component set for cooling coil
			SetUpCompSets( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, "UNDEFINED", "UNDEFINED" );

			// Set up component set for heating coil
			SetUpCompSets( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).HCoilType, FanCoil( FanCoilNum ).HCoilName, "UNDEFINED", NodeID( FanCoil( FanCoilNum ).AirOutNode ) );

			// Set up component set for OA mixer - use OA node and Mixed air node
			if ( FanCoil( FanCoilNum ).OutsideAirNode > 0 ) {
				SetUpCompSets( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).OAMixType, FanCoil( FanCoilNum ).OAMixName, NodeID( FanCoil( FanCoilNum ).OutsideAirNode ), NodeID( FanCoil( FanCoilNum ).MixedAirNode ) );
			}
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input. Preceding condition(s) cause termination." );
		}

		for ( FanCoilNum = 1; FanCoilNum <= NumFanCoils; ++FanCoilNum ) {
			// Setup Report variables for the Fan Coils
			// CurrentModuleObject='ZoneHVAC:FourPipeFanCoil'
			SetupOutputVariable( "Fan Coil Heating Rate [W]", FanCoil( FanCoilNum ).HeatPower, "System", "Average", FanCoil( FanCoilNum ).Name );
			SetupOutputVariable( "Fan Coil Heating Energy [J]", FanCoil( FanCoilNum ).HeatEnergy, "System", "Sum", FanCoil( FanCoilNum ).Name );
			SetupOutputVariable( "Fan Coil Total Cooling Rate [W]", FanCoil( FanCoilNum ).TotCoolPower, "System", "Average", FanCoil( FanCoilNum ).Name );
			SetupOutputVariable( "Fan Coil Total Cooling Energy [J]", FanCoil( FanCoilNum ).TotCoolEnergy, "System", "Sum", FanCoil( FanCoilNum ).Name );
			SetupOutputVariable( "Fan Coil Sensible Cooling Rate [W]", FanCoil( FanCoilNum ).SensCoolPower, "System", "Average", FanCoil( FanCoilNum ).Name );
			SetupOutputVariable( "Fan Coil Sensible Cooling Energy [J]", FanCoil( FanCoilNum ).SensCoolEnergy, "System", "Sum", FanCoil( FanCoilNum ).Name );
			SetupOutputVariable( "Fan Coil Fan Electric Power [W]", FanCoil( FanCoilNum ).ElecPower, "System", "Average", FanCoil( FanCoilNum ).Name );
			SetupOutputVariable( "Fan Coil Fan Electric Energy [J]", FanCoil( FanCoilNum ).ElecEnergy, "System", "Sum", FanCoil( FanCoilNum ).Name );
			if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_CycFan || FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_MultiSpeedFan ) {
				SetupOutputVariable( "Fan Coil Runtime Fraction []", FanCoil( FanCoilNum ).PLR, "System", "Average", FanCoil( FanCoilNum ).Name );
				SetupOutputVariable( "Fan Coil Fan Speed Level []", FanCoil( FanCoilNum ).SpeedFanSel, "System", "Average", FanCoil( FanCoilNum ).Name );
				if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_MultiSpeedFan ) {
					SetupOutputVariable( "Fan Coil Speed Ratio []", FanCoil( FanCoilNum ).SpeedRatio, "System", "Average", FanCoil( FanCoilNum ).Name );
					SetupOutputVariable( "Fan Coil Part Load Ratio []", FanCoil( FanCoilNum ).PLR, "System", "Average", FanCoil( FanCoilNum ).Name );
				}
			}
			if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_VarFanVarFlow || FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_VarFanConsFlow ) {
				SetupOutputVariable( "Fan Coil Part Load Ratio []", FanCoil( FanCoilNum ).PLR, "System", "Average", FanCoil( FanCoilNum ).Name );
			}
			SetupOutputVariable( "Fan Coil Availability Status []", FanCoil( FanCoilNum ).AvailStatus, "System", "Average", FanCoil( FanCoilNum ).Name );
		}

	}

	void
	InitFanCoilUnits(
		int const FanCoilNum, // number of the current fan coil unit being simulated
		int const ZoneNum // number of zone being served
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2000
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Fan Coil Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::FanCoil4Pipe_Num;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using DataHVACGlobals::ZoneComp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitFanCoilUnits" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // inlet node number in fan coil loop
		int OutNode; // outlet node number in fan coil loop
		int InletNode; // inlet node number for fan coil FanCoilNum
		int HotConNode; // hot water control node number in fan coil loop
		int ColdConNode; // hot water control node number in fan coil loop
		int OutsideAirNode; // outside air node number in fan coil loop
		int AirRelNode; // relief air node number in fan coil loop
		Real64 RhoAir; // air density at InNode
		int Loop;
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		Real64 rho;
		bool errFlag;

		// FLOW:

		// Do the one time initializations
		if ( InitFanCoilUnitsOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumFanCoils );
			MySizeFlag.allocate( NumFanCoils );
			MyPlantScanFlag.allocate( NumFanCoils );
			MyZoneEqFlag.allocate ( NumFanCoils );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;
			MyZoneEqFlag = true;
			InitFanCoilUnitsOneTimeFlag = false;

		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( FanCoilNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( FanCoil4Pipe_Num ).ZoneCompAvailMgrs( FanCoilNum ).AvailManagerListName = FanCoil( FanCoilNum ).AvailManagerListName;
				ZoneComp( FanCoil4Pipe_Num ).ZoneCompAvailMgrs( FanCoilNum ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( FanCoilNum ) = false;
			}
			FanCoil( FanCoilNum ).AvailStatus = ZoneComp( FanCoil4Pipe_Num ).ZoneCompAvailMgrs( FanCoilNum ).AvailStatus;
		}

		if ( MyPlantScanFlag( FanCoilNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				ScanPlantLoopsForObject( FanCoil( FanCoilNum ).HCoilName, FanCoil( FanCoilNum ).HCoilPlantTypeOfNum, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum, _, _, _, _, _, errFlag );

				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + FanCoil( FanCoilNum ).Name + "\", type=" + FanCoil( FanCoilNum ).UnitType );
					ShowFatalError( "InitFanCoilUnits: Program terminated for previous conditions." );
				}

				FanCoil( FanCoilNum ).HotPlantOutletNode = PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).LoopSide( FanCoil( FanCoilNum ).HWLoopSide ).Branch( FanCoil( FanCoilNum ).HWBranchNum ).Comp( FanCoil( FanCoilNum ).HWCompNum ).NodeNumOut;

			} else if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Electric ) {
				// do nothing, valid type
			} else {
				ShowFatalError( "InitFanCoilUnits: FanCoil=" + FanCoil( FanCoilNum ).Name + ", invalid heating coil type. Program terminated." );
			}

			if ( ( FanCoil( FanCoilNum ).CCoilPlantTypeOfNum == TypeOf_CoilWaterCooling ) || ( FanCoil( FanCoilNum ).CCoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling ) ) {
				ScanPlantLoopsForObject( FanCoil( FanCoilNum ).CCoilPlantName, FanCoil( FanCoilNum ).CCoilPlantTypeOfNum, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + FanCoil( FanCoilNum ).Name + "\", type=" + FanCoil( FanCoilNum ).UnitType );
					ShowFatalError( "InitFanCoilUnits: Program terminated for previous conditions." );
				}
				FanCoil( FanCoilNum ).ColdPlantOutletNode = PlantLoop( FanCoil( FanCoilNum ).CWLoopNum ).LoopSide( FanCoil( FanCoilNum ).CWLoopSide ).Branch( FanCoil( FanCoilNum ).CWBranchNum ).Comp( FanCoil( FanCoilNum ).CWCompNum ).NodeNumOut;
			} else {
				ShowFatalError( "InitFanCoilUnits: FanCoil=" + FanCoil( FanCoilNum ).Name + ", invalid cooling coil type. Program terminated." );
			}

			MyPlantScanFlag( FanCoilNum ) = false;
		}

		if ( ! InitFanCoilUnitsCheckInZoneEquipmentListFlag && ZoneEquipInputsFilled ) {
			InitFanCoilUnitsCheckInZoneEquipmentListFlag = true;
			for ( Loop = 1; Loop <= NumFanCoils; ++Loop ) {
				if ( CheckZoneEquipmentList( FanCoil( Loop ).UnitType, FanCoil( Loop ).Name ) ) continue;
				ShowSevereError( "InitFanCoil: FanCoil Unit=[" + FanCoil( Loop ).UnitType + ',' + FanCoil( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( FanCoilNum ) && ! MyPlantScanFlag( FanCoilNum ) ) {

			SizeFanCoilUnit( FanCoilNum );

			MySizeFlag( FanCoilNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( FanCoilNum ) && ! MyPlantScanFlag( FanCoilNum ) ) {
			InNode = FanCoil( FanCoilNum ).AirInNode;
			OutNode = FanCoil( FanCoilNum ).AirOutNode;
			OutsideAirNode = FanCoil( FanCoilNum ).OutsideAirNode;
			RhoAir = StdRhoAir;
			HotConNode = FanCoil( FanCoilNum ).HotControlNode;
			ColdConNode = FanCoil( FanCoilNum ).ColdControlNode;
			// set the mass flow rates from the input volume flow rates
			FanCoil( FanCoilNum ).MaxAirMassFlow = RhoAir * FanCoil( FanCoilNum ).MaxAirVolFlow;
			FanCoil( FanCoilNum ).OutAirMassFlow = RhoAir * FanCoil( FanCoilNum ).OutAirVolFlow;

			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				rho = GetDensityGlycol( PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).FluidIndex, RoutineName );
				FanCoil( FanCoilNum ).MaxHotWaterFlow = rho * FanCoil( FanCoilNum ).MaxHotWaterVolFlow;
				FanCoil( FanCoilNum ).MinHotWaterFlow = rho * FanCoil( FanCoilNum ).MinHotWaterVolFlow;
			}

			rho = GetDensityGlycol( PlantLoop( FanCoil( FanCoilNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( FanCoil( FanCoilNum ).CWLoopNum ).FluidIndex, RoutineName );
			FanCoil( FanCoilNum ).MaxColdWaterFlow = rho * FanCoil( FanCoilNum ).MaxColdWaterVolFlow;
			FanCoil( FanCoilNum ).MinColdWaterFlow = rho * FanCoil( FanCoilNum ).MinColdWaterVolFlow;

			// set the node max and min mass flow rates
			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				InitComponentNodes( FanCoil( FanCoilNum ).MinHotWaterFlow, FanCoil( FanCoilNum ).MaxHotWaterFlow, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
			}

			InitComponentNodes( FanCoil( FanCoilNum ).MinColdWaterFlow, FanCoil( FanCoilNum ).MaxColdWaterFlow, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
			//  Node(HotConNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxHotWaterFlow
			//  Node(HotConNode)%MassFlowRateMin = FanCoil(FanCoilNum)%MinHotWaterFlow
			//  Node(ColdConNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxColdWaterFlow
			//  Node(ColdConNode)%MassFlowRateMin = FanCoil(FanCoilNum)%MinColdWaterFlow

			if ( FanCoil( FanCoilNum ).OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRateMax = FanCoil( FanCoilNum ).OutAirMassFlow;
				Node( OutsideAirNode ).MassFlowRateMin = 0.0;
			}
			Node( OutNode ).MassFlowRateMax = FanCoil( FanCoilNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMin = 0.0;
			Node( InNode ).MassFlowRateMax = FanCoil( FanCoilNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMin = 0.0;
			MyEnvrnFlag( FanCoilNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( FanCoilNum ) = true;
		}

		// These initializations are done every iteration
		InletNode = FanCoil( FanCoilNum ).AirInNode;
		OutsideAirNode = FanCoil( FanCoilNum ).OutsideAirNode;
		AirRelNode = FanCoil( FanCoilNum ).AirReliefNode;
		FanCoil( FanCoilNum ).SpeedRatio = 0.0;
		if ( FanCoil( FanCoilNum ).FanOpModeSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( FanCoil( FanCoilNum ).FanOpModeSchedPtr ) == 0.0 ) {
				FanCoil( FanCoilNum ).FanOpMode = CycFanCycCoil;
			} else {
				FanCoil( FanCoilNum ).FanOpMode = ContFanCycCoil;
			}
		}
		// Set the inlet node mass flow rate
		if ( GetCurrentScheduleValue( FanCoil( FanCoilNum ).SchedPtr ) > 0.0 ) {
			Node( InletNode ).MassFlowRate = FanCoil( FanCoilNum ).MaxAirMassFlow;
			Node( InletNode ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRate;
			Node( InletNode ).MassFlowRateMinAvail = Node( InletNode ).MassFlowRate;

			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRate = FanCoil( FanCoilNum ).OutAirMassFlow;
				Node( OutsideAirNode ).MassFlowRateMaxAvail = FanCoil( FanCoilNum ).OutAirMassFlow;
				Node( OutsideAirNode ).MassFlowRateMinAvail = FanCoil( FanCoilNum ).OutAirMassFlow;
				Node( AirRelNode ).MassFlowRate = FanCoil( FanCoilNum ).OutAirMassFlow;
				Node( AirRelNode ).MassFlowRateMaxAvail = FanCoil( FanCoilNum ).OutAirMassFlow;
				Node( AirRelNode ).MassFlowRateMinAvail = FanCoil( FanCoilNum ).OutAirMassFlow;
			}

		} else {
			Node( InletNode ).MassFlowRate = 0.0;
			Node( InletNode ).MassFlowRateMaxAvail = 0.0;
			Node( InletNode ).MassFlowRateMinAvail = 0.0;
			if ( OutsideAirNode > 0 ) {
				Node( OutsideAirNode ).MassFlowRate = 0.0;
				Node( OutsideAirNode ).MassFlowRateMaxAvail = 0.0;
				Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
				Node( AirRelNode ).MassFlowRate = 0.0;
				Node( AirRelNode ).MassFlowRateMaxAvail = 0.0;
				Node( AirRelNode ).MassFlowRateMinAvail = 0.0;
			}
		}

	}

	void
	SizeFanCoilUnit( int const FanCoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Fan Coil Unit components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHFnTdbW;
		using Fans::SimulateFanComponents;
		using Fans::GetFanDesignVolumeFlowRate;
		using General::TrimSigDigits;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using HVACHXAssistedCoolingCoil::GetHXCoilType;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using General::RoundSigDigits;
		using DataHVACGlobals::SystemAirflowSizing;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::HeatingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHVACGlobals::ZoneCoolingLoadSizing;
		using DataHVACGlobals::ZoneHeatingLoadSizing;
		using DataHVACGlobals::MinSATempCoolingSizing;
		using DataHVACGlobals::MaxSATempHeatingSizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeFanCoilUnit: " ); // include trailing blank space
		static std::string const RoutineNameNoSpace( "SizeFanCoilUnit" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
		bool ErrorsFound; // TRUE if errors foind during sizing
		Real64 DesCoilLoad; // coil load used for sizing [W]
		static int CoilWaterInletNode( 0 );
		static int CoilWaterOutletNode( 0 );
		std::string CoolingCoilName;
		std::string CoolingCoilType;
		Real64 rho;
		Real64 Cp;
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		bool IsAutoSize; // Indicator to autosize for reporting
		Real64 MaxAirVolFlowDes; // Autosized max air flow for reporting
		Real64 MaxAirVolFlowUser; // Hardsized max air flow for reporting
		Real64 OutAirVolFlowDes; // Autosized outdoor air flow for reporting
		Real64 OutAirVolFlowUser; // Hardsized outdoor air flow for reporting
		Real64 MaxHotWaterVolFlowDes; // Autosized hot water flow for reporting
		Real64 MaxHotWaterVolFlowUser; // Hardsized hot water flow for reporting
		Real64 MaxColdWaterVolFlowDes; // Autosized cold water flow for reporting
		Real64 MaxColdWaterVolFlowUser; // Hardsized cold water flow for reporting
		Real64 CoolingAirVolFlowDes; // cooling supply air flow rate
		Real64 HeatingAirVolFlowDes; // heating supply air flow rate
		std::string CompName; // component name
		std::string	CompType; // component type
		std::string SizingString;  // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )
		bool SizingDesRunThisZone; // test for zone sizing

		PltSizCoolNum = 0;
		PltSizHeatNum = 0;
		ErrorsFound = false;
		IsAutoSize = false;
		MaxAirVolFlowDes = 0.0;
		MaxAirVolFlowUser = 0.0;
		OutAirVolFlowDes = 0.0;
		OutAirVolFlowUser = 0.0;
		MaxHotWaterVolFlowDes = 0.0;
		MaxHotWaterVolFlowUser = 0.0;
		MaxColdWaterVolFlowDes = 0.0;
		MaxColdWaterVolFlowUser = 0.0;

		CoolingAirVolFlowDes = 0.0;
		HeatingAirVolFlowDes = 0.0;
		ZoneHeatingOnlyFan = false;
		ZoneCoolingOnlyFan = false;
		DataScalableSizingON = false;
		DataScalableCapSizingON = false;

		DataFracOfAutosizedCoolingAirflow = 1.0;
		DataFracOfAutosizedHeatingAirflow = 1.0;
		DataFracOfAutosizedCoolingCapacity = 1.0;
		DataFracOfAutosizedHeatingCapacity = 1.0;

		CompType = FanCoil(FanCoilNum).UnitType;
		CompName = FanCoil(FanCoilNum).Name;
		DataZoneNumber = FanCoil(FanCoilNum).ZonePtr;

		if ( CurZoneEqNum > 0 ) {
			if (FanCoil( FanCoilNum ).HVACSizingIndex > 0) {

				zoneHVACIndex = FanCoil( FanCoilNum ).HVACSizingIndex;
				FieldNum = 1;
				PrintFlag = true;
				SizingString = FanCoilNumericFields( FanCoilNum ).FieldNames( FieldNum ) + " [m3/s]";
				if ( ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod > 0 ) {
					SizingMethod = CoolingAirflowSizing;
					SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
					ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
					if ( SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
						if ( SAFMethod == SupplyAirFlowRate ) {
							if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
								ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
								ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
							}
							TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						} else if ( SAFMethod == FlowPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow * Zone( DataZoneNumber ).FloorArea;
							TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
							DataScalableSizingON = true;
						} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ) {
							DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
							TempSize = AutoSize;
							DataScalableSizingON = true;
						} else {
							TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						}
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						CoolingAirVolFlowDes = TempSize;

					} else if ( SAFMethod == FlowPerCoolingCapacity ) {
						SizingMethod = CoolingCapacitySizing;
						TempSize = AutoSize;
						PrintFlag = false;
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						if ( ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod == FractionOfAutosizedCoolingCapacity ) {
							DataFracOfAutosizedCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
						}
						DataAutosizedCoolingCapacity = TempSize;
						DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						SizingMethod = CoolingAirflowSizing;
						PrintFlag = true;
						TempSize = AutoSize;
						DataScalableSizingON = true;
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						CoolingAirVolFlowDes = TempSize;
					}
				} else if ( ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod > 0 ) {
					// now do heating supply air flow rate sizing
					SizingMethod = HeatingAirflowSizing;
					SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
					ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
					if ( SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						if ( SAFMethod == SupplyAirFlowRate ) {
							if ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow > 0.0 ) {
								ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
								ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
							}
							TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
						} else if ( SAFMethod == FlowPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow * Zone( DataZoneNumber ).FloorArea;
							TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
							DataScalableSizingON = true;
						} else if ( SAFMethod == FractionOfAutosizedHeatingAirflow ) {
							DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
							TempSize = AutoSize;
							DataScalableSizingON = true;
						} else {
							TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
						}
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						HeatingAirVolFlowDes = TempSize;
					} else if ( SAFMethod == FlowPerHeatingCapacity ) {
						SizingMethod = HeatingCapacitySizing;
						TempSize = AutoSize;
						PrintFlag = false;
						DataScalableSizingON = true;
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						if ( ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod == FractionOfAutosizedHeatingCapacity ) {
							DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
						}
						DataAutosizedHeatingCapacity = TempSize;
						DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
						SizingMethod = HeatingAirflowSizing;
						PrintFlag = true;
						TempSize = AutoSize;
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						HeatingAirVolFlowDes = TempSize;
					}
				}

				if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow == AutoSize || ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow == AutoSize ) {
					IsAutoSize = true;
					FanCoil( FanCoilNum ).MaxAirVolFlow = AutoSize;
					MaxAirVolFlowDes = max(CoolingAirVolFlowDes, HeatingAirVolFlowDes);
				} else {
					FanCoil( FanCoilNum ).MaxAirVolFlow = max(CoolingAirVolFlowDes, HeatingAirVolFlowDes);
					MaxAirVolFlowDes = 0.0;
				}
			} else {
				//SizingString = "Supply Air Maximum Flow Rate [m3/s]";
				SizingMethod = SystemAirflowSizing;
				FieldNum = 1;
				SizingString = FanCoilNumericFields( FanCoilNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = FanCoil( FanCoilNum ).MaxAirVolFlow;
				PrintFlag = true;
				RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
				if ( FanCoil( FanCoilNum ).MaxAirVolFlow == AutoSize ) {
					IsAutoSize = true;
					MaxAirVolFlowDes = TempSize;
				} else {
					MaxAirVolFlowDes = 0.0;
				}

			}
		}

		if ( CurZoneEqNum > 0 ) {

			if ( !IsAutoSize && !ZoneSizingRunDone ) {

			} else {
				if ( MaxAirVolFlowDes < SmallAirVolFlow ) {
					MaxAirVolFlowDes = 0.0;
				}

				//     If fan is autosized, get fan volumetric flow rate
				if ( FanCoil( FanCoilNum ).FanAirVolFlow == AutoSize ) {
					SimulateFanComponents( FanCoil( FanCoilNum ).FanName, true, FanCoil( FanCoilNum ).FanIndex );
					FanCoil( FanCoilNum ).FanAirVolFlow = GetFanDesignVolumeFlowRate( cFanTypes( FanCoil( FanCoilNum ).FanType_Num ), FanCoil( FanCoilNum ).FanName, ErrorsFound );
				}
				//     Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
				if ( MaxAirVolFlowDes > FanCoil( FanCoilNum ).FanAirVolFlow ) {
					ShowWarningError( RoutineName + FanCoil( FanCoilNum ).UnitType + ": " + FanCoil( FanCoilNum ).Name );
					ShowContinueError( "... Maximum supply air flow rate is greater than the maximum fan flow rate." );
					ShowContinueError( "... Fan Coil Unit flow = " + TrimSigDigits( MaxAirVolFlowDes, 5 ) + " [m3/s]." );
					ShowContinueError( "... Fan = " + cFanTypes( FanCoil( FanCoilNum ).FanType_Num ) + ": " + FanCoil( FanCoilNum ).FanName );
					ShowContinueError( "... Fan flow = " + TrimSigDigits( FanCoil( FanCoilNum ).FanAirVolFlow, 5 ) + " [m3/s]." );
					ShowContinueError( "... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues." );
					MaxAirVolFlowDes = FanCoil( FanCoilNum ).FanAirVolFlow;
				}

				if ( IsAutoSize ) {
					FanCoil( FanCoilNum ).MaxAirVolFlow = MaxAirVolFlowDes;
				} else { // Hard size with sizing data
					if ( FanCoil( FanCoilNum ).MaxAirVolFlow > 0.0 && MaxAirVolFlowDes > 0.0 ) {
						MaxAirVolFlowUser = FanCoil( FanCoilNum ).MaxAirVolFlow;
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxAirVolFlowDes - MaxAirVolFlowUser ) / MaxAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil( FanCoilNum ).UnitType + ' ' + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "User-Specified Supply Air Maximum Flow Rate of " + RoundSigDigits( MaxAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Supply Air Maximum Flow Rate of " + RoundSigDigits( MaxAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		} else if ( FanCoil( FanCoilNum ).FanAirVolFlow == AutoSize ) {
			SimulateFanComponents( FanCoil( FanCoilNum ).FanName, true, FanCoil( FanCoilNum ).FanIndex );
			FanCoil( FanCoilNum ).FanAirVolFlow = GetFanDesignVolumeFlowRate( cFanTypes( FanCoil( FanCoilNum ).FanType_Num ), FanCoil( FanCoilNum ).FanName, ErrorsFound );
			//   Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
			if  ( FanCoil( FanCoilNum ).MaxAirVolFlow > FanCoil( FanCoilNum ).FanAirVolFlow ) {
				ShowWarningError( RoutineName + FanCoil (FanCoilNum ).UnitType + ": " + FanCoil( FanCoilNum ).Name );
				ShowContinueError( "... Maximum supply air flow rate is greater than the maximum fan flow rate." );
				ShowContinueError( "... Fan Coil Unit flow = " + TrimSigDigits( FanCoil( FanCoilNum ).MaxAirVolFlow, 5 ) + " m3/s." );
				ShowContinueError( "... Fan = " + cFanTypes( FanCoil( FanCoilNum ).FanType_Num ) + ": " + FanCoil( FanCoilNum ).FanName );
				ShowContinueError( "... Fan flow = " + TrimSigDigits( FanCoil( FanCoilNum ).FanAirVolFlow, 5 ) + " m3/s." );
				ShowContinueError( "... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues." );
				FanCoil( FanCoilNum ).MaxAirVolFlow = FanCoil( FanCoilNum ).FanAirVolFlow;
			}
		}


		IsAutoSize = false;
		if ( FanCoil( FanCoilNum ).OutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}

		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) {
				if ( FanCoil( FanCoilNum ).OutAirVolFlow > 0.0 ) {
					ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "User-Specified Maximum Outdoor Air Flow Rate [m3/s]", FanCoil( FanCoilNum ).OutAirVolFlow );
				}
			} else {
				CheckZoneSizing( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name );
				OutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, FanCoil( FanCoilNum ).MaxAirVolFlow );
				if ( OutAirVolFlowDes < SmallAirVolFlow ) {
					OutAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					FanCoil( FanCoilNum ).OutAirVolFlow = OutAirVolFlowDes;
					ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "Design Size Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes );
				} else {
					if ( FanCoil( FanCoilNum ).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0 ) {
						OutAirVolFlowUser = FanCoil( FanCoilNum ).OutAirVolFlow;
						ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "Design Size Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes, "User-Specified Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( OutAirVolFlowDes - OutAirVolFlowUser ) / OutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil( FanCoilNum ).UnitType + ' ' + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "User-Specified Maximum Outdoor Air Flow Rate of " + RoundSigDigits( OutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Outdoor Air Flow Rate of " + RoundSigDigits( OutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {

			IsAutoSize = false;
			if ( FanCoil( FanCoilNum ).MaxHotWaterVolFlow == AutoSize ) {
				IsAutoSize = true;
			}

			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) {
					if ( FanCoil( FanCoilNum ).MaxHotWaterVolFlow > 0.0 ) {
						ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "User-Specified Maximum Hot Water Flow [m3/s]", FanCoil( FanCoilNum ).MaxHotWaterVolFlow );
					}
				} else {
					CoilWaterInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", FanCoil( FanCoilNum ).HCoilName, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( "Coil:Heating:Water", FanCoil( FanCoilNum ).HCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", FanCoil( FanCoilNum ).HCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {
							SizingMethod = HeatingCapacitySizing;
							if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow > 0.0 ) {
								FinalZoneSizing( CurZoneEqNum ).DesHeatOAFlowFrac = min( FanCoil( FanCoilNum ).OutAirVolFlow / FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow, 1.0 );
							} else {
								FinalZoneSizing( CurZoneEqNum ).DesHeatOAFlowFrac = 0.0;
							}
							if ( FanCoil( FanCoilNum ).HVACSizingIndex > 0 ) {
								zoneHVACIndex = FanCoil( FanCoilNum ).HVACSizingIndex;
								CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
								ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
								if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
									if ( CapSizingMethod == HeatingDesignCapacity ) {
										if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0 ) {
											ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										}
										TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
									} else if ( CapSizingMethod == CapacityPerFloorArea ) {
										if ( ZoneSizingRunDone ) {
											PrintFlag = false;
											TempSize = AutoSize;
											DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = TempSize;
											ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
										}
										TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
										DataScalableCapSizingON = true;
									} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
										CheckZoneSizing( CompType, CompName );
										PrintFlag = false;
										TempSize = AutoSize;
										DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
										RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
										ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = TempSize;
										ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
										TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad * ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										DataScalableCapSizingON = true;
									}
								}
								SizingString = "Heating Design Capacity [W]";
								PrintFlag = false;
								RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
								DesCoilLoad = TempSize;
								DataScalableCapSizingON = false;
								DataFlowUsedForSizing = 0.0;

							} else {
								SizingString = "Heating Design Capacity [W]";
								PrintFlag = false;
								TempSize = AutoSize;
								RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
								DesCoilLoad = TempSize;
							}
							FanCoil( FanCoilNum ).DesHeatingLoad = DesCoilLoad;
							if ( DesCoilLoad >= SmallLoad ) {
								rho = GetDensityGlycol( PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).FluidIndex, RoutineNameNoSpace );
								Cp = GetSpecificHeatGlycol( PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).FluidIndex, RoutineNameNoSpace );

								MaxHotWaterVolFlowDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
							} else {
								MaxHotWaterVolFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in " + FanCoil( FanCoilNum ).UnitType + " Object=" + FanCoil( FanCoilNum ).Name );
							ErrorsFound = true;
						}
					}
				}

				if ( IsAutoSize ) {
					FanCoil( FanCoilNum ).MaxHotWaterVolFlow = MaxHotWaterVolFlowDes;
					ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxHotWaterVolFlowDes );
				} else { // Hard size with sizing data
					if ( FanCoil( FanCoilNum ).MaxHotWaterVolFlow > 0.0 && MaxHotWaterVolFlowDes > 0.0 ) {
						MaxHotWaterVolFlowDes = FanCoil( FanCoilNum ).MaxHotWaterVolFlow;
						ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxHotWaterVolFlowDes, "User-Specified Maximum Hot Water Flow [m3/s]", MaxHotWaterVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( (std::abs (MaxHotWaterVolFlowDes - MaxHotWaterVolFlowUser ) / MaxHotWaterVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil( FanCoilNum ).UnitType + ' ' + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "User-Specified Maximum Hot Water Flow of " + RoundSigDigits( MaxHotWaterVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Hot Water Flow of " + RoundSigDigits( MaxHotWaterVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		} else if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Electric ) {
			if ( FanCoil( FanCoilNum ).DesignHeatingCapacity == AutoSize ) {
				CompName = FanCoil( FanCoilNum ).HCoilName;
				CompType = FanCoil( FanCoilNum ).HCoilType;
				SizingMethod = HeatingCapacitySizing;
				PrintFlag = false;
				TempSize = FanCoil( FanCoilNum ).DesignHeatingCapacity;
				SizingString = "Nominal Heating Capacity [W]";
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				FanCoil( FanCoilNum ).DesignHeatingCapacity = TempSize;
				FanCoil( FanCoilNum ).DesHeatingLoad = FanCoil( FanCoilNum ).DesignHeatingCapacity;
			}
		}

		IsAutoSize = false;
		if ( FanCoil( FanCoilNum ).MaxColdWaterVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( !IsAutoSize && !ZoneSizingRunDone ) {
				if ( FanCoil( FanCoilNum ).MaxColdWaterVolFlow > 0.0 ) {
					ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "User-Specified Maximum Cold Water Flow [m3/s]", FanCoil( FanCoilNum ).MaxColdWaterVolFlow );
				}
			} else {
				if ( SameString( FanCoil( FanCoilNum ).CCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
					CoolingCoilName = GetHXDXCoilName( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, ErrorsFound );
					CoolingCoilType = GetHXCoilType( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, ErrorsFound );
				} else {
					CoolingCoilName = FanCoil( FanCoilNum ).CCoilName;
					CoolingCoilType = FanCoil( FanCoilNum ).CCoilType;
				}
				CoilWaterInletNode = GetCoilWaterInletNode( CoolingCoilType, CoolingCoilName, ErrorsFound );
				CoilWaterOutletNode = GetCoilWaterOutletNode( CoolingCoilType, CoolingCoilName, ErrorsFound );
				if ( IsAutoSize ) {
					PltSizCoolNum = MyPlantSizingIndex( CoolingCoilType, CoolingCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
					if ( PltSizCoolNum > 0 ) {
						SizingMethod = CoolingCapacitySizing;
						if ( FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow > 0.0 ) {
							FinalZoneSizing( CurZoneEqNum ).DesCoolOAFlowFrac = min( FanCoil( FanCoilNum ).OutAirVolFlow / FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow, 1.0 );
						} else {
							FinalZoneSizing( CurZoneEqNum ).DesCoolOAFlowFrac = 0.0;
						}
						if (FanCoil( FanCoilNum ).HVACSizingIndex > 0) {
							zoneHVACIndex = FanCoil( FanCoilNum ).HVACSizingIndex;
							CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod;
							ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
							if ( CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
								if ( CapSizingMethod == CoolingDesignCapacity ) {
									if ( ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity > 0.0 ) {
										ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
										ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
									} else {
										DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
									}
									TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
								} else if ( CapSizingMethod == CapacityPerFloorArea ) {
									if ( ZoneSizingRunDone ) {
										CheckZoneSizing( CompType, CompName );
										PrintFlag = false;
										TempSize = AutoSize;
										DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
										RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
										ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = TempSize;
										ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
									}
									TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity * Zone( DataZoneNumber ).FloorArea;
									DataScalableCapSizingON = true;
								} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
									PrintFlag = false;
									TempSize = AutoSize;
									DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = TempSize;
									ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
									TempSize = ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad * ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
									DataScalableCapSizingON = true;
								}
							}
							SizingString = "Cooling Design Capacity [W]";
							PrintFlag = false;
							RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
							DesCoilLoad = TempSize;
							DataScalableCapSizingON = false;
							DataFlowUsedForSizing = 0.0;
						} else {
							SizingString = "Cooling Design Capacity [W]";
							PrintFlag = false;
							TempSize = AutoSize;
							DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
							RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
							DesCoilLoad = TempSize;
						}
						FanCoil(FanCoilNum).DesCoolingLoad = DesCoilLoad;
						if ( DesCoilLoad >= SmallLoad ) {
							rho = GetDensityGlycol( PlantLoop( FanCoil( FanCoilNum ).CWLoopNum ).FluidName, 5., PlantLoop( FanCoil( FanCoilNum ).CWLoopNum ).FluidIndex, RoutineNameNoSpace );
							Cp = GetSpecificHeatGlycol(PlantLoop(FanCoil(FanCoilNum).CWLoopNum).FluidName, 5., PlantLoop(FanCoil(FanCoilNum).CWLoopNum).FluidIndex, RoutineNameNoSpace );
							MaxColdWaterVolFlowDes = DesCoilLoad / ( PlantSizData( PltSizCoolNum ).DeltaT * Cp * rho );
						} else {
							MaxColdWaterVolFlowDes = 0.0;
						}
					} else {
						ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
						ShowContinueError( "Occurs in " + FanCoil( FanCoilNum ).UnitType + " Object=" + FanCoil( FanCoilNum ).Name );
						ErrorsFound = true;
					}
				}
				if ( IsAutoSize ) {
					FanCoil( FanCoilNum ).MaxColdWaterVolFlow = MaxColdWaterVolFlowDes;
					ReportSizingOutput (FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "Design Size Maximum Cold Water Flow [m3/s]", MaxColdWaterVolFlowDes );
				} else { // Hard size with sizing data
					if ( FanCoil( FanCoilNum ).MaxColdWaterVolFlow > 0.0 && MaxColdWaterVolFlowDes > 0.0 ) {
						MaxColdWaterVolFlowUser = FanCoil( FanCoilNum ).MaxColdWaterVolFlow;
						ReportSizingOutput( FanCoil( FanCoilNum ).UnitType, FanCoil( FanCoilNum ).Name, "Design Size Maximum Cold Water Flow [m3/s]", MaxColdWaterVolFlowDes, "User-Specified Maximum Cold Water Flow [m3/s]", MaxColdWaterVolFlowUser );
						if (DisplayExtraWarnings) {
							if ( ( std::abs( MaxColdWaterVolFlowDes - MaxColdWaterVolFlowUser ) / MaxColdWaterVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeFanCoilUnit: Potential issue with equipment sizing for " + FanCoil( FanCoilNum ).UnitType + ' ' + FanCoil ( FanCoilNum ).Name );
								ShowContinueError( "User-Specified Maximum Cold Water Flow of " + RoundSigDigits( MaxColdWaterVolFlowUser, 5 ) + "[m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Cold Water Flow of " + RoundSigDigits( MaxColdWaterVolFlowDes, 5 ) + "[m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}

			if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_ASHRAE && !FanCoil( FanCoilNum ).ASHRAETempControl ) {

				CompType = FanCoil( FanCoilNum ).UnitType;
				CompName = FanCoil( FanCoilNum ).Name;
				SizingMethod = ZoneCoolingLoadSizing;
				PrintFlag = true;
				SizingString = "Zone Cooling Sensible Load [W]";
				RequestSizing( CompType, CompName, SizingMethod, SizingString, FanCoil( FanCoilNum ).DesZoneCoolingLoad, PrintFlag, RoutineName );
				FanCoil( FanCoilNum ).DesZoneCoolingLoad *= -1.0;

				SizingMethod = ZoneHeatingLoadSizing;
				SizingString = "Zone Heating Sensible Load [W]";
				RequestSizing( CompType, CompName, SizingMethod, SizingString, FanCoil( FanCoilNum ).DesZoneHeatingLoad, PrintFlag, RoutineName );

				FanCoil( FanCoilNum ).DSOAPtr = FinalZoneSizing( CurZoneEqNum ).ZoneDesignSpecOAIndex;

			} else if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_ASHRAE && FanCoil( FanCoilNum ).ASHRAETempControl ) {

				CompType = FanCoil( FanCoilNum ).UnitType;
				CompName = FanCoil( FanCoilNum ).Name;
				SizingMethod = MinSATempCoolingSizing;
				DataCapacityUsedForSizing = FanCoil( FanCoilNum ).DesCoolingLoad;
				DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
				PrintFlag = true;
				FieldNum = 11; // Minimum Supply Air Temperature in Cooling Mode
				SizingString = FanCoilNumericFields( FanCoilNum ).FieldNames( FieldNum ) + " [C]";
				RequestSizing( CompType, CompName, SizingMethod, SizingString, FanCoil( FanCoilNum ).MinSATempCooling, PrintFlag, RoutineName );

				SizingMethod = MaxSATempHeatingSizing;
				FieldNum = 12; // Maximum Supply Air Temperature in Heating Mode
				SizingString = FanCoilNumericFields( FanCoilNum ).FieldNames( FieldNum ) + " [C]";
				DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, FanCoil( FanCoilNum ).MaxSATempHeating, PrintFlag, RoutineName );

				DataCapacityUsedForSizing = 0.0; // reset so other routines don't use this inadvertently
				DataFlowUsedForSizing = 0.0;

				SizingDesRunThisZone = false;
				CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );

				if ( SizingDesRunThisZone ) {

					FanCoil( FanCoilNum ).DesZoneCoolingLoad = -1.0 * ( FanCoil( FanCoilNum ).DesCoolingLoad / FinalZoneSizing( CurZoneEqNum ).CoolSizingFactor );
					FanCoil( FanCoilNum ).DesZoneHeatingLoad = FanCoil( FanCoilNum ).DesHeatingLoad / FinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
					FanCoil( FanCoilNum ).DSOAPtr = FinalZoneSizing( CurZoneEqNum ).ZoneDesignSpecOAIndex;

				} else {

					FanCoil( FanCoilNum ).DesZoneCoolingLoad = -1.0 * FanCoil( FanCoilNum ).DesCoolingLoad;
					FanCoil( FanCoilNum ).DesZoneHeatingLoad = FanCoil( FanCoilNum ).DesHeatingLoad;

		}
			}

		} // if ( CurZoneEqNum > 0 )

		// set the design air flow rates for the heating and cooling coils
		if ( SameString( FanCoil( FanCoilNum ).CCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
			CoolingCoilName = GetHXDXCoilName( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, ErrorsFound );
			CoolingCoilType = GetHXCoilType( FanCoil( FanCoilNum ).CCoilType, FanCoil( FanCoilNum ).CCoilName, ErrorsFound );
		} else {
			CoolingCoilName = FanCoil( FanCoilNum ).CCoilName;
			CoolingCoilType = FanCoil( FanCoilNum ).CCoilType;
		}
		if ( ZoneSizingRunDone ) {
			SetCoilDesFlow( CoolingCoilType, CoolingCoilName, FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, ErrorsFound );
			SetCoilDesFlow( FanCoil( FanCoilNum ).HCoilType, FanCoil( FanCoilNum ).HCoilName, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow, ErrorsFound );
		} else {
			SetCoilDesFlow( CoolingCoilType, CoolingCoilName, FanCoil( FanCoilNum ).MaxAirVolFlow, ErrorsFound );
			SetCoilDesFlow( FanCoil( FanCoilNum ).HCoilType, FanCoil( FanCoilNum ).HCoilName, FanCoil( FanCoilNum ).MaxAirVolFlow, ErrorsFound );
		}
		if ( CurZoneEqNum > 0 ) {
			ZoneEqSizing( CurZoneEqNum ).MaxHWVolFlow = FanCoil( FanCoilNum ).MaxHotWaterVolFlow;
			ZoneEqSizing( CurZoneEqNum ).MaxCWVolFlow = FanCoil( FanCoilNum ).MaxColdWaterVolFlow;
			ZoneEqSizing( CurZoneEqNum ).OAVolFlow = FanCoil( FanCoilNum ).OutAirVolFlow;
			ZoneEqSizing( CurZoneEqNum ).AirVolFlow = FanCoil( FanCoilNum ).MaxAirVolFlow;
			ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = FanCoil( FanCoilNum ).DesCoolingLoad;
			ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = FanCoil( FanCoilNum ).DesHeatingLoad;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	Sim4PipeFanCoil(
		int & FanCoilNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		int const ControlledZoneNum, // index into ZoneEqupConfig
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       MODIFIED       Arnaud Flament June 2010 (added airflow capacity control methods)
		//       MODIFIED      R. Raustad, FSEC, Feb 2016 (added ASHRAE 90.1 SZVAV system control)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a 4 pipe fan coil unit; adjust its output to match the
		// remaining zone load.

		// METHODOLOGY EMPLOYED:
		// If unit is on, calls ControlCompOutput to obtain the desired unit output

		// REFERENCES:
		// SZVAV sysetm control:
		// ASHRAE 90.1 2010 Section 6.4.3.10 - Single Zone Variable-Air-volume Controls (described in Trane newsletter entitled Understanding Single-Zone VAV Systems)
		// Trane Engineers Newsletter - https://www.trane.com/content/dam/Trane/Commercial/global/products-systems/education-training/engineers-newsletters/airside-design/admapn047en_0413.pdf
		//

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataHeatBalFanSys::TempControlType;
		using General::TrimSigDigits;
		using PlantUtilities::SetComponentFlowRate;
		using General::SolveRegulaFalsi;
		using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
		using namespace DataPlant;
		using namespace DataLoopNode;
		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterCycl( 100 );

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QZnReq; // heating or cooling needed by zone [watts]
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 QUnitOutMax; // heating or sens. cooling provided by fan coil unit (running during an entire timestep)
		Real64 PLR; // Part Load Ratio, fraction of time step fancoil is on
		bool UnitOn; // TRUE if unit is on
		int ControlNode; // the hot water or cold water inlet node
		Real64 ControlOffset; // tolerance for output control
		Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/sec]
		Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/sec]
		int OutletNode; // unit air outlet node
		int InletNode; // unit air inlet node
		Real64 QTotUnitOut; // total unit output [watts]
		Real64 AirMassFlow; // air mass flow rate [kg/sec]
		Real64 QUnitOutNoHC; // unit output with no active heating or cooling [W]
		Real64 QUnitOutMaxC; // unit output with full active cooling [W]
		Real64 QUnitOutMaxH; // unit output with full active heating [W]
		Real64 QCoilHeatSP; // coil load to the heating setpoint [W]
		Real64 QCoilCoolSP; // coil load to the cooling setpoint [W]
		Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
		Real64 Error; // Error between QZnReq and QUnitOut
		Real64 AbsError; // Absolute error between QZnReq and QUnitOut [W]   !FB
		int Iter; // iteration counter
		Real64 Relax;
		Real64 DelPLR;
		Real64 mdot;
		Real64 Low_mdot;
		Real64 QSensUnitOutNoATM; // unit output not including air added by supply side air terminal mixer
		int SolFlag; // return flag from RegulaFalsi for sensible load
		Array1D< Real64 > Par( 10 ); // parameters passed to RegulaFalsi function
		Real64 ElectricHeaterControl; // 1 or 0, enables or disables heating coil
		Real64 OAVolumeFlowRate; // OA volume flow rate based on design specifications object [m3/s]
		Real64 OAMassFlow; // OA mass flow rate based on design specifications object [kg/s]
		Real64 RhoAir; // density of air [kg/m3]
		Real64 MinSAMassFlowRate; // minimum supply air mass flow rate [kg/s]
		Real64 MaxSAMassFlowRate; // maximum supply air mass flow rate [kg/s]
		Real64 FCOutletTempOn; // ASHRAE outlet air temperature when coil is on [C]
		Real64 HWFlow; // hot water mass flow rate solution [kg/s]		Real64 HWFlowBypass; // hot water bypassed mass flow rate [kg/s]
		Real64 MdotLockH; // saved value of locked chilled water mass flow rate [kg/s]
		Real64 MdotLockC; // saved value of locked hot water mass flow rate [kg/s]
		Real64 CWFlow; // cold water mass flow rate solution [kg/s]
		Real64 CWFlowBypass; // cold water bypassed mass flow rate [kg/s]
		Real64 HWFlowBypass; // hot water bypassed mass flow rate [kg/s]
		bool ColdFlowLocked; // if true cold water flow is locked
		bool HotFlowLocked; // if true Hot water flow is locked
		// FLOW
		FanElecPower = 0.0;
		// initialize local variables
		UnitOn = true;
		ControlNode = 0;
		QUnitOut = 0.0;
		QUnitOutMax = 0.0;
		PLR = 0.0;
		LatentOutput = 0.0;
		QUnitOutNoHC = 0.0;
		QCoilHeatSP = 0.0;
		QCoilCoolSP = 0.0;
		QZnReq = 0.0;
		ControlOffset = 0.0;
		MaxWaterFlow = 0.0;
		MinWaterFlow = 0.0;
		OutletNode = FanCoil( FanCoilNum ).AirOutNode;
		InletNode = FanCoil( FanCoilNum ).AirInNode;
		AirMassFlow = Node( InletNode ).MassFlowRate;
		Error = 1.0;
		AbsError = 2.0 * SmallLoad;
		Iter = 0;
		Relax = 1.0;
		ElectricHeaterControl = 0.0;
		HWFlow = 0.0;
		HWFlowBypass = 0.0;
		MdotLockH = 0.0;
		MdotLockC = 0.0;
		ColdFlowLocked = false;
		HotFlowLocked = false;

		// select capacity control method
		{ auto const SELECT_CASE_var( FanCoil( FanCoilNum ).CapCtrlMeth_Num );

		// constant fan variable flow
		if ( SELECT_CASE_var == CCM_ConsFanVarFlow ) {

			if ( AirMassFlow < SmallMassFlow ) UnitOn = false;
			// zero the hot & cold water flows
			//    Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = 0.0
			//    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = 0.0
			mdot = 0.0;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
			if ( PlantLoop( FanCoil( FanCoilNum ).CWLoopNum ).LoopSide( FanCoil( FanCoilNum ).CWLoopSide ).FlowLock == FlowLocked ) {
				ColdFlowLocked = true; // check for flow lock
			}
			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
				if ( PlantLoop( FanCoil( FanCoilNum ).HWLoopNum ).LoopSide( FanCoil( FanCoilNum ).HWLoopSide ).FlowLock == FlowLocked ) {
					HotFlowLocked = true; // save locked flow
				}
			}
			// obtain unit output with no active heating/cooling
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutNoHC, 0.0 );
			if ( FirstHVACIteration ) {
				FanCoil( FanCoilNum ).QUnitOutNoHC = QUnitOutNoHC;
			}
			else {
				QUnitOutNoHC = FanCoil( FanCoilNum ).QUnitOutNoHC;
			}
			// get the loads at the coils
			QCoilHeatSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP - QUnitOutNoHC;
			QCoilCoolSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP - QUnitOutNoHC;
			// if cooling
			if ( UnitOn && QCoilCoolSP < ( -1.0 * SmallLoad ) && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
				ControlNode = FanCoil( FanCoilNum ).ColdControlNode;
				ControlOffset = FanCoil( FanCoilNum ).ColdControlOffset;
				MaxWaterFlow = FanCoil( FanCoilNum ).MaxColdWaterFlow;
				MinWaterFlow = FanCoil( FanCoilNum ).MinColdWaterFlow;
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ! FirstHVACIteration ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				// get full load result
				mdot = MaxWaterFlow;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMaxC );
				if ( ! ColdFlowLocked ) {
					FanCoil( FanCoilNum ).QUnitOutMaxC = QUnitOutMaxC;
				} else {
					QUnitOutMaxC = FanCoil( FanCoilNum ).QUnitOutMaxC;
					MdotLockC = mdot; // save locked flow
				}
				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
				if ( QUnitOutMaxC < QZnReq ) {
					// more cooling than required, find reduced water flow rate to meet the load
					//solve for the cold water flow rate with no limit set by flow rate lockdown
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if ( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = QZnReq;
					SolveRegulaFalsi( 0.001, MaxIterCycl, SolFlag, CWFlow, CalcFanCoilCWLoadResidual, 0.0, MaxWaterFlow, Par );
					if ( SolFlag == -1 ) {
						++FanCoil( FanCoilNum ).ConvgErrCountC;
						if ( FanCoil( FanCoilNum ).ConvgErrCountC < 2 ) {
							ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
							ShowContinueErrorTimeStamp( "..Water flow rate set to last iteration value " );
						} else {
							ShowRecurringWarningErrorAtEnd( "Cold water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name,
								FanCoil( FanCoilNum ).MaxIterIndexC );
						}
					} else if ( SolFlag == -2 ) {
						++FanCoil( FanCoilNum ).LimitErrCountC;
						if ( FanCoil( FanCoilNum ).LimitErrCountC < 2 ) {
							ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Bad cold water mass flow limits" );
							ShowContinueErrorTimeStamp( "..Water flow rate set to lower limit " );
						} else{
							ShowRecurringWarningErrorAtEnd( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name,
								FanCoil( FanCoilNum ).BadMassFlowLimIndexC );
						}
					}
				} else {
					// demand greater than capacity
					CWFlow = MaxWaterFlow;
				}
				if ( ! ColdFlowLocked ) {
					mdot = CWFlow; // not flowlocked - set flow to CWFlow
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum,
						FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut ); // get QUnitOut
				}
				else {
					// flow lock on 
					if ( MdotLockC > CWFlow ) { // if mdot > CWFlow, bypass extra flow
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut ); // get QUnitOut with CWFlow; rest will be bypassed 
						Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate = MdotLockC; // reset flow to locked value. Since lock is on, must do this by hand
						Node( FanCoil( FanCoilNum ).ColdPlantOutletNode ).MassFlowRate = MdotLockC;
						// Keep soln flow rate but reset outlet water temperature - i.e. bypass extra water
						CWFlowBypass = MdotLockC - CWFlow;
						// change water outlet temperature and enthalpy
						Node( FanCoil( FanCoilNum ).ColdPlantOutletNode ).Temp = ( CWFlowBypass * Node( FanCoil( FanCoilNum ).ColdControlNode ).Temp +
							CWFlow * Node( FanCoil( FanCoilNum ).ColdPlantOutletNode ).Temp ) / MdotLockC;
						Node( FanCoil( FanCoilNum ).ColdPlantOutletNode ).Enthalpy = ( CWFlowBypass * Node( FanCoil( FanCoilNum ).ColdControlNode ).Enthalpy +
							CWFlow * Node( FanCoil( FanCoilNum ).ColdPlantOutletNode ).Enthalpy ) / MdotLockC;
					}
					else {
						// if MdotLockC <= HWFlow use MdotLockC as is
						Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate = MdotLockC; // reset flow to locked value. Since lock is on, must do this by hand
						Node( FanCoil( FanCoilNum ).ColdPlantOutletNode ).MassFlowRate = MdotLockC;
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
					}
				}
				QUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
			// if heating
			} else if ( UnitOn && QCoilHeatSP > SmallLoad && TempControlType( ZoneNum ) != SingleCoolingSetPoint ) {
				// get full load result
				if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) { // if HW Coil
					ControlNode = FanCoil( FanCoilNum ).HotControlNode;
					ControlOffset = FanCoil( FanCoilNum ).HotControlOffset;
					MaxWaterFlow = FanCoil( FanCoilNum ).MaxHotWaterFlow;
					MinWaterFlow = FanCoil( FanCoilNum ).MinHotWaterFlow;
					//On the first HVAC iteration the system values are given to the controller, but after that
					// the demand limits are in place and there needs to be feedback to the Zone Equipment
					if ( !FirstHVACIteration ) {
						MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
						MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
					}
					mdot = MaxWaterFlow;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMaxH );
					if ( ! HotFlowLocked ) {
						FanCoil( FanCoilNum ).QUnitOutMaxH = QUnitOutMaxH;
					}
					else {
						QUnitOutMaxH = FanCoil( FanCoilNum ).QUnitOutMaxH;
						MdotLockH = mdot; // save locked flow
					}
				} else {
					// not HW coil
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMaxH, 1.0 );
				}
				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
				if ( QUnitOutMaxH > QZnReq ) {
					// more heating than required, find reduced water flow rate to meet the load
					if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
						//solve for the hot water flow rate with no limit set by flow rate lockdown
						Par( 1 ) = double( FanCoilNum );
						Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
						if ( FirstHVACIteration ) Par( 2 ) = 1.0;
						Par( 3 ) = ControlledZoneNum;
						Par( 4 ) = QZnReq;
						SolveRegulaFalsi( 0.001, MaxIterCycl, SolFlag, HWFlow, CalcFanCoilHWLoadResidual, 0.0, MaxWaterFlow, Par );
						if ( SolFlag == -1 ) {
							++FanCoil( FanCoilNum ).ConvgErrCountH;
							if ( FanCoil( FanCoilNum ).ConvgErrCountH < 2 ) {
								ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
								ShowContinueErrorTimeStamp( "..Water flow rate set to last iteration value " );
							} else {
								ShowRecurringWarningErrorAtEnd( "Hot water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name,
									FanCoil( FanCoilNum ).MaxIterIndexH );
							}
						} else if ( SolFlag == -2 ) {
							++FanCoil( FanCoilNum ).LimitErrCountH;
							if ( FanCoil( FanCoilNum ).LimitErrCountH < 2 ) {
								ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Bad hot water mass flow limits" );
								ShowContinueErrorTimeStamp( "..Water flow rate set to lower limit " );
							} else {
								ShowRecurringWarningErrorAtEnd( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name,
									FanCoil( FanCoilNum ).BadMassFlowLimIndexH );
							}
						}
					}
					else {
						Par( 1 ) = double( FanCoilNum );
						Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
						if ( FirstHVACIteration ) Par( 2 ) = 1.0;
						Par( 3 ) = ControlledZoneNum;
						Par( 4 ) = QZnReq;
						SolveRegulaFalsi( 0.001, MaxIterCycl, SolFlag, PLR, CalcFanCoilLoadResidual, 0.0, 1.0, Par );
					}
				} else {
					// demand greater than capacity
					if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
						HWFlow = MaxWaterFlow;
					} else{
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 );
					}
				}
				if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
					if ( !HotFlowLocked ) {
						mdot = HWFlow; // not flowlocked - set flow to HWFlow
						SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum,
							FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut ); // get QUnitOut
					}
					else {
						// flow lock on 
						if ( MdotLockH > HWFlow ) { // if mdot > HWFlow, bypass extra flow
							Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut ); // get QUnitOut with HWFlow; rest will be bypassed 
							Node( FanCoil( FanCoilNum ).HotControlNode ).MassFlowRate = MdotLockH; // reset flow to locked value. Since lock is on, must do this by hand
							Node( FanCoil( FanCoilNum ).HotPlantOutletNode ).MassFlowRate = MdotLockH;
							// Keep soln flow rate but reset outlet water temperature - i.e. bypass extra water
							HWFlowBypass = MdotLockH - HWFlow;
							// change outlet water temperature and enthalpy
							Node( FanCoil( FanCoilNum ).HotPlantOutletNode ).Temp = ( HWFlowBypass * Node( FanCoil( FanCoilNum ).HotControlNode ).Temp +
								HWFlow * Node( FanCoil( FanCoilNum ).HotPlantOutletNode ).Temp ) / MdotLockH;
							Node( FanCoil( FanCoilNum ).HotPlantOutletNode ).Enthalpy = ( HWFlowBypass * Node( FanCoil( FanCoilNum ).HotControlNode ).Enthalpy +
								HWFlow * Node( FanCoil( FanCoilNum ).HotPlantOutletNode ).Enthalpy ) / MdotLockH;
						}
						else {
							// if MdotLockH <= HWFlow use MdotLockH as is
							Node( FanCoil( FanCoilNum ).HotControlNode ).MassFlowRate = MdotLockH; // reset flow to locked value. Since lock is on, must do this by hand
							Node( FanCoil( FanCoilNum ).HotPlantOutletNode ).MassFlowRate = MdotLockH;
							Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
						}
					}
				}
				QUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
			} else {
				// no action
				QUnitOut = QUnitOutNoHC;
			}

			// CR9155 Remove specific humidity calculations
			SpecHumOut = Node( OutletNode ).HumRat;
			SpecHumIn = Node( InletNode ).HumRat;
			LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
			QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			// report variables
			FanCoil( FanCoilNum ).HeatPower = max( 0.0, QUnitOut );
			FanCoil( FanCoilNum ).SensCoolPower = std::abs( min( constant_zero, QUnitOut ) );
			FanCoil( FanCoilNum ).TotCoolPower = std::abs( min( constant_zero, QTotUnitOut ) );
			FanCoil( FanCoilNum ).ElecPower = FanElecPower;
			PowerMet = QUnitOut;
			LatOutputProvided = LatentOutput;

			// cycling fan constant water flow AND VarFanVarFlow
		} else if ( ( SELECT_CASE_var == CCM_CycFan ) || ( SELECT_CASE_var == CCM_VarFanVarFlow ) ) {

			if ( CurDeadBandOrSetback( ZoneNum ) || AirMassFlow < SmallMassFlow ) UnitOn = false;

			// speed fan selection only for multispeed cycling fan
			if ( UnitOn && ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_CycFan ) ) {
				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
				Node( InletNode ).MassFlowRateMax = FanCoil( FanCoilNum ).LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );
				FanCoil( FanCoilNum ).SpeedFanSel = 1;
				FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).LowSpeedRatio;
				if ( std::abs( QUnitOutMax ) < std::abs( QZnReq ) ) {
					Node( InletNode ).MassFlowRateMax = FanCoil( FanCoilNum ).MedSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );
					FanCoil( FanCoilNum ).SpeedFanSel = 2;
					FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).MedSpeedRatio;
				}
				if ( std::abs( QUnitOutMax ) < std::abs( QZnReq ) ) {
					FanCoil( FanCoilNum ).SpeedFanSel = 3;
					FanCoil( FanCoilNum ).SpeedFanRatSel = 1.0;
					Node( InletNode ).MassFlowRateMax = FanCoil( FanCoilNum ).MaxAirMassFlow;
				}
			} else {
				FanCoil( FanCoilNum ).SpeedFanSel = 0;
			}

			//  zero the hot & cold water flows
			//      Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = 0.0
			//      Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = 0.0
			mdot = 0.0;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );

			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
			}
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutNoHC, 0.0 ); // needs PLR=0 for electric heating coil, otherwise will run at full capacity

			if ( UnitOn && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP < ( -1.0 * SmallLoad ) && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
				// cooling coil action, maximum cold water flow
				mdot = FanCoil( FanCoilNum ).MaxColdWaterFlow;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );

				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
				ControlOffset = FanCoil( FanCoilNum ).ColdControlOffset;

				// get the maximum output of the fcu
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );

				if ( QUnitOutMax < 0.0 ) { // protect against QUnitOutMax = 0 (no water flow, plant off, etc.)
					// calculate the PLR, if load greater than output, PLR = 1 (output = max)
					PLR = std::abs( QZnReq / QUnitOutMax );
					if ( PLR > 1.0 ) PLR = 1.0;

					// adjust the PLR to meet the cooling load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
					while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > SmallLoad && Iter < MaxIterCycl && PLR != 1.0 ) {
						// the water flow rate is at the maximum flow rate times the PLR
						//    Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = PLR * FanCoil(FanCoilNum)%MaxColdWaterFlow
						mdot = PLR * FanCoil( FanCoilNum ).MaxColdWaterFlow;
						SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
						Error = ( QZnReq - QUnitOut ) / QZnReq;
						AbsError = QZnReq - QUnitOut;
						DelPLR = ( QZnReq - QUnitOut ) / QUnitOutMax;
						PLR += Relax * DelPLR;
						PLR = max( 0.0, min( 1.0, PLR ) );
						++Iter;
						if ( Iter == 32) Relax = 0.5;
						if ( Iter == 65 ) Relax = 0.25;
						if ( Iter > 70 && PLR == 0.0 && DelPLR < 0.0 ) Error = 0.0; // exit loop if PLR = 0
					}

					// warning if not converged
					if ( Iter > ( MaxIterCycl - 1 ) ) {
						if ( FanCoil( FanCoilNum ).MaxIterIndexC == 0 ) {
							ShowWarningMessage( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within the cooling convergence tolerance." );
							ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIterCycl ) );
						}
						ShowRecurringWarningErrorAtEnd( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\"  -- Exceeded max iterations error (sensible runtime) continues...", FanCoil( FanCoilNum ).MaxIterIndexC );
					}
				} else {
					PLR = 1.0;
					mdot = PLR * FanCoil( FanCoilNum ).MaxColdWaterFlow;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
				}

				// at the end calculate output with adjusted PLR
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );

			} else if ( UnitOn && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP > SmallLoad && TempControlType( ZoneNum ) != SingleCoolingSetPoint ) {
				// heating coil action, maximun hot water flow
				//    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = FanCoil(FanCoilNum)%MaxHotWaterFlow

				if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
					mdot = FanCoil( FanCoilNum ).MaxHotWaterFlow;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
				}

				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
				ControlOffset = FanCoil( FanCoilNum ).HotControlOffset;

				// get the maximum output of the fcu
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );
				// calculate the PLR, if load greater than output, PLR = 1 (output = max)
				if ( QUnitOutMax > 0.0 ) { // protect against QUnitOutMax = 0 (no water flow, plant off, etc.)
					PLR = std::abs( QZnReq / QUnitOutMax );
					if ( PLR > 1.0 ) PLR = 1.0;

					// adjust the PLR to meet the heating load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
					while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > SmallLoad && Iter < MaxIterCycl && PLR != 1.0 ) {
						// the water flow rate is at the maximum flow rate time the PLR
						//    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = PLR * FanCoil(FanCoilNum)%MaxHotWaterFlow

						if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
							mdot = PLR * FanCoil( FanCoilNum ).MaxHotWaterFlow;
							SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
						}

						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
						Error = ( QZnReq - QUnitOut ) / QZnReq;
						AbsError = QZnReq - QUnitOut;
						DelPLR = ( QZnReq - QUnitOut ) / QUnitOutMax;
						PLR += Relax * DelPLR;
						PLR = max( 0.0, min( 1.0, PLR ) );
						++Iter;
						if ( Iter == 32 ) Relax = 0.5;
						if ( Iter == 65 ) Relax = 0.25;
						if ( Iter > 70 && PLR == 0.0 && DelPLR < 0.0 ) Error = 0.0; // exit loop if PLR = 0
					}

					// warning if not converged
					if ( Iter > ( MaxIterCycl - 1 ) ) {
						if ( FanCoil( FanCoilNum ).MaxIterIndexH == 0 ) {
							ShowWarningMessage( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within the heating convergence tolerance." );
							ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIterCycl ) );
						}
						ShowRecurringWarningErrorAtEnd( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\"  -- Exceeded max iterations error (sensible runtime) continues...", FanCoil( FanCoilNum ).MaxIterIndexH );
					}
				} else {
					PLR = 1.0;
					if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
						mdot = PLR * FanCoil( FanCoilNum ).MaxHotWaterFlow;
						SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
					}
				}

				// at the end calculate output with adjusted PLR
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );

				//this part of the code is just if we want ventilation in the deadband zone
				//ELSE IF (AirMassFlow .gt. 0.0d0) THEN
				// if fan scheduled available : just ventilation, PLR = 1
				//QUnitOut = QUnitOutNOHC
				//PLR = 1.

			} else {
				// no action, zero the air flow rate, the unit is off
				Node( InletNode ).MassFlowRate = 0.0;
				Node( OutletNode ).MassFlowRate = 0.0;
				FanCoil( FanCoilNum ).SpeedFanSel = 0;
				PLR = 0.0;
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
			}

			AirMassFlow = Node( InletNode ).MassFlowRate;
			// CR9155 Remove specific humidity calculations
			SpecHumOut = Node( OutletNode ).HumRat;
			SpecHumIn = Node( InletNode ).HumRat;
			LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
			QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			// report variables
			FanCoil( FanCoilNum ).HeatPower = max( 0.0, QUnitOut );
			FanCoil( FanCoilNum ).SensCoolPower = std::abs( min( constant_zero, QUnitOut ) );
			FanCoil( FanCoilNum ).TotCoolPower = std::abs( min( constant_zero, QTotUnitOut ) );
			FanCoil( FanCoilNum ).ElecPower = FanElecPower;
			FanCoil( FanCoilNum ).PLR = PLR;
			PowerMet = QUnitOut;
			LatOutputProvided = LatentOutput;

		} else if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_ASHRAE ) {

			if ( CurDeadBandOrSetback( ZoneNum ) || AirMassFlow < SmallMassFlow ) UnitOn = false;

			//  zero the hot & cold water flows
			mdot = 0.0;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
			OAMassFlow = 0.0;

			// determine minimum outdoor air flow rate
			if ( FanCoil( FanCoilNum ).DSOAPtr > 0 && FanCoil( FanCoilNum ).OutsideAirNode > 0 ) {
				OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( FanCoil( FanCoilNum ).DSOAPtr, ZoneNum, true, true );
				RhoAir = PsyRhoAirFnPbTdbW( Node( FanCoil( FanCoilNum ).OutsideAirNode ).Press, Node( FanCoil( FanCoilNum ).OutsideAirNode ).Temp, Node( FanCoil( FanCoilNum ).OutsideAirNode ).HumRat );
				OAMassFlow = OAVolumeFlowRate * RhoAir;
			}

			MinSAMassFlowRate = min( max( OAMassFlow, FanCoil( FanCoilNum ).MaxAirMassFlow * FanCoil( FanCoilNum ).LowSpeedRatio ), FanCoil( FanCoilNum ).MaxAirMassFlow );
			MaxSAMassFlowRate = FanCoil( FanCoilNum ).MaxAirMassFlow;
			Node( InletNode ).MassFlowRate = MinSAMassFlowRate;

			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
			}

			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutNoHC, 0.0 ); // needs PLR=0 for electric heating coil, otherwise will run at full capacity

			// Operating mode = cooling
			if ( UnitOn && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP < ( -1.0 * SmallLoad ) && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
				// cooling coil action, maximum cold water flow
				mdot = FanCoil( FanCoilNum ).MaxColdWaterFlow;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );

				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
				ControlOffset = FanCoil( FanCoilNum ).ColdControlOffset;

				// get the maximum output of the fcu
				Node( InletNode ).MassFlowRate = MaxSAMassFlowRate;
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );

				// zone load is less than reduced capacity (e.g., 50% capacity) so operate at low speed fan and modulate coil water flow rate to meet load
				// operate at low speed fan and modulate coil water flow rate to meet load
				if ( QUnitOutMax < 0.0 && ( QZnReq > ( FanCoil( FanCoilNum ).DesZoneCoolingLoad * FanCoil( FanCoilNum ).LowSpeedRatio ) ) ) {

					Node( InletNode ).MassFlowRate = MinSAMassFlowRate;

					//solve for the cold water flow rate with no limit set by flow rate lockdown
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if ( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = QZnReq;
					Par( 5 ) = double( FanCoil( FanCoilNum ).ColdControlNode );
					SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowResidual, 0.0, FanCoil( FanCoilNum ).MaxColdWaterFlow, Par );
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
					if ( SolFlag == -1 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
						ShowRecurringWarningErrorAtEnd( "Cold water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
					} else if ( SolFlag == -2 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Bad cold water mass flow limits" );
						ShowRecurringWarningErrorAtEnd( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
					}

				// operate at variable air flow and modulate coil water flow rate to meet load
				} else if ( QUnitOutMax < 0.0 && ( QZnReq > FanCoil( FanCoilNum ).DesZoneCoolingLoad ) && ( QZnReq > QUnitOutMax ) ) {

					Node( InletNode ).MassFlowRate = MinSAMassFlowRate;

					//solve for the cold water flow rate at low speed fan operation
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = FanCoil( FanCoilNum ).DesZoneCoolingLoad * FanCoil( FanCoilNum ).LowSpeedRatio; // QZnReq
					Par( 5 ) = double( FanCoil( FanCoilNum ).ColdControlNode );
					SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowResidual, 0.0, FanCoil( FanCoilNum ).MaxColdWaterFlow, Par );
					if ( SolFlag == -1 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
						ShowRecurringWarningErrorAtEnd( "Cold water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
					} else if ( SolFlag == -2 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Bad cold water mass flow limits" );
						ShowRecurringWarningErrorAtEnd( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
					}
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );

					// if cooling load is greater than low fan speed unit output, modulate air and water flow rate so that each  (i.e., mdot and air flow) are above last iteration result
					if ( QZnReq < QUnitOut ) {
						//solve for the cold water and air flow rate that meets zone load
						Par( 1 ) = double( FanCoilNum );
						Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
						if( FirstHVACIteration ) Par( 2 ) = 1.0;
						Par( 3 ) = ControlledZoneNum;
						Par( 4 ) = QZnReq;
						Par( 5 ) = double( FanCoil( FanCoilNum ).ColdControlNode );
						Par( 6 ) = mdot;
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilAirAndWaterFlowResidual, 0.0, 1.0, Par );
						Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate = mdot + ( PLR * ( FanCoil( FanCoilNum ).MaxColdWaterFlow - mdot ) );
						SetComponentFlowRate( Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
						if ( SolFlag == -1 ) {
							ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
							ShowRecurringWarningErrorAtEnd( "Cold water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
						} else if ( SolFlag == -2 ) {
							ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Bad cold water mass flow limits" );
							ShowRecurringWarningErrorAtEnd( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
							Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = MinSAMassFlowRate;
							SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
							Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
						}
					} else {
						SetComponentFlowRate( Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
					}

				// operate at variable coil water flow rate to meet load
				} else if ( QUnitOutMax < 0.0 && ( QZnReq > QUnitOutMax ) ) {

					//solve for the cold water flow rate with no limit set by flow rate lockdown
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if ( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = QZnReq;
					Par( 5 ) = double( FanCoil( FanCoilNum ).ColdControlNode );
					SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowResidual, 0.0, FanCoil( FanCoilNum ).MaxColdWaterFlow, Par );
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
					if ( SolFlag == -1 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
						ShowRecurringWarningErrorAtEnd( "Cold water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
					} else if ( SolFlag == -2 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Bad cold water mass flow limits" );
						ShowRecurringWarningErrorAtEnd( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
					}

				} else { // else run at full capacity
					PLR = 1.0;
					mdot = FanCoil( FanCoilNum ).MaxColdWaterFlow;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
				}

				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
				FCOutletTempOn = Node( OutletNode ).Temp;

				// check supply air outlet temperature limit
				if ( FCOutletTempOn < FanCoil( FanCoilNum ).MinSATempCooling ) {
					// RegulaFalsi on air flow and temp to meet load
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if ( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = FanCoil( FanCoilNum ).MinSATempCooling;
					Par( 5 ) = QZnReq;
					Par( 6 ) = double( FanCoil( FanCoilNum ).ColdControlNode );
					SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowTempResidual, 0.0, FanCoil( FanCoilNum ).MaxColdWaterFlow, Par );
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
					if ( SolFlag == -1 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
						ShowRecurringWarningErrorAtEnd( "Cold water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
					} else if ( SolFlag == -2 ) {
						ShowWarningError( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Bad cold water mass flow limits" );
						ShowRecurringWarningErrorAtEnd( "Cold Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
					}
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
				}

			// Operating mode = heating
			} else if ( UnitOn && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP > SmallLoad && TempControlType( ZoneNum ) != SingleCoolingSetPoint ) {

				// heating coil action, maximun hot water flow
				if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
					mdot = FanCoil( FanCoilNum ).MaxHotWaterFlow;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
				}

				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
				ControlOffset = FanCoil( FanCoilNum ).HotControlOffset;

				// get the maximum output of the fcu
				Node( InletNode ).MassFlowRate = MaxSAMassFlowRate;
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );

				// zone load is less than reduced capacity (e.g., 50% capacity) so operate at low speed fan and modulate coil water flow rate to meet load
				// operate at low speed fan and modulate coil water flow rate or elec heater PLR to meet load
				if ( QUnitOutMax > 0.0 && ( QZnReq < ( FanCoil( FanCoilNum ).DesZoneHeatingLoad * FanCoil( FanCoilNum ).LowSpeedRatio ) ) ) {

					Node( InletNode ).MassFlowRate = MinSAMassFlowRate;

					if ( QZnReq > QUnitOutNoHC ) {
						//solve for the hot water flow rate at low speed fan
						Par( 1 ) = double( FanCoilNum );
						Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
						if( FirstHVACIteration ) Par( 2 ) = 1.0;
						Par( 3 ) = ControlledZoneNum;
						Par( 4 ) = QZnReq;
						Par( 5 ) = double( FanCoil( FanCoilNum ).HotControlNode );
						if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
							SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowResidual, 0.0, FanCoil( FanCoilNum ).MaxHotWaterFlow, Par );
							SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
							if ( SolFlag == -1 ) {
								ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
								ShowRecurringWarningErrorAtEnd( "Hot water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
							} else if ( SolFlag == -2 ) {
								ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Bad hot water mass flow limits" );
								ShowRecurringWarningErrorAtEnd( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
								Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = MinSAMassFlowRate;
								if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
									mdot = 0.0;
									SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
								}
							}
							Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
							PLR = 1.0;

						} else if( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Electric ) {
							Par( 6 ) = 0.0;
							SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilElecHeatResidual, 0.0, 1.0, Par );
							if( SolFlag == -1 ) {
								ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Iteration limit exceeded in calculating heating coil part load ratio." );
								ShowRecurringWarningErrorAtEnd( "Electric heating coil iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
							} else if( SolFlag == -2 ) {
								ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Bad starting values for electric heating coil control at minimum air flow." );
								ShowRecurringWarningErrorAtEnd( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
							}
							Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
						}

					} else { // unit coils are off
						if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
							mdot = 0.0;
							SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
						}
						PLR = 0.0;
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutNoHC, PLR ); // needs PLR=0 for electric heating coil, otherwise will run at full capacity
					}

				// zone load is greater than reduced zone load and less than design zone load, modulate air flow rate to meet load
				// operate at variable air flow and modulate coil water flow rate or elec heater PLR to meet load
				} else if ( QUnitOutMax > 0.0 && ( QZnReq < FanCoil( FanCoilNum ).DesZoneHeatingLoad ) && ( QZnReq < QUnitOutMax ) ) {

					Node( InletNode ).MassFlowRate = MinSAMassFlowRate;
					//solve for the hot water flow rate with no limit set by flow rate lockdown
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = FanCoil( FanCoilNum ).DesZoneHeatingLoad * FanCoil( FanCoilNum ).LowSpeedRatio; // QZnReq
					Par( 5 ) = double( FanCoil( FanCoilNum ).HotControlNode );
					if( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowResidual, 0.0, FanCoil( FanCoilNum ).MaxHotWaterFlow, Par );
						Low_mdot = mdot;
						if ( SolFlag == -1 ) {
							ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
							ShowRecurringWarningErrorAtEnd( "Hot water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						} else if ( SolFlag == -2 ) {
							ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Bad hot water mass flow limits" );
							ShowRecurringWarningErrorAtEnd( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						}

						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );

						Node( InletNode ).MassFlowRate = MaxSAMassFlowRate;
						//solve for the hot water flow rate with no limit set by flow rate lockdown
						Par( 1 ) = double( FanCoilNum );
						Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
						if( FirstHVACIteration ) Par( 2 ) = 1.0;
						Par( 3 ) = ControlledZoneNum;
						Par( 4 ) = FanCoil( FanCoilNum ).DesZoneHeatingLoad; // QZnReq;
						Par( 5 ) = double( FanCoil( FanCoilNum ).HotControlNode );
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowResidual, 0.0, FanCoil( FanCoilNum ).MaxHotWaterFlow, Par );
						if ( SolFlag == -1 ) {
							ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
							ShowRecurringWarningErrorAtEnd( "Hot water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						} else if ( SolFlag == -2 ) {
							ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Bad hot water mass flow limits" );
							ShowRecurringWarningErrorAtEnd( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						}

						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );

						if ( ( QUnitOut < QZnReq ) && ( QUnitOutMax > QZnReq ) ) {
							Par( 1 ) = double( FanCoilNum );
							Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
							if( FirstHVACIteration ) Par( 2 ) = 1.0;
							Par( 3 ) = ControlledZoneNum;
							Par( 4 ) = QZnReq;
							Par( 5 ) = Low_mdot;
							Par( 6 ) = mdot;
							Par( 7 ) = double( FanCoil( FanCoilNum ).HotControlNode );
							SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilBothFlowResidual, 0.0, 1.0, Par );
							if ( SolFlag == -1 ) {
								ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Iteration limit exceeded in calculating air flow rate " );
								ShowRecurringWarningErrorAtEnd( "Hot water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
							} else if ( SolFlag == -2 ) {
								ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
								ShowContinueError( "  Bad air mass flow limits" );
								ShowRecurringWarningErrorAtEnd( "Hot Water air flow control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
							}

							SetComponentFlowRate( Node( FanCoil( FanCoilNum ).HotControlNode ).MassFlowRate, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );

						} else if ( QUnitOutMax > FanCoil( FanCoilNum ).DesZoneHeatingLoad  ) {
							if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
								SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
							}

						} else if ( QUnitOut < FanCoil( FanCoilNum ).DesZoneHeatingLoad * FanCoil( FanCoilNum ).LowSpeedRatio ) {
							Node( InletNode ).MassFlowRate = MinSAMassFlowRate;
							if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
								SetComponentFlowRate( Low_mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
							}

						}
						PLR = 1.0;

					} else { // not a hot water coil

						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
						Par( 4 ) = QZnReq;
						if ( QUnitOut < FanCoil( FanCoilNum ).DesZoneHeatingLoad * FanCoil( FanCoilNum ).LowSpeedRatio ) {
							// just modulate the heating coil
						} else {
							Par( 5 ) = MaxSAMassFlowRate;
							Par( 6 ) = -1.0; // also modulate air flow rate
						}
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilElecHeatResidual, 0.0, 1.0, Par );
						if( SolFlag == -1 ) {
							ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Iteration limit exceeded in calculating heating coil part load ratio." );
							ShowRecurringWarningErrorAtEnd( "Electric heating coil iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						} else if( SolFlag == -2 ) {
							ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Bad starting values for electric heating coil control at modulating air flow." );
							ShowRecurringWarningErrorAtEnd( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						}

					}

				} else if ( QUnitOutMax > 0.0 && ( QZnReq < QUnitOutMax ) ) {

					Node( InletNode ).MassFlowRate = MaxSAMassFlowRate;

					//solve for the hot water flow rate with no limit set by flow rate lockdown
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = QZnReq;
					Par( 5 ) = double( FanCoil( FanCoilNum ).HotControlNode );
					if( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowResidual, 0.0, FanCoil( FanCoilNum ).MaxHotWaterFlow, Par );
						if ( SolFlag == -1 ) {
							ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Iteration limit exceeded in calculating water flow rate " );
							ShowRecurringWarningErrorAtEnd( "Hot water flow Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						} else if ( SolFlag == -2 ) {
							ShowWarningError( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Bad hot water mass flow limits" );
							ShowRecurringWarningErrorAtEnd( "Hot Water control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						}

						PLR = 1.0;
					} else {

						Par( 6 ) = 0.0;
						Node( InletNode ).MassFlowRate = MaxSAMassFlowRate;
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilElecHeatResidual, 0.0, 1.0, Par );
						if( SolFlag == -1 ) {
							ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Iteration limit exceeded in calculating heating coil part load ratio." );
							ShowRecurringWarningErrorAtEnd( "Electric heating coil iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						} else if( SolFlag == -2 ) {
							ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
							ShowContinueError( "  Bad starting values for electric heating coil control at maximum air flow." );
							ShowRecurringWarningErrorAtEnd( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
						}

					}
				} else {
					PLR = 1.0;
					if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
						mdot = FanCoil( FanCoilNum ).MaxHotWaterFlow;
						SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
					}
				}

				if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut);
				} else {
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
				}
				FCOutletTempOn = Node( OutletNode ).Temp;

				if ( FCOutletTempOn > FanCoil( FanCoilNum ).MaxSATempHeating && FanCoil( FanCoilNum ).ASHRAETempControl ) {
				// RegulaFalsi on air flow and temp to meet load
					Par( 1 ) = double( FanCoilNum );
					Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
					if ( FirstHVACIteration ) Par( 2 ) = 1.0;
					Par( 3 ) = ControlledZoneNum;
					Par( 4 ) = FanCoil( FanCoilNum ).MaxSATempHeating;
					if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
						Par( 5 ) = QZnReq;
						Par( 6 ) = double( FanCoil( FanCoilNum ).HotControlNode );
						SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, mdot, CalcFanCoilWaterFlowTempResidual, 0.0, FanCoil( FanCoilNum ).MaxHotWaterFlow, Par );
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
					} else {
						Par( 5 ) = FanCoil( FanCoilNum ).MaxAirMassFlow;
						Par( 6 ) = -1.0; // modulate air flow and heating coil PLR
						Par( 7 ) = MinSAMassFlowRate;
						SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilElecHeatTempResidual, 0.0, 1.0, Par );
						Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
					}
					if( SolFlag == -1 ) {
						ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating heating coil part load ratio." );
						ShowRecurringWarningErrorAtEnd( "Electric heating coil iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
					} else if( SolFlag == -2 ) {
						ShowWarningError( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
						ShowContinueError( "  Bad starting values for electric heating coil control at constant temperature." );
						ShowRecurringWarningErrorAtEnd( "Electric heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexH );
					}
					FCOutletTempOn = Node( OutletNode ).Temp;

// How to iterate on multiple independent variables, air flow rate, SA temperature, water flow rate or elec heating coil PLR, zone load.
// The following code has drawbacks. One is that it dependently adjusts both air flow and water flow or PLR. It could only adjust air flow to a point, but this won't work for elec heaters (i.e., elec heaters put out a Q regardless of air flow).
// For now SA temperature will limit output. The following would increase SA flow and/or water flow to meet load. Not sure that's required at this point.
//					// if temperature control reduced capacity so that zone load is not met, iterate on air and water flow (an attempt to hold outlet temp constant)
//					if ( ( QZnReq - QUnitOut ) > 0.0 && Node( InletNode ).MassFlowRate < FanCoil( FanCoilNum ).MaxAirMassFlow ) {
//						//solve for the hot water and air flow rate that meets zone load
//						Par( 1 ) = double( FanCoilNum );
//						Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
//						if( FirstHVACIteration ) Par( 2 ) = 1.0;
//						Par( 3 ) = ControlledZoneNum;
//						Par( 4 ) = min( QZnReq, QUnitOutMax );
//						Par( 5 ) = double( FanCoil( FanCoilNum ).HotControlNode );
//						Par( 7 ) = Node( InletNode ).MassFlowRate;
//						if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
//							Par( 6 ) = mdot;
//							SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilAirAndWaterInStepResidual, 0.0, 1.0, Par );
//							SetComponentFlowRate( Node( FanCoil( FanCoilNum ).HotControlNode ).MassFlowRate, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
//							Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
//						} else {
//							Par( 6 ) = PLR;
//							SolveRegulaFalsi( ControlOffset, MaxIterCycl, SolFlag, PLR, CalcFanCoilAirAndWaterInStepResidual, 0.0, 1.0, Par );
//							PLR = Par( 6 ) + ( PLR * ( 1.0 - Par( 6 ) ) );
//							Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
//						}
//						if ( SolFlag == -1 ) {
//							ShowWarningError( "Heating Coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
//							ShowContinueError( "  Iteration limit exceeded in calculating heating coil output " );
//							ShowRecurringWarningErrorAtEnd( "Heating coil Iteration limit exceeded in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
//						} else if ( SolFlag == -2 ) {
//							ShowWarningError( "Heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name );
//							ShowContinueError( "  Bad starting values for electric heating coil control to meet zone load at constant temperature." );
//							ShowRecurringWarningErrorAtEnd( "Heating coil control failed in fan coil unit " + FanCoil( FanCoilNum ).Name, FanCoil( FanCoilNum ).MaxIterIndexC );
//							Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = MinSAMassFlowRate;
//							if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
//								SetComponentFlowRate( Node( FanCoil( FanCoilNum ).HotControlNode ).MassFlowRate, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
//								Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
//							} else {
//								Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
//							}
//						}
//						FCOutletTempOn = Node( OutletNode ).Temp;
//					}


				}

			} else {
				// no action, zero the air flow rate, the unit is off
				Node( InletNode ).MassFlowRate = 0.0;
				Node( OutletNode ).MassFlowRate = 0.0;
				FanCoil( FanCoilNum ).SpeedFanSel = 0;
				PLR = 0.0;
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
			}

			AirMassFlow = Node( InletNode ).MassFlowRate;
			// CR9155 Remove specific humidity calculations
			SpecHumOut = Node( OutletNode ).HumRat;
			SpecHumIn = Node( InletNode ).HumRat;
			LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
			QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			// report variables
			FanCoil( FanCoilNum ).HeatPower = max( 0.0, QUnitOut );
			FanCoil( FanCoilNum ).SensCoolPower = std::abs( min( constant_zero, QUnitOut ) );
			FanCoil( FanCoilNum ).TotCoolPower = std::abs( min( constant_zero, QTotUnitOut ) );
			FanCoil( FanCoilNum ).ElecPower = FanElecPower;
			FanCoil( FanCoilNum ).PLR = PLR;
			PowerMet = QUnitOut;
			LatOutputProvided = LatentOutput;

			// cycling fan constant water flow AND VarFanVarFlow
		} else if ( SELECT_CASE_var == CCM_VarFanConsFlow ) {

			if ( CurDeadBandOrSetback( ZoneNum ) || AirMassFlow < SmallMassFlow ) UnitOn = false;

			//  zero the hot & cold water flows
			//    Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = 0.0
			//    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = 0.0
			mdot = 0.0;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );

			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
			}
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutNoHC, 0.0 ); // needs PLR=0 for electric heating coil, otherwise will run at full capacity

			if ( UnitOn && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP < ( -1.0 * SmallLoad ) && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
				// cooling coil action, maximum cold water flow
				mdot = FanCoil( FanCoilNum ).MaxColdWaterFlow;
				SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
				ControlOffset = FanCoil( FanCoilNum ).ColdControlOffset;

				// get the maximum output of the fcu
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );
				// calculate the PLR, if load greater than output, PLR = 1 (output = max)
				if ( QUnitOutMax != 0.0 ) PLR = std::abs( QZnReq / QUnitOutMax );
				if ( PLR > 1.0 ) PLR = 1.0;

				// adjust the PLR to meet the cooling load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
				while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > SmallLoad && Iter < MaxIterCycl && PLR != 1.0 ) {
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
					Error = ( QZnReq - QUnitOut ) / QZnReq;
					AbsError = QZnReq - QUnitOut;
					DelPLR = ( QZnReq - QUnitOut ) / QUnitOutMax;
					PLR += Relax * DelPLR;
					PLR = max( 0.0, min( 1.0, PLR ) );
					++Iter;
					if ( Iter == 32 ) Relax = 0.5;
					if ( Iter == 65 ) Relax = 0.25;
				}

				// warning if not converged
				if ( Iter > ( MaxIterCycl - 1 ) ) {
					if ( FanCoil( FanCoilNum ).MaxIterIndexC == 0 ) {
						ShowWarningMessage( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within the cooling convergence tolerance." );
						ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIterCycl ) );
					}
					ShowRecurringWarningErrorAtEnd( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\"  -- Exceeded max iterations error (sensible runtime) continues...", FanCoil( FanCoilNum ).MaxIterIndexC );
				}

				// at the end calculate output with adjusted PLR
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );

			} else if ( UnitOn && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP > SmallLoad && TempControlType( ZoneNum ) != SingleCoolingSetPoint ) {
				// heating coil action, maximun hot water flow
				if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
					mdot = FanCoil( FanCoilNum ).MaxHotWaterFlow;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
				}
				QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
				ControlOffset = FanCoil( FanCoilNum ).HotControlOffset;

				// get the maximum output of the fcu
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOutMax );
				// calculate the PLR, if load greater than output, PLR = 1 (output = max)
				if ( QUnitOutMax != 0.0 ) PLR = std::abs( QZnReq / QUnitOutMax );
				if ( PLR > 1.0 ) PLR = 1.0;

				// adjust the PLR to meet the heating load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
				while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > SmallLoad && Iter < MaxIterCycl && PLR != 1.0 ) {
					Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
					Error = ( QZnReq - QUnitOut ) / QZnReq;
					AbsError = QZnReq - QUnitOut;
					DelPLR = ( QZnReq - QUnitOut ) / QUnitOutMax;
					PLR += Relax * DelPLR;
					PLR = max( 0.0, min( 1.0, PLR ) );
					++Iter;
					if ( Iter == 32 ) Relax = 0.5;
					if ( Iter == 65 ) Relax = 0.25;
				}

				// warning if not converged
				if ( Iter > ( MaxIterCycl - 1 ) ) {
					if ( FanCoil( FanCoilNum ).MaxIterIndexH == 0 ) {
						ShowWarningMessage( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within the heating convergence tolerance." );
						ShowContinueError( "...Requested zone load = " + TrimSigDigits( QZnReq, 3 ) + " [W]");
						ShowContinueError( "...Fan coil capacity   = " + TrimSigDigits( QUnitOut, 3 ) + " [W]" );
						ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIterCycl ) );
					}
					ShowRecurringWarningErrorAtEnd( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\"  -- Exceeded max iterations error (sensible runtime) continues...", FanCoil( FanCoilNum ).MaxIterIndexH );
				}

				// at the end calculate output with adjusted PLR
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );

				//this part of the code is just if we want ventilation in the deadband zone
				//ELSE IF (AirMassFlow .gt. 0.0d0) THEN
				// if fan scheduled available : just ventilation, PLR = 1
				//QUnitOut = QUnitOutNOHC
				//PLR = 1.

			} else {
				// no action, zero the air flow rate, the unit is off
				Node( InletNode ).MassFlowRate = 0.0;
				Node( OutletNode ).MassFlowRate = 0.0;
				FanCoil( FanCoilNum ).SpeedFanSel = 0;
				PLR = 0.0;
				Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
			}

			AirMassFlow = Node( InletNode ).MassFlowRate;
			// CR9155 Remove specific humidity calculations
			SpecHumOut = Node( OutletNode ).HumRat;
			SpecHumIn = Node( InletNode ).HumRat;
			LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
			QSensUnitOutNoATM = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
			QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			// report variables
			FanCoil( FanCoilNum ).HeatPower = max( 0.0, QSensUnitOutNoATM );
			FanCoil( FanCoilNum ).SensCoolPower = std::abs( min( constant_zero, QSensUnitOutNoATM ) );
			FanCoil( FanCoilNum ).TotCoolPower = std::abs( min( constant_zero, QTotUnitOut ) );
			FanCoil( FanCoilNum ).ElecPower = FanElecPower;
			FanCoil( FanCoilNum ).PLR = PLR;
			PowerMet = QUnitOut;
			LatOutputProvided = LatentOutput;

		} else if ( SELECT_CASE_var == CCM_MultiSpeedFan ) {
			// call multi-speed fan staging calculation
			SimMultiStage4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut );
			AirMassFlow = Node( InletNode ).MassFlowRate;
			SpecHumOut = Node( OutletNode ).HumRat;
			SpecHumIn = Node( InletNode ).HumRat;
			LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
			QSensUnitOutNoATM = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
			QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			// report variables
			FanCoil( FanCoilNum ).HeatPower = max( 0.0, QSensUnitOutNoATM );
			FanCoil( FanCoilNum ).SensCoolPower = std::abs( min( constant_zero, QSensUnitOutNoATM ) );
			FanCoil( FanCoilNum ).TotCoolPower = std::abs( min( constant_zero, QTotUnitOut ) );
			FanCoil( FanCoilNum ).ElecPower = FanElecPower;
			PowerMet = QUnitOut;
			LatOutputProvided = LatentOutput;

		}}

	}

	void
	Calc4PipeFanCoil(
		int const FanCoilNum, // Unit index in fan coil array
		int const ControlledZoneNum, // ZoneEquipConfig index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet, // load met by unit (watts)
		Optional< Real64 > PLR // Part Load Ratio, fraction of time step fancoil is on
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2000
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate the components making up the 4 pipe fan coil unit.

		// METHODOLOGY EMPLOYED:
		// Simulates the unit components sequentially in the air flow direction.

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::SimOAMixer;
		using SingleDuct::SimATMixer;
		using Fans::SimulateFanComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using Psychrometrics::PsyHFnTdbW;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataZoneEquipment::ZoneEquipConfig;

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
		int InletNode; // unit air inlet node
		static int ATMixOutNode( 0 ); // outlet node of ATM Mixer
		static int ZoneNode( 0 ); // zone node
		Real64 AirMassFlow; // total mass flow through the unit
		Real64 PartLoad; // if PLR present PartLoad = PLR
		Real64 OASchedValue; // value of OASchedValue, =1 if not schedule
		Real64 ElecHeaterControl( 1.0 ); // 1 or 0, enables or disables heating coil
		Real64 FanSpeedRatio; // ratio of actual fan flow to max design fan flow
		// FLOW

		// if PLR present in arguments, get its value, else default PLR = 1
		if ( present( PLR ) ) {
			PartLoad = PLR;
		} else {
			PartLoad = 1.0;
		}

		OutletNode = FanCoil( FanCoilNum ).AirOutNode;
		InletNode = FanCoil( FanCoilNum ).AirInNode;
		ZoneNode = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;

		// Assume the unit is able to vary the flow. A cycling unit is treated as
		// if it were variable flow, with the flow being the averaqe flow over the time step
		if ( GetCurrentScheduleValue( FanCoil( FanCoilNum ).SchedPtr ) > 0.0 ) {
			if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num != CCM_ConsFanVarFlow ) {
				if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num != CCM_ASHRAE ) Node( InletNode ).MassFlowRate = PartLoad * Node( InletNode ).MassFlowRateMax;
			} else {
				Node( InletNode ).MassFlowRate = Node( InletNode ).MassFlowRateMax;
			}
		}

		// use the value of the outside air schedule if present
		if ( FanCoil( FanCoilNum ).SchedOutAirPtr > 0 ) {
			OASchedValue = GetCurrentScheduleValue( FanCoil( FanCoilNum ).SchedOutAirPtr );
		} else {
			OASchedValue = 1.0;
		}

		if ( FanCoil( FanCoilNum ).ATMixerExists ) {
			ATMixOutNode = FanCoil( FanCoilNum ).ATMixerOutNode;
			if ( FanCoil( FanCoilNum ).ATMixerType == ATMixer_InletSide ) {
				// set the primary air inlet mass flow rate
				Node( FanCoil( FanCoilNum ).ATMixerPriNode ).MassFlowRate = min( Node( FanCoil( FanCoilNum ).ATMixerPriNode ).MassFlowRateMaxAvail, Node( InletNode ).MassFlowRate );
				// now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
				// the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
				SimATMixer( FanCoil( FanCoilNum ).ATMixerName, FirstHVACIteration, FanCoil( FanCoilNum ).ATMixerIndex );
			}
			AirMassFlow = Node( InletNode ).MassFlowRate;
		} else {
			// OutdoorAir:Mixer
			if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_CycFan ) {
				Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRate = min( OASchedValue * Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRateMax * PartLoad * FanCoil( FanCoilNum ).SpeedFanRatSel, Node( InletNode ).MassFlowRate );
			} else if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_MultiSpeedFan ) {
				Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRate = min( OASchedValue * Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRateMax * PartLoad * FanFlowRatio, Node( InletNode ).MassFlowRate );
			} else {
				if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num != CCM_ConsFanVarFlow && FanCoil( FanCoilNum ).CapCtrlMeth_Num != CCM_ASHRAE ) {
					Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRate = min( OASchedValue * Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRateMax * PartLoad, Node( InletNode ).MassFlowRate );
				} else {
					Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRate = min( OASchedValue * Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRateMax, Node( InletNode ).MassFlowRate );
				}
			}
			Node( FanCoil( FanCoilNum ).AirReliefNode ).MassFlowRate = Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRate;
			AirMassFlow = Node( InletNode ).MassFlowRate;
			SimOAMixer( FanCoil( FanCoilNum ).OAMixName, FirstHVACIteration, FanCoil( FanCoilNum ).OAMixIndex );
		}

		if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_CycFan ) {
			// cycling fan coil unit calculation
			if ( FanCoil( FanCoilNum ).SpeedFanSel == 1 ) {
				SimulateFanComponents( FanCoil( FanCoilNum ).FanName, FirstHVACIteration, FanCoil( FanCoilNum ).FanIndex, FanCoil( FanCoilNum ).LowSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
			} else if ( FanCoil( FanCoilNum ).SpeedFanSel == 2 ) {
				SimulateFanComponents( FanCoil( FanCoilNum ).FanName, FirstHVACIteration, FanCoil( FanCoilNum ).FanIndex, FanCoil( FanCoilNum ).MedSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
			} else { // using 1.0 here for fan speed ratio seems wrong if FCU max flow rate is different than the fan maximum flow rate
				SimulateFanComponents( FanCoil( FanCoilNum ).FanName, FirstHVACIteration, FanCoil( FanCoilNum ).FanIndex, 1.0, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
			}
			if ( FanCoil( FanCoilNum ).CCoilType_Num == CCoil_HXAssist ) {
				SimHXAssistedCoolingCoil( FanCoil( FanCoilNum ).CCoilName, FirstHVACIteration, On, 0.0, FanCoil( FanCoilNum ).CCoilName_Index, ContFanCycCoil );
			} else {
				SimulateWaterCoilComponents( FanCoil( FanCoilNum ).CCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).CCoilName_Index, _, 1, PLR );
			}
			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				SimulateWaterCoilComponents( FanCoil( FanCoilNum ).HCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).HCoilName_Index, _, 1, PLR );
			} else {
				if ( Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate > 0.0 ) ElecHeaterControl = 0.0;
				SimulateHeatingCoilComponents( FanCoil( FanCoilNum ).HCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).DesignHeatingCapacity * PartLoad * ElecHeaterControl, FanCoil( FanCoilNum ).HCoilName_Index, _, false, ContFanCycCoil, PartLoad );
			}

		} else if ( FanCoil( FanCoilNum ).CapCtrlMeth_Num == CCM_MultiSpeedFan ) {
			SimulateFanComponents( FanCoil( FanCoilNum ).FanName, FirstHVACIteration, FanCoil( FanCoilNum ).FanIndex, FanFlowRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
			if ( FanCoil( FanCoilNum ).CCoilType_Num == CCoil_HXAssist ) {
				SimHXAssistedCoolingCoil( FanCoil( FanCoilNum ).CCoilName, FirstHVACIteration, On, 0.0, FanCoil( FanCoilNum ).CCoilName_Index, ContFanCycCoil );
			} else {
				SimulateWaterCoilComponents( FanCoil( FanCoilNum ).CCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).CCoilName_Index, _, 1, PLR );
			}
			SimulateWaterCoilComponents( FanCoil( FanCoilNum ).HCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).HCoilName_Index, _, 1, PLR );

		} else { // capacity control method is VariableFanVariableFlow, VariableFanConstantFlow, or ASHRAE90.1

			// calculate fan speed ratio for Fan:OnOff (not used for other fan types). Only used in fan model if performance curves are present.
			FanSpeedRatio = Node( InletNode ).MassFlowRate / ( FanCoil( FanCoilNum ).FanAirVolFlow * StdRhoAir );

			// Constant fan and variable flow calculation AND variable fan
			SimulateFanComponents( FanCoil( FanCoilNum ).FanName, FirstHVACIteration, FanCoil( FanCoilNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
			if ( FanCoil( FanCoilNum ).CCoilType_Num == CCoil_HXAssist ) {
				SimHXAssistedCoolingCoil( FanCoil( FanCoilNum ).CCoilName, FirstHVACIteration, On, 0.0, FanCoil( FanCoilNum ).CCoilName_Index, ContFanCycCoil );
			} else {
				SimulateWaterCoilComponents( FanCoil( FanCoilNum ).CCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).CCoilName_Index );
			}
			if ( FanCoil( FanCoilNum ).HCoilType_Num == HCoil_Water ) {
				SimulateWaterCoilComponents( FanCoil( FanCoilNum ).HCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).HCoilName_Index );
			} else {
				if ( Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate > 0.0 ) ElecHeaterControl = 0.0;
				SimulateHeatingCoilComponents( FanCoil( FanCoilNum ).HCoilName, FirstHVACIteration, FanCoil( FanCoilNum ).DesignHeatingCapacity * PartLoad * ElecHeaterControl, FanCoil( FanCoilNum ).HCoilName_Index, _, false, ContFanCycCoil, PartLoad );
			}

		}

		if ( FanCoil( FanCoilNum ).ATMixerExists ) {
			if ( FanCoil( FanCoilNum ).ATMixerType == ATMixer_SupplySide ) {
				// Now calculate the ATM mixer if it is on the supply side of the zone unit
				SimATMixer( FanCoil( FanCoilNum ).ATMixerName, FirstHVACIteration, FanCoil( FanCoilNum ).ATMixerIndex );
			}
		}

		if ( FanCoil( FanCoilNum ).ATMixerExists ) {
			if ( FanCoil( FanCoilNum ).ATMixerType == ATMixer_SupplySide ) {
				LoadMet = Node( ATMixOutNode ).MassFlowRate * ( PsyHFnTdbW( Node( ATMixOutNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
			} else {
				// ATM Mixer on inlet side
				LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
			}
		} else {
			LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
		}

	}

	void
	SimMultiStage4PipeFanCoil(
		int & FanCoilNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet // Sensible power supplied (W)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   July 2015
		//       MODIFIED       na


		// PURPOSE OF THIS SUBROUTINE:
		// Manages multi-speed fancoil unit simulation;

		// METHODOLOGY EMPLOYED:
		// Selects the appropriate fan speed for a given zone heating or cooling load
		// and determines whether heating or cooling is required, then runs the hot
		// or chilled water coils.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataHeatBalFanSys::TempControlType;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SimMultiStage4PipeFanCoil" );
		//int const MaxIterCycl( 100 );

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 mdot; // chilled or hot water flow rate through the water coils
		Real64 QZnReq; // heating or cooling needed by zone [watts]
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 QUnitOutMax; // heating or sens. cooling provided by fan coil unit (running during an entire timestep)
		Real64 QTotUnitOut; // total unit output [watts]
		Real64 AirMassFlow; // air mass flow rate [kg/sec]
		Real64 QUnitOutNoHC; // unit output with no active heating or cooling [W]
		Real64 QCoilHeatSP; // coil load to the heating setpoint [W]
		Real64 QCoilCoolSP; // coil load to the cooling setpoint [W]
		Real64 SpeedRatio; // ratio between lower and higher fan speed
		Real64 PartLoadRatio; // Part Load Ratio, fraction of time step fancoil is on
		int OutletNode; // unit air outlet node
		int InletNode; // unit air inlet node
		bool UnitOn; // TRUE if unit is on


		// initialize local variables
		UnitOn = true;
		SpeedRatio = 0.0;
		FanElecPower = 0.0;
		PartLoadRatio = 0.0;
		QZnReq = 0.0;
		QUnitOut = 0.0;
		QTotUnitOut = 0.0;
		QUnitOutMax = 0.0;
		QUnitOutNoHC = 0.0;

		OutletNode = FanCoil( FanCoilNum ).AirOutNode;
		InletNode = FanCoil( FanCoilNum ).AirInNode;
		AirMassFlow = Node( InletNode ).MassFlowRate;

		if ( CurDeadBandOrSetback( ZoneNum ) || AirMassFlow < SmallMassFlow ) UnitOn = false;

		FanCoil( FanCoilNum ).SpeedFanSel = 1;
		FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).LowSpeedRatio;
		FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
		AirMassFlow = FanCoil( FanCoilNum ).LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
		Node( InletNode ).MassFlowRate = AirMassFlow;
		Node( InletNode ).MassFlowRateMax = AirMassFlow;
		Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
		Node( InletNode ).MassFlowRateMinAvail = AirMassFlow;

		mdot = 0.0;
		SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
		mdot = 0.0;
		SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
		Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutNoHC );

		QCoilCoolSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		QCoilHeatSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		HeatingLoad = false;
		CoolingLoad = false;

		if ( QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 && TempControlType( ZoneNum ) != SingleCoolingSetPoint ) {
			QZnReq = QCoilHeatSP;
			HeatingLoad = true;
		} else if ( QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 && TempControlType( ZoneNum ) == SingleCoolingSetPoint ) {
			QZnReq = 0.0;
		} else if ( QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
			QZnReq = QCoilCoolSP;
			CoolingLoad = true;
		} else if ( QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 && TempControlType( ZoneNum ) == SingleHeatingSetPoint ) {
			QZnReq = 0.0;
		} else if ( QCoilHeatSP <= 0.0 && QCoilCoolSP >= 0.0 ) {
			QZnReq = 0.0;
		}

		// Zone load calculation for constant fan systems, adopted from unitary system
		if ( FanCoil( FanCoilNum ).FanOpMode == ContFanCycCoil ) {
				{ auto const SELECT_CASE_var( TempControlType( ZoneNum ) );
				if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
					CoolingLoad = false;
					// No heating load and constant fan pushes zone below heating set point
					if ( QUnitOutNoHC < 0.0 && QCoilHeatSP < 0.0 && QUnitOutNoHC - QCoilHeatSP < -SmallLoad ) {
						HeatingLoad = true;
						CoolingLoad = false;
						QZnReq = QCoilHeatSP;
					}
				} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
					HeatingLoad = false;
					// No heating load and constant fan pushes zone above cooling set point
					if ( QUnitOutNoHC > 0.0 && QCoilCoolSP > 0.0 && QUnitOutNoHC - QCoilCoolSP > SmallLoad ) {
						HeatingLoad = false;
						CoolingLoad = true;
						QZnReq = QCoilCoolSP;
					}
				} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
					// zone temp above cooling and heating set point temps
					if ( QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 ) {
						// zone pushed below heating set point
						if ( QUnitOutNoHC < 0.0 && QCoilHeatSP - QUnitOutNoHC > SmallLoad ) {
							HeatingLoad = true;
							CoolingLoad = false;
							QZnReq = QCoilHeatSP;
						}
						// zone temp below heating set point temp
					} else if ( QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 ) {
						// zone pushed above cooling set point
						if ( QUnitOutNoHC > 0.0 && QCoilCoolSP - QUnitOutNoHC > SmallLoad ) {
							HeatingLoad = false;
							CoolingLoad = true;
							QZnReq = QCoilCoolSP;
						}
					}
				} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
					// zone temp above cooling and heating set point temps
					if ( QCoilHeatSP < 0.0 && QCoilCoolSP < 0.0 ) {
						// zone pushed into deadband
						if ( QUnitOutNoHC < 0.0 && QCoilCoolSP - QUnitOutNoHC > SmallLoad ) {
							HeatingLoad = false;
							CoolingLoad = false;
							QZnReq = 0.0;
						}
						// zone pushed below heating set point
						if ( QUnitOutNoHC < 0.0 && QCoilHeatSP - QUnitOutNoHC > SmallLoad ) {
							HeatingLoad = true;
							CoolingLoad = false;
							QZnReq = QCoilHeatSP;
						}
						// zone temp below heating set point temp
					} else if ( QCoilHeatSP > 0.0 && QCoilCoolSP > 0.0 ) {
						// zone pushed into deadband
						if ( QUnitOutNoHC > 0.0 && QUnitOutNoHC - QCoilHeatSP > SmallLoad ) {
							HeatingLoad = false;
							CoolingLoad = false;
							QZnReq = 0.0;
						}
						// zone pushed above cooling set point
						if ( QUnitOutNoHC > 0.0 && QUnitOutNoHC - QCoilCoolSP > SmallLoad ) {
							HeatingLoad = false;
							CoolingLoad = true;
							QZnReq = QCoilCoolSP;
						}
						// zone temp between set point temps
					} else if ( QCoilHeatSP < 0.0 && QCoilCoolSP > 0.0 ) {
						// zone pushed below heating set point
						if ( QUnitOutNoHC < 0.0 && QUnitOutNoHC - QCoilHeatSP < -SmallLoad ) {
							HeatingLoad = true;
							CoolingLoad = false;
							QZnReq = QCoilHeatSP;
							// zone pushed above cooling set point
						} else if ( QUnitOutNoHC > 0.0 && QUnitOutNoHC - QCoilCoolSP > SmallLoad ) {
							HeatingLoad = false;
							CoolingLoad = true;
							QZnReq = QCoilCoolSP;
						}
					}
				} else {
				}}
			// IF small loads to meet, just shut down unit
			if ( std::abs( QZnReq ) < Small5WLoad ) {
				QZnReq = 0.0;
				CoolingLoad = false;
				HeatingLoad = false;
			}
		}

		if ( UnitOn && QZnReq < ( -1.0 * Small5WLoad ) && CoolingLoad ) {

			mdot = 0.0;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
			mdot = FanCoil( FanCoilNum ).MaxColdWaterFlow;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
			// select fan speed
			FanCoil( FanCoilNum ).SpeedFanSel = 1;
			FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).LowSpeedRatio;
			FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
			AirMassFlow = FanCoil( FanCoilNum ).LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
			Node( InletNode ).MassFlowRate = AirMassFlow;
			Node( InletNode ).MassFlowRateMax = AirMassFlow;
			Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
			Node( InletNode ).MassFlowRateMinAvail = AirMassFlow;
			Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax );
			if ( std::abs( QUnitOutMax ) < std::abs( QZnReq ) ) {
				FanCoil( FanCoilNum ).SpeedFanSel = 2;
				FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).MedSpeedRatio;
				FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
				AirMassFlow = FanCoil( FanCoilNum ).MedSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlow;
				Node( InletNode ).MassFlowRateMax = AirMassFlow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
				Node( InletNode ).MassFlowRateMinAvail = FanCoil( FanCoilNum ).LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax );
			}
			if ( std::abs( QUnitOutMax ) < std::abs( QZnReq ) ) {
				FanCoil( FanCoilNum ).SpeedFanSel = 3;
				FanCoil( FanCoilNum ).SpeedFanRatSel = 1.0;
				FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
				AirMassFlow = FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlow;
				Node( InletNode ).MassFlowRateMax = AirMassFlow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
				Node( InletNode ).MassFlowRateMinAvail = FanCoil( FanCoilNum ).MedSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
			}
			CalcMultiStage4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut );

		} else if ( UnitOn && QZnReq > Small5WLoad && HeatingLoad ) {

			mdot = 0.0;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
			mdot = FanCoil( FanCoilNum ).MaxHotWaterFlow;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
			// select fan speed
			FanCoil( FanCoilNum ).SpeedFanSel = 1;
			FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).LowSpeedRatio;
			FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
			AirMassFlow = FanCoil( FanCoilNum ).LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
			Node( InletNode ).MassFlowRate = AirMassFlow;
			Node( InletNode ).MassFlowRateMax = AirMassFlow;
			Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
			Node( InletNode ).MassFlowRateMinAvail = AirMassFlow;
			Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax );
			if ( std::abs( QUnitOutMax ) < std::abs( QZnReq ) ) {
				FanCoil( FanCoilNum ).SpeedFanSel = 2;
				FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).MedSpeedRatio;
				FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
				AirMassFlow = FanCoil( FanCoilNum ).MedSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlow;
				Node( InletNode ).MassFlowRateMax = AirMassFlow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
				Node( InletNode ).MassFlowRateMinAvail = FanCoil( FanCoilNum ).LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax );
			}
			if ( std::abs( QUnitOutMax ) < std::abs( QZnReq ) ) {
				FanCoil( FanCoilNum ).SpeedFanSel = 3;
				FanCoil( FanCoilNum ).SpeedFanRatSel = 1.0;
				FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
				AirMassFlow = FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlow;
				Node( InletNode ).MassFlowRateMax = AirMassFlow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
				Node( InletNode ).MassFlowRateMinAvail = FanCoil( FanCoilNum ).MedSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
			}

			CalcMultiStage4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut );

		} else {
			//SpeedRatio = 0.0;
			if ( FanCoil( FanCoilNum ).FanOpMode == ContFanCycCoil ) {
				PartLoadRatio = 1.0;
				FanCoil( FanCoilNum ).SpeedFanSel = 1;
				FanCoil( FanCoilNum ).SpeedFanRatSel = FanCoil( FanCoilNum ).LowSpeedRatio;
				FanFlowRatio = FanCoil( FanCoilNum ).SpeedFanRatSel;
				AirMassFlow = FanCoil( FanCoilNum ).LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlow;
				Node( InletNode ).MassFlowRateMax = AirMassFlow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
				Node( InletNode ).MassFlowRateMinAvail = AirMassFlow;
			} else {
				PartLoadRatio = 0.0;
				AirMassFlow = 0.0;
				Node( InletNode ).MassFlowRate = AirMassFlow;
				Node( InletNode ).MassFlowRateMax = AirMassFlow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;
				Node( InletNode ).MassFlowRateMinAvail = AirMassFlow;
				Node( InletNode ).MassFlowRate = 0.0;
				Node( OutletNode ).MassFlowRate = 0.0;
				FanCoil( FanCoilNum ).SpeedFanSel = 0;
				FanFlowRatio = 0.0;
			}
			mdot = 0.0;
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
			SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
			Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PartLoadRatio );
		}
		// output variable
		Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
		FanCoil( FanCoilNum ).PLR = PartLoadRatio;
		FanCoil( FanCoilNum ).SpeedRatio = SpeedRatio;
		PowerMet = QUnitOut;

	}

	void
	CalcMultiStage4PipeFanCoil(
		int & FanCoilNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 const QZnReq, // current zone cooling or heating load
		Real64 & SpeedRatio, // fan coil speed ratio
		Real64 & PartLoadRatio, // fan coil part load ratio
		Real64 & PowerMet // Sensible power supplied (W)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   July 2015
		//       MODIFIED       na


		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a multi-stage fan 4 pipe fan coil unit; adjust its output to
		// match the remaining zone load.

		// METHODOLOGY EMPLOYED:
		// If this unit is on, calculated the speed ratio when cycling between
		// consecutive fan speeds. The hot or chilled water flows either at
		// maximum or zero.  The water flow rate is set to zero if there is no
		// load.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataHeatBalFanSys::TempControlType;
		using PlantUtilities::SetComponentFlowRate;
		using General::TrimSigDigits;


		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMultiStage4PipeFanCoil" );
		int const MaxIterCycl( 100 );

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 PLR; // Part Load Ratio, fraction of time step fancoil is on
		Real64 SRatio; // capacity speed ratio of the for multi-stage fan fancoil unit
		Real64 mdot; // chilled or hot water flow rate through the water coils
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 QUnitOutMax; // max heating or sens. cooling provided by fan coil unit [watts]
		Real64 ControlOffset; // tolerance for output control
		Real64 QUnitOutMaxHS; // higher fan speed output
		Real64 QUnitOutMaxLS; // lower fan speed output
		Real64 HighSpeedRatio; // fan flow ratio at low speed
		Real64 LowSpeedRatio; // fan flow ratio at low speed
		Real64 AirMassFlowAvg; // supply air flow rate weighted by speed ratio
		Real64 AirMassFlowLow; // supply air flow rate at lower speed
		Real64 AirMassFlowHigh; // supply air flow rate at higher speed
		Real64 FanElecPowerHS; // fan electric power calculated at (fan) higher speed
		Real64 FanElecPowerLS; // fan electric power calculated at (fan) lower speed
		Real64 Error; // Error between QZnReq and QUnitOut
		Real64 AbsError; // Absolute error between QZnReq and QUnitOut [W]   !FB
		Real64 Relax;
		Real64 DelPLR;
		int OutletNode; // unit air outlet node
		int InletNode; // unit air inlet node
		int Iter; // iteration counter

		// initialize local variables
		mdot = 0.0;
		PLR = 1.0;
		SRatio = 0.0;
		QUnitOut = 0.0;
		QUnitOutMax = 0.0;
		FanElecPower = 0.0;
		ControlOffset = 0.0;
		FanElecPowerHS = 0.0;
		FanElecPowerLS = 0.0;
		AirMassFlowAvg = 0.0;
		AirMassFlowLow = 0.0;
		AirMassFlowHigh = 0.0;
		AbsError = 2.0 * Small5WLoad;
		Error = 1.0;
		Relax = 1.0;
		Iter = 0;

		OutletNode = FanCoil( FanCoilNum ).AirOutNode;
		InletNode = FanCoil( FanCoilNum ).AirInNode;

		if ( QZnReq < ( -1.0 * Small5WLoad ) && CoolingLoad ) {
			ControlOffset = FanCoil( FanCoilNum ).ColdControlOffset;
			if ( FanCoil( FanCoilNum ).SpeedFanSel == 1 ) {
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax );
				PLR = std::abs( QZnReq / QUnitOutMax );
				if ( PLR > 1.0 ) PLR = 1.0;
				// adjust the PLR to meet the cooling load by calling Calc4PipeFanCoil repeatedly
				while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > Small5WLoad && Iter < MaxIterCycl && PLR != 1.0 ) {
					Node( InletNode ).MassFlowRateMinAvail = Node( InletNode ).MassFlowRate;
					mdot = PLR * FanCoil( FanCoilNum ).MaxColdWaterFlow;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).ColdControlNode, FanCoil( FanCoilNum ).ColdPlantOutletNode, FanCoil( FanCoilNum ).CWLoopNum, FanCoil( FanCoilNum ).CWLoopSide, FanCoil( FanCoilNum ).CWBranchNum, FanCoil( FanCoilNum ).CWCompNum );
					if ( FanCoil( FanCoilNum ).FanOpMode == ContFanCycCoil ) {
						Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
					} else {
						Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR );
					}
					Error = ( QZnReq - QUnitOut ) / QZnReq;
					AbsError = QZnReq - QUnitOut;
					DelPLR = ( QZnReq - QUnitOut ) / QUnitOutMax;
					PLR += Relax * DelPLR;
					PLR = max( 0.0, min( 1.0, PLR ) );
					++Iter;
					if ( Iter == 32 ) Relax = 0.5;
					if ( Iter == 65 ) Relax = 0.25;
					if ( Iter > 70 && PLR == 0.0 && DelPLR < 0.0 ) Error = 0.0;
				}
				if ( FanCoil( FanCoilNum ).FanOpMode == ContFanCycCoil ) {
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
				} else {
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR );
				}
				// warning if not converged
				if ( Iter >( MaxIterCycl - 1 ) ) {
					if ( FanCoil( FanCoilNum ).MaxIterIndexC == 0 ) {
						ShowWarningMessage( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within the cooling convergence tolerance." );
						ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIterCycl ) );
					}
					ShowRecurringWarningErrorAtEnd( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\"  -- Exceeded max iterations error (sensible runtime) continues...", FanCoil( FanCoilNum ).MaxIterIndexC );
				}

			} else {
				if ( FanCoil( FanCoilNum ).SpeedFanSel == 2 ) {
					HighSpeedRatio = FanCoil( FanCoilNum ).MedSpeedRatio;
					LowSpeedRatio = FanCoil( FanCoilNum ).LowSpeedRatio;
				} else {
					HighSpeedRatio = 1;
					LowSpeedRatio = FanCoil( FanCoilNum ).MedSpeedRatio;
				}
				// get capacity at lower speed
				FanCoil( FanCoilNum ).SpeedFanRatSel = LowSpeedRatio;
				FanCoil( FanCoilNum ).SpeedFanSel = FanCoil( FanCoilNum ).SpeedFanSel - 1;
				AirMassFlowLow = LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlowLow;
				Node( InletNode ).MassFlowRateMax = AirMassFlowLow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowLow;
				Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
				FanFlowRatio = LowSpeedRatio;
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxLS );
				FanElecPowerLS = FanElecPower;
				// get capacity at higher speed
				FanCoil( FanCoilNum ).SpeedFanRatSel = HighSpeedRatio;
				FanCoil( FanCoilNum ).SpeedFanSel = FanCoil( FanCoilNum ).SpeedFanSel + 1;
				AirMassFlowHigh = HighSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlowHigh;
				Node( InletNode ).MassFlowRateMax = AirMassFlowHigh;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowHigh;
				Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
				FanFlowRatio = HighSpeedRatio;
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxHS );
				FanElecPowerHS = FanElecPower;
				// calc speed ratio
				if ( std::abs( QZnReq ) > std::abs( QUnitOutMaxHS ) ) {
					SRatio = 1.0;
					AirMassFlowAvg = AirMassFlowHigh;
					Node( InletNode ).MassFlowRate = AirMassFlowHigh;
					Node( InletNode ).MassFlowRateMax = AirMassFlowHigh;
					Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowHigh;
					Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
					FanFlowRatio = HighSpeedRatio;
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
				} else {
					SRatio = std::abs( ( QZnReq - QUnitOutMaxLS ) / ( QUnitOutMaxHS - QUnitOutMaxLS ) );
					if ( SRatio > 1.0 ) SRatio = 1.0;
					AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * ( 1.0 - SRatio );
					Node( InletNode ).MassFlowRate = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMax = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
					FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * ( 1.0 - SRatio );
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
					// adjust the PLR to meet the cooling load by calling Calc4PipeFanCoil repeatedly
					while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > Small5WLoad && Iter < MaxIterCycl && SRatio != 1.0 ) {
						AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * ( 1.0 - SRatio );
						FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * ( 1.0 - SRatio );
						Node( InletNode ).MassFlowRate = AirMassFlowAvg;
						Node( InletNode ).MassFlowRateMax = AirMassFlowAvg;
						Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowAvg;
						Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
						Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
						Error = ( QZnReq - QUnitOut ) / QZnReq;
						AbsError = QZnReq - QUnitOut;
						DelPLR = ( QZnReq - QUnitOut ) / ( QUnitOutMaxHS - QUnitOutMaxLS );
						SRatio += Relax * DelPLR;
						SRatio = max( 0.0, min( 1.0, SRatio ) );
						++Iter;
						if ( Iter == 32 ) Relax = 0.5;
						if ( Iter == 65 ) Relax = 0.25;
						if ( Iter > 70 && SRatio == 0.0 && DelPLR < 0.0 ) Error = 0.0;
					}
				}
			}
		} else if ( QZnReq > Small5WLoad && HeatingLoad ) {
			ControlOffset = FanCoil( FanCoilNum ).HotControlOffset;
			if ( FanCoil( FanCoilNum ).SpeedFanSel == 1 ) {
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMax );
				PLR = std::abs( QZnReq / QUnitOutMax );
				if ( PLR > 1.0 ) PLR = 1.0;
				// adjust the PLR to meet the heating load by calling Calc4PipeFanCoil repeatedly
				while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > Small5WLoad && Iter < MaxIterCycl && PLR != 1.0 ) {
					mdot = PLR * FanCoil( FanCoilNum ).MaxHotWaterFlow;
					Node( InletNode ).MassFlowRateMinAvail = Node( InletNode ).MassFlowRate;
					SetComponentFlowRate( mdot, FanCoil( FanCoilNum ).HotControlNode, FanCoil( FanCoilNum ).HotPlantOutletNode, FanCoil( FanCoilNum ).HWLoopNum, FanCoil( FanCoilNum ).HWLoopSide, FanCoil( FanCoilNum ).HWBranchNum, FanCoil( FanCoilNum ).HWCompNum );
					if ( FanCoil( FanCoilNum ).FanOpMode == ContFanCycCoil ) {
						Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
					} else {
						Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR );
					}
					Error = ( QZnReq - QUnitOut ) / QZnReq;
					AbsError = QZnReq - QUnitOut;
					DelPLR = ( QZnReq - QUnitOut ) / QUnitOutMax;
					PLR += Relax * DelPLR;
					PLR = max( 0.0, min( 1.0, PLR ) );
					++Iter;
					if ( Iter == 32 ) Relax = 0.5;
					if ( Iter == 65 ) Relax = 0.25;
					if ( Iter > 70 && PLR == 0.0 && DelPLR < 0.0 ) Error = 0.0; // exit loop if PLR = 0
				}
				if ( FanCoil( FanCoilNum ).FanOpMode == ContFanCycCoil ) {
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
				} else {
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, PLR );
				}
				// warning if not converged
				if ( Iter >( MaxIterCycl - 1 ) ) {
					if ( FanCoil( FanCoilNum ).MaxIterIndexH == 0 ) {
						ShowWarningMessage( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\" -- Exceeded max iterations while adjusting cycling fan sensible runtime to meet the zone load within the heating convergence tolerance." );
						ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIterCycl ) );
					}
					ShowRecurringWarningErrorAtEnd( "ZoneHVAC:FourPipeFanCoil=\"" + FanCoil( FanCoilNum ).Name + "\"  -- Exceeded max iterations error (sensible runtime) continues...", FanCoil( FanCoilNum ).MaxIterIndexH );
				}

			} else {
				if ( FanCoil( FanCoilNum ).SpeedFanSel == 2 ) {
					HighSpeedRatio = FanCoil( FanCoilNum ).MedSpeedRatio;
					LowSpeedRatio = FanCoil( FanCoilNum ).LowSpeedRatio;
				} else {
					HighSpeedRatio = 1;
					LowSpeedRatio = FanCoil( FanCoilNum ).MedSpeedRatio;
				}
				// get capacity at lower speed ratio
				FanCoil( FanCoilNum ).SpeedFanRatSel = LowSpeedRatio;
				FanCoil( FanCoilNum ).SpeedFanSel = FanCoil( FanCoilNum ).SpeedFanSel - 1;
				AirMassFlowLow = LowSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlowLow;
				Node( InletNode ).MassFlowRateMax = AirMassFlowLow;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowLow;
				Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
				FanFlowRatio = LowSpeedRatio;
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxLS );
				FanElecPowerLS = FanElecPower;
				// get capacity at higher speed
				FanCoil( FanCoilNum ).SpeedFanRatSel = HighSpeedRatio;
				FanCoil( FanCoilNum ).SpeedFanSel = FanCoil( FanCoilNum ).SpeedFanSel + 1;
				AirMassFlowHigh = HighSpeedRatio * FanCoil( FanCoilNum ).MaxAirMassFlow;
				Node( InletNode ).MassFlowRate = AirMassFlowHigh;
				Node( InletNode ).MassFlowRateMax = AirMassFlowHigh;
				Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowHigh;
				Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
				FanFlowRatio = HighSpeedRatio;
				Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOutMaxHS );
				FanElecPowerHS = FanElecPower;
				// calc speed ratio
				if ( std::abs( QZnReq ) > std::abs( QUnitOutMaxHS ) ) {
					SRatio = 1.0;
					AirMassFlowAvg = AirMassFlowHigh;
					Node( InletNode ).MassFlowRate = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMax = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
					FanFlowRatio = HighSpeedRatio;
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
				} else {
					SRatio = std::abs( ( QZnReq - QUnitOutMaxLS ) / ( QUnitOutMaxHS - QUnitOutMaxLS ) );
					if ( SRatio > 1.0 ) SRatio = 1.0;
					AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * ( 1.0 - SRatio );
					Node( InletNode ).MassFlowRate = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMax = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowAvg;
					Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
					FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * ( 1.0 - SRatio );
					Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
					ControlOffset = FanCoil( FanCoilNum ).HotControlOffset;
					// adjust the PLR to meet the heating load calling Calc4PipeFanCoil repeatedly
					while ( std::abs( Error ) > ControlOffset && std::abs( AbsError ) > Small5WLoad && Iter < MaxIterCycl && SRatio != 1.0 ) {
						AirMassFlowAvg = AirMassFlowHigh * SRatio + AirMassFlowLow * ( 1.0 - SRatio );
						Node( InletNode ).MassFlowRate = AirMassFlowAvg;
						Node( InletNode ).MassFlowRateMax = AirMassFlowAvg;
						Node( InletNode ).MassFlowRateMaxAvail = AirMassFlowAvg;
						Node( InletNode ).MassFlowRateMinAvail = AirMassFlowLow;
						FanFlowRatio = HighSpeedRatio * SRatio + LowSpeedRatio * ( 1.0 - SRatio );
						Calc4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut );
						Error = ( QZnReq - QUnitOut ) / QZnReq;
						AbsError = QZnReq - QUnitOut;
						DelPLR = ( QZnReq - QUnitOut ) / ( QUnitOutMaxHS - QUnitOutMaxLS );
						SRatio += Relax * DelPLR;
						SRatio = max( 0.0, min( 1.0, SRatio ) );
						++Iter;
						if ( Iter == 32 ) Relax = 0.5;
						if ( Iter == 65 ) Relax = 0.25;
						if ( Iter > 70 && SRatio == 0.0 && DelPLR < 0.0 ) Error = 0.0;
					}
				}
				FanElecPower = FanElecPowerHS * SRatio + FanElecPowerLS * ( 1.0 - SRatio );
			}
		}
		Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
		PartLoadRatio = PLR;
		SpeedRatio = SRatio;
		PowerMet = QUnitOut;
	}

	void
	ReportFanCoilUnit( int const FanCoilNum ) // number of the current fan coil unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fills some of the report variables for the fan coil units

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
		Real64 ReportingConstant;

		// FLOW
		ReportingConstant = TimeStepSys * SecInHour;
		FanCoil( FanCoilNum ).HeatEnergy = FanCoil( FanCoilNum ).HeatPower * ReportingConstant;
		FanCoil( FanCoilNum ).SensCoolEnergy = FanCoil( FanCoilNum ).SensCoolPower * ReportingConstant;
		FanCoil( FanCoilNum ).TotCoolEnergy = FanCoil( FanCoilNum ).TotCoolPower * ReportingConstant;
		FanCoil( FanCoilNum ).ElecEnergy = FanCoil( FanCoilNum ).ElecPower * ReportingConstant;

	}

	int
	GetFanCoilZoneInletAirNode( int const FanCoilNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet node for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetFanCoilZoneInletAirNode;

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

		if ( GetFanCoilInputFlag ) {
			GetFanCoilUnits();
			GetFanCoilInputFlag = false;
		}

		GetFanCoilZoneInletAirNode = 0;
		if ( FanCoilNum > 0 && FanCoilNum <= NumFanCoils ) {
			GetFanCoilZoneInletAirNode = FanCoil( FanCoilNum ).AirOutNode;
		}

		return GetFanCoilZoneInletAirNode;

	}

	int
	GetFanCoilOutAirNode( int const FanCoilNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet node for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetFanCoilOutAirNode;

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

		if ( GetFanCoilInputFlag ) {
			GetFanCoilUnits();
			GetFanCoilInputFlag = false;
		}

		GetFanCoilOutAirNode = 0;
		if ( FanCoilNum > 0 && FanCoilNum <= NumFanCoils ) {
			GetFanCoilOutAirNode = FanCoil( FanCoilNum ).OutsideAirNode;
		}

		return GetFanCoilOutAirNode;

	}

	int
	GetFanCoilReturnAirNode( int const FanCoilNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixer's return node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::GetOAMixerReturnNodeNumber;

		// Return value
		int GetFanCoilReturnAirNode;

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

		if ( GetFanCoilInputFlag ) {
			GetFanCoilUnits();
			GetFanCoilInputFlag = false;
		}

		GetFanCoilReturnAirNode = 0;
		if ( FanCoilNum > 0 && FanCoilNum <= NumFanCoils ) {
			if ( FanCoil( FanCoilNum ).OAMixIndex > 0 ) {
				GetFanCoilReturnAirNode = GetOAMixerReturnNodeNumber( FanCoil( FanCoilNum ).OAMixIndex );
			} else {
				GetFanCoilReturnAirNode = 0;
			}
		}

		return GetFanCoilReturnAirNode;

	}

	int
	GetFanCoilMixedAirNode( int const FanCoilNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixer's return node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::GetOAMixerMixedNodeNumber;

		// Return value
		int GetFanCoilMixedAirNode;

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

		if ( GetFanCoilInputFlag ) {
			GetFanCoilUnits();
			GetFanCoilInputFlag = false;
		}

		GetFanCoilMixedAirNode = 0;
		if ( FanCoilNum > 0 && FanCoilNum <= NumFanCoils ) {
			if ( FanCoil( FanCoilNum ).OAMixIndex > 0 ) {
				GetFanCoilMixedAirNode = GetOAMixerMixedNodeNumber( FanCoil( FanCoilNum ).OAMixIndex );
			} else {
				GetFanCoilMixedAirNode = 0;
			}
		}

		return GetFanCoilMixedAirNode;

	}

	int
	GetFanCoilInletAirNode( int const FanCoilNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for inlet node for Fan Coil unit

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetFanCoilInletAirNode;

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

		if ( GetFanCoilInputFlag ) {
			GetFanCoilUnits();
			GetFanCoilInputFlag = false;
		}

		GetFanCoilInletAirNode = 0;
		if ( FanCoilNum > 0 && FanCoilNum <= NumFanCoils ) {
			GetFanCoilInletAirNode = FanCoil( FanCoilNum ).AirOutNode;
		}

		return GetFanCoilInletAirNode;

	}

	void
	GetFanCoilIndex(
		std::string const & FanCoilName,
		int & FanCoilIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   April 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the index for a given PT Unit

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

		bool ErrorsFound; // for error trapping

		if ( GetFanCoilInputFlag ) {
			GetFanCoilUnits();
			GetFanCoilInputFlag = false;
		}

		FanCoilIndex = FindItemInList( FanCoilName, FanCoil );
		if ( FanCoilIndex == 0 ) {
			ShowSevereError( "GetFanCoilIndex: Fan Coil Unit not found=" + FanCoilName );
		}
		ErrorsFound = true;

	}

	Real64
	CalcFanCoilLoadResidual(
		Real64 const PartLoadRatio, // coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   July 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with electric heating coil

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (force to 0)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if ( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );

		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PartLoadRatio ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity

		// Calculate residual based on output magnitude
		if ( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		} else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}

	Real64
		CalcFanCoilHWLoadResidual(
		Real64 const HWFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
		)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl, FSEC
		//       DATE WRITTEN   Jan 2016
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with electric heating coil

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na
		
		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (force to 0)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if ( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		}
		else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );

		Node( FanCoil( FanCoilNum ).HotControlNode ).MassFlowRate = HWFlow;
		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 ); 

		// Calculate residual based on output magnitude
		if ( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		}
		else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}

	Real64
		CalcFanCoilCWLoadResidual(
		Real64 const CWFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
		)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl Jan 2016
		//       DATE WRITTEN   July 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with electric heating coil

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (force to 0)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if ( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		}
		else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );

		Node( FanCoil( FanCoilNum ).ColdControlNode ).MassFlowRate = CWFlow;
		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 ); 

		// Calculate residual based on output magnitude
		if ( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		}
		else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}
	Real64
	CalcFanCoilWaterFlowTempResidual(
		Real64 const WaterFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
	)
	{
		
				// FUNCTION INFORMATION:
				//       AUTHOR         Richard Raustad, FSEC
				//       DATE WRITTEN   December 2015
				//       MODIFIED       na
				//       RE-ENGINEERED  na
			
				// PURPOSE OF THIS SUBROUTINE:
				// To calculate the part-load ratio for the FCU with varying water flow rate
			
				// METHODOLOGY EMPLOYED:
				// Use SolveRegulaFalsi to CALL this Function to converge on a solution
			
				// REFERENCES:
				// na
			
				// USE STATEMENTS:
				// na
			
				// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

				// Argument array dimensioning
			
				// Locals
				// SUBROUTINE ARGUMENT DEFINITIONS:
			
				//   Parameter description example:
				//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
				//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
				//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
				//       Par(4)  = QZnReq               ! zone load [W]
				//       Par(5)  = WaterControlNode     ! CW or HW control node number
			
				// SUBROUTINE PARAMETER DEFINITIONS:
				// na
			
				// INTERFACE BLOCK SPECIFICATIONS
				// na
			
				// DERIVED TYPE DEFINITIONS
				// na
			
				// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		int WaterControlNode; // water node to control
		Real64 OutletTemp; // FCU outlet temperature SP [C]
		Real64 QUnitOut; // delivered capacity [W]
		Real64 QZnReq;
		Real64 FCOutletTempOn; // FCU outlet temperature
		
		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if ( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		OutletTemp = Par( 4 );
		QZnReq = Par( 5 );
		WaterControlNode = int( Par( 6 ) );
		
		if ( WaterControlNode == FanCoil( FanCoilNum ).ColdControlNode || ( WaterControlNode == FanCoil( FanCoilNum ).HotControlNode && FanCoil( FanCoilNum ).HCoilType_Num != HCoil_Electric ) ) {
			Node( WaterControlNode ).MassFlowRate = WaterFlow;
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 0.0 ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity
		} else {
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity
		}
		
		FCOutletTempOn = Node( FanCoil( FanCoilNum ).AirOutNode ).Temp;
		// Calculate residual based on output magnitude
		Residuum = ( FCOutletTempOn - OutletTemp );
		
		return Residuum;
	}
	
	Real64
	CalcFanCoilWaterFlowResidual(
		Real64 const WaterFlow, // water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
	)
	{
		
				// FUNCTION INFORMATION:
				//       AUTHOR         Richard Raustad, FSEC
				//       DATE WRITTEN   December 2015
				//       MODIFIED       na
				//       RE-ENGINEERED  na
			
				// PURPOSE OF THIS SUBROUTINE:
				// To calculate the part-load ratio for the FCU with varying water flow rate
			
				// METHODOLOGY EMPLOYED:
				// Use SolveRegulaFalsi to CALL this Function to converge on a solution
			
				// REFERENCES:
				// na
			
				// USE STATEMENTS:
				// na
			
				// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

				// Argument array dimensioning
			
				// Locals
				// SUBROUTINE ARGUMENT DEFINITIONS:
			
				//   Parameter description example:
				//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
				//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
				//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
				//       Par(4)  = QZnReq               ! zone load [W]
				//       Par(5)  = WaterControlNode     ! CW or HW control node number
			
				// SUBROUTINE PARAMETER DEFINITIONS:
				// na
			
				// INTERFACE BLOCK SPECIFICATIONS
				// na
			
				// DERIVED TYPE DEFINITIONS
				// na
			
				// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		int WaterControlNode; // water node to control
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]
		
		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if ( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );
		WaterControlNode = int( Par( 5 ) );
		
		if ( WaterControlNode == FanCoil( FanCoilNum ).ColdControlNode || ( WaterControlNode == FanCoil( FanCoilNum ).HotControlNode && FanCoil( FanCoilNum ).HCoilType_Num != HCoil_Electric ) ) {
			Node( WaterControlNode ).MassFlowRate = WaterFlow;
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 0.0 ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity
		} else {
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity
		}
		
		// Calculate residual based on output magnitude
		if ( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		} else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}
		
		return Residuum;
	}

	Real64
	CalcFanCoilAirFlowResidual(
		Real64 const AirFlow, // air mass flow rate [kg/s]
		Array1< Real64 > const & Par // Function parameters
		)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   December 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with varying water flow rate

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]
		//       Par(5)  = AirInletNode     ! equipment air inlet node number

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		int AirInletNode; // air node to modulate
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );
		AirInletNode = int( Par( 5 ) );

		Node( AirInletNode ).MassFlowRate = AirFlow;
		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity

		// Calculate residual based on output magnitude
		if( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		} else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}

	Real64
	CalcFanCoilAirAndWaterFlowResidual(
		Real64 const PLR, // water and air part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   December 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with varying water flow rate

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]
		//       Par(5)  = WaterControlNode     ! CW or HW control node number

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		int WaterControlNode; // water node to control
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]
		Real64 MinWaterFlow; // water flow rate that meets reduced zone load with reduced air flow rate

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );
		WaterControlNode = int( Par( 5 ) );
		MinWaterFlow = Par( 6 );

		// set air flow rate
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = FanCoil( FanCoilNum ).MaxAirMassFlow * ( FanCoil( FanCoilNum ).LowSpeedRatio + ( PLR * ( 1.0 - FanCoil( FanCoilNum ).LowSpeedRatio ) ) );
		// set water flow rate
		if( WaterControlNode == FanCoil( FanCoilNum ).ColdControlNode ) {
			Node( WaterControlNode ).MassFlowRate = MinWaterFlow + ( PLR * ( FanCoil( FanCoilNum ).MaxColdWaterFlow - MinWaterFlow ) );
		} else if( WaterControlNode == FanCoil( FanCoilNum ).HotControlNode ) {
			Node( WaterControlNode ).MassFlowRate = MinWaterFlow + ( PLR * ( FanCoil( FanCoilNum ).MaxHotWaterFlow - MinWaterFlow ) );
		} else {
			// developer error
			ShowFatalError( "Developer Error - CalcFanCoilAirAndWaterFlowResidual: Water control node not found for " + FanCoil( FanCoilNum ).Name );
		}
		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity

		// Calculate residual based on output magnitude
		if( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		} else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}

	Real64
	CalcFanCoilAirAndWaterInStepResidual(
		Real64 const PLR, // water and air part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   December 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with varying water flow rate

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]
		//       Par(5)  = WaterControlNode     ! CW or HW control node number

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		int WaterControlNode; // water node to control
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]
		Real64 MinWaterFlow; // water flow rate that meets zone load
		Real64 MinAirFlow; // air flow rate that meets zone load
		Real64 MinHeaterPLR; // PLR of heating coil prior to increasing fan coil capacity

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );
		WaterControlNode = int( Par( 5 ) );
		MinAirFlow = Par( 7 );

		// set air flow rate
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = MinAirFlow + ( PLR * ( FanCoil( FanCoilNum ).MaxAirMassFlow - MinAirFlow ) );
		// set water flow rate
		if ( WaterControlNode == FanCoil( FanCoilNum ).ColdControlNode ) {
			MinWaterFlow = Par( 6 );
			Node( WaterControlNode ).MassFlowRate = MinWaterFlow + ( PLR * ( FanCoil( FanCoilNum ).MaxColdWaterFlow - MinWaterFlow ) );
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 );
		} else if ( WaterControlNode == 0 ) { // do this before the water coil else if block because 0 = 0
			MinHeaterPLR = Par( 6 );
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, MinHeaterPLR + ( PLR * ( 1.0 - MinHeaterPLR ) ) );
		} else if ( WaterControlNode == FanCoil( FanCoilNum ).HotControlNode ) {
			MinWaterFlow = Par( 6 );
			Node( WaterControlNode ).MassFlowRate = MinWaterFlow + ( PLR * ( FanCoil( FanCoilNum ).MaxHotWaterFlow - MinWaterFlow ) );
			Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 );
		} else {
			// developer error
			ShowFatalError( "Developer Error - CalcFanCoilAirAndWaterFlowResidual: Water control node not found for " + FanCoil( FanCoilNum ).Name );
		}

		// Calculate residual based on output magnitude
		if( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		} else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}

	Real64
	CalcFanCoilBothFlowResidual(
		Real64 const PLR, // water and air part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   December 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with varying water flow rate

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]
		//       Par(5)  = Low_mdot             ! CW or HW minimum flow
		//       Par(6)  = mdot                 ! CW or HW maximum flow

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		int WaterControlNode; // water node to control
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]
		Real64 MinWaterFlow; // water flow rate that meets reduced zone load with reduced air flow rate
		Real64 MaxWaterFlow; // water flow rate that meets design zone load with maximum air flow rate

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );
		MinWaterFlow = Par( 5 );
		MaxWaterFlow = Par( 6 );
		WaterControlNode = Par( 7 );

		// set air flow rate
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = FanCoil( FanCoilNum ).MaxAirMassFlow * ( FanCoil( FanCoilNum ).LowSpeedRatio + ( PLR * ( 1.0 - FanCoil( FanCoilNum ).LowSpeedRatio ) ) );
		// set water flow rate
		Node( WaterControlNode ).MassFlowRate = MinWaterFlow + ( PLR * ( MaxWaterFlow - MinWaterFlow ) );
		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, 1.0 ); // needs PLR=0 for electric heating coil, otherwise will run a full capacity

		// Calculate residual based on output magnitude
		if( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		} else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}

	Real64
	CalcFanCoilElecHeatResidual(
		Real64 const PLR, // water and air part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   December 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with varying water flow rate

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]
		//       Par(5)                         ! not applicable

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		Real64 QZnReq; // Sensible load to be met [W]
		Real64 QUnitOut; // delivered capacity [W]
		Real64 MaxAirFlow; // maximum fan coil fan flow rate [kg/s]

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		QZnReq = Par( 4 );

		if ( Par( 6 ) == -1.0 ) {
			MaxAirFlow = Par( 5 );
			// set air flow rate
			Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = PLR * MaxAirFlow;
		}
		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );

		// Calculate residual based on output magnitude
		if( std::abs( QZnReq ) <= 100.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / 100.0;
		} else {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;
	}

	Real64
	CalcFanCoilElecHeatTempResidual(
		Real64 const PLR, // water and air part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   December 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the FCU with varying water flow rate

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to CALL this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // Result (forces solution to be within tolerance)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//       Par(1)  = REAL(FanCoilNum,r64) ! Index to fan coil unit
		//       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//       Par(3)  = REAL(ControlledZoneNum,r64)     ! zone index
		//       Par(4)  = QZnReq               ! zone load [W]
		//       Par(5)                         ! not applicable

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanCoilNum; // Index to this fan coil unit
		bool FirstHVACIteration; // FirstHVACIteration flag
		int ControlledZoneNum; // zone index
		Real64 MaxOutletTemp; // maximum supply air Temperature [C]
		Real64 QUnitOut; // delivered capacity [W]
		Real64 MaxAirFlow; // maximum fan coil fan flow rate [kg/s]
		Real64 FCOutletTempOn; // FCU outlet temperature

		// Convert parameters to usable variables
		FanCoilNum = int( Par( 1 ) );
		if( Par( 2 ) == 1.0 ) {
			FirstHVACIteration = true;
		} else {
			FirstHVACIteration = false;
		}
		ControlledZoneNum = int( Par( 3 ) );
		MaxOutletTemp = Par( 4 );

		if ( Par( 6 ) == -1.0 ) {
			MaxAirFlow = Par( 5 );
			// set air flow rate
			Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = max( Par( 7 ), PLR * MaxAirFlow );
		}
		Calc4PipeFanCoil( FanCoilNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, PLR );
		FCOutletTempOn = Node( FanCoil( FanCoilNum ).AirOutNode ).Temp;

		// Calculate residual based on output magnitude
		Residuum = ( FCOutletTempOn - MaxOutletTemp );

		return Residuum;
	}

} // FanCoilUnits

} // EnergyPlus
