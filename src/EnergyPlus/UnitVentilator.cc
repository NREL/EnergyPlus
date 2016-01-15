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
#include <UnitVentilator.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalance.hh>
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
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace UnitVentilator {

	// Module containing the routines dealing with the Unit Ventilator

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   May 2000
	//       MODIFIED       March 2001   (addition of gas and electric coils)
	//                      October 2003 (addition of cooling coil type)
	//       MODIFIED       Bereket Nigusse, FSEC, October 2013, Added cycling fan operating mode
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To simulate unit ventilators.

	// METHODOLOGY EMPLOYED:
	// Units are modeled as a collection of components: outside air mixer,
	// fan, heating coil and/or cooling coil plus an integrated control
	// algorithm that adjusts the hot or cold water flow to meet the zone
	// load.  Outside air mixing is handled locally as either fixed percent
	// or as attempting to meet a prescribed mixed air temperature.

	// REFERENCES:
	// ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.1-31.3
	// Fred Buhl's fan coil module (FanCoilUnits.cc)

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
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::CycFanCycCoil;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using namespace Psychrometrics;
	using namespace FluidProperties;

	// Data
	// MODULE PARAMETER DEFINITIONS

	// Currrent Module Unit type
	std::string const cMO_UnitVentilator( "ZoneHVAC:UnitVentilator" );

	// Parameters for outside air control types:
	int const Heating_ElectricCoilType( 1 );
	int const Heating_GasCoilType( 2 );
	int const Heating_WaterCoilType( 3 );
	int const Heating_SteamCoilType( 4 );
	int const Cooling_CoilWaterCooling( 1 );
	int const Cooling_CoilDetailedCooling( 2 );
	int const Cooling_CoilHXAssisted( 3 );
	// OA operation modes
	int const VariablePercent( 1 );
	int const FixedTemperature( 2 );
	int const FixedOAControl( 3 );
	// coil operation
	int const On( 1 ); // normal coil operation
	int const Off( 0 ); // signal coil shouldn't run
	int const NoneOption( 0 );
	int const BothOption( 1 );
	int const HeatingOption( 2 );
	int const CoolingOption( 3 );

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	bool HCoilOn( false ); // TRUE if the heating coil (gas or electric especially) should be running
	int NumOfUnitVents( 0 ); // Number of unit ventilators in the input file
	Real64 OAMassFlowRate( 0.0 ); // Outside air mass flow rate for the unit ventilator
	Real64 QZnReq( 0.0 ); // heating or cooling needed by zone [watts]
	Array1D_bool MySizeFlag;
	bool GetUnitVentilatorInputFlag( true ); // First time, input is "gotten"
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE UnitVentilator
	//PRIVATE UpdateUnitVentilator

	// Object Data
	Array1D< UnitVentilatorData > UnitVent;
	Array1D< UnitVentNumericFieldData > UnitVentNumericFields;

	// Functions

	void
	clear_state()
	{
		HCoilOn = false;
		NumOfUnitVents = 0;
		OAMassFlowRate = 0.0;
		QZnReq = 0.0;
		GetUnitVentilatorInputFlag = true;
		MySizeFlag.deallocate();
		CheckEquipName.deallocate();
		UnitVent.deallocate();
		UnitVentNumericFields.deallocate();
	}

	void
	SimUnitVentilator(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is the main driver subroutine for the Unit Ventilator simulation.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataSizing::ZoneEqUnitVent;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UnitVentNum; // index of unit ventilator being simulated

		// FLOW:
		if ( GetUnitVentilatorInputFlag ) {
			GetUnitVentilatorInput();
			GetUnitVentilatorInputFlag = false;
		}

		// Find the correct Unit Ventilator Equipment
		if ( CompIndex == 0 ) {
			UnitVentNum = FindItemInList( CompName, UnitVent );
			if ( UnitVentNum == 0 ) {
				ShowFatalError( "SimUnitVentilator: Unit not found=" + CompName );
			}
			CompIndex = UnitVentNum;
		} else {
			UnitVentNum = CompIndex;
			if ( UnitVentNum > NumOfUnitVents || UnitVentNum < 1 ) {
				ShowFatalError( "SimUnitVentilator:  Invalid CompIndex passed=" + TrimSigDigits( UnitVentNum ) + ", Number of Units=" + TrimSigDigits( NumOfUnitVents ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( UnitVentNum ) ) {
				if ( CompName != UnitVent( UnitVentNum ).Name ) {
					ShowFatalError( "SimUnitVentilator: Invalid CompIndex passed=" + TrimSigDigits( UnitVentNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + UnitVent( UnitVentNum ).Name );
				}
				CheckEquipName( UnitVentNum ) = false;
			}
		}

		ZoneEqUnitVent = true;

		InitUnitVentilator( UnitVentNum, FirstHVACIteration, ZoneNum );

		CalcUnitVentilator( UnitVentNum, ZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided );

		//  CALL UpdateUnitVentilator

		ReportUnitVentilator( UnitVentNum );

		ZoneEqUnitVent = false;
	}

	void
	GetUnitVentilatorInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
		//                      Bereket Nigusse, FSEC, April 2011: eliminated input node names
		//                                                         & added fan object type
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine obtains the input for unit ventilators and sets
		// up the appropriate derived type.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// Fred Buhl's fan coil module (FanCoilUnits.cc)

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		auto & GetWaterCoilMaxFlowRate( WaterCoils::GetCoilMaxWaterFlowRate );
		using WaterCoils::GetCoilWaterInletNode;
		auto & GetSteamCoilMaxFlowRate( SteamCoils::GetCoilMaxWaterFlowRate );
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilSteamInletNode;
		auto & GetHXAssistedCoilFlowRate( HVACHXAssistedCoolingCoil::GetCoilMaxWaterFlowRate );
		auto & GetHXCoilWaterInletNode( HVACHXAssistedCoolingCoil::GetCoilWaterInletNode );
		using HVACHXAssistedCoolingCoil::GetHXCoilTypeAndName;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using Fans::GetFanType;
		using Fans::GetFanOutletNode;
		using Fans::GetFanAvailSchPtr;
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleVAV;
		using DataHVACGlobals::FanType_SimpleOnOff;

		using DataSizing::AutoSize;
		using DataSizing::ZoneHVACSizing;
		using General::TrimSigDigits;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataGlobals::NumOfZones;
		using DataGlobals::ScheduleAlwaysOn;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetUnitVentilatorInput: " ); // include trailing blank

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		int NumFields; // Total number of fields in object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int UnitVentNum; // Item to be "gotten"
		bool IsValid; // Set for outside air node check
		static bool errFlag( false ); // interim error flag
		std::string cCoolingCoilType; // Cooling coil object type
		std::string cHeatingCoilType; // Heating coil object type
		int FanIndex; // index to fan used for flow checks
		Real64 FanVolFlow; // volumetric flow rate of fan
		std::string CurrentModuleObject;
		Array1D_string Alphas; // Alpha items for object
		Array1D< Real64 > Numbers; // Numeric items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		int CtrlZone; // index to loop counter
		int NodeNum; // index to loop counter
		bool ZoneNodeNotFound; // used in error checking

		// FLOW:

		// Figure out how many unit ventilators there are in the input file

		CurrentModuleObject = cMO_UnitVentilator;
		NumOfUnitVents = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		Numbers.dimension( NumNumbers, 0.0 );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// Allocate the local derived type and do one-time initializations for all parts of it
		if ( NumOfUnitVents > 0 ) {
			UnitVent.allocate( NumOfUnitVents );
			CheckEquipName.allocate( NumOfUnitVents );
			UnitVentNumericFields.allocate( NumOfUnitVents );
		}
		CheckEquipName = true;

		for ( UnitVentNum = 1; UnitVentNum <= NumOfUnitVents; ++UnitVentNum ) { // Begin looping over all of the unit ventilators found in the input file...

			GetObjectItem( CurrentModuleObject, UnitVentNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			UnitVentNumericFields( UnitVentNum ).FieldNames.allocate (NumNumbers );
			UnitVentNumericFields( UnitVentNum ).FieldNames = "";
			UnitVentNumericFields( UnitVentNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), UnitVent, UnitVentNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}

			UnitVent( UnitVentNum ).Name = Alphas( 1 );
			UnitVent( UnitVentNum ).SchedName = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				UnitVent( UnitVentNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				UnitVent( UnitVentNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( UnitVent( UnitVentNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", invalid" );
					ShowContinueError( "not found: " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			UnitVent( UnitVentNum ).MaxAirVolFlow = Numbers( 1 );

			// Outside air information:
			UnitVent( UnitVentNum ).MinOutAirVolFlow = Numbers( 2 );

			UnitVent( UnitVentNum ).MinOASchedName = Alphas( 4 );
			UnitVent( UnitVentNum ).MinOASchedPtr = GetScheduleIndex( Alphas( 4 ) ); // convert schedule name to pointer
			if ( UnitVent( UnitVentNum ).MinOASchedPtr == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
				ShowContinueError( "not found: " + cAlphaFields( 4 ) + "=\"" + Alphas( 4 ) + "\"." );
				ErrorsFound = true;
			}

			UnitVent( UnitVentNum ).OutAirVolFlow = Numbers( 3 );
			cCoolingCoilType = "";
			cHeatingCoilType = "";

			{ auto const SELECT_CASE_var( Alphas( 3 ) );
			if ( SELECT_CASE_var == "VARIABLEPERCENT" ) {
				UnitVent( UnitVentNum ).OAControlType = VariablePercent;
				UnitVent( UnitVentNum ).MaxOASchedName = Alphas( 5 );
				UnitVent( UnitVentNum ).MaxOASchedPtr = GetScheduleIndex( Alphas( 5 ) ); // convert schedule name to pointer
				if ( UnitVent( UnitVentNum ).MaxOASchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
					ShowContinueError( "not found:" + cAlphaFields( 5 ) + "=\"" + UnitVent( UnitVentNum ).MaxOASchedName + "\"." );
					ErrorsFound = true;
				} else if ( ! CheckScheduleValueMinMax( UnitVent( UnitVentNum ).MaxOASchedPtr, ">=0", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
					ShowContinueError( "out of range [0,1]: " + cAlphaFields( 5 ) + "=\"" + UnitVent( UnitVentNum ).MaxOASchedName + "\"." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == "FIXEDAMOUNT" ) {
				UnitVent( UnitVentNum ).OAControlType = FixedOAControl;
				UnitVent( UnitVentNum ).MaxOASchedName = Alphas( 5 );
				UnitVent( UnitVentNum ).MaxOASchedPtr = GetScheduleIndex( Alphas( 5 ) ); // convert schedule name to pointer
				if ( UnitVent( UnitVentNum ).MaxOASchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 5 ) + " not found = " + UnitVent( UnitVentNum ).MaxOASchedName );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitVent( UnitVentNum ).Name );
					ErrorsFound = true;
				} else if ( ! CheckScheduleValueMinMax( UnitVent( UnitVentNum ).MaxOASchedPtr, ">=0", 0.0 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
					ShowContinueError( "out of range [0,1]: " + cAlphaFields( 5 ) + "=\"" + UnitVent( UnitVentNum ).MaxOASchedName + "\"." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == "FIXEDTEMPERATURE" ) {
				UnitVent( UnitVentNum ).OAControlType = FixedTemperature;
				UnitVent( UnitVentNum ).TempSchedName = Alphas( 5 );
				UnitVent( UnitVentNum ).TempSchedPtr = GetScheduleIndex( Alphas( 5 ) ); // convert schedule name to pointer
				if ( UnitVent( UnitVentNum ).TempSchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
					ShowContinueError( " not found: " + cAlphaFields( 5 ) + "=\"" + UnitVent( UnitVentNum ).MaxOASchedName + "\"." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
				ShowContinueError( "Illegal " + cAlphaFields( 3 ) + "=\"" + Alphas( 3 ) + "\"." );
			}}

			// Main air nodes (except outside air node):
			// For node connections, this object is both a parent and a non-parent, because the
			// OA mixing box is not called out as a separate component, its nodes must be connected
			// as ObjectIsNotParent.  But for the fan and coils, the nodes are connected as ObjectIsParent
			// To support the diagramming tool, the unit ventilator inlet node must appear both as
			// an inlet to the unit ventilator parent object and as an inlet to the implied
			// non-parent OA mixing box within the unit ventilator.
			// Because there is overlap between the nodes that are parent and non-parent, use a different
			// object type for the non parent nodes
			UnitVent( UnitVentNum ).AirInNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			UnitVent( UnitVentNum ).AirInNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			UnitVent( UnitVentNum ).AirOutNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			// Fan information:
			//   A11, \field Supply Air Fan Object Type
			//        \required-field
			//        \type choice
			//        \key Fan:ConstantVolume
			//        \key Fan:VariableVolume
			//        \note Allowable fan types are Fan:ConstantVolume and
			//        \note Fan:VariableVolume
			//   A12, \field Fan Name
			//        \required-field
			//        \type object-list
			//        \object-list FansCVandVAV

			UnitVent( UnitVentNum ).FanType = Alphas( 11 );
			UnitVent( UnitVentNum ).FanName = Alphas( 12 );
			errFlag = false;
			ValidateComponent( UnitVent( UnitVentNum ).FanType, UnitVent( UnitVentNum ).FanName, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + UnitVent( UnitVentNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				GetFanType( UnitVent( UnitVentNum ).FanName, UnitVent( UnitVentNum ).FanType_Num, errFlag, CurrentModuleObject, UnitVent( UnitVentNum ).Name );

				{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).FanType_Num );
				if ( ( SELECT_CASE_var == FanType_SimpleConstVolume ) || ( SELECT_CASE_var == FanType_SimpleVAV ) || ( SELECT_CASE_var == FanType_SimpleOnOff ) ) {

					// Get fan outlet node
					UnitVent( UnitVentNum ).FanOutletNode = GetFanOutletNode( UnitVent( UnitVentNum ).FanType, UnitVent( UnitVentNum ).FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + UnitVent( UnitVentNum ).Name + "\"." );
						ErrorsFound = true;
					} else {
						GetFanIndex( UnitVent( UnitVentNum ).FanName, FanIndex, errFlag, CurrentModuleObject );
						// Other error checks should trap before it gets to this point in the code, but including just in case.

						GetFanVolFlow( FanIndex, FanVolFlow );
						if ( FanVolFlow != AutoSize && UnitVent( UnitVentNum ).MaxAirVolFlow != AutoSize && FanVolFlow < UnitVent( UnitVentNum ).MaxAirVolFlow ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"" );
							ShowContinueError( "...air flow rate [" + TrimSigDigits( FanVolFlow, 7 ) + "] in fan object " + UnitVent( UnitVentNum ).FanName + " is less than the unit ventilator maximum supply air flow rate [" + TrimSigDigits( UnitVent( UnitVentNum ).MaxAirVolFlow, 7 ) + "]." );
							ShowContinueError( "...the fan flow rate must be greater than or equal to the unit ventilator maximum supply air flow rate." );
							ErrorsFound = true;
						} else if ( FanVolFlow == AutoSize && UnitVent( UnitVentNum ).MaxAirVolFlow != AutoSize ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"" );
							ShowContinueError( "...the fan flow rate is autosized while the unit ventilator flow rate is not." );
							ShowContinueError( "...this can lead to unexpected results where the fan flow rate is less than required." );
						} else if ( FanVolFlow != AutoSize && UnitVent( UnitVentNum ).MaxAirVolFlow == AutoSize ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"" );
							ShowContinueError( "...the unit ventilator flow rate is autosized while the fan flow rate is not." );
							ShowContinueError( "...this can lead to unexpected results where the fan flow rate is less than required." );
						}
						// Get the fan's availability schedule
						errFlag = false;
						UnitVent( UnitVentNum ).FanAvailSchedPtr = GetFanAvailSchPtr( UnitVent( UnitVentNum ).FanType, UnitVent( UnitVentNum ).FanName, errFlag );
						if ( errFlag ) {
							ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"" );
							ErrorsFound = true;
						}
					}
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"" );
					ShowContinueError( "Fan Type must be Fan:ConstantVolume or Fan:VariableVolume." );
					ErrorsFound = true;
				}}
			}
			// For node connections, this object is both a parent and a non-parent, because the
			// OA mixing box is not called out as a separate component, its nodes must be connected
			// as ObjectIsNotParent.  But for the fan and coils, the nodes are connected as ObjectIsParent
			// Because there is overlap between the nodes that are parent and non-parent, use a different
			// object type for the non parent nodes
			//  Set connection type to 'OutdoorAir', because this is hardwired to OA conditions
			UnitVent( UnitVentNum ).OutsideAirNode = GetOnlySingleNode( Alphas( 8 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", Alphas( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
			if ( ( ! lAlphaBlanks( 8 ) ) ) {
				CheckAndAddAirNodeNumber( UnitVent( UnitVentNum ).OutsideAirNode, IsValid );
				if ( ! IsValid ) {
					ShowWarningError( RoutineName + CurrentModuleObject + ", Adding " + cAlphaFields( 8 ) + '=' + Alphas( 8 ) );
				}
			}

			UnitVent( UnitVentNum ).AirReliefNode = GetOnlySingleNode( Alphas( 9 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", Alphas( 1 ), NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsNotParent );

			UnitVent( UnitVentNum ).OAMixerOutNode = GetOnlySingleNode( Alphas( 10 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ( UnitVent( UnitVentNum ).OAControlType == FixedOAControl ) {
				UnitVent( UnitVentNum ).OutAirVolFlow = UnitVent( UnitVentNum ).MinOutAirVolFlow;
				UnitVent( UnitVentNum ).MaxOASchedName = UnitVent( UnitVentNum ).MinOASchedName;
				UnitVent( UnitVentNum ).MaxOASchedPtr = GetScheduleIndex( UnitVent( UnitVentNum ).MinOASchedName );
			}

			// Add fan to component sets array
			SetUpCompSets( CurrentModuleObject, UnitVent( UnitVentNum ).Name, UnitVent( UnitVentNum ).FanType, UnitVent( UnitVentNum ).FanName, NodeID( UnitVent( UnitVentNum ).OAMixerOutNode ), NodeID( UnitVent( UnitVentNum ).FanOutletNode ) );

			if ( ! lAlphaBlanks( 18 ) ) {
				UnitVent( UnitVentNum ).AvailManagerListName = Alphas( 18 );
			}

			UnitVent( UnitVentNum ).HVACSizingIndex = 0;
			if (!lAlphaBlanks( 20 )) {
				UnitVent( UnitVentNum ).HVACSizingIndex = FindItemInList( Alphas( 20 ), ZoneHVACSizing );
				if (UnitVent( UnitVentNum ).HVACSizingIndex == 0) {
					ShowSevereError( cAlphaFields( 20 ) + " = " + Alphas( 20 ) + " not found.");
					ShowContinueError( "Occurs in " + cMO_UnitVentilator + " = " + UnitVent(UnitVentNum).Name );
					ErrorsFound = true;
				}
			}
			//   A13, \field Coil Option
			//        \required-field
			//        \type choice
			//        \key None
			//        \key Heating
			//        \key Cooling
			//        \key HeatingAndCooling

			{ auto const SELECT_CASE_var( Alphas( 13 ) );
			if ( SELECT_CASE_var == "HEATINGANDCOOLING" ) {
				UnitVent( UnitVentNum ).CoilOption = BothOption;
			} else if ( SELECT_CASE_var == "HEATING" ) {
				UnitVent( UnitVentNum ).CoilOption = HeatingOption;
			} else if ( SELECT_CASE_var == "COOLING" ) {
				UnitVent( UnitVentNum ).CoilOption = CoolingOption;
			} else if ( SELECT_CASE_var == "NONE" ) {
				UnitVent( UnitVentNum ).CoilOption = NoneOption;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
				ShowContinueError( "illegal value: " + cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\"." );
				ErrorsFound = true;
			}}

			UnitVent( UnitVentNum ).FanSchedPtr = GetScheduleIndex( Alphas( 14 ) );
			// Default to cycling fan when fan mode schedule is not present
			if ( ! lAlphaBlanks( 14 ) && UnitVent( UnitVentNum ).FanSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + " \"" + UnitVent( UnitVentNum ).Name + "\" " + cAlphaFields( 14 ) + " not found: " + Alphas( 14 ) );
				ErrorsFound = true;
			} else if ( lAlphaBlanks( 14 ) ) {
				if ( UnitVent( UnitVentNum ).FanType_Num == FanType_SimpleOnOff ) {
					UnitVent( UnitVentNum ).OpMode = CycFanCycCoil;
				} else {
					UnitVent( UnitVentNum ).OpMode = ContFanCycCoil;
				}
			}

			// Check fan's schedule for cycling fan operation if constant volume fan is used
			if ( UnitVent( UnitVentNum ).FanSchedPtr > 0 && UnitVent( UnitVentNum ).FanType_Num == FanType_SimpleConstVolume ) {
				if ( ! CheckScheduleValueMinMax( UnitVent( UnitVentNum ).FanSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "For " + cAlphaFields( 11 ) + " = " + Alphas( 11 ) );
					ShowContinueError( "Fan operating mode must be continuous (fan operating mode schedule values > 0)." );
					ShowContinueError( "Error found in " + cAlphaFields( 14 ) + " = " + Alphas( 14 ) );
					ShowContinueError( "...schedule values must be (>0., <=1.)" );
					ErrorsFound = true;
				}
			}

			// Get Coil information
			if ( UnitVent( UnitVentNum ).CoilOption == BothOption || UnitVent( UnitVentNum ).CoilOption == HeatingOption ) {
				// Heating coil information:
				// A14, \field Heating Coil Object Type
				//      \type choice
				//      \key Coil:Heating:Water
				//      \key Coil:Heating:Electric
				//      \key Coil:Heating:Gas
				//      \key Coil:Heating:Steam
				// A15, \field Heating Coil Name
				//      \type object-list
				//      \object-list HeatingCoilName
				if ( ( ! lAlphaBlanks( 16 ) ) ) {
					UnitVent( UnitVentNum ).HCoilPresent = true;
					errFlag = false;

					cHeatingCoilType = Alphas( 15 );
					UnitVent( UnitVentNum ).HCoilTypeCh = cHeatingCoilType;
					{ auto const SELECT_CASE_var( cHeatingCoilType );
					if ( SELECT_CASE_var == "COIL:HEATING:WATER" ) {
						UnitVent( UnitVentNum ).HCoilType = Heating_WaterCoilType;
						UnitVent( UnitVentNum ).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
					} else if ( SELECT_CASE_var == "COIL:HEATING:STEAM" ) {
						UnitVent( UnitVentNum ).HCoilType = Heating_SteamCoilType;
						UnitVent( UnitVentNum ).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
					} else if ( SELECT_CASE_var == "COIL:HEATING:ELECTRIC" ) {
						UnitVent( UnitVentNum ).HCoilType = Heating_ElectricCoilType;
					} else if ( SELECT_CASE_var == "COIL:HEATING:GAS" ) {
						UnitVent( UnitVentNum ).HCoilType = Heating_GasCoilType;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
						ShowContinueError( "illegal value: " + cAlphaFields( 15 ) + "=\"" + Alphas( 15 ) + "\"." );
						ErrorsFound = true;
						errFlag = true;
					}}
					if ( ! errFlag ) {
						UnitVent( UnitVentNum ).HCoilName = Alphas( 16 );
						ValidateComponent( cHeatingCoilType, UnitVent( UnitVentNum ).HCoilName, IsNotOK, CurrentModuleObject );
						if ( IsNotOK ) {
							ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"." );
							ErrorsFound = true;
						} else {
							// The heating coil control node is necessary for a hot water coil, but not necessary for an
							// electric or gas coil.
							if ( UnitVent( UnitVentNum ).HCoilType == Heating_WaterCoilType || UnitVent( UnitVentNum ).HCoilType == Heating_SteamCoilType ) {
								// mine the hot water or steam node from the coil object
								errFlag = false;
								if ( UnitVent( UnitVentNum ).HCoilType == Heating_WaterCoilType ) {
									UnitVent( UnitVentNum ).HotControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", UnitVent( UnitVentNum ).HCoilName, errFlag );
								} else {
									UnitVent( UnitVentNum ).HCoil_Index = GetSteamCoilIndex( "COIL:HEATING:STEAM", UnitVent( UnitVentNum ).HCoilName, errFlag );
									UnitVent( UnitVentNum ).HotControlNode = GetCoilSteamInletNode( UnitVent( UnitVentNum ).HCoil_Index, UnitVent( UnitVentNum ).HCoilName, errFlag );
								}
								// Other error checks should trap before it gets to this point in the code, but including just in case.
								if ( errFlag ) {
									ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"." );
									ErrorsFound = true;
								}
							}
						}
					}

					UnitVent( UnitVentNum ).MinVolHotWaterFlow = 0.0;
					UnitVent( UnitVentNum ).MinVolHotSteamFlow = 0.0;

					UnitVent( UnitVentNum ).HotControlOffset = Numbers( 4 );
					// Set default convergence tolerance
					if ( UnitVent( UnitVentNum ).HotControlOffset <= 0.0 ) {
						UnitVent( UnitVentNum ).HotControlOffset = 0.001;
					}
					{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).HCoilType );

					if ( SELECT_CASE_var == Heating_WaterCoilType ) {
						UnitVent( UnitVentNum ).MaxVolHotWaterFlow = GetWaterCoilMaxFlowRate( "Coil:Heating:Water", UnitVent( UnitVentNum ).HCoilName, ErrorsFound );
						UnitVent( UnitVentNum ).MaxVolHotSteamFlow = UnitVent( UnitVentNum ).MaxVolHotWaterFlow;

					} else if ( SELECT_CASE_var == Heating_SteamCoilType ) {
						UnitVent( UnitVentNum ).MaxVolHotWaterFlow = GetSteamCoilMaxFlowRate( "Coil:Heating:Steam", UnitVent( UnitVentNum ).HCoilName, ErrorsFound );
						UnitVent( UnitVentNum ).MaxVolHotSteamFlow = UnitVent( UnitVentNum ).MaxVolHotWaterFlow;

					} else if ( SELECT_CASE_var == Heating_ElectricCoilType ) {
					} else if ( SELECT_CASE_var == Heating_GasCoilType ) {
					} else {
					}}
				} else { // heating coil is required for these options
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", missing heating coil" );
					ShowContinueError( "a heating coil is required for " + cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\"." );
					ErrorsFound = true;
				} // IF (.NOT. lAlphaBlanks(15)) THEN - from the start of heating coil information
			} // is option both or heating only

			if ( UnitVent( UnitVentNum ).CoilOption == BothOption || UnitVent( UnitVentNum ).CoilOption == CoolingOption ) {
				// Cooling coil information (if one is present):
				// A16, \field Cooling Coil Object Type
				//      \type choice
				//      \key Coil:Cooling:Water
				//      \key Coil:Cooling:Water:DetailedGeometry
				//      \key CoilSystem:Cooling:Water:HeatExchangerAssisted
				// A17, \field Cooling Coil Name
				//      \type object-list
				//      \object-list CoolingCoilsWater
				if ( ! lAlphaBlanks( 18 ) ) {
					UnitVent( UnitVentNum ).CCoilPresent = true;
					errFlag = false;

					cCoolingCoilType = Alphas( 17 );
					UnitVent( UnitVentNum ).CCoilTypeCh = cCoolingCoilType;
					{ auto const SELECT_CASE_var( cCoolingCoilType );
					if ( SELECT_CASE_var == "COIL:COOLING:WATER" ) {
						UnitVent( UnitVentNum ).CCoilType = Cooling_CoilWaterCooling;
						UnitVent( UnitVentNum ).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
						UnitVent( UnitVentNum ).CCoilPlantName = Alphas( 18 );
					} else if ( SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) {
						UnitVent( UnitVentNum ).CCoilType = Cooling_CoilDetailedCooling;
						UnitVent( UnitVentNum ).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
						UnitVent( UnitVentNum ).CCoilPlantName = Alphas( 18 );
					} else if ( SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED" ) {
						UnitVent( UnitVentNum ).CCoilType = Cooling_CoilHXAssisted;
						GetHXCoilTypeAndName( cCoolingCoilType, Alphas( 18 ), ErrorsFound, UnitVent( UnitVentNum ).CCoilPlantType, UnitVent( UnitVentNum ).CCoilPlantName );
						if ( SameString( UnitVent( UnitVentNum ).CCoilPlantType, "Coil:Cooling:Water" ) ) {
							UnitVent( UnitVentNum ).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
						} else if ( SameString( UnitVent( UnitVentNum ).CCoilPlantType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
							UnitVent( UnitVentNum ).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
							ShowContinueError( "For: " + cAlphaFields( 17 ) + "=\"" + Alphas( 17 ) + "\"." );
							ShowContinueError( "Invalid Coil Type=" + UnitVent( UnitVentNum ).CCoilPlantType + ", Name=" + UnitVent( UnitVentNum ).CCoilPlantName );
							ShowContinueError( "must be \"Coil:Cooling:Water\" or \"Coil:Cooling:Water:DetailedGeometry\"" );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", invalid" );
						ShowContinueError( "illegal value: " + cAlphaFields( 17 ) + "=\"" + cCoolingCoilType + "\"." );
						ErrorsFound = true;
						errFlag = true;
					}}

					if ( ! errFlag ) {
						UnitVent( UnitVentNum ).CCoilName = Alphas( 18 );
						ValidateComponent( cCoolingCoilType, UnitVent( UnitVentNum ).CCoilName, IsNotOK, CurrentModuleObject );
						if ( IsNotOK ) {
							ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"." );
							ErrorsFound = true;
						} else {
							if ( UnitVent( UnitVentNum ).CCoilType != Cooling_CoilHXAssisted ) {
								// mine the cold water node from the coil object
								UnitVent( UnitVentNum ).ColdControlNode = GetCoilWaterInletNode( UnitVent( UnitVentNum ).CCoilTypeCh, UnitVent( UnitVentNum ).CCoilName, errFlag );
							} else {
								UnitVent( UnitVentNum ).ColdControlNode = GetHXCoilWaterInletNode( UnitVent( UnitVentNum ).CCoilTypeCh, UnitVent( UnitVentNum ).CCoilName, errFlag );
							}
							// Other error checks should trap before it gets to this point in the code, but including just in case.
							if ( errFlag ) {
								ShowContinueError( "...specified in " + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\"." );
								ErrorsFound = true;
							}
						}
					}

					UnitVent( UnitVentNum ).MinVolColdWaterFlow = 0.0;
					UnitVent( UnitVentNum ).ColdControlOffset = Numbers( 5 );
					// Set default convergence tolerance
					if ( UnitVent( UnitVentNum ).ColdControlOffset <= 0.0 ) {
						UnitVent( UnitVentNum ).ColdControlOffset = 0.001;
					}
					{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).CCoilType );

					if ( SELECT_CASE_var == Cooling_CoilWaterCooling ) {
						UnitVent( UnitVentNum ).MaxVolColdWaterFlow = GetWaterCoilMaxFlowRate( "Coil:Cooling:Water", UnitVent( UnitVentNum ).CCoilName, ErrorsFound );
					} else if ( SELECT_CASE_var == Cooling_CoilDetailedCooling ) {
						UnitVent( UnitVentNum ).MaxVolColdWaterFlow = GetWaterCoilMaxFlowRate( "Coil:Cooling:Water:DetailedGeometry", UnitVent( UnitVentNum ).CCoilName, ErrorsFound );
					} else if ( SELECT_CASE_var == Cooling_CoilHXAssisted ) {
						UnitVent( UnitVentNum ).MaxVolColdWaterFlow = GetHXAssistedCoilFlowRate( "CoilSystem:Cooling:Water:HeatExchangerAssisted", UnitVent( UnitVentNum ).CCoilName, ErrorsFound );
					} else {

					}}
				} else { // Cooling Coil is required for this/these options
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + UnitVent( UnitVentNum ).Name + "\", missing cooling coil" );
					ShowContinueError( "a cooling coil is required for " + cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\"." );
					ErrorsFound = true;
				} //IF (.NOT. lAlphaBlanks(17)) THEN - from the start of cooling coil information
			}

			// check that unit ventilator air inlet node is the same as a zone exhaust node
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
					if ( UnitVent( UnitVentNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
						ZoneNodeNotFound = false;
						break;
					}
				}
			}
			if ( ZoneNodeNotFound ) {
				ShowSevereError( CurrentModuleObject + " = \"" + UnitVent( UnitVentNum ).Name + "\". Unit ventilator air inlet node name must be the same as a zone exhaust node name." );
				ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "..Unit ventilator air inlet node name = " + NodeID( UnitVent( UnitVentNum ).AirInNode ) );
				ErrorsFound = true;
			}
			// check that unit ventilator air outlet node is the same as a zone inlet node.
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
					if ( UnitVent( UnitVentNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
						UnitVent( UnitVentNum ).ZonePtr = CtrlZone;
						ZoneNodeNotFound = false;
						break;
					}
				}
			}
			if ( ZoneNodeNotFound ) {
				ShowSevereError( CurrentModuleObject + " = \"" + UnitVent( UnitVentNum ).Name + "\". Unit ventilator air outlet node name must be the same as a zone inlet node name." );
				ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "..Unit ventilator air outlet node name = " + NodeID( UnitVent( UnitVentNum ).AirOutNode ) );
				ErrorsFound = true;
			}

			{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).CoilOption );
			if ( SELECT_CASE_var == BothOption ) { // 'HeatingAndCooling'
				// Add cooling coil to component sets array when present
				SetUpCompSets( CurrentModuleObject, UnitVent( UnitVentNum ).Name, cCoolingCoilType, UnitVent( UnitVentNum ).CCoilName, NodeID( UnitVent( UnitVentNum ).FanOutletNode ), "UNDEFINED" );

				// Add heating coil to component sets array when cooling coil present
				SetUpCompSets( CurrentModuleObject, UnitVent( UnitVentNum ).Name, cHeatingCoilType, UnitVent( UnitVentNum ).HCoilName, "UNDEFINED", NodeID( UnitVent( UnitVentNum ).AirOutNode ) );

			} else if ( SELECT_CASE_var == HeatingOption ) { // 'Heating'
				// Add heating coil to component sets array when no cooling coil present
				SetUpCompSets( CurrentModuleObject, UnitVent( UnitVentNum ).Name, cHeatingCoilType, UnitVent( UnitVentNum ).HCoilName, NodeID( UnitVent( UnitVentNum ).FanOutletNode ), NodeID( UnitVent( UnitVentNum ).AirOutNode ) );

			} else if ( SELECT_CASE_var == CoolingOption ) { // 'Cooling'
				// Add cooling coil to component sets array when no heating coil present
				SetUpCompSets( CurrentModuleObject, UnitVent( UnitVentNum ).Name, cCoolingCoilType, UnitVent( UnitVentNum ).CCoilName, NodeID( UnitVent( UnitVentNum ).FanOutletNode ), NodeID( UnitVent( UnitVentNum ).AirOutNode ) );

			} else if ( SELECT_CASE_var == NoneOption ) {

			} else {

			}}

		} // ...loop over all of the unit ventilators found in the input file

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) ShowFatalError( RoutineName + "Errors found in input." );

		// Setup Report variables for the Unit Ventilators, CurrentModuleObject='ZoneHVAC:UnitVentilator'
		for ( UnitVentNum = 1; UnitVentNum <= NumOfUnitVents; ++UnitVentNum ) {
			SetupOutputVariable( "Zone Unit Ventilator Heating Rate [W]", UnitVent( UnitVentNum ).HeatPower, "System", "Average", UnitVent( UnitVentNum ).Name );
			SetupOutputVariable( "Zone Unit Ventilator Heating Energy [J]", UnitVent( UnitVentNum ).HeatEnergy, "System", "Sum", UnitVent( UnitVentNum ).Name );
			SetupOutputVariable( "Zone Unit Ventilator Total Cooling Rate [W]", UnitVent( UnitVentNum ).TotCoolPower, "System", "Average", UnitVent( UnitVentNum ).Name );
			SetupOutputVariable( "Zone Unit Ventilator Total Cooling Energy [J]", UnitVent( UnitVentNum ).TotCoolEnergy, "System", "Sum", UnitVent( UnitVentNum ).Name );
			SetupOutputVariable( "Zone Unit Ventilator Sensible Cooling Rate [W]", UnitVent( UnitVentNum ).SensCoolPower, "System", "Average", UnitVent( UnitVentNum ).Name );
			SetupOutputVariable( "Zone Unit Ventilator Sensible Cooling Energy [J]", UnitVent( UnitVentNum ).SensCoolEnergy, "System", "Sum", UnitVent( UnitVentNum ).Name );
			SetupOutputVariable( "Zone Unit Ventilator Fan Electric Power [W]", UnitVent( UnitVentNum ).ElecPower, "System", "Average", UnitVent( UnitVentNum ).Name );
			// Note that the unit vent fan electric is NOT metered because this value is already metered through the fan component
			SetupOutputVariable( "Zone Unit Ventilator Fan Electric Energy [J]", UnitVent( UnitVentNum ).ElecEnergy, "System", "Sum", UnitVent( UnitVentNum ).Name );
			SetupOutputVariable( "Zone Unit Ventilator Fan Availability Status []", UnitVent( UnitVentNum ).AvailStatus, "System", "Average", UnitVent( UnitVentNum ).Name );
			if ( UnitVent( UnitVentNum ).FanType_Num == FanType_SimpleOnOff ) {
				SetupOutputVariable( "Zone Unit Ventilator Fan Part Load Ratio []", UnitVent( UnitVentNum ).FanPartLoadRatio, "System", "Average", UnitVent( UnitVentNum ).Name );
			}
		}

	}

	void
	InitUnitVentilator(
		int const UnitVentNum, // index for the current unit ventilator
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const ZoneNum // number of zone being served
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes all of the data elements which are necessary
		// to simulate a unit ventilator.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::StdRhoAir;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::UnitVentilator_Num;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using DataGlobals::AnyPlantInModel;
		using namespace DataZoneEnergyDemands;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitUnitVentilator" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirRelNode; // relief air node number in unit ventilator loop
		int ColdConNode; // cold water control node number in unit ventilator loop
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		int HotConNode; // hot water control node number in unit ventilator loop
		int InNode; // inlet node number in unit ventilator loop
		int OutNode; // outlet node number in unit ventilator loop
		int OutsideAirNode; // outside air node number in unit ventilator loop
		Real64 RhoAir; // air density at InNode
		Real64 TempSteamIn;
		Real64 SteamDensity;
		Real64 rho; // local fluid density
		bool errFlag;
		bool SetMassFlowRateToZero; // TRUE when mass flow rates need to be set to zero

		SetMassFlowRateToZero = false;
		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumOfUnitVents );
			MySizeFlag.allocate( NumOfUnitVents );
			MyPlantScanFlag.allocate( NumOfUnitVents );
			MyZoneEqFlag.allocate ( NumOfUnitVents );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;
			MyZoneEqFlag = true;
			MyOneTimeFlag = false;

		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( UnitVentNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( UnitVentilator_Num ).ZoneCompAvailMgrs( UnitVentNum ).AvailManagerListName = UnitVent( UnitVentNum ).AvailManagerListName;
				ZoneComp( UnitVentilator_Num ).ZoneCompAvailMgrs( UnitVentNum ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( UnitVentNum ) = false;
			}
			UnitVent( UnitVentNum ).AvailStatus = ZoneComp( UnitVentilator_Num ).ZoneCompAvailMgrs( UnitVentNum ).AvailStatus;
		}

		if ( MyPlantScanFlag( UnitVentNum ) && allocated( PlantLoop ) ) {
			if ( ( UnitVent( UnitVentNum ).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating ) || ( UnitVent( UnitVentNum ).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoil_PlantTypeNum, UnitVent( UnitVentNum ).HWLoopNum, UnitVent( UnitVentNum ).HWLoopSide, UnitVent( UnitVentNum ).HWBranchNum, UnitVent( UnitVentNum ).HWCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + UnitVent( UnitVentNum ).Name + "\", type=ZoneHVAC:UnitVentilator" );
					ShowFatalError( "InitUnitVentilator: Program terminated due to previous condition(s)." );
				}

				UnitVent( UnitVentNum ).HotCoilOutNodeNum = PlantLoop( UnitVent( UnitVentNum ).HWLoopNum ).LoopSide( UnitVent( UnitVentNum ).HWLoopSide ).Branch( UnitVent( UnitVentNum ).HWBranchNum ).Comp( UnitVent( UnitVentNum ).HWCompNum ).NodeNumOut;
			}
			if ( ( UnitVent( UnitVentNum ).CCoil_PlantTypeNum == TypeOf_CoilWaterCooling ) || ( UnitVent( UnitVentNum ).CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( UnitVent( UnitVentNum ).CCoilPlantName, UnitVent( UnitVentNum ).CCoil_PlantTypeNum, UnitVent( UnitVentNum ).CWLoopNum, UnitVent( UnitVentNum ).CWLoopSide, UnitVent( UnitVentNum ).CWBranchNum, UnitVent( UnitVentNum ).CWCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + UnitVent( UnitVentNum ).Name + "\", type=ZoneHVAC:UnitVentilator" );
					ShowFatalError( "InitUnitVentilator: Program terminated due to previous condition(s)." );
				}

				UnitVent( UnitVentNum ).ColdCoilOutNodeNum = PlantLoop( UnitVent( UnitVentNum ).CWLoopNum ).LoopSide( UnitVent( UnitVentNum ).CWLoopSide ).Branch( UnitVent( UnitVentNum ).CWBranchNum ).Comp( UnitVent( UnitVentNum ).CWCompNum ).NodeNumOut;
			} else {
				if ( UnitVent( UnitVentNum ).CCoilPresent ) ShowFatalError( "InitUnitVentilator: Unit=" + UnitVent( UnitVentNum ).Name + ", invalid cooling coil type. Program terminated." );
			}
			MyPlantScanFlag( UnitVentNum ) = false;
		} else if ( MyPlantScanFlag( UnitVentNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( UnitVentNum ) = false;
		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumOfUnitVents; ++Loop ) {
				if ( CheckZoneEquipmentList( "ZoneHVAC:UnitVentilator", UnitVent( Loop ).Name ) ) continue;
				ShowSevereError( "InitUnitVentilator: Unit=[UNIT VENTILATOR," + UnitVent( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( UnitVentNum ) && ! MyPlantScanFlag( UnitVentNum ) ) {

			SizeUnitVentilator( UnitVentNum );

			MySizeFlag( UnitVentNum ) = false;
		}

		// Do the one time initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( UnitVentNum ) && ! MyPlantScanFlag( UnitVentNum ) ) {
			InNode = UnitVent( UnitVentNum ).AirInNode;
			OutNode = UnitVent( UnitVentNum ).AirOutNode;
			HotConNode = UnitVent( UnitVentNum ).HotControlNode;
			ColdConNode = UnitVent( UnitVentNum ).ColdControlNode;
			OutsideAirNode = UnitVent( UnitVentNum ).OutsideAirNode;
			RhoAir = StdRhoAir;

			// set the mass flow rates from the input volume flow rates
			UnitVent( UnitVentNum ).MaxAirMassFlow = RhoAir * UnitVent( UnitVentNum ).MaxAirVolFlow;
			UnitVent( UnitVentNum ).OutAirMassFlow = RhoAir * UnitVent( UnitVentNum ).OutAirVolFlow;
			UnitVent( UnitVentNum ).MinOutAirMassFlow = RhoAir * UnitVent( UnitVentNum ).MinOutAirVolFlow;
			if ( UnitVent( UnitVentNum ).OutAirMassFlow > UnitVent( UnitVentNum ).MaxAirMassFlow ) {
				UnitVent( UnitVentNum ).OutAirMassFlow = UnitVent( UnitVentNum ).MaxAirMassFlow;
				UnitVent( UnitVentNum ).MinOutAirMassFlow = UnitVent( UnitVentNum ).OutAirMassFlow * ( UnitVent( UnitVentNum ).MinOutAirVolFlow / UnitVent( UnitVentNum ).OutAirVolFlow );
				ShowWarningError( "Outdoor air mass flow rate higher than unit flow rate, reset to unit flow rate for " + UnitVent( UnitVentNum ).Name );
			}

			// set the node max and min mass flow rates
			Node( OutsideAirNode ).MassFlowRateMax = UnitVent( UnitVentNum ).OutAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMin = 0.0;

			Node( OutNode ).MassFlowRateMax = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMin = 0.0;

			Node( InNode ).MassFlowRateMax = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMin = 0.0;

			if ( UnitVent( UnitVentNum ).HCoilPresent ) { // Only initialize these if a heating coil is actually present

				if ( UnitVent( UnitVentNum ).HCoilType == Heating_WaterCoilType ) {

					rho = GetDensityGlycol( PlantLoop( UnitVent( UnitVentNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( UnitVent( UnitVentNum ).HWLoopNum ).FluidIndex, RoutineName );

					UnitVent( UnitVentNum ).MaxHotWaterFlow = rho * UnitVent( UnitVentNum ).MaxVolHotWaterFlow;
					UnitVent( UnitVentNum ).MinHotWaterFlow = rho * UnitVent( UnitVentNum ).MinVolHotWaterFlow;

					InitComponentNodes( UnitVent( UnitVentNum ).MinHotWaterFlow, UnitVent( UnitVentNum ).MaxHotWaterFlow, UnitVent( UnitVentNum ).HotControlNode, UnitVent( UnitVentNum ).HotCoilOutNodeNum, UnitVent( UnitVentNum ).HWLoopNum, UnitVent( UnitVentNum ).HWLoopSide, UnitVent( UnitVentNum ).HWBranchNum, UnitVent( UnitVentNum ).HWCompNum );

				}
				if ( UnitVent( UnitVentNum ).HCoilType == Heating_SteamCoilType ) {
					TempSteamIn = 100.00;
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, UnitVent( UnitVentNum ).HCoil_FluidIndex, RoutineName );
					UnitVent( UnitVentNum ).MaxHotSteamFlow = SteamDensity * UnitVent( UnitVentNum ).MaxVolHotSteamFlow;
					UnitVent( UnitVentNum ).MinHotSteamFlow = SteamDensity * UnitVent( UnitVentNum ).MinVolHotSteamFlow;

					InitComponentNodes( UnitVent( UnitVentNum ).MinHotSteamFlow, UnitVent( UnitVentNum ).MaxHotSteamFlow, UnitVent( UnitVentNum ).HotControlNode, UnitVent( UnitVentNum ).HotCoilOutNodeNum, UnitVent( UnitVentNum ).HWLoopNum, UnitVent( UnitVentNum ).HWLoopSide, UnitVent( UnitVentNum ).HWBranchNum, UnitVent( UnitVentNum ).HWCompNum );
				}
			} //(UnitVent(UnitVentNum)%HCoilPresent)

			if ( UnitVent( UnitVentNum ).CCoilPresent ) { // Only initialize these if a cooling coil is actually present
				rho = GetDensityGlycol( PlantLoop( UnitVent( UnitVentNum ).CWLoopNum ).FluidName, 5.0, PlantLoop( UnitVent( UnitVentNum ).CWLoopNum ).FluidIndex, RoutineName );

				UnitVent( UnitVentNum ).MaxColdWaterFlow = rho * UnitVent( UnitVentNum ).MaxVolColdWaterFlow;
				UnitVent( UnitVentNum ).MinColdWaterFlow = rho * UnitVent( UnitVentNum ).MinVolColdWaterFlow;
				InitComponentNodes( UnitVent( UnitVentNum ).MinColdWaterFlow, UnitVent( UnitVentNum ).MaxColdWaterFlow, UnitVent( UnitVentNum ).ColdControlNode, UnitVent( UnitVentNum ).ColdCoilOutNodeNum, UnitVent( UnitVentNum ).CWLoopNum, UnitVent( UnitVentNum ).CWLoopSide, UnitVent( UnitVentNum ).CWBranchNum, UnitVent( UnitVentNum ).CWCompNum );

			}
			MyEnvrnFlag( UnitVentNum ) = false;
		} // ...end start of environment inits

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( UnitVentNum ) = true;

		// These initializations are done every iteration...
		InNode = UnitVent( UnitVentNum ).AirInNode;
		OutNode = UnitVent( UnitVentNum ).AirOutNode;
		OutsideAirNode = UnitVent( UnitVentNum ).OutsideAirNode;
		AirRelNode = UnitVent( UnitVentNum ).AirReliefNode;

		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired; // zone load needed
		UnitVent( UnitVentNum ).FanPartLoadRatio = 0.0;

		if ( UnitVent( UnitVentNum ).FanSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( UnitVent( UnitVentNum ).FanSchedPtr ) == 0.0 && UnitVent( UnitVentNum ).FanType_Num == FanType_SimpleOnOff ) {
				UnitVent( UnitVentNum ).OpMode = CycFanCycCoil;
			} else {
				UnitVent( UnitVentNum ).OpMode = ContFanCycCoil;
			}
		}

		if ( GetCurrentScheduleValue( UnitVent( UnitVentNum ).SchedPtr ) > 0 ) {
			if ( ( GetCurrentScheduleValue( UnitVent( UnitVentNum ).FanAvailSchedPtr ) > 0 || ZoneCompTurnFansOn ) && ! ZoneCompTurnFansOff ) {
				if ( ( std::abs( ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired ) < SmallLoad ) || ( CurDeadBandOrSetback( ZoneNum ) ) ) {
					SetMassFlowRateToZero = true;
				}
			} else {
				SetMassFlowRateToZero = true;
			}
		} else {
			SetMassFlowRateToZero = true;
		}

		if ( SetMassFlowRateToZero ) {
			Node( InNode ).MassFlowRate = 0.0;
			Node( InNode ).MassFlowRateMaxAvail = 0.0;
			Node( InNode ).MassFlowRateMinAvail = 0.0;
			Node( OutNode ).MassFlowRate = 0.0;
			Node( OutNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutNode ).MassFlowRateMinAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRate = 0.0;
			Node( OutsideAirNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
			Node( AirRelNode ).MassFlowRate = 0.0;
			Node( AirRelNode ).MassFlowRateMaxAvail = 0.0;
			Node( AirRelNode ).MassFlowRateMinAvail = 0.0;
		} else {
			Node( InNode ).MassFlowRate = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMaxAvail = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMinAvail = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRate = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMaxAvail = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMinAvail = UnitVent( UnitVentNum ).MaxAirMassFlow;
			Node( OutsideAirNode ).MassFlowRate = UnitVent( UnitVentNum ).OutAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMaxAvail = UnitVent( UnitVentNum ).OutAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMinAvail = UnitVent( UnitVentNum ).OutAirMassFlow;
			Node( AirRelNode ).MassFlowRate = UnitVent( UnitVentNum ).OutAirMassFlow;
			Node( AirRelNode ).MassFlowRateMaxAvail = UnitVent( UnitVentNum ).OutAirMassFlow;
			Node( AirRelNode ).MassFlowRateMinAvail = UnitVent( UnitVentNum ).OutAirMassFlow;
		}

		// Initialize the relief air (same as inlet conditions to the unit ventilator...
		// Note that mass flow rates will be taken care of later.
		Node( AirRelNode ) = Node( InNode );
		OAMassFlowRate = 0.0;

		// Just in case the unit is off and conditions do not get sent through
		// the unit for some reason, set the outlet conditions equal to the inlet
		// conditions of the unit ventilator
		Node( OutNode ).Temp = Node( InNode ).Temp;
		Node( OutNode ).Press = Node( InNode ).Press;
		Node( OutNode ).HumRat = Node( InNode ).HumRat;
		Node( OutNode ).Enthalpy = Node( InNode ).Enthalpy;

		// These initializations only need to be done once at the start of the iterations...
		if ( FirstHVACIteration ) {
			// Initialize the outside air conditions...
			Node( OutsideAirNode ).Temp = Node( OutsideAirNode ).OutAirDryBulb;
			//    Node(OutsideAirNode)%HumRat   = OutHumRat
			//    Node(OutsideAirNode)%Press    = OutBaroPress
			//    Node(OutsideAirNode)%Enthalpy = PsyHFnTdbW(OutDryBulbTemp,OutHumRat)
		}

	}

	void
	SizeUnitVentilator( int const UnitVentNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Unit Ventilator components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		using SteamCoils::GetCoilSteamOutletNode;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using HVACHXAssistedCoolingCoil::GetHXCoilType;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using DataHVACGlobals::SystemAirflowSizing;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::HeatingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeUnitVentilator" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
		bool ErrorsFound;
		Real64 DesCoolingLoad;
		Real64 DesHeatingLoad;
		Real64 TempSteamIn;
		Real64 EnthSteamInDry;
		Real64 EnthSteamOutWet;
		Real64 LatentHeatSteam;
		Real64 SteamDensity;
		static int RefrigIndex( 0 );
		static int CoilWaterInletNode( 0 );
		static int CoilWaterOutletNode( 0 );
		static int CoilSteamInletNode( 0 );
		static int CoilSteamOutletNode( 0 );
		std::string CoolingCoilName;
		std::string CoolingCoilType;
		Real64 rho;
		Real64 Cp;
		static int DummyWaterIndex( 1 );
		bool IsAutoSize; // Index to autosize
		Real64 MaxAirVolFlowDes; // Autosized maximum air flow for reporting
		Real64 MaxAirVolFlowUser; // Hardsized maximum air flow for reporting
		Real64 OutAirVolFlowDes; // Autosized outdoor air flow for reporting
		Real64 OutAirVolFlowUser; // Hardsized outdoor air flow for reporting
		Real64 MinOutAirVolFlowDes; // Autosized minimum outdoor air flow for reporting
		Real64 MinOutAirVolFlowUser; // Hardsized minimum outdoor air flow for reporting
		Real64 MaxVolHotWaterFlowDes; // Autosized maximum water flow for reporting
		Real64 MaxVolHotWaterFlowUser; // Hardsized maximum water flow for reporting
		Real64 MaxVolHotSteamFlowDes; // Autosized maximum steam flow for reporting
		Real64 MaxVolHotSteamFlowUser; // Hardsized maximum steam flow for reporting
		Real64 MaxVolColdWaterFlowDes; // Autosized maximum chilled water flow for reporting
		Real64 MaxVolColdWaterFlowUser; // Hardsized maximum chilled water flow for reporting

		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )
		Real64 CoolingAirVolFlowScalable; // cooling airvolume for rate determined using scalable sizing method
		Real64 HeatingAirVolFlowScalable; // heating airvolume for rate determined using scalable sizing method

		PltSizHeatNum = 0;
		ErrorsFound = false;
		IsAutoSize = false;
		MaxAirVolFlowDes = 0.0;
		MaxAirVolFlowUser = 0.0;
		OutAirVolFlowDes = 0.0;
		OutAirVolFlowUser = 0.0;
		MinOutAirVolFlowDes = 0.0;
		MinOutAirVolFlowUser = 0.0;
		MaxVolHotWaterFlowDes = 0.0;
		MaxVolHotWaterFlowUser = 0.0;
		MaxVolHotSteamFlowDes = 0.0;
		MaxVolHotSteamFlowUser = 0.0;
		MaxVolColdWaterFlowDes = 0.0;
		MaxVolColdWaterFlowUser = 0.0;
		CoolingAirVolFlowScalable = 0.0;
		HeatingAirVolFlowScalable = 0.0;
		DataScalableSizingON = false;
		DataScalableCapSizingON = false;
		CompType = cMO_UnitVentilator;
		CompName = UnitVent(UnitVentNum).Name;
		DataZoneNumber = UnitVent(UnitVentNum).ZonePtr;
		ZoneCoolingOnlyFan = false;
		ZoneHeatingOnlyFan = false;

		if ( UnitVent( UnitVentNum ).CoilOption == BothOption ) {
			ZoneCoolingOnlyFan = true;
			ZoneHeatingOnlyFan = true;
		} else if ( UnitVent( UnitVentNum ).CoilOption == HeatingOption ) {
			ZoneHeatingOnlyFan = true;
		} else if ( UnitVent( UnitVentNum ).CoilOption == CoolingOption ) {
			ZoneCoolingOnlyFan = true;
		} else if ( UnitVent( UnitVentNum ).CoilOption == NoneOption ) {
		}

		if ( CurZoneEqNum > 0 ) {

			if ( UnitVent( UnitVentNum ).HVACSizingIndex > 0 ) {
				zoneHVACIndex = UnitVent( UnitVentNum ).HVACSizingIndex;
				// N1 , \field Maximum Supply Air Flow Rate
				FieldNum = 1;
				PrintFlag = true;
				SizingString = UnitVentNumericFields( UnitVentNum ).FieldNames( FieldNum ) + " [m3/s]";

				if ( ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod > 0 && ZoneCoolingOnlyFan && !ZoneHeatingOnlyFan ) {

					SizingMethod = CoolingAirflowSizing;
					SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
					ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
					if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
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
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						CoolingAirVolFlowScalable = TempSize;

					} else if ( SAFMethod == FlowPerCoolingCapacity ) {
						SizingMethod = CoolingCapacitySizing;
						TempSize = AutoSize;
						PrintFlag = false;
						DataScalableSizingON = true;
						DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DataAutosizedCoolingCapacity = TempSize;
						DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						SizingMethod = CoolingAirflowSizing;
						PrintFlag = true;
						TempSize = AutoSize;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						CoolingAirVolFlowScalable = TempSize;
					}
					//DataScalableSizingON = false;

				} else if ( ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod > 0 && ZoneHeatingOnlyFan && !ZoneCoolingOnlyFan ) {
					SizingMethod = HeatingAirflowSizing;
					SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
					ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
					if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
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
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						HeatingAirVolFlowScalable = TempSize;

					} else if ( SAFMethod == FlowPerHeatingCapacity ) {
						SizingMethod = HeatingCapacitySizing;
						TempSize = AutoSize;
						PrintFlag = false;
						DataScalableSizingON = true;
						DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DataAutosizedHeatingCapacity = TempSize;
						DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
						SizingMethod = HeatingAirflowSizing;
						PrintFlag = true;
						TempSize = AutoSize;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						HeatingAirVolFlowScalable = TempSize;
					}
					//DataScalableSizingON = false;
				} else {

					if ( UnitVent( UnitVentNum ).CoilOption != NoneOption ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod > 0 ) {
							SizingMethod = CoolingAirflowSizing;
							SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
							ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
							if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
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
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								CoolingAirVolFlowScalable = TempSize;

							} else if ( SAFMethod == FlowPerCoolingCapacity ) {
								SizingMethod = CoolingCapacitySizing;
								TempSize = AutoSize;
								PrintFlag = false;
								DataScalableSizingON = true;
								DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								DataAutosizedCoolingCapacity = TempSize;
								DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
								SizingMethod = CoolingAirflowSizing;
								PrintFlag = true;
								TempSize = AutoSize;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								CoolingAirVolFlowScalable = TempSize;
							}
						} else if ( ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod > 0 )	{
							SizingMethod = HeatingAirflowSizing;
							SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
							ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
							if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
								SizingMethod = SystemAirflowSizing;
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
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								HeatingAirVolFlowScalable = TempSize;

							} else if ( SAFMethod == FlowPerHeatingCapacity ) {
								SizingMethod = HeatingCapacitySizing;
								TempSize = AutoSize;
								PrintFlag = false;
								DataScalableSizingON = true;
								DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								DataAutosizedHeatingCapacity = TempSize;
								DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
								SizingMethod = HeatingAirflowSizing;
								PrintFlag = true;
								TempSize = AutoSize;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								HeatingAirVolFlowScalable = TempSize;
							}
						}
						//DataScalableSizingON = false;
					} else {        // if ( UnitVent (UnitVentNum ).CoilOption /= NoneOption )

						PrintFlag = true;
						FieldNum = 1;
						SizingString = UnitVentNumericFields( UnitVentNum ).FieldNames( FieldNum ) + " [m3/s]";
						SizingMethod = SystemAirflowSizing;
						if ( UnitVent( UnitVentNum ).MaxAirVolFlow == AutoSize ) {
							TempSize = FinalZoneSizing( CurZoneEqNum ).MinOA;
						} else {
							TempSize = UnitVent( UnitVentNum ).MaxAirVolFlow;
						}
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						HeatingAirVolFlowScalable = TempSize;
					}
				}

				UnitVent( UnitVentNum ).MaxAirVolFlow = max( CoolingAirVolFlowScalable, HeatingAirVolFlowScalable );

			} else {
				// no scalble sizing method has been specified. Sizing proceeds using the method
				// specified in the zoneHVAC object
				// N1 , \field Maximum Supply Air Flow Rate
				PrintFlag = true;
				FieldNum = 1;
				SizingString = UnitVentNumericFields( UnitVentNum ).FieldNames( FieldNum ) + " [m3/s]";
				SizingMethod = SystemAirflowSizing;
				if ( UnitVent( UnitVentNum ).CoilOption == NoneOption ) {

					if ( UnitVent( UnitVentNum ).MaxAirVolFlow == AutoSize ) {
						TempSize = FinalZoneSizing( CurZoneEqNum ).MinOA;
					} else {
						TempSize = UnitVent( UnitVentNum ).MaxAirVolFlow;
					}

				} else {
					TempSize = UnitVent( UnitVentNum ).MaxAirVolFlow;
				}
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				UnitVent( UnitVentNum ).MaxAirVolFlow = TempSize;
			}
		}

		IsAutoSize = false;
		if ( UnitVent( UnitVentNum ).OutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( UnitVent( UnitVentNum ).OutAirVolFlow > 0.0 ) {
					ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "User-Specified Maximum Outdoor Air Flow Rate [m3/s]", UnitVent( UnitVentNum ).OutAirVolFlow );
				}
			} else {
				CheckZoneSizing( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name );
				OutAirVolFlowDes = UnitVent( UnitVentNum ).MaxAirVolFlow;
				if ( IsAutoSize ) {
					UnitVent( UnitVentNum ).OutAirVolFlow = OutAirVolFlowDes;
					ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes );
				} else {
					if ( UnitVent( UnitVentNum ).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0 ) {
						OutAirVolFlowUser = UnitVent( UnitVentNum ).OutAirVolFlow;
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes, "User-Specified Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( OutAirVolFlowDes - OutAirVolFlowUser ) / OutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeUnitVentilator: Potential issue with equipment sizing for " + cMO_UnitVentilator + ' ' + UnitVent( UnitVentNum ).Name );
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

		IsAutoSize = false;
		if ( UnitVent( UnitVentNum ).MinOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
				if ( UnitVent( UnitVentNum ).MinOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "User-Specified Minimum Outdoor Air Flow Rate [m3/s]", UnitVent( UnitVentNum ).MinOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name );
				MinOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, UnitVent( UnitVentNum ).MaxAirVolFlow );
				if ( MinOutAirVolFlowDes < SmallAirVolFlow ) {
					MinOutAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					UnitVent( UnitVentNum ).MinOutAirVolFlow = MinOutAirVolFlowDes;
					ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Minimum Outdoor Air Flow Rate [m3/s]", MinOutAirVolFlowDes );
				} else {
					if ( UnitVent( UnitVentNum ).MinOutAirVolFlow > 0.0 && MinOutAirVolFlowDes > 0.0 ) {
						MinOutAirVolFlowUser = UnitVent( UnitVentNum ).MinOutAirVolFlow;
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Minimum Outdoor Air Flow Rate [m3/s]", MinOutAirVolFlowDes, "User-Specified Minimum Outdoor Air Flow Rate [m3/s]", MinOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MinOutAirVolFlowDes - MinOutAirVolFlowUser ) / MinOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeUnitVentilator: Potential issue with equipment sizing for " + cMO_UnitVentilator + ' ' + UnitVent( UnitVentNum ).Name );
								ShowContinueError( "User-Specified Minimum Outdoor Air Flow Rate of " + RoundSigDigits( MinOutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Minimum Outdoor Air Flow Rate of " + RoundSigDigits( MinOutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( UnitVent( UnitVentNum ).MaxVolHotWaterFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( UnitVent( UnitVentNum ).HCoilType == Heating_WaterCoilType ) {
			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // Simulation continue
					if ( UnitVent( UnitVentNum ).MaxVolHotWaterFlow > 0.0 ) {
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "User-Specified Maximum Hot Water Flow [m3/s]", UnitVent( UnitVentNum ).MaxVolHotWaterFlow );
					}
				} else {
					CheckZoneSizing( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name );

					CoilWaterInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", UnitVent( UnitVentNum ).HCoilName, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( "Coil:Heating:Water", UnitVent( UnitVentNum ).HCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", UnitVent( UnitVentNum ).HCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {
							if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallAirVolFlow ) {
								SizingMethod = HeatingCapacitySizing;
								if ( UnitVent( UnitVentNum ).HVACSizingIndex > 0 ) {
									zoneHVACIndex = UnitVent( UnitVentNum ).HVACSizingIndex;
									CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
									ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
									if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
										if ( CapSizingMethod == HeatingDesignCapacity ) {
											if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0 ) {
												ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
												ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											} else {
												DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											}
											TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										} else if ( CapSizingMethod == CapacityPerFloorArea ) {
											ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
											DataScalableCapSizingON = true;
										} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
											DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											TempSize = AutoSize;
											DataScalableCapSizingON = true;
										}
									}
									SizingString = "";
									PrintFlag = false;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesHeatingLoad = TempSize;
								} else {
									SizingString = "";
									PrintFlag = false;
									TempSize = AutoSize;
									DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesHeatingLoad = TempSize;
								}
								rho = GetDensityGlycol( PlantLoop( UnitVent( UnitVentNum ).HWLoopNum ).FluidName, 60., PlantLoop( UnitVent( UnitVentNum ).HWLoopNum ).FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( PlantLoop( UnitVent( UnitVentNum ).HWLoopNum ).FluidName, 60., PlantLoop( UnitVent( UnitVentNum ).HWLoopNum ).FluidIndex, RoutineName );
								MaxVolHotWaterFlowDes = DesHeatingLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );

							} else {
								MaxVolHotWaterFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in " + cMO_UnitVentilator + " Object=" + UnitVent( UnitVentNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						UnitVent( UnitVentNum ).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowDes );
					} else {
						if ( UnitVent( UnitVentNum ).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0 ) {
							MaxVolHotWaterFlowUser = UnitVent( UnitVentNum ).MaxVolHotWaterFlow;
							ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowDes, "User-Specified Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser ) / MaxVolHotWaterFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeUnitVentilator: Potential issue with equipment sizing for " + cMO_UnitVentilator + ' ' + UnitVent( UnitVentNum ).Name );
									ShowContinueError( "User-Specified Maximum Hot Water Flow of " + RoundSigDigits( MaxVolHotWaterFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Hot Water Flow of " + RoundSigDigits( MaxVolHotWaterFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			UnitVent( UnitVentNum ).MaxVolHotWaterFlow = 0.0;
		}

		IsAutoSize = false;
		if ( UnitVent( UnitVentNum ).MaxVolHotSteamFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( UnitVent( UnitVentNum ).HCoilType == Heating_SteamCoilType ) {
			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // Simulation continue
					if ( UnitVent( UnitVentNum ).MaxVolHotSteamFlow > 0.0 ) {
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "User-Specified Maximum Steam Flow [m3/s]", UnitVent( UnitVentNum ).MaxVolHotSteamFlow );
					}
				} else {
					CheckZoneSizing( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name );

					CoilSteamInletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", UnitVent( UnitVentNum ).HCoilName, ErrorsFound );
					CoilSteamOutletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", UnitVent( UnitVentNum ).HCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Steam", UnitVent( UnitVentNum ).HCoilName, CoilSteamInletNode, CoilSteamOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {
							if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallAirVolFlow ) {
								SizingMethod = HeatingCapacitySizing;
								if ( UnitVent( UnitVentNum ).HVACSizingIndex > 0 ) {
									zoneHVACIndex = UnitVent( UnitVentNum ).HVACSizingIndex;
									CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
									ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
									if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
										if ( CapSizingMethod == HeatingDesignCapacity ) {
											if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0 ) {
												ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
												ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											} else {
												DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											}
											TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										} else if ( CapSizingMethod == CapacityPerFloorArea ) {
											ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
											DataScalableCapSizingON = true;
										} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
											DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											TempSize = AutoSize;
											DataScalableCapSizingON = true;
										}
									}
									SizingString = "";
									PrintFlag = false;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesHeatingLoad = TempSize;
								} else {
									SizingString = "";
									PrintFlag = false;
									TempSize = AutoSize;
									DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesHeatingLoad = TempSize;
								}
								TempSteamIn = 100.00;
								EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, RefrigIndex, RoutineName );
								EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, RefrigIndex, RoutineName );
								LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
								SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, RefrigIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( fluidNameWater, PlantSizData( PltSizHeatNum ).ExitTemp, DummyWaterIndex, RoutineName );
								MaxVolHotSteamFlowDes = DesHeatingLoad / ( SteamDensity * ( LatentHeatSteam + PlantSizData( PltSizHeatNum ).DeltaT * Cp ) );
							} else {
								MaxVolHotSteamFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of Steam flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in " + cMO_UnitVentilator + " Object=" + UnitVent( UnitVentNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						UnitVent( UnitVentNum ).MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowDes );
					} else {
						if ( UnitVent( UnitVentNum ).MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0 ) {
							MaxVolHotSteamFlowUser = UnitVent( UnitVentNum ).MaxVolHotSteamFlow;
							ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowDes, "User-Specified Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser ) / MaxVolHotSteamFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeUnitVentilator: Potential issue with equipment sizing for " + cMO_UnitVentilator + ' ' + UnitVent( UnitVentNum ).Name );
									ShowContinueError( "User-Specified Maximum Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			UnitVent( UnitVentNum ).MaxVolHotSteamFlow = 0.0;
		}

		IsAutoSize = false;
		if ( UnitVent( UnitVentNum ).MaxVolColdWaterFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( UnitVent( UnitVentNum ).CCoilType == Cooling_CoilWaterCooling || UnitVent( UnitVentNum ).CCoilType == Cooling_CoilDetailedCooling || UnitVent( UnitVentNum ).CCoilType == Cooling_CoilHXAssisted ) {

			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // Simulation continue
					if ( UnitVent( UnitVentNum ).MaxVolColdWaterFlow > 0.0 ) {
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "User-Specified Maximum Cold Water Flow [m3/s]", UnitVent( UnitVentNum ).MaxVolColdWaterFlow );
					}
				} else {
					CheckZoneSizing( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name );

					if ( UnitVent( UnitVentNum ).CCoilType == Cooling_CoilHXAssisted ) {
						CoolingCoilName = GetHXDXCoilName( UnitVent( UnitVentNum ).CCoilTypeCh, UnitVent( UnitVentNum ).CCoilName, ErrorsFound );
						CoolingCoilType = GetHXCoilType( UnitVent( UnitVentNum ).CCoilTypeCh, UnitVent( UnitVentNum ).CCoilName, ErrorsFound );
					} else {
						CoolingCoilName = UnitVent( UnitVentNum ).CCoilName;
						CoolingCoilType = UnitVent( UnitVentNum ).CCoilTypeCh;
					}
					CoilWaterInletNode = GetCoilWaterInletNode( CoolingCoilType, CoolingCoilName, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( CoolingCoilType, CoolingCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizCoolNum = MyPlantSizingIndex( CoolingCoilType, CoolingCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						if ( PltSizCoolNum > 0 ) {
							if ( FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow >= SmallAirVolFlow ) {
								SizingMethod = CoolingCapacitySizing;
								if ( UnitVent( UnitVentNum ).HVACSizingIndex > 0 ) {
									zoneHVACIndex = UnitVent( UnitVentNum ).HVACSizingIndex;
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
											ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
											ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity * Zone( DataZoneNumber ).FloorArea;
											DataScalableCapSizingON = true;
										} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
											DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
											DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
											TempSize = AutoSize;
											DataScalableCapSizingON = true;
										}
									}
									SizingString = "";
									PrintFlag = false;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesCoolingLoad = TempSize;
								} else {
									SizingString = "";
									PrintFlag = false;
									TempSize = AutoSize;
									DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesCoolingLoad = TempSize;
								}
								rho = GetDensityGlycol( PlantLoop( UnitVent( UnitVentNum ).CWLoopNum ).FluidName, 5., PlantLoop( UnitVent( UnitVentNum ).CWLoopNum ).FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( PlantLoop( UnitVent( UnitVentNum ).CWLoopNum ).FluidName, 5., PlantLoop( UnitVent( UnitVentNum ).CWLoopNum ).FluidIndex, RoutineName );
								MaxVolColdWaterFlowDes = DesCoolingLoad / ( PlantSizData( PltSizCoolNum ).DeltaT * Cp * rho );

								if ( MaxVolColdWaterFlowDes < 0.0 ) {
									ShowWarningError( "Autosizing of water flow resulted in negative value." );
									ShowContinueError( "Occurs in " + cMO_UnitVentilator + " Object=" + UnitVent( UnitVentNum ).Name );
									ShowContinueError( "...Sizing information found during sizing simulation:" );
									ShowContinueError( "...Calculated coil design load = " + TrimSigDigits( DesCoolingLoad, 3 ) + " W" );
									ShowContinueError( "...Calculated water flow rate  = " + TrimSigDigits( MaxVolColdWaterFlowDes, 3 ) + " m3/s" );
									ShowContinueError( "...Water flow rate will be set to 0. Check sizing inputs for zone and plant, inputs for water cooling coil object, and design day specifications." );
									ShowContinueError( "...Consider autosizing all inputs if not already doing so." );
									MaxVolColdWaterFlowDes = 0.0;
								}
							} else {
								MaxVolColdWaterFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
							ShowContinueError( "Occurs in " + cMO_UnitVentilator + " Object=" + UnitVent( UnitVentNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						UnitVent( UnitVentNum ).MaxVolColdWaterFlow = MaxVolColdWaterFlowDes;
						ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Cold Water Flow [m3/s]", MaxVolColdWaterFlowDes );
					} else {
						if ( UnitVent( UnitVentNum ).MaxVolColdWaterFlow > 0.0 && MaxVolColdWaterFlowDes > 0.0 ) {
							MaxVolColdWaterFlowUser = UnitVent( UnitVentNum ).MaxVolColdWaterFlow;
							ReportSizingOutput( cMO_UnitVentilator, UnitVent( UnitVentNum ).Name, "Design Size Maximum Cold Water Flow [m3/s]", MaxVolColdWaterFlowDes, "User-Specified Maximum Cold Water Flow [m3/s]", MaxVolColdWaterFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolColdWaterFlowDes - MaxVolColdWaterFlowUser ) / MaxVolColdWaterFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeUnitVentilator: Potential issue with equipment sizing for " + cMO_UnitVentilator + ' ' + UnitVent( UnitVentNum ).Name );
									ShowContinueError( "User-Specified Maximum Cold Water Flow of " + RoundSigDigits( MaxVolColdWaterFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Cold Water Flow of " + RoundSigDigits( MaxVolColdWaterFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		}

		// set the design air flow rates for the heating and cooling coils
		if ( UnitVent( UnitVentNum ).CCoilType == Cooling_CoilHXAssisted ) {
			CoolingCoilName = GetHXDXCoilName( UnitVent( UnitVentNum ).CCoilTypeCh, UnitVent( UnitVentNum ).CCoilName, ErrorsFound );
			CoolingCoilType = GetHXCoilType( UnitVent( UnitVentNum ).CCoilTypeCh, UnitVent( UnitVentNum ).CCoilName, ErrorsFound );
		} else {
			CoolingCoilName = UnitVent( UnitVentNum ).CCoilName;
			CoolingCoilType = UnitVent( UnitVentNum ).CCoilTypeCh;
		}
		SetCoilDesFlow( CoolingCoilType, CoolingCoilName, UnitVent( UnitVentNum ).MaxAirVolFlow, ErrorsFound );
		SetCoilDesFlow( UnitVent( UnitVentNum ).HCoilTypeCh, UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).MaxAirVolFlow, ErrorsFound );

		if ( CurZoneEqNum > 0 ) {
			ZoneEqSizing( CurZoneEqNum ).MaxHWVolFlow = UnitVent( UnitVentNum ).MaxVolHotWaterFlow;
			ZoneEqSizing( CurZoneEqNum ).MaxCWVolFlow = UnitVent( UnitVentNum ).MaxVolColdWaterFlow;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcUnitVentilator(
		int & UnitVentNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine mainly controls the action of the unit ventilator
		// (or more exactly, it controls the amount of outside air brought in)
		// based on the user input for controls and the defined controls
		// algorithms.  There are currently (at the initial creation of this
		// subroutine) two control methods: variable percent (ASHRAE "Cycle I"
		// or "Cycle II") and fixed temperature (ASHRAE "Cycle III").

		// METHODOLOGY EMPLOYED:
		// Unit is controlled based on user input and what is happening in the
		// simulation.  There are various cases to consider:
		// 1. OFF: Unit is schedule off or there is no load on it.  All flow
		//    rates are set to zero and the temperatures are set to zone conditions
		//    (except for the outside air inlet).
		// 2. HEATING/VARIABLE PERCENT: The unit is on, there is a heating load,
		//    and variable percent control is specified.  The outside air fraction
		//    is set to the minimum outside air fraction (schedule based) and the
		//    heating coil is activated.
		// 3. HEATING/FIXED TEMPERATURE: The unit is on, there is a heating load,
		//    and fixed temperature control is specified.  The outside air fraction
		//    is varied in an attempt to obtain a mixed air temperature equal to
		//    the user specified temperature (schedule based).  The heating coil
		//    is activated, if necessary.
		// 4. COOLING/NO COIL: The unit is on, there is a cooling load, and no
		//    coil is present or it has been scheduled off.  Set the amount of
		//    outside air based on the control type.  Simulate the "mixing box".
		// 5. COOLING/WITH COIL: The unit is on, there is a cooling load, and
		//    a cooling coil is present and scheduled on.  Tries to use outside
		//    air as best as possible and then calls a cooling coil
		// Note: controls are strictly temperature based and do not factor
		// humidity into the equation (not an enthalpy economy cycle but rather
		// a simple return air economy cycle).  In addition, temperature predictions
		// are not strict energy balances here in the control routine though
		// in the mixing routine an energy balance is preserved.

		// REFERENCES:
		// ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using HeatingCoils::CheckHeatingCoilSchedule;
		using WaterCoils::CheckWaterCoilSchedule;
		using HVACHXAssistedCoolingCoil::CheckHXAssistedCoolingCoilSchedule;
		using SteamCoils::CheckSteamCoilSchedule;
		using DataZoneEquipment::UnitVentilator_Num;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using PlantUtilities::SetComponentFlowRate;
		using General::SolveRegulaFalsi;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const LowTempDiff( 0.1 ); // Smallest allowed temperature difference for comparisons
		// (below this value the temperatures are assumed equal)
		Real64 const LowOAFracDiff( 0.01 ); // Smallest allowed outside air fraction difference for comparison
		// (below this value the fractions are assumed equal)
		int const MaxIter( 50 ); // maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // air mass flow rate [kg/sec]
		int AirRelNode; // outside air relief node
		int ControlNode; // the hot water or cold water inlet node
		Real64 ControlOffset; // tolerance for output control
		int InletNode; // unit air inlet node
		Real64 MaxOAFrac; // maximum possible outside air fraction
		Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/sec]
		Real64 MinOAFrac; // minimum possible outside air fraction
		Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/sec]
		int OutletNode; // unit air outlet node
		int OutsideAirNode; // outside air node
		Real64 QTotUnitOut; // total unit output [watts]
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 Tdesired; // desired temperature after mixing inlet and outdoor air [degrees C]
		Real64 Tinlet; // temperature of air coming into the unit ventilator [degrees C]
		Real64 Toutdoor; // temperature of outdoor air being introduced into the unit ventilator [degrees C]
		Real64 MaxSteamFlow;
		Real64 MinSteamFlow;
		Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
		Real64 mdot;
		Array1D< Real64 > Par( 3 ); // parameters passed to RegulaFalsi function
		int OpMode; // operatin gmode of the fan
		Real64 PartLoadFrac; // part load ratio of the unit ventilator
		Real64 NoOutput; // no load output of the unit ventilator
		Real64 FullOutput; // full load output of the unit ventilator
		int SolFlag; // return flag from RegulaFalsi for sensible load

		{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).CoilOption );
		if ( SELECT_CASE_var == BothOption ) {

			{ auto const SELECT_CASE_var1( UnitVent( UnitVentNum ).HCoilType );

			if ( SELECT_CASE_var1 == Heating_WaterCoilType ) {
				CheckWaterCoilSchedule( "Coil:Heating:Water", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_SteamCoilType ) {
				CheckSteamCoilSchedule( "Coil:Heating:Steam", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_ElectricCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Electric", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_GasCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Gas", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else {
				//      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%HCoilType))
			}}

			{ auto const SELECT_CASE_var1( UnitVent( UnitVentNum ).CCoilType );

			if ( SELECT_CASE_var1 == Cooling_CoilWaterCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water", UnitVent( UnitVentNum ).CCoilName, UnitVent( UnitVentNum ).CCoilSchedValue, UnitVent( UnitVentNum ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilDetailedCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water:DetailedGeometry", UnitVent( UnitVentNum ).CCoilName, UnitVent( UnitVentNum ).CCoilSchedValue, UnitVent( UnitVentNum ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilHXAssisted ) {
				CheckHXAssistedCoolingCoilSchedule( "CoilSystem:Cooling:Water:HeatExchangerAssisted", UnitVent( UnitVentNum ).CCoilName, UnitVent( UnitVentNum ).CCoilSchedValue, UnitVent( UnitVentNum ).CCoil_Index );
			} else {
				//      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%CCoilType))
			}}

		} else if ( SELECT_CASE_var == HeatingOption ) {

			{ auto const SELECT_CASE_var1( UnitVent( UnitVentNum ).HCoilType );

			if ( SELECT_CASE_var1 == Heating_WaterCoilType ) {
				CheckWaterCoilSchedule( "Coil:Heating:Water", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_SteamCoilType ) {
				CheckSteamCoilSchedule( "Coil:Heating:Steam", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_ElectricCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Electric", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_GasCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Gas", UnitVent( UnitVentNum ).HCoilName, UnitVent( UnitVentNum ).HCoilSchedValue, UnitVent( UnitVentNum ).HCoil_Index );
			} else {
				//      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%HCoilType))
			}}

		} else if ( SELECT_CASE_var == CoolingOption ) {

			{ auto const SELECT_CASE_var1( UnitVent( UnitVentNum ).CCoilType );

			if ( SELECT_CASE_var1 == Cooling_CoilWaterCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water", UnitVent( UnitVentNum ).CCoilName, UnitVent( UnitVentNum ).CCoilSchedValue, UnitVent( UnitVentNum ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilDetailedCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water:DetailedGeometry", UnitVent( UnitVentNum ).CCoilName, UnitVent( UnitVentNum ).CCoilSchedValue, UnitVent( UnitVentNum ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilHXAssisted ) {
				CheckHXAssistedCoolingCoilSchedule( "CoilSystem:Cooling:Water:HeatExchangerAssisted", UnitVent( UnitVentNum ).CCoilName, UnitVent( UnitVentNum ).CCoilSchedValue, UnitVent( UnitVentNum ).CCoil_Index );
			} else {
				//      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%CCoilType))

			}}

		} else if ( SELECT_CASE_var == NoneOption ) {

		}}

		// FLOW:
		FanElecPower = 0.0;
		// initialize local variables
		ControlNode = 0;
		QUnitOut = 0.0;
		LatentOutput = 0.0;
		ControlOffset = 0.0;
		MaxWaterFlow = 0.0;
		MinWaterFlow = 0.0;
		NoOutput = 0.0;
		FullOutput = 0.0;
		SolFlag = 0; // # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect
		InletNode = UnitVent( UnitVentNum ).AirInNode;
		OutletNode = UnitVent( UnitVentNum ).AirOutNode;
		OutsideAirNode = UnitVent( UnitVentNum ).OutsideAirNode;
		AirRelNode = UnitVent( UnitVentNum ).AirReliefNode;

		OpMode = UnitVent( UnitVentNum ).OpMode;
		PartLoadFrac = 0.0;

		if ( ( std::abs( QZnReq ) < SmallLoad ) || ( CurDeadBandOrSetback( ZoneNum ) ) || ( GetCurrentScheduleValue( UnitVent( UnitVentNum ).SchedPtr ) <= 0 ) || ( ( GetCurrentScheduleValue( UnitVent( UnitVentNum ).FanAvailSchedPtr ) <= 0 && ! ZoneCompTurnFansOn ) || ZoneCompTurnFansOff ) ) {

			// Unit is off or has no load upon it; set the flow rates to zero and then
			// simulate the components with the no flow conditions
			AirMassFlow = Node( OutletNode ).MassFlowRate;
			HCoilOn = false;
			if ( UnitVent( UnitVentNum ).HotControlNode > 0 ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, UnitVent( UnitVentNum ).HotControlNode, UnitVent( UnitVentNum ).HotCoilOutNodeNum, UnitVent( UnitVentNum ).HWLoopNum, UnitVent( UnitVentNum ).HWLoopSide, UnitVent( UnitVentNum ).HWBranchNum, UnitVent( UnitVentNum ).HWCompNum );
			}
			if ( UnitVent( UnitVentNum ).ColdControlNode > 0 ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, UnitVent( UnitVentNum ).ColdControlNode, UnitVent( UnitVentNum ).ColdCoilOutNodeNum, UnitVent( UnitVentNum ).CWLoopNum, UnitVent( UnitVentNum ).CWLoopSide, UnitVent( UnitVentNum ).CWBranchNum, UnitVent( UnitVentNum ).CWCompNum );
			}

			if ( OpMode == CycFanCycCoil ) {
				CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac );
				if ( Node( InletNode ).MassFlowRateMax > 0.0 ) {
					UnitVent( UnitVentNum ).FanPartLoadRatio = Node( InletNode ).MassFlowRate / Node( InletNode ).MassFlowRateMax;
				}
			} else {
				CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut );
			}
		} else { // Unit is on-->this section is intended to control the outside air and the main
			//              result is to set the outside air flow rate variable OAMassFlowRate
			UnitVent( UnitVentNum ).FanPartLoadRatio = 1.0;
			if ( QZnReq > SmallLoad ) { // HEATING MODE

				ControlNode = UnitVent( UnitVentNum ).HotControlNode;
				ControlOffset = UnitVent( UnitVentNum ).HotControlOffset;
				MaxWaterFlow = UnitVent( UnitVentNum ).MaxHotWaterFlow;
				MinWaterFlow = UnitVent( UnitVentNum ).MinHotWaterFlow;
				MaxSteamFlow = UnitVent( UnitVentNum ).MaxHotSteamFlow;
				MinSteamFlow = UnitVent( UnitVentNum ).MinHotSteamFlow;
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ! FirstHVACIteration && UnitVent( UnitVentNum ).HCoilType == Heating_WaterCoilType ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}

				if ( ! FirstHVACIteration && UnitVent( UnitVentNum ).HCoilType == Heating_SteamCoilType ) {
					MaxSteamFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinSteamFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				HCoilOn = true;

				if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
					MinOAFrac = GetCurrentScheduleValue( UnitVent( UnitVentNum ).MinOASchedPtr ) * ( UnitVent( UnitVentNum ).MinOutAirMassFlow / Node( OutsideAirNode ).MassFlowRate );
				} else {
					MinOAFrac = 0.0;
				}
				MinOAFrac = min( 1.0, max( 0.0, MinOAFrac ) );

				if ( ( ! UnitVent( UnitVentNum ).HCoilPresent ) || ( UnitVent( UnitVentNum ).HCoilSchedValue <= 0.0 ) ) {
					// In heating mode, but there is no coil to provide heating.  This is handled
					// differently than if there was a heating coil present.  Fixed temperature
					// will still try to vary the amount of outside air to meet the desired
					// mixed air temperature, while variable percent will go to full ventilation
					// when it is most advantageous.

					{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the minimum value
						// which is equal to the maximum value, regardless of all the other conditions.

						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// This algorithm is probably a bit simplistic in that it just bounces
						// back and forth between the maximum outside air and the minimum.  In
						// REAL(r64)ity, a system *might* vary between the two based on the load in
						// the zone.
						Tinlet = Node( InletNode ).Temp;
						Toutdoor = Node( OutsideAirNode ).Temp;

						if ( Tinlet >= Toutdoor ) {

							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

						} else { // Tinlet < Toutdoor

							MaxOAFrac = GetCurrentScheduleValue( UnitVent( UnitVentNum ).MaxOASchedPtr );
							OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;

						}

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// In heating mode, the outside air for "fixed temperature" attempts
						// to control the outside air fraction so that a desired temperature
						// is met (if possible).  If this desired temperature is between the
						// outside air temperature and the zone air temperature (inlet air
						// temperature), then this is possible.  If not, the control will try
						// to maximize the amount of air coming from the source that is closer
						// in temperature to the desired temperature.
						Tdesired = GetCurrentScheduleValue( UnitVent( UnitVentNum ).TempSchedPtr );
						Tinlet = Node( InletNode ).Temp;
						Toutdoor = Node( OutsideAirNode ).Temp;
						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( "ZoneHVAC:UnitVentilator simulation control: illogical condition for " + UnitVent( UnitVentNum ).Name );
						}

					}}

					if ( OpMode == CycFanCycCoil ) {
						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac );
						if ( Node( InletNode ).MassFlowRateMax > 0.0 ) {
							UnitVent( UnitVentNum ).FanPartLoadRatio = Node( InletNode ).MassFlowRate / Node( InletNode ).MassFlowRateMax;
						}
					} else {
						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut );
					}

				} else { //  Coil/no coil block
					// There is a heating load and there is a heating coil present (presumably).
					// Variable percent will throttle outside air back to the minimum while
					// fixed temperature will still try to vary the outside air amount to meet
					// the desired mixed air temperature.

					{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the maximum value
						// which is equal to the minimum value, regardless of all the other conditions.
						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// In heating mode, the outside air for "variable percent" control
						// is set to the minimum value
						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// In heating mode, the outside air for "fixed temperature" attempts
						// to control the outside air fraction so that a desired temperature
						// is met (if possible).  If this desired temperature is between the
						// outside air temperature and the zone air temperature (inlet air
						// temperature), then this is possible.  If not, the control will try
						// to maximize the amount of air coming from the source that is closer
						// in temperature to the desired temperature.
						Tdesired = GetCurrentScheduleValue( UnitVent( UnitVentNum ).TempSchedPtr );
						Tinlet = Node( InletNode ).Temp;
						Toutdoor = Node( OutsideAirNode ).Temp;
						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( "ZoneHVAC:UnitVentilator simulation control: illogical condition for " + UnitVent( UnitVentNum ).Name );
						}

					}}

					if ( OpMode == CycFanCycCoil ) {

						// Find part load ratio of unit ventilator coils
						PartLoadFrac = 0.0;
						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, NoOutput, OpMode, PartLoadFrac );
						if ( ( NoOutput - QZnReq ) < SmallLoad ) {
							// Unit ventilator is unable to meet the load with coil off, set PLR = 1
							PartLoadFrac = 1.0;
							CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, FullOutput, OpMode, PartLoadFrac );
							if ( ( FullOutput - QZnReq ) > SmallLoad ) {
								// Unit ventilator full load capacity is able to meet the load, Find PLR
								Par( 1 ) = double( UnitVentNum );
								Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
								if ( FirstHVACIteration ) Par( 2 ) = 1.0;
								Par( 3 ) = double( OpMode );
								// Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
								SolveRegulaFalsi( 0.001, MaxIter, SolFlag, PartLoadFrac, CalcUnitVentilatorResidual, 0.0, 1.0, Par );
							}
						}

						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac );
						UnitVent( UnitVentNum ).PartLoadFrac = PartLoadFrac;
						UnitVent( UnitVentNum ).FanPartLoadRatio = PartLoadFrac;

					} else { // Not a cycling operating mode

						{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).HCoilType );

						if ( SELECT_CASE_var == Heating_WaterCoilType ) {
							// control water flow to obtain output matching QZnReq
							ControlCompOutput( UnitVent( UnitVentNum ).Name, cMO_UnitVentilator, UnitVentNum, FirstHVACIteration, QZnReq, ControlNode, MaxWaterFlow, MinWaterFlow, ControlOffset, UnitVent( UnitVentNum ).ControlCompTypeNum, UnitVent( UnitVentNum ).CompErrIndex, _, _, _, _, _, UnitVent( UnitVentNum ).HWLoopNum, UnitVent( UnitVentNum ).HWLoopSide, UnitVent( UnitVentNum ).HWBranchNum );

						} else if ( ( SELECT_CASE_var == Heating_GasCoilType ) || ( SELECT_CASE_var == Heating_ElectricCoilType ) || ( SELECT_CASE_var == Heating_SteamCoilType ) ) {

							CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut );

						}}

					}
				} //  Coil/no coil block

			} else { // COOLING MODE

				ControlNode = UnitVent( UnitVentNum ).ColdControlNode;
				ControlOffset = UnitVent( UnitVentNum ).ColdControlOffset;
				MaxWaterFlow = UnitVent( UnitVentNum ).MaxColdWaterFlow;
				MinWaterFlow = UnitVent( UnitVentNum ).MinColdWaterFlow;
				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ( ! FirstHVACIteration ) && ( ControlNode > 0 ) && ( UnitVent( UnitVentNum ).CCoilPresent ) ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				HCoilOn = false;

				Tinlet = Node( InletNode ).Temp;
				Toutdoor = Node( OutsideAirNode ).Temp;

				if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
					MinOAFrac = GetCurrentScheduleValue( UnitVent( UnitVentNum ).MinOASchedPtr ) * ( UnitVent( UnitVentNum ).MinOutAirMassFlow / Node( OutsideAirNode ).MassFlowRate );
				} else {
					MinOAFrac = 0.0;
				}
				MinOAFrac = min( 1.0, max( 0.0, MinOAFrac ) );

				if ( ( ! UnitVent( UnitVentNum ).CCoilPresent ) || ( UnitVent( UnitVentNum ).CCoilSchedValue <= 0.0 ) ) {
					// In cooling mode, but there is no coil to provide cooling.  This is handled
					// differently than if there was a cooling coil present.  Fixed temperature
					// will still try to vary the amount of outside air to meet the desired
					// mixed air temperature, while variable percent will go to full ventilation
					// when it is most advantageous.
					{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the maximum value
						// which is equal to the minimum value, regardless of all the other conditions.
						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// This algorithm is probably a bit simplistic in that it just bounces
						// back and forth between the maximum outside air and the minimum.  In
						// REAL(r64)ity, a system *might* vary between the two based on the load in
						// the zone.  This simple flow control might cause some overcooling but
						// chances are that if there is a cooling load and the zone temperature
						// gets above the outside temperature that overcooling won't be significant.
						if ( Tinlet <= Toutdoor ) {

							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

						} else { // Tinlet > Toutdoor

							MaxOAFrac = GetCurrentScheduleValue( UnitVent( UnitVentNum ).MaxOASchedPtr );
							OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;

						}

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// This is basically the same algorithm as for the heating case...
						Tdesired = GetCurrentScheduleValue( UnitVent( UnitVentNum ).TempSchedPtr );
						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( "ZoneHVAC:UnitVentilator simulation control: illogical condition for " + UnitVent( UnitVentNum ).Name );
						}

					}}

					if ( OpMode == CycFanCycCoil ) {
						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac );
						if ( Node( InletNode ).MassFlowRateMax > 0.0 ) {
							UnitVent( UnitVentNum ).FanPartLoadRatio = Node( InletNode ).MassFlowRate / Node( InletNode ).MassFlowRateMax;
						}
					} else {
						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut );
					}

				} else {
					// There is a cooling load and there is a cooling coil present (presumably).
					// Variable percent will throttle outside air back to the minimum while
					// fixed temperature will still try to vary the outside air amount to meet
					// the desired mixed air temperature.

					{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the maximum value
						// which is equal to the minimum value, regardless of all the other conditions.
						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// A cooling coil is present so let it try to do the cooling...
						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// This is basically the same algorithm as for the heating case...
						Tdesired = GetCurrentScheduleValue( UnitVent( UnitVentNum ).TempSchedPtr );

						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( "ZoneHVAC:UnitVentilator simulation control: illogical condition for " + UnitVent( UnitVentNum ).Name );
						}

					}}

					if ( OpMode == CycFanCycCoil ) {

						HCoilOn = false;
						// Find part load ratio of unit ventilator coils
						PartLoadFrac = 0.0;
						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, NoOutput, OpMode, PartLoadFrac );
						if ( ( NoOutput - QZnReq ) > SmallLoad ) {
							// Unit ventilator is unable to meet the load with coil off, set PLR = 1
							PartLoadFrac = 1.0;
							CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, FullOutput, OpMode, PartLoadFrac );
							if ( ( FullOutput - QZnReq ) < SmallLoad ) {
								// Unit ventilator full load capacity is able to meet the load, Find PLR
								Par( 1 ) = double( UnitVentNum );
								Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
								if ( FirstHVACIteration ) Par( 2 ) = 1.0;
								Par( 3 ) = double( OpMode );
								// Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
								SolveRegulaFalsi( 0.001, MaxIter, SolFlag, PartLoadFrac, CalcUnitVentilatorResidual, 0.0, 1.0, Par );
							}
						}
						CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac );
						UnitVent( UnitVentNum ).PartLoadFrac = PartLoadFrac;
						UnitVent( UnitVentNum ).FanPartLoadRatio = PartLoadFrac;

					} else { // NOT a cycling operating mode
						// control water flow to obtain output matching QZnReq
						HCoilOn = false;
						ControlCompOutput( UnitVent( UnitVentNum ).Name, cMO_UnitVentilator, UnitVentNum, FirstHVACIteration, QZnReq, ControlNode, MaxWaterFlow, MinWaterFlow, ControlOffset, UnitVent( UnitVentNum ).ControlCompTypeNum, UnitVent( UnitVentNum ).CompErrIndex, _, _, _, _, _, UnitVent( UnitVentNum ).CWLoopNum, UnitVent( UnitVentNum ).CWLoopSide, UnitVent( UnitVentNum ).CWBranchNum );

					} // end from IF (OpMode .EQ. CycFanCycCoil) THEN

				}

			} // ...end of HEATING/COOLING IF-THEN block

			AirMassFlow = Node( OutletNode ).MassFlowRate;
			QUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		} // ...end of unit ON/OFF IF-THEN block

		// CR9155 Remove specific humidity calculations
		SpecHumOut = Node( OutletNode ).HumRat;
		SpecHumIn = Node( InletNode ).HumRat;
		LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative

		QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );

		// Report variables...
		UnitVent( UnitVentNum ).HeatPower = max( 0.0, QUnitOut );
		UnitVent( UnitVentNum ).SensCoolPower = std::abs( min( 0.0, QUnitOut ) );
		UnitVent( UnitVentNum ).TotCoolPower = std::abs( min( 0.0, QTotUnitOut ) );
		UnitVent( UnitVentNum ).ElecPower = FanElecPower;

		PowerMet = QUnitOut;
		LatOutputProvided = LatentOutput;

	}

	void
	CalcUnitVentilatorComponents(
		int const UnitVentNum, // Unit index in unit ventilator array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet, // load met by unit (watts)
		Optional_int_const OpMode, // Fan Type
		Optional< Real64 const > PartLoadFrac // Part Load Ratio of coil and fan
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine launches the individual component simulations.
		// This is called either when the unit is off to carry null conditions
		// through the unit or during control iterations to continue updating
		// what is going on within the unit.

		// METHODOLOGY EMPLOYED:
		// Simply calls the different components in order.  Only slight wrinkles
		// here are that the unit ventilator has it's own outside air mixed and
		// that a cooling coil must be present in order to call a cooling coil
		// simulation.  Other than that, the subroutine is very straightforward.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataZoneEquipment::UnitVentilator_Num;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataHVACGlobals::FanType_SimpleOnOff;
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
		Real64 AirMassFlow; // total mass flow through the unit
		Real64 CpAirZn; // specific heat of dry air at zone conditions (zone conditions same as unit inlet)
		int HCoilInAirNode; // inlet node number for fan exit/coil inlet
		int InletNode; // unit air inlet node
		int OutletNode; // unit air outlet node
		Real64 QCoilReq; // Heat addition required from an electric/gas heating coil
		Real64 mdot; // hot water or steam mass flow rate for current time step
		Real64 PartLoadRatio; // unit ventilator part load ratio
		int FanOpMode; // fan operating mode or fan type

		// FLOW:
		InletNode = UnitVent( UnitVentNum ).AirInNode;
		OutletNode = UnitVent( UnitVentNum ).AirOutNode;
		QCoilReq = QZnReq;

		if ( present( PartLoadFrac ) ) {
			PartLoadRatio = PartLoadFrac;
		} else {
			PartLoadRatio = 1.0;
		}
		if ( present( OpMode ) ) {
			FanOpMode = OpMode;
		} else {
			FanOpMode = ContFanCycCoil;
		}

		if ( FanOpMode != CycFanCycCoil ) {

			SimUnitVentOAMixer( UnitVentNum, FanOpMode );

			SimulateFanComponents( UnitVent( UnitVentNum ).FanName, FirstHVACIteration, UnitVent( UnitVentNum ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );

			if ( UnitVent( UnitVentNum ).CCoilPresent ) {
				if ( UnitVent( UnitVentNum ).CCoilType == Cooling_CoilHXAssisted ) {
					SimHXAssistedCoolingCoil( UnitVent( UnitVentNum ).CCoilName, FirstHVACIteration, On, 0.0, UnitVent( UnitVentNum ).CCoil_Index, ContFanCycCoil );
				} else {
					SimulateWaterCoilComponents( UnitVent( UnitVentNum ).CCoilName, FirstHVACIteration, UnitVent( UnitVentNum ).CCoil_Index );
				}
			}

			if ( UnitVent( UnitVentNum ).HCoilPresent ) {

				{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).HCoilType );

				if ( SELECT_CASE_var == Heating_WaterCoilType ) {

					SimulateWaterCoilComponents( UnitVent( UnitVentNum ).HCoilName, FirstHVACIteration, UnitVent( UnitVentNum ).HCoil_Index );

				} else if ( SELECT_CASE_var == Heating_SteamCoilType ) {

					if ( ! HCoilOn ) {
						QCoilReq = 0.0;
					} else {
						HCoilInAirNode = UnitVent( UnitVentNum ).FanOutletNode;
						CpAirZn = PsyCpAirFnWTdb( Node( UnitVent( UnitVentNum ).AirInNode ).HumRat, Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
						QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
					}

					if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool

					SimulateSteamCoilComponents( UnitVent( UnitVentNum ).HCoilName, FirstHVACIteration, UnitVent( UnitVentNum ).HCoil_Index, QCoilReq );

				} else if ( ( SELECT_CASE_var == Heating_ElectricCoilType ) || ( SELECT_CASE_var == Heating_GasCoilType ) ) {

					if ( ! HCoilOn ) {
						QCoilReq = 0.0;
					} else {
						HCoilInAirNode = UnitVent( UnitVentNum ).FanOutletNode;
						CpAirZn = PsyCpAirFnWTdb( Node( UnitVent( UnitVentNum ).AirInNode ).HumRat, Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
						QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
					}

					if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool

					SimulateHeatingCoilComponents( UnitVent( UnitVentNum ).HCoilName, FirstHVACIteration, QCoilReq, UnitVent( UnitVentNum ).HCoil_Index );

				}}

			} // (UnitVent(UnitVentNum)%HCoilPresent)

		} else { // Fan is Fan:OnOff and is cycling

			Node( InletNode ).MassFlowRate = Node( InletNode ).MassFlowRateMax * PartLoadRatio;
			AirMassFlow = Node( InletNode ).MassFlowRate;
			// Set the fan inlet node maximum available mass flow rates for cycling fans
			Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;

			SimUnitVentOAMixer( UnitVentNum, FanOpMode );

			SimulateFanComponents( UnitVent( UnitVentNum ).FanName, FirstHVACIteration, UnitVent( UnitVentNum ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );

			if ( UnitVent( UnitVentNum ).CCoilPresent ) {

				mdot = UnitVent( UnitVentNum ).MaxColdWaterFlow * PartLoadRatio;
				SetComponentFlowRate( mdot, UnitVent( UnitVentNum ).ColdControlNode, UnitVent( UnitVentNum ).ColdCoilOutNodeNum, UnitVent( UnitVentNum ).CWLoopNum, UnitVent( UnitVentNum ).CWLoopSide, UnitVent( UnitVentNum ).CWBranchNum, UnitVent( UnitVentNum ).CWCompNum );

				if ( UnitVent( UnitVentNum ).CCoilType == Cooling_CoilHXAssisted ) {
					SimHXAssistedCoolingCoil( UnitVent( UnitVentNum ).CCoilName, FirstHVACIteration, On, PartLoadRatio, UnitVent( UnitVentNum ).CCoil_Index, FanOpMode );
				} else {
					SimulateWaterCoilComponents( UnitVent( UnitVentNum ).CCoilName, FirstHVACIteration, UnitVent( UnitVentNum ).CCoil_Index, QCoilReq, FanOpMode, PartLoadRatio );
				}
			}

			if ( UnitVent( UnitVentNum ).HCoilPresent ) {

				{ auto const SELECT_CASE_var( UnitVent( UnitVentNum ).HCoilType );

				if ( SELECT_CASE_var == Heating_WaterCoilType ) {
					if ( ! HCoilOn ) {
						QCoilReq = 0.0;
						mdot = 0.0;
					} else {
						HCoilInAirNode = UnitVent( UnitVentNum ).FanOutletNode;
						CpAirZn = PsyCpAirFnWTdb( Node( UnitVent( UnitVentNum ).AirInNode ).HumRat, Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
						QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
						mdot = UnitVent( UnitVentNum ).MaxHotWaterFlow * PartLoadRatio;
					}

					if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool
					SetComponentFlowRate( mdot, UnitVent( UnitVentNum ).HotControlNode, UnitVent( UnitVentNum ).HotCoilOutNodeNum, UnitVent( UnitVentNum ).HWLoopNum, UnitVent( UnitVentNum ).HWLoopSide, UnitVent( UnitVentNum ).HWBranchNum, UnitVent( UnitVentNum ).HWCompNum );
					SimulateWaterCoilComponents( UnitVent( UnitVentNum ).HCoilName, FirstHVACIteration, UnitVent( UnitVentNum ).HCoil_Index, QCoilReq, FanOpMode, PartLoadRatio );

				} else if ( SELECT_CASE_var == Heating_SteamCoilType ) {

					if ( ! HCoilOn ) {
						QCoilReq = 0.0;
						mdot = 0.0;
					} else {
						HCoilInAirNode = UnitVent( UnitVentNum ).FanOutletNode;
						CpAirZn = PsyCpAirFnWTdb( Node( UnitVent( UnitVentNum ).AirInNode ).HumRat, Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
						QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
						mdot = UnitVent( UnitVentNum ).MaxHotSteamFlow * PartLoadFrac;
					}

					if ( QCoilReq < 0.0 ) QCoilReq = 0.0;
					SetComponentFlowRate( mdot, UnitVent( UnitVentNum ).HotControlNode, UnitVent( UnitVentNum ).HotCoilOutNodeNum, UnitVent( UnitVentNum ).HWLoopNum, UnitVent( UnitVentNum ).HWLoopSide, UnitVent( UnitVentNum ).HWBranchNum, UnitVent( UnitVentNum ).HWCompNum );
					SimulateSteamCoilComponents( UnitVent( UnitVentNum ).HCoilName, FirstHVACIteration, UnitVent( UnitVentNum ).HCoil_Index, QCoilReq, _, FanOpMode, PartLoadRatio );

				} else if ( ( SELECT_CASE_var == Heating_ElectricCoilType ) || ( SELECT_CASE_var == Heating_GasCoilType ) ) {

					if ( ! HCoilOn ) {
						QCoilReq = 0.0;
					} else {
						HCoilInAirNode = UnitVent( UnitVentNum ).FanOutletNode;
						CpAirZn = PsyCpAirFnWTdb( Node( UnitVent( UnitVentNum ).AirInNode ).HumRat, Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
						QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitVent( UnitVentNum ).AirInNode ).Temp );
					}
					if ( QCoilReq < 0.0 ) QCoilReq = 0.0;
					SimulateHeatingCoilComponents( UnitVent( UnitVentNum ).HCoilName, FirstHVACIteration, QCoilReq, UnitVent( UnitVentNum ).HCoil_Index, _, _, FanOpMode, PartLoadRatio );
				}}

			}

		}
		InletNode = UnitVent( UnitVentNum ).AirInNode;
		OutletNode = UnitVent( UnitVentNum ).AirOutNode;
		AirMassFlow = Node( OutletNode ).MassFlowRate;

		LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

	}

	void
	SimUnitVentOAMixer(
		int const UnitVentNum, // Unit index in unit ventilator array
		int const FanOpMode // unit ventilator fan operating mode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This responsibility of this subroutine is to set the air flow rates
		// through the mixing box portion of the unit ventilator and then perform
		// an energy balance to arrive at outlet conditions which then would
		// serve as inlet conditions to the coils (or outlet conditions for
		// the device).  There is some question as to whether this needs to be
		// called every time the coils and fan are called since how the fans and
		// coil operate won't presumable change how the mixer operates.  The
		// method in which this routine is called is slightly cleaner though
		// from a code readability standpoint though less efficient.

		// METHODOLOGY EMPLOYED:
		// The OAMassFlowRate has already been calculated in the main control
		// algorithm.  Use this flow rate to establish all of the other flow
		// rates and perform an energy balance on the mixing of the return and
		// outdoor air streams.

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
		int AirRelNode; // relief air node number in unit ventilator loop
		int InletNode; // inlet node number for unit ventilator loop
		Real64 OAFraction; // Outside air fraction of inlet air
		int OAMixOutNode; // outside air mixer outlet node for unit ventilator loop
		int OutsideAirNode; // outside air node number in unit ventilator loop
		Real64 OutAirMassFlowRate; // Outside air mass flow rate capped for cycling fan

		// FLOW:
		AirRelNode = UnitVent( UnitVentNum ).AirReliefNode;
		InletNode = UnitVent( UnitVentNum ).AirInNode;
		OAMixOutNode = UnitVent( UnitVentNum ).OAMixerOutNode;
		OutsideAirNode = UnitVent( UnitVentNum ).OutsideAirNode;
		OutAirMassFlowRate = OAMassFlowRate;

		// Limit the outdoor air mass flow rate if cycling fan
		if ( FanOpMode == CycFanCycCoil ) {
			OutAirMassFlowRate = min( OAMassFlowRate, Node( InletNode ).MassFlowRate );
		}

		// "Resolve" the air flow rates...
		Node( OutsideAirNode ).MassFlowRate = OutAirMassFlowRate;
		Node( OutsideAirNode ).MassFlowRateMinAvail = OutAirMassFlowRate;
		Node( OutsideAirNode ).MassFlowRateMaxAvail = OutAirMassFlowRate;

		Node( AirRelNode ).MassFlowRate = OutAirMassFlowRate;
		Node( AirRelNode ).MassFlowRateMinAvail = OutAirMassFlowRate;
		Node( AirRelNode ).MassFlowRateMaxAvail = OutAirMassFlowRate;

		Node( OAMixOutNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
		Node( OAMixOutNode ).MassFlowRateMinAvail = Node( InletNode ).MassFlowRate;
		Node( OAMixOutNode ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRate;

		// "Inlet" conditions for InletNode and OutsideAirNode have already
		// been set elsewhere so we just need to set the "outlet" conditions
		Node( AirRelNode ).Temp = Node( InletNode ).Temp;
		Node( AirRelNode ).Press = Node( InletNode ).Press;
		Node( AirRelNode ).HumRat = Node( InletNode ).HumRat;
		Node( AirRelNode ).Enthalpy = Node( InletNode ).Enthalpy;

		if ( Node( InletNode ).MassFlowRate > 0.0 ) {
			OAFraction = Node( OutsideAirNode ).MassFlowRate / Node( InletNode ).MassFlowRate;
		} else {
			OAFraction = 0.0;
		}

		// Perform an energy and moisture mass balance on the mixing portion of the unit ventilator
		Node( OAMixOutNode ).Enthalpy = OAFraction * Node( OutsideAirNode ).Enthalpy + ( 1.0 - OAFraction ) * Node( InletNode ).Enthalpy;
		Node( OAMixOutNode ).HumRat = OAFraction * Node( OutsideAirNode ).HumRat + ( 1.0 - OAFraction ) * Node( InletNode ).HumRat;

		// Find the other key state points based on calculated conditions
		Node( OAMixOutNode ).Temp = PsyTdbFnHW( Node( OAMixOutNode ).Enthalpy, Node( OAMixOutNode ).HumRat );
		Node( OAMixOutNode ).Press = Node( InletNode ).Press;

		if ( Contaminant.CO2Simulation ) {
			Node( AirRelNode ).CO2 = Node( InletNode ).CO2;
			Node( OAMixOutNode ).CO2 = OAFraction * Node( OutsideAirNode ).CO2 + ( 1.0 - OAFraction ) * Node( InletNode ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( AirRelNode ).GenContam = Node( InletNode ).GenContam;
			Node( OAMixOutNode ).GenContam = OAFraction * Node( OutsideAirNode ).GenContam + ( 1.0 - OAFraction ) * Node( InletNode ).GenContam;
		}

	}

	//SUBROUTINE UpdateUnitVentilator

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateUnitVentilator

	void
	ReportUnitVentilator( int const UnitVentNum ) // Unit index in unit ventilator array
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
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

		// FLOW:
		UnitVent( UnitVentNum ).HeatEnergy = UnitVent( UnitVentNum ).HeatPower * TimeStepSys * SecInHour;
		UnitVent( UnitVentNum ).SensCoolEnergy = UnitVent( UnitVentNum ).SensCoolPower * TimeStepSys * SecInHour;
		UnitVent( UnitVentNum ).TotCoolEnergy = UnitVent( UnitVentNum ).TotCoolPower * TimeStepSys * SecInHour;
		UnitVent( UnitVentNum ).ElecEnergy = UnitVent( UnitVentNum ).ElecPower * TimeStepSys * SecInHour;

	}

	int
	GetUnitVentilatorOutAirNode( int const UnitVentNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetUnitVentilatorOutAirNode;

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
		if ( GetUnitVentilatorInputFlag ) {
			GetUnitVentilatorInput();
			GetUnitVentilatorInputFlag = false;
		}

		GetUnitVentilatorOutAirNode = 0;
		if ( UnitVentNum > 0 && UnitVentNum <= NumOfUnitVents ) {
			GetUnitVentilatorOutAirNode = UnitVent( UnitVentNum ).OutsideAirNode;
		}

		return GetUnitVentilatorOutAirNode;

	}

	int
	GetUnitVentilatorZoneInletAirNode( int const UnitVentNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for zone air inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetUnitVentilatorZoneInletAirNode;

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
		if ( GetUnitVentilatorInputFlag ) {
			GetUnitVentilatorInput();
			GetUnitVentilatorInputFlag = false;
		}

		GetUnitVentilatorZoneInletAirNode = 0;
		if ( UnitVentNum > 0 && UnitVentNum <= NumOfUnitVents ) {
			GetUnitVentilatorZoneInletAirNode = UnitVent( UnitVentNum ).AirOutNode;
		}

		return GetUnitVentilatorZoneInletAirNode;

	}

	int
	GetUnitVentilatorMixedAirNode( int const UnitVentNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixed air node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetUnitVentilatorMixedAirNode;

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
		if ( GetUnitVentilatorInputFlag ) {
			GetUnitVentilatorInput();
			GetUnitVentilatorInputFlag = false;
		}

		GetUnitVentilatorMixedAirNode = 0;
		if ( UnitVentNum > 0 && UnitVentNum <= NumOfUnitVents ) {
			GetUnitVentilatorMixedAirNode = UnitVent( UnitVentNum ).OAMixerOutNode;
		}

		return GetUnitVentilatorMixedAirNode;

	}

	int
	GetUnitVentilatorReturnAirNode( int const UnitVentNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for return air node into "mixer"

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetUnitVentilatorReturnAirNode;

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
		if ( GetUnitVentilatorInputFlag ) {
			GetUnitVentilatorInput();
			GetUnitVentilatorInputFlag = false;
		}

		GetUnitVentilatorReturnAirNode = 0;
		if ( UnitVentNum > 0 && UnitVentNum <= NumOfUnitVents ) {
			GetUnitVentilatorReturnAirNode = UnitVent( UnitVentNum ).AirInNode;
		}

		return GetUnitVentilatorReturnAirNode;

	}

	Real64
	CalcUnitVentilatorResidual(
		Real64 const PartLoadRatio, // Coil Part Load Ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   October 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the part-load ratio for the unit ventilator.
		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to call this Function to converge on a solution
		// REFERENCES:
		// na
		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum( 0.0 ); // Result (force to 0)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  Parameter description:
		//  Par(1)  = REAL(UnitVentNum,r64)   ! Index to Unit Ventilator
		//  Par(2)  = 0.0                     ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//  Par(3)  = REAL(OpMode,r64)        ! Fan control, IF 1.0 then cycling fan, if 0.0 then continuous fan
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UnitVentNum; // Index to this UnitHeater
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Cycling fan or constant fan
		Real64 QUnitOut; // heating/Cooling provided by unit ventilator [watts]

		// Convert parameters to usable variables
		UnitVentNum = int( Par( 1 ) );
		FirstHVACIteration = ( Par( 2 ) == 1.0 );
		OpMode = int( Par( 3 ) );
		CalcUnitVentilatorComponents( UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadRatio );
		// Calculate residual based on output calculation flag
		if ( QZnReq != 0.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}
		return Residuum;
	}

} // UnitVentilator

} // EnergyPlus
