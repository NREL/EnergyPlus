// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EvaporativeCoolers.hh>
#include <BranchNodeConnections.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <DataZoneEnergyDemands.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace EvaporativeCoolers {
	// Module containing the EvaporativeCoolers simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   Oct 2000
	//       MODIFIED       BG July 2003 ResearchSpecial Indirect
	//                      BG Febraury 2007 outside air nodes
	//                      BG March 2009 ResearchSpecial Direct
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required for
	// Evaporative Coolers Components for use in mechanical air systems

	// provide models for evaporative coolers as zone forced air units.

	// METHODOLOGY EMPLOYED:
	// various evaporative component models in this module
	//   different models share common module level data structure.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
	using namespace DataLoopNode;
	using DataEnvironment::OutBaroPress;
	using namespace ScheduleManager;
	using namespace Psychrometrics;
	using namespace DataGlobalConstants;
	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const WaterSupplyFromMains( 101 );
	int const WaterSupplyFromTank( 102 );

	int const BlowThruFan( 50 );
	int const DrawThruFan( 51 );

	int const ZoneTemperatureDeadBandOnOffCycling( 20 );
	int const ZoneCoolingLoadOnOffCycling( 21 );
	int const ZoneCoolingLoadVariableSpeedFan( 22 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	bool GetInputEvapComponentsFlag( true ); // Flag set to make sure you get input once
	int NumEvapCool( 0 ); // The Number of Evap Coolers found in the Input
	FArray1D_bool MySizeFlag;
	FArray1D_bool CheckEquipName;

	int NumZoneEvapUnits( 0 );
	FArray1D_bool CheckZoneEvapUnitName;
	bool GetInputZoneEvapUnit( true );

	// SUBROUTINE SPECIFICATIONS FOR MODULE EvapCoolers

	// component model routines

	// zone unit routines

	// Object Data
	FArray1D< EvapConditions > EvapCond;
	FArray1D< ZoneEvapCoolerUnitStruct > ZoneEvapUnit;
	FArray1D< ZoneEvapCoolerUnitFieldData > ZoneEvapCoolerUnitFields;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SimEvapCooler(
		std::string const & CompName,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   October 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages EvapCooler component simulation.
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
		int EvapCoolNum; // The EvapCooler that you are currently loading input into

		// FLOW:

		// Obtains and Allocates EvapCooler related parameters from input file
		if ( GetInputEvapComponentsFlag ) { //First time subroutine has been entered
			GetEvapInput();
			GetInputEvapComponentsFlag = false;
		}

		// Find the correct EvapCoolNumber
		if ( CompIndex == 0 ) {
			EvapCoolNum = FindItemInList( CompName, EvapCond.EvapCoolerName(), NumEvapCool );
			if ( EvapCoolNum == 0 ) {
				ShowFatalError( "SimEvapCooler: Unit not found=" + CompName );
			}
			CompIndex = EvapCoolNum;
		} else {
			EvapCoolNum = CompIndex;
			if ( EvapCoolNum > NumEvapCool || EvapCoolNum < 1 ) {
				ShowFatalError( "SimEvapCooler:  Invalid CompIndex passed=" + TrimSigDigits( EvapCoolNum ) + ", Number of Units=" + TrimSigDigits( NumEvapCool ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( EvapCoolNum ) ) {
				if ( CompName != EvapCond( EvapCoolNum ).EvapCoolerName ) {
					ShowFatalError( "SimEvapCooler: Invalid CompIndex passed=" + TrimSigDigits( EvapCoolNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + EvapCond( EvapCoolNum ).EvapCoolerName );
				}
				CheckEquipName( EvapCoolNum ) = false;
			}
		}

		// With the correct EvapCoolNum Initialize
		InitEvapCooler( EvapCoolNum ); // Initialize all related parameters

		{ auto const SELECT_CASE_var( EvapCond( EvapCoolNum ).EvapCoolerType );

		if ( SELECT_CASE_var == iEvapCoolerDirectCELDEKPAD ) {
			CalcDirectEvapCooler( EvapCoolNum );
		} else if ( SELECT_CASE_var == iEvapCoolerInDirectCELDEKPAD ) {
			CalcDryIndirectEvapCooler( EvapCoolNum );
		} else if ( SELECT_CASE_var == iEvapCoolerInDirectWETCOIL ) {
			CalcWetIndirectEvapCooler( EvapCoolNum );
		} else if ( SELECT_CASE_var == iEvapCoolerInDirectRDDSpecial ) {
			CalcResearchSpecialPartLoad( EvapCoolNum );
			CalcIndirectResearchSpecialEvapCooler( EvapCoolNum );
		} else if ( SELECT_CASE_var == iEvapCoolerDirectResearchSpecial ) {
			CalcResearchSpecialPartLoad( EvapCoolNum );
			CalcDirectResearchSpecialEvapCooler( EvapCoolNum );
		}}
		// Update the current Evap Cooler to the outlet nodes
		UpdateEvapCooler( EvapCoolNum );

		// Report the current Evap Cooler
		ReportEvapCooler( EvapCoolNum );

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetEvapInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       BTG,  adding in EVAPCOOLER:INDIRECT:RDDSPECIAL
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
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using WaterManager::SetupTankDemandComponent;
		using OutAirNodeManager::CheckOutAirNodeNumber;

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
		int EvapCoolNum; // The EvapCooler that you are currently loading input into
		int NumDirectEvapCool; // The number of Direct CelDek EvapCooler in this simulation
		int NumDryInDirectEvapCool; // The number of dry indirect evap coolers
		int NumWetInDirectEvapCool; // The number of wet indirect evap coolers
		int NumRDDEvapCool; // the number of special research indirect evap coolers
		int NumDirectResearchSpecialEvapCool; // the number of special research direct evap coolers

		int IndEvapCoolNum; // Do Loop Counter for indirect evap coolers
		int DirectEvapCoolNum; // Do loop counter for direct evap cooler
		int NumAlphas;
		int NumNums;
		int IOStat;
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		// Start getting the input data
		NumDirectEvapCool = GetNumObjectsFound( "EvaporativeCooler:Direct:CelDekPad" );
		NumDryInDirectEvapCool = GetNumObjectsFound( "EvaporativeCooler:Indirect:CelDekPad" );
		NumWetInDirectEvapCool = GetNumObjectsFound( "EvaporativeCooler:Indirect:WetCoil" );
		NumRDDEvapCool = GetNumObjectsFound( "EvaporativeCooler:Indirect:ResearchSpecial" );
		NumDirectResearchSpecialEvapCool = GetNumObjectsFound( "EvaporativeCooler:Direct:ResearchSpecial" );

		//Sum up all of the Evap Cooler Types
		NumEvapCool = NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + NumRDDEvapCool + NumDirectResearchSpecialEvapCool;

		if ( NumEvapCool > 0 ) EvapCond.allocate( NumEvapCool );
		CheckEquipName.dimension( NumEvapCool, true );

		cCurrentModuleObject = "EvaporativeCooler:Direct:CelDekPad";

		for ( EvapCoolNum = 1; EvapCoolNum <= NumDirectEvapCool; ++EvapCoolNum ) {
			GetObjectItem( cCurrentModuleObject, EvapCoolNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), EvapCond.EvapCoolerName(), EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			EvapCond( EvapCoolNum ).EvapCoolerName = cAlphaArgs( 1 );
			EvapCond( EvapCoolNum ).EvapCoolerType = iEvapCoolerDirectCELDEKPAD;

			EvapCond( EvapCoolNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				EvapCond( EvapCoolNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				EvapCond( EvapCoolNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( EvapCond( EvapCoolNum ).SchedPtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			EvapCond( EvapCoolNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Evap Air Nodes" );

			EvapCond( EvapCoolNum ).EvapControlType = cAlphaArgs( 5 );

			//input the numerical data
			EvapCond( EvapCoolNum ).PadArea = rNumericArgs( 1 );
			EvapCond( EvapCoolNum ).PadDepth = rNumericArgs( 2 );
			EvapCond( EvapCoolNum ).RecircPumpPower = rNumericArgs( 3 );

			SetupOutputVariable( "Evaporative Cooler Wet Bulb Effectiveness []", EvapCond( EvapCoolNum ).SatEff, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );

			// A6 ; \Field Name of Water Supply Storage Tank
			EvapCond( EvapCoolNum ).EvapWaterSupplyName = cAlphaArgs( 6 );
			if ( lAlphaFieldBlanks( 6 ) ) {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( EvapCond( EvapCoolNum ).EvapCoolerName, cCurrentModuleObject, EvapCond( EvapCoolNum ).EvapWaterSupplyName, ErrorsFound, EvapCond( EvapCoolNum ).EvapWaterSupTankID, EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );

			}

		} // end Number of EvapCooler Loop

		//**************************************************************
		//This is the start of the Dry Indirect Evap Cooler Loop
		cCurrentModuleObject = "EvaporativeCooler:Indirect:CelDekPad";

		for ( IndEvapCoolNum = 1; IndEvapCoolNum <= NumDryInDirectEvapCool; ++IndEvapCoolNum ) {
			EvapCoolNum = NumDirectEvapCool + IndEvapCoolNum;

			GetObjectItem( cCurrentModuleObject, IndEvapCoolNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), EvapCond.EvapCoolerName(), EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			EvapCond( EvapCoolNum ).EvapCoolerName = cAlphaArgs( 1 );
			EvapCond( EvapCoolNum ).EvapCoolerType = iEvapCoolerInDirectCELDEKPAD; //'EvaporativeCooler:Indirect:CelDekPad'

			EvapCond( EvapCoolNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				EvapCond( EvapCoolNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				EvapCond( EvapCoolNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( EvapCond( EvapCoolNum ).SchedPtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			EvapCond( EvapCoolNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Evap Air Nodes" );

			EvapCond( EvapCoolNum ).EvapControlType = cAlphaArgs( 5 );

			//input the numerical data
			EvapCond( EvapCoolNum ).IndirectPadArea = rNumericArgs( 1 );
			EvapCond( EvapCoolNum ).IndirectPadDepth = rNumericArgs( 2 );
			EvapCond( EvapCoolNum ).IndirectRecircPumpPower = rNumericArgs( 3 );
			EvapCond( EvapCoolNum ).IndirectVolFlowRate = rNumericArgs( 4 );
			EvapCond( EvapCoolNum ).IndirectFanEff = rNumericArgs( 5 );
			EvapCond( EvapCoolNum ).IndirectFanDeltaPress = rNumericArgs( 6 );
			EvapCond( EvapCoolNum ).IndirectHXEffectiveness = rNumericArgs( 7 );

			SetupOutputVariable( "Evaporative Cooler Wetbulb Effectiveness []", EvapCond( EvapCoolNum ).SatEff, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );
			SetupOutputVariable( "Evaporative Cooler Total Stage Effectiveness []", EvapCond( EvapCoolNum ).StageEff, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );

			// A6 ; \Field Name of Water Supply Storage Tank
			EvapCond( EvapCoolNum ).EvapWaterSupplyName = cAlphaArgs( 6 );
			if ( lAlphaFieldBlanks( 6 ) ) {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( EvapCond( EvapCoolNum ).EvapCoolerName, cCurrentModuleObject, EvapCond( EvapCoolNum ).EvapWaterSupplyName, ErrorsFound, EvapCond( EvapCoolNum ).EvapWaterSupTankID, EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );

			}

			//A7 ; \field Secondary Outside Air Inlet node.
			if ( lAlphaFieldBlanks( 7 ) ) {
				EvapCond( EvapCoolNum ).SecondaryInletNode = 0;
			} else {
				EvapCond( EvapCoolNum ).SecondaryInletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( EvapCond( EvapCoolNum ).SecondaryInletNode ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					//TODO rename point
					ShowContinueError( "Node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}

			}

		} // end Number of Dry Indirect EvapCooler Loop

		//**************************************************************
		//This is the start of the WetIndirect Evap Cooler Loop
		cCurrentModuleObject = "EvaporativeCooler:Indirect:WetCoil";
		for ( IndEvapCoolNum = 1; IndEvapCoolNum <= NumWetInDirectEvapCool; ++IndEvapCoolNum ) {
			EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + IndEvapCoolNum;

			GetObjectItem( cCurrentModuleObject, IndEvapCoolNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), EvapCond.EvapCoolerName(), EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			EvapCond( EvapCoolNum ).EvapCoolerName = cAlphaArgs( 1 );
			EvapCond( EvapCoolNum ).EvapCoolerType = iEvapCoolerInDirectWETCOIL; //'EvaporativeCooler:Indirect:WetCoil'

			EvapCond( EvapCoolNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				EvapCond( EvapCoolNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				EvapCond( EvapCoolNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( EvapCond( EvapCoolNum ).SchedPtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			EvapCond( EvapCoolNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Evap Air Nodes" );

			EvapCond( EvapCoolNum ).EvapControlType = cAlphaArgs( 5 );

			//input the numerical data
			EvapCond( EvapCoolNum ).WetCoilMaxEfficiency = rNumericArgs( 1 );
			EvapCond( EvapCoolNum ).WetCoilFlowRatio = rNumericArgs( 2 );
			EvapCond( EvapCoolNum ).IndirectRecircPumpPower = rNumericArgs( 3 );
			EvapCond( EvapCoolNum ).IndirectVolFlowRate = rNumericArgs( 4 );
			EvapCond( EvapCoolNum ).IndirectFanEff = rNumericArgs( 5 );
			EvapCond( EvapCoolNum ).IndirectFanDeltaPress = rNumericArgs( 6 );

			SetupOutputVariable( "Evaporative Cooler Total Stage Effectiveness []", EvapCond( EvapCoolNum ).StageEff, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );

			//  A6 ; \Field Name of Water Supply Storage Tank
			EvapCond( EvapCoolNum ).EvapWaterSupplyName = cAlphaArgs( 6 );
			if ( lAlphaFieldBlanks( 6 ) ) {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( EvapCond( EvapCoolNum ).EvapCoolerName, cCurrentModuleObject, EvapCond( EvapCoolNum ).EvapWaterSupplyName, ErrorsFound, EvapCond( EvapCoolNum ).EvapWaterSupTankID, EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );

			}

			// A7 ; \field Secondary Outside Air Inlet node.
			if ( lAlphaFieldBlanks( 7 ) ) {
				EvapCond( EvapCoolNum ).SecondaryInletNode = 0;
			} else {
				EvapCond( EvapCoolNum ).SecondaryInletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( EvapCond( EvapCoolNum ).SecondaryInletNode ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					//TODO rename point
					ShowContinueError( "Node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}

			}

		} // end Number of Wet Coil Indirect EvapCooler Loop
		//**************************************************************
		//This is the start of the Indirect Research Special Evap Cooler
		cCurrentModuleObject = "EvaporativeCooler:Indirect:ResearchSpecial";
		for ( IndEvapCoolNum = 1; IndEvapCoolNum <= NumRDDEvapCool; ++IndEvapCoolNum ) {
			EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + IndEvapCoolNum;
			GetObjectItem( cCurrentModuleObject, IndEvapCoolNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), EvapCond.EvapCoolerName(), EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			EvapCond( EvapCoolNum ).EvapCoolerName = cAlphaArgs( 1 );
			EvapCond( EvapCoolNum ).EvapCoolerType = iEvapCoolerInDirectRDDSpecial; //'EvaporativeCooler:Indirect:ResearchSpecial'

			EvapCond( EvapCoolNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				EvapCond( EvapCoolNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				EvapCond( EvapCoolNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( EvapCond( EvapCoolNum ).SchedPtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			EvapCond( EvapCoolNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Evap Air Nodes" );

			EvapCond( EvapCoolNum ).EvapControlType = cAlphaArgs( 5 );

			// A6 ; \field Secondary Air Inlet Node Name
			if ( lAlphaFieldBlanks( 6 ) ) {
				EvapCond( EvapCoolNum ).SecondaryInletNode = 0;
			} else {
				EvapCond( EvapCoolNum ).SecondaryInletNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			}

			EvapCond( EvapCoolNum ).EvapControlNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).TertiaryInletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 3, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).EvapWaterSupplyName = cAlphaArgs( 9 );
			if ( lAlphaFieldBlanks( 9 ) ) {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( EvapCond( EvapCoolNum ).EvapCoolerName, cCurrentModuleObject, EvapCond( EvapCoolNum ).EvapWaterSupplyName, ErrorsFound, EvapCond( EvapCoolNum ).EvapWaterSupTankID, EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );

			}

			//input the numerical data
			EvapCond( EvapCoolNum ).WetCoilMaxEfficiency = rNumericArgs( 1 );
			EvapCond( EvapCoolNum ).WetCoilFlowRatio = rNumericArgs( 2 );
			EvapCond( EvapCoolNum ).IndirectRecircPumpPower = rNumericArgs( 3 );
			EvapCond( EvapCoolNum ).IndirectVolFlowRate = rNumericArgs( 4 );
			EvapCond( EvapCoolNum ).IndirectFanEff = rNumericArgs( 5 );
			EvapCond( EvapCoolNum ).IndirectFanDeltaPress = rNumericArgs( 6 );
			EvapCond( EvapCoolNum ).DPBoundFactor = rNumericArgs( 7 );
			if ( lNumericFieldBlanks( 8 ) ) {
				EvapCond( EvapCoolNum ).DriftFraction = 0.0;
			} else {
				EvapCond( EvapCoolNum ).DriftFraction = rNumericArgs( 8 );
			}

			if ( lNumericFieldBlanks( 9 ) ) {
				EvapCond( EvapCoolNum ).BlowDownRatio = 0.0;
			} else {
				EvapCond( EvapCoolNum ).BlowDownRatio = rNumericArgs( 9 );
			}

			SetupOutputVariable( "Evaporative Cooler Total Stage Effectiveness []", EvapCond( EvapCoolNum ).StageEff, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );
			SetupOutputVariable( "Evaporative Cooler Part Load Ratio []", EvapCond( EvapCoolNum ).PartLoadFract, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );

			SetupOutputVariable( "Evaporative Cooler Dewpoint Bound Status []", EvapCond( EvapCoolNum ).DewPointBoundFlag, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );

		} // end of Indirect Research Special cooler input loop

		cCurrentModuleObject = "EvaporativeCooler:Direct:ResearchSpecial";
		for ( DirectEvapCoolNum = 1; DirectEvapCoolNum <= NumDirectResearchSpecialEvapCool; ++DirectEvapCoolNum ) {
			EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + NumRDDEvapCool + DirectEvapCoolNum;
			GetObjectItem( cCurrentModuleObject, DirectEvapCoolNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), EvapCond.EvapCoolerName(), EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			EvapCond( EvapCoolNum ).EvapCoolerName = cAlphaArgs( 1 );
			EvapCond( EvapCoolNum ).EvapCoolerType = iEvapCoolerDirectResearchSpecial;

			EvapCond( EvapCoolNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				EvapCond( EvapCoolNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				EvapCond( EvapCoolNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( EvapCond( EvapCoolNum ).SchedPtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			EvapCond( EvapCoolNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Evap Air Nodes" );

			EvapCond( EvapCoolNum ).EvapControlNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).EvapWaterSupplyName = cAlphaArgs( 6 );

			if ( lAlphaFieldBlanks( 6 ) ) {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( EvapCond( EvapCoolNum ).EvapCoolerName, cCurrentModuleObject, EvapCond( EvapCoolNum ).EvapWaterSupplyName, ErrorsFound, EvapCond( EvapCoolNum ).EvapWaterSupTankID, EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );
			}
			EvapCond( EvapCoolNum ).DirectEffectiveness = rNumericArgs( 1 );
			EvapCond( EvapCoolNum ).RecircPumpPower = rNumericArgs( 2 );

			if ( lNumericFieldBlanks( 3 ) ) {
				EvapCond( EvapCoolNum ).DriftFraction = 0.0;
			} else {
				EvapCond( EvapCoolNum ).DriftFraction = rNumericArgs( 3 );
			}

			if ( lNumericFieldBlanks( 4 ) ) {
				EvapCond( EvapCoolNum ).BlowDownRatio = 0.0;
			} else {
				EvapCond( EvapCoolNum ).BlowDownRatio = rNumericArgs( 4 );
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for evaporative coolers" );
		}

		for ( EvapCoolNum = 1; EvapCoolNum <= NumEvapCool; ++EvapCoolNum ) {
			// Setup Report variables for the Evap Coolers
			SetupOutputVariable( "Evaporative Cooler Electric Energy [J]", EvapCond( EvapCoolNum ).EvapCoolerEnergy, "System", "Sum", EvapCond( EvapCoolNum ).EvapCoolerName, _, "Electric", "Cooling", _, "System" );
			SetupOutputVariable( "Evaporative Cooler Electric Power [W]", EvapCond( EvapCoolNum ).EvapCoolerPower, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );
			// this next report variable is setup differently depending on how the water should be metered here.
			if ( EvapCond( EvapCoolNum ).EvapWaterSupplyMode == WaterSupplyFromMains ) {
				SetupOutputVariable( "Evaporative Cooler Water Volume [m3]", EvapCond( EvapCoolNum ).EvapWaterConsump, "System", "Sum", EvapCond( EvapCoolNum ).EvapCoolerName, _, "Water", "Cooling", _, "System" );
				SetupOutputVariable( "Evaporative Cooler Mains Water Volume [m3]", EvapCond( EvapCoolNum ).EvapWaterConsump, "System", "Sum", EvapCond( EvapCoolNum ).EvapCoolerName, _, "MainsWater", "Cooling", _, "System" );

			} else if ( EvapCond( EvapCoolNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
				SetupOutputVariable( "Evaporative Cooler Storage Tank Water Volume [m3]", EvapCond( EvapCoolNum ).EvapWaterConsump, "System", "Sum", EvapCond( EvapCoolNum ).EvapCoolerName, _, "Water", "Cooling", _, "System" );
				SetupOutputVariable( "Evaporative Cooler Starved Water Volume [m3]", EvapCond( EvapCoolNum ).EvapWaterStarvMakup, "System", "Sum", EvapCond( EvapCoolNum ).EvapCoolerName, _, "Water", "Cooling", _, "System" );
				SetupOutputVariable( "Evaporative Cooler Starved Mains Water Volume [m3]", EvapCond( EvapCoolNum ).EvapWaterStarvMakup, "System", "Sum", EvapCond( EvapCoolNum ).EvapCoolerName, _, "MainsWater", "Cooling", _, "System" );
			}

		}

	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitEvapCooler( int const EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   October 2000
		//       MODIFIED       B. Griffith, May 2009, added EMS setpoint check
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for  initializations of the EvapCooler Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::DoSetPointTest;
		using DataHVACGlobals::SetPointErrorFlag;
		using DataEnvironment::OutAirDensity;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutEnthalpy;
		using DataEnvironment::OutWetBulbTemp;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int SecInletNode; // local index for secondary inlet node.
		Real64 RhoAir; // Air Density
		int ControlNode;
		int OutNode;
		int EvapUnitNum;
		static bool MySetPointCheckFlag( true );
		static bool MyOneTimeFlag( true );
		static bool localSetPointCheck( false );

		if ( MyOneTimeFlag ) {
			MySizeFlag.dimension( NumEvapCool, true );
			MyOneTimeFlag = false;
		}

		// FLOW:
		//Check that setpoint is active
		if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
			for ( EvapUnitNum = 1; EvapUnitNum <= NumEvapCool; ++EvapUnitNum ) {

				//only check evap coolers that are supposed to have a control node
				if ( ( EvapCond( EvapCoolNum ).EvapCoolerType != iEvapCoolerInDirectRDDSpecial ) && ( EvapCond( EvapCoolNum ).EvapCoolerType != iEvapCoolerDirectResearchSpecial ) ) continue;

				ControlNode = EvapCond( EvapUnitNum ).EvapControlNodeNum;
				if ( ControlNode > 0 ) {
					if ( Node( ControlNode ).TempSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							ShowSevereError( "Missing temperature setpoint for Evap Cooler unit " + EvapCond( EvapCoolNum ).EvapCoolerName );
							ShowContinueError( " use a Setpoint Manager to establish a setpoint at the unit control node." );
						} else {
							localSetPointCheck = false;
							CheckIfNodeSetPointManagedByEMS( ControlNode, iTemperatureSetPoint, localSetPointCheck );
							if ( localSetPointCheck ) {
								ShowSevereError( "Missing temperature setpoint for Evap Cooler unit " + EvapCond( EvapCoolNum ).EvapCoolerName );
								ShowContinueError( " use a Setpoint Manager to establish a setpoint at the unit control node." );
								ShowContinueError( " or use an EMS actuator to establish a setpoint at the unit control node." );
							}
						}
					}
				}
			}
			MySetPointCheckFlag = false;
		}

		if ( ! SysSizingCalc && MySizeFlag( EvapCoolNum ) ) {
			// for each cooler, do the sizing once.
			SizeEvapCooler( EvapCoolNum );

			MySizeFlag( EvapCoolNum ) = false;
		}

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.

		//Transfer the node data to EvapCond data structure
		InletNode = EvapCond( EvapCoolNum ).InletNode;

		RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, Node( InletNode ).Temp, Node( InletNode ).HumRat );

		// set the volume flow rates from the input mass flow rates
		EvapCond( EvapCoolNum ).VolFlowRate = Node( InletNode ).MassFlowRate / RhoAir;

		// Calculate the entering wet bulb temperature for inlet conditions
		EvapCond( EvapCoolNum ).InletWetBulbTemp = PsyTwbFnTdbWPb( Node( InletNode ).Temp, Node( InletNode ).HumRat, OutBaroPress );

		//Set all of the inlet mass flow variables from the nodes
		EvapCond( EvapCoolNum ).InletMassFlowRate = Node( InletNode ).MassFlowRate;
		EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail = Node( InletNode ).MassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail = Node( InletNode ).MassFlowRateMinAvail;
		//Set all of the inlet state variables from the inlet nodes
		EvapCond( EvapCoolNum ).InletTemp = Node( InletNode ).Temp;
		EvapCond( EvapCoolNum ).InletHumRat = Node( InletNode ).HumRat;
		EvapCond( EvapCoolNum ).InletEnthalpy = Node( InletNode ).Enthalpy;
		EvapCond( EvapCoolNum ).InletPressure = Node( InletNode ).Press;
		//Set default outlet state to inlet states(?)
		EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
		EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;
		EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;
		EvapCond( EvapCoolNum ).OutletPressure = EvapCond( EvapCoolNum ).InletPressure;

		EvapCond( EvapCoolNum ).OutletMassFlowRate = EvapCond( EvapCoolNum ).InletMassFlowRate;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail;

		//Set all of the secondary inlet mass flow variables from the nodes
		SecInletNode = EvapCond( EvapCoolNum ).SecondaryInletNode;
		if ( SecInletNode != 0 ) {
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = Node( SecInletNode ).MassFlowRate;
			EvapCond( EvapCoolNum ).SecInletMassFlowRateMaxAvail = Node( SecInletNode ).MassFlowRateMaxAvail;
			EvapCond( EvapCoolNum ).SecInletMassFlowRateMinAvail = Node( SecInletNode ).MassFlowRateMinAvail;
			EvapCond( EvapCoolNum ).SecInletTemp = Node( SecInletNode ).Temp;
			EvapCond( EvapCoolNum ).SecInletHumRat = Node( SecInletNode ).HumRat;
			EvapCond( EvapCoolNum ).SecInletEnthalpy = Node( SecInletNode ).Enthalpy;
			EvapCond( EvapCoolNum ).SecInletPressure = Node( SecInletNode ).Press;
		} else {
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = EvapCond( EvapCoolNum ).IndirectVolFlowRate * OutAirDensity;
			EvapCond( EvapCoolNum ).SecInletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).IndirectVolFlowRate * OutAirDensity;
			EvapCond( EvapCoolNum ).SecInletMassFlowRateMinAvail = 0.0;
			EvapCond( EvapCoolNum ).SecInletTemp = OutDryBulbTemp;
			EvapCond( EvapCoolNum ).SecInletHumRat = PsyWFnTdbTwbPb( OutDryBulbTemp, OutWetBulbTemp, OutBaroPress );
			EvapCond( EvapCoolNum ).SecInletEnthalpy = OutEnthalpy;
			EvapCond( EvapCoolNum ).SecInletPressure = OutBaroPress;
		}
		//Set the energy consumption to zero each time through for reporting
		EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;
		EvapCond( EvapCoolNum ).EvapCoolerPower = 0.0;
		EvapCond( EvapCoolNum ).DewPointBoundFlag = 0;
		//Set the water consumption to zero each time through for reporting
		EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;
		EvapCond( EvapCoolNum ).EvapWaterConsump = 0.0;
		EvapCond( EvapCoolNum ).EvapWaterStarvMakup = 0.0;

		//Set the Saturation and Stage Efficiency to zero each time through for reporting
		EvapCond( EvapCoolNum ).StageEff = 0.0;
		EvapCond( EvapCoolNum ).SatEff = 0.0;

		// These initializations are done every iteration
		OutNode = EvapCond( EvapCoolNum ).OutletNode;
		ControlNode = EvapCond( EvapCoolNum ).EvapControlNodeNum;

		if ( ControlNode == 0 ) {
			EvapCond( EvapCoolNum ).DesiredOutletTemp = 0.0;
		} else if ( ControlNode == OutNode ) {
			EvapCond( EvapCoolNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint;
		} else {
			EvapCond( EvapCoolNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint - ( Node( ControlNode ).Temp - Node( OutNode ).Temp );
		}

	}

	void
	SizeEvapCooler( int const EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Size calculations for Evap coolers
		//  currently just for secondary side of Research Special Indirect evap cooler

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataAirSystems::PrimaryAirSystem;
		using InputProcessor::SameString;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool CoolerOnOApath( false );
		static bool CoolerOnMainAirLoop( false );
		//unused0509  INTEGER :: OAsysIndex = 0
		static int AirSysBranchLoop( 0 );
		//unused0509  INTEGER :: OAsysLoop = 0
		//unuse0509  INTEGER :: OAcompLoop = 0
		static int BranchComp( 0 );

		//inits
		CoolerOnOApath = false;
		CoolerOnMainAirLoop = false;

		if ( EvapCond( EvapCoolNum ).IndirectVolFlowRate == AutoSize ) {
			if ( CurSysNum > 0 ) { //central system
				//where is this cooler located, is it on OA system or main loop?
				// search for this component in Air loop branches.
				for ( AirSysBranchLoop = 1; AirSysBranchLoop <= PrimaryAirSystem( CurSysNum ).NumBranches; ++AirSysBranchLoop ) {
					for ( BranchComp = 1; BranchComp <= PrimaryAirSystem( CurSysNum ).Branch( AirSysBranchLoop ).TotalComponents; ++BranchComp ) {

						if ( SameString( PrimaryAirSystem( CurSysNum ).Branch( AirSysBranchLoop ).Comp( BranchComp ).Name, EvapCond( EvapCoolNum ).EvapCoolerName ) ) {
							CoolerOnMainAirLoop = true;
						}

					}
				}

				// would like search for this componenent in some OutsideAirSys structure
				// but thats not so easy becuase of circular USE with MixedAir.cc
				//  So assume if its not on main air path, its on OA path (for now)
				if ( ! CoolerOnMainAirLoop ) CoolerOnOApath = true;

				if ( CoolerOnMainAirLoop ) {
					EvapCond( EvapCoolNum ).IndirectVolFlowRate = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				} else if ( CoolerOnOApath ) {
					EvapCond( EvapCoolNum ).IndirectVolFlowRate = max( FinalSysSizing( CurSysNum ).DesOutAirVolFlow, 0.5 * FinalSysSizing( CurSysNum ).DesMainVolFlow );
				}

			} else { //zone equipment
				// we have no zone equipment evap coolers yet

			}

			ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName, "Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).IndirectVolFlowRate );
		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcDirectEvapCooler( int & EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   October 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//REAL(r64) Variables
		Real64 PadDepth; // EvapCooler Pad Depth in Meters as input by the User
		Real64 SatEff; // Saturation Efficiency of the CelDek Pad
		Real64 AirVel; // The Calculated Air Velocity through the Pad
		Real64 TEDB; // Entering Dry Bulb Temperature
		Real64 TEWB; // Entering Wet Bulb Temperature
		Real64 RhoWater;

		// If the Evaporative Cooler  is operating there should be some mass flow rate
		//  Also the evap cooler has to be scheduled to be available
		if ( ( EvapCond( EvapCoolNum ).InletMassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( EvapCond( EvapCoolNum ).SchedPtr ) > 0.0 ) ) {

			PadDepth = EvapCond( EvapCoolNum ).PadDepth;
			//******************************************************************************
			//   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
			//   FOR A DIRECT EVAPORATIVE AIR COOLER SUPPLIED WITH CFMAir,DIRPAD,TEWB,TEDB,
			//   AND PB (ATM. PRESS.) FOR AIR DENSITY CALCULATIONS.
			//******************************************************************************

			AirVel = EvapCond( EvapCoolNum ).VolFlowRate / EvapCond( EvapCoolNum ).PadArea;

			//******************************************************************************
			//   SAT EFF IS FOR DIFFERENT THICKNESS CELDEK PAD (CURVE FIT FROM DATA)
			//******************************************************************************
			SatEff = 0.792714 + 0.958569 * PadDepth - 0.25193 * AirVel - 1.03215 * pow_2( PadDepth ) + 2.62659e-2 * pow_2( AirVel ) + 0.914869 * PadDepth * AirVel - 1.48241 * AirVel * pow_2( PadDepth ) - 1.89919e-2 * pow_3( AirVel ) * PadDepth + 1.13137 * pow_3( PadDepth ) * AirVel + 3.27622e-2 * pow_3( AirVel ) * pow_2( PadDepth ) - 0.145384 * pow_3( PadDepth ) * pow_2( AirVel );

			if ( SatEff >= 1.0 ) SatEff = 1.0;
			if ( SatEff < 0.0 ) { // we have a serious problem.  Pad Area and/or depth not suitable for system air flow rates
				ShowSevereError( "EVAPCOOLER:DIRECT:CELDEKPAD: " + EvapCond( EvapCoolNum ).EvapCoolerName + " has a problem" );
				ShowContinueError( "Check size of Pad Area and/or Pad Depth in input" );
				ShowContinueError( "Cooler Effectiveness calculated as: " + RoundSigDigits( SatEff, 2 ) );
				ShowContinueError( "Air velocity (m/s) through pads calculated as: " + RoundSigDigits( AirVel, 2 ) );
				ShowFatalError( "Program Terminates due to previous error condition" );

			}
			EvapCond( EvapCoolNum ).SatEff = SatEff;
			//***************************************************************************
			//   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
			//   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
			//   ACROSS A DIRECT EVAPORATION COOLER.
			TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
			TEDB = EvapCond( EvapCoolNum ).InletTemp;

			EvapCond( EvapCoolNum ).OutletTemp = TEDB - ( ( TEDB - TEWB ) * SatEff );

			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;

			EvapCond( EvapCoolNum ).OutletHumRat = PsyWFnTdbTwbPb( EvapCond( EvapCoolNum ).OutletTemp, TEWB, OutBaroPress );

			EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );

			//***************************************************************************
			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//Add the pump energy to the total Evap Cooler energy comsumption
			EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).RecircPumpPower;

			//******************
			//             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
			//             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
			//                                /RhoWater [kg H2O/m3 H2O]
			//******************
			RhoWater = RhoH2O( EvapCond( EvapCoolNum ).OutletTemp );
			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = ( EvapCond( EvapCoolNum ).OutletHumRat - EvapCond( EvapCoolNum ).InletHumRat ) * EvapCond( EvapCoolNum ).InletMassFlowRate / RhoWater;
			// A numerical check to keep from having very tiny negative water consumption values being reported
			if ( EvapCond( EvapCoolNum ).EvapWaterConsumpRate < 0.0 ) EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		} else {
			// The evap cooler is not running and does not change conditions from inlet to outlet
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;

			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;

			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;

			EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;

			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		}
		// all of the mass flowrates are not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletMassFlowRate = EvapCond( EvapCoolNum ).InletMassFlowRate;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail;

		// the pressure is not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletPressure = EvapCond( EvapCoolNum ).InletPressure;

	}

	void
	CalcDryIndirectEvapCooler( int & EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   October 2000
		//       MODIFIED       BG Feb. 2007 secondary air inlet node
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

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
		//REAL(r64) Variables
		Real64 PadDepth; // EvapCooler Pad Depth in Meters as input by the User
		Real64 SatEff; // Saturation Efficiency of the CelDek Pad
		Real64 AirVel; // The Calculated Air Velocity through the Pad
		Real64 TDBSec; // Secondary leaving dry bulb
		Real64 TWBSec; // Secondary Leaving Wet Bulb
		Real64 HumRatSec; // Secondary leaving Humidity Ratio
		Real64 EffHX; // Effectiveness of Secondary Heat Exchanger
		Real64 QHX; // Q Across Sec HX
		Real64 RhoWater;
		Real64 RhoAir; // Density of the primary side air
		Real64 CpAir; // Cp of the primary side air
		Real64 CFMAir;
		Real64 CFMSec;

		// If the Evaporative Cooler  is operating there should be some mass flow rate
		//  Also the evap cooler has to be scheduled to be available
		if ( ( EvapCond( EvapCoolNum ).InletMassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( EvapCond( EvapCoolNum ).SchedPtr ) > 0.0 ) ) {

			PadDepth = EvapCond( EvapCoolNum ).IndirectPadDepth;
			//******************************************************************************
			//   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
			//   FOR A DIRECT EVAPORATIVE AIR COOLER SUPPLIED WITH CFMAir,DIRPAD,TEWB,TEDB,
			//   AND PB (ATM. PRESS.) FOR AIR DENSITY CALCULATIONS.
			//******************************************************************************

			AirVel = EvapCond( EvapCoolNum ).IndirectVolFlowRate / EvapCond( EvapCoolNum ).IndirectPadArea;

			//******************************************************************************
			//   SAT EFF IS FOR DIFFERENT THICKNESS CELDEK PAD (CURVE FIT FROM DATA)
			//******************************************************************************
			SatEff = 0.792714 + 0.958569 * PadDepth - 0.25193 * AirVel - 1.03215 * pow_2( PadDepth ) + 2.62659e-2 * pow_2( AirVel ) + 0.914869 * PadDepth * AirVel - 1.48241 * AirVel * pow_2( PadDepth ) - 1.89919e-2 * pow_3( AirVel ) * PadDepth + 1.13137 * pow_3( PadDepth ) * AirVel + 3.27622e-2 * pow_3( AirVel ) * pow_2( PadDepth ) - 0.145384 * pow_3( PadDepth ) * pow_2( AirVel );

			if ( SatEff >= 1.0 ) SatEff = 1.0;
			EvapCond( EvapCoolNum ).SatEff = SatEff;
			//***************************************************************************
			//   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
			//   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE PAD BEFORE THE HX.
			//***************************************************************************
			//***** FIRST CHECK IF THIS TEWB IS A FEASIBLE POINT ON PSYCH CHART**********

			// BG Feb 2007 mods for oa node (eg. height-dependent outside air model)
			TWBSec = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).SecInletTemp, EvapCond( EvapCoolNum ).SecInletHumRat, EvapCond( EvapCoolNum ).SecInletPressure ); //  OutWetBulbTemp
			TDBSec = EvapCond( EvapCoolNum ).SecInletTemp - ( ( EvapCond( EvapCoolNum ).SecInletTemp - TWBSec ) * SatEff );

			HumRatSec = PsyWFnTdbTwbPb( TDBSec, TWBSec, EvapCond( EvapCoolNum ).SecInletPressure );

			//***************************************************************************
			//                  CALCULATE THE TLDB FROM HX EQUATIONS GIVEN AN EFFICIENCY
			//***************************************************************************
			EffHX = EvapCond( EvapCoolNum ).IndirectHXEffectiveness;
			CpAir = PsyCpAirFnWTdb( EvapCond( EvapCoolNum ).InletHumRat, EvapCond( EvapCoolNum ).InletTemp );
			RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, EvapCond( EvapCoolNum ).InletTemp, EvapCond( EvapCoolNum ).InletHumRat );
			CFMAir = EvapCond( EvapCoolNum ).VolFlowRate; //Volume Flow Rate Primary Side
			CFMSec = EvapCond( EvapCoolNum ).IndirectVolFlowRate; //Volume Flolw Rate Secondary Side

			QHX = EffHX * min( CFMSec, CFMAir ) * RhoAir * CpAir * ( EvapCond( EvapCoolNum ).InletTemp - TDBSec );
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp - QHX / ( RhoAir * CFMAir * CpAir );
			// This is a rough approximation of the Total Indirect Stage Efficiency for the Dry stage which
			//   is a 2 step process the first being teh pad efficiency and then the HX Effectiveness.  I think that
			//   this would mainly be used for evap sizing purposes.
			EvapCond( EvapCoolNum ).StageEff = SatEff * EffHX;
			//***************************************************************************
			//                  CALCULATE THE WET BULB TEMP in the primary system air USING PSYCH ROUTINES
			// There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
			EvapCond( EvapCoolNum ).OuletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).InletHumRat, OutBaroPress );
			//***************************************************************************
			//   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
			//   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
			//   ACROSS A DIRECT EVAPORATION COOLER.

			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );

			//***************************************************************************
			//                  POWER OF THE SECONDARY AIR FAN
			if ( EvapCond( EvapCoolNum ).IndirectFanEff > 0.0 ) {
				EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectFanDeltaPress * EvapCond( EvapCoolNum ).IndirectVolFlowRate / EvapCond( EvapCoolNum ).IndirectFanEff;
			}

			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//Add the pump energy to the total Evap Cooler energy comsumption
			EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectRecircPumpPower;

			//******************
			//             WATER CONSUMPTION IN LITERS OF WATER FOR DIRECT
			//             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
			//                                /RhoWater [kg H2O/m3 H2O]
			//******************
			RhoWater = RhoH2O( TDBSec );
			RhoAir = ( PsyRhoAirFnPbTdbW( EvapCond( EvapCoolNum ).SecInletPressure, EvapCond( EvapCoolNum ).SecInletTemp, EvapCond( EvapCoolNum ).SecInletHumRat ) + PsyRhoAirFnPbTdbW( EvapCond( EvapCoolNum ).SecInletPressure, TDBSec, HumRatSec ) ) / 2.0;
			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = ( HumRatSec - EvapCond( EvapCoolNum ).SecInletHumRat ) * EvapCond( EvapCoolNum ).IndirectVolFlowRate * RhoAir / RhoWater;
			// A numerical check to keep from having very tiny negative water consumption values being reported
			if ( EvapCond( EvapCoolNum ).EvapWaterConsumpRate < 0.0 ) EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		} else {
			// The evap cooler is not running and does not change conditions from inlet to outlet
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;

			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;

			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;

			EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;

			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		}
		// all of the mass flowrates are not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletMassFlowRate = EvapCond( EvapCoolNum ).InletMassFlowRate;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail;

		// the pressure is not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletPressure = EvapCond( EvapCoolNum ).InletPressure;

	}

	void
	CalcWetIndirectEvapCooler( int & EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   October 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//     Use DataEnvironment, ONLY: OutDryBulbTemp, OutWetBulbTemp, OutHumRat, OutBaroPress

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//REAL(r64) Variables
		Real64 StageEff; // Stage Efficiency of the Heat Exchanger
		Real64 TEDB; // Entering Dry Bulb Temperature
		Real64 TEWB; // Entering Wet Bulb Temperature
		Real64 QHX; // Q Across Sec HX in Watts or J/sec
		Real64 RhoWater;
		Real64 RhoAir; // Density of the primary side air
		Real64 CFMAir;
		Real64 CFMSec;
		Real64 TWBSec; // wet bulb of secondary air

		// If the Evaporative Cooler  is operating there should be some mass flow rate
		//  Also the evap cooler has to be scheduled to be available
		if ( ( EvapCond( EvapCoolNum ).InletMassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( EvapCond( EvapCoolNum ).SchedPtr ) > 0.0 ) ) {

			//******************************************************************************
			//   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
			//   FOR A WET COIL EVAPORATIVE COOLER
			//******************************************************************************
			//  INDIRECT STAGE EFFICIENCY FOR WET COIL INDIRECT EVAP COOLERS
			CFMAir = EvapCond( EvapCoolNum ).VolFlowRate; //Volume Flow Rate Primary Side
			CFMSec = EvapCond( EvapCoolNum ).IndirectVolFlowRate; //Volume Flolw Rate Secondary Side

			StageEff = EvapCond( EvapCoolNum ).WetCoilMaxEfficiency - min( EvapCond( EvapCoolNum ).WetCoilFlowRatio * CFMAir / CFMSec, EvapCond( EvapCoolNum ).WetCoilMaxEfficiency );

			if ( StageEff >= 1.0 ) StageEff = 1.0;
			// This is a rough approximation of the Total Indirect Stage Efficiency.  I think that
			//   this would mainly be used for evap sizing purposes.
			EvapCond( EvapCoolNum ).StageEff = StageEff;
			//***************************************************************************
			//   TEMP LEAVING DRY BULB IS CALCULATED FROM A SIMPLE WET BULB APPROACH
			//   MODEL GIVEN THE INDIRECT STAGE EFFICIENCY.
			//   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE INDIRECT STAGE.
			//***************************************************************************
			//                  CALCULATE THE TLDB
			TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
			TEDB = EvapCond( EvapCoolNum ).InletTemp;
			TWBSec = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).SecInletTemp, EvapCond( EvapCoolNum ).SecInletHumRat, EvapCond( EvapCoolNum ).SecInletPressure );
			EvapCond( EvapCoolNum ).OutletTemp = TEDB - StageEff * ( TEDB - TWBSec );

			//***************************************************************************
			//                  CALCULATE THE WET BULB TEMP in the primary system air using PSYCH ROUTINES
			// There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
			EvapCond( EvapCoolNum ).OuletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).InletHumRat, OutBaroPress );
			//***************************************************************************
			//                  CALCULATE other outlet properties using PSYCH ROUTINES
			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );

			//***************************************************************************
			//                  POWER OF THE SECONDARY AIR FAN
			if ( EvapCond( EvapCoolNum ).IndirectFanEff > 0.0 ) {
				EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectFanDeltaPress * EvapCond( EvapCoolNum ).IndirectVolFlowRate / EvapCond( EvapCoolNum ).IndirectFanEff;
			}

			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//Add the pump energy to the total Evap Cooler energy comsumption
			EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectRecircPumpPower;

			//******************
			//             WATER CONSUMPTION IN LITERS OF WATER FOR Wet InDIRECT
			//             H2O [m3/sec] = (QHX [J/s])/(2,500,000 [J/kg H2O] * RhoWater [kg H2O/m3 H2O])
			//******************
			//***** FIRST calculate the heat exchange on the primary air side**********
			RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, EvapCond( EvapCoolNum ).InletTemp, EvapCond( EvapCoolNum ).InletHumRat );
			QHX = CFMAir * RhoAir * ( EvapCond( EvapCoolNum ).InletEnthalpy - EvapCond( EvapCoolNum ).OutletEnthalpy );

			RhoWater = RhoH2O( EvapCond( EvapCoolNum ).SecInletTemp );
			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = ( QHX / StageEff ) / ( 2500000.0 * RhoWater );
			// A numerical check to keep from having very tiny negative water consumption values being reported
			if ( EvapCond( EvapCoolNum ).EvapWaterConsumpRate < 0.0 ) EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		} else {
			// The evap cooler is not running and does not change conditions from inlet to outlet
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;

			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;

			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;

			EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;

			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		}
		// all of the mass flowrates are not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletMassFlowRate = EvapCond( EvapCoolNum ).InletMassFlowRate;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail;

		// the pressure is not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletPressure = EvapCond( EvapCoolNum ).InletPressure;

	}

	void
	CalcResearchSpecialPartLoad( int & EvapCoolNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine models a "special" cooler.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// copied CalcWetIndirectEvapCooler as template for new cooler

		// Using/Aliasing
		using DataHVACGlobals::TempControlTol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		Real64 const MinAirMassFlow( 0.001 );
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		std::string CompName;
		Real64 FullOutput( 0.0 );
		Real64 ReqOutput( 0.0 );
		int InletNode;
		int OutletNode;
		int ControlNode;
		Real64 PartLoadFrac;
		Real64 DesOutTemp;
		// Set local variables
		// Retrieve the load on the controlled zone

		OutletNode = EvapCond( EvapCoolNum ).OutletNode;
		InletNode = EvapCond( EvapCoolNum ).InletNode;
		ControlNode = EvapCond( EvapCoolNum ).EvapControlNodeNum;
		DesOutTemp = EvapCond( EvapCoolNum ).DesiredOutletTemp;
		PartLoadFrac = 0.0;
		CompName = EvapCond( EvapCoolNum ).EvapCoolerName;

		// If Evap Cooler runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
		if ( ( GetCurrentScheduleValue( EvapCond( EvapCoolNum ).SchedPtr ) > 0.0 ) && ( Node( InletNode ).MassFlowRate > MinAirMassFlow ) && ( Node( InletNode ).Temp > Node( ControlNode ).TempSetPoint ) && ( std::abs( Node( InletNode ).Temp - DesOutTemp ) > TempControlTol ) ) {

			// Get full load result, depending on model
			EvapCond( EvapCoolNum ).PartLoadFract = 1.0;
			{ auto const SELECT_CASE_var( EvapCond( EvapCoolNum ).EvapCoolerType );
			if ( SELECT_CASE_var == iEvapCoolerInDirectRDDSpecial ) {
				CalcIndirectResearchSpecialEvapCooler( EvapCoolNum );
				UpdateEvapCooler( EvapCoolNum );
				FullOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

				ReqOutput = Node( InletNode ).MassFlowRate * ( PsyHFnTdbW( EvapCond( EvapCoolNum ).DesiredOutletTemp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

				// now reinit after test call
				InitEvapCooler( EvapCoolNum );

			} else if ( SELECT_CASE_var == iEvapCoolerDirectResearchSpecial ) {
				CalcDirectResearchSpecialEvapCooler( EvapCoolNum );
				UpdateEvapCooler( EvapCoolNum );
				FullOutput = Node( OutletNode ).Temp - Node( InletNode ).Temp;
				ReqOutput = EvapCond( EvapCoolNum ).DesiredOutletTemp - Node( InletNode ).Temp;

				// now reinit after test call
				InitEvapCooler( EvapCoolNum );

			} else {
				assert( false );
			}}

			// Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
			// Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
			// Calculate the part load fraction
			if ( FullOutput == 0.0 ) {
				FullOutput = 0.00001;
			}
			PartLoadFrac = ReqOutput / FullOutput;
			if ( PartLoadFrac > 1.0 ) {
				PartLoadFrac = 1.0;
			} else if ( PartLoadFrac < 0.0 ) {
				PartLoadFrac = 0.0;
			}

		} else { // No cooling
			PartLoadFrac = 0.0;

		} // End of the cooler running If block
		//Set the final results
		EvapCond( EvapCoolNum ).PartLoadFract = PartLoadFrac;

	}

	void
	CalcIndirectResearchSpecialEvapCooler( int const EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine models a "special" cooler that allows high effectiveness and controls

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// copied CalcWetIndirectEvapCooler as template for new cooler

		// Using/Aliasing
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutWetBulbTemp;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutBaroPress;
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//REAL(r64) Variables
		Real64 SecondaryInletWetBulbTemp; // entering wet bulb for secondary/purge side
		Real64 SecondaryInletDewPointTemp; // entering dewpoint for secondary/purge side
		Real64 StageEff; // Stage Efficiency of the Heat Exchanger
		Real64 TEDB; // Entering Dry Bulb Temperature
		Real64 TEWB; // Entering Wet Bulb Temperature
		Real64 QHX; // Q Across Sec HX in Watts or J/sec
		Real64 RhoWater;
		Real64 RhoAir; // Density of the primary side air
		Real64 CFMAir;
		int TertNode; // inlet node for relief (from bulding) to mix for purge
		Real64 BoundTemp; // temperature limit for outlet
		Real64 PartLoad;
		Real64 TotalVolFlow;
		Real64 TertMdot;
		Real64 TertHumRate;
		Real64 TertTemp;
		Real64 TertRho;
		Real64 TertVdot;
		Real64 SecVdot;
		Real64 SecRho;
		Real64 SecMdot;
		Real64 PurgeMdot;
		Real64 PurgeHumRat;
		Real64 PurgeEnthalpy;
		Real64 PurgeTemp;
		static Real64 BlowDownVdot( 0.0 );
		static Real64 DriftVdot( 0.0 );
		static Real64 EvapVdot( 0.0 );

		// If the Evaporative Cooler  is operating there should be some mass flow rate
		//  Also the evap cooler has to be scheduled to be available
		if ( ( EvapCond( EvapCoolNum ).InletMassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( EvapCond( EvapCoolNum ).SchedPtr ) > 0.0 ) ) {

			//******************************************************************************
			//   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
			//   FOR A WET COIL EVAPORATIVE COOLER
			//******************************************************************************
			//  INDIRECT STAGE EFFICIENCY FOR WET COIL INDIRECT EVAP COOLERS
			CFMAir = EvapCond( EvapCoolNum ).VolFlowRate; //Volume Flow Rate Primary Side
			//      CFMSec = EvapCond(EvapCoolNum)%IndirectVolFlowRate    !Volume Flolw Rate Secondary Side

			StageEff = EvapCond( EvapCoolNum ).WetCoilMaxEfficiency;

			// This is model is for special indirect cooler with efficiency greater than 1.0

			if ( StageEff >= 1.5 ) StageEff = 1.5;

			EvapCond( EvapCoolNum ).StageEff = StageEff;

			//***********************************************
			//  Unit is allowed to mix relief air that would otherwise be exhausted outdoors for ventilation
			//  If tertiary node is set >0 then it assumed that this node is the exhaust out of the building
			//  and the remainder will be made up with outside air from the secondary node
			//*********************************************

			TertNode = EvapCond( EvapCoolNum ).TertiaryInletNode;
			if ( TertNode == 0 ) {

				SecondaryInletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).SecInletTemp, EvapCond( EvapCoolNum ).SecInletHumRat, OutBaroPress );
				SecondaryInletDewPointTemp = PsyTdpFnTdbTwbPb( EvapCond( EvapCoolNum ).SecInletTemp, SecondaryInletWetBulbTemp, OutBaroPress );

			} else {

				TotalVolFlow = EvapCond( EvapCoolNum ).IndirectVolFlowRate;
				TertMdot = Node( TertNode ).MassFlowRate;
				TertHumRate = Node( TertNode ).HumRat;
				TertTemp = Node( TertNode ).Temp;
				// is Node pressure available or better? using outdoor pressure for now
				TertRho = PsyRhoAirFnPbTdbW( OutBaroPress, TertTemp, TertHumRate );
				TertVdot = TertMdot / TertRho;

				SecVdot = TotalVolFlow - TertVdot;

				if ( SecVdot < 0.0 ) { // all tertiary/releif air e.g. econonizer wide open
					SecVdot = 0.0;
					SecondaryInletWetBulbTemp = PsyTwbFnTdbWPb( TertTemp, TertHumRate, OutBaroPress );
					SecondaryInletDewPointTemp = PsyTdpFnTdbTwbPb( TertTemp, SecondaryInletWetBulbTemp, OutBaroPress );

				} else {

					// First determine mass flow of OA,  in secondary
					SecRho = PsyRhoAirFnPbTdbW( OutBaroPress, EvapCond( EvapCoolNum ).SecInletTemp, EvapCond( EvapCoolNum ).SecInletHumRat );
					SecMdot = SecRho * SecVdot;
					// Mass balance on moisture to get outlet air humidity ratio
					// this mixing takes place before wet media.
					PurgeMdot = SecMdot + TertMdot;
					PurgeHumRat = ( SecMdot * EvapCond( EvapCoolNum ).SecInletHumRat + TertMdot * TertHumRate ) / PurgeMdot;

					// Energy balance to get outlet air enthalpy

					PurgeEnthalpy = ( SecMdot * PsyHFnTdbW( EvapCond( EvapCoolNum ).SecInletTemp, EvapCond( EvapCoolNum ).SecInletHumRat ) + TertMdot * PsyHFnTdbW( TertTemp, TertHumRate ) ) / PurgeMdot;

					// Use Enthalpy and humidity ratio to get outlet temperature from psych chart

					PurgeTemp = PsyTdbFnHW( PurgeEnthalpy, PurgeHumRat );
					SecondaryInletWetBulbTemp = PsyTwbFnTdbWPb( PurgeTemp, PurgeHumRat, OutBaroPress );
					SecondaryInletDewPointTemp = PsyTdpFnTdbTwbPb( PurgeTemp, SecondaryInletWetBulbTemp, OutBaroPress );
				}
			}
			//***************************************************************************
			//   TEMP LEAVING DRY BULB IS CALCULATED FROM A SIMPLE WET BULB APPROACH
			//   MODEL GIVEN THE INDIRECT STAGE EFFICIENCY.
			//   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE INDIRECT STAGE.
			//***************************************************************************
			TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
			TEDB = EvapCond( EvapCoolNum ).InletTemp;
			PartLoad = EvapCond( EvapCoolNum ).PartLoadFract;

			if ( PartLoad == 1.0 ) {
				//                                 Tout = Tin -  (   0.7    (Tin  - Tpurge,wb,in)
				EvapCond( EvapCoolNum ).OutletTemp = TEDB - StageEff * ( TEDB - SecondaryInletWetBulbTemp );
				//  now bound with secondary dewpoint.
				// unless the resulting Tout<=Tpurge,dp,in ; in which case Tout = Tin - 0.9(Tin-Tpurge,dp,in)

				BoundTemp = TEDB - EvapCond( EvapCoolNum ).DPBoundFactor * ( TEDB - SecondaryInletDewPointTemp );
				if ( EvapCond( EvapCoolNum ).OutletTemp < BoundTemp ) {
					EvapCond( EvapCoolNum ).OutletTemp = BoundTemp;
					EvapCond( EvapCoolNum ).DewPointBoundFlag = 1;
				}
			} else if ( ( PartLoad < 1.0 ) && ( PartLoad > 0.0 ) ) {
				// assume perfect control Use PLF for energy consumption
				if ( EvapCond( EvapCoolNum ).DesiredOutletTemp < TEDB ) {
					EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).DesiredOutletTemp;
				}
			} else {
				//part load set to zero so no cooling
				EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
			}

			//***************************************************************************
			//                  CALCULATE THE WET BULB TEMP in the primary system air using PSYCH ROUTINES
			// There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
			EvapCond( EvapCoolNum ).OuletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).InletHumRat, OutBaroPress );
			//***************************************************************************
			//                  CALCULATE other outlet propertiesusing PSYCH ROUTINES
			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );

			//***************************************************************************
			//                  POWER OF THE SECONDARY AIR FAN with part load factor applied (assumes const efficiency)
			if ( EvapCond( EvapCoolNum ).IndirectFanEff > 0.0 ) {
				EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectFanDeltaPress * EvapCond( EvapCoolNum ).IndirectVolFlowRate / EvapCond( EvapCoolNum ).IndirectFanEff * PartLoad;
			}
			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//Add the pump energy to the total Evap Cooler energy comsumption
			EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectRecircPumpPower * PartLoad;

			//******************
			//             WATER CONSUMPTION IN LITERS OF WATER FOR Wet InDIRECT
			//             H2O [m3/sec] = (QHX [J/s])/(2,500,000 [J/kg H2O] * RhoWater [kg H2O/m3 H2O])
			//******************
			//***** FIRST calculate the heat exchange on the primary air side**********
			RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, EvapCond( EvapCoolNum ).InletTemp, EvapCond( EvapCoolNum ).InletHumRat );
			QHX = CFMAir * RhoAir * ( EvapCond( EvapCoolNum ).InletEnthalpy - EvapCond( EvapCoolNum ).OutletEnthalpy );

			RhoWater = RhoH2O( OutDryBulbTemp );
			EvapVdot = ( QHX ) / ( 2500000.0 * RhoWater );
			DriftVdot = EvapVdot * EvapCond( EvapCoolNum ).DriftFraction;
			if ( EvapCond( EvapCoolNum ).BlowDownRatio > 0.0 ) {
				BlowDownVdot = EvapVdot / ( EvapCond( EvapCoolNum ).BlowDownRatio - 1 ) - DriftVdot;
				if ( BlowDownVdot < 0.0 ) BlowDownVdot = 0.0;
			} else {
				BlowDownVdot = 0.0;
			}
			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = EvapVdot + DriftVdot + BlowDownVdot;
			// A numerical check to keep from having very tiny negative water consumption values being reported
			if ( EvapCond( EvapCoolNum ).EvapWaterConsumpRate < 0.0 ) EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		} else {
			// The evap cooler is not running and does not change conditions from inlet to outlet
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;

			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;

			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;

			EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;
			EvapCond( EvapCoolNum ).EvapCoolerPower = 0.0;
			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		}

		// all of the mass flowrates are not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletMassFlowRate = EvapCond( EvapCoolNum ).InletMassFlowRate;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail;

		// the pressure is not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletPressure = EvapCond( EvapCoolNum ).InletPressure;

	}

	void
	CalcDirectResearchSpecialEvapCooler( int const EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate model for direct evaporative cooler that is simple and controllable

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SatEff; // Saturation Efficiency of the CelDek Pad
		Real64 TEDB; // Entering Dry Bulb Temperature
		Real64 TEWB; // Entering Wet Bulb Temperature
		Real64 RhoWater;
		Real64 PartLoad;
		static Real64 BlowDownVdot( 0.0 );
		static Real64 DriftVdot( 0.0 );
		static Real64 EvapVdot( 0.0 );

		// If the Evaporative Cooler  is operating there should be some mass flow rate
		//  Also the evap cooler has to be scheduled to be available
		if ( ( EvapCond( EvapCoolNum ).InletMassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( EvapCond( EvapCoolNum ).SchedPtr ) > 0.0 ) ) {

			//***************************************************************************
			//   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
			//   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
			//   ACROSS A DIRECT EVAPORATION COOLER.
			TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
			TEDB = EvapCond( EvapCoolNum ).InletTemp;
			SatEff = EvapCond( EvapCoolNum ).DirectEffectiveness;
			PartLoad = EvapCond( EvapCoolNum ).PartLoadFract;
			if ( PartLoad == 1.0 ) {
				EvapCond( EvapCoolNum ).OutletTemp = TEDB - ( ( TEDB - TEWB ) * SatEff );
				EvapCond( EvapCoolNum ).OuletWetBulbTemp = TEWB;
				EvapCond( EvapCoolNum ).OutletHumRat = PsyWFnTdbTwbPb( EvapCond( EvapCoolNum ).OutletTemp, TEWB, OutBaroPress );
				EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );
			} else if ( ( PartLoad < 1.0 ) && ( PartLoad > 0.0 ) ) {
				// assume perfect control Use PLF for energy consumption
				if ( EvapCond( EvapCoolNum ).DesiredOutletTemp < TEDB ) {
					EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).DesiredOutletTemp;
					EvapCond( EvapCoolNum ).OuletWetBulbTemp = TEWB;
					EvapCond( EvapCoolNum ).OutletHumRat = PsyWFnTdbTwbPb( EvapCond( EvapCoolNum ).OutletTemp, TEWB, OutBaroPress );

					EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );
				} else { //do no cooling
					EvapCond( EvapCoolNum ).OutletTemp = TEDB;
					EvapCond( EvapCoolNum ).OuletWetBulbTemp = TEWB;
					EvapCond( EvapCoolNum ).OutletHumRat = PsyWFnTdbTwbPb( EvapCond( EvapCoolNum ).OutletTemp, TEWB, OutBaroPress );
					EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );
				}
			} else {
				//part load set to zero so no cooling
				EvapCond( EvapCoolNum ).OutletTemp = TEDB;
				EvapCond( EvapCoolNum ).OuletWetBulbTemp = TEWB;
				EvapCond( EvapCoolNum ).OutletHumRat = PsyWFnTdbTwbPb( EvapCond( EvapCoolNum ).OutletTemp, TEWB, OutBaroPress );
				EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );
			}

			//***************************************************************************
			//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
			//Add the pump energy to the total Evap Cooler energy comsumption
			EvapCond( EvapCoolNum ).EvapCoolerPower = EvapCond( EvapCoolNum ).RecircPumpPower * PartLoad;

			//******************
			//             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
			//             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
			//                                /RhoWater [kg H2O/m3 H2O]
			//******************
			RhoWater = RhoH2O( EvapCond( EvapCoolNum ).OutletTemp );
			EvapVdot = ( EvapCond( EvapCoolNum ).OutletHumRat - EvapCond( EvapCoolNum ).InletHumRat ) * EvapCond( EvapCoolNum ).InletMassFlowRate / RhoWater;
			DriftVdot = EvapVdot * EvapCond( EvapCoolNum ).DriftFraction;

			if ( EvapCond( EvapCoolNum ).BlowDownRatio > 0.0 ) {
				BlowDownVdot = EvapVdot / ( EvapCond( EvapCoolNum ).BlowDownRatio - 1.0 ) - DriftVdot;
				if ( BlowDownVdot < 0.0 ) BlowDownVdot = 0.0;
			} else {
				BlowDownVdot = 0.0;
			}

			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = EvapVdot + DriftVdot + BlowDownVdot;

			// A numerical check to keep from having very tiny negative water consumption values being reported
			if ( EvapCond( EvapCoolNum ).EvapWaterConsumpRate < 0.0 ) EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		} else {
			// The evap cooler is not running and does not change conditions from inlet to outlet
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;

			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;

			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

			EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;
			EvapCond( EvapCoolNum ).EvapCoolerPower = 0.0;
			EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;

			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;

		}
		// all of the mass flowrates are not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletMassFlowRate = EvapCond( EvapCoolNum ).InletMassFlowRate;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail;

		// the pressure is not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletPressure = EvapCond( EvapCoolNum ).InletPressure;

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the EvapCooler Module
	// *****************************************************************************

	void
	UpdateEvapCooler( int const EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   October 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataWater;
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
		static Real64 AvailWaterRate( 0.0 );

		OutletNode = EvapCond( EvapCoolNum ).OutletNode;
		InletNode = EvapCond( EvapCoolNum ).InletNode;

		// Set the outlet air nodes of the EvapCooler
		Node( OutletNode ).MassFlowRate = EvapCond( EvapCoolNum ).OutletMassFlowRate;
		Node( OutletNode ).MassFlowRateMaxAvail = EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail;
		Node( OutletNode ).MassFlowRateMinAvail = EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail;
		Node( OutletNode ).Temp = EvapCond( EvapCoolNum ).OutletTemp;
		Node( OutletNode ).HumRat = EvapCond( EvapCoolNum ).OutletHumRat;
		Node( OutletNode ).Enthalpy = EvapCond( EvapCoolNum ).OutletEnthalpy;
		Node( OutletNode ).Press = EvapCond( EvapCoolNum ).OutletPressure;
		// Set the outlet nodes for properties that just pass through & not used
		Node( OutletNode ).Quality = Node( InletNode ).Quality;

		// Set the demand request for supply water from water storage tank (if needed)
		if ( EvapCond( EvapCoolNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
			WaterStorage( EvapCond( EvapCoolNum ).EvapWaterSupTankID ).VdotRequestDemand( EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID ) = EvapCond( EvapCoolNum ).EvapWaterConsumpRate;
		}

		//check if should be starved by restricted flow from tank
		if ( EvapCond( EvapCoolNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
			AvailWaterRate = WaterStorage( EvapCond( EvapCoolNum ).EvapWaterSupTankID ).VdotAvailDemand( EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );
			if ( AvailWaterRate < EvapCond( EvapCoolNum ).EvapWaterConsumpRate ) {
				EvapCond( EvapCoolNum ).EvapWaterStarvMakupRate = EvapCond( EvapCoolNum ).EvapWaterConsumpRate - AvailWaterRate;
				EvapCond( EvapCoolNum ).EvapWaterConsumpRate = AvailWaterRate;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterStarvMakupRate = 0.0;
			}
		}

		if ( Contaminant.CO2Simulation ) {
			Node( OutletNode ).CO2 = Node( InletNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( OutletNode ).GenContam = Node( InletNode ).GenContam;
		}

	}

	//        End of Update subroutines for the EvapCooler Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the EvapCooler Module
	// *****************************************************************************

	void
	ReportEvapCooler( int const EvapCoolNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

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
		// na
		// report the Evap Cooler energy from this component
		EvapCond( EvapCoolNum ).EvapCoolerPower = EvapCond( EvapCoolNum ).EvapCoolerPower;
		EvapCond( EvapCoolNum ).EvapCoolerEnergy = EvapCond( EvapCoolNum ).EvapCoolerPower * TimeStepSys * SecInHour;

		// Report Water comsumption in cubic meters per timestep
		EvapCond( EvapCoolNum ).EvapWaterConsump = EvapCond( EvapCoolNum ).EvapWaterConsumpRate * TimeStepSys * SecInHour;
		EvapCond( EvapCoolNum ).EvapWaterStarvMakup = EvapCond( EvapCoolNum ).EvapWaterStarvMakupRate * TimeStepSys * SecInHour;

	}

	//***************
	//Begin routines for zone HVAC Evaporative cooler unit
	//_______________________________________________________________________________________________________________________
	//***************

	void
	SimZoneEvaporativeCoolerUnit(
		std::string const & CompName, // name of the packaged terminal heat pump
		int const ZoneNum, // number of zone being served
		Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
		Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
		int & CompIndex // index to zone hvac unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// public simulation routine for managing zone hvac evaporative cooler unit

		// METHODOLOGY EMPLOYED:
		// <description>

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
		int CompNum;

		if ( GetInputZoneEvapUnit ) {
			GetInputZoneEvaporativeCoolerUnit();
			GetInputZoneEvapUnit = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			CompNum = FindItemInList( CompName, ZoneEvapUnit.Name(), NumZoneEvapUnits );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimZoneEvaporativeCoolerUnit: Zone evaporative cooler unit not found." );
			}
			CompIndex = CompNum;
		} else {
			CompNum = CompIndex;
			if ( CompNum < 1 || CompNum > NumZoneEvapUnits ) {
				ShowFatalError( "SimZoneEvaporativeCoolerUnit: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of units =" + TrimSigDigits( NumZoneEvapUnits ) + ", Entered Unit name = " + CompName );
			}
			if ( CheckZoneEvapUnitName( CompNum ) ) {
				if ( CompName != ZoneEvapUnit( CompNum ).Name ) {
					ShowFatalError( "SimZoneEvaporativeCoolerUnit: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Unit name=" + CompName + ", stored unit name for that index=" + ZoneEvapUnit( CompNum ).Name );
				}
				CheckZoneEvapUnitName( CompNum ) = false;
			}
		}

		InitZoneEvaporativeCoolerUnit( CompNum, ZoneNum );

		CalcZoneEvaporativeCoolerUnit( CompNum, ZoneNum, SensibleOutputProvided, LatentOutputProvided );

		ReportZoneEvaporativeCoolerUnit( CompNum );

	}

	void
	GetInputZoneEvaporativeCoolerUnit()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get input for zone evap cooler unit

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::GetFanType;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using Fans::GetFanAvailSchPtr;
		using General::TrimSigDigits;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataZoneEquipment::ZoneEvaporativeCoolerUnit_Num;
		using BranchNodeConnections::SetUpCompSets;
		using DataSizing::NumZoneHVACSizing;
		using DataSizing::ZoneHVACSizing;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataGlobals::NumOfZones;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetInputZoneEvaporativeCoolerUnit: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CurrentModuleObject; // Object type for getting and error messages
		FArray1D_string Alphas; // Alpha items for object
		FArray1D< Real64 > Numbers; // Numeric items for object
		FArray1D_string cAlphaFields; // Alpha field names
		FArray1D_string cNumericFields; // Numeric field names
		FArray1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		FArray1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int MaxAlphas; // Maximum number of alpha fields in all objects
		int MaxNumbers; // Maximum number of numeric fields in all objects
		int NumFields; // Total number of fields in object
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		Real64 FanVolFlow;
		int UnitLoop;
		int CtrlZone; // index to loop counter
		int NodeNum; // index to loop counter

		if ( GetInputEvapComponentsFlag ) {
			GetEvapInput();
			GetInputEvapComponentsFlag = false;
		}

		MaxNumbers = 0;
		MaxAlphas = 0;

		CurrentModuleObject = "ZoneHVAC:EvaporativeCoolerUnit";
		NumZoneEvapUnits = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		Alphas.allocate( MaxAlphas );
		Numbers.dimension( MaxNumbers, 0.0 );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNumbers );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNumbers, true );

		if ( NumZoneEvapUnits > 0 ) {
			CheckZoneEvapUnitName.dimension( NumZoneEvapUnits, true );
			ZoneEvapUnit.allocate( NumZoneEvapUnits );
			ZoneEvapCoolerUnitFields.allocate( NumZoneEvapUnits );

			for ( UnitLoop = 1; UnitLoop <= NumZoneEvapUnits; ++UnitLoop ) {
				GetObjectItem( CurrentModuleObject, UnitLoop, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				ZoneEvapCoolerUnitFields( UnitLoop ).FieldNames.allocate( NumNumbers );
				ZoneEvapCoolerUnitFields( UnitLoop ).FieldNames = "";
				ZoneEvapCoolerUnitFields( UnitLoop ).FieldNames = cNumericFields;

				IsNotOK = false;
				IsBlank = false;
				VerifyName( Alphas( 1 ), ZoneEvapUnit.Name(), UnitLoop - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
				}
				ZoneEvapUnit( UnitLoop ).Name = Alphas( 1 );
				if ( lAlphaBlanks( 2 ) ) {
					ZoneEvapUnit( UnitLoop ).AvailSchedIndex = ScheduleAlwaysOn;
				} else {
					ZoneEvapUnit( UnitLoop ).AvailSchedIndex = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer (index number)
					if ( ZoneEvapUnit( UnitLoop ).AvailSchedIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
						ShowContinueError( "invalid-not found " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaBlanks( 3 ) ) {
					ZoneEvapUnit( UnitLoop ).AvailManagerListName = Alphas( 3 );
				}

				ZoneEvapUnit( UnitLoop ).OAInletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_OutsideAir, 1, ObjectIsParent );

				ZoneEvapUnit( UnitLoop ).UnitOutletNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

				if ( ! lAlphaBlanks( 6 ) ) {
					ZoneEvapUnit( UnitLoop ).UnitReliefNodeNum = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
				}

				ZoneEvapUnit( UnitLoop ).FanObjectClassName = Alphas( 7 );
				ZoneEvapUnit( UnitLoop ).FanName = Alphas( 8 );
				errFlag = false;
				GetFanType( ZoneEvapUnit( UnitLoop ).FanName, ZoneEvapUnit( UnitLoop ).FanType_Num, errFlag, CurrentModuleObject, ZoneEvapUnit( UnitLoop ).Name );
				FanVolFlow = 0.0;
				if ( errFlag ) {
					ShowContinueError( "specified in " + CurrentModuleObject + " = " + ZoneEvapUnit( UnitLoop ).Name );
					ErrorsFound = true;
				} else {
					GetFanIndex( ZoneEvapUnit( UnitLoop ).FanName, ZoneEvapUnit( UnitLoop ).FanIndex, errFlag, CurrentModuleObject );
					ZoneEvapUnit( UnitLoop ).FanInletNodeNum = GetFanInletNode( ZoneEvapUnit( UnitLoop ).FanObjectClassName, ZoneEvapUnit( UnitLoop ).FanName, errFlag );
					ZoneEvapUnit( UnitLoop ).FanOutletNodeNum = GetFanOutletNode( ZoneEvapUnit( UnitLoop ).FanObjectClassName, ZoneEvapUnit( UnitLoop ).FanName, errFlag );
					GetFanVolFlow( ZoneEvapUnit( UnitLoop ).FanIndex, FanVolFlow );
					ZoneEvapUnit( UnitLoop ).ActualFanVolFlowRate = FanVolFlow;
					// Get the fan's availability schedule
					ZoneEvapUnit( UnitLoop ).FanAvailSchedPtr = GetFanAvailSchPtr( ZoneEvapUnit( UnitLoop ).FanObjectClassName, ZoneEvapUnit( UnitLoop ).FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "...specified in " + CurrentModuleObject + " = " + ZoneEvapUnit( UnitLoop ).Name );
						ErrorsFound = true;
					}
				}

				ZoneEvapUnit( UnitLoop ).DesignAirVolumeFlowRate = Numbers( 1 );

				{ auto const SELECT_CASE_var( Alphas( 9 ) );
				if ( SELECT_CASE_var == "BLOWTHROUGH" ) {
					ZoneEvapUnit( UnitLoop ).FanLocation = BlowThruFan;
				} else if ( SELECT_CASE_var == "DRAWTHROUGH" ) {
					ZoneEvapUnit( UnitLoop ).FanLocation = DrawThruFan;
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
					ShowContinueError( "invalid choice found " + cAlphaFields( 9 ) + "=\"" + Alphas( 9 ) + "\"." );
					ErrorsFound = true;
				}}

				// get the zone numer served by the zoneHVAC evaporative cooler
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( ZoneEvapUnit( UnitLoop ).UnitOutletNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							ZoneEvapUnit( UnitLoop ).ZonePtr = CtrlZone;
							break;
						}
					}
				}

				{ auto const SELECT_CASE_var( Alphas( 10 ) );
				if ( SELECT_CASE_var == "ZONETEMPERATUREDEADBANDONOFFCYCLING" ) {
					ZoneEvapUnit( UnitLoop ).ControlSchemeType = ZoneTemperatureDeadBandOnOffCycling;
				} else if ( SELECT_CASE_var == "ZONECOOLINGLOADONOFFCYCLING" ) {
					ZoneEvapUnit( UnitLoop ).ControlSchemeType = ZoneCoolingLoadOnOffCycling;
				} else if ( SELECT_CASE_var == "ZONECOOLINGLOADVARIABLESPEEDFAN" ) {
					ZoneEvapUnit( UnitLoop ).ControlSchemeType = ZoneCoolingLoadVariableSpeedFan;
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
					ShowContinueError( "invalid choice found " + cAlphaFields( 10 ) + "=\"" + Alphas( 10 ) + "\"." );
					ErrorsFound = true;
				}}

				ZoneEvapUnit( UnitLoop ).ThrottlingRange = Numbers( 2 );
				ZoneEvapUnit( UnitLoop ).ThresholdCoolingLoad = Numbers( 3 );

				{ auto const SELECT_CASE_var( Alphas( 11 ) );

				if ( SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:CELDEKPAD" ) {
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_ObjectClassName = "EvaporativeCooler:Direct:CelDekPad";
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_Type_Num = iEvapCoolerDirectCELDEKPAD;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL" ) {
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_ObjectClassName = "EvaporativeCooler:Direct:ResearchSpecial";
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_Type_Num = iEvapCoolerDirectResearchSpecial;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD" ) {
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_ObjectClassName = "EvaporativeCooler:Indirect:CelDekPad";
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_Type_Num = iEvapCoolerInDirectCELDEKPAD;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:WETCOIL" ) {
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_ObjectClassName = "EvaporativeCooler:Indirect:WetCoil";
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_Type_Num = iEvapCoolerInDirectWETCOIL;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL" ) {
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_ObjectClassName = "EvaporativeCooler:Indirect:ResearchSpecial";
					ZoneEvapUnit( UnitLoop ).EvapCooler_1_Type_Num = iEvapCoolerInDirectRDDSpecial;
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
					ShowContinueError( "invalid choice found " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
					ErrorsFound = true;
				}}

				ZoneEvapUnit( UnitLoop ).EvapCooler_1_Name = Alphas( 12 );
				ZoneEvapUnit( UnitLoop ).EvapCooler_1_Index = FindItemInList( Alphas( 12 ), EvapCond.EvapCoolerName(), NumEvapCool );
				if ( ZoneEvapUnit( UnitLoop ).EvapCooler_1_Index == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
					ShowContinueError( "invalid, not found " + cAlphaFields( 12 ) + "=\"" + Alphas( 12 ) + "\"." );
					ErrorsFound = true;
				}

				if ( ! lAlphaBlanks( 13 ) ) {
					{ auto const SELECT_CASE_var( Alphas( 13 ) );

					if ( SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:CELDEKPAD" ) {
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_ObjectClassName = "EvaporativeCooler:Direct:CelDekPad";
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Type_Num = iEvapCoolerDirectCELDEKPAD;
					} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL" ) {
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_ObjectClassName = "EvaporativeCooler:Direct:ResearchSpecial";
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Type_Num = iEvapCoolerDirectResearchSpecial;
					} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD" ) {
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_ObjectClassName = "EvaporativeCooler:Indirect:CelDekPad";
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Type_Num = iEvapCoolerInDirectCELDEKPAD;
					} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:WETCOIL" ) {
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_ObjectClassName = "EvaporativeCooler:Indirect:WetCoil";
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Type_Num = iEvapCoolerInDirectWETCOIL;
					} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL" ) {
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_ObjectClassName = "EvaporativeCooler:Indirect:ResearchSpecial";
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Type_Num = iEvapCoolerInDirectRDDSpecial;
					} else {
						ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
						ShowContinueError( "invalid choice found " + cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\"." );
						ErrorsFound = true;
					}}
					if ( ! lAlphaBlanks( 14 ) ) {
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Name = Alphas( 14 );
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Index = FindItemInList( Alphas( 14 ), EvapCond.EvapCoolerName(), NumEvapCool );
						if ( ZoneEvapUnit( UnitLoop ).EvapCooler_2_Index == 0 ) {
							ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
							ShowContinueError( "invalid, not found " + cAlphaFields( 14 ) + "=\"" + Alphas( 14 ) + "\"." );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
						ShowContinueError( "missing input for " + cAlphaFields( 14 ) );
						ErrorsFound = true;
					}
				}

				ZoneEvapUnit( UnitLoop ).HVACSizingIndex = 0;
				if ( !lAlphaBlanks( 15 ) ) {
					ZoneEvapUnit( UnitLoop ).HVACSizingIndex = FindItemInList(Alphas( 15 ), ZoneHVACSizing.Name(), NumZoneHVACSizing );
					if ( ZoneEvapUnit( UnitLoop ).HVACSizingIndex == 0 ) {
						ShowSevereError( cAlphaFields( 15 ) + " = " + Alphas( 15 ) + " not found." );
						ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneEvapUnit( UnitLoop ).Name );
						ErrorsFound = true;
					}
				}

				//Add fan to component sets array
				SetUpCompSets( CurrentModuleObject, ZoneEvapUnit( UnitLoop ).Name, ZoneEvapUnit( UnitLoop ).FanObjectClassName, ZoneEvapUnit( UnitLoop ).FanName, NodeID( ZoneEvapUnit( UnitLoop ).FanInletNodeNum ), NodeID( ZoneEvapUnit( UnitLoop ).FanOutletNodeNum ) );

				//Add first evap cooler to component sets array
				SetUpCompSets( CurrentModuleObject, ZoneEvapUnit( UnitLoop ).Name, ZoneEvapUnit( UnitLoop ).EvapCooler_1_ObjectClassName, ZoneEvapUnit( UnitLoop ).EvapCooler_1_Name, NodeID( EvapCond( ZoneEvapUnit( UnitLoop ).EvapCooler_1_Index ).InletNode ), NodeID( EvapCond( ZoneEvapUnit( UnitLoop ).EvapCooler_1_Index ).OutletNode ) );

				if ( ZoneEvapUnit( UnitLoop ).EvapCooler_2_Index > 0 ) {
					//Add second evap cooler to component sets array
					SetUpCompSets( CurrentModuleObject, ZoneEvapUnit( UnitLoop ).Name, ZoneEvapUnit( UnitLoop ).EvapCooler_2_ObjectClassName, ZoneEvapUnit( UnitLoop ).EvapCooler_2_Name, NodeID( EvapCond( ZoneEvapUnit( UnitLoop ).EvapCooler_2_Index ).InletNode ), NodeID( EvapCond( ZoneEvapUnit( UnitLoop ).EvapCooler_2_Index ).OutletNode ) );

				}

				// check that fan type is consistent with control method
				if ( ZoneEvapUnit( UnitLoop ).ControlSchemeType == ZoneCoolingLoadVariableSpeedFan ) { // must have a VS fan type
					if ( ZoneEvapUnit( UnitLoop ).FanType_Num == FanType_SimpleConstVolume ) {
						ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
						ShowContinueError( "Fan:ConstantVolume is not consistent with control method ZoneCoolingLoadVariableSpeedFan." );
						ShowContinueError( "Change to a variable speed fan object type" );
						ErrorsFound = true;
					} else if ( ZoneEvapUnit( UnitLoop ).FanType_Num == FanType_SimpleOnOff ) {
						ShowSevereError( CurrentModuleObject + "=\"" + ZoneEvapUnit( UnitLoop ).Name + "\" invalid data." );
						ShowContinueError( "Fan:OnOff is not consistent with control method ZoneCoolingLoadVariableSpeedFan." );
						ShowContinueError( "Change to a variable speed fan object type" );
						ErrorsFound = true;
					}

				}

			} // unit loop

		}

		//***********************************************************************************

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input." );
			ShowContinueError( "... Preceding condition causes termination." );
		}

		// setup output variables
		for ( UnitLoop = 1; UnitLoop <= NumZoneEvapUnits; ++UnitLoop ) {

			SetupOutputVariable( "Zone Evaporative Cooler Unit Total Cooling Rate [W]", ZoneEvapUnit( UnitLoop ).UnitTotalCoolingRate, "System", "Average", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Total Cooling Energy [J]", ZoneEvapUnit( UnitLoop ).UnitTotalCoolingEnergy, "System", "Sum", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Sensible Cooling Rate [W]", ZoneEvapUnit( UnitLoop ).UnitSensibleCoolingRate, "System", "Average", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Sensible Cooling Energy [J]", ZoneEvapUnit( UnitLoop ).UnitSensibleCoolingEnergy, "System", "Sum", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Latent Heating Rate [W]", ZoneEvapUnit( UnitLoop ).UnitLatentHeatingRate, "System", "Average", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Latent Heating Energy [J]", ZoneEvapUnit( UnitLoop ).UnitLatentHeatingEnergy, "System", "Sum", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Latent Cooling Rate [W]", ZoneEvapUnit( UnitLoop ).UnitLatentCoolingRate, "System", "Average", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Latent Cooling Energy [J]", ZoneEvapUnit( UnitLoop ).UnitLatentCoolingEnergy, "System", "Sum", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Fan Speed Ratio []", ZoneEvapUnit( UnitLoop ).UnitFanSpeedRatio, "System", "Average", ZoneEvapUnit( UnitLoop ).Name );
			SetupOutputVariable( "Zone Evaporative Cooler Unit Fan Availability Status []", ZoneEvapUnit( UnitLoop ).FanAvailStatus, "System", "Average", ZoneEvapUnit( UnitLoop ).Name );
		}

	}

	void
	InitZoneEvaporativeCoolerUnit(
		int const UnitNum, // unit number
		int const ZoneNum // number of zone being served
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::WarmupFlag;
		using DataGlobals::HourOfDay;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::ZoneEvaporativeCoolerUnit_Num;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::SysTimeElapsed;
		using DataSizing::AutoSize;
		using DataEnvironment::StdRhoAir;
		using Fans::GetFanVolFlow;

		// Locals
		static FArray1D_bool MySizeFlag;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // one time flag
		static FArray1D_bool MyEnvrnFlag;
		static FArray1D_bool MyFanFlag;
		static FArray1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		bool errFlag;
		int Loop;
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		Real64 TimeElapsed;

		if ( MyOneTimeFlag ) {
			MySizeFlag.dimension( NumZoneEvapUnits, true );
			MyEnvrnFlag.dimension( NumZoneEvapUnits, true );
			MyFanFlag.dimension( NumZoneEvapUnits, true );
			MyZoneEqFlag.allocate ( NumZoneEvapUnits );
			MyZoneEqFlag = true;
			MyOneTimeFlag = false;
		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( UnitNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( ZoneEvaporativeCoolerUnit_Num ).ZoneCompAvailMgrs( UnitNum ).AvailManagerListName = ZoneEvapUnit( UnitNum ).AvailManagerListName;
				ZoneComp( ZoneEvaporativeCoolerUnit_Num ).ZoneCompAvailMgrs( UnitNum ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( UnitNum ) = false;
			}
			ZoneEvapUnit( UnitNum ).FanAvailStatus = ZoneComp( ZoneEvaporativeCoolerUnit_Num ).ZoneCompAvailMgrs( UnitNum ).AvailStatus;
		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumZoneEvapUnits; ++Loop ) {
				if ( CheckZoneEquipmentList( "ZoneHVAC:EvaporativeCoolerUnit", ZoneEvapUnit( Loop ).Name ) ) {
					ZoneEvapUnit( Loop ).ZoneNodeNum = ZoneEquipConfig( ZoneNum ).ZoneNode;
				} else {
					ShowSevereError( "InitZoneEvaporativeCoolerUnit: ZoneHVAC:EvaporativeCoolerUnit = " + ZoneEvapUnit( Loop ).Name + ", is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
				}
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( UnitNum ) ) {
			SizeZoneEvaporativeCoolerUnit( UnitNum );
			MySizeFlag( UnitNum ) = false;
		}

		if ( MyFanFlag( UnitNum ) ) {
			if ( ZoneEvapUnit( UnitNum ).ActualFanVolFlowRate != AutoSize ) {
				if ( ZoneEvapUnit( UnitNum ).ActualFanVolFlowRate > 0.0 ) {
					ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio = ZoneEvapUnit( UnitNum ).DesignAirVolumeFlowRate / ZoneEvapUnit( UnitNum ).ActualFanVolFlowRate;
				}
				MyFanFlag( UnitNum ) = false;
			} else {
				GetFanVolFlow( ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).ActualFanVolFlowRate );
			}
		}

		if ( ZoneEvapUnit( UnitNum ).FanAvailSchedPtr > 0 ) {
			// include fan is not available, then unit is not available
			if ( ( GetCurrentScheduleValue( ZoneEvapUnit( UnitNum ).FanAvailSchedPtr ) > 0.0 ) && ( GetCurrentScheduleValue( ZoneEvapUnit( UnitNum ).AvailSchedIndex ) > 0.0 ) ) {
				ZoneEvapUnit( UnitNum ).UnitIsAvailable = true;
			} else {
				ZoneEvapUnit( UnitNum ).UnitIsAvailable = false;
			}
		} else {
			if ( GetCurrentScheduleValue( ZoneEvapUnit( UnitNum ).AvailSchedIndex ) > 0.0 ) {
				ZoneEvapUnit( UnitNum ).UnitIsAvailable = true;
			} else {
				ZoneEvapUnit( UnitNum ).UnitIsAvailable = false;
			}
		}

		if ( GetCurrentScheduleValue( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).SchedPtr ) > 0.0 ) {
			ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus = true;
		} else {
			ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus = false;
		}

		if ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) {
			if ( GetCurrentScheduleValue( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).SchedPtr ) > 0.0 ) {
				ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus = true;
			} else {
				ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus = false;
			}
		}
		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( UnitNum ) ) {

			ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate = StdRhoAir * ZoneEvapUnit( UnitNum ).DesignAirVolumeFlowRate;
			Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMax = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate;
			Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMin = 0.0;
			Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMinAvail = 0.0;

			Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMax = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate;
			Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMin = 0.0;
			Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMinAvail = 0.0;

			if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
				Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMax = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate;
				Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMin = 0.0;
				Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMinAvail = 0.0;
			}
			ZoneEvapUnit( UnitNum ).WasOnLastTimestep = false;
			ZoneEvapUnit( UnitNum ).IsOnThisTimestep = false;
			ZoneEvapUnit( UnitNum ).FanSpeedRatio = 0.0;
			ZoneEvapUnit( UnitNum ).UnitFanSpeedRatio = 0.0;
			ZoneEvapUnit( UnitNum ).UnitTotalCoolingRate = 0.0;
			ZoneEvapUnit( UnitNum ).UnitTotalCoolingEnergy = 0.0;
			ZoneEvapUnit( UnitNum ).UnitSensibleCoolingRate = 0.0;
			ZoneEvapUnit( UnitNum ).UnitSensibleCoolingEnergy = 0.0;
			ZoneEvapUnit( UnitNum ).UnitLatentHeatingRate = 0.0;
			ZoneEvapUnit( UnitNum ).UnitLatentHeatingEnergy = 0.0;
			ZoneEvapUnit( UnitNum ).UnitLatentCoolingRate = 0.0;
			ZoneEvapUnit( UnitNum ).UnitLatentCoolingEnergy = 0.0;
			ZoneEvapUnit( UnitNum ).FanAvailStatus = 0.0;

			// place default cold setpoints on control nodes of select evap coolers
			if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_1_Type_Num == iEvapCoolerDirectResearchSpecial ) || ( ZoneEvapUnit( UnitNum ).EvapCooler_1_Type_Num == iEvapCoolerInDirectRDDSpecial ) ) {
				if ( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).EvapControlNodeNum > 0 ) {
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).EvapControlNodeNum ).TempSetPoint = -20.0;
				}
			}
			if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Type_Num == iEvapCoolerDirectResearchSpecial ) || ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Type_Num == iEvapCoolerInDirectRDDSpecial ) ) {
				if ( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).EvapControlNodeNum > 0 ) {
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).EvapControlNodeNum ).TempSetPoint = -20.0;
				}
			}

			MyEnvrnFlag( UnitNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( UnitNum ) = true;
		}

		TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;
		if ( ZoneEvapUnit( UnitNum ).TimeElapsed != TimeElapsed ) {
			ZoneEvapUnit( UnitNum ).WasOnLastTimestep = ZoneEvapUnit( UnitNum ).IsOnThisTimestep;

			ZoneEvapUnit( UnitNum ).TimeElapsed = TimeElapsed;
		}

	}

	void
	SizeZoneEvaporativeCoolerUnit( int const UnitNum ) // unit number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2013
		//       MODIFIED       August 2014 Bereket Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using namespace DataSizing;
		using DataHVACGlobals::SmallAirVolFlow;
		using DataHVACGlobals::SystemAirflowSizing;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName("SizeZoneEvaporativeCoolerUnit: "); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod(0); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		DataScalableSizingON = false;
		ZoneHeatingOnlyFan = false;
		ZoneCoolingOnlyFan = false;

		CompType = "ZoneHVAC:EvaporativeCoolerUnit";
		CompName = ZoneEvapUnit( UnitNum ).Name;
		DataZoneNumber = ZoneEvapUnit( UnitNum ).ZonePtr;
		SizingMethod = CoolingAirflowSizing;
		FieldNum = 1; // N1 , \field Maximum Supply Air Flow Rate
		PrintFlag = true;
		SizingString = ZoneEvapCoolerUnitFields(UnitNum).FieldNames(FieldNum) + " [m3/s]";

		if (CurZoneEqNum > 0) {

			if ( ZoneEvapUnit( UnitNum ).HVACSizingIndex > 0) {
				ZoneCoolingOnlyFan = true;
				zoneHVACIndex = ZoneEvapUnit( UnitNum ).HVACSizingIndex;
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ){
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
							PrintFlag = false;
						}
					} else if ( SAFMethod == FlowPerFloorArea ){
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ){
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					}
					else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					}
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					ZoneEvapUnit( UnitNum ).DesignAirVolumeFlowRate = TempSize;
				} else if ( SAFMethod == FlowPerCoolingCapacity ) {
					SizingMethod = CoolingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
					if ( ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod == FractionOfAutosizedCoolingCapacity ) {
						DataFracOfAutosizedCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
					}
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					DataCapacityUsedForSizing = TempSize;
					DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					SizingMethod = CoolingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
					ZoneEvapUnit( UnitNum ).DesignAirVolumeFlowRate = TempSize;
				}
				DataScalableSizingON = false;
				ZoneCoolingOnlyFan = false;
			} else {
				// no scalble sizing method has been specified. Sizing proceeds using the method
				// specified in the zoneHVAC object
				// N1 , \field Maximum Supply Air Flow Rate
				ZoneCoolingOnlyFan = true;
				if ( ZoneEvapUnit( UnitNum ).DesignAirVolumeFlowRate > 0.0) {
					 PrintFlag = false;
				}
				TempSize = ZoneEvapUnit( UnitNum ).DesignAirVolumeFlowRate;
				RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
				ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate = TempSize;
				ZoneCoolingOnlyFan = false;
			}
		}

	}

	void
	CalcZoneEvaporativeCoolerUnit(
		int const UnitNum, // unit number
		int const ZoneNum, // number of zone being served
		Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
		Real64 & LatentOutputProvided // Latent add/removal  (kg/s), dehumid = negative
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::SmallLoad;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using Fans::SimulateFanComponents;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneCoolingLoad;
		Real64 MinHumRat;
		Real64 CoolingLoadThreashold;
		Real64 ZoneTemp;
		Real64 CoolSetLowThrottle;
		Real64 CoolSetHiThrottle;

		{ auto const SELECT_CASE_var( ZoneEvapUnit( UnitNum ).ControlSchemeType );

		if ( SELECT_CASE_var == ZoneTemperatureDeadBandOnOffCycling ) {
			ZoneTemp = Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).Temp;
			CoolSetLowThrottle = ZoneThermostatSetPointHi( ZoneNum ) - ( 0.5 * ZoneEvapUnit( UnitNum ).ThrottlingRange );
			CoolSetHiThrottle = ZoneThermostatSetPointHi( ZoneNum ) + ( 0.5 * ZoneEvapUnit( UnitNum ).ThrottlingRange );

			if ( ( ZoneTemp < CoolSetLowThrottle ) || ! ZoneEvapUnit( UnitNum ).UnitIsAvailable ) {
				ZoneEvapUnit( UnitNum ).IsOnThisTimestep = false;
			} else if ( ZoneTemp > CoolSetHiThrottle ) {
				ZoneEvapUnit( UnitNum ).IsOnThisTimestep = true;
			} else {
				if ( ZoneEvapUnit( UnitNum ).WasOnLastTimestep ) {
					ZoneEvapUnit( UnitNum ).IsOnThisTimestep = true;
				} else {
					ZoneEvapUnit( UnitNum ).IsOnThisTimestep = false;
				}
			}

			if ( ZoneEvapUnit( UnitNum ).IsOnThisTimestep ) {

				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate * ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio;
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate;

				if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
					Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
				}

				if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}
			} else { // not running
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanInletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanInletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = 0.0;

				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).InletNode ).MassFlowRate = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).InletNode ).MassFlowRateMaxAvail = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).OutletNode ).MassFlowRate = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).OutletNode ).MassFlowRateMaxAvail = 0.0;

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) {
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).InletNode ).MassFlowRate = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).InletNode ).MassFlowRateMaxAvail = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).OutletNode ).MassFlowRate = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).OutletNode ).MassFlowRateMaxAvail = 0.0;
				}

				if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = 0.0;
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = 0.0;
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
				}

				if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}
			}

		} else if ( SELECT_CASE_var == ZoneCoolingLoadOnOffCycling ) {

			// get zone loads
			ZoneCoolingLoad = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
			CoolingLoadThreashold = - 1.0 * ZoneEvapUnit( UnitNum ).ThresholdCoolingLoad;

			if ( ( ZoneCoolingLoad < CoolingLoadThreashold ) && ZoneEvapUnit( UnitNum ).UnitIsAvailable ) {

				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate * ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio;
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate;

				if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
					Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
				}

				if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}
			} else {
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanInletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanInletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = 0.0;

				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).InletNode ).MassFlowRate = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).InletNode ).MassFlowRateMaxAvail = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).OutletNode ).MassFlowRate = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).OutletNode ).MassFlowRateMaxAvail = 0.0;

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) {
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).InletNode ).MassFlowRate = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).InletNode ).MassFlowRateMaxAvail = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).OutletNode ).MassFlowRate = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).OutletNode ).MassFlowRateMaxAvail = 0.0;
				}

				if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = 0.0;
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = 0.0;
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
				}

				if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}
			}

		} else if ( SELECT_CASE_var == ZoneCoolingLoadVariableSpeedFan ) {
			// get zone loads
			ZoneCoolingLoad = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
			CoolingLoadThreashold = - 1.0 * ZoneEvapUnit( UnitNum ).ThresholdCoolingLoad;
			if ( ( ZoneCoolingLoad < CoolingLoadThreashold ) && ZoneEvapUnit( UnitNum ).UnitIsAvailable ) {

				//determine fan speed to meet load
				ControlVSEvapUnitToMeetLoad( UnitNum, ZoneNum, ZoneCoolingLoad );

				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate * ZoneEvapUnit( UnitNum ).FanSpeedRatio;
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate;

				if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
					Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
				}

				if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}
			} else {
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanInletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanInletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = 0.0;
				Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = 0.0;

				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).InletNode ).MassFlowRate = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).InletNode ).MassFlowRateMaxAvail = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).OutletNode ).MassFlowRate = 0.0;
				Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_1_Index ).OutletNode ).MassFlowRateMaxAvail = 0.0;

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) {
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).InletNode ).MassFlowRate = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).InletNode ).MassFlowRateMaxAvail = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).OutletNode ).MassFlowRate = 0.0;
					Node( EvapCond( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index ).OutletNode ).MassFlowRateMaxAvail = 0.0;
				}

				if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = 0.0;
					Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = 0.0;
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}

				if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
				}

				if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
					SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
				}
				if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
					SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
				}
			}
		}}

		// calculate sensible load met (unit serving Zone) using delta enthalpy at a constant (minimum) humidity ratio)
		MinHumRat = min( Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).HumRat, Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).HumRat );
		SensibleOutputProvided = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).Temp, MinHumRat ) );
		LatentOutputProvided = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate * ( Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).HumRat - Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).HumRat );
	}

	void
	ControlVSEvapUnitToMeetLoad(
		int const UnitNum, // unit number
		int const ZoneNum, // number of zone being served
		Real64 const ZoneCoolingLoad // target cooling load
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using Fans::SimulateFanComponents;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinHumRat;
		FArray1D< Real64 > Par( 5 ); // Parameters passed to RegulaFalsi
		Real64 FanSpeedRatio;
		static Real64 ErrorToler( 0.001 ); // error tolerance
		int SolFla; // Flag of RegulaFalsi solver
		Real64 FullFlowSensibleOutputProvided;

		// first get full load result
		ErrorToler = 0.01;

		ZoneEvapUnit( UnitNum ).FanSpeedRatio = ZoneEvapUnit( UnitNum ).DesignFanSpeedRatio;
		Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate * ZoneEvapUnit( UnitNum ).FanSpeedRatio;
		Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
		Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
		Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate;

		if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
			Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
			Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
		}
		if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
			Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
			Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
			SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
			SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
		}

		if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
			SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
		}
		if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
			SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, ZoneEvapUnit( UnitNum ).FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		// calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
		MinHumRat = min( Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).HumRat, Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).HumRat );
		FullFlowSensibleOutputProvided = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).Temp, MinHumRat ) );

		if ( FullFlowSensibleOutputProvided < ZoneCoolingLoad ) { // find speed ratio by regula falsi numerical method
			Par( 1 ) = UnitNum;
			Par( 2 ) = ZoneNum;
			Par( 3 ) = ZoneEvapUnit( UnitNum ).ZoneNodeNum;
			Par( 5 ) = ZoneCoolingLoad;
			FanSpeedRatio = 1.0;

			SolveRegulaFalsi( ErrorToler, MaxIte, SolFla, FanSpeedRatio, VSEvapUnitLoadResidual, 0.0, 1.0, Par );
			if ( SolFla == -1 ) {
				if ( ZoneEvapUnit( UnitNum ).UnitVSControlMaxIterErrorIndex == 0 ) {
					ShowWarningError( "Iteration limit exceeded calculating variable speed evap unit fan speed ratio, for unit=" + ZoneEvapUnit( UnitNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Fan speed ratio returned=" + RoundSigDigits( FanSpeedRatio, 2 ) );
					ShowContinueError( "Check input for Fan Placement." );
				}
				ShowRecurringWarningErrorAtEnd( "Zone Evaporative Cooler unit control failed (iteration limit [" + RoundSigDigits( MaxIte ) + "]) for ZoneHVAC:EvaporativeCoolerUnit =\"" + ZoneEvapUnit( UnitNum ).Name, ZoneEvapUnit( UnitNum ).UnitVSControlMaxIterErrorIndex );

			} else if ( SolFla == -2 ) {
				if ( ZoneEvapUnit( UnitNum ).UnitVSControlLimitsErrorIndex == 0 ) {
					ShowWarningError( "Variable speed evaporative cooler unit calculation failed: fan speed ratio limits exceeded," " for unit = " + ZoneEvapUnit( UnitNum ).Name );
					ShowContinueError( "Check input for Fan Placement." );
					ShowContinueErrorTimeStamp( "" );
					if ( WarmupFlag ) ShowContinueError( "Error occurred during warmup days." );
				}
				ShowRecurringWarningErrorAtEnd( "Zone Evaporative Cooler unit control failed (limits exceeded) " "for ZoneHVAC:EvaporativeCoolerUnit =\"" + ZoneEvapUnit( UnitNum ).Name, ZoneEvapUnit( UnitNum ).UnitVSControlLimitsErrorIndex );
			}
			ZoneEvapUnit( UnitNum ).FanSpeedRatio = FanSpeedRatio;
		}

	}

	Real64
	VSEvapUnitLoadResidual(
		Real64 const FanSpeedRatio,
		Optional< FArray1S< Real64 > const > Par // parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using Fans::SimulateFanComponents;

		// Return value
		Real64 Residual;

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int UnitNum;
		int ZoneNum;
		int ZoneNodeNum;
		Real64 LoadToBeMet; // sensible load to be met
		Real64 MinHumRat;
		Real64 SensibleOutputProvided;

		UnitNum = int( Par()( 1 ) );
		ZoneNum = int( Par()( 2 ) );
		ZoneNodeNum = int( Par()( 3 ) );
		LoadToBeMet = Par()( 5 );

		Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate = ZoneEvapUnit( UnitNum ).DesignAirMassFlowRate * FanSpeedRatio;
		Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
		Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
		Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate;

		if ( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum > 0 ) {
			Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
			Node( ZoneEvapUnit( UnitNum ).UnitReliefNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
		}
		if ( ZoneEvapUnit( UnitNum ).FanLocation == BlowThruFan ) {
			Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRate = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
			Node( ZoneEvapUnit( UnitNum ).FanOutletNodeNum ).MassFlowRateMaxAvail = Node( ZoneEvapUnit( UnitNum ).OAInletNodeNum ).MassFlowRate;
			SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( ZoneEvapUnit( UnitNum ).EvapCooler_1_AvailStatus ) {
			SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_1_Name, ZoneEvapUnit( UnitNum ).EvapCooler_1_Index );
		}

		if ( ( ZoneEvapUnit( UnitNum ).EvapCooler_2_Index > 0 ) && ZoneEvapUnit( UnitNum ).EvapCooler_2_AvailStatus ) {
			SimEvapCooler( ZoneEvapUnit( UnitNum ).EvapCooler_2_Name, ZoneEvapUnit( UnitNum ).EvapCooler_2_Index );
		}
		if ( ZoneEvapUnit( UnitNum ).FanLocation == DrawThruFan ) {
			SimulateFanComponents( ZoneEvapUnit( UnitNum ).FanName, false, ZoneEvapUnit( UnitNum ).FanIndex, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		MinHumRat = min( Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).HumRat, Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).HumRat );
		SensibleOutputProvided = Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( ZoneEvapUnit( UnitNum ).UnitOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( ZoneEvapUnit( UnitNum ).ZoneNodeNum ).Temp, MinHumRat ) );

		Residual = SensibleOutputProvided - LoadToBeMet;

		return Residual;
	}

	void
	ReportZoneEvaporativeCoolerUnit( int const UnitNum ) // unit number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update output variables for the zone evap unit

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNodeNum;
		int UnitOutletNodeNum;
		Real64 AirMassFlow;
		Real64 MinHumRat;
		Real64 QTotUnitOut;
		Real64 QSensUnitOut;

		ZoneNodeNum = ZoneEvapUnit( UnitNum ).ZoneNodeNum;
		UnitOutletNodeNum = ZoneEvapUnit( UnitNum ).UnitOutletNodeNum;
		AirMassFlow = Node( UnitOutletNodeNum ).MassFlowRate;
		QTotUnitOut = AirMassFlow * ( Node( UnitOutletNodeNum ).Enthalpy - Node( ZoneNodeNum ).Enthalpy );
		MinHumRat = min( Node( ZoneNodeNum ).HumRat, Node( UnitOutletNodeNum ).HumRat );
		QSensUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( UnitOutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( ZoneNodeNum ).Temp, MinHumRat ) );

		ZoneEvapUnit( UnitNum ).UnitTotalCoolingRate = std::abs( min( 0.0, QTotUnitOut ) );
		ZoneEvapUnit( UnitNum ).UnitTotalCoolingEnergy = ZoneEvapUnit( UnitNum ).UnitTotalCoolingRate * TimeStepSys * SecInHour;
		ZoneEvapUnit( UnitNum ).UnitSensibleCoolingRate = std::abs( min( 0.0, QSensUnitOut ) );
		ZoneEvapUnit( UnitNum ).UnitSensibleCoolingEnergy = ZoneEvapUnit( UnitNum ).UnitSensibleCoolingRate * TimeStepSys * SecInHour;
		ZoneEvapUnit( UnitNum ).UnitLatentHeatingRate = std::abs( max( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
		ZoneEvapUnit( UnitNum ).UnitLatentHeatingEnergy = ZoneEvapUnit( UnitNum ).UnitLatentHeatingRate * TimeStepSys * SecInHour;
		ZoneEvapUnit( UnitNum ).UnitLatentCoolingRate = std::abs( min( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
		ZoneEvapUnit( UnitNum ).UnitLatentCoolingEnergy = ZoneEvapUnit( UnitNum ).UnitLatentCoolingRate * TimeStepSys * SecInHour;
		ZoneEvapUnit( UnitNum ).UnitFanSpeedRatio = ZoneEvapUnit( UnitNum ).FanSpeedRatio;

	}

	//        End of Reporting subroutines for the EvaporativeCoolers Module
	// *****************************************************************************

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // EvaporativeCoolers

} // EnergyPlus
