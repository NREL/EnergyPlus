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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EvaporativeCoolers.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
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
	using DataGlobals::MaxNameLength;
	using DataGlobals::DisplayExtraWarnings;
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
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;

	int NumZoneEvapUnits( 0 );
	Array1D_bool CheckZoneEvapUnitName;
	bool GetInputZoneEvapUnit( true );

	// Indirect Evaporative Coolers Research Special Operating Modes
	int const None( 0 ); // the indirect evaporative cooler Research Special is scheduled off or turned off
	int const DryModulated( 1 ); // the indirect evaporative cooler Research Special is modulated in Dry Mode
	int const DryFull( 2 ); // the indirect evaporative cooler Research Special is run in full capacity in Dry Mode
	int const DryWetModulated( 3 ); // the indirect evaporative cooler Research Special is modulated in Dry Mode or wet Mode
	int const WetModulated( 4 ); // the indirect evaporative cooler Research Special is modulated in wet Mode
	int const WetFull( 5 ); // the indirect evaporative cooler Research Special is run in full capacity in Wet Mode

	// SUBROUTINE SPECIFICATIONS FOR MODULE EvapCoolers

	// component model routines

	// zone unit routines

	// Object Data
	Array1D< EvapConditions > EvapCond;
	Array1D< ZoneEvapCoolerUnitStruct > ZoneEvapUnit;
	Array1D< ZoneEvapCoolerUnitFieldData > ZoneEvapCoolerUnitFields;

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
			EvapCoolNum = FindItemInList( CompName, EvapCond, &EvapConditions::EvapCoolerName );
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
		using CurveManager::GetCurveIndex;
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
			VerifyName( cAlphaArgs( 1 ), EvapCond, &EvapConditions::EvapCoolerName, EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
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
			VerifyName( cAlphaArgs( 1 ), EvapCond, &EvapConditions::EvapCoolerName, EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
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
			VerifyName( cAlphaArgs( 1 ), EvapCond, &EvapConditions::EvapCoolerName, EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
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
			VerifyName( cAlphaArgs( 1 ), EvapCond, &EvapConditions::EvapCoolerName, EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
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

			EvapCond( EvapCoolNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 7 ), cAlphaArgs( 8 ), "Evap Air Nodes" );

			if ( lAlphaFieldBlanks( 9 ) ) {
				EvapCond( EvapCoolNum ).SecondaryInletNode = 0;
			} else {
				EvapCond( EvapCoolNum ).SecondaryInletNode = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			}

			if ( lAlphaFieldBlanks( 10 ) ) {
				EvapCond( EvapCoolNum ).SecondaryOutletNode = 0;
			} else {
				EvapCond( EvapCoolNum ).SecondaryOutletNode = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			}

			EvapCond( EvapCoolNum ).EvapControlNodeNum = GetOnlySingleNode( cAlphaArgs( 11 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).TertiaryInletNode = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 3, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).EvapWaterSupplyName = cAlphaArgs( 13 );
			if ( lAlphaFieldBlanks( 13 ) ) {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( EvapCond( EvapCoolNum ).EvapCoolerName, cCurrentModuleObject, EvapCond( EvapCoolNum ).EvapWaterSupplyName, ErrorsFound, EvapCond( EvapCoolNum ).EvapWaterSupTankID, EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );

			}

			//input the numerical data
			EvapCond( EvapCoolNum ).WetCoilMaxEfficiency = rNumericArgs( 1 );
			if ( lNumericFieldBlanks( 2 ) ) {
				EvapCond( EvapCoolNum ).DryCoilMaxEfficiency = 0.0;
			} else {
				EvapCond( EvapCoolNum ).DryCoilMaxEfficiency = rNumericArgs( 2 );
			}
			EvapCond( EvapCoolNum ).IndirectRecircPumpPower = rNumericArgs( 3 );
			EvapCond( EvapCoolNum ).RecircPumpSizingFactor = rNumericArgs( 4 );
			EvapCond( EvapCoolNum ).IndirectVolFlowRate = rNumericArgs( 5 );
			EvapCond( EvapCoolNum ).IndirectVolFlowScalingFactor = rNumericArgs( 6 );
			EvapCond( EvapCoolNum ).IndirectFanPower = rNumericArgs( 7 );
			EvapCond( EvapCoolNum ).FanSizingSpecificPower = rNumericArgs( 8 );
			EvapCond( EvapCoolNum ).VolFlowRate = rNumericArgs( 9 );
			EvapCond( EvapCoolNum ).DPBoundFactor = rNumericArgs( 10 );
			if ( lNumericFieldBlanks( 11 ) ) {
				EvapCond( EvapCoolNum ).DriftFraction = 0.0;
			} else {
				EvapCond( EvapCoolNum ).DriftFraction = rNumericArgs( 11 );
			}
			if ( lNumericFieldBlanks( 12 ) ) {
				EvapCond( EvapCoolNum ).BlowDownRatio = 0.0;
			} else {
				EvapCond( EvapCoolNum ).BlowDownRatio = rNumericArgs( 12 );
			}
			if ( lNumericFieldBlanks( 2 ) || lNumericFieldBlanks( 13 ) || lNumericFieldBlanks( 14 ) || lNumericFieldBlanks( 15 ) ) {
				EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag = false;
			} else {
				if ( !lNumericFieldBlanks( 2 ) && !lNumericFieldBlanks( 13 ) && !lNumericFieldBlanks( 14 ) && !lNumericFieldBlanks( 15 ) ) {
					EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag = true;
					EvapCond( EvapCoolNum ).MinOATDBEvapCooler = rNumericArgs( 13 );
					EvapCond( EvapCoolNum ).MaxOATWBEvapCooler = rNumericArgs( 14 );
					EvapCond( EvapCoolNum ).MaxOATDBEvapCooler = rNumericArgs( 15 );
				} else {
					EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag = false;
				}
			}
			EvapCond( EvapCoolNum ).WetbulbEffecCurveIndex = GetCurveIndex( cAlphaArgs( 3 ) );
			EvapCond( EvapCoolNum ).DrybulbEffecCurveIndex = GetCurveIndex( cAlphaArgs( 4 ) );
			EvapCond( EvapCoolNum ).PumpPowerModifierCurveIndex = GetCurveIndex( cAlphaArgs( 5 ) );
			EvapCond( EvapCoolNum ).FanPowerModifierCurveIndex = GetCurveIndex( cAlphaArgs( 6 ) );

			SetupOutputVariable( "Evaporative Cooler Total Stage Effectiveness []", EvapCond( EvapCoolNum ).StageEff, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );
			SetupOutputVariable( "Evaporative Cooler Part Load Ratio []", EvapCond( EvapCoolNum ).PartLoadFract, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );

			SetupOutputVariable( "Evaporative Cooler Dewpoint Bound Status []", EvapCond( EvapCoolNum ).DewPointBoundFlag, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );
			SetupOutputVariable( "Evaporative Cooler Operating Mode Status []", EvapCond( EvapCoolNum ).IECOperatingStatus, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );


		} // end of Indirect Research Special cooler input loop

		cCurrentModuleObject = "EvaporativeCooler:Direct:ResearchSpecial";
		for ( DirectEvapCoolNum = 1; DirectEvapCoolNum <= NumDirectResearchSpecialEvapCool; ++DirectEvapCoolNum ) {
			EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + NumRDDEvapCool + DirectEvapCoolNum;
			GetObjectItem( cCurrentModuleObject, DirectEvapCoolNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), EvapCond, &EvapConditions::EvapCoolerName, EvapCoolNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
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

			EvapCond( EvapCoolNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Evap Air Nodes" );

			EvapCond( EvapCoolNum ).EvapControlNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			EvapCond( EvapCoolNum ).EvapWaterSupplyName = cAlphaArgs( 8 );

			if ( lAlphaFieldBlanks( 8 ) ) {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				EvapCond( EvapCoolNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( EvapCond( EvapCoolNum ).EvapCoolerName, cCurrentModuleObject, EvapCond( EvapCoolNum ).EvapWaterSupplyName, ErrorsFound, EvapCond( EvapCoolNum ).EvapWaterSupTankID, EvapCond( EvapCoolNum ).EvapWaterTankDemandARRID );
			}
			EvapCond( EvapCoolNum ).DirectEffectiveness = rNumericArgs( 1 );

			EvapCond( EvapCoolNum ).VolFlowRate = rNumericArgs( 2 );
			EvapCond( EvapCoolNum ).RecircPumpPower = rNumericArgs( 3 );
			EvapCond( EvapCoolNum ).RecircPumpSizingFactor = rNumericArgs( 4 );
			if ( lNumericFieldBlanks( 5 ) ) {
				EvapCond( EvapCoolNum ).DriftFraction = 0.0;
			} else {
				EvapCond( EvapCoolNum ).DriftFraction = rNumericArgs( 5 );
			}
			if ( lNumericFieldBlanks( 6 ) ) {
				EvapCond( EvapCoolNum ).BlowDownRatio = 0.0;
			} else {
				EvapCond( EvapCoolNum ).BlowDownRatio = rNumericArgs( 6 );
			}
			if ( lNumericFieldBlanks( 7 ) || lNumericFieldBlanks( 8 ) || lNumericFieldBlanks( 9 ) ) {
				EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag = false;
			} else {
				if ( !lNumericFieldBlanks( 7 ) && !lNumericFieldBlanks( 8 ) && !lNumericFieldBlanks( 9 ) ) {
					EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag = true;
					EvapCond( EvapCoolNum ).MinOATDBEvapCooler = rNumericArgs( 7 );
					EvapCond( EvapCoolNum ).MaxOATWBEvapCooler = rNumericArgs( 8 );
					EvapCond( EvapCoolNum ).MaxOATDBEvapCooler = rNumericArgs( 9 );
				} else {
					EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag = false;
				}
			}
			EvapCond( EvapCoolNum ).WetbulbEffecCurveIndex = GetCurveIndex( cAlphaArgs( 3 ) );
			EvapCond( EvapCoolNum ).PumpPowerModifierCurveIndex = GetCurveIndex( cAlphaArgs( 4 ) );

			SetupOutputVariable( "Evaporative Cooler Stage Effectiveness []", EvapCond( EvapCoolNum ).StageEff, "System", "Average", EvapCond( EvapCoolNum ).EvapCoolerName );
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
		EvapCond( EvapCoolNum ).IECOperatingStatus = 0;

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
		//       MODIFIED       March 2014 Daeho Kang, Add sizing additional fields
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
		using Fans::SetFanData;
		using General::RoundSigDigits;

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
		static int AirSysBranchLoop( 0 );
		static int BranchComp( 0 );
		bool HardSizeNoDesRun;			// Indicator to a hard-sized field with no design sizing data
		bool IsAutoSize;				// Indicator to autosize
		Real64 IndirectVolFlowRateDes;	// Autosized volume flow rate for reporting
		Real64 IndirectVolFlowRateUser;	// Hardsized volume flow rate for reporting
		bool SizingDesRunThisAirSys;	// true if a particular air system had a Sizing:System object and system sizing done
		bool SizingDesRunThisZone;		// true if a particular zone had a Sizing:Zone object and zone sizing was done
		Real64 PadAreaDes;				// Autosized celdek pad area for reporting
		Real64 PadAreaUser;				// Hardsized celdek pad area for reporting
		Real64 PadDepthDes;				// Autosized celdek pad depth for reporting
		Real64 PadDepthUser;			// Hardsized celdek pad depth for reporting

		Real64 volFlowRateDes;			// Autosized volume flow rate for reporting
		std::string CompType; // for ease in getting objects

		//inits
		CoolerOnOApath = false;
		CoolerOnMainAirLoop = false;
		IndirectVolFlowRateDes = 0.0;
		IndirectVolFlowRateUser = 0.0;
		PadAreaDes = 0.0;
		PadAreaUser = 0.0;
		PadDepthDes = 0.0;
		PadDepthUser = 0.0;

		if ( SysSizingRunDone || ZoneSizingRunDone) {
			HardSizeNoDesRun = false;
		} else {
			HardSizeNoDesRun = true;
		}
		if ( CurSysNum > 0 ) {
			CheckThisAirSystemForSizing( CurSysNum, SizingDesRunThisAirSys );
		} else {
			SizingDesRunThisAirSys = false;
		}
		if ( CurZoneEqNum > 0 ) {
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
		} else {
			SizingDesRunThisZone = false;
		}
		if ( SizingDesRunThisAirSys ) {
			HardSizeNoDesRun = false; // Check if design infomation is available
		}

		{ auto const SELECT_CASE_var( EvapCond( EvapCoolNum ).EvapCoolerType );
		if ( SELECT_CASE_var == iEvapCoolerInDirectCELDEKPAD ) {
			CompType = "EvaporativeCooler:Indirect:CelDekPad";
		} else if ( SELECT_CASE_var == iEvapCoolerInDirectWETCOIL ) {
			CompType = "EvaporativeCooler:Indirect:WetCoil";
		} else if ( SELECT_CASE_var == iEvapCoolerInDirectRDDSpecial ) {
			CompType = "EvaporativeCooler:Indirect:ResearchSpecial";
		} else if ( SELECT_CASE_var == iEvapCoolerDirectResearchSpecial ) {
			CompType = "EvaporativeCooler:Direct:ResearchSpecial";
		} else if ( SELECT_CASE_var == iEvapCoolerDirectCELDEKPAD ) {
			CompType = "EvaporativeCooler:Direct:CelDekPad";
		}}

		// Search once for the object on an air system
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
			if ( ! CoolerOnMainAirLoop ) CoolerOnOApath = true;
		}

		// Start with the indirect volume flow rate
		IsAutoSize = false;
		if ( EvapCond( EvapCoolNum ).IndirectVolFlowRate == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys ) {
			HardSizeNoDesRun = true;
		}
		if ( CurSysNum > 0 ) { //central system
			if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
				if ( EvapCond( EvapCoolNum ).IndirectVolFlowRate > 0.0 ) {
					if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectCELDEKPAD || EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectWETCOIL || EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial ) {
						ReportSizingOutput( CompType, EvapCond( EvapCoolNum ).EvapCoolerName, "User-Specified Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).IndirectVolFlowRate );
					}
				}
			} else {  // Autosize or hardsize with design data
				if ( CoolerOnMainAirLoop ) {
					IndirectVolFlowRateDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				} else if ( CoolerOnOApath ) {
					IndirectVolFlowRateDes = max( FinalSysSizing( CurSysNum ).DesOutAirVolFlow, 0.5*FinalSysSizing( CurSysNum ).DesMainVolFlow );
				}
				// apply scaling factor the secondary air fan flow rate
				if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial ) {
					IndirectVolFlowRateDes = IndirectVolFlowRateDes * EvapCond( EvapCoolNum ).IndirectVolFlowScalingFactor;
				}
			}
		} else if ( CurZoneEqNum > 0 ) { //zone equipment
			if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
				if ( EvapCond( EvapCoolNum ).IndirectVolFlowRate > 0.0 ) {
					// report for the indirect evap cooler types only
					if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectCELDEKPAD || EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectWETCOIL || EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial ) {
						ReportSizingOutput( CompType, EvapCond( EvapCoolNum ).EvapCoolerName,"User-Specified Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).IndirectVolFlowRate );
					}
				}
			} else {  // Autosize or hardsize with design data
				// zone equip evap coolers
				IndirectVolFlowRateDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
				// apply scaling factor the secondary air fan flow rate
				if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial ) {
					IndirectVolFlowRateDes = IndirectVolFlowRateDes * EvapCond( EvapCoolNum ).IndirectVolFlowScalingFactor;
				}
			}

		} else {

		}
		if ( !HardSizeNoDesRun ) {
			if ( IsAutoSize ) {
				EvapCond( EvapCoolNum ).IndirectVolFlowRate = IndirectVolFlowRateDes;
				if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectCELDEKPAD || EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectWETCOIL || EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial ) {
					ReportSizingOutput( CompType, EvapCond( EvapCoolNum ).EvapCoolerName, "Design Size Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).IndirectVolFlowRate );
				}
			} else {
				if ( EvapCond( EvapCoolNum ).IndirectVolFlowRate > 0.0 && IndirectVolFlowRateDes > 0.0 ) {
					IndirectVolFlowRateUser = EvapCond( EvapCoolNum ).IndirectVolFlowRate;
					ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName,
						"Design Size Secondary Fan Flow Rate [m3/s]", IndirectVolFlowRateDes,
						"User-Specified Secondary Fan Flow Rate [m3/s]", IndirectVolFlowRateUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( IndirectVolFlowRateDes - IndirectVolFlowRateUser ) / IndirectVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeEvaporativeCooler:Indirect:ResearchSpecial: Potential issue with equipment sizing for " + EvapCond( EvapCoolNum ).EvapCoolerName );
							ShowContinueError( "User-Specified Secondary Fan Flow Rate of " +
								RoundSigDigits( IndirectVolFlowRateUser, 5 ) + " [m3/s]" );
							ShowContinueError( "differs from Design Size Secondary Fan Flow Rate of " +
								RoundSigDigits( IndirectVolFlowRateDes, 5 ) + " [m3/s]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}

		// Next up the other volume flow rate
		IsAutoSize = false;
		if ( EvapCond( EvapCoolNum ).VolFlowRate == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys ) {
			HardSizeNoDesRun = true;
		}
		if ( CurSysNum > 0 ) { //central system
			if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
				// the .VolFlowRate variable wasn't reported to the eio in develop, so not doing it here
				//if ( EvapCond( EvapCoolNum ).VolFlowRate > 0.0 ) {
					//ReportSizingOutput( CompType, EvapCond( EvapCoolNum ).EvapCoolerName,
						//"User-Specified Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).VolFlowRate );
				//}
			} else {  // Autosize or hardsize with design data
				if ( CoolerOnMainAirLoop ) {
					volFlowRateDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				} else if ( CoolerOnOApath ) {
					volFlowRateDes = max( FinalSysSizing( CurSysNum ).DesOutAirVolFlow, 0.5*FinalSysSizing( CurSysNum ).DesMainVolFlow );
				}
				// no scaling factor on the volFlowRate in develop, so not doing it here
			}
		} else if ( CurZoneEqNum > 0 ) { //zone equipment
			// zone equip evap coolers

			if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
				// the .VolFlowRate variable wasn't reported to the eio in develop, so not doing it here
				//if ( EvapCond( EvapCoolNum ).VolFlowRate > 0.0 ) {
				//ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName,
				//"User-Specified Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).VolFlowRate );
				//}
			} else {  // Autosize or hardsize with design data
				volFlowRateDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
			}

		} else { //zone equipment
			// can't do zone equip evap coolers yet
		}
		if ( !HardSizeNoDesRun ) {
			if ( IsAutoSize ) {
				EvapCond( EvapCoolNum ).VolFlowRate = volFlowRateDes;
				// only these two evap coolers has primary air design flow rate
				if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial ) {
					ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName, "Primary Air Design Flow Rate [m3/s]", EvapCond( EvapCoolNum ).VolFlowRate );
					ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName, "Secondary Air Design Flow Rate [m3/s]", EvapCond( EvapCoolNum ).IndirectVolFlowRate );
				} else if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerDirectResearchSpecial ) {
					ReportSizingOutput( "EvaporativeCooler:Direct:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName, "Primary Air Design Flow Rate [m3/s]", EvapCond( EvapCoolNum ).VolFlowRate );
				}
			} else {
				// the .VolFlowRate variable wasn't reported to the eio in develop, so not doing it here
				//if ( EvapCond( EvapCoolNum ).IndirectVolFlowRate > 0.0 && IndirectVolFlowRateDes > 0.0 ) {
					//IndirectVolFlowRateUser = EvapCond( EvapCoolNum ).IndirectVolFlowRate;
					//ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName,
						//"Design Size Secondary Fan Flow Rate [m3/s]", IndirectVolFlowRateDes,
						//"User-Specified Secondary Fan Flow Rate [m3/s]", IndirectVolFlowRateUser );
					//if ( DisplayExtraWarnings ) {
						//if ( ( std::abs( IndirectVolFlowRateDes - IndirectVolFlowRateUser ) / IndirectVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
							//ShowMessage( "SizeEvaporativeCooler:Indirect:ResearchSpecial: \nPotential issue with equipment sizing for " + EvapCond( EvapCoolNum ).EvapCoolerName );
							//ShowContinueError( "User-Specified Secondary Fan Flow Rate of " +
								//RoundSigDigits( IndirectVolFlowRateUser, 5 ) + " [m3/s]" );
							//ShowContinueError( "differs from Design Size Secondary Fan Flow Rate of " +
								//RoundSigDigits( IndirectVolFlowRateDes, 5 ) + " [m3/s]" );
							//ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							//ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						//}
					//}
				//}
			}
		}

		if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerDirectCELDEKPAD ) {
			IsAutoSize = false;
			if ( EvapCond( EvapCoolNum ).PadArea == AutoSize ) {
				IsAutoSize = true;
			}
			if ( CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys ) {
				HardSizeNoDesRun = true;
			}
			if ( SizingDesRunThisAirSys ) HardSizeNoDesRun = false; // Check if design infomation is available
			// Design air flow rate
			if ( CurSysNum > 0 ) {  // central system
				if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
					HardSizeNoDesRun = true;
					if ( EvapCond( EvapCoolNum ).PadArea > 0.0 ) {
						ReportSizingOutput( "EvaporativeCooler:Direct:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
							"User-Specified Celdek Pad Area [m2]", EvapCond( EvapCoolNum ).PadArea );
					}
				} else {  // Autosize or hardsize with design data
					if ( CoolerOnMainAirLoop ) {
						IndirectVolFlowRateDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else if ( CoolerOnOApath ) {
						IndirectVolFlowRateDes = std::max( FinalSysSizing( CurSysNum ).DesOutAirVolFlow, 0.50*FinalSysSizing( CurSysNum ).DesMainVolFlow );
					}
					// Face air velocity of 3m/s is assumed
					PadAreaDes = IndirectVolFlowRateDes / 3.0;
				}
			} else if ( CurZoneEqNum > 0 ) { //zone equipment
				// zone equip evap coolers
				if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
					HardSizeNoDesRun = true;
					if ( EvapCond( EvapCoolNum ).PadArea > 0.0 ) {
						// report for the indirect evap cooler types only
						if ( EvapCond( EvapCoolNum ).PadArea > 0.0 ) {
							ReportSizingOutput( "EvaporativeCooler:Direct:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
								"User-Specified Celdek Pad Area [m2]", EvapCond( EvapCoolNum ).PadArea );
						}
					}
				} else {  // Autosize or hardsize with design data
					// zone equip evap coolers
					IndirectVolFlowRateDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
					// Face air velocity of 3m/s is assumed
					PadAreaDes = IndirectVolFlowRateDes / 3.0;
				}
			} else {

			}

			if ( !HardSizeNoDesRun ) {
				if ( IsAutoSize ) {
					EvapCond( EvapCoolNum ).PadArea = PadAreaDes;
					ReportSizingOutput( "EvaporativeCooler:Direct:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
						"Design Size Celdek Pad Area [m2]", PadAreaDes );
				} else {
					if ( EvapCond( EvapCoolNum ).PadArea > 0.0 && PadAreaDes > 0.0 ) {
						PadAreaUser = EvapCond( EvapCoolNum ).PadArea;
						ReportSizingOutput( "EvaporativeCooler:Direct:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
							"Design Size Celdek Pad Area [m2]", PadAreaDes, "User-Specified Celdek Pad Area [m2]", PadAreaUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( PadAreaDes - PadAreaUser ) / PadAreaUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeEvaporativeCooler:Direct:CelDekPad: \nPotential issue with equipment sizing for " +
									EvapCond( EvapCoolNum ).EvapCoolerName );
								ShowContinueError( "User-Specified Celdek Pad Area of" + RoundSigDigits( PadAreaUser, 2 ) + " [m2]" );
								ShowContinueError( "differs from Design Size Celdek Pad Area of " + RoundSigDigits( PadAreaDes, 2 ) + " [m2]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}

			IsAutoSize = false;
			if ( EvapCond( EvapCoolNum ).PadDepth == AutoSize ) {
				IsAutoSize = true;
			}
			if ( CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys ) {
				HardSizeNoDesRun = true;
			}
			// The following regression equation is used to determine pad depth,
			// assuming saturation effectiveness of 70% and face air velocity of 3m/s:
			// Effectiveness = 0.792714 + 0.958569D - 0.25193V - 1.03215D^2 + 0.0262659V^2 + 0.914869DV -
			// 1.48241VD^2 - 0.018992V^3D + 1.13137D^3V + 0.0327622V^3D^2 - 0.145384D^3V^2
			PadDepthDes = 0.17382;
			if ( IsAutoSize ) {
				EvapCond( EvapCoolNum ).PadDepth = PadDepthDes;
				ReportSizingOutput( "EvaporativeCooler:Direct:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
					"Design Size Celdek Pad Depth [m]", PadDepthDes );
			} else {
				if ( EvapCond( EvapCoolNum ).PadDepth > 0.0 && PadDepthDes > 0.0 ) {
					PadDepthUser = EvapCond( EvapCoolNum ).PadDepth;
					ReportSizingOutput( "EvaporativeCooler:Direct:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
						"Design Size Celdek Pad Depth [m]", PadDepthDes, "User-Specified Celdek Pad Depth [m]", PadDepthUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( PadDepthDes - PadDepthUser ) / PadDepthUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeEvaporativeCooler:Direct:CelDekPad: \nPotential issue with equipment sizing for " +
								EvapCond( EvapCoolNum ).EvapCoolerName );
							ShowContinueError( "User-Specified Celdek Pad Depth of" + RoundSigDigits( PadDepthUser, 2 ) + " [m]" );
							ShowContinueError( "differs from Design Size Celdek Pad Depth of " + RoundSigDigits( PadDepthDes, 2 ) + " [m]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}

		if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectCELDEKPAD ) {
			IsAutoSize = false;

			if ( EvapCond( EvapCoolNum ).IndirectPadArea == AutoSize ) {
				IsAutoSize = true;
			}
			if ( SizingDesRunThisAirSys ) {
				HardSizeNoDesRun = false;  // Check if design infomation is available
			}
			// Design air flow rate
			if ( CurSysNum > 0 ) { //central system
				// where is this cooler located, is it on OA system or main loop?
				// search for this component in Air loop branches.
				for ( AirSysBranchLoop = 1; AirSysBranchLoop <= PrimaryAirSystem( CurSysNum ).NumBranches; ++AirSysBranchLoop) {
					for ( BranchComp = 1; BranchComp <= PrimaryAirSystem( CurSysNum ).Branch( AirSysBranchLoop ).TotalComponents; ++BranchComp ) {
						if ( SameString( PrimaryAirSystem( CurSysNum ).Branch( AirSysBranchLoop ).Comp( BranchComp ).Name,
							EvapCond( EvapCoolNum ).EvapCoolerName ) ) {
							CoolerOnMainAirLoop = true;
						}
					}
				}
				if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
					HardSizeNoDesRun = true;
					if ( EvapCond( EvapCoolNum ).IndirectPadArea > 0.0 ) {
						ReportSizingOutput( "EvaporativeCooler:Indirect:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
							"User-Specified Celdek Pad Area [m2]", EvapCond( EvapCoolNum ).IndirectPadArea );
					}
				} else { // Autosize or hardsize with design data
					if ( !CoolerOnMainAirLoop ) {
						CoolerOnOApath = true;
					}
					if ( CoolerOnMainAirLoop ) {
						IndirectVolFlowRateDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else if ( CoolerOnOApath ) {
						IndirectVolFlowRateDes = std::max( FinalSysSizing( CurSysNum ).DesOutAirVolFlow, 0.5*FinalSysSizing( CurSysNum ).DesMainVolFlow );
					}
					// Face air velocity of 3m/s is assumed
					PadAreaDes = IndirectVolFlowRateDes / 3.0;
				}
			} else if ( CurZoneEqNum > 0 ) { //zone equipment
				// zone equip evap coolers
				if ( !IsAutoSize && !SizingDesRunThisAirSys ) {
					HardSizeNoDesRun = true;
					if ( EvapCond( EvapCoolNum ).IndirectPadArea > 0.0 ) {
						// report for the indirect evap cooler types only
						if ( EvapCond( EvapCoolNum ).PadArea > 0.0 ) {
							ReportSizingOutput( "EvaporativeCooler:Indirect:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
								"User-Specified Celdek Pad Area [m2]", EvapCond( EvapCoolNum ).IndirectPadArea );
						}
					}
				} else {  // Autosize or hardsize with design data
					// zone equip evap coolers
					IndirectVolFlowRateDes = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
					// Face air velocity of 3m/s is assumed
					PadAreaDes = IndirectVolFlowRateDes / 3.0;
				}
			} else {
			}

			if ( !HardSizeNoDesRun ) {
				if ( IsAutoSize ) {
					EvapCond( EvapCoolNum ).IndirectPadArea = PadAreaDes;
					ReportSizingOutput( "EvaporativeCooler:Indirect:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
						"Design Size Celdek Pad Area [m2]", PadAreaDes );
				} else {
					if ( EvapCond( EvapCoolNum ).IndirectPadArea > 0.0 && PadAreaDes > 0.0 ) {
						PadAreaUser = EvapCond( EvapCoolNum ).IndirectPadArea;
						ReportSizingOutput( "EvaporativeCooler:Indirect:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
							"Design Size Celdek Pad Area [m2]", PadAreaDes, "User-Specified Celdek Pad Area [m2]", PadAreaUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( PadAreaDes - PadAreaUser ) / PadAreaUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeEvaporativeCooler:Indirect:CelDekPad: \nPotential issue with equipment sizing for " +
									EvapCond( EvapCoolNum ).EvapCoolerName );
								ShowContinueError( "User-Specified Celdek Pad Area " + RoundSigDigits( PadAreaUser, 2) + " [m2]" );
								ShowContinueError( "differs from Design Size Celdek Pad Area of " + RoundSigDigits( PadAreaDes, 2) + " [m2]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}

			IsAutoSize = false;
			if ( EvapCond( EvapCoolNum ).IndirectPadDepth == AutoSize ) {
				IsAutoSize = true;
			}
			// The following regression equation is used to determine pad depth,
			// assuming saturation effectiveness of 70% and face air velocity of 3m/s:
			// Effectiveness = 0.792714 + 0.958569D - 0.25193V - 1.03215D^2 + 0.0262659V^2 + 0.914869DV -
			// 1.48241VD^2 - 0.018992V^3D + 1.13137D^3V + 0.0327622V^3D^2 - 0.145384D^3V^2

			PadDepthDes = 0.17382;
			if ( IsAutoSize ) {
				EvapCond( EvapCoolNum ).IndirectPadDepth = PadDepthDes;
				ReportSizingOutput( "EvaporativeCooler:Indirect:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
					"Design Size Celdek Pad Depth [m]", PadDepthDes );
			} else {
				if ( EvapCond( EvapCoolNum ).IndirectPadDepth > 0.0 && PadDepthDes > 0.0 ) {
					PadDepthUser = EvapCond( EvapCoolNum ).IndirectPadDepth;
					ReportSizingOutput( "EvaporativeCooler:Indirect:CelDekPad", EvapCond( EvapCoolNum ).EvapCoolerName,
						"Design Size Celdek Pad Depth [m]", PadDepthDes, "User-Specified Celdek Pad Depth [m]", PadDepthUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( PadDepthDes - PadDepthUser ) / PadDepthUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeEvaporativeCooler:Indirect:CelDekPad: \nPotential issue with equipment sizing for " +
								EvapCond( EvapCoolNum ).EvapCoolerName );
							ShowContinueError( "User-Specified Celdek Pad Depth of" + RoundSigDigits( PadDepthUser, 2) + " [m]" );
							ShowContinueError( "differs from Design Size Celdek Pad Depth of " + RoundSigDigits( PadDepthDes, 2 ) + " [m]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}

		if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial ) {
			// secondary air fan sizing: Secondary flow Rate (m3/s) * Fan Flow Sizing Factor (W/(m3/s)
			if ( EvapCond( EvapCoolNum ).IndirectFanPower == AutoSize ) {
				EvapCond( EvapCoolNum ).IndirectFanPower = EvapCond( EvapCoolNum ).IndirectVolFlowRate * EvapCond( EvapCoolNum ).FanSizingSpecificPower;
				ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName, "Secondary Fan Power [W]", EvapCond( EvapCoolNum ).IndirectFanPower );
			}
			// recirculating water pump sizing: Secondary flow Rate (m3/s) * Pump Sizing Factor (W/(m3/s)
			if ( EvapCond( EvapCoolNum ).IndirectRecircPumpPower == AutoSize ) {
				EvapCond( EvapCoolNum ).IndirectRecircPumpPower = EvapCond( EvapCoolNum ).IndirectVolFlowRate * EvapCond( EvapCoolNum ).RecircPumpSizingFactor;
				ReportSizingOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName, "Recirculating Pump Power [W]", EvapCond( EvapCoolNum ).IndirectRecircPumpPower );
			}
		}

		if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerDirectResearchSpecial ) {
			// recirculating water pump sizing: Primary Air Design flow Rate (m3/s) * Pump Sizing Factor (W/(m3/s)
			if ( EvapCond( EvapCoolNum ).RecircPumpPower == AutoSize ) {
				EvapCond( EvapCoolNum ).RecircPumpPower = EvapCond( EvapCoolNum ).VolFlowRate * EvapCond( EvapCoolNum ).RecircPumpSizingFactor;
				ReportSizingOutput( "EvaporativeCooler:Direct:ResearchSpecial", EvapCond( EvapCoolNum ).EvapCoolerName, "Recirculating Pump Power [W]", EvapCond( EvapCoolNum ).RecircPumpPower );
			}
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
		//       RE-ENGINEERED  October 2014, B Nigusse, added dry and wet operating modes
		//                      and secondary air flow control

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine models a "special" cooler that allows high effectiveness and controls

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// copied CalcWetIndirectEvapCooler as template for new cooler

		// Using/Aliasing
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutBaroPress;
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:


		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//REAL(r64) Variables
		Real64 SecondaryInletDryBulbTemp; // entering drybulb for secondary/purge side
		Real64 SecondaryInletWetBulbTemp; // entering wet bulb for secondary/purge side
		Real64 SecondaryInletDewPointTemp; // entering dewpoint for secondary/purge side
		Real64 SecondaryInletHumRatio; // entering humidity ratio for secondary/purge side
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
				SecondaryInletDryBulbTemp = EvapCond( EvapCoolNum ).SecInletTemp;
				SecondaryInletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).SecInletTemp, EvapCond( EvapCoolNum ).SecInletHumRat, OutBaroPress );
				SecondaryInletDewPointTemp = PsyTdpFnTdbTwbPb( EvapCond( EvapCoolNum ).SecInletTemp, SecondaryInletWetBulbTemp, OutBaroPress );
				SecondaryInletHumRatio = EvapCond( EvapCoolNum ).SecInletHumRat;

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
					SecondaryInletDryBulbTemp = TertTemp;
					SecondaryInletWetBulbTemp = PsyTwbFnTdbWPb( TertTemp, TertHumRate, OutBaroPress );
					SecondaryInletDewPointTemp = PsyTdpFnTdbTwbPb( TertTemp, SecondaryInletWetBulbTemp, OutBaroPress );
					SecondaryInletHumRatio = TertHumRate;
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
					SecondaryInletDryBulbTemp = PurgeTemp;
					SecondaryInletWetBulbTemp = PsyTwbFnTdbWPb( PurgeTemp, PurgeHumRat, OutBaroPress );
					SecondaryInletDewPointTemp = PsyTdpFnTdbTwbPb( PurgeTemp, SecondaryInletWetBulbTemp, OutBaroPress );
					SecondaryInletHumRatio = PurgeHumRat;
				}
			}
			if ( EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag ) {
				// addvanced mode: runs either in dry or wet depending on the entering conditions
				CalcIndirectResearchSpecialEvapCoolerAdvanced( EvapCoolNum, SecondaryInletDryBulbTemp, SecondaryInletWetBulbTemp, SecondaryInletDewPointTemp, SecondaryInletHumRatio );

			} else {

				TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
				TEDB = EvapCond( EvapCoolNum ).InletTemp;
				PartLoad = EvapCond( EvapCoolNum ).PartLoadFract;

				//***************************************************************************
				//   TEMP LEAVING DRY BULB IS CALCULATED FROM A SIMPLE WET BULB APPROACH
				//   MODEL GIVEN THE INDIRECT STAGE EFFICIENCY.
				//   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE INDIRECT STAGE.
				//***************************************************************************
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
				//                  POWER OF THE SECONDARY AIR FAN with part load factor applied (assumes const efficiency)
				EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectVolFlowRate * EvapCond( EvapCoolNum ).FanSizingSpecificPower * PartLoad;

				//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
				//                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
				//Add the pump energy to the total Evap Cooler energy comsumption
				EvapCond( EvapCoolNum ).EvapCoolerPower += EvapCond( EvapCoolNum ).IndirectRecircPumpPower * PartLoad;

				//***************************************************************************
				//                  CALCULATE THE WET BULB TEMP in the primary system air using PSYCH ROUTINES
				// There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
				EvapCond( EvapCoolNum ).OuletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).InletHumRat, OutBaroPress );
				//***************************************************************************
				//                  CALCULATE other outlet propertiesusing PSYCH ROUTINES
				EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;

				EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );
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

			}

		} else {
			// The evap cooler is not running and does not change conditions from inlet to outlet
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;
			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;
			EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;
			EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;
			EvapCond( EvapCoolNum ).EvapCoolerPower = 0.0;
			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = 0.0;
		}


		// all of the mass flowrates are not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletMassFlowRate = EvapCond( EvapCoolNum ).InletMassFlowRate;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMaxAvail;
		EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail = EvapCond( EvapCoolNum ).InletMassFlowRateMinAvail;
		// set secondary air side inlet mass flow rate to the outlet node
		EvapCond( EvapCoolNum ).SecOutletMassFlowRate = EvapCond( EvapCoolNum ).SecInletMassFlowRate;
		Node( EvapCond( EvapCoolNum ).SecondaryInletNode ).MassFlowRate = EvapCond( EvapCoolNum ).SecInletMassFlowRate;

		// the pressure is not changed across the evap cooler
		EvapCond( EvapCoolNum ).OutletPressure = EvapCond( EvapCoolNum ).InletPressure;

	}

	void
	CalcIndirectResearchSpecialEvapCoolerAdvanced(
		int const EvapCoolNum,
		Real64 const InletDryBulbTempSec,
		Real64 const InletWetBulbTempSec,
		Real64 const InletDewPointTempSec,
		Real64 const InletHumRatioSec
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Bigusse
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine models indirect evaporative cooler with variable effectiveness for wet and dry
		// operating modes depending on entering conditions

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutBaroPress;
		using CurveManager::CurveValue;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:


		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const TempTol( 0.01 ); // convergence tollerance

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TEDB; // Entering Dry Bulb Temperature
		Real64 TEWB; // Entering Wet Bulb Temperature
		Real64 BoundTemp; // temperature limit for outlet
		Real64 PartLoad;
		Real64 SecRho;
		Real64 TdbOutSysWetMin; // system( primary ) air drybulb outlet temperature minimum based on wet coil
		Real64 TdbOutSysDryMin; // system (primary) air drybulb outlet temperature minimum based on dry coil
		Real64 SysTempSetPoint; // evaporative cooler outlet setpoint temperature, drybulb
		Real64 MassFlowRateSecMax; // Design secondary air mass flow rate
		Real64 AirMassFlowSec; // current secondary air mass flow rate
		Real64 AirMassFlowSecDry; // current secondary air mass flow rate in dry mode
		Real64 AirMassFlowSecWet; // current secondary air mass flow rate in wet mode
		Real64 FlowRatioSec; // secondary air flow ratio in dry and wet mode
		Real64 FlowRatioSecDry; // current secondary air mass flow ratio in dry mode
		Real64 FlowRatioSecWet; // current secondary air mass flow ratio in wet mode
		Real64 EvapCoolerTotalElectricPowerDry; // evaporative cooler current total electric power drawn
		Real64 EvapCoolerTotalElectricPowerWet; // evaporative cooler current total electric power drawn
		int SolFla; // Flag of solver
		Array1D< Real64 > Par( 6 ); // Parameter array passed to solver
		Real64 QHXLatent; // evaporative cooler latent heat transfer rate
		Real64 hfg; // latent heat of vaporization of water at the secondary air inlet condition

		Real64 QHX; // Q Across Sec HX in Watts or J/sec
		Real64 RhoWater;
		Real64 RhoAir; // Density of the primary side air
		Real64 MassFlowRateSecMin;
		static Real64 BlowDownVdot( 0.0 );
		static Real64 DriftVdot( 0.0 );
		static Real64 EvapVdot( 0.0 );

		FlowRatioSecDry = 0.0;
		FlowRatioSecWet = 0.0;
		EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = None;
		TEDB = EvapCond( EvapCoolNum ).InletTemp;
		TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
		SysTempSetPoint = EvapCond( EvapCoolNum ).DesiredOutletTemp;
		SecRho = PsyRhoAirFnPbTdbW( OutBaroPress, InletDryBulbTempSec, InletHumRatioSec );
		MassFlowRateSecMax = SecRho * EvapCond( EvapCoolNum ).IndirectVolFlowRate;
		CalcIndirectRDDEvapCoolerOutletTemp( EvapCoolNum, WetFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec );
		TdbOutSysWetMin = EvapCond( EvapCoolNum ).OutletTemp;
		CalcIndirectRDDEvapCoolerOutletTemp( EvapCoolNum, DryFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec );
		TdbOutSysDryMin = EvapCond( EvapCoolNum ).OutletTemp;

		// Now determine the operating modes of indirect evaporative cooler research special. There are five allowed operating modes
		if ( ( TEDB <=  SysTempSetPoint ) || ( TEDB > EvapCond( EvapCoolNum ).MaxOATDBEvapCooler && InletWetBulbTempSec > EvapCond( EvapCoolNum ).MaxOATWBEvapCooler ) || ( TEDB <= InletDryBulbTempSec ) ) {
			EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = None;
		} else if ( ( InletDryBulbTempSec < EvapCond( EvapCoolNum ).MinOATDBEvapCooler && TdbOutSysDryMin < SysTempSetPoint ) ) {
			EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = DryModulated; // dry mode capacity modulated
		} else if ( ( InletDryBulbTempSec < EvapCond( EvapCoolNum ).MinOATDBEvapCooler && SysTempSetPoint <= TdbOutSysDryMin ) ) {
			EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = DryFull; // dry mode in full capacity
		} else if ( ( InletDryBulbTempSec >= EvapCond( EvapCoolNum ).MinOATDBEvapCooler && InletWetBulbTempSec < EvapCond( EvapCoolNum ).MaxOATWBEvapCooler && SysTempSetPoint <= TdbOutSysWetMin ) ) {
			EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = WetFull; // wet mode in full capacity
		} else if ( ( InletDryBulbTempSec >= EvapCond( EvapCoolNum ).MinOATDBEvapCooler && InletWetBulbTempSec < EvapCond( EvapCoolNum ).MaxOATWBEvapCooler && TdbOutSysWetMin < SysTempSetPoint ) ) { // && SysTempSetPoint < TdbOutSysDryMin
			EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = WetModulated; // wet mode capacity modulated
		} else if ( ( InletDryBulbTempSec >= EvapCond( EvapCoolNum ).MinOATDBEvapCooler && InletDryBulbTempSec < EvapCond( EvapCoolNum ).MaxOATDBEvapCooler && InletWetBulbTempSec < EvapCond( EvapCoolNum ).MaxOATWBEvapCooler && SysTempSetPoint < TdbOutSysDryMin && TdbOutSysWetMin < SysTempSetPoint ) ) {
			EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = DryWetModulated; // modulated in dry and wet mode, and the lower total power will be used
		} else {
			EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = None;  // this condition should not happen unless the bounds do not cover all combinations possible
		}
		MassFlowRateSecMin = 0.0;
		AirMassFlowSec = MassFlowRateSecMax;
		PartLoad = EvapCond( EvapCoolNum ).PartLoadFract;
		{ auto const SELECT_CASE_var( EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode );
		if ( SELECT_CASE_var == DryModulated ) {
			Par( 1 ) = double( EvapCoolNum );
			Par( 2 ) = double( DryModulated );
			Par( 3 ) = SysTempSetPoint;
			Par( 4 ) = InletDryBulbTempSec;
			Par( 5 ) = InletWetBulbTempSec;
			Par( 6 ) = InletHumRatioSec;
			SolveRegulaFalsi( TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par );
			// if the numerical inversion failed, issue error messages.
			if ( SolFla == -1 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationLimit == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "  Iteration limit [" + RoundSigDigits( MaxIte ) + "] exceeded in calculating secondary air mass flow rate" );
						ShowContinueError( "  Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationLimit );
				}
			} else if ( SolFla == -2 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationFailed == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "...Bad secondary air mass flow rate limits" );
						ShowContinueError( "...Given minimum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMin, 3 ) + " kg/s" );
						ShowContinueError( "...Given maximum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMax, 3 ) + " kg/s" );
						ShowContinueError( " Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationFailed );
				}
			}
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = AirMassFlowSec;
			if ( AirMassFlowSec > 0.0 ) {
				if ( MassFlowRateSecMax > 0.0 ) {
					FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
				} else {
					FlowRatioSec = 0.0;
				}
			} else {
				FlowRatioSec = 0.0;
			}
			EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower( EvapCoolNum, DryModulated, FlowRatioSec );
			EvapCond( EvapCoolNum ).IECOperatingStatus = 1;
		} else if ( SELECT_CASE_var == DryFull ) {
			CalcIndirectRDDEvapCoolerOutletTemp( EvapCoolNum, DryFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec );
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = MassFlowRateSecMax;
			FlowRatioSec = 1.0;
			EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower( EvapCoolNum, DryFull, FlowRatioSec );
			EvapCond( EvapCoolNum ).IECOperatingStatus = 1;
		} else if ( SELECT_CASE_var == DryWetModulated ) {
			Par( 1 ) = double( EvapCoolNum );
			Par( 3 ) = SysTempSetPoint;
			Par( 4 ) = InletDryBulbTempSec;
			Par( 5 ) = InletWetBulbTempSec;
			Par( 6 ) = InletHumRatioSec;
			// get dry operation performance first
			Par( 2 ) = double( DryModulated );
			SolveRegulaFalsi( TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par );
			// if the numerical inversion failed, issue error messages.
			if ( SolFla == -1 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationLimit == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "  Iteration limit [" + RoundSigDigits( MaxIte ) + "] exceeded in calculating secondary air mass flow rate" );
						ShowContinueError( "  Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationLimit );
				}
			} else if ( SolFla == -2 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationFailed == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "...Bad secondary air mass flow rate limits" );
						ShowContinueError( "...Given minimum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMin, 3 ) + " kg/s" );
						ShowContinueError( "...Given maximum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMax, 3 ) + " kg/s" );
						ShowContinueError( " Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationFailed );
				}
			}
			if ( AirMassFlowSec > 0.0 ) {
				if ( MassFlowRateSecMax > 0.0 ) {
					FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
				} else {
					FlowRatioSec = 0.0;
				}
			} else {
				FlowRatioSec = 0.0;
			}
			FlowRatioSecDry = FlowRatioSec;
			AirMassFlowSecDry = AirMassFlowSec;
			EvapCoolerTotalElectricPowerDry = IndEvapCoolerPower( EvapCoolNum, DryModulated, FlowRatioSecDry );
			// get wet operation performance
			Par( 2 ) = double( WetModulated );
			SolveRegulaFalsi( TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par );
			// if the numerical inversion failed, issue error messages.
			if ( SolFla == -1 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationLimit == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "  Iteration limit [" + RoundSigDigits( MaxIte ) + "] exceeded in calculating secondary air mass flow rate" );
						ShowContinueError( "  Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationLimit );
				}
			} else if ( SolFla == -2 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationFailed == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "...Bad secondary air mass flow rate limits" );
						ShowContinueError( "...Given minimum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMin, 3 ) + " kg/s" );
						ShowContinueError( "...Given maximum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMax, 3 ) + " kg/s" );
						ShowContinueError( " Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationFailed );
				}
			}
			if ( AirMassFlowSec > 0.0 ) {
				if ( MassFlowRateSecMax > 0.0 ) {
					FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
				} else {
					FlowRatioSec = 0.0;
				}
			} else {
				FlowRatioSec = 0.0;
			}
			FlowRatioSecWet = FlowRatioSec;
			AirMassFlowSecWet = AirMassFlowSec;
			EvapCoolerTotalElectricPowerWet = IndEvapCoolerPower( EvapCoolNum, WetModulated, FlowRatioSecWet );
			// compare the dry and wet operation total electric power
			if ( EvapCoolerTotalElectricPowerDry < EvapCoolerTotalElectricPowerWet ) {
				EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = DryModulated;
				FlowRatioSec = FlowRatioSecDry;
				EvapCond( EvapCoolNum ).SecInletMassFlowRate = AirMassFlowSecDry;
				CalcIndirectRDDEvapCoolerOutletTemp( EvapCoolNum, DryModulated, AirMassFlowSecDry, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec );
				EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower( EvapCoolNum, DryModulated, FlowRatioSec );
				EvapCond( EvapCoolNum ).IECOperatingStatus = 1;
			} else {
				EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode = WetModulated;
				FlowRatioSec = FlowRatioSecWet;
				EvapCond( EvapCoolNum ).SecInletMassFlowRate = AirMassFlowSecWet;
				CalcIndirectRDDEvapCoolerOutletTemp( EvapCoolNum, WetModulated, AirMassFlowSecWet, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec );
				EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower( EvapCoolNum, WetModulated, FlowRatioSec );
				EvapCond( EvapCoolNum ).IECOperatingStatus = 2;
			}
		} else if ( SELECT_CASE_var == WetModulated ) {
			Par( 1 ) = double( EvapCoolNum );
			Par( 2 ) = double( WetModulated );
			Par( 3 ) = SysTempSetPoint;
			Par( 4 ) = InletDryBulbTempSec;
			Par( 5 ) = InletWetBulbTempSec;
			Par( 6 ) = InletHumRatioSec;
			SolveRegulaFalsi( TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par );
			// if the numerical inversion failed, issue error messages.
			if ( SolFla == -1 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationLimit == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "  Iteration limit [" + RoundSigDigits( MaxIte ) + "] exceeded in calculating secondary air mass flow rate" );
						ShowContinueError( "  Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationLimit );
				}
			} else if ( SolFla == -2 ) {
				if ( !WarmupFlag ) {
					if ( EvapCond( EvapCoolNum ).IterationFailed == 0 ) {
						ShowSevereError( "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "...Bad secondary air mass flow rate limits" );
						ShowContinueError( "...Given minimum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMin, 3 ) + " kg/s" );
						ShowContinueError( "...Given maximum secondary air mass flow rate=" + RoundSigDigits( MassFlowRateSecMax, 3 ) + " kg/s" );
						ShowContinueError( " Simulation continues" );
					}
					ShowRecurringWarningErrorAtEnd( "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " + EvapCond( EvapCoolNum ).EvapCoolerName, EvapCond( EvapCoolNum ).IterationFailed );
				}
			}
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = AirMassFlowSec;
			if ( AirMassFlowSec > 0.0 ) {
				if ( MassFlowRateSecMax > 0.0 ) {
					FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
				} else {
					FlowRatioSec = 0.0;
				}
			} else {
				FlowRatioSec = 0.0;
			}
			EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower( EvapCoolNum, WetModulated, FlowRatioSec );
			EvapCond( EvapCoolNum ).IECOperatingStatus = 2;
		} else if ( SELECT_CASE_var == WetFull ) {
			CalcIndirectRDDEvapCoolerOutletTemp( EvapCoolNum, WetFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec );
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = MassFlowRateSecMax;
			FlowRatioSec = 1.0;
			EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower( EvapCoolNum, WetFull, FlowRatioSec );
			EvapCond( EvapCoolNum ).IECOperatingStatus = 2;
		}
		}
		if ( PartLoad == 1.0 ) {
			if ( EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode == WetModulated || EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode == WetFull ) {
				BoundTemp = TEDB - EvapCond( EvapCoolNum ).DPBoundFactor * ( TEDB - InletDewPointTempSec );
				if ( EvapCond( EvapCoolNum ).OutletTemp < BoundTemp ) {
					EvapCond( EvapCoolNum ).OutletTemp = BoundTemp;
					EvapCond( EvapCoolNum ).DewPointBoundFlag = 1;
				}
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
		if ( EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode != None ) {
			// There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
			EvapCond( EvapCoolNum ).OuletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).InletHumRat, OutBaroPress );
			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;
			EvapCond( EvapCoolNum ).OutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).OutletTemp, EvapCond( EvapCoolNum ).OutletHumRat );
			RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, EvapCond( EvapCoolNum ).InletTemp, EvapCond( EvapCoolNum ).InletHumRat );
			QHX = EvapCond( EvapCoolNum ).VolFlowRate * RhoAir * ( EvapCond( EvapCoolNum ).InletEnthalpy - EvapCond( EvapCoolNum ).OutletEnthalpy );
			if ( QHX > SmallLoad ) {
				// get secondary air outlet condition
				CalcSecondaryAirOutletCondition( EvapCoolNum, EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode, EvapCond( EvapCoolNum ).SecInletMassFlowRate, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec, QHX, QHXLatent );
				RhoWater = RhoH2O( OutDryBulbTemp ); // this if it is at the outside air inlet node condition
				hfg = PsyHfgAirFnWTdb( InletHumRatioSec, InletDryBulbTempSec );
				EvapVdot = ( QHXLatent ) / ( hfg * RhoWater );
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
				EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
				EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;
				EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;
				EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;
				EvapCond( EvapCoolNum ).EvapCoolerPower = 0.0;
				EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;
				EvapCond( EvapCoolNum ).SecInletMassFlowRate = 0.0;
				EvapCond( EvapCoolNum ).IECOperatingStatus = 0;
				EvapCond( EvapCoolNum ).StageEff = 0.0;
				CalcSecondaryAirOutletCondition( EvapCoolNum, EvapCond( EvapCoolNum ).EvapCoolerRDDOperatingMode, 0.0, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec, QHX, QHXLatent );
			}

		} else {
			// The evap cooler is not running and does not change conditions from inlet to outlet
			EvapCond( EvapCoolNum ).OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
			EvapCond( EvapCoolNum ).OuletWetBulbTemp = EvapCond( EvapCoolNum ).InletWetBulbTemp;
			EvapCond( EvapCoolNum ).OutletHumRat = EvapCond( EvapCoolNum ).InletHumRat;
			EvapCond( EvapCoolNum ).OutletEnthalpy = EvapCond( EvapCoolNum ).InletEnthalpy;
			EvapCond( EvapCoolNum ).SecOutletTemp = EvapCond( EvapCoolNum ).SecInletTemp;
			EvapCond( EvapCoolNum ).SecOutletHumRat = EvapCond( EvapCoolNum ).SecInletHumRat;
			EvapCond( EvapCoolNum ).SecOutletEnthalpy = EvapCond( EvapCoolNum ).SecInletEnthalpy;
			EvapCond( EvapCoolNum ).SecOutletMassFlowRate = EvapCond( EvapCoolNum ).SecInletMassFlowRate;
			EvapCond( EvapCoolNum ).EvapCoolerEnergy = 0.0;
			EvapCond( EvapCoolNum ).EvapCoolerPower = 0.0;
			EvapCond( EvapCoolNum ).EvapWaterConsumpRate = 0.0;
			EvapCond( EvapCoolNum ).SecInletMassFlowRate = 0.0;
			EvapCond( EvapCoolNum ).IECOperatingStatus = 0;
			EvapCond( EvapCoolNum ).StageEff = 0.0;
		}
	}

	Real64
	CalcEvapCoolRDDSecFlowResidual(
		Real64 const AirMassFlowSec, // secondary air mass flow rate in kg/s
		Array1< Real64 > const & Par // Par(2) is desired outlet temperature of Evap Cooler
	)
	{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         B. Nigusse
			//       DATE WRITTEN   Sep 2014
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Indirect research special evaporative cooler part load operation:
			// determines the secondary air flow rate

			// METHODOLOGY EMPLOYED:
			// Uses regula falsi to minimize setpoint temperature residual to by varying the
			// secondary air flow rate.

			// REFERENCES:
			//

			// Using/Aliasing
			// na

			// SUBROUTINE PARAMETER DEFINITIONS:
			//na

			// INTERFACE BLOCK SPECIFICATIONS
			// na

			// DERIVED TYPE DEFINITIONS
			// na

			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			int EvapCoolIndex; // evaporative cooler index
			int DryOrWetOperatingMode; // provides index for dry mode and wet mode operation
			Real64 EDBTSecAirSide; // current entering dry bulb temperature of the secondary side
			Real64 EWBTSecAirSide; // current entering wet bulb temperature of the secondary side
			Real64 EHumRatSecAirSide; // current entering humidity ratio of the secondary side
			Real64 OutletAirTemp; // evap Coler outlet air temperature
			Real64 SysTempSetPoint; // evaporative cooler outlet setpoint temperature, drybulb
			Real64 Residuum; // Residual to be minimized to zero

			EvapCoolIndex = int( Par( 1 ) );
			DryOrWetOperatingMode = int( Par( 2 ) );
			SysTempSetPoint = Par( 3 );
			EDBTSecAirSide = Par( 4 );
			EWBTSecAirSide = Par( 5 );
			EHumRatSecAirSide = Par( 6 );
			EvapCond( EvapCoolIndex ).SecInletMassFlowRate = AirMassFlowSec;
			CalcIndirectRDDEvapCoolerOutletTemp( EvapCoolIndex, DryOrWetOperatingMode, AirMassFlowSec, EDBTSecAirSide, EWBTSecAirSide, EHumRatSecAirSide );
			OutletAirTemp = EvapCond( EvapCoolIndex ).OutletTemp;
			Residuum = SysTempSetPoint - OutletAirTemp;

			return Residuum;
	}

	void
	CalcIndirectRDDEvapCoolerOutletTemp(
		int const EvapCoolNum,
		int const DryOrWetOperatingMode,
		Real64 const AirMassFlowSec,
		Real64 const EDBTSec,
		Real64 const EWBTSec,
		Real64 const EHumRatSec
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse
		//       DATE WRITTEN   Sep 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Indirect research special evaporative cooler perfomance:
		// determines the IEC primary air outlet temperature

		// METHODOLOGY EMPLOYED:
		// Uses effectiveness and energy balance equations to determine
		// primary air outlet temperature.  The dry and wet effectiveness
		// values are used depending on operating modes.

		// REFERENCES:
		//

		// Using/Aliasing
		//using DataHVACGlobals::TempControlTol;
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OutletTemp; // evaporative cooler current outlet air drybulb temperature
		Real64 RhoAirSec; // density of secondary air at inlet condition
		Real64 RhoAirSys; // density of primary air at inlet condition
		Real64 EffectivenessDry; // dry coil effectiveness
		Real64 EffectivenessWet; // wet coil effectiveness
		Real64 FlowRatio; // flow ratio based on current to the design of secondary air flow rate
		Real64 EffModDryMode; // dry mode effectiveness modifier for flow ratio
		Real64 EffModWetMode; // wet mode effectiveness modifier for flow ratio
		Real64 CapFlowSys; // capacity flow (massFlowRate * Specific Heat) of primary air system
		Real64 CapFlowSec; // capacity flow (massFlowRate * Specific Heat) of secondary system
		Real64 CpAirSec; // specific heat of secondary air at inlet condition
		Real64 CpAirSys; // specific heat of primary air at inlet condition

		Real64 QHXRate; // total heat transfer rate
		Real64 OutletTempSec; // secondary air outlet temperature
		Real64 SecOutletAirHumRat; // secondary air humidity ratio at constant temperature (Pure mass transfer)
		Real64 SecOutletEnthalpy; // secondary air outlet enthalpy

		if ( EvapCond( EvapCoolNum ).InletMassFlowRate > 0.0 ) {
			FlowRatio = AirMassFlowSec / EvapCond( EvapCoolNum ).InletMassFlowRate; // ratio of current secondary air flow to current primary air flow
		} else {
			FlowRatio = 1.0;
		}
		if ( AirMassFlowSec > 0.0 ) {
			RhoAirSec = PsyRhoAirFnPbTdbW( OutBaroPress, EDBTSec, EHumRatSec );
			RhoAirSys = PsyRhoAirFnPbTdbW( OutBaroPress, EvapCond( EvapCoolNum ).InletTemp, EvapCond( EvapCoolNum ).InletHumRat );
			if ( DryOrWetOperatingMode == DryModulated || DryOrWetOperatingMode == DryFull ) {
				if ( EvapCond( EvapCoolNum ).DrybulbEffecCurveIndex > 0 ) {
					EffModDryMode = CurveValue( EvapCond( EvapCoolNum ).DrybulbEffecCurveIndex, FlowRatio );
				} else {
					EffModDryMode = 1.0;
				}
				EffectivenessDry = EvapCond( EvapCoolNum ).DryCoilMaxEfficiency * EffModDryMode;
				EvapCond( EvapCoolNum ).StageEff = EffectivenessDry;
				OutletTemp = EvapCond( EvapCoolNum ).InletTemp - EffectivenessDry * ( EvapCond( EvapCoolNum ).InletTemp - EDBTSec );
				if ( OutletTemp > EvapCond( EvapCoolNum ).InletTemp ) {
					OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
				}
				CpAirSys = PsyCpAirFnWTdb( EvapCond( EvapCoolNum ).InletHumRat, EvapCond( EvapCoolNum ).InletTemp );
				CapFlowSys = EvapCond( EvapCoolNum ).InletMassFlowRate * CpAirSys;
				QHXRate = CapFlowSys * ( EvapCond( EvapCoolNum ).InletTemp - OutletTemp );
				CpAirSec = PsyCpAirFnWTdb( EHumRatSec, EDBTSec );
				CapFlowSec = AirMassFlowSec * CpAirSec;
				OutletTempSec = EDBTSec + QHXRate / CapFlowSec;
				if ( OutletTempSec >= EvapCond( EvapCoolNum ).InletTemp ) {
					OutletTempSec = EvapCond( EvapCoolNum ).InletTemp - 0.2;
					QHXRate = CapFlowSec * ( OutletTempSec - EDBTSec );
					OutletTemp = EvapCond( EvapCoolNum ).InletTemp - QHXRate / CapFlowSys;
				}
				EvapCond( EvapCoolNum ).SecOutletTemp = OutletTempSec;
			} else if ( DryOrWetOperatingMode == WetModulated || DryOrWetOperatingMode == WetFull ) {
				if ( EvapCond( EvapCoolNum ).WetbulbEffecCurveIndex > 0 ) {
					EffModWetMode = CurveValue( EvapCond( EvapCoolNum ).WetbulbEffecCurveIndex, FlowRatio );
				} else {
					EffModWetMode = 1.0;
				}
				EffectivenessWet = EvapCond( EvapCoolNum ).WetCoilMaxEfficiency * EffModWetMode;
				EvapCond( EvapCoolNum ).StageEff = EffectivenessWet;
				OutletTemp = EvapCond( EvapCoolNum ).InletTemp - EffectivenessWet * ( EvapCond( EvapCoolNum ).InletTemp - EWBTSec );
				if ( OutletTemp > EvapCond( EvapCoolNum ).InletTemp ) {
					OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
				}
				CpAirSys = PsyCpAirFnWTdb( EvapCond( EvapCoolNum ).InletHumRat, EvapCond( EvapCoolNum ).InletTemp );
				CapFlowSys = EvapCond( EvapCoolNum ).InletMassFlowRate * CpAirSys;
				QHXRate = CapFlowSys * ( EvapCond( EvapCoolNum ).InletTemp - OutletTemp );
				SecOutletEnthalpy = EvapCond( EvapCoolNum ).SecInletEnthalpy + QHXRate / AirMassFlowSec;
				SecOutletAirHumRat = PsyWFnTdbH( EDBTSec, SecOutletEnthalpy );  // assumes constant temperature moisture addition
				// we may need check based on maximum allowed humidity ratio
				EvapCond( EvapCoolNum ).SecOutletTemp = EDBTSec;
				EvapCond( EvapCoolNum ).SecOutletHumRat = SecOutletAirHumRat;
				EvapCond( EvapCoolNum ).SecOutletEnthalpy = SecOutletEnthalpy;
			} else {
				OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
				EvapCond( EvapCoolNum ).StageEff = 0.0;
			}
		} else {
			OutletTemp = EvapCond( EvapCoolNum ).InletTemp;
			EvapCond( EvapCoolNum ).StageEff = 0.0;

		}
        // set results to into output variables
		EvapCond( EvapCoolNum ).OutletTemp = OutletTemp;

	}

	void
	CalcSecondaryAirOutletCondition(
		int const EvapCoolNum,
		int const OperatingMode,
		Real64 const AirMassFlowSec,
		Real64 const EDBTSec,
		Real64 const EWBTSec,
		Real64 const EHumRatSec,
		Real64 const QHXTotal,
		Real64 & QHXLatent
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse
		//       DATE WRITTEN   Oct 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Indirect research special evaporative cooler: determines the secondary air outlet conditions

		// METHODOLOGY EMPLOYED:
		// applies energy balance equations to determine the secondary air outlet condition
		// For wt operations assumes the secondary air leaves at at inlet temperature, i.e.,
		// latent heat transfer only.  For dry operation the humdity ratio remains constant.

		// REFERENCES:
		// CalculateWaterUseage routine of cooling towers for wet operation mode

		// Using/Aliasing
		using Psychrometrics::PsyHfgAirFnWTdb;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyWFnTdbH;
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
		Real64 SecOutletAirHumRat; // secondary air humidity ratio at the outlet node
		Real64 SecOutletEnthalpy; // secondary air outlet enthalpy
		Real64 CpAirSec; // specific heat of secondary air at inlet condition
		Real64 hfg; // secondary air side enthaly of evaporation

		QHXLatent = 0.0;
		if ( AirMassFlowSec > 0.0 ) {
			if ( (OperatingMode == DryModulated || OperatingMode == DryFull ) ) {
				EvapCond( EvapCoolNum ).SecOutletHumRat = EHumRatSec;
				CpAirSec = PsyCpAirFnWTdb( EHumRatSec, EDBTSec );
				EvapCond( EvapCoolNum ).SecOutletTemp = EDBTSec + QHXTotal / AirMassFlowSec / CpAirSec;
				EvapCond( EvapCoolNum ).SecOutletEnthalpy = PsyHFnTdbW( EvapCond( EvapCoolNum ).SecOutletTemp, EHumRatSec );
				EvapCond( EvapCoolNum ).SecOuletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).SecOutletTemp, EHumRatSec, OutBaroPress );
			} else if ( ( OperatingMode == WetModulated || OperatingMode == WetFull ) ) {
				SecOutletEnthalpy = EvapCond( EvapCoolNum ).SecInletEnthalpy + QHXTotal / AirMassFlowSec;
				SecOutletAirHumRat = PsyWFnTdbH( EDBTSec, SecOutletEnthalpy );  // assumes a constant temperature moisture addition
				EvapCond( EvapCoolNum ).SecOutletTemp = EDBTSec;
				EvapCond( EvapCoolNum ).SecOutletHumRat = SecOutletAirHumRat;
				EvapCond( EvapCoolNum ).SecOutletEnthalpy = SecOutletEnthalpy;
				EvapCond( EvapCoolNum ).SecOuletWetBulbTemp = PsyTwbFnTdbWPb( EvapCond( EvapCoolNum ).SecOutletTemp, SecOutletAirHumRat, OutBaroPress );
				hfg = PsyHfgAirFnWTdb( EHumRatSec, EDBTSec );
				QHXLatent = min( QHXTotal, AirMassFlowSec *  ( SecOutletAirHumRat - EHumRatSec ) * hfg );
			} else {
				// set results to into output variables
				EvapCond( EvapCoolNum ).SecOutletTemp = EDBTSec;
				EvapCond( EvapCoolNum ).SecOuletWetBulbTemp = EWBTSec;
				EvapCond( EvapCoolNum ).SecOutletHumRat = EHumRatSec;
				EvapCond( EvapCoolNum ).SecOutletEnthalpy = EvapCond( EvapCoolNum ).SecInletEnthalpy;
			}
		} else {
			EvapCond( EvapCoolNum ).SecOutletTemp = EDBTSec;
			EvapCond( EvapCoolNum ).SecOuletWetBulbTemp = EWBTSec;
			EvapCond( EvapCoolNum ).SecOutletHumRat = EHumRatSec;
			EvapCond( EvapCoolNum ).SecOutletEnthalpy = EvapCond( EvapCoolNum ).SecInletEnthalpy;
		}
	}

	Real64
	IndEvapCoolerPower(
		int const EvapCoolIndex, // Unit index
		int const DryWetMode, // dry or wet operating mode of evaporator cooler
		Real64 const FlowRatio // secondary air flow fraction
	)
	{

			// SUBROUTINE INFORMATION:
			//       AUTHOR         B. Nigusse
			//       DATE WRITTEN   Sep 2014
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Calculates the Indirect Evaporative Cooler Total Electric Power

			// METHODOLOGY EMPLOYED:
			// Scales the design fan and pump power depending on secondary air flow fraction
			// and sums the two to determine the evaporative cooler total electric power.

			// REFERENCES:
			// na

			// Using/Aliasing
			using CurveManager::CurveValue;

			// Locals
			// SUBROUTINE ARGUMENT DEFINITIONS:

			// SUBROUTINE PARAMETER DEFINITIONS:
			// na

			// INTERFACE BLOCK SPECIFICATIONS
			// na

			// DERIVED TYPE DEFINITIONS
			// na

			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			Real64 FanPowerModCurveValue; // fan power modifier curve value
			Real64 PumpPowerModCurveValue; // fan power modifier curve value
			Real64 EvapCoolertotalPower; // current evapoartive cooler total electric power

			EvapCoolertotalPower = 0.0;
			if ( FlowRatio > 0.0 ) {
				if ( EvapCond( EvapCoolIndex ).FanPowerModifierCurveIndex > 0 ) {
					FanPowerModCurveValue = CurveValue( EvapCond( EvapCoolIndex ).FanPowerModifierCurveIndex, FlowRatio );
				} else {
					FanPowerModCurveValue = EvapCond( EvapCoolIndex ).PartLoadFract;
				}
				EvapCoolertotalPower += EvapCond( EvapCoolIndex ).IndirectFanPower * FanPowerModCurveValue;
				if ( DryWetMode == WetModulated || DryWetMode == WetFull ) {
					//Add the pump power to the total Evap Cooler power for wet operating mode
					if ( EvapCond( EvapCoolIndex ).PumpPowerModifierCurveIndex > 0 ) {
						PumpPowerModCurveValue = CurveValue( EvapCond( EvapCoolIndex ).PumpPowerModifierCurveIndex, FlowRatio );
					} else {
						// linearly scale pump power using part-load-fraction when pump power modifier curve is not specified
						PumpPowerModCurveValue = EvapCond( EvapCoolIndex ).PartLoadFract;
					}
					EvapCoolertotalPower += EvapCond( EvapCoolIndex ).IndirectRecircPumpPower * PumpPowerModCurveValue;
				}
			} else {
				EvapCoolertotalPower = 0.0;
			}
			return EvapCoolertotalPower;
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

		// Using/Aliasing
		using CurveManager::CurveValue;

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
		Real64 EffModCurveValue; // effectiveness modifier curve value
		Real64 PumpPowerModCurveValue; // recirculation pump power modifier curve value
		Real64 FlowRatio( 0 ); // primary air flow frcation (current flow divided by the design flow rate)
		Real64 MassFlowRateSysDesign; // primary air design mass flow rate
		Real64 MassFlowRateSys; // primary air current mass flow rate
		int InletNode; // inlet node number
		static Real64 BlowDownVdot( 0.0 );
		static Real64 DriftVdot( 0.0 );
		static Real64 EvapVdot( 0.0 );
		bool EvapCoolerOperatingLimitFlag( false );


		EvapCoolerOperatingLimitFlag = false;
		TEDB = EvapCond( EvapCoolNum ).InletTemp;
		TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
		if ( EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag ) {
			if ( TEDB >= EvapCond( EvapCoolNum ).MinOATDBEvapCooler && ( TEWB <= EvapCond( EvapCoolNum ).MaxOATWBEvapCooler || TEDB <= EvapCond( EvapCoolNum ).MaxOATDBEvapCooler ) ) {
				EvapCoolerOperatingLimitFlag = true;
			}
		} else {
			EvapCoolerOperatingLimitFlag = true;
		}

		// If the Evaporative Cooler  is operating there should be some mass flow rate
		//  Also the evap cooler has to be scheduled to be available
		if ( ( EvapCond( EvapCoolNum ).InletMassFlowRate > 0.0 ) && ( GetCurrentScheduleValue( EvapCond( EvapCoolNum ).SchedPtr ) > 0.0) && EvapCoolerOperatingLimitFlag ) {


			//***************************************************************************
			//   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
			//   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
			//   ACROSS A DIRECT EVAPORATION COOLER.
			TEWB = EvapCond( EvapCoolNum ).InletWetBulbTemp;
			TEDB = EvapCond( EvapCoolNum ).InletTemp;
			InletNode = EvapCond( EvapCoolNum ).InletNode;
			if ( EvapCond( EvapCoolNum ).WetbulbEffecCurveIndex > 0 ) {
				MassFlowRateSys = EvapCond( EvapCoolNum ).InletMassFlowRate;
				MassFlowRateSysDesign = Node( InletNode ).MassFlowRateMax;
				if ( MassFlowRateSysDesign > 0.0 ) {
					if ( MassFlowRateSys > 0.0 ) {
						FlowRatio = MassFlowRateSys / MassFlowRateSysDesign;
					} else {
						FlowRatio = 1.0;
					}
				}
				EffModCurveValue = CurveValue( EvapCond( EvapCoolNum ).WetbulbEffecCurveIndex, FlowRatio );
			} else {
				// if no curve specified assume constant effectiveness
				EffModCurveValue = 1.0;
			}
			SatEff = EvapCond( EvapCoolNum ).DirectEffectiveness * EffModCurveValue;
			EvapCond( EvapCoolNum ).StageEff = SatEff;
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
			if ( EvapCond( EvapCoolNum ).PumpPowerModifierCurveIndex > 0 ) {
				PumpPowerModCurveValue = CurveValue( EvapCond( EvapCoolNum ).PumpPowerModifierCurveIndex, FlowRatio );
			} else {
				// if no pump power modifier curve specified, then assume linear variation with part-load
				PumpPowerModCurveValue = PartLoad;
			}
			EvapCond( EvapCoolNum ).EvapCoolerPower = EvapCond( EvapCoolNum ).RecircPumpPower * PumpPowerModCurveValue;
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
		int OutletNodeSec;
		int InletNodeSec;
		static Real64 AvailWaterRate( 0.0 );

		OutletNode = EvapCond( EvapCoolNum ).OutletNode;
		InletNode = EvapCond( EvapCoolNum ).InletNode;

		InletNodeSec = EvapCond( EvapCoolNum ).SecondaryInletNode;
		OutletNodeSec = EvapCond( EvapCoolNum ).SecondaryOutletNode;

		// Set the outlet air nodes of the EvapCooler
		Node( OutletNode ).MassFlowRate = EvapCond( EvapCoolNum ).OutletMassFlowRate;
		Node( OutletNode ).MassFlowRateMaxAvail = EvapCond( EvapCoolNum ).OutletMassFlowRateMaxAvail;
		Node( OutletNode ).MassFlowRateMinAvail = EvapCond( EvapCoolNum ).OutletMassFlowRateMinAvail;
		Node( OutletNode ).Temp = EvapCond( EvapCoolNum ).OutletTemp;
		Node( OutletNode ).HumRat = EvapCond( EvapCoolNum ).OutletHumRat;
		Node( OutletNode ).Enthalpy = EvapCond( EvapCoolNum ).OutletEnthalpy;
		Node( OutletNode ).Press = EvapCond( EvapCoolNum ).OutletPressure;

		if ( EvapCond( EvapCoolNum ).SecondaryOutletNode > 0 ) {
			// set outlet nodes of the secondary air side of the EvapCooler (mass Flow Rate Only)
			if ( EvapCond( EvapCoolNum ).EvapCoolerType == iEvapCoolerInDirectRDDSpecial && EvapCond( EvapCoolNum ).EvapCoolerOperationControlFlag ) {
				Node( OutletNodeSec ).Temp = EvapCond( EvapCoolNum ).SecOutletTemp;
				Node( OutletNodeSec ).HumRat = EvapCond( EvapCoolNum ).SecOutletHumRat;
				Node( OutletNodeSec ).Enthalpy = EvapCond( EvapCoolNum ).SecOutletEnthalpy;
				Node( OutletNodeSec ).MassFlowRate = EvapCond( EvapCoolNum ).SecOutletMassFlowRate;
			}
		}

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
			CompNum = FindItemInList( CompName, ZoneEvapUnit );
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
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using BranchNodeConnections::SetUpCompSets;
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
		Array1D_string Alphas; // Alpha items for object
		Array1D< Real64 > Numbers; // Numeric items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
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
				VerifyName( Alphas( 1 ), ZoneEvapUnit, UnitLoop - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
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
				ZoneEvapUnit( UnitLoop ).EvapCooler_1_Index = FindItemInList( Alphas( 12 ), EvapCond, &EvapConditions::EvapCoolerName );
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
						ZoneEvapUnit( UnitLoop ).EvapCooler_2_Index = FindItemInList( Alphas( 14 ), EvapCond, &EvapConditions::EvapCoolerName );
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
					ZoneEvapUnit( UnitLoop ).HVACSizingIndex = FindItemInList( Alphas( 15 ), ZoneHVACSizing );
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
			SetupOutputVariable( "Zone Evaporative Cooler Unit Total Cooling Energy [J]", ZoneEvapUnit( UnitLoop ).UnitTotalCoolingEnergy, "System", "Sum", ZoneEvapUnit( UnitLoop ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
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
		static Array1D_bool MySizeFlag;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyFanFlag;
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
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
		//       MODIFIED       January 2013 Daeho Kang, add component sizing table entries
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
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHeatBalance::Zone;
		using DataSizing::AutoSize;
		using DataSizing::CurZoneEqNum;
		using DataSizing::FinalZoneSizing;
		using DataSizing::ZoneSizingRunDone;
		using DataSizing::AutoVsHardSizingThreshold;
		using General::RoundSigDigits;

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
		int FieldNum; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod(0); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)

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
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
							PrintFlag = false;
						}
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
		Array1D< Real64 > Par( 5 ); // Parameters passed to RegulaFalsi
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
					ShowWarningError( "Variable speed evaporative cooler unit calculation failed: fan speed ratio limits exceeded, for unit = " + ZoneEvapUnit( UnitNum ).Name );
					ShowContinueError( "Check input for Fan Placement." );
					ShowContinueErrorTimeStamp( "" );
					if ( WarmupFlag ) ShowContinueError( "Error occurred during warmup days." );
				}
				ShowRecurringWarningErrorAtEnd( "Zone Evaporative Cooler unit control failed (limits exceeded) for ZoneHVAC:EvaporativeCoolerUnit =\"" + ZoneEvapUnit( UnitNum ).Name, ZoneEvapUnit( UnitNum ).UnitVSControlLimitsErrorIndex );
			}
			ZoneEvapUnit( UnitNum ).FanSpeedRatio = FanSpeedRatio;
		}

	}

	Real64
	VSEvapUnitLoadResidual(
		Real64 const FanSpeedRatio,
		Array1< Real64 > const & Par // parameters
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

		UnitNum = int( Par( 1 ) );
		ZoneNum = int( Par( 2 ) );
		ZoneNodeNum = int( Par( 3 ) );
		LoadToBeMet = Par( 5 );

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

} // EvaporativeCoolers

} // EnergyPlus
