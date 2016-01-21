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
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EvaporativeFluidCoolers.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace EvaporativeFluidCoolers {

	// Module containing the routines dealing with the objects EvaporativeFluidCooler:SingleSpeed and
	// EvaporativeFluidCooler:TwoSpeed

	// MODULE INFORMATION:
	//       AUTHOR         Chandan Sharma
	//       DATE WRITTEN   May 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Model the performance of evaporative fluid coolers

	// METHODOLOGY EMPLOYED:
	// Based on cooling tower by Shirey, Raustad: Dec 2000; Shirey, Sept 2002

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::KelvinConv;
	using DataGlobals::SecInHour;
	using DataGlobals::WarmupFlag;
	using DataGlobals::InitConvTemp;
	using namespace DataHVACGlobals;
	using namespace DataLoopNode;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutWetBulbTemp;
	using General::TrimSigDigits;
	using DataPlant::PlantLoop;
	using DataBranchAirLoopPlant::MassFlowTolerance;

	// Use statements for access to subroutines in other modules
	using Psychrometrics::PsyWFnTdbTwbPb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyHFnTdbRhPb;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyTsatFnHPb;
	using Psychrometrics::PsyWFnTdbH;
	using FluidProperties::GetDensityGlycol;
	using FluidProperties::GetSpecificHeatGlycol;

	// Data
	// MODULE PARAMETER DEFINITIONS

	std::string const cEvapFluidCooler_SingleSpeed( "EvaporativeFluidCooler:SingleSpeed" );
	std::string const cEvapFluidCooler_TwoSpeed( "EvaporativeFluidCooler:TwoSpeed" );

	int const EvapLossByUserFactor( 80 );
	int const EvapLossByMoistTheory( 81 );

	int const BlowdownByConcentration( 90 );
	int const BlowdownBySchedule( 91 );

	int const PIM_StandardDesignCapacity( 1 );
	int const PIM_UFactor( 2 );
	int const PIM_UserSpecifiedDesignCapacity( 3 );

	int const EvapFluidCooler_SingleSpeed( 1 );
	int const EvapFluidCooler_TwoSpeed( 2 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumSimpleEvapFluidCoolers( 0 ); // Number of similar evaporative fluid coolers

	// The following block of variables are used to carry model results for a evaporative fluid cooler instance
	//   across sim, update, and report routines.  Simulation manager must be careful
	//   in models with multiple evaporative fluid coolers.

	Real64 InletWaterTemp( 0.0 ); // CW temperature at evaporative fluid cooler inlet
	Real64 OutletWaterTemp( 0.0 ); // CW temperature at evaporative fluid cooler outlet
	int WaterInletNode( 0 ); // Node number at evaporative fluid cooler inlet
	int WaterOutletNode( 0 ); // Node number at evaporative fluid cooler outlet
	Real64 WaterMassFlowRate( 0.0 ); // WaterMassFlowRate through evaporative fluid cooler
	//DSU this is plant level stuff now REAL(r64)   :: EvapFluidCoolerMassFlowRateMax     = 0.0d0    ! Max Hardware Mass Flow Rate
	//DSU this is plant level stuff now REAL(r64)   :: EvapFluidCoolerMassFlowRateMin     = 0.0d0    ! Min Hardware Mass Flow Rate
	//DSU this is plant level stuff now REAL(r64)   :: LoopMassFlowRateMaxAvail = 0.0d0    ! Max Loop Mass Flow Rate available
	//DSU this is plant level stuff now REAL(r64)   :: LoopMassFlowRateMinAvail = 0.0d0    ! Min Loop Mass Flow Rate available
	Real64 Qactual( 0.0 ); // Evaporative fluid cooler heat transfer
	Real64 FanPower( 0.0 ); // Evaporative fluid cooler fan power used
	Real64 AirFlowRateRatio( 0.0 ); // Ratio of air flow rate through VS evaporative fluid cooler
	// to design air flow rate
	Real64 WaterUsage( 0.0 ); // Evaporative fluid cooler water usage (m3/s)

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE EvaporativeFluidCoolers

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module
	// also, calculates UA based on Standard Design Capacity input(s)

	// Update routines to check convergence and update nodes

	// Object Data
	Array1D< EvapFluidCoolerspecs > SimpleEvapFluidCooler; // dimension to number of machines
	Array1D< EvapFluidCoolerInletConds > SimpleEvapFluidCoolerInlet; // inlet conditions
	Array1D< ReportVars > SimpleEvapFluidCoolerReport; // report variables

	// MODULE SUBROUTINES:

	// Beginning of EvaporativeFluidCoolers Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimEvapFluidCoolers(
		std::string const & EvapFluidCoolerType,
		std::string const & EvapFluidCoolerName,
		int & CompIndex,
		bool & RunFlag,
		bool const InitLoopEquip,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor // sizing factor
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Main evaporative fluid cooler driver subroutine.  Gets called from
		// PlantCondLoopSupplySideManager.

		// METHODOLOGY EMPLOYED:
		// After being called by PlantCondLoopSupplySideManager, this subroutine
		// calls GetEvapFluidCoolerInput to get all evaporative fluid cooler input info (one time only),
		// then calls the appropriate subroutine to calculate evaporative fluid cooler performance,
		// update records (node info) and writes output report info.

		// REFERENCES:
		// Based on SimTowers subroutine by Fred Buhl, May 2002

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
		static bool GetInput( true );
		int EvapFluidCoolerNum; // Pointer to EvapFluidCooler

		//GET INPUT
		if ( GetInput ) {
			GetEvapFluidCoolerInput();
			GetInput = false;
		}

		// Find the correct EvapCooler
		if ( CompIndex == 0 ) {
			EvapFluidCoolerNum = FindItemInList( EvapFluidCoolerName, SimpleEvapFluidCooler );
			if ( EvapFluidCoolerNum == 0 ) {
				ShowFatalError( "SimEvapFluidCoolers: Unit not found = " + EvapFluidCoolerName );
			}
			CompIndex = EvapFluidCoolerNum;
		} else {
			EvapFluidCoolerNum = CompIndex;
			if ( EvapFluidCoolerNum > NumSimpleEvapFluidCoolers || EvapFluidCoolerNum < 1 ) {
				ShowFatalError( "SimEvapFluidCoolers:  Invalid CompIndex passed = " + TrimSigDigits( EvapFluidCoolerNum ) + ", Number of Units = " + TrimSigDigits( NumSimpleEvapFluidCoolers ) + ", Entered Unit name = " + EvapFluidCoolerName );
			}
			if ( CheckEquipName( EvapFluidCoolerNum ) ) {
				if ( EvapFluidCoolerName != SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name ) {
					ShowFatalError( "SimEvapFluidCoolers: Invalid CompIndex passed = " + TrimSigDigits( EvapFluidCoolerNum ) + ", Unit name = " + EvapFluidCoolerName + ", stored Unit Name for that index = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				}
				CheckEquipName( EvapFluidCoolerNum ) = false;
			}
		}

		// INITIALIZE
		InitSimVars();

		// CALCULATE
		{ auto const SELECT_CASE_var( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num );

		if ( SELECT_CASE_var == EvapFluidCooler_SingleSpeed ) {

			if ( InitLoopEquip ) {
				InitEvapFluidCooler( EvapFluidCoolerNum, RunFlag );
				SizeEvapFluidCooler( EvapFluidCoolerNum );
				MinCap = 0.0; // signifies non-load based model (i.e. forward
				MaxCap = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HeatRejectCapNomCapSizingRatio;
				OptCap = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity;
				if ( GetSizingFactor ) {
					SizingFactor = SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac;
				}
				return;
			}
			InitEvapFluidCooler( EvapFluidCoolerNum, RunFlag );
			CalcSingleSpeedEvapFluidCooler( EvapFluidCoolerNum );
			CalculateWaterUseage( EvapFluidCoolerNum );
			UpdateEvapFluidCooler( EvapFluidCoolerNum );
			ReportEvapFluidCooler( RunFlag, EvapFluidCoolerNum );

		} else if ( SELECT_CASE_var == EvapFluidCooler_TwoSpeed ) {
			if ( GetSizingFactor ) {
				SizingFactor = SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac;
				return;
			}
			if ( InitLoopEquip ) {
				InitEvapFluidCooler( EvapFluidCoolerNum, RunFlag );
				SizeEvapFluidCooler( EvapFluidCoolerNum );
				MinCap = 0.0; // signifies non-load based model (i.e. forward
				MaxCap = 0.0; // heat exhanger model)
				OptCap = 0.0;
				return;
			}
			InitEvapFluidCooler( EvapFluidCoolerNum, RunFlag );
			CalcTwoSpeedEvapFluidCooler( EvapFluidCoolerNum );
			CalculateWaterUseage( EvapFluidCoolerNum );
			UpdateEvapFluidCooler( EvapFluidCoolerNum );
			ReportEvapFluidCooler( RunFlag, EvapFluidCoolerNum );

		} else {
			ShowFatalError( "SimEvapFluidCoolers: Invalid evaporative fluid cooler Type Requested = " + EvapFluidCoolerType );

		}} // TypeOfEquip

	}

	// End EvaporativeFluidCoolers Module Driver Subroutines
	//******************************************************************************

	// Beginning of EvaporativeFluidCoolers Module Get Input subroutines
	//******************************************************************************

	void
	GetEvapFluidCoolerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Chandan Sharma
		//       DATE WRITTEN:    May 2009
		//       MODIFIED         Chandan Sharma, April 2010
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for evaporative fluid coolers and stores it in SimpleEvapFluidCooler data structure.

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in the data.

		// REFERENCES:
		// Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002
		// B.A. Qureshi and S.M. Zubair , Prediction of evaporation losses in evaporative fluid coolers
		// Applied thermal engineering 27 (2007) 520-527

		// Using/Aliasing
		using namespace DataSizing;
		using namespace DataLoopNode;
		//  USE DataPlant,          ONLY: PlantLoop
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::MakeUPPERCase;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using CurveManager::GetCurveIndex;
		using ScheduleManager::GetScheduleIndex;
		using WaterManager::SetupTankDemandComponent;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using General::TrimSigDigits;
		using FluidProperties::CheckFluidPropertyName;
		using FluidProperties::FindGlycol;
		using FluidProperties::GetGlycolNameByIndex;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutRelHumValue;

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
		int EvapFluidCoolerNum; // Evaporative fluid cooler number,
		// reference counter for SimpleEvapFluidCooler data array
		int NumSingleSpeedEvapFluidCoolers; // Total number of single-speed evaporative fluid coolers
		int SingleSpeedEvapFluidCoolerNumber; // Specific single-speed evaporative fluid cooler of interest
		int NumTwoSpeedEvapFluidCoolers; // Number of two-speed evaporative fluid coolers
		int TwoSpeedEvapFluidCoolerNumber; // Specific two-speed evaporative fluid cooler of interest
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool ErrorsFound( false ); // Logical flag set .TRUE. if errors found while getting input data
		Array1D< Real64 > NumArray( 25 ); // Numeric input data array
		Array1D_string AlphArray( 13 ); // Character string input data array
		std::string FluidName;

		// Get number of all evaporative fluid coolers specified in the input data file (idf)
		NumSingleSpeedEvapFluidCoolers = GetNumObjectsFound( cEvapFluidCooler_SingleSpeed );
		NumTwoSpeedEvapFluidCoolers = GetNumObjectsFound( cEvapFluidCooler_TwoSpeed );
		NumSimpleEvapFluidCoolers = NumSingleSpeedEvapFluidCoolers + NumTwoSpeedEvapFluidCoolers;

		if ( NumSimpleEvapFluidCoolers <= 0 ) ShowFatalError( "No evaporative fluid cooler objects found in input, however, a branch object has specified an evaporative fluid cooler. Search the input for evaporative fluid cooler to determine the cause for this error." );

		// See if load distribution manager has already gotten the input
		if ( allocated( SimpleEvapFluidCooler ) ) return;

		// Allocate data structures to hold evaporative fluid cooler input data,
		// report data and evaporative fluid cooler inlet conditions
		SimpleEvapFluidCooler.allocate( NumSimpleEvapFluidCoolers );
		SimpleEvapFluidCoolerReport.allocate( NumSimpleEvapFluidCoolers );
		SimpleEvapFluidCoolerInlet.allocate( NumSimpleEvapFluidCoolers );
		CheckEquipName.dimension( NumSimpleEvapFluidCoolers, true );

		// Load data structures with evaporative fluid cooler input data
		cCurrentModuleObject = cEvapFluidCooler_SingleSpeed;
		for ( SingleSpeedEvapFluidCoolerNumber = 1; SingleSpeedEvapFluidCoolerNumber <= NumSingleSpeedEvapFluidCoolers; ++SingleSpeedEvapFluidCoolerNumber ) {
			EvapFluidCoolerNum = SingleSpeedEvapFluidCoolerNumber;
			GetObjectItem( cCurrentModuleObject, SingleSpeedEvapFluidCoolerNumber, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SimpleEvapFluidCooler, EvapFluidCoolerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name = AlphArray( 1 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType = cCurrentModuleObject;
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num = EvapFluidCooler_SingleSpeed;
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerMassFlowRateMultiplier = 2.5;
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Chilled Water Nodes" );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate = NumArray( 1 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRateWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower = NumArray( 2 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPowerWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignSprayWaterFlowRate = NumArray( 3 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HeatRejectCapNomCapSizingRatio = NumArray( 4 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity = NumArray( 5 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = NumArray( 6 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUAWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate = NumArray( 7 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRateWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedUserSpecifiedDesignCapacity = NumArray( 8 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp = NumArray( 9 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp = NumArray( 10 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp = NumArray( 11 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).FluidIndex = PlantLoop( CurLoopNum ).FluidIndex;
			FluidName = GetGlycolNameByIndex( SimpleEvapFluidCooler( EvapFluidCoolerNum ).FluidIndex );

			if ( lAlphaFieldBlanks( 4 ) || AlphArray( 4 ).empty() ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\" Performance input method is not specified. " );
				ErrorsFound = true;
			}
			if ( SameString( AlphArray( 4 ), "STANDARDDESIGNCAPACITY" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_StandardDesignCapacity;
				if ( FluidName != "WATER" ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". StandardDesignCapacity performance input method is only valid for fluid type = \"Water\"." );
					ShowContinueError( "Currently, Fluid Type = " + FluidName + " in CondenserLoop = " + PlantLoop( CurLoopNum ).Name );
					ErrorsFound = true;
				}
			}

			//outdoor air inlet node
			if ( lAlphaFieldBlanks( 5 ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum = 0;
			} else {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, cCurrentModuleObject, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum ) ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray( 5 ) );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}
			}

			//   fluid bypass for single speed evaporative fluid cooler
			if ( lAlphaFieldBlanks( 6 ) || AlphArray( 6 ).empty() ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).CapacityControl = 0; // FanCycling
			} else {
				{ auto const SELECT_CASE_var( MakeUPPERCase( AlphArray( 6 ) ) );
				if ( SELECT_CASE_var == "FANCYCLING" ) {
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).CapacityControl = 0;
				} else if ( SELECT_CASE_var == "FLUIDBYPASS" ) {
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).CapacityControl = 1;
				} else {
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).CapacityControl = 0;
					ShowWarningError( cCurrentModuleObject + ", \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\" The Capacity Control is not specified correctly. The default Fan Cycling is used." );
				}}
			}

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac = NumArray( 12 ); //  N11  \field Sizing Factor
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac <= 0.0 ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac = 1.0;

			// begin water use and systems get input
			if ( SameString( AlphArray( 7 ), "LossFactor" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode = EvapLossByUserFactor;
			} else if ( SameString( AlphArray( 7 ), "SaturatedExit" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else if ( AlphArray( 7 ).empty() ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else {
				ShowSevereError( "Invalid, " + cAlphaFieldNames( 7 ) + " = " + AlphArray( 7 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor = NumArray( 13 ); //  N13 , \field Evaporation Loss Factor
			if ( ( NumNums < 13 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor == 0.0 ) ) {
				// assume Evaporation loss factor not entered and should be calculated
				if ( ( OutRelHumValue >= 0.1 ) && ( OutRelHumValue <= 0.7 ) ) {
					//Use correlation by B.A. Qureshi and S.M. Zubair if within these limits
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor = ( 113.0 - 8.417 * OutRelHumValue + 1.6147 * OutDryBulbTemp ) * 1.0e-5;
				} else { // Inlet conditions are out of the limit of correlation; An approximate default value of loss factor is used
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor = 0.2;
				}
			}

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DriftLossFraction = NumArray( 14 ) / 100.0; //  N14, \field Drift Loss Percent

			if ( ( NumNums < 13 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DriftLossFraction == 0.0 ) ) {
				// assume Drift loss not entered and should be defaulted
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).DriftLossFraction = 0.008 / 100.0;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio = NumArray( 15 ); //  N15, \field Blowdown Concentration Ratio

			if ( SameString( AlphArray( 8 ), "ScheduledRate" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode = BlowdownBySchedule;
			} else if ( SameString( AlphArray( 8 ), "ConcentrationRatio" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode = BlowdownByConcentration;
			} else if ( AlphArray( 8 ).empty() ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode = BlowdownByConcentration;
				if ( ( NumNums < 15 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio == 0.0 ) ) {
					// assume Concetration ratio was omitted and should be defaulted
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio = 3.0;
				}
			} else {
				ShowSevereError( "Invalid, " + cAlphaFieldNames( 8 ) + " = " + AlphArray( 8 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " =" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).SchedIDBlowdown = GetScheduleIndex( AlphArray( 9 ) );
			if ( ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SchedIDBlowdown == 0 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode == BlowdownBySchedule ) ) {
				ShowSevereError( "Invalid, " + cAlphaFieldNames( 9 ) + " = " + AlphArray( 9 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " =" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			if ( AlphArray( 10 ).empty() ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( AlphArray( 1 ), cCurrentModuleObject, AlphArray( 10 ), ErrorsFound, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankID, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankDemandARRID );
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).SuppliedByWaterSystem = true;
			}

			//   Check various inputs to ensure that all the required variables are specified.

			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignSprayWaterFlowRate <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler input requires a design spray water flow rate greater than zero for all performance input methods." );
				ErrorsFound = true;
			}
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 1 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
				ErrorsFound = true;
			}
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 2 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
				ErrorsFound = true;
			}

			if ( SameString( AlphArray( 4 ), "UFACTORTIMESAREAANDDESIGNWATERFLOWRATE" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_UFactor;
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 6 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 7 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
			} else if ( SameString( AlphArray( 4 ), "STANDARDDESIGNCAPACITY" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_StandardDesignCapacity;
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 5 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
			} else if ( SameString( AlphArray( 4 ), "USERSPECIFIEDDESIGNCAPACITY" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_UserSpecifiedDesignCapacity;
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 7 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedUserSpecifiedDesignCapacity <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 8 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 9 ) + "\", entered value <= 0.0, but must be >0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 10 ) + "\", entered value <= 0.0, but must be >0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 11 ) + "\", entered value <= 0.0, but must be >0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", " + cNumericFieldNames( 9 ) + " must be greater than " + cNumericFieldNames( 11 ) + '.' );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", " + cNumericFieldNames( 10 ) + " must be greater than " + cNumericFieldNames( 11 ) + '.' );
					ErrorsFound = true;
				}
			} else { // Evaporative fluid cooler performance input method is not specified as a valid "choice"
				ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler Performance Input Method must be \"UFactorTimesAreaAndDesignWaterFlowRate\" or \"StandardDesignCapacity\" or \"UserSpecifiedDesignCapacity\"." );
				ShowContinueError( "Evaporative fluid cooler Performance Input Method currently specified as: " + AlphArray( 4 ) );
				ErrorsFound = true;
			}
		} // End Single-Speed Evaporative Fluid Cooler Loop

		cCurrentModuleObject = cEvapFluidCooler_TwoSpeed;
		for ( TwoSpeedEvapFluidCoolerNumber = 1; TwoSpeedEvapFluidCoolerNumber <= NumTwoSpeedEvapFluidCoolers; ++TwoSpeedEvapFluidCoolerNumber ) {
			EvapFluidCoolerNum = NumSingleSpeedEvapFluidCoolers + TwoSpeedEvapFluidCoolerNumber;
			GetObjectItem( cCurrentModuleObject, TwoSpeedEvapFluidCoolerNumber, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SimpleEvapFluidCooler, EvapFluidCoolerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name = AlphArray( 1 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType = cCurrentModuleObject;
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num = EvapFluidCooler_TwoSpeed;
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerMassFlowRateMultiplier = 2.5;
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Chilled Water Nodes" );

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate = NumArray( 1 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRateWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower = NumArray( 2 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPowerWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate = NumArray( 3 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRateWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRateSizingFactor = NumArray( 4 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower = NumArray( 5 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPowerWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPowerSizingFactor = NumArray( 6 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignSprayWaterFlowRate = NumArray( 7 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HeatRejectCapNomCapSizingRatio = NumArray( 8 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity = NumArray( 9 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedStandardDesignCapacity = NumArray( 10 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedStandardDesignCapacitySizingFactor = NumArray( 11 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = NumArray( 12 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUAWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA = NumArray( 13 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUAWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUASizingFactor = NumArray( 14 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate = NumArray( 15 );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate == AutoSize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRateWasAutoSized = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedUserSpecifiedDesignCapacity = NumArray( 16 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedUserSpecifiedDesignCapacity = NumArray( 17 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedUserSpecifiedDesignCapacitySizingFactor = NumArray( 18 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp = NumArray( 19 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp = NumArray( 20 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp = NumArray( 21 );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).FluidIndex = PlantLoop( CurLoopNum ).FluidIndex;
			FluidName = GetGlycolNameByIndex( SimpleEvapFluidCooler( EvapFluidCoolerNum ).FluidIndex );

			if ( lAlphaFieldBlanks( 4 ) ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\" Performance input method is not specified. " );
				ErrorsFound = true;
			}

			if ( SameString( AlphArray( 4 ), "STANDARDDESIGNCAPACITY" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_StandardDesignCapacity;
				if ( FluidName != "WATER" ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". StandardDesignCapacity performance input method is only valid for fluid type = \"Water\"." );
					ShowContinueError( "Currently, Fluid Type = " + FluidName + " in CondenserLoop = " + PlantLoop( CurLoopNum ).Name );
					ErrorsFound = true;
				}
			}

			// outdoor air inlet node
			if ( lAlphaFieldBlanks( 5 ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum = 0;
			} else {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, cCurrentModuleObject, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum ) ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray( 5 ) );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}
			}

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac = NumArray( 22 ); //  N16  \field Sizing Factor
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac <= 0.0 ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac = 1.0;

			// begin water use and systems get input
			if ( SameString( AlphArray( 6 ), "LossFactor" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode = EvapLossByUserFactor;
			} else if ( SameString( AlphArray( 6 ), "SaturatedExit" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else if ( lAlphaFieldBlanks( 6 ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + " = " + AlphArray( 6 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor = NumArray( 23 ); //  N23 , \field Evaporation Loss Factor
			if ( ( NumNums < 23 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor == 0.0 ) ) {
				// assume Evaporation loss factor not entered and should be calculated
				if ( ( OutRelHumValue >= 0.1 ) && ( OutRelHumValue <= 0.7 ) ) {
					//Use correlation by B.A. Qureshi and S.M. Zubair if within these limits
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor = ( 113.0 - 8.417 * OutRelHumValue + 1.6147 * OutDryBulbTemp ) * 1.0e-5;
				} else { // Inlet conditions are out of the limit of correlation; An approximate default value of loss factor is used
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor = 0.2;
				}
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DriftLossFraction = NumArray( 24 ) / 100.0; //  N24, \field Drift Loss Percent
			if ( ( NumNums < 24 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DriftLossFraction == 0.0 ) ) {
				// assume Drift loss not entered and should be defaulted
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).DriftLossFraction = 0.008 / 100.0;
			}

			SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio = NumArray( 25 ); //  N25, \field Blowdown Concentration Ratio

			if ( SameString( AlphArray( 7 ), "ScheduledRate" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode = BlowdownBySchedule;
			} else if ( SameString( AlphArray( 7 ), "ConcentrationRatio" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode = BlowdownByConcentration;
			} else if ( lAlphaFieldBlanks( 7 ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode = BlowdownByConcentration;
				if ( ( NumNums < 25 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio == 0.0 ) ) {
					// assume Concetration ratio was omitted and should be defaulted
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio = 3.0;
				}
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + " = " + AlphArray( 7 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).SchedIDBlowdown = GetScheduleIndex( AlphArray( 8 ) );
			if ( ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SchedIDBlowdown == 0 ) && ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode == BlowdownBySchedule ) ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + " = " + AlphArray( 8 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			if ( lAlphaFieldBlanks( 9 ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( AlphArray( 1 ), cCurrentModuleObject, AlphArray( 9 ), ErrorsFound, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankID, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankDemandARRID );
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).SuppliedByWaterSystem = true;
			}

			//   Check various inputs to ensure that all the required variables are specified.

			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignSprayWaterFlowRate <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler input requires a design spray water flow rate greater than zero for all performance input methods." );
				ErrorsFound = true;
			}
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + "= \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler input requires design air flow rate at high fan speed to be greater than zero for all performance input methods." );
				ErrorsFound = true;
			}
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + "= \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler input requires design air flow rate at low fan speed to be greater than zero for all performance input methods." );
				ErrorsFound = true;
			}
			//   High speed air flow rate must be greater than low speed air flow rate.
			//   Can't tell yet if autosized, check later in InitEvapFluidCooler.
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler air flow rate at low fan speed must be less than the air flow rate at high fan speed." );
				ErrorsFound = true;
			}
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 2 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
				ErrorsFound = true;
			}
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 5 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
				ErrorsFound = true;
			}
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler low speed fan power must be less than the high speed fan power ." );
				ErrorsFound = true;
			}

			if ( SameString( AlphArray( 4 ), "UFACTORTIMESAREAANDDESIGNWATERFLOWRATE" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_UFactor;
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 12 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 13 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA && SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler U-factor Times Area Value at Low Fan Speed must be less than the U-factor Times Area Value at High Fan Speed." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 15 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
			} else if ( SameString( AlphArray( 4 ), "STANDARDDESIGNCAPACITY" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_StandardDesignCapacity;
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 9 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedStandardDesignCapacity <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 10 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedStandardDesignCapacity >= SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Low-Speed Standard Design Capacity must be less than the High-Speed Standard Design Capacity." );
					ErrorsFound = true;
				}
			} else if ( SameString( AlphArray( 4 ), "USERSPECIFIEDDESIGNCAPACITY" ) ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num = PIM_UserSpecifiedDesignCapacity;
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate <= 0.0 && SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 15 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedUserSpecifiedDesignCapacity <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 16 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedUserSpecifiedDesignCapacity <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 17 ) + "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA != 0.0 ) {
					if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". UserSpecifiedDesignCapacity performance input method and evaporative fluid cooler UA at high fan speed have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". UserSpecifiedDesignCapacity performance input method has been specified and evaporative fluid cooler UA at high fan speed is being autosized." );
					}
					ShowContinueError( "Evaporative fluid cooler UA at high fan speed must be left blank when UserSpecifiedDesignCapacity performance input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA != 0.0 ) {
					if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". UserSpecifiedDesignCapacity performance input method and evaporative fluid cooler UA at low fan speed have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". UserSpecifiedDesignCapacity performance input method has been specified and evaporative fluid cooler UA at low fan speed is being autosized." );
					}
					ShowContinueError( "Evaporative fluid cooler UA at low fan speed must be left blank when UserSpecifiedDesignCapacity performance input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedUserSpecifiedDesignCapacity >= SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedUserSpecifiedDesignCapacity ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Low-Speed User Specified Design Capacity must be less than the High-Speed User Specified Design Dapacity." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 19 ) + "\", entered value <= 0.0, but must be >0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 20 ) + "\", entered value <= 0.0, buy must be >0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", invalid data for \"" + cNumericFieldNames( 21 ) + "\", entered value <= 0.0, but must be >0 for " + cAlphaFieldNames( 4 ) + " = \"" + AlphArray( 4 ) + "\"." );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", " + cNumericFieldNames( 19 ) + " must be greater than " + cNumericFieldNames( 15 ) + '.' );
					ErrorsFound = true;
				}
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp ) {
					ShowSevereError( cCurrentModuleObject + " = \"" + AlphArray( 1 ) + "\", " + cNumericFieldNames( 20 ) + " must be greater than " + cNumericFieldNames( 15 ) + '.' );
					ErrorsFound = true;
				}
			} else { // Evaporative fluid cooler performance input method is not specified as a valid "choice"
				ShowSevereError( cCurrentModuleObject + " = \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler Performance Input Method must be \"UFactorTimesAreaAndDesignWaterFlowRate\" or \"StandardDesignCapacity\" or \"UserSpecifiedDesignCapacity\"." );
				ShowContinueError( "Evaporative fluid cooler Performanace Input Method currently specified as: " + AlphArray( 4 ) );
				ErrorsFound = true;
			}
		} // End Two-Speed Evaporative Fluid Cooler Loop

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in getting evaporative fluid cooler input." );
		}

		// Set up output variables
		// CurrentModuleObject='EvaporativeFluidCooler:SingleSpeed'
		for ( EvapFluidCoolerNum = 1; EvapFluidCoolerNum <= NumSingleSpeedEvapFluidCoolers; ++EvapFluidCoolerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).InletWaterTemp, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).OutletWaterTemp, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).WaterMassFlowRate, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).Qactual, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanPower, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanEnergy, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			// Added for fluid bypass
			SetupOutputVariable( "Cooling Tower Bypass Fraction []", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).BypassFraction, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
		}

		// CurrentModuleObject='EvaporativeFluidCooler:TwoSpeed'
		for ( EvapFluidCoolerNum = NumSingleSpeedEvapFluidCoolers + 1; EvapFluidCoolerNum <= NumSingleSpeedEvapFluidCoolers + NumTwoSpeedEvapFluidCoolers; ++EvapFluidCoolerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).InletWaterTemp, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).OutletWaterTemp, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).WaterMassFlowRate, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).Qactual, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanPower, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanEnergy, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );

		}

		// setup common water reporting for all types of evaporative fluid coolers.
		// CurrentModuleObject='EvaporativeFluidCooler:*'
		for ( EvapFluidCoolerNum = 1; EvapFluidCoolerNum <= NumSingleSpeedEvapFluidCoolers + NumTwoSpeedEvapFluidCoolers; ++EvapFluidCoolerNum ) {
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SuppliedByWaterSystem ) {
				SetupOutputVariable( "Cooling Tower Make Up Water Volume Flow Rate [m3/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).MakeUpVdot, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Water Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).MakeUpVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				SetupOutputVariable( "Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).TankSupplyVdot, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				SetupOutputVariable( "Cooling Tower Storage Tank Water Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).TankSupplyVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, _, "Water", "HeatRejection", _, "Plant" );
				SetupOutputVariable( "Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).StarvedMakeUpVdot, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				SetupOutputVariable( "Cooling Tower Starved Storage Tank Water Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).StarvedMakeUpVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, _, "Water", "HeatRejection", _, "Plant" );
				SetupOutputVariable( "Cooling Tower Make Up Mains Water Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).StarvedMakeUpVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, _, "MainsWater", "HeatRejection", _, "Plant" );
			} else { // Evaporative fluid cooler water from mains and gets metered
				SetupOutputVariable( "Cooling Tower Make Up Water Volume Flow Rate [m3/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).MakeUpVdot, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Water Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).MakeUpVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, _, "Water", "HeatRejection", _, "Plant" );
				SetupOutputVariable( "Cooling Tower Make Up Mains Water Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).MakeUpVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, _, "MainsWater", "HeatRejection", _, "Plant" );
			}

			SetupOutputVariable( "Cooling Tower Water Evaporation Volume Flow Rate [m3/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).EvaporationVdot, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Evaporation Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).EvaporationVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Drift Volume Flow Rate [m3/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).DriftVdot, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Drift Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).DriftVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Blowdown Volume Flow Rate [m3/s]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).BlowdownVdot, "System", "Average", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Blowdown Volume [m3]", SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).BlowdownVol, "System", "Sum", SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
		} // loop all evaporative fluid coolers

	}

	// End of Get Input subroutines for the Evaporative Fluid Cooler Module
	//******************************************************************************

	// Beginning Initialization Section for the Evaporative Fluid Coolers Module
	//******************************************************************************

	void
	InitSimVars()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Chandan Sharma
		//       DATE WRITTEN:    May 2009
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize the simulation variables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		//INITIALIZE MODULE LEVEL VARIABLES

		InletWaterTemp = 0.0; // CW temperature at evaporative fluid cooler inlet
		OutletWaterTemp = 0.0; // CW temperature at evaporative fluid cooler outlet
		WaterInletNode = 0; // Node number at evaporative fluid cooler inlet
		WaterOutletNode = 0; // Node number at evaporative fluid cooler outlet
		WaterMassFlowRate = 0.0; // WaterMassFlowRate through evaporative fluid cooler
		//    EvapFluidCoolerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
		//    EvapFluidCoolerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
		//    LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
		//    LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
		Qactual = 0.0; // Evaporative fluid cooler heat transfer
		FanPower = 0.0; // Evaporative fluid cooler fan power used
		AirFlowRateRatio = 0.0; // Ratio of air flow rate through VS Evaporative fluid cooler to design air flow rate
		WaterUsage = 0.0; // Evaporative fluid cooler water usage (m3/s)

	}

	void
	InitEvapFluidCooler(
		int const EvapFluidCoolerNum, // Number of the current evaporative fluid cooler being simulated
		bool const EP_UNUSED( RunFlag ) // Indication of
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the evaporative fluid cooler components and for
		// final checking of evaporative fluid cooler inputs (post autosizing)

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// Based on InitTower subroutine by Don Shirey Sept/Oct 2002, F Buhl Oct 2002

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using InputProcessor::SameString;
		//  USE FluidProperties, ONLY : GetDensityGlycol
		using DataPlant::TypeOf_EvapFluidCooler_SingleSpd;
		using DataPlant::TypeOf_EvapFluidCooler_TwoSpd;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegulateCondenserCompFlowReqOp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitEvapFluidCooler" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Flag if input data errors are found
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool OneTimeFlagForEachEvapFluidCooler;
		int TypeOf_Num( 0 );
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		Real64 rho; // local density of fluid
		//  LOGICAL   :: FatalError

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumSimpleEvapFluidCoolers );
			OneTimeFlagForEachEvapFluidCooler.allocate( NumSimpleEvapFluidCoolers );

			OneTimeFlagForEachEvapFluidCooler = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;

		}

		if ( OneTimeFlagForEachEvapFluidCooler( EvapFluidCoolerNum ) ) {

			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed ) {
				TypeOf_Num = TypeOf_EvapFluidCooler_SingleSpd;
			} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
				TypeOf_Num = TypeOf_EvapFluidCooler_TwoSpd;
			} else {
				assert( false );
			}
			ErrorsFound = false;
			// Locate the tower on the plant loops for later usage
			ScanPlantLoopsForObject( SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name, TypeOf_Num, SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).BranchNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).CompNum, _, _, _, _, _, ErrorsFound );

			if ( ErrorsFound ) {
				ShowFatalError( "InitEvapFluidCooler: Program terminated due to previous condition(s)." );
			}

			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate > 0.0 ) {
					if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate ) {
						ShowSevereError( "EvaporativeFluidCooler:TwoSpeed \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Low speed air flow rate must be less than the high speed air flow rate." );
						ErrorsFound = true;
					}
					if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA <= SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA ) {
						ShowSevereError( "EvaporativeFluidCooler:TwoSpeed \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\". Evaporative fluid cooler UA at low fan speed must be less than the evaporative fluid cooler UA at high fan speed." );
						ErrorsFound = true;
					}
				}
			}

			if ( ErrorsFound ) {
				ShowFatalError( "InitEvapFluidCooler: Program terminated due to previous condition(s)." );
			}

			OneTimeFlagForEachEvapFluidCooler( EvapFluidCoolerNum ) = false;

		}

		// Begin environment initializations
		if ( MyEnvrnFlag( EvapFluidCoolerNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, RoutineName );
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesWaterMassFlowRate = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate * rho;
			InitComponentNodes( 0.0, SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesWaterMassFlowRate, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterOutletNodeNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).BranchNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).CompNum );
			MyEnvrnFlag( EvapFluidCoolerNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( EvapFluidCoolerNum ) = true;
		}

		// Each time initializations
		WaterInletNode = SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum;
		SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp = Node( WaterInletNode ).Temp;

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum != 0 ) {
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp = Node( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum ).Temp;
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat = Node( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum ).HumRat;
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress = Node( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum ).Press;
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb = Node( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutdoorAirInletNodeNum ).OutAirWetBulb;
		} else {
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp = OutDryBulbTemp;
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat = OutHumRat;
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress = OutBaroPress;
			SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb = OutWetBulbTemp;
		}

		LoopNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum;
		LoopSideNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum;
		BranchIndex = SimpleEvapFluidCooler( EvapFluidCoolerNum ).BranchNum;
		CompIndex = SimpleEvapFluidCooler( EvapFluidCoolerNum ).CompNum;

		WaterMassFlowRate = RegulateCondenserCompFlowReqOp( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).BranchNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).CompNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesWaterMassFlowRate * SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerMassFlowRateMultiplier );

		SetComponentFlowRate( WaterMassFlowRate, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterOutletNodeNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).BranchNum, SimpleEvapFluidCooler( EvapFluidCoolerNum ).CompNum );
	}

	void
	SizeEvapFluidCooler( int const EvapFluidCoolerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       Chandan Sharma, April 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing evaporative fluid cooler Components for which capacities and flow rates
		// have not been specified in the input. This subroutine also calculates evaporative fluid cooler UA if the user
		// has specified evaporative fluid cooler performance via the "Standard Design Capacity" method.

		// METHODOLOGY EMPLOYED:
		// Obtains condenser flow rate from the plant sizing array. If evaporative fluid cooler performance is specified
		// via the "Standard Design Capacity" method, the water flow rate is directly proportional to capacity.

		// REFERENCES:
		// Based on SizeTower by Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005

		// Using/Aliasing
		using namespace DataSizing;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using InputProcessor::SameString;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations
		Real64 const Acc( 0.0001 ); // Accuracy of result
		static std::string const CalledFrom( "SizeEvapFluidCooler" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizCondNum; // Plant Sizing index for condenser loop
		int SolFla; // Flag of solver
		Real64 DesEvapFluidCoolerLoad; // Design evaporative fluid cooler load [W]
		Real64 UA0; // Lower bound for UA [W/C]
		Real64 UA1; // Upper bound for UA [W/C]
		Real64 UA; // Calculated UA value [W/C]
		Real64 OutWaterTempAtUA0; // Water outlet temperature at UA0
		Real64 OutWaterTempAtUA1; // Water outlet temperature at UA1
		Real64 DesignEnteringAirWetBulb; // Intermediate variable to check that design exit
		// temperature specified in the plant:sizing object
		// is higher than the design entering air wet-bulb temp
		// when autosize feature is used
		Array1D< Real64 > Par( 6 ); // Parameter array need for RegulaFalsi routine
		std::string equipName;
		Real64 Cp; // local specific heat for fluid
		Real64 rho; // local density for fluid
		Real64 tmpDesignWaterFlowRate; // local temporary for water volume flow rate
		Real64 tmpHighSpeedFanPower; // local temporary for high speed fan power
		Real64 tmpHighSpeedAirFlowRate; // local temporary for high speed air flow rate
		Real64 tmpHighSpeedEvapFluidCoolerUA; // local temporary for high speed cooler UA

		DesEvapFluidCoolerLoad = 0.0;
		tmpDesignWaterFlowRate = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate;
		tmpHighSpeedFanPower = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower;
		tmpHighSpeedAirFlowRate = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate;
		tmpHighSpeedEvapFluidCoolerUA = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA;

		PltSizCondNum = PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).PlantSizNum;

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRateWasAutoSized && SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num != PIM_StandardDesignCapacity ) {
			if ( PltSizCondNum > 0 ) {
				if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					tmpDesignWaterFlowRate = PlantSizData( PltSizCondNum ).DesVolFlowRate * SimpleEvapFluidCooler( EvapFluidCoolerNum ).SizFac;
					if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate = tmpDesignWaterFlowRate;

				} else {
					tmpDesignWaterFlowRate = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate = tmpDesignWaterFlowRate;
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Design Water Flow Rate [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial Design Water Flow Rate [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate );
					}
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing error for evaporative fluid cooler object = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
					ShowFatalError( "Autosizing of evaporative fluid cooler condenser flow rate requires a loop Sizing:Plant object." );
				}
			}
			// Check when the user specified Condenser/Evaporative Fluid Cooler water design setpoint
			// temperature is less than design inlet air wet bulb temperature
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_UFactor ) {
				DesignEnteringAirWetBulb = 25.6;
			} else {
				DesignEnteringAirWetBulb = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp;
			}
			if ( PlantSizData( PltSizCondNum ).ExitTemp <= DesignEnteringAirWetBulb ) {
				ShowSevereError( "Error when autosizing the UA value for Evaporative Fluid Cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + '.' );
				ShowContinueError( "Design Loop Exit Temperature (" + RoundSigDigits( PlantSizData( PltSizCondNum ).ExitTemp, 2 ) + " C) must be greater than design entering air wet-bulb temperature (" + RoundSigDigits( DesignEnteringAirWetBulb, 2 ) + " C) when autosizing the Evaporative Fluid Cooler UA." );
				ShowContinueError( "It is recommended that the Design Loop Exit Temperature = Design Entering Air Wet-bulb Temp plus the Evaporative Fluid Cooler design approach temperature (e.g., 4 C)." );
				ShowContinueError( "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be > Design Entering Air Wet-bulb Temp if autosizing the Evaporative Fluid Cooler." );
				ShowFatalError( "Review and revise design input values as appropriate." );
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_UFactor
				&& ! SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUAWasAutoSized ) {
			if ( PltSizCondNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				DesEvapFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity = DesEvapFluidCoolerLoad / SimpleEvapFluidCooler( EvapFluidCoolerNum ).HeatRejectCapNomCapSizingRatio;
			} else {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity = 0.0;
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_StandardDesignCapacity ) {
			// Design water flow rate is assumed to be 3 gpm per ton (SI equivalent 5.382E-8 m3/s per watt)
			tmpDesignWaterFlowRate = 5.382e-8 * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity;
			if ( PlantFirstSizesOkayToFinalize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate = tmpDesignWaterFlowRate;
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Design Water Flow Rate based on evaporative fluid cooler Standard Design Capacity [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial Design Water Flow Rate based on evaporative fluid cooler Standard Design Capacity [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate );
					}
				} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Design Water Flow Rate based on evaporative fluid cooler high-speed Standard Design Capacity [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial Design Water Flow Rate based on evaporative fluid cooler high-speed Standard Design Capacity [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate );
					}
				}
			}
		}

		RegisterPlantCompDesignFlow( SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum, tmpDesignWaterFlowRate );

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPowerWasAutoSized ) {
			// We assume the nominal fan power is 0.0105 times the design load
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_StandardDesignCapacity ) {
				tmpHighSpeedFanPower = 0.0105 * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity;
				if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
			} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_UserSpecifiedDesignCapacity ) {
				tmpHighSpeedFanPower = 0.0105 * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedUserSpecifiedDesignCapacity;
				if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
			} else {
				if ( DesEvapFluidCoolerLoad > 0 ) {
					tmpHighSpeedFanPower = 0.0105 * DesEvapFluidCoolerLoad;
					if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
				} else if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
						Cp = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
						DesEvapFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
						tmpHighSpeedFanPower = 0.0105 * DesEvapFluidCoolerLoad;
						if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
					} else {
						tmpHighSpeedFanPower = 0.0;
						if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
					}
				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						ShowSevereError( "Autosizing of evaporative fluid cooler fan power requires a loop Sizing:Plant object." );
						ShowFatalError( " Occurs in evaporative fluid cooler object= " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
					}
				}
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Fan Power at Design Air Flow Rate [W]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial Fan Power at Design Air Flow Rate [W]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower );
					}
				} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Fan Power at High Fan Speed [W]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial Fan Power at High Fan Speed [W]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower );
					}
				}
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRateWasAutoSized ) {
			// Plant Sizing Object is not required to AUTOSIZE this field since its simply a multiple of another field.

			tmpHighSpeedAirFlowRate = tmpHighSpeedFanPower * 0.5 * ( 101325.0 / StdBaroPress ) / 190.0;
			if ( PlantFirstSizesOkayToFinalize ) {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;

				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Design Air Flow Rate [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial Design Air Flow Rate [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate );
					}
				} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Air Flow Rate at High Fan Speed [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial Air Flow Rate at High Fan Speed [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate );
					}
				}
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUAWasAutoSized && SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_UFactor ) {
			if ( PltSizCondNum > 0 ) {
				if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					// This conditional statement is to trap when the user specified Condenser/Evaporative Fluid Cooler water design setpoint
					// temperature is less than design inlet air wet bulb temperature of 25.6 C
					if ( PlantSizData( PltSizCondNum ).ExitTemp <= 25.6 ) {
						ShowSevereError( "Error when autosizing the UA value for Evaporative Fluid Cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + '.' );
						ShowContinueError( "Design Loop Exit Temperature (" + RoundSigDigits( PlantSizData( PltSizCondNum ).ExitTemp, 2 ) + " C) must be greater than 25.6 C when autosizing the Evaporative Fluid Cooler UA." );
						ShowContinueError( "The Design Loop Exit Temperature specified in Sizing:Plant object = " + PlantSizData( PltSizCondNum ).PlantLoopName );
						ShowContinueError( "It is recommended that the Design Loop Exit Temperature = 25.6 C plus the Evaporative Fluid Cooler design approach temperature (e.g., 4 C)." );
						ShowContinueError( "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be > 25.6 C if autosizing the Evaporative Fluid Cooler." );
						ShowFatalError( "Review and revise design input values as appropriate." );
					}
					rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
					Cp = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
					DesEvapFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
					Par( 1 ) = DesEvapFluidCoolerLoad;
					Par( 2 ) = double( EvapFluidCoolerNum );
					Par( 3 ) = rho * tmpDesignWaterFlowRate; // Design water mass flow rate
					Par( 4 ) = tmpHighSpeedAirFlowRate; // Design air volume flow rate
					Par( 5 ) = Cp;
					UA0 = 0.0001 * DesEvapFluidCoolerLoad; // Assume deltaT = 10000K (limit)
					UA1 = DesEvapFluidCoolerLoad; // Assume deltaT = 1K
					SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp = 35.0;
					SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb = 25.6;
					SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress = StdBaroPress;
					SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );
					SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par );
					if ( SolFla == -1 ) {
						ShowWarningError( "Iteration limit exceeded in calculating evaporative fluid cooler UA." );
						ShowContinueError( "Autosizing of fluid cooler UA failed for evaporative fluid cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
						ShowContinueError( "The final UA value = " + RoundSigDigits( UA, 2 ) + "W/C, and the simulation continues..." );
					} else if ( SolFla == -2 ) {
						SimSimpleEvapFluidCooler( int( Par( 2 ) ), Par( 3 ), Par( 4 ), UA0, OutWaterTempAtUA0 );
						SimSimpleEvapFluidCooler( int( Par( 2 ) ), Par( 3 ), Par( 4 ), UA1, OutWaterTempAtUA1 );
						ShowSevereError( CalledFrom + ": The combination of design input values did not allow the calculation of a " );
						ShowContinueError( "reasonable UA value. Review and revise design input values as appropriate. Specifying hard" );
						ShowContinueError( "sizes for some \"autosizable\" fields while autosizing other \"autosizable\" fields may be contributing to this problem." );
						ShowContinueError( "This model iterates on UA to find the heat transfer required to provide the design outlet " );
						ShowContinueError( "water temperature. Initially, the outlet water temperatures at high and low UA values are " );
						ShowContinueError( "calculated. The Design Exit Water Temperature should be between the outlet water " );
						ShowContinueError( "temperatures calculated at high and low UA values. If the Design Exit Water Temperature is " );
						ShowContinueError( "out of this range, the solution will not converge and UA will not be calculated. " );
						ShowContinueError( "The possible solutions could be to manually input adjusted water and/or air flow rates " );
						ShowContinueError( "based on the autosized values shown below or to adjust design evaporative fluid cooler air inlet wet-bulb temperature." );
						ShowContinueError( "Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp)." );
						ShowContinueError( "Inputs to the evaporative fluid cooler object:" );
						ShowContinueError( "Design Evaporative Fluid Cooler Load [W]                      = " + RoundSigDigits( Par( 1 ), 2 ) );
						ShowContinueError( "Design Evaporative Fluid Cooler Water Volume Flow Rate [m3/s] = " + RoundSigDigits( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate, 6 ) );
						ShowContinueError( "Design Evaporative Fluid Cooler Air Volume Flow Rate [m3/s]   = " + RoundSigDigits( Par( 4 ), 2 ) );
						ShowContinueError( "Design Evaporative Fluid Cooler Air Inlet Wet-bulb Temp [C]   = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, 2 ) );
						ShowContinueError( "Design Evaporative Fluid Cooler Water Inlet Temp [C]          = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp, 2 ) );
						ShowContinueError( "Inputs to the plant sizing object:" );
						ShowContinueError( "Design Exit Water Temp [C]                                    = " + RoundSigDigits( PlantSizData( PltSizCondNum ).ExitTemp, 2 ) );
						ShowContinueError( "Loop Design Temperature Difference [C]                        = " + RoundSigDigits( PlantSizData( PltSizCondNum ).DeltaT, 2 ) );
						ShowContinueError( "Design Evaporative Fluid Cooler Water Inlet Temp [C]          = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp, 2 ) );
						ShowContinueError( "Calculated water outlet temperature at low UA [C](UA = " + RoundSigDigits( UA0, 2 ) + " W/C)  = " + RoundSigDigits( OutWaterTempAtUA0, 2 ) );
						ShowContinueError( "Calculated water outlet temperature at high UA [C](UA = " + RoundSigDigits( UA1, 2 ) + " W/C)  = " + RoundSigDigits( OutWaterTempAtUA1, 2 ) );
						ShowFatalError( "Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
					}
					tmpHighSpeedEvapFluidCoolerUA = UA;
					if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA;
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity = DesEvapFluidCoolerLoad / SimpleEvapFluidCooler( EvapFluidCoolerNum ).HeatRejectCapNomCapSizingRatio;
				} else {
					tmpHighSpeedEvapFluidCoolerUA = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA;
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed ) {
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
								"U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
								"Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
						}
					} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
								"U-Factor Times Area Value at High Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
								"Initial U-Factor Times Area Value at High Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
						}
					}
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing error for evaporative fluid cooler object = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
					ShowFatalError( "Autosizing of evaporative fluid cooler UA requires a loop Sizing:Plant object." );
				}
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_StandardDesignCapacity ) {
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate >= SmallWaterVolFlow ) {
				// Standard Design Capacity doesn't include compressor heat;
				// predefined factor was 1.25 W heat rejection per W of delivered cooling, now a user input with 1.25 default
				rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, 35.0, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				DesEvapFluidCoolerLoad = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesEvapFluidCoolerLoad;
				Par( 2 ) = double( EvapFluidCoolerNum );
				Par( 3 ) = rho * SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate; // Design water mass flow rate
				Par( 4 ) = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate; // Design air volume flow rate
				Par( 5 ) = Cp;
				UA0 = 0.0001 * DesEvapFluidCoolerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesEvapFluidCoolerLoad; // Assume deltaT = 1K
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp = 35.0; // 95F design inlet water temperature
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress = StdBaroPress;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowWarningError( "Iteration limit exceeded in calculating evaporative fluid cooler UA." );
					ShowContinueError( "Autosizing of fluid cooler UA failed for evaporative fluid cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
					ShowContinueError( "The final UA value = " + RoundSigDigits( UA, 2 ) + "W/C, and the simulation continues..." );
				} else if ( SolFla == -2 ) {
					ShowSevereError( CalledFrom + ": The combination of design input values did not allow the calculation of a " );
					ShowContinueError( "reasonable UA value. Review and revise design input values as appropriate. " );
					ShowFatalError( "Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				}
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = UA;
			} else {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
				} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"U-Factor Times Area Value at High Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial U-Factor Times Area Value at High Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
				}
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_UserSpecifiedDesignCapacity ) {
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				DesEvapFluidCoolerLoad = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedUserSpecifiedDesignCapacity;
				Par( 1 ) = DesEvapFluidCoolerLoad;
				Par( 2 ) = double( EvapFluidCoolerNum );
				Par( 3 ) = rho * tmpDesignWaterFlowRate; // Design water mass flow rate
				Par( 4 ) = tmpHighSpeedAirFlowRate; // Design air volume flow rate
				Par( 5 ) = Cp;
				UA0 = 0.0001 * DesEvapFluidCoolerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesEvapFluidCoolerLoad; // Assume deltaT = 1K

				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress = StdBaroPress;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowWarningError( "Iteration limit exceeded in calculating evaporative fluid cooler UA." );
					ShowContinueError( "Autosizing of fluid cooler UA failed for evaporative fluid cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
					ShowContinueError( "The final UA value = " + RoundSigDigits( UA, 2 ) + "W/C, and the simulation continues..." );
				} else if ( SolFla == -2 ) {
					SimSimpleEvapFluidCooler( int( Par( 2 ) ), Par( 3 ), Par( 4 ), UA0, OutWaterTempAtUA0 );
					SimSimpleEvapFluidCooler( int( Par( 2 ) ), Par( 3 ), Par( 4 ), UA1, OutWaterTempAtUA1 );
					ShowSevereError( CalledFrom + ": The combination of design input values did not allow the calculation of a " );
					ShowContinueError( "reasonable UA value. Review and revise design input values as appropriate. Specifying hard" );
					ShowContinueError( "sizes for some \"autosizable\" fields while autosizing other \"autosizable\" fields may be contributing to this problem." );
					ShowContinueError( "This model iterates on UA to find the heat transfer required to provide the design outlet " );
					ShowContinueError( "water temperature. Initially, the outlet water temperatures at high and low UA values are " );
					ShowContinueError( "calculated. The Design Exit Water Temperature should be between the outlet water " );
					ShowContinueError( "temperatures calculated at high and low UA values. If the Design Exit Water Temperature is " );
					ShowContinueError( "out of this range, the solution will not converge and UA will not be calculated. " );
					ShowContinueError( "The possible solutions could be to manually input adjusted water and/or air flow rates " );
					ShowContinueError( "based on the autosized values shown below or to adjust design evaporative fluid cooler air inlet wet-bulb temperature." );
					ShowContinueError( "Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp)." );
					ShowContinueError( "Inputs to the evaporative fluid cooler object:" );
					ShowContinueError( "Design Evaporative Fluid Cooler Load [W]                      = " + RoundSigDigits( Par( 1 ), 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Water Volume Flow Rate [m3/s] = " + RoundSigDigits( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate, 6 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Air Volume Flow Rate [m3/s]   = " + RoundSigDigits( Par( 4 ), 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Air Inlet Wet-bulb Temp [C]   = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Water Inlet Temp [C]          = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp, 2 ) );
					ShowContinueError( "Inputs to the plant sizing object:" );
					ShowContinueError( "Design Exit Water Temp [C]                                    = " + RoundSigDigits( PlantSizData( PltSizCondNum ).ExitTemp, 2 ) );
					ShowContinueError( "Loop Design Temperature Difference [C]                        = " + RoundSigDigits( PlantSizData( PltSizCondNum ).DeltaT, 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Water Inlet Temp [C]          = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp, 2 ) );
					ShowContinueError( "Calculated water outlet temperature at low UA [C](UA = " + RoundSigDigits( UA0, 2 ) + " W/C)  = " + RoundSigDigits( OutWaterTempAtUA0, 2 ) );
					ShowContinueError( "Calculated water outlet temperature at high UA [C](UA = " + RoundSigDigits( UA1, 2 ) + " W/C)  = " + RoundSigDigits( OutWaterTempAtUA1, 2 ) );
					ShowFatalError( "Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				}
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = UA;
			} else {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_SingleSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
				} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"U-Factor Times Area Value at High Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( cEvapFluidCooler_TwoSpeed, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
							"Initial U-Factor Times Area Value at High Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA );
					}
				}
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRateSizingFactor * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate;
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
					"Air Flow Rate at Low Fan Speed [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
					"Initial Air Flow Rate at Low Fan Speed [m3/s]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate );
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPowerWasAutoSized && PlantFirstSizesOkayToFinalize ) {
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPowerSizingFactor * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower;
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
					"Fan Power at Low Fan Speed [W]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
					"Initial Fan Power at Low Fan Speed [W]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower );
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUAWasAutoSized && PlantFirstSizesOkayToFinalize ) {
			SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUASizingFactor * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA;
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
					"U-Factor Times Area Value at Low Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
					"Initial U-Factor Times Area Value at Low Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA );
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_StandardDesignCapacity
				&& SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate >= SmallWaterVolFlow
					&& SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedStandardDesignCapacity > 0.0 ) {
				// Standard design capacity doesn't include compressor heat;
				// predefined factor was 1.25 W heat rejection per W of delivered cooling, now user input with default 1.25
				rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				DesEvapFluidCoolerLoad = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedStandardDesignCapacity * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesEvapFluidCoolerLoad;
				Par( 2 ) = double( EvapFluidCoolerNum );
				Par( 3 ) = rho * tmpDesignWaterFlowRate; // Design water mass flow rate
				Par( 4 ) = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate; // Air volume flow rate at low fan speed
				Par( 5 ) = Cp;
				UA0 = 0.0001 * DesEvapFluidCoolerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesEvapFluidCoolerLoad; // Assume deltaT = 1K
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp = 35.0; // 95F design inlet water temperature
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress = StdBaroPress;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowWarningError( "Iteration limit exceeded in calculating evaporative fluid cooler UA." );
					ShowContinueError( "Autosizing of fluid cooler UA failed for evaporative fluid cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
					ShowContinueError( "The final UA value = " + RoundSigDigits( UA, 2 ) + "W/C, and the simulation continues..." );
				} else if ( SolFla == -2 ) {
					ShowSevereError( CalledFrom + ": The combination of design input values did not allow the calculation of a " );
					ShowContinueError( "reasonable low-speed UA value. Review and revise design input values as appropriate. " );
					ShowFatalError( "Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				}
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA = UA;
			} else {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
						"U-Factor Times Area Value at Low Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
						"Initial U-Factor Times Area Value at Low Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA );
				}
			}
		}

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).PerformanceInputMethod_Num == PIM_UserSpecifiedDesignCapacity
				&& SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed ) {
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignWaterFlowRate >= SmallWaterVolFlow
					&& SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedUserSpecifiedDesignCapacity > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, CalledFrom );
				DesEvapFluidCoolerLoad = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedUserSpecifiedDesignCapacity;
				Par( 1 ) = DesEvapFluidCoolerLoad;
				Par( 2 ) = double( EvapFluidCoolerNum );
				Par( 3 ) = rho * tmpDesignWaterFlowRate; // Design water mass flow rate
				Par( 4 ) = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate; // Air volume flow rate at low fan speed
				Par( 5 ) = Cp;
				UA0 = 0.0001 * DesEvapFluidCoolerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesEvapFluidCoolerLoad; // Assume deltaT = 1K
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringWaterTemp;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirTemp;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignEnteringAirWetBulbTemp;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress = StdBaroPress;
				SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating EvaporativeFluidCooler UA" );
					ShowFatalError( "Autosizing of EvaporativeFluidCooler UA failed for EvaporativeFluidCooler " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				} else if ( SolFla == -2 ) {
					SimSimpleEvapFluidCooler( int( Par( 2 ) ), Par( 3 ), Par( 4 ), UA0, OutWaterTempAtUA0 );
					SimSimpleEvapFluidCooler( int( Par( 2 ) ), Par( 3 ), Par( 4 ), UA1, OutWaterTempAtUA1 );
					ShowSevereError( CalledFrom + ": The combination of design input values did not allow the calculation of a " );
					ShowContinueError( "reasonable UA value. Review and revise design input values as appropriate. Specifying hard" );
					ShowContinueError( "sizes for some \"autosizable\" fields while autosizing other \"autosizable\" fields may be contributing to this problem." );
					ShowContinueError( "This model iterates on UA to find the heat transfer required to provide the design outlet " );
					ShowContinueError( "water temperature. Initially, the outlet water temperatures at high and low UA values are " );
					ShowContinueError( "calculated. The Design Exit Water Temperature should be between the outlet water " );
					ShowContinueError( "temperatures calculated at high and low UA values. If the Design Exit Water Temperature is " );
					ShowContinueError( "out of this range, the solution will not converge and UA will not be calculated. " );
					ShowContinueError( "Inputs to the Evaporative Fluid Cooler model are:" );
					ShowContinueError( "Design Evaporative Fluid Cooler Load                    = " + RoundSigDigits( Par( 1 ), 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Water Volume Flow Rate  = " + RoundSigDigits( Par( 3 ), 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Air Volume Flow Rate    = " + RoundSigDigits( Par( 4 ), 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Air Inlet Wet-bulb Temp = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Water Inlet Temp        = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp, 2 ) );
					ShowContinueError( "Design Exit Water Temp                                  = " + RoundSigDigits( PlantSizData( PltSizCondNum ).ExitTemp, 2 ) );
					ShowContinueError( "Design Evaporative Fluid Cooler Water Inlet Temp [C]    = " + RoundSigDigits( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp, 2 ) );
					ShowContinueError( "Calculated water outlet temperature at low UA(" + RoundSigDigits( UA0, 2 ) + ")  = " + RoundSigDigits( OutWaterTempAtUA0, 2 ) );
					ShowContinueError( "Calculated water outlet temperature at high UA(" + RoundSigDigits( UA1, 2 ) + ")  = " + RoundSigDigits( OutWaterTempAtUA1, 2 ) );
					ShowFatalError( "Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				}
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA = UA;
			} else {
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
						"U-Factor Times Area Value at Low Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType, SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name,
						"Initial U-Factor Times Area Value at Low Fan Speed [W/C]", SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA );
				}

			}
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType );
			PreDefTableEntry( pdchMechNomCap, equipName, SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedStandardDesignCapacity );
		}

	}

	// End Initialization Section for the EvaporativeFluidCoolers Module
	//******************************************************************************

	// Beginning of the EvaporativeFluidCoolers Module Simulation Subroutines
	// *****************************************************************************

	void
	CalcSingleSpeedEvapFluidCooler( int & EvapFluidCoolerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To simulate the operation of a single-speed fan evaporative fluid cooler.

		// METHODOLOGY EMPLOYED:
		// The evaporative fluid cooler is modeled using effectiveness-NTU relationships for
		// counterflow heat exchangers based on Merkel's theory.
		// The subroutine calculates the period of time required to meet a
		// leaving water temperature setpoint. It assumes that part-load
		// operation represents a linear interpolation of two steady-state regimes.
		// Cyclic losses are neglected. The period of time required to meet the
		// leaving water temperature setpoint is used to determine the required
		// fan power and energy.
		// A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
		// or schedule, of the evaporative fluid cooler. If the evaporative fluid cooler is OFF, outlet water
		// temperature and flow rate are passed through the model from inlet node to
		// outlet node without intervention. Reports are also updated with fan power and energy being zero.
		// When the RunFlag indicates an ON condition for the evaporative fluid cooler, the
		// mass flow rate and water temperature are read from the inlet node of the
		// evaporative fluid cooler (water-side). The outdoor air wet-bulb temperature is used
		// as the entering condition to the evaporative fluid cooler (air-side).
		// The evaporative fluid cooler fan is turned on and design parameters are used
		// to calculate the leaving water temperature.
		// If the calculated leaving water temperature is below the setpoint, a fan
		// run-time fraction is calculated and used to determine fan power. The leaving
		// water temperature setpoint is placed on the outlet node. If the calculated
		// leaving water temperature is at or above the setpoint, the calculated
		// leaving water temperature is placed on the outlet node and the fan runs at
		// full power. Water mass flow rate is passed from inlet node to outlet node
		// with no intervention.
		// REFERENCES:
		// ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

		// Based on SingleSpeedTower subroutine by Dan Fisher ,Sept 1998
		// Dec. 2008. BG. added RunFlag logic per original methodology

		// USE STATEMENTS:
		//  USE FluidProperties, ONLY : GetSpecificHeatGlycol
		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		Real64 InletWaterTemp;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSingleSpeedEvapFluidCooler" );
		int const MaxIteration( 100 ); // Maximum fluid bypass iteration calculations
		static std::string const MaxItChar( "100" );
		Real64 const BypassFractionThreshold( 0.01 ); // Threshold to stop bypass iteration
		Real64 const OWTLowerLimit( 0.0 ); // The limit of evaporative fluid cooler exit fluid temperature used
		// in the fluid bypass calculation to avoid fluid freezing. For water,
		// it is 0 degreeC and for glycols, it can be much lower. The fluid type
		// is stored at the loop. Current choices are Water and Steam,
		// needs to expand for glycols

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirFlowRate;
		Real64 UAdesign; // UA value at design conditions (entered by user or calculated)
		Real64 FanModeFrac;
		Real64 FanPowerOn;
		Real64 CpWater;
		Real64 TempSetPoint;

		//Added variables for fluid bypass
		int NumIteration;
		int CapacityControl; // Capacity Control (0 - FanCycling, 1 - FluidBypass)
		int BypassFlag; // Flag indicator for fluid bypass (0 - no bypass, 1 - bypass)
		Real64 BypassFraction; // Fluid bypass fraction
		Real64 BypassFraction2; // Fluid bypass fraction
		Real64 BypassFractionPrev;
		Real64 OutletWaterTempPrev;
		int LoopNum;
		int LoopSideNum;

		//set inlet and outlet nodes
		WaterInletNode = SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum;
		WaterOutletNode = SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		FanPower = 0.0;
		InletWaterTemp = Node( WaterInletNode ).Temp;
		OutletWaterTemp = InletWaterTemp;
		LoopNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum;
		LoopSideNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum;
		AirFlowRate = 0.0;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
		}}

		// Added for fluid bypass. First assume no fluid bypass
		BypassFlag = 0;
		BypassFraction = 0.0;
		BypassFraction2 = 0.0;
		SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction = 0.0;
		CapacityControl = SimpleEvapFluidCooler( EvapFluidCoolerNum ).CapacityControl;

		//   MassFlowTol is a parameter to indicate a no flow condition
		if ( WaterMassFlowRate <= MassFlowTolerance || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) return;

		if ( InletWaterTemp > TempSetPoint ) {
			//     Turn on evaporative fluid cooler fan
			UAdesign = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA;
			AirFlowRate = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate;
			FanPowerOn = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower;

			SimSimpleEvapFluidCooler( EvapFluidCoolerNum, WaterMassFlowRate, AirFlowRate, UAdesign, OutletWaterTemp );

			if ( OutletWaterTemp <= TempSetPoint ) {
				if ( CapacityControl == 0 || OutletWaterTemp <= OWTLowerLimit ) {
					//         Setpoint was met with pump ON and fan ON, calculate run-time fraction
					FanModeFrac = ( TempSetPoint - InletWaterTemp ) / ( OutletWaterTemp - InletWaterTemp );
					FanPower = FanModeFrac * FanPowerOn;
					OutletWaterTemp = TempSetPoint;
				} else {
					//FluidBypass, fan runs at full speed for the entire time step
					FanModeFrac = 1.0;
					FanPower = FanPowerOn;
					BypassFlag = 1;
				}
			} else {
				//       Setpoint was not met, evaporative fluid cooler ran at full capacity
				FanModeFrac = 1.0;
				FanPower = FanPowerOn;
			}
		} else if ( InletWaterTemp <= TempSetPoint ) {
			//Inlet water temperature lower than setpoint, assume 100% bypass, evaporative fluid cooler fan off
			if ( CapacityControl == 1 ) {
				if ( InletWaterTemp > OWTLowerLimit ) {
					FanPower = 0.0;
					BypassFraction = 1.0;
					SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction = 1.0;
					OutletWaterTemp = InletWaterTemp;
				}
			}
		}

		// Calculate bypass fraction since OWTLowerLimit < OutletWaterTemp < TempSetPoint.
		// The iteration ends when the numer of iteration exceeds the limit or the difference
		//  between the new and old bypass fractions is less than the threshold.
		if ( BypassFlag == 1 ) {
			BypassFraction = ( TempSetPoint - OutletWaterTemp ) / ( InletWaterTemp - OutletWaterTemp );
			if ( BypassFraction > 1.0 || BypassFraction < 0.0 ) {
				// Bypass cannot meet setpoint, assume no bypass
				BypassFlag = 0;
				BypassFraction = 0.0;
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction = 0.0;
				AirFlowRate = 0.0;
			} else {
				NumIteration = 0;
				BypassFractionPrev = BypassFraction;
				OutletWaterTempPrev = OutletWaterTemp;
				while ( NumIteration < MaxIteration ) {
					++NumIteration;
					// need to iterate for the new OutletWaterTemp while bypassing evaporative fluid cooler water
					SimSimpleEvapFluidCooler( EvapFluidCoolerNum, WaterMassFlowRate * ( 1.0 - BypassFraction ), AirFlowRate, UAdesign, OutletWaterTemp );
					// Calc new BypassFraction based on the new OutletWaterTemp
					if ( std::abs( OutletWaterTemp - OWTLowerLimit ) <= 0.01 ) {
						BypassFraction2 = BypassFraction;
						break;
					} else if ( OutletWaterTemp < OWTLowerLimit ) {
						// Set OutletWaterTemp = OWTLowerLimit, and use linear interpolation to calculate the bypassFraction
						BypassFraction2 = BypassFractionPrev - ( BypassFractionPrev - BypassFraction ) * ( OutletWaterTempPrev - OWTLowerLimit ) / ( OutletWaterTempPrev - OutletWaterTemp );
						SimSimpleEvapFluidCooler( EvapFluidCoolerNum, WaterMassFlowRate * ( 1.0 - BypassFraction2 ), AirFlowRate, UAdesign, OutletWaterTemp );
						if ( OutletWaterTemp < OWTLowerLimit ) {
							//Use previous iteraction values
							BypassFraction2 = BypassFractionPrev;
							OutletWaterTemp = OutletWaterTempPrev;
						}
						break;
					} else {
						BypassFraction2 = ( TempSetPoint - OutletWaterTemp ) / ( InletWaterTemp - OutletWaterTemp );
					}
					// Compare two BypassFraction to determine when to stop
					if ( std::abs( BypassFraction2 - BypassFraction ) <= BypassFractionThreshold ) break;
					BypassFractionPrev = BypassFraction;
					OutletWaterTempPrev = OutletWaterTemp;
					BypassFraction = BypassFraction2;
				}
				if ( NumIteration > MaxIteration ) {
					ShowWarningError( "Evaporative fluid cooler fluid bypass iteration exceeds maximum limit of " + MaxItChar + " for " + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name );
				}
				SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction = BypassFraction2;
				// may not meet TempSetPoint due to limit of evaporative fluid cooler outlet temp to OWTLowerLimit
				OutletWaterTemp = ( 1.0 - BypassFraction2 ) * OutletWaterTemp + BypassFraction2 * InletWaterTemp;
			}
		}

		//Should this be water inlet node num?????
		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, RoutineName );
		Qactual = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );
		AirFlowRateRatio = AirFlowRate / SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate;

	}

	void
	CalcTwoSpeedEvapFluidCooler( int & EvapFluidCoolerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To simulate the operation of a evaporative fluid cooler with a two-speed fan.

		// METHODOLOGY EMPLOYED:
		// The evaporative fluid cooler is modeled using effectiveness-NTU relationships for
		// counterflow heat exchangers based on Merkel's theory.
		// The subroutine calculates the period of time required to meet a
		// leaving water temperature setpoint. It assumes that part-load
		// operation represents a linear interpolation of three steady-state regimes
		// (high-speed fan operation and low-speed fan operation ).
		// Cyclic losses are neglected. The period of time required to meet the
		// leaving water temperature setpoint is used to determine the required
		// fan power and energy. When the leaving water temperature is at or above the setpoint
		// the evaporative fluid cooler fan is turned on,
		// .
		// A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
		// or schedule, of the evaporative fluid cooler. If the evaporative fluid cooler is OFF, outlet water
		// temperature and flow rate are passed through the model from inlet node to
		// outlet node without intervention. Reports are also updated with fan power and fan energy being zero.
		// When the RunFlag indicates an ON condition for the evaporative fluid cooler, the
		// mass flow rate and water temperature are read from the inlet node of the
		// evaporative fluid cooler (water-side). The outdoor air wet-bulb temperature is used
		// as the entering condition to the evaporative fluid cooler (air-side). If the incoming
		// water temperature is above the setpoint, the evaporative fluid cooler fan is turned on
		// and parameters for low fan speed are used to again calculate the leaving
		// water temperature. If the calculated leaving water temperature is
		// below the setpoint, a fan run-time fraction (FanModeFrac) is calculated and
		// used to determine fan power. The leaving water temperature setpoint is placed
		// on the outlet node. If the calculated leaving water temperature is at or above
		// the setpoint, the evaporative fluid cooler fan is turned on 'high speed' and the routine is
		// repeated. If the calculated leaving water temperature is below the setpoint,
		// a fan run-time fraction is calculated for the second stage fan and fan power
		// is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
		// If the calculated leaving water temperature is above the leaving water temp.
		// setpoint, the calculated leaving water temperature is placed on the outlet
		// node and the fan runs at full power (High Speed Fan Power). Water mass flow
		// rate is passed from inlet node to outlet node with no intervention.
		// REFERENCES:
		// ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
		// Based on TwoSpeedTower by Dan Fisher ,Sept. 1998
		// Dec. 2008. BG. added RunFlag logic per original methodology

		// USE STATEMENTS:
		//  USE FluidProperties, ONLY : GetSpecificHeatGlycol
		// Using/Aliasing
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		Real64 InletWaterTemp;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcTwoSpeedEvapFluidCooler" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirFlowRate;
		Real64 UAdesign; // UA value at design conditions (entered by user) [W/C]
		Real64 OutletWaterTemp1stStage;
		Real64 OutletWaterTemp2ndStage;
		Real64 FanModeFrac;
		Real64 FanPowerLow;
		Real64 FanPowerHigh;
		Real64 CpWater;
		Real64 TempSetPoint;
		int LoopNum;
		int LoopSideNum;

		//set inlet and outlet nodes

		WaterInletNode = SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum;
		WaterOutletNode = SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		FanPower = 0.0;
		InletWaterTemp = Node( WaterInletNode ).Temp;
		OutletWaterTemp = InletWaterTemp;

		OutletWaterTemp1stStage = OutletWaterTemp;
		OutletWaterTemp2ndStage = OutletWaterTemp;
		FanModeFrac = 0.0;
		AirFlowRate = 0.0;
		LoopNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum;
		LoopSideNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
		}}

		//   MassFlowTol is a parameter to indicate a no flow condition
		if ( WaterMassFlowRate <= MassFlowTolerance || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) return;

		if ( InletWaterTemp > TempSetPoint ) {
			//     Setpoint was not met ,turn on evaporative fluid cooler 1st stage fan
			UAdesign = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedEvapFluidCoolerUA;
			AirFlowRate = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedAirFlowRate;
			FanPowerLow = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LowSpeedFanPower;

			SimSimpleEvapFluidCooler( EvapFluidCoolerNum, WaterMassFlowRate, AirFlowRate, UAdesign, OutletWaterTemp1stStage );

			if ( OutletWaterTemp1stStage <= TempSetPoint ) {
				//         Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
				FanModeFrac = ( TempSetPoint - InletWaterTemp ) / ( OutletWaterTemp1stStage - InletWaterTemp );
				FanPower = FanModeFrac * FanPowerLow;
				OutletWaterTemp = TempSetPoint;
				Qactual *= FanModeFrac;
			} else {
				//         Setpoint was not met, turn on evaporative fluid cooler 2nd stage fan
				UAdesign = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedEvapFluidCoolerUA;
				AirFlowRate = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate;
				FanPowerHigh = SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedFanPower;

				SimSimpleEvapFluidCooler( EvapFluidCoolerNum, WaterMassFlowRate, AirFlowRate, UAdesign, OutletWaterTemp2ndStage );

				if ( ( OutletWaterTemp2ndStage <= TempSetPoint ) && UAdesign > 0.0 ) {
					//           Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
					FanModeFrac = ( TempSetPoint - OutletWaterTemp1stStage ) / ( OutletWaterTemp2ndStage - OutletWaterTemp1stStage );
					FanPower = ( FanModeFrac * FanPowerHigh ) + ( 1.0 - FanModeFrac ) * FanPowerLow;
					OutletWaterTemp = TempSetPoint;
				} else {
					//           Setpoint was not met, evaporative fluid cooler ran at full capacity
					OutletWaterTemp = OutletWaterTemp2ndStage;
					FanPower = FanPowerHigh;
				}

			}

		}

		//Should this be water inlet node num??
		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, RoutineName );
		Qactual = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );
		AirFlowRateRatio = AirFlowRate / SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate;

	}

	void
	SimSimpleEvapFluidCooler(
		int const EvapFluidCoolerNum,
		Real64 const WaterMassFlowRate,
		Real64 const AirFlowRate,
		Real64 const UAdesign,
		Real64 & OutletWaterTemp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// See purpose for single speed or Two speed evaporative fluid cooler model

		// METHODOLOGY EMPLOYED:
		// See methodology for single speed or two speed evaporative fluid cooler model

		// REFERENCES:
		// Based on SimTower subroutine by Dan Fisher Sept. 1998
		// Merkel, F. 1925.  Verduftungskuhlung. VDI Forschungsarbeiten, Nr 275, Berlin.
		// ASHRAE     1999.  HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculations.

		// USE STATEMENTS:
		//  USE FluidProperties, ONLY : GetSpecificHeatGlycol

		// Locals
		Real64 Qactual; // Actual heat transfer rate between evaporative fluid cooler water and air [W]

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const IterMax( 50 ); // Maximum number of iterations allowed
		Real64 const WetBulbTolerance( 0.00001 ); // Maximum error for exiting wet-bulb temperature between iterations
		// [delta K/K]
		Real64 const DeltaTwbTolerance( 0.001 ); // Maximum error (tolerance) in DeltaTwb for iteration convergence [C]
		static std::string const RoutineName( "SimSimpleEvapFluidCooler" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Iter; // Number of iterations completed
		Real64 MdotCpWater; // Water mass flow rate times the heat capacity [W/K]
		Real64 InletAirTemp; // Dry-bulb temperature of air entering the evaporative fluid cooler [C]
		Real64 CpWater; // Heat capacity of water [J/kg/K]
		Real64 CpAir; // Heat capacity of air [J/kg/K]
		Real64 AirDensity; // Density of air [kg/m3]
		Real64 AirMassFlowRate; // Mass flow rate of air [kg/s]
		Real64 effectiveness; // Effectiveness of the heat exchanger [-]
		Real64 UAactual; // UA value at actual conditions [W/C]
		Real64 InletAirEnthalpy; // Enthalpy of entering moist air [J/kg]
		Real64 InletAirWetBulb; // Wetbulb temp of entering moist air [C]
		Real64 OutletAirEnthalpy; // Enthalpy of exiting moist air [J/kg]
		Real64 OutletAirWetBulb; // Wetbulb temp of exiting moist air [C]
		Real64 OutletAirWetBulbLast; // temporary Wetbulb temp of exiting moist air [C]
		Real64 AirCapacity; // MdotCp of air through the evaporative fluid cooler
		Real64 CapacityRatioMin; // Minimum capacity of airside and waterside
		Real64 CapacityRatioMax; // Maximum capacity of airside and waterside
		Real64 CapacityRatio; // Ratio of minimum to maximum capacity
		Real64 NumTransferUnits; // Number of transfer Units [NTU]
		Real64 WetBulbError; // Calculated error for exiting wet-bulb temperature between iterations [delta K/K]
		Real64 CpAirside; // Delta enthalpy of the evaporative fluid cooler air /
		// delta air wet-bulb temp [J/kg/K]
		Real64 DeltaTwb; // Absolute value of difference between inlet and outlet air wet-bulb temp [C]

		// set inlet and outlet node numbers, and initialize some local variables

		WaterInletNode = SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterInletNodeNum;
		WaterOutletNode = SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		WetBulbError = 1.0;
		DeltaTwb = 1.0;

		// set local evaporative fluid cooler inlet and outlet temperature variables
		InletWaterTemp = SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).WaterTemp;
		OutletWaterTemp = InletWaterTemp;
		InletAirTemp = SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp;
		InletAirWetBulb = SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb;

		if ( UAdesign == 0.0 ) return;

		// set water and air properties
		AirDensity = PsyRhoAirFnPbTdbW( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress, InletAirTemp, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat );
		AirMassFlowRate = AirFlowRate * AirDensity;
		CpAir = PsyCpAirFnWTdb( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat, InletAirTemp );
		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, InletWaterTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, RoutineName );
		InletAirEnthalpy = PsyHFnTdbRhPb( InletAirWetBulb, 1.0, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );

		// initialize exiting wet bulb temperature before iterating on final solution
		OutletAirWetBulb = InletAirWetBulb + 6.0;

		// Calcluate mass flow rates
		MdotCpWater = WaterMassFlowRate * CpWater;
		Iter = 0;
		while ( ( WetBulbError > WetBulbTolerance ) && ( Iter <= IterMax ) && ( DeltaTwb > DeltaTwbTolerance ) ) {
			++Iter;
			OutletAirEnthalpy = PsyHFnTdbRhPb( OutletAirWetBulb, 1.0, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );
			// calculate the airside specific heat and capacity
			CpAirside = ( OutletAirEnthalpy - InletAirEnthalpy ) / ( OutletAirWetBulb - InletAirWetBulb );
			AirCapacity = AirMassFlowRate * CpAirside;
			// calculate the minimum to maximum capacity ratios of airside and waterside
			CapacityRatioMin = min( AirCapacity, MdotCpWater );
			CapacityRatioMax = max( AirCapacity, MdotCpWater );
			CapacityRatio = CapacityRatioMin / CapacityRatioMax;
			// Calculate heat transfer coefficient and number of transfer units (NTU)
			UAactual = UAdesign * CpAirside / CpAir;
			NumTransferUnits = UAactual / CapacityRatioMin;
			// calculate heat exchanger effectiveness
			if ( CapacityRatio <= 0.995 ) {
				effectiveness = ( 1.0 - std::exp( -1.0 * NumTransferUnits * ( 1.0 - CapacityRatio ) ) ) / ( 1.0 - CapacityRatio * std::exp( -1.0 * NumTransferUnits * ( 1.0 - CapacityRatio ) ) );
			} else {
				effectiveness = NumTransferUnits / ( 1.0 + NumTransferUnits );
			}
			// calculate water to air heat transfer and store last exiting WB temp of air
			Qactual = effectiveness * CapacityRatioMin * ( InletWaterTemp - InletAirWetBulb );
			OutletAirWetBulbLast = OutletAirWetBulb;
			// calculate new exiting wet bulb temperature of airstream
			OutletAirWetBulb = InletAirWetBulb + Qactual / AirCapacity;
			// Check error tolerance and exit if satisfied
			DeltaTwb = std::abs( OutletAirWetBulb - InletAirWetBulb );
			// Add KelvinConv to denominator below convert OutletAirWetBulbLast to Kelvin to avoid divide by zero.
			// Wet bulb error units are delta K/K
			WetBulbError = std::abs( ( OutletAirWetBulb - OutletAirWetBulbLast ) / ( OutletAirWetBulbLast + KelvinConv ) );
		}

		if ( Qactual >= 0.0 ) {
			OutletWaterTemp = InletWaterTemp - Qactual / MdotCpWater;
		} else {
			OutletWaterTemp = InletWaterTemp;
		}

	}

	Real64
	SimpleEvapFluidCoolerUAResidual(
		Real64 const UA, // UA of evaporative fluid cooler
		Array1< Real64 > const & Par // par(1) = design evaporative fluid cooler load [W]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Design evaporative fluid cooler load - evaporative fluid cooler cooling output)
		//                                    / Design evaporative fluid cooler load.
		// Evaporative fluid cooler Cooling Output depends on the UA which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Puts UA into the evaporative fluid cooler data structure, calls SimSimpleEvapFluidCooler, and calculates
		// the residual as defined above.

		// REFERENCES:
		// Based on SimpleTowerUAResidual by Fred Buhl, May 2002

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = Evaporative fluid cooler number
		// par(3) = design water mass flow rate [kg/s]
		// par(4) = design air volume flow rate [m3/s]
		// par(5) = water specific heat [J/(kg*C)]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int EvapFluidCoolerIndex; // index of this evaporative fluid cooler
		Real64 OutWaterTemp; // outlet water temperature [C]
		Real64 CoolingOutput; // Evaporative fluid cooler cooling output [W]

		EvapFluidCoolerIndex = int( Par( 2 ) );
		SimSimpleEvapFluidCooler( EvapFluidCoolerIndex, Par( 3 ), Par( 4 ), UA, OutWaterTemp );
		CoolingOutput = Par( 5 ) * Par( 3 ) * ( SimpleEvapFluidCoolerInlet( EvapFluidCoolerIndex ).WaterTemp - OutWaterTemp );
		Residuum = ( Par( 1 ) - CoolingOutput ) / Par( 1 );
		return Residuum;
	}

	// End of the EvaporativeFluidCoolers Module Simulation Subroutines
	// *****************************************************************************

	void
	CalculateWaterUseage( int const EvapFluidCoolerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Collect evaporative fluid cooler water useage calculations for
		// reuse by all the evaporative fluid cooler models.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Based on CalculateWaterUseage subroutine for cooling tower by B. Griffith, August 2006

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalculateWaterUseage" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirDensity;
		Real64 AirMassFlowRate;
		Real64 AvailTankVdot;
		static Real64 BlowDownVdot( 0.0 );
		static Real64 DriftVdot( 0.0 );
		static Real64 EvapVdot( 0.0 );
		Real64 InletAirEnthalpy;
		Real64 InSpecificHumRat;
		Real64 OutSpecificHumRat;
		Real64 TairAvg;
		Real64 MakeUpVdot;
		Real64 OutletAirEnthalpy;
		Real64 OutletAirHumRatSat;
		Real64 OutletAirTSat;
		Real64 StarvedVdot;
		Real64 TankSupplyVdot;
		Real64 rho;
		Real64 AverageWaterTemp;

		AverageWaterTemp = ( InletWaterTemp + OutletWaterTemp ) / 2.0;

		// Set water and air properties
		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode == EvapLossByMoistTheory ) {

			AirDensity = PsyRhoAirFnPbTdbW( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat );
			AirMassFlowRate = AirFlowRateRatio * SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighSpeedAirFlowRate * AirDensity;
			InletAirEnthalpy = PsyHFnTdbRhPb( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirWetBulb, 1.0, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );

			if ( AirMassFlowRate > 0.0 ) {
				// Calculate outlet air conditions for determining water usage

				OutletAirEnthalpy = InletAirEnthalpy + Qactual / AirMassFlowRate;
				OutletAirTSat = PsyTsatFnHPb( OutletAirEnthalpy, SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirPress );
				OutletAirHumRatSat = PsyWFnTdbH( OutletAirTSat, OutletAirEnthalpy );

				// calculate specific humidity ratios (HUMRAT to mass of moist air not dry air)
				InSpecificHumRat = SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat / ( 1 + SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirHumRat );
				OutSpecificHumRat = OutletAirHumRatSat / ( 1 + OutletAirHumRatSat );

				// calculate average air temp for density call
				TairAvg = ( SimpleEvapFluidCoolerInlet( EvapFluidCoolerNum ).AirTemp + OutletAirTSat ) / 2.0;

				// Amount of water evaporated
				rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, TairAvg, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, RoutineName );
				EvapVdot = ( AirMassFlowRate * ( OutSpecificHumRat - InSpecificHumRat ) ) / rho; // [m3/s]
				if ( EvapVdot < 0.0 ) EvapVdot = 0.0;
			} else {
				EvapVdot = 0.0;
			}

		} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode == EvapLossByUserFactor ) {
			rho = GetDensityGlycol( PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidName, AverageWaterTemp, PlantLoop( SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum ).FluidIndex, RoutineName );
			EvapVdot = SimpleEvapFluidCooler( EvapFluidCoolerNum ).UserEvapLossFactor * ( InletWaterTemp - OutletWaterTemp ) * ( WaterMassFlowRate / rho );
			if ( EvapVdot < 0.0 ) EvapVdot = 0.0;
		} else {
			// should never come here
		}

		//   amount of water lost due to drift
		DriftVdot = SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesignSprayWaterFlowRate * SimpleEvapFluidCooler( EvapFluidCoolerNum ).DriftLossFraction * AirFlowRateRatio;

		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode == BlowdownBySchedule ) {
			// Amount of water lost due to blow down (purging contaminants from evaporative fluid cooler basin)
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SchedIDBlowdown > 0 ) {
				BlowDownVdot = GetCurrentScheduleValue( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SchedIDBlowdown );
			} else {
				BlowDownVdot = 0.0;
			}
		} else if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).BlowdownMode == BlowdownByConcentration ) {
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio > 2.0 ) { // protect divide by zero
				BlowDownVdot = EvapVdot / ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).ConcentrationRatio - 1 ) - DriftVdot;
			} else {
				BlowDownVdot = EvapVdot - DriftVdot;
			}
			if ( BlowDownVdot < 0.0 ) BlowDownVdot = 0.0;
		} else {
			//should never come here
		}

		// Added for fluid bypass
		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).CapacityControl == 1 ) {
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapLossMode == EvapLossByUserFactor ) EvapVdot *= ( 1 - SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction );
			DriftVdot *= ( 1 - SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction );
			BlowDownVdot *= ( 1 - SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction );
		}

		MakeUpVdot = EvapVdot + DriftVdot + BlowDownVdot;

		// set demand request in Water STorage if needed
		StarvedVdot = 0.0;
		TankSupplyVdot = 0.0;
		if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SuppliedByWaterSystem ) {

			// set demand request
			WaterStorage( SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankID ).VdotRequestDemand( SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankDemandARRID ) = MakeUpVdot;

			AvailTankVdot = WaterStorage( SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankID ).VdotAvailDemand( SimpleEvapFluidCooler( EvapFluidCoolerNum ).WaterTankDemandARRID ); // check what tank can currently provide

			TankSupplyVdot = MakeUpVdot; // init
			if ( AvailTankVdot < MakeUpVdot ) { // calculate starved flow
				StarvedVdot = MakeUpVdot - AvailTankVdot;
				TankSupplyVdot = AvailTankVdot;
			}
		} else { // supplied by mains

		}

		//   total water usage
		// update report variables
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).EvaporationVdot = EvapVdot;
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).EvaporationVol = EvapVdot * ( TimeStepSys * SecInHour );
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).DriftVdot = DriftVdot;
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).DriftVol = DriftVdot * ( TimeStepSys * SecInHour );
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).BlowdownVdot = BlowDownVdot;
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).BlowdownVol = BlowDownVdot * ( TimeStepSys * SecInHour );
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).MakeUpVdot = MakeUpVdot;
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).MakeUpVol = MakeUpVdot * ( TimeStepSys * SecInHour );
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).TankSupplyVdot = TankSupplyVdot;
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).TankSupplyVol = TankSupplyVdot * ( TimeStepSys * SecInHour );
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).StarvedMakeUpVdot = StarvedVdot;
		SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).StarvedMakeUpVol = StarvedVdot * ( TimeStepSys * SecInHour );

	}

	// Beginning of Record Keeping subroutines for the EvaporativeFluidCooler Module
	// *****************************************************************************

	void
	UpdateEvapFluidCooler( int const EvapFluidCoolerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Chandan Sharma
		//       DATE WRITTEN:    May 2009
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for passing results to the outlet water node.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//unused0909  USE DataEnvironment, ONLY: EnvironmentName, CurMnDy
		// Using/Aliasing
		using General::TrimSigDigits;
		//  USE FluidProperties, ONLY : GetDensityGlycol
		//  USE DataPlant, ONLY: PlantLoop

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt LowTempFmt( "(' ',F6.2)" );
		Real64 const TempAllowance( 0.02 ); // Minimum difference b/w fluid cooler water outlet temp and
		// minimum condenser loop temp [C]

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CharErrOut;
		std::string CharLowOutletTemp;
		Real64 TempDifference;
		int LoopNum;
		int LoopSideNum;
		Real64 LoopMinTemp;

		// set node information

		Node( WaterOutletNode ).Temp = OutletWaterTemp;

		LoopNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopNum;
		LoopSideNum = SimpleEvapFluidCooler( EvapFluidCoolerNum ).LoopSideNum;
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 || WarmupFlag ) return;

		// Check flow rate through evaporative fluid cooler and compare to design flow rate,
		// show warning if greater than Design * Mulitplier
		if ( Node( WaterOutletNode ).MassFlowRate > SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesWaterMassFlowRate * SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerMassFlowRateMultiplier ) {
			++SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighMassFlowErrorCount;
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighMassFlowErrorCount < 2 ) {
				ShowWarningError( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\"" );
				ShowContinueError( " Condenser Loop Mass Flow Rate is much greater than the evaporative fluid coolers design mass flow rate." );
				ShowContinueError( " Condenser Loop Mass Flow Rate = " + TrimSigDigits( Node( WaterOutletNode ).MassFlowRate, 6 ) );
				ShowContinueError( " Evaporative Fluid Cooler Design Mass Flow Rate   = " + TrimSigDigits( SimpleEvapFluidCooler( EvapFluidCoolerNum ).DesWaterMassFlowRate, 6 ) );
				ShowContinueErrorTimeStamp( "" );
			} else {
				ShowRecurringWarningErrorAtEnd( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\"  Condenser Loop Mass Flow Rate is much greater than the evaporative fluid coolers design mass flow rate error", SimpleEvapFluidCooler( EvapFluidCoolerNum ).HighMassFlowErrorIndex, Node( WaterOutletNode ).MassFlowRate, Node( WaterOutletNode ).MassFlowRate );
			}
		}

		// Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
		LoopMinTemp = PlantLoop( LoopNum ).MinTemp;
		TempDifference = PlantLoop( LoopNum ).MinTemp - OutletWaterTemp;
		if ( TempDifference > TempAllowance && WaterMassFlowRate > 0.0 ) {
			++SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutletWaterTempErrorCount;
			gio::write( CharLowOutletTemp, LowTempFmt ) << LoopMinTemp;
			gio::write( CharErrOut, LowTempFmt ) << OutletWaterTemp;
			strip( CharErrOut );
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutletWaterTempErrorCount < 2 ) {
				ShowWarningError( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\"" );
				ShowContinueError( "Evaporative fluid cooler water outlet temperature (" + CharErrOut + " C) is below the specified minimum condenser loop temp of " + stripped( CharLowOutletTemp ) + " C" );
				ShowContinueErrorTimeStamp( "" );
			} else {
				ShowRecurringWarningErrorAtEnd( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\" Evaporative fluid cooler water outlet temperature is below the specified minimum condenser loop temp error", SimpleEvapFluidCooler( EvapFluidCoolerNum ).OutletWaterTempErrorIndex, OutletWaterTemp, OutletWaterTemp );
			}
		}

		// Check if water mass flow rate is small (e.g. no flow) and warn user
		if ( WaterMassFlowRate > 0.0 && WaterMassFlowRate <= MassFlowTolerance ) {
			++SimpleEvapFluidCooler( EvapFluidCoolerNum ).SmallWaterMassFlowErrorCount;
			if ( SimpleEvapFluidCooler( EvapFluidCoolerNum ).SmallWaterMassFlowErrorCount < 2 ) {
				ShowWarningError( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\"" );
				ShowContinueError( "Evaporative fluid cooler water mass flow rate near zero." );
				ShowContinueErrorTimeStamp( "" );
				ShowContinueError( "Actual Mass flow = " + TrimSigDigits( WaterMassFlowRate, 2 ) );
			} else {
				ShowRecurringWarningErrorAtEnd( SimpleEvapFluidCooler( EvapFluidCoolerNum ).EvapFluidCoolerType + " \"" + SimpleEvapFluidCooler( EvapFluidCoolerNum ).Name + "\" Evaporative fluid cooler water mass flow rate near zero error continues...", SimpleEvapFluidCooler( EvapFluidCoolerNum ).SmallWaterMassFlowErrorIndex, WaterMassFlowRate, WaterMassFlowRate );
			}
		}

		//   ! Check if water mass flow rate is lower than loop minimum and warn user
		//   IF(WaterMassFlowRate .LT. LoopMassFlowRateMinAvail)THEN
		//     SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrCount =   &
		//        SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrCount + 1
		//     IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrCount < 2) THEN
		//       CALL ShowWarningError(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
		//          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'"')
		//       CALL ShowContinueError('Evaporative fluid cooler water mass flow below loop minimum.')
		//       CALL ShowContinueErrorTimeStamp(' ')
		//       CALL ShowContinueError('Actual Mass flow  = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
		//       CALL ShowContinueError('Loop Minimum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMinAvail,2)))
		//     ELSE
		//       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
		//          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
		//          '" Evaporative fluid cooler water mass flow rate below loop minimum error continues...' &
		//          , SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
		//     ENDIF
		//   END IF
		//   ! Check if water mass flow rate is greater than loop maximum and warn user
		//   IF(WaterMassFlowRate .GT. LoopMassFlowRateMaxAvail)THEN
		//     SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount =   &
		//        SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount + 1
		//     IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount < 2) THEN
		//       CALL ShowWarningError(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
		//          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'"')
		//       CALL ShowContinueError('Evaporative fluid cooler water mass flow above loop maximum.')
		//       CALL ShowContinueErrorTimeStamp(' ')
		//       CALL ShowContinueError('Actual Mass flow = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
		//       CALL ShowContinueError('Loop Maximum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMaxAvail,2)))
		//     ELSE
		//       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//' "'//  &
		//           TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
		//          '" Evaporative fluid cooler water mass flow rate above loop maximum error continues...' &
		//          , SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
		//     ENDIF
		//   END IF

	}

	// End of Record Keeping subroutines for the EvaporativeFluidCooler Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the EvaporativeFluidCooler Module
	// *****************************************************************************

	void
	ReportEvapFluidCooler(
		bool const RunFlag,
		int const EvapFluidCoolerNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Chandan Sharma
		//       DATE WRITTEN:    May 2009
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variables for the evaporative fluid cooler.

		// METHODOLOGY EMPLOYED:
		// na

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
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;

		if ( ! RunFlag ) {
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).InletWaterTemp = Node( WaterInletNode ).Temp;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).OutletWaterTemp = Node( WaterInletNode ).Temp;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).WaterMassFlowRate = WaterMassFlowRate;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).Qactual = 0.0;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanPower = 0.0;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanEnergy = 0.0;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).AirFlowRatio = 0.0;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).WaterAmountUsed = 0.0;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).BypassFraction = 0.0; // added for fluid bypass
		} else {
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).InletWaterTemp = Node( WaterInletNode ).Temp;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).OutletWaterTemp = OutletWaterTemp;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).WaterMassFlowRate = WaterMassFlowRate;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).Qactual = Qactual;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanPower = FanPower;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).FanEnergy = FanPower * ReportingConstant;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).AirFlowRatio = AirFlowRateRatio;
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).WaterAmountUsed = WaterUsage * ReportingConstant;
			// added for fluid bypass
			SimpleEvapFluidCoolerReport( EvapFluidCoolerNum ).BypassFraction = SimpleEvapFluidCooler( EvapFluidCoolerNum ).BypassFraction;
		}

		// set

	}

} // EvaporativeFluidCoolers

} // EnergyPlus
