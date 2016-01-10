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
#include <CondenserLoopTowers.hh>
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
#include <GeneralRoutines.hh>
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

namespace CondenserLoopTowers {

	// Module containing the routines dealing with the objects COOLING TOWER:SINGLE SPEED,
	// COOLING TOWER:TWO SPEED, and COOLING TOWER:VARIABLE SPEED

	// MODULE INFORMATION:
	//       AUTHOR         Dan Fisher
	//       DATE WRITTEN   April 1998
	//       MODIFIED       Shirey, Raustad: Dec 2000; Shirey, Sept 2002, Raustad Mar 2005
	//                      B Griffith Aug 2006, added water consumption and water system interactions
	//                      T Hong, Aug 2008. Added fluid bypass for single speed cooling tower
	//                      Chandan Sharma, FSEC, February 2010, Added basin heater
	//                      A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Model the performance of cooling towers

	// METHODOLOGY EMPLOYED:

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

	using FluidProperties::GetDensityGlycol;
	using FluidProperties::GetSpecificHeatGlycol;
	using DataPlant::PlantLoop;
	// Use statements for access to subroutines in other modules
	using Psychrometrics::PsyWFnTdbTwbPb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyHFnTdbRhPb;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyTsatFnHPb;
	using Psychrometrics::PsyWFnTdbH;
	using General::TrimSigDigits;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Empirical Model Type
	int const CoolToolsXFModel( 1 );
	// CoolTools counterflow model does not work properly. The empirical model seems flawed since the tower
	// operates in the free convection regime on the design day.
	// INTEGER, PARAMETER             :: CoolToolsCFModel     = 2
	int const CoolToolsUserDefined( 3 );
	int const YorkCalcModel( 4 );
	int const YorkCalcUserDefined( 5 );

	int const EvapLossByUserFactor( 80 );
	int const EvapLossByMoistTheory( 81 );

	int const BlowdownByConcentration( 90 );
	int const BlowdownBySchedule( 91 );

	std::string const cCoolingTower_SingleSpeed( "CoolingTower:SingleSpeed" );
	std::string const cCoolingTower_TwoSpeed( "CoolingTower:TwoSpeed" );
	std::string const cCoolingTower_VariableSpeed( "CoolingTower:VariableSpeed" );
	std::string const cCoolingTower_VariableSpeedMerkel( "CoolingTower:VariableSpeed:Merkel" );

	int const PIM_NominalCapacity( 1 );
	int const PIM_UFactor( 2 );

	int const CoolingTower_SingleSpeed( 1 );
	int const CoolingTower_TwoSpeed( 2 );
	int const CoolingTower_VariableSpeed( 3 );
	int const CoolingTower_VariableSpeedMerkel( 4 );

	int const CapacityControl_FanCycling( 1 );
	int const CapacityControl_FluidBypass( 2 );

	int const CellCtrl_MinCell( 1 );
	int const CellCtrl_MaxCell( 2 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumSimpleTowers( 0 ); // Number of similar towers
	bool GetInput( true );
	bool InitTowerOneTimeFlag( true );
	//? The following block of variables are used to carry model results for a tower instance
	//   across sim, update, and report routines.  Simulation manager must be careful
	//   in models with multiple towers.

	Real64 InletWaterTemp( 0.0 ); // CW temperature at tower inlet
	Real64 OutletWaterTemp( 0.0 ); // CW temperature at tower outlet
	int WaterInletNode( 0 ); // Node number at tower inlet
	int WaterOutletNode( 0 ); // Node number at tower outlet
	Real64 WaterMassFlowRate( 0.0 ); // WaterMassFlowRate through tower

	Real64 Qactual( 0.0 ); // Tower heat transfer
	Real64 CTFanPower( 0.0 ); // Tower fan power used
	Real64 AirFlowRateRatio( 0.0 ); // Ratio of air flow rate through VS cooling tower to design air flow rate
	Real64 BasinHeaterPower( 0.0 ); // Basin heater power use (W)
	Real64 WaterUsage( 0.0 ); // Tower water usage (m3/s)
	Real64 FanCyclingRatio( 0.0 ); // cycling ratio of tower fan when min fan speed provide to much capacity

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserLoopTowers

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Object Data
	Array1D< Towerspecs > SimpleTower; // dimension to number of machines
	Array1D< TowerInletConds > SimpleTowerInlet; // inlet conditions
	Array1D< ReportVars > SimpleTowerReport; // report variables
	Array1D< VSTowerData > VSTower; // model coefficients and specific variables for VS tower

	// MODULE SUBROUTINES:

	// Beginning of CondenserLoopTowers Module Driver Subroutines
	//*************************************************************************

	// Functions
	void
	clear_state()
	{
		NumSimpleTowers = 0;
		GetInput = true;
		InitTowerOneTimeFlag = true;
		InletWaterTemp = 0.0;
		OutletWaterTemp = 0.0;
		WaterInletNode = 0;
		WaterOutletNode = 0;
		WaterMassFlowRate = 0.0;
		Qactual = 0.0;
		CTFanPower = 0.0;
		AirFlowRateRatio = 0.0;
		BasinHeaterPower = 0.0;
		WaterUsage = 0.0;
		FanCyclingRatio = 0.0;
		CheckEquipName.deallocate();
		SimpleTower.deallocate();
		SimpleTowerInlet.deallocate();
		SimpleTowerReport.deallocate();
		VSTower.deallocate();
	}

	void
	SimTowers(
		std::string const & TowerType,
		std::string const & TowerName,
		int & CompIndex,
		bool & RunFlag,
		bool const InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor // sizing factor
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey
		//       DATE WRITTEN   Dec. 2000
		//       MODIFIED       Fred Buhl, May 2002; Richard Raustad, FSEC, Feb 2005 (added VS tower)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Main cooling tower driver subroutine.  Gets called from
		// PlantLoopEquipments.

		// METHODOLOGY EMPLOYED:
		// After being called by PlantLoopEquipments, this subroutine
		// calls GetTowerInput to get all cooling tower input info (one time only),
		// then calls the appropriate subroutine to calculate tower performance,
		// update records (node info) and writes output report info.

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

		int TowerNum;

		//GET INPUT
		if ( GetInput ) {
			GetTowerInput();
			GetInput = false;
		}

		// Find the correct CoolingTower
		if ( CompIndex == 0 ) {
			TowerNum = FindItemInList( TowerName, SimpleTower );
			if ( TowerNum == 0 ) {
				ShowFatalError( "SimTowers: Unit not found=" + TowerName );
			}
			CompIndex = TowerNum;
		} else {
			TowerNum = CompIndex;
			if ( TowerNum > NumSimpleTowers || TowerNum < 1 ) {
				ShowFatalError( "SimTowers:  Invalid CompIndex passed=" + TrimSigDigits( TowerNum ) + ", Number of Units=" + TrimSigDigits( NumSimpleTowers ) + ", Entered Unit name=" + TowerName );
			}
			if ( CheckEquipName( TowerNum ) ) {
				if ( TowerName != SimpleTower( TowerNum ).Name ) {
					ShowFatalError( "SimTowers: Invalid CompIndex passed=" + TrimSigDigits( TowerNum ) + ", Unit name=" + TowerName + ", stored Unit Name for that index=" + SimpleTower( TowerNum ).Name );
				}
				CheckEquipName( TowerNum ) = false;
			}
		}

		//INITIALIZE
		InitSimVars();

		//CALCULATE
		{ auto const SELECT_CASE_var( SimpleTower( TowerNum ).TowerType_Num );

		if ( SELECT_CASE_var == CoolingTower_SingleSpeed ) {

			if ( InitLoopEquip ) {
				InitTower( TowerNum, RunFlag );
				SizeTower( TowerNum );
				MinCap = 0.0;
				MaxCap = SimpleTower( TowerNum ).TowerNominalCapacity * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				OptCap = SimpleTower( TowerNum ).TowerNominalCapacity;
				if ( GetSizingFactor ) {
					SizingFactor = SimpleTower( TowerNum ).SizFac;
				}
				return;
			}
			InitTower( TowerNum, RunFlag );
			CalcSingleSpeedTower( TowerNum );
			CalculateWaterUseage( TowerNum );
			UpdateTowers( TowerNum );
			ReportTowers( RunFlag, TowerNum );

		} else if ( SELECT_CASE_var == CoolingTower_TwoSpeed ) {

			if ( InitLoopEquip ) {
				InitTower( TowerNum, RunFlag );
				SizeTower( TowerNum );
				MinCap = 0.0;
				MaxCap = SimpleTower( TowerNum ).TowerNominalCapacity * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				OptCap = SimpleTower( TowerNum ).TowerNominalCapacity;
				if ( GetSizingFactor ) {
					SizingFactor = SimpleTower( TowerNum ).SizFac;
				}
				return;
			}
			InitTower( TowerNum, RunFlag );
			CalcTwoSpeedTower( TowerNum );
			CalculateWaterUseage( TowerNum );
			UpdateTowers( TowerNum );
			ReportTowers( RunFlag, TowerNum );

		} else if ( SELECT_CASE_var == CoolingTower_VariableSpeedMerkel ) {

			if ( InitLoopEquip ) {
				InitTower( TowerNum, RunFlag );
				SizeVSMerkelTower( TowerNum );
				MinCap = 0.0;
				MaxCap = SimpleTower( TowerNum ).TowerNominalCapacity * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				OptCap = SimpleTower( TowerNum ).TowerNominalCapacity;
				if ( GetSizingFactor ) {
					SizingFactor = SimpleTower( TowerNum ).SizFac;
				}
				return;
			}
			InitTower( TowerNum, RunFlag );
			CalcMerkelVariableSpeedTower( TowerNum, MyLoad );
			CalculateWaterUseage( TowerNum );
			UpdateTowers( TowerNum );
			ReportTowers( RunFlag, TowerNum );

		} else if ( SELECT_CASE_var == CoolingTower_VariableSpeed ) {

			if ( InitLoopEquip ) {
				InitTower( TowerNum, RunFlag );
				SizeTower( TowerNum );
				MinCap = 0.0;
				MaxCap = SimpleTower( TowerNum ).TowerNominalCapacity * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				OptCap = SimpleTower( TowerNum ).TowerNominalCapacity;
				if ( GetSizingFactor ) {
					SizingFactor = SimpleTower( TowerNum ).SizFac;
				}
				return;
			}
			InitTower( TowerNum, RunFlag );
			CalcVariableSpeedTower( TowerNum );
			CalculateWaterUseage( TowerNum );
			UpdateTowers( TowerNum );
			ReportTowers( RunFlag, TowerNum );

		} else {
			ShowFatalError( "SimTowers: Invalid Tower Type Requested=" + TowerType );

		}} // TypeOfEquip

	}

	// End CondenserLoopTowers Module Driver Subroutines
	//******************************************************************************

	// Beginning of CondenserLoopTowers Module Get Input subroutines
	//******************************************************************************

	void
	GetTowerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 1998
		//       MODIFIED         Don Shirey, Jan 2001 and Sept/Oct 2002; Richard Raustad, FSEC, Feb 2005 (added VS tower)
		//                        B. Griffith, Aug. 2006 water consumption modeling and water system connections
		//                        T Hong, Aug. 2008: added fluid bypass for single speed tower
		//                        A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for cooling towers and stores it in SimpleTower data structure. Additional structure
		// (VSTower) stores the coefficients for each VS tower.

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in the data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::MakeUPPERCase;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataSizing::AutoSize;
		using CurveManager::GetCurveIndex;
		using ScheduleManager::GetScheduleIndex;
		using WaterManager::SetupTankDemandComponent;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt OutputFormat( "(F5.2)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TowerNum; // Tower number, reference counter for SimpleTower data array
		int NumSingleSpeedTowers; // Total number of single-speed cooling towers
		int SingleSpeedTowerNumber; // Specific single-speed tower of interest
		int NumTwoSpeedTowers; // Number of two-speed cooling towers
		int TwoSpeedTowerNumber; // Specific two-speed tower of interest
		int NumVariableSpeedTowers; // Number of variable-speed cooling towers
		int VariableSpeedTowerNumber; // Specific variable-speed tower of interest
		int NumVSCoolToolsModelCoeffs; // Number of CoolTools VS cooling tower coefficient objects
		int NumVSYorkCalcModelCoeffs; // Number of YorkCalc VS cooling tower coefficient objects
		int NumVSMerkelTowers; // Number of Merkel variable speed cooling towers
		int MerkelVSTowerNum; // specific merkel variable speed tower of interest
		int VSModelCoeffNum; // Specific variable-speed tower coefficient object of interest
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int NumAlphas2; // Number of elements in the alpha2 array
		int NumNums2; // Number of elements in the numeric2 array
		int IOStat; // IO Status when calling get input subroutine
		int CoeffNum; // Index for reading user defined VS tower coefficients
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool ErrorsFound( false ); // Logical flag set .TRUE. if errors found while getting input data
		std::string OutputChar; // report variable for warning messages
		std::string OutputCharLo; // report variable for warning messages
		std::string OutputCharHi; // report variable for warning messages
		Array1D< Real64 > NumArray( 29 ); // Numeric input data array
		Array1D< Real64 > NumArray2( 43 ); // Numeric input data array for VS tower coefficients
		Array1D_string AlphArray( 15 ); // Character string input data array
		Array1D_string AlphArray2( 1 ); // Character string input data array for VS tower coefficients

		// Get number of all cooling towers specified in the input data file (idf)
		NumSingleSpeedTowers = GetNumObjectsFound( cCoolingTower_SingleSpeed );
		NumTwoSpeedTowers = GetNumObjectsFound( cCoolingTower_TwoSpeed );
		NumVariableSpeedTowers = GetNumObjectsFound( cCoolingTower_VariableSpeed );
		NumVSMerkelTowers = GetNumObjectsFound( cCoolingTower_VariableSpeedMerkel );
		NumSimpleTowers = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers;

		if ( NumSimpleTowers <= 0 ) ShowFatalError( "No Cooling Tower objects found in input, however, a branch object has specified a cooling tower. Search the input for CoolingTower to determine the cause for this error." );

		// See if load distribution manager has already gotten the input
		if ( allocated( SimpleTower ) ) return;

		// Allocate data structures to hold tower input data, report data and tower inlet conditions
		SimpleTower.allocate( NumSimpleTowers );
		SimpleTowerReport.allocate( NumSimpleTowers );
		SimpleTowerInlet.allocate( NumSimpleTowers );
		CheckEquipName.dimension( NumSimpleTowers, true );
		// Allocate variable-speed tower structure with data specific to this type
		if ( NumVariableSpeedTowers > 0 ) {
			VSTower.allocate( NumVariableSpeedTowers );
			// Allow users to input model coefficients other than default
			NumVSCoolToolsModelCoeffs = GetNumObjectsFound( "CoolingTowerPerformance:CoolTools" );
			NumVSYorkCalcModelCoeffs = GetNumObjectsFound( "CoolingTowerPerformance:YorkCalc" );
		}

		// Load data structures with cooling tower input data
		cCurrentModuleObject = cCoolingTower_SingleSpeed;
		for ( SingleSpeedTowerNumber = 1; SingleSpeedTowerNumber <= NumSingleSpeedTowers; ++SingleSpeedTowerNumber ) {
			TowerNum = SingleSpeedTowerNumber;
			GetObjectItem( cCurrentModuleObject, SingleSpeedTowerNumber, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SimpleTower, TowerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SimpleTower( TowerNum ).Name = AlphArray( 1 );
			SimpleTower( TowerNum ).TowerType = cCurrentModuleObject;
			SimpleTower( TowerNum ).TowerType_Num = CoolingTower_SingleSpeed;
			SimpleTower( TowerNum ).TowerMassFlowRateMultiplier = 2.5;
			SimpleTower( TowerNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SimpleTower( TowerNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Chilled Water Nodes" );
			SimpleTower( TowerNum ).DesignWaterFlowRate = NumArray( 1 );
			if (SimpleTower( TowerNum ).DesignWaterFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).DesignWaterFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).HighSpeedAirFlowRate = NumArray( 2 );
			if ( SimpleTower( TowerNum ).HighSpeedAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).HighSpeedFanPower = NumArray( 3 );
			if ( SimpleTower( TowerNum ).HighSpeedFanPower == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedFanPowerWasAutoSized = true;
			}
			SimpleTower( TowerNum ).HighSpeedTowerUA = NumArray( 4 );
			if (SimpleTower( TowerNum ).HighSpeedTowerUA == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedTowerUAWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvAirFlowRate = NumArray( 5 );
			if ( SimpleTower( TowerNum ).FreeConvAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).FreeConvAirFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvAirFlowRateSizingFactor = NumArray( 6 );
			SimpleTower( TowerNum ).FreeConvTowerUA = NumArray( 7 );
			if ( SimpleTower( TowerNum ).FreeConvTowerUA == AutoSize ) {
				SimpleTower( TowerNum ).FreeConvTowerUAWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvTowerUASizingFactor = NumArray( 8 );
			SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio = NumArray( 9 );
			SimpleTower( TowerNum ).TowerNominalCapacity = NumArray( 10 );
			if ( SimpleTower( TowerNum ).TowerNominalCapacity == AutoSize ) {
				SimpleTower( TowerNum ).TowerNominalCapacityWasAutoSized = true;
			}
			SimpleTower( TowerNum ).TowerFreeConvNomCap = NumArray( 11 );
			if ( SimpleTower( TowerNum ).TowerFreeConvNomCap == AutoSize ) {
				SimpleTower( TowerNum ).TowerFreeConvNomCapWasAutoSized = true;
			}
			SimpleTower( TowerNum ).TowerFreeConvNomCapSizingFactor = NumArray( 12 );
			if ( NumAlphas >= 4 ) {
				if ( SameString( AlphArray( 4 ), "UFactorTimesAreaAndDesignWaterFlowRate" ) ) {
					SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_UFactor;
				} else if ( SameString( AlphArray( 4 ), "NominalCapacity" ) ) {
					SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_NominalCapacity;
				} else {
					ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
					ErrorsFound = true;
				}
			} else {
				// Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
				SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_UFactor;
			}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff = NumArray( 13 );
			if ( NumArray( 13 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater power as a function of temperature difference must be >= 0" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).BasinHeaterSetPointTemp = NumArray( 14 );

			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 14 ) {
					SimpleTower( TowerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( SimpleTower( TowerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + SimpleTower( TowerNum ).Name + "\", " + cNumericFieldNames( 14 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! AlphArray( 5 ).empty() ) {
				SimpleTower( TowerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( AlphArray( 5 ) );
				if ( SimpleTower( TowerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater schedule name \"" + AlphArray( 5 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

			// begin water use and systems get input
			if ( SameString( AlphArray( 6 ), "LossFactor" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByUserFactor;
			} else if ( SameString( AlphArray( 6 ), "SaturatedExit" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else if ( AlphArray( 6 ).empty() ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid, " + cAlphaFieldNames( 6 ) + " = " + AlphArray( 6 ) );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).UserEvapLossFactor = NumArray( 15 ); //  N11 , \field Evaporation Loss Factor
			SimpleTower( TowerNum ).DriftLossFraction = NumArray( 16 ) / 100.0; //  N12, \field Drift Loss Percent

			if ( ( NumNums < 16 ) && ( SimpleTower( TowerNum ).DriftLossFraction == 0.0 ) ) {
				// assume Drift loss not entered and should be defaulted
				SimpleTower( TowerNum ).DriftLossFraction = 0.008 / 100.0;
			}

			SimpleTower( TowerNum ).ConcentrationRatio = NumArray( 17 ); //  N13, \field Blowdown Concentration Ratio
			SimpleTower( TowerNum ).SizFac = NumArray( 21 ); //  N17  \field Sizing Factor
			if ( SimpleTower( TowerNum ).SizFac <= 0.0 ) SimpleTower( TowerNum ).SizFac = 1.0;

			if ( SameString( AlphArray( 7 ), "ScheduledRate" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownBySchedule;
			} else if ( SameString( AlphArray( 7 ), "ConcentrationRatio" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
			} else if ( AlphArray( 7 ).empty() ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
				if ( ( NumNums < 17 ) && ( SimpleTower( TowerNum ).ConcentrationRatio == 0.0 ) ) {
					// assume Concetratino ratio was omitted and should be defaulted
					SimpleTower( TowerNum ).ConcentrationRatio = 3.0;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid, " + cAlphaFieldNames( 7 ) + " = " + AlphArray( 7 ) );
				ErrorsFound = true;
			}
			SimpleTower( TowerNum ).SchedIDBlowdown = GetScheduleIndex( AlphArray( 8 ) );
			if ( ( SimpleTower( TowerNum ).SchedIDBlowdown == 0 ) && ( SimpleTower( TowerNum ).BlowdownMode == BlowdownBySchedule ) ) {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid, " + cAlphaFieldNames( 8 ) + " = " + AlphArray( 8 ) );
				ErrorsFound = true;
			}

			if ( AlphArray( 9 ).empty() ) {
				SimpleTower( TowerNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( AlphArray( 1 ), cCurrentModuleObject, AlphArray( 9 ), ErrorsFound, SimpleTower( TowerNum ).WaterTankID, SimpleTower( TowerNum ).WaterTankDemandARRID );
				SimpleTower( TowerNum ).SuppliedByWaterSystem = true;
			}

			//   outdoor air inlet node

			if ( lAlphaFieldBlanks( 10 ) ) {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = 0;
			} else {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = GetOnlySingleNode( AlphArray( 10 ), ErrorsFound, cCurrentModuleObject, SimpleTower( TowerNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ) ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray( 10 ) );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}
			}

			//   fluid bypass for single speed tower
			if ( lAlphaFieldBlanks( 11 ) || AlphArray( 11 ).empty() ) {
				SimpleTower( TowerNum ).CapacityControl = CapacityControl_FanCycling; // FanCycling
			} else {
				{ auto const SELECT_CASE_var( MakeUPPERCase( AlphArray( 11 ) ) );
				if ( SELECT_CASE_var == "FANCYCLING" ) {
					SimpleTower( TowerNum ).CapacityControl = CapacityControl_FanCycling;
				} else if ( SELECT_CASE_var == "FLUIDBYPASS" ) {
					SimpleTower( TowerNum ).CapacityControl = CapacityControl_FluidBypass;
				} else {
					SimpleTower( TowerNum ).CapacityControl = CapacityControl_FanCycling;
					ShowWarningError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" The Capacity Control is not specified correctly. The default Fan Cycling is used." );
				}}
			}

			//added for multi-cell
			SimpleTower( TowerNum ).NumCell = NumArray( 18 );
			if ( ( NumNums < 18 ) && ( SimpleTower( TowerNum ).NumCell == 0 ) ) {
				// assume Number of Cells not entered and should be defaulted
				SimpleTower( TowerNum ).NumCell = 1;
			}
			SimpleTower( TowerNum ).MinFracFlowRate = NumArray( 19 );
			if ( ( NumNums < 19 ) && ( SimpleTower( TowerNum ).MinFracFlowRate == 0.0 ) ) {
				// assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MinFracFlowRate = 0.33;
			}
			SimpleTower( TowerNum ).MaxFracFlowRate = NumArray( 20 );
			if ( ( NumNums < 20 ) && ( SimpleTower( TowerNum ).MaxFracFlowRate == 0.0 ) ) {
				// assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MaxFracFlowRate = 2.5;
			}

			if ( NumAlphas >= 12 ) {
				if ( lAlphaFieldBlanks( 12 ) || AlphArray( 12 ).empty() ) {
					SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
					SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
				} else {
					if ( SameString( AlphArray( 12 ), "MinimalCell" ) || SameString( AlphArray( 12 ), "MaximalCell" ) ) {
						if ( SameString( AlphArray( 12 ), "MinimalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MinCell;
							SimpleTower( TowerNum ).CellCtrl = "MinimalCell";
						}
						if ( SameString( AlphArray( 12 ), "MaximalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
							SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
						}
					} else {
						ShowSevereError( "Illegal " + cAlphaFieldNames( 12 ) + " = " + AlphArray( 12 ) );
						ShowContinueError( "Occurs in " + SimpleTower( TowerNum ).TowerType + '=' + SimpleTower( TowerNum ).Name );
						ErrorsFound = true;
					}
				}
			} else {
				//assume Cell Control not entered and should be defaulted
				SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
				SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
			}

			//   High speed air flow rate must be greater than free convection air flow rate.
			//   Can't tell yet if autosized, check later in InitTower.
			if ( SimpleTower( TowerNum ).HighSpeedAirFlowRate <= SimpleTower( TowerNum ).FreeConvAirFlowRate && SimpleTower( TowerNum ).HighSpeedAirFlowRate != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow rate must be less than the design air flow rate." );
				ErrorsFound = true;
			}

			//   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
			if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_UFactor ) {
				if ( SimpleTower( TowerNum ).DesignWaterFlowRate == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower performance input method requires a design water flow rate greater than zero." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).HighSpeedTowerUA <= SimpleTower( TowerNum ).FreeConvTowerUA && SimpleTower( TowerNum ).HighSpeedTowerUA != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection UA must be less than the design tower UA." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).FreeConvTowerUA > 0.0 && SimpleTower( TowerNum ).FreeConvAirFlowRate == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow rate must be greater than zero when free convection UA is greater than zero." );
					ErrorsFound = true;
				}
			} else if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {
				if ( SimpleTower( TowerNum ).TowerNominalCapacity == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower performance input method requires valid nominal capacity." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).DesignWaterFlowRate != 0.0 ) {
					if ( SimpleTower( TowerNum ).DesignWaterFlowRate > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method and design water flow rate have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method has been specified and design water flow rate is being autosized." );
					}
					ShowContinueError( "Design water flow rate must be left blank when nominal tower capacity input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).HighSpeedTowerUA != 0.0 ) {
					if ( SimpleTower( TowerNum ).HighSpeedTowerUA > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal tower capacity and design tower UA have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal tower capacity has been specified and design tower UA is being autosized." );
					}
					ShowContinueError( "Design tower UA field must be left blank when nominal tower capacity input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).FreeConvTowerUA != 0.0 ) {
					if ( SimpleTower( TowerNum ).FreeConvTowerUA > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method and free convection UA have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method has been specified and free convection UA is being autosized." );
					}
					ShowContinueError( "Free convection UA should be left blank when nominal tower capacity input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).TowerFreeConvNomCap >= SimpleTower( TowerNum ).TowerNominalCapacity ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection nominal capacity must be less than the nominal (design) tower capacity." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).TowerFreeConvNomCap > 0.0 && SimpleTower( TowerNum ).FreeConvAirFlowRate == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow must be greater than zero when tower free convection capacity is specified." );
					ErrorsFound = true;
				}
			} else { // Tower performance input method is not specified as a valid "choice"
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower Performance Input Method must be \"UFactorTimesAreaAndDesignWaterFlowRate\" or \"NominalCapacity\"." );
				ShowContinueError( "Tower Performanace Input Method currently specified as: " + AlphArray( 4 ) );
				ErrorsFound = true;
			}
		} // End Single-Speed Tower Loop

		cCurrentModuleObject = cCoolingTower_TwoSpeed;
		for ( TwoSpeedTowerNumber = 1; TwoSpeedTowerNumber <= NumTwoSpeedTowers; ++TwoSpeedTowerNumber ) {
			TowerNum = NumSingleSpeedTowers + TwoSpeedTowerNumber;
			GetObjectItem( cCurrentModuleObject, TwoSpeedTowerNumber, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SimpleTower, TowerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SimpleTower( TowerNum ).Name = AlphArray( 1 );
			SimpleTower( TowerNum ).TowerType = cCurrentModuleObject;
			SimpleTower( TowerNum ).TowerType_Num = CoolingTower_TwoSpeed;
			SimpleTower( TowerNum ).TowerMassFlowRateMultiplier = 2.5;
			SimpleTower( TowerNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SimpleTower( TowerNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Chilled Water Nodes" );

			if ( NumAlphas >= 4 ) {
				if ( SameString( AlphArray( 4 ), "UFactorTimesAreaAndDesignWaterFlowRate" ) ) {
					SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_UFactor;
				} else if ( SameString( AlphArray( 4 ), "NominalCapacity" ) ) {
					SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_NominalCapacity;
				} else {
					ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
					ErrorsFound = true;
				}
			} else {
				// Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
				SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_UFactor;
			}
			SimpleTower( TowerNum ).DesignWaterFlowRate = NumArray( 1 );
			if (SimpleTower( TowerNum ).DesignWaterFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).DesignWaterFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).HighSpeedAirFlowRate = NumArray( 2 );
			if ( SimpleTower( TowerNum ).HighSpeedAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).HighSpeedFanPower = NumArray( 3 );
			if ( SimpleTower( TowerNum ).HighSpeedFanPower == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedFanPowerWasAutoSized = true;
			}
			SimpleTower( TowerNum ).HighSpeedTowerUA = NumArray( 4 );
			if ( SimpleTower( TowerNum ).HighSpeedTowerUA == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedTowerUAWasAutoSized = true;
			}
			SimpleTower( TowerNum ).LowSpeedAirFlowRate = NumArray( 5 );
			if ( SimpleTower( TowerNum ).LowSpeedAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).LowSpeedAirFlowRateWasAutoSized = true;
			}

			SimpleTower( TowerNum ).LowSpeedAirFlowRateSizingFactor = NumArray( 6 );
			SimpleTower( TowerNum ).LowSpeedFanPower = NumArray( 7 );
			if ( SimpleTower( TowerNum ).LowSpeedFanPower == AutoSize ) {
				SimpleTower( TowerNum ).LowSpeedFanPowerWasAutoSized = true;
			}
			SimpleTower( TowerNum ).LowSpeedFanPowerSizingFactor = NumArray( 8 );
			SimpleTower( TowerNum ).LowSpeedTowerUA = NumArray( 9 );
			if ( SimpleTower( TowerNum ).LowSpeedTowerUA == AutoSize ) {
				SimpleTower( TowerNum ).LowSpeedTowerUAWasAutoSized = true;
			}
			SimpleTower( TowerNum ).LowSpeedTowerUASizingFactor = NumArray( 10 );
			SimpleTower( TowerNum ).FreeConvAirFlowRate = NumArray( 11 );
			if ( SimpleTower( TowerNum ).FreeConvAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).FreeConvAirFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvAirFlowRateSizingFactor = NumArray( 12 );
			SimpleTower( TowerNum ).FreeConvTowerUA = NumArray( 13 );
			if ( SimpleTower( TowerNum ).FreeConvTowerUA == AutoSize ) {
				SimpleTower( TowerNum ).FreeConvTowerUAWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvTowerUASizingFactor = NumArray( 14 );
			SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio = NumArray( 15 );
			SimpleTower( TowerNum ).TowerNominalCapacity = NumArray( 16 );

			SimpleTower( TowerNum ).TowerLowSpeedNomCap = NumArray( 17 );
			if ( SimpleTower( TowerNum ).TowerLowSpeedNomCap == AutoSize ) {
				SimpleTower( TowerNum ).TowerLowSpeedNomCapWasAutoSized = true;
			}
			SimpleTower( TowerNum ).TowerLowSpeedNomCapSizingFactor = NumArray( 18 );
			SimpleTower( TowerNum ).TowerFreeConvNomCap = NumArray( 19 );
			if ( SimpleTower( TowerNum ).TowerFreeConvNomCap == AutoSize ) {
				SimpleTower( TowerNum ).TowerFreeConvNomCapWasAutoSized = true;
			}
			SimpleTower( TowerNum ).TowerFreeConvNomCapSizingFactor = NumArray( 20 );

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff = NumArray( 21 );
			if ( NumArray( 21 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater power as a function of temperature difference must be >= 0" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).BasinHeaterSetPointTemp = NumArray( 22 );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 22 ) {
					SimpleTower( TowerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( SimpleTower( TowerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + SimpleTower( TowerNum ).Name + "\", " + cNumericFieldNames( 22 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! AlphArray( 5 ).empty() ) {
				SimpleTower( TowerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( AlphArray( 5 ) );
				if ( SimpleTower( TowerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater schedule name \"" + AlphArray( 5 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

			// begin water use and systems get input
			if ( SameString( AlphArray( 6 ), "LossFactor" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByUserFactor;
			} else if ( SameString( AlphArray( 6 ), "SaturatedExit" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else if ( lAlphaFieldBlanks( 6 ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + AlphArray( 6 ) );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).UserEvapLossFactor = NumArray( 23 ); //  N23 , \field Evaporation Loss Factor
			SimpleTower( TowerNum ).DriftLossFraction = NumArray( 24 ) / 100.0; //  N24, \field Drift Loss Percent
			if ( ( NumNums < 24 ) && ( SimpleTower( TowerNum ).DriftLossFraction == 0.0 ) ) {
				// assume Drift loss not entered and should be defaulted
				SimpleTower( TowerNum ).DriftLossFraction = 0.008 / 100.0;
			}

			SimpleTower( TowerNum ).ConcentrationRatio = NumArray( 25 ); //  N17, \field Blowdown Concentration Ratio
			SimpleTower( TowerNum ).SizFac = NumArray( 29 ); //  N21  \field Sizing Factor
			if ( SimpleTower( TowerNum ).SizFac <= 0.0 ) SimpleTower( TowerNum ).SizFac = 1.0;

			if ( SameString( AlphArray( 7 ), "ScheduledRate" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownBySchedule;
			} else if ( SameString( AlphArray( 7 ), "ConcentrationRatio" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
			} else if ( lAlphaFieldBlanks( 7 ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
				if ( ( NumNums < 25 ) && ( SimpleTower( TowerNum ).ConcentrationRatio == 0.0 ) ) {
					// assume Concetration ratio was omitted and should be defaulted
					SimpleTower( TowerNum ).ConcentrationRatio = 3.0;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + AlphArray( 7 ) );
				ErrorsFound = true;
			}
			SimpleTower( TowerNum ).SchedIDBlowdown = GetScheduleIndex( AlphArray( 8 ) );
			if ( ( SimpleTower( TowerNum ).SchedIDBlowdown == 0 ) && ( SimpleTower( TowerNum ).BlowdownMode == BlowdownBySchedule ) ) {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + AlphArray( 8 ) );
				ErrorsFound = true;
			}

			//added for multi-cell
			SimpleTower( TowerNum ).NumCell = NumArray( 26 );
			if ( ( NumNums < 26 ) && ( SimpleTower( TowerNum ).NumCell == 0 ) ) {
				// assume Number of Cells not entered and should be defaulted
				SimpleTower( TowerNum ).NumCell = 1;
			}
			SimpleTower( TowerNum ).MinFracFlowRate = NumArray( 27 );
			if ( ( NumNums < 27 ) && ( SimpleTower( TowerNum ).MinFracFlowRate == 0.0 ) ) {
				// assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MinFracFlowRate = 0.33;
			}
			SimpleTower( TowerNum ).MaxFracFlowRate = NumArray( 28 );
			if ( ( NumNums < 28 ) && ( SimpleTower( TowerNum ).MaxFracFlowRate == 0.0 ) ) {
				// assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MaxFracFlowRate = 2.5;
			}

			if ( NumAlphas >= 11 ) {
				if ( lAlphaFieldBlanks( 11 ) || AlphArray( 11 ).empty() ) {
					SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
					SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
				} else {
					if ( SameString( AlphArray( 11 ), "MinimalCell" ) || SameString( AlphArray( 11 ), "MaximalCell" ) ) {
						if ( SameString( AlphArray( 11 ), "MinimalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MinCell;
							SimpleTower( TowerNum ).CellCtrl = "MinimalCell";
						}
						if ( SameString( AlphArray( 11 ), "MaximalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
							SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
						}
					} else {
						ShowSevereError( "Illegal " + cAlphaFieldNames( 12 ) + " = " + AlphArray( 12 ) );
						ShowContinueError( "Occurs in " + SimpleTower( TowerNum ).TowerType + '=' + SimpleTower( TowerNum ).Name );
						ErrorsFound = true;
					}
				}
			} else {
				//assume Cell Control not entered and should be defaulted
				SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
				SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
			}

			if ( lAlphaFieldBlanks( 9 ) ) {
				SimpleTower( TowerNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( AlphArray( 1 ), cCurrentModuleObject, AlphArray( 9 ), ErrorsFound, SimpleTower( TowerNum ).WaterTankID, SimpleTower( TowerNum ).WaterTankDemandARRID );
				SimpleTower( TowerNum ).SuppliedByWaterSystem = true;
			}

			//   outdoor air inlet node
			if ( lAlphaFieldBlanks( 10 ) ) {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = 0;
			} else {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = GetOnlySingleNode( AlphArray( 10 ), ErrorsFound, cCurrentModuleObject, SimpleTower( TowerNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ) ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray( 10 ) );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}
			}

			//   High speed air flow rate must be greater than low speed air flow rate.
			//   Can't tell yet if autosized, check later in InitTower.
			if ( SimpleTower( TowerNum ).HighSpeedAirFlowRate <= SimpleTower( TowerNum ).LowSpeedAirFlowRate && SimpleTower( TowerNum ).HighSpeedAirFlowRate != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Low speed air flow rate must be less than the high speed air flow rate." );
				ErrorsFound = true;
			}
			//   Low speed air flow rate must be greater than free convection air flow rate.
			//   Can't tell yet if autosized, check later in InitTower.
			if ( SimpleTower( TowerNum ).LowSpeedAirFlowRate <= SimpleTower( TowerNum ).FreeConvAirFlowRate && SimpleTower( TowerNum ).LowSpeedAirFlowRate != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow rate must be less than the low speed air flow rate." );
				ErrorsFound = true;
			}

			//   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
			if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_UFactor ) {
				if ( SimpleTower( TowerNum ).DesignWaterFlowRate == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower performance input method requires a design water flow rate greater than zero." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).HighSpeedTowerUA <= SimpleTower( TowerNum ).LowSpeedTowerUA && SimpleTower( TowerNum ).HighSpeedTowerUA != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower UA at low fan speed must be less than the tower UA at high fan speed." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).LowSpeedTowerUA <= SimpleTower( TowerNum ).FreeConvTowerUA && SimpleTower( TowerNum ).LowSpeedTowerUA != AutoSize ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).FreeConvTowerUA > 0.0 && SimpleTower( TowerNum ).FreeConvAirFlowRate == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow rate must be greater than zero when free convection UA is greater than zero." );
					ErrorsFound = true;
				}
			} else if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {
				if ( SimpleTower( TowerNum ).TowerNominalCapacity == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower performance input method requires valid high-speed nominal capacity." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).TowerLowSpeedNomCap == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower performance input method requires valid low-speed nominal capacity." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).DesignWaterFlowRate != 0.0 ) {
					if ( SimpleTower( TowerNum ).DesignWaterFlowRate > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method and design water flow rate have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method has been specified and design water flow rate is being autosized." );
					}
					ShowContinueError( "Design water flow rate must be left blank when nominal tower capacity input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).HighSpeedTowerUA != 0.0 ) {
					if ( SimpleTower( TowerNum ).HighSpeedTowerUA > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method and tower UA at high fan speed have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method has been specified and tower UA at high fan speed is being autosized." );
					}
					ShowContinueError( "Tower UA at high fan speed must be left blank when nominal tower capacity input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).LowSpeedTowerUA != 0.0 ) {
					if ( SimpleTower( TowerNum ).LowSpeedTowerUA > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method and tower UA at low fan speed have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method has been specified and tower UA at low fan speed is being autosized." );
					}
					ShowContinueError( "Tower UA at low fan speed must be left blank when nominal tower capacity input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).FreeConvTowerUA != 0.0 ) {
					if ( SimpleTower( TowerNum ).FreeConvTowerUA > 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method and free convection UA have been specified." );
					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Nominal capacity input method has been specified and free convection UA is being autosized." );
					}
					ShowContinueError( "Free convection UA should be left blank when nominal tower capacity input method is used." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).TowerLowSpeedNomCap >= SimpleTower( TowerNum ).TowerNominalCapacity ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Low-speed nominal capacity must be less than the high-speed nominal capacity." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).TowerFreeConvNomCap >= SimpleTower( TowerNum ).TowerLowSpeedNomCap ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection nominal capacity must be less than the low-speed nominal capacity." );
					ErrorsFound = true;
				}
				if ( SimpleTower( TowerNum ).TowerFreeConvNomCap > 0.0 && SimpleTower( TowerNum ).FreeConvAirFlowRate == 0.0 ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow must be greater than zero when tower free convection capacity is specified." );
					ErrorsFound = true;
				}
			} else { // Tower performance input method is not specified as a valid "choice"
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Tower Performance Input Method must be \"UFactorTimesAreaAndDesignWaterFlowRate\" or \"NominalCapacity\"." );
				ShowContinueError( "Tower Performanace Input Method currently specified as: " + AlphArray( 4 ) );
				ErrorsFound = true;
			}
		} // End Two-Speed Tower Loop

		cCurrentModuleObject = cCoolingTower_VariableSpeed;
		for ( VariableSpeedTowerNumber = 1; VariableSpeedTowerNumber <= NumVariableSpeedTowers; ++VariableSpeedTowerNumber ) {
			TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + VariableSpeedTowerNumber;
			GetObjectItem( cCurrentModuleObject, VariableSpeedTowerNumber, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SimpleTower, TowerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SimpleTower( TowerNum ).VSTower = VariableSpeedTowerNumber;
			SimpleTower( TowerNum ).Name = AlphArray( 1 );
			SimpleTower( TowerNum ).TowerType = cCurrentModuleObject;
			SimpleTower( TowerNum ).TowerType_Num = CoolingTower_VariableSpeed;
			SimpleTower( TowerNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SimpleTower( TowerNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Chilled Water Nodes" );

			if ( ( SameString( AlphArray( 4 ), "CoolToolsUserDefined" ) || SameString( AlphArray( 4 ), "YorkCalcUserDefined" ) ) && lAlphaFieldBlanks( 5 ) ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" a " + cAlphaFieldNames( 5 ) + " must be specified when " + cAlphaFieldNames( 4 ) + " is specified as CoolToolsUserDefined or YorkCalcUserDefined" );
				ErrorsFound = true;
			} else if ( ( SameString( AlphArray( 4 ), "CoolToolsCrossFlow" ) || SameString( AlphArray( 4 ), "YorkCalc" ) ) && ! lAlphaFieldBlanks( 5 ) ) {
				ShowWarningError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" a Tower Model Coefficient Name is specified and the Tower Model Type is not specified as CoolToolsUserDefined or YorkCalcUserDefined. The CoolingTowerPerformance:CoolTools (orCoolingTowerPerformance:YorkCalc) data object will not be used." );
			} else {
				SimpleTower( TowerNum ).ModelCoeffObjectName = AlphArray( 5 );
			}

			if ( ! lAlphaFieldBlanks( 6 ) ) {
				SimpleTower( TowerNum ).FanPowerfAirFlowCurve = GetCurveIndex( AlphArray( 6 ) );
				if ( SimpleTower( TowerNum ).FanPowerfAirFlowCurve == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" the Fan Power Ratio as a function of Air Flow Rate Ratio Curve Name specified as " + AlphArray( 6 ) + " was not found. Fan Power as a function of Air Flow Rate Ratio will default to Fan Power = (Air Flow Rate Ratio)^3 and the simulation continues." );
				}
			}

			VSTower( VariableSpeedTowerNumber ).Coeff.allocate( 35 );
			VSTower( VariableSpeedTowerNumber ).Coeff = 0.0;

			if ( SameString( AlphArray( 4 ), "CoolToolsCrossFlow" ) ) {
				SimpleTower( TowerNum ).TowerModelType = CoolToolsXFModel;
				//     set cross-flow model coefficients
				//       Outputs approach in F
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  = -2.1985908408527
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = -24.3108065555106
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  = 21.9333667825398
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  = -4.94979078884808
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  = 14.6788552214526
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  = -15.4612468065777
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  = 2.83753688605444
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  = 10.0023162199558
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  = 2.70780345372045
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) = -5.91993527180418
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) = 0.194222288920726
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) = 0.142543400927955
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) = -0.0818947291400898
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) = -0.169584760441541
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) = 0.0186741309635284
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) = 0.0536824177590012
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) = -0.00375848174056975
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) = 0.000623763881051551
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) = -0.000709769430542879
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) = 0.0000234697776728891
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) = 2.45541543720225
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) = -0.607566456611435
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) = 0.117339576910507
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) = 1.64648551160799
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) = -0.135898905926974
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) = -0.152577581866506
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) = -0.034055419164321
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) = 0.00274052705314173
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) = -0.00442366885652332
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) = 0.0000687098236486247
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) = -0.0416435261408276
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) = 0.00263481599534274
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) = -0.010325259545311
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) = 0.000356999078067433
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) = 0.000249188476685273

				//       Outputs approach in C
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 1 ) = 0.52049709836241;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 2 ) = -10.617046395344;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 3 ) = 10.7292974722538;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 4 ) = -2.74988377158227;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 5 ) = 4.73629943913743;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 6 ) = -8.25759700874711;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 7 ) = 1.57640938114136;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 8 ) = 6.51119643791324;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 9 ) = 1.50433525206692;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 10 ) = -3.2888529287801;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 11 ) = 0.0257786145353773;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 12 ) = 0.182464289315254;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 13 ) = -0.0818947291400898;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 14 ) = -0.215010003996285;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 15 ) = 0.0186741309635284;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 16 ) = 0.0536824177590012;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 17 ) = -0.00270968955115031;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 18 ) = 0.00112277498589279;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 19 ) = -0.00127758497497718;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 20 ) = 0.0000760420796601607;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 21 ) = 1.43600088336017;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 22 ) = -0.5198695909109;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 23 ) = 0.117339576910507;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 24 ) = 1.50492810819924;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 25 ) = -0.135898905926974;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 26 ) = -0.152577581866506;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 27 ) = -0.0533843828114562;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 28 ) = 0.00493294869565511;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 29 ) = -0.00796260394174197;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 30 ) = 0.000222619828621544;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 31 ) = -0.0543952001568055;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 32 ) = 0.00474266879161693;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 33 ) = -0.0185854671815598;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 34 ) = 0.00115667701293848;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 35 ) = 0.000807370664460284;

				//       set minimum and maximum boundaries for CoolTools crossflow model input variables
				VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp = -1.0;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp = 26.6667;
				VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp = 1.1111;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp = 11.1111;
				VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp = 1.1111;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp = 11.1111;
				VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio = 0.75;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio = 1.25;

				//    CoolTools counterflow model does not work properly. The empirical model seems flawed since the tower
				//    operates in the free convection regime on the design day.
				//    ELSEIF(SameString(AlphArray(5),'COOLTOOLS COUNTERFLOW'))THEN
				//      SimpleTower(TowerNum)%TowerModelType               = CoolToolsCFModel
				//!     set counter-flow model coefficients
				//!       Outputs approach in F
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  = -4.48760943345722
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = 0.741749875850003
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  = 1.74679844252553
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  = -0.397320959632943
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  = 19.5106208955792
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  = -9.79489761472574
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  = 1.96690857354709
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  = -1.40803729637148
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  = 0.633867141219563
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) = -0.517255742412696
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) = 0.0546335532842876
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) = 0.0468060318806566
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) = -0.0244033403339062
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) = -0.267365212754448
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) = 0.0385664546399435
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) = 0.037765628073743
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) = -0.000928698541521428
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) = -0.000122211107650076
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) = 0.000682937021895334
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) = 0.00000679217734960548
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) = 1.47274732178792
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) = -0.869303590626237
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) = 0.149995781695274
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) = 2.4548219494635
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) = -0.161092120908292
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) = -0.0830303891087807
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) = -0.0251101427687245
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) = 0.00430042875730149
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) = -0.013969370453107
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) = 0.000096171182587938
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) = -0.0251558254472348
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) = 0.0077094706621763
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) = -0.0173842428341529
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) = 0.000244578460749651
				//!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) = 0.000123026859143619
				//!       Outputs approach in C
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  =  -1.92653164860338
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  =   1.17466595655408
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  =   0.536606417689184
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  =  -0.220733866462746
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  =   6.4745897765876
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  =  -4.75598392569308
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  =   1.09272698530394
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  =  -0.110853998895391
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  =   0.352148411788646
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) =  -0.287364301340387
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) =   0.0160624154449042
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) =   0.0389845209910517
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) =  -0.0244033403339062
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) =  -0.223657243353147
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) =   0.0385664546399435
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) =   0.037765628073743
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) =  -0.000497969128726743
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) =  -0.000219979993770137
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) =   0.0012292866394116
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) =   0.0000220066546127218
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) =   0.767702044158785
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) =  -0.731689870392589
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) =   0.149995781695274
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) =   2.00780209496408
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) =  -0.161092120908292
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) =  -0.0830303891087807
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) =  -0.0341193367495736
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) =   0.00774077176314268
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) =  -0.0251448668155926
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) =   0.000311594631584919
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) =  -0.0311927664658427
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) =   0.0138770471919173
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) =  -0.0312916371014752
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) =   0.000792434212828869
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) =   0.000398607023625325

				//!       set minimum and maximum boundaries for CoolTools counterflow model input variables
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp = -1.0
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp = 26.6667
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp      = 1.1111
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp      = 11.1111
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp   = 1.1111
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp   = 11.1111
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio = 0.75
				//        VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio = 1.25

			} else if ( SameString( AlphArray( 4 ), "YorkCalc" ) ) {
				SimpleTower( TowerNum ).TowerModelType = YorkCalcModel;
				//     set counter-flow model coefficients
				//       Outputs approach in F
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  = 2.471005863
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = -0.139855144
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  = 0.001325024
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  = 0.768721437
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  = -0.023370562
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  = 0.000149476
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  = -0.01116139
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  = 0.000325406
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  = -0.00000230183
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) = 9.852803844
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) = -0.173673565
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) = 0.000811069
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) = 1.749920395
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) = 0.004930143
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) = -0.00022193
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) = -0.009865402
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) = -0.000283361
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) = 0.00000466261
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) = 0.09746009
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) = -0.011167959
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) = 0.000138903
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) = -0.135414837
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) = 0.001004747
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) = 0.0000119203
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) = -0.002255673
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) = 0.0000192893
				//        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) = -0.000000260086

				//       Outputs approach in C
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 1 ) = -0.359741205;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 2 ) = -0.055053608;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 3 ) = 0.0023850432;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 4 ) = 0.173926877;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 5 ) = -0.0248473764;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 6 ) = 0.00048430224;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 7 ) = -0.005589849456;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 8 ) = 0.0005770079712;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 9 ) = -0.00001342427256;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 10 ) = 2.84765801111111;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 11 ) = -0.121765149;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 12 ) = 0.0014599242;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 13 ) = 1.680428651;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 14 ) = -0.0166920786;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 15 ) = -0.0007190532;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 16 ) = -0.025485194448;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 17 ) = 0.0000487491696;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 18 ) = 0.00002719234152;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 19 ) = -0.0653766255555556;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 20 ) = -0.002278167;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 21 ) = 0.0002500254;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 22 ) = -0.0910565458;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 23 ) = 0.00318176316;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 24 ) = 0.000038621772;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 25 ) = -0.0034285382352;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 26 ) = 0.00000856589904;
				VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 27 ) = -0.000001516821552;

				//       set minimum and maximum boundaries for YorkCalc model input variables
				VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp = -34.4;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp = 29.4444;
				VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp = 1.1111;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp = 22.2222;
				VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp = 1.1111;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp = 40.0;
				VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio = 0.75;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio = 1.25;
				VSTower( SimpleTower( TowerNum ).VSTower ).MaxLiquidToGasRatio = 8.0;

			} else if ( SameString( AlphArray( 4 ), "CoolToolsUserDefined" ) ) {
				SimpleTower( TowerNum ).TowerModelType = CoolToolsUserDefined;
				// Nested Get-input routines below.  Should pull out of here and read in beforehand.
				for ( VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSCoolToolsModelCoeffs; ++VSModelCoeffNum ) {
					GetObjectItem( "CoolingTowerPerformance:CoolTools", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat );
					if ( ! SameString( AlphArray2( 1 ), SimpleTower( TowerNum ).ModelCoeffObjectName ) ) continue;
					VSTower( SimpleTower( TowerNum ).VSTower ).FoundModelCoeff = true;
					// verify the correct number of coefficients for the CoolTools model
					if ( NumNums2 != 43 ) {
						ShowSevereError( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\". The number of numeric inputs for object CoolingTowerPerformance:CoolTools \"" + SimpleTower( TowerNum ).ModelCoeffObjectName + "\" must equal 43." );
						ErrorsFound = true;
					} else {

						VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp = NumArray2( 1 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp = NumArray2( 2 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp = NumArray2( 3 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp = NumArray2( 4 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp = NumArray2( 5 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp = NumArray2( 6 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio = NumArray2( 7 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio = NumArray2( 8 );

						for ( CoeffNum = 9; CoeffNum <= NumNums2; ++CoeffNum ) {
							VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( CoeffNum - 8 ) = NumArray2( CoeffNum );
						}
					}
					break;
				}
				if ( ! VSTower( SimpleTower( TowerNum ).VSTower ).FoundModelCoeff ) {
					ShowSevereError( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\". User defined name for variable speed cooling tower model coefficients object not found = " + SimpleTower( TowerNum ).ModelCoeffObjectName );
					ErrorsFound = true;
				}
			} else if ( SameString( AlphArray( 4 ), "YorkCalcUserDefined" ) ) {
				SimpleTower( TowerNum ).TowerModelType = YorkCalcUserDefined;
				// Nested Get-input routines below.  Should pull out of here and read in beforehand.
				for ( VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSYorkCalcModelCoeffs; ++VSModelCoeffNum ) {
					GetObjectItem( "CoolingTowerPerformance:YorkCalc", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat );
					if ( ! SameString( AlphArray2( 1 ), SimpleTower( TowerNum ).ModelCoeffObjectName ) ) continue;
					VSTower( SimpleTower( TowerNum ).VSTower ).FoundModelCoeff = true;
					// verify the correct number of coefficients for the YorkCalc model
					if ( NumNums2 != 36 ) {
						ShowSevereError( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\". The number of numeric inputs for object CoolingTowerPerformance:YorkCalc \"" + SimpleTower( TowerNum ).ModelCoeffObjectName + "\" must equal 36." );
						ErrorsFound = true;
					} else {

						VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp = NumArray2( 1 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp = NumArray2( 2 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp = NumArray2( 3 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp = NumArray2( 4 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp = NumArray2( 5 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp = NumArray2( 6 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio = NumArray2( 7 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio = NumArray2( 8 );
						VSTower( SimpleTower( TowerNum ).VSTower ).MaxLiquidToGasRatio = NumArray2( 9 );

						for ( CoeffNum = 10; CoeffNum <= NumNums2; ++CoeffNum ) {
							VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( CoeffNum - 9 ) = NumArray2( CoeffNum );
						}
					}
					break;
				}

				if ( ! VSTower( SimpleTower( TowerNum ).VSTower ).FoundModelCoeff ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined name for variable speed cooling tower model coefficients object not found = " + SimpleTower( TowerNum ).ModelCoeffObjectName );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Illegal Tower Model Type = " + AlphArray( 5 ) );
				ShowContinueError( " Tower Model Type must be \"CoolToolsCrossFlow\", \"YorkCalc\", \"CoolToolsUserDefined\", or \"YorkCalcUserDefined." );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).TowerMassFlowRateMultiplier = VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio;

			//   check user defined minimums to be greater than 0
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined minimum approach temperature must be > 0" );
				ErrorsFound = true;
			}
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined minimum range temperature must be > 0" );
				ErrorsFound = true;
			}
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined minimum water flow rate ratio must be > 0" );
				ErrorsFound = true;
			}

			//   check that the user defined maximums are greater than the minimums
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp < VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined maximum approach temperature must be > the minimum approach temperature" );
				ErrorsFound = true;
			}
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp < VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined maximum range temperature must be > the minimum range temperature" );
				ErrorsFound = true;
			}
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio < VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined maximum water flow rate ratio must be > the minimum water flow rate ratio" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).DesignInletWB = NumArray( 1 );
			if ( NumArray( 1 ) < VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp || NumArray( 1 ) > VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp ) {
				gio::write( OutputChar, OutputFormat ) << SimpleTower( TowerNum ).DesignInletWB;
				gio::write( OutputCharLo, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp;
				gio::write( OutputCharHi, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp;
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" the design inlet air wet-bulb temperature of " + OutputChar + " must be within the model limits of " + OutputCharLo + " and " + OutputCharHi + " degrees C" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).DesignApproach = NumArray( 2 );
			if ( NumArray( 2 ) < VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp || NumArray( 2 ) > VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp ) {
				gio::write( OutputChar, OutputFormat ) << SimpleTower( TowerNum ).DesignApproach;
				gio::write( OutputCharLo, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp;
				gio::write( OutputCharHi, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp;
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" the design approach temperature of " + OutputChar + " must be within the model limits of " + OutputCharLo + " and " + OutputCharHi + " degrees C" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).DesignRange = NumArray( 3 );
			if ( NumArray( 3 ) < VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp || NumArray( 3 ) > VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp ) {
				gio::write( OutputChar, OutputFormat ) << SimpleTower( TowerNum ).DesignRange;
				gio::write( OutputCharLo, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp;
				gio::write( OutputCharHi, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp;
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" the design range temperature of " + OutputChar + " must be within the model limits of " + OutputCharLo + " and " + OutputCharHi + " degrees C" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).DesignWaterFlowRate = NumArray( 4 );
			if ( SimpleTower( TowerNum ).DesignWaterFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).DesignWaterFlowRateWasAutoSized = true;
			}
			if ( NumArray( 4 ) <= 0.0 && NumArray( 4 ) != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" design water flow rate must be > 0" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).HighSpeedAirFlowRate = NumArray( 5 );
			if ( SimpleTower( TowerNum ).HighSpeedAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized = true;
			}
			if ( NumArray( 5 ) <= 0.0 && NumArray( 5 ) != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" design air flow rate must be > 0" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).HighSpeedFanPower = NumArray( 6 );
			if ( SimpleTower( TowerNum ).HighSpeedFanPower == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedFanPowerWasAutoSized = true;
			}
			if ( NumArray( 6 ) <= 0.0 && NumArray( 6 ) != AutoSize ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" design fan power must be > 0" );
				ErrorsFound = true;
			}

			//   minimum air flow rate fraction must be >= 0.2 and <= 0.5, below this value the tower fan cycles to maintain the setpoint
			SimpleTower( TowerNum ).MinimumVSAirFlowFrac = NumArray( 7 );
			SimpleTower( TowerNum ).MinimumVSAirFlowFrac = NumArray( 7 );
			if ( NumArray( 7 ) < 0.2 || NumArray( 7 ) > 0.5 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" minimum VS air flow rate ratio must be >= 0.2 and <= 0.5" );
				ErrorsFound = true;
			}

			//   fraction of tower capacity in free convection regime must be >= to 0 and <= 0.2
			SimpleTower( TowerNum ).FreeConvectionCapacityFraction = NumArray( 8 );
			if ( NumArray( 8 ) < 0.0 || NumArray( 8 ) > 0.2 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" fraction of tower capacity in free convection regime must be >= 0 and <= 0.2" );
				ErrorsFound = true;
			}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff = NumArray( 9 );
			if ( NumArray( 9 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater power as a function of temperature difference must be >= 0" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).BasinHeaterSetPointTemp = NumArray( 10 );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 10 ) {
					SimpleTower( TowerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( SimpleTower( TowerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + SimpleTower( TowerNum ).Name + "\", " + cNumericFieldNames( 10 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			// Performance Input Method for Variable Speed Towers is assigned to be UA AND DESIGN WATER FLOW RATE
			// for autosizing calculations (see SizeTower)
			SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_UFactor;

			//   Makeup water drift percentage must be greater than or equal to 0
			//    SimpleTower(TowerNum)%MakeupWaterDrift          = NumArray(10)/100.0
			//     IF(NumArray(10) .LT. 0.0) THEN
			//       CALL ShowSevereError('COOLING TOWER:VARIABLE SPEED, "'//TRIM(SimpleTower(TowerNum)%Name)//&
			//                       '" Makeup Water Drift as a percentage of design water flow rate must be >= 0')
			//       ErrorsFound = .TRUE.
			//     END IF

			if ( ! AlphArray( 7 ).empty() ) {
				SimpleTower( TowerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( AlphArray( 7 ) );
				if ( SimpleTower( TowerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater schedule name \"" + AlphArray( 7 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

			//    IF(AlphArray(9) .NE. ' ')THEN
			//      SimpleTower(TowerNum)%BlowDownSchedulePtr       = GetScheduleIndex(AlphArray(9))
			//      IF(SimpleTower(TowerNum)%BlowDownSchedulePtr .EQ. 0)THEN
			//        CALL ShowWarningError('COOLING TOWER:VARIABLE SPEED, "'//TRIM(SimpleTower(TowerNum)%Name)//&
			//                       '" blowdown schedule name "'//TRIM(AlphArray(9)) &
			//                       //'" was not found. Basin blowdown will not be modeled and the simulation continues')
			//      END IF
			//    END IF

			// begin water use and systems get input
			if ( SameString( AlphArray( 8 ), "LossFactor" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByUserFactor;
			} else if ( SameString( AlphArray( 8 ), "SaturatedExit" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else if ( lAlphaFieldBlanks( 8 ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + AlphArray( 8 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).UserEvapLossFactor = NumArray( 11 ); //  N11 , \field Evaporation Loss Factor
			SimpleTower( TowerNum ).DriftLossFraction = NumArray( 12 ) / 100.0; //  N12, \field Drift Loss Percent
			SimpleTower( TowerNum ).ConcentrationRatio = NumArray( 13 ); //  N13, \field Blowdown Concentration Ratio
			SimpleTower( TowerNum ).SizFac = NumArray( 17 ); //  N14  \field Sizing Factor
			if ( SimpleTower( TowerNum ).SizFac <= 0.0 ) SimpleTower( TowerNum ).SizFac = 1.0;

			if ( SameString( AlphArray( 9 ), "ScheduledRate" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownBySchedule;
			} else if ( SameString( AlphArray( 9 ), "ConcentrationRatio" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
			} else if ( lAlphaFieldBlanks( 9 ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + AlphArray( 9 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}
			SimpleTower( TowerNum ).SchedIDBlowdown = GetScheduleIndex( AlphArray( 10 ) );
			if ( ( SimpleTower( TowerNum ).SchedIDBlowdown == 0 ) && ( SimpleTower( TowerNum ).BlowdownMode == BlowdownBySchedule ) ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + AlphArray( 10 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			//added for multi-cell
			SimpleTower( TowerNum ).NumCell = NumArray( 14 );
			if ( ( NumNums < 14 ) && ( SimpleTower( TowerNum ).NumCell == 0 ) ) {
				// assume Number of Cells not entered and should be defaulted
				SimpleTower( TowerNum ).NumCell = 1;
			}
			SimpleTower( TowerNum ).MinFracFlowRate = NumArray( 15 );
			if ( ( NumNums < 15 ) && ( SimpleTower( TowerNum ).MinFracFlowRate == 0.0 ) ) {
				// assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MinFracFlowRate = 0.33;
			}
			SimpleTower( TowerNum ).MaxFracFlowRate = NumArray( 16 );
			if ( ( NumNums < 16 ) && ( SimpleTower( TowerNum ).MaxFracFlowRate == 0.0 ) ) {
				// assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MaxFracFlowRate = 2.5;
			}

			if ( NumAlphas >= 13 ) {
				if ( lAlphaFieldBlanks( 13 ) || AlphArray( 13 ).empty() ) {
					SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
					SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
				} else {
					if ( SameString( AlphArray( 13 ), "MinimalCell" ) || SameString( AlphArray( 13 ), "MaximalCell" ) ) {
						if ( SameString( AlphArray( 13 ), "MinimalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MinCell;
							SimpleTower( TowerNum ).CellCtrl = "MinimalCell";
						}
						if ( SameString( AlphArray( 13 ), "MaximalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
							SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
						}
					} else {
						ShowSevereError( "Illegal " + cAlphaFieldNames( 13 ) + " = " + AlphArray( 13 ) );
						ShowContinueError( "Occurs in " + SimpleTower( TowerNum ).TowerType + '=' + SimpleTower( TowerNum ).Name );
						ErrorsFound = true;
					}
				}
			} else {
				//assume Cell Control not entered and should be defaulted
				SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
				SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
			}

			if ( lAlphaFieldBlanks( 11 ) ) {
				SimpleTower( TowerNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( AlphArray( 1 ), cCurrentModuleObject, AlphArray( 11 ), ErrorsFound, SimpleTower( TowerNum ).WaterTankID, SimpleTower( TowerNum ).WaterTankDemandARRID );
				SimpleTower( TowerNum ).SuppliedByWaterSystem = true;
			}

			//   outdoor air inlet node
			if ( lAlphaFieldBlanks( 12 ) ) {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = 0;
			} else {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = GetOnlySingleNode( AlphArray( 12 ), ErrorsFound, cCurrentModuleObject, SimpleTower( TowerNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ) ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray( 12 ) );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}
			}

		} // End Variable-Speed Tower Loop

		cCurrentModuleObject = cCoolingTower_VariableSpeedMerkel;
		for ( MerkelVSTowerNum = 1; MerkelVSTowerNum <= NumVSMerkelTowers; ++MerkelVSTowerNum ) {
			TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + MerkelVSTowerNum;
			GetObjectItem( cCurrentModuleObject, MerkelVSTowerNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SimpleTower, TowerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SimpleTower( TowerNum ).Name = AlphArray( 1 );
			SimpleTower( TowerNum ).TowerType = cCurrentModuleObject;
			SimpleTower( TowerNum ).TowerType_Num = CoolingTower_VariableSpeedMerkel;
			SimpleTower( TowerNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			SimpleTower( TowerNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Chilled Water Nodes" );

			if ( SameString( AlphArray( 4 ), "UFactorTimesAreaAndDesignWaterFlowRate" ) ) {
				SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_UFactor;
			} else if ( SameString( AlphArray( 4 ), "NominalCapacity" ) ) {
				SimpleTower( TowerNum ).PerformanceInputMethod_Num = PIM_NominalCapacity;
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).FanPowerfAirFlowCurve = GetCurveIndex( AlphArray( 5 ) );
			if ( SimpleTower( TowerNum ).FanPowerfAirFlowCurve == 0 ) {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + AlphArray( 5 ) );
				ShowContinueError( "Curve name not found." );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio = NumArray( 1 );
			SimpleTower( TowerNum ).TowerNominalCapacity = NumArray( 2 );
			if ( SimpleTower( TowerNum ).TowerNominalCapacity == AutoSize ) {
				SimpleTower( TowerNum ).TowerNominalCapacityWasAutoSized = true;
			}
			SimpleTower( TowerNum ).TowerFreeConvNomCap = NumArray( 3 );
			if ( SimpleTower( TowerNum ).TowerFreeConvNomCap == AutoSize ) {
				SimpleTower( TowerNum ).TowerFreeConvNomCapWasAutoSized = true;
			}
			SimpleTower( TowerNum ).TowerFreeConvNomCapSizingFactor = NumArray( 4 );
			SimpleTower( TowerNum ).DesignWaterFlowRate = NumArray( 5 );
			if (SimpleTower( TowerNum ).DesignWaterFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).DesignWaterFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).DesignWaterFlowPerUnitNomCap = NumArray( 6 );
			SimpleTower( TowerNum ).HighSpeedAirFlowRate = NumArray( 7 );
			if ( SimpleTower( TowerNum ).HighSpeedAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized = true;
			}
			if ( lNumericFieldBlanks( 8 ) ) {
				SimpleTower( TowerNum ).DefaultedDesignAirFlowScalingFactor = true;
			} else {
				SimpleTower( TowerNum ).DefaultedDesignAirFlowScalingFactor = false;
			}
			SimpleTower( TowerNum ).DesignAirFlowPerUnitNomCap = NumArray( 8 );
			SimpleTower( TowerNum ).MinimumVSAirFlowFrac = NumArray( 9 );
			SimpleTower( TowerNum ).HighSpeedFanPower = NumArray( 10 );
			if ( SimpleTower( TowerNum ).HighSpeedFanPower == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedFanPowerWasAutoSized = true;
			}
			SimpleTower( TowerNum ).DesignFanPowerPerUnitNomCap = NumArray( 11 );
			SimpleTower( TowerNum ).FreeConvAirFlowRate = NumArray( 12 );
			if ( SimpleTower( TowerNum ).FreeConvAirFlowRate == AutoSize ) {
				SimpleTower( TowerNum ).FreeConvAirFlowRateWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvAirFlowRateSizingFactor = NumArray( 13 );
			SimpleTower( TowerNum ).HighSpeedTowerUA = NumArray( 14 );
			if (SimpleTower( TowerNum ).HighSpeedTowerUA == AutoSize ) {
				SimpleTower( TowerNum ).HighSpeedTowerUAWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvTowerUA = NumArray( 15 );
			if ( SimpleTower( TowerNum ).FreeConvTowerUA == AutoSize ) {
				SimpleTower( TowerNum ).FreeConvTowerUAWasAutoSized = true;
			}
			SimpleTower( TowerNum ).FreeConvTowerUASizingFactor = NumArray( 16 );

			SimpleTower( TowerNum ).UAModFuncAirFlowRatioCurvePtr = GetCurveIndex( AlphArray( 6 ) );
			if ( SimpleTower( TowerNum ).UAModFuncAirFlowRatioCurvePtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + AlphArray( 6 ) );
				ShowContinueError( "Curve name not found." );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).UAModFuncWetBulbDiffCurvePtr = GetCurveIndex( AlphArray( 7 ) );
			if ( SimpleTower( TowerNum ).UAModFuncWetBulbDiffCurvePtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + AlphArray( 7 ) );
				ShowContinueError( "Curve name not found." );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).UAModFuncWaterFlowRatioCurvePtr = GetCurveIndex( AlphArray( 8 ) );
			if ( SimpleTower( TowerNum ).UAModFuncWaterFlowRatioCurvePtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + AlphArray( 8 ) );
				ShowContinueError( "Curve name not found." );
				ErrorsFound = true;
			}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff = NumArray( 17 );
			if ( NumArray( 17 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater power as a function of temperature difference must be >= 0" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).BasinHeaterSetPointTemp = NumArray( 18 );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 18 ) {
					SimpleTower( TowerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( SimpleTower( TowerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + SimpleTower( TowerNum ).Name + "\", " + cNumericFieldNames( 18 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! AlphArray( 9 ).empty() ) {
				SimpleTower( TowerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( AlphArray( 9 ) );
				if ( SimpleTower( TowerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" basin heater schedule name \"" + AlphArray( 9 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

			// begin water use and systems get input
			if ( SameString( AlphArray( 10 ), "LossFactor" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByUserFactor;
			} else if ( SameString( AlphArray( 10 ), "SaturatedExit" ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else if ( lAlphaFieldBlanks( 10 ) ) {
				SimpleTower( TowerNum ).EvapLossMode = EvapLossByMoistTheory;
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + AlphArray( 10 ) );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).UserEvapLossFactor = NumArray( 19 ); //  N19 , \field Evaporation Loss Factor
			SimpleTower( TowerNum ).DriftLossFraction = NumArray( 20 ) / 100.0; //  N20, \field Drift Loss Percent
			if ( ( NumNums < 20 ) && ( SimpleTower( TowerNum ).DriftLossFraction == 0.0 ) ) {
				// assume Drift loss not entered and should be defaulted
				SimpleTower( TowerNum ).DriftLossFraction = 0.008 / 100.0;
			}

			SimpleTower( TowerNum ).ConcentrationRatio = NumArray( 21 ); //  N21, \field Blowdown Concentration Ratio
			SimpleTower( TowerNum ).SizFac = NumArray( 25 ); //  N25  \field Sizing Factor
			if ( SimpleTower( TowerNum ).SizFac <= 0.0 ) SimpleTower( TowerNum ).SizFac = 1.0;

			if ( SameString( AlphArray( 11 ), "ScheduledRate" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownBySchedule;
			} else if ( SameString( AlphArray( 11 ), "ConcentrationRatio" ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
			} else if ( lAlphaFieldBlanks( 11 ) ) {
				SimpleTower( TowerNum ).BlowdownMode = BlowdownByConcentration;
				if ( ( NumNums < 21 ) && ( SimpleTower( TowerNum ).ConcentrationRatio == 0.0 ) ) {
					// assume Concetration ratio was omitted and should be defaulted
					SimpleTower( TowerNum ).ConcentrationRatio = 3.0;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + AlphArray( 11 ) );
				ErrorsFound = true;
			}
			SimpleTower( TowerNum ).SchedIDBlowdown = GetScheduleIndex( AlphArray( 12 ) );
			if ( ( SimpleTower( TowerNum ).SchedIDBlowdown == 0 ) && ( SimpleTower( TowerNum ).BlowdownMode == BlowdownBySchedule ) ) {
				ShowSevereError( cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 12 ) + '=' + AlphArray( 12 ) );
				ErrorsFound = true;
			}

			//added for multi-cell
			SimpleTower( TowerNum ).NumCell = NumArray( 22 );
			if ( ( NumNums < 22 ) && ( SimpleTower( TowerNum ).NumCell == 0 ) ) {
				// assume Number of Cells not entered and should be defaulted
				SimpleTower( TowerNum ).NumCell = 1;
			}
			SimpleTower( TowerNum ).MinFracFlowRate = NumArray( 23 );
			if ( ( NumNums < 23 ) && ( SimpleTower( TowerNum ).MinFracFlowRate == 0.0 ) ) {
				// assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MinFracFlowRate = 0.33;
			}
			SimpleTower( TowerNum ).MaxFracFlowRate = NumArray( 24 );
			if ( ( NumNums < 24 ) && ( SimpleTower( TowerNum ).MaxFracFlowRate == 0.0 ) ) {
				// assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
				SimpleTower( TowerNum ).MaxFracFlowRate = 2.5;
			}
			SimpleTower( TowerNum ).TowerMassFlowRateMultiplier = SimpleTower( TowerNum ).MaxFracFlowRate;
			if ( NumAlphas >= 15 ) {
				if ( lAlphaFieldBlanks( 15 ) || AlphArray( 15 ).empty() ) {
					SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
					SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
				} else {
					if ( SameString( AlphArray( 15 ), "MinimalCell" ) || SameString( AlphArray( 15 ), "MaximalCell" ) ) {
						if ( SameString( AlphArray( 15 ), "MinimalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MinCell;
							SimpleTower( TowerNum ).CellCtrl = "MinimalCell";
						}
						if ( SameString( AlphArray( 15 ), "MaximalCell" ) ) {
							SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
							SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
						}
					} else {
						ShowSevereError( "Illegal " + cAlphaFieldNames( 15 ) + " = " + AlphArray( 15 ) );
						ShowContinueError( "Occurs in " + SimpleTower( TowerNum ).TowerType + '=' + SimpleTower( TowerNum ).Name );
						ErrorsFound = true;
					}
				}
			} else {
				//assume Cell Control not entered and should be defaulted
				SimpleTower( TowerNum ).CellCtrl = "MaximalCell";
				SimpleTower( TowerNum ).CellCtrl_Num = CellCtrl_MaxCell;
			}

			if ( lAlphaFieldBlanks( 13 ) ) {
				SimpleTower( TowerNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( AlphArray( 1 ), cCurrentModuleObject, AlphArray( 13 ), ErrorsFound, SimpleTower( TowerNum ).WaterTankID, SimpleTower( TowerNum ).WaterTankDemandARRID );
				SimpleTower( TowerNum ).SuppliedByWaterSystem = true;
			}

			//   outdoor air inlet node
			if ( lAlphaFieldBlanks( 14 ) ) {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = 0;
			} else {
				SimpleTower( TowerNum ).OutdoorAirInletNodeNum = GetOnlySingleNode( AlphArray( 14 ), ErrorsFound, cCurrentModuleObject, SimpleTower( TowerNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ) ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray( 14 ) );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}
			}

		} // end merkel vs tower loop

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in getting cooling tower input." );
		}

		// Set up output variables CurrentModuleObject='CoolingTower:SingleSpeed'
		for ( TowerNum = 1; TowerNum <= NumSingleSpeedTowers; ++TowerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTowerReport( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTowerReport( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTowerReport( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTowerReport( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTowerReport( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTowerReport( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			// Added for fluid bypass
			SetupOutputVariable( "Cooling Tower Bypass Fraction []", SimpleTowerReport( TowerNum ).BypassFraction, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTowerReport( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Cycling Ratio []", SimpleTowerReport( TowerNum ).FanCyclingRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTowerReport( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTowerReport( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}
		}

		// CurrentModuleObject='CoolingTower:TwoSpeed'
		for ( TowerNum = NumSingleSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers; ++TowerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTowerReport( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTowerReport( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTowerReport( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTowerReport( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTowerReport( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTowerReport( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			SetupOutputVariable( "Cooling Tower Fan Cycling Ratio []", SimpleTowerReport( TowerNum ).FanCyclingRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Speed Level []", SimpleTowerReport( TowerNum ).SpeedSelected, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTowerReport( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTowerReport( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTowerReport( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}
		}

		// CurrentModuleObject='CoolingTower:VariableSpeed'
		for ( TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers; ++TowerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTowerReport( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTowerReport( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTowerReport( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTowerReport( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTowerReport( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTowerReport( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			SetupOutputVariable( "Cooling Tower Air Flow Rate Ratio []", SimpleTowerReport( TowerNum ).AirFlowRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Part Load Ratio []", SimpleTowerReport( TowerNum ).FanCyclingRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTowerReport( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTowerReport( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTowerReport( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}

			//    CALL SetupOutputVariable('Tower Makeup Water Consumption [m3]', &
			//          SimpleTowerReport(TowerNum)%WaterAmountUsed,'System','Sum',SimpleTower(TowerNum)%Name, &
			//                                  ResourceTypeKey='Water',EndUseKey='HeatRejection',GroupKey='Plant')

		}

		// CurrentModuleObject='CoolingTower:VariableSpeed:Merkel'
		for ( TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers; ++TowerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTowerReport( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTowerReport( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTowerReport( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTowerReport( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTowerReport( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTowerReport( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			SetupOutputVariable( "Cooling Tower Fan Speed Ratio []", SimpleTowerReport( TowerNum ).AirFlowRatio, "System", "Average", SimpleTower( TowerNum ).Name );

			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTowerReport( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTowerReport( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTowerReport( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}
		}
		// setup common water reporting for all types of towers.
		for ( TowerNum = 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers; ++TowerNum ) {
			if ( SimpleTower( TowerNum ).SuppliedByWaterSystem ) {
				SetupOutputVariable( "Cooling Tower Make Up Water Volume Flow Rate [m3/s]", SimpleTowerReport( TowerNum ).MakeUpVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Water Volume [m3]", SimpleTowerReport( TowerNum ).MakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]", SimpleTowerReport( TowerNum ).TankSupplyVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Storage Tank Water Volume [m3]", SimpleTowerReport( TowerNum ).TankSupplyVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Water", "HeatRejection", _, "Plant" );
				SetupOutputVariable( "Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]", SimpleTowerReport( TowerNum ).StarvedMakeUpVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Starved Storage Tank Water Volume [m3]", SimpleTowerReport( TowerNum ).StarvedMakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Mains Water Volume [m3]", SimpleTowerReport( TowerNum ).StarvedMakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "MainsWater", "HeatRejection", _, "Plant" );
			} else { // tower water from mains and gets metered
				SetupOutputVariable( "Cooling Tower Make Up Water Volume Flow Rate [m3/s]", SimpleTowerReport( TowerNum ).MakeUpVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Water Volume [m3]", SimpleTowerReport( TowerNum ).MakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Water", "HeatRejection", _, "Plant" );
				SetupOutputVariable( "Cooling Tower Make Up Mains Water Volume [m3]", SimpleTowerReport( TowerNum ).MakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "MainsWater", "HeatRejection", _, "Plant" );
			}

			SetupOutputVariable( "Cooling Tower Water Evaporation Volume Flow Rate [m3/s]", SimpleTowerReport( TowerNum ).EvaporationVdot, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Evaporation Volume [m3]", SimpleTowerReport( TowerNum ).EvaporationVol, "System", "Sum", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Drift Volume Flow Rate [m3/s]", SimpleTowerReport( TowerNum ).DriftVdot, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Drift Volume [m3]", SimpleTowerReport( TowerNum ).DriftVol, "System", "Sum", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Blowdown Volume Flow Rate [m3/s]", SimpleTowerReport( TowerNum ).BlowdownVdot, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Blowdown Volume [m3]", SimpleTowerReport( TowerNum ).BlowdownVol, "System", "Sum", SimpleTower( TowerNum ).Name );
		} // loop all towers

	}

	// End of Get Input subroutines for the CondenserLoopTowers Module
	//******************************************************************************

	// Beginning Initialization Section for the CondenserLoopTowers Module
	//******************************************************************************

	void
	InitSimVars()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998
		//       MODIFIED         Jan 2001, Richard Raustad
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

		InletWaterTemp = 0.0; // CW temperature at tower inlet
		OutletWaterTemp = 0.0; // CW temperature at tower outlet
		WaterInletNode = 0; // Node number at tower inlet
		WaterOutletNode = 0; // Node number at tower outlet
		WaterMassFlowRate = 0.0; // WaterMassFlowRate through tower
		// TowerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
		// TowerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
		//LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
		//LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
		Qactual = 0.0; // Tower heat transfer
		CTFanPower = 0.0; // Tower fan power used
		AirFlowRateRatio = 0.0; // Ratio of air flow rate through VS cooling tower to design air flow rate
		BasinHeaterPower = 0.0; // Basin heater power use (W)
		WaterUsage = 0.0; // Tower water usage (m3/s)
		FanCyclingRatio = 0.0; // cycling ratio of tower fan when min fan speed provide to much capacity

	}

	void
	InitTower(
		int const TowerNum, // Number of the current cooling tower being simulated
		bool const EP_UNUSED( RunFlag ) // Indication of
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2002
		//       MODIFIED       Don Shirey Sept/Oct 2002, F Buhl Oct 2002
		//       RE-ENGINEERED  R. Raustad, Oct 2005, moved Max/MinAvail to Init and allowed more than design
		//                      water flow rate to pass through towers (up to 2.5 and 1.25 times the design flow
		//                      for 1 or 2-speed and variable speed towers, respectively). Flow multiplier for
		//                      VS Tower is defaulted to 1.25 and can be reassigned by user.

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Cooling Tower components and for
		// final checking of tower inputs (post autosizing)

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using InputProcessor::SameString;
		using DataPlant::TypeOf_CoolingTower_SingleSpd;
		using DataPlant::TypeOf_CoolingTower_TwoSpd;
		using DataPlant::TypeOf_CoolingTower_VarSpd;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::TypeOf_CoolingTower_VarSpdMerkel;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegulateCondenserCompFlowReqOp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitTower" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Flag if input data errors are found

		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool OneTimeFlagForEachTower;
		//  LOGICAL                                 :: FatalError
		int TypeOf_Num( 0 );
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		Real64 rho; // local density of fluid

		// Do the one time initializations
		if ( InitTowerOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumSimpleTowers );
			OneTimeFlagForEachTower.allocate( NumSimpleTowers );

			OneTimeFlagForEachTower = true;
			MyEnvrnFlag = true;
			InitTowerOneTimeFlag = false;

		}

		if ( OneTimeFlagForEachTower( TowerNum ) ) {

			if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_SingleSpeed ) {
				TypeOf_Num = TypeOf_CoolingTower_SingleSpd;
			} else if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_TwoSpeed ) {
				TypeOf_Num = TypeOf_CoolingTower_TwoSpd;
			} else if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_VariableSpeed ) {
				TypeOf_Num = TypeOf_CoolingTower_VarSpd;
			} else if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_VariableSpeedMerkel ) {
				TypeOf_Num = TypeOf_CoolingTower_VarSpdMerkel;
			} else {
				assert( false );
			}

			// Locate the tower on the plant loops for later usage
			ScanPlantLoopsForObject( SimpleTower( TowerNum ).Name, TypeOf_Num, SimpleTower( TowerNum ).LoopNum, SimpleTower( TowerNum ).LoopSideNum, SimpleTower( TowerNum ).BranchNum, SimpleTower( TowerNum ).CompNum, _, _, _, _, _, ErrorsFound );
			if ( ErrorsFound ) {
				ShowFatalError( "InitTower: Program terminated due to previous condition(s)." );
			}

			// check if setpoint on outlet node
			if ( ( Node( SimpleTower( TowerNum ).WaterOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( SimpleTower( TowerNum ).WaterOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
				SimpleTower( TowerNum ).SetpointIsOnOutlet = false;
			} else {
				SimpleTower( TowerNum ).SetpointIsOnOutlet = true;
			}

			OneTimeFlagForEachTower( TowerNum ) = false;

		}

		// Begin environment initializations
		if ( MyEnvrnFlag( TowerNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

			SimpleTower( TowerNum ).DesWaterMassFlowRate = SimpleTower( TowerNum ).DesignWaterFlowRate * rho;
			SimpleTower( TowerNum ).DesWaterMassFlowRatePerCell = SimpleTower( TowerNum ).DesWaterMassFlowRate / SimpleTower( TowerNum ).NumCell;
			InitComponentNodes( 0.0, SimpleTower( TowerNum ).DesWaterMassFlowRate, SimpleTower( TowerNum ).WaterInletNodeNum, SimpleTower( TowerNum ).WaterOutletNodeNum, SimpleTower( TowerNum ).LoopNum, SimpleTower( TowerNum ).LoopSideNum, SimpleTower( TowerNum ).BranchNum, SimpleTower( TowerNum ).CompNum );

			MyEnvrnFlag( TowerNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( TowerNum ) = true;
		}

		// Each time initializations
		WaterInletNode = SimpleTower( TowerNum ).WaterInletNodeNum;
		SimpleTowerInlet( TowerNum ).WaterTemp = Node( WaterInletNode ).Temp;

		if ( SimpleTower( TowerNum ).OutdoorAirInletNodeNum != 0 ) {
			SimpleTowerInlet( TowerNum ).AirTemp = Node( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ).Temp;
			SimpleTowerInlet( TowerNum ).AirHumRat = Node( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ).HumRat;
			SimpleTowerInlet( TowerNum ).AirPress = Node( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ).Press;
			//    SimpleTowerInlet(TowerNum)%AirWetBulb = PsyTwbFnTdbWPb(SimpleTowerInlet(TowerNum)%AirTemp, &
			//                                            SimpleTowerInlet(TowerNum)%AirHumRat,SimpleTowerInlet(TowerNum)%AirPress)
			SimpleTowerInlet( TowerNum ).AirWetBulb = Node( SimpleTower( TowerNum ).OutdoorAirInletNodeNum ).OutAirWetBulb;
		} else {
			SimpleTowerInlet( TowerNum ).AirTemp = OutDryBulbTemp;
			SimpleTowerInlet( TowerNum ).AirHumRat = OutHumRat;
			SimpleTowerInlet( TowerNum ).AirPress = OutBaroPress;
			SimpleTowerInlet( TowerNum ).AirWetBulb = OutWetBulbTemp;
		}

		LoopNum = SimpleTower( TowerNum ).LoopNum;
		LoopSideNum = SimpleTower( TowerNum ).LoopSideNum;
		BranchIndex = SimpleTower( TowerNum ).BranchNum;
		CompIndex = SimpleTower( TowerNum ).CompNum;

		WaterMassFlowRate = RegulateCondenserCompFlowReqOp( SimpleTower( TowerNum ).LoopNum, SimpleTower( TowerNum ).LoopSideNum, SimpleTower( TowerNum ).BranchNum, SimpleTower( TowerNum ).CompNum, SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).TowerMassFlowRateMultiplier );

		SetComponentFlowRate( WaterMassFlowRate, SimpleTower( TowerNum ).WaterInletNodeNum, SimpleTower( TowerNum ).WaterOutletNodeNum, SimpleTower( TowerNum ).LoopNum, SimpleTower( TowerNum ).LoopSideNum, SimpleTower( TowerNum ).BranchNum, SimpleTower( TowerNum ).CompNum );

		// Added for fluid bypass. 8/2008
		SimpleTower( TowerNum ).BypassFraction = 0.0;
		BasinHeaterPower = 0.0;

	}

	void
	SizeTower( int const TowerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2002
		//       MODIFIED       Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Cooling Tower Components for which capacities and flow rates
		// have not been specified in the input. This subroutine also calculates tower UA if the user
		// has specified tower performance via the "Nominal Capacity" method.

		// METHODOLOGY EMPLOYED:
		// Obtains condenser flow rate from the plant sizing array. If tower performance is specified
		// via the "Nominal Capacity" method, the water flow rate is directly proportional to capacity.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using General::SolveRegulaFalsi;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt OutputFormat( "(F6.2)" );
		static gio::Fmt OutputFormat2( "(F9.6)" );
		int const MaxIte( 500 ); // Maximum number of iterations
		Real64 const Acc( 0.0001 ); // Accuracy of result
		static std::string const RoutineName( "SizeTower" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizCondNum( 0 ); // Plant Sizing index for condenser loop
		int SolFla; // Flag of solver
		Real64 DesTowerLoad( 0.0 ); // Design tower load [W]
		Real64 UA0; // Lower bound for UA [W/C]
		Real64 UA1; // Upper bound for UA [W/C]
		Real64 UA; // Calculated UA value
		Real64 Twb; // tower inlet air wet-bulb temperature [C]
		Real64 Tr; // tower range temperature [C]
		Real64 Ta; // tower approach temperature [C]
		Real64 WaterFlowRatio( 0.0 ); // tower water flow rate ratio found during model calibration
		Real64 MaxWaterFlowRateRatio; // maximum water flow rate ratio which yields desired approach temp
		Real64 WaterFlowRateRatio( 0.0 ); // tower water flow rate ratio
		Real64 Tapproach; // temporary tower approach temp variable [C]
		Real64 ModelWaterFlowRatioMax; // maximum water flow rate ratio used for model calibration
		Real64 FlowRateRatioStep; // flow rate ratio to determine maximum water flow rate ratio during calibration
		Array1D< Real64 > Par( 6 ); // Parameter array need for RegulaFalsi routine
		bool ModelCalibrated; // TRUE if water flow rate ratio is with the specified range
		std::string OutputChar; // report variable for warning messages
		std::string OutputChar2; // report variable for warning messages
		std::string OutputCharLo; // report variable for warning messages
		std::string OutputCharHi; // report variable for warning messages
		std::string equipName;
		Real64 Cp; // local specific heat for fluid
		Real64 rho; // local density for fluid
		Real64 tmpDesignWaterFlowRate; // local temporary for water volume flow rate
		Real64 tmpHighSpeedFanPower; // local temporary for high speed fan power
		Real64 tmpHighSpeedAirFlowRate; // local temporary for high speed air flow rate
		Real64 tmpLowSpeedAirFlowRate; // local temporary for low speed air flow rate
		Real64 AssumedDeltaT; // default delta T for nominal capacity of hard sized with UA method
		Real64 AssumedExitTemp; // default for cp fo nominal capacity of hard sized with UA method
		bool ErrorsFound;

		tmpDesignWaterFlowRate = SimpleTower( TowerNum ).DesignWaterFlowRate;
		tmpHighSpeedFanPower = SimpleTower( TowerNum ).HighSpeedFanPower;
		tmpHighSpeedAirFlowRate = SimpleTower( TowerNum ).HighSpeedAirFlowRate;
		tmpLowSpeedAirFlowRate = SimpleTower( TowerNum ).LowSpeedAirFlowRate;

		// Find the appropriate Plant Sizing object
		PltSizCondNum = PlantLoop( SimpleTower( TowerNum ).LoopNum ).PlantSizNum;

		if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_UFactor && (! SimpleTower( TowerNum ).HighSpeedTowerUAWasAutoSized )) {
			if ( PltSizCondNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
				DesTowerLoad = rho * Cp * SimpleTower( TowerNum ).DesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
				SimpleTower( TowerNum ).TowerNominalCapacity = DesTowerLoad / SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
			} else {
				AssumedDeltaT = 11.0;
				AssumedExitTemp = 21.0;
				rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, AssumedExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, AssumedExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

				DesTowerLoad = rho * Cp * SimpleTower( TowerNum ).DesignWaterFlowRate * AssumedDeltaT;
				SimpleTower( TowerNum ).TowerNominalCapacity = DesTowerLoad / SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
			}
		}

		if ( SimpleTower( TowerNum ).DesignWaterFlowRateWasAutoSized ) {
			if ( PltSizCondNum > 0 ) {
				if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					tmpDesignWaterFlowRate = PlantSizData( PltSizCondNum ).DesVolFlowRate * SimpleTower( TowerNum ).SizFac;
					if ( PlantFirstSizesOkayToFinalize ) SimpleTower( TowerNum ).DesignWaterFlowRate = tmpDesignWaterFlowRate;
				} else {
					tmpDesignWaterFlowRate = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) SimpleTower( TowerNum ).DesignWaterFlowRate = tmpDesignWaterFlowRate;
				}
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Design Water Flow Rate [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Design Water Flow Rate [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					ShowSevereError( "Autosizing error for cooling tower object = " + SimpleTower( TowerNum ).Name );
					ShowFatalError( "Autosizing of cooling tower condenser flow rate requires a loop Sizing:Plant object." );
				}

			}
		}

		if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			// Design water flow rate is assumed to be 3 gpm per ton (SI equivalent 5.382E-8 m3/s per watt)
			SimpleTower( TowerNum ).DesignWaterFlowRate = 5.382e-8 * SimpleTower( TowerNum ).TowerNominalCapacity;
			tmpDesignWaterFlowRate = SimpleTower( TowerNum ).DesignWaterFlowRate;
			if ( SameString( SimpleTower( TowerNum ).TowerType, "CoolingTower:SingleSpeed" ) ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Design Water Flow Rate based on tower nominal capacity [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Design Water Flow Rate based on tower nominal capacity [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
				}
			} else if ( SameString( SimpleTower( TowerNum ).TowerType, "CoolingTower:TwoSpeed" ) ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
				}
			}
		}

		RegisterPlantCompDesignFlow( SimpleTower( TowerNum ).WaterInletNodeNum, tmpDesignWaterFlowRate );

		if ( SimpleTower( TowerNum ).HighSpeedFanPowerWasAutoSized ) {
			// We assume the nominal fan power is 0.0105 times the design load
			if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {
				SimpleTower( TowerNum ).HighSpeedFanPower = 0.0105 * SimpleTower( TowerNum ).TowerNominalCapacity;
			} else {
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
						DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
						tmpHighSpeedFanPower = 0.0105 * DesTowerLoad;
						if ( PlantFirstSizesOkayToFinalize ) SimpleTower( TowerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
					} else {
						tmpHighSpeedFanPower = 0.0;
						if ( PlantFirstSizesOkayToFinalize ) SimpleTower( TowerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
					}
				} else {
				if ( PlantFinalSizesOkayToReport ) {
						ShowSevereError( "Autosizing of cooling tower fan power requires a loop Sizing:Plant object." );
						ShowFatalError( " Occurs in cooling tower object= " + SimpleTower( TowerNum ).Name );
					}
				}
			}
			if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_SingleSpeed || SimpleTower( TowerNum ).TowerType_Num == CoolingTower_VariableSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Fan Power at Design Air Flow Rate [W]", SimpleTower( TowerNum ).HighSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Fan Power at Design Air Flow Rate [W]", SimpleTower( TowerNum ).HighSpeedFanPower );
				}
			} else if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Fan Power at High Fan Speed [W]", SimpleTower( TowerNum ).HighSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Fan Power at High Fan Speed [W]", SimpleTower( TowerNum ).HighSpeedFanPower );
				}
			}
		}

		if ( SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized ) {
			// Plant Sizing Object is not required to AUTOSIZE this field since its simply a multiple of another field.
			tmpHighSpeedAirFlowRate = tmpHighSpeedFanPower * 0.5 * ( 101325.0 / StdBaroPress ) / 190.0;
			if ( PlantFirstSizesOkayToFinalize ) SimpleTower( TowerNum ).HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;

			if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_SingleSpeed || SimpleTower( TowerNum ).TowerType_Num == CoolingTower_VariableSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
				}
			} else if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Air Flow Rate at High Fan Speed [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Air Flow Rate at High Fan Speed [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
				}
			}
		}

		if ( SimpleTower( TowerNum ).HighSpeedTowerUAWasAutoSized ) {
			if ( PltSizCondNum > 0 ) {
				if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
					DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;

					// This conditional statement is to trap when the user specified condenser/tower water design setpoint
					//  temperature is less than design inlet air wet bulb temperature of 25.6 C
					if ( PlantSizData( PltSizCondNum ).ExitTemp <= 25.6 ) {
						ShowSevereError( "Error when autosizing the UA value for cooling tower = " + SimpleTower( TowerNum ).Name + ". Design Loop Exit Temperature must be greater than 25.6 C when autosizing the tower UA." );
						ShowContinueError( "The Design Loop Exit Temperature specified in Sizing:Plant object = " + PlantSizData( PltSizCondNum ).PlantLoopName );
						ShowContinueError( "is less than or equal to the design inlet air wet-bulb temperature of 25.6 C." );
						ShowContinueError( "It is recommended that the Design Loop Exit Temperature = 25.6 C plus the cooling tower design approach temperature (e.g., 4 C)." );
						ShowContinueError( "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be > 25.6 C if autosizing the cooling tower." );
						ShowFatalError( "Autosizing of cooling tower fails for tower = " + SimpleTower( TowerNum ).Name + '.' );
					}
					Par( 1 ) = DesTowerLoad;
					Par( 2 ) = double( TowerNum );
					Par( 3 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
					Par( 4 ) = tmpHighSpeedAirFlowRate; // design air volume flow rate
					Par( 5 ) = Cp;
					UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
					UA1 = DesTowerLoad; // Assume deltaT = 1K
					SimpleTowerInlet( TowerNum ).WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					SimpleTowerInlet( TowerNum ).AirTemp = 35.0;
					SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6;
					SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
					SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
					//        SimpleTowerInlet(TowerNum)%AirHumRat = PsyWFnTdbTwbPb(35.,25.6,StdBaroPress)
					SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
					if ( SolFla == -1 ) {
						ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
					} else if ( SolFla == -2 ) {
						ShowSevereError( "Bad starting values for UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
					}

					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).HighSpeedTowerUA = UA;
					}
					SimpleTower( TowerNum ).TowerNominalCapacity = DesTowerLoad / SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).HighSpeedTowerUA = 0.0;
					}
				}
				if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_SingleSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
					}
				} else if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_TwoSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"U-Factor Times Area Value at High Fan Speed [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial U-Factor Times Area Value at High Fan Speed [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
					}
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					ShowSevereError( "Autosizing error for cooling tower object= " + SimpleTower( TowerNum ).Name );
					ShowFatalError( "Autosizing of cooling tower UA requires a loop Sizing:Plant object." );
				}

			}
		}

		if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			if ( SimpleTower( TowerNum ).DesignWaterFlowRate >= SmallWaterVolFlow ) {
				// nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of delivered cooling but now is a user input
				rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, 29.44, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, 29.44, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp

				DesTowerLoad = SimpleTower( TowerNum ).TowerNominalCapacity * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesTowerLoad;
				Par( 2 ) = double( TowerNum );
				Par( 3 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 4 ) = tmpHighSpeedAirFlowRate; // design air volume flow rate
				Par( 5 ) = Cp; // 85F design exiting water temp
				UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesTowerLoad; // Assume deltaT = 1K
				SimpleTowerInlet( TowerNum ).WaterTemp = 35.0; // 95F design inlet water temperature
				SimpleTowerInlet( TowerNum ).AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
				SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
				//      SimpleTowerInlet(TowerNum)%AirHumRat = PsyWFnTdbTwbPb(35.,25.6,StdBaroPress)
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).HighSpeedTowerUA = UA;
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).HighSpeedTowerUA = 0.0;
				}
			}
			if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_SingleSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
				}
			} else if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"U-Factor Times Area Value at High Fan Speed [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial U-Factor Times Area Value at High Fan Speed [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
				}
			}
		}

		if ( SimpleTower( TowerNum ).LowSpeedAirFlowRateWasAutoSized ) {

			if ( PlantFirstSizesOkayToFinalize ) {
				SimpleTower( TowerNum ).LowSpeedAirFlowRate = SimpleTower( TowerNum ).LowSpeedAirFlowRateSizingFactor * SimpleTower( TowerNum ).HighSpeedAirFlowRate;
				tmpLowSpeedAirFlowRate = SimpleTower( TowerNum ).LowSpeedAirFlowRate;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Low Fan Speed Air Flow Rate [m3/s]", SimpleTower( TowerNum ).LowSpeedAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Low Fan Speed Air Flow Rate [m3/s]", SimpleTower( TowerNum ).LowSpeedAirFlowRate );
				}
			} else {
				tmpLowSpeedAirFlowRate = SimpleTower( TowerNum ).LowSpeedAirFlowRateSizingFactor * tmpHighSpeedAirFlowRate;
			}
		}

		if ( SimpleTower( TowerNum ).LowSpeedFanPowerWasAutoSized ) {
			if ( PlantFirstSizesOkayToFinalize ) {
				SimpleTower( TowerNum ).LowSpeedFanPower = SimpleTower( TowerNum ).LowSpeedFanPowerSizingFactor * SimpleTower( TowerNum ).HighSpeedFanPower;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Fan Power at Low Fan Speed [W]", SimpleTower( TowerNum ).LowSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Fan Power at Low Fan Speed [W]", SimpleTower( TowerNum ).LowSpeedFanPower );
				}
			}
		}

		if ( SimpleTower( TowerNum ).LowSpeedTowerUAWasAutoSized && PlantFirstSizesOkayToFinalize ) {
			SimpleTower( TowerNum ).LowSpeedTowerUA = SimpleTower( TowerNum ).LowSpeedTowerUASizingFactor * SimpleTower( TowerNum ).HighSpeedTowerUA;
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"U-Factor Times Area Value at Low Fan Speed [W/K]", SimpleTower( TowerNum ).LowSpeedTowerUA );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Initial U-Factor Times Area Value at Low Fan Speed [W/K]", SimpleTower( TowerNum ).LowSpeedTowerUA );
			}
		}

		if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			if ( SimpleTower( TowerNum ).TowerLowSpeedNomCapWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).TowerLowSpeedNomCap = SimpleTower( TowerNum ).TowerLowSpeedNomCapSizingFactor * SimpleTower( TowerNum ).TowerNominalCapacity;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Low Speed Nominal Capacity [W]", SimpleTower( TowerNum ).TowerLowSpeedNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial Low Speed Nominal Capacity [W]", SimpleTower( TowerNum ).TowerLowSpeedNomCap );
					}
				}
			}
			if ( SimpleTower( TowerNum ).TowerFreeConvNomCapWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).TowerFreeConvNomCap = SimpleTower( TowerNum ).TowerFreeConvNomCapSizingFactor * SimpleTower( TowerNum ).TowerNominalCapacity;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
					}
				}
			}

		}

		if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity && SameString( SimpleTower( TowerNum ).TowerType, "CoolingTower:TwoSpeed" ) ) {
			if ( SimpleTower( TowerNum ).DesignWaterFlowRate >= SmallWaterVolFlow && SimpleTower( TowerNum ).TowerLowSpeedNomCap > 0.0 ) {
				// nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now is a user input
				rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, 29.44, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, 29.44, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				DesTowerLoad = SimpleTower( TowerNum ).TowerLowSpeedNomCap * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesTowerLoad;
				Par( 2 ) = double( TowerNum );
				Par( 3 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 4 ) = tmpLowSpeedAirFlowRate; // Air volume flow rate at low fan speed
				Par( 5 ) = Cp; // 85F design exiting water temp
				UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesTowerLoad; // Assume deltaT = 1K
				SimpleTowerInlet( TowerNum ).WaterTemp = 35.0; // 95F design inlet water temperature
				SimpleTowerInlet( TowerNum ).AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
				SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).LowSpeedTowerUA = UA;
				}
			} else {
				SimpleTower( TowerNum ).LowSpeedTowerUA = 0.0;
			}
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Low Fan Speed U-Factor Times Area Value [W/K]", SimpleTower( TowerNum ).LowSpeedTowerUA );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Initial Low Fan Speed U-Factor Times Area Value [W/K]", SimpleTower( TowerNum ).LowSpeedTowerUA );
			}
		}

		if ( SimpleTower( TowerNum ).FreeConvAirFlowRateWasAutoSized ) {
			if ( PlantFirstSizesOkayToFinalize ) {
				SimpleTower( TowerNum ).FreeConvAirFlowRate = SimpleTower( TowerNum ).FreeConvAirFlowRateSizingFactor * SimpleTower( TowerNum ).HighSpeedAirFlowRate;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
				}
			}
		}

		if ( SimpleTower( TowerNum ).FreeConvTowerUAWasAutoSized ) {
			if ( PlantFirstSizesOkayToFinalize ) {
				SimpleTower( TowerNum ).FreeConvTowerUA = SimpleTower( TowerNum ).FreeConvTowerUASizingFactor * SimpleTower( TowerNum ).HighSpeedTowerUA;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Free Convection U-Factor Times Area Value [W/K]", SimpleTower( TowerNum ).FreeConvTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Free Convection U-Factor Times Area Value [W/K]", SimpleTower( TowerNum ).FreeConvTowerUA );
				}
			}
		}

		if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			if ( SimpleTower( TowerNum ).DesignWaterFlowRate >= SmallWaterVolFlow && SimpleTower( TowerNum ).TowerFreeConvNomCap > 0.0 ) {
				// nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now user input
				rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, 29.44, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, 29.44, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				DesTowerLoad = SimpleTower( TowerNum ).TowerFreeConvNomCap * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesTowerLoad;
				Par( 2 ) = double( TowerNum );
				Par( 3 ) = rho * SimpleTower( TowerNum ).DesignWaterFlowRate; // design water mass flow rate
				Par( 4 ) = SimpleTower( TowerNum ).FreeConvAirFlowRate; // free convection air volume flow rate
				Par( 5 ) = Cp; // 85F design exiting water temp
				UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesTowerLoad; // Assume deltaT = 1K
				SimpleTowerInlet( TowerNum ).WaterTemp = 35.0; // 95F design inlet water temperature
				SimpleTowerInlet( TowerNum ).AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
				SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).FreeConvTowerUA = UA;
				}
			} else {
				SimpleTower( TowerNum ).FreeConvTowerUA = 0.0;
			}
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
			}
		}

		// calibrate variable speed tower model based on user input by finding calibration water flow rate ratio that
		// yields an approach temperature that matches user input
		if ( SameString( SimpleTower( TowerNum ).TowerType, "CoolingTower:VariableSpeed" ) ) {

			Twb = SimpleTower( TowerNum ).DesignInletWB;
			Tr = SimpleTower( TowerNum ).DesignRange;
			Ta = SimpleTower( TowerNum ).DesignApproach;

			Par( 1 ) = TowerNum; // Index to cooling tower
			Par( 2 ) = 1.0; // air flow rate ratio
			Par( 3 ) = Twb; // inlet air wet-bulb temperature [C]
			Par( 4 ) = Tr; // tower range temperature [C]
			Par( 5 ) = Ta; // design approach temperature [C]
			Par( 6 ) = 0.0; // Calculation FLAG, 0.0 = calc water flow ratio, 1.0 calc air flow ratio

			//   check range for water flow rate ratio (make sure RegulaFalsi converges)
			MaxWaterFlowRateRatio = 0.5;
			Tapproach = 0.0;
			FlowRateRatioStep = ( VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio - VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio ) / 10.0;
			ModelCalibrated = true;
			ModelWaterFlowRatioMax = VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio * 4.0;
			//   find a flow rate large enough to provide an approach temperature > than the user defined approach
			while ( Tapproach < Ta && MaxWaterFlowRateRatio <= ModelWaterFlowRatioMax ) {
				WaterFlowRateRatio = MaxWaterFlowRateRatio;
				CalcVSTowerApproach( TowerNum, WaterFlowRateRatio, 1.0, Twb, Tr, Tapproach );
				if ( Tapproach < Ta ) {
					MaxWaterFlowRateRatio += FlowRateRatioStep;
				}
				// a water flow rate large enough to provide an approach temperature > than the user defined approach does not exist
				// within the tolerances specified by the user
				if ( ( MaxWaterFlowRateRatio == 0.5 && Tapproach > Ta ) || MaxWaterFlowRateRatio >= ModelWaterFlowRatioMax ) {
					ModelCalibrated = false;
					break;
				}
			}

			if ( ModelCalibrated ) {
				SolveRegulaFalsi( Acc, MaxIte, SolFla, WaterFlowRatio, SimpleTowerApproachResidual, constant_pointfive, MaxWaterFlowRateRatio, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower water flow ratio during calibration" );
					ShowContinueError( "Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio for this variable-speed cooling tower." );
					ShowFatalError( "Cooling tower calibration failed for tower " + SimpleTower( TowerNum ).Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for cooling tower water flow rate ratio calibration." );
					ShowContinueError( "Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio for this variable-speed cooling tower." );
					ShowFatalError( "Cooling tower calibration failed for tower " + SimpleTower( TowerNum ).Name + '.' );
				}
			} else {
				gio::write( OutputChar2, OutputFormat2 ) << WaterFlowRateRatio;
				gio::write( OutputChar, OutputFormat ) << Tapproach;
				ShowSevereError( "Bad starting values for cooling tower water flow rate ratio calibration." );
				ShowContinueError( "Design inlet air wet-bulb or range temperature must be modified to achieve the design approach" );
				ShowContinueError( "A water flow rate ratio of " + OutputChar2 + " was calculated to yield an approach temperature of " + OutputChar + '.' );
				ShowFatalError( "Cooling tower calibration failed for tower " + SimpleTower( TowerNum ).Name + '.' );
			}

			SimpleTower( TowerNum ).CalibratedWaterFlowRate = SimpleTower( TowerNum ).DesignWaterFlowRate / WaterFlowRatio;

			if ( WaterFlowRatio < VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio || WaterFlowRatio > VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio ) {
				gio::write( OutputChar2, OutputFormat2 ) << WaterFlowRatio;
				gio::write( OutputCharLo, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio;
				gio::write( OutputCharHi, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio;
				ShowWarningError( "CoolingTower:VariableSpeed, \"" + SimpleTower( TowerNum ).Name + "\" the calibrated water flow rate ratio is determined to be " + OutputChar2 + ". This is outside the valid range of " + OutputCharLo + " to " + OutputCharHi + '.' );
			}

			rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, ( Twb + Ta + Tr ), PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
			Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, ( Twb + Ta + Tr ), PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

			SimpleTower( TowerNum ).TowerNominalCapacity = ( ( rho * tmpDesignWaterFlowRate ) * Cp * Tr );
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Initial Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
			}
			SimpleTower( TowerNum ).FreeConvAirFlowRate = SimpleTower( TowerNum ).MinimumVSAirFlowFrac * SimpleTower( TowerNum ).HighSpeedAirFlowRate;

			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Air Flow Rate in free convection regime [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Initial Air Flow Rate in free convection regime [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
			}
			SimpleTower( TowerNum ).TowerFreeConvNomCap = SimpleTower( TowerNum ).TowerNominalCapacity * SimpleTower( TowerNum ).FreeConvectionCapacityFraction;

			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Tower capacity in free convection regime at design conditions [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
					"Initial Tower capacity in free convection regime at design conditions [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
			}
		}
		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = SimpleTower( TowerNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, SimpleTower( TowerNum ).TowerType );
			PreDefTableEntry( pdchMechNomCap, equipName, SimpleTower( TowerNum ).TowerNominalCapacity );
		}

		// input error checking
		ErrorsFound = false;
		if ( PlantFinalSizesOkayToReport ) {
			if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_SingleSpeed ) {
				if ( SimpleTower( TowerNum ).DesignWaterFlowRate > 0.0 ) {
					if ( SimpleTower( TowerNum ).FreeConvAirFlowRate >= SimpleTower( TowerNum ).HighSpeedAirFlowRate ) {
						ShowSevereError( cCoolingTower_SingleSpeed + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow rate must be less than the design air flow rate." );
						ErrorsFound = true;
					}
					if ( SimpleTower( TowerNum ).FreeConvTowerUA >= SimpleTower( TowerNum ).HighSpeedTowerUA ) {
						ShowSevereError( cCoolingTower_SingleSpeed + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection UA must be less than the design tower UA." );
						ErrorsFound = true;
					}
				}
			}

			if ( SimpleTower( TowerNum ).TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( SimpleTower( TowerNum ).DesignWaterFlowRate > 0.0 ) {
					if ( SimpleTower( TowerNum ).HighSpeedAirFlowRate <= SimpleTower( TowerNum ).LowSpeedAirFlowRate ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + SimpleTower( TowerNum ).Name + "\". Low speed air flow rate must be less than the high speed air flow rate." );
						ErrorsFound = true;
					}
					if ( SimpleTower( TowerNum ).LowSpeedAirFlowRate <= SimpleTower( TowerNum ).FreeConvAirFlowRate ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + SimpleTower( TowerNum ).Name + "\". Free convection air flow rate must be less than the low speed air flow rate." );
						ErrorsFound = true;
					}
					if ( SimpleTower( TowerNum ).HighSpeedTowerUA <= SimpleTower( TowerNum ).LowSpeedTowerUA ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + SimpleTower( TowerNum ).Name + "\". Tower UA at low fan speed must be less than the tower UA at high fan speed." );
						ErrorsFound = true;
					}
					if ( SimpleTower( TowerNum ).LowSpeedTowerUA <= SimpleTower( TowerNum ).FreeConvTowerUA ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + SimpleTower( TowerNum ).Name + "\". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed." );
						ErrorsFound = true;
					}
				}
			}
			if ( ErrorsFound ) {
				ShowFatalError( "InitTower: Program terminated due to previous condition(s)." );
			}
		}

	}

	void
	SizeVSMerkelTower( int const TowerNum )
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
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using General::SolveRegulaFalsi;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations
		Real64 const Acc( 0.0001 ); // Accuracy of result
		static std::string const RoutineName( "SizeTower" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizCondNum; // Plant Sizing index for condenser loop
		int SolFla; // Flag of solver
		Real64 tmpNomTowerCap;
		Real64 tmpDesignWaterFlowRate;
		Real64 tmpTowerFreeConvNomCap;
		Real64 tmpDesignAirFlowRate;
		Real64 tmpHighSpeedFanPower;
		Real64 tmpFreeConvAirFlowRate;

		Array1D< Real64 > Par( 6 ); // Parameter array need for RegulaFalsi routine
		Real64 UA0; // Lower bound for UA [W/C]
		Real64 UA1; // Upper bound for UA [W/C]
		Real64 DesTowerLoad; // Design tower load [W]
		Real64 Cp( 0 ); // local specific heat for fluid
		Real64 rho( 0 ); // local density for fluid
		Real64 UA; // Calculated UA value
		Real64 OutWaterTemp;

		// Find the appropriate Plant Sizing object
		PltSizCondNum = PlantLoop( SimpleTower( TowerNum ).LoopNum ).PlantSizNum;

		tmpNomTowerCap = SimpleTower( TowerNum ).TowerNominalCapacity;
		tmpDesignWaterFlowRate = SimpleTower( TowerNum ).DesignWaterFlowRate;

		tmpTowerFreeConvNomCap = SimpleTower( TowerNum ).TowerFreeConvNomCap;
		tmpDesignAirFlowRate = SimpleTower( TowerNum ).HighSpeedAirFlowRate;
		tmpHighSpeedFanPower = SimpleTower( TowerNum ).HighSpeedFanPower;
		tmpFreeConvAirFlowRate = SimpleTower( TowerNum ).FreeConvAirFlowRate;

		if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_NominalCapacity ) {

			if ( SimpleTower( TowerNum ).TowerNominalCapacityWasAutoSized ) {
				// get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
						DesTowerLoad = rho * Cp * PlantSizData( PltSizCondNum ).DesVolFlowRate * PlantSizData( PltSizCondNum ).DeltaT * SimpleTower( TowerNum ).SizFac;
						tmpNomTowerCap = DesTowerLoad / SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
						if ( PlantFirstSizesOkayToFinalize ) {
							SimpleTower( TowerNum ).TowerNominalCapacity = tmpNomTowerCap;
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name, "Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
						}
					} else {
						tmpNomTowerCap = 0.0;
						if ( PlantFirstSizesOkayToFinalize ) {
							SimpleTower( TowerNum ).TowerNominalCapacity = tmpNomTowerCap;
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name, "Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
						}
					}

				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						ShowSevereError( "Autosizing error for cooling tower object = " + SimpleTower( TowerNum ).Name );
						ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
					}
				}

			}

			if ( SimpleTower( TowerNum ).TowerFreeConvNomCapWasAutoSized ) {
				tmpTowerFreeConvNomCap = tmpNomTowerCap * SimpleTower( TowerNum ).TowerFreeConvNomCapSizingFactor;
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
					}
				}
			}

			if ( SimpleTower( TowerNum ).DesignWaterFlowRateWasAutoSized ) {
				// for nominal cap input method, get design water flow rate from nominal cap and scalable sizing factor
				tmpDesignWaterFlowRate = tmpNomTowerCap * SimpleTower( TowerNum ).DesignWaterFlowPerUnitNomCap;
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).DesignWaterFlowRate = tmpDesignWaterFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Design Water Flow Rate [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial Design Water Flow Rate [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
					}
				}
			}

			RegisterPlantCompDesignFlow( SimpleTower( TowerNum ).WaterInletNodeNum, tmpDesignWaterFlowRate );

			if ( SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized ) {
				if ( SimpleTower( TowerNum ).DefaultedDesignAirFlowScalingFactor ) {
					tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower( TowerNum ).DesignAirFlowPerUnitNomCap * ( 101325.0 / StdBaroPress );
				} else {
					tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower( TowerNum ).DesignAirFlowPerUnitNomCap;
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).HighSpeedAirFlowRate = tmpDesignAirFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
					}
				}
			}

			if ( SimpleTower( TowerNum ).FreeConvAirFlowRate == AutoSize ) {
				tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower( TowerNum ).FreeConvAirFlowRateSizingFactor;
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
					}
				}
			}

			// now calcuate UA values from nominal capacities and flow rates
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( PltSizCondNum > 0 ) { // user has a plant sizing object
					Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
					SimpleTowerInlet( TowerNum ).WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
				} else { // probably no plant sizing object
					Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
					SimpleTowerInlet( TowerNum ).WaterTemp = 35.0; // design condition
				}
				rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

				// full speed fan tower UA
				Par( 1 ) = tmpNomTowerCap * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				Par( 2 ) = double( TowerNum );
				Par( 3 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 4 ) = tmpDesignAirFlowRate; // design air volume flow rate
				Par( 5 ) = Cp;
				UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
				UA1 = Par( 1 ); // Assume deltaT = 1K

				SimpleTowerInlet( TowerNum ).AirTemp = 35.0;
				SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6;
				SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
				SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "calculating cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				}
				SimpleTower( TowerNum ).HighSpeedTowerUA = UA;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
				}
				// free convection tower UA
				Par( 1 ) = tmpTowerFreeConvNomCap * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				Par( 2 ) = double( TowerNum );
				Par( 3 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 4 ) = tmpFreeConvAirFlowRate; // design air volume flow rate
				Par( 5 ) = Cp;
				UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
				UA0 = max( UA0, 1.0 ); // limit to 1.0
				UA1 = Par( 1 ); // Assume deltaT = 1K

				SimpleTowerInlet( TowerNum ).AirTemp = 35.0;
				SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6;
				SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
				SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower free convection UA" );
					ShowFatalError( "calculating cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for free convection tower " + SimpleTower( TowerNum ).Name );
				}
				SimpleTower( TowerNum ).FreeConvTowerUA = UA;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
				}
			}

		} else if ( SimpleTower( TowerNum ).PerformanceInputMethod_Num == PIM_UFactor ) {
			//UA input method

			if ( SimpleTower( TowerNum ).DesignWaterFlowRateWasAutoSized ) { // get from plant sizing
				// UA input method using plant sizing for flow rate, whereas Nominal capacity method uses scalable sizing factor per cap
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						tmpDesignWaterFlowRate = PlantSizData( PltSizCondNum ).DesVolFlowRate * SimpleTower( TowerNum ).SizFac;
						if ( PlantFirstSizesOkayToFinalize ) {
							SimpleTower( TowerNum ).DesignWaterFlowRate = tmpDesignWaterFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
									"Design Water Flow Rate [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
									"Initial Design Water Flow Rate [m3/s]", SimpleTower( TowerNum ).DesignWaterFlowRate );
							}
						}
					} else {
						tmpDesignWaterFlowRate = 0.0;

					}

				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						ShowSevereError( "Autosizing error for cooling tower object = " + SimpleTower( TowerNum ).Name );
						ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
					}
				}
			}
			RegisterPlantCompDesignFlow( SimpleTower( TowerNum ).WaterInletNodeNum, tmpDesignWaterFlowRate );

			if ( SimpleTower( TowerNum ).HighSpeedTowerUAWasAutoSized ) {
				// get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
						DesTowerLoad = rho * Cp * PlantSizData( PltSizCondNum ).DesVolFlowRate * PlantSizData( PltSizCondNum ).DeltaT * SimpleTower( TowerNum ).SizFac;
						tmpNomTowerCap = DesTowerLoad / SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
						if ( PlantFirstSizesOkayToFinalize ) {
							SimpleTower( TowerNum ).TowerNominalCapacity = tmpNomTowerCap;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
									"Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
									"Initial Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
							}
						}
					} else {
						tmpNomTowerCap = 0.0;
						if ( PlantFirstSizesOkayToFinalize ) {
							SimpleTower( TowerNum ).TowerNominalCapacity = tmpNomTowerCap;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
									"Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
									"Initial Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
							}
						}
					}
				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						ShowSevereError( "Autosizing error for cooling tower object = " + SimpleTower( TowerNum ).Name );
						ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
					}
				}
				if ( SimpleTower( TowerNum ).TowerFreeConvNomCapWasAutoSized ) {
					tmpTowerFreeConvNomCap = tmpNomTowerCap * SimpleTower( TowerNum ).TowerFreeConvNomCapSizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Initial Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
						}
					}
				}
				if ( SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized ) {
					if ( SimpleTower( TowerNum ).DefaultedDesignAirFlowScalingFactor ) {
						tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower( TowerNum ).DesignAirFlowPerUnitNomCap * ( 101325.0 / StdBaroPress );
					} else {
						tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower( TowerNum ).DesignAirFlowPerUnitNomCap;
					}
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).HighSpeedAirFlowRate = tmpDesignAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Initial Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
						}
					}
				}
				if ( SimpleTower( TowerNum ).FreeConvAirFlowRateWasAutoSized ) {
					tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower( TowerNum ).FreeConvAirFlowRateSizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Initial Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
						}
					}
				}
				// now calcuate UA values from nominal capacities and flow rates
				if ( PlantFirstSizesOkayToFinalize ) {
					rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
					// full speed fan tower UA
					Par( 1 ) = tmpNomTowerCap * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
					Par( 2 ) = double( TowerNum );
					Par( 3 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
					Par( 4 ) = tmpDesignAirFlowRate; // design air volume flow rate
					Par( 5 ) = Cp;
					UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
					UA1 = Par( 1 ); // Assume deltaT = 1K
					SimpleTowerInlet( TowerNum ).WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					SimpleTowerInlet( TowerNum ).AirTemp = 35.0;
					SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6;
					SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
					SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
					SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
					if ( SolFla == -1 ) {
						ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
						ShowFatalError( "calculating cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
					} else if ( SolFla == -2 ) {
						ShowSevereError( "Bad starting values for UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
					}
					SimpleTower( TowerNum ).HighSpeedTowerUA = UA;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", SimpleTower( TowerNum ).HighSpeedTowerUA );
					}
					// free convection tower UA
					Par( 1 ) = tmpTowerFreeConvNomCap * SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
					Par( 2 ) = double( TowerNum );
					Par( 3 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
					Par( 4 ) = tmpFreeConvAirFlowRate; // design air volume flow rate
					Par( 5 ) = Cp;
					UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
					UA1 = Par( 1 ); // Assume deltaT = 1K
					SimpleTowerInlet( TowerNum ).WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					SimpleTowerInlet( TowerNum ).AirTemp = 35.0;
					SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6;
					SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
					SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
					SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par );
					if ( SolFla == -1 ) {
						ShowSevereError( "Iteration limit exceeded in calculating tower free convection UA" );
						ShowFatalError( "calculating cooling tower UA failed for tower " + SimpleTower( TowerNum ).Name );
					} else if ( SolFla == -2 ) {
						ShowSevereError( "Bad starting values for UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for free convection tower " + SimpleTower( TowerNum ).Name );
					}
					SimpleTower( TowerNum ).LowSpeedTowerUA = UA;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
					}
				}

			} else { //full speed UA given

				if ( SimpleTower( TowerNum ).FreeConvTowerUAWasAutoSized ) { // determine from scalable sizing factor
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).FreeConvTowerUA = SimpleTower( TowerNum ).HighSpeedTowerUA * SimpleTower( TowerNum ).FreeConvTowerUASizingFactor;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", SimpleTower( TowerNum ).FreeConvTowerUA );
						}
					}
				}

				if ( SimpleTower( TowerNum ).HighSpeedAirFlowRateWasAutoSized ) { // given UA but not air flow rate
					// need an air flow rate to find capacity from UA but flow rate is scaled off capacity
					// get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
					if ( PltSizCondNum > 0 ) {
						if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
							rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
							Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
							DesTowerLoad = rho * Cp * PlantSizData( PltSizCondNum ).DesVolFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
							tmpNomTowerCap = DesTowerLoad / SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
							if ( PlantFirstSizesOkayToFinalize ) {
								SimpleTower( TowerNum ).TowerNominalCapacity = tmpNomTowerCap;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
										"Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
								}
								if ( PlantFirstSizesOkayToReport ) {
									ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
										"Initial Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
								}
							}
						} else {
							tmpNomTowerCap = rho = Cp = 0.0; // rho and Cp added: Used below
							if ( PlantFirstSizesOkayToFinalize ) {
								SimpleTower( TowerNum ).TowerNominalCapacity = tmpNomTowerCap;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
										"Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
								}
								if ( PlantFirstSizesOkayToReport ) {
									ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
										"Initial Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
								}
							}
						}

					} else {
						tmpNomTowerCap = 0.0; // Suppress uninitialized warnings
						if ( PlantFirstSizesOkayToFinalize ) {
							ShowSevereError( "Autosizing error for cooling tower object = " + SimpleTower( TowerNum ).Name );
							ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
						}
					}

					if ( SimpleTower( TowerNum ).DefaultedDesignAirFlowScalingFactor ) {
						tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower( TowerNum ).DesignAirFlowPerUnitNomCap * ( 101325.0 / StdBaroPress );
					} else {
						tmpDesignAirFlowRate = tmpNomTowerCap * SimpleTower( TowerNum ).DesignAirFlowPerUnitNomCap;
					}
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).HighSpeedAirFlowRate = tmpDesignAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Initial Design Air Flow Rate [m3/s]", SimpleTower( TowerNum ).HighSpeedAirFlowRate );
						}
					}

				} else { // UA and Air flow rate given, so find Nominal Cap from running model

					rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

					SimpleTowerInlet( TowerNum ).WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					SimpleTowerInlet( TowerNum ).AirTemp = 35.0;
					SimpleTowerInlet( TowerNum ).AirWetBulb = 25.6;
					SimpleTowerInlet( TowerNum ).AirPress = StdBaroPress;
					SimpleTowerInlet( TowerNum ).AirHumRat = PsyWFnTdbTwbPb( SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirWetBulb, SimpleTowerInlet( TowerNum ).AirPress );
					SimSimpleTower( TowerNum, rho * tmpDesignWaterFlowRate, SimpleTower( TowerNum ).HighSpeedAirFlowRate, SimpleTower( TowerNum ).HighSpeedTowerUA, OutWaterTemp );
					tmpNomTowerCap = Cp * rho * tmpDesignWaterFlowRate * ( SimpleTowerInlet( TowerNum ).WaterTemp - OutWaterTemp );
					tmpNomTowerCap /= SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).TowerNominalCapacity = tmpNomTowerCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Initial Nominal Capacity [W]", SimpleTower( TowerNum ).TowerNominalCapacity );
						}
					}

				} // both UA and air flow rate given

				if ( SimpleTower( TowerNum ).FreeConvAirFlowRateWasAutoSized ) {
					tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower( TowerNum ).FreeConvAirFlowRateSizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) {
						SimpleTower( TowerNum ).FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
								"Initial Free Convection Regime Air Flow Rate [m3/s]", SimpleTower( TowerNum ).FreeConvAirFlowRate );
						}
					}
				}

				SimSimpleTower( TowerNum, rho * tmpDesignWaterFlowRate, tmpFreeConvAirFlowRate, SimpleTower( TowerNum ).FreeConvTowerUA, OutWaterTemp );
				tmpTowerFreeConvNomCap = Cp * rho * tmpDesignWaterFlowRate * ( SimpleTowerInlet( TowerNum ).WaterTemp - OutWaterTemp );
				tmpTowerFreeConvNomCap /= SimpleTower( TowerNum ).HeatRejectCapNomCapSizingRatio;
				if ( PlantFirstSizesOkayToFinalize ) {
					SimpleTower( TowerNum ).TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
							"Initial Free Convection Nominal Capacity [W]", SimpleTower( TowerNum ).TowerFreeConvNomCap );
					}
				}
			}
		}

		if ( SimpleTower( TowerNum ).HighSpeedFanPowerWasAutoSized ) {
			tmpHighSpeedFanPower = tmpNomTowerCap * SimpleTower( TowerNum ).DesignFanPowerPerUnitNomCap;
			if ( PlantFirstSizesOkayToFinalize ) {
				SimpleTower( TowerNum ).HighSpeedFanPower = tmpHighSpeedFanPower;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Design Fan Power [W]", SimpleTower( TowerNum ).HighSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( SimpleTower( TowerNum ).TowerType, SimpleTower( TowerNum ).Name,
						"Initial Design Fan Power [W]", SimpleTower( TowerNum ).HighSpeedFanPower );
				}
			}
		}

	}

	// End Initialization Section for the CondenserLoopTowers Module
	//******************************************************************************

	// Beginning of the CondenserLoopTowers Module Simulation Subroutines
	// *****************************************************************************

	void
	CalcSingleSpeedTower( int & TowerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       T Hong, Aug. 2008. Added fluid bypass for single speed cooling tower
		//                      The OutletWaterTemp from SimSimpleTower can be lower than 0 degreeC
		//                      which may not be allowed in practice if water is the tower fluid.
		//                      Chandan Sharma, FSEC, February 2010, Added basin heater
		//                      A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
		//       RE-ENGINEERED  Jan 2001, Richard Raustad

		// PURPOSE OF THIS SUBROUTINE:
		// To simulate the operation of a single-speed fan cooling tower.

		// METHODOLOGY EMPLOYED:
		// The cooling tower is modeled using effectiveness-NTU relationships for
		// counterflow heat exchangers based on Merkel's theory.
		// The subroutine calculates the period of time required to meet a
		// leaving water temperature setpoint. It assumes that part-load
		// operation represents a linear interpolation of two steady-state regimes.
		// Cyclic losses are neglected. The period of time required to meet the
		// leaving water temperature setpoint is used to determine the required
		// fan power and energy. Free convection regime is also modeled. This
		// occures when the pump is operating and the fan is off. If free convection
		// regime cooling is all that is required for a given time step, the leaving
		// water temperature is allowed to fall below the leaving water temperature
		// setpoint (free cooling). At times when the cooling tower fan is required,
		// the leaving water temperature is at or above the setpoint.
		// A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
		// or schedule, of the cooling tower. If the tower is OFF, outlet water
		// temperature and flow rate are passed through the model from inlet node to
		// outlet node without intervention (with the exception of free convection
		// where water temperature is allowed to float below the outlet water set
		// point). Reports are also updated with fan power and energy being zero.
		// When the RunFlag indicates an ON condition for the cooling tower, the
		// mass flow rate and water temperature are read from the inlet node of the
		// cooling tower (water-side). The outdoor air wet-bulb temperature is used
		// as the entering condition to the cooling tower (air-side). Input deck
		// parameters are read for the free convection regime (pump ON and fan OFF)
		// and a leaving water temperature is calculated. If the leaving water temperature
		// is at or below the setpoint, the calculated leaving water temperature is
		// placed on the outlet node and no fan power is used. If the calculated leaving
		// water temperature is above the setpoint, the cooling tower fan is turned on
		// and design parameters are used to again calculate the leaving water temperature.
		// If the calculated leaving water temperature is below the setpoint, a fan
		// run-time fraction is calculated and used to determine fan power. The leaving
		// water temperature setpoint is placed on the outlet node. If the calculated
		// leaving water temperature is at or above the setpoint, the calculated
		// leaving water temperature is placed on the outlet node and the fan runs at
		// full power. Water mass flow rate is passed from inlet node to outlet node
		// with no intervention.
		// If a tower has multiple cells, the specified inputs of or the autosized capacity
		//  and air/water flow rates are for the entire tower. The number of cells to operate
		//  is first determined based on the user entered minimal and maximal water flow fractions
		//  per cell. If the loads are not met, more cells (if available) will operate to meet
		//  the loads. Inside each cell, the capacity controls still apply. Each cell operates
		//  in the same way.

		// REFERENCES:
		// ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSingleSpeedTower" );
		int const MaxIteration( 100 ); // Maximum fluid bypass iteration calculations
		static std::string const MaxItChar( "100" );
		Real64 const BypassFractionThreshold( 0.01 ); // Threshold to stop bypass iteration
		Real64 const OWTLowerLimit( 0.0 ); // The limit of tower exit fluid temperature used in the fluid bypass
		//  calculation to avoid fluid freezing. For water, it is 0 degreeC,
		//  for glycols, it can be much lower. The fluid type is stored at the loop.
		//  Current choices are Water and Steam, needs to expand for glycols

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirFlowRate;
		Real64 UAdesign; // UA value at design conditions (entered by user or calculated)
		Real64 OutletWaterTempOFF;
		Real64 FanModeFrac;
		Real64 DesignWaterFlowRate;
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

		//Added variables for multicell
		Real64 WaterMassFlowRatePerCellMin;
		Real64 WaterMassFlowRatePerCellMax;
		static int NumCellMin( 0 );
		static int NumCellMax( 0 );
		static int NumCellOn( 0 );
		Real64 WaterMassFlowRatePerCell;
		bool IncrNumCellFlag; // determine if yes or no we increase the number of cells

		int LoopNum;
		int LoopSideNum;

		//set inlet and outlet nodes
		WaterInletNode = SimpleTower( TowerNum ).WaterInletNodeNum;
		WaterOutletNode = SimpleTower( TowerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		CTFanPower = 0.0;
		OutletWaterTemp = Node( WaterInletNode ).Temp;
		LoopNum = SimpleTower( TowerNum ).LoopNum;
		LoopSideNum = SimpleTower( TowerNum ).LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( SimpleTower( TowerNum ).SetpointIsOnOutlet ) {
				TempSetPoint = Node( WaterOutletNode ).TempSetPoint;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( SimpleTower( TowerNum ).SetpointIsOnOutlet ) {
				TempSetPoint = Node( WaterOutletNode ).TempSetPointHi;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
			}
		}}

		// Added for fluid bypass. First assume no fluid bypass
		BypassFlag = 0;
		BypassFraction = 0.0;
		BypassFraction2 = 0.0;
		SimpleTower( TowerNum ).BypassFraction = 0.0;
		CapacityControl = SimpleTower( TowerNum ).CapacityControl;

		// Added for multi-cell. Determine the number of cells operating
		if ( SimpleTower( TowerNum ).DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MinFracFlowRate / SimpleTower( TowerNum ).NumCell;
			WaterMassFlowRatePerCellMax = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MaxFracFlowRate / SimpleTower( TowerNum ).NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
			NumCellMax = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( SimpleTower( TowerNum ).CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		SimpleTower( TowerNum ).NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;

		// Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.

		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( WaterMassFlowRate <= MassFlowTolerance ) {
			// for multiple cells, we assume that it's a commun bassin
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			return;
		}

		IncrNumCellFlag = true; // set value to true to enter in the loop

		while ( IncrNumCellFlag ) {
			IncrNumCellFlag = false;

			//   Initialize local variables to the free convection design values
			UAdesign = SimpleTower( TowerNum ).FreeConvTowerUA / SimpleTower( TowerNum ).NumCell;
			AirFlowRate = SimpleTower( TowerNum ).FreeConvAirFlowRate / SimpleTower( TowerNum ).NumCell;
			DesignWaterFlowRate = SimpleTower( TowerNum ).DesignWaterFlowRate;
			OutletWaterTempOFF = Node( WaterInletNode ).Temp;
			OutletWaterTemp = OutletWaterTempOFF;
			FanModeFrac = 0.0;

			SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTempOFF );

			//   Assume Setpoint was met using free convection regime (pump ON and fan OFF)
			CTFanPower = 0.0;
			OutletWaterTemp = OutletWaterTempOFF;

			if ( OutletWaterTempOFF > TempSetPoint ) {
				//     Setpoint was not met (or free conv. not used), turn on cooling tower fan
				UAdesign = SimpleTower( TowerNum ).HighSpeedTowerUA / SimpleTower( TowerNum ).NumCell;
				AirFlowRate = SimpleTower( TowerNum ).HighSpeedAirFlowRate / SimpleTower( TowerNum ).NumCell;

				// The fan power is for all cells operating
				FanPowerOn = SimpleTower( TowerNum ).HighSpeedFanPower * NumCellOn / SimpleTower( TowerNum ).NumCell;

				SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTemp );

				if ( OutletWaterTemp <= TempSetPoint ) {
					if ( CapacityControl == CapacityControl_FanCycling || OutletWaterTemp <= OWTLowerLimit ) {
						//           Setpoint was met with pump ON and fan ON, calculate run-time fraction
						FanModeFrac = ( TempSetPoint - OutletWaterTempOFF ) / ( OutletWaterTemp - OutletWaterTempOFF );
						CTFanPower = FanModeFrac * FanPowerOn;
						OutletWaterTemp = TempSetPoint;
					} else {
						//FluidBypass, fan runs at full speed for the entire time step
						FanModeFrac = 1.0;
						CTFanPower = FanPowerOn;
						BypassFlag = 1;
					}
				} else {
					//         Setpoint was not met, cooling tower ran at full capacity
					FanModeFrac = 1.0;
					CTFanPower = FanPowerOn;
					// if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
					if ( NumCellOn < SimpleTower( TowerNum ).NumCell && ( WaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) {
						++NumCellOn;
						WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;
						IncrNumCellFlag = true;
					}
				}
			} else if ( OutletWaterTempOFF < TempSetPoint ) {
				// Need to bypass in free convection cooling mode if bypass is allowed
				if ( CapacityControl == CapacityControl_FluidBypass ) {
					if ( OutletWaterTempOFF > OWTLowerLimit ) {
						BypassFlag = 1;
					}
				}
			}
		}

		// Calculate bypass fraction since OWTLowerLimit < OutletWaterTemp < TempSetPoint.
		// The iteraction ends when the numer of iteraction exceeds the limit or the difference
		//  between the new and old bypass fractions is less than the threshold.
		if ( BypassFlag == 1 ) {
			//Inlet water temperature lower than setpoint, assume 100% bypass, tower fan off
			if ( InletWaterTemp <= TempSetPoint ) {
				CTFanPower = 0.0;
				BypassFraction = 1.0;
				SimpleTower( TowerNum ).BypassFraction = 1.0;
				OutletWaterTemp = InletWaterTemp;
			} else {
				if ( std::abs( InletWaterTemp - OutletWaterTemp ) <= 0.01 ) {
					// Outlet temp is close enough to inlet temp, assume 100% bypass, tower fan off
					BypassFraction = 1.0;
					SimpleTower( TowerNum ).BypassFraction = 1.0;
					CTFanPower = 0.0;
				} else {
					BypassFraction = ( TempSetPoint - OutletWaterTemp ) / ( InletWaterTemp - OutletWaterTemp );
					if ( BypassFraction > 1.0 || BypassFraction < 0.0 ) {
						// Bypass cannot meet setpoint, assume no bypass
						BypassFlag = 0;
						BypassFraction = 0.0;
						SimpleTower( TowerNum ).BypassFraction = 0.0;
					} else {
						NumIteration = 0;
						BypassFractionPrev = BypassFraction;
						OutletWaterTempPrev = OutletWaterTemp;
						while ( NumIteration < MaxIteration ) {
							++NumIteration;
							// need to iterate for the new OutletWaterTemp while bypassing tower water
							SimSimpleTower( TowerNum, WaterMassFlowRatePerCell * ( 1.0 - BypassFraction ), AirFlowRate, UAdesign, OutletWaterTemp );
							// Calc new BypassFraction based on the new OutletWaterTemp
							if ( std::abs( OutletWaterTemp - OWTLowerLimit ) <= 0.01 ) {
								BypassFraction2 = BypassFraction;
								break;
							} else if ( OutletWaterTemp < OWTLowerLimit ) {
								// Set OutletWaterTemp = OWTLowerLimit, and use linear interpolation to calculate the bypassFraction
								BypassFraction2 = BypassFractionPrev - ( BypassFractionPrev - BypassFraction ) * ( OutletWaterTempPrev - OWTLowerLimit ) / ( OutletWaterTempPrev - OutletWaterTemp );
								SimSimpleTower( TowerNum, WaterMassFlowRatePerCell * ( 1.0 - BypassFraction2 ), AirFlowRate, UAdesign, OutletWaterTemp );
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
							ShowWarningError( "Cooling tower fluid bypass iteration exceeds maximum limit of " + MaxItChar + " for " + SimpleTower( TowerNum ).Name );
						}
						SimpleTower( TowerNum ).BypassFraction = BypassFraction2;
						// may not meet TempSetPoint due to limit of tower outlet temp to OWTLowerLimit
						OutletWaterTemp = ( 1.0 - BypassFraction2 ) * OutletWaterTemp + BypassFraction2 * InletWaterTemp;
					}
				}
			}
		}

		//output the fraction of the time step the fan is ON
		FanCyclingRatio = FanModeFrac;
		// output the number of cells operating
		SimpleTower( TowerNum ).NumCellOn = NumCellOn;
		//Should this be water inlet node num?????
		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

		Qactual = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );
		AirFlowRateRatio = ( AirFlowRate * SimpleTower( TowerNum ).NumCell ) / SimpleTower( TowerNum ).HighSpeedAirFlowRate;

	}

	void
	CalcTwoSpeedTower( int & TowerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
		//                      A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To simulate the operation of a cooling tower with a two-speed fan.

		// METHODOLOGY EMPLOYED:
		// The cooling tower is modeled using effectiveness-NTU relationships for
		// counterflow heat exchangers based on Merkel's theory.
		// The subroutine calculates the period of time required to meet a
		// leaving water temperature setpoint. It assumes that part-load
		// operation represents a linear interpolation of three steady-state regimes
		// (high-speed fan operation, low-speed fan operation and free convection regime).
		// Cyclic losses are neglected. The period of time required to meet the
		// leaving water temperature setpoint is used to determine the required
		// fan power and energy. Free convection regime is also modeled. This
		// occures when the pump is operating and the fan is off. If free convection
		// regime cooling is all that is required for a given time step, the leaving
		// water temperature is allowed to fall below the leaving water temperature
		// setpoint (free cooling). At times when the cooling tower fan is required,
		// the leaving water temperature is at or above the setpoint.
		// A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
		// or schedule, of the cooling tower. If the tower is OFF, outlet water
		// temperature and flow rate are passed through the model from inlet node to
		// outlet node without intervention (with the exception of free convection
		// where water temperature is allowed to float below the outlet water set
		// point). Reports are also updated with fan power and fan energy being zero.
		// When the RunFlag indicates an ON condition for the cooling tower, the
		// mass flow rate and water temperature are read from the inlet node of the
		// cooling tower (water-side). The outdoor air wet-bulb temperature is used
		// as the entering condition to the cooling tower (air-side). Input deck
		// parameters are read for the free convection regime (pump ON and fan OFF)
		// and a leaving water temperature is calculated. If the leaving water temperature
		// is at or below the setpoint, the calculated leaving water temperature is
		// placed on the outlet node and no fan power is used. If the calculated leaving
		// water temperature is above the setpoint, the cooling tower fan is turned on
		// and parameters for low fan speed are used to again calculate the leaving
		// water temperature. If the calculated leaving water temperature is
		// below the setpoint, a fan run-time fraction (FanModeFrac) is calculated and
		// used to determine fan power. The leaving water temperature setpoint is placed
		// on the outlet node. If the calculated leaving water temperature is at or above
		// the setpoint, the cooling tower fan is turned on 'high speed' and the routine is
		// repeated. If the calculated leaving water temperature is below the setpoint,
		// a fan run-time fraction is calculated for the second stage fan and fan power
		// is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
		// If the calculated leaving water temperature is above the leaving water temp.
		// setpoint, the calculated leaving water temperature is placed on the outlet
		// node and the fan runs at full power (High Speed Fan Power). Water mass flow
		// rate is passed from inlet node to outlet node with no intervention.
		// If a tower has multiple cells, the specified inputs of or the autosized capacity
		//  and air/water flow rates are for the entire tower. The number of cells to operate
		//  is first determined based on the user entered minimal and maximal water flow fractions
		//  per cell. If the loads are not met, more cells (if available) will operate to meet
		//  the loads. Each cell operates in same way - same fan speed etc.
		// REFERENCES:
		// ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcTwoSpeedTower" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirFlowRate;
		Real64 UAdesign; // UA value at design conditions (entered by user) [W/C]
		Real64 OutletWaterTempOFF;
		Real64 OutletWaterTemp1stStage;
		Real64 OutletWaterTemp2ndStage;
		Real64 FanModeFrac;
		Real64 DesignWaterFlowRate;
		Real64 FanPowerLow;
		Real64 FanPowerHigh;
		Real64 CpWater;
		Real64 TempSetPoint;

		int LoopNum;
		int LoopSideNum;

		static int SpeedSel( 0 );

		//Added variables for multicell
		Real64 WaterMassFlowRatePerCellMin;
		Real64 WaterMassFlowRatePerCellMax;
		static int NumCellMin( 0 );
		static int NumCellMax( 0 );
		static int NumCellOn( 0 );
		Real64 WaterMassFlowRatePerCell;
		bool IncrNumCellFlag; // determine if yes or no we increase the number of cells

		//set inlet and outlet nodes

		WaterInletNode = SimpleTower( TowerNum ).WaterInletNodeNum;
		WaterOutletNode = SimpleTower( TowerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		CTFanPower = 0.0;
		OutletWaterTemp = Node( WaterInletNode ).Temp;
		LoopNum = SimpleTower( TowerNum ).LoopNum;
		LoopSideNum = SimpleTower( TowerNum ).LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( SimpleTower( TowerNum ).SetpointIsOnOutlet ) {
				TempSetPoint = Node( WaterOutletNode ).TempSetPoint;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( SimpleTower( TowerNum ).SetpointIsOnOutlet ) {
				TempSetPoint = Node( WaterOutletNode ).TempSetPointHi;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
			}
		}}

		// Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) return;
		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( WaterMassFlowRate <= MassFlowTolerance ) {
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			return;
		}

		// Added for multi-cell. Determine the number of cells operating
		if ( SimpleTower( TowerNum ).DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MinFracFlowRate / SimpleTower( TowerNum ).NumCell;
			WaterMassFlowRatePerCellMax = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MaxFracFlowRate / SimpleTower( TowerNum ).NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
			NumCellMax = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( SimpleTower( TowerNum ).CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		SimpleTower( TowerNum ).NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;

		IncrNumCellFlag = true;

		while ( IncrNumCellFlag ) {
			IncrNumCellFlag = false;

			//set local variable for tower
			UAdesign = SimpleTower( TowerNum ).FreeConvTowerUA / SimpleTower( TowerNum ).NumCell; // where is NumCellOn?
			AirFlowRate = SimpleTower( TowerNum ).FreeConvAirFlowRate / SimpleTower( TowerNum ).NumCell;
			DesignWaterFlowRate = SimpleTower( TowerNum ).DesignWaterFlowRate; // ??useless subroutine variable??
			OutletWaterTempOFF = Node( WaterInletNode ).Temp;
			WaterMassFlowRate = Node( WaterInletNode ).MassFlowRate;
			OutletWaterTemp1stStage = OutletWaterTemp;
			OutletWaterTemp2ndStage = OutletWaterTemp;
			FanModeFrac = 0.0;

			SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTempOFF );

			//     Setpoint was met using free convection regime (pump ON and fan OFF)
			CTFanPower = 0.0;
			OutletWaterTemp = OutletWaterTempOFF;
			SpeedSel = 0;

			if ( OutletWaterTempOFF > TempSetPoint ) {
				//     Setpoint was not met (or free conv. not used),turn on cooling tower 1st stage fan
				UAdesign = SimpleTower( TowerNum ).LowSpeedTowerUA / SimpleTower( TowerNum ).NumCell;
				AirFlowRate = SimpleTower( TowerNum ).LowSpeedAirFlowRate / SimpleTower( TowerNum ).NumCell;
				FanPowerLow = SimpleTower( TowerNum ).LowSpeedFanPower * NumCellOn / SimpleTower( TowerNum ).NumCell;

				SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTemp1stStage );

				if ( OutletWaterTemp1stStage <= TempSetPoint ) {
					//         Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
					FanModeFrac = ( TempSetPoint - OutletWaterTempOFF ) / ( OutletWaterTemp1stStage - OutletWaterTempOFF );
					CTFanPower = FanModeFrac * FanPowerLow;
					OutletWaterTemp = TempSetPoint;
					Qactual *= FanModeFrac;
					SpeedSel = 1;
				} else {
					//         Setpoint was not met, turn on cooling tower 2nd stage fan
					UAdesign = SimpleTower( TowerNum ).HighSpeedTowerUA / SimpleTower( TowerNum ).NumCell;
					AirFlowRate = SimpleTower( TowerNum ).HighSpeedAirFlowRate / SimpleTower( TowerNum ).NumCell;
					FanPowerHigh = SimpleTower( TowerNum ).HighSpeedFanPower * NumCellOn / SimpleTower( TowerNum ).NumCell;

					SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTemp2ndStage );

					if ( ( OutletWaterTemp2ndStage <= TempSetPoint ) && UAdesign > 0.0 ) {
						//           Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
						FanModeFrac = ( TempSetPoint - OutletWaterTemp1stStage ) / ( OutletWaterTemp2ndStage - OutletWaterTemp1stStage );
						CTFanPower = ( FanModeFrac * FanPowerHigh ) + ( 1.0 - FanModeFrac ) * FanPowerLow;
						OutletWaterTemp = TempSetPoint;
						SpeedSel = 2;
					} else {
						//           Setpoint was not met, cooling tower ran at full capacity
						OutletWaterTemp = OutletWaterTemp2ndStage;
						CTFanPower = FanPowerHigh;
						SpeedSel = 2;
						FanModeFrac = 1.0;
						// if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
						if ( NumCellOn < SimpleTower( TowerNum ).NumCell && ( WaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) {
							++NumCellOn;
							WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;
							IncrNumCellFlag = true;
						}
					}

				}

			}
		}

		//output the fraction of the time step the fan is ON
		FanCyclingRatio = FanModeFrac;
		SimpleTower( TowerNum ).SpeedSelected = SpeedSel;
		SimpleTower( TowerNum ).NumCellOn = NumCellOn;

		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
		Qactual = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );
		AirFlowRateRatio = ( AirFlowRate * SimpleTower( TowerNum ).NumCell ) / SimpleTower( TowerNum ).HighSpeedAirFlowRate;

	}

	void
	CalcMerkelVariableSpeedTower(
		int const TowerNum,
		Real64 & MyLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.Griffith
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate varialble speed tower model using Merkel's theory with UA adjustments developed by Scheier

		// METHODOLOGY EMPLOYED:
		// Find a fan speed that operates the tower to meet MyLoad

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DesignWetBulb( 25.56 ); // tower outdoor air entering wetbulb for design [C]
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.e-3 ); // Accuracy of solver result
		static std::string const RoutineName( "CalcMerkelVariableSpeedTower" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > Par( 8 ); // Parameter array passed to solver
		int SolFla; // Flag of solver
		Real64 CpWater;
		int LoopNum;
		int LoopSideNum;
		Real64 TempSetPoint;
		Real64 WaterMassFlowRatePerCellMin;
		Real64 WaterMassFlowRatePerCellMax;
		int NumCellMin = 0;
		int NumCellMax = 0;
		int NumCellOn;
		Real64 WaterMassFlowRatePerCell;
		Real64 UAdesignPerCell;
		Real64 AirFlowRatePerCell;
		Real64 OutletWaterTempOFF;
		Real64 FreeConvQdot;
		Real64 WaterFlowRateRatio;
		Real64 UAwetbulbAdjFac;
		Real64 UAairflowAdjFac;
		Real64 UAwaterflowAdjFac;
		Real64 UAadjustedPerCell;
		Real64 FullSpeedFanQdot;
		bool IncrNumCellFlag;
		Real64 MinSpeedFanQdot;
		Real64 FanPowerAdjustFac;

		WaterInletNode = SimpleTower( TowerNum ).WaterInletNodeNum;
		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
		WaterOutletNode = SimpleTower( TowerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		CTFanPower = 0.0;
		OutletWaterTemp = Node( WaterInletNode ).Temp;
		LoopNum = SimpleTower( TowerNum ).LoopNum;
		LoopSideNum = SimpleTower( TowerNum ).LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( SimpleTower( TowerNum ).SetpointIsOnOutlet ) {
				TempSetPoint = Node( WaterOutletNode ).TempSetPoint;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( SimpleTower( TowerNum ).SetpointIsOnOutlet ) {
				TempSetPoint = Node( WaterOutletNode ).TempSetPointHi;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
			}
		}}

		// Added for multi-cell. Determine the number of cells operating
		if ( SimpleTower( TowerNum ).DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MinFracFlowRate / SimpleTower( TowerNum ).NumCell;
			WaterMassFlowRatePerCellMax = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MaxFracFlowRate / SimpleTower( TowerNum ).NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
			NumCellMax = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( SimpleTower( TowerNum ).CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		SimpleTower( TowerNum ).NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;
		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( WaterMassFlowRate <= MassFlowTolerance || ( MyLoad > SmallLoad ) ) {
			// for multiple cells, we assume that it's a common bassin
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			return;
		}

		if ( std::abs( MyLoad ) <= SmallLoad ) {
		// tower doesn't need to do anything
			OutletWaterTemp = Node( WaterInletNode ).Temp;
			CTFanPower = 0.0;
			AirFlowRateRatio = 0.0;
			Qactual = 0.0;
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			return;
		}

		// first find free convection cooling rate
		UAdesignPerCell = SimpleTower( TowerNum ).FreeConvTowerUA / SimpleTower( TowerNum ).NumCell;
		AirFlowRatePerCell = SimpleTower( TowerNum ).FreeConvAirFlowRate / SimpleTower( TowerNum ).NumCell;
		OutletWaterTempOFF = Node( WaterInletNode ).Temp;
		WaterMassFlowRate = Node( WaterInletNode ).MassFlowRate;
		SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAdesignPerCell, OutletWaterTempOFF );

		FreeConvQdot = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTempOFF );
		CTFanPower = 0.0;

		if ( std::abs( MyLoad ) <= FreeConvQdot ) { // can meet load with free convection and fan off


			OutletWaterTemp = OutletWaterTempOFF;
			AirFlowRateRatio = 0.0;
			Qactual = FreeConvQdot;
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );

			return;
		}

		// next find full fan speed cooling rate
		UAdesignPerCell = SimpleTower( TowerNum ).HighSpeedTowerUA / SimpleTower( TowerNum ).NumCell;
		AirFlowRatePerCell = SimpleTower( TowerNum ).HighSpeedAirFlowRate / SimpleTower( TowerNum ).NumCell;
		AirFlowRateRatio = 1.0;
		WaterFlowRateRatio = WaterMassFlowRatePerCell / SimpleTower( TowerNum ).DesWaterMassFlowRatePerCell;
		UAwetbulbAdjFac = CurveValue( SimpleTower( TowerNum ).UAModFuncWetBulbDiffCurvePtr, ( DesignWetBulb - SimpleTowerInlet( TowerNum ).AirWetBulb ) );
		UAairflowAdjFac = CurveValue( SimpleTower( TowerNum ).UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio );
		UAwaterflowAdjFac = CurveValue( SimpleTower( TowerNum ).UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio );
		UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
		SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, OutletWaterTemp );
		FullSpeedFanQdot = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );

		if ( FullSpeedFanQdot <= std::abs( MyLoad ) ) { // full speed is what we want.

			if ( ( FullSpeedFanQdot + SmallLoad ) < std::abs( MyLoad ) && ( NumCellOn < SimpleTower( TowerNum ).NumCell ) && ( ( WaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) ) {
				// If full fan and not meeting setpoint, then increase number of cells until all are used or load is satisfied
				IncrNumCellFlag = true; // set value to true to enter in the loop
				while ( IncrNumCellFlag ) {
					++NumCellOn;
					SimpleTower( TowerNum ).NumCellOn = NumCellOn;
					WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;
					WaterFlowRateRatio = WaterMassFlowRatePerCell / SimpleTower( TowerNum ).DesWaterMassFlowRatePerCell;
					UAwaterflowAdjFac = CurveValue( SimpleTower( TowerNum ).UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio );
					UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
					SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, OutletWaterTemp );
					if ( ( FullSpeedFanQdot + SmallLoad ) < std::abs( MyLoad ) && ( NumCellOn < SimpleTower( TowerNum ).NumCell ) && ( ( WaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) ) {
						IncrNumCellFlag = true;
					} else {
						IncrNumCellFlag = false;
					}
				}
				FullSpeedFanQdot = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );
			}
			Qactual = FullSpeedFanQdot;
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			// now calculate fan power
			FanPowerAdjustFac = CurveValue( SimpleTower( TowerNum ).FanPowerfAirFlowCurve, AirFlowRateRatio );
			CTFanPower = SimpleTower( TowerNum ).HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / SimpleTower( TowerNum ).NumCell;

			return;

		}

		// next find minimum air flow ratio cooling rate
		AirFlowRateRatio = SimpleTower( TowerNum ).MinimumVSAirFlowFrac;
		AirFlowRatePerCell = AirFlowRateRatio * SimpleTower( TowerNum ).HighSpeedAirFlowRate / SimpleTower( TowerNum ).NumCell;
		UAairflowAdjFac = CurveValue( SimpleTower( TowerNum ).UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio );
		UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
		SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, OutletWaterTemp );
		MinSpeedFanQdot = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );

		if ( std::abs( MyLoad ) <= MinSpeedFanQdot ) { // min fan speed already exceeds load)
			Qactual = MinSpeedFanQdot;
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			// now calculate fan power
			FanPowerAdjustFac = CurveValue( SimpleTower( TowerNum ).FanPowerfAirFlowCurve, AirFlowRateRatio );
			CTFanPower = SimpleTower( TowerNum ).HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / SimpleTower( TowerNum ).NumCell;
			return;
		}

		if ( ( MinSpeedFanQdot < std::abs( MyLoad ) ) && ( std::abs( MyLoad ) < FullSpeedFanQdot ) ) {
			// load can be refined by modulationg fan speed, call regulafalsi

			Par( 1 ) = double( TowerNum );
			Par( 2 ) = MyLoad;
			Par( 3 ) = WaterMassFlowRatePerCell;
			Par( 4 ) = UAdesignPerCell;
			Par( 5 ) = UAwetbulbAdjFac;
			Par( 6 ) = UAwaterflowAdjFac;
			Par( 7 ) = CpWater;
			Par( 8 ) = WaterMassFlowRate;

			SolveRegulaFalsi( Acc, MaxIte, SolFla, AirFlowRateRatio, VSMerkelResidual, SimpleTower( TowerNum ).MinimumVSAirFlowFrac, 1.0, Par );

			if ( SolFla == -1 ) {
				if ( ! WarmupFlag ) {
					if ( SimpleTower( TowerNum ).VSMerkelAFRErrorIter < 1 ) {
						++SimpleTower( TowerNum ).VSMerkelAFRErrorIter;
						ShowWarningError( cCoolingTower_VariableSpeedMerkel + " - Iteration limit exceeded calculating variable speed fan ratio for unit = " + SimpleTower( TowerNum ).Name );
						ShowContinueError( "Estimated air flow ratio  = " + RoundSigDigits( ( std::abs( MyLoad ) - MinSpeedFanQdot ) / ( FullSpeedFanQdot - MinSpeedFanQdot ), 4 ) );
						ShowContinueError( "Calculated air flow ratio = " + RoundSigDigits( AirFlowRateRatio, 4 ) );
						ShowContinueErrorTimeStamp( "The calculated air flow ratio will be used and the simulation continues. Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( cCoolingTower_VariableSpeedMerkel + " \"" + SimpleTower( TowerNum ).Name + "\" - Iteration limit exceeded calculating air flow ratio error continues. air flow ratio statistics follow.", SimpleTower( TowerNum ).VSMerkelAFRErrorIter, AirFlowRateRatio, AirFlowRateRatio );
				}
			} else if ( SolFla == -2 ) {
				AirFlowRateRatio = ( std::abs( MyLoad ) - MinSpeedFanQdot ) / ( FullSpeedFanQdot - MinSpeedFanQdot );
				if ( ! WarmupFlag ) {
					if ( SimpleTower( TowerNum ).VSMerkelAFRErrorFail < 1 ) {
						++SimpleTower( TowerNum ).VSMerkelAFRErrorFail;
						ShowWarningError( cCoolingTower_VariableSpeedMerkel + " - solver failed calculating variable speed fan ratio for unit = " + SimpleTower( TowerNum ).Name );
						ShowContinueError( "Estimated air flow ratio  = " + RoundSigDigits( AirFlowRateRatio, 4 ) );
						ShowContinueErrorTimeStamp( "The estimated air flow ratio will be used and the simulation continues. Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( cCoolingTower_VariableSpeedMerkel + " \"" + SimpleTower( TowerNum ).Name + "\" - solver failed calculating air flow ratio error continues. air flow ratio statistics follow.", SimpleTower( TowerNum ).VSMerkelAFRErrorFail, AirFlowRateRatio, AirFlowRateRatio );
				}
			}

			// now rerun to get peformance with AirFlowRateRatio
			AirFlowRatePerCell = AirFlowRateRatio * SimpleTower( TowerNum ).HighSpeedAirFlowRate / SimpleTower( TowerNum ).NumCell;

			UAairflowAdjFac = CurveValue( SimpleTower( TowerNum ).UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio );
			UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

			SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, OutletWaterTemp );
			Qactual = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );

			// now calculate fan power
			FanPowerAdjustFac = CurveValue( SimpleTower( TowerNum ).FanPowerfAirFlowCurve, AirFlowRateRatio );
			CTFanPower = SimpleTower( TowerNum ).HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / SimpleTower( TowerNum ).NumCell;

		}

	}

	Real64
	VSMerkelResidual(
		Real64 const AirFlowRateRatio, // fan speed ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = Tower number
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
		using CurveManager::CurveValue;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// par(2) =MyLoad [W] , negative is cooling
		// par(3) = water mass flow per cell
		// par(4) = Design UA per cell
		// par(5) = UA adjust factor for wetbulb
		// par(6) = UA adjust factor for water flow rate
		// par(7) = specific heat of water at inlet temp
		// par(8) = water mass flow rate, total [kg/s]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int TowerNum;
		Real64 WaterMassFlowRatePerCell;
		Real64 TargetLoad;
		Real64 UAdesignPerCell;
		Real64 UAwetbulbAdjFac;
		Real64 UAairflowAdjFac;
		Real64 UAwaterflowAdjFac;
		Real64 CpWater;
		Real64 TotalWaterMassFlowRate;
		Real64 AirFlowRatePerCell;
		Real64 UAadjustedPerCell;
		Real64 Qdot;
		Real64 OutletWaterTempTrial;

		TowerNum = int( Par( 1 ) );
		TargetLoad = Par( 2 );
		WaterMassFlowRatePerCell = Par( 3 );
		UAdesignPerCell = Par( 4 );
		UAwetbulbAdjFac = Par( 5 );
		UAwaterflowAdjFac = Par( 6 );
		CpWater = Par( 7 );
		TotalWaterMassFlowRate = Par( 8 );

		AirFlowRatePerCell = AirFlowRateRatio * SimpleTower( TowerNum ).HighSpeedAirFlowRate / SimpleTower( TowerNum ).NumCell;

		UAairflowAdjFac = CurveValue( SimpleTower( TowerNum ).UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio );
		UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

		SimSimpleTower( TowerNum, WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, OutletWaterTempTrial );

		Qdot = TotalWaterMassFlowRate * CpWater * ( Node( SimpleTower( TowerNum ).WaterInletNodeNum ).Temp - OutletWaterTempTrial );

		Residuum = std::abs( TargetLoad ) - Qdot;

		return Residuum;
	}

	void
	CalcVariableSpeedTower( int const TowerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   Feb 2005
		//       MODIFIED       A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
		//                      B Griffith, general fluid props
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// To simulate the operation of a variable-speed fan cooling tower.

		// METHODOLOGY EMPLOYED:
		// For each simulation time step, a desired range temperature (Twater,inlet-Twater,setpoint) and desired approach
		// temperature (Twater,setpoint-Tair,WB) is calculated which meets the outlet water temperature setpoint. This
		// desired range and approach temperature also provides a balance point for the empirical model where:
		// Tair,WB + Twater,range + Tapproach = Node(WaterInletNode)%Temp
		// Calculation of water outlet temperature uses one of the following equations:
		// Twater,outlet = Tair,WB + Tapproach          (1)  or
		// Twater,outlet = Twater,inlet - Twater,range  (2)
		// If a solution (or balance) is found, these 2 calculation methods are equal. Equation 2 is used to calculate
		// the outlet water temperature in the free convection regime and at the minimum or maximum fan speed so that
		// if a solution is not reached, the outlet water temperature is approximately equal to the inlet water temperature
		// and the tower fan must be varied to meet the setpoint. Equation 1 is used when the fan speed is varied between
		// the minimum and maximum fan speed to meet the outlet water temperature setpoint.
		// The outlet water temperature in the free convection regime is first calculated to see if the setpoint is met.
		// If the setpoint is met, the fan is OFF and the outlet water temperature is allowed to float below the set
		// point temperature. If the setpoint is not met, the outlet water temperature is re-calculated at the minimum
		// fan speed. If the setpoint is met, the fan is cycled to exactly meet the outlet water temperature setpoint.
		// If the setpoint is not met at the minimum fan speed, the outlet water temperature is re-calculated at the
		// maximum fan speed. If the setpoint at the maximum fan speed is not met, the fan runs at maximum speed the
		// entire time step. If the setpoint is met at the maximum fan speed, the fan speed is varied to meet the setpoint.
		// If a tower has multiple cells, the specified inputs of or the autosized capacity
		//  and air/water flow rates are for the entire tower. The number of cells to operate
		//  is first determined based on the user entered minimal and maximal water flow fractions
		//  per cell. If the loads are not met, more cells (if available) will operate to meet
		//  the loads. Inside each cell, the fan speed varies in the same way.
		// REFERENCES:
		// Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
		// "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
		// ASHRAE Transactions 2002, V. 108, Pt. 1.
		// York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
		// Form 160.00-SG2 (0502). 2002.

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using CurveManager::CurveValue;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataGlobals::CurrentTime;
		using General::CreateSysTimeIntervalString;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::MassFlowTolerance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt OutputFormat( "(F5.2)" );
		static gio::Fmt OutputFormat2( "(F8.5)" );
		int const MaxIte( 500 ); // Maximum number of iterations
		Real64 const Acc( 0.0001 ); // Accuracy of result
		static std::string const RoutineName( "CalcVariableSpeedTower" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OutletWaterTempOFF; // Outlet water temperature with fan OFF (C)
		Real64 OutletWaterTempON; // Outlet water temperature with fan ON at maximum fan speed (C)
		Real64 OutletWaterTempMIN; // Outlet water temperature with fan at minimum speed (C)
		Real64 CpWater; // Specific heat of water
		Real64 TempSetPoint( 0.0 ); // Outlet water temperature setpoint (C)
		Real64 FanCurveValue; // Output of fan power as a func of air flow rate ratio curve
		Real64 AirDensity; // Density of air [kg/m3]
		Real64 AirMassFlowRate; // Mass flow rate of air [kg/s]
		Real64 InletAirEnthalpy; // Enthalpy of entering moist air [J/kg]
		int SolFla; // Flag of solver
		Array1D< Real64 > Par( 6 ); // Parameter array for regula falsi solver
		Real64 Twb; // inlet air wet-bulb temperature
		Real64 TwbCapped; // inlet air wet-bulb temp passed to VS tower model
		Real64 Tr; // range temperature
		Real64 TrCapped; // range temp passed to VS tower model
		Real64 Ta; // approach temperature
		Real64 TaCapped; // approach temp passed to VS tower model
		Real64 WaterFlowRateRatio; // Water flow rate ratio
		Real64 WaterFlowRateRatioCapped; // Water flow rate ratio passed to VS tower model
		Real64 WaterDensity; // density of inlet water
		Real64 FreeConvectionCapFrac; // fraction of tower capacity in free convection
		Real64 FlowFraction; // liquid to gas (L/G) ratio for cooling tower
		std::string OutputChar; // character string used for warning messages
		std::string OutputChar2; // character string used for warning messages
		std::string OutputChar3; // character string used for warning messages
		std::string OutputChar4; // character string used for warning messages
		std::string OutputChar5; // character string used for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		Real64 CurrentEndTime; // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		int LoopNum;
		int LoopSideNum;

		//Added variables for multicell
		Real64 WaterMassFlowRatePerCellMin;
		Real64 WaterMassFlowRatePerCellMax;
		static int NumCellMin( 0 );
		static int NumCellMax( 0 );
		static int NumCellOn( 0 );
		Real64 WaterMassFlowRatePerCell;
		bool IncrNumCellFlag;

		// Added for multi-cell. Determine the number of cells operating
		if ( SimpleTower( TowerNum ).DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MinFracFlowRate / SimpleTower( TowerNum ).NumCell;
			WaterMassFlowRatePerCellMax = SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).MaxFracFlowRate / SimpleTower( TowerNum ).NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
			NumCellMax = min( int( ( WaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), SimpleTower( TowerNum ).NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( SimpleTower( TowerNum ).CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		SimpleTower( TowerNum ).NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;

		// Set inlet and outlet nodes and initialize subroutine variables

		WaterInletNode = SimpleTower( TowerNum ).WaterInletNodeNum;
		WaterOutletNode = SimpleTower( TowerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		CTFanPower = 0.0;
		OutletWaterTemp = Node( WaterInletNode ).Temp;

		WaterUsage = 0.0;
		Twb = SimpleTowerInlet( TowerNum ).AirWetBulb;
		TwbCapped = SimpleTowerInlet( TowerNum ).AirWetBulb;
		LoopNum = SimpleTower( TowerNum ).LoopNum;
		LoopSideNum = SimpleTower( TowerNum ).LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
		} else {
			assert( false );
		}}

		Tr = Node( WaterInletNode ).Temp - TempSetPoint;
		Ta = TempSetPoint - SimpleTowerInlet( TowerNum ).AirWetBulb;

		// Do not RETURN here if flow rate is less than MassFlowTolerance. Check basin heater and then RETURN.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) return;
		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( WaterMassFlowRate <= MassFlowTolerance ) {
			CalcBasinHeaterPower( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff, SimpleTower( TowerNum ).BasinHeaterSchedulePtr, SimpleTower( TowerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			return;
		}

		//loop to increment NumCell if we cannot meet the setpoint with the actual number of cells calculated above
		IncrNumCellFlag = true;
		while ( IncrNumCellFlag ) {
			IncrNumCellFlag = false;
			// Initialize inlet node water properties
			WaterDensity = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
			WaterFlowRateRatio = WaterMassFlowRatePerCell / ( WaterDensity * SimpleTower( TowerNum ).CalibratedWaterFlowRate / SimpleTower( TowerNum ).NumCell );

			// check independent inputs with respect to model boundaries
			CheckModelBounds( TowerNum, Twb, Tr, Ta, WaterFlowRateRatio, TwbCapped, TrCapped, TaCapped, WaterFlowRateRatioCapped );

			//   determine the free convection capacity by finding the outlet temperature at full air flow and multiplying
			//   the tower's full capacity temperature difference by the percentage of tower capacity in free convection
			//   regime specified by the user

			AirFlowRateRatio = 1.0;
			OutletWaterTempOFF = Node( WaterInletNode ).Temp;
			OutletWaterTempON = Node( WaterInletNode ).Temp;
			OutletWaterTemp = OutletWaterTempOFF;
			FreeConvectionCapFrac = SimpleTower( TowerNum ).FreeConvectionCapacityFraction;

			SimVariableTower( TowerNum, WaterFlowRateRatioCapped, AirFlowRateRatio, TwbCapped, OutletWaterTempON );

			if ( OutletWaterTempON > TempSetPoint ) {
				FanCyclingRatio = 1.0;
				AirFlowRateRatio = 1.0;
				CTFanPower = SimpleTower( TowerNum ).HighSpeedFanPower * NumCellOn / SimpleTower( TowerNum ).NumCell;
				OutletWaterTemp = OutletWaterTempON;
				// if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
				if ( NumCellOn < SimpleTower( TowerNum ).NumCell && ( WaterMassFlowRate / ( NumCellOn + 1 ) ) > WaterMassFlowRatePerCellMin ) {
					++NumCellOn;
					WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellOn;
					IncrNumCellFlag = true;
				}
			}
		}

		// find the correct air ratio only if full flow is  too much
		if ( OutletWaterTempON < TempSetPoint ) {
			//   outlet water temperature is calculated in the free convection regime
			OutletWaterTempOFF = Node( WaterInletNode ).Temp - FreeConvectionCapFrac * ( Node( WaterInletNode ).Temp - OutletWaterTempON );
			//   fan is OFF
			FanCyclingRatio = 0.0;
			//   air flow ratio is assumed to be the fraction of tower capacity in the free convection regime (fan is OFF but air is flowing)
			AirFlowRateRatio = FreeConvectionCapFrac;

			// Assume setpoint was met using free convection regime (pump ON and fan OFF)
			CTFanPower = 0.0;
			OutletWaterTemp = OutletWaterTempOFF;

			if ( OutletWaterTempOFF > TempSetPoint ) {
				// Setpoint was not met, turn on cooling tower fan at minimum fan speed

				AirFlowRateRatio = SimpleTower( TowerNum ).MinimumVSAirFlowFrac;
				SimVariableTower( TowerNum, WaterFlowRateRatioCapped, AirFlowRateRatio, TwbCapped, OutletWaterTempMIN );

				if ( OutletWaterTempMIN < TempSetPoint ) {
					//         if setpoint was exceeded, cycle the fan at minimum air flow to meet the setpoint temperature
					if ( SimpleTower( TowerNum ).FanPowerfAirFlowCurve == 0 ) {
						CTFanPower = pow_3( AirFlowRateRatio ) * SimpleTower( TowerNum ).HighSpeedFanPower * NumCellOn / SimpleTower( TowerNum ).NumCell;
					} else {
						FanCurveValue = CurveValue( SimpleTower( TowerNum ).FanPowerfAirFlowCurve, AirFlowRateRatio );
						CTFanPower = max( 0.0, ( SimpleTower( TowerNum ).HighSpeedFanPower * FanCurveValue ) ) * NumCellOn / SimpleTower( TowerNum ).NumCell;
					}
					//       fan is cycling ON and OFF at the minimum fan speed. Adjust fan power and air flow rate ratio according to cycling rate
					FanCyclingRatio = ( ( OutletWaterTempOFF - TempSetPoint ) / ( OutletWaterTempOFF - OutletWaterTempMIN ) );
					CTFanPower *= FanCyclingRatio;
					OutletWaterTemp = TempSetPoint;
					AirFlowRateRatio = ( FanCyclingRatio * SimpleTower( TowerNum ).MinimumVSAirFlowFrac ) + ( ( 1 - FanCyclingRatio ) * FreeConvectionCapFrac );
				} else {
					//       if setpoint was not met at minimum fan speed, set fan speed to maximum
					AirFlowRateRatio = 1.0;
					//         fan will not cycle and runs the entire time step
					FanCyclingRatio = 1.0;

					SimVariableTower( TowerNum, WaterFlowRateRatioCapped, AirFlowRateRatio, TwbCapped, OutletWaterTemp );

					// Setpoint was met with pump ON and fan ON at full flow
					// Calculate the fraction of full air flow to exactly meet the setpoint temperature

					Par( 1 ) = TowerNum; // Index to cooling tower
					//         cap the water flow rate ratio and inlet air wet-bulb temperature to provide a stable output
					Par( 2 ) = WaterFlowRateRatioCapped; // water flow rate ratio
					Par( 3 ) = TwbCapped; // Inlet air wet-bulb temperature [C]
					//         do not cap desired range and approach temperature to provide a valid (balanced) output for this simulation time step
					Par( 4 ) = Tr; // Tower range temperature [C]
					Par( 5 ) = Ta; // desired approach temperature [C]
					Par( 6 ) = 1.0; // calculate the air flow rate ratio required for a balance

					SolveRegulaFalsi( Acc, MaxIte, SolFla, AirFlowRateRatio, SimpleTowerApproachResidual, SimpleTower( TowerNum ).MinimumVSAirFlowFrac, 1.0, Par );
					if ( SolFla == -1 ) {
						if ( ! WarmupFlag ) ShowWarningError( "Cooling tower iteration limit exceeded when calculating air flow rate ratio for tower " + SimpleTower( TowerNum ).Name );
						//           IF RegulaFalsi cannot find a solution then provide detailed output for debugging
					} else if ( SolFla == -2 ) {
						if ( ! WarmupFlag ) {
							gio::write( OutputChar, OutputFormat ) << TwbCapped;
							gio::write( OutputChar2, OutputFormat ) << Tr;
							gio::write( OutputChar3, OutputFormat ) << Ta;
							gio::write( OutputChar4, OutputFormat ) << WaterFlowRateRatioCapped;
							gio::write( OutputChar5, OutputFormat ) << SimpleTower( TowerNum ).MinimumVSAirFlowFrac;
							if ( SimpleTower( TowerNum ).CoolingTowerAFRRFailedCount < 1 ) {
								++SimpleTower( TowerNum ).CoolingTowerAFRRFailedCount;
								ShowWarningError( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\" - Cooling tower air flow rate ratio calculation failed " );
								ShowContinueError( "...with conditions as Twb = " + OutputChar + ", Trange = " + OutputChar2 + ", Tapproach = " + OutputChar3 + ", and water flow rate ratio = " + OutputChar4 );
								ShowContinueError( "...a solution could not be found within the valid range of air flow rate ratios" );
								ShowContinueErrorTimeStamp( " ...Valid air flow rate ratio range = " + OutputChar5 + " to 1.0." );
								ShowContinueError( "...Consider modifying the design approach or design range temperature for this tower." );
							} else {
								ShowRecurringWarningErrorAtEnd( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\" - Cooling tower air flow rate ratio calculation failed error continues.", SimpleTower( TowerNum ).CoolingTowerAFRRFailedIndex );
							}
						}
					}

					//         Use theoretical cubic for deterination of fan power if user has not specified a fan power ratio curve
					if ( SimpleTower( TowerNum ).FanPowerfAirFlowCurve == 0 ) {
						CTFanPower = pow_3( AirFlowRateRatio ) * SimpleTower( TowerNum ).HighSpeedFanPower * NumCellOn / SimpleTower( TowerNum ).NumCell;
					} else {
						FanCurveValue = CurveValue( SimpleTower( TowerNum ).FanPowerfAirFlowCurve, AirFlowRateRatio );
						CTFanPower = max( 0.0, ( SimpleTower( TowerNum ).HighSpeedFanPower * FanCurveValue ) ) * NumCellOn / SimpleTower( TowerNum ).NumCell;
					}
					//           outlet water temperature is calculated as the inlet air wet-bulb temperature plus tower approach temperature
					OutletWaterTemp = Twb + Ta;
				} // IF(OutletWaterTempMIN .LT. TempSetPoint)THEN

			} // IF(OutletWaterTempOFF .GT. TempSetPoint)THEN
		} // IF(OutletWaterTempON .LT. TempSetPoint) ie if tower should not run at full capacity

		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, Node( SimpleTower( TowerNum ).WaterInletNodeNum ).Temp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
		Qactual = WaterMassFlowRate * CpWater * ( Node( WaterInletNode ).Temp - OutletWaterTemp );
		SimpleTower( TowerNum ).NumCellOn = NumCellOn;
		// Set water and air properties
		AirDensity = PsyRhoAirFnPbTdbW( SimpleTowerInlet( TowerNum ).AirPress, SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirHumRat );
		AirMassFlowRate = AirFlowRateRatio * SimpleTower( TowerNum ).HighSpeedAirFlowRate * AirDensity * SimpleTower( TowerNum ).NumCellOn / SimpleTower( TowerNum ).NumCell;
		InletAirEnthalpy = PsyHFnTdbRhPb( SimpleTowerInlet( TowerNum ).AirWetBulb, 1.0, SimpleTowerInlet( TowerNum ).AirPress );

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).PrintLGMessage ) {
				++VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountFlowFrac;
				//       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountFlowFrac < 2 ) {
					ShowWarningError( VSTower( SimpleTower( TowerNum ).VSTower ).LGBuffer1 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).LGBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Liquid to gas ratio is out of range error continues...", VSTower( SimpleTower( TowerNum ).VSTower ).ErrIndexLG, VSTower( SimpleTower( TowerNum ).VSTower ).LGLast, VSTower( SimpleTower( TowerNum ).VSTower ).LGLast );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//   warn user on first occurrence if flow fraction is greater than maximum for the YorkCalc model, use recurring warning stats
		if ( SimpleTower( TowerNum ).TowerModelType == YorkCalcModel || SimpleTower( TowerNum ).TowerModelType == YorkCalcUserDefined ) {
			VSTower( SimpleTower( TowerNum ).VSTower ).PrintLGMessage = false;
			//      Do not report error message in free convection regime
			if ( AirFlowRateRatio > SimpleTower( TowerNum ).MinimumVSAirFlowFrac ) {
				FlowFraction = WaterFlowRateRatioCapped / AirFlowRateRatio;
				//        Flow fractions greater than a MaxLiquidToGasRatio of 8 are not reliable using the YorkCalc model
				if ( FlowFraction > VSTower( SimpleTower( TowerNum ).VSTower ).MaxLiquidToGasRatio ) {
					//          Report warnings only during actual simulation
					if ( ! WarmupFlag ) {
						VSTower( SimpleTower( TowerNum ).VSTower ).PrintLGMessage = true;
						gio::write( OutputChar, OutputFormat ) << FlowFraction;
						gio::write( OutputChar2, OutputFormat ) << VSTower( SimpleTower( TowerNum ).VSTower ).MaxLiquidToGasRatio;
						VSTower( SimpleTower( TowerNum ).VSTower ).LGBuffer1 = SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Liquid to gas ratio (L/G) is out of range at " + OutputChar + '.';
						VSTower( SimpleTower( TowerNum ).VSTower ).LGBuffer2 = " ...Valid maximum ratio = " + OutputChar2 + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
						VSTower( SimpleTower( TowerNum ).VSTower ).LGLast = FlowFraction;
					}
				}
			}
		}

	}

	void
	SimSimpleTower(
		int const TowerNum,
		Real64 const WaterMassFlowRate,
		Real64 const AirFlowRate,
		Real64 const UAdesign,
		Real64 & OutletWaterTemp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  Shirey, Raustad, Jan 2001

		// PURPOSE OF THIS SUBROUTINE:
		// See purpose for Single Speed or Two Speed tower model

		// METHODOLOGY EMPLOYED:
		// See methodology for Single Speed or Two Speed tower model

		// REFERENCES:
		// Merkel, F. 1925.  Verduftungskuhlung. VDI Forschungsarbeiten, Nr 275, Berlin.
		// ASHRAE     1999.  HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculations.

		// USE STATEMENTS:
		// na

		// Locals
		Real64 Qactual; // Actual heat transfer rate between tower water and air [W]

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const IterMax( 50 ); // Maximum number of iterations allowed
		Real64 const WetBulbTolerance( 0.00001 ); // Maximum error for exiting wet-bulb temperature between iterations
		// [delta K/K]
		Real64 const DeltaTwbTolerance( 0.001 ); // Maximum error (tolerance) in DeltaTwb for iteration convergence [C]
		static std::string const RoutineName( "SimSimpleTower" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Iter; // Number of iterations completed
		Real64 MdotCpWater; // Water mass flow rate times the heat capacity [W/K]
		Real64 InletAirTemp; // Dry-bulb temperature of air entering the tower [C]
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
		Real64 AirCapacity; // MdotCp of air through the tower
		Real64 CapacityRatioMin; // Minimum capacity of airside and waterside
		Real64 CapacityRatioMax; // Maximum capacity of airside and waterside
		Real64 CapacityRatio; // Ratio of minimum to maximum capacity
		Real64 NumTransferUnits; // Number of transfer Units [NTU]
		Real64 WetBulbError; // Calculated error for exiting wet-bulb temperature between iterations [delta K/K]
		Real64 CpAirside; // Delta enthalpy of the tower air divides by delta air wet-bulb temp [J/kg/K]
		Real64 DeltaTwb; // Absolute value of difference between inlet and outlet air wet-bulb temp [C]

		// set inlet and outlet node numbers, and initialize some local variables

		WaterInletNode = SimpleTower( TowerNum ).WaterInletNodeNum;
		WaterOutletNode = SimpleTower( TowerNum ).WaterOutletNodeNum;
		Qactual = 0.0;
		//    WetBulbTolerance  = 0.00001
		WetBulbError = 1.0;
		//    IterMax           = 50
		DeltaTwb = 1.0;
		//    DeltaTwbTolerance = 0.001

		// set local tower inlet and outlet temperature variables
		InletWaterTemp = SimpleTowerInlet( TowerNum ).WaterTemp;
		OutletWaterTemp = InletWaterTemp;
		InletAirTemp = SimpleTowerInlet( TowerNum ).AirTemp;
		InletAirWetBulb = SimpleTowerInlet( TowerNum ).AirWetBulb;

		if ( UAdesign == 0.0 ) return;

		// set water and air properties
		AirDensity = PsyRhoAirFnPbTdbW( SimpleTowerInlet( TowerNum ).AirPress, InletAirTemp, SimpleTowerInlet( TowerNum ).AirHumRat );
		AirMassFlowRate = AirFlowRate * AirDensity;
		CpAir = PsyCpAirFnWTdb( SimpleTowerInlet( TowerNum ).AirHumRat, InletAirTemp );
		CpWater = GetSpecificHeatGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, SimpleTowerInlet( TowerNum ).WaterTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );
		InletAirEnthalpy = PsyHFnTdbRhPb( SimpleTowerInlet( TowerNum ).AirWetBulb, 1.0, SimpleTowerInlet( TowerNum ).AirPress );

		// initialize exiting wet bulb temperature before iterating on final solution
		OutletAirWetBulb = InletAirWetBulb + 6.0;

		// Calcluate mass flow rates
		if ( WaterMassFlowRate > 0.0 ) {
			MdotCpWater = WaterMassFlowRate * CpWater;
		} else {
			OutletWaterTemp = InletWaterTemp;
			return;
		}
		Iter = 0;
		while ( ( WetBulbError > WetBulbTolerance ) && ( Iter <= IterMax ) && ( DeltaTwb > DeltaTwbTolerance ) ) {
			++Iter;
			//        OutletAirEnthalpy = PsyHFnTdbRhPb(OutletAirWetBulb,1.0,OutBaroPress)
			OutletAirEnthalpy = PsyHFnTdbRhPb( OutletAirWetBulb, 1.0, SimpleTowerInlet( TowerNum ).AirPress );
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

	void
	SimVariableTower(
		int const TowerNum, // variable speed tower index
		Real64 const WaterFlowRateRatio, // current water flow rate ratio (capped if applicable)
		Real64 const AirFlowRateRatio, // current air flow rate ratio
		Real64 const Twb, // current inlet air wet-bulb temperature (C, capped if applicable)
		Real64 & OutletWaterTemp // calculated tower outlet water temperature (C)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Feb. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the leaving water temperature of the variable speed cooling tower.

		// METHODOLOGY EMPLOYED:
		// The range temperature is varied to determine balance point where model output (Tapproach),
		// range temperature and inlet air wet-bulb temperature show a balance as:
		// Twb + Tapproach + Trange = Node(WaterInletNode)%Temp

		// REFERENCES:
		// Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
		// "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
		// ASHRAE Transactions 2002, V. 108, Pt. 1.
		// York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
		// Form 160.00-SG2 (0502). 2002.

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations
		Real64 const Acc( 0.0001 ); // Accuracy of result

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SolFla; // Flag of solver
		Array1D< Real64 > Par( 4 ); // Parameter array for regula falsi solver
		Real64 Tr; // range temperature which results in an energy balance
		Real64 TempSetPoint( 0.0 ); // local temporary for loop setpoint

		//   determine tower outlet water temperature
		Par( 1 ) = TowerNum; // Index to cooling tower
		Par( 2 ) = WaterFlowRateRatio; // water flow rate ratio
		Par( 3 ) = AirFlowRateRatio; // air flow rate ratio
		Par( 4 ) = Twb; // inlet air wet-bulb temperature [C]
		SolveRegulaFalsi( Acc, MaxIte, SolFla, Tr, SimpleTowerTrResidual, 0.001, VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp, Par );

		OutletWaterTemp = SimpleTowerInlet( TowerNum ).WaterTemp - Tr;

		if ( SolFla == -1 ) {
			ShowSevereError( "Iteration limit exceeded in calculating tower nominal capacity at minimum air flow ratio" );
			ShowContinueError( "Design inlet air wet-bulb or approach temperature must be modified to achieve an acceptable range at the minimum air flow rate" );
			ShowContinueError( "Cooling tower simulation failed to converge for tower " + SimpleTower( TowerNum ).Name );
			//    if SolFla = -2, Tr is returned as minimum value (0.001) and outlet temp = inlet temp - 0.001
		} else if ( SolFla == -2 ) { // decide if should run at max flow
			{ auto const SELECT_CASE_var( PlantLoop( SimpleTower( TowerNum ).LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				TempSetPoint = PlantLoop( SimpleTower( TowerNum ).LoopNum ).LoopSide( SimpleTower( TowerNum ).LoopSideNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				TempSetPoint = PlantLoop( SimpleTower( TowerNum ).LoopNum ).LoopSide( SimpleTower( TowerNum ).LoopSideNum ).TempSetPointHi;
			} else {
				assert( false );
			}}
			if ( SimpleTowerInlet( TowerNum ).WaterTemp > ( TempSetPoint + VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp ) ) { // run flat out
				OutletWaterTemp = SimpleTowerInlet( TowerNum ).WaterTemp - VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp;
			}
		}

	}

	void
	CalcVSTowerApproach(
		int const TowerNum, // Index to cooling tower
		Real64 const PctWaterFlow, // Water flow ratio of cooling tower
		Real64 const AirFlowRatio, // Air flow ratio of cooling tower
		Real64 const Twb, // Inlet air wet-bulb temperature [C]
		Real64 const Tr, // Cooling tower range (outlet water temp minus inlet air wet-bulb temp) [C]
		Real64 & Approach // Calculated approach temperature [C]
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Feb. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate tower approach temperature (e.g. outlet water temp minus inlet air wet-bulb temp)
		// given air flow ratio, water flow ratio, inlet air wet-bulb temp, and tower range.

		// METHODOLOGY EMPLOYED:
		// Calculation method used empirical models from CoolTools or York to determine performance
		// of variable speed (variable air flow rate) cooling towers.

		// REFERENCES:
		// Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
		// "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
		// ASHRAE Transactions 2002, V. 108, Pt. 1.
		// York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
		// Form 160.00-SG2 (0502). 2002.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    REAL(r64)        :: Twb                       ! Inlet air wet-bulb temperature [C] (or [F] for CoolTools Model)
		//    REAL(r64)        :: Tr                        ! Cooling tower range (outlet water temp minus inlet air wet-bulb temp) [C]
		//   (or [F] for CoolTools Model)
		static Real64 PctAirFlow( 0.0 ); // air flow rate ratio (fan power ratio in the case of CoolTools model)
		static Real64 FlowFactor( 0.0 ); // water flow rate to air flow rate ratio (L/G) for YorkCalc model

		//    IF(SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsXFModel .OR. &
		//        SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsCFModel .OR. &
		//        SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcModel)THEN
		//      Twb        = (TwbIN * 1.8) + 32.0 ! Convert Celsius to Fahrenheit for CoolTools Model
		//      Tr         = (TrIN * 1.8)
		// Convert air flow rate ratio to fan power ratio for CoolTools Model
		//      IF(SimpleTower(TowerNum)%TowerModelType .NE. YorkCalcModel)PctAirFlow = (AirFlowRatio)**3.0
		//    ELSE
		//      Twb        = TwbIN
		//      Tr         = TrIN
		//      IF(SimpleTower(TowerNum)%TowerModelType .NE. YorkCalcUserDefined)PctAirFlow = (AirFlowRatio)**3.0
		//    END IF

		if ( SimpleTower( TowerNum ).TowerModelType == YorkCalcModel || SimpleTower( TowerNum ).TowerModelType == YorkCalcUserDefined ) {
			PctAirFlow = AirFlowRatio;
			FlowFactor = PctWaterFlow / PctAirFlow;
			Approach = VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 1 ) + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 2 ) * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 3 ) * Twb * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 4 ) * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 5 ) * Twb * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 6 ) * Twb * Twb * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 7 ) * Tr * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 8 ) * Twb * Tr * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 9 ) * Twb * Twb * Tr * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 10 ) * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 11 ) * Twb * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 12 ) * Twb * Twb * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 13 ) * Tr * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 14 ) * Twb * Tr * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 15 ) * Twb * Twb * Tr * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 16 ) * Tr * Tr * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 17 ) * Twb * Tr * Tr * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 18 ) * Twb * Twb * Tr * Tr * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 19 ) * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 20 ) * Twb * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 21 ) * Twb * Twb * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 22 ) * Tr * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 23 ) * Twb * Tr * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 24 ) * Twb * Twb * Tr * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 25 ) * Tr * Tr * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 26 ) * Twb * Tr * Tr * FlowFactor * FlowFactor + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 27 ) * Twb * Twb * Tr * Tr * FlowFactor * FlowFactor;

		} else { // empirical model is CoolTools format

			//     the CoolTools model actually uses PctFanPower = AirFlowRatio^3 as an input to the model
			PctAirFlow = pow_3( AirFlowRatio );
			Approach = VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 1 ) + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 2 ) * PctAirFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 3 ) * PctAirFlow * PctAirFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 4 ) * PctAirFlow * PctAirFlow * PctAirFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 5 ) * PctWaterFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 6 ) * PctAirFlow * PctWaterFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 7 ) * PctAirFlow * PctAirFlow * PctWaterFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 8 ) * PctWaterFlow * PctWaterFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 9 ) * PctAirFlow * PctWaterFlow * PctWaterFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 10 ) * PctWaterFlow * PctWaterFlow * PctWaterFlow + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 11 ) * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 12 ) * PctAirFlow * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 13 ) * PctAirFlow * PctAirFlow * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 14 ) * PctWaterFlow * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 15 ) * PctAirFlow * PctWaterFlow * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 16 ) * PctWaterFlow * PctWaterFlow * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 17 ) * Twb * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 18 ) * PctAirFlow * Twb * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 19 ) * PctWaterFlow * Twb * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 20 ) * Twb * Twb * Twb + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 21 ) * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 22 ) * PctAirFlow * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 23 ) * PctAirFlow * PctAirFlow * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 24 ) * PctWaterFlow * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 25 ) * PctAirFlow * PctWaterFlow * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 26 ) * PctWaterFlow * PctWaterFlow * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 27 ) * Twb * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 28 ) * PctAirFlow * Twb * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 29 ) * PctWaterFlow * Twb * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 30 ) * Twb * Twb * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 31 ) * Tr * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 32 ) * PctAirFlow * Tr * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 33 ) * PctWaterFlow * Tr * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 34 ) * Twb * Tr * Tr + VSTower( SimpleTower( TowerNum ).VSTower ).Coeff( 35 ) * Tr * Tr * Tr;
		}
		//    capping approach to 0 results in failure of RegulaFalsi routine
		//    Approach = MAX(0.0, Approach)

		//    IF(SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsXFModel .OR. &
		//        SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsCFModel .OR. &
		//        SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcModel)THEN
		//      Approach = (Approach / 1.8)  ! Convert from Fahrenheit to Celsius
		//    END IF

	}

	void
	CheckModelBounds(
		int const TowerNum, // index to tower
		Real64 const Twb, // current inlet air wet-bulb temperature (C)
		Real64 const Tr, // requested range temperature for current time step (C)
		Real64 const Ta, // requested approach temperature for current time step (C)
		Real64 const WaterFlowRateRatio, // current water flow rate ratio at water inlet node
		Real64 & TwbCapped, // bounded value of inlet air wet-bulb temperature (C)
		Real64 & TrCapped, // bounded value of range temperature (C)
		Real64 & TaCapped, // bounded value of approach temperature (C)
		Real64 & WaterFlowRateRatioCapped // bounded value of water flow rate ratio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   Feb 2005
		//       MODIFIED       na
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// To verify that the empirical model's independent variables are within the limits used during the
		// developement of the empirical model.

		// METHODOLOGY EMPLOYED:
		// The empirical models used for simulating a variable speed cooling tower are based on a limited data set.
		// Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
		// The range of each independent variable is provided either by the CoolTools or York model limits, or
		// specified by the user if the model is User Defined (in either the CoolTools or York model format).
		// These limits are tested in this subroutine each time step and returned for use by the calling routine.
		// The independent variables capped here may or may not be passed to the empirical model in the calling
		// routine depending on their use.
		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::CurrentTime;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using General::CreateSysTimeIntervalString;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F5.2)'
		//  CHARACTER(len=*), PARAMETER :: OutputFormat2 ='(F8.5)'

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string OutputChar; // character string for warning messages
		static std::string OutputCharLo; // character string for warning messages
		static std::string OutputCharHi; // character string for warning messages
		static std::string TrimValue; // character string for warning messages
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		// current end time is compared with last to see if time step changed

		//   initialize capped variables in case independent variables are in bounds
		TwbCapped = Twb;
		TrCapped = Tr;
		TaCapped = Ta;
		WaterFlowRateRatioCapped = WaterFlowRateRatio;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).PrintTrMessage ) {
				++VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountTR;
				if ( VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountTR < 2 ) {
					ShowWarningError( VSTower( SimpleTower( TowerNum ).VSTower ).TrBuffer1 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).TrBuffer2 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).TrBuffer3 );
					ShowContinueError( " ...Range temperatures outside model boundaries may not adversely affect tower performance." );
					ShowContinueError( " ...This is not an unexpected occurrence when simulating actual conditions." );
				} else {
					ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Tower range temperature is out of range error continues...", VSTower( SimpleTower( TowerNum ).VSTower ).ErrIndexTR, VSTower( SimpleTower( TowerNum ).VSTower ).TrLast, VSTower( SimpleTower( TowerNum ).VSTower ).TrLast );
				}
			}
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).PrintTwbMessage ) {
				++VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountIAWB;
				if ( VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountIAWB < 6 ) {
					ShowWarningError( VSTower( SimpleTower( TowerNum ).VSTower ).TwbBuffer1 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).TwbBuffer2 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).TwbBuffer3 );
					ShowContinueError( " ...Wet-bulb temperatures outside model boundaries may not adversely affect tower performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Inlet air wet-bulb temperature is out of range error continues...", VSTower( SimpleTower( TowerNum ).VSTower ).ErrIndexIAWB, VSTower( SimpleTower( TowerNum ).VSTower ).TwbLast, VSTower( SimpleTower( TowerNum ).VSTower ).TwbLast );
				}
			}
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).PrintTaMessage ) {
				++VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountTA;
				if ( VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountTA < 2 ) {
					ShowWarningError( VSTower( SimpleTower( TowerNum ).VSTower ).TaBuffer1 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).TaBuffer2 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).TaBuffer3 );
					ShowContinueError( " ...Approach temperatures outside model boundaries may not adversely affect tower performance." );
					ShowContinueError( " ...This is not an unexpected occurrence when simulating actual conditions." );
				} else {
					ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Tower approach temperature is out of range error continues...", VSTower( SimpleTower( TowerNum ).VSTower ).ErrIndexTA, VSTower( SimpleTower( TowerNum ).VSTower ).TaLast, VSTower( SimpleTower( TowerNum ).VSTower ).TaLast );
				}
			}
			if ( VSTower( SimpleTower( TowerNum ).VSTower ).PrintWFRRMessage ) {
				++VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountWFRR;
				if ( VSTower( SimpleTower( TowerNum ).VSTower ).VSErrorCountWFRR < 6 ) {
					ShowWarningError( VSTower( SimpleTower( TowerNum ).VSTower ).WFRRBuffer1 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).WFRRBuffer2 );
					ShowContinueError( VSTower( SimpleTower( TowerNum ).VSTower ).WFRRBuffer3 );
					ShowContinueError( " ...Water flow rate ratios outside model boundaries may not adversely affect tower performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Water flow rate ratio is out of range error continues...", VSTower( SimpleTower( TowerNum ).VSTower ).ErrIndexWFRR, VSTower( SimpleTower( TowerNum ).VSTower ).WaterFlowRateRatioLast, VSTower( SimpleTower( TowerNum ).VSTower ).WaterFlowRateRatioLast );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
		if ( Twb < VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp || Twb > VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp ) {
			OutputChar = RoundSigDigits( Twb, 2 );
			OutputCharLo = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp, 2 );
			OutputCharHi = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp, 2 );
			if ( Twb < VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp ) {
				TwbCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MinInletAirWBTemp;
			}
			if ( Twb > VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp ) {
				TwbCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MaxInletAirWBTemp;
			}
			if ( ! WarmupFlag ) {
				VSTower( SimpleTower( TowerNum ).VSTower ).PrintTwbMessage = true;
				VSTower( SimpleTower( TowerNum ).VSTower ).TwbBuffer1 = SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Inlet air wet-bulb temperature is outside model boundaries at " + OutputChar + '.';
				VSTower( SimpleTower( TowerNum ).VSTower ).TwbBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				TrimValue = RoundSigDigits( TwbCapped, 6 );
				VSTower( SimpleTower( TowerNum ).VSTower ).TwbBuffer3 = " ...Inlet air wet-bulb temperature passed to the model = " + TrimValue;
				VSTower( SimpleTower( TowerNum ).VSTower ).TwbLast = Twb;
			} else {
				VSTower( SimpleTower( TowerNum ).VSTower ).PrintTwbMessage = false;
			}
		} else {
			VSTower( SimpleTower( TowerNum ).VSTower ).PrintTwbMessage = false;
		}

		if ( Tr < VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp || Tr > VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp ) {
			OutputChar = RoundSigDigits( Tr, 2 );
			OutputCharLo = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp, 2 );
			OutputCharHi = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp, 2 );
			if ( Tr < VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp ) {
				TrCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MinRangeTemp;
			}
			if ( Tr > VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp ) {
				TrCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MaxRangeTemp;
			}
			if ( ! WarmupFlag ) {
				VSTower( SimpleTower( TowerNum ).VSTower ).PrintTrMessage = true;
				VSTower( SimpleTower( TowerNum ).VSTower ).TrBuffer1 = SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Tower range temperature is outside model boundaries at " + OutputChar + '.';
				VSTower( SimpleTower( TowerNum ).VSTower ).TrBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				TrimValue = RoundSigDigits( Tr, 5 );
				VSTower( SimpleTower( TowerNum ).VSTower ).TrBuffer3 = " ...Tower range temperature passed to the model = " + TrimValue;
				VSTower( SimpleTower( TowerNum ).VSTower ).TrLast = Tr;
			} else {
				VSTower( SimpleTower( TowerNum ).VSTower ).PrintTrMessage = false;
			}
		} else {
			VSTower( SimpleTower( TowerNum ).VSTower ).PrintTrMessage = false;
		}

		if ( Ta < VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp || Ta > VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp ) {
			OutputChar = RoundSigDigits( Ta, 2 );
			OutputCharLo = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp, 2 );
			OutputCharHi = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp, 2 );
			if ( Ta < VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp ) {
				TaCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MinApproachTemp;
			}
			if ( Ta > VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp ) {
				TaCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MaxApproachTemp;
			}
			if ( ! WarmupFlag ) {
				VSTower( SimpleTower( TowerNum ).VSTower ).PrintTaMessage = true;
				VSTower( SimpleTower( TowerNum ).VSTower ).TaBuffer1 = SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Tower approach temperature is outside model boundaries at " + OutputChar + '.';
				VSTower( SimpleTower( TowerNum ).VSTower ).TaBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				TrimValue = RoundSigDigits( Ta, 5 );
				VSTower( SimpleTower( TowerNum ).VSTower ).TaBuffer3 = " ...Tower approach temperature passed to the model = " + TrimValue;
				VSTower( SimpleTower( TowerNum ).VSTower ).TaLast = Ta;
			} else {
				VSTower( SimpleTower( TowerNum ).VSTower ).PrintTaMessage = false;
			}
		} else {
			VSTower( SimpleTower( TowerNum ).VSTower ).PrintTaMessage = false;
		}

		if ( SimpleTower( TowerNum ).TowerModelType == YorkCalcModel || SimpleTower( TowerNum ).TowerModelType == YorkCalcUserDefined ) {
			//     Water flow rate ratio warning not valid for YorkCalc model, print liquid to gas ratio
			//     warning instead (bottom of Subroutine VariableSpeedTower)
			VSTower( SimpleTower( TowerNum ).VSTower ).PrintWFRRMessage = false;
		} else {
			if ( WaterFlowRateRatio < VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio || WaterFlowRateRatio > VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio ) {
				OutputChar = RoundSigDigits( WaterFlowRateRatio, 2 );
				OutputCharLo = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio, 2 );
				OutputCharHi = RoundSigDigits( VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio, 2 );
				if ( WaterFlowRateRatio < VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio ) {
					WaterFlowRateRatioCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MinWaterFlowRatio;
				}
				if ( WaterFlowRateRatio > VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio ) {
					WaterFlowRateRatioCapped = VSTower( SimpleTower( TowerNum ).VSTower ).MaxWaterFlowRatio;
				}
				if ( ! WarmupFlag ) {
					VSTower( SimpleTower( TowerNum ).VSTower ).PrintWFRRMessage = true;
					VSTower( SimpleTower( TowerNum ).VSTower ).WFRRBuffer1 = SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" - Water flow rate ratio is outside model boundaries at " + OutputChar + '.';
					VSTower( SimpleTower( TowerNum ).VSTower ).WFRRBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
					TrimValue = RoundSigDigits( WaterFlowRateRatioCapped, 5 );
					VSTower( SimpleTower( TowerNum ).VSTower ).WFRRBuffer3 = " ...Water flow rate ratio passed to the model = " + TrimValue;
					VSTower( SimpleTower( TowerNum ).VSTower ).WaterFlowRateRatioLast = WaterFlowRateRatio;
				} else {
					VSTower( SimpleTower( TowerNum ).VSTower ).PrintWFRRMessage = false;
				}
			} else {
				VSTower( SimpleTower( TowerNum ).VSTower ).PrintWFRRMessage = false;
			}
		}

	}

	Real64
	SimpleTowerUAResidual(
		Real64 const UA, // UA of cooling tower
		Array1< Real64 > const & Par // par(1) = design tower load [W]
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Design Tower Load - Tower Cooling Output) / Design Tower Load.
		// Tower Cooling Output depends on the UA which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Puts UA into the cooling tower data structure, calls SimSimpleTower, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = tower number
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
		int TowerIndex; // index of this tower
		Real64 OutWaterTemp; // outlet water temperature [C]
		Real64 CoolingOutput; // tower cooling output [W]

		TowerIndex = int( Par( 2 ) );
		SimSimpleTower( TowerIndex, Par( 3 ), Par( 4 ), UA, OutWaterTemp );
		CoolingOutput = Par( 5 ) * Par( 3 ) * ( SimpleTowerInlet( TowerIndex ).WaterTemp - OutWaterTemp );
		Residuum = ( Par( 1 ) - CoolingOutput ) / Par( 1 );
		return Residuum;
	}

	Real64
	SimpleTowerTrResidual(
		Real64 const Trange, // cooling tower range temperature [C]
		Array1< Real64 > const & Par // par(1) = tower number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Feb 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (where residual shows a balance point of model and desired performance)
		// Tower Approach depends on the range temperature which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Varies tower range temperature until a balance point exists where the model output corresponds
		// to the desired independent variables

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		Real64 AirFlowRateRatio; // ratio of water flow rate to design water flow rate

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = water flow ratio
		// par(3) = air flow ratio
		// par(4) = inlet air wet-bulb temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int TowerIndex; // index of this tower
		Real64 WaterFlowRateRatio; // ratio of water flow rate to design water flow rate
		Real64 InletAirWB; // inlet air wet-bulb temperature [C]
		Real64 Tapproach; // tower approach temperature [C]

		TowerIndex = int( Par( 1 ) );
		WaterFlowRateRatio = Par( 2 );
		AirFlowRateRatio = Par( 3 );
		InletAirWB = Par( 4 );
		Tapproach = 0.0;

		// call model to determine approach temperature given other independent variables (range temp is being varied to find balance)
		CalcVSTowerApproach( TowerIndex, WaterFlowRateRatio, AirFlowRateRatio, InletAirWB, Trange, Tapproach );
		// calculate residual based on a balance where Twb + Ta + Tr = Node(WaterInletNode)%Temp
		Residuum = ( InletAirWB + Tapproach + Trange ) - Node( SimpleTower( TowerIndex ).WaterInletNodeNum ).Temp;

		return Residuum;
	}

	Real64
	SimpleTowerApproachResidual(
		Real64 const FlowRatio, // water or air flow ratio of cooling tower
		Array1< Real64 > const & Par // par(1) = tower number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Feb 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Desired Approach - Model Approach Output)
		// Tower Approach depends on the water (or air) flow rate ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// In SizeTower, calibrates tower water flow rate ratio at an air flow rate ratio of 1.
		// In VariableSpeedTower, calculates air flow rate ratio at the inlet water flow rate ratio.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		Real64 AirFlowRateRatio; // ratio of water flow rate to design water flow rate

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = water or air flow ratio (opposite of input variable)
		// par(3) = inlet air wet-bulb temp [C]
		// par(4) = tower range [C]
		// par(5) = desired approach [C]
		// par(6) = 0.0 to calculate water flow rate ratio, 1.0 for air

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int TowerIndex; // index of this tower
		Real64 WaterFlowRateRatio; // ratio of water flow rate to design water flow rate
		Real64 InletAirWB; // inlet air wet-bulb temperature [C]
		Real64 Trange; // tower range temperature [C]
		Real64 TapproachActual; // actual tower approach temperature [C]
		Real64 TapproachDesired; // desired tower approach temperature [C]

		TowerIndex = int( Par( 1 ) );
		if ( Par( 6 ) == 0.0 ) {
			AirFlowRateRatio = Par( 2 );
			WaterFlowRateRatio = FlowRatio;
		} else {
			AirFlowRateRatio = FlowRatio;
			WaterFlowRateRatio = Par( 2 );
		}
		InletAirWB = Par( 3 );
		Trange = Par( 4 );
		TapproachDesired = Par( 5 );
		TapproachActual = 0.0;

		// call model to determine tower approach temperature given other independent variables
		CalcVSTowerApproach( TowerIndex, WaterFlowRateRatio, AirFlowRateRatio, InletAirWB, Trange, TapproachActual );
		Residuum = TapproachDesired - TapproachActual;

		return Residuum;
	}

	// End of the CondenserLoopTowers Module Simulation Subroutines

	// *****************************************************************************

	void
	CalculateWaterUseage( int const TowerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2006
		//       MODIFIED       T Hong, Aug. 2008. Added fluid bypass for single speed cooling tower
		//                      A Flament, July 2010. Added multi-cell capability
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Collect tower water useage calculations for
		// reuse by all the tower models.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Code for this routine started from VariableSpeedTower

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
		if ( SimpleTower( TowerNum ).EvapLossMode == EvapLossByMoistTheory ) {

			AirDensity = PsyRhoAirFnPbTdbW( SimpleTowerInlet( TowerNum ).AirPress, SimpleTowerInlet( TowerNum ).AirTemp, SimpleTowerInlet( TowerNum ).AirHumRat );
			AirMassFlowRate = AirFlowRateRatio * SimpleTower( TowerNum ).HighSpeedAirFlowRate * AirDensity * SimpleTower( TowerNum ).NumCellOn / SimpleTower( TowerNum ).NumCell;
			InletAirEnthalpy = PsyHFnTdbRhPb( SimpleTowerInlet( TowerNum ).AirWetBulb, 1.0, SimpleTowerInlet( TowerNum ).AirPress );

			if ( AirMassFlowRate > 0.0 ) {
				// Calculate outlet air conditions for determining water usage

				OutletAirEnthalpy = InletAirEnthalpy + Qactual / AirMassFlowRate;
				OutletAirTSat = PsyTsatFnHPb( OutletAirEnthalpy, SimpleTowerInlet( TowerNum ).AirPress );
				OutletAirHumRatSat = PsyWFnTdbH( OutletAirTSat, OutletAirEnthalpy );

				// calculate specific humidity ratios (HUMRAT to mass of moist air not dry air)
				InSpecificHumRat = SimpleTowerInlet( TowerNum ).AirHumRat / ( 1 + SimpleTowerInlet( TowerNum ).AirHumRat );
				OutSpecificHumRat = OutletAirHumRatSat / ( 1 + OutletAirHumRatSat );

				// calculate average air temp for density call
				TairAvg = ( SimpleTowerInlet( TowerNum ).AirTemp + OutletAirTSat ) / 2.0;

				// Amount of water evaporated, get density water at air temp or 4 C if too cold
				rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, max( TairAvg, 4.0 ), PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

				EvapVdot = ( AirMassFlowRate * ( OutSpecificHumRat - InSpecificHumRat ) ) / rho; // [m3/s]
				if ( EvapVdot < 0.0 ) EvapVdot = 0.0;
			} else {
				EvapVdot = 0.0;
			}

		} else if ( SimpleTower( TowerNum ).EvapLossMode == EvapLossByUserFactor ) {
			//    EvapVdot   = SimpleTower(TowerNum)%UserEvapLossFactor * (InletWaterTemp - OutletWaterTemp) &
			//                     * SimpleTower(TowerNum)%DesignWaterFlowRate
			rho = GetDensityGlycol( PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidName, AverageWaterTemp, PlantLoop( SimpleTower( TowerNum ).LoopNum ).FluidIndex, RoutineName );

			EvapVdot = SimpleTower( TowerNum ).UserEvapLossFactor * ( InletWaterTemp - OutletWaterTemp ) * ( WaterMassFlowRate / rho );
			if ( EvapVdot < 0.0 ) EvapVdot = 0.0;
		} else {
			// should never come here
		}

		//   amount of water lost due to drift
		DriftVdot = SimpleTower( TowerNum ).DesignWaterFlowRate * SimpleTower( TowerNum ).NumCellOn / SimpleTower( TowerNum ).NumCell * SimpleTower( TowerNum ).DriftLossFraction * AirFlowRateRatio;

		if ( SimpleTower( TowerNum ).BlowdownMode == BlowdownBySchedule ) {
			// Amount of water lost due to blow down (purging contaminants from tower basin)
			if ( SimpleTower( TowerNum ).SchedIDBlowdown > 0 ) {
				BlowDownVdot = GetCurrentScheduleValue( SimpleTower( TowerNum ).SchedIDBlowdown );
			} else {
				BlowDownVdot = 0.0;
			}
		} else if ( SimpleTower( TowerNum ).BlowdownMode == BlowdownByConcentration ) {
			if ( SimpleTower( TowerNum ).ConcentrationRatio > 2.0 ) { // protect divide by zero
				BlowDownVdot = EvapVdot / ( SimpleTower( TowerNum ).ConcentrationRatio - 1 ) - DriftVdot;
			} else {
				BlowDownVdot = EvapVdot - DriftVdot;
			}
			if ( BlowDownVdot < 0.0 ) BlowDownVdot = 0.0;
		} else {
			//should never come here
		}

		// Added for fluid bypass
		if ( SimpleTower( TowerNum ).CapacityControl == CapacityControl_FluidBypass ) {
			if ( SimpleTower( TowerNum ).EvapLossMode == EvapLossByUserFactor ) EvapVdot *= ( 1 - SimpleTower( TowerNum ).BypassFraction );
			DriftVdot *= ( 1 - SimpleTower( TowerNum ).BypassFraction );
			BlowDownVdot *= ( 1 - SimpleTower( TowerNum ).BypassFraction );
		}

		MakeUpVdot = EvapVdot + DriftVdot + BlowDownVdot;

		// set demand request in Water STorage if needed
		StarvedVdot = 0.0;
		TankSupplyVdot = 0.0;
		if ( SimpleTower( TowerNum ).SuppliedByWaterSystem ) {

			// set demand request
			WaterStorage( SimpleTower( TowerNum ).WaterTankID ).VdotRequestDemand( SimpleTower( TowerNum ).WaterTankDemandARRID ) = MakeUpVdot;

			AvailTankVdot = WaterStorage( SimpleTower( TowerNum ).WaterTankID ).VdotAvailDemand( SimpleTower( TowerNum ).WaterTankDemandARRID ); // check what tank can currently provide

			TankSupplyVdot = MakeUpVdot; // init
			if ( AvailTankVdot < MakeUpVdot ) { // calculate starved flow
				StarvedVdot = MakeUpVdot - AvailTankVdot;
				TankSupplyVdot = AvailTankVdot;
			}
		} else { // supplied by mains

		}

		//   total water usage
		// update report variables
		SimpleTowerReport( TowerNum ).EvaporationVdot = EvapVdot;
		SimpleTowerReport( TowerNum ).EvaporationVol = EvapVdot * ( TimeStepSys * SecInHour );
		SimpleTowerReport( TowerNum ).DriftVdot = DriftVdot;
		SimpleTowerReport( TowerNum ).DriftVol = DriftVdot * ( TimeStepSys * SecInHour );
		SimpleTowerReport( TowerNum ).BlowdownVdot = BlowDownVdot;
		SimpleTowerReport( TowerNum ).BlowdownVol = BlowDownVdot * ( TimeStepSys * SecInHour );
		SimpleTowerReport( TowerNum ).MakeUpVdot = MakeUpVdot;
		SimpleTowerReport( TowerNum ).MakeUpVol = MakeUpVdot * ( TimeStepSys * SecInHour );
		SimpleTowerReport( TowerNum ).TankSupplyVdot = TankSupplyVdot;
		SimpleTowerReport( TowerNum ).TankSupplyVol = TankSupplyVdot * ( TimeStepSys * SecInHour );
		SimpleTowerReport( TowerNum ).StarvedMakeUpVdot = StarvedVdot;
		SimpleTowerReport( TowerNum ).StarvedMakeUpVol = StarvedVdot * ( TimeStepSys * SecInHour );

	}

	// Beginning of Record Keeping subroutines for the Tower Module
	// *****************************************************************************

	void
	UpdateTowers( int const TowerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for passing results to the outlet water node.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using General::TrimSigDigits;
		using DataPlant::PlantLoop;
		using DataBranchAirLoopPlant::MassFlowTolerance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt LowTempFmt( "(' ',F6.2)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CharErrOut;
		std::string CharLowOutletTemp;
		int LoopNum;
		int LoopSideNum;
		Real64 LoopMinTemp;
		// set node information

		Node( WaterOutletNode ).Temp = OutletWaterTemp;

		LoopNum = SimpleTower( TowerNum ).LoopNum;
		LoopSideNum = SimpleTower( TowerNum ).LoopSideNum;
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 || WarmupFlag ) return;

		//Check flow rate through tower and compare to design flow rate, show warning if greater than Design * Mulitplier
		if ( Node( WaterOutletNode ).MassFlowRate > SimpleTower( TowerNum ).DesWaterMassFlowRate * SimpleTower( TowerNum ).TowerMassFlowRateMultiplier ) {
			++SimpleTower( TowerNum ).HighMassFlowErrorCount;
			if ( SimpleTower( TowerNum ).HighMassFlowErrorCount < 2 ) {
				ShowWarningError( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\"" );
				ShowContinueError( " Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate." );
				ShowContinueError( " Condenser Loop Mass Flow Rate = " + TrimSigDigits( Node( WaterOutletNode ).MassFlowRate, 6 ) );
				ShowContinueError( " Tower Design Mass Flow Rate   = " + TrimSigDigits( SimpleTower( TowerNum ).DesWaterMassFlowRate, 6 ) );
				ShowContinueErrorTimeStamp( "" );
			} else {
				ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\"  Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate error continues...", SimpleTower( TowerNum ).HighMassFlowErrorIndex, Node( WaterOutletNode ).MassFlowRate, Node( WaterOutletNode ).MassFlowRate );
			}
		}

		// Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
		LoopMinTemp = PlantLoop( LoopNum ).MinTemp;
		if ( OutletWaterTemp < LoopMinTemp && WaterMassFlowRate > 0.0 ) {
			++SimpleTower( TowerNum ).OutletWaterTempErrorCount;
			gio::write( CharLowOutletTemp, LowTempFmt ) << LoopMinTemp;
			gio::write( CharErrOut, LowTempFmt ) << OutletWaterTemp;
			strip( CharErrOut );
			if ( SimpleTower( TowerNum ).OutletWaterTempErrorCount < 2 ) {
				ShowWarningError( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\"" );
				ShowContinueError( "Cooling tower water outlet temperature (" + CharErrOut + " C) is below the specified minimum condenser loop temp of " + stripped( CharLowOutletTemp ) + " C" );
				ShowContinueErrorTimeStamp( "" );
			} else {
				ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\" Cooling tower water outlet temperature is below the specified minimum condenser loop temp error continues...", SimpleTower( TowerNum ).OutletWaterTempErrorIndex, OutletWaterTemp, OutletWaterTemp );
			}
		}

		// Check if water mass flow rate is small (e.g. no flow) and warn user
		if ( WaterMassFlowRate > 0.0 && WaterMassFlowRate <= MassFlowTolerance ) {
			++SimpleTower( TowerNum ).SmallWaterMassFlowErrorCount;
			if ( SimpleTower( TowerNum ).SmallWaterMassFlowErrorCount < 2 ) {
				ShowWarningError( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\"" );
				ShowContinueError( "Cooling tower water mass flow rate near zero." );
				ShowContinueErrorTimeStamp( "" );
				ShowContinueError( "Actual Mass flow = " + TrimSigDigits( WaterMassFlowRate, 2 ) );
			} else {
				ShowRecurringWarningErrorAtEnd( SimpleTower( TowerNum ).TowerType + " \"" + SimpleTower( TowerNum ).Name + "\"  Cooling tower water mass flow rate near zero error continues...", SimpleTower( TowerNum ).SmallWaterMassFlowErrorIndex, WaterMassFlowRate, WaterMassFlowRate );
			}
		}

		// Check if water mass flow rate is lower than loop minimum and warn user
		//   IF(WaterMassFlowRate .LT. LoopMassFlowRateMinAvail)THEN
		//     SimpleTower(TowerNum)%WMFRLessThanMinAvailErrCount = SimpleTower(TowerNum)%WMFRLessThanMinAvailErrCount + 1
		//     IF (SimpleTower(TowerNum)%WMFRLessThanMinAvailErrCount < 2) THEN
		//       CALL ShowWarningError (TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//'"')
		//       CALL ShowContinueError ('Cooling tower water mass flow below loop minimum.')
		//       CALL ShowContinueErrorTimeStamp(' ')
		//       CALL ShowContinueError('Actual Mass flow  = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
		//       CALL ShowContinueError('Loop Minimum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMinAvail,2)))
		//     ELSE
		//       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
		//          '" Cooling tower water mass flow rate below loop minimum error continues...' &
		//          , SimpleTower(TowerNum)%WMFRLessThanMinAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
		//     ENDIF
		//   END IF

		// Check if water mass flow rate is greater than loop maximum and warn user
		//   IF(WaterMassFlowRate .GT. LoopMassFlowRateMaxAvail)THEN
		//     SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrCount = SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrCount + 1
		//     IF (SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrCount < 2) THEN
		//       CALL ShowWarningError (TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//'"')
		//       CALL ShowContinueError ('Cooling Tower water mass flow above loop maximum.')
		//       CALL ShowContinueErrorTimeStamp(' ')
		//       CALL ShowContinueError('Actual Mass flow='//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
		//       CALL ShowContinueError('Loop Maximum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMaxAvail,2)))
		//     ELSE
		//       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
		//          '" Cooling tower water mass flow rate above loop maximum error continues...' &
		//          , SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
		//     ENDIF
		//   END IF

	}

	// End of Record Keeping subroutines for the Tower Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Tower Module
	// *****************************************************************************

	void
	ReportTowers(
		bool const RunFlag,
		int const TowerNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variables for the tower.

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
			SimpleTowerReport( TowerNum ).InletWaterTemp = Node( WaterInletNode ).Temp;
			SimpleTowerReport( TowerNum ).OutletWaterTemp = Node( WaterInletNode ).Temp;
			SimpleTowerReport( TowerNum ).WaterMassFlowRate = WaterMassFlowRate;
			SimpleTowerReport( TowerNum ).Qactual = 0.0;
			SimpleTowerReport( TowerNum ).FanPower = 0.0;
			SimpleTowerReport( TowerNum ).FanEnergy = 0.0;
			SimpleTowerReport( TowerNum ).AirFlowRatio = 0.0;
			SimpleTowerReport( TowerNum ).WaterAmountUsed = 0.0;
			SimpleTowerReport( TowerNum ).BasinHeaterPower = BasinHeaterPower;
			SimpleTowerReport( TowerNum ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			SimpleTowerReport( TowerNum ).FanCyclingRatio = 0.0;
			SimpleTowerReport( TowerNum ).BypassFraction = 0.0; // added for fluid bypass
			SimpleTowerReport( TowerNum ).NumCellOn = 0;
			SimpleTowerReport( TowerNum ).SpeedSelected = 0;
		} else {
			SimpleTowerReport( TowerNum ).InletWaterTemp = Node( WaterInletNode ).Temp;
			SimpleTowerReport( TowerNum ).OutletWaterTemp = OutletWaterTemp;
			SimpleTowerReport( TowerNum ).WaterMassFlowRate = WaterMassFlowRate;
			SimpleTowerReport( TowerNum ).Qactual = Qactual;
			SimpleTowerReport( TowerNum ).FanPower = CTFanPower;
			SimpleTowerReport( TowerNum ).FanEnergy = CTFanPower * ReportingConstant;
			SimpleTowerReport( TowerNum ).AirFlowRatio = AirFlowRateRatio;
			SimpleTowerReport( TowerNum ).WaterAmountUsed = WaterUsage * ReportingConstant;
			SimpleTowerReport( TowerNum ).BasinHeaterPower = BasinHeaterPower;
			SimpleTowerReport( TowerNum ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			SimpleTowerReport( TowerNum ).FanCyclingRatio = FanCyclingRatio;
			SimpleTowerReport( TowerNum ).BypassFraction = SimpleTower( TowerNum ).BypassFraction; // added for fluid bypass
			SimpleTowerReport( TowerNum ).NumCellOn = SimpleTower( TowerNum ).NumCellOn;
			SimpleTowerReport( TowerNum ).SpeedSelected = SimpleTower( TowerNum ).SpeedSelected;
		}

	}

} // CondenserLoopTowers

} // EnergyPlus
