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
#include <PlantComponent.hh>
#include <PlantLocation.hh>
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
//	bool InitTowerOneTimeFlag( true );
	Array1D_bool CheckEquipName;

	//? The following block of variables are used to carry model results for a tower instance
	//   across sim, update, and report routines.  Simulation manager must be careful
	//   in models with multiple towers.

	Real64 nsvInletWaterTemp( 0.0 ); // CW temperature at tower inlet
	Real64 nsvOutletWaterTemp( 0.0 ); // CW temperature at tower outlet
	int nsvWaterInletNode( 0 ); // Node number at tower inlet
	int nsvWaterOutletNode( 0 ); // Node number at tower outlet
	Real64 nsvWaterMassFlowRate( 0.0 ); // WaterMassFlowRate through tower

	Real64 nsvQactual( 0.0 ); // Tower heat transfer
	Real64 nsvCTFanPower( 0.0 ); // Tower fan power used
	Real64 nsvAirFlowRateRatio( 0.0 ); // Ratio of air flow rate through VS cooling tower to design air flow rate
	Real64 nsvBasinHeaterPower( 0.0 ); // Basin heater power use (W)
	Real64 nsvWaterUsage( 0.0 ); // Tower water usage (m3/s)
	Real64 nsvFanCyclingRatio( 0.0 ); // cycling ratio of tower fan when min fan speed provide to much capacity

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserLoopTowers

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Object Data
	Array1D< Towerspecs > SimpleTower; // dimension to number of machines
//	Array1D< ReportVars > SimpleTowerReport; // report variables
//	Array1D< VSTowerData > VSTower; // model coefficients and specific variables for VS tower

	// MODULE SUBROUTINES:

	// Beginning of CondenserLoopTowers Module Driver Subroutines
	//*************************************************************************

	// Functions
	void
	clear_state()
	{
		NumSimpleTowers = 0;
		GetInput = true;
//		InitTowerOneTimeFlag = true;
		nsvInletWaterTemp = 0.0;
		nsvOutletWaterTemp = 0.0;
		nsvWaterInletNode = 0;
		nsvWaterOutletNode = 0;
		nsvWaterMassFlowRate = 0.0;
		nsvQactual = 0.0;
		nsvCTFanPower = 0.0;
		nsvAirFlowRateRatio = 0.0;
		nsvBasinHeaterPower = 0.0;
		nsvWaterUsage = 0.0;
		nsvFanCyclingRatio = 0.0;
		CheckEquipName.deallocate();
		SimpleTower.deallocate();
//		SimpleTowerReport.deallocate();
//		VSTower.deallocate();
	}

	PlantComponent * Towerspecs::factory( int objectType, std::string objectName ) {
		// Process the input data for cooling towers if it hasn't been done already
		if( GetInput ) {
			GetTowerInput();
			GetInput = false;
		}
		// Now look for this particular pipe in the list
		for( auto & tower : SimpleTower ) {
			if( tower.Tower_TypeOf == objectType && tower.Name == objectName ) {
				return &tower;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "CoolingTowerDataFactory: Error getting inputs for tower named: " + objectName );
		// Shut up the compiler
		return nullptr;
	}

	void Towerspecs::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ), bool const EP_UNUSED( FirstHVACIteration ), Real64 & CurLoad ) {

		//INITIALIZE
		InitSimVars();

		//CALCULATE

		this->InitTower();

		if( this->TowerType_Num == CoolingTower_SingleSpeed ) {

			this->CalcSingleSpeedTower();

		} else if( this->TowerType_Num == CoolingTower_TwoSpeed ) {

			this->CalcTwoSpeedTower();

		} else if( this->TowerType_Num == CoolingTower_VariableSpeedMerkel ) {

			this->CalcMerkelVariableSpeedTower( CurLoad );

		} else if( this->TowerType_Num == CoolingTower_VariableSpeed ) {

			this->CalcVariableSpeedTower();

		} else {
			ShowFatalError( "SimTowers: Invalid Tower Type Requested=" + TowerType );

		} // TypeOfEquip

		this->CalculateWaterUseage();
		this->UpdateTowers();
		this->ReportTowers( CurLoad );


	}

	void Towerspecs::getDesignCapacities( const PlantLocation & EP_UNUSED( calledFromLocation ), Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad )
	{

		if ( this->TowerType_Num == CoolingTower_VariableSpeedMerkel ) {
			this->SizeVSMerkelTower();
		} else {
			this->SizeTower();
		}

		MinLoad = 0.0;
		MaxLoad = this->TowerNominalCapacity * this->HeatRejectCapNomCapSizingRatio;
		OptLoad = this->TowerNominalCapacity;

	}

	void Towerspecs::onInitLoopEquip( const PlantLocation & EP_UNUSED( calledFromLocation ) )
	{

		//INITIALIZE
		InitSimVars();

		InitTower();

	}

	void Towerspecs::getSizingFactor( Real64 & SizFac ) {

		SizFac = this->SizFac;

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
		using DataPlant::TypeOf_CoolingTower_SingleSpd;
		using DataPlant::TypeOf_CoolingTower_TwoSpd;
		using DataPlant::TypeOf_CoolingTower_VarSpd;
		using DataPlant::TypeOf_CoolingTower_VarSpdMerkel;

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
		CheckEquipName.dimension( NumSimpleTowers, true );
		// Allocate variable-speed tower structure with data specific to this type
		if ( NumVariableSpeedTowers > 0 ) {
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
			SimpleTower( TowerNum ).Tower_TypeOf = TypeOf_CoolingTower_SingleSpd;
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
			SimpleTower( TowerNum ).Tower_TypeOf = TypeOf_CoolingTower_TwoSpd;
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
			SimpleTower( TowerNum ).Tower_TypeOf = TypeOf_CoolingTower_VarSpd;
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

			SimpleTower( VariableSpeedTowerNumber ).Coeff.allocate( 35 );
			SimpleTower( VariableSpeedTowerNumber ).Coeff = 0.0;

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
				SimpleTower( TowerNum ).Coeff( 1 ) = 0.52049709836241;
				SimpleTower( TowerNum ).Coeff( 2 ) = -10.617046395344;
				SimpleTower( TowerNum ).Coeff( 3 ) = 10.7292974722538;
				SimpleTower( TowerNum ).Coeff( 4 ) = -2.74988377158227;
				SimpleTower( TowerNum ).Coeff( 5 ) = 4.73629943913743;
				SimpleTower( TowerNum ).Coeff( 6 ) = -8.25759700874711;
				SimpleTower( TowerNum ).Coeff( 7 ) = 1.57640938114136;
				SimpleTower( TowerNum ).Coeff( 8 ) = 6.51119643791324;
				SimpleTower( TowerNum ).Coeff( 9 ) = 1.50433525206692;
				SimpleTower( TowerNum ).Coeff( 10 ) = -3.2888529287801;
				SimpleTower( TowerNum ).Coeff( 11 ) = 0.0257786145353773;
				SimpleTower( TowerNum ).Coeff( 12 ) = 0.182464289315254;
				SimpleTower( TowerNum ).Coeff( 13 ) = -0.0818947291400898;
				SimpleTower( TowerNum ).Coeff( 14 ) = -0.215010003996285;
				SimpleTower( TowerNum ).Coeff( 15 ) = 0.0186741309635284;
				SimpleTower( TowerNum ).Coeff( 16 ) = 0.0536824177590012;
				SimpleTower( TowerNum ).Coeff( 17 ) = -0.00270968955115031;
				SimpleTower( TowerNum ).Coeff( 18 ) = 0.00112277498589279;
				SimpleTower( TowerNum ).Coeff( 19 ) = -0.00127758497497718;
				SimpleTower( TowerNum ).Coeff( 20 ) = 0.0000760420796601607;
				SimpleTower( TowerNum ).Coeff( 21 ) = 1.43600088336017;
				SimpleTower( TowerNum ).Coeff( 22 ) = -0.5198695909109;
				SimpleTower( TowerNum ).Coeff( 23 ) = 0.117339576910507;
				SimpleTower( TowerNum ).Coeff( 24 ) = 1.50492810819924;
				SimpleTower( TowerNum ).Coeff( 25 ) = -0.135898905926974;
				SimpleTower( TowerNum ).Coeff( 26 ) = -0.152577581866506;
				SimpleTower( TowerNum ).Coeff( 27 ) = -0.0533843828114562;
				SimpleTower( TowerNum ).Coeff( 28 ) = 0.00493294869565511;
				SimpleTower( TowerNum ).Coeff( 29 ) = -0.00796260394174197;
				SimpleTower( TowerNum ).Coeff( 30 ) = 0.000222619828621544;
				SimpleTower( TowerNum ).Coeff( 31 ) = -0.0543952001568055;
				SimpleTower( TowerNum ).Coeff( 32 ) = 0.00474266879161693;
				SimpleTower( TowerNum ).Coeff( 33 ) = -0.0185854671815598;
				SimpleTower( TowerNum ).Coeff( 34 ) = 0.00115667701293848;
				SimpleTower( TowerNum ).Coeff( 35 ) = 0.000807370664460284;

				//       set minimum and maximum boundaries for CoolTools crossflow model input variables
				SimpleTower( TowerNum ).MinInletAirWBTemp = -1.0;
				SimpleTower( TowerNum ).MaxInletAirWBTemp = 26.6667;
				SimpleTower( TowerNum ).MinRangeTemp = 1.1111;
				SimpleTower( TowerNum ).MaxRangeTemp = 11.1111;
				SimpleTower( TowerNum ).MinApproachTemp = 1.1111;
				SimpleTower( TowerNum ).MaxApproachTemp = 11.1111;
				SimpleTower( TowerNum ).MinWaterFlowRatio = 0.75;
				SimpleTower( TowerNum ).MaxWaterFlowRatio = 1.25;

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
				SimpleTower( TowerNum ).Coeff( 1 ) = -0.359741205;
				SimpleTower( TowerNum ).Coeff( 2 ) = -0.055053608;
				SimpleTower( TowerNum ).Coeff( 3 ) = 0.0023850432;
				SimpleTower( TowerNum ).Coeff( 4 ) = 0.173926877;
				SimpleTower( TowerNum ).Coeff( 5 ) = -0.0248473764;
				SimpleTower( TowerNum ).Coeff( 6 ) = 0.00048430224;
				SimpleTower( TowerNum ).Coeff( 7 ) = -0.005589849456;
				SimpleTower( TowerNum ).Coeff( 8 ) = 0.0005770079712;
				SimpleTower( TowerNum ).Coeff( 9 ) = -0.00001342427256;
				SimpleTower( TowerNum ).Coeff( 10 ) = 2.84765801111111;
				SimpleTower( TowerNum ).Coeff( 11 ) = -0.121765149;
				SimpleTower( TowerNum ).Coeff( 12 ) = 0.0014599242;
				SimpleTower( TowerNum ).Coeff( 13 ) = 1.680428651;
				SimpleTower( TowerNum ).Coeff( 14 ) = -0.0166920786;
				SimpleTower( TowerNum ).Coeff( 15 ) = -0.0007190532;
				SimpleTower( TowerNum ).Coeff( 16 ) = -0.025485194448;
				SimpleTower( TowerNum ).Coeff( 17 ) = 0.0000487491696;
				SimpleTower( TowerNum ).Coeff( 18 ) = 0.00002719234152;
				SimpleTower( TowerNum ).Coeff( 19 ) = -0.0653766255555556;
				SimpleTower( TowerNum ).Coeff( 20 ) = -0.002278167;
				SimpleTower( TowerNum ).Coeff( 21 ) = 0.0002500254;
				SimpleTower( TowerNum ).Coeff( 22 ) = -0.0910565458;
				SimpleTower( TowerNum ).Coeff( 23 ) = 0.00318176316;
				SimpleTower( TowerNum ).Coeff( 24 ) = 0.000038621772;
				SimpleTower( TowerNum ).Coeff( 25 ) = -0.0034285382352;
				SimpleTower( TowerNum ).Coeff( 26 ) = 0.00000856589904;
				SimpleTower( TowerNum ).Coeff( 27 ) = -0.000001516821552;

				//       set minimum and maximum boundaries for YorkCalc model input variables
				SimpleTower( TowerNum ).MinInletAirWBTemp = -34.4;
				SimpleTower( TowerNum ).MaxInletAirWBTemp = 29.4444;
				SimpleTower( TowerNum ).MinRangeTemp = 1.1111;
				SimpleTower( TowerNum ).MaxRangeTemp = 22.2222;
				SimpleTower( TowerNum ).MinApproachTemp = 1.1111;
				SimpleTower( TowerNum ).MaxApproachTemp = 40.0;
				SimpleTower( TowerNum ).MinWaterFlowRatio = 0.75;
				SimpleTower( TowerNum ).MaxWaterFlowRatio = 1.25;
				SimpleTower( TowerNum ).MaxLiquidToGasRatio = 8.0;

			} else if ( SameString( AlphArray( 4 ), "CoolToolsUserDefined" ) ) {
				SimpleTower( TowerNum ).TowerModelType = CoolToolsUserDefined;
				// Nested Get-input routines below.  Should pull out of here and read in beforehand.
				for ( VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSCoolToolsModelCoeffs; ++VSModelCoeffNum ) {
					GetObjectItem( "CoolingTowerPerformance:CoolTools", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat );
					if ( ! SameString( AlphArray2( 1 ), SimpleTower( TowerNum ).ModelCoeffObjectName ) ) continue;
					SimpleTower( TowerNum ).FoundModelCoeff = true;
					// verify the correct number of coefficients for the CoolTools model
					if ( NumNums2 != 43 ) {
						ShowSevereError( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\". The number of numeric inputs for object CoolingTowerPerformance:CoolTools \"" + SimpleTower( TowerNum ).ModelCoeffObjectName + "\" must equal 43." );
						ErrorsFound = true;
					} else {

						SimpleTower( TowerNum ).MinInletAirWBTemp = NumArray2( 1 );
						SimpleTower( TowerNum ).MaxInletAirWBTemp = NumArray2( 2 );
						SimpleTower( TowerNum ).MinRangeTemp = NumArray2( 3 );
						SimpleTower( TowerNum ).MaxRangeTemp = NumArray2( 4 );
						SimpleTower( TowerNum ).MinApproachTemp = NumArray2( 5 );
						SimpleTower( TowerNum ).MaxApproachTemp = NumArray2( 6 );
						SimpleTower( TowerNum ).MinWaterFlowRatio = NumArray2( 7 );
						SimpleTower( TowerNum ).MaxWaterFlowRatio = NumArray2( 8 );

						for ( CoeffNum = 9; CoeffNum <= NumNums2; ++CoeffNum ) {
							SimpleTower( TowerNum ).Coeff( CoeffNum - 8 ) = NumArray2( CoeffNum );
						}
					}
					break;
				}
				if ( ! SimpleTower( TowerNum ).FoundModelCoeff ) {
					ShowSevereError( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\". User defined name for variable speed cooling tower model coefficients object not found = " + SimpleTower( TowerNum ).ModelCoeffObjectName );
					ErrorsFound = true;
				}
			} else if ( SameString( AlphArray( 4 ), "YorkCalcUserDefined" ) ) {
				SimpleTower( TowerNum ).TowerModelType = YorkCalcUserDefined;
				// Nested Get-input routines below.  Should pull out of here and read in beforehand.
				for ( VSModelCoeffNum = 1; VSModelCoeffNum <= NumVSYorkCalcModelCoeffs; ++VSModelCoeffNum ) {
					GetObjectItem( "CoolingTowerPerformance:YorkCalc", VSModelCoeffNum, AlphArray2, NumAlphas2, NumArray2, NumNums2, IOStat );
					if ( ! SameString( AlphArray2( 1 ), SimpleTower( TowerNum ).ModelCoeffObjectName ) ) continue;
					SimpleTower( TowerNum ).FoundModelCoeff = true;
					// verify the correct number of coefficients for the YorkCalc model
					if ( NumNums2 != 36 ) {
						ShowSevereError( "CoolingTower:VariableSpeed \"" + SimpleTower( TowerNum ).Name + "\". The number of numeric inputs for object CoolingTowerPerformance:YorkCalc \"" + SimpleTower( TowerNum ).ModelCoeffObjectName + "\" must equal 36." );
						ErrorsFound = true;
					} else {

						SimpleTower( TowerNum ).MinInletAirWBTemp = NumArray2( 1 );
						SimpleTower( TowerNum ).MaxInletAirWBTemp = NumArray2( 2 );
						SimpleTower( TowerNum ).MinRangeTemp = NumArray2( 3 );
						SimpleTower( TowerNum ).MaxRangeTemp = NumArray2( 4 );
						SimpleTower( TowerNum ).MinApproachTemp = NumArray2( 5 );
						SimpleTower( TowerNum ).MaxApproachTemp = NumArray2( 6 );
						SimpleTower( TowerNum ).MinWaterFlowRatio = NumArray2( 7 );
						SimpleTower( TowerNum ).MaxWaterFlowRatio = NumArray2( 8 );
						SimpleTower( TowerNum ).MaxLiquidToGasRatio = NumArray2( 9 );

						for ( CoeffNum = 10; CoeffNum <= NumNums2; ++CoeffNum ) {
							SimpleTower( TowerNum ).Coeff( CoeffNum - 9 ) = NumArray2( CoeffNum );
						}
					}
					break;
				}

				if ( ! SimpleTower( TowerNum ).FoundModelCoeff ) {
					ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined name for variable speed cooling tower model coefficients object not found = " + SimpleTower( TowerNum ).ModelCoeffObjectName );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". Illegal Tower Model Type = " + AlphArray( 5 ) );
				ShowContinueError( " Tower Model Type must be \"CoolToolsCrossFlow\", \"YorkCalc\", \"CoolToolsUserDefined\", or \"YorkCalcUserDefined." );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).TowerMassFlowRateMultiplier = SimpleTower( TowerNum ).MaxWaterFlowRatio;

			//   check user defined minimums to be greater than 0
			if ( SimpleTower( TowerNum ).MinApproachTemp < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined minimum approach temperature must be > 0" );
				ErrorsFound = true;
			}
			if ( SimpleTower( TowerNum ).MinRangeTemp < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined minimum range temperature must be > 0" );
				ErrorsFound = true;
			}
			if ( SimpleTower( TowerNum ).MinWaterFlowRatio < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined minimum water flow rate ratio must be > 0" );
				ErrorsFound = true;
			}

			//   check that the user defined maximums are greater than the minimums
			if ( SimpleTower( TowerNum ).MaxApproachTemp < SimpleTower( TowerNum ).MinApproachTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined maximum approach temperature must be > the minimum approach temperature" );
				ErrorsFound = true;
			}
			if ( SimpleTower( TowerNum ).MaxRangeTemp < SimpleTower( TowerNum ).MinRangeTemp ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined maximum range temperature must be > the minimum range temperature" );
				ErrorsFound = true;
			}
			if ( SimpleTower( TowerNum ).MaxWaterFlowRatio < SimpleTower( TowerNum ).MinWaterFlowRatio ) {
				ShowSevereError( cCurrentModuleObject + " \"" + SimpleTower( TowerNum ).Name + "\". User defined maximum water flow rate ratio must be > the minimum water flow rate ratio" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).DesignInletWB = NumArray( 1 );
			if ( NumArray( 1 ) < SimpleTower( TowerNum ).MinInletAirWBTemp || NumArray( 1 ) > SimpleTower( TowerNum ).MaxInletAirWBTemp ) {
				gio::write( OutputChar, OutputFormat ) << SimpleTower( TowerNum ).DesignInletWB;
				gio::write( OutputCharLo, OutputFormat ) << SimpleTower( TowerNum ).MinInletAirWBTemp;
				gio::write( OutputCharHi, OutputFormat ) << SimpleTower( TowerNum ).MaxInletAirWBTemp;
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" the design inlet air wet-bulb temperature of " + OutputChar + " must be within the model limits of " + OutputCharLo + " and " + OutputCharHi + " degrees C" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).DesignApproach = NumArray( 2 );
			if ( NumArray( 2 ) < SimpleTower( TowerNum ).MinApproachTemp || NumArray( 2 ) > SimpleTower( TowerNum ).MaxApproachTemp ) {
				gio::write( OutputChar, OutputFormat ) << SimpleTower( TowerNum ).DesignApproach;
				gio::write( OutputCharLo, OutputFormat ) << SimpleTower( TowerNum ).MinApproachTemp;
				gio::write( OutputCharHi, OutputFormat ) << SimpleTower( TowerNum ).MaxApproachTemp;
				ShowSevereError( cCurrentModuleObject + ", \"" + SimpleTower( TowerNum ).Name + "\" the design approach temperature of " + OutputChar + " must be within the model limits of " + OutputCharLo + " and " + OutputCharHi + " degrees C" );
				ErrorsFound = true;
			}

			SimpleTower( TowerNum ).DesignRange = NumArray( 3 );
			if ( NumArray( 3 ) < SimpleTower( TowerNum ).MinRangeTemp || NumArray( 3 ) > SimpleTower( TowerNum ).MaxRangeTemp ) {
				gio::write( OutputChar, OutputFormat ) << SimpleTower( TowerNum ).DesignRange;
				gio::write( OutputCharLo, OutputFormat ) << SimpleTower( TowerNum ).MinRangeTemp;
				gio::write( OutputCharHi, OutputFormat ) << SimpleTower( TowerNum ).MaxRangeTemp;
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
			SimpleTower( TowerNum ).Tower_TypeOf = TypeOf_CoolingTower_VarSpdMerkel;
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
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTower( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTower( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTower( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTower( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTower( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTower( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			// Added for fluid bypass
			SetupOutputVariable( "Cooling Tower Bypass Fraction []", SimpleTower( TowerNum ).BypassFraction, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTower( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Cycling Ratio []", SimpleTower( TowerNum ).FanCyclingRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTower( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTower( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}
		}

		// CurrentModuleObject='CoolingTower:TwoSpeed'
		for ( TowerNum = NumSingleSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers; ++TowerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTower( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTower( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTower( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTower( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTower( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTower( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			SetupOutputVariable( "Cooling Tower Fan Cycling Ratio []", SimpleTower( TowerNum ).FanCyclingRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Speed Level []", SimpleTower( TowerNum ).SpeedSelected, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTower( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTower( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTower( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}
		}

		// CurrentModuleObject='CoolingTower:VariableSpeed'
		for ( TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers; ++TowerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTower( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTower( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTower( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTower( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTower( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTower( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			SetupOutputVariable( "Cooling Tower Air Flow Rate Ratio []", SimpleTower( TowerNum ).AirFlowRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Part Load Ratio []", SimpleTower( TowerNum ).FanCyclingRatio, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTower( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTower( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTower( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}

			//    CALL SetupOutputVariable('Tower Makeup Water Consumption [m3]', &
			//          SimpleTower(TowerNum)%WaterAmountUsed,'System','Sum',SimpleTower(TowerNum)%Name, &
			//                                  ResourceTypeKey='Water',EndUseKey='HeatRejection',GroupKey='Plant')

		}

		// CurrentModuleObject='CoolingTower:VariableSpeed:Merkel'
		for ( TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers; ++TowerNum ) {
			SetupOutputVariable( "Cooling Tower Inlet Temperature [C]", SimpleTower( TowerNum ).InletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Outlet Temperature [C]", SimpleTower( TowerNum ).OutletWaterTemp, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Mass Flow Rate [kg/s]", SimpleTower( TowerNum ).WaterMassFlowRate, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Heat Transfer Rate [W]", SimpleTower( TowerNum ).Qactual, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Power [W]", SimpleTower( TowerNum ).FanPower, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Fan Electric Energy [J]", SimpleTower( TowerNum ).FanEnergy, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			SetupOutputVariable( "Cooling Tower Fan Speed Ratio []", SimpleTower( TowerNum ).AirFlowRatio, "System", "Average", SimpleTower( TowerNum ).Name );

			SetupOutputVariable( "Cooling Tower Operating Cells Count []", SimpleTower( TowerNum ).NumCellOn, "System", "Average", SimpleTower( TowerNum ).Name );
			if ( SimpleTower( TowerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Power [W]", SimpleTower( TowerNum ).BasinHeaterPower, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Basin Heater Electric Energy [J]", SimpleTower( TowerNum ).BasinHeaterConsumption, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Electric", "HeatRejection", _, "Plant" );
			}
		}
		// setup common water reporting for all types of towers.
		for ( TowerNum = 1; TowerNum <= NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers; ++TowerNum ) {
			if ( SimpleTower( TowerNum ).SuppliedByWaterSystem ) {
				SetupOutputVariable( "Cooling Tower Make Up Water Volume Flow Rate [m3/s]", SimpleTower( TowerNum ).MakeUpVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Water Volume [m3]", SimpleTower( TowerNum ).MakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]", SimpleTower( TowerNum ).TankSupplyVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Storage Tank Water Volume [m3]", SimpleTower( TowerNum ).TankSupplyVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Water", "HeatRejection", _, "Plant" );
				SetupOutputVariable( "Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]", SimpleTower( TowerNum ).StarvedMakeUpVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Starved Storage Tank Water Volume [m3]", SimpleTower( TowerNum ).StarvedMakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Mains Water Volume [m3]", SimpleTower( TowerNum ).StarvedMakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "MainsWater", "HeatRejection", _, "Plant" );
			} else { // tower water from mains and gets metered
				SetupOutputVariable( "Cooling Tower Make Up Water Volume Flow Rate [m3/s]", SimpleTower( TowerNum ).MakeUpVdot, "System", "Average", SimpleTower( TowerNum ).Name );
				SetupOutputVariable( "Cooling Tower Make Up Water Volume [m3]", SimpleTower( TowerNum ).MakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "Water", "HeatRejection", _, "Plant" );
				SetupOutputVariable( "Cooling Tower Make Up Mains Water Volume [m3]", SimpleTower( TowerNum ).MakeUpVol, "System", "Sum", SimpleTower( TowerNum ).Name, _, "MainsWater", "HeatRejection", _, "Plant" );
			}

			SetupOutputVariable( "Cooling Tower Water Evaporation Volume Flow Rate [m3/s]", SimpleTower( TowerNum ).EvaporationVdot, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Evaporation Volume [m3]", SimpleTower( TowerNum ).EvaporationVol, "System", "Sum", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Drift Volume Flow Rate [m3/s]", SimpleTower( TowerNum ).DriftVdot, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Drift Volume [m3]", SimpleTower( TowerNum ).DriftVol, "System", "Sum", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Blowdown Volume Flow Rate [m3/s]", SimpleTower( TowerNum ).BlowdownVdot, "System", "Average", SimpleTower( TowerNum ).Name );
			SetupOutputVariable( "Cooling Tower Water Blowdown Volume [m3]", SimpleTower( TowerNum ).BlowdownVol, "System", "Sum", SimpleTower( TowerNum ).Name );
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

		nsvInletWaterTemp = 0.0; // CW temperature at tower inlet
		nsvOutletWaterTemp = 0.0; // CW temperature at tower outlet
		nsvWaterInletNode = 0; // Node number at tower inlet
		nsvWaterOutletNode = 0; // Node number at tower outlet
		nsvWaterMassFlowRate = 0.0; // nsvWaterMassFlowRate through tower
		nsvQactual = 0.0; // Tower heat transfer
		nsvCTFanPower = 0.0; // Tower fan power used
		nsvAirFlowRateRatio = 0.0; // Ratio of air flow rate through VS cooling tower to design air flow rate
		nsvBasinHeaterPower = 0.0; // Basin heater power use (W)
		nsvWaterUsage = 0.0; // Tower water usage (m3/s)
		nsvFanCyclingRatio = 0.0; // cycling ratio of tower fan when min fan speed provide to much capacity

	}

	void
	Towerspecs::InitTower()
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

//		static Array1D_bool OneTimeFlagForEachTower;
		//  LOGICAL                                 :: FatalError
//		int TypeOf_Num( 0 );
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		Real64 rho; // local density of fluid

		// Do the one time initializations
//		if ( InitTowerOneTimeFlag ) {
//			InitTowerOneTimeFlag = false;
//		}

		if ( this->OneTimeFlagForEachTower ) {

//			if( this->TowerType_Num == CoolingTower_SingleSpeed ) {
//				TypeOf_Num = TypeOf_CoolingTower_SingleSpd;
//			} else if ( this->TowerType_Num == CoolingTower_TwoSpeed ) {
//				TypeOf_Num = TypeOf_CoolingTower_TwoSpd;
//			} else if ( this->TowerType_Num == CoolingTower_VariableSpeed ) {
//				TypeOf_Num = TypeOf_CoolingTower_VarSpd;
//			} else if ( this->TowerType_Num == CoolingTower_VariableSpeedMerkel ) {
//				TypeOf_Num = TypeOf_CoolingTower_VarSpdMerkel;
//			} else {
//				assert( false );
//			}

			// Locate the tower on the plant loops for later usage
			ScanPlantLoopsForObject( this->Name, this->Tower_TypeOf, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, _, _, _, _, _, ErrorsFound );
			if ( ErrorsFound ) {
				ShowFatalError( "InitTower: Program terminated due to previous condition(s)." );
			}

			// check if setpoint on outlet node
			if ( ( Node( this->WaterOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( this->WaterOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
				this->SetpointIsOnOutlet = false;
			} else {
				this->SetpointIsOnOutlet = true;
			}

			this->OneTimeFlagForEachTower = false;

		}

		// Begin environment initializations
		if ( this->MyEnvrnFlag && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, InitConvTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

			this->DesWaterMassFlowRate = this->DesignWaterFlowRate * rho;
			this->DesWaterMassFlowRatePerCell = this->DesWaterMassFlowRate / this->NumCell;
			InitComponentNodes( 0.0, this->DesWaterMassFlowRate, this->WaterInletNodeNum, this->WaterOutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );

			this->MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			this->MyEnvrnFlag = true;
		}

		// Each time initializations
		nsvWaterInletNode = this->WaterInletNodeNum;
		this->WaterTemp = Node( nsvWaterInletNode ).Temp;

		if ( this->OutdoorAirInletNodeNum != 0 ) {
			this->AirTemp = Node( this->OutdoorAirInletNodeNum ).Temp;
			this->AirHumRat = Node( this->OutdoorAirInletNodeNum ).HumRat;
			this->AirPress = Node( this->OutdoorAirInletNodeNum ).Press;
			this->AirWetBulb = Node( this->OutdoorAirInletNodeNum ).OutAirWetBulb;
		} else {
			this->AirTemp = OutDryBulbTemp;
			this->AirHumRat = OutHumRat;
			this->AirPress = OutBaroPress;
			this->AirWetBulb = OutWetBulbTemp;
		}

		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		BranchIndex = this->BranchNum;
		CompIndex = this->CompNum;

		nsvWaterMassFlowRate = RegulateCondenserCompFlowReqOp( this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, this->DesWaterMassFlowRate * this->TowerMassFlowRateMultiplier );

		SetComponentFlowRate( nsvWaterMassFlowRate, this->WaterInletNodeNum, this->WaterOutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );

		// Added for fluid bypass. 8/2008
		this->BypassFraction = 0.0;
		nsvBasinHeaterPower = 0.0;

	}

	void
		Towerspecs::SizeTower()
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

		tmpDesignWaterFlowRate = this->DesignWaterFlowRate;
		tmpHighSpeedFanPower = this->HighSpeedFanPower;
		tmpHighSpeedAirFlowRate = this->HighSpeedAirFlowRate;
		tmpLowSpeedAirFlowRate = this->LowSpeedAirFlowRate;

		// Find the appropriate Plant Sizing object
		PltSizCondNum = PlantLoop( this->LoopNum ).PlantSizNum;

		if ( this->PerformanceInputMethod_Num == PIM_UFactor && (! this->HighSpeedTowerUAWasAutoSized )) {
			if ( PltSizCondNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
				DesTowerLoad = rho * Cp * this->DesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
				this->TowerNominalCapacity = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
			} else {
				AssumedDeltaT = 11.0;
				AssumedExitTemp = 21.0;
				rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, AssumedExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, AssumedExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

				DesTowerLoad = rho * Cp * this->DesignWaterFlowRate * AssumedDeltaT;
				this->TowerNominalCapacity = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
			}
		}

		if ( this->DesignWaterFlowRateWasAutoSized ) {
			if ( PltSizCondNum > 0 ) {
				if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					tmpDesignWaterFlowRate = PlantSizData( PltSizCondNum ).DesVolFlowRate * this->SizFac;
					if ( PlantFirstSizesOkayToFinalize ) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
				} else {
					tmpDesignWaterFlowRate = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
				}
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate );
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					ShowSevereError( "Autosizing error for cooling tower object = " + this->Name );
					ShowFatalError( "Autosizing of cooling tower condenser flow rate requires a loop Sizing:Plant object." );
				}

			}
		}

		if ( this->PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			// Design water flow rate is assumed to be 3 gpm per ton (SI equivalent 5.382E-8 m3/s per watt)
			this->DesignWaterFlowRate = 5.382e-8 * this->TowerNominalCapacity;
			tmpDesignWaterFlowRate = this->DesignWaterFlowRate;
			if ( SameString( this->TowerType, "CoolingTower:SingleSpeed" ) ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Design Water Flow Rate based on tower nominal capacity [m3/s]", this->DesignWaterFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Design Water Flow Rate based on tower nominal capacity [m3/s]", this->DesignWaterFlowRate );
				}
			} else if ( SameString( this->TowerType, "CoolingTower:TwoSpeed" ) ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]", this->DesignWaterFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Design Water Flow Rate based on tower high-speed nominal capacity [m3/s]", this->DesignWaterFlowRate );
				}
			}
		}

		RegisterPlantCompDesignFlow( this->WaterInletNodeNum, tmpDesignWaterFlowRate );

		if ( this->HighSpeedFanPowerWasAutoSized ) {
			// We assume the nominal fan power is 0.0105 times the design load
			if ( this->PerformanceInputMethod_Num == PIM_NominalCapacity ) {
				this->HighSpeedFanPower = 0.0105 * this->TowerNominalCapacity;
			} else {
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, InitConvTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
						DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
						tmpHighSpeedFanPower = 0.0105 * DesTowerLoad;
						if ( PlantFirstSizesOkayToFinalize ) this->HighSpeedFanPower = tmpHighSpeedFanPower;
					} else {
						tmpHighSpeedFanPower = 0.0;
						if ( PlantFirstSizesOkayToFinalize ) this->HighSpeedFanPower = tmpHighSpeedFanPower;
					}
				} else {
				if ( PlantFinalSizesOkayToReport ) {
						ShowSevereError( "Autosizing of cooling tower fan power requires a loop Sizing:Plant object." );
						ShowFatalError( " Occurs in cooling tower object= " + this->Name );
					}
				}
			}
			if ( this->TowerType_Num == CoolingTower_SingleSpeed || this->TowerType_Num == CoolingTower_VariableSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower );
				}
			} else if ( this->TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Fan Power at High Fan Speed [W]", this->HighSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Fan Power at High Fan Speed [W]", this->HighSpeedFanPower );
				}
			}
		}

		if ( this->HighSpeedAirFlowRateWasAutoSized ) {
			// Plant Sizing Object is not required to AUTOSIZE this field since its simply a multiple of another field.
			tmpHighSpeedAirFlowRate = tmpHighSpeedFanPower * 0.5 * ( 101325.0 / StdBaroPress ) / 190.0;
			if ( PlantFirstSizesOkayToFinalize ) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;

			if ( this->TowerType_Num == CoolingTower_SingleSpeed || this->TowerType_Num == CoolingTower_VariableSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
				}
			} else if ( this->TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate );
				}
			}
		}

		if ( this->HighSpeedTowerUAWasAutoSized ) {
			if ( PltSizCondNum > 0 ) {
				if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, InitConvTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
					DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData( PltSizCondNum ).DeltaT;

					// This conditional statement is to trap when the user specified condenser/tower water design setpoint
					//  temperature is less than design inlet air wet bulb temperature of 25.6 C
					if ( PlantSizData( PltSizCondNum ).ExitTemp <= 25.6 ) {
						ShowSevereError( "Error when autosizing the UA value for cooling tower = " + this->Name + ". Design Loop Exit Temperature must be greater than 25.6 C when autosizing the tower UA." );
						ShowContinueError( "The Design Loop Exit Temperature specified in Sizing:Plant object = " + PlantSizData( PltSizCondNum ).PlantLoopName );
						ShowContinueError( "is less than or equal to the design inlet air wet-bulb temperature of 25.6 C." );
						ShowContinueError( "It is recommended that the Design Loop Exit Temperature = 25.6 C plus the cooling tower design approach temperature (e.g., 4 C)." );
						ShowContinueError( "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be > 25.6 C if autosizing the cooling tower." );
						ShowFatalError( "Autosizing of cooling tower fails for tower = " + this->Name + '.' );
					}
					Par( 1 ) = DesTowerLoad;
					Par( 2 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
					Par( 3 ) = tmpHighSpeedAirFlowRate; // design air volume flow rate
					Par( 4 ) = Cp;
					UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
					UA1 = DesTowerLoad; // Assume deltaT = 1K
					this->WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					this->AirTemp = 35.0;
					this->AirWetBulb = 25.6;
					this->AirPress = StdBaroPress;
					this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
					//        SimpleTowerInlet(TowerNum)%AirHumRat = PsyWFnTdbTwbPb(35.,25.6,StdBaroPress)
					SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
					if ( SolFla == -1 ) {
						ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
					} else if ( SolFla == -2 ) {
						ShowSevereError( "Bad starting values for UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
					}

					if ( PlantFirstSizesOkayToFinalize ) {
						this->HighSpeedTowerUA = UA;
					}
					this->TowerNominalCapacity = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						this->HighSpeedTowerUA = 0.0;
					}
				}
				if ( this->TowerType_Num == CoolingTower_SingleSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"U-Factor Times Area Value at Design Air Flow Rate [W/C]", this->HighSpeedTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]", this->HighSpeedTowerUA );
					}
				} else if ( this->TowerType_Num == CoolingTower_TwoSpeed ) {
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA );
					}
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					ShowSevereError( "Autosizing error for cooling tower object= " + this->Name );
					ShowFatalError( "Autosizing of cooling tower UA requires a loop Sizing:Plant object." );
				}

			}
		}

		if ( this->PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			if ( this->DesignWaterFlowRate >= SmallWaterVolFlow ) {
				// nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of delivered cooling but now is a user input
				rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, 29.44, PlantLoop( this->LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, 29.44, PlantLoop( this->LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp

				DesTowerLoad = this->TowerNominalCapacity * this->HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesTowerLoad;
				Par( 2 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 3 ) = tmpHighSpeedAirFlowRate; // design air volume flow rate
				Par( 4 ) = Cp; // 85F design exiting water temp
				UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesTowerLoad; // Assume deltaT = 1K
				this->WaterTemp = 35.0; // 95F design inlet water temperature
				this->AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				this->AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				this->AirPress = StdBaroPress;
				this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
				//      SimpleTowerInlet(TowerNum)%AirHumRat = PsyWFnTdbTwbPb(35.,25.6,StdBaroPress)
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					this->HighSpeedTowerUA = UA;
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					this->HighSpeedTowerUA = 0.0;
				}
			}
			if ( this->TowerType_Num == CoolingTower_SingleSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"U-Factor Times Area Value at Design Air Flow Rate [W/C]", this->HighSpeedTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial U-Factor Times Area Value at Design Air Flow Rate [W/C]", this->HighSpeedTowerUA );
				}
			} else if ( this->TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial U-Factor Times Area Value at High Fan Speed [W/C]", this->HighSpeedTowerUA );
				}
			}
		}

		if ( this->LowSpeedAirFlowRateWasAutoSized ) {

			if ( PlantFirstSizesOkayToFinalize ) {
				this->LowSpeedAirFlowRate = this->LowSpeedAirFlowRateSizingFactor * this->HighSpeedAirFlowRate;
				tmpLowSpeedAirFlowRate = this->LowSpeedAirFlowRate;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Low Fan Speed Air Flow Rate [m3/s]", this->LowSpeedAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Low Fan Speed Air Flow Rate [m3/s]", this->LowSpeedAirFlowRate );
				}
			} else {
				tmpLowSpeedAirFlowRate = this->LowSpeedAirFlowRateSizingFactor * tmpHighSpeedAirFlowRate;
			}
		}

		if ( this->LowSpeedFanPowerWasAutoSized ) {
			if ( PlantFirstSizesOkayToFinalize ) {
				this->LowSpeedFanPower = this->LowSpeedFanPowerSizingFactor * this->HighSpeedFanPower;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower );
				}
			}
		}

		if ( this->LowSpeedTowerUAWasAutoSized && PlantFirstSizesOkayToFinalize ) {
			this->LowSpeedTowerUA = this->LowSpeedTowerUASizingFactor * this->HighSpeedTowerUA;
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"U-Factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedTowerUA );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Initial U-Factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedTowerUA );
			}
		}

		if ( this->PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			if ( this->TowerLowSpeedNomCapWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					this->TowerLowSpeedNomCap = this->TowerLowSpeedNomCapSizingFactor * this->TowerNominalCapacity;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Low Speed Nominal Capacity [W]", this->TowerLowSpeedNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial Low Speed Nominal Capacity [W]", this->TowerLowSpeedNomCap );
					}
				}
			}
			if ( this->TowerFreeConvNomCapWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					this->TowerFreeConvNomCap = this->TowerFreeConvNomCapSizingFactor * this->TowerNominalCapacity;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
					}
				}
			}

		}

		if ( this->PerformanceInputMethod_Num == PIM_NominalCapacity && SameString( this->TowerType, "CoolingTower:TwoSpeed" ) ) {
			if ( this->DesignWaterFlowRate >= SmallWaterVolFlow && this->TowerLowSpeedNomCap > 0.0 ) {
				// nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now is a user input
				rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, 29.44, PlantLoop( this->LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, 29.44, PlantLoop( this->LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				DesTowerLoad = this->TowerLowSpeedNomCap * this->HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesTowerLoad;
				Par( 2 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 3 ) = tmpLowSpeedAirFlowRate; // Air volume flow rate at low fan speed
				Par( 4 ) = Cp; // 85F design exiting water temp
				UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesTowerLoad; // Assume deltaT = 1K
				this->WaterTemp = 35.0; // 95F design inlet water temperature
				this->AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				this->AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				this->AirPress = StdBaroPress;
				this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					this->LowSpeedTowerUA = UA;
				}
			} else {
				this->LowSpeedTowerUA = 0.0;
			}
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Low Fan Speed U-Factor Times Area Value [W/K]", this->LowSpeedTowerUA );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Initial Low Fan Speed U-Factor Times Area Value [W/K]", this->LowSpeedTowerUA );
			}
		}

		if ( this->FreeConvAirFlowRateWasAutoSized ) {
			if ( PlantFirstSizesOkayToFinalize ) {
				this->FreeConvAirFlowRate = this->FreeConvAirFlowRateSizingFactor * this->HighSpeedAirFlowRate;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
				}
			}
		}

		if ( this->FreeConvTowerUAWasAutoSized ) {
			if ( PlantFirstSizesOkayToFinalize ) {
				this->FreeConvTowerUA = this->FreeConvTowerUASizingFactor * this->HighSpeedTowerUA;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Free Convection U-Factor Times Area Value [W/K]", this->FreeConvTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Free Convection U-Factor Times Area Value [W/K]", this->FreeConvTowerUA );
				}
			}
		}

		if ( this->PerformanceInputMethod_Num == PIM_NominalCapacity ) {
			if ( this->DesignWaterFlowRate >= SmallWaterVolFlow && this->TowerFreeConvNomCap > 0.0 ) {
				// nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling but now user input
				rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, 29.44, PlantLoop( this->LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, 29.44, PlantLoop( this->LoopNum ).FluidIndex, RoutineName ); // 85F design exiting water temp
				DesTowerLoad = this->TowerFreeConvNomCap * this->HeatRejectCapNomCapSizingRatio;
				Par( 1 ) = DesTowerLoad;
				Par( 2 ) = rho * this->DesignWaterFlowRate; // design water mass flow rate
				Par( 3 ) = this->FreeConvAirFlowRate; // free convection air volume flow rate
				Par( 4 ) = Cp; // 85F design exiting water temp
				UA0 = 0.0001 * DesTowerLoad; // Assume deltaT = 10000K (limit)
				UA1 = DesTowerLoad; // Assume deltaT = 1K
				this->WaterTemp = 35.0; // 95F design inlet water temperature
				this->AirTemp = 35.0; // 95F design inlet air dry-bulb temp
				this->AirWetBulb = 25.6; // 78F design inlet air wet-bulb temp
				this->AirPress = StdBaroPress;
				this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					this->FreeConvTowerUA = UA;
				}
			} else {
				this->FreeConvTowerUA = 0.0;
			}
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
			}
		}

		// calibrate variable speed tower model based on user input by finding calibration water flow rate ratio that
		// yields an approach temperature that matches user input
		if ( SameString( this->TowerType, "CoolingTower:VariableSpeed" ) ) {

			Twb = this->DesignInletWB;
			Tr = this->DesignRange;
			Ta = this->DesignApproach;

			Par( 1 ) = 1.0; // air flow rate ratio
			Par( 2 ) = Twb; // inlet air wet-bulb temperature [C]
			Par( 3 ) = Tr; // tower range temperature [C]
			Par( 4 ) = Ta; // design approach temperature [C]
			Par( 5 ) = 0.0; // Calculation FLAG, 0.0 = calc water flow ratio, 1.0 calc air flow ratio

			//   check range for water flow rate ratio (make sure RegulaFalsi converges)
			MaxWaterFlowRateRatio = 0.5;
			Tapproach = 0.0;
			FlowRateRatioStep = ( this->MaxWaterFlowRatio - this->MinWaterFlowRatio ) / 10.0;
			ModelCalibrated = true;
			ModelWaterFlowRatioMax = this->MaxWaterFlowRatio * 4.0;
			//   find a flow rate large enough to provide an approach temperature > than the user defined approach
			while ( Tapproach < Ta && MaxWaterFlowRateRatio <= ModelWaterFlowRatioMax ) {
				WaterFlowRateRatio = MaxWaterFlowRateRatio;
				this->CalcVSTowerApproach( WaterFlowRateRatio, 1.0, Twb, Tr, Tapproach );
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
				SolveRegulaFalsi( Acc, MaxIte, SolFla, WaterFlowRatio, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return SimpleTowerApproachResidual( X, Par ); }, constant_pointfive, MaxWaterFlowRateRatio, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower water flow ratio during calibration" );
					ShowContinueError( "Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio for this variable-speed cooling tower." );
					ShowFatalError( "Cooling tower calibration failed for tower " + this->Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for cooling tower water flow rate ratio calibration." );
					ShowContinueError( "Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of water flow rate ratio for this variable-speed cooling tower." );
					ShowFatalError( "Cooling tower calibration failed for tower " + this->Name + '.' );
				}
			} else {
				gio::write( OutputChar2, OutputFormat2 ) << WaterFlowRateRatio;
				gio::write( OutputChar, OutputFormat ) << Tapproach;
				ShowSevereError( "Bad starting values for cooling tower water flow rate ratio calibration." );
				ShowContinueError( "Design inlet air wet-bulb or range temperature must be modified to achieve the design approach" );
				ShowContinueError( "A water flow rate ratio of " + OutputChar2 + " was calculated to yield an approach temperature of " + OutputChar + '.' );
				ShowFatalError( "Cooling tower calibration failed for tower " + this->Name + '.' );
			}

			this->CalibratedWaterFlowRate = this->DesignWaterFlowRate / WaterFlowRatio;

			if ( WaterFlowRatio < this->MinWaterFlowRatio || WaterFlowRatio > this->MaxWaterFlowRatio ) {
				gio::write( OutputChar2, OutputFormat2 ) << WaterFlowRatio;
				gio::write( OutputCharLo, OutputFormat ) << this->MinWaterFlowRatio;
				gio::write( OutputCharHi, OutputFormat ) << this->MaxWaterFlowRatio;
				ShowWarningError( "CoolingTower:VariableSpeed, \"" + this->Name + "\" the calibrated water flow rate ratio is determined to be " + OutputChar2 + ". This is outside the valid range of " + OutputCharLo + " to " + OutputCharHi + '.' );
			}

			rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, ( Twb + Ta + Tr ), PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
			Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, ( Twb + Ta + Tr ), PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

			this->TowerNominalCapacity = ( ( rho * tmpDesignWaterFlowRate ) * Cp * Tr );
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Nominal Capacity [W]", this->TowerNominalCapacity );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Initial Nominal Capacity [W]", this->TowerNominalCapacity );
			}
			this->FreeConvAirFlowRate = this->MinimumVSAirFlowFrac * this->HighSpeedAirFlowRate;

			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Air Flow Rate in free convection regime [m3/s]", this->FreeConvAirFlowRate );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Initial Air Flow Rate in free convection regime [m3/s]", this->FreeConvAirFlowRate );
			}
			this->TowerFreeConvNomCap = this->TowerNominalCapacity * this->FreeConvectionCapacityFraction;

			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Tower capacity in free convection regime at design conditions [W]", this->TowerFreeConvNomCap );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( this->TowerType, this->Name,
					"Initial Tower capacity in free convection regime at design conditions [W]", this->TowerFreeConvNomCap );
			}
		}
		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = this->Name;
			PreDefTableEntry( pdchMechType, equipName, this->TowerType );
			PreDefTableEntry( pdchMechNomCap, equipName, this->TowerNominalCapacity );
		}

		// input error checking
		ErrorsFound = false;
		if ( PlantFinalSizesOkayToReport ) {
			if ( this->TowerType_Num == CoolingTower_SingleSpeed ) {
				if ( this->DesignWaterFlowRate > 0.0 ) {
					if ( this->FreeConvAirFlowRate >= this->HighSpeedAirFlowRate ) {
						ShowSevereError( cCoolingTower_SingleSpeed + " \"" + this->Name + "\". Free convection air flow rate must be less than the design air flow rate." );
						ErrorsFound = true;
					}
					if ( this->FreeConvTowerUA >= this->HighSpeedTowerUA ) {
						ShowSevereError( cCoolingTower_SingleSpeed + " \"" + this->Name + "\". Free convection UA must be less than the design tower UA." );
						ErrorsFound = true;
					}
				}
			}

			if ( this->TowerType_Num == CoolingTower_TwoSpeed ) {
				if ( this->DesignWaterFlowRate > 0.0 ) {
					if ( this->HighSpeedAirFlowRate <= this->LowSpeedAirFlowRate ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + this->Name + "\". Low speed air flow rate must be less than the high speed air flow rate." );
						ErrorsFound = true;
					}
					if ( this->LowSpeedAirFlowRate <= this->FreeConvAirFlowRate ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + this->Name + "\". Free convection air flow rate must be less than the low speed air flow rate." );
						ErrorsFound = true;
					}
					if ( this->HighSpeedTowerUA <= this->LowSpeedTowerUA ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + this->Name + "\". Tower UA at low fan speed must be less than the tower UA at high fan speed." );
						ErrorsFound = true;
					}
					if ( this->LowSpeedTowerUA <= this->FreeConvTowerUA ) {
						ShowSevereError( cCoolingTower_TwoSpeed + " \"" + this->Name + "\". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed." );
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
	Towerspecs::SizeVSMerkelTower()
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
		PltSizCondNum = PlantLoop( this->LoopNum ).PlantSizNum;

		tmpNomTowerCap = this->TowerNominalCapacity;
		tmpDesignWaterFlowRate = this->DesignWaterFlowRate;

		tmpTowerFreeConvNomCap = this->TowerFreeConvNomCap;
		tmpDesignAirFlowRate = this->HighSpeedAirFlowRate;
		tmpHighSpeedFanPower = this->HighSpeedFanPower;
		tmpFreeConvAirFlowRate = this->FreeConvAirFlowRate;

		if ( this->PerformanceInputMethod_Num == PIM_NominalCapacity ) {

			if ( this->TowerNominalCapacityWasAutoSized ) {
				// get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
						DesTowerLoad = rho * Cp * PlantSizData( PltSizCondNum ).DesVolFlowRate * PlantSizData( PltSizCondNum ).DeltaT * this->SizFac;
						tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
						if ( PlantFirstSizesOkayToFinalize ) {
							this->TowerNominalCapacity = tmpNomTowerCap;
							ReportSizingOutput( this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity );
						}
					} else {
						tmpNomTowerCap = 0.0;
						if ( PlantFirstSizesOkayToFinalize ) {
							this->TowerNominalCapacity = tmpNomTowerCap;
							ReportSizingOutput( this->TowerType, this->Name, "Nominal Capacity [W]", this->TowerNominalCapacity );
						}
					}

				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						ShowSevereError( "Autosizing error for cooling tower object = " + this->Name );
						ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
					}
				}

			}

			if ( this->TowerFreeConvNomCapWasAutoSized ) {
				tmpTowerFreeConvNomCap = tmpNomTowerCap * this->TowerFreeConvNomCapSizingFactor;
				if ( PlantFirstSizesOkayToFinalize ) {
					this->TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
					}
				}
			}

			if ( this->DesignWaterFlowRateWasAutoSized ) {
				// for nominal cap input method, get design water flow rate from nominal cap and scalable sizing factor
				tmpDesignWaterFlowRate = tmpNomTowerCap * this->DesignWaterFlowPerUnitNomCap;
				if ( PlantFirstSizesOkayToFinalize ) {
					this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate );
					}
				}
			}

			RegisterPlantCompDesignFlow( this->WaterInletNodeNum, tmpDesignWaterFlowRate );

			if ( this->HighSpeedAirFlowRateWasAutoSized ) {
				if ( this->DefaultedDesignAirFlowScalingFactor ) {
					tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap * ( 101325.0 / StdBaroPress );
				} else {
					tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap;
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					this->HighSpeedAirFlowRate = tmpDesignAirFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
					}
				}
			}

			if ( this->FreeConvAirFlowRate == AutoSize ) {
				tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * this->FreeConvAirFlowRateSizingFactor;
				if ( PlantFirstSizesOkayToFinalize ) {
					this->FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
					}
				}
			}

			// now calcuate UA values from nominal capacities and flow rates
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( PltSizCondNum > 0 ) { // user has a plant sizing object
					Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
					this->WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
				} else { // probably no plant sizing object
					Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, InitConvTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
					this->WaterTemp = 35.0; // design condition
				}
				rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, InitConvTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

				// full speed fan tower UA
				Par( 1 ) = tmpNomTowerCap * this->HeatRejectCapNomCapSizingRatio;
				Par( 2 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 3 ) = tmpDesignAirFlowRate; // design air volume flow rate
				Par( 4 ) = Cp;
				UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
				UA1 = Par( 1 ); // Assume deltaT = 1K

				this->AirTemp = 35.0;
				this->AirWetBulb = 25.6;
				this->AirPress = StdBaroPress;
				this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
					ShowFatalError( "calculating cooling tower UA failed for tower " + this->Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
				}
				this->HighSpeedTowerUA = UA;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", this->HighSpeedTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", this->HighSpeedTowerUA );
				}
				// free convection tower UA
				Par( 1 ) = tmpTowerFreeConvNomCap * this->HeatRejectCapNomCapSizingRatio;
				Par( 2 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
				Par( 3 ) = tmpFreeConvAirFlowRate; // design air volume flow rate
				Par( 4 ) = Cp;
				UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
				UA0 = max( UA0, 1.0 ); // limit to 1.0
				UA1 = Par( 1 ); // Assume deltaT = 1K

				this->AirTemp = 35.0;
				this->AirWetBulb = 25.6;
				this->AirPress = StdBaroPress;
				this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
				SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
				if ( SolFla == -1 ) {
					ShowSevereError( "Iteration limit exceeded in calculating tower free convection UA" );
					ShowFatalError( "calculating cooling tower UA failed for tower " + this->Name );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Bad starting values for UA" );
					ShowFatalError( "Autosizing of cooling tower UA failed for free convection tower " + this->Name );
				}
				this->FreeConvTowerUA = UA;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
				}
			}

		} else if ( this->PerformanceInputMethod_Num == PIM_UFactor ) {
			//UA input method

			if ( this->DesignWaterFlowRateWasAutoSized ) { // get from plant sizing
				// UA input method using plant sizing for flow rate, whereas Nominal capacity method uses scalable sizing factor per cap
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						tmpDesignWaterFlowRate = PlantSizData( PltSizCondNum ).DesVolFlowRate * this->SizFac;
						if ( PlantFirstSizesOkayToFinalize ) {
							this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( this->TowerType, this->Name,
									"Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( this->TowerType, this->Name,
									"Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate );
							}
						}
					} else {
						tmpDesignWaterFlowRate = 0.0;

					}

				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						ShowSevereError( "Autosizing error for cooling tower object = " + this->Name );
						ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
					}
				}
			}
			RegisterPlantCompDesignFlow( this->WaterInletNodeNum, tmpDesignWaterFlowRate );

			if ( this->HighSpeedTowerUAWasAutoSized ) {
				// get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
				if ( PltSizCondNum > 0 ) {
					if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
						DesTowerLoad = rho * Cp * PlantSizData( PltSizCondNum ).DesVolFlowRate * PlantSizData( PltSizCondNum ).DeltaT * this->SizFac;
						tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
						if ( PlantFirstSizesOkayToFinalize ) {
							this->TowerNominalCapacity = tmpNomTowerCap;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( this->TowerType, this->Name,
									"Nominal Capacity [W]", this->TowerNominalCapacity );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( this->TowerType, this->Name,
									"Initial Nominal Capacity [W]", this->TowerNominalCapacity );
							}
						}
					} else {
						tmpNomTowerCap = 0.0;
						if ( PlantFirstSizesOkayToFinalize ) {
							this->TowerNominalCapacity = tmpNomTowerCap;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( this->TowerType, this->Name,
									"Nominal Capacity [W]", this->TowerNominalCapacity );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( this->TowerType, this->Name,
									"Initial Nominal Capacity [W]", this->TowerNominalCapacity );
							}
						}
					}
				} else {
					if ( PlantFirstSizesOkayToFinalize ) {
						ShowSevereError( "Autosizing error for cooling tower object = " + this->Name );
						ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
					}
				}
				if ( this->TowerFreeConvNomCapWasAutoSized ) {
					tmpTowerFreeConvNomCap = tmpNomTowerCap * this->TowerFreeConvNomCapSizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) {
						this->TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Initial Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
						}
					}
				}
				if ( this->HighSpeedAirFlowRateWasAutoSized ) {
					if ( this->DefaultedDesignAirFlowScalingFactor ) {
						tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap * ( 101325.0 / StdBaroPress );
					} else {
						tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap;
					}
					if ( PlantFirstSizesOkayToFinalize ) {
						this->HighSpeedAirFlowRate = tmpDesignAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
						}
					}
				}
				if ( this->FreeConvAirFlowRateWasAutoSized ) {
					tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * this->FreeConvAirFlowRateSizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) {
						this->FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Initial Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
						}
					}
				}
				// now calcuate UA values from nominal capacities and flow rates
				if ( PlantFirstSizesOkayToFinalize ) {
					rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, InitConvTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
					// full speed fan tower UA
					Par( 1 ) = tmpNomTowerCap * this->HeatRejectCapNomCapSizingRatio;
					Par( 2 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
					Par( 3 ) = tmpDesignAirFlowRate; // design air volume flow rate
					Par( 4 ) = Cp;
					UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
					UA1 = Par( 1 ); // Assume deltaT = 1K
					this->WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					this->AirTemp = 35.0;
					this->AirWetBulb = 25.6;
					this->AirPress = StdBaroPress;
					this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
					SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
					if ( SolFla == -1 ) {
						ShowSevereError( "Iteration limit exceeded in calculating tower UA" );
						ShowFatalError( "calculating cooling tower UA failed for tower " + this->Name );
					} else if ( SolFla == -2 ) {
						ShowSevereError( "Bad starting values for UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for tower " + this->Name );
					}
					this->HighSpeedTowerUA = UA;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", this->HighSpeedTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]", this->HighSpeedTowerUA );
					}
					// free convection tower UA
					Par( 1 ) = tmpTowerFreeConvNomCap * this->HeatRejectCapNomCapSizingRatio;
					Par( 2 ) = rho * tmpDesignWaterFlowRate; // design water mass flow rate
					Par( 3 ) = tmpFreeConvAirFlowRate; // design air volume flow rate
					Par( 4 ) = Cp;
					UA0 = 0.0001 * Par( 1 ); // Assume deltaT = 10000K (limit)
					UA1 = Par( 1 ); // Assume deltaT = 1K
					this->WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					this->AirTemp = 35.0;
					this->AirWetBulb = 25.6;
					this->AirPress = StdBaroPress;
					this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
					SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerUAResidual( X, Par ); }, UA0, UA1, Par );
					if ( SolFla == -1 ) {
						ShowSevereError( "Iteration limit exceeded in calculating tower free convection UA" );
						ShowFatalError( "calculating cooling tower UA failed for tower " + this->Name );
					} else if ( SolFla == -2 ) {
						ShowSevereError( "Bad starting values for UA" );
						ShowFatalError( "Autosizing of cooling tower UA failed for free convection tower " + this->Name );
					}
					this->LowSpeedTowerUA = UA;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
					}
				}

			} else { //full speed UA given

				if ( this->FreeConvTowerUAWasAutoSized ) { // determine from scalable sizing factor
					if ( PlantFirstSizesOkayToFinalize ) {
						this->FreeConvTowerUA = this->HighSpeedTowerUA * this->FreeConvTowerUASizingFactor;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Initial U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]", this->FreeConvTowerUA );
						}
					}
				}

				if ( this->HighSpeedAirFlowRateWasAutoSized ) { // given UA but not air flow rate
					// need an air flow rate to find capacity from UA but flow rate is scaled off capacity
					// get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
					if ( PltSizCondNum > 0 ) {
						if ( PlantSizData( PltSizCondNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
							rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
							Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
							DesTowerLoad = rho * Cp * PlantSizData( PltSizCondNum ).DesVolFlowRate * PlantSizData( PltSizCondNum ).DeltaT;
							tmpNomTowerCap = DesTowerLoad / this->HeatRejectCapNomCapSizingRatio;
							if ( PlantFirstSizesOkayToFinalize ) {
								this->TowerNominalCapacity = tmpNomTowerCap;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( this->TowerType, this->Name,
										"Nominal Capacity [W]", this->TowerNominalCapacity );
								}
								if ( PlantFirstSizesOkayToReport ) {
									ReportSizingOutput( this->TowerType, this->Name,
										"Initial Nominal Capacity [W]", this->TowerNominalCapacity );
								}
							}
						} else {
							tmpNomTowerCap = rho = Cp = 0.0; // rho and Cp added: Used below
							if ( PlantFirstSizesOkayToFinalize ) {
								this->TowerNominalCapacity = tmpNomTowerCap;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( this->TowerType, this->Name,
										"Nominal Capacity [W]", this->TowerNominalCapacity );
								}
								if ( PlantFirstSizesOkayToReport ) {
									ReportSizingOutput( this->TowerType, this->Name,
										"Initial Nominal Capacity [W]", this->TowerNominalCapacity );
								}
							}
						}

					} else {
						tmpNomTowerCap = 0.0; // Suppress uninitialized warnings
						if ( PlantFirstSizesOkayToFinalize ) {
							ShowSevereError( "Autosizing error for cooling tower object = " + this->Name );
							ShowFatalError( "Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object." );
						}
					}

					if ( this->DefaultedDesignAirFlowScalingFactor ) {
						tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap * ( 101325.0 / StdBaroPress );
					} else {
						tmpDesignAirFlowRate = tmpNomTowerCap * this->DesignAirFlowPerUnitNomCap;
					}
					if ( PlantFirstSizesOkayToFinalize ) {
						this->HighSpeedAirFlowRate = tmpDesignAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate );
						}
					}

				} else { // UA and Air flow rate given, so find Nominal Cap from running model

					rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, PlantSizData( PltSizCondNum ).ExitTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

					this->WaterTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
					this->AirTemp = 35.0;
					this->AirWetBulb = 25.6;
					this->AirPress = StdBaroPress;
					this->AirHumRat = PsyWFnTdbTwbPb( this->AirTemp, this->AirWetBulb, this->AirPress );
					this->SimSimpleTower( rho * tmpDesignWaterFlowRate, this->HighSpeedAirFlowRate, this->HighSpeedTowerUA, OutWaterTemp );
					tmpNomTowerCap = Cp * rho * tmpDesignWaterFlowRate * ( this->WaterTemp - OutWaterTemp );
					tmpNomTowerCap /= this->HeatRejectCapNomCapSizingRatio;
					if ( PlantFirstSizesOkayToFinalize ) {
						this->TowerNominalCapacity = tmpNomTowerCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Nominal Capacity [W]", this->TowerNominalCapacity );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Initial Nominal Capacity [W]", this->TowerNominalCapacity );
						}
					}

				} // both UA and air flow rate given

				if ( this->FreeConvAirFlowRateWasAutoSized ) {
					tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * this->FreeConvAirFlowRateSizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) {
						this->FreeConvAirFlowRate = tmpFreeConvAirFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( this->TowerType, this->Name,
								"Initial Free Convection Regime Air Flow Rate [m3/s]", this->FreeConvAirFlowRate );
						}
					}
				}

				this->SimSimpleTower( rho * tmpDesignWaterFlowRate, tmpFreeConvAirFlowRate, this->FreeConvTowerUA, OutWaterTemp );
				tmpTowerFreeConvNomCap = Cp * rho * tmpDesignWaterFlowRate * ( this->WaterTemp - OutWaterTemp );
				tmpTowerFreeConvNomCap /= this->HeatRejectCapNomCapSizingRatio;
				if ( PlantFirstSizesOkayToFinalize ) {
					this->TowerFreeConvNomCap = tmpTowerFreeConvNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( this->TowerType, this->Name,
							"Initial Free Convection Nominal Capacity [W]", this->TowerFreeConvNomCap );
					}
				}
			}
		}

		if ( this->HighSpeedFanPowerWasAutoSized ) {
			tmpHighSpeedFanPower = tmpNomTowerCap * this->DesignFanPowerPerUnitNomCap;
			if ( PlantFirstSizesOkayToFinalize ) {
				this->HighSpeedFanPower = tmpHighSpeedFanPower;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Design Fan Power [W]", this->HighSpeedFanPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( this->TowerType, this->Name,
						"Initial Design Fan Power [W]", this->HighSpeedFanPower );
				}
			}
		}

	}

	// End Initialization Section for the CondenserLoopTowers Module
	//******************************************************************************

	// Beginning of the CondenserLoopTowers Module Simulation Subroutines
	// *****************************************************************************

	void
	Towerspecs::CalcSingleSpeedTower()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       T Hong, Aug. 2008. Added fluid bypass for single speed cooling tower
		//                      The nsvOutletWaterTemp from SimSimpleTower can be lower than 0 degreeC
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
		nsvWaterInletNode = this->WaterInletNodeNum;
		nsvWaterOutletNode = this->WaterOutletNodeNum;
		nsvQactual = 0.0;
		nsvCTFanPower = 0.0;
		nsvOutletWaterTemp = Node( nsvWaterInletNode ).Temp;
		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( this->SetpointIsOnOutlet ) {
				TempSetPoint = Node( nsvWaterOutletNode ).TempSetPoint;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( this->SetpointIsOnOutlet ) {
				TempSetPoint = Node( nsvWaterOutletNode ).TempSetPointHi;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
			}
		}}

		// Added for fluid bypass. First assume no fluid bypass
		BypassFlag = 0;
		BypassFraction = 0.0;
		BypassFraction2 = 0.0;
		this->BypassFraction = 0.0;
		CapacityControl = this->CapacityControl;

		// Added for multi-cell. Determine the number of cells operating
		if ( this->DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
			WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), this->NumCell );
			NumCellMax = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), this->NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( this->CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		this->NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;

		// Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.

		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( nsvWaterMassFlowRate <= MassFlowTolerance ) {
			// for multiple cells, we assume that it's a commun bassin
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );
			return;
		}

		IncrNumCellFlag = true; // set value to true to enter in the loop

		while ( IncrNumCellFlag ) {
			IncrNumCellFlag = false;

			//   Initialize local variables to the free convection design values
			UAdesign = this->FreeConvTowerUA / this->NumCell;
			AirFlowRate = this->FreeConvAirFlowRate / this->NumCell;
			DesignWaterFlowRate = this->DesignWaterFlowRate;
			OutletWaterTempOFF = Node( nsvWaterInletNode ).Temp;
			nsvOutletWaterTemp = OutletWaterTempOFF;
			FanModeFrac = 0.0;

			this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTempOFF );

			//   Assume Setpoint was met using free convection regime (pump ON and fan OFF)
			nsvCTFanPower = 0.0;
			nsvOutletWaterTemp = OutletWaterTempOFF;

			if ( OutletWaterTempOFF > TempSetPoint ) {
				//     Setpoint was not met (or free conv. not used), turn on cooling tower fan
				UAdesign = this->HighSpeedTowerUA / this->NumCell;
				AirFlowRate = this->HighSpeedAirFlowRate / this->NumCell;

				// The fan power is for all cells operating
				FanPowerOn = this->HighSpeedFanPower * NumCellOn / this->NumCell;

				this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRate, UAdesign, nsvOutletWaterTemp );

				if ( nsvOutletWaterTemp <= TempSetPoint ) {
					if ( CapacityControl == CapacityControl_FanCycling || nsvOutletWaterTemp <= OWTLowerLimit ) {
						//           Setpoint was met with pump ON and fan ON, calculate run-time fraction
						FanModeFrac = ( TempSetPoint - OutletWaterTempOFF ) / ( nsvOutletWaterTemp - OutletWaterTempOFF );
						nsvCTFanPower = FanModeFrac * FanPowerOn;
						nsvOutletWaterTemp = TempSetPoint;
					} else {
						//FluidBypass, fan runs at full speed for the entire time step
						FanModeFrac = 1.0;
						nsvCTFanPower = FanPowerOn;
						BypassFlag = 1;
					}
				} else {
					//         Setpoint was not met, cooling tower ran at full capacity
					FanModeFrac = 1.0;
					nsvCTFanPower = FanPowerOn;
					// if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
					if ( NumCellOn < this->NumCell && ( nsvWaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) {
						++NumCellOn;
						WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;
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

		// Calculate bypass fraction since OWTLowerLimit < nsvOutletWaterTemp < TempSetPoint.
		// The iteraction ends when the numer of iteraction exceeds the limit or the difference
		//  between the new and old bypass fractions is less than the threshold.
		if ( BypassFlag == 1 ) {
			//Inlet water temperature lower than setpoint, assume 100% bypass, tower fan off
			if ( nsvInletWaterTemp <= TempSetPoint ) {
				nsvCTFanPower = 0.0;
				BypassFraction = 1.0;
				this->BypassFraction = 1.0;
				nsvOutletWaterTemp = nsvInletWaterTemp;
			} else {
				if ( std::abs( nsvInletWaterTemp - nsvOutletWaterTemp ) <= 0.01 ) {
					// Outlet temp is close enough to inlet temp, assume 100% bypass, tower fan off
					BypassFraction = 1.0;
					this->BypassFraction = 1.0;
					nsvCTFanPower = 0.0;
				} else {
					BypassFraction = ( TempSetPoint - nsvOutletWaterTemp ) / ( nsvInletWaterTemp - nsvOutletWaterTemp );
					if ( BypassFraction > 1.0 || BypassFraction < 0.0 ) {
						// Bypass cannot meet setpoint, assume no bypass
						BypassFlag = 0;
						BypassFraction = 0.0;
						this->BypassFraction = 0.0;
					} else {
						NumIteration = 0;
						BypassFractionPrev = BypassFraction;
						OutletWaterTempPrev = nsvOutletWaterTemp;
						while ( NumIteration < MaxIteration ) {
							++NumIteration;
							// need to iterate for the new nsvOutletWaterTemp while bypassing tower water
							this->SimSimpleTower( WaterMassFlowRatePerCell * ( 1.0 - BypassFraction ), AirFlowRate, UAdesign, nsvOutletWaterTemp );
							// Calc new BypassFraction based on the new nsvOutletWaterTemp
							if ( std::abs( nsvOutletWaterTemp - OWTLowerLimit ) <= 0.01 ) {
								BypassFraction2 = BypassFraction;
								break;
							} else if ( nsvOutletWaterTemp < OWTLowerLimit ) {
								// Set nsvOutletWaterTemp = OWTLowerLimit, and use linear interpolation to calculate the bypassFraction
								BypassFraction2 = BypassFractionPrev - ( BypassFractionPrev - BypassFraction ) * ( OutletWaterTempPrev - OWTLowerLimit ) / ( OutletWaterTempPrev - nsvOutletWaterTemp );
								this->SimSimpleTower( WaterMassFlowRatePerCell * ( 1.0 - BypassFraction2 ), AirFlowRate, UAdesign, nsvOutletWaterTemp );
								if ( nsvOutletWaterTemp < OWTLowerLimit ) {
									//Use previous iteraction values
									BypassFraction2 = BypassFractionPrev;
									nsvOutletWaterTemp = OutletWaterTempPrev;
								}
								break;
							} else {
								BypassFraction2 = ( TempSetPoint - nsvOutletWaterTemp ) / ( nsvInletWaterTemp - nsvOutletWaterTemp );
							}

							// Compare two BypassFraction to determine when to stop
							if ( std::abs( BypassFraction2 - BypassFraction ) <= BypassFractionThreshold ) break;
							BypassFractionPrev = BypassFraction;
							OutletWaterTempPrev = nsvOutletWaterTemp;
							BypassFraction = BypassFraction2;
						}
						if ( NumIteration > MaxIteration ) {
							ShowWarningError( "Cooling tower fluid bypass iteration exceeds maximum limit of " + MaxItChar + " for " + this->Name );
						}
						this->BypassFraction = BypassFraction2;
						// may not meet TempSetPoint due to limit of tower outlet temp to OWTLowerLimit
						nsvOutletWaterTemp = ( 1.0 - BypassFraction2 ) * nsvOutletWaterTemp + BypassFraction2 * nsvInletWaterTemp;
					}
				}
			}
		}

		//output the fraction of the time step the fan is ON
		nsvFanCyclingRatio = FanModeFrac;
		// output the number of cells operating
		this->NumCellOn = NumCellOn;
		//Should this be water inlet node num?????
		CpWater = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, Node( nsvWaterInletNode ).Temp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

		nsvQactual = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - nsvOutletWaterTemp );
		nsvAirFlowRateRatio = ( AirFlowRate * this->NumCell ) / this->HighSpeedAirFlowRate;

	}

	void
	Towerspecs::CalcTwoSpeedTower()
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

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS: nsvOutletWaterTempOFF
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

		nsvWaterInletNode = this->WaterInletNodeNum;
		nsvWaterOutletNode = this->WaterOutletNodeNum;
		nsvQactual = 0.0;
		nsvCTFanPower = 0.0;
		nsvOutletWaterTemp = Node( nsvWaterInletNode ).Temp;
		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( this->SetpointIsOnOutlet ) {
				TempSetPoint = Node( nsvWaterOutletNode ).TempSetPoint;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( this->SetpointIsOnOutlet ) {
				TempSetPoint = Node( nsvWaterOutletNode ).TempSetPointHi;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
			}
		}}

		// Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) return;
		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( nsvWaterMassFlowRate <= MassFlowTolerance ) {
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );
			return;
		}

		// Added for multi-cell. Determine the number of cells operating
		if ( this->DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
			WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), this->NumCell );
			NumCellMax = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), this->NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( this->CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		this->NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;

		IncrNumCellFlag = true;

		while ( IncrNumCellFlag ) {
			IncrNumCellFlag = false;

			//set local variable for tower
			UAdesign = this->FreeConvTowerUA / this->NumCell; // where is NumCellOn?
			AirFlowRate = this->FreeConvAirFlowRate / this->NumCell;
			DesignWaterFlowRate = this->DesignWaterFlowRate; // ??useless subroutine variable??
			OutletWaterTempOFF = Node( nsvWaterInletNode ).Temp;
			nsvWaterMassFlowRate = Node( nsvWaterInletNode ).MassFlowRate;
			OutletWaterTemp1stStage = nsvOutletWaterTemp;
			OutletWaterTemp2ndStage = nsvOutletWaterTemp;
			FanModeFrac = 0.0;

			this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTempOFF );

			//     Setpoint was met using free convection regime (pump ON and fan OFF)
			nsvCTFanPower = 0.0;
			nsvOutletWaterTemp = OutletWaterTempOFF;
			SpeedSel = 0;

			if ( OutletWaterTempOFF > TempSetPoint ) {
				//     Setpoint was not met (or free conv. not used),turn on cooling tower 1st stage fan
				UAdesign = this->LowSpeedTowerUA / this->NumCell;
				AirFlowRate = this->LowSpeedAirFlowRate / this->NumCell;
				FanPowerLow = this->LowSpeedFanPower * NumCellOn / this->NumCell;

				this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTemp1stStage );

				if ( OutletWaterTemp1stStage <= TempSetPoint ) {
					//         Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
					FanModeFrac = ( TempSetPoint - OutletWaterTempOFF ) / ( OutletWaterTemp1stStage - OutletWaterTempOFF );
					nsvCTFanPower = FanModeFrac * FanPowerLow;
					nsvOutletWaterTemp = TempSetPoint;
					nsvQactual *= FanModeFrac;
					SpeedSel = 1;
				} else {
					//         Setpoint was not met, turn on cooling tower 2nd stage fan
					UAdesign = this->HighSpeedTowerUA / this->NumCell;
					AirFlowRate = this->HighSpeedAirFlowRate / this->NumCell;
					FanPowerHigh = this->HighSpeedFanPower * NumCellOn / this->NumCell;

					this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRate, UAdesign, OutletWaterTemp2ndStage );

					if ( ( OutletWaterTemp2ndStage <= TempSetPoint ) && UAdesign > 0.0 ) {
						//           Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
						FanModeFrac = ( TempSetPoint - OutletWaterTemp1stStage ) / ( OutletWaterTemp2ndStage - OutletWaterTemp1stStage );
						nsvCTFanPower = ( FanModeFrac * FanPowerHigh ) + ( 1.0 - FanModeFrac ) * FanPowerLow;
						nsvOutletWaterTemp = TempSetPoint;
						SpeedSel = 2;
					} else {
						//           Setpoint was not met, cooling tower ran at full capacity
						nsvOutletWaterTemp = OutletWaterTemp2ndStage;
						nsvCTFanPower = FanPowerHigh;
						SpeedSel = 2;
						FanModeFrac = 1.0;
						// if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
						if ( NumCellOn < this->NumCell && ( nsvWaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) {
							++NumCellOn;
							WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;
							IncrNumCellFlag = true;
						}
					}

				}

			}
		}

		//output the fraction of the time step the fan is ON
		nsvFanCyclingRatio = FanModeFrac;
		this->SpeedSelected = SpeedSel;
		this->NumCellOn = NumCellOn;

		CpWater = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, Node( nsvWaterInletNode ).Temp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
		nsvQactual = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - nsvOutletWaterTemp );
		nsvAirFlowRateRatio = ( AirFlowRate * this->NumCell ) / this->HighSpeedAirFlowRate;

	}

	void
	Towerspecs::CalcMerkelVariableSpeedTower(
		Real64 const MyLoad
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

		nsvWaterInletNode = this->WaterInletNodeNum;
		CpWater = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, Node( nsvWaterInletNode ).Temp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
		nsvWaterOutletNode = this->WaterOutletNodeNum;
		nsvQactual = 0.0;
		nsvCTFanPower = 0.0;
		nsvOutletWaterTemp = Node( nsvWaterInletNode ).Temp;
		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( this->SetpointIsOnOutlet ) {
				TempSetPoint = Node( nsvWaterOutletNode ).TempSetPoint;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( this->SetpointIsOnOutlet ) {
				TempSetPoint = Node( nsvWaterOutletNode ).TempSetPointHi;
			} else {
				TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
			}
		}}

		// Added for multi-cell. Determine the number of cells operating
		if ( this->DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
			WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), this->NumCell );
			NumCellMax = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), this->NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( this->CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		this->NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;
		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( nsvWaterMassFlowRate <= MassFlowTolerance || ( MyLoad > SmallLoad ) ) {
			// for multiple cells, we assume that it's a common bassin
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );
			return;
		}

		if ( std::abs( MyLoad ) <= SmallLoad ) {
		// tower doesn't need to do anything
			nsvOutletWaterTemp = Node( nsvWaterInletNode ).Temp;
			nsvCTFanPower = 0.0;
			nsvAirFlowRateRatio = 0.0;
			nsvQactual = 0.0;
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );
			return;
		}

		// first find free convection cooling rate
		UAdesignPerCell = this->FreeConvTowerUA / this->NumCell;
		AirFlowRatePerCell = this->FreeConvAirFlowRate / this->NumCell;
		OutletWaterTempOFF = Node( nsvWaterInletNode ).Temp;
		nsvWaterMassFlowRate = Node( nsvWaterInletNode ).MassFlowRate;
		this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRatePerCell, UAdesignPerCell, OutletWaterTempOFF );

		FreeConvQdot = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - OutletWaterTempOFF );
		nsvCTFanPower = 0.0;

		if ( std::abs( MyLoad ) <= FreeConvQdot ) { // can meet load with free convection and fan off


			nsvOutletWaterTemp = OutletWaterTempOFF;
			nsvAirFlowRateRatio = 0.0;
			nsvQactual = FreeConvQdot;
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );

			return;
		}

		// next find full fan speed cooling rate
		UAdesignPerCell = this->HighSpeedTowerUA / this->NumCell;
		AirFlowRatePerCell = this->HighSpeedAirFlowRate / this->NumCell;
		nsvAirFlowRateRatio = 1.0;
		WaterFlowRateRatio = WaterMassFlowRatePerCell / this->DesWaterMassFlowRatePerCell;
		UAwetbulbAdjFac = CurveValue( this->UAModFuncWetBulbDiffCurvePtr, ( DesignWetBulb - this->AirWetBulb ) );
		UAairflowAdjFac = CurveValue( this->UAModFuncAirFlowRatioCurvePtr, nsvAirFlowRateRatio );
		UAwaterflowAdjFac = CurveValue( this->UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio );
		UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
		this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, nsvOutletWaterTemp );
		FullSpeedFanQdot = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - nsvOutletWaterTemp );

		if ( FullSpeedFanQdot <= std::abs( MyLoad ) ) { // full speed is what we want.

			if ( ( FullSpeedFanQdot + SmallLoad ) < std::abs( MyLoad ) && ( NumCellOn < this->NumCell ) && ( ( nsvWaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) ) {
				// If full fan and not meeting setpoint, then increase number of cells until all are used or load is satisfied
				IncrNumCellFlag = true; // set value to true to enter in the loop
				while ( IncrNumCellFlag ) {
					++NumCellOn;
					this->NumCellOn = NumCellOn;
					WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;
					WaterFlowRateRatio = WaterMassFlowRatePerCell / this->DesWaterMassFlowRatePerCell;
					UAwaterflowAdjFac = CurveValue( this->UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio );
					UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
					this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, nsvOutletWaterTemp );
					if ( ( FullSpeedFanQdot + SmallLoad ) < std::abs( MyLoad ) && ( NumCellOn < this->NumCell ) && ( ( nsvWaterMassFlowRate / ( NumCellOn + 1 ) ) >= WaterMassFlowRatePerCellMin ) ) {
						IncrNumCellFlag = true;
					} else {
						IncrNumCellFlag = false;
					}
				}
				FullSpeedFanQdot = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - nsvOutletWaterTemp );
			}
			nsvQactual = FullSpeedFanQdot;
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );
			// now calculate fan power
			FanPowerAdjustFac = CurveValue( this->FanPowerfAirFlowCurve, nsvAirFlowRateRatio );
			nsvCTFanPower = this->HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / this->NumCell;

			return;

		}

		// next find minimum air flow ratio cooling rate
		nsvAirFlowRateRatio = this->MinimumVSAirFlowFrac;
		AirFlowRatePerCell = nsvAirFlowRateRatio * this->HighSpeedAirFlowRate / this->NumCell;
		UAairflowAdjFac = CurveValue( this->UAModFuncAirFlowRatioCurvePtr, nsvAirFlowRateRatio );
		UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;
		this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, nsvOutletWaterTemp );
		MinSpeedFanQdot = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - nsvOutletWaterTemp );

		if ( std::abs( MyLoad ) <= MinSpeedFanQdot ) { // min fan speed already exceeds load)
			nsvQactual = MinSpeedFanQdot;
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );
			// now calculate fan power
			FanPowerAdjustFac = CurveValue( this->FanPowerfAirFlowCurve, nsvAirFlowRateRatio );
			nsvCTFanPower = this->HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / this->NumCell;
			return;
		}

		if ( ( MinSpeedFanQdot < std::abs( MyLoad ) ) && ( std::abs( MyLoad ) < FullSpeedFanQdot ) ) {
			// load can be refined by modulationg fan speed, call regulafalsi

			Par( 1 ) = MyLoad;
			Par( 2 ) = WaterMassFlowRatePerCell;
			Par( 3 ) = UAdesignPerCell;
			Par( 4 ) = UAwetbulbAdjFac;
			Par( 5 ) = UAwaterflowAdjFac;
			Par( 6 ) = CpWater;
			Par( 7 ) = nsvWaterMassFlowRate;

			SolveRegulaFalsi( Acc, MaxIte, SolFla, nsvAirFlowRateRatio, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return VSMerkelResidual( X, Par ); }, this->MinimumVSAirFlowFrac, 1.0, Par );
			if ( SolFla == -1 ) {
				if ( ! WarmupFlag ) {
					if ( this->VSMerkelAFRErrorIter < 1 ) {
						++this->VSMerkelAFRErrorIter;
						ShowWarningError( cCoolingTower_VariableSpeedMerkel + " - Iteration limit exceeded calculating variable speed fan ratio for unit = " + this->Name );
						ShowContinueError( "Estimated air flow ratio  = " + RoundSigDigits( ( std::abs( MyLoad ) - MinSpeedFanQdot ) / ( FullSpeedFanQdot - MinSpeedFanQdot ), 4 ) );
						ShowContinueError( "Calculated air flow ratio = " + RoundSigDigits( nsvAirFlowRateRatio, 4 ) );
						ShowContinueErrorTimeStamp( "The calculated air flow ratio will be used and the simulation continues. Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( cCoolingTower_VariableSpeedMerkel + " \"" + this->Name + "\" - Iteration limit exceeded calculating air flow ratio error continues. air flow ratio statistics follow.", this->VSMerkelAFRErrorIter, nsvAirFlowRateRatio, nsvAirFlowRateRatio );
				}
			} else if ( SolFla == -2 ) {
				nsvAirFlowRateRatio = ( std::abs( MyLoad ) - MinSpeedFanQdot ) / ( FullSpeedFanQdot - MinSpeedFanQdot );
				if ( ! WarmupFlag ) {
					if ( this->VSMerkelAFRErrorFail < 1 ) {
						++this->VSMerkelAFRErrorFail;
						ShowWarningError( cCoolingTower_VariableSpeedMerkel + " - solver failed calculating variable speed fan ratio for unit = " + this->Name );
						ShowContinueError( "Estimated air flow ratio  = " + RoundSigDigits( nsvAirFlowRateRatio, 4 ) );
						ShowContinueErrorTimeStamp( "The estimated air flow ratio will be used and the simulation continues. Occurrence info:" );
					}
					ShowRecurringWarningErrorAtEnd( cCoolingTower_VariableSpeedMerkel + " \"" + this->Name + "\" - solver failed calculating air flow ratio error continues. air flow ratio statistics follow.", this->VSMerkelAFRErrorFail, nsvAirFlowRateRatio, nsvAirFlowRateRatio );
				}
			}

			// now rerun to get peformance with nsvAirFlowRateRatio
			AirFlowRatePerCell = nsvAirFlowRateRatio * this->HighSpeedAirFlowRate / this->NumCell;

			UAairflowAdjFac = CurveValue( this->UAModFuncAirFlowRatioCurvePtr, nsvAirFlowRateRatio );
			UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

			this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, nsvOutletWaterTemp );
			nsvQactual = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - nsvOutletWaterTemp );
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );

			// now calculate fan power
			FanPowerAdjustFac = CurveValue( this->FanPowerfAirFlowCurve, nsvAirFlowRateRatio );
			nsvCTFanPower = this->HighSpeedFanPower * FanPowerAdjustFac * NumCellOn / this->NumCell;

		}

	}

	Real64
	Towerspecs::VSMerkelResidual(
		Real64 const nsvAirFlowRateRatio, // fan speed ratio (1.0 is continuous, 0.0 is off)
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
		// par(1) = MyLoad [W] , negative is cooling
		// par(2) = water mass flow per cell
		// par(3) = Design UA per cell
		// par(4) = UA adjust factor for wetbulb
		// par(5) = UA adjust factor for water flow rate
		// par(6) = specific heat of water at inlet temp
		// par(7) = water mass flow rate, total [kg/s]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
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

		TargetLoad = Par( 1 );
		WaterMassFlowRatePerCell = Par( 2 );
		UAdesignPerCell = Par( 3 );
		UAwetbulbAdjFac = Par( 4 );
		UAwaterflowAdjFac = Par( 5 );
		CpWater = Par( 6 );
		TotalWaterMassFlowRate = Par( 7 );

		AirFlowRatePerCell = nsvAirFlowRateRatio * this->HighSpeedAirFlowRate / this->NumCell;

		UAairflowAdjFac = CurveValue( this->UAModFuncAirFlowRatioCurvePtr, nsvAirFlowRateRatio );
		UAadjustedPerCell = UAdesignPerCell * UAwetbulbAdjFac * UAairflowAdjFac * UAwaterflowAdjFac;

		this->SimSimpleTower( WaterMassFlowRatePerCell, AirFlowRatePerCell, UAadjustedPerCell, OutletWaterTempTrial );

		Qdot = TotalWaterMassFlowRate * CpWater * ( Node( this->WaterInletNodeNum ).Temp - OutletWaterTempTrial );

		Residuum = std::abs( TargetLoad ) - Qdot;

		return Residuum;
	}

	void
	Towerspecs::CalcVariableSpeedTower()
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
		if ( this->DesWaterMassFlowRate > 0.0 ) {
			WaterMassFlowRatePerCellMin = this->DesWaterMassFlowRate * this->MinFracFlowRate / this->NumCell;
			WaterMassFlowRatePerCellMax = this->DesWaterMassFlowRate * this->MaxFracFlowRate / this->NumCell;

			//round it up to the nearest integer
			NumCellMin = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMax ) + 0.9999 ), this->NumCell );
			NumCellMax = min( int( ( nsvWaterMassFlowRate / WaterMassFlowRatePerCellMin ) + 0.9999 ), this->NumCell );
		}

		// cap min at 1
		if ( NumCellMin <= 0 ) NumCellMin = 1;
		if ( NumCellMax <= 0 ) NumCellMax = 1;

		if ( this->CellCtrl_Num == CellCtrl_MinCell ) {
			NumCellOn = NumCellMin;
		} else {
			NumCellOn = NumCellMax;
		}

		this->NumCellOn = NumCellOn;
		WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;

		// Set inlet and outlet nodes and initialize subroutine variables

		nsvWaterInletNode = this->WaterInletNodeNum;
		nsvWaterOutletNode = this->WaterOutletNodeNum;
		nsvQactual = 0.0;
		nsvCTFanPower = 0.0;
		nsvOutletWaterTemp = Node( nsvWaterInletNode ).Temp;

		nsvWaterUsage = 0.0;
		Twb = this->AirWetBulb;
		TwbCapped = this->AirWetBulb;
		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			TempSetPoint = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempSetPointHi;
		} else {
			assert( false );
		}}

		Tr = Node( nsvWaterInletNode ).Temp - TempSetPoint;
		Ta = TempSetPoint - this->AirWetBulb;

		// Do not RETURN here if flow rate is less than MassFlowTolerance. Check basin heater and then RETURN.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) return;
		// MassFlowTolerance is a parameter to indicate a no flow condition
		if ( nsvWaterMassFlowRate <= MassFlowTolerance ) {
			CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, nsvBasinHeaterPower );
			return;
		}

		//loop to increment NumCell if we cannot meet the setpoint with the actual number of cells calculated above
		IncrNumCellFlag = true;
		while ( IncrNumCellFlag ) {
			IncrNumCellFlag = false;
			// Initialize inlet node water properties
			WaterDensity = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, Node( nsvWaterInletNode ).Temp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
			WaterFlowRateRatio = WaterMassFlowRatePerCell / ( WaterDensity * this->CalibratedWaterFlowRate / this->NumCell );

			// check independent inputs with respect to model boundaries
			this->CheckModelBounds( Twb, Tr, Ta, WaterFlowRateRatio, TwbCapped, TrCapped, TaCapped, WaterFlowRateRatioCapped );

			//   determine the free convection capacity by finding the outlet temperature at full air flow and multiplying
			//   the tower's full capacity temperature difference by the percentage of tower capacity in free convection
			//   regime specified by the user

			nsvAirFlowRateRatio = 1.0;
			OutletWaterTempOFF = Node( nsvWaterInletNode ).Temp;
			OutletWaterTempON = Node( nsvWaterInletNode ).Temp;
			nsvOutletWaterTemp = OutletWaterTempOFF;
			FreeConvectionCapFrac = this->FreeConvectionCapacityFraction;

			this->SimVariableTower( WaterFlowRateRatioCapped, nsvAirFlowRateRatio, TwbCapped, OutletWaterTempON );

			if ( OutletWaterTempON > TempSetPoint ) {
				nsvFanCyclingRatio = 1.0;
				nsvAirFlowRateRatio = 1.0;
				nsvCTFanPower = this->HighSpeedFanPower * NumCellOn / this->NumCell;
				nsvOutletWaterTemp = OutletWaterTempON;
				// if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
				if ( NumCellOn < this->NumCell && ( nsvWaterMassFlowRate / ( NumCellOn + 1 ) ) > WaterMassFlowRatePerCellMin ) {
					++NumCellOn;
					WaterMassFlowRatePerCell = nsvWaterMassFlowRate / NumCellOn;
					IncrNumCellFlag = true;
				}
			}
		}

		// find the correct air ratio only if full flow is  too much
		if ( OutletWaterTempON < TempSetPoint ) {
			//   outlet water temperature is calculated in the free convection regime
			OutletWaterTempOFF = Node( nsvWaterInletNode ).Temp - FreeConvectionCapFrac * ( Node( nsvWaterInletNode ).Temp - OutletWaterTempON );
			//   fan is OFF
			nsvFanCyclingRatio = 0.0;
			//   air flow ratio is assumed to be the fraction of tower capacity in the free convection regime (fan is OFF but air is flowing)
			nsvAirFlowRateRatio = FreeConvectionCapFrac;

			// Assume setpoint was met using free convection regime (pump ON and fan OFF)
			nsvCTFanPower = 0.0;
			nsvOutletWaterTemp = OutletWaterTempOFF;

			if ( OutletWaterTempOFF > TempSetPoint ) {
				// Setpoint was not met, turn on cooling tower fan at minimum fan speed

				nsvAirFlowRateRatio = this->MinimumVSAirFlowFrac;
				this->SimVariableTower( WaterFlowRateRatioCapped, nsvAirFlowRateRatio, TwbCapped, OutletWaterTempMIN );

				if ( OutletWaterTempMIN < TempSetPoint ) {
					//         if setpoint was exceeded, cycle the fan at minimum air flow to meet the setpoint temperature
					if ( this->FanPowerfAirFlowCurve == 0 ) {
						nsvCTFanPower = pow_3( nsvAirFlowRateRatio ) * this->HighSpeedFanPower * NumCellOn / this->NumCell;
					} else {
						FanCurveValue = CurveValue( this->FanPowerfAirFlowCurve, nsvAirFlowRateRatio );
						nsvCTFanPower = max( 0.0, ( this->HighSpeedFanPower * FanCurveValue ) ) * NumCellOn / this->NumCell;
					}
					//       fan is cycling ON and OFF at the minimum fan speed. Adjust fan power and air flow rate ratio according to cycling rate
					nsvFanCyclingRatio = ( ( OutletWaterTempOFF - TempSetPoint ) / ( OutletWaterTempOFF - OutletWaterTempMIN ) );
					nsvCTFanPower *= nsvFanCyclingRatio;
					nsvOutletWaterTemp = TempSetPoint;
					nsvAirFlowRateRatio = ( nsvFanCyclingRatio * this->MinimumVSAirFlowFrac ) + ( ( 1 - nsvFanCyclingRatio ) * FreeConvectionCapFrac );
				} else {
					//       if setpoint was not met at minimum fan speed, set fan speed to maximum
					nsvAirFlowRateRatio = 1.0;
					//         fan will not cycle and runs the entire time step
					nsvFanCyclingRatio = 1.0;

					this->SimVariableTower( WaterFlowRateRatioCapped, nsvAirFlowRateRatio, TwbCapped, nsvOutletWaterTemp );

					// Setpoint was met with pump ON and fan ON at full flow
					// Calculate the fraction of full air flow to exactly meet the setpoint temperature

					//         cap the water flow rate ratio and inlet air wet-bulb temperature to provide a stable output
					Par( 1 ) = WaterFlowRateRatioCapped; // water flow rate ratio
					Par( 2 ) = TwbCapped; // Inlet air wet-bulb temperature [C]
					//         do not cap desired range and approach temperature to provide a valid (balanced) output for this simulation time step
					Par( 3 ) = Tr; // Tower range temperature [C]
					Par( 4 ) = Ta; // desired approach temperature [C]
					Par( 5 ) = 1.0; // calculate the air flow rate ratio required for a balance

					SolveRegulaFalsi( Acc, MaxIte, SolFla, nsvAirFlowRateRatio, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return SimpleTowerApproachResidual( X, Par ); }, this->MinimumVSAirFlowFrac, 1.0, Par );
					if ( SolFla == -1 ) {
						if ( ! WarmupFlag ) ShowWarningError( "Cooling tower iteration limit exceeded when calculating air flow rate ratio for tower " + this->Name );
						//           IF RegulaFalsi cannot find a solution then provide detailed output for debugging
					} else if ( SolFla == -2 ) {
						if ( ! WarmupFlag ) {
							gio::write( OutputChar, OutputFormat ) << TwbCapped;
							gio::write( OutputChar2, OutputFormat ) << Tr;
							gio::write( OutputChar3, OutputFormat ) << Ta;
							gio::write( OutputChar4, OutputFormat ) << WaterFlowRateRatioCapped;
							gio::write( OutputChar5, OutputFormat ) << this->MinimumVSAirFlowFrac;
							if ( this->CoolingTowerAFRRFailedCount < 1 ) {
								++this->CoolingTowerAFRRFailedCount;
								ShowWarningError( "CoolingTower:VariableSpeed \"" + this->Name + "\" - Cooling tower air flow rate ratio calculation failed " );
								ShowContinueError( "...with conditions as Twb = " + OutputChar + ", Trange = " + OutputChar2 + ", Tapproach = " + OutputChar3 + ", and water flow rate ratio = " + OutputChar4 );
								ShowContinueError( "...a solution could not be found within the valid range of air flow rate ratios" );
								ShowContinueErrorTimeStamp( " ...Valid air flow rate ratio range = " + OutputChar5 + " to 1.0." );
								ShowContinueError( "...Consider modifying the design approach or design range temperature for this tower." );
							} else {
								ShowRecurringWarningErrorAtEnd( "CoolingTower:VariableSpeed \"" + this->Name + "\" - Cooling tower air flow rate ratio calculation failed error continues.", this->CoolingTowerAFRRFailedIndex );
							}
						}
					}

					//         Use theoretical cubic for deterination of fan power if user has not specified a fan power ratio curve
					if ( this->FanPowerfAirFlowCurve == 0 ) {
						nsvCTFanPower = pow_3( nsvAirFlowRateRatio ) * this->HighSpeedFanPower * NumCellOn / this->NumCell;
					} else {
						FanCurveValue = CurveValue( this->FanPowerfAirFlowCurve, nsvAirFlowRateRatio );
						nsvCTFanPower = max( 0.0, ( this->HighSpeedFanPower * FanCurveValue ) ) * NumCellOn / this->NumCell;
					}
					//           outlet water temperature is calculated as the inlet air wet-bulb temperature plus tower approach temperature
					nsvOutletWaterTemp = Twb + Ta;
				} // IF(OutletWaterTempMIN .LT. TempSetPoint)THEN

			} // IF(OutletWaterTempOFF .GT. TempSetPoint)THEN
		} // IF(OutletWaterTempON .LT. TempSetPoint) ie if tower should not run at full capacity

		CpWater = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, Node( this->WaterInletNodeNum ).Temp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
		nsvQactual = nsvWaterMassFlowRate * CpWater * ( Node( nsvWaterInletNode ).Temp - nsvOutletWaterTemp );
		this->NumCellOn = NumCellOn;
		// Set water and air properties
		AirDensity = PsyRhoAirFnPbTdbW( this->AirPress, this->AirTemp, this->AirHumRat );
		AirMassFlowRate = nsvAirFlowRateRatio * this->HighSpeedAirFlowRate * AirDensity * this->NumCellOn / this->NumCell;
		InletAirEnthalpy = PsyHFnTdbRhPb( this->AirWetBulb, 1.0, this->AirPress );

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( this->PrintLGMessage ) {
				++this->VSErrorCountFlowFrac;
				//       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( this->VSErrorCountFlowFrac < 2 ) {
					ShowWarningError( this->LGBuffer1 );
					ShowContinueError( this->LGBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\" - Liquid to gas ratio is out of range error continues...", this->ErrIndexLG, this->LGLast, this->LGLast );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//   warn user on first occurrence if flow fraction is greater than maximum for the YorkCalc model, use recurring warning stats
		if ( this->TowerModelType == YorkCalcModel || this->TowerModelType == YorkCalcUserDefined ) {
			this->PrintLGMessage = false;
			//      Do not report error message in free convection regime
			if ( nsvAirFlowRateRatio > this->MinimumVSAirFlowFrac ) {
				FlowFraction = WaterFlowRateRatioCapped / nsvAirFlowRateRatio;
				//        Flow fractions greater than a MaxLiquidToGasRatio of 8 are not reliable using the YorkCalc model
				if ( FlowFraction > this->MaxLiquidToGasRatio ) {
					//          Report warnings only during actual simulation
					if ( ! WarmupFlag ) {
						this->PrintLGMessage = true;
						gio::write( OutputChar, OutputFormat ) << FlowFraction;
						gio::write( OutputChar2, OutputFormat ) << this->MaxLiquidToGasRatio;
						this->LGBuffer1 = this->TowerType + " \"" + this->Name + "\" - Liquid to gas ratio (L/G) is out of range at " + OutputChar + '.';
						this->LGBuffer2 = " ...Valid maximum ratio = " + OutputChar2 + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
						this->LGLast = FlowFraction;
					}
				}
			}
		}

	}

	void
	Towerspecs::SimSimpleTower(
		Real64 const nsvWaterMassFlowRate,
		Real64 const AirFlowRate,
		Real64 const UAdesign,
		Real64 & nsvOutletWaterTemp
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
		Real64 localQactual; // Actual heat transfer rate between tower water and air [W]

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

		nsvWaterInletNode = this->WaterInletNodeNum;
		nsvWaterOutletNode = this->WaterOutletNodeNum;
		localQactual = 0.0;
		//    WetBulbTolerance  = 0.00001
		WetBulbError = 1.0;
		//    IterMax           = 50
		DeltaTwb = 1.0;
		//    DeltaTwbTolerance = 0.001

		// set local tower inlet and outlet temperature variables
		nsvInletWaterTemp = this->WaterTemp;
		nsvOutletWaterTemp = nsvInletWaterTemp;
		InletAirTemp = this->AirTemp;
		InletAirWetBulb = this->AirWetBulb;

		if ( UAdesign == 0.0 ) return;

		// set water and air properties
		AirDensity = PsyRhoAirFnPbTdbW( this->AirPress, InletAirTemp, this->AirHumRat );
		AirMassFlowRate = AirFlowRate * AirDensity;
		CpAir = PsyCpAirFnWTdb( this->AirHumRat, InletAirTemp );
		CpWater = GetSpecificHeatGlycol( PlantLoop( this->LoopNum ).FluidName, this->WaterTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );
		InletAirEnthalpy = PsyHFnTdbRhPb( this->AirWetBulb, 1.0, this->AirPress );

		// initialize exiting wet bulb temperature before iterating on final solution
		OutletAirWetBulb = InletAirWetBulb + 6.0;

		// Calcluate mass flow rates
		if ( nsvWaterMassFlowRate > 0.0 ) {
			MdotCpWater = nsvWaterMassFlowRate * CpWater;
		} else {
			nsvOutletWaterTemp = nsvInletWaterTemp;
			return;
		}
		Iter = 0;
		while ( ( WetBulbError > WetBulbTolerance ) && ( Iter <= IterMax ) && ( DeltaTwb > DeltaTwbTolerance ) ) {
			++Iter;
			//        OutletAirEnthalpy = PsyHFnTdbRhPb(OutletAirWetBulb,1.0,OutBaroPress)
			OutletAirEnthalpy = PsyHFnTdbRhPb( OutletAirWetBulb, 1.0, this->AirPress );
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
			localQactual = effectiveness * CapacityRatioMin * ( nsvInletWaterTemp - InletAirWetBulb );
			OutletAirWetBulbLast = OutletAirWetBulb;
			// calculate new exiting wet bulb temperature of airstream
			OutletAirWetBulb = InletAirWetBulb + localQactual / AirCapacity;
			// Check error tolerance and exit if satisfied
			DeltaTwb = std::abs( OutletAirWetBulb - InletAirWetBulb );
			// Add KelvinConv to denominator below convert OutletAirWetBulbLast to Kelvin to avoid divide by zero.
			// Wet bulb error units are delta K/K
			WetBulbError = std::abs( ( OutletAirWetBulb - OutletAirWetBulbLast ) / ( OutletAirWetBulbLast + KelvinConv ) );
		}

		if ( localQactual >= 0.0 ) {
			nsvOutletWaterTemp = nsvInletWaterTemp - localQactual / MdotCpWater;
		} else {
			nsvOutletWaterTemp = nsvInletWaterTemp;
		}

	}

	void
	Towerspecs::SimVariableTower(
		Real64 const WaterFlowRateRatio, // current water flow rate ratio (capped if applicable)
		Real64 const nsvAirFlowRateRatio, // current air flow rate ratio
		Real64 const Twb, // current inlet air wet-bulb temperature (C, capped if applicable)
		Real64 & nsvOutletWaterTemp // calculated tower outlet water temperature (C)
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
		Par( 1 ) = WaterFlowRateRatio; // water flow rate ratio
		Par( 2 ) = nsvAirFlowRateRatio; // air flow rate ratio
		Par( 3 ) = Twb; // inlet air wet-bulb temperature [C]
		SolveRegulaFalsi( Acc, MaxIte, SolFla, Tr, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return simpleTowerTrResidual( X, Par ); }, 0.001, this->MaxRangeTemp, Par );
		
		nsvOutletWaterTemp = this->WaterTemp - Tr;

		if ( SolFla == -1 ) {
			ShowSevereError( "Iteration limit exceeded in calculating tower nominal capacity at minimum air flow ratio" );
			ShowContinueError( "Design inlet air wet-bulb or approach temperature must be modified to achieve an acceptable range at the minimum air flow rate" );
			ShowContinueError( "Cooling tower simulation failed to converge for tower " + this->Name );
			//    if SolFla = -2, Tr is returned as minimum value (0.001) and outlet temp = inlet temp - 0.001
		} else if ( SolFla == -2 ) { // decide if should run at max flow
			{ auto const SELECT_CASE_var( PlantLoop( this->LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				TempSetPoint = PlantLoop( this->LoopNum ).LoopSide( this->LoopSideNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				TempSetPoint = PlantLoop( this->LoopNum ).LoopSide( this->LoopSideNum ).TempSetPointHi;
			} else {
				assert( false );
			}}
			if ( this->WaterTemp > ( TempSetPoint + this->MaxRangeTemp ) ) { // run flat out
				nsvOutletWaterTemp = this->WaterTemp - this->MaxRangeTemp;
			}
		}

	}

	void
	Towerspecs::CalcVSTowerApproach(
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

		if ( this->TowerModelType == YorkCalcModel || this->TowerModelType == YorkCalcUserDefined ) {
			PctAirFlow = AirFlowRatio;
			FlowFactor = PctWaterFlow / PctAirFlow;
			Approach = this->Coeff( 1 ) + this->Coeff( 2 ) * Twb + this->Coeff( 3 ) * Twb * Twb + this->Coeff( 4 ) * Tr + this->Coeff( 5 ) * Twb * Tr + this->Coeff( 6 ) * Twb * Twb * Tr + this->Coeff( 7 ) * Tr * Tr + this->Coeff( 8 ) * Twb * Tr * Tr + this->Coeff( 9 ) * Twb * Twb * Tr * Tr + this->Coeff( 10 ) * FlowFactor + this->Coeff( 11 ) * Twb * FlowFactor + this->Coeff( 12 ) * Twb * Twb * FlowFactor + this->Coeff( 13 ) * Tr * FlowFactor + this->Coeff( 14 ) * Twb * Tr * FlowFactor + this->Coeff( 15 ) * Twb * Twb * Tr * FlowFactor + this->Coeff( 16 ) * Tr * Tr * FlowFactor + this->Coeff( 17 ) * Twb * Tr * Tr * FlowFactor + this->Coeff( 18 ) * Twb * Twb * Tr * Tr * FlowFactor + this->Coeff( 19 ) * FlowFactor * FlowFactor + this->Coeff( 20 ) * Twb * FlowFactor * FlowFactor + this->Coeff( 21 ) * Twb * Twb * FlowFactor * FlowFactor + this->Coeff( 22 ) * Tr * FlowFactor * FlowFactor + this->Coeff( 23 ) * Twb * Tr * FlowFactor * FlowFactor + this->Coeff( 24 ) * Twb * Twb * Tr * FlowFactor * FlowFactor + this->Coeff( 25 ) * Tr * Tr * FlowFactor * FlowFactor + this->Coeff( 26 ) * Twb * Tr * Tr * FlowFactor * FlowFactor + this->Coeff( 27 ) * Twb * Twb * Tr * Tr * FlowFactor * FlowFactor;

		} else { // empirical model is CoolTools format

			//     the CoolTools model actually uses PctFanPower = AirFlowRatio^3 as an input to the model
			PctAirFlow = pow_3( AirFlowRatio );
			Approach = this->Coeff( 1 ) + this->Coeff( 2 ) * PctAirFlow + this->Coeff( 3 ) * PctAirFlow * PctAirFlow + this->Coeff( 4 ) * PctAirFlow * PctAirFlow * PctAirFlow + this->Coeff( 5 ) * PctWaterFlow + this->Coeff( 6 ) * PctAirFlow * PctWaterFlow + this->Coeff( 7 ) * PctAirFlow * PctAirFlow * PctWaterFlow + this->Coeff( 8 ) * PctWaterFlow * PctWaterFlow + this->Coeff( 9 ) * PctAirFlow * PctWaterFlow * PctWaterFlow + this->Coeff( 10 ) * PctWaterFlow * PctWaterFlow * PctWaterFlow + this->Coeff( 11 ) * Twb + this->Coeff( 12 ) * PctAirFlow * Twb + this->Coeff( 13 ) * PctAirFlow * PctAirFlow * Twb + this->Coeff( 14 ) * PctWaterFlow * Twb + this->Coeff( 15 ) * PctAirFlow * PctWaterFlow * Twb + this->Coeff( 16 ) * PctWaterFlow * PctWaterFlow * Twb + this->Coeff( 17 ) * Twb * Twb + this->Coeff( 18 ) * PctAirFlow * Twb * Twb + this->Coeff( 19 ) * PctWaterFlow * Twb * Twb + this->Coeff( 20 ) * Twb * Twb * Twb + this->Coeff( 21 ) * Tr + this->Coeff( 22 ) * PctAirFlow * Tr + this->Coeff( 23 ) * PctAirFlow * PctAirFlow * Tr + this->Coeff( 24 ) * PctWaterFlow * Tr + this->Coeff( 25 ) * PctAirFlow * PctWaterFlow * Tr + this->Coeff( 26 ) * PctWaterFlow * PctWaterFlow * Tr + this->Coeff( 27 ) * Twb * Tr + this->Coeff( 28 ) * PctAirFlow * Twb * Tr + this->Coeff( 29 ) * PctWaterFlow * Twb * Tr + this->Coeff( 30 ) * Twb * Twb * Tr + this->Coeff( 31 ) * Tr * Tr + this->Coeff( 32 ) * PctAirFlow * Tr * Tr + this->Coeff( 33 ) * PctWaterFlow * Tr * Tr + this->Coeff( 34 ) * Twb * Tr * Tr + this->Coeff( 35 ) * Tr * Tr * Tr;
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
	Towerspecs::CheckModelBounds(
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
			if ( this->PrintTrMessage ) {
				++this->VSErrorCountTR;
				if ( this->VSErrorCountTR < 2 ) {
					ShowWarningError( this->TrBuffer1 );
					ShowContinueError( this->TrBuffer2 );
					ShowContinueError( this->TrBuffer3 );
					ShowContinueError( " ...Range temperatures outside model boundaries may not adversely affect tower performance." );
					ShowContinueError( " ...This is not an unexpected occurrence when simulating actual conditions." );
				} else {
					ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\" - Tower range temperature is out of range error continues...", this->ErrIndexTR, this->TrLast, this->TrLast );
				}
			}
			if ( this->PrintTwbMessage ) {
				++this->VSErrorCountIAWB;
				if ( this->VSErrorCountIAWB < 6 ) {
					ShowWarningError( this->TwbBuffer1 );
					ShowContinueError( this->TwbBuffer2 );
					ShowContinueError( this->TwbBuffer3 );
					ShowContinueError( " ...Wet-bulb temperatures outside model boundaries may not adversely affect tower performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\" - Inlet air wet-bulb temperature is out of range error continues...", this->ErrIndexIAWB, this->TwbLast, this->TwbLast );
				}
			}
			if ( this->PrintTaMessage ) {
				++this->VSErrorCountTA;
				if ( this->VSErrorCountTA < 2 ) {
					ShowWarningError( this->TaBuffer1 );
					ShowContinueError( this->TaBuffer2 );
					ShowContinueError( this->TaBuffer3 );
					ShowContinueError( " ...Approach temperatures outside model boundaries may not adversely affect tower performance." );
					ShowContinueError( " ...This is not an unexpected occurrence when simulating actual conditions." );
				} else {
					ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\" - Tower approach temperature is out of range error continues...", this->ErrIndexTA, this->TaLast, this->TaLast );
				}
			}
			if ( this->PrintWFRRMessage ) {
				++this->VSErrorCountWFRR;
				if ( this->VSErrorCountWFRR < 6 ) {
					ShowWarningError( this->WFRRBuffer1 );
					ShowContinueError( this->WFRRBuffer2 );
					ShowContinueError( this->WFRRBuffer3 );
					ShowContinueError( " ...Water flow rate ratios outside model boundaries may not adversely affect tower performance." );
				} else {
					ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\" - Water flow rate ratio is out of range error continues...", this->ErrIndexWFRR, this->WaterFlowRateRatioLast, this->WaterFlowRateRatioLast );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
		if ( Twb < this->MinInletAirWBTemp || Twb > this->MaxInletAirWBTemp ) {
			OutputChar = RoundSigDigits( Twb, 2 );
			OutputCharLo = RoundSigDigits( this->MinInletAirWBTemp, 2 );
			OutputCharHi = RoundSigDigits( this->MaxInletAirWBTemp, 2 );
			if ( Twb < this->MinInletAirWBTemp ) {
				TwbCapped = this->MinInletAirWBTemp;
			}
			if ( Twb > this->MaxInletAirWBTemp ) {
				TwbCapped = this->MaxInletAirWBTemp;
			}
			if ( ! WarmupFlag ) {
				this->PrintTwbMessage = true;
				this->TwbBuffer1 = this->TowerType + " \"" + this->Name + "\" - Inlet air wet-bulb temperature is outside model boundaries at " + OutputChar + '.';
				this->TwbBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				TrimValue = RoundSigDigits( TwbCapped, 6 );
				this->TwbBuffer3 = " ...Inlet air wet-bulb temperature passed to the model = " + TrimValue;
				this->TwbLast = Twb;
			} else {
				this->PrintTwbMessage = false;
			}
		} else {
			this->PrintTwbMessage = false;
		}

		if ( Tr < this->MinRangeTemp || Tr > this->MaxRangeTemp ) {
			OutputChar = RoundSigDigits( Tr, 2 );
			OutputCharLo = RoundSigDigits( this->MinRangeTemp, 2 );
			OutputCharHi = RoundSigDigits( this->MaxRangeTemp, 2 );
			if ( Tr < this->MinRangeTemp ) {
				TrCapped = this->MinRangeTemp;
			}
			if ( Tr > this->MaxRangeTemp ) {
				TrCapped = this->MaxRangeTemp;
			}
			if ( ! WarmupFlag ) {
				this->PrintTrMessage = true;
				this->TrBuffer1 = this->TowerType + " \"" + this->Name + "\" - Tower range temperature is outside model boundaries at " + OutputChar + '.';
				this->TrBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				TrimValue = RoundSigDigits( Tr, 5 );
				this->TrBuffer3 = " ...Tower range temperature passed to the model = " + TrimValue;
				this->TrLast = Tr;
			} else {
				this->PrintTrMessage = false;
			}
		} else {
			this->PrintTrMessage = false;
		}

		if ( Ta < this->MinApproachTemp || Ta > this->MaxApproachTemp ) {
			OutputChar = RoundSigDigits( Ta, 2 );
			OutputCharLo = RoundSigDigits( this->MinApproachTemp, 2 );
			OutputCharHi = RoundSigDigits( this->MaxApproachTemp, 2 );
			if ( Ta < this->MinApproachTemp ) {
				TaCapped = this->MinApproachTemp;
			}
			if ( Ta > this->MaxApproachTemp ) {
				TaCapped = this->MaxApproachTemp;
			}
			if ( ! WarmupFlag ) {
				this->PrintTaMessage = true;
				this->TaBuffer1 = this->TowerType + " \"" + this->Name + "\" - Tower approach temperature is outside model boundaries at " + OutputChar + '.';
				this->TaBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				TrimValue = RoundSigDigits( Ta, 5 );
				this->TaBuffer3 = " ...Tower approach temperature passed to the model = " + TrimValue;
				this->TaLast = Ta;
			} else {
				this->PrintTaMessage = false;
			}
		} else {
			this->PrintTaMessage = false;
		}

		if ( this->TowerModelType == YorkCalcModel || this->TowerModelType == YorkCalcUserDefined ) {
			//     Water flow rate ratio warning not valid for YorkCalc model, print liquid to gas ratio
			//     warning instead (bottom of Subroutine VariableSpeedTower)
			this->PrintWFRRMessage = false;
		} else {
			if ( WaterFlowRateRatio < this->MinWaterFlowRatio || WaterFlowRateRatio > this->MaxWaterFlowRatio ) {
				OutputChar = RoundSigDigits( WaterFlowRateRatio, 2 );
				OutputCharLo = RoundSigDigits( this->MinWaterFlowRatio, 2 );
				OutputCharHi = RoundSigDigits( this->MaxWaterFlowRatio, 2 );
				if ( WaterFlowRateRatio < this->MinWaterFlowRatio ) {
					WaterFlowRateRatioCapped = this->MinWaterFlowRatio;
				}
				if ( WaterFlowRateRatio > this->MaxWaterFlowRatio ) {
					WaterFlowRateRatioCapped = this->MaxWaterFlowRatio;
				}
				if ( ! WarmupFlag ) {
					this->PrintWFRRMessage = true;
					this->WFRRBuffer1 = this->TowerType + " \"" + this->Name + "\" - Water flow rate ratio is outside model boundaries at " + OutputChar + '.';
					this->WFRRBuffer2 = " ...Valid range = " + OutputCharLo + " to " + OutputCharHi + ". Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
					TrimValue = RoundSigDigits( WaterFlowRateRatioCapped, 5 );
					this->WFRRBuffer3 = " ...Water flow rate ratio passed to the model = " + TrimValue;
					this->WaterFlowRateRatioLast = WaterFlowRateRatio;
				} else {
					this->PrintWFRRMessage = false;
				}
			} else {
				this->PrintWFRRMessage = false;
			}
		}

	}

	Real64
	Towerspecs::simpleTowerUAResidual(
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
		// par(1) = tower load [W]
		// par(2) = design water mass flow rate [kg/s]
		// par(3) = design air volume flow rate [m3/s]
		// par(4) = water specific heat [J/(kg*C)]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 OutWaterTemp; // outlet water temperature [C]
		Real64 CoolingOutput; // tower cooling output [W]

		this->SimSimpleTower( Par( 2 ), Par( 3 ), UA, OutWaterTemp );
		CoolingOutput = Par( 4 ) * Par( 2 ) * ( this->WaterTemp - OutWaterTemp );
		Residuum = ( Par( 1 ) - CoolingOutput ) / Par( 1 );
		return Residuum;
	}

	Real64
	Towerspecs::simpleTowerTrResidual(
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
		// par(1) = water flow ratio
		// par(2) = air flow ratio
		// par(3) = inlet air wet-bulb temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 WaterFlowRateRatio; // ratio of water flow rate to design water flow rate
		Real64 InletAirWB; // inlet air wet-bulb temperature [C]
		Real64 Tapproach; // tower approach temperature [C]

		WaterFlowRateRatio = Par( 1 );
		AirFlowRateRatio = Par( 2 );
		InletAirWB = Par( 3 );
		Tapproach = 0.0;

		// call model to determine approach temperature given other independent variables (range temp is being varied to find balance)
		this->CalcVSTowerApproach( WaterFlowRateRatio, AirFlowRateRatio, InletAirWB, Trange, Tapproach );
		// calculate residual based on a balance where Twb + Ta + Tr = Node(WaterInletNode)%Temp
		Residuum = ( InletAirWB + Tapproach + Trange ) - Node( this->WaterInletNodeNum ).Temp;

		return Residuum;
	}

	Real64
	Towerspecs::SimpleTowerApproachResidual(
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
		// par(1) = water or air flow ratio (opposite of input variable)
		// par(2) = inlet air wet-bulb temp [C]
		// par(3) = tower range [C]
		// par(4) = desired approach [C]
		// par(5) = 0.0 to calculate water flow rate ratio, 1.0 for air

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 WaterFlowRateRatio; // ratio of water flow rate to design water flow rate
		Real64 InletAirWB; // inlet air wet-bulb temperature [C]
		Real64 Trange; // tower range temperature [C]
		Real64 TapproachActual; // actual tower approach temperature [C]
		Real64 TapproachDesired; // desired tower approach temperature [C]

		if ( Par( 5 ) == 0.0 ) {
			AirFlowRateRatio = Par( 1 );
			WaterFlowRateRatio = FlowRatio;
		} else {
			AirFlowRateRatio = FlowRatio;
			WaterFlowRateRatio = Par( 1 );
		}
		InletAirWB = Par( 2 );
		Trange = Par( 3 );
		TapproachDesired = Par( 4 );
		TapproachActual = 0.0;

		// call model to determine tower approach temperature given other independent variables
		this->CalcVSTowerApproach( WaterFlowRateRatio, AirFlowRateRatio, InletAirWB, Trange, TapproachActual );
		Residuum = TapproachDesired - TapproachActual;

		return Residuum;
	}

	// End of the CondenserLoopTowers Module Simulation Subroutines

	// *****************************************************************************

	void
	Towerspecs::CalculateWaterUseage()
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

		AverageWaterTemp = ( nsvInletWaterTemp + nsvOutletWaterTemp ) / 2.0;

		// Set water and air properties
		if ( this->EvapLossMode == EvapLossByMoistTheory ) {

			AirDensity = PsyRhoAirFnPbTdbW( this->AirPress, this->AirTemp, this->AirHumRat );
			AirMassFlowRate = nsvAirFlowRateRatio * this->HighSpeedAirFlowRate * AirDensity * this->NumCellOn / this->NumCell;
			InletAirEnthalpy = PsyHFnTdbRhPb( this->AirWetBulb, 1.0, this->AirPress );

			if ( AirMassFlowRate > 0.0 ) {
				// Calculate outlet air conditions for determining water usage

				OutletAirEnthalpy = InletAirEnthalpy + nsvQactual / AirMassFlowRate;
				OutletAirTSat = PsyTsatFnHPb( OutletAirEnthalpy, this->AirPress );
				OutletAirHumRatSat = PsyWFnTdbH( OutletAirTSat, OutletAirEnthalpy );

				// calculate specific humidity ratios (HUMRAT to mass of moist air not dry air)
				InSpecificHumRat = this->AirHumRat / ( 1 + this->AirHumRat );
				OutSpecificHumRat = OutletAirHumRatSat / ( 1 + OutletAirHumRatSat );

				// calculate average air temp for density call
				TairAvg = ( this->AirTemp + OutletAirTSat ) / 2.0;

				// Amount of water evaporated, get density water at air temp or 4 C if too cold
				rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, max( TairAvg, 4.0 ), PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

				EvapVdot = ( AirMassFlowRate * ( OutSpecificHumRat - InSpecificHumRat ) ) / rho; // [m3/s]
				if ( EvapVdot < 0.0 ) EvapVdot = 0.0;
			} else {
				EvapVdot = 0.0;
			}

		} else if ( this->EvapLossMode == EvapLossByUserFactor ) {
			//    EvapVdot   = SimpleTower(TowerNum)%UserEvapLossFactor * (InletWaterTemp - nsvOutletWaterTemp) &
			//                     * SimpleTower(TowerNum)%DesignWaterFlowRate
			rho = GetDensityGlycol( PlantLoop( this->LoopNum ).FluidName, AverageWaterTemp, PlantLoop( this->LoopNum ).FluidIndex, RoutineName );

			EvapVdot = this->UserEvapLossFactor * ( nsvInletWaterTemp - nsvOutletWaterTemp ) * ( nsvWaterMassFlowRate / rho );
			if ( EvapVdot < 0.0 ) EvapVdot = 0.0;
		} else {
			// should never come here
		}

		//   amount of water lost due to drift
		DriftVdot = this->DesignWaterFlowRate * this->NumCellOn / this->NumCell * this->DriftLossFraction * nsvAirFlowRateRatio;

		if ( this->BlowdownMode == BlowdownBySchedule ) {
			// Amount of water lost due to blow down (purging contaminants from tower basin)
			if ( this->SchedIDBlowdown > 0 ) {
				BlowDownVdot = GetCurrentScheduleValue( this->SchedIDBlowdown );
			} else {
				BlowDownVdot = 0.0;
			}
		} else if ( this->BlowdownMode == BlowdownByConcentration ) {
			if ( this->ConcentrationRatio > 2.0 ) { // protect divide by zero
				BlowDownVdot = EvapVdot / ( this->ConcentrationRatio - 1 ) - DriftVdot;
			} else {
				BlowDownVdot = EvapVdot - DriftVdot;
			}
			if ( BlowDownVdot < 0.0 ) BlowDownVdot = 0.0;
		} else {
			//should never come here
		}

		// Added for fluid bypass
		if ( this->CapacityControl == CapacityControl_FluidBypass ) {
			if ( this->EvapLossMode == EvapLossByUserFactor ) EvapVdot *= ( 1 - this->BypassFraction );
			DriftVdot *= ( 1 - this->BypassFraction );
			BlowDownVdot *= ( 1 - this->BypassFraction );
		}

		MakeUpVdot = EvapVdot + DriftVdot + BlowDownVdot;

		// set demand request in Water STorage if needed
		StarvedVdot = 0.0;
		TankSupplyVdot = 0.0;
		if ( this->SuppliedByWaterSystem ) {

			// set demand request
			WaterStorage( this->WaterTankID ).VdotRequestDemand( this->WaterTankDemandARRID ) = MakeUpVdot;

			AvailTankVdot = WaterStorage( this->WaterTankID ).VdotAvailDemand( this->WaterTankDemandARRID ); // check what tank can currently provide

			TankSupplyVdot = MakeUpVdot; // init
			if ( AvailTankVdot < MakeUpVdot ) { // calculate starved flow
				StarvedVdot = MakeUpVdot - AvailTankVdot;
				TankSupplyVdot = AvailTankVdot;
			}
		} else { // supplied by mains

		}

		//   total water usage
		// update report variables
		this->EvaporationVdot = EvapVdot;
		this->EvaporationVol = EvapVdot * ( TimeStepSys * SecInHour );
		this->DriftVdot = DriftVdot;
		this->DriftVol = DriftVdot * ( TimeStepSys * SecInHour );
		this->BlowdownVdot = BlowDownVdot;
		this->BlowdownVol = BlowDownVdot * ( TimeStepSys * SecInHour );
		this->MakeUpVdot = MakeUpVdot;
		this->MakeUpVol = MakeUpVdot * ( TimeStepSys * SecInHour );
		this->TankSupplyVdot = TankSupplyVdot;
		this->TankSupplyVol = TankSupplyVdot * ( TimeStepSys * SecInHour );
		this->StarvedMakeUpVdot = StarvedVdot;
		this->StarvedMakeUpVol = StarvedVdot * ( TimeStepSys * SecInHour );

	}

	// Beginning of Record Keeping subroutines for the Tower Module
	// *****************************************************************************

	void
	Towerspecs::UpdateTowers()
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

		Node( nsvWaterOutletNode ).Temp = nsvOutletWaterTemp;

		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 || WarmupFlag ) return;

		//Check flow rate through tower and compare to design flow rate, show warning if greater than Design * Mulitplier
		if ( Node( nsvWaterOutletNode ).MassFlowRate > this->DesWaterMassFlowRate * this->TowerMassFlowRateMultiplier ) {
			++this->HighMassFlowErrorCount;
			if ( this->HighMassFlowErrorCount < 2 ) {
				ShowWarningError( this->TowerType + " \"" + this->Name + "\"" );
				ShowContinueError( " Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate." );
				ShowContinueError( " Condenser Loop Mass Flow Rate = " + TrimSigDigits( Node( nsvWaterOutletNode ).MassFlowRate, 6 ) );
				ShowContinueError( " Tower Design Mass Flow Rate   = " + TrimSigDigits( this->DesWaterMassFlowRate, 6 ) );
				ShowContinueErrorTimeStamp( "" );
			} else {
				ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\"  Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate error continues...", this->HighMassFlowErrorIndex, Node( nsvWaterOutletNode ).MassFlowRate, Node( nsvWaterOutletNode ).MassFlowRate );
			}
		}

		// Check if nsvOutletWaterTemp is below the minimum condenser loop temp and warn user
		LoopMinTemp = PlantLoop( LoopNum ).MinTemp;
		if ( nsvOutletWaterTemp < LoopMinTemp && nsvWaterMassFlowRate > 0.0 ) {
			++this->OutletWaterTempErrorCount;
			gio::write( CharLowOutletTemp, LowTempFmt ) << LoopMinTemp;
			gio::write( CharErrOut, LowTempFmt ) << nsvOutletWaterTemp;
			strip( CharErrOut );
			if ( this->OutletWaterTempErrorCount < 2 ) {
				ShowWarningError( this->TowerType + " \"" + this->Name + "\"" );
				ShowContinueError( "Cooling tower water outlet temperature (" + CharErrOut + " C) is below the specified minimum condenser loop temp of " + stripped( CharLowOutletTemp ) + " C" );
				ShowContinueErrorTimeStamp( "" );
			} else {
				ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\" Cooling tower water outlet temperature is below the specified minimum condenser loop temp error continues...", this->OutletWaterTempErrorIndex, nsvOutletWaterTemp, nsvOutletWaterTemp );
			}
		}

		// Check if water mass flow rate is small (e.g. no flow) and warn user
		if ( nsvWaterMassFlowRate > 0.0 && nsvWaterMassFlowRate <= MassFlowTolerance ) {
			++this->SmallWaterMassFlowErrorCount;
			if ( this->SmallWaterMassFlowErrorCount < 2 ) {
				ShowWarningError( this->TowerType + " \"" + this->Name + "\"" );
				ShowContinueError( "Cooling tower water mass flow rate near zero." );
				ShowContinueErrorTimeStamp( "" );
				ShowContinueError( "Actual Mass flow = " + TrimSigDigits( nsvWaterMassFlowRate, 2 ) );
			} else {
				ShowRecurringWarningErrorAtEnd( this->TowerType + " \"" + this->Name + "\"  Cooling tower water mass flow rate near zero error continues...", this->SmallWaterMassFlowErrorIndex, nsvWaterMassFlowRate, nsvWaterMassFlowRate );
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
		//          , SimpleTower(TowerNum)%WMFRLessThanMinAvailErrIndex, nsvWaterMassFlowRate, nsvWaterMassFlowRate)
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
		//          , SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrIndex, nsvWaterMassFlowRate, nsvWaterMassFlowRate)
		//     ENDIF
		//   END IF

	}

	// End of Record Keeping subroutines for the Tower Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Tower Module
	// *****************************************************************************

	void
		Towerspecs::ReportTowers( Real64 const MyLoad )
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

		if ( MyLoad == 0.0 ) {
			this->InletWaterTemp = Node( nsvWaterInletNode ).Temp;
			this->OutletWaterTemp = Node( nsvWaterInletNode ).Temp;
			this->WaterMassFlowRate = nsvWaterMassFlowRate;
			this->Qactual = 0.0;
			this->FanPower = 0.0;
			this->FanEnergy = 0.0;
			this->AirFlowRatio = 0.0;
			this->WaterAmountUsed = 0.0;
			this->BasinHeaterPower = nsvBasinHeaterPower;
			this->BasinHeaterConsumption = nsvBasinHeaterPower * ReportingConstant;
			this->FanCyclingRatio = 0.0;
			this->BypassFraction = 0.0; // added for fluid bypass
			this->NumCellOn = 0;
			this->SpeedSelected = 0;
		} else {
			this->InletWaterTemp = Node( nsvWaterInletNode ).Temp;
			this->OutletWaterTemp = nsvOutletWaterTemp;
			this->WaterMassFlowRate = nsvWaterMassFlowRate;
			this->Qactual = nsvQactual;
			this->FanPower = nsvCTFanPower;
			this->FanEnergy = nsvCTFanPower * ReportingConstant;
			this->AirFlowRatio = nsvAirFlowRateRatio;
			this->WaterAmountUsed = nsvWaterUsage * ReportingConstant;
			this->BasinHeaterPower = nsvBasinHeaterPower;
			this->BasinHeaterConsumption = nsvBasinHeaterPower * ReportingConstant;
			this->FanCyclingRatio = nsvFanCyclingRatio;
		}

	}

} // CondenserLoopTowers

} // EnergyPlus
