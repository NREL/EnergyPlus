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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <CoolTower.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataWater.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

// (ref: Object: COOLTOWER:SHOWER)

namespace CoolTower {
	// Module containing the data for cooltower system

	// MODULE INFORMATION:
	//       AUTHOR         Daeho Kang
	//       DATE WRITTEN   Aug 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithyms required to manage the cooltower component.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// Baruch Givoni. 1994. Passive and Low Energy Cooling of Buildings. Chapter 5: Evaporative Cooling Systems.
	//     John Wiley & Sons, Inc.
	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHeatBalance;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const WaterSupplyFromMains( 101 );
	int const WaterSupplyFromTank( 102 );
	int const WaterFlowSchedule( 0 );
	int const WindDrivenFlow( 1 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	int NumCoolTowers( 0 ); // Total cooltower statements in inputs

	// Subroutine Specifications for the Heat Balance Module

	// Object Data
	Array1D< CoolTowerParams > CoolTowerSys;

	// Functions

	void
	clear_state()
	{
		NumCoolTowers = 0;
		CoolTowerSys.deallocate();
	}

	void
	ManageCoolTower()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   Aug 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the simulation of Cooltower component.
		// This driver manages the calls to all of the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

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
		static bool GetInputFlag( true );
		//unused1208  LOGICAL :: ErrorsFound=.FALSE.
		//unused1208  INTEGER :: CoolTowerNum

		// Obtains and allocates heat balance related parameters from input
		if ( GetInputFlag ) {
			GetCoolTower();
			GetInputFlag = false;
		}

		if ( NumCoolTowers == 0 ) return;

		CalcCoolTower();

		UpdateCoolTower();

		ReportCoolTower();

	}

	void
	GetCoolTower()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   Aug 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets input data for cooltower components
		// and stores it in the Cooltower data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using ScheduleManager::GetScheduleIndex;
		using WaterManager::SetupTankDemandComponent;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "ZoneCoolTower:Shower" );
		Real64 const MaximumWaterFlowRate( 0.016667 ); // Maximum limit of water flow rate in m3/s (1000 l/min)
		Real64 const MinimumWaterFlowRate( 0.0 ); // Minimum limit of water flow rate
		Real64 const MaxHeight( 30.0 ); // Maximum effective tower height in m
		Real64 const MinHeight( 1.0 ); // Minimum effective tower height in m
		Real64 const MaxValue( 100.0 ); // Maximum limit of outlet area, airflow, and temperature
		Real64 const MinValue( 0.0 ); // Minimum limit of outlet area, airflow, and temperature
		Real64 const MaxFrac( 1.0 ); // Maximum fraction
		Real64 const MinFrac( 0.0 ); // Minimum fraction

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int CoolTowerNum; // Cooltower number
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int NumArgs;
		int IOStat;
		Array1D_string cAlphaArgs; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > rNumericArgs; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// Initializations and allocations
		GetObjectDefMaxArgs( CurrentModuleObject, NumArgs, NumAlphas, NumNumbers );
		cAlphaArgs.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		rNumericArgs.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		NumCoolTowers = GetNumObjectsFound( CurrentModuleObject );

		CoolTowerSys.allocate( NumCoolTowers );

		// Obtain inputs
		for ( CoolTowerNum = 1; CoolTowerNum <= NumCoolTowers; ++CoolTowerNum ) {

			GetObjectItem( CurrentModuleObject, CoolTowerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), CoolTowerSys, CoolTowerNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
			}

			CoolTowerSys( CoolTowerNum ).Name = cAlphaArgs( 1 ); // Name of cooltower

			CoolTowerSys( CoolTowerNum ).Schedule = cAlphaArgs( 2 ); // Get schedule
			if ( lAlphaBlanks( 2 ) ) {
				CoolTowerSys( CoolTowerNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				CoolTowerSys( CoolTowerNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( CoolTowerSys( CoolTowerNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid data" );
					ShowContinueError( "Invalid-Schedule not found " + cAlphaFields( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			CoolTowerSys( CoolTowerNum ).ZoneName = cAlphaArgs( 3 ); // Name of zone where cooltower is serving
			CoolTowerSys( CoolTowerNum ).ZonePtr = FindItemInList( cAlphaArgs( 3 ), Zone );
			if ( CoolTowerSys( CoolTowerNum ).ZonePtr == 0 ) {
				if ( lAlphaBlanks( 3 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 3 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
				}
				ErrorsFound = true;
			}

			CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyName = cAlphaArgs( 4 ); // Name of water storage tank
			if ( lAlphaBlanks( 4 ) ) {
				CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyMode = WaterSupplyFromMains;
			} else if ( CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyMode == WaterSupplyFromTank ) {
				SetupTankDemandComponent( CoolTowerSys( CoolTowerNum ).Name, CurrentModuleObject, CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyName, ErrorsFound, CoolTowerSys( CoolTowerNum ).CoolTWaterSupTankID, CoolTowerSys( CoolTowerNum ).CoolTWaterTankDemandARRID );
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 5 ) ); // Type of flow control
			if ( SELECT_CASE_var == "WATERFLOWSCHEDULE" ) {
				CoolTowerSys( CoolTowerNum ).FlowCtrlType = WaterFlowSchedule;
			} else if ( ( SELECT_CASE_var == "WINDDRIVENFLOW" ) || ( SELECT_CASE_var == "NONE" ) || ( SELECT_CASE_var == "" ) ) {
				CoolTowerSys( CoolTowerNum ).FlowCtrlType = WindDrivenFlow;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
				ErrorsFound = true;
			}}

			CoolTowerSys( CoolTowerNum ).PumpSchedName = cAlphaArgs( 6 ); //Get schedule for water pump
			CoolTowerSys( CoolTowerNum ).PumpSchedPtr = GetScheduleIndex( cAlphaArgs( 6 ) );
			if ( CoolTowerSys( CoolTowerNum ).PumpSchedPtr == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 6 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\" not found." );
				}
				ErrorsFound = true;
			}

			CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate = rNumericArgs( 1 ); // Maximum limit of water supply
			if ( CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate > MaximumWaterFlowRate ) {
				CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate = MaximumWaterFlowRate;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 1 ) + "=[" + RoundSigDigits( rNumericArgs( 1 ), 2 ) + "]." );
				ShowContinueError( "...Maximum Allowable=[" + RoundSigDigits( MaximumWaterFlowRate, 2 ) + "]." );
			}
			if ( CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate < MinimumWaterFlowRate ) {
				CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate = MinimumWaterFlowRate;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 1 ) + "=[" + RoundSigDigits( rNumericArgs( 1 ), 2 ) + "]." );
				ShowContinueError( "...Minimum Allowable=[" + RoundSigDigits( MinimumWaterFlowRate, 2 ) + "]." );
			}

			CoolTowerSys( CoolTowerNum ).TowerHeight = rNumericArgs( 2 ); // Get effctive tower height
			if ( CoolTowerSys( CoolTowerNum ).TowerHeight > MaxHeight ) {
				CoolTowerSys( CoolTowerNum ).TowerHeight = MaxHeight;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 2 ) + "=[" + RoundSigDigits( rNumericArgs( 2 ), 2 ) + "]." );
				ShowContinueError( "...Maximum Allowable=[" + RoundSigDigits( MaxHeight, 2 ) + "]." );
			}
			if ( CoolTowerSys( CoolTowerNum ).TowerHeight < MinHeight ) {
				CoolTowerSys( CoolTowerNum ).TowerHeight = MinHeight;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 2 ) + "=[" + RoundSigDigits( rNumericArgs( 2 ), 2 ) + "]." );
				ShowContinueError( "...Minimum Allowable=[" + RoundSigDigits( MinHeight, 2 ) + "]." );
			}

			CoolTowerSys( CoolTowerNum ).OutletArea = rNumericArgs( 3 ); // Get outlet area
			if ( CoolTowerSys( CoolTowerNum ).OutletArea > MaxValue ) {
				CoolTowerSys( CoolTowerNum ).OutletArea = MaxValue;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 3 ) + "=[" + RoundSigDigits( rNumericArgs( 3 ), 2 ) + "]." );
				ShowContinueError( "...Maximum Allowable=[" + RoundSigDigits( MaxValue, 2 ) + "]." );
			}
			if ( CoolTowerSys( CoolTowerNum ).OutletArea < MinValue ) {
				CoolTowerSys( CoolTowerNum ).OutletArea = MinValue;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 3 ) + "=[" + RoundSigDigits( rNumericArgs( 3 ), 2 ) + "]." );
				ShowContinueError( "...Minimum Allowable=[" + RoundSigDigits( MinValue, 2 ) + "]." );
			}

			CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate = rNumericArgs( 4 ); // Maximum limit of air flow to the space
			if ( CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate > MaxValue ) {
				CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate = MaxValue;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 4 ) + "=[" + RoundSigDigits( rNumericArgs( 4 ), 2 ) + "]." );
				ShowContinueError( "...Maximum Allowable=[" + RoundSigDigits( MaxValue, 2 ) + "]." );
			}
			if ( CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate < MinValue ) {
				CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate = MinValue;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 4 ) + "=[" + RoundSigDigits( rNumericArgs( 4 ), 2 ) + "]." );
				ShowContinueError( "...Minimum Allowable=[" + RoundSigDigits( MinValue, 2 ) + "]." );
			}

			CoolTowerSys( CoolTowerNum ).MinZoneTemp = rNumericArgs( 5 ); // Get minimum temp limit which gets this cooltower off
			if ( CoolTowerSys( CoolTowerNum ).MinZoneTemp > MaxValue ) {
				CoolTowerSys( CoolTowerNum ).MinZoneTemp = MaxValue;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 5 ) + "=[" + RoundSigDigits( rNumericArgs( 5 ), 2 ) + "]." );
				ShowContinueError( "...Maximum Allowable=[" + RoundSigDigits( MaxValue, 2 ) + "]." );
			}
			if ( CoolTowerSys( CoolTowerNum ).MinZoneTemp < MinValue ) {
				CoolTowerSys( CoolTowerNum ).MinZoneTemp = MinValue;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 5 ) + "=[" + RoundSigDigits( rNumericArgs( 5 ), 2 ) + "]." );
				ShowContinueError( "...Minimum Allowable=[" + RoundSigDigits( MinValue, 2 ) + "]." );
			}

			CoolTowerSys( CoolTowerNum ).FracWaterLoss = rNumericArgs( 6 ); // Fraction of water loss
			if ( CoolTowerSys( CoolTowerNum ).FracWaterLoss > MaxFrac ) {
				CoolTowerSys( CoolTowerNum ).FracWaterLoss = MaxFrac;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 6 ) + "=[" + RoundSigDigits( rNumericArgs( 6 ), 2 ) + "]." );
				ShowContinueError( "...Maximum Allowable=[" + RoundSigDigits( MaxFrac, 2 ) + "]." );
			}
			if ( CoolTowerSys( CoolTowerNum ).FracWaterLoss < MinFrac ) {
				CoolTowerSys( CoolTowerNum ).FracWaterLoss = MinFrac;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 6 ) + "=[" + RoundSigDigits( rNumericArgs( 6 ), 2 ) + "]." );
				ShowContinueError( "...Minimum Allowable=[" + RoundSigDigits( MinFrac, 2 ) + "]." );
			}

			CoolTowerSys( CoolTowerNum ).FracFlowSched = rNumericArgs( 7 ); // Fraction of loss of air flow
			if ( CoolTowerSys( CoolTowerNum ).FracFlowSched > MaxFrac ) {
				CoolTowerSys( CoolTowerNum ).FracFlowSched = MaxFrac;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 7 ) + "=[" + RoundSigDigits( rNumericArgs( 7 ), 2 ) + "]." );
				ShowContinueError( "...Maximum Allowable=[" + RoundSigDigits( MaxFrac, 2 ) + "]." );
			}
			if ( CoolTowerSys( CoolTowerNum ).FracFlowSched < MinFrac ) {
				CoolTowerSys( CoolTowerNum ).FracFlowSched = MinFrac;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 7 ) + "=[" + RoundSigDigits( rNumericArgs( 7 ), 5 ) + "]." );
				ShowContinueError( "...Minimum Allowable=[" + RoundSigDigits( MinFrac, 2 ) + "]." );
			}

			CoolTowerSys( CoolTowerNum ).RatedPumpPower = rNumericArgs( 8 ); // Get rated pump power

		}

		cAlphaArgs.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		rNumericArgs.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) ShowFatalError( CurrentModuleObject + " errors occurred in input.  Program terminates." );

		for ( CoolTowerNum = 1; CoolTowerNum <= NumCoolTowers; ++CoolTowerNum ) {
			SetupOutputVariable( "Zone Cooltower Sensible Heat Loss Energy [J]", CoolTowerSys( CoolTowerNum ).SenHeatLoss, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Sensible Heat Loss Rate [W]", CoolTowerSys( CoolTowerNum ).SenHeatPower, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Latent Heat Loss Energy [J]", CoolTowerSys( CoolTowerNum ).LatHeatLoss, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Latent Heat Loss Rate [W]", CoolTowerSys( CoolTowerNum ).LatHeatPower, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Air Volume [m3]", CoolTowerSys( CoolTowerNum ).CoolTAirVol, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Current Density Air Volume Flow Rate [m3/s]", CoolTowerSys( CoolTowerNum ).AirVolFlowRate, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Standard Density Air Volume Flow Rate [m3/s]", CoolTowerSys( CoolTowerNum ).AirVolFlowRateStd, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Air Mass [kg]", CoolTowerSys( CoolTowerNum ).CoolTAirMass, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Air Mass Flow Rate [kg/s]", CoolTowerSys( CoolTowerNum ).AirMassFlowRate, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Air Inlet Temperature [C]", CoolTowerSys( CoolTowerNum ).InletDBTemp, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Air Inlet Humidity Ratio [kgWater/kgDryAir]", CoolTowerSys( CoolTowerNum ).InletHumRat, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Air Outlet Temperature [C]", CoolTowerSys( CoolTowerNum ).OutletTemp, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Air Outlet Humidity Ratio [kgWater/kgDryAir]", CoolTowerSys( CoolTowerNum ).OutletHumRat, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Pump Electric Power [W]", CoolTowerSys( CoolTowerNum ).PumpElecPower, "System", "Average", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
			SetupOutputVariable( "Zone Cooltower Pump Electric Energy [J]", CoolTowerSys( CoolTowerNum ).PumpElecConsump, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name, _, "Electric", "Cooling", _, "System" );
			if ( CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyMode == WaterSupplyFromMains ) {
				SetupOutputVariable( "Zone Cooltower Water Volume [m3]", CoolTowerSys( CoolTowerNum ).CoolTWaterConsump, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Cooltower Mains Water Volume [m3]", CoolTowerSys( CoolTowerNum ).CoolTWaterConsump, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name, _, "MainsWater", "Cooling", _, "System" );
			} else if ( CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyMode == WaterSupplyFromTank ) {
				SetupOutputVariable( "Zone Cooltower Water Volume [m3]", CoolTowerSys( CoolTowerNum ).CoolTWaterConsump, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Cooltower Storage Tank Water Volume [m3]", CoolTowerSys( CoolTowerNum ).CoolTWaterConsump, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Cooltower Starved Mains Water Volume [m3]", CoolTowerSys( CoolTowerNum ).CoolTWaterStarvMakeup, "System", "Sum", Zone( CoolTowerSys( CoolTowerNum ).ZonePtr ).Name, _, "MainsWater", "Cooling", _, "System" );
			}
		}

	}

	void
	CalcCoolTower()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   Aug 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Baruch Givoni. 1994. Passive and Low Energy Cooling of Buildings. Chapter 5: Evaporative Cooling Systems.
		//     John Wiley & Sons, Inc.

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::MCPC;
		using DataHeatBalFanSys::MCPTC;
		using DataHeatBalFanSys::CTMFL;
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyWFnTdbH;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::RhoH2O;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutWetBulbTemp;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::WindSpeed;
		using DataEnvironment::OutEnthalpy;
		using DataEnvironment::StdRhoAir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MinWindSpeed( 0.1 ); // Minimum limit of outdoor air wind speed in m/s
		Real64 const MaxWindSpeed( 30.0 ); // Maximum limit of outdoor air wind speed in m/s
		Real64 const UCFactor( 60000.0 ); // Unit conversion factor m3/s to l/min

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // Number of zone being served
		int CoolTowerNum; // Number of coolter being served
		Real64 CVF_ZoneNum; // Design flow rate in m3/s
		Real64 AirMassFlowRate; // Actual air mass flow rate in kg/s
		Real64 AirSpecHeat; // Specific heat of air
		Real64 AirDensity; // Density of air
		Real64 RhoWater; // Density of water
		Real64 PumpPartLoadRat; // Pump part load ratio (based on user schedule, or 1.0 for no schedule)
		Real64 WaterFlowRate; // Calculated water flow rate in m3/s
		Real64 AirVolFlowRate; // Calculated air volume flow rate in m3/s
		Real64 InletHumRat; // Humidity ratio of outdoor air
		//unused1208REAL(r64) :: InletEnthalpy      ! Enthalpy of outdoor air
		Real64 OutletHumRat; // Humidity ratio of air at the cooltower outlet
		Real64 OutletTemp; // Dry bulb temperature of air at the cooltower outlet
		Real64 IntHumRat; // Humidity ratio of initialized air

		MCPTC = 0.0;
		MCPC = 0.0;
		CTMFL = 0.0;

		for ( CoolTowerNum = 1; CoolTowerNum <= NumCoolTowers; ++CoolTowerNum ) {
			ZoneNum = CoolTowerSys( CoolTowerNum ).ZonePtr;

			if ( GetCurrentScheduleValue( CoolTowerSys( CoolTowerNum ).SchedPtr ) > 0.0 ) {
				// check component operation
				if ( WindSpeed < MinWindSpeed || WindSpeed > MaxWindSpeed ) continue;
				if ( MAT( ZoneNum ) < CoolTowerSys( CoolTowerNum ).MinZoneTemp ) continue;

				// Unit is on and simulate this component
				// Determine the temperature and air flow rate at the cooltower outlet
				if ( CoolTowerSys( CoolTowerNum ).FlowCtrlType == WindDrivenFlow ) {
					Real64 const height_sqrt( std::sqrt( CoolTowerSys( CoolTowerNum ).TowerHeight ) );
					CoolTowerSys( CoolTowerNum ).OutletVelocity = 0.7 * height_sqrt + 0.47 * ( WindSpeed - 1.0 );
					AirVolFlowRate = CoolTowerSys( CoolTowerNum ).OutletArea * CoolTowerSys( CoolTowerNum ).OutletVelocity;
					AirVolFlowRate = min( AirVolFlowRate, CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate );
					WaterFlowRate = ( AirVolFlowRate / ( 0.0125 * height_sqrt ) );
					if ( WaterFlowRate > CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate * UCFactor ) {
						WaterFlowRate = CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate * UCFactor;
						AirVolFlowRate = 0.0125 * WaterFlowRate * height_sqrt;
						AirVolFlowRate = min( AirVolFlowRate, CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate );
					}
					WaterFlowRate = min( WaterFlowRate, ( CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate * UCFactor ) );
					OutletTemp = OutDryBulbTemp - ( OutDryBulbTemp - OutWetBulbTemp ) * ( 1.0 - std::exp( -0.8 * CoolTowerSys( CoolTowerNum ).TowerHeight ) ) * ( 1.0 - std::exp( -0.15 * WaterFlowRate ) );
				} else if ( CoolTowerSys( CoolTowerNum ).FlowCtrlType == WaterFlowSchedule ) {
					WaterFlowRate = CoolTowerSys( CoolTowerNum ).MaxWaterFlowRate * UCFactor;
					AirVolFlowRate = 0.0125 * WaterFlowRate * std::sqrt( CoolTowerSys( CoolTowerNum ).TowerHeight );
					AirVolFlowRate = min( AirVolFlowRate, CoolTowerSys( CoolTowerNum ).MaxAirVolFlowRate );
					OutletTemp = OutDryBulbTemp - ( OutDryBulbTemp - OutWetBulbTemp ) * ( 1.0 - std::exp( -0.8 * CoolTowerSys( CoolTowerNum ).TowerHeight ) ) * ( 1.0 - std::exp( -0.15 * WaterFlowRate ) );
				}

				if ( OutletTemp < OutWetBulbTemp ) {
					ShowSevereError( "Cooltower outlet temperature exceed the outdoor wet bulb temperature reset to input values" );
					ShowContinueError( "Occurs in Cooltower =" + CoolTowerSys( CoolTowerNum ).Name );
				}

				WaterFlowRate /= UCFactor;
				// Determine actual water flow rate
				if ( CoolTowerSys( CoolTowerNum ).FracWaterLoss > 0.0 ) {
					CoolTowerSys( CoolTowerNum ).ActualWaterFlowRate = WaterFlowRate * ( 1.0 + CoolTowerSys( CoolTowerNum ).FracWaterLoss );
				} else {
					CoolTowerSys( CoolTowerNum ).ActualWaterFlowRate = WaterFlowRate;
				}

				// Determine actual air flow rate
				if ( CoolTowerSys( CoolTowerNum ).FracFlowSched > 0.0 ) {
					CoolTowerSys( CoolTowerNum ).ActualAirVolFlowRate = AirVolFlowRate * ( 1.0 - CoolTowerSys( CoolTowerNum ).FracFlowSched );
				} else {
					CoolTowerSys( CoolTowerNum ).ActualAirVolFlowRate = AirVolFlowRate;
				}

				// Determine pump power
				if ( GetCurrentScheduleValue( CoolTowerSys( CoolTowerNum ).PumpSchedPtr ) > 0 ) {
					PumpPartLoadRat = GetCurrentScheduleValue( CoolTowerSys( CoolTowerNum ).PumpSchedPtr );
				} else {
					PumpPartLoadRat = 1.0;
				}

				// Determine air mass flow rate and volume flow rate
				InletHumRat = PsyWFnTdbTwbPb( OutDryBulbTemp, OutWetBulbTemp, OutBaroPress );
				// Assume no pressure drops and no changes in enthalpy between inlet and outlet air
				IntHumRat = PsyWFnTdbH( OutletTemp, OutEnthalpy ); // Initialized humidity ratio
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, OutletTemp, IntHumRat );
				AirMassFlowRate = AirDensity * CoolTowerSys( CoolTowerNum ).ActualAirVolFlowRate;
				// From the mass balance W_in*(m_air + m_water) = W_out*m_air
				RhoWater = RhoH2O( OutletTemp ); // Assume T_water = T_outlet
				OutletHumRat = ( InletHumRat * ( AirMassFlowRate + ( CoolTowerSys( CoolTowerNum ).ActualWaterFlowRate * RhoWater ) ) ) / AirMassFlowRate;
				AirSpecHeat = PsyCpAirFnWTdb( OutletHumRat, OutletTemp );
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, OutletTemp, OutletHumRat ); // Outlet air density
				CVF_ZoneNum = CoolTowerSys( CoolTowerNum ).ActualAirVolFlowRate * GetCurrentScheduleValue( CoolTowerSys( CoolTowerNum ).SchedPtr );
				MCPC( ZoneNum ) = CVF_ZoneNum * AirDensity * AirSpecHeat;
				MCPTC( ZoneNum ) = MCPC( ZoneNum ) * OutletTemp;
				CTMFL( ZoneNum ) = MCPC( ZoneNum ) / AirSpecHeat;

				CoolTowerSys( CoolTowerNum ).SenHeatPower = MCPC( ZoneNum ) * std::abs( ZT( ZoneNum ) - OutletTemp );
				CoolTowerSys( CoolTowerNum ).LatHeatPower = CVF_ZoneNum * std::abs( ZoneAirHumRat( ZoneNum ) - OutletHumRat );
				CoolTowerSys( CoolTowerNum ).OutletTemp = OutletTemp;
				CoolTowerSys( CoolTowerNum ).OutletHumRat = OutletHumRat;
				CoolTowerSys( CoolTowerNum ).AirVolFlowRate = CVF_ZoneNum;
				CoolTowerSys( CoolTowerNum ).AirMassFlowRate = CTMFL( ZoneNum );
				CoolTowerSys( CoolTowerNum ).AirVolFlowRateStd = CTMFL( ZoneNum ) / StdRhoAir;
				CoolTowerSys( CoolTowerNum ).InletDBTemp = Zone( ZoneNum ).OutDryBulbTemp;
				CoolTowerSys( CoolTowerNum ).InletWBTemp = Zone( ZoneNum ).OutWetBulbTemp;
				CoolTowerSys( CoolTowerNum ).InletHumRat = OutHumRat;
				CoolTowerSys( CoolTowerNum ).CoolTWaterConsumpRate = ( std::abs( InletHumRat - OutletHumRat ) * CTMFL( ZoneNum ) ) / RhoWater;
				CoolTowerSys( CoolTowerNum ).CoolTWaterStarvMakeupRate = 0.0; // initialize -- calc in update
				CoolTowerSys( CoolTowerNum ).PumpElecPower = CoolTowerSys( CoolTowerNum ).RatedPumpPower * PumpPartLoadRat;
			} else { // Unit is off
				CoolTowerSys( CoolTowerNum ).SenHeatPower = 0.0;
				CoolTowerSys( CoolTowerNum ).LatHeatPower = 0.0;
				CoolTowerSys( CoolTowerNum ).OutletTemp = 0.0;
				CoolTowerSys( CoolTowerNum ).OutletHumRat = 0.0;
				CoolTowerSys( CoolTowerNum ).AirVolFlowRate = 0.0;
				CoolTowerSys( CoolTowerNum ).AirMassFlowRate = 0.0;
				CoolTowerSys( CoolTowerNum ).AirVolFlowRateStd = 0.0;
				CoolTowerSys( CoolTowerNum ).InletDBTemp = 0.0;
				CoolTowerSys( CoolTowerNum ).InletHumRat = 0.0;
				CoolTowerSys( CoolTowerNum ).PumpElecPower = 0.0;
				CoolTowerSys( CoolTowerNum ).CoolTWaterConsumpRate = 0.0;
				CoolTowerSys( CoolTowerNum ).CoolTWaterStarvMakeupRate = 0.0;
			}

		}

	}

	void
	UpdateCoolTower()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   October 2000
		//       MODIFIED       Aug 2008 Daeho Kang
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataWater;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoolTowerNum;
		Real64 AvailWaterRate;

		for ( CoolTowerNum = 1; CoolTowerNum <= NumCoolTowers; ++CoolTowerNum ) {

			// Set the demand request for supply water from water storage tank (if needed)
			if ( CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyMode == WaterSupplyFromTank ) {
				WaterStorage( CoolTowerSys( CoolTowerNum ).CoolTWaterSupTankID ).VdotRequestDemand( CoolTowerSys( CoolTowerNum ).CoolTWaterTankDemandARRID ) = CoolTowerSys( CoolTowerNum ).CoolTWaterConsumpRate;
			}

			//check if should be starved by restricted flow from tank
			if ( CoolTowerSys( CoolTowerNum ).CoolTWaterSupplyMode == WaterSupplyFromTank ) {
				AvailWaterRate = WaterStorage( CoolTowerSys( CoolTowerNum ).CoolTWaterSupTankID ).VdotAvailDemand( CoolTowerSys( CoolTowerNum ).CoolTWaterTankDemandARRID );
				if ( AvailWaterRate < CoolTowerSys( CoolTowerNum ).CoolTWaterConsumpRate ) {
					CoolTowerSys( CoolTowerNum ).CoolTWaterStarvMakeupRate = CoolTowerSys( CoolTowerNum ).CoolTWaterConsumpRate - AvailWaterRate;
					CoolTowerSys( CoolTowerNum ).CoolTWaterConsumpRate = AvailWaterRate;
				}
			}

		}

	}

	void
	ReportCoolTower()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   Aut 2008
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
		int CoolTowerNum;
		Real64 TSMult;

		TSMult = TimeStepSys * SecInHour;

		for ( CoolTowerNum = 1; CoolTowerNum <= NumCoolTowers; ++CoolTowerNum ) {

			CoolTowerSys( CoolTowerNum ).CoolTAirVol = CoolTowerSys( CoolTowerNum ).AirVolFlowRate * TSMult;
			CoolTowerSys( CoolTowerNum ).CoolTAirMass = CoolTowerSys( CoolTowerNum ).AirMassFlowRate * TSMult;
			CoolTowerSys( CoolTowerNum ).SenHeatLoss = CoolTowerSys( CoolTowerNum ).SenHeatPower * TSMult;
			CoolTowerSys( CoolTowerNum ).LatHeatLoss = CoolTowerSys( CoolTowerNum ).LatHeatPower * TSMult;
			CoolTowerSys( CoolTowerNum ).PumpElecConsump = CoolTowerSys( CoolTowerNum ).PumpElecPower * TSMult;
			CoolTowerSys( CoolTowerNum ).CoolTWaterConsump = CoolTowerSys( CoolTowerNum ).CoolTWaterConsumpRate * TSMult;
			CoolTowerSys( CoolTowerNum ).CoolTWaterStarvMakeup = CoolTowerSys( CoolTowerNum ).CoolTWaterStarvMakeupRate * TSMult;
		}

	}

	//*****************************************************************************************

} // CoolTower

} // EnergyPlus
