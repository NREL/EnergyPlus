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
#include <VariableSpeedCoils.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataWater.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
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

namespace VariableSpeedCoils {

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using namespace Psychrometrics;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::EnvironmentName;
	using DataEnvironment::CurMnDy;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutWetBulbTemp;
	using DataEnvironment::OutEnthalpy;
	using namespace DataSizing;
	using DataPlant::TypeOf_CoilVSWAHPHeatingEquationFit;
	using DataPlant::TypeOf_CoilVSWAHPCoolingEquationFit;
	using General::RoundSigDigits;

	// Use statements for access to subroutines in other modules

	// Data
	//MODULE PARAMETER DEFINITIONS

	Real64 const RatedInletAirTemp( 26.6667 ); // 26.6667C or 80F
	Real64 const RatedInletWetBulbTemp( 19.44 ); // 19.44 or 67F, cooling mode
	Real64 const RatedInletAirHumRat( 0.01125 ); // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
	Real64 const RatedInletWaterTemp( 29.4 ); // 85 F cooling mode
	Real64 const RatedAmbAirTemp( 35.0 ); // 95 F cooling mode
	Real64 const RatedInletAirTempHeat( 21.11 ); // 21.11C or 70F, heating mode
	Real64 const RatedInletWaterTempHeat( 21.11 ); // 21.11C or 70F, heating mode
	Real64 const RatedAmbAirTempHeat( 8.33 ); // 8.33 or 47F, heating mode
	Real64 const RatedAmbAirWBHeat( 6.11 ); // 8.33 or 43F, heating mode, rated wet bulb temperature

	// Airflow per total capacity range
	Real64 const MaxRatedVolFlowPerRatedTotCap( 0.00006041 ); // m3/s per watt = 450 cfm/ton
	Real64 const MinRatedVolFlowPerRatedTotCap( 0.00004027 ); // m3/s per watt = 300 cfm/ton
	Real64 const MaxHeatVolFlowPerRatedTotCap( 0.00008056 ); // m3/s per watt = 600 cfm/ton
	Real64 const MaxCoolVolFlowPerRatedTotCap( 0.00006713 ); // m3/s per watt = 500 cfm/ton
	Real64 const MinOperVolFlowPerRatedTotCap( 0.00002684 ); // m3/s per watt = 200 cfm/ton

	//Water Systems
	int const CondensateDiscarded( 1001 ); // default mode where water is "lost"
	int const CondensateToTank( 1002 ); // collect coil condensate from air and store in water storage tank

	int const WaterSupplyFromMains( 101 );
	int const WaterSupplyFromTank( 102 );

	// Curve Types
	int const Linear( 1 );
	int const BiLinear( 2 );
	int const Quadratic( 3 );
	int const BiQuadratic( 4 );
	int const Cubic( 5 );

	// Defrost strategy (heat pump only)
	int const ReverseCycle( 1 ); // uses reverse cycle defrost strategy
	int const Resistive( 2 ); // uses electric resistance heater for defrost
	// Defrost control  (heat pump only)
	int const Timed( 1 ); // defrost cycle is timed
	int const OnDemand( 2 ); // defrost cycle occurs only when required

	int const MaxSpedLevels( 10 ); // Maximum number of speed that supports

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// Identifier is VarSpeedCoil
	int NumWatertoAirHPs( 0 ); // The Number of Water to Air Heat Pumps found in the Input

	bool GetCoilsInputFlag( true ); // Flag set to make sure you get input once
	// LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag

	Real64 SourceSideMassFlowRate( 0.0 ); // Source Side Mass flow rate [Kg/s]
	Real64 SourceSideInletTemp( 0.0 ); // Source Side Inlet Temperature [C]
	Real64 SourceSideInletEnth( 0.0 ); // Source Side Inlet Enthalpy [J/kg]
	Real64 LoadSideMassFlowRate( 0.0 ); // Load Side Mass flow rate [Kg/s]
	Real64 LoadSideInletDBTemp( 0.0 ); // Load Side Inlet Dry Bulb Temp [C]
	Real64 LoadSideInletWBTemp( 0.0 ); // Load Side Inlet Wet Bulb Temp [C]
	Real64 LoadSideInletHumRat( 0.0 ); // Load Side Outlet Humidity ratio
	Real64 LoadSideInletEnth( 0.0 ); // Load Side Inlet Enthalpy [J/kg]
	Real64 LoadSideOutletDBTemp( 0.0 ); // Load Side Outlet Dry Bulb Temp [C]
	Real64 LoadSideOutletHumRat( 0.0 ); // Load Side Outlet Humidity ratio
	Real64 LoadSideOutletEnth( 0.0 ); // Load Side Outlet Enthalpy [J/kg]
	Real64 QSensible( 0.0 ); // Load side sensible heat transfer rate [W]
	Real64 QLoadTotal( 0.0 ); // Load side total heat transfer rate [W]
	Real64 QLatRated( 0.0 ); // Latent Capacity [W] rated at entering air conditions [Tdb=26.7C Twb=19.4C]
	Real64 QLatActual( 0.0 ); // Actual Latent Capacity [W]
	Real64 QSource( 0.0 ); // Source side heat transfer rate [W]
	Real64 Winput( 0.0 ); // Power Consumption [W]
	Real64 PLRCorrLoadSideMdot( 0.0 ); // Load Side Mdot corrected for Part Load Ratio of the unit

	Real64 VSHPWHHeatingCapacity( 0.0 ); // Used by Heat Pump:Water Heater object as total water heating capacity [W]
	Real64 VSHPWHHeatingCOP( 0.0 ); // Used by Heat Pump:Water Heater object as water heating COP [W/W]

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Update routine

	// Utility routines
	//SHR, bypass factor routines

	// Object Data
	Array1D< VariableSpeedCoilData > VarSpeedCoil;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions
	void
	clear_state()
	{
		NumWatertoAirHPs = 0; 
		GetCoilsInputFlag = true ; 
		SourceSideMassFlowRate = 0.0;
		SourceSideInletTemp = 0.0;
		SourceSideInletEnth = 0.0;
		LoadSideMassFlowRate = 0.0;
		LoadSideInletDBTemp = 0.0;
		LoadSideInletWBTemp = 0.0;
		LoadSideInletHumRat = 0.0;
		LoadSideInletEnth = 0.0;
		LoadSideOutletDBTemp = 0.0;
		LoadSideOutletHumRat = 0.0;
		LoadSideOutletEnth = 0.0;
		QSensible = 0.0;
		QLoadTotal = 0.0;
		QLatRated = 0.0; 
		QLatActual = 0.0; 
		QSource = 0.0; 
		Winput = 0.0; 
		PLRCorrLoadSideMdot = 0.0;
		VSHPWHHeatingCapacity = 0.0; 
		VSHPWHHeatingCOP = 0.0;
		VarSpeedCoil.deallocate();
	}


	void
	SimVariableSpeedCoils(
		std::string const & CompName, // Coil Name
		int & CompIndex, // Index for Component name
		int const CyclingScheme, // Continuous fan OR cycling compressor
		Real64 & MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 & HPTimeConstant, // Heat pump time constant [s]
		Real64 & FanDelayTime, // Fan delay time, time delay for the HP's fan to
		int const CompOp, // compressor on/off. 0 = off; 1= on
		Real64 const PartLoadFrac,
		int const SpeedNum, // compressor speed number
		Real64 const SpeedRatio, // compressor speed ratio
		Real64 const SensLoad, // Sensible demand load [W]
		Real64 const LatentLoad, // Latent demand load [W]
		Optional< Real64 const > OnOffAirFlowRat // ratio of comp on to comp off air flow rate
	)
	{

		//       AUTHOR         Bo Shen, ORNL
		//       DATE WRITTEN   March 2012
		//       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages variable-speed Water to Air Heat Pump component simulation.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// N/A

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using FluidProperties::FindGlycol;
		using General::TrimSigDigits;
		using General::SolveRegulaFalsi;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// shut off after compressor cycle off  [s]
		// part-load ratio = load/total capacity, passed in by the parent object

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoilNum; // The WatertoAirHP that you are currently loading input into
		Real64 OnOffAirFlowRatio; // ratio of comp on to comp off air flow rate
		Real64 RuntimeFrac; // run time fraction
		int SpeedCal; // variable for error proof speed input

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		if ( CompIndex == 0 ) {
			DXCoilNum = FindItemInList( CompName, VarSpeedCoil );
			if ( DXCoilNum == 0 ) {
				ShowFatalError( "WaterToAirHPVSWEquationFit not found=" + CompName );
			}
			CompIndex = DXCoilNum;
		} else {
			DXCoilNum = CompIndex;
			if ( DXCoilNum > NumWatertoAirHPs || DXCoilNum < 1 ) {
				ShowFatalError( "SimVariableSpeedCoils: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", Number of Water to Air HPs=" + TrimSigDigits( NumWatertoAirHPs ) + ", WaterToAir HP name=" + CompName );
			}
			if ( ! CompName.empty() && CompName != VarSpeedCoil( DXCoilNum ).Name ) {
				ShowFatalError( "SimVariableSpeedCoils: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", WaterToAir HP name=" + CompName + ", stored WaterToAir HP Name for that index=" + VarSpeedCoil( DXCoilNum ).Name );
			}
		}

		if ( present( OnOffAirFlowRat ) ) {
			OnOffAirFlowRatio = OnOffAirFlowRat;
		} else {
			OnOffAirFlowRatio = 1.0;
		}

		//ERROR PROOF
		if ( SpeedNum < 1 ) {
			SpeedCal = 1;
		} else {
			SpeedCal = SpeedNum;
		}

		if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit ) || ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) ) {
			// Cooling mode
			InitVarSpeedCoil( DXCoilNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, SpeedRatio, SpeedCal );
			CalcVarSpeedCoilCooling( DXCoilNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadFrac, OnOffAirFlowRatio, SpeedRatio, SpeedCal );
			UpdateVarSpeedCoil( DXCoilNum );
		} else if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) || ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) ) {
			// Heating mode
			InitVarSpeedCoil( DXCoilNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, SpeedRatio, SpeedCal );
			CalcVarSpeedCoilHeating( DXCoilNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadFrac, OnOffAirFlowRatio, SpeedRatio, SpeedCal );
			UpdateVarSpeedCoil( DXCoilNum );
		} else if ( VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed ) {
			// Heating mode
			InitVarSpeedCoil(DXCoilNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, SpeedRatio, SpeedCal);
			CalcVarSpeedHPWH(DXCoilNum, RuntimeFrac, PartLoadFrac, SpeedRatio, SpeedNum, CyclingScheme);
			UpdateVarSpeedCoil(DXCoilNum);
		} else {
			ShowFatalError( "SimVariableSpeedCoils: WatertoAir heatpump not in either HEATING or COOLING mode" );
		}

		// two additional output variables
		VarSpeedCoil( DXCoilNum ).SpeedNumReport = SpeedCal;
		VarSpeedCoil( DXCoilNum ).SpeedRatioReport = SpeedRatio;

	}

	void
	GetVarSpeedCoilInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for HPs and stores it in HP data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// n/a

		// Using/Aliasing
		using namespace InputProcessor;
		using namespace NodeInputManager;
		using BranchNodeConnections::TestCompSet;
		using GlobalNames::VerifyUniqueCoilName;
		using namespace OutputReportPredefined;
		using General::TrimSigDigits;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::CurveValue;
		using CurveManager::SetCurveOutputMinMaxValues;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using WaterManager::SetupTankDemandComponent;
		using WaterManager::SetupTankSupplyComponent;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetVarSpeedCoilInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoilNum; // The Water to Air HP that you are currently loading input into
		int NumCool; // Counter for cooling coil, water source
		int NumCoolAS; // Counter for cooling coil, air source
		int NumHeat; // Counter for heating coil, water source
		int NumHeatAS; // Counter for heating coil, air source
		int NumHPWHAirToWater; //counter for air source HPWH
		int CoilCounter; // Counter
		int I; // Loop index increment
		int NumAlphas; // Number of variables in String format
		int NumNums; // Number of variables in Numeric format
		int NumParams; // Total number of input fields
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		int IOStat;
		int AlfaFieldIncre; // increment number of Alfa field
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		Real64 CurveVal; // Used to verify modifier curves equal 1 at rated conditions
		Real64 WHInletAirTemp; // Used to pass proper inlet air temp to HPWH DX coil performance curves
		Real64 WHInletWaterTemp; // Used to pass proper inlet water temp to HPWH DX coil performance curves
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		NumCool = GetNumObjectsFound( "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" );
		NumHeat = GetNumObjectsFound( "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" );
		NumCoolAS = GetNumObjectsFound( "COIL:COOLING:DX:VARIABLESPEED" );
		NumHeatAS = GetNumObjectsFound( "COIL:HEATING:DX:VARIABLESPEED" );
		NumHPWHAirToWater = GetNumObjectsFound("COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED");
		NumWatertoAirHPs = NumCool + NumHeat + NumCoolAS + NumHeatAS + NumHPWHAirToWater;
		DXCoilNum = 0;

		if ( NumWatertoAirHPs <= 0 ) {
			ShowSevereError( "No Equipment found in GetVarSpeedCoilInput" );
			ErrorsFound = true;
		}

		// Allocate Arrays
		if ( NumWatertoAirHPs > 0 ) {
			VarSpeedCoil.allocate( NumWatertoAirHPs );
		}

		GetObjectDefMaxArgs( "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", NumParams, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", NumParams, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		GetObjectDefMaxArgs( "COIL:COOLING:DX:VARIABLESPEED", NumParams, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "COIL:HEATING:DX:VARIABLESPEED", NumParams, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		//variable speed air-source HPWH
		GetObjectDefMaxArgs("COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED", NumParams, NumAlphas, NumNums);
		MaxNums = max(MaxNums, NumNums);
		MaxAlphas = max(MaxAlphas, NumAlphas);

		AlphArray.allocate(MaxAlphas);
		cAlphaFields.allocate(MaxAlphas);
		lAlphaBlanks.dimension(MaxAlphas, true);
		cNumericFields.allocate(MaxNums);
		lNumericBlanks.dimension(MaxNums, true);
		NumArray.dimension(MaxNums, 0.0);

		// Get the data for cooling coil, WATER SOURCE
		CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit"; //for reporting

		for ( CoilCounter = 1; CoilCounter <= NumCool; ++CoilCounter ) {

			++DXCoilNum;
			AlfaFieldIncre = 1;

			GetObjectItem( CurrentModuleObject, CoilCounter, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;

			VerifyName( AlphArray( 1 ), VarSpeedCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			VarSpeedCoil( DXCoilNum ).bIsDesuperheater = false; 
			VarSpeedCoil( DXCoilNum ).Name = AlphArray( 1 );
			VarSpeedCoil( DXCoilNum ).CoolHeatType = "COOLING";
			VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum = TypeOf_CoilVSWAHPCoolingEquationFit;
			VarSpeedCoil( DXCoilNum ).NumOfSpeeds = int( NumArray( 1 ) );
			VarSpeedCoil( DXCoilNum ).NormSpedLevel = int( NumArray( 2 ) );
			VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal = NumArray( 3 );
			VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate = NumArray( 4 );
			VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate = NumArray( 5 );
			VarSpeedCoil( DXCoilNum ).Twet_Rated = NumArray( 6 );
			VarSpeedCoil( DXCoilNum ).Gamma_Rated = NumArray( 7 );
			VarSpeedCoil( DXCoilNum ).HOTGASREHEATFLG = int( NumArray( 8 ) );
			VarSpeedCoil( DXCoilNum ).CondenserType = WaterCooled;

			VarSpeedCoil( DXCoilNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Water Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Air Nodes" );

			//   If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
			if ( VarSpeedCoil( DXCoilNum ).NumOfSpeeds < 1 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " must be >= 1. entered number is " + TrimSigDigits( NumArray( 1 ), 0 ) );
				ErrorsFound = true;
			}

			if ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) {
				VarSpeedCoil( DXCoilNum ).NormSpedLevel = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;
			}

			if ( ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) || ( VarSpeedCoil( DXCoilNum ).NormSpedLevel <= 0 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 2 ) + " must be valid speed level entered number is " + TrimSigDigits( NumArray( 2 ), 0 ) );
				ErrorsFound = true;
			}

			//part load curve
			VarSpeedCoil( DXCoilNum ).PLFFPLR = GetCurveIndex( AlphArray( 6 ) ); // convert curve name to number
			if ( VarSpeedCoil( DXCoilNum ).PLFFPLR == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 6 ) + "=\"" + AlphArray( 6 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).PLFFPLR, 1.0 );
				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
					ShowContinueError( "..." + cAlphaFields( 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
					ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}
			}

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) = NumArray( 9 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSRatedSHR( I ) = NumArray( 10 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSRatedCOP( I ) = NumArray( 11 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) = NumArray( 12 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( I ) = NumArray( 13 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( I ) = NumArray( 14 + ( I - 1 ) * 6 );

				AlfaFieldIncre = 7 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ), RatedInletWetBulbTemp, RatedInletWaterTemp );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 8 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 9 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 10 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ), RatedInletWetBulbTemp, RatedInletWaterTemp );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( 1 ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 11 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 12 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 13 + ( I - 1 ) * 7;
				// Read waste heat modifier curve name
				VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ), RatedInletWaterTemp, RatedInletAirTemp );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

			}

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedPercentTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( VarSpeedCoil( DXCoilNum ).NumOfSpeeds );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowPerRatedTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I );
				VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowPerRatedTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I );
			}

			// CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit"
			SetupOutputVariable( "Cooling Coil Electric Energy [J]", VarSpeedCoil( DXCoilNum ).Energy, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "Cooling", _, "System" );
			SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLoadTotal, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergySensible, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLatent, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Energy [J]", VarSpeedCoil( DXCoilNum ).EnergySource, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );

			//for table output, being consistent with outher water-to-air coils
			//        IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal /= AutoSize) THEN
			//            VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
			//                *VarSpeedCoil(DXCoilNum)%MSRatedSHR(VarSpeedCoil(DXCoilNum)%NormSpedLevel)
			//        ELSE
			//            VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = AUTOSIZE
			//        END IF

			VarSpeedCoil( DXCoilNum ).RatedCapCoolSens = AutoSize; //always auto-sized, to be determined in the sizing calculation

			// BAN Sept 30 2103, CR9322, commented out, now it is redundant, it is reported from sizing routine
			//create predefined report entries
			//PreDefTableEntry( pdchCoolCoilType, VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject );
			//PreDefTableEntry( pdchCoolCoilTotCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
			//PreDefTableEntry( pdchCoolCoilSensCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
			//PreDefTableEntry( pdchCoolCoilLatCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal - VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
			//PreDefTableEntry( pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens / VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
			//PreDefTableEntry( pdchCoolCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) );
		}

		//-------------------------AIR SOURCE, COOLING---BEGIN
		// Get the data for cooling coil, AIR SOURCE
		CurrentModuleObject = "Coil:Cooling:DX:VariableSpeed"; //for reporting

		for ( CoilCounter = 1; CoilCounter <= NumCoolAS; ++CoilCounter ) {

			++DXCoilNum;
			AlfaFieldIncre = 1;

			GetObjectItem( CurrentModuleObject, CoilCounter, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;

			VerifyName( AlphArray( 1 ), VarSpeedCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			VarSpeedCoil( DXCoilNum ).bIsDesuperheater = false;
			VarSpeedCoil( DXCoilNum ).Name = AlphArray( 1 );
			VarSpeedCoil( DXCoilNum ).CoolHeatType = "COOLING";
			VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum = Coil_CoolingAirToAirVariableSpeed;
			VarSpeedCoil( DXCoilNum ).NumOfSpeeds = int( NumArray( 1 ) );
			VarSpeedCoil( DXCoilNum ).NormSpedLevel = int( NumArray( 2 ) );
			VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal = NumArray( 3 );
			VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate = NumArray( 4 );
			VarSpeedCoil( DXCoilNum ).Twet_Rated = NumArray( 5 );
			VarSpeedCoil( DXCoilNum ).Gamma_Rated = NumArray( 6 );

			VarSpeedCoil( DXCoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ((VarSpeedCoil(DXCoilNum).AirInletNodeNum > 0) && (VarSpeedCoil(DXCoilNum).AirOutletNodeNum > 0)) // IHP allows no node inputs for coil
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Air Nodes" );

			if ( VarSpeedCoil( DXCoilNum ).NumOfSpeeds < 1 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " must be >= 1. entered number is " + TrimSigDigits( NumArray( 1 ), 0 ) );
				ErrorsFound = true;
			}

			if ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) {
				VarSpeedCoil( DXCoilNum ).NormSpedLevel = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;
			}

			if ( ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) || ( VarSpeedCoil( DXCoilNum ).NormSpedLevel <= 0 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 2 ) + " must be valid speed level entered number is " + TrimSigDigits( NumArray( 2 ), 0 ) );
				ErrorsFound = true;
			}

			//part load curve
			VarSpeedCoil( DXCoilNum ).PLFFPLR = GetCurveIndex( AlphArray( 4 ) ); // convert curve name to number
			if ( VarSpeedCoil( DXCoilNum ).PLFFPLR == 0 ) {
				if ( lAlphaBlanks( 4 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 4 ) + "=\"" + AlphArray( 4 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).PLFFPLR, 1.0 );
				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
					ShowContinueError( "..." + cAlphaFields( 4 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
					ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}
			}

			// outdoor condenser node
			if ( lAlphaBlanks( 5 ) ) {
				VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum = 0;
			} else {
				VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, VarSpeedCoil( DXCoilNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );

				if ( ! CheckOutAirNodeNumber( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", may be invalid" );
					ShowContinueError( cAlphaFields( 10 ) + "=\"" + AlphArray( 5 ) + "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ShowContinueError( "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues" );
				}
			}

			if ( ( SameString( AlphArray( 6 ), "AirCooled" ) ) || lAlphaBlanks( 6 ) ) {
				VarSpeedCoil( DXCoilNum ).CondenserType = AirCooled;
			} else if ( SameString( AlphArray( 6 ), "EvaporativelyCooled" ) ) {
				VarSpeedCoil( DXCoilNum ).CondenserType = EvapCooled;
				VarSpeedCoil( DXCoilNum ).ReportEvapCondVars = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cAlphaFields( 6 ) + "=\"" + AlphArray( 6 ) + "\":" );
				ShowContinueError( "...must be AirCooled or EvaporativelyCooled." );
				ErrorsFound = true;
			}

			VarSpeedCoil( DXCoilNum ).EvapCondPumpElecNomPower = NumArray( 7 );

			if ( VarSpeedCoil( DXCoilNum ).EvapCondPumpElecNomPower < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 7 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( NumArray( 7 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater capacity
			VarSpeedCoil( DXCoilNum ).CrankcaseHeaterCapacity = NumArray( 8 );
			if ( VarSpeedCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 8 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( NumArray( 8 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater cutout temperature
			VarSpeedCoil( DXCoilNum ).MaxOATCrankcaseHeater = NumArray( 9 );

			// Get Water System tank connections
			//  A7, \field Name of Water Storage Tank for Supply
			VarSpeedCoil( DXCoilNum ).EvapWaterSupplyName = AlphArray( 7 );
			if ( lAlphaBlanks( 7 ) ) {
				VarSpeedCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				VarSpeedCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject, VarSpeedCoil( DXCoilNum ).EvapWaterSupplyName, ErrorsFound, VarSpeedCoil( DXCoilNum ).EvapWaterSupTankID, VarSpeedCoil( DXCoilNum ).EvapWaterTankDemandARRID );
			}

			//A8; \field Name of Water Storage Tank for Condensate Collection
			VarSpeedCoil( DXCoilNum ).CondensateCollectName = AlphArray( 8 );
			if ( lAlphaBlanks( 8 ) ) {
				VarSpeedCoil( DXCoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				VarSpeedCoil( DXCoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject, VarSpeedCoil( DXCoilNum ).CondensateCollectName, ErrorsFound, VarSpeedCoil( DXCoilNum ).CondensateTankID, VarSpeedCoil( DXCoilNum ).CondensateTankSupplyARRID );
			}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			VarSpeedCoil( DXCoilNum ).BasinHeaterPowerFTempDiff = NumArray( 10 );
			if ( NumArray( 10 ) < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 10 ) + " must be >= 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( NumArray( 10 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			VarSpeedCoil( DXCoilNum ).BasinHeaterSetPointTemp = NumArray( 11 );
			if ( VarSpeedCoil( DXCoilNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( VarSpeedCoil( DXCoilNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", freeze possible" );
					ShowContinueError( "..." + cNumericFields( 11 ) + " is < 2 {C}. Freezing could occur." );
					ShowContinueError( "...entered value=[" + TrimSigDigits( NumArray( 11 ), 2 ) + "]." );
				}
			}

			if ( ! lAlphaBlanks( 9 ) ) {
				VarSpeedCoil( DXCoilNum ).BasinHeaterSchedulePtr = GetScheduleIndex( AlphArray( 9 ) );
				if ( VarSpeedCoil( DXCoilNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 14 ) + "=\"" + AlphArray( 9 ) + "\"." );
					ShowContinueError( "Basin heater will be available to operate throughout the simulation." );
				}
			}

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) = NumArray( 12 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSRatedSHR( I ) = NumArray( 13 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSRatedCOP( I ) = NumArray( 14 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) = NumArray( 15 + ( I - 1 ) * 6 );
				VarSpeedCoil( DXCoilNum ).EvapCondAirFlow( I ) = NumArray( 16 + ( I - 1 ) * 6 );

				VarSpeedCoil( DXCoilNum ).EvapCondEffect( I ) = NumArray( 17 + ( I - 1 ) * 6 );
				if ( VarSpeedCoil( DXCoilNum ).EvapCondEffect( I ) < 0.0 || VarSpeedCoil( DXCoilNum ).EvapCondEffect( I ) > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 17 + ( I - 1 ) * 6 ) + " cannot be < 0.0 or > 1.0." );
					ShowContinueError( "...entered value=[" + TrimSigDigits( NumArray( 17 + ( I - 1 ) * 6 ), 2 ) + "]." );
					ErrorsFound = true;
				}

				AlfaFieldIncre = 10 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ), RatedInletWetBulbTemp, RatedAmbAirTemp );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 11 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 12 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ), RatedInletWetBulbTemp, RatedAmbAirTemp );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( 1 ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 13 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}
			}

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedPercentTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( VarSpeedCoil( DXCoilNum ).NumOfSpeeds );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowPerRatedTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I );
				VarSpeedCoil( DXCoilNum ).MSRatedEvapCondVolFlowPerRatedTotCap( I ) = VarSpeedCoil( DXCoilNum ).EvapCondAirFlow( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I );
			}

			// CurrentModuleObject = "Coil:Cooling:DX:VariableSpeed"
			SetupOutputVariable( "Cooling Coil Electric Energy [J]", VarSpeedCoil( DXCoilNum ).Energy, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "Cooling", _, "System" );
			SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLoadTotal, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergySensible, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLatent, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Energy [J]", VarSpeedCoil( DXCoilNum ).EnergySource, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );

			VarSpeedCoil( DXCoilNum ).RatedCapCoolSens = AutoSize; //always auto-sized, to be determined in the sizing calculation

			// BAN Sept 30 2103, CR9322, commented out, now it is redundant, it is reported from sizing routine
			//create predefined report entries
			//PreDefTableEntry( pdchCoolCoilType, VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject );
			//PreDefTableEntry( pdchCoolCoilTotCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
			//PreDefTableEntry( pdchCoolCoilSensCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
			//PreDefTableEntry( pdchCoolCoilLatCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal - VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
			//PreDefTableEntry( pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens / VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
			//PreDefTableEntry( pdchCoolCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) );

		}

		//-------------------------AIR SOURCE COOLING---END

		// Get the data for heating coil, WATER SOURCE
		CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit";

		for ( CoilCounter = 1; CoilCounter <= NumHeat; ++CoilCounter ) {

			++DXCoilNum;

			GetObjectItem( CurrentModuleObject, CoilCounter, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;

			VerifyName( AlphArray( 1 ), VarSpeedCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			VarSpeedCoil(DXCoilNum).bIsDesuperheater = false;
			VarSpeedCoil( DXCoilNum ).Name = AlphArray( 1 );
			VarSpeedCoil( DXCoilNum ).CoolHeatType = "HEATING";
			VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum = TypeOf_CoilVSWAHPHeatingEquationFit;
			VarSpeedCoil( DXCoilNum ).NumOfSpeeds = int( NumArray( 1 ) );
			VarSpeedCoil( DXCoilNum ).NormSpedLevel = int( NumArray( 2 ) );
			VarSpeedCoil( DXCoilNum ).RatedCapHeat = NumArray( 3 );
			VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate = NumArray( 4 );
			VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate = NumArray( 5 );
			VarSpeedCoil( DXCoilNum ).CondenserType = WaterCooled;

			VarSpeedCoil( DXCoilNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Water Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Air Nodes" );

			//       If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
			if ( VarSpeedCoil( DXCoilNum ).NumOfSpeeds < 1 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " must be >= 1. entered number is " + TrimSigDigits( NumArray( 1 ), 0 ) );
				ErrorsFound = true;
			}

			if ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) {
				VarSpeedCoil( DXCoilNum ).NormSpedLevel = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;
			}

			if ( ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) || ( VarSpeedCoil( DXCoilNum ).NormSpedLevel <= 0 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 2 ) + " must be valid speed level entered number is " + TrimSigDigits( NumArray( 2 ), 0 ) );
				ErrorsFound = true;
			}

			//part load curve
			VarSpeedCoil( DXCoilNum ).PLFFPLR = GetCurveIndex( AlphArray( 6 ) ); // convert curve name to number
			if ( VarSpeedCoil( DXCoilNum ).PLFFPLR == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 6 ) + "=\"" + AlphArray( 6 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).PLFFPLR, 1.0 );
				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
					ShowContinueError( "..." + cAlphaFields( 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
					ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}
			}

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) = NumArray( 6 + ( I - 1 ) * 5 );
				VarSpeedCoil( DXCoilNum ).MSRatedCOP( I ) = NumArray( 7 + ( I - 1 ) * 5 );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) = NumArray( 8 + ( I - 1 ) * 5 );
				VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( I ) = NumArray( 9 + ( I - 1 ) * 5 );
				VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( I ) = NumArray( 10 + ( I - 1 ) * 5 );

				AlfaFieldIncre = 7 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ), RatedInletAirTempHeat, RatedInletWaterTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 8 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 9 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( 14 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 10 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ), RatedInletAirTempHeat, RatedInletWaterTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( 1 ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 11 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( 16 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 12 + ( I - 1 ) * 7;
				VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 13 + ( I - 1 ) * 7;
				// Read waste heat modifier curve name
				VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ), RatedInletAirTempHeat, RatedInletWaterTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSWasteHeat( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

			}

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedPercentTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( VarSpeedCoil( DXCoilNum ).NumOfSpeeds );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowPerRatedTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I );
				VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowPerRatedTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I );
			}

			// CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit"
			SetupOutputVariable( "Heating Coil Electric Energy [J]", VarSpeedCoil( DXCoilNum ).Energy, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Heating Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLoadTotal, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Source Side Heat Transfer Energy [J]", VarSpeedCoil( DXCoilNum ).EnergySource, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "HEATINGCOILS", _, "System" );

			//create predefined report entries
			PreDefTableEntry( pdchHeatCoilType, VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject );
			PreDefTableEntry( pdchHeatCoilNomCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapHeat );
			PreDefTableEntry( pdchHeatCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) );

		}

		//-------------------------AIR SOURCE, HEATING---BEGIN
		// Get the data for heating coil, AIR SOURCE
		CurrentModuleObject = "COIL:HEATING:DX:VARIABLESPEED";

		for ( CoilCounter = 1; CoilCounter <= NumHeatAS; ++CoilCounter ) {

			++DXCoilNum;

			GetObjectItem( CurrentModuleObject, CoilCounter, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;

			VerifyName( AlphArray( 1 ), VarSpeedCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			VarSpeedCoil( DXCoilNum ).bIsDesuperheater = false;
			VarSpeedCoil( DXCoilNum ).Name = AlphArray( 1 );
			VarSpeedCoil( DXCoilNum ).CoolHeatType = "HEATING";
			VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum = Coil_HeatingAirToAirVariableSpeed;
			VarSpeedCoil( DXCoilNum ).NumOfSpeeds = int( NumArray( 1 ) );
			VarSpeedCoil( DXCoilNum ).NormSpedLevel = int( NumArray( 2 ) );
			VarSpeedCoil( DXCoilNum ).RatedCapHeat = NumArray( 3 );
			VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate = NumArray( 4 );

			VarSpeedCoil( DXCoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			VarSpeedCoil( DXCoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ((VarSpeedCoil(DXCoilNum).AirInletNodeNum > 0) && (VarSpeedCoil(DXCoilNum).AirOutletNodeNum > 0)) // IHP allows no node inputs for coil
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Air Nodes" );

			if ( VarSpeedCoil( DXCoilNum ).NumOfSpeeds < 1 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " must be >= 1. entered number is " + TrimSigDigits( NumArray( 1 ), 0 ) );
				ErrorsFound = true;
			}

			if ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) {
				VarSpeedCoil( DXCoilNum ).NormSpedLevel = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;
			}

			if ( ( VarSpeedCoil( DXCoilNum ).NormSpedLevel > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) || ( VarSpeedCoil( DXCoilNum ).NormSpedLevel <= 0 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 2 ) + " must be valid speed level entered number is " + TrimSigDigits( NumArray( 2 ), 0 ) );
				ErrorsFound = true;
			}

			//part load curve
			VarSpeedCoil( DXCoilNum ).PLFFPLR = GetCurveIndex( AlphArray( 4 ) ); // convert curve name to number
			if ( VarSpeedCoil( DXCoilNum ).PLFFPLR == 0 ) {
				if ( lAlphaBlanks( 4 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 4 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 4 ) + "=\"" + AlphArray( 4 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).PLFFPLR, 1.0 );
				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
					ShowContinueError( "..." + cAlphaFields( 4 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
					ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}
			}

			VarSpeedCoil( DXCoilNum ).DefrostEIRFT = GetCurveIndex( AlphArray( 5 ) ); // convert curve name to number

			if ( SameString( AlphArray( 6 ), "ReverseCycle" ) ) {
				if ( VarSpeedCoil( DXCoilNum ).DefrostEIRFT == 0 ) {
					if ( lAlphaBlanks( 5 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
						ShowContinueError( "...field is required because " + cAlphaFields( 6 ) + " is \"ReverseCycle\"." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + AlphArray( 5 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).DefrostEIRFT ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).DefrostEIRFT ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( SameString( AlphArray( 6 ), "ReverseCycle" ) ) VarSpeedCoil( DXCoilNum ).DefrostStrategy = ReverseCycle;
			if ( SameString( AlphArray( 6 ), "Resistive" ) ) VarSpeedCoil( DXCoilNum ).DefrostStrategy = Resistive;
			if ( VarSpeedCoil( DXCoilNum ).DefrostStrategy == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cAlphaFields( 6 ) + "=\"" + AlphArray( 6 ) + "\"." );
				ShowContinueError( "...valid values for this field are ReverseCycle or Resistive." );
				ErrorsFound = true;
			}

			if ( SameString( AlphArray( 7 ), "Timed" ) ) VarSpeedCoil( DXCoilNum ).DefrostControl = Timed;
			if ( SameString( AlphArray( 7 ), "OnDemand" ) ) VarSpeedCoil( DXCoilNum ).DefrostControl = OnDemand;
			if ( VarSpeedCoil( DXCoilNum ).DefrostControl == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cAlphaFields( 7 ) + "=\"" + AlphArray( 7 ) + "\"." );
				ShowContinueError( "...valid values for this field are Timed or OnDemand." );
				ErrorsFound = true;
			}

			//Set minimum OAT for heat pump compressor operation
			VarSpeedCoil( DXCoilNum ).MinOATCompressor = NumArray( 5 );

			// reserved for HSPF calculation
			VarSpeedCoil( DXCoilNum ).OATempCompressorOn = NumArray( 6 );

			//Set maximum outdoor temp for defrost to occur
			VarSpeedCoil( DXCoilNum ).MaxOATDefrost = NumArray( 7 );

			//Set crankcase heater capacity
			VarSpeedCoil( DXCoilNum ).CrankcaseHeaterCapacity = NumArray( 8 );
			if ( VarSpeedCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 9 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( NumArray( 9 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater cutout temperature
			VarSpeedCoil( DXCoilNum ).MaxOATCrankcaseHeater = NumArray( 9 );

			//Set defrost time period
			VarSpeedCoil( DXCoilNum ).DefrostTime = NumArray( 10 );
			if ( VarSpeedCoil( DXCoilNum ).DefrostTime == 0.0 && VarSpeedCoil( DXCoilNum ).DefrostControl == 1 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", " );
				ShowContinueError( "..." + cNumericFields( 5 ) + " = 0.0 for defrost control = TIMED." );
			}

			//Set defrost capacity (for resistive defrost)
			VarSpeedCoil( DXCoilNum ).DefrostCapacity = NumArray( 11 );
			if ( VarSpeedCoil( DXCoilNum ).DefrostCapacity == 0.0 && VarSpeedCoil( DXCoilNum ).DefrostStrategy == 2 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", " );
				ShowContinueError( "..." + cNumericFields( 6 ) + " = 0.0 for defrost strategy = RESISTIVE." );
			}

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) = NumArray( 12 + ( I - 1 ) * 3 );
				VarSpeedCoil( DXCoilNum ).MSRatedCOP( I ) = NumArray( 13 + ( I - 1 ) * 3 );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) = NumArray( 14 + ( I - 1 ) * 3 );

				if ( VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) < 1.e-10 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid value" );
					ShowContinueError( "...too small " + cNumericFields( 12 + ( I - 1 ) * 3 ) + "=[" + RoundSigDigits( VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ), 2 ) + "]." );
					ErrorsFound = true;
				}

				AlfaFieldIncre = 8 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ), RatedInletAirTempHeat, RatedAmbAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 9 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 10 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( I ), RatedInletAirTempHeat, RatedAmbAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( 1 ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				AlfaFieldIncre = 11 + ( I - 1 ) * 4;
				VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) = GetCurveIndex( AlphArray( AlfaFieldIncre ) ); // convert curve name to number
				if ( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( AlfaFieldIncre ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( AlfaFieldIncre ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( AlfaFieldIncre ) + "=\"" + AlphArray( AlfaFieldIncre ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( AlfaFieldIncre ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( AlfaFieldIncre ) + " type for this object = " + GetCurveType( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( ErrorsFound ) continue;

			for ( I = 1; I <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				VarSpeedCoil( DXCoilNum ).MSRatedPercentTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( VarSpeedCoil( DXCoilNum ).NumOfSpeeds );
				VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowPerRatedTotCap( I ) = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( I );
			}

			// CurrentModuleObject = "Coil:Heating:DX:Variablespeed "
			SetupOutputVariable( "Heating Coil Electric Energy [J]", VarSpeedCoil( DXCoilNum ).Energy, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Heating Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLoadTotal, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Source Side Heat Transfer Energy [J]", VarSpeedCoil( DXCoilNum ).EnergySource, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "HEATINGCOILS", _, "System" );

			//create predefined report entries
			PreDefTableEntry( pdchHeatCoilType, VarSpeedCoil( DXCoilNum ).Name, CurrentModuleObject );
			PreDefTableEntry( pdchHeatCoilNomCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapHeat );
			PreDefTableEntry( pdchHeatCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) );

		}

		//-------------------------AIR SOURCE HEATING---END


		//------------------------VARIABLE-SPEED AIR SOURCE HPWH---BEGIN
		CurrentModuleObject = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED"; //for reporting

		for (CoilCounter = 1; CoilCounter <= NumHPWHAirToWater; ++CoilCounter) {

			++DXCoilNum;
			AlfaFieldIncre = 1;

			GetObjectItem(CurrentModuleObject, CoilCounter, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields);

			IsNotOK = false;
			IsBlank = false;

			VerifyName(AlphArray(1), VarSpeedCoil, &VariableSpeedCoilData::Name, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name");
			if (IsNotOK) {
				ErrorsFound = true;
				if (IsBlank) AlphArray(1) = "xxxxx";
			}
			VerifyUniqueCoilName(CurrentModuleObject, AlphArray(1), errFlag, CurrentModuleObject + " Name");
			if (errFlag) {
				ErrorsFound = true;
			}

			VarSpeedCoil(DXCoilNum).bIsDesuperheater = false;
			VarSpeedCoil(DXCoilNum).CondenserType = WaterHeater;
			VarSpeedCoil(DXCoilNum).CoolHeatType = "WATERHEATING";
			VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum = CoilDX_HeatPumpWaterHeaterVariableSpeed;
			VarSpeedCoil(DXCoilNum).VarSpeedCoilType = cAllCoilTypes(CoilDX_HeatPumpWaterHeaterVariableSpeed);

			VarSpeedCoil(DXCoilNum).Name = AlphArray(1);
			VarSpeedCoil(DXCoilNum).NumOfSpeeds = int(NumArray(1));
			VarSpeedCoil(DXCoilNum).NormSpedLevel = int(NumArray(2));

			if (VarSpeedCoil(DXCoilNum).NumOfSpeeds < 1) {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError("..." + cNumericFields(1) + " must be >= 1." " entered number is " + TrimSigDigits(NumArray(1), 0));
				ErrorsFound = true;
			}

			if (VarSpeedCoil(DXCoilNum).NormSpedLevel > VarSpeedCoil(DXCoilNum).NumOfSpeeds) {
				VarSpeedCoil(DXCoilNum).NormSpedLevel = VarSpeedCoil(DXCoilNum).NumOfSpeeds;
			}

			if ((VarSpeedCoil(DXCoilNum).NormSpedLevel > VarSpeedCoil(DXCoilNum).NumOfSpeeds) || (VarSpeedCoil(DXCoilNum).NormSpedLevel <= 0)) {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError("..." + cNumericFields(2) + " must be valid speed level" " entered number is " + TrimSigDigits(NumArray(2), 0));
				ErrorsFound = true;
			}

			VarSpeedCoil(DXCoilNum).RatedCapWH = NumArray(3);
			if (VarSpeedCoil(DXCoilNum).RatedCapWH <= 0.0) {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError("..." + cNumericFields(3) + " must be > 0.0, entered value=[" + TrimSigDigits(NumArray(3), 2) + "].");
				ErrorsFound = true;
			}

			VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp = NumArray(4);
			VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp = NumArray(5);
			VarSpeedCoil(DXCoilNum).WHRatedInletWaterTemp = NumArray(6);

			VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = NumArray(7);
			VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate = NumArray(8);

			if (VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate != AutoCalculate) {
				if (VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate <= 0.0) {
					ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
					ShowContinueError("..." + cNumericFields(7) + " must be > 0.0.  entered value=[" + TrimSigDigits(NumArray(7), 3) + "].");
					ErrorsFound = true;
				}
			}

			if (VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate != AutoCalculate) {
				if (VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate <= 0.0) {
					ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
					ShowContinueError("..." + cNumericFields(8) + " must be > 0.0  entered value=[" + TrimSigDigits(NumArray(8), 3) + "].");
					ErrorsFound = true;
				}
			}

			if (SameString(AlphArray(2), "Yes") || SameString(AlphArray(2), "No")) {
				//  initialized to TRUE on allocate
				if (SameString(AlphArray(2), "No")) VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP = false;
				else VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP = true;
			} else {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError(",,,invalid choice for " + cAlphaFields(2) + ".  Entered choice = " + AlphArray(2));
				ShowContinueError("Valid choices are Yes or No.");
				ErrorsFound = true;
			}

			if (SameString(AlphArray(3), "Yes") || SameString(AlphArray(3), "No")) {
				//  initialized to FALSE on allocate
				if (SameString(AlphArray(3), "Yes")) VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP = true;
				else VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP = false;
			} else {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError(",,,invalid choice for " + cAlphaFields(3) + ".  Entered choice = " + AlphArray(3));
				ShowContinueError("Valid choices are Yes or No.");
				ErrorsFound = true;
			}

			if (SameString(AlphArray(4), "Yes") || SameString(AlphArray(4), "No")) {
				//  initialized to FALSE on allocate
				if (SameString(AlphArray(4), "Yes")) VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity = true;
				else VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity = false;
			} else {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError(",,,invalid choice for " + cAlphaFields(4) + ".  Entered choice = " + AlphArray(4));
				ShowContinueError("Valid choices are Yes or No.");
				ErrorsFound = true;
			}

			VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater = NumArray(9);
			if (VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater <= 0.0 || VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater > 1.0) {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError("..." + cNumericFields(10) + " must be >= 0 and <= 1.  entered value=[" + TrimSigDigits(NumArray(9), 3) + "].");
				ErrorsFound = true;
			}

			if ( !VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity ) {
				VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater = 0.0;
			}

			//Air nodes
			VarSpeedCoil(DXCoilNum).AirInletNodeNum = GetOnlySingleNode(AlphArray(5), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

			VarSpeedCoil(DXCoilNum).AirOutletNodeNum = GetOnlySingleNode(AlphArray(6), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

			if ((VarSpeedCoil(DXCoilNum).AirInletNodeNum > 0) && (VarSpeedCoil(DXCoilNum).AirOutletNodeNum > 0)) // IHP allows no node inputs for coil
			TestCompSet(CurrentModuleObject, AlphArray(1), AlphArray(5), AlphArray(6), "Air Nodes");

			//Check if the air inlet node is OA node, to justify whether the coil is placed in zone or not
			VarSpeedCoil( DXCoilNum ).IsDXCoilInZone = ! CheckOutAirNodeNumber( VarSpeedCoil( DXCoilNum ).AirInletNodeNum );
			
			//Water nodes
			VarSpeedCoil(DXCoilNum).WaterInletNodeNum = GetOnlySingleNode(AlphArray(7), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent);

			VarSpeedCoil(DXCoilNum).WaterOutletNodeNum = GetOnlySingleNode(AlphArray(8), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent);

			if ((VarSpeedCoil(DXCoilNum).WaterInletNodeNum > 0) && (VarSpeedCoil(DXCoilNum).WaterOutletNodeNum > 0)) // IHP allows no node inputs for coil
			TestCompSet(CurrentModuleObject, AlphArray(1), AlphArray(7), AlphArray(8), "Water Nodes");

			VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity = NumArray(10);
			if (VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity < 0.0) {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" +
					VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError("..." + cNumericFields(10) +
					" must be >= 0.0  entered value=[" + TrimSigDigits(NumArray(10), 1) + "].");
				ErrorsFound = true;
			}

			VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater = NumArray(11);
			if (VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater < 0.0) {
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" +
					VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError("..." + cNumericFields(11) +
					" must be >= 0 {C}.  entered value=[" + TrimSigDigits(NumArray(11), 1) + "].");
				ErrorsFound = true;
			}

			if (SameString(AlphArray(9), "DryBulbTemperature")) {
				VarSpeedCoil(DXCoilNum).InletAirTemperatureType = DryBulbIndicator;
			} else if (SameString(AlphArray(9), "WetBulbTemperature")) {
				VarSpeedCoil(DXCoilNum).InletAirTemperatureType = WetBulbIndicator;
			} else {
				//   wrong temperature type selection
				ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
				ShowContinueError("..." + cAlphaFields(9) + " must be DryBulbTemperature or WetBulbTemperature.");
				ShowContinueError("...entered value=\"" + AlphArray(9) + "\".");
				ErrorsFound = true;
			}

			// set rated inlet air temperature for curve object verification
			if (VarSpeedCoil(DXCoilNum).InletAirTemperatureType == WetBulbIndicator) {
				WHInletAirTemp = VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp;
			} else {
				WHInletAirTemp = VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp;
			}
			// set rated water temperature for curve object verification
			WHInletWaterTemp = VarSpeedCoil(DXCoilNum).WHRatedInletWaterTemp;

			//part load curve
			VarSpeedCoil(DXCoilNum).PLFFPLR = GetCurveIndex(AlphArray(10)); // convert curve name to number
			if (VarSpeedCoil(DXCoilNum).PLFFPLR == 0) {
				if (lAlphaBlanks(10)) {
					ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", missing");
					ShowContinueError("...required " + cAlphaFields(10) + " is blank.");
				} else {
					ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
					ShowContinueError("...not found " + cAlphaFields(10) + "=\"" + AlphArray(10) + "\".");
				}
				ErrorsFound = true;
			} else {
				CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).PLFFPLR, 1.0);
				if (CurveVal > 1.10 || CurveVal < 0.90) {
					ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
					ShowContinueError("..." + cAlphaFields(10) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
					ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
				}
			}

			for (I = 1; I <= VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
				VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) = NumArray(12 + (I - 1) * 6);
				VarSpeedCoil(DXCoilNum).MSRatedCOP(I) = NumArray(13 + (I - 1) * 6);
				VarSpeedCoil(DXCoilNum).MSRatedSHR(I) = NumArray(14 + (I - 1) * 6);
				VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) = NumArray(15 + (I - 1) * 6);
				VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) = NumArray(16 + (I - 1) * 6);
				VarSpeedCoil(DXCoilNum).MSWHPumpPower(I) = NumArray(17 + (I - 1) * 6);

				AlfaFieldIncre = 11 + (I - 1) * 6;
				VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)); // convert curve name to number
				if (VarSpeedCoil(DXCoilNum).MSCCapFTemp(I) == 0) {
					if (lAlphaBlanks(AlfaFieldIncre)) {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", missing");
						ShowContinueError("...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
					} else {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
						ShowContinueError("...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
						{ auto const SELECT_CASE_var(GetCurveType(VarSpeedCoil(DXCoilNum).MSCCapFTemp(I)));

						if (SELECT_CASE_var == "BIQUADRATIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapFTemp(I), WHInletAirTemp, WHInletWaterTemp);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else {
							ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
							ShowContinueError("...illegal " + cAlphaFields(AlfaFieldIncre) + " type for this object = " + GetCurveType(VarSpeedCoil(DXCoilNum).MSCCapFTemp(I)));
							ShowContinueError("Curve type must be BiQuadratic.");
							ErrorsFound = true;
						}}
				}

				AlfaFieldIncre = 12 + (I - 1) * 6;
				VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)); // convert curve name to number
				if (VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I) == 0) {
					if (lAlphaBlanks(AlfaFieldIncre)) {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", missing");
						ShowContinueError("...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
					} else {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
						ShowContinueError("...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
						{ auto const SELECT_CASE_var(GetCurveType(VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I)));

						if (SELECT_CASE_var == "QUADRATIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else if (SELECT_CASE_var == "CUBIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else {
							ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
							ShowContinueError("...illegal " + cAlphaFields(AlfaFieldIncre) + " type for this object = " + GetCurveType(VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(I)));
							ShowContinueError("Curve type must be Quadratic or Cubic.");
							ErrorsFound = true;
						}}
				}

				AlfaFieldIncre = 13 + (I - 1) * 6;
				VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)); // convert curve name to number
				if (VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I) == 0) {
					if (lAlphaBlanks(AlfaFieldIncre)) {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", missing");
						ShowContinueError("...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
					} else {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
						ShowContinueError("...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
						{ auto const SELECT_CASE_var(GetCurveType(VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I)));

						if (SELECT_CASE_var == "QUADRATIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else if (SELECT_CASE_var == "CUBIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else {
							ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
							ShowContinueError("...illegal " + cAlphaFields(AlfaFieldIncre) + " type for this object = " + GetCurveType(VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(I)));
							ShowContinueError("Curve type must be Quadratic or Cubic.");
							ErrorsFound = true;
						}}
				}

				AlfaFieldIncre = 14 + (I - 1) * 6;
				VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)); // convert curve name to number
				if (VarSpeedCoil(DXCoilNum).MSEIRFTemp(I) == 0) {
					if (lAlphaBlanks(AlfaFieldIncre)) {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", missing");
						ShowContinueError("...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
					} else {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
						ShowContinueError("...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
						{ auto const SELECT_CASE_var(GetCurveType(VarSpeedCoil(DXCoilNum).MSEIRFTemp(I)));

						if (SELECT_CASE_var == "BIQUADRATIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRFTemp(I), WHInletAirTemp, WHInletWaterTemp);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else {
							ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
							ShowContinueError("...illegal " + cAlphaFields(AlfaFieldIncre) + " type for this object = " + GetCurveType(VarSpeedCoil(DXCoilNum).MSEIRFTemp(1)));
							ShowContinueError("Curve type must be BiQuadratic.");
							ErrorsFound = true;
						}}
				}

				AlfaFieldIncre = 15 + (I - 1) * 6;
				VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)); // convert curve name to number
				if (VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I) == 0) {
					if (lAlphaBlanks(AlfaFieldIncre)) {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", missing");
						ShowContinueError("...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
					} else {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
						ShowContinueError("...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
						{ auto const SELECT_CASE_var(GetCurveType(VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I)));

						if (SELECT_CASE_var == "QUADRATIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else if (SELECT_CASE_var == "CUBIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else {
							ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
							ShowContinueError("...illegal " + cAlphaFields(AlfaFieldIncre) + " type for this object = " + GetCurveType(VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(I)));
							ShowContinueError("Curve type must be Quadratic or Cubic.");
							ErrorsFound = true;
						}}
				}

				AlfaFieldIncre = 16 + (I - 1) * 6;
				VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)); // convert curve name to number
				if (VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I) == 0) {
					if (lAlphaBlanks(AlfaFieldIncre)) {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", missing");
						ShowContinueError("...required " + cAlphaFields(AlfaFieldIncre) + " is blank.");
					} else {
						ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
						ShowContinueError("...not found " + cAlphaFields(AlfaFieldIncre) + "=\"" + AlphArray(AlfaFieldIncre) + "\".");
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
						{ auto const SELECT_CASE_var(GetCurveType(VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I)));

						if (SELECT_CASE_var == "QUADRATIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else if (SELECT_CASE_var == "CUBIC") {
							CurveVal = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I), 1.0);
							if (CurveVal > 1.10 || CurveVal < 0.90) {
								ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", curve values");
								ShowContinueError("..." + cAlphaFields(AlfaFieldIncre) + " output is not equal to 1.0 " "(+ or - 10%) at rated conditions.");
								ShowContinueError("...Curve output at rated conditions = " + TrimSigDigits(CurveVal, 3));
							}

						} else {
							ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + VarSpeedCoil(DXCoilNum).Name + "\", invalid");
							ShowContinueError("...illegal " + cAlphaFields(AlfaFieldIncre) + " type for this object = " + GetCurveType(VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(I)));
							ShowContinueError("Curve type must be Quadratic or Cubic.");
							ErrorsFound = true;
						}}
				}
			}

			//get scale values
			for (I = 1; I <= VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++I) {
				VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(I) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(I) / VarSpeedCoil(DXCoilNum).MSRatedTotCap(VarSpeedCoil(DXCoilNum).NumOfSpeeds);
				VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(I) / VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
				VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(I) / VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
				VarSpeedCoil(DXCoilNum).MSWHPumpPowerPerRatedTotCap(I) =
					VarSpeedCoil(DXCoilNum).MSWHPumpPower(I) / VarSpeedCoil(DXCoilNum).MSRatedTotCap(I);
			}

			// CurrentModuleObject = "Coil:Waterheating:Airtowaterheatpump:Variablespeed"
			SetupOutputVariable("Cooling Coil Electric Energy [J]", VarSpeedCoil(DXCoilNum).Energy, "System", "Summed", VarSpeedCoil(DXCoilNum).Name, _, "Electric", "Heating", _, "System");
			SetupOutputVariable("Cooling Coil Sensible Cooling Energy [J]", VarSpeedCoil(DXCoilNum).EnergySensible, "System", "Summed", VarSpeedCoil(DXCoilNum).Name);
			SetupOutputVariable("Cooling Coil Latent Cooling Energy [J]", VarSpeedCoil(DXCoilNum).EnergyLatent, "System", "Summed", VarSpeedCoil(DXCoilNum).Name);
			SetupOutputVariable("Cooling Coil Water Side Heat Transfer Energy [J]", VarSpeedCoil(DXCoilNum).EnergySource, "System", "Summed", VarSpeedCoil(DXCoilNum).Name, _, "PLANTLOOPHEATINGDEMAND", "HEATINGCOILS", _, "System");
			
			if( VarSpeedCoil( DXCoilNum ).IsDXCoilInZone ){
				SetupOutputVariable( "Cooling Coil Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLoadTotal, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			} else {
				SetupOutputVariable( "Cooling Coil Cooling Energy [J]", VarSpeedCoil( DXCoilNum ).EnergyLoadTotal, "System", "Summed", VarSpeedCoil( DXCoilNum ).Name );
			}

			VarSpeedCoil(DXCoilNum).RatedCapCoolSens = AutoSize; //always auto-sized, to be determined in the sizing calculation
		}
		//---------------------------VARIABLE-SPEED AIR SOURCE HPWH END --------------



		AlphArray.deallocate();
		cAlphaFields.deallocate();
		lAlphaBlanks.deallocate();
		cNumericFields.deallocate();
		lNumericBlanks.deallocate();
		NumArray.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found getting input. Program terminates." );
		}

		for ( DXCoilNum = 1; DXCoilNum <= NumWatertoAirHPs; ++DXCoilNum ) {
			if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) ||
				( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) ) {
				// Setup Report variables for the Heat Pump

				//cooling and heating coils separately
				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					// air source cooling coils
					SetupOutputVariable( "Cooling Coil Air Mass Flow Rate [kg/s]", VarSpeedCoil( DXCoilNum ).AirMassFlowRate, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Inlet Temperature [C]", VarSpeedCoil( DXCoilNum ).InletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).InletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", VarSpeedCoil( DXCoilNum ).QLatent, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Outlet Temperature [C]", VarSpeedCoil( DXCoilNum ).OutletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).OutletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", VarSpeedCoil( DXCoilNum ).QSensible, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", VarSpeedCoil( DXCoilNum ).QLoadTotal, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Part Load Ratio []", VarSpeedCoil( DXCoilNum ).PartLoadRatio, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Electric Power [W]", VarSpeedCoil( DXCoilNum ).Power, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Runtime Fraction []", VarSpeedCoil( DXCoilNum ).RunFrac, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Rate [W]", VarSpeedCoil( DXCoilNum ).QSource, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Upper Speed Level []", VarSpeedCoil( DXCoilNum ).SpeedNumReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Neighboring Speed Levels Ratio []", VarSpeedCoil( DXCoilNum ).SpeedRatioReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					if ( VarSpeedCoil( DXCoilNum ).CondensateCollectMode == CondensateToTank ) {
						SetupOutputVariable( "Cooling Coil Condensate Volume Flow Rate [m3/s]", VarSpeedCoil( DXCoilNum ).CondensateVdot, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
						SetupOutputVariable( "Cooling Coil Condensate Volume [m3]", VarSpeedCoil( DXCoilNum ).CondensateVol, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "OnSiteWater", "Condensate", _, "System" );
					}

					if ( VarSpeedCoil( DXCoilNum ).ReportEvapCondVars ) {
						SetupOutputVariable( "Cooling Coil Condenser Inlet Temperature [C]", VarSpeedCoil( DXCoilNum ).CondInletTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
						SetupOutputVariable( "Cooling Coil Evaporative Condenser Water Volume [m3]", VarSpeedCoil( DXCoilNum ).EvapWaterConsump, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "Water", "Cooling", _, "System" );
						SetupOutputVariable( "Cooling Coil Evaporative Condenser Mains Water Volume [m3]", VarSpeedCoil( DXCoilNum ).EvapWaterConsump, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "MainsWater", "Cooling", _, "System" );
						SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Power [W]", VarSpeedCoil( DXCoilNum ).EvapCondPumpElecPower, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
						SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Energy [J]", VarSpeedCoil( DXCoilNum ).EvapCondPumpElecConsumption, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "COOLING", _, "System" );
						if ( VarSpeedCoil( DXCoilNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
							SetupOutputVariable( "Cooling Coil Basin Heater Electric Power [W]", VarSpeedCoil( DXCoilNum ).BasinHeaterPower, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
							SetupOutputVariable( "Cooling Coil Basin Heater Electric Energy [J]", VarSpeedCoil( DXCoilNum ).BasinHeaterConsumption, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "COOLING", _, "System" );
						}
					}

					SetupOutputVariable( "Cooling Coil Crankcase Heater Electric Power [W]", VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Crankcase Heater Electric Energy [J]", VarSpeedCoil( DXCoilNum ).CrankcaseHeaterConsumption, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "COOLING", _, "System" );
				} else {
					// air source heating coils
					SetupOutputVariable( "Heating Coil Air Mass Flow Rate [kg/s]", VarSpeedCoil( DXCoilNum ).AirMassFlowRate, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Inlet Temperature [C]", VarSpeedCoil( DXCoilNum ).InletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).InletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Outlet Temperature [C]", VarSpeedCoil( DXCoilNum ).OutletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).OutletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Sensible Heating Rate [W]", VarSpeedCoil( DXCoilNum ).QSensible, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Heating Rate [W]", VarSpeedCoil( DXCoilNum ).QLoadTotal, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Part Load Ratio []", VarSpeedCoil( DXCoilNum ).PartLoadRatio, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Electric Power [W]", VarSpeedCoil( DXCoilNum ).Power, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Runtime Fraction []", VarSpeedCoil( DXCoilNum ).RunFrac, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					SetupOutputVariable( "Heating Coil Source Side Heat Transfer Rate [W]", VarSpeedCoil( DXCoilNum ).QSource, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Upper Speed Level []", VarSpeedCoil( DXCoilNum ).SpeedNumReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Neighboring Speed Levels Ratio []", VarSpeedCoil( DXCoilNum ).SpeedRatioReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					SetupOutputVariable( "Heating Coil Defrost Electric Power [W]", VarSpeedCoil( DXCoilNum ).DefrostPower, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Defrost Electric Energy [J]", VarSpeedCoil( DXCoilNum ).DefrostConsumption, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "HEATING", _, "System" );
					SetupOutputVariable( "Heating Coil Crankcase Heater Electric Power [W]", VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Crankcase Heater Electric Energy [J]", VarSpeedCoil( DXCoilNum ).CrankcaseHeaterConsumption, "System", "Sum", VarSpeedCoil( DXCoilNum ).Name, _, "Electric", "HEATING", _, "System" );

				}
			} else {

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit ) {
					// cooling WAHP coil
					// Setup Report variables for water source Heat Pump
					SetupOutputVariable( "Cooling Coil Electric Power [W]", VarSpeedCoil( DXCoilNum ).Power, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", VarSpeedCoil( DXCoilNum ).QLoadTotal, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", VarSpeedCoil( DXCoilNum ).QSensible, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", VarSpeedCoil( DXCoilNum ).QLatent, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Rate [W]", VarSpeedCoil( DXCoilNum ).QSource, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Part Load Ratio []", VarSpeedCoil( DXCoilNum ).PartLoadRatio, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Runtime Fraction []", VarSpeedCoil( DXCoilNum ).RunFrac, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					SetupOutputVariable( "Cooling Coil Air Mass Flow Rate [kg/s]", VarSpeedCoil( DXCoilNum ).AirMassFlowRate, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Inlet Temperature [C]", VarSpeedCoil( DXCoilNum ).InletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).InletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Outlet Temperature [C]", VarSpeedCoil( DXCoilNum ).OutletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).OutletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Source Side Mass Flow Rate [kg/s]", VarSpeedCoil( DXCoilNum ).WaterMassFlowRate, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Source Side Inlet Temperature [C]", VarSpeedCoil( DXCoilNum ).InletWaterTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Source Side Outlet Temperature [C]", VarSpeedCoil( DXCoilNum ).OutletWaterTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					SetupOutputVariable( "Cooling Coil Upper Speed Level []", VarSpeedCoil( DXCoilNum ).SpeedNumReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Neighboring Speed Levels Ratio []", VarSpeedCoil( DXCoilNum ).SpeedRatioReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Cooling Coil Recoverable Heat Transfer Rate [W]", VarSpeedCoil( DXCoilNum ).QWasteHeat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
				} else if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) {
					// heating WAHP coil
					// Setup Report variables for water source Heat Pump
					SetupOutputVariable( "Heating Coil Electric Power [W]", VarSpeedCoil( DXCoilNum ).Power, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Heating Rate [W]", VarSpeedCoil( DXCoilNum ).QLoadTotal, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Sensible Heating Rate [W]", VarSpeedCoil( DXCoilNum ).QSensible, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					SetupOutputVariable( "Heating Coil Source Side Heat Transfer Rate [W]", VarSpeedCoil( DXCoilNum ).QSource, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Part Load Ratio []", VarSpeedCoil( DXCoilNum ).PartLoadRatio, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Runtime Fraction []", VarSpeedCoil( DXCoilNum ).RunFrac, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					SetupOutputVariable( "Heating Coil Air Mass Flow Rate [kg/s]", VarSpeedCoil( DXCoilNum ).AirMassFlowRate, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Inlet Temperature [C]", VarSpeedCoil( DXCoilNum ).InletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).InletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Outlet Temperature [C]", VarSpeedCoil( DXCoilNum ).OutletAirDBTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil( DXCoilNum ).OutletAirHumRat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Source Side Mass Flow Rate [kg/s]", VarSpeedCoil( DXCoilNum ).WaterMassFlowRate, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Source Side Inlet Temperature [C]", VarSpeedCoil( DXCoilNum ).InletWaterTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Source Side Outlet Temperature [C]", VarSpeedCoil( DXCoilNum ).OutletWaterTemp, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );

					SetupOutputVariable( "Heating Coil Upper Speed Level []", VarSpeedCoil( DXCoilNum ).SpeedNumReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Neighboring Speed Levels Ratio []", VarSpeedCoil( DXCoilNum ).SpeedRatioReport, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
					SetupOutputVariable( "Heating Coil Recoverable Heat Transfer Rate [W]", VarSpeedCoil( DXCoilNum ).QWasteHeat, "System", "Average", VarSpeedCoil( DXCoilNum ).Name );
				} else if (VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed) {
					// air source water heating coil
					SetupOutputVariable("Cooling Coil Water Heating Electric Power [W]", VarSpeedCoil(DXCoilNum).Power, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Total Cooling Rate [W]", VarSpeedCoil(DXCoilNum).QLoadTotal, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Sensible Cooling Rate [W]", VarSpeedCoil(DXCoilNum).QSensible, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Latent Cooling Rate [W]", VarSpeedCoil(DXCoilNum).QLatent, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Total Water Heating Rate [W]", VarSpeedCoil(DXCoilNum).TotalHeatingEnergyRate, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Part Load Ratio []", VarSpeedCoil(DXCoilNum).PartLoadRatio, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Runtime Fraction []", VarSpeedCoil(DXCoilNum).RunFrac, "System", "Average", VarSpeedCoil(DXCoilNum).Name);

					SetupOutputVariable("Cooling Coil Air Mass Flow Rate [kg/s]", VarSpeedCoil(DXCoilNum).AirMassFlowRate, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Air Inlet Temperature [C]", VarSpeedCoil(DXCoilNum).InletAirDBTemp, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil(DXCoilNum).InletAirHumRat, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Air Outlet Temperature [C]", VarSpeedCoil(DXCoilNum).OutletAirDBTemp, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]", VarSpeedCoil(DXCoilNum).OutletAirHumRat, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Water Mass Flow Rate [kg/s]", VarSpeedCoil(DXCoilNum).WaterMassFlowRate, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Water Inlet Temperature [C]", VarSpeedCoil(DXCoilNum).InletWaterTemp, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Water Outlet Temperature [C]", VarSpeedCoil(DXCoilNum).OutletWaterTemp, "System", "Average", VarSpeedCoil(DXCoilNum).Name);

					SetupOutputVariable("Cooling Coil Crankcase Heater Electric Power [W]", VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Crankcase Heater Electric Energy [J]", VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption, "System", "Sum", VarSpeedCoil(DXCoilNum).Name, _, "Electric", "HEATING", _, "System");

					SetupOutputVariable("Cooling Coil Upper Speed Level []", VarSpeedCoil(DXCoilNum).SpeedNumReport, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Neighboring Speed Levels Ratio []", VarSpeedCoil(DXCoilNum).SpeedRatioReport, "System", "Average", VarSpeedCoil(DXCoilNum).Name);

					SetupOutputVariable("Cooling Coil Water Heating Pump Electric Power [W]", VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower, "System", "Average", VarSpeedCoil(DXCoilNum).Name);
					SetupOutputVariable("Cooling Coil Water Heating Pump Electric Energy [J]", VarSpeedCoil(DXCoilNum).EvapCondPumpElecConsumption, "System", "Sum", VarSpeedCoil(DXCoilNum).Name, _, "Electric", "HEATING", _, "System");
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination." );
		}

	}

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitVarSpeedCoil(
		int const DXCoilNum, // Current DXCoilNum under simulation
		Real64 const MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 const HPTimeConstant, // Heat pump time constant [s]
		Real64 const FanDelayTime, // Fan delay time, time delay for the HP's fan to
		Real64 const SensLoad, // Control zone sensible load[W]
		Real64 const LatentLoad, // Control zone latent load[W]
		int const CyclingScheme, // fan operating mode
		Real64 const EP_UNUSED( OnOffAirFlowRatio ), // ratio of compressor on flow to average flow over time step
		Real64 const SpeedRatio, // compressor speed ratio
		int const SpeedNum // compressor speed number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, based on  MODULE WaterToAirHeatPumpSimple:InitSimpleWatertoAirHP
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the variable speed Water to Air HP Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES: na

		// Using/Aliasing
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataGlobals::SysSizingCalc;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// shut off after compressor cycle off  [s]

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineNameSimpleWatertoAirHP( "InitSimpleWatertoAirHP" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirInletNode; // Node Number of the air inlet
		int WaterInletNode; // Node Number of the Water inlet
		static bool MyOneTimeFlag( true ); // one time allocation flag
		static Array1D_bool MyEnvrnFlag; // used for initializations each begin environment flag
		static Array1D_bool MySizeFlag; // used for sizing PTHP inputs one time
		static Array1D_bool MyPlantScanFlag;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		int SpeedCal; // calculated speed level
		bool errFlag;
		static bool ErrorsFound( false ); // TRUE when errors found, air loop initialization error
		Real64 RatedVolFlowPerRatedTotCap; // Rated Air Volume Flow Rate divided by Rated Total Capacity [m3/s-W)
		int Mode; // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Real64 RatedHeatPumpIndoorAirTemp;
		// Indoor dry-bulb temperature to heat pump evaporator at rated conditions [C]
		Real64 RatedHeatPumpIndoorHumRat; // Inlet humidity ratio to heat pump evaporator at rated conditions [kg/kg]
		Real64 WaterFlowScale; // water flow scaling factor match rated flow rate

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitVarSpeedCoil" );

		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MySizeFlag.allocate( NumWatertoAirHPs );
			MyEnvrnFlag.allocate( NumWatertoAirHPs );
			MyPlantScanFlag.allocate( NumWatertoAirHPs );
			MySizeFlag = true;
			MyEnvrnFlag = true;
			MyPlantScanFlag = true;
			MyOneTimeFlag = false;
		}


		//variable-speed heat pump water heating, begin
		if (VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed
			&& MySizeFlag(DXCoilNum)) {

			SizeVarSpeedCoil(DXCoilNum);

			//   get rated coil bypass factor excluding fan heat
			FanElecPower = 0.0;
			MySizeFlag(DXCoilNum) = false;
		}
		//variable-speed heat pump water heating, end


		// water source
		if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit ) || ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) ) {
			if ( MyPlantScanFlag( DXCoilNum ) && allocated( PlantLoop ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum, VarSpeedCoil( DXCoilNum ).LoopNum, VarSpeedCoil( DXCoilNum ).LoopSide, VarSpeedCoil( DXCoilNum ).BranchNum, VarSpeedCoil( DXCoilNum ).CompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitVarSpeedCoil: Program terminated for previous conditions." );
				}
				MyPlantScanFlag( DXCoilNum ) = false;
			}
		} else {
			MyPlantScanFlag( DXCoilNum ) = false;
		}

		if ( ! SysSizingCalc && MySizeFlag( DXCoilNum ) && ! MyPlantScanFlag( DXCoilNum ) ) {
			// for each furnace, do the sizing once.
			SizeVarSpeedCoil( DXCoilNum );

			MySizeFlag( DXCoilNum ) = false;

			// Multispeed Cooling
			if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit ) || ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) ) {
				for ( Mode = 1; Mode <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {
					// Check for zero capacity or zero max flow rate
					if ( VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode ) <= 0.0 ) {
						ShowSevereError( "Sizing: " + VarSpeedCoil( DXCoilNum ).VarSpeedCoilType + ' ' + VarSpeedCoil( DXCoilNum ).Name + " has zero rated total capacity at speed " + TrimSigDigits( Mode ) );
						ErrorsFound = true;
					}
					if ( VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) <= 0.0 ) {
						ShowSevereError( "Sizing: " + VarSpeedCoil( DXCoilNum ).VarSpeedCoilType + ' ' + VarSpeedCoil( DXCoilNum ).Name + " has zero rated air flow rate at speed " + TrimSigDigits( Mode ) );
						ErrorsFound = true;
					}
					if ( ErrorsFound ) {
						ShowFatalError( "Preceding condition causes termination." );
					}
					// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
					RatedVolFlowPerRatedTotCap = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode );
					//note: variable-speed HP can exceed the flow rate restrictions at low speed levels
					//        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
					//           ((RatedVolFlowPerRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
					//          CALL ShowSevereError('Sizing: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType) &
					//           // ' "'//TRIM(VarSpeedCoil(DXCoilNum)%Name)//  &
					//                '": Rated air volume flow rate per watt of rated total '// &
					//                'cooling capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
					//          CALL ShowContinueError &
					//           ('Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
					//           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
					//           Max Rated Vol Flow Per Watt=['// &
					//           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference Manual for valid range.')
					//        END IF
					//        VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
					//          PsyRhoAirFnPbTdbW(OutBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
					//        ! get high speed rated coil bypass factor
					//        VarSpeedCoil(DXCoilNum)%MSRatedCBF(Mode) = CalcCBF(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType, &
					//               VarSpeedCoil(DXCoilNum)%Name,&
					//                                           RatedInletAirTemp,RatedInletAirHumRat,VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode),&
					//                                           VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode), &
					//                           VarSpeedCoil(DXCoilNum)%MSRatedSHR(Mode))
				}
			}

			// Multispeed Heating
			if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) || ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) ) {
				RatedHeatPumpIndoorAirTemp = 21.11; // 21.11C or 70F
				RatedHeatPumpIndoorHumRat = 0.00881; // Humidity ratio corresponding to 70F dry bulb/60F wet bulb
				for ( Mode = 1; Mode <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {

					VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( Mode ) = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) * PsyRhoAirFnPbTdbW( OutBaroPress, RatedHeatPumpIndoorAirTemp, RatedHeatPumpIndoorHumRat, RoutineName );
					// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
					RatedVolFlowPerRatedTotCap = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode );
					//note: variable-speed HP can exceed the flow rate restrictions at low speed levels
					//        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
					//            ((RatedVolFlowperRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
					//          CALL ShowSevereError('Coil:Heating:DX:MultiSpeed '//TRIM(VarSpeedCoil(DXCoilNum)%Name)//  &
					//                              ': Rated air volume flow rate per watt of rated total '// &
					//                'heating capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
					//          CALL ShowContinueError('Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits &
					//           (MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
					//           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
					//               Max Rated Vol Flow Per Watt=['// &
					//           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference  &
					//                Manual for valid range.')
					//        END IF
				}
			}

		}

		if ( SpeedNum > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) {
			SpeedCal = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;
		} else if ( SpeedNum < 1 ) {
			SpeedCal = 1;
		} else {
			SpeedCal = SpeedNum;
		}

		if ( ( SpeedNum <= 1 ) || ( SpeedNum > VarSpeedCoil( DXCoilNum ).NumOfSpeeds ) ) {
			VarSpeedCoil( DXCoilNum ).DesignAirMassFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedCal );
			VarSpeedCoil( DXCoilNum ).DesignAirVolFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( SpeedCal );
			VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedWaterMassFlowRate( SpeedCal );
			VarSpeedCoil( DXCoilNum ).DesignWaterVolFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( SpeedCal );
		} else {
			VarSpeedCoil( DXCoilNum ).DesignAirMassFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedCal - 1 );
			VarSpeedCoil( DXCoilNum ).DesignAirVolFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( SpeedCal - 1 );
			VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedWaterMassFlowRate( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).MSRatedWaterMassFlowRate( SpeedCal - 1 );
			VarSpeedCoil( DXCoilNum ).DesignWaterVolFlowRate = VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( SpeedCal - 1 );
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( DXCoilNum ) && ! MyPlantScanFlag( DXCoilNum ) ) {
			// Do the initializations to start simulation

			//check air side nodes
			if ((0 == VarSpeedCoil(DXCoilNum).AirInletNodeNum) || (0 == VarSpeedCoil(DXCoilNum).AirOutletNodeNum)) {
				ShowFatalError("InitVarSpeedCoil: Invalid inlet or outlet air node" + TrimSigDigits(DXCoilNum) +
					", Inlet Node No =" + TrimSigDigits(VarSpeedCoil(DXCoilNum).AirInletNodeNum) + 
					", Outlet Node No =" + TrimSigDigits(VarSpeedCoil(DXCoilNum).AirOutletNodeNum));
			}


			AirInletNode = VarSpeedCoil( DXCoilNum ).AirInletNodeNum;

			//Initialize all report variables to a known state at beginning of simulation
			VarSpeedCoil( DXCoilNum ).AirVolFlowRate = 0.0;
			VarSpeedCoil( DXCoilNum ).InletAirDBTemp = 0.0;
			VarSpeedCoil( DXCoilNum ).InletAirHumRat = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletAirDBTemp = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletAirHumRat = 0.0;
			VarSpeedCoil( DXCoilNum ).WaterVolFlowRate = 0.0;
			VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = 0.0;
			VarSpeedCoil( DXCoilNum ).InletWaterTemp = 0.0;
			VarSpeedCoil( DXCoilNum ).InletWaterEnthalpy = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletWaterTemp = 0.0;
			VarSpeedCoil( DXCoilNum ).Power = 0.0;
			VarSpeedCoil( DXCoilNum ).QLoadTotal = 0.0;
			VarSpeedCoil( DXCoilNum ).QSensible = 0.0;
			VarSpeedCoil( DXCoilNum ).QLatent = 0.0;
			VarSpeedCoil( DXCoilNum ).QSource = 0.0;
			VarSpeedCoil( DXCoilNum ).Energy = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergyLoadTotal = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergySensible = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergyLatent = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergySource = 0.0;
			VarSpeedCoil( DXCoilNum ).COP = 0.0;
			VarSpeedCoil( DXCoilNum ).RunFrac = 0.0;
			VarSpeedCoil( DXCoilNum ).PartLoadRatio = 0.0;

			VarSpeedCoil( DXCoilNum ).MaxONOFFCyclesperHour = MaxONOFFCyclesperHour;
			VarSpeedCoil( DXCoilNum ).HPTimeConstant = HPTimeConstant;
			VarSpeedCoil( DXCoilNum ).FanDelayTime = FanDelayTime;

			if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) || ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit ) ) {
				
				//check water side nodes
				if ((0 == VarSpeedCoil(DXCoilNum).WaterInletNodeNum) || (0 == VarSpeedCoil(DXCoilNum).WaterOutletNodeNum)) {
					ShowFatalError("InitVarSpeedCoil: Invalid inlet or outlet water node" + TrimSigDigits(DXCoilNum) +
						", Inlet Node No =" + TrimSigDigits(VarSpeedCoil(DXCoilNum).WaterInletNodeNum) +
						", Outlet Node No =" + TrimSigDigits(VarSpeedCoil(DXCoilNum).WaterOutletNodeNum));
				}
				
				
				WaterInletNode = VarSpeedCoil( DXCoilNum ).WaterInletNodeNum;

				rho = GetDensityGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineNameSimpleWatertoAirHP );
				Cp = GetSpecificHeatGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineNameSimpleWatertoAirHP );

				//    VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate= &
				//                             rho * VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate

				InitComponentNodes( 0.0, VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate, VarSpeedCoil( DXCoilNum ).WaterInletNodeNum, VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum, VarSpeedCoil( DXCoilNum ).LoopNum, VarSpeedCoil( DXCoilNum ).LoopSide, VarSpeedCoil( DXCoilNum ).BranchNum, VarSpeedCoil( DXCoilNum ).CompNum );

				Node( WaterInletNode ).Temp = 5.0;
				Node( WaterInletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
				Node( WaterInletNode ).Quality = 0.0;
				Node( WaterInletNode ).Press = 0.0;
				Node( WaterInletNode ).HumRat = 0.0;

				Node( VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum ).Temp = 5.0;
				Node( VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
				Node( VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum ).Quality = 0.0;
				Node( VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum ).Press = 0.0;
				Node( VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum ).HumRat = 0.0;
			}

			VarSpeedCoil( DXCoilNum ).SimFlag = true;

			MyEnvrnFlag( DXCoilNum ) = false;

		} // End If for the Begin Environment initializations

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( DXCoilNum ) = true;
		}

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.
		// First set the conditions for the air into the heat pump model

		// Set water and air inlet nodes

		AirInletNode = VarSpeedCoil( DXCoilNum ).AirInletNodeNum;
		WaterInletNode = VarSpeedCoil( DXCoilNum ).WaterInletNodeNum;

		if ( ( SensLoad != 0.0 || LatentLoad != 0.0 ) && ( Node( AirInletNode ).MassFlowRate > 0.0 ) ) {

			if ( VarSpeedCoil( DXCoilNum ).MSRatedWaterMassFlowRate( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) > 0.0 ) {
				WaterFlowScale = VarSpeedCoil( DXCoilNum ).RatedWaterMassFlowRate / VarSpeedCoil( DXCoilNum ).MSRatedWaterMassFlowRate( VarSpeedCoil( DXCoilNum ).NormSpedLevel );
				VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate * WaterFlowScale;
			} else {
				VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = 0.0;
			}

			if ( CyclingScheme == ContFanCycCoil ) {
				// continuous fan, cycling compressor
				VarSpeedCoil( DXCoilNum ).AirMassFlowRate = Node( AirInletNode ).MassFlowRate;
				//    VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = VarSpeedCoil(DXCoilNum)%DesignAirVolFlowRate*  &
				//             PsyRhoAirFnPbTdbW(OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
				//If air flow is less than 25% rated flow. Then set air flow to the 25% of rated conditions
				if ( VarSpeedCoil( DXCoilNum ).AirMassFlowRate < 0.25 * VarSpeedCoil( DXCoilNum ).DesignAirVolFlowRate * PsyRhoAirFnPbTdbW( OutBaroPress, Node( AirInletNode ).Temp, Node( AirInletNode ).HumRat ) ) {
					VarSpeedCoil( DXCoilNum ).AirMassFlowRate = 0.25 * VarSpeedCoil( DXCoilNum ).DesignAirVolFlowRate * PsyRhoAirFnPbTdbW( OutBaroPress, Node( AirInletNode ).Temp, Node( AirInletNode ).HumRat );
				}
			} else { //CYCLIC FAN, NOT CORRECTION, WILL BE PROCESSED IN THE FOLLOWING SUBROUTINES
				VarSpeedCoil( DXCoilNum ).AirMassFlowRate = Node( AirInletNode ).MassFlowRate;
			}

		} else { //heat pump is off
			VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = 0.0;
			VarSpeedCoil( DXCoilNum ).AirMassFlowRate = 0.0;
		}

		if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) || ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit ) ) {
			SetComponentFlowRate( VarSpeedCoil( DXCoilNum ).WaterMassFlowRate, VarSpeedCoil( DXCoilNum ).WaterInletNodeNum, VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum, VarSpeedCoil( DXCoilNum ).LoopNum, VarSpeedCoil( DXCoilNum ).LoopSide, VarSpeedCoil( DXCoilNum ).BranchNum, VarSpeedCoil( DXCoilNum ).CompNum );

			VarSpeedCoil( DXCoilNum ).InletWaterTemp = Node( WaterInletNode ).Temp;
			VarSpeedCoil( DXCoilNum ).InletWaterEnthalpy = Node( WaterInletNode ).Enthalpy;
		} else {
			VarSpeedCoil( DXCoilNum ).InletWaterTemp = 0.0;
			VarSpeedCoil( DXCoilNum ).InletWaterEnthalpy = 0.0;
		}

		if ( VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed ) {
			VarSpeedCoil(DXCoilNum).InletWaterTemp = Node(WaterInletNode).Temp;
			VarSpeedCoil(DXCoilNum).InletWaterEnthalpy = Node(WaterInletNode).Enthalpy;
		};

		VarSpeedCoil( DXCoilNum ).InletAirDBTemp = Node( AirInletNode ).Temp;
		VarSpeedCoil( DXCoilNum ).InletAirHumRat = Node( AirInletNode ).HumRat;
		VarSpeedCoil( DXCoilNum ).InletAirEnthalpy = Node( AirInletNode ).Enthalpy;

		VarSpeedCoil( DXCoilNum ).MaxONOFFCyclesperHour = MaxONOFFCyclesperHour;
		VarSpeedCoil( DXCoilNum ).HPTimeConstant = HPTimeConstant;
		VarSpeedCoil( DXCoilNum ).FanDelayTime = FanDelayTime;

		VarSpeedCoil( DXCoilNum ).InletAirPressure = OutBaroPress; //temporary
		// Outlet variables
		VarSpeedCoil( DXCoilNum ).Power = 0.0;
		VarSpeedCoil( DXCoilNum ).QLoadTotal = 0.0;
		VarSpeedCoil( DXCoilNum ).QSensible = 0.0;
		VarSpeedCoil( DXCoilNum ).QLatent = 0.0;
		VarSpeedCoil( DXCoilNum ).QSource = 0.0;
		VarSpeedCoil( DXCoilNum ).QWasteHeat = 0.0;
		VarSpeedCoil( DXCoilNum ).Energy = 0.0;
		VarSpeedCoil( DXCoilNum ).EnergyLoadTotal = 0.0;
		VarSpeedCoil( DXCoilNum ).EnergySensible = 0.0;
		VarSpeedCoil( DXCoilNum ).EnergyLatent = 0.0;
		VarSpeedCoil( DXCoilNum ).EnergySource = 0.0;
		VarSpeedCoil( DXCoilNum ).COP = 0.0;

		VarSpeedCoil( DXCoilNum ).OutletAirDBTemp = 0.0;
		VarSpeedCoil( DXCoilNum ).OutletWaterTemp = 0.0;
		VarSpeedCoil( DXCoilNum ).OutletAirHumRat = 0.0;
		VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy = 0.0;
		VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy = 0.0;

		// bug fix, must set zeros to the variables below, otherwise can't pass switch DD test
		VarSpeedCoil( DXCoilNum ).CrankcaseHeaterConsumption = 0.0;
		VarSpeedCoil( DXCoilNum ).EvapWaterConsump = 0.0;
		VarSpeedCoil( DXCoilNum ).BasinHeaterConsumption = 0.0;
		VarSpeedCoil( DXCoilNum ).EvapCondPumpElecConsumption = 0.0;
		VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower = 0.0;
		VarSpeedCoil( DXCoilNum ).DefrostConsumption = 0.0;
		VarSpeedCoil( DXCoilNum ).CondensateVdot = 0.0;
		VarSpeedCoil( DXCoilNum ).CondensateVol = 0.0;
		VarSpeedCoil( DXCoilNum ).QWasteHeat = 0.0;

		//clear zeros to HPWH variables
		VarSpeedCoil(DXCoilNum).ElecWaterHeatingPower = 0.0; // Total electric power consumed by compressor and condenser pump [W]
		VarSpeedCoil(DXCoilNum).ElecWaterHeatingConsumption = 0.0; // Total electric consumption by compressor and condenser pump [J]
		VarSpeedCoil(DXCoilNum).TotalHeatingEnergy = 0.0; //total water heating energy
		VarSpeedCoil(DXCoilNum).TotalHeatingEnergyRate = 0.0;//total WH energy rate
		VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower = 0.0;//power power

		VSHPWHHeatingCapacity = 0.0; // Used by Heat Pump:Water Heater object as total water heating capacity [W]
		VSHPWHHeatingCOP = 0.0; // Used by Heat Pump:Water Heater object as water heating COP [W/W]
		VarSpeedCoil(DXCoilNum).OutletWaterTemp = VarSpeedCoil(DXCoilNum).InletWaterTemp;
	}

	void
	SizeVarSpeedCoil( int const DXCoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:SizeHVACWaterToAir
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//       MODIFIED       Bo Shen, 12/2014, add variable-speed HPWH
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing WSHP Components for which nominal capacities
		// and flow rates have not been specified in the input

		// METHODOLOGY EMPLOYED:
		// Obtains heating capacities and flow rates from the zone or system sizing arrays.
		// NOTE: For WSHP's we are sizing the heating capacity to be
		// equal to the cooling capacity.  Thus the cooling and
		// and heating capacities of a DX heat pump system will be identical. In real life the ARI
		// heating and cooling capacities are close but not identical.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Psychrometrics;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using DataHVACGlobals::SmallAirVolFlow;
		using DataHVACGlobals::SmallLoad;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using DataAirSystems::PrimaryAirSystem;
		using namespace OutputReportPredefined;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using CurveManager::CurveValue;

		// Locals
		Real64 QLoadTotal; // placeholder for calculating SHR

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeVarSpeedCoil" );
		static std::string const RoutineNameAlt( "SizeHVACWaterToAir" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 rhoair;
		Real64 MixTemp;
		Real64 MixHumRat;
		Real64 MixEnth;
		Real64 MixWetBulb;
		Real64 SupTemp;
		Real64 SupHumRat;
		Real64 SupEnth;
		Real64 OutTemp;
		Real64 OutAirFrac;
		Real64 VolFlowRate;
		Real64 CoolCapAtPeak;
		Real64 TotCapTempModFac;
		int TimeStepNumAtMax;
		int DDNum;
		int PltSizNum;
		bool RatedCapCoolTotalAutoSized;
		bool RatedCapCoolSensAutoSized;
		bool RatedCapHeatAutoSized;
		bool RatedAirFlowAutoSized;
		bool RatedWaterFlowAutoSized;
		bool ErrorsFound;
		Real64 SystemCapacity;
		Real64 rho;
		Real64 cp;
		int NormSpeed; // norminal speed level
		int UpperSpeed; // highest speed level
		int Mode; // speed level
		Real64 rhoW; // water density
		Real64 rhoA; // air density
		Real64 SHR; // sensible heat transfer ratio
		Real64 RatedAirMassFlowRate; // rated air mass flow rate
		Real64 CBFRated; // bypass factor at the rated condition, considering difference in flow rates
		Real64 RatedInletEnth; // rated inlet air enthalpy
		Real64 QLoadTotal1; // placeholder for calculating SHR
		Real64 QLoadTotal2; // placeholder for calculating SHR
		Real64 AirMassFlowRatio; // air mass flow ratio
		Real64 WaterMassFlowRatio; // water mass flow rate
		Real64 RatedSourceTempCool; // rated source temperature, space cooling mode
		std::string CurrentObjSubfix; // Object subfix type for printing
		bool IsAutoSize; // Indicator to autosize
		bool HardSizeNoDesRun; // Indicator to hardsize withouth sizing runs
		bool HardSizeNoDesRunAirFlow; // Indicator to hardsize withouth sizing runs for rated air flow field
		Real64 RatedAirVolFlowRateDes; // Autosized rated air flow for reporting
		Real64 RatedAirVolFlowRateUser; // Hardsized rated air flow for reporting
		Real64 RatedCapCoolTotalDes; // Autosized rated total cooling capacity for reporting
		Real64 RatedCapCoolTotalUser; // Hardsized rated total cooling capacity for reporting
		Real64 RatedCapHeatDes; // Autosized rated heating capacity for reporting
		Real64 RatedCapHeatUser; // Hardsized rated heating capacity for reporting
		Real64 RatedWaterVolFlowRateDes; // Autosized rated water flow for reporting
		Real64 RatedWaterVolFlowRateUser; // Hardsized rated water flow for reporting
		Real64 RatedCapCoolSensDes; // Autosized rated sensible cooling capacity for reporting
		Real64 RatedCapCoolSensUser; // Hardsized rated sensible cooling capacity for reporting
		Real64 EvapCondPumpElecNomPowerDes; // Autosized evaporative condenser pump power for reporting
		Real64 EvapCondPumpElecNomPowerUser; // Hardsized evaporative condenser pump power for reporting
		Real64 DefrostCapacityDes; // Autosized resistive defrost heater capacity for reporting
		Real64 DefrostCapacityUser; // Hardsized resistive defrost heater capacity for reporting
		bool SizingDesRunThisAirSys; // true if a particular air system had a Sizing:System object and system sizing done
		bool SizingDesRunThisZone; // true if a particular zone had a Sizing:Zone object and zone sizing was done
		Real64 HPInletAirHumRat; // Rated inlet air humidity ratio for heat pump water heater [kgWater/kgDryAir]
		Real64 HPWHCoolCapacity; //estimate cooling capacity in HPWH

		UpperSpeed = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;
		NormSpeed = VarSpeedCoil( DXCoilNum ).NormSpedLevel;
		PltSizNum = 0;
		ErrorsFound = false;
		RatedAirFlowAutoSized = false;
		RatedWaterFlowAutoSized = false;
		RatedCapHeatAutoSized = false;
		IsAutoSize = false;
		if ( SysSizingRunDone || ZoneSizingRunDone ) {
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
		HardSizeNoDesRunAirFlow = false;
		RatedAirVolFlowRateDes = 0.0;
		RatedAirVolFlowRateUser = 0.0;
		RatedCapCoolTotalDes = 0.0;
		RatedCapCoolTotalUser = 0.0;
		RatedCapHeatDes = 0.0;
		RatedCapHeatUser = 0.0;
		RatedWaterVolFlowRateDes = 0.0;
		RatedWaterVolFlowRateUser = 0.0;
		RatedCapCoolSensDes = 0.0;
		RatedCapCoolSensUser = 0.0;
		EvapCondPumpElecNomPowerDes = 0.0;
		EvapCondPumpElecNomPowerUser = 0.0;
		DefrostCapacityDes = 0.0;
		DefrostCapacityUser = 0.0;

		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit || VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) {
			CurrentObjSubfix = ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT";
		} else if (VarSpeedCoil(DXCoilNum).VSCoilTypeOfNum == CoilDX_HeatPumpWaterHeaterVariableSpeed) {
			CurrentObjSubfix = ":WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
		} else {
			CurrentObjSubfix = ":DX:VARIABLESPEED";
		}

		if (VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
			if (VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate == AutoCalculate) {
				VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate = VarSpeedCoil(DXCoilNum).RatedCapWH *
					VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(NormSpeed) /
					VarSpeedCoil(DXCoilNum).MSRatedTotCap(NormSpeed);//0.00005035;
				VarSpeedCoil(DXCoilNum).AirVolFlowAutoSized = true;
			}

			if (VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate == AutoCalculate) {
				VarSpeedCoil(DXCoilNum).RatedHPWHCondWaterFlow = VarSpeedCoil(DXCoilNum).RatedCapWH *
					VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(NormSpeed) /
					VarSpeedCoil(DXCoilNum).MSRatedTotCap(NormSpeed);// 0.00000004487;
				VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate =
					VarSpeedCoil(DXCoilNum).RatedHPWHCondWaterFlow;
				VarSpeedCoil(DXCoilNum).WaterVolFlowAutoSized = true;
			}
		}

		if ( VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate == AutoSize ) {
			RatedAirFlowAutoSized = true;
		}

		if ( CurSysNum > 0 ) {
			if ( ! RatedAirFlowAutoSized && ! SizingDesRunThisAirSys ) { // Simulation continue
				HardSizeNoDesRunAirFlow = true;
				if ( VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate > 0.0 ) {
					ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "User-Specified Rated Air Flow Rate [m3/s]", VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate );
				}
			} else {
				CheckSysSizing( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name );
				if ( FinalSysSizing( CurSysNum ).DesMainVolFlow >= SmallAirVolFlow ) {
					RatedAirVolFlowRateDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				} else {
					RatedAirVolFlowRateDes = 0.0;
				}
			}
		}

		if ( CurZoneEqNum > 0 ) {
			if ( ! RatedAirFlowAutoSized && ! SizingDesRunThisZone ) { // Simulation continue
				HardSizeNoDesRunAirFlow = true;
				if ( VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate > 0.0 ) {
					ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "User-Specified Rated Air Flow Rate [m3/s]", VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate );
				}
			} else {
				CheckZoneSizing( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name );
				RatedAirVolFlowRateDes = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( RatedAirVolFlowRateDes < SmallAirVolFlow ) {
					RatedAirVolFlowRateDes = 0.0;
				}
			}
		}

		if ( RatedAirFlowAutoSized ) VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate = RatedAirVolFlowRateDes;
		//    CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
		//                             VarSpeedCoil(DXCoilNum)%Name, &
		//                            'Rated Air Flow Rate [m3/s]', &
		//                             VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate)

		//RatedAirFlowAutoSized = .TRUE.

		RatedCapCoolTotalAutoSized = false;
		RatedCapCoolSensAutoSized = false;

		// size rated total cooling capacity
		IsAutoSize = false;
		if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal == AutoSize && VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) {
			RatedCapCoolTotalAutoSized = true;
		}
		if ( SizingDesRunThisZone || SizingDesRunThisAirSys ) HardSizeNoDesRun = false;
		if ( CurSysNum > 0 ) {
			if ( ! RatedCapCoolTotalAutoSized && ! SizingDesRunThisAirSys ) { // Simulation continue
				HardSizeNoDesRun = true;
				if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal > 0.0 ) {
					ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "User-Specified Rated Total Cooling Capacity [W]", VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
				}
			} else {
				CheckSysSizing( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name );
				VolFlowRate = VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate;
				if ( VolFlowRate >= SmallAirVolFlow ) {
					if ( CurOASysNum > 0 ) { // coil is in the OA stream
						MixTemp = FinalSysSizing( CurSysNum ).OutTempAtCoolPeak;
						MixHumRat = FinalSysSizing( CurSysNum ).OutHumRatAtCoolPeak;
						SupTemp = FinalSysSizing( CurSysNum ).PrecoolTemp;
						SupHumRat = FinalSysSizing( CurSysNum ).PrecoolHumRat;
					} else { // coil is on the main air loop
						SupTemp = FinalSysSizing( CurSysNum ).CoolSupTemp;
						SupHumRat = FinalSysSizing( CurSysNum ).CoolSupHumRat;
						if ( PrimaryAirSystem( CurSysNum ).NumOACoolCoils == 0 ) { // there is no precooling of the OA stream
							MixTemp = FinalSysSizing( CurSysNum ).MixTempAtCoolPeak;
							MixHumRat = FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak;
						} else { // there is precooling of OA stream
							if ( VolFlowRate > 0.0 ) {
								OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / VolFlowRate;
							} else {
								OutAirFrac = 1.0;
							}
							OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
							MixTemp = OutAirFrac * FinalSysSizing( CurSysNum ).PrecoolTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).RetTempAtCoolPeak;
							MixHumRat = OutAirFrac * FinalSysSizing( CurSysNum ).PrecoolHumRat + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).RetHumRatAtCoolPeak;
						}
					}
					OutTemp = FinalSysSizing( CurSysNum ).OutTempAtCoolPeak;
					rhoair = PsyRhoAirFnPbTdbW( OutBaroPress, MixTemp, MixHumRat, RoutineName );
					MixEnth = PsyHFnTdbW( MixTemp, MixHumRat );
					MixWetBulb = PsyTwbFnTdbWPb( MixTemp, MixHumRat, OutBaroPress, RoutineName );
					SupEnth = PsyHFnTdbW( SupTemp, SupHumRat );

					TotCapTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( VarSpeedCoil( DXCoilNum ).NormSpedLevel ), MixWetBulb, RatedInletWaterTemp );
					//       The mixed air temp for zone equipment without an OA mixer is 0.
					//       This test avoids a negative capacity until a solution can be found.
					if ( MixEnth > SupEnth ) {
						CoolCapAtPeak = rhoair * VolFlowRate * ( MixEnth - SupEnth );
					} else {
						CoolCapAtPeak = rhoair * VolFlowRate * ( 48000.0 - SupEnth );
					}
					CoolCapAtPeak = max( 0.0, CoolCapAtPeak );
					if ( TotCapTempModFac > 0.0 ) {
						RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac;
					} else {
						RatedCapCoolTotalDes = CoolCapAtPeak;
					}
				} else {
					RatedCapCoolTotalDes = 0.0;
				}
			}

		} else if ( CurZoneEqNum > 0 ) {
			if ( ! RatedCapCoolTotalAutoSized && ! SizingDesRunThisZone ) { // Simulation continue
				HardSizeNoDesRun = true;
				if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal > 0.0 ) {
					ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "User-Specified Rated Total Cooling Capacity [W]", VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
				}
			} else {
				CheckZoneSizing( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name );
				VolFlowRate = VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate;
				if ( VolFlowRate >= SmallAirVolFlow ) {
					if ( ZoneEqDXCoil ) {
						if ( ZoneEqSizing( CurZoneEqNum ).OAVolFlow > 0.0 ) {
							MixTemp = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp;
							MixHumRat = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat;
						} else {
							MixTemp = FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak;
							MixHumRat = FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak;
						}
					} else {
						MixTemp = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp;
						MixHumRat = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat;
					}
					SupTemp = FinalZoneSizing( CurZoneEqNum ).CoolDesTemp;
					SupHumRat = FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat;
					TimeStepNumAtMax = FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax;
					DDNum = FinalZoneSizing( CurZoneEqNum ).CoolDDNum;
					if ( DDNum > 0 && TimeStepNumAtMax > 0 ) {
						OutTemp = DesDayWeath( DDNum ).Temp( TimeStepNumAtMax );
					} else {
						OutTemp = 0.0;
					}
					rhoair = PsyRhoAirFnPbTdbW( OutBaroPress, MixTemp, MixHumRat, RoutineName );
					MixEnth = PsyHFnTdbW( MixTemp, MixHumRat );
					MixWetBulb = PsyTwbFnTdbWPb( MixTemp, MixHumRat, OutBaroPress, RoutineName );
					SupEnth = PsyHFnTdbW( SupTemp, SupHumRat );

					TotCapTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( VarSpeedCoil( DXCoilNum ).NormSpedLevel ), MixWetBulb, RatedInletWaterTemp );
					//       The mixed air temp for zone equipment without an OA mixer is 0.
					//       This test avoids a negative capacity until a solution can be found.
					if ( MixEnth > SupEnth ) {
						CoolCapAtPeak = rhoair * VolFlowRate * ( MixEnth - SupEnth );
					} else {
						CoolCapAtPeak = rhoair * VolFlowRate * ( 48000.0 - SupEnth );
					}
					CoolCapAtPeak = max( 0.0, CoolCapAtPeak );
					if ( TotCapTempModFac > 0.0 ) {
						RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac;
					} else {
						RatedCapCoolTotalDes = CoolCapAtPeak;
					}
				} else {
					RatedCapCoolTotalDes = 0.0;
				}
			}
		}
		if ( RatedCapCoolTotalDes < SmallLoad ) {
			RatedCapCoolTotalDes = 0.0;
		}
		if ( ! HardSizeNoDesRun ) {
			if ( RatedCapCoolTotalAutoSized ) {
				VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal = RatedCapCoolTotalDes;
				ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Total Cooling Capacity [W]", VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
				PreDefTableEntry( pdchCoolCoilTotCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
				PreDefTableEntry( pdchCoolCoilLatCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal - VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
				if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal != 0.0 ) {
					PreDefTableEntry( pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens / VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
					PreDefTableEntry( pdchCoolCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( NormSpeed ) );
				} else {
					PreDefTableEntry( pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, 0.0 );
					PreDefTableEntry( pdchCoolCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, 0.0 );
				}
				addFootNoteSubTable( pdstCoolCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );
			} else {
				if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal > 0.0 && RatedCapCoolTotalDes > 0.0 ) {
					RatedCapCoolTotalUser = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal;
					ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Total Cooling Capacity [W]", RatedCapCoolTotalDes, "User-Specified Rated Total Cooling Capacity [W]", RatedCapCoolTotalUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( RatedCapCoolTotalDes - RatedCapCoolTotalUser ) / RatedCapCoolTotalUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeVarSpeedCoil: Potential issue with equipment sizing for " + VarSpeedCoil( DXCoilNum ).CoolHeatType + ' ' + CurrentObjSubfix );
							ShowContinueError( "Coil Name =" + VarSpeedCoil( DXCoilNum ).Name );
							ShowContinueError( "User-Specified Rated Total Cooling Capacity of " + RoundSigDigits( RatedCapCoolTotalUser, 2 ) + " [W]" );
							ShowContinueError( "differs from Design Size Rated Total Cooling Capacity of " + RoundSigDigits( RatedCapCoolTotalDes, 2 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}

		// Set the global DX cooling coil capacity variable for use by other objects
		if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) {
			DXCoolCap = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal;
		}

		// size rated heating capacity
		if ( VarSpeedCoil( DXCoilNum ).RatedCapHeat == AutoSize && VarSpeedCoil( DXCoilNum ).CoolHeatType == "HEATING" ) {
			RatedCapHeatAutoSized = true;
		}
		//   simply set heating capacity equal to the cooling capacity
		//VarSpeedCoil(DXCoilNum)%RatedCapHeat = DXCoolCap
		if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "HEATING" ) {
			if ( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum > 0 ) {
				RatedCapHeatDes = VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).RatedCapCoolTotal;
				VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal = RatedCapHeatDes; //AVOID BEING ZERO
			} else {
				RatedCapHeatDes = DXCoolCap; //previous code, can be risky
			}
			//END IF
			if ( RatedCapHeatAutoSized ) {
				if ( RatedCapHeatDes == AutoSize ) {
					ShowWarningError( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" + VarSpeedCoil( DXCoilNum ).Name + "\"" );
					ShowContinueError( RoutineName + ": Heating coil could not be autosized since cooling coil was not previously sized." );
					ShowContinueError( "... Cooling coil must be upstream of heating coil." );
					ShowContinueError( "... Manually sizing this heating coil will be required." );
				}
			}
			if ( RatedCapHeatDes < SmallLoad ) {
				RatedCapHeatDes = 0.0;
			}
		}
		if ( RatedCapHeatAutoSized ) {
			VarSpeedCoil( DXCoilNum ).RatedCapHeat = RatedCapHeatDes;
			ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Nominal Heating Capacity [W]", RatedCapHeatDes );
			PreDefTableEntry( pdchHeatCoilNomCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapHeat );
			if ( VarSpeedCoil( DXCoilNum ).RatedCapHeat != 0.0 ) {
				PreDefTableEntry( pdchHeatCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( NormSpeed ) );
			} else {
				PreDefTableEntry( pdchHeatCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, 0.0 );
			}
			addFootNoteSubTable( pdstHeatCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );
		} else {
			if ( VarSpeedCoil( DXCoilNum ).RatedCapHeat > 0.0 && RatedCapHeatDes > 0.0 ) {
				RatedCapHeatUser = VarSpeedCoil( DXCoilNum ).RatedCapHeat;
				ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Nominal Heating Capacity [W]", RatedCapHeatDes, "User-Specified Nominal Heating Capacity [W]", RatedCapHeatUser );
				if ( DisplayExtraWarnings ) {
					if ( ( std::abs( RatedCapHeatDes - RatedCapHeatUser ) / RatedCapHeatUser ) > AutoVsHardSizingThreshold ) {
						ShowMessage( "SizeVarSpeedCoil: Potential issue with equipment sizing for " + VarSpeedCoil( DXCoilNum ).CoolHeatType + ' ' + CurrentObjSubfix );
						ShowContinueError( "Coil Name =" + VarSpeedCoil( DXCoilNum ).Name );
						ShowContinueError( "User-Specified Rated Total Heating Capacity of " + RoundSigDigits( RatedCapHeatUser, 2 ) + " [W]" );
						ShowContinueError( "differs from Design Size Rated Total Heating Capacity of " + RoundSigDigits( RatedCapHeatDes, 2 ) + " [W]" );
						ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
						ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
					}
				}
			}
		}

		// FORCE BACK TO THE RATED AIR FLOW RATE WITH THE SAME RATIO DEFINED BY THE CATLOG DATA
		if ( ! HardSizeNoDesRunAirFlow ) {
			if ( ( RatedCapCoolTotalAutoSized ) && ( RatedAirFlowAutoSized ) ) {
				RatedAirVolFlowRateDes = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal * VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowPerRatedTotCap( NormSpeed );
			} else if ( ( RatedCapHeatAutoSized ) && ( RatedAirFlowAutoSized ) ) {
				RatedAirVolFlowRateDes = VarSpeedCoil( DXCoilNum ).RatedCapHeat * VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowPerRatedTotCap( NormSpeed );
			}

			// write the air flow sizing output
			if ( RatedAirFlowAutoSized ) {
				VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate = RatedAirVolFlowRateDes;
				ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Air Flow Rate [m3/s]", RatedAirVolFlowRateDes );
			} else {
				if ( VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate > 0.0 && RatedAirVolFlowRateDes > 0.0 ) {
					RatedAirVolFlowRateUser = VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate;
					ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Air Flow Rate [m3/s]", RatedAirVolFlowRateDes, "User-Specified Rated Air Flow Rate [m3/s]", RatedAirVolFlowRateUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( RatedAirVolFlowRateDes - RatedAirVolFlowRateUser ) / RatedAirVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeVarSpeedCoil: Potential issue with equipment sizing for" + VarSpeedCoil( DXCoilNum ).CoolHeatType + ' ' + CurrentObjSubfix );
							ShowContinueError( "Coil Name =" + VarSpeedCoil( DXCoilNum ).Name );
							ShowContinueError( "User-Specified Rated Air Flow Rate of " + RoundSigDigits( RatedAirVolFlowRateUser, 5 ) + " [m3/s]" );
							ShowContinueError( "differs from Design Size Rated Air Flow Rate of " + RoundSigDigits( RatedAirVolFlowRateDes, 5 ) + " [m3/s]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}

		// Check that heat pump heating capacity is within 20% of cooling capacity. Check only for heating coil and report both.
		if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "HEATING" && VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum > 0 ) {

			if ( VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).RatedCapCoolTotal > 0.0 ) {

				if ( std::abs( VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).RatedCapCoolTotal - VarSpeedCoil( DXCoilNum ).RatedCapHeat ) / VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).RatedCapCoolTotal > 0.2 ) {

					ShowWarningError( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" + VarSpeedCoil( DXCoilNum ).Name + "\"" );
					ShowContinueError( "...used with COIL:" + VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).CoolHeatType + ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" + VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).Name + "\"" );
					ShowContinueError( "...heating capacity is disproportionate (> 20% different) to total cooling capacity" );
					ShowContinueError( "...heating capacity = " + TrimSigDigits( VarSpeedCoil( DXCoilNum ).RatedCapHeat, 3 ) + " W" );
					ShowContinueError( "...cooling capacity = " + TrimSigDigits( VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).RatedCapCoolTotal, 3 ) + " W" );
				}
			}
		}

		//ASSIGN CAPACITY
		if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) {
			VarSpeedCoil( DXCoilNum ).MSRatedTotCap( UpperSpeed ) = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal / VarSpeedCoil( DXCoilNum ).MSRatedPercentTotCap( NormSpeed );
		} else if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "HEATING" ) {
			VarSpeedCoil( DXCoilNum ).MSRatedTotCap( UpperSpeed ) = VarSpeedCoil( DXCoilNum ).RatedCapHeat / VarSpeedCoil( DXCoilNum ).MSRatedPercentTotCap( NormSpeed );
		} else if (VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
			VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) = VarSpeedCoil(DXCoilNum).RatedCapWH /
				VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(NormSpeed);
		}

		if ( VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING" ) {
			HPInletAirHumRat = PsyWFnTdbTwbPb(VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp,
				VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp, StdBaroPress, RoutineName);
			rhoA = PsyRhoAirFnPbTdbW(OutBaroPress, VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp,
				HPInletAirHumRat, RoutineName);

			for (Mode = VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
				VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) *
					VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(Mode);
				VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
					VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(Mode);
				VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode)
					* rhoA;
				// EVAPORATIVE PRECOOLING CONDENSER AIR FLOW RATE
				VarSpeedCoil(DXCoilNum).EvapCondAirFlow(Mode) = 0.0;
			}
		} else {
			rhoA = PsyRhoAirFnPbTdbW(OutBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName);
			//HPWH, the mass flow rate will be updated by a revised entering air density
			for (Mode = VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
				VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(UpperSpeed) * VarSpeedCoil(DXCoilNum).MSRatedPercentTotCap(Mode);
				VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) * VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowPerRatedTotCap(Mode);
				VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) * rhoA;
				// EVAPORATIVE PRECOOLING CONDENSER AIR FLOW RATE
				VarSpeedCoil(DXCoilNum).EvapCondAirFlow(Mode) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) * VarSpeedCoil(DXCoilNum).MSRatedEvapCondVolFlowPerRatedTotCap(Mode);
			}
		}

		// size rated power
		if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) {

			VarSpeedCoil( DXCoilNum ).RatedCOPCool = VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel );
			VarSpeedCoil( DXCoilNum ).RatedPowerCool = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal / VarSpeedCoil( DXCoilNum ).RatedCOPCool;

		} else if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "HEATING" ) {
			VarSpeedCoil( DXCoilNum ).RatedCOPHeat = VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel );
			VarSpeedCoil( DXCoilNum ).RatedPowerHeat = VarSpeedCoil( DXCoilNum ).RatedCapHeat / VarSpeedCoil( DXCoilNum ).RatedCOPHeat;
			VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal = VarSpeedCoil( DXCoilNum ).RatedCapHeat;
		} else if (VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
			VarSpeedCoil(DXCoilNum).RatedCOPHeat = VarSpeedCoil(DXCoilNum).MSRatedCOP(VarSpeedCoil(DXCoilNum).NormSpedLevel);
			VarSpeedCoil(DXCoilNum).RatedPowerHeat = VarSpeedCoil(DXCoilNum).RatedCapWH / VarSpeedCoil(DXCoilNum).RatedCOPHeat;
			VarSpeedCoil(DXCoilNum).RatedCapCoolTotal = VarSpeedCoil(DXCoilNum).RatedCapWH *
				(1.0 - 1.0/VarSpeedCoil(DXCoilNum).RatedCOPHeat);
		}

		// Size water volumetric flow rate
		if ( ( VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate == AutoSize ) && ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit || VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) ) {
			RatedWaterFlowAutoSized = true;
		}

		//!   if not found on a plant loop, check condenser loop and warn user if not found
		//    IF(PltSizNum == 0) THEN
		//      PltSizNum = &
		//          MyCondPlantSizingIndex('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)// &
		//               TRIM(CurrentObjSubfix), &
		//                                 VarSpeedCoil(DXCoilNum)%Name, &
		//                                 VarSpeedCoil(DXCoilNum)%WaterInletNodeNum, &
		//                                 VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum, ErrorsFound)
		//    END IF

		//   WSHP condenser can be on either a plant loop or condenser loop. Test each to find plant sizing number.
		//   first check to see if coil is connected to a plant loop, no warning on this CALL
		if ( RatedWaterFlowAutoSized ) {
			if ( VarSpeedCoil( DXCoilNum ).CondenserType == WaterCooled ) PltSizNum = MyPlantSizingIndex( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).WaterInletNodeNum, VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum, ErrorsFound, false );

			if ( PltSizNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, PlantSizData( PltSizNum ).ExitTemp, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineNameAlt );
				cp = GetSpecificHeatGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, PlantSizData( PltSizNum ).ExitTemp, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineNameAlt );

				if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "HEATING" ) {

					RatedWaterVolFlowRateDes = VarSpeedCoil( DXCoilNum ).RatedCapHeat / ( PlantSizData( PltSizNum ).DeltaT * cp * rho );

					//        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//&
					//                                TRIM(CurrentObjSubfix), &
					//                                  VarSpeedCoil(DXCoilNum)%Name, &
					//                                  'Rated Water Flow Rate [m3/s]', VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate)

				} else if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) {

					//       use companion heating coil capacity to calculate volumetric flow rate
					if ( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum > 0 ) {
						SystemCapacity = VarSpeedCoil( VarSpeedCoil( DXCoilNum ).CompanionCoolingCoilNum ).RatedCapHeat;
					} else {
						SystemCapacity = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal;
					}

					RatedWaterVolFlowRateDes = SystemCapacity / ( PlantSizData( PltSizNum ).DeltaT * cp * rho );

					//        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)&
					//                                //TRIM(CurrentObjSubfix), &
					//                                  VarSpeedCoil(DXCoilNum)%Name, &
					//                                  'Rated Water Flow Rate [m3/s]', VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate)
				}
			} else {
				ShowSevereError( "Autosizing of water flow requires a loop Sizing:Plant object" );
				ShowContinueError( "Autosizing also requires physical connection to a plant or condenser loop." );
				ShowContinueError( "Occurs in COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix + " Object=" + VarSpeedCoil( DXCoilNum ).Name );
				ErrorsFound = true;
			}
		}

		//WRITE THE WATER SIZING OUTPUT
		if ( RatedWaterFlowAutoSized ) {
			// FORCE BACK TO THE RATED WATER FLOW RATE WITH THE SAME RATIO DEFINED BY THE CATLOG DATA
			if ( RatedCapCoolTotalAutoSized ) {
				RatedWaterVolFlowRateDes = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal * VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowPerRatedTotCap( NormSpeed );
			} else if ( RatedCapHeatAutoSized ) {
				RatedWaterVolFlowRateDes = VarSpeedCoil( DXCoilNum ).RatedCapHeat * VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowPerRatedTotCap( NormSpeed );
			}
			VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate = RatedWaterVolFlowRateDes;
			ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Water Flow Rate [m3/s]", RatedWaterVolFlowRateDes );
			// Ensure water flow rate at lower speed must be lower or
			// equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
			for ( Mode = 1; Mode <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
				if ( VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( Mode ) > VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( Mode + 1 ) * 1.05 ) {
					ShowWarningError( "SizeDXCoil: " + VarSpeedCoil( DXCoilNum ).VarSpeedCoilType + ' ' + VarSpeedCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Air Flow Rate must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Air Flow Rate." );
					ShowContinueError( "Instead, " + RoundSigDigits( VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ), 2 ) + " > " + RoundSigDigits( VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode + 1 ), 2 ) );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}
		} else {
			if ( VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate > 0.0 && RatedWaterVolFlowRateDes > 0.0 ) {
				RatedWaterVolFlowRateUser = VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate;
				ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Water Flow Rate [m3/s]", RatedWaterVolFlowRateDes, "User-Specified Rated Water Flow Rate [m3/s]", RatedWaterVolFlowRateUser );
				if ( DisplayExtraWarnings ) {
					if ( ( std::abs( RatedWaterVolFlowRateDes - RatedWaterVolFlowRateUser ) / RatedWaterVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
						ShowMessage( "SizeVarSpeedCoil: Potential issue with equipment sizing for" + VarSpeedCoil( DXCoilNum ).CoolHeatType + ' ' + CurrentObjSubfix );
						ShowContinueError( "Coil Name =" + VarSpeedCoil( DXCoilNum ).Name );
						ShowContinueError( "User-Specified Rated Water Flow Rate of " + RoundSigDigits( RatedWaterVolFlowRateUser, 5 ) + " [m3/s]" );
						ShowContinueError( "differs from Design Size Rated Water Flow Rate of " + RoundSigDigits( RatedWaterVolFlowRateDes, 5 ) + " [m3/s]" );
						ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
						ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
					}
				}
			}
		}

		// Save component design water volumetric flow rate.
		if (VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate > 0.0 &&
			VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
			RegisterPlantCompDesignFlow(VarSpeedCoil(DXCoilNum).WaterInletNodeNum, VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate);
		}
		// Use 1/2 flow since both cooling and heating coil will save flow yet only 1 will operate at a time
		else if ( VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate > 0.0 ) {
			RegisterPlantCompDesignFlow( VarSpeedCoil( DXCoilNum ).WaterInletNodeNum, 0.5 * VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate );
		}

		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit || VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit ) {

			RatedSourceTempCool = RatedInletWaterTemp;

			if ( PltSizNum > 0 ) {
				rhoW = rho;
			} else if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit ) {
				rhoW = GetDensityGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, RatedInletWaterTemp, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineName );
			} else {
				rhoW = GetDensityGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, RatedInletWaterTempHeat, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineName );
			}

			VarSpeedCoil( DXCoilNum ).RatedWaterMassFlowRate = VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate * rhoW;
			for ( Mode = VarSpeedCoil( DXCoilNum ).NumOfSpeeds; Mode >= 1; --Mode ) {
				VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( Mode ) = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode ) * VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowPerRatedTotCap( Mode );
				VarSpeedCoil( DXCoilNum ).MSRatedWaterMassFlowRate( Mode ) = VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( Mode ) * rhoW;
			}
		} else if (VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
			RatedSourceTempCool = VarSpeedCoil(DXCoilNum).WHRatedInletWaterTemp;
			rhoW = RhoH2O(RatedSourceTempCool);
			VarSpeedCoil(DXCoilNum).RatedWaterMassFlowRate = VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate * rhoW;
			for (Mode = VarSpeedCoil(DXCoilNum).NumOfSpeeds; Mode >= 1; --Mode) {
				VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
					VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowPerRatedTotCap(Mode);
				VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode) *
					VarSpeedCoil(DXCoilNum).MSWHPumpPowerPerRatedTotCap(Mode);
				VarSpeedCoil(DXCoilNum).MSRatedWaterMassFlowRate(Mode) =
					VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(Mode) * rhoW;
			}
		} else {
			RatedSourceTempCool = RatedAmbAirTemp;
		}

		// Ensure air flow rate at lower speed must be lower or
		// equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
		for ( Mode = 1; Mode <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
			if ( VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) > VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode + 1 ) ) {
				ShowWarningError( "SizeDXCoil: " + VarSpeedCoil( DXCoilNum ).VarSpeedCoilType + ' ' + VarSpeedCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Air Flow Rate must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Air Flow Rate." );
				ShowContinueError( "Instead, " + RoundSigDigits( VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ), 2 ) + " > " + RoundSigDigits( VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode + 1 ), 2 ) );
				ShowFatalError( "Preceding conditions cause termination." );
			}
		}

		// Ensure capacity at lower speed must be lower or equal to the capacity at higher speed.
		for ( Mode = 1; Mode <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
			if ( VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode ) > VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode + 1 ) ) {
				ShowWarningError( "SizeDXCoil: " + VarSpeedCoil( DXCoilNum ).VarSpeedCoilType + ' ' + VarSpeedCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Total Cooling Capacity must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Total Cooling Capacity." );
				ShowContinueError( "Instead, " + RoundSigDigits( VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode ), 2 ) + " > " + RoundSigDigits( VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode + 1 ), 2 ) );
				ShowFatalError( "Preceding conditions cause termination." );
			}
		}

		//convert SHR to rated Bypass factor and effective air side surface area
		if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) {
			for ( Mode = 1; Mode <= VarSpeedCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {
				VarSpeedCoil( DXCoilNum ).MSRatedCBF( Mode ) = CalcCBF( VarSpeedCoil( DXCoilNum ).VarSpeedCoilType, VarSpeedCoil( DXCoilNum ).Name, RatedInletAirTemp, RatedInletAirHumRat, VarSpeedCoil( DXCoilNum ).MSRatedTotCap( Mode ), VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( Mode ), VarSpeedCoil( DXCoilNum ).MSRatedSHR( Mode ) );
				if ( VarSpeedCoil( DXCoilNum ).MSRatedCBF( Mode ) > 0.0 ) {
					VarSpeedCoil( DXCoilNum ).MSEffectiveAo( Mode ) = -std::log( VarSpeedCoil( DXCoilNum ).MSRatedCBF( Mode ) ) * VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( Mode );
				} else {
					VarSpeedCoil( DXCoilNum ).MSEffectiveAo( Mode ) = 0.0;
				}
			}
		} else if (VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
			HPWHInletDBTemp = VarSpeedCoil(DXCoilNum).WHRatedInletDBTemp;
			HPWHInletWBTemp = VarSpeedCoil(DXCoilNum).WHRatedInletWBTemp;

			rhoA = PsyRhoAirFnPbTdbW(StdBaroPress, HPWHInletDBTemp, HPInletAirHumRat, RoutineName);
			for (Mode = 1; Mode <= VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++Mode) {
				VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode)
					* rhoA;
				// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
				//RatedVolFlowPerRatedTotCap = VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(Mode) /
				//	VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode);//prepared for checking ratios, not used here
			}

			for (Mode = 1; Mode <= VarSpeedCoil(DXCoilNum).NumOfSpeeds; ++Mode) {
				//get cooling capacity, without fan power, i.e. total coil cooling
				if (VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP)
				HPWHCoolCapacity = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode)*
					(1.0 - 1.0 / VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) +
					VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) -
					VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
					VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater	;
				else
				HPWHCoolCapacity = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode)*
					(1.0 - 1.0 / VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) -
					VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
					VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;

				VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode) =
					CalcCBF(VarSpeedCoil(DXCoilNum).VarSpeedCoilType, VarSpeedCoil(DXCoilNum).Name,
					HPWHInletDBTemp, HPInletAirHumRat, HPWHCoolCapacity,
					VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode), VarSpeedCoil(DXCoilNum).MSRatedSHR(Mode));
				if (VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode) > 0.0) {
					VarSpeedCoil(DXCoilNum).MSEffectiveAo(Mode) = -std::log(VarSpeedCoil(DXCoilNum).MSRatedCBF(Mode)) *
						VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(Mode);
				} else {
					VarSpeedCoil(DXCoilNum).MSEffectiveAo(Mode) = 0.0;
				}
			}

			//update VarSpeedCoil(DXCoilNum).RatedCapCoolTotal
			Mode = VarSpeedCoil(DXCoilNum).NormSpedLevel;
			if (VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
				VarSpeedCoil(DXCoilNum).RatedCapCoolTotal = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode)*
				(1.0 - 1.0 / VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) +
				VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) -
				VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
				VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;
			} else {
				VarSpeedCoil(DXCoilNum).RatedCapCoolTotal = VarSpeedCoil(DXCoilNum).MSRatedTotCap(Mode)*
				(1.0 - 1.0 / VarSpeedCoil(DXCoilNum).MSRatedCOP(Mode)) -
				VarSpeedCoil(DXCoilNum).MSWHPumpPower(Mode) *
				VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;
			}
		}


		// size rated sensible cooling capacity
		RatedCapCoolSensAutoSized = true; //always do that

		if ( VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate >= SmallAirVolFlow && VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) {
			RatedAirMassFlowRate = VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName );
			RatedInletEnth = PsyHFnTdbW( RatedInletAirTemp, RatedInletAirHumRat );
			CBFRated = AdjustCBF( VarSpeedCoil( DXCoilNum ).MSRatedCBF( NormSpeed ), VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( NormSpeed ), RatedAirMassFlowRate );
			if ( CBFRated > 0.999 ) CBFRated = 0.999;
			AirMassFlowRatio = VarSpeedCoil( DXCoilNum ).RatedAirVolFlowRate / VarSpeedCoil( DXCoilNum ).MSRatedAirVolFlowRate( NormSpeed );

			if ( VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( NormSpeed ) > 1.0e-10 ) {
				WaterMassFlowRatio = VarSpeedCoil( DXCoilNum ).RatedWaterVolFlowRate / VarSpeedCoil( DXCoilNum ).MSRatedWaterVolFlowRate( NormSpeed );
			} else {
				WaterMassFlowRatio = 1.0;
			}

			CalcTotCapSHR_VSWSHP( RatedInletAirTemp, RatedInletAirHumRat, RatedInletEnth, RatedInletWetBulbTemp, AirMassFlowRatio, WaterMassFlowRatio, RatedAirMassFlowRate, CBFRated, VarSpeedCoil( DXCoilNum ).MSRatedTotCap( NormSpeed ), VarSpeedCoil( DXCoilNum ).MSCCapFTemp( NormSpeed ), VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( NormSpeed ), VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( NormSpeed ), 0.0, 0, 0, 0, QLoadTotal1, QLoadTotal2, QLoadTotal, SHR, RatedSourceTempCool, StdBaroPress, 0.0, 1 );

			RatedCapCoolSensDes = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal * SHR;
		} else if (VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate >= SmallAirVolFlow &&
			VarSpeedCoil(DXCoilNum).CoolHeatType == "WATERHEATING") {
		/*	RatedAirMassFlowRate = VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate * PsyRhoAirFnPbTdbW(StdBaroPress,
				HPWHInletDBTemp, HPInletAirHumRat, RoutineName);
			RatedInletEnth = PsyHFnTdbW(HPWHInletDBTemp, HPInletAirHumRat);
			CBFRated = AdjustCBF(VarSpeedCoil(DXCoilNum).MSRatedCBF(NormSpeed),
				VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(NormSpeed), RatedAirMassFlowRate);
			if (CBFRated > 0.999) CBFRated = 0.999;
			AirMassFlowRatio = VarSpeedCoil(DXCoilNum).RatedAirVolFlowRate /
				VarSpeedCoil(DXCoilNum).MSRatedAirVolFlowRate(NormSpeed);

			if (VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(NormSpeed) > 1.0e-10) {
				WaterMassFlowRatio = VarSpeedCoil(DXCoilNum).RatedWaterVolFlowRate /
					VarSpeedCoil(DXCoilNum).MSRatedWaterVolFlowRate(NormSpeed);
			}
			else {
				WaterMassFlowRatio = 1.0;
			}

			CalcTotCapSHR_VSWSHP(HPWHInletDBTemp, HPInletAirHumRat,
				RatedInletEnth, HPWHInletWBTemp,
				AirMassFlowRatio, WaterMassFlowRatio, RatedAirMassFlowRate,
				CBFRated, VarSpeedCoil(DXCoilNum).MSRatedTotCap(NormSpeed),
				VarSpeedCoil(DXCoilNum).MSCCapFTemp(NormSpeed),
				VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(NormSpeed),
				VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(NormSpeed), 0.0, 0, 0, 0,
				QLoadTotal1, QLoadTotal2, QLoadTotal, SHR, RatedSourceTempCool, StdBaroPress, 0.0, 1);*/

			SHR = VarSpeedCoil(DXCoilNum).MSRatedSHR(NormSpeed);
			RatedCapCoolSensDes = VarSpeedCoil(DXCoilNum).RatedCapCoolTotal * SHR;
		} else {
			RatedCapCoolSensDes = 0.0;
		}

		if ( RatedCapCoolSensDes < SmallLoad ) {
			RatedCapCoolSensDes = 0.0;
		}

		if ( VarSpeedCoil( DXCoilNum ).CoolHeatType == "COOLING" ) { //always report for cooling mode
			if ( RatedCapCoolTotalAutoSized ) {
				VarSpeedCoil( DXCoilNum ).RatedCapCoolSens = RatedCapCoolSensDes;
				ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Sensible Cooling Capacity [W]", VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
				//CALL PreDefTableEntry(pdchCoolCoilSensCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
				//CALL PreDefTableEntry(pdchCoolCoilLatCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
				//                           - VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
				//IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal /= 0.0d0) THEN
				//  CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens &
				//                           / VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
				//ELSE
				//  CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,0.0d0)
				//ENDIF
			} else {
				// sensible capacity does not have an input field
				if ( RatedCapCoolSensDes > 0.0 ) {
					VarSpeedCoil( DXCoilNum ).RatedCapCoolSens = RatedCapCoolSensDes;
					ReportSizingOutput( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + CurrentObjSubfix, VarSpeedCoil( DXCoilNum ).Name, "Design Size Rated Sensible Cooling Capacity [W]", RatedCapCoolSensDes ); //, &
					//                            'User-Specified Rated Sensible Cooling Capacity [W]', &
					//                             RatedCapCoolSensUser)
					//       IF (DisplayExtraWarnings) THEN
					//       IF ((ABS(RatedCapCoolSensDes - RatedCapCoolSensUser)/RatedCapCoolSensUser) > AutoVsHardSizingThreshold) THEN
					//          CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for:' &
					//                      //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
					//          CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
					//          CALL ShowContinueError('User-Specified Rated Sensible Cooling Capacity of '// &
					//                                    TRIM(RoundSigDigits(RatedCapCoolSensUser,2))// ' [W]')
					//          CALL ShowContinueError('differs from Design Size Rated Sensible Cooling Capacity of ' // &
					//                                    TRIM(RoundSigDigits(RatedCapCoolSensDes,2))// ' [W]')
					//          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
					//          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
					//        END IF
					//        ENDIF
				}
			}
			PreDefTableEntry( pdchCoolCoilTotCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
			PreDefTableEntry( pdchCoolCoilSensCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
			PreDefTableEntry( pdchCoolCoilLatCap, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal - VarSpeedCoil( DXCoilNum ).RatedCapCoolSens );
			if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal != 0.0 ) {
				PreDefTableEntry( pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).RatedCapCoolSens / VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal );
			} else {
				PreDefTableEntry( pdchCoolCoilSHR, VarSpeedCoil( DXCoilNum ).Name, 0.0 );
			}
			PreDefTableEntry( pdchCoolCoilNomEff, VarSpeedCoil( DXCoilNum ).Name, VarSpeedCoil( DXCoilNum ).MSRatedCOP( VarSpeedCoil( DXCoilNum ).NormSpedLevel ) );
			addFootNoteSubTable( pdstCoolCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );
		}

		// START SIZING EVAP PRECOOLING PUMP POWER
		IsAutoSize = false;
		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
			if ( VarSpeedCoil( DXCoilNum ).EvapCondPumpElecNomPower == AutoSize ) {
				IsAutoSize = true;
			}
			//     Auto size high speed evap condenser pump power to Total Capacity * 0.004266 w/w (15 w/ton)
			EvapCondPumpElecNomPowerDes = VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal * 0.004266;
			if ( IsAutoSize ) {
				VarSpeedCoil( DXCoilNum ).EvapCondPumpElecNomPower = EvapCondPumpElecNomPowerDes;
				ReportSizingOutput( "AS VS COOLING COIL", VarSpeedCoil( DXCoilNum ).Name, "Design Size Evaporative Condenser Pump Rated Power Consumption [W]", EvapCondPumpElecNomPowerDes );
			} else {
				if ( VarSpeedCoil( DXCoilNum ).EvapCondPumpElecNomPower > 0.0 && EvapCondPumpElecNomPowerDes > 0.0 ) {
					EvapCondPumpElecNomPowerUser = VarSpeedCoil( DXCoilNum ).EvapCondPumpElecNomPower;
					ReportSizingOutput( "AS VS COOLING COIL", VarSpeedCoil( DXCoilNum ).Name, "Design Size Evaporative Condenser Pump Rated Power Consumption [W]", EvapCondPumpElecNomPowerDes, "User-Specified Evaporative Condenser Pump Rated Power Consumption [W]", EvapCondPumpElecNomPowerUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( EvapCondPumpElecNomPowerDes - EvapCondPumpElecNomPowerUser ) / EvapCondPumpElecNomPowerUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeVarSpeedCoil: Potential issue with equipment sizing for " + VarSpeedCoil( DXCoilNum ).CoolHeatType + ' ' + CurrentObjSubfix );
							ShowContinueError( "Coil Name =" + VarSpeedCoil( DXCoilNum ).Name );
							ShowContinueError( "User-Specified Evaporative Condenser Pump Rated Power Consumption of " + RoundSigDigits( EvapCondPumpElecNomPowerUser, 2 ) + " [W]" );
							ShowContinueError( "differs from Design Size Evaporative Condenser Pump Rated Power Consumption of " + RoundSigDigits( EvapCondPumpElecNomPowerDes, 2 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}
		// END SIZING EVAP PRE-COOLING PUMP POWER

		//SIZE DEFROST HEATER

		// Resistive Defrost Heater Capacity = capacity at the first stage
		IsAutoSize = false;
		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
			if ( VarSpeedCoil( DXCoilNum ).DefrostCapacity == AutoSize ) {
				IsAutoSize = true;
			}
			if ( VarSpeedCoil( DXCoilNum ).DefrostStrategy == Resistive ) {
				DefrostCapacityDes = VarSpeedCoil( DXCoilNum ).RatedCapHeat;
			} else {
				DefrostCapacityDes = 0.0;
			}
			if ( IsAutoSize ) {
				VarSpeedCoil( DXCoilNum ).DefrostCapacity = DefrostCapacityDes;
				ReportSizingOutput( "AS VS HEATING COIL", VarSpeedCoil( DXCoilNum ).Name, "Design Size Resistive Defrost Heater Capacity [W]", DefrostCapacityDes );
			} else {
				if ( VarSpeedCoil( DXCoilNum ).DefrostCapacity > 0.0 && DefrostCapacityDes > 0.0 && ! HardSizeNoDesRun ) {
					DefrostCapacityUser = VarSpeedCoil( DXCoilNum ).DefrostCapacity;
					ReportSizingOutput( "AS VS HEATING COIL", VarSpeedCoil( DXCoilNum ).Name, "Design Size Resistive Defrost Heater Capacity [W]", DefrostCapacityDes, "User-Specified Resistive Defrost Heater Capacity [W]", DefrostCapacityUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( DefrostCapacityDes - DefrostCapacityUser ) / DefrostCapacityUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeVarSpeedCoil: Potential issue with equipment sizing for " + VarSpeedCoil( DXCoilNum ).CoolHeatType + ' ' + CurrentObjSubfix );
							ShowContinueError( "Coil Name =" + VarSpeedCoil( DXCoilNum ).Name );
							ShowContinueError( "User-Specified Resistive Defrost Heater Capacity of " + RoundSigDigits( DefrostCapacityUser, 2 ) + " [W]" );
							ShowContinueError( "differs from Design Size Resistive Defrost Heater Capacity of " + RoundSigDigits( DefrostCapacityDes, 2 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}
		//END SIZING DEFROST HEATER

		// test autosized sensible and total cooling capacity for total > sensible
		if ( RatedCapCoolSensAutoSized && RatedCapCoolTotalAutoSized ) {
			if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolSens > VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal ) {
				ShowWarningError( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" + VarSpeedCoil( DXCoilNum ).Name + "\"" );
				ShowContinueError( RoutineName + ": Rated Sensible Cooling Capacity > Rated Total Cooling Capacity" );
				ShowContinueError( "Each of these capacity inputs have been autosized." );
				ShowContinueError( "Rated Sensible Cooling Capacity = " + TrimSigDigits( VarSpeedCoil( DXCoilNum ).RatedCapCoolSens, 2 ) + " W" );
				ShowContinueError( "Rated Total Cooling Capacity    = " + TrimSigDigits( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal, 2 ) + " W" );
				ShowContinueError( "See eio file for further details." );
				ShowContinueError( "Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate." );
				ShowContinueError( "Check Zone and System Sizing objects to verify sizing inputs." );
				ShowContinueError( "Sizing statistics:" );
				ShowContinueError( "Entering Air Dry-Bulb Temperature = " + TrimSigDigits( MixTemp, 3 ) + " C" );
				ShowContinueError( "Entering Air Wet-Bulb Temperature = " + TrimSigDigits( MixWetBulb, 3 ) + " C" );
				ShowContinueError( "Entering Condenser Water Temperature used = 24.4444 C" );
				ShowContinueError( "Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)" );
				ShowContinueError( "ratioTDB = " + TrimSigDigits( ( ( MixTemp + 283.15 ) / 273.15 ), 3 ) );
				ShowContinueError( "ratioTWB = " + TrimSigDigits( ( ( MixWetBulb + 283.15 ) / 273.15 ), 3 ) );
				ShowContinueError( "ratioTS  = " + TrimSigDigits( ( ( 85.0 + 283.15 ) / 273.15 ), 3 ) );
				ShowContinueError( "Rated Sensible Cooling Capacity = Rated Total Cooling Capacity * Sensible Heat Ratio" );
				ShowContinueError( "Total Cooling Capacity Modifier = " + TrimSigDigits( TotCapTempModFac, 5 ) );
				ShowContinueError( "...Rated Total Cooling Capacity = Total Design Load / Total Cooling Capacity Modifier" );
				ShowContinueError( "Carefully review the Load Side Total, Sensible, and Latent heat transfer rates" );
				ShowContinueError( "... to ensure they meet the expected manufacturers performance specifications." );
			}
		} else if ( RatedCapCoolTotalAutoSized ) {
			if ( VarSpeedCoil( DXCoilNum ).RatedCapCoolSens > VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal ) {
				ShowWarningError( "COIL:" + VarSpeedCoil( DXCoilNum ).CoolHeatType + ":WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT \"" + VarSpeedCoil( DXCoilNum ).Name + "\"" );
				ShowContinueError( RoutineName + ": Rated Sensible Cooling Capacity > Rated Total Cooling Capacity" );
				ShowContinueError( "Only the rated total capacity input is autosized, consider autosizing both inputs." );
				ShowContinueError( "Rated Sensible Cooling Capacity = " + TrimSigDigits( VarSpeedCoil( DXCoilNum ).RatedCapCoolSens, 2 ) + " W" );
				ShowContinueError( "Rated Total Cooling Capacity    = " + TrimSigDigits( VarSpeedCoil( DXCoilNum ).RatedCapCoolTotal, 2 ) + " W" );
				ShowContinueError( "See eio file for further details." );
				ShowContinueError( "Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate." );
				ShowContinueError( "Check Zone and System Sizing objects to verify sizing inputs." );
				ShowContinueError( "Sizing statistics for Total Cooling Capacity:" );
				ShowContinueError( "Entering Air Wet-Bulb Temperature = " + TrimSigDigits( MixWetBulb, 3 ) + " C" );
				ShowContinueError( "Entering Condenser Water Temperature used = 24.4444 C" );
				ShowContinueError( "Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)" );
				ShowContinueError( "ratioTWB = " + TrimSigDigits( ( ( MixWetBulb + 283.15 ) / 273.15 ), 3 ) );
				ShowContinueError( "ratioTS  = " + TrimSigDigits( ( ( 85.0 + 283.15 ) / 273.15 ), 3 ) );
				ShowContinueError( "Rated Sensible Cooling Capacity = Rated Total Cooling Capacity * Sensible Heat Ratio" );
				ShowContinueError( "Carefully review the Load Side Total, Sensible, and Latent heat transfer rates" );
				ShowContinueError( "... to ensure they meet the expected manufacturers performance specifications." );
			}
		}

	}

	void
	CalcVarSpeedCoilCooling(
		int const DXCoilNum, // Heat Pump Number
		int const CyclingScheme, // Fan/Compressor cycling scheme indicator
		Real64 & RuntimeFrac, // Runtime Fraction of compressor or percent on time (on-time/cycle time)
		Real64 const EP_UNUSED( SensDemand ), // Cooling Sensible Demand [W] !unused1208
		Real64 const EP_UNUSED( LatentDemand ), // Cooling Latent Demand [W]
		int const CompOp, // compressor operation flag
		Real64 const PartLoadRatio, // compressor part load ratio
		Real64 const EP_UNUSED( OnOffAirFlowRatio ), // ratio of compressor on flow to average flow over time step
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
		int const SpeedNum // Speed number, high bound
	)
	{

		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:CalcHPCoolingSimple
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for simulating the cooling mode of the Variable-Speed Water to Air HP Simple

		// METHODOLOGY EMPLOYED:
		// Simulate the heat pump performance using the coefficients and rated conditions, interpolating between speed levels
		// If the LatDegradModelSimFlag is enabled, the coil will be simulated twice:
		// (1)first simulation at the rated conditions (2) second simulation at the
		// actual operating conditions. Then call CalcEffectiveSHR and the effective SHR
		// is adjusted.
		// If the LatDegradModelSimFlag is disabled, the cooling coil is only simulated
		// once at the actual operating conditions.
		// Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
		// and RuntimeFrac.

		// REFERENCES:
		// n/a

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::DXElecCoolingPower;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyWFnTdbH;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMultiSpeedVarSpeedCoilCooling" );
		static std::string const RoutineNameSourceSideInletTemp( "CalcVSHPCoolingSimple:SourceSideInletTemp" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 Twet_Rated; // Twet at rated conditions (coil air flow rate and air temperatures), sec
		Real64 Gamma_Rated; // Gamma at rated conditions (coil air flow rate and air temperatures)

		Real64 SHRss; // Sensible heat ratio at steady state
		Real64 SHReff; // Effective sensible heat ratio at part-load condition
		Real64 CpSource; // Specific heat of water [J/kg_C]
		Real64 CpAir; // Specific heat of air [J/kg_C]
		Real64 ReportingConstant;

		bool LatDegradModelSimFlag; // Latent degradation model simulation flag
		int NumIteration; // Iteration Counter
		static int Count( 0 ); // No idea what this is for.
		static bool firstTime( true );
		static Real64 LoadSideInletDBTemp_Init; // rated conditions
		static Real64 LoadSideInletWBTemp_Init; // rated conditions
		static Real64 LoadSideInletHumRat_Init; // rated conditions
		static Real64 LoadSideInletEnth_Init; // rated conditions
		static Real64 CpAir_Init; // rated conditions
		Real64 LoadSideInletDBTemp_Unit; // calc conditions for unit
		Real64 LoadSideInletWBTemp_Unit; // calc conditions for unit
		Real64 LoadSideInletHumRat_Unit; // calc conditions for unit
		Real64 LoadSideInletEnth_Unit; // calc conditions for unit
		Real64 CpAir_Unit; // calc conditions for unit
		Real64 AirMassFlowRatio; // airflow ratio at low speed
		Real64 WaterMassFlowRatio; // airflow ratio at high speed
		Real64 EIRAirFFModFac; // air flow fraction modification
		Real64 EIRWaterFFModFac; // water flow fraction modification
		Real64 EIRTempModFac; // total capacity temperature correctio fraction
		Real64 CBFSpeed; // total capacity temperature correctio fraction
		Real64 SHR; // total capacity temperature correctio fraction
		Real64 EIR; // total capacity temperature correctio fraction
		int MaxSpeed; // maximum speed level
		int SpeedCal; // calculated speed level
		Real64 AoEff; // effective air side surface area
		Real64 QLoadTotal1; // total capacity at low speed
		Real64 QLoadTotal2; // total capacity at high speed
		Real64 Winput1; // power consumption at low speed
		Real64 Winput2; // power consumption at high speed
		Real64 QWasteHeat; // recoverable waste heat
		Real64 QWasteHeat1; // recoverable waste heat at low speed
		Real64 QWasteHeat2; // recoverable waste heat at high speed
		Real64 PLF; // part-load function
		Real64 MaxHumRat; // max possible humidity
		Real64 MaxOutletEnth; // max possible outlet enthalpy

		// ADDED VARIABLES FOR air source coil
		static Real64 OutdoorDryBulb( 0.0 ); // Outdoor dry-bulb temperature at condenser (C)
		static Real64 OutdoorWetBulb( 0.0 ); // Outdoor wet-bulb temperature at condenser (C)
		static Real64 OutdoorHumRat( 0.0 ); // Outdoor humidity ratio at condenser (kg/kg)
		static Real64 OutdoorPressure( 0.0 ); // Outdoor barometric pressure at condenser (Pa)
		static Real64 CrankcaseHeatingPower( 0.0 ); // power due to crankcase heater
		static Real64 CompAmbTemp( 0.0 ); // Ambient temperature at compressor
		Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
		// Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
		Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
		// For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
		Real64 CondAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 RhoSourceAir; // Density of air [kg/m3]
		Real64 RhoEvapCondWater; // Density of water used for evaporative condenser [kg/m3]
		Real64 EvapCondEffectSped; // condenser evaporative effectiveness at the speed level
		Real64 RhoWater; // condensed water density
		Real64 SpecHumIn; // inlet air specific humidity
		Real64 SpecHumOut; // outlet air specific humidity
		Real64 rhoair(0);//entering air density

		if ( firstTime ) {
			//Set indoor air conditions to the rated condition
			LoadSideInletDBTemp_Init = 26.7;
			LoadSideInletHumRat_Init = 0.0111;
			LoadSideInletEnth_Init = PsyHFnTdbW( LoadSideInletDBTemp_Init, LoadSideInletHumRat_Init );
			CpAir_Init = PsyCpAirFnWTdb( LoadSideInletHumRat_Init, LoadSideInletDBTemp_Init );
			firstTime = false;
		}
		LoadSideInletWBTemp_Init = PsyTwbFnTdbWPb( LoadSideInletDBTemp_Init, LoadSideInletHumRat_Init, OutBaroPress, RoutineName );

		MaxSpeed = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;

		// must be placed inside the loop, otherwise cause bug in release mode, need to be present at two places
		if ( SpeedNum > MaxSpeed ) {
			SpeedCal = MaxSpeed;
		} else {
			SpeedCal = SpeedNum;
		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		if ( ! ( CyclingScheme == ContFanCycCoil ) && PartLoadRatio > 0.0 ) {
			VarSpeedCoil( DXCoilNum ).AirMassFlowRate = Node( VarSpeedCoil( DXCoilNum ).AirInletNodeNum ).MassFlowRate / PartLoadRatio;
		}

		Twet_Rated = VarSpeedCoil( DXCoilNum ).Twet_Rated;
		Gamma_Rated = VarSpeedCoil( DXCoilNum ).Gamma_Rated;

		LoadSideMassFlowRate = VarSpeedCoil( DXCoilNum ).AirMassFlowRate;

		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
			// Get condenser outdoor node info from DX COOLING Coil
			if ( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum != 0 ) {
				OutdoorDryBulb = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).Temp;
				OutdoorHumRat = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).HumRat;
				OutdoorPressure = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).Press;
				OutdoorWetBulb = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).OutAirWetBulb;
			} else {
				OutdoorDryBulb = OutDryBulbTemp;
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			}

			RhoSourceAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat );

			if ( ( SpeedNum == 1 ) || ( SpeedNum > MaxSpeed ) || ( SpeedRatio == 1.0 ) ) {
				CondAirMassFlow = RhoSourceAir * VarSpeedCoil( DXCoilNum ).EvapCondAirFlow( SpeedCal );
			} else {
				CondAirMassFlow = RhoSourceAir * ( VarSpeedCoil( DXCoilNum ).EvapCondAirFlow( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).EvapCondAirFlow( SpeedCal - 1 ) );
			}

			// AIR COOL OR EVAP COOLED CONDENSER
			if ( VarSpeedCoil( DXCoilNum ).CondenserType == EvapCooled ) {
				if ( ( SpeedNum == 1 ) || ( SpeedNum > MaxSpeed ) || ( SpeedRatio == 1.0 ) ) {
					EvapCondEffectSped = VarSpeedCoil( DXCoilNum ).EvapCondEffect( SpeedCal );
				} else {
					EvapCondEffectSped = VarSpeedCoil( DXCoilNum ).EvapCondEffect( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).EvapCondEffect( SpeedCal - 1 );
				}
				// (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
				CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - EvapCondEffectSped );
				CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure );
				CompAmbTemp = CondInletTemp;
			} else { //AIR COOLED CONDENSER
				CondInletTemp = OutdoorDryBulb; // Outdoor dry-bulb temp
				CompAmbTemp = OutdoorDryBulb;
				CondInletHumRat = OutHumRat;
			}

			SourceSideMassFlowRate = CondAirMassFlow;
			SourceSideInletTemp = CondInletTemp;
			SourceSideInletEnth = PsyHFnTdbW( CondInletTemp, CondInletHumRat );
			CpSource = PsyCpAirFnWTdb( CondInletHumRat, CondInletTemp );
			VarSpeedCoil( DXCoilNum ).CondInletTemp = CondInletTemp;

			// If used in a heat pump, the value of MaxOAT in the heating coil overrides that in the cooling coil (in GetInput)
			// Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
			if ( OutdoorDryBulb < VarSpeedCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
				CrankcaseHeatingPower = VarSpeedCoil( DXCoilNum ).CrankcaseHeaterCapacity;
			} else {
				CrankcaseHeatingPower = 0.0;
			}
		} else {
			SourceSideMassFlowRate = VarSpeedCoil( DXCoilNum ).WaterMassFlowRate;
			SourceSideInletTemp = VarSpeedCoil( DXCoilNum ).InletWaterTemp;
			SourceSideInletEnth = VarSpeedCoil( DXCoilNum ).InletWaterEnthalpy;
			CpSource = GetSpecificHeatGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, SourceSideInletTemp, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineNameSourceSideInletTemp );
		}

		//Check for flows, do not perform simulation if no flow in load side or source side.
		if ( SourceSideMassFlowRate <= 0.0 || LoadSideMassFlowRate <= 0.0 ) {

			if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) && ( VarSpeedCoil( DXCoilNum ).CondenserType == AirCooled ) && ( LoadSideMassFlowRate > 0.0 ) ) {
				//ALLOW SIMULATION IF AIR-COOLED CONDENSER COIL
				VarSpeedCoil( DXCoilNum ).SimFlag = true;
			} else {
				VarSpeedCoil( DXCoilNum ).SimFlag = false;
				return;
			}
		} else {
			VarSpeedCoil( DXCoilNum ).SimFlag = true;
		}

		if ( CompOp == 0 ) {
			VarSpeedCoil( DXCoilNum ).SimFlag = false;
			return;
		}

		//Loop the calculation at least once depending whether the latent degradation model
		//is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
		//and 2nd iteration to calculate the  QLatent(actual)
		if ( ( PartLoadRatio < 1e-10 ) || ( Twet_Rated <= 0.0 ) || ( Gamma_Rated <= 0.0 ) || ( SpeedNum > 1.0 ) ) {
			LatDegradModelSimFlag = false;
			//Set NumIteration=1 so that latent model would quit after 1 simulation with the actual condition
			NumIteration = 1;
		} else {
			LatDegradModelSimFlag = true;
			//Set NumIteration=0 so that latent model would simulate twice with rated and actual condition
			NumIteration = 0;
		}

		//Set indoor air conditions to the actual condition
		LoadSideInletDBTemp_Unit = VarSpeedCoil( DXCoilNum ).InletAirDBTemp;
		LoadSideInletHumRat_Unit = VarSpeedCoil( DXCoilNum ).InletAirHumRat;
		LoadSideInletWBTemp_Unit = PsyTwbFnTdbWPb( LoadSideInletDBTemp_Unit, LoadSideInletHumRat_Unit, OutBaroPress, RoutineName );
		LoadSideInletEnth_Unit = VarSpeedCoil( DXCoilNum ).InletAirEnthalpy;
		CpAir_Unit = PsyCpAirFnWTdb( LoadSideInletHumRat_Unit, LoadSideInletDBTemp_Unit );

		RuntimeFrac = 1.0;
		VarSpeedCoil( DXCoilNum ).RunFrac = 1.0;
		if ( ( SpeedNum == 1 ) && ( PartLoadRatio < 1.0 ) ) {
			PLF = CurveValue( VarSpeedCoil( DXCoilNum ).PLFFPLR, PartLoadRatio );
			if ( PLF < 0.7 ) {
				PLF = 0.7;
			}
			// calculate the run time fraction
			VarSpeedCoil( DXCoilNum ).RunFrac = PartLoadRatio / PLF;
			VarSpeedCoil( DXCoilNum ).PartLoadRatio = PartLoadRatio;

			if ( VarSpeedCoil( DXCoilNum ).RunFrac > 1.0 ) {
				VarSpeedCoil( DXCoilNum ).RunFrac = 1.0; // Reset coil runtime fraction to 1.0
			} else if ( VarSpeedCoil( DXCoilNum ).RunFrac < 0.0 ) {
				VarSpeedCoil( DXCoilNum ).RunFrac = 0.0;
			}

			RuntimeFrac = VarSpeedCoil( DXCoilNum ).RunFrac;
		}

		while ( true ) {
			++NumIteration;
			if ( NumIteration == 1 ) {
				//Set indoor air conditions to the rated conditions
				LoadSideInletDBTemp = LoadSideInletDBTemp_Init;
				LoadSideInletHumRat = LoadSideInletHumRat_Init;
				LoadSideInletWBTemp = LoadSideInletWBTemp_Init;
				LoadSideInletEnth = LoadSideInletEnth_Init;
				CpAir = CpAir_Init;
			} else {
				//Set indoor air conditions to the actual condition
				LoadSideInletDBTemp = LoadSideInletDBTemp_Unit;
				LoadSideInletHumRat = LoadSideInletHumRat_Unit;
				LoadSideInletWBTemp = LoadSideInletWBTemp_Unit;
				LoadSideInletEnth = LoadSideInletEnth_Unit;
				CpAir = CpAir_Unit;
			}

			// must be placed inside the loop, otherwise cause bug in release mode
			if ( SpeedNum > MaxSpeed ) {
				SpeedCal = MaxSpeed;
			} else {
				SpeedCal = SpeedNum;
			}

			if ( ( SpeedNum == 1 ) || ( SpeedNum > MaxSpeed ) || ( SpeedRatio == 1.0 ) ) {
				AirMassFlowRatio = LoadSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignAirMassFlowRate;

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					WaterMassFlowRatio = 1.0;
				} else {
					WaterMassFlowRatio = SourceSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate;
				}

				EIRTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( SpeedCal ), LoadSideInletWBTemp, SourceSideInletTemp );
				EIRAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( SpeedCal ), AirMassFlowRatio );

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					EIRWaterFFModFac = 1.0;
				} else {
					EIRWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( SpeedCal ), WaterMassFlowRatio );
				}

				EIR = ( 1.0 / VarSpeedCoil( DXCoilNum ).MSRatedCOP( SpeedCal ) ) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac;

				CBFSpeed = AdjustCBF( VarSpeedCoil( DXCoilNum ).MSRatedCBF( SpeedCal ), VarSpeedCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedCal ), LoadSideMassFlowRate );

				if ( CBFSpeed > 0.999 ) CBFSpeed = 0.999;

				CalcTotCapSHR_VSWSHP( LoadSideInletDBTemp, LoadSideInletHumRat, LoadSideInletEnth, LoadSideInletWBTemp, AirMassFlowRatio, WaterMassFlowRatio, LoadSideMassFlowRate, CBFSpeed, VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal ), VarSpeedCoil( DXCoilNum ).MSCCapFTemp( SpeedCal ), VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( SpeedCal ), VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( SpeedCal ), 0.0, 0, 0, 0, QLoadTotal1, QLoadTotal2, QLoadTotal, SHR, SourceSideInletTemp, VarSpeedCoil( DXCoilNum ).InletAirPressure, 0.0, 1 );

				Winput = QLoadTotal * EIR;

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					QWasteHeat = 0.0;
				} else {
					QWasteHeat = Winput * VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( SpeedCal );
					QWasteHeat *= CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( SpeedCal ), LoadSideInletWBTemp, SourceSideInletTemp );
				}
			} else {
				AirMassFlowRatio = LoadSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignAirMassFlowRate;

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					WaterMassFlowRatio = 1.0;
				} else {
					WaterMassFlowRatio = SourceSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate;
				}

				AoEff = VarSpeedCoil( DXCoilNum ).MSEffectiveAo( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).MSEffectiveAo( SpeedCal - 1 );

				CBFSpeed = std::exp( -AoEff / LoadSideMassFlowRate );

				if ( CBFSpeed > 0.999 ) CBFSpeed = 0.999;

				CalcTotCapSHR_VSWSHP( LoadSideInletDBTemp, LoadSideInletHumRat, LoadSideInletEnth, LoadSideInletWBTemp, AirMassFlowRatio, WaterMassFlowRatio, LoadSideMassFlowRate, CBFSpeed, VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal - 1 ), VarSpeedCoil( DXCoilNum ).MSCCapFTemp( SpeedCal - 1 ), VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( SpeedCal - 1 ), VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( SpeedCal - 1 ), VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal ), VarSpeedCoil( DXCoilNum ).MSCCapFTemp( SpeedCal ), VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( SpeedCal ), VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( SpeedCal ), QLoadTotal1, QLoadTotal2, QLoadTotal, SHR, SourceSideInletTemp, VarSpeedCoil( DXCoilNum ).InletAirPressure, SpeedRatio, 2 );

				SpeedCal = SpeedNum - 1;
				EIRTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( SpeedCal ), LoadSideInletWBTemp, SourceSideInletTemp );
				EIRAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( SpeedCal ), AirMassFlowRatio );

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					EIRWaterFFModFac = 1.0;
				} else {
					EIRWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( SpeedCal ), WaterMassFlowRatio );
				}

				EIR = ( 1.0 / VarSpeedCoil( DXCoilNum ).MSRatedCOP( SpeedCal ) ) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac;
				Winput1 = QLoadTotal1 * EIR;

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					QWasteHeat1 = 0.0;
				} else {
					QWasteHeat1 = Winput1 * VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( SpeedCal );
					QWasteHeat1 *= CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( SpeedCal ), LoadSideInletWBTemp, SourceSideInletTemp );
				}

				SpeedCal = SpeedNum;
				EIRTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( SpeedCal ), LoadSideInletWBTemp, SourceSideInletTemp );
				EIRAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( SpeedCal ), AirMassFlowRatio );

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					EIRWaterFFModFac = 1.0;
				} else {
					EIRWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( SpeedCal ), WaterMassFlowRatio );
				}

				EIR = ( 1.0 / VarSpeedCoil( DXCoilNum ).MSRatedCOP( SpeedCal ) ) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac;
				Winput2 = QLoadTotal2 * EIR;

				if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
					QWasteHeat2 = 0.0;
				} else {
					QWasteHeat2 = Winput2 * VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( SpeedCal );
					QWasteHeat2 *= CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( SpeedCal ), LoadSideInletWBTemp, SourceSideInletTemp );
				}

				Winput = Winput2 * SpeedRatio + ( 1.0 - SpeedRatio ) * Winput1;
				QWasteHeat = QWasteHeat2 * SpeedRatio + ( 1.0 - SpeedRatio ) * QWasteHeat1;
			}

			QSensible = QLoadTotal * SHR;

			QSource = QLoadTotal + Winput - QWasteHeat;

			if ( QSource < 0 ) {
				QSource = 0.0;
				QWasteHeat = QLoadTotal + Winput;
			}

			//Check if the Sensible Load is greater than the Total Cooling Load
			if ( QSensible > QLoadTotal ) {
				QSensible = QLoadTotal;
			}

			if ( LatDegradModelSimFlag ) {
				//Calculate for SHReff using the Latent Degradation Model
				if ( NumIteration == 1 ) {
					QLatRated = QLoadTotal - QSensible;
				} else if ( NumIteration == 2 ) {
					QLatActual = QLoadTotal - QSensible;
					SHRss = QSensible / QLoadTotal;
					SHReff = CalcEffectiveSHR( DXCoilNum, SHRss, CyclingScheme, RuntimeFrac, QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp );
					//       Update sensible capacity based on effective SHR
					QSensible = QLoadTotal * SHReff;
					goto LOOP_exit;
				}
			} else {
				//Assume SHReff=SHRss
				SHReff = QSensible / QLoadTotal;
				goto LOOP_exit;
			}
		}
		LOOP_exit: ;

		// considering hot gas reheat here
		if ( VarSpeedCoil( DXCoilNum ).HOTGASREHEATFLG > 0 ) {
			QLoadTotal -= QWasteHeat;
			QSensible -= QWasteHeat;
			SHReff = QSensible / QLoadTotal;
		}

		VarSpeedCoil( DXCoilNum ).BasinHeaterPower = 0.0;
		VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower = 0.0;

		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
			if ( VarSpeedCoil( DXCoilNum ).CondenserType == EvapCooled ) {
				//******************
				//             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
				//             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
				//                                /RhoWater [kg H2O/m3 H2O]
				//******************
				RhoEvapCondWater = RhoH2O( OutdoorDryBulb );
				VarSpeedCoil( DXCoilNum ).EvapWaterConsumpRate = ( CondInletHumRat - OutdoorHumRat ) * CondAirMassFlow / RhoEvapCondWater * RuntimeFrac;
				VarSpeedCoil( DXCoilNum ).EvapCondPumpElecPower = VarSpeedCoil( DXCoilNum ).EvapCondPumpElecNomPower * RuntimeFrac;
				// Calculate basin heater power
				CalcBasinHeaterPower( VarSpeedCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, VarSpeedCoil( DXCoilNum ).BasinHeaterSchedulePtr, VarSpeedCoil( DXCoilNum ).BasinHeaterSetPointTemp, VarSpeedCoil( DXCoilNum ).BasinHeaterPower );
				VarSpeedCoil( DXCoilNum ).BasinHeaterPower *= ( 1.0 - RuntimeFrac );
			}

			VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - RuntimeFrac );

			//set water system demand request (if needed)
			if ( VarSpeedCoil( DXCoilNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
				WaterStorage( VarSpeedCoil( DXCoilNum ).EvapWaterSupTankID ).VdotRequestDemand( VarSpeedCoil( DXCoilNum ).EvapWaterTankDemandARRID ) = VarSpeedCoil( DXCoilNum ).EvapWaterConsumpRate;
			}

		}

		//calculate coil outlet state variables
		LoadSideOutletEnth = LoadSideInletEnth - QLoadTotal / LoadSideMassFlowRate;
		LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible / ( LoadSideMassFlowRate * CpAir );

		MaxHumRat = PsyWFnTdbRhPb( LoadSideOutletDBTemp, 0.9999, VarSpeedCoil( DXCoilNum ).InletAirPressure, RoutineName );
		MaxOutletEnth = PsyHFnTdbW( LoadSideOutletDBTemp, MaxHumRat );
		if ( LoadSideOutletEnth > MaxOutletEnth ) {
			LoadSideOutletEnth = MaxOutletEnth;
			//QLoadTotal = LoadSideMassFlowRate * (LoadSideInletEnth - LoadSideOutletEnth)
		}
		LoadSideOutletHumRat = PsyWFnTdbH( LoadSideOutletDBTemp, LoadSideOutletEnth, RoutineName );
		if ( LoadSideOutletHumRat > MaxHumRat ) {
			LoadSideOutletHumRat = MaxHumRat;
		}

		++Count;
		//Actual outlet conditions are "average" for time step
		if ( CyclingScheme == ContFanCycCoil ) {
			// continuous fan, cycling compressor
			VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy = PartLoadRatio * LoadSideOutletEnth + ( 1.0 - PartLoadRatio ) * LoadSideInletEnth;
			VarSpeedCoil( DXCoilNum ).OutletAirHumRat = PartLoadRatio * LoadSideOutletHumRat + ( 1.0 - PartLoadRatio ) * LoadSideInletHumRat;
			VarSpeedCoil( DXCoilNum ).OutletAirDBTemp = PsyTdbFnHW( VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy, VarSpeedCoil( DXCoilNum ).OutletAirHumRat );
			PLRCorrLoadSideMdot = LoadSideMassFlowRate;
		} else {
			// default to cycling fan, cycling compressor
			VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy = LoadSideOutletEnth;
			VarSpeedCoil( DXCoilNum ).OutletAirHumRat = LoadSideOutletHumRat;
			VarSpeedCoil( DXCoilNum ).OutletAirDBTemp = LoadSideOutletDBTemp;
			PLRCorrLoadSideMdot = LoadSideMassFlowRate * PartLoadRatio;
		}

		// scale heat transfer rates to PLR and power to RTF
		QLoadTotal *= PartLoadRatio;
		QSensible *= PartLoadRatio;
		// count the powr separately
		Winput *= RuntimeFrac; //+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower &
		//+ VarSpeedCoil(DXCoilNum)%BasinHeaterPower + VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower
		QSource *= PartLoadRatio;
		QWasteHeat *= PartLoadRatio;

		//  Add power to global variable so power can be summed by parent object
		DXElecCoolingPower = Winput;

		ReportingConstant = TimeStepSys * SecInHour;
		//Update heat pump data structure
		VarSpeedCoil( DXCoilNum ).Power = Winput;
		VarSpeedCoil( DXCoilNum ).QLoadTotal = QLoadTotal;
		VarSpeedCoil( DXCoilNum ).QSensible = QSensible;
		VarSpeedCoil( DXCoilNum ).QLatent = QLoadTotal - QSensible;
		VarSpeedCoil( DXCoilNum ).QSource = QSource;
		VarSpeedCoil( DXCoilNum ).Energy = Winput * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergyLoadTotal = QLoadTotal * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergySensible = QSensible * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergyLatent = ( QLoadTotal - QSensible ) * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergySource = QSource * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).CrankcaseHeaterConsumption = VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EvapWaterConsump = VarSpeedCoil( DXCoilNum ).EvapWaterConsumpRate * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).BasinHeaterConsumption = VarSpeedCoil( DXCoilNum ).BasinHeaterPower * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EvapCondPumpElecConsumption = VarSpeedCoil( DXCoilNum ).EvapCondPumpElecPower * ReportingConstant;
		if ( RuntimeFrac == 0.0 ) {
			VarSpeedCoil( DXCoilNum ).COP = 0.0;
		} else {
			VarSpeedCoil( DXCoilNum ).COP = QLoadTotal / Winput;
		}
		VarSpeedCoil( DXCoilNum ).RunFrac = RuntimeFrac;
		VarSpeedCoil( DXCoilNum ).PartLoadRatio = PartLoadRatio;
		VarSpeedCoil( DXCoilNum ).AirMassFlowRate = PLRCorrLoadSideMdot;
		rhoair = PsyRhoAirFnPbTdbW(OutBaroPress, LoadSideInletDBTemp, LoadSideInletHumRat, RoutineName);
		VarSpeedCoil(DXCoilNum).AirVolFlowRate = VarSpeedCoil(DXCoilNum).AirMassFlowRate / rhoair; 

		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_CoolingAirToAirVariableSpeed ) {
			VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletWaterTemp = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy = 0.0;
		} else {
			VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = SourceSideMassFlowRate;
			VarSpeedCoil( DXCoilNum ).OutletWaterTemp = SourceSideInletTemp + QSource / ( SourceSideMassFlowRate * CpSource );
			VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy = SourceSideInletEnth + QSource / SourceSideMassFlowRate;
		}

		VarSpeedCoil( DXCoilNum ).QWasteHeat = QWasteHeat;

		if ( VarSpeedCoil( DXCoilNum ).CondensateCollectMode == CondensateToTank ) {
			// calculate and report condensation rates  (how much water extracted from the air stream)
			// water flow of water in m3/s for water system interactions
			RhoWater = RhoH2O( ( VarSpeedCoil( DXCoilNum ).InletAirDBTemp + VarSpeedCoil( DXCoilNum ).OutletAirDBTemp ) / 2.0 );
			//     CR9155 Remove specific humidity calculations
			SpecHumIn = LoadSideInletHumRat;
			SpecHumOut = LoadSideOutletHumRat;
			//  mdot * del HumRat / rho water
			VarSpeedCoil( DXCoilNum ).CondensateVdot = max( 0.0, ( LoadSideMassFlowRate * ( SpecHumIn - SpecHumOut ) / RhoWater ) );
			VarSpeedCoil( DXCoilNum ).CondensateVol = VarSpeedCoil( DXCoilNum ).CondensateVdot * ReportingConstant;
		}

	}

	void
	CalcVarSpeedHPWH(
		int const DXCoilNum, // the number of the DX coil to be simulated
		Real64 & RuntimeFrac, // Runtime Fraction of compressor or percent on time (on-time/cycle time)
		Real64 const PartLoadRatio, // sensible water heating load / full load sensible water heating capacity
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
		int const SpeedNum, // Speed number, high bound
		int const CyclingScheme // Continuous fan OR cycling compressor
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, ORNL
		//       DATE WRITTEN   12/2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the gross cooling capacity of a varaible-speed heat pump water heater evaporator and
		// heating capacity of the condenser coil given the rated heating capacity and COP.

		// METHODOLOGY EMPLOYED:
		// The routine requires the user to enter the total heating capacity and COP for the
		// heat pump water heater along with logicals defining if fan and condenser pump are included at numerous speed levels.
		// Since manufacturer's can rate their HPWH equipment with or without including condenser
		// pump heat, this information is required to accurately determine the condenser's leaving
		// water temperature. In addition, knowledge of the fan heat is required to back into
		// a compressor COP.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using DataHVACGlobals::FanElecPower;
		using DataHVACGlobals::HPWHInletDBTemp;
		using DataHVACGlobals::HPWHInletWBTemp;
		using DataHVACGlobals::DXCoilTotalCapacity;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName("CalcVarSpeedHPWH");

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OperatingHeatingCapacity; // Water heating operating capacity including the impact of capacity and COP curves (W)
		Real64 OperatingHeatingCOP; // Water heating operating COP including the impact of capacity and COP curves (W/W)
		Real64 OperatingHeatingPower; // Water heating operating Power (W)
		Real64 CompressorPower; // Power consumed by compressor only (W)

		Real64 TotalTankHeatingCapacity; // Water heating capacity corrected for condenser water pump heat (W)
		Real64 TankHeatingCOP; // Water heating COP corrected for fan and condenser water pump power (W/W)
		// (these previous 2 variables also include the impact of capacity and COP curves)
		Real64 EvapCoolingCapacity; // Air cooling capacity corrected for evap fan and cond water pump heat (W)
		Real64 InletWaterTemp; // Condenser water inlet temperature (C)
		Real64 OutletWaterTemp; // Condenser water outlet temperature (C)
		Real64 EvapInletMassFlowRate; // Evaporator air inlet mass flow rate (m3/s)
		Real64 CondInletMassFlowRate; // Condenser water inlet mass flow rate (m3/s)
		Real64 CpWater; // Specific heat of condenser inlet water (J/Kg/k)
		Real64 InletAirTemp; // HPWH inlet air temperature (dry-bulb or wet-bulb) (C)
		Real64 AirMassFlowRatio; // Ratio of evaporator inlet air mass flow rate to rated mass flow rate
		Real64 WaterMassFlowRatio; // Ratio of evaporator inlet water mass flow rate to rated mass flow rate
		Real64 PumpHeatToWater; // Amount of pump heat attributed to heating water
		Real64 HPRTF; // Heat pump run time fraction
		Real64 PLF; // part-load function
		Real64 CBFSpeed; // bypass factor as individual speed level
		Real64 COPAirFFModFac; // air flow fraction modification
		Real64 COPWaterFFModFac; // water flow fraction modification
		Real64 COPTempModFac; // total capacity temperature correctio fraction
		Real64 TOTCAPAirFFModFac; // air flow fraction modification
		Real64 TOTCAPWaterFFModFac; // water flow fraction modification
		Real64 TOTCAPTempModFac; // total capacity temperature correctio fraction
		Real64 SHR; // total capacity temperature correctio fraction
		Real64 COP; // total capacity temperature correctio fraction
		Real64 AoEff; // effective air side surface area
		Real64 Winput1; // power consumption at low speed
		Real64 Winput2; // power consumption at high speed
		Real64 LoadPressure; // evaporator inlet pressure
		Real64 CrankcaseHeatingPower; // power due to crankcase heater
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 WHCAP1; // total heating capacity at low speed [W]
		Real64 WHCAP2; // total heating capacity at high speed [W]
		Real64 CpAir; // Specific heat of air [J/kg_C]
		Real64 MaxHumRat; // max possible humidity
		Real64 MaxOutletEnth; // max possible outlet enthalpy
		Real64 ReportingConstant;
		int EvapInletNode; // Evaporator air inlet node number
		int EvapOutletNode; // Evaporator air outlet node number
		int CondInletNode; // Condenser water inlet node number
		int CondOutletNode; // Condenser water outlet node number
		int MaxSpeed; // maximum speed level
		int SpeedCal; // calculated speed level
		Real64 rhoair(0.0);//entering air density
		Real64 RhoWater(0.0);//water density

		//note: load side is the evaporator side, and source side is the condenser side

		CondInletNode = VarSpeedCoil(DXCoilNum).WaterInletNodeNum;
		CondOutletNode = VarSpeedCoil(DXCoilNum).WaterOutletNodeNum;
		// If heat pump water heater is OFF, set outlet to inlet and RETURN
		if (PartLoadRatio == 0.0) {
			Node(CondOutletNode) = Node(CondInletNode);
			return;
		} else {
			EvapInletNode = VarSpeedCoil(DXCoilNum).AirInletNodeNum;
			EvapOutletNode = VarSpeedCoil(DXCoilNum).AirOutletNodeNum;
			InletWaterTemp = Node(CondInletNode).Temp;
			CondInletMassFlowRate = Node(CondInletNode).MassFlowRate;
			EvapInletMassFlowRate = Node(EvapInletNode).MassFlowRate;
			CpWater = CPHW(InletWaterTemp);
			CompressorPower = 0.0;
			OperatingHeatingPower = 0.0;
			TankHeatingCOP = 0.0;
		}


		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		if (!(CyclingScheme == ContFanCycCoil) && PartLoadRatio > 0.0) {
			CondInletMassFlowRate = CondInletMassFlowRate / PartLoadRatio;
			EvapInletMassFlowRate = EvapInletMassFlowRate / PartLoadRatio;
		}

		VarSpeedCoil(DXCoilNum).AirMassFlowRate = EvapInletMassFlowRate;
		VarSpeedCoil(DXCoilNum).WaterMassFlowRate = CondInletMassFlowRate;

		// determine inlet air temperature type for curve objects
		if (VarSpeedCoil(DXCoilNum).InletAirTemperatureType == WetBulbIndicator) {
			InletAirTemp = HPWHInletWBTemp;
		} else {
			InletAirTemp = HPWHInletDBTemp;
		}

		// check if indoor evaporator or outdoor evaporator
		CrankcaseHeatingPower = 0.0;
		if (EvapInletNode != 0) {
			LoadSideInletDBTemp = Node(EvapInletNode).Temp;
			LoadSideInletHumRat = Node(EvapInletNode).HumRat;
			LoadPressure = Node(EvapInletNode).Press;
			//prevent the air pressure not given
			if (LoadPressure < 10.0) LoadPressure = OutBaroPress;

			LoadSideInletWBTemp = Node(EvapInletNode).OutAirWetBulb;
			LoadSideInletEnth = Node(EvapInletNode).Enthalpy;
		} else {
			LoadSideInletDBTemp = OutDryBulbTemp;
			LoadSideInletHumRat = OutHumRat;
			LoadPressure = OutBaroPress;
			LoadSideInletWBTemp = OutWetBulbTemp;
			LoadSideInletEnth = OutEnthalpy;

			// Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
			if (OutDryBulbTemp < VarSpeedCoil(DXCoilNum).MaxOATCrankcaseHeater) {
				CrankcaseHeatingPower = VarSpeedCoil(DXCoilNum).CrankcaseHeaterCapacity;
			};
		}


		LoadSideMassFlowRate = EvapInletMassFlowRate;
		SourceSideMassFlowRate = CondInletMassFlowRate;
		SourceSideInletTemp = InletWaterTemp;
		SourceSideInletEnth = Node(CondInletNode).Enthalpy;
		VarSpeedCoil(DXCoilNum).InletWaterEnthalpy = SourceSideInletEnth;

		//Check for flows, do not perform simulation if no flow in load side or source side.
		if ((SourceSideMassFlowRate <= 0.0) || (LoadSideMassFlowRate <= 0.0)) {
			VarSpeedCoil(DXCoilNum).SimFlag = false;
			return;
		}
		else {
			VarSpeedCoil(DXCoilNum).SimFlag = true;
		}


		MaxSpeed = VarSpeedCoil(DXCoilNum).NumOfSpeeds;

		// must be placed inside the loop, otherwise cause bug in release mode, need to be present at two places
		if (SpeedNum > MaxSpeed) {
			SpeedCal = MaxSpeed;
		} else {
			SpeedCal = SpeedNum;
		}

		//part-load calculation
		RuntimeFrac = 1.0;
		VarSpeedCoil(DXCoilNum).RunFrac = 1.0;
		if ((SpeedNum == 1) && (PartLoadRatio < 1.0)) {
			PLF = CurveValue(VarSpeedCoil(DXCoilNum).PLFFPLR, PartLoadRatio);
			if (PLF < 0.7) {
				PLF = 0.7;
			}
			// calculate the run time fraction
			VarSpeedCoil(DXCoilNum).RunFrac = PartLoadRatio / PLF;
			VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;

			if (VarSpeedCoil(DXCoilNum).RunFrac > 1.0) {
				VarSpeedCoil(DXCoilNum).RunFrac = 1.0; // Reset coil runtime fraction to 1.0
			} else if (VarSpeedCoil(DXCoilNum).RunFrac < 0.0) {
				VarSpeedCoil(DXCoilNum).RunFrac = 0.0;
			}

			RuntimeFrac = VarSpeedCoil(DXCoilNum).RunFrac;
		}

		//interpolate between speeds
		// must be placed inside the loop, otherwise cause bug in release mode
		if (SpeedNum > MaxSpeed) {
			SpeedCal = MaxSpeed;
		} else {
			SpeedCal = SpeedNum;
		}


		if ((SpeedNum == 1) || (SpeedNum > MaxSpeed) || (SpeedRatio == 1.0)) {
			AirMassFlowRatio = LoadSideMassFlowRate / VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;
			WaterMassFlowRatio = SourceSideMassFlowRate / VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
			VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower = VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedCal);

			COPTempModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal), InletAirTemp, SourceSideInletTemp);
			COPAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);
			COPWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);

			COP =  VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal) * COPTempModFac * COPAirFFModFac *
				COPWaterFFModFac;

			TOTCAPTempModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
				InletAirTemp, SourceSideInletTemp);
			//   Get capacity modifying factor (function of mass flow) for off-rated conditions
			TOTCAPAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal),
				AirMassFlowRatio);
			//Get capacity modifying factor (function of mass flow) for off-rated conditions
			TOTCAPWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal),
				WaterMassFlowRatio);

			OperatingHeatingCapacity = VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TOTCAPTempModFac *
				TOTCAPAirFFModFac * TOTCAPWaterFFModFac;

			Winput = OperatingHeatingCapacity / COP;
			OperatingHeatingPower = Winput;

			OperatingHeatingCOP = COP;
			PumpHeatToWater = VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower *
				VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;
			TankHeatingCOP = OperatingHeatingCOP;

			// account for pump heat if not included in total water heating capacity
			if (VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
				TotalTankHeatingCapacity = OperatingHeatingCapacity;
			} else {
				TotalTankHeatingCapacity = OperatingHeatingCapacity + PumpHeatToWater;
			}

			HPRTF = RuntimeFrac;
			// calculate evaporator total cooling capacity
			if (VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP) {
				if (VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
					//       make sure fan power is full load fan power
					CompressorPower = OperatingHeatingPower - FanElecPower / HPRTF
						- VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
					if (OperatingHeatingPower > 0.0) TankHeatingCOP = TotalTankHeatingCapacity / OperatingHeatingPower;
				} else {
					CompressorPower = OperatingHeatingPower - FanElecPower / HPRTF;
					if ((OperatingHeatingPower + VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
						TankHeatingCOP = TotalTankHeatingCapacity /
						(OperatingHeatingPower + VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
				}
			} else {
				if (VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
					//       make sure fan power is full load fan power
					CompressorPower = OperatingHeatingPower -
						VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
					if ((OperatingHeatingPower + FanElecPower / HPRTF) > 0.0)
						TankHeatingCOP = TotalTankHeatingCapacity /
						(OperatingHeatingPower + FanElecPower / HPRTF);
				} else {
					CompressorPower = OperatingHeatingPower;
					if ((OperatingHeatingPower + FanElecPower / HPRTF +
						VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
						TankHeatingCOP = TotalTankHeatingCapacity /
						(OperatingHeatingPower + FanElecPower / HPRTF +
						VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
				}
			}

			if (VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
				EvapCoolingCapacity = TotalTankHeatingCapacity - PumpHeatToWater - CompressorPower;
			} else {
				EvapCoolingCapacity = TotalTankHeatingCapacity - CompressorPower;
			}

			CBFSpeed = AdjustCBF(VarSpeedCoil(DXCoilNum).MSRatedCBF(SpeedCal),
				VarSpeedCoil(DXCoilNum).MSRatedAirMassFlowRate(SpeedCal), LoadSideMassFlowRate);

		} else {
			AirMassFlowRatio = LoadSideMassFlowRate / VarSpeedCoil(DXCoilNum).DesignAirMassFlowRate;
			WaterMassFlowRatio = SourceSideMassFlowRate / VarSpeedCoil(DXCoilNum).DesignWaterMassFlowRate;
			AoEff = VarSpeedCoil(DXCoilNum).MSEffectiveAo(SpeedCal) * SpeedRatio +
				(1.0 - SpeedRatio) * VarSpeedCoil(DXCoilNum).MSEffectiveAo(SpeedCal - 1);
			CBFSpeed = std::exp(-AoEff / LoadSideMassFlowRate);

			//calculate low speed
			SpeedCal = SpeedNum - 1;

			VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower = VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedCal);
			COPTempModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal), InletAirTemp, SourceSideInletTemp);
			COPAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);
			COPWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);

			COP =  VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal) * COPTempModFac * COPAirFFModFac * COPWaterFFModFac;

			TOTCAPTempModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
				InletAirTemp, SourceSideInletTemp);
			//   Get capacity modifying factor (function of mass flow) for off-rated conditions
			TOTCAPAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal),
				AirMassFlowRatio);
			//Get capacity modifying factor (function of mass flow) for off-rated conditions
			TOTCAPWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal),
				WaterMassFlowRatio);

			OperatingHeatingCapacity = VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TOTCAPTempModFac *
				TOTCAPAirFFModFac * TOTCAPWaterFFModFac;

			Winput = OperatingHeatingCapacity / COP;
			OperatingHeatingPower = Winput;
			Winput1 = Winput;
			WHCAP1 = OperatingHeatingCapacity;

			//calculate upper speed
			SpeedCal = SpeedNum;

			VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower = VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedCal);
			COPTempModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRFTemp(SpeedCal), InletAirTemp, SourceSideInletTemp);
			COPAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRAirFFlow(SpeedCal), AirMassFlowRatio);
			COPWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSEIRWaterFFlow(SpeedCal), WaterMassFlowRatio);

			COP = VarSpeedCoil(DXCoilNum).MSRatedCOP(SpeedCal) * COPTempModFac * COPAirFFModFac * COPWaterFFModFac;

			TOTCAPTempModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapFTemp(SpeedCal),
				InletAirTemp, SourceSideInletTemp);
			//   Get capacity modifying factor (function of mass flow) for off-rated conditions
			TOTCAPAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapAirFFlow(SpeedCal),
				AirMassFlowRatio);
			//Get capacity modifying factor (function of mass flow) for off-rated conditions
			TOTCAPWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum).MSCCapWaterFFlow(SpeedCal),
				WaterMassFlowRatio);

			OperatingHeatingCapacity = VarSpeedCoil(DXCoilNum).MSRatedTotCap(SpeedCal) * TOTCAPTempModFac *
				TOTCAPAirFFModFac * TOTCAPWaterFFModFac;

			Winput = OperatingHeatingCapacity / COP;
			OperatingHeatingPower = Winput;

			Winput2 = Winput;
			WHCAP2 = OperatingHeatingCapacity;

			//interpolation
			Winput = Winput2 * SpeedRatio + (1.0 - SpeedRatio) * Winput1;
			OperatingHeatingPower = Winput;
			OperatingHeatingCapacity = WHCAP2 * SpeedRatio + (1.0 - SpeedRatio) * WHCAP1;
			VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower = VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedNum)* SpeedRatio +
				(1.0 - SpeedRatio) * VarSpeedCoil(DXCoilNum).MSWHPumpPower(SpeedNum - 1);

			OperatingHeatingCOP = OperatingHeatingCapacity / OperatingHeatingPower;
			TankHeatingCOP = OperatingHeatingCOP;

			PumpHeatToWater = VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower *
				VarSpeedCoil(DXCoilNum).HPWHCondPumpFracToWater;

			// account for pump heat if not included in total water heating capacity
			if (VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
				TotalTankHeatingCapacity = OperatingHeatingCapacity;
			} else {
				TotalTankHeatingCapacity = OperatingHeatingCapacity + PumpHeatToWater;
			}

			HPRTF = RuntimeFrac;
			// calculate evaporator total cooling capacity
			if (VarSpeedCoil(DXCoilNum).FanPowerIncludedInCOP) {
				if (VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
					//       make sure fan power is full load fan power
					CompressorPower = OperatingHeatingPower - FanElecPower / HPRTF
						- VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
					if (OperatingHeatingPower > 0.0) TankHeatingCOP = TotalTankHeatingCapacity / OperatingHeatingPower;
				} else {
					CompressorPower = OperatingHeatingPower - FanElecPower / HPRTF;
					if ((OperatingHeatingPower + VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
						TankHeatingCOP = TotalTankHeatingCapacity /
						(OperatingHeatingPower + VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
				}
			} else {
				if (VarSpeedCoil(DXCoilNum).CondPumpPowerInCOP) {
					//       make sure fan power is full load fan power
					CompressorPower = OperatingHeatingPower -
						VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower;
					if ((OperatingHeatingPower + FanElecPower / HPRTF) > 0.0)
						TankHeatingCOP = TotalTankHeatingCapacity /
						(OperatingHeatingPower + FanElecPower / HPRTF);
				} else {
					CompressorPower = OperatingHeatingPower;
					if ((OperatingHeatingPower + FanElecPower / HPRTF +
						VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) > 0.0)
						TankHeatingCOP = TotalTankHeatingCapacity /
						(OperatingHeatingPower + FanElecPower / HPRTF +
						VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower);
				}
			}

			if (VarSpeedCoil(DXCoilNum).CondPumpHeatInCapacity) {
				EvapCoolingCapacity = TotalTankHeatingCapacity - PumpHeatToWater - CompressorPower;
			} else {
				EvapCoolingCapacity = TotalTankHeatingCapacity - CompressorPower;
			}
		}

		QSource = TotalTankHeatingCapacity;
		QLoadTotal = EvapCoolingCapacity;
		DXCoilTotalCapacity = EvapCoolingCapacity;//for standard rating calculation
		SHR = 1.0;
		//if indoor, calculate SHR
		if (EvapInletNode != 0) {
			if (CBFSpeed > 0.999) CBFSpeed = 0.999;

			if (CBFSpeed < 0.001){
				SHR = 1.0; 
			}
			else{
				hDelta = QLoadTotal / LoadSideMassFlowRate;
				hADP = LoadSideInletEnth - hDelta / (1.0 - CBFSpeed);
				tADP = PsyTsatFnHPb(hADP, LoadPressure, RoutineName);
				wADP = PsyWFnTdbH(tADP, hADP, RoutineName);
				hTinwADP = PsyHFnTdbW(LoadSideInletDBTemp, wADP);
				if ((LoadSideInletEnth - hADP) > 1.e-10) {
					SHR = min((hTinwADP - hADP) / (LoadSideInletEnth - hADP), 1.0);
				}
				else {
					SHR = 1.0;
				}
			}
		}

		QSensible = QLoadTotal * SHR;

		// determine condenser water inlet/outlet condition at full capacity
		if (CondInletMassFlowRate == 0.0) {
			OutletWaterTemp = InletWaterTemp;
		} else {
			OutletWaterTemp = InletWaterTemp + TotalTankHeatingCapacity / (CpWater * CondInletMassFlowRate);
		}

		Node(CondOutletNode).Temp = OutletWaterTemp;

		Node(CondOutletNode).MassFlowRate = Node(CondInletNode).MassFlowRate;

		// send heating capacity and COP to water heater module for standards rating calculation
		// total heating capacity including condenser pump
		VSHPWHHeatingCapacity = TotalTankHeatingCapacity;
		// total heating COP including compressor, fan, and condenser pump
		VSHPWHHeatingCOP = TankHeatingCOP;

		VarSpeedCoil(DXCoilNum).TotalHeatingEnergyRate = TotalTankHeatingCapacity * PartLoadRatio;
		// calculate total compressor plus condenser pump power, fan power reported in fan module
		VarSpeedCoil(DXCoilNum).ElecWaterHeatingPower = (CompressorPower +
			VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower) * HPRTF;

		//pass the outputs for the cooling coil section
		VarSpeedCoil(DXCoilNum).BasinHeaterPower = 0.0;
		VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower = CrankcaseHeatingPower*(1.0 - RuntimeFrac);

		//calculate coil outlet state variables
		LoadSideOutletEnth = LoadSideInletEnth - QLoadTotal / LoadSideMassFlowRate;
		CpAir = PsyCpAirFnWTdb(LoadSideInletHumRat, LoadSideInletDBTemp);
		LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible / (LoadSideMassFlowRate * CpAir);

		MaxHumRat = PsyWFnTdbRhPb(LoadSideOutletDBTemp, 0.9999, VarSpeedCoil(DXCoilNum).InletAirPressure, RoutineName);
		MaxOutletEnth = PsyHFnTdbW(LoadSideOutletDBTemp, MaxHumRat);
		if (LoadSideOutletEnth > MaxOutletEnth) {
			LoadSideOutletEnth = MaxOutletEnth;
		}
		LoadSideOutletHumRat = PsyWFnTdbH(LoadSideOutletDBTemp, LoadSideOutletEnth, RoutineName);
		if (LoadSideOutletHumRat > MaxHumRat) {
			LoadSideOutletHumRat = MaxHumRat;
		}

		//Actual outlet conditions are "average" for time step
		if (CyclingScheme == ContFanCycCoil) {
			// continuous fan, cycling compressor
			VarSpeedCoil(DXCoilNum).OutletAirEnthalpy = PartLoadRatio * LoadSideOutletEnth + (1.0 - PartLoadRatio) * LoadSideInletEnth;
			VarSpeedCoil(DXCoilNum).OutletAirHumRat = PartLoadRatio * LoadSideOutletHumRat + (1.0 - PartLoadRatio) * LoadSideInletHumRat;
			VarSpeedCoil(DXCoilNum).OutletAirDBTemp = PsyTdbFnHW(VarSpeedCoil(DXCoilNum).OutletAirEnthalpy, VarSpeedCoil(DXCoilNum).OutletAirHumRat);
			PLRCorrLoadSideMdot = LoadSideMassFlowRate;
		} else {
			VarSpeedCoil(DXCoilNum).OutletAirEnthalpy = LoadSideOutletEnth;
			VarSpeedCoil(DXCoilNum).OutletAirHumRat = LoadSideOutletHumRat;
			VarSpeedCoil(DXCoilNum).OutletAirDBTemp = LoadSideOutletDBTemp;
			PLRCorrLoadSideMdot = LoadSideMassFlowRate * PartLoadRatio;
		}

		// scale heat transfer rates to PLR and power to RTF
		QLoadTotal *= PartLoadRatio;
		QSensible *= PartLoadRatio;
		// count the powr separately
		Winput *= RuntimeFrac; //+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower &
		//+ VarSpeedCoil(DXCoilNum)%BasinHeaterPower + VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower
		QSource *= PartLoadRatio;

		//  Add power to global variable so power can be summed by parent object
		DXElecCoolingPower = Winput;

		ReportingConstant = TimeStepSys * SecInHour;
		//Update heat pump data structure
		VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower =
			VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower*RuntimeFrac;// water heating pump power
		VarSpeedCoil(DXCoilNum).Power = Winput;
		VarSpeedCoil(DXCoilNum).QLoadTotal = QLoadTotal;
		VarSpeedCoil(DXCoilNum).QSensible = QSensible;
		VarSpeedCoil(DXCoilNum).QLatent = QLoadTotal - QSensible;
		VarSpeedCoil(DXCoilNum).QSource = QSource;
		VarSpeedCoil(DXCoilNum).Energy = Winput * ReportingConstant;
		VarSpeedCoil(DXCoilNum).EnergyLoadTotal = QLoadTotal * ReportingConstant;
		VarSpeedCoil(DXCoilNum).EnergySensible = QSensible * ReportingConstant;
		VarSpeedCoil(DXCoilNum).EnergyLatent = (QLoadTotal - QSensible) * ReportingConstant;
		VarSpeedCoil(DXCoilNum).EnergySource = QSource * ReportingConstant;
		VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption = VarSpeedCoil(DXCoilNum).CrankcaseHeaterPower * ReportingConstant;
		VarSpeedCoil(DXCoilNum).EvapWaterConsump = 0.0;
		VarSpeedCoil(DXCoilNum).BasinHeaterConsumption = 0.0;
		//re-use EvapCondPumpElecConsumption to store WH pump energy consumption
		VarSpeedCoil(DXCoilNum).EvapCondPumpElecConsumption =
			VarSpeedCoil(DXCoilNum).HPWHCondPumpElecNomPower * ReportingConstant;
		if (RuntimeFrac == 0.0) {
			VarSpeedCoil(DXCoilNum).COP = 0.0;
		} else {
			VarSpeedCoil(DXCoilNum).COP = QLoadTotal / Winput;
		}
		VarSpeedCoil(DXCoilNum).RunFrac = RuntimeFrac;
		VarSpeedCoil(DXCoilNum).PartLoadRatio = PartLoadRatio;
		VarSpeedCoil(DXCoilNum).AirMassFlowRate = PLRCorrLoadSideMdot;
		rhoair = PsyRhoAirFnPbTdbW(OutBaroPress, LoadSideInletDBTemp, LoadSideInletHumRat, RoutineName);
		VarSpeedCoil(DXCoilNum).AirVolFlowRate = VarSpeedCoil(DXCoilNum).AirMassFlowRate / rhoair;
		VarSpeedCoil(DXCoilNum).WaterMassFlowRate = SourceSideMassFlowRate;
		RhoWater = RhoH2O(InletWaterTemp); // initialize
		VarSpeedCoil(DXCoilNum).WaterVolFlowRate = VarSpeedCoil(DXCoilNum).WaterMassFlowRate / RhoWater;

		VarSpeedCoil(DXCoilNum).OutletWaterTemp = SourceSideInletTemp + QSource / (SourceSideMassFlowRate * CpWater);
		VarSpeedCoil(DXCoilNum).OutletWaterEnthalpy = SourceSideInletEnth + QSource / SourceSideMassFlowRate;
		VarSpeedCoil(DXCoilNum).QWasteHeat = 0.0;

		if (true == VarSpeedCoil(DXCoilNum).bIsDesuperheater)//desuperheater doesn't save power and cooling energy variables
		{
			//source side is the water side; load side is the air side
			VarSpeedCoil(DXCoilNum).Power = 0.0;
			VarSpeedCoil(DXCoilNum).QLoadTotal = 0.0;
			VarSpeedCoil(DXCoilNum).QSensible = 0.0;
			VarSpeedCoil(DXCoilNum).QLatent = 0.0;
			VarSpeedCoil(DXCoilNum).Energy = 0.0;
			VarSpeedCoil(DXCoilNum).EnergyLoadTotal = 0.0;
			VarSpeedCoil(DXCoilNum).EnergySensible = 0.0;
			VarSpeedCoil(DXCoilNum).EnergyLatent = 0.0;
			VarSpeedCoil(DXCoilNum).CrankcaseHeaterConsumption = 0.0; 
		}
	}

	void
	CalcVarSpeedCoilHeating(
		int const DXCoilNum, // Heat Pump Number
		int const CyclingScheme, // Fan/Compressor cycling scheme indicator
		Real64 & RuntimeFrac, // Runtime Fraction of compressor or percent on time (on-time/cycle time)
		Real64 const EP_UNUSED( SensDemand ), // Cooling Sensible Demand [W] !unused1208
		int const CompOp, // compressor operation flag
		Real64 const PartLoadRatio, // compressor part load ratio
		Real64 const EP_UNUSED( OnOffAirFlowRatio ), // ratio of compressor on flow to average flow over time step
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
		int const SpeedNum // Speed number, high bound, i.e. SpeedNum - 1 is the other side
	)
	{

		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:CalcHPHeatingSimple
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for simulating the heating mode of the Variable Speed Water to Air HP Simple

		// METHODOLOGY EMPLOYED:
		// Simulate the heat pump performance using the coefficients and rated conditions
		// Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
		// and RuntimeFrac.

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::DXElecHeatingPower;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyWFnTdbH;
		using Psychrometrics::PsyHFnTdbW;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcVarSpeedCoilHeating" );
		static std::string const RoutineNameSourceSideInletTemp( "CalcVarSpeedCoilHeating:SourceSideInletTemp" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpSource; // Specific heat of water [J/kg_C]
		Real64 CpAir; // Specific heat of air [J/kg_C]

		Real64 AirMassFlowRatio; // airflow ratio at low speed
		Real64 WaterMassFlowRatio; // airflow ratio at high speed
		Real64 TotCapAirFFModFac; // air flow fraction modification
		Real64 TotCapWaterFFModFac; // water flow fraction modification
		Real64 TotCapTempModFac; // total capacity temperature correctio fraction
		Real64 EIRAirFFModFac; // air flow fraction modification
		Real64 EIRWaterFFModFac; // water flow fraction modification
		Real64 EIRTempModFac; // total capacity temperature correctio fraction
		Real64 EIR; // total capacity temperature correctio fraction
		int MaxSpeed; // maximum speed level
		int SpeedCal; // calculated speed level
		Real64 QLoadTotal1; // heating capacit at low speed
		Real64 QLoadTotal2; // heating capacity at high speed
		Real64 Winput1; // power consumption at low speed
		Real64 Winput2; // power consumption at high speed
		Real64 QWasteHeat; // recoverable waste heat
		Real64 QWasteHeat1; // recoverable waste heat at low speed
		Real64 QWasteHeat2; // recoverable waste heat at high speed
		Real64 PLF; // part-load function
		Real64 ReportingConstant;
		Real64 rhoair(0.0);//entering air density

		// ADDED VARIABLES FOR air source coil
		static Real64 OutdoorCoilT( 0.0 ); // Outdoor coil temperature (C)
		static Real64 OutdoorCoildw( 0.0 ); // Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
		static Real64 OutdoorDryBulb( 0.0 ); // Outdoor dry-bulb temperature at condenser (C)
		static Real64 OutdoorWetBulb( 0.0 ); // Outdoor wet-bulb temperature at condenser (C)
		static Real64 OutdoorHumRat( 0.0 ); // Outdoor humidity ratio at condenser (kg/kg)
		static Real64 OutdoorPressure( 0.0 ); // Outdoor barometric pressure at condenser (Pa)
		static Real64 FractionalDefrostTime( 0.0 ); // Fraction of time step system is in defrost
		static Real64 HeatingCapacityMultiplier( 0.0 ); // Multiplier for heating capacity when system is in defrost
		static Real64 InputPowerMultiplier( 0.0 ); // Multiplier for power when system is in defrost
		static Real64 LoadDueToDefrost( 0.0 ); // Additional load due to defrost
		static Real64 CrankcaseHeatingPower( 0.0 ); // power due to crankcase heater
		static Real64 DefrostEIRTempModFac( 0.0 ); // EIR modifier for defrost (function of entering wetbulb, outside drybulb)
		static Real64 TotRatedCapacity( 0.0 ); // total rated capacity at the given speed and speed ratio for defrosting

		MaxSpeed = VarSpeedCoil( DXCoilNum ).NumOfSpeeds;

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		if ( ! ( CyclingScheme == ContFanCycCoil ) && PartLoadRatio > 0.0 ) {
			VarSpeedCoil( DXCoilNum ).AirMassFlowRate = Node( VarSpeedCoil( DXCoilNum ).AirInletNodeNum ).MassFlowRate / PartLoadRatio;
		}

		LoadSideMassFlowRate = VarSpeedCoil( DXCoilNum ).AirMassFlowRate;
		LoadSideInletDBTemp = VarSpeedCoil( DXCoilNum ).InletAirDBTemp;
		LoadSideInletHumRat = VarSpeedCoil( DXCoilNum ).InletAirHumRat;

		LoadSideInletWBTemp = PsyTwbFnTdbWPb( LoadSideInletDBTemp, LoadSideInletHumRat, OutBaroPress, RoutineName );
		LoadSideInletEnth = VarSpeedCoil( DXCoilNum ).InletAirEnthalpy;
		CpAir = PsyCpAirFnWTdb( LoadSideInletHumRat, LoadSideInletDBTemp );

		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
			// Get condenser outdoor node info from DX Heating Coil
			if ( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum != 0 ) {
				OutdoorDryBulb = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).Temp;
				OutdoorHumRat = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).HumRat;
				OutdoorPressure = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).Press;
				OutdoorWetBulb = Node( VarSpeedCoil( DXCoilNum ).CondenserInletNodeNum ).OutAirWetBulb;
			} else {
				OutdoorDryBulb = OutDryBulbTemp;
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			}
			SourceSideMassFlowRate = 1.0; // not used and avoid divided by zero
			SourceSideInletTemp = OutdoorDryBulb;
			SourceSideInletEnth = PsyHFnTdbW( OutdoorDryBulb, OutdoorHumRat );
			CpSource = PsyCpAirFnWTdb( OutHumRat, OutdoorDryBulb );

			// Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
			if ( OutdoorDryBulb < VarSpeedCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
				CrankcaseHeatingPower = VarSpeedCoil( DXCoilNum ).CrankcaseHeaterCapacity;
			} else {
				CrankcaseHeatingPower = 0.0;
			}
		} else {
			SourceSideMassFlowRate = VarSpeedCoil( DXCoilNum ).WaterMassFlowRate;
			SourceSideInletTemp = VarSpeedCoil( DXCoilNum ).InletWaterTemp;
			SourceSideInletEnth = VarSpeedCoil( DXCoilNum ).InletWaterEnthalpy;
			CpSource = GetSpecificHeatGlycol( PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidName, SourceSideInletTemp, PlantLoop( VarSpeedCoil( DXCoilNum ).LoopNum ).FluidIndex, RoutineNameSourceSideInletTemp );
		}

		//Check for flows, do not perform simulation if no flow in load side or source side.
		if ( ( SourceSideMassFlowRate <= 0.0 ) || ( LoadSideMassFlowRate <= 0.0 ) ) {
			VarSpeedCoil( DXCoilNum ).SimFlag = false;
			return;
		} else {
			VarSpeedCoil( DXCoilNum ).SimFlag = true;
		}

		if ( ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) && ( OutdoorDryBulb < VarSpeedCoil( DXCoilNum ).MinOATCompressor ) ) {
			VarSpeedCoil( DXCoilNum ).SimFlag = false;
			return;
		}

		if ( CompOp == 0 ) {
			VarSpeedCoil( DXCoilNum ).SimFlag = false;
			return;
		}

		if ( SpeedNum > MaxSpeed ) {
			SpeedCal = MaxSpeed;
		} else {
			SpeedCal = SpeedNum;
		}

		RuntimeFrac = 1.0;
		VarSpeedCoil( DXCoilNum ).RunFrac = 1.0;
		if ( ( SpeedNum == 1 ) && ( PartLoadRatio < 1.0 ) ) {
			PLF = CurveValue( VarSpeedCoil( DXCoilNum ).PLFFPLR, PartLoadRatio );
			if ( PLF < 0.7 ) {
				PLF = 0.7;
			}
			// calculate the run time fraction
			VarSpeedCoil( DXCoilNum ).RunFrac = PartLoadRatio / PLF;
			VarSpeedCoil( DXCoilNum ).PartLoadRatio = PartLoadRatio;

			if ( VarSpeedCoil( DXCoilNum ).RunFrac > 1.0 ) {
				VarSpeedCoil( DXCoilNum ).RunFrac = 1.0; // Reset coil runtime fraction to 1.0
			} else if ( VarSpeedCoil( DXCoilNum ).RunFrac < 0.0 ) {
				VarSpeedCoil( DXCoilNum ).RunFrac = 0.0;
			}

			RuntimeFrac = VarSpeedCoil( DXCoilNum ).RunFrac;
		}

		if ( ( SpeedNum == 1 ) || ( SpeedNum > MaxSpeed ) || ( SpeedRatio == 1.0 ) ) {
			AirMassFlowRatio = LoadSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignAirMassFlowRate;

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				WaterMassFlowRatio = 1.0;
			} else {
				WaterMassFlowRatio = SourceSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate;
			}

			TotCapTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			TotCapAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( SpeedCal ), AirMassFlowRatio );

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				TotCapWaterFFModFac = 1.0;
			} else {
				TotCapWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( SpeedCal ), WaterMassFlowRatio );
			}

			QLoadTotal = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal ) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac;
			TotRatedCapacity = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal ); // for defrosting power cal

			EIRTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			EIRAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( SpeedCal ), AirMassFlowRatio );

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				EIRWaterFFModFac = 1.0;
			} else {
				EIRWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( SpeedCal ), WaterMassFlowRatio );
			}

			EIR = ( 1.0 / VarSpeedCoil( DXCoilNum ).MSRatedCOP( SpeedCal ) ) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac;
			Winput = QLoadTotal * EIR;

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				QWasteHeat = 0.0;
			} else {
				QWasteHeat = Winput * VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( SpeedCal );
				QWasteHeat *= CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			}

		} else {
			AirMassFlowRatio = LoadSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignAirMassFlowRate;

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				WaterMassFlowRatio = 1.0;
			} else {
				WaterMassFlowRatio = SourceSideMassFlowRate / VarSpeedCoil( DXCoilNum ).DesignWaterMassFlowRate;
			}

			SpeedCal = SpeedNum - 1;
			TotCapTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			TotCapAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( SpeedCal ), AirMassFlowRatio );

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				TotCapWaterFFModFac = 1.0;
			} else {
				TotCapWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( SpeedCal ), WaterMassFlowRatio );
			}

			QLoadTotal1 = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal ) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac;

			EIRTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			EIRAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( SpeedCal ), AirMassFlowRatio );

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				EIRWaterFFModFac = 1.0;
			} else {
				EIRWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( SpeedCal ), WaterMassFlowRatio );
			}

			EIR = ( 1.0 / VarSpeedCoil( DXCoilNum ).MSRatedCOP( SpeedCal ) ) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac;
			Winput1 = QLoadTotal1 * EIR;

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				QWasteHeat1 = 0.0;
			} else {
				QWasteHeat1 = Winput1 * VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( SpeedCal );
				QWasteHeat1 *= CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			}

			SpeedCal = SpeedNum;
			TotCapTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapFTemp( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			TotCapAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapAirFFlow( SpeedCal ), AirMassFlowRatio );

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				TotCapWaterFFModFac = 1.0;
			} else {
				TotCapWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSCCapWaterFFlow( SpeedCal ), WaterMassFlowRatio );
			}

			QLoadTotal2 = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal ) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac;

			EIRTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRFTemp( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			EIRAirFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRAirFFlow( SpeedCal ), AirMassFlowRatio );

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				EIRWaterFFModFac = 1.0;
			} else {
				EIRWaterFFModFac = CurveValue( VarSpeedCoil( DXCoilNum ).MSEIRWaterFFlow( SpeedCal ), WaterMassFlowRatio );
			}

			EIR = ( 1.0 / VarSpeedCoil( DXCoilNum ).MSRatedCOP( SpeedCal ) ) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac;
			Winput2 = QLoadTotal2 * EIR;

			if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
				QWasteHeat2 = 0.0;
			} else {
				QWasteHeat2 = Winput2 * VarSpeedCoil( DXCoilNum ).MSWasteHeatFrac( SpeedCal );
				QWasteHeat2 *= CurveValue( VarSpeedCoil( DXCoilNum ).MSWasteHeat( SpeedCal ), LoadSideInletDBTemp, SourceSideInletTemp );
			}

			QLoadTotal = QLoadTotal2 * SpeedRatio + ( 1.0 - SpeedRatio ) * QLoadTotal1;
			Winput = Winput2 * SpeedRatio + ( 1.0 - SpeedRatio ) * Winput1;
			QWasteHeat = QWasteHeat2 * SpeedRatio + ( 1.0 - SpeedRatio ) * QWasteHeat1;
			TotRatedCapacity = VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal ) * SpeedRatio + ( 1.0 - SpeedRatio ) * VarSpeedCoil( DXCoilNum ).MSRatedTotCap( SpeedCal - 1 );
		}

		VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower = 0.0; //necessary to clear zero for water source coils
		VarSpeedCoil( DXCoilNum ).DefrostPower = 0.0; //clear the defrost power
		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
			// Calculating adjustment factors for defrost
			// Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
			OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589;
			OutdoorCoildw = max( 1.0e-6, ( OutdoorHumRat - PsyWFnTdpPb( OutdoorCoilT, OutdoorPressure ) ) );

			// Initializing defrost adjustment factors
			LoadDueToDefrost = 0.0;
			HeatingCapacityMultiplier = 1.0;
			FractionalDefrostTime = 0.0;
			InputPowerMultiplier = 1.0;
			// Check outdoor temperature to determine of defrost is active
			if ( OutdoorDryBulb <= VarSpeedCoil( DXCoilNum ).MaxOATDefrost ) {
				// Calculate defrost adjustment factors depending on defrost control type
				if ( VarSpeedCoil( DXCoilNum ).DefrostControl == Timed ) {
					FractionalDefrostTime = VarSpeedCoil( DXCoilNum ).DefrostTime;
					HeatingCapacityMultiplier = 0.909 - 107.33 * OutdoorCoildw;
					InputPowerMultiplier = 0.90 - 36.45 * OutdoorCoildw;
				} else { //else defrost control is on-demand
					FractionalDefrostTime = 1.0 / ( 1.0 + 0.01446 / OutdoorCoildw );
					HeatingCapacityMultiplier = 0.875 * ( 1.0 - FractionalDefrostTime );
					InputPowerMultiplier = 0.954 * ( 1.0 - FractionalDefrostTime );
				}
				// correction fractional defrost time shorten by runtime fraction
				FractionalDefrostTime *= RuntimeFrac;

				if ( FractionalDefrostTime > 0.0 ) {
					// Calculate defrost adjustment factors depending on defrost control strategy
					if ( VarSpeedCoil( DXCoilNum ).DefrostStrategy == ReverseCycle ) {
						LoadDueToDefrost = ( 0.01 * FractionalDefrostTime ) * ( 7.222 - OutdoorDryBulb ) * ( TotRatedCapacity / 1.01667 );
						DefrostEIRTempModFac = CurveValue( VarSpeedCoil( DXCoilNum ).DefrostEIRFT, max( 15.555, LoadSideInletWBTemp ), max( 15.555, OutdoorDryBulb ) );
						VarSpeedCoil( DXCoilNum ).DefrostPower = DefrostEIRTempModFac * ( TotRatedCapacity / 1.01667 ) * FractionalDefrostTime;
					} else { // Defrost strategy is resistive
						VarSpeedCoil( DXCoilNum ).DefrostPower = VarSpeedCoil( DXCoilNum ).DefrostCapacity * FractionalDefrostTime;
					}
				} else { // Defrost is not active because (OutDryBulbTemp > VarSpeedCoil(DXCoilNum).MaxOATDefrost)
					VarSpeedCoil( DXCoilNum ).DefrostPower = 0.0;
				}
			}

			VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - RuntimeFrac );
			//! Modify total heating capacity based on defrost heating capacity multiplier
			//! MaxHeatCap passed from parent object VRF Condenser and is used to limit capacity of TU's to that available from condenser
			//  IF(PRESENT(MaxHeatCap))THEN
			//    TotCap = MIN(MaxHeatCap,TotCap * HeatingCapacityMultiplier)
			//  ELSE
			//    TotCap = TotCap * HeatingCapacityMultiplier
			//  END IF
			QLoadTotal = QLoadTotal * HeatingCapacityMultiplier - LoadDueToDefrost;
			// count the powr separately
			Winput *= InputPowerMultiplier; //+ VarSpeedCoil(DXCoilNum)%DefrostPower

		}

		QSource = QLoadTotal + QWasteHeat - Winput;
		QSensible = QLoadTotal;

		if ( QSource < 0 ) {
			QSource = 0.0;
			QWasteHeat = Winput - QLoadTotal;
		}

		// calculate coil outlet state variables
		LoadSideOutletEnth = LoadSideInletEnth + QLoadTotal / LoadSideMassFlowRate;
		LoadSideOutletDBTemp = LoadSideInletDBTemp + QSensible / ( LoadSideMassFlowRate * CpAir );
		LoadSideOutletHumRat = PsyWFnTdbH( LoadSideOutletDBTemp, LoadSideOutletEnth, RoutineName );

		// Actual outlet conditions are "average" for time step
		if ( CyclingScheme == ContFanCycCoil ) {
			// continuous fan, cycling compressor
			VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy = PartLoadRatio * LoadSideOutletEnth + ( 1.0 - PartLoadRatio ) * LoadSideInletEnth;
			VarSpeedCoil( DXCoilNum ).OutletAirHumRat = PartLoadRatio * LoadSideOutletHumRat + ( 1.0 - PartLoadRatio ) * LoadSideInletHumRat;
			VarSpeedCoil( DXCoilNum ).OutletAirDBTemp = PsyTdbFnHW( VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy, VarSpeedCoil( DXCoilNum ).OutletAirHumRat );
			PLRCorrLoadSideMdot = LoadSideMassFlowRate;
		} else {
			// default to cycling fan, cycling compressor
			VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy = LoadSideOutletEnth;
			VarSpeedCoil( DXCoilNum ).OutletAirHumRat = LoadSideOutletHumRat;
			VarSpeedCoil( DXCoilNum ).OutletAirDBTemp = LoadSideOutletDBTemp;
			PLRCorrLoadSideMdot = LoadSideMassFlowRate * PartLoadRatio;
		}

		// scale heat transfer rates to PLR and power to RTF
		QLoadTotal *= PartLoadRatio;
		QSensible *= PartLoadRatio;
		// count the powr separately
		Winput *= RuntimeFrac; //+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower
		QSource *= PartLoadRatio;
		QWasteHeat *= PartLoadRatio;

		//  Add power to global variable so power can be summed by parent object
		DXElecHeatingPower = Winput;

		ReportingConstant = TimeStepSys * SecInHour;
		//Update heat pump data structure
		VarSpeedCoil( DXCoilNum ).Power = Winput;
		VarSpeedCoil( DXCoilNum ).QLoadTotal = QLoadTotal;
		VarSpeedCoil( DXCoilNum ).QSensible = QSensible;
		VarSpeedCoil( DXCoilNum ).QSource = QSource;
		VarSpeedCoil( DXCoilNum ).Energy = Winput * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergyLoadTotal = QLoadTotal * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergySensible = QSensible * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergyLatent = 0.0;
		VarSpeedCoil( DXCoilNum ).EnergySource = QSource * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).CrankcaseHeaterConsumption = VarSpeedCoil( DXCoilNum ).CrankcaseHeaterPower * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).DefrostConsumption = VarSpeedCoil( DXCoilNum ).DefrostPower * ReportingConstant;
		if ( RuntimeFrac == 0.0 ) {
			VarSpeedCoil( DXCoilNum ).COP = 0.0;
		} else {
			VarSpeedCoil( DXCoilNum ).COP = QLoadTotal / Winput;
		}
		VarSpeedCoil( DXCoilNum ).RunFrac = RuntimeFrac;
		VarSpeedCoil( DXCoilNum ).PartLoadRatio = PartLoadRatio;
		VarSpeedCoil( DXCoilNum ).AirMassFlowRate = PLRCorrLoadSideMdot;
		rhoair = PsyRhoAirFnPbTdbW(OutBaroPress, LoadSideInletDBTemp, LoadSideInletHumRat, RoutineName);
		VarSpeedCoil(DXCoilNum).AirVolFlowRate = VarSpeedCoil(DXCoilNum).AirMassFlowRate / rhoair;

		if ( VarSpeedCoil( DXCoilNum ).VSCoilTypeOfNum == Coil_HeatingAirToAirVariableSpeed ) {
			VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletWaterTemp = 0.0;
			VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy = 0.0;
		} else {
			VarSpeedCoil( DXCoilNum ).WaterMassFlowRate = SourceSideMassFlowRate;
			VarSpeedCoil( DXCoilNum ).OutletWaterTemp = SourceSideInletTemp - QSource / ( SourceSideMassFlowRate * CpSource );
			VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy = SourceSideInletEnth - QSource / SourceSideMassFlowRate;
		}

		VarSpeedCoil( DXCoilNum ).QWasteHeat = QWasteHeat;
	}

	Real64
	GetCoilCapacityVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilCapacity
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the rated coil capacity at the nominal speed level for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::FindGlycol;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;

		// Return value
		Real64 CoilCapacity; // returned capacity of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) ||
			SameString( CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) ||
			SameString( CoilType, "COIL:COOLING:DX:VARIABLESPEED" ) ||
			SameString( CoilType, "COIL:HEATING:DX:VARIABLESPEED" ) ||
			SameString( CoilType, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED")) {
			WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
			if ( WhichCoil != 0 ) {
				if ( SameString( CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) || SameString( CoilType, "COIL:HEATING:DX:VARIABLESPEED" ) ) {
					CoilCapacity = VarSpeedCoil( WhichCoil ).RatedCapHeat;
				} else if ( SameString( CoilType, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED" ) ) {
					CoilCapacity = VarSpeedCoil(WhichCoil).RatedCapWH;
				} else {
					CoilCapacity = VarSpeedCoil( WhichCoil ).RatedCapCoolTotal;
				}
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilCapacityVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			CoilCapacity = -1000.0;
		}

		return CoilCapacity;

	}

	int
	GetCoilIndexVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilIndex
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the coil index for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::FindGlycol;
		using InputProcessor::FindItemInList;

		// Return value
		int IndexNum; // returned index of matched coil

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

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		IndexNum = FindItemInList( CoilName, VarSpeedCoil );

		if ( IndexNum == 0 ) {
		//	ShowSevereError( "GetCoilIndexVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );//screen the message for IHP coil call
			ErrorsFound = true;
		}

		return IndexNum;

	}

	Real64
	GetCoilAirFlowRateVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilAirFlowRate
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the max coil air flow rate for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//  USE FluidProperties, ONLY: FindGlycol
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;

		// Return value
		Real64 CoilAirFlowRate; // returned air volume flow rate of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) ||
			SameString( CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" ) ||
			SameString( CoilType, "COIL:COOLING:DX:VARIABLESPEED" ) ||
			SameString( CoilType, "COIL:HEATING:DX:VARIABLESPEED" ) ||
			SameString( CoilType, "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED")) {
			WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
			if ( WhichCoil != 0 ) {
				//CoilAirFlowRate=VarSpeedCoil(WhichCoil)%RatedAirVolFlowRate
				if ( VarSpeedCoil( WhichCoil ).RatedAirVolFlowRate == AutoSize ) { //means autosize
					CoilAirFlowRate = VarSpeedCoil( WhichCoil ).RatedAirVolFlowRate;
				} else {
					CoilAirFlowRate = VarSpeedCoil( WhichCoil ).MSRatedAirVolFlowRate( VarSpeedCoil( WhichCoil ).NumOfSpeeds ) / VarSpeedCoil( WhichCoil ).MSRatedAirVolFlowRate( VarSpeedCoil( WhichCoil ).NormSpedLevel ) * VarSpeedCoil( WhichCoil ).RatedAirVolFlowRate;
				} // use largest air flow rate
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilAirFlowRateVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			CoilAirFlowRate = -1000.0;
		}

		return CoilAirFlowRate;

	}


	int
	GetVSCoilPLFFPLR(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   12/2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns PLR curve index.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int PLRNumber; // returned outlet node of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if (GetCoilsInputFlag) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
		if (WhichCoil != 0) {
			PLRNumber = VarSpeedCoil(WhichCoil).PLFFPLR;
		}

		if (WhichCoil == 0) {
			ShowSevereError("GetVSCoilPLFFPLR: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
			ErrorsFound = true;
			PLRNumber = 0;
		}

		return PLRNumber;

	}

	int
	GetCoilInletNodeVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilInletNode
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the inlet node.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::FindGlycol;
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned outlet node of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
		if ( WhichCoil != 0 ) {
			NodeNumber = VarSpeedCoil( WhichCoil ).AirInletNodeNum;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilInletNodeVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilOutletNodeVariableSpeed(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:GetCoilOutletNode
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the outlet node.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::FindGlycol;
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned outlet node of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
		if ( WhichCoil != 0 ) {
			NodeNumber = VarSpeedCoil( WhichCoil ).AirOutletNodeNum;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilOutletNodeVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	void SetAirNodes(std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		int const InNode,
		int const OutNode
		)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Jan 2016
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//pass air side nodes from an IHP parent object to the coil object

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if (GetCoilsInputFlag) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList(CoilName, VarSpeedCoil);
		if (WhichCoil != 0) {
			VarSpeedCoil(WhichCoil).AirInletNodeNum = InNode;
			VarSpeedCoil(WhichCoil).AirOutletNodeNum = OutNode;
		}

		if (WhichCoil == 0) {
			ShowSevereError("SetAirNodes: Could not find Coil=\"" + CoilName + "\"");
			ErrorsFound = true;
		}
	}

	void SetWaterNodes(std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		int const InNode,
		int const OutNode
		)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   Jan 2016
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//pass water side nodes from an IHP parent object to the coil object

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if (GetCoilsInputFlag) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList(CoilName, VarSpeedCoil);
		if (WhichCoil != 0) {
			VarSpeedCoil(WhichCoil).WaterInletNodeNum = InNode;
			VarSpeedCoil(WhichCoil).WaterOutletNodeNum = OutNode;
		}

		if (WhichCoil == 0) {
			ShowSevereError("SetAirNodes: Could not find Coil=\"" + CoilName + "\"");
			ErrorsFound = true;
		}
	}

	int
	GetVSCoilCondenserInletNode(
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen, based on DXCoil:GetCoilCondenserInletNode
		//       DATE WRITTEN   July 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the condenser inlet node.  If
		// incorrect coil  name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int CondNode; // returned condenser node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
		if ( WhichCoil != 0 ) {
			CondNode = VarSpeedCoil( WhichCoil ).CondenserInletNodeNum;
		} else {
			ShowSevereError( "GetCoilCondenserInletNode: Invalid VS DX Coil, Type= VS DX Cooling Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			CondNode = 0;
		}

		return CondNode;

	}

	Real64
	GetVSCoilMinOATCompressor(
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bo Shen
		//       DATE WRITTEN   July 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns min OAT for compressor operation.  If
		// incorrect coil  name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Real64 MinOAT; // returned min OAT for compressor operation

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
		if ( WhichCoil != 0 ) {
			MinOAT = VarSpeedCoil( WhichCoil ).MinOATCompressor;
		} else {
			ShowSevereError( "GetVSCoilMinOATCompressor: Invalid VS DX Coil, Type= VS DX Cooling Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			MinOAT = -1000.0;
		}

		return MinOAT;

	}

	int
	GetVSCoilNumOfSpeeds(
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns number of speeds.  If
		// incorrect coil name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int Speeds; // returned number of speeds

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, VarSpeedCoil );
		if ( WhichCoil != 0 ) {
			Speeds = VarSpeedCoil( WhichCoil ).NumOfSpeeds;
		} else {
			ShowSevereError( "GetVSCoilNumOfSpeeds: Invalid VS DX Coil, Type= VS DX Cooling Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			Speeds = 0;
		}

		return Speeds;
	}

	void
	SetVarSpeedCoilData(
		int const WSHPNum, // Number of OA Controller
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_int CompanionCoolingCoilNum, // Index to cooling coil for heating coil = SimpleWSHPNum
		Optional_int CompanionHeatingCoilNum // Index to heating coil for cooling coil = SimpleWSHPNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:SetWSHPData
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine was designed to "push" information from a parent object to
		// this WSHP coil object.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using FluidProperties::FindGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetVarSpeedCoilInput();
			//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		if ( WSHPNum <= 0 || WSHPNum > NumWatertoAirHPs ) {
			ShowSevereError( "SetVarSpeedCoilData: called with VS WSHP Coil Number out of range=" + TrimSigDigits( WSHPNum ) + " should be >0 and <" + TrimSigDigits( NumWatertoAirHPs ) );
			ErrorsFound = true;
			return;
		}

		if ( present( CompanionCoolingCoilNum ) ) {
			VarSpeedCoil( WSHPNum ).CompanionCoolingCoilNum = CompanionCoolingCoilNum;
			VarSpeedCoil( WSHPNum ).FindCompanionUpStreamCoil = true;
			VarSpeedCoil( CompanionCoolingCoilNum ).CompanionHeatingCoilNum = WSHPNum;
		}

		if ( present( CompanionHeatingCoilNum ) ) {
			VarSpeedCoil( WSHPNum ).CompanionHeatingCoilNum = CompanionHeatingCoilNum;
			VarSpeedCoil( CompanionHeatingCoilNum ).CompanionCoolingCoilNum = WSHPNum;
		}

	}

	void
	UpdateVarSpeedCoil( int const DXCoilNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:UpdateSimpleWSHP
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the Water to Air Heat Pump outlet nodes.

		// METHODOLOGY EMPLOYED:
		// Data is moved from the HP data structure to the HP outlet nodes.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using PlantUtilities::SafeCopyPlantNode;
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
		int AirInletNode;
		int WaterInletNode;
		int AirOutletNode;
		int WaterOutletNode;
		Real64 ReportingConstant;

		//WatertoAirHP(DXCoilNum)%SimFlag=.FALSE.
		if ( ! VarSpeedCoil( DXCoilNum ).SimFlag ) {
			// Heatpump is off; just pass through conditions
			VarSpeedCoil( DXCoilNum ).Power = 0.0;
			VarSpeedCoil( DXCoilNum ).QLoadTotal = 0.0;
			VarSpeedCoil( DXCoilNum ).QSensible = 0.0;
			VarSpeedCoil( DXCoilNum ).QLatent = 0.0;
			VarSpeedCoil( DXCoilNum ).QSource = 0.0;
			VarSpeedCoil( DXCoilNum ).Energy = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergyLoadTotal = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergySensible = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergyLatent = 0.0;
			VarSpeedCoil( DXCoilNum ).EnergySource = 0.0;
			VarSpeedCoil( DXCoilNum ).COP = 0.0;
			VarSpeedCoil( DXCoilNum ).RunFrac = 0.0;
			VarSpeedCoil( DXCoilNum ).PartLoadRatio = 0.0;

			VarSpeedCoil( DXCoilNum ).OutletAirDBTemp = VarSpeedCoil( DXCoilNum ).InletAirDBTemp;
			VarSpeedCoil( DXCoilNum ).OutletAirHumRat = VarSpeedCoil( DXCoilNum ).InletAirHumRat;
			VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy = VarSpeedCoil( DXCoilNum ).InletAirEnthalpy;
			VarSpeedCoil( DXCoilNum ).OutletWaterTemp = VarSpeedCoil( DXCoilNum ).InletWaterTemp;
			VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy = VarSpeedCoil( DXCoilNum ).InletWaterEnthalpy;
		}

		AirInletNode = VarSpeedCoil( DXCoilNum ).AirInletNodeNum;
		WaterInletNode = VarSpeedCoil( DXCoilNum ).WaterInletNodeNum;
		AirOutletNode = VarSpeedCoil( DXCoilNum ).AirOutletNodeNum;
		WaterOutletNode = VarSpeedCoil( DXCoilNum ).WaterOutletNodeNum;

		// Set the air outlet  nodes of the WatertoAirHPSimple
		Node( AirOutletNode ).MassFlowRate = Node( AirInletNode ).MassFlowRate; //LoadSideMassFlowRate
		Node( AirOutletNode ).Temp = VarSpeedCoil( DXCoilNum ).OutletAirDBTemp;
		Node( AirOutletNode ).HumRat = VarSpeedCoil( DXCoilNum ).OutletAirHumRat;
		Node( AirOutletNode ).Enthalpy = VarSpeedCoil( DXCoilNum ).OutletAirEnthalpy;

		// Set the air outlet nodes for properties that just pass through & not used
		Node( AirOutletNode ).Quality = Node( AirInletNode ).Quality;
		Node( AirOutletNode ).Press = Node( AirInletNode ).Press;
		Node( AirOutletNode ).MassFlowRateMin = Node( AirInletNode ).MassFlowRateMin;
		Node( AirOutletNode ).MassFlowRateMax = Node( AirInletNode ).MassFlowRateMax; //LoadSideMassFlowRate
		Node( AirOutletNode ).MassFlowRateMinAvail = Node( AirInletNode ).MassFlowRateMinAvail;
		Node( AirOutletNode ).MassFlowRateMaxAvail = Node( AirInletNode ).MassFlowRateMaxAvail; //LoadSideMassFlowRate

		// Set the water outlet node of the WatertoAirHPSimple
		// Set the water outlet nodes for properties that just pass through & not used
		if ( WaterInletNode != 0 && WaterOutletNode != 0 ) {
			SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
			Node( WaterOutletNode ).Temp = VarSpeedCoil( DXCoilNum ).OutletWaterTemp;
			Node( WaterOutletNode ).Enthalpy = VarSpeedCoil( DXCoilNum ).OutletWaterEnthalpy;
		}

		ReportingConstant = TimeStepSys * SecInHour;
		VarSpeedCoil( DXCoilNum ).Energy = VarSpeedCoil( DXCoilNum ).Power * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergyLoadTotal = VarSpeedCoil( DXCoilNum ).QLoadTotal * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergySensible = VarSpeedCoil( DXCoilNum ).QSensible * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergyLatent = VarSpeedCoil( DXCoilNum ).QLatent * ReportingConstant;
		VarSpeedCoil( DXCoilNum ).EnergySource = VarSpeedCoil( DXCoilNum ).QSource * ReportingConstant;

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNode ).CO2 = Node( AirInletNode ).CO2;
		}

	}

	Real64
	CalcEffectiveSHR(
		int const DXCoilNum, // Index number for cooling coil
		Real64 const SHRss, // Steady-state sensible heat ratio
		int const CyclingScheme, // Fan/compressor cycling scheme indicator
		Real64 const RTF, // Compressor run-time fraction
		Real64 const QLatRated, // Rated latent capacity
		Real64 const QLatActual, // Actual latent capacity
		Real64 const EnteringDB, // Entering air dry-bulb temperature
		Real64 const EnteringWB // Entering air wet-bulb temperature
	)
	{

		// FUNCTION INFORMATION:
		//    AUTHOR         Bo Shen, based on WaterToAirHeatPumpSimple:CalcEffectiveSHR
		//    DATE WRITTEN   March 2012
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//    Adjust sensible heat ratio to account for degradation of DX coil latent
		//    capacity at part-load (cycling) conditions.

		// METHODOLOGY EMPLOYED:
		//    With model parameters entered by the user, the part-load latent performance
		//    of a DX cooling coil is determined for a constant air flow system with
		//    a cooling coil that cycles on/off. The model calculates the time
		//    required for condensate to begin falling from the cooling coil.
		//    Runtimes greater than this are integrated to a "part-load" latent
		//    capacity which is used to determine the "part-load" sensible heat ratio.
		//    See reference below for additional details (linear decay model, Eq. 8b).

		//    For cycling fan operation, a modified version of Henderson and Rengarajan (1996)
		//    model is used by ultilizing the fan delay time as the time-off (or time duration
		//    for the re-evaporation of moisture from time coil). Refer to Tang, C.C. (2005)

		// REFERENCES:
		// na

		// Return value
		Real64 SHReff; // Effective sensible heat ratio, includes degradation due to cycling effects

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Twet; // Nominal time for condensate to begin leaving the coil's condensate drain line
		// at the current operating conditions (sec)
		Real64 Gamma; // Initial moisture evaporation rate divided by steady-state AC latent capacity
		// at the current operating conditions
		Real64 Twet_Rated; // Twet at rated conditions (coil air flow rate and air temperatures), sec
		Real64 Gamma_Rated; // Gamma at rated conditions (coil air flow rate and air temperatures)
		Real64 Twet_max; // Maximum allowed value for Twet
		Real64 MaxONOFFCyclesperHour; // Maximum cycling rate of heat pump [cycles/hr]
		Real64 HPTimeConstant; // Heat pump time constant [s]
		Real64 FanDelayTime; // Fan delay time, time delay for the HP's fan to
		// shut off after compressor cycle off  [s]
		Real64 Ton; // Coil on time (sec)
		Real64 Toff; // Coil off time (sec)
		Real64 Toffa; // Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
		Real64 aa; // Intermediate variable
		Real64 To1; // Intermediate variable (first guess at To). To = time to the start of moisture removal
		Real64 To2; // Intermediate variable (second guess at To). To = time to the start of moisture removal
		Real64 Error; // Error for iteration (DO) loop
		Real64 LHRmult; // Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

		Twet_Rated = VarSpeedCoil( DXCoilNum ).Twet_Rated;
		Gamma_Rated = VarSpeedCoil( DXCoilNum ).Gamma_Rated;
		MaxONOFFCyclesperHour = VarSpeedCoil( DXCoilNum ).MaxONOFFCyclesperHour;
		HPTimeConstant = VarSpeedCoil( DXCoilNum ).HPTimeConstant;
		FanDelayTime = VarSpeedCoil( DXCoilNum ).FanDelayTime;

		//  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
		//  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
		//  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
		if ( ( RTF >= 1.0 ) || ( QLatRated == 0.0 ) || ( QLatActual == 0.0 ) || ( Twet_Rated <= 0.0 ) || ( Gamma_Rated <= 0.0 ) || ( MaxONOFFCyclesperHour <= 0.0 ) || ( HPTimeConstant <= 0.0 ) || ( RTF <= 0.0 ) ) {
			SHReff = SHRss;
			return SHReff;
		}

		Twet_max = 9999.0; // high limit for Twet

		//  Calculate the model parameters at the actual operating conditions
		Twet = min( Twet_Rated * QLatRated / ( QLatActual + 1.e-10 ), Twet_max );
		Gamma = Gamma_Rated * QLatRated * ( EnteringDB - EnteringWB ) / ( ( 26.7 - 19.4 ) * QLatActual + 1.e-10 );

		//  Calculate the compressor on and off times using a converntional thermostat curve
		Ton = 3600.0 / ( 4.0 * MaxONOFFCyclesperHour * ( 1.0 - RTF ) ); // duration of cooling coil on-cycle (sec)

		if ( ( CyclingScheme == CycFanCycCoil ) && ( FanDelayTime != 0.0 ) ) {
			// For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
			// until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
			Toff = FanDelayTime;
		} else {
			// For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
			// for the entire heat pump off-cycle.
			Toff = 3600.0 / ( 4.0 * MaxONOFFCyclesperHour * RTF ); // duration of cooling coil off-cycle (sec)
		}

		//  Cap Toff to meet the equation restriction
		if ( Gamma > 0.0 ) {
			Toffa = min( Toff, 2.0 * Twet / Gamma );
		} else {
			Toffa = Toff;
		}

		//  Use sucessive substitution to solve for To
		aa = ( Gamma * Toffa ) - ( 0.25 / Twet ) * pow_2( Gamma ) * pow_2( Toffa );

		To1 = aa + HPTimeConstant;
		Error = 1.0;
		while ( Error > 0.001 ) {
			To2 = aa - HPTimeConstant * ( std::exp( -To1 / HPTimeConstant ) - 1.0 );
			Error = std::abs( ( To2 - To1 ) / To1 );
			To1 = To2;
		}

		//  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
		//  Floating underflow errors occur when -Ton/HPTimeConstant is a large negative number.
		//  Cap lower limit at -700 to avoid the underflow errors.
		aa = std::exp( max( -700.0, - Ton / HPTimeConstant ) );
		//  Calculate latent heat ratio multiplier
		LHRmult = max( ( ( Ton - To2 ) / ( Ton + HPTimeConstant * ( aa - 1.0 ) ) ), 0.0 );

		//  Calculate part-load or "effective" sensible heat ratio
		SHReff = 1.0 - ( 1.0 - SHRss ) * LHRmult;

		if ( SHReff < SHRss ) SHReff = SHRss; // Effective SHR can be less than the steady-state SHR
		if ( SHReff > 1.0 ) SHReff = 1.0; // Effective sensible heat ratio can't be greater than 1.0

		return SHReff;

	}

	void
	CalcTotCapSHR_VSWSHP(
		Real64 const InletDryBulb, // inlet air dry bulb temperature [C]
		Real64 const InletHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const InletEnthalpy, // inlet air specific enthalpy [J/kg]
		Real64 const InletWetBulb, // inlet air wet bulb temperature [C]
		Real64 const AirMassFlowRatio, // Ratio of actual air mass flow to nominal air mass flow
		Real64 const WaterMassFlowRatio, // Ratio of actual water mass flow to nominal water mass flow
		Real64 const AirMassFlow, // actual mass flow for capacity and SHR calculation
		Real64 const CBF, // coil bypass factor
		Real64 const TotCapNom1, // nominal total capacity at low speed [W]
		int const CCapFTemp1, // capacity modifier curve index, function of entering wetbulb at low speed
		int const CCapAirFFlow1, // capacity modifier curve, function of actual air flow vs rated flow at low speed
		int const CCapWaterFFlow1, // capacity modifier curve, function of actual water flow vs rated flow at low speed
		Real64 const TotCapNom2, // nominal total capacity at high speed [W]
		int const CCapFTemp2, // capacity modifier curve index, function of entering wetbulb at high speed
		int const CCapAirFFlow2, // capacity modifier curve, function of actual air flow vs rated flow at high speed
		int const CCapWaterFFlow2, // capacity modifier curve, function of actual water flow vs rated flow at high speed
		Real64 & TotCap1, // total capacity at the given conditions [W] at low speed
		Real64 & TotCap2, // total capacity at the given conditions [W] at high speed
		Real64 & TotCapSpeed, // integrated total capacity corresponding to the speed ratio
		Real64 & SHR, // sensible heat ratio at the given conditions
		Real64 const CondInletTemp, // Condenser inlet temperature [C]
		Real64 const Pressure, // air pressure [Pa]
		Real64 const SpeedRatio, // from 0.0 to 1.0
		int const NumSpeeds // number of speeds for input
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bo Shen, , based on DX:CalcTotCapSHR, introducing two speed levels
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates total capacity and sensible heat ratio of a DX coil at the specified conditions

		// METHODOLOGY EMPLOYED:
		// With the rated performance data entered by the user, the model employs some of the
		// DOE-2.1E curve fits to adjust the capacity and SHR of the unit as a function
		// of entering air temperatures and supply air flow rate (actual vs rated flow). The model
		// does NOT employ the exact same methodology to calculate performance as DOE-2, although
		// some of the DOE-2 curve fits are employed by this model.

		// The model checks for coil dryout conditions, and adjusts the calculated performance
		// appropriately.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit page 4-81.
		// Henderson, H.I. Jr., K. Rengarajan and D.B. Shirey, III. 1992.The impact of comfort
		// control on air conditioner energy use in humid climates. ASHRAE Transactions 98(2):
		// 104-113.
		// Henderson, H.I. Jr., Danny Parker and Y.J. Huang. 2000.Improving DOE-2's RESYS routine:
		// User Defined Functions to Provide More Accurate Part Load Energy Use and Humidity
		// Predictions. Proceedings of ACEEE Conference.

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcTotCapSHR_VSWSHP" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 InletWetBulbCalc; // calculated inlet wetbulb temperature used for finding dry coil point [C]
		Real64 InletHumRatCalc; // calculated inlet humidity ratio used for finding dry coil point [kg water / kg dry air]
		Real64 TotCapTempModFac1;
		// Total capacity modifier (function of entering wetbulb, outside water inlet temp) at low speed
		Real64 TotCapAirFlowModFac1;
		// Total capacity modifier (function of actual supply air flow vs nominal flow) at low speed
		Real64 TotCapWaterFlowModFac1;
		// Total capacity modifier (function of actual supply water flow vs nominal flow) at low speed
		Real64 TotCapTempModFac2;
		// Total capacity modifier (function of entering wetbulb, outside water inlet temp) at high speed
		Real64 TotCapAirFlowModFac2;
		// Total capacity modifier (function of actual supply air flow vs nominal flow) at high speed
		Real64 TotCapWaterFlowModFac2;
		// Total capacity modifier (function of actual supply water flow vs nominal flow) at high speed
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 SHRCalc; // temporary calculated value of SHR
		Real64 TotCapCalc; // temporary calculated value of total capacity [W]
		Real64 TotCapCalc1; // temporary calculated value of total capacity [W] at low speed
		Real64 TotCapCalc2; // temporary calculated value of total capacity [W] at high speed
		int Counter; // Counter for dry evaporator iterations
		int MaxIter; // Maximum number of iterations for dry evaporator calculations
		Real64 RF; // Relaxation factor for dry evaporator iterations
		Real64 Tolerance; // Error tolerance for dry evaporator iterations
		Real64 werror; // Deviation of humidity ratio in dry evaporator iteration loop
		static bool LoopOn( true ); // flag to control the loop iteration

		MaxIter = 30;
		RF = 0.4;
		Counter = 0;
		Tolerance = 0.01;
		werror = 0.0;

		InletWetBulbCalc = InletWetBulb;
		InletHumRatCalc = InletHumRat;
		LoopOn = true;

		//  DO WHILE (ABS(werror) .gt. Tolerance .OR. Counter == 0)
		//   Get capacity modifying factor (function of inlet wetbulb & outside drybulb) for off-rated conditions
		while ( LoopOn ) {
			TotCapTempModFac1 = CurveValue( CCapFTemp1, InletWetBulbCalc, CondInletTemp );
			//   Get capacity modifying factor (function of mass flow) for off-rated conditions
			TotCapAirFlowModFac1 = CurveValue( CCapAirFFlow1, AirMassFlowRatio );
			//Get capacity modifying factor (function of mass flow) for off-rated conditions
			if ( CCapWaterFFlow1 == 0 ) {
				TotCapWaterFlowModFac1 = 1.0;
			} else {
				TotCapWaterFlowModFac1 = CurveValue( CCapWaterFFlow1, WaterMassFlowRatio );
			}

			//   Get total capacity
			if ( NumSpeeds < 2 ) { //ONLY ONE SPEED
				TotCapCalc = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1;
				TotCapCalc1 = TotCapCalc;
				TotCapCalc2 = 0.0;
			} else {
				TotCapTempModFac2 = CurveValue( CCapFTemp2, InletWetBulbCalc, CondInletTemp );
				TotCapAirFlowModFac2 = CurveValue( CCapAirFFlow2, AirMassFlowRatio );

				if ( CCapWaterFFlow2 == 0 ) {
					TotCapWaterFlowModFac2 = 1.0;
				} else {
					TotCapWaterFlowModFac2 = CurveValue( CCapWaterFFlow2, WaterMassFlowRatio );
				}

				TotCapCalc1 = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1;
				TotCapCalc2 = TotCapNom2 * TotCapAirFlowModFac2 * TotCapWaterFlowModFac2 * TotCapTempModFac2;

				TotCapCalc = TotCapCalc2 * SpeedRatio + ( 1.0 - SpeedRatio ) * TotCapCalc1;

			}

			if (CBF < 0.001) {
				SHRCalc = 1.0; 
				LoopOn = false;
			}
			else{
				//   Calculate apparatus dew point conditions using TotCap and CBF
				hDelta = TotCapCalc / AirMassFlow;
				hADP = InletEnthalpy - hDelta / (1.0 - CBF);
				tADP = PsyTsatFnHPb(hADP, Pressure);
				wADP = PsyWFnTdbH(tADP, hADP);
				hTinwADP = PsyHFnTdbW(InletDryBulb, wADP);
				SHRCalc = min((hTinwADP - hADP) / (InletEnthalpy - hADP), 1.0);
				//   Check for dry evaporator conditions (win < wadp)
				if (wADP > InletHumRatCalc || (Counter >= 1 && Counter < MaxIter)) {
					if (InletHumRatCalc == 0.0) InletHumRatCalc = 0.00001;
					werror = (InletHumRatCalc - wADP) / InletHumRatCalc;
					//     Increase InletHumRatCalc at constant inlet air temp to find coil dry-out point. Then use the
					//     capacity at the dry-out point to determine exiting conditions from coil. This is required
					//     since the TotCapTempModFac doesn't work properly with dry-coil conditions.
					InletHumRatCalc = RF * wADP + (1.0 - RF) * InletHumRatCalc;
					InletWetBulbCalc = PsyTwbFnTdbWPb(InletDryBulb, InletHumRatCalc, Pressure);
					++Counter;
					if (std::abs(werror) > Tolerance) {
						LoopOn = true; //go to 50   ! Recalculate with modified inlet conditions
					}
					else {
						LoopOn = false;
					}
				}
				else {
					LoopOn = false;
				}
			}
		}

		// END DO

		//  Calculate full load output conditions
		if ( SHRCalc > 1.0 || Counter > 0 ) SHRCalc = 1.0;

		SHR = SHRCalc;
		TotCap1 = TotCapCalc1;
		TotCap2 = TotCapCalc2;
		TotCapSpeed = TotCapCalc;

		// IF(SHR < 0.3d0) SHR = 0.3d0

	}

	Real64
	AdjustCBF(
		Real64 const CBFNom, // nominal coil bypass factor
		Real64 const AirMassFlowRateNom, // nominal air mass flow rate [kg/s]
		Real64 const AirMassFlowRate // actual air mass flow rate [kg/s]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl using Don Shirey's code
		//       DATE WRITTEN   September 2002
		//       Modified       BOs March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//    Adjust coil bypass factor for actual air flow rate.

		// METHODOLOGY EMPLOYED:
		// Uses relation CBF = exp(-NTU) whereNTU = A0/(m*cp). Relationship models the cooling coil
		// as a heat exchanger with Cmin/Cmax = 0.
		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 CBFAdj; // the result - the adjusted coil bypass factor

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 A0; // intermediate variable
		Real64 ADiff; // intermediate variable

		if ( CBFNom > 0.0 ) {
			A0 = -std::log( CBFNom ) * AirMassFlowRateNom;
		} else {
			A0 = 0.0;
		}
		ADiff = -A0 / AirMassFlowRate;
		if ( ADiff >= EXP_LowerLimit ) {
			CBFAdj = std::exp( ADiff );
		} else {
			CBFAdj = 1.0e-6;
		}

		return CBFAdj;
	}

	Real64
	CalcCBF(
		std::string const & UnitType,
		std::string const & UnitName,
		Real64 const InletAirTemp, // inlet air temperature [C]
		Real64 const InletAirHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const TotCap, // total cooling  capacity [Watts]
		Real64 const AirMassFlowRate, // the air mass flow rate at the given capacity [kg/s]
		Real64 const SHR // sensible heat ratio at the given capacity and flow rate
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl using Don Shirey's code
		//       DATE WRITTEN   September 2002
		//       Modified by BOS Mark 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the coil bypass factor for a coil given the total capacity at the entering conditions,
		// air mass flow rate at the entering conditions, and the sensible heat ratio (SHR) at the
		// entering conditions.

		// METHODOLOGY EMPLOYED:
		// calculate SlopeRated (deltahumrat/deltaT) using rated unit information provided by
		// user. Then hunt along saturation curve of psychrometric chart until the slope of the line
		// between the saturation point and rated inlet air humidity ratio and T is the same as SlopeRated.
		// When the slopes are equal, then we have located the apparatus dewpoint of the coil at rated
		// conditions. From this information, coil bypass factor is calculated.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Return value
		Real64 CBF; // the result - the coil bypass factor

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcCBF" );
		static Real64 SmallDifferenceTest( 0.00000001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 InletAirEnthalpy; // Enthalpy of inlet air to evaporator at given conditions [J/kg]
		Real64 DeltaH(0.0); // Enthalpy drop across evaporator at given conditions [J/kg]
		Real64 DeltaT(0.0); // Temperature drop across evaporator at given conditions [C]
		Real64 DeltaHumRat(0.0); // Humidity ratio drop across evaporator at given conditions [kg/kg]
		Real64 OutletAirTemp(0.0); // Outlet dry-bulb temperature from evaporator at given conditions [C]
		Real64 OutletAirEnthalpy(0.0); // Enthalpy of outlet air at given conditions [J/kg]
		Real64 OutletAirHumRat(0.0); // Outlet humidity ratio from evaporator at given conditions [kg/kg]
		Real64 OutletAirRH(0.0); // relative humidity of the outlet air
		Real64 Error; // Error term used in given coil bypass factor (CBF) calculations
		Real64 ErrorLast; // Error term, from previous iteration
		int Iter; // Iteration loop counter in CBF calculations
		int IterMax(50); // Maximum number of iterations in CBF calculations
		Real64 ADPTemp; // Apparatus dewpoint temperature used in CBF calculations [C]
		Real64 ADPHumRat; // Apparatus dewpoint humidity used in CBF calculations [kg/kg]
		Real64 ADPEnthalpy; // Air enthalpy at apparatus dew point [J/kg]
		Real64 DeltaADPTemp; // Change in Apparatus Dew Point used in CBF calculations [C]
		Real64 SlopeAtConds(0.0); // Slope (DeltaHumRat/DeltaT) at given conditions
		Real64 Slope(0.0); // Calculated Slope used while hunting for Tadp
		Real64 Tolerance; // Convergence tolerance for CBF calculations
		Real64 HTinHumRatOut; // Air enthalpy at inlet air temp and outlet air humidity ratio [J/kg]
		static bool CBFErrors( false ); // Set to true if errors in CBF calculation, fatal at end of routine

		OutletAirTemp = InletAirTemp;
		OutletAirHumRat = InletAirHumRat;

		DeltaH = TotCap / AirMassFlowRate;
		InletAirEnthalpy = PsyHFnTdbW( InletAirTemp, InletAirHumRat );
		HTinHumRatOut = InletAirEnthalpy - ( 1.0 - SHR ) * DeltaH;
		OutletAirHumRat = PsyWFnTdbH( InletAirTemp, HTinHumRatOut );
		DeltaHumRat = InletAirHumRat - OutletAirHumRat;
		OutletAirEnthalpy = InletAirEnthalpy - DeltaH;
		OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//  Pressure will have to be pass into this subroutine to fix this one
		OutletAirRH = PsyRhFnTdbWPb( OutletAirTemp, OutletAirHumRat, StdBaroPress, RoutineName );
		if ( OutletAirRH >= 1.0 ) {
			ShowSevereError( "For object = " + UnitType + ", name = \"" + UnitName + "\"" );
			ShowContinueError( "Calculated outlet air relative humidity greater than 1. The combination of" );
			ShowContinueError( "rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting" );
			ShowContinueError( "air conditions above the saturation curve. Possible fixes are to reduce the rated total cooling" );
			ShowContinueError( "capacity, increase the rated air volume flow rate, or reduce the rated sensible heat ratio for this coil." );
			ShowContinueError( "If autosizing, it is recommended that all three of these values be autosized." );
			ShowContinueError( "...Inputs used for calculating cooling coil bypass factor." );
			ShowContinueError( "...Inlet Air Temperature     = " + RoundSigDigits( InletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Outlet Air Temperature    = " + RoundSigDigits( OutletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Inlet Air Humidity Ratio  = " + RoundSigDigits( InletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Outlet Air Humidity Ratio = " + RoundSigDigits( OutletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Total Cooling Capacity used in calculation = " + RoundSigDigits( TotCap, 2 ) + " W" );
			ShowContinueError( "...Air Mass Flow Rate used in calculation     = " + RoundSigDigits( AirMassFlowRate, 6 ) + " kg/s" );
			ShowContinueError( "...Air Volume Flow Rate used in calculation   = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ), 6 ) + " m3/s" );
			if ( TotCap > 0.0 ) {
				if ( ( ( MinRatedVolFlowPerRatedTotCap - AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap ) > SmallDifferenceTest ) || ( ( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap - MaxRatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) ) {
					ShowContinueError( "...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap, 7 ) + " m3/s/W" );
				}
			}
			ShowContinueErrorTimeStamp( "" );
			ShowFatalError( "Check and revise the input data for this coil before rerunning the simulation." );
		}
		DeltaT = InletAirTemp - OutletAirTemp;
		if ( DeltaT <= 0.0 ) {
			ShowSevereError( "For object = " + UnitType + ", name = \"" + UnitName + "\"" );
			ShowContinueError( "Calculated coil delta T is less than or equal to 0. The combination of" );
			ShowContinueError( "rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting" );
			ShowContinueError( "air conditions that are not reasonable. Possible fixes are to adjust the rated total cooling" );
			ShowContinueError( "capacity, rated air volume flow rate, or rated sensible heat ratio for this coil." );
			ShowContinueError( "If autosizing, it is recommended that all three of these values be autosized." );
			ShowContinueError( "...Inputs used for calculating cooling coil bypass factor." );
			ShowContinueError( "...Inlet Air Temperature     = " + RoundSigDigits( InletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Outlet Air Temperature    = " + RoundSigDigits( OutletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Inlet Air Humidity Ratio  = " + RoundSigDigits( InletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Outlet Air Humidity Ratio = " + RoundSigDigits( OutletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Total Cooling Capacity used in calculation = " + RoundSigDigits( TotCap, 2 ) + " W" );
			ShowContinueError( "...Air Mass Flow Rate used in calculation     = " + RoundSigDigits( AirMassFlowRate, 6 ) + " kg/s" );
			ShowContinueError( "...Air Volume Flow Rate used in calculation   = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ), 6 ) + " m3/s" );
			if ( TotCap > 0.0 ) {
				if ( ( ( MinRatedVolFlowPerRatedTotCap - AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap ) > SmallDifferenceTest ) || ( ( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap - MaxRatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) ) {
					ShowContinueError( "...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap, 7 ) + " m3/s/W" );
				}
			}
			ShowContinueErrorTimeStamp( "" );
			ShowFatalError( "Check and revise the input data for this coil before rerunning the simulation." );
		}
		// Calculate slope at given conditions
		if ( DeltaT > 0.0 ) SlopeAtConds = DeltaHumRat / DeltaT;

		//  IF (SlopeAtConds .le. .0000001d0 .or. OutletAirHumRat .le. 0.0d0) THEN
		if ( SlopeAtConds < 0.0 || OutletAirHumRat <= 0.0 ) {
			//   Invalid conditions, slope can't be less than zero (SHR > 1) or
			//   outlet air humidity ratio can't be less than zero.
			ShowSevereError( UnitType + " \"" + UnitName + "\"" );
			ShowContinueError( "...Invalid slope or outlet air condition when calculating cooling coil bypass factor." );
			ShowContinueError( "...Slope = " + RoundSigDigits( SlopeAtConds, 8 ) );
			ShowContinueError( "...Inlet Air Temperature     = " + RoundSigDigits( InletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Outlet Air Temperature    = " + RoundSigDigits( OutletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Inlet Air Humidity Ratio  = " + RoundSigDigits( InletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Outlet Air Humidity Ratio = " + RoundSigDigits( OutletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Total Cooling Capacity used in calculation = " + RoundSigDigits( TotCap, 2 ) + " W" );
			ShowContinueError( "...Air Mass Flow Rate used in calculation     = " + RoundSigDigits( AirMassFlowRate, 6 ) + " kg/s" );
			ShowContinueError( "...Air Volume Flow Rate used in calculation   = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ), 6 ) + " m3/s" );
			if ( TotCap > 0.0 ) {
				if ( ( ( MinRatedVolFlowPerRatedTotCap - AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap ) > SmallDifferenceTest ) || ( ( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap - MaxRatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) ) {
					ShowContinueError( "...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( StdBaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap, 7 ) + " m3/s/W" );
				}
			}
			ShowContinueErrorTimeStamp( "" );
			CBFErrors = true;
			CBF = 0.0; //Autodesk:Return Suppress static testing from complaining about return value not set
		} else {

			//   First guess for Tadp is outlet air dew point
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  Pressure will have to be pass into this subroutine to fix this one
			ADPTemp = PsyTdpFnWPb( OutletAirHumRat, StdBaroPress );

			Tolerance = 1.0; // initial conditions for iteration
			ErrorLast = 100.0;
			Iter = 0;
			DeltaADPTemp = 5.0;
			while ( ( Iter <= IterMax ) && ( Tolerance > 0.001 ) ) {
				//     Do for IterMax iterations or until the error gets below .1%
				if ( Iter > 0 ) ADPTemp += DeltaADPTemp;
				++Iter;

				//     Find new slope using guessed Tadp

				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  Pressure will have to be pass into this subroutine to fix this one
				ADPHumRat = PsyWFnTdpPb( ADPTemp, StdBaroPress );
				Slope = ( InletAirHumRat - ADPHumRat ) / ( InletAirTemp - ADPTemp );

				//     check for convergence (slopes are equal to within error tolerance)

				Error = ( Slope - SlopeAtConds ) / SlopeAtConds;
				if ( ( Error > 0.0 ) && ( ErrorLast < 0.0 ) ) DeltaADPTemp = -DeltaADPTemp / 2.0;
				if ( ( Error < 0.0 ) && ( ErrorLast > 0.0 ) ) DeltaADPTemp = -DeltaADPTemp / 2.0;
				ErrorLast = Error;

				Tolerance = std::abs( Error );

			}

			//   Calculate Bypass Factor from Enthalpies

			InletAirEnthalpy = PsyHFnTdbW( InletAirTemp, InletAirHumRat );
			OutletAirEnthalpy = PsyHFnTdbW( OutletAirTemp, OutletAirHumRat );
			ADPEnthalpy = PsyHFnTdbW( ADPTemp, ADPHumRat );
			CBF = ( OutletAirEnthalpy - ADPEnthalpy ) / ( InletAirEnthalpy - ADPEnthalpy );
			if ( Iter > IterMax ) {
				ShowSevereError( UnitType + " \"" + UnitName + "\" -- coil bypass factor calculation did not converge after max iterations." );
				ShowContinueError( "The RatedSHR of [" + RoundSigDigits( SHR, 3 ) + "], entered by the user or autosized (see *.eio file)," );
				ShowContinueError( "may be causing this. The line defined by the coil rated inlet air conditions" );
				ShowContinueError( "(26.7C drybulb and 19.4C wetbulb) and the RatedSHR (i.e., slope of the line) must intersect" );
				ShowContinueError( "the saturation curve of the psychrometric chart. If the RatedSHR is too low, then this" );
				ShowContinueError( "intersection may not occur and the coil bypass factor calculation will not converge." );
				ShowContinueError( "If autosizing the SHR, recheck the design supply air humidity ratio and design supply air" );
				ShowContinueError( "temperature values in the Sizing:System and Sizing:Zone objects. In general, the temperatures" );
				ShowContinueError( "and humidity ratios specified in these two objects should be the same for each system" );
				ShowContinueError( "and the zones that it serves." );
				ShowContinueErrorTimeStamp( "" );
				CBFErrors = true; // Didn't converge within MaxIter iterations
			}
			if ( CBF < 0.0 ) {
				ShowSevereError( UnitType + " \"" + UnitName + "\" -- negative coil bypass factor calculated." );
				ShowContinueErrorTimeStamp( "" );
				CBFErrors = true; // Negative CBF not valid
			}
		}

		// Show fatal error for specific coil that caused a CBF error
		if ( CBFErrors ) {
			ShowFatalError( UnitType + " \"" + UnitName + "\" Errors found in calculating coil bypass factors" );
		}

		return CBF;
	}

} // VariableSpeedCoils

} // EnergyPlus
