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
#include <WaterToAirHeatPump.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataContaminantBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace WaterToAirHeatPump {
	// Module containing the Water to Air Heat Pump simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Hui Jin
	//       DATE WRITTEN   Oct 2000
	//       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
	//                      Brent Griffith, plant upgrades, fluid props
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the Water to Air Heat Pump Component

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// Jin, H. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd Thesis.
	// Oklahoma State University.

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using DataHVACGlobals::CycFanCycCoil;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::TimeStepSys;
	using DataPlant::TypeOf_CoilWAHPCoolingParamEst;
	using DataPlant::TypeOf_CoilWAHPHeatingParamEst;
	using DataPlant::PlantLoop;

	// Use statements for access to subroutines in other modules

	// Data
	//MODULE PARAMETER DEFINITIONS
	int const CompressorType_Reciprocating( 1 );
	int const CompressorType_Rotary( 2 );
	int const CompressorType_Scroll( 3 );

	static std::string const fluidNameWater( "WATER" );
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// Output Variables Type definition

	//MODULE VARIABLE DECLARATIONS:
	int NumWatertoAirHPs( 0 ); // The Number of Water to Air Heat Pumps found in the Input
	Array1D_bool CheckEquipName;

	int RefrigIndex( 0 ); // Refrigerant index
	int WaterIndex( 0 ); // Water index
	bool GetCoilsInputFlag( true ); // Flag set to make sure you get input once
	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Computational routines

	// Update routine to check convergence and update nodes

	// Utility routines

	// Object Data
	Array1D< WatertoAirHPEquipConditions > WatertoAirHP;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SimWatertoAirHP(
		std::string const & CompName, // component name
		int & CompIndex, // Index for Component name
		Real64 const DesignAirflow, // design air flow rate
		int const CyclingScheme, // cycling scheme--either continuous fan/cycling compressor or
		bool const FirstHVACIteration, // first iteration flag
		Real64 const RuntimeFrac, // compressor run time fraction
		Real64 & MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 & HPTimeConstant, // Heat pump time constant [s]
		Real64 & FanDelayTime, // Fan delay time, time delay for the HP's fan to
		bool const InitFlag, // initialization flag used to suppress property routine errors
		Real64 const SensLoad, // sensible load
		Real64 const LatentLoad, // latent load
		int const CompOp,
		Real64 const PartLoadRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hui Jin
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Water to Air Heat Pump component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using FluidProperties::FindGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//shut off after compressor cycle off  [s]
		//cycling fan/cycling compressor

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HPNum; // The WatertoAirHP that you are currently loading input into

		// FLOW:

		// Obtains and Allocates WatertoAirHP related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			WaterIndex = FindGlycol( fluidNameWater ); //Initialize the WaterIndex once
			GetWatertoAirHPInput();
			GetCoilsInputFlag = false;
		}

		if ( CompIndex == 0 ) {
			HPNum = FindItemInList( CompName, WatertoAirHP );
			if ( HPNum == 0 ) {
				ShowFatalError( "WaterToAir HP not found=" + CompName );
			}
			CompIndex = HPNum;
		} else {
			HPNum = CompIndex;
			if ( HPNum > NumWatertoAirHPs || HPNum < 1 ) {
				ShowFatalError( "SimWatertoAirHP: Invalid CompIndex passed=" + TrimSigDigits( HPNum ) + ", Number of Water to Air HPs=" + TrimSigDigits( NumWatertoAirHPs ) + ", WaterToAir HP name=" + CompName );
			}
			if ( CheckEquipName( HPNum ) ) {
				if ( ! CompName.empty() && CompName != WatertoAirHP( HPNum ).Name ) {
					ShowFatalError( "SimWatertoAirHP: Invalid CompIndex passed=" + TrimSigDigits( HPNum ) + ", WaterToAir HP name=" + CompName + ", stored WaterToAir HP Name for that index=" + WatertoAirHP( HPNum ).Name );
				}
				CheckEquipName( HPNum ) = false;
			}
		}
		// Calculate the Correct Water to Air HP Model with the current HPNum

		if ( WatertoAirHP( HPNum ).WAHPPlantTypeOfNum == TypeOf_CoilWAHPCoolingParamEst ) {
			InitWatertoAirHP( HPNum, InitFlag, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio );
			CalcWatertoAirHPCooling( HPNum, CyclingScheme, FirstHVACIteration, RuntimeFrac, InitFlag, SensLoad, CompOp, PartLoadRatio );

			UpdateWatertoAirHP( HPNum );

		} else if ( WatertoAirHP( HPNum ).WAHPPlantTypeOfNum == TypeOf_CoilWAHPHeatingParamEst ) {
			InitWatertoAirHP( HPNum, InitFlag, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio );
			CalcWatertoAirHPHeating( HPNum, CyclingScheme, FirstHVACIteration, RuntimeFrac, InitFlag, SensLoad, CompOp, PartLoadRatio );

			UpdateWatertoAirHP( HPNum );

		} else {
			ShowFatalError( "SimWatertoAirHP: AirtoAir heatpump not in either HEATING or COOLING" );
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetWatertoAirHPInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hui Jin
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for HPs and stores it in HP data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using namespace NodeInputManager;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::CheckFluidPropertyName;
		using FluidProperties::FindGlycol;
		using GlobalNames::VerifyUniqueCoilName;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using namespace OutputReportPredefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetWatertoAirHPInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HPNum; // The Water to Air HP that you are currently loading input into
		int NumCool;
		int NumHeat;
		int WatertoAirHPNum;
		int NumAlphas;
		int NumParams;
		int NumNums;
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// FLOW

		NumCool = GetNumObjectsFound( "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation" );
		NumHeat = GetNumObjectsFound( "Coil:Heating:WaterToAirHeatPump:ParameterEstimation" );
		NumWatertoAirHPs = NumCool + NumHeat;
		HPNum = 0;

		if ( NumWatertoAirHPs <= 0 ) {
			ShowSevereError( "No Equipment found in SimWatertoAirHP" );
			ErrorsFound = true;
		}

		// Allocate Arrays
		if ( NumWatertoAirHPs > 0 ) {
			WatertoAirHP.allocate( NumWatertoAirHPs );
			CheckEquipName.dimension( NumWatertoAirHPs, true );
		}

		GetObjectDefMaxArgs( "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation", NumParams, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Heating:WaterToAirHeatPump:ParameterEstimation", NumParams, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		AlphArray.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		lAlphaBlanks.dimension( MaxAlphas, true );
		cNumericFields.allocate( MaxNums );
		lNumericBlanks.dimension( MaxNums, true );
		NumArray.dimension( MaxNums, 0.0 );

		// Get the data for detailed cooling Heat Pump
		CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation";

		for ( WatertoAirHPNum = 1; WatertoAirHPNum <= NumCool; ++WatertoAirHPNum ) {

			++HPNum;

			GetObjectItem( CurrentModuleObject, HPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;

			VerifyName( AlphArray( 1 ), WatertoAirHP, HPNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			WatertoAirHP( HPNum ).Name = AlphArray( 1 );
			WatertoAirHP( HPNum ).WatertoAirHPType = "COOLING";
			WatertoAirHP( HPNum ).WAHPPlantTypeOfNum = TypeOf_CoilWAHPCoolingParamEst;
			WatertoAirHP( HPNum ).Refrigerant = AlphArray( 3 );
			WatertoAirHP( HPNum ).DesignWaterVolFlowRate = NumArray( 1 );
			WatertoAirHP( HPNum ).CoolingCapacity = NumArray( 2 );
			WatertoAirHP( HPNum ).Twet_Rated = NumArray( 3 );
			WatertoAirHP( HPNum ).Gamma_Rated = NumArray( 4 );

			WatertoAirHP( HPNum ).HighPressCutoff = NumArray( 5 );
			WatertoAirHP( HPNum ).LowPressCutoff = NumArray( 6 );

			WatertoAirHP( HPNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			WatertoAirHP( HPNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			WatertoAirHP( HPNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			WatertoAirHP( HPNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 7 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			//2010-01-13 ESL: Jason Glazer noted that these were out of order previously, but they are good now
			WatertoAirHP( HPNum ).LoadSideTotalUACoeff = NumArray( 7 );
			WatertoAirHP( HPNum ).LoadSideOutsideUACoeff = NumArray( 8 );

			if ( ( WatertoAirHP( HPNum ).LoadSideOutsideUACoeff < rTinyValue ) || ( WatertoAirHP( HPNum ).LoadSideTotalUACoeff < rTinyValue ) ) {
				ShowSevereError( "Input problem for " + CurrentModuleObject + '=' + WatertoAirHP( HPNum ).Name );
				ShowContinueError( " One or both load side UA values entered are below tolerance, likely zero or blank." );
				ShowContinueError( " Verify inputs, as the parameter syntax for this object went through a change with" );
				ShowContinueError( "  the release of EnergyPlus version 5." );
				ErrorsFound = true;
			}

			WatertoAirHP( HPNum ).SuperheatTemp = NumArray( 9 );
			WatertoAirHP( HPNum ).PowerLosses = NumArray( 10 );
			WatertoAirHP( HPNum ).LossFactor = NumArray( 11 );

			{ auto const SELECT_CASE_var( AlphArray( 2 ) );

			if ( SELECT_CASE_var == "RECIPROCATING" ) {
				WatertoAirHP( HPNum ).CompressorType = CompressorType_Reciprocating;
				WatertoAirHP( HPNum ).CompPistonDisp = NumArray( 12 );
				WatertoAirHP( HPNum ).CompSucPressDrop = NumArray( 13 );
				WatertoAirHP( HPNum ).CompClearanceFactor = NumArray( 14 );

			} else if ( SELECT_CASE_var == "ROTARY" ) {
				WatertoAirHP( HPNum ).CompressorType = CompressorType_Rotary;
				WatertoAirHP( HPNum ).CompPistonDisp = NumArray( 12 );
				WatertoAirHP( HPNum ).CompSucPressDrop = NumArray( 13 );

			} else if ( SELECT_CASE_var == "SCROLL" ) {
				WatertoAirHP( HPNum ).CompressorType = CompressorType_Scroll;
				WatertoAirHP( HPNum ).RefVolFlowRate = NumArray( 15 );
				WatertoAirHP( HPNum ).VolumeRatio = NumArray( 16 );
				WatertoAirHP( HPNum ).LeakRateCoeff = NumArray( 17 );

			} else {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 2 ) + " (" + AlphArray( 2 ) + ") entered." + CurrentModuleObject + '=' + WatertoAirHP( HPNum ).Name );
				ErrorsFound = true;

			}}

			WatertoAirHP( HPNum ).SourceSideUACoeff = NumArray( 18 );
			WatertoAirHP( HPNum ).SourceSideHTR1 = NumArray( 19 );
			WatertoAirHP( HPNum ).SourceSideHTR2 = NumArray( 20 );

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Water Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 6 ), AlphArray( 7 ), "Air Nodes" );

			// Setup Report variables for the detailed cooling Heat Pump
			// CurrentModuleObject = "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation"
			SetupOutputVariable( "Cooling Coil Electric Energy [J]", WatertoAirHP( HPNum ).Energy, "System", "Summed", WatertoAirHP( HPNum ).Name, _, "Electric", "Cooling", _, "System" );
			SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", WatertoAirHP( HPNum ).EnergyLoadTotal, "System", "Summed", WatertoAirHP( HPNum ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", WatertoAirHP( HPNum ).EnergySensible, "System", "Summed", WatertoAirHP( HPNum ).Name );
			SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", WatertoAirHP( HPNum ).EnergyLatent, "System", "Summed", WatertoAirHP( HPNum ).Name );
			SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Energy [J]", WatertoAirHP( HPNum ).EnergySource, "System", "Summed", WatertoAirHP( HPNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );

			// save the design source side flow rate for use by plant loop sizing algorithms
			RegisterPlantCompDesignFlow( WatertoAirHP( HPNum ).WaterInletNodeNum, 0.5 * WatertoAirHP( HPNum ).DesignWaterVolFlowRate );

			//create predefined report entries
			PreDefTableEntry( pdchCoolCoilType, WatertoAirHP( HPNum ).Name, CurrentModuleObject );
			PreDefTableEntry( pdchCoolCoilTotCap, WatertoAirHP( HPNum ).Name, WatertoAirHP( HPNum ).CoolingCapacity );
			PreDefTableEntry( pdchCoolCoilSensCap, WatertoAirHP( HPNum ).Name, "-" );
			PreDefTableEntry( pdchCoolCoilLatCap, WatertoAirHP( HPNum ).Name, "-" );
			PreDefTableEntry( pdchCoolCoilSHR, WatertoAirHP( HPNum ).Name, "-" );
			PreDefTableEntry( pdchCoolCoilNomEff, WatertoAirHP( HPNum ).Name, "-" );

		}

		CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:ParameterEstimation";

		for ( WatertoAirHPNum = 1; WatertoAirHPNum <= NumHeat; ++WatertoAirHPNum ) {

			++HPNum;

			GetObjectItem( CurrentModuleObject, WatertoAirHPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;

			VerifyName( AlphArray( 1 ), WatertoAirHP, HPNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			WatertoAirHP( HPNum ).Name = AlphArray( 1 );
			WatertoAirHP( HPNum ).WatertoAirHPType = "HEATING";
			WatertoAirHP( HPNum ).WAHPPlantTypeOfNum = TypeOf_CoilWAHPHeatingParamEst;
			WatertoAirHP( HPNum ).Refrigerant = AlphArray( 3 );
			WatertoAirHP( HPNum ).DesignWaterVolFlowRate = NumArray( 1 );
			WatertoAirHP( HPNum ).HeatingCapacity = NumArray( 2 );

			WatertoAirHP( HPNum ).HighPressCutoff = NumArray( 3 );
			WatertoAirHP( HPNum ).LowPressCutoff = NumArray( 4 );

			WatertoAirHP( HPNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			WatertoAirHP( HPNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			WatertoAirHP( HPNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			WatertoAirHP( HPNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 7 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			WatertoAirHP( HPNum ).LoadSideTotalUACoeff = NumArray( 5 );
			if ( WatertoAirHP( HPNum ).LoadSideTotalUACoeff < rTinyValue ) {
				ShowSevereError( "Input problem for " + CurrentModuleObject + '=' + WatertoAirHP( HPNum ).Name );
				ShowContinueError( " Load side UA value is less than tolerance, likely zero or blank." );
				ShowContinueError( " Verify inputs, as the parameter syntax for this object went through a change with" );
				ShowContinueError( "  the release of EnergyPlus version 5." );
				ErrorsFound = true;
			}

			WatertoAirHP( HPNum ).SuperheatTemp = NumArray( 6 );
			WatertoAirHP( HPNum ).PowerLosses = NumArray( 7 );
			WatertoAirHP( HPNum ).LossFactor = NumArray( 8 );

			{ auto const SELECT_CASE_var( AlphArray( 2 ) );

			if ( SELECT_CASE_var == "RECIPROCATING" ) {
				WatertoAirHP( HPNum ).CompressorType = CompressorType_Reciprocating;
				WatertoAirHP( HPNum ).CompPistonDisp = NumArray( 9 );
				WatertoAirHP( HPNum ).CompSucPressDrop = NumArray( 10 );
				WatertoAirHP( HPNum ).CompClearanceFactor = NumArray( 11 );

			} else if ( SELECT_CASE_var == "ROTARY" ) {
				WatertoAirHP( HPNum ).CompressorType = CompressorType_Rotary;
				WatertoAirHP( HPNum ).CompPistonDisp = NumArray( 9 );
				WatertoAirHP( HPNum ).CompSucPressDrop = NumArray( 10 );

			} else if ( SELECT_CASE_var == "SCROLL" ) {
				WatertoAirHP( HPNum ).CompressorType = CompressorType_Scroll;
				WatertoAirHP( HPNum ).RefVolFlowRate = NumArray( 12 );
				WatertoAirHP( HPNum ).VolumeRatio = NumArray( 13 );
				WatertoAirHP( HPNum ).LeakRateCoeff = NumArray( 14 );

			} else {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 2 ) + " (" + AlphArray( 2 ) + ") entered." + CurrentModuleObject + '=' + WatertoAirHP( HPNum ).Name );
				ErrorsFound = true;

			}}

			WatertoAirHP( HPNum ).SourceSideUACoeff = NumArray( 15 );
			WatertoAirHP( HPNum ).SourceSideHTR1 = NumArray( 16 );
			WatertoAirHP( HPNum ).SourceSideHTR2 = NumArray( 17 );

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Water Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 6 ), AlphArray( 7 ), "Air Nodes" );

			// CurrentModuleObject = "Coil:Heating:WaterToAirHeatPump:ParameterEstimation"
			SetupOutputVariable( "Heating Coil Electric Energy [J]", WatertoAirHP( HPNum ).Energy, "System", "Summed", WatertoAirHP( HPNum ).Name, _, "Electric", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Heating Energy [J]", WatertoAirHP( HPNum ).EnergyLoadTotal, "System", "Summed", WatertoAirHP( HPNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Source Side Heat Transfer Energy [J]", WatertoAirHP( HPNum ).EnergySource, "System", "Summed", WatertoAirHP( HPNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "HEATINGCOILS", _, "System" );

			// save the design source side flow rate for use by plant loop sizing algorithms
			RegisterPlantCompDesignFlow( WatertoAirHP( HPNum ).WaterInletNodeNum, 0.5 * WatertoAirHP( HPNum ).DesignWaterVolFlowRate );

			//create predefined report entries
			PreDefTableEntry( pdchHeatCoilType, WatertoAirHP( HPNum ).Name, CurrentModuleObject );
			PreDefTableEntry( pdchHeatCoilNomCap, WatertoAirHP( HPNum ).Name, WatertoAirHP( HPNum ).HeatingCapacity );
			PreDefTableEntry( pdchHeatCoilNomEff, WatertoAirHP( HPNum ).Name, "-" );

		}

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		lAlphaBlanks.deallocate();
		cNumericFields.deallocate();
		lNumericBlanks.deallocate();
		NumArray.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found getting input. Program terminates." );
		}

		for ( HPNum = 1; HPNum <= NumWatertoAirHPs; ++HPNum ) {

			if ( WatertoAirHP( HPNum ).WAHPPlantTypeOfNum == TypeOf_CoilWAHPCoolingParamEst ) {
				// COOLING COIL: Setup Report variables for the Heat Pump
				SetupOutputVariable( "Cooling Coil Electric Power [W]", WatertoAirHP( HPNum ).Power, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", WatertoAirHP( HPNum ).QLoadTotal, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", WatertoAirHP( HPNum ).QSensible, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", WatertoAirHP( HPNum ).QLatent, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Rate [W]", WatertoAirHP( HPNum ).QSource, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Cooling Coil Part Load Ratio []", WatertoAirHP( HPNum ).PartLoadRatio, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Cooling Coil Runtime Fraction []", WatertoAirHP( HPNum ).RunFrac, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Cooling Coil Air Mass Flow Rate [kg/s]", WatertoAirHP( HPNum ).OutletAirMassFlowRate, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Cooling Coil Air Inlet Temperature [C]", WatertoAirHP( HPNum ).InletAirDBTemp, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]", WatertoAirHP( HPNum ).InletAirHumRat, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Cooling Coil Air Outlet Temperature [C]", WatertoAirHP( HPNum ).OutletAirDBTemp, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]", WatertoAirHP( HPNum ).OutletAirHumRat, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Cooling Coil Source Side Mass Flow Rate [kg/s]", WatertoAirHP( HPNum ).OutletWaterMassFlowRate, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Cooling Coil Source Side Inlet Temperature [C]", WatertoAirHP( HPNum ).InletWaterTemp, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Cooling Coil Source Side Outlet Temperature [C]", WatertoAirHP( HPNum ).OutletWaterTemp, "System", "Average", WatertoAirHP( HPNum ).Name );
			} else if ( WatertoAirHP( HPNum ).WAHPPlantTypeOfNum == TypeOf_CoilWAHPHeatingParamEst ) {
				// HEATING COIL Setup Report variables for the Heat Pump
				SetupOutputVariable( "Heating Coil Electric Power [W]", WatertoAirHP( HPNum ).Power, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Heating Coil Heating Rate [W]", WatertoAirHP( HPNum ).QLoadTotal, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Heating Coil Sensible Heating Rate [W]", WatertoAirHP( HPNum ).QSensible, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Heating Coil Source Side Heat Transfer Rate [W]", WatertoAirHP( HPNum ).QSource, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Heating Coil Part Load Ratio []", WatertoAirHP( HPNum ).PartLoadRatio, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Heating Coil Runtime Fraction []", WatertoAirHP( HPNum ).RunFrac, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Heating Coil Air Mass Flow Rate [kg/s]", WatertoAirHP( HPNum ).OutletAirMassFlowRate, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Heating Coil Air Inlet Temperature [C]", WatertoAirHP( HPNum ).InletAirDBTemp, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]", WatertoAirHP( HPNum ).InletAirHumRat, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Heating Coil Air Outlet Temperature [C]", WatertoAirHP( HPNum ).OutletAirDBTemp, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]", WatertoAirHP( HPNum ).OutletAirHumRat, "System", "Average", WatertoAirHP( HPNum ).Name );

				SetupOutputVariable( "Heating Coil Source Side Mass Flow Rate [kg/s]", WatertoAirHP( HPNum ).OutletWaterMassFlowRate, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Heating Coil Source Side Inlet Temperature [C]", WatertoAirHP( HPNum ).InletWaterTemp, "System", "Average", WatertoAirHP( HPNum ).Name );
				SetupOutputVariable( "Heating Coil Source Side Outlet Temperature [C]", WatertoAirHP( HPNum ).OutletWaterTemp, "System", "Average", WatertoAirHP( HPNum ).Name );

			}

		}

	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitWatertoAirHP(
		int const HPNum, // index to main heat pump data structure
		bool const InitFlag,
		Real64 const MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
		Real64 const HPTimeConstant, // Heat pump time constant [s]
		Real64 const FanDelayTime, // Fan delay time, time delay for the HP's fan to
		Real64 const SensLoad,
		Real64 const LatentLoad,
		Real64 const DesignAirFlow,
		Real64 const PartLoadRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hui Jin
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
		//                      Brent Griffith, Sept 2010, plant upgrades, general fluid properties

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Water to Air HP Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//shut off after compressor cycle off  [s]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// REAL(r64), PARAMETER        :: CpWater=4210.d0          ! Specific heat of water J/kg_C
		static std::string const RoutineName( "InitWatertoAirHP" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER             :: WatertoAirHPNum          ! heat pump number
		int AirInletNode; // air inlet node number
		int WaterInletNode; // water inlet node number
		int PlantOutletNode;
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyEnvrnFlag;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		bool errFlag;

		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumWatertoAirHPs );
			MyPlantScanFlag.allocate( NumWatertoAirHPs );
			MyEnvrnFlag = true;
			MyPlantScanFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyPlantScanFlag( HPNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( WatertoAirHP( HPNum ).Name, WatertoAirHP( HPNum ).WAHPPlantTypeOfNum, WatertoAirHP( HPNum ).LoopNum, WatertoAirHP( HPNum ).LoopSide, WatertoAirHP( HPNum ).BranchNum, WatertoAirHP( HPNum ).CompNum, _, _, _, _, _, errFlag );

			if ( PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidName == "WATER" ) {
				if ( WatertoAirHP( HPNum ).SourceSideUACoeff < rTinyValue ) {
					ShowSevereError( "Input problem for water to air heat pump, \"" + WatertoAirHP( HPNum ).Name + "\"." );
					ShowContinueError( " Source side UA value is less than tolerance, likely zero or blank." );
					ShowContinueError( " Verify inputs, as the parameter syntax for this object went through a change with" );
					ShowContinueError( "  the release of EnergyPlus version 5." );
					errFlag = true;
				}
			} else {
				if ( ( WatertoAirHP( HPNum ).SourceSideHTR1 < rTinyValue ) || ( WatertoAirHP( HPNum ).SourceSideHTR2 < rTinyValue ) ) {
					ShowSevereError( "Input problem for water to air heat pump, \"" + WatertoAirHP( HPNum ).Name + "\"." );
					ShowContinueError( " A source side heat transfer resistance value is less than tolerance, likely zero or blank." );
					ShowContinueError( " Verify inputs, as the parameter syntax for this object went through a change with" );
					ShowContinueError( "  the release of EnergyPlus version 5." );
					errFlag = true;
				}
			}

			if ( errFlag ) {
				ShowFatalError( "InitWatertoAirHP: Program terminated for previous conditions." );
			}

			MyPlantScanFlag( HPNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( HPNum ) && ! MyPlantScanFlag( HPNum ) ) {
			// Do the initializations to start simulation
			// Set water and air inlet nodes
			AirInletNode = WatertoAirHP( HPNum ).AirInletNodeNum;
			WaterInletNode = WatertoAirHP( HPNum ).WaterInletNodeNum;

			//Initialize all report variables to a known state at beginning of simulation
			WatertoAirHP( HPNum ).Power = 0.0;
			WatertoAirHP( HPNum ).Energy = 0.0;
			WatertoAirHP( HPNum ).QLoadTotal = 0.0;
			WatertoAirHP( HPNum ).QSensible = 0.0;
			WatertoAirHP( HPNum ).QLatent = 0.0;
			WatertoAirHP( HPNum ).QSource = 0.0;
			WatertoAirHP( HPNum ).EnergyLoadTotal = 0.0;
			WatertoAirHP( HPNum ).EnergySensible = 0.0;
			WatertoAirHP( HPNum ).EnergyLatent = 0.0;
			WatertoAirHP( HPNum ).EnergySource = 0.0;
			WatertoAirHP( HPNum ).RunFrac = 0.0;
			WatertoAirHP( HPNum ).PartLoadRatio = 0.0;
			WatertoAirHP( HPNum ).OutletAirDBTemp = 0.0;
			WatertoAirHP( HPNum ).OutletAirHumRat = 0.0;
			WatertoAirHP( HPNum ).InletAirDBTemp = 0.0;
			WatertoAirHP( HPNum ).InletAirHumRat = 0.0;
			WatertoAirHP( HPNum ).OutletWaterTemp = 0.0;
			WatertoAirHP( HPNum ).InletWaterTemp = 0.0;
			WatertoAirHP( HPNum ).InletAirMassFlowRate = 0.0;
			WatertoAirHP( HPNum ).InletWaterMassFlowRate = 0.0;
			WatertoAirHP( HPNum ).OutletAirEnthalpy = 0.0;
			WatertoAirHP( HPNum ).OutletWaterEnthalpy = 0.0;

			// The rest of the one time initializations
			rho = GetDensityGlycol( PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidIndex, RoutineName );
			Cp = GetSpecificHeatGlycol( PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidIndex, RoutineName );

			WatertoAirHP( HPNum ).DesignWaterMassFlowRate = rho * WatertoAirHP( HPNum ).DesignWaterVolFlowRate;
			WatertoAirHP( HPNum ).MaxONOFFCyclesperHour = MaxONOFFCyclesperHour;
			WatertoAirHP( HPNum ).HPTimeConstant = HPTimeConstant;
			WatertoAirHP( HPNum ).FanDelayTime = FanDelayTime;

			PlantOutletNode = PlantLoop( WatertoAirHP( HPNum ).LoopNum ).LoopSide( WatertoAirHP( HPNum ).LoopSide ).Branch( WatertoAirHP( HPNum ).BranchNum ).Comp( WatertoAirHP( HPNum ).CompNum ).NodeNumOut;
			InitComponentNodes( 0.0, WatertoAirHP( HPNum ).DesignWaterMassFlowRate, WaterInletNode, PlantOutletNode, WatertoAirHP( HPNum ).LoopNum, WatertoAirHP( HPNum ).LoopSide, WatertoAirHP( HPNum ).BranchNum, WatertoAirHP( HPNum ).CompNum );

			Node( WaterInletNode ).Temp = 5.0;
			Node( WaterInletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
			Node( WaterInletNode ).Quality = 0.0;
			Node( WaterInletNode ).Press = 0.0;
			Node( WaterInletNode ).HumRat = 0.0;

			Node( PlantOutletNode ).Temp = 5.0;
			Node( PlantOutletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
			Node( PlantOutletNode ).Quality = 0.0;
			Node( PlantOutletNode ).Press = 0.0;
			Node( PlantOutletNode ).HumRat = 0.0;

			WatertoAirHP( HPNum ).SimFlag = true;

			MyEnvrnFlag( HPNum ) = false;
		} // End If for the Begin Environment initializations

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( HPNum ) = true;
		}

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.
		// First set the conditions for the air into the heat pump model

		// Set water and air inlet nodes
		AirInletNode = WatertoAirHP( HPNum ).AirInletNodeNum;
		WaterInletNode = WatertoAirHP( HPNum ).WaterInletNodeNum;

		//  ! Set heat pump simulation flag to false if the air loop and water loop conditions have not changed
		//  IF( .NOT. (BeginEnvrnFlag .and. MyEnvrnFlag) .AND. (&
		//  WatertoAirHP(HPNum)%InletWaterTemp      >= (Node(WaterInletNode)%Temp + TempTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletWaterTemp      <= (Node(WaterInletNode)%Temp - TempTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletWaterEnthalpy  >= (Node(WaterInletNode)%Enthalpy + EnthTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletWaterEnthalpy  <= (Node(WaterInletNode)%Enthalpy - EnthTOL) .OR. &!!

		//  WatertoAirHP(HPNum)%InletAirDBTemp      >= (Node(AirInletNode)%Temp + TempTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletAirDBTemp      <= (Node(AirInletNode)%Temp - TempTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletAirHumRat      >= (Node(AirInletNode)%HumRat + HumRatTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletAirHumRat      <= (Node(AirInletNode)%HumRat - HumRatTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletAirEnthalpy    >= (Node(AirInletNode)%Enthalpy + EnthTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletAirEnthalpy    <= (Node(AirInletNode)%Enthalpy - EnthTOL) .OR. &
		//  WatertoAirHP(HPNum)%InletAirMassFlowRate > 0.0))THEN
		//    WatertoAirHP(HPNum)%SimFlag =.TRUE.
		//  ELSE
		//    WatertoAirHP(HPNum)%SimFlag =.FALSE.
		//  ENDIF

		if ( ( ( SensLoad != 0.0 || LatentLoad != 0.0 ) || ( SensLoad == 0.0 && InitFlag ) ) && Node( AirInletNode ).MassFlowRate > 0.0 && PartLoadRatio > 0.0 ) {
			//set the water side flow rate to the design flow rate unless constrained by
			//the demand side manager (MIN/MAX available). now done by call to setcomponentFlowRate
			WatertoAirHP( HPNum ).InletWaterMassFlowRate = WatertoAirHP( HPNum ).DesignWaterMassFlowRate;
			WatertoAirHP( HPNum ).InletAirMassFlowRate = DesignAirFlow; //This is required instead of the node temperature
			//because the air loop operates handles part load for
			//cycling equipment by modulating the air flow rate
			//the heat pump model requires an accurate (i.e. full load
			//flow rate for accurate simulation.
		} else { //heat pump is off
			WatertoAirHP( HPNum ).InletWaterMassFlowRate = 0.0;

			WatertoAirHP( HPNum ).InletAirMassFlowRate = 0.0;
		}
		//constrain water flow provided by plant
		SetComponentFlowRate( WatertoAirHP( HPNum ).InletWaterMassFlowRate, WatertoAirHP( HPNum ).WaterInletNodeNum, WatertoAirHP( HPNum ).WaterOutletNodeNum, WatertoAirHP( HPNum ).LoopNum, WatertoAirHP( HPNum ).LoopSide, WatertoAirHP( HPNum ).BranchNum, WatertoAirHP( HPNum ).CompNum );

		WatertoAirHP( HPNum ).InletWaterTemp = Node( WaterInletNode ).Temp;
		//  IF (WatertoAirHP(HPNum)%InletWaterTemp < 0.0) THEN  ! Debug trap
		//    Temptemp         = Node(WaterInletNode)%Temp
		//  ENDIF
		WatertoAirHP( HPNum ).InletWaterEnthalpy = Node( WaterInletNode ).Enthalpy;

		WatertoAirHP( HPNum ).InletAirDBTemp = Node( AirInletNode ).Temp;
		WatertoAirHP( HPNum ).InletAirHumRat = Node( AirInletNode ).HumRat;
		WatertoAirHP( HPNum ).InletAirEnthalpy = Node( AirInletNode ).Enthalpy;

		WatertoAirHP( HPNum ).Power = 0.0;
		WatertoAirHP( HPNum ).Energy = 0.0;
		WatertoAirHP( HPNum ).QLoadTotal = 0.0;
		WatertoAirHP( HPNum ).QSensible = 0.0;
		WatertoAirHP( HPNum ).QLatent = 0.0;
		WatertoAirHP( HPNum ).QSource = 0.0;
		WatertoAirHP( HPNum ).EnergyLoadTotal = 0.0;
		WatertoAirHP( HPNum ).EnergySensible = 0.0;
		WatertoAirHP( HPNum ).EnergyLatent = 0.0;
		WatertoAirHP( HPNum ).EnergySource = 0.0;
		WatertoAirHP( HPNum ).RunFrac = 0.0;
		WatertoAirHP( HPNum ).OutletAirDBTemp = 0.0;
		WatertoAirHP( HPNum ).OutletAirHumRat = 0.0;
		WatertoAirHP( HPNum ).OutletWaterTemp = 0.0;
		WatertoAirHP( HPNum ).OutletAirEnthalpy = 0.0;
		WatertoAirHP( HPNum ).OutletWaterEnthalpy = 0.0;

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcWatertoAirHPCooling(
		int const HPNum, // heat pump number
		int const CyclingScheme, // fan/compressor cycling scheme indicator
		bool const FirstHVACIteration, // first iteration flag
		Real64 const RuntimeFrac,
		bool const EP_UNUSED( InitFlag ), // suppress property errors if true
		Real64 const SensDemand,
		int const CompOp,
		Real64 const PartLoadRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hui Jin
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004), R. Raustad (Oct 2006) Revised iteration technique
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a parameter estimation based water to air heat pump model

		// USE STATEMENTS:
		// na

		// Using/Aliasing
		using namespace FluidProperties;
		//  USE DataZoneEnergyDemands
		using CurveManager::CurveValue;
		using CurveManager::GetCurveIndex;
		using Psychrometrics::PsyHFnTdbW; // ,PsyHFnTdbRhPb,PsyWFnTdpPb
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyWFnTdbH;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTsatFnHPb;
		using General::RoundSigDigits;
		using General::SolveRegulaFalsi;
		using InputProcessor::SameString;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const CpWater( 4210.0 ); // Specific heat of water J/kg_C
		Real64 const DegreeofSuperheat( 80.0 ); // Initial guess of degree of superheat
		Real64 const gamma( 1.114 ); // Expansion Coefficient
		Real64 const RelaxParam( 0.5 ); // Relaxation Parameter
		Real64 const ERR( 0.01 ); // Error Value
		Real64 const PB( 1.013e5 ); // Barometric Pressure (Pa)

		int const STOP1( 100000 ); // Iteration stopper1
		int const STOP2( 100000 ); // Iteration stopper2
		int const STOP3( 100000 ); // Iteration stopper3

		static std::string const RoutineNameSourceSideInletTemp( "CalcWatertoAirHPCooling:SourceSideInletTemp" );
		static std::string const RoutineNameSourceSideTemp( "CalcWatertoAirHPCooling:SourceSideTemp" );
		static std::string const RoutineNameLoadSideTemp( "CalcWatertoAirHPCooling:LoadSideTemp" );
		static std::string const RoutineNameCompressInletTemp( "CalcWatertoAirHPCooling:CompressInletTemp" );
		static std::string const RoutineNameSuctionPr( "CalcWatertoAirHPCooling:SuctionPr" );
		static std::string const RoutineNameCompSuctionTemp( "CalcWatertoAirHPCooling:CompSuctionTemp");

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//      INTEGER                :: NumIteration1            ! Number of Iteration1
		int NumIteration2; // Number of Iteration2
		int NumIteration3; // Number of Iteration3
		int NumIteration4; // Number of Iteration4 (use of latent degradation model ONLY)
		int SourceSideFluidIndex; // Source Side Fluid Index

		int CompressorType; // Type of Compressor ie. Reciprocating,Rotary or Scroll
		std::string SourceSideFluidName; // Name of source side fluid
		std::string Refrigerant; // Name of refrigerant
		//      CHARACTER(len=25) :: CErrCount
		Real64 NominalCoolingCapacity; // Nominal Cooling Capacity (W)
		Real64 LoadSideTotalUA; // Load Side Total Heat Transfer coefficient [W/C]
		Real64 LoadSideoutsideUA; // Load Side Outside Heat Transfer coefficient [W/C]
		Real64 SourceSideUA; // Source Side Heat Transfer coefficient [W/C]
		Real64 PressureDrop; // Suction or Discharge Pressure Drop [Pa]
		Real64 ClearanceFactor; // Compressor Clearance Factor
		Real64 PistonDisp; // Compressor Piston Displacement [m3/s]
		Real64 ShTemp; // Superheat Temperature [C]
		Real64 LosFac; // Compressor Power Loss Factor
		Real64 PowerLos; // Constant Part of Power Losses [kW]
		Real64 RefVolFlowRate; // Refrigerant Volume Flow rate at the beginning
		Real64 VolumeRatio; // Built-in-volume ratio [~]
		Real64 LeakRateCoeff; // Coefficient for the relationship between
		// Pressure Ratio and Leakage Rate [~]
		Real64 SourceSideHTRes1; // Source Side Heat Transfer Resistance coefficient 1 [~]
		Real64 SourceSideHTRes2; // Source Side Heat Transfer Resistance coefficient 2 [K/kW]
		Real64 HighPressCutoff; // High Pressure Cut-off [Pa]
		Real64 LowPressCutoff; // Low Pressure Cut-off [Pa]

		Real64 Quality; // Quality of Refrigerant
		Real64 SourceSideMassFlowRate; // Source Side Mass Flow Rate [kg/s]
		Real64 SourceSideInletTemp; // Source Side Inlet Temperature [C]
		Real64 SourceSideWaterInletEnth; // Source Side Outlet Enthalpy [J/kg]
		Real64 SourceSideOutletTemp; // Source Side Outlet Temperature [C]
		Real64 SourceSideVolFlowRate; // Source Side Volumetric Flow Rate [m3/s]
		Real64 DegradFactor; // Degradation Factor [~]
		Real64 CpFluid; // Specific heat of source side fluid(J/kg)
		Real64 LoadSideMassFlowRate; // Load Side Mass Flow Rate [kg/s]
		Real64 LoadSideInletWBTemp; // Wet-bulb temperature of indoor inlet air [C]
		Real64 LoadSideInletDBTemp; // Load Side Inlet Dry Bulb Temp [C]
		Real64 LoadSideInletHumRat; // Load Side Inlet Humidity Ratio [kg/kg]
		Real64 LoadSideOutletDBTemp; // Load Side Outlet Dry Bulb Temperature [C]
		Real64 LoadSideOutletHumRat; // Load Side Outlet Humidity Ratio [kg/kg]
		Real64 LoadSideAirInletEnth; // Load Side Inlet Enthalpy [J/kg]
		Real64 LoadSideAirOutletEnth; // Load Side Outlet Enthalpy [J/kg]
		//      REAL(r64)        :: EffectiveSurfaceTemp1    ! Effective Surface Temperature Guess #1 [C]
		//      REAL(r64)        :: EffectiveSurfaceTemp2    ! Effective Surface Temperature Guess #2 [C]
		static Real64 EffectiveSurfaceTemp; // Effective Surface Temperature [C]
		Real64 EffectiveSatEnth; // Saturated Enthalpy of Air Corresponding to the Effective Surface
		// Temperature [J/kg]
		//      REAL(r64)        :: EffectiveSatEnth1        ! Guess of the Saturated Enthalpy of Air Corresponding to the
		//                                                   ! Effective Surface Temperature [J/kg]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 QLoadTotal; // Load Side Total Heat Transfer Rate [W]
		Real64 QSensible; // Load Side Sensible Heat Transfer Rate [W]
		Real64 Power; // Power Consumption [W]
		//      REAL(r64)        :: EvapTemp1                ! Evaporating Temperature Guess #1 [C]
		//      REAL(r64)        :: EvapTemp2                ! Evaporating Temperature Guess #2 [C]
		static Real64 EvapTemp; // Evaporating Temperature [C]
		Real64 ANTUWET; // Number of Transfer Unit for Wet Condition
		Real64 EffectWET; // Load Side Heat Exchanger Effectiveness
		Real64 EvapSatEnth; // Saturated Enthalpy of Air Corresponding to the Evaporating
		// Temperature [J/kg]
		//      REAL(r64)        :: EvapSatEnth1             ! Guess of the Saturated Enthalpy of Air Corresponding to the
		//                                                   ! Evaporating Temperature [J/kg]
		Real64 SourceSideEffect; // Source Side Heat Exchanger Effectiveness
		Real64 SourceSideTemp; // Source Side Saturated Refrigerant Temperature [C]
		Real64 LoadSideTemp; // Load Side Saturated Refrigerant Temperature [C]
		Real64 SourceSidePressure; // Source Side Saturated Refrigerant Pressure [Pa]
		Real64 LoadSidePressure; // Load Side Saturated Refrigerant Pressure [Pa]
		Real64 SuctionPr; // Compressor Suction Pressure [Pa]
		Real64 DischargePr; // Compressor Discharge Pressure [Pa]
		Real64 CompressInletTemp; // Temperature of the Refrigerant Entering the Compressor [C]
		Real64 MassRef; // Mass Flow Rate of Refrigerant [kg/s]
		Real64 SourceSideOutletEnth; // Enthalpy of Refrigerant leaving the Source Side Heat Exchanger [J/kg]
		Real64 LoadSideOutletEnth; // Enthalpy of Refrigerant leaving the Load Side Heat Exchanger [J/kg]
		Real64 CpAir; // Specific Heat of Air [J/kg_C]
		static Real64 initialQSource; // Guess Source Side Heat Transfer Rate [W]
		static Real64 initialQLoadTotal; // Guess Load Side Heat Transfer rate [W]
		Real64 SuperHeatEnth; // Enthalpy of the Superheated Refrigerant [J/kg]
		Real64 CompSuctionTemp1; // Guess of the Temperature of the Refrigerant Entering the
		// Compressor #1 [C]
		Real64 CompSuctionTemp2; // Guess of the Temperature of the Refrigerant Entering the
		// Compressor #2 [C]
		static Real64 CompSuctionTemp; // Temperature of the Refrigerant Entering the Compressor [C]
		Real64 CompSuctionEnth; // Enthalpy of the Refrigerant Entering the Compressor [J/kg]
		Real64 CompSuctionDensity; // Density of the Refrigerant Entering the Compressorkg/m3
		Real64 CompSuctionSatTemp; // Temperature of Saturated Refrigerant at Compressor Suction Pressure [C]
		Real64 Twet_Rated; // Twet at rated conditions (coil air flow rate and air temperatures), sec
		Real64 Gamma_Rated; // Gamma at rated conditions (coil air flow rate and air temperatures)
		bool LatDegradModelSimFlag; // Latent degradation model simulation flag
		bool FinalSimFlag; // Final Simulation Flag
		bool Converged; // overall convergence Flag

		Real64 QLatRated; // Qlatent at rated conditions of indoor(TDB,TWB)=(26.7C,19.4C)
		Real64 QLatActual; // Qlatent at actual operating conditions
		Real64 SHRss; // Sensible heat ratio at steady state
		Real64 SHReff; // Effective sensible heat ratio at part-load condition
		Array1D< Real64 > Par( 4 ); // Parameter array passed to RegulaFalsi function
		int SolFlag; // Solution flag returned from RegulaFalsi function
		static bool firstTime( true );
		static Real64 LoadSideInletDBTemp_Init; // rated conditions
		static Real64 LoadSideInletHumRat_Init; // rated conditions
		static Real64 LoadSideAirInletEnth_Init; // rated conditions
		Real64 LoadSideInletDBTemp_Unit; // calc conditions for unit
		Real64 LoadSideInletHumRat_Unit; // calc conditions for unit
		Real64 LoadSideAirInletEnth_Unit; // calc conditions for unit

		if ( firstTime ) {
			//Set indoor air conditions to the rated condition
			LoadSideInletDBTemp_Init = 26.7;
			LoadSideInletHumRat_Init = 0.0111;
			LoadSideAirInletEnth_Init = PsyHFnTdbW( LoadSideInletDBTemp_Init, LoadSideInletHumRat_Init );
			firstTime = false;
		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		NominalCoolingCapacity = WatertoAirHP( HPNum ).CoolingCapacity;
		CompressorType = WatertoAirHP( HPNum ).CompressorType;
		Refrigerant = WatertoAirHP( HPNum ).Refrigerant;
		LoadSideTotalUA = WatertoAirHP( HPNum ).LoadSideTotalUACoeff;
		LoadSideoutsideUA = WatertoAirHP( HPNum ).LoadSideOutsideUACoeff;
		PistonDisp = WatertoAirHP( HPNum ).CompPistonDisp;
		ClearanceFactor = WatertoAirHP( HPNum ).CompClearanceFactor;
		PressureDrop = WatertoAirHP( HPNum ).CompSucPressDrop;
		ShTemp = WatertoAirHP( HPNum ).SuperheatTemp;
		PowerLos = WatertoAirHP( HPNum ).PowerLosses;
		LosFac = WatertoAirHP( HPNum ).LossFactor;
		RefVolFlowRate = WatertoAirHP( HPNum ).RefVolFlowRate;
		VolumeRatio = WatertoAirHP( HPNum ).VolumeRatio;
		LeakRateCoeff = WatertoAirHP( HPNum ).LeakRateCoeff;
		SourceSideUA = WatertoAirHP( HPNum ).SourceSideUACoeff;
		SourceSideHTRes1 = WatertoAirHP( HPNum ).SourceSideHTR1;
		SourceSideHTRes2 = WatertoAirHP( HPNum ).SourceSideHTR2;
		HighPressCutoff = WatertoAirHP( HPNum ).HighPressCutoff;
		LowPressCutoff = WatertoAirHP( HPNum ).LowPressCutoff;

		LoadSideMassFlowRate = WatertoAirHP( HPNum ).InletAirMassFlowRate;
		//Set indoor air conditions to the actual condition
		LoadSideInletDBTemp_Unit = WatertoAirHP( HPNum ).InletAirDBTemp;
		LoadSideInletHumRat_Unit = WatertoAirHP( HPNum ).InletAirHumRat;
		CpAir = PsyCpAirFnWTdb( LoadSideInletHumRat_Unit, LoadSideInletDBTemp_Unit );
		LoadSideAirInletEnth_Unit = PsyHFnTdbW( LoadSideInletDBTemp_Unit, LoadSideInletHumRat_Unit );

		SourceSideInletTemp = WatertoAirHP( HPNum ).InletWaterTemp;
		SourceSideWaterInletEnth = WatertoAirHP( HPNum ).InletWaterEnthalpy;
		SourceSideFluidName = PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidName;
		SourceSideFluidIndex = PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidIndex;
		SourceSideMassFlowRate = WatertoAirHP( HPNum ).InletWaterMassFlowRate;
		SourceSideVolFlowRate = SourceSideMassFlowRate / GetDensityGlycol( SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp );

		Twet_Rated = WatertoAirHP( HPNum ).Twet_Rated;
		Gamma_Rated = WatertoAirHP( HPNum ).Gamma_Rated;

		FinalSimFlag = false;

		// If heat pump is not operating, return
		if ( SensDemand == 0.0 || LoadSideMassFlowRate <= 0.0 || SourceSideMassFlowRate <= 0.0 ) {
			WatertoAirHP( HPNum ).SimFlag = false;
			return;
		} else {
			WatertoAirHP( HPNum ).SimFlag = true;
		}

		if ( CompOp == 0 ) {
			WatertoAirHP( HPNum ).SimFlag = false;
			return;
		}

		if ( FirstHVACIteration ) {
			initialQSource = NominalCoolingCapacity;
			initialQLoadTotal = NominalCoolingCapacity;
		}

		if ( initialQLoadTotal == 0.0 ) initialQLoadTotal = NominalCoolingCapacity;
		if ( initialQSource == 0.0 ) initialQSource = NominalCoolingCapacity;

		//Loop the calculation at least twice depending whether the latent degradation model
		//is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
		//and 2nd iteration to calculate the  QLatent(actual)

		QLatRated = 0.0;
		QLatActual = 0.0;
		//IF((RuntimeFrac .GE. 1.0) .OR. (Twet_rated .LE. 0.0) .OR. (Gamma_rated .LE. 0.0)) THEN
		// Cycling fan does not required latent degradation model, only the constant fan case
		if ( ( RuntimeFrac >= 1.0 ) || ( Twet_Rated <= 0.0 ) || ( Gamma_Rated <= 0.0 ) || ( CyclingScheme == CycFanCycCoil ) ) {
			LatDegradModelSimFlag = false;
			//Set NumIteration4=1 so that latent model would quit after 1 simulation with the actual condition
			NumIteration4 = 1;
		} else {
			LatDegradModelSimFlag = true;
			//Set NumIteration4=0 so that latent model would simulate twice with rated and actual condition
			NumIteration4 = 0;
		}

		//Tuned Hoisted quantities out of nested loop that don't change
		Real64 const LoadSideMassFlowRate_CpAir_inv( 1.0 / ( LoadSideMassFlowRate * CpAir ) );
		Real64 const LoadSideEffec( 1.0 - std::exp( -LoadSideoutsideUA * LoadSideMassFlowRate_CpAir_inv ) ); // Load Side Effectiveness based on Outside Heat Transfer Coefficient
		Real64 const LoadSideEffec_MassFlowRate_inv( 1.0 / ( LoadSideEffec * LoadSideMassFlowRate ) );
		ANTUWET = LoadSideTotalUA * LoadSideMassFlowRate_CpAir_inv;
		EffectWET = 1.0 - std::exp( -ANTUWET );

		while ( true ) {
			++NumIteration4;
			if ( NumIteration4 == 1 ) {
				//Set indoor air conditions to the rated condition
				LoadSideInletDBTemp = LoadSideInletDBTemp_Init;
				LoadSideInletHumRat = LoadSideInletHumRat_Init;
				LoadSideAirInletEnth = LoadSideAirInletEnth_Init;
			} else {
				//Set indoor air conditions to the actual condition
				LoadSideInletDBTemp = LoadSideInletDBTemp_Unit;
				LoadSideInletHumRat = LoadSideInletHumRat_Unit;
				LoadSideAirInletEnth = LoadSideAirInletEnth_Unit;
			}

			//Outerloop: Calculate source side heat transfer
			NumIteration2 = 0;
			Converged = false;
			FinalSimFlag = false;
			while ( true ) {
				if ( Converged ) FinalSimFlag = true;

				++NumIteration2;

				if ( NumIteration2 > STOP2 ) {
					WatertoAirHP( HPNum ).SimFlag = false;
					return;
				}

				//Innerloop: Calculate load side heat transfer
				NumIteration3 = 0;
				while ( true ) {

					++NumIteration3;

					if ( NumIteration3 > STOP3 ) {
						WatertoAirHP( HPNum ).SimFlag = false;
						return;
					}

					// Determine Effectiveness of Source Side
					CpFluid = GetSpecificHeatGlycol( SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp );

					//      IF (SourceSideFluidName=='WATER') THEN
					if ( SourceSideFluidIndex == WaterIndex ) { // SourceSideFluidName=='Water'
						SourceSideEffect = 1.0 - std::exp( -SourceSideUA / ( CpFluid * SourceSideMassFlowRate ) );
					} else {
						DegradFactor = DegradF( SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex );
						SourceSideEffect = 1.0 / ( ( SourceSideHTRes1 * std::pow( SourceSideVolFlowRate, -0.8 ) ) / DegradFactor + SourceSideHTRes2 );
					}

					// Determine Source Side Tempertaure (Condensing Temp in this case)
					SourceSideTemp = SourceSideInletTemp + initialQSource / ( SourceSideEffect * CpFluid * SourceSideMassFlowRate );

					// Compute the Effective Surface Temperature
					EffectiveSatEnth = LoadSideAirInletEnth - initialQLoadTotal * LoadSideEffec_MassFlowRate_inv;

					//      ! Set up the Initial Range of Effective Surface Temperature
					//      IF(.NOT. Converged)THEN
					//        EffectiveSurfaceTemp1=-100.
					//        EffectiveSurfaceTemp2=200.
					//      END IF
					//      ! Iterate to calculate the effective surface temp from the corresponding enthalpy
					//      NumIteration1=0
					//      LOOP1: DO
					//        NumIteration1=NumIteration1+1
					//        IF (NumIteration1.GT.STOP1) THEN
					//          WatertoAirHP(HPNum)%SimFlag = .FALSE.
					//          RETURN
					//        END IF
					//        EffectiveSurfaceTemp=0.5d0*(EffectiveSurfaceTemp1+EffectiveSurfaceTemp2)
					//        EffectiveSatEnth1=PsyHFnTdbRhPb(EffectiveSurfaceTemp,1.0,PB)
					//        IF(ABS(EffectiveSatEnth-EffectiveSatEnth1).LT.0.01 .OR. &
					//          ABS(EffectiveSurfaceTemp1-EffectiveSurfaceTemp2).LT.0.001) THEN
					//          EXIT LOOP1
					//        END IF
					//        IF(EffectiveSatEnth1.LT.EffectiveSatEnth) THEN
					//          EffectiveSurfaceTemp1=EffectiveSurfaceTemp
					//        ELSE
					//          EffectiveSurfaceTemp2=EffectiveSurfaceTemp
					//        END IF
					//      END DO LOOP1

					EffectiveSurfaceTemp = PsyTsatFnHPb( EffectiveSatEnth, PB );

					QSensible = LoadSideMassFlowRate * CpAir * ( LoadSideInletDBTemp - EffectiveSurfaceTemp ) * LoadSideEffec;
					EvapSatEnth = LoadSideAirInletEnth - initialQLoadTotal / ( EffectWET * LoadSideMassFlowRate );

					//      ! Iterate to compute Evaporating Temperature
					//      IF(.NOT. Converged)THEN
					//        EvapTemp1=-150
					//        EvapTemp2=100
					//      END IF
					//      NumIteration1=0
					//      LOOP2: DO
					//        NumIteration1=NumIteration1+1
					//        IF (NumIteration1.GT.STOP1) THEN
					//          WatertoAirHP(HPNum)%SimFlag = .FALSE.
					//          RETURN
					//        END IF
					//        EvapTemp=0.5d0*(EvapTemp1+EvapTemp2)
					//        EvapSatEnth1=PsyHFnTdbRhPb(EvapTemp,1.0,PB)
					//        IF(ABS((EvapSatEnth-EvapSatEnth1)/EvapSatEnth).LT.ERR1) THEN
					//         EXIT LOOP2
					//        END IF
					//        IF(EvapSatEnth1.LT.EvapSatEnth) THEN
					//         EvapTemp1=EvapTemp
					//        ELSE
					//         EvapTemp2=EvapTemp
					//        END IF
					//      END DO LOOP2

					EvapTemp = PsyTsatFnHPb( EvapSatEnth, PB );

					// Load Side Saturated Temperature (Evaporating Temp in this case)
					LoadSideTemp = EvapTemp;

					// Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
					SourceSidePressure = GetSatPressureRefrig( Refrigerant, SourceSideTemp, RefrigIndex, RoutineNameSourceSideTemp );
					LoadSidePressure = GetSatPressureRefrig( Refrigerant, LoadSideTemp, RefrigIndex, RoutineNameLoadSideTemp );

					if ( LoadSidePressure < LowPressCutoff && ! FirstHVACIteration ) {
						if ( ! WarmupFlag ) {
							ShowRecurringWarningErrorAtEnd( "WaterToAir Heat pump:cooling [" + WatertoAirHP( HPNum ).Name + "] shut off on low pressure < " + RoundSigDigits( LowPressCutoff, 0 ), WatertoAirHP( HPNum ).LowPressClgError, LoadSidePressure, LoadSidePressure, _, "[Pa]", "[Pa]" );
						}
						WatertoAirHP( HPNum ).SimFlag = false;
						return;
					}

					if ( SourceSidePressure > HighPressCutoff && ! FirstHVACIteration ) {
						if ( ! WarmupFlag ) {
							ShowRecurringWarningErrorAtEnd( "WaterToAir Heat pump:cooling [" + WatertoAirHP( HPNum ).Name + "] shut off on high pressure > " + RoundSigDigits( HighPressCutoff, 0 ), WatertoAirHP( HPNum ).HighPressClgError, SourceSideInletTemp, SourceSideInletTemp, _, "SourceSideInletTemp[C]", "SourceSideInletTemp[C]" );
						}
						WatertoAirHP( HPNum ).SimFlag = false;
						return;
					}

					// Determine Suction Pressure & Discharge Pressure at Compressor Exit
					if ( CompressorType == CompressorType_Reciprocating ) { // RECIPROCATING
						SuctionPr = LoadSidePressure - PressureDrop;
						DischargePr = SourceSidePressure + PressureDrop;
					} else if ( CompressorType == CompressorType_Rotary ) { // ROTARY
						SuctionPr = LoadSidePressure;
						DischargePr = SourceSidePressure + PressureDrop;
					} else if ( CompressorType == CompressorType_Scroll ) { // SCROLL
						SuctionPr = LoadSidePressure;
						DischargePr = SourceSidePressure;
					}

					// Determine the Load Side Outlet Enthalpy (Saturated Gas)
					Quality = 1.0;
					LoadSideOutletEnth = GetSatEnthalpyRefrig( Refrigerant, LoadSideTemp, Quality, RefrigIndex, RoutineNameLoadSideTemp );

					// Determine Source Side Outlet Enthalpy (Saturated Liquid)
					Quality = 0.0;
					SourceSideOutletEnth = GetSatEnthalpyRefrig( Refrigerant, SourceSideTemp, Quality, RefrigIndex, RoutineNameSourceSideTemp );
					// Determine Superheated Temperature of the Load Side outlet/compressor Inlet
					CompressInletTemp = LoadSideTemp + ShTemp;

					// Determine the Enthalpy of the Superheated Fluid at Load Side Outlet/Compressor Inlet
					SuperHeatEnth = GetSupHeatEnthalpyRefrig( Refrigerant, CompressInletTemp, LoadSidePressure, RefrigIndex, RoutineNameCompressInletTemp );

					// Determining the suction state of the fluid from inlet state involves interation
					// Method employed...
					// Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
					// check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached
					if ( ! Converged ) {
						CompSuctionSatTemp = GetSatTemperatureRefrig( Refrigerant, SuctionPr, RefrigIndex, RoutineNameSuctionPr );
						CompSuctionTemp1 = CompSuctionSatTemp;

						// Shoot into the Superheated Region
						CompSuctionTemp2 = CompSuctionSatTemp + DegreeofSuperheat;
					}
					// Iterate to find the Suction State
					//      NumIteration1=0
					//       LOOP: DO
					//           NumIteration1=NumIteration1+1
					//           IF (NumIteration1.GT.STOP1) THEN
					//             WatertoAirHP(HPNum)%SimFlag = .FALSE.
					//             RETURN
					//           END IF
					//               CompSuctionTemp = 0.5d0 * ( CompSuctionTemp1 + CompSuctionTemp2 )
					//               CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
					//               CompSuctionDensity = GetSupHeatDensityRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
					//               IF (ABS(CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth < ERR)  THEN
					//                   EXIT LOOP
					//               END IF
					//               IF ( CompsuctionEnth < SuperHeatEnth ) THEN
					//                   CompSuctionTemp1 = CompSuctionTemp
					//               ELSE
					//                   CompSuctionTemp2 = CompSuctionTemp
					//               END IF
					//        END DO LOOP

					//  Do not need the name of the refrigerant if we already have the index (from above CALLs)
					Par( 1 ) = SuctionPr;
					Par( 2 ) = double( RefrigIndex );
					Par( 3 ) = SuperHeatEnth;

					SolveRegulaFalsi( ERR, STOP1, SolFlag, CompSuctionTemp, CalcCompSuctionTempResidual, CompSuctionTemp1, CompSuctionTemp2, Par );
					if ( SolFlag == -1 ) {
						WatertoAirHP( HPNum ).SimFlag = false;
						return;
					}
					CompSuctionEnth = GetSupHeatEnthalpyRefrig( Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex, RoutineNameCompSuctionTemp );
					CompSuctionDensity = GetSupHeatDensityRefrig( Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex, RoutineNameCompSuctionTemp );

					// Find Refrigerant Flow Rate
					{ auto const SELECT_CASE_var( CompressorType );
					if ( SELECT_CASE_var == CompressorType_Reciprocating ) { // RECIPROCATING
						MassRef = PistonDisp * CompSuctionDensity * ( 1.0 + ClearanceFactor - ClearanceFactor * std::pow( DischargePr / SuctionPr, 1.0 / gamma ) );
					} else if ( SELECT_CASE_var == CompressorType_Rotary ) { // ROTARY
						MassRef = PistonDisp * CompSuctionDensity;
					} else if ( SELECT_CASE_var == CompressorType_Scroll ) { // SCROLL
						MassRef = RefVolFlowRate * CompSuctionDensity - LeakRateCoeff * ( DischargePr / SuctionPr );
					}}

					// Find the Load Side Heat Transfer
					QLoadTotal = MassRef * ( LoadSideOutletEnth - SourceSideOutletEnth );

					if ( std::abs( QLoadTotal - initialQLoadTotal ) / initialQLoadTotal < ERR ) {
						goto LOOPLoadEnth_exit;
					} else {
						initialQLoadTotal += RelaxParam * ( QLoadTotal - initialQLoadTotal );
					}

				}
				LOOPLoadEnth_exit: ;

				// Determine the Power Consumption
				{ auto const SELECT_CASE_var( CompressorType );
				if ( SELECT_CASE_var == CompressorType_Reciprocating ) { // RECIPROCATING
					Power = PowerLos + ( 1.0 / LosFac ) * ( MassRef * gamma / ( gamma - 1.0 ) * SuctionPr / CompSuctionDensity * ( std::pow( DischargePr / SuctionPr, ( gamma - 1.0 ) / gamma ) - 1.0 ) );
				} else if ( SELECT_CASE_var == CompressorType_Rotary ) { // ROTARY
					Power = PowerLos + ( 1.0 / LosFac ) * ( MassRef * gamma / ( gamma - 1.0 ) * SuctionPr / CompSuctionDensity * ( std::pow( DischargePr / SuctionPr, ( gamma - 1.0 ) / gamma ) - 1.0 ) );
				} else if ( SELECT_CASE_var == CompressorType_Scroll ) { // SCROLL
					Power = PowerLos + ( 1.0 / LosFac ) * ( gamma / ( gamma - 1.0 ) ) * SuctionPr * RefVolFlowRate * ( ( ( gamma - 1.0 ) / gamma ) * ( ( DischargePr / SuctionPr ) / VolumeRatio ) + ( ( 1.0 / gamma ) * std::pow( VolumeRatio, gamma - 1.0 ) ) - 1.0 );
				}}

				// Determine the Sourceside Heat Rate
				QSource = Power + QLoadTotal;

				if ( std::abs( QSource - initialQSource ) / initialQSource < ERR ) {
					Converged = true;
				} else {
					initialQSource += RelaxParam * ( QSource - initialQSource );
				}

				if ( FinalSimFlag ) goto LOOPSourceEnth_exit;
			}
			LOOPSourceEnth_exit: ;

			if ( SuctionPr < LowPressCutoff ) {
				ShowWarningError( "Heat pump:cooling shut down on low pressure" );
				WatertoAirHP( HPNum ).SimFlag = false;
			}

			if ( DischargePr > HighPressCutoff && ! FirstHVACIteration ) {
				ShowWarningError( "Heat pump:cooling shut down on high pressure" );
				WatertoAirHP( HPNum ).SimFlag = false;
			}

			if ( QSensible > QLoadTotal ) {
				QSensible = QLoadTotal;
			}

			if ( LatDegradModelSimFlag ) {
				if ( NumIteration4 == 1 ) {
					QLatRated = QLoadTotal - QSensible;

				} else if ( NumIteration4 == 2 ) {
					QLatActual = QLoadTotal - QSensible;
					SHRss = QSensible / QLoadTotal;
					LoadSideInletWBTemp = PsyTwbFnTdbWPb( LoadSideInletDBTemp, LoadSideInletHumRat, PB );
					SHReff = CalcEffectiveSHR( HPNum, SHRss, CyclingScheme, RuntimeFrac, QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp );
					//   Update sensible capacity based on effective SHR
					QSensible = QLoadTotal * SHReff;
					goto LOOPLatentDegradationModel_exit;
				}
			} else {

				SHReff = QSensible / QLoadTotal;
				goto LOOPLatentDegradationModel_exit;
			}
		}
		LOOPLatentDegradationModel_exit: ;

		//calculate coil outlet state variables
		LoadSideAirOutletEnth = LoadSideAirInletEnth - QLoadTotal / LoadSideMassFlowRate;
		LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible * LoadSideMassFlowRate_CpAir_inv;
		LoadSideOutletHumRat = PsyWFnTdbH( LoadSideOutletDBTemp, LoadSideAirOutletEnth );
		SourceSideOutletTemp = SourceSideInletTemp + QSource / ( SourceSideMassFlowRate * CpWater );

		// Actual outlet conditions are "average" for time step
		if ( CyclingScheme == ContFanCycCoil ) {
			// continuous fan, cycling compressor
			WatertoAirHP( HPNum ).OutletAirEnthalpy = PartLoadRatio * LoadSideAirOutletEnth + ( 1.0 - PartLoadRatio ) * LoadSideAirInletEnth;
			WatertoAirHP( HPNum ).OutletAirHumRat = PartLoadRatio * LoadSideOutletHumRat + ( 1.0 - PartLoadRatio ) * LoadSideInletHumRat;
			WatertoAirHP( HPNum ).OutletAirDBTemp = PsyTdbFnHW( WatertoAirHP( HPNum ).OutletAirEnthalpy, WatertoAirHP( HPNum ).OutletAirHumRat );
		} else {
			// default to cycling fan, cycling compressor
			WatertoAirHP( HPNum ).OutletAirEnthalpy = LoadSideAirOutletEnth;
			WatertoAirHP( HPNum ).OutletAirHumRat = LoadSideOutletHumRat;
			WatertoAirHP( HPNum ).OutletAirDBTemp = LoadSideOutletDBTemp;
		}

		//scale heat transfer rates and power to run time
		QLoadTotal *= PartLoadRatio;
		QSensible *= PartLoadRatio;
		Power *= RuntimeFrac;
		QSource *= PartLoadRatio;

		//Update heat pump data structure
		WatertoAirHP( HPNum ).Power = Power;
		WatertoAirHP( HPNum ).QLoadTotal = QLoadTotal;
		WatertoAirHP( HPNum ).QSensible = QSensible;
		WatertoAirHP( HPNum ).QLatent = QLoadTotal - QSensible;
		WatertoAirHP( HPNum ).QSource = QSource;
		WatertoAirHP( HPNum ).RunFrac = RuntimeFrac;
		WatertoAirHP( HPNum ).PartLoadRatio = PartLoadRatio;

		//  Air-side outlet conditions are already calculated above
		//  WatertoAirHP(HPNum)%OutletAirDBTemp=LoadSideOutletDBTemp
		//  WatertoAirHP(HPNum)%OutletAirHumRat=LoadsideOutletHumRat
		//  WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth

		WatertoAirHP( HPNum ).OutletAirMassFlowRate = LoadSideMassFlowRate;
		WatertoAirHP( HPNum ).OutletWaterTemp = SourceSideOutletTemp;
		WatertoAirHP( HPNum ).OutletWaterMassFlowRate = SourceSideMassFlowRate;
		WatertoAirHP( HPNum ).OutletWaterEnthalpy = SourceSideWaterInletEnth + QSource / SourceSideMassFlowRate;

	}

	Real64
	CalcCompSuctionTempResidual(
		Real64 const CompSuctionTemp, // HP compressor suction temperature (C)
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the compressor suction temperature for water to air HP's

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to call this Function to converge on a solution

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetSupHeatEnthalpyRefrig;
		using FluidProperties::GetSupHeatDensityRefrig;

		// Return value
		Real64 Residuum; // Result (force to 0)

		// Argument array dimensioning

		// Locals
		int RefrigIndex;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcWaterToAirHPHeating:CalcCompSuctionTemp" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string Refrigerant; // Name of refrigerant
		Real64 SuctionPr;
		Real64 CompSuctionEnth;
		Real64 SuperHeatEnth;

		// Convert parameters to usable variables
		SuctionPr = Par( 1 );
		RefrigIndex = int( Par( 2 ) );
		SuperHeatEnth = Par( 3 );

		CompSuctionEnth = GetSupHeatEnthalpyRefrig( Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex, RoutineName );

		// Calculate residual based on output calculation flag
		Residuum = ( CompSuctionEnth - SuperHeatEnth ) / SuperHeatEnth;

		return Residuum;
	}

	void
	CalcWatertoAirHPHeating(
		int const HPNum, // heat pump number
		int const CyclingScheme, // fan/compressor cycling scheme indicator
		bool const FirstHVACIteration, // first iteration flag
		Real64 const RuntimeFrac,
		bool const EP_UNUSED( InitFlag ), // first iteration flag
		Real64 const SensDemand,
		int const CompOp,
		Real64 const PartLoadRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hui Jin
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       R. Raustad (Oct 2006) Revised iteration technique
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a parameter estimation based water to air heat pump model

		// USE STATEMENTS:
		// na

		// Using/Aliasing
		using namespace FluidProperties;
		using Psychrometrics::PsyCpAirFnWTdb; // ,PsyHFnTdbRhPb,PsyWFnTdpPb
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyWFnTdbH;
		//  USE DataZoneEnergyDemands
		using General::RoundSigDigits;
		using General::SolveRegulaFalsi;
		using InputProcessor::SameString;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const CpWater( 4210.0 ); // Specific heat of water J/kg_C
		Real64 const DegreeofSuperheat( 80.0 ); // Initial guess of degree of superheat
		Real64 const gamma( 1.114 ); // Expnasion Coefficient
		Real64 const RelaxParam( 0.5 ); // Relaxation Parameter
		Real64 const ERR( 0.01 ); // Error Value
		int const STOP1( 10000 ); // Iteration stopper1
		int const STOP2( 100000 ); // Iteration stopper2
		int const STOP3( 100000 ); // Iteration stopper3

		static std::string const RoutineNameSourceSideInletTemp( "CalcWatertoAirHPHeating:SourceSideInletTemp" );
		static std::string const RoutineNameSourceSideTemp( "CalcWatertoAirHPHeating:SourceSideTemp" );
		static std::string const RoutineNameLoadSideTemp( "CalcWatertoAirHPHeating:LoadSideTemp" );
		static std::string const RoutineNameCompressInletTemp( "CalcWatertoAirHPHeating:CompressInletTemp" );
		static std::string const RoutineNameSuctionPr( "CalcWatertoAirHPHeating:SuctionPr" );
		static std::string const RoutineNameCompSuctionTemp( "CalcWatertoAirHPHeating:CompSuctionTemp");

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//      INTEGER                :: NumIteration1            ! Number of Iteration1
		int NumIteration2; // Number of Iteration2
		int NumIteration3; // Number of Iteration3
		int SourceSideFluidIndex; // Source Side Fluid Index

		int CompressorType; // Type of Compressor ie. Reciprocating,Rotary or Scroll
		std::string SourceSideFluidName; // Name of source side fluid
		std::string Refrigerant; // Name of refrigerant
		//      CHARACTER(len=25) :: CErrCount
		Real64 NominalHeatingCapacity; // Nominal Heating Capacity(W)
		Real64 LoadSideUA; // Load Side Heat Transfer coefficient [W/C]
		Real64 SourceSideUA; // Source Side Heat Transfer coefficient [W/C]
		Real64 PressureDrop; // Suction or Discharge Pressure Drop [Pa]
		Real64 ClearanceFactor; // Compressor Clearance Factor
		Real64 PistonDisp; // Compressor Piston Displacement [m3/s]
		Real64 ShTemp; // Superheat Temperature [C]
		Real64 LosFac; // Compressor Power Loss Factor
		Real64 PowerLos; // Constant Part of Power Losses [kW]
		Real64 RefVolFlowRate; // Refrigerant Volume Flow rate at the beginning
		Real64 VolumeRatio; // Built-in-volume ratio [~]
		Real64 LeakRateCoeff; // Coefficient for the relationship between
		// Pressure Ratio and Leakage Rate [~]
		Real64 SourceSideHTRes1; // Source Side Heat Transfer Resistance coefficient 1 [~]
		Real64 SourceSideHTRes2; // Source Side Heat Transfer Resistance coefficient 2 [K/kW]
		Real64 HighPressCutoff; // High Pressure Cut-off [Pa]
		Real64 LowPressCutoff; // Low Pressure Cut-off [Pa]

		Real64 Quality;
		Real64 SourceSideMassFlowRate; // Source Side Mass Flow Rate [kg/s]
		Real64 SourceSideInletTemp; // Source Side Inlet Temperature [C]
		Real64 SourceSideWaterInletEnth; // Source Side Inlet Water Enthalpy [J/kg]
		Real64 SourceSideOutletTemp; // Source Side Outlet Temperature [C]
		Real64 SourceSideVolFlowRate; // Source Side Volumetric Flow Rate [m3/s]
		Real64 CpFluid; // Specific heat of source side fluid(J/kg)
		Real64 LoadSideMassFlowRate; // Load Side Mass Flow Rate [kg/s]
		Real64 LoadSideInletDBTemp; // Load Side Inlet Dry Bulb Temp [C]
		Real64 LoadSideInletHumRat; // Load Side Inlet Humidity Ratio [kg/kg]
		Real64 LoadSideOutletDBTemp; // Load Side Outlet Dry Bulb Temperature [C]
		Real64 LoadSideOutletHumRat; // Load Side Outlet Humidity Ratio [kg/kg]
		Real64 LoadSideAirInletEnth; // Load Side Inlet Enthalpy [J/kg]
		Real64 LoadSideAirOutletEnth; // Load Side Outlet Enthalpy [J/kg]
		Real64 CpAir; // Specific Heat of Air [J/kg_C]
		Real64 DegradFactor; // Degradation Factor [~]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 QLoadTotal; // Load Side Heat Transfer Rate [W]
		Real64 Power; // Power Consumption [W]

		Real64 SourceSideEffect; // Source Side Heat Exchanger Effectiveness
		Real64 SourceSideTemp; // Source Side Saturated Refrigerant Temperature [C]
		Real64 LoadSideTemp; // Load Side Saturated Refrigerant Temperature [C]
		Real64 SourceSidePressure; // Source Side Saturated Refrigerant Pressure [Pa]
		Real64 LoadSidePressure; // Load Side Saturated Refrigerant Pressure [Pa]
		Real64 SuctionPr; // Compressor Suction Pressure [Pa]
		Real64 DischargePr; // Compressor Discharge Pressure [Pa]
		Real64 CompressInletTemp; // Temperature of the Refrigerant Entering the Compressor [C]
		Real64 MassRef; // Mass Flow Rate of Refrigerant [kg/s]
		Real64 SourceSideOutletEnth; // Enthalpy of Refrigerant leaving the Source Side Heat Exchanger [J/kg]
		Real64 LoadSideOutletEnth; // Enthalpy of Refrigerant leaving the Load Side Heat Exchanger [J/kg]
		static Real64 initialQSource; // Guess Source Side Heat Transfer Rate [W]
		static Real64 initialQLoad; // Guess Load Side Heat Transfer rate [W]
		Real64 SuperHeatEnth; // Enthalpy of the Superheated Refrigerant [J/kg]
		Real64 CompSuctionTemp1; // Guess of the Temperature of the Refrigerant Entering the
		// Compressor #1 [C]
		Real64 CompSuctionTemp2; // Guess of the Temperature of the Refrigerant Entering the
		// Compressor #2 [C]
		Real64 CompSuctionTemp; // Temperature of the Refrigerant Entering the Compressor [C]
		Real64 CompSuctionEnth; // Enthalpy of the Refrigerant Entering the Compressor [J/kg]
		Real64 CompSuctionDensity; // Density of the Refrigerant Entering the Compressorkg/m3
		Real64 CompSuctionSatTemp; // Temperature of Saturated Refrigerant at Compressor Suction Pressure [C]
		bool FinalSimFlag; // Final Simulation Flag
		bool Converged; // Overall convergence Flag
		Array1D< Real64 > Par( 4 ); // Parameter array passed to RegulaFalsi function
		int SolFlag; // Solution flag returned from RegulaFalsi function

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

		NominalHeatingCapacity = WatertoAirHP( HPNum ).HeatingCapacity;
		CompressorType = WatertoAirHP( HPNum ).CompressorType;
		Refrigerant = WatertoAirHP( HPNum ).Refrigerant;

		LoadSideUA = WatertoAirHP( HPNum ).LoadSideTotalUACoeff;
		PistonDisp = WatertoAirHP( HPNum ).CompPistonDisp;
		ClearanceFactor = WatertoAirHP( HPNum ).CompClearanceFactor;
		PressureDrop = WatertoAirHP( HPNum ).CompSucPressDrop;
		ShTemp = WatertoAirHP( HPNum ).SuperheatTemp;
		PowerLos = WatertoAirHP( HPNum ).PowerLosses;
		LosFac = WatertoAirHP( HPNum ).LossFactor;
		RefVolFlowRate = WatertoAirHP( HPNum ).RefVolFlowRate;
		VolumeRatio = WatertoAirHP( HPNum ).VolumeRatio;
		LeakRateCoeff = WatertoAirHP( HPNum ).LeakRateCoeff;
		HighPressCutoff = WatertoAirHP( HPNum ).HighPressCutoff;
		LowPressCutoff = WatertoAirHP( HPNum ).LowPressCutoff;
		SourceSideUA = WatertoAirHP( HPNum ).SourceSideUACoeff;
		SourceSideHTRes1 = WatertoAirHP( HPNum ).SourceSideHTR1;
		SourceSideHTRes2 = WatertoAirHP( HPNum ).SourceSideHTR2;

		LoadSideMassFlowRate = WatertoAirHP( HPNum ).InletAirMassFlowRate;
		LoadSideInletDBTemp = WatertoAirHP( HPNum ).InletAirDBTemp;
		LoadSideInletHumRat = WatertoAirHP( HPNum ).InletAirHumRat;
		CpAir = PsyCpAirFnWTdb( LoadSideInletHumRat, LoadSideInletDBTemp );

		SourceSideInletTemp = WatertoAirHP( HPNum ).InletWaterTemp;
		SourceSideWaterInletEnth = WatertoAirHP( HPNum ).InletWaterEnthalpy;
		SourceSideFluidName = PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidName;
		SourceSideFluidIndex = PlantLoop( WatertoAirHP( HPNum ).LoopNum ).FluidIndex;
		SourceSideMassFlowRate = WatertoAirHP( HPNum ).InletWaterMassFlowRate;
		SourceSideVolFlowRate = WatertoAirHP( HPNum ).InletWaterMassFlowRate / GetDensityGlycol( SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp );

		// Load Side Inlet Air Enthalpy
		LoadSideAirInletEnth = WatertoAirHP( HPNum ).InletAirEnthalpy;

		// If heat pump is not operating, return
		if ( SensDemand == 0.0 || LoadSideMassFlowRate <= 0.0 || SourceSideMassFlowRate <= 0.0 ) {
			WatertoAirHP( HPNum ).SimFlag = false;
			return;
		} else {
			WatertoAirHP( HPNum ).SimFlag = true;
		}

		if ( CompOp == 0 ) {
			WatertoAirHP( HPNum ).SimFlag = false;
			return;
		}

		if ( FirstHVACIteration ) {
			initialQLoad = NominalHeatingCapacity;
			initialQSource = NominalHeatingCapacity;
		}

		if ( initialQLoad == 0.0 ) initialQLoad = NominalHeatingCapacity;
		if ( initialQSource == 0.0 ) initialQSource = NominalHeatingCapacity;

		//Tuned Hoisted quantities out of nested loop that don't change
		Real64 const LoadSideMassFlowRate_CpAir_inv( 1.0 / ( LoadSideMassFlowRate * CpAir ) );
		Real64 const LoadSideEffect( 1.0 - std::exp( -LoadSideUA * LoadSideMassFlowRate_CpAir_inv ) ); // Load Side Effectiveness based on Outside Heat Transfer Coefficient
		Real64 const LoadSideEffect_CpAir_MassFlowRate_inv( 1.0 / ( LoadSideEffect * CpAir * LoadSideMassFlowRate ) );

		//Outerloop: calculate load side heat transfer
		NumIteration3 = 0;
		Converged = false;
		FinalSimFlag = false;
		while ( true ) {
			if ( Converged ) FinalSimFlag = true;

			++NumIteration3;

			if ( NumIteration3 > STOP3 ) {
				WatertoAirHP( HPNum ).SimFlag = false;
				return;
			}

			//Innerloop: calculate load side heat transfer
			NumIteration2 = 0;
			while ( true ) {

				++NumIteration2;

				if ( NumIteration2 > STOP2 ) {
					WatertoAirHP( HPNum ).SimFlag = false;
					return;
				}

				// Determine Effectiveness of Source Side
				CpFluid = GetSpecificHeatGlycol( SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex, RoutineNameSourceSideInletTemp );

				//      IF (SourceSideFluidName=='WATER') THEN
				if ( SourceSideFluidIndex == WaterIndex ) {
					SourceSideEffect = 1.0 - std::exp( -SourceSideUA / ( CpFluid * SourceSideMassFlowRate ) ); // SourceSideFluidName=='Water'
				} else {
					DegradFactor = DegradF( SourceSideFluidName, SourceSideInletTemp, SourceSideFluidIndex );
					SourceSideEffect = 1.0 / ( ( SourceSideHTRes1 * std::pow( SourceSideVolFlowRate, -0.8 ) ) / DegradFactor + SourceSideHTRes2 );
				}

				// Determine Source Side Tempertaure (Evap. Temp for this mode)
				SourceSideTemp = SourceSideInletTemp - initialQSource / ( SourceSideEffect * CpFluid * SourceSideMassFlowRate );

				// Determine Load Side Tempertaure (Condensing Temp for this mode)
				LoadSideTemp = LoadSideInletDBTemp + initialQLoad * LoadSideEffect_CpAir_MassFlowRate_inv;

				// Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
				SourceSidePressure = GetSatPressureRefrig( Refrigerant, SourceSideTemp, RefrigIndex, RoutineNameSourceSideTemp );
				LoadSidePressure = GetSatPressureRefrig( Refrigerant, LoadSideTemp, RefrigIndex, RoutineNameLoadSideTemp );
				if ( SourceSidePressure < LowPressCutoff && ! FirstHVACIteration ) {
					if ( ! WarmupFlag ) {
						ShowRecurringWarningErrorAtEnd( "WaterToAir Heat pump:heating [" + WatertoAirHP( HPNum ).Name + "] shut off on low pressure < " + RoundSigDigits( LowPressCutoff, 0 ), WatertoAirHP( HPNum ).LowPressHtgError, SourceSidePressure, SourceSidePressure, _, "[Pa]", "[Pa]" );
					}
					WatertoAirHP( HPNum ).SimFlag = false;
					return;
				}

				if ( LoadSidePressure > HighPressCutoff && ! FirstHVACIteration ) {
					if ( ! WarmupFlag ) {
						ShowRecurringWarningErrorAtEnd( "WaterToAir Heat pump:heating [" + WatertoAirHP( HPNum ).Name + "] shut off on high pressure > " + RoundSigDigits( HighPressCutoff, 0 ), WatertoAirHP( HPNum ).HighPressHtgError, SourceSideInletTemp, SourceSideInletTemp, _, "SourceSideInletTemp[C]", "SourceSideInletTemp[C]" );
					}
					//         CALL ShowWarningError('Heat pump:heating shut off on high pressure')
					//         WRITE(CErrCount,*) SourceSideInletTemp
					//         CErrCount=ADJUSTL(CErrCount)
					//         CALL ShowContinueError('Source side inlet temperature too low, T='//TRIM(CErrCount))
					//         CALL ShowContinueError('Heat pump heating demand not met by plant side')
					WatertoAirHP( HPNum ).SimFlag = false;
					return;
				}

				// Determine Suction Pressure at Compressor Entrance & Discharge Pressure at Compressor Exit
				{ auto const SELECT_CASE_var( CompressorType );
				if ( SELECT_CASE_var == CompressorType_Reciprocating ) { // RECIPROCATING
					SuctionPr = SourceSidePressure - PressureDrop;
					DischargePr = LoadSidePressure + PressureDrop;
				} else if ( SELECT_CASE_var == CompressorType_Rotary ) { // ROTARY
					SuctionPr = SourceSidePressure;
					DischargePr = LoadSidePressure + PressureDrop;
				} else if ( SELECT_CASE_var == CompressorType_Scroll ) { // SCROLL
					SuctionPr = SourceSidePressure;
					DischargePr = LoadSidePressure;
				}}

				// Determine the Source Side Outlet Enthalpy
				// Quality of the refrigerant leaving the evaporator is saturated gas
				Quality = 1.0;
				SourceSideOutletEnth = GetSatEnthalpyRefrig( Refrigerant, SourceSideTemp, Quality, RefrigIndex, RoutineNameSourceSideTemp );

				// Determine Load Side Outlet Enthalpy
				// Quality of the refrigerant leaving the condenser is saturated liguid
				Quality = 0.0;
				LoadSideOutletEnth = GetSatEnthalpyRefrig( Refrigerant, LoadSideTemp, Quality, RefrigIndex, RoutineNameLoadSideTemp );

				// Determine Superheated Temperature of the Source Side outlet/compressor Inlet
				CompressInletTemp = SourceSideTemp + ShTemp;

				// Determine the Enathalpy of the Superheated Fluid at Source Side Outlet/Compressor Inlet
				SuperHeatEnth = GetSupHeatEnthalpyRefrig( Refrigerant, CompressInletTemp, SourceSidePressure, RefrigIndex, RoutineNameCompressInletTemp );

				// Determining the suction state of the fluid from inlet state involves interation
				// Method employed...
				// Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
				// check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

				if ( ! Converged ) {
					CompSuctionSatTemp = GetSatTemperatureRefrig( Refrigerant, SuctionPr, RefrigIndex, RoutineNameSuctionPr );
					CompSuctionTemp1 = CompSuctionSatTemp;

					// Shoot into the Superheated Region
					CompSuctionTemp2 = CompSuctionSatTemp + DegreeofSuperheat;
				}

				//       ! Iterate to find the Suction State
				//       NumIteration1=0
				//       LOOP: DO
				//           NumIteration1=NumIteration1+1
				//           IF (NumIteration1.GT.STOP1) THEN
				//             WatertoAirHP(HPNum)%SimFlag = .FALSE.
				//             RETURN
				//           END IF
				//               CompSuctionTemp = 0.5d0 * ( CompSuctionTemp1 + CompSuctionTemp2 )
				//               CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
				//               CompSuctionDensity = GetSupHeatDensityRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
				//               IF (ABS(CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth < ERR)  THEN
				//                   EXIT LOOP
				//               END IF
				//               IF ( CompsuctionEnth < SuperHeatEnth ) THEN
				//                   CompSuctionTemp1 = CompSuctionTemp
				//               ELSE
				//                   CompSuctionTemp2 = CompSuctionTemp
				//               END IF
				//        END DO LOOP

				//       Do not need the name of the refrigerant if we already have the index (from above CALLs)
				Par( 1 ) = SuctionPr;
				Par( 2 ) = double( RefrigIndex );
				Par( 3 ) = SuperHeatEnth;

				SolveRegulaFalsi( ERR, STOP1, SolFlag, CompSuctionTemp, CalcCompSuctionTempResidual, CompSuctionTemp1, CompSuctionTemp2, Par );
				if ( SolFlag == -1 ) {
					WatertoAirHP( HPNum ).SimFlag = false;
					return;
				}
				CompSuctionEnth = GetSupHeatEnthalpyRefrig( Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex, RoutineNameCompSuctionTemp );
				CompSuctionDensity = GetSupHeatDensityRefrig( Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex, RoutineNameCompSuctionTemp );

				// Find Refrigerant Flow Rate
				{ auto const SELECT_CASE_var( CompressorType );
				if ( SELECT_CASE_var == CompressorType_Reciprocating ) { // RECIPROCATING
					MassRef = PistonDisp * CompSuctionDensity * ( 1 + ClearanceFactor - ClearanceFactor * std::pow( DischargePr / SuctionPr, 1 / gamma ) );
				} else if ( SELECT_CASE_var == CompressorType_Rotary ) { // ROTARY
					MassRef = PistonDisp * CompSuctionDensity;
				} else if ( SELECT_CASE_var == CompressorType_Scroll ) { // SCROLL
					MassRef = RefVolFlowRate * CompSuctionDensity - LeakRateCoeff * ( DischargePr / SuctionPr );
				}}

				// Find the Source Side Heat Transfer
				QSource = MassRef * ( SourceSideOutletEnth - LoadSideOutletEnth );

				if ( std::abs( QSource - initialQSource ) / initialQSource < ERR ) {
					goto LOOPSourceEnth_exit;
				} else {
					initialQSource += RelaxParam * ( QSource - initialQSource );
				}
			}
			LOOPSourceEnth_exit: ;

			// Determine the Power Consumption
			{ auto const SELECT_CASE_var( CompressorType );
			if ( SELECT_CASE_var == CompressorType_Reciprocating ) { // RECIPROCATING
				Power = PowerLos + ( 1 / LosFac ) * ( MassRef * gamma / ( gamma - 1 ) * SuctionPr / CompSuctionDensity * ( std::pow( DischargePr / SuctionPr, ( gamma - 1 ) / gamma ) - 1 ) );
			} else if ( SELECT_CASE_var == CompressorType_Rotary ) { // ROTARY
				Power = PowerLos + ( 1 / LosFac ) * ( MassRef * gamma / ( gamma - 1 ) * SuctionPr / CompSuctionDensity * ( std::pow( DischargePr / SuctionPr, ( gamma - 1 ) / gamma ) - 1 ) );
			} else if ( SELECT_CASE_var == CompressorType_Scroll ) { // SCROLL
				Power = PowerLos + ( 1 / LosFac ) * ( gamma / ( gamma - 1 ) ) * SuctionPr * RefVolFlowRate * ( ( ( gamma - 1 ) / gamma ) * ( ( DischargePr / SuctionPr ) / VolumeRatio ) + ( ( 1 / gamma ) * std::pow( VolumeRatio, gamma - 1 ) ) - 1 );
			}}

			// Determine the Load Side Heat Rate
			QLoadTotal = Power + QSource;

			if ( std::abs( QLoadTotal - initialQLoad ) / initialQLoad < ERR ) {
				Converged = true;
			} else {
				initialQLoad += RelaxParam * ( QLoadTotal - initialQLoad );
			}

			if ( FinalSimFlag ) goto LOOPLoadEnth_exit;
		}
		LOOPLoadEnth_exit: ;

		if ( SuctionPr < LowPressCutoff && ! FirstHVACIteration ) {
			ShowWarningError( "Heat pump:heating shut down on low pressure" );
			WatertoAirHP( HPNum ).SimFlag = false;
			return;
		}

		if ( DischargePr > HighPressCutoff && ! FirstHVACIteration ) {
			ShowWarningError( "Heat pump:heating shut down on high pressure" );
			WatertoAirHP( HPNum ).SimFlag = false;
			return;
		}

		//calculate coil outlet state variables
		LoadSideAirOutletEnth = LoadSideAirInletEnth + QLoadTotal / LoadSideMassFlowRate;
		LoadSideOutletDBTemp = LoadSideInletDBTemp + QLoadTotal / ( LoadSideMassFlowRate * CpAir );
		LoadSideOutletHumRat = PsyWFnTdbH( LoadSideOutletDBTemp, LoadSideAirOutletEnth );
		SourceSideOutletTemp = SourceSideInletTemp - QSource / ( SourceSideMassFlowRate * CpWater );

		// Calculate actual outlet conditions for the run time fraction
		// Actual outlet conditions are "average" for time step
		if ( CyclingScheme == ContFanCycCoil ) {
			// continuous fan, cycling compressor
			WatertoAirHP( HPNum ).OutletAirEnthalpy = PartLoadRatio * LoadSideAirOutletEnth + ( 1.0 - PartLoadRatio ) * LoadSideAirInletEnth;
			WatertoAirHP( HPNum ).OutletAirHumRat = PartLoadRatio * LoadSideOutletHumRat + ( 1.0 - PartLoadRatio ) * LoadSideInletHumRat;
			WatertoAirHP( HPNum ).OutletAirDBTemp = PsyTdbFnHW( WatertoAirHP( HPNum ).OutletAirEnthalpy, WatertoAirHP( HPNum ).OutletAirHumRat );
		} else {
			// default to cycling fan, cycling compressor
			WatertoAirHP( HPNum ).OutletAirEnthalpy = LoadSideAirOutletEnth;
			WatertoAirHP( HPNum ).OutletAirHumRat = LoadSideOutletHumRat;
			WatertoAirHP( HPNum ).OutletAirDBTemp = LoadSideOutletDBTemp;
		}
		//scale heat transfer rates and power to run time
		QLoadTotal *= PartLoadRatio;
		Power *= RuntimeFrac;
		QSource *= PartLoadRatio;

		//Update heat pump data structure
		WatertoAirHP( HPNum ).Power = Power;
		WatertoAirHP( HPNum ).QLoadTotal = QLoadTotal;
		WatertoAirHP( HPNum ).QSensible = QLoadTotal;

		WatertoAirHP( HPNum ).QSource = QSource;
		WatertoAirHP( HPNum ).RunFrac = RuntimeFrac;
		WatertoAirHP( HPNum ).PartLoadRatio = PartLoadRatio;

		//  Air-side outlet conditions are already calculated above
		//  WatertoAirHP(HPNum)%OutletAirDBTemp=LoadSideOutletDBTemp
		//  WatertoAirHP(HPNum)%OutletAirHumRat=LoadsideOutletHumRat
		//  WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth

		WatertoAirHP( HPNum ).OutletAirMassFlowRate = LoadSideMassFlowRate;
		WatertoAirHP( HPNum ).OutletWaterTemp = SourceSideOutletTemp;
		WatertoAirHP( HPNum ).OutletWaterMassFlowRate = SourceSideMassFlowRate;
		WatertoAirHP( HPNum ).OutletWaterEnthalpy = SourceSideWaterInletEnth - QSource / SourceSideMassFlowRate;

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the WatertoAirHP Module
	// *****************************************************************************

	void
	UpdateWatertoAirHP( int const HPNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hui Jin
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the Water to Air Heat Pump outlet nodes.

		// METHODOLOGY EMPLOYED:
		// Data is moved from the HP data structure to the HP outlet nodes.

		// REFERENCES:

		// Using/Aliasing
		using PlantUtilities::SafeCopyPlantNode;
		using DataHVACGlobals::TimeStepSys;
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

		ReportingConstant = TimeStepSys * SecInHour;
		//WatertoAirHP(HPNum)%SimFlag=.FALSE.
		if ( ! WatertoAirHP( HPNum ).SimFlag ) {
			// Heatpump is off; just pass through conditions
			WatertoAirHP( HPNum ).Power = 0.0;
			WatertoAirHP( HPNum ).Energy = 0.0;
			WatertoAirHP( HPNum ).QLoadTotal = 0.0;
			WatertoAirHP( HPNum ).QSensible = 0.0;
			WatertoAirHP( HPNum ).QLatent = 0.0;
			WatertoAirHP( HPNum ).QSource = 0.0;
			// These will be overwritten below based on variables above that are already set to 0.
			//  WatertoAirHP(HPNum)%EnergyLoadTotal=0.0
			//  WatertoAirHP(HPNum)%EnergySensible=0.0
			//  WatertoAirHP(HPNum)%EnergySource=0.0
			//  WatertoAirHP(HPNum)%EnergyLatent=0.0
			WatertoAirHP( HPNum ).RunFrac = 0.0;
			WatertoAirHP( HPNum ).PartLoadRatio = 0.0;
			WatertoAirHP( HPNum ).OutletAirDBTemp = WatertoAirHP( HPNum ).InletAirDBTemp;
			WatertoAirHP( HPNum ).OutletAirHumRat = WatertoAirHP( HPNum ).InletAirHumRat;
			WatertoAirHP( HPNum ).OutletWaterTemp = WatertoAirHP( HPNum ).InletWaterTemp;
			WatertoAirHP( HPNum ).OutletAirMassFlowRate = WatertoAirHP( HPNum ).InletAirMassFlowRate;
			WatertoAirHP( HPNum ).OutletWaterMassFlowRate = WatertoAirHP( HPNum ).InletWaterMassFlowRate;
			WatertoAirHP( HPNum ).OutletAirEnthalpy = WatertoAirHP( HPNum ).InletAirEnthalpy;
			WatertoAirHP( HPNum ).OutletWaterEnthalpy = WatertoAirHP( HPNum ).InletWaterEnthalpy;
		}

		AirInletNode = WatertoAirHP( HPNum ).AirInletNodeNum;
		WaterInletNode = WatertoAirHP( HPNum ).WaterInletNodeNum;
		AirOutletNode = WatertoAirHP( HPNum ).AirOutletNodeNum;
		WaterOutletNode = WatertoAirHP( HPNum ).WaterOutletNodeNum;

		// Set the outlet air nodes of the WatertoAirHP
		Node( AirOutletNode ).MassFlowRate = Node( AirInletNode ).MassFlowRate;
		Node( AirOutletNode ).Temp = WatertoAirHP( HPNum ).OutletAirDBTemp;
		Node( AirOutletNode ).HumRat = WatertoAirHP( HPNum ).OutletAirHumRat;
		Node( AirOutletNode ).Enthalpy = WatertoAirHP( HPNum ).OutletAirEnthalpy;

		// Set the outlet nodes for properties that just pass through & not used
		SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
		// Set the outlet water nodes for the heat pump
		Node( WaterOutletNode ).Temp = WatertoAirHP( HPNum ).OutletWaterTemp;
		Node( WaterOutletNode ).Enthalpy = WatertoAirHP( HPNum ).OutletWaterEnthalpy;

		// Set the outlet nodes for properties that just pass through & not used
		Node( AirOutletNode ).Quality = Node( AirInletNode ).Quality;
		Node( AirOutletNode ).Press = Node( AirInletNode ).Press;
		Node( AirOutletNode ).MassFlowRateMin = Node( AirInletNode ).MassFlowRateMin;
		Node( AirOutletNode ).MassFlowRateMax = Node( AirInletNode ).MassFlowRateMax;
		Node( AirOutletNode ).MassFlowRateMinAvail = Node( AirInletNode ).MassFlowRateMinAvail;
		Node( AirOutletNode ).MassFlowRateMaxAvail = Node( AirInletNode ).MassFlowRateMaxAvail;

		// Pass through the load side mass flow rates
		WatertoAirHP( HPNum ).InletAirMassFlowRate = Node( AirInletNode ).MassFlowRate;
		WatertoAirHP( HPNum ).OutletAirMassFlowRate = WatertoAirHP( HPNum ).InletAirMassFlowRate;

		WatertoAirHP( HPNum ).Energy = WatertoAirHP( HPNum ).Power * ReportingConstant;
		WatertoAirHP( HPNum ).EnergyLoadTotal = WatertoAirHP( HPNum ).QLoadTotal * ReportingConstant;
		WatertoAirHP( HPNum ).EnergySensible = WatertoAirHP( HPNum ).QSensible * ReportingConstant;
		WatertoAirHP( HPNum ).EnergyLatent = WatertoAirHP( HPNum ).QLatent * ReportingConstant;
		WatertoAirHP( HPNum ).EnergySource = WatertoAirHP( HPNum ).QSource * ReportingConstant;

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNode ).CO2 = Node( AirInletNode ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutletNode ).GenContam = Node( AirInletNode ).GenContam;
		}

	}

	//        End of Update subroutines for the WatertoAirHP Module
	// *****************************************************************************

	Real64
	CalcEffectiveSHR(
		int const HPNum, // Index number for cooling coil
		Real64 const SHRss, // Steady-state sensible heat ratio
		int const CyclingScheme, // fan/compressor cycling scheme indicator
		Real64 const RTF, // Compressor run-time fraction
		Real64 const QLatRated, // Rated latent capacity
		Real64 const QLatActual, // Actual latent capacity
		Real64 const EnteringDB, // Entering air dry-bulb temperature
		Real64 const EnteringWB // Entering air wet-bulb temperature
	)
	{

		// FUNCTION INFORMATION:
		//    AUTHOR         Richard Raustad, FSEC
		//    DATE WRITTEN   September 2003
		//    MODIFIED       Kenneth Tang (Aug 2004) Added capability for simulating CycFanCycCoil
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
		// REFERENCES:
		//   "A Model to Predict the Latent Capacity of Air Conditioners and
		//    Heat Pumps at Part-Load Conditions with Constant Fan Operation"
		//    1996 ASHRAE Transactions, Volume 102, Part 1, Pp. 266 - 274,
		//    Hugh I. Henderson, Jr., P.E., Kannan Rengarajan, P.E.

		// USE STATEMENTS:

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
		//   at the current operating conditions (sec)
		Real64 Gamma; // Initial moisture evaporation rate divided by steady-state AC latent capacity
		//   at the current operating conditions
		Real64 Twet_Rated; // Twet at rated conditions (coil air flow rate and air temperatures), sec
		Real64 Gamma_Rated; // Gamma at rated conditions (coil air flow rate and air temperatures)
		Real64 Twet_max; // Maximum allowed value for Twet
		Real64 MaxONOFFCyclesperHour; // Maximum cycling rate of heat pump [cycles/hr]
		Real64 HPTimeConstant; // Heat pump time constant [s]
		Real64 FanDelayTime; // Fan delay time, time delay for the HP's fan to
		//shut off after compressor cycle off  [s]

		Real64 Ton; // Coil on time (sec)
		Real64 Toff; // Coil off time (sec)
		Real64 Toffa; // Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
		Real64 aa; // Intermediate variable
		Real64 To1; // Intermediate variable (first guess at To). To = time to the start of moisture removal
		Real64 To2; // Intermediate variable (second guess at To). To = time to the start of moisture removal
		Real64 Error; // Error for iteration (DO) loop
		Real64 LHRmult; // Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

		Twet_Rated = WatertoAirHP( HPNum ).Twet_Rated;
		Gamma_Rated = WatertoAirHP( HPNum ).Gamma_Rated;
		MaxONOFFCyclesperHour = WatertoAirHP( HPNum ).MaxONOFFCyclesperHour;
		HPTimeConstant = WatertoAirHP( HPNum ).HPTimeConstant;
		FanDelayTime = WatertoAirHP( HPNum ).FanDelayTime;

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
			//  For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
			//  until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
			Toff = FanDelayTime;
		} else {
			//  For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
			//  for the entire heat pump off-cycle.
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

	Real64
	DegradF(
		std::string & FluidName, // Name of glycol used in source side
		Real64 & Temp, // Temperature of the fluid
		int & FluidIndex // Index number for the fluid
	)
	{
		// FUNCTION INFORMATION:
		//    AUTHOR         Kenneth Tang
		//    DATE WRITTEN   October 2004
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//    Calculate the degradation factor to predict the heat pump performance
		//    when antifreeze is used.
		// METHODOLOGY EMPLOYED:
		//    Use FluidProperties to calculate the properties of water and glycol
		//    at the given temperature. Then substitute the properties into the equation.
		// REFERENCES:
		//    Jin, H. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd Thesis.
		//    Oklahoma State University.

		// Using/Aliasing
		using namespace FluidProperties;

		// FUNCTION ARGUMENT DEFINITIONS:

		// Return value
		Real64 DegradF;

		// Locals
		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const CalledFrom( "HVACWaterToAir:DegradF" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 VisWater; // Viscosity of water [mPa-s]
		Real64 DensityWater; // Density of water [kg/m3]
		Real64 CpWater; // Specific heat of water [J/kg-K]
		Real64 CondWater; // Conductivity of water [W/m-K]
		Real64 VisCoolant; // Viscosity of water [mPa-s]
		Real64 DensityCoolant; // Density of water [kg/m3]
		Real64 CpCoolant; // Specific heat of water [J/kg-K]
		Real64 CondCoolant; // Conductivity of water [W/m-K]

		VisWater = GetViscosityGlycol( fluidNameWater, Temp, WaterIndex, CalledFrom );
		DensityWater = GetDensityGlycol( fluidNameWater, Temp, WaterIndex, CalledFrom );
		CpWater = GetSpecificHeatGlycol( fluidNameWater, Temp, WaterIndex, CalledFrom );
		CondWater = GetConductivityGlycol( fluidNameWater, Temp, WaterIndex, CalledFrom );
		VisCoolant = GetViscosityGlycol( FluidName, Temp, FluidIndex, CalledFrom );
		DensityCoolant = GetDensityGlycol( FluidName, Temp, FluidIndex, CalledFrom );
		CpCoolant = GetSpecificHeatGlycol( FluidName, Temp, FluidIndex, CalledFrom );
		CondCoolant = GetConductivityGlycol( FluidName, Temp, FluidIndex, CalledFrom );

		DegradF = std::pow( VisCoolant / VisWater, -0.47 ) * std::pow( DensityCoolant / DensityWater, 0.8 ) * std::pow( CpCoolant / CpWater, 0.33 ) * std::pow( CondCoolant / CondWater, 0.67 );

		return DegradF;
	}

	int
	GetCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   August 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the index.  If
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
			GetWatertoAirHPInput();
			WaterIndex = FindGlycol( fluidNameWater ); //Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		IndexNum = FindItemInList( CoilName, WatertoAirHP );

		if ( IndexNum == 0 ) {
			ShowSevereError( "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
		}

		return IndexNum;

	}

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the coil capacity for the given coil and returns it.  If
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
			WaterIndex = FindGlycol( fluidNameWater ); //Initialize the WaterIndex once
			GetWatertoAirHPInput();
			GetCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION" ) || SameString( CoilType, "COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION" ) ) {
			WhichCoil = FindItemInList( CoilName, WatertoAirHP );
			if ( WhichCoil != 0 ) {
				if ( SameString( CoilType, "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION" ) ) {
					CoilCapacity = WatertoAirHP( WhichCoil ).HeatingCapacity;
				} else {
					CoilCapacity = WatertoAirHP( WhichCoil ).CoolingCapacity;
				}
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			CoilCapacity = -1000.0;
		}

		return CoilCapacity;

	}

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
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
			GetWatertoAirHPInput();
			WaterIndex = FindGlycol( fluidNameWater ); //Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, WatertoAirHP );
		if ( WhichCoil != 0 ) {
			NodeNumber = WatertoAirHP( WhichCoil ).AirInletNodeNum;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   July 2007
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
			GetWatertoAirHPInput();
			WaterIndex = FindGlycol( fluidNameWater ); //Initialize the WaterIndex once
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, WatertoAirHP );
		if ( WhichCoil != 0 ) {
			NodeNumber = WatertoAirHP( WhichCoil ).AirOutletNodeNum;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

} // WaterToAirHeatPump

} // EnergyPlus
