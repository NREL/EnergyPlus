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
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <HeatPumpWaterToWaterCOOLING.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterCOOLING {
	// Module containing the routines dealing with the Water to Water Heat Pump (Cooling)

	// MODULE INFORMATION:
	//       AUTHOR         ARUN
	//       DATE WRITTEN   7/18/2000
	//       MODIFIED       ARUN: 6/27/2002: Cycle Time
	//                      L Lawrie: V1.1.1 (5/20/2003) add meters and energy to several reporting variables
	//                      L Lawrie: V1.1.1 (5/20/2003) restructure modules to comply with standard templates
	//                      B Griffith, Sept 2010, plant upgrades, general fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates a water to Water Heat Pump (Cooling)

	// METHODOLOGY EMPLOYED:
	// This simulation is based on a set of selected parameters,
	// Which are obtained using Parameter Estimation technique.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginSimFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::HourOfDay;
	using DataGlobals::TimeStep;
	using DataGlobals::TimeStepZone;
	using DataGlobals::DayOfSim;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SecInHour;
	using namespace DataLoopNode;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	std::string const ModuleCompName( "HeatPump:WaterToWater:ParameterEstimation:Cooling" );
	std::string const ModuleCompNameUC( "HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING" );

	// DERIVED TYPE DEFINITIONS

	// Type Description of Heat Pump

	// Output Variables Type definition

	// MODULE VARIABLE DECLARATIONS:
	Array1D_bool CheckEquipName;

	std::string GSHPRefrigerant( "R22" ); // refrigerent name and index
	int GSHPRefrigIndex( 0 );

	int NumGSHPs( 0 ); // number of Gshps specified in input
	Real64 LoadSideWaterMassFlowRate( 0.0 ); // Load Side mass flow rate, water side kg/s
	Real64 SourceSideWaterMassFlowRate( 0.0 ); // Source Side mass flow rate, water side kg/s
	Real64 Power( 0.0 ); // power consumption Watts
	Real64 QLoad( 0.0 ); // heat rejection from Load Side coil Watts
	Real64 QSource( 0.0 ); // cooling capacity Watts
	Real64 SourceSideWaterOutletTemp( 0.0 ); // Source Side outlet temperature °C
	Real64 SourceSideWaterInletTemp( 0.0 ); // Source Side outlet temperature °C
	Real64 LoadSideWaterOutletTemp( 0.0 ); // Source Side outlet temperature °C
	Real64 LoadSideWaterInletTemp( 0.0 ); // Source Side outlet temperature °C

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Name Public routines, optionally name Private routines within this module

	// Object Data
	Array1D< GshpSpecs > GSHP; // dimension to number of machines
	Array1D< ReportVars > GSHPReport;

	// MODULE SUBROUTINES:

	// Functions

	void
	SimHPWatertoWaterCOOLING(
		std::string const & GSHPType, // type ofGSHP
		std::string const & GSHPName, // user specified name ofGSHP
		int & CompIndex,
		bool const FirstHVACIteration,
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		Real64 & MaxCap, // W - maximum operating capacity of GSHP
		Real64 & MinCap, // W - minimum operating capacity of GSHP
		Real64 & OptCap, // W - optimal operating capacity of GSHP
		int const LoopNum
	)
	{
		//       SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   Feb 2000
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the  water to water Heat Pump driver.
		// It gets the input for the models, initializes simulation variables, calls
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using DataPlant::TypeOf_HPWaterEFCooling;
		using InputProcessor::FindItemInList;
		using namespace DataEnvironment;
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
		static bool GetInput( true ); // then TRUE, calls subroutine to read input file.
		int GSHPNum;

		//Get input from IDF

		if ( GetInput ) {
			GetGshpInput();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			GSHPNum = FindItemInList( GSHPName, GSHP );
			if ( GSHPNum == 0 ) {
				ShowFatalError( "SimHPWatertoWaterCOOLING: Unit not found=" + GSHPName );
			}
			CompIndex = GSHPNum;
		} else {
			GSHPNum = CompIndex;
			if ( GSHPNum > NumGSHPs || GSHPNum < 1 ) {
				ShowFatalError( "SimHPWatertoWaterCOOLING:  Invalid CompIndex passed=" + TrimSigDigits( GSHPNum ) + ", Number of Units=" + TrimSigDigits( NumGSHPs ) + ", Entered Unit name=" + GSHPName );
			}
			if ( CheckEquipName( GSHPNum ) ) {
				if ( GSHPName != GSHP( GSHPNum ).Name ) {
					ShowFatalError( "SimHPWatertoWaterCOOLING: Invalid CompIndex passed=" + TrimSigDigits( GSHPNum ) + ", Unit name=" + GSHPName + ", stored Unit Name for that index=" + GSHP( GSHPNum ).Name );
				}
				CheckEquipName( GSHPNum ) = false;
			}
		}

		// Calculate Demand on heat pump
		if ( InitLoopEquip ) {
			MinCap = GSHP( GSHPNum ).NomCap * GSHP( GSHPNum ).MinPartLoadRat;
			MaxCap = GSHP( GSHPNum ).NomCap * GSHP( GSHPNum ).MaxPartLoadRat;
			OptCap = GSHP( GSHPNum ).NomCap * GSHP( GSHPNum ).OptPartLoadRat;
			return;
		}

		// Simulate the model for the Demand "MyLoad"
		if ( LoopNum == GSHP( GSHPNum ).LoadLoopNum ) { // chilled water loop
			InitGshp( GSHPNum );
			CalcGshpModel( GSHPType, GSHPName, GSHPNum, MyLoad, FirstHVACIteration );
			UpdateGSHPRecords( GSHPNum );
		} else if ( LoopNum == GSHP( GSHPNum ).SourceLoopNum ) { // condenser loop
			UpdateChillerComponentCondenserSide( GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, TypeOf_HPWaterEFCooling, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHPReport( GSHPNum ).QSource, GSHPReport( GSHPNum ).SourceSideWaterInletTemp, GSHPReport( GSHPNum ).SourceSideWaterOutletTemp, GSHPReport( GSHPNum ).SourceSidemdot, FirstHVACIteration );
		} else {
			ShowFatalError( "SimHPWatertoWaterCOOLING:: Invalid loop connection " + ModuleCompName + ", Requested Unit=" + GSHPName );
		}

	}

	void
	GetGshpInput()
	{
		//       SUBROUTINE INFORMATION:
		//       AUTHOR:
		//       DATE WRITTEN:    April 1998

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the GSHP models.  As such
		// it will interact with the Input Scanner to retrieve
		// information from the input file, count the number of
		// GSHPs and begin to fill the
		// arrays associated with the typeGSHP.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataPlant::TypeOf_HPWaterPECooling;
		using DataPlant::ScanPlantLoopsForObject;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::FindRefrigerant;
		using PlantUtilities::RegisterPlantCompDesignFlow;

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
		int GSHPNum; // Gshp counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 5 ); // character string data
		Array1D< Real64 > NumArray( 23 ); // numeric data

		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;

		NumGSHPs = GetNumObjectsFound( ModuleCompNameUC );

		if ( NumGSHPs <= 0 ) {
			ShowSevereError( "No Equipment found in SimGshp" );
			ErrorsFound = true;
		}

		// Allocate Arrays
		GSHP.allocate( NumGSHPs );
		GSHPReport.allocate( NumGSHPs );
		CheckEquipName.dimension( NumGSHPs, true );

		for ( GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum ) {
			GetObjectItem( ModuleCompNameUC, GSHPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = true;
			VerifyName( AlphArray( 1 ), GSHP, GSHPNum - 1, IsNotOK, IsBlank, "GHSP Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			GSHP( GSHPNum ).Name = AlphArray( 1 );

			GSHP( GSHPNum ).WWHPPlantTypeOfNum = TypeOf_HPWaterPECooling;

			GSHP( GSHPNum ).COP = NumArray( 1 );
			if ( NumArray( 1 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":COP = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			// zero values for NumArray 3 - 6 checked in input - idd

			GSHP( GSHPNum ).NomCap = NumArray( 2 );

			GSHP( GSHPNum ).MinPartLoadRat = NumArray( 3 );

			GSHP( GSHPNum ).MaxPartLoadRat = NumArray( 4 );

			GSHP( GSHPNum ).OptPartLoadRat = NumArray( 5 );

			GSHP( GSHPNum ).LoadSideVolFlowRate = NumArray( 6 );
			if ( NumArray( 6 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Load Side Vol Flow Rate = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).SourceSideVolFlowRate = NumArray( 7 );
			if ( NumArray( 7 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Source Side Vol Flow Rate = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).LoadSideUACoeff = NumArray( 8 );
			if ( NumArray( 9 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Load Side Heat Transfer Coeffcient = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).SourceSideUACoeff = NumArray( 9 );
			if ( NumArray( 8 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Source Side Heat Transfer Coeffcient = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).CompPistonDisp = NumArray( 10 );
			if ( NumArray( 10 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Compressor Piston displacement/Storke = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).CompClearanceFactor = NumArray( 11 );
			if ( NumArray( 11 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Compressor Clearance Factor = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).CompSucPressDrop = NumArray( 12 );
			if ( NumArray( 12 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ": Pressure Drop = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).SuperheatTemp = NumArray( 13 );
			if ( NumArray( 13 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Source Side SuperHeat = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).PowerLosses = NumArray( 14 );
			if ( NumArray( 14 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Compressor Power Loss = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}
			GSHP( GSHPNum ).LossFactor = NumArray( 15 );
			if ( NumArray( 15 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Efficiency = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).HighPressCutoff = NumArray( 16 );
			if ( NumArray( 16 ) == 0.0 ) {
				GSHP( GSHPNum ).HighPressCutoff = 500000000.0;
				//CALL ShowWarningError(ModuleCompName//': High Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
			}

			GSHP( GSHPNum ).LowPressCutoff = NumArray( 17 );
			if ( NumArray( 17 ) == 0.0 ) {
				GSHP( GSHPNum ).LowPressCutoff = 0.0;
				//CALL ShowWarningError(ModuleCompName//': Low Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
			}

			GSHP( GSHPNum ).SourceSideInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, "HeatPump:WaterToWater Cooling", AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).SourceSideOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, "HeatPump:WaterToWater Cooling", AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, "HeatPump:WaterToWater Cooling", AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, "HeatPump:WaterToWater Cooling", AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			// Test node sets
			TestCompSet( ModuleCompNameUC, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Condenser Water Nodes" );
			TestCompSet( ModuleCompNameUC, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Chilled Water Nodes" );

			// save the design source side flow rate for use by plant loop sizing algorithms
			RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, 0.5 * GSHP( GSHPNum ).SourceSideVolFlowRate );

			GSHPReport( GSHPNum ).QLoad = 0.0;
			GSHPReport( GSHPNum ).QSource = 0.0;
			GSHPReport( GSHPNum ).Power = 0.0;
			GSHPReport( GSHPNum ).LoadSideWaterInletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideWaterInletTemp = 0.0;
			GSHPReport( GSHPNum ).LoadSideWaterOutletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideWaterOutletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSidemdot = 0.0;
			GSHPReport( GSHPNum ).LoadSidemdot = 0.0;
			GSHP( GSHPNum ).IsOn = false;
			GSHP( GSHPNum ).MustRun = true;

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors Found in getting Gshp input" );
		}

		GSHPRefrigIndex = FindRefrigerant( GSHPRefrigerant );
		if ( GSHPRefrigIndex == 0 ) {
			ShowFatalError( "Refrigerant for HeatPump:WaterToWater Heating not found, should have been=" + GSHPRefrigerant );
			ShowFatalError( "FluidProperties:* objects for " + GSHPRefrigerant + " must be included in the idf file.");
		}

		//CurrentModuleObject='HeatPump:WaterToWater:ParameterEstimation:Cooling'
		for ( GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum ) {
			SetupOutputVariable( "Water to Water Heat Pump Electric Power [W]", GSHPReport( GSHPNum ).Power, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Electric Energy [J]", GSHPReport( GSHPNum ).Energy, "System", "Sum", GSHP( GSHPNum ).Name, _, "Electricity", "Cooling", _, "Plant" );

			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Rate [W]", GSHPReport( GSHPNum ).QLoad, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QLoadEnergy, "System", "Sum", GSHP( GSHPNum ).Name );

			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Rate [W]", GSHPReport( GSHPNum ).QSource, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QSourceEnergy, "System", "Sum", GSHP( GSHPNum ).Name );

			SetupOutputVariable( "Water to Water Heat Pump Load Side Outlet Temperature [C]", GSHPReport( GSHPNum ).LoadSideWaterOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Inlet Temperature [C]", GSHPReport( GSHPNum ).LoadSideWaterInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Outlet Temperature [C]", GSHPReport( GSHPNum ).SourceSideWaterOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Inlet Temperature [C]", GSHPReport( GSHPNum ).SourceSideWaterInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Mass Flow Rate [kg/s]", GSHPReport( GSHPNum ).LoadSidemdot, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Mass Flow Rate [kg/s]", GSHPReport( GSHPNum ).SourceSidemdot, "System", "Average", GSHP( GSHPNum ).Name );

			//scan for loop connection data
			errFlag = false;
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, GSHP( GSHPNum ).WWHPPlantTypeOfNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum, _, _, _, GSHP( GSHPNum ).SourceSideInletNodeNum, _, errFlag );
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, GSHP( GSHPNum ).WWHPPlantTypeOfNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum, _, _, _, GSHP( GSHPNum ).LoadSideInletNodeNum, _, errFlag );

			if ( ! errFlag ) {
				PlantUtilities::InterConnectTwoPlantLoopSides( GSHP( GSHPNum ).LoadLoopNum,  GSHP( GSHPNum ).LoadLoopSideNum,  GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).WWHPPlantTypeOfNum, true );
			}

			if ( errFlag ) {
				ShowFatalError( "GetWatertoWaterHPInput: Program terminated on scan for loop data" );
			}

		}

	}

	void
	InitGshp( int const GSHPNum ) // GSHP number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    July 2007

		// PURPOSE OF THIS SUBROUTINE:
		// initialization

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using DataPlant::TypeOf_HPWaterPECooling;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitGshp" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlanScanFlag;
		static bool MyOneTimeFlag( true );
		Real64 rho; // local fluid density
		bool errFlag;

		if ( MyOneTimeFlag ) {
			MyPlanScanFlag.allocate( NumGSHPs );
			MyEnvrnFlag.allocate( NumGSHPs );
			MyOneTimeFlag = false;
			MyEnvrnFlag = true;
			MyPlanScanFlag = true;
		}

		if ( MyPlanScanFlag( GSHPNum ) ) {
			// Locate the heating on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, TypeOf_HPWaterPECooling, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum, _, _, _, GSHP( GSHPNum ).SourceSideInletNodeNum, _, errFlag );
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, TypeOf_HPWaterPECooling, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum, _, _, _, GSHP( GSHPNum ).LoadSideInletNodeNum, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitGshp: Program terminated due to previous condition(s)." );
			}
			MyPlanScanFlag( GSHPNum ) = false;
		}

		//For each new environment
		if ( BeginEnvrnFlag && MyEnvrnFlag( GSHPNum ) ) {
			GSHPReport( GSHPNum ).QLoad = 0.0;
			GSHPReport( GSHPNum ).QSource = 0.0;
			GSHPReport( GSHPNum ).Power = 0.0;
			GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
			GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
			GSHPReport( GSHPNum ).Energy = 0.0;
			GSHPReport( GSHPNum ).LoadSideWaterInletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideWaterInletTemp = 0.0;
			GSHPReport( GSHPNum ).LoadSideWaterOutletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideWaterOutletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSidemdot = 0.0;
			GSHPReport( GSHPNum ).LoadSidemdot = 0.0;
			GSHP( GSHPNum ).IsOn = false;
			GSHP( GSHPNum ).MustRun = true;

			MyEnvrnFlag( GSHPNum ) = false;
			rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
			GSHP( GSHPNum ).LoadSideDesignMassFlow = GSHP( GSHPNum ).LoadSideVolFlowRate * rho;

			InitComponentNodes( 0.0, GSHP( GSHPNum ).LoadSideDesignMassFlow, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );

			rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
			GSHP( GSHPNum ).SourceSideDesignMassFlow = GSHP( GSHPNum ).SourceSideVolFlowRate * rho;

			InitComponentNodes( 0.0, GSHP( GSHPNum ).SourceSideDesignMassFlow, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );

			Node( GSHP( GSHPNum ).SourceSideInletNodeNum ).Temp = 35.0;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( GSHPNum ) = true;

		// Init more variables

		//On every call
		GSHPReport( GSHPNum ).Running = 0;

		GSHP( GSHPNum ).MustRun = true; // Reset MustRun Flag to TRUE

		LoadSideWaterMassFlowRate = 0.0; // Load Side mass flow rate, water side
		SourceSideWaterMassFlowRate = 0.0; // Source Side mass flow rate, water side
		Power = 0.0; // power consumption
		QLoad = 0.0; // heat rejection from Load Side coil
		QSource = 0.0;

	}

	void
	CalcGshpModel(
		std::string const & EP_UNUSED( GSHPType ), // type ofGSHP
		std::string const & GSHPName, // user specified name ofGSHP
		int const GSHPNum, // GSHP Number
		Real64 & MyLoad, // Operating Load
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       April 1999
		//                      September 2002, SJR
		//       RE-ENGINEERED  Mar2000

		// PURPOSE OF THIS SUBROUTINE: This routine performs

		// METHODOLOGY EMPLOYED: under development

		// REFERENCES:

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using namespace FluidProperties;
		using General::TrimSigDigits;
		using DataPlant::PlantLoop;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const gamma( 1.114 ); // Expnasion Coefficient
		Real64 const HeatBalTol( 0.0005 );
		Real64 const RelaxParam( 0.6 );
		Real64 const SmallNum( 1.0e-20 );
		int const IterationLimit( 500 );
		static std::string const RoutineName( "CalcGshpModel" );
		static std::string const RoutineNameLoadSideRefridgTemp( "CalcGSHPModel:LoadSideRefridgTemp" );
		static std::string const RoutineNameSourceSideRefridgTemp( "CalcGSHPModel:SourceSideRefridgTemp" );
		static std::string const RoutineNameCompressInletTemp( "CalcGSHPModel:CompressInletTemp" );
		static std::string const RoutineNameSuctionPr( "CalcGSHPModel:SuctionPr" );
		static std::string const RoutineNameCompSuctionTemp( "CalcGSHPModel:CompSuctionTemp" );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SourceSideEffect; // Source Side effectiveness
		Real64 LoadSideEffect; // Load Side effectiveness
		Real64 SourceSideRefridgTemp; // Source Side temperature
		Real64 LoadSideRefridgTemp; // Load Side temperature
		Real64 SourceSideUA; // Source Side heat transfer coefficient
		Real64 LoadSideUA; // Load Side heat transfer coefficient
		Real64 SourceSidePressure; // Source Side pressure
		Real64 LoadSidePressure; // Load Side pressure
		Real64 SuctionPr; // Suction Pressure
		Real64 DischargePr; // Discharge Pressure
		Real64 CompressInletTemp; // Compressor inlet temperature
		Real64 PressureDrop; // Suction Pressure drop
		Real64 ClearanceFactor; // Clearance factor
		Real64 PistonDisp; // Compressor piston displacement
		Real64 ShTemp; // Superheat temperature
		Real64 LosFac; // Loss factor used to define the electromechanical loss for compressor
		Real64 MassRef; // mass flow rate of refrigerant
		Real64 SourceSideOutletEnth; // Enthalpy at Source Side pressure
		Real64 LoadSideOutletEnth; // Enthalpy at Condensor Pressure
		Real64 initialQSource; // Guess Source Side Heat rate
		Real64 initialQLoad; // Guess Load Side Heat rate
		Real64 qual; // quality
		Real64 COP;
		Real64 SuperHeatEnth;
		Real64 T110;
		Real64 T111;
		Real64 CompSuctionTemp;
		Real64 CompSuctionEnth;
		Real64 CompSuctionDensity;
		Real64 PowerLosses;
		Real64 CompSuctionSatTemp;
		Real64 HighPressCutoff;
		Real64 LowPressCutoff;
		std::string ErrString;
		Real64 DutyFactor;
		int IterationCount;

		static Real64 CurrentSimTime( 0.0 );
		static Real64 PrevSimTime( 0.0 );
		static bool OneTimeFlag( true );
		// Nodes
		int SourceSideInletNode; // Source Side inlet node number, water side
		int SourceSideOutletNode; // Source Side outlet node number, water side
		int LoadSideInletNode; // Load Side inlet node number, water side
		int LoadSideOutletNode; // Load Side outlet node number, water side
		int LoopNum;
		int LoopSideNum;
		Real64 CpSourceSide; // local temporary for fluid specific heat
		Real64 CpLoadSide; // local temporary for fluid specific heat

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		PressureDrop = GSHP( GSHPNum ).CompSucPressDrop;
		ClearanceFactor = GSHP( GSHPNum ).CompClearanceFactor;
		PistonDisp = GSHP( GSHPNum ).CompPistonDisp;
		ShTemp = GSHP( GSHPNum ).SuperheatTemp;
		LosFac = GSHP( GSHPNum ).LossFactor;
		SourceSideUA = GSHP( GSHPNum ).SourceSideUACoeff;
		LoadSideUA = GSHP( GSHPNum ).LoadSideUACoeff;
		PowerLosses = GSHP( GSHPNum ).PowerLosses;
		COP = GSHP( GSHPNum ).COP;
		HighPressCutoff = GSHP( GSHPNum ).HighPressCutoff;
		LowPressCutoff = GSHP( GSHPNum ).LowPressCutoff;
		LoadSideInletNode = GSHP( GSHPNum ).LoadSideInletNodeNum;
		LoadSideOutletNode = GSHP( GSHPNum ).LoadSideOutletNodeNum;
		SourceSideInletNode = GSHP( GSHPNum ).SourceSideInletNodeNum;
		SourceSideOutletNode = GSHP( GSHPNum ).SourceSideOutletNodeNum;
		LoopNum = GSHP( GSHPNum ).LoadLoopNum;
		LoopSideNum = GSHP( GSHPNum ).LoadLoopSideNum;

		if ( PrevSimTime != CurrentSimTime ) {
			PrevSimTime = CurrentSimTime;
		}

		// CALCULATE THE SIMULATION TIME
		CurrentSimTime = ( DayOfSim - 1 ) * 24 + HourOfDay - 1 + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed;

		// initialize event time array when the environment simulation begins
		if ( CurrentSimTime == 0.0 && OneTimeFlag ) {
			OneTimeFlag = false;
		}

		if ( CurrentSimTime > 0.0 ) OneTimeFlag = true;

		if ( MyLoad < 0.0 ) {
			GSHP( GSHPNum ).MustRun = true;
			GSHP( GSHPNum ).IsOn = true;
		} else {
			GSHP( GSHPNum ).MustRun = false;
			GSHP( GSHPNum ).IsOn = false;
		}

		//*******Set flow based on "flowlock" and "run" flags**********
		// Set flows if the heat pump is not running
		if ( ! GSHP( GSHPNum ).MustRun ) {
			LoadSideWaterMassFlowRate = 0.0;
			SetComponentFlowRate( LoadSideWaterMassFlowRate, LoadSideInletNode, LoadSideOutletNode, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
			SourceSideWaterMassFlowRate = 0.0;
			SetComponentFlowRate( SourceSideWaterMassFlowRate, SourceSideInletNode, SourceSideOutletNode, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );
			PlantUtilities::PullCompInterconnectTrigger( GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum , GSHP( GSHPNum ).CondMassFlowIndex, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, SourceSideWaterMassFlowRate );
			//now initialize simulation variables for "heat pump off"
			QLoad = 0.0;
			QSource = 0.0;
			Power = 0.0;
			LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			LoadSideWaterOutletTemp = LoadSideWaterInletTemp;
			SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			SourceSideWaterOutletTemp = SourceSideWaterInletTemp;
			return; //if heat pump is not running return without simulation

			// Set flows if the heat pump is running
		} else { // the heat pump must run
			LoadSideWaterMassFlowRate = GSHP( GSHPNum ).LoadSideDesignMassFlow;
			SetComponentFlowRate( LoadSideWaterMassFlowRate, LoadSideInletNode, LoadSideOutletNode, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );

			SourceSideWaterMassFlowRate = GSHP( GSHPNum ).SourceSideDesignMassFlow;
			SetComponentFlowRate( SourceSideWaterMassFlowRate, SourceSideInletNode, SourceSideOutletNode, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );
			// get inlet temps
			LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			//if there's no flow, turn the "heat pump off"
			if ( LoadSideWaterMassFlowRate < MassFlowTolerance || SourceSideWaterMassFlowRate < MassFlowTolerance ) {
				LoadSideWaterMassFlowRate = 0.0;
				SetComponentFlowRate( LoadSideWaterMassFlowRate, LoadSideInletNode, LoadSideOutletNode, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
				SourceSideWaterMassFlowRate = 0.0;
				SetComponentFlowRate( SourceSideWaterMassFlowRate, SourceSideInletNode, SourceSideOutletNode, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );
				PlantUtilities::PullCompInterconnectTrigger( GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum , GSHP( GSHPNum ).CondMassFlowIndex, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, SourceSideWaterMassFlowRate );
				QLoad = 0.0;
				QSource = 0.0;
				Power = 0.0;
				LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
				LoadSideWaterOutletTemp = LoadSideWaterInletTemp;
				SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
				SourceSideWaterOutletTemp = SourceSideWaterInletTemp;
				return;
			}
			PlantUtilities::PullCompInterconnectTrigger( GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum , GSHP( GSHPNum ).CondMassFlowIndex, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, SourceSideWaterMassFlowRate );
		}

		//**********BEGIN THE CALCULATION**************

		// initialize the source and load side heat transfer rates for the simulation
		initialQSource = 0.0;
		initialQLoad = 0.0;
		IterationCount = 0;

		CpSourceSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideWaterInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		CpLoadSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideWaterInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		//Determine effectiveness of Load Side
		LoadSideEffect = 1.0 - std::exp( -LoadSideUA / ( CpLoadSide * LoadSideWaterMassFlowRate ) );
		// Determine effectiveness of Source Side
		SourceSideEffect = 1.0 - std::exp( -SourceSideUA / ( CpSourceSide * SourceSideWaterMassFlowRate ) );

		// main iteration loop to solve model equations
		while ( true ) {
			++IterationCount;

			// To determine Load Side temperature
			LoadSideRefridgTemp = LoadSideWaterInletTemp - initialQLoad / ( LoadSideEffect * CpLoadSide * LoadSideWaterMassFlowRate );

			// Determine Source Side tempertaure
			SourceSideRefridgTemp = SourceSideWaterInletTemp + initialQSource / ( SourceSideEffect * CpSourceSide * SourceSideWaterMassFlowRate );

			// Determine the evaporating and condensing pressures
			SourceSidePressure = GetSatPressureRefrig( GSHPRefrigerant, SourceSideRefridgTemp, GSHPRefrigIndex, RoutineName );
			LoadSidePressure = GetSatPressureRefrig( GSHPRefrigerant, LoadSideRefridgTemp, GSHPRefrigIndex, RoutineName );

			if ( SourceSidePressure < LowPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + GSHPName + "\" Cooling Source Side Pressure Less than the Design Minimum" );
				ShowContinueError( "Cooling Source Side Pressure=" + TrimSigDigits( SourceSidePressure, 2 ) + " and user specified Design Minimum Pressure=" + TrimSigDigits( LowPressCutoff, 2 ) );
				ShowContinueErrorTimeStamp( "" );
				ShowFatalError( "Preceding Conditions cause termination." );
			}

			if ( LoadSidePressure > HighPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + GSHPName + "\" Cooling Load Side Pressure greater than the Design Maximum" );
				ShowContinueError( "Cooling Load Side Pressure=" + TrimSigDigits( LoadSidePressure, 2 ) + " and user specified Design Maximum Pressure=" + TrimSigDigits( HighPressCutoff, 2 ) );
				ShowContinueErrorTimeStamp( "" );
				ShowFatalError( "Preceding Conditions cause termination." );
			}
			// Determine Suction Pressure at compressor inlet
			SuctionPr = LoadSidePressure - PressureDrop;
			// Determine Discharge Pressure at compressor exit
			DischargePr = SourceSidePressure + PressureDrop;

			if ( SuctionPr < LowPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + GSHPName + "\" Cooling Suction Pressure Less than the Design Minimum" );
				ShowContinueError( "Cooling Suction Pressure=" + TrimSigDigits( SuctionPr, 2 ) + " and user specified Design Minimum Pressure=" + TrimSigDigits( LowPressCutoff, 2 ) );
				ShowContinueErrorTimeStamp( "" );
				ShowFatalError( "Preceding Conditions cause termination." );
			}

			if ( DischargePr > HighPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + GSHPName + "\" Cooling Discharge Pressure greater than the Design Maximum" );
				ShowContinueError( "Cooling Discharge Pressure=" + TrimSigDigits( DischargePr, 2 ) + " and user specified Design Maximum Pressure=" + TrimSigDigits( HighPressCutoff, 2 ) );
				ShowContinueErrorTimeStamp( "" );
				ShowFatalError( "Preceding Conditions cause termination." );
			}

			// Determine the Source Side Outlet Enthalpy

			qual = 1.0;
			LoadSideOutletEnth = GetSatEnthalpyRefrig( GSHPRefrigerant, LoadSideRefridgTemp, qual, GSHPRefrigIndex, RoutineNameLoadSideRefridgTemp );

			qual = 0.0;
			SourceSideOutletEnth = GetSatEnthalpyRefrig( GSHPRefrigerant, SourceSideRefridgTemp, qual, GSHPRefrigIndex, RoutineNameSourceSideRefridgTemp );

			// Determine Load Side Outlet Enthalpy
			// Determine superheated temperature of the LoadSide outlet/compressor inlet
			CompressInletTemp = LoadSideRefridgTemp + ShTemp;

			// Determine the enathalpy of the super heated fluid at Source Side outlet
			SuperHeatEnth = GetSupHeatEnthalpyRefrig( GSHPRefrigerant, CompressInletTemp, LoadSidePressure, GSHPRefrigIndex, RoutineNameCompressInletTemp );

			// Determining the suction state of the fluid from inlet state involves interation
			// Method employed...
			// Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
			// check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

			//this routine was reenginerred from HVACsim + takes pressure in Pascals, tolrance, refrgerant # R22 =6
			CompSuctionSatTemp = GetSatTemperatureRefrig( GSHPRefrigerant, SuctionPr, GSHPRefrigIndex, RoutineNameSuctionPr );

			T110 = CompSuctionSatTemp;
			//            Shoot into the super heated region
			T111 = CompSuctionSatTemp + 100.0;
			// Iterate to find the Suction State
			while ( true ) {
				CompSuctionTemp = 0.5 * ( T110 + T111 );

				CompSuctionEnth = GetSupHeatEnthalpyRefrig( GSHPRefrigerant, CompSuctionTemp, SuctionPr, GSHPRefrigIndex, RoutineNameCompSuctionTemp );

				if ( std::abs( CompSuctionEnth - SuperHeatEnth ) / SuperHeatEnth < 0.0001 ) {
					goto LOOP_exit;
				}

				if ( CompSuctionEnth < SuperHeatEnth ) {
					T110 = CompSuctionTemp;
				} else {
					T111 = CompSuctionTemp;
				}
			}
			LOOP_exit: ;

			// Determine the Mass flow rate of refrigerant
			CompSuctionDensity = GetSupHeatDensityRefrig( GSHPRefrigerant, CompSuctionTemp, SuctionPr, GSHPRefrigIndex, RoutineNameCompSuctionTemp );
			MassRef = PistonDisp * CompSuctionDensity * ( 1 + ClearanceFactor - ClearanceFactor * std::pow( DischargePr / SuctionPr, 1 / gamma ) );

			// Find the Source Side Heat Transfer

			QLoad = MassRef * ( LoadSideOutletEnth - SourceSideOutletEnth );

			// Determine the theoretical power
			Power = PowerLosses + ( MassRef * gamma / ( gamma - 1 ) * SuctionPr / CompSuctionDensity / LosFac * ( std::pow( DischargePr / SuctionPr, ( gamma - 1 ) / gamma ) - 1 ) );

			// Determine the Loadside HeatRate (QLoad)
			QSource = Power + QLoad;
			// convergence and iteration limit check
			if ( std::abs( ( QSource - initialQSource ) / ( initialQSource + SmallNum ) ) < HeatBalTol || IterationCount > IterationLimit ) {
				if ( IterationCount > IterationLimit ) {
					ShowWarningError( "HeatPump:WaterToWater:ParameterEstimation, Cooling did not converge" );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Heatpump Name = " + GSHP( GSHPNum ).Name );
					gio::write( ErrString, fmtLD ) << std::abs( 100.0 * ( QSource - initialQSource ) / ( initialQSource + SmallNum ) );
					ShowContinueError( "Heat Inbalance (%)             = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << QLoad;
					ShowContinueError( "Load-side heat transfer rate   = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << QSource;
					ShowContinueError( "Source-side heat transfer rate = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << SourceSideWaterMassFlowRate;
					ShowContinueError( "Source-side mass flow rate     = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << LoadSideWaterMassFlowRate;
					ShowContinueError( "Load-side mass flow rate       = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << SourceSideWaterInletTemp;
					ShowContinueError( "Source-side inlet temperature  = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << LoadSideWaterInletTemp;
					ShowContinueError( "Load-side inlet temperature    = " + stripped( ErrString ) );

				}
				goto LOOPSourceEnth_exit;

			} else { // update load
				initialQSource += RelaxParam * ( QSource - initialQSource );
				initialQLoad += RelaxParam * ( QLoad - initialQLoad );

			}

		}
		LOOPSourceEnth_exit: ;

		//Control Strategy
		if ( std::abs( MyLoad ) < QLoad ) {
			DutyFactor = std::abs( MyLoad ) / QLoad;
			QLoad = std::abs( MyLoad );
			Power *= DutyFactor;
			QSource *= DutyFactor;
			// Determine the Exterior fluid temperature at the Load Side oulet and evaporator outlet...
			LoadSideWaterOutletTemp = LoadSideWaterInletTemp - QLoad / ( LoadSideWaterMassFlowRate * CpLoadSide ); // Chilled water
			SourceSideWaterOutletTemp = SourceSideWaterInletTemp + QSource / ( SourceSideWaterMassFlowRate * CpSourceSide ); // cooling water
			return;
		}

		LoadSideWaterOutletTemp = LoadSideWaterInletTemp - QLoad / ( LoadSideWaterMassFlowRate * CpLoadSide ); // Chilled water
		SourceSideWaterOutletTemp = SourceSideWaterInletTemp + QSource / ( SourceSideWaterMassFlowRate * CpSourceSide );
		GSHPReport( GSHPNum ).Running = 1;

	}

	void
	UpdateGSHPRecords( int const GSHPNum ) // GSHP number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SourceSideInletNode; // Source Side inlet node number, water side
		int SourceSideOutletNode; // Source Side outlet node number, water side
		int LoadSideInletNode; // Load Side inlet node number, water side
		int LoadSideOutletNode; // Load Side outlet node number, water side
		Real64 ReportingConstant;

		LoadSideInletNode = GSHP( GSHPNum ).LoadSideInletNodeNum;
		LoadSideOutletNode = GSHP( GSHPNum ).LoadSideOutletNodeNum;
		SourceSideInletNode = GSHP( GSHPNum ).SourceSideInletNodeNum;
		SourceSideOutletNode = GSHP( GSHPNum ).SourceSideOutletNodeNum;

		if ( ! GSHP( GSHPNum ).MustRun ) {
			//set node temperatures
			Node( SourceSideOutletNode ).Temp = Node( SourceSideInletNode ).Temp;
			Node( LoadSideOutletNode ).Temp = Node( LoadSideInletNode ).Temp;
			GSHPReport( GSHPNum ).Power = 0.0;
			GSHPReport( GSHPNum ).Energy = 0.0;
			GSHPReport( GSHPNum ).QSource = 0.0;
			GSHPReport( GSHPNum ).QLoad = 0.0;
			GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
			GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
			GSHPReport( GSHPNum ).SourceSidemdot = SourceSideWaterMassFlowRate;
			GSHPReport( GSHPNum ).LoadSidemdot = LoadSideWaterMassFlowRate;
			GSHPReport( GSHPNum ).SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			GSHPReport( GSHPNum ).SourceSideWaterOutletTemp = Node( SourceSideOutletNode ).Temp;
			GSHPReport( GSHPNum ).LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			GSHPReport( GSHPNum ).LoadSideWaterOutletTemp = Node( LoadSideOutletNode ).Temp;

		} else {
			//set node temperatures
			Node( LoadSideOutletNode ).Temp = LoadSideWaterOutletTemp;
			Node( SourceSideOutletNode ).Temp = SourceSideWaterOutletTemp;

			//set node flow rates;  for these load based models
			//assume that the sufficient Source Side flow rate available

			ReportingConstant = TimeStepSys * SecInHour;

			GSHPReport( GSHPNum ).Power = Power;
			GSHPReport( GSHPNum ).QSource = QSource;
			GSHPReport( GSHPNum ).QLoad = QLoad;
			GSHPReport( GSHPNum ).Energy = Power * ReportingConstant;
			GSHPReport( GSHPNum ).QSourceEnergy = QSource * ReportingConstant;
			GSHPReport( GSHPNum ).QLoadEnergy = QLoad * ReportingConstant;
			GSHPReport( GSHPNum ).SourceSidemdot = SourceSideWaterMassFlowRate;
			GSHPReport( GSHPNum ).LoadSidemdot = LoadSideWaterMassFlowRate;
			GSHPReport( GSHPNum ).SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			GSHPReport( GSHPNum ).SourceSideWaterOutletTemp = Node( SourceSideOutletNode ).Temp;
			GSHPReport( GSHPNum ).LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			GSHPReport( GSHPNum ).LoadSideWaterOutletTemp = Node( LoadSideOutletNode ).Temp;

		}
		//    Node(SourceSideInletNode)%MassFlowRate = SourceSideWaterMassFlowRate
		//    Node(SourceSideOutletNode)%MassFlowRate = SourceSideWaterMassFlowRate
		//    Node(LoadSideInletNode)%MassFlowRate = LoadSideWaterMassFlowRate
		//    Node(LoadSideOutletNode)%MassFlowRate = LoadSideWaterMassFlowRate
		//    Node(SourceSideOutletNode)%MassFlowRateMaxAvail = Node(SourceSideInletNode)%MassFlowRateMaxAvail
		//    Node(SourceSideOutletNode)%MassFlowRateMinAvail = Node(SourceSideInletNode)%MassFlowRateMinAvail
		//    Node(LoadSideOutletNode)%MassFlowRateMaxAvail = Node(LoadSideInletNode)%MassFlowRateMaxAvail
		//    Node(LoadSideOutletNode)%MassFlowRateMinAvail = Node(LoadSideInletNode)%MassFlowRateMinAvail
	}

} // HeatPumpWaterToWaterCOOLING

} // EnergyPlus
