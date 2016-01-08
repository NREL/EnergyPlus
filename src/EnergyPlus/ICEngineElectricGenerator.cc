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
#include <ICEngineElectricGenerator.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ICEngineElectricGenerator {

	//__________________________________________________________________________
	// BLAST inherited generators:
	// ICENGINEElectricGenerator (Internal Combustion, curve fit from BLAST)

	// MODULE INFORMATION:
	//       AUTHOR         Dan Fisher
	//       DATE WRITTEN   Sept. 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates the operation of IC ENGINE Generators.

	// METHODOLOGY EMPLOYED:
	// Once the ElectricPowerManager determines that the IC ENGINE Generator
	// is available to meet an electric load demand, it calls SimICEngineGenerator
	// which in turn calls the ICEngine Generator model.

	// REFERENCES:
	// N/A

	// OTHER NOTES:
	// N/A

	// Using/Aliasing
	using namespace DataLoopNode;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::SecInHour;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobalConstants::iGeneratorICEngine;
	using General::RoundSigDigits;

	// Data
	//MODULE PARAMETER DEFINITIONS
	Real64 const ReferenceTemp( 25.0 ); // Reference temperature by which lower heating
	// value is reported.  This should be subtracted
	// off of when calculated exhaust energies.

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumICEngineGenerators( 0 ); // number of IC ENGINE Generators specified in input
	bool GetICEInput( true ); // When TRUE, calls subroutine to read input file.
	Array1D_bool CheckEquipName;
	// SUBROUTINE SPECIFICATIONS FOR MODULE IC ENGINEElectricGenerator

	// Object Data
	Array1D< ICEngineGeneratorSpecs > ICEngineGenerator; // dimension to number of machines
	Array1D< ReportVars > ICEngineGeneratorReport;

	// MODULE SUBROUTINES:

	// Beginning of IC ENGINE Generator Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimICEngineGenerator(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // simulate Generator when TRUE
		Real64 const MyLoad, // demand on electric generator
		bool const FirstHVACIteration
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the IC ENGINE Generator model driver.  It
		// gets the input for the models, initializes simulation variables, call
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

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
		int GenNum; // Generator number counter

		//Get Generator data from input file
		if ( GetICEInput ) {
			GetICEngineGeneratorInput();
			GetICEInput = false;
		}

		//SELECT and CALL MODELS
		if ( GeneratorIndex == 0 ) {
			GenNum = FindItemInList( GeneratorName, ICEngineGenerator );
			if ( GenNum == 0 ) ShowFatalError( "SimICEngineGenerator: Specified Generator not one of Valid ICEngine Generators " + GeneratorName );
			GeneratorIndex = GenNum;
		} else {
			GenNum = GeneratorIndex;
			if ( GenNum > NumICEngineGenerators || GenNum < 1 ) {
				ShowFatalError( "SimICEngineGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Number of IC Engine Generators=" + TrimSigDigits( NumICEngineGenerators ) + ", Generator name=" + GeneratorName );
			}
			if ( CheckEquipName( GenNum ) ) {
				if ( GeneratorName != ICEngineGenerator( GenNum ).Name ) {
					ShowFatalError( "SimICEngineGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + ICEngineGenerator( GenNum ).Name );
				}
				CheckEquipName( GenNum ) = false;
			}
		}

		InitICEngineGenerators( GenNum, RunFlag, MyLoad, FirstHVACIteration );
		CalcICEngineGeneratorModel( GenNum, RunFlag, MyLoad, FirstHVACIteration );
		UpdateICEngineGeneratorRecords( RunFlag, GenNum );

	}

	void
	GetICEGeneratorResults(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
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
		GeneratorPower = ICEngineGeneratorReport( GeneratorIndex ).PowerGen;
		GeneratorEnergy = ICEngineGeneratorReport( GeneratorIndex ).EnergyGen;
		ThermalPower = ICEngineGeneratorReport( GeneratorIndex ).QTotalHeatRecovered;
		ThermalEnergy = ICEngineGeneratorReport( GeneratorIndex ).TotalHeatEnergyRec;

	}

	void
	SimICEPlantHeatRecovery(
		std::string const & EP_UNUSED( CompType ),
		std::string const & CompName,
		int const EP_UNUSED( CompTypeNum ),
		int & CompNum,
		bool const EP_UNUSED( RunFlag ),
		bool & InitLoopEquip,
		Real64 & EP_UNUSED( MyLoad ),
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         BGriffith
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fill data needed in PlantLoopEquipments

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateComponentHeatRecoverySide;
		using DataPlant::TypeOf_Generator_ICEngine;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN)          :: FlowLock !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetICEInput ) {
			GetICEngineGeneratorInput();
			GetICEInput = false;
		}

		if ( InitLoopEquip ) {
			CompNum = FindItemInList( CompName, ICEngineGenerator );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimICEPlantHeatRecovery: ICE Generator Unit not found=" + CompName );
				return;
			}
			MinCap = 0.0;
			MaxCap = 0.0;
			OptCap = 0.0;
			return;
		} // End Of InitLoopEquip

		UpdateComponentHeatRecoverySide( ICEngineGenerator( CompNum ).HRLoopNum, ICEngineGenerator( CompNum ).HRLoopSideNum, TypeOf_Generator_ICEngine, ICEngineGenerator( CompNum ).HeatRecInletNodeNum, ICEngineGenerator( CompNum ).HeatRecOutletNodeNum, ICEngineGeneratorReport( CompNum ).QTotalHeatRecovered, ICEngineGeneratorReport( CompNum ).HeatRecInletTemp, ICEngineGeneratorReport( CompNum ).HeatRecOutletTemp, ICEngineGeneratorReport( CompNum ).HeatRecMdot, FirstHVACIteration );

	}

	// End IC ENGINE Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of IC ENGINE Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetICEngineGeneratorInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    Sept. 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the IC ENGINE Generator models.

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using CurveManager::GetCurveIndex;
		using CurveManager::CurveValue;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// PARAMETERS

		//LOCAL VARIABLES
		int GeneratorNum; // Generator counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 10 ); // character string data
		Array1D< Real64 > NumArray( 11 ); // numeric data
		static bool ErrorsFound( false ); // error flag
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		Real64 xValue; // test curve limits

		//FLOW
		cCurrentModuleObject = "Generator:InternalCombustionEngine";
		NumICEngineGenerators = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumICEngineGenerators <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}

		//ALLOCATE ARRAYS
		ICEngineGenerator.allocate( NumICEngineGenerators );
		CheckEquipName.dimension( NumICEngineGenerators, true );

		ICEngineGeneratorReport.allocate( NumICEngineGenerators );

		//LOAD ARRAYS WITH IC ENGINE Generator CURVE FIT  DATA
		for ( GeneratorNum = 1; GeneratorNum <= NumICEngineGenerators; ++GeneratorNum ) {
			GetObjectItem( cCurrentModuleObject, GeneratorNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), ICEngineGenerator, GeneratorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			ICEngineGenerator( GeneratorNum ).Name = AlphArray( 1 );

			ICEngineGenerator( GeneratorNum ).RatedPowerOutput = NumArray( 1 );
			if ( NumArray( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( NumArray( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			// Not sure what to do with electric nodes, so do not use optional arguments
			ICEngineGenerator( GeneratorNum ).ElectricCircuitNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Electric, NodeConnectionType_Electric, 1, ObjectIsNotParent );

			ICEngineGenerator( GeneratorNum ).MinPartLoadRat = NumArray( 2 );
			ICEngineGenerator( GeneratorNum ).MaxPartLoadRat = NumArray( 3 );
			ICEngineGenerator( GeneratorNum ).OptPartLoadRat = NumArray( 4 );

			//Load Special IC ENGINE Generator Curve Fit Inputs
			ICEngineGenerator( GeneratorNum ).ElecOutputFuelCurve = GetCurveIndex( AlphArray( 3 ) ); // convert curve name to number
			if ( ICEngineGenerator( GeneratorNum ).ElecOutputFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + AlphArray( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			ICEngineGenerator( GeneratorNum ).RecJacHeattoFuelCurve = GetCurveIndex( AlphArray( 4 ) ); // convert curve name to number
			if ( ICEngineGenerator( GeneratorNum ).RecJacHeattoFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + AlphArray( 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			ICEngineGenerator( GeneratorNum ).RecLubeHeattoFuelCurve = GetCurveIndex( AlphArray( 5 ) ); // convert curve name to number
			if ( ICEngineGenerator( GeneratorNum ).RecLubeHeattoFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + AlphArray( 5 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			ICEngineGenerator( GeneratorNum ).TotExhausttoFuelCurve = GetCurveIndex( AlphArray( 6 ) ); // convert curve name to number
			if ( ICEngineGenerator( GeneratorNum ).TotExhausttoFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + AlphArray( 6 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			ICEngineGenerator( GeneratorNum ).ExhaustTempCurve = GetCurveIndex( AlphArray( 7 ) ); // convert curve name to number
			if ( ICEngineGenerator( GeneratorNum ).ExhaustTempCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + AlphArray( 7 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			} else {
				xValue = CurveValue( ICEngineGenerator( GeneratorNum ).ExhaustTempCurve, 1.0 );
				if ( xValue < ReferenceTemp ) {
					ShowSevereError( "GetICEngineGeneratorInput: " + cAlphaFieldNames( 7 ) + " output has very low value." );
					ShowContinueError( "...curve generates [" + RoundSigDigits( xValue, 3 ) + " C] at PLR=1.0" );
					ShowContinueError( "...this is less than the Reference Temperature [" + RoundSigDigits( ReferenceTemp, 2 ) + " C] and may cause errors." );
				}
			}

			ICEngineGenerator( GeneratorNum ).UACoef( 1 ) = NumArray( 5 );
			ICEngineGenerator( GeneratorNum ).UACoef( 2 ) = NumArray( 6 );

			ICEngineGenerator( GeneratorNum ).MaxExhaustperPowerOutput = NumArray( 7 );
			ICEngineGenerator( GeneratorNum ).DesignMinExitGasTemp = NumArray( 8 );
			ICEngineGenerator( GeneratorNum ).FuelHeatingValue = NumArray( 9 );
			ICEngineGenerator( GeneratorNum ).DesignHeatRecVolFlowRate = NumArray( 10 );
			if ( ICEngineGenerator( GeneratorNum ).DesignHeatRecVolFlowRate > 0.0 ) {
				ICEngineGenerator( GeneratorNum ).HeatRecActive = true;
				ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum = GetOnlySingleNode( AlphArray( 8 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				if ( ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + AlphArray( 8 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
				ICEngineGenerator( GeneratorNum ).HeatRecOutletNodeNum = GetOnlySingleNode( AlphArray( 9 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				if ( ICEngineGenerator( GeneratorNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + AlphArray( 9 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
				TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 8 ), AlphArray( 9 ), "Heat Recovery Nodes" );
				RegisterPlantCompDesignFlow( ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum, ICEngineGenerator( GeneratorNum ).DesignHeatRecVolFlowRate );
			} else {
				ICEngineGenerator( GeneratorNum ).HeatRecActive = false;
				ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum = 0;
				ICEngineGenerator( GeneratorNum ).HeatRecOutletNodeNum = 0;
				if ( ! lAlphaFieldBlanks( 8 ) || ! lAlphaFieldBlanks( 9 ) ) {
					ShowWarningError( "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "However, Node names were specified for Heat Recovery inlet or outlet nodes" );
				}
			}

			//Fuel Type Case Statement
			{ auto const SELECT_CASE_var( AlphArray( 10 ) );
			if ( is_blank( SELECT_CASE_var ) ) { //If blank then the default is Diesel
				ICEngineGenerator( GeneratorNum ).FuelType = "Diesel";

			} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "Gas";

			} else if ( SELECT_CASE_var == "DIESEL" ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "Diesel";

			} else if ( SELECT_CASE_var == "GASOLINE" ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "Gasoline";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "FuelOil#1";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "FuelOil#2";

			} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "Propane";

			} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "OtherFuel1";

			} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
				ICEngineGenerator( GeneratorNum ).FuelType = "OtherFuel2";

			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + AlphArray( 10 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}}

			ICEngineGenerator( GeneratorNum ).HeatRecMaxTemp = NumArray( 11 );

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( GeneratorNum = 1; GeneratorNum <= NumICEngineGenerators; ++GeneratorNum ) {
			SetupOutputVariable( "Generator Produced Electric Power [W]", ICEngineGeneratorReport( GeneratorNum ).PowerGen, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
			SetupOutputVariable( "Generator Produced Electric Energy [J]", ICEngineGeneratorReport( GeneratorNum ).EnergyGen, "System", "Sum", ICEngineGenerator( GeneratorNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Generator " + ICEngineGenerator( GeneratorNum ).FuelType + " Rate [W]", ICEngineGeneratorReport( GeneratorNum ).FuelEnergyUseRate, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
			SetupOutputVariable( "Generator " + ICEngineGenerator( GeneratorNum ).FuelType + " Energy [J]", ICEngineGeneratorReport( GeneratorNum ).FuelEnergy, "System", "Sum", ICEngineGenerator( GeneratorNum ).Name, _, ICEngineGenerator( GeneratorNum ).FuelType, "COGENERATION", _, "Plant" );

			//    general fuel use report to match other generators.
			SetupOutputVariable( "Generator Fuel HHV Basis Rate [W]", ICEngineGeneratorReport( GeneratorNum ).FuelEnergyUseRate, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
			SetupOutputVariable( "Generator Fuel HHV Basis Energy [J]", ICEngineGeneratorReport( GeneratorNum ).FuelEnergy, "System", "Sum", ICEngineGenerator( GeneratorNum ).Name );

			SetupOutputVariable( "Generator " + ICEngineGenerator( GeneratorNum ).FuelType + " Mass Flow Rate [kg/s]", ICEngineGeneratorReport( GeneratorNum ).FuelMdot, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );

			SetupOutputVariable( "Generator Exhaust Air Temperature [C]", ICEngineGeneratorReport( GeneratorNum ).ExhaustStackTemp, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );

			if ( ICEngineGenerator( GeneratorNum ).HeatRecActive ) {
				SetupOutputVariable( "Generator Heat Recovery Mass Flow Rate [kg/s]", ICEngineGeneratorReport( GeneratorNum ).HeatRecMdot, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Jacket Heat Recovery Rate [W]", ICEngineGeneratorReport( GeneratorNum ).QJacketRecovered, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Jacket Heat Recovery Energy [J]", ICEngineGeneratorReport( GeneratorNum ).JacketEnergyRec, "System", "Sum", ICEngineGenerator( GeneratorNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Generator Lube Heat Recovery Rate [W]", ICEngineGeneratorReport( GeneratorNum ).QLubeOilRecovered, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Lube Heat Recovery Energy [J]", ICEngineGeneratorReport( GeneratorNum ).LubeOilEnergyRec, "System", "Sum", ICEngineGenerator( GeneratorNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Generator Exhaust Heat Recovery Rate [W]", ICEngineGeneratorReport( GeneratorNum ).QExhaustRecovered, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Exhaust Heat Recovery Energy [J]", ICEngineGeneratorReport( GeneratorNum ).ExhaustEnergyRec, "System", "Sum", ICEngineGenerator( GeneratorNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Generator Produced Thermal Rate [W]", ICEngineGeneratorReport( GeneratorNum ).QTotalHeatRecovered, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Produced Thermal Energy [J]", ICEngineGeneratorReport( GeneratorNum ).TotalHeatEnergyRec, "System", "Sum", ICEngineGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Heat Recovery Inlet Temperature [C]", ICEngineGeneratorReport( GeneratorNum ).HeatRecInletTemp, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Heat Recovery Outlet Temperature [C]", ICEngineGeneratorReport( GeneratorNum ).HeatRecOutletTemp, "System", "Average", ICEngineGenerator( GeneratorNum ).Name );
			}

		}

	}

	// End of Get Input subroutines for the IC ENGINE Generator Module
	//******************************************************************************

	// Beginning of Generator model Subroutines
	// *****************************************************************************

	void
	CalcICEngineGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 2000
		//       MODIFIED     na
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a IC ENGINE generator using the BLAST model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data:

		// REFERENCES:na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using CurveManager::CurveValue;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ExhaustCP( 1.047 ); // Exhaust Gas Specific Heat (J/kg-K)
		Real64 const KJtoJ( 1000.0 ); // convert Kjoules to joules
		static std::string const RoutineName( "CalcICEngineGeneratorModel" );

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 PLR; // Generator operating part load ratio
		Real64 RatedPowerOutput; // Generator nominal capacity (W)
		Real64 ElecPowerGenerated; // Generator output (W)
		Real64 ElectricEnergyGen; // Generator output (J)

		// Special variables for IC ENGINE Generator
		Real64 MaxExhaustperPowerOutput; // curve fit parameter
		Real64 ElecOutputFuelRat; // (RELDC) Ratio of generator output to Fuel Energy Input
		Real64 RecJacHeattoFuelRat; // (RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
		Real64 RecLubeHeattoFuelRat; // (RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
		Real64 TotExhausttoFuelRat; // (REXDC) Total Exhaust Energy Input to Fuel Energy Input
		Real64 ExhaustTemp; // (TEX) Exhaust Gas Temp
		Real64 UA; // (UACDC) exhaust gas Heat Exchanger UA
		Real64 FuelEnergyUseRate; // IC ENGINE fuel use rate (W)
		Real64 FuelEnergyUsed; // IC ENGINE fuel use (J)
		Real64 QTotalHeatRecovered;
		Real64 QJacketRec; // water jacket heat recovered (W)
		Real64 QLubeOilRec; // lube oil cooler heat recovered (W)
		Real64 QExhaustRec; // exhaust gas heat recovered (W)
		Real64 JacketEnergyRec; // water jacket heat recovered (J)
		Real64 LubeOilEnergyRec; // lube oil cooler heat recovered (J)
		Real64 ExhaustEnergyRec; // exhaust gas heat recovered (J)
		Real64 QExhaustTotal; // total engine exhaust heat (W)
		Real64 ExhaustGasFlow; // exhaust gas mass flow rate (kg/s)
		Real64 ExhaustStackTemp( 0 ); // engine stack temp. (C)
		Real64 DesignMinExitGasTemp; // design engine stact saturated steam temp. (C)
		Real64 FuelHeatingValue; // Heating Value of Fuel in kJ/kg
		int HeatRecInNode; // Heat Recovery Fluid Inlet Node Num
		Real64 HeatRecInTemp; // Heat Recovery Fluid Inlet Temperature (C)
		Real64 HeatRecMdot; // Heat Recovery Fluid Mass FlowRate (kg/s)
		Real64 HRecRatio; // When Max Temp is reached the amount of recovered heat has to be reduced.
		// and this assumption uses this ratio to accomplish this task.

		// LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		MinPartLoadRat = ICEngineGenerator( GeneratorNum ).MinPartLoadRat;
		MaxPartLoadRat = ICEngineGenerator( GeneratorNum ).MaxPartLoadRat;
		RatedPowerOutput = ICEngineGenerator( GeneratorNum ).RatedPowerOutput;
		MaxExhaustperPowerOutput = ICEngineGenerator( GeneratorNum ).MaxExhaustperPowerOutput;
		if ( ICEngineGenerator( GeneratorNum ).HeatRecActive ) {
			HeatRecInNode = ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum;
			HeatRecInTemp = Node( HeatRecInNode ).Temp;
			HeatRecMdot = Node( HeatRecInNode ).MassFlowRate;

		} else {
			HeatRecInTemp = 0.0;
			HeatRecMdot = 0.0;
		}

		//If no loop demand or Generator OFF, return
		if ( ! RunFlag ) {
			ICEngineGenerator( GeneratorNum ).ElecPowerGenerated = 0.0;
			ICEngineGenerator( GeneratorNum ).ElecEnergyGenerated = 0.0;
			ICEngineGenerator( GeneratorNum ).HeatRecInletTemp = HeatRecInTemp;
			ICEngineGenerator( GeneratorNum ).HeatRecOutletTemp = HeatRecInTemp;
			ICEngineGenerator( GeneratorNum ).HeatRecMdotActual = 0.0;
			ICEngineGenerator( GeneratorNum ).QJacketRecovered = 0.0;
			ICEngineGenerator( GeneratorNum ).QExhaustRecovered = 0.0;
			ICEngineGenerator( GeneratorNum ).QLubeOilRecovered = 0.0;
			ICEngineGenerator( GeneratorNum ).QTotalHeatRecovered = 0.0;
			ICEngineGenerator( GeneratorNum ).JacketEnergyRec = 0.0;
			ICEngineGenerator( GeneratorNum ).ExhaustEnergyRec = 0.0;
			ICEngineGenerator( GeneratorNum ).LubeOilEnergyRec = 0.0;
			ICEngineGenerator( GeneratorNum ).TotalHeatEnergyRec = 0.0;
			ICEngineGenerator( GeneratorNum ).FuelEnergyUseRate = 0.0;
			ICEngineGenerator( GeneratorNum ).FuelEnergy = 0.0;
			ICEngineGenerator( GeneratorNum ).FuelMdot = 0.0;
			ICEngineGenerator( GeneratorNum ).ExhaustStackTemp = 0.0;

			return;
		}

		// CALCULATE POWER GENERATED AND PLR
		ElecPowerGenerated = min( MyLoad, RatedPowerOutput );
		ElecPowerGenerated = max( ElecPowerGenerated, 0.0 );
		PLR = min( ElecPowerGenerated / RatedPowerOutput, MaxPartLoadRat );
		PLR = max( PLR, MinPartLoadRat );
		ElecPowerGenerated = PLR * RatedPowerOutput;

		//DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

		//Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
		//energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/power generated (J/s).
		if ( PLR > 0.0 ) {
			ElecOutputFuelRat = CurveValue( ICEngineGenerator( GeneratorNum ).ElecOutputFuelCurve, PLR );
			FuelEnergyUseRate = ElecPowerGenerated / ElecOutputFuelRat;
		} else {
			FuelEnergyUseRate = 0.0;
		}

		//Use Curve fit to determine heat recovered in the water jacket.  This curve calculates the water jacket heat recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
		//particular part load.

		RecJacHeattoFuelRat = CurveValue( ICEngineGenerator( GeneratorNum ).RecJacHeattoFuelCurve, PLR );
		QJacketRec = FuelEnergyUseRate * RecJacHeattoFuelRat;

		//Use Curve fit to determine Heat Recovered Lubricant heat.  This curve calculates the lube heat recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
		//particular part load.
		RecLubeHeattoFuelRat = CurveValue( ICEngineGenerator( GeneratorNum ).RecLubeHeattoFuelCurve, PLR );
		QLubeOilRec = FuelEnergyUseRate * RecLubeHeattoFuelRat;

		//Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  heat recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
		//particular part load.
		TotExhausttoFuelRat = CurveValue( ICEngineGenerator( GeneratorNum ).TotExhausttoFuelCurve, PLR );
		QExhaustTotal = FuelEnergyUseRate * TotExhausttoFuelRat;

		//Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
		//of the exhaust temperature in C to the part load ratio.
		if ( PLR > 0.0 ) {
			ExhaustTemp = CurveValue( ICEngineGenerator( GeneratorNum ).ExhaustTempCurve, PLR );

			if ( ExhaustTemp > ReferenceTemp ) {

				ExhaustGasFlow = QExhaustTotal / ( ExhaustCP * ( ExhaustTemp - ReferenceTemp ) );

				//Use Curve fit to determine stack temp after heat recovery
				UA = ICEngineGenerator( GeneratorNum ).UACoef( 1 ) * std::pow( RatedPowerOutput, ICEngineGenerator( GeneratorNum ).UACoef( 2 ) );

				DesignMinExitGasTemp = ICEngineGenerator( GeneratorNum ).DesignMinExitGasTemp;

				ExhaustStackTemp = DesignMinExitGasTemp + ( ExhaustTemp - DesignMinExitGasTemp ) / std::exp( UA / ( max( ExhaustGasFlow, MaxExhaustperPowerOutput * RatedPowerOutput ) * ExhaustCP ) );

				QExhaustRec = max( ExhaustGasFlow * ExhaustCP * ( ExhaustTemp - ExhaustStackTemp ), 0.0 );
			} else {
				if ( ICEngineGenerator( GeneratorNum ).ErrExhaustTempIndex == 0 ) {
					ShowWarningMessage( "CalcICEngineGeneratorModel: " + ICEngineGenerator( GeneratorNum ).TypeOf + "=\"" + ICEngineGenerator( GeneratorNum ).Name + "\" low Exhaust Temperature from Curve Value" );
					ShowContinueError( "...curve generated temperature=[" + RoundSigDigits( ExhaustTemp, 3 ) + " C], PLR=[" + RoundSigDigits( PLR, 3 ) + "]." );
					ShowContinueError( "...simulation will continue with exhaust heat reclaim set to 0." );
				}
				ShowRecurringWarningErrorAtEnd( "CalcICEngineGeneratorModel: " + ICEngineGenerator( GeneratorNum ).TypeOf + "=\"" + ICEngineGenerator( GeneratorNum ).Name + "\" low Exhaust Temperature continues...", ICEngineGenerator( GeneratorNum ).ErrExhaustTempIndex, ExhaustTemp, ExhaustTemp, _, "[C]", "[C]" );
				QExhaustRec = 0.0;
				ExhaustStackTemp = ICEngineGenerator( GeneratorNum ).DesignMinExitGasTemp;
			}
		} else {
			QExhaustRec = 0.0;
			//Bug ExhaustStackTemp not set but used below
		}

		QTotalHeatRecovered = QExhaustRec + QLubeOilRec + QJacketRec;

		if ( ICEngineGenerator( GeneratorNum ).HeatRecActive ) {
			CalcICEngineGenHeatRecovery( GeneratorNum, QTotalHeatRecovered, HeatRecMdot, HRecRatio );
			QExhaustRec *= HRecRatio;
			QLubeOilRec *= HRecRatio;
			QJacketRec *= HRecRatio;
			QTotalHeatRecovered *= HRecRatio;
		} else {
			ICEngineGenerator( GeneratorNum ).HeatRecInletTemp = HeatRecInTemp;
			ICEngineGenerator( GeneratorNum ).HeatRecOutletTemp = HeatRecInTemp;
			ICEngineGenerator( GeneratorNum ).HeatRecMdotActual = HeatRecMdot;

		}

		//Calculate Energy
		ElectricEnergyGen = ElecPowerGenerated * TimeStepSys * SecInHour;
		FuelEnergyUsed = FuelEnergyUseRate * TimeStepSys * SecInHour;
		JacketEnergyRec = QJacketRec * TimeStepSys * SecInHour;
		LubeOilEnergyRec = QLubeOilRec * TimeStepSys * SecInHour;
		ExhaustEnergyRec = QExhaustRec * TimeStepSys * SecInHour;

		ICEngineGenerator( GeneratorNum ).ElecPowerGenerated = ElecPowerGenerated;
		ICEngineGenerator( GeneratorNum ).ElecEnergyGenerated = ElectricEnergyGen;
		ICEngineGenerator( GeneratorNum ).QJacketRecovered = QJacketRec;
		ICEngineGenerator( GeneratorNum ).QLubeOilRecovered = QLubeOilRec;
		ICEngineGenerator( GeneratorNum ).QExhaustRecovered = QExhaustRec;
		ICEngineGenerator( GeneratorNum ).QTotalHeatRecovered = QTotalHeatRecovered;
		ICEngineGenerator( GeneratorNum ).JacketEnergyRec = JacketEnergyRec;
		ICEngineGenerator( GeneratorNum ).LubeOilEnergyRec = LubeOilEnergyRec;
		ICEngineGenerator( GeneratorNum ).ExhaustEnergyRec = ExhaustEnergyRec;
		ICEngineGenerator( GeneratorNum ).QTotalHeatRecovered = ( QExhaustRec + QLubeOilRec + QJacketRec );
		ICEngineGenerator( GeneratorNum ).TotalHeatEnergyRec = ( ExhaustEnergyRec + LubeOilEnergyRec + JacketEnergyRec );
		ICEngineGenerator( GeneratorNum ).FuelEnergyUseRate = std::abs( FuelEnergyUseRate );
		ICEngineGenerator( GeneratorNum ).FuelEnergy = std::abs( FuelEnergyUsed );

		FuelHeatingValue = ICEngineGenerator( GeneratorNum ).FuelHeatingValue;

		ICEngineGenerator( GeneratorNum ).FuelMdot = std::abs( FuelEnergyUseRate ) / ( FuelHeatingValue * KJtoJ );
		ICEngineGenerator( GeneratorNum ).ExhaustStackTemp = ExhaustStackTemp;

	}

	void
	CalcICEngineGenHeatRecovery(
		int const Num, // HR Component number
		Real64 const EnergyRecovered, // Amount of heat recovered
		Real64 const HeatRecMdot,
		Real64 & HRecRatio // Max Heat recovery ratio
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Brandon Anderson
		//       DATE WRITTEN:    November 2000

		// PURPOSE OF THIS SUBROUTINE:
		// To perform heat recovery calculations and node updates

		// METHODOLOGY EMPLOYED: This routine is required for the heat recovery loop.
		// It works in conjunction with the Heat Recovery Manager, and the PlantWaterHeater.
		// The chiller sets the flow on the loop first by the input design flowrate and then
		// performs a check to verify that

		// REFERENCES: na

		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcICEngineGeneratorModel" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatRecInNode;
		Real64 MinHeatRecMdot;
		Real64 HeatRecInTemp;
		Real64 HeatRecOutTemp;
		Real64 HeatRecCp;

		//Load inputs to local structure
		HeatRecInNode = ICEngineGenerator( Num ).HeatRecInletNodeNum;

		//Need to set the HeatRecRatio to 1.0 if it is not modified
		HRecRatio = 1.0;

		HeatRecInTemp = Node( HeatRecInNode ).Temp;
		HeatRecCp = GetSpecificHeatGlycol( PlantLoop( ICEngineGenerator( Num ).HRLoopNum ).FluidName, HeatRecInTemp, PlantLoop( ICEngineGenerator( Num ).HRLoopNum ).FluidIndex, RoutineName );

		//Don't divide by zero - Note This also results in no heat recovery when
		//  design Mdot for Heat Recovery - Specified on Chiller Input - is zero
		//  In order to see what minimum heat recovery flow rate is for the design temperature
		//  The design heat recovery flow rate can be set very small, but greater than zero.
		if ( ( HeatRecMdot > 0 ) && ( HeatRecCp > 0 ) ) {
			HeatRecOutTemp = ( EnergyRecovered ) / ( HeatRecMdot * HeatRecCp ) + HeatRecInTemp;
		} else {
			HeatRecOutTemp = HeatRecInTemp;
		}

		//Note: check to make sure the Max Temperature was not exceeded
		if ( HeatRecOutTemp > ICEngineGenerator( Num ).HeatRecMaxTemp ) {
			if ( ICEngineGenerator( Num ).HeatRecMaxTemp != HeatRecInTemp ) {
				MinHeatRecMdot = ( EnergyRecovered ) / ( HeatRecCp * ( ICEngineGenerator( Num ).HeatRecMaxTemp - HeatRecInTemp ) );
				if ( MinHeatRecMdot < 0.0 ) MinHeatRecMdot = 0.0;
			} else {
				MinHeatRecMdot = 0.0;
			}

			//Recalculate Outlet Temperature, with adjusted flowrate
			if ( ( MinHeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
				HeatRecOutTemp = ( EnergyRecovered ) / ( MinHeatRecMdot * HeatRecCp ) + HeatRecInTemp;
				HRecRatio = HeatRecMdot / MinHeatRecMdot;
			} else {
				HeatRecOutTemp = HeatRecInTemp;
				HRecRatio = 0.0;
			}

		}

		//Update global variables for reporting later
		ICEngineGenerator( Num ).HeatRecInletTemp = HeatRecInTemp;
		ICEngineGenerator( Num ).HeatRecOutletTemp = HeatRecOutTemp;
		ICEngineGenerator( Num ).HeatRecMdotActual = HeatRecMdot;

	}

	// End IC ENGINE Generator Module Model Subroutines
	// *****************************************************************************

	// Begin IC ENGINE Generator Module Utility Subroutines
	// *****************************************************************************

	void
	InitICEngineGenerators(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const EP_UNUSED( MyLoad ), // Generator demand
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Oct 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  Brent Griffith, Sept 2010, plant upgrades, generalize fluid props

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the IC ENGINE generators.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_Generator_ICEngine;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitICEngineGenerators" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatRecInletNode; // inlet node number in heat recovery loop
		int HeatRecOutletNode; // outlet node number in heat recovery loop
		static bool MyOneTimeFlag( true ); // Initialization flag

		static Array1D_bool MyEnvrnFlag; // Used for initializations each begin environment flag
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MySizeAndNodeInitFlag;
		Real64 mdot;
		Real64 rho;
		bool errFlag;

		// FLOW:
		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumICEngineGenerators );
			MyPlantScanFlag.allocate( NumICEngineGenerators );
			MySizeAndNodeInitFlag.allocate( NumICEngineGenerators );
			MyEnvrnFlag = true;
			MyPlantScanFlag = true;
			MyOneTimeFlag = false;
			MySizeAndNodeInitFlag = true;
		}
		if ( MyPlantScanFlag( GeneratorNum ) && allocated( PlantLoop ) && ICEngineGenerator( GeneratorNum ).HeatRecActive ) {
			errFlag = false;
			ScanPlantLoopsForObject( ICEngineGenerator( GeneratorNum ).Name, TypeOf_Generator_ICEngine, ICEngineGenerator( GeneratorNum ).HRLoopNum, ICEngineGenerator( GeneratorNum ).HRLoopSideNum, ICEngineGenerator( GeneratorNum ).HRBranchNum, ICEngineGenerator( GeneratorNum ).HRCompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitICEngineGenerators: Program terminated due to previous condition(s)." );
			}

			MyPlantScanFlag( GeneratorNum ) = false;
		}

		if ( MySizeAndNodeInitFlag( GeneratorNum ) && ( ! MyPlantScanFlag( GeneratorNum ) ) && ICEngineGenerator( GeneratorNum ).HeatRecActive ) {

			HeatRecInletNode = ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum;
			HeatRecOutletNode = ICEngineGenerator( GeneratorNum ).HeatRecOutletNodeNum;

			//size mass flow rate
			rho = GetDensityGlycol( PlantLoop( ICEngineGenerator( GeneratorNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( ICEngineGenerator( GeneratorNum ).HRLoopNum ).FluidIndex, RoutineName );

			ICEngineGenerator( GeneratorNum ).DesignHeatRecMassFlowRate = rho * ICEngineGenerator( GeneratorNum ).DesignHeatRecVolFlowRate;
			ICEngineGenerator( GeneratorNum ).HeatRecMdotDesign = ICEngineGenerator( GeneratorNum ).DesignHeatRecMassFlowRate;

			InitComponentNodes( 0.0, ICEngineGenerator( GeneratorNum ).DesignHeatRecMassFlowRate, HeatRecInletNode, HeatRecOutletNode, ICEngineGenerator( GeneratorNum ).HRLoopNum, ICEngineGenerator( GeneratorNum ).HRLoopSideNum, ICEngineGenerator( GeneratorNum ).HRBranchNum, ICEngineGenerator( GeneratorNum ).HRCompNum );

			MySizeAndNodeInitFlag( GeneratorNum ) = false;
		} // end one time inits

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( GeneratorNum ) && ICEngineGenerator( GeneratorNum ).HeatRecActive ) {
			HeatRecInletNode = ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum;
			HeatRecOutletNode = ICEngineGenerator( GeneratorNum ).HeatRecOutletNodeNum;
			// set the node Temperature, assuming freeze control
			Node( HeatRecInletNode ).Temp = 20.0;
			Node( HeatRecOutletNode ).Temp = 20.0;
			// set the node max and min mass flow rates
			InitComponentNodes( 0.0, ICEngineGenerator( GeneratorNum ).DesignHeatRecMassFlowRate, HeatRecInletNode, HeatRecOutletNode, ICEngineGenerator( GeneratorNum ).HRLoopNum, ICEngineGenerator( GeneratorNum ).HRLoopSideNum, ICEngineGenerator( GeneratorNum ).HRBranchNum, ICEngineGenerator( GeneratorNum ).HRCompNum );

			MyEnvrnFlag( GeneratorNum ) = false;
		} // end environmental inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( GeneratorNum ) = true;
		}

		if ( ICEngineGenerator( GeneratorNum ).HeatRecActive ) {
			if ( FirstHVACIteration ) {
				if ( RunFlag ) {
					mdot = ICEngineGenerator( GeneratorNum ).DesignHeatRecMassFlowRate;
				} else {
					mdot = 0.0;
				}
				SetComponentFlowRate( mdot, ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum, ICEngineGenerator( GeneratorNum ).HeatRecOutletNodeNum, ICEngineGenerator( GeneratorNum ).HRLoopNum, ICEngineGenerator( GeneratorNum ).HRLoopSideNum, ICEngineGenerator( GeneratorNum ).HRBranchNum, ICEngineGenerator( GeneratorNum ).HRCompNum );

			} else {
				SetComponentFlowRate( ICEngineGenerator( GeneratorNum ).HeatRecMdotActual, ICEngineGenerator( GeneratorNum ).HeatRecInletNodeNum, ICEngineGenerator( GeneratorNum ).HeatRecOutletNodeNum, ICEngineGenerator( GeneratorNum ).HRLoopNum, ICEngineGenerator( GeneratorNum ).HRLoopSideNum, ICEngineGenerator( GeneratorNum ).HRBranchNum, ICEngineGenerator( GeneratorNum ).HRCompNum );
			}
		}

	}

	// End IC ENGINE Generator Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the IC ENGINE Generator Module
	// *****************************************************************************

	void
	UpdateICEngineGeneratorRecords(
		bool const EP_UNUSED( RunFlag ), // TRUE if Generator operating
		int const Num // Generator number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 2000

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatRecOutletNode;

		if ( ICEngineGenerator( Num ).HeatRecActive ) {
			HeatRecOutletNode = ICEngineGenerator( Num ).HeatRecOutletNodeNum;
			//      Node(HeatRecOutletNode)%MassFlowRate            = ICEngineGenerator(Num)%HeatRecMdotActual
			Node( HeatRecOutletNode ).Temp = ICEngineGenerator( Num ).HeatRecOutletTemp;
			//      Node(HeatRecOutletNode)%MassFlowRateMaxAvail = Node(HeatRecInletNode)%MassFlowRateMaxAvail
			//      Node(HeatRecOutletNode)%MassFlowRateMinAvail = Node(HeatRecInletNode)%MassFlowRateMinAvail
		}
		ICEngineGeneratorReport( Num ).PowerGen = ICEngineGenerator( Num ).ElecPowerGenerated;
		ICEngineGeneratorReport( Num ).QJacketRecovered = ICEngineGenerator( Num ).QJacketRecovered;
		ICEngineGeneratorReport( Num ).QLubeOilRecovered = ICEngineGenerator( Num ).QLubeOilRecovered;
		ICEngineGeneratorReport( Num ).QExhaustRecovered = ICEngineGenerator( Num ).QExhaustRecovered;
		ICEngineGeneratorReport( Num ).QTotalHeatRecovered = ICEngineGenerator( Num ).QTotalHeatRecovered;
		ICEngineGeneratorReport( Num ).FuelEnergyUseRate = ICEngineGenerator( Num ).FuelEnergyUseRate;
		ICEngineGeneratorReport( Num ).EnergyGen = ICEngineGenerator( Num ).ElecEnergyGenerated;
		ICEngineGeneratorReport( Num ).JacketEnergyRec = ICEngineGenerator( Num ).JacketEnergyRec;
		ICEngineGeneratorReport( Num ).LubeOilEnergyRec = ICEngineGenerator( Num ).LubeOilEnergyRec;
		ICEngineGeneratorReport( Num ).ExhaustEnergyRec = ICEngineGenerator( Num ).ExhaustEnergyRec;
		ICEngineGeneratorReport( Num ).TotalHeatEnergyRec = ICEngineGenerator( Num ).TotalHeatEnergyRec;
		ICEngineGeneratorReport( Num ).FuelEnergy = ICEngineGenerator( Num ).FuelEnergy;
		ICEngineGeneratorReport( Num ).FuelMdot = ICEngineGenerator( Num ).FuelMdot;
		ICEngineGeneratorReport( Num ).ExhaustStackTemp = ICEngineGenerator( Num ).ExhaustStackTemp;
		ICEngineGeneratorReport( Num ).HeatRecInletTemp = ICEngineGenerator( Num ).HeatRecInletTemp;
		ICEngineGeneratorReport( Num ).HeatRecOutletTemp = ICEngineGenerator( Num ).HeatRecOutletTemp;
		ICEngineGeneratorReport( Num ).HeatRecMdot = ICEngineGenerator( Num ).HeatRecMdotActual;

	}

	// End of Record Keeping subroutines for the IC ENGINE Generator Module
	// *****************************************************************************

} // ICEngineElectricGenerator

} // EnergyPlus
