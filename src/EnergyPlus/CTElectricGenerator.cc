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
#include <CTElectricGenerator.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace CTElectricGenerator {

	//__________________________________________________________________________
	// BLAST inherited generators:
	// CTElectricGenerator (COMBUSTION Turbine)

	// MODULE INFORMATION:
	//       AUTHOR         Dan Fisher
	//       DATE WRITTEN   Sept 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates the performance of the COMBUSTION turbine
	// Generators.

	// METHODOLOGY EMPLOYED:
	// Once the Electric power manager determines that the CT Generator
	// is available, it calls SimCTGenerator which in turn calls the
	// appropriate COMBUSTION turbine Generator model.
	// All CT Generator models are based on a polynomial fit of Generator
	// performance data.

	// REFERENCES: na

	// OTHER NOTES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::SecInHour;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobalConstants::iGeneratorCombTurbine;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumCTGenerators( 0 ); // number of CT Generators specified in input
	bool GetCTInput( true ); // then TRUE, calls subroutine to read input file.

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Object Data
	Array1D< CTGeneratorSpecs > CTGenerator; // dimension to number of machines
	Array1D< ReportVars > CTGeneratorReport;

	// MODULE SUBROUTINES:
	// Beginning of CT Generator Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimCTGenerator(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // simulate Generator when TRUE
		Real64 const MyLoad, // generator demand
		bool const FirstHVACIteration
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the CT Generator driver.  It
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
		if ( GetCTInput ) {
			GetCTGeneratorInput();
			GetCTInput = false;
		}

		//SELECT and CALL MODELS
		if ( GeneratorIndex == 0 ) {
			GenNum = FindItemInList( GeneratorName, CTGenerator );
			if ( GenNum == 0 ) ShowFatalError( "SimCTGenerator: Specified Generator not one of Valid COMBUSTION Turbine Generators " + GeneratorName );
			GeneratorIndex = GenNum;
		} else {
			GenNum = GeneratorIndex;
			if ( GenNum > NumCTGenerators || GenNum < 1 ) {
				ShowFatalError( "SimCTGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Number of CT Engine Generators=" + TrimSigDigits( NumCTGenerators ) + ", Generator name=" + GeneratorName );
			}
			if ( CheckEquipName( GenNum ) ) {
				if ( GeneratorName != CTGenerator( GenNum ).Name ) {
					ShowFatalError( "SimCTGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + CTGenerator( GenNum ).Name );
				}
				CheckEquipName( GenNum ) = false;
			}
		}

		InitCTGenerators( GenNum, RunFlag, MyLoad, FirstHVACIteration );
		CalcCTGeneratorModel( GenNum, RunFlag, MyLoad, FirstHVACIteration );
		UpdateCTGeneratorRecords( RunFlag, GenNum );

	}

	void
	SimCTPlantHeatRecovery(
		std::string const & EP_UNUSED( CompType ), // unused1208
		std::string const & CompName,
		int const EP_UNUSED( CompTypeNum ), // unused1208
		int & CompNum,
		bool const EP_UNUSED( RunFlag ),
		bool & InitLoopEquip,
		Real64 & EP_UNUSED( MyLoad ),
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const EP_UNUSED( FirstHVACIteration ) // TRUE if First iteration of simulation
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

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN)          :: FlowLock !unused1208 !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( GetCTInput ) {
			GetCTGeneratorInput();
			GetCTInput = false;
		}

		if ( InitLoopEquip ) {
			CompNum = FindItemInList( CompName, CTGenerator );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimCTPlantHeatRecovery: CT Generator Unit not found=" + CompName );
				return;
			}
			MinCap = 0.0;
			MaxCap = 0.0;
			OptCap = 0.0;
			return;
		} // End Of InitLoopEquip

	}

	// End CT Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of CT Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetCTGeneratorInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the CT Generator models.

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using CurveManager::GetCurveIndex;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// PARAMETERS

		//LOCAL VARIABLES
		int GeneratorNum; // Generator counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 12 ); // character string data
		Array1D< Real64 > NumArray( 12 ); // numeric data
		static bool ErrorsFound( false ); // error flag
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		//FLOW

		cCurrentModuleObject = "Generator:CombustionTurbine";
		NumCTGenerators = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumCTGenerators <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}

		//ALLOCATE ARRAYS
		CTGenerator.allocate( NumCTGenerators );
		CheckEquipName.dimension( NumCTGenerators, true );

		CTGeneratorReport.allocate( NumCTGenerators );

		//LOAD ARRAYS WITH CT CURVE FIT Generator DATA
		for ( GeneratorNum = 1; GeneratorNum <= NumCTGenerators; ++GeneratorNum ) {
			GetObjectItem( cCurrentModuleObject, GeneratorNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), CTGenerator, GeneratorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			CTGenerator( GeneratorNum ).Name = AlphArray( 1 );

			CTGenerator( GeneratorNum ).RatedPowerOutput = NumArray( 1 );
			if ( NumArray( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( NumArray( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			// Not sure what to do with electric nodes, so do not use optional arguments
			CTGenerator( GeneratorNum ).ElectricCircuitNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Electric, NodeConnectionType_Electric, 1, ObjectIsNotParent );

			CTGenerator( GeneratorNum ).MinPartLoadRat = NumArray( 2 );
			CTGenerator( GeneratorNum ).MaxPartLoadRat = NumArray( 3 );
			CTGenerator( GeneratorNum ).OptPartLoadRat = NumArray( 4 );

			//Load Special CT Generator Input

			CTGenerator( GeneratorNum ).PLBasedFuelInputCurve = GetCurveIndex( AlphArray( 3 ) ); // convert curve name to number
			if ( CTGenerator( GeneratorNum ).PLBasedFuelInputCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + AlphArray( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			CTGenerator( GeneratorNum ).TempBasedFuelInputCurve = GetCurveIndex( AlphArray( 4 ) ); // convert curve name to number
			if ( CTGenerator( GeneratorNum ).TempBasedFuelInputCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + AlphArray( 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			CTGenerator( GeneratorNum ).ExhaustFlowCurve = GetCurveIndex( AlphArray( 5 ) ); // convert curve name to number
			if ( CTGenerator( GeneratorNum ).ExhaustFlowCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + AlphArray( 5 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			CTGenerator( GeneratorNum ).PLBasedExhaustTempCurve = GetCurveIndex( AlphArray( 6 ) ); // convert curve name to number
			if ( CTGenerator( GeneratorNum ).PLBasedExhaustTempCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + AlphArray( 6 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			CTGenerator( GeneratorNum ).TempBasedExhaustTempCurve = GetCurveIndex( AlphArray( 7 ) ); // convert curve name to number
			if ( CTGenerator( GeneratorNum ).TempBasedExhaustTempCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + AlphArray( 7 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			CTGenerator( GeneratorNum ).QLubeOilRecoveredCurve = GetCurveIndex( AlphArray( 8 ) ); // convert curve name to number
			if ( CTGenerator( GeneratorNum ).QLubeOilRecoveredCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + AlphArray( 8 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			CTGenerator( GeneratorNum ).UACoef( 1 ) = NumArray( 5 );
			CTGenerator( GeneratorNum ).UACoef( 2 ) = NumArray( 6 );

			CTGenerator( GeneratorNum ).MaxExhaustperCTPower = NumArray( 7 );
			CTGenerator( GeneratorNum ).DesignMinExitGasTemp = NumArray( 8 );
			CTGenerator( GeneratorNum ).DesignAirInletTemp = NumArray( 9 );
			CTGenerator( GeneratorNum ).FuelHeatingValue = NumArray( 10 );
			CTGenerator( GeneratorNum ).DesignHeatRecVolFlowRate = NumArray( 11 );

			if ( CTGenerator( GeneratorNum ).DesignHeatRecVolFlowRate > 0.0 ) {
				CTGenerator( GeneratorNum ).HeatRecActive = true;
				CTGenerator( GeneratorNum ).HeatRecInletNodeNum = GetOnlySingleNode( AlphArray( 9 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				if ( CTGenerator( GeneratorNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( "Missing Node Name, Heat Recovery Inlet, for " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
				CTGenerator( GeneratorNum ).HeatRecOutletNodeNum = GetOnlySingleNode( AlphArray( 10 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				if ( CTGenerator( GeneratorNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( "Missing Node Name, Heat Recovery Outlet, for " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
				TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 9 ), AlphArray( 10 ), "Heat Recovery Nodes" );
				RegisterPlantCompDesignFlow( CTGenerator( GeneratorNum ).HeatRecInletNodeNum, CTGenerator( GeneratorNum ).DesignHeatRecVolFlowRate );
			} else {
				CTGenerator( GeneratorNum ).HeatRecActive = false;
				CTGenerator( GeneratorNum ).HeatRecInletNodeNum = 0;
				CTGenerator( GeneratorNum ).HeatRecOutletNodeNum = 0;
				if ( ! lAlphaFieldBlanks( 9 ) || ! lAlphaFieldBlanks( 10 ) ) {
					ShowWarningError( "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "However, Node names were specified for Heat Recovery inlet or outlet nodes" );
				}
			}

			//Fuel Type Case Statement
			{ auto const SELECT_CASE_var( AlphArray( 11 ) );
			if ( is_blank( SELECT_CASE_var ) ) { //If blank then the default is Natural Gas
				CTGenerator( GeneratorNum ).FuelType = "Gas";

			} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
				CTGenerator( GeneratorNum ).FuelType = "Gas";

			} else if ( SELECT_CASE_var == "DIESEL" ) {
				CTGenerator( GeneratorNum ).FuelType = "Diesel";

			} else if ( SELECT_CASE_var == "GASOLINE" ) {
				CTGenerator( GeneratorNum ).FuelType = "Gasoline";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
				CTGenerator( GeneratorNum ).FuelType = "FuelOil#1";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
				CTGenerator( GeneratorNum ).FuelType = "FuelOil#2";

			} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
				CTGenerator( GeneratorNum ).FuelType = "Propane";

			} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
				CTGenerator( GeneratorNum ).FuelType = "OtherFuel1";

			} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
				CTGenerator( GeneratorNum ).FuelType = "OtherFuel2";

			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + AlphArray( 11 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}}

			CTGenerator( GeneratorNum ).HeatRecMaxTemp = NumArray( 12 );

			//begin CR7021
			if ( lAlphaFieldBlanks( 12 ) ) {
				CTGenerator( GeneratorNum ).OAInletNode = 0;
			} else {
				CTGenerator( GeneratorNum ).OAInletNode = GetOnlySingleNode( AlphArray( 12 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( CTGenerator( GeneratorNum ).OAInletNode ) ) {
					ShowSevereError( cCurrentModuleObject + ", \"" + CTGenerator( GeneratorNum ).Name + "\" Outdoor Air Inlet Node Name not valid Outdoor Air Node= " + AlphArray( 12 ) );
					ShowContinueError( "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ErrorsFound = true;
				}

			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( GeneratorNum = 1; GeneratorNum <= NumCTGenerators; ++GeneratorNum ) {
			SetupOutputVariable( "Generator Produced Electric Power [W]", CTGeneratorReport( GeneratorNum ).PowerGen, "System", "Average", CTGenerator( GeneratorNum ).Name );
			SetupOutputVariable( "Generator Produced Electric Energy [J]", CTGeneratorReport( GeneratorNum ).EnergyGen, "System", "Sum", CTGenerator( GeneratorNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Generator " + CTGenerator( GeneratorNum ).FuelType + " Rate [W]", CTGeneratorReport( GeneratorNum ).FuelEnergyUseRate, "System", "Average", CTGenerator( GeneratorNum ).Name );
			SetupOutputVariable( "Generator " + CTGenerator( GeneratorNum ).FuelType + " Energy [J]", CTGeneratorReport( GeneratorNum ).FuelEnergy, "System", "Sum", CTGenerator( GeneratorNum ).Name, _, CTGenerator( GeneratorNum ).FuelType, "COGENERATION", _, "Plant" );

			//    general fuel use report (to match other generators)
			SetupOutputVariable( "Generator Fuel HHV Basis Rate [W]", CTGeneratorReport( GeneratorNum ).FuelEnergyUseRate, "System", "Average", CTGenerator( GeneratorNum ).Name );
			SetupOutputVariable( "Generator Fuel HHV Basis Energy [J]", CTGeneratorReport( GeneratorNum ).FuelEnergy, "System", "Sum", CTGenerator( GeneratorNum ).Name );

			SetupOutputVariable( "Generator " + CTGenerator( GeneratorNum ).FuelType + " Mass Flow Rate [kg/s]", CTGeneratorReport( GeneratorNum ).FuelMdot, "System", "Average", CTGenerator( GeneratorNum ).Name );

			SetupOutputVariable( "Generator Exhaust Air Temperature [C]", CTGeneratorReport( GeneratorNum ).ExhaustStackTemp, "System", "Average", CTGenerator( GeneratorNum ).Name );

			if ( CTGenerator( GeneratorNum ).HeatRecActive ) {
				SetupOutputVariable( "Generator Exhaust Heat Recovery Rate [W]", CTGeneratorReport( GeneratorNum ).QExhaustRecovered, "System", "Average", CTGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Exhaust Heat Recovery Energy [J]", CTGeneratorReport( GeneratorNum ).ExhaustEnergyRec, "System", "Sum", CTGenerator( GeneratorNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Generator Lube Heat Recovery Rate [W]", CTGeneratorReport( GeneratorNum ).QLubeOilRecovered, "System", "Average", CTGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Lube Heat Recovery Energy [J]", CTGeneratorReport( GeneratorNum ).LubeOilEnergyRec, "System", "Sum", CTGenerator( GeneratorNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Generator Produced Thermal Rate [W]", CTGeneratorReport( GeneratorNum ).QTotalHeatRecovered, "System", "Average", CTGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Produced Thermal Energy [J]", CTGeneratorReport( GeneratorNum ).TotalHeatEnergyRec, "System", "Sum", CTGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Heat Recovery Inlet Temperature [C]", CTGeneratorReport( GeneratorNum ).HeatRecInletTemp, "System", "Average", CTGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Heat Recovery Outlet Temperature [C]", CTGeneratorReport( GeneratorNum ).HeatRecOutletTemp, "System", "Average", CTGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Heat Recovery Mass Flow Rate [kg/s]", CTGeneratorReport( GeneratorNum ).HeatRecMdot, "System", "Average", CTGenerator( GeneratorNum ).Name );
			}

		}

	}

	// End of Get Input subroutines for the CT Generator Module
	//******************************************************************************

	// Beginning of Generator model Subroutines
	// *****************************************************************************

	void
	CalcCTGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a vapor compression Generator using the CT model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data.  This model was originally
		// developed by Dale Herron for the BLAST program

		// REFERENCES: na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		using DataEnvironment::OutDryBulbTemp;
		using CurveManager::CurveValue;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ExhaustCP( 1.047 ); // Exhaust Gas Specific Heat (J/kg-K)
		Real64 const KJtoJ( 1000.0 ); // convert Kjoules to joules
		static std::string const RoutineName( "CalcCTGeneratorModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// INTERFACE

		//  REAL(r64) FUNCTION CurveValue(CurveIndex,Var1,Var2)
		//    INTEGER, INTENT (IN)        :: CurveIndex  ! index of curve in curve array
		//    REAL(r64), INTENT (IN)           :: Var1        ! 1st independent variable
		//    REAL(r64), INTENT (IN), OPTIONAL :: Var2        ! 2nd independent variable
		//  END FUNCTION CurveValue
		// END INTERFACE

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 RatedPowerOutput; // Generator nominal capacity (W)
		Real64 ElecPowerGenerated; // Generator output (W)
		Real64 ElectricEnergyGen; // Generator output (J)

		// Special variables for CT Generator
		Real64 MaxExhaustperCTPower; // MAX EXHAUST FLOW PER W POWER OUTPUT COEFF
		Real64 PLR; // Generator operating part load ratio
		Real64 FuelUseRate; // (EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
		Real64 FuelEnergyUsed; // Amount of Fuel Energy Required to run COMBUSTION turbine (J)
		Real64 ExhaustFlow; // (FEX) Exhaust Gas Flow Rate cubic meters per second???
		Real64 ExhaustTemp; // (TEX) Exhaust Gas Temperature in C
		Real64 UA; // (UACGC) Heat Exchanger UA to Capacity
		Real64 AmbientDeltaT; // (ATAIR) Difference between ambient actual and ambient design temperatures
		Real64 DesignAirInletTemp; // design turbine inlet temperature (C)
		Real64 QLubeOilRec; // recovered lube oil heat (W)
		Real64 QExhaustRec; // recovered exhaust heat (W)
		Real64 LubeOilEnergyRec; // recovered lube oil heat (J)
		Real64 ExhaustEnergyRec; // recovered exhaust heat (J)
		Real64 MinHeatRecMdot; // Heat Recovery Flow Rate if minimal heat recovery is accomplished
		Real64 DesignMinExitGasTemp; // design engine stact saturated steam temp. (C)
		Real64 ExhaustStackTemp; // turbine stack temp. (C)
		int HeatRecInNode; // Heat Recovery Fluid Inlet Node Num
		//notused  INTEGER :: HeatRecOutNode         !Heat Recovery Fluid Outlet Node Num
		Real64 HeatRecInTemp; // Heat Recovery Fluid Inlet Temperature (C)
		Real64 HeatRecOutTemp; // Heat Recovery Fluid Outlet Temperature (C)
		Real64 HeatRecMdot; // Heat Recovery Fluid Mass FlowRate (kg/s)
		Real64 HeatRecCp; // Specific Heat of the Heat Recovery Fluid (J/kg-K)
		Real64 FuelHeatingValue; // Heating Value of Fuel in (kJ/kg)
		Real64 HRecRatio; // When Max Temp is reached the amount of recovered heat has to be reduced.
		// and this assumption uses this ratio to accomplish this task.

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		MinPartLoadRat = CTGenerator( GeneratorNum ).MinPartLoadRat;
		MaxPartLoadRat = CTGenerator( GeneratorNum ).MaxPartLoadRat;
		RatedPowerOutput = CTGenerator( GeneratorNum ).RatedPowerOutput;
		MaxExhaustperCTPower = CTGenerator( GeneratorNum ).MaxExhaustperCTPower;
		DesignAirInletTemp = CTGenerator( GeneratorNum ).DesignAirInletTemp;
		if ( CTGenerator( GeneratorNum ).HeatRecActive ) {
			HeatRecInNode = CTGenerator( GeneratorNum ).HeatRecInletNodeNum;
			HeatRecInTemp = Node( HeatRecInNode ).Temp;

			HeatRecCp = GetSpecificHeatGlycol( PlantLoop( CTGenerator( GeneratorNum ).HRLoopNum ).FluidName, HeatRecInTemp, PlantLoop( CTGenerator( GeneratorNum ).HRLoopNum ).FluidIndex, RoutineName );
			if ( FirstHVACIteration && RunFlag ) {
				HeatRecMdot = CTGenerator( GeneratorNum ).DesignHeatRecMassFlowRate;
			} else {
				HeatRecMdot = Node( HeatRecInNode ).MassFlowRate;
			}
		} else {
			HeatRecInTemp = 0.0;
			HeatRecCp = 0.0;
			HeatRecMdot = 0.0;
		}

		//If no loop demand or Generator OFF, return
		if ( ! RunFlag ) {
			CTGenerator( GeneratorNum ).ElecPowerGenerated = 0.0;
			CTGenerator( GeneratorNum ).ElecEnergyGenerated = 0.0;
			CTGenerator( GeneratorNum ).HeatRecInletTemp = HeatRecInTemp;
			CTGenerator( GeneratorNum ).HeatRecOutletTemp = HeatRecInTemp;
			CTGenerator( GeneratorNum ).HeatRecMdot = 0.0;
			CTGenerator( GeneratorNum ).QLubeOilRecovered = 0.0;
			CTGenerator( GeneratorNum ).QExhaustRecovered = 0.0;
			CTGenerator( GeneratorNum ).QTotalHeatRecovered = 0.0;
			CTGenerator( GeneratorNum ).LubeOilEnergyRec = 0.0;
			CTGenerator( GeneratorNum ).ExhaustEnergyRec = 0.0;
			CTGenerator( GeneratorNum ).TotalHeatEnergyRec = 0.0;
			CTGenerator( GeneratorNum ).FuelEnergyUseRate = 0.0;
			CTGenerator( GeneratorNum ).FuelEnergy = 0.0;
			CTGenerator( GeneratorNum ).FuelMdot = 0.0;
			CTGenerator( GeneratorNum ).ExhaustStackTemp = 0.0;
			return;
		}

		// CALCULATE POWER GENERATED AND PLR
		ElecPowerGenerated = min( MyLoad, RatedPowerOutput );
		ElecPowerGenerated = max( ElecPowerGenerated, 0.0 );
		PLR = min( ElecPowerGenerated / RatedPowerOutput, MaxPartLoadRat );
		PLR = max( PLR, MinPartLoadRat );
		ElecPowerGenerated = PLR * RatedPowerOutput;

		// SET OFF-DESIGN AIR TEMPERATURE DIFFERENCE
		//   use OA node if set by user CR7021
		if ( CTGenerator( GeneratorNum ).OAInletNode == 0 ) {
			AmbientDeltaT = OutDryBulbTemp - DesignAirInletTemp;
		} else {
			AmbientDeltaT = Node( CTGenerator( GeneratorNum ).OAInletNode ).Temp - DesignAirInletTemp;
		}

		//Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
		//energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/power generated (J/s).
		//The TempBasedFuelInputCurve is a correction based on deviation from design inlet air temperature conditions.
		//The first coefficient of this fit should be 1.0 to ensure that no correction is made at design conditions.
		FuelUseRate = ElecPowerGenerated * CurveValue( CTGenerator( GeneratorNum ).PLBasedFuelInputCurve, PLR ) * CurveValue( CTGenerator( GeneratorNum ).TempBasedFuelInputCurve, AmbientDeltaT );

		//Use Curve fit to determine Exhaust Flow.  This curve shows the ratio of exhaust gas flow (kg/s) to electric power
		//output (J/s).  The units on ExhaustFlowCurve are (kg/J).  When multiplied by the rated power of the unit,
		//it gives the exhaust flow rate in kg/s
		ExhaustFlow = RatedPowerOutput * CurveValue( CTGenerator( GeneratorNum ).ExhaustFlowCurve, AmbientDeltaT );

		//Use Curve fit to determine Exhaust Temperature.  This curve calculates the exhaust temperature (C) by
		//multiplying the exhaust temperature (C) for a particular part load as given by PLBasedExhaustTempCurve
		//a correction factor based on the deviation from design temperature, TempBasedExhaustTempCurve
		if ( ( PLR > 0.0 ) && ( ( ExhaustFlow > 0.0 ) || ( MaxExhaustperCTPower > 0.0 ) ) ) {

			ExhaustTemp = CurveValue( CTGenerator( GeneratorNum ).PLBasedExhaustTempCurve, PLR ) * CurveValue( CTGenerator( GeneratorNum ).TempBasedExhaustTempCurve, AmbientDeltaT );

			UA = CTGenerator( GeneratorNum ).UACoef( 1 ) * std::pow( RatedPowerOutput, CTGenerator( GeneratorNum ).UACoef( 2 ) );

			DesignMinExitGasTemp = CTGenerator( GeneratorNum ).DesignMinExitGasTemp;
			ExhaustStackTemp = DesignMinExitGasTemp + ( ExhaustTemp - DesignMinExitGasTemp ) / std::exp( UA / ( max( ExhaustFlow, MaxExhaustperCTPower * RatedPowerOutput ) * ExhaustCP ) );

			QExhaustRec = max( ExhaustFlow * ExhaustCP * ( ExhaustTemp - ExhaustStackTemp ), 0.0 );
		} else {
			ExhaustStackTemp = CTGenerator( GeneratorNum ).DesignMinExitGasTemp;
			QExhaustRec = 0.0;
		}

		//Use Curve fit to determine Heat Recovered Lubricant heat.  This curve calculates the lube heat recovered (J/s) by
		//multiplying the total power generated by the fraction of that power that could be recovered in the lube oil at that
		//particular part load.
		QLubeOilRec = ElecPowerGenerated * CurveValue( CTGenerator( GeneratorNum ).QLubeOilRecoveredCurve, PLR );

		//Check for divide by zero
		if ( ( HeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
			HeatRecOutTemp = ( QExhaustRec + QLubeOilRec ) / ( HeatRecMdot * HeatRecCp ) + HeatRecInTemp;
		} else {
			HeatRecMdot = 0.0;
			HeatRecOutTemp = HeatRecInTemp;
			QExhaustRec = 0.0;
			QLubeOilRec = 0.0;
		}

		//Now verify the maximum temperature was not exceeded
		HRecRatio = 1.0;
		MinHeatRecMdot = 0.0;
		if ( HeatRecOutTemp > CTGenerator( GeneratorNum ).HeatRecMaxTemp ) {
			if ( CTGenerator( GeneratorNum ).HeatRecMaxTemp != HeatRecInTemp ) {
				MinHeatRecMdot = ( QExhaustRec + QLubeOilRec ) / ( HeatRecCp * ( CTGenerator( GeneratorNum ).HeatRecMaxTemp - HeatRecInTemp ) );
				if ( MinHeatRecMdot < 0.0 ) MinHeatRecMdot = 0.0;
			}

			//Recalculate Outlet Temperature, with adjusted flowrate
			if ( ( MinHeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
				HeatRecOutTemp = ( QExhaustRec + QLubeOilRec ) / ( MinHeatRecMdot * HeatRecCp ) + HeatRecInTemp;
				HRecRatio = HeatRecMdot / MinHeatRecMdot;
			} else {
				HeatRecOutTemp = HeatRecInTemp;
				HRecRatio = 0.0;
			}
			QLubeOilRec *= HRecRatio;
			QExhaustRec *= HRecRatio;
		}

		//Calculate Energy
		ElectricEnergyGen = ElecPowerGenerated * TimeStepSys * SecInHour;
		FuelEnergyUsed = FuelUseRate * TimeStepSys * SecInHour;
		LubeOilEnergyRec = QLubeOilRec * TimeStepSys * SecInHour;
		ExhaustEnergyRec = QExhaustRec * TimeStepSys * SecInHour;

		CTGenerator( GeneratorNum ).ElecPowerGenerated = ElecPowerGenerated;
		CTGenerator( GeneratorNum ).ElecEnergyGenerated = ElectricEnergyGen;

		CTGenerator( GeneratorNum ).HeatRecInletTemp = HeatRecInTemp;
		CTGenerator( GeneratorNum ).HeatRecOutletTemp = HeatRecOutTemp;

		CTGenerator( GeneratorNum ).HeatRecMdot = HeatRecMdot;
		CTGenerator( GeneratorNum ).QExhaustRecovered = QExhaustRec;
		CTGenerator( GeneratorNum ).QLubeOilRecovered = QLubeOilRec;
		CTGenerator( GeneratorNum ).QTotalHeatRecovered = QExhaustRec + QLubeOilRec;
		CTGenerator( GeneratorNum ).FuelEnergyUseRate = std::abs( FuelUseRate );
		CTGenerator( GeneratorNum ).ExhaustEnergyRec = ExhaustEnergyRec;
		CTGenerator( GeneratorNum ).LubeOilEnergyRec = LubeOilEnergyRec;
		CTGenerator( GeneratorNum ).TotalHeatEnergyRec = ExhaustEnergyRec + LubeOilEnergyRec;
		CTGenerator( GeneratorNum ).FuelEnergy = std::abs( FuelEnergyUsed );

		FuelHeatingValue = CTGenerator( GeneratorNum ).FuelHeatingValue;

		CTGenerator( GeneratorNum ).FuelMdot = std::abs( FuelUseRate ) / ( FuelHeatingValue * KJtoJ );

		CTGenerator( GeneratorNum ).ExhaustStackTemp = ExhaustStackTemp;

	}

	// End of CT Generator Module Model Subroutines
	// *****************************************************************************

	// Begin CT Generator Module Utility Subroutines
	// *****************************************************************************

	void
	InitCTGenerators(
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
		//       RE-ENGINEERED  Brent Griffith, Sept 2010 plant upgrades, generalize fluid props

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the CT generators.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_Generator_CTurbine;
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
			MyEnvrnFlag.allocate( NumCTGenerators );
			MyPlantScanFlag.allocate( NumCTGenerators );
			MySizeAndNodeInitFlag.allocate( NumCTGenerators );
			MyEnvrnFlag = true;
			MyPlantScanFlag = true;
			MyOneTimeFlag = false;
			MySizeAndNodeInitFlag = true;
		}

		if ( MyPlantScanFlag( GeneratorNum ) && allocated( PlantLoop ) && CTGenerator( GeneratorNum ).HeatRecActive ) {
			errFlag = false;
			ScanPlantLoopsForObject( CTGenerator( GeneratorNum ).Name, TypeOf_Generator_CTurbine, CTGenerator( GeneratorNum ).HRLoopNum, CTGenerator( GeneratorNum ).HRLoopSideNum, CTGenerator( GeneratorNum ).HRBranchNum, CTGenerator( GeneratorNum ).HRCompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitCTGenerators: Program terminated due to previous condition(s)." );
			}
			MyPlantScanFlag( GeneratorNum ) = false;
		}

		if ( MySizeAndNodeInitFlag( GeneratorNum ) && ( ! MyPlantScanFlag( GeneratorNum ) ) && CTGenerator( GeneratorNum ).HeatRecActive ) {
			HeatRecInletNode = CTGenerator( GeneratorNum ).HeatRecInletNodeNum;
			HeatRecOutletNode = CTGenerator( GeneratorNum ).HeatRecOutletNodeNum;

			//size mass flow rate
			rho = GetDensityGlycol( PlantLoop( CTGenerator( GeneratorNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( CTGenerator( GeneratorNum ).HRLoopNum ).FluidIndex, RoutineName );

			CTGenerator( GeneratorNum ).DesignHeatRecMassFlowRate = rho * CTGenerator( GeneratorNum ).DesignHeatRecVolFlowRate;

			InitComponentNodes( 0.0, CTGenerator( GeneratorNum ).DesignHeatRecMassFlowRate, HeatRecInletNode, HeatRecOutletNode, CTGenerator( GeneratorNum ).HRLoopNum, CTGenerator( GeneratorNum ).HRLoopSideNum, CTGenerator( GeneratorNum ).HRBranchNum, CTGenerator( GeneratorNum ).HRCompNum );

			MySizeAndNodeInitFlag( GeneratorNum ) = false;
		} // end one time inits

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( GeneratorNum ) && CTGenerator( GeneratorNum ).HeatRecActive ) {
			HeatRecInletNode = CTGenerator( GeneratorNum ).HeatRecInletNodeNum;
			HeatRecOutletNode = CTGenerator( GeneratorNum ).HeatRecOutletNodeNum;
			// set the node Temperature, assuming freeze control
			Node( HeatRecInletNode ).Temp = 20.0;
			Node( HeatRecOutletNode ).Temp = 20.0;
			// set the node max and min mass flow rates
			InitComponentNodes( 0.0, CTGenerator( GeneratorNum ).DesignHeatRecMassFlowRate, HeatRecInletNode, HeatRecOutletNode, CTGenerator( GeneratorNum ).HRLoopNum, CTGenerator( GeneratorNum ).HRLoopSideNum, CTGenerator( GeneratorNum ).HRBranchNum, CTGenerator( GeneratorNum ).HRCompNum );

			MyEnvrnFlag( GeneratorNum ) = false;
		} // end environmental inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( GeneratorNum ) = true;
		}

		if ( CTGenerator( GeneratorNum ).HeatRecActive ) {
			if ( FirstHVACIteration ) {
				if ( RunFlag ) {
					mdot = CTGenerator( GeneratorNum ).DesignHeatRecMassFlowRate;
				} else {
					mdot = 0.0;
				}
				SetComponentFlowRate( mdot, CTGenerator( GeneratorNum ).HeatRecInletNodeNum, CTGenerator( GeneratorNum ).HeatRecOutletNodeNum, CTGenerator( GeneratorNum ).HRLoopNum, CTGenerator( GeneratorNum ).HRLoopSideNum, CTGenerator( GeneratorNum ).HRBranchNum, CTGenerator( GeneratorNum ).HRCompNum );

			} else {
				SetComponentFlowRate( CTGenerator( GeneratorNum ).HeatRecMdot, CTGenerator( GeneratorNum ).HeatRecInletNodeNum, CTGenerator( GeneratorNum ).HeatRecOutletNodeNum, CTGenerator( GeneratorNum ).HRLoopNum, CTGenerator( GeneratorNum ).HRLoopSideNum, CTGenerator( GeneratorNum ).HRBranchNum, CTGenerator( GeneratorNum ).HRCompNum );
			}
		}

	}

	// End CT Generator Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the CT Generator Module
	// *****************************************************************************

	void
	UpdateCTGeneratorRecords(
		bool const EP_UNUSED( RunFlag ), // TRUE if Generator operating
		int const Num // Generator number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998

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
		int HeatRecInletNode;
		int HeatRecOutletNode;

		if ( CTGenerator( Num ).HeatRecActive ) {
			HeatRecInletNode = CTGenerator( Num ).HeatRecInletNodeNum;
			HeatRecOutletNode = CTGenerator( Num ).HeatRecOutletNodeNum;

			//    Node(HeatRecOutletNode)%MassFlowRate = CTGenerator(Num)%HeatRecMdot
			Node( HeatRecOutletNode ).Temp = CTGenerator( Num ).HeatRecOutletTemp;
			//    Node(HeatRecOutletNode)%MassFlowRateMaxAvail = Node(HeatRecInletNode)%MassFlowRateMaxAvail
			//    Node(HeatRecOutletNode)%MassFlowRateMinAvail = Node(HeatRecInletNode)%MassFlowRateMinAvail

		}
		CTGeneratorReport( Num ).PowerGen = CTGenerator( Num ).ElecPowerGenerated;
		CTGeneratorReport( Num ).EnergyGen = CTGenerator( Num ).ElecEnergyGenerated;
		CTGeneratorReport( Num ).QExhaustRecovered = CTGenerator( Num ).QExhaustRecovered;
		CTGeneratorReport( Num ).QLubeOilRecovered = CTGenerator( Num ).QLubeOilRecovered;
		CTGeneratorReport( Num ).ExhaustEnergyRec = CTGenerator( Num ).ExhaustEnergyRec;
		CTGeneratorReport( Num ).LubeOilEnergyRec = CTGenerator( Num ).LubeOilEnergyRec;
		CTGeneratorReport( Num ).QTotalHeatRecovered = CTGenerator( Num ).QTotalHeatRecovered;
		CTGeneratorReport( Num ).TotalHeatEnergyRec = CTGenerator( Num ).TotalHeatEnergyRec;
		CTGeneratorReport( Num ).FuelEnergyUseRate = CTGenerator( Num ).FuelEnergyUseRate;
		CTGeneratorReport( Num ).FuelEnergy = CTGenerator( Num ).FuelEnergy;
		CTGeneratorReport( Num ).FuelMdot = CTGenerator( Num ).FuelMdot;
		CTGeneratorReport( Num ).ExhaustStackTemp = CTGenerator( Num ).ExhaustStackTemp;
		CTGeneratorReport( Num ).HeatRecInletTemp = CTGenerator( Num ).HeatRecInletTemp;
		CTGeneratorReport( Num ).HeatRecOutletTemp = CTGenerator( Num ).HeatRecOutletTemp;
		CTGeneratorReport( Num ).HeatRecMdot = CTGenerator( Num ).HeatRecMdot;

	}

	void
	GetCTGeneratorResults(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get some results for load center's aggregation

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
		GeneratorPower = CTGeneratorReport( GeneratorIndex ).PowerGen;
		GeneratorEnergy = CTGeneratorReport( GeneratorIndex ).EnergyGen;
		ThermalPower = CTGeneratorReport( GeneratorIndex ).QTotalHeatRecovered;
		ThermalEnergy = CTGeneratorReport( GeneratorIndex ).TotalHeatEnergyRec;

	}

	// End of Record Keeping subroutines for the CT Generator Module
	// *****************************************************************************

} // CTElectricGenerator

} // EnergyPlus
