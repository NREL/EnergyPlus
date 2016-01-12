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
#include <MicroturbineElectricGenerator.hh>
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
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace MicroturbineElectricGenerator {

	//__________________________________________________________________________
	// New microturbine model added by FSEC:
	//   MicroturbineElectricGenerator  ! Microturbine Electric Generator Module

	// MODULE INFORMATION:
	//       AUTHOR         R. Raustad/D. Shirey
	//       DATE WRITTEN   Mar 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	//  This module simulates the performance of microturbine electric
	//  generators.

	// METHODOLOGY EMPLOYED:
	//  Once the electric power manager determines that the MT Generator
	//  is available, it calls SimMTGenerator which in turn calls the
	//  appropriate microturbine generator model.
	//  MT Generator models are based on polynomial curve fits of generator
	//  performance data.

	// REFERENCES: na

	// OTHER NOTES: na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::SecInHour;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobalConstants::iGeneratorMicroturbine;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumMTGenerators( 0 ); // number of MT Generators specified in input
	bool GetMTInput( true ); // then TRUE, calls subroutine to read input file.

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE MicroturbineElectricGenerator

	// Object Data
	Array1D< MTGeneratorSpecs > MTGenerator; // dimension to number of generators
	Array1D< ReportVars > MTGeneratorReport;

	// MODULE SUBROUTINES:
	// Beginning of MT Generator Module Driver Subroutine
	//*************************************************************************

	// Functions

	void
	SimMTGenerator(
		int const EP_UNUSED( GeneratorType ), // Type of generator !unused1208
		std::string const & GeneratorName, // User-specified name of generator
		int & GeneratorIndex, // Index to microturbine generator
		bool const RunFlag, // Simulate generator when TRUE
		Real64 const MyLoad, // Generator demand (W)
		bool const FirstHVACIteration // Simulation flag for First HVAC (system) iteration
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad/D. Shirey
		//       DATE WRITTEN   Mar 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the MT Generator driver subroutine. It gets the input
		//                             for the model, initializes simulation variables, calls
		//                             the appropriate model and updates reporting variables.

		// METHODOLOGY EMPLOYED:       Uses empirical models based on manufacturers data

		// REFERENCES:
		//  na

		// USE STATEMENTS:

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
		int GenNum; // Generator number counter

		// Get Generator data from input file
		if ( GetMTInput ) {
			GetMTGeneratorInput();
			GetMTInput = false;
		}

		// SELECT and CALL GENERATOR MODEL
		if ( GeneratorIndex == 0 ) {
			GenNum = FindItemInList( GeneratorName, MTGenerator );
			if ( GenNum == 0 ) ShowFatalError( "SimMTGenerator: Specified Generator not a valid COMBUSTION Turbine Generator " + GeneratorName );
			GeneratorIndex = GenNum;
		} else {
			GenNum = GeneratorIndex;
			if ( GenNum > NumMTGenerators || GenNum < 1 ) {
				ShowFatalError( "SimMTGenerator: Invalid GeneratorIndex passed = " + TrimSigDigits( GenNum ) + ", Number of CT Engine Generators = " + TrimSigDigits( NumMTGenerators ) + ", Generator name = " + GeneratorName );
			}

			if ( CheckEquipName( GenNum ) ) {
				if ( GeneratorName != MTGenerator( GenNum ).Name ) {
					ShowFatalError( "SimMTGenerator: Invalid GeneratorIndex passed = " + TrimSigDigits( GenNum ) + ", Generator name = " + GeneratorName + ", stored Generator Name for that index = " + MTGenerator( GenNum ).Name );
				}
				CheckEquipName( GenNum ) = false;
			}
		}

		InitMTGenerators( GenNum, RunFlag, MyLoad, FirstHVACIteration );
		CalcMTGeneratorModel( GenNum, RunFlag, MyLoad, FirstHVACIteration );
		UpdateMTGeneratorRecords( GenNum );

	}

	void
	SimMTPlantHeatRecovery(
		std::string const & EP_UNUSED( CompType ), // unused1208
		std::string const & CompName,
		int const EP_UNUSED( CompTypeNum ), // unused1208
		int & CompNum,
		bool const EP_UNUSED( RunFlag ), // unused1208
		bool & InitLoopEquip,
		Real64 & EP_UNUSED( MyLoad ), // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const EP_UNUSED( FirstHVACIteration ) // TRUE if First iteration of simulation !unused1208
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
		// INTEGER, INTENT(IN)          :: FlowLock !unused1208 !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetMTInput ) {
			GetMTGeneratorInput();
			GetMTInput = false;
		}

		if ( InitLoopEquip ) {
			CompNum = FindItemInList( CompName, MTGenerator );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimMTPlantHeatRecovery: Microturbine Generator Unit not found=" + CompName );
				return;
			}
			MinCap = MTGenerator( CompNum ).MinThermalPowerOutput;
			MaxCap = MTGenerator( CompNum ).MaxThermalPowerOutput;
			OptCap = MTGenerator( CompNum ).RefThermalPowerOutput;
			return;
		} // End Of InitLoopEquip

	}

	// End MT Generator Module Driver Subroutine
	//******************************************************************************

	// Beginning of Microturbine (MT) Generator Module Get Input Subroutine
	//******************************************************************************

	void
	GetMTGeneratorInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad/D. Shirey
		//       DATE WRITTEN   Mar 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine gets the input information for the Microturbine (MT) Generator model.

		// METHODOLOGY EMPLOYED:
		//  EnergyPlus input processor.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using BranchNodeConnections::TestCompSet;
		using CurveManager::GetCurveIndex;
		using CurveManager::CurveValue;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveMinMaxValues;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using ScheduleManager::GetScheduleIndex;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// PARAMETERS:
		//  na

		// LOCAL VARIABLES:
		int GeneratorNum; // Index to generator
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false ); // Error flag... trips fatal error message at end of get input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		Real64 ElectOutFTempElevOutput; // Output of Electrical Power Output Modifier Curve (function of temp and elev)
		Real64 ElecEfficFTempOutput; // Output of Electrical Efficiency Modifier Curve (function of temp)
		Real64 ElecEfficFPLROutput; // Output of Electrical Efficiency Modifier Curve (function of PLR)
		Real64 AncillaryPowerOutput; // Output of Ancillary Power Modifer Curve (function of temps and fuel flow)
		Real64 RefFuelUseMdot; // Fuel mass flow rate at reference conditions (kg/s)
		Real64 RefBaroPressure; // Reference barometric pressure, adjusted for reference elevation (Pa)
		Real64 ThermalEffTempElevOutput; // Output of Thermal Efficiency Modifier Curve (function of temp and elevation)
		Real64 HeatRecRateFPLROutput; // Output of Heat Recovery Rate Modifier Curve (function of PLR)
		Real64 HeatRecRateFTempOutput; // Output of Heat Recovery Rate Modifier Curve (function of inlet water temp)
		Real64 HeatRecRateFFlowOutput; // Output of Heat Recovery Rate Modifier Curve (function of water flow rate)
		Real64 ExhFlowFTempOutput; // Output of Exhaust Air Flow Modifier Curve (function of inlet air temp)
		Real64 ExhFlowFPLROutput; // Output of Exhaust Air Flow Modifier Curve (function of PLR)
		Real64 ExhAirTempFTempOutput; // Output of Exhaust Air Temperature Modifier Curve (function of inlet air temp)
		Real64 ExhOutAirTempFPLROutput; // Output of Exhaust Air Temperature Modifier Curve (function of PLR)
		static Real64 Var1Min( 0.0 ); // Minimum value for variable 1, value obtained from a curve object
		static Real64 Var1Max( 0.0 ); // Maximum value for variable 1, value obtained from a curve object

		Array1D< Real64 > NumArray( 19 ); // Numeric data array

		Array1D_string AlphArray( 20 ); // Character string data array
		std::string FuelType; // Type of fuel used for generator

		// FLOW:
		cCurrentModuleObject = "Generator:MicroTurbine";
		NumMTGenerators = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumMTGenerators <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}

		// ALLOCATE ARRAYS
		MTGenerator.allocate( NumMTGenerators );
		MTGeneratorReport.allocate( NumMTGenerators );
		CheckEquipName.dimension( NumMTGenerators, true );

		// LOAD ARRAYS WITH MICROTURBINE GENERATOR DATA
		for ( GeneratorNum = 1; GeneratorNum <= NumMTGenerators; ++GeneratorNum ) {
			GetObjectItem( cCurrentModuleObject, GeneratorNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), MTGenerator, GeneratorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			MTGenerator( GeneratorNum ).Name = AlphArray( 1 );

			MTGenerator( GeneratorNum ).RefElecPowerOutput = NumArray( 1 );
			if ( MTGenerator( GeneratorNum ).RefElecPowerOutput <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( NumArray( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be greater than 0." );
				ErrorsFound = true;
			}

			MTGenerator( GeneratorNum ).MinElecPowerOutput = NumArray( 2 );
			MTGenerator( GeneratorNum ).MaxElecPowerOutput = NumArray( 3 );

			if ( MTGenerator( GeneratorNum ).MinElecPowerOutput < 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( NumArray( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be greater than 0." );
				ErrorsFound = true;
			}

			if ( lNumericFieldBlanks( 3 ) ) {
				MTGenerator( GeneratorNum ).MaxElecPowerOutput = MTGenerator( GeneratorNum ).RefElecPowerOutput;
			} else {
				if ( MTGenerator( GeneratorNum ).MaxElecPowerOutput <= 0.0 ) {
					ShowSevereError( "Invalid " + cNumericFieldNames( 3 ) + '=' + RoundSigDigits( NumArray( 3 ), 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( cNumericFieldNames( 3 ) + " must be greater than 0." );
					ErrorsFound = true;
				}
			}

			if ( MTGenerator( GeneratorNum ).MinElecPowerOutput >= MTGenerator( GeneratorNum ).MaxElecPowerOutput ) {
				ShowSevereError( cCurrentModuleObject + "= " + MTGenerator( GeneratorNum ).Name );
				ShowContinueError( cNumericFieldNames( 2 ) + " [" + RoundSigDigits( NumArray( 2 ), 2 ) + "] > " + cNumericFieldNames( 3 ) + " [" + RoundSigDigits( NumArray( 3 ), 2 ) + ']' );
				ShowContinueError( "Minimum Full Load Electrical Power Output must be less than or equal" );
				ShowContinueError( "to Maximum Full Load Electrical Power Output." );
				ErrorsFound = true;
			}

			if ( MTGenerator( GeneratorNum ).RefElecPowerOutput > MTGenerator( GeneratorNum ).MaxElecPowerOutput || MTGenerator( GeneratorNum ).RefElecPowerOutput < MTGenerator( GeneratorNum ).MinElecPowerOutput ) {
				ShowSevereError( cCurrentModuleObject + "= " + MTGenerator( GeneratorNum ).Name );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be >= " + cNumericFieldNames( 2 ) );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be <= " + cNumericFieldNames( 3 ) );
				ShowContinueError( cNumericFieldNames( 1 ) + " = " + RoundSigDigits( NumArray( 1 ), 2 ) );
				ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( NumArray( 2 ), 2 ) );
				ShowContinueError( cNumericFieldNames( 3 ) + " = " + RoundSigDigits( NumArray( 3 ), 2 ) );
				ErrorsFound = true;
			}

			MTGenerator( GeneratorNum ).RefElecEfficiencyLHV = NumArray( 4 );

			if ( MTGenerator( GeneratorNum ).RefElecEfficiencyLHV <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + RoundSigDigits( NumArray( 4 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 4 ) + " must be greater than 0." );
				ErrorsFound = true;
			}

			MTGenerator( GeneratorNum ).RefCombustAirInletTemp = NumArray( 5 );
			MTGenerator( GeneratorNum ).RefCombustAirInletHumRat = NumArray( 6 );
			MTGenerator( GeneratorNum ).RefElevation = NumArray( 7 );

			if ( MTGenerator( GeneratorNum ).RefCombustAirInletHumRat <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 6 ) + '=' + RoundSigDigits( NumArray( 6 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 6 ) + " must be greater than 0." );
				ErrorsFound = true;
			} else {
				//      Barometric pressure adjusted for elevation
				RefBaroPressure = 101325.0 * std::pow( 1.0 - 2.25577e-05 * MTGenerator( GeneratorNum ).RefElevation, 5.2559 );
				MTGenerator( GeneratorNum ).RefCombustAirInletDensity = PsyRhoAirFnPbTdbW( RefBaroPressure, MTGenerator( GeneratorNum ).RefCombustAirInletTemp, MTGenerator( GeneratorNum ).RefCombustAirInletHumRat );
			}

			MTGenerator( GeneratorNum ).ElecPowFTempElevCurveNum = GetCurveIndex( AlphArray( 2 ) ); // Convert curve name to number
			if ( MTGenerator( GeneratorNum ).ElecPowFTempElevCurveNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + AlphArray( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			} else {
				// Verify curve object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ElecPowFTempElevCurveNum ) );

				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					// Check electrical power output at reference combustion inlet temp and elevation
					ElectOutFTempElevOutput = CurveValue( MTGenerator( GeneratorNum ).ElecPowFTempElevCurveNum, MTGenerator( GeneratorNum ).RefCombustAirInletTemp, MTGenerator( GeneratorNum ).RefElevation );
					if ( std::abs( ElectOutFTempElevOutput - 1.0 ) > 0.1 ) {
						ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
						ShowContinueError( "...Curve output at reference conditions should equal 1 (+-10%)." );
						ShowContinueError( "...Reference combustion air inlet temperature = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefCombustAirInletTemp, 4 ) + " C" );
						ShowContinueError( "...Reference elevation                        = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefElevation, 4 ) + " m" );
						ShowContinueError( "...Curve output                               = " + TrimSigDigits( ElectOutFTempElevOutput, 4 ) );
					}

				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "... illegal " + cAlphaFieldNames( 2 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ElecPowFTempElevCurveNum ) );
					ShowContinueError( "... Curve type must be BIQUADRATIC." ); //TODO rename point (curves)
					ErrorsFound = true;

				}}

			}

			MTGenerator( GeneratorNum ).ElecEffFTempCurveNum = GetCurveIndex( AlphArray( 3 ) ); // Convert curve name to number
			if ( MTGenerator( GeneratorNum ).ElecEffFTempCurveNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowSevereError( cAlphaFieldNames( 3 ) + " not found = " + AlphArray( 3 ) );
				ErrorsFound = true;
			} else {
				// Verify curve object, only legal types are Quadratic and Cubic
				{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ElecEffFTempCurveNum ) );

				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
					// Check electrical efficiency at reference combustion inlet temp
					ElecEfficFTempOutput = CurveValue( MTGenerator( GeneratorNum ).ElecEffFTempCurveNum, MTGenerator( GeneratorNum ).RefCombustAirInletTemp );
					if ( std::abs( ElecEfficFTempOutput - 1.0 ) > 0.1 ) {
						ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( cAlphaFieldNames( 3 ) + " = " + AlphArray( 3 ) );
						ShowContinueError( "... Curve output at reference condition should equal 1 (+-10%)." );
						ShowContinueError( "... Reference combustion air inlet temperature = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefCombustAirInletTemp, 4 ) + " C" );
						ShowContinueError( "... Curve output                               = " + TrimSigDigits( ElecEfficFTempOutput, 4 ) );
					}

				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "...illegal " + cAlphaFieldNames( 3 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ElecEffFTempCurveNum ) );
					ShowContinueError( "Curve type must be QUADRATIC or CUBIC." ); //TODO rename point (curves)
					ErrorsFound = true;

				}}

			}

			MTGenerator( GeneratorNum ).ElecEffFPLRCurveNum = GetCurveIndex( AlphArray( 4 ) ); // Convert curve name to number
			if ( MTGenerator( GeneratorNum ).ElecEffFPLRCurveNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowSevereError( cAlphaFieldNames( 4 ) + " not found = " + AlphArray( 4 ) );
				ErrorsFound = true;
			} else {
				// Verify curve object, only legal types are Quadratic and Cubic
				{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ElecEffFPLRCurveNum ) );

				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
					// Check electrical efficiency at PLR = 1
					ElecEfficFPLROutput = CurveValue( MTGenerator( GeneratorNum ).ElecEffFPLRCurveNum, 1.0 );
					if ( std::abs( ElecEfficFPLROutput - 1.0 ) > 0.1 ) {
						ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
						ShowContinueError( "... Curve output at a part-load ratio of 1 should equal 1 (+-10%)." );
						ShowContinueError( "... Curve output = " + TrimSigDigits( ElecEfficFPLROutput, 4 ) );
					}

					GetCurveMinMaxValues( MTGenerator( GeneratorNum ).ElecEffFPLRCurveNum, Var1Min, Var1Max );
					MTGenerator( GeneratorNum ).MinPartLoadRat = Var1Min;
					MTGenerator( GeneratorNum ).MaxPartLoadRat = Var1Max;

				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "...illegal " + cAlphaFieldNames( 4 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ElecEffFPLRCurveNum ) );
					ShowContinueError( "Curve type must be QUADRATIC or CUBIC." ); //TODO rename point (curves)
					ErrorsFound = true;

				}}

			}

			// Fuel Type case statement
			{ auto const SELECT_CASE_var( AlphArray( 5 ) );
			if ( is_blank( SELECT_CASE_var ) ) { // If blank, then the default is Natural Gas
				FuelType = "Gas";

			} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
				FuelType = "Gas";

				//    CASE ('DIESEL')
				//      FuelType = 'Diesel'

				//    CASE ('GASOLINE')
				//      FuelType = 'Gasoline'

				//    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
				//      FuelType = 'FuelOil#1'

				//    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
				//      FuelType = 'FuelOil#2'

			} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
				FuelType = "Propane";

				//    CASE ('OTHERFUEL1')
				//       FuelType = 'OtherFuel1'

				//    CASE ('OTHERFUEL2')
				//       FuelType = 'OtherFuel2'

			} else {
				ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + "  = " + AlphArray( 5 ) );
				ErrorsFound = true;
			}}

			MTGenerator( GeneratorNum ).FuelHigherHeatingValue = NumArray( 8 );
			MTGenerator( GeneratorNum ).FuelLowerHeatingValue = NumArray( 9 );

			if ( MTGenerator( GeneratorNum ).FuelLowerHeatingValue <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 9 ) + '=' + RoundSigDigits( NumArray( 9 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " must be greater than 0." );
				ErrorsFound = true;
			}

			if ( MTGenerator( GeneratorNum ).FuelHigherHeatingValue <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 8 ) + '=' + RoundSigDigits( NumArray( 8 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 8 ) + " must be greater than 0." );
				ErrorsFound = true;
			}

			if ( MTGenerator( GeneratorNum ).FuelLowerHeatingValue > MTGenerator( GeneratorNum ).FuelHigherHeatingValue ) {
				ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowContinueError( cNumericFieldNames( 8 ) + " must be greater than the " + cNumericFieldNames( 9 ) );
				ShowContinueError( cNumericFieldNames( 8 ) + '=' + RoundSigDigits( NumArray( 8 ), 2 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + '=' + RoundSigDigits( NumArray( 9 ), 2 ) );
				ErrorsFound = true;
			}

			MTGenerator( GeneratorNum ).StandbyPower = NumArray( 10 );
			if ( MTGenerator( GeneratorNum ).StandbyPower < 0.0 ) {
				ShowWarningError( "Invalid " + cNumericFieldNames( 10 ) + '=' + RoundSigDigits( NumArray( 10 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 10 ) + " must be greater than 0." );
				ShowContinueError( "Resetting to 0 and the simulation continues." );
				MTGenerator( GeneratorNum ).StandbyPower = 0.0;
			}

			MTGenerator( GeneratorNum ).AncillaryPower = NumArray( 11 );
			if ( MTGenerator( GeneratorNum ).AncillaryPower < 0.0 ) {
				ShowWarningError( "Invalid " + cNumericFieldNames( 11 ) + '=' + RoundSigDigits( NumArray( 11 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ShowContinueError( cNumericFieldNames( 11 ) + " must be greater than 0." );
				ShowContinueError( "Resetting to 0 and the simulation continues." );
				MTGenerator( GeneratorNum ).AncillaryPower = 0.0;
			}

			MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum = GetCurveIndex( AlphArray( 6 ) ); // Convert curve name to number
			//   If blank, then the calc routine assumes modifier curve value = 1 for entire simulation
			if ( ! lAlphaFieldBlanks( 6 ) && MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + AlphArray( 6 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			} else if ( MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum > 0 ) {
				// Verify curve object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

					if ( MTGenerator( GeneratorNum ).FuelLowerHeatingValue > 0.0 && MTGenerator( GeneratorNum ).RefElecEfficiencyLHV > 0.0 ) {

						RefFuelUseMdot = ( MTGenerator( GeneratorNum ).RefElecPowerOutput / MTGenerator( GeneratorNum ).RefElecEfficiencyLHV ) / ( MTGenerator( GeneratorNum ).FuelLowerHeatingValue * 1000.0 );
						AncillaryPowerOutput = CurveValue( MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum, RefFuelUseMdot );
						if ( std::abs( AncillaryPowerOutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 6 ) + " = " + AlphArray( 6 ) );
							ShowContinueError( "... Curve output at reference conditions should equal 1 (+-10%)." );
							ShowContinueError( "... Reference Electrical Power Output           = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefElecPowerOutput, 2 ) + " W" );
							ShowContinueError( "... Reference Electrical Efficiency (LHV basis) = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefElecEfficiencyLHV, 4 ) );
							ShowContinueError( "... Fuel Lower Heating Value                    = " + TrimSigDigits( MTGenerator( GeneratorNum ).FuelLowerHeatingValue, 2 ) + " kJ/kg" );
							ShowContinueError( "... Calculated fuel flow                        = " + TrimSigDigits( RefFuelUseMdot, 4 ) + " kg/s" );
							ShowContinueError( "... Curve output                                = " + TrimSigDigits( AncillaryPowerOutput, 4 ) );
						}
					}

				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "... illegal " + cAlphaFieldNames( 6 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum ) );
					ShowContinueError( "... Curve type must be QUADRATIC." );
					ErrorsFound = true;

				}}

			}

			if ( ! lAlphaFieldBlanks( 7 ) ) {
				MTGenerator( GeneratorNum ).HeatRecInletNodeNum = GetOnlySingleNode( AlphArray( 7 ), ErrorsFound, cCurrentModuleObject, MTGenerator( GeneratorNum ).Name, NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			}

			if ( ! lAlphaFieldBlanks( 8 ) ) {
				MTGenerator( GeneratorNum ).HeatRecOutletNodeNum = GetOnlySingleNode( AlphArray( 8 ), ErrorsFound, cCurrentModuleObject, MTGenerator( GeneratorNum ).Name, NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			}

			if ( MTGenerator( GeneratorNum ).HeatRecInletNodeNum > 0 && MTGenerator( GeneratorNum ).HeatRecOutletNodeNum > 0 ) {
				TestCompSet( cCurrentModuleObject, MTGenerator( GeneratorNum ).Name, AlphArray( 7 ), AlphArray( 8 ), "Heat Recovery Nodes" );
			}

			if ( ( MTGenerator( GeneratorNum ).HeatRecOutletNodeNum > 0 && MTGenerator( GeneratorNum ).HeatRecInletNodeNum == 0 ) || ( MTGenerator( GeneratorNum ).HeatRecOutletNodeNum == 0 && MTGenerator( GeneratorNum ).HeatRecInletNodeNum > 0 ) ) {
				ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowContinueError( "... If one Heat Recovery Water Node Name is specified, then both the Inlet and Outlet Heat Recovery" );
				ShowContinueError( "... Water Node Names must be specified. Only one water node is being specified for this generator." );
				ErrorsFound = true;
			}

			//   Heat recovery to water input fields only valid if water nodes are defined
			if ( MTGenerator( GeneratorNum ).HeatRecInletNodeNum != 0 && MTGenerator( GeneratorNum ).HeatRecOutletNodeNum != 0 ) {

				MTGenerator( GeneratorNum ).HeatRecActive = true;

				MTGenerator( GeneratorNum ).RefThermalEffLHV = NumArray( 12 );
				if ( MTGenerator( GeneratorNum ).RefThermalEffLHV < 0.0 ) {
					ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( cNumericFieldNames( 12 ) + " must be >= 0." );
					ShowContinueError( "Resetting to 0 and the simulation continues." );
					MTGenerator( GeneratorNum ).RefThermalEffLHV = 0.0;
				}

				// Next store thermal power output ranges using nominal thermal to electrical efficiency ratio and electrical power data
				MTGenerator( GeneratorNum ).RefThermalPowerOutput = MTGenerator( GeneratorNum ).RefElecPowerOutput * MTGenerator( GeneratorNum ).RefThermalEffLHV / MTGenerator( GeneratorNum ).RefElecEfficiencyLHV;
				MTGenerator( GeneratorNum ).MinThermalPowerOutput = MTGenerator( GeneratorNum ).MinElecPowerOutput * MTGenerator( GeneratorNum ).RefThermalEffLHV / MTGenerator( GeneratorNum ).RefElecEfficiencyLHV;
				MTGenerator( GeneratorNum ).MaxThermalPowerOutput = MTGenerator( GeneratorNum ).MaxElecPowerOutput * MTGenerator( GeneratorNum ).RefThermalEffLHV / MTGenerator( GeneratorNum ).RefElecEfficiencyLHV;

				MTGenerator( GeneratorNum ).RefInletWaterTemp = NumArray( 13 );

				if ( SameString( AlphArray( 9 ), "InternalControl" ) ) {
					MTGenerator( GeneratorNum ).InternalFlowControl = true; //  A9, \field Heat Recovery Water Flow Operating Mode
					MTGenerator( GeneratorNum ).PlantFlowControl = false;
				}
				if ( ( ! ( SameString( AlphArray( 9 ), "InternalControl" ) ) ) && ( ! ( SameString( AlphArray( 9 ), "PlantControl" ) ) ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + AlphArray( 9 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "Operating Mode must be INTERNAL CONTROL or PLANT CONTROL." );
					ErrorsFound = true;
				}

				MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate = NumArray( 14 );

				if ( MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate <= 0.0 ) {
					ShowSevereError( "Invalid " + cNumericFieldNames( 14 ) + '=' + RoundSigDigits( NumArray( 14 ), 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( cNumericFieldNames( 14 ) + " must be greater than 0." );
					ErrorsFound = true;
				}

				if ( MTGenerator( GeneratorNum ).InternalFlowControl ) { // Get Heat Recovery Water Flow Rate Modifier Curve

					MTGenerator( GeneratorNum ).HeatRecFlowFTempPowCurveNum = GetCurveIndex( AlphArray( 10 ) );
					if ( MTGenerator( GeneratorNum ).HeatRecFlowFTempPowCurveNum != 0 ) {
						// Verify curve object, only legal type is BiQuadratic
						{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).HeatRecFlowFTempPowCurveNum ) );

						if ( SELECT_CASE_var == "BIQUADRATIC" ) {
							//          NEED TO FIGURE OUT WHAT TO USE FOR Pnet............Shirey
							//    HeatRecFlowFTempPowCurveOutput = CurveValue(MTGenerator(GeneratorNum)%HeatRecFlowFTempPowCurveNum, Pnet)
							//    IF(ABS(HeatRecFlowFTempPowCurveOutput-1.0d0) .GT. 0.1d0)THEN !
							//      CALL ShowWarningError('GENERATOR:MICROTURBINE "'//TRIM(MTGenerator(GeneratorNum)%Name)//'"')
							//      CALL ShowContinueError('Heat Recovery Water Flow Rate Modifier Curve (function of temp and power) = '//TRIM(AlphArray(10)))
							//      CALL ShowContinueError('... Curve ouput at a reference conditions should equal 1 (+-10%).')
							//      CALL ShowContinueError('... Curve output = '//TRIM(TrimSigDigits(HeatRecFlowFTempPowCurveOutput,4)))
							//    END IF

						} else {
							ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( "... illegal " + cAlphaFieldNames( 10 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).HeatRecFlowFTempPowCurveNum ) );
							ShowContinueError( "Curve type must be BIQUADRATIC." );
							ErrorsFound = true;

						}}

					}

				} // End of IF (MTGenerator(GeneratorNum)%InternalFlowControl) THEN

				MTGenerator( GeneratorNum ).ThermEffFTempElevCurveNum = GetCurveIndex( AlphArray( 11 ) ); // convert curve name to number
				if ( MTGenerator( GeneratorNum ).ThermEffFTempElevCurveNum != 0 ) {
					// Verify curve object, only legal types are BiQuadratic and BiCubic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ThermEffFTempElevCurveNum ) );

					if ( ( SELECT_CASE_var == "BIQUADRATIC" ) || ( SELECT_CASE_var == "BICUBIC" ) ) {

						ThermalEffTempElevOutput = CurveValue( MTGenerator( GeneratorNum ).ThermEffFTempElevCurveNum, MTGenerator( GeneratorNum ).RefCombustAirInletTemp, MTGenerator( GeneratorNum ).RefElevation );

						if ( std::abs( ThermalEffTempElevOutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 11 ) + " = " + AlphArray( 11 ) );
							ShowContinueError( "... Curve output at reference conditions should equal 1 (+-10%)." );
							ShowContinueError( "... Reference combustion air inlet temperature      = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefCombustAirInletTemp, 4 ) + " C" );
							ShowContinueError( "... Reference elevation                             = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefElevation, 4 ) + " m" );
							ShowContinueError( "... Curve output                                    = " + TrimSigDigits( ThermalEffTempElevOutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 11 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ThermEffFTempElevCurveNum ) );
						ShowContinueError( "Curve type must be BIQUADRATIC or BICUBIC." );
						ErrorsFound = true;

					}}

				}

				MTGenerator( GeneratorNum ).HeatRecRateFPLRCurveNum = GetCurveIndex( AlphArray( 12 ) ); // convert curve name to number
				if ( MTGenerator( GeneratorNum ).HeatRecRateFPLRCurveNum != 0 ) {
					// Verify curve object, only legal types are Quadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).HeatRecRateFPLRCurveNum ) );

					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

						HeatRecRateFPLROutput = CurveValue( MTGenerator( GeneratorNum ).HeatRecRateFPLRCurveNum, 1.0 );

						if ( std::abs( HeatRecRateFPLROutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 12 ) + " = " + AlphArray( 12 ) );
							ShowContinueError( "... Curve output at a part-load ratio of 1 should equal 1 (+-10%)." );
							ShowContinueError( "... Curve output = " + TrimSigDigits( HeatRecRateFPLROutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 12 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).HeatRecRateFPLRCurveNum ) );
						ShowContinueError( "... Curve type must be QUADRATIC or CUBIC." );
						ErrorsFound = true;

					}}

				}

				MTGenerator( GeneratorNum ).HeatRecRateFTempCurveNum = GetCurveIndex( AlphArray( 13 ) ); // convert curve name to number
				if ( MTGenerator( GeneratorNum ).HeatRecRateFTempCurveNum != 0 ) {
					// Verify curve object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).HeatRecRateFTempCurveNum ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {

						HeatRecRateFTempOutput = CurveValue( MTGenerator( GeneratorNum ).HeatRecRateFTempCurveNum, MTGenerator( GeneratorNum ).RefInletWaterTemp );

						if ( std::abs( HeatRecRateFTempOutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 13 ) + " = " + AlphArray( 13 ) );
							ShowContinueError( "... Curve output at reference condition should equal 1 (+-10%)." );
							ShowContinueError( "... Reference inlet water temperature temperature      = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefInletWaterTemp, 4 ) + " C" );
							ShowContinueError( "... Curve output = " + TrimSigDigits( HeatRecRateFTempOutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 13 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).HeatRecRateFTempCurveNum ) );
						ShowContinueError( "... Curve type must be QUADRATIC." );
						ErrorsFound = true;

					}}

				}

				MTGenerator( GeneratorNum ).HeatRecRateFWaterFlowCurveNum = GetCurveIndex( AlphArray( 14 ) );
				if ( MTGenerator( GeneratorNum ).HeatRecRateFWaterFlowCurveNum != 0 ) {
					// Verify curve object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).HeatRecRateFWaterFlowCurveNum ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {

						HeatRecRateFFlowOutput = CurveValue( MTGenerator( GeneratorNum ).HeatRecRateFWaterFlowCurveNum, MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate );

						if ( std::abs( HeatRecRateFFlowOutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 14 ) + " = " + AlphArray( 14 ) );
							ShowContinueError( "... Curve output at reference condition should equal 1 (+-10%)." );
							ShowContinueError( "... Reference Heat Recovery Water Flow Rate      = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate, 4 ) + " m3/s" );
							ShowContinueError( "... Curve output = " + TrimSigDigits( HeatRecRateFFlowOutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 14 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).HeatRecRateFWaterFlowCurveNum ) );
						ShowContinueError( "... Curve type must be QUADRATIC." );
						ErrorsFound = true;

					}}

				}

				MTGenerator( GeneratorNum ).HeatRecMinVolFlowRate = NumArray( 15 );
				if ( MTGenerator( GeneratorNum ).HeatRecMinVolFlowRate < 0.0 ) {
					ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( cNumericFieldNames( 15 ) + " must be >= 0." );
					ShowContinueError( "Resetting to 0 and the simulation continues." );
					MTGenerator( GeneratorNum ).HeatRecMinVolFlowRate = 0.0;
				}

				MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate = NumArray( 16 );
				if ( MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate < 0.0 ) {
					ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( cNumericFieldNames( 16 ) + " must be >= 0." );
					ShowContinueError( "Resetting to 0 and the simulation continues." );
					MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate = 0.0;
				}

				if ( MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate < MTGenerator( GeneratorNum ).HeatRecMinVolFlowRate ) {
					ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( cNumericFieldNames( 16 ) + " must be >= " + cNumericFieldNames( 15 ) );
					ShowContinueError( "Resetting " + cNumericFieldNames( 16 ) + " = " + cNumericFieldNames( 15 ) + " and the simulation continues." );
					MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate = MTGenerator( GeneratorNum ).HeatRecMinVolFlowRate;
				}

				//     Check if reference heat recovery water flow rate is below the minimum flow rate
				if ( MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate < MTGenerator( GeneratorNum ).HeatRecMinVolFlowRate ) {
					ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( cNumericFieldNames( 14 ) + " must be >= " + cNumericFieldNames( 15 ) );
					ShowContinueError( "Resetting " + cNumericFieldNames( 14 ) + " = " + cNumericFieldNames( 15 ) + " and the simulation continues." );
					MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate = MTGenerator( GeneratorNum ).HeatRecMinVolFlowRate;
				}

				//     Check if reference heat recovery water flow rate is above the maximum flow rate
				if ( MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate > MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate ) {
					ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( cNumericFieldNames( 14 ) + " must be <= " + cNumericFieldNames( 16 ) );
					ShowContinueError( "Resetting " + cNumericFieldNames( 14 ) + " = " + cNumericFieldNames( 16 ) + " and the simulation continues." );
					MTGenerator( GeneratorNum ).RefHeatRecVolFlowRate = MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate;
				}

				RegisterPlantCompDesignFlow( MTGenerator( GeneratorNum ).HeatRecInletNodeNum, MTGenerator( GeneratorNum ).HeatRecMaxVolFlowRate );

				MTGenerator( GeneratorNum ).HeatRecMaxWaterTemp = NumArray( 17 );

			} // End of 'IF (MTGenerator(GeneratorNum)%HeatRecInletNodeNum .NE. 0 .AND. &
			//             MTGenerator(GeneratorNum)%HeatRecOutletNodeNum .NE. 0) THEN'

			if ( ! lAlphaFieldBlanks( 15 ) ) {
				MTGenerator( GeneratorNum ).CombustionAirInletNodeNum = GetOnlySingleNode( AlphArray( 15 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			}

			//    Combustion air inlet node must be an outside air node
			if ( ! lAlphaFieldBlanks( 15 ) && ! CheckOutAirNodeNumber( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum ) ) {
				ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowContinueError( cAlphaFieldNames( 15 ) + " is not a valid Outdoor Air Node = " + AlphArray( 15 ) );
				ShowContinueError( "it does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
				ErrorsFound = true;
			}

			if ( ! lAlphaFieldBlanks( 16 ) ) {
				MTGenerator( GeneratorNum ).CombustionAirOutletNodeNum = GetOnlySingleNode( AlphArray( 16 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			}

			if ( MTGenerator( GeneratorNum ).CombustionAirOutletNodeNum > 0 && MTGenerator( GeneratorNum ).CombustionAirInletNodeNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowContinueError( "A " + cAlphaFieldNames( 15 ) + " must be specified when a " + cAlphaFieldNames( 16 ) + " is specified." );
				ErrorsFound = true;
			}

			//   Get other exhaust air inputs only if combustion air inlet and outlet nodes are valid
			if ( MTGenerator( GeneratorNum ).CombustionAirOutletNodeNum > 0 && MTGenerator( GeneratorNum ).CombustionAirInletNodeNum > 0 ) {

				MTGenerator( GeneratorNum ).ExhAirCalcsActive = true;
				MTGenerator( GeneratorNum ).RefExhaustAirMassFlowRate = NumArray( 18 );
				if ( MTGenerator( GeneratorNum ).RefExhaustAirMassFlowRate <= 0.0 && ! lNumericFieldBlanks( 18 ) ) {
					ShowSevereError( "Invalid " + cNumericFieldNames( 18 ) + '=' + RoundSigDigits( NumArray( 18 ), 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( cNumericFieldNames( 18 ) + " must be greater than 0." );
					ErrorsFound = true;
				}

				MTGenerator( GeneratorNum ).ExhFlowFTempCurveNum = GetCurveIndex( AlphArray( 17 ) );
				if ( MTGenerator( GeneratorNum ).ExhFlowFTempCurveNum != 0 ) {
					// Verify curve object, only legal types are Quadratic and Cubic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ExhFlowFTempCurveNum ) );

					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

						ExhFlowFTempOutput = CurveValue( MTGenerator( GeneratorNum ).ExhFlowFTempCurveNum, MTGenerator( GeneratorNum ).RefCombustAirInletTemp );

						if ( std::abs( ExhFlowFTempOutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 17 ) + " = " + AlphArray( 17 ) );
							ShowContinueError( "... Curve output at reference condition should equal 1 (+-10%)." );
							ShowContinueError( "... Reference combustion air inlet temperature      = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefCombustAirInletTemp, 4 ) + " C" );
							ShowContinueError( "... Curve output = " + TrimSigDigits( ExhFlowFTempOutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 17 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ExhFlowFTempCurveNum ) );
						ShowContinueError( "... Curve type must be QUADRATIC or CUBIC." );
						ErrorsFound = true;

					}}

				}

				MTGenerator( GeneratorNum ).ExhFlowFPLRCurveNum = GetCurveIndex( AlphArray( 18 ) ); // convert curve name to number
				if ( MTGenerator( GeneratorNum ).ExhFlowFPLRCurveNum != 0 ) {
					// Verify curve object, legal types are Quadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ExhFlowFPLRCurveNum ) );

					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

						ExhFlowFPLROutput = CurveValue( MTGenerator( GeneratorNum ).ExhFlowFPLRCurveNum, 1.0 );

						if ( std::abs( ExhFlowFPLROutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 18 ) + " = " + AlphArray( 18 ) );
							ShowContinueError( "... Curve output at a part-load ratio of 1 should equal 1 (+-10%)." );
							ShowContinueError( "... Curve output = " + TrimSigDigits( ExhFlowFPLROutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 18 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ExhFlowFPLRCurveNum ) );
						ShowContinueError( "... Curve type must be QUADRATIC or CUBIC." );
						ErrorsFound = true;

					}}

				}

				MTGenerator( GeneratorNum ).NomExhAirOutletTemp = NumArray( 19 );

				MTGenerator( GeneratorNum ).ExhAirTempFTempCurveNum = GetCurveIndex( AlphArray( 19 ) );
				if ( MTGenerator( GeneratorNum ).ExhAirTempFTempCurveNum != 0 ) {
					// Verify curve object, only legal types are Quadratic and Cubic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ExhAirTempFTempCurveNum ) );

					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

						ExhAirTempFTempOutput = CurveValue( MTGenerator( GeneratorNum ).ExhAirTempFTempCurveNum, MTGenerator( GeneratorNum ).RefCombustAirInletTemp );

						if ( std::abs( ExhAirTempFTempOutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 19 ) + " = " + AlphArray( 19 ) );
							ShowContinueError( "... Curve output at reference condition should equal 1 (+-10%)." );
							ShowContinueError( "... Reference combustion air inlet temperature      = " + TrimSigDigits( MTGenerator( GeneratorNum ).RefCombustAirInletTemp, 4 ) + " C" );
							ShowContinueError( "... Curve output = " + TrimSigDigits( ExhAirTempFTempOutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 19 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ExhAirTempFTempCurveNum ) );
						ShowContinueError( "... Curve type must be QUADRATIC or CUBIC." );
						ErrorsFound = true;

					}}

				}

				MTGenerator( GeneratorNum ).ExhAirTempFPLRCurveNum = GetCurveIndex( AlphArray( 20 ) ); // convert curve name to number
				if ( MTGenerator( GeneratorNum ).ExhAirTempFPLRCurveNum != 0 ) {
					// Verify curve object, legal types are Quadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( MTGenerator( GeneratorNum ).ExhAirTempFPLRCurveNum ) );

					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

						ExhOutAirTempFPLROutput = CurveValue( MTGenerator( GeneratorNum ).ExhAirTempFPLRCurveNum, 1.0 );

						if ( std::abs( ExhOutAirTempFPLROutput - 1.0 ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
							ShowContinueError( cAlphaFieldNames( 20 ) + " = " + AlphArray( 20 ) );
							ShowContinueError( "... Curve output at a part-load ratio of 1 should equal 1 (+-10%)." );
							ShowContinueError( "... Curve output = " + TrimSigDigits( ExhOutAirTempFPLROutput, 4 ) );
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... illegal " + cAlphaFieldNames( 20 ) + " type for this object = " + GetCurveType( MTGenerator( GeneratorNum ).ExhAirTempFPLRCurveNum ) );
						ShowContinueError( "... Curve type must be QUADRATIC or CUBIC." );
						ErrorsFound = true;

					}}

				}

			} // End of '    IF (MTGenerator(GeneratorNum)%CombustionAirOutletNodeNum .GT. 0 .AND. &
			//                 MTGenerator(GeneratorNum)%CombustionAirInletNodeNum .GT. 0) THEN

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( GeneratorNum = 1; GeneratorNum <= NumMTGenerators; ++GeneratorNum ) {
			SetupOutputVariable( "Generator Produced Electric Power [W]", MTGeneratorReport( GeneratorNum ).PowerGen, "System", "Average", MTGenerator( GeneratorNum ).Name );

			SetupOutputVariable( "Generator Produced Electric Energy [J]", MTGeneratorReport( GeneratorNum ).EnergyGen, "System", "Sum", MTGenerator( GeneratorNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Generator LHV Basis Electric Efficiency []", MTGeneratorReport( GeneratorNum ).ElectricEfficiencyLHV, "System", "Average", MTGenerator( GeneratorNum ).Name );

			//    Fuel specific report variables
			SetupOutputVariable( "Generator " + FuelType + " HHV Basis Rate [W]", MTGeneratorReport( GeneratorNum ).FuelEnergyUseRateHHV, "System", "Average", MTGenerator( GeneratorNum ).Name );

			SetupOutputVariable( "Generator " + FuelType + " HHV Basis Energy [J]", MTGeneratorReport( GeneratorNum ).FuelEnergyHHV, "System", "Sum", MTGenerator( GeneratorNum ).Name, _, FuelType, "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Generator " + FuelType + " Mass Flow Rate [kg/s]", MTGeneratorReport( GeneratorNum ).FuelMdot, "System", "Average", MTGenerator( GeneratorNum ).Name );

			//    general fuel use report (to match other generators)
			SetupOutputVariable( "Generator Fuel HHV Basis Rate [W]", MTGeneratorReport( GeneratorNum ).FuelEnergyUseRateHHV, "System", "Average", MTGenerator( GeneratorNum ).Name );

			SetupOutputVariable( "Generator Fuel HHV Basis Energy [J]", MTGeneratorReport( GeneratorNum ).FuelEnergyHHV, "System", "Sum", MTGenerator( GeneratorNum ).Name );

			//    Heat recovery (to water) report variables
			if ( MTGenerator( GeneratorNum ).HeatRecActive ) {

				SetupOutputVariable( "Generator Produced Thermal Rate [W]", MTGeneratorReport( GeneratorNum ).QHeatRecovered, "System", "Average", MTGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Produced Thermal Energy [J]", MTGeneratorReport( GeneratorNum ).ExhaustEnergyRec, "System", "Sum", MTGenerator( GeneratorNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Generator Thermal Efficiency LHV Basis []", MTGeneratorReport( GeneratorNum ).ThermalEfficiencyLHV, "System", "Average", MTGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Heat Recovery Inlet Temperature [C]", MTGeneratorReport( GeneratorNum ).HeatRecInletTemp, "System", "Average", MTGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Heat Recovery Outlet Temperature [C]", MTGeneratorReport( GeneratorNum ).HeatRecOutletTemp, "System", "Average", MTGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Heat Recovery Water Mass Flow Rate [kg/s]", MTGeneratorReport( GeneratorNum ).HeatRecMdot, "System", "Average", MTGenerator( GeneratorNum ).Name );

			}

			if ( MTGenerator( GeneratorNum ).StandbyPower > 0.0 ) { // Report Standby Power if entered by user
				SetupOutputVariable( "Generator Standby Electric Power [W]", MTGeneratorReport( GeneratorNum ).StandbyPowerRate, "System", "Average", MTGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Standby Electric Energy [J]", MTGeneratorReport( GeneratorNum ).StandbyEnergy, "System", "Sum", MTGenerator( GeneratorNum ).Name, _, "Electricity", "Cogeneration", _, "Plant" );
			}

			if ( MTGenerator( GeneratorNum ).AncillaryPower > 0.0 ) { // Report Ancillary Power if entered by user
				SetupOutputVariable( "Generator Ancillary Electric Power [W]", MTGeneratorReport( GeneratorNum ).AncillaryPowerRate, "System", "Average", MTGenerator( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Ancillary Electric Energy [J]", MTGeneratorReport( GeneratorNum ).AncillaryEnergy, "System", "Sum", MTGenerator( GeneratorNum ).Name );
			}
			//   Report combustion air outlet conditions if exhaust air calculations are active
			if ( MTGenerator( GeneratorNum ).ExhAirCalcsActive ) {
				SetupOutputVariable( "Generator Exhaust Air Mass Flow Rate [kg/s]", MTGeneratorReport( GeneratorNum ).ExhAirMassFlowRate, "System", "Average", MTGenerator( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Exhaust Air Temperature  [C]", MTGeneratorReport( GeneratorNum ).ExhAirTemperature, "System", "Average", MTGenerator( GeneratorNum ).Name );
			}

		}

	}

	// End of Get Input subroutine for the MT Generator Module
	//******************************************************************************

	// Begin MT Generator Module Initialize Subroutine
	// *****************************************************************************

	void
	InitMTGenerators(
		int const GenNum,
		bool const RunFlag,
		Real64 const MyLoad, // electrical load in W
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad/D. Shirey
		//       DATE WRITTEN   Mar 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith, Sept 2010, plant upgrades, general fluid props

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine is for initializations of the CT generators.

		// METHODOLOGY EMPLOYED:
		//  Uses the status flags to trigger initializations.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using CurveManager::CurveValue;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_Generator_MicroTurbine;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitMTGenerators" );

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatRecInletNode; // Inlet node number in heat recovery loop
		int HeatRecOutletNode; // Outlet node number in heat recovery loop
		static Array1D_bool MyEnvrnFlag; // Flag for init once at start of environment
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MySizeAndNodeInitFlag;
		static bool MyOneTimeFlag( true ); // Initialization flag
		Real64 rho; // local temporary fluid density
		Real64 DesiredMassFlowRate;
		bool errFlag;

		// FLOW:
		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumMTGenerators );
			MyPlantScanFlag.allocate( NumMTGenerators );
			MySizeAndNodeInitFlag.allocate( NumMTGenerators );
			MyEnvrnFlag = true;
			MyPlantScanFlag = true;
			MySizeAndNodeInitFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyPlantScanFlag( GenNum ) && allocated( PlantLoop ) && MTGenerator( GenNum ).HeatRecActive ) {
			errFlag = false;
			ScanPlantLoopsForObject( MTGenerator( GenNum ).Name, TypeOf_Generator_MicroTurbine, MTGenerator( GenNum ).HRLoopNum, MTGenerator( GenNum ).HRLoopSideNum, MTGenerator( GenNum ).HRBranchNum, MTGenerator( GenNum ).HRCompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitMTGenerators: Program terminated due to previous condition(s)." );
			}

			MyPlantScanFlag( GenNum ) = false;
		}

		if ( MySizeAndNodeInitFlag( GenNum ) && ( ! MyPlantScanFlag( GenNum ) ) && MTGenerator( GenNum ).HeatRecActive ) {

			HeatRecInletNode = MTGenerator( GenNum ).HeatRecInletNodeNum;
			HeatRecOutletNode = MTGenerator( GenNum ).HeatRecOutletNodeNum;

			//size mass flow rate
			rho = GetDensityGlycol( PlantLoop( MTGenerator( GenNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( MTGenerator( GenNum ).HRLoopNum ).FluidIndex, RoutineName );

			MTGenerator( GenNum ).DesignHeatRecMassFlowRate = rho * MTGenerator( GenNum ).RefHeatRecVolFlowRate;
			MTGenerator( GenNum ).HeatRecMaxMassFlowRate = rho * MTGenerator( GenNum ).HeatRecMaxVolFlowRate;

			InitComponentNodes( 0.0, MTGenerator( GenNum ).HeatRecMaxMassFlowRate, HeatRecInletNode, HeatRecOutletNode, MTGenerator( GenNum ).HRLoopNum, MTGenerator( GenNum ).HRLoopSideNum, MTGenerator( GenNum ).HRBranchNum, MTGenerator( GenNum ).HRCompNum );

			MySizeAndNodeInitFlag( GenNum ) = false;

		} // end one time inits

		if ( ! MTGenerator( GenNum ).HeatRecActive ) return;

		HeatRecInletNode = MTGenerator( GenNum ).HeatRecInletNodeNum;
		HeatRecOutletNode = MTGenerator( GenNum ).HeatRecOutletNodeNum;
		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( GenNum ) ) {
			// set the node max and min mass flow rates
			InitComponentNodes( 0.0, MTGenerator( GenNum ).HeatRecMaxMassFlowRate, HeatRecInletNode, HeatRecOutletNode, MTGenerator( GenNum ).HRLoopNum, MTGenerator( GenNum ).HRLoopSideNum, MTGenerator( GenNum ).HRBranchNum, MTGenerator( GenNum ).HRCompNum );

			Node( HeatRecInletNode ).Temp = 20.0; // Set the node temperature, assuming freeze control
			Node( HeatRecOutletNode ).Temp = 20.0;

			MyEnvrnFlag( GenNum ) = false;
		} // end environmental inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( GenNum ) = true;
		}

		// set/request flow rates
		if ( FirstHVACIteration ) {

			if ( ! RunFlag ) {
				DesiredMassFlowRate = 0.0;

			} else if ( RunFlag && MTGenerator( GenNum ).InternalFlowControl ) {
				// assume dispatch power in MyLoad is what gets produced (future, reset during calc routine and iterate)
				if ( MTGenerator( GenNum ).HeatRecFlowFTempPowCurveNum != 0 ) {
					DesiredMassFlowRate = MTGenerator( GenNum ).DesignHeatRecMassFlowRate * CurveValue( MTGenerator( GenNum ).HeatRecFlowFTempPowCurveNum, Node( HeatRecInletNode ).Temp, MyLoad );
				} else {
					DesiredMassFlowRate = MTGenerator( GenNum ).DesignHeatRecMassFlowRate; // Assume modifier = 1 if curve not specified
				}

				DesiredMassFlowRate = max( constant_zero, DesiredMassFlowRate ); // protect from neg. curve result

			} else if ( RunFlag && ( ! MTGenerator( GenNum ).InternalFlowControl ) ) {
				DesiredMassFlowRate = MTGenerator( GenNum ).DesignHeatRecMassFlowRate;
			}

			SetComponentFlowRate( DesiredMassFlowRate, HeatRecInletNode, HeatRecOutletNode, MTGenerator( GenNum ).HRLoopNum, MTGenerator( GenNum ).HRLoopSideNum, MTGenerator( GenNum ).HRBranchNum, MTGenerator( GenNum ).HRCompNum );
		} else { // not FirstHVACIteration
			if ( ! RunFlag ) {
				Node( HeatRecInletNode ).MassFlowRate = min( constant_zero, Node( HeatRecInletNode ).MassFlowRateMaxAvail );
				Node( HeatRecInletNode ).MassFlowRate = max( constant_zero, Node( HeatRecInletNode ).MassFlowRateMinAvail );

			} else if ( RunFlag && MTGenerator( GenNum ).InternalFlowControl ) {
				// assume dispatch power in MyLoad is what gets produced (future, reset during calc routine and iterate)
				if ( MTGenerator( GenNum ).HeatRecFlowFTempPowCurveNum != 0 ) {
					DesiredMassFlowRate = MTGenerator( GenNum ).DesignHeatRecMassFlowRate * CurveValue( MTGenerator( GenNum ).HeatRecFlowFTempPowCurveNum, Node( HeatRecInletNode ).Temp, MyLoad );
					SetComponentFlowRate( DesiredMassFlowRate, HeatRecInletNode, HeatRecOutletNode, MTGenerator( GenNum ).HRLoopNum, MTGenerator( GenNum ).HRLoopSideNum, MTGenerator( GenNum ).HRBranchNum, MTGenerator( GenNum ).HRCompNum );
				} else {
					SetComponentFlowRate( MTGenerator( GenNum ).HeatRecMdot, HeatRecInletNode, HeatRecOutletNode, MTGenerator( GenNum ).HRLoopNum, MTGenerator( GenNum ).HRLoopSideNum, MTGenerator( GenNum ).HRBranchNum, MTGenerator( GenNum ).HRCompNum );
				}
			} else if ( RunFlag && ( ! MTGenerator( GenNum ).InternalFlowControl ) ) {
				SetComponentFlowRate( MTGenerator( GenNum ).HeatRecMdot, HeatRecInletNode, HeatRecOutletNode, MTGenerator( GenNum ).HRLoopNum, MTGenerator( GenNum ).HRLoopSideNum, MTGenerator( GenNum ).HRBranchNum, MTGenerator( GenNum ).HRCompNum );
			}
		}

	}

	//  End of MT Generator Module Initialize Subroutine
	// *****************************************************************************

	//  Beginning of MT Generator Model Calculation Subroutine
	// *****************************************************************************

	void
	CalcMTGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when generator is being asked to operate
		Real64 const MyLoad, // Generator demand (W)
		bool const EP_UNUSED( FirstHVACIteration ) // unused1208
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad/D. Shirey
		//       DATE WRITTEN   Mar 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Simulate a combustion generator.

		// METHODOLOGY EMPLOYED:
		//  Curve fits of performance data.

		// REFERENCES: na

		// Using/Aliasing
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::Elevation;
		using CurveManager::CurveValue;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using General::TrimSigDigits;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const KJtoJ( 1000.0 ); // Convert kilojoules to joules
		int const MaxAncPowerIter( 50 ); // Maximum number of iteration (subroutine ancillary power iteration loop)
		Real64 const AncPowerDiffToler( 5.0 ); // Tolerance for Ancillary Power Difference (W)
		Real64 const RelaxFactor( 0.7 ); // Relaxation factor for iteration loop
		static std::string const RoutineName( "CalcMTGeneratorModel" );

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinPartLoadRat; // Min allowed operating fraction at full load
		Real64 MaxPartLoadRat; // Max allowed operating fraction at full load
		Real64 ReferencePowerOutput; // Generator reference capacity (W)
		Real64 RefElecEfficiency; // Reference electrical efficiency
		Real64 OperatingElecEfficiency; // Actual operating efficiency
		Real64 ElecEfficiencyFTemp; // Electrical efficiency as a function of temperature curve output
		Real64 ElecEfficiencyFPLR; // Electrical efficiency as a function of PLR curve output
		Real64 ThermalEffFTempElev; // Thermal efficiency as a function of air temperature and elevation
		Real64 PLR; // Generator operating part load ratio
		Real64 PowerFTempElev; // Power ratio as a function of inlet air temperature and elevation
		Real64 CombustionAirInletTemp; // Combustion air inlet temperature (C)
		Real64 CombustionAirInletPress; // Barometric pressure of combustion inlet air (Pa)
		Real64 CombustionAirInletW; // Combustion air inlet humidity ratio (kg/kg)
		Real64 ExhFlowFTemp; // Exhaust air flow rate as a function of temperature curve output
		Real64 ExhFlowFPLR; // Exhaust air flow rate as a function of part-load ratio curve output
		Real64 ExhAirMassFlowRate; // Actual exhaust air mass flow rate (accounting for temp and PLR modifier curves)
		Real64 ExhAirTempFTemp; // Exhaust air temperature as a function of inlet air temp curve output
		Real64 ExhAirTempFPLR; // Exhaust air temperature as a function of part-load ratio curve output
		Real64 ExhaustAirTemp; // Actual exhaust air temperature (accounting for temp and PLR modifier curves)
		Real64 CpAir; // Heat capacity of air (J/kg-C)
		Real64 H2OHtOfVap; // Heat of vaporization of water (J/kg)
		Real64 ActualElevation; // Actual elevation of the microturbine (m)
		Real64 AirDensity; // Density of air at actual combustion inlet air conditions (kg/m3)

		Real64 ElecPowerGenerated; // Generator electric power output (W)
		Real64 FullLoadPowerOutput; // Generator full-load power output at actual inlet conditions and elevation (W)

		Real64 FuelUseEnergyRateLHV; // Rate of fuel energy required to run microturbine, LHV basis (W)
		Real64 QHeatRecToWater; // Recovered waste heat to water (W)
		Real64 MinHeatRecMdot; // Heat recovery flow rate if minimal heat recovery is accomplished (kg/s)
		int HeatRecInNode; // Heat recovery fluid inlet node number
		Real64 HeatRecInTemp; // Heat recovery fluid inlet temperature (C)
		Real64 HeatRecOutTemp; // Heat recovery fluid outlet temperature (C)
		Real64 HeatRecMdot; // Heat recovery fluid mass flow rate (kg/s)
		Real64 HeatRecVolFlowRate; // Heat recovery fluid flow rate (m3/s)
		Real64 HeatRecCp; // Specific heat of the heat recovery fluid (J/kg-K)
		Real64 HeatRecRateFPLR; // Heat recovery rate as a function of PLR curve output
		Real64 HeatRecRateFTemp; // Heat recovery rate as a function of inlet water temp curve output
		Real64 HeatRecRateFFlow; // Heat recovery rate as a function of water flow rate curve output
		Real64 FuelHigherHeatingValue; // Higher heating value (HHV) of fuel (kJ/kg)
		Real64 FuelLowerHeatingValue; // Lower heating value (LLV) of fuel kJ/kg)
		Real64 HRecRatio; // When maximum temperature is reached the amount of recovered heat has to be reduced
		Real64 AncillaryPowerRate; // Ancillary power used by pump (if not specified in manufacturers data)
		Real64 AncillaryPowerRateLast; // Ancillary power used by pump from last iteration (iteration loop within this subroutine)
		Real64 AncillaryPowerRateDiff; // Difference between ancillary power rate and ancillary power rate last (last iteration)
		Real64 AnciPowerFMdotFuel; // Ancillary power as a function of fuel flow curve output
		int AncPowerCalcIterIndex; // Index for subroutine iteration loop if Ancillary Power (function of fuel flow) is used
		Real64 rho; // local fluid density

		//   Load local variables from data structure (for code readability)
		MinPartLoadRat = MTGenerator( GeneratorNum ).MinPartLoadRat;
		MaxPartLoadRat = MTGenerator( GeneratorNum ).MaxPartLoadRat;
		ReferencePowerOutput = MTGenerator( GeneratorNum ).RefElecPowerOutput;
		RefElecEfficiency = MTGenerator( GeneratorNum ).RefElecEfficiencyLHV;

		//   Initialize variables
		MTGenerator( GeneratorNum ).ElecPowerGenerated = 0.0;
		MTGenerator( GeneratorNum ).HeatRecInletTemp = 0.0;
		MTGenerator( GeneratorNum ).HeatRecOutletTemp = 0.0;
		MTGenerator( GeneratorNum ).HeatRecMdot = 0.0;
		MTGenerator( GeneratorNum ).QHeatRecovered = 0.0;
		MTGenerator( GeneratorNum ).ExhaustEnergyRec = 0.0;
		MTGenerator( GeneratorNum ).FuelEnergyUseRateHHV = 0.0;
		MTGenerator( GeneratorNum ).FuelMdot = 0.0;
		MTGenerator( GeneratorNum ).AncillaryPowerRate = 0.0;
		MTGenerator( GeneratorNum ).StandbyPowerRate = 0.0;
		MTGenerator( GeneratorNum ).FuelEnergyUseRateLHV = 0.0;
		MTGenerator( GeneratorNum ).ExhaustAirMassFlowRate = 0.0;
		MTGenerator( GeneratorNum ).ExhaustAirTemperature = 0.0;
		MTGenerator( GeneratorNum ).ExhaustAirHumRat = 0.0;
		ExhAirTempFTemp = 0.0;
		QHeatRecToWater = 0.0;

		if ( MTGenerator( GeneratorNum ).HeatRecActive ) {
			HeatRecInNode = MTGenerator( GeneratorNum ).HeatRecInletNodeNum;
			HeatRecInTemp = Node( HeatRecInNode ).Temp;
			HeatRecCp = GetSpecificHeatGlycol( PlantLoop( MTGenerator( GeneratorNum ).HRLoopNum ).FluidName, HeatRecInTemp, PlantLoop( MTGenerator( GeneratorNum ).HRLoopNum ).FluidIndex, RoutineName );
			HeatRecMdot = Node( HeatRecInNode ).MassFlowRate;
		} else {
			HeatRecInTemp = 0.0;
			HeatRecCp = 0.0;
			HeatRecMdot = 0.0;
		}

		//   Set combustion inlet air temperature, humidity ratio and pressure local variables
		if ( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum == 0 ) { // no inlet air node specified, so use weather file values
			CombustionAirInletTemp = OutDryBulbTemp;
			CombustionAirInletW = OutHumRat;
			CombustionAirInletPress = OutBaroPress;
			ActualElevation = Elevation; // from DataEnvironment
		} else { // use inlet node information
			CombustionAirInletTemp = Node( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum ).Temp;
			CombustionAirInletW = Node( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum ).HumRat;
			CombustionAirInletPress = Node( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum ).Press;
			ActualElevation = Elevation; // from DataEnvironment
			if ( Node( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum ).Height > 0.0 ) {
				ActualElevation = Elevation + Node( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum ).Height;
			}
			//     Initialize combustion outlet air conditions to inlet air conditions (all node properties)
			if ( MTGenerator( GeneratorNum ).ExhAirCalcsActive ) {
				Node( MTGenerator( GeneratorNum ).CombustionAirOutletNodeNum ) = Node( MTGenerator( GeneratorNum ).CombustionAirInletNodeNum );
			}
		}

		//   If no loop demand or generator OFF, set some variables and then return
		//    IF (.NOT. RunFlag .OR. MyLoad .LE. 0.0d0) THEN
		if ( MyLoad <= 0.0 ) {
			MTGenerator( GeneratorNum ).HeatRecInletTemp = HeatRecInTemp;
			MTGenerator( GeneratorNum ).HeatRecOutletTemp = HeatRecInTemp;
			if ( RunFlag ) {
				MTGenerator( GeneratorNum ).StandbyPowerRate = MTGenerator( GeneratorNum ).StandbyPower;
			}
			MTGenerator( GeneratorNum ).ExhaustAirTemperature = CombustionAirInletTemp;
			MTGenerator( GeneratorNum ).ExhaustAirHumRat = CombustionAirInletW;
			return;
		}

		//   Calculate power modifier curve value (function of inlet air temperature and elevation)
		PowerFTempElev = CurveValue( MTGenerator( GeneratorNum ).ElecPowFTempElevCurveNum, CombustionAirInletTemp, Elevation );

		//   Warn user if power modifier curve output is less than 0
		if ( PowerFTempElev < 0.0 ) {
			if ( MTGenerator( GeneratorNum ).PowerFTempElevErrorIndex == 0 ) {
				//        MTGenerator(GeneratorNum)%PowerFTempElevErrorCount = MTGenerator(GeneratorNum)%PowerFTempElevErrorCount + 1
				ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowContinueError( "... Electrical Power Modifier curve (function of temperature and elevation) output is less than zero (" + TrimSigDigits( PowerFTempElev, 4 ) + ")." );
				ShowContinueError( "... Value occurs using a combustion inlet air temperature of " + TrimSigDigits( CombustionAirInletTemp, 2 ) + " C." );
				ShowContinueError( "... and an elevation of " + TrimSigDigits( Elevation, 2 ) + " m." );
				ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
			}
			ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Electrical Power Modifier curve is less than zero warning continues...", MTGenerator( GeneratorNum ).PowerFTempElevErrorIndex, PowerFTempElev, PowerFTempElev );
			PowerFTempElev = 0.0;
		}

		//   Calculate available full-load power output. cannot exceed maximum full-load power output.
		FullLoadPowerOutput = min( ( ReferencePowerOutput * PowerFTempElev ), MTGenerator( GeneratorNum ).MaxElecPowerOutput );
		//   Also can't be below the minimum full-load power output.
		FullLoadPowerOutput = max( FullLoadPowerOutput, MTGenerator( GeneratorNum ).MinElecPowerOutput );

		AncillaryPowerRate = MTGenerator( GeneratorNum ).AncillaryPower;
		AncillaryPowerRateLast = AncillaryPowerRate;
		AncillaryPowerRateDiff = AncPowerDiffToler + 1.0; // Initialize to force through DO WHILE Loop at least once
		AncPowerCalcIterIndex = 0; // Initialize iteration index (counter)

		while ( AncillaryPowerRateDiff > AncPowerDiffToler && AncPowerCalcIterIndex <= MaxAncPowerIter ) {

			++AncPowerCalcIterIndex; // Increment iteration loop counter

			//     Calculate operating power output (gross)
			ElecPowerGenerated = min( max( 0.0, MyLoad + AncillaryPowerRate ), FullLoadPowerOutput );

			//     Calculate PLR, but must be between the minPLR and maxPLR
			if ( FullLoadPowerOutput > 0.0 ) {
				PLR = min( ElecPowerGenerated / FullLoadPowerOutput, MaxPartLoadRat );
				PLR = max( PLR, MinPartLoadRat );
			} else {
				PLR = 0.0;
			}

			//     Recalculate ElecPowerGenerated based on "final" PLR
			ElecPowerGenerated = FullLoadPowerOutput * PLR;

			//     Calculate electrical efficiency modifier curve output (function of temp)
			ElecEfficiencyFTemp = CurveValue( MTGenerator( GeneratorNum ).ElecEffFTempCurveNum, CombustionAirInletTemp );

			//     Warn user if efficiency modifier curve output is less than 0
			if ( ElecEfficiencyFTemp < 0.0 ) {
				if ( MTGenerator( GeneratorNum ).EffFTempErrorIndex == 0 ) {
					//          MTGenerator(GeneratorNum)%EffFTempErrorCount = MTGenerator(GeneratorNum)%EffFTempErrorCount + 1
					ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "... Electrical Efficiency Modifier (function of temperature) output is less than zero (" + TrimSigDigits( ElecEfficiencyFTemp, 4 ) + ")." );
					ShowContinueError( "... Value occurs using a combustion inlet air temperature of " + TrimSigDigits( CombustionAirInletTemp, 2 ) + " C." );
					ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Electrical Efficiency Modifier (function of temperature) output is less than zero warning continues...", MTGenerator( GeneratorNum ).EffFTempErrorIndex, ElecEfficiencyFTemp, ElecEfficiencyFTemp );
				ElecEfficiencyFTemp = 0.0;
			}

			//     Calculate efficiency modifier curve output (function of PLR)
			ElecEfficiencyFPLR = CurveValue( MTGenerator( GeneratorNum ).ElecEffFPLRCurveNum, PLR );

			//     Warn user if efficiency modifier curve output is less than 0
			if ( ElecEfficiencyFPLR < 0.0 ) {
				if ( MTGenerator( GeneratorNum ).EffFPLRErrorIndex == 0 ) {
					ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "... Electrical Efficiency Modifier (function of part-load ratio) output is less than zero (" + TrimSigDigits( ElecEfficiencyFPLR, 4 ) + ")." );
					ShowContinueError( "... Value occurs using a part-load ratio of " + TrimSigDigits( PLR, 3 ) + '.' );
					ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Electrical Efficiency Modifier (function of part-load ratio) output is less than zero warning continues...", MTGenerator( GeneratorNum ).EffFPLRErrorIndex, ElecEfficiencyFPLR, ElecEfficiencyFPLR );
				ElecEfficiencyFPLR = 0.0;
			}

			//     Calculate operating electrical efficiency
			OperatingElecEfficiency = RefElecEfficiency * ElecEfficiencyFTemp * ElecEfficiencyFPLR;

			//     Calculate fuel use (W = J/s), LHV basis
			if ( OperatingElecEfficiency > 0.0 ) {
				FuelUseEnergyRateLHV = ElecPowerGenerated / OperatingElecEfficiency;
			} else {
				FuelUseEnergyRateLHV = 0.0; // If fuel use rate is zero, then
				ElecPowerGenerated = 0.0; //  electric power generated must be zero.
			}

			//     Set fuel heating values
			FuelHigherHeatingValue = MTGenerator( GeneratorNum ).FuelHigherHeatingValue;
			FuelLowerHeatingValue = MTGenerator( GeneratorNum ).FuelLowerHeatingValue;

			//     Calculate fuel mass flow rate
			MTGenerator( GeneratorNum ).FuelMdot = FuelUseEnergyRateLHV / ( FuelLowerHeatingValue * KJtoJ );

			//     Calculate ancillary power requirement
			if ( MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum > 0 ) {
				AnciPowerFMdotFuel = CurveValue( MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum, MTGenerator( GeneratorNum ).FuelMdot );
				//       Warn user if ancillary power modifier curve output is less than 0
				if ( AnciPowerFMdotFuel < 0.0 ) {
					if ( MTGenerator( GeneratorNum ).AnciPowerFMdotFuelErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... Ancillary Power Modifier (function of fuel input) output is less than zero (" + TrimSigDigits( AnciPowerFMdotFuel, 4 ) + ")." );
						ShowContinueError( "... Value occurs using a fuel input mass flow rate of " + TrimSigDigits( MTGenerator( GeneratorNum ).FuelMdot, 4 ) + " kg/s." );
						ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Ancillary Power Modifier (function of fuel input) output is less than zero warning continues...", MTGenerator( GeneratorNum ).AnciPowerFMdotFuelErrorIndex, AnciPowerFMdotFuel, AnciPowerFMdotFuel );
					AnciPowerFMdotFuel = 0.0;
				}
			} else {
				AnciPowerFMdotFuel = 1.0;
			}

			AncillaryPowerRateLast = AncillaryPowerRate;

			if ( MTGenerator( GeneratorNum ).AncillaryPowerFuelCurveNum > 0 ) {
				AncillaryPowerRate = RelaxFactor * MTGenerator( GeneratorNum ).AncillaryPower * AnciPowerFMdotFuel - ( 1.0 - RelaxFactor ) * AncillaryPowerRateLast;
			}

			AncillaryPowerRateDiff = std::abs( AncillaryPowerRate - AncillaryPowerRateLast );

		}

		if ( AncPowerCalcIterIndex > MaxAncPowerIter ) {

			if ( MTGenerator( GeneratorNum ).AnciPowerIterErrorIndex == 0 ) {
				ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
				ShowContinueError( "... Iteration loop for electric power generation is not converging within tolerance." );
				ShowContinueError( "... Check the Ancillary Power Modifier Curve (function of fuel input)." );
				ShowContinueError( "... Ancillary Power = " + TrimSigDigits( AncillaryPowerRate, 1 ) + " W." );
				ShowContinueError( "... Fuel input rate = " + TrimSigDigits( AnciPowerFMdotFuel, 4 ) + " kg/s." );
				ShowContinueErrorTimeStamp( "... Simulation will continue." );
			}
			ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Iteration loop for electric power generation is not converging within tolerance continues...", MTGenerator( GeneratorNum ).AnciPowerIterErrorIndex );

		}

		//   Calculate electrical power generated
		MTGenerator( GeneratorNum ).ElecPowerGenerated = ElecPowerGenerated - AncillaryPowerRate;

		//   Report fuel energy use rate on HHV basis, which is the unit of measure when the fuel is sold
		MTGenerator( GeneratorNum ).FuelEnergyUseRateHHV = MTGenerator( GeneratorNum ).FuelMdot * FuelHigherHeatingValue * KJtoJ;
		MTGenerator( GeneratorNum ).AncillaryPowerRate = AncillaryPowerRate; // Move to data structure for later reporting
		MTGenerator( GeneratorNum ).FuelEnergyUseRateLHV = FuelUseEnergyRateLHV; // Move to data structure for reporting calculations

		//   When generator operates, standby losses are 0
		MTGenerator( GeneratorNum ).StandbyPowerRate = 0.0;

		//   Calculate heat recovery if active
		if ( MTGenerator( GeneratorNum ).HeatRecActive ) {

			if ( MTGenerator( GeneratorNum ).ThermEffFTempElevCurveNum > 0 ) {
				ThermalEffFTempElev = CurveValue( MTGenerator( GeneratorNum ).ThermEffFTempElevCurveNum, CombustionAirInletTemp, Elevation );
				//       Warn user if power modifier curve output is less than 0
				if ( ThermalEffFTempElev < 0.0 ) {
					if ( MTGenerator( GeneratorNum ).ThermEffFTempElevErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... Electrical Power Modifier curve (function of temperature and elevation) output is less than zero (" + TrimSigDigits( PowerFTempElev, 4 ) + ")." );
						ShowContinueError( "... Value occurs using a combustion inlet air temperature of " + TrimSigDigits( CombustionAirInletTemp, 2 ) + " C." );
						ShowContinueError( "... and an elevation of " + TrimSigDigits( Elevation, 2 ) + " m." );
						ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Electrical Power Modifier curve is less than zero warning continues...", MTGenerator( GeneratorNum ).ThermEffFTempElevErrorIndex, ThermalEffFTempElev, ThermalEffFTempElev );
					ThermalEffFTempElev = 0.0;
				}
			} else {
				ThermalEffFTempElev = 1.0; // If no curve provided, assume multiplier factor = 1.0
			}

			QHeatRecToWater = FuelUseEnergyRateLHV * MTGenerator( GeneratorNum ).RefThermalEffLHV * ThermalEffFTempElev;

			//     Calculate heat recovery rate modifier curve output (function of PLR)
			if ( MTGenerator( GeneratorNum ).HeatRecRateFPLRCurveNum > 0 ) {
				HeatRecRateFPLR = CurveValue( MTGenerator( GeneratorNum ).HeatRecRateFPLRCurveNum, PLR );
				//       Warn user if heat recovery modifier curve output is less than 0
				if ( HeatRecRateFPLR < 0.0 ) {
					if ( MTGenerator( GeneratorNum ).HeatRecRateFPLRErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... Heat Recovery Rate Modifier (function of part-load ratio) output is less than zero (" + TrimSigDigits( HeatRecRateFPLR, 4 ) + ")." );
						ShowContinueError( "... Value occurs using a part-load ratio of " + TrimSigDigits( PLR, 3 ) + '.' );
						ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Heat Recovery Rate Modifier (function of part-load ratio) output is less than zero warning continues...", MTGenerator( GeneratorNum ).HeatRecRateFPLRErrorIndex, HeatRecRateFPLR, HeatRecRateFPLR );
					HeatRecRateFPLR = 0.0;
				}
			} else {
				HeatRecRateFPLR = 1.0; // If no curve provided, assume multiplier factor = 1.0
			}

			//     Calculate heat recovery rate modifier curve output (function of inlet water temp)
			if ( MTGenerator( GeneratorNum ).HeatRecRateFTempCurveNum > 0 ) {
				HeatRecRateFTemp = CurveValue( MTGenerator( GeneratorNum ).HeatRecRateFTempCurveNum, HeatRecInTemp );
				if ( HeatRecRateFTemp < 0.0 ) {
					if ( MTGenerator( GeneratorNum ).HeatRecRateFTempErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... Heat Recovery Rate Modifier (function of inlet water temp) output is less than zero (" + TrimSigDigits( HeatRecRateFTemp, 4 ) + ")." );
						ShowContinueError( "... Value occurs using an inlet water temperature temperature of " + TrimSigDigits( HeatRecInTemp, 2 ) + " C." );
						ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Heat Recovery Rate Modifier (function of inlet water temp) output is less than zero warning continues...", MTGenerator( GeneratorNum ).HeatRecRateFTempErrorIndex, HeatRecRateFTemp, HeatRecRateFTemp );
					HeatRecRateFTemp = 0.0;
				}
			} else {
				HeatRecRateFTemp = 1.0; // If no curve provided, assume multiplier factor = 1.0
			}

			//     Calculate heat recovery rate modifier curve output (function of water [volumetric] flow rate)
			if ( MTGenerator( GeneratorNum ).HeatRecRateFWaterFlowCurveNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( MTGenerator( GeneratorNum ).HRLoopNum ).FluidName, HeatRecInTemp, PlantLoop( MTGenerator( GeneratorNum ).HRLoopNum ).FluidIndex, RoutineName );

				HeatRecVolFlowRate = HeatRecMdot / rho;
				HeatRecRateFFlow = CurveValue( MTGenerator( GeneratorNum ).HeatRecRateFWaterFlowCurveNum, HeatRecVolFlowRate );
				if ( HeatRecRateFFlow < 0.0 ) {
					if ( MTGenerator( GeneratorNum ).HeatRecRateFFlowErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "... Heat Recovery Rate Modifier (function of water flow rate) output is less than zero (" + TrimSigDigits( HeatRecRateFFlow, 4 ) + ")." );
						ShowContinueError( "... Value occurs using a water flow rate of " + TrimSigDigits( HeatRecVolFlowRate, 4 ) + " m3/s." );
						ShowContinueErrorTimeStamp( "... Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Heat Recovery Rate Modifier (function of water flow rate) output is less than zero warning continues...", MTGenerator( GeneratorNum ).HeatRecRateFFlowErrorIndex, HeatRecRateFFlow, HeatRecRateFFlow );
					HeatRecRateFFlow = 0.0;
				}
			} else {
				HeatRecRateFFlow = 1.0; // If no curve provided, assume multiplier factor = 1.0
			}

			QHeatRecToWater *= HeatRecRateFPLR * HeatRecRateFTemp * HeatRecRateFFlow;

			//     Check for divide by zero
			if ( ( HeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
				HeatRecOutTemp = HeatRecInTemp + QHeatRecToWater / ( HeatRecMdot * HeatRecCp );
			} else {
				HeatRecMdot = 0.0;
				HeatRecOutTemp = HeatRecInTemp;
				QHeatRecToWater = 0.0;
			}

			//     Now verify the maximum heat recovery temperature was not exceeded
			HRecRatio = 1.0;
			MinHeatRecMdot = 0.0;
			if ( HeatRecOutTemp > MTGenerator( GeneratorNum ).HeatRecMaxWaterTemp ) {

				if ( MTGenerator( GeneratorNum ).HeatRecMaxWaterTemp != HeatRecInTemp ) {
					MinHeatRecMdot = QHeatRecToWater / ( HeatRecCp * ( MTGenerator( GeneratorNum ).HeatRecMaxWaterTemp - HeatRecInTemp ) );
					if ( MinHeatRecMdot < 0.0 ) MinHeatRecMdot = 0.0;
				}

				//       Recalculate outlet water temperature with minimum flow rate (will normally match the max water outlet temp,
				//       unless the inlet water temp is greater than the max outlet temp)
				if ( ( MinHeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
					HeatRecOutTemp = QHeatRecToWater / ( MinHeatRecMdot * HeatRecCp ) + HeatRecInTemp;
					HRecRatio = HeatRecMdot / MinHeatRecMdot;
				} else {
					HeatRecOutTemp = HeatRecInTemp;
					HRecRatio = 0.0;
				}
				QHeatRecToWater *= HRecRatio; // Scale heat recovery rate using HRecRatio. Don't adjust flow rate.

			}

			//     Check water mass flow rate against minimum
			if ( MTGenerator( GeneratorNum ).HeatRecMinMassFlowRate > HeatRecMdot && HeatRecMdot > 0.0 ) {
				if ( MTGenerator( GeneratorNum ).HRMinFlowErrorIndex == 0 ) {
					ShowWarningError( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "...Heat reclaim water flow rate is below the generators minimum mass flow rate of (" + TrimSigDigits( MTGenerator( GeneratorNum ).HeatRecMinMassFlowRate, 4 ) + ")." );
					ShowContinueError( "...Heat reclaim water mass flow rate = " + TrimSigDigits( HeatRecMdot, 4 ) + '.' );
					ShowContinueErrorTimeStamp( "...Check inputs for heat recovery water flow rate." );
				}
				ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Heat recovery water flow rate is below the generators minimum mass flow rate warning continues...", MTGenerator( GeneratorNum ).HRMinFlowErrorIndex, HeatRecMdot, HeatRecMdot );
			}

			//     Check water mass flow rate against maximum
			if ( HeatRecMdot > MTGenerator( GeneratorNum ).HeatRecMaxMassFlowRate && HeatRecMdot > 0.0 ) {
				if ( MTGenerator( GeneratorNum ).HRMaxFlowErrorIndex == 0 ) {
					ShowWarningError( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "...Heat reclaim water flow rate is above the generators maximum mass flow rate of (" + TrimSigDigits( MTGenerator( GeneratorNum ).HeatRecMaxMassFlowRate, 4 ) + ")." );
					ShowContinueError( "...Heat reclaim water mass flow rate = " + TrimSigDigits( HeatRecMdot, 4 ) + '.' );
					ShowContinueErrorTimeStamp( "...Check inputs for heat recovery water flow rate." );
				}
				ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Heat recovery water flow rate is above the generators maximum mass flow rate warning continues...", MTGenerator( GeneratorNum ).HRMaxFlowErrorIndex, HeatRecMdot, HeatRecMdot );
			}

			//     Set report variables
			MTGenerator( GeneratorNum ).HeatRecInletTemp = HeatRecInTemp;
			MTGenerator( GeneratorNum ).HeatRecOutletTemp = HeatRecOutTemp;
			MTGenerator( GeneratorNum ).HeatRecMdot = HeatRecMdot;
			MTGenerator( GeneratorNum ).QHeatRecovered = QHeatRecToWater;

		} // End of  IF (MTGenerator(GeneratorNum)%HeatRecActive) THEN

		//   Calculate combustion air outlet conditions if exhaust air calculations are active
		if ( MTGenerator( GeneratorNum ).ExhAirCalcsActive ) {

			if ( MTGenerator( GeneratorNum ).ExhFlowFTempCurveNum != 0 ) { // Exhaust Flow Rate versus Inlet Air Temp
				ExhFlowFTemp = CurveValue( MTGenerator( GeneratorNum ).ExhFlowFTempCurveNum, CombustionAirInletTemp );
				//       Warn user if exhaust modifier curve output is less than or equal to 0
				if ( ExhFlowFTemp <= 0.0 ) {
					if ( MTGenerator( GeneratorNum ).ExhFlowFTempErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "...Exhaust Air Flow Rate Modifier (function of temperature) output is less than or equal to zero (" + TrimSigDigits( ExhFlowFTemp, 4 ) + ")." );
						ShowContinueError( "...Value occurs using a combustion inlet air temperature of " + TrimSigDigits( CombustionAirInletTemp, 2 ) + '.' );
						ShowContinueErrorTimeStamp( "...Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Exhaust Air Flow Rate Modifier (function of temperature) output is less than or equal to zero warning continues...", MTGenerator( GeneratorNum ).ExhFlowFTempErrorIndex, ExhFlowFTemp, ExhFlowFTemp );
					ExhFlowFTemp = 0.0;
				}
			} else {
				ExhFlowFTemp = 1.0; // No curve input means modifier = 1.0 always
			}

			if ( MTGenerator( GeneratorNum ).ExhFlowFPLRCurveNum != 0 ) { // Exhaust Flow Rate versus Part-Load Ratio
				ExhFlowFPLR = CurveValue( MTGenerator( GeneratorNum ).ExhFlowFPLRCurveNum, PLR );
				//       Warn user if exhaust modifier curve output is less than or equal to 0
				if ( ExhFlowFPLR <= 0.0 ) {
					if ( MTGenerator( GeneratorNum ).ExhFlowFPLRErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "...Exhaust Air Flow Rate Modifier (function of part-load ratio) output is less than or equal to zero (" + TrimSigDigits( ExhFlowFPLR, 4 ) + ")." );
						ShowContinueError( "...Value occurs using a part-load ratio of " + TrimSigDigits( PLR, 2 ) + '.' );
						ShowContinueErrorTimeStamp( "...Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Exhaust Air Flow Rate Modifier (function of part-load ratio) output is less than or equal to zero warning continues...", MTGenerator( GeneratorNum ).ExhFlowFPLRErrorIndex, ExhFlowFPLR, ExhFlowFPLR );
					ExhFlowFPLR = 0.0;
				}
			} else {
				ExhFlowFPLR = 1.0; // No curve input means modifier = 1.0 always
			}

			//     Calculate exhaust air mass flow, accounting for temperature and PLR modifier factors
			ExhAirMassFlowRate = MTGenerator( GeneratorNum ).RefExhaustAirMassFlowRate * ExhFlowFTemp * ExhFlowFPLR;
			//     Adjust for difference in air density at reference conditions versus actual inlet air conditions
			AirDensity = PsyRhoAirFnPbTdbW( CombustionAirInletPress, CombustionAirInletTemp, CombustionAirInletW );
			if ( MTGenerator( GeneratorNum ).RefCombustAirInletDensity >= 0.0 ) {
				ExhAirMassFlowRate = max( 0.0, ExhAirMassFlowRate * AirDensity / MTGenerator( GeneratorNum ).RefCombustAirInletDensity );
			} else {
				ExhAirMassFlowRate = 0.0;
			}
			MTGenerator( GeneratorNum ).ExhaustAirMassFlowRate = ExhAirMassFlowRate;

			if ( MTGenerator( GeneratorNum ).ExhAirTempFTempCurveNum != 0 ) { // Exhaust Air Temp versus Inlet Air Temp
				ExhAirTempFTemp = CurveValue( MTGenerator( GeneratorNum ).ExhAirTempFTempCurveNum, CombustionAirInletTemp );
				//       Warn user if exhaust modifier curve output is less than or equal to 0
				if ( ExhAirTempFTemp <= 0.0 ) {
					if ( MTGenerator( GeneratorNum ).ExhTempFTempErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "...Exhaust Air Temperature Modifier (function of temperature) output is less than or equal to zero (" + TrimSigDigits( ExhAirTempFTemp, 4 ) + ")." );
						ShowContinueError( "...Value occurs using a combustion inlet air temperature of " + TrimSigDigits( CombustionAirInletTemp, 2 ) + '.' );
						ShowContinueErrorTimeStamp( "...Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Exhaust Air Temperature Modifier (function of temperature) output is less than or equal to zero warning continues...", MTGenerator( GeneratorNum ).ExhTempFTempErrorIndex, ExhAirTempFTemp, ExhAirTempFTemp );
					ExhAirTempFTemp = 0.0;
				}
			} else {
				ExhAirTempFTemp = 1.0; // No curve input means modifier = 1.0 always
			}

			if ( MTGenerator( GeneratorNum ).ExhAirTempFPLRCurveNum != 0 ) { // Exhaust Air Temp versus Part-Load Ratio
				ExhAirTempFPLR = CurveValue( MTGenerator( GeneratorNum ).ExhAirTempFPLRCurveNum, PLR );
				//       Warn user if exhaust modifier curve output is less than or equal to 0
				if ( ExhAirTempFPLR <= 0.0 ) {
					if ( MTGenerator( GeneratorNum ).ExhTempFPLRErrorIndex == 0 ) {
						ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
						ShowContinueError( "...Exhaust Air Temperature Modifier (function of part-load ratio) output is less than or equal to zero (" + TrimSigDigits( ExhAirTempFPLR, 4 ) + ")." );
						ShowContinueError( "...Value occurs using a part-load ratio of " + TrimSigDigits( PLR, 2 ) + '.' );
						ShowContinueErrorTimeStamp( "...Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Exhaust Air Temperature Modifier (function of part-load ratio) output is less than or equal to zero warning continues...", MTGenerator( GeneratorNum ).ExhTempFPLRErrorIndex, ExhAirTempFPLR, ExhAirTempFPLR );
					ExhAirTempFPLR = 0.0;
				}
			} else {
				ExhAirTempFPLR = 1.0; // No curve input means modifier = 1.0 always
			}

			if ( ExhAirMassFlowRate <= 0.0 ) {
				MTGenerator( GeneratorNum ).ExhaustAirTemperature = CombustionAirInletTemp;
				MTGenerator( GeneratorNum ).ExhaustAirHumRat = CombustionAirInletW;
			} else {
				//       Calculate exhaust air temperature, accounting for inlet air temperature and PLR modifier factors
				ExhaustAirTemp = MTGenerator( GeneratorNum ).NomExhAirOutletTemp * ExhAirTempFTemp * ExhAirTempFPLR;
				MTGenerator( GeneratorNum ).ExhaustAirTemperature = ExhaustAirTemp;
				//       Adjust exhaust air temperature if heat recovery to water is being done
				if ( QHeatRecToWater > 0.0 ) {
					CpAir = PsyCpAirFnWTdb( CombustionAirInletW, CombustionAirInletTemp );
					if ( CpAir > 0.0 ) {
						MTGenerator( GeneratorNum ).ExhaustAirTemperature = ExhaustAirTemp - QHeatRecToWater / ( CpAir * ExhAirMassFlowRate );
					}
				}
				//       Calculate exhaust air humidity ratio
				H2OHtOfVap = PsyHfgAirFnWTdb( 1.0, 16.0 ); // W not used, passing 1.0 as dummy.
				// Assume fuel is at 16C (ASHRAE HOF)
				if ( H2OHtOfVap > 0.0 ) {
					MTGenerator( GeneratorNum ).ExhaustAirHumRat = CombustionAirInletW + MTGenerator( GeneratorNum ).FuelMdot * ( ( FuelHigherHeatingValue - FuelLowerHeatingValue ) * KJtoJ / H2OHtOfVap ) / ExhAirMassFlowRate;
				} else {
					MTGenerator( GeneratorNum ).ExhaustAirHumRat = CombustionAirInletW;
				}
			}

			if ( MTGenerator( GeneratorNum ).ExhaustAirTemperature < CombustionAirInletTemp ) {
				if ( MTGenerator( GeneratorNum ).ExhTempLTInletTempIndex == 0 ) {
					ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "...The model has calculated the exhaust air temperature to be less than the combustion air inlet temperature." );
					ShowContinueError( "...Value of exhaust air temperature   =" + TrimSigDigits( MTGenerator( GeneratorNum ).ExhaustAirTemperature, 4 ) + " C." );
					ShowContinueError( "...Value of combustion air inlet temp =" + TrimSigDigits( CombustionAirInletTemp, 4 ) + " C." );
					ShowContinueErrorTimeStamp( "... Simulation will continue." );
				}
				ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Exhaust air temperature less than combustion air inlet temperature warning continues...", MTGenerator( GeneratorNum ).ExhTempLTInletTempIndex, MTGenerator( GeneratorNum ).ExhaustAirTemperature, MTGenerator( GeneratorNum ).ExhaustAirTemperature );
			}

			if ( MTGenerator( GeneratorNum ).ExhaustAirHumRat < CombustionAirInletW ) {
				if ( MTGenerator( GeneratorNum ).ExhHRLTInletHRIndex == 0 ) {
					ShowWarningMessage( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\"" );
					ShowContinueError( "...The model has calculated the exhaust air humidity ratio to be less than the combustion air inlet humidity ratio." );
					ShowContinueError( "...Value of exhaust air humidity ratio          =" + TrimSigDigits( MTGenerator( GeneratorNum ).ExhaustAirHumRat, 6 ) + " kgWater/kgDryAir." );
					ShowContinueError( "...Value of combustion air inlet humidity ratio =" + TrimSigDigits( CombustionAirInletW, 6 ) + " kgWater/kgDryAir." );
					ShowContinueErrorTimeStamp( "... Simulation will continue." );
				}
				ShowRecurringWarningErrorAtEnd( "GENERATOR:MICROTURBINE \"" + MTGenerator( GeneratorNum ).Name + "\": Exhaust air humidity ratio less than combustion air inlet humidity ratio warning continues...", MTGenerator( GeneratorNum ).ExhHRLTInletHRIndex, MTGenerator( GeneratorNum ).ExhaustAirHumRat, MTGenerator( GeneratorNum ).ExhaustAirHumRat );
			}

		} // End of IF (MTGenerator(GeneratorNum)%ExhAirCalcsActive) THEN

	}

	//  End of MT Generator Model Calculation Subroutine
	// *****************************************************************************

	//  Beginning of record keeping subroutine for the MT Generator Module
	// *****************************************************************************

	void
	UpdateMTGeneratorRecords( int const Num ) // Generator number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad/D. Shirey
		//       DATE WRITTEN   Mar 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Reporting and updating nodes if necessary.

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
		int HeatRecInletNode; // Node number for heat recovery (water) inlet node
		int HeatRecOutletNode; // Node number for heat recovery (water) outlet node
		int ExhaustAirNodeNum; // Node number for exhaust air node
		int CombustAirInletNodeNum; // Node number for combustion inlet air node

		if ( MTGenerator( Num ).HeatRecActive ) {

			HeatRecInletNode = MTGenerator( Num ).HeatRecInletNodeNum;
			HeatRecOutletNode = MTGenerator( Num ).HeatRecOutletNodeNum;

			//   Node(HeatRecOutletNode)%MassFlowRate         = MTGenerator(Num)%HeatRecMdot
			Node( HeatRecOutletNode ).Temp = MTGenerator( Num ).HeatRecOutletTemp;
			//    Node(HeatRecOutletNode)%MassFlowRateMaxAvail = Node(HeatRecInletNode)%MassFlowRateMaxAvail
			//    Node(HeatRecOutletNode)%MassFlowRateMinAvail = Node(HeatRecInletNode)%MassFlowRateMinAvail

		}

		if ( MTGenerator( Num ).ExhAirCalcsActive ) {
			ExhaustAirNodeNum = MTGenerator( Num ).CombustionAirOutletNodeNum;
			CombustAirInletNodeNum = MTGenerator( Num ).CombustionAirInletNodeNum;

			Node( ExhaustAirNodeNum ).MassFlowRate = MTGenerator( Num ).ExhaustAirMassFlowRate;
			Node( CombustAirInletNodeNum ).MassFlowRate = MTGenerator( Num ).ExhaustAirMassFlowRate;

			Node( ExhaustAirNodeNum ).Temp = MTGenerator( Num ).ExhaustAirTemperature;
			Node( ExhaustAirNodeNum ).HumRat = MTGenerator( Num ).ExhaustAirHumRat;
			Node( ExhaustAirNodeNum ).MassFlowRateMaxAvail = Node( CombustAirInletNodeNum ).MassFlowRateMaxAvail;
			Node( ExhaustAirNodeNum ).MassFlowRateMinAvail = Node( CombustAirInletNodeNum ).MassFlowRateMinAvail;

			// also update the report variables
			MTGeneratorReport( Num ).ExhAirMassFlowRate = MTGenerator( Num ).ExhaustAirMassFlowRate;
			MTGeneratorReport( Num ).ExhAirTemperature = MTGenerator( Num ).ExhaustAirTemperature;
			// for exhaust only report

		}

		MTGeneratorReport( Num ).PowerGen = MTGenerator( Num ).ElecPowerGenerated;
		MTGeneratorReport( Num ).EnergyGen = MTGenerator( Num ).ElecPowerGenerated * TimeStepSys * SecInHour;
		MTGeneratorReport( Num ).QHeatRecovered = MTGenerator( Num ).QHeatRecovered;
		MTGeneratorReport( Num ).ExhaustEnergyRec = MTGenerator( Num ).QHeatRecovered * TimeStepSys * SecInHour;
		MTGeneratorReport( Num ).FuelEnergyUseRateHHV = MTGenerator( Num ).FuelEnergyUseRateHHV;
		MTGeneratorReport( Num ).FuelEnergyHHV = MTGenerator( Num ).FuelEnergyUseRateHHV * TimeStepSys * SecInHour;
		MTGeneratorReport( Num ).FuelMdot = MTGenerator( Num ).FuelMdot;
		if ( MTGenerator( Num ).FuelEnergyUseRateLHV > 0.0 ) {
			MTGeneratorReport( Num ).ElectricEfficiencyLHV = MTGenerator( Num ).ElecPowerGenerated / MTGenerator( Num ).FuelEnergyUseRateLHV;
			MTGeneratorReport( Num ).ThermalEfficiencyLHV = MTGenerator( Num ).QHeatRecovered / MTGenerator( Num ).FuelEnergyUseRateLHV;
		} else {
			MTGeneratorReport( Num ).ElectricEfficiencyLHV = 0.0;
			MTGeneratorReport( Num ).ThermalEfficiencyLHV = 0.0;
		}
		MTGeneratorReport( Num ).HeatRecInletTemp = MTGenerator( Num ).HeatRecInletTemp;
		MTGeneratorReport( Num ).HeatRecOutletTemp = MTGenerator( Num ).HeatRecOutletTemp;
		MTGeneratorReport( Num ).HeatRecMdot = MTGenerator( Num ).HeatRecMdot;
		MTGeneratorReport( Num ).AncillaryPowerRate = MTGenerator( Num ).AncillaryPowerRate;
		MTGeneratorReport( Num ).AncillaryEnergy = MTGenerator( Num ).AncillaryPowerRate * TimeStepSys * SecInHour;
		MTGeneratorReport( Num ).StandbyPowerRate = MTGenerator( Num ).StandbyPowerRate;
		MTGeneratorReport( Num ).StandbyEnergy = MTGenerator( Num ).StandbyPowerRate * TimeStepSys * SecInHour;

	}

	void
	GetMTGeneratorResults(
		int const EP_UNUSED( GeneratorType ), // type of Generator !unused1208
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

		GeneratorPower = MTGeneratorReport( GeneratorIndex ).PowerGen;
		GeneratorEnergy = MTGeneratorReport( GeneratorIndex ).EnergyGen;
		ThermalPower = MTGeneratorReport( GeneratorIndex ).QHeatRecovered;
		ThermalEnergy = MTGeneratorReport( GeneratorIndex ).ExhaustEnergyRec;

	}

	void
	GetMTGeneratorExhaustNode(
		int const EP_UNUSED( CompType ),
		std::string const & CompName,
		int & ExhaustOutletNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mahabir Bhandari
		//       DATE WRITTEN   Jul 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To pass exhaust outlet number from Micro Turbine to Exhaust fired absorption chiller.
		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

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

		if ( GetMTInput ) {
			// Read input data.
			GetMTGeneratorInput();
			GetMTInput = false;
		}

		ExhaustOutletNodeNum = 0;

		CompNum = FindItemInList( CompName, MTGenerator );

		if ( CompNum == 0 ) {
			ShowFatalError( "GetMTGeneratorExhaustNode: Unit not found=" + CompName );
		} else {
			ExhaustOutletNodeNum = MTGenerator( CompNum ).CombustionAirOutletNodeNum;
		}
	}

	// End of Record Keeping subroutine for the MT Generator Module
	// *****************************************************************************

} // MicroturbineElectricGenerator

} // EnergyPlus
