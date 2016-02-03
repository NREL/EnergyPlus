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

// EnergyPlus Headers
#include <BoilerSteam.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataGlobalConstants.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace BoilerSteam {

	// Module containing the routines dealing with the Boilers

	// MODULE INFORMATION:
	//    AUTHOR         Rahul Chillar
	//    DATE WRITTEN   Dec 2004
	//    MODIFIED       na
	//    RE-ENGINEERED  na
	// PURPOSE OF THIS MODULE:
	// Performs steam boiler simulation for plant simulation

	// METHODOLOGY EMPLOYED:
	// The steam boiler based on

	// REFERENCES:
	// none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::SecInHour;
	using DataGlobals::DisplayExtraWarnings;
	using namespace DataLoopNode;
	using namespace DataHVACGlobals;
	using General::TrimSigDigits;
	using General::RoundSigDigits;
	using DataBranchAirLoopPlant::MassFlowTolerance;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	Real64 FuelUsed( 0.0 ); // W - Boiler fuel used
	Real64 BoilerLoad( 0.0 ); // W - Boiler Load
	Real64 BoilerMassFlowRate( 0.0 ); // kg/s - Boiler mass flow rate
	Real64 BoilerOutletTemp( 0.0 ); // W - Boiler outlet temperature
	Real64 BoilerMaxPress( 0.0 );
	int NumBoilers( 0 ); // Number of boilers
	Real64 BoilerMassFlowMaxAvail( 0.0 ); // kg/s - Boiler mass flow rate
	Real64 BoilerMassFlowMinAvail( 0.0 ); // kg/s - Boiler mass flow rate
	static std::string const FluidNameSteam( "STEAM" );

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Boilers

	// Object Data
	Array1D< BoilerSteamSpecs > Boiler; // dimension to number of machines
	Array1D< ReportVars > BoilerReport;

	// MODULE SUBROUTINES:

	// Beginning of Boiler Module Driver Subroutines

	// Functions

	void
	clear_state()
	{
		FuelUsed = 0.0;
		BoilerLoad = 0.0;
		BoilerMassFlowRate = 0.0;
		BoilerOutletTemp = 0.0;
		BoilerMaxPress = 0.0;
		NumBoilers = 0;
		BoilerMassFlowMaxAvail = 0.0;
		BoilerMassFlowMinAvail = 0.0;
		CheckEquipName.deallocate();
		Boiler.deallocate();
		BoilerReport.deallocate();
	}

	PlantComponent * BoilerSteamSpecs::factory( int const EP_UNUSED(objectType), std::string objectName ) {

		static bool GetInput( true ); // if TRUE read user input
		
		//Get Input
		if ( GetInput ) {
			GetBoilerInput();
			GetInput = false;
		}
		
		// Now look for this particular component in the list
		for ( auto & BoilerItem : Boiler ) {
			if ( BoilerItem.Name == objectName ) {
				return &BoilerItem;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "BoilerSteamSpecs::factory : Error getting inputs for Boiler named: " + objectName );
		// Shut up the compiler
		return nullptr;
	}
	
	void
	BoilerSteamSpecs::simulate( 
		const PlantLocation & calledFromLocation, 
		bool const FirstHVACIteration, 
		Real64 & CurLoad )
	{
	}
	
	void
	SimSteamBoiler(
		std::string const & EP_UNUSED( BoilerType ), // boiler type (used in CASE statement)
		std::string const & BoilerName, // boiler identifier
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // boiler counter/identifier
		bool const RunFlag, // if TRUE run boiler simulation--boiler is ON
		bool const FirstHVACIteration, // TRUE if First iteration of simulation
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // W - Actual demand boiler must satisfy--calculated by load dist. routine
		Real64 & MaxCap, // W - maximum boiler operating capacity
		Real64 & MinCap, // W - minimum boiler operating capacity
		Real64 & OptCap, // W - optimal boiler operating capacity
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor // sizing factor
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subrountine controls the boiler component simulation

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using namespace FluidProperties;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BoilerNum; // boiler counter/identifier

		// Initialize Loop Equipment
		if ( InitLoopEquip ) {
			InitBoiler( BoilerNum );
			SizeBoiler( BoilerNum );
			MinCap = Boiler( BoilerNum ).NomCap * Boiler( BoilerNum ).MinPartLoadRat;
			MaxCap = Boiler( BoilerNum ).NomCap * Boiler( BoilerNum ).MaxPartLoadRat;
			OptCap = Boiler( BoilerNum ).NomCap * Boiler( BoilerNum ).OptPartLoadRat;
			if ( GetSizingFactor ) {
				SizingFactor = Boiler( BoilerNum ).SizFac;
			}
			return;
		}

		//Calculate Load
		//Select boiler type and call boiler model
		InitBoiler( BoilerNum );
		CalcBoilerModel( BoilerNum, MyLoad, RunFlag, EquipFlowCtrl );
		UpdateBoilerRecords( MyLoad, RunFlag, BoilerNum, FirstHVACIteration );

	}

	void 
	BoilerSteamSpecs::onInitLoopEquip(
		const PlantLocation & calledFromLocation 
	){
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   
		//       MODIFIED       Feb. 2016, R. Zhang - LBNL, refactor plant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subrountine controls the boiler component simulation

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		
		this->InitBoiler();
		this->SizeBoiler();
		
	};
	
	void 
	BoilerSpecs::getDesignCapacities( 
		const PlantLocation & EP_UNUSED(calledFromLocation), 
		Real64 & MaxLoad, 
		Real64 & MinLoad, 
		Real64 & OptLoad 
	){
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   
		//       MODIFIED       Feb. 2016, R. Zhang - LBNL, refactor plant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// User Defined plant generic component

		// METHODOLOGY EMPLOYED:
		// This routine to be called from PlantLoopEquipment.

		// REFERENCES:
		// na
		
		// now interface sizing related values with rest of E+
		MinLoad = this->NomCap * this->MinPartLoadRat;
		MaxLoad = this->NomCap * this->MaxPartLoadRat;
		OptLoad = this->NomCap * this->OptPartLoadRat;
		
	}
	
	void 
	BoilerSteamSpecs::getSizingFactor( 
		Real64 & SizFac 
	){
		// SUBROUTINE INFORMATION:
		//       AUTHOR         DAN FISHER
		//       DATE WRITTEN   Sep. 1998
		//       MODIFIED       Feb. 2016, R. Zhang, Refactor plant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// User Defined plant generic component

		// METHODOLOGY EMPLOYED:
		// This routine to be called from PlantLoopEquipment.

		// REFERENCES:
		// na
		
		SizFac = this->SizFac;
	}
	
	void
	GetBoilerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   Dec 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get all boiler data from input file

		// METHODOLOGY EMPLOYED:
		// standard EnergyPlus input retrieval using input Processor

		// REFERENCES: na

		// Using/Aliasing
		using namespace DataGlobalConstants;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using FluidProperties::FindRefrigerant;
		using GlobalNames::VerifyUniqueBoilerName;
		using General::RoundSigDigits;
		using DataSizing::AutoSize;

		// Locals
		// PARAMETERS
		static std::string const RoutineName( "GetBoilerInput: " );

		//LOCAL VARIABLES
		int BoilerNum; // boiler identifier
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int SteamFluidIndex; // Fluid Index for Steam
		static bool ErrorsFound( false );
		bool errFlag;
		Array1D_string BoilerFuelTypeForOutputVariable; // used to set up report variables

		SteamFluidIndex = 0;
		cCurrentModuleObject = "Boiler:Steam";
		NumBoilers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumBoilers <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}

		//See if load distribution manager has already gotten the input
		if ( allocated( Boiler ) ) return;

		// Boiler will have fuel input to it , that is it !
		Boiler.allocate( NumBoilers );
		CheckEquipName.dimension( NumBoilers, true );
		BoilerFuelTypeForOutputVariable.allocate( NumBoilers );

		BoilerReport.allocate( NumBoilers );

		//LOAD ARRAYS WITH CURVE FIT Boiler DATA
		for ( BoilerNum = 1; BoilerNum <= NumBoilers; ++BoilerNum ) {
			GetObjectItem( cCurrentModuleObject, BoilerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Boiler, BoilerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueBoilerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			Boiler( BoilerNum ).Name = cAlphaArgs( 1 );

			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

			if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) || ( SELECT_CASE_var == "ELEC" ) ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "Electric";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "ELECTRICITY" );

			} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "Gas";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "NATURALGAS" );

			} else if ( SELECT_CASE_var == "DIESEL" ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "Diesel";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "DIESEL" );

			} else if ( SELECT_CASE_var == "GASOLINE" ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "Gasoline";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "GASOLINE" );

			} else if ( SELECT_CASE_var == "COAL" ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "Coal";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "COAL" );

			} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "FuelOil#1";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "DISTILLATE OIL" );

			} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "FuelOil#2";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "RESIDUAL OIL" );

			} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "Propane";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "PROPANE" );

			} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "OtherFuel1";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "OTHERFUEL1" );

			} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "OtherFuel2";
				Boiler( BoilerNum ).FuelType = AssignResourceTypeNum( "OTHERFUEL2" );

			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );

				// Set to Electric to avoid errors when setting up output variables
				BoilerFuelTypeForOutputVariable( BoilerNum ) = "Electric";
				ErrorsFound = true;
			}}

			// INPUTS from the IDF file
			Boiler( BoilerNum ).BoilerMaxOperPress = rNumericArgs( 1 );
			Boiler( BoilerNum ).Effic = rNumericArgs( 2 );
			Boiler( BoilerNum ).TempUpLimitBoilerOut = rNumericArgs( 3 );
			Boiler( BoilerNum ).NomCap = rNumericArgs( 4 );
			if ( Boiler( BoilerNum ).NomCap == AutoSize ) {
				Boiler( BoilerNum ).NomCapWasAutoSized = true;
			}
			Boiler( BoilerNum ).MinPartLoadRat = rNumericArgs( 5 );
			Boiler( BoilerNum ).MaxPartLoadRat = rNumericArgs( 6 );
			Boiler( BoilerNum ).OptPartLoadRat = rNumericArgs( 7 );
			Boiler( BoilerNum ).FullLoadCoef( 1 ) = rNumericArgs( 8 );
			Boiler( BoilerNum ).FullLoadCoef( 2 ) = rNumericArgs( 9 );
			Boiler( BoilerNum ).FullLoadCoef( 3 ) = rNumericArgs( 10 );
			Boiler( BoilerNum ).SizFac = rNumericArgs( 11 );
			if ( Boiler( BoilerNum ).SizFac <= 0.0 ) Boiler( BoilerNum ).SizFac = 1.0;

			if ( ( rNumericArgs( 8 ) + rNumericArgs( 9 ) + rNumericArgs( 10 ) ) == 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( " Sum of fuel use curve coefficients = 0.0" );
				ErrorsFound = true;
			}

			if ( rNumericArgs( 5 ) == 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cNumericFieldNames( 5 ) + '=' + RoundSigDigits( rNumericArgs( 5 ), 3 ) );
				ErrorsFound = true;
			}

			if ( rNumericArgs( 3 ) == 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cNumericFieldNames( 3 ) + '=' + RoundSigDigits( rNumericArgs( 3 ), 3 ) );
				ErrorsFound = true;
			}
			Boiler( BoilerNum ).BoilerInletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Boiler( BoilerNum ).BoilerOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Hot Steam Nodes" );

			if ( SteamFluidIndex == 0 && BoilerNum == 1 ) {
				SteamFluidIndex = FindRefrigerant( "Steam" );
				if ( SteamFluidIndex == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
					ShowContinueError( "Steam Properties not found; Steam Fluid Properties must be included in the input file." );
					ErrorsFound = true;
				}
			}

			Boiler( BoilerNum ).FluidIndex = SteamFluidIndex;

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in processing " + cCurrentModuleObject + " input." );
		}

		for ( BoilerNum = 1; BoilerNum <= NumBoilers; ++BoilerNum ) {
			SetupOutputVariable( "Boiler Heating Rate [W]", BoilerReport( BoilerNum ).BoilerLoad, "System", "Average", Boiler( BoilerNum ).Name );
			SetupOutputVariable( "Boiler Heating Energy [J]", BoilerReport( BoilerNum ).BoilerEnergy, "System", "Sum", Boiler( BoilerNum ).Name, _, "ENERGYTRANSFER", "BOILERS", _, "Plant" );
			if ( SameString( BoilerFuelTypeForOutputVariable( BoilerNum ), "Electric" ) ) {
				SetupOutputVariable( "Boiler " + BoilerFuelTypeForOutputVariable( BoilerNum ) + " Power [W]", BoilerReport( BoilerNum ).FuelUsed, "System", "Average", Boiler( BoilerNum ).Name );
			} else {
				SetupOutputVariable( "Boiler " + BoilerFuelTypeForOutputVariable( BoilerNum ) + " Rate [W]", BoilerReport( BoilerNum ).FuelUsed, "System", "Average", Boiler( BoilerNum ).Name );
			}
			SetupOutputVariable( "Boiler " + BoilerFuelTypeForOutputVariable( BoilerNum ) + " Energy [J]", BoilerReport( BoilerNum ).FuelConsumed, "System", "Sum", Boiler( BoilerNum ).Name, _, BoilerFuelTypeForOutputVariable( BoilerNum ), "Heating", _, "Plant" );
			SetupOutputVariable( "Boiler Steam Inlet Temperature [C]", BoilerReport( BoilerNum ).BoilerInletTemp, "System", "Average", Boiler( BoilerNum ).Name );
			SetupOutputVariable( "Boiler Steam Outlet Temperature [C]", BoilerReport( BoilerNum ).BoilerOutletTemp, "System", "Average", Boiler( BoilerNum ).Name );
			SetupOutputVariable( "Boiler Steam Mass Flow Rate [kg/s]", BoilerReport( BoilerNum ).Mdot, "System", "Average", Boiler( BoilerNum ).Name );

		}

		BoilerFuelTypeForOutputVariable.deallocate();

	}

	void
	BoilerSteamSpecs::InitBoiler() // number of the current electric chiller being simulated
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   Dec 2004
		//       MODIFIED       Feb. 2016, R. Zhang - LBNL, refactor plant component
		//       RE-ENGINEERED  D. Shirey, rework for plant upgrade

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Boiler components

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// Na

		// USE STATEMENTS:

		// Using/Aliasing
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSatSpecificHeatRefrig;
		using DataPlant::TypeOf_Boiler_Steam;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using PlantUtilities::InitComponentNodes;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitBoiler" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool FatalError;
		Real64 TempUpLimitBoilerOut; // C - Boiler outlet maximum temperature limit
		Real64 EnthSteamOutWet;
		Real64 EnthSteamOutDry;
		Real64 LatentEnthSteam;
		Real64 CpWater; // Heat capacity of condensed steam (liquid)
		int BoilerInletNode; // Boiler inlet node number
		int BoilerOutletNode; // Boiler outlet node number
		bool errFlag;

		// Init more variables
		if ( this->MyFlag ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_Boiler_Steam, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitBoiler: Program terminated due to previous condition(s)." );
			}

			this->MyFlag = false;
		}

		BoilerInletNode = this->BoilerInletNodeNum;
		BoilerOutletNode = this->BoilerOutletNodeNum;

		if ( BeginEnvrnFlag && this->MyEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			//BoilerOutletTemp     = Node(BoilerOutletNode)%TempSetPoint
			//TempUpLimitBoilerOut =Boiler(BoilerNum)%TempUpLimitBoilerOut
			//      TempUpLimitBoilerOut = Node(BoilerOutletNode)%TempSetPoint
			TempUpLimitBoilerOut = this->TempUpLimitBoilerOut; // Design Outlet Steam Temperature
			EnthSteamOutDry = GetSatEnthalpyRefrig( FluidNameSteam, TempUpLimitBoilerOut, 1.0, this->FluidIndex, RoutineName );
			EnthSteamOutWet = GetSatEnthalpyRefrig( FluidNameSteam, TempUpLimitBoilerOut, 0.0, this->FluidIndex, RoutineName );
			LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

			CpWater = GetSatSpecificHeatRefrig( FluidNameSteam, TempUpLimitBoilerOut, 0.0, this->FluidIndex, RoutineName );

			this->DesMassFlowRate = this->NomCap / ( LatentEnthSteam + CpWater * ( TempUpLimitBoilerOut - Node( BoilerInletNode ).Temp ) );

			InitComponentNodes( 0.0, this->DesMassFlowRate, this->BoilerInletNodeNum, this->BoilerOutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );

			this->BoilerPressCheck = 0.0;
			FuelUsed = 0.0;
			BoilerLoad = 0.0;
			//         BoilerMassFlowRate = 0.0
			BoilerOutletTemp = 0.0;
			BoilerMaxPress = 0.0;
			//        BoilerMassFlowMaxAvail = 0.0
			//        BoilerMassFlowMinAvail = 0.0

			if ( ( Node( this->BoilerOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( this->BoilerOutletNodeNum ).TempSetPointLo == SensedNodeFlagValue ) ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					if ( ! this->MissingSetPointErrDone ) {
						ShowWarningError( "Missing temperature setpoint for Boiler:Steam = " + this->Name );
						ShowContinueError( " A temperature setpoint is needed at the outlet node of the boiler, use a SetpointManager" );
						ShowContinueError( " The overall loop setpoint will be assumed for this boiler. The simulation continues ..." );
						this->MissingSetPointErrDone = true;
					}
				} else {
					// need call to EMS to check node
					FatalError = false; // but not really fatal yet, but should be.
					CheckIfNodeSetPointManagedByEMS( this->BoilerOutletNodeNum, iTemperatureSetPoint, FatalError );
					if ( FatalError ) {
						if ( ! this->MissingSetPointErrDone ) {
							ShowWarningError( "Missing temperature setpoint for VariableFlow mode Boiler named " + this->Name );
							ShowContinueError( " A temperature setpoint is needed at the outlet node of the boiler." );
							ShowContinueError( " Use a Setpoint Manager to establish a setpoint at the boiler outlet node " );
							ShowContinueError( " or use an EMS actuator to establish a setpoint at the boiler outlet node." );
							ShowContinueError( " The overall loop setpoint will be assumed for this boiler. The simulation continues..." );
							this->MissingSetPointErrDone = true;
						}
					}
				}
				this->UseLoopSetPoint = true; // this is for backward compatibility and could be removed
			}

			this->MyEnvrnFlag = false;

		} // End If for the Begin Environment initializations

		if ( ! BeginEnvrnFlag ) {
			this->MyEnvrnFlag = true;
		}

		if ( this->UseLoopSetPoint ) {
			//  At some point, need to circle back and get from plant data structure instead of node
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			{ auto const SELECT_CASE_var( PlantLoop( this->LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				Node( BoilerOutletNode ).TempSetPoint = Node( PlantLoop( this->LoopNum ).TempSetPointNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				Node( BoilerOutletNode ).TempSetPointLo = Node( PlantLoop( this->LoopNum ).TempSetPointNodeNum ).TempSetPointLo;
			}}

		}

	}

	void
	BoilerSteamSpecs::SizeBoiler()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   Dec 2004
		//       MODIFIED       Nov. 2013, Daeho Kang, add component sizing table entries
		//       MODIFIED       Feb. 2016, R. Zhang - LBNL, refactor plant component
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Boiler Components for which capacities and flow rates
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains Steam flow rate from the plant sizing array. Calculates nominal capacity from
		// the hot water flow rate and the hot water loop design delta T.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSatSpecificHeatRefrig;
		using namespace OutputReportPredefined;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeBoiler" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNum( 0 ); // Plant Sizing index corresponding to CurLoopNum
		bool ErrorsFound( false ); // If errors detected in input
		Real64 SteamDensity;
		Real64 EnthSteamOutWet;
		Real64 EnthSteamOutDry;
		Real64 LatentEnthSteam;
		Real64 SizingTemp;
		Real64 CpWater; // Heat capacity of condensed steam
		std::string equipName;
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 NomCapUser( 0.0 ); // Hardsized nominal capacity for reporting

		tmpNomCap = this->NomCap;

		// Find the appropriate Plant Sizing object
		PltSizNum = PlantLoop( this->LoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				SizingTemp = this->TempUpLimitBoilerOut;
				SteamDensity = GetSatDensityRefrig( FluidNameSteam, SizingTemp, 1.0, this->FluidIndex, RoutineName );
				EnthSteamOutDry = GetSatEnthalpyRefrig( FluidNameSteam, SizingTemp, 1.0, this->FluidIndex, RoutineName );
				EnthSteamOutWet = GetSatEnthalpyRefrig( FluidNameSteam, SizingTemp, 0.0, this->FluidIndex, RoutineName );
				LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
				CpWater = GetSatSpecificHeatRefrig( FluidNameSteam, SizingTemp, 0.0, this->FluidIndex, RoutineName );
				tmpNomCap = ( CpWater * SteamDensity * this->SizFac * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate + PlantSizData( PltSizNum ).DesVolFlowRate * SteamDensity * LatentEnthSteam );
				if ( ! this->NomCapWasAutoSized ) tmpNomCap = this->NomCap;
			} else {
				if ( this->NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->NomCapWasAutoSized ) {
					this->NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Boiler:Steam", this->Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Boiler:Steam", this->Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else { // Hard-sized with sizing data
					if ( this->NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = this->NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Boiler:Steam", this->Name,
								"Design Size Nominal Capacity [W]", tmpNomCap,
								"User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizePump: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( NomCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = NomCapUser;
					}
				}
			}
		} else {
			if ( this->NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Boiler nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Boiler:Steam object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->NomCapWasAutoSized && this->NomCap > 0.0
					&& PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Boiler:Steam", this->Name,
						"User-Specified Nominal Capacity [W]", this->NomCap );
			}
		}

		//  model has no volume flow rate, may need something else for steam loop sizing DSU??
		//DSU?      CALL RegisterPlantCompDesignFlow(Boiler(BoilerNum)%BoilerInletNodeNum,Boiler(BoilerNum)%VolFlowRate)

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = this->Name;
			PreDefTableEntry( pdchMechType, equipName, "Boiler:Steam" );
			PreDefTableEntry( pdchMechNomEff, equipName, this->Effic );
			PreDefTableEntry( pdchMechNomCap, equipName, this->NomCap );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	BoilerSteamSpecs::CalcBoilerModel(
		Real64 & MyLoad, // W - hot water demand to be met by boiler
		bool const RunFlag, // TRUE if boiler operating
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   Dec 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the boiler fuel consumption and the associated
		// hot water demand met by the boiler

		// METHODOLOGY EMPLOYED:
		// The model is based on a single combustion efficiency (=1 for electric)
		// and a second order polynomial fit of performance data to obtain part
		// load performance

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using FluidProperties::GetSatPressureRefrig;
		using FluidProperties::GetSatSpecificHeatRefrig;
		using FluidProperties::GetSatEnthalpyRefrig;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcBoilerModel" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 BoilerEff; // boiler efficiency
		Real64 BoilerNomCap; // W - boiler nominal capacity
		Real64 BoilerMaxPLR; // boiler maximum part load ratio
		Real64 BoilerMinPLR; // boiler minimum part load ratio
		Real64 TheorFuelUse; // Theoretical (stoichiometric) fuel use
		Real64 OperPLR; // operating part load ratio
		Real64 BoilerDeltaTemp( 0.0 ); // C - boiler inlet to outlet temperature difference
		Real64 TempUpLimitBout; // C - boiler high temperature limit
		Real64 BoilerMassFlowRateMax; // Max Design Boiler Mass Flow Rate converted from Volume Flow Rate
		Real64 EnthSteamOutDry;
		Real64 EnthSteamOutWet;
		Real64 LatentEnthSteam;
		Real64 QualitySteam;
		Array1D< Real64 > LoadCoef( 3 ); // coefficients of the fuel use/part load curve
		Real64 CpWater; // Heat capacity of condensed steam
		int BoilerInletNode; // Boiler inlet node number
		int BoilerOutletNode; // Boiler outlet node number
		//      CHARACTER(len=25) CErrCount                        !
		//      INTEGER,SAVE :: PressErrCount=0                    !
		int LoopNum;
		int LoopSideNum;

		//Loading the variables derived type in to local variables
		BoilerLoad = 0.0;
		BoilerMassFlowRate = 0.0;
		BoilerInletNode = this->BoilerInletNodeNum;
		BoilerOutletNode = this->BoilerOutletNodeNum;
		BoilerNomCap = this->NomCap;
		BoilerMaxPLR = this->MaxPartLoadRat;
		BoilerMinPLR = this->MinPartLoadRat;
		LoadCoef = this->FullLoadCoef;
		TempUpLimitBout = this->TempUpLimitBoilerOut;
		BoilerMassFlowRateMax = this->DesMassFlowRate;
		BoilerMaxPress = this->BoilerMaxOperPress;
		BoilerEff = this->Effic;

		QualitySteam = Node( BoilerInletNode ).Quality;
		LoopNum = this->LoopNum;
		LoopSideNum = this->LoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPointLo;
		}}
		//If the specified load is 0.0 or the boiler should not run then we leave this subroutine.Before leaving
		//if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
		//will not shut down the branch
		if ( MyLoad <= 0.0 || ! RunFlag ) {
			if ( EquipFlowCtrl == ControlType_SeriesActive ) BoilerMassFlowRate = Node( BoilerInletNode ).MassFlowRate;
			return;
		}

		//Set the current load equal to the boiler load
		BoilerLoad = MyLoad;

		this->BoilerPressCheck = GetSatPressureRefrig( FluidNameSteam, BoilerOutletTemp, this->FluidIndex, RoutineName );

		if ( ( this->BoilerPressCheck ) > BoilerMaxPress ) {
			if ( this->PressErrIndex == 0 ) {
				ShowSevereError( "Boiler:Steam=\"" + this->Name + "\", Saturation Pressure is greater than Maximum Operating Pressure," );
				ShowContinueError( "Lower Input Temperature" );
				ShowContinueError( "Steam temperature=[" + RoundSigDigits( BoilerOutletTemp, 2 ) + "] C" );
				ShowContinueError( "Refrigerant Saturation Pressure =[" + RoundSigDigits( this->BoilerPressCheck, 0 ) + "] Pa" );
			}
			ShowRecurringSevereErrorAtEnd( "Boiler:Steam=\"" + this->Name + "\", Saturation Pressure is greater than Maximum Operating Pressure..continues", this->PressErrIndex, this->BoilerPressCheck, this->BoilerPressCheck, _, "[Pa]", "[Pa]" );
		}

		CpWater = GetSatSpecificHeatRefrig( FluidNameSteam, Node( BoilerInletNode ).Temp, 0.0, this->FluidIndex, RoutineName );

		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			// Calculate the flow for the boiler

			{ auto const SELECT_CASE_var( PlantLoop( this->LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				BoilerDeltaTemp = Node( BoilerOutletNode ).TempSetPoint - Node( BoilerInletNode ).Temp;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				BoilerDeltaTemp = Node( BoilerOutletNode ).TempSetPointLo - Node( BoilerInletNode ).Temp;
			} else {
				assert( false );
			}}
			BoilerOutletTemp = BoilerDeltaTemp + Node( BoilerInletNode ).Temp;

			EnthSteamOutDry = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName );

			EnthSteamOutWet = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName );

			LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

			BoilerMassFlowRate = BoilerLoad / ( LatentEnthSteam + ( CpWater * BoilerDeltaTemp ) );
			//Check to see if the Maximum is exceeded, if so set to maximum
			//       BoilerMassFlowRate = MIN(BoilerMassFlowRateMax, BoilerMassFlowRate)
			//       BoilerMassFlowRate = MIN(BoilerMassFlowRate,Node(BoilerInletNode)%MassFlowRateMaxAvail)  !CRBranchPump
			//       BoilerMassFlowRate = MAX(BoilerMassFlowRate,Node(BoilerInletNode)%MassFlowRateMinAvail)     !CRBranchPump

			SetComponentFlowRate( BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );

		} else { // If FlowLock is True
			// Set the boiler flow rate from inlet node and then check performance
			BoilerMassFlowRate = Node( BoilerInletNode ).MassFlowRate;
			// Assume that it can meet the setpoint
			{ auto const SELECT_CASE_var( PlantLoop( this->LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				BoilerDeltaTemp = Node( BoilerOutletNode ).TempSetPoint - Node( BoilerInletNode ).Temp;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				BoilerDeltaTemp = Node( BoilerOutletNode ).TempSetPointLo - Node( BoilerInletNode ).Temp;
			}}
			//If boiler outlet temp is already greater than setpoint than it does not need to operate this iteration
			if ( BoilerDeltaTemp < 0.0 ) {
				{ auto const SELECT_CASE_var( PlantLoop( this->LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPointLo;
				}}
				EnthSteamOutDry = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName );
				EnthSteamOutWet = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName );

				LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

				BoilerLoad = ( BoilerMassFlowRate * LatentEnthSteam );

			} else {

				{ auto const SELECT_CASE_var( PlantLoop( this->LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPointLo;
				}}

				EnthSteamOutDry = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName );
				EnthSteamOutWet = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName );

				LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

				// Calculate the boiler load with the specified flow rate.
				BoilerLoad = std::abs( BoilerMassFlowRate * LatentEnthSteam ) + std::abs( BoilerMassFlowRate * CpWater * BoilerDeltaTemp );

			}

			// If load exceeds the distributed load set to the distributed load
			if ( BoilerLoad > MyLoad ) {
				BoilerLoad = MyLoad;

				// Reset later , here just for calculating latent heat
				{ auto const SELECT_CASE_var( PlantLoop( this->LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					BoilerOutletTemp = Node( BoilerOutletNode ).TempSetPointLo;
				}}

				EnthSteamOutDry = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName );

				EnthSteamOutWet = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName );

				LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

				BoilerDeltaTemp = BoilerOutletTemp - Node( BoilerInletNode ).Temp;

				BoilerMassFlowRate = BoilerLoad / ( LatentEnthSteam + CpWater * BoilerDeltaTemp );

				SetComponentFlowRate( BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );
			}

			// Checks Boiler Load on the basis of the machine limits.
			if ( BoilerLoad > BoilerNomCap ) {
				if ( BoilerMassFlowRate > MassFlowTolerance ) {
					BoilerLoad = BoilerNomCap;

					EnthSteamOutDry = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName );
					EnthSteamOutWet = GetSatEnthalpyRefrig( FluidNameSteam, BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName );

					LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

					BoilerDeltaTemp = BoilerOutletTemp - Node( BoilerInletNode ).Temp;

					BoilerMassFlowRate = BoilerLoad / ( LatentEnthSteam + CpWater * BoilerDeltaTemp );

					SetComponentFlowRate( BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum );
				} else {
					BoilerLoad = 0.0;
					BoilerOutletTemp = Node( BoilerInletNode ).Temp;
				}
			}

		} //End of the FlowLock If block

		// Limit BoilerOutletTemp.  If > max temp, trip boiler.
		if ( BoilerOutletTemp > TempUpLimitBout ) {
			BoilerDeltaTemp = 0.0;
			BoilerLoad = 0.0;
			BoilerOutletTemp = Node( BoilerInletNode ).Temp;
			//  Does BoilerMassFlowRate need to be set????
		}

		OperPLR = BoilerLoad / BoilerNomCap;
		OperPLR = min( OperPLR, BoilerMaxPLR );
		OperPLR = max( OperPLR, BoilerMinPLR );
		TheorFuelUse = BoilerLoad / BoilerEff;

		// Calculate fuel used
		FuelUsed = TheorFuelUse / ( LoadCoef( 1 ) + LoadCoef( 2 ) * OperPLR + LoadCoef( 3 ) * pow_2( OperPLR ) );

	}

	// Beginning of Record Keeping subroutines for the BOILER:SIMPLE Module

	void
	UpdateBoilerRecords(
		Real64 const MyLoad, // boiler operating load
		bool const RunFlag, // boiler on when TRUE
		int const Num, // boiler number
		bool const EP_UNUSED( FirstHVACIteration ) // TRUE if First iteration of simulation
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rahul Chillar
		//       DATE WRITTEN   Dec 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Boiler simulation reporting

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//     USE DataPlant, ONLY : PlantLoop
		// Using/Aliasing
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BoilerInletNode; // Boiler inlet node number
		int BoilerOutletNode; // Boiler outlet node number
		Real64 ReportingConstant;
		int LoopNum;
		int LoopSideNum;

		ReportingConstant = TimeStepSys * SecInHour;

		BoilerInletNode = Boiler( Num ).BoilerInletNodeNum;
		BoilerOutletNode = Boiler( Num ).BoilerOutletNodeNum;

		if ( MyLoad <= 0.0 || ! RunFlag ) {
			//set node temperatures
			SafeCopyPlantNode( BoilerInletNode, BoilerOutletNode );
			Node( BoilerOutletNode ).Temp = Node( BoilerInletNode ).Temp;
			BoilerReport( Num ).BoilerOutletTemp = Node( BoilerInletNode ).Temp;
			BoilerReport( Num ).BoilerLoad = 0.0;
			BoilerReport( Num ).FuelUsed = 0.0;
			Node( BoilerInletNode ).Press = Boiler( Num ).BoilerPressCheck;
			Node( BoilerOutletNode ).Press = Node( BoilerInletNode ).Press;
			Node( BoilerInletNode ).Quality = 0.0;
			Node( BoilerOutletNode ).Quality = Node( BoilerInletNode ).Quality;

		} else {
			//set node temperatures
			SafeCopyPlantNode( BoilerInletNode, BoilerOutletNode );
			Node( BoilerOutletNode ).Temp = BoilerOutletTemp;
			BoilerReport( Num ).BoilerOutletTemp = BoilerOutletTemp;
			BoilerReport( Num ).BoilerLoad = BoilerLoad;
			BoilerReport( Num ).FuelUsed = FuelUsed;
			Node( BoilerInletNode ).Press = Boiler( Num ).BoilerPressCheck; //???
			Node( BoilerOutletNode ).Press = Node( BoilerInletNode ).Press;
			Node( BoilerOutletNode ).Quality = 1.0; // Model assumes saturated steam exiting the boiler

		}

		BoilerReport( Num ).BoilerInletTemp = Node( BoilerInletNode ).Temp;
		BoilerReport( Num ).Mdot = Node( BoilerOutletNode ).MassFlowRate;
		LoopNum = Boiler( Num ).LoopNum;
		LoopSideNum = Boiler( Num ).LoopSideNum;

		BoilerReport( Num ).BoilerEnergy = BoilerReport( Num ).BoilerLoad * ReportingConstant;
		BoilerReport( Num ).FuelConsumed = BoilerReport( Num ).FuelUsed * ReportingConstant;

	}

	// End of Record Keeping subroutines for the BOILER:STEAM Module

} // BoilerSteam

} // EnergyPlus
