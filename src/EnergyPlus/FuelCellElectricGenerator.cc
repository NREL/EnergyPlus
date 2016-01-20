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
#include <FuelCellElectricGenerator.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataGenerators.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneratorDynamicsManager.hh>
#include <GeneratorFuelSupply.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace FuelCellElectricGenerator {

	//_______________________________________________________________________
	// IEA Annex 42 generators:
	// FuelCellElectricGenerator  (Solid Oxide Fuel Cell (SOFC))
	//                            (Proton Exchange Membrane FC (PEMFC))
	//                            IEA/ECBCS Annex 42 model)

	// MODULE INFORMATION:
	//       AUTHOR         Brent Griffth
	//       DATE WRITTEN   August. 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates the operation of Solid oxide fuel cell Generators.

	// METHODOLOGY EMPLOYED:
	// Once the ElectricPowerManager determines that the FuelCell Generator
	// is available to meet an electric load demand, it calls SimFuelCellGenerator
	// which in turn calls the FuelCell model.
	// See DataGenerators.cc for structures and variables

	// REFERENCES:
	// IEA/ECBCS Annex 42 model specification for Solid oxide and proton exchange membrane fuel cells

	// OTHER NOTES:
	// N/A

	// Using/Aliasing
	using namespace DataGenerators;
	using namespace DataLoopNode;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::NumOfZones;
	using DataGlobals::CurrentTime;
	using DataGlobals::DayOfSim;
	using DataGlobals::SecInHour;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::WarmupFlag;
	using DataGlobals::KelvinConv;
	using DataGlobals::HoursInDay;
	using DataGlobalConstants::iGeneratorFuelCell;
	using namespace GeneratorFuelSupply;
	using namespace GeneratorDynamicsManager;

	// Data
	//MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	bool GetFuelCellInput( true ); // When TRUE, calls subroutine to read input file.
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE FuelCell ElectricGenerator

	//PRIVATE    SetupFuelAndAirConstituentData ! hardwired data for gas phase thermochemistry calcs

	// MODULE SUBROUTINES:

	// Beginning of FuelCell Generator Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimFuelCellGenerator(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // simulate Generator when TRUE
		Real64 const MyLoad, // demand on electric generator
		bool const FirstHVACIteration
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   MArch 2005
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the Solid oxide fuel cell Generator model driver.  It
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
		if ( GetFuelCellInput ) {
			GetFuelCellGeneratorInput();
			GetFuelCellInput = false;
		}

		if ( GeneratorIndex == 0 ) {
			GenNum = FindItemInList( GeneratorName, FuelCell );
			if ( GenNum == 0 ) ShowFatalError( "SimFuelCellGenerator: Specified Generator not one of Valid FuelCell Generators " + GeneratorName );
			GeneratorIndex = GenNum;
		} else {
			GenNum = GeneratorIndex;
			if ( GenNum > NumFuelCellGenerators || GenNum < 1 ) {
				ShowFatalError( "SimFuelCellGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Number of FuelCell Generators=" + TrimSigDigits( NumFuelCellGenerators ) + ", Generator name=" + GeneratorName );
			}
			if ( CheckEquipName( GenNum ) ) {
				if ( GeneratorName != FuelCell( GenNum ).Name ) {
					ShowFatalError( "SimFuelCellGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( GenNum ) + ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + FuelCell( GenNum ).Name );
				}
				CheckEquipName( GenNum ) = false;
			}
		}

		InitFuelCellGenerators( GenNum );

		CalcFuelCellGeneratorModel( GenNum, RunFlag, MyLoad, FirstHVACIteration );

		CalcUpdateHeatRecovery( GenNum, FirstHVACIteration );

		UpdateFuelCellGeneratorRecords( RunFlag, GenNum );

	}

	// End FuelCell Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of FuelCell Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetFuelCellGeneratorInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Brent Griffith
		//       DATE WRITTEN:    April 2005

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the FuelCell Generator models.

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using namespace DataGenerators;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::FindItem;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using CurveManager::GetCurveIndex;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataHeatBalance::Zone;
		using DataHeatBalance::IntGainTypeOf_GeneratorFuelCell;
		using ScheduleManager::GetScheduleIndex;
		using General::RoundSigDigits;
		using DataGlobals::DisplayAdvancedReportVariables;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// PARAMETERS

		//LOCAL VARIABLES
		int GeneratorNum; // Generator counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 25 ); // character string data
		Array1D< Real64 > NumArray( 200 ); // numeric data TODO deal with allocatable for extensible
		static bool ErrorsFound( false ); // error flag
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumFuelCellPMs; // number of power subsystems in input file
		int NumFuelCellAirSups; // number of air supply subsystems in input file
		//  INTEGER       :: NumFuelCellFuelSups      ! number of fuel supply subsystems in input file
		int NumFCWaterSups; // number of water supply subsystems in input file
		int NumFuelCellAuxilHeaters;
		int NumFCExhaustGasHXs;
		int NumFCElecStorageUnits; // number of electrical storage objects in input file
		//  INTEGER       :: NumBatteries  !number of Manwell and McGowan battery data objects
		int NumFCPowerCondUnits; // number of power conditioning units (inverter)
		int NumFCStackCoolers; // number of stack coolers.
		int NumAirConstit; // number of gas constituents in air
		int FCPMNum; // loop counter over power subsystems
		int FCAirSupNum; // loop counter over air supply subsystems
		//  INTEGER       :: FCFuelSupNum !loop counter over fuel supply subsystems
		int ConstitNum; // loop counter for consituents
		int FCWaterSupNum; // loop counter over water supply subsystems
		int FCHXNum; // loop counter for heat exchangers
		int FCAuxHeatNum; // loop counter over auxiliar heater
		int FCPCUNum; // loop counter over inverters
		int StorageNum; // loop counter over electrical storage subsystems
		int FCScoolNum; // loop counter over stack coolers
		int thisFuelCell; // temporary index
		int otherFuelCell; // loop counter and temporary indexer
		int i; // loop counter
		static bool MyOneTimeFlag( true );
		std::string thisName;
		int NumHardCodedConstituents;
		int thisConstituent;
		int thisGasID;
		int FuelSupNum;

		//Autodesk:Uninit Initialize variables used uninitialized
		thisFuelCell = 0; //Autodesk:Uninit Force default initialization: Will cause intentional failure if used as a FuelCell array index: Issue may be fixed by changes in EP 8.2 but that wasn't documented here (initialization is harmless so left in for now)

		// execution
		if ( MyOneTimeFlag ) {

			cCurrentModuleObject = "Generator:FuelCell";
			NumFuelCellGenerators = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFuelCellGenerators <= 0 ) {
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			//ALLOCATE ARRAYS
			FuelCell.allocate( NumFuelCellGenerators ); // inits handeled in derived type definitions
			CheckEquipName.dimension( NumFuelCellGenerators, true );

			// first load in FuelCell names
			for ( GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum ) {
				GetObjectItem( cCurrentModuleObject, GeneratorNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell, GeneratorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				FuelCell( GeneratorNum ).Name = AlphArray( 1 );
				FuelCell( GeneratorNum ).NameFCPM = AlphArray( 2 );
				FuelCell( GeneratorNum ).NameFCAirSup = AlphArray( 3 );
				FuelCell( GeneratorNum ).NameFCFuelSup = AlphArray( 4 );
				FuelCell( GeneratorNum ).NameFCWaterSup = AlphArray( 5 );
				FuelCell( GeneratorNum ).NameFCAuxilHeat = AlphArray( 6 );
				FuelCell( GeneratorNum ).NameExhaustHX = AlphArray( 7 );
				FuelCell( GeneratorNum ).NameElecStorage = AlphArray( 8 );
				FuelCell( GeneratorNum ).NameInverter = AlphArray( 9 );
				if ( NumAlphas == 10 ) {
					FuelCell( GeneratorNum ).NameStackCooler = AlphArray( 10 );
				}
			}

			cCurrentModuleObject = "Generator:FuelCell:PowerModule";
			NumFuelCellPMs = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFuelCellPMs <= 0 ) { //Autodesk:Uninit Allowing code to continue past this condition used thisFuelCell uninitialized in EP 8.0
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			for ( FCPMNum = 1; FCPMNum <= NumFuelCellPMs; ++FCPMNum ) {
				GetObjectItem( cCurrentModuleObject, FCPMNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::FCPM ), FCPMNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameFCPM );
				if ( thisFuelCell > 0 ) { //cr9323

					FuelCell( thisFuelCell ).FCPM.Name = AlphArray( 1 );
					if ( SameString( AlphArray( 2 ), "ANNEX42" ) ) FuelCell( thisFuelCell ).FCPM.EffMode = DirectCurveMode;
					if ( SameString( AlphArray( 2 ), "NORMALIZED" ) ) FuelCell( thisFuelCell ).FCPM.EffMode = NormalizedCurveMode;
					if ( FuelCell( thisFuelCell ).FCPM.EffMode == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}
					FuelCell( thisFuelCell ).FCPM.EffCurveID = GetCurveIndex( AlphArray( 3 ) );
					if ( FuelCell( thisFuelCell ).FCPM.EffCurveID == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + AlphArray( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					FuelCell( thisFuelCell ).FCPM.NomEff = NumArray( 1 );
					FuelCell( thisFuelCell ).FCPM.NomPel = NumArray( 2 );
					FuelCell( thisFuelCell ).FCPM.NumCycles = NumArray( 3 );
					FuelCell( thisFuelCell ).FCPM.CyclingDegradRat = NumArray( 4 );
					FuelCell( thisFuelCell ).FCPM.NumRunHours = NumArray( 5 );
					FuelCell( thisFuelCell ).FCPM.OperateDegradRat = NumArray( 6 );
					FuelCell( thisFuelCell ).FCPM.ThreshRunHours = NumArray( 7 );
					FuelCell( thisFuelCell ).FCPM.UpTranLimit = NumArray( 8 );
					FuelCell( thisFuelCell ).FCPM.DownTranLimit = NumArray( 9 );
					FuelCell( thisFuelCell ).FCPM.StartUpTime = NumArray( 10 ) / SecInHour; //convert to hours from seconds
					FuelCell( thisFuelCell ).FCPM.StartUpFuel = NumArray( 11 );
					FuelCell( thisFuelCell ).FCPM.StartUpElectConsum = NumArray( 12 );
					FuelCell( thisFuelCell ).FCPM.StartUpElectProd = NumArray( 13 );
					FuelCell( thisFuelCell ).FCPM.ShutDownTime = NumArray( 14 ) / SecInHour; //convert to hours from seconds
					FuelCell( thisFuelCell ).FCPM.ShutDownFuel = NumArray( 15 );
					FuelCell( thisFuelCell ).FCPM.ShutDownElectConsum = NumArray( 16 );
					FuelCell( thisFuelCell ).FCPM.ANC0 = NumArray( 17 );
					FuelCell( thisFuelCell ).FCPM.ANC1 = NumArray( 18 );
					if ( SameString( AlphArray( 4 ), "ConstantRate" ) ) FuelCell( thisFuelCell ).FCPM.SkinLossMode = ConstantRateSkinLoss;
					if ( SameString( AlphArray( 4 ), "UAForProcessGasTemperature" ) ) FuelCell( thisFuelCell ).FCPM.SkinLossMode = UADTSkinLoss;
					if ( SameString( AlphArray( 4 ), "QUADRATIC FUNCTION OF FUEL RATE" ) ) FuelCell( thisFuelCell ).FCPM.SkinLossMode = QuadraticFuelNdotSkin;
					if ( FuelCell( thisFuelCell ).FCPM.SkinLossMode == 0 ) {
						//throw error
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;

					}
					FuelCell( thisFuelCell ).FCPM.ZoneName = AlphArray( 5 );
					FuelCell( thisFuelCell ).FCPM.ZoneID = FindItemInList( FuelCell( thisFuelCell ).FCPM.ZoneName, Zone );
					if ( FuelCell( thisFuelCell ).FCPM.ZoneID == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " = " + AlphArray( 5 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowContinueError( "Zone Name was not found " );
						ErrorsFound = true;
					}

					FuelCell( thisFuelCell ).FCPM.RadiativeFract = NumArray( 19 );
					FuelCell( thisFuelCell ).FCPM.QdotSkin = NumArray( 20 );
					FuelCell( thisFuelCell ).FCPM.UAskin = NumArray( 21 );

					FuelCell( thisFuelCell ).FCPM.SkinLossCurveID = GetCurveIndex( AlphArray( 6 ) );
					if ( FuelCell( thisFuelCell ).FCPM.SkinLossCurveID == 0 ) {
						if ( FuelCell( thisFuelCell ).FCPM.SkinLossMode == QuadraticFuelNdotSkin ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " = " + AlphArray( 6 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
							ErrorsFound = true;
						}
					}

					FuelCell( thisFuelCell ).FCPM.NdotDilutionAir = NumArray( 22 );
					FuelCell( thisFuelCell ).FCPM.StackHeatLossToDilution = NumArray( 23 );
					FuelCell( thisFuelCell ).FCPM.DilutionInletNodeName = AlphArray( 7 );
					FuelCell( thisFuelCell ).FCPM.DilutionInletNode = GetOnlySingleNode( AlphArray( 7 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					FuelCell( thisFuelCell ).FCPM.DilutionExhaustNodeName = AlphArray( 8 );
					FuelCell( thisFuelCell ).FCPM.DilutionExhaustNode = GetOnlySingleNode( AlphArray( 8 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

					FuelCell( thisFuelCell ).FCPM.PelMin = NumArray( 24 );
					FuelCell( thisFuelCell ).FCPM.PelMax = NumArray( 25 );

					//check for other FuelCell using the same power module and fill
					for ( otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell ) {
						if ( SameString( FuelCell( otherFuelCell ).FCPM.Name, FuelCell( thisFuelCell ).FCPM.Name ) ) {
							FuelCell( otherFuelCell ).FCPM = FuelCell( thisFuelCell ).FCPM;
						}

					}
				} else { // throw warning, did not find power module input
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			} // loop over NumFuelCellPMs

			GetGeneratorFuelSupplyInput();

			for ( FuelSupNum = 1; FuelSupNum <= NumGeneratorFuelSups; ++FuelSupNum ) {
				SetupFuelConstituentData( FuelSupNum, ErrorsFound );
			}

			//set fuel supply ID in Fuel cell structure
			for ( GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum ) {
				FuelCell( GeneratorNum ).FuelSupNum = FindItemInList( FuelCell( GeneratorNum ).NameFCFuelSup, FuelSupply ); // Fuel Supply ID
				if ( FuelCell( GeneratorNum ).FuelSupNum == 0 ) {
					ShowSevereError( "Fuel Supply Name: " + FuelCell( GeneratorNum ).NameFCFuelSup + " not found in " + FuelCell( GeneratorNum ).Name );
					ErrorsFound = true;
				}
			}

			cCurrentModuleObject = "Generator:FuelCell:AirSupply";
			NumFuelCellAirSups = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFuelCellAirSups <= 0 ) { //Autodesk:Uninit thisFuelCell was possibly uninitialized past this condition
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			for ( FCAirSupNum = 1; FCAirSupNum <= NumFuelCellAirSups; ++FCAirSupNum ) {
				GetObjectItem( cCurrentModuleObject, FCAirSupNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::AirSup ), FCAirSupNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameFCAirSup );
				if ( thisFuelCell > 0 ) {

					FuelCell( thisFuelCell ).AirSup.Name = AlphArray( 1 );
					FuelCell( thisFuelCell ).AirSup.NodeName = AlphArray( 2 );

					// check the node connections
					FuelCell( thisFuelCell ).AirSup.SupNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

					FuelCell( thisFuelCell ).AirSup.BlowerPowerCurveID = GetCurveIndex( AlphArray( 3 ) );
					if ( FuelCell( thisFuelCell ).AirSup.BlowerPowerCurveID == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + AlphArray( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowContinueError( "Curve name was not found " );
						ErrorsFound = true;
					}
					FuelCell( thisFuelCell ).AirSup.BlowerHeatLossFactor = NumArray( 1 );

					if ( SameString( AlphArray( 4 ), "AirRatiobyStoics" ) ) {
						FuelCell( thisFuelCell ).AirSup.AirSupRateMode = ConstantStoicsAirRat;
					} else if ( SameString( AlphArray( 4 ), "QuadraticFunctionofElectricPower" ) ) {
						FuelCell( thisFuelCell ).AirSup.AirSupRateMode = QuadraticFuncofPel;
					} else if ( SameString( AlphArray( 4 ), "QUADRATIC FUNCTION OF FUEL RATE" ) ) {
						FuelCell( thisFuelCell ).AirSup.AirSupRateMode = QuadraticFuncofNdot;
					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					FuelCell( thisFuelCell ).AirSup.Stoics = NumArray( 2 ) + 1.0;

					FuelCell( thisFuelCell ).AirSup.AirFuncPelCurveID = GetCurveIndex( AlphArray( 5 ) );
					if ( ( FuelCell( thisFuelCell ).AirSup.AirFuncPelCurveID == 0 ) && ( FuelCell( thisFuelCell ).AirSup.AirSupRateMode == QuadraticFuncofPel ) ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " = " + AlphArray( 5 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowSevereError( "Curve name was not found" );
						ErrorsFound = true;
					}

					FuelCell( thisFuelCell ).AirSup.AirTempCoeff = NumArray( 3 );

					FuelCell( thisFuelCell ).AirSup.AirFuncNdotCurveID = GetCurveIndex( AlphArray( 6 ) );
					if ( ( FuelCell( thisFuelCell ).AirSup.AirFuncNdotCurveID == 0 ) && ( FuelCell( thisFuelCell ).AirSup.AirSupRateMode == QuadraticFuncofNdot ) ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " = " + AlphArray( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowSevereError( "Curve name was not found" );
						ErrorsFound = true;
					}

					if ( SameString( "RecoverBurnerInverterStorage", AlphArray( 7 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.IntakeRecoveryMode = RecoverBurnInvertBatt;
					} else if ( SameString( "RecoverAuxiliaryBurner", AlphArray( 7 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.IntakeRecoveryMode = RecoverAuxiliaryBurner;
					} else if ( SameString( "RecoverInverterandStorage", AlphArray( 7 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.IntakeRecoveryMode = RecoverInverterBatt;
					} else if ( SameString( "RecoverInverter", AlphArray( 7 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.IntakeRecoveryMode = RecoverInverter;
					} else if ( SameString( "RecoverElectricalStorage", AlphArray( 7 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.IntakeRecoveryMode = RecoverBattery;
					} else if ( SameString( "NoRecovery", AlphArray( 7 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.IntakeRecoveryMode = NoRecoveryOnAirIntake;
					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 7 ) + " = " + AlphArray( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					if ( SameString( "AmbientAir", AlphArray( 8 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.ConstituentMode = RegularAir;
					} else if ( SameString( "UserDefinedConstituents", AlphArray( 8 ) ) ) {
						FuelCell( thisFuelCell ).AirSup.ConstituentMode = UserDefinedConstituents;
					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 8 ) + " = " + AlphArray( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					if ( FuelCell( thisFuelCell ).AirSup.ConstituentMode == UserDefinedConstituents ) {
						NumAirConstit = NumArray( 4 );
						FuelCell( thisFuelCell ).AirSup.NumConstituents = NumAirConstit;

						if ( NumAirConstit > 5 ) {
							ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + RoundSigDigits( NumArray( 4 ), 2 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
							ShowContinueError( "Fuel Cell model not set up for more than 5 air constituents" );
							ErrorsFound = true;
						}

						for ( ConstitNum = 1; ConstitNum <= NumAirConstit; ++ConstitNum ) {
							FuelCell( thisFuelCell ).AirSup.ConstitName( ConstitNum ) = AlphArray( ConstitNum + 8 );
							FuelCell( thisFuelCell ).AirSup.ConstitMolalFract( ConstitNum ) = NumArray( ConstitNum + 4 );

						}

					} else { //regular air
						NumAirConstit = 5;

						FuelCell( thisFuelCell ).AirSup.NumConstituents = NumAirConstit;

						FuelCell( thisFuelCell ).AirSup.ConstitName( 1 ) = "Nitrogen";
						FuelCell( thisFuelCell ).AirSup.ConstitMolalFract( 1 ) = 0.7728;

						FuelCell( thisFuelCell ).AirSup.ConstitName( 2 ) = "Oxygen";
						FuelCell( thisFuelCell ).AirSup.ConstitMolalFract( 2 ) = 0.2073;

						FuelCell( thisFuelCell ).AirSup.ConstitName( 3 ) = "Water";
						FuelCell( thisFuelCell ).AirSup.ConstitMolalFract( 3 ) = 0.0104;

						FuelCell( thisFuelCell ).AirSup.ConstitName( 4 ) = "Argon";
						FuelCell( thisFuelCell ).AirSup.ConstitMolalFract( 4 ) = 0.0092;

						FuelCell( thisFuelCell ).AirSup.ConstitName( 5 ) = "CarbonDioxide";
						FuelCell( thisFuelCell ).AirSup.ConstitMolalFract( 5 ) = 0.0003;

					}

					// check for molar fractions summing to 1.0.
					if ( std::abs( sum( FuelCell( thisFuelCell ).AirSup.ConstitMolalFract ) - 1.0 ) > 0.0001 ) {

						ShowSevereError( cCurrentModuleObject + " molar fractions do not sum to 1.0" );
						ShowContinueError( "..Sum was=" + RoundSigDigits( sum( FuelCell( thisFuelCell ).AirSup.ConstitMolalFract ), 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					//check for other FuelCell using the same Air Supply module and fill
					for ( otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell ) {
						if ( SameString( FuelCell( otherFuelCell ).AirSup.Name, FuelCell( thisFuelCell ).AirSup.Name ) ) {
							FuelCell( otherFuelCell ).AirSup = FuelCell( thisFuelCell ).AirSup;
						}
					}
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

			}

			for ( GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum ) {
				// find molal fraction of oxygen in air supply
				thisConstituent = FindItem( "Oxygen", FuelCell( GeneratorNum ).AirSup.ConstitName, FuelCell( GeneratorNum ).AirSup.NumConstituents );
				if ( thisConstituent > 0 ) FuelCell( GeneratorNum ).AirSup.O2fraction = FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisConstituent );

				NumHardCodedConstituents = 14;

				// Loop over air constituents and do one-time setup
				for ( i = 1; i <= FuelCell( GeneratorNum ).AirSup.NumConstituents; ++i ) {

					thisName = FuelCell( GeneratorNum ).AirSup.ConstitName( i );

					thisGasID = FindItem( thisName, GasPhaseThermoChemistryData, &GasPropertyDataStruct::ConstituentName );

					FuelCell( GeneratorNum ).AirSup.GasLibID( i ) = thisGasID;

				}

				//set up gas constiuents for product gases
				FuelCell( GeneratorNum ).FCPM.GasLibID( 1 ) = 1; //Carbon Dioxide
				FuelCell( GeneratorNum ).FCPM.GasLibID( 2 ) = 2; //Nitrogen
				FuelCell( GeneratorNum ).FCPM.GasLibID( 3 ) = 3; //Oxygen
				FuelCell( GeneratorNum ).FCPM.GasLibID( 4 ) = 4; //Water
				FuelCell( GeneratorNum ).FCPM.GasLibID( 5 ) = 5; //Argon
			}

			cCurrentModuleObject = "Generator:FuelCell:WaterSupply";
			NumFCWaterSups = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFCWaterSups <= 0 ) {
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			for ( FCWaterSupNum = 1; FCWaterSupNum <= NumFCWaterSups; ++FCWaterSupNum ) {
				GetObjectItem( cCurrentModuleObject, FCWaterSupNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::WaterSup ), FCWaterSupNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameFCWaterSup );
				if ( thisFuelCell > 0 ) {
					//  this is only the first instance of a FuelCell generator using this type of Water supply module
					FuelCell( thisFuelCell ).WaterSup.Name = AlphArray( 1 );
					FuelCell( thisFuelCell ).WaterSup.WaterSupRateCurveID = GetCurveIndex( AlphArray( 2 ) );
					if ( FuelCell( thisFuelCell ).WaterSup.WaterSupRateCurveID == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowContinueError( "Curve name was not found " );
						ErrorsFound = true;
					}
					FuelCell( thisFuelCell ).WaterSup.PmpPowerCurveID = GetCurveIndex( AlphArray( 3 ) );
					if ( FuelCell( thisFuelCell ).WaterSup.PmpPowerCurveID == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + AlphArray( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowContinueError( "Curve name was not found " );
						ErrorsFound = true;
					}
					FuelCell( thisFuelCell ).WaterSup.PmpPowerLossFactor = NumArray( 1 );

					//!CR9240?
					if ( SameString( "TemperatureFromAirNode", AlphArray( 4 ) ) ) {
						FuelCell( thisFuelCell ).WaterSup.WaterTempMode = WaterInReformAirNode;

						FuelCell( thisFuelCell ).WaterSup.NodeName = AlphArray( 5 );
						FuelCell( thisFuelCell ).WaterSup.NodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

					} else if ( SameString( "TemperatureFromWaterNode", AlphArray( 4 ) ) ) {
						FuelCell( thisFuelCell ).WaterSup.WaterTempMode = WaterInReformWaterNode;

						FuelCell( thisFuelCell ).WaterSup.NodeName = AlphArray( 5 );
						FuelCell( thisFuelCell ).WaterSup.NodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

					} else if ( SameString( "MainsWaterTemperature", AlphArray( 4 ) ) ) {
						FuelCell( thisFuelCell ).WaterSup.WaterTempMode = WaterInReformMains;

					} else if ( SameString( "TemperatureFromSchedule", AlphArray( 4 ) ) ) {
						FuelCell( thisFuelCell ).WaterSup.WaterTempMode = WaterInReformSchedule;
					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					FuelCell( thisFuelCell ).WaterSup.SchedNum = GetScheduleIndex( AlphArray( 6 ) );
					if ( ( FuelCell( thisFuelCell ).WaterSup.SchedNum == 0 ) && ( FuelCell( thisFuelCell ).WaterSup.WaterTempMode == WaterInReformSchedule ) ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " = " + AlphArray( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowContinueError( "Schedule was not found" );
						ErrorsFound = true;
					}

					//check for other FuelCell using the same Water Supply module and fill
					for ( otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell ) {
						if ( SameString( FuelCell( otherFuelCell ).WaterSup.Name, FuelCell( thisFuelCell ).WaterSup.Name ) ) {
							FuelCell( otherFuelCell ).WaterSup = FuelCell( thisFuelCell ).WaterSup;
						}
					}
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

			}

			cCurrentModuleObject = "Generator:FuelCell:AuxiliaryHeater";
			NumFuelCellAuxilHeaters = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFuelCellAuxilHeaters <= 0 ) {
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			for ( FCAuxHeatNum = 1; FCAuxHeatNum <= NumFuelCellAuxilHeaters; ++FCAuxHeatNum ) {
				GetObjectItem( cCurrentModuleObject, FCAuxHeatNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::AuxilHeat ), FCAuxHeatNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameFCAuxilHeat );
				if ( thisFuelCell > 0 ) {
					FuelCell( thisFuelCell ).AuxilHeat.Name = AlphArray( 1 );

					FuelCell( thisFuelCell ).AuxilHeat.ExcessAirRAT = NumArray( 1 );
					FuelCell( thisFuelCell ).AuxilHeat.ANC0 = NumArray( 2 );
					FuelCell( thisFuelCell ).AuxilHeat.ANC1 = NumArray( 3 );
					FuelCell( thisFuelCell ).AuxilHeat.UASkin = NumArray( 4 );

					if ( SameString( "SurroundingZone", AlphArray( 2 ) ) ) {
						FuelCell( thisFuelCell ).AuxilHeat.SkinLossDestination = SurroundingZone;
					} else if ( SameString( "AirInletForFuelCell", AlphArray( 2 ) ) ) {
						FuelCell( thisFuelCell ).AuxilHeat.SkinLossDestination = AirInletForFC;
					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					FuelCell( thisFuelCell ).AuxilHeat.ZoneName = AlphArray( 3 );
					FuelCell( thisFuelCell ).AuxilHeat.ZoneID = FindItemInList( AlphArray( 3 ), Zone );
					if ( ( FuelCell( thisFuelCell ).AuxilHeat.ZoneID == 0 ) && ( FuelCell( thisFuelCell ).AuxilHeat.SkinLossDestination == SurroundingZone ) ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + AlphArray( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowContinueError( "Zone name was not found " );
						ErrorsFound = true;
					}
					FuelCell( thisFuelCell ).AuxilHeat.MaxPowerW = NumArray( 5 );
					FuelCell( thisFuelCell ).AuxilHeat.MinPowerW = NumArray( 6 );
					FuelCell( thisFuelCell ).AuxilHeat.MaxPowerkmolperSec = NumArray( 7 );
					FuelCell( thisFuelCell ).AuxilHeat.MinPowerkmolperSec = NumArray( 8 );

					// TODO finish Auxiliary heater

					//check for other FuelCell using the same Auxiliary Heating module and fill
					for ( otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell ) {
						if ( SameString( FuelCell( otherFuelCell ).AuxilHeat.Name, FuelCell( thisFuelCell ).AuxilHeat.Name ) ) {
							FuelCell( otherFuelCell ).AuxilHeat = FuelCell( thisFuelCell ).AuxilHeat;
						}
					}
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

			}

			// exhaust gas heat exchanger
			cCurrentModuleObject = "Generator:FuelCell:ExhaustGasToWaterHeatExchanger";
			NumFCExhaustGasHXs = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumFCExhaustGasHXs <= 0 ) {
				ShowWarningError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ShowContinueError( "Fuel Cell model requires an " + cCurrentModuleObject + " object" );
				ErrorsFound = true;
			}

			for ( FCHXNum = 1; FCHXNum <= NumFCExhaustGasHXs; ++FCHXNum ) {
				GetObjectItem( cCurrentModuleObject, FCHXNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::ExhaustHX ), FCHXNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameExhaustHX );
				if ( thisFuelCell > 0 ) {
					FuelCell( thisFuelCell ).ExhaustHX.Name = AlphArray( 1 );
					FuelCell( thisFuelCell ).ExhaustHX.WaterInNodeName = AlphArray( 2 );
					FuelCell( thisFuelCell ).ExhaustHX.WaterOutNodeName = AlphArray( 3 );
					//find node ids for water path
					FuelCell( thisFuelCell ).ExhaustHX.WaterInNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					FuelCell( thisFuelCell ).ExhaustHX.WaterOutNode = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
					TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Heat Recovery Nodes" );

					//CR9240
					FuelCell( thisFuelCell ).ExhaustHX.ExhaustOutNodeName = AlphArray( 4 );
					FuelCell( thisFuelCell ).ExhaustHX.ExhaustOutNode = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

					if ( SameString( "FixedEffectiveness", AlphArray( 5 ) ) ) {
						FuelCell( thisFuelCell ).ExhaustHX.HXmodelMode = FixedEffectiveness;
					} else if ( SameString( "EmpiricalUAeff", AlphArray( 5 ) ) ) {
						FuelCell( thisFuelCell ).ExhaustHX.HXmodelMode = LMTDempiricalUAeff;
					} else if ( SameString( "FundementalUAeff", AlphArray( 5 ) ) ) {
						FuelCell( thisFuelCell ).ExhaustHX.HXmodelMode = LMTDfundementalUAeff;
					} else if ( SameString( "CONDENSING", AlphArray( 5 ) ) ) {
						FuelCell( thisFuelCell ).ExhaustHX.HXmodelMode = Condensing;
					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " = " + AlphArray( 5 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;

					}
					FuelCell( thisFuelCell ).ExhaustHX.WaterVolumeFlowMax = NumArray( 1 );
					FuelCell( thisFuelCell ).ExhaustHX.HXEffect = NumArray( 2 );
					FuelCell( thisFuelCell ).ExhaustHX.hxs0 = NumArray( 3 );
					FuelCell( thisFuelCell ).ExhaustHX.hxs1 = NumArray( 4 );
					FuelCell( thisFuelCell ).ExhaustHX.hxs2 = NumArray( 5 );
					FuelCell( thisFuelCell ).ExhaustHX.hxs3 = NumArray( 6 );
					FuelCell( thisFuelCell ).ExhaustHX.hxs4 = NumArray( 7 );
					FuelCell( thisFuelCell ).ExhaustHX.h0gas = NumArray( 8 );
					FuelCell( thisFuelCell ).ExhaustHX.NdotGasRef = NumArray( 9 );
					FuelCell( thisFuelCell ).ExhaustHX.nCoeff = NumArray( 10 );
					FuelCell( thisFuelCell ).ExhaustHX.AreaGas = NumArray( 11 );
					FuelCell( thisFuelCell ).ExhaustHX.h0Water = NumArray( 12 );
					FuelCell( thisFuelCell ).ExhaustHX.NdotWaterRef = NumArray( 13 );
					FuelCell( thisFuelCell ).ExhaustHX.mCoeff = NumArray( 14 );
					FuelCell( thisFuelCell ).ExhaustHX.AreaWater = NumArray( 15 );
					FuelCell( thisFuelCell ).ExhaustHX.Fadjust = NumArray( 16 );
					FuelCell( thisFuelCell ).ExhaustHX.l1Coeff = NumArray( 17 );
					FuelCell( thisFuelCell ).ExhaustHX.l2Coeff = NumArray( 18 );
					FuelCell( thisFuelCell ).ExhaustHX.CondensationThresholdTemp = NumArray( 19 );

					// store cooling water volume flow rate for autosizing system
					RegisterPlantCompDesignFlow( FuelCell( thisFuelCell ).ExhaustHX.WaterInNode, FuelCell( thisFuelCell ).ExhaustHX.WaterVolumeFlowMax );
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}

			cCurrentModuleObject = "Generator:FuelCell:ElectricalStorage";
			NumFCElecStorageUnits = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFCElecStorageUnits <= 0 ) {
				ShowWarningError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ShowContinueError( "Fuel Cell model requires an " + cCurrentModuleObject + " object" );
				ErrorsFound = true;
			}

			for ( StorageNum = 1; StorageNum <= NumFCElecStorageUnits; ++StorageNum ) {
				GetObjectItem( cCurrentModuleObject, StorageNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::ElecStorage ), StorageNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameElecStorage );
				if ( thisFuelCell > 0 ) {
					FuelCell( thisFuelCell ).ElecStorage.Name = AlphArray( 1 );

					if ( SameString( AlphArray( 2 ), "SimpleEfficiencyWithConstraints" ) ) {
						FuelCell( thisFuelCell ).ElecStorage.StorageModelMode = SimpleEffConstraints;
					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;

					}
					FuelCell( thisFuelCell ).ElecStorage.EnergeticEfficCharge = NumArray( 1 );
					FuelCell( thisFuelCell ).ElecStorage.EnergeticEfficDischarge = NumArray( 2 );
					FuelCell( thisFuelCell ).ElecStorage.NominalEnergyCapacity = NumArray( 3 );
					FuelCell( thisFuelCell ).ElecStorage.MaxPowerDraw = NumArray( 4 );
					FuelCell( thisFuelCell ).ElecStorage.MaxPowerStore = NumArray( 5 );
					FuelCell( thisFuelCell ).ElecStorage.StartingEnergyStored = NumArray( 6 );

					//check for other FuelCell using the same Electrical Storage and fill
					for ( otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell ) {
						if ( SameString( FuelCell( otherFuelCell ).ElecStorage.Name, FuelCell( thisFuelCell ).ElecStorage.Name ) ) {
							FuelCell( otherFuelCell ).ElecStorage = FuelCell( thisFuelCell ).ElecStorage;
						}
					}
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

			}

			cCurrentModuleObject = "Generator:FuelCell:Inverter";
			NumFCPowerCondUnits = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFCPowerCondUnits <= 0 ) {
				ShowWarningError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ShowContinueError( "Fuel Cell model requires a " + cCurrentModuleObject + " object" );

				ErrorsFound = true;
			}

			for ( FCPCUNum = 1; FCPCUNum <= NumFCPowerCondUnits; ++FCPCUNum ) {
				GetObjectItem( cCurrentModuleObject, FCPCUNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::Inverter ), FCPCUNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameInverter );
				if ( thisFuelCell > 0 ) {
					FuelCell( thisFuelCell ).Inverter.Name = AlphArray( 1 );

					if ( SameString( AlphArray( 2 ), "QUADRATIC" ) ) FuelCell( thisFuelCell ).Inverter.EffMode = InverterEffQuadratic;
					if ( SameString( AlphArray( 2 ), "Constant" ) ) FuelCell( thisFuelCell ).Inverter.EffMode = InverterEffConstant;
					if ( FuelCell( thisFuelCell ).Inverter.EffMode == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}

					FuelCell( thisFuelCell ).Inverter.ConstEff = NumArray( 1 );

					FuelCell( thisFuelCell ).Inverter.EffQuadraticCurveID = GetCurveIndex( AlphArray( 3 ) );
					if ( ( FuelCell( thisFuelCell ).Inverter.EffQuadraticCurveID == 0 ) && ( FuelCell( thisFuelCell ).Inverter.EffMode == InverterEffQuadratic ) ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + AlphArray( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ShowContinueError( "Curve was not found " );
						ErrorsFound = true;
					}

					//check for other FuelCell using the same Inverter and fill
					for ( otherFuelCell = thisFuelCell + 1; otherFuelCell <= NumFuelCellGenerators; ++otherFuelCell ) {
						if ( SameString( FuelCell( otherFuelCell ).Inverter.Name, FuelCell( thisFuelCell ).Inverter.Name ) ) {
							FuelCell( otherFuelCell ).Inverter = FuelCell( thisFuelCell ).Inverter;
						}
					}
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

			}

			cCurrentModuleObject = "Generator:FuelCell:StackCooler";
			NumFCStackCoolers = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumFCStackCoolers > 0 ) { // get stack cooler input data
				for ( FCScoolNum = 1; FCScoolNum <= NumFCStackCoolers; ++FCScoolNum ) {
					GetObjectItem( cCurrentModuleObject, FCScoolNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( AlphArray( 1 ), FuelCell.ma( &FCDataStruct::StackCooler ), NumFCStackCoolers - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
					}
					thisFuelCell = FindItemInList( AlphArray( 1 ), FuelCell, &FCDataStruct::NameStackCooler );
					if ( thisFuelCell > 0 ) {
						FuelCell( thisFuelCell ).StackCooler.Name = AlphArray( 1 );
						FuelCell( thisFuelCell ).StackCooler.WaterInNodeName = AlphArray( 2 );

						FuelCell( thisFuelCell ).StackCooler.WaterOutNodeName = AlphArray( 3 );

						FuelCell( thisFuelCell ).StackCooler.WaterInNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						FuelCell( thisFuelCell ).StackCooler.WaterOutNode = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
						TestCompSet( cCurrentModuleObject, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Heat Recovery Nodes" );

						FuelCell( thisFuelCell ).StackCooler.TstackNom = NumArray( 1 );
						FuelCell( thisFuelCell ).StackCooler.TstackActual = NumArray( 2 );
						FuelCell( thisFuelCell ).StackCooler.r0 = NumArray( 3 );
						FuelCell( thisFuelCell ).StackCooler.r1 = NumArray( 4 );
						FuelCell( thisFuelCell ).StackCooler.r2 = NumArray( 5 );
						FuelCell( thisFuelCell ).StackCooler.r3 = NumArray( 6 );
						FuelCell( thisFuelCell ).StackCooler.MdotStackCoolant = NumArray( 7 );
						FuelCell( thisFuelCell ).StackCooler.UAs_cool = NumArray( 8 );
						FuelCell( thisFuelCell ).StackCooler.Fs_cogen = NumArray( 9 );
						FuelCell( thisFuelCell ).StackCooler.As_cogen = NumArray( 10 );
						FuelCell( thisFuelCell ).StackCooler.MdotCogenNom = NumArray( 11 );
						FuelCell( thisFuelCell ).StackCooler.hCogenNom = NumArray( 12 );
						FuelCell( thisFuelCell ).StackCooler.ns = NumArray( 13 );
						FuelCell( thisFuelCell ).StackCooler.PstackPumpEl = NumArray( 14 );
						FuelCell( thisFuelCell ).StackCooler.PmpPowerLossFactor = NumArray( 15 );
						FuelCell( thisFuelCell ).StackCooler.f0 = NumArray( 16 );
						FuelCell( thisFuelCell ).StackCooler.f1 = NumArray( 17 );
						FuelCell( thisFuelCell ).StackCooler.f1 = NumArray( 18 );

						FuelCell( thisFuelCell ).StackCooler.StackCoolerPresent = true;

					} else {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 1 ) + " = " + AlphArray( 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
						ErrorsFound = true;
					}
				}
			}

			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in getting input for fuel cell model " );
			}

			for ( GeneratorNum = 1; GeneratorNum <= NumFuelCellGenerators; ++GeneratorNum ) {
				SetupOutputVariable( "Generator Produced Electric Power [W]", FuelCell( GeneratorNum ).Report.ACPowerGen, "System", "Average", FuelCell( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Produced Electric Energy [J]", FuelCell( GeneratorNum ).Report.ACEnergyGen, "System", "Sum", FuelCell( GeneratorNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "Plant" );
				SetupOutputVariable( "Generator Produced Thermal Rate [W]", FuelCell( GeneratorNum ).Report.qHX, "System", "Average", FuelCell( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Produced Thermal Energy [J]", FuelCell( GeneratorNum ).Report.HXenergy, "System", "Sum", FuelCell( GeneratorNum ).Name, _, "ENERGYTRANSFER", "COGENERATION", _, "Plant" );

				SetupOutputVariable( "Generator Fuel HHV Basis Energy [J]", FuelCell( GeneratorNum ).Report.FuelEnergyHHV, "System", "Sum", FuelCell( GeneratorNum ).Name, _, "Gas", "COGENERATION", _, "Plant" );
				SetupOutputVariable( "Generator Fuel HHV Basis Rate [W]", FuelCell( GeneratorNum ).Report.FuelEnergyUseRateHHV, "System", "Average", FuelCell( GeneratorNum ).Name );

				SetupOutputVariable( "Generator Zone Sensible Heat Transfer Rate [W]", FuelCell( GeneratorNum ).Report.SkinLossPower, "System", "Average", FuelCell( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Zone Sensible Heat Transfer Energy [J]", FuelCell( GeneratorNum ).Report.SkinLossEnergy, "System", "Sum", FuelCell( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Zone Convection Heat Transfer Rate [W]", FuelCell( GeneratorNum ).Report.SkinLossConvect, "System", "Average", FuelCell( GeneratorNum ).Name );
				SetupOutputVariable( "Generator Zone Radiation Heat Transfer Rate [W]", FuelCell( GeneratorNum ).Report.SkinLossRadiat, "System", "Average", FuelCell( GeneratorNum ).Name );

				SetupZoneInternalGain( FuelCell( GeneratorNum ).FCPM.ZoneID, "Generator:FuelCell", FuelCell( GeneratorNum ).Name, IntGainTypeOf_GeneratorFuelCell, FuelCell( GeneratorNum ).Report.SkinLossConvect, _, FuelCell( GeneratorNum ).Report.SkinLossRadiat );

				if ( DisplayAdvancedReportVariables ) { // show extra data originally needed for detailed comparative testing
					SetupOutputVariable( "Generator Air Inlet Temperature [C]", FuelCell( GeneratorNum ).Report.TairInlet, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Power Module Entering Air Temperature [C]", FuelCell( GeneratorNum ).Report.TairIntoFCPM, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Air Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotAir, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Power Module Entering Air Enthalpy [W]", FuelCell( GeneratorNum ).Report.TotAirInEnthalphy, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Blower Electric Power [W]", FuelCell( GeneratorNum ).Report.BlowerPower, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Blower Electric Energy [J]", FuelCell( GeneratorNum ).Report.BlowerEnergy, "System", "Sum", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Blower Skin Heat Loss Rate [W]", FuelCell( GeneratorNum ).Report.BlowerSkinLoss, "System", "Average", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Fuel Inlet Temperature [C]", FuelCell( GeneratorNum ).Report.TfuelInlet, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Power Module Entering Fuel Temperature [C]", FuelCell( GeneratorNum ).Report.TfuelIntoFCPM, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotFuel, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Consumption LHV Basis Energy [J]", FuelCell( GeneratorNum ).Report.FuelEnergyLHV, "System", "Sum", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Consumption Rate LHV Basis [W]", FuelCell( GeneratorNum ).Report.FuelEnergyUseRateLHV, "System", "Average", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Power Module Entering Fuel Enthalpy [W]", FuelCell( GeneratorNum ).Report.TotFuelInEnthalpy, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Compressor Electric Power [W]", FuelCell( GeneratorNum ).Report.FuelCompressPower, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Compressor Electric Energy [J]", FuelCell( GeneratorNum ).Report.FuelCompressEnergy, "System", "Sum", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Compressor Skin Heat Loss Rate [W]", FuelCell( GeneratorNum ).Report.FuelCompressSkinLoss, "System", "Average", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Fuel Reformer Water Inlet Temperature [C]", FuelCell( GeneratorNum ).Report.TwaterInlet, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Power Module Entering Reforming Water Temperature [C]", FuelCell( GeneratorNum ).Report.TwaterIntoFCPM, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Reformer Water Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotWater, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Reformer Water Pump Electric Power [W]", FuelCell( GeneratorNum ).Report.WaterPumpPower, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Fuel Reformer Water Pump Electric Energy [J]", FuelCell( GeneratorNum ).Report.WaterPumpEnergy, "System", "Sum", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Power Module Entering Reforming Water Enthalpy [W]", FuelCell( GeneratorNum ).Report.WaterIntoFCPMEnthalpy, "System", "Average", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Product Gas Temperature [C]", FuelCell( GeneratorNum ).Report.TprodGas, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Product Gas Enthalpy [W]", FuelCell( GeneratorNum ).Report.EnthalProdGas, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Product Gas Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotProdGas, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Product Gas Ar Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotProdAr, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Product Gas CO2 Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotProdCO2, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Product Gas H2O Vapor Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotProdH2O, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Product Gas N2 Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotProdN2, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Product Gas O2 Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.NdotProdO2, "System", "Average", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Heat Recovery Exit Gas Temperature [C]", FuelCell( GeneratorNum ).Report.THXexh, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Heat Recovery Exit Gas H2O Vapor Fraction [ ]", FuelCell( GeneratorNum ).Report.WaterVaporFractExh, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Heat Recovery Water Condensate Molar Flow Rate [kmol/s]", FuelCell( GeneratorNum ).Report.CondensateRate, "System", "Average", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Inverter Loss Power [W]", FuelCell( GeneratorNum ).Report.PCUlosses, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Produced DC Electric Power [W]", FuelCell( GeneratorNum ).Report.DCPowerGen, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator DC Power Efficiency [ ]", FuelCell( GeneratorNum ).Report.DCPowerEff, "System", "Average", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Electric Storage Charge State [J]", FuelCell( GeneratorNum ).Report.ElectEnergyinStorage, "System", "Average", FuelCell( GeneratorNum ).Name ); //? 'Sum'
					SetupOutputVariable( "Generator DC Storage Charging Power [W]", FuelCell( GeneratorNum ).Report.StoredPower, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator DC Storage Charging Energy [J]", FuelCell( GeneratorNum ).Report.StoredEnergy, "System", "Sum", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator DC Storage Discharging Power [W]", FuelCell( GeneratorNum ).Report.DrawnPower, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator DC Storage Discharging Energy [J]", FuelCell( GeneratorNum ).Report.DrawnEnergy, "System", "Sum", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Ancillary AC Electric Power [W]", FuelCell( GeneratorNum ).Report.ACancillariesPower, "System", "Average", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Ancillary AC Electric Energy [J]", FuelCell( GeneratorNum ).Report.ACancillariesEnergy, "System", "Sum", FuelCell( GeneratorNum ).Name );

					SetupOutputVariable( "Generator Fuel Cell Model Iteration Count [ ]", FuelCell( GeneratorNum ).Report.SeqSubstIterations, "System", "Sum", FuelCell( GeneratorNum ).Name );
					SetupOutputVariable( "Generator Regula Falsi Iteration Count [ ]", FuelCell( GeneratorNum ).Report.RegulaFalsiIterations, "System", "Sum", FuelCell( GeneratorNum ).Name );
				}
			}

			MyOneTimeFlag = false;
		}

	}

	// End of Get Input subroutines for the FuelCell Generator Module

	// Beginning of Generator model Subroutines
	// *****************************************************************************

	void
	CalcFuelCellGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED     na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a FuelCell generator using the Annex 42 model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data:
		// many subdomains such as fuel and air compressors, wa

		// REFERENCES: IEA/ECBCS Annex 42....

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using CurveManager::CurveValue;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHeatBalFanSys::ZT;
		using DataEnvironment::WaterMainsTemp;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 PpcuLosses; // losses in inverter [W]
		Real64 Pel; // DC power generated in Fuel Cell Power Module
		Real64 Pdemand;
		Real64 Eel;
		Real64 Tavg; // working average temperature
		static bool ConstrainedFCPM( false ); // true if power prod is constrained for some reason
		static bool ConstrainedFCPMTrans( false );
		Real64 PelDiff;
		int iter; // loop index over repeating set of inter dependent calculaitons
		Real64 NdotO2; // molar rate coeff working varible
		Real64 CpWater; // heat capacity of water in molar units
		Real64 WaterEnthOfForm; // Standard molar enthalpy of formation
		Real64 NdotFuel; // fuel flow rate
		Real64 NdotStoicAir; // Air to match fuel molar rate coeff, working variable
		Real64 NdotExcessAir; // Air in excess of match for fuel
		Real64 NdotCO2ProdGas; // CO2 from reaction
		Real64 NdotH20ProdGas; // Water from reaction
		Real64 NdotCO2; // temp CO2 molar rate coef product gas stream
		Real64 NdotN2; // temp Nitrogen rate coef product gas stream
		Real64 Ndot02; // temp Oxygen rate coef product gas stream
		Real64 NdotH20; // temp Water rate coef product gas stream
		Real64 NdotAr; // tmep Argon rate coef product gas stream
		Real64 Cp; // temp Heat Capacity, used in thermochemistry units of (J/mol K)
		Real64 Hmolfuel; // temp enthalpy of fuel mixture in KJ/mol
		Real64 Hmolair; // temp enthalpy of air mixture in KJ/mol
		Real64 HmolProdGases; // enthalpy of product gas mixture in KJ/mol
		Real64 HLiqWater; // temp enthalpy of liquid water in KJ/mol   No Formation
		Real64 HGasWater; // temp enthalpy of gaseous water in KJ/mol  No Formation
		int thisGas; // loop index
		Real64 MagofImbalance; // error signal to control exiting loop and targeting product enthalpy
		Real64 tmpTotProdGasEnthalphy;
		Real64 Acc; // accuracy control for SolveRegulaFalsi
		int MaxIter; // iteration control for SolveRegulaFalsi
		int SolverFlag; // feed back flag from SolveRegulaFalsi
		Array1D< Real64 > Par( 3 ); // parameters passed in to SolveRegulaFalsi
		// Par(1) = generator number index in structure
		// Par(2) = targeted enthalpy (W)
		// Par(3) = molar flow rate of product gases (kmol/s)
		Real64 tmpTprodGas;
		//unused  REAL(r64) :: LHV  !Lower Heating Value
		bool ConstrainedStorage; // contrained overall elect because of storage
		Real64 PgridExtra; // extra electric power that should go into storage but can't
		Real64 Pstorage; // power into storage (+),  power from storage (-)
		Real64 PintoInverter; // power into inverter after storage interactions
		Real64 PoutofInverter; // power out of inverter after losses and including storage
		Real64 PacAncillariesTotal; // total AC ancillaries

		//! begin controls block to be moved out to GeneratorDynamics module
		//If no loop demand or Generator OFF, return
		if ( ! RunFlag ) {

			// TODO zero out terms as appropriate

			if ( FuelCell( GeneratorNum ).FCPM.HasBeenOn ) {
				//FuelCell just now beginning to shut down,

				// set Day and Time of Last Shut Down
				FuelCell( GeneratorNum ).FCPM.FractionalDayofLastShutDown = double( DayOfSim ) + ( int( CurrentTime ) + ( SysTimeElapsed + ( CurrentTime - int( CurrentTime ) ) ) ) / HoursInDay;
				FuelCell( GeneratorNum ).FCPM.HasBeenOn = false;

				if ( FuelCell( GeneratorNum ).FCPM.ShutDownTime > 0.0 ) FuelCell( GeneratorNum ).FCPM.DuringShutDown = true;

			}

			//TODO  check to see if still in shut down mode and using fuel.
			if ( FuelCell( GeneratorNum ).FCPM.DuringShutDown ) {

			}

			return;
		}

		if ( ! FuelCell( GeneratorNum ).FCPM.HasBeenOn ) {
			//fuel cell just turned on
			// set Day and Time of Last STart Up

			FuelCell( GeneratorNum ).FCPM.FractionalDayofLastStartUp = double( DayOfSim ) + ( int( CurrentTime ) + ( SysTimeElapsed + ( CurrentTime - int( CurrentTime ) ) ) ) / HoursInDay;

			FuelCell( GeneratorNum ).FCPM.HasBeenOn = true;
			++FuelCell( GeneratorNum ).FCPM.NumCycles; // increment cycling counter

			if ( FuelCell( GeneratorNum ).FCPM.StartUpTime > 0.0 ) FuelCell( GeneratorNum ).FCPM.DuringStartUp = true;

		}

		//TODO deal with things when jump out if not running?
		if ( ! RunFlag ) {
			return;
		}

		// Note: MyLoad (input) is Pdemand (electical Power requested)
		Pdemand = MyLoad;
		PacAncillariesTotal = 0.0;
		PpcuLosses = 0.0;
		Pstorage = 0.0;
		PgridExtra = 0.0;
		PoutofInverter = 0.0;
		ConstrainedFCPM = false;

		//!BEGIN SEQUENTIAL SUBSTITUTION to handle a lot of inter-related calcs
		for ( iter = 1; iter <= 20; ++iter ) {
			if ( iter > 1 ) {
				FigurePowerConditioningLosses( GeneratorNum, PoutofInverter, PpcuLosses );
				FigureACAncillaries( GeneratorNum, PacAncillariesTotal );
				Pdemand = MyLoad + PacAncillariesTotal + PpcuLosses;
			} else {
				// control Step 1a: Figure ancillary AC power draws
				FigureACAncillaries( GeneratorNum, PacAncillariesTotal );
				Pdemand = MyLoad + PacAncillariesTotal;
				// Control Step 1b: Calculate losses associated with Power conditioning
				FigurePowerConditioningLosses( GeneratorNum, Pdemand, PpcuLosses );
				Pdemand += PpcuLosses;
				Pel = Pdemand;
			}

			FuelCell( GeneratorNum ).Inverter.PCUlosses = PpcuLosses;

			// Control step 2: adjust for transient and startup/shut down constraints

			FigureTransientConstraints( GeneratorNum, Pel, ConstrainedFCPMTrans, PelDiff );

			// Control step 3: adjust for max and min limits on Pel

			if ( Pel < FuelCell( GeneratorNum ).FCPM.PelMin ) {
				PelDiff += ( FuelCell( GeneratorNum ).FCPM.PelMin - Pel );
				Pel = FuelCell( GeneratorNum ).FCPM.PelMin;

				ConstrainedFCPM = true;
			}
			if ( Pel > FuelCell( GeneratorNum ).FCPM.PelMax ) {
				PelDiff += ( FuelCell( GeneratorNum ).FCPM.PelMax - Pel );
				Pel = FuelCell( GeneratorNum ).FCPM.PelMax;
				ConstrainedFCPM = true;

			}
			if ( ConstrainedFCPM ) {

			}

			FuelCell( GeneratorNum ).FCPM.Pel = Pel;
			//Now calculate FC models.  return to controls and batter after

			//Calculation Step 1. Determine electrical Efficiency Eel

			if ( FuelCell( GeneratorNum ).FCPM.EffMode == NormalizedCurveMode ) {
				//Equation (8) in FuelCell Spec modified for normalized curve

				Eel = CurveValue( FuelCell( GeneratorNum ).FCPM.EffCurveID, Pel / FuelCell( GeneratorNum ).FCPM.NomPel ) * FuelCell( GeneratorNum ).FCPM.NomEff * ( 1.0 - FuelCell( GeneratorNum ).FCPM.NumCycles * FuelCell( GeneratorNum ).FCPM.CyclingDegradRat ) * ( 1.0 - max( ( FuelCell( GeneratorNum ).FCPM.NumRunHours - FuelCell( GeneratorNum ).FCPM.ThreshRunHours ), 0.0 ) * FuelCell( GeneratorNum ).FCPM.OperateDegradRat );

			} else if ( FuelCell( GeneratorNum ).FCPM.EffMode == DirectCurveMode ) {
				//Equation (8) in FuelCell Spec
				Eel = CurveValue( FuelCell( GeneratorNum ).FCPM.EffCurveID, Pel ) * ( 1.0 - FuelCell( GeneratorNum ).FCPM.NumCycles * FuelCell( GeneratorNum ).FCPM.CyclingDegradRat ) * ( 1.0 - max( ( FuelCell( GeneratorNum ).FCPM.NumRunHours - FuelCell( GeneratorNum ).FCPM.ThreshRunHours ), 0.0 ) * FuelCell( GeneratorNum ).FCPM.OperateDegradRat );
			}

			FuelCell( GeneratorNum ).FCPM.Eel = Eel;
			// Calculation Step 2. Determine fuel rate

			NdotFuel = Pel / ( Eel * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).LHV * 1000000.0 ); //Eq. 10 solved for Ndot

			FuelCell( GeneratorNum ).FCPM.NdotFuel = NdotFuel;
			if ( Pel <= 0.0 ) {
				//TODO zero stuff before leaving
				Pel = 0.0;
				FuelCell( GeneratorNum ).FCPM.Pel = 0.0;
				return;
			} else {

				FuelCell( GeneratorNum ).FCPM.Pel = Pel;
			}

			// Calculation Step 3. Determine Air rate

			if ( FuelCell( GeneratorNum ).AirSup.AirSupRateMode == ConstantStoicsAirRat ) { //MEthod 1
				NdotO2 = FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).StoicOxygenRate * FuelCell( GeneratorNum ).FCPM.NdotFuel * FuelCell( GeneratorNum ).AirSup.Stoics;

				FuelCell( GeneratorNum ).FCPM.NdotAir = NdotO2 / FuelCell( GeneratorNum ).AirSup.O2fraction;

			} else if ( FuelCell( GeneratorNum ).AirSup.AirSupRateMode == QuadraticFuncofPel ) { //MEthod 2

				FuelCell( GeneratorNum ).FCPM.NdotAir = CurveValue( FuelCell( GeneratorNum ).AirSup.AirFuncPelCurveID, Pel ) * ( 1 + FuelCell( GeneratorNum ).AirSup.AirTempCoeff * FuelCell( GeneratorNum ).AirSup.TairIntoFCPM );

			} else if ( FuelCell( GeneratorNum ).AirSup.AirSupRateMode == QuadraticFuncofNdot ) { // method 3
				FuelCell( GeneratorNum ).FCPM.NdotAir = CurveValue( FuelCell( GeneratorNum ).AirSup.AirFuncNdotCurveID, FuelCell( GeneratorNum ).FCPM.NdotFuel ) * ( 1 + FuelCell( GeneratorNum ).AirSup.AirTempCoeff * FuelCell( GeneratorNum ).AirSup.TairIntoFCPM );

			}

			// Calculation Step 4. fuel compressor power

			FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).PfuelCompEl = CurveValue( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).CompPowerCurveID, FuelCell( GeneratorNum ).FCPM.NdotFuel );

			// calculation Step 5, Fuel Compressor (need outlet temperature)

			if ( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).FuelTempMode == FuelInTempFromNode ) {

				FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoCompress = Node( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).NodeNum ).Temp;

			} else if ( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).FuelTempMode == FuelInTempSchedule ) {

				FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoCompress = GetCurrentScheduleValue( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).SchedNum );

			}

			//  evaluate  heat capacity at average temperature usign shomate
			Tavg = ( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoCompress + FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoFCPM ) / 2.0;
			FigureFuelHeatCap( GeneratorNum, Tavg, Cp ); // Cp in (J/mol K)

			// calculate a Temp of fuel out of compressor and into power module

			if ( FuelCell( GeneratorNum ).FCPM.NdotFuel <= 0.0 ) { //just pass through, domain probably collapased in modeling
				FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoFCPM = FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoCompress;
			} else {
				FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoFCPM = ( ( 1.0 - FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).CompPowerLossFactor ) * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).PfuelCompEl / ( FuelCell( GeneratorNum ).FCPM.NdotFuel * Cp * 1000.0 ) ) + FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoCompress; //1000 Cp units mol-> kmol
			}
			// calc skin losses from fuel compressor
			FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).QskinLoss = FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).CompPowerLossFactor * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).PfuelCompEl;

			if ( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).QskinLoss < 0.0 ) {
				//   write(*,*) 'problem in FuelSupply%QskinLoss ', FuelSupply(FuelCell(GeneratorNum)%FuelSupNum)%QskinLoss
				ShowWarningError( "problem in FuelSupply%QskinLoss " + RoundSigDigits( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).QskinLoss, 3 ) );
				FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).QskinLoss = 0.0;
			}

			// calculate tatal fuel enthalpy coming into power module

			// (Hmolfuel in KJ/mol)
			FigureFuelEnthalpy( GeneratorNum, FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).TfuelIntoFCPM, Hmolfuel );

			// units, NdotFuel in kmol/sec. Hmolfule in KJ/mol ,
			//        factor of 1000's to get to J/s or watts
			FuelCell( GeneratorNum ).FCPM.TotFuelInEnthalphy = Hmolfuel * 1000.0 * FuelCell( GeneratorNum ).FCPM.NdotFuel * 1000.0;

			//Calculation Step 6, water compressor calculations

			// calculate water consumption

			FuelCell( GeneratorNum ).FCPM.NdotLiqwater = CurveValue( FuelCell( GeneratorNum ).WaterSup.WaterSupRateCurveID, FuelCell( GeneratorNum ).FCPM.NdotFuel );

			// set inlet temp.  (could move to init)

			{ auto const SELECT_CASE_var( FuelCell( GeneratorNum ).WaterSup.WaterTempMode );

			if ( SELECT_CASE_var == WaterInReformMains ) {

				FuelCell( GeneratorNum ).WaterSup.TwaterIntoCompress = WaterMainsTemp;

			} else if ( ( SELECT_CASE_var == WaterInReformAirNode ) || ( SELECT_CASE_var == WaterInReformWaterNode ) ) {

				FuelCell( GeneratorNum ).WaterSup.TwaterIntoCompress = Node( FuelCell( GeneratorNum ).WaterSup.NodeNum ).Temp;

			} else if ( SELECT_CASE_var == WaterInReformSchedule ) {

				FuelCell( GeneratorNum ).WaterSup.TwaterIntoCompress = GetCurrentScheduleValue( FuelCell( GeneratorNum ).WaterSup.SchedNum );

			}}

			FuelCell( GeneratorNum ).WaterSup.PwaterCompEl = CurveValue( FuelCell( GeneratorNum ).WaterSup.PmpPowerCurveID, FuelCell( GeneratorNum ).FCPM.NdotLiqwater );

			// 75.325  J/mol K Water at 0.1 MPa and 298 K, reference NIST WEBBOOK
			FigureLiquidWaterHeatCap( FuelCell( GeneratorNum ).WaterSup.TwaterIntoCompress, CpWater );

			WaterEnthOfForm = -241.8264; //KJ/mol

			if ( FuelCell( GeneratorNum ).FCPM.NdotLiqwater <= 0.0 ) { //just pass through, domain probably collapased in modeling
				FuelCell( GeneratorNum ).WaterSup.TwaterIntoFCPM = FuelCell( GeneratorNum ).WaterSup.TwaterIntoCompress;
			} else {

				FuelCell( GeneratorNum ).WaterSup.TwaterIntoFCPM = ( ( 1 - FuelCell( GeneratorNum ).WaterSup.PmpPowerLossFactor ) * FuelCell( GeneratorNum ).WaterSup.PwaterCompEl / ( FuelCell( GeneratorNum ).FCPM.NdotLiqwater * CpWater * 1000.0 ) ) + FuelCell( GeneratorNum ).WaterSup.TwaterIntoCompress;
			}

			FuelCell( GeneratorNum ).WaterSup.QskinLoss = FuelCell( GeneratorNum ).WaterSup.PmpPowerLossFactor * FuelCell( GeneratorNum ).WaterSup.PwaterCompEl;

			if ( FuelCell( GeneratorNum ).WaterSup.QskinLoss < 0.0 ) {
				// write(*,*) 'problem in WaterSup%QskinLoss ',FuelCell(GeneratorNum)%WaterSup%QskinLoss
				FuelCell( GeneratorNum ).WaterSup.QskinLoss = 0.0;
			}

			FigureLiquidWaterEnthalpy( FuelCell( GeneratorNum ).WaterSup.TwaterIntoFCPM, HLiqWater ); // HLiqWater in KJ/mol

			FuelCell( GeneratorNum ).FCPM.WaterInEnthalpy = FuelCell( GeneratorNum ).FCPM.NdotLiqwater * HLiqWater * 1000.0 * 1000.0;

			//Calculation Step 7, Air compressor

			FuelCell( GeneratorNum ).AirSup.TairIntoBlower = Node( FuelCell( GeneratorNum ).AirSup.SupNodeNum ).Temp;

			FuelCell( GeneratorNum ).AirSup.PairCompEl = CurveValue( FuelCell( GeneratorNum ).AirSup.BlowerPowerCurveID, FuelCell( GeneratorNum ).FCPM.NdotAir );

			Tavg = ( FuelCell( GeneratorNum ).AirSup.TairIntoBlower + FuelCell( GeneratorNum ).AirSup.TairIntoFCPM ) / 2.0;

			FigureAirHeatCap( GeneratorNum, Tavg, Cp ); // Cp in (J/mol K)

			// if PEMFC with stack cooler, then calculate stack cooler impacts
			if ( FuelCell( GeneratorNum ).StackCooler.StackCoolerPresent ) {

				FuelCell( GeneratorNum ).StackCooler.qs_cool = ( FuelCell( GeneratorNum ).StackCooler.r0 + FuelCell( GeneratorNum ).StackCooler.r1 * ( FuelCell( GeneratorNum ).StackCooler.TstackActual - FuelCell( GeneratorNum ).StackCooler.TstackNom ) ) * ( 1 + FuelCell( GeneratorNum ).StackCooler.r2 * Pel + FuelCell( GeneratorNum ).StackCooler.r3 * Pel * Pel ) * Pel;

				FuelCell( GeneratorNum ).FCPM.QdotStackCool = FuelCell( GeneratorNum ).StackCooler.qs_cool;

			}

			//Figure heat recovery from Electrical Storage, power conditioning, and auxiliary burner

			{ auto const SELECT_CASE_var( FuelCell( GeneratorNum ).AirSup.IntakeRecoveryMode );

			if ( SELECT_CASE_var == RecoverBurnInvertBatt ) {
				FuelCell( GeneratorNum ).AirSup.QintakeRecovery = FuelCell( GeneratorNum ).AuxilHeat.QairIntake + FuelCell( GeneratorNum ).ElecStorage.QairIntake + FuelCell( GeneratorNum ).Inverter.QairIntake;
			} else if ( SELECT_CASE_var == RecoverAuxiliaryBurner ) {
				FuelCell( GeneratorNum ).AirSup.QintakeRecovery = FuelCell( GeneratorNum ).AuxilHeat.QairIntake;
			} else if ( SELECT_CASE_var == RecoverInverterBatt ) {
				FuelCell( GeneratorNum ).AirSup.QintakeRecovery = FuelCell( GeneratorNum ).ElecStorage.QairIntake + FuelCell( GeneratorNum ).Inverter.QairIntake;
			} else if ( SELECT_CASE_var == RecoverInverter ) {
				FuelCell( GeneratorNum ).AirSup.QintakeRecovery = FuelCell( GeneratorNum ).Inverter.QairIntake;
			} else if ( SELECT_CASE_var == RecoverBattery ) {
				FuelCell( GeneratorNum ).AirSup.QintakeRecovery = FuelCell( GeneratorNum ).ElecStorage.QairIntake;
			} else if ( SELECT_CASE_var == NoRecoveryOnAirIntake ) {
				FuelCell( GeneratorNum ).AirSup.QintakeRecovery = 0.0;

			}}

			if ( FuelCell( GeneratorNum ).FCPM.NdotAir <= 0.0 ) { //just pass through, domain probably collapased in modeling
				FuelCell( GeneratorNum ).AirSup.TairIntoFCPM = FuelCell( GeneratorNum ).AirSup.TairIntoBlower;

			} else {
				FuelCell( GeneratorNum ).AirSup.TairIntoFCPM = ( ( ( 1 - FuelCell( GeneratorNum ).AirSup.BlowerHeatLossFactor ) * FuelCell( GeneratorNum ).AirSup.PairCompEl + FuelCell( GeneratorNum ).AirSup.QintakeRecovery ) / ( FuelCell( GeneratorNum ).FCPM.NdotAir * Cp * 1000.0 ) ) + FuelCell( GeneratorNum ).AirSup.TairIntoBlower; //1000 Cp units mol-> kmol
			}

			FuelCell( GeneratorNum ).AirSup.QskinLoss = FuelCell( GeneratorNum ).AirSup.BlowerHeatLossFactor * FuelCell( GeneratorNum ).AirSup.PairCompEl;

			if ( FuelCell( GeneratorNum ).AirSup.QskinLoss < 0.0 ) {
				//   write(*,*) 'problem in AirSup%QskinLoss ', FuelCell(GeneratorNum)%AirSup%QskinLoss
				ShowWarningError( "problem in AirSup%QskinLoss " + RoundSigDigits( FuelCell( GeneratorNum ).AirSup.QskinLoss, 3 ) );
				FuelCell( GeneratorNum ).AirSup.QskinLoss = 0.0;
			}

			FigureAirEnthalpy( GeneratorNum, FuelCell( GeneratorNum ).AirSup.TairIntoFCPM, Hmolair ); // (Hmolair in KJ/mol)

			// units, NdotAir in kmol/sec.; Hmolfuel in KJ/mol ,
			//        factor of 1000's to get to J/s or watts
			FuelCell( GeneratorNum ).FCPM.TotAirInEnthalphy = Hmolair * 1000.0 * FuelCell( GeneratorNum ).FCPM.NdotAir * 1000.0;

			// calculation Step 8, Figure Product Gases

			// figure stoic N dot for air
			NdotO2 = FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).StoicOxygenRate * FuelCell( GeneratorNum ).FCPM.NdotFuel;

			NdotStoicAir = NdotO2 / FuelCell( GeneratorNum ).AirSup.O2fraction;

			// figure excess air rate

			NdotExcessAir = FuelCell( GeneratorNum ).FCPM.NdotAir - NdotStoicAir;

			if ( NdotExcessAir < 0 ) { //can't meet stoichiometric fuel reaction

				ShowWarningError( "Air flow rate into fuel cell is too low for stoichiometric fuel reaction" );
				ShowContinueError( "Increase air flow in GENERATOR:FC:AIR SUPPLY object:" + FuelCell( GeneratorNum ).AirSup.Name );
			}

			// figure CO2 and Water rate from products (coefs setup during one-time processing in gas phase library )

			NdotCO2ProdGas = FuelCell( GeneratorNum ).FCPM.NdotFuel * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).CO2ProductGasCoef;

			NdotH20ProdGas = FuelCell( GeneratorNum ).FCPM.NdotFuel * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).H20ProductGasCoef;

			//  set product gas constituent fractions  (assume five usual components)
			NdotCO2 = 0.0;
			NdotN2 = 0.0;
			Ndot02 = 0.0;
			NdotH20 = 0.0;
			NdotAr = 0.0;

			// Product gas constiuents are fixed (not a user defined thing)

			for ( thisGas = 1; thisGas <= FuelCell( GeneratorNum ).AirSup.NumConstituents; ++thisGas ) {

				{ auto const SELECT_CASE_var( FuelCell( GeneratorNum ).AirSup.GasLibID( thisGas ) );

				if ( SELECT_CASE_var == 1 ) {
					// all the CO2 coming in plus the new CO2 from reactions
					NdotCO2 = NdotCO2ProdGas + FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisGas ) * FuelCell( GeneratorNum ).FCPM.NdotAir;

				} else if ( SELECT_CASE_var == 2 ) {
					// all the nitrogen comming in
					NdotN2 = FuelCell( GeneratorNum ).FCPM.NdotAir * FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisGas );

				} else if ( SELECT_CASE_var == 3 ) {
					// all the oxygen in the excess air stream
					Ndot02 = NdotExcessAir * FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisGas );

				} else if ( SELECT_CASE_var == 4 ) {
					// all the H20 comming in plus the new H20 from reactions and the H20 from water used in reforming
					NdotH20 = NdotH20ProdGas + FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisGas ) * FuelCell( GeneratorNum ).FCPM.NdotAir;
					//+ FuelCell(GeneratorNum)%FCPM%NdotLiqwater

				} else if ( SELECT_CASE_var == 5 ) {
					// all the argon coming in.
					NdotAr = FuelCell( GeneratorNum ).FCPM.NdotAir * FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisGas );

				} else {

				}}
			}

			FuelCell( GeneratorNum ).FCPM.NdotProdGas = NdotCO2 + NdotN2 + Ndot02 + NdotH20 + NdotAr;

			// now that we have the total, figure molar fractions

			FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( 1 ) = NdotCO2 / FuelCell( GeneratorNum ).FCPM.NdotProdGas;

			// all the nitrogen comming in
			FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( 2 ) = NdotN2 / FuelCell( GeneratorNum ).FCPM.NdotProdGas;

			// all the oxygen in the excess air stream
			FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( 3 ) = Ndot02 / FuelCell( GeneratorNum ).FCPM.NdotProdGas;

			// all the H20 comming in plus the new H20 from reactions and the H20 from water used in reforming
			FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( 4 ) = NdotH20 / FuelCell( GeneratorNum ).FCPM.NdotProdGas;

			// all the argon coming in.
			FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( 5 ) = NdotAr / FuelCell( GeneratorNum ).FCPM.NdotProdGas;

			//HmolProdGases KJ/mol)
			FigureProductGasesEnthalpy( GeneratorNum, FuelCell( GeneratorNum ).FCPM.TprodGasLeavingFCPM, HmolProdGases );

			// units, NdotProdGas in kmol/sec.; HmolProdGases in KJ/mol ,
			//        factor of 1000's to get to J/s or watts
			FuelCell( GeneratorNum ).FCPM.TotProdGasEnthalphy = HmolProdGases * 1000.0 * FuelCell( GeneratorNum ).FCPM.NdotProdGas * 1000.0;

			// calculation Step 9, Figure Skin lossess

			if ( FuelCell( GeneratorNum ).FCPM.SkinLossMode == ConstantRateSkinLoss ) {
				// do nothing just use QdotSkin

			} else if ( FuelCell( GeneratorNum ).FCPM.SkinLossMode == UADTSkinLoss ) {

				// get zone air temp
				FuelCell( GeneratorNum ).FCPM.QdotSkin = FuelCell( GeneratorNum ).FCPM.UAskin * ( FuelCell( GeneratorNum ).FCPM.TprodGasLeavingFCPM - ZT( FuelCell( GeneratorNum ).FCPM.ZoneID ) );

			} else if ( FuelCell( GeneratorNum ).FCPM.SkinLossMode == QuadraticFuelNdotSkin ) {

				FuelCell( GeneratorNum ).FCPM.QdotSkin = CurveValue( FuelCell( GeneratorNum ).FCPM.SkinLossCurveID, FuelCell( GeneratorNum ).FCPM.NdotFuel );

			}

			// calculation Step 10, AC FCPM power ancillaries

			FuelCell( GeneratorNum ).FCPM.PelancillariesAC = FuelCell( GeneratorNum ).FCPM.ANC0 + FuelCell( GeneratorNum ).FCPM.ANC1 * FuelCell( GeneratorNum ).FCPM.NdotFuel;

			// calculation Step 11, Dilution air
			FigureAirEnthalpy( GeneratorNum, FuelCell( GeneratorNum ).AirSup.TairIntoBlower, Hmolair ); // (Hmolair in KJ/mol)

			// units, NdotDilutionAir in kmol/sec.; Hmolair in KJ/mol ,
			//        factor of 1000's to get to J/s or watts
			FuelCell( GeneratorNum ).FCPM.DilutionAirInEnthalpy = Hmolair * 1000.0 * FuelCell( GeneratorNum ).FCPM.NdotDilutionAir * 1000.0;
			FuelCell( GeneratorNum ).FCPM.DilutionAirOutEnthalpy = FuelCell( GeneratorNum ).FCPM.DilutionAirInEnthalpy + FuelCell( GeneratorNum ).FCPM.StackHeatLossToDilution;

			// calculation Step 12, Calculate Reforming water out enthalpy
			FigureGaseousWaterEnthalpy( FuelCell( GeneratorNum ).FCPM.TprodGasLeavingFCPM, HGasWater );

			FuelCell( GeneratorNum ).FCPM.WaterOutEnthalpy = HGasWater * 1000.0 * FuelCell( GeneratorNum ).FCPM.NdotLiqwater * 1000.0;

			// calculation Step 13, Calculate Heat balance
			//    move all terms in Equation 7 to RHS and calculate imbalance

			MagofImbalance = -FuelCell( GeneratorNum ).FCPM.TotFuelInEnthalphy - FuelCell( GeneratorNum ).FCPM.TotAirInEnthalphy - FuelCell( GeneratorNum ).FCPM.WaterInEnthalpy - FuelCell( GeneratorNum ).FCPM.DilutionAirInEnthalpy - FuelCell( GeneratorNum ).FCPM.NdotFuel * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).LHV * 1000000.0 - FuelCell( GeneratorNum ).FCPM.PelancillariesAC + FuelCell( GeneratorNum ).FCPM.Pel + FuelCell( GeneratorNum ).FCPM.TotProdGasEnthalphy + FuelCell( GeneratorNum ).FCPM.WaterOutEnthalpy + FuelCell( GeneratorNum ).FCPM.QdotStackCool + FuelCell( GeneratorNum ).FCPM.QdotSkin + FuelCell( GeneratorNum ).FCPM.DilutionAirOutEnthalpy;

			// Now find a new total prod Gas Enthalphy that would result in an energy balance
			// TODO check signs...
			tmpTotProdGasEnthalphy = FuelCell( GeneratorNum ).FCPM.TotProdGasEnthalphy - MagofImbalance;

			// solve for a new TprodGasLeavingFCPM using regula falsi method

			Acc = 0.01; // guessing need to refine
			MaxIter = 150; // guessing need to refine
			SolverFlag = 0; //init
			Par( 1 ) = double( GeneratorNum );
			Par( 2 ) = tmpTotProdGasEnthalphy;
			Par( 3 ) = FuelCell( GeneratorNum ).FCPM.NdotProdGas;
			tmpTprodGas = FuelCell( GeneratorNum ).FCPM.TprodGasLeavingFCPM;
			SolveRegulaFalsi( Acc, MaxIter, SolverFlag, tmpTprodGas, FuelCellProductGasEnthResidual, MinProductGasTemp, MaxProductGasTemp, Par );

			if ( SolverFlag == -2 ) {

				ShowWarningError( "CalcFuelCellGeneratorModel: Regula falsi problem, flag = -2, check signs, all positive" );

			}
			if ( SolverFlag == -1 ) {
				ShowWarningError( "CalcFuelCellGeneratorModel: Regula falsi problem, flag = -1, check accuracy and iterations, did not converge" );

			}
			if ( SolverFlag > 0 ) {
				FuelCell( GeneratorNum ).FCPM.TprodGasLeavingFCPM = tmpTprodGas;
				//  write(*,*) 'Number of regula falsi iterations: ', solverFlag
			}

			//  moved call to HeatBalanceInternalGains.   Call FigureFuelCellZoneGains(GeneratorNum)

			// Control Step 3 determine interaction with electrical storage
			// How much power is really going into inverter?
			PintoInverter = Pel + Pstorage; // Back out so we can reapply
			ManageElectStorInteractions( GeneratorNum, Pdemand, PpcuLosses, ConstrainedStorage, Pstorage, PgridExtra );
			PintoInverter = Pel - Pstorage;
			// refine power conditioning losses with more current power production

			if ( FuelCell( GeneratorNum ).Inverter.EffMode == InverterEffConstant ) {

				PpcuLosses = ( 1.0 - FuelCell( GeneratorNum ).Inverter.ConstEff ) * PintoInverter;

			}

			if ( FuelCell( GeneratorNum ).Inverter.EffMode == InverterEffQuadratic ) {

				PpcuLosses = ( 1.0 - CurveValue( FuelCell( GeneratorNum ).Inverter.EffQuadraticCurveID, PintoInverter ) ) * PintoInverter;

			}

			PoutofInverter = PintoInverter - PpcuLosses;

			FuelCell( GeneratorNum ).ACPowerGen = PoutofInverter - FuelCell( GeneratorNum ).FCPM.PelancillariesAC - FuelCell( GeneratorNum ).AirSup.PairCompEl - FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).PfuelCompEl - FuelCell( GeneratorNum ).WaterSup.PwaterCompEl;
			FuelCell( GeneratorNum ).Inverter.PCUlosses = PpcuLosses;
			// model assumes air intake is drawn over power conditioner to recovery heat
			FuelCell( GeneratorNum ).Inverter.QairIntake = FuelCell( GeneratorNum ).Inverter.PCUlosses;

			CalcFuelCellAuxHeater( GeneratorNum );

			CalcFuelCellGenHeatRecovery( GeneratorNum );
			// calculation Step 11, If imbalance below threshold, then exit out of do loop.

			if ( ( std::abs( MagofImbalance ) < std::abs( ImBalanceTol * FuelCell( GeneratorNum ).FCPM.Pel ) ) && ( iter > 2 ) ) {
				break;
			}

		} //sequential substitution loop

		FuelCell( GeneratorNum ).FCPM.SeqSubstitIter = iter;
		FuelCell( GeneratorNum ).FCPM.RegulaFalsiIter = SolverFlag;

	}

	void
	ManageElectStorInteractions(
		int const Num, // Generator number, index for structure
		Real64 const Pdemand,
		Real64 const EP_UNUSED( PpcuLosses ),
		bool & Constrained,
		Real64 & Pstorage,
		Real64 & PgridOverage // electricity that can't be stored and needs to go out
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// manage controls and calculations related to electrical storage in FuelCell model

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpPdraw; // power draw from storage, working var
		Real64 tmpPcharge; // power charge to storage, working var
		bool drawing; // true if drawing power
		bool charging; // true if charging

		//initialize locals
		tmpPdraw = 0.0;
		tmpPcharge = 0.0;
		drawing = false;
		charging = false;
		Constrained = false;
		Pstorage = 0.0;
		PgridOverage = 0.0;

		// step 1 figure out what is desired of electrical storage system

		if ( FuelCell( Num ).FCPM.Pel < ( Pdemand ) ) {
			//draw from storage
			tmpPdraw = ( Pdemand ) - FuelCell( Num ).FCPM.Pel;
			drawing = true;
		}

		if ( FuelCell( Num ).FCPM.Pel > ( Pdemand ) ) {
			//add to storage
			tmpPcharge = FuelCell( Num ).FCPM.Pel - ( Pdemand );
			charging = true;

		}

		//  step 2, figure out what is possible for electrical storage draws/charges

		if ( charging ) {

			if ( FuelCell( Num ).ElecStorage.StorageModelMode == SimpleEffConstraints ) {

				if ( FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge >= FuelCell( Num ).ElecStorage.NominalEnergyCapacity ) {
					// storage full!  no more allowed!
					PgridOverage = tmpPcharge;
					tmpPcharge = 0.0;
					Constrained = true;
				}
				if ( tmpPcharge > FuelCell( Num ).ElecStorage.MaxPowerStore ) {
					PgridOverage = tmpPcharge - FuelCell( Num ).ElecStorage.MaxPowerStore;
					tmpPcharge = FuelCell( Num ).ElecStorage.MaxPowerStore;
					Constrained = true;
				}

				//now add energy to storage from charging
				if ( ( FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge + tmpPcharge * TimeStepSys * SecInHour * FuelCell( Num ).ElecStorage.EnergeticEfficCharge ) < FuelCell( Num ).ElecStorage.NominalEnergyCapacity ) {

					FuelCell( Num ).ElecStorage.ThisTimeStepStateOfCharge = FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge + tmpPcharge * TimeStepSys * SecInHour * FuelCell( Num ).ElecStorage.EnergeticEfficCharge;
				} else { // would over charge this time step

					tmpPcharge = ( FuelCell( Num ).ElecStorage.NominalEnergyCapacity - FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge ) / ( TimeStepSys * SecInHour * FuelCell( Num ).ElecStorage.EnergeticEfficCharge );
					Constrained = true;
					FuelCell( Num ).ElecStorage.ThisTimeStepStateOfCharge = FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge + tmpPcharge * TimeStepSys * SecInHour * FuelCell( Num ).ElecStorage.EnergeticEfficCharge;
				}

				//losses go into QairIntake
				FuelCell( Num ).ElecStorage.QairIntake = tmpPcharge * ( 1.0 - FuelCell( Num ).ElecStorage.EnergeticEfficCharge );

			} else if ( FuelCell( Num ).ElecStorage.StorageModelMode == LeadAcidBatterManwellMcGowan ) {
				ShowWarningError( "ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 " );

			} else if ( FuelCell( Num ).ElecStorage.StorageModelMode == LeadAcidBatterySaupe ) {
				ShowWarningError( "ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 " );

			} else {

				//should not come here
			}

			Pstorage = tmpPcharge;

		} //charging

		if ( drawing ) {
			if ( FuelCell( Num ).ElecStorage.StorageModelMode == SimpleEffConstraints ) {

				if ( FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge <= 0.0 ) {
					// storage empty  no more allowed!
					tmpPdraw = 0.0;
					Constrained = true;
					drawing = false;
				}
				if ( tmpPdraw > FuelCell( Num ).ElecStorage.MaxPowerDraw ) {
					tmpPdraw = FuelCell( Num ).ElecStorage.MaxPowerDraw;
					Constrained = true;
				}

				//now take energy from storage by drawing  (amplified by energetic effic)
				if ( ( FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge - tmpPdraw * TimeStepSys * SecInHour / FuelCell( Num ).ElecStorage.EnergeticEfficDischarge ) > 0.0 ) {

					FuelCell( Num ).ElecStorage.ThisTimeStepStateOfCharge = FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge - tmpPdraw * TimeStepSys * SecInHour / FuelCell( Num ).ElecStorage.EnergeticEfficDischarge;
				} else { //would over drain storage this timestep so reduce tmpPdraw
					tmpPdraw = FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge * FuelCell( Num ).ElecStorage.EnergeticEfficDischarge / ( TimeStepSys * SecInHour );
					FuelCell( Num ).ElecStorage.ThisTimeStepStateOfCharge = FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge - tmpPdraw * TimeStepSys * SecInHour / FuelCell( Num ).ElecStorage.EnergeticEfficDischarge;

					Constrained = true;
				}
				//losses go into QairIntake
				FuelCell( Num ).ElecStorage.QairIntake = tmpPdraw * ( 1.0 / FuelCell( Num ).ElecStorage.EnergeticEfficDischarge - 1.0 );
			} else if ( FuelCell( Num ).ElecStorage.StorageModelMode == LeadAcidBatterManwellMcGowan ) {
				ShowWarningError( "ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Manwell and McGowan 1993 " );

			} else if ( FuelCell( Num ).ElecStorage.StorageModelMode == LeadAcidBatterySaupe ) {
				ShowWarningError( "ManageElectStorInteractions: Not yet implemented: Lead Acid Battery By Saupe 1993 " );

			} else {

				//should not come here
			}

			Pstorage = -tmpPdraw;

		} //drawing

		if ( ( ! charging ) && ( ! drawing ) ) {

			FuelCell( Num ).ElecStorage.ThisTimeStepStateOfCharge = FuelCell( Num ).ElecStorage.LastTimeStepStateOfCharge;
			FuelCell( Num ).ElecStorage.PelNeedFromStorage = 0.0;
			FuelCell( Num ).ElecStorage.PelFromStorage = 0.0;
			FuelCell( Num ).ElecStorage.QairIntake = 0.0;
		}

		if ( Pstorage >= 0.0 ) {

			FuelCell( Num ).ElecStorage.PelIntoStorage = Pstorage;
			FuelCell( Num ).ElecStorage.PelFromStorage = 0.0;
		}
		if ( Pstorage < 0.0 ) {

			FuelCell( Num ).ElecStorage.PelIntoStorage = 0.0;
			FuelCell( Num ).ElecStorage.PelFromStorage = -Pstorage;

		}

	}

	Real64
	FuelCellProductGasEnthResidual(
		Real64 const TprodGas, // temperature, this is "x" being searched
		Array1< Real64 > const & Par // par(1) = Generator Number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith NREL
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Provide function for call to regula falsi search
		// Search for an product gas stream temperature that provides a
		// certain enthaply. (enthalpy is based on Shomate and can't be inverted)

		// METHODOLOGY EMPLOYED:
		// Calculates residual function for product gas enthalpy
		// calls procedure FigureProductGasesEnthalpy

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // F(x)

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// par(2) = Desired Enthalpy
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int GeneratorNum;
		Real64 thisHmolalProdGases;
		Real64 desiredHprodGases;
		Real64 NdotProdGases;

		GeneratorNum = std::floor( Par( 1 ) );
		desiredHprodGases = Par( 2 );
		NdotProdGases = Par( 3 );

		FigureProductGasesEnthalpy( GeneratorNum, TprodGas, thisHmolalProdGases );

		Residuum = ( thisHmolalProdGases * NdotProdGases * 1000000.0 ) - desiredHprodGases;

		return Residuum;

	}

	void
	FigureAirHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate Cp from Shomate equations for fuel

		// METHODOLOGY EMPLOYED:
		// sum by weighting molar fractions of all Air constituents.
		// assumes mixture is sum of parts.

		// REFERENCES:
		// NIST Webbook on gas phase thermochemistry

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
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempCp;
		int thisConstit; // loop index
		int gasID;
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff

		// loop through fuel constituents and sum up Cp

		// two different themodynamic curve fits might be used

		tempCp = 0.0;
		Tkel = ( FluidTemp + KelvinConv );
		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;

		for ( thisConstit = 1; thisConstit <= FuelCell( GeneratorNum ).AirSup.NumConstituents; ++thisConstit ) {
			gasID = FuelCell( GeneratorNum ).AirSup.GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {

					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;

					tempCp += ( ( A + B * Tsho + C * pow_2( Tsho ) + D * pow_3( Tsho ) + E / pow_2( Tsho ) ) * FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisConstit ) );
				}

				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {

					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;

					tempCp += ( A1 + A2 * Tkel + A3 * pow_2( Tkel ) + A4 * pow_3( Tkel ) + A5 * pow_4( Tkel ) ) * RinKJperMolpK * FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisConstit );

				}
			}
		}

		Cp = tempCp;

	}

	void
	FigureAirEnthalpy(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Hair // (kJ/mol)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate Enthalpy from Shomate equations for fuel

		// METHODOLOGY EMPLOYED:
		// sum by weighting molar fractions of all fuel constituents.
		// assumes mixture is sum of parts.

		// REFERENCES:
		// NIST Webbook on gas phase thermochemistry

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
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempHair;
		Real64 HairI;
		int thisConstit; // loop index
		int gasID; // look up into Gas structure
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 F; // shomate coeff
		Real64 H; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff
		Real64 A6; // NASA poly coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;
		Tkel = ( FluidTemp + KelvinConv );
		// loop through fuel constituents and sum up Cp

		tempHair = 0.0;

		for ( thisConstit = 1; thisConstit <= FuelCell( GeneratorNum ).AirSup.NumConstituents; ++thisConstit ) {
			gasID = FuelCell( GeneratorNum ).AirSup.GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {

					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;
					F = GasPhaseThermoChemistryData( gasID ).ShomateF;
					H = GasPhaseThermoChemistryData( gasID ).ShomateH;

					HairI = ( A * Tsho + B * pow_2( Tsho ) / 2.0 + C * pow_3( Tsho ) / 3.0 + D * pow_4( Tsho ) / 4.0 - E / Tsho + F - H );

					tempHair += HairI * FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisConstit );

				}
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {
					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;
					A6 = GasPhaseThermoChemistryData( gasID ).NASA_A6;

					tempHair += ( ( ( A1 + A2 * Tkel / 2.0 + A3 * pow_2( Tkel ) / 3.0 + A4 * pow_3( Tkel ) / 4.0 + A5 * pow_4( Tkel ) / 5.0 + A6 / Tkel ) * RinKJperMolpK * Tkel ) - GasPhaseThermoChemistryData( gasID ).StdRefMolarEnthOfForm ) * FuelCell( GeneratorNum ).AirSup.ConstitMolalFract( thisConstit );
				}
			}
		}

		Hair = tempHair;

	}

	void
	FigureFuelHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate Cp from Shomate equations for fuel

		// METHODOLOGY EMPLOYED:
		// sum by weighting molar fractions of all fuel constituents.
		// assumes mixture is sum of parts.

		// REFERENCES:
		// NIST Webbook on gas phase thermochemistry

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
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempCp;
		int thisConstit; // loop index
		int gasID; // look up into Gas structure
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;
		Tkel = ( FluidTemp + KelvinConv );
		// loop through fuel constituents and sum up Cp

		tempCp = 0.0;

		for ( thisConstit = 1; thisConstit <= FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).NumConstituents; ++thisConstit ) {
			gasID = FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {

					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;

					tempCp += ( ( A + B * Tsho + C * pow_2( Tsho ) + D * pow_3( Tsho ) + E / pow_2( Tsho ) ) * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).ConstitMolalFract( thisConstit ) );
				}

				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {
					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;

					tempCp += ( A1 + A2 * Tkel + A3 * pow_2( Tkel ) + A4 * pow_3( Tkel ) + A5 * pow_4( Tkel ) ) * RinKJperMolpK * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).ConstitMolalFract( thisConstit );

				}
			}
		}

		Cp = tempCp;

	}

	void
	FigureFuelEnthalpy(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Hfuel // kJ/mol
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate Enthalpy from Shomate equations for fuel

		// METHODOLOGY EMPLOYED:
		// sum by weighting molar fractions of all fuel constituents.
		// assumes mixture is sum of parts.

		// REFERENCES:
		// NIST Webbook on gas phase thermochemistry

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
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempHfuel;
		Real64 HfuelI;
		int thisConstit; // loop index
		int gasID; // look up into Gas structure
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 F; // shomate coeff
		Real64 H; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff
		Real64 A6; // NASA poly coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;
		Tkel = ( FluidTemp + KelvinConv );
		// loop through fuel constituents and sum up Cp

		tempHfuel = 0.0;

		for ( thisConstit = 1; thisConstit <= FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).NumConstituents; ++thisConstit ) {
			gasID = FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {
					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;
					F = GasPhaseThermoChemistryData( gasID ).ShomateF;
					H = GasPhaseThermoChemistryData( gasID ).ShomateH;

					HfuelI = ( A * Tsho + B * pow_2( Tsho ) / 2.0 + C * pow_3( Tsho ) / 3.0 + D * pow_4( Tsho ) / 4.0 - E / Tsho + F - H );

					tempHfuel += HfuelI * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).ConstitMolalFract( thisConstit );

				}
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {

					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;
					A6 = GasPhaseThermoChemistryData( gasID ).NASA_A6;

					tempHfuel += ( ( ( A1 + A2 * Tkel / 2.0 + A3 * pow_2( Tkel ) / 3.0 + A4 * pow_3( Tkel ) / 4.0 + A5 * pow_4( Tkel ) / 5.0 + A6 / Tkel ) * RinKJperMolpK * Tkel ) - GasPhaseThermoChemistryData( gasID ).StdRefMolarEnthOfForm ) * FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).ConstitMolalFract( thisConstit );
				}
			}
		}

		Hfuel = tempHfuel;

	}

	void
	FigureProductGasesEnthalpy(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & HProdGases // kJ/mol
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate Enthalpy from Shomate equations for gases

		// METHODOLOGY EMPLOYED:
		// sum by weighting molar fractions of all product gas constituents.
		// assumes mixture is sum of parts.

		// REFERENCES:
		// NIST Webbook on gas phase thermochemistry

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
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempHprodGases;
		int thisConstit; // loop index
		int gasID; // look up into Gas structure
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 F; // shomate coeff
		Real64 H; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff
		Real64 A6; // NASA poly coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;
		Tkel = ( FluidTemp + KelvinConv );
		// loop through fuel constituents and sum up Cp

		tempHprodGases = 0.0;

		for ( thisConstit = 1; thisConstit <= 5; ++thisConstit ) {
			gasID = FuelCell( GeneratorNum ).FCPM.GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {
					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;
					F = GasPhaseThermoChemistryData( gasID ).ShomateF;
					H = GasPhaseThermoChemistryData( gasID ).ShomateH;

					tempHprodGases += ( ( A * Tsho + B * pow_2( Tsho ) / 2.0 + C * pow_3( Tsho ) / 3.0 + D * pow_4( Tsho ) / 4.0 - E / Tsho + F - H ) * FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( thisConstit ) );
				}
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {
					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;
					A6 = GasPhaseThermoChemistryData( gasID ).NASA_A6;

					tempHprodGases += ( ( ( A1 + A2 * Tkel / 2.0 + A3 * pow_2( Tkel ) / 3.0 + A4 * pow_3( Tkel ) / 4.0 + A5 * pow_4( Tkel ) / 5.0 + A6 / Tkel ) * RinKJperMolpK * Tkel ) - GasPhaseThermoChemistryData( gasID ).StdRefMolarEnthOfForm ) * FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( thisConstit );
				}
			} // gasid > 0
		}

		HProdGases = tempHprodGases;

	}

	void
	FigureProductGasHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug. 2005
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
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempCp;
		int thisConstit; // loop index
		int gasID; // look up into Gas structure
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;
		Tkel = ( FluidTemp + KelvinConv );
		// loop through fuel constituents and sum up Cp

		tempCp = 0.0;

		for ( thisConstit = 1; thisConstit <= isize( FuelCell( GeneratorNum ).FCPM.GasLibID ); ++thisConstit ) {
			gasID = FuelCell( GeneratorNum ).FCPM.GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {

					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;

					tempCp += ( ( A + B * Tsho + C * pow_2( Tsho ) + D * pow_3( Tsho ) + E / pow_2( Tsho ) ) * FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( thisConstit ) );
				}

				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {
					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;

					tempCp += ( A1 + A2 * Tkel + A3 * pow_2( Tkel ) + A4 * pow_3( Tkel ) + A5 * pow_4( Tkel ) ) * RinKJperMolpK * FuelCell( GeneratorNum ).FCPM.ConstitMolalFract( thisConstit );

				}

			}

		}

		Cp = tempCp;

	}

	void
	FigureAuxilHeatGasHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug. 2005
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
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempCp;
		int thisConstit; // loop index
		int gasID; // look up into Gas structure
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;
		Tkel = ( FluidTemp + KelvinConv );
		// loop through fuel constituents and sum up Cp

		tempCp = 0.0;

		for ( thisConstit = 1; thisConstit <= isize( FuelCell( GeneratorNum ).AuxilHeat.GasLibID ); ++thisConstit ) {
			gasID = FuelCell( GeneratorNum ).AuxilHeat.GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {

					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;

					tempCp += ( ( A + B * Tsho + C * pow_2( Tsho ) + D * pow_3( Tsho ) + E / pow_2( Tsho ) ) * FuelCell( GeneratorNum ).AuxilHeat.ConstitMolalFract( thisConstit ) );
				}

				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {
					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;

					tempCp += ( A1 + A2 * Tkel + A3 * pow_2( Tkel ) + A4 * pow_3( Tkel ) + A5 * pow_4( Tkel ) ) * RinKJperMolpK * FuelCell( GeneratorNum ).AuxilHeat.ConstitMolalFract( thisConstit );

				}

			}

		}

		Cp = tempCp;

	}

	void
	FigureHXleavingGasHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug. 2005
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
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 Tkel; // temp for NASA eq. in Kelvin
		Real64 tempCp;
		int thisConstit; // loop index
		int gasID; // look up into Gas structure
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 A1; // NASA poly coeff
		Real64 A2; // NASA poly coeff
		Real64 A3; // NASA poly coeff
		Real64 A4; // NASA poly coeff
		Real64 A5; // NASA poly coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;
		Tkel = ( FluidTemp + KelvinConv );
		// loop through fuel constituents and sum up Cp

		tempCp = 0.0;

		for ( thisConstit = 1; thisConstit <= isize( FuelCell( GeneratorNum ).ExhaustHX.GasLibID ); ++thisConstit ) {
			gasID = FuelCell( GeneratorNum ).ExhaustHX.GasLibID( thisConstit );
			if ( gasID > 0 ) {
				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NISTShomate ) {

					A = GasPhaseThermoChemistryData( gasID ).ShomateA;
					B = GasPhaseThermoChemistryData( gasID ).ShomateB;
					C = GasPhaseThermoChemistryData( gasID ).ShomateC;
					D = GasPhaseThermoChemistryData( gasID ).ShomateD;
					E = GasPhaseThermoChemistryData( gasID ).ShomateE;

					tempCp += ( ( A + B * Tsho + C * pow_2( Tsho ) + D * pow_3( Tsho ) + E / pow_2( Tsho ) ) * FuelCell( GeneratorNum ).ExhaustHX.ConstitMolalFract( thisConstit ) );
				}

				if ( GasPhaseThermoChemistryData( gasID ).ThermoMode == NASAPolynomial ) {
					A1 = GasPhaseThermoChemistryData( gasID ).NASA_A1;
					A2 = GasPhaseThermoChemistryData( gasID ).NASA_A2;
					A3 = GasPhaseThermoChemistryData( gasID ).NASA_A3;
					A4 = GasPhaseThermoChemistryData( gasID ).NASA_A4;
					A5 = GasPhaseThermoChemistryData( gasID ).NASA_A5;

					tempCp += ( A1 + A2 * Tkel + A3 * pow_2( Tkel ) + A4 * pow_3( Tkel ) + A5 * pow_4( Tkel ) ) * RinKJperMolpK * FuelCell( GeneratorNum ).ExhaustHX.ConstitMolalFract( thisConstit );

				}

			}

		}

		Cp = tempCp;

	}

	void
	FigureGaseousWaterEnthalpy(
		Real64 const FluidTemp, // degree C
		Real64 & HGasWater // kJ/mol
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B griffith
		//       DATE WRITTEN   December 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate Enthalpy from Shomate equations for gaseous water
		// No ethalphy of formation in this one.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// NIST Webbook on gas phase thermochemistry

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
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 F; // shomate coeff
		//  REAL(r64) :: H ! shomate coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;

		A = 29.0373;
		B = 10.2573;
		C = 2.81048;
		D = -0.95914;
		E = 0.11725;
		F = -250.569;

		HGasWater = A * Tsho + B * pow_2( Tsho ) / 2.0 + C * pow_3( Tsho ) / 3.0 + D * pow_4( Tsho ) / 4.0 - E / Tsho + F; //- H

	}

	void
	FigureLiquidWaterEnthalpy(
		Real64 const FluidTemp, // degree C
		Real64 & HLiqWater // kJ/mol
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B griffith
		//       DATE WRITTEN   December 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate Enthalpy from Shomate equations for liquid water
		// No enthalpy of formation in this one

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// NIST Webbook on gas phase thermochemistry

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
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff
		Real64 F; // shomate coeff
		Real64 H; // shomate coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;

		A = -203.606;
		B = 1523.29;
		C = -3196.413;
		D = 2474.455;
		E = 3.85533;
		F = -256.5478;
		H = -285.8304;

		HLiqWater = A * Tsho + B * pow_2( Tsho ) / 2.0 + C * pow_3( Tsho ) / 3.0 + D * pow_4( Tsho ) / 4.0 - E / Tsho + F; //- H

	}

	void
	FigureLiquidWaterHeatCap(
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   December 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate shomate eq. for pure liquid water

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tsho; // temp for Shomate eq  in (Kelvin/1000)
		Real64 A; // shomate coeff
		Real64 B; // shomate coeff
		Real64 C; // shomate coeff
		Real64 D; // shomate coeff
		Real64 E; // shomate coeff

		Tsho = ( FluidTemp + KelvinConv ) / 1000.0;

		A = -203.606;
		B = 1523.29;
		C = -3196.413;
		D = 2474.455;
		E = 3.85533;

		Cp = A + B * Tsho + C * pow_2( Tsho ) + D * pow_3( Tsho ) + E / pow_2( Tsho );

	}

	void
	FigureLHVofFuel(
		int const Num,
		Real64 const NdotFuel,
		Real64 const NdotCO2,
		Real64 const NdotH20,
		Real64 & LHV
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate LHV

		// METHODOLOGY EMPLOYED:
		// ANNEX 42 eq. 6 method from molar enthalpies

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
		Real64 DelfHfuel;
		Real64 DelfHCO2;
		Real64 DelfHH20;
		int i;
		Real64 h_i;
		int CO2dataID;
		int WaterDataID;
		int thisGasID;

		CO2dataID = 1; //hard-coded in SetupFuelAndAirConstituentData
		WaterDataID = 4; //hard-coded in SetupFuelAndAirConstituentData
		DelfHfuel = 0.0;

		for ( i = 1; i <= FuelSupply( FuelCell( Num ).FuelSupNum ).NumConstituents; ++i ) {
			thisGasID = FuelSupply( FuelCell( Num ).FuelSupNum ).GasLibID( i );

			h_i = GasPhaseThermoChemistryData( thisGasID ).StdRefMolarEnthOfForm;

			DelfHfuel += NdotFuel * h_i * FuelSupply( FuelCell( Num ).FuelSupNum ).ConstitMolalFract( i );

		}

		DelfHCO2 = GasPhaseThermoChemistryData( CO2dataID ).StdRefMolarEnthOfForm * NdotCO2;

		DelfHH20 = GasPhaseThermoChemistryData( WaterDataID ).StdRefMolarEnthOfForm * NdotH20;

		LHV = ( DelfHfuel - DelfHCO2 - DelfHH20 ) / NdotFuel; //Equation 6

	}

	void
	FigureACAncillaries(
		int const GeneratorNum,
		Real64 & PacAncill
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   March 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the AC ancillaries to determine Pel

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		//  Using lagged values inside a sequential substitution loop
		PacAncill = 0.0;
		// sect. 5.9
		FuelCell( GeneratorNum ).FCPM.PelancillariesAC = FuelCell( GeneratorNum ).FCPM.ANC0 + FuelCell( GeneratorNum ).FCPM.ANC1 * FuelCell( GeneratorNum ).FCPM.NdotFuel;

		// sect 6.0
		FuelCell( GeneratorNum ).AirSup.PairCompEl = CurveValue( FuelCell( GeneratorNum ).AirSup.BlowerPowerCurveID, FuelCell( GeneratorNum ).FCPM.NdotAir );
		// sect 7.0
		FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).PfuelCompEl = CurveValue( FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).CompPowerCurveID, FuelCell( GeneratorNum ).FCPM.NdotFuel );

		// sect. 8.0
		FuelCell( GeneratorNum ).WaterSup.PwaterCompEl = CurveValue( FuelCell( GeneratorNum ).WaterSup.PmpPowerCurveID, FuelCell( GeneratorNum ).FCPM.NdotLiqwater );

		PacAncill = FuelCell( GeneratorNum ).FCPM.PelancillariesAC + FuelCell( GeneratorNum ).AirSup.PairCompEl + FuelSupply( FuelCell( GeneratorNum ).FuelSupNum ).PfuelCompEl + FuelCell( GeneratorNum ).WaterSup.PwaterCompEl;

	}

	void
	FigurePowerConditioningLosses(
		int const GeneratorNum,
		Real64 const Pdemand,
		Real64 & PpcuLosses
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate inverter losses

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 lastPpcuLosses; // used in iterative solution
		int iter;
		Real64 Pel;

		if ( FuelCell( GeneratorNum ).Inverter.EffMode == InverterEffConstant ) {

			PpcuLosses = Pdemand * ( 1 - FuelCell( GeneratorNum ).Inverter.ConstEff ) / FuelCell( GeneratorNum ).Inverter.ConstEff;

		}

		if ( FuelCell( GeneratorNum ).Inverter.EffMode == InverterEffQuadratic ) {

			// first use Pdemand instead of Pel to get initial estimate
			lastPpcuLosses = Pdemand * ( 1.0 - CurveValue( FuelCell( GeneratorNum ).Inverter.EffQuadraticCurveID, Pdemand ) ) / CurveValue( FuelCell( GeneratorNum ).Inverter.EffQuadraticCurveID, Pdemand );

			for ( iter = 1; iter <= 20; ++iter ) { // seems like need to iterate (??) Need to investigate number and convergence success here

				Pel = Pdemand + lastPpcuLosses;

				lastPpcuLosses = ( 1.0 - CurveValue( FuelCell( GeneratorNum ).Inverter.EffQuadraticCurveID, Pel ) ) * Pel;

			}

			PpcuLosses = lastPpcuLosses;

		}

	}

	void
	FigureTransientConstraints(
		int const GeneratorNum, // index number for accessing correct generator
		Real64 & Pel, // DC power control setting for power module
		bool & Constrained, // true if transient constraints kick in
		Real64 & PelDiff // if constrained then this is the difference, positive
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2005
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
		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused  REAL(r64)  :: CurrentHours
		Real64 CurrentFractionalDay; // working var, time in decimal days
		Real64 EndingFractionalDay; // working var, time is decimal days
		Real64 MaxPel; // working variable for max allowed by transient constraint
		Real64 MinPel; // working variabel for min allowed by transient constraint
		Real64 PelInput; // hold initial value of inout var

		PelInput = Pel;

		// Check if in start up and if it still should be
		if ( FuelCell( GeneratorNum ).FCPM.DuringStartUp ) {

			//calculate time for end of start up period
			CurrentFractionalDay = double( DayOfSim ) + ( int( CurrentTime ) + ( SysTimeElapsed + ( CurrentTime - int( CurrentTime ) ) ) ) / HoursInDay;

			EndingFractionalDay = FuelCell( GeneratorNum ).FCPM.FractionalDayofLastStartUp + FuelCell( GeneratorNum ).FCPM.StartUpTime / HoursInDay;

			if ( CurrentFractionalDay > EndingFractionalDay ) {
				//start up period is now over
				FuelCell( GeneratorNum ).FCPM.DuringStartUp = false;
			}
		}

		// Check if in shut down up and if it still should be
		if ( FuelCell( GeneratorNum ).FCPM.DuringShutDown ) {

			//calculate time for end of shut down period
			CurrentFractionalDay = double( DayOfSim ) + ( int( CurrentTime ) + ( SysTimeElapsed + ( CurrentTime - int( CurrentTime ) ) ) ) / HoursInDay;

			EndingFractionalDay = FuelCell( GeneratorNum ).FCPM.FractionalDayofLastShutDown + FuelCell( GeneratorNum ).FCPM.ShutDownTime / HoursInDay;

			if ( CurrentFractionalDay > EndingFractionalDay ) {
				//start up period is now over
				FuelCell( GeneratorNum ).FCPM.DuringShutDown = false;
			}
		}
		//compare

		if ( ! ( FuelCell( GeneratorNum ).FCPM.DuringShutDown ) && ! ( FuelCell( GeneratorNum ).FCPM.DuringStartUp ) ) {
			//unit is neither starting or stopping and the only constraints would come from transient limits
			if ( Pel > FuelCell( GeneratorNum ).FCPM.PelLastTimeStep ) { // powering up
				MaxPel = FuelCell( GeneratorNum ).FCPM.PelLastTimeStep + FuelCell( GeneratorNum ).FCPM.UpTranLimit * TimeStepSys * SecInHour;
				if ( MaxPel < Pel ) {
					Pel = MaxPel;
					Constrained = true;
				} else {
					Constrained = false;
				}
			} else if ( Pel < FuelCell( GeneratorNum ).FCPM.PelLastTimeStep ) { //powering down
				MinPel = FuelCell( GeneratorNum ).FCPM.PelLastTimeStep - FuelCell( GeneratorNum ).FCPM.DownTranLimit * TimeStepSys * SecInHour;
				if ( Pel < MinPel ) {
					Pel = MinPel;
					Constrained = true;
				} else {
					Constrained = false;
				}
			} else { //the same
				//do nothing
				Constrained = false;
			}

		} //not in start up or shut down

		if ( FuelCell( GeneratorNum ).FCPM.DuringStartUp ) {
			//constant during start up modeling artifact
			Pel = FuelCell( GeneratorNum ).FCPM.StartUpElectProd / FuelCell( GeneratorNum ).FCPM.StartUpTime;
			Constrained = true;
		}

		if ( FuelCell( GeneratorNum ).FCPM.DuringShutDown ) {

			Pel = 0.0; // assumes no power generated during shut down
			Constrained = true;
		}

		PelDiff = 0.0;
		if ( Constrained ) {
			PelDiff = PelInput - Pel;
		}

	}

	void
	CalcFuelCellAuxHeater( int const Num ) // Generator number
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
		// na

		//not yet implemented, just pass product gases thru nul domain

		FuelCell( Num ).AuxilHeat.TauxMix = FuelCell( Num ).FCPM.TprodGasLeavingFCPM;
		FuelCell( Num ).AuxilHeat.NdotAuxMix = FuelCell( Num ).FCPM.NdotProdGas;
		FuelCell( Num ).AuxilHeat.ConstitMolalFract = FuelCell( Num ).FCPM.ConstitMolalFract;
		FuelCell( Num ).AuxilHeat.GasLibID = FuelCell( Num ).FCPM.GasLibID;

	}

	void
	CalcFuelCellGenHeatRecovery( int const Num ) // Generator number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Brent Griffith
		//       DATE WRITTEN:    Aug. 2005

		// PURPOSE OF THIS SUBROUTINE:
		// To perform heat recovery calculations and node updates

		// METHODOLOGY EMPLOYED:
		// model exhaust gas to water heat exchanger

		// REFERENCES: Annex 42 model documentation

		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcFuelCellGenHeatRecovery" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 eHX; // fixed effectiveness
		Real64 MdotWater( 0.0 );
		int inNodeNum( 0 );
		Real64 MWwater;
		Real64 NdotWater;
		Real64 TwaterIn;
		Real64 CpWaterMol;
		Real64 NdotGas;
		Real64 TprodGasIn;
		Real64 CpProdGasMol;
		Real64 NdotCp;
		Real64 qHX( 0.0 );
		Real64 UAeff;
		Real64 TauxMix;
		Real64 NdotCpWater;
		Real64 NdotCpAuxMix;
		Real64 THXexh( 0.0 );
		Real64 TwaterOut( 0.0 );
		Real64 hgas;
		Real64 hwater;
		static Real64 waterFract( 0.0 );
		Real64 NdotWaterVapor;
		Real64 TcondThresh;
		Real64 hxl1;
		Real64 hxl2;
		static Real64 NdotWaterCond( 0.0 );
		Real64 hfpwater;
		int i;

		Real64 qSens;
		Real64 qLatent;
		int loop;
		Real64 Cp;

		{ auto const SELECT_CASE_var( FuelCell( Num ).ExhaustHX.HXmodelMode );

		if ( SELECT_CASE_var == FixedEffectiveness ) { //Method 1

			eHX = FuelCell( Num ).ExhaustHX.HXEffect;

			inNodeNum = FuelCell( Num ).ExhaustHX.WaterInNode;

			MdotWater = FuelCell( Num ).ExhaustHX.WaterMassFlowRate;
			MWwater = GasPhaseThermoChemistryData( 4 ).MolecularWeight;
			NdotWater = MdotWater / MWwater;
			TwaterIn = FuelCell( Num ).ExhaustHX.WaterInletTemp;

			FigureLiquidWaterHeatCap( TwaterIn, CpWaterMol );

			NdotGas = FuelCell( Num ).AuxilHeat.NdotAuxMix;
			TprodGasIn = FuelCell( Num ).AuxilHeat.TauxMix;
			FigureAuxilHeatGasHeatCap( Num, TprodGasIn, CpProdGasMol ); // Cp in (J/mol*K)
			//factor of 1000.0 for kmol -> mol
			NdotCp = min( NdotGas * CpProdGasMol * 1000.0, NdotWater * CpWaterMol * 1000.0 );

			qHX = eHX * NdotCp * ( TprodGasIn - TwaterIn );

			THXexh = TprodGasIn - qHX / ( NdotGas * CpProdGasMol * 1000.0 );

			Cp = GetSpecificHeatGlycol( PlantLoop( FuelCell( Num ).CWLoopNum ).FluidName, TwaterIn, PlantLoop( FuelCell( Num ).CWLoopNum ).FluidIndex, RoutineName );

			if ( MdotWater * Cp <= 0.0 ) {
				TwaterOut = TwaterIn;
			} else {
				TwaterOut = TwaterIn + qHX / ( MdotWater * Cp );
			}

		} else if ( SELECT_CASE_var == LMTDempiricalUAeff ) { //method 2
			inNodeNum = FuelCell( Num ).ExhaustHX.WaterInNode;
			MdotWater = FuelCell( Num ).ExhaustHX.WaterMassFlowRate;
			MWwater = GasPhaseThermoChemistryData( 4 ).MolecularWeight;
			NdotWater = MdotWater / MWwater;
			NdotGas = FuelCell( Num ).AuxilHeat.NdotAuxMix;

			UAeff = FuelCell( Num ).ExhaustHX.hxs0 + FuelCell( Num ).ExhaustHX.hxs1 * NdotWater + FuelCell( Num ).ExhaustHX.hxs2 * pow_2( NdotWater ) + FuelCell( Num ).ExhaustHX.hxs3 * NdotGas + FuelCell( Num ).ExhaustHX.hxs4 * pow_2( NdotGas );

			TauxMix = FuelCell( Num ).AuxilHeat.TauxMix;
			TwaterIn = FuelCell( Num ).ExhaustHX.WaterInletTemp;
			FigureLiquidWaterHeatCap( TwaterIn, CpWaterMol );
			//factor of 1000.0 for kmol -> mol
			NdotCpWater = NdotWater * CpWaterMol * 1000.0;
			FigureAuxilHeatGasHeatCap( Num, TauxMix, CpProdGasMol ); // Cp in (J/mol*K)
			NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

			// commented out protection for taking exponent of too large a number
			//   because it hasn't been a problem in testing
			//testVal = LOG(huge(NdotCpAuxMix))
			//ExpTestVal = 700.0
			//IF (UAeff*(1/NdotCpAuxMix) > ExpTestVal) THEN
			// write(*,*) 'Houston, we have a problem, EXP [] func will fail for UAeff*(1/NdotCpAuxMix):', UAeff*(1/NdotCpAuxMix)
			// ELSEIF (UAeff*(1/NdotCpWater) > ExpTestVal) THEN
			//   write(*,*) 'Houston, we have a problem, EXP [] func will fail for UAeff*(1/NdotCpWater:', UAeff*(1/NdotCpWater)
			//  ELSE

			if ( ( NdotCpWater != 0.0 ) && ( NdotCpAuxMix != 0.0 ) ) { // trap divide by zero
				// now evaluate Eq. 44
				THXexh = ( ( 1.0 - NdotCpAuxMix / NdotCpWater ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - NdotCpAuxMix / NdotCpWater ) ) * TauxMix + ( ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - 1.0 ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - NdotCpAuxMix / NdotCpWater ) ) * TwaterIn;

				TwaterOut = TwaterIn + ( NdotCpAuxMix / NdotCpWater ) * ( TauxMix - THXexh ); // Eq. 42

			} else {
				THXexh = TauxMix;
				TwaterOut = TwaterIn;
			}
			// ENDIF

			if ( ( THXexh - TwaterIn ) != 0.0 ) { // trap divide by zero
				qHX = UAeff * ( ( TauxMix - TwaterOut ) - ( THXexh - TwaterIn ) ) / std::log( ( TauxMix - TwaterOut ) / ( THXexh - TwaterIn ) );
			} else {
				qHX = 0.0;
			}

		} else if ( SELECT_CASE_var == LMTDfundementalUAeff ) { //method 3
			NdotGas = FuelCell( Num ).AuxilHeat.NdotAuxMix;
			inNodeNum = FuelCell( Num ).ExhaustHX.WaterInNode;
			MdotWater = FuelCell( Num ).ExhaustHX.WaterMassFlowRate;
			MWwater = GasPhaseThermoChemistryData( 4 ).MolecularWeight;
			NdotWater = MdotWater / MWwater;

			hgas = FuelCell( Num ).ExhaustHX.h0gas * std::pow( NdotGas / FuelCell( Num ).ExhaustHX.NdotGasRef, FuelCell( Num ).ExhaustHX.nCoeff ); //Eq. 48

			hwater = FuelCell( Num ).ExhaustHX.h0Water * std::pow( NdotWater / FuelCell( Num ).ExhaustHX.NdotWaterRef, FuelCell( Num ).ExhaustHX.mCoeff ); //Eq. 48

			// now equation 47
			UAeff = 1.0 / ( 1.0 / ( hgas * FuelCell( Num ).ExhaustHX.AreaGas ) + 1.0 / ( hwater * FuelCell( Num ).ExhaustHX.AreaWater ) + FuelCell( Num ).ExhaustHX.Fadjust );

			TauxMix = FuelCell( Num ).AuxilHeat.TauxMix;
			TwaterIn = FuelCell( Num ).ExhaustHX.WaterInletTemp;
			FigureLiquidWaterHeatCap( TwaterIn, CpWaterMol );
			NdotCpWater = NdotWater * CpWaterMol * 1000.0;
			FigureAuxilHeatGasHeatCap( Num, TauxMix, CpProdGasMol ); // Cp in (J/mol*K)
			NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

			if ( ( NdotCpWater != 0.0 ) && ( NdotCpAuxMix != 0.0 ) ) { // trap divide by zero
				// now evaluate Eq. 44
				THXexh = ( ( 1.0 - NdotCpAuxMix / NdotCpWater ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - NdotCpAuxMix / NdotCpWater ) ) * TauxMix + ( ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - 1.0 ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - NdotCpAuxMix / NdotCpWater ) ) * TwaterIn;

				TwaterOut = TwaterIn + ( NdotCpAuxMix / NdotCpWater ) * ( TauxMix - THXexh ); // Eq. 42

			} else {
				THXexh = TauxMix;
				TwaterOut = TwaterIn;
			}

			if ( ( THXexh - TwaterIn ) != 0.0 ) { // trap divide by zero
				qHX = UAeff * ( ( TauxMix - TwaterOut ) - ( THXexh - TwaterIn ) ) / std::log( ( TauxMix - TwaterOut ) / ( THXexh - TwaterIn ) );
			} else {
				qHX = 0.0;
			}

		} else if ( SELECT_CASE_var == Condensing ) { //method 4
			inNodeNum = FuelCell( Num ).ExhaustHX.WaterInNode;
			MdotWater = FuelCell( Num ).ExhaustHX.WaterMassFlowRate;
			if ( MdotWater != 0.0 ) {

				MWwater = GasPhaseThermoChemistryData( 4 ).MolecularWeight;
				NdotWater = MdotWater / MWwater;
				NdotGas = FuelCell( Num ).AuxilHeat.NdotAuxMix;

				UAeff = FuelCell( Num ).ExhaustHX.hxs0 + FuelCell( Num ).ExhaustHX.hxs1 * NdotWater + FuelCell( Num ).ExhaustHX.hxs2 * pow_2( NdotWater ) + FuelCell( Num ).ExhaustHX.hxs3 * NdotGas + FuelCell( Num ).ExhaustHX.hxs4 * pow_2( NdotGas );

				TauxMix = FuelCell( Num ).AuxilHeat.TauxMix;
				TwaterIn = FuelCell( Num ).ExhaustHX.WaterInletTemp;
				FigureLiquidWaterHeatCap( TwaterIn, CpWaterMol );
				NdotCpWater = NdotWater * CpWaterMol * 1000.0;
				FigureAuxilHeatGasHeatCap( Num, TauxMix, CpProdGasMol ); // Cp in (J/mol*K)
				NdotCpAuxMix = NdotGas * CpProdGasMol * 1000.0;

				//find water fraction in incoming gas stream
				for ( i = 1; i <= isize( FuelCell( Num ).AuxilHeat.GasLibID ); ++i ) {
					if ( FuelCell( Num ).AuxilHeat.GasLibID( i ) == 4 ) waterFract = FuelCell( Num ).AuxilHeat.ConstitMolalFract( i );
				}
				NdotWaterVapor = waterFract * NdotGas;

				TcondThresh = FuelCell( Num ).ExhaustHX.CondensationThresholdTemp;
				hxl1 = FuelCell( Num ).ExhaustHX.l1Coeff;
				hxl2 = FuelCell( Num ).ExhaustHX.l2Coeff;

				NdotWaterCond = ( TcondThresh - TwaterIn ) * ( hxl1 * ( NdotWaterVapor / NdotGas ) + hxl2 * pow_2( NdotWaterVapor / NdotGas ) );

				if ( NdotWaterCond < 0.0 ) NdotWaterCond = 0.0;

				hfpwater = 4.4004e+07; // molal heat of vaporization of water J/kmol)

				if ( ( NdotCpWater != 0.0 ) && ( NdotCpAuxMix != 0.0 ) ) { // trap divide by zero

					// now evaluate Eq. 44
					THXexh = ( ( 1.0 - NdotCpAuxMix / NdotCpWater ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - NdotCpAuxMix / NdotCpWater ) ) * TauxMix + ( ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - 1.0 ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) - NdotCpAuxMix / NdotCpWater ) ) * TwaterIn;

					TwaterOut = TwaterIn + ( NdotCpAuxMix / NdotCpWater ) * ( TauxMix - THXexh ) + ( NdotWaterCond * hfpwater ) / NdotCpWater;

					if ( NdotWaterCond > 0 ) { // Eq. 44 is not correct. use its result as first guess for revised way...

						for ( loop = 1; loop <= 5; ++loop ) { // iterative soluion because in condensing case THXexh is function of qSens and qLatent

							if ( ( THXexh - TwaterIn ) != 0.0 ) { // trap divide by zero
								qSens = UAeff * ( ( TauxMix - TwaterOut ) - ( THXexh - TwaterIn ) ) / std::log( ( TauxMix - TwaterOut ) / ( THXexh - TwaterIn ) );
							} else {
								qSens = 0.0;
							}
							qLatent = NdotWaterCond * hfpwater;
							if ( qSens > 0 ) {
								THXexh = TauxMix * ( ( 1.0 - NdotCpAuxMix / NdotCpWater ) / ( ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) / ( std::exp( ( UAeff * qLatent ) / ( NdotCpWater * qSens ) ) ) ) - NdotCpAuxMix / NdotCpWater ) ) + TwaterIn * ( ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) / ( std::exp( ( UAeff * qLatent ) / ( NdotCpWater * qSens ) ) ) - 1.0 ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) / ( std::exp( ( UAeff * qLatent ) / ( NdotCpWater * qSens ) ) ) - NdotCpAuxMix / NdotCpWater ) ) - ( ( qLatent / NdotCpWater ) / ( std::exp( UAeff * ( 1.0 / NdotCpAuxMix - 1.0 / NdotCpWater ) ) / ( std::exp( ( UAeff * qLatent ) / ( NdotCpWater * qSens ) ) ) - NdotCpAuxMix / NdotCpWater ) );
							} else {
								THXexh = TauxMix;
							}

							TwaterOut = TwaterIn + ( NdotCpAuxMix / NdotCpWater ) * ( TauxMix - THXexh ) + ( NdotWaterCond * hfpwater ) / NdotCpWater;

						}

					}

				} else {
					THXexh = TauxMix;
					TwaterOut = TwaterIn;

				}

				if ( ( THXexh - TwaterIn ) != 0.0 ) { // trap divide by zero

					qHX = UAeff * ( ( TauxMix - TwaterOut ) - ( THXexh - TwaterIn ) ) / std::log( ( TauxMix - TwaterOut ) / ( THXexh - TwaterIn ) ) + NdotWaterCond * hfpwater;
				} else {
					qHX = 0.0;
				}
			} else { //no cooling water flow, model will blow up.
				qHX = 0.0;
				THXexh = FuelCell( Num ).AuxilHeat.TauxMix;
				TwaterOut = FuelCell( Num ).ExhaustHX.WaterInletTemp;
				NdotWaterCond = 0.0;
				waterFract = -9999.0; // not defined

			}
			//init input from Auxiliary heater
			// FuelCell(Num)%ExhaustHX%NdotHXleaving      = FuelCell(Num)%AuxilHeat%NdotAuxMix
			// FuelCell(Num)%ExhaustHX%ConstitMolalFract  = FuelCell(Num)%AuxilHeat%ConstitMolalFract
			// FuelCell(Num)%ExhaustHX%GasLibID           = FuelCell(Num)%AuxilHeat%GasLibID

			// now modify leaving gas constituents for condensed water.
			// FuelCell(Num)%ExhaustHX%NdotHXleaving = FuelCell(Num)%AuxilHeat%NdotAuxMix - NdotWaterCond
			// If ( FuelCell(Num)%ExhaustHX%NdotHXleaving > 0) then
			//   DO I = 1, SIZE(FuelCell(Num)%AuxilHeat%GasLibID)
			//     If (FuelCell(Num)%AuxilHeat%GasLibID(I) == 4) then ! water constituent
			//       FuelCell(Num)%ExhaustHX%ConstitMolalFract(I) = &
			//            (FuelCell(Num)%AuxilHeat%ConstitMolalFract(I)* FuelCell(Num)%AuxilHeat%NdotAuxMix - NdotWaterCond) &
			//            /     FuelCell(Num)%ExhaustHX%NdotHXleaving
			//       cycle
			//     ENDIF

			//     FuelCell(Num)%ExhaustHX%ConstitMolalFract(I) = FuelCell(Num)%AuxilHeat%ConstitMolalFract(I) &
			//                                       * FuelCell(Num)%AuxilHeat%NdotAuxMix / FuelCell(Num)%ExhaustHX%NdotHXleaving
			//   ENDDO
			// ENDIF

			// get new average heat capacity
			//CALL FigureHXleavingGasHeatCap(Num, (THXexh + TauxMix)/2 , CpHXleavingGasMol)

			// NdotCpHXleaving = FuelCell(Num)%ExhaustHX%NdotHXleaving*CpHXleavingGasMol* 1000.0

			// update gas leaving temperature with modified heat transfer rate
			//  IF ((NdotCpHXleaving > 0) .AND. (qHX > 0)) THEN
			//     THXexh = TauxMix - (qHX / NdotCpHXleaving)
			//  ELSE
			//     THXexh = TauxMix
			//  ENDIF
			// update water leaving temperature with modified heat transfer rate
			//  IF (MdotWater * CPCW( (TwaterIn + TwaterOut)/2 )  <= 0.0) THEN
			//    TwaterOut =  TwaterIn
			//  ELSE
			//    TwaterOut  =  TwaterIn + qHX / (MdotWater * CPCW( (TwaterIn + TwaterOut)/2 ))
			//  ENDIF

		} else {
			assert( false ); // Variables not set are used below
		}}

		// update results in data structure.
		FuelCell( Num ).ExhaustHX.qHX = qHX;
		FuelCell( Num ).ExhaustHX.THXexh = THXexh;
		FuelCell( Num ).ExhaustHX.WaterMassFlowRate = MdotWater;
		FuelCell( Num ).ExhaustHX.WaterVaporFractExh = waterFract;

		FuelCell( Num ).ExhaustHX.CondensateRate = NdotWaterCond;
		FuelCell( Num ).ExhaustHX.WaterOutletTemp = TwaterOut;
		FuelCell( Num ).ExhaustHX.WaterOutletEnthalpy = Node( inNodeNum ).Enthalpy + qHX;

		// now update water outlet node Changing to Kg/s!
		//  OutNodeNum = FuelCell(Num)%ExhaustHX%WaterOutNode
		//  inNodeNum  = FuelCell(Num)%ExhaustHX%WaterInNode
		//  Node(OutNodeNum)%Temp = Twaterout
		//  Node(OutNodeNum)%Enthalpy =
		//  Node(OutNodeNum)%MassFlowRate = MdotWater

	}

	void
	SimFuelCellPlantHeatRecovery(
		std::string const & EP_UNUSED( CompType ),
		std::string const & CompName,
		int const CompTypeNum,
		int & CompNum,
		bool const EP_UNUSED( RunFlag ),
		bool & InitLoopEquip,
		Real64 & EP_UNUSED( MyLoad ), // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// makes sure input are gotten and setup from Plant loop perspective.
		// does not (re)simulate entire FuelCell model

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataPlant::TypeOf_Generator_FCExhaust;
		using DataPlant::TypeOf_Generator_FCStackCooler;
		using PlantUtilities::UpdateComponentHeatRecoverySide;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// INTEGER, INTENT(IN)          :: FlowLock !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetFuelCellInput ) {

			// Read input data.
			GetFuelCellGeneratorInput();
			GetFuelCellInput = false;
		}

		if ( InitLoopEquip ) {
			if ( CompTypeNum == TypeOf_Generator_FCExhaust ) {
				CompNum = FindItemInList( CompName, FuelCell, &FCDataStruct::NameExhaustHX );
			} else if ( CompTypeNum == TypeOf_Generator_FCStackCooler ) {
				CompNum = FindItemInList( CompName, FuelCell, &FCDataStruct::NameStackCooler );
			}
			if ( CompNum == 0 ) {
				ShowFatalError( "SimFuelCellPlantHeatRecovery: Fuel Cell Generator Unit not found=" + CompName );
			}
			MinCap = 0.0;
			MaxCap = 0.0;
			OptCap = 0.0;
			return;
		} // End Of InitLoopEquip

		if ( CompTypeNum == TypeOf_Generator_FCStackCooler ) {
			UpdateComponentHeatRecoverySide( FuelCell( CompNum ).CWLoopNum, FuelCell( CompNum ).CWLoopSideNum, TypeOf_Generator_FCStackCooler, FuelCell( CompNum ).StackCooler.WaterInNode, FuelCell( CompNum ).StackCooler.WaterOutNode, FuelCell( CompNum ).Report.qHX, FuelCell( CompNum ).Report.HeatRecInletTemp, FuelCell( CompNum ).Report.HeatRecOutletTemp, FuelCell( CompNum ).Report.HeatRecMdot, FirstHVACIteration );
		} else if ( CompTypeNum == TypeOf_Generator_FCExhaust ) {
			UpdateComponentHeatRecoverySide( FuelCell( CompNum ).CWLoopNum, FuelCell( CompNum ).CWLoopSideNum, TypeOf_Generator_FCExhaust, FuelCell( CompNum ).ExhaustHX.WaterInNode, FuelCell( CompNum ).ExhaustHX.WaterOutNode, FuelCell( CompNum ).Report.qHX, FuelCell( CompNum ).Report.HeatRecInletTemp, FuelCell( CompNum ).Report.HeatRecOutletTemp, FuelCell( CompNum ).Report.HeatRecMdot, FirstHVACIteration );

		}

	}

	// End FuelCell Generator Module Model Subroutines
	// *****************************************************************************

	// Begin FuelCell Generator Module Utility Subroutines
	// *****************************************************************************

	void
	InitFuelCellGenerators( int const FCnum ) // index to specific fuel cell generator
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Sept 2010, plant upgrades

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the FuelCell generators.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::HourOfDay;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Generator_FCExhaust;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitFuelCellGenerators" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool InitGeneratorOnce( true ); // flag for 1 time initialization
		static Array1D_bool MyEnvrnFlag; // flag for init once at start of environment
		static Array1D_bool MyWarmupFlag; // flag for init after warmup complete
		int inNode; // inlet index in Node array
		int outNode; // outlet, index in Node array
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		static Array1D_bool MyPlantScanFlag;
		Real64 mdot; // local temporary mass flow rate
		Real64 rho; // local temporary fluid density
		bool errFlag;

		// FLOW:
		// Do the one time initializations
		if ( InitGeneratorOnce ) {
			MyEnvrnFlag.allocate( NumFuelCellGenerators );
			MyWarmupFlag.allocate( NumFuelCellGenerators );
			MyPlantScanFlag.allocate( NumFuelCellGenerators );
			MyEnvrnFlag = true;
			MyWarmupFlag = false;
			InitGeneratorOnce = false;
			MyPlantScanFlag = true;
		} // end one time setups and inits

		if ( MyPlantScanFlag( FCnum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( FuelCell( FCnum ).NameExhaustHX, TypeOf_Generator_FCExhaust, FuelCell( FCnum ).CWLoopNum, FuelCell( FCnum ).CWLoopSideNum, FuelCell( FCnum ).CWBranchNum, FuelCell( FCnum ).CWCompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitFuelCellGenerators: Program terminated due to previous condition(s)." );
			}
			MyPlantScanFlag( FCnum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( FCnum ) && ! MyPlantScanFlag( FCnum ) ) {

			FuelSupply( FuelCell( FCnum ).FuelSupNum ).PfuelCompEl = 0.0;
			FuelSupply( FuelCell( FCnum ).FuelSupNum ).TfuelIntoFCPM = 0.0;
			FuelSupply( FuelCell( FCnum ).FuelSupNum ).TfuelIntoCompress = 0.0;
			FuelSupply( FuelCell( FCnum ).FuelSupNum ).QskinLoss = 0.0;

			FuelCell( FCnum ).AirSup.TairIntoFCPM = 0.0;
			FuelCell( FCnum ).AirSup.PairCompEl = 0.0;
			FuelCell( FCnum ).AirSup.TairIntoBlower = 0.0;
			FuelCell( FCnum ).AirSup.QskinLoss = 0.0;
			FuelCell( FCnum ).AirSup.QintakeRecovery = 0.0;
			FuelCell( FCnum ).FCPM.NumCycles = 0;
			FuelCell( FCnum ).FCPM.Pel = 0.0;
			FuelCell( FCnum ).FCPM.PelLastTimeStep = 0.0;
			FuelCell( FCnum ).FCPM.Eel = 0.0;
			FuelCell( FCnum ).FCPM.PelancillariesAC = 0.0;
			FuelCell( FCnum ).FCPM.NdotFuel = 0.0;
			FuelCell( FCnum ).FCPM.TotFuelInEnthalphy = 0.0;
			FuelCell( FCnum ).FCPM.NdotProdGas = 0.0;
			FuelCell( FCnum ).FCPM.TprodGasLeavingFCPM = 0.0;
			FuelCell( FCnum ).FCPM.TotProdGasEnthalphy = 0.0;
			FuelCell( FCnum ).FCPM.NdotAir = 0.0;
			FuelCell( FCnum ).FCPM.TotAirInEnthalphy = 0.0;
			FuelCell( FCnum ).FCPM.NdotLiqwater = 0.0;
			FuelCell( FCnum ).FCPM.TwaterInlet = 0.0;
			FuelCell( FCnum ).FCPM.WaterInEnthalpy = 0.0;
			FuelCell( FCnum ).FCPM.TprodGasLeavingFCPM = 200.0;
			FuelCell( FCnum ).FCPM.FractionalDayofLastStartUp = 0.0;
			FuelCell( FCnum ).FCPM.FractionalDayofLastShutDown = 0.0;
			FuelCell( FCnum ).FCPM.HasBeenOn = true;
			FuelCell( FCnum ).FCPM.DuringShutDown = false;
			FuelCell( FCnum ).FCPM.DuringStartUp = false;
			FuelCell( FCnum ).WaterSup.TwaterIntoCompress = 0.0;
			FuelCell( FCnum ).WaterSup.TwaterIntoFCPM = 0.0;
			FuelCell( FCnum ).WaterSup.PwaterCompEl = 0.0;
			FuelCell( FCnum ).WaterSup.QskinLoss = 0.0;
			FuelCell( FCnum ).AuxilHeat.TauxMix = 0.0;
			FuelCell( FCnum ).AuxilHeat.NdotAuxMix = 0.0;
			FuelCell( FCnum ).AuxilHeat.QskinLoss = 0.0;
			FuelCell( FCnum ).AuxilHeat.QairIntake = 0.0;
			FuelCell( FCnum ).ExhaustHX.NdotHXleaving = 0.0;
			FuelCell( FCnum ).ExhaustHX.WaterOutletTemp = 0.0;
			FuelCell( FCnum ).ExhaustHX.WaterOutletEnthalpy = 0.0;

			FuelCell( FCnum ).ElecStorage.LastTimeStepStateOfCharge = FuelCell( FCnum ).ElecStorage.StartingEnergyStored;
			FuelCell( FCnum ).ElecStorage.ThisTimeStepStateOfCharge = FuelCell( FCnum ).ElecStorage.StartingEnergyStored;
			FuelCell( FCnum ).ElecStorage.PelNeedFromStorage = 0.0;
			FuelCell( FCnum ).ElecStorage.IdesiredDischargeCurrent = 0.0;
			FuelCell( FCnum ).ElecStorage.PelFromStorage = 0.0;
			FuelCell( FCnum ).ElecStorage.IfromStorage = 0.0;
			FuelCell( FCnum ).ElecStorage.PelIntoStorage = 0.0;
			FuelCell( FCnum ).ElecStorage.QairIntake = 0.0;

			FuelCell( FCnum ).Inverter.PCUlosses = 0.0;
			FuelCell( FCnum ).Inverter.QairIntake = 0.0;

			rho = GetDensityGlycol( PlantLoop( FuelCell( FCnum ).CWLoopNum ).FluidName, InitHRTemp, PlantLoop( FuelCell( FCnum ).CWLoopNum ).FluidIndex, RoutineName );

			FuelCell( FCnum ).ExhaustHX.WaterMassFlowRateDesign = FuelCell( FCnum ).ExhaustHX.WaterVolumeFlowMax * rho;
			FuelCell( FCnum ).ExhaustHX.WaterMassFlowRate = FuelCell( FCnum ).ExhaustHX.WaterMassFlowRateDesign;
			inNode = FuelCell( FCnum ).ExhaustHX.WaterInNode;
			outNode = FuelCell( FCnum ).ExhaustHX.WaterOutNode;
			Node( inNode ).Temp = InitHRTemp;
			Node( outNode ).Temp = InitHRTemp;

			InitComponentNodes( 0.0, FuelCell( FCnum ).ExhaustHX.WaterMassFlowRateDesign, inNode, outNode, FuelCell( FCnum ).CWLoopNum, FuelCell( FCnum ).CWLoopSideNum, FuelCell( FCnum ).CWBranchNum, FuelCell( FCnum ).CWCompNum );

			MyEnvrnFlag( FCnum ) = false;
			MyWarmupFlag( FCnum ) = true;
		} // end environmental inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( FCnum ) = true;
		}

		if ( MyWarmupFlag( FCnum ) && ( ! WarmupFlag ) ) {
			// need to reset initial state of charge at beginning of environment but after warm up is complete
			FuelCell( FCnum ).ElecStorage.LastTimeStepStateOfCharge = FuelCell( FCnum ).ElecStorage.StartingEnergyStored;
			FuelCell( FCnum ).ElecStorage.ThisTimeStepStateOfCharge = FuelCell( FCnum ).ElecStorage.StartingEnergyStored;
			MyWarmupFlag( FCnum ) = false;
		}

		//using and elapsed time method rather than FirstHVACIteration here
		TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;
		if ( FuelCell( FCnum ).TimeElapsed != TimeElapsed ) {

			FuelCell( FCnum ).ElecStorage.LastTimeStepStateOfCharge = FuelCell( FCnum ).ElecStorage.ThisTimeStepStateOfCharge;
			FuelCell( FCnum ).FCPM.PelLastTimeStep = FuelCell( FCnum ).FCPM.Pel;

			inNode = FuelCell( FCnum ).ExhaustHX.WaterInNode;
			outNode = FuelCell( FCnum ).ExhaustHX.WaterOutNode;
			// intialize flow rate in water loop, this is "requesting" flow
			mdot = FuelCell( FCnum ).ExhaustHX.WaterMassFlowRateDesign;

			SetComponentFlowRate( mdot, inNode, outNode, FuelCell( FCnum ).CWLoopNum, FuelCell( FCnum ).CWLoopSideNum, FuelCell( FCnum ).CWBranchNum, FuelCell( FCnum ).CWCompNum );

			FuelCell( FCnum ).ExhaustHX.WaterMassFlowRate = mdot;
			FuelCell( FCnum ).ExhaustHX.WaterInletTemp = Node( inNode ).Temp;
			FuelCell( FCnum ).TimeElapsed = TimeElapsed;
		} else {
			inNode = FuelCell( FCnum ).ExhaustHX.WaterInNode;

			SetComponentFlowRate( FuelCell( FCnum ).ExhaustHX.WaterMassFlowRate, FuelCell( FCnum ).ExhaustHX.WaterInNode, FuelCell( FCnum ).ExhaustHX.WaterOutNode, FuelCell( FCnum ).CWLoopNum, FuelCell( FCnum ).CWLoopSideNum, FuelCell( FCnum ).CWBranchNum, FuelCell( FCnum ).CWCompNum );

			FuelCell( FCnum ).ExhaustHX.WaterInletTemp = Node( inNode ).Temp;
		}

	}

	// End FuelCell Generator Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the FuelCell Generator Module
	// *****************************************************************************

	void
	FigureFuelCellZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       BG March 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Couple equpment skin losses to the Zone Heat Balance
		// calculate skin losses from different subsystems and set the value

		// METHODOLOGY EMPLOYED:
		// This routine adds up the various skin losses and then
		//  sets the values in the ZoneIntGain structure

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused  INTEGER :: thisZone ! index in Zone structure array
		Real64 TotalZoneHeatGain; // working variable for zone gain [w]
		//  INTEGER :: ZoneNum
		int FCnum; // number of fuel cell
		static bool MyEnvrnFlag( true );

		if ( NumFuelCellGenerators == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( auto & e : FuelSupply ) e.QskinLoss = 0.0;
			MyEnvrnFlag = false;
			for ( int i = FuelCell.l(), e = FuelCell.u(); i <= e; ++i) {
				auto & cell( FuelCell( i ) );
				cell.FCPM.HasBeenOn = false;
				cell.AirSup.PairCompEl = 0.0;
				cell.QconvZone = 0.0;
				cell.QradZone = 0.0;
				cell.AirSup.QskinLoss = 0.0;
				cell.WaterSup.QskinLoss = 0.0;
				cell.AuxilHeat.QskinLoss = 0.0;
				cell.FCPM.QdotSkin = 0.0;
				cell.Report.SkinLossConvect = 0.0;
				cell.Report.SkinLossRadiat = 0.0;
				cell.AuxilHeat.QairIntake = 0.0;
				cell.ElecStorage.QairIntake = 0.0;
				cell.Inverter.QairIntake = 0.0;
			}
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// this routine needs to do something for zone gains during sizing

		//first collect skin losses from different subsystems
		for ( FCnum = 1; FCnum <= NumFuelCellGenerators; ++FCnum ) {
			TotalZoneHeatGain = FuelCell( FCnum ).AirSup.QskinLoss + FuelSupply( FuelCell( FCnum ).FuelSupNum ).QskinLoss + FuelCell( FCnum ).WaterSup.QskinLoss + FuelCell( FCnum ).AuxilHeat.QskinLoss + FuelCell( FCnum ).FCPM.QdotSkin; // intake Blower losses to zone | fuel compressor losses to zone | water pump losses to zone | auxil burner losses to zone | power module (stack and reformer) losses to zone

			// now account for other subsystems that may or may not have air intake recovery
			{ auto const SELECT_CASE_var( FuelCell( FCnum ).AirSup.IntakeRecoveryMode );

			if ( SELECT_CASE_var == NoRecoveryOnAirIntake ) { // then the heat has to go into zone
				TotalZoneHeatGain += FuelCell( FCnum ).AuxilHeat.QairIntake + FuelCell( FCnum ).ElecStorage.QairIntake + FuelCell( FCnum ).Inverter.QairIntake;
			} else if ( SELECT_CASE_var == RecoverAuxiliaryBurner ) {
				TotalZoneHeatGain += FuelCell( FCnum ).ElecStorage.QairIntake + FuelCell( FCnum ).Inverter.QairIntake;

			} else if ( SELECT_CASE_var == RecoverInverterBatt ) {
				TotalZoneHeatGain += FuelCell( FCnum ).AuxilHeat.QairIntake;

			} else if ( SELECT_CASE_var == RecoverInverter ) {
				TotalZoneHeatGain += FuelCell( FCnum ).AuxilHeat.QairIntake + FuelCell( FCnum ).ElecStorage.QairIntake;
			} else if ( SELECT_CASE_var == RecoverBattery ) {
				TotalZoneHeatGain += FuelCell( FCnum ).AuxilHeat.QairIntake + FuelCell( FCnum ).Inverter.QairIntake;

			} else if ( SELECT_CASE_var == RecoverBurnInvertBatt ) {
				// do nothing

			}}

			FuelCell( FCnum ).QconvZone = TotalZoneHeatGain * ( 1 - FuelCell( FCnum ).FCPM.RadiativeFract );
			FuelCell( FCnum ).Report.SkinLossConvect = FuelCell( FCnum ).QconvZone;
			FuelCell( FCnum ).QradZone = TotalZoneHeatGain * FuelCell( FCnum ).FCPM.RadiativeFract;
			FuelCell( FCnum ).Report.SkinLossRadiat = FuelCell( FCnum ).QradZone;

		} // over number of Fuel cells

		//  IF(DoingSizing)THEN

		//  ENDIF

	}

	void
	UpdateExhaustAirFlows( int const EP_UNUSED( Num ) ) // generator number
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
		// na

	}

	void
	CalcUpdateHeatRecovery(
		int const Num, // Generator number
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update plant loop interactions, do any calcs needed

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNodeNum;
		int OutNodeNum;

		// now update water outlet node Changing to Kg/s!
		OutNodeNum = FuelCell( Num ).ExhaustHX.WaterOutNode;
		InNodeNum = FuelCell( Num ).ExhaustHX.WaterInNode;

		SafeCopyPlantNode( InNodeNum, OutNodeNum );

		Node( OutNodeNum ).Temp = FuelCell( Num ).ExhaustHX.WaterOutletTemp;
		Node( OutNodeNum ).Enthalpy = FuelCell( Num ).ExhaustHX.WaterOutletEnthalpy;

		//  IF (FirstHVACIteration) Then
		//    Node(InNodeNum)%MassFlowRateMaxAvail     = FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign
		//    Node(InNodeNum)%MassFlowRateMinAvail     = 0.0D0
		//    Node(InNodeNum)%MassFlowRate             = MAX(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
		//                                                   Node(InNodeNum)%MassFlowRateMin)
		//    Node(InNodeNum)%MassFlowRate             = MIN(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
		//                                                   Node(InNodeNum)%MassFlowRateMax)
		//  ELSE
		//    Node(InNodeNum)%MassFlowRate             = MAX(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
		//                                                   Node(InNodeNum)%MassFlowRateMin)
		//    Node(InNodeNum)%MassFlowRate             = MAX(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
		//                                                   Node(InNodeNum)%MassFlowRateMinAvail)
		//    Node(InNodeNum)%MassFlowRate             = MIN(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
		//                                                   Node(InNodeNum)%MassFlowRateMax)
		//    Node(InNodeNum)%MassFlowRate             = MIN(FuelCell(Num)%ExhaustHX%WaterMassFlowRateDesign,   &
		//                                                   Node(InNodeNum)%MassFlowRateMaxAvail)
		//  ENDIF
		//  Node(OutNodeNum)%MassFlowRate             = Node(InNodeNum)%MassFlowRate
		//  Node(OutNodeNum)%MassFlowRateMaxAvail     = Node(InNodeNum)%MassFlowRateMaxAvail
		//  Node(OutNodeNum)%MassFlowRateMinAvail     = Node(InNodeNum)%MassFlowRateMinAvail
		//  Node(OutNodeNum)%MassFlowRateMax          = Node(InNodeNum)%MassFlowRateMax
		//  Node(OutNodeNum)%MassFlowRateMin          = Node(InNodeNum)%MassFlowRateMin

	}

	void
	UpdateFuelCellGeneratorRecords(
		bool const EP_UNUSED( RunFlag ), // TRUE if Generator operating
		int const Num // Generator number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          BG
		//       DATE WRITTEN:

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		FuelCell( Num ).Report.ACPowerGen = FuelCell( Num ).ACPowerGen; //electrical power produced [W]
		FuelCell( Num ).Report.ACEnergyGen = FuelCell( Num ).ACPowerGen * TimeStepSys * SecInHour; // energy produced (J)
		FuelCell( Num ).Report.QdotExhaust = 0.0; // reporting: exhaust gas heat recovered (W)
		FuelCell( Num ).Report.TotalHeatEnergyRec = 0.0; // reporting: total heat recovered (J)
		FuelCell( Num ).Report.ExhaustEnergyRec = 0.0; // reporting: exhaust gas heat recovered (J)

		FuelCell( Num ).Report.HeatRecInletTemp = 0.0; // reporting: Heat Recovery Loop Inlet Temperature (C)
		FuelCell( Num ).Report.HeatRecOutletTemp = 0.0; // reporting: Heat Recovery Loop Outlet Temperature (C)
		FuelCell( Num ).Report.HeatRecMdot = 0.0; // reporting: Heat Recovery Loop Mass flow rate (kg/s)

		FuelCell( Num ).Report.ElectEfficiency = 0.0;
		FuelCell( Num ).Report.ThermalEfficiency = 0.0;
		FuelCell( Num ).Report.OverallEfficiency = 0.0;
		FuelCell( Num ).Report.ExergyEfficiency = 0.0;

		FuelCell( Num ).Report.TairInlet = FuelCell( Num ).AirSup.TairIntoBlower; // State point 1
		FuelCell( Num ).Report.TairIntoFCPM = FuelCell( Num ).AirSup.TairIntoFCPM; // State point 4
		FuelCell( Num ).Report.NdotAir = FuelCell( Num ).FCPM.NdotAir; // air flow in kmol/sec
		FuelCell( Num ).Report.TotAirInEnthalphy = FuelCell( Num ).FCPM.TotAirInEnthalphy; // State point 4
		FuelCell( Num ).Report.BlowerPower = FuelCell( Num ).AirSup.PairCompEl; // electrical power used by air supply blower
		FuelCell( Num ).Report.BlowerEnergy = FuelCell( Num ).AirSup.PairCompEl * TimeStepSys * SecInHour; // electrical energy
		FuelCell( Num ).Report.BlowerSkinLoss = FuelCell( Num ).AirSup.QskinLoss; // heat rate of losses by blower

		FuelCell( Num ).Report.TfuelInlet = FuelSupply( FuelCell( Num ).FuelSupNum ).TfuelIntoCompress; // State point 2
		FuelCell( Num ).Report.TfuelIntoFCPM = FuelSupply( FuelCell( Num ).FuelSupNum ).TfuelIntoFCPM; // TEmperature state point 5 [C]
		FuelCell( Num ).Report.NdotFuel = FuelCell( Num ).FCPM.NdotFuel; // fuel flow in kmol/sec
		FuelCell( Num ).Report.TotFuelInEnthalpy = FuelCell( Num ).FCPM.TotFuelInEnthalphy; // enthalpy at state point 5 [W]
		FuelCell( Num ).Report.FuelCompressPower = FuelSupply( FuelCell( Num ).FuelSupNum ).PfuelCompEl;
		// electrical power used by fuel supply compressor [W]
		FuelCell( Num ).Report.FuelCompressEnergy = FuelSupply( FuelCell( Num ).FuelSupNum ).PfuelCompEl * TimeStepSys * SecInHour; // elect energy
		FuelCell( Num ).Report.FuelCompressSkinLoss = FuelSupply( FuelCell( Num ).FuelSupNum ).QskinLoss;
		//heat rate of losses.by fuel supply compressor [W]
		FuelCell( Num ).Report.FuelEnergyLHV = FuelCell( Num ).FCPM.NdotFuel * FuelSupply( FuelCell( Num ).FuelSupNum ).LHV * 1000000.0 * TimeStepSys * SecInHour; // reporting: Fuel Energy used (J)
		FuelCell( Num ).Report.FuelEnergyUseRateLHV = FuelCell( Num ).FCPM.NdotFuel * FuelSupply( FuelCell( Num ).FuelSupNum ).LHV * 1000000.0; // reporting: Fuel Energy used (W)
		FuelCell( Num ).Report.FuelEnergyHHV = FuelCell( Num ).FCPM.NdotFuel * FuelSupply( FuelCell( Num ).FuelSupNum ).HHV * FuelSupply( FuelCell( Num ).FuelSupNum ).KmolPerSecToKgPerSec * TimeStepSys * SecInHour;

		FuelCell( Num ).Report.FuelEnergyUseRateHHV = FuelCell( Num ).FCPM.NdotFuel * FuelSupply( FuelCell( Num ).FuelSupNum ).HHV * FuelSupply( FuelCell( Num ).FuelSupNum ).KmolPerSecToKgPerSec;

		FuelCell( Num ).Report.FuelRateMdot = 0.0; // (Kg/s)

		FuelCell( Num ).Report.TwaterInlet = FuelCell( Num ).WaterSup.TwaterIntoCompress;
		FuelCell( Num ).Report.TwaterIntoFCPM = FuelCell( Num ).WaterSup.TwaterIntoFCPM;
		FuelCell( Num ).Report.NdotWater = FuelCell( Num ).FCPM.NdotLiqwater; // water flow in kmol/sec (reformer water)
		FuelCell( Num ).Report.WaterPumpPower = FuelCell( Num ).WaterSup.PwaterCompEl;
		FuelCell( Num ).Report.WaterPumpEnergy = FuelCell( Num ).WaterSup.PwaterCompEl * TimeStepSys * SecInHour; // electrical energy
		FuelCell( Num ).Report.WaterIntoFCPMEnthalpy = FuelCell( Num ).FCPM.WaterInEnthalpy;

		FuelCell( Num ).Report.TprodGas = FuelCell( Num ).FCPM.TprodGasLeavingFCPM; // temperature at State point 7
		FuelCell( Num ).Report.EnthalProdGas = FuelCell( Num ).FCPM.TotProdGasEnthalphy; // enthalpy at State point 7
		FuelCell( Num ).Report.NdotProdGas = FuelCell( Num ).FCPM.NdotProdGas; // flow rate at point 7 [kmol/sec]
		FuelCell( Num ).Report.NdotProdAr = FuelCell( Num ).FCPM.ConstitMolalFract( 5 ) * FuelCell( Num ).FCPM.NdotProdGas;
		FuelCell( Num ).Report.NdotProdCO2 = FuelCell( Num ).FCPM.ConstitMolalFract( 1 ) * FuelCell( Num ).FCPM.NdotProdGas;
		FuelCell( Num ).Report.NdotProdH2O = FuelCell( Num ).FCPM.ConstitMolalFract( 4 ) * FuelCell( Num ).FCPM.NdotProdGas;
		FuelCell( Num ).Report.NdotProdN2 = FuelCell( Num ).FCPM.ConstitMolalFract( 2 ) * FuelCell( Num ).FCPM.NdotProdGas;
		FuelCell( Num ).Report.NdotProdO2 = FuelCell( Num ).FCPM.ConstitMolalFract( 3 ) * FuelCell( Num ).FCPM.NdotProdGas;

		FuelCell( Num ).Report.qHX = FuelCell( Num ).ExhaustHX.qHX;
		FuelCell( Num ).Report.HXenergy = FuelCell( Num ).ExhaustHX.qHX * TimeStepSys * SecInHour;
		FuelCell( Num ).Report.THXexh = FuelCell( Num ).ExhaustHX.THXexh;
		FuelCell( Num ).Report.WaterVaporFractExh = FuelCell( Num ).ExhaustHX.WaterVaporFractExh;
		FuelCell( Num ).Report.CondensateRate = FuelCell( Num ).ExhaustHX.CondensateRate;

		FuelCell( Num ).Report.SeqSubstIterations = FuelCell( Num ).FCPM.SeqSubstitIter; // number of iterations in FuelCell loop
		FuelCell( Num ).Report.RegulaFalsiIterations = FuelCell( Num ).FCPM.RegulaFalsiIter; // number of iterations in Tproduct gas solving

		FuelCell( Num ).Report.ACancillariesPower = FuelCell( Num ).FCPM.PelancillariesAC;
		FuelCell( Num ).Report.ACancillariesEnergy = FuelCell( Num ).FCPM.PelancillariesAC * TimeStepSys * SecInHour;

		FuelCell( Num ).Report.PCUlosses = FuelCell( Num ).Inverter.PCUlosses; // inverter losses
		FuelCell( Num ).Report.DCPowerGen = FuelCell( Num ).FCPM.Pel; //DC power out of FCPM.
		FuelCell( Num ).Report.DCPowerEff = FuelCell( Num ).FCPM.Eel; // FCPM efficienty Eel.
		FuelCell( Num ).Report.ElectEnergyinStorage = FuelCell( Num ).ElecStorage.ThisTimeStepStateOfCharge;
		FuelCell( Num ).Report.StoredPower = FuelCell( Num ).ElecStorage.PelIntoStorage;
		FuelCell( Num ).Report.StoredEnergy = FuelCell( Num ).ElecStorage.PelIntoStorage * TimeStepSys * SecInHour;
		FuelCell( Num ).Report.DrawnPower = FuelCell( Num ).ElecStorage.PelFromStorage;
		FuelCell( Num ).Report.DrawnEnergy = FuelCell( Num ).ElecStorage.PelFromStorage * TimeStepSys * SecInHour;

		FuelCell( Num ).Report.SkinLossPower = FuelCell( Num ).QconvZone + FuelCell( Num ).QradZone;
		FuelCell( Num ).Report.SkinLossEnergy = ( FuelCell( Num ).QconvZone + FuelCell( Num ).QradZone ) * TimeStepSys * SecInHour;
		FuelCell( Num ).Report.SkinLossConvect = FuelCell( Num ).QconvZone;
		FuelCell( Num ).Report.SkinLossRadiat = FuelCell( Num ).QradZone;

	}

	void
	GetFuelCellGeneratorResults(
		int const EP_UNUSED( GeneratorType ), // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// provide a get method to collect results at the load center level

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
		GeneratorPower = FuelCell( GeneratorIndex ).Report.ACPowerGen;
		GeneratorEnergy = FuelCell( GeneratorIndex ).Report.ACEnergyGen;
		ThermalPower = FuelCell( GeneratorIndex ).Report.qHX;
		ThermalEnergy = FuelCell( GeneratorIndex ).Report.HXenergy;

	}

} // FuelCellElectricGenerator

} // EnergyPlus
