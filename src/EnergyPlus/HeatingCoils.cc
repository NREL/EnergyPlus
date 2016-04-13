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
#include <HeatingCoils.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatingCoils {
	// Module containing the HeatingCoil simulation routines other than the Water coils

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   May 2000
	//       MODIFIED       Therese Stovall June 2008 to add references to refrigation condensers
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the HeatingCoil System Component

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using DataEnvironment::StdRhoAir;
	using DataHeatBalance::NumRefrigeratedRacks;
	using DataHeatBalance::HeatReclaimRefrigeratedRack;
	using DataHeatBalance::HeatReclaimRefrigCondenser;
	using DataHeatBalance::HeatReclaimDXCoil;
	using DataHeatBalance::NumRefrigCondensers;
	using DataHeatBalance::RefrigSystemTypeDetailed;
	using DataHeatBalance::RefrigSystemTypeRack;
	using DataHeatBalance::RefrigCondenserTypeAir;
	using DataHeatBalance::RefrigCondenserTypeEvap;
	using DataHeatBalance::RefrigCondenserTypeWater;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyHFnTdbW;
	using Psychrometrics::PsyRhoAirFnPbTdbW;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using DXCoils::NumDXCoils;
	using DXCoils::GetDXCoilIndex;
	using RefrigeratedCase::GetRefrigeratedRackIndex;

	// Data
	//MODULE PARAMETER DEFINITIONS
	Real64 const MinAirMassFlow( 0.001 );
	int NumDesuperheaterCoil; // Total number of desuperheater heating coil objects in input

	static std::string const BlankString;

	// reclaim heat object types
	int const COMPRESSORRACK_REFRIGERATEDCASE( 1 );
	int const COIL_DX_COOLING( 2 );
	int const COIL_DX_MULTISPEED( 3 );
	int const COIL_DX_MULTIMODE( 4 );
	int const CONDENSER_REFRIGERATION( 5 );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumHeatingCoils( 0 ); // The Number of HeatingCoils found in the Input
	Array1D_bool MySizeFlag;
	Array1D_bool ValidSourceType; // Used to determine if a source for a desuperheater heating coil is valid
	bool GetCoilsInputFlag( true ); // Flag set to make sure you get input once
	bool CoilIsSuppHeater( false ); // Flag set to indicate the heating coil is a supplemental heater
	bool MyOneTimeFlag( true ); // one time initialization flag
	Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Utility routines for module

	// Object Data
	Array1D< HeatingCoilEquipConditions > HeatingCoil;
	Array1D< HeatingCoilNumericFieldData > HeatingCoilNumericFields;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SimulateHeatingCoilComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		Optional< Real64 const > QCoilReq, // coil load to be met
		Optional_int CompIndex,
		Optional< Real64 > QCoilActual, // coil load actually delivered returned to calling component
		Optional_bool_const SuppHeat, // True if current heating coil is a supplemental heating coil
		Optional_int_const FanOpMode, // fan operating mode, CycFanCycCoil or ContFanCycCoil
		Optional< Real64 const > PartLoadRatio, // part-load ratio of heating coil
		Optional_int StageNum,
		Optional< Real64 const > SpeedRatio // Speed ratio of MultiStage heating coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages HeatingCoil component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// in a unitary system

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoilNum( 0 ); // The HeatingCoil that you are currently loading input into
		Real64 QCoilActual2; // coil load actually delivered returned from specific coil
		int OpMode; // fan operating mode
		Real64 PartLoadFrac; // part-load fraction of heating coil
		Real64 QCoilRequired; // local variable for optional argument

		// FLOW:
		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		// Find the correct HeatingCoilNumber with the Coil Name
		if ( present( CompIndex ) ) {
			if ( CompIndex == 0 ) {
				CoilNum = FindItemInList( CompName, HeatingCoil );
				if ( CoilNum == 0 ) {
					ShowFatalError( "SimulateHeatingCoilComponents: Coil not found=" + CompName );
				}
				//    CompIndex=CoilNum
			} else {
				CoilNum = CompIndex;
				if ( CoilNum > NumHeatingCoils || CoilNum < 1 ) {
					ShowFatalError( "SimulateHeatingCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Number of Heating Coils=" + TrimSigDigits( NumHeatingCoils ) + ", Coil name=" + CompName );
				}
				if ( CheckEquipName( CoilNum ) ) {
					if ( ! CompName.empty() && CompName != HeatingCoil( CoilNum ).Name ) {
						ShowFatalError( "SimulateHeatingCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + HeatingCoil( CoilNum ).Name );
					}
					CheckEquipName( CoilNum ) = false;
				}
			}
		} else {
			ShowSevereError( "SimulateHeatingCoilComponents: CompIndex argument not used." );
			ShowContinueError( "..CompName = " + CompName );
			ShowFatalError( "Preceding conditions cause termination." );
		}

		if ( present( SuppHeat ) ) {
			CoilIsSuppHeater = SuppHeat;
		} else {
			CoilIsSuppHeater = false;
		}

		if ( present( FanOpMode ) ) {
			OpMode = FanOpMode;
		} else {
			OpMode = ContFanCycCoil;
		}

		if ( present( PartLoadRatio ) ) {
			PartLoadFrac = PartLoadRatio;
		} else {
			PartLoadFrac = 1.0;
		}

		if ( present( QCoilReq ) ) {
			QCoilRequired = QCoilReq;
		} else {
			QCoilRequired = SensedLoadFlagValue;
		}

		// With the correct CoilNum Initialize
		InitHeatingCoil( CoilNum, FirstHVACIteration, QCoilRequired ); // Initialize all HeatingCoil related parameters

		// Calculate the Correct HeatingCoil Model with the current CoilNum
		if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingElectric ) {
			CalcElectricHeatingCoil( CoilNum, QCoilRequired, QCoilActual2, OpMode, PartLoadFrac );
		} else if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingElectric_MultiStage ) {
			CalcMultiStageElectricHeatingCoil( CoilNum, SpeedRatio, PartLoadRatio, StageNum, OpMode ); //Autodesk:OPTIONAL SpeedRatio, PartLoadRatio, StageNum used without PRESENT check
		} else if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingGas ) {
			CalcGasHeatingCoil( CoilNum, QCoilRequired, QCoilActual2, OpMode, PartLoadFrac );
		} else if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingGas_MultiStage ) {
			CalcMultiStageGasHeatingCoil( CoilNum, SpeedRatio, PartLoadRatio, StageNum, OpMode ); //Autodesk:OPTIONAL SpeedRatio, PartLoadRatio, StageNum used without PRESENT check
		} else if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingDesuperheater ) {
			CalcDesuperheaterHeatingCoil( CoilNum, QCoilRequired, QCoilActual2 );
		} else {
			QCoilActual2 = 0.0;
		}

		// Update the current HeatingCoil to the outlet nodes
		UpdateHeatingCoil( CoilNum );

		// Report the current HeatingCoil
		ReportHeatingCoil( CoilNum );

		if ( present( QCoilActual ) ) {
			QCoilActual = QCoilActual2;
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetHeatingCoilInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for coils and stores it in coil data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using CurveManager::GetCurveIndex;
		using namespace DataIPShortCuts;
		using GlobalNames::VerifyUniqueCoilName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetHeatingCoilInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoilNum; // The HeatingCoil that you are currently loading input into
		int NumElecCoil;
		int NumElecCoilMultiStage;
		int NumGasCoil;
		int NumGasCoilMultiStage;
		int ElecCoilNum;
		int GasCoilNum;
		int DesuperheaterCoilNum; // Index to desuperheater heating coil
		int RemainingCoils; // Index for error checking DO loop for desuperheater coils on remaining heating coil
		static int SourceIndexNum( 0 ); // Index to reclaim heating source (condenser) of a specific type
		std::string SourceTypeString; // character string used in error message for desuperheating coil
		std::string SourceNameString; // character string used in error message for desuperheating coil
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file
		int NumAlphas;
		int NumNums;
		int IOStat;
		int StageNum;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool DXCoilErrFlag; // Used in GetDXCoil mining functions
		bool errFlag;
		// Flow

		NumElecCoil = GetNumObjectsFound( "Coil:Heating:Electric" );
		NumElecCoilMultiStage = GetNumObjectsFound( "Coil:Heating:Electric:MultiStage" );
		NumGasCoil = GetNumObjectsFound( "Coil:Heating:Gas" );
		NumGasCoilMultiStage = GetNumObjectsFound( "Coil:Heating:Gas:MultiStage" );
		NumDesuperheaterCoil = GetNumObjectsFound( "Coil:Heating:Desuperheater" );
		NumHeatingCoils = NumElecCoil + NumElecCoilMultiStage + NumGasCoil + NumGasCoilMultiStage + NumDesuperheaterCoil;
		if ( NumHeatingCoils > 0 ) {
			HeatingCoil.allocate( NumHeatingCoils );
			HeatingCoilNumericFields.allocate( NumHeatingCoils );
			ValidSourceType.dimension( NumHeatingCoils, false );
			CheckEquipName.dimension( NumHeatingCoils, true );
		}

		GetObjectDefMaxArgs( "Coil:Heating:Electric", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Heating:Electric:MultiStage", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Heating:Gas", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Heating:Gas:MultiStage", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Heating:Desuperheater", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		Numbers.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		// Get the data for electric heating coils
		for ( ElecCoilNum = 1; ElecCoilNum <= NumElecCoil; ++ElecCoilNum ) {

			CoilNum = ElecCoilNum;

			CurrentModuleObject = "Coil:Heating:Electric";

			GetObjectItem( CurrentModuleObject, ElecCoilNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			HeatingCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			HeatingCoilNumericFields( CoilNum ).FieldNames = "";
			HeatingCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), HeatingCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			HeatingCoil( CoilNum ).Name = Alphas( 1 );
			HeatingCoil( CoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				HeatingCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				HeatingCoil( CoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( HeatingCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			HeatingCoil( CoilNum ).HeatingCoilType = "Heating";
			HeatingCoil( CoilNum ).HeatingCoilModel = "Electric";
			HeatingCoil( CoilNum ).HCoilType_Num = Coil_HeatingElectric;

			HeatingCoil( CoilNum ).Efficiency = Numbers( 1 );
			HeatingCoil( CoilNum ).NominalCapacity = Numbers( 2 );
			HeatingCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			HeatingCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			HeatingCoil( CoilNum ).TempSetPointNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			// Setup Report variables for the Electric Coils
			// CurrentModuleObject = "Coil:Heating:Electric"
			SetupOutputVariable( "Heating Coil Air Heating Energy [J]", HeatingCoil( CoilNum ).HeatingCoilLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Air Heating Rate [W]", HeatingCoil( CoilNum ).HeatingCoilRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Electric Energy [J]", HeatingCoil( CoilNum ).ElecUseLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Electric", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Electric Power [W]", HeatingCoil( CoilNum ).ElecUseRate, "System", "Average", HeatingCoil( CoilNum ).Name );

		}

		// Get the data for electric heating coils
		for ( ElecCoilNum = 1; ElecCoilNum <= NumElecCoilMultiStage; ++ElecCoilNum ) {

			CoilNum = NumElecCoil + ElecCoilNum;

			CurrentModuleObject = "Coil:Heating:Electric:MultiStage";

			GetObjectItem( CurrentModuleObject, ElecCoilNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			HeatingCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			HeatingCoilNumericFields( CoilNum ).FieldNames = "";
			HeatingCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), HeatingCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			HeatingCoil( CoilNum ).Name = Alphas( 1 );
			HeatingCoil( CoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				HeatingCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				HeatingCoil( CoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( HeatingCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			HeatingCoil( CoilNum ).HeatingCoilType = "Heating";
			HeatingCoil( CoilNum ).HeatingCoilModel = "ElectricMultiStage";
			HeatingCoil( CoilNum ).HCoilType_Num = Coil_HeatingElectric_MultiStage;

			HeatingCoil( CoilNum ).NumOfStages = Numbers( 1 );

			HeatingCoil( CoilNum ).MSEfficiency.allocate( HeatingCoil( CoilNum ).NumOfStages );
			HeatingCoil( CoilNum ).MSNominalCapacity.allocate( HeatingCoil( CoilNum ).NumOfStages );

			for ( StageNum = 1; StageNum <= HeatingCoil( CoilNum ).NumOfStages; ++StageNum ) {

				HeatingCoil( CoilNum ).MSEfficiency( StageNum ) = Numbers( StageNum * 2 );
				HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) = Numbers( StageNum * 2 + 1 );

			}

			HeatingCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			HeatingCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			HeatingCoil( CoilNum ).TempSetPointNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			// Setup Report variables for the Electric Coils
			// CurrentModuleObject = "Coil:Heating:Electric:MultiStage"
			SetupOutputVariable( "Heating Coil Air Heating Energy [J]", HeatingCoil( CoilNum ).HeatingCoilLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Air Heating Rate [W]", HeatingCoil( CoilNum ).HeatingCoilRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Electric Energy [J]", HeatingCoil( CoilNum ).ElecUseLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Electric", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Electric Power [W]", HeatingCoil( CoilNum ).ElecUseRate, "System", "Average", HeatingCoil( CoilNum ).Name );

		}

		// Get the data for for gas heating coils
		for ( GasCoilNum = 1; GasCoilNum <= NumGasCoil; ++GasCoilNum ) {

			CoilNum = NumElecCoil + NumElecCoilMultiStage + GasCoilNum;

			CurrentModuleObject = "Coil:Heating:Gas";

			GetObjectItem( CurrentModuleObject, GasCoilNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			HeatingCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			HeatingCoilNumericFields( CoilNum ).FieldNames = "";
			HeatingCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), HeatingCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			HeatingCoil( CoilNum ).Name = Alphas( 1 );
			HeatingCoil( CoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				HeatingCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				HeatingCoil( CoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( HeatingCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			HeatingCoil( CoilNum ).HeatingCoilType = "Heating";
			HeatingCoil( CoilNum ).HeatingCoilModel = "Gas";
			HeatingCoil( CoilNum ).HCoilType_Num = Coil_HeatingGas;

			HeatingCoil( CoilNum ).Efficiency = Numbers( 1 );
			HeatingCoil( CoilNum ).NominalCapacity = Numbers( 2 );
			HeatingCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			HeatingCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			HeatingCoil( CoilNum ).TempSetPointNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			//parasitic electric load associated with the gas heating coil
			HeatingCoil( CoilNum ).ParasiticElecLoad = Numbers( 3 );

			HeatingCoil( CoilNum ).PLFCurveIndex = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number

			//parasitic gas load associated with the gas heating coil (standing pilot light)
			HeatingCoil( CoilNum ).ParasiticGasCapacity = Numbers( 4 );

			// Setup Report variables for the Gas Coils
			// CurrentModuleObject = "Coil:Heating:Gas"
			SetupOutputVariable( "Heating Coil Air Heating Energy [J]", HeatingCoil( CoilNum ).HeatingCoilLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Air Heating Rate [W]", HeatingCoil( CoilNum ).HeatingCoilRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Gas Energy [J]", HeatingCoil( CoilNum ).GasUseLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Gas", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Gas Rate [W]", HeatingCoil( CoilNum ).GasUseRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Electric Energy [J]", HeatingCoil( CoilNum ).ElecUseLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Electricity", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Electric Power [W]", HeatingCoil( CoilNum ).ElecUseRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Runtime Fraction []", HeatingCoil( CoilNum ).RTF, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Ancillary Gas Rate [W]", HeatingCoil( CoilNum ).ParasiticGasRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Ancillary Gas Energy [J]", HeatingCoil( CoilNum ).ParasiticGasLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Gas", "Heating", _, "System" );

		}

		// Get the data for for gas heating coils
		for ( GasCoilNum = 1; GasCoilNum <= NumGasCoilMultiStage; ++GasCoilNum ) {

			CoilNum = NumElecCoil + NumElecCoilMultiStage + NumGasCoil + GasCoilNum;

			CurrentModuleObject = "Coil:Heating:Gas:MultiStage";

			GetObjectItem( CurrentModuleObject, GasCoilNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			HeatingCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			HeatingCoilNumericFields( CoilNum ).FieldNames = "";
			HeatingCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), HeatingCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			HeatingCoil( CoilNum ).Name = Alphas( 1 );
			HeatingCoil( CoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				HeatingCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				HeatingCoil( CoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( HeatingCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			HeatingCoil( CoilNum ).HeatingCoilType = "Heating";
			HeatingCoil( CoilNum ).HeatingCoilModel = "GasMultiStage";
			HeatingCoil( CoilNum ).HCoilType_Num = Coil_HeatingGas_MultiStage;

			HeatingCoil( CoilNum ).ParasiticGasCapacity = Numbers( 1 );

			HeatingCoil( CoilNum ).NumOfStages = Numbers( 2 );

			HeatingCoil( CoilNum ).MSEfficiency.allocate( HeatingCoil( CoilNum ).NumOfStages );
			HeatingCoil( CoilNum ).MSNominalCapacity.allocate( HeatingCoil( CoilNum ).NumOfStages );
			HeatingCoil( CoilNum ).MSParasiticElecLoad.allocate( HeatingCoil( CoilNum ).NumOfStages );

			for ( StageNum = 1; StageNum <= HeatingCoil( CoilNum ).NumOfStages; ++StageNum ) {

				HeatingCoil( CoilNum ).MSEfficiency( StageNum ) = Numbers( StageNum * 3 );
				HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) = Numbers( StageNum * 3 + 1 );
				HeatingCoil( CoilNum ).MSParasiticElecLoad( StageNum ) = Numbers( StageNum * 3 + 2 );

			}

			HeatingCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			HeatingCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			HeatingCoil( CoilNum ).TempSetPointNodeNum = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			//parasitic electric load associated with the gas heating coil
			HeatingCoil( CoilNum ).ParasiticElecLoad = Numbers( 10 );

			HeatingCoil( CoilNum ).PLFCurveIndex = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number

			//parasitic gas load associated with the gas heating coil (standing pilot light)

			// Setup Report variables for the Gas Coils
			// CurrentModuleObject = "Coil:Heating:Gas:MultiStage"
			SetupOutputVariable( "Heating Coil Air Heating Energy [J]", HeatingCoil( CoilNum ).HeatingCoilLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Air Heating Rate [W]", HeatingCoil( CoilNum ).HeatingCoilRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Gas Energy [J]", HeatingCoil( CoilNum ).GasUseLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Gas", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Gas Rate [W]", HeatingCoil( CoilNum ).GasUseRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Electric Energy [J]", HeatingCoil( CoilNum ).ElecUseLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Electricity", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Electric Power [W]", HeatingCoil( CoilNum ).ElecUseRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Runtime Fraction []", HeatingCoil( CoilNum ).RTF, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Ancillary Gas Rate [W]", HeatingCoil( CoilNum ).ParasiticGasRate, "System", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Ancillary Gas Energy [J]", HeatingCoil( CoilNum ).ParasiticGasLoad, "System", "Sum", HeatingCoil( CoilNum ).Name, _, "Gas", "Heating", _, "System" );

		}

		// Get the data for for desuperheater heating coils
		for ( DesuperheaterCoilNum = 1; DesuperheaterCoilNum <= NumDesuperheaterCoil; ++DesuperheaterCoilNum ) {

			CoilNum = NumElecCoil + NumElecCoilMultiStage + NumGasCoil + NumGasCoilMultiStage + DesuperheaterCoilNum;

			CurrentModuleObject = "Coil:Heating:Desuperheater";

			GetObjectItem( CurrentModuleObject, DesuperheaterCoilNum, Alphas, NumAlphas, Numbers, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			HeatingCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			HeatingCoilNumericFields( CoilNum ).FieldNames = "";
			HeatingCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), HeatingCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			HeatingCoil( CoilNum ).Name = Alphas( 1 );
			HeatingCoil( CoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				HeatingCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				HeatingCoil( CoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( HeatingCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": Invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			//       check availability schedule for values between 0 and 1
			if ( HeatingCoil( CoilNum ).SchedPtr > 0 ) {
				if ( ! CheckScheduleValueMinMax( HeatingCoil( CoilNum ).SchedPtr, ">=", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + " = \"" + HeatingCoil( CoilNum ).Name + "\"" );
					ShowContinueError( "Error found in " + cAlphaFields( 2 ) + " = " + Alphas( 2 ) );
					ShowContinueError( "Schedule values must be (>=0., <=1.)" );
					ErrorsFound = true;
				}
			}

			HeatingCoil( CoilNum ).HeatingCoilType = "Heating";
			HeatingCoil( CoilNum ).HeatingCoilModel = "Desuperheater";
			HeatingCoil( CoilNum ).HCoilType_Num = Coil_HeatingDesuperheater;

			//HeatingCoil(CoilNum)%Efficiency       = Numbers(1)
			//(Numbers(1)) error limits checked and defaults applied on efficiency after
			//       identifying souce type.

			HeatingCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			HeatingCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			// Find the DX equipment index associated with the desuperheater heating coil.
			// The CoilNum may not be found here when zone heating equip. exists. Check again in InitHeatingCoil.
			// (when zone equipment heating coils are included in the input, the air loop DX equipment has not yet been read in)
			if ( SameString( Alphas( 5 ), "Refrigeration:CompressorRack" ) ) {
				HeatingCoil( CoilNum ).ReclaimHeatingSource = COMPRESSORRACK_REFRIGERATEDCASE;
				GetRefrigeratedRackIndex( Alphas( 6 ), HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum, RefrigSystemTypeRack, DXCoilErrFlag, Alphas( 5 ) );
				if ( HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum > 0 ) ValidSourceType( CoilNum ) = true;
			} else if ( ( SameString( Alphas( 5 ), "Refrigeration:Condenser:AirCooled" ) ) || ( SameString( Alphas( 5 ), "Refrigeration:Condenser:EvaporativeCooled" ) ) || ( SameString( Alphas( 5 ), "Refrigeration:Condenser:WaterCooled" ) ) ) {
				HeatingCoil( CoilNum ).ReclaimHeatingSource = CONDENSER_REFRIGERATION;
				GetRefrigeratedRackIndex( Alphas( 6 ), HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum, RefrigSystemTypeDetailed, DXCoilErrFlag, Alphas( 5 ) );
				if ( HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum > 0 ) ValidSourceType( CoilNum ) = true;
			} else if ( SameString( Alphas( 5 ), "Coil:Cooling:DX:SingleSpeed" ) ) {
				HeatingCoil( CoilNum ).ReclaimHeatingSource = COIL_DX_COOLING;
				GetDXCoilIndex( Alphas( 6 ), HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum, DXCoilErrFlag, Alphas( 5 ) );
				if ( HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum > 0 ) ValidSourceType( CoilNum ) = true;
			} else if ( SameString( Alphas( 5 ), "Coil:Cooling:DX:TwoSpeed" ) ) {
				HeatingCoil( CoilNum ).ReclaimHeatingSource = COIL_DX_MULTISPEED;
				GetDXCoilIndex( Alphas( 6 ), HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum, DXCoilErrFlag, Alphas( 5 ) );
				if ( HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum > 0 ) ValidSourceType( CoilNum ) = true;
			} else if ( SameString( Alphas( 5 ), "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {
				HeatingCoil( CoilNum ).ReclaimHeatingSource = COIL_DX_MULTIMODE;
				GetDXCoilIndex( Alphas( 6 ), HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum, DXCoilErrFlag, Alphas( 5 ) );
				if ( HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum > 0 ) ValidSourceType( CoilNum ) = true;
			} else {
				ShowSevereError( CurrentModuleObject + ", \"" + HeatingCoil( CoilNum ).Name + "\" valid desuperheater heat source object type not found: " + Alphas( 5 ) );
				ShowContinueError( "Valid desuperheater heat source objects are:" );
				ShowContinueError( "Refrigeration:CompressorRack, Coil:Cooling:DX:SingleSpeed, Refrigeration:Condenser:AirCooled, Refrigeration:Condenser:EvaporativeCooled, Refrigeration:Condenser:WaterCooled,Coil:Cooling:DX:TwoSpeed, and Coil:Cooling:DX:TwoStageWithHumidityControlMode" );
				ErrorsFound = true;
			}

			if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
				if ( lNumericBlanks( 1 ) ) {
					HeatingCoil( CoilNum ).Efficiency = 0.8;
				} else {
					HeatingCoil( CoilNum ).Efficiency = Numbers( 1 );
					if ( Numbers( 1 ) < 0.0 || Numbers( 1 ) > 0.9 ) {
						ShowSevereError( CurrentModuleObject + ", \"" + HeatingCoil( CoilNum ).Name + "\" heat reclaim recovery efficiency must be >= 0 and <=0.9" );
						ErrorsFound = true;
					}
				}
			} else {
				if ( lNumericBlanks( 1 ) ) {
					HeatingCoil( CoilNum ).Efficiency = 0.25;
				} else {
					HeatingCoil( CoilNum ).Efficiency = Numbers( 1 );
					if ( Numbers( 1 ) < 0.0 || Numbers( 1 ) > 0.3 ) {
						ShowSevereError( CurrentModuleObject + ", \"" + HeatingCoil( CoilNum ).Name + "\" heat reclaim recovery efficiency must be >= 0 and <=0.3" );
						ErrorsFound = true;
					}
				}
			}

			HeatingCoil( CoilNum ).ReclaimHeatingCoilName = Alphas( 6 );

			HeatingCoil( CoilNum ).TempSetPointNodeNum = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

			//parasitic electric load associated with the desuperheater heating coil
			HeatingCoil( CoilNum ).ParasiticElecLoad = Numbers( 2 );

			if ( Numbers( 2 ) < 0.0 ) {
				ShowSevereError( CurrentModuleObject + ", \"" + HeatingCoil( CoilNum ).Name + "\" parasitic electric load must be >= 0" );
				ErrorsFound = true;
			}

			// Setup Report variables for the Desuperheater Heating Coils
			// CurrentModuleObject = "Coil:Heating:Desuperheater"
			SetupOutputVariable( "Heating Coil Air Heating Energy [J]", HeatingCoil( CoilNum ).HeatingCoilLoad, "HVAC", "Sum", HeatingCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Air Heating Rate [W]", HeatingCoil( CoilNum ).HeatingCoilRate, "HVAC", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Electric Energy [J]", HeatingCoil( CoilNum ).ElecUseLoad, "HVAC", "Sum", HeatingCoil( CoilNum ).Name, _, "Electricity", "Heating", _, "System" );
			SetupOutputVariable( "Heating Coil Electric Power [W]", HeatingCoil( CoilNum ).ElecUseRate, "HVAC", "Average", HeatingCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil Runtime Fraction []", HeatingCoil( CoilNum ).RTF, "HVAC", "Average", HeatingCoil( CoilNum ).Name );

		}

		// perform error check to make sure duplicate heating sources are not used (i.e. 2 desuperheating coils cannot
		// use the same heat source). This error check will be expanded in the future to check for duplicates in
		// desuperheaters used for water heating purposed.
		for ( CoilNum = NumElecCoil + NumElecCoilMultiStage + NumGasCoil + NumGasCoilMultiStage + 1; CoilNum <= NumHeatingCoils; ++CoilNum ) {
			for ( RemainingCoils = CoilNum + 1; RemainingCoils <= NumHeatingCoils; ++RemainingCoils ) {
				if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == HeatingCoil( RemainingCoils ).ReclaimHeatingSource && HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum == HeatingCoil( RemainingCoils ).ReclaimHeatingSourceIndexNum ) {
					SourceIndexNum = HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum;
					if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE ) {
						SourceTypeString = "Refrigeration:CompressorRack";
						SourceNameString = HeatReclaimRefrigeratedRack( SourceIndexNum ).Name;
					}
					if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
						SourceNameString = HeatReclaimRefrigCondenser( SourceIndexNum ).Name;
						if ( HeatReclaimRefrigCondenser( SourceIndexNum ).SourceType == RefrigCondenserTypeAir ) SourceTypeString = "Refrigeration:Condenser:AirCooled";
						if ( HeatReclaimRefrigCondenser( SourceIndexNum ).SourceType == RefrigCondenserTypeEvap ) SourceTypeString = "Refrigeration:Condenser:EvaporativeCooled";
						if ( HeatReclaimRefrigCondenser( SourceIndexNum ).SourceType == RefrigCondenserTypeWater ) SourceTypeString = "Refrigeration:Condenser:WaterCooled";
					}
					if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_COOLING ) {
						SourceTypeString = "Coil:Cooling:DX:SingleSpeed";
						SourceNameString = HeatReclaimDXCoil( SourceIndexNum ).Name;
					}
					if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTISPEED ) {
						SourceTypeString = "Coil:Cooling:DX:TwoSpeed";
						SourceNameString = HeatReclaimDXCoil( SourceIndexNum ).Name;
					}
					if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTIMODE ) {
						SourceTypeString = "Coil:Cooling:DX:TwoStageWithHumidityControlMode";
						SourceNameString = HeatReclaimDXCoil( SourceIndexNum ).Name;
					}
					ShowSevereError( "Coil:Heating:Desuperheater, \"" + HeatingCoil( CoilNum ).Name + "\" and \"" + HeatingCoil( RemainingCoils ).Name + "\" cannot use the same" );
					ShowContinueError( " heat source object " + SourceTypeString + ", \"" + SourceNameString + "\"" );
					ErrorsFound = true;
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitHeatingCoil(
		int const CoilNum,
		bool const FirstHVACIteration,
		Real64 const QCoilRequired
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       B. Griffith, May 2009 added EMS setpoint check
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the HeatingCoil Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::SameString;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirInletNode; // coil air inlet node number
		int AirOutletNode; // coil air outlet node number
		int ControlNode; // coil control node number
		int RackNum; // Index to refrigerated case compressor rack
		int CondNum; // Index to refrigeration condenser
		int DXCoilNum; // Index to DX cooling coil
		static int ValidSourceTypeCounter( 0 ); // Counter used to determine if desuperheater source name is valid
		static bool HeatingCoilFatalError( false ); // used for error checking
		static Array1D_bool MySPTestFlag; // used for error checking
		static Array1D_bool ShowSingleWarning; // Used for single warning message for desuperheater coil
		static Array1D_bool MyEnvrnFlag; // one time environment flag

		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumHeatingCoils );
			MySizeFlag.allocate( NumHeatingCoils );
			ShowSingleWarning.allocate( NumHeatingCoils );
			MySPTestFlag.allocate( NumHeatingCoils );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			ShowSingleWarning = true;
			MyOneTimeFlag = false;
			MySPTestFlag = true;

		}

		if ( ! SysSizingCalc && MySizeFlag( CoilNum ) ) {
			// for each coil, do the sizing once.
			SizeHeatingCoil( CoilNum );

			MySizeFlag( CoilNum ) = false;
		}

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.
		//First set the conditions for the air into the coil model
		AirInletNode = HeatingCoil( CoilNum ).AirInletNodeNum;
		AirOutletNode = HeatingCoil( CoilNum ).AirOutletNodeNum;
		ControlNode = HeatingCoil( CoilNum ).TempSetPointNodeNum;
		HeatingCoil( CoilNum ).InletAirMassFlowRate = Node( AirInletNode ).MassFlowRate;
		HeatingCoil( CoilNum ).InletAirTemp = Node( AirInletNode ).Temp;
		HeatingCoil( CoilNum ).InletAirHumRat = Node( AirInletNode ).HumRat;
		HeatingCoil( CoilNum ).InletAirEnthalpy = Node( AirInletNode ).Enthalpy;

		// Set the reporting variables to zero at each timestep.
		HeatingCoil( CoilNum ).HeatingCoilLoad = 0.0;
		HeatingCoil( CoilNum ).GasUseLoad = 0.0;
		HeatingCoil( CoilNum ).ElecUseLoad = 0.0;
		HeatingCoil( CoilNum ).RTF = 0.0;

		// If a temperature setpoint controlled coil must set the desired outlet temp everytime
		if ( ControlNode == 0 ) {
			HeatingCoil( CoilNum ).DesiredOutletTemp = 0.0;
		} else if ( ControlNode == AirOutletNode ) {
			HeatingCoil( CoilNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint;
		} else {
			HeatingCoil( CoilNum ).DesiredOutletTemp = Node( ControlNode ).TempSetPoint - ( Node( ControlNode ).Temp - Node( AirOutletNode ).Temp );
		}

		if ( QCoilRequired == SensedLoadFlagValue && MySPTestFlag( CoilNum ) && HeatingCoil( CoilNum ).HCoilType_Num != Coil_HeatingElectric_MultiStage && HeatingCoil( CoilNum ).HCoilType_Num != Coil_HeatingGas_MultiStage ) {

			//   If the coil is temperature controlled (QCoilReq == -999.0), both a control node and setpoint are required.
			if ( ! SysSizingCalc && DoSetPointTest ) {
				//     3 possibilities here:
				//     1) TempSetPointNodeNum .GT. 0 and TempSetPoint /= SensedNodeFlagValue, this is correct
				//     2) TempSetPointNodeNum .EQ. 0, this is not correct, control node is required
				//     3) TempSetPointNodeNum .GT. 0 and TempSetPoint == SensedNodeFlagValue, this is not correct, missing temperature setpoint
				//     test 2) here (fatal message)
				if ( ControlNode == 0 ) {
					ShowSevereError( cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + " \"" + HeatingCoil( CoilNum ).Name + "\"" );
					ShowContinueError( "... Missing control node for heating coil." );
					ShowContinueError( "... enter a control node name in the coil temperature setpoint node field for this heating coil." );
					ShowContinueError( "... use a Setpoint Manager to establish a setpoint at the coil temperature setpoint node." );
					HeatingCoilFatalError = true;
					//     test 3) here (fatal message)
				} else { //IF(ControlNode .GT. 0)THEN
					if ( Node( ControlNode ).TempSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							ShowSevereError( cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + " \"" + HeatingCoil( CoilNum ).Name + "\"" );
							ShowContinueError( "... Missing temperature setpoint for heating coil." );
							ShowContinueError( "... use a Setpoint Manager to establish a setpoint at the coil temperature setpoint node." );
							HeatingCoilFatalError = true;
						} else {
							CheckIfNodeSetPointManagedByEMS( ControlNode, iTemperatureSetPoint, HeatingCoilFatalError );
							if ( HeatingCoilFatalError ) {
								ShowSevereError( cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + " \"" + HeatingCoil( CoilNum ).Name + "\"" );
								ShowContinueError( "... Missing temperature setpoint for heating coil." );
								ShowContinueError( "... use a Setpoint Manager to establish a setpoint at the coil temperature setpoint node." );
								ShowContinueError( "... or use an EMS Actuator to establish a setpoint at the coil temperature setpoint node." );
							}
						}
					}
				}
				MySPTestFlag( CoilNum ) = false;
			}
		} else if ( MySPTestFlag( CoilNum ) ) {
			//  If QCoilReq /= SensedLoadFlagValue, the coil is load controlled and does not require a control node
			//   4 possibilities here:
			//   1) TempSetPointNodeNum .EQ. 0 and TempSetPoint == SensedNodeFlagValue, this is correct
			//   2) TempSetPointNodeNum .EQ. 0 and TempSetPoint /= SensedNodeFlagValue, this may be correct,
			//      (if no control node specified and SP on heating coil outlet do not show warning, other SP managers may be using SP)
			//   3) TempSetPointNodeNum .GT. 0 and TempSetPoint == SensedNodeFlagValue, control node not required if load based control
			//   4) TempSetPointNodeNum .GT. 0 and TempSetPoint /= SensedNodeFlagValue, control node not required if load based control
			//   test 3) and 4) here (warning only)
			if ( ControlNode > 0 ) {
				ShowWarningError( cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + " \"" + HeatingCoil( CoilNum ).Name + "\"" );
				ShowContinueError( " The Temperature Setpoint Node Name input is not required for this heating coil because" );
				ShowContinueError( " this heating coil is controlled based on the load calculated by the thermostat." );
				ShowContinueError( "... this heating coil is not controlled by using a temperature setpoint manager." );
				ShowContinueError( "... if a temperature setpoint is placed at the outlet node of this heating coil, that temperature setpoint will not be used." );
				ShowContinueError( "... leaving the input field \"Temperature Setpoint Node Name\" blank will eliminate this warning." );
			}
			MySPTestFlag( CoilNum ) = false;
		}

		// delay fatal error until all coils are called
		if ( ! FirstHVACIteration && HeatingCoilFatalError ) {
			ShowFatalError( "... errors found in heating coil input." );
		}

		// Find the heating source index for the desuperheater heating coil if not already found. This occurs when zone heating
		// equip. exists. (when zone equipment heating coils are included in the input, the air loop DX equipment has not yet been read)
		// Issue a single warning if the coil is not found and continue the simulation
		if ( ! ValidSourceType( CoilNum ) && ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingDesuperheater ) && ShowSingleWarning( CoilNum ) ) {
			++ValidSourceTypeCounter;
			if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE ) {
				for ( RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum ) {
					if ( ! SameString( HeatReclaimRefrigeratedRack( RackNum ).Name, HeatingCoil( CoilNum ).ReclaimHeatingCoilName ) ) continue;
					HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum = RackNum;
					if ( allocated( HeatReclaimRefrigeratedRack ) ) ValidSourceType( CoilNum ) = true;
					break;
				}
			} else if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
				for ( CondNum = 1; CondNum <= NumRefrigCondensers; ++CondNum ) {
					if ( ! SameString( HeatReclaimRefrigCondenser( CondNum ).Name, HeatingCoil( CoilNum ).ReclaimHeatingCoilName ) ) continue;
					HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum = CondNum;
					if ( allocated( HeatReclaimRefrigCondenser ) ) ValidSourceType( CoilNum ) = true;
					break;
				}
			} else if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_COOLING || HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTISPEED || HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTIMODE ) {
				for ( DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum ) {
					if ( ! SameString( HeatReclaimDXCoil( DXCoilNum ).Name, HeatingCoil( CoilNum ).ReclaimHeatingCoilName ) ) continue;
					HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum = DXCoilNum;
					if ( allocated( HeatReclaimDXCoil ) ) ValidSourceType( CoilNum ) = true;
					break;
				}
			}
			if ( ( ValidSourceTypeCounter > NumDesuperheaterCoil * 2 ) && ShowSingleWarning( CoilNum ) && ! ValidSourceType( CoilNum ) ) {
				ShowWarningError( "Coil:Heating:Desuperheater, \"" + HeatingCoil( CoilNum ).Name + "\" desuperheater heat source object name not found: " + HeatingCoil( CoilNum ).ReclaimHeatingCoilName );
				ShowContinueError( " Desuperheater heating coil is not modeled and simulation continues." );
				ShowSingleWarning( CoilNum ) = false;
			}
		}

	}

	void
	SizeHeatingCoil( int const CoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  Mar 2014 FSEC, moved calculations to common routine in ReportSizingManager

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Heating Coil Components for which nominal capcities have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains heating capacities from the zone or system sizing arrays or parent object as necessary.
		// heating coil or other routine sets up any required data varaibles (e.g., DataCoilIsSuppHeater, TermUnitPIU, etc.),
		// sizing variable (e.g., HeatingCoil( CoilNum ).NominalCapacity in this routine since it can be multi-staged and new routine
		// currently only handles single values) and associated string representing that sizing variable.
		// RequestSizing functions handles the actual sizing and reporting.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using namespace OutputReportPredefined;
		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeHeatingCoil: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompName; // component name
		std::string	CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		bool IsAutoSize; // Indicator to autosize for reporting
		bool bPRINT = true; // TRUE if sizing is reported to output (eio)
		bool ThisStageAutoSize; // Indicator to autosize at each stage for reporting
		Real64 NominalCapacityDes; // Autosized nominal capacity for reporting
		Real64 NominalCapacityUser; // Hardsized nominal capacity for reporting
		Real64 TempCap; // autosized capacity of heating coil [W]
		int StageNum; // actual stage of multi-stage heating coil
		int NumOfStages; // total number of stages of multi-stage heating coil
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int NumCoilsSized = 0; // counter used to deallocate temporary string array after all coils have been sized
		Real64 TempSize; // sizing variable temp value

		if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingElectric_MultiStage ) {
			FieldNum = 1 + ( HeatingCoil( CoilNum ).NumOfStages * 2 );
			TempCap = HeatingCoil( CoilNum ).MSNominalCapacity( HeatingCoil( CoilNum ).NumOfStages );
		} else if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingGas_MultiStage ) {
			FieldNum = 1 + ( HeatingCoil( CoilNum ).NumOfStages * 3 );
			TempCap = HeatingCoil( CoilNum ).MSNominalCapacity( HeatingCoil( CoilNum ).NumOfStages );
		} else if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingDesuperheater ) {
			return; // no autosizable inputs for desupterheater
		} else {
			FieldNum = 2;
			TempCap = HeatingCoil( CoilNum ).NominalCapacity;
		}
		SizingString = HeatingCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [W]";
		CompType = "Coil:" + HeatingCoil( CoilNum ).HeatingCoilType + ':' + HeatingCoil( CoilNum ).HeatingCoilModel;
		CompName = HeatingCoil( CoilNum ).Name;
		DataCoilIsSuppHeater = CoilIsSuppHeater; // set global instead of using optional argument
		DataCoolCoilCap = 0.0; // global only used for heat pump heating coils, non-HP heating coils are sized with other global variables

		if ( TempCap == AutoSize ) {
			if ( HeatingCoil( CoilNum ).DesiccantRegenerationCoil ) {
				DataDesicRegCoil = true;
				bPRINT = false;
				DataDesicDehumNum = HeatingCoil( CoilNum ).DesiccantDehumNum;
				TempSize = AutoSize;
				RequestSizing( CompType, CompName, HeatingCoilDesAirInletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
				DataDesInletAirTemp = TempSize;
				TempSize = AutoSize;
				RequestSizing( CompType, CompName, HeatingCoilDesAirOutletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
				DataDesOutletAirTemp = TempSize;
				if ( CurOASysNum > 0 ) {
					OASysEqSizing( CurOASysNum ).AirFlow = true;
					OASysEqSizing( CurOASysNum ).AirVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
				}
				DataDesicDehumNum = 0;
				bPRINT = true;
			}
		}
		RequestSizing( CompType, CompName, HeatingCapacitySizing, SizingString, TempCap, bPRINT, RoutineName );
		DataCoilIsSuppHeater = false; // reset global to false so other heating coils are not affected
		DataDesicRegCoil = false; // reset global to false so other heating coils are not affected
		DataDesInletAirTemp = 0.0; // reset global data to zero so other heating coils are not 
		DataDesOutletAirTemp = 0.0; // reset global data to zero so other heating coils are not affected

		if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingElectric_MultiStage || HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingGas_MultiStage ) {
			HeatingCoil( CoilNum ).MSNominalCapacity( HeatingCoil( CoilNum ).NumOfStages ) = TempCap;
			IsAutoSize = false;
			if ( any_eq( HeatingCoil( CoilNum ).MSNominalCapacity, AutoSize ) ) {
				IsAutoSize = true;
			}
			if ( IsAutoSize ) {
				NumOfStages = HeatingCoil( CoilNum ).NumOfStages;
				for ( StageNum = NumOfStages-1; StageNum >= 1; --StageNum ) {
					if ( HeatingCoil( CoilNum ).HCoilType_Num == Coil_HeatingElectric_MultiStage ) {
						FieldNum = 1 + ( StageNum * 2 );
					} else {
						FieldNum = 1 + ( StageNum * 3 );
					}
					SizingString = HeatingCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [W]";
					if ( HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) == AutoSize ) {
						ThisStageAutoSize = true;
					}
					NominalCapacityDes = TempCap * StageNum / NumOfStages;
					if ( ThisStageAutoSize ) {
						HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) = NominalCapacityDes;
						ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, NominalCapacityDes );
					} else {
						if ( HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) > 0.0 && NominalCapacityDes > 0.0 ) {
							NominalCapacityUser = TempCap * StageNum / NumOfStages;  // HeatingCoil( CoilNum ).MSNominalCapacity( StageNum );
							ReportSizingOutput( CompType, CompName, "Design Size " + SizingString, NominalCapacityDes, "User-Specified " + SizingString, NominalCapacityUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( NominalCapacityDes - NominalCapacityUser ) / NominalCapacityUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeHeatingCoil: Potential issue with equipment sizing for " + CompType + ", " + CompName );
									ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( NominalCapacityUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( NominalCapacityDes, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			} else { // No autosize
				NumOfStages = HeatingCoil( CoilNum ).NumOfStages;
				for ( StageNum = NumOfStages-1; StageNum >= 1; --StageNum ) {
					if ( HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) > 0.0 ) {
						ReportSizingOutput( CompType, CompName, "User-Specified " + SizingString, HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) );
					}
				}
			}
			// Ensure capacity at lower Stage must be lower or equal to the capacity at higher Stage.
			for ( StageNum = 1; StageNum <= HeatingCoil( CoilNum ).NumOfStages - 1; ++StageNum ) {
				if ( HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) > HeatingCoil( CoilNum ).MSNominalCapacity( StageNum + 1 ) ) {
					ShowSevereError( "SizeHeatingCoil: " + HeatingCoil( CoilNum ).HeatingCoilType + ' ' + HeatingCoil( CoilNum ).Name + ", Stage " + TrimSigDigits( StageNum ) + " Nominal Capacity (" + RoundSigDigits( HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ), 2 ) + " W) must be less than or equal to Stage " + TrimSigDigits( StageNum + 1 ) + " Nominal Capacity (" + RoundSigDigits( HeatingCoil( CoilNum ).MSNominalCapacity( StageNum + 1 ), 2 ) + " W)." );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}
		} else { // not a multi-speed coil
			HeatingCoil( CoilNum ).NominalCapacity = TempCap;
		}

		if ( ++NumCoilsSized == NumHeatingCoils ) HeatingCoilNumericFields.deallocate(); // remove temporary array for field names at end of sizing

		//create predefined report entries
		{ auto const SELECT_CASE_var( HeatingCoil( CoilNum ).HCoilType_Num );
		if ( SELECT_CASE_var == Coil_HeatingElectric ) {
			PreDefTableEntry( pdchHeatCoilType, HeatingCoil( CoilNum ).Name, "Coil:Heating:Electric" );
			PreDefTableEntry( pdchHeatCoilNomCap, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).NominalCapacity );
			PreDefTableEntry( pdchHeatCoilNomEff, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).Efficiency );
		} else if ( SELECT_CASE_var == Coil_HeatingElectric_MultiStage ) {
			PreDefTableEntry( pdchHeatCoilType, HeatingCoil( CoilNum ).Name, "Coil:Heating:Electric:MultiStage" );
			PreDefTableEntry( pdchHeatCoilNomCap, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).MSNominalCapacity( HeatingCoil( CoilNum ).NumOfStages ) );
			PreDefTableEntry( pdchHeatCoilNomEff, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).MSEfficiency( HeatingCoil( CoilNum ).NumOfStages ) );
		} else if ( SELECT_CASE_var == Coil_HeatingGas ) {
			PreDefTableEntry( pdchHeatCoilType, HeatingCoil( CoilNum ).Name, "Coil:Heating:Gas" );
			PreDefTableEntry( pdchHeatCoilNomCap, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).NominalCapacity );
			PreDefTableEntry( pdchHeatCoilNomEff, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).Efficiency );
		} else if ( SELECT_CASE_var == Coil_HeatingGas_MultiStage ) {
			PreDefTableEntry( pdchHeatCoilType, HeatingCoil( CoilNum ).Name, "Coil:Heating:Gas:MultiStage" );
			PreDefTableEntry( pdchHeatCoilNomCap, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).MSNominalCapacity( HeatingCoil( CoilNum ).NumOfStages ) );
			PreDefTableEntry( pdchHeatCoilNomEff, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).MSEfficiency( HeatingCoil( CoilNum ).NumOfStages ) );
		} else if ( SELECT_CASE_var == Coil_HeatingDesuperheater ) {
			PreDefTableEntry( pdchHeatCoilType, HeatingCoil( CoilNum ).Name, "Coil:Heating:Desuperheater" );
			PreDefTableEntry( pdchHeatCoilNomCap, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).NominalCapacity );
			PreDefTableEntry( pdchHeatCoilNomEff, HeatingCoil( CoilNum ).Name, HeatingCoil( CoilNum ).Efficiency );
		}}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcElectricHeatingCoil(
		int const CoilNum, // index to heating coil
		Real64 & QCoilReq,
		Real64 & QCoilActual, // coil load actually delivered (W)
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rich Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a simple Electric heating coil with an efficiency

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataHVACGlobals::TempControlTol;
		using DataHVACGlobals::ElecHeatingCoilPower;
		using DataAirLoop::LoopHeatingCoilMaxRTF;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // [kg/sec]
		Real64 TempAirIn; // [C]
		Real64 TempAirOut; // [C]
		Real64 Win;
		Real64 Effic;
		Real64 CapacitanceAir;
		Real64 HeatingCoilLoad;
		Real64 QCoilCap;
		Real64 TempSetPoint;
		int Control;

		Effic = HeatingCoil( CoilNum ).Efficiency;
		TempAirIn = HeatingCoil( CoilNum ).InletAirTemp;
		Win = HeatingCoil( CoilNum ).InletAirHumRat;
		Control = HeatingCoil( CoilNum ).Control;
		TempSetPoint = HeatingCoil( CoilNum ).DesiredOutletTemp;

		//  adjust mass flow rates for cycling fan cycling coil operation
		if ( FanOpMode == CycFanCycCoil ) {
			if ( PartLoadRatio > 0.0 ) {
				AirMassFlow = HeatingCoil( CoilNum ).InletAirMassFlowRate / PartLoadRatio;
				QCoilReq /= PartLoadRatio;
			} else {
				AirMassFlow = 0.0;
			}
		} else {
			AirMassFlow = HeatingCoil( CoilNum ).InletAirMassFlowRate;
		}

		CapacitanceAir = PsyCpAirFnWTdb( Win, TempAirIn ) * AirMassFlow;

		// If the coil is operating there should be some heating capacitance
		//  across the coil, so do the simulation. If not set outlet to inlet and no load.
		//  Also the coil has to be scheduled to be available.

		// Control output to meet load QCoilReq (QCoilReq is passed in if load controlled, otherwise QCoilReq=-999)
		if ( ( AirMassFlow > 0.0 && HeatingCoil( CoilNum ).NominalCapacity > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( QCoilReq > 0.0 ) ) {

			//check to see if the Required heating capacity is greater than the user specified capacity.
			if ( QCoilReq > HeatingCoil( CoilNum ).NominalCapacity ) {
				QCoilCap = HeatingCoil( CoilNum ).NominalCapacity;
			} else {
				QCoilCap = QCoilReq;
			}

			TempAirOut = TempAirIn + QCoilCap / CapacitanceAir;
			HeatingCoilLoad = QCoilCap;

			//The HeatingCoilLoad is the change in the enthalpy of the Heating
			HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoilLoad / Effic;

			// Control coil output to meet a setpoint temperature.
		} else if ( ( AirMassFlow > 0.0 && HeatingCoil( CoilNum ).NominalCapacity > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( QCoilReq == SensedLoadFlagValue ) && ( std::abs( TempSetPoint - TempAirIn ) > TempControlTol ) ) {

			QCoilCap = CapacitanceAir * ( TempSetPoint - TempAirIn );
			// check to see if setpoint above enetering temperature. If not, set
			// output to zero.
			if ( QCoilCap <= 0.0 ) {
				QCoilCap = 0.0;
				TempAirOut = TempAirIn;
				//check to see if the Required heating capacity is greater than the user
				// specified capacity.
			} else if ( QCoilCap > HeatingCoil( CoilNum ).NominalCapacity ) {
				QCoilCap = HeatingCoil( CoilNum ).NominalCapacity;
				TempAirOut = TempAirIn + QCoilCap / CapacitanceAir;
			} else {
				TempAirOut = TempSetPoint;
			}

			HeatingCoilLoad = QCoilCap;

			//The HeatingCoilLoad is the change in the enthalpy of the Heating
			HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoilLoad / Effic;

		} else { // If not running Conditions do not change across coil from inlet to outlet

			TempAirOut = TempAirIn;
			HeatingCoilLoad = 0.0;
			HeatingCoil( CoilNum ).ElecUseLoad = 0.0;
		}

		if ( FanOpMode == CycFanCycCoil ) {
			HeatingCoil( CoilNum ).ElecUseLoad *= PartLoadRatio;
			HeatingCoilLoad *= PartLoadRatio;
		}

		HeatingCoil( CoilNum ).HeatingCoilLoad = HeatingCoilLoad;
		ElecHeatingCoilPower = HeatingCoil( CoilNum ).ElecUseLoad;

		// Set the outlet conditions
		HeatingCoil( CoilNum ).OutletAirTemp = TempAirOut;

		// This HeatingCoil does not change the moisture or Mass Flow across the component
		HeatingCoil( CoilNum ).OutletAirHumRat = HeatingCoil( CoilNum ).InletAirHumRat;
		HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;
		//Set the outlet enthalpys for air and Heating
		HeatingCoil( CoilNum ).OutletAirEnthalpy = PsyHFnTdbW( HeatingCoil( CoilNum ).OutletAirTemp, HeatingCoil( CoilNum ).OutletAirHumRat );

		QCoilActual = HeatingCoilLoad;
		if ( std::abs( HeatingCoil( CoilNum ).NominalCapacity ) < 1.e-8 ) {
			LoopHeatingCoilMaxRTF = max( LoopHeatingCoilMaxRTF, 0.0 );
		} else {
			LoopHeatingCoilMaxRTF = max( LoopHeatingCoilMaxRTF, HeatingCoilLoad / HeatingCoil( CoilNum ).NominalCapacity );
		}

	}

	void
	CalcMultiStageElectricHeatingCoil(
		int & CoilNum, // the number of the electric heating coil to be simulated
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
		Real64 const CycRatio, // cycling part load ratio
		int const StageNum, // Stage number
		int const FanOpMode // Fan operation mode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side performance and electrical energy use of multistage electric heating coil.

		// METHODOLOGY EMPLOYED:
		// Uses the same methodology as the single stage electric heating unit model (SUBROUTINE CalcelectricHeatingCoil).
		// In addition it assumes that the unit performance is obtained by interpolating between
		// the performance at high stage and that at low stage. If the output needed is below
		// that produced at low stage, the coil cycles between off and low stage.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataHVACGlobals::MSHPMassFlowRateLow;
		using DataHVACGlobals::MSHPMassFlowRateHigh;
		using DataHVACGlobals::ElecHeatingCoilPower;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyTsatFnHPb;
		using Psychrometrics::PsyWFnTdbH;
		using DataEnvironment::OutBaroPress;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMultiStageElectricHeatingCoil" );
		static std::string const RoutineNameAverageLoad( "CalcMultiStageElectricHeatingCoil:Averageload" );
		static std::string const RoutineNameFullLoad( "CalcMultiStageElectricHeatingCoil:fullload" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		Real64 OutletAirEnthalpy; // outlet air enthalpy [J/kg]
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		Real64 TotCapHS; // total capacity at high stage [W]
		Real64 TotCapLS; // total capacity at low stage [W]
		Real64 TotCap; // total capacity at current stage [W]
		Real64 EffHS; // total capacity at high stage [W]
		Real64 EffLS; // total capacity at low stage [W]
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		int StageNumHS; // High stage number
		int StageNumLS; // Low stage number
		Real64 FullLoadOutAirEnth; // Outlet full load enthalpy
		Real64 FullLoadOutAirHumRat; // Outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // Outlet temperature at full load
		Real64 FullLoadOutAirRH; // Outler relative humidity at full load
		Real64 OutletAirTemp; // Supply ari temperature
		Real64 LSFullLoadOutAirEnth; // Outlet full load enthalpy at low stage
		Real64 HSFullLoadOutAirEnth; // Outlet full load enthalpy at high stage
		Real64 LSElecHeatingPower; // Full load power at low stage
		Real64 HSElecHeatingPower; // Full load power at high stage
		Real64 PartLoadRat; // part load ratio

		// FLOW
		if ( StageNum > 1 ) {
			StageNumLS = StageNum - 1;
			StageNumHS = StageNum;
			if ( StageNum > HeatingCoil( CoilNum ).NumOfStages ) {
				StageNumLS = HeatingCoil( CoilNum ).NumOfStages - 1;
				StageNumHS = HeatingCoil( CoilNum ).NumOfStages;
			}
		} else {
			StageNumLS = 1;
			StageNumHS = 1;
		}

		AirMassFlow = HeatingCoil( CoilNum ).InletAirMassFlowRate;
		InletAirDryBulbTemp = HeatingCoil( CoilNum ).InletAirTemp;
		InletAirEnthalpy = HeatingCoil( CoilNum ).InletAirEnthalpy;
		InletAirHumRat = HeatingCoil( CoilNum ).InletAirHumRat;

		OutdoorPressure = OutBaroPress;

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( ( CycRatio > 0.0 ) || ( SpeedRatio > 0.0 ) ) ) {

			if ( StageNum > 1 ) {

				TotCapLS = HeatingCoil( CoilNum ).MSNominalCapacity( StageNumLS );
				TotCapHS = HeatingCoil( CoilNum ).MSNominalCapacity( StageNumHS );

				EffLS = HeatingCoil( CoilNum ).MSEfficiency( StageNumLS );
				EffHS = HeatingCoil( CoilNum ).MSEfficiency( StageNumHS );

				// Get full load output and power
				LSFullLoadOutAirEnth = InletAirEnthalpy + TotCapLS / MSHPMassFlowRateLow;
				HSFullLoadOutAirEnth = InletAirEnthalpy + TotCapHS / MSHPMassFlowRateHigh;
				LSElecHeatingPower = TotCapLS / EffLS;
				HSElecHeatingPower = TotCapHS / EffHS;
				OutletAirHumRat = InletAirHumRat;

				// if cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
				// IF (FanOpMode .EQ. CycFanCycCoil) OnOffFanPartLoadFraction = 1.0d0

				// Power calculation
				HeatingCoil( CoilNum ).ElecUseLoad = SpeedRatio * HSElecHeatingPower + ( 1.0 - SpeedRatio ) * LSElecHeatingPower;

				ElecHeatingCoilPower = HeatingCoil( CoilNum ).ElecUseLoad;
				HeatingCoil( CoilNum ).HeatingCoilLoad = MSHPMassFlowRateHigh * ( HSFullLoadOutAirEnth - InletAirEnthalpy ) * SpeedRatio + MSHPMassFlowRateLow * ( LSFullLoadOutAirEnth - InletAirEnthalpy ) * ( 1.0 - SpeedRatio );

				OutletAirEnthalpy = InletAirEnthalpy + HeatingCoil( CoilNum ).HeatingCoilLoad / HeatingCoil( CoilNum ).InletAirMassFlowRate;
				OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
				FullLoadOutAirRH = PsyRhFnTdbWPb( OutletAirTemp, OutletAirHumRat, OutdoorPressure, RoutineNameAverageLoad );

				if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
					OutletAirTemp = PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure, RoutineName );
					OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, OutletAirEnthalpy, RoutineName );
				}

				HeatingCoil( CoilNum ).OutletAirTemp = OutletAirTemp;
				HeatingCoil( CoilNum ).OutletAirHumRat = OutletAirHumRat;
				HeatingCoil( CoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;

				// Stage 1
			} else if ( CycRatio > 0.0 ) {

				PartLoadRat = min( 1.0, CycRatio );

				// for cycling fan, reset mass flow to full on rate
				if ( FanOpMode == CycFanCycCoil ) AirMassFlow /= PartLoadRat;
				if ( FanOpMode == ContFanCycCoil ) AirMassFlow = MSHPMassFlowRateLow;

				TotCap = HeatingCoil( CoilNum ).MSNominalCapacity( StageNumLS );

				// Calculate full load outlet conditions
				FullLoadOutAirEnth = InletAirEnthalpy + TotCap / AirMassFlow;
				FullLoadOutAirHumRat = InletAirHumRat;
				FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
				FullLoadOutAirRH = PsyRhFnTdbWPb( FullLoadOutAirTemp, FullLoadOutAirHumRat, OutdoorPressure, RoutineNameFullLoad );

				if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
					FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure, RoutineName );
					//  Eventually inlet air conditions will be used in electric Coil, these lines are commented out and marked with this comment line
					//  FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
					FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName );
				}

				// Set outlet conditions from the full load calculation
				if ( FanOpMode == CycFanCycCoil ) {
					OutletAirEnthalpy = FullLoadOutAirEnth;
					OutletAirHumRat = FullLoadOutAirHumRat;
					OutletAirTemp = FullLoadOutAirTemp;
				} else {
					OutletAirEnthalpy = PartLoadRat * FullLoadOutAirEnth + ( 1.0 - PartLoadRat ) * InletAirEnthalpy;
					OutletAirHumRat = PartLoadRat * FullLoadOutAirHumRat + ( 1.0 - PartLoadRat ) * InletAirHumRat;
					OutletAirTemp = PartLoadRat * FullLoadOutAirTemp + ( 1.0 - PartLoadRat ) * InletAirDryBulbTemp;
				}

				EffLS = HeatingCoil( CoilNum ).MSEfficiency( StageNumLS );

				//    HeatingCoil(CoilNum)%HeatingCoilLoad = TotCap
				//   This would require a CR to change
				HeatingCoil( CoilNum ).HeatingCoilLoad = TotCap * PartLoadRat;

				HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoil( CoilNum ).HeatingCoilLoad / EffLS;

				ElecHeatingCoilPower = HeatingCoil( CoilNum ).ElecUseLoad;

				HeatingCoil( CoilNum ).OutletAirTemp = OutletAirTemp;
				HeatingCoil( CoilNum ).OutletAirHumRat = OutletAirHumRat;
				HeatingCoil( CoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;
				// this would require a CR to correct (i.e., calculate outputs when coil is off)
				//  ELSE
				//    ! electric coil is off; just pass through conditions
				//    HeatingCoil(CoilNum)%OutletAirEnthalpy = HeatingCoil(CoilNum)%InletAirEnthalpy
				//    HeatingCoil(CoilNum)%OutletAirHumRat   = HeatingCoil(CoilNum)%InletAirHumRat
				//    HeatingCoil(CoilNum)%OutletAirTemp     = HeatingCoil(CoilNum)%InletAirTemp
				//    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
				//    HeatingCoil(CoilNum)%ElecUseLoad      = 0.0
				//    HeatingCoil(CoilNum)%HeatingCoilLoad  = 0.0
				//    ElecHeatingCoilPower                  = 0.0
			}

		} else {

			// electric coil is off; just pass through conditions
			HeatingCoil( CoilNum ).OutletAirEnthalpy = HeatingCoil( CoilNum ).InletAirEnthalpy;
			HeatingCoil( CoilNum ).OutletAirHumRat = HeatingCoil( CoilNum ).InletAirHumRat;
			HeatingCoil( CoilNum ).OutletAirTemp = HeatingCoil( CoilNum ).InletAirTemp;
			HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;

			// some of these are reset in Init, can be removed to speed up code
			HeatingCoil( CoilNum ).ElecUseLoad = 0.0;
			HeatingCoil( CoilNum ).HeatingCoilLoad = 0.0;
			ElecHeatingCoilPower = 0.0;

		} // end of on/off if - else

	}

	void
	CalcGasHeatingCoil(
		int const CoilNum, // index to heating coil
		Real64 const QCoilReq,
		Real64 & QCoilActual, // coil load actually delivered (W)
		int const FanOpMode, // fan operating mode
		Real64 const EP_UNUSED( PartLoadRatio ) // part-load ratio of heating coil
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rich Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a simple Gas heating coil with a burner efficiency

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataHVACGlobals::TempControlTol;
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using DataAirLoop::LoopHeatingCoilMaxRTF;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // [kg/sec]
		Real64 TempAirIn; // [C]
		Real64 TempAirOut; // [C]
		Real64 Win;
		Real64 Effic;
		Real64 CapacitanceAir;
		Real64 HeatingCoilLoad;
		Real64 QCoilCap;
		Real64 TempSetPoint;
		int Control;
		Real64 PartLoadRat;
		Real64 PLF;

		Effic = HeatingCoil( CoilNum ).Efficiency;
		TempAirIn = HeatingCoil( CoilNum ).InletAirTemp;
		Win = HeatingCoil( CoilNum ).InletAirHumRat;
		Control = HeatingCoil( CoilNum ).Control;
		TempSetPoint = HeatingCoil( CoilNum ).DesiredOutletTemp;
		AirMassFlow = HeatingCoil( CoilNum ).InletAirMassFlowRate;

		CapacitanceAir = PsyCpAirFnWTdb( Win, TempAirIn ) * AirMassFlow;

		// If the coil is operating there should be some heating capacitance
		//  across the coil, so do the simulation. If not set outlet to inlet and no load.
		//  Also the coil has to be scheduled to be available.

		// Control output to meet load QCoilReq (QCoilReq is passed in if load controlled, otherwise QCoilReq=-999)
		if ( ( AirMassFlow > 0.0 && HeatingCoil( CoilNum ).NominalCapacity > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( QCoilReq > 0.0 ) ) {

			//check to see if the Required heating capacity is greater than the user specified capacity.
			if ( QCoilReq > HeatingCoil( CoilNum ).NominalCapacity ) {
				QCoilCap = HeatingCoil( CoilNum ).NominalCapacity;
			} else {
				QCoilCap = QCoilReq;
			}

			TempAirOut = TempAirIn + QCoilCap / CapacitanceAir;
			HeatingCoilLoad = QCoilCap;

			PartLoadRat = HeatingCoilLoad / HeatingCoil( CoilNum ).NominalCapacity;

			//The HeatingCoilLoad is the change in the enthalpy of the Heating
			HeatingCoil( CoilNum ).GasUseLoad = HeatingCoilLoad / Effic;
			HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoil( CoilNum ).ParasiticElecLoad * PartLoadRat;
			HeatingCoil( CoilNum ).ParasiticGasRate = HeatingCoil( CoilNum ).ParasiticGasCapacity * ( 1.0 - PartLoadRat );

			// Control coil output to meet a setpoint temperature.
		} else if ( ( AirMassFlow > 0.0 && HeatingCoil( CoilNum ).NominalCapacity > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( QCoilReq == SensedLoadFlagValue ) && ( std::abs( TempSetPoint - TempAirIn ) > TempControlTol ) ) {

			QCoilCap = CapacitanceAir * ( TempSetPoint - TempAirIn );
			// check to see if setpoint above entering temperature. If not, set
			// output to zero.
			if ( QCoilCap <= 0.0 ) {
				QCoilCap = 0.0;
				TempAirOut = TempAirIn;
				//check to see if the Required heating capacity is greater than the user
				// specified capacity.
			} else if ( QCoilCap > HeatingCoil( CoilNum ).NominalCapacity ) {
				QCoilCap = HeatingCoil( CoilNum ).NominalCapacity;
				TempAirOut = TempAirIn + QCoilCap / CapacitanceAir;
			} else {
				TempAirOut = TempSetPoint;
			}

			HeatingCoilLoad = QCoilCap;

			PartLoadRat = HeatingCoilLoad / HeatingCoil( CoilNum ).NominalCapacity;

			//The HeatingCoilLoad is the change in the enthalpy of the Heating
			HeatingCoil( CoilNum ).GasUseLoad = HeatingCoilLoad / Effic;
			HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoil( CoilNum ).ParasiticElecLoad * PartLoadRat;
			HeatingCoil( CoilNum ).ParasiticGasRate = HeatingCoil( CoilNum ).ParasiticGasCapacity * ( 1.0 - PartLoadRat );

		} else { // If not running Conditions do not change across coil from inlet to outlet

			TempAirOut = TempAirIn;
			HeatingCoilLoad = 0.0;
			PartLoadRat = 0.0;
			HeatingCoil( CoilNum ).GasUseLoad = 0.0;
			HeatingCoil( CoilNum ).ElecUseLoad = 0.0;
			HeatingCoil( CoilNum ).ParasiticGasRate = HeatingCoil( CoilNum ).ParasiticGasCapacity;
		}

		HeatingCoil( CoilNum ).RTF = PartLoadRat;

		// If the PLF curve is defined the gas usage needs to be modified
		if ( HeatingCoil( CoilNum ).PLFCurveIndex > 0 ) {
			if ( PartLoadRat == 0 ) {
				HeatingCoil( CoilNum ).GasUseLoad = 0.0;
			} else {
				PLF = CurveValue( HeatingCoil( CoilNum ).PLFCurveIndex, PartLoadRat );
				if ( PLF < 0.7 ) {
					if ( HeatingCoil( CoilNum ).PLFErrorCount < 1 ) {
						++HeatingCoil( CoilNum ).PLFErrorCount;
						ShowWarningError( "CalcGasHeatingCoil: " + cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + "=\"" + HeatingCoil( CoilNum ).Name + "\", PLF curve values" );
						ShowContinueError( "The PLF curve value = " + TrimSigDigits( PLF, 5 ) + " for part-load ratio = " + TrimSigDigits( PartLoadRat, 5 ) );
						ShowContinueError( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and the simulation continues..." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas]." );
					} else {
						ShowRecurringWarningErrorAtEnd( HeatingCoil( CoilNum ).Name + ", Heating coil PLF curve < 0.7 warning continues... ", HeatingCoil( CoilNum ).PLFErrorIndex, PLF, PLF );
					}
					PLF = 0.7;
				}
				// Modify the Gas Coil Consumption and parasitic loads based on PLF curve
				HeatingCoil( CoilNum ).RTF = PartLoadRat / PLF;
				if ( HeatingCoil( CoilNum ).RTF > 1.0 && std::abs( HeatingCoil( CoilNum ).RTF - 1.0 ) > 0.001 ) {
					if ( HeatingCoil( CoilNum ).RTFErrorCount < 1 ) {
						++HeatingCoil( CoilNum ).RTFErrorCount;
						ShowWarningError( "CalcGasHeatingCoil: " + cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + "=\"" + HeatingCoil( CoilNum ).Name + "\", runtime fraction" );
						ShowContinueError( "The runtime fraction exceeded 1.0. [" + TrimSigDigits( HeatingCoil( CoilNum ).RTF, 4 ) + "]." );
						ShowContinueError( "Runtime fraction is set to 1.0 and the simulation continues..." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas]." );
					} else {
						ShowRecurringWarningErrorAtEnd( HeatingCoil( CoilNum ).Name + ", Heating coil runtime fraction > 1.0 warning continues... ", HeatingCoil( CoilNum ).RTFErrorIndex, HeatingCoil( CoilNum ).RTF, HeatingCoil( CoilNum ).RTF );
					}
					HeatingCoil( CoilNum ).RTF = 1.0; // Reset coil runtime fraction to 1.0
				} else if ( HeatingCoil( CoilNum ).RTF > 1.0 ) {
					HeatingCoil( CoilNum ).RTF = 1.0; // Reset coil runtime fraction to 1.0
				}
				HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoil( CoilNum ).ParasiticElecLoad * HeatingCoil( CoilNum ).RTF;
				HeatingCoil( CoilNum ).GasUseLoad = HeatingCoil( CoilNum ).NominalCapacity / Effic * HeatingCoil( CoilNum ).RTF;
				HeatingCoil( CoilNum ).ParasiticGasRate = HeatingCoil( CoilNum ).ParasiticGasCapacity * ( 1.0 - HeatingCoil( CoilNum ).RTF );
				// Fan power will also be modified by the heating coil's part load fraction
				// OnOffFanPartLoadFraction passed to fan via DataHVACGlobals (cycling fan only)
				if ( FanOpMode == CycFanCycCoil ) {
					OnOffFanPartLoadFraction = PLF;
				}
			}
		}

		// Set the outlet conditions
		HeatingCoil( CoilNum ).HeatingCoilLoad = HeatingCoilLoad;
		HeatingCoil( CoilNum ).OutletAirTemp = TempAirOut;

		// This HeatingCoil does not change the moisture or Mass Flow across the component
		HeatingCoil( CoilNum ).OutletAirHumRat = HeatingCoil( CoilNum ).InletAirHumRat;
		HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;
		//Set the outlet enthalpys for air and Heating
		HeatingCoil( CoilNum ).OutletAirEnthalpy = PsyHFnTdbW( HeatingCoil( CoilNum ).OutletAirTemp, HeatingCoil( CoilNum ).OutletAirHumRat );

		QCoilActual = HeatingCoilLoad;
		LoopHeatingCoilMaxRTF = max( LoopHeatingCoilMaxRTF, HeatingCoil( CoilNum ).RTF );
		ElecHeatingCoilPower = HeatingCoil( CoilNum ).ElecUseLoad;

	}

	void
	CalcMultiStageGasHeatingCoil(
		int & CoilNum, // the number of the Gas heating coil to be simulated
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
		Real64 const CycRatio, // cycling part load ratio
		int const StageNum, // Speed number
		int const FanOpMode // Fan operation mode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side performance and energy use of a multi stage gas heating coil.

		// METHODOLOGY EMPLOYED:
		// Uses the same methodology as the single speed Gas heating unit model (SUBROUTINE CalcGasHeatingCoil).
		// In addition it assumes that the unit performance is obtained by interpolating between
		// the performance at high stage and that at low stage. If the output needed is below
		// that produced at low stage, the coil cycles between off and low stage.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataHVACGlobals::MSHPMassFlowRateLow;
		using DataHVACGlobals::MSHPMassFlowRateHigh;
		using DataHVACGlobals::ElecHeatingCoilPower;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyTsatFnHPb;
		using Psychrometrics::PsyWFnTdbH;
		using DataEnvironment::OutBaroPress;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMultiStageGasHeatingCoil" );
		static std::string const RoutineNameAverageLoad( "CalcMultiStageGasHeatingCoil:Averageload" );
		static std::string const RoutineNameFullLoad( "CalcMultiStageGasHeatingCoil:fullload" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		Real64 OutletAirEnthalpy; // outlet air enthalpy [J/kg]
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		Real64 TotCapHS; // total capacity at high stage [W]
		Real64 TotCapLS; // total capacity at low stage [W]
		Real64 TotCap; // total capacity at current stage [W]
		Real64 EffHS; // efficiency at high stage
		Real64 EffLS( 0.0 ); // efficiency at low stage
		Real64 EffAvg; // average efficiency
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		int StageNumHS; // High stage number
		int StageNumLS; // Low stage number
		Real64 FullLoadOutAirEnth; // Outlet full load enthalpy
		Real64 FullLoadOutAirHumRat; // Outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // Outlet temperature at full load
		Real64 FullLoadOutAirRH; // Outler relative humidity at full load
		Real64 OutletAirTemp; // Supply ari temperature
		Real64 LSFullLoadOutAirEnth; // Outlet full load enthalpy at low stage
		Real64 HSFullLoadOutAirEnth; // Outlet full load enthalpy at high stage
		Real64 LSGasHeatingPower; // Full load power at low stage
		Real64 HSGasHeatingPower; // Full load power at high stage
		Real64 PartLoadRat( 0.0 ); // part load ratio
		Real64 PLF; // part load factor used to calculate RTF

		// FLOW
		if ( StageNum > 1 ) {
			StageNumLS = StageNum - 1;
			StageNumHS = StageNum;
			if ( StageNum > HeatingCoil( CoilNum ).NumOfStages ) {
				StageNumLS = HeatingCoil( CoilNum ).NumOfStages - 1;
				StageNumHS = HeatingCoil( CoilNum ).NumOfStages;
			}
		} else {
			StageNumLS = 1;
			StageNumHS = 1;
		}

		AirMassFlow = HeatingCoil( CoilNum ).InletAirMassFlowRate;
		InletAirDryBulbTemp = HeatingCoil( CoilNum ).InletAirTemp;
		InletAirEnthalpy = HeatingCoil( CoilNum ).InletAirEnthalpy;
		InletAirHumRat = HeatingCoil( CoilNum ).InletAirHumRat;

		OutdoorPressure = OutBaroPress;

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( ( CycRatio > 0.0 ) || ( SpeedRatio > 0.0 ) ) ) {

			if ( StageNum > 1 ) {

				TotCapLS = HeatingCoil( CoilNum ).MSNominalCapacity( StageNumLS );
				TotCapHS = HeatingCoil( CoilNum ).MSNominalCapacity( StageNumHS );

				EffLS = HeatingCoil( CoilNum ).MSEfficiency( StageNumLS );
				EffHS = HeatingCoil( CoilNum ).MSEfficiency( StageNumHS );

				PartLoadRat = min( 1.0, SpeedRatio );
				// Get full load output and power
				LSFullLoadOutAirEnth = InletAirEnthalpy + TotCapLS / MSHPMassFlowRateLow;
				HSFullLoadOutAirEnth = InletAirEnthalpy + TotCapHS / MSHPMassFlowRateHigh;
				LSGasHeatingPower = TotCapLS / EffLS;
				HSGasHeatingPower = TotCapHS / EffHS;
				OutletAirHumRat = InletAirHumRat;

				// if cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
				// IF (FanOpMode .EQ. CycFanCycCoil) OnOffFanPartLoadFraction = 1.0d0

				// Power calculation. If PartLoadRat (SpeedRatio) = 0, operate at LS the whole time step
				HeatingCoil( CoilNum ).ElecUseLoad = PartLoadRat * HeatingCoil( CoilNum ).MSParasiticElecLoad( StageNumHS ) + ( 1.0 - PartLoadRat ) * HeatingCoil( CoilNum ).MSParasiticElecLoad( StageNumLS );

				ElecHeatingCoilPower = HeatingCoil( CoilNum ).ElecUseLoad;
				HeatingCoil( CoilNum ).HeatingCoilLoad = MSHPMassFlowRateHigh * ( HSFullLoadOutAirEnth - InletAirEnthalpy ) * PartLoadRat + MSHPMassFlowRateLow * ( LSFullLoadOutAirEnth - InletAirEnthalpy ) * ( 1.0 - PartLoadRat );
				EffAvg = ( EffHS * PartLoadRat ) + ( EffLS * ( 1.0 - PartLoadRat ) );
				HeatingCoil( CoilNum ).GasUseLoad = HeatingCoil( CoilNum ).HeatingCoilLoad / EffAvg;
				HeatingCoil( CoilNum ).ParasiticGasRate = 0.0;

				OutletAirEnthalpy = InletAirEnthalpy + HeatingCoil( CoilNum ).HeatingCoilLoad / HeatingCoil( CoilNum ).InletAirMassFlowRate;
				OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
				FullLoadOutAirRH = PsyRhFnTdbWPb( OutletAirTemp, OutletAirHumRat, OutdoorPressure, RoutineNameAverageLoad );

				if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
					OutletAirTemp = PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure, RoutineName );
					OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, OutletAirEnthalpy, RoutineName );
				}

				HeatingCoil( CoilNum ).OutletAirTemp = OutletAirTemp;
				HeatingCoil( CoilNum ).OutletAirHumRat = OutletAirHumRat;
				HeatingCoil( CoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;

				// Stage 1
			} else if ( CycRatio > 0.0 ) {

				// for cycling fan, reset mass flow to full on rate
				if ( FanOpMode == CycFanCycCoil ) AirMassFlow /= CycRatio;
				if ( FanOpMode == ContFanCycCoil ) AirMassFlow = MSHPMassFlowRateLow;

				TotCap = HeatingCoil( CoilNum ).MSNominalCapacity( StageNumLS );

				PartLoadRat = min( 1.0, CycRatio );

				// Calculate full load outlet conditions
				FullLoadOutAirEnth = InletAirEnthalpy + TotCap / AirMassFlow;
				FullLoadOutAirHumRat = InletAirHumRat;
				FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
				FullLoadOutAirRH = PsyRhFnTdbWPb( FullLoadOutAirTemp, FullLoadOutAirHumRat, OutdoorPressure, RoutineNameFullLoad );

				if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
					FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure, RoutineName );
					//  Eventually inlet air conditions will be used in Gas Coil, these lines are commented out and marked with this comment line
					//  FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
					FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName );
				}

				// Set outlet conditions from the full load calculation
				if ( FanOpMode == CycFanCycCoil ) {
					OutletAirEnthalpy = FullLoadOutAirEnth;
					OutletAirHumRat = FullLoadOutAirHumRat;
					OutletAirTemp = FullLoadOutAirTemp;
				} else {
					OutletAirEnthalpy = PartLoadRat * FullLoadOutAirEnth + ( 1.0 - PartLoadRat ) * InletAirEnthalpy;
					OutletAirHumRat = PartLoadRat * FullLoadOutAirHumRat + ( 1.0 - PartLoadRat ) * InletAirHumRat;
					OutletAirTemp = PartLoadRat * FullLoadOutAirTemp + ( 1.0 - PartLoadRat ) * InletAirDryBulbTemp;
				}

				EffLS = HeatingCoil( CoilNum ).MSEfficiency( StageNumLS );

				//    HeatingCoil(CoilNum)%HeatingCoilLoad = TotCap
				//   This would require a CR to change
				HeatingCoil( CoilNum ).HeatingCoilLoad = TotCap * PartLoadRat;

				HeatingCoil( CoilNum ).GasUseLoad = HeatingCoil( CoilNum ).HeatingCoilLoad / EffLS;
				//   parasitics are calculated when the coil is off (1-PLR)
				HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoil( CoilNum ).MSParasiticElecLoad( StageNumLS ) * ( 1.0 - PartLoadRat );
				HeatingCoil( CoilNum ).ParasiticGasRate = HeatingCoil( CoilNum ).ParasiticGasCapacity * ( 1.0 - PartLoadRat );
				ElecHeatingCoilPower = HeatingCoil( CoilNum ).ElecUseLoad;

				HeatingCoil( CoilNum ).OutletAirTemp = OutletAirTemp;
				HeatingCoil( CoilNum ).OutletAirHumRat = OutletAirHumRat;
				HeatingCoil( CoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;
				// This seems unecessary (i.e., cycratio or speedratio is > 0) , and would require a CR to change
				//  ELSE
				//    ! Gas coil is off; just pass through conditions
				//    HeatingCoil(CoilNum)%OutletAirEnthalpy = HeatingCoil(CoilNum)%InletAirEnthalpy
				//    HeatingCoil(CoilNum)%OutletAirHumRat   = HeatingCoil(CoilNum)%InletAirHumRat
				//    HeatingCoil(CoilNum)%OutletAirTemp     = HeatingCoil(CoilNum)%InletAirTemp
				//    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
				//    HeatingCoil(CoilNum)%ElecUseLoad      = 0.0
				//    HeatingCoil(CoilNum)%HeatingCoilLoad  = 0.0
				//    ElecHeatingCoilPower                  = 0.0
			}

			// This requires a CR to correct (i.e., calculate outputs when coil is off)
		} else {

			// Gas coil is off; just pass through conditions
			HeatingCoil( CoilNum ).OutletAirEnthalpy = HeatingCoil( CoilNum ).InletAirEnthalpy;
			HeatingCoil( CoilNum ).OutletAirHumRat = HeatingCoil( CoilNum ).InletAirHumRat;
			HeatingCoil( CoilNum ).OutletAirTemp = HeatingCoil( CoilNum ).InletAirTemp;
			HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;

			// some of these are reset in Init, can be removed to speed up code
			HeatingCoil( CoilNum ).ElecUseLoad = 0.0;
			HeatingCoil( CoilNum ).HeatingCoilLoad = 0.0;
			HeatingCoil( CoilNum ).GasUseLoad = 0.0;
			HeatingCoil( CoilNum ).ParasiticGasRate = HeatingCoil( CoilNum ).ParasiticGasCapacity;
			ElecHeatingCoilPower = 0.0;
			PartLoadRat = 0.0;

		} // end of on/off if - else

		// If the PLF curve is defined the gas usage needs to be modified.
		// The PLF curve is only used when the coil cycles.
		if ( HeatingCoil( CoilNum ).PLFCurveIndex > 0 ) {
			if ( PartLoadRat > 0.0 && StageNum < 2 ) {
				PLF = CurveValue( HeatingCoil( CoilNum ).PLFCurveIndex, PartLoadRat );
				if ( PLF < 0.7 ) {
					if ( HeatingCoil( CoilNum ).PLFErrorCount < 1 ) {
						++HeatingCoil( CoilNum ).PLFErrorCount;
						ShowWarningError( "CalcGasHeatingCoil: " + cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + "=\"" + HeatingCoil( CoilNum ).Name + "\", PLF curve values" );
						ShowContinueError( "The PLF curve value = " + TrimSigDigits( PLF, 5 ) + " for part-load ratio = " + TrimSigDigits( PartLoadRat, 5 ) );
						ShowContinueError( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and the simulation continues..." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas]." );
					} else {
						ShowRecurringWarningErrorAtEnd( HeatingCoil( CoilNum ).Name + ", Heating coil PLF curve < 0.7 warning continues... ", HeatingCoil( CoilNum ).PLFErrorIndex, PLF, PLF );
					}
					PLF = 0.7;
				}
				// Modify the Gas Coil Consumption and parasitic loads based on PLF curve
				HeatingCoil( CoilNum ).RTF = PartLoadRat / PLF;
				if ( HeatingCoil( CoilNum ).RTF > 1.0 && std::abs( HeatingCoil( CoilNum ).RTF - 1.0 ) > 0.001 ) {
					if ( HeatingCoil( CoilNum ).RTFErrorCount < 1 ) {
						++HeatingCoil( CoilNum ).RTFErrorCount;
						ShowWarningError( "CalcGasHeatingCoil: " + cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + "=\"" + HeatingCoil( CoilNum ).Name + "\", runtime fraction" );
						ShowContinueError( "The runtime fraction exceeded 1.0. [" + TrimSigDigits( HeatingCoil( CoilNum ).RTF, 4 ) + "]." );
						ShowContinueError( "Runtime fraction is set to 1.0 and the simulation continues..." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas]." );
					} else {
						ShowRecurringWarningErrorAtEnd( HeatingCoil( CoilNum ).Name + ", Heating coil runtime fraction > 1.0 warning continues... ", HeatingCoil( CoilNum ).RTFErrorIndex, HeatingCoil( CoilNum ).RTF, HeatingCoil( CoilNum ).RTF );
					}
					HeatingCoil( CoilNum ).RTF = 1.0; // Reset coil runtime fraction to 1.0
				} else if ( HeatingCoil( CoilNum ).RTF > 1.0 ) {
					HeatingCoil( CoilNum ).RTF = 1.0; // Reset coil runtime fraction to 1.0
				}
				HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoil( CoilNum ).MSParasiticElecLoad( StageNum ) * HeatingCoil( CoilNum ).RTF;
				HeatingCoil( CoilNum ).GasUseLoad = ( HeatingCoil( CoilNum ).MSNominalCapacity( StageNum ) / EffLS ) * HeatingCoil( CoilNum ).RTF;
				HeatingCoil( CoilNum ).ParasiticGasRate = HeatingCoil( CoilNum ).ParasiticGasCapacity * ( 1.0 - HeatingCoil( CoilNum ).RTF );
				// Fan power will also be modified by the heating coil's part load fraction
				// OnOffFanPartLoadFraction passed to fan via DataHVACGlobals (cycling fan only)
				if ( FanOpMode == CycFanCycCoil ) {
					OnOffFanPartLoadFraction = PLF;
				}
			}
			// This requires a CR to correct (i.e., if PLFCurveIndex = 0 do this)
			//   ELSE
			//     IF(CycRatio > 0.0d0 .AND. StageNum < 2)THEN
			//       HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%MSParasiticElecLoad(StageNum) * CycRatio
			//       HeatingCoil(CoilNum)%GasUseLoad  = HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) / EffLS * CycRatio
			//       HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity * (1.0d0 - CycRatio)
			//     END IF
		}

	}

	void
	CalcDesuperheaterHeatingCoil(
		int const CoilNum, // index to desuperheater heating coil
		Real64 const QCoilReq, // load requested by the simulation for load based control [W]
		Real64 & QCoilActual // coil load actually delivered
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a simple desuperheater heating coil with a heat reclaim efficiency
		// (eff = ratio of condenser waste heat reclaimed to total condenser waste heat rejected)

		// METHODOLOGY EMPLOYED:
		// The available capacity of the desuperheater heating coil is determined by the
		// amount of heat rejected at the heating source condenser multiplied by the
		// desuperheater heat reclaim efficiency. This capacity is either applied towards
		// a requested load (load based control) or applied to the air stream to meet a
		// heating setpoint (temperature based control). This subroutine is similar to
		// the electric or gas heating coil except that the NominalCapacity is variable
		// and based on the runtime fraction and heat rejection of the heat source object.

		// REFERENCES:

		// Using/Aliasing
		using DataHVACGlobals::TempControlTol;
		using namespace DXCoils;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // air mass flow through the desuperheater heating coil [kg/sec]
		Real64 AvailTemp; // Lowest temperature available from desuperheater (~T condensing)[C]
		Real64 TempAirIn; // temperature of the air entering the desuperheater heating coil [C]
		Real64 TempAirOut; // temperature of the air leaving the desuperheater heating coil [C]
		Real64 Win; // humidity ratio of the air entering the desuperheater heating coil [kg/kg]
		Real64 Effic; // ratio of condenser waste heat reclaimed to total condenser waste heat rejected
		Real64 CapacitanceAir; // MdotCp of air entering the desuperheater heating coil
		Real64 HeatingCoilLoad; // actual load delivered by the desuperheater heating coil [W]
		Real64 QCoilCap; // available capacity of the desuperheater heating coil [W]
		Real64 TempSetPoint; // setpoint temperature to be met when using temperature based control [C]
		int SourceID; // waste heat source id number

		Effic = HeatingCoil( CoilNum ).Efficiency;
		AirMassFlow = HeatingCoil( CoilNum ).InletAirMassFlowRate;
		TempAirIn = HeatingCoil( CoilNum ).InletAirTemp;
		Win = HeatingCoil( CoilNum ).InletAirHumRat;
		CapacitanceAir = PsyCpAirFnWTdb( Win, TempAirIn ) * AirMassFlow;
		TempSetPoint = HeatingCoil( CoilNum ).DesiredOutletTemp;

		// Access the appropriate structure to find the available heating capacity of the desuperheater heating coil
		// The nominal capacity of the desuperheater heating coil varies based on the amount of heat rejected by the source
		// Stovall 2011, add comparison to available temperature of heat reclaim source
		if ( ValidSourceType( CoilNum ) ) {
			SourceID = HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum;
			if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE ) {
				//Added last term to available energy equations to avoid double counting reclaimed energy
				// because refrigeration systems are solved outside the hvac time step iterations
				HeatingCoil( CoilNum ).RTF = 1.0;
				HeatingCoil( CoilNum ).NominalCapacity = HeatReclaimRefrigeratedRack( SourceID ).AvailCapacity * Effic - HeatReclaimRefrigeratedRack( SourceID ).UsedWaterHeater;
			} else if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
				AvailTemp = HeatReclaimRefrigCondenser( SourceID ).AvailTemperature;
				HeatingCoil( CoilNum ).RTF = 1.0;
				if ( AvailTemp <= TempAirIn ) {
					HeatingCoil( CoilNum ).NominalCapacity = 0.0;
					ShowRecurringWarningErrorAtEnd( "Coil:Heating:Desuperheater " + HeatingCoil( CoilNum ).Name + " - Waste heat source temperature was too low to be useful.", HeatingCoil( CoilNum ).InsuffTemperatureWarn );
				} else {
					HeatingCoil( CoilNum ).NominalCapacity = HeatReclaimRefrigCondenser( SourceID ).AvailCapacity * Effic - HeatReclaimRefrigCondenser( SourceID ).UsedWaterHeater;
				}
			} else if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_COOLING || HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTISPEED || HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTIMODE ) {
				HeatingCoil( CoilNum ).RTF = DXCoil( SourceID ).CoolingCoilRuntimeFraction;
				HeatingCoil( CoilNum ).NominalCapacity = HeatReclaimDXCoil( SourceID ).AvailCapacity * Effic;
			}
		} else {
			HeatingCoil( CoilNum ).NominalCapacity = 0.0;
		}

		// Control output to meet load (QCoilReq)
		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( QCoilReq > 0.0 ) ) {

			//check to see if the Required heating capacity is greater than the available heating capacity.
			if ( QCoilReq > HeatingCoil( CoilNum ).NominalCapacity ) {
				QCoilCap = HeatingCoil( CoilNum ).NominalCapacity;
			} else {
				QCoilCap = QCoilReq;
			}

			// report the runtime fraction of the desuperheater heating coil
			if ( HeatingCoil( CoilNum ).NominalCapacity > 0.0 ) {
				HeatingCoil( CoilNum ).RTF *= ( QCoilCap / HeatingCoil( CoilNum ).NominalCapacity );
				TempAirOut = TempAirIn + QCoilCap / CapacitanceAir;
				HeatingCoilLoad = QCoilCap;
			} else {
				HeatingCoil( CoilNum ).RTF = 0.0;
				TempAirOut = TempAirIn;
				HeatingCoilLoad = 0.0;
			}

			// Control coil output to meet a setpoint temperature.
		} else if ( ( AirMassFlow > 0.0 && HeatingCoil( CoilNum ).NominalCapacity > 0.0 ) && ( GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( QCoilReq == SensedLoadFlagValue ) && ( std::abs( TempSetPoint - TempAirIn ) > TempControlTol ) ) {

			QCoilCap = CapacitanceAir * ( TempSetPoint - TempAirIn );
			// check to see if setpoint is above entering air temperature. If not, set output to zero.
			if ( QCoilCap <= 0.0 ) {
				QCoilCap = 0.0;
				TempAirOut = TempAirIn;
				//check to see if the required heating capacity is greater than the available capacity.
			} else if ( QCoilCap > HeatingCoil( CoilNum ).NominalCapacity ) {
				QCoilCap = HeatingCoil( CoilNum ).NominalCapacity;
				TempAirOut = TempAirIn + QCoilCap / CapacitanceAir;
			} else {
				TempAirOut = TempSetPoint;
			}

			HeatingCoilLoad = QCoilCap;
			//     report the runtime fraction of the desuperheater heating coil
			HeatingCoil( CoilNum ).RTF *= ( QCoilCap / HeatingCoil( CoilNum ).NominalCapacity );

		} else { // If not running, conditions do not change across heating coil from inlet to outlet

			TempAirOut = TempAirIn;
			HeatingCoilLoad = 0.0;
			HeatingCoil( CoilNum ).ElecUseLoad = 0.0;
			HeatingCoil( CoilNum ).RTF = 0.0;

		}

		// Set the outlet conditions
		HeatingCoil( CoilNum ).HeatingCoilLoad = HeatingCoilLoad;
		HeatingCoil( CoilNum ).OutletAirTemp = TempAirOut;

		// This HeatingCoil does not change the moisture or Mass Flow across the component
		HeatingCoil( CoilNum ).OutletAirHumRat = HeatingCoil( CoilNum ).InletAirHumRat;
		HeatingCoil( CoilNum ).OutletAirMassFlowRate = HeatingCoil( CoilNum ).InletAirMassFlowRate;
		//Set the outlet enthalpy
		HeatingCoil( CoilNum ).OutletAirEnthalpy = PsyHFnTdbW( HeatingCoil( CoilNum ).OutletAirTemp, HeatingCoil( CoilNum ).OutletAirHumRat );

		HeatingCoil( CoilNum ).ElecUseLoad = HeatingCoil( CoilNum ).ParasiticElecLoad * HeatingCoil( CoilNum ).RTF;
		QCoilActual = HeatingCoilLoad;

		// Update remaining waste heat (just in case multiple users of waste heat use same source)
		if ( ValidSourceType( CoilNum ) ) {
			SourceID = HeatingCoil( CoilNum ).ReclaimHeatingSourceIndexNum;
			//   Refrigerated cases are simulated at the zone time step, do not decrement available capacity
			//   (the heat reclaim available capacity will not get reinitialized as the air loop iterates)
			if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE ) {
				HeatReclaimRefrigeratedRack( SourceID ).UsedHVACCoil = HeatingCoilLoad;
			} else if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
				HeatReclaimRefrigCondenser( SourceID ).UsedHVACCoil = HeatingCoilLoad;
			} else if ( HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_COOLING || HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTISPEED || HeatingCoil( CoilNum ).ReclaimHeatingSource == COIL_DX_MULTIMODE ) {
				HeatReclaimDXCoil( SourceID ).AvailCapacity -= HeatingCoilLoad;
			}
		}

	}

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the HeatingCoil Module
	// *****************************************************************************

	void
	UpdateHeatingCoil( int const CoilNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the coil outlet nodes.

		// METHODOLOGY EMPLOYED:
		// Data is moved from the coil data structure to the coil outlet nodes.

		// REFERENCES:
		// na

		// Using/Aliasing
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
		int AirOutletNode;

		AirInletNode = HeatingCoil( CoilNum ).AirInletNodeNum;
		AirOutletNode = HeatingCoil( CoilNum ).AirOutletNodeNum;

		// Set the outlet air nodes of the HeatingCoil
		Node( AirOutletNode ).MassFlowRate = HeatingCoil( CoilNum ).OutletAirMassFlowRate;
		Node( AirOutletNode ).Temp = HeatingCoil( CoilNum ).OutletAirTemp;
		Node( AirOutletNode ).HumRat = HeatingCoil( CoilNum ).OutletAirHumRat;
		Node( AirOutletNode ).Enthalpy = HeatingCoil( CoilNum ).OutletAirEnthalpy;

		// Set the outlet nodes for properties that just pass through & not used
		Node( AirOutletNode ).Quality = Node( AirInletNode ).Quality;
		Node( AirOutletNode ).Press = Node( AirInletNode ).Press;
		Node( AirOutletNode ).MassFlowRateMin = Node( AirInletNode ).MassFlowRateMin;
		Node( AirOutletNode ).MassFlowRateMax = Node( AirInletNode ).MassFlowRateMax;
		Node( AirOutletNode ).MassFlowRateMinAvail = Node( AirInletNode ).MassFlowRateMinAvail;
		Node( AirOutletNode ).MassFlowRateMaxAvail = Node( AirInletNode ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNode ).CO2 = Node( AirInletNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutletNode ).GenContam = Node( AirInletNode ).GenContam;
		}

	}

	//        End of Update subroutines for the HeatingCoil Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the HeatingCoil Module
	// *****************************************************************************

	void
	ReportHeatingCoil( int const CoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variable for the coils.

		// METHODOLOGY EMPLOYED:
		// NA

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
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;
		// report the HeatingCoil energy from this component
		HeatingCoil( CoilNum ).HeatingCoilRate = HeatingCoil( CoilNum ).HeatingCoilLoad;
		HeatingCoil( CoilNum ).HeatingCoilLoad *= ReportingConstant;

		HeatingCoil( CoilNum ).GasUseRate = HeatingCoil( CoilNum ).GasUseLoad;
		HeatingCoil( CoilNum ).ElecUseRate = HeatingCoil( CoilNum ).ElecUseLoad;
		HeatingCoil( CoilNum ).GasUseLoad *= ReportingConstant;
		HeatingCoil( CoilNum ).ElecUseLoad *= ReportingConstant;

		HeatingCoil( CoilNum ).ParasiticGasLoad = HeatingCoil( CoilNum ).ParasiticGasRate * ReportingConstant;

	}

	//        End of Reporting subroutines for the HeatingCoil Module

	void
	GetCoilIndex(
		std::string const & HeatingCoilName,
		int & HeatingCoilIndex,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   March 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given DX Coil -- issues error message if that
		// DX Coil is not a legal DX Coil.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		HeatingCoilIndex = FindItem( HeatingCoilName, HeatingCoil );
		if ( HeatingCoilIndex == 0 ) {
			ShowSevereError( "GetCoilIndex: Heating coil not found=" + HeatingCoilName );
			ErrorsFound = true;
		}

	}

	void
	CheckHeatingCoilSchedule(
		std::string const & CompType, // unused1208
		std::string const & CompName,
		Real64 & Value,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine provides a method for outside routines to check if
		// the heating coil is scheduled to be on.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
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
		int CoilNum;

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		// Find the correct Coil number
		if ( CompIndex == 0 ) {
			CoilNum = FindItem( CompName, HeatingCoil );
			if ( CoilNum == 0 ) {
				ShowFatalError( "CheckHeatingCoilSchedule: Coil not found=\"" + CompName + "\"." );
			}
			if ( ! SameString( CompType, cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) ) ) {
				ShowSevereError( "CheckHeatingCoilSchedule: Coil=\"" + CompName + "\"" );
				ShowContinueError( "...expected type=\"" + CompType + "\", actual type=\"" + cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + "\"." );
				ShowFatalError( "Program terminates due to preceding conditions." );
			}
			CompIndex = CoilNum;
			Value = GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ); // not scheduled?
		} else {
			CoilNum = CompIndex;
			if ( CoilNum > NumHeatingCoils || CoilNum < 1 ) {
				ShowFatalError( "CheckHeatingCoilSchedule: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Number of Heating Coils=" + TrimSigDigits( NumHeatingCoils ) + ", Coil name=" + CompName );
			}
			if ( CompName != HeatingCoil( CoilNum ).Name ) {
				ShowSevereError( "CheckHeatingCoilSchedule: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + HeatingCoil( CoilNum ).Name );
				ShowContinueError( "...expected type=\"" + CompType + "\", actual type=\"" + cAllCoilTypes( HeatingCoil( CoilNum ).HCoilType_Num ) + "\"." );
				ShowFatalError( "Program terminates due to preceding conditions." );
			}
			Value = GetCurrentScheduleValue( HeatingCoil( CoilNum ).SchedPtr ); // not scheduled?
		}

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
		using InputProcessor::FindItem;

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
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				CoilCapacity = HeatingCoil( WhichCoil ).NominalCapacity;
			}
		} else if ( FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas_MultiStage ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				CoilCapacity = HeatingCoil( WhichCoil ).MSNominalCapacity( HeatingCoil( WhichCoil ).NumOfStages );
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) { //Autodesk:Return Reworked block to assure CoilCapacity is set before return
			if ( FoundType == 0 ) {
				ShowSevereError( "GetCoilCapacity: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			} else if ( FoundType > 0 ) {
				ShowSevereError( "GetCoilCapacity: Invalid coil type for capacity, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
				ShowContinueError( "...only " + cAllCoilTypes( Coil_HeatingElectric ) + ", " + cAllCoilTypes( Coil_HeatingGas ) + " or " + cAllCoilTypes( Coil_HeatingDesuperheater ) + " are valid in this context." );
			}
			ShowContinueError( "... returning Coil Capacity as -1000." );
			ErrorsFound = true;
			CoilCapacity = -1000.0;
		}

		return CoilCapacity;

	}

	int
	GetCoilAvailScheduleIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the availability schedule index.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int AvailSchIndex; // returned availability schedule of matched coil

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
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = 0;
		AvailSchIndex = 0;
		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingGas_MultiStage || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				AvailSchIndex = HeatingCoil( WhichCoil ).SchedPtr;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilAvailScheduleIndex: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			AvailSchIndex = 0;
		}

		return AvailSchIndex;
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
		// This function looks up the given coil and returns the inlet node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int NodeNumber; // returned node number of matched coil

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
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = 0;
		NodeNumber = 0;
		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingGas_MultiStage || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = HeatingCoil( WhichCoil ).AirInletNodeNum;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
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
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the outlet node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int NodeNumber; // returned node number of matched coil

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
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = 0;
		NodeNumber = 0;
		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingGas_MultiStage || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = HeatingCoil( WhichCoil ).AirOutletNodeNum;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetHeatReclaimSourceIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the heating coil index number if it is a desuperheating coil.
		// If incorrect coil type or name is given, ErrorsFound is returned as true and index number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;

		// Return value
		int CoilFound; // returned index number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool GetCoilErrFlag;
		bool SuppressWarning;
		int NumCoil;
		int CoilNum( 0 );

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		SuppressWarning = true;
		CoilFound = 0;

		//This function only used for dessicant regeneration and refrigeration desuperheat not a valid source
		//IF (SameString(CoilType,'REFRIGERATION:COMPRESSORRACK')) THEN
		//    CALL GetRefrigeratedRackIndex(CoilName, CoilNum,RefrigSystemTypeRack, GetCoilErrFlag, CoilType, SuppressWarning)
		//    DO NumCoil = 1, NumHeatingCoils
		//      IF(HeatingCoil(NumCoil)%ReclaimHeatingSource .NE. COMPRESSORRACK_REFRIGERATEDCASE .AND. &
		//         HeatingCoil(NumCoil)%ReclaimHeatingCoilName .NE. CoilName)CYCLE
		//      CoilFound = CoilNum
		//      EXIT
		//    END DO
		// ELSEIF (SameString(CoilType,'REFRIGERATION:CONDENSER')) THEN   bbb
		//    CALL GetRefrigeratedRackIndex(CoilName, CoilNum,RefrigSystemTypeDetailed, GetCoilErrFlag, CoilType, SuppressWarning)
		//    DO NumCoil = 1, NumHeatingCoils
		//      IF(HeatingCoil(NumCoil)%ReclaimHeatingSource .NE. CONDENSER_REFRIGERATION .AND. &
		//         HeatingCoil(NumCoil)%ReclaimHeatingCoilName .NE. CoilName)CYCLE
		//      CoilFound = CoilNum
		//      EXIT
		//    END DO
		// ELSEIF
		//note should eventually get rid of this string comparison
		if ( SameString( CoilType, "COIL:COOLING:DX:SINGLESPEED" ) || SameString( CoilType, "COIL:COOLING:DX:TWOSPEED" ) || SameString( CoilType, "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ) ) {
			GetDXCoilIndex( CoilName, CoilNum, GetCoilErrFlag, CoilType, SuppressWarning );
			for ( NumCoil = 1; NumCoil <= NumHeatingCoils; ++NumCoil ) {
				if ( HeatingCoil( NumCoil ).ReclaimHeatingSource != COIL_DX_COOLING && HeatingCoil( NumCoil ).ReclaimHeatingSource != COIL_DX_MULTISPEED && HeatingCoil( NumCoil ).ReclaimHeatingSource != COIL_DX_MULTIMODE && HeatingCoil( NumCoil ).ReclaimHeatingCoilName != CoilName ) continue;
				CoilFound = CoilNum;
				break;
			}
		}

		if ( CoilNum == 0 ) {
			ErrorsFound = true;
		}

		return CoilFound;

	}

	int
	GetCoilControlNodeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the control node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int NodeNumber; // returned node number of matched coil

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
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = 0;
		NodeNumber = 0;
		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingGas_MultiStage || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = HeatingCoil( WhichCoil ).TempSetPointNodeNum;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilControlNodeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetHeatingCoilTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the type number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and type number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int TypeNum; // returned type number of matched coil

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
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = 0;
		TypeNum = 0;
		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingGas_MultiStage || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				TypeNum = HeatingCoil( WhichCoil ).HCoilType_Num;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetHeatingCoilTypeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			TypeNum = 0;
		}

		return TypeNum;

	}

	int
	GetHeatingCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the index into the structure.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int WhichCoil; // returned index number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = 0;
		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingGas_MultiStage || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetHeatingCoilIndex: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
		}

		return WhichCoil;

	}

	int
	GetHeatingCoilPLFCurveIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the PLF curve index.  If
		// incorrect coil name is given for gas or electric heating coils, ErrorsFound
		// is returned as true and curve index is returned as zero.
		// If not a gas or electric heating coil, ErrorsFound is unchanged and index is 0.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int IndexNum; // returned PLF curve index of matched coil

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
		int FoundType; // Integer equivalent of coil type

		// Obtains and Allocates HeatingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		FoundType = FindItem( CoilType, cAllCoilTypes, NumAllCoilTypes );
		if ( FoundType == Coil_HeatingElectric || FoundType == Coil_HeatingElectric_MultiStage || FoundType == Coil_HeatingGas || FoundType == Coil_HeatingGas_MultiStage || FoundType == Coil_HeatingDesuperheater ) {
			WhichCoil = FindItem( CoilName, HeatingCoil );
			if ( WhichCoil != 0 ) {
				IndexNum = HeatingCoil( WhichCoil ).PLFCurveIndex;
			} else {
				ShowSevereError( "GetHeatingCoilPLFCurveIndex: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
				ErrorsFound = true;
				IndexNum = 0;
			}
		} else {
			IndexNum = 0;
		}

		return IndexNum;

	}

	int
	GetHeatingCoilNumberOfStages(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   February 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the number of speeds for multistage coils.
		// If incorrect coil type or name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NumberOfStages; // returned the number of speed of matched coil

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

		// Obtains and Allocates HeatingCoils
		if ( GetCoilsInputFlag ) { //First time subroutine has been entered
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, HeatingCoil );
		if ( WhichCoil != 0 ) {
			NumberOfStages = HeatingCoil( WhichCoil ).NumOfStages;
		} else {
			ShowSevereError( "GetHeatingCoilNumberOfSpeeds: Invalid Heating Coil Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NumberOfStages = 0;
		}

		return NumberOfStages;

	}
 
	// Clears the global data in HeatingCoils.
	// Needed for unit tests, should not be normally called.
	void
		clear_state()
	{

		NumHeatingCoils = 0;
		GetCoilsInputFlag = true;
		CoilIsSuppHeater = false;
		MyOneTimeFlag = true;

		MySizeFlag.deallocate();
		ValidSourceType.deallocate();
		CheckEquipName.deallocate();
		HeatingCoil.deallocate();
		HeatingCoilNumericFields.deallocate();

	}

	void
	SetHeatingCoilData(
		int const CoilNum, // Number of electric or gas heating Coil
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_bool DesiccantRegenerationCoil, // Flag that this coil is used as regeneration air heating coil
		Optional_int DesiccantDehumIndex // Index for the desiccant dehum system where this coil is used 
		) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   February 2016
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function sets data to Heating Coil using the coil index and arguments passed

		// Using/Aliasing
		using General::TrimSigDigits;

		if ( GetCoilsInputFlag ) { 
			GetHeatingCoilInput();
			GetCoilsInputFlag = false;
		}

		if ( CoilNum <= 0 || CoilNum > NumHeatingCoils ) {
			ShowSevereError( "SetHeatingCoilData: called with heating coil Number out of range=" + TrimSigDigits( CoilNum ) + " should be >0 and <" + TrimSigDigits( NumHeatingCoils ) );
			ErrorsFound = true;
			return;
		}

		if ( present( DesiccantRegenerationCoil ) ) {
			HeatingCoil( CoilNum ).DesiccantRegenerationCoil = DesiccantRegenerationCoil;
		}

		if ( present( DesiccantDehumIndex ) ) {
			HeatingCoil( CoilNum ).DesiccantDehumNum = DesiccantDehumIndex;
		}

	}

	//        End of Utility subroutines for the HeatingCoil Module

} // HeatingCoils

} // EnergyPlus
