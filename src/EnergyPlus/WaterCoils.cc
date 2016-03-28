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
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <WaterCoils.hh>
#include <BranchNodeConnections.hh>
#include <DataAirSystems.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <EMSManager.hh>
#include <FaultsManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace WaterCoils {
	// Module containing the WaterCoil simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   April 1998
	//       MODIFIED       April 2004: Rahul Chillar
	//                      Feb. 2010, Brent Griffith, Plant Demand Side Update, general fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the WaterCoil System Component

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;
	using namespace DataHVACGlobals;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyHFnTdbW;
	using Psychrometrics::PsyTdpFnWPb;
	using Psychrometrics::PsyWFnTdbH;
	using Psychrometrics::PsyWFnTdpPb;
	using Psychrometrics::PsyTdbFnHW;
	using Psychrometrics::PsyWFnTdbRhPb;
	using Psychrometrics::PsyWFnTdbTwbPb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyHFnTdbRhPb;
	using Psychrometrics::PsyTsatFnHPb;
	using FluidProperties::GetSpecificHeatGlycol;
	using FluidProperties::GetDensityGlycol;
	using DataPlant::TypeOf_CoilWaterCooling;
	using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
	using DataPlant::TypeOf_CoilWaterSimpleHeating;
	using DataPlant::PlantLoop;
	using DataPlant::MyPlantSizingIndex;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	//PRIVATE ! Everything private unless explicitly made public

	//MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	int const MaxPolynomOrder( 4 );
	int const MaxOrderedPairs( 60 );

	Real64 const PolyConvgTol( 1.E-05 );
	Real64 const MinWaterMassFlowFrac( 0.000001 );
	Real64 const MinAirMassFlow( 0.001 );

	// coil types in this module
	int const WaterCoil_SimpleHeating( TypeOf_CoilWaterSimpleHeating );
	int const WaterCoil_DetFlatFinCooling( TypeOf_CoilWaterDetailedFlatCooling );
	int const WaterCoil_Cooling( TypeOf_CoilWaterCooling );

	int const CoilType_Cooling( 1 );
	int const CoilType_Heating( 2 );

	int const CoilModel_Simple( 1 );
	int const CoilModel_Cooling( 2 );
	int const CoilModel_Detailed( 3 );

	// Parameters for Heat Exchanger Configuration
	int const CounterFlow( 1 );
	int const CrossFlow( 2 );
	int const SimpleAnalysis( 1 );
	int const DetailedAnalysis( 2 );

	//Water Systems
	int const CondensateDiscarded( 1001 ); // default mode where water is "lost"
	int const CondensateToTank( 1002 ); // collect coil condensate from air and store in water storage tank

	//Parameters for COIL:Water:SimpleHeating Coil Performance Input Method
	int const UAandFlow( 1 ); // for Coil Performance Input Method = UA and Design Water Flow Rate
	int const NomCap( 2 ); // for Coil Performance Input Method = Nominal Capacity

	// Parameters Subroutine CoolingCoil: design calc or simulation calc.
	int const DesignCalc( 1 ); // ignore on/off check in CoolingCoil
	int const SimCalc( 2 ); // pay attention to on/off check in CoolingCoil

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumWaterCoils( 0 ); // The Number of WaterCoils found in the Input
	Array1D_bool MySizeFlag;
	Array1D_bool MyUAAndFlowCalcFlag;
	Array1D_bool MyCoilDesignFlag;
	Array1D_bool CoilWarningOnceFlag;
	Array1D_int WaterTempCoolCoilErrs; // error counting for detailed coils
	Array1D_int PartWetCoolCoilErrs; // error counting for detailed coils
	bool GetWaterCoilsInputFlag( true ); // Flag set to make sure you get input once
	Array1D_bool CheckEquipName;
	namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool InitWaterCoilOneTimeFlag( true );
	}
	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Other routines

	// Object Data
	Array1D< WaterCoilEquipConditions > WaterCoil;
	Array1D< WaterCoilNumericFieldData > WaterCoilNumericFields;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions
	void
	clear_state()
	{
		NumWaterCoils = 0;
		InitWaterCoilOneTimeFlag = true;
		MySizeFlag.deallocate();
		MyUAAndFlowCalcFlag.deallocate();
		MyCoilDesignFlag.deallocate();
		CoilWarningOnceFlag.deallocate();
		WaterTempCoolCoilErrs.deallocate();
		PartWetCoolCoilErrs.deallocate();
		GetWaterCoilsInputFlag = true;
		CheckEquipName.deallocate();
		WaterCoil.deallocate();
		WaterCoilNumericFields.deallocate();
	}

	void
	SimulateWaterCoilComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex,
		Optional< Real64 > QActual,
		Optional_int_const FanOpMode,
		Optional< Real64 const > PartLoadRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages WaterCoil component simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

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
		int CoilNum; // The WaterCoil that you are currently loading input into
		int OpMode; // fan operating mode
		Real64 PartLoadFrac; // part-load fraction of heating coil

		// FLOW:

		// Obtains and Allocates WaterCoil related parameters from input file
		if ( GetWaterCoilsInputFlag ) { //First time subroutine has been entered
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		// Find the correct WaterCoilNumber with the Coil Name
		if ( CompIndex == 0 ) {
			CoilNum = FindItemInList( CompName, WaterCoil );
			if ( CoilNum == 0 ) {
				ShowFatalError( "SimulateWaterCoilComponents: Coil not found=" + CompName );
			}
			CompIndex = CoilNum;
		} else {
			CoilNum = CompIndex;
			if ( CoilNum > NumWaterCoils || CoilNum < 1 ) {
				ShowFatalError( "SimulateWaterCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Number of Water Coils=" + TrimSigDigits( NumWaterCoils ) + ", Coil name=" + CompName );
			}
			if ( CheckEquipName( CoilNum ) ) {
				if ( CompName != WaterCoil( CoilNum ).Name ) {
					ShowFatalError( "SimulateWaterCoilComponents: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + WaterCoil( CoilNum ).Name );
				}
				CheckEquipName( CoilNum ) = false;
			}
		}

		// With the correct CoilNum Initialize
		InitWaterCoil( CoilNum, FirstHVACIteration ); // Initialize all WaterCoil related parameters

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

		// Calculate the Correct WaterCoil Model with the current CoilNum
		if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_DetFlatFinCooling ) {
			CalcDetailFlatFinCoolingCoil( CoilNum, SimCalc, OpMode, PartLoadFrac );
			if ( present( QActual ) ) QActual = WaterCoil( CoilNum ).SenWaterCoolingCoilRate;
		} else if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_Cooling ) {
			CoolingCoil( CoilNum, FirstHVACIteration, SimCalc, OpMode, PartLoadFrac );
			if ( present( QActual ) ) QActual = WaterCoil( CoilNum ).SenWaterCoolingCoilRate;
		}

		if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) {
			CalcSimpleHeatingCoil( CoilNum, OpMode, PartLoadFrac, SimCalc );
			if ( present( QActual ) ) QActual = WaterCoil( CoilNum ).TotWaterHeatingCoilRate;
		}

		// Update the current WaterCoil to the outlet nodes
		UpdateWaterCoil( CoilNum );

		// Report the current WaterCoil
		ReportWaterCoil( CoilNum );

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetWaterCoilInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   April 1998
		//       MODIFIED       April 2004: Rahul Chillar
		//                      November 2013: Tianzhen Hong for fouling coils
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for coils and stores it in coil data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::AutoSize;
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using WaterManager::SetupTankSupplyComponent;
		using namespace DataIPShortCuts;
		using GlobalNames::VerifyUniqueCoilName;
		using SetPointManager::NodeHasSPMCtrlVarType;
		using namespace FaultsManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetWaterCoilInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoilNum; // The WaterCoil that you are currently loading input into
		static int NumSimpHeat( 0 );
		static int NumFlatFin( 0 );
		static int NumCooling( 0 );
		int SimpHeatNum;
		int FlatFinNum;
		int CoolingNum;
		int NumAlphas;
		int NumNums;
		int IOStat;
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		static int j1( 0 );

		// Flow
		NumSimpHeat = GetNumObjectsFound( "Coil:Heating:Water" );
		NumFlatFin = GetNumObjectsFound( "Coil:Cooling:Water:DetailedGeometry" );
		NumCooling = GetNumObjectsFound( "Coil:Cooling:Water" );
		NumWaterCoils = NumSimpHeat + NumFlatFin + NumCooling;

		if ( NumWaterCoils > 0 ) {
			WaterCoil.allocate( NumWaterCoils );
			WaterCoilNumericFields.allocate( NumWaterCoils );
			WaterTempCoolCoilErrs.dimension( NumWaterCoils, 0 );
			PartWetCoolCoilErrs.dimension( NumWaterCoils, 0 );
			CheckEquipName.dimension( NumWaterCoils, true );
		}

		GetObjectDefMaxArgs( "Coil:Heating:Water", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Cooling:Water:DetailedGeometry", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Cooling:Water", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		AlphArray.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		NumArray.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		CurrentModuleObject = "Coil:Heating:Water";
		// Get the data for simple heating coils
		for ( SimpHeatNum = 1; SimpHeatNum <= NumSimpHeat; ++SimpHeatNum ) {

			CoilNum = SimpHeatNum;

			GetObjectItem( CurrentModuleObject, SimpHeatNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			WaterCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			WaterCoilNumericFields( CoilNum ).FieldNames = "";
			WaterCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), WaterCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			WaterCoil( CoilNum ).Name = AlphArray( 1 );
			WaterCoil( CoilNum ).Schedule = AlphArray( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				WaterCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				WaterCoil( CoilNum ).SchedPtr = GetScheduleIndex( AlphArray( 2 ) );
				if ( WaterCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + AlphArray( 2 ) + " for " + cAlphaFields( 1 ) + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}

			WaterCoil( CoilNum ).WaterCoilTypeA = "Heating";
			WaterCoil( CoilNum ).WaterCoilType = CoilType_Heating; // 'Heating'
			WaterCoil( CoilNum ).WaterCoilModelA = "SIMPLE";
			WaterCoil( CoilNum ).WaterCoilModel = CoilModel_Simple; // 'SIMPLE'
			WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_SimpleHeating;

			WaterCoil( CoilNum ).UACoil = NumArray( 1 );
			WaterCoil( CoilNum ).UACoilVariable = WaterCoil( CoilNum ).UACoil;
			WaterCoil( CoilNum ).MaxWaterVolFlowRate = NumArray( 2 );
			WaterCoil( CoilNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			WaterCoil( CoilNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			WaterCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			WaterCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			{ auto const SELECT_CASE_var( AlphArray( 7 ) );
			if ( SELECT_CASE_var == "UFACTORTIMESAREAANDDESIGNWATERFLOWRATE" ) {
				WaterCoil( CoilNum ).CoilPerfInpMeth = UAandFlow;

			} else if ( SELECT_CASE_var == "NOMINALCAPACITY" ) {
				WaterCoil( CoilNum ).CoilPerfInpMeth = NomCap;

			} else {
				// will be caught by input processor
				WaterCoil( CoilNum ).CoilPerfInpMeth = UAandFlow;
			}}

			WaterCoil( CoilNum ).DesTotWaterCoilLoad = NumArray( 3 );

			if ( WaterCoil( CoilNum ).UACoil == AutoSize && WaterCoil( CoilNum ).CoilPerfInpMeth == UAandFlow ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			if ( WaterCoil( CoilNum ).MaxWaterVolFlowRate == AutoSize && WaterCoil( CoilNum ).CoilPerfInpMeth == UAandFlow ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			if ( WaterCoil( CoilNum ).DesTotWaterCoilLoad == AutoSize && WaterCoil( CoilNum ).CoilPerfInpMeth == NomCap ) WaterCoil( CoilNum ).RequestingAutoSize = true;

			WaterCoil( CoilNum ).DesInletWaterTemp = NumArray( 4 );
			WaterCoil( CoilNum ).DesInletAirTemp = NumArray( 5 );
			WaterCoil( CoilNum ).DesOutletWaterTemp = NumArray( 6 );
			WaterCoil( CoilNum ).DesOutletAirTemp = NumArray( 7 );
			WaterCoil( CoilNum ).RatioAirSideToWaterSideConvect = NumArray( 8 );

			if ( WaterCoil( CoilNum ).DesInletWaterTemp <= WaterCoil( CoilNum ).DesOutletWaterTemp ) {
				ShowSevereError( "For " + CurrentModuleObject + ", " + AlphArray( 1 ) );
				ShowContinueError( "  the " + cNumericFields( 4 ) + " must be greater than the " + cNumericFields( 6 ) + '.' );
				ErrorsFound = true;
			}
			if ( WaterCoil( CoilNum ).DesInletAirTemp >= WaterCoil( CoilNum ).DesOutletAirTemp ) {
				ShowSevereError( "For " + CurrentModuleObject + ", " + AlphArray( 1 ) );
				ShowContinueError( "  the " + cNumericFields( 5 ) + " must be less than the " + cNumericFields( 7 ) + '.' );
				ErrorsFound = true;
			}
			if ( WaterCoil( CoilNum ).DesInletAirTemp >= WaterCoil( CoilNum ).DesInletWaterTemp ) {
				ShowSevereError( "For " + CurrentModuleObject + ", " + AlphArray( 1 ) );
				ShowContinueError( "  the " + cNumericFields( 5 ) + " must be less than the " + cNumericFields( 4 ) + '.' );
				ErrorsFound = true;
			}

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 3 ), AlphArray( 4 ), "Water Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 5 ), AlphArray( 6 ), "Air Nodes" );

			//Setup the Simple Heating Coil reporting variables
			//CurrentModuleObject = "Coil:Heating:Water"
			SetupOutputVariable( "Heating Coil Heating Energy [J]", WaterCoil( CoilNum ).TotWaterHeatingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Source Side Heat Transfer Energy [J]", WaterCoil( CoilNum ).TotWaterHeatingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Heating Coil Heating Rate [W]", WaterCoil( CoilNum ).TotWaterHeatingCoilRate, "System", "Average", WaterCoil( CoilNum ).Name );
			SetupOutputVariable( "Heating Coil U Factor Times Area Value [W/K]", WaterCoil( CoilNum ).UACoilVariable, "System", "Average", WaterCoil( CoilNum ).Name );

		}

		CurrentModuleObject = "Coil:Cooling:Water:DetailedGeometry";
		// Get the data for detailed cooling coils.
		for ( FlatFinNum = 1; FlatFinNum <= NumFlatFin; ++FlatFinNum ) {

			CoilNum = NumSimpHeat + FlatFinNum;

			GetObjectItem( CurrentModuleObject, FlatFinNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			WaterCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			WaterCoilNumericFields( CoilNum ).FieldNames = "";
			WaterCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), WaterCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			WaterCoil( CoilNum ).Name = AlphArray( 1 );
			WaterCoil( CoilNum ).Schedule = AlphArray( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				WaterCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				WaterCoil( CoilNum ).SchedPtr = GetScheduleIndex( AlphArray( 2 ) );
				if ( WaterCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + AlphArray( 2 ) + " for " + cAlphaFields( 1 ) + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}

			WaterCoil( CoilNum ).WaterCoilTypeA = "Cooling";
			WaterCoil( CoilNum ).WaterCoilType = CoilType_Cooling; // 'Cooling'
			WaterCoil( CoilNum ).WaterCoilModelA = "DETAILED FLAT FIN";
			WaterCoil( CoilNum ).WaterCoilModel = CoilModel_Detailed; // 'DETAILED FLAT FIN'
			WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_DetFlatFinCooling;

			WaterCoil( CoilNum ).MaxWaterVolFlowRate = NumArray( 1 );
			if ( WaterCoil( CoilNum ).MaxWaterVolFlowRate == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).TubeOutsideSurfArea = NumArray( 2 );
			if ( WaterCoil( CoilNum ).TubeOutsideSurfArea == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).TotTubeInsideArea = NumArray( 3 );
			if ( WaterCoil( CoilNum ).TotTubeInsideArea == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).FinSurfArea = NumArray( 4 );
			if ( WaterCoil( CoilNum ).FinSurfArea == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).MinAirFlowArea = NumArray( 5 );
			if ( WaterCoil( CoilNum ).MinAirFlowArea == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).CoilDepth = NumArray( 6 );
			if ( WaterCoil( CoilNum ).CoilDepth == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).FinDiam = NumArray( 7 );
			if ( WaterCoil( CoilNum ).FinDiam == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).FinThickness = NumArray( 8 );
			if ( WaterCoil( CoilNum ).FinThickness <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + ": " + cNumericFields( 8 ) + " must be > 0.0, for " + cAlphaFields( 1 ) + " = " + WaterCoil( CoilNum ).Name );
				ErrorsFound = true;
			}
			WaterCoil( CoilNum ).TubeInsideDiam = NumArray( 9 );
			WaterCoil( CoilNum ).TubeOutsideDiam = NumArray( 10 );
			WaterCoil( CoilNum ).TubeThermConductivity = NumArray( 11 );
			if ( WaterCoil( CoilNum ).TubeThermConductivity <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + ": " + cNumericFields( 11 ) + " must be > 0.0, for " + cAlphaFields( 1 ) + " = " + WaterCoil( CoilNum ).Name );
				ErrorsFound = true;
			}
			WaterCoil( CoilNum ).FinThermConductivity = NumArray( 12 );
			if ( WaterCoil( CoilNum ).FinThermConductivity <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + ": " + cNumericFields( 12 ) + " must be > 0.0, for " + cAlphaFields( 1 ) + " = " + WaterCoil( CoilNum ).Name );
				ErrorsFound = true;
			}
			WaterCoil( CoilNum ).FinSpacing = NumArray( 13 );
			WaterCoil( CoilNum ).TubeDepthSpacing = NumArray( 14 );
			WaterCoil( CoilNum ).NumOfTubeRows = NumArray( 15 );
			WaterCoil( CoilNum ).NumOfTubesPerRow = NumArray( 16 );
			if ( WaterCoil( CoilNum ).NumOfTubesPerRow == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			WaterCoil( CoilNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			WaterCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			WaterCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			// A7 ; \field Name of Water Storage Tank for Condensate Collection
			WaterCoil( CoilNum ).CondensateCollectName = AlphArray( 7 );
			if ( lAlphaBlanks( 7 ) ) {
				WaterCoil( CoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				WaterCoil( CoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( WaterCoil( CoilNum ).Name, CurrentModuleObject, WaterCoil( CoilNum ).CondensateCollectName, ErrorsFound, WaterCoil( CoilNum ).CondensateTankID, WaterCoil( CoilNum ).CondensateTankSupplyARRID );
			}

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 3 ), AlphArray( 4 ), "Water Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 5 ), AlphArray( 6 ), "Air Nodes" );

			// Setup Report variables for the Detailed Flat Fin Cooling Coils
			// CurrentModuleObject = "Coil:Cooling:Water:DetailedGeometry"
			SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", WaterCoil( CoilNum ).TotWaterCoolingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Energy [J]", WaterCoil( CoilNum ).TotWaterCoolingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", WaterCoil( CoilNum ).SenWaterCoolingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", WaterCoil( CoilNum ).TotWaterCoolingCoilRate, "System", "Average", WaterCoil( CoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", WaterCoil( CoilNum ).SenWaterCoolingCoilRate, "System", "Average", WaterCoil( CoilNum ).Name );

			if ( WaterCoil( CoilNum ).CondensateCollectMode == CondensateToTank ) {

				SetupOutputVariable( "Cooling Coil Condensate Volume Flow Rate [m3/s]", WaterCoil( CoilNum ).CondensateVdot, "System", "Average", WaterCoil( CoilNum ).Name );
				SetupOutputVariable( "Cooling Coil Condensate Volume [m3]", WaterCoil( CoilNum ).CondensateVol, "System", "Sum", WaterCoil( CoilNum ).Name, _, "OnSiteWater", "Condensate", _, "System" );
			}

		}

		CurrentModuleObject = "Coil:Cooling:Water";
		// Get the data for Cooling coils.
		for ( CoolingNum = 1; CoolingNum <= NumCooling; ++CoolingNum ) {

			CoilNum = NumSimpHeat + NumFlatFin + CoolingNum;

			GetObjectItem( CurrentModuleObject, CoolingNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			WaterCoilNumericFields( CoilNum ).FieldNames.allocate( MaxNums );
			WaterCoilNumericFields( CoilNum ).FieldNames = "";
			WaterCoilNumericFields( CoilNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), WaterCoil, CoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, AlphArray( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			WaterCoil( CoilNum ).Name = AlphArray( 1 );
			WaterCoil( CoilNum ).Schedule = AlphArray( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				WaterCoil( CoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				WaterCoil( CoilNum ).SchedPtr = GetScheduleIndex( AlphArray( 2 ) );
				if ( WaterCoil( CoilNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + AlphArray( 2 ) + " for " + cAlphaFields( 1 ) + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}
			}

			WaterCoil( CoilNum ).WaterCoilTypeA = "Cooling";
			WaterCoil( CoilNum ).WaterCoilType = CoilType_Cooling; // 'Cooling'
			WaterCoil( CoilNum ).WaterCoilModelA = "Cooling";
			WaterCoil( CoilNum ).WaterCoilModel = CoilModel_Cooling; // 'Cooling'
			WaterCoil( CoilNum ).WaterCoilType_Num = WaterCoil_Cooling;

			WaterCoil( CoilNum ).MaxWaterVolFlowRate = NumArray( 1 ); //Liquid mass flow rate at Design  kg/s
			if ( WaterCoil( CoilNum ).MaxWaterVolFlowRate == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).DesAirVolFlowRate = NumArray( 2 ); //Dry air mass flow rate at Design (kg/s)
			if ( WaterCoil( CoilNum ).DesAirVolFlowRate == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).DesInletWaterTemp = NumArray( 3 ); //Entering water temperature at Design C
			if ( WaterCoil( CoilNum ).DesInletWaterTemp == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).DesInletAirTemp = NumArray( 4 ); //Entering air dry bulb temperature at Design(C)
			if ( WaterCoil( CoilNum ).DesInletAirTemp == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).DesOutletAirTemp = NumArray( 5 ); //Leaving air dry bulb temperature at Design(C)
			if ( WaterCoil( CoilNum ).DesOutletAirTemp == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).DesInletAirHumRat = NumArray( 6 ); //Entering air humidity ratio  at Design
			if ( WaterCoil( CoilNum ).DesInletAirHumRat == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;
			WaterCoil( CoilNum ).DesOutletAirHumRat = NumArray( 7 ); //Leaving air humidity ratio  at Design
			if ( WaterCoil( CoilNum ).DesOutletAirHumRat == AutoSize ) WaterCoil( CoilNum ).RequestingAutoSize = true;

			WaterCoil( CoilNum ).WaterInletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			WaterCoil( CoilNum ).WaterOutletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			WaterCoil( CoilNum ).AirInletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			WaterCoil( CoilNum ).AirOutletNodeNum = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			{ auto const SELECT_CASE_var( AlphArray( 7 ) );
			//The default is SimpleAnalysis = 2.  and DetailedAnalysis   =1
			if ( SELECT_CASE_var == "SIMPLEANALYSIS" ) {
				WaterCoil( CoilNum ).CoolingCoilAnalysisMode = SimpleAnalysis;

			} else if ( SELECT_CASE_var == "DETAILEDANALYSIS" ) {
				WaterCoil( CoilNum ).CoolingCoilAnalysisMode = DetailedAnalysis;

			} else {
				WaterCoil( CoilNum ).CoolingCoilAnalysisMode = SimpleAnalysis;
			}}

			{ auto const SELECT_CASE_var( AlphArray( 8 ) );
			//The default is CrossFlow = 2.  and CounterFlow=1
			if ( SELECT_CASE_var == "CROSSFLOW" ) {
				WaterCoil( CoilNum ).HeatExchType = CrossFlow;

			} else if ( SELECT_CASE_var == "COUNTERFLOW" ) {
				WaterCoil( CoilNum ).HeatExchType = CounterFlow;

			} else {
				WaterCoil( CoilNum ).HeatExchType = CrossFlow;
			}}

			//A9; \field Name of Water Storage Tank for Condensate Collection
			WaterCoil( CoilNum ).CondensateCollectName = AlphArray( 9 );
			if ( lAlphaBlanks( 9 ) ) {
				WaterCoil( CoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				WaterCoil( CoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( WaterCoil( CoilNum ).Name, CurrentModuleObject, WaterCoil( CoilNum ).CondensateCollectName, ErrorsFound, WaterCoil( CoilNum ).CondensateTankID, WaterCoil( CoilNum ).CondensateTankSupplyARRID );
			}

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 3 ), AlphArray( 4 ), "Water Nodes" );
			TestCompSet( CurrentModuleObject, AlphArray( 1 ), AlphArray( 5 ), AlphArray( 6 ), "Air Nodes" );

			// Setup Report variables for the Design input Cooling Coils
			// CurrentModuleObject = "Coil:Cooling:Water"
			SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", WaterCoil( CoilNum ).TotWaterCoolingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Cooling Coil Source Side Heat Transfer Energy [J]", WaterCoil( CoilNum ).TotWaterCoolingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", WaterCoil( CoilNum ).SenWaterCoolingCoilEnergy, "System", "Sum", WaterCoil( CoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", WaterCoil( CoilNum ).TotWaterCoolingCoilRate, "System", "Average", WaterCoil( CoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", WaterCoil( CoilNum ).SenWaterCoolingCoilRate, "System", "Average", WaterCoil( CoilNum ).Name );
			SetupOutputVariable( "Cooling Coil Wetted Area Fraction []", WaterCoil( CoilNum ).SurfAreaWetFraction, "System", "Average", WaterCoil( CoilNum ).Name );

			if ( WaterCoil( CoilNum ).CondensateCollectMode == CondensateToTank ) {

				SetupOutputVariable( "Cooling Coil Condensate Volume Flow Rate [m3/s]", WaterCoil( CoilNum ).CondensateVdot, "System", "Average", WaterCoil( CoilNum ).Name );
				SetupOutputVariable( "Cooling Coil Condensate Volume [m3]", WaterCoil( CoilNum ).CondensateVol, "System", "Sum", WaterCoil( CoilNum ).Name, _, "OnSiteWater", "Condensate", _, "System" );
			}

		}

		// added to store FouledCoilID for simple water heating and cooling coils
		for ( CoilNum = 1; CoilNum <= NumWaterCoils; ++CoilNum ) {
			if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_Cooling || WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) {
				for ( j1 = 1; j1 <= NumFouledCoil; ++j1 ) {
					if ( SameString( WaterCoil( CoilNum ).Name, FouledCoils( j1 ).FouledCoilName ) ) {
						FouledCoils( j1 ).FouledCoilID = CoilNum;
						break;
					}
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting input." );
		}

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		NumArray.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitWaterCoil(
		int const CoilNum,
		bool const FirstHVACIteration // unused1208
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       April 2004: Rahul Chillar
		//                      November 2013: XP, Tianzhen Hong to handle fouling coils
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the WaterCoil Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:

		// Using/Aliasing
		using General::RoundSigDigits;
		using General::SolveRegulaFalsi;
		using General::Iterate;
		using General::SafeDivide;
		using DataSizing::AutoSize;
		using DataSizing::CurSysNum;
		using namespace OutputReportPredefined;
		using DataPlant::ScanPlantLoopsForObject;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using namespace FaultsManager;
		using DataAirSystems::PrimaryAirSystem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallNo( 1.e-9 ); // SmallNo number in place of zero
		int const itmax( 10 );
		int const MaxIte( 500 ); // Maximum number of iterations
		Real64 const Acc( 0.0001 ); // Accuracy of result
		static std::string const RoutineName( "InitWaterCoil" );
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DesInletAirEnth; // Entering air enthalpy at rating (J/kg)
		Real64 DesOutletAirEnth; // Leaving air enthalpy at rating(J/kg)
		Real64 DesAirApparatusDewPtEnth; // Air enthalpy at apparatus dew point at rating(J/kg)
		Real64 DesSatEnthAtWaterInTemp; // Saturated enthalpy at entering liquid temp(J/kg)
		Real64 DesHumRatAtWaterInTemp; // Enthalpy at water inlet temp and entering air HumRat (J/kg)
		Real64 CapacitanceAir; // Air-side capacity rate(W/C)
		Real64 DesAirTempApparatusDewPt; // Temperature apparatus dew point at design capacity
		Real64 DesAirHumRatApparatusDewPt; // Humdity Ratio at apparatus dew point at design capacity
		Real64 DesBypassFactor; // ByPass Factor at design condition
		Real64 SlopeTempVsHumRatio; // Ratio temperature difference to humidity difference
		// between entering and leaving air states
		Real64 TempApparatusDewPtEstimate; // Estimate of TAdp from SlopeTempVsHumRatio
		Real64 Y1; // Previous values of dependent variable in ITERATE
		Real64 X1; // Previous values of independent variable in ITERATE
		Real64 error; // Deviation of dependent variable in iteration
		int iter; // Iteration counter
		int icvg; // Iteration convergence flag
		Real64 ResultX; // Output variable from ITERATE function.
		int Ipass; // loop index for App_Dewpoint_Loop
		static Real64 TOutNew( 0.0 ); // reset outlet air temperature for Coil:Cooling:Water
		static Real64 WOutNew( 0.0 ); // reset outlet air humidity ratio for Coil:Cooling:Water

		int AirInletNode;
		int WaterInletNode;
		int WaterOutletNode;

		static Array1D< Real64 > DesCpAir; // CpAir at Design Inlet Air Temp
		static Array1D< Real64 > DesUARangeCheck; // Value for range check based on Design Inlet Air Humidity Ratio
		/////////// hoisted into namespace InitWaterCoilOneTimeFlag
		//static bool MyOneTimeFlag( true );
		/////////////////////////
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyCoilReportFlag;
		static Array1D_bool PlantLoopScanFlag;

		static Array1D< Real64 > CoefSeries( 5 ); //Tuned Changed to static: High call count: Set before use
		Real64 FinDiamVar;
		Real64 TubeToFinDiamRatio;

		Real64 RhoAirStd; // density of air at standard conditions
		Real64 CpAirStd; // specific heat of air at std conditions
		int SolFla; // Flag of solver
		Real64 UA0; // lower bound for UA
		Real64 UA1; // upper bound for UA
		Real64 UA;
		static Array1D< Real64 > Par( 4 ); //Tuned Changed to static: High call count: Set before use

		static bool NoSatCurveIntersect( false ); // TRUE if failed to find appatatus dew-point
		static bool BelowInletWaterTemp( false ); // TRUE if apparatus dew-point below design inlet water temperature
		static bool CBFTooLarge( false ); // TRUE if the coil bypass factor is unrealistically large
		static bool NoExitCondReset( false ); // TRUE if exit condition reset is not to be done

		static Real64 RatedLatentCapacity( 0.0 ); // latent cooling capacity at the rating point [W]
		static Real64 RatedSHR( 0.0 ); // sensible heat ratio at the rating point
		static Real64 CapacitanceWater( 0.0 ); // capacitance of the water stream [W/K]
		static Real64 CMin( 0.0 ); // minimum capacitance of 2 streams [W/K]
		static Real64 CoilEffectiveness( 0.0 ); // effectiveness of the coil (rated)
		static Real64 SurfaceArea( 0.0 ); // heat exchanger surface area, [m2]
		static Real64 UATotal( 0.0 ); // heat exchanger UA total, [W/C]
		static Array1D_bool RptCoilHeaderFlag( 2, true );
		Real64 x_a; // result of Eq.70 in Wetter 1999
		Real64 x_w; // result of Eq.72 in Wetter 1999
		Real64 AirConvectTerm; // result of Eq.71 in Wetter 1999
		Real64 WaterConvectTerm; // result of Eq.73 in Wetter 1999
		Real64 WaterConvSensitivity; // "s" in Wetter 1999, temperature sensitivity in water side convection

		Real64 DesUACoilExternalEnth; // enthalpy based UAExternal for wet coil surface {kg/s}
		Real64 LogMeanEnthDiff; // long mean enthalpy difference {J/kg}
		Real64 LogMeanTempDiff; // long mean temperature difference {C}

		Real64 DesOutletWaterTemp;
		Real64 DesSatEnthAtWaterOutTemp;
		Real64 DesEnthAtWaterOutTempAirInHumRat;
		Real64 DesEnthWaterOut;
		Real64 Cp; // local fluid specific heat
		Real64 rho; // local fluid density
		bool errFlag;
		static Real64 EnthCorrFrac( 0.0 ); // enthalpy correction factor
		static Real64 TempCorrFrac( 0.0 ); // temperature correction factor
		int i;
		static Real64 rSchVal( 0.0 );

		// FLOW:

		if ( InitWaterCoilOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumWaterCoils );
			MySizeFlag.allocate( NumWaterCoils );
			CoilWarningOnceFlag.allocate( NumWaterCoils );
			DesCpAir.allocate( NumWaterCoils );
			MyUAAndFlowCalcFlag.allocate( NumWaterCoils );
			MyCoilDesignFlag.allocate( NumWaterCoils );
			MyCoilReportFlag.allocate( NumWaterCoils );
			DesUARangeCheck.allocate( NumWaterCoils );
			PlantLoopScanFlag.allocate( NumWaterCoils );

			DesCpAir = 0.0;
			DesUARangeCheck = 0.0;
			MyEnvrnFlag = true;
			MySizeFlag = true;
			CoilWarningOnceFlag = true;
			MyUAAndFlowCalcFlag = true;
			MyCoilDesignFlag = true;
			MyCoilReportFlag = true;
			InitWaterCoilOneTimeFlag = false;
			PlantLoopScanFlag = true;
		}

		if ( PlantLoopScanFlag( CoilNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).WaterCoilType_Num, WaterCoil( CoilNum ).WaterLoopNum, WaterCoil( CoilNum ).WaterLoopSide, WaterCoil( CoilNum ).WaterLoopBranchNum, WaterCoil( CoilNum ).WaterLoopCompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitWaterCoil: Program terminated for previous conditions." );
			}
			PlantLoopScanFlag( CoilNum ) = false;
		}
		if ( ! SysSizingCalc && MySizeFlag( CoilNum ) ) {
			// for each coil, do the sizing once.
			SizeWaterCoil( CoilNum );

			MySizeFlag( CoilNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( CoilNum ) ) {
			rho = GetDensityGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );
			//Initialize all report variables to a known state at beginning of simulation
			WaterCoil( CoilNum ).TotWaterHeatingCoilEnergy = 0.0;
			WaterCoil( CoilNum ).TotWaterCoolingCoilEnergy = 0.0;
			WaterCoil( CoilNum ).SenWaterCoolingCoilEnergy = 0.0;
			WaterCoil( CoilNum ).TotWaterHeatingCoilRate = 0.0;
			WaterCoil( CoilNum ).TotWaterCoolingCoilRate = 0.0;
			WaterCoil( CoilNum ).SenWaterCoolingCoilRate = 0.0;

			// The rest of the one time initializations
			AirInletNode = WaterCoil( CoilNum ).AirInletNodeNum;
			WaterInletNode = WaterCoil( CoilNum ).WaterInletNodeNum;
			WaterOutletNode = WaterCoil( CoilNum ).WaterOutletNodeNum;

			DesCpAir( CoilNum ) = PsyCpAirFnWTdb( 0.0, WaterCoil( CoilNum ).DesInletAirTemp );
			DesUARangeCheck( CoilNum ) = ( -1568.6 * WaterCoil( CoilNum ).DesInletAirHumRat + 20.157 );

			if ( WaterCoil( CoilNum ).WaterCoilType == CoilType_Cooling ) { // 'Cooling'
				Node( WaterInletNode ).Temp = 5.0;

				Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );

				Node( WaterInletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
				Node( WaterInletNode ).Quality = 0.0;
				Node( WaterInletNode ).Press = 0.0;
				Node( WaterInletNode ).HumRat = 0.0;
			}

			if ( WaterCoil( CoilNum ).WaterCoilType == CoilType_Heating ) { // 'Heating'
				Node( WaterInletNode ).Temp = 60.0;

				Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );

				Node( WaterInletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
				Node( WaterInletNode ).Quality = 0.0;
				Node( WaterInletNode ).Press = 0.0;
				Node( WaterInletNode ).HumRat = 0.0;

				if ( ( WaterCoil( CoilNum ).DesTotWaterCoilLoad != AutoSize ) && MyUAAndFlowCalcFlag( CoilNum ) ) {
					// calculate design water flow rate
					if ( ( WaterCoil( CoilNum ).CoilPerfInpMeth == NomCap ) || ( WaterCoil( CoilNum ).CoilPerfInpMeth == UAandFlow && WaterCoil( CoilNum ).MaxWaterVolFlowRate == AutoSize ) ) {
						// check for very small heating capacity
						if ( WaterCoil( CoilNum ).DesTotWaterCoilLoad > SmallLoad ) {
							WaterCoil( CoilNum ).MaxWaterVolFlowRate = WaterCoil( CoilNum ).DesTotWaterCoilLoad / ( Cp * rho * ( WaterCoil( CoilNum ).DesInletWaterTemp - WaterCoil( CoilNum ).DesOutletWaterTemp ) );
							// save the design water volumetric flow rate for use by the water loop sizing algorithms
							RegisterPlantCompDesignFlow( WaterCoil( CoilNum ).WaterInletNodeNum, WaterCoil( CoilNum ).MaxWaterVolFlowRate );
						} else {
							WaterCoil( CoilNum ).MaxWaterVolFlowRate = 0.0;
						}
					}
					// calculate the coil UA
					if ( ( WaterCoil( CoilNum ).CoilPerfInpMeth == NomCap ) || ( WaterCoil( CoilNum ).CoilPerfInpMeth == UAandFlow && WaterCoil( CoilNum ).UACoil == AutoSize ) ) {
						// check for very small heating capacity
						if ( WaterCoil( CoilNum ).DesTotWaterCoilLoad > SmallLoad ) {
							RhoAirStd = StdRhoAir;
							CpAirStd = PsyCpAirFnWTdb( 0.0, 20.0 );
							Par( 1 ) = WaterCoil( CoilNum ).DesTotWaterCoilLoad;
							Par( 2 ) = double( CoilNum );
							Par( 3 ) = double( ContFanCycCoil ); //fan operating mode
							Par( 4 ) = 1.0; // part-load ratio
							WaterCoil( CoilNum ).InletAirTemp = WaterCoil( CoilNum ).DesInletAirTemp;
							WaterCoil( CoilNum ).InletAirHumRat = 0.008;
							WaterCoil( CoilNum ).InletWaterTemp = WaterCoil( CoilNum ).DesInletWaterTemp;
							WaterCoil( CoilNum ).InletWaterMassFlowRate = rho * WaterCoil( CoilNum ).MaxWaterVolFlowRate;
							WaterCoil( CoilNum ).InletAirMassFlowRate = WaterCoil( CoilNum ).DesTotWaterCoilLoad / ( CpAirStd * ( WaterCoil( CoilNum ).DesOutletAirTemp - WaterCoil( CoilNum ).DesInletAirTemp ) );
							WaterCoil( CoilNum ).DesAirVolFlowRate = WaterCoil( CoilNum ).InletAirMassFlowRate / RhoAirStd;
							// set the lower and upper limits on the UA
							UA0 = 0.001 * WaterCoil( CoilNum ).DesTotWaterCoilLoad;
							UA1 = WaterCoil( CoilNum ).DesTotWaterCoilLoad;
							// Invert the simple heating coil model: given the design inlet conditions and the design load, fins the design UA
							SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, SimpleHeatingCoilUAResidual, UA0, UA1, Par );
							// if the numerical inversion failed, issue error messages.
							if ( SolFla == -1 ) {
								ShowSevereError( "Calculation of heating coil UA failed for coil " + WaterCoil( CoilNum ).Name );
								ShowContinueError( "  Iteration limit exceeded in calculating coil UA" );
								ShowFatalError( "Preceding error causes program termination" );
							} else if ( SolFla == -2 ) {
								ShowSevereError( "Calculation of heating coil UA failed for coil " + WaterCoil( CoilNum ).Name );
								ShowContinueError( "  Bad starting values for UA" );
								ShowFatalError( "Preceding error causes program termination" );
							}
							WaterCoil( CoilNum ).UACoil = UA;
						} else {
							WaterCoil( CoilNum ).UACoil = 1.0;
						}
					}
				}
				MyUAAndFlowCalcFlag( CoilNum ) = false;
				//fill values for variable UA
				CpAirStd = PsyCpAirFnWTdb( 0.0, 20.0 );
				WaterCoil( CoilNum ).DesAirMassFlowRate = StdRhoAir * WaterCoil( CoilNum ).DesAirVolFlowRate;
				WaterCoil( CoilNum ).LiquidSideNominalConvect = WaterCoil( CoilNum ).UACoil * ( WaterCoil( CoilNum ).RatioAirSideToWaterSideConvect + 1 ) / WaterCoil( CoilNum ).RatioAirSideToWaterSideConvect;
				WaterCoil( CoilNum ).AirSideNominalConvect = WaterCoil( CoilNum ).RatioAirSideToWaterSideConvect * WaterCoil( CoilNum ).LiquidSideNominalConvect;
			} else {
				MyUAAndFlowCalcFlag( CoilNum ) = false;
			}

			WaterCoil( CoilNum ).MaxWaterMassFlowRate = rho * WaterCoil( CoilNum ).MaxWaterVolFlowRate;

			InitComponentNodes( 0.0, WaterCoil( CoilNum ).MaxWaterMassFlowRate, WaterCoil( CoilNum ).WaterInletNodeNum, WaterCoil( CoilNum ).WaterOutletNodeNum, WaterCoil( CoilNum ).WaterLoopNum, WaterCoil( CoilNum ).WaterLoopSide, WaterCoil( CoilNum ).WaterLoopBranchNum, WaterCoil( CoilNum ).WaterLoopCompNum );

			// effective fin diameter for detailed flat fin coil
			if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
				WaterCoil( CoilNum ).EffectiveFinDiam = std::sqrt( 4.0 * WaterCoil( CoilNum ).FinDiam * WaterCoil( CoilNum ).CoilDepth / ( Pi * WaterCoil( CoilNum ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow ) );

				//   calculate fixed geometric parameters of the coil:
				//   Total Area
				WaterCoil( CoilNum ).TotCoilOutsideSurfArea = WaterCoil( CoilNum ).TubeOutsideSurfArea + WaterCoil( CoilNum ).FinSurfArea;
				//   Effective Tube Inside Diameter - the model assumes that the coil
				//   can be simulated as a tube with an equivalent hydraulic diameter.
				WaterCoil( CoilNum ).CoilEffectiveInsideDiam = 4.0 * WaterCoil( CoilNum ).MinAirFlowArea * WaterCoil( CoilNum ).CoilDepth / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
				//   Ratio of tube outside diameter to effective fin diameter should always
				//   be less than 1
				TubeToFinDiamRatio = WaterCoil( CoilNum ).TubeOutsideDiam / WaterCoil( CoilNum ).EffectiveFinDiam;
				if ( TubeToFinDiamRatio > 1.0 ) {
					ShowWarningError( "InitWaterCoil: Detailed Flat Fin Coil, TubetoFinDiamRatio > 1.0, [" + RoundSigDigits( TubeToFinDiamRatio, 4 ) + ']' );
					// reset tube depth spacing and recalc dependent parameters
					WaterCoil( CoilNum ).TubeDepthSpacing *= ( pow_2( TubeToFinDiamRatio ) + 0.1 );
					WaterCoil( CoilNum ).CoilDepth = WaterCoil( CoilNum ).TubeDepthSpacing * WaterCoil( CoilNum ).NumOfTubeRows;
					WaterCoil( CoilNum ).EffectiveFinDiam = std::sqrt( 4.0 * WaterCoil( CoilNum ).FinDiam * WaterCoil( CoilNum ).CoilDepth / ( Pi * WaterCoil( CoilNum ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow ) );
					WaterCoil( CoilNum ).CoilEffectiveInsideDiam = 4.0 * WaterCoil( CoilNum ).MinAirFlowArea * WaterCoil( CoilNum ).CoilDepth / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
					TubeToFinDiamRatio = WaterCoil( CoilNum ).TubeOutsideDiam / WaterCoil( CoilNum ).EffectiveFinDiam;
					ShowContinueError( "  Resetting tube depth spacing to " + RoundSigDigits( WaterCoil( CoilNum ).TubeDepthSpacing, 4 ) + " meters" );
					ShowContinueError( "  Resetting coil depth to " + RoundSigDigits( WaterCoil( CoilNum ).CoilDepth, 4 ) + " meters" );
				}

				CalcDryFinEffCoef( TubeToFinDiamRatio, CoefSeries );

				WaterCoil( CoilNum ).DryFinEfficncyCoef = CoefSeries;

				FinDiamVar = 0.5 * ( WaterCoil( CoilNum ).EffectiveFinDiam - WaterCoil( CoilNum ).TubeOutsideDiam );

				WaterCoil( CoilNum ).GeometryCoef1 = 0.159 * std::pow( WaterCoil( CoilNum ).FinThickness / WaterCoil( CoilNum ).CoilEffectiveInsideDiam, -0.065 ) * std::pow( WaterCoil( CoilNum ).FinThickness / FinDiamVar, 0.141 );
				WaterCoil( CoilNum ).GeometryCoef2 = -0.323 * std::pow( WaterCoil( CoilNum ).FinSpacing / FinDiamVar, 0.049 ) * std::pow( WaterCoil( CoilNum ).EffectiveFinDiam / WaterCoil( CoilNum ).TubeDepthSpacing, 0.549 ) * std::pow( WaterCoil( CoilNum ).FinThickness / WaterCoil( CoilNum ).FinSpacing, -0.028 );

				// Set some initial values for simulation
				WaterCoil( CoilNum ).SatEnthlCurveConstCoef = -10.57;
				WaterCoil( CoilNum ).SatEnthlCurveSlope = 3.3867;
				WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope = 3.3867;
				WaterCoil( CoilNum ).EnthVsTempCurveConst = -10.57;
				// Set Saved Values to Zero
				WaterCoil( CoilNum ).SurfAreaWetSaved = 0.0;
				WaterCoil( CoilNum ).MeanWaterTempSaved = 0.0;
				WaterCoil( CoilNum ).InWaterTempSaved = 0.0;
				WaterCoil( CoilNum ).OutWaterTempSaved = 0.0;

			} // End the Detailed Flat Fin Coil Initialization

			// Calculation for Cooling Coil, The part between the '@@@' are design condition
			// and are calculated only once to calculate standard values for UAs and other physical parameters of
			// the cooling coil.
			// Basic Idea for UA:  Heat Transfer= UAenthalpybased*(Delta enthalpy), this is a necessity since the
			// coil may be Wet or Dry or Partially Wet-Dry, so latent effects are accounted for in this model while
			// calculating the UA. A fictitious specific heat is also defined to caculate the conventional UA.
			// On the air side, enthalpy capacity rate is the air mass flow rate,while on water side it is
			// enthalpy of saturated air at water temperature.
			//@@@ DESIGN CONDITION BEGIN HERE @@@

			// Check for zero design cooling capacity as specified by coil design inputs
			if ( MyCoilDesignFlag( CoilNum ) && ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Cooling ) && ( WaterCoil( CoilNum ).DesAirVolFlowRate > 0.0 ) && ( WaterCoil( CoilNum ).MaxWaterMassFlowRate > 0.0 ) ) {

				DesInletAirEnth = PsyHFnTdbW( WaterCoil( CoilNum ).DesInletAirTemp, WaterCoil( CoilNum ).DesInletAirHumRat );
				DesOutletAirEnth = PsyHFnTdbW( WaterCoil( CoilNum ).DesOutletAirTemp, WaterCoil( CoilNum ).DesOutletAirHumRat );
				DesSatEnthAtWaterInTemp = PsyHFnTdbW( WaterCoil( CoilNum ).DesInletWaterTemp, PsyWFnTdpPb( WaterCoil( CoilNum ).DesInletWaterTemp, StdBaroPress ) );
				// check for dry coil
				DesHumRatAtWaterInTemp = PsyWFnTdbH( WaterCoil( CoilNum ).DesInletWaterTemp, DesSatEnthAtWaterInTemp, RoutineName );
				if ( DesHumRatAtWaterInTemp > WaterCoil( CoilNum ).DesOutletAirHumRat && WaterCoil( CoilNum ).DesOutletAirTemp > WaterCoil( CoilNum ).DesInletWaterTemp ) {
					// if the design outlet air humrat is lower than the saturated air humrat at the design inlet water temp
					// and the design outlet air temperature is higher than the design inlet water temp (i.e, cooling possible),
					// move the design outlet air saturated enthalpy down (i.e., to Twaterin, Wair,out) to allow the coil to size.
					DesSatEnthAtWaterInTemp = PsyHFnTdbW( WaterCoil( CoilNum ).DesInletWaterTemp, WaterCoil( CoilNum ).DesOutletAirHumRat ) - 0.0001;
				}
				if ( DesOutletAirEnth >= DesInletAirEnth || WaterCoil( CoilNum ).DesInletWaterTemp >= WaterCoil( CoilNum ).DesInletAirTemp ) {
					ShowWarningError( "The design cooling capacity is zero for Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
					ShowContinueError( "  The maximum water flow rate for this coil will be set to zero and the coil will do no cooling." );
					ShowContinueError( "  Check the following coil design inputs for problems: Tair,in = " + RoundSigDigits( WaterCoil( CoilNum ).DesInletAirTemp, 4 ) );
					ShowContinueError( "                                                       Wair,in = " + RoundSigDigits( WaterCoil( CoilNum ).DesInletAirHumRat, 6 ) );
					ShowContinueError( "                                                       Twater,in = " + RoundSigDigits( WaterCoil( CoilNum ).DesInletWaterTemp, 4 ) );
					ShowContinueError( "                                                       Tair,out = " + RoundSigDigits( WaterCoil( CoilNum ).DesOutletAirTemp, 4 ) );
					ShowContinueError( "                                                       Wair,out = " + RoundSigDigits( WaterCoil( CoilNum ).DesOutletAirHumRat, 6 ) );
					WaterCoil( CoilNum ).MaxWaterVolFlowRate = 0.0;
					WaterCoil( CoilNum ).MaxWaterMassFlowRate = 0.0;
				}
			}

			if ( MyCoilDesignFlag( CoilNum ) && ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Cooling ) && ( WaterCoil( CoilNum ).DesAirVolFlowRate > 0.0 ) && ( WaterCoil( CoilNum ).MaxWaterMassFlowRate > 0.0 ) ) { // 'Cooling'

				MyCoilDesignFlag( CoilNum ) = false;
				NoSatCurveIntersect = false;
				BelowInletWaterTemp = false;
				CBFTooLarge = false;
				NoExitCondReset = false;
				for ( Ipass = 1; Ipass <= 2; ++Ipass ) {
					if ( Ipass == 2 ) {
						if ( ! NoSatCurveIntersect && ! BelowInletWaterTemp && ! CBFTooLarge ) {
							goto Inlet_Conditions_Loop_exit; // coil UA calcs OK
						} else {
							ShowWarningError( "In calculating the design coil UA for Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
							if ( NoSatCurveIntersect ) {
								ShowContinueError( "no apparatus dew-point can be found for the initial entering and leaving conditions;" );
							}
							if ( BelowInletWaterTemp ) {
								ShowContinueError( "the apparatus dew-point is below the coil design inlet water temperature;" );
							}
							if ( CBFTooLarge ) {
								ShowContinueError( "the coil bypass factor is unrealistically large;" );
							}
							if ( ! NoExitCondReset ) {
								ShowContinueError( "the coil outlet design conditions will be changed to correct the problem." );
							}
							ShowContinueError( "The initial design conditions are: Tair,in = " + RoundSigDigits( WaterCoil( CoilNum ).DesInletAirTemp, 4 ) );
							ShowContinueError( "                                   Wair,in = " + RoundSigDigits( WaterCoil( CoilNum ).DesInletAirHumRat, 6 ) );
							ShowContinueError( "                                   Twater,in = " + RoundSigDigits( WaterCoil( CoilNum ).DesInletWaterTemp, 4 ) );
							ShowContinueError( "                                   Tair,out = " + RoundSigDigits( WaterCoil( CoilNum ).DesOutletAirTemp, 4 ) );
							ShowContinueError( "                                   Wair,out = " + RoundSigDigits( WaterCoil( CoilNum ).DesOutletAirHumRat, 6 ) );
							if ( ! NoExitCondReset ) {
								ShowContinueError( "The revised design conditions are: Tair,out = " + RoundSigDigits( TOutNew, 4 ) );
								ShowContinueError( "                                   Wair,out = " + RoundSigDigits( WOutNew, 6 ) );
								WaterCoil( CoilNum ).DesOutletAirHumRat = WOutNew;
								WaterCoil( CoilNum ).DesOutletAirTemp = TOutNew;
							}
						}
					}

					// Volume flow rate being converted to mass flow rate for water
					WaterCoil( CoilNum ).DesAirMassFlowRate = StdRhoAir * WaterCoil( CoilNum ).DesAirVolFlowRate;

					// Enthalpy of Air at Inlet design conditions
					DesInletAirEnth = PsyHFnTdbW( WaterCoil( CoilNum ).DesInletAirTemp, WaterCoil( CoilNum ).DesInletAirHumRat );

					// Enthalpy of Air at outlet at design conditions
					DesOutletAirEnth = PsyHFnTdbW( WaterCoil( CoilNum ).DesOutletAirTemp, WaterCoil( CoilNum ).DesOutletAirHumRat );

					// already calculated above and possibly reset if dry coil
					//        ! Enthalpy of Water at Inlet design conditions
					//        DesSatEnthAtWaterInTemp =PsyHFnTdbW(WaterCoil(CoilNum)%DesInletWaterTemp, &
					//                                             PsyWFnTdpPb(WaterCoil(CoilNum)%DesInletWaterTemp,StdBaroPress))

					// Total Coil Load from Inlet and Outlet Air States.
					WaterCoil( CoilNum ).DesTotWaterCoilLoad = WaterCoil( CoilNum ).DesAirMassFlowRate * ( DesInletAirEnth - DesOutletAirEnth );
					if ( CurSysNum > 0 ) {
						WaterCoil( CoilNum ).DesTotWaterCoilLoad = WaterCoil( CoilNum ).DesTotWaterCoilLoad + PrimaryAirSystem( CurSysNum ).FanDesCoolLoad;
					}

					// Enthalpy of Water at Intlet design conditions
					Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, WaterCoil( CoilNum ).DesInletWaterTemp, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );

					DesOutletWaterTemp = WaterCoil( CoilNum ).DesInletWaterTemp + WaterCoil( CoilNum ).DesTotWaterCoilLoad / ( WaterCoil( CoilNum ).MaxWaterMassFlowRate * Cp );

					DesSatEnthAtWaterOutTemp = PsyHFnTdbW( DesOutletWaterTemp, PsyWFnTdpPb( DesOutletWaterTemp, StdBaroPress ) );
					DesEnthAtWaterOutTempAirInHumRat = PsyHFnTdbW( DesOutletWaterTemp, WaterCoil( CoilNum ).DesInletAirHumRat );
					DesEnthWaterOut = min( DesSatEnthAtWaterOutTemp, DesEnthAtWaterOutTempAirInHumRat );

					// dry coil test
					if ( WaterCoil( CoilNum ).DesOutletAirHumRat < WaterCoil( CoilNum ).DesInletAirHumRat && DesHumRatAtWaterInTemp < WaterCoil( CoilNum ).DesInletAirHumRat ) { // wet coil

						// Calculations for BYPASS FACTOR at design conditions
						// Calculate "slope" of temperature vs. humidity ratio between entering and leaving states
						SlopeTempVsHumRatio = ( WaterCoil( CoilNum ).DesInletAirTemp - WaterCoil( CoilNum ).DesOutletAirTemp ) / max( ( WaterCoil( CoilNum ).DesInletAirHumRat - WaterCoil( CoilNum ).DesOutletAirHumRat ), SmallNo );

						// Initialize iteration parameters
						DesAirTempApparatusDewPt = PsyTdpFnWPb( WaterCoil( CoilNum ).DesOutletAirHumRat, OutBaroPress );

						// Iterating to calculate Apparatus Dew Point Temperature at Design Conditions
						for ( iter = 1; iter <= itmax; ++iter ) {

							// Calculate apparatus dewpoint and compare with predicted value
							// using entering conditions and SlopeTempVsHumRatio
							DesAirHumRatApparatusDewPt = PsyWFnTdpPb( DesAirTempApparatusDewPt, OutBaroPress );

							// Initial Estimate for apparatus Dew Point Temperature
							TempApparatusDewPtEstimate = WaterCoil( CoilNum ).DesInletAirTemp - SlopeTempVsHumRatio * ( WaterCoil( CoilNum ).DesInletAirHumRat - DesAirHumRatApparatusDewPt );

							// Iterating to calculate Apparatus Dew Point Temperature at Design Condition
							error = DesAirTempApparatusDewPt - TempApparatusDewPtEstimate;
							Iterate( ResultX, 0.01, DesAirTempApparatusDewPt, error, X1, Y1, iter, icvg );
							DesAirTempApparatusDewPt = ResultX;

							// If converged, exit loop
							if ( icvg == 1 ) {
								goto App_DewPoint_Loop1_exit;
							}

							// If not converged due to low Humidity Ratio approximate value at outlet conditions
							if ( iter == itmax ) {
								NoSatCurveIntersect = true;
								DesAirTempApparatusDewPt = PsyTdpFnWPb( WaterCoil( CoilNum ).DesOutletAirHumRat, OutBaroPress );
								DesAirHumRatApparatusDewPt = PsyWFnTdpPb( DesAirTempApparatusDewPt, OutBaroPress );
								goto App_DewPoint_Loop1_exit;
							}

							// End of Loop for Iteration
						}
						App_DewPoint_Loop1_exit: ;

						// Air enthalpy at apparatus dew point at design conditions
						DesAirApparatusDewPtEnth = PsyHFnTdbW( DesAirTempApparatusDewPt, DesAirHumRatApparatusDewPt );

						// Calculate bypass factor from enthalpies calculated above.
						DesBypassFactor = ( DesOutletAirEnth - DesAirApparatusDewPtEnth ) / ( DesInletAirEnth - DesAirApparatusDewPtEnth );

						// Check for bypass factor for unsuitable value. Note that bypass factor is never used in the coil calculation
						if ( ( DesBypassFactor > 0.5 ) || ( DesBypassFactor < 0.0 ) ) {
							CBFTooLarge = true;
							DesBypassFactor = 0.37;
						}

						if ( DesEnthWaterOut > DesInletAirEnth ) {
							ShowWarningError( "In calculating the design coil UA for Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
							ShowContinueError( "the outlet chilled water design enthalpy is greater than the inlet air design enthalpy." );
							ShowContinueError( "To correct this condition the design chilled water flow rate will be increased from " + RoundSigDigits( WaterCoil( CoilNum ).MaxWaterVolFlowRate, 5 ) );
							EnthCorrFrac = ( DesEnthWaterOut - DesInletAirEnth ) / ( DesEnthWaterOut - DesSatEnthAtWaterInTemp );
							WaterCoil( CoilNum ).MaxWaterVolFlowRate *= ( 1.0 + 2.0 * EnthCorrFrac );
							ShowContinueError( "to " + RoundSigDigits( WaterCoil( CoilNum ).MaxWaterVolFlowRate, 5 ) + " m3/s" );
							WaterCoil( CoilNum ).MaxWaterMassFlowRate = rho * WaterCoil( CoilNum ).MaxWaterVolFlowRate;
							DesOutletWaterTemp = WaterCoil( CoilNum ).DesInletWaterTemp + WaterCoil( CoilNum ).DesTotWaterCoilLoad / ( WaterCoil( CoilNum ).MaxWaterMassFlowRate * Cp );
							DesSatEnthAtWaterOutTemp = PsyHFnTdbW( DesOutletWaterTemp, PsyWFnTdpPb( DesOutletWaterTemp, StdBaroPress ) );
							DesEnthAtWaterOutTempAirInHumRat = PsyHFnTdbW( DesOutletWaterTemp, WaterCoil( CoilNum ).DesInletAirHumRat );
							DesEnthWaterOut = min( DesSatEnthAtWaterOutTemp, DesEnthAtWaterOutTempAirInHumRat );
						}

						// Determine air-side coefficient, UACoilExternal, assuming that the
						// surface temperature is at the apparatus dewpoint temperature
						if ( DesAirApparatusDewPtEnth <= DesSatEnthAtWaterInTemp ) BelowInletWaterTemp = true;
						if ( ( DesInletAirEnth - DesEnthWaterOut ) > SmallNo && ( DesOutletAirEnth - DesSatEnthAtWaterInTemp ) > SmallNo ) {
							LogMeanEnthDiff = ( ( DesInletAirEnth - DesEnthWaterOut ) - ( DesOutletAirEnth - DesSatEnthAtWaterInTemp ) ) / std::log( ( DesInletAirEnth - DesEnthWaterOut ) / ( DesOutletAirEnth - DesSatEnthAtWaterInTemp ) );
						} else {
							LogMeanEnthDiff = 2000.0; // UA will be 1/2 the design coil load
						}
						DesUACoilExternalEnth = WaterCoil( CoilNum ).DesTotWaterCoilLoad / LogMeanEnthDiff;
						WaterCoil( CoilNum ).UACoilExternal = DesUACoilExternalEnth * PsyCpAirFnWTdb( WaterCoil( CoilNum ).DesInletAirHumRat, WaterCoil( CoilNum ).DesInletAirTemp );

						if ( Ipass == 1 && ( NoSatCurveIntersect || CBFTooLarge || BelowInletWaterTemp ) ) {
							// reset outlet conditions to 90% relative humidity at the same outlet enthalpy
							TOutNew = TdbFnHRhPb( DesOutletAirEnth, 0.9, StdBaroPress );
							WOutNew = PsyWFnTdbH( TOutNew, DesOutletAirEnth );
							if ( WOutNew >= WaterCoil( CoilNum ).DesInletAirHumRat || TOutNew > WaterCoil( CoilNum ).DesOutletAirTemp ) {
								NoExitCondReset = true;
							}
							goto Inlet_Conditions_Loop_loop;
						}

						WaterCoil( CoilNum ).UACoilInternal = WaterCoil( CoilNum ).UACoilExternal * 3.30;
						// Overall heat transfer coefficient
						WaterCoil( CoilNum ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoilExternal + 1.0 / WaterCoil( CoilNum ).UACoilInternal );

					} else { // dry coil

						if ( DesOutletWaterTemp > WaterCoil( CoilNum ).DesInletAirTemp ) {
							ShowWarningError( "In calculating the design coil UA for Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
							ShowContinueError( "the outlet chilled water design temperature is greater than the inlet air design temperature." );
							ShowContinueError( "To correct this condition the design chilled water flow rate will be increased from " + RoundSigDigits( WaterCoil( CoilNum ).MaxWaterVolFlowRate, 5 ) );
							TempCorrFrac = ( DesOutletWaterTemp - WaterCoil( CoilNum ).DesInletAirTemp ) / ( DesOutletWaterTemp - WaterCoil( CoilNum ).DesInletWaterTemp );
							WaterCoil( CoilNum ).MaxWaterVolFlowRate *= ( 1.0 + 2.0 * TempCorrFrac );
							ShowContinueError( "to " + RoundSigDigits( WaterCoil( CoilNum ).MaxWaterVolFlowRate, 5 ) + " m3/s" );
							WaterCoil( CoilNum ).MaxWaterMassFlowRate = rho * WaterCoil( CoilNum ).MaxWaterVolFlowRate;
							DesOutletWaterTemp = WaterCoil( CoilNum ).DesInletWaterTemp + WaterCoil( CoilNum ).DesTotWaterCoilLoad / ( WaterCoil( CoilNum ).MaxWaterMassFlowRate * Cp );
						}

						if ( ( WaterCoil( CoilNum ).DesInletAirTemp - DesOutletWaterTemp ) > SmallNo && ( WaterCoil( CoilNum ).DesOutletAirTemp - WaterCoil( CoilNum ).DesInletWaterTemp ) > SmallNo ) {
							LogMeanTempDiff = ( ( WaterCoil( CoilNum ).DesInletAirTemp - DesOutletWaterTemp ) - ( WaterCoil( CoilNum ).DesOutletAirTemp - WaterCoil( CoilNum ).DesInletWaterTemp ) ) / std::log( ( WaterCoil( CoilNum ).DesInletAirTemp - DesOutletWaterTemp ) / ( WaterCoil( CoilNum ).DesOutletAirTemp - WaterCoil( CoilNum ).DesInletWaterTemp ) );
							WaterCoil( CoilNum ).UACoilExternal = WaterCoil( CoilNum ).DesTotWaterCoilLoad / LogMeanTempDiff;
						} else {
							WaterCoil( CoilNum ).UACoilExternal = WaterCoil( CoilNum ).DesTotWaterCoilLoad / 2.0; // make the UA large
						}
						WaterCoil( CoilNum ).UACoilInternal = WaterCoil( CoilNum ).UACoilExternal * 3.30;
						// Overall heat transfer coefficient
						WaterCoil( CoilNum ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoilExternal + 1.0 / WaterCoil( CoilNum ).UACoilInternal );
						goto Inlet_Conditions_Loop_exit;

					}

					Inlet_Conditions_Loop_loop: ;
				}
				Inlet_Conditions_Loop_exit: ;

				// estimate the heat external transfer surface area using typical design over all U value
				WaterCoil( CoilNum ).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea( CoilNum );
				// calculate internal and external "UA per external surface area"
				WaterCoil( CoilNum ).UACoilInternalPerUnitArea = WaterCoil( CoilNum ).UACoilInternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
				WaterCoil( CoilNum ).UAWetExtPerUnitArea = WaterCoil( CoilNum ).UACoilExternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
				// approximate the dry UA as 1.0 times wet UA
				WaterCoil( CoilNum ).UADryExtPerUnitArea = WaterCoil( CoilNum ).UAWetExtPerUnitArea;

				// Now use SolveRegulaFalsi to "invert" the cooling coil model to obtain the UA given the specified design inlet and outlet conditions
				// Note that the UAs we have obtained so far are rough estimates that are the starting points for the the following iterative
				//   calulation of the actual UAs.
				Par( 1 ) = WaterCoil( CoilNum ).DesTotWaterCoilLoad;
				Par( 2 ) = double( CoilNum );
				Par( 3 ) = double( ContFanCycCoil ); //fan operating mode
				Par( 4 ) = 1.0; // part-load ratio
				WaterCoil( CoilNum ).InletAirTemp = WaterCoil( CoilNum ).DesInletAirTemp;
				WaterCoil( CoilNum ).InletAirHumRat = WaterCoil( CoilNum ).DesInletAirHumRat;
				WaterCoil( CoilNum ).InletWaterTemp = WaterCoil( CoilNum ).DesInletWaterTemp;
				WaterCoil( CoilNum ).InletWaterMassFlowRate = rho * WaterCoil( CoilNum ).MaxWaterVolFlowRate;
				WaterCoil( CoilNum ).InletAirMassFlowRate = WaterCoil( CoilNum ).DesAirMassFlowRate;
				// set the lower and upper limits on the UA
				UA0 = 0.1 * WaterCoil( CoilNum ).UACoilExternal;
				UA1 = 10.0 * WaterCoil( CoilNum ).UACoilExternal;
				// Invert the simple cooling coil model: given the design inlet conditions and the design load, find the design UA
				SolveRegulaFalsi( 0.001, MaxIte, SolFla, UA, SimpleCoolingCoilUAResidual, UA0, UA1, Par );
				// if the numerical inversion failed, issue error messages.
				if ( SolFla == -1 ) {
					ShowSevereError( "Calculation of cooling coil design UA failed for coil " + WaterCoil( CoilNum ).Name );
					ShowContinueError( "  Iteration limit exceeded in calculating coil UA" );
					// CALL ShowFatalError('Preceeding error causes program termination')
					WaterCoil( CoilNum ).UACoilExternal = UA0 * 10.0;
					WaterCoil( CoilNum ).UACoilInternal = WaterCoil( CoilNum ).UACoilExternal * 3.3;
					WaterCoil( CoilNum ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoilExternal + 1.0 / WaterCoil( CoilNum ).UACoilInternal );
					WaterCoil( CoilNum ).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea( CoilNum );
					WaterCoil( CoilNum ).UACoilInternalPerUnitArea = WaterCoil( CoilNum ).UACoilInternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
					WaterCoil( CoilNum ).UAWetExtPerUnitArea = WaterCoil( CoilNum ).UACoilExternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
					WaterCoil( CoilNum ).UADryExtPerUnitArea = WaterCoil( CoilNum ).UAWetExtPerUnitArea;
					ShowContinueError( " Coil design UA set to " + RoundSigDigits( WaterCoil( CoilNum ).UACoilTotal, 6 ) + " [W/C]" );
				} else if ( SolFla == -2 ) {
					ShowSevereError( "Calculation of cooling coil design UA failed for coil " + WaterCoil( CoilNum ).Name );
					ShowContinueError( "  Bad starting values for UA" );
					// CALL ShowFatalError('Preceeding error causes program termination')
					WaterCoil( CoilNum ).UACoilExternal = UA0 * 10.0;
					WaterCoil( CoilNum ).UACoilInternal = WaterCoil( CoilNum ).UACoilExternal * 3.3;
					WaterCoil( CoilNum ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoilExternal + 1.0 / WaterCoil( CoilNum ).UACoilInternal );
					WaterCoil( CoilNum ).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea( CoilNum );
					WaterCoil( CoilNum ).UACoilInternalPerUnitArea = WaterCoil( CoilNum ).UACoilInternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
					WaterCoil( CoilNum ).UAWetExtPerUnitArea = WaterCoil( CoilNum ).UACoilExternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
					WaterCoil( CoilNum ).UADryExtPerUnitArea = WaterCoil( CoilNum ).UAWetExtPerUnitArea;
					ShowContinueError( " Coil design UA set to " + RoundSigDigits( WaterCoil( CoilNum ).UACoilTotal, 6 ) + " [W/C]" );
				}

				// cooling coil surface area
				SurfaceArea = WaterCoil( CoilNum ).TotCoilOutsideSurfArea;

				// cooling coil overall UA value
				UATotal = WaterCoil( CoilNum ).UACoilTotal;

				// save the design internal and external UAs
				WaterCoil( CoilNum ).UACoilExternalDes = WaterCoil( CoilNum ).UACoilExternal;
				WaterCoil( CoilNum ).UACoilInternalDes = WaterCoil( CoilNum ).UACoilInternal;

			}

			//@@@@ DESIGN CONDITION END HERE @@@@

			// Calculate rated Total, latent, sensible capacity, SHR, effectiveness
			if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) {
				WaterCoil( CoilNum ).InletAirTemp = 16.6;
				WaterCoil( CoilNum ).InletAirHumRat = PsyWFnTdbRhPb( 16.6, 0.5, StdBaroPress, RoutineName );
				WaterCoil( CoilNum ).InletWaterTemp = 82.2;
			} else {
				WaterCoil( CoilNum ).InletAirTemp = 26.67;
				WaterCoil( CoilNum ).InletAirHumRat = PsyWFnTdbTwbPb( 26.67, 19.44, StdBaroPress, RoutineName );
				WaterCoil( CoilNum ).InletWaterTemp = 6.67;
			}
			WaterCoil( CoilNum ).InletAirEnthalpy = PsyHFnTdbW( WaterCoil( CoilNum ).InletAirTemp, WaterCoil( CoilNum ).InletAirHumRat );
			WaterCoil( CoilNum ).InletWaterMassFlowRate = WaterCoil( CoilNum ).MaxWaterMassFlowRate;
			WaterCoil( CoilNum ).InletAirMassFlowRate = StdRhoAir * WaterCoil( CoilNum ).DesAirVolFlowRate;
			CapacitanceAir = WaterCoil( CoilNum ).InletAirMassFlowRate * PsyCpAirFnWTdb( WaterCoil( CoilNum ).InletAirHumRat, WaterCoil( CoilNum ).InletAirTemp );

			Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, WaterCoil( CoilNum ).InletWaterTemp, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );

			CapacitanceWater = WaterCoil( CoilNum ).InletWaterMassFlowRate * Cp;
			CMin = min( CapacitanceAir, CapacitanceWater );
			if ( CMin > 0.0 ) {
				if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_Cooling ) {
					CoolingCoil( CoilNum, FirstHVACIteration, DesignCalc, ContFanCycCoil, 1.0 );
					CoilEffectiveness = ( WaterCoil( CoilNum ).InletAirTemp - WaterCoil( CoilNum ).OutletAirTemp ) / ( WaterCoil( CoilNum ).InletAirTemp - WaterCoil( CoilNum ).InletWaterTemp ) * ( CapacitanceAir / CMin );
					RatedLatentCapacity = WaterCoil( CoilNum ).TotWaterCoolingCoilRate - WaterCoil( CoilNum ).SenWaterCoolingCoilRate;
					RatedSHR = WaterCoil( CoilNum ).SenWaterCoolingCoilRate / WaterCoil( CoilNum ).TotWaterCoolingCoilRate;
				} else if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_DetFlatFinCooling ) {
					CalcDetailFlatFinCoolingCoil( CoilNum, DesignCalc, ContFanCycCoil, 1.0 );
					CoilEffectiveness = ( WaterCoil( CoilNum ).InletAirTemp - WaterCoil( CoilNum ).OutletAirTemp ) / ( WaterCoil( CoilNum ).InletAirTemp - WaterCoil( CoilNum ).InletWaterTemp ) * ( CapacitanceAir / CMin );
					RatedLatentCapacity = WaterCoil( CoilNum ).TotWaterCoolingCoilRate - WaterCoil( CoilNum ).SenWaterCoolingCoilRate;
					RatedSHR = WaterCoil( CoilNum ).SenWaterCoolingCoilRate / WaterCoil( CoilNum ).TotWaterCoolingCoilRate;
				} else if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) {
					CalcSimpleHeatingCoil( CoilNum, ContFanCycCoil, 1.0, DesignCalc );
					CoilEffectiveness = ( WaterCoil( CoilNum ).OutletAirTemp - WaterCoil( CoilNum ).InletAirTemp ) / ( WaterCoil( CoilNum ).InletWaterTemp - WaterCoil( CoilNum ).InletAirTemp ) * ( CapacitanceAir / CMin );
				}
			} else {
				CoilEffectiveness = 0.0;
				WaterCoil( CoilNum ).TotWaterHeatingCoilRate = 0.0;
				WaterCoil( CoilNum ).TotWaterCoolingCoilRate = 0.0;
				WaterCoil( CoilNum ).SenWaterCoolingCoilRate = 0.0;
				RatedLatentCapacity = 0.0;
				RatedSHR = 0.0;
			}
			MyEnvrnFlag( CoilNum ) = false;

		} // End If for the Begin Environment initializations

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( CoilNum ) = true;
		}

		if ( ! DoingSizing ) {
			if ( MyCoilReportFlag( CoilNum ) ) {
				//create predefined report entries
				MyCoilReportFlag( CoilNum ) = false;
				{ auto const SELECT_CASE_var( WaterCoil( CoilNum ).WaterCoilType_Num );
				if ( SELECT_CASE_var == WaterCoil_SimpleHeating ) {
					if ( RptCoilHeaderFlag( 1 ) ) {
						gio::write( OutputFileInits, fmtA ) << "! <Water Heating Coil Capacity Information>,Component Type,Name,Nominal Total Capacity {W}";
						RptCoilHeaderFlag( 1 ) = false;
					}
					PreDefTableEntry( pdchHeatCoilType, WaterCoil( CoilNum ).Name, "Coil:Heating:Water" );
					PreDefTableEntry( pdchHeatCoilDesCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).DesWaterHeatingCoilRate );
					PreDefTableEntry( pdchHeatCoilNomCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).TotWaterHeatingCoilRate );
					PreDefTableEntry( pdchHeatCoilNomEff, WaterCoil( CoilNum ).Name, "-" );
					addFootNoteSubTable( pdstHeatCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );
					gio::write( OutputFileInits, fmtA ) << "Water Heating Coil Capacity Information,Coil:Heating:Water," + WaterCoil( CoilNum ).Name + ',' + RoundSigDigits( WaterCoil( CoilNum ).TotWaterHeatingCoilRate, 2 );
				} else if ( SELECT_CASE_var == WaterCoil_DetFlatFinCooling ) {
					if ( RptCoilHeaderFlag( 2 ) ) {
						gio::write( OutputFileInits, fmtA ) << "! <Water Cooling Coil Capacity Information>,Component Type,Name,Nominal Total Capacity {W},Nominal Sensible Capacity {W},Nominal Latent Capacity {W},Nominal Sensible Heat Ratio";
						RptCoilHeaderFlag( 2 ) = false;
					}
					RatedLatentCapacity = WaterCoil( CoilNum ).TotWaterCoolingCoilRate - WaterCoil( CoilNum ).SenWaterCoolingCoilRate;
					RatedSHR = SafeDivide( WaterCoil( CoilNum ).SenWaterCoolingCoilRate, WaterCoil( CoilNum ).TotWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilType, WaterCoil( CoilNum ).Name, "Coil:Cooling:Water:DetailedGeometry" );
					PreDefTableEntry( pdchCoolCoilDesCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).DesWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilTotCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).TotWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilSensCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).SenWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilLatCap, WaterCoil( CoilNum ).Name, RatedLatentCapacity );
					PreDefTableEntry( pdchCoolCoilSHR, WaterCoil( CoilNum ).Name, RatedSHR );
					PreDefTableEntry( pdchCoolCoilNomEff, WaterCoil( CoilNum ).Name, "-" );
					addFootNoteSubTable( pdstCoolCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );
					gio::write( OutputFileInits, fmtA ) << "Water Cooling Coil Capacity Information,Coil:Cooling:Water:DetailedGeometry," + WaterCoil( CoilNum ).Name + ',' + RoundSigDigits( WaterCoil( CoilNum ).TotWaterCoolingCoilRate, 2 ) + ',' + RoundSigDigits( WaterCoil( CoilNum ).SenWaterCoolingCoilRate, 2 ) + ',' + RoundSigDigits( RatedLatentCapacity, 2 ) + ',' + RoundSigDigits( RatedSHR, 2 );
				} else if ( SELECT_CASE_var == WaterCoil_Cooling ) {
					if ( RptCoilHeaderFlag( 2 ) ) {
						gio::write( OutputFileInits, fmtA ) << "! <Water Cooling Coil Capacity Information>,Component Type,Name,Nominal Total Capacity {W},Nominal Sensible Capacity {W},Nominal Latent Capacity {W},Nominal Sensible Heat Ratio, Nominal Coil UA Value {W/C}, Nominal Coil Surface Area {m2}";
						RptCoilHeaderFlag( 2 ) = false;
					}
					RatedLatentCapacity = WaterCoil( CoilNum ).TotWaterCoolingCoilRate - WaterCoil( CoilNum ).SenWaterCoolingCoilRate;
					RatedSHR = SafeDivide( WaterCoil( CoilNum ).SenWaterCoolingCoilRate, WaterCoil( CoilNum ).TotWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilType, WaterCoil( CoilNum ).Name, "Coil:Cooling:Water" );
					PreDefTableEntry( pdchCoolCoilDesCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).DesWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilTotCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).TotWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilSensCap, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).SenWaterCoolingCoilRate );
					PreDefTableEntry( pdchCoolCoilLatCap, WaterCoil( CoilNum ).Name, RatedLatentCapacity );
					PreDefTableEntry( pdchCoolCoilSHR, WaterCoil( CoilNum ).Name, RatedSHR );
					PreDefTableEntry( pdchCoolCoilNomEff, WaterCoil( CoilNum ).Name, "-" );
					PreDefTableEntry( pdchCoolCoilUATotal, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).UACoilTotal );
					PreDefTableEntry( pdchCoolCoilArea, WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).TotCoilOutsideSurfArea );
					addFootNoteSubTable( pdstCoolCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );
					gio::write( OutputFileInits, fmtA ) << "Water Cooling Coil Capacity Information,Coil:Cooling:Water," + WaterCoil( CoilNum ).Name + ',' + RoundSigDigits( WaterCoil( CoilNum ).TotWaterCoolingCoilRate, 2 ) + ',' + RoundSigDigits( WaterCoil( CoilNum ).SenWaterCoolingCoilRate, 2 ) + ',' + RoundSigDigits( RatedLatentCapacity, 2 ) + ',' + RoundSigDigits( RatedSHR, 2 ) + ',' + RoundSigDigits( UATotal, 2 ) + ',' + RoundSigDigits( SurfaceArea, 2 );
				}}
				if ( WaterCoil( CoilNum ).DesWaterCoolingCoilRate <= 0.0 ) WaterCoil( CoilNum ).DesWaterCoolingCoilRate = WaterCoil( CoilNum ).TotWaterCoolingCoilRate;
				if ( WaterCoil( CoilNum ).DesWaterHeatingCoilRate <= 0.0 ) WaterCoil( CoilNum ).DesWaterHeatingCoilRate = WaterCoil( CoilNum ).TotWaterHeatingCoilRate;
			}
		}

		// Do the Begin Day initializations
		// NONE

		// Do the begin HVAC time step initializations
		// NONE

		// Do the following initializations (every time step): This should be the info from
		// the previous components outlets or the node data in this section.
		//First set the conditions for the air into the coil model
		AirInletNode = WaterCoil( CoilNum ).AirInletNodeNum;
		WaterInletNode = WaterCoil( CoilNum ).WaterInletNodeNum;
		WaterCoil( CoilNum ).InletAirMassFlowRate = Node( AirInletNode ).MassFlowRate;
		WaterCoil( CoilNum ).InletAirTemp = Node( AirInletNode ).Temp;
		WaterCoil( CoilNum ).InletAirHumRat = Node( AirInletNode ).HumRat;
		WaterCoil( CoilNum ).InletAirEnthalpy = Node( AirInletNode ).Enthalpy;

		WaterCoil( CoilNum ).InletWaterMassFlowRate = Node( WaterInletNode ).MassFlowRate;
		WaterCoil( CoilNum ).InletWaterTemp = Node( WaterInletNode ).Temp;
		WaterCoil( CoilNum ).InletWaterEnthalpy = Node( WaterInletNode ).Enthalpy;

		WaterCoil( CoilNum ).UACoilVariable = WaterCoil( CoilNum ).UACoil;

		// added for fouling coils
		WaterCoil( CoilNum ).FoulingFactor = 0.0;
		if ( ( ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) || ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_Cooling ) ) && ( ! ( MyUAAndFlowCalcFlag( CoilNum ) ) ) ) {

			for ( i = 1; i <= NumFouledCoil; ++i ) {
				if ( FouledCoils( i ).FouledCoilID == CoilNum ) {
					// Check faults availability and severity schedules
					rSchVal = 0.0;
					if ( GetCurrentScheduleValue( FouledCoils( i ).AvaiSchedPtr ) > 0.0 ) {
						rSchVal = 1.0;
						if ( FouledCoils( i ).SeveritySchedPtr > 0 ) {
							rSchVal = GetCurrentScheduleValue( FouledCoils( i ).SeveritySchedPtr );
						}
					}

					if ( FouledCoils( i ).FoulingInputMethod == iFouledCoil_UARated ) {
						if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) {
							WaterCoil( CoilNum ).FoulingFactor = rSchVal * ( 1.0 / FouledCoils( i ).UAFouled - 1.0 / WaterCoil( CoilNum ).UACoil );
						} else {
							WaterCoil( CoilNum ).FoulingFactor = rSchVal * ( 1.0 / FouledCoils( i ).UAFouled - 1.0 / WaterCoil( CoilNum ).UACoilTotal );
						}
					} else {
						WaterCoil( CoilNum ).FoulingFactor = rSchVal * ( FouledCoils( i ).Rfw / ( FouledCoils( i ).Aratio * FouledCoils( i ).Aout ) + FouledCoils( i ).Rfa / FouledCoils( i ).Aout );
					}

					if ( WaterCoil( CoilNum ).FoulingFactor < 0.0 ) WaterCoil( CoilNum ).FoulingFactor = 0.0;

					break;
				}
			}
		}

		if ( ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) && ( ! ( MyUAAndFlowCalcFlag( CoilNum ) ) ) ) { // update Coil UA based on inlet mass flows and temps
			x_a = 1.0 + 4.769E-3 * ( WaterCoil( CoilNum ).InletAirTemp - WaterCoil( CoilNum ).DesInletAirTemp );
			if ( WaterCoil( CoilNum ).DesAirMassFlowRate > 0.0 ) {
				AirConvectTerm = x_a * std::pow( WaterCoil( CoilNum ).InletAirMassFlowRate / WaterCoil( CoilNum ).DesAirMassFlowRate, 0.8 ) * WaterCoil( CoilNum ).AirSideNominalConvect;
			} else {
				AirConvectTerm = 0.0;
			}
			WaterConvSensitivity = 0.014 / ( 1.0 + 0.014 * WaterCoil( CoilNum ).DesInletWaterTemp );
			x_w = 1.0 + WaterConvSensitivity * ( WaterCoil( CoilNum ).InletWaterTemp - WaterCoil( CoilNum ).DesInletWaterTemp );
			if ( WaterCoil( CoilNum ).MaxWaterMassFlowRate > 0.0 ) {
				WaterConvectTerm = x_w * std::pow( WaterCoil( CoilNum ).InletWaterMassFlowRate / WaterCoil( CoilNum ).MaxWaterMassFlowRate, 0.85 ) * WaterCoil( CoilNum ).LiquidSideNominalConvect;
			} else {
				WaterConvectTerm = 0.0;
			}
			if ( ( AirConvectTerm > 0.0 ) && ( WaterConvectTerm > 0.0 ) ) {
				WaterCoil( CoilNum ).UACoilVariable = 1.0 / ( ( 1.0 / WaterConvectTerm ) + WaterCoil( CoilNum ).FoulingFactor + ( 1.0 / AirConvectTerm ) );
			} else {
				// use nominal UA since variable UA cannot be calculated
				WaterCoil( CoilNum ).UACoilVariable = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoil + WaterCoil( CoilNum ).FoulingFactor );
			}
		}

		// update Coil UA based on inlet mass flows and temps
		if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_Cooling && ( ! MyCoilDesignFlag( CoilNum ) ) ) {
			x_a = 1.0 + 4.769E-3 * ( WaterCoil( CoilNum ).InletAirTemp - WaterCoil( CoilNum ).DesInletAirTemp );
			if ( WaterCoil( CoilNum ).DesAirMassFlowRate > 0.0 ) {
				WaterCoil( CoilNum ).UACoilExternal = x_a * std::pow( WaterCoil( CoilNum ).InletAirMassFlowRate / WaterCoil( CoilNum ).DesAirMassFlowRate, 0.8 ) * WaterCoil( CoilNum ).UACoilExternalDes;
			} else {
				WaterCoil( CoilNum ).UACoilExternal = WaterCoil( CoilNum ).UACoilExternalDes;
			}
			WaterConvSensitivity = 0.014 / ( 1.0 + 0.014 * WaterCoil( CoilNum ).DesInletWaterTemp );
			x_w = 1.0 + WaterConvSensitivity * ( WaterCoil( CoilNum ).InletWaterTemp - WaterCoil( CoilNum ).DesInletWaterTemp );
			if ( WaterCoil( CoilNum ).MaxWaterMassFlowRate > 0.0 ) {
				WaterCoil( CoilNum ).UACoilInternal = x_w * std::pow( WaterCoil( CoilNum ).InletWaterMassFlowRate / WaterCoil( CoilNum ).MaxWaterMassFlowRate, 0.85 ) * WaterCoil( CoilNum ).UACoilInternalDes;
			} else {
				WaterCoil( CoilNum ).UACoilInternal = WaterCoil( CoilNum ).UACoilInternalDes;
			}
			if ( WaterCoil( CoilNum ).UACoilInternal > 0.0 && WaterCoil( CoilNum ).UACoilExternal > 0.0 ) {
				WaterCoil( CoilNum ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoilExternal + WaterCoil( CoilNum ).FoulingFactor + 1.0 / WaterCoil( CoilNum ).UACoilInternal );
			} else {
				WaterCoil( CoilNum ).UACoilInternal = WaterCoil( CoilNum ).UACoilInternalDes;
				WaterCoil( CoilNum ).UACoilExternal = WaterCoil( CoilNum ).UACoilExternalDes;
				WaterCoil( CoilNum ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoilExternal + WaterCoil( CoilNum ).FoulingFactor + 1.0 / WaterCoil( CoilNum ).UACoilInternal );
			}
			WaterCoil( CoilNum ).UACoilInternalPerUnitArea = WaterCoil( CoilNum ).UACoilInternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
			WaterCoil( CoilNum ).UAWetExtPerUnitArea = WaterCoil( CoilNum ).UACoilExternal / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
			WaterCoil( CoilNum ).UADryExtPerUnitArea = WaterCoil( CoilNum ).UAWetExtPerUnitArea;
		}

		WaterCoil( CoilNum ).TotWaterHeatingCoilRate = 0.0;
		WaterCoil( CoilNum ).TotWaterCoolingCoilRate = 0.0;
		WaterCoil( CoilNum ).SenWaterCoolingCoilRate = 0.0;

	}

	void
	SizeWaterCoil( int const CoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   November 2001
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Water Coil Components for which flow rates and UAs have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays and plant sizing data. UAs are
		// calculated by numerically inverting the individual coil calculation routines.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using General::SolveRegulaFalsi;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using DataEnvironment::StdBaroPress;
		using DataAirSystems::PrimaryAirSystem;
		//  USE BranchInputManager, ONLY: MyPlantSizingIndex
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::GetCoilDesFlowT;
		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const InitWaterCoil( "InitWaterCoil" );
		static std::string const RoutineName( "SizeWaterCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNum; // do loop index for plant sizing
		int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		bool ErrorsFound; // If errors detected in input
		bool LoopErrorsFound;
		Real64 rho;
		int FieldNum = 2; // IDD numeric field number where input field description is found
		std::string CompName; // component name
		std::string	CompType; // component type
		int SizingType; // type of sizing to perform
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		bool bPRINT = true; // TRUE if sizing is reported to output (eio)
		Real64 TempSize; // autosized value
		Real64 CpAirStd; // specific heat of air at standard conditions
		Real64 DesCoilAirFlow; // design air flow rate for the coil [m3/s]
		Real64 DesCoilExitTemp; // design coil exit temperature [C]

		ErrorsFound = false;
		PltSizCoolNum = 0;
		PltSizHeatNum = 0;
		PltSizNum = 0;
		DesCoilAirFlow = 0.0;
		DesCoilExitTemp = 0.0;
		LoopErrorsFound = false;
		CpAirStd = PsyCpAirFnWTdb( 0.0, 20.0 );

		// cooling coils
		if ( WaterCoil( CoilNum ).WaterCoilType == CoilType_Cooling && WaterCoil( CoilNum ).RequestingAutoSize ) {
			// find the appropriate Plant Sizing object
			PltSizCoolNum = MyPlantSizingIndex( "chilled water coil", WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).WaterInletNodeNum, WaterCoil( CoilNum ).WaterOutletNodeNum, LoopErrorsFound );
		}

		if ( WaterCoil( CoilNum ).WaterCoilType == CoilType_Cooling ) { // 'Cooling'
			if ( PltSizCoolNum > 0 ) {

				DataPltSizCoolNum = PltSizCoolNum;
				DataWaterLoopNum = WaterCoil ( CoilNum ).WaterLoopNum;

				if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
					CompType = cAllCoilTypes(Coil_CoolingWaterDetailed); // Coil:Cooling:Water:DetailedGeometry
				} else {
					CompType = cAllCoilTypes(Coil_CoolingWater); // Coil:Cooling:Water
				}
				bPRINT = false; // do not print this sizing request since the autosized value is needed and this input may not be autosized (we should print this!)
				TempSize = AutoSize; // get the autosized air volume flow rate for use in other calculations
				SizingString.clear(); // doesn't matter
				CompName = WaterCoil( CoilNum ).Name;
				RequestSizing( CompType, CompName, CoolingAirflowSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).InletAirMassFlowRate = StdRhoAir * TempSize; // inlet air mass flow rate is the autosized value
				DataAirFlowUsedForSizing = TempSize; // many autosized inputs use the design (autosized) air volume flow rate, save this value
				DataFlowUsedForSizing = TempSize;

				TempSize = AutoSize;
				bPRINT = true;
				if ( WaterCoil ( CoilNum ).MaxWaterVolFlowRate != AutoSize ) bPRINT = false;
				if ( CurSysNum == 0 ) bPRINT = false;
				if ( CurSysNum > 0 && CurOASysNum == 0 ) {
					GetCoilDesFlowT( CurSysNum, CpAirStd, DesCoilAirFlow, DesCoilExitTemp );
					DataAirFlowUsedForSizing = DesCoilAirFlow;
					DataFlowUsedForSizing = DesCoilAirFlow;
					DataDesOutletAirTemp = DesCoilExitTemp;
					DataDesOutletAirHumRat = PsyWFnTdbRhPb( DataDesOutletAirTemp, 0.9, StdBaroPress, RoutineName );
				}

				SizingString = "Design Coil Load [W]"; // there is no input field for this value and this is not the rated capacity (we should always print this!)
				RequestSizing( CompType, CompName, CoolingCapacitySizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil( CoilNum ).DesWaterCoolingCoilRate = TempSize;
				WaterCoil( CoilNum ).InletAirMassFlowRate = StdRhoAir * DataFlowUsedForSizing; // inlet air mass flow rate is the autosized value
				DataCapacityUsedForSizing = WaterCoil( CoilNum ).DesWaterCoolingCoilRate;

				// Why isn't the water volume flow rate based on the user inputs for inlet/outlet air/water temps? Water volume flow rate is always based on autosized inputs.
				bPRINT = true;
				FieldNum = 1; //  CoilModel_Detailed: N1 , \field Maximum Water Flow Rate, else: N1 , \field Design Water Flow Rate
				SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = WaterCoil ( CoilNum ).MaxWaterVolFlowRate;
				RequestSizing( CompType, CompName, CoolingWaterflowSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).MaxWaterVolFlowRate = TempSize;
				DataWaterFlowUsedForSizing =  TempSize;

				if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
					FieldNum = 1;
					bPRINT = false; // do not print this sizing request since this coil does not have a design air flow rate input field (we should print this!)
				} else {
					FieldNum = 2; //  N2 , \field Design Air Flow Rate
					bPRINT = true;
				}
				SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = WaterCoil ( CoilNum ).DesAirVolFlowRate;
				RequestSizing( CompType, CompName, CoolingAirflowSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).DesAirVolFlowRate = TempSize;
				if ( WaterCoil( CoilNum ).DesAirVolFlowRate <= 0.0 ) {
					WaterCoil( CoilNum ).DesAirVolFlowRate = 0.0;
					ShowWarningError( "The design air flow rate is zero for Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
					ShowContinueError( "The autosize value for max air volume flow rate is zero" );
				}

				if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
					FieldNum = 1;
					bPRINT = false; // do not print this sizing request since this coil does not have a design inlet air temp input field (we should print this!)
				} else {
					FieldNum = 4; //  N4 , \field Design Inlet Air Temperature
					bPRINT = true;
				}
				SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [C]";
				DataFlowUsedForSizing = DataAirFlowUsedForSizing; // used by air loop coils
				TempSize = WaterCoil ( CoilNum ).DesInletAirTemp;
				RequestSizing( CompType, CompName, CoolingWaterDesAirInletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).DesInletAirTemp = TempSize;
				DataDesInletAirTemp = TempSize;

				if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
					FieldNum = 1; // do not print this sizing request since this coil does not have a design inlet water temp input field (we should print this!)
					bPRINT = false; // no field for detailed water coil
				} else {
					FieldNum = 3; //  N3 , \field Design Inlet Water Temperature
					bPRINT = true;
				}
				SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [C]";
				DataFlowUsedForSizing = DataAirFlowUsedForSizing;
				TempSize = WaterCoil ( CoilNum ).DesInletWaterTemp;
				RequestSizing( CompType, CompName, CoolingWaterDesWaterInletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).DesInletWaterTemp = TempSize;

				if ( CurZoneEqNum > 0 ) { // zone equipment use air inlet humrat to calculate design outlet air temperature
					if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
						FieldNum = 1; // do not print this sizing request since this coil does not have a design inlet air humrat input field (we should print this!)
						bPRINT = false; // no field for detailed water coil
					} else {
						FieldNum = 6; //  N6 , \field Design Inlet Air Humidity Ratio
						bPRINT = true;
					}
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ); // + " [kgWater/kgDryAir]";
					DataFlowUsedForSizing = DataAirFlowUsedForSizing;
					TempSize = WaterCoil ( CoilNum ).DesInletAirHumRat;
					RequestSizing( CompType, CompName, CoolingWaterDesAirInletHumRatSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).DesInletAirHumRat = TempSize;
				}

				if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
					FieldNum = 1; // do not print this sizing request since this coil does not have a design outlet air temp input field (we should print this!)
					bPRINT = false; // no field for detailed water coil
				} else {
					FieldNum = 5; //  N5 , \field Design Outlet Air Temperature
					bPRINT = true;
				}

				SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [C]";
				DataDesInletWaterTemp = WaterCoil( CoilNum ).DesInletWaterTemp; // used for warning messages
				DataDesInletAirHumRat = WaterCoil ( CoilNum ).DesInletAirHumRat;
				TempSize = WaterCoil ( CoilNum ).DesOutletAirTemp;
				RequestSizing( CompType, CompName, CoolingWaterDesAirOutletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).DesOutletAirTemp = TempSize;
				DataDesOutletAirTemp = TempSize;

				if ( CurSysNum > 0 ) { // This call can be deleted at a future time and remove the if ( CurZoneEqNum > 0 ) check above. This will change the order of the eio file.
					if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
						FieldNum = 1; // do not print this sizing request since this coil does not have a design inlet air humrat input field (we should print this!)
						bPRINT = false; // no field for detailed water coil
					} else {
						FieldNum = 6; //  N6 , \field Design Inlet Air Humidity Ratio
						bPRINT = true;
					}
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ); // + " [kgWater/kgDryAir]";
					DataFlowUsedForSizing = DataAirFlowUsedForSizing;
					TempSize = WaterCoil ( CoilNum ).DesInletAirHumRat;
					RequestSizing( CompType, CompName, CoolingWaterDesAirInletHumRatSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).DesInletAirHumRat = TempSize;
				}

				if ( WaterCoil( CoilNum ).WaterCoilModel == CoilModel_Detailed ) { // 'DETAILED FLAT FIN'
					FieldNum = 1; // do not print this sizing request since this coil does not have a design outlet air humrat input field (we should print this!)
					bPRINT = false; // no field for detailed water coil
				} else {
					FieldNum = 7; //  N7 , \field Design Outlet Air Humidity Ratio
					bPRINT = true;
				}
				SizingString = WaterCoilNumericFields ( CoilNum ).FieldNames ( FieldNum ); // + " [kgWater/kgDryAir]";
				DataCapacityUsedForSizing = WaterCoil ( CoilNum ).DesWaterCoolingCoilRate; // used for warning messages
				DataDesInletAirTemp = WaterCoil( CoilNum ).DesOutletAirTemp;
				DataDesInletAirHumRat = WaterCoil ( CoilNum ).DesInletAirHumRat;
				DataDesInletWaterTemp = WaterCoil( CoilNum ).DesInletWaterTemp;
				TempSize = WaterCoil( CoilNum ).DesOutletAirHumRat;
				RequestSizing( CompType, CompName, CoolingWaterDesAirOutletHumRatSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).DesOutletAirHumRat = TempSize;

				if ( WaterCoil ( CoilNum ).WaterCoilModel == CoilModel_Detailed ) {

					FieldNum = 16; //  N16, \field Number of Tubes per Row
					bPRINT = true;
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum );
					// Auto size detailed cooling coil number of tubes per row = int( 13750.0 * WaterCoil( CoilNum ).MaxWaterVolFlowRate ) + 1
					DataFlowUsedForSizing = WaterCoil( CoilNum ).MaxWaterVolFlowRate;
					TempSize = float(WaterCoil( CoilNum ).NumOfTubesPerRow);
					RequestSizing( CompType, CompName, CoolingWaterNumofTubesPerRowSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).NumOfTubesPerRow = int ( TempSize );

					FieldNum = 7; //  N7 , \field Fin Diameter
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m]";
					// Auto size water coil fin diameter = 0.335 * WaterCoil( CoilNum ).InletAirMassFlowRate
					DataConstantUsedForSizing = WaterCoil ( CoilNum ).InletAirMassFlowRate;
					DataFractionUsedForSizing = 0.335;
					TempSize = WaterCoil( CoilNum ).FinDiam;
					RequestSizing( CompType, CompName, AutoCalculateSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).FinDiam = TempSize;

					FieldNum = 5; //  N5 , \field Minimum Airflow Area
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m2]";
					// Auto size water coil minimum airflow area = 0.44 * WaterCoil( CoilNum ).InletAirMassFlowRate
					DataConstantUsedForSizing = WaterCoil( CoilNum ).InletAirMassFlowRate;
					DataFractionUsedForSizing = 0.44;
					TempSize = WaterCoil( CoilNum ).MinAirFlowArea;
					RequestSizing( CompType, CompName, AutoCalculateSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).MinAirFlowArea = TempSize;
					if ( WaterCoil( CoilNum ).MinAirFlowArea <= 0.0 ) {
						ShowSevereError( "Coil:Cooling:Water:DetailedGeometry: \"" + WaterCoil( CoilNum ).Name + "\"" );
						ShowContinueError( "Coil Minimum Airflow Area must be greater than 0. Coil area = " + TrimSigDigits( WaterCoil( CoilNum ).MinAirFlowArea, 6 ) );
						ErrorsFound = true;
					}

					FieldNum = 4; //  N4 , \field Fin Surface Area
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m2]";
					// Auto size water coil finned surface area = 78.5 * WaterCoil( CoilNum ).InletAirMassFlowRate
					DataConstantUsedForSizing = WaterCoil( CoilNum ).InletAirMassFlowRate; // actual autosized air mass flow rate, not calculated from user input
					DataFractionUsedForSizing = 78.5;
					TempSize = WaterCoil( CoilNum ).FinSurfArea;
					RequestSizing( CompType, CompName, AutoCalculateSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).FinSurfArea = TempSize;

					FieldNum = 3; //  N3 , \field Total Tube Inside Area
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m2]";
					// Auto size water coil total tube inside surface area = 4.4 * WaterCoil( CoilNum ).TubeInsideDiam * WaterCoil( CoilNum ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow
					DataConstantUsedForSizing = WaterCoil( CoilNum ).TubeInsideDiam * WaterCoil( CoilNum ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow;
					DataFractionUsedForSizing = 4.4;
					TempSize = WaterCoil( CoilNum ).TotTubeInsideArea;
					RequestSizing( CompType, CompName, AutoCalculateSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).TotTubeInsideArea = TempSize;

					FieldNum = 2; //  N2 , \field Tube Outside Surface Area
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m2]";
					// Auto size water coil total tube outside surface area = 4.1 * WaterCoil( CoilNum ).TubeOutsideDiam * WaterCoil( CoilNum ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow
					DataConstantUsedForSizing = WaterCoil( CoilNum ).TubeOutsideDiam * WaterCoil( CoilNum ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow;
					DataFractionUsedForSizing = 4.1;
					TempSize = WaterCoil( CoilNum ).TubeOutsideSurfArea;
					RequestSizing( CompType, CompName, AutoCalculateSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).TubeOutsideSurfArea = TempSize;
					if ( ( WaterCoil( CoilNum ).FinSurfArea + WaterCoil( CoilNum ).TubeOutsideSurfArea ) <= 0.0 ) {
						ShowSevereError( "Coil:Cooling:Water:DetailedGeometry: \"" + WaterCoil( CoilNum ).Name + "\"" );
						ShowContinueError( "Coil Fin Surface Area plus Coil Tube Outside Surface Area must be greater than 0. Total surface area = " + TrimSigDigits( ( WaterCoil( CoilNum ).FinSurfArea + WaterCoil( CoilNum ).TubeOutsideSurfArea ), 6 ) );
						ErrorsFound = true;
					}

					FieldNum = 6; //  N6 , \field Coil Depth
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m]";
					// Auto size water coil coil depth = WaterCoil( CoilNum ).TubeDepthSpacing * WaterCoil( CoilNum ).NumOfTubeRows
					DataConstantUsedForSizing = WaterCoil( CoilNum ).TubeDepthSpacing;
					DataFractionUsedForSizing = WaterCoil( CoilNum ).NumOfTubeRows;
					TempSize = WaterCoil( CoilNum ).CoilDepth;
					RequestSizing( CompType, CompName, AutoCalculateSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).CoilDepth = TempSize;
				}
				DataPltSizCoolNum = 0; // reset all globals to 0 to ensure correct sizing for other child components
				DataWaterLoopNum = 0;
				DataConstantUsedForSizing = 0.0;
				DataFractionUsedForSizing = 0.0;
				DataAirFlowUsedForSizing = 0.0;
				DataFlowUsedForSizing = 0.0;
				DataWaterFlowUsedForSizing = 0.0;
				DataCapacityUsedForSizing = 0.0;
				DataDesInletAirTemp = 0.0;
				DataDesOutletAirTemp = 0.0;
				DataDesOutletAirHumRat = 0.0;
				DataDesInletAirHumRat = 0.0;
				DataDesInletWaterTemp = 0.0;

			} else {
				// If there is no cooling Plant Sizing object and autosizing was requested, issue fatal error message
				if ( WaterCoil( CoilNum ).RequestingAutoSize ) {
					ShowSevereError( "Autosizing of water coil requires a cooling loop Sizing:Plant object" );
					ShowContinueError( "Occurs in water coil object= " + WaterCoil( CoilNum ).Name );
					ErrorsFound = true;
				}
			} // end of cooling Plant Sizing existence IF - ELSE
		} // end cooling coil IF

		// if this is a heating coil
		if ( WaterCoil( CoilNum ).WaterCoilType == CoilType_Heating && WaterCoil( CoilNum ).RequestingAutoSize ) {
			// find the appropriate heating Plant Sizing object
			PltSizHeatNum = MyPlantSizingIndex( "hot water coil", WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).WaterInletNodeNum, WaterCoil( CoilNum ).WaterOutletNodeNum, LoopErrorsFound );
		}

		if ( WaterCoil( CoilNum ).WaterCoilType == CoilType_Heating ) { // 'Heating'
			if ( WaterCoil( CoilNum ).RequestingAutoSize ) {
				// find the appropriate heating Plant Sizing object
				PltSizHeatNum = MyPlantSizingIndex( "hot water coil", WaterCoil( CoilNum ).Name, WaterCoil( CoilNum ).WaterInletNodeNum, WaterCoil( CoilNum ).WaterOutletNodeNum, LoopErrorsFound );
			}
			if ( PltSizHeatNum > 0 ) {

				DataPltSizHeatNum = PltSizHeatNum;
				DataWaterLoopNum = WaterCoil ( CoilNum ).WaterLoopNum;

				bPRINT = false; // do not print this sizing request
				TempSize = AutoSize; // get the autosized air volume flow rate for use in other calculations
				SizingString.clear(); // doesn't matter
				CompType = cAllCoilTypes(Coil_HeatingWater); // "Coil:Heating:Water"
				CompName = WaterCoil( CoilNum ).Name;
				if ( WaterCoil( CoilNum ).DesiccantRegenerationCoil ) {
					DataDesicRegCoil = true;
					DataDesicDehumNum = WaterCoil( CoilNum ).DesiccantDehumNum;
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
					TempSize = AutoSize; // reset back
				}
				RequestSizing( CompType, CompName, HeatingAirflowSizing, SizingString, TempSize, bPRINT, RoutineName );
				// reset the design air volume flow rate for air loop coils only
				if ( CurSysNum > 0 ) WaterCoil( CoilNum ).DesAirVolFlowRate = TempSize;
				WaterCoil ( CoilNum ).InletAirMassFlowRate = StdRhoAir * TempSize; // inlet air mass flow rate is not the autosized value
				DataAirFlowUsedForSizing = TempSize;
				DataFlowUsedForSizing = TempSize; // many autosized inputs use the design (autosized) air flow rate, save this value

				bPRINT = true;
				TempSize = AutoSize;
				SizingString = "Design Coil Load [W]";
				if ( CurSysNum > 0 ) {
					SizingType = HeatingCapacitySizing;
				} else {
					SizingType = WaterHeatingCapacitySizing;
				}
				RequestSizing( CompType, CompName, SizingType, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil( CoilNum ).DesWaterHeatingCoilRate = TempSize;
				if ( ( WaterCoil( CoilNum ).CoilPerfInpMeth == UAandFlow && WaterCoil( CoilNum ).UACoil == AutoSize ) || ( WaterCoil( CoilNum ).CoilPerfInpMeth == NomCap && WaterCoil( CoilNum ).DesTotWaterCoilLoad == AutoSize ) ) {
					// WaterCoil ( CoilNum ).DesTotWaterCoilLoad will be set below in UA calc
				} else {
					if (WaterCoil ( CoilNum ).DesTotWaterCoilLoad == AutoSize ) WaterCoil ( CoilNum ).DesTotWaterCoilLoad = TempSize;
				}

				DataCapacityUsedForSizing = WaterCoil( CoilNum ).DesWaterHeatingCoilRate;
				FieldNum = 2; // N2 , \field Maximum Water Flow Rate
				SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = WaterCoil ( CoilNum ).MaxWaterVolFlowRate;
				RequestSizing( CompType, CompName, HeatingWaterflowSizing, SizingString, TempSize, bPRINT, RoutineName );
				WaterCoil ( CoilNum ).MaxWaterVolFlowRate = TempSize;
				DataWaterFlowUsedForSizing = WaterCoil ( CoilNum ).MaxWaterVolFlowRate;
				if ( WaterCoil ( CoilNum ).MaxWaterVolFlowRate <= 0.0 ) {
//					MaxWaterVolFlowRateDes = 0.0;
					ShowWarningError( "The design coil load is zero for Coil:Heating:Water " + WaterCoil( CoilNum ).Name );
					ShowContinueError( "The autosize value for maximum water flow rate is zero" );
					ShowContinueError( "To change this, input a value for UA, change the heating design day, or raise the" );
					ShowContinueError( "  system heating design supply air temperature. Also check to make sure the Preheat" );
					ShowContinueError( "  Design Temperature is not the same as the Central Heating Design Supply Air Temperature. " );
				}

				// initialize the water coil inlet conditions
				bPRINT = false; // no need to print to eio since we only need the values
				DataFlowUsedForSizing = DataAirFlowUsedForSizing;

				if ( WaterCoil( CoilNum ).DesiccantRegenerationCoil ) {
					TempSize = AutoSize; // these data are initially 0, set to autosize to receive a result from RequestSizing
					RequestSizing( CompType, CompName, HeatingCoilDesAirInletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil( CoilNum ).InletAirTemp = TempSize;
					TempSize = AutoSize; // these data are initially 0, set to autosize to receive a result from RequestSizing
					RequestSizing( CompType, CompName, HeatingCoilDesAirInletHumRatSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil( CoilNum ).InletAirHumRat = TempSize;
					TempSize = AutoSize; // these data are initially 0, set to autosize to receive a result from RequestSizing
					RequestSizing( CompType, CompName, HeatingAirflowUASizing, SizingString, TempSize, bPRINT, RoutineName );
					//WaterCoil( CoilNum ).InletAirMassFlowRate = TempSize;
					WaterCoil( CoilNum ).InletAirMassFlowRate = DataAirFlowUsedForSizing;
				} else {
					TempSize = AutoSize; // these data are initially 0, set to autosize to receive a result from RequestSizing
					RequestSizing( CompType, CompName, HeatingWaterDesAirInletTempSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil( CoilNum ).InletAirTemp = TempSize;
					TempSize = AutoSize; // these data are initially 0, set to autosize to receive a result from RequestSizing
					RequestSizing( CompType, CompName, HeatingWaterDesAirInletHumRatSizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil( CoilNum ).InletAirHumRat = TempSize;
					TempSize = AutoSize; // these data are initially 0, set to autosize to receive a result from RequestSizing
					RequestSizing( CompType, CompName, HeatingAirflowUASizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil( CoilNum ).InletAirMassFlowRate = TempSize;
				}

				// zone and air loop coils use different design coil load calculations, air loop coils use air side capacity, zone coils use water side capacity
				DataDesInletAirTemp = WaterCoil ( CoilNum ).InletAirTemp;
				DataDesInletAirHumRat = WaterCoil ( CoilNum ).InletAirHumRat;
				DataFlowUsedForSizing = DataAirFlowUsedForSizing * StdRhoAir;
				WaterCoil( CoilNum ).MaxWaterVolFlowRate = DataWaterFlowUsedForSizing;
				TempSize = AutoSize;
				RequestSizing( CompType, CompName, HeatingWaterDesCoilLoadUsedForUASizing, SizingString, TempSize, bPRINT, RoutineName );
				DataCapacityUsedForSizing = TempSize;
				TempSize = AutoSize; // get the water volume flow rate used to size UA
				RequestSizing( CompType, CompName, HeatingWaterDesCoilWaterVolFlowUsedForUASizing, SizingString, TempSize, bPRINT, RoutineName );
				DataWaterFlowUsedForSizing = TempSize;
				WaterCoil( CoilNum ).InletWaterTemp = PlantSizData( PltSizHeatNum ).ExitTemp;
				rho = GetDensityGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );
				WaterCoil( CoilNum ).InletWaterMassFlowRate = rho * DataWaterFlowUsedForSizing;
				WaterCoil( CoilNum ).DesWaterHeatingCoilRate = DataCapacityUsedForSizing;

				if ( ( WaterCoil( CoilNum ).CoilPerfInpMeth == UAandFlow && WaterCoil( CoilNum ).UACoil == AutoSize ) || ( WaterCoil( CoilNum ).CoilPerfInpMeth == NomCap && WaterCoil( CoilNum ).DesTotWaterCoilLoad == AutoSize ) ) {

					// calculate UA
					if ( CurSysNum > 0 ) WaterCoil( CoilNum ).DesTotWaterCoilLoad = DataCapacityUsedForSizing;
					FieldNum = 1; // N1 , \field U-Factor Times Area Value
					bPRINT = true; // report to eio the UA value
					SizingString = WaterCoilNumericFields( CoilNum ).FieldNames( FieldNum ) + " [W/K]";
					DataCoilNum = CoilNum;
					DataFanOpMode = ContFanCycCoil;
					TempSize = WaterCoil ( CoilNum ).UACoil;
					RequestSizing( CompType, CompName, WaterHeatingCoilUASizing, SizingString, TempSize, bPRINT, RoutineName );
					WaterCoil ( CoilNum ).UACoil = TempSize;
					WaterCoil( CoilNum ).DesWaterHeatingCoilRate = DataCapacityUsedForSizing;
				}
				DataWaterLoopNum = 0; // reset all globals to 0 to ensure correct sizing for other child components
				DataPltSizHeatNum = 0;
				DataCoilNum = 0;
				DataFanOpMode = 0;
				DataCapacityUsedForSizing = 0.0;
				DataWaterFlowUsedForSizing = 0.0;
				DataDesInletAirTemp = 0.0;
				DataDesInletAirHumRat = 0.0;
				DataAirFlowUsedForSizing = 0.0;
				DataFlowUsedForSizing = 0.0;
				DataDesicDehumNum = 0;
				DataDesicRegCoil = false;

			} else {
				// if there is no heating Plant Sizing object and autosizng was requested, issue an error message
				if ( WaterCoil( CoilNum ).RequestingAutoSize ) {
					ShowSevereError( "Autosizing of water coil requires a heating loop Sizing:Plant object" );
					ShowContinueError( "Occurs in water coil object= " + WaterCoil( CoilNum ).Name );
					ErrorsFound = true;
				}
			} // end of heating Plant Sizing existence IF - ELSE
		} // end heating coil IF

		// save the design water volumetric flow rate for use by the water loop sizing algorithms
		if ( WaterCoil( CoilNum ).MaxWaterVolFlowRate > 0.0 ) {
			RegisterPlantCompDesignFlow( WaterCoil( CoilNum ).WaterInletNodeNum, WaterCoil( CoilNum ).MaxWaterVolFlowRate );
		}

		if ( ErrorsFound || DataErrorsFound) {
			ShowFatalError( "Preceding water coil sizing errors cause program termination" );
		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcSimpleHeatingCoil(
		int const CoilNum, // index to heating coil
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio, // part-load ratio of heating coil
		int const CalcMode // 1 = design calc; 2 = simulation calculation
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rich Liesen
		//       DATE WRITTEN
		//       MODIFIED       Aug. 2007 - R. Raustad, added fan operating mode and part-load ratio to
		//                                  calculate the outlet conditions when fan and coil cycle.
		//                                  Air and water outlet temperature are full output with average
		//                                  air and water mass flow rate when fan and coil cycle.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a simple NTU effectiveness model heating coil

		// METHODOLOGY EMPLOYED:
		// (1) outlet conditions are calculated from the effectiveness and the inlet conditions.
		// (2) Effectiveness is calculated from the NTU formula for a cross flow heat exchanger
		//     with both streams unmixed.
		// Note: UA is input by user and is fixed.

		// REFERENCES:
		// See for instance ASHRAE HVAC 2 Toolkit, page 4-4, formula (4-7)

		// Using/Aliasing
		using DataBranchAirLoopPlant::MassFlowTolerance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSimpleHeatingCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 WaterMassFlowRate;
		Real64 AirMassFlow; // [kg/sec]
		Real64 TempAirIn; // [C]
		Real64 TempAirOut; // [C]
		Real64 Win;
		Real64 TempWaterIn;
		Real64 TempWaterOut;
		Real64 UA;
		Real64 CapacitanceAir;
		Real64 CapacitanceWater;
		Real64 CapacitanceMin;
		Real64 CapacitanceMax;
		Real64 HeatingCoilLoad;
		Real64 NTU;
		Real64 ETA;
		Real64 A;
		Real64 CapRatio;
		Real64 E1;
		Real64 E2;
		Real64 Effec;
		Real64 Cp;
		int Control;

		UA = WaterCoil( CoilNum ).UACoilVariable;
		TempAirIn = WaterCoil( CoilNum ).InletAirTemp;
		Win = WaterCoil( CoilNum ).InletAirHumRat;
		Control = WaterCoil( CoilNum ).Control;
		TempWaterIn = WaterCoil( CoilNum ).InletWaterTemp;

		// adjust mass flow rates for cycling fan cycling coil operation
		if ( FanOpMode == CycFanCycCoil ) {
			if ( PartLoadRatio > 0.0 ) {
				AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate / PartLoadRatio;
				WaterMassFlowRate = min( WaterCoil( CoilNum ).InletWaterMassFlowRate / PartLoadRatio, WaterCoil( CoilNum ).MaxWaterMassFlowRate );
			} else {
				AirMassFlow = 0.0;
				WaterMassFlowRate = 0.0;
			}
		} else {
			AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate;
			WaterMassFlowRate = WaterCoil( CoilNum ).InletWaterMassFlowRate;
		}

		if ( WaterMassFlowRate > MassFlowTolerance ) { // If the coil is operating
			CapacitanceAir = PsyCpAirFnWTdb( Win, 0.5 * ( TempAirIn + TempWaterIn ) ) * AirMassFlow;
			Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, TempWaterIn, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );
			CapacitanceWater = Cp * WaterMassFlowRate;
			CapacitanceMin = min( CapacitanceAir, CapacitanceWater );
			CapacitanceMax = max( CapacitanceAir, CapacitanceWater );
		} else {
			CapacitanceAir = 0.0;
			CapacitanceWater = 0.0;
		}

		// If the coil is operating there should be some heating capacitance
		//  across the coil, so do the simulation. If not set outlet to inlet and no load.
		//  Also the coil has to be scheduled to be available
		if ( ( ( CapacitanceAir > 0.0 ) && ( CapacitanceWater > 0.0 ) ) && ( CalcMode == DesignCalc || MySizeFlag( CoilNum ) || MyUAAndFlowCalcFlag( CoilNum ) || GetCurrentScheduleValue( WaterCoil( CoilNum ).SchedPtr ) > 0.0 ) ) {

			if ( UA <= 0.0 ) {
				ShowFatalError( "UA is zero for COIL:Heating:Water " + WaterCoil( CoilNum ).Name );
			}
			NTU = UA / CapacitanceMin;
			ETA = std::pow( NTU, 0.22 );
			CapRatio = CapacitanceMin / CapacitanceMax;
			A = CapRatio * NTU / ETA;

			if ( A > 20.0 ) {
				A = ETA * 1.0 / CapRatio;
			} else {
				E1 = std::exp( -A );
				A = ETA * ( 1.0 - E1 ) / CapRatio;
			}

			if ( A > 20.0 ) {
				Effec = 1.0;
			} else {
				E2 = std::exp( -A );
				Effec = 1.0 - E2;
			}

			TempAirOut = TempAirIn + Effec * CapacitanceMin * ( TempWaterIn - TempAirIn ) / CapacitanceAir;
			TempWaterOut = TempWaterIn - CapacitanceAir * ( TempAirOut - TempAirIn ) / CapacitanceWater;
			HeatingCoilLoad = CapacitanceWater * ( TempWaterIn - TempWaterOut );
			//The HeatingCoilLoad is the change in the enthalpy of the water
			WaterCoil( CoilNum ).OutletWaterEnthalpy = WaterCoil( CoilNum ).InletWaterEnthalpy - HeatingCoilLoad / WaterCoil( CoilNum ).InletWaterMassFlowRate;
			WaterCoil( CoilNum ).OutletWaterMassFlowRate = WaterCoil( CoilNum ).InletWaterMassFlowRate;

		} else { // If not running Conditions do not change across coil from inlet to outlet

			TempAirOut = TempAirIn;
			TempWaterOut = TempWaterIn;
			HeatingCoilLoad = 0.0;
			WaterCoil( CoilNum ).OutletWaterEnthalpy = WaterCoil( CoilNum ).InletWaterEnthalpy;
			WaterCoil( CoilNum ).OutletWaterMassFlowRate = 0.0;
		}

		if ( FanOpMode == CycFanCycCoil ) {
			HeatingCoilLoad *= PartLoadRatio;
		}

		// Set the outlet conditions
		WaterCoil( CoilNum ).TotWaterHeatingCoilRate = HeatingCoilLoad;
		WaterCoil( CoilNum ).OutletAirTemp = TempAirOut;
		WaterCoil( CoilNum ).OutletWaterTemp = TempWaterOut;

		// This WaterCoil does not change the moisture or Mass Flow across the component
		WaterCoil( CoilNum ).OutletAirHumRat = WaterCoil( CoilNum ).InletAirHumRat;
		WaterCoil( CoilNum ).OutletAirMassFlowRate = WaterCoil( CoilNum ).InletAirMassFlowRate;
		//Set the outlet enthalpys for air and water
		WaterCoil( CoilNum ).OutletAirEnthalpy = PsyHFnTdbW( WaterCoil( CoilNum ).OutletAirTemp, WaterCoil( CoilNum ).OutletAirHumRat );

	}

	void
	CalcDetailFlatFinCoolingCoil(
		int const CoilNum,
		int const CalcMode,
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR(S)      Russell Taylor / Richard Liesen
		//       DATE WRITTEN   Mar 1997
		//       MODIFIED       Feb 2010, B. Nigusse, FSEC, corrected units inconsistency for tube and fins
		//                      materials thermal conductivties. Now input values in the idf are in {W/(m.K)}
		//       RE-ENGINEERED  Sept 1998

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates a chilled water cooling coil.  Provided with
		// the coil geometry and the flow (i.e. air and water) inlet conditions,
		// it will calculate the flow outlet conditions and the total and latent
		// heat extraction rates from the air.  The coil model has some limitations
		// as noted in the code.

		// METHODOLOGY EMPLOYED:
		// successive substitution, solve coil as if all wet, then
		// again if partly or entirely dry

		// REFERENCES:
		// First found in Type 12 from MODSIM, but now
		// programmed directly from Elmahdy, A.H. and Mitalas, G.P.  "A
		// Simple Model for Cooling and Dehumidifying Coils for Use in
		// Calculating Energy Requirements for Buildings"  _ASHRAE
		// Transactions_ Vol. 83, Part 2, pp. 103-117 (1977).

		// OTHER NOTES:
		// Routine was originally adapted for use in IBLAST by R.D. Taylor in l993.
		// Subsequently rewritten and improved by J.C. Vanderzee in 1994
		// Revised and further enanced by R.D. Taylor in Jan 1996
		// Re-engineered for EnergyPlus by Richard Liesen PhD in 1998

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 const exp_47( std::exp( -0.41718 ) );
		static Real64 const exp_35( std::exp( -0.3574 ) );
		static std::string const RoutineName( "CalcDetailFlatFinCoolingCoil" );

		Real64 const AirViscosity( 1.846e-5 ); // Dynamic Viscosity of Air in kg/(m.s)
		Real64 const ConvK( 1.0e-3 ); // Unit conversion factor
		Real64 const unity( 1.0 );
		Real64 const zero( 0.0 );
		Real64 const TubeFoulFactor( 5.0e-2 ); // Inside tube fouling factor for water, in m2K/kW
		// Changed from m2K/W to m2K/kW for consistency with the
		// other parameters in "TubeFoulThermResis" calculation

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoefPointer;
		//    INTEGER :: CoolCoilErrs = 0
		int PartWetIterations;
		int WaterTempConvgLoop;

		bool CoilPartWetConvg;
		bool WaterTempConvg;

		Real64 AirEnthAtRsdInletWaterTemp;
		Real64 AirExitEnthlAtCoilSurfTemp;
		Real64 AirExitCoilSurfTemp;
		Real64 AirReynoldsNo;
		Real64 AirEnthAtWetDryIntrfcSurfTemp;
		Real64 AirSideDrySurfFilmCoef;
		Real64 AirSideWetSurfFilmCoef;
		Real64 AirWetDryInterfcTemp;
		Real64 CoilToAirThermResistDrySurf;
		Real64 CoilToAirThermResistWetSurf;
		Real64 DryAirSpecHeat;
		Real64 DryCoilCoeff1;
		Real64 DryCoilCoeff;
		Real64 DryCoilEfficiency;
		Real64 DryFinEfficncy;
		Real64 DryCoilInThermResist;
		Real64 DrySideEffectiveWaterTemp;
		Real64 EnterAirDewPoint;
		Real64 EnterAirHumRatDiff;
		Real64 WetDryInterSurfTempErrorLast;
		Real64 WetDryInterSurfTempError;
		Real64 expon;
		Real64 FilmCoefEqnFactor;
		Real64 FilmCoefReynldsCorrelatnFact;
		Real64 FinToTotSurfAreaRatio;
		Real64 InCoilSurfTemp;
		Real64 InsdToOutsdThermResistRatio;
		Real64 InSurfTempSatAirEnthl;
		Real64 K1;
		Real64 MeanWaterTemp;
		Real64 MoistAirSpecificHeat;
		Real64 OutCoilSurfTemp;
		Real64 OutSurfTempSatAirEnthl;
		Real64 RaisedInletWaterTemp;
		Real64 RsdInletWaterTempSatAirHumRat;
		Real64 ScaledAirMassFlowRate;
		Real64 ScaledCoilAirThermResistWetSurf;
		Real64 ScaledWaterSpecHeat;
		Real64 ScaledWaterToTubeThermResist;
		Real64 SensToTotEnthDiffRatio;
		Real64 SurfAreaWet;
		Real64 TubeFoulThermResist;
		Real64 TubeWaterVel;
		Real64 UACoilAllWet;
		Real64 UACoilPartWet;
		Real64 UADryCoil;
		Real64 WaterToTubeThermResist;
		Real64 WetAreaChange;
		Real64 WetAreaLast;
		Real64 WetCoilCoeff;
		Real64 WetCoilFinEfficncy;
		Real64 WetDryInterfcAirEnthl;
		Real64 WetDryInterfcSurfTemp;
		Real64 WetDryInterfcWaterTemp;
		Real64 WetFinEfficncy;
		Real64 WetSideEffctvWaterTemp;
		Real64 y;
		Real64 TempAirIn;
		Real64 TempAirOut;
		Real64 InletAirHumRat;
		Real64 OutletAirHumRat;
		Real64 InletAirEnthalpy;
		Real64 OutletAirEnthalpy;
		Real64 WaterMassFlowRate;
		Real64 AirMassFlow;
		Real64 TempWaterIn;
		Real64 TempWaterOut;
		Real64 TotWaterCoilLoad;
		Real64 SenWaterCoilLoad;
		Real64 AirDensity;
		Real64 AirVelocity;
		Real64 denom;
		Real64 rho;
		Real64 Cp;

		// Set derived type variables to shorter local variables
		TempAirIn = WaterCoil( CoilNum ).InletAirTemp;
		InletAirHumRat = WaterCoil( CoilNum ).InletAirHumRat;
		TempWaterIn = WaterCoil( CoilNum ).InletWaterTemp;

		//  adjust mass flow rates for cycling fan cycling coil operation
		if ( FanOpMode == CycFanCycCoil ) {
			if ( PartLoadRatio > 0.0 ) {
				AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate / PartLoadRatio;
				WaterMassFlowRate = min( WaterCoil( CoilNum ).InletWaterMassFlowRate / PartLoadRatio, WaterCoil( CoilNum ).MaxWaterMassFlowRate );
			} else {
				AirMassFlow = 0.0;
				WaterMassFlowRate = 0.0;
			}
		} else {
			AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate;
			WaterMassFlowRate = WaterCoil( CoilNum ).InletWaterMassFlowRate;
		}

		if ( WaterMassFlowRate < WaterCoil( CoilNum ).MaxWaterMassFlowRate * MinWaterMassFlowFrac ) {
			WaterMassFlowRate = 0.0;
		}
		if ( TempAirIn <= TempWaterIn ) {
			WaterMassFlowRate = 0.0;
		}
		WetDryInterfcAirEnthl = 0.0;
		OutletAirEnthalpy = 0.0;
		InletAirEnthalpy = 0.0;

		//Warning and error messages for large flow rates for the given user input geometry
		AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, TempAirIn, InletAirHumRat, RoutineName );
		if ( AirMassFlow > ( 5.0 * WaterCoil( CoilNum ).MinAirFlowArea / AirDensity ) && CoilWarningOnceFlag( CoilNum ) ) {
			ShowWarningError( "Coil:Cooling:Water:DetailedGeometry in Coil =" + WaterCoil( CoilNum ).Name );
			ShowContinueError( "Air Flow Rate Velocity has greatly exceeded upper design guidelines of ~2.5 m/s" );
			ShowContinueError( "Air MassFlowRate[kg/s]=" + TrimSigDigits( AirMassFlow, 6 ) );
			AirVelocity = AirMassFlow * AirDensity / WaterCoil( CoilNum ).MinAirFlowArea;
			ShowContinueError( "Air Face Velocity[m/s]=" + TrimSigDigits( AirVelocity, 6 ) );
			ShowContinueError( "Approximate MassFlowRate limit for Face Area[kg/s]=" + TrimSigDigits( 2.5 * WaterCoil( CoilNum ).MinAirFlowArea / AirDensity, 6 ) );
			ShowContinueError( "Coil:Cooling:Water:DetailedGeometry could be resized/autosized to handle capacity" );
			CoilWarningOnceFlag( CoilNum ) = false;
		} else if ( AirMassFlow > ( 44.7 * WaterCoil( CoilNum ).MinAirFlowArea / AirDensity ) ) {
			ShowSevereError( "Coil:Cooling:Water:DetailedGeometry in Coil =" + WaterCoil( CoilNum ).Name );
			ShowContinueError( "Air Flow Rate Velocity is > 100MPH (44.7m/s) and simulation cannot continue" );
			ShowContinueError( "Air Mass Flow Rate[kg/s]=" + TrimSigDigits( AirMassFlow, 6 ) );
			AirVelocity = AirMassFlow * AirDensity / WaterCoil( CoilNum ).MinAirFlowArea;
			ShowContinueError( "Air Face Velocity[m/s]=" + TrimSigDigits( AirVelocity, 6 ) );
			ShowContinueError( "Approximate MassFlowRate limit for Face Area[kg/s]=" + TrimSigDigits( 2.5 * WaterCoil( CoilNum ).MinAirFlowArea / AirDensity, 6 ) );
			ShowFatalError( "Coil:Cooling:Water:DetailedGeometry needs to be resized/autosized to handle capacity" );
		}

		// If Coil is Scheduled ON then do the simulation
		if ( ( ( GetCurrentScheduleValue( WaterCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( WaterMassFlowRate > 0.0 ) && ( AirMassFlow >= MinAirMassFlow ) ) || ( CalcMode == DesignCalc ) ) {
			//        transfer inputs to simulation variables and calculate
			//        known thermodynamic functions
			// All coil calcs are done in KJoules.  Convert to KJ here and then convert
			//  back to Joules at the end of the Subroutine.
			DryAirSpecHeat = PsyCpAirFnWTdb( zero, TempAirIn ) * ConvK;
			MoistAirSpecificHeat = PsyCpAirFnWTdb( InletAirHumRat, TempAirIn ) * ConvK;
			InletAirEnthalpy = WaterCoil( CoilNum ).InletAirEnthalpy * ConvK;

			EnterAirDewPoint = PsyTdpFnWPb( InletAirHumRat, OutBaroPress, RoutineName );
			//       Ratio of secondary (fin) to total (secondary plus primary) surface areas
			FinToTotSurfAreaRatio = WaterCoil( CoilNum ).FinSurfArea / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
			//      known water and air flow parameters:
			rho = GetDensityGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, TempWaterIn, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );
			//      water flow velocity - assuming number of water circuits = NumOfTubesPerRow
			TubeWaterVel = WaterMassFlowRate * 4.0 / ( WaterCoil( CoilNum ).NumOfTubesPerRow * rho * Pi * WaterCoil( CoilNum ).TubeInsideDiam * WaterCoil( CoilNum ).TubeInsideDiam );
			//      air mass flow rate per unit area
			ScaledAirMassFlowRate = ( 1.0 + InletAirHumRat ) * AirMassFlow / WaterCoil( CoilNum ).MinAirFlowArea;
			//      air flow Reynold's Number
			AirReynoldsNo = WaterCoil( CoilNum ).CoilEffectiveInsideDiam * ScaledAirMassFlowRate / AirViscosity;
			//       heat transfer coefficients and resistance components:
			//              inside (water)
			WaterToTubeThermResist = std::pow( WaterCoil( CoilNum ).TubeInsideDiam, 0.2 ) / ( WaterCoil( CoilNum ).TotTubeInsideArea * 1.429 * std::pow( TubeWaterVel, 0.8 ) );
			//              metal and fouling
			TubeFoulThermResist = ( 0.5 * ( WaterCoil( CoilNum ).TubeOutsideDiam - WaterCoil( CoilNum ).TubeInsideDiam ) / ( ConvK * WaterCoil( CoilNum ).TubeThermConductivity ) + TubeFoulFactor ) / WaterCoil( CoilNum ).TotTubeInsideArea;
			//              outside (wet and dry coil)
			FilmCoefEqnFactor = WaterCoil( CoilNum ).GeometryCoef1 * std::pow( AirReynoldsNo, WaterCoil( CoilNum ).GeometryCoef2 );
			//       (1.23 is 1/Prandt(air)**(2/3))
			AirSideDrySurfFilmCoef = 1.23 * FilmCoefEqnFactor * MoistAirSpecificHeat * ScaledAirMassFlowRate;
			FilmCoefReynldsCorrelatnFact = 1.425 + AirReynoldsNo * ( -0.51e-3 + AirReynoldsNo * 0.263e-6 );
			//       NOTE: the equation for FilmCoefReynldsCorrelatnFact generates valid results over
			//             a limited range of Air Reynolds Numbers as indicated by
			//             deleted code below.  Reynolds Numbers outside this range
			//             may result in inaccurate results or failure of the coil
			//             simulation to obtain a solution
			//             Deleted code by J.C. Vanderzee

			AirSideWetSurfFilmCoef = FilmCoefReynldsCorrelatnFact * AirSideDrySurfFilmCoef;
			//--                     need wet fin efficiency for outside
			RaisedInletWaterTemp = TempWaterIn + 0.5;

			// By this statement the Inlet Air enthalpy will never be equal to AirEnthAtRsdInletWaterTemp
			if ( ( RaisedInletWaterTemp - TempAirIn ) < 0.000001 ) {
				RaisedInletWaterTemp = TempWaterIn + 0.3;
			}
			if ( TempAirIn < RaisedInletWaterTemp ) {
				RaisedInletWaterTemp = TempAirIn - 0.3;
			}

			RsdInletWaterTempSatAirHumRat = PsyWFnTdbRhPb( RaisedInletWaterTemp, unity, OutBaroPress, RoutineName );
			AirEnthAtRsdInletWaterTemp = PsyHFnTdbW( RaisedInletWaterTemp, RsdInletWaterTempSatAirHumRat ) * ConvK;

			SensToTotEnthDiffRatio = DryAirSpecHeat * ( TempAirIn - RaisedInletWaterTemp ) / ( InletAirEnthalpy - AirEnthAtRsdInletWaterTemp );

			EnterAirHumRatDiff = InletAirHumRat - RsdInletWaterTempSatAirHumRat;
			DryFinEfficncy = 0.5 * ( WaterCoil( CoilNum ).EffectiveFinDiam - WaterCoil( CoilNum ).TubeOutsideDiam ) * std::sqrt( 2.0 * AirSideWetSurfFilmCoef / ( ConvK * WaterCoil( CoilNum ).FinThermConductivity * WaterCoil( CoilNum ).FinThickness ) );
			if ( EnterAirHumRatDiff < 0 ) {
				//       note that this condition indicates dry coil
				EnterAirHumRatDiff = -EnterAirHumRatDiff;
				SensToTotEnthDiffRatio = std::abs( SensToTotEnthDiffRatio );
			}

			if ( EnterAirHumRatDiff > 1.0 ) {
				EnterAirHumRatDiff = 1.0;
			} else if ( EnterAirHumRatDiff < 0.00001 ) {
				EnterAirHumRatDiff = 0.00001;
			}

			if ( DryFinEfficncy > 1.0 ) {
				DryFinEfficncy = 1.0;
			} else if ( DryFinEfficncy < 0.00001 ) {
				DryFinEfficncy = 0.00001;
			}

			if ( TempAirIn > 48.0 / 1.8 ) {
				WetFinEfficncy = exp_47 * std::pow( SensToTotEnthDiffRatio, 0.09471 ) * std::pow( EnterAirHumRatDiff, 0.0108 ) * std::pow( DryFinEfficncy, -0.50303 );
			} else {
				WetFinEfficncy = exp_35 * std::pow( SensToTotEnthDiffRatio, 0.16081 ) * std::pow( EnterAirHumRatDiff, 0.01995 ) * std::pow( DryFinEfficncy, -0.52951 );
			}

			if ( WetFinEfficncy > 1.0 ) WetFinEfficncy = 0.99;
			if ( WetFinEfficncy < 0.0 ) WetFinEfficncy = 0.001;
			//       wet coil fin efficiency

			WetCoilFinEfficncy = 1.0 + FinToTotSurfAreaRatio * ( WetFinEfficncy - 1.0 );
			//       wet coil outside thermal resistance = [1/UA] (wet coil)
			CoilToAirThermResistWetSurf = MoistAirSpecificHeat / ( WaterCoil( CoilNum ).TotCoilOutsideSurfArea * AirSideWetSurfFilmCoef * WetCoilFinEfficncy );
			//--                     and dry fin efficiency
			DryFinEfficncy = 0.5 * ( WaterCoil( CoilNum ).EffectiveFinDiam - WaterCoil( CoilNum ).TubeOutsideDiam ) * std::sqrt( 2.0 * AirSideDrySurfFilmCoef / ( ConvK * WaterCoil( CoilNum ).FinThermConductivity * WaterCoil( CoilNum ).FinThickness ) );
			//      NOTE: The same caveats on the validity of the FilmCoefReynldsCorrelatnFact equation
			//            hold for the DryFinEfficncy equation.  Values of DryFinEfficncy outside the
			//            specified range of validity are not guaranteed to
			//            produce results
			//             Deleted code by J.C. Vanderzee
			//       dry coil fin efficiency
			DryCoilEfficiency = 0.0;
//Tuned Replaced by below to eliminate pow calls
//			for ( CoefPointer = 1; CoefPointer <= 5; ++CoefPointer ) {
//				DryCoilEfficiency += WaterCoil( CoilNum ).DryFinEfficncyCoef( CoefPointer ) * std::pow( DryFinEfficncy, CoefPointer - 1 );
//			} // CoefPointer
			auto const & dry_fin_eff_coef( WaterCoil( CoilNum ).DryFinEfficncyCoef );
			auto DryFinEfficncy_pow( 1.0 );
			for ( CoefPointer = 1; CoefPointer <= 5; ++CoefPointer ) {
				DryCoilEfficiency += dry_fin_eff_coef( CoefPointer ) * DryFinEfficncy_pow;
				DryFinEfficncy_pow *= DryFinEfficncy;
			} // CoefPointer
			DryCoilEfficiency = 1.0 + FinToTotSurfAreaRatio * ( DryCoilEfficiency - 1.0 );
			//       dry coil outside thermal resistance = [1/UA] (dry coil)
			CoilToAirThermResistDrySurf = 1.0 / ( WaterCoil( CoilNum ).TotCoilOutsideSurfArea * AirSideDrySurfFilmCoef * DryCoilEfficiency );
			//       definitions made to simplify some of the expressions used below
			Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, TempWaterIn, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );
			ScaledWaterSpecHeat = WaterMassFlowRate * Cp * ConvK / AirMassFlow;
			DryCoilCoeff1 = 1.0 / ( AirMassFlow * MoistAirSpecificHeat ) - 1.0 / ( WaterMassFlowRate * Cp * ConvK );
			//       perform initialisations for all wet solution
			WetSideEffctvWaterTemp = WaterCoil( CoilNum ).MeanWaterTempSaved + ( TempWaterIn - WaterCoil( CoilNum ).InWaterTempSaved );
			WaterTempConvgLoop = 0;
			WaterTempConvg = false;
			//       Loop to solve coil as if all wet, converges on MeanWaterTemp eq WetSideEffctvWaterTemp
			//       if conv=.TRUE. at any time program exits loop and proceeds
			//       to part wet / part dry solution
			while ( WaterTempConvgLoop < 8 && ! WaterTempConvg ) {
				++WaterTempConvgLoop;
				ScaledWaterToTubeThermResist = WaterToTubeThermResist / ( 1.0 + 0.0146 * WetSideEffctvWaterTemp );
				ScaledCoilAirThermResistWetSurf = CoilToAirThermResistWetSurf / WaterCoil( CoilNum ).SatEnthlCurveSlope;
				UACoilAllWet = 1.0 / ( WaterCoil( CoilNum ).SatEnthlCurveSlope * ( TubeFoulThermResist + ScaledWaterToTubeThermResist + ScaledCoilAirThermResistWetSurf ) );
				//       prevents floating point error when taking exponential
				//       of a very large number
				expon = UACoilAllWet * ( 1.0 / AirMassFlow - WaterCoil( CoilNum ).SatEnthlCurveSlope / ( WaterMassFlowRate * Cp * ConvK ) );
				if ( expon < 20.0 ) { //CR7189 changed from ABS(expon) < 20
					//       negative expon can happen, but lead to tiny WetCoilCoef that aren't a problem
					WetCoilCoeff = std::exp( expon );
					// following appears similar to eq. 320 in Eng Ref but neglects K1 term
					TempWaterOut = ( ( 1.0 - WetCoilCoeff ) * ( InletAirEnthalpy - WaterCoil( CoilNum ).SatEnthlCurveConstCoef ) + WetCoilCoeff * TempWaterIn * ( WaterCoil( CoilNum ).SatEnthlCurveSlope - ScaledWaterSpecHeat ) ) / ( WaterCoil( CoilNum ).SatEnthlCurveSlope - WetCoilCoeff * ScaledWaterSpecHeat );
				} else {
					// following appears to be same as above with equation simplified to use only significant terms when WetCoilCoeff very large
					TempWaterOut = ( ( InletAirEnthalpy - WaterCoil( CoilNum ).SatEnthlCurveConstCoef ) - TempWaterIn * ( WaterCoil( CoilNum ).SatEnthlCurveSlope - ScaledWaterSpecHeat ) ) / ScaledWaterSpecHeat;

				}
				//      above is inverted form of WaterMassFlowRate*cpw*(TempWaterOut-TempWaterIn) = UA(LMHD)
				//      note simplification that hsat = WaterCoil(CoilNum)%SatEnthlCurveConstCoef +  &
				//                                      WaterCoil(CoilNum)%SatEnthlCurveSlope*WetSideEffctvWaterTemp
				MeanWaterTemp = 0.5 * ( TempWaterIn + TempWaterOut );
				OutletAirEnthalpy = InletAirEnthalpy - ( TempWaterOut - TempWaterIn ) * ScaledWaterSpecHeat;

				InsdToOutsdThermResistRatio = ( TubeFoulThermResist + ScaledWaterToTubeThermResist ) / ScaledCoilAirThermResistWetSurf;
				InCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf * ( WaterCoil( CoilNum ).SatEnthlCurveSlope * TempWaterIn + ( OutletAirEnthalpy - WaterCoil( CoilNum ).SatEnthlCurveConstCoef ) * InsdToOutsdThermResistRatio );
				OutCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf * ( WaterCoil( CoilNum ).SatEnthlCurveSlope * TempWaterOut + ( InletAirEnthalpy - WaterCoil( CoilNum ).SatEnthlCurveConstCoef ) * InsdToOutsdThermResistRatio );

				if ( std::abs( MeanWaterTemp - WetSideEffctvWaterTemp ) > 0.01 ) {
					WetSideEffctvWaterTemp = MeanWaterTemp;
					InSurfTempSatAirEnthl = PsyHFnTdbRhPb( InCoilSurfTemp, unity, OutBaroPress, RoutineName ) * ConvK;
					OutSurfTempSatAirEnthl = PsyHFnTdbRhPb( OutCoilSurfTemp, unity, OutBaroPress, RoutineName ) * ConvK;

					WaterCoil( CoilNum ).SatEnthlCurveSlope = ( OutSurfTempSatAirEnthl - InSurfTempSatAirEnthl ) / ( OutCoilSurfTemp - InCoilSurfTemp );
					WaterCoil( CoilNum ).SatEnthlCurveConstCoef = InSurfTempSatAirEnthl - WaterCoil( CoilNum ).SatEnthlCurveSlope * InCoilSurfTemp;
				} else {
					WaterTempConvg = true;
				}
			} // End of iteration loop to get MeanWaterTemp=WetSideEffctvWaterTemp
			//      if 8 CoolCoilErrs are reached without convergence and the
			//      predicted coil surface temperature at the outlet is less than
			//      the dew point coil is apparently all wet but a solution
			//      cannot be obtained
			if ( ! WaterTempConvg && ! WarmupFlag && ( OutCoilSurfTemp < EnterAirDewPoint ) ) {
				ShowRecurringWarningErrorAtEnd( WaterCoil( CoilNum ).Name + " not converged (8 iterations) due to \"Wet Convergence\" conditions.", WaterTempCoolCoilErrs( CoilNum ), std::abs( MeanWaterTemp - WetSideEffctvWaterTemp ), std::abs( MeanWaterTemp - WetSideEffctvWaterTemp ) );
				//       CoolCoilErrs = CoolCoilErrs + 1
				//       IF (CoolCoilErrs .LE. MaxCoolCoilErrs) THEN
				//          CALL ShowWarningError('tp12c0:  not converged in 8 CoolCoilErrs')
				//       END IF
			}
			WaterCoil( CoilNum ).MeanWaterTempSaved = MeanWaterTemp;
			//      now simulate wet dry coil - test outlet condition from all
			//      wet case to give an idea of the expected solution
			PartWetIterations = 0;
			WetDryInterSurfTempError = 0.0;
			CoilPartWetConvg = false;
			//      Surface temp at coil water outlet (air inlet) is less than
			//      the dew point - Coil must be completely wet so no need to
			//      simulate wet/dry case
			if ( OutCoilSurfTemp < EnterAirDewPoint ) {
				CoilPartWetConvg = true;
				WaterCoil( CoilNum ).SurfAreaWetFraction = 1.0;
				TotWaterCoilLoad = AirMassFlow * ( InletAirEnthalpy - OutletAirEnthalpy );
				AirWetDryInterfcTemp = TempAirIn;
				WetDryInterfcAirEnthl = InletAirEnthalpy;
				//      Surface temperature at coil water inlet is greater than the
				//      dewpoint - coil cannot be all wet but may be all dry -
				//      initialise with all dry solution
			} else if ( InCoilSurfTemp > EnterAirDewPoint ) {
				SurfAreaWet = 0.0;
				WaterCoil( CoilNum ).SurfAreaWetFraction = 0.0;
				WetDryInterfcWaterTemp = TempWaterIn;
				TempWaterOut = WaterCoil( CoilNum ).OutWaterTempSaved + ( TempWaterIn - WaterCoil( CoilNum ).InWaterTempSaved );
				WetAreaLast = 0.05 * WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
				//      General case - must be part-wet/part-dry - initialise
				//      accordingly with some non-zero wet area
			} else {
				if ( WaterCoil( CoilNum ).SurfAreaWetSaved != 0.0 ) {
					SurfAreaWet = WaterCoil( CoilNum ).SurfAreaWetSaved;
				} else {
					SurfAreaWet = 0.8 * WaterCoil( CoilNum ).TotCoilOutsideSurfArea * ( EnterAirDewPoint - InCoilSurfTemp ) / ( OutCoilSurfTemp - InCoilSurfTemp );
				}
				WetDryInterfcWaterTemp = TempWaterIn + EnterAirDewPoint - InCoilSurfTemp;
				WetAreaLast = 0.0;
			}
			//       Loop to solve partly wet coil, converges on wet area and
			//       boundary temperature at dew point
			//       Dry coil is special case with zero wet area, converges on
			//       mean water temperature
			while ( PartWetIterations < 40 && ! CoilPartWetConvg ) {
				++PartWetIterations;
				//      effective water temp on dry side of coil
				DrySideEffectiveWaterTemp = 0.5 * ( TempWaterOut + WetDryInterfcWaterTemp );
				//      tube inside thermal resistance
				DryCoilInThermResist = WaterToTubeThermResist / ( 1.0 + 0.0146 * DrySideEffectiveWaterTemp );
				//      overall UA, from water to air, of dry portion of coil

				UADryCoil = ( WaterCoil( CoilNum ).TotCoilOutsideSurfArea - SurfAreaWet ) / ( WaterCoil( CoilNum ).TotCoilOutsideSurfArea * ( TubeFoulThermResist + DryCoilInThermResist + CoilToAirThermResistDrySurf ) );

				// This is a numerical trap for a very small number in the EXP function that is approaching zero
				if ( UADryCoil * DryCoilCoeff1 < -60.0 ) {
					DryCoilCoeff = 0.0;
				} else {
					DryCoilCoeff = std::exp( UADryCoil * DryCoilCoeff1 );
				}

				K1 = WaterMassFlowRate * Cp * ConvK * ( DryCoilCoeff - 1.0 ) / ( WaterMassFlowRate * Cp * ConvK * DryCoilCoeff - AirMassFlow * MoistAirSpecificHeat );
				if ( SurfAreaWet != 0 ) {
					WaterCoil( CoilNum ).SurfAreaWetFraction = SurfAreaWet / WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
					//      effective water temp on wet side of coil
					WetSideEffctvWaterTemp = 0.5 * ( TempWaterIn + WetDryInterfcWaterTemp );
					//      tube inside thermal resistance
					ScaledWaterToTubeThermResist = WaterToTubeThermResist / ( 1.0 + 0.0146 * WetSideEffctvWaterTemp );
					ScaledCoilAirThermResistWetSurf = CoilToAirThermResistWetSurf / WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope;
					//      overall UA, from water to air, of wet portion of coil
					UACoilAllWet = 1.0 / ( WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope * ( TubeFoulThermResist + ScaledWaterToTubeThermResist + ScaledCoilAirThermResistWetSurf ) );
					UACoilPartWet = WaterCoil( CoilNum ).SurfAreaWetFraction * UACoilAllWet;
					expon = UACoilPartWet * ( 1.0 / AirMassFlow - WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope / ( WaterMassFlowRate * Cp * ConvK ) );
					//        prevents floating point error when taking exponential
					//        of a very large number
					if ( expon < 20.0 ) {
						WetCoilCoeff = std::exp( expon );
						//          write(outputfiledebug,*) ' wcc=',wetcoilcoeff
						denom = ( WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope - WetCoilCoeff * ScaledWaterSpecHeat - ( 1.0 - WetCoilCoeff ) * K1 * MoistAirSpecificHeat );
						//          write(outputfiledebug,*) ' denom=',denom
						//          WetDryInterfcWaterTemp = ((1.0 - WetCoilCoeff) * (InletAirEnthalpy - WaterCoil(CoilNum)%EnthVsTempCurveConst - K1 *  &
						//                                     MoistAirSpecificHeat * TempAirIn) + WetCoilCoeff * &
						//                                     TempWaterIn * (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
						//                                     ScaledWaterSpecHeat)) / (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
						//                                      WetCoilCoeff * ScaledWaterSpecHeat - (1.0 - WetCoilCoeff) * K1 * &
						//                                     MoistAirSpecificHeat)
						WetDryInterfcWaterTemp = ( ( 1.0 - WetCoilCoeff ) * ( InletAirEnthalpy - WaterCoil( CoilNum ).EnthVsTempCurveConst - K1 * MoistAirSpecificHeat * TempAirIn ) + WetCoilCoeff * TempWaterIn * ( WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope - ScaledWaterSpecHeat ) ) / denom;
					} else {
						//         approximation to equation for WetDryInterfcWaterTemp when WetCoilCoeff-->inf.
						WetDryInterfcWaterTemp = ( TempWaterIn * ( WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope - ScaledWaterSpecHeat ) - ( InletAirEnthalpy - WaterCoil( CoilNum ).EnthVsTempCurveConst - K1 * MoistAirSpecificHeat * TempAirIn ) ) / ( K1 * MoistAirSpecificHeat - ScaledWaterSpecHeat );
					}
				}
				//        air temperature at wet-dry interface
				AirWetDryInterfcTemp = TempAirIn - ( TempAirIn - WetDryInterfcWaterTemp ) * K1;
				//        coil surface temperature at wet-dry interface
				WetDryInterfcSurfTemp = WetDryInterfcWaterTemp + ( AirWetDryInterfcTemp - WetDryInterfcWaterTemp ) * ( TubeFoulThermResist + DryCoilInThermResist ) / ( TubeFoulThermResist + DryCoilInThermResist + CoilToAirThermResistDrySurf );
				if ( SurfAreaWet != 0 ) {
					WetDryInterfcAirEnthl = InletAirEnthalpy - MoistAirSpecificHeat * ( TempAirIn - AirWetDryInterfcTemp );
					//        conservation of energy - wet portion of coil
					OutletAirEnthalpy = WetDryInterfcAirEnthl - WaterMassFlowRate * Cp * ConvK * ( WetDryInterfcWaterTemp - TempWaterIn ) / AirMassFlow;
					//        ratio of inside to outside thermal resistance
					InsdToOutsdThermResistRatio = ( TubeFoulThermResist + ScaledWaterToTubeThermResist ) / ScaledCoilAirThermResistWetSurf;
					//        coil surface temperature at water inlet (air outlet)
					InCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf * ( WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope * TempWaterIn + ( OutletAirEnthalpy - WaterCoil( CoilNum ).EnthVsTempCurveConst ) * InsdToOutsdThermResistRatio );
					WetDryInterSurfTempErrorLast = WetDryInterSurfTempError;
					//        in part-wet/part-dry solution EnterAirDewPoint=WetDryInterfcSurfTemp drives WetDryInterSurfTempError->0
					WetDryInterSurfTempError = EnterAirDewPoint - WetDryInterfcSurfTemp;
				} else {
					//        dry coil solution
					WetDryInterfcAirEnthl = 0.0;
					OutletAirEnthalpy = InletAirEnthalpy - MoistAirSpecificHeat * ( TempAirIn - AirWetDryInterfcTemp );
				}
				//        total cooling = change in air enmthalpy across coil
				TotWaterCoilLoad = AirMassFlow * ( InletAirEnthalpy - OutletAirEnthalpy );
				//        conservation of energy on water stream gives water outlet
				//        temperature
				TempWaterOut = WaterMassFlowRate * Cp * ConvK; // Temp for next calc
				TempWaterOut = min( TempWaterIn + TotWaterCoilLoad / TempWaterOut, TempAirIn );
				//        update estimate of coil wet area

				if ( SurfAreaWet == 0 ) {
					MeanWaterTemp = 0.5 * ( TempWaterOut + WetDryInterfcWaterTemp );
					if ( EnterAirDewPoint > WetDryInterfcSurfTemp ) {
						SurfAreaWet = 0.5 * WetAreaLast;
					} else if ( std::abs( MeanWaterTemp - DrySideEffectiveWaterTemp ) <= 0.00002 ) {
						CoilPartWetConvg = true;
					}
				} else if ( std::abs( WetDryInterSurfTempError ) > 0.00002 || std::abs( SurfAreaWet - WetAreaLast ) / WaterCoil( CoilNum ).TotCoilOutsideSurfArea > 0.00001 ) {
					if ( WetAreaLast == 0 ) {
						WetAreaLast = SurfAreaWet;
						SurfAreaWet += 0.4 * WaterCoil( CoilNum ).TotCoilOutsideSurfArea * WetDryInterSurfTempError / ( OutCoilSurfTemp - InCoilSurfTemp );
					} else if ( WetDryInterSurfTempError != WetDryInterSurfTempErrorLast ) {
						WetAreaChange = SurfAreaWet - WetAreaLast;
						WetAreaLast = SurfAreaWet;
						SurfAreaWet -= 0.8 * WetDryInterSurfTempError * WetAreaChange / ( WetDryInterSurfTempError - WetDryInterSurfTempErrorLast );
					}
					if ( SurfAreaWet >= WaterCoil( CoilNum ).TotCoilOutsideSurfArea ) {
						SurfAreaWet = WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
						MeanWaterTemp = 0.5 * ( TempWaterIn + WetDryInterfcWaterTemp );
						if ( WetAreaLast == WaterCoil( CoilNum ).TotCoilOutsideSurfArea && std::abs( MeanWaterTemp - WetSideEffctvWaterTemp ) <= 0.00002 ) {
							CoilPartWetConvg = true;
						}
					}
					if ( SurfAreaWet <= 0 ) {
						SurfAreaWet = 0.0;
						WaterCoil( CoilNum ).SurfAreaWetFraction = 0.0;
						WetDryInterfcWaterTemp = TempWaterIn;
					}
					InSurfTempSatAirEnthl = PsyHFnTdbRhPb( InCoilSurfTemp, unity, OutBaroPress, RoutineName ) * ConvK;
					if ( ( EnterAirDewPoint - InCoilSurfTemp ) >= 0.0001 ) {
						AirEnthAtWetDryIntrfcSurfTemp = PsyHFnTdbRhPb( EnterAirDewPoint, unity, OutBaroPress, RoutineName ) * ConvK;
						WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope = ( AirEnthAtWetDryIntrfcSurfTemp - InSurfTempSatAirEnthl ) / ( EnterAirDewPoint - InCoilSurfTemp );
					} else {
						AirEnthAtWetDryIntrfcSurfTemp = PsyHFnTdbRhPb( InCoilSurfTemp + 0.0001, unity, OutBaroPress, RoutineName ) * ConvK;
						WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope = ( AirEnthAtWetDryIntrfcSurfTemp - InSurfTempSatAirEnthl ) / 0.0001;
					}
					WaterCoil( CoilNum ).EnthVsTempCurveConst = InSurfTempSatAirEnthl - WaterCoil( CoilNum ).EnthVsTempCurveAppxSlope * InCoilSurfTemp;
				} else {
					CoilPartWetConvg = true;
				}
			}
			//      error checking to see if convergence has been achieved
			if ( ! CoilPartWetConvg && ! WarmupFlag ) {
				ShowRecurringWarningErrorAtEnd( WaterCoil( CoilNum ).Name + " not converged (40 iterations) due to \"Partial Wet Convergence\" conditions.", PartWetCoolCoilErrs( CoilNum ) );
				//      CoolCoilErrs = CoolCoilErrs + 1
				//      IF (CoolCoilErrs .LE. MaxCoolCoilErrs) THEN
				//        CALL ShowWarningError('tp12c0:  not converged in 20 CoolCoilErrs')
				//      END IF
			}
			if ( WaterCoil( CoilNum ).SurfAreaWetFraction > 0 && WaterCoil( CoilNum ).SurfAreaWetFraction < 1 ) {
				WaterCoil( CoilNum ).SurfAreaWetSaved = SurfAreaWet;
			}
			//       calculate TempAirOut, OutletAirHumRat, and SensCoolRate based on equations from
			//       TYPE12 and the ASHRAE toolkit
			if ( WaterCoil( CoilNum ).SurfAreaWetFraction == 0 ) {
				//       dry coil
				TempAirOut = TempAirIn - TotWaterCoilLoad / ( AirMassFlow * MoistAirSpecificHeat );
				OutletAirHumRat = InletAirHumRat;
				SenWaterCoilLoad = TotWaterCoilLoad;
			} else {
				//       coil effectiveness
				expon = WaterCoil( CoilNum ).SurfAreaWetFraction / ( CoilToAirThermResistWetSurf * AirMassFlow );
				y = 0.0;
				if ( expon < 20.0 ) y = std::exp( -expon );
				AirExitEnthlAtCoilSurfTemp = WetDryInterfcAirEnthl - ( WetDryInterfcAirEnthl - OutletAirEnthalpy ) / ( 1.0 - y );
				AirExitCoilSurfTemp = AirExitEnthlAtCoilSurfTemp / ConvK; // TEmporary calc
				AirExitCoilSurfTemp = PsyTsatFnHPb( AirExitCoilSurfTemp, OutBaroPress );
				//       Implementation of epsilon*NTU method
				TempAirOut = AirExitCoilSurfTemp + ( AirWetDryInterfcTemp - AirExitCoilSurfTemp ) * y;
				OutletAirHumRat = PsyWFnTdbH( TempAirOut, 1000.0 * OutletAirEnthalpy, RoutineName );
				SenWaterCoilLoad = AirMassFlow * ( PsyCpAirFnWTdb( InletAirHumRat, TempAirIn ) * TempAirIn - PsyCpAirFnWTdb( OutletAirHumRat, TempAirOut ) * TempAirOut ) * ConvK;
			}

			if ( FanOpMode == CycFanCycCoil ) {
				TotWaterCoilLoad *= PartLoadRatio;
				SenWaterCoilLoad *= PartLoadRatio;
			}

			// Set the outlet conditions
			WaterCoil( CoilNum ).TotWaterCoolingCoilRate = TotWaterCoilLoad * 1000.0;
			WaterCoil( CoilNum ).SenWaterCoolingCoilRate = SenWaterCoilLoad * 1000.0;
			WaterCoil( CoilNum ).OutletAirTemp = TempAirOut;
			WaterCoil( CoilNum ).OutletWaterTemp = TempWaterOut;
			WaterCoil( CoilNum ).OutletAirEnthalpy = OutletAirEnthalpy * 1000.0;
			WaterCoil( CoilNum ).OutletAirHumRat = OutletAirHumRat;
			//The CoolingCoilLoad is the change in the enthalpy of the water
			WaterCoil( CoilNum ).OutletWaterEnthalpy = WaterCoil( CoilNum ).InletWaterEnthalpy + WaterCoil( CoilNum ).TotWaterCoolingCoilRate / WaterCoil( CoilNum ).InletWaterMassFlowRate;

			//This WaterCoil does not change the Mass Flow across the component
			WaterCoil( CoilNum ).OutletAirMassFlowRate = WaterCoil( CoilNum ).InletAirMassFlowRate;
			WaterCoil( CoilNum ).OutletWaterMassFlowRate = WaterCoil( CoilNum ).InletWaterMassFlowRate;
		} else {
			// If Coil is scheduled OFF then Outlet conditions are set to Inlet Conditions
			WaterCoil( CoilNum ).TotWaterCoolingCoilRate = 0.0;
			WaterCoil( CoilNum ).SenWaterCoolingCoilRate = 0.0;
			TempAirOut = TempAirIn;
			TempWaterOut = TempWaterIn;
			// set the outlet conditions to the coil derived type
			WaterCoil( CoilNum ).OutletAirTemp = TempAirOut;
			WaterCoil( CoilNum ).OutletWaterTemp = TempWaterOut;
			WaterCoil( CoilNum ).OutletAirEnthalpy = WaterCoil( CoilNum ).InletAirEnthalpy;
			WaterCoil( CoilNum ).OutletAirHumRat = WaterCoil( CoilNum ).InletAirHumRat;
			//The CoolingCoilLoad is the change in the enthalpy of the water
			WaterCoil( CoilNum ).OutletWaterEnthalpy = WaterCoil( CoilNum ).InletWaterEnthalpy;

			//This WaterCoil does not change the Mass Flow across the component
			WaterCoil( CoilNum ).OutletAirMassFlowRate = WaterCoil( CoilNum ).InletAirMassFlowRate;
			WaterCoil( CoilNum ).OutletWaterMassFlowRate = 0.0;
		}

		//Save some of the Values for next Time step
		WaterCoil( CoilNum ).InWaterTempSaved = TempWaterIn;
		WaterCoil( CoilNum ).OutWaterTempSaved = TempWaterOut;

	}

	void
	CoolingCoil(
		int const CoilNum,
		bool const FirstHVACIteration,
		int const CalcMode,
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	)
	{

		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   Mar 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// The subroutine has the coil logic. Three types of Cooling Coils exist:
		// They are 1.CoilDry , 2.CoilWet, 3. CoilPartDryPartWet. The logic for
		// the three individual cases is in this subroutine.

		// METHODOLOGY EMPLOYED:
		// Simulates a Coil Model from Design conditions and subsequently uses
		// configuration values (example: UA)calculated from those design conditions
		// to calculate new performance of coil from operating inputs.The values are
		// calculated in the Subroutine InitWaterCoil

		// REFERENCES:
		// ASHRAE Secondary HVAC Toolkit TRNSYS.  1990.  A Transient System
		// Simulation Program: Reference Manual. Solar Energy Laboratory, Univ. Wisconsin-
		// Madison, pp. 4.6.8-1 - 4.6.8-12.
		// Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
		// Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.

		// Using/Aliasing
		using General::SafeDivide;

		// Enforce explicit typing of all variables in this routine

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 AirInletCoilSurfTemp; // Coil surface temperature at air entrance(C)
		Real64 AirDewPointTemp; // Temperature dew point at operating condition
		Real64 OutletAirTemp; // Outlet air temperature at operating condition
		Real64 OutletAirHumRat; // Outlet air humidity ratio at operating condition
		Real64 OutletWaterTemp; // Outlet water temperature at operating condtitons
		Real64 TotWaterCoilLoad; // Total heat transfer rate(W)
		Real64 SenWaterCoilLoad; // Sensible heat transfer rate
		Real64 SurfAreaWetFraction; // Fraction of surface area wet
		Real64 AirMassFlowRate; // Air mass flow rate for the calculation

		AirInletCoilSurfTemp = 0.0; // Coil surface temperature at air entrance(C)
		AirDewPointTemp = 0.0; // Temperature dew point at operating condition
		OutletAirTemp = 0.0; // Outlet air temperature at operating condition
		OutletAirHumRat = 0.0; // Outlet air humidity ratio at operating condition
		OutletWaterTemp = 0.0; // Outlet water temperature at operating condtitons
		TotWaterCoilLoad = 0.0; // Total heat transfer rate(W)
		SenWaterCoilLoad = 0.0; // Sensible heat transfer rate
		SurfAreaWetFraction = 0.0; // Fraction of surface area wet

		if ( FanOpMode == CycFanCycCoil && PartLoadRatio > 0.0 ) { //FB Start
			AirMassFlowRate = WaterCoil( CoilNum ).InletAirMassFlowRate / PartLoadRatio;
		} else {
			AirMassFlowRate = WaterCoil( CoilNum ).InletAirMassFlowRate;
		}

		// If Coil is Scheduled ON then do the simulation
		if ( ( ( GetCurrentScheduleValue( WaterCoil( CoilNum ).SchedPtr ) > 0.0 ) && ( WaterCoil( CoilNum ).InletWaterMassFlowRate > 0.0 ) && ( AirMassFlowRate >= MinAirMassFlow ) && ( WaterCoil( CoilNum ).DesAirVolFlowRate > 0.0 ) && ( WaterCoil( CoilNum ).MaxWaterMassFlowRate > 0.0 ) ) || ( CalcMode == DesignCalc ) ) {

			//Calculate Temperature Dew Point at operating conditions.
			AirDewPointTemp = PsyTdpFnWPb( WaterCoil( CoilNum ).InletAirHumRat, OutBaroPress );

			{ auto const SELECT_CASE_var( WaterCoil( CoilNum ).CoolingCoilAnalysisMode );
			if ( SELECT_CASE_var == DetailedAnalysis ) {
				//Coil is completely dry if AirDewPointTemp is less than InletWaterTemp,hence Call CoilCompletelyDry
				if ( AirDewPointTemp <= WaterCoil( CoilNum ).InletWaterTemp ) {

					//Calculate the leaving conditions and performance of dry coil
					CoilCompletelyDry( CoilNum, WaterCoil( CoilNum ).InletWaterTemp, WaterCoil( CoilNum ).InletAirTemp, WaterCoil( CoilNum ).UACoilTotal, OutletWaterTemp, OutletAirTemp, OutletAirHumRat, TotWaterCoilLoad, FanOpMode, PartLoadRatio );

					SenWaterCoilLoad = TotWaterCoilLoad;
					SurfAreaWetFraction = 0.0;

				} else {
					//Else If AirDewPointTemp is greater than InletWaterTemp then assume the
					//external surface of coil is completely wet,hence Call CoilCompletelyWet
					//Calculate the leaving conditions and performance of wet coil
					CoilCompletelyWet( CoilNum, WaterCoil( CoilNum ).InletWaterTemp, WaterCoil( CoilNum ).InletAirTemp, WaterCoil( CoilNum ).InletAirHumRat, WaterCoil( CoilNum ).UACoilInternal, WaterCoil( CoilNum ).UACoilExternal, OutletWaterTemp, OutletAirTemp, OutletAirHumRat, TotWaterCoilLoad, SenWaterCoilLoad, SurfAreaWetFraction, AirInletCoilSurfTemp, FanOpMode, PartLoadRatio );

					//If AirDewPointTemp is less than temp of coil surface at entry of air
					if ( AirDewPointTemp < AirInletCoilSurfTemp ) {

						//Then coil is partially wet and dry hence call CoilPartWetPartDry
						//Calculate the leaving conditions and performance of dry coil
						CoilPartWetPartDry( CoilNum, FirstHVACIteration, WaterCoil( CoilNum ).InletWaterTemp, WaterCoil( CoilNum ).InletAirTemp, AirDewPointTemp, OutletWaterTemp, OutletAirTemp, OutletAirHumRat, TotWaterCoilLoad, SenWaterCoilLoad, SurfAreaWetFraction, FanOpMode, PartLoadRatio );

					} //End if for part wet part dry coil
				} //End if for dry coil

			} else if ( SELECT_CASE_var == SimpleAnalysis ) {
				//Coil is completely dry if AirDewPointTemp is less than InletWaterTemp,hence Call CoilCompletelyDry
				if ( AirDewPointTemp <= WaterCoil( CoilNum ).InletWaterTemp ) {

					//Calculate the leaving conditions and performance of dry coil
					CoilCompletelyDry( CoilNum, WaterCoil( CoilNum ).InletWaterTemp, WaterCoil( CoilNum ).InletAirTemp, WaterCoil( CoilNum ).UACoilTotal, OutletWaterTemp, OutletAirTemp, OutletAirHumRat, TotWaterCoilLoad, FanOpMode, PartLoadRatio );

					SenWaterCoilLoad = TotWaterCoilLoad;
					SurfAreaWetFraction = 0.0;

				} else {
					//Else If AirDewPointTemp is greater than InletWaterTemp then assume the
					//external surface of coil is completely wet,hence Call CoilCompletelyWet
					//Calculate the leaving conditions and performance of wet coil
					CoilCompletelyWet( CoilNum, WaterCoil( CoilNum ).InletWaterTemp, WaterCoil( CoilNum ).InletAirTemp, WaterCoil( CoilNum ).InletAirHumRat, WaterCoil( CoilNum ).UACoilInternal, WaterCoil( CoilNum ).UACoilExternal, OutletWaterTemp, OutletAirTemp, OutletAirHumRat, TotWaterCoilLoad, SenWaterCoilLoad, SurfAreaWetFraction, AirInletCoilSurfTemp, FanOpMode, PartLoadRatio );

				} //End if for dry coil

			}}

			// Report outlet variables at nodes
			WaterCoil( CoilNum ).OutletAirTemp = OutletAirTemp;
			WaterCoil( CoilNum ).OutletAirHumRat = OutletAirHumRat;
			WaterCoil( CoilNum ).OutletWaterTemp = OutletWaterTemp;
			//Report output results if the coil was operating

			if ( FanOpMode == CycFanCycCoil ) {
				TotWaterCoilLoad *= PartLoadRatio;
				SenWaterCoilLoad *= PartLoadRatio;
			}

			WaterCoil( CoilNum ).TotWaterCoolingCoilRate = TotWaterCoilLoad;
			WaterCoil( CoilNum ).SenWaterCoolingCoilRate = SenWaterCoilLoad;
			WaterCoil( CoilNum ).SurfAreaWetFraction = SurfAreaWetFraction;
			//       WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy+ &
			//                                WaterCoil(CoilNum)%TotWaterCoolingCoilRate/WaterCoil(CoilNum)%InletWaterMassFlowRate
			WaterCoil( CoilNum ).OutletWaterEnthalpy = WaterCoil( CoilNum ).InletWaterEnthalpy + SafeDivide( WaterCoil( CoilNum ).TotWaterCoolingCoilRate, WaterCoil( CoilNum ).InletWaterMassFlowRate );

		} else {
			//If both mass flow rates are zero, set outputs to inputs and return
			WaterCoil( CoilNum ).OutletWaterTemp = WaterCoil( CoilNum ).InletWaterTemp;
			WaterCoil( CoilNum ).OutletAirTemp = WaterCoil( CoilNum ).InletAirTemp;
			WaterCoil( CoilNum ).OutletAirHumRat = WaterCoil( CoilNum ).InletAirHumRat;
			WaterCoil( CoilNum ).OutletWaterEnthalpy = WaterCoil( CoilNum ).InletWaterEnthalpy;
			WaterCoil( CoilNum ).TotWaterCoolingCoilEnergy = 0.0;
			WaterCoil( CoilNum ).SenWaterCoolingCoilEnergy = 0.0;
			WaterCoil( CoilNum ).SurfAreaWetFraction = 0.0;

		} //End of the Flow or No flow If block
		WaterCoil( CoilNum ).OutletWaterMassFlowRate = WaterCoil( CoilNum ).InletWaterMassFlowRate;
		WaterCoil( CoilNum ).OutletAirMassFlowRate = WaterCoil( CoilNum ).InletAirMassFlowRate;
		WaterCoil( CoilNum ).OutletAirEnthalpy = PsyHFnTdbW( WaterCoil( CoilNum ).OutletAirTemp, WaterCoil( CoilNum ).OutletAirHumRat );

	}

	// End Algorithm Section of the Module

	// Coil Completely Dry Subroutine for Cooling Coil

	void
	CoilCompletelyDry(
		int const CoilNum,
		Real64 const WaterTempIn, // Entering water temperature
		Real64 const AirTempIn, // Entering air dry bulb temperature
		Real64 const CoilUA, // Overall heat transfer coefficient
		Real64 & OutletWaterTemp, // Leaving water temperature
		Real64 & OutletAirTemp, // Leaving air dry bulb temperature
		Real64 & OutletAirHumRat, // Leaving air humidity ratio
		Real64 & Q, // Heat transfer rate
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	)
	{

		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   March 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the performance of a sensible air-liquid heat exchanger.  Calculated
		// results include outlet air temperature and humidity, outlet water temperature,
		// and heat transfer rate.

		// METHODOLOGY EMPLOYED:
		// Models coil using effectiveness-NTU model.

		// REFERENCES:
		// Kays, W.M. and A.L. London.  1964,Compact Heat Exchangers, 2nd Edition,
		// New York: McGraw-Hill.

		// USE STATEMENTS:
		// na

		// Enforce explicit typing of all variables in this routine

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CoilCompletelyDry" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 CapacitanceAir; // Air-side capacity rate(W/C)
		Real64 CapacitanceWater; // Water-side capacity rate(W/C)
		Real64 AirMassFlow;
		Real64 WaterMassFlowRate;
		Real64 Cp;

		//  adjust mass flow rates for cycling fan cycling coil operation
		if ( FanOpMode == CycFanCycCoil ) {
			if ( PartLoadRatio > 0.0 ) {
				AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate / PartLoadRatio;
				WaterMassFlowRate = min( WaterCoil( CoilNum ).InletWaterMassFlowRate / PartLoadRatio, WaterCoil( CoilNum ).MaxWaterMassFlowRate );
			} else {
				AirMassFlow = 0.0;
				WaterMassFlowRate = 0.0;
			}
		} else {
			AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate;
			WaterMassFlowRate = WaterCoil( CoilNum ).InletWaterMassFlowRate;
		}

		// Calculate air and water capacity rates
		CapacitanceAir = AirMassFlow * PsyCpAirFnWTdb( WaterCoil( CoilNum ).InletAirHumRat, WaterCoil( CoilNum ).InletAirTemp );
		// Water Capacity Rate
		Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, WaterTempIn, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );

		CapacitanceWater = WaterMassFlowRate * Cp;

		// Determine the air and water outlet conditions
		CoilOutletStreamCondition( CoilNum, CapacitanceWater, WaterTempIn, CapacitanceAir, AirTempIn, CoilUA, OutletWaterTemp, OutletAirTemp );

		// Calculate the total and sensible heat transfer rate both are equal in case of Dry Coil
		Q = CapacitanceAir * ( AirTempIn - OutletAirTemp );

		// Outlet humidity is equal to Inlet Humidity because its a dry coil
		OutletAirHumRat = WaterCoil( CoilNum ).InletAirHumRat;

	}

	// Coil Completely Wet Subroutine for Cooling Coil

	void
	CoilCompletelyWet(
		int const CoilNum, // Number of Coil
		Real64 const WaterTempIn, // Water temperature IN to this function (C)
		Real64 const AirTempIn, // Air dry bulb temperature IN to this function(C)
		Real64 const AirHumRat, // Air Humidity Ratio IN to this funcation (C)
		Real64 const UAInternalTotal, // Internal overall heat transfer coefficient(W/m2 C)
		Real64 const UAExternalTotal, // External overall heat transfer coefficient(W/m2 C)
		Real64 & OutletWaterTemp, // Leaving water temperature (C)
		Real64 & OutletAirTemp, // Leaving air dry bulb temperature(C)
		Real64 & OutletAirHumRat, // Leaving air humidity ratio
		Real64 & TotWaterCoilLoad, // Total heat transfer rate(W)
		Real64 & SenWaterCoilLoad, // Sensible heat transfer rate(W)
		Real64 & SurfAreaWetFraction, // Fraction of surface area wet
		Real64 & AirInletCoilSurfTemp, // Surface temperature at air entrance(C)
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	)
	{

		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   Mar 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the performance of a cooling coil when the external fin surface is
		// complete wet.  Results include outlet air temperature and humidity,
		// outlet water temperature, sensible and total cooling capacities, and the wet
		// fraction of the air-side surface area.

		// METHODOLOGY EMPLOYED:
		// Models coil as counterflow heat exchanger. Approximates saturated air enthalpy as
		// a linear function of temperature
		// TRNSYS.  1990.  A Transient System Simulation Program: Reference Manual.
		// Solar Energy Laboratory, Univ. Wisconsin Madison, pp. 4.6.8-1 - 4.6.8-12.
		// Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
		// Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.
		// Coil Uses Enthalpy Based Heat Transfer Coefficents and converts them to
		// convential UA values. Intermediate value of fictitious Cp is defined. This follow
		// the same procedure followed in the Design Calculation of the Coil. See the node in
		// the one time calculation for design condition.

		// REFERENCES:
		// Elmahdy, A.H. and Mitalas, G.P.  1977."A Simple Model for Cooling and
		// Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
		// ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.

		// USE STATEMENTS:

		// Enforce explicit typing of all variables in this routine

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CoilCompletelyWet" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 AirSideResist; // Air-side resistance to heat transfer(m2 C/W)
		Real64 WaterSideResist; // Liquid-side resistance to heat transfer(m2 C/W)
		Real64 EnteringAirDewPt; // Entering air dew point(C)
		Real64 UACoilTotalEnth; // Overall enthalpy heat transfer coefficient(kg/s)
		Real64 CapacityRateAirWet; // Air-side capacity rate(kg/s)
		Real64 CapacityRateWaterWet; // Liquid-side capacity rate(kg/s)
		Real64 ResistRatio; // Ratio of resistances
		Real64 EnthAirOutlet; // Outlet air enthalpy
		Real64 EnthSatAirInletWaterTemp; // Saturated enthalpy of air at entering water temperature(J/kg)
		Real64 EnthSatAirOutletWaterTemp; // Saturated enthalpy of air at exit water temperature(J/kg)
		Real64 EnthSatAirCoilSurfaceEntryTemp; // Saturated enthalpy of air at entering surface temperature(J/kg)
		Real64 EnthSatAirCoilSurfaceExitTemp; // Saturated enthalpy of air at exit surface temperature(J/kg)
		Real64 EnthAirInlet; // Enthalpy of air at inlet
		Real64 IntermediateCpSat; // Coefficient for equation below(J/kg C)
		// EnthSat1-EnthSat2 = IntermediateCpSat*(TSat1-TSat2)
		// (all water and surface temperatures are
		// related to saturated air enthalpies for
		// wet surface heat transfer calculations)
		Real64 const SmallNo( 1.e-9 ); // smallNo used in place of 0
		Real64 AirMassFlow;
		Real64 WaterMassFlowRate;
		Real64 Cp;

		SurfAreaWetFraction = 1.0;
		AirSideResist = 1.0 / max( UAExternalTotal, SmallNo );
		WaterSideResist = 1.0 / max( UAInternalTotal, SmallNo );

		//  adjust mass flow rates for cycling fan cycling coil operation
		if ( FanOpMode == CycFanCycCoil ) {
			if ( PartLoadRatio > 0.0 ) {
				AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate / PartLoadRatio;
				WaterMassFlowRate = min( WaterCoil( CoilNum ).InletWaterMassFlowRate / PartLoadRatio, WaterCoil( CoilNum ).MaxWaterMassFlowRate );
			} else {
				AirMassFlow = 0.0;
				WaterMassFlowRate = 0.0;
			}
		} else {
			AirMassFlow = WaterCoil( CoilNum ).InletAirMassFlowRate;
			WaterMassFlowRate = WaterCoil( CoilNum ).InletWaterMassFlowRate;
		}

		// Calculate enthalpies of entering air and water

		// Enthalpy of air at inlet to the coil
		EnthAirInlet = PsyHFnTdbW( AirTempIn, AirHumRat );

		// Saturation Enthalpy of Air at inlet water temperature
		EnthSatAirInletWaterTemp = PsyHFnTdbW( WaterTempIn, PsyWFnTdpPb( WaterTempIn, OutBaroPress ) );

		// Estimate IntermediateCpSat using entering air dewpoint and water temperature
		EnteringAirDewPt = PsyTdpFnWPb( AirHumRat, OutBaroPress );

		// An intermediate value of Specific heat . EnthSat1-EnthSat2 = IntermediateCpSat*(TSat1-TSat2)
		IntermediateCpSat = ( PsyHFnTdbW( EnteringAirDewPt, PsyWFnTdpPb( EnteringAirDewPt, OutBaroPress ) ) - EnthSatAirInletWaterTemp ) / ( EnteringAirDewPt - WaterTempIn );

		// Determine air and water enthalpy outlet conditions by modeling
		// coil as counterflow enthalpy heat exchanger
		UACoilTotalEnth = 1.0 / ( IntermediateCpSat * WaterSideResist + AirSideResist * PsyCpAirFnWTdb( 0.0, AirTempIn ) );
		CapacityRateAirWet = AirMassFlow;
		Cp = GetSpecificHeatGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, WaterTempIn, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );
		CapacityRateWaterWet = WaterMassFlowRate * ( Cp / IntermediateCpSat );
		CoilOutletStreamCondition( CoilNum, CapacityRateAirWet, EnthAirInlet, CapacityRateWaterWet, EnthSatAirInletWaterTemp, UACoilTotalEnth, EnthAirOutlet, EnthSatAirOutletWaterTemp );

		// Calculate entering and leaving external surface conditions from
		// air and water conditions and the ratio of resistances
		ResistRatio = ( WaterSideResist ) / ( WaterSideResist + PsyCpAirFnWTdb( 0.0, AirTempIn ) / IntermediateCpSat * AirSideResist );
		EnthSatAirCoilSurfaceEntryTemp = EnthSatAirOutletWaterTemp + ResistRatio * ( EnthAirInlet - EnthSatAirOutletWaterTemp );
		EnthSatAirCoilSurfaceExitTemp = EnthSatAirInletWaterTemp + ResistRatio * ( EnthAirOutlet - EnthSatAirInletWaterTemp );

		// Calculate Coil Surface Temperature at air entry to the coil
		AirInletCoilSurfTemp = PsyTsatFnHPb( EnthSatAirCoilSurfaceEntryTemp, OutBaroPress );

		// Calculate outlet air temperature and humidity from enthalpies and surface conditions.
		TotWaterCoilLoad = WaterCoil( CoilNum ).InletAirMassFlowRate * ( EnthAirInlet - EnthAirOutlet );
		OutletWaterTemp = WaterTempIn + TotWaterCoilLoad / max( WaterCoil( CoilNum ).InletWaterMassFlowRate, SmallNo ) / Cp;

		// Calculates out put variable for  the completely wet coil
		WetCoilOutletCondition( CoilNum, AirTempIn, EnthAirInlet, EnthAirOutlet, UAExternalTotal, OutletAirTemp, OutletAirHumRat, SenWaterCoilLoad );

	}

	// Coil Part Wet Part Dry Subroutine for Cooling Coil

	void
	CoilPartWetPartDry(
		int const CoilNum, // Number of Coil
		bool const FirstHVACIteration, // Saving Old values
		Real64 const InletWaterTemp, // Entering liquid temperature(C)
		Real64 const InletAirTemp, // Entering air dry bulb temperature(C)
		Real64 const AirDewPointTemp, // Entering air dew point(C)
		Real64 & OutletWaterTemp, // Leaving liquid temperature(C)
		Real64 & OutletAirTemp, // Leaving air dry bulb temperature(C)
		Real64 & OutletAirHumRat, // Leaving air humidity ratio
		Real64 & TotWaterCoilLoad, // Total heat transfer rate (W)
		Real64 & SenWaterCoilLoad, // Sensible heat transfer rate (W)
		Real64 & SurfAreaWetFraction, // Fraction of surface area wet
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	)
	{

		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   March 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the performance of a cooling  coil when the external fin surface is
		// part wet and part dry.  Results include outlet air temperature and humidity,
		// outlet liquid temperature, sensible and total cooling capacities, and the wet
		// fraction of the air-side surface area.

		// METHODOLOGY EMPLOYED:
		// Models coil using effectiveness NTU model

		// REFERENCES:
		// Elmahdy, A.H. and Mitalas, G.P.  1977. "A Simple Model for Cooling and
		// Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
		// ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.
		// TRNSYS.  1990.  A Transient System Simulation Program: Reference Manual.
		// Solar Energy Laboratory, Univ. Wisconsin- Madison, pp. 4.6.8-1 - 4.6.8-12.
		// Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
		// Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.

		// Using/Aliasing
		using General::Iterate;

		// Enforce explicit typing of all variables in this routine

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const itmax( 60 );
		Real64 const smalltempdiff( 1.0e-9 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DryCoilHeatTranfer; // Heat transfer rate for dry coil(W)
		Real64 WetCoilTotalHeatTransfer; // Total heat transfer rate for wet coil(W)
		Real64 WetCoilSensibleHeatTransfer; // Sensible heat transfer rate for wet coil(W)
		Real64 SurfAreaWet; // Air-side area of wet coil(m2)
		Real64 SurfAreaDry; // Air-side area of dry coil(m2)
		Real64 DryCoilUA; // Overall heat transfer coefficient for dry coil(W/C)
		Real64 WetDryInterfcWaterTemp; // Liquid temperature at wet/dry boundary(C)
		Real64 WetDryInterfcAirTemp; // Air temperature at wet/dry boundary(C)
		Real64 WetDryInterfcSurfTemp; // Surface temperature at wet/dry boundary(C)
		Real64 EstimateWetDryInterfcWaterTemp; // Estimated liquid temperature at wet/dry boundary(C)
		Real64 EstimateSurfAreaWetFraction; // Initial Estimate for Fraction of Surface Wet with condensation
		Real64 WetPartUAInternal; // UA of Wet Coil Internal
		Real64 WetPartUAExternal; // UA of Dry Coil External
		Real64 WetDryInterfcHumRat; // Humidity Ratio at interface of the wet dry transition
		Real64 X1T; // Variables used in the two iteration in this subroutine.
		Real64 NewSurfAreaWetFrac; // Variables used in the two iteration in this subroutine.
		Real64 ResultXT; // Variables used in the two iteration in this subroutine.
		Real64 Y1T; // Variables used in the two iterations in this subroutine.
		Real64 errorT; // Error in interation for First If loop
		Real64 error; // Deviation of dependent variable in iteration
		Real64 SurfAreaFracPrevious;
		Real64 ErrorPrevious;
		Real64 SurfAreaFracLast;
		Real64 ErrorLast;
		int iter; // Iteration counter
		int icvg; // Iteration convergence flag
		int icvgT; // Iteration Convergence Flag for First If loop
		int itT; // Iteration Counter for First If Loop

		// Iterates on SurfAreaWetFraction to converge on surface temperature equal to
		// entering air dewpoint at wet/dry boundary.

		// Preliminary estimates of coil performance to begin iteration
		OutletWaterTemp = InletAirTemp;
		DryCoilHeatTranfer = 0.0;
		WetCoilTotalHeatTransfer = 0.0;
		WetCoilSensibleHeatTransfer = 0.0;

		if ( FirstHVACIteration ) {
			// Estimate liquid temperature at boundary as entering air dew point
			WetDryInterfcWaterTemp = AirDewPointTemp;

			// Estimate fraction wet surface area based on liquid temperatures
			if ( std::abs( OutletWaterTemp - InletWaterTemp ) > smalltempdiff ) {
				SurfAreaWetFraction = ( WetDryInterfcWaterTemp - InletWaterTemp ) / ( OutletWaterTemp - InletWaterTemp );
			} else {
				SurfAreaWetFraction = 0.0;
			}

		} else {
			SurfAreaWetFraction = WaterCoil( CoilNum ).SurfAreaWetFractionSaved;

		}
		// BEGIN LOOP to converge on SurfAreaWetFraction
		// The method employed in this loop is as follows: The coil is partially wet and partially dry,
		// we calculate the temperature of the coil at the interface, (the point at which the moisture begins
		// to condense) temperature of the  water  at interface and air temp is dew point at that location.
		// This is done by Iterating between the Completely Dry and Completely Wet Coil until the outlet
		// water temperature of one coil equals the inlet water temperature of another.
		// Using this value of interface temperature we now iterate to calculate Surface Fraction Wet, Iterate
		// function perturbs the value of Surface Fraction Wet and based on this new value the entire loop is
		// repeated to get a new interface water temperature and then surface fraction wet is again calculated.
		// This process continues till the error between the Wet Dry Interface Temp and Air Dew Point becomes
		// very negligible and in 95% of the cases its is a complete convergence to give the exact surface Wet
		// fraction.
		NewSurfAreaWetFrac = SurfAreaWetFraction;
		error = 0.0;
		SurfAreaFracPrevious = SurfAreaWetFraction;
		ErrorPrevious = 0.0;
		SurfAreaFracLast = SurfAreaWetFraction;
		ErrorLast = 0.0;

		for ( iter = 1; iter <= itmax; ++iter ) {

			// Calculating Surface Area Wet and Surface Area Dry
			SurfAreaWet = SurfAreaWetFraction * WaterCoil( CoilNum ).TotCoilOutsideSurfArea;
			SurfAreaDry = WaterCoil( CoilNum ).TotCoilOutsideSurfArea - SurfAreaWet;

			// Calculating UA values for the Dry Part of the Coil
			DryCoilUA = SurfAreaDry / ( 1.0 / WaterCoil( CoilNum ).UACoilInternalPerUnitArea + 1.0 / WaterCoil( CoilNum ).UADryExtPerUnitArea );

			// Calculating UA Value for the Wet part of the Coil
			WetPartUAExternal = WaterCoil( CoilNum ).UAWetExtPerUnitArea * SurfAreaWet;
			WetPartUAInternal = WaterCoil( CoilNum ).UACoilInternalPerUnitArea * SurfAreaWet;

			// Calculating Water Temperature at Wet Dry Interface of the coil
			WetDryInterfcWaterTemp = InletWaterTemp + SurfAreaWetFraction * ( OutletWaterTemp - InletWaterTemp );

			// BEGIN LOOP to converge on liquid temperature at wet/dry boundary
			for ( itT = 1; itT <= itmax; ++itT ) {

				// Calculate dry coil performance with estimated liquid temperature at the boundary.
				CoilCompletelyDry( CoilNum, WetDryInterfcWaterTemp, InletAirTemp, DryCoilUA, OutletWaterTemp, WetDryInterfcAirTemp, WetDryInterfcHumRat, DryCoilHeatTranfer, FanOpMode, PartLoadRatio );

				// Calculate wet coil performance with calculated air temperature at the boundary.
				CoilCompletelyWet( CoilNum, InletWaterTemp, WetDryInterfcAirTemp, WetDryInterfcHumRat, WetPartUAInternal, WetPartUAExternal, EstimateWetDryInterfcWaterTemp, OutletAirTemp, OutletAirHumRat, WetCoilTotalHeatTransfer, WetCoilSensibleHeatTransfer, EstimateSurfAreaWetFraction, WetDryInterfcSurfTemp, FanOpMode, PartLoadRatio );

				// Iterating to calculate the actual wet dry interface water temperature.
				errorT = EstimateWetDryInterfcWaterTemp - WetDryInterfcWaterTemp;
				Iterate( ResultXT, 0.001, WetDryInterfcWaterTemp, errorT, X1T, Y1T, itT, icvgT );
				WetDryInterfcWaterTemp = ResultXT;

				// IF convergence is achieved then exit the itT to itmax Do loop.
				if ( icvgT == 1 ) break;

			} // End Do for Liq Boundary temp Convergence

			// Wet Dry Interface temperature not converged after maximum specified iterations.
			// Print error message, set return error flag
			if ( ( itT > itmax ) && ( ! WarmupFlag ) ) {
				ShowWarningError( "For Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
				ShowContinueError( "CoilPartWetPartDry: Maximum iterations exceeded for Liq Temp, at Interface" );
			}

			// If Following condition prevails then surface is dry, calculate dry coil performance and return
			if ( SurfAreaWetFraction <= 0.0 && WetDryInterfcSurfTemp >= AirDewPointTemp ) {

				// Calculating Value of Dry UA for the coil
				DryCoilUA = WaterCoil( CoilNum ).TotCoilOutsideSurfArea / ( 1.0 / WaterCoil( CoilNum ).UACoilInternalPerUnitArea + 1.0 / WaterCoil( CoilNum ).UADryExtPerUnitArea );

				// Calling the Completely Dry Coil for outputs
				CoilCompletelyDry( CoilNum, InletWaterTemp, InletAirTemp, DryCoilUA, OutletWaterTemp, OutletAirTemp, OutletAirHumRat, TotWaterCoilLoad, FanOpMode, PartLoadRatio );

				// Sensible load = Total load in a Completely Dry Coil
				SenWaterCoilLoad = TotWaterCoilLoad;

				// All coil is Dry so fraction wet is ofcourse =0
				SurfAreaWetFraction = 0.0;
				return;
			}

			// IF the coil is not Dry then iterate to calculate Fraction of surface area that is wet.
			error = WetDryInterfcSurfTemp - AirDewPointTemp;
			CoilAreaFracIter( NewSurfAreaWetFrac, SurfAreaWetFraction, error, SurfAreaFracPrevious, ErrorPrevious, SurfAreaFracLast, ErrorLast, iter, icvg );
			SurfAreaWetFraction = NewSurfAreaWetFrac;

			//If converged, leave iteration loop
			if ( icvg == 1 ) break;

			// Surface temperature not converged.  Repeat calculations with new
			// estimate of fraction wet surface area.
			if ( SurfAreaWetFraction > 1.0 ) SurfAreaWetFraction = 1.0;
			if ( SurfAreaWetFraction <= 0.0 ) SurfAreaWetFraction = 0.0098;

		} // End do for the overall iteration

		// Calculate sum of total and sensible heat transfer from dry and wet parts.
		TotWaterCoilLoad = DryCoilHeatTranfer + WetCoilTotalHeatTransfer;
		SenWaterCoilLoad = DryCoilHeatTranfer + WetCoilSensibleHeatTransfer;

		// Save last iterations values for this current time step
		WaterCoil( CoilNum ).SurfAreaWetFractionSaved = SurfAreaWetFraction;

	}

	// Calculating coil UA for Cooling Coil

	Real64
	CalcCoilUAbyEffectNTU(
		int const CoilNum,
		Real64 const CapacityStream1, // Capacity rate of stream1.(W/C)
		Real64 const EnergyInStreamOne, // Inlet state of stream1.(C)
		Real64 const CapacityStream2, // Capacity rate of stream2.(W/C)
		Real64 const EnergyInStreamTwo, // Inlet state of stream2.(C)
		Real64 const DesTotalHeatTransfer // Heat transfer rate(W)
	)
	{

		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   March 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the UA of a heat exchanger using the effectiveness-NTU relationships
		// given the entering capacity rate and temperature of each flow stream, the
		// heat transfer rate under these conditions and the heat exchanger configuration.

		// METHODOLOGY EMPLOYED:
		// Models coil using effectiveness NTU model

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::Iterate;

		// Enforce explicit typing of all variables in this routine

		// Return value
		Real64 CalcCoilUAbyEffectNTU; // Overall heat transfer coefficient(W/C)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const SmallNo( 1.e-9 );
		int const itmax( 12 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 MaxHeatTransfer; // Maximum heat transfer from inlet conditions (W)
		Real64 EstimatedHeatTransfer; // Estimated heat transfer in iteration(W)
		Real64 CoilUA; // Estimated heat transfer coefficient(W/C)
		Real64 error; // Deviation of dependent variable in iteration
		Real64 X1; // Previous values of independent variable in iteration
		Real64 Y1;
		Real64 ResultX;
		Real64 EnergyOutStreamOne; // Intermediate Variable used
		Real64 EnergyOutStreamTwo; // Intermediate variable used
		Real64 DesTotalHeatTransferCheck; // Check value to keep design total heat transfer in range
		int iter; // Iteration index
		int icvg; // Iteration convergence flag

		// Check for Q out of range (effectiveness > 1)
		MaxHeatTransfer = std::abs( min( CapacityStream1, CapacityStream2 ) * ( EnergyInStreamOne - EnergyInStreamTwo ) );

		// Error Message
		if ( ( std::abs( DesTotalHeatTransfer ) - MaxHeatTransfer ) / max( MaxHeatTransfer, SmallNo ) > SmallNo ) {
			ShowWarningError( "For Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
			ShowContinueError( "CalcCoilUAbyEffectNTU:Given Q impossible for given inlet states, proceeding with MaxHeat Transfer" );
			ShowContinueError( "Check the Sizing:System and Sizing:Zone cooling design supply air temperature and " );
			ShowContinueError( "the Sizing:Plant design Loop exit temperature.  There must be sufficient difference between these two temperatures." );
		}

		// Design Heat Transfer cannot exceed Max heat Transfer , setting it value such that effectiveness<1.0
		if ( ( DesTotalHeatTransfer ) > ( MaxHeatTransfer ) ) {
			// Pegging value so that effectiveness is less than 1.
			DesTotalHeatTransferCheck = 0.9 * MaxHeatTransfer;

			// Estimate CalcCoilUAbyEffectNTU
			CoilUA = std::abs( DesTotalHeatTransferCheck / ( EnergyInStreamOne - EnergyInStreamTwo ) );

		} else {

			// Estimate CalcCoilUAbyEffectNTU
			CoilUA = std::abs( DesTotalHeatTransfer / ( EnergyInStreamOne - EnergyInStreamTwo ) );

		}

		// BEGIN LOOP to iteratively calculate CalcCoilUAbyEffectNTU
		for ( iter = 1; iter <= itmax; ++iter ) {

			// Calculate heat transfer rate for estimated CalcCoilUAbyEffectNTU
			CoilOutletStreamCondition( CoilNum, CapacityStream1, EnergyInStreamOne, CapacityStream2, EnergyInStreamTwo, CoilUA, EnergyOutStreamOne, EnergyOutStreamTwo );

			// Initial Guess for a value of heat transfer
			EstimatedHeatTransfer = CapacityStream1 * ( EnergyInStreamOne - EnergyOutStreamOne );

			// Calculate new estimate for CalcCoilUAbyEffectNTU by iteration
			if ( DesTotalHeatTransfer > MaxHeatTransfer ) {
				error = std::abs( EstimatedHeatTransfer ) - std::abs( DesTotalHeatTransferCheck );
			} else {
				error = std::abs( EstimatedHeatTransfer ) - std::abs( DesTotalHeatTransfer );
			}
			Iterate( ResultX, 0.01, CoilUA, error, X1, Y1, iter, icvg );
			CoilUA = ResultX;
			// If converged, leave loop
			if ( icvg == 1 ) break;
		}

		// If not converged after itmax iterations, return error code
		if ( ( iter > itmax ) && ( ! WarmupFlag ) ) {
			ShowWarningError( "For Coil:Cooling:Water " + WaterCoil( CoilNum ).Name );
			ShowContinueError( "CalcCoilUAbyEffectNTU: Maximum iterations exceeded:Coil UA calculation" );
			CalcCoilUAbyEffectNTU = 0.0; //Autodesk:Return Line added to set return value: Using non-converged CoilUA value may be preferred but that was not happening
		} else {

			// Assign value to CalcCoilUAbyEffectNTU
			CalcCoilUAbyEffectNTU = CoilUA;
		}

		return CalcCoilUAbyEffectNTU;
	}

	// Calculating coil outlet stream conditions and coil UA for Cooling Coil

	void
	CoilOutletStreamCondition(
		int const CoilNum,
		Real64 const CapacityStream1, // Capacity rate of stream1(W/C)
		Real64 const EnergyInStreamOne, // Inlet state of stream1 (C)
		Real64 const CapacityStream2, // Capacity rate of stream2 (W/C)
		Real64 const EnergyInStreamTwo, // Inlet state of stream2 (C)
		Real64 const CoilUA, // Heat transfer rateW)
		Real64 & EnergyOutStreamOne, // Outlet state of stream1 (C)
		Real64 & EnergyOutStreamTwo // Outlet state of stream2 (C)
	)
	{

		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   March 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the outlet states of a simple heat exchanger using the effectiveness-Ntu
		// method of analysis.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Kays, W.M. and A.L. London.  1964.Compact Heat Exchangers, 2nd Ed.McGraw-Hill:New York.

		// USE STATEMENTS:
		// na

		// Enforce explicit typing of all variables in this routine

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const LargeNo( 1.e10 ); // value used in place of infinity
		Real64 const SmallNo( 1.e-15 ); // value used in place of zero

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 MinimumCapacityStream; // Minimum capacity rate of the streams(W/C)
		Real64 MaximumCapacityStream; // Maximum capacity rate of the streams(W/C)
		Real64 RatioStreamCapacity; // Ratio of minimum to maximum capacity rate
		Real64 NTU; // Number of transfer units
		static Real64 effectiveness( 0.0 ); // Heat exchanger effectiveness
		Real64 MaxHeatTransfer; // Maximum heat transfer possible(W)
		Real64 e; // Intermediate variables in effectivness equation
		Real64 eta;
		Real64 b;
		Real64 d;

		// NTU and MinimumCapacityStream/MaximumCapacityStream (RatioStreamCapacity) calculations
		MinimumCapacityStream = min( CapacityStream1, CapacityStream2 );
		MaximumCapacityStream = max( CapacityStream1, CapacityStream2 );

		if ( std::abs( MaximumCapacityStream ) <= 1.e-6 ) { // .EQ. 0.0d0) THEN
			RatioStreamCapacity = 1.0;
		} else {
			RatioStreamCapacity = MinimumCapacityStream / MaximumCapacityStream;
		}

		if ( std::abs( MinimumCapacityStream ) <= 1.e-6 ) { // .EQ. 0.0d0) THEN
			NTU = LargeNo;
		} else {
			NTU = CoilUA / MinimumCapacityStream;
		}

		// Calculate effectiveness for special limiting cases
		if ( NTU <= 0.0 ) {
			effectiveness = 0.0;

		} else if ( RatioStreamCapacity < SmallNo ) {
			// MinimumCapacityStream/MaximumCapacityStream = 0 and effectiveness is independent of configuration
			// 20 is the Limit Chosen for Exponential Function, beyond which there is float point error.
			if ( NTU > 20.0 ) {
				effectiveness = 1.0;
			} else {
				effectiveness = 1.0 - std::exp( -NTU );
			}
			// Calculate effectiveness depending on heat exchanger configuration
		} else if ( WaterCoil( CoilNum ).HeatExchType == CounterFlow ) {

			// Counterflow Heat Exchanger Configuration
			if ( std::abs( RatioStreamCapacity - 1.0 ) < SmallNo ) {
				effectiveness = NTU / ( NTU + 1.0 );
			} else {
				if ( NTU * ( 1.0 - RatioStreamCapacity ) > 20.0 ) {
					e = 0.0;
				} else {
					e = std::exp( -NTU * ( 1.0 - RatioStreamCapacity ) );
				}
				effectiveness = ( 1.0 - e ) / ( 1.0 - RatioStreamCapacity * e );
			}

		} else if ( WaterCoil( CoilNum ).HeatExchType == CrossFlow ) {
			// Cross flow, both streams unmixed
			eta = std::pow( NTU, -0.22 );
			if ( ( NTU * RatioStreamCapacity * eta ) > 20.0 ) {
				b = 1.0 / ( RatioStreamCapacity * eta );
				if ( b > 20.0 ) {
					effectiveness = 1.0;
				} else {
					effectiveness = 1.0 - std::exp( -b );
					if ( effectiveness < 0.0 ) effectiveness = 0.0;
				}
			} else {
				d = ( ( std::exp( -NTU * RatioStreamCapacity * eta ) - 1.0 ) / ( RatioStreamCapacity * eta ) );
				if ( d < -20.0 || d > 0.0 ) {
					effectiveness = 1.0;
				} else {
					effectiveness = 1.0 - std::exp( ( std::exp( -NTU * RatioStreamCapacity * eta ) - 1.0 ) / ( RatioStreamCapacity * eta ) );
					if ( effectiveness < 0.0 ) effectiveness = 0.0;
				}
			}

		}

		// Determine leaving conditions for the two streams
		MaxHeatTransfer = max( MinimumCapacityStream, SmallNo ) * ( EnergyInStreamOne - EnergyInStreamTwo );
		EnergyOutStreamOne = EnergyInStreamOne - effectiveness * MaxHeatTransfer / max( CapacityStream1, SmallNo );
		EnergyOutStreamTwo = EnergyInStreamTwo + effectiveness * MaxHeatTransfer / max( CapacityStream2, SmallNo );

	}

	// Subroutine for caculating outlet condition if coil is wet , for Cooling Coil

	void
	WetCoilOutletCondition(
		int const CoilNum,
		Real64 const AirTempIn, // Entering air dry bulb temperature(C)
		Real64 const EnthAirInlet, // Entering air enthalpy(J/kg)
		Real64 const EnthAirOutlet, // Leaving air enthalpy(J/kg)
		Real64 const UACoilExternal, // Heat transfer coefficient for external surface (W/C)
		Real64 & OutletAirTemp, // Leaving air dry bulb temperature(C)
		Real64 & OutletAirHumRat, // Leaving air humidity ratio
		Real64 & SenWaterCoilLoad // Sensible heat transfer rate(W)
	)
	{

		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   Mar 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the leaving air temperature,the leaving air humidity ratio and the
		// sensible cooling capacity of wet cooling coil.

		// METHODOLOGY EMPLOYED:
		// Assumes condensate at uniform temperature.

		// REFERENCES:
		// Elmahdy, A.H. and Mitalas, G.P.  1977."A Simple Model for Cooling and
		// Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
		// ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.

		// USE STATEMENTS:

		// Enforce explicit typing of all variables in this routine

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const SmallNo( 1.e-9 ); // SmallNo value used in place of zero

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 CapacitanceAir; // Air capacity rate(W/C)
		Real64 NTU; // Number of heat transfer units
		Real64 effectiveness; // Heat exchanger effectiveness
		Real64 EnthAirCondensateTemp; // Saturated air enthalpy at temperature of condensate(J/kg)
		Real64 TempCondensation; // Temperature of condensate(C)
		Real64 TempAirDewPoint; // Temperature air dew point

		// Determine the temperature effectiveness, assuming the temperature
		// of the condensate is constant (MinimumCapacityStream/MaximumCapacityStream = 0) and the specific heat
		// of moist air is constant
		CapacitanceAir = WaterCoil( CoilNum ).InletAirMassFlowRate * PsyCpAirFnWTdb( WaterCoil( CoilNum ).InletAirHumRat, AirTempIn );

		// Calculating NTU from UA and Capacitance.
		//del      NTU = UACoilExternal/MAX(CapacitanceAir,SmallNo)
		//del      effectiveness = 1 - EXP(-MAX(0.0d0,NTU))
		// Calculating NTU from UA and Capacitance.
		if ( UACoilExternal > 0.0 ) {
			if ( CapacitanceAir > 0.0 ) {
				NTU = UACoilExternal / CapacitanceAir;
			} else {
				NTU = 0.0;
			}
			effectiveness = 1.0 - std::exp( -NTU );
		} else {
			effectiveness = 0.0;
		}

		// Calculate coil surface enthalpy and temperature at the exit
		// of the wet part of the coil using the effectiveness relation
		effectiveness = max( effectiveness, SmallNo );
		EnthAirCondensateTemp = EnthAirInlet - ( EnthAirInlet - EnthAirOutlet ) / effectiveness;

		// Calculate condensate temperature as the saturation temperature
		// at given saturation enthalpy
		TempCondensation = PsyTsatFnHPb( EnthAirCondensateTemp, OutBaroPress );

		TempAirDewPoint = PsyTdpFnWPb( WaterCoil( CoilNum ).InletAirHumRat, OutBaroPress );

		if ( ( TempAirDewPoint - TempCondensation ) > 0.1 ) {

			// Calculate Outlet Air Temperature using effectivness
			OutletAirTemp = AirTempIn - ( AirTempIn - TempCondensation ) * effectiveness;
			// Calculate Outlet air humidity ratio from PsyWFnTdbH routine
			OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, EnthAirOutlet );

		} else {
			OutletAirHumRat = WaterCoil( CoilNum ).InletAirHumRat;
			OutletAirTemp = PsyTdbFnHW( EnthAirOutlet, OutletAirHumRat );

		}

		// Calculate Sensible Coil Load
		SenWaterCoilLoad = CapacitanceAir * ( AirTempIn - OutletAirTemp );

	}

	// Beginning of Update subroutines for the WaterCoil Module
	// *****************************************************************************

	void
	UpdateWaterCoil( int const CoilNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   1998
		//       MODIFIED       April 2004: Rahul Chillar
		//                      Feb 2010 B. Griffith, plant upgrades
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the coil outlet nodes.

		// METHODOLOGY EMPLOYED:
		// Data is moved from the coil data structure to the coil outlet nodes.

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
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

		AirInletNode = WaterCoil( CoilNum ).AirInletNodeNum;
		WaterInletNode = WaterCoil( CoilNum ).WaterInletNodeNum;
		AirOutletNode = WaterCoil( CoilNum ).AirOutletNodeNum;
		WaterOutletNode = WaterCoil( CoilNum ).WaterOutletNodeNum;

		// Set the outlet air nodes of the WaterCoil
		Node( AirOutletNode ).MassFlowRate = WaterCoil( CoilNum ).OutletAirMassFlowRate;
		Node( AirOutletNode ).Temp = WaterCoil( CoilNum ).OutletAirTemp;
		Node( AirOutletNode ).HumRat = WaterCoil( CoilNum ).OutletAirHumRat;
		Node( AirOutletNode ).Enthalpy = WaterCoil( CoilNum ).OutletAirEnthalpy;

		Node( WaterOutletNode ).Temp = WaterCoil( CoilNum ).OutletWaterTemp;
		Node( WaterOutletNode ).Enthalpy = WaterCoil( CoilNum ).OutletWaterEnthalpy;

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

	//        End of Update subroutines for the WaterCoil Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the WaterCoil Module
	// *****************************************************************************

	void
	ReportWaterCoil( int const CoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variable for the coils.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReportWaterCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RhoWater;
		Real64 Tavg;
		Real64 SpecHumOut;
		Real64 SpecHumIn;
		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;
		// report the WaterCoil energy from this component
		WaterCoil( CoilNum ).TotWaterHeatingCoilEnergy = WaterCoil( CoilNum ).TotWaterHeatingCoilRate * ReportingConstant;
		WaterCoil( CoilNum ).TotWaterCoolingCoilEnergy = WaterCoil( CoilNum ).TotWaterCoolingCoilRate * ReportingConstant;
		WaterCoil( CoilNum ).SenWaterCoolingCoilEnergy = WaterCoil( CoilNum ).SenWaterCoolingCoilRate * ReportingConstant;

		// report the WaterCoil water collection to water storage tank (if needed)

		if ( WaterCoil( CoilNum ).CondensateCollectMode == CondensateToTank ) {
			// calculate and report condensation rates  (how much water extracted from the air stream)
			// water volumetric flow of water in m3/s for water system interactions
			//  put here to catch all types of DX coils
			Tavg = ( WaterCoil( CoilNum ).InletAirTemp - WaterCoil( CoilNum ).OutletAirTemp ) / 2.0;

			RhoWater = GetDensityGlycol( PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidName, Tavg, PlantLoop( WaterCoil( CoilNum ).WaterLoopNum ).FluidIndex, RoutineName );
			//   CR9155 Remove specific humidity calculations
			SpecHumIn = WaterCoil( CoilNum ).InletAirHumRat;
			SpecHumOut = WaterCoil( CoilNum ).OutletAirHumRat;
			//  mdot * del HumRat / rho water
			WaterCoil( CoilNum ).CondensateVdot = max( 0.0, ( WaterCoil( CoilNum ).InletAirMassFlowRate * ( SpecHumIn - SpecHumOut ) / RhoWater ) );
			WaterCoil( CoilNum ).CondensateVol = WaterCoil( CoilNum ).CondensateVdot * ReportingConstant;

			WaterStorage( WaterCoil( CoilNum ).CondensateTankID ).VdotAvailSupply( WaterCoil( CoilNum ).CondensateTankSupplyARRID ) = WaterCoil( CoilNum ).CondensateVdot;
			WaterStorage( WaterCoil( CoilNum ).CondensateTankID ).TwaterSupply( WaterCoil( CoilNum ).CondensateTankSupplyARRID ) = WaterCoil( CoilNum ).OutletAirTemp;

		}

	}

	//        End of Reporting subroutines for the WaterCoil Module
	// *****************************************************************************

	// Beginning of Coil Utility subroutines for the Detailed Model
	// *****************************************************************************

	void
	CalcDryFinEffCoef(
		Real64 const OutTubeEffFinDiamRatio,
		Array1< Real64 > & PolynomCoef
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR   Unknown
		//       DATE WRITTEN   Unknown
		//       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// The following subroutines are used once per cooling coil
		// simulation to obtain the coefficients of the dry fin
		// efficiency equation.  CalcDryFinEffCoef is the main calling
		// routine which manages calls to the Bessel funtion and polynomial
		// fit routines.

		// REFERENCES:
		// First found in MODSIM.
		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array2D< Real64 > OrderedPair( MaxOrderedPairs, 2 ); //Tuned Changed to static: Set before use
		Real64 FAI;
		Real64 FED;
		Real64 FEDnumerator;
		int I;
		int IE1;
		int IE2;
		int IE3;
		int IE4;
		int IE5;
		int IE6;
		Real64 R1;
		Real64 R1I1;
		Real64 R1K1;
		Real64 R2;
		Real64 R2I0;
		Real64 R2I1;
		Real64 R2K0;
		Real64 R2K1;
		Real64 RO;

		FAI = 0.02;
		for ( I = 1; I <= MaxOrderedPairs; ++I ) {
			FAI += 0.035;
			R1 = FAI / ( 1.0 - OutTubeEffFinDiamRatio );
			R2 = R1 * OutTubeEffFinDiamRatio;
			RO = 2.0 * OutTubeEffFinDiamRatio / ( FAI * ( 1.0 + OutTubeEffFinDiamRatio ) );
			CalcIBesselFunc( R1, 1, R1I1, IE1 );
			CalcKBesselFunc( R2, 1, R2K1, IE2 );
			CalcIBesselFunc( R2, 1, R2I1, IE3 );
			CalcKBesselFunc( R1, 1, R1K1, IE4 );
			CalcIBesselFunc( R2, 0, R2I0, IE5 );
			CalcKBesselFunc( R2, 0, R2K0, IE6 );
			FEDnumerator = RO * ( R1I1 * R2K1 - R2I1 * R1K1 );
			if ( FEDnumerator != 0.0 ) {
				FED = FEDnumerator / ( R2I0 * R1K1 + R1I1 * R2K0 );
			} else {
				FED = 0.0;
			}
			//      FED = RO * (R1I1 * R2K1 - R2I1 * R1K1) / (R2I0 * R1K1 + R1I1 * R2K0)
			OrderedPair( I, 1 ) = FAI;
			OrderedPair( I, 2 ) = FED;
		}
		CalcPolynomCoef( OrderedPair, PolynomCoef );
	}

	void
	CalcIBesselFunc(
		Real64 const BessFuncArg,
		int const BessFuncOrd,
		Real64 & IBessFunc,
		int & ErrorCode
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR   Unknown
		//       DATE WRITTEN   Unknown
		//       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the modified Bessel Function from order 0 to BessFuncOrd
		// BessFuncArg    ARGUMENT OF BESSEL FUNCTION
		// BessFuncOrd    ORDER OF BESSEL FUNCTION, GREATER THAN OR EQUAL TO ZERO
		// IBessFunc   RESULTANT VALUE OF I BESSEL FUNCTION
		// ErrorCode  RESULTANT ERROR CODE:
		//       ErrorCode = 0   NO ERROR
		//       ErrorCode = 1   BessFuncOrd .LT. 0
		//       ErrorCode = 2   BessFuncArg .LT. 0
		//       ErrorCode = 3   IBessFunc .LT. 10**(-30),     IBessFunc IS SET TO 0
		//       ErrorCode = 4   BessFuncArg .GT. BessFuncOrd & BessFuncArg .GT. 90,  IBessFunc IS SET TO 10**38

		// REFERENCES:
		// First found in MODSIM.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrorTol( 1.0e-06 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopCount;

		Real64 FI;
		Real64 FK;
		Real64 TERM;

		ErrorCode = 0;
		IBessFunc = 1.0;
		if ( BessFuncArg == 0.0 && BessFuncOrd == 0 ) return;

		if ( BessFuncOrd < 0 ) {
			ErrorCode = 1;
			return;
		} else if ( BessFuncArg < 0.0 ) {
			ErrorCode = 2;
			return;
		} else if ( BessFuncArg > 12.0 && BessFuncArg > BessFuncOrd ) {
			if ( BessFuncArg > 90.0 ) {
				ErrorCode = 4;
				IBessFunc = 1.0e30;
				return;
			}
			TERM = 1.0;
			IBessFunc = 1.0;
			for ( LoopCount = 1; LoopCount <= 30; ++LoopCount ) { //Start of 1st LoopCount Loop
				if ( std::abs( TERM ) <= std::abs( ErrorTol * IBessFunc ) ) {
					IBessFunc *= std::exp( BessFuncArg ) / std::sqrt( 2.0 * Pi * BessFuncArg );
					return;
				}
				TERM *= 0.125 / BessFuncArg * ( pow_2( 2 * LoopCount - 1 ) - 4 * BessFuncOrd * BessFuncOrd ) / double( LoopCount );
				IBessFunc += TERM;
			} // End of 1st LoopCount loop
		}

		TERM = 1.0;
		if ( BessFuncOrd > 0 ) {
			for ( LoopCount = 1; LoopCount <= BessFuncOrd; ++LoopCount ) { //Start of 2nd LoopCount Loop
				FI = LoopCount;
				if ( std::abs( TERM ) < 1.0e-30 * FI / ( BessFuncArg * 2.0 ) ) {
					ErrorCode = 3;
					IBessFunc = 0.0;
					return;
				}
				TERM *= BessFuncArg / ( 2.0 * FI );
			} //End of 2nd LoopCount loop
		}

		IBessFunc = TERM;
		for ( LoopCount = 1; LoopCount <= 1000; ++LoopCount ) { //Start of 3rd LoopCount Loop
			if ( std::abs( TERM ) <= std::abs( IBessFunc * ErrorTol ) ) return;
			FK = LoopCount * ( BessFuncOrd + LoopCount );
			TERM *= pow_2( BessFuncArg ) / ( 4.0 * FK );
			IBessFunc += TERM;
		} //End of  3rd LoopCount loop

	}

	void
	CalcKBesselFunc(
		Real64 const BessFuncArg,
		int const BessFuncOrd,
		Real64 & KBessFunc,
		int & ErrorCode
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR   Unknown
		//       DATE WRITTEN   Unknown
		//       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the K Bessel Function for a given argument and
		// order
		//  BessFuncArg    THE ARGUMENT OF THE K BESSEL FUNCTION DESIRED
		//  BessFuncOrd    THE ORDER OF THE K BESSEL FUNCTION DESIRED
		//  KBessFunc   THE RESULTANT K BESSEL FUNCTION
		//  ErrorCode  RESULTANT ERROR CODE:
		//        ErrorCode=0  NO ERROR
		//        ErrorCode=1  BessFuncOrd IS NEGATIVE
		//        ErrorCode=2  BessFuncArg IS ZERO OR NEGATIVE
		//        ErrorCode=3  BessFuncArg .GT. 85, KBessFunc .LT. 10**-38; KBessFunc SET TO 0.
		//        ErrorCode=4  KBessFunc .GT. 10**38; KBessFunc SET TO 10**38
		// NOTE: BessFuncOrd MUST BE GREATER THAN OR EQUAL TO ZERO
		// METHOD:
		//  COMPUTES ZERO ORDER AND FIRST ORDER BESSEL FUNCTIONS USING
		//  SERIES APPROXIMATIONS AND THEN COMPUTES BessFuncOrd TH ORDER FUNCTION
		//  USING RECURRENCE RELATION.
		//  RECURRENCE RELATION AND POLYNOMIAL APPROXIMATION TECHNIQUE
		//  AS DESCRIBED BY A.J.M. HITCHCOCK, 'POLYNOMIAL APPROXIMATIONS
		//  TO BESSEL FUNCTIONS OF ORDER ZERO AND ONE AND TO RELATED
		//  FUNCTIONS,' M.T.A.C., V.11, 1957, PP. 86-88, AND G.BessFuncOrd. WATSON,
		//  'A TREATISE ON THE THEORY OF BESSEL FUNCTIONS,' CAMBRIDGE
		//  UNIVERSITY PRESS, 1958, P.62

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const GJMAX( 1.0e+38 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopCount;
		bool StopLoop;

		Real64 FACT;
		Real64 G0;
		Real64 G1;
		Real64 GJ;
		Real64 HJ;
		Array1D< Real64 > T( 12 );
		Real64 X2J;

		KBessFunc = 0.0;
		G0 = 0.0;
		GJ = 0.0;

		if ( BessFuncOrd < 0.0 ) {
			ErrorCode = 1;
			return;
		} else if ( BessFuncArg <= 0.0 ) {
			ErrorCode = 2;
			return;
		} else if ( BessFuncArg > 85.0 ) {
			ErrorCode = 3;
			KBessFunc = 0.0;
			return;
		}

		ErrorCode = 0;

		//     Use polynomial approximation if BessFuncArg > 1.

		if ( BessFuncArg > 1.0 ) {
			T( 1 ) = 1.0 / BessFuncArg;
			for ( LoopCount = 2; LoopCount <= 12; ++LoopCount ) {
				T( LoopCount ) = T( LoopCount - 1 ) / BessFuncArg;
			} //End of LoopCount Loop
			if ( BessFuncOrd != 1 ) {

				//     Compute K0 using polynomial approximation

				G0 = std::exp( -BessFuncArg ) * ( 1.2533141 - 0.1566642 * T( 1 ) + 0.08811128 * T( 2 ) - 0.09139095 * T( 3 ) + 0.1344596 * T( 4 ) - 0.2299850 * T( 5 ) + 0.3792410 * T( 6 ) - 0.5247277 * T( 7 ) + 0.5575368 * T( 8 ) - 0.4262633 * T( 9 ) + 0.2184518 * T( 10 ) - 0.06680977 * T( 11 ) + 0.009189383 * T( 12 ) ) * std::sqrt( 1.0 / BessFuncArg );
				if ( BessFuncOrd == 0 ) {
					KBessFunc = G0;
					return;
				}
			}

			//     Compute K1 using polynomial approximation

			G1 = std::exp( -BessFuncArg ) * ( 1.2533141 + 0.4699927 * T( 1 ) - 0.1468583 * T( 2 ) + 0.1280427 * T( 3 ) - 0.1736432 * T( 4 ) + 0.2847618 * T( 5 ) - 0.4594342 * T( 6 ) + 0.6283381 * T( 7 ) - 0.6632295 * T( 8 ) + 0.5050239 * T( 9 ) - 0.2581304 * T( 10 ) + 0.07880001 * T( 11 ) - 0.01082418 * T( 12 ) ) * std::sqrt( 1.0 / BessFuncArg );
			if ( BessFuncOrd == 1 ) {
				KBessFunc = G1;
				return;
			}
		} else {

			//     Use series expansion if BessFuncArg <= 1.

			if ( BessFuncOrd != 1 ) {

				//     Compute K0 using series expansion

				G0 = -( 0.5772157 + std::log( BessFuncArg / 2.0 ) );
				X2J = 1.0;
				FACT = 1.0;
				HJ = 0.0;
				for ( LoopCount = 1; LoopCount <= 6; ++LoopCount ) {
					X2J *= pow_2( BessFuncArg ) / 4.0;
					FACT *= pow_2( 1.0 / double( LoopCount ) );
					HJ += 1.0 / double( LoopCount );
					G0 += X2J * FACT * ( HJ - ( 0.5772157 + std::log( BessFuncArg / 2.0 ) ) );
				} //End of LoopCount Loop
				if ( BessFuncOrd == 0.0 ) {
					KBessFunc = G0;
					return;
				}
			}

			//     Compute K1 using series expansion

			X2J = BessFuncArg / 2.0;
			FACT = 1.0;
			HJ = 1.0;
			G1 = 1.0 / BessFuncArg + X2J * ( 0.5 + ( 0.5772157 + std::log( BessFuncArg / 2.0 ) ) - HJ );
			for ( LoopCount = 2; LoopCount <= 8; ++LoopCount ) {
				X2J *= pow_2( BessFuncArg ) / 4.0;
				FACT *= pow_2( 1.0 / double( LoopCount ) );
				HJ += 1.0 / double( LoopCount );
				G1 += X2J * FACT * ( 0.5 + ( ( 0.5772157 + std::log( BessFuncArg / 2.0 ) ) - HJ ) * double( LoopCount ) );
			} //End of LoopCount Loop
			if ( BessFuncOrd == 1 ) {
				KBessFunc = G1;
				return;
			}
		}

		//     From K0 and K1 compute KN using recurrence relation

		LoopCount = 2;
		StopLoop = false;
		while ( LoopCount <= BessFuncOrd && ! StopLoop ) {
			GJ = 2.0 * ( double( LoopCount ) - 1.0 ) * G1 / BessFuncArg + G0;
			if ( GJ - GJMAX > 0.0 ) {
				ErrorCode = 4;
				GJ = GJMAX;
				StopLoop = true;
			} else {
				G0 = G1;
				G1 = GJ;
				++LoopCount;
			}
		} //End of LoopCount Loop
		KBessFunc = GJ;

	}

	void
	CalcPolynomCoef(
		Array2< Real64 > const & OrderedPair,
		Array1< Real64 > & PolynomCoef
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR   Unknown
		//       DATE WRITTEN   Unknown
		//       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// Fits polynomial of order from 1 to MaxPolynomOrder to the
		// ordered pairs of data points X,Y

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool Converged;
		static Array2D< Real64 > OrdPairSum( 10, 2 ); //Tuned Changed to static and whole array zero-initialized
		static Array2D< Real64 > OrdPairSumMatrix( 10, 10 ); //Tuned Changed to static
		Real64 B;
		int I;
		int II;
		int J;
		int PolynomOrder;
		int CurrentOrder;
		int CurrentOrdPair;
		Real64 S1;
		Real64 S2;

		OrdPairSum = 0.0;
		OrdPairSum( 1, 1 ) = MaxOrderedPairs;
		PolynomCoef = 0.0;
		for ( CurrentOrdPair = 1; CurrentOrdPair <= MaxOrderedPairs; ++CurrentOrdPair ) {
			OrdPairSum( 2, 1 ) += OrderedPair( CurrentOrdPair, 1 );
			OrdPairSum( 3, 1 ) += OrderedPair( CurrentOrdPair, 1 ) * OrderedPair( CurrentOrdPair, 1 );
			OrdPairSum( 1, 2 ) += OrderedPair( CurrentOrdPair, 2 );
			OrdPairSum( 2, 2 ) += OrderedPair( CurrentOrdPair, 1 ) * OrderedPair( CurrentOrdPair, 2 );
		}
		PolynomOrder = 1;
		Converged = false;
		while ( ! Converged ) {
			for ( CurrentOrder = 1; CurrentOrder <= PolynomOrder + 1; ++CurrentOrder ) {
				for ( J = 1; J <= PolynomOrder + 1; ++J ) {
					OrdPairSumMatrix( J, CurrentOrder ) = OrdPairSum( J - 1 + CurrentOrder, 1 );
				} //End of J loop
				OrdPairSumMatrix( PolynomOrder + 2, CurrentOrder ) = OrdPairSum( CurrentOrder, 2 );
			} //End of CurrentOrder loop

			for ( CurrentOrder = 1; CurrentOrder <= PolynomOrder + 1; ++CurrentOrder ) {
				OrdPairSumMatrix( CurrentOrder, PolynomOrder + 2 ) = -1.0;
				for ( J = CurrentOrder + 1; J <= PolynomOrder + 2; ++J ) {
					OrdPairSumMatrix( J, PolynomOrder + 2 ) = 0.0;
				} //End of J loop

				for ( II = 2; II <= PolynomOrder + 2; ++II ) {
					for ( J = CurrentOrder + 1; J <= PolynomOrder + 2; ++J ) {
						OrdPairSumMatrix( J, II ) -= OrdPairSumMatrix( J, 1 ) * OrdPairSumMatrix( CurrentOrder, II ) / OrdPairSumMatrix( CurrentOrder, 1 );
					} //End of J loop
				} //End of II loop
				for ( II = 1; II <= PolynomOrder + 1; ++II ) {
					for ( J = CurrentOrder + 1; J <= PolynomOrder + 2; ++J ) {
						OrdPairSumMatrix( J, II ) = OrdPairSumMatrix( J, II + 1 );
					} //End of J loop
				} //End of II loop
			} //End of CurrentOrder loop

			S2 = 0.0;
			for ( CurrentOrdPair = 1; CurrentOrdPair <= MaxOrderedPairs; ++CurrentOrdPair ) {
				S1 = OrdPairSumMatrix( PolynomOrder + 2, 1 );
				auto const OrderedPair1C( OrderedPair( CurrentOrdPair, 1 ) );
				auto OrderedPair1C_pow( 1.0 );
				for ( CurrentOrder = 1; CurrentOrder <= PolynomOrder; ++CurrentOrder ) {
					OrderedPair1C_pow *= OrderedPair1C;
					S1 += OrdPairSumMatrix( PolynomOrder + 2, CurrentOrder + 1 ) * OrderedPair1C_pow;
				} //End of CurrentOrder loop
				S2 += ( S1 - OrderedPair( CurrentOrdPair, 2 ) ) * ( S1 - OrderedPair( CurrentOrdPair, 2 ) );
			} //End of CurrentOrdPair loop
			B = MaxOrderedPairs - ( PolynomOrder + 1 );
			if ( S2 > 0.0001 ) S2 = std::sqrt( S2 / B );
			for ( CurrentOrder = 1; CurrentOrder <= PolynomOrder + 1; ++CurrentOrder ) {
				PolynomCoef( CurrentOrder ) = OrdPairSumMatrix( PolynomOrder + 2, CurrentOrder );
			} //End of CurrentOrder loop

			if ( ( PolynomOrder - MaxPolynomOrder < 0 ) && ( S2 - PolyConvgTol > 0.0 ) ) {
				++PolynomOrder;
				J = 2 * PolynomOrder;
				OrdPairSum( J, 1 ) = OrdPairSum( J + 1, 1 ) = 0.0;
				auto OrdPairSum2P = OrdPairSum( PolynomOrder + 1, 2 ) = 0.0;
				for ( I = 1; I <= MaxOrderedPairs; ++I ) {
					auto const OrderedPair1I( OrderedPair( I, 1 ) );
					auto OrderedPair_pow( std::pow( OrderedPair1I, J - 1 ) );
					OrdPairSum( J, 1 ) += OrderedPair_pow;
					OrderedPair_pow *= OrderedPair1I;
					OrdPairSum( J + 1, 1 ) += OrderedPair_pow;
					OrdPairSum2P += OrderedPair( I, 2 ) * std::pow( OrderedPair1I, PolynomOrder );
				}
				OrdPairSum( PolynomOrder + 1, 2 ) = OrdPairSum2P;
			} else {
				Converged = true;
			}

		}

	}

	Real64
	SimpleHeatingCoilUAResidual(
		Real64 const UA, // UA of coil
		Array1< Real64 > const & Par // par(1) = design coil load [W]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   November 2001
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Design Coil Load - Coil Heating Output) / Design Coil Load.
		// Coil Heating Output depends on the UA which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Puts UA into the water coil data structure, calls CalcSimpleHeatingCoil, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		using DataSizing::DataDesignCoilCapacity; // Data variable used in eq component sizing routines

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex;
		int FanOpMode;
		Real64 PartLoadRatio;

		CoilIndex = int( Par( 2 ) );
		FanOpMode = ( Par( 3 ) == 1.0 ? CycFanCycCoil : ContFanCycCoil );
		PartLoadRatio = Par( 4 );
		WaterCoil( CoilIndex ).UACoilVariable = UA;
		CalcSimpleHeatingCoil( CoilIndex, FanOpMode, PartLoadRatio, SimCalc );
		Residuum = ( Par( 1 ) - WaterCoil( CoilIndex ).TotWaterHeatingCoilRate ) / Par( 1 );
		DataDesignCoilCapacity = WaterCoil ( CoilIndex ).TotWaterHeatingCoilRate;

		return Residuum;
	}

	Real64
	SimpleCoolingCoilUAResidual(
		Real64 const UA, // UA of coil
		Array1< Real64 > const & Par // par(1) = design coil load [W]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2011
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Design Coil Load - Coil Cooling Output) / Design Coil Load.
		// Coil Cooling Output depends on the UA which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Puts UA into the water coil data structure, calls CoolingCoil, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex;
		int FanOpMode;
		Real64 PartLoadRatio;

		CoilIndex = int( Par( 2 ) );
		FanOpMode = ( Par( 3 ) == 1.0 ? CycFanCycCoil : ContFanCycCoil );
		PartLoadRatio = Par( 4 );
		WaterCoil( CoilIndex ).UACoilExternal = UA;
		WaterCoil( CoilIndex ).UACoilInternal = WaterCoil( CoilIndex ).UACoilExternal * 3.3;
		WaterCoil( CoilIndex ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilIndex ).UACoilExternal + 1.0 / WaterCoil( CoilIndex ).UACoilInternal );
		WaterCoil( CoilIndex ).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea( CoilIndex );
		WaterCoil( CoilIndex ).UACoilInternalPerUnitArea = WaterCoil( CoilIndex ).UACoilInternal / WaterCoil( CoilIndex ).TotCoilOutsideSurfArea;
		WaterCoil( CoilIndex ).UAWetExtPerUnitArea = WaterCoil( CoilIndex ).UACoilExternal / WaterCoil( CoilIndex ).TotCoilOutsideSurfArea;
		WaterCoil( CoilIndex ).UADryExtPerUnitArea = WaterCoil( CoilIndex ).UAWetExtPerUnitArea;

		CoolingCoil( CoilIndex, true, DesignCalc, FanOpMode, PartLoadRatio );

		Residuum = ( Par( 1 ) - WaterCoil( CoilIndex ).TotWaterCoolingCoilRate ) / Par( 1 );

		return Residuum;
	}

	// Iterate Routine for Cooling Coil

	void
	CoilAreaFracIter(
		Real64 & NewSurfAreaWetFrac, // Out Value of variable
		Real64 const SurfAreaFracCurrent, // Driver Value
		Real64 const ErrorCurrent, // Objective Function
		Real64 & SurfAreaFracPrevious, // First Previous value of Surf Area Fraction
		Real64 & ErrorPrevious, // First Previous value of error
		Real64 & SurfAreaFracLast, // Second Previous value of Surf Area Fraction
		Real64 & ErrorLast, // Second Previous value of error
		int const IterNum, // Number of Iterations
		int & icvg // Iteration convergence flag
	)
	{
		// FUNCTION INFORMATION:
		// AUTHOR         Rahul Chillar
		// DATE WRITTEN   June 2004
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Iterately solves for the value of SurfAreaWetFraction for the Cooling Coil.

		// METHODOLOGY EMPLOYED:
		// First function generates 2 sets of guess points by perturbation and subsequently
		// by Linear Fit and using the generated points calculates coeffecients for Quadratic
		// fit to predict the next value of surface area wet fraction.

		// REFERENCES:
		// ME 423 Design of Thermal Systems Class Notes.UIUC. W.F.Stoecker

		// USE STATEMENTS:
		// na

		// Enforce explicit typing of all variables in this routine

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const Tolerance( 1.e-5 ); // Relative error tolerance
		Real64 const PerturbSurfAreaFrac( 0.1 ); // Perturbation applied to Surf Fraction to initialize iteration
		Real64 const SmallNum( 1.e-9 ); // Small Number

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 check; // Validity Check for moving to Quad Solution
		Real64 QuadCoefThree; // Term under radical in quadratic solution
		Real64 QuadCoefOne; // Term under radical in quadratic solution
		Real64 QuadCoefTwo; // Term under radical in quadratic solution
		Real64 Slope; // Slope for linear fit
		Real64 SurfAreaFracOther; // Intermediate Value of Surf Area
		int mode; // Linear/ perturbation option

		// Convergence Check  by comparing previous and current value of surf area fraction
		if ( ( std::abs( SurfAreaFracCurrent - SurfAreaFracPrevious ) < Tolerance * max( std::abs( SurfAreaFracCurrent ), SmallNum ) && IterNum != 1 ) || ErrorCurrent == 0.0 ) {
			// Setting value for surface area fraction for coil
			NewSurfAreaWetFrac = SurfAreaFracCurrent;
			icvg = 1; // Convergance Flag
			return;
		}

		// If Icvg = 0 , it has not converged.By perturbation for getting second set of
		// data (mode=1), Getting Third set of data by performing a  linear fit(Mode=2).
		// Now using the above 3 points generated by perturbation and Linear Fit to perform
		// a quadratic fit.This will happen after second iteration only.
		icvg = 0; // Convergance flag = false
		// For First Iteration Start with perturbation, For second iteration start with linear fit
		// from the previous two values
		mode = IterNum;

Label10: ;
		if ( mode == 1 ) {

			// FirstGuess Set of Points provided by perturbation
			if ( std::abs( SurfAreaFracCurrent ) > SmallNum ) {
				NewSurfAreaWetFrac = SurfAreaFracCurrent * ( 1.0 + PerturbSurfAreaFrac );
			} else {
				NewSurfAreaWetFrac = PerturbSurfAreaFrac;
			}

			// Second set of values being calculated from the first set of values (incoming & perturb)
		} else if ( mode == 2 ) {

			// Calculating Slope for interpolating to the New Point (Simple Linear Extrapolation)
			Slope = ( ErrorPrevious - ErrorCurrent ) / ( SurfAreaFracPrevious - SurfAreaFracCurrent );
			// Error Check for value or Slope
			if ( Slope == 0.0 ) {
				mode = 1; // Go back to Perturbation
				goto Label10;
			}
			// Guessing New Value for Surface Area Fraction
			NewSurfAreaWetFrac = SurfAreaFracCurrent - ErrorCurrent / Slope;
		} else {

			// Check for Quadratic Fit possible here ,Previous value of surf area fraction
			// equals current value then Try linear fit for another point.
			if ( SurfAreaFracCurrent == SurfAreaFracPrevious ) {
				// Assign Value of previous point to Last Variable for storing
				// Go back and calculate new value for Previous.
				SurfAreaFracPrevious = SurfAreaFracLast;
				ErrorPrevious = ErrorLast;
				mode = 2;
				goto Label10;
			} else if ( SurfAreaFracCurrent == SurfAreaFracLast ) {
				// Calculate another value using Linear Fit.
				mode = 2;
				goto Label10;
			}

			// Now We have enough previous points to calculate coefficients and
			// perform a quadratic fit for new guess value of surface area fraction

			// Calculating First Coefficients for Quadratic Curve Fit
			QuadCoefThree = ( ( ErrorLast - ErrorCurrent ) / ( SurfAreaFracLast - SurfAreaFracCurrent ) - ( ErrorPrevious - ErrorCurrent ) / ( SurfAreaFracPrevious - SurfAreaFracCurrent ) ) / ( SurfAreaFracLast - SurfAreaFracPrevious );
			// Calculating Second Coefficients for Quadratic Curve Fit
			QuadCoefTwo = ( ErrorPrevious - ErrorCurrent ) / ( SurfAreaFracPrevious - SurfAreaFracCurrent ) - ( SurfAreaFracPrevious + SurfAreaFracCurrent ) * QuadCoefThree;

			// Calculating Third Coefficients for Quadratic Curve Fit
			QuadCoefOne = ErrorCurrent - ( QuadCoefTwo + QuadCoefThree * SurfAreaFracCurrent ) * SurfAreaFracCurrent;

			// Check for validity of coefficients , if not REAL(r64) ,Then fit is linear
			if ( std::abs( QuadCoefThree ) < 1.E-10 ) {
				mode = 2; // going to Linear mode, due to colinear points.
				goto Label10;
			}

			// If value of Quadratic coefficients not suitable enought due to round off errors
			// to predict new point go to linear fit and acertain new values for the coefficients.
			if ( std::abs( ( QuadCoefOne + ( QuadCoefTwo + QuadCoefThree * SurfAreaFracPrevious ) * SurfAreaFracPrevious - ErrorPrevious ) / ErrorPrevious ) > 1.E-4 ) {
				mode = 2; // go to linear mode
				goto Label10;
			}

			// Validity Check for Imaginary roots, In this case go back to linear fit.
			check = pow_2( QuadCoefTwo ) - 4.0 * QuadCoefOne * QuadCoefThree;
			// Imaginary Root Exist
			if ( check < 0 ) {
				mode = 2;
				goto Label10;
			} else if ( check > 0 ) {
				// real unequal roots exist, Determine the roots nearest to most recent guess
				NewSurfAreaWetFrac = ( -QuadCoefTwo + std::sqrt( check ) ) / QuadCoefThree / 2.0;
				SurfAreaFracOther = -NewSurfAreaWetFrac - QuadCoefTwo / QuadCoefThree;
				// Assigning value to Surface Area Fraction with recent
				if ( std::abs( NewSurfAreaWetFrac - SurfAreaFracCurrent ) > std::abs( SurfAreaFracOther - SurfAreaFracCurrent ) ) NewSurfAreaWetFrac = SurfAreaFracOther;
			} else {
				// The roots are real, one solution exists.
				NewSurfAreaWetFrac = -QuadCoefTwo / QuadCoefThree / 2;
			}

		}

		if ( mode < 3 ) {
			// No valid previous points to eliminate, since it just has 2 points.
			// Loading previous values into last
			SurfAreaFracLast = SurfAreaFracPrevious;
			ErrorLast = ErrorPrevious;
			// Loading Current Values into previous
			SurfAreaFracPrevious = SurfAreaFracCurrent;
			ErrorPrevious = ErrorCurrent;
		} else {

			// Elimination the most distance previous point from the answer based on sign and
			// magnitute of the error. Keeping Current Point
			if ( ErrorPrevious * ErrorCurrent > 0 && ErrorLast * ErrorCurrent > 0 ) {
				// If sign are same , simply eliminate the one with biggest error value.
				if ( std::abs( ErrorLast ) > std::abs( ErrorPrevious ) ) {
					// Eliminating Last Value
					SurfAreaFracLast = SurfAreaFracPrevious;
					ErrorLast = ErrorPrevious;
				}
			} else {
				// If signs are different eliminate previous error with same sign as current error
				if ( ErrorLast * ErrorCurrent > 0 ) {
					// Previous Loaded to Last
					SurfAreaFracLast = SurfAreaFracPrevious;
					ErrorLast = ErrorPrevious;
				}
			}
			// Current Loaded into previous.
			SurfAreaFracPrevious = SurfAreaFracCurrent;
			ErrorPrevious = ErrorCurrent;
		}

	}

	void
	CheckWaterCoilSchedule(
		std::string const & EP_UNUSED( CompType ), // unused1208
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
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

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
		int CoilNum;

		// Obtains and Allocates WaterCoil related parameters from input file
		if ( GetWaterCoilsInputFlag ) { //First time subroutine has been entered
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		// Find the correct Coil number
		if ( CompIndex == 0 ) {
			CoilNum = FindItemInList( CompName, WaterCoil );
			if ( CoilNum == 0 ) {
				ShowFatalError( "CheckWaterCoilSchedule: Coil not found=" + CompName );
			}
			CompIndex = CoilNum;
			Value = GetCurrentScheduleValue( WaterCoil( CoilNum ).SchedPtr ); // not scheduled?
		} else {
			CoilNum = CompIndex;
			if ( CoilNum > NumWaterCoils || CoilNum < 1 ) {
				ShowFatalError( "CheckWaterCoilSchedule: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Number of Heating Coils=" + TrimSigDigits( NumWaterCoils ) + ", Coil name=" + CompName );
			}
			if ( CompName != WaterCoil( CoilNum ).Name ) {
				ShowFatalError( "CheckWaterCoilSchedule: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + WaterCoil( CoilNum ).Name );
			}
			Value = GetCurrentScheduleValue( WaterCoil( CoilNum ).SchedPtr ); // not scheduled?
		}

	}

	Real64
	GetCoilMaxWaterFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the max water flow rate for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		Real64 MaxWaterFlowRate; // returned max water flow rate of matched coil

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

		// Obtains and Allocates WaterCoil related parameters from input file
		if ( GetWaterCoilsInputFlag ) { //First time subroutine has been entered
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		WhichCoil = 0;
		if ( SameString( CoilType, "Coil:Heating:Water" ) || SameString( CoilType, "Coil:Cooling:Water:DetailedGeometry" ) || SameString( CoilType, "Coil:Cooling:Water" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				// coil does not specify MaxWaterFlowRate
				MaxWaterFlowRate = WaterCoil( WhichCoil ).MaxWaterVolFlowRate;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ShowContinueError( "... Max Water Flow rate returned as -1000." );
			ErrorsFound = true;
			MaxWaterFlowRate = -1000.0;
		}

		return MaxWaterFlowRate;

	}

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   March 2007
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
		using InputProcessor::SameString;

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

		// Obtains and Allocates DXCoils
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		NodeNumber = 0;
		WhichCoil = 0;
		if ( SameString( CoilType, "Coil:Heating:Water" ) || SameString( CoilType, "Coil:Cooling:Water:DetailedGeometry" ) || SameString( CoilType, "Coil:Cooling:Water" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = WaterCoil( WhichCoil ).AirInletNodeNum;
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
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   March 2007
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
		using InputProcessor::SameString;

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

		// Obtains and Allocates DXCoils
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		WhichCoil = 0;
		NodeNumber = 0;
		if ( SameString( CoilType, "Coil:Heating:Water" ) || SameString( CoilType, "Coil:Cooling:Water:DetailedGeometry" ) || SameString( CoilType, "Coil:Cooling:Water" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = WaterCoil( WhichCoil ).AirOutletNodeNum;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\" when accessing coil outlet node number." );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilWaterInletNode(
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
		// This function looks up the given coil and returns the inlet water control node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

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

		// Obtains and Allocates DXCoils
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		NodeNumber = 0;
		WhichCoil = 0;
		if ( SameString( CoilType, "Coil:Heating:Water" ) || SameString( CoilType, "Coil:Cooling:Water:DetailedGeometry" ) || SameString( CoilType, "Coil:Cooling:Water" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = WaterCoil( WhichCoil ).WaterInletNodeNum;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilWaterInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilWaterOutletNode(
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
		// This function looks up the given coil and returns the outlet water node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

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

		// Obtains and Allocates DXCoils
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		NodeNumber = 0;
		WhichCoil = 0;
		if ( SameString( CoilType, "Coil:Heating:Water" ) || SameString( CoilType, "Coil:Cooling:Water:DetailedGeometry" ) || SameString( CoilType, "Coil:Cooling:Water" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				NodeNumber = WaterCoil( WhichCoil ).WaterOutletNodeNum;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilWaterOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	void
	SetCoilDesFlow(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		Real64 const CoilDesFlow, // coil volumetric air flow rate [m3/s]
		bool & ErrorsFound // set to true if problem
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine is designed to set the design air volume flow rate in the
		// water coil data structure. Some of the coil types do not have this datum as
		// an input parameter and it is needed for calculating capacity for output reporting.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichCoil; // index to coil

		if ( GetWaterCoilsInputFlag ) { //First time subroutine has been entered
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:Water" ) || SameString( CoilType, "Coil:Cooling:Water:DetailedGeometry" ) || SameString( CoilType, "Coil:Cooling:Water" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				if ( SameString( CoilType, "Coil:Cooling:Water" ) && WaterCoil( WhichCoil ).DesAirVolFlowRate < 0.0 ) {
					WaterCoil( WhichCoil ).DesAirVolFlowRate = CoilDesFlow;
				} else {
					WaterCoil( WhichCoil ).DesAirVolFlowRate = CoilDesFlow;
				}
			} else {
				ShowSevereError( "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
				ErrorsFound = true;
			}
		}

	}

	Real64
	GetWaterCoilDesAirFlow(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine is designed to set the design air volume flow rate in the
		// water coil data structure. Some of the coil types do not have this datum as
		// an input parameter and it is needed for calculating capacity for output reporting.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichCoil; // index to coil
		Real64 CoilDesAirFlow;

		CoilDesAirFlow = 0.0;

		if ( GetWaterCoilsInputFlag ) { //First time subroutine has been entered
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Cooling:Water" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				CoilDesAirFlow = WaterCoil( WhichCoil ).DesAirVolFlowRate;
			} else {
				ShowSevereError( "GetWaterCoilDesAirFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
				ErrorsFound = true;
			}
		} else {
			ShowSevereError( "GetWaterCoilDesAirFlowRate: Funciton not valid for Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
		}

		return CoilDesAirFlow;
	}

	void
	CheckActuatorNode(
		int const ActuatorNodeNum, // input actuator node number
		int & iNodeType, // Cooling or Heating or 0
		bool & NodeNotFound // true if matching water inlet node not found
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This subroutine checks that the input actuator node number is matched by
		// the water inlet node number of some water coil

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

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
		int CoilNum;

		// Obtains and Allocates DXCoils
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		WhichCoil = 0;
		iNodeType = 0;
		NodeNotFound = true;
		for ( CoilNum = 1; CoilNum <= NumWaterCoils; ++CoilNum ) {
			if ( WaterCoil( CoilNum ).WaterInletNodeNum == ActuatorNodeNum ) {
				WhichCoil = CoilNum;
				iNodeType = WaterCoil( CoilNum ).WaterCoilType;
				NodeNotFound = false;
			}
		}

	}

	void
	CheckForSensorAndSetPointNode(
		int const SensorNodeNum, // controller sensor node number
		int const ControlledVar, // controlled variable type
		bool & NodeNotFound // true if matching air outlet node not found
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks that the sensor node number matches the air outlet node number
		// of some water coils

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using SetPointManager::NodeHasSPMCtrlVarType;
		using SetPointManager::iCtrlVarType_Temp;
		using SetPointManager::iCtrlVarType_MaxHumRat;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::iHumidityRatioMaxSetPoint;

		//USE HVACControllers,     ONLY: iTemperature, iHumidityRatio, iTemperatureAndHumidityRatio

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CheckForSensorAndSetpointNode: " );
		int const iTemperature( 1 );
		int const iHumidityRatio( 2 );
		int const iTemperatureAndHumidityRatio( 3 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichCoil; // water coil index
		int CoilNum; // counter
		std::string WaterCoilType; // water coil type
		bool EMSSetPointErrorFlag; // flag true is EMS is used to set node setpoints

		// Obtains and Allocates DXCoils
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		WhichCoil = 0;
		NodeNotFound = true;

		for ( CoilNum = 1; CoilNum <= NumWaterCoils; ++CoilNum ) {
			if ( SensorNodeNum != WaterCoil( CoilNum ).AirOutletNodeNum ) continue;
			NodeNotFound = false;
			WhichCoil = CoilNum;
			break;
		}
		// now if the sensor node is on the water coil air outlet node then check that
		// a setpoint is also specified on the water coil outlet node
		if ( ! NodeNotFound ) {
			if ( WhichCoil > 0 ) {
				if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_DetFlatFinCooling ) {
					WaterCoilType = "Coil:Cooling:Water:DetailedGeometry";
				} else if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_Cooling ) {
					WaterCoilType = "Coil:Cooling:Water";
				} else if ( WaterCoil( CoilNum ).WaterCoilType_Num == WaterCoil_SimpleHeating ) {
					WaterCoilType = "Coil:Heating:Water";
				}
				EMSSetPointErrorFlag = false;
				{ auto const SELECT_CASE_var( ControlledVar );
				if ( SELECT_CASE_var == iTemperature ) {
					CheckIfNodeSetPointManagedByEMS( SensorNodeNum, iTemperatureSetPoint, EMSSetPointErrorFlag );
					if ( EMSSetPointErrorFlag ) {
						if ( ! NodeHasSPMCtrlVarType( SensorNodeNum, iCtrlVarType_Temp ) ) {
							ShowWarningError( RoutineName + WaterCoilType + "=\"" + WaterCoil( WhichCoil ).Name + "\". " );
							ShowContinueError( " ..Temperature setpoint not found on coil air outlet node." );
							ShowContinueError( " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node." );
							ShowContinueError( " ..Specify the setpoint and the sensor on the coil air outlet node when possible." );
						}
					}
				} else if ( SELECT_CASE_var == iHumidityRatio ) {
					CheckIfNodeSetPointManagedByEMS( SensorNodeNum, iHumidityRatioMaxSetPoint, EMSSetPointErrorFlag );
					if ( EMSSetPointErrorFlag ) {
						if ( ! NodeHasSPMCtrlVarType( SensorNodeNum, iCtrlVarType_MaxHumRat ) ) {
							ShowWarningError( RoutineName + WaterCoilType + "=\"" + WaterCoil( WhichCoil ).Name + "\". " );
							ShowContinueError( " ..Humidity ratio setpoint not found on coil air outlet node." );
							ShowContinueError( " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node." );
							ShowContinueError( " ..Specify the setpoint and the sensor on the coil air outlet node when possible." );
						}
					}
				} else if ( SELECT_CASE_var == iTemperatureAndHumidityRatio ) {
					CheckIfNodeSetPointManagedByEMS( SensorNodeNum, iTemperatureSetPoint, EMSSetPointErrorFlag );
					if ( EMSSetPointErrorFlag ) {
						if ( ! NodeHasSPMCtrlVarType( SensorNodeNum, iCtrlVarType_Temp ) ) {
							ShowWarningError( RoutineName + WaterCoilType + "=\"" + WaterCoil( WhichCoil ).Name + "\". " );
							ShowContinueError( " ..Temperature setpoint not found on coil air outlet node." );
							ShowContinueError( " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node." );
							ShowContinueError( " ..Specify the setpoint and the sensor on the coil air outlet node when possible." );
						}
					}
					EMSSetPointErrorFlag = false;
					CheckIfNodeSetPointManagedByEMS( SensorNodeNum, iHumidityRatioMaxSetPoint, EMSSetPointErrorFlag );
					if ( EMSSetPointErrorFlag ) {
						if ( ! NodeHasSPMCtrlVarType( SensorNodeNum, iCtrlVarType_MaxHumRat ) ) {
							ShowWarningError( RoutineName + WaterCoilType + "=\"" + WaterCoil( WhichCoil ).Name + "\". " );
							ShowContinueError( " ..Humidity ratio setpoint not found on coil air outlet node." );
							ShowContinueError( " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node." );
							ShowContinueError( " ..Specify the setpoint and the sensor on the coil air outlet node when possible." );
						}
					}
				}}

			}
		}

	}

	Real64
	TdbFnHRhPb(
		Real64 const H, // specific enthalpy {J/kg}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Real64 const PB // barometric pressure {Pascals}
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 1, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given the specific enthalpy, relative humidity, and the
		// barometric pressure, the function returns the dry bulb temperature.

		// METHODOLOGY EMPLOYED:
		// Inverts PsyHFnTdbRhPb

		// REFERENCES:
		// none

		// Using/Aliasing
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;

		// Return value
		Real64 T; // result=> humidity ratio

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations
		Real64 const Acc( 1.0 ); // Accuracy of result

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int SolFla; // Flag of solver
		Real64 T0; // lower bound for Tprov [C]
		Real64 T1; // upper bound for Tprov [C]
		static Real64 Tprov( 0.0 ); // provisional value of drybulb temperature [C]
		Array1D< Real64 > Par( 3 ); // Par(1) = desired enthaply H [J/kg]
		// Par(2) = desired relative humidity (0.0 - 1.0)
		// Par(3) = barometric pressure [N/m2 (Pascals)]

		T0 = 1.0;
		T1 = 50.0;
		Par( 1 ) = H;
		Par( 2 ) = RH;
		Par( 3 ) = PB;
		SolveRegulaFalsi( Acc, MaxIte, SolFla, Tprov, EnthalpyResidual, T0, T1, Par );
		// if the numerical inversion failed, issue error messages.
		if ( SolFla == -1 ) {
			ShowSevereError( "Calculation of drybulb temperature failed in TdbFnHRhPb(H,RH,PB)" );
			ShowContinueError( "   Iteration limit exceeded" );
			ShowContinueError( "   H=[" + RoundSigDigits( H, 6 ) + "], RH=[" + RoundSigDigits( RH, 4 ) + "], PB=[" + RoundSigDigits( PB, 5 ) + "]." );
		} else if ( SolFla == -2 ) {
			ShowSevereError( "Calculation of drybulb temperature failed in TdbFnHRhPb(H,RH,PB)" );
			ShowContinueError( "  Bad starting values for Tdb" );
			ShowContinueError( "   H=[" + RoundSigDigits( H, 6 ) + "], RH=[" + RoundSigDigits( RH, 4 ) + "], PB=[" + RoundSigDigits( PB, 5 ) + "]." );
		}
		if ( SolFla < 0 ) {
			T = 0.0;
		} else {
			T = Tprov;
		}

		return T;
	}

	Real64
	EnthalpyResidual(
		Real64 const Tprov, // test value of Tdb [C]
		Array1< Real64 > const & Par // Par(1) = desired enthaply H [J/kg]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2009
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function Hdesired - H(Tdb,Rh,Pb)

		// METHODOLOGY EMPLOYED:
		// Calls PsyHFnTdbRhPb

		// REFERENCES:

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbRhPb;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired relative humidity (0.0 - 1.0)
		// Par(3) = barometric pressure [N/m2 (Pascals)]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Residuum = Par( 1 ) - PsyHFnTdbRhPb( Tprov, Par( 2 ), Par( 3 ) );

		return Residuum;
	}

	Real64
	EstimateHEXSurfaceArea( int const CoilNum ) // coil number, [-]
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket A Nigusse, FSEC
		//       DATE WRITTEN   July 2010
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Splits the UA value of a simple coil:cooling:water heat exchanger model into
		// "A" and U" values.

		// METHODOLOGY EMPLOYED:
		// A typical design U overall heat transfer coefficient is used to split the "UA" into "A"
		// and "U" values. Currently a constant U value calculated for a typical cooling coil is
		// used. The assumptions used to calculate a typical U value are:
		//     (1) tube side water velocity of 2.0 [m/s]
		//     (2) inside to outside total surface area ratio (Ai/Ao) =  0.07 [-]
		//     (3) fins overall efficiency = 0.92 based on aluminum fin, 12 fins per inch, and
		//         fins area to total outside surafce area ratio of about 90%.
		//     (4) air side convection coefficient of 140.0 [W/m2C].  Assumes sensible convection
		//         of 58.0 [W/m2C] and 82.0 [W/m2C] sensible convection equivalent of the mass
		//         transfer coefficient converted using the approximate relation:
		//         hequivalent = hmasstransfer/CpAir.

		// REFERENCES:

		// USE STATEMENTS:

		// Return value

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static Real64 const OverallFinEfficiency( 0.92 ); // Assumes aluminum fins, 12 fins per inch, fins
		// area of about 90% of external surface area Ao.

		static Real64 const AreaRatio( 0.07 ); // Heat exchanger Inside to Outside surface area ratio
		// design values range from (Ai/Ao) = 0.06 to 0.08

		// Constant value air side heat transfer coefficient is assumed. This coefficient has sensible
		// (58.d0 [W/m2C]) and latent (82.d0 [W/m2C]) heat transfer coefficient components.
		static Real64 const hAirTubeOutside( 58.0 + 82.0 ); // Air side heat transfer coefficient [W/m2C]

		// Tube side water convection heat transfer coefficient of the cooling coil is calculated for
		// inside tube diameter of 0.0122m (~0.5 inch nominal diameter) and water velocity 2.0 m/s:
		static Real64 const hWaterTubeInside( 1429.0 * std::pow( 2.0, 0.8 ) * std::pow( 0.0122, -0.2 ) ); // water (tube) side heat transfer coefficient [W/m2C]

		// Estimate the overall heat transfer coefficient, UOverallHeatTransferCoef in [W/(m2C)].
		// Neglecting tube wall and fouling resistance, the overall U value can be estimated as:
		// 1/UOverallHeatTransferCoef = 1/(hi*AreaRatio) + 1/(ho*OverallFinEfficiency)
		static Real64 const UOverallHeatTransferCoef_inv( 1.0 / ( hWaterTubeInside * AreaRatio ) + 1.0 / ( hAirTubeOutside * OverallFinEfficiency ) ); // Inverse of overall heat transfer coefficient for coil [W/m2C]

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		WaterCoil( CoilNum ).UACoilTotal = 1.0 / ( 1.0 / WaterCoil( CoilNum ).UACoilExternal + 1.0 / WaterCoil( CoilNum ).UACoilInternal );

		// the heat exchanger surface area is calculated as follows:
		return WaterCoil( CoilNum ).UACoilTotal * UOverallHeatTransferCoef_inv; // Heat exchanger surface area [m2]
	}

	int
	GetWaterCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Nigusse, FSEC
		//       DATE WRITTEN   Feb 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the index for the given coil and returns it.  If incorrect coil
		// type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int IndexNum; // returned coil index if matched coil

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

		// Obtains and allocates WaterCoil related parameters from input file
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		IndexNum = 0;
		if ( CoilType == "COIL:HEATING:WATER" ) {
			IndexNum = FindItemInList( CoilName, WaterCoil );
		} else if ( CoilType == "COIL:COOLING:WATER" ) {
			IndexNum = FindItemInList( CoilName, WaterCoil );
		} else if ( CoilType == "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) {
			IndexNum = FindItemInList( CoilName, WaterCoil );
		} else {
			IndexNum = 0;
		}

		if ( IndexNum == 0 ) {
			ShowSevereError( "GetWaterCoilIndex: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
		}

		return IndexNum;

	}

	Real64
	GetWaterCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad, FSEC
		//       DATE WRITTEN   Sep 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the capacity for the given coil and returns it.  If incorrect coil
		// type or name is given, ErrorsFound is returned as true and capacity is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Real64 Capacity; // returned coil capacity if matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int IndexNum; // index to water coil

		// Obtains and allocates WaterCoil related parameters from input file
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		Capacity = -1.0;

		if ( CoilType == "COIL:HEATING:WATER" ) {
			IndexNum = FindItemInList( CoilName, WaterCoil );
			Capacity = WaterCoil( IndexNum ).DesWaterHeatingCoilRate;
		} else if ( CoilType == "COIL:COOLING:WATER" ) {
			IndexNum = FindItemInList( CoilName, WaterCoil );
			Capacity = WaterCoil( IndexNum ).DesWaterCoolingCoilRate;
		} else if ( CoilType == "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) {
			IndexNum = FindItemInList( CoilName, WaterCoil );
			Capacity = WaterCoil( IndexNum ).DesWaterCoolingCoilRate;
		} else {
			IndexNum = 0;
		}

		if ( IndexNum == 0 ) {
			ShowSevereError( "GetWaterCoilCapacity: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
		}

		return Capacity;
	}

	void
	UpdateWaterToAirCoilPlantConnection(
		int const CoilTypeNum,
		std::string const & CoilName,
		int const EP_UNUSED( EquipFlowCtrl ), // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const EP_UNUSED( FirstHVACIteration ),
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   February 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update sim routine called from plant

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::KickOffSimulation;
		using DataLoopNode::Node;
		using DataPlant::ccSimPlantEquipTypes;
		using DataPlant::PlantLoop;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataHVACGlobals::SimAirLoopsFlag;
		using DataHVACGlobals::SimZoneEquipmentFlag;

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
		static bool DidAnythingChange( false ); // set to true if conditions changed
		int InletNodeNum;
		int OutletNodeNum;

		// Find the correct water coil
		if ( CompIndex == 0 ) {
			CoilNum = FindItemInList( CoilName, WaterCoil );
			if ( CoilNum == 0 ) {
				ShowFatalError( "UpdateWaterToAirCoilPlantConnection: Specified Coil not one of Valid water coils=" + CoilName );
			}
			CompIndex = CoilNum;
		} else {
			CoilNum = CompIndex;
			if ( CoilNum > NumWaterCoils || CoilNum < 1 ) {
				ShowFatalError( "UpdateWaterToAirCoilPlantConnection:  Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Number of Coils=" + TrimSigDigits( NumWaterCoils ) + ", Entered Coil name=" + CoilName );
			}
			if ( KickOffSimulation ) {
				if ( CoilName != WaterCoil( CoilNum ).Name ) {
					ShowFatalError( "UpdateWaterToAirCoilPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CoilName + ", stored Coil Name for that index=" + WaterCoil( CoilNum ).Name );
				}
				if ( CoilTypeNum != WaterCoil( CoilNum ).WaterCoilType_Num ) {
					ShowFatalError( "UpdateWaterToAirCoilPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( CoilNum ) + ", Coil name=" + CoilName + ", stored Coil Name for that index=" + ccSimPlantEquipTypes( CoilTypeNum ) );
				}
			}
		}

		if ( InitLoopEquip ) {
			return;
		}

		DidAnythingChange = false;

		InletNodeNum = WaterCoil( CoilNum ).WaterInletNodeNum;
		OutletNodeNum = WaterCoil( CoilNum ).WaterOutletNodeNum;

		if ( Node( InletNodeNum ).Temp != WaterCoil( CoilNum ).InletWaterTemp ) DidAnythingChange = true;

		if ( Node( OutletNodeNum ).Temp != WaterCoil( CoilNum ).OutletWaterTemp ) DidAnythingChange = true;

		if ( Node( InletNodeNum ).MassFlowRate != WaterCoil( CoilNum ).OutletWaterMassFlowRate ) {
			DidAnythingChange = true;
			Node( OutletNodeNum ).MassFlowRate = Node( InletNodeNum ).MassFlowRate; // make sure flows are consistent
		}

		if ( Node( OutletNodeNum ).MassFlowRate != WaterCoil( CoilNum ).OutletWaterMassFlowRate ) DidAnythingChange = true;

		if ( DidAnythingChange ) {
			// set sim flag for this loop
			PlantLoop( LoopNum ).LoopSide( LoopSide ).SimLoopSideNeeded = true;
			//set sim flags for air side users of coils

			SimAirLoopsFlag = true;
			SimZoneEquipmentFlag = true;
		} else { // nothing changed so turn off sim flag
			PlantLoop( LoopNum ).LoopSide( LoopSide ).SimLoopSideNeeded = false;
		}

	}

	int
	GetWaterCoilAvailScheduleIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
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
		using InputProcessor::SameString;

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

		// Obtains and Allocates HeatingCoil related parameters from input file
		// Obtains and Allocates DXCoils
		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		WhichCoil = 0;
		AvailSchIndex = 0;

		if ( SameString( CoilType, "Coil:Heating:Water" ) || SameString( CoilType, "Coil:Cooling:Water" ) || SameString( CoilType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
			WhichCoil = FindItem( CoilName, WaterCoil );
			if ( WhichCoil != 0 ) {
				AvailSchIndex = WaterCoil( WhichCoil ).SchedPtr;
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

	void
	SetWaterCoilData(
		int const CoilNum, // Number of hot water heating Coil
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_bool DesiccantRegenerationCoil, // Flag that this coil is used as regeneration air heating coil
		Optional_int DesiccantDehumIndex // Index for the desiccant dehum system where this caoil is used 
		) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   February 2016
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function sets data to water Heating Coil using the coil index and arguments passed

		// Using/Aliasing
		using General::TrimSigDigits;

		if ( GetWaterCoilsInputFlag ) {
			GetWaterCoilInput();
			GetWaterCoilsInputFlag = false;
		}

		if ( CoilNum <= 0 || CoilNum > NumWaterCoils ) {
			ShowSevereError( "SetHeatingCoilData: called with heating coil Number out of range=" + TrimSigDigits( CoilNum ) + " should be >0 and <" + TrimSigDigits( NumWaterCoils ) );
			ErrorsFound = true;
			return;
		}

		if ( present( DesiccantRegenerationCoil ) ) {
			WaterCoil( CoilNum ).DesiccantRegenerationCoil = DesiccantRegenerationCoil;
		}

		if ( present( DesiccantDehumIndex ) ) {
			WaterCoil( CoilNum ).DesiccantDehumNum = DesiccantDehumIndex;
		}

	}

	// End of Coil Utility subroutines
	// *****************************************************************************

} // WaterCoils

} // EnergyPlus
