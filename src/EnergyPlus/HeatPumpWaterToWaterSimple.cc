// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>

// EnergyPlus Headers
#include <HeatPumpWaterToWaterSimple.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterSimple {

	// MODULE INFORMATION:
	//       AUTHOR         Kenneth Tang
	//       DATE WRITTEN   March 2005
	//       MODIFIED       Brent Griffith, plant upgrades, fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates a Water-to-Water Heat Pump Simple (Equation-Fit Model)

	// METHODOLOGY EMPLOYED:
	// This simulation is based on a set of coefficients generated from
	// the manufacturer catalog data using the generalized least square method

	// REFERENCES:
	// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
	// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
	// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
	// (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
	// Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
	// M.S. Thesis, Department of Mechanical and Aerospace Engineering,
	// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginSimFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::HourOfDay;
	using DataGlobals::KelvinConv;
	using DataGlobals::TimeStep;
	using DataGlobals::TimeStepZone;
	using DataGlobals::DayOfSim;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SecInHour;
	using General::TrimSigDigits;
	using namespace DataLoopNode;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	std::string const HPEqFitHeating( "HeatPump:WatertoWater:EquationFit:Heating" );
	std::string const HPEqFitHeatingUC( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING" );
	std::string const HPEqFitCooling( "HeatPump:WatertoWater:EquationFit:Cooling" );
	std::string const HPEqFitCoolingUC( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING" );

	// DERIVED TYPE DEFINITIONS

	// Type Description of Heat Pump

	// Output Variables Type definition

	// MODULE VARIABLE DECLARATIONS:
	int NumGSHPs( 0 ); // Number of GSHPs specified in input

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Computational routines

	// Update routine to check convergence and update nodes

	// Object Data
	FArray1D< GshpSpecs > GSHP;
	FArray1D< ReportVars > GSHPReport;

	// MODULE SUBROUTINES:

	// Functions

	void
	SimHPWatertoWaterSimple(
		std::string const & GSHPType, // Type of GSHP
		int const GSHPTypeNum, // Type of GSHP in Plant equipment
		std::string const & GSHPName, // User Specified Name of GSHP
		int & GSHPNum, // Index of Equipment
		bool const FirstHVACIteration,
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 const MyLoad, // Loop demand component will meet
		Real64 & MaxCap, // Maximum operating capacity of GSHP [W]
		Real64 & MinCap, // Minimum operating capacity of GSHP [W]
		Real64 & OptCap, // Optimal operating capacity of GSHP [W]
		int const LoopNum // The calling loop number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Water-to-Water Heat Pump Simple (Equation-Fit Model)

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using namespace DataEnvironment;
		using General::TrimSigDigits;
		using DataPlant::TypeOf_HPWaterEFCooling;
		using DataPlant::TypeOf_HPWaterEFHeating;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // then TRUE, calls subroutine to read input file.

		//Get input from IDF
		if ( GetInputFlag ) {
			GetWatertoWaterHPInput();
			GetInputFlag = false;
		}

		if ( InitLoopEquip ) {
			GSHPNum = FindItemInList( GSHPName, GSHP.Name(), NumGSHPs );
			if ( GSHPNum != 0 ) { // if 0, fall through to next
				GSHP( GSHPNum ).IsThisSized = false;
				InitWatertoWaterHP( GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad );
				GSHP( GSHPNum ).IsThisSized = true;
				SizeWatertoWaterHP( GSHPNum );
				{ auto const SELECT_CASE_var( GSHPTypeNum );
				if ( SELECT_CASE_var == TypeOf_HPWaterEFCooling ) {
					MinCap = 0.0;
					MaxCap = GSHP( GSHPNum ).RatedCapCool;
					OptCap = GSHP( GSHPNum ).RatedCapCool;
				} else if ( SELECT_CASE_var == TypeOf_HPWaterEFHeating ) {
					MinCap = 0.0;
					MaxCap = GSHP( GSHPNum ).RatedCapHeat;
					OptCap = GSHP( GSHPNum ).RatedCapHeat;
				} else {
					ShowFatalError( "SimHPWatertoWaterSimple: Module called with incorrect GSHPType=" + GSHPType );
				}}
				return;
			}
		}

		// Calculate Demand on heat pump
		TypeOfEquip: { auto const SELECT_CASE_var( GSHPTypeNum );
		if ( SELECT_CASE_var == TypeOf_HPWaterEFCooling ) {
			if ( GSHPNum != 0 ) {
				if ( LoopNum == GSHP( GSHPNum ).LoadLoopNum ) { // chilled water loop

					InitWatertoWaterHP( GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad );
					CalcWatertoWaterHPCooling( GSHPNum, MyLoad );
					UpdateGSHPRecords( GSHPNum );

				} else if ( LoopNum == GSHP( GSHPNum ).SourceLoopNum ) { // condenser loop
					UpdateChillerComponentCondenserSide( GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, TypeOf_HPWaterEFCooling, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHPReport( GSHPNum ).QSource, GSHPReport( GSHPNum ).SourceSideInletTemp, GSHPReport( GSHPNum ).SourceSideOutletTemp, GSHPReport( GSHPNum ).SourceSideMassFlowRate, FirstHVACIteration );
				} else {
					ShowFatalError( "SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + GSHPName );
				}
			} else {
				ShowFatalError( "SimHPWatertoWaterSimple:: Invalid " + HPEqFitCooling + ", Requested Unit=" + GSHPName );
			}
		} else if ( SELECT_CASE_var == TypeOf_HPWaterEFHeating ) {
			if ( GSHPNum != 0 ) {
				if ( LoopNum == GSHP( GSHPNum ).LoadLoopNum ) { // chilled water loop

					InitWatertoWaterHP( GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad );
					CalcWatertoWaterHPHeating( GSHPNum, MyLoad );
					UpdateGSHPRecords( GSHPNum );
				} else if ( LoopNum == GSHP( GSHPNum ).SourceLoopNum ) { // condenser loop
					UpdateChillerComponentCondenserSide( GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, TypeOf_HPWaterEFHeating, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, - GSHPReport( GSHPNum ).QSource, GSHPReport( GSHPNum ).SourceSideInletTemp, GSHPReport( GSHPNum ).SourceSideOutletTemp, GSHPReport( GSHPNum ).SourceSideMassFlowRate, FirstHVACIteration );
				} else {
					ShowFatalError( "SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + GSHPName );
				}
			} else {
				ShowFatalError( "SimHPWatertoWaterSimple:: Invalid " + HPEqFitHeating + ", Requested Unit=" + GSHPName );
			}
		} else {
			ShowFatalError( "SimHPWatertoWaterSimple: Module called with incorrect GSHPType=" + GSHPType );
		}} // TypeOfEquip

	}

	void
	GetWatertoWaterHPInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtain input from IDF and store them in data structures

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using DataPlant::TypeOf_HPWaterEFCooling;
		using DataPlant::TypeOf_HPWaterEFHeating;
		using DataPlant::ScanPlantLoopsForObject;

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
		int GSHPNum; // GSHP number
		int HPNum; // Counter
		int NumCoolCoil; // Number of Cooling Coils
		int NumHeatCoil; // Number of Heating Coils
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		FArray1D_string AlphArray( 5 ); // character string data
		FArray1D< Real64 > NumArray( 15 ); // numeric data

		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;

		NumCoolCoil = GetNumObjectsFound( HPEqFitCoolingUC );
		NumHeatCoil = GetNumObjectsFound( HPEqFitHeatingUC );
		NumGSHPs = NumCoolCoil + NumHeatCoil;

		if ( NumGSHPs <= 0 ) {
			ShowSevereError( "GetEquationFitWaterToWater Input: No Equipment found" );
			ErrorsFound = true;
		}

		if ( NumGSHPs > 0 ) {
			GSHP.allocate( NumGSHPs );
			GSHPReport.allocate( NumGSHPs );
			// initialize the data structures
		}

		//Load data structure for cooling coil
		for ( HPNum = 1; HPNum <= NumCoolCoil; ++HPNum ) {

			GSHPNum = HPNum;

			GetObjectItem( HPEqFitCoolingUC, HPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = true;
			VerifyName( AlphArray( 1 ), GSHP.Name(), HPNum - 1, IsNotOK, IsBlank, "GHSP Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			GSHP( GSHPNum ).WWHPPlantTypeOfNum = TypeOf_HPWaterEFCooling;
			GSHP( GSHPNum ).Name = AlphArray( 1 );
			GSHP( GSHPNum ).RatedLoadVolFlowCool = NumArray( 1 );
			GSHP( GSHPNum ).RatedSourceVolFlowCool = NumArray( 2 );
			GSHP( GSHPNum ).RatedCapCool = NumArray( 3 );
			GSHP( GSHPNum ).RatedCOPCool = NumArray( 4 );
			GSHP( GSHPNum ).CoolCap1 = NumArray( 5 );
			GSHP( GSHPNum ).CoolCap2 = NumArray( 6 );
			GSHP( GSHPNum ).CoolCap3 = NumArray( 7 );
			GSHP( GSHPNum ).CoolCap4 = NumArray( 8 );
			GSHP( GSHPNum ).CoolCap5 = NumArray( 9 );
//			GSHP( GSHPNum ).CoolPower1 = NumArray( 10 );
//			GSHP( GSHPNum ).CoolPower2 = NumArray( 11 );
//			GSHP( GSHPNum ).CoolPower3 = NumArray( 12 );
//			GSHP( GSHPNum ).CoolPower4 = NumArray( 13 );
//			GSHP( GSHPNum ).CoolPower5 = NumArray( 14 );

			GSHP( GSHPNum ).SourceSideInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, HPEqFitCoolingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).SourceSideOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, HPEqFitCoolingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, HPEqFitCoolingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, HPEqFitCoolingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			// Test node sets
			TestCompSet( HPEqFitCoolingUC, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Condenser Water Nodes" );
			TestCompSet( HPEqFitCoolingUC, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Hot Water Nodes" );

			// save the design source side flow rate for use by plant loop sizing algorithms
			// RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, 0.5 * GSHP( GSHPNum ).RatedSourceVolFlowCool );

			// CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Cooling'
			SetupOutputVariable( "Water to Water Heat Pump Electric Energy [J]", GSHPReport( GSHPNum ).Energy, "System", "Sum", GSHP( GSHPNum ).Name, _, "Electricity", "Cooling", _, "Plant" );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QLoadEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QSourceEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
		}

		//Load data structure for heating coil
		for ( HPNum = 1; HPNum <= NumHeatCoil; ++HPNum ) {

			GSHPNum = NumCoolCoil + HPNum;

			GetObjectItem( HPEqFitHeatingUC, HPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = true;
			VerifyName( AlphArray( 1 ), GSHP.Name(), HPNum - 1, IsNotOK, IsBlank, "GHSP Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			GSHP( GSHPNum ).WWHPPlantTypeOfNum = TypeOf_HPWaterEFHeating;
			GSHP( GSHPNum ).Name = AlphArray( 1 );
			GSHP( GSHPNum ).RatedLoadVolFlowHeat = NumArray( 1 );
			GSHP( GSHPNum ).RatedSourceVolFlowHeat = NumArray( 2 );
			GSHP( GSHPNum ).RatedCapHeat = NumArray( 3 );
			GSHP( GSHPNum ).RatedCOPHeat = NumArray( 4 );
			GSHP( GSHPNum ).HeatCap1 = NumArray( 5 );
			GSHP( GSHPNum ).HeatCap2 = NumArray( 6 );
			GSHP( GSHPNum ).HeatCap3 = NumArray( 7 );
			GSHP( GSHPNum ).HeatCap4 = NumArray( 8 );
			GSHP( GSHPNum ).HeatCap5 = NumArray( 9 );
//			GSHP( GSHPNum ).HeatPower1 = NumArray( 10 );
//			GSHP( GSHPNum ).HeatPower2 = NumArray( 11 );
//			GSHP( GSHPNum ).HeatPower3 = NumArray( 12 );
//			GSHP( GSHPNum ).HeatPower4 = NumArray( 13 );
//			GSHP( GSHPNum ).HeatPower5 = NumArray( 14 );

			GSHP( GSHPNum ).SourceSideInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, HPEqFitHeatingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).SourceSideOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, HPEqFitHeatingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, HPEqFitHeatingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, HPEqFitHeatingUC, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			// Test node sets
			TestCompSet( HPEqFitHeatingUC, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Condenser Water Nodes" );
			TestCompSet( HPEqFitHeatingUC, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Hot Water Nodes" );

			// save the design source side flow rate for use by plant loop sizing algorithms
			// RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, 0.5 * GSHP( GSHPNum ).RatedSourceVolFlowHeat );

			// CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Heating'
			SetupOutputVariable( "Water to Water Heat Pump Electric Energy [J]", GSHPReport( GSHPNum ).Energy, "System", "Sum", GSHP( GSHPNum ).Name, _, "Electricity", "Heating", _, "Plant" );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QLoadEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QSourceEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
		}

		for ( GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum ) {
			//setup output variables
			SetupOutputVariable( "Water to Water Heat Pump Electric Power [W]", GSHPReport( GSHPNum ).Power, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Rate [W]", GSHPReport( GSHPNum ).QLoad, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Rate [W]", GSHPReport( GSHPNum ).QSource, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Outlet Temperature [C]", GSHPReport( GSHPNum ).LoadSideOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Inlet Temperature [C]", GSHPReport( GSHPNum ).LoadSideInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Outlet Temperature [C]", GSHPReport( GSHPNum ).SourceSideOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Inlet Temperature [C]", GSHPReport( GSHPNum ).SourceSideInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Mass Flow Rate [kg/s]", GSHPReport( GSHPNum ).LoadSideMassFlowRate, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Mass Flow Rate [kg/s]", GSHPReport( GSHPNum ).SourceSideMassFlowRate, "System", "Average", GSHP( GSHPNum ).Name );

			//scan for loop connection data
			errFlag = false;
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, GSHP( GSHPNum ).WWHPPlantTypeOfNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum, _, _, _, GSHP( GSHPNum ).SourceSideInletNodeNum, _, errFlag );
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, GSHP( GSHPNum ).WWHPPlantTypeOfNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum, _, _, _, GSHP( GSHPNum ).LoadSideInletNodeNum, _, errFlag );

			if ( errFlag ) {
				ShowFatalError( "GetWatertoWaterHPInput: Program terminated on scan for loop data" );
			}

		}

	}

	void
	InitWatertoWaterHP(
		int const GSHPTypeNum, // Type of GSHP
		std::string const & GSHPName, // User Specified Name of GSHP
		int const GSHPNum, // GSHP Number
		bool const FirstHVACIteration,
		Real64 const MyLoad // Demand Load
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Water-to-Water HP Simple

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
		// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
		// (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
		// Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
		// M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataPlant::TypeOf_HPWaterEFCooling;
		using DataPlant::TypeOf_HPWaterEFHeating;
		using DataPlant::PlantLoop;
		using DataPlant::PlantSizeNotComplete;
		using DataPlant::PlantSizesOkayToFinalize;
		using InputProcessor::SameString;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitGshp" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoadSideInletNode; // Load Side Inlet Node
		int LoadSideOutletNode; // Load Side Outlet Node
		int SourceSideInletNode; // Source Side Inlet Node
		int SourceSideOutletNode; // Source Side Outlet Node
		static FArray1D_bool MyEnvrnFlag; // Flag required to keep track of initialization
		static bool OneTimeFlag( true ); // One Time Flag
		static Real64 CurrentSimTime( 0.0 ); // Current Simulation Time
		static Real64 PrevSimTime( 0.0 ); // Previous Simulation Time
		static FArray1D_bool MyPlanScanFlag;
		static bool MyOneTimeFlag( true );
		int LoopNum;
		int LoopSideNum;
		Real64 rho; // local fluid density

		if ( MyOneTimeFlag ) {
			MyPlanScanFlag.allocate( NumGSHPs );
			MyEnvrnFlag.allocate( NumGSHPs );
			MyOneTimeFlag = false;
			MyEnvrnFlag = true;
			MyPlanScanFlag = true;
		}

		GSHP( GSHPNum ).MustRun = true; // Reset MustRun flag to TRUE
		LoadSideInletNode = GSHP( GSHPNum ).LoadSideInletNodeNum;
		LoadSideOutletNode = GSHP( GSHPNum ).LoadSideOutletNodeNum;
		SourceSideInletNode = GSHP( GSHPNum ).SourceSideInletNodeNum;
		SourceSideOutletNode = GSHP( GSHPNum ).SourceSideOutletNodeNum;

		if ( MyEnvrnFlag( GSHPNum ) && BeginEnvrnFlag && PlantSizesOkayToFinalize ) {

			if( PlantSizeNotComplete ) { 
				SizeWatertoWaterHP( GSHPNum ); 
			}

			//Initialize all report variables to a known state at beginning of simulation

			GSHPReport( GSHPNum ).Power = 0.0;
			GSHPReport( GSHPNum ).Energy = 0.0;
			GSHPReport( GSHPNum ).QLoad = 0.0;
			GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
			GSHPReport( GSHPNum ).QSource = 0.0;
			GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
			GSHPReport( GSHPNum ).LoadSideMassFlowRate = 0.0;
			GSHPReport( GSHPNum ).LoadSideInletTemp = 0.0;
			GSHPReport( GSHPNum ).LoadSideOutletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideMassFlowRate = 0.0;
			GSHPReport( GSHPNum ).SourceSideInletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideOutletTemp = 0.0;
			GSHP( GSHPNum ).IsOn = false;
			GSHP( GSHPNum ).MustRun = true;

			if ( GSHP( GSHPNum ).WWHPPlantTypeOfNum == TypeOf_HPWaterEFHeating ) {
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).LoadSideDesignMassFlow = GSHP( GSHPNum ).RatedLoadVolFlowHeat * rho;
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).SourceSideDesignMassFlow = GSHP( GSHPNum ).RatedSourceVolFlowHeat * rho;
			} else if ( GSHP( GSHPNum ).WWHPPlantTypeOfNum == TypeOf_HPWaterEFCooling ) {
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).LoadSideDesignMassFlow = GSHP( GSHPNum ).RatedLoadVolFlowCool * rho;
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).SourceSideDesignMassFlow = GSHP( GSHPNum ).RatedSourceVolFlowCool * rho;
			}

			InitComponentNodes( 0.0, GSHP( GSHPNum ).LoadSideDesignMassFlow, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );

			InitComponentNodes( 0.0, GSHP( GSHPNum ).SourceSideDesignMassFlow, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );

			if ( Node( GSHP( GSHPNum ).SourceSideOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) Node( GSHP( GSHPNum ).SourceSideOutletNodeNum ).TempSetPoint = 0.0;
			Node( GSHP( GSHPNum ).SourceSideInletNodeNum ).Temp = Node( GSHP( GSHPNum ).SourceSideOutletNodeNum ).TempSetPoint + 30;

			MyEnvrnFlag( GSHPNum ) = false;
		}
		// Reset the environment flag
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( GSHPNum ) = true;

		if ( PrevSimTime != CurrentSimTime ) {
			PrevSimTime = CurrentSimTime;
		}

		// Calculate the simulation time
		CurrentSimTime = ( DayOfSim - 1 ) * 24 + ( HourOfDay - 1 ) + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed;

		// Initialize event time array when the environment simulation begins
		if ( CurrentSimTime == 0.0 && OneTimeFlag ) {
			OneTimeFlag = false;
		}

		LoopNum = GSHP( GSHPNum ).LoadLoopNum;
		LoopSideNum = GSHP( GSHPNum ).LoadLoopSideNum;

		if ( CurrentSimTime > 0.0 ) OneTimeFlag = true;

		if ( MyLoad > 0.0 && GSHPTypeNum == TypeOf_HPWaterEFHeating ) {
			GSHP( GSHPNum ).MustRun = true;
			GSHP( GSHPNum ).IsOn = true;
		} else if ( MyLoad < 0.0 && GSHPTypeNum == TypeOf_HPWaterEFCooling ) {
			GSHP( GSHPNum ).MustRun = true;
			GSHP( GSHPNum ).IsOn = true;
		} else {
			GSHP( GSHPNum ).MustRun = false;
			GSHP( GSHPNum ).IsOn = false;
		}

		//*******Set flow based on "flowlock" and "run" flags**********
		// Set flows if the heat pump is not running
		if ( ! GSHP( GSHPNum ).MustRun ) {
			GSHPReport( GSHPNum ).LoadSideMassFlowRate = 0.0;
			GSHPReport( GSHPNum ).SourceSideMassFlowRate = 0.0;

			SetComponentFlowRate( GSHPReport( GSHPNum ).LoadSideMassFlowRate, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
			SetComponentFlowRate( GSHPReport( GSHPNum ).SourceSideMassFlowRate, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );

			// Set flows if the heat pump is running
		} else { // the heat pump must run

			GSHPReport( GSHPNum ).LoadSideMassFlowRate = GSHP( GSHPNum ).LoadSideDesignMassFlow;
			GSHPReport( GSHPNum ).SourceSideMassFlowRate = GSHP( GSHPNum ).SourceSideDesignMassFlow;
			// now check against and request in plant
			SetComponentFlowRate( GSHPReport( GSHPNum ).LoadSideMassFlowRate, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
			SetComponentFlowRate( GSHPReport( GSHPNum ).SourceSideMassFlowRate, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );
			//if there's no flowin one, turn the entire "heat pump off"
			if ( GSHPReport( GSHPNum ).LoadSideMassFlowRate <= 0.0 || GSHPReport( GSHPNum ).SourceSideMassFlowRate <= 0.0 ) {

				GSHPReport( GSHPNum ).LoadSideMassFlowRate = 0.0;
				GSHPReport( GSHPNum ).SourceSideMassFlowRate = 0.0;
				GSHP( GSHPNum ).MustRun = false;

				SetComponentFlowRate( GSHPReport( GSHPNum ).LoadSideMassFlowRate, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
				SetComponentFlowRate( GSHPReport( GSHPNum ).SourceSideMassFlowRate, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );

				return;
			}
		}

		// Get inlet temps
		GSHPReport( GSHPNum ).LoadSideInletTemp = Node( LoadSideInletNode ).Temp;
		GSHPReport( GSHPNum ).SourceSideInletTemp = Node( SourceSideInletNode ).Temp;

		// Outlet variables
		GSHPReport( GSHPNum ).Power = 0.0;
		GSHPReport( GSHPNum ).Energy = 0.0;
		GSHPReport( GSHPNum ).QLoad = 0.0;
		GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
		GSHPReport( GSHPNum ).QSource = 0.0;
		GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
		GSHPReport( GSHPNum ).LoadSideOutletTemp = 0.0;
		GSHPReport( GSHPNum ).SourceSideOutletTemp = 0.0;

	}
	
	void 
	SizeWatertoWaterHP(
		int const GSHPNum
	)
	{
          // SUBROUTINE INFORMATION:
          //       AUTHOR         Daeho Kang
          //       DATE WRITTEN   April 2014
          //       MODIFIED       na
          //       RE-ENGINEERED  na

          // PURPOSE OF THIS SUBROUTINE:
          // This subroutine is for sizing water source heat pump components for which capacities and flow rates
          // have not been specified in the input.

          // METHODOLOGY EMPLOYED:
          // Obtains chilled water or hot water flow rate from the plant sizing array. 
          // Calculates nominal cooling or heating capacity from the design flow rates and loop design temperature difference.
          // The source side flow rates are calculated from the nominal capacity, the COP, and the loop design delta T.

          // REFERENCES:
          // na

          using namespace DataSizing;
		  using DataGlobals::DisplayExtraWarnings;
		  using DataPlant::PlantLoop;
		  using DataPlant::PlantSizesOkayToFinalize;
		  using DataPlant::TypeOf_HPWaterEFCooling;
		  using DataPlant::TypeOf_HPWaterEFHeating;
		  using PlantUtilities::RegisterPlantCompDesignFlow;
		  using ReportSizingManager::ReportSizingOutput;
		  using OutputReportPredefined::pdchMechType;
		  using OutputReportPredefined::pdchMechNomCap;
		  using OutputReportPredefined::pdchMechNomEff;
		  using OutputReportPredefined::PreDefTableEntry;
		  using FluidProperties::GetDensityGlycol;
		  using FluidProperties::GetSpecificHeatGlycol;
		  using DataHVACGlobals::SmallWaterVolFlow;
		  using General::RoundSigDigits;

 
          // SUBROUTINE ARGUMENT DEFINITIONS:
  

          // SUBROUTINE PARAMETER DEFINITIONS:
          // na

          // INTERFACE BLOCK SPECIFICATIONS
          // na

          // DERIVED TYPE DEFINITIONS
          // na

          // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		  int PltSizCoolNum; // Plant Sizing index corresponding to chilled water loop
		  int PltSizHeatNum; // Plant Sizing index corresponding to hot water loop
		  int PltSizCondNum; // Plant Sizing index for condenser loop
		  bool ErrorsFound;   // If errors detected in input
		  bool LoopErrorsFound;
//		  CHARACTER(len=MaxNameLength) :: equipName
		  std::string equipName;
		  Real64 rho; // local fluid density
		  Real64 Cp;  // local fluid specific heat
		  Real64 tmpRatedCapCool;				// local nominal capacity cooling power
		  Real64 tmpRatedLoadVolFlowCool;		// local chilled water design volume flow rate in cooling mode
		  Real64 tmpRatedSourceVolFlowCool;		// local source water design volume flow rate in cooling mode
		  Real64 tmpRatedCapHeat;				// local nominal capacity heating power
		  Real64 tmpRatedLoadVolFlowHeat;		// local hot water design volume flow rate in heating mode
		  Real64 tmpRatedSourceVolFlowHeat;		// local source water design volume flow rate in heating mode
		  bool IsAutoSize;						// Indicator to autosize for reporting
		  Real64 RatedCapCoolUser;				// Hardsized nominal capacity cooling power for reporting
		  Real64 RatedLoadVolFlowCoolUser;		// Hardsized chilled water flow in cooling mode for reporting
		  Real64 RatedSourceVolFlowCoolUser;	// Hardsized source water flow in cooling mode for reporting
		  Real64 RatedCapHeatUser;				// Hardsized nominal capacity heating power for reporting
		  Real64 RatedLoadVolFlowHeatUser;		// Hardsized chilled water flow in heating mode for reporting
		  Real64 RatedSourceVolFlowHeatUser;	// Hardsized source water flow in heating mode for reporting

		  PltSizCoolNum = 0;
		  PltSizHeatNum = 0;
		  PltSizCondNum = 0;
		  ErrorsFound = false;
		  IsAutoSize = false;

		  if ( GSHP( GSHPNum ).WWHPPlantTypeOfNum == TypeOf_HPWaterEFCooling ) {
			  tmpRatedCapCool = GSHP( GSHPNum ).RatedCapCool;
			  tmpRatedLoadVolFlowCool = GSHP( GSHPNum ).RatedLoadVolFlowCool;
			  tmpRatedSourceVolFlowCool = GSHP( GSHPNum ).RatedSourceVolFlowCool;

			  RatedCapCoolUser = 0.0;
			  RatedLoadVolFlowCoolUser = 0.0;
			  RatedSourceVolFlowCoolUser = 0.0;

			  PltSizCoolNum = PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).PlantSizNum;
			  PltSizCondNum = PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).PlantSizNum;
			  
			  if ( GSHP( GSHPNum ).RatedCapCool == AutoSize) {
					IsAutoSize = true;
			  }

			  if ( PltSizCoolNum > 0 ) {
				  if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					  rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, InitConvTemp, 
						  PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING" );
					  Cp = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, InitConvTemp,
						  PlantLoop( GSHP( GSHPNum).LoadLoopNum ).FluidIndex, "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING" );
					  tmpRatedCapCool = Cp * rho * PlantSizData( PltSizCoolNum ).DeltaT * PlantSizData( PltSizCoolNum ).DesVolFlowRate;
				  } else {
					  if ( IsAutoSize ) tmpRatedCapCool = 0.0;
				  }
				  
				  if ( PlantSizesOkayToFinalize ) {
					  if ( IsAutoSize ) {
						  GSHP( GSHPNum ).RatedCapCool = tmpRatedCapCool;
						  if ( GSHP( GSHPNum ).IsThisSized == false ) {
							  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name,
								  "Design Size Nominal Capacity [W]", tmpRatedCapCool );
						  }
					  } else { // Hard-size with sizing data
						  if ( GSHP( GSHPNum ).RatedCapCool > 0.0 && tmpRatedCapCool > 0.0) {
							  RatedCapCoolUser = GSHP( GSHPNum ).RatedCapCool;
							  if ( GSHP( GSHPNum ).IsThisSized == false ) {
								  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name,
									  "Design Size Nominal Capacity [W]", tmpRatedCapCool, "User-Specified Nominal Capacity [W]", RatedCapCoolUser );
								  if ( DisplayExtraWarnings ) {
									  if (( std::abs( tmpRatedCapCool - RatedCapCoolUser )/RatedCapCoolUser ) > AutoVsHardSizingThreshold ) {
										  ShowMessage( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING: Potential issue with equipment sizing for " + 
											  GSHP( GSHPNum ).Name );
										  ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( RatedCapCoolUser, 2 ) + " [W]" );
										  ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpRatedCapCool, 2 ) + " [W]" );
										  ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										  ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									  }
								  }
							  }
						  }
					  }
				  }

				  // Set hard-sized capacity to be tmporary variable for calculating flow rates
				  if ( !IsAutoSize && tmpRatedCapCool != GSHP( GSHPNum ).RatedCapCool ) {
					  tmpRatedCapCool = GSHP( GSHPNum ).RatedCapCool;
				  }
			  } else {
				  if ( IsAutoSize ) {
					  ShowSevereError( "Autosizing of nominal capacity requires a loop Sizing:Plant object" );
					  ShowContinueError( "Occurs in HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING object=" + GSHP( GSHPNum ).Name );
					  ErrorsFound = true;
				  } else {
					  if ( GSHP( GSHPNum ).IsThisSized == false ) {
						  if ( GSHP( GSHPNum ).RatedCapCool > 0.0 ) {
							  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name,
								  "User-Specified Nominal Capacity [W]", GSHP( GSHPNum ).RatedCapCool );
						  }
					  }
				  }
			  }

			  IsAutoSize = false;
			  if ( GSHP( GSHPNum ).RatedLoadVolFlowCool == AutoSize) {
				  IsAutoSize = true;
			  }
			  if ( PltSizCoolNum > 0 ) {
				  if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					  tmpRatedLoadVolFlowCool = PlantSizData( PltSizCoolNum ).DesVolFlowRate;
				  } else {
					  if ( IsAutoSize ) {
						  tmpRatedLoadVolFlowCool = 0.0;
					  }
				  }

				  if ( PlantSizesOkayToFinalize ) {
					  if ( IsAutoSize ) {
						  GSHP( GSHPNum ).RatedLoadVolFlowCool = tmpRatedLoadVolFlowCool;
						  if ( GSHP( GSHPNum ).IsThisSized == false ) {
							  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name, 
								  "Design Size Load Side Flow Rate [m3/s]", tmpRatedLoadVolFlowCool );
						  }
					  } else {
						  if ( GSHP( GSHPNum ).RatedLoadVolFlowCool > 0.0 && tmpRatedLoadVolFlowCool > 0.0 ) {
							  RatedLoadVolFlowCoolUser = GSHP( GSHPNum ).RatedLoadVolFlowCool;
							  if ( GSHP( GSHPNum ).IsThisSized == false ) {
								  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name, 
									  "Design Size Load Side Flow Rate [m3/s]", tmpRatedLoadVolFlowCool, 
									  "User-Specified Load Side Flow Rate [m3/s]", RatedLoadVolFlowCoolUser );
								  if ( DisplayExtraWarnings ) {
									  if (( std::abs( tmpRatedLoadVolFlowCool - RatedLoadVolFlowCoolUser )/RatedLoadVolFlowCoolUser ) > AutoVsHardSizingThreshold ) {
										  ShowMessage( "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
										  ShowContinueError( "User-Specified Load Side Flow Rate of " + RoundSigDigits( RatedLoadVolFlowCoolUser, 5 ) + " [m3/s]" );
										  ShowContinueError( "differs from Design Size Load Side Flow Rate of " + RoundSigDigits( tmpRatedLoadVolFlowCool, 5 ) + " [m3/s]" );
										  ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										  ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									  }
								  }
							  }
						  }
					  }
				  }

				  // Set hard-sized evaporative flow rate to be tmporary variable for registering it
				  if ( !IsAutoSize && tmpRatedLoadVolFlowCool != GSHP( GSHPNum ).RatedLoadVolFlowCool ) {
					  tmpRatedLoadVolFlowCool = GSHP( GSHPNum ).RatedLoadVolFlowCool;
				  }
			  } else {
				  if ( IsAutoSize ) {
					  ShowSevereError( "Autosizing of load side flow rate requires a loop Sizing:Plant objec" );
					  ShowContinueError( "Occurs in HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING object=" + GSHP( GSHPNum ).Name );
					  ErrorsFound = true;
				  } else {
					  if ( GSHP( GSHPNum ).IsThisSized == false ) {
						  if ( GSHP( GSHPNum ).RatedLoadVolFlowCool > 0.0 ) {
							  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name, 
								  "User-Specified Load Side Flow Rate [m3/s]", GSHP( GSHPNum ).RatedLoadVolFlowCool );
						  }
					  }
				  }
			  }

			  RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, tmpRatedLoadVolFlowCool );
			  IsAutoSize = false;
			  if ( GSHP( GSHPNum ).RatedSourceVolFlowCool == AutoSize ) {
				  IsAutoSize = true;
			  }
			  if ( PltSizCondNum > 0 && PltSizCoolNum > 0 ) {
				  if ( PlantSizData( PltSizCoolNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpRatedCapCool > 0.0 ) {
					  rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, 29.44, 
						  PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING" );
					  Cp = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, 29.44,
						  PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING" );
					  tmpRatedSourceVolFlowCool = tmpRatedCapCool *  (1.0 + 1.0/GSHP( GSHPNum ).RatedCOPCool ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				  } else {
					  if ( IsAutoSize ) {
						  tmpRatedSourceVolFlowCool = 0.0;
					  }
				  }
				  
				  if ( PlantSizesOkayToFinalize ) {
					  if ( IsAutoSize ) {
						  GSHP( GSHPNum ).RatedSourceVolFlowCool = tmpRatedSourceVolFlowCool;
						  if ( GSHP( GSHPNum ).IsThisSized == false ) {
							  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name,
								  "Design Size Source Side Flow Rate [m3/s]", tmpRatedSourceVolFlowCool );
						  }
					  } else {
						  if ( GSHP( GSHPNum ).RatedSourceVolFlowCool > 0.0 && tmpRatedSourceVolFlowCool > 0.0 ) {
							  RatedSourceVolFlowCoolUser = GSHP( GSHPNum ).RatedSourceVolFlowCool;
							  if ( GSHP( GSHPNum ).IsThisSized == false ) {
								  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name,
									  "Design Size Source Side Flow Rate [m3/s]", tmpRatedSourceVolFlowCool,
									  "User-Specified Source Side Flow Rate [m3/s]", RatedSourceVolFlowCoolUser );
								  if ( DisplayExtraWarnings ) {
									  if (( std::abs( tmpRatedSourceVolFlowCool - RatedSourceVolFlowCoolUser ) / RatedSourceVolFlowCoolUser ) > AutoVsHardSizingThreshold) {
										  ShowMessage( "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
										  ShowContinueError( "User-Specified Source Side Flow Rate of " + RoundSigDigits( RatedSourceVolFlowCoolUser, 5 ) + " [m3/s]" );
										  ShowContinueError( "differs from Design Size Source Side Flow Rate of " + RoundSigDigits( tmpRatedSourceVolFlowCool, 5 ) + " [m3/s]" );
										  ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										  ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									  }
								  }
							  }
						  }
					  }
				  }

				  // Set hard-sized evaporative flow rate to be tmporary variable for registering it
				  if ( !IsAutoSize && tmpRatedSourceVolFlowCool != GSHP( GSHPNum ).RatedSourceVolFlowCool ) {
					  tmpRatedSourceVolFlowCool = GSHP( GSHPNum ).RatedSourceVolFlowCool;
				  }
			  } else {
				  if ( IsAutoSize ) {
					  ShowSevereError( "Autosizing of source side flow rate requires a loop Sizing:Plant object" );
					  ShowContinueError( "Occurs in HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING object=" + GSHP( GSHPNum ).Name );
					  ErrorsFound = true;
				  } else {
					  if ( GSHP( GSHPNum ).IsThisSized == false ) {
						  if ( GSHP( GSHPNum ).RatedSourceVolFlowCool > 0.0 ) {
							  ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", GSHP( GSHPNum ).Name, 
								  "User-Specified Source Side Flow Rate [m3/s]", GSHP( GSHPNum ).RatedSourceVolFlowCool );
						  }
					  }
				  }
			  }
			  
			  // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
			  RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, tmpRatedSourceVolFlowCool );

			  if ( ErrorsFound ) {
				  ShowFatalError( "Preceding sizing errors cause program termination" );
			  }

			  // create predefined report
			  if ( PlantSizesOkayToFinalize ) {
				  equipName = GSHP( GSHPNum ).Name;
				  PreDefTableEntry( pdchMechType, equipName, "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING" );
				  PreDefTableEntry( pdchMechNomEff, equipName, GSHP( GSHPNum ).RatedCOPCool );
				  PreDefTableEntry( pdchMechNomCap, equipName, GSHP( GSHPNum ).RatedCapCool );
			  }
		} else if ( GSHP( GSHPNum ).WWHPPlantTypeOfNum == TypeOf_HPWaterEFHeating ) {
			tmpRatedCapHeat = GSHP( GSHPNum ).RatedCapHeat;
			tmpRatedLoadVolFlowHeat = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
			tmpRatedSourceVolFlowHeat = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
			RatedCapHeatUser = 0.0;
			RatedLoadVolFlowHeatUser = 0.0;
			RatedSourceVolFlowHeatUser = 0.0;

			PltSizHeatNum = PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).PlantSizNum;
			PltSizCondNum = PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).PlantSizNum;

			if ( GSHP( GSHPNum ).RatedCapHeat  == AutoSize) {
				IsAutoSize = true;
			}
			
			if ( PltSizHeatNum > 0 ) {
				if ( PlantSizData( PltSizHeatNum).DesVolFlowRate >= SmallWaterVolFlow ) {
					rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex,
						"SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING" );
					Cp = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, InitConvTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex,
						"SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING" );
					tmpRatedCapHeat = Cp * rho * PlantSizData( PltSizHeatNum ).DeltaT * PlantSizData( PltSizHeatNum ).DesVolFlowRate;
				} else {
					if ( IsAutoSize ) {
						tmpRatedCapHeat = 0.0;
					}
				}
				if ( PlantSizesOkayToFinalize ) {
					if ( IsAutoSize ) {
						GSHP( GSHPNum ).RatedCapHeat = tmpRatedCapHeat;
						if ( GSHP( GSHPNum ).IsThisSized == false ) {
							ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name, "Design Size Nominal Capacity [W]", tmpRatedCapHeat );
						}
					} else { // Hard-size with sizing data
						if ( GSHP( GSHPNum ).RatedCapHeat > 0.0 && tmpRatedCapHeat > 0.0 ) {
							RatedCapHeatUser = GSHP( GSHPNum ).RatedCapHeat;
							if ( GSHP( GSHPNum ).IsThisSized == false ) {
								ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name, "Design Size Nominal Capacity [W]", tmpRatedCapHeat, 
									"User-Specified Nominal Capacity [W]", RatedCapHeatUser );
								if ( DisplayExtraWarnings ) {
									if ( ( std::abs( tmpRatedCapHeat - RatedCapHeatUser ) / RatedCapHeatUser ) > AutoVsHardSizingThreshold ) {
										ShowMessage( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
										ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( RatedCapHeatUser, 2 ) + " [W]" );
										ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpRatedCapHeat, 2 ) + " [W]" );
										ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									}
								}
							}
						}
					}
				}

				// Set hard-sized capacity to be tmporary variable for calculating flow rates
				if ( !IsAutoSize && tmpRatedCapHeat != GSHP( GSHPNum ).RatedCapHeat ) {
					tmpRatedCapHeat = GSHP( GSHPNum ).RatedCapHeat;
				}
			} else {
				if ( IsAutoSize ) {
					ShowSevereError( "Autosizing of nominal capacity requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING object=" + GSHP( GSHPNum ).Name );
					ErrorsFound = true;
				} else {
					if ( GSHP( GSHPNum ).IsThisSized == false ) {
						if ( GSHP( GSHPNum ).RatedCapHeat > 0.0 ) {
							ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name, "User-Specified Nominal Capacity [W]", GSHP( GSHPNum ).RatedCapHeat );
						}
					}
				}
			}

			IsAutoSize = false;
			if ( GSHP( GSHPNum ).RatedLoadVolFlowHeat == AutoSize) {
				IsAutoSize = true;
			}
			if ( PltSizHeatNum > 0 ) {
				if ( PlantSizData( PltSizHeatNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					tmpRatedLoadVolFlowHeat = PlantSizData( PltSizHeatNum ).DesVolFlowRate;
				} else {
					if ( IsAutoSize ) {
						tmpRatedLoadVolFlowHeat = 0.0;
					}
				}
				if ( PlantSizesOkayToFinalize ) {
					if ( IsAutoSize ) {
						GSHP( GSHPNum ).RatedLoadVolFlowHeat = tmpRatedLoadVolFlowHeat;
						if ( GSHP( GSHPNum ).IsThisSized == false ) {
							ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum).Name,
								"Design Size Load Side Flow Rate [m3/s]", tmpRatedLoadVolFlowHeat );
						}
					} else {
						if ( GSHP( GSHPNum ).RatedLoadVolFlowHeat > 0.0 && tmpRatedLoadVolFlowHeat > 0.0 ) {
							RatedLoadVolFlowHeatUser = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
							if ( GSHP( GSHPNum ).IsThisSized == false ) {
								ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name, "Design Size Load Side Flow Rate [m3/s]", tmpRatedLoadVolFlowHeat,
									"User-Specified Load Side Flow Rate [m3/s]", RatedLoadVolFlowHeatUser );
								if ( DisplayExtraWarnings ) {
									if ( ( std::abs( tmpRatedLoadVolFlowHeat - RatedLoadVolFlowHeatUser ) / RatedLoadVolFlowHeatUser ) > AutoVsHardSizingThreshold ) {
										ShowMessage( "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name);
										ShowContinueError( "User-Specified Load Side Flow Rate of " + RoundSigDigits( RatedLoadVolFlowHeatUser, 5) + " [m3/s]" );
										ShowContinueError( "differs from Design Size Load Side Flow Rate of " + RoundSigDigits( tmpRatedLoadVolFlowHeat, 5 ) + " [m3/s]" );
										ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									}
								}
							}
						}
					}
				}

				// Set hard-sized evaporative flow rate to be tmporary variable for registering it
				if ( !IsAutoSize && tmpRatedLoadVolFlowHeat != GSHP( GSHPNum ).RatedLoadVolFlowHeat ) {
					tmpRatedLoadVolFlowHeat = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
				}
			} else {
				if ( IsAutoSize ) {
					ShowSevereError( "Autosizing of load side flow rate requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING object=" + GSHP( GSHPNum ).Name);
					ErrorsFound = true;
				} else {
					if ( GSHP( GSHPNum ).IsThisSized == false ) {
						if ( GSHP( GSHPNum ).RatedLoadVolFlowHeat > 0.0 ) {
							ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name, 
								"User-Specified Load Side Flow Rate [m3/s]", GSHP( GSHPNum ).RatedLoadVolFlowHeat );
						}
					}
				}
			}

			RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, tmpRatedLoadVolFlowHeat ); 
			
			IsAutoSize = false;
			if ( GSHP( GSHPNum ).RatedSourceVolFlowHeat == AutoSize ) {
				IsAutoSize = true;
			}
			if ( PltSizCondNum > 0 && PltSizHeatNum > 0 ) {
				if ( PlantSizData( PltSizHeatNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpRatedCapHeat > 0.0 ) {
					rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, 29.44, 
						PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING" );
					Cp = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, 29.44, 
						PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING" );
					tmpRatedSourceVolFlowHeat = tmpRatedCapHeat *  (1.0 - 1.0/GSHP( GSHPNum ).RatedCOPHeat ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				} else {
					if ( IsAutoSize ) {
						tmpRatedSourceVolFlowHeat = 0.0;
					}
				}

				if ( PlantSizesOkayToFinalize ) {
					if ( IsAutoSize ) {
						GSHP( GSHPNum ).RatedSourceVolFlowHeat = tmpRatedSourceVolFlowHeat;
						if ( GSHP( GSHPNum ).IsThisSized == false ) {
							ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name,
								"Design Size Source Side Flow Rate [m3/s]", tmpRatedSourceVolFlowHeat );
						}
					} else {
						if ( GSHP( GSHPNum ).RatedSourceVolFlowHeat > 0.0 && tmpRatedSourceVolFlowHeat > 0.0 ) {
							RatedSourceVolFlowHeatUser = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
							if ( GSHP( GSHPNum ).IsThisSized == false ) {
								ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name, 
									"Design Size Source Side Flow Rate [m3/s]", tmpRatedSourceVolFlowHeat,
									"User-Specified Source Side Flow Rate [m3/s]", RatedSourceVolFlowHeatUser );
								if ( DisplayExtraWarnings ) {
									if (( std::abs( tmpRatedSourceVolFlowHeat - RatedSourceVolFlowHeatUser ) / RatedSourceVolFlowHeatUser ) > AutoVsHardSizingThreshold ) {
										ShowMessage( "SizeHEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
										ShowContinueError( "User-Specified Source Side Flow Rate of " + RoundSigDigits( RatedSourceVolFlowHeatUser, 5 ) + " [m3/s]" );
										ShowContinueError( "differs from Design Size Source Side Flow Rate of " + RoundSigDigits( tmpRatedSourceVolFlowHeat, 5) + " [m3/s]" );
										ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									}
								}
							}
						}
					}
				}

				// Set hard-sized evaporative flow rate to be tmporary variable for registering it
				if ( !IsAutoSize && tmpRatedSourceVolFlowHeat != GSHP( GSHPNum ).RatedSourceVolFlowHeat ) {
					tmpRatedSourceVolFlowHeat = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
				}
			} else {
				if ( IsAutoSize ) {
					ShowSevereError( "Autosizing of source side flow rate requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING object=" + GSHP( GSHPNum ).Name );
					ErrorsFound = true;
				} else {
					if ( GSHP( GSHPNum ).IsThisSized == false ) {
						if ( GSHP( GSHPNum ).RatedSourceVolFlowHeat > 0.0 ) {
							ReportSizingOutput( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", GSHP( GSHPNum ).Name,
								"User-Specified Source Side Flow Rate [m3/s]", GSHP( GSHPNum ).RatedSourceVolFlowHeat );
						}
					}
				}
			}

			// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
			RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, tmpRatedSourceVolFlowHeat );

			if ( ErrorsFound ) {
				ShowFatalError( "Preceding sizing errors cause program termination" );
			}

			// create predefined report
			if ( PlantSizesOkayToFinalize ) {
				equipName = GSHP( GSHPNum ).Name;
				PreDefTableEntry( pdchMechType, equipName, "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING" );
				PreDefTableEntry( pdchMechNomEff, equipName, GSHP( GSHPNum ).RatedCOPHeat );
				PreDefTableEntry( pdchMechNomCap, equipName, GSHP( GSHPNum ).RatedCapHeat );
			}
		}
	}

	void
	CalcWatertoWaterHPCooling(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// This routine simulate the heat pump peformance in cooling mode

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
		// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const CelsiustoKelvin( KelvinConv ); // Conversion from Celsius to Kelvin
		Real64 const Tref( 283.15 ); // Reference Temperature for performance curves,10C [K]
		static std::string const RoutineName( "CalcWatertoWaterHPCooling" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 CoolCapRated; // Rated Cooling Capacity [W]
		//Real64 CoolPowerRated; // Rated Cooling Power Consumption[W]
		Real64 LoadSideVolFlowRateRated; // Rated Load Side Volumetric Flow Rate [m3/s]
		Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]
		Real64 CoolCapCoeff1; // 1st coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff2; // 2nd coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff3; // 3rd coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff4; // 4th coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff5; // 5th coefficient of the cooling capacity performance curve
		//Real64 CoolPowerCoeff1; // 1st coefficient of the cooling power consumption curve
		//Real64 CoolPowerCoeff2; // 2nd coefficient of the cooling power consumption curve
		//Real64 CoolPowerCoeff3; // 3rd coefficient of the cooling power consumption curve
		//Real64 CoolPowerCoeff4; // 4th coefficient of the cooling power consumption curve
		//Real64 CoolPowerCoeff5; // 5th coefficient of the cooling power consumption curve

		Real64 LoadSideMassFlowRate; // Load Side Mass Flow Rate [kg/s]
		Real64 LoadSideInletTemp; // Load Side Inlet Temperature [C]
		Real64 LoadSideOutletTemp; // Load side Outlet Temperature [C]
		Real64 SourceSideMassFlowRate; // Source Side Mass Flow Rate [kg/s]
		Real64 SourceSideInletTemp; // Source Side Inlet Temperature [C]
		Real64 SourceSideOutletTemp; // Source Side Outlet Temperature [C]

		Real64 func1; // Portion of the heat transfer and power equation
		Real64 func2; // Portion of the heat transfer and power equation
		Real64 func3; // Portion of the heat transfer and power equation
		Real64 func4; // Portion of the heat transfer and power equation
		Real64 Power; // Power Consumption [W]
		Real64 QLoad; // Cooling Capacity [W]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 PartLoadRatio; // Part-Load Ratio
		Real64 ReportingConstant;
		Real64 rhoLoadSide;
		Real64 rhoSourceSide;
		Real64 CpLoadSide;
		Real64 CpSourceSide;

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
		LoadSideVolFlowRateRated = GSHP( GSHPNum ).RatedLoadVolFlowCool;
		SourceSideVolFlowRateRated = GSHP( GSHPNum ).RatedSourceVolFlowCool;
		CoolCapRated = GSHP( GSHPNum ).RatedCapCool;
		//CoolPowerRated = GSHP( GSHPNum ).RatedCOPCool;
		CoolCapCoeff1 = GSHP( GSHPNum ).CoolCap1;
		CoolCapCoeff2 = GSHP( GSHPNum ).CoolCap2;
		CoolCapCoeff3 = GSHP( GSHPNum ).CoolCap3;
		CoolCapCoeff4 = GSHP( GSHPNum ).CoolCap4;
		CoolCapCoeff5 = GSHP( GSHPNum ).CoolCap5;
//		CoolPowerCoeff1 = GSHP( GSHPNum ).CoolPower1;
//		CoolPowerCoeff2 = GSHP( GSHPNum ).CoolPower2;
//		CoolPowerCoeff3 = GSHP( GSHPNum ).CoolPower3;
//		CoolPowerCoeff4 = GSHP( GSHPNum ).CoolPower4;
//		CoolPowerCoeff5 = GSHP( GSHPNum ).CoolPower5;

		LoadSideMassFlowRate = GSHPReport( GSHPNum ).LoadSideMassFlowRate;
		LoadSideInletTemp = GSHPReport( GSHPNum ).LoadSideInletTemp;
		SourceSideMassFlowRate = GSHPReport( GSHPNum ).SourceSideMassFlowRate;
		SourceSideInletTemp = GSHPReport( GSHPNum ).SourceSideInletTemp;

		// If heat pump is not operating, THEN return
		if ( ! GSHP( GSHPNum ).MustRun ) {
			return;
		}

		rhoLoadSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		rhoSourceSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		func1 = ( ( LoadSideInletTemp + CelsiustoKelvin ) / Tref );
		func2 = ( ( SourceSideInletTemp + CelsiustoKelvin ) / Tref );
		func3 = ( LoadSideMassFlowRate / ( LoadSideVolFlowRateRated * rhoLoadSide ) );
		func4 = ( SourceSideMassFlowRate / ( SourceSideVolFlowRateRated * rhoSourceSide ) );

		QLoad = CoolCapRated * ( CoolCapCoeff1 + ( func1 * CoolCapCoeff2 ) + ( func2 * CoolCapCoeff3 ) + ( func3 * CoolCapCoeff4 ) + ( func4 * CoolCapCoeff5 ) );
//		Power = CoolPowerRated * ( CoolPowerCoeff1 + ( func1 * CoolPowerCoeff2 ) + ( func2 * CoolPowerCoeff3 ) + ( func3 * CoolPowerCoeff4 ) + ( func4 * CoolPowerCoeff5 ) );
		Power = QLoad / GSHP( GSHPNum ).RatedCOPCool;

		if ( ( QLoad <= 0.0 || Power <= 0.0 ) && ! WarmupFlag ) {
			if ( QLoad <= 0.0 ) {
				if ( GSHP( GSHPNum ).CoolCapNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).CoolCapNegativeCounter;
					ShowWarningError( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Cooling capacity curve output is <= 0.0 (" + TrimSigDigits( QLoad, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\": Cooling capacity curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).CoolCapNegativeIndex, QLoad, QLoad );
				}
			}
			if ( Power <= 0.0 ) {
				if ( GSHP( GSHPNum ).CoolPowerNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).CoolPowerNegativeCounter;
					ShowWarningError( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Cooling compressor power curve output is <= 0.0 (" + TrimSigDigits( Power, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\": Cooling compressor power curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).CoolPowerNegativeIndex, Power, Power );
				}
			}

			QLoad = 0.0;
			Power = 0.0;

		}

		QSource = QLoad + Power; //assume no losses

		//Control Strategy
		if ( std::abs( MyLoad ) < QLoad && QLoad != 0.0 ) {
			PartLoadRatio = std::abs( MyLoad ) / QLoad;
			QLoad = std::abs( MyLoad );
			Power *= PartLoadRatio;
			QSource *= PartLoadRatio;
		}

		CpLoadSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		CpSourceSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		LoadSideOutletTemp = LoadSideInletTemp - QLoad / ( LoadSideMassFlowRate * CpLoadSide );
		SourceSideOutletTemp = SourceSideInletTemp + QSource / ( SourceSideMassFlowRate * CpSourceSide );

		ReportingConstant = TimeStepSys * SecInHour;

		GSHPReport( GSHPNum ).Power = Power;
		GSHPReport( GSHPNum ).Energy = Power * ReportingConstant;
		GSHPReport( GSHPNum ).QSource = QSource;
		GSHPReport( GSHPNum ).QLoad = QLoad;
		GSHPReport( GSHPNum ).QSourceEnergy = QSource * ReportingConstant;
		GSHPReport( GSHPNum ).QLoadEnergy = QLoad * ReportingConstant;
		GSHPReport( GSHPNum ).LoadSideOutletTemp = LoadSideOutletTemp;
		GSHPReport( GSHPNum ).SourceSideOutletTemp = SourceSideOutletTemp;
	}

	void
	CalcWatertoWaterHPHeating(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// This routine simulate the heat pump peformance in heating mode

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
		// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const CelsiustoKelvin( KelvinConv ); // Conversion from Celsius to Kelvin
		Real64 const Tref( 283.15 ); // Reference Temperature for performance curves,10C [K]
		static std::string const RoutineName( "CalcWatertoWaterHPHeating" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 HeatCapRated; // Rated Heating Capacity [W]
		//Real64 HeatPowerRated; // Rated Heating Compressor Power[W]
		Real64 LoadSideVolFlowRateRated; // Rated Load Side Volumetric Flow Rate [m3/s]
		Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]
		Real64 HeatCapCoeff1; // 1st coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff2; // 2nd coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff3; // 3rd coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff4; // 4th coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff5; // 5th coefficient of the heating capacity performance curve
		//Real64 HeatPowerCoeff1; // 1st coefficient of the heating power consumption curve
		//Real64 HeatPowerCoeff2; // 2nd coefficient of the heating power consumption curve
		//Real64 HeatPowerCoeff3; // 3rd coefficient of the heating power consumption curve
		//Real64 HeatPowerCoeff4; // 4th coefficient of the heating power consumption curve
		//Real64 HeatPowerCoeff5; // 5th coefficient of the heating power consumption curve
		Real64 LoadSideMassFlowRate; // Load Side Mass Flow Rate [kg/s]
		Real64 LoadSideInletTemp; // Load Side Inlet Temperature [C]
		Real64 LoadSideOutletTemp; // Load side Outlet Temperature [C]
		Real64 SourceSideMassFlowRate; // Source Side Mass Flow Rate [kg/s]
		Real64 SourceSideInletTemp; // Source Side Inlet Temperature [C]
		Real64 SourceSideOutletTemp; // Source Side Outlet Temperature [C]
		Real64 func1; // Portion of the heat transfer and power equation
		Real64 func2; // Portion of the heat transfer and power equation
		Real64 func3; // Portion of the heat transfer and power equation
		Real64 func4; // Portion of the heat transfer and power equation
		Real64 Power; // Power Consumption [W]
		Real64 QLoad; // Cooling Capacity [W]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 PartLoadRatio; // Part Load Ratio
		Real64 ReportingConstant;
		Real64 rhoLoadSide;
		Real64 rhoSourceSide;
		Real64 CpLoadSide;
		Real64 CpSourceSide;

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
		LoadSideVolFlowRateRated = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
		SourceSideVolFlowRateRated = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
		HeatCapRated = GSHP( GSHPNum ).RatedCapHeat;
		//HeatPowerRated = GSHP( GSHPNum ).RatedCOPHeat;
		HeatCapCoeff1 = GSHP( GSHPNum ).HeatCap1;
		HeatCapCoeff2 = GSHP( GSHPNum ).HeatCap2;
		HeatCapCoeff3 = GSHP( GSHPNum ).HeatCap3;
		HeatCapCoeff4 = GSHP( GSHPNum ).HeatCap4;
		HeatCapCoeff5 = GSHP( GSHPNum ).HeatCap5;
//		HeatPowerCoeff1 = GSHP( GSHPNum ).HeatPower1;
//		HeatPowerCoeff2 = GSHP( GSHPNum ).HeatPower2;
//		HeatPowerCoeff3 = GSHP( GSHPNum ).HeatPower3;
//		HeatPowerCoeff4 = GSHP( GSHPNum ).HeatPower4;
//		HeatPowerCoeff5 = GSHP( GSHPNum ).HeatPower5;

		LoadSideMassFlowRate = GSHPReport( GSHPNum ).LoadSideMassFlowRate;
		LoadSideInletTemp = GSHPReport( GSHPNum ).LoadSideInletTemp;
		SourceSideMassFlowRate = GSHPReport( GSHPNum ).SourceSideMassFlowRate;
		SourceSideInletTemp = GSHPReport( GSHPNum ).SourceSideInletTemp;

		// If heat pump is not operating, THEN return
		if ( ! GSHP( GSHPNum ).MustRun ) {
			return;
		}
		rhoLoadSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		rhoSourceSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		func1 = ( ( LoadSideInletTemp + CelsiustoKelvin ) / Tref );
		func2 = ( ( SourceSideInletTemp + CelsiustoKelvin ) / Tref );
		func3 = ( LoadSideMassFlowRate / ( LoadSideVolFlowRateRated * rhoLoadSide ) );
		func4 = ( SourceSideMassFlowRate / ( SourceSideVolFlowRateRated * rhoSourceSide ) );

		QLoad = HeatCapRated * ( HeatCapCoeff1 + ( func1 * HeatCapCoeff2 ) + ( func2 * HeatCapCoeff3 ) + ( func3 * HeatCapCoeff4 ) + ( func4 * HeatCapCoeff5 ) );
//		Power = HeatPowerRated * ( HeatPowerCoeff1 + ( func1 * HeatPowerCoeff2 ) + ( func2 * HeatPowerCoeff3 ) + ( func3 * HeatPowerCoeff4 ) + ( func4 * HeatPowerCoeff5 ) );
		Power = QLoad / GSHP( GSHPNum ).RatedCOPHeat;

		if ( ( QLoad <= 0.0 || Power <= 0.0 ) && ! WarmupFlag ) {
			if ( QLoad <= 0.0 ) {
				if ( GSHP( GSHPNum ).HeatCapNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).HeatCapNegativeCounter;
					ShowWarningError( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Heating capacity curve output is <= 0.0 (" + TrimSigDigits( QLoad, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\": Heating capacity curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).HeatCapNegativeIndex, QLoad, QLoad );
				}
			}
			if ( Power <= 0.0 ) {
				if ( GSHP( GSHPNum ).HeatPowerNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).HeatPowerNegativeCounter;
					ShowWarningError( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Heating compressor power curve output is <= 0.0 (" + TrimSigDigits( Power, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\": Heating compressor power curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).HeatPowerNegativeIndex, Power, Power );
				}
			}

			QLoad = 0.0;
			Power = 0.0;

		}

		QSource = QLoad - Power; //assume no losses

		//Control Strategy
		if ( std::abs( MyLoad ) < QLoad && QLoad != 0.0 ) {
			PartLoadRatio = std::abs( MyLoad ) / QLoad;
			QLoad = std::abs( MyLoad );
			Power *= PartLoadRatio;
			QSource *= PartLoadRatio;
		}

		CpLoadSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		CpSourceSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		LoadSideOutletTemp = LoadSideInletTemp + QLoad / ( LoadSideMassFlowRate * CpLoadSide );
		SourceSideOutletTemp = SourceSideInletTemp - QSource / ( SourceSideMassFlowRate * CpSourceSide );

		ReportingConstant = TimeStepSys * SecInHour;

		GSHPReport( GSHPNum ).Power = Power;
		GSHPReport( GSHPNum ).Energy = Power * ReportingConstant;
		GSHPReport( GSHPNum ).QSource = QSource;
		GSHPReport( GSHPNum ).QLoad = QLoad;
		GSHPReport( GSHPNum ).QSourceEnergy = QSource * ReportingConstant;
		GSHPReport( GSHPNum ).QLoadEnergy = QLoad * ReportingConstant;
		GSHPReport( GSHPNum ).LoadSideOutletTemp = LoadSideOutletTemp;
		GSHPReport( GSHPNum ).SourceSideOutletTemp = SourceSideOutletTemp;
	}

	void
	UpdateGSHPRecords( int const GSHPNum ) // GSHP number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Kenneth Tang
		//       DATE WRITTEN:    March 2005

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS:

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

		LoadSideInletNode = GSHP( GSHPNum ).LoadSideInletNodeNum;
		LoadSideOutletNode = GSHP( GSHPNum ).LoadSideOutletNodeNum;
		SourceSideInletNode = GSHP( GSHPNum ).SourceSideInletNodeNum;
		SourceSideOutletNode = GSHP( GSHPNum ).SourceSideOutletNodeNum;

		if ( ! GSHP( GSHPNum ).MustRun ) {
			// Heatpump is off; just pass through conditions
			GSHPReport( GSHPNum ).Power = 0.0;
			GSHPReport( GSHPNum ).Energy = 0.0;
			GSHPReport( GSHPNum ).QSource = 0.0;
			GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
			GSHPReport( GSHPNum ).QLoad = 0.0;
			GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
			GSHPReport( GSHPNum ).LoadSideOutletTemp = GSHPReport( GSHPNum ).LoadSideInletTemp;
			GSHPReport( GSHPNum ).SourceSideOutletTemp = GSHPReport( GSHPNum ).SourceSideInletTemp;
		}

		Node( SourceSideOutletNode ).Temp = GSHPReport( GSHPNum ).SourceSideOutletTemp;
		Node( LoadSideOutletNode ).Temp = GSHPReport( GSHPNum ).LoadSideOutletTemp;
	}

} // HeatPumpWaterToWaterSimple

} // EnergyPlus
