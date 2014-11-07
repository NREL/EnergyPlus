// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <Humidifiers.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace Humidifiers {

	// Module containing the routines dealing with humidifiers

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   September 2000
	//       MODIFIED       B Griffith, Aug. 2006 added water system interactions
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and routines required to model humidifier
	// components in the EnergyPlus HVAC simulation

	// METHODOLOGY EMPLOYED:
	// The humidifier encompasses not just the component but also its
	// control. The humidifier adds moisture to its air inlet to meet
	// the HumRatMin setpoint at its exit node. The HumRatMin is set by
	// an external setpoint manager.

	// REFERENCES: ASHRAE HVAC 2 Toolkit, page 4-112

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
	using namespace DataLoopNode;
	using DataEnvironment::OutBaroPress;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SetPointErrorFlag;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const Humidifier_Steam_Electric( 1 );

	FArray1D_string const HumidifierType( 1, std::string( "Humidifier:Steam:Electric" ) );

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumHumidifiers( 0 ); // number of humidifiers of all types
	int NumElecSteamHums( 0 ); // number of electric steam humidifiers
	FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	FArray1D< HumidifierData > Humidifier;

	// Functions

	void
	SimHumidifier(
		std::string const & CompName, // name of the humidifier unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex // Pointer to Humidifier Unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manage the simulation of an air humidifier

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// NA

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
		int HumNum; // index of humidifier unit being simulated
		static bool GetInputFlag( true ); // First time, input is "gotten"
		Real64 WaterAddNeeded; // output in kg/s needed from humidifier to meet humidity setpoint

		if ( GetInputFlag ) {
			GetHumidifierInput();
			GetInputFlag = false;
		}

		// Get the humidifier unit index
		if ( CompIndex == 0 ) {
			HumNum = FindItemInList( CompName, Humidifier.Name(), NumHumidifiers );
			if ( HumNum == 0 ) {
				ShowFatalError( "SimHumidifier: Unit not found=" + CompName );
			}
			CompIndex = HumNum;
		} else {
			HumNum = CompIndex;
			if ( HumNum > NumHumidifiers || HumNum < 1 ) {
				ShowFatalError( "SimHumidifier:  Invalid CompIndex passed=" + TrimSigDigits( HumNum ) + ", Number of Units=" + TrimSigDigits( NumHumidifiers ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( HumNum ) ) {
				if ( CompName != Humidifier( HumNum ).Name ) {
					ShowFatalError( "SimHumidifier: Invalid CompIndex passed=" + TrimSigDigits( HumNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + Humidifier( HumNum ).Name );
				}
				CheckEquipName( HumNum ) = false;
			}
		}
		if ( HumNum <= 0 ) {
			ShowFatalError( "SimHumidifier: Unit not found=" + CompName );
		}

		InitHumidifier( HumNum );

		ControlHumidifier( HumNum, WaterAddNeeded );

		// call the correct humidifier calculation routine
		{ auto const SELECT_CASE_var( Humidifier( HumNum ).HumType_Code );

		if ( SELECT_CASE_var == Humidifier_Steam_Electric ) { // 'HUMIDIFIER:STEAM:ELECTRIC'

			CalcElecSteamHumidifier( HumNum, WaterAddNeeded );

		} else {
			ShowSevereError( "SimHumidifier: Invalid Humidifier Type Code=" + TrimSigDigits( Humidifier( HumNum ).HumType_Code ) );
			ShowContinueError( "...Component Name=[" + CompName + "]." );
			ShowFatalError( "Preceding Condition causes termination." );

		}}

		UpdateReportWaterSystem( HumNum );

		UpdateHumidifier( HumNum );

		ReportHumidifier( HumNum );

	}

	void
	GetHumidifierInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for humidifiers and stores it in humidifier data structures.

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using WaterManager::SetupTankDemandComponent;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetHumidifierInputs: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HumidifierIndex; // loop index
		int HumNum; // current humidifier number
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // for ease in getting objects
		FArray1D_string Alphas; // Alpha input items for object
		FArray1D_string cAlphaFields; // Alpha field names
		FArray1D_string cNumericFields; // Numeric field names
		FArray1D< Real64 > Numbers; // Numeric input items for object
		FArray1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		FArray1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file

		CurrentModuleObject = "Humidifier:Steam:Electric";
		NumElecSteamHums = GetNumObjectsFound( CurrentModuleObject );
		NumHumidifiers = NumElecSteamHums;
		// allocate the data array
		Humidifier.allocate( NumHumidifiers );
		CheckEquipName.allocate( NumHumidifiers );
		CheckEquipName = true;

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		Alphas.allocate( NumAlphas );
		Alphas = "";
		cAlphaFields.allocate( NumAlphas );
		cAlphaFields = "";
		cNumericFields.allocate( NumNumbers );
		cNumericFields = "";
		Numbers.allocate( NumNumbers );
		Numbers = 0.0;
		lAlphaBlanks.allocate( NumAlphas );
		lAlphaBlanks = true;
		lNumericBlanks.allocate( NumNumbers );
		lNumericBlanks = true;

		// loop over electric steam humidifiers and load the input data
		for ( HumidifierIndex = 1; HumidifierIndex <= NumElecSteamHums; ++HumidifierIndex ) {
			GetObjectItem( CurrentModuleObject, HumidifierIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			HumNum = HumidifierIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Humidifier.Name(), HumNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Humidifier( HumNum ).Name = Alphas( 1 );
			//    Humidifier(HumNum)%HumType  = TRIM(CurrentModuleObject)
			Humidifier( HumNum ).HumType_Code = Humidifier_Steam_Electric;
			Humidifier( HumNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				Humidifier( HumNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				Humidifier( HumNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( Humidifier( HumNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}
			Humidifier( HumNum ).NomCapVol = Numbers( 1 );
			Humidifier( HumNum ).NomPower = Numbers( 2 );
			Humidifier( HumNum ).FanPower = Numbers( 3 );
			Humidifier( HumNum ).StandbyPower = Numbers( 4 );
			Humidifier( HumNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Humidifier( HumNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			//  A5; \field Name of Water Storage Tank
			if ( lAlphaBlanks( 5 ) ) {
				Humidifier( HumNum ).SuppliedByWaterSystem = false;
			} else { // water from storage tank
				SetupTankDemandComponent( Alphas( 1 ), CurrentModuleObject, Alphas( 5 ), ErrorsFound, Humidifier( HumNum ).WaterTankID, Humidifier( HumNum ).WaterTankDemandARRID );
				Humidifier( HumNum ).SuppliedByWaterSystem = true;
			}

		}

		for ( HumNum = 1; HumNum <= NumHumidifiers; ++HumNum ) {
			// Setup Report variables for the Humidifiers
			if ( Humidifier( HumNum ).SuppliedByWaterSystem ) {
				SetupOutputVariable( "Humidifier Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).WaterConsRate, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Water Volume [m3]", Humidifier( HumNum ).WaterCons, "System", "Sum", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Storage Tank Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).TankSupplyVdot, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Storage Tank Water Volume [m3]", Humidifier( HumNum ).TankSupplyVol, "System", "Sum", Humidifier( HumNum ).Name, _, "Water", "HUMIDIFIER", _, "SYSTEM" );
				SetupOutputVariable( "Humidifier Starved Storage Tank Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).StarvedSupplyVdot, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Starved Storage Tank Water Volume [m3]", Humidifier( HumNum ).StarvedSupplyVol, "System", "Sum", Humidifier( HumNum ).Name, _, "Water", "HUMIDIFIER", _, "SYSTEM" );
				SetupOutputVariable( "Humidifier Mains Water Volume [m3]", Humidifier( HumNum ).StarvedSupplyVol, "System", "Sum", Humidifier( HumNum ).Name, _, "MainsWater", "HUMIDIFIER", _, "SYSTEM" );

			} else {
				SetupOutputVariable( "Humidifier Water Volume Flow Rate [m3/s]", Humidifier( HumNum ).WaterConsRate, "System", "Average", Humidifier( HumNum ).Name );
				SetupOutputVariable( "Humidifier Water Volume [m3]", Humidifier( HumNum ).WaterCons, "System", "Sum", Humidifier( HumNum ).Name, _, "WATER", "HUMIDIFIER", _, "System" );
				SetupOutputVariable( "Humidifier Mains Water Volume [m3]", Humidifier( HumNum ).WaterCons, "System", "Sum", Humidifier( HumNum ).Name, _, "MAINSWATER", "HUMIDIFIER", _, "System" );

			}
			SetupOutputVariable( "Humidifier Electric Power [W]", Humidifier( HumNum ).ElecUseRate, "System", "Average", Humidifier( HumNum ).Name );
			SetupOutputVariable( "Humidifier Electric Energy [J]", Humidifier( HumNum ).ElecUseEnergy, "System", "Sum", Humidifier( HumNum ).Name, _, "ELECTRICITY", "HUMIDIFIER", _, "System" );

		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input." );
		}

	}

	void
	InitHumidifier( int const HumNum ) // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Humidifier Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::RhoH2O;
		using DataHVACGlobals::DoSetPointTest;
		using InputProcessor::SameString;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using EMSManager::iHumidityRatioMinSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::FindGlycol;
		using FluidProperties::FindRefrigerant;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CalledFrom( "Humidifier:InitHumidifier" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // inlet node number
		int OutNode; // outlet node number
		int NumHum;
		int RefrigerantIndex; // refiferant index
		int WaterIndex; // fluid type index
		Real64 NominalPower; // Nominal power input to humidifier, W
		Real64 WaterSpecHeat; // specific heat of water , J/kgK
		Real64 SteamSatEnthalpy; // enthalpy of saturated steam at 100C, J/kg
		Real64 WaterSatEnthalpy; // enthalpy of saturated water at 100C, J/kg

		static bool MyOneTimeFlag( true );
		static FArray1D_bool MyEnvrnFlag;
		static bool MySetPointCheckFlag( true );
		static FArray1D_bool MySizeFlag;

		// do one time initializations
		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumHumidifiers );
			MySizeFlag.allocate( NumHumidifiers );
			MyEnvrnFlag = true;

			MyOneTimeFlag = false;
			MySizeFlag = true;

		}

		// do sizing calculation
		if ( MySizeFlag( HumNum ) ) {
			SizeHumidifier( HumNum );
			MySizeFlag( HumNum ) = false;
		}

		if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
			for ( NumHum = 1; NumHum <= NumHumidifiers; ++NumHum ) {
				OutNode = Humidifier( NumHum ).AirOutNode;

				if ( OutNode > 0 ) {
					if ( Node( OutNode ).HumRatMin == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							ShowSevereError( "Humidifiers: Missing humidity setpoint for " + HumidifierType( Humidifier( NumHum ).HumType_Code ) + " = " + Humidifier( HumNum ).Name );
							ShowContinueError( "  use a Setpoint Manager with Control Variable = \"MinimumHumidityRatio\" to establish" "a setpoint at the humidifier outlet node." );
							ShowContinueError( "  expecting it on Node=\"" + NodeID( OutNode ) + "\"." );
							SetPointErrorFlag = true;
						} else {
							CheckIfNodeSetPointManagedByEMS( OutNode, iHumidityRatioMinSetPoint, SetPointErrorFlag );
							if ( SetPointErrorFlag ) {
								ShowSevereError( "Humidifiers: Missing humidity setpoint for " + HumidifierType( Humidifier( NumHum ).HumType_Code ) + " = " + Humidifier( HumNum ).Name );
								ShowContinueError( "  use a Setpoint Manager with Control Variable = \"MinimumHumidityRatio\" to establish" "a setpoint at the humidifier outlet node." );
								ShowContinueError( "  expecting it on Node=\"" + NodeID( OutNode ) + "\"." );
								ShowContinueError( "  or use an EMS actuator to control minimum humidity ratio to establish" "a setpoint at the humidifier outlet node." );
							}
						}
					}
				}
			}
			MySetPointCheckFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( HumNum ) = true;
		}

		// do these initializations every HVAC time step
		InNode = Humidifier( HumNum ).AirInNode;
		OutNode = Humidifier( HumNum ).AirOutNode;
		Humidifier( HumNum ).HumRatSet = Node( OutNode ).HumRatMin;
		Humidifier( HumNum ).AirInTemp = Node( InNode ).Temp;
		Humidifier( HumNum ).AirInHumRat = Node( InNode ).HumRat;
		Humidifier( HumNum ).AirInEnthalpy = Node( InNode ).Enthalpy;
		Humidifier( HumNum ).AirInMassFlowRate = Node( InNode ).MassFlowRate;
		Humidifier( HumNum ).WaterAdd = 0.0;
		Humidifier( HumNum ).ElecUseEnergy = 0.0;
		Humidifier( HumNum ).ElecUseRate = 0.0;
		Humidifier( HumNum ).WaterCons = 0.0;
		Humidifier( HumNum ).WaterConsRate = 0.0;

	}

	void
	SizeHumidifier( int const HumNum ) // number of the current humidifier being sized
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, UCF/FSEC,
		//       DATE WRITTEN   March, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for for sizing electric steam humidifier nominal electric power.

		// METHODOLOGY EMPLOYED:
		// Uses user sepecified nominal capacity in m3/s and water enthalpy change required to
		// vaporize water from a reference temperature of 20.0C. to steam at 100.0C.
		//  m_dot = Nominal Capacity [m3/s] * Density of water at 5.05 [kg/m3]
		//  Nominal Capacity =  m_dot [kg/s] * delta_enthalpy [J/kg]

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::RhoH2O;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::FindGlycol;
		using FluidProperties::FindRefrigerant;
		using General::RoundSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CalledFrom( "Humidifier:SizeHumidifier" );
		Real64 const Tref( 20.0 ); // Reference temp of water for rated capacity calac [C]
		Real64 const TSteam( 100.0 ); // saturated steam temperatur generated by Humidifier [C]

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumHum;
		int RefrigerantIndex; // refiferant index
		int WaterIndex; // fluid type index
		Real64 NominalPower; // Nominal power input to humidifier, W
		Real64 WaterSpecHeatAvg; // specific heat of water, J/kgK
		Real64 SteamSatEnthalpy; // enthalpy of saturated steam at 100C, J/kg
		Real64 WaterSatEnthalpy; // enthalpy of saturated water at 100C, J/kg

		if ( Humidifier( HumNum ).HumType_Code == Humidifier_Steam_Electric ) {
			Humidifier( HumNum ).NomCap = RhoH2O( InitConvTemp ) * Humidifier( HumNum ).NomCapVol;

			RefrigerantIndex = FindRefrigerant( fluidNameSteam );
			WaterIndex = FindGlycol( fluidNameWater );
			SteamSatEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, TSteam, 1.0, RefrigerantIndex, CalledFrom );
			WaterSatEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, TSteam, 0.0, RefrigerantIndex, CalledFrom );
			WaterSpecHeatAvg = 0.5 * ( GetSpecificHeatGlycol( fluidNameWater, TSteam, WaterIndex, CalledFrom ) + GetSpecificHeatGlycol( fluidNameWater, Tref, WaterIndex, CalledFrom ) );

			NominalPower = Humidifier( HumNum ).NomCap * ( ( SteamSatEnthalpy - WaterSatEnthalpy ) + WaterSpecHeatAvg * ( TSteam - Tref ) );

			if ( Humidifier( HumNum ).NomPower == AutoSize ) {
				Humidifier( HumNum ).NomPower = NominalPower;
				ReportSizingOutput( "Humidifier:Steam:Electric", Humidifier( HumNum ).Name, "Rated Power [W]", Humidifier( HumNum ).NomPower );
			} else if ( Humidifier( HumNum ).NomPower >= 0.0 && Humidifier( HumNum ).NomCap > 0.0 ) {
				if ( Humidifier( HumNum ).NomPower < NominalPower ) {
					ShowWarningError( "Humidifier:Steam:Electric: specified Rated Power is less than nominal Rated " " Power for electric steam humidifier = " + Humidifier( HumNum ).Name + ". " );
					ShowContinueError( " specified Rated Power = " + RoundSigDigits( Humidifier( HumNum ).NomPower, 2 ) );
					ShowContinueError( " while expecting a minimum Rated Power = " + RoundSigDigits( NominalPower, 2 ) );
				}
			} else {
				ShowWarningError( "Humidifier:Steam:Electric: specified nominal capacity is zero " " for electric steam humidifier = " + Humidifier( HumNum ).Name + ". " );
				ShowContinueError( " For zero rated capacity humidifier the rated power is zero." );
			}
		}

	}

	void
	ControlHumidifier(
		int const HumNum, // number of the current humidifier being simulated
		Real64 & WaterAddNeeded // moisture addition rate needed to meet minimum humidity ratio setpoint [kg/s]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets the output required from the humidifier

		// METHODOLOGY EMPLOYED:
		// Uses a minimum humidity setpoint and water mass balance to calculate moisture addition needed

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyWFnTdbRhPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		static std::string const RoutineName( "ControlHumidifier" );

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool UnitOn; // unit on flag
		Real64 AirMassFlowRate; // air mass flow rate [kg/s]
		Real64 HumRatSatIn; // humidity ratio at saturation at the inlet temperature  [kg H2O / kg dry air]

		AirMassFlowRate = 0.0;
		UnitOn = true;
		if ( Humidifier( HumNum ).HumRatSet <= 0.0 ) UnitOn = false;
		AirMassFlowRate = Humidifier( HumNum ).AirInMassFlowRate;
		if ( AirMassFlowRate <= SmallMassFlow ) UnitOn = false;
		if ( GetCurrentScheduleValue( Humidifier( HumNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( Humidifier( HumNum ).AirInHumRat >= Humidifier( HumNum ).HumRatSet ) UnitOn = false;
		HumRatSatIn = PsyWFnTdbRhPb( Humidifier( HumNum ).AirInTemp, 1.0, OutBaroPress, RoutineName );
		if ( Humidifier( HumNum ).AirInHumRat >= HumRatSatIn ) UnitOn = false;
		if ( UnitOn ) {
			// AirMassFlowRate*AirInHumRat + WaterAddNeeded = AirMassFlowRate*HumRatSet
			WaterAddNeeded = AirMassFlowRate * ( Humidifier( HumNum ).HumRatSet - Humidifier( HumNum ).AirInHumRat );
		} else {
			WaterAddNeeded = 0.0;
		}

	}

	void
	CalcElecSteamHumidifier(
		int const HumNum, // number of the current humidifier being simulated
		Real64 const WaterAddNeeded // moisture addition rate set by controller [kg/s]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the electricity consumption and the outlet conditions for an electric steam
		// humidifier, given the inlet conditions and the steam addition rate.

		// METHODOLOGY EMPLOYED:
		// Uses energy and mass balance as well as pschrometric relations.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit, page 4-112
		// 1997 ASHRAE Handbook Fundamentals, page 6.18

		// Using/Aliasing
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyTdbFnHW;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::RhoH2O;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcElecSteamHumidifier" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 AirMassFlowRate; // air mass flow rate [kg/s]
		Real64 HumRatSatOut; // humidity ratio at saturation at the outlet temperature [kg H2O / kg dry air]
		Real64 HumRatSatIn; // humidity ratio at saturation at the inlet temperature  [kg H2O / kg dry air]
		Real64 WaterAddRate; // moisture addition rate by humidifier [kg/s]
		Real64 WaterAddNeededMax; // moisture addition rate set by controller, limited by humidifier capacity
		Real64 AirOutEnthalpy; // outlet air enthalpy [J/kg]
		Real64 AirOutHumRat; // outlet air humidity ratio [kg H2O / kg dry air]
		Real64 AirOutTemp; // outlet air temperature [C]
		Real64 WaterInEnthalpy; // enthalpy of the inlet steam [J/kg]
		Real64 HumRatSatApp; // the approximate humidity ratio where the line drawn between inlet and desired outlet conditions
		// crosses the saturation line.
		Real64 WaterDens; // density of liquid water [kg/m3]

		AirMassFlowRate = Humidifier( HumNum ).AirInMassFlowRate;
		HumRatSatIn = PsyWFnTdbRhPb( Humidifier( HumNum ).AirInTemp, 1.0, OutBaroPress, RoutineName );
		HumRatSatOut = 0.0;
		HumRatSatApp = 0.0;
		WaterInEnthalpy = 2676125.0; // At 100 C
		WaterDens = RhoH2O( InitConvTemp );
		WaterAddNeededMax = min( WaterAddNeeded, Humidifier( HumNum ).NomCap );
		if ( WaterAddNeededMax > 0.0 ) {
			//   ma*W1 + mw = ma*W2
			//   ma*h1 + mw*hw = ma*h2
			// where ma is air mass flow rate; h1,W1 are the inlet enthalpy and humidity ratio; h2 and W2 are
			// the outlet enthalpy and humidity ratio; mw is the steam mass flow rate; hw is the steam enthalpy.
			// Setting mw equal to the desired water addition rate, use the above 2 equations to calculate the
			// outlet conditions
			AirOutEnthalpy = ( AirMassFlowRate * Humidifier( HumNum ).AirInEnthalpy + WaterAddNeededMax * WaterInEnthalpy ) / AirMassFlowRate;
			AirOutHumRat = ( AirMassFlowRate * Humidifier( HumNum ).AirInHumRat + WaterAddNeededMax ) / AirMassFlowRate;
			AirOutTemp = PsyTdbFnHW( AirOutEnthalpy, AirOutHumRat );
			HumRatSatOut = PsyWFnTdbRhPb( AirOutTemp, 1.0, OutBaroPress, RoutineName );
			if ( AirOutHumRat <= HumRatSatOut ) {
				// If the outlet condition is below the saturation curve, the desired moisture addition rate can be met.
				WaterAddRate = WaterAddNeededMax;
			} else {
				// The desired moisture addition rate results in an outlet state above the saturation curve. We need to
				// find the point where the line drawn between state 1 (inlet) and state 2 (our desired outlet) crosses
				// the saturation curve. This will be the new outlet condition. Rather than iterate to obtain this point,
				// we find it approximately by solving for the point where 2 lines cross: the first drawn from
				// state 1 to state 2, the second from T1, W1s to T2, W2s; where T1 is the inlet temperature, W1s is
				// the humidity ratio at saturation at temperature T1; and T2 is the desired outlet temperature, W2s
				// is the humidity ratio at saturation at temperature T2. The 2 lines are given by the equations:
				//   W = W1 + ((W2-W1)/(T2-T1))*(T-T1)
				//   W = W1s + ((W2s-W1s)/(T2-T1))*(T-T1)
				// Solving for the point where the line cross (T3,W3):
				//   W3 = W1 + ((W2-W1)*(W1s-W1))/(W2-W2s + W1s-W1)
				//   T3 = T1 + (W3-W1)*((T2-T1)/(W2-W1))  ! "T1 +" added by Shirey 8/12/04  That's correct! [WFB 9/29/2004]
				HumRatSatApp = Humidifier( HumNum ).AirInHumRat + ( AirOutHumRat - Humidifier( HumNum ).AirInHumRat ) * ( HumRatSatIn - Humidifier( HumNum ).AirInHumRat ) / ( AirOutHumRat - HumRatSatOut + HumRatSatIn - Humidifier( HumNum ).AirInHumRat );
				AirOutTemp = Humidifier( HumNum ).AirInTemp + ( HumRatSatApp - Humidifier( HumNum ).AirInHumRat ) * ( ( AirOutTemp - Humidifier( HumNum ).AirInTemp ) / ( AirOutHumRat - Humidifier( HumNum ).AirInHumRat ) );
				// This point isn't quite on the saturation curve since we made a linear approximation of the curve,
				// but the temperature should be very close to the correct outlet temperature. We will use this temperature
				// as the outlet temperature and move to the saturation curve for the outlet humidity and enthalpy
				AirOutHumRat = PsyWFnTdbRhPb( AirOutTemp, 1.0, OutBaroPress, RoutineName );
				AirOutEnthalpy = PsyHFnTdbW( AirOutTemp, AirOutHumRat );
				WaterAddRate = AirMassFlowRate * ( AirOutHumRat - Humidifier( HumNum ).AirInHumRat );
			}

		} else {
			WaterAddRate = 0.0;
			AirOutEnthalpy = Humidifier( HumNum ).AirInEnthalpy;
			AirOutTemp = Humidifier( HumNum ).AirInTemp;
			AirOutHumRat = Humidifier( HumNum ).AirInHumRat;
		}

		Humidifier( HumNum ).WaterAdd = WaterAddRate;
		Humidifier( HumNum ).AirOutTemp = AirOutTemp;
		Humidifier( HumNum ).AirOutHumRat = AirOutHumRat;
		Humidifier( HumNum ).AirOutEnthalpy = AirOutEnthalpy;
		Humidifier( HumNum ).AirOutMassFlowRate = AirMassFlowRate;

		if ( WaterAddRate > 0.0 ) {
			Humidifier( HumNum ).ElecUseRate = ( WaterAddRate / Humidifier( HumNum ).NomCap ) * Humidifier( HumNum ).NomPower + Humidifier( HumNum ).FanPower + Humidifier( HumNum ).StandbyPower;
		} else if ( GetCurrentScheduleValue( Humidifier( HumNum ).SchedPtr ) > 0.0 ) {
			Humidifier( HumNum ).ElecUseRate = Humidifier( HumNum ).StandbyPower;
		} else {
			Humidifier( HumNum ).ElecUseRate = 0.0;
		}
		Humidifier( HumNum ).WaterConsRate = Humidifier( HumNum ).WaterAdd / WaterDens;

	}

	void
	UpdateReportWaterSystem( int const HumNum ) // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Aug. 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect water system calculations , update and report them

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataWater::WaterStorage;
		using DataGlobals::SecInHour;
		using DataGlobals::BeginTimeStepFlag;
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
		Real64 AvailTankVdot;
		Real64 TankSupplyVdot;
		Real64 StarvedVdot;

		// set demand request in WaterStorage if needed.
		if ( Humidifier( HumNum ).SuppliedByWaterSystem ) {
			WaterStorage( Humidifier( HumNum ).WaterTankID ).VdotRequestDemand( Humidifier( HumNum ).WaterTankDemandARRID ) = Humidifier( HumNum ).WaterConsRate;

			AvailTankVdot = WaterStorage( Humidifier( HumNum ).WaterTankID ).VdotAvailDemand( Humidifier( HumNum ).WaterTankDemandARRID ); // check what tank can currently provide

			StarvedVdot = 0.0;
			TankSupplyVdot = Humidifier( HumNum ).WaterConsRate; // init
			if ( ( AvailTankVdot < Humidifier( HumNum ).WaterConsRate ) && ( ! ( BeginTimeStepFlag ) ) ) { // calculate starved flow
				StarvedVdot = Humidifier( HumNum ).WaterConsRate - AvailTankVdot;
				TankSupplyVdot = AvailTankVdot;
			}

			Humidifier( HumNum ).TankSupplyVdot = TankSupplyVdot;
			Humidifier( HumNum ).TankSupplyVol = TankSupplyVdot * ( TimeStepSys * SecInHour );
			Humidifier( HumNum ).StarvedSupplyVdot = StarvedVdot;
			Humidifier( HumNum ).StarvedSupplyVol = StarvedVdot * ( TimeStepSys * SecInHour );

		}

	}

	void
	UpdateHumidifier( int const HumNum ) // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Moves humidifier output to the outlet nodes.

		// METHODOLOGY EMPLOYED:
		// NA

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
		int InNode; // inlet node number
		int OutNode; // outlet node number

		InNode = Humidifier( HumNum ).AirInNode;
		OutNode = Humidifier( HumNum ).AirOutNode;
		// Set the outlet air node of the humidifier
		Node( OutNode ).MassFlowRate = Humidifier( HumNum ).AirOutMassFlowRate;
		Node( OutNode ).Temp = Humidifier( HumNum ).AirOutTemp;
		Node( OutNode ).HumRat = Humidifier( HumNum ).AirOutHumRat;
		Node( OutNode ).Enthalpy = Humidifier( HumNum ).AirOutEnthalpy;

		// Set the outlet nodes for properties that just pass through & not used
		Node( OutNode ).Quality = Node( InNode ).Quality;
		Node( OutNode ).Press = Node( InNode ).Press;
		Node( OutNode ).MassFlowRateMin = Node( InNode ).MassFlowRateMin;
		Node( OutNode ).MassFlowRateMax = Node( InNode ).MassFlowRateMax;
		Node( OutNode ).MassFlowRateMinAvail = Node( InNode ).MassFlowRateMinAvail;
		Node( OutNode ).MassFlowRateMaxAvail = Node( InNode ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( OutNode ).CO2 = Node( InNode ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( OutNode ).GenContam = Node( InNode ).GenContam;
		}

	}

	void
	ReportHumidifier( int const HumNum ) // number of the current humidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fill remaining report variables

		// METHODOLOGY EMPLOYED:
		// na

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
		// na

		Humidifier( HumNum ).ElecUseEnergy = Humidifier( HumNum ).ElecUseRate * TimeStepSys * SecInHour;
		Humidifier( HumNum ).WaterCons = Humidifier( HumNum ).WaterConsRate * TimeStepSys * SecInHour;

	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // Humidifiers

} // EnergyPlus
