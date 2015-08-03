// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <PipeHeatTransfer.hh>
#include <BranchNodeConnections.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PipeHeatTransfer {

	// Module containing the routines dealing with pipes with transport delay
	// and heat transfer.

	// MODULE INFORMATION:
	//       AUTHOR         Simon Rees
	//       DATE WRITTEN   July 2007
	//       MODIFIED       May 2008
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate a pipe with heat transfer

	// METHODOLOGY EMPLOYED:
	// An implicit finite difference method is used to solve the temperature distribution of the
	// fluid in the pipe as a result of the transport delay and heat transfer to the environment.
	// For buried pipes, the simulation involves an implicit finite difference model of the soil,
	// which was originally based on Piechowski's thesis (below).  Equation numbers for
	// pipe:underground calculations are from Piechowski's thesis.  In Piechowski, the near-pipe
	// region is solved with a detailed finite difference grid, this current model makes use of
	// the Hanby model to simulate the actual pipe.

	// Kusuda, T. & Achenbach, P. (1965), ‘Earth temperature and thermal diffusivity at
	//     selected stations in the united states’, ASHRAE Transactions 71(1), 61-75.
	// Piechowski, M. (1996), A Ground Coupled Heat Pump System with Energy Storage,
	//     PhD thesis, University of Melbourne.

	// OTHER NOTES: Equation Numbers listed in buried pipe routines are from Piechowski's thesis

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataPlant::TypeOf_PipeExterior;
	using DataPlant::TypeOf_PipeInterior;
	using DataPlant::TypeOf_PipeUnderground;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	int const None( 0 );
	int const ZoneEnv( 1 );
	int const ScheduleEnv( 2 );
	int const OutsideAirEnv( 3 );
	int const GroundEnv( 4 );

	int const PreviousTimeIndex( 1 );
	int const CurrentTimeIndex( 2 );
	int const TentativeTimeIndex( 3 );

	Real64 const InnerDeltaTime( 60.0 ); // one minute time step in seconds

	// DERIVED TYPE DEFINITIONS

	// the model data structures

	// MODULE VARIABLE DECLARATIONS:
	int NumOfPipeHT( 0 ); // Number of Pipe Heat Transfer objects
	int InletNodeNum( 0 ); // module variable for inlet node number
	int OutletNodeNum( 0 ); // module variable for outlet node number
	int PipeHTNum( 0 ); // object index
	Real64 MassFlowRate( 0.0 ); // pipe mass flow rate
	Real64 VolumeFlowRate( 0.0 ); // pipe volumetric flow rate
	Real64 DeltaTime( 0.0 ); // time change from last update
	Real64 InletTemp( 0.0 ); // pipe inlet temperature
	Real64 OutletTemp( 0.0 ); // pipe outlet temperature
	Real64 EnvironmentTemp( 0.0 ); // environmental temperature (surrounding pipe)
	Real64 EnvHeatLossRate( 0.0 ); // heat loss rate from pipe to the environment
	Real64 FluidHeatLossRate( 0.0 ); // overall heat loss from fluid to pipe
	bool GetPipeInputFlag( true ); // First time, input is "gotten"
	int NumInnerTimeSteps( 0 ); // the number of "inner" time steps for our model

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< PipeHTData > PipeHT;
	Array1D< PipeHeatTransferReport > PipeHTReport;

	//==============================================================================

	// Functions

	void
	SimPipesHeatTransfer(
		int const EquipType,
		std::string const & EquipName, // name of the Pipe Heat Transfer.
		int & EqNum, // index in local derived types for external calling
		bool const InitLoopEquip,
		bool const FirstHVACIteration // component number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the public interface to this component.
		// Other calcs are made by calling private routines.

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
		int InnerTimeStepCtr;

		// check for input
		if ( GetPipeInputFlag ) {
			GetPipesHeatTransfer();
			GetPipeInputFlag = false;
		}

		if ( EqNum == 0 ) {
			PipeHTNum = FindItemInList( EquipName, PipeHT.Name(), NumOfPipeHT );
			if ( PipeHTNum == 0 ) {
				ShowFatalError( "SimPipeHeatTransfer: Pipe:heat transfer requested not found=" + EquipName ); // Catch any bad names before crashing
			}
			EqNum = PipeHTNum;
		} else {
			PipeHTNum = EqNum;
			if ( PipeHTNum > NumOfPipeHT || PipeHTNum < 1 ) {
				ShowFatalError( "SimPipeHeatTransfer:  Invalid component index passed=" + TrimSigDigits( PipeHTNum ) + ", Number of Units=" + TrimSigDigits( NumOfPipeHT ) + ", Entered Unit name=" + EquipName );
			}
			if ( PipeHT( PipeHTNum ).CheckEquipName ) {
				if ( EquipName != PipeHT( PipeHTNum ).Name ) {
					ShowFatalError( "SimPipeHeatTransfer: Invalid component name passed=" + TrimSigDigits( PipeHTNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + PipeHT( PipeHTNum ).Name );
				}
				PipeHT( PipeHTNum ).CheckEquipName = false;
			}
		}

		if ( InitLoopEquip ) return;
		// initialize
		InitPipesHeatTransfer( EquipType, PipeHTNum, FirstHVACIteration );
		// make the calculations
		for ( InnerTimeStepCtr = 1; InnerTimeStepCtr <= NumInnerTimeSteps; ++InnerTimeStepCtr ) {
			{ auto const SELECT_CASE_var( PipeHT( PipeHTNum ).EnvironmentPtr );
			if ( SELECT_CASE_var == GroundEnv ) {
				CalcBuriedPipeSoil( PipeHTNum );
			} else {
				CalcPipesHeatTransfer( PipeHTNum );
			}}
			PushInnerTimeStepArrays( PipeHTNum );
		}
		// update vaiables
		UpdatePipesHeatTransfer();
		// update report variables
		ReportPipesHeatTransfer( PipeHTNum );

	}

	//==============================================================================

	void
	PushInnerTimeStepArrays( int const PipeHTNum )
	{

		// Locals
		int LengthIndex;
		int DepthIndex;
		int WidthIndex;

		if ( PipeHT( PipeHTNum ).EnvironmentPtr == GroundEnv ) {
			for ( LengthIndex = 2; LengthIndex <= PipeHT( PipeHTNum ).NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeHTNum ).NumDepthNodes; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= PipeHT( PipeHTNum ).PipeNodeWidth; ++WidthIndex ) {
						//This will store the old 'current' values as the new 'previous values'  This allows
						// us to use the previous time array as history terms in the equations
						PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, PreviousTimeIndex ) = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex );
					}
				}
			}
		}

		//Then update the Hanby near pipe model temperatures
		PipeHT( PipeHTNum ).PreviousFluidTemp = PipeHT( PipeHTNum ).FluidTemp;
		PipeHT( PipeHTNum ).PreviousPipeTemp = PipeHT( PipeHTNum ).PipeTemp;

	}

	void
	GetPipesHeatTransfer()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for hydronic Pipe Heat Transfers
		// from the user input file.  This will contain all of the information
		// needed to define and simulate the surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:

		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using DataGlobals::SecInHour;
		using DataGlobals::Pi;
		using DataHeatBalance::Construct;
		using DataHeatBalance::TotConstructs;
		using DataHeatBalance::Zone;
		using DataHeatBalance::Material;
		using DataHeatBalance::TotMaterials;
		using DataHeatBalance::IntGainTypeOf_PipeIndoor;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using General::RoundSigDigits;
		using namespace DataLoopNode;
		using ScheduleManager::GetScheduleIndex;
		using OutAirNodeManager::CheckOutAirNodeNumber;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NumPipeSections( 20 );
		int const NumberOfDepthNodes( 8 ); // Number of nodes in the cartesian grid-Should be an even # for now
		Real64 const SecondsInHour( SecInHour );
		Real64 const HoursInDay( 24.0 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input,
		static bool IsNotOK( false );
		static bool IsBlank( false );

		// fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int PipeItem;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int NumOfPipeHTInt; // Number of Pipe Heat Transfer objects
		int NumOfPipeHTExt; // Number of Pipe Heat Transfer objects
		int NumOfPipeHTUG; // Number of Pipe Heat Transfer objects
		int NumSections; // total number of sections in pipe

		// Initializations and allocations
		cCurrentModuleObject = "Pipe:Indoor";
		NumOfPipeHTInt = GetNumObjectsFound( cCurrentModuleObject );
		cCurrentModuleObject = "Pipe:Outdoor";
		NumOfPipeHTExt = GetNumObjectsFound( cCurrentModuleObject );
		cCurrentModuleObject = "Pipe:Underground";
		NumOfPipeHTUG = GetNumObjectsFound( cCurrentModuleObject );

		NumOfPipeHT = NumOfPipeHTInt + NumOfPipeHTExt + NumOfPipeHTUG;
		// allocate data structures
		if ( allocated( PipeHT ) ) PipeHT.deallocate();
		if ( allocated( PipeHTReport ) ) PipeHTReport.deallocate();

		PipeHT.allocate( NumOfPipeHT );
		PipeHTReport.allocate( NumOfPipeHT );

		//  Numbers = 0.0
		//  Alphas = Blank
		Item = 0;

		cCurrentModuleObject = "Pipe:Indoor";
		for ( PipeItem = 1; PipeItem <= NumOfPipeHTInt; ++PipeItem ) {
			++Item;
			// get the object name
			GetObjectItem( cCurrentModuleObject, PipeItem, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PipeHT.Name(), Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PipeHT( Item ).Name = cAlphaArgs( 1 );
			PipeHT( Item ).TypeOf = TypeOf_PipeInterior;

			// General user input data
			PipeHT( Item ).Construction = cAlphaArgs( 2 );
			PipeHT( Item ).ConstructionNum = FindItemInList( cAlphaArgs( 2 ), Construct.Name(), TotConstructs );

			if ( PipeHT( Item ).ConstructionNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
				//    ELSE
				//      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
				//         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
			}

			//get inlet node data
			PipeHT( Item ).InletNode = cAlphaArgs( 3 );
			PipeHT( Item ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PipeHT( Item ).InletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// get outlet node data
			PipeHT( Item ).OutletNode = cAlphaArgs( 4 );
			PipeHT( Item ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PipeHT( Item ).OutletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Pipe Nodes" );

			// get environmental boundary condition type

			if ( lAlphaFieldBlanks( 5 ) ) cAlphaArgs( 5 ) = "ZONE";

			{ auto const SELECT_CASE_var( cAlphaArgs( 5 ) );

			if ( SELECT_CASE_var == "ZONE" ) {
				PipeHT( Item ).EnvironmentPtr = ZoneEnv;
				PipeHT( Item ).EnvrZone = cAlphaArgs( 6 );
				PipeHT( Item ).EnvrZonePtr = FindItemInList( cAlphaArgs( 6 ), Zone.Name(), NumOfZones );
				if ( PipeHT( Item ).EnvrZonePtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

			} else if ( SELECT_CASE_var == "SCHEDULE" ) {
				PipeHT( Item ).EnvironmentPtr = ScheduleEnv;
				PipeHT( Item ).EnvrSchedule = cAlphaArgs( 7 );
				PipeHT( Item ).EnvrSchedPtr = GetScheduleIndex( PipeHT( Item ).EnvrSchedule );
				PipeHT( Item ).EnvrVelSchedule = cAlphaArgs( 8 );
				PipeHT( Item ).EnvrVelSchedPtr = GetScheduleIndex( PipeHT( Item ).EnvrVelSchedule );
				if ( PipeHT( Item ).EnvrSchedPtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				if ( PipeHT( Item ).EnvrVelSchedPtr == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Should be \"ZONE\" or \"SCHEDULE\"" ); //TODO rename point
				ErrorsFound = true;

			}}

			// dimensions
			PipeHT( Item ).PipeID = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
				ShowSevereError( "GetPipesHeatTransfer: invalid " + cNumericFieldNames( 1 ) + " of " + RoundSigDigits( rNumericArgs( 1 ), 4 ) );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0.0" );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );

				ErrorsFound = true;
			}

			PipeHT( Item ).Length = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
				ShowSevereError( "GetPipesHeatTransfer: invalid " + cNumericFieldNames( 2 ) + " of " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0.0" );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			if ( PipeHT( Item ).ConstructionNum != 0 ) {
				ValidatePipeConstruction( cCurrentModuleObject, cAlphaArgs( 2 ), cAlphaFieldNames( 2 ), PipeHT( Item ).ConstructionNum, Item, ErrorsFound );
			}

		} // end of input loop

		cCurrentModuleObject = "Pipe:Outdoor";
		for ( PipeItem = 1; PipeItem <= NumOfPipeHTExt; ++PipeItem ) {
			++Item;
			// get the object name
			GetObjectItem( cCurrentModuleObject, PipeItem, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PipeHT.Name(), Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PipeHT( Item ).Name = cAlphaArgs( 1 );
			PipeHT( Item ).TypeOf = TypeOf_PipeExterior;

			// General user input data
			PipeHT( Item ).Construction = cAlphaArgs( 2 );
			PipeHT( Item ).ConstructionNum = FindItemInList( cAlphaArgs( 2 ), Construct.Name(), TotConstructs );

			if ( PipeHT( Item ).ConstructionNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
				//    ELSE
				//      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
				//         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
			}

			//get inlet node data
			PipeHT( Item ).InletNode = cAlphaArgs( 3 );
			PipeHT( Item ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PipeHT( Item ).InletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// get outlet node data
			PipeHT( Item ).OutletNode = cAlphaArgs( 4 );
			PipeHT( Item ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PipeHT( Item ).OutletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Pipe Nodes" );

			// get environmental boundary condition type
			//    PipeHT(Item)%Environment = 'OutdoorAir'
			PipeHT( Item ).EnvironmentPtr = OutsideAirEnv;

			PipeHT( Item ).EnvrAirNode = cAlphaArgs( 5 );
			PipeHT( Item ).EnvrAirNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
			if ( ! lAlphaFieldBlanks( 5 ) ) {
				if ( ! CheckOutAirNodeNumber( PipeHT( Item ).EnvrAirNodeNum ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node" );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "An " + cAlphaFieldNames( 5 ) + " must be used " );
				ErrorsFound = true;
			}

			// dimensions
			PipeHT( Item ).PipeID = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + " of " + RoundSigDigits( rNumericArgs( 1 ), 4 ) );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0.0" );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			PipeHT( Item ).Length = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + " of " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0.0" );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			if ( PipeHT( Item ).ConstructionNum != 0 ) {
				ValidatePipeConstruction( cCurrentModuleObject, cAlphaArgs( 2 ), cAlphaFieldNames( 2 ), PipeHT( Item ).ConstructionNum, Item, ErrorsFound );
			}

		} // end of input loop

		cCurrentModuleObject = "Pipe:Underground";
		for ( PipeItem = 1; PipeItem <= NumOfPipeHTUG; ++PipeItem ) {

			++Item;
			// get the object name
			GetObjectItem( cCurrentModuleObject, PipeItem, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PipeHT.Name(), Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PipeHT( Item ).Name = cAlphaArgs( 1 );
			PipeHT( Item ).TypeOf = TypeOf_PipeUnderground;

			// General user input data
			PipeHT( Item ).Construction = cAlphaArgs( 2 );
			PipeHT( Item ).ConstructionNum = FindItemInList( cAlphaArgs( 2 ), Construct.Name(), TotConstructs );

			if ( PipeHT( Item ).ConstructionNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
				//    ELSE
				//      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
				//         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
			}

			//get inlet node data
			PipeHT( Item ).InletNode = cAlphaArgs( 3 );
			PipeHT( Item ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PipeHT( Item ).InletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// get outlet node data
			PipeHT( Item ).OutletNode = cAlphaArgs( 4 );
			PipeHT( Item ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PipeHT( Item ).OutletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Pipe Nodes" );

			PipeHT( Item ).EnvironmentPtr = GroundEnv;

			// Solar inclusion flag
			// A6,  \field Sun Exposure
			if ( SameString( cAlphaArgs( 5 ), "SUNEXPOSED" ) ) {
				PipeHT( Item ).SolarExposed = true;
			} else if ( SameString( cAlphaArgs( 5 ), "NOSUN" ) ) {
				PipeHT( Item ).SolarExposed = false;
			} else {
				ShowSevereError( "GetPipesHeatTransfer: invalid key for sun exposure flag for " + cAlphaArgs( 1 ) );
				ShowContinueError( "Key should be either SunExposed or NoSun.  Entered Key: " + cAlphaArgs( 5 ) );
				ErrorsFound = true;
			}

			// dimensions
			PipeHT( Item ).PipeID = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + " of " + RoundSigDigits( rNumericArgs( 1 ), 4 ) );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0.0" );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			PipeHT( Item ).Length = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + " of " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0.0" );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// Also get the soil material name
			// A7,  \field Soil Material
			PipeHT( Item ).SoilMaterial = cAlphaArgs( 6 );
			PipeHT( Item ).SoilMaterialNum = FindItemInList( cAlphaArgs( 6 ), Material.Name(), TotMaterials );
			if ( PipeHT( Item ).SoilMaterialNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + PipeHT( Item ).SoilMaterial );
				ShowContinueError( "Found in " + cCurrentModuleObject + '=' + PipeHT( Item ).Name );
				ErrorsFound = true;
			} else {
				PipeHT( Item ).SoilDensity = Material( PipeHT( Item ).SoilMaterialNum ).Density;
				PipeHT( Item ).SoilDepth = Material( PipeHT( Item ).SoilMaterialNum ).Thickness;
				PipeHT( Item ).SoilCp = Material( PipeHT( Item ).SoilMaterialNum ).SpecHeat;
				PipeHT( Item ).SoilConductivity = Material( PipeHT( Item ).SoilMaterialNum ).Conductivity;
				PipeHT( Item ).SoilThermAbs = Material( PipeHT( Item ).SoilMaterialNum ).AbsorpThermal;
				PipeHT( Item ).SoilSolarAbs = Material( PipeHT( Item ).SoilMaterialNum ).AbsorpSolar;
				PipeHT( Item ).SoilRoughness = Material( PipeHT( Item ).SoilMaterialNum ).Roughness;
				PipeHT( Item ).PipeDepth = PipeHT( Item ).SoilDepth + PipeHT( Item ).PipeID / 2.0;
				PipeHT( Item ).DomainDepth = PipeHT( Item ).PipeDepth * 2.0;
				PipeHT( Item ).SoilDiffusivity = PipeHT( Item ).SoilConductivity / ( PipeHT( Item ).SoilDensity * PipeHT( Item ).SoilCp );
				PipeHT( Item ).SoilDiffusivityPerDay = PipeHT( Item ).SoilDiffusivity * SecondsInHour * HoursInDay;

				// Mesh the cartesian domain
				PipeHT( Item ).NumDepthNodes = NumberOfDepthNodes;
				PipeHT( Item ).PipeNodeDepth = PipeHT( Item ).NumDepthNodes / 2;
				PipeHT( Item ).PipeNodeWidth = PipeHT( Item ).NumDepthNodes / 2;
				PipeHT( Item ).DomainDepth = PipeHT( Item ).PipeDepth * 2.0;
				PipeHT( Item ).dSregular = PipeHT( Item ).DomainDepth / ( PipeHT( Item ).NumDepthNodes - 1 );
			}

			// Now we need to see if average annual temperature data is brought in here
			if ( NumNumbers >= 3 ) {
				PipeHT( Item ).AvgAnnualManualInput = 1;

				//If so, we need to read in the data
				// N3,  \field Average soil surface temperature
				PipeHT( Item ).AvgGroundTemp = rNumericArgs( 3 );
				//IF (PipeHT(Item)%AvgGroundTemp == 0) THEN
				//  CALL ShowSevereError('GetPipesHeatTransfer: Invalid Average Ground Temp for PIPE:UNDERGROUND=' &
				//                        //TRIM(PipeHT(Item)%Name))
				//  CALL ShowContinueError('If any one annual ground temperature item is entered, all 3 items must be entered')
				//  ErrorsFound=.TRUE.
				//ENDIF

				// N4,  \field Amplitude of soil surface temperature
				if ( NumNumbers >= 4 ) {
					PipeHT( Item ).AvgGndTempAmp = rNumericArgs( 4 );
					if ( PipeHT( Item ).AvgGndTempAmp < 0.0 ) {
						ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + RoundSigDigits( PipeHT( Item ).AvgGndTempAmp, 2 ) );
						ShowContinueError( "Found in " + cCurrentModuleObject + '=' + PipeHT( Item ).Name );
						ErrorsFound = true;
					}
				}

				// N5;  \field Phase constant of soil surface temperature
				if ( NumNumbers >= 5 ) {
					PipeHT( Item ).PhaseShiftDays = rNumericArgs( 5 );
					if ( PipeHT( Item ).PhaseShiftDays < 0 ) {
						ShowSevereError( "Invalid " + cNumericFieldNames( 5 ) + '=' + RoundSigDigits( PipeHT( Item ).PhaseShiftDays ) );
						ShowContinueError( "Found in " + cCurrentModuleObject + '=' + PipeHT( Item ).Name );
						ErrorsFound = true;
					}
				}

				if ( NumNumbers >= 3 && NumNumbers < 5 ) {
					ShowSevereError( cCurrentModuleObject + '=' + PipeHT( Item ).Name );
					ShowContinueError( "If any one annual ground temperature item is entered, all 3 items must be entered" );
					ErrorsFound = true;
				}

			}

			if ( PipeHT( Item ).ConstructionNum != 0 ) {
				ValidatePipeConstruction( cCurrentModuleObject, cAlphaArgs( 2 ), cAlphaFieldNames( 2 ), PipeHT( Item ).ConstructionNum, Item, ErrorsFound );
			}

			// Select number of pipe sections.  Hanby's optimal number of 20 section is selected.
			NumSections = NumPipeSections;
			PipeHT( Item ).NumSections = NumPipeSections;

			// For buried pipes, we need to allocate the cartesian finite difference array
			PipeHT( Item ).T.allocate( PipeHT( Item ).PipeNodeWidth, PipeHT( Item ).NumDepthNodes, PipeHT( Item ).NumSections, TentativeTimeIndex );
			PipeHT( Item ).T = 0.0;

		} // PipeUG input loop

		for ( Item = 1; Item <= NumOfPipeHT; ++Item ) {
			// Select number of pipe sections.  Hanby's optimal number of 20 section is selected.
			NumSections = NumPipeSections;
			PipeHT( Item ).NumSections = NumPipeSections;

			// We need to allocate the Hanby model arrays for all pipes, including buried
			PipeHT( Item ).TentativeFluidTemp.allocate( {0,NumSections} );
			PipeHT( Item ).TentativePipeTemp.allocate( {0,NumSections} );
			PipeHT( Item ).FluidTemp.allocate( {0,NumSections} );
			PipeHT( Item ).PreviousFluidTemp.allocate( {0,NumSections} );
			PipeHT( Item ).PipeTemp.allocate( {0,NumSections} );
			PipeHT( Item ).PreviousPipeTemp.allocate( {0,NumSections} );

			PipeHT( Item ).TentativeFluidTemp = 0.0;
			PipeHT( Item ).FluidTemp = 0.0;
			PipeHT( Item ).PreviousFluidTemp = 0.0;
			PipeHT( Item ).TentativePipeTemp = 0.0;
			PipeHT( Item ).PipeTemp = 0.0;
			PipeHT( Item ).PreviousPipeTemp = 0.0;

			// work out heat transfer areas (area per section)
			PipeHT( Item ).InsideArea = Pi * PipeHT( Item ).PipeID * PipeHT( Item ).Length / NumSections;
			PipeHT( Item ).OutsideArea = Pi * ( PipeHT( Item ).PipeOD + 2 * PipeHT( Item ).InsulationThickness ) * PipeHT( Item ).Length / NumSections;

			// cross sectional area
			PipeHT( Item ).SectionArea = Pi * 0.25 * pow_2( PipeHT( Item ).PipeID );

			// pipe & insulation mass
			PipeHT( Item ).PipeHeatCapacity = PipeHT( Item ).PipeCp * PipeHT( Item ).PipeDensity * ( Pi * 0.25 * pow_2( PipeHT( Item ).PipeOD ) - PipeHT( Item ).SectionArea ); // the metal component
		}

		// final error check
		if ( ErrorsFound ) {
			ShowFatalError( "GetPipesHeatTransfer: Errors found in input. Preceding conditions cause termination." );
		}

		// Set up the output variables CurrentModuleObject='Pipe:Indoor/Outdoor/Underground'
		for ( Item = 1; Item <= NumOfPipeHT; ++Item ) {

			SetupOutputVariable( "Pipe Fluid Heat Transfer Rate [W]", PipeHTReport( Item ).FluidHeatLossRate, "Plant", "Average", PipeHT( Item ).Name );
			SetupOutputVariable( "Pipe Fluid Heat Transfer Energy [J]", PipeHTReport( Item ).FluidHeatLossEnergy, "Plant", "Sum", PipeHT( Item ).Name );

			if ( PipeHT( Item ).EnvironmentPtr == ZoneEnv ) {
				SetupOutputVariable( "Pipe Ambient Heat Transfer Rate [W]", PipeHTReport( Item ).EnvironmentHeatLossRate, "Plant", "Average", PipeHT( Item ).Name );
				SetupOutputVariable( "Pipe Ambient Heat Transfer Energy [J]", PipeHTReport( Item ).EnvHeatLossEnergy, "Plant", "Sum", PipeHT( Item ).Name );

				SetupZoneInternalGain( PipeHT( Item ).EnvrZonePtr, "Pipe:Indoor", PipeHT( Item ).Name, IntGainTypeOf_PipeIndoor, PipeHT( Item ).ZoneHeatGainRate );

			}

			SetupOutputVariable( "Pipe Mass Flow Rate [kg/s]", PipeHTReport( Item ).MassFlowRate, "Plant", "Average", PipeHT( Item ).Name );
			SetupOutputVariable( "Pipe Volume Flow Rate [m3/s]", PipeHTReport( Item ).VolumeFlowRate, "Plant", "Average", PipeHT( Item ).Name );
			SetupOutputVariable( "Pipe Inlet Temperature [C]", PipeHTReport( Item ).FluidInletTemp, "Plant", "Average", PipeHT( Item ).Name );
			SetupOutputVariable( "Pipe Outlet Temperature [C]", PipeHTReport( Item ).FluidOutletTemp, "Plant", "Average", PipeHT( Item ).Name );
		}

	}

	void
	ValidatePipeConstruction(
		std::string const & PipeType, // module object of pipe (error messages)
		std::string const & ConstructionName, // construction name of pipe (error messages)
		std::string const & FieldName, // fieldname of pipe (error messages)
		int const ConstructionNum, // pointer into construction data
		int const PipeNum, // pointer into pipe data
		bool & ErrorsFound // set to true if errors found here
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine, called from GetInput, validates the pipe construction usage.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Construct;
		using DataHeatBalance::Material;
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
		Real64 Resistance; // overall thermal resistance [m^2.C/W]
		Real64 Density; // average density [kg/m^3]
		Real64 TotThickness; // total thickness of all layers
		Real64 SpHeat; // average specific heat [J/kg.K]
		int LayerNum;
		int TotalLayers; // total number of layers (pipe layer + insulation layers)

		Resistance = 0.0;
		TotThickness = 0.0;

		// CTF stuff
		TotalLayers = Construct( ConstructionNum ).TotLayers;
		// get pipe properties
		if ( TotalLayers == 1 ) { // no insulation layer

			PipeHT( PipeNum ).PipeConductivity = Material( Construct( ConstructionNum ).LayerPoint( 1 ) ).Conductivity;
			PipeHT( PipeNum ).PipeDensity = Material( Construct( ConstructionNum ).LayerPoint( 1 ) ).Density;
			PipeHT( PipeNum ).PipeCp = Material( Construct( ConstructionNum ).LayerPoint( 1 ) ).SpecHeat;
			PipeHT( PipeNum ).PipeOD = PipeHT( PipeNum ).PipeID + 2.0 * Material( Construct( ConstructionNum ).LayerPoint( 1 ) ).Thickness;
			PipeHT( PipeNum ).InsulationOD = PipeHT( PipeNum ).PipeOD;
			PipeHT( PipeNum ).SumTK = Material( Construct( ConstructionNum ).LayerPoint( 1 ) ).Thickness / Material( Construct( ConstructionNum ).LayerPoint( 1 ) ).Conductivity;

		} else if ( TotalLayers >= 2 ) { // first layers are insulation, last layer is pipe

			for ( LayerNum = 1; LayerNum <= TotalLayers - 1; ++LayerNum ) {
				Resistance += Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Thickness / Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Conductivity;
				Density = Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Density * Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Thickness;
				TotThickness += Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Thickness;
				SpHeat = Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).SpecHeat * Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Thickness;
				PipeHT( PipeNum ).InsulationThickness = Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Thickness;
				PipeHT( PipeNum ).SumTK += Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Thickness / Material( Construct( ConstructionNum ).LayerPoint( LayerNum ) ).Conductivity;
			}

			PipeHT( PipeNum ).InsulationResistance = Resistance;
			PipeHT( PipeNum ).InsulationConductivity = TotThickness / Resistance;
			PipeHT( PipeNum ).InsulationDensity = Density / TotThickness;
			PipeHT( PipeNum ).InsulationCp = SpHeat / TotThickness;
			PipeHT( PipeNum ).InsulationThickness = TotThickness;

			PipeHT( PipeNum ).PipeConductivity = Material( Construct( ConstructionNum ).LayerPoint( TotalLayers ) ).Conductivity;
			PipeHT( PipeNum ).PipeDensity = Material( Construct( ConstructionNum ).LayerPoint( TotalLayers ) ).Density;
			PipeHT( PipeNum ).PipeCp = Material( Construct( ConstructionNum ).LayerPoint( TotalLayers ) ).SpecHeat;

			PipeHT( PipeNum ).PipeOD = PipeHT( PipeNum ).PipeID + 2.0 * Material( Construct( ConstructionNum ).LayerPoint( TotalLayers ) ).Thickness;
			PipeHT( PipeNum ).InsulationOD = PipeHT( PipeNum ).PipeOD + 2.0 * PipeHT( PipeNum ).InsulationThickness;

		} else {
			ShowSevereError( PipeType + ": invalid " + FieldName + "=\"" + ConstructionName + "\", too many layers=[" + TrimSigDigits( TotalLayers ) + "], only 1 or 2 allowed." );
			ErrorsFound = true;
		}

	}

	//==============================================================================

	void
	InitPipesHeatTransfer(
		int const EP_UNUSED( PipeType ),
		int const PipeHTNum, // component number
		bool const FirstHVACIteration // component number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       L. Gu, 6/19/08, pipe wall heat capacity has metal layer only
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine Resets the elements of the data structure as necessary
		// at the first step, and start of each call to simulated

		// METHODOLOGY EMPLOYED:
		// Check flags and update data structure

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::BeginSimFlag;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::Pi;
		using DataGlobals::DayOfSim;
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::PubGroundTempSurface;
		using DataEnvironment::PubGroundTempSurfFlag;
		using DataLoopNode::Node;
		using DataHeatBalance::TotConstructs;
		using DataHeatBalance::TotMaterials;
		using DataHeatBalance::Construct;
		using DataHeatBalance::Material;
		using DataHeatBalFanSys::MAT; // average (mean) zone air temperature [C]
		using InputProcessor::SameString;
		using ScheduleManager::GetCurrentScheduleValue;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MonthsInYear( 12 ); // Number of months in the year
		int const AvgDaysInMonth( 30 ); // Average days in a month
		Real64 const LargeNumber( 9999.9 ); // Large number (compared to temperature values)
		static std::string const RoutineName( "InitPipesHeatTransfer" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static bool OneTimeInit( true ); // one time flag
		Real64 FirstTemperatures; // initial temperature of every node in pipe (set to inlet temp) [C]
		int PipeNum; // number of pipes
		int MonthIndex;
		int TimeIndex;
		int LengthIndex;
		int DepthIndex;
		int WidthIndex;
		Real64 CurrentDepth;
		Real64 CurTemp;
		Real64 CurSimDay;
		bool PushArrays;
		bool errFlag;

		// Assign variable
		CurSimDay = double( DayOfSim );

		// some useful module variables
		InletNodeNum = PipeHT( PipeHTNum ).InletNodeNum;
		OutletNodeNum = PipeHT( PipeHTNum ).OutletNodeNum;
		MassFlowRate = Node( InletNodeNum ).MassFlowRate;
		InletTemp = Node( InletNodeNum ).Temp;

		// get some data only once
		if ( OneTimeInit ) {

			errFlag = false;
			for ( PipeNum = 1; PipeNum <= NumOfPipeHT; ++PipeNum ) {

				ScanPlantLoopsForObject( PipeHT( PipeNum ).Name, PipeHT( PipeNum ).TypeOf, PipeHT( PipeNum ).LoopNum, PipeHT( PipeNum ).LoopSideNum, PipeHT( PipeNum ).BranchNum, PipeHT( PipeNum ).CompNum, _, _, _, _, _, errFlag );

				// 2010-03-15 ESL:
				// The following code was in place because during the first implementation stage, bizarre MaxIters were found when
				//  heat transfer pipes were placed on demand sides
				// Since then, a large number of plant and component upgrades were performed.  As such, heat transfer pipes were
				//  re-tested on several locations of the demand side
				// No problems were encountered placing the pipes on the demand side.  This restriction is removed unless there are any
				//  problems encountered.  If problems are encountered, it is expected that this restriction will still be avoided, and
				//  the proper fix implemented to allow the pipes to be placed on the demand side
				//  IF (PipeHT(PipeNum)%LoopSideNum == DemandSide) THEN
				//    CALL ShowSevereError('InitPipesHeatTransfer: Heat Transfer Pipe='//TRIM(PipeHT(PipeNum)%Name)//&
				//                      ' was encountered on the demand side of loop: '//TRIM(PlantLoop(PipeHT(PipeNum)%LoopNum)%Name)//'.')
				//    CALL ShowContinueError('Due to simulation restrictions, heat transfer pipes are only allowed on supply sides.')
				//    CALL ShowFatalError('Preceding errors cause termination')
				//  END IF

				if ( errFlag ) continue;

				//If there are any underground buried pipes, we must bring in data
				if ( PipeHT( PipeNum ).EnvironmentPtr == GroundEnv ) {

					//If ground temp data was not brought in manually in GETINPUT,
					// then we must get it from the surface ground temperatures
					if ( PipeHT( PipeNum ).AvgAnnualManualInput == 0 ) {

						if ( ! PubGroundTempSurfFlag ) {
							ShowFatalError( "No Site:GroundTemperature:Shallow object found.  This is required for a Pipe:Underground object." );
						}

						//Calculate Average Ground Temperature for all 12 months of the year:
						PipeHT( PipeNum ).AvgGroundTemp = 0.0;
						for ( MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
							PipeHT( PipeNum ).AvgGroundTemp += PubGroundTempSurface( MonthIndex );
						}
						PipeHT( PipeNum ).AvgGroundTemp /= MonthsInYear;

						//Calculate Average Amplitude from Average:
						PipeHT( PipeNum ).AvgGndTempAmp = 0.0;
						for ( MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
							PipeHT( PipeNum ).AvgGndTempAmp += std::abs( PubGroundTempSurface( MonthIndex ) - PipeHT( PipeNum ).AvgGroundTemp );
						}
						PipeHT( PipeNum ).AvgGndTempAmp /= MonthsInYear;

						//Also need to get the month of minimum surface temperature to set phase shift for Kusuda and Achenbach:
						PipeHT( PipeNum ).MonthOfMinSurfTemp = 0;
						PipeHT( PipeNum ).MinSurfTemp = LargeNumber; //Set high so that the first months temp will be lower and actually get updated
						for ( MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
							if ( PubGroundTempSurface( MonthIndex ) <= PipeHT( PipeNum ).MinSurfTemp ) {
								PipeHT( PipeNum ).MonthOfMinSurfTemp = MonthIndex;
								PipeHT( PipeNum ).MinSurfTemp = PubGroundTempSurface( MonthIndex );
							}
						}
						PipeHT( PipeNum ).PhaseShiftDays = PipeHT( PipeNum ).MonthOfMinSurfTemp * AvgDaysInMonth;
					} //End manual ground data input structure

				}

			}
			if ( errFlag ) {
				ShowFatalError( "InitPipesHeatTransfer: Program terminated due to previous condition(s)." );
			}
			// unset one-time flag
			OneTimeInit = false;

		}

		// initialize temperatures by inlet node temp
		if ( ( BeginSimFlag && PipeHT( PipeHTNum ).BeginSimInit ) || ( BeginEnvrnFlag && PipeHT( PipeHTNum ).BeginSimEnvrn ) ) {

			// For underground pipes, we need to re-init the cartesian array each environment
			for ( PipeNum = 1; PipeNum <= NumOfPipeHT; ++PipeNum ) {
				if ( PipeHT( PipeNum ).EnvironmentPtr == GroundEnv ) {
					for ( TimeIndex = PreviousTimeIndex; TimeIndex <= TentativeTimeIndex; ++TimeIndex ) {
						//Loop through all length, depth, and width of pipe to init soil temperature
						for ( LengthIndex = 1; LengthIndex <= PipeHT( PipeNum ).NumSections; ++LengthIndex ) {
							for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeNum ).NumDepthNodes; ++DepthIndex ) {
								for ( WidthIndex = 1; WidthIndex <= PipeHT( PipeNum ).PipeNodeWidth; ++WidthIndex ) {
									CurrentDepth = ( DepthIndex - 1 ) * PipeHT( PipeNum ).dSregular;
									PipeHT( PipeNum ).T( WidthIndex, DepthIndex, LengthIndex, TimeIndex ) = TBND( CurrentDepth, CurSimDay, PipeNum );
								}
							}
						}
					}
				}
			}

			// We also need to re-init the Hanby arrays for all pipes, including buried
			FirstTemperatures = 21.0; //Node(InletNodeNum)%Temp
			PipeHT( PipeHTNum ).TentativeFluidTemp = FirstTemperatures;
			PipeHT( PipeHTNum ).FluidTemp = FirstTemperatures;
			PipeHT( PipeHTNum ).PreviousFluidTemp = FirstTemperatures;
			PipeHT( PipeHTNum ).TentativePipeTemp = FirstTemperatures;
			PipeHT( PipeHTNum ).PipeTemp = FirstTemperatures;
			PipeHT( PipeHTNum ).PreviousPipeTemp = FirstTemperatures;
			PipeHT( PipeHTNum ).PreviousSimTime = 0.0;
			DeltaTime = 0.0;
			OutletTemp = 0.0;
			EnvironmentTemp = 0.0;
			EnvHeatLossRate = 0.0;
			FluidHeatLossRate = 0.0;

			PipeHT( PipeHTNum ).BeginSimInit = false;
			PipeHT( PipeHTNum ).BeginSimEnvrn = false;

		}

		if ( ! BeginSimFlag ) PipeHT( PipeHTNum ).BeginSimInit = true;
		if ( ! BeginEnvrnFlag ) PipeHT( PipeHTNum ).BeginSimEnvrn = true;

		// time step in seconds
		DeltaTime = TimeStepSys * SecInHour;
		NumInnerTimeSteps = int( DeltaTime / InnerDeltaTime );

		// previous temps are updated if necessary at start of timestep rather than end
		if ( ( FirstHVACIteration && PipeHT( PipeHTNum ).FirstHVACupdateFlag ) || ( BeginEnvrnFlag && PipeHT( PipeHTNum ).BeginEnvrnupdateFlag ) ) {

			//We need to update boundary conditions here, as well as updating the arrays
			if ( PipeHT( PipeHTNum ).EnvironmentPtr == GroundEnv ) {

				// And then update Ground Boundary Conditions
				for ( TimeIndex = 1; TimeIndex <= TentativeTimeIndex; ++TimeIndex ) {
					for ( LengthIndex = 1; LengthIndex <= PipeHT( PipeHTNum ).NumSections; ++LengthIndex ) {
						for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeHTNum ).NumDepthNodes; ++DepthIndex ) {
							//Farfield boundary
							CurrentDepth = ( DepthIndex - 1 ) * PipeHT( PipeHTNum ).dSregular;
							CurTemp = TBND( CurrentDepth, CurSimDay, PipeHTNum );
							PipeHT( PipeHTNum ).T( 1, DepthIndex, LengthIndex, TimeIndex ) = CurTemp;
						}
						for ( WidthIndex = 1; WidthIndex <= PipeHT( PipeHTNum ).PipeNodeWidth; ++WidthIndex ) {
							//Bottom side of boundary
							CurrentDepth = PipeHT( PipeHTNum ).DomainDepth;
							CurTemp = TBND( CurrentDepth, CurSimDay, PipeHTNum );
							PipeHT( PipeHTNum ).T( WidthIndex, PipeHT( PipeHTNum ).NumDepthNodes, LengthIndex, TimeIndex ) = CurTemp;
						}
					}
				}
			}

			// should next choose environment temperature according to coupled with air or ground
			{ auto const SELECT_CASE_var( PipeHT( PipeHTNum ).EnvironmentPtr );
			if ( SELECT_CASE_var == GroundEnv ) {
				//EnvironmentTemp = GroundTemp
			} else if ( SELECT_CASE_var == OutsideAirEnv ) {
				EnvironmentTemp = OutDryBulbTemp;
			} else if ( SELECT_CASE_var == ZoneEnv ) {
				EnvironmentTemp = MAT( PipeHT( PipeHTNum ).EnvrZonePtr );
			} else if ( SELECT_CASE_var == ScheduleEnv ) {
				EnvironmentTemp = GetCurrentScheduleValue( PipeHT( PipeHTNum ).EnvrSchedPtr );
			} else if ( SELECT_CASE_var == None ) { //default to outside temp
				EnvironmentTemp = OutDryBulbTemp;
			}}

			PipeHT( PipeHTNum ).BeginEnvrnupdateFlag = false;
			PipeHT( PipeHTNum ).FirstHVACupdateFlag = false;

		}

		if ( ! BeginEnvrnFlag ) PipeHT( PipeHTNum ).BeginEnvrnupdateFlag = true;
		if ( ! FirstHVACIteration ) PipeHT( PipeHTNum ).FirstHVACupdateFlag = true;

		//Calculate the current sim time for this pipe (not necessarily structure variable, but it is ok for consistency)
		PipeHT( PipeHTNum ).CurrentSimTime = ( DayOfSim - 1 ) * 24 + HourOfDay - 1 + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed;
		if ( std::abs( PipeHT( PipeHTNum ).CurrentSimTime - PipeHT( PipeHTNum ).PreviousSimTime ) > 1.0e-6 ) {
			PushArrays = true;
			PipeHT( PipeHTNum ).PreviousSimTime = PipeHT( PipeHTNum ).CurrentSimTime;
		} else {
			PushArrays = false; //Time hasn't passed, don't accept the tentative values yet!
		}

		if ( PushArrays ) {

			//If sim time has changed all values from previous runs should have been acceptable.
			// Thus we will now shift the arrays from 2>1 and 3>2 so we can then begin
			// to update 2 and 3 again.
			if ( PipeHT( PipeHTNum ).EnvironmentPtr == GroundEnv ) {
				for ( LengthIndex = 2; LengthIndex <= PipeHT( PipeHTNum ).NumSections; ++LengthIndex ) {
					for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeHTNum ).NumDepthNodes; ++DepthIndex ) {
						for ( WidthIndex = 2; WidthIndex <= PipeHT( PipeHTNum ).PipeNodeWidth; ++WidthIndex ) {
							//This will essentially 'accept' the tentative values that were calculated last iteration
							// as the new officially 'current' values
							PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex ) = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex );
						}
					}
				}
			}

			//Then update the Hanby near pipe model temperatures
			PipeHT( PipeHTNum ).FluidTemp = PipeHT( PipeHTNum ).TentativeFluidTemp;
			PipeHT( PipeHTNum ).PipeTemp = PipeHT( PipeHTNum ).TentativePipeTemp;

		} else { //  IF(.NOT. FirstHVACIteration)THEN

			//If we don't have FirstHVAC, the last iteration values were not accepted, and we should
			// not step through time.  Thus we will revert our T(3,:,:,:) array back to T(2,:,:,:) to
			// start over with the same values as last time.
			for ( LengthIndex = 2; LengthIndex <= PipeHT( PipeHTNum ).NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeHTNum ).NumDepthNodes; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= PipeHT( PipeHTNum ).PipeNodeWidth; ++WidthIndex ) {
						//This will essentially erase the past iterations and revert back to the correct values
						PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex );
					}
				}
			}

			//Similarly for Hanby model arrays
			PipeHT( PipeHTNum ).TentativeFluidTemp = PipeHT( PipeHTNum ).FluidTemp;
			PipeHT( PipeHTNum ).TentativePipeTemp = PipeHT( PipeHTNum ).PipeTemp;

		}

		//This still catches even in winter design day
		//Even though the loop eventually has no flow rate, it appears it initializes to a value, then converges to OFF
		//Thus, this is called at the beginning of every time step once.

		PipeHT( PipeHTNum ).FluidSpecHeat = GetSpecificHeatGlycol( PlantLoop( PipeHT( PipeHTNum ).LoopNum ).FluidName, InletTemp, PlantLoop( PipeHT( PipeHTNum ).LoopNum ).FluidIndex, RoutineName );
		PipeHT( PipeHTNum ).FluidDensity = GetDensityGlycol( PlantLoop( PipeHT( PipeHTNum ).LoopNum ).FluidName, InletTemp, PlantLoop( PipeHT( PipeHTNum ).LoopNum ).FluidIndex, RoutineName );

		// At this point, for all Pipe:Interior objects we should zero out the energy and rate arrays
		PipeHTReport( PipeHTNum ).FluidHeatLossRate = 0.0;
		PipeHTReport( PipeHTNum ).FluidHeatLossEnergy = 0.0;
		PipeHTReport( PipeHTNum ).EnvironmentHeatLossRate = 0.0;
		PipeHTReport( PipeHTNum ).EnvHeatLossEnergy = 0.0;
		PipeHT( PipeHTNum ).ZoneHeatGainRate = 0.0;
		FluidHeatLossRate = 0.0;
		EnvHeatLossRate = 0.0;
		OutletTemp = 0.0;

		if ( PipeHT( PipeHTNum ).FluidDensity > 0.0 ) {
			//The density will only be zero the first time through, which will be a warmup day, and not reported
			VolumeFlowRate = MassFlowRate / PipeHT( PipeHTNum ).FluidDensity;
		}

	}

	//==============================================================================

	void
	InitializeHeatTransferPipes(
		int const EP_UNUSED( PipeType ), // Type of Pipe
		std::string const & PipeName, // Name of Pipe
		int & PipeNum // Index into pipe structure for name
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provide an external call to initialize Pipes/index numbers.

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

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetPipeInputFlag ) {
			GetPipesHeatTransfer();
			GetPipeInputFlag = false;
		}

		if ( PipeNum == 0 ) {
			PipeNum = FindItemInList( PipeName, PipeHT.Name(), NumOfPipeHT );
			if ( PipeNum == 0 ) {
				ShowFatalError( "SimPipes: Pipe requested not found =" + PipeName ); // Catch any bad names before crashing
			}
		} else {
			if ( PipeNum > NumOfPipeHT || PipeNum < 1 ) {
				ShowFatalError( "InitializePipe: Invalid PipeNum passed=" + TrimSigDigits( PipeNum ) + ", Number of Pipes=" + TrimSigDigits( NumOfPipeHT ) + ", Pipe name=" + PipeName );
			}
			if ( PipeName != PipeHT( PipeNum ).Name ) {
				ShowFatalError( "InitializePipe: Invalid PipeNum passed=" + TrimSigDigits( PipeNum ) + ", Pipe name=" + PipeName + ", stored Pipe Name for that index=" + PipeHT( PipeNum ).Name );
			}
		}

	}

	//==============================================================================

	void
	CalcPipesHeatTransfer(
		int const PipeHTNum, // component number
		Optional_int_const LengthIndex
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a Pipe Heat Transfer.  Calls are made to appropriate routines
		// for heat transfer coefficients

		// METHODOLOGY EMPLOYED:
		// Differential equations for pipe and fluid nodes along the pipe are solved
		// taking backward differences in time.
		// The heat loss/gain calculations are run continuously, even when the loop is off.
		// Fluid temps will drift according to environmental conditions when there is zero flow.

		// REFERENCES:

		// Using/Aliasing
		using namespace DataEnvironment;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// fluid node heat balance (see engineering doc).
		static Real64 A1( 0.0 ); // sum of the heat balance terms
		static Real64 A2( 0.0 ); // mass flow term
		static Real64 A3( 0.0 ); // inside pipe wall convection term
		static Real64 A4( 0.0 ); // fluid node heat capacity term
		// pipe wall node heat balance (see engineering doc).
		static Real64 B1( 0.0 ); // sum of the heat balance terms
		static Real64 B2( 0.0 ); // inside pipe wall convection term
		static Real64 B3( 0.0 ); // outside pipe wall convection term
		static Real64 B4( 0.0 ); // fluid node heat capacity term

		static Real64 AirConvCoef( 0.0 ); // air-pipe convection coefficient
		static Real64 FluidConvCoef( 0.0 ); // fluid-pipe convection coefficient
		static Real64 EnvHeatTransCoef( 0.0 ); // external convection coefficient (outside pipe)
		static Real64 FluidNodeHeatCapacity( 0.0 ); // local var for MCp for single node of pipe

		static int PipeDepth( 0 );
		static int PipeWidth( 0 );
		int curnode;
		Real64 TempBelow;
		Real64 TempBeside;
		Real64 TempAbove;
		Real64 Numerator;
		Real64 Denominator;
		Real64 SurfaceTemp;

		// traps fluid properties problems such as freezing conditions
		if ( PipeHT( PipeHTNum ).FluidSpecHeat <= 0.0 || PipeHT( PipeHTNum ).FluidDensity <= 0.0 ) {
			// leave the state of the pipe as it was
			OutletTemp = PipeHT( PipeHTNum ).TentativeFluidTemp( PipeHT( PipeHTNum ).NumSections );
			// set heat transfer rates to zero for consistency
			EnvHeatLossRate = 0.0;
			FluidHeatLossRate = 0.0;
			return;
		}

		//  AirConvCoef =  OutsidePipeHeatTransCoef(PipeHTNum)
		// Revised by L. Gu by including insulation conductance 6/19/08

		if ( PipeHT( PipeHTNum ).EnvironmentPtr != GroundEnv ) {
			AirConvCoef = 1.0 / ( 1.0 / OutsidePipeHeatTransCoef( PipeHTNum ) + PipeHT( PipeHTNum ).InsulationResistance );
		}

		FluidConvCoef = CalcPipeHeatTransCoef( PipeHTNum, InletTemp, MassFlowRate, PipeHT( PipeHTNum ).PipeID );

		// heat transfer to air or ground
		{ auto const SELECT_CASE_var( PipeHT( PipeHTNum ).EnvironmentPtr );
		if ( SELECT_CASE_var == GroundEnv ) {
			//Approximate conductance using ground conductivity, (h=k/L), where L is grid spacing
			// between pipe wall and next closest node.
			EnvHeatTransCoef = PipeHT( PipeHTNum ).SoilConductivity / ( PipeHT( PipeHTNum ).dSregular - ( PipeHT( PipeHTNum ).PipeID / 2.0 ) );
		} else if ( SELECT_CASE_var == OutsideAirEnv ) {
			EnvHeatTransCoef = AirConvCoef;
		} else if ( SELECT_CASE_var == ZoneEnv ) {
			EnvHeatTransCoef = AirConvCoef;
		} else if ( SELECT_CASE_var == ScheduleEnv ) {
			EnvHeatTransCoef = AirConvCoef;
		} else if ( SELECT_CASE_var == None ) {
			EnvHeatTransCoef = 0.0;
		} else {
			EnvHeatTransCoef = 0.0;
		}}

		// work out the coefficients
		FluidNodeHeatCapacity = PipeHT( PipeHTNum ).SectionArea * PipeHT( PipeHTNum ).Length / PipeHT( PipeHTNum ).NumSections * PipeHT( PipeHTNum ).FluidSpecHeat * PipeHT( PipeHTNum ).FluidDensity; // Mass of Node x Specific heat

		// coef of fluid heat balance
		A1 = FluidNodeHeatCapacity + MassFlowRate * PipeHT( PipeHTNum ).FluidSpecHeat * DeltaTime + FluidConvCoef * PipeHT( PipeHTNum ).InsideArea * DeltaTime;

		A2 = MassFlowRate * PipeHT( PipeHTNum ).FluidSpecHeat * DeltaTime;

		A3 = FluidConvCoef * PipeHT( PipeHTNum ).InsideArea * DeltaTime;

		A4 = FluidNodeHeatCapacity;

		// coef of pipe heat balance
		B1 = PipeHT( PipeHTNum ).PipeHeatCapacity + FluidConvCoef * PipeHT( PipeHTNum ).InsideArea * DeltaTime + EnvHeatTransCoef * PipeHT( PipeHTNum ).OutsideArea * DeltaTime;

		B2 = A3;

		B3 = EnvHeatTransCoef * PipeHT( PipeHTNum ).OutsideArea * DeltaTime;

		B4 = PipeHT( PipeHTNum ).PipeHeatCapacity;

		PipeHT( PipeHTNum ).TentativeFluidTemp( 0 ) = InletTemp;

		PipeHT( PipeHTNum ).TentativePipeTemp( 0 ) = PipeHT( PipeHTNum ).PipeTemp( 1 ); // for convenience

		if ( present( LengthIndex ) ) { //Just simulate the single section if being called from Pipe:Underground

			PipeDepth = PipeHT( PipeHTNum ).PipeNodeDepth;
			PipeWidth = PipeHT( PipeHTNum ).PipeNodeWidth;
			TempBelow = PipeHT( PipeHTNum ).T( PipeWidth, PipeDepth + 1, LengthIndex, CurrentTimeIndex );
			TempBeside = PipeHT( PipeHTNum ).T( PipeWidth - 1, PipeDepth, LengthIndex, CurrentTimeIndex );
			TempAbove = PipeHT( PipeHTNum ).T( PipeWidth, PipeDepth - 1, LengthIndex, CurrentTimeIndex );
			EnvironmentTemp = ( TempBelow + TempBeside + TempAbove ) / 3.0;

			PipeHT( PipeHTNum ).TentativeFluidTemp( LengthIndex ) = ( A2 * PipeHT( PipeHTNum ).TentativeFluidTemp( LengthIndex - 1 ) + A3 / B1 * ( B3 * EnvironmentTemp + B4 * PipeHT( PipeHTNum ).PreviousPipeTemp( LengthIndex ) ) + A4 * PipeHT( PipeHTNum ).PreviousFluidTemp( LengthIndex ) ) / ( A1 - A3 * B2 / B1 );

			PipeHT( PipeHTNum ).TentativePipeTemp( LengthIndex ) = ( B2 * PipeHT( PipeHTNum ).TentativeFluidTemp( LengthIndex ) + B3 * EnvironmentTemp + B4 * PipeHT( PipeHTNum ).PreviousPipeTemp( LengthIndex ) ) / B1;

			// Get exterior surface temperature from energy balance at the surface
			Numerator = EnvironmentTemp - PipeHT( PipeHTNum ).TentativeFluidTemp( LengthIndex );
			Denominator = EnvHeatTransCoef * ( ( 1 / EnvHeatTransCoef ) + PipeHT( PipeHTNum ).SumTK );
			SurfaceTemp = EnvironmentTemp - Numerator / Denominator;

			// keep track of environmental heat loss rate - not same as fluid loss at same time
			EnvHeatLossRate += EnvHeatTransCoef * PipeHT( PipeHTNum ).OutsideArea * ( SurfaceTemp - EnvironmentTemp );

		} else { //Simulate all sections at once if not pipe:underground

			// start loop along pipe
			// b1 must not be zero but this should have been checked on input
			for ( curnode = 1; curnode <= PipeHT( PipeHTNum ).NumSections; ++curnode ) {
				PipeHT( PipeHTNum ).TentativeFluidTemp( curnode ) = ( A2 * PipeHT( PipeHTNum ).TentativeFluidTemp( curnode - 1 ) + A3 / B1 * ( B3 * EnvironmentTemp + B4 * PipeHT( PipeHTNum ).PreviousPipeTemp( curnode ) ) + A4 * PipeHT( PipeHTNum ).PreviousFluidTemp( curnode ) ) / ( A1 - A3 * B2 / B1 );

				PipeHT( PipeHTNum ).TentativePipeTemp( curnode ) = ( B2 * PipeHT( PipeHTNum ).TentativeFluidTemp( curnode ) + B3 * EnvironmentTemp + B4 * PipeHT( PipeHTNum ).PreviousPipeTemp( curnode ) ) / B1;

				// Get exterior surface temperature from energy balance at the surface
				Numerator = EnvironmentTemp - PipeHT( PipeHTNum ).TentativeFluidTemp( curnode );
				Denominator = EnvHeatTransCoef * ( ( 1 / EnvHeatTransCoef ) + PipeHT( PipeHTNum ).SumTK );
				SurfaceTemp = EnvironmentTemp - Numerator / Denominator;

				// Keep track of environmental heat loss
				EnvHeatLossRate += EnvHeatTransCoef * PipeHT( PipeHTNum ).OutsideArea * ( SurfaceTemp - EnvironmentTemp );

			}

		}

		FluidHeatLossRate = MassFlowRate * PipeHT( PipeHTNum ).FluidSpecHeat * ( PipeHT( PipeHTNum ).TentativeFluidTemp( 0 ) - PipeHT( PipeHTNum ).TentativeFluidTemp( PipeHT( PipeHTNum ).NumSections ) );

		OutletTemp = PipeHT( PipeHTNum ).TentativeFluidTemp( PipeHT( PipeHTNum ).NumSections );

	}

	//==============================================================================

	void
	CalcBuriedPipeSoil( int const PipeHTNum ) // Current Simulation Pipe Number
	{

		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   May 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// soil heat transfer with a Buried Pipe.

		// METHODOLOGY EMPLOYED:
		// An implicit pseudo 3D finite difference grid
		// is set up, which simulates transient behavior in the soil.
		// This then interfaces with the Hanby model for near-pipe region

		// REFERENCES: See Module Level Description

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::SkyTemp;
		using DataEnvironment::WindSpeed;
		using DataEnvironment::BeamSolarRad;
		using DataEnvironment::DifSolarRad;
		using DataEnvironment::SOLCOS;
		using DataGlobals::Pi;
		using DataGlobals::TimeStep;
		using DataGlobals::HourOfDay;
		using DataGlobals::KelvinConv;
		using DataGlobals::rTinyValue;
		using ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NumSections( 20 );
		Real64 const ConvCrit( 0.05 );
		int const MaxIterations( 200 );
		Real64 const StefBoltzmann( 5.6697e-08 ); // Stefan-Boltzmann constant

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int IterationIndex( 0 ); // Index when stepping through equations
		static int LengthIndex( 0 ); // Index for nodes along length of pipe
		static int DepthIndex( 0 ); // Index for nodes in the depth direction
		static int WidthIndex( 0 ); // Index for nodes in the width direction
		static Real64 ConvCoef( 0.0 ); // Current convection coefficient = f(Wind Speed,Roughness)
		static Real64 RadCoef( 0.0 ); // Current radiation coefficient
		static Real64 QSolAbsorbed( 0.0 ); // Current total solar energy absorbed
		Array3D< Real64 > T_O( PipeHT( PipeHTNum ).PipeNodeWidth, PipeHT( PipeHTNum ).NumDepthNodes, NumSections );

		//Local variable placeholders for code readability
		static Real64 A1( 0.0 ); // Placeholder for CoefA1
		static Real64 A2( 0.0 ); // Placeholder for CoefA2
		static Real64 NodeBelow( 0.0 ); // Placeholder for Node temp below current node
		static Real64 NodeAbove( 0.0 ); // Placeholder for Node temp above current node
		static Real64 NodeRight( 0.0 ); // Placeholder for Node temp to the right of current node
		static Real64 NodeLeft( 0.0 ); // Placeholder for Node temp to the left of current node
		static Real64 NodePast( 0.0 ); // Placeholder for Node temp at current node but previous time step
		static Real64 PastNodeTempAbs( 0.0 ); // Placeholder for absolute temperature (K) version of NodePast
		static Real64 Ttemp( 0.0 ); // Placeholder for a current temperature node in convergence check
		static Real64 SkyTempAbs( 0.0 ); // Placeholder for current sky temperature in Kelvin
		static int TopRoughness( 0 ); // Placeholder for soil surface roughness
		static Real64 TopThermAbs( 0.0 ); // Placeholder for soil thermal radiation absorptivity
		static Real64 TopSolarAbs( 0.0 ); // Placeholder for soil solar radiation absorptivity
		static Real64 kSoil( 0.0 ); // Placeholder for soil conductivity
		static Real64 dS( 0.0 ); // Placeholder for soil grid spacing
		static Real64 rho( 0.0 ); // Placeholder for soil density
		static Real64 Cp( 0.0 ); // Placeholder for soil specific heat

		// There are a number of coefficients which change through the simulation, and they are updated here
		PipeHT( PipeHTNum ).FourierDS = PipeHT( PipeHTNum ).SoilDiffusivity * DeltaTime / pow_2( PipeHT( PipeHTNum ).dSregular ); //Eq. D4
		PipeHT( PipeHTNum ).CoefA1 = PipeHT( PipeHTNum ).FourierDS / ( 1 + 4 * PipeHT( PipeHTNum ).FourierDS ); //Eq. D2
		PipeHT( PipeHTNum ).CoefA2 = 1 / ( 1 + 4 * PipeHT( PipeHTNum ).FourierDS ); //Eq. D3

		for ( IterationIndex = 1; IterationIndex <= MaxIterations; ++IterationIndex ) {
			if ( IterationIndex == MaxIterations ) {
				ShowWarningError( "BuriedPipeHeatTransfer: Large number of iterations detected in object: " + PipeHT( PipeHTNum ).Name );
			}

			//Store computed values in T_O array
			for ( LengthIndex = 2; LengthIndex <= PipeHT( PipeHTNum ).NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeHTNum ).NumDepthNodes - 1; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= PipeHT( PipeHTNum ).PipeNodeWidth; ++WidthIndex ) {
						T_O( WidthIndex, DepthIndex, LengthIndex ) = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex );
					}
				}
			}

			//Loop along entire length of pipe, analyzing cross sects
			for ( LengthIndex = 1; LengthIndex <= PipeHT( PipeHTNum ).NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeHTNum ).NumDepthNodes - 1; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= PipeHT( PipeHTNum ).PipeNodeWidth; ++WidthIndex ) {

						if ( DepthIndex == 1 ) { //Soil Surface Boundary

							//If on soil boundary, load up local variables and perform calculations
							NodePast = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, PreviousTimeIndex );
							PastNodeTempAbs = NodePast + KelvinConv;
							SkyTempAbs = SkyTemp + KelvinConv;
							TopRoughness = PipeHT( PipeHTNum ).SoilRoughness;
							TopThermAbs = PipeHT( PipeHTNum ).SoilThermAbs;
							TopSolarAbs = PipeHT( PipeHTNum ).SoilSolarAbs;
							kSoil = PipeHT( PipeHTNum ).SoilConductivity;
							dS = PipeHT( PipeHTNum ).dSregular;
							rho = PipeHT( PipeHTNum ).SoilDensity;
							Cp = PipeHT( PipeHTNum ).SoilCp;

							// ASHRAE simple convection coefficient model for external surfaces.
							PipeHT( PipeHTNum ).OutdoorConvCoef = CalcASHRAESimpExtConvectCoeff( TopRoughness, WindSpeed );
							ConvCoef = PipeHT( PipeHTNum ).OutdoorConvCoef;

							// thermal radiation coefficient using surf temp from past time step
							if ( std::abs( PastNodeTempAbs - SkyTempAbs ) > rTinyValue ) {
								RadCoef = StefBoltzmann * TopThermAbs * ( pow_4( PastNodeTempAbs ) - pow_4( SkyTempAbs ) ) / ( PastNodeTempAbs - SkyTempAbs );
							} else {
								RadCoef = 0.0;
							}

							// total absorbed solar - no ground solar
							QSolAbsorbed = TopSolarAbs * ( max( SOLCOS( 3 ), 0.0 ) * BeamSolarRad + DifSolarRad );

							// If sun is not exposed, then turn off both solar and thermal radiation
							if ( ! PipeHT( PipeHTNum ).SolarExposed ) {
								RadCoef = 0.0;
								QSolAbsorbed = 0.0;
							}

							if ( WidthIndex == PipeHT( PipeHTNum ).PipeNodeWidth ) { //Symmetric centerline boundary

								//-Coefficients and Temperatures
								NodeBelow = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
								NodeLeft = PipeHT( PipeHTNum ).T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );

								//-Update Equation, basically a detailed energy balance at the surface
								PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = ( QSolAbsorbed + RadCoef * SkyTemp + ConvCoef * OutDryBulbTemp + ( kSoil / dS ) * ( NodeBelow + 2 * NodeLeft ) + ( rho * Cp / DeltaTime ) * NodePast ) / ( RadCoef + ConvCoef + 3 * ( kSoil / dS ) + ( rho * Cp / DeltaTime ) );

							} else { //Soil surface, but not on centerline

								//-Coefficients and Temperatures
								NodeBelow = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
								NodeLeft = PipeHT( PipeHTNum ).T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );
								NodeRight = PipeHT( PipeHTNum ).T( WidthIndex + 1, DepthIndex, LengthIndex, CurrentTimeIndex );

								//-Update Equation
								PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = ( QSolAbsorbed + RadCoef * SkyTemp + ConvCoef * OutDryBulbTemp + ( kSoil / dS ) * ( NodeBelow + NodeLeft + NodeRight ) + ( rho * Cp / DeltaTime ) * NodePast ) / ( RadCoef + ConvCoef + 3 * ( kSoil / dS ) + ( rho * Cp / DeltaTime ) );

							} //Soil-to-air surface node structure

						} else if ( WidthIndex == PipeHT( PipeHTNum ).PipeNodeWidth ) { //On Symmetric centerline boundary

							if ( DepthIndex == PipeHT( PipeHTNum ).PipeNodeDepth ) { //On the node containing the pipe

								//-Call to simulate a single pipe segment (by passing OPTIONAL LengthIndex argument)
								CalcPipesHeatTransfer( PipeHTNum, LengthIndex );

								//-Update node for cartesian system
								PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = PipeHT( PipeHTNum ).PipeTemp( LengthIndex );

							} else if ( DepthIndex != 1 ) { //Not surface node

								//-Coefficients and Temperatures
								NodeLeft = PipeHT( PipeHTNum ).T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );
								NodeAbove = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex - 1, LengthIndex, CurrentTimeIndex );
								NodeBelow = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
								NodePast = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex - 1 );
								A1 = PipeHT( PipeHTNum ).CoefA1;
								A2 = PipeHT( PipeHTNum ).CoefA2;

								//-Update Equation
								PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = A1 * ( NodeBelow + NodeAbove + 2 * NodeLeft ) + A2 * NodePast;

							} //Symmetric centerline node structure

						} else { //All Normal Interior Nodes

							//-Coefficients and Temperatures
							A1 = PipeHT( PipeHTNum ).CoefA1;
							A2 = PipeHT( PipeHTNum ).CoefA2;
							NodeBelow = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
							NodeAbove = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex - 1, LengthIndex, CurrentTimeIndex );
							NodeRight = PipeHT( PipeHTNum ).T( WidthIndex + 1, DepthIndex, LengthIndex, CurrentTimeIndex );
							NodeLeft = PipeHT( PipeHTNum ).T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );
							NodePast = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex - 1 );

							//-Update Equation
							PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = A1 * ( NodeBelow + NodeAbove + NodeRight + NodeLeft ) + A2 * NodePast; //Eq. D1

						}
					}
				}
			}

			//Check for convergence
			for ( LengthIndex = 2; LengthIndex <= PipeHT( PipeHTNum ).NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= PipeHT( PipeHTNum ).NumDepthNodes - 1; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= PipeHT( PipeHTNum ).PipeNodeWidth; ++WidthIndex ) {
						Ttemp = PipeHT( PipeHTNum ).T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex );
						if ( std::abs( T_O( WidthIndex, DepthIndex, LengthIndex ) - Ttemp ) > ConvCrit ) goto IterationLoop_loop;
					}
				}
			}

			//If we didn't cycle back, then the system is converged
			//PipeHTReport(PipeHTNum)%PipeUGIters=IterationIndex
			goto IterationLoop_exit;

			IterationLoop_loop: ;
		}
		IterationLoop_exit: ;

		gio::close( 112 );

	}

	//==============================================================================

	void
	UpdatePipesHeatTransfer()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for
		// Pipe Heat Transfers. This routine must also set the outlet water conditions.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataPlant::PlantLoop;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN) :: PipeHTNum       ! Index for the surface

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// only outlet node temp should need updating
		Node( OutletNodeNum ).Temp = OutletTemp;

		// pass everything else through
		Node( OutletNodeNum ).TempMin = Node( InletNodeNum ).TempMin;
		Node( OutletNodeNum ).TempMax = Node( InletNodeNum ).TempMax;
		Node( OutletNodeNum ).MassFlowRate = Node( InletNodeNum ).MassFlowRate;
		Node( OutletNodeNum ).MassFlowRateMin = Node( InletNodeNum ).MassFlowRateMin;
		Node( OutletNodeNum ).MassFlowRateMax = Node( InletNodeNum ).MassFlowRateMax;
		Node( OutletNodeNum ).MassFlowRateMinAvail = Node( InletNodeNum ).MassFlowRateMinAvail;
		Node( OutletNodeNum ).MassFlowRateMaxAvail = Node( InletNodeNum ).MassFlowRateMaxAvail;
		Node( OutletNodeNum ).Quality = Node( InletNodeNum ).Quality;
		//Only pass pressure if we aren't doing a pressure simulation
		if ( PlantLoop( PipeHT( PipeHTNum ).LoopNum ).PressureSimType > 1 ) {
			//Don't do anything
		} else {
			Node( OutletNodeNum ).Press = Node( InletNodeNum ).Press;
		}
		Node( OutletNodeNum ).Enthalpy = Node( InletNodeNum ).Enthalpy;
		Node( OutletNodeNum ).HumRat = Node( InletNodeNum ).HumRat;

	}

	//==============================================================================

	void
	ReportPipesHeatTransfer( int const PipeHTNum ) // Index for the surface under consideration
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply updates the report data

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// update flows and temps from module variables
		PipeHTReport( PipeHTNum ).FluidInletTemp = InletTemp;
		PipeHTReport( PipeHTNum ).FluidOutletTemp = OutletTemp;
		PipeHTReport( PipeHTNum ).MassFlowRate = MassFlowRate;
		PipeHTReport( PipeHTNum ).VolumeFlowRate = VolumeFlowRate;

		// update other variables from module variables
		PipeHTReport( PipeHTNum ).FluidHeatLossRate = FluidHeatLossRate;
		PipeHTReport( PipeHTNum ).FluidHeatLossEnergy = FluidHeatLossRate * DeltaTime; // DeltaTime is in seconds
		PipeHTReport( PipeHTNum ).PipeInletTemp = PipeHT( PipeHTNum ).PipeTemp( 1 );
		PipeHTReport( PipeHTNum ).PipeOutletTemp = PipeHT( PipeHTNum ).PipeTemp( PipeHT( PipeHTNum ).NumSections );

		// need to average the heat rate because it is now summing over multiple inner time steps
		PipeHTReport( PipeHTNum ).EnvironmentHeatLossRate = EnvHeatLossRate / NumInnerTimeSteps;
		PipeHTReport( PipeHTNum ).EnvHeatLossEnergy = PipeHTReport( PipeHTNum ).EnvironmentHeatLossRate * DeltaTime;

		// for zone heat gains, we assign the averaged heat rate over all inner time steps
		if ( PipeHT( PipeHTNum ).EnvironmentPtr == ZoneEnv ) {
			PipeHT( PipeHTNum ).ZoneHeatGainRate = PipeHTReport( PipeHTNum ).EnvironmentHeatLossRate;
		}

	}

	//==============================================================================

	void
	CalcZonePipesHeatGain()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   September 2008
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the zone internal gains due to pipe heat transfer objects.

		// METHODOLOGY EMPLOYED:
		// Sums the heat losses from all of the water heaters in the zone to add as a gain to the zone.

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER :: PipeNum
		//  INTEGER :: ZoneNum
		static bool MyEnvrnFlag( true );
		//  REAL(r64) :: QLossToZone

		// FLOW:
		if ( NumOfPipeHT == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			PipeHT.ZoneHeatGainRate() = 0.0;
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// this routine needs to model approx zone pipe gains for use during sizing
		//  IF(DoingSizing)THEN
		//    DO PipeNum = 1, NumOfPipeHT
		//      PipeHT(pipeNum)%ZoneHeatGainRate =
		//    ENDDO
		//  ENDIF

	}

	//==============================================================================

	Real64
	CalcPipeHeatTransCoef(
		int const PipeHTNum,
		Real64 const Temperature, // Temperature of water entering the surface, in C
		Real64 const MassFlowRate, // Mass flow rate, in kg/s
		Real64 const Diameter // Pipe diameter, m
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates pipe/fluid heat transfer coefficients.
		// This routine is adapted from that in the low temp radiant surface model.

		// METHODOLOGY EMPLOYED:
		// Currently assumes water data when calculating Pr and Re

		// REFERENCES:
		// See RadiantSystemLowTemp module.
		// Property data for water shown below as parameters taken from
		// Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
		// Heat exchanger information also from Incropera and DeWitt.
		// Code based loosely on code from IBLAST program (research version)

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataPlant::PlantLoop;
		using FluidProperties::GetConductivityGlycol;
		using FluidProperties::GetViscosityGlycol;

		// Return value
		Real64 CalcPipeHeatTransCoef;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PipeHeatTransfer::CalcPipeHeatTransCoef: " );
		Real64 const MaxLaminarRe( 2300.0 ); // Maximum Reynolds number for laminar flow
		int const NumOfPropDivisions( 13 ); // intervals in property correlation
		static Array1D< Real64 > const Temps( NumOfPropDivisions, { 1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85 } ); // Temperature, in C
		static Array1D< Real64 > const Mu( NumOfPropDivisions, { 0.001652, 0.001422, 0.001225, 0.00108, 0.000959, 0.000855, 0.000769, 0.000695, 0.000631, 0.000577, 0.000528, 0.000489, 0.000453 } ); // Viscosity, in Ns/m2
		static Array1D< Real64 > const Conductivity( NumOfPropDivisions, { 0.574, 0.582, 0.590, 0.598, 0.606, 0.613, 0.620, 0.628, 0.634, 0.640, 0.645, 0.650, 0.656 } ); // Conductivity, in W/mK
		static Array1D< Real64 > const Pr( NumOfPropDivisions, { 12.22, 10.26, 8.81, 7.56, 6.62, 5.83, 5.20, 4.62, 4.16, 3.77, 3.42, 3.15, 2.88 } ); // Prandtl number (dimensionless)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int idx;
		Real64 InterpFrac;
		Real64 NuD;
		Real64 ReD;
		Real64 Kactual;
		Real64 MUactual;
		Real64 PRactual;
		int LoopNum;

		//retrieve loop index for this component so we can look up fluid properties
		LoopNum = PipeHT( PipeHTNum ).LoopNum;

		//since the fluid properties routine doesn't have Prandtl, we'll just use water values
		idx = 1;
		while ( idx <= NumOfPropDivisions ) {
			if ( Temperature < Temps( idx ) ) {
				if ( idx == 1 ) {
					PRactual = Pr( idx );
				} else if ( idx > NumOfPropDivisions ) {
					PRactual = Pr( NumOfPropDivisions ); //CR 8566
				} else {
					InterpFrac = ( Temperature - Temps( idx - 1 ) ) / ( Temps( idx ) - Temps( idx - 1 ) );
					PRactual = Pr( idx - 1 ) + InterpFrac * ( Pr( idx ) - Pr( idx - 1 ) );
				}
				break; // DO loop
			} else { //CR 8566
				PRactual = Pr( NumOfPropDivisions );
			}
			++idx;
		}

		//look up conductivity and viscosity
		Kactual = GetConductivityGlycol( PlantLoop( LoopNum ).FluidName, PipeHT( PipeHTNum ).FluidTemp( 0 ), PlantLoop( LoopNum ).FluidIndex, RoutineName ); //W/m-K
		MUactual = GetViscosityGlycol( PlantLoop( LoopNum ).FluidName, PipeHT( PipeHTNum ).FluidTemp( 0 ), PlantLoop( LoopNum ).FluidIndex, RoutineName ) / 1000.0; //Note fluid properties routine returns mPa-s, we need Pa-s

		// Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter) - as RadiantSysLowTemp
		ReD = 4.0 * MassFlowRate / ( Pi * MUactual * Diameter );

		if ( ReD == 0.0 ) { // No flow

			//For now just leave it how it was doing it before
			NuD = 3.66;
			//Although later it would be nice to have a natural convection correlation

		} else { // Calculate the Nusselt number based on what flow regime one is in

			if ( ReD >= MaxLaminarRe ) { // Turbulent flow --> use Colburn equation
				NuD = 0.023 * std::pow( ReD, 0.8 ) * std::pow( PRactual, 1.0 / 3.0 );
			} else { // Laminar flow --> use constant surface temperature relation
				NuD = 3.66;
			}

		}

		CalcPipeHeatTransCoef = Kactual * NuD / Diameter;

		return CalcPipeHeatTransCoef;

	}

	//==============================================================================

	Real64
	OutsidePipeHeatTransCoef( int const PipeHTNum ) // Index number of surface under consideration
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the convection heat transfer
		// coefficient for a cylinder in cross flow.

		// REFERENCES:
		// Fundamentals of Heat and Mass Transfer: Incropera and DeWitt, 4th ed.
		// p. 369-370 (Eq. 7:55b)

		// Using/Aliasing
		using DataHeatBalFanSys::MAT; // average (mean) zone air temperature [C]
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataEnvironment::WindSpeed;

		// Return value
		Real64 OutsidePipeHeatTransCoef;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Pr( 0.7 ); // Prandl number for air (assume constant)
		Real64 const CondAir( 0.025 ); // thermal conductivity of air (assume constant) [W/m.K]
		Real64 const RoomAirVel( 0.381 ); // room air velocity of 75 ft./min [m/s]
		Real64 const NaturalConvNusselt( 0.36 );
		//Nusselt for natural convection for horizontal cylinder
		//from: Correlations for Convective Heat Transfer
		//      Dr. Bernhard Spang
		//      Chemical Engineers’ Resource Page: http://www.cheresources.com/convection.pdf
		int const NumOfParamDivisions( 5 ); // intervals in property correlation
		int const NumOfPropDivisions( 12 ); // intervals in property correlation

		static Array1D< Real64 > const CCoef( NumOfParamDivisions, { 0.989, 0.911, 0.683, 0.193, 0.027 } ); // correlation coefficient
		static Array1D< Real64 > const mExp( NumOfParamDivisions, { 0.33, 0.385, 0.466, 0.618, 0.805 } ); // exponent
		static Array1D< Real64 > const LowerBound( NumOfParamDivisions, { 0.4, 4.0, 40.0, 4000.0, 40000.0 } ); // upper bound of correlation range
		static Array1D< Real64 > const UpperBound( NumOfParamDivisions, { 4.0, 40.0, 4000.0, 40000.0, 400000.0 } ); // lower bound of correlation range

		static Array1D< Real64 > const Temperature( NumOfPropDivisions, { -73.0, -23.0, -10.0, 0.0, 10.0, 20.0, 27.0, 30.0, 40.0, 50.0, 76.85, 126.85 } ); // temperature [C]
		static Array1D< Real64 > const DynVisc( NumOfPropDivisions, { 75.52e-7, 11.37e-6, 12.44e-6, 13.3e-6, 14.18e-6, 15.08e-6, 15.75e-6, 16e-6, 16.95e-6, 17.91e-6, 20.92e-6, 26.41e-6 } ); // dynamic viscosity [m^2/s]

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int idx;
		Real64 NuD;
		Real64 ReD;
		Real64 Coef;
		Real64 rExp;
		Real64 AirVisc;
		Real64 AirVel;
		Real64 AirTemp;
		Real64 PipeOD;
		bool ViscositySet;
		bool CoefSet;

		//Set environmental variables
		{ auto const SELECT_CASE_var( PipeHT( PipeHTNum ).TypeOf );

		if ( SELECT_CASE_var == TypeOf_PipeInterior ) {

			{ auto const SELECT_CASE_var1( PipeHT( PipeHTNum ).EnvironmentPtr );
			if ( SELECT_CASE_var1 == ScheduleEnv ) {
				AirTemp = GetCurrentScheduleValue( PipeHT( PipeHTNum ).EnvrSchedPtr );
				AirVel = GetCurrentScheduleValue( PipeHT( PipeHTNum ).EnvrVelSchedPtr );

			} else if ( SELECT_CASE_var1 == ZoneEnv ) {
				AirTemp = MAT( PipeHT( PipeHTNum ).EnvrZonePtr );
				AirVel = RoomAirVel;
			}}

		} else if ( SELECT_CASE_var == TypeOf_PipeExterior ) {

			{ auto const SELECT_CASE_var1( PipeHT( PipeHTNum ).EnvironmentPtr );
			if ( SELECT_CASE_var1 == OutsideAirEnv ) {
				AirTemp = Node( PipeHT( PipeHTNum ).EnvrAirNodeNum ).Temp;
				AirVel = WindSpeed;
			}}

		}}

		PipeOD = PipeHT( PipeHTNum ).InsulationOD;

		ViscositySet = false;
		for ( idx = 1; idx <= NumOfPropDivisions; ++idx ) {
			if ( AirTemp <= Temperature( idx ) ) {
				AirVisc = DynVisc( idx );
				ViscositySet = true;
				break;
			}
		}

		if ( ! ViscositySet ) {
			AirVisc = DynVisc( NumOfPropDivisions );
			if ( AirTemp > Temperature( NumOfPropDivisions ) ) {
				ShowWarningError( "Heat Transfer Pipe = " + PipeHT( PipeHTNum ).Name + "Viscosity out of range, air temperature too high, setting to upper limit." );
			}
		}

		// Calculate the Reynold's number
		CoefSet = false;
		if ( AirVisc > 0.0 ) {
			ReD = AirVel * PipeOD / ( AirVisc );
		}

		for ( idx = 1; idx <= NumOfParamDivisions; ++idx ) {
			if ( ReD <= UpperBound( idx ) ) {
				Coef = CCoef( idx );
				rExp = mExp( idx );
				CoefSet = true;
				break;
			}
		}

		if ( ! CoefSet ) {
			Coef = CCoef( NumOfParamDivisions );
			rExp = mExp( NumOfParamDivisions );
			if ( ReD > UpperBound( NumOfParamDivisions ) ) {
				ShowWarningError( "Heat Transfer Pipe = " + PipeHT( PipeHTNum ).Name + "Reynolds Number out of range, setting coefficients to upper limit." );
			}
		}

		// Calculate the Nusselt number
		NuD = Coef * std::pow( ReD, rExp ) * std::pow( Pr, 1.0 / 3.0 );

		// If the wind speed is too small, we need to use natural convection behavior:
		NuD = max( NuD, NaturalConvNusselt );

		// h = (k)(Nu)/D
		OutsidePipeHeatTransCoef = CondAir * NuD / PipeOD;

		return OutsidePipeHeatTransCoef;

	}

	//==============================================================================

	Real64
	TBND(
		Real64 const z, // Current Depth
		Real64 const DayOfSim, // Current Simulation Day
		int const PipeHTNum // Current Pipe Number
	)
	{

		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   December 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Returns a temperature to be used on the boundary of the buried pipe model domain

		// METHODOLOGY EMPLOYED:
		// Kusuda and Achenbach correlation is used

		// REFERENCES: See Module Level Description

		// Using/Aliasing
		using DataGlobals::Pi;

		// Return value
		Real64 TBND;

		//Kusuda and Achenbach
		TBND = PipeHT( PipeHTNum ).AvgGroundTemp - PipeHT( PipeHTNum ).AvgGndTempAmp * std::exp( -z * std::sqrt( Pi / ( 365.0 * PipeHT( PipeHTNum ).SoilDiffusivityPerDay ) ) ) * std::cos( ( 2.0 * Pi / 365.0 ) * ( DayOfSim - PipeHT( PipeHTNum ).PhaseShiftDays - ( z / 2.0 ) * std::sqrt( 365.0 / ( Pi * PipeHT( PipeHTNum ).SoilDiffusivityPerDay ) ) ) );

		return TBND;

	}

	//===============================================================================

	//===============================================================================

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // PipeHeatTransfer

} // EnergyPlus
