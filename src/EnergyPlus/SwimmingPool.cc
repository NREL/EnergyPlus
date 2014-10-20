// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <SwimmingPool.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataConversions.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaceLists.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SwimmingPool {

	// Module containing the routines dealing with swimming pools

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand, Ho-Sung Kim
	//       DATE WRITTEN   June 2012 (F90) and October 2014 (C++)
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to encapsulate the data and algorithms required
 	// to manage the SwimmingPool System Component.

	// METHODOLOGY EMPLOYED:
	// The swimming pool acts as a surface within the heat balance and then connects
	// to the plant via a water loop.
	
	// REFERENCES:
	// 1. ASHRAE (2011). 2011 ASHRAE Handbook – HVAC Applications. Atlanta: American Society of Heating,
	//    Refrigerating and Air-Conditioning Engineers, Inc., p.5.6-5.9.
	// 2. Janis, R. and W. Tao (2005). Mechanical and Electrical Systems in Buildings. 3rd ed. Upper
	//    Saddle River, NJ: Pearson Education, Inc., p.246.
	// 3. Kittler, R. (1989). Indoor Natatorium Design and Energy Recycling. ASHRAE Transactions 95(1), p.521-526.
	// 4. Smith, C., R. Jones, and G. Löf (1993). Energy Requirements and Potential Savings for Heated
	//    Indoor Swimming Pools. ASHRAE Transactions 99(2), p.864-874.
	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataSurfaces::Surface;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// System types:
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	int NumSwimmingPools( 0 ); // Number of swimming pools
	FArray1D_bool CheckEquipName;
	FArray1D_int SurfaceToPoolIndex;

	// SUBROUTINE SPECIFICATIONS FOR MODULE LowTempRadiantSystem

	// Object Data
	FArray1D< SwimmingPoolData > Pool;

	// Functions

	void
	SimSwimmingPool (
		int const SurfNum,
		Real64 & TempSurfIn,
		Real64 const RefAirTemp,
		Real64 const IterDampConst,
		Real64 const TempInsOld
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the simulation of SwimmingPool.
		// This driver manages the calls to all of
		// the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology (Get, Init, Calc, Update, Report, etc.)

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
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int PoolNum; // Pool number index

		// FLOW:
		if ( GetInputFlag ) {
			GetSwimmingPool;
			GetInputFlag = false;
		}

		// Find the correct swimming pool
		PoolNum = SurfaceToPoolIndex( SurfNum );
		if ( PoolNum > NumSwimmingPools || PoolNum < 1 ) {
			ShowFatalError( "SimSwimmingPool:  Invalid Pool Index =" + TrimSigDigits( PoolNum ) + ", Number of Units=" + TrimSigDigits( NumSwimmingPools ) + ", Surface name=" + Surface( SurfNum ).Name );
		}

		InitSwimmingPool( PoolNum );
		
		CalcSwimmingPool( PoolNum, SurfNum, TempSurfIn, RefAirTemp, IterDampConst, TempInsOld );
		
		UpdateSwimmingPool( PoolNum );

		ReportSwimmingPool( PoolNum );

	}

	void
	GetSwimmingPool( )
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for all swimming pools present in
		// the user input file.  This will contain all of the information needed
		// to simulate a swimming pool.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using BranchNodeConnections::TestCompSet;
		using DataHeatBalance::Zone;
		using DataHeatBalance::Construct;
		using General::TrimSigDigits;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using ScheduleManager::GetScheduleIndex;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_Floor;
		using DataSurfaces::TotSurfaces;
		using namespace DataLoopNode;
		using namespace DataSurfaceLists;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetSwimmingPool: " ); // include trailing blank space
		int const MinCoverFactor( 0.0 ); // minimum value for cover factors
		int const MaxCoverFactor( 1.0 ); // maximum value for cover factors
		int const MinDepth( 0.05 ); // minimum average pool depth (to avoid obvious input errors)
		int const MaxDepth( 10.0 ); // maximum average pool depth (to avoid obvious input errors)
		int const MinPowerFactor( 0.0 ); // minimum power factor for miscellaneous equipment

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if something goes wrong
		std::string CurrentModuleObject; // for ease in getting objects
		FArray1D_string Alphas; // Alpha items for object
		FArray1D_string cAlphaFields; // Alpha field names
		FArray1D_string cNumericFields; // Numeric field names
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int MaxAlphas; // Maximum number of alphas for these input keywords
		int MaxNumbers; // Maximum number of numbers for these input keywords
		FArray1D< Real64 > Numbers; // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumArgs; // Unused variable that is part of a subroutine call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		FArray1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		FArray1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// FLOW:
		// Initializations and allocations
		MaxAlphas = 0;
		MaxNumbers = 0;

		GetObjectDefMaxArgs( "SwimmingPool:Indoor", NumArgs, NumAlphas, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNumbers = max( MaxNumbers, NumNumbers );

		Alphas.allocate( MaxAlphas );
		Alphas = "";
		Numbers.allocate( MaxNumbers );
		Numbers = 0.0;
		cAlphaFields.allocate( MaxAlphas );
		cAlphaFields = "";
		cNumericFields.allocate( MaxNumbers );
		cNumericFields = "";
		lAlphaBlanks.allocate( MaxAlphas );
		lAlphaBlanks = true;
		lNumericBlanks.allocate( MaxNumbers );
		lNumericBlanks = true;

		NumSwimmingPools = GetNumObjectsFound( "SwimmingPool:Indoor" );
		CheckEquipName.allocate( NumSwimmingPools );
		CheckEquipName = true;

		Pool.allocate( NumSwimmingPools );
		SurfaceToPoolIndex.allocate( TotSurfaces );
		SurfaceToPoolIndex = 0;

		// Obtain all of the user data related to indoor swimming pools...
		CurrentModuleObject = "SwimmingPool:Indoor";
		for ( Item = 1; Item <= NumSwimmingPools; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Pool.Name(), Item, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Pool( Item ).Name = Alphas( 1 );

			Pool( Item ).SurfaceName = Alphas( 2 );
			Pool( Item ).SurfacePtr = FindItemInList( Pool( Item ).SurfaceName, SurfList.Name(), NumOfSurfaceLists );
			if ( Pool( Item ).SurfacePtr <= 0 ) {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 2 ) + " = " + Alphas( 2 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			} else if ( Surface( Pool( Item ).SurfacePtr ).PartOfVentSlabOrRadiantSurface ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Invalid Surface" );
				ShowContinueError( cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" has been used in another radiant system, ventilated slab, or pool." );
				ShowContinueError( "A single surface can only be a radiant system, a ventilated slab, or a pool.  It CANNOT be more than one of these." );
				ErrorsFound = true;
			} else { // ( Pool( Item ).SurfacePtr > 0 )
				Surface( Pool( Item ).SurfacePtr ).PartOfVentSlabOrRadiantSurface = true;
				Surface( Pool( Item ).SurfacePtr ).IsPool = true;
				SurfaceToPoolIndex( Pool( Item ).SurfacePtr ) = Item;
				// Check to make sure pool surface is a floor
				if ( Surface( Pool( Item ).SurfacePtr).Class /= SurfaceClass_Floor ) {
					ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " contains a surface name that is NOT a floor." );
					ShowContinueError( "A swimming pool must be associated with a surface that is a FLOOR.  Association with other surface types is not permitted.");
					ErrorsFound = true;
				}
			}

			Pool( Item ).AvgDepth = Numbers( 1 );
			if ( Pool( Item ).AvgDepth < MinDepth ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an average depth that is too small." );
				ShowContinueError( "The pool average depth has been reset to the minimum allowed depth." );
			} else if ( Pool( Item).AvgDepth > MaxDepth ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an average depth that is too large." );
				ShowContinueError( "The pool depth must be less than the maximum average depth of 10 meters." );
				ErrorsFound = true;
			}

			Pool( Item ).ActivityFactorSchedName = Alphas( 3 );
			Pool( Item ).ActivityFactorSchedPtr = GetScheduleIndex( Alphas( 3 ) );
			if ( ( Pool( Item ).ActivityFactorSchedPtr == 0 ) && ( ! lAlphaBlanks( 3 ) ) ) {
				ShowSevereError( cAlphaFields( 3 ) + " not found: " + Alphas( 3 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			
			Pool( Item ).MakeupWaterSupplySchedName = Alphas( 4 );
			Pool( Item ).MakeupWaterSupplySchedPtr = GetScheduleIndex( Alphas( 4 ) );
			if ( ( Pool( Item ).MakeupWaterSupplySchedPtr == 0 ) && ( ! lAlphaBlanks( 4 ) ) ) {
				ShowSevereError( cAlphaFields( 4 ) + " not found: " + Alphas( 4 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			
			Pool( Item ).CoverSchedName = Alphas( 5 );
			Pool( Item ).CoverSchedPtr = GetScheduleIndex( Alphas( 5 ) );
			if ( ( Pool( Item ).CoverSchedPtr == 0 ) && ( ! lAlphaBlanks( 5 ) ) ) {
				ShowSevereError( cAlphaFields( 5 ) + " not found: " + Alphas( 5 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			Pool( Item ).CoverEvapFactor = Numbers( 2 );
			if ( Pool( Item ).CoverEvapFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an evaporation cover factor less than zero." );
				ShowContinueError( "The evaporation cover factor has been reset to zero." );
				Pool( Item ).CoverEvapFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverEvapFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an evaporation cover factor greater than one." );
				ShowContinueError( "The evaporation cover factor has been reset to one." );
				Pool( Item ).CoverEvapFactor = MaxCoverFactor;
			}
			
			Pool( Item ).CoverConvFactor = Numbers( 3 );
			if ( Pool( Item ).CoverConvFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a convection cover factor less than zero." );
				ShowContinueError( "The convection cover factor has been reset to zero." );
				Pool( Item ).CoverConvFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverConvFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a convection cover factor greater than one." );
				ShowContinueError( "The convection cover factor has been reset to one." );
				Pool( Item ).CoverConvFactor = MaxCoverFactor;
			}

			Pool( Item ).CoverSWRadFactor = Numbers( 4 );
			if ( Pool( Item ).CoverSWRadFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a short-wavelength radiation cover factor less than zero." );
				ShowContinueError( "The short-wavelength radiation cover factor has been reset to zero." );
				Pool( Item ).CoverSWRadFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverSWRadFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a short-wavelength radiation cover factor greater than one." );
				ShowContinueError( "The short-wavelength radiation cover factor has been reset to one." );
				Pool( Item ).CoverSWRadFactor = MaxCoverFactor;
			}
			
			Pool( Item ).CoverLWRadFactor = Numbers( 5 );
			if ( Pool( Item ).CoverLWRadFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a long-wavelength radiation cover factor less than zero." );
				ShowContinueError( "The long-wavelength radiation cover factor has been reset to zero." );
				Pool( Item ).CoverLWRadFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverLWRadFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a long-wavelength radiation cover factor greater than one." );
				ShowContinueError( "The long-wavelength radiation cover factor has been reset to one." );
				Pool( Item ).CoverLWRadFactor = MaxCoverFactor;
			}
			
			Pool( Item ).WaterInletNodeName = Alphas( 6 );
			Pool( Item ).WaterOutletNodeName = Alphas( 7 );
			Pool( Item ).WaterInletNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Pool( Item ).WaterOutletNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( ( ! lAlphaBlanks( 6 ) ) || ( ! lAlphaBlanks( 7 ) ) ) {
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 6 ), Alphas( 7 ), "Hot Water Nodes" );
			}

			Pool( Item ).MiscPowerFactor = Numbers( 6 );
			if ( Pool( Item ).MiscPowerFactor < MinPowerFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a miscellaneous power factor less than zero." );
				ShowContinueError( "The miscellaneous power factor has been reset to zero." );
				Pool( Item ).MiscPowerFactor = MinPowerFactor;
			}
			
			Pool( Item ).SetPtTempSchedName = Alphas( 8 );
			Pool( Item ).SetPtTempSchedPtr = GetScheduleIndex( Alphas( 8 ) );
			if ( ( Pool( Item ).SetPtTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 8 ) ) ) {
				ShowSevereError( cAlphaFields( 8 ) + " not found: " + Alphas( 8 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			if ( lAlphaBlanks(8) ) {
				ShowSevereError( cAlphaFields( 8 ) + " left blank.  This is NOT allowed as there must be a pool water setpoint temperature." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			
			Pool( Item ).MaxNumOfPeople = Numbers( 7 );
			if ( Pool( Item ).MaxNumOfPeople < 0.0 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " was entered with negative people.  This is not allowed." );
				ShowContinueError( "The number of people has been reset to zero." );
				Pool( Item ).MaxNumOfPeople = 0.0;
			}

			Pool( Item ).PeopleSchedName = Alphas( 9 );
			Pool( Item ).PeopleSchedPtr = GetScheduleIndex( Alphas( 9 ) );
			if ( ( Pool( Item ).PeopleSchedPtr == 0 ) && ( ! lAlphaBlanks( 9 ) ) ) {
				ShowSevereError( cAlphaFields( 9 ) + " not found: " + Alphas( 9 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
		
			Pool( Item ).PeopleHeatGainSchedName = Alphas( 10 );
			Pool( Item ).PeopleHeatGainSchedPtr = GetScheduleIndex( Alphas( 10 ) );
			if ( ( Pool( Item ).PeopleHeatGainSchedPtr == 0 ) && ( ! lAlphaBlanks( 10 ) ) ) {
				ShowSevereError( cAlphaFields( 10 ) + " not found: " + Alphas( 10 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			
		}

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in swimming pool input. Preceding conditions cause termination." );
		}

		// Set up the output variables for swimming pools
		for ( Item = 1; Item <= NumSwimmingPools; ++Item ) {
			SetupOutputVariable( "Indoor Pool Makeup Water Rate [m3/s]", Pool( Item ).HeatPower, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Makeup Water Volume [m3]", Pool( Item ).HeatEnergy, "System", "Sum", Pool( Item ).Name);
			SetupOutputVariable( "Indoor Pool Water Temperature [C]", Pool( Item ).PoolWaterTemp, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Inlet Water Temperature [C]", Pool( Item ).WaterInletTemp, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Inlet Water Mass Flow Rate [kg/s]", Pool( Item ).WaterMassFlowRate, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Miscellaneous Equipment Power [W]", Pool( Item ).MiscEquipPower, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Miscellaneous Equipment Energy [J]", Pool( Item ).MiscEquipEnergy, "System", "Sum", Pool( Item ).Name);
			SetupOutputVariable( "Indoor Pool Water Heating Rate [W]", Pool( Item ).HeatPower, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Water Heating Energy [J]", Pool( Item ).HeatEnergy, "System", "Sum", Pool( Item ).Name);
		}

	}

	void
	InitSwimmingPool(
		int const PoolNum // Index for the low temperature radiant system under consideration within the derived types
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes variables relating to low temperature radiant
		// systems.

		// METHODOLOGY EMPLOYED:
		// Simply initializes whatever needs initializing.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyPlantInModel;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_SwimmingPool_Indoor;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using FluidProperties::GetDensityGlycol;
		using DataEnvironment::WaterMainsTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitSwimmingPool" );
		Real64 const MinActivityFactor = 0.0; // Minimum value for activity factor
		Real64 const MaxActivityFactor = 10.0; // Maximum value for activity factor (realistically)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // Flag for one-time initializations
		std::string Errout; // Message for errors
		static FArray1D_bool MyPlantScanFlagPool;
		bool errFlag;
		Real64 mdot;
		Real64 HeatGainPerPerson;
		Real64 PeopleModifier;

		// FLOW:

		if ( MyOneTimeFlag ) {
			MyOneTimeFlag = false;
			if ( MyPlantScanFlagPool( PoolNum ) && allocated( PlantLoop ) ) {
				errFlag = false;
				if ( Pool( PoolNum ).WaterInletNode > 0 ) {
					ScanPlantLoopsForObject( Pool( PoolNum ).Name, TypeOf_SwimmingPool_Indoor, Pool( PoolNum ).HWLoopNum, Pool( PoolNum ).HWLoopSide, Pool( PoolNum ).HWBranchNum, Pool( PoolNum ).HWCompNum, _, _, _, Pool( PoolNum ).WaterInletNode, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( RoutineName + ": Program terminated due to previous condition(s)." );
					}
				}
				MyPlantScanFlagPool( PoolNum ) = false;
			} else if ( MyPlantScanFlagPool( PoolNum ) && ! AnyPlantInModel ) {
				MyPlantScanFlagPool( PoolNum ) = false;
			}
		}

		if ( BeginEnvrnFlag ) {
			Pool( PoolNum ).PoolWaterTemp = 23.0;
			Pool( PoolNum ).HeatPower = 0.0;
			Pool( PoolNum ).HeatEnergy = 0.0;
			Pool( PoolNum ).MiscEquipPower = 0.0;
			Pool( PoolNum ).MiscEquipEnergy = 0.0;
			Pool( PoolNum ).WaterInletTemp = 0.0;
			Pool( PoolNum ).WaterOutletTemp = 0.0;
			Pool( PoolNum ).WaterMassFlowRate = 0.0;
			Pool( PoolNum ).PeopleHeatGain = 0.0;
			
			if ( ! MyPlantScanFlagPool( PoolNum ) ) {
				if ( Pool( PoolNum ).WaterInletNode > 0 ) {
					InitComponentNodes( 0.0, Pool( PoolNum ).WaterVolFlowMax, Pool( PoolNum ).WaterInletNode, Pool( PoolNum ).WaterOutletNode, Pool( PoolNum ).HWLoopNum, Pool( PoolNum ).HWLoopSide, Pool( PoolNum ).HWBranchNum, Pool( PoolNum ).HWCompNum );
				}
			}
		}
		
		// initialize the flow rate for the component on the plant side (this follows standard procedure for other components like low temperature radiant systems)
		mdot = 0.0;
		SetComponentFlowRate( mdot, Pool( PoolNum ).WaterInletNode, Pool( PoolNum ).WaterOutletNode, Pool( PoolNum ).HWLoopNum, Pool( PoolNum ).HWLoopSide, Pool( PoolNum ).HWBranchNum, Pool( PoolNum ).HWCompNum );

		// get the schedule values for different scheduled parameters
		if ( Pool( PoolNum ).ActivityFactorSchedPtr > 0 ) {
			Pool( PoolNum ).CurActivityFactor = GetCurrentScheduleValue( Pool( PoolNum ).ActivityFactorSchedPtr );
			if ( Pool( PoolNum ).CurActivityFactor < MinActivityFactor ) {
				Pool( PoolNum ).CurActivityFactor = MinActivityFactor;
				ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Activity Factor Schedule =\"" + Pool( PoolNum ).ActivityFactorSchedName + " has a negative value.  This is not allowed." );
				ShowContinueError( "The activity factor has been reset to zero." );
			}
			if ( Pool( PoolNum ).CurActivityFactor > MaxActivityFactor ) {
				Pool( PoolNum ).CurActivityFactor = 1.0;
				ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Activity Factor Schedule =\"" + Pool( PoolNum ).ActivityFactorSchedName + " has a value larger than 10.  This is not allowed." );
				ShowContinueError( "The activity factor has been reset to unity." );
			}
		} else {
			// default is activity factor of 1.0
			Pool( PoolNum ).CurActivityFactor = 1.0;
		}

		Pool( PoolNum ).CurSetPtTemp = GetCurrentScheduleValue( Pool( PoolNum ).SetPtTempSchedPtr );
		
		if ( Pool( PoolNum ).MakeupWaterSupplySchedPtr > 0 ) {
			Pool( PoolNum ).CurMakeupWaterTemp = GetCurrentScheduleValue( Pool( PoolNum ).MakeupWaterSupplySchedPtr );
		} else {
			// use water main temperaure if no schedule present in input
			Pool( PoolNum ).CurMakeupWaterTemp = WaterMainsTemp;
		}
		
		// determine the current heat gain from people
		if ( Pool( PoolNum ).PeopleHeatGainSchedPtr > 0 ) {
			HeatGainPerPerson = GetCurrentScheduleValue( Pool(PoolNum).PeopleHeatGainSchedPtr );
			if ( HeatGainPerPerson < 0.0 ) {
				ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Heat Gain Schedule =\"" + Pool( PoolNum ).PeopleHeatGainSchedName + " has a negative value.  This is not allowed." );
				ShowContinueError( "The heat gain per person has been reset to zero." );
				HeatGainPerPerson = 0.0;
			}
			if ( Pool( PoolNum ).PeopleSchedPtr > 0 ) {
				PeopleModifier = GetCurrentScheduleValue( Pool( PoolNum ).PeopleSchedPtr );
				if ( PeopleModifier < 0.0 ) {
					ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " People Schedule =\"" + Pool( PoolNum ).PeopleSchedName + " has a negative value.  This is not allowed." );
					ShowContinueError( "The number of people has been reset to zero." );
					PeopleModifier = 0.0;
				}
			} else { // no people schedule entered--assume that full number always present
				PeopleModifier = 1.0;
			}
		} else { // no heat gain schedule added--assume a zero value for Heat Gain per Person and no people present
			HeatGainPerPerson = 0.0;
			PeopleModifier = 0.0;
		}
		Pool( PoolNum ).PeopleHeatGain = PeopleModifier * HeatGainPerPerson * Pool( PoolNum ).MaxNumOfPeople;
		
		// once cover schedule value is established, define the current values of the cover heat transfer factors
		if ( Pool( PoolNum ).CoverSchedPtr > 0 ) {
			Pool( PoolNum ).CurCoverSchedVal = GetCurrentScheduleValue( Pool( PoolNum ).CoverSchedPtr );
		} else {
			// default is NO pool cover
			Pool( PoolNum ).CurCoverSchedVal = 0.0;
		}
		// for the current cover factors, a value of 1.0 means that the pool is open (not covered)
		// the user input values determine the amount the pool cover degrades one of the factors
		// for example, if the cover reduces convection by 50% and the pool is half covered, then
		// the reduction factor for convection is 25% or 75% of the normal value.  this establishes
		// the following relationships and how they are used in other parts of the code.
		// note that for the radiation factors, the reduction in absorption of radiation caused by
		// the cover will result in a net imbalance if this energy which is no longer accounted for
		// in the surface heat balance is not accounted for elsewhere.  thus, these terms will dump
		// any reduced radiation into the air heat balance as an additional convective gain to avoid
		// any loss of energy in the overall heat balance.
		Pool( PoolNum ).CurCoverEvapFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverEvapFactor );
		Pool( PoolNum ).CurCoverConvFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverConvFactor );
		Pool( PoolNum ).CurCoverSWRadFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverSWRadFactor );
		Pool( PoolNum ).CurCoverLWRadFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverLWRadFactor );
		
	}

	void
	CalcSwimmingPool(
		int const PoolNum, // number of the swimming pool
		int const SurfNum,
		Real64 & TempSurfIn,
		Real64 const RefAirTemp,
		Real64 const IterDampConst,
		Real64 const TempInsOld
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the components making up the Indoor Swimming Pool model.

		// METHODOLOGY EMPLOYED:
		// The swimming pool is modeled as a SURFACE to get access to all of the existing
		// surface related algorithms.  This subroutine mainly models the components of the
		// swimming pool so that information can be used in a standard surface heat balance.
		// The pool is assumed to be located at the inside surface face with a possible cover
		// affecting the heat balance.  The pool model takes the form of an equation solving
		// for the inside surface temperature which is assumed to be the same as the pool
		// water temperature.
		
		// REFERENCES:
		//  1. ASHRAE (2011). 2011 ASHRAE Handbook – HVAC Applications. Atlanta: American Society of Heating,
		//     Refrigerating and Air-Conditioning Engineers, Inc., p.5.6-5.9.
		//  2. Janis, R. and W. Tao (2005). Mechanical and Electrical Systems in Buildings. 3rd ed. Upper
		//     Saddle River, NJ: Pearson Education, Inc., p.246.
		//  3. Kittler, R. (1989). Indoor Natatorium Design and Energy Recycling. ASHRAE Transactions 95(1), p.521-526.
		//  4. Smith, C., R. Jones, and G. Löf (1993). Energy Requirements and Potential Savings for Heated
		//     Indoor Swimming Pools. ASHRAE Transactions 99(2), p.864-874.

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRatAvg;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataConversions::CFA;
		using DataConversions::CFMF;
		using Psychrometrics::PsyPsatFnTemp;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalance::QRadThermInAbs;
		using DataHeatBalSurface::NetLWRadToSurf;
		using DataHeatBalFanSys::QHTRadSysSurf;
		using DataHeatBalFanSys::QHWBaseboardSurf;
		using DataHeatBalFanSys::QSteamBaseboardSurf;
		using DataHeatBalFanSys::QElecBaseboardSurf;
		using DataHeatBalSurface::QRadSWInAbs;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSwimmingPool" );
		static Real64 const CFinHg( 0.00029613 ); // Multiple pressure in Pa by this constant to get inches of Hg

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 HConvIn; // convection coefficient for pool
		Real64 EvapRate; // evaporation rate for pool in kg/s
		Real64 EvapEnergyLossPerArea; // energy effect of evaporation rate per unit area in W/m2
		Real64 PSatPool; // saturation pressure at pool water temperature
		Real64 PParAir; // partial pressure of vapor of zone air
		int ZoneNum; // index to zone array
		Real64 LWtotal; // total flux from long-wavelength radiation to surface
		Real64 LWsum; // summation of all long-wavelenth radiation going to surface
		Real64 SWtotal; // total flux from short-wavelength radiation to surface
		

		// FLOW:
		// initialize local variables
		
		// Standard Heat Balance Equation:
		//		TempSurfInTmp( SurfNum ) = ( CTFConstInPart( SurfNum ) + QRadThermInAbs( SurfNum ) + QRadSWInAbs( SurfNum ) + HConvIn( SurfNum ) * RefAirTemp( SurfNum ) + NetLWRadToSurf( SurfNum ) + Construct( ConstrNum ).CTFSourceIn( 0 ) * QsrcHist( SurfNum, 1 ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) + IterDampConst * TempInsOld( SurfNum ) + Construct( ConstrNum ).CTFCross( 0 ) * TH11 ) / ( Construct( ConstrNum ).CTFInside( 0 ) + HConvIn( SurfNum ) + IterDampConst ); // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW radiation from internal sources | Convection from surface to zone air | Net radiant exchange with other zone surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from high temp radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for stability) | Current conduction from | the outside surface | Coefficient for conduction (current time) | Convection and damping term

		// Convection coefficient calculation
		HConvIn = 0.22 * std::pow( abs( Pool( PoolNum ).PoolWaterTemp - RefAirTemp ), 1.0 / 3.0 ) * Pool( PoolNum ).CurCoverConvFac;

		// Evaporation calculation:
		// Evaporation Rate (lb/h) = 0.1 * Area (ft2) * Activity Factor * (Psat,pool - Ppar,air) (in Hg)
		// So evaporation rate, area, and pressures have to be converted to standard E+ units (kg/s, m2, and Pa, respectively)
		// Evaporation Rate per Area = Evaporation Rate * Heat of Vaporization / Area of Surface
		PSatPool = PsyPsatFnTemp( Pool( PoolNum ).PoolWaterTemp, RoutineName );
		ZoneNum = Surface( SurfNum ).Zone;
		PParAir = PsyPsatFnTemp( MAT( ZoneNum ), RoutineName ) * PsyRhFnTdbWPb( MAT( ZoneNum ), ZoneAirHumRatAvg( ZoneNum ), OutBaroPress );
		if ( PSatPool < PParAir ) PSatPool = PParAir;
		EvapRate = ( 0.1 * ( Surface( SurfNum ).Area / CFA ) * Pool( PoolNum ).CurActivityFactor * ( ( PSatPool - PParAir ) * CFinHg ) ) * CFMF * Pool( PoolNum ).CurCoverEvapFac;
		EvapEnergyLossPerArea = -EvapRate *  PsyHfgAirFnWTdb( ZoneAirHumRatAvg( ZoneNum ), MAT( ZoneNum ) ) / Surface( SurfNum ).Area;

		// LW and SW radiation term modification: any "excess" radiation blocked by the cover gets convected
		// to the air directly and added to the zone air heat balance
		LWsum = ( QRadThermInAbs( SurfNum ) +  NetLWRadToSurf( SurfNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) );
		LWtotal = Pool( PoolNum ).CurCoverLWRadFac * LWsum;
		SWtotal = Pool( PoolNum ).CurCoverSWRadFac * QRadSWInAbs( SurfNum );
		Pool( PoolNum ).RadConvertToConvect = ( ( 1.0 - Pool( PoolNum ).CurCoverLWRadFac ) * LWsum ) + ( ( 1.0 - Pool( PoolNum ).CurCoverSWRadFac ) * QRadSWInAbs( SurfNum ) );

	
	}

	void
	UpdateSwimmingPool(
		int const PoolNum // number of the swimming pool
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for the
		// swimming pool model.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataHeatBalance::Zone;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataLoopNode::Node;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "UpdateSwimmingPool" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:
		
		// Don't forget to total up convective gains to various zones from pool cover!

	}

	void
	ReportSwimmingPool(
		int const PoolNum // number of the swimming pool
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for the swimming pool model.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataSurfaces::Surface;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReportSwimmingPool" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

	}

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // SwimmingPool

} // EnergyPlus
