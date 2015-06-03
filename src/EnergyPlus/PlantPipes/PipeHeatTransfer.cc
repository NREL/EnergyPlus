// C++ Headers
#include <cmath>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <PlantPipes/PipeHeatTransfer.hh>
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

	// Kusuda, T. & Achenbach, P. (1965), "Earth temperature and thermal diffusivity at
	//     selected stations in the united states", ASHRAE Transactions 71(1), 61-75.
	// Piechowski, M. (1996), A Ground Coupled Heat Pump System with Energy Storage,
	//     PhD thesis, University of Melbourne.

	// OTHER NOTES: Equation Numbers listed in buried pipe routines are from Piechowski's thesis

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

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

	Array1D< std::shared_ptr< PipeHTData > > PipeHT;

	std::shared_ptr<PlantComponent>
	PipeHTData::pipeHTFactory( int objectType, std::string objectName ) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for hydronic Pipe Heat Transfers
		// from the user input file.  This will contain all of the information
		// needed to define and simulate the surface.
		
		using namespace DataIPShortCuts; // Data for field names, blank numerics

		int const NumPipeSections( 20 );
		int const NumberOfDepthNodes( 8 ); // Number of nodes in the cartesian grid-Should be an even # for now
		Real64 const HoursInDay( 24.0 );

		bool found = false;
		bool ErrorsFound( false ); // Set to true if errors in input,
		int IOStatus; // Used in GetObjectItem
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call

		// create a new instance of a pipe heat transfer
		std::shared_ptr<PipeHTData> thisPipe( new PipeHTData() );

		if ( objectType == DataPlant::TypeOf_PipeExterior ) {
			cCurrentModuleObject = "Pipe:Outdoor";
			int const NumOfPipeHTExt = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
			for ( int PipeItem = 1; PipeItem <= NumOfPipeHTExt; ++PipeItem ) {

				InputProcessor::GetObjectItem( cCurrentModuleObject, PipeItem, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				if ( objectName == cAlphaArgs( 1 ) ) {
					found = true;
				} else {
					continue;
				}

				thisPipe->Name = cAlphaArgs( 1 );
				thisPipe->compType = DataPlant::TypeOf_PipeExterior;

				// General user input data
				thisPipe->Construction = cAlphaArgs( 2 );
				thisPipe->ConstructionNum = InputProcessor::FindItemInList( cAlphaArgs( 2 ), DataHeatBalance::Construct.Name(), DataHeatBalance::TotConstructs );

				if ( thisPipe->ConstructionNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				//get inlet node data
				thisPipe->InletNode = cAlphaArgs( 3 );
				thisPipe->InletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
				if ( thisPipe->InletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				// get outlet node data
				thisPipe->OutletNode = cAlphaArgs( 4 );
				thisPipe->OutletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );
				if ( thisPipe->OutletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				BranchNodeConnections::TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Pipe Nodes" );

				// get environmental boundary condition type
				thisPipe->EnvironmentPtr = OutsideAirEnv;

				thisPipe->EnvrAirNode = cAlphaArgs( 5 );
				thisPipe->EnvrAirNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_OutsideAirReference, 1, DataLoopNode::ObjectIsNotParent );
				if ( ! lAlphaFieldBlanks( 5 ) ) {
					if ( ! OutAirNodeManager::CheckOutAirNodeNumber( thisPipe->EnvrAirNodeNum ) ) {
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
				thisPipe->PipeID = rNumericArgs( 1 );
				if ( rNumericArgs( 1 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
					ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + " of " + General::RoundSigDigits( rNumericArgs( 1 ), 4 ) );
					ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0.0" );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				thisPipe->Length = rNumericArgs( 2 );
				if ( rNumericArgs( 2 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
					ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + " of " + General::RoundSigDigits( rNumericArgs( 2 ), 4 ) );
					ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0.0" );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				if ( thisPipe->ConstructionNum != 0 ) {
					thisPipe->validatePipeConstruction();
				}

				// exit since we found one
				break;

			} // end of input loop

		} else if ( objectType == DataPlant::TypeOf_PipeInterior ) {
			cCurrentModuleObject = "Pipe:Indoor";
			int const NumOfPipeHTInt = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
			for ( int PipeItem = 1; PipeItem <= NumOfPipeHTInt; ++PipeItem ) {

				InputProcessor::GetObjectItem( cCurrentModuleObject, PipeItem, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				if ( objectName == cAlphaArgs( 1 ) ) {
					found = true;
				} else {
					continue;
				}

				thisPipe->Name = cAlphaArgs( 1 );
				thisPipe->compType = DataPlant::TypeOf_PipeInterior;

				// General user input data
				thisPipe->Construction = cAlphaArgs( 2 );
				thisPipe->ConstructionNum = InputProcessor::FindItemInList( cAlphaArgs( 2 ), DataHeatBalance::Construct.Name(), DataHeatBalance::TotConstructs );

				if ( thisPipe->ConstructionNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				//get inlet node data
				thisPipe->InletNode = cAlphaArgs( 3 );
				thisPipe->InletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
				if ( thisPipe->InletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				// get outlet node data
				thisPipe->OutletNode = cAlphaArgs( 4 );
				thisPipe->OutletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );
				if ( thisPipe->OutletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				BranchNodeConnections::TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Pipe Nodes" );

				// get environmental boundary condition type

				if ( lAlphaFieldBlanks( 5 ) ) cAlphaArgs( 5 ) = "ZONE";

				if ( cAlphaArgs( 5 ) == "ZONE" ) {
					thisPipe->EnvironmentPtr = ZoneEnv;
					thisPipe->EnvrZone = cAlphaArgs( 6 );
					thisPipe->EnvrZonePtr = InputProcessor::FindItemInList( cAlphaArgs( 6 ), DataHeatBalance::Zone.Name(), DataGlobals::NumOfZones );
					if ( thisPipe->EnvrZonePtr == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

				} else if ( cAlphaArgs( 5 ) == "SCHEDULE" ) {
					thisPipe->EnvironmentPtr = ScheduleEnv;
					thisPipe->EnvrSchedule = cAlphaArgs( 7 );
					thisPipe->EnvrSchedPtr = ScheduleManager::GetScheduleIndex( thisPipe->EnvrSchedule );
					thisPipe->EnvrVelSchedule = cAlphaArgs( 8 );
					thisPipe->EnvrVelSchedPtr = ScheduleManager::GetScheduleIndex( thisPipe->EnvrVelSchedule );
					if ( thisPipe->EnvrSchedPtr == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
					if ( thisPipe->EnvrVelSchedPtr == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "Should be \"ZONE\" or \"SCHEDULE\"" ); //TODO rename point
					ErrorsFound = true;

				}

				// dimensions
				thisPipe->PipeID = rNumericArgs( 1 );
				if ( rNumericArgs( 1 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
					ShowSevereError( "GetPipesHeatTransfer: invalid " + cNumericFieldNames( 1 ) + " of " + General::RoundSigDigits( rNumericArgs( 1 ), 4 ) );
					ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0.0" );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				thisPipe->Length = rNumericArgs( 2 );
				if ( rNumericArgs( 2 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
					ShowSevereError( "GetPipesHeatTransfer: invalid " + cNumericFieldNames( 2 ) + " of " + General::RoundSigDigits( rNumericArgs( 2 ), 4 ) );
					ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0.0" );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				if ( thisPipe->ConstructionNum != 0 ) {
					thisPipe->validatePipeConstruction();
				}

				// exit since we found one
				break;
			}

		} else if ( objectType == DataPlant::TypeOf_PipeUnderground ) {

			cCurrentModuleObject = "Pipe:Underground";
			int const NumOfPipeHTUG = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
			for ( int PipeItem = 1; PipeItem <= NumOfPipeHTUG; ++PipeItem ) {

				InputProcessor::GetObjectItem( cCurrentModuleObject, PipeItem, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				if ( objectName == cAlphaArgs( 1 ) ) {
					found = true;
				} else {
					continue;
				}

				thisPipe->Name = cAlphaArgs( 1 );
				thisPipe->compType = DataPlant::TypeOf_PipeUnderground;

				// General user input data
				thisPipe->Construction = cAlphaArgs( 2 );
				thisPipe->ConstructionNum = InputProcessor::FindItemInList( cAlphaArgs( 2 ), DataHeatBalance::Construct.Name(), DataHeatBalance::TotConstructs );

				if ( thisPipe->ConstructionNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				//get inlet node data
				thisPipe->InletNode = cAlphaArgs( 3 );
				thisPipe->InletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
				if ( thisPipe->InletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				// get outlet node data
				thisPipe->OutletNode = cAlphaArgs( 4 );
				thisPipe->OutletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );
				if ( thisPipe->OutletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				BranchNodeConnections::TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Pipe Nodes" );

				thisPipe->EnvironmentPtr = GroundEnv;

				// Solar inclusion flag
				// A6,  \field Sun Exposure
				if ( InputProcessor::SameString( cAlphaArgs( 5 ), "SUNEXPOSED" ) ) {
					thisPipe->SolarExposed = true;
				} else if ( InputProcessor::SameString( cAlphaArgs( 5 ), "NOSUN" ) ) {
					thisPipe->SolarExposed = false;
				} else {
					ShowSevereError( "GetPipesHeatTransfer: invalid key for sun exposure flag for " + cAlphaArgs( 1 ) );
					ShowContinueError( "Key should be either SunExposed or NoSun.  Entered Key: " + cAlphaArgs( 5 ) );
					ErrorsFound = true;
				}

				// dimensions
				thisPipe->PipeID = rNumericArgs( 1 );
				if ( rNumericArgs( 1 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
					ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + " of " + General::RoundSigDigits( rNumericArgs( 1 ), 4 ) );
					ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0.0" );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				thisPipe->Length = rNumericArgs( 2 );
				if ( rNumericArgs( 2 ) <= 0.0 ) { // not really necessary because idd field has "minimum> 0"
					ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + " of " + General::RoundSigDigits( rNumericArgs( 2 ), 4 ) );
					ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0.0" );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				// Also get the soil material name
				// A7,  \field Soil Material
				thisPipe->SoilMaterial = cAlphaArgs( 6 );
				thisPipe->SoilMaterialNum = InputProcessor::FindItemInList( cAlphaArgs( 6 ), DataHeatBalance::Material.Name(), DataHeatBalance::TotMaterials );
				if ( thisPipe->SoilMaterialNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + thisPipe->SoilMaterial );
					ShowContinueError( "Found in " + cCurrentModuleObject + '=' + thisPipe->Name );
					ErrorsFound = true;
				} else {
					thisPipe->SoilDensity = DataHeatBalance::Material( thisPipe->SoilMaterialNum ).Density;
					thisPipe->SoilDepth = DataHeatBalance::Material( thisPipe->SoilMaterialNum ).Thickness;
					thisPipe->SoilCp = DataHeatBalance::Material( thisPipe->SoilMaterialNum ).SpecHeat;
					thisPipe->SoilConductivity = DataHeatBalance::Material( thisPipe->SoilMaterialNum ).Conductivity;
					thisPipe->SoilThermAbs = DataHeatBalance::Material( thisPipe->SoilMaterialNum ).AbsorpThermal;
					thisPipe->SoilSolarAbs = DataHeatBalance::Material( thisPipe->SoilMaterialNum ).AbsorpSolar;
					thisPipe->SoilRoughness = DataHeatBalance::Material( thisPipe->SoilMaterialNum ).Roughness;
					thisPipe->PipeDepth = thisPipe->SoilDepth + thisPipe->PipeID / 2.0;
					thisPipe->DomainDepth = thisPipe->PipeDepth * 2.0;
					thisPipe->SoilDiffusivity = thisPipe->SoilConductivity / ( thisPipe->SoilDensity * thisPipe->SoilCp );
					thisPipe->SoilDiffusivityPerDay = thisPipe->SoilDiffusivity * DataGlobals::SecInHour * HoursInDay;

					// Mesh the cartesian domain
					thisPipe->NumDepthNodes = NumberOfDepthNodes;
					thisPipe->PipeNodeDepth = thisPipe->NumDepthNodes / 2;
					thisPipe->PipeNodeWidth = thisPipe->NumDepthNodes / 2;
					thisPipe->DomainDepth = thisPipe->PipeDepth * 2.0;
					thisPipe->dSregular = thisPipe->DomainDepth / ( thisPipe->NumDepthNodes - 1 );
				}

				// Now we need to see if average annual temperature data is brought in here
				if ( NumNumbers >= 3 ) {
					thisPipe->AvgAnnualManualInput = 1;

					//If so, we need to read in the data
					// N3,  \field Average soil surface temperature
					thisPipe->AvgGroundTemp = rNumericArgs( 3 );

					// N4,  \field Amplitude of soil surface temperature
					if ( NumNumbers >= 4 ) {
						thisPipe->AvgGndTempAmp = rNumericArgs( 4 );
						if ( thisPipe->AvgGndTempAmp < 0.0 ) {
							ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + General::RoundSigDigits( thisPipe->AvgGndTempAmp, 2 ) );
							ShowContinueError( "Found in " + cCurrentModuleObject + '=' + thisPipe->Name );
							ErrorsFound = true;
						}
					}

					// N5;  \field Phase constant of soil surface temperature
					if ( NumNumbers >= 5 ) {
						thisPipe->PhaseShiftDays = rNumericArgs( 5 );
						if ( thisPipe->PhaseShiftDays < 0 ) {
							ShowSevereError( "Invalid " + cNumericFieldNames( 5 ) + '=' + General::RoundSigDigits( thisPipe->PhaseShiftDays ) );
							ShowContinueError( "Found in " + cCurrentModuleObject + '=' + thisPipe->Name );
							ErrorsFound = true;
						}
					}

					if ( NumNumbers >= 3 && NumNumbers < 5 ) {
						ShowSevereError( cCurrentModuleObject + '=' + thisPipe->Name );
						ShowContinueError( "If any one annual ground temperature item is entered, all 3 items must be entered" );
						ErrorsFound = true;
					}

				}

				if ( thisPipe->ConstructionNum != 0 ) {
					thisPipe->validatePipeConstruction();
				}

				// Select number of pipe sections.  Hanby's optimal number of 20 section is selected.
				thisPipe->NumSections = NumPipeSections;

				// For buried pipes, we need to allocate the cartesian finite difference array
				thisPipe->T.allocate( thisPipe->PipeNodeWidth, thisPipe->NumDepthNodes, thisPipe->NumSections, TentativeTimeIndex );
				thisPipe->T = 0.0;

				// exit since we found one
				break;

			} // PipeUG input loop
		}
		thisPipe->NumSections = NumPipeSections;

		// We need to allocate the Hanby model arrays for all pipes, including buried
		thisPipe->TentativeFluidTemp.allocate( {0,thisPipe->NumSections} );
		thisPipe->TentativePipeTemp.allocate( {0,thisPipe->NumSections} );
		thisPipe->FluidTemp.allocate( {0,thisPipe->NumSections} );
		thisPipe->PreviousFluidTemp.allocate( {0,thisPipe->NumSections} );
		thisPipe->PipeTemp.allocate( {0,thisPipe->NumSections} );
		thisPipe->PreviousPipeTemp.allocate( {0,thisPipe->NumSections} );
		thisPipe->TentativeFluidTemp = 0.0;
		thisPipe->TentativePipeTemp = 0.0;
		thisPipe->FluidTemp = 0.0;
		thisPipe->PreviousFluidTemp = 0.0;
		thisPipe->PipeTemp = 0.0;
		thisPipe->PreviousPipeTemp = 0.0;

		// work out heat transfer areas (area per section)
		thisPipe->InsideArea = DataGlobals::Pi * thisPipe->PipeID * thisPipe->Length / thisPipe->NumSections;
		thisPipe->OutsideArea = DataGlobals::Pi * ( thisPipe->PipeOD + 2 * thisPipe->InsulationThickness ) * thisPipe->Length / thisPipe->NumSections;

		// cross sectional area
		thisPipe->SectionArea = DataGlobals::Pi * 0.25 * pow_2( thisPipe->PipeID );

		// pipe & insulation mass
		thisPipe->PipeHeatCapacity = thisPipe->PipeCp * thisPipe->PipeDensity * ( DataGlobals::Pi * 0.25 * pow_2( thisPipe->PipeOD ) - thisPipe->SectionArea ); // the metal component

		// Set up the output variables CurrentModuleObject='Pipe:Indoor/Outdoor/Underground'
		SetupOutputVariable( "Pipe Fluid Heat Transfer Rate [W]", thisPipe->FluidHeatLossRate, "Plant", "Average", thisPipe->Name );
		SetupOutputVariable( "Pipe Fluid Heat Transfer Energy [J]", thisPipe->FluidHeatLossEnergy, "Plant", "Sum", thisPipe->Name );

		if ( thisPipe->EnvironmentPtr == ZoneEnv ) {
			SetupOutputVariable( "Pipe Ambient Heat Transfer Rate [W]", thisPipe->EnvironmentHeatLossRate, "Plant", "Average", thisPipe->Name );
			SetupOutputVariable( "Pipe Ambient Heat Transfer Energy [J]", thisPipe->EnvHeatLossEnergy, "Plant", "Sum", thisPipe->Name );
			SetupZoneInternalGain( thisPipe->EnvrZonePtr, "Pipe:Indoor", thisPipe->Name, DataHeatBalance::IntGainTypeOf_PipeIndoor, thisPipe->ZoneHeatGainRate );
		}

		SetupOutputVariable( "Pipe Mass Flow Rate [kg/s]", thisPipe->MassFlowRate, "Plant", "Average", thisPipe->Name );
		SetupOutputVariable( "Pipe Volume Flow Rate [m3/s]", thisPipe->VolumeFlowRate, "Plant", "Average", thisPipe->Name );
		SetupOutputVariable( "Pipe Inlet Temperature [C]", thisPipe->FluidInletTemp, "Plant", "Average", thisPipe->Name );
		SetupOutputVariable( "Pipe Outlet Temperature [C]", thisPipe->FluidOutletTemp, "Plant", "Average", thisPipe->Name );

		if ( found && !ErrorsFound ) {
			PipeHT.push_back( thisPipe );
			return thisPipe;
		} else {
			ShowFatalError( "GetPipesHeatTransfer: Errors found in input. Preceding conditions cause termination." );
			// add a dummy return here to hush up the compiler
			return nullptr;
		}
	}

	int
	PipeHTData::performEveryTimeInit() {

		// for reporting
		std::string const RoutineName = "PipeHTData::performEveryTimeInit";

		// some useful struct variables
		this->MassFlowRate = DataLoopNode::Node( this->InletNodeNum ).MassFlowRate;
		this->FluidInletTemp = DataLoopNode::Node( this->InletNodeNum ).Temp;
		this->DeltaTime = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->NumInnerTimeSteps = int( this->DeltaTime / InnerDeltaTime );

		//Calculate the current sim time for this pipe (not necessarily structure variable, but it is ok for consistency)
		this->CurrentSimTime = ( DataGlobals::DayOfSim - 1 ) * 24 + DataGlobals::HourOfDay - 1 + ( DataGlobals::TimeStep - 1 ) * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
		
		// initialize the push variable to false, then override if time has moved
		bool PushArrays = false; //Time hasn't passed, don't accept the tentative values yet!
		if ( std::abs( this->CurrentSimTime - this->PreviousSimTime ) > 1.0e-6 ) {
			PushArrays = true;
			this->PreviousSimTime = this->CurrentSimTime;
		}

		if ( PushArrays ) {

			//If sim time has changed all values from previous runs should have been acceptable.
			// Thus we will now shift the arrays from 2>1 and 3>2 so we can then begin
			// to update 2 and 3 again.
			if ( this->EnvironmentPtr == GroundEnv ) {
				for ( int LengthIndex = 2; LengthIndex <= this->NumSections; ++LengthIndex ) {
					for ( int DepthIndex = 1; DepthIndex <= this->NumDepthNodes; ++DepthIndex ) {
						for ( int WidthIndex = 2; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {
							//This will essentially 'accept' the tentative values that were calculated last iteration
							// as the new officially 'current' values
							this->T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex ) = this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex );
						}
					}
				}
			}

			//Then update the Hanby near pipe model temperatures
			this->FluidTemp = this->TentativeFluidTemp;
			this->PipeTemp = this->TentativePipeTemp;

		} else {

			//If we don't move forward, the last iteration values were not accepted, and we should
			// not step through time.  Thus we will revert our T(3,:,:,:) array back to T(2,:,:,:) to
			// start over with the same values as last time.
			for ( int LengthIndex = 2; LengthIndex <= this->NumSections; ++LengthIndex ) {
				for ( int DepthIndex = 1; DepthIndex <= this->NumDepthNodes; ++DepthIndex ) {
					for ( int WidthIndex = 2; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {
						//This will essentially erase the past iterations and revert back to the correct values
						this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = this->T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex );
					}
				}
			}

			//Similarly for Hanby model arrays
			this->TentativeFluidTemp = this->FluidTemp;
			this->TentativePipeTemp = this->PipeTemp;

		}

		auto & thisPlantLoop = DataPlant::PlantLoop( this->LoopNum );
		this->FluidSpecHeat = FluidProperties::GetSpecificHeatGlycol( thisPlantLoop.FluidName, this->FluidInletTemp, thisPlantLoop.FluidIndex, RoutineName );
		this->FluidDensity = FluidProperties::GetDensityGlycol( thisPlantLoop.FluidName, this->FluidInletTemp, thisPlantLoop.FluidIndex, RoutineName );

		// At this point, for all Pipe:Interior objects we should zero out the energy and rate arrays
		this->FluidHeatLossRate = 0.0;
		this->FluidHeatLossEnergy = 0.0;
		this->EnvironmentHeatLossRate = 0.0;
		this->EnvHeatLossEnergy = 0.0;
		this->ZoneHeatGainRate = 0.0;
		this->FluidHeatLossRate = 0.0;
		this->EnvHeatLossRate = 0.0;
		
		if ( this->FluidDensity > 0.0 ) {
			//The density will only be zero the first time through, which will be a warmup day, and not reported
			this->VolumeFlowRate = this->MassFlowRate / this->FluidDensity;
		}

		// return value can be used to trigger success/failure
		return 0;
	}

	int
	PipeHTData::performOneTimeInit() {

		int const MonthsInYear( 12 ); // Number of months in the year
		int const AvgDaysInMonth( 30 ); // Average days in a month
		Real64 const LargeNumber( 9999.9 ); // Large number (compared to temperature values)

		bool errFlag = false;
		DataPlant::ScanPlantLoopsForObject( this->Name, this->compType, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, _, _, _, _, _, errFlag );

		//If there are any underground buried pipes, we must bring in data
		if ( this->EnvironmentPtr == GroundEnv ) {

			//If ground temp data was not brought in manually in GETINPUT,
			// then we must get it from the surface ground temperatures
			if ( this->AvgAnnualManualInput == 0 ) {

				if ( ! DataEnvironment::PubGroundTempSurfFlag ) {
					ShowFatalError( "No Site:GroundTemperature:Shallow object found.  This is required for a Pipe:Underground object." );
				}

				//Calculate Average Ground Temperature for all 12 months of the year:
				this->AvgGroundTemp = 0.0;
				for ( int MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
					this->AvgGroundTemp += DataEnvironment::PubGroundTempSurface( MonthIndex );
				}
				this->AvgGroundTemp /= MonthsInYear;

				//Calculate Average Amplitude from Average:
				this->AvgGndTempAmp = 0.0;
				for ( int MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
					this->AvgGndTempAmp += std::abs( DataEnvironment::PubGroundTempSurface( MonthIndex ) - this->AvgGroundTemp );
				}
				this->AvgGndTempAmp /= MonthsInYear;

				//Also need to get the month of minimum surface temperature to set phase shift for Kusuda and Achenbach:
				this->MonthOfMinSurfTemp = 0;
				this->MinSurfTemp = LargeNumber; //Set high so that the first months temp will be lower and actually get updated
				for ( int MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
					if ( DataEnvironment::PubGroundTempSurface( MonthIndex ) <= this->MinSurfTemp ) {
						this->MonthOfMinSurfTemp = MonthIndex;
						this->MinSurfTemp = DataEnvironment::PubGroundTempSurface( MonthIndex );
					}
				}
				this->PhaseShiftDays = this->MonthOfMinSurfTemp * AvgDaysInMonth;
				
			} //End manual ground data input structure
		}
		if ( errFlag ) {
			ShowFatalError( "InitPipesHeatTransfer: Program terminated due to previous condition(s)." );
			return 1;
		} else {
			return 0;
		}
	}

	int
	PipeHTData::performBeginEnvrnInit() {

		// For underground pipes, we need to re-init the cartesian array each environment
		Real64 CurSimDay = double( DataGlobals::DayOfSim );
		if ( this->EnvironmentPtr == GroundEnv ) {
			for ( int TimeIndex = PreviousTimeIndex; TimeIndex <= TentativeTimeIndex; ++TimeIndex ) {
				//Loop through all length, depth, and width of pipe to init soil temperature
				for ( int LengthIndex = 1; LengthIndex <= this->NumSections; ++LengthIndex ) {
					for ( int DepthIndex = 1; DepthIndex <= this->NumDepthNodes; ++DepthIndex ) {
						for ( int WidthIndex = 1; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {
							Real64 CurrentDepth = ( DepthIndex - 1 ) * this->dSregular;
							this->T( WidthIndex, DepthIndex, LengthIndex, TimeIndex ) = this->farfieldTemperature( CurrentDepth, CurSimDay );
						}
					}
				}
			}
		}

		// We also need to re-init the Hanby arrays for all pipes, including buried
		Real64 const FirstTemperatures = 21.0;
		this->TentativeFluidTemp = FirstTemperatures;
		this->FluidTemp = FirstTemperatures;
		this->PreviousFluidTemp = FirstTemperatures;
		this->TentativePipeTemp = FirstTemperatures;
		this->PipeTemp = FirstTemperatures;
		this->PreviousPipeTemp = FirstTemperatures;
		this->PreviousSimTime = 0.0;
		this->DeltaTime = 0.0;
		this->FluidOutletTemp = 0.0;
		this->EnvironmentTemp = 0.0;
		this->EnvHeatLossRate = 0.0;
		this->FluidHeatLossRate = 0.0;
		this->ZoneHeatGainRate = 0.0;

		// return value can be used to report success/failure
		return 0;
	}

	int
	PipeHTData::performFirstHVACInit() {
	
		//We need to update boundary conditions here, as well as updating the arrays
		if ( this->EnvironmentPtr == GroundEnv ) {

			// And then update Ground Boundary Conditions
			Real64 CurSimDay = double( DataGlobals::DayOfSim );
			for ( int TimeIndex = 1; TimeIndex <= TentativeTimeIndex; ++TimeIndex ) {
				for ( int LengthIndex = 1; LengthIndex <= this->NumSections; ++LengthIndex ) {
					for ( int DepthIndex = 1; DepthIndex <= this->NumDepthNodes; ++DepthIndex ) {
						//Farfield boundary
						Real64 CurrentDepth = ( DepthIndex - 1 ) * this->dSregular;
						this->T( 1, DepthIndex, LengthIndex, TimeIndex ) = this->farfieldTemperature( CurrentDepth, CurSimDay );
					}
					for ( int WidthIndex = 1; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {
						//Bottom side of boundary
						Real64 CurrentDepth = this->DomainDepth;
						this->T( WidthIndex, this->NumDepthNodes, LengthIndex, TimeIndex ) = this->farfieldTemperature( CurrentDepth, CurSimDay );
					}
				}
			}
		}

		// should next choose environment temperature according to coupled with air or ground
		if ( this->EnvironmentPtr == GroundEnv ) {
			//this->EnvironmentTemp = GroundTemp
		} else if ( this->EnvironmentPtr == OutsideAirEnv ) {
			this->EnvironmentTemp = DataEnvironment::OutDryBulbTemp;
		} else if ( this->EnvironmentPtr == ZoneEnv ) {
			this->EnvironmentTemp = DataHeatBalFanSys::MAT( this->EnvrZonePtr );
		} else if ( this->EnvironmentPtr == ScheduleEnv ) {
			this->EnvironmentTemp = ScheduleManager::GetCurrentScheduleValue( this->EnvrSchedPtr );
		} else if ( this->EnvironmentPtr == None ) { //default to outside temp
			this->EnvironmentTemp = DataEnvironment::OutDryBulbTemp;
		}

		// return value could be used to report success/failure
		return 0;
	}

	int
	PipeHTData::simulate( const PlantLocation & EP_UNUSED(calledFromLocation) ) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine uses the Kusuda-Achenbach correlation to return a farfield ground temperature

		// make the calculations
		for ( int InnerTimeStepCtr = 1; InnerTimeStepCtr <= this->NumInnerTimeSteps; ++InnerTimeStepCtr ) {
			if ( this->EnvironmentPtr == GroundEnv ) {
				this->calcBuriedPipeSoil();
			} else {
				this->calcPipesHeatTransfer();
			}
			this->pushInnerTimeStepArrays();
		}
		// update vaiables
		this->updatePipesHeatTransfer();
		// update report variables
		this->reportPipesHeatTransfer();
		// return value could be used to report success/failure
		return 0;
	}

	void
	PipeHTData::pushInnerTimeStepArrays() {
		if ( this->EnvironmentPtr == GroundEnv ) {
			for ( int LengthIndex = 2; LengthIndex <= this->NumSections; ++LengthIndex ) {
				for ( int DepthIndex = 1; DepthIndex <= this->NumDepthNodes; ++DepthIndex ) {
					for ( int WidthIndex = 2; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {
						//This will store the old 'current' values as the new 'previous values'  This allows
						// us to use the previous time array as history terms in the equations
						this->T( WidthIndex, DepthIndex, LengthIndex, PreviousTimeIndex ) = this->T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex );
					}
				}
			}
		}
		//Then update the Hanby near pipe model temperatures
		this->PreviousFluidTemp = this->FluidTemp;
		this->PreviousPipeTemp = this->PipeTemp;
	}

	int
	PipeHTData::validatePipeConstruction() {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine, called from GetInput, validates the pipe construction usage.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Resistance = 0; // overall thermal resistance [m^2.C/W]
		Real64 Density; // average density [kg/m^3]
		Real64 TotThickness = 0; // total thickness of all layers
		Real64 SpHeat; // average specific heat [J/kg.K]
		int LayerNum;
		int TotalLayers; // total number of layers (pipe layer + insulation layers)

		// CTF stuff
		TotalLayers = DataHeatBalance::Construct( ConstructionNum ).TotLayers;
		auto & thisConstruct = DataHeatBalance::Construct( this->ConstructionNum );
		
		// get pipe properties
		if ( TotalLayers == 1 ) { // no insulation layer

			auto & firstLayer = DataHeatBalance::Material( thisConstruct.LayerPoint( 1 ) );
			this->PipeConductivity = firstLayer.Conductivity;
			this->PipeDensity = firstLayer.Density;
			this->PipeCp = firstLayer.SpecHeat;
			this->PipeOD = this->PipeID + 2.0 * firstLayer.Thickness;
			this->InsulationOD = this->PipeOD;
			this->SumTK = firstLayer.Thickness / firstLayer.Conductivity;

		} else if ( TotalLayers >= 2 ) { // first layers are insulation, last layer is pipe

			for ( LayerNum = 1; LayerNum <= TotalLayers - 1; ++LayerNum ) {
				auto & thisLayer = DataHeatBalance::Material( thisConstruct.LayerPoint( LayerNum ) );
				Resistance += thisLayer.Thickness / thisLayer.Conductivity;
				Density = thisLayer.Density * thisLayer.Thickness;
				TotThickness += thisLayer.Thickness;
				SpHeat = thisLayer.SpecHeat * thisLayer.Thickness;
				this->InsulationThickness = thisLayer.Thickness;
				this->SumTK += thisLayer.Thickness / thisLayer.Conductivity;
			}

			this->InsulationResistance = Resistance;
			this->InsulationConductivity = TotThickness / Resistance;
			this->InsulationDensity = Density / TotThickness;
			this->InsulationCp = SpHeat / TotThickness;
			this->InsulationThickness = TotThickness;

			auto & lastLayer = DataHeatBalance::Material( thisConstruct.LayerPoint( LayerNum ) );
			this->PipeConductivity = lastLayer.Conductivity;
			this->PipeDensity = lastLayer.Density;
			this->PipeCp = lastLayer.SpecHeat;
			this->PipeOD = this->PipeID + 2.0 * lastLayer.Thickness;
			this->InsulationOD = this->PipeOD + 2.0 * this->InsulationThickness;

		}
		return 0;
	}

	void
	PipeHTData::calcPipesHeatTransfer(int LengthIndex) {

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

		// traps fluid properties problems such as freezing conditions
		if ( this->FluidSpecHeat <= 0.0 || this->FluidDensity <= 0.0 ) {
			// leave the state of the pipe as it was
			this->FluidOutletTemp = this->TentativeFluidTemp( this->NumSections );
			// set heat transfer rates to zero for consistency
			this->EnvHeatLossRate = 0.0;
			this->FluidHeatLossRate = 0.0;
			return;
		}

		Real64 FluidConvCoef = this->calcPipeHeatTransCoef();
		Real64 AirConvCoef;
		if ( this->EnvironmentPtr != GroundEnv ) {
			// Revised by L. Gu by including insulation conductance 6/19/08
			AirConvCoef = 1.0 / ( 1.0 / this->outsidePipeHeatTransCoef() + this->InsulationResistance );
		}

		// heat transfer to air or ground
		Real64 EnvHeatTransCoef;
		if ( this->EnvironmentPtr == GroundEnv ) {
			//Approximate conductance using ground conductivity, (h=k/L), where L is grid spacing
			// between pipe wall and next closest node.
			EnvHeatTransCoef = this->SoilConductivity / ( this->dSregular - ( this->PipeID / 2.0 ) );
		} else if ( this->EnvironmentPtr == OutsideAirEnv ) {
			EnvHeatTransCoef = AirConvCoef;
		} else if ( this->EnvironmentPtr == ZoneEnv ) {
			EnvHeatTransCoef = AirConvCoef;
		} else if ( this->EnvironmentPtr == ScheduleEnv ) {
			EnvHeatTransCoef = AirConvCoef;
		} else if ( this->EnvironmentPtr == None ) {
			EnvHeatTransCoef = 0.0;
		} else {
			EnvHeatTransCoef = 0.0;
		}

		// work out the coefficients
		Real64 FluidNodeHeatCapacity = this->SectionArea * this->Length / this->NumSections * this->FluidSpecHeat * this->FluidDensity; // Mass of Node x Specific heat

		// coef of fluid heat balance
		Real64 A1 = FluidNodeHeatCapacity + this->MassFlowRate * this->FluidSpecHeat * DeltaTime + FluidConvCoef * this->InsideArea * DeltaTime;
		Real64 A2 = MassFlowRate * this->FluidSpecHeat * DeltaTime;
		Real64 A3 = FluidConvCoef * this->InsideArea * DeltaTime;
		Real64 A4 = FluidNodeHeatCapacity;

		// coef of pipe heat balance
		Real64 B1 = this->PipeHeatCapacity + FluidConvCoef * this->InsideArea * DeltaTime + EnvHeatTransCoef * this->OutsideArea * DeltaTime;
		Real64 B2 = A3;
		Real64 B3 = EnvHeatTransCoef * this->OutsideArea * DeltaTime;
		Real64 B4 = this->PipeHeatCapacity;

		this->TentativeFluidTemp( 0 ) = this->FluidInletTemp;

		this->TentativePipeTemp( 0 ) = this->PipeTemp( 1 ); // for convenience

		if ( LengthIndex > -1 ) { //Just simulate the single section if being called from Pipe:Underground

			Real64 PipeDepth = this->PipeNodeDepth;
			Real64 PipeWidth = this->PipeNodeWidth;
			Real64 TempBelow = this->T( PipeWidth, PipeDepth + 1, LengthIndex, CurrentTimeIndex );
			Real64 TempBeside = this->T( PipeWidth - 1, PipeDepth, LengthIndex, CurrentTimeIndex );
			Real64 TempAbove = this->T( PipeWidth, PipeDepth - 1, LengthIndex, CurrentTimeIndex );
			Real64 EnvironmentTemp = ( TempBelow + TempBeside + TempAbove ) / 3.0;

			this->TentativeFluidTemp( LengthIndex ) = ( A2 * this->TentativeFluidTemp( LengthIndex - 1 ) + A3 / B1 * ( B3 * EnvironmentTemp + B4 * this->PreviousPipeTemp( LengthIndex ) ) + A4 * this->PreviousFluidTemp( LengthIndex ) ) / ( A1 - A3 * B2 / B1 );

			this->TentativePipeTemp( LengthIndex ) = ( B2 * this->TentativeFluidTemp( LengthIndex ) + B3 * EnvironmentTemp + B4 * this->PreviousPipeTemp( LengthIndex ) ) / B1;

			// Get exterior surface temperature from energy balance at the surface
			Real64 Numerator = this->EnvironmentTemp - this->TentativeFluidTemp( LengthIndex );
			Real64 Denominator = EnvHeatTransCoef * ( ( 1 / EnvHeatTransCoef ) + this->SumTK );
			Real64 SurfaceTemp = this->EnvironmentTemp - Numerator / Denominator;

			// keep track of environmental heat loss rate - not same as fluid loss at same time
			this->EnvHeatLossRate += EnvHeatTransCoef * this->OutsideArea * ( SurfaceTemp - this->EnvironmentTemp );

		} else { //Simulate all sections at once if not pipe:underground

			// start loop along pipe
			// b1 must not be zero but this should have been checked on input
			for ( int curnode = 1; curnode <= this->NumSections; ++curnode ) {
				this->TentativeFluidTemp( curnode ) = ( A2 * this->TentativeFluidTemp( curnode - 1 ) + A3 / B1 * ( B3 * EnvironmentTemp + B4 * this->PreviousPipeTemp( curnode ) ) + A4 * this->PreviousFluidTemp( curnode ) ) / ( A1 - A3 * B2 / B1 );

				this->TentativePipeTemp( curnode ) = ( B2 * this->TentativeFluidTemp( curnode ) + B3 * EnvironmentTemp + B4 * this->PreviousPipeTemp( curnode ) ) / B1;

				// Get exterior surface temperature from energy balance at the surface
				Real64 Numerator = this->EnvironmentTemp - this->TentativeFluidTemp( curnode );
				Real64 Denominator = EnvHeatTransCoef * ( ( 1 / EnvHeatTransCoef ) + this->SumTK );
				Real64 SurfaceTemp = this->EnvironmentTemp - Numerator / Denominator;

				// Keep track of environmental heat loss
				this->EnvHeatLossRate += EnvHeatTransCoef * this->OutsideArea * ( SurfaceTemp - this->EnvironmentTemp );

			}

		}

		this->FluidHeatLossRate = MassFlowRate * this->FluidSpecHeat * ( this->TentativeFluidTemp( 0 ) - this->TentativeFluidTemp( this->NumSections ) );

		this->FluidOutletTemp = this->TentativeFluidTemp( this->NumSections );

	}

	void
	PipeHTData::calcBuriedPipeSoil() {

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
		Array3D< Real64 > T_O( this->PipeNodeWidth, this->NumDepthNodes, NumSections );

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
		this->FourierDS = this->SoilDiffusivity * DeltaTime / pow_2( this->dSregular ); //Eq. D4
		this->CoefA1 = this->FourierDS / ( 1 + 4 * this->FourierDS ); //Eq. D2
		this->CoefA2 = 1 / ( 1 + 4 * this->FourierDS ); //Eq. D3

		for ( IterationIndex = 1; IterationIndex <= MaxIterations; ++IterationIndex ) {
			if ( IterationIndex == MaxIterations ) {
				ShowWarningError( "BuriedPipeHeatTransfer: Large number of iterations detected in object: " + this->Name );
			}

			//Store computed values in T_O array
			for ( LengthIndex = 2; LengthIndex <= this->NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= this->NumDepthNodes - 1; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {
						T_O( WidthIndex, DepthIndex, LengthIndex ) = this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex );
					}
				}
			}

			//Loop along entire length of pipe, analyzing cross sects
			for ( LengthIndex = 1; LengthIndex <= this->NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= this->NumDepthNodes - 1; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {

						if ( DepthIndex == 1 ) { //Soil Surface Boundary

							//If on soil boundary, load up local variables and perform calculations
							NodePast = this->T( WidthIndex, DepthIndex, LengthIndex, PreviousTimeIndex );
							PastNodeTempAbs = NodePast + DataGlobals::KelvinConv;
							SkyTempAbs = DataEnvironment::SkyTemp + DataGlobals::KelvinConv;
							TopRoughness = this->SoilRoughness;
							TopThermAbs = this->SoilThermAbs;
							TopSolarAbs = this->SoilSolarAbs;
							kSoil = this->SoilConductivity;
							dS = this->dSregular;
							rho = this->SoilDensity;
							Cp = this->SoilCp;

							// ASHRAE simple convection coefficient model for external surfaces.
							this->OutdoorConvCoef = ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff( TopRoughness, DataEnvironment::WindSpeed );
							ConvCoef = this->OutdoorConvCoef;

							// thermal radiation coefficient using surf temp from past time step
							if ( std::abs( PastNodeTempAbs - SkyTempAbs ) > DataGlobals::rTinyValue ) {
								RadCoef = StefBoltzmann * TopThermAbs * ( pow_4( PastNodeTempAbs ) - pow_4( SkyTempAbs ) ) / ( PastNodeTempAbs - SkyTempAbs );
							} else {
								RadCoef = 0.0;
							}

							// total absorbed solar - no ground solar
							QSolAbsorbed = TopSolarAbs * ( max( DataEnvironment::SOLCOS( 3 ), 0.0 ) * DataEnvironment::BeamSolarRad + DataEnvironment::DifSolarRad );

							// If sun is not exposed, then turn off both solar and thermal radiation
							if ( ! this->SolarExposed ) {
								RadCoef = 0.0;
								QSolAbsorbed = 0.0;
							}

							if ( WidthIndex == this->PipeNodeWidth ) { //Symmetric centerline boundary

								//-Coefficients and Temperatures
								NodeBelow = this->T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
								NodeLeft = this->T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );

								//-Update Equation, basically a detailed energy balance at the surface
								this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = ( QSolAbsorbed + RadCoef * DataEnvironment::SkyTemp + ConvCoef * DataEnvironment::OutDryBulbTemp + ( kSoil / dS ) * ( NodeBelow + 2 * NodeLeft ) + ( rho * Cp / DeltaTime ) * NodePast ) / ( RadCoef + ConvCoef + 3 * ( kSoil / dS ) + ( rho * Cp / DeltaTime ) );

							} else { //Soil surface, but not on centerline

								//-Coefficients and Temperatures
								NodeBelow = this->T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
								NodeLeft = this->T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );
								NodeRight = this->T( WidthIndex + 1, DepthIndex, LengthIndex, CurrentTimeIndex );

								//-Update Equation
								this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = ( QSolAbsorbed + RadCoef * DataEnvironment::SkyTemp + ConvCoef * DataEnvironment::OutDryBulbTemp + ( kSoil / dS ) * ( NodeBelow + NodeLeft + NodeRight ) + ( rho * Cp / DeltaTime ) * NodePast ) / ( RadCoef + ConvCoef + 3 * ( kSoil / dS ) + ( rho * Cp / DeltaTime ) );

							} //Soil-to-air surface node structure

						} else if ( WidthIndex == this->PipeNodeWidth ) { //On Symmetric centerline boundary

							if ( DepthIndex == this->PipeNodeDepth ) { //On the node containing the pipe

								//-Call to simulate a single pipe segment (by passing OPTIONAL LengthIndex argument)
								this->calcPipesHeatTransfer( LengthIndex );

								//-Update node for cartesian system
								this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = this->PipeTemp( LengthIndex );

							} else if ( DepthIndex != 1 ) { //Not surface node

								//-Coefficients and Temperatures
								NodeLeft = this->T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );
								NodeAbove = this->T( WidthIndex, DepthIndex - 1, LengthIndex, CurrentTimeIndex );
								NodeBelow = this->T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
								NodePast = this->T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex - 1 );
								A1 = this->CoefA1;
								A2 = this->CoefA2;

								//-Update Equation
								this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = A1 * ( NodeBelow + NodeAbove + 2 * NodeLeft ) + A2 * NodePast;

							} //Symmetric centerline node structure

						} else { //All Normal Interior Nodes

							//-Coefficients and Temperatures
							A1 = this->CoefA1;
							A2 = this->CoefA2;
							NodeBelow = this->T( WidthIndex, DepthIndex + 1, LengthIndex, CurrentTimeIndex );
							NodeAbove = this->T( WidthIndex, DepthIndex - 1, LengthIndex, CurrentTimeIndex );
							NodeRight = this->T( WidthIndex + 1, DepthIndex, LengthIndex, CurrentTimeIndex );
							NodeLeft = this->T( WidthIndex - 1, DepthIndex, LengthIndex, CurrentTimeIndex );
							NodePast = this->T( WidthIndex, DepthIndex, LengthIndex, CurrentTimeIndex - 1 );

							//-Update Equation
							this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex ) = A1 * ( NodeBelow + NodeAbove + NodeRight + NodeLeft ) + A2 * NodePast; //Eq. D1

						}
					}
				}
			}

			//Check for convergence
			for ( LengthIndex = 2; LengthIndex <= this->NumSections; ++LengthIndex ) {
				for ( DepthIndex = 1; DepthIndex <= this->NumDepthNodes - 1; ++DepthIndex ) {
					for ( WidthIndex = 2; WidthIndex <= this->PipeNodeWidth; ++WidthIndex ) {
						Ttemp = this->T( WidthIndex, DepthIndex, LengthIndex, TentativeTimeIndex );
						if ( std::abs( T_O( WidthIndex, DepthIndex, LengthIndex ) - Ttemp ) > ConvCrit ) goto IterationLoop_loop;
					}
				}
			}

			//If we didn't cycle back, then the system is converged
			goto IterationLoop_exit;

			IterationLoop_loop: ;
		}
		IterationLoop_exit: ;

	}

	void
	PipeHTData::updatePipesHeatTransfer() {

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the outlet node conditions

		auto & outletNode = DataLoopNode::Node( this->OutletNodeNum );
		auto & inletNode = DataLoopNode::Node( this->InletNodeNum );
		
		outletNode.Temp = this->FluidOutletTemp;

		// pass everything else through
		outletNode.TempMin = inletNode.TempMin;
		outletNode.TempMax = inletNode.TempMax;
		outletNode.MassFlowRate = inletNode.MassFlowRate;
		outletNode.MassFlowRateMin = inletNode.MassFlowRateMin;
		outletNode.MassFlowRateMax = inletNode.MassFlowRateMax;
		outletNode.MassFlowRateMinAvail = inletNode.MassFlowRateMinAvail;
		outletNode.MassFlowRateMaxAvail = inletNode.MassFlowRateMaxAvail;
		outletNode.Quality = inletNode.Quality;
		//Only pass pressure if we aren't doing a pressure simulation
		if ( DataPlant::PlantLoop( this->LoopNum ).PressureSimType > 1 ) {
			//Don't do anything
		} else {
			outletNode.Press = inletNode.Press;
		}
		outletNode.Enthalpy = inletNode.Enthalpy;
		outletNode.HumRat = inletNode.HumRat;
	}

	void
	PipeHTData::reportPipesHeatTransfer() {

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates report variables for pipe heat transfer objects

		this->MassFlowRate = this->MassFlowRate;
		this->VolumeFlowRate = this->VolumeFlowRate;

		// update other variables from module variables
		this->FluidHeatLossRate = this->FluidHeatLossRate;
		this->FluidHeatLossEnergy = this->FluidHeatLossRate * this->DeltaTime; // DeltaTime is in seconds
		this->PipeInletTemp = this->PipeTemp( 1 );
		this->PipeOutletTemp = this->PipeTemp( this->NumSections );

		// need to average the heat rate because it is now summing over multiple inner time steps
		this->EnvironmentHeatLossRate = this->EnvHeatLossRate / this->NumInnerTimeSteps;
		this->EnvHeatLossEnergy = this->EnvironmentHeatLossRate * this->DeltaTime;

		// for zone heat gains, we assign the averaged heat rate over all inner time steps
		if ( this->EnvironmentPtr == ZoneEnv ) {
			this->ZoneHeatGainRate = this->EnvironmentHeatLossRate;
		}
	}

	Real64
	PipeHTData::calcPipeHeatTransCoef() {
		
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

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PipeHeatTransfer::CalcPipeHeatTransCoef: " );
		Real64 const MaxLaminarRe( 2300.0 ); // Maximum Reynolds number for laminar flow
		int const NumOfPropDivisions( 13 ); // intervals in property correlation
		static Array1D< Real64 > const Temps( NumOfPropDivisions, { 1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85 } ); // Temperature, in C
		static Array1D< Real64 > const Mu( NumOfPropDivisions, { 0.001652, 0.001422, 0.001225, 0.00108, 0.000959, 0.000855, 0.000769, 0.000695, 0.000631, 0.000577, 0.000528, 0.000489, 0.000453 } ); // Viscosity, in Ns/m2
		static Array1D< Real64 > const Conductivity( NumOfPropDivisions, { 0.574, 0.582, 0.590, 0.598, 0.606, 0.613, 0.620, 0.628, 0.634, 0.640, 0.645, 0.650, 0.656 } ); // Conductivity, in W/mK
		static Array1D< Real64 > const Pr( NumOfPropDivisions, { 12.22, 10.26, 8.81, 7.56, 6.62, 5.83, 5.20, 4.62, 4.16, 3.77, 3.42, 3.15, 2.88 } ); // Prandtl number (dimensionless)

		Real64 const Temperature = this->FluidInletTemp; // Temperature of water entering the surface, in C
		Real64 const MassFlowRate = this->MassFlowRate; // Mass flow rate, in kg/s
		Real64 const Diameter = this->PipeID; // Pipe diameter, m

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 InterpFrac;
		Real64 NuD;
		Real64 ReD;
		Real64 Kactual;
		Real64 MUactual;
		Real64 PRactual;

		//retrieve loop index for this component so we can look up fluid properties
		auto thisLoop = DataPlant::PlantLoop( this->LoopNum );

		//since the fluid properties routine doesn't have Prandtl, we'll just use water values
		int idx = 1;
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
		Kactual = FluidProperties::GetConductivityGlycol( thisLoop.FluidName, this->FluidTemp( 0 ), thisLoop.FluidIndex, RoutineName ); //W/m-K
		MUactual = FluidProperties::GetViscosityGlycol( thisLoop.FluidName, this->FluidTemp( 0 ), thisLoop.FluidIndex, RoutineName ) / 1000.0; //Note fluid properties routine returns mPa-s, we need Pa-s

		// Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter) - as RadiantSysLowTemp
		ReD = 4.0 * MassFlowRate / ( DataGlobals::Pi * MUactual * Diameter );

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

		return Kactual * NuD / Diameter;

	}

	Real64
	PipeHTData::outsidePipeHeatTransCoef() {

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

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Pr( 0.7 ); // Prandl number for air (assume constant)
		Real64 const CondAir( 0.025 ); // thermal conductivity of air (assume constant) [W/m.K]
		Real64 const RoomAirVel( 0.381 ); // room air velocity of 75 ft./min [m/s]
		Real64 const NaturalConvNusselt( 0.36 );
		//Nusselt for natural convection for horizontal cylinder
		//from: Correlations for Convective Heat Transfer
		//      Dr. Bernhard Spang
		//      Chemical Engineers' Resource Page: http://www.cheresources.com/convection.pdf
		int const NumOfParamDivisions( 5 ); // intervals in property correlation
		int const NumOfPropDivisions( 12 ); // intervals in property correlation

		static Array1D< Real64 > const CCoef( NumOfParamDivisions, { 0.989, 0.911, 0.683, 0.193, 0.027 } ); // correlation coefficient
		static Array1D< Real64 > const mExp( NumOfParamDivisions, { 0.33, 0.385, 0.466, 0.618, 0.805 } ); // exponent
		static Array1D< Real64 > const LowerBound( NumOfParamDivisions, { 0.4, 4.0, 40.0, 4000.0, 40000.0 } ); // upper bound of correlation range
		static Array1D< Real64 > const UpperBound( NumOfParamDivisions, { 4.0, 40.0, 4000.0, 40000.0, 400000.0 } ); // lower bound of correlation range

		static Array1D< Real64 > const Temperature( NumOfPropDivisions, { -73.0, -23.0, -10.0, 0.0, 10.0, 20.0, 27.0, 30.0, 40.0, 50.0, 76.85, 126.85 } ); // temperature [C]
		static Array1D< Real64 > const DynVisc( NumOfPropDivisions, { 75.52e-7, 11.37e-6, 12.44e-6, 13.3e-6, 14.18e-6, 15.08e-6, 15.75e-6, 16e-6, 16.95e-6, 17.91e-6, 20.92e-6, 26.41e-6 } ); // dynamic viscosity [m^2/s]

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
		
		if ( this->compType == DataPlant::TypeOf_PipeInterior ) {

			if ( this->EnvironmentPtr == ScheduleEnv ) {
				AirTemp = GetCurrentScheduleValue( this->EnvrSchedPtr );
				AirVel = GetCurrentScheduleValue( this->EnvrVelSchedPtr );

			} else if ( this->EnvironmentPtr == ZoneEnv ) {
				AirTemp = MAT( this->EnvrZonePtr );
				AirVel = RoomAirVel;
			}

		} else if ( this->compType == DataPlant::TypeOf_PipeExterior ) {

			if ( this->EnvironmentPtr == OutsideAirEnv ) {
				AirTemp = Node( this->EnvrAirNodeNum ).Temp;
				AirVel = WindSpeed;
			}

		}

		PipeOD = this->InsulationOD;

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
				ShowWarningError( "Heat Transfer Pipe = " + this->Name + "Viscosity out of range, air temperature too high, setting to upper limit." );
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
				ShowWarningError( "Heat Transfer Pipe = " + this->Name + "Reynolds Number out of range, setting coefficients to upper limit." );
			}
		}

		// Calculate the Nusselt number
		NuD = Coef * std::pow( ReD, rExp ) * std::pow( Pr, 1.0 / 3.0 );

		// If the wind speed is too small, we need to use natural convection behavior:
		NuD = max( NuD, NaturalConvNusselt );

		// h = (k)(Nu)/D
		return CondAir * NuD / PipeOD;

	}

	Real64
	PipeHTData::farfieldTemperature(
		Real64 const z, // Current Depth
		Real64 const DayOfSim // Current Simulation Day
	) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine uses the Kusuda-Achenbach correlation to return a farfield ground temperature

		return this->AvgGroundTemp - this->AvgGndTempAmp * 
				std::exp( -z * std::sqrt( DataGlobals::Pi / ( 365.0 * this->SoilDiffusivityPerDay ) ) ) * 
				std::cos( ( 2.0 * DataGlobals::Pi / 365.0 ) * ( DayOfSim - this->PhaseShiftDays - ( z / 2.0 ) * std::sqrt( 365.0 / ( DataGlobals::Pi * this->SoilDiffusivityPerDay ) ) ) );
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

} // PipeHeatTransfer

} // EnergyPlus
