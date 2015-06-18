// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <SurfaceGroundHeatExchanger.hh>
#include <BranchNodeConnections.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SurfaceGroundHeatExchanger {

	// Module containing the routines dealing with surface/panel ground heat exchangers

	// MODULE INFORMATION:
	//       AUTHOR         Simon Rees
	//       DATE WRITTEN   August 2002
	//       MODIFIED       Brent Griffith, Sept 2010, plant upgrades
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate hydronic Surface Ground Heat
	// Exchangers. This includes pavement surfaces with embedded pipes for snow-
	// melting or heat rejection from hybrid ground source heat pump systems.
	// The heat exchanger may be gound coupled or not. In the latter case the
	// bottom surface is exposed to the wind but not solar gains.

	// METHODOLOGY EMPLOYED:
	// This model is based on the QTF formulation of heat transfer through
	// building elements with embedded heat sources/sinks. The model uses
	// a heat exchanger analogy to relate the inlet fluid temperature to the
	// net heat transfer rate and consequently outlet temperature. The model
	// is entirely passive i.e. it does not set any flow rates or incorporate
	// any controls. In order to deal with the non-linear boundary conditions
	// at the top surface due to the presence of ice/snow fluxes have to be
	// calculated by the QTF model and temperature calculated from the surface
	// heat balance. This requires some iteration.
	// Note: top surface variables correspond to 'outside' variables in standard
	// CTF/QTF definition. Bottom surface variables correspond to 'inside' variables.

	// REFERENCES:
	// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
	//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
	//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
	//   Engineering.
	// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
	//   of Wisconsin-Madison.

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::KelvinConv;
	using namespace DataLoopNode;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	Real64 const SmallNum( 1.0e-30 ); // Very small number to avoid div0 errors
	Real64 const StefBoltzmann( 5.6697e-08 ); // Stefan-Boltzmann constant
	Real64 const SurfaceHXHeight( 0.0 ); // Surface Height above ground -- used in height dependent calcs.
	static std::string const BlankString;

	int const SurfCond_Ground( 1 );
	int const SurfCond_Exposed( 2 );

namespace loc {
	int const MaxCTFTerms( 19 ); // Maximum number of CTF terms allowed to still allow stability //Note Duplicate of DataHeatBalance::MaxCTFTerms to avoid static initialization order bug: Keep them in sync
} // loc

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// utility variables initialized once
	int NumOfSurfaceGHEs( 0 ); // Number of surface GHE ground heat exchangers
	bool NoSurfaceGroundTempObjWarning( true ); // This will cause a warning to be issued if no "surface" ground
	// temperature object was input.
	// Utility variables - initialized for each instance of a surface GHE
	int InletNodeNum( 0 ); // inlet node number
	int OutletNodeNum( 0 ); // oulet node number
	int ConstructionNum( 0 ); // construction index number
	int TopRoughness( 0 ); // roughness of top layer
	int BtmRoughness( 0 ); // roughness of bottom layer
	Real64 InletTemp( 0.0 ); // water inlet temperature
	Real64 OutletTemp( 0.0 ); // water outlet temperature
	Real64 FlowRate( 0.0 ); // water mass flow rate
	Real64 TopSurfTemp( 0.0 ); // Top  surface temperature
	Real64 BtmSurfTemp( 0.0 ); // Bottom  surface temperature
	Real64 TopSurfFlux( 0.0 ); // Top  surface heat flux
	Real64 BtmSurfFlux( 0.0 ); // Bottom  surface heat flux
	Real64 SourceFlux( 0.0 ); // total heat transfer rate, Watts
	Real64 SourceTemp( 0.0 ); // total heat transfer rate, Watts
	Real64 SurfaceArea( 0.0 ); // surface GHE surface area
	Real64 TopThermAbs( 0.0 ); // Thermal absortivity of top layer
	Real64 BtmThermAbs( 0.0 ); // Thermal absortivity of bottom layer
	Real64 TopSolarAbs( 0.0 ); // Solar absortivity of top layer
	Array1D_bool CheckEquipName;

	// weather data records updated every zone time step
	Real64 PastBeamSolarRad( 0.0 ); // Previous beam normal solar irradiance
	Real64 PastSolarDirCosVert( 0.0 ); // Previous vertical component of solar normal
	Real64 PastDifSolarRad( 0.0 ); // Previous sky diffuse solar horizontal irradiance
	Real64 PastGroundTemp( 0.0 ); // Previous ground temperature
	bool PastIsRain( false ); // Previous Surfaces are wet for this time interval
	bool PastIsSnow( false ); // Previous Snow on the ground for this time interval
	Real64 PastOutBaroPress( 0.0 ); // Previous outdoor air barometric pressure
	Real64 PastOutDryBulbTemp( 0.0 ); // Previous outdoor air dry bulb temperature
	Real64 PastOutHumRat( 0.0 ); // Previous outdoor air humidity ratio
	Real64 PastOutAirDensity( 0.0 ); // Previous outdoor air density
	Real64 PastOutWetBulbTemp( 0.0 ); // Previous outdoor air wet bulb temperature
	Real64 PastOutDewPointTemp( 0.0 ); // Previous outdoor dewpoint temperature
	Real64 PastSkyTemp( 0.0 ); // Previous sky temperature
	Real64 PastWindSpeed( 0.0 ); // Previous outdoor air wind speed
	Real64 PastCloudFraction( 0.0 ); // Previous Fraction of sky covered by clouds

	// time keeping variables used for keeping track of average flux over each time step
	Array1D< Real64 > QRadSysSrcAvg; // Average source over the time step
	Array1D< Real64 > LastSysTimeElapsed; // record of system time
	Array1D< Real64 > LastTimeStepSys; // previous time step size

	// SUBROUTINE SPECIFICATIONS FOR MODULE PlantSurfaceGroundHeatExchangers

	// Object Data
	Array1D< SurfaceGroundHeatExchangerData > SurfaceGHE;
	Array1D< SurfaceGroundHeatExchangerQTF > SurfaceGHEQTF;
	Array1D< SurfaceGroundHeatExchngrReport > SurfaceGHEReport;

	//==============================================================================

	// Functions

	void
	SimSurfaceGroundHeatExchanger(
		std::string const & CompName, // name of the surface GHE
		int & CompIndex,
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		bool const RunFlag, // TRUE if equipment is operating
		bool & InitLoopEquip
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the public routine that is used to simulate
		// the operation of surface ground heat exchangers at each system
		// time step.

		// METHODOLOGY EMPLOYED:
		// Several private routines are called to get data, make the calculations
		// and update stuff. This is called for each instance of surface GHE components.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER,          INTENT(IN)  :: FlowLock            ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // Flag first time, input is fetched
		int SurfaceGHENum( 0 ); // index in local derived types

		// check for input
		if ( GetInputFlag ) {
			GetSurfaceGroundHeatExchanger();
			GetInputFlag = false;
		}

		if ( InitLoopEquip ) {
			SurfaceGHENum = FindItemInList( CompName, SurfaceGHE.Name(), NumOfSurfaceGHEs );
			CompIndex = SurfaceGHENum;
			return;
		}

		// Find the correct Surface Ground Heat Exchanger
		if ( CompIndex <= 0 ) {
			ShowFatalError( "SimSurfaceGroundHeatExchanger: Unit not found=" + CompName );
		} else {
			SurfaceGHENum = CompIndex;
			if ( SurfaceGHENum > NumOfSurfaceGHEs || SurfaceGHENum < 1 ) {
				ShowFatalError( "SimSurfaceGroundHeatExchanger:  Invalid CompIndex passed=" + TrimSigDigits( SurfaceGHENum ) + ", Number of Units=" + TrimSigDigits( NumOfSurfaceGHEs ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( SurfaceGHENum ) ) {
				if ( CompName != SurfaceGHE( SurfaceGHENum ).Name ) {
					ShowFatalError( "SimSurfaceGroundHeatExchanger: Invalid CompIndex passed=" + TrimSigDigits( SurfaceGHENum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + SurfaceGHE( SurfaceGHENum ).Name );
				}
				CheckEquipName( SurfaceGHENum ) = false;
			}
		}

		// initialize
		InitSurfaceGroundHeatExchanger( SurfaceGHENum, RunFlag ); //DSU
		// make the calculations
		CalcSurfaceGroundHeatExchanger( SurfaceGHENum, FirstHVACIteration ); //DSU
		// update vaiables
		UpdateSurfaceGroundHeatExchngr( SurfaceGHENum ); //DSU
		// update report variables
		ReportSurfaceGroundHeatExchngr( SurfaceGHENum );

	}

	//==============================================================================

	void
	GetSurfaceGroundHeatExchanger()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for hydronic Surface Ground Heat Exchangers
		// from the user input file.  This will contain all of the information
		// needed to define and simulate the surface.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Construct;
		using DataHeatBalance::TotConstructs;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::CheckFluidPropertyName;
		using FluidProperties::FindGlycol;
		using DataEnvironment::GroundTemp_Surface;
		using DataEnvironment::GroundTemp_SurfaceObjInput;
		using General::RoundSigDigits;
		using namespace DataLoopNode;

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

		static bool ErrorsFound( false ); // Set to true if errors in input,
		// fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		//  INTEGER                        :: NumFluids            ! number of fluids in sim.

		// Initializations and allocations
		cCurrentModuleObject = "GroundHeatExchanger:Surface";
		NumOfSurfaceGHEs = GetNumObjectsFound( cCurrentModuleObject );
		// allocate data structures
		if ( allocated( SurfaceGHE ) ) SurfaceGHE.deallocate();
		if ( allocated( SurfaceGHEQTF ) ) SurfaceGHEQTF.deallocate();
		if ( allocated( SurfaceGHEReport ) ) SurfaceGHEReport.deallocate();

		SurfaceGHE.allocate( NumOfSurfaceGHEs );
		SurfaceGHEQTF.allocate( NumOfSurfaceGHEs );
		SurfaceGHEReport.allocate( NumOfSurfaceGHEs );
		CheckEquipName.dimension( NumOfSurfaceGHEs, true );

		// initialize data structures
		// surface data
		// Obtain all of the user data related to the surfaces...
		for ( Item = 1; Item <= NumOfSurfaceGHEs; ++Item ) {

			// get the input data
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );

			// General user input data
			SurfaceGHE( Item ).Name = cAlphaArgs( 1 );
			SurfaceGHE( Item ).ConstructionName = cAlphaArgs( 2 );
			SurfaceGHE( Item ).ConstructionNum = FindItemInList( cAlphaArgs( 2 ), Construct.Name(), TotConstructs );

			if ( SurfaceGHE( Item ).ConstructionNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// Error checking for surfaces, zones, and construction information
			if ( ! Construct( SurfaceGHE( Item ).ConstructionNum ).SourceSinkPresent ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Construction must have internal source/sink and use Construction:InternalSource object" );
				ErrorsFound = true;
			}

			//get inlet node data
			SurfaceGHE( Item ).InletNode = cAlphaArgs( 3 );
			SurfaceGHE( Item ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( SurfaceGHE( Item ).InletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// get outlet node data
			SurfaceGHE( Item ).OutletNode = cAlphaArgs( 4 );
			SurfaceGHE( Item ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( SurfaceGHE( Item ).OutletNodeNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Condenser Water Nodes" );

			// tube data
			SurfaceGHE( Item ).TubeDiameter = rNumericArgs( 1 );
			SurfaceGHE( Item ).TubeCircuits = rNumericArgs( 2 );
			SurfaceGHE( Item ).TubeSpacing = rNumericArgs( 3 );

			if ( rNumericArgs( 2 ) == 0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 3 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 3 ) + '=' + RoundSigDigits( rNumericArgs( 3 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}

			// surface geometry data
			SurfaceGHE( Item ).SurfaceLength = rNumericArgs( 4 );
			SurfaceGHE( Item ).SurfaceWidth = rNumericArgs( 5 );
			if ( rNumericArgs( 4 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + RoundSigDigits( rNumericArgs( 4 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}
			if ( rNumericArgs( 5 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 5 ) + '=' + RoundSigDigits( rNumericArgs( 5 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value must be greater than 0.0" );
				ErrorsFound = true;
			}

			// get lower b.c. type
			if ( SameString( cAlphaArgs( 5 ), "GROUND" ) ) {
				SurfaceGHE( Item ).LowerSurfCond = SurfCond_Ground;
			} else if ( SameString( cAlphaArgs( 5 ), "EXPOSED" ) ) {
				SurfaceGHE( Item ).LowerSurfCond = SurfCond_Exposed;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Only \"Ground\" or \"Exposed\" is allowed." );
				ErrorsFound = true;
			}

		} // end of input loop

		// final error check
		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		// Set up the output variables
		for ( Item = 1; Item <= NumOfSurfaceGHEs; ++Item ) {
			SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate [W]", SurfaceGHEReport( Item ).HeatTransferRate, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Surface Heat Transfer Rate [W]", SurfaceGHEReport( Item ).SurfHeatTransferRate, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Energy [J]", SurfaceGHEReport( Item ).Energy, "Plant", "Sum", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", SurfaceGHEReport( Item ).MassFlowRate, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", SurfaceGHEReport( Item ).InletTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", SurfaceGHEReport( Item ).OutletTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Top Surface Temperature [C]", SurfaceGHEReport( Item ).TopSurfaceTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Bottom Surface Temperature [C]", SurfaceGHEReport( Item ).BtmSurfaceTemp, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Top Surface Heat Transfer Energy per Area [J/m2]", SurfaceGHEReport( Item ).TopSurfaceFlux, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Bottom Surface Heat Transfer Energy per Area [J/m2]", SurfaceGHEReport( Item ).BtmSurfaceFlux, "Plant", "Average", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Surface Heat Transfer Energy [J]", SurfaceGHEReport( Item ).SurfEnergy, "Plant", "Sum", SurfaceGHE( Item ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Source Temperature [C]", SurfaceGHEReport( Item ).SourceTemp, "Plant", "Average", SurfaceGHE( Item ).Name );

		}

		if ( NoSurfaceGroundTempObjWarning ) {
			if ( ! GroundTemp_SurfaceObjInput ) {
				ShowWarningError( "GetSurfaceGroundHeatExchanger: No \"Site:GroundTemperature:Shallow\" were input." );
				ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp_Surface, 1 ) + ") will be used." );
			}
			NoSurfaceGroundTempObjWarning = false;
		}

	}

	//==============================================================================

	void
	InitSurfaceGroundHeatExchanger(
		int const SurfaceGHENum, // component number
		bool const EP_UNUSED( RunFlag ) // TRUE if equipment is operating
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine Resets the elements of the data structure as necessary
		// at the first HVAC iteration of each time step. The weather and QTF data
		// is initialized once only.

		// METHODOLOGY EMPLOYED:
		// Check flags and update data structure

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::BeginEnvrnFlag;
		using namespace DataEnvironment;
		using DataLoopNode::Node;
		using DataHeatBalance::TotConstructs;
		using DataHeatBalance::Construct;
		using DataHeatBalance::Material;
		using InputProcessor::SameString;
		using DataPlant::TypeOf_GrndHtExchgSurface;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using PlantUtilities::RegulateCondenserCompFlowReqOp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DesignVelocity( 0.5 ); // Hypothetical design max pipe velocity [m/s]
		static std::string const RoutineName( "InitSurfaceGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 DesignFlow; // Hypothetical design flow rate
		static bool InitQTF( true ); // one time flag
		static bool MyEnvrnFlag( true );
		int Cons; // construction counter
		int Surface; // Surface number counter
		int LayerNum; // material layer number for bottom
		Real64 OutDryBulb; // Height Dependent dry bulb.
		static Array1D_bool MyFlag;
		static bool MyOneTimeFlag( true );
		int LoopNum;
		int LoopSideNum;
		Real64 rho; // local fluid density
		bool errFlag;

		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumOfSurfaceGHEs );
			MyOneTimeFlag = false;
			MyFlag = true;
		}

		// Init more variables
		if ( MyFlag( SurfaceGHENum ) ) {
			// Locate the hx on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( SurfaceGHE( SurfaceGHENum ).Name, TypeOf_GrndHtExchgSurface, SurfaceGHE( SurfaceGHENum ).LoopNum, SurfaceGHE( SurfaceGHENum ).LoopSideNum, SurfaceGHE( SurfaceGHENum ).BranchNum, SurfaceGHE( SurfaceGHENum ).CompNum, _, _, _, _, _, errFlag );

			if ( errFlag ) {
				ShowFatalError( "InitSurfaceGroundHeatExchanger: Program terminated due to previous condition(s)." );
			}
			rho = GetDensityGlycol( PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidName, constant_zero, PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidIndex, RoutineName );
			SurfaceGHE( SurfaceGHENum ).DesignMassFlowRate = Pi / 4.0 * pow_2( SurfaceGHE( SurfaceGHENum ).TubeDiameter ) * DesignVelocity * rho * SurfaceGHE( SurfaceGHENum ).TubeCircuits;
			InitComponentNodes( 0.0, SurfaceGHE( SurfaceGHENum ).DesignMassFlowRate, SurfaceGHE( SurfaceGHENum ).InletNodeNum, SurfaceGHE( SurfaceGHENum ).OutletNodeNum, SurfaceGHE( SurfaceGHENum ).LoopNum, SurfaceGHE( SurfaceGHENum ).LoopSideNum, SurfaceGHE( SurfaceGHENum ).BranchNum, SurfaceGHE( SurfaceGHENum ).CompNum );
			RegisterPlantCompDesignFlow( SurfaceGHE( SurfaceGHENum ).InletNodeNum, SurfaceGHE( SurfaceGHENum ).DesignMassFlowRate / rho );

			MyFlag( SurfaceGHENum ) = false;
		}

		// get QTF data - only once
		if ( InitQTF ) {
			for ( Surface = 1; Surface <= NumOfSurfaceGHEs; ++Surface ) {
				for ( Cons = 1; Cons <= TotConstructs; ++Cons ) {
					if ( SameString( Construct( Cons ).Name, SurfaceGHE( Surface ).ConstructionName ) ) {
						// some error checking ??
						// CTF stuff
						LayerNum = Construct( Cons ).TotLayers;
						SurfaceGHEQTF( Surface ).NumCTFTerms = Construct( Cons ).NumCTFTerms;
						SurfaceGHEQTF( Surface ).CTFin = Construct( Cons ).CTFInside; // Z coefficents
						SurfaceGHEQTF( Surface ).CTFout = Construct( Cons ).CTFOutside; // X coefficents
						SurfaceGHEQTF( Surface ).CTFcross = Construct( Cons ).CTFCross; // Y coefficents
						SurfaceGHEQTF( Surface ).CTFflux( {1,_} ) = Construct( Cons ).CTFFlux; // F & f coefficents
						// QTF stuff
						SurfaceGHEQTF( Surface ).CTFSourceIn = Construct( Cons ).CTFSourceIn; // Wi coefficents
						SurfaceGHEQTF( Surface ).CTFSourceOut = Construct( Cons ).CTFSourceOut; // Wo coefficents
						SurfaceGHEQTF( Surface ).CTFTSourceOut = Construct( Cons ).CTFTSourceOut; // y coefficents
						SurfaceGHEQTF( Surface ).CTFTSourceIn = Construct( Cons ).CTFTSourceIn; // x coefficents
						SurfaceGHEQTF( Surface ).CTFTSourceQ = Construct( Cons ).CTFTSourceQ; // w coefficents
						SurfaceGHE( Surface ).ConstructionNum = Cons;
						// set the initial history
						//          SurfaceGHEQTF(Surface)%CTFflux(0)    = 0.0D0
						//          SurfaceGHEQTF(Surface)%TbtmHistory    = OutDryBulbTemp
						//          SurfaceGHEQTF(Surface)%TtopHistory   = OutDryBulbTemp
						//          SurfaceGHEQTF(Surface)%TsrcHistory   = OutDryBulbTemp
						//          SurfaceGHEQTF(Surface)%QbtmHistory    = 0.0D0
						//          SurfaceGHEQTF(Surface)%QtopHistory   = 0.0D0
						//          SurfaceGHEQTF(Surface)%QsrcHistory   = 0.0D0
						// surface properties
						SurfaceGHE( Surface ).BtmRoughness = Material( Construct( Cons ).LayerPoint( LayerNum ) ).Roughness;
						SurfaceGHE( Surface ).TopThermAbs = Material( Construct( Cons ).LayerPoint( LayerNum ) ).AbsorpThermal;
						SurfaceGHE( Surface ).TopRoughness = Material( Construct( Cons ).LayerPoint( 1 ) ).Roughness;
						SurfaceGHE( Surface ).TopThermAbs = Material( Construct( Cons ).LayerPoint( 1 ) ).AbsorpThermal;
						SurfaceGHE( Surface ).TopSolarAbs = Material( Construct( Cons ).LayerPoint( 1 ) ).AbsorpSolar;
					}
				}
			}
			// set one-time flag
			InitQTF = false;
		}

		if ( MyEnvrnFlag && BeginEnvrnFlag ) {
			OutDryBulb = OutDryBulbTempAt( SurfaceHXHeight );
			for ( Surface = 1; Surface <= NumOfSurfaceGHEs; ++Surface ) {
				SurfaceGHEQTF( Surface ).CTFflux( 0 ) = 0.0;
				SurfaceGHEQTF( Surface ).TbtmHistory = OutDryBulb;
				SurfaceGHEQTF( Surface ).TtopHistory = OutDryBulb;
				SurfaceGHEQTF( Surface ).TsrcHistory = OutDryBulb;
				SurfaceGHEQTF( Surface ).QbtmHistory = 0.0;
				SurfaceGHEQTF( Surface ).QtopHistory = 0.0;
				SurfaceGHEQTF( Surface ).QsrcHistory = 0.0;
				SurfaceGHEQTF( Surface ).TsrcConstCoef = 0.0;
				SurfaceGHEQTF( Surface ).TsrcVarCoef = 0.0;
				SurfaceGHEQTF( Surface ).QbtmConstCoef = 0.0;
				SurfaceGHEQTF( Surface ).QbtmVarCoef = 0.0;
				SurfaceGHEQTF( Surface ).QtopConstCoef = 0.0;
				SurfaceGHEQTF( Surface ).QtopVarCoef = 0.0;
				SurfaceGHEQTF( Surface ).QSrc = 0.0;
				SurfaceGHEQTF( Surface ).QSrcAvg = 0.0;
				SurfaceGHEQTF( Surface ).LastQSrc = 0.0;
				SurfaceGHEQTF( Surface ).LastSysTimeElapsed = 0.0;
				SurfaceGHEQTF( Surface ).LastTimeStepSys = 0.0;

			}
			// initialize past weather variables
			PastBeamSolarRad = BeamSolarRad;
			PastSolarDirCosVert = SOLCOS( 3 );
			PastDifSolarRad = DifSolarRad;
			PastGroundTemp = GroundTemp_Surface;
			PastIsRain = IsRain;
			PastIsSnow = IsSnow;
			PastOutBaroPress = OutBaroPress;
			PastOutDryBulbTemp = OutDryBulbTempAt( SurfaceHXHeight );
			PastOutHumRat = OutHumRat;
			PastOutAirDensity = OutAirDensity;
			PastOutWetBulbTemp = OutWetBulbTempAt( SurfaceHXHeight );
			PastOutDewPointTemp = OutDewPointTemp;
			PastSkyTemp = SkyTemp;
			PastWindSpeed = WindSpeedAt( SurfaceHXHeight );
			PastCloudFraction = CloudFraction;
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// always initialize - module variables
		InletNodeNum = SurfaceGHE( SurfaceGHENum ).InletNodeNum;
		OutletNodeNum = SurfaceGHE( SurfaceGHENum ).OutletNodeNum;
		ConstructionNum = SurfaceGHE( SurfaceGHENum ).ConstructionNum;
		SurfaceArea = SurfaceGHE( SurfaceGHENum ).SurfaceLength * SurfaceGHE( SurfaceGHENum ).SurfaceWidth;
		InletTemp = Node( InletNodeNum ).Temp;
		OutletTemp = Node( OutletNodeNum ).Temp;
		TopThermAbs = SurfaceGHE( SurfaceGHENum ).TopThermAbs;
		TopRoughness = SurfaceGHE( SurfaceGHENum ).TopRoughness;
		BtmRoughness = SurfaceGHE( SurfaceGHENum ).BtmRoughness;
		BtmThermAbs = SurfaceGHE( SurfaceGHENum ).BtmThermAbs;
		TopSolarAbs = SurfaceGHE( SurfaceGHENum ).TopSolarAbs;
		LoopNum = SurfaceGHE( SurfaceGHENum ).LoopNum;
		LoopSideNum = SurfaceGHE( SurfaceGHENum ).LoopSideNum;

		// If loop operation is controlled by an environmental variable (DBtemp, WBtemp, etc)
		// then shut branch down when equipment is not scheduled to run.
		DesignFlow = RegulateCondenserCompFlowReqOp( SurfaceGHE( SurfaceGHENum ).LoopNum, SurfaceGHE( SurfaceGHENum ).LoopSideNum, SurfaceGHE( SurfaceGHENum ).BranchNum, SurfaceGHE( SurfaceGHENum ).CompNum, SurfaceGHE( SurfaceGHENum ).DesignMassFlowRate );

		SetComponentFlowRate( DesignFlow, SurfaceGHE( SurfaceGHENum ).InletNodeNum, SurfaceGHE( SurfaceGHENum ).OutletNodeNum, SurfaceGHE( SurfaceGHENum ).LoopNum, SurfaceGHE( SurfaceGHENum ).LoopSideNum, SurfaceGHE( SurfaceGHENum ).BranchNum, SurfaceGHE( SurfaceGHENum ).CompNum );

		// get the current flow rate - module variable
		FlowRate = Node( InletNodeNum ).MassFlowRate;

	}

	//==============================================================================

	void
	CalcSurfaceGroundHeatExchanger(
		int const SurfaceGHENum, // component number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a surface ground heat exchanger.  Calls are made to appropriate subroutines
		// either in this module or outside of it.

		// METHODOLOGY EMPLOYED:
		// To update temperature and flux histories it is necessary to make a surface
		// flux/temperature calculation at the begining of each zone time step using the
		// weather data from the previous step, and using the average source flux.
		// Once this has been done a new source flux, and current surface temperatures,
		// are calculated using the current weather data. These surface temperatures and
		// fluxes are used for the rest of the system time steps. During subsequent system
		// time steps only the source flux is updated.

		// Surface fluxes are calculated from the QTF equations using assumed surface
		// temperatures. Surface fluxes are then dependant only on source flux. Constant
		// and terms and terms that multiply the source flux from the QTF equations, are
		// grouped together for convenience. These are calculated in "CalcBottomFluxCoefficents"
		// etc. It is necessary to iterate on these equations, updating the current surface
		// temperatures at each step.

		// REFERENCES:
		// See 'LowTempRadiantSystem' module
		// IBLAST-QTF research program, completed in January 1995 (unreleased)
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.
		// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
		//   of Wisconsin-Madison.

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataGlobals::BeginTimeStepFlag;
		using namespace DataEnvironment;
		using DataPlant::PlantLoop;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// INTEGER, INTENT(IN) :: FlowLock             ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SurfFluxTol( 0.001 ); // tolerance on the surface fluxes
		Real64 const SrcFluxTol( 0.001 ); // tolerance on the source flux
		Real64 const RelaxT( 0.1 ); // temperature relaxation factor
		int const Maxiter( 100 );
		int const Maxiter1( 100 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// variables used with previous environmental conditions
		// not used  REAL(r64)    :: Concentration    ! set to 0.5 if glycol, 0.0 if water
		Real64 PastFluxTop; // top surface flux - past value
		Real64 PastFluxBtm; // bottom surface flux - past value
		Real64 PastTempBtm; // bottom surface temp - past value
		Real64 PastTempTop; // top surface temp - past value
		Real64 OldPastFluxTop; // top surface flux - past value used during iteration
		Real64 OldPastFluxBtm; // bottom surface flux - past value used during iteration
		// variables used with current environmental conditions
		static Real64 FluxTop; // top surface flux
		static Real64 FluxBtm; // bottom surface flux
		static Real64 TempBtm; // bottom surface temp
		static Real64 TempTop; // top surface temp
		Real64 TempT; // top surface temp - used in underrelaxation
		Real64 TempB; // bottom surface temp - used in underrelaxation
		Real64 OldFluxTop; // top surface flux - value used during iteration
		Real64 OldFluxBtm; // bottom surface flux - value used during iteration
		Real64 OldSourceFlux; // previous value of source flux - used during iteration
		int iter;
		int iter1;
		//  INTEGER, SAVE ::ErrCount1=0
		//  INTEGER, SAVE ::ErrCount2=0
		//  INTEGER, SAVE ::ErrCount3=0
		static bool InitializeTempTop( false );
		int LoopNum;
		int LoopSideNum;

		LoopNum = SurfaceGHE( SurfaceGHENum ).LoopNum;
		LoopSideNum = SurfaceGHE( SurfaceGHENum ).LoopSideNum;

		// check if we are in very first call for this zone time step
		if ( BeginTimeStepFlag && FirstHVACIteration && PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) { //DSU
			// calc temps and fluxes with past env. conditions and average source flux
			SourceFlux = SurfaceGHEQTF( SurfaceGHENum ).QSrcAvg;
			// starting values for the surface temps
			PastTempBtm = SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( 1 );
			PastTempTop = SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( 1 );
			OldPastFluxTop = 1.0e+30;
			OldPastFluxBtm = 1.0e+30;
			OldSourceFlux = 1.0e+30;
			TempB = 0.0;
			TempT = 0.0;
			iter = 0;
			while ( true ) { // iterate to find surface heat balances
				// update coefficients

				++iter;
				CalcTopFluxCoefficents( SurfaceGHENum, PastTempBtm, PastTempTop );
				// calc top surface flux
				PastFluxTop = SurfaceGHEQTF( SurfaceGHENum ).QtopConstCoef + SurfaceGHEQTF( SurfaceGHENum ).QtopVarCoef * SourceFlux;

				//calc new top surface temp
				CalcTopSurfTemp( SurfaceGHENum, -PastFluxTop, TempT, PastOutDryBulbTemp, PastOutWetBulbTemp, PastSkyTemp, PastBeamSolarRad, PastDifSolarRad, PastSolarDirCosVert, PastWindSpeed, PastIsRain, PastIsSnow );
				// under relax
				PastTempTop = PastTempTop * ( 1.0 - RelaxT ) + RelaxT * TempT;

				// update coefficients
				CalcBottomFluxCoefficents( SurfaceGHENum, PastTempBtm, PastTempTop );
				PastFluxBtm = SurfaceGHEQTF( SurfaceGHENum ).QbtmConstCoef + SurfaceGHEQTF( SurfaceGHENum ).QbtmVarCoef * SourceFlux;

				if ( std::abs( ( OldPastFluxTop - PastFluxTop ) / OldPastFluxTop ) <= SurfFluxTol && std::abs( ( OldPastFluxBtm - PastFluxBtm ) / OldPastFluxBtm ) <= SurfFluxTol ) break;

				//calc new surface temps
				CalcBottomSurfTemp( SurfaceGHENum, PastFluxBtm, TempB, PastOutDryBulbTemp, PastWindSpeed, PastGroundTemp );
				// underrelax
				PastTempBtm = PastTempBtm * ( 1.0 - RelaxT ) + RelaxT * TempB;
				// update flux record
				OldPastFluxTop = PastFluxTop;
				OldPastFluxBtm = PastFluxBtm;

				//Check for non-convergence
				if ( iter > Maxiter ) {
					if ( SurfaceGHE( SurfaceGHENum ).ConvErrIndex1 == 0 ) {
						ShowWarningMessage( "CalcSurfaceGroundHeatExchanger=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", Did not converge (part 1), Iterations=" + TrimSigDigits( Maxiter ) );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "CalcSurfaceGroundHeatExchanger=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", Did not converge (part 1)", SurfaceGHE( SurfaceGHENum ).ConvErrIndex1 );
					break;
				}
			}

			if ( ! InitializeTempTop ) {
				TempTop = TempT;
				TempBtm = TempB;
				FluxTop = PastFluxTop;
				FluxBtm = PastFluxBtm;
				InitializeTempTop = true;
			}

			// update module variables
			TopSurfTemp = TempTop;
			BtmSurfTemp = TempBtm;
			TopSurfFlux = -FluxTop;
			BtmSurfFlux = FluxBtm;

			// get source temp for output
			CalcSourceTempCoefficents( SurfaceGHENum, PastTempBtm, PastTempTop );
			SourceTemp = SurfaceGHEQTF( SurfaceGHENum ).TsrcConstCoef + SurfaceGHEQTF( SurfaceGHENum ).TsrcVarCoef * SourceFlux;
			// update histories
			UpdateHistories( SurfaceGHENum, PastFluxTop, PastFluxBtm, SourceFlux, SourceTemp );

			// At the beginning of a time step, reset to zero so average calculation can start again
			SurfaceGHEQTF( SurfaceGHENum ).QSrcAvg = 0.0;
			SurfaceGHEQTF( SurfaceGHENum ).LastSysTimeElapsed = 0.0;
			SurfaceGHEQTF( SurfaceGHENum ).LastTimeStepSys = 0.0;

			// get current env. conditions
			PastBeamSolarRad = BeamSolarRad;
			PastSolarDirCosVert = SOLCOS( 3 );
			PastDifSolarRad = DifSolarRad;
			PastGroundTemp = GroundTemp_Surface;
			PastIsRain = IsRain;
			PastIsSnow = IsSnow;
			PastOutBaroPress = OutBaroPress;
			PastOutDryBulbTemp = OutDryBulbTempAt( SurfaceHXHeight );
			PastOutHumRat = OutHumRat;
			PastOutAirDensity = OutAirDensity;
			PastOutWetBulbTemp = OutWetBulbTempAt( SurfaceHXHeight );
			PastOutDewPointTemp = OutDewPointTempAt( SurfaceHXHeight );
			PastSkyTemp = SkyTemp;
			PastWindSpeed = WindSpeedAt( SurfaceHXHeight );
			PastCloudFraction = CloudFraction;

			TempBtm = SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( 1 );
			TempTop = SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( 1 );
			OldFluxTop = 1.0e+30;
			OldFluxBtm = 1.0e+30;
			OldSourceFlux = 1.0e+30;
			SourceFlux = CalcSourceFlux( SurfaceGHENum );
			iter = 0;
			while ( true ) { // iterate to find source flux
				++iter;
				iter1 = 0;
				while ( true ) { // iterate to find surface heat balances
					++iter1;
					// update top coefficients
					CalcTopFluxCoefficents( SurfaceGHENum, TempBtm, TempTop );
					// calc top surface fluxe
					FluxTop = SurfaceGHEQTF( SurfaceGHENum ).QtopConstCoef + SurfaceGHEQTF( SurfaceGHENum ).QtopVarCoef * SourceFlux;
					//calc new surface temps
					CalcTopSurfTemp( SurfaceGHENum, -FluxTop, TempT, PastOutDryBulbTemp, PastOutWetBulbTemp, PastSkyTemp, PastBeamSolarRad, PastDifSolarRad, PastSolarDirCosVert, PastWindSpeed, PastIsRain, PastIsSnow );
					// under-relax
					TempTop = TempTop * ( 1.0 - RelaxT ) + RelaxT * TempT;
					// update bottom coefficients
					CalcBottomFluxCoefficents( SurfaceGHENum, TempBtm, TempTop );
					FluxBtm = SurfaceGHEQTF( SurfaceGHENum ).QbtmConstCoef + SurfaceGHEQTF( SurfaceGHENum ).QbtmVarCoef * SourceFlux;
					// convergence test on surface fluxes
					if ( std::abs( ( OldFluxTop - FluxTop ) / OldFluxTop ) <= SurfFluxTol && std::abs( ( OldFluxBtm - FluxBtm ) / OldFluxBtm ) <= SurfFluxTol ) break;

					//calc new surface temps
					CalcBottomSurfTemp( SurfaceGHENum, FluxBtm, TempB, PastOutDryBulbTemp, PastOutDryBulbTemp, GroundTemp_Surface );
					// under-relax
					TempBtm = TempBtm * ( 1.0 - RelaxT ) + RelaxT * TempB;
					// update flux record
					OldFluxBtm = FluxBtm;
					OldFluxTop = FluxTop;

					//Check for non-convergence
					if ( iter1 > Maxiter1 ) {
						if ( SurfaceGHE( SurfaceGHENum ).ConvErrIndex2 == 0 ) {
							ShowWarningMessage( "CalcSurfaceGroundHeatExchanger=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", Did not converge (part 2), Iterations=" + TrimSigDigits( Maxiter ) );
							ShowContinueErrorTimeStamp( "" );
						}
						ShowRecurringWarningErrorAtEnd( "CalcSurfaceGroundHeatExchanger=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", Did not converge (part 2)", SurfaceGHE( SurfaceGHENum ).ConvErrIndex2 );
						break;
					}
				}
				// update the source temp coefficients and update the source flux
				CalcSourceTempCoefficents( SurfaceGHENum, TempBtm, TempTop );
				SourceFlux = CalcSourceFlux( SurfaceGHENum );
				// check source flux convergence
				if ( std::abs( ( OldSourceFlux - SourceFlux ) / ( 1.0e-20 + OldSourceFlux ) ) <= SrcFluxTol ) break;
				OldSourceFlux = SourceFlux;

				//Check for non-convergence
				if ( iter > Maxiter ) {
					if ( SurfaceGHE( SurfaceGHENum ).ConvErrIndex3 == 0 ) {
						ShowWarningMessage( "CalcSurfaceGroundHeatExchanger=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", Did not converge (part 3), Iterations=" + TrimSigDigits( Maxiter ) );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "CalcSurfaceGroundHeatExchanger=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", Did not converge (part 3)", SurfaceGHE( SurfaceGHENum ).ConvErrIndex3 );
					break;
				}
			} // end surface heat balance iteration

		} else { // end source flux iteration

			// For the rest of the system time steps ...
			// update source flux from Twi
			SourceFlux = CalcSourceFlux( SurfaceGHENum );

		}

	}

	//==============================================================================

	void
	CalcBottomFluxCoefficents(
		int const SurfaceGHENum, // component number
		Real64 const Tbottom, // current bottom (lower) surface temperature
		Real64 const Ttop // current top (upper) surface temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates current version of constant variable parts of QTF equations.

		// METHODOLOGY EMPLOYED:
		// For given current surface temperatures the terms of the QTF equations can be
		// grouped into constant terms, and those depending on the current source flux.
		// This routine calculates the current coefficient values for the bottom flux
		// equation.

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Term;

		// add current surface temperatures to history data
		SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( 0 ) = Tbottom;
		SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( 0 ) = Ttop;

		// Bottom Surface Coefficients
		SurfaceGHEQTF( SurfaceGHENum ).QbtmConstCoef = 0.0;
		for ( Term = 0; Term <= SurfaceGHEQTF( SurfaceGHENum ).NumCTFTerms - 1; ++Term ) {

			SurfaceGHEQTF( SurfaceGHENum ).QbtmConstCoef += ( -SurfaceGHEQTF( SurfaceGHENum ).CTFin( Term ) * SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFcross( Term ) * SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFflux( Term ) * SurfaceGHEQTF( SurfaceGHENum ).QbtmHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFSourceIn( Term ) * SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory( Term ) );

		}

		//     SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef =  SUM(-SurfaceGHEQTF(SurfaceGHENum)%CTFin * &
		//                                                  SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory + &
		//                                                  SurfaceGHEQTF(SurfaceGHENum)%CTFcross * &
		//                                                  SurfaceGHEQTF(SurfaceGHENum)%TtopHistory + &
		//                                                  SurfaceGHEQTF(SurfaceGHENum)%CTFflux * &
		//                                                  SurfaceGHEQTF(SurfaceGHENum)%QbtmHistory + &
		//                                                  SurfaceGHEQTF(SurfaceGHENum)%CTFSourceIn * &
		//                                                  SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory)
		// correct for extra bottom surface flux term
		SurfaceGHEQTF( SurfaceGHENum ).QbtmConstCoef -= SurfaceGHEQTF( SurfaceGHENum ).CTFSourceIn( 0 ) * SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory( 0 );
		// source flux current coefficient
		SurfaceGHEQTF( SurfaceGHENum ).QbtmVarCoef = SurfaceGHEQTF( SurfaceGHENum ).CTFSourceIn( 0 );

	}

	//==============================================================================

	void
	CalcTopFluxCoefficents(
		int const SurfaceGHENum, // component number
		Real64 const Tbottom, // current bottom (lower) surface temperature
		Real64 const Ttop // current top (upper) surface temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates current version of constant variable parts of QTF equations.

		// METHODOLOGY EMPLOYED:
		// For given current surface temperatures the terms of the QTF equations can be
		// grouped into constant terms, and those depending on the current source flux.
		// This routine calculates the current coefficient values for the top flux
		// equation.

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// add current surface temperatures to history data
		SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( 0 ) = Tbottom;
		SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( 0 ) = Ttop;

		// Top Surface Coefficients
		SurfaceGHEQTF( SurfaceGHENum ).QtopConstCoef = 0.0;
		for ( int Term = 0; Term <= SurfaceGHEQTF( SurfaceGHENum ).NumCTFTerms - 1; ++Term ) {

			SurfaceGHEQTF( SurfaceGHENum ).QtopConstCoef += ( SurfaceGHEQTF( SurfaceGHENum ).CTFout( Term ) * SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( Term ) ) - ( SurfaceGHEQTF( SurfaceGHENum ).CTFcross( Term ) * SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFflux( Term ) * SurfaceGHEQTF( SurfaceGHENum ).QtopHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFSourceOut( Term ) * SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory( Term ) );

		}

		//     ! Top Surface Coefficients
		//     SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef = SUM(SurfaceGHEQTF(SurfaceGHENum)%CTFout * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%TtopHistory - &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%CTFcross * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory + &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%CTFflux * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%QtopHistory + &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%CTFSourceOut * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory)

		// correct for extra top surface flux term
		SurfaceGHEQTF( SurfaceGHENum ).QtopConstCoef -= ( SurfaceGHEQTF( SurfaceGHENum ).CTFSourceOut( 0 ) * SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory( 0 ) );
		// surface flux current coefficient
		SurfaceGHEQTF( SurfaceGHENum ).QtopVarCoef = SurfaceGHEQTF( SurfaceGHENum ).CTFSourceOut( 0 );

	}

	//==============================================================================

	void
	CalcSourceTempCoefficents(
		int const SurfaceGHENum, // component number
		Real64 const Tbottom, // current bottom (lower) surface temperature
		Real64 const Ttop // current top (upper) surface temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates current version of constant variable parts of QTF equations.

		// METHODOLOGY EMPLOYED:
		// For given current surface temperatures the terms of the QTF equations can be
		// grouped into constant terms, and those depending on the current source flux.
		// This routine calculates the current coefficient values for the source temperature
		// equation.

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Term;

		// add current surface temperatures to history data
		SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( 0 ) = Tbottom;
		SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( 0 ) = Ttop;

		SurfaceGHEQTF( SurfaceGHENum ).TsrcConstCoef = 0.0;
		for ( Term = 0; Term <= SurfaceGHEQTF( SurfaceGHENum ).NumCTFTerms - 1; ++Term ) {

			SurfaceGHEQTF( SurfaceGHENum ).TsrcConstCoef += ( SurfaceGHEQTF( SurfaceGHENum ).CTFTSourceIn( Term ) * SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFTSourceOut( Term ) * SurfaceGHEQTF( SurfaceGHENum ).TtopHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFflux( Term ) * SurfaceGHEQTF( SurfaceGHENum ).TsrcHistory( Term ) ) + ( SurfaceGHEQTF( SurfaceGHENum ).CTFTSourceQ( Term ) * SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory( Term ) );

		}

		// Source Temperature terms
		//     SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef = SUM(SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceIn * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory + &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceOut * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%TtopHistory + &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%CTFflux * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%TsrcHistory + &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceQ * &
		//                                              SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory)
		// correct for extra source flux term
		SurfaceGHEQTF( SurfaceGHENum ).TsrcConstCoef -= SurfaceGHEQTF( SurfaceGHENum ).CTFTSourceQ( 0 ) * SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory( 0 );
		// source flux current coefficient
		SurfaceGHEQTF( SurfaceGHENum ).TsrcVarCoef = SurfaceGHEQTF( SurfaceGHENum ).CTFTSourceQ( 0 );

	}

	//==============================================================================

	Real64
	CalcSourceFlux( int const SurfaceGHENum ) // component number
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This calculates the source flux given the inlet fluid temperature. A
		// heat exchanger analogy is used, with the surface as a 'Fixed' fluid.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// USE STATEMENTS:

		// Return value
		Real64 CalcSourceFlux;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 EpsMdotCp; // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat

		// Effectiveness * Modot * specific heat
		if ( FlowRate > 0.0 ) {
			EpsMdotCp = CalcHXEffectTerm( SurfaceGHENum, InletTemp, FlowRate );
			// calc flux
			CalcSourceFlux = ( InletTemp - SurfaceGHEQTF( SurfaceGHENum ).TsrcConstCoef ) / ( SurfaceArea / EpsMdotCp + SurfaceGHEQTF( SurfaceGHENum ).TsrcVarCoef );
		} else {
			EpsMdotCp = 0.0;
			CalcSourceFlux = 0.0;
		}

		return CalcSourceFlux;
	}

	//==============================================================================

	void
	UpdateHistories(
		int const SurfaceGHENum, // component number
		Real64 const TopFlux, // current top (top) surface flux
		Real64 const BottomFlux, // current bottom (bottom) surface flux
		Real64 const SourceFlux, // current source surface flux
		Real64 const SourceTemp // current source temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is used to update the temperature and flux records for the QTF
		// calculations. This is called at the start of each zone timestep.

		// METHODOLOGY EMPLOYED:
		// Just shift along and replace zero index element with current value.

		// REFERENCES:
		// n/a

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// update top surface temps
		SurfaceGHEQTF( SurfaceGHENum ).TtopHistory = eoshift( SurfaceGHEQTF( SurfaceGHENum ).TtopHistory, -1 );

		// update bottom surface temps
		SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory = eoshift( SurfaceGHEQTF( SurfaceGHENum ).TbtmHistory, -1 );

		// update bottom surface temps
		SurfaceGHEQTF( SurfaceGHENum ).TsrcHistory = eoshift( SurfaceGHEQTF( SurfaceGHENum ).TsrcHistory, -1 );
		SurfaceGHEQTF( SurfaceGHENum ).TsrcHistory( 1 ) = SourceTemp;

		// update bottom surface fluxes
		SurfaceGHEQTF( SurfaceGHENum ).QbtmHistory = eoshift( SurfaceGHEQTF( SurfaceGHENum ).QbtmHistory, -1 );
		SurfaceGHEQTF( SurfaceGHENum ).QbtmHistory( 1 ) = BottomFlux;

		// update bottom surface fluxes
		SurfaceGHEQTF( SurfaceGHENum ).QtopHistory = eoshift( SurfaceGHEQTF( SurfaceGHENum ).QtopHistory, -1 );
		SurfaceGHEQTF( SurfaceGHENum ).QtopHistory( 1 ) = TopFlux;

		// update bottom surface fluxes
		SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory = eoshift( SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory, -1 );
		SurfaceGHEQTF( SurfaceGHENum ).QsrcHistory( 1 ) = SourceFlux;

	}

	//==============================================================================

	Real64
	CalcHXEffectTerm(
		int const SurfaceGHENum, // Index number of surface under consideration
		Real64 const Temperature, // Temperature of water entering the surface, in C
		Real64 const WaterMassFlow // Mass flow rate, in kg/s
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   December 2000
		//       MODIFIED       Simon Rees, August 2002
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the "heat exchanger"
		// effectiveness term.  This is equal to the mass flow rate of water
		// times the specific heat of water times the effectiveness of
		// the surface heat exchanger. This routine is adapted from that in
		// the low temp radiant surface model.

		// METHODOLOGY EMPLOYED:
		// Assumes that the only REAL(r64) heat transfer term that we have to
		// deal with is the convection from the water to the tube.  The
		// other assumptions are that the tube bottom surface temperature
		// is equal to the "source location temperature" and that it is
		// a CONSTANT throughout the surface.

		// REFERENCES:
		// See RadiantSystemLowTemp module.
		// Property data for water shown below as parameters taken from
		//   Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
		// Heat exchanger information also from Incropera and DeWitt.
		// Code based loosely on code from IBLAST program (research version)

		// Using/Aliasing
		using DataGlobals::Pi;
		using General::RoundSigDigits;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Return value
		Real64 CalcHXEffectTerm;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxLaminarRe( 2300.0 ); // Maximum Reynolds number for laminar flow
		int const NumOfPropDivisions( 13 ); // intervals in property correlation
		static Array1D< Real64 > const Temps( NumOfPropDivisions, { 1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85 } ); // Temperature, in C
		static Array1D< Real64 > const Mu( NumOfPropDivisions, { 0.001652, 0.001422, 0.001225, 0.00108, 0.000959, 0.000855, 0.000769, 0.000695, 0.000631, 0.000577, 0.000528, 0.000489, 0.000453 } ); // Viscosity, in Ns/m2
		static Array1D< Real64 > const Conductivity( NumOfPropDivisions, { 0.574, 0.582, 0.590, 0.598, 0.606, 0.613, 0.620, 0.628, 0.634, 0.640, 0.645, 0.650, 0.656 } ); // Conductivity, in W/mK
		static Array1D< Real64 > const Pr( NumOfPropDivisions, { 12.22, 10.26, 8.81, 7.56, 6.62, 5.83, 5.20, 4.62, 4.16, 3.77, 3.42, 3.15, 2.88 } ); // Prandtl number (dimensionless)
		int const WaterIndex( 1 );
		static std::string const RoutineName( "SurfaceGroundHeatExchanger:CalcHXEffectTerm" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Index;
		Real64 InterpFrac;
		Real64 NuD;
		Real64 ReD;
		Real64 NTU;
		Real64 CpWater;
		Real64 Kactual;
		Real64 MUactual;
		Real64 PRactual;
		Real64 PipeLength;
		//  INTEGER, SAVE                             :: ErrCount

		// FLOW:
		// First find out where we are in the range of temperatures
		Index = 1;
		while ( Index <= NumOfPropDivisions ) {
			if ( Temperature < Temps( Index ) ) break; // DO loop
			++Index;
		}

		// Initialize thermal properties of water
		if ( Index == 1 ) {
			MUactual = Mu( Index );
			Kactual = Conductivity( Index );
			PRactual = Pr( Index );
		} else if ( Index > NumOfPropDivisions ) {
			Index = NumOfPropDivisions;
			MUactual = Mu( Index );
			Kactual = Conductivity( Index );
			PRactual = Pr( Index );
		} else {
			InterpFrac = ( Temperature - Temps( Index - 1 ) ) / ( Temps( Index ) - Temps( Index - 1 ) );
			MUactual = Mu( Index - 1 ) + InterpFrac * ( Mu( Index ) - Mu( Index - 1 ) );
			Kactual = Conductivity( Index - 1 ) + InterpFrac * ( Conductivity( Index ) - Conductivity( Index - 1 ) );
			PRactual = Pr( Index - 1 ) + InterpFrac * ( Pr( Index ) - Pr( Index - 1 ) );
		}
		// arguments are glycol name, temperature, and concentration
		if ( Temperature < 0.0 ) { // check if fluid is water and would be freezing
			if ( PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidIndex == WaterIndex ) {
				if ( SurfaceGHE( SurfaceGHENum ).FrozenErrIndex1 == 0 ) {
					ShowWarningMessage( "GroundHeatExchanger:Surface=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", water is frozen; Model not valid. Calculated Water Temperature=[" + RoundSigDigits( InletTemp, 2 ) + "] C" );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( "GroundHeatExchanger:Surface=\"" + SurfaceGHE( SurfaceGHENum ).Name + "\", water is frozen", SurfaceGHE( SurfaceGHENum ).FrozenErrIndex1, InletTemp, InletTemp, _, "[C]", "[C]" );
				InletTemp = max( InletTemp, 0.0 );
			}
		}
		CpWater = GetSpecificHeatGlycol( PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidName, Temperature, PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidIndex, RoutineName );

		// Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
		ReD = 4.0 * WaterMassFlow / ( Pi * MUactual * SurfaceGHE( SurfaceGHENum ).TubeDiameter * SurfaceGHE( SurfaceGHENum ).TubeCircuits );

		// Calculate the Nusselt number based on what flow regime one is in
		if ( ReD >= MaxLaminarRe ) { // Turbulent flow --> use Colburn equation
			NuD = 0.023 * std::pow( ReD, 0.8 ) * std::pow( PRactual, 1.0 / 3.0 );
		} else { // Laminar flow --> use constant surface temperature relation
			NuD = 3.66;
		}
		// Calculate the NTU parameter
		// NTU = UA/[(Mdot*Cp)min]
		// where: U = h (convection coefficient) and h = (k)(Nu)/D
		//        A = Pi*D*TubeLength
		//  NTU = PI * Kactual * NuD * SurfaceGHE(SurfaceGHENum)%TubeLength / (WaterMassFlow * CpWater)

		PipeLength = SurfaceGHE( SurfaceGHENum ).SurfaceLength * SurfaceGHE( SurfaceGHENum ).SurfaceWidth / SurfaceGHE( SurfaceGHENum ).TubeSpacing;

		NTU = Pi * Kactual * NuD * PipeLength / ( WaterMassFlow * CpWater );
		// Calculate Epsilon*MassFlowRate*Cp
		if ( -NTU >= EXP_LowerLimit ) {
			CalcHXEffectTerm = ( 1.0 - std::exp( -NTU ) ) * WaterMassFlow * CpWater;
		} else {
			CalcHXEffectTerm = 1.0 * WaterMassFlow * CpWater;
		}

		return CalcHXEffectTerm;

	}

	//==============================================================================

	void
	CalcTopSurfTemp(
		int const SurfaceNum, // surface index number
		Real64 const FluxTop, // top surface flux
		Real64 & TempTop, // top surface temperature
		Real64 const ThisDryBulb, // dry bulb temperature
		Real64 const ThisWetBulb, // wet bulb temperature
		Real64 const ThisSkyTemp, // sky temperature
		Real64 const ThisBeamSolarRad, // beam solar radiation
		Real64 const ThisDifSolarRad, // diffuse solar radiation
		Real64 const ThisSolarDirCosVert, // vertical component of solar normal
		Real64 const ThisWindSpeed, // wind speed
		bool const ThisIsRain, // rain flag
		bool const ThisIsSnow // snow flag
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is used to calculate the top surface
		// temperature for the given surface flux.

		// METHODOLOGY EMPLOYED:
		// calc surface heat balance

		// REFERENCES:
		// USE STATEMENTS:

		// Using/Aliasing
		using ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// n/a
		// INTERFACE BLOCK SPECIFICATIONS
		// n/a

		// DERIVED TYPE DEFINITIONS
		// n/a

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ConvCoef; // convection coefficient
		Real64 RadCoef; // radiation coefficient
		Real64 ExternalTemp; // external environmental temp - drybulb or wetbulb
		Real64 OldSurfTemp; // previous surface temperature
		Real64 QSolAbsorbed; // absorbed solar flux
		Real64 SurfTempAbs; // absolute value of surface temp
		Real64 SkyTempAbs; // absolute value of sky temp

		// make a surface heat balance and solve for temperature

		// set appropriate external temp
		if ( ThisIsSnow ) {
			ExternalTemp = ThisWetBulb;
		} else if ( ThisIsRain ) {
			ExternalTemp = ThisWetBulb;
		} else { // normal dry conditions
			ExternalTemp = ThisDryBulb;
		}

		// set previous surface temp
		OldSurfTemp = SurfaceGHEQTF( SurfaceNum ).TtopHistory( 1 );
		// absolute temperatures
		SurfTempAbs = OldSurfTemp + KelvinConv;
		SkyTempAbs = ThisSkyTemp + KelvinConv;

		// ASHRAE simple convection coefficient model for external surfaces.
		ConvCoef = CalcASHRAESimpExtConvectCoeff( TopRoughness, ThisWindSpeed );
		// radiation coefficient using surf temp from past time step
		if ( std::abs( SurfTempAbs - SkyTempAbs ) > SmallNum ) {
			RadCoef = StefBoltzmann * TopThermAbs * ( pow_4( SurfTempAbs ) - pow_4( SkyTempAbs ) ) / ( SurfTempAbs - SkyTempAbs );
		} else {
			RadCoef = 0.0;
		}

		// total absorbed solar - no ground solar
		QSolAbsorbed = TopSolarAbs * ( max( ThisSolarDirCosVert, 0.0 ) * ThisBeamSolarRad + ThisDifSolarRad );

		// solve for temperature
		TempTop = ( FluxTop + ConvCoef * ExternalTemp + RadCoef * ThisSkyTemp + QSolAbsorbed ) / ( ConvCoef + RadCoef );

	}

	//==============================================================================

	void
	CalcBottomSurfTemp(
		int const SurfaceNum, // surface index number
		Real64 const FluxBtm, // bottom surface flux
		Real64 & TempBtm, // bottom surface temperature
		Real64 const ThisDryBulb, // dry bulb temperature
		Real64 const ThisWindSpeed, // wind speed
		Real64 const ThisGroundTemp // ground temperature
	)
	{

		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is used to calculate the bottom surface
		// temperature for the given surface flux.

		// METHODOLOGY EMPLOYED:
		// calc surface heat balances

		// REFERENCES:
		// USE STATEMENTS:

		// Using/Aliasing
		using ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 ConvCoef; // convection coefficient
		Real64 RadCoef; // radiation coefficient
		Real64 OldSurfTemp; // previous surface temperature
		Real64 SurfTempAbs; // absolute value of surface temp
		Real64 ExtTempAbs; // absolute value of sky temp

		if ( SurfaceGHE( SurfaceNum ).LowerSurfCond == SurfCond_Exposed ) {

			// make a surface heat balance and solve for temperature
			OldSurfTemp = SurfaceGHEQTF( SurfaceNum ).TbtmHistory( 1 );
			// absolute temperatures
			SurfTempAbs = OldSurfTemp + KelvinConv;
			ExtTempAbs = ThisDryBulb + KelvinConv;

			// ASHRAE simple convection coefficient model for external surfaces.
			ConvCoef = CalcASHRAESimpExtConvectCoeff( TopRoughness, ThisWindSpeed );

			// radiation coefficient using surf temp from past time step
			if ( std::abs( SurfTempAbs - ExtTempAbs ) > SmallNum ) {
				RadCoef = StefBoltzmann * TopThermAbs * ( pow_4( SurfTempAbs ) - pow_4( ExtTempAbs ) ) / ( SurfTempAbs - ExtTempAbs );
			} else {
				RadCoef = 0.0;
			}

			// total absorbed solar - no ground solar
			TempBtm = ( FluxBtm + ConvCoef * ThisDryBulb + RadCoef * ThisDryBulb ) / ( ConvCoef + RadCoef );

		} else { // ground coupled
			// just use the supplied ground temperature
			TempBtm = ThisGroundTemp;
		}

	}

	//==============================================================================

	void
	UpdateSurfaceGroundHeatExchngr( int const SurfaceGHENum ) // Index for the surface
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for surface
		// ground heat exchangers.  One of the most important functions of
		// this routine is to update the average heat source/sink for a
		// particular system over the various system time steps that make up
		// the zone time step. This routine must also set the outlet water conditions.

		// METHODOLOGY EMPLOYED:
		// For the source/sink average update, if the system time step elapsed
		// is still what it used to be, then either we are still iterating or
		// we had to go back and shorten the time step.  As a result, we have
		// to subtract out the previous value that we added.  If the system
		// time step elapsed is different, then we just need to add the new
		// values to the running average.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataLoopNode::Node;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SurfaceGroundHeatExchanger:Update" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpFluid; // Specific heat of working fluid
		//  INTEGER,SAVE    :: ErrCount
		int LoopNum;
		int LoopSideNum;

		// update flux
		SurfaceGHEQTF( SurfaceGHENum ).QSrc = SourceFlux;

		LoopNum = SurfaceGHE( SurfaceGHENum ).LoopNum;
		LoopSideNum = SurfaceGHE( SurfaceGHENum ).LoopSideNum;
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock > 0 ) { // only update in normal mode !DSU
			if ( SurfaceGHEQTF( SurfaceGHENum ).LastSysTimeElapsed == SysTimeElapsed ) {
				// Still iterating or reducing system time step, so subtract old values which were
				// not valid
				SurfaceGHEQTF( SurfaceGHENum ).QSrcAvg -= SurfaceGHEQTF( SurfaceGHENum ).LastQSrc * SurfaceGHEQTF( SurfaceGHENum ).LastTimeStepSys / TimeStepZone;
			}

			// Update the running average and the "last" values with the current values of the appropriate variables
			SurfaceGHEQTF( SurfaceGHENum ).QSrcAvg += SurfaceGHEQTF( SurfaceGHENum ).QSrc * TimeStepSys / TimeStepZone;

			SurfaceGHEQTF( SurfaceGHENum ).LastQSrc = SourceFlux;
			SurfaceGHEQTF( SurfaceGHENum ).LastSysTimeElapsed = SysTimeElapsed;
			SurfaceGHEQTF( SurfaceGHENum ).LastTimeStepSys = TimeStepSys;

		}

		// Calculate the water side outlet conditions and set the
		// appropriate conditions on the correct HVAC node.
		if ( PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidName == "WATER" ) {
			if ( InletTemp < 0.0 ) {
				ShowRecurringWarningErrorAtEnd( "UpdateSurfaceGroundHeatExchngr: Water is frozen in Surf HX=" + SurfaceGHE( SurfaceGHENum ).Name, SurfaceGHE( SurfaceGHENum ).FrozenErrIndex2, InletTemp, InletTemp );
			}
			InletTemp = max( InletTemp, 0.0 );
		}

		CpFluid = GetSpecificHeatGlycol( PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidName, InletTemp, PlantLoop( SurfaceGHE( SurfaceGHENum ).LoopNum ).FluidIndex, RoutineName );

		SafeCopyPlantNode( SurfaceGHE( SurfaceGHENum ).InletNodeNum, SurfaceGHE( SurfaceGHENum ).OutletNodeNum );
		// check for flow
		if ( ( CpFluid > 0.0 ) && ( FlowRate > 0.0 ) ) {
			Node( SurfaceGHE( SurfaceGHENum ).OutletNodeNum ).Temp = InletTemp - SurfaceArea * SourceFlux / ( FlowRate * CpFluid );
			Node( SurfaceGHE( SurfaceGHENum ).OutletNodeNum ).Enthalpy = Node( SurfaceGHE( SurfaceGHENum ).OutletNodeNum ).Temp * CpFluid;
		}

	}

	//==============================================================================

	void
	ReportSurfaceGroundHeatExchngr( int const SurfaceGHENum ) // Index for the surface under consideration
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Rees
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for Surface ground heat exchangers

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

		// update flows and temps from node data
		SurfaceGHEReport( SurfaceGHENum ).InletTemp = Node( SurfaceGHE( SurfaceGHENum ).InletNodeNum ).Temp;
		SurfaceGHEReport( SurfaceGHENum ).OutletTemp = Node( SurfaceGHE( SurfaceGHENum ).OutletNodeNum ).Temp;
		SurfaceGHEReport( SurfaceGHENum ).MassFlowRate = Node( SurfaceGHE( SurfaceGHENum ).InletNodeNum ).MassFlowRate;

		// update other variables from module variables
		SurfaceGHEReport( SurfaceGHENum ).HeatTransferRate = SourceFlux * SurfaceArea;
		SurfaceGHEReport( SurfaceGHENum ).SurfHeatTransferRate = SurfaceArea * ( TopSurfFlux + BtmSurfFlux );
		SurfaceGHEReport( SurfaceGHENum ).Energy = SourceFlux * SurfaceArea * TimeStepSys * SecInHour;
		SurfaceGHEReport( SurfaceGHENum ).TopSurfaceTemp = TopSurfTemp;
		SurfaceGHEReport( SurfaceGHENum ).BtmSurfaceTemp = BtmSurfTemp;
		SurfaceGHEReport( SurfaceGHENum ).TopSurfaceFlux = TopSurfFlux;
		SurfaceGHEReport( SurfaceGHENum ).BtmSurfaceFlux = BtmSurfFlux;
		SurfaceGHEReport( SurfaceGHENum ).SurfEnergy = SurfaceArea * ( TopSurfFlux + BtmSurfFlux ) * TimeStepSys * SecInHour;
		SurfaceGHEReport( SurfaceGHENum ).SourceTemp = SourceTemp;

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

} // SurfaceGroundHeatExchanger

} // EnergyPlus
