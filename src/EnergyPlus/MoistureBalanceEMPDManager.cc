// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <MoistureBalanceEMPDManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataIPShortCuts.hh>
#include <DataMoistureBalanceEMPD.hh>
#include <DataMoistureBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace MoistureBalanceEMPDManager {

	// Module containing the routines to calculate moisture adsorption and desorption
	// at interior wall surfaces

	// MODULE INFORMATION:
	//   Authors:        Muthusamy Swami and Lixing Gu
	//   Date written:   August, 1999
	//   Modified:       na
	//   Re-engineered:  Jason Woods and Noel Merket, August 2015

	// PURPOSE OF THIS MODULE:
	// To calculate moisture adsorption and desorption at interior wall surfaces
	// using EMPD model (Effective Moisture Penetration Depth) developed by
	// Florida Solar Energy Center. Input consists of interior surface temperatures
	// and sorption curve of interior layer materials. Output consists of mositure
	// fluxes from wall interior surfaces, which will be used in zone moisture balance.

	// METHODOLOGY EMPLOYED:
	// Add something
	// EMPD is a simplified method of analysing moisture transport in buildings and
	// is easy to incorporate into existing building energy analysis computer codes.
	// The components of the moisture balance equation involving moisture adsorption
	// and desorption are described in detail where the concept of EMPD is discussed.
	// The assumptions. parameters required, and limitations of the model are also discussed.
	// Results of simulation using the model and comparison with measured data are given.
	// Data of isotherms compiled from the literature of some commonly used building materials are also given.

	// REFERENCES:
	// Kerestecioglu A A., Swami M V., Kamel A A., "Theoretical and computational
	// investigation of simultaneous heat and moisture transfer in buildings: 'Effective
	// penetration depth' theory," ASHRAE Trans., 1990, Vol. 96, Part 1, 447-454

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data used in the module
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataEnvironment::OutBaroPress;
	using namespace DataHeatBalance;
	using namespace DataGlobals;
	using DataHeatBalFanSys::ZoneAirHumRat;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::Surface;
	using DataSurfaces::SurfaceClass_Window;
	using namespace DataMoistureBalanceEMPD;
	using DataMoistureBalance::HMassConvInFD;
	using DataMoistureBalance::HConvInFD;
	using DataMoistureBalance::RhoVaporAirIn;

	// Data
	// MODULE VARIABLE and Function DECLARATIONs
	Array1D< Real64 > RhoVapEMPD; // Inside Surface Vapor Density Reporting variable
	Array1D< Real64 > WSurfEMPD; // Inside Surface Humidity Ratio Reporting variable
	Array1D< Real64 > RHEMPD; // Inside Surface Relative Humidity Reporting variable

	// SUBROUTINE SPECIFICATION FOR MODULE MoistureBalanceEMPDManager

	//******************************************************************************

	// Functions

	void
	GetMoistureBalanceEMPDInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Muthusamy V. Swami and Lixing Gu
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver for initializations within the
		// heat balance using the EMPD model.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using DataSurfaces::HeatTransferModel_EMPD;

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
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string MaterialNames( 3 ); // Number of Material Alpha names defined
		int MaterNum; // Counter to keep track of the material number
		int MaterialNumAlpha; // Number of material alpha names being passed
		int MaterialNumProp; // Number of material properties being passed
		Array1D< Real64 > MaterialProps( 9 ); // Temporary array to transfer material properties
		static bool ErrorsFound( false ); // If errors detected in input

		int EMPDMat; // EMPD Moisture Material additional properties for each base material
		int Loop;
		int Layer;
		int SurfNum; // Surface number
		int MatNum; // Material number at interior layer
		int ConstrNum; // Construction number
		Array1D_bool EMPDzone; // EMPD property check for each zone
		static int ErrCount( 0 );

		// Load the additional EMPD Material properties
		cCurrentModuleObject = "MaterialProperty:MoisturePenetrationDepth:Settings";
		EMPDMat = GetNumObjectsFound( cCurrentModuleObject );

		if ( EMPDMat == 0 ) {
			ShowSevereError( "EMPD Solution requested, but no \"" + cCurrentModuleObject + "\" objects were found." );
			ErrorsFound = true;
		}

		for ( Loop = 1; Loop <= EMPDMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( cCurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			//Load the material derived type from the input data.
			MaterNum = FindItemInList( MaterialNames( 1 ), Material );
			if ( MaterNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + " entered=" + MaterialNames( 1 ) + ", must match to a valid Material name." );
				ErrorsFound = true;
				continue;
			}

			// See if Material was defined with R only.  (No density is defined then and not applicable for EMPD).
			//  What about materials other than "regular materials" (e.g. Glass, Air, etc)
			if ( Material( MaterNum ).Group == RegularMaterial && MaterialProps( 1 ) > 0.0 ) {
				if ( Material( MaterNum ).ROnly ) {
					//        CALL ShowSevereError('EMPD base material = "'//TRIM(Material(MaterNum)%Name)//  &
					//                             '" was Material:NoMass. It cannot be used for EMPD calculations.')
					ShowContinueError( "..Only Material base materials are allowed to have EMPD properties." );
					ShowSevereError( cCurrentModuleObject + ": Reference Material is not appropriate type for EMPD properties, material=" + Material( MaterNum ).Name + ", must have regular properties (L,Cp,K,D)" );
					ErrorsFound = true;
				}
			}
			if ( Material( MaterNum ).Group != RegularMaterial ) {
				//      CALL ShowSevereError('GetMoistureBalanceEMPDInput: Only Material:Regular base materials are allowed '// &
				//                           'to have EMPD properties, material = '// TRIM(Material(MaterNum)%Name))
				ShowSevereError( cCurrentModuleObject + ": Reference Material is not appropriate type for EMPD properties, material=" + Material( MaterNum ).Name + ", must have regular properties (L,Cp,K,D)" );
				ErrorsFound = true;
			}

			// Once the material derived type number is found then load the additional moisture material properties
			Material( MaterNum ).EMPDMaterialProps = true;
			Material( MaterNum ).EMPDperm = MaterialProps( 1 );
			Material( MaterNum ).MoistACoeff = MaterialProps( 2 );
			Material( MaterNum ).MoistBCoeff = MaterialProps( 3 );
			Material( MaterNum ).MoistCCoeff = MaterialProps( 4 );
			Material( MaterNum ).MoistDCoeff = MaterialProps( 5 );
			Material( MaterNum ).EMPDPeriodShort = MaterialProps( 6 );
			Material( MaterNum ).EMPDPeriodLong = MaterialProps( 7 );
			Material( MaterNum ).CoatingPerm = MaterialProps( 8 );
			Material( MaterNum ).CoatingThickness = MaterialProps( 9 );

		}

		// Ensure at least one interior EMPD surface for each zone
		EMPDzone.dimension( NumOfZones, false );
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf || Surface( SurfNum ).Class == SurfaceClass_Window ) continue; // Heat transfer surface only and not a window
			if ( Surface( SurfNum ).HeatTransferAlgorithm != HeatTransferModel_EMPD ) continue;
			ConstrNum = Surface( SurfNum ).Construction;
			MatNum = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers );
			if ( Material( MatNum ).EMPDperm > 0.0 && Surface( SurfNum ).Zone > 0 ) {
				EMPDzone( Surface( SurfNum ).Zone ) = true;
			} else {
				++ErrCount;
				if ( ErrCount == 1 && ! DisplayExtraWarnings ) {
					ShowMessage( "GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the inside layer of Surfaces" );
					ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces." );
				}
				if ( DisplayExtraWarnings ) {
					ShowMessage( "GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the inside layer in Surface=" + Surface( SurfNum ).Name );
					ShowContinueError( "with Construction=" + Construct( ConstrNum ).Name );
				}
			}
			if ( Construct( ConstrNum ).TotLayers == 1 ) { // One layer construction
				continue;
			} else { // Multiple layer construction
				if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).EMPDMaterialProps && Surface( SurfNum ).ExtBoundCond <= 0 ) { // The external layer is not exposed to zone
					ShowSevereError( "GetMoistureBalanceEMPDInput: EMPD properties are assigned to the outside layer in Construction=" + Construct( ConstrNum ).Name );
					ShowContinueError( "..Outside layer material with EMPD properties = " + Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Name );
					ShowContinueError( "..A material with EMPD properties must be assigned to the inside layer of a construction." );
					ErrorsFound = true;
				}
				for ( Layer = 2; Layer <= Construct( ConstrNum ).TotLayers - 1; ++Layer ) {
					if ( Material( Construct( ConstrNum ).LayerPoint( Layer ) ).EMPDMaterialProps ) {
						ShowSevereError( "GetMoistureBalanceEMPDInput: EMPD properties are assigned to a middle layer in Construction=" + Construct( ConstrNum ).Name );
						ShowContinueError( "..Middle layer material with EMPD properties = " + Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Name );
						ShowContinueError( "..A material with EMPD properties must be assigned to the inside layer of a construction." );
						ErrorsFound = true;
					}
				}
			}
		}

		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			if ( ! EMPDzone( Loop ) ) {
				ShowSevereError( "GetMoistureBalanceEMPDInput: None of the constructions for zone = " + Zone( Loop ).Name + " has an inside layer with EMPD properties" );
				ShowContinueError( "..For each zone, the inside layer of at least one construction must have EMPD properties" );
				ErrorsFound = true;
			}
		}

		EMPDzone.deallocate();

		ReportMoistureBalanceEMPD();

		if ( ErrorsFound ) {
			ShowFatalError( "GetMoistureBalanceEMPDInput: Errors found getting EMPD material properties, program terminated." );
		}

	}

	void
	InitMoistureBalanceEMPD()
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date written:   August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Create dynamic array for surface moisture calculation

		// METHODOLOGY EMPLOYED:

		// USE STATEMENTS:

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
		int ZoneNum;
		int Loop;
		int SurfNum;
		static bool InitEnvrnFlag( true );

		if ( InitEnvrnFlag ) {
			MoistEMPDOld.allocate( TotSurfaces );
			MoistEMPDInt.allocate( TotSurfaces );
			MoistEMPDNew.allocate( TotSurfaces );
			MoistEMPDFlux.allocate( TotSurfaces );
			RhoVapEMPD.allocate( TotSurfaces );
			WSurfEMPD.allocate( TotSurfaces );
			RHEMPD.allocate( TotSurfaces );
			RVsurface.allocate( TotSurfaces );
			RVsurfOld.allocate( TotSurfaces );
			RVdeep.allocate( TotSurfaces );
			RVdeepOld.allocate( TotSurfaces );
			RVwall.allocate( TotSurfaces );
			HMshort.allocate( TotSurfaces );
			FluxSurf.allocate( TotSurfaces );
			FluxDeep.allocate( TotSurfaces );
			FluxZone.allocate( TotSurfaces );
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			ZoneNum = Surface( SurfNum ).Zone;
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			if ( ZoneAirHumRat( ZoneNum ) == 0.0 ) {
				MoistEMPDOld( SurfNum ) = 0.0001;
				MoistEMPDInt( SurfNum ) = 0.0001;
				MoistEMPDNew( SurfNum ) = 0.0001;
				RVsurface( SurfNum ) = 0.001;
				RVsurfOld( SurfNum ) = 0.001;
				RVdeep( SurfNum ) = 0.001;
				RVdeepOld( SurfNum ) = 0.001;
				RVwall( SurfNum ) = 0.001;
				HMshort( SurfNum ) = 0.001;
				FluxSurf( SurfNum ) = 0.001;
				FluxDeep( SurfNum ) = 0.001;
				FluxZone( SurfNum ) = 0.001;
			} else {
				MoistEMPDOld( SurfNum ) = ZoneAirHumRat( ZoneNum ); // Surface moisture level initialization
				MoistEMPDInt( SurfNum ) = ZoneAirHumRat( ZoneNum ); // by assuming initial values be equal to ZoneAirHumRat
				MoistEMPDNew( SurfNum ) = ZoneAirHumRat( ZoneNum );
				RVsurface( SurfNum ) = RhoVaporAirIn( SurfNum );
				RVsurfOld( SurfNum ) = RhoVaporAirIn( SurfNum );
				RVdeep( SurfNum ) = RhoVaporAirIn( SurfNum );
				RVdeepOld( SurfNum ) = RhoVaporAirIn( SurfNum );
				RVwall( SurfNum ) = RhoVaporAirIn( SurfNum );
				HMshort( SurfNum ) = 0.0003;
				FluxSurf( SurfNum ) = 0.000;
				FluxDeep( SurfNum ) = 0.000;
				FluxZone( SurfNum ) = 0.000;
			}
		}
		if ( ! InitEnvrnFlag ) return;
		//Initialize the report variable
		RhoVapEMPD = 0.015;
		WSurfEMPD = 0.015;
		RHEMPD = 0.0;
		MoistEMPDFlux = 0.0;

		GetMoistureBalanceEMPDInput();

		for ( Loop = 1; Loop <= TotSurfaces; ++Loop ) {
			if ( ! Surface( Loop ).HeatTransSurf ) continue;
			if ( Surface( Loop ).Class == SurfaceClass_Window ) continue;
			SetupOutputVariable( "EMPD Surface Inside Face Water Vapor Density [kg/m3]", RhoVapEMPD( Loop ), "Zone", "State", Surface( Loop ).Name );
			SetupOutputVariable( "EMPD Surface Inside Face Humidity Ratio [kgWater/kgDryAir]", WSurfEMPD( Loop ), "Zone", "State", Surface( Loop ).Name );
			SetupOutputVariable( "EMPD Surface Inside Face Relative Humidity [%]", RHEMPD( Loop ), "Zone", "State", Surface( Loop ).Name );
		}

		if ( InitEnvrnFlag ) InitEnvrnFlag = false;

	}

	void
	CalcMoistureBalanceEMPD(
		int const SurfNum,
		Real64 const TempSurfIn, // INSIDE SURFACE TEMPERATURE at current time step
		Real64 const TempSurfInOld, // INSIDE SURFACE TEMPERATURE at previous time step.
		Real64 const TempZone, // Zone temperature at current time step.
		Real64 & TempSat // Satutare surface temperature.
	)
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date written:   August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate surface moisture level using EMPD model

		// METHODOLOGY EMPLOYED:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyRhFnTdbRhovLBnd0C;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyRhovFnTdbWPb;
		using Psychrometrics::PsyRhFnTdbRhov;
		using Psychrometrics::PsyRhovFnTdbRh;
		using Psychrometrics::PsyPsatFnTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Error( 0.01 ); // Totlarence (%)
		Real64 const RLXM( 0.3 ); // Relaxation factor (0-1)
		Real64 const Lam( 0.1 ); // Heat of vaporization (J/kg)
		static std::string const RoutineName( "CalcMoistureEMPD" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NOFITR; // Number of iterations
		int ZoneNum; // Surface number
		int MatNum; // Material number at interior layer
		int ConstrNum; // Construction number
		Real64 RHOBULK; // Material bulk density
		Real64 HMlong; // Overall deep-layer transfer coefficient
		Real64 RSurfaceLayer; // Mass transfer resistance between actual surface and surface layer node
		Real64 Taver; // Average zone temperature between current time and previous time
		//    REAL(r64)    :: Waver     ! Average zone humidity ratio between current time and previous time
		Real64 RHaver; // Average zone relative humidity {0-1} between current time and previous time
		Real64 RVaver; // Average zone vapor density
		Real64 AT;
		Real64 BR;
		Real64 RALPHA; // Zone vapor density
		Real64 BB; // Coefficient for ODE
		Real64 CC; // Coefficient for ODE
		Real64 ErrorM; // Percent error
		int Flag; // Convergence flag (0 - converged)
		static bool OneTimeFlag( true );
		Real64 Wsurf; // Surface moisture flux
		Real64 PVsurf; // Surface vapor pressure
		Real64 PVsat; // saturation surface vapor pressure
		Real64 RHSurfOld;
		Real64 RHDeepOld;
		Real64 Wsat; // saturation humidity ratio
		Real64 dEMPD;
		Real64 dEMPDdeep;
		Real64 EMPDdiffusivity;
		Real64 Rcoating;
		Real64 RHsurface;
		Real64 RHdeep;

		if ( BeginEnvrnFlag && OneTimeFlag ) {
			InitMoistureBalanceEMPD();
			OneTimeFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			OneTimeFlag = true;
		}

		MoistEMPDFlux( SurfNum ) = 0.0;
		Flag = 1;
		NOFITR = 0;
		if ( ! Surface( SurfNum ).HeatTransSurf ) {
			return;
		}
		ConstrNum = Surface( SurfNum ).Construction;
		MatNum = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers ); // Then find the material pointer

		ZoneNum = Surface( SurfNum ).Zone;
		auto const & material( Material( MatNum ) );
		if ( material.EMPDperm <= 0.0 ) {
			MoistEMPDNew( SurfNum ) = PsyRhovFnTdbWPb( TempZone, ZoneAirHumRat( ZoneNum ), OutBaroPress );
			return;
		}

		Taver = 24.5;
		RVaver = ( MoistEMPDNew( SurfNum ) + MoistEMPDOld( SurfNum ) ) * 0.5;
		RHaver = RVaver * 461.52 * ( Taver + KelvinConv ) * std::exp( -23.7093 + 4111.0 / ( Taver + 237.7 ));
		PVsat = PsyPsatFnTemp( Taver, RoutineName );
		Wsat = 0.622 * PVsat / ( OutBaroPress - PVsat );
		
		RHOBULK = material.Density;
		
		EMPDdiffusivity = material.EMPDperm * 461.52 * ( Taver + KelvinConv );
		
		AT = material.MoistACoeff * material.MoistBCoeff * pow( RHaver, material.MoistBCoeff - 1 ) + material.MoistCCoeff * material.MoistCCoeff * material.MoistDCoeff * pow( RHaver, material.MoistDCoeff - 1 );
		
		dEMPD = sqrt( material.EMPDperm * material.EMPDPeriodShort * PVsat / ( RHOBULK * AT * Pi ) );
		dEMPDdeep = sqrt( material.EMPDperm * material.EMPDPeriodLong * PVsat / ( RHOBULK * AT * Pi ) );
		
		Rcoating = material.CoatingThickness * material.CoatingPerm * 461.52 * ( Taver + KelvinConv );
		HMshort( SurfNum ) = 1.0 / ( 0.5 * dEMPD / EMPDdiffusivity + 1.0 / HMassConvInFD( SurfNum ) + Rcoating );
		HMlong = 2.0 * EMPDdiffusivity / ( dEMPDdeep + dEMPD );
		
		FluxSurf( SurfNum ) = HMshort( SurfNum ) * ( RVsurface( SurfNum ) - RhoVaporAirIn( SurfNum ) ) + HMlong * ( RVsurface( SurfNum ) - RVdeep( SurfNum ) );
		FluxDeep( SurfNum ) = HMlong * ( RVsurface( SurfNum ) - RVdeep( SurfNum ) );
		FluxZone( SurfNum ) = HMshort( SurfNum ) * ( RVsurface( SurfNum ) - RhoVaporAirIn( SurfNum ) );
		
		RHDeepOld = PsyRhFnTdbRhov( Taver, RVdeepOld( SurfNum ) );
		RHSurfOld = PsyRhFnTdbRhov( Taver, RVsurfOld( SurfNum ) );
		
		RHsurface = RHSurfOld + TimeStepZone * 3600.0 * ( - FluxSurf( SurfNum ) / ( RHOBULK * dEMPD * AT ) );
		RHdeep = RHDeepOld + TimeStepZone * 3600.0 * FluxDeep( SurfNum ) / ( RHOBULK * dEMPDdeep * AT );
		
		RVsurface( SurfNum ) = PsyRhovFnTdbRh( Taver, RHsurface );
		RVdeep( SurfNum ) = PsyRhovFnTdbRh( Taver, RHdeep );

		// Calculate latent load
		PVsurf = RHaver * std::exp( 23.7093 - 4111.0 / ( Taver + 237.7 ) );

		// Calculate surface dew point temperature based on surface vapor density
		TempSat = 4111.0 / ( 23.7093 - std::log( PVsurf ) ) + 35.45 - KelvinConv;

		// Put results in the single precision reporting variable
		RhoVapEMPD( SurfNum ) = RVsurface( SurfNum );
		RHEMPD( SurfNum ) = RHsurface * 100.0;
		WSurfEMPD( SurfNum ) = RhoVaporAirIn( SurfNum );

	}

	void
	SolverMoistureBalanceEMPD(
		Real64 & VARNEW, // Value at current time step
		Real64 const VAROLD, // Value at previous time step
		Real64 const A, // Coefficient of time derivative in AdV/dt+BV=C
		Real64 const B, // Coefficienct of variable
		Real64 const C // Constant
	)
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date writtenn:  August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Solve a first order ordinary differential equation, A dV/dt + B V = C

		// METHODOLOGY EMPLOYED:
		// Finite difference method

		// Using/Aliasing

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

		VARNEW = ( VAROLD + TimeStepZoneSec * C / A ) / ( 1.0 + TimeStepZoneSec * B / A );

	}

	void
	CloseMoistureBalanceEMPD()
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date writtenn:  August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Deallocate dynamic arrays for surface moisture calculation

		// METHODOLOGY EMPLOYED:

		// USE STATEMENTS:

		MoistEMPDOld.deallocate();
		MoistEMPDInt.deallocate();
		MoistEMPDNew.deallocate();
		MoistEMPDFlux.deallocate();
		RVsurface.deallocate();
		RVsurfOld.deallocate();
		RVdeep.deallocate();
		RVdeepOld.deallocate();

	}

	void
	UpdateMoistureBalanceEMPD( int const SurfNum ) // Surface number
	{

		// SUBROUTINE INFORMATION:
		//   Authors:        Muthusamy Swami and Lixing Gu
		//   Date writtenn:  August, 1999
		//   Modified:       na
		//   Re-engineered:  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update inside surface vapor density
		// METHODOLOGY EMPLOYED:

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
		// na

		MoistEMPDOld( SurfNum ) = MoistEMPDNew( SurfNum );
		RVdeepOld( SurfNum ) = RVdeep( SurfNum );
		RVsurfOld( SurfNum ) = RVsurface( SurfNum );

	}

	void
	ReportMoistureBalanceEMPD()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gives a detailed report to the user about
		// EMPD Properties of each construction.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// The subroutine of ReportCTFs written by Linda Lawrie was used to develop this routine.

		// Using/Aliasing
		using General::ScanForReports;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool DoReport;

		int ConstrNum;
		int MatNum;

		// Formats
		static gio::Fmt Format_700( "(' Construction EMPD, ',A,', ',A,', ',4(F8.4,', '),F8.4)" );

		ScanForReports( "Constructions", DoReport, "Constructions" );

		if ( ! DoReport ) return;
		//   Write Descriptions
		gio::write( OutputFileInits, fmtA ) << "! <Construction EMPD>, Construction Name, Inside Layer Material Name, Penetration Depth {m}, a, b, c, d";

		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
			if ( Construct( ConstrNum ).TypeIsWindow ) continue;
			MatNum = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers );
			if ( Material( MatNum ).EMPDMaterialProps ) {
				gio::write( OutputFileInits, Format_700 ) << Construct( ConstrNum ).Name << Material( MatNum ).Name << Material( MatNum ).EMPDperm << Material( MatNum ).MoistACoeff << Material( MatNum ).MoistBCoeff << Material( MatNum ).MoistCCoeff << Material( MatNum ).MoistDCoeff;
			}
		}

	}

	//     NOTICE

	//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
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

} // MoistureBalanceEMPDManager

} // EnergyPlus
