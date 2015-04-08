// EnergyPlus::ICS collector un-allocated collector data bug fix test

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataEnvironment.hh>

using namespace ObjexxFCL;
using namespace EnergyPlus;
using namespace EnergyPlus::ConvectionCoefficients;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalSurface;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::DataEnvironment;
using DataGlobals::BeginEnvrnFlag;

TEST( ICSSolarCollectorTest, CalcPassiveExteriorBaffleGapTest ) {
	ShowMessage( "Begin Test: ICSSolarCollectorTest, CalcPassiveExteriorBaffleGapTest" );

	// ICS collector un-allocated collector data bug fix test.  This unit test
	// does not test ICS collector performance but it does test a bug fix for
	// issue #4723 (crash) occured due to unallocated ICS collector data.
	// ! Collector.allocated()

	int const NumOfSurf( 1 );
	int SurfNum;
	int ZoneNum;
	int ConstrNum;
	int MatNum;

	InitializePsychRoutines();

	BeginEnvrnFlag = true;
	OutBaroPress = 101325.0;
	SkyTemp = 24.0;
	IsRain = false;
	MatNum = 1;
	ZoneNum = 1;
	SurfNum = 1;
	ConstrNum = 1;
	// allocate surface variable data
	Surface.allocate( NumOfSurf );
	Surface( SurfNum ).Area = 10.0;
	Surface( SurfNum ).OutDryBulbTemp = 20.0;
	Surface( SurfNum ).OutWetBulbTemp = 15.0;
	Surface( SurfNum ).WindSpeed = 3.0;
	Surface( SurfNum ).Construction = ConstrNum;
	Surface( SurfNum ).BaseSurf = SurfNum;
	Surface( SurfNum ).Zone = ZoneNum;
	Surface( SurfNum ).IsICS = true;
	Surface( SurfNum ).ExtConvCoeff = 0;
	Surface( SurfNum ).ExtWind = false;
	// allocate construction variable data
	Construct.allocate( ConstrNum );
	Construct( ConstrNum ).LayerPoint.allocate( MatNum );
	Construct( ConstrNum ).LayerPoint( MatNum ) = 1;
	Material.allocate( MatNum );
	Material( MatNum ).AbsorpThermal = 0.8;
	// allocate exterior vented cavaity variable data
	ExtVentedCavity.allocate( 1 );
	ExtVentedCavity( NumOfSurf ).SurfPtrs.allocate( NumOfSurf );
	ExtVentedCavity( NumOfSurf ).SurfPtrs( NumOfSurf ) = 1;
	// allocate zone variable data
	Zone.allocate( ZoneNum );
	Zone( ZoneNum ).OutsideConvectionAlgo = ASHRAESimple;
	// allocate surface temperature variable data
	TH.allocate( NumOfSurf, 1, 2 );
	TH( SurfNum, 1, 1 ) = 22.0;
	// allocate solar incident radiation variable data
	QRadSWOutIncident.allocate( 1 );
	QRadSWOutIncident( 1 ) = 0.0;
	// set user defined conv. coeff. calculation to false
	GetUserSuppliedConvectionCoeffs = false;

	// SurfPtr( 1 ); // Array of indexes pointing to Surface structure in DataSurfaces
	Real64 const VentArea( 0.1 ); // Area available for venting the gap [m2]
	Real64 const Cv( 0.1 ); // Oriface coefficient for volume-based discharge, wind-driven [--]
	Real64 const Cd( 0.5 ); // oriface coefficient for discharge,  bouyancy-driven [--]
	Real64 const HdeltaNPL( 3.0 ); // Height difference from neutral pressure level [m]
	Real64 const SolAbs( 0.75 ); // solar absorptivity of baffle [--]
	Real64 const AbsExt( 0.8 ); // thermal absorptance/emittance of baffle material [--]
	Real64 const Tilt( 0.283 ); // Tilt of gap [Degrees]
	Real64 const AspRat( 0.9 ); // aspect ratio of gap  Height/gap [--]
	Real64 const GapThick( 0.05 ); // Thickness of air space between baffle and underlying heat transfer surface
	int Roughness( 1 ); // Roughness index (1-6), see DataHeatBalance parameters
	Real64 QdotSource( 0.0 ); // Source/sink term, e.g. electricity exported from solar cell [W]
	Real64 TsBaffle( 20.0 ); // Temperature of baffle (both sides) use lagged value on input [C]
	Real64 TaGap( 22.0 ); // Temperature of air gap (assumed mixed) use lagged value on input [C]
	Real64 HcGapRpt; // gap convection coefficient [W/m2C]
	Real64 HrGapRpt; // gap radiation coefficient [W/m2C]
	Real64 IscRpt; //
	Real64 MdotVentRpt; // gap air mass flow rate [kg/s]
	Real64 VdotWindRpt; // gap wind driven air volume flow rate [m3/s]
	Real64 VdotBouyRpt; // gap bouyancy driven volume flow rate [m3/s]

	// call to test fix to resolve crash
	CalcPassiveExteriorBaffleGap( ExtVentedCavity( 1 ).SurfPtrs, VentArea, Cv, Cd, HdeltaNPL, SolAbs, AbsExt, Tilt, AspRat, GapThick, Roughness, QdotSource, TsBaffle, TaGap, HcGapRpt, HrGapRpt, IscRpt, MdotVentRpt, VdotWindRpt, VdotBouyRpt );

	EXPECT_NEAR( 21.862, TsBaffle, 0.001 );
	EXPECT_NEAR( 1.692, HcGapRpt, 0.001 );
	EXPECT_NEAR( 3.694, HrGapRpt, 0.001 );
	EXPECT_NEAR( 0.036, MdotVentRpt, 0.001 );

	// deallocated variables
	Surface.deallocate();
	Construct( ConstrNum ).LayerPoint.deallocate();
	Construct.deallocate();
	Material.deallocate();
	ExtVentedCavity( NumOfSurf ).SurfPtrs.deallocate();
	ExtVentedCavity.deallocate();
	Zone.deallocate();
	TH.deallocate();
	QRadSWOutIncident.deallocate();
}
