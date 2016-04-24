// EnergyPlus::WindowManager unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <WindowManager.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataSurfaces.hh>
#include <ElectricPowerServiceManager.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceIntRadExchange.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <Psychrometrics.hh>
#include <SolarShading.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WindowManager;

TEST_F(EnergyPlusFixture, WindowFrameTest )
{

	bool ErrorsFound( false );

	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Material,",
		"  Concrete Block,          !- Name",
		"  MediumRough,             !- Roughness",
		"  0.1014984,               !- Thickness {m}",
		"  0.3805070,               !- Conductivity {W/m-K}",
		"  608.7016,                !- Density {kg/m3}",
		"  836.8000;                !- Specific Heat {J/kg-K}",
		"Construction,",
		"  WallConstruction,        !- Name",
		"  Concrete Block;          !- Outside Layer",
		"WindowMaterial:SimpleGlazingSystem,",
		"  WindowMaterial,          !- Name",
		"  5.778,                   !- U-Factor {W/m2-K}",
		"  0.819,                   !- Solar Heat Gain Coefficient",
		"  0.881;                   !- Visible Transmittance",
		"Construction,",
		"  WindowConstruction,      !- Name",
		"  WindowMaterial;          !- Outside Layer",
		"WindowProperty:FrameAndDivider,",
	  "  WindowFrame,             !- Name",
	  "  0.05,                    !- Frame Width {m}",
	  "  0.00,                    !- Frame Outside Projection {m}",
	  "  0.00,                    !- Frame Inside Projection {m}",
	  "  5.0,                     !- Frame Conductance {W/m2-K}",
	  "  1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Glass Conductance",
	  "  0.8,                     !- Frame Solar Absorptance",
	  "  0.8,                     !- Frame Visible Absorptance",
	  "  0.9,                     !- Frame Thermal Hemispherical Emissivity",
	  "  DividedLite,             !- Divider Type",
	  "  0.02,                    !- Divider Width {m}",
	  "  2,                       !- Number of Horizontal Dividers",
	  "  2,                       !- Number of Vertical Dividers",
	  "  0.00,                    !- Divider Outside Projection {m}",
	  "  0.00,                    !- Divider Inside Projection {m}",
	  "  5.0,                     !- Divider Conductance {W/m2-K}",
	  "  1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-Glass Conductance",
	  "  0.8,                     !- Divider Solar Absorptance",
	  "  0.8,                     !- Divider Visible Absorptance",
	  "  0.9;                     !- Divider Thermal Hemispherical Emissivity",
		"FenestrationSurface:Detailed,",
		"  FenestrationSurface,     !- Name",
		"  Window,                  !- Surface Type",
	  "  WindowConstruction,      !- Construction Name",
	  "  Wall,                    !- Building Surface Name",
	  "  ,                        !- Outside Boundary Condition Object",
	  "  0.5000000,               !- View Factor to Ground",
	  "  ,                        !- Shading Control Name",
	  "  WindowFrame,             !- Frame and Divider Name",
	  "  1.0,                     !- Multiplier",
	  "  4,                       !- Number of Vertices",
	  "  0.200000,0.000000,9.900000,  !- X,Y,Z ==> Vertex 1 {m}",
	  "  0.200000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 2 {m}",
	  "  9.900000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 3 {m}",
	  "  9.900000,0.000000,9.900000;  !- X,Y,Z ==> Vertex 4 {m}",
		"BuildingSurface:Detailed,"
	  "  Wall,                    !- Name",
	  "  Wall,                    !- Surface Type",
	  "  WallConstruction,        !- Construction Name",
	  "  Zone,                    !- Zone Name",
	  "  Outdoors,                !- Outside Boundary Condition",
	  "  ,                        !- Outside Boundary Condition Object",
	  "  SunExposed,              !- Sun Exposure",
	  "  WindExposed,             !- Wind Exposure",
	  "  0.5000000,               !- View Factor to Ground",
	  "  4,                       !- Number of Vertices",
	  "  0.000000,0.000000,10.00000,  !- X,Y,Z ==> Vertex 1 {m}",
	  "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
	  "  10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
	  "  10.00000,0.000000,10.00000;  !- X,Y,Z ==> Vertex 4 {m}",
		"BuildingSurface:Detailed,"
	  "  Floor,                   !- Name",
	  "  Floor,                   !- Surface Type",
	  "  WallConstruction,        !- Construction Name",
	  "  Zone,                    !- Zone Name",
	  "  Outdoors,                !- Outside Boundary Condition",
	  "  ,                        !- Outside Boundary Condition Object",
	  "  NoSun,                   !- Sun Exposure",
	  "  NoWind,                  !- Wind Exposure",
	  "  1.0,                     !- View Factor to Ground",
	  "  4,                       !- Number of Vertices",
	  "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
	  "  0.000000,10.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
	  "  10.00000,10.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
	  "  10.00000,0.000000,0;  !- X,Y,Z ==> Vertex 4 {m}",
		"Zone,"
	  "  Zone,                    !- Name",
	  "  0,                       !- Direction of Relative North {deg}",
	  "  6.000000,                !- X Origin {m}",
	  "  6.000000,                !- Y Origin {m}",
	  "  0,                       !- Z Origin {m}",
	  "  1,                       !- Type",
	  "  1,                       !- Multiplier",
	  "  autocalculate,           !- Ceiling Height {m}",
	  "  autocalculate;           !- Volume {m3}"
	});


	ASSERT_FALSE(process_idf(idf_objects));

	DataHeatBalance::ZoneIntGain.allocate(1);

	createFacilityElectricPowerServiceObject();
	HeatBalanceManager::SetPreConstructionInputParameters();
	HeatBalanceManager::GetProjectControlData( ErrorsFound );
	HeatBalanceManager::GetFrameAndDividerData( ErrorsFound );
	HeatBalanceManager::GetMaterialData( ErrorsFound );
	HeatBalanceManager::GetConstructData( ErrorsFound );
	HeatBalanceManager::GetBuildingData( ErrorsFound );

	Psychrometrics::InitializePsychRoutines();

	DataGlobals::TimeStep = 1;
	DataGlobals::TimeStepZone = 1;
	DataGlobals::HourOfDay = 1;
	DataGlobals::NumOfTimeStepInHour = 1;
	DataGlobals::BeginSimFlag = true;
	DataGlobals::BeginEnvrnFlag = true;
	DataEnvironment::OutBaroPress = 100000;

	HeatBalanceManager::ManageHeatBalance();

	// This test will emulate NFRC 100 U-factor test
	int winNum;

	for (size_t i = 1; i <= DataSurfaces::Surface.size_; ++i) {
		if (DataSurfaces::Surface( i ).Class == DataSurfaces::SurfaceClass_Window) {
			winNum = i;
		}
	}

	int cNum;

	for (size_t i = 1; i <= DataHeatBalance::Construct.size_; ++i) {
		if (DataHeatBalance::Construct( i ).TypeIsWindow) {
			cNum = i;
		}
	}

	Real64 T_in = 21.0;
	Real64 T_out = -18.0;
	Real64 I_s = 0.0;
	Real64 v_ws = 5.5;

	// Overrides for testing
	DataHeatBalance::CosIncAng.dimension( 1, 1, 3, 1.0 );
	DataHeatBalance::SunlitFrac.dimension( 1, 1, 3, 1.0 );
	DataHeatBalance::SunlitFracWithoutReveal.dimension( 1, 1, 3, 1.0 );

	DataSurfaces::Surface( winNum ).OutDryBulbTemp = T_out;
	DataHeatBalance::TempEffBulkAir( winNum ) = T_in;
	DataSurfaces::SurfaceWindow( winNum ).IRfromParentZone = DataGlobals::StefanBoltzmann*std::pow(T_in + DataGlobals::KelvinConv,4);
	DataHeatBalFanSys::ZoneAirHumRatAvg.dimension(1, 0.01);
	DataHeatBalFanSys::ZoneAirHumRat.dimension(1, 0.01);
	DataHeatBalFanSys::MAT.dimension(1, T_in);

	// initial guess temperatures
	int numTemps = 2 + 2*DataHeatBalance::Construct( cNum ).TotGlassLayers;
	Real64 inSurfTemp = T_in - (1.0/(numTemps-1))*(T_in - T_out);
	Real64 outSurfTemp = T_out + (1.0/(numTemps-1))*(T_in - T_out);

	Real64 h_exterior_f = 4 + v_ws*4;
	Real64 h_exterior;

	DataEnvironment::BeamSolarRad = I_s;

	if ( I_s > 0.0 ) {
		DataEnvironment::SunIsUp = true;
	}

	HeatBalanceSurfaceManager::InitSolarHeatGains();
	SolarShading::CalcInteriorSolarDistribution();

	// Calculate heat balance (iteratively solve for surface temperatures)
	Real64 outSurfTempPrev = outSurfTemp;
	Real64 inSurfTempPrev = inSurfTemp;

	Real64 outSurfTempDiff;
	Real64 inSurfTempDiff;

	int maxIterations = 20;
	Real64 tolerance = 0.1; // deg C

	// Save tilt information for natural convection calculations
	Real64 tiltSave = DataSurfaces::Surface( winNum ).Tilt;

	for (int i = 0; i < maxIterations; i++) {

		// Use complementary angle for exterior natural convection calculations
		DataSurfaces::Surface( 1 ).Tilt = 180 - tiltSave;
		DataSurfaces::Surface( 1 ).CosTilt = cos(DataSurfaces::Surface( winNum ).Tilt*DataGlobals::Pi/180);
		DataSurfaces::Surface( 1 ).SinTilt = sin(DataSurfaces::Surface( winNum ).Tilt*DataGlobals::Pi/180);
		ConvectionCoefficients::CalcISO15099WindowIntConvCoeff( winNum, outSurfTemp, T_out); // This subroutine sets the global HConvIn( 1 ) variable. We will use it to set the exterior natural convection.
		h_exterior = h_exterior_f + DataHeatBalance::HConvIn( winNum ); // add natural convection

		// revert tilt for interior natural convection calculations
		DataSurfaces::Surface( 1 ).Tilt = tiltSave;
		DataSurfaces::Surface( 1 ).CosTilt = cos(tiltSave*DataGlobals::Pi/180);
		DataSurfaces::Surface( 1 ).SinTilt = sin(tiltSave*DataGlobals::Pi/180);
		ConvectionCoefficients::CalcISO15099WindowIntConvCoeff( winNum, inSurfTemp, T_in); // This time it's actually being used as intended. HConvIn( 1 ) is referenced from the actual heat balance calculation.

		WindowManager::CalcWindowHeatBalance( winNum, h_exterior, inSurfTemp, outSurfTemp );

		outSurfTempDiff = std::fabs(outSurfTemp - outSurfTempPrev);
		inSurfTempDiff = std::fabs(inSurfTemp - inSurfTempPrev);

		if ( (outSurfTempDiff < tolerance) && (inSurfTempDiff < tolerance) ) {
			break;
		}

		outSurfTempPrev = outSurfTemp;
		inSurfTempPrev = inSurfTemp;

	}

	EXPECT_GT(DataSurfaces::WinHeatLossRep( winNum ), DataSurfaces::WinHeatTransfer( winNum ));

}

TEST_F(EnergyPlusFixture, WindowManager_TransAndReflAtPhi)
{
	
	Real64 const cs = 0.86603;  // Cosine of incidence angle
	Real64 const tf0 = 0.8980; // Transmittance at zero incidence angle
	Real64 const rf0 = 0.0810; // Front reflectance at zero incidence angle
	Real64 const rb0 = 0.0810; // Back reflectance at zero incidence angle

	Real64 tfp = 0.; // Transmittance at cs
	Real64 rfp = 0.; // Front reflectance at cs
	Real64 rbp = 0.; // Back reflectance at cs

	bool const SimpleGlazingSystem = false; // .TRUE. if simple block model being used
	Real64 const SimpleGlazingSHGC = 0.; // SHGC value to use in alternate model for simple glazing system
	Real64 const SimpleGlazingU = 0.; // U-factor value to use in alternate model for simple glazing system

	TransAndReflAtPhi(cs, tf0, rf0, rb0, tfp, rfp, rbp, SimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU);

	EXPECT_NEAR(tfp, 0.89455, 0.0001);
	EXPECT_NEAR(rfp, 0.08323, 0.0001);
	EXPECT_NEAR(rbp, 0.08323, 0.0001);

}



