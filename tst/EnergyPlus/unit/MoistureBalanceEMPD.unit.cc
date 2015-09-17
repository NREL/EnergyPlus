// EnergyPlus::MoistureBalanceEMPD Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/HVACFixture.hh"
#include <EnergyPlus/MoistureBalanceEMPDManager.hh>
#include <EnergyPlus/HeatBalancemanager.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataGlobals.hh>

using namespace EnergyPlus;

TEST_F( HVACFixture, CheckEMPD )
{
	std::string const idf_objects = delimited_string({
		"Version, 8.3;",
		"Material,",
		"Concrete,                !- Name",
		"Rough,                   !- Roughness",
		"0.152,                   !- Thickness {m}",
		"0.3,                     !- Conductivity {W/m-K}",
		"1000,                    !- Density {kg/m3}",
		"950,                     !- Specific Heat {J/kg-K}",
		"0.900000,                !- Thermal Absorptance",
		"0.600000,                !- Solar Absorptance",
		"0.600000;                !- Visible Absorptance",
		"MaterialProperty:MoisturePenetrationDepth:Settings,",
		"Concrete,                !- Name",
		"6.554,                     !- Water Vapor Diffusion Resistance Factor {dimensionless} (mu)",
		"0.0661,                   !- Moisture Equation Coefficient a {dimensionless} (MoistACoeff)",
		"1,                       !- Moisture Equation Coefficient b {dimensionless} (MoistBCoeff)",
		"0,                       !- Moisture Equation Coefficient c {dimensionless} (MoistCCoeff)",
		"1,                       !- Moisture Equation Coefficient d {dimensionless} (MoistDCoeff)",
		"0.006701,                    !- Surface-layer penetrtion depth {m} (dEMPD)",
		"0.0,                    !- Deep-layer penetration depth {m} (dEPMDdeep)",
		"0,                       !- Coating layer permability {m} (CoatingThickness)",
		"1;                       !- Coating layer water vapor diffusion resistance factor {dimensionless} (muCoating)	});"
	});

	ASSERT_FALSE( process_idf(idf_objects) );

	bool errors_found( false );
	HeatBalanceManager::GetMaterialData(errors_found);
	ASSERT_FALSE( errors_found ) << "Errors in GetMaterialData";

	// Surface
	using DataSurfaces::TotSurfaces;
	TotSurfaces = 1;
	DataSurfaces::Surface.allocate( TotSurfaces );
	DataSurfaces::SurfaceData & surface = DataSurfaces::Surface( 1 );
	surface.Name = "Surface1";
	surface.Area = 1.0;
	surface.HeatTransSurf = true;

	// Zone
	surface.Zone = 1;
	DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
	DataMoistureBalance::RhoVaporAirIn.allocate(1);
	DataMoistureBalance::HMassConvInFD.allocate(1);

	// Construction
	surface.Construction = 1;
	DataHeatBalance::Construct.allocate(1);
	DataHeatBalance::ConstructionData & construction = DataHeatBalance::Construct(1);
	construction.TotLayers = 1;
	construction.LayerPoint(construction.TotLayers) = InputProcessor::FindItemInList( "CONCRETE", DataHeatBalance::Material );

	// Initialize and get inputs
	MoistureBalanceEMPDManager::InitMoistureBalanceEMPD();

	// Set up conditions
	DataGlobals::TimeStepZone = 0.25;
	DataEnvironment::OutBaroPress = 101325.;
	DataHeatBalFanSys::ZoneAirHumRat(1) = 0.0061285406810457849;
	DataMoistureBalanceEMPD::RVSurface(1) = 0.007077173214149593;
	DataMoistureBalanceEMPD::RVSurfaceOld(1) = DataMoistureBalanceEMPD::RVSurface(1);
	DataMoistureBalance::HMassConvInFD(1) = 0.0016826898264131584;
	DataMoistureBalance::RhoVaporAirIn(1) = 0.0073097913062508896;
	DataMoistureBalanceEMPD::RVSurfLayer(1) = 0.007038850125652322;
	DataMoistureBalanceEMPD::RVDeepLayer(1) = 0.0051334905162138695;
	DataMoistureBalanceEMPD::RVdeepOld(1) = 0.0051334905162138695;
	DataMoistureBalanceEMPD::RVSurfLayerOld(1) = 0.007038850125652322;

	// Do calcs
	Real64 Tsat(0.0);
	MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD( 1, 19.907302679986064, 19.901185713164697, Tsat );

	EXPECT_DOUBLE_EQ(6.3445188238394508, Tsat);
	EXPECT_DOUBLE_EQ(0.0072030790357129866, DataMoistureBalanceEMPD::RVSurface(1));
	EXPECT_DOUBLE_EQ(-0.00000019077843350248091, DataMoistureBalanceEMPD::MassFluxSurfaceLayer(1));
	EXPECT_DOUBLE_EQ(0.0, DataMoistureBalanceEMPD::MassFluxDeepLayer(1));
	EXPECT_DOUBLE_EQ(-0.00000019077843350248091, DataMoistureBalanceEMPD::MassFluxZone(1));
	EXPECT_DOUBLE_EQ(0.0070455149199232948, DataMoistureBalanceEMPD::RVSurfLayer(1));
	EXPECT_DOUBLE_EQ(0.00070413228834764112, DataMoistureBalanceEMPD::HMSurfaceLayer(1));
	EXPECT_DOUBLE_EQ(0.0051334905162138695, DataMoistureBalanceEMPD::RVDeepLayer(1));
	EXPECT_DOUBLE_EQ(-0.47694608375620229, DataMoistureBalanceEMPD::HeatFluxLatent(1));

	// Clean up
	DataHeatBalFanSys::ZoneAirHumRat.deallocate();
	DataMoistureBalance::RhoVaporAirIn.deallocate();
	MoistureBalanceEMPDManager::CloseMoistureBalanceEMPD();
}
