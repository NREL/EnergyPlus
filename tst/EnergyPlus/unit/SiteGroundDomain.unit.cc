// Site:GroundDomain unit tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include "EnergyPlus/DataPlantPipingSystems.hh"
#include "EnergyPlus/PlantPipingSystemsManager.hh"

using namespace EnergyPlus;
using namespace DataPlantPipingSystems;
using namespace PlantPipingSystemsManager;

TEST_F( EnergyPlusFixture, SiteGroundDomainSlabAndBasementModelsIndexChecking )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KA1,						!- Name of object",
			"1.8,						!- Soil Thermal Conductivity {W/m-K}",
			"3200,						!- Soil Density {kg/m3}",
			"836,						!- Soil Specific Heat {J/kg-K}",
			"15.5,						!- Annual average surface temperature {C}",
			"12.8,						!- Annual amplitude of surface temperature {delta C}",
			"17.3;						!- Phase shift of minimum surface temperature {days}",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KA2,						!- Name of object",
			"1.8,						!- Soil Thermal Conductivity {W/m-K}",
			"3200,						!- Soil Density {kg/m3}",
			"836,						!- Soil Specific Heat {J/kg-K}",
			"15.5,						!- Annual average surface temperature {C}",
			"12.8,						!- Annual amplitude of surface temperature {delta C}",
			"17.3;						!- Phase shift of minimum surface temperature {days}",
	});

	EXPECT_FALSE( process_idf( idf_objects ) );

	PipingSystemDomains.allocate( 2 );

	GetGroundTempModel( PipingSystemDomains( 1 ).Farfield.groundTempModel, "Site:GroundTemperature:Undisturbed:KusudaAchenbach", "KA1" );

	GetGroundTempModel( PipingSystemDomains( 2 ).Farfield.groundTempModel, "Site:GroundTemperature:Undisturbed:KusudaAchenbach", "KA2" );

	EXPECT_NE( PipingSystemDomains( 1 ).Farfield.groundTempModel, PipingSystemDomains( 2 ).Farfield.groundTempModel );

}
