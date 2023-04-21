#include "lib_geothermal_test.h"



TEST_F(GeothermalPlantAnalyzer, TestBinaryPlant_lib_geothermal)
{
	conversion_type = 0;	
	GeothermalPlantAnalyzer::SetUp();

	EXPECT_NEAR(geoPlant_outputs.max_secondlaw, 0.4, 0.2);
	EXPECT_NEAR(geoPlant_outputs.md_GrossPlantOutputMW, 33.159, 3);
	EXPECT_NEAR(geoPlant_outputs.GF_flowrate, 4993110, 200000);	
}


TEST_F(GeothermalPlantAnalyzer, TestFlashPlant_lib_geothermal) {
	conversion_type = 1;	
	GeothermalPlantAnalyzer::SetUp();

	EXPECT_EQ(geoPlant_outputs.flash_count, 2);	//Dual Flash (Constrained) Plant Type
	EXPECT_NEAR(geoPlant_outputs.md_GrossPlantOutputMW, 33.978, 1);	//Expected value of 33.978 taken from GETEM
	EXPECT_NEAR(geoPlant_outputs.max_secondlaw, 0.5, 0.3);
}
