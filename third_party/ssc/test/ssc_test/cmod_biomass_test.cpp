#include "cmod_biomass_test.h"
#include "gtest/gtest.h"

TEST_F(CMBiomass, SingleOwnerDefault_cmod_biomass) {

	int biopower_errors = run_module(data, "biomass");
	ASSERT_EQ(biopower_errors, 0);

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 353982820.997, 0.1);

}