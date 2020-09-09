#include <gtest/gtest.h>
#include <vector>

#include "../ssc/core.h"
#include "vartab.h"
#include "../ssc/common.h"
#include "cmod_windpower.h"
#include "cmod_windpower_test.h"

/// Measurement heights are different from the turbine's hub height
TEST_F(CMWindPowerIntegration, HubHeightInterpolation_cmod_windpower) {
	// Case 1: hubheight is 200, error
	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(1,1);
	var_table *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);
	vt->assign("wind_turbine_hub_ht", 200);

	bool completed = compute(false);
	EXPECT_FALSE(completed) << "Heights difference > 35m";

	// Case 2: hubweight is 90, use shear exponent interpolation
	vt->unassign("wind_turbine_hub_ht");
	vt->assign("wind_turbine_hub_ht", 90);

	compute();
	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_GT(annual_energy, 4e06) << "Annual energy should be higher than height at 90";

	free_winddata_array(windresourcedata);
}

/// Using Wind Resource File with various Wake Models
TEST_F(CMWindPowerIntegration, WakeModelsUsingFile_cmod_windpower){
	// Simple Wake Model
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 33224154, e) << "Simple";

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e) << "Simple: January";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e) << "Simple: December";

	ssc_number_t wake_loss;
	ssc_data_get_number(data, "wake_losses", &wake_loss);
    EXPECT_NEAR(wake_loss, 1.546, 1e-3) << "Simple: Wake loss";


    // WAsp Model
	ssc_data_set_number(data, "wind_farm_wake_model", 1);
	compute();

	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 32346158, e) << "Wasp";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.7472e6, e) << "Wasp: Jan";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.7472e6, e)<< "Wasp: Dec";

    ssc_data_get_number(data, "wake_losses", &wake_loss);
    EXPECT_NEAR(wake_loss, 4.148, 1e-3) << "Wasp: Wake loss";

	// Eddy Viscosity Model
	ssc_data_set_number(data, "wind_farm_wake_model", 2);
	compute();

	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 31081848, e) << "Eddy";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.6398e6, e) << "Eddy: Jan";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.6398e6, e) << "Eddy: Dec";

    ssc_data_get_number(data, "wake_losses", &wake_loss);
    EXPECT_NEAR(wake_loss, 7.895, 1e-3) << "Eddy: Wake loss";

	// Constant Loss Model
    ssc_data_set_number(data, "wind_farm_wake_model", 3);
    ssc_data_set_number(data, "wake_int_loss", 5);

    compute();

    ssc_number_t gross;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    ssc_data_get_number(data, "annual_gross_energy", &gross);
    EXPECT_NEAR(annual_energy, gross*0.95, e) << "Constant";

    ssc_data_get_number(data, "wake_losses", &wake_loss);
    EXPECT_NEAR(wake_loss, 5, 1e-3) << "Constant: Wake loss";
}

/// Using Interpolated Subhourly Wind Data
TEST_F(CMWindPowerIntegration, UsingInterpolatedSubhourly_cmod_windpower){
	// Using AR Northwestern-Flat Lands

    const char * SSCDIR = std::getenv("SSCDIR");
    char file[256];
    sprintf(file, "%s/test/input_docs/AR Northwestern-Flat Lands.srw", SSCDIR);

	ssc_data_set_string(data, "wind_resource_filename", file);
	bool success = compute();

    EXPECT_TRUE(success) << "Computation 1 should succeed";

    ssc_number_t hourly_annual_energy;
	ssc_data_get_number(data, "annual_energy", &hourly_annual_energy);

	ssc_number_t hourly_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];


	// Using 15 min File
	sprintf(file, "%s/test/input_docs/AR Northwestern-Flat Lands-15min.srw", SSCDIR);

	ssc_data_set_string(data, "wind_resource_filename", file);
	success = compute();

	EXPECT_TRUE(success) << "Computation 2 should succeed";

	ssc_number_t check_annual_energy;
	ssc_data_get_number(data, "annual_energy", &check_annual_energy);
	EXPECT_NEAR(check_annual_energy, hourly_annual_energy, 0.005*check_annual_energy);

	ssc_number_t check_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(check_january_energy, hourly_january_energy, 0.005*check_january_energy);

	size_t nEntries = static_cast<var_table*>(data)->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 4);

	// Using 5 min File
    sprintf(file, "%s/test/input_docs/AR Northwestern-Flat Lands-5min.srw", SSCDIR);

	ssc_data_set_string(data, "wind_resource_filename", file);
	success = compute();

    EXPECT_TRUE(success) << "Computation 3 should succeed";

    check_annual_energy;
	ssc_data_get_number(data, "annual_energy", &check_annual_energy);
	EXPECT_NEAR(check_annual_energy, hourly_annual_energy, 0.005*check_annual_energy);

	check_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(check_january_energy, hourly_january_energy, 0.005*check_january_energy);

	nEntries = static_cast<var_table*>(data)->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 12);
}

/// Using Wind Resource Data
TEST_F(CMWindPowerIntegration, UsingDataArray_cmod_windpower){
	// using hourly data
	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(1,1);
	var_table *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);

	compute();
	double expectedAnnualEnergy = 4219481;
	double relErr = expectedAnnualEnergy * .001;


	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, expectedAnnualEnergy, relErr);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 0, relErr/10.);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 1972735, relErr/10.);

	free_winddata_array(windresourcedata);

	// 15 min data
	ssc_data_unassign(data, "wind_resource_data");
	windresourcedata = create_winddata_array(4,1);
	vt->assign("wind_resource_data", *windresourcedata);

	compute();

	annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, expectedAnnualEnergy, relErr);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 0, relErr / 10.);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 1972735, relErr / 10.);

	int gen_length = 0;
	ssc_data_get_array(data, "gen", &gen_length);
	EXPECT_EQ(gen_length, 8760 * 4);

	free_winddata_array(windresourcedata);
}

/// Using Weibull Distribution
TEST_F(CMWindPowerIntegration, Weibull_cmod_windpower) {
	ssc_data_set_number(data, "wind_resource_model_choice", 1);
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 180453760, e);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 15326247, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 15326247, e);
}

/// Using Wind Resource 2-D Distribution
TEST_F(CMWindPowerIntegration, WindDist_cmod_windpower) {
    ssc_data_set_number(data, "wind_resource_model_choice", 2);
    double dist[18] = {1.5, 180, .12583,
                       5, 180, .3933,
                       8, 180, .18276,
                       10, 180, .1341,
                       13.5, 180, .14217,
                       19, 180, .0211};

    ssc_data_set_matrix(data, "wind_resource_distribution", dist, 6, 3);
    compute();

    ssc_number_t annual_energy;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    EXPECT_NEAR(annual_energy, 159807000, e);

    ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
    EXPECT_NEAR(monthly_energy, 13573000, e);

    monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
    EXPECT_NEAR(monthly_energy, 13573000, e);
}

/// Using Wind Resource 2-D Distribution
TEST_F(CMWindPowerIntegration, WindDist2_cmod_windpower) {
    ssc_data_set_number(data, "wind_resource_model_choice", 2);
    // mimic a weibull with k factor 2 and avg speed 7.25 for comparison -> scale param : 8.181
    double dst[18] = {1.5, 180, .12583,
                      5, 180, .3933,
                      8, 180, .18276,
                      10, 180, .1341,
                      13.5, 180, .14217,
                      19, 180, .0211};

    var_data dist = var_data(dst, 6, 3);
    auto *vt = static_cast<var_table*>(data);
    vt->assign("wind_resource_distribution", dist);
    compute();

    ssc_number_t annual_energy;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    EXPECT_NEAR(annual_energy, 159806945, e);

    ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
    EXPECT_NEAR(monthly_energy, 13572644, e);

    monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
    EXPECT_NEAR(monthly_energy, 13572644, e);
}

/// Using Wind Resource 2-D Distribution with Constant Loss Wake Model
TEST_F(CMWindPowerIntegration, WindDist3_cmod_windpower) {
    ssc_data_set_number(data, "wind_resource_model_choice", 2);
    // mimic a weibull with k factor 2 and avg speed 7.25 for comparison -> scale param : 8.181
    double dst[18] = {1.5, 180, .12583,
                      5, 180, .3933,
                      8, 180, .18276,
                      10, 180, .1341,
                      13.5, 180, .14217,
                      19, 180, .0211};

    var_data dist = var_data(dst, 6, 3);
    auto *vt = static_cast<var_table*>(data);
    vt->assign("wind_resource_distribution", dist);
    ssc_data_set_number(data, "wind_farm_wake_model", 3);
    ssc_data_set_number(data, "wake_int_loss", 5);
    ssc_data_set_number(data, "avail_turb_loss", 5);

    compute();

    ssc_number_t annual_energy, gross;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    ssc_data_get_number(data, "annual_gross_energy", &gross);
    EXPECT_NEAR(gross, 160804000, e);
    EXPECT_NEAR(annual_energy, gross*0.95*0.95, e);

    ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
    EXPECT_NEAR(monthly_energy, 12326000, e);

}

/// Icing and Low Temp Cutoff, with Wind Resource Data
TEST_F(CMWindPowerIntegration, IcingAndLowTempCutoff_cmod_windpower) {
	//modify test inputs
	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(1,1);
	double rh[8760];
	for (unsigned int i = 0; i < 8760; i++) {
		if (i % 2 == 0) rh[i] = 0.75f;
		else rh[i] = 0.0f;
	}
	var_data rh_vd = var_data(rh, 8760);
	windresourcedata->table.assign("rh", rh_vd);
	auto *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);
	vt->assign("en_low_temp_cutoff", 1);
	vt->assign("en_icing_cutoff", 1);
	vt->assign("low_temp_cutoff", 40.f);
	vt->assign("icing_cutoff_temp", 55.f);
	vt->assign("icing_cutoff_rh", 0.70f);

	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 2110545, e) << "Reduced annual energy";

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 0, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 986114, e);

	ssc_number_t losses_percent;
	ssc_data_get_number(data, "cutoff_losses", &losses_percent);
	EXPECT_NEAR(losses_percent, 0.5, 0.01);

	free_winddata_array(windresourcedata);
}

/// Testing Turbine powercurve calculation
TEST(Turbine_powercurve_cmod_windpower_eqns, NoData){
    ASSERT_THROW(Turbine_calculate_powercurve(nullptr), std::runtime_error);
}

TEST(Turbine_powercurve_cmod_windpower_eqns, MissingVariables){
    var_table* vd = new var_table;
    ASSERT_THROW(Turbine_calculate_powercurve(vd), std::runtime_error);
}

TEST(Turbine_powercurve_cmod_windpower_eqns, Case1){
    var_table* vd = new var_table;
    vd->assign("turbine_size", 1500);
    vd->assign("wind_turbine_rotor_diameter", 75);
    vd->assign("elevation", 0);
    vd->assign("wind_turbine_max_cp", 0.45);
    vd->assign("max_tip_speed", 80);
    vd->assign("max_tip_sp_ratio", 8);
    vd->assign("cut_in", 4);
    vd->assign("cut_out", 25);
    vd->assign("drive_train", 0);

    Turbine_calculate_powercurve(vd);

    util::matrix_t<ssc_number_t> ws = vd->lookup("wind_turbine_powercurve_windspeeds")->num;
    util::matrix_t<ssc_number_t> power = vd->lookup("wind_turbine_powercurve_powerout")->num;
    util::matrix_t<ssc_number_t> eff = vd->lookup("hub_efficiency")->num;
    double rated_wx = vd->lookup("rated_wind_speed")->num;
    ASSERT_NEAR(power[17], 64.050, 1e-2);
    ASSERT_NEAR(power[18], 80.0420, 1e-2);
    ASSERT_NEAR(power[43], 1346.764, 1e-2);
    ASSERT_NEAR(power[44], 1431.227, 1e-2);
    ASSERT_NEAR(power[45], 1500., 1e-2);
    ASSERT_NEAR(power[100], 0., 1e-2);
    ASSERT_NEAR(ws[100], 25., 1e-2);
    ASSERT_NEAR(rated_wx, 11.204, 1e-2);
}

TEST(Turbine_powercurve_cmod_windpower_eqns, Case2){
    var_table* vd = new var_table;
    vd->assign("turbine_size", 1500);
    vd->assign("wind_turbine_rotor_diameter", 75);
    vd->assign("elevation", 0);
    vd->assign("wind_turbine_max_cp", 0.45);
    vd->assign("max_tip_speed", 80);
    vd->assign("max_tip_sp_ratio", 8);
    vd->assign("cut_in", 4);
    vd->assign("cut_out", 25);
    vd->assign("drive_train", 1);

    Turbine_calculate_powercurve(vd);

    util::matrix_t<ssc_number_t> ws = vd->lookup("wind_turbine_powercurve_windspeeds")->num;
    util::matrix_t<ssc_number_t> power = vd->lookup("wind_turbine_powercurve_powerout")->num;
    util::matrix_t<ssc_number_t> eff = vd->lookup("hub_efficiency")->num;
    double rated_wx = vd->lookup("rated_wind_speed")->num;
    ASSERT_NEAR(power[17], 67.26, 1e-2);
    ASSERT_NEAR(power[18], 83.971, 1e-2);
    ASSERT_NEAR(power[44], 1416.36, 1e-2);
    ASSERT_NEAR(power[45], 1494.44, 1e-2);
    ASSERT_NEAR(power[46], 1500., 1e-2);
    ASSERT_NEAR(power[100], 0., 1e-2);
    ASSERT_NEAR(ws[100], 25., 1e-2);
    ASSERT_NEAR(rated_wx, 11.27, 1e-2);
}

TEST(Turbine_powercurve_cmod_windpower_eqns, Case3){
    var_table* vd = new var_table;
    vd->assign("turbine_size", 1500);
    vd->assign("wind_turbine_rotor_diameter", 75);
    vd->assign("elevation", 0);
    vd->assign("wind_turbine_max_cp", 0.45);
    vd->assign("max_tip_speed", 80);
    vd->assign("max_tip_sp_ratio", 8);
    vd->assign("cut_in", 4);
    vd->assign("cut_out", 25);
    vd->assign("drive_train", 2);

    Turbine_calculate_powercurve(vd);

    util::matrix_t<ssc_number_t> ws = vd->lookup("wind_turbine_powercurve_windspeeds")->num;
    util::matrix_t<ssc_number_t> power = vd->lookup("wind_turbine_powercurve_powerout")->num;
    util::matrix_t<ssc_number_t> eff = vd->lookup("hub_efficiency")->num;
    double rated_wx = vd->lookup("rated_wind_speed")->num;
    ASSERT_NEAR(power[17], 62.66, 1e-2);
    ASSERT_NEAR(power[18], 79.24, 1e-2);
    ASSERT_NEAR(power[44], 1405.26, 1e-2);
    ASSERT_NEAR(power[45], 1483.27, 1e-2);
    ASSERT_NEAR(power[46], 1500., 1e-2);
    ASSERT_NEAR(power[100], 0., 1e-2);
    ASSERT_NEAR(ws[100], 25., 1e-2);
    ASSERT_NEAR(rated_wx, 11.30, 1e-2);
}

TEST(Turbine_powercurve_cmod_windpower_eqns, Case4){
    var_table* vd = new var_table;
    vd->assign("turbine_size", 1500);
    vd->assign("wind_turbine_rotor_diameter", 75);
    vd->assign("elevation", 0);
    vd->assign("wind_turbine_max_cp", 0.45);
    vd->assign("max_tip_speed", 80);
    vd->assign("max_tip_sp_ratio", 8);
    vd->assign("cut_in", 4);
    vd->assign("cut_out", 25);
    vd->assign("drive_train", 3);

    Turbine_calculate_powercurve(vd);

    util::matrix_t<ssc_number_t> ws = vd->lookup("wind_turbine_powercurve_windspeeds")->num;
    util::matrix_t<ssc_number_t> power = vd->lookup("wind_turbine_powercurve_powerout")->num;
    util::matrix_t<ssc_number_t> eff = vd->lookup("hub_efficiency")->num;
    double rated_wx = vd->lookup("rated_wind_speed")->num;
    ASSERT_NEAR(power[17], 74.44, 1e-2);
    ASSERT_NEAR(power[18], 91.43, 1e-2);
    ASSERT_NEAR(power[43], 1356.14, 1e-2);
    ASSERT_NEAR(power[44], 1434.82, 1e-2);
    ASSERT_NEAR(power[45], 1500., 1e-2);
    ASSERT_NEAR(power[100], 0., 1e-2);
    ASSERT_NEAR(ws[100], 25., 1e-2);
    ASSERT_NEAR(rated_wx, 11.21, 1e-2);
}