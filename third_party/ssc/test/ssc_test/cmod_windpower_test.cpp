
#include <fstream>
#include <gtest/gtest.h>
#include <vector>

#include <json/json.h>
#include <lib_util.h>
#include "../ssc/sscapi.h"
#include "vartab.h"
#include "cmod_windpower_test.h"

/// Measurement heights are different from the turbine's hub height
TEST_F(CMWindPowerIntegration, HubHeightInterpolation_cmod_windpower) {
    // Case 1: hubheight is 200, error
    ssc_data_unassign(data, "wind_resource_filename");
    var_data *windresourcedata = create_winddata_array(1, 1);
    var_table *vt = static_cast<var_table *>(data);
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
TEST_F(CMWindPowerIntegration, WakeModelsUsingFile_cmod_windpower) {
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
    EXPECT_NEAR(monthly_energy, 2.7472e6, e) << "Wasp: Dec";

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
    EXPECT_NEAR(annual_energy, gross * 0.95, e) << "Constant";

    ssc_data_get_number(data, "wake_losses", &wake_loss);
    EXPECT_NEAR(wake_loss, 5, 1e-3) << "Constant: Wake loss";
}

/// Using Interpolated Subhourly Wind Data
TEST_F(CMWindPowerIntegration, UsingInterpolatedSubhourly_cmod_windpower) {
    // Using AR Northwestern-Flat Lands

    const char *SSCDIR = std::getenv("SSCDIR");
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
    EXPECT_NEAR(check_annual_energy, hourly_annual_energy, 0.005 * check_annual_energy);

    ssc_number_t check_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
    EXPECT_NEAR(check_january_energy, hourly_january_energy, 0.005 * check_january_energy);

    size_t nEntries = static_cast<var_table *>(data)->lookup("gen")->num.ncols();
    EXPECT_EQ(nEntries, 8760 * 4);

    // Using 5 min File
    sprintf(file, "%s/test/input_docs/AR Northwestern-Flat Lands-5min.srw", SSCDIR);

    ssc_data_set_string(data, "wind_resource_filename", file);
    success = compute();

    EXPECT_TRUE(success) << "Computation 3 should succeed";

    ssc_data_get_number(data, "annual_energy", &check_annual_energy);
    EXPECT_NEAR(check_annual_energy, hourly_annual_energy, 0.005 * check_annual_energy);

    check_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
    EXPECT_NEAR(check_january_energy, hourly_january_energy, 0.005 * check_january_energy);

    nEntries = static_cast<var_table *>(data)->lookup("gen")->num.ncols();
    EXPECT_EQ(nEntries, 8760 * 12);
}

/// Using Wind Resource Data
TEST_F(CMWindPowerIntegration, UsingDataArray_cmod_windpower) {
    // using hourly data
    ssc_data_unassign(data, "wind_resource_filename");
    var_data *windresourcedata = create_winddata_array(1, 1);
    var_table *vt = static_cast<var_table *>(data);
    vt->assign("wind_resource_data", *windresourcedata);

    compute();
    double expectedAnnualEnergy = 4219481;
    double relErr = expectedAnnualEnergy * .001;


    ssc_number_t annual_energy;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    EXPECT_NEAR(annual_energy, expectedAnnualEnergy, relErr);

    ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
    EXPECT_NEAR(monthly_energy, 0, relErr / 10.);

    monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
    EXPECT_NEAR(monthly_energy, 1972735, relErr / 10.);

    free_winddata_array(windresourcedata);

    // 15 min data
    ssc_data_unassign(data, "wind_resource_data");
    windresourcedata = create_winddata_array(4, 1);
    vt->assign("wind_resource_data", *windresourcedata);

    compute();

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
    auto *vt = static_cast<var_table *>(data);
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
    auto *vt = static_cast<var_table *>(data);
    vt->assign("wind_resource_distribution", dist);
    ssc_data_set_number(data, "wind_farm_wake_model", 3);
    ssc_data_set_number(data, "wake_int_loss", 5);
    ssc_data_set_number(data, "avail_turb_loss", 5);

    compute();

    ssc_number_t annual_energy, gross;
    ssc_data_get_number(data, "annual_energy", &annual_energy);
    ssc_data_get_number(data, "annual_gross_energy", &gross);
    EXPECT_NEAR(gross, 160804000, e);
    EXPECT_NEAR(annual_energy, gross * 0.95 * 0.95, e);

    ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
    EXPECT_NEAR(monthly_energy, 12326000, e);

}

/// Icing and Low Temp Cutoff, with Wind Resource Data
TEST_F(CMWindPowerIntegration, IcingAndLowTempCutoff_cmod_windpower) {
    //modify test inputs
    ssc_data_unassign(data, "wind_resource_filename");
    var_data *windresourcedata = create_winddata_array(1, 1);
    double rh[8760];
    for (unsigned int i = 0; i < 8760; i++) {
        if (i % 2 == 0) rh[i] = 0.75f;
        else rh[i] = 0.0f;
    }
    var_data rh_vd = var_data(rh, 8760);
    windresourcedata->table.assign("rh", rh_vd);
    auto *vt = static_cast<var_table *>(data);
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
TEST(Turbine_powercurve_cmod_windpower_eqns, NoData) {
    ASSERT_THROW(Turbine_calculate_powercurve(nullptr), std::runtime_error);
}

TEST(Turbine_powercurve_cmod_windpower_eqns, MissingVariables) {
    var_table *vd = new var_table;
    ASSERT_THROW(Turbine_calculate_powercurve(vd), std::runtime_error);
}

TEST(Turbine_powercurve_cmod_windpower_eqns, Case1) {
    var_table *vd = new var_table;
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

TEST(Turbine_powercurve_cmod_windpower_eqns, Case2) {
    var_table *vd = new var_table;
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

TEST(Turbine_powercurve_cmod_windpower_eqns, Case3) {
    var_table *vd = new var_table;
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

TEST(Turbine_powercurve_cmod_windpower_eqns, Case4) {
    var_table *vd = new var_table;
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

bool setup_python() {
#ifdef __WINDOWS__
    auto python_dir = std::string(std::getenv("SAMNTDIR")) + "\\deploy\\runtime\\python\\";
#else
    if (!std::getenv("CMAKEBLDDIR")) return false;
    auto python_dir = std::string(std::getenv("CMAKEBLDDIR")) + "/sam/SAM.app/Contents/runtime/python/";

    if (!util::dir_exists(std::string(python_dir + "Miniconda-4.8.2/").c_str())){
        std::cerr << "Python not configured.";
        return false;
    }
#endif

    set_python_path(python_dir.c_str());
    return true;
}

TEST(windpower_landbosse, SetupPython) {
	// load python configuration
	if (!setup_python())
	    return;

	Json::Value python_config_root;
	std::string configPath = std::string(get_python_path()) + "python_config.json";

	std::ifstream python_config_doc(configPath);
	if (python_config_doc.fail()) {
	    printf("Could not open %s", configPath.c_str());
	    return;
	}

	python_config_doc >> python_config_root;

	if (!python_config_root.isMember("miniconda_version"))
		throw std::runtime_error("Missing key 'miniconda_version' in " + configPath);
	if (!python_config_root.isMember("python_version"))
		throw std::runtime_error("Missing key 'python_version' in " + configPath);
	if (!python_config_root.isMember("exec_path"))
		throw std::runtime_error("Missing key 'exec_path' in " + configPath);
	if (!python_config_root.isMember("pip_path"))
		throw std::runtime_error("Missing key 'pip_path' in " + configPath);
	if (!python_config_root.isMember("packages"))
		throw std::runtime_error("Missing key 'packages' in " + configPath);

	std::vector<std::string> packages;
	for (auto &i : python_config_root["packages"])
		packages.push_back(i.asString());

	std::vector<std::string> config = { python_config_root["python_version"].asString(),
						   python_config_root["miniconda_version"].asString(),
						   python_config_root["exec_path"].asString(),
						   python_config_root["pip_path"].asString()
						    };

}

bool check_Python_setup() {
    if (!setup_python())  {
        std::cerr << "Python not configured.";
        return false;
    }
    std::string configPath = std::string(get_python_path()) + "python_config.json";
    std::ifstream python_config_doc(configPath);
    Json::Value python_config_root;
    python_config_doc >> python_config_root;
    if (python_config_root["exec_path"].asString().empty()) {
        std::cerr << "Python not configured.";
        return false;
    }
    return true;
}

TEST(windpower_landbosse, RunSuccess) {
    if (!check_Python_setup())
        return;

    char file[256];
    sprintf(file, "%s/test/input_docs/AR Northwestern-Flat Lands.srw", SSCDIR);

    auto *vd = new var_table;
    vd->assign("en_landbosse", 1);
    vd->assign("wind_resource_filename", std::string(file));
    vd->assign("turbine_rating_MW", 1.5);
    vd->assign("wind_turbine_rotor_diameter", 45);
    vd->assign("wind_turbine_hub_ht", 80);
    vd->assign("num_turbines", 100);
    vd->assign("wind_resource_shear", 0.2);
    vd->assign("turbine_spacing_rotor_diameters", 4);
    vd->assign("row_spacing_rotor_diameters", 10);

    vd->assign("interconnect_voltage_kV", 137);
    vd->assign("distance_to_interconnect_mi", 10);
    vd->assign("depth", 2.36);
    vd->assign("rated_thrust_N", 589000);
    vd->assign("labor_cost_multiplier", 1);
    vd->assign("gust_velocity_m_per_s", 59.50);

    auto landbosse = ssc_module_create("wind_landbosse");

    ssc_module_exec(landbosse, vd);

    ASSERT_EQ(vd->lookup("errors")->str, "0");
    EXPECT_NEAR(vd->lookup("total_collection_cost")->num[0], 4202342, 1e2);
    EXPECT_NEAR(vd->lookup("total_development_cost")->num[0], 150000, 1e2);
    EXPECT_NEAR(vd->lookup("total_erection_cost")->num[0], 6057403, 1e2);
    EXPECT_NEAR(vd->lookup("total_foundation_cost")->num[0], 10036157, 1e2);
    EXPECT_NEAR(vd->lookup("total_gridconnection_cost")->num[0], 5.61774e+06, 1e2);
    EXPECT_NEAR(vd->lookup("total_management_cost")->num[0], 10516516, 1e2);
    EXPECT_NEAR(vd->lookup("total_bos_cost")->num[0], 43836161, 1e2);
    EXPECT_NEAR(vd->lookup("total_sitepreparation_cost")->num[0], 2698209, 1e2);
    EXPECT_NEAR(vd->lookup("total_substation_cost")->num[0], 4940746, 1e2);

    std::vector<std::string> all_outputs = {"bonding_usd", "collection_equipment_rental_usd", "collection_labor_usd",
                                            "collection_material_usd", "collection_mobilization_usd",
                                            "construction_permitting_usd", "development_labor_usd",
                                            "development_material_usd", "development_mobilization_usd",
                                            "engineering_usd", "erection_equipment_rental_usd", "erection_fuel_usd",
                                            "erection_labor_usd", "erection_material_usd", "erection_mobilization_usd",
                                            "erection_other_usd", "foundation_equipment_rental_usd",
                                            "foundation_labor_usd", "foundation_material_usd",
                                            "foundation_mobilization_usd", "insurance_usd", "markup_contingency_usd",
                                            "project_management_usd", "site_facility_usd",
                                            "sitepreparation_equipment_rental_usd", "sitepreparation_labor_usd",
                                            "sitepreparation_material_usd", "sitepreparation_mobilization_usd"};

    for (auto& i : all_outputs){
        EXPECT_GE(vd->lookup(i)->num[0], 0) << i;
    }
}

TEST(windpower_landbosse, SubhourlyFail) {
    if (!check_Python_setup())
        return;

    char file[256];
    sprintf(file, "%s/test/input_docs/AR Northwestern-Flat Lands-15min.srw", SSCDIR);

    auto *vd = new var_table;
    vd->assign("en_landbosse", 1);
    vd->assign("wind_resource_filename", std::string(file));
    vd->assign("turbine_rating_MW", 1.5);
    vd->assign("wind_turbine_rotor_diameter", 45);
    vd->assign("wind_turbine_hub_ht", 80);
    vd->assign("num_turbines", 100);
    vd->assign("wind_resource_shear", 0.2);
    vd->assign("turbine_spacing_rotor_diameters", 4);
    vd->assign("row_spacing_rotor_diameters", 10);

    vd->assign("interconnect_voltage_kV", 137);
    vd->assign("distance_to_interconnect_mi", 10);
    vd->assign("depth", 2.36);
    vd->assign("rated_thrust_N", 589000);
    vd->assign("labor_cost_multiplier", 1);
    vd->assign("gust_velocity_m_per_s", 59.50);

    auto landbosse = ssc_module_create("wind_landbosse");

    bool success = ssc_module_exec(landbosse, vd);

    EXPECT_FALSE(success);

    auto err = vd->lookup("errors")->str;
    EXPECT_EQ(err, "Error in Weather_Data: Length of values does not match length of index");

}

TEST(windpower_landbosse, NegativeInputFail) {
    if (!check_Python_setup())
        return;

    char file[256];
    sprintf(file, "%s/test/input_docs/AR Northwestern-Flat Lands.srw", SSCDIR);

    auto *vd = new var_table;
    vd->assign("en_landbosse", 1);
    vd->assign("wind_resource_filename", std::string(file));
    vd->assign("turbine_rating_MW", 1.5);
    vd->assign("wind_turbine_rotor_diameter", 45);
    vd->assign("wind_turbine_hub_ht", 80);
    vd->assign("num_turbines", 100);
    vd->assign("wind_resource_shear", 0.2);
    vd->assign("turbine_spacing_rotor_diameters", 4);
    vd->assign("row_spacing_rotor_diameters", 10);

    vd->assign("interconnect_voltage_kV", 137);
    vd->assign("distance_to_interconnect_mi", 10);
    vd->assign("depth", -2.36);
    vd->assign("rated_thrust_N", 589000);
    vd->assign("labor_cost_multiplier", 1);
    vd->assign("gust_velocity_m_per_s", 59.50);

    auto landbosse = ssc_module_create("wind_landbosse");

    bool success = ssc_module_exec(landbosse, vd);

    EXPECT_FALSE(success);

    auto err = vd->lookup("errors")->str;
    EXPECT_EQ(err, "Error in NegativeInputError: User entered a negative value for depth. This is an invalid entry");

}
