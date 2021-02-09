#include <gtest/gtest.h>

#include "cmod_tcsmolten_salt_test.h"
#include "../tcs_test/tcsmolten_salt_cases.h"
#include "../input_cases/weather_inputs.h"

/// Test tcsmolten_salt with all defaults and the single owner financial model
TEST_F(CMTcsMoltenSalt, Rankine_Default_SingleOwner_cmod_tcsmolten_salt) {

    ssc_data_t data = ssc_data_create();
    int test_errors = tcsmolten_salt_daggett_default(data);

    EXPECT_FALSE(test_errors);
    if (!test_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 571408807.373179, 571408807.373179 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t land_area_base;
        ssc_data_get_number(data, "land_area_base", &land_area_base);
        EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 63.023494, 63.023494 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_W_cycle_gross;
        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
        EXPECT_NEAR(annual_W_cycle_gross, 642428580.492706, 642428580.492706 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 5520.858042, 5520.858042 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t conversion_factor;
        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
        EXPECT_NEAR(conversion_factor, 88.945110, 88.945110 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t N_hel;
        ssc_data_get_number(data, "N_hel", &N_hel);
        EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t rec_height;
        ssc_data_get_number(data, "rec_height", &rec_height);
        EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t A_sf;
        ssc_data_get_number(data, "A_sf", &A_sf);
        EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t D_rec;
        ssc_data_get_number(data, "D_rec", &D_rec);
        EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t annual_total_water_use;
        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
        EXPECT_NEAR(annual_total_water_use, 98221.126175, 98221.126175 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t csp_pt_cost_total_land_area;
        ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
        EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        ssc_number_t h_tower;
        ssc_data_get_number(data, "h_tower", &h_tower);
        EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

        //ssc_number_t VARIABLE;
        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
    }
}

/// Test tcsmolten_salt with alternative turbine inlet pressure control: Sliding pressure
/// Rest default configurations with respect to the single owner financial model
TEST_F(CMTcsMoltenSalt, Rankine_Sliding_P_SingleOwner_cmod_tcsmolten_salt) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsmolten_salt_daggett_sliding_pressure(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 576302445.677569, 576302445.677569 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t land_area_base;
		ssc_data_get_number(data, "land_area_base", &land_area_base);
		EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 63.563237, 63.563237 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 647174668.052062, 647174668.052062 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 5568.139572, 5568.139572 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 89.048981, 89.048981 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t N_hel;
		ssc_data_get_number(data, "N_hel", &N_hel);
		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t rec_height;
		ssc_data_get_number(data, "rec_height", &rec_height);
		EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t A_sf;
		ssc_data_get_number(data, "A_sf", &A_sf);
		EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t D_rec;
		ssc_data_get_number(data, "D_rec", &D_rec);
		EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 98238.031245, 98238.031245 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t csp_pt_cost_total_land_area;
		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t h_tower;
		ssc_data_get_number(data, "h_tower", &h_tower);
		EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		//ssc_number_t VARIABLE;
		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
	}
}

/// Test tcsmolten_salt with alternative flow pattern: Flow pattern 8
/// Rest default configurations with respect to the single owner financial model
TEST_F(CMTcsMoltenSalt, Rankine_Flow_Pattern_SingleOwner_cmod_tcsmolten_salt) {

	ssc_data_t data = ssc_data_create();
	int test_errors = tcsmolten_salt_daggett_flow_pattern(data);

	EXPECT_FALSE(test_errors);
	if (!test_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 518055493.136035, 518055493.136035 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t land_area_base;
		ssc_data_get_number(data, "land_area_base", &land_area_base);
		EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 57.138894, 57.138894 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_W_cycle_gross;
		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
		EXPECT_NEAR(annual_W_cycle_gross, 646287965.853696, 646287965.853696 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 5005.367083, 5005.367083 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t conversion_factor;
		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
		EXPECT_NEAR(conversion_factor, 80.158617, 80.158617 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t N_hel;
		ssc_data_get_number(data, "N_hel", &N_hel);
		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t rec_height;
		ssc_data_get_number(data, "rec_height", &rec_height);
		EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t A_sf;
		ssc_data_get_number(data, "A_sf", &A_sf);
		EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t D_rec;
		ssc_data_get_number(data, "D_rec", &D_rec);
		EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t annual_total_water_use;
		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
		EXPECT_NEAR(annual_total_water_use, 98470.230665, 98470.230665 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t csp_pt_cost_total_land_area;
		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		ssc_number_t h_tower;
		ssc_data_get_number(data, "h_tower", &h_tower);
		EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi

		//ssc_number_t VARIABLE;
		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
	}
}

/// Testing Molten Salt Power Tower UI Equations

TEST(Mspt_cmod_csp_tower_eqns, NoData) {
	ASSERT_THROW(MSPT_System_Design_Equations(nullptr), std::runtime_error);
	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(nullptr), std::runtime_error);
	//ASSERT_THROW(MSPT_Receiver_Equations(nullptr), std::runtime_error);
	//ASSERT_THROW(MSPT_System_Control_Equations(nullptr), std::runtime_error);
	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(nullptr), std::runtime_error);
	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(nullptr), std::runtime_error);
	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(nullptr), std::runtime_error);
}

//TEST(Mspt_cmod_csp_tower_eqns, MissingVariables) {
//	var_table* vd = new var_table;
//	ASSERT_THROW(MSPT_System_Design_Equations(vd), std::runtime_error);
//	ASSERT_THROW(Tower_SolarPilot_Solar_Field_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_Receiver_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(MSPT_System_Control_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_DSPT_Equations(vd), std::runtime_error);
//	//ASSERT_THROW(Tower_SolarPilot_Capital_Costs_ISCC_Equations(vd), std::runtime_error);
//}

TEST(Mspt_cmod_csp_tower_eqns, Case1) {
	double error_tolerance = 0.01;
	var_table* vd = new var_table;
	vd->assign("design_eff", 0.412);
	vd->assign("gross_net_conversion_factor", 0.9);
	vd->assign("P_ref", 115.);
	vd->assign("solarm", 2.4);
	vd->assign("tshours", 10.);

	MSPT_System_Design_Equations(vd);

	double nameplate = vd->lookup("nameplate")->num;
	double q_pb_design = vd->lookup("q_pb_design")->num;
	double q_rec_des = vd->lookup("q_rec_des")->num;
	double tshours_sf = vd->lookup("tshours_sf")->num;
	ASSERT_NEAR(nameplate, 103.5, 103.5 * error_tolerance);
	ASSERT_NEAR(q_pb_design, 279., 279. * error_tolerance);
	ASSERT_NEAR(q_rec_des, 670., 670. * error_tolerance);
	ASSERT_NEAR(tshours_sf, 4.16667, 4.16667 * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case2) {
	double error_tolerance = 0.01;
	var_table* vd = new var_table;
	vd->assign("c_atm_0", 0.006789);
	vd->assign("c_atm_1", 0.1046);
	vd->assign("c_atm_2", -0.017);
	vd->assign("c_atm_3", 0.002845);
	vd->assign("csp_pt_sf_fixed_land_area", 45.);
	vd->assign("csp_pt_sf_land_overhead_factor", 1.);
	vd->assign("dens_mirror", 0.97);
	vd->assign("dni_des", 950.);
	vd->assign("h_tower", 193.458);
	vd->assign("helio_height", 12.2);
	vd->assign("helio_optical_error_mrad", 1.53);
	util::matrix_t<double> helio_positions(8790, 2, 1.e3);
	vd->assign("helio_positions", helio_positions);
	vd->assign("helio_width", 12.2);
	vd->assign("land_area_base", 1847.04);
	vd->assign("land_max", 9.5);
	vd->assign("land_min", 0.75);
	vd->assign("override_layout", 0);
	vd->assign("override_opt", 0);
	vd->assign("q_rec_des", 670.);

	Tower_SolarPilot_Solar_Field_Equations(vd);

	double a_sf_ui = vd->lookup("a_sf_ui")->num;
	double c_atm_info = vd->lookup("c_atm_info")->num;
	double csp_pt_sf_heliostat_area = vd->lookup("csp_pt_sf_heliostat_area")->num;
	double csp_pt_sf_total_land_area = vd->lookup("csp_pt_sf_total_land_area")->num;
	//double csp_pt_sf_total_reflective_area = vd->lookup("csp_pt_sf_total_reflective_area")->num;	//  This one is not being read in the UI
	double csp_pt_sf_tower_height = vd->lookup("csp_pt_sf_tower_height")->num;
	double dni_des_calc = vd->lookup("dni_des_calc")->num;
	double error_equiv = vd->lookup("error_equiv")->num;
	double field_model_type = vd->lookup("field_model_type")->num;
	double helio_area_tot = vd->lookup("helio_area_tot")->num;
	double is_optimize = vd->lookup("is_optimize")->num;
	double land_max_calc = vd->lookup("land_max_calc")->num;
	double land_min_calc = vd->lookup("land_min_calc")->num;
	double n_hel = vd->lookup("n_hel")->num;
	double opt_algorithm = vd->lookup("opt_algorithm")->num;
	double opt_flux_penalty = vd->lookup("opt_flux_penalty")->num;
	double q_design = vd->lookup("q_design")->num;
	ASSERT_NEAR(a_sf_ui, 1269055., 1269055. * error_tolerance);
	ASSERT_NEAR(c_atm_info, 12.97, 12.97 * error_tolerance);
	ASSERT_NEAR(csp_pt_sf_heliostat_area, 144.375, 144.375 * error_tolerance);
	ASSERT_NEAR(csp_pt_sf_total_land_area, 1892., 1892. * error_tolerance);
	//ASSERT_NEAR(csp_pt_sf_total_reflective_area, 1269056.25, 1269056.25 * error_tolerance);			//  This one is not being read in the UI
	ASSERT_NEAR(csp_pt_sf_tower_height, 193.458, 193.458 * error_tolerance);
	ASSERT_NEAR(dni_des_calc, 950., 950. * error_tolerance);
	ASSERT_NEAR(error_equiv, 4.32749, 4.32749 * error_tolerance);
	ASSERT_NEAR(field_model_type, 2., 2. * error_tolerance);
	ASSERT_NEAR(helio_area_tot, 1269055., 1269055. * error_tolerance);
	ASSERT_NEAR(is_optimize, 0., 0. * error_tolerance);
	ASSERT_NEAR(land_max_calc, 1837.85, 1837.85 * error_tolerance);
	ASSERT_NEAR(land_min_calc, 145.094, 145.094 * error_tolerance);
	ASSERT_NEAR(n_hel, 8790., 8790. * error_tolerance);
	ASSERT_NEAR(opt_algorithm, 1., 1. * error_tolerance);
	ASSERT_NEAR(opt_flux_penalty, 0.25, 0.25 * error_tolerance);
	ASSERT_NEAR(q_design, 670., 670. * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case2b) {
    // Testing period use in variable names
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("c_atm_0", 0.006789);
    vd->assign("c_atm_1", 0.1046);
    vd->assign("c_atm_2", -0.017);
    vd->assign("c_atm_3", 0.002845);
    vd->assign("csp.pt.sf.fixed_land_area", 45.);
    vd->assign("csp.pt.sf.land_overhead_factor", 1.);
    vd->assign("dens_mirror", 0.97);
    vd->assign("dni_des", 950.);
    vd->assign("h_tower", 193.458);
    vd->assign("helio_height", 12.2);
    vd->assign("helio_optical_error_mrad", 1.53);
    util::matrix_t<double> helio_positions(8790, 2, 1.e3);
    vd->assign("helio_positions", helio_positions);
    vd->assign("helio_width", 12.2);
    vd->assign("land_area_base", 1847.04);
    vd->assign("land_max", 9.5);
    vd->assign("land_min", 0.75);
    vd->assign("override_layout", 0);
    vd->assign("override_opt", 0);
    vd->assign("q_rec_des", 670.);

    Tower_SolarPilot_Solar_Field_Equations(vd);

    double a_sf_ui = vd->lookup("a_sf_ui")->num;
    double c_atm_info = vd->lookup("c_atm_info")->num;
    double csp_pt_sf_heliostat_area = vd->lookup("csp.pt.sf.heliostat_area")->num;
    double csp_pt_sf_total_land_area = vd->lookup("csp.pt.sf.total_land_area")->num;
    //double csp_pt_sf_total_reflective_area = vd->lookup("csp.pt.sf.total_reflective_area")->num;	//  This one is not being read in the UI
    double csp_pt_sf_tower_height = vd->lookup("csp.pt.sf.tower_height")->num;
    double dni_des_calc = vd->lookup("dni_des_calc")->num;
    double error_equiv = vd->lookup("error_equiv")->num;
    double field_model_type = vd->lookup("field_model_type")->num;
    double helio_area_tot = vd->lookup("helio_area_tot")->num;
    double is_optimize = vd->lookup("is_optimize")->num;
    double land_max_calc = vd->lookup("land_max_calc")->num;
    double land_min_calc = vd->lookup("land_min_calc")->num;
    double n_hel = vd->lookup("n_hel")->num;
    double opt_algorithm = vd->lookup("opt_algorithm")->num;
    double opt_flux_penalty = vd->lookup("opt_flux_penalty")->num;
    double q_design = vd->lookup("q_design")->num;
    ASSERT_NEAR(a_sf_ui, 1269055., 1269055. * error_tolerance);
    ASSERT_NEAR(c_atm_info, 12.97, 12.97 * error_tolerance);
    ASSERT_NEAR(csp_pt_sf_heliostat_area, 144.375, 144.375 * error_tolerance);
    ASSERT_NEAR(csp_pt_sf_total_land_area, 1892., 1892. * error_tolerance);
    //ASSERT_NEAR(csp_pt_sf_total_reflective_area, 1269056.25, 1269056.25 * error_tolerance);			//  This one is not being read in the UI
    ASSERT_NEAR(csp_pt_sf_tower_height, 193.458, 193.458 * error_tolerance);
    ASSERT_NEAR(dni_des_calc, 950., 950. * error_tolerance);
    ASSERT_NEAR(error_equiv, 4.32749, 4.32749 * error_tolerance);
    ASSERT_NEAR(field_model_type, 2., 2. * error_tolerance);
    ASSERT_NEAR(helio_area_tot, 1269055., 1269055. * error_tolerance);
    ASSERT_NEAR(is_optimize, 0., 0. * error_tolerance);
    ASSERT_NEAR(land_max_calc, 1837.85, 1837.85 * error_tolerance);
    ASSERT_NEAR(land_min_calc, 145.094, 145.094 * error_tolerance);
    ASSERT_NEAR(n_hel, 8790., 8790. * error_tolerance);
    ASSERT_NEAR(opt_algorithm, 1., 1. * error_tolerance);
    ASSERT_NEAR(opt_flux_penalty, 0.25, 0.25 * error_tolerance);
    ASSERT_NEAR(q_design, 670., 670. * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case3) {
	double error_tolerance = 0.01;
	var_table* vd = new var_table;
	vd->assign("t_htf_cold_des", 290.);
	vd->assign("t_htf_hot_des", 574.);
	vd->assign("rec_htf", 17);
	vd->assign("csp_pt_rec_max_oper_frac", 1.2);
	vd->assign("q_rec_des", 660.9);
	vd->assign("rec_d_spec", 15.);
	vd->assign("csp_pt_rec_cav_ap_hw_ratio", 1.2);
	vd->assign("d_rec", 17.65);
	vd->assign("rec_height", 23.8084);
	vd->assign("h_tower", 193.458);
	vd->assign("piping_length_mult", 2.6);
	vd->assign("piping_length_const", 0.);
	vd->assign("piping_loss", 10200.);
	std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
	util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
	vd->assign("field_fl_props", field_fl_props);

	MSPT_Receiver_Equations(vd);

	double csp_pt_rec_htf_t_avg = vd->lookup("csp_pt_rec_htf_t_avg")->num;
	double csp_pt_rec_htf_c_avg = vd->lookup("csp_pt_rec_htf_c_avg")->num;
	double csp_pt_rec_max_flow_to_rec = vd->lookup("csp_pt_rec_max_flow_to_rec")->num;
	double csp_pt_rec_cav_ap_height = vd->lookup("csp_pt_rec_cav_ap_height")->num;
	double rec_aspect = vd->lookup("rec_aspect")->num;
	double piping_length = vd->lookup("piping_length")->num;
	double piping_loss_tot = vd->lookup("piping_loss_tot")->num;
	ASSERT_NEAR(csp_pt_rec_htf_t_avg, 432., 432. * error_tolerance);
	ASSERT_NEAR(csp_pt_rec_htf_c_avg, 1.5066, 1.5066 * error_tolerance);
	ASSERT_NEAR(csp_pt_rec_max_flow_to_rec, 1853.5, 1853.5 * error_tolerance);
	ASSERT_NEAR(csp_pt_rec_cav_ap_height, 18., 18. * error_tolerance);
	ASSERT_NEAR(rec_aspect, 1.349, 1.349 * error_tolerance);
	ASSERT_NEAR(piping_length, 502.991, 502.991 * error_tolerance);
	ASSERT_NEAR(piping_loss_tot, 5130.51, 5130.51 * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case3b) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("t_htf_cold_des", 290.);
    vd->assign("t_htf_hot_des", 574.);
    vd->assign("rec_htf", 17);
    vd->assign("csp.pt.rec.max_oper_frac", 1.2);
    vd->assign("q_rec_des", 660.9);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp.pt.rec.cav_ap_hw_ratio", 1.2);
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 23.8084);
    vd->assign("h_tower", 193.458);
    vd->assign("piping_length_mult", 2.6);
    vd->assign("piping_length_const", 0.);
    vd->assign("piping_loss", 10200.);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    vd->assign("field_fl_props", field_fl_props);

    MSPT_Receiver_Equations(vd);

    double csp_pt_rec_htf_t_avg = vd->lookup("csp.pt.rec.htf_t_avg")->num;
    double csp_pt_rec_htf_c_avg = vd->lookup("csp.pt.rec.htf_c_avg")->num;
    double csp_pt_rec_max_flow_to_rec = vd->lookup("csp.pt.rec.max_flow_to_rec")->num;
    double csp_pt_rec_cav_ap_height = vd->lookup("csp.pt.rec.cav_ap_height")->num;
    double rec_aspect = vd->lookup("rec_aspect")->num;
    double piping_length = vd->lookup("piping_length")->num;
    double piping_loss_tot = vd->lookup("piping_loss_tot")->num;
    ASSERT_NEAR(csp_pt_rec_htf_t_avg, 432., 432. * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_htf_c_avg, 1.5066, 1.5066 * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_max_flow_to_rec, 1853.5, 1853.5 * error_tolerance);
    ASSERT_NEAR(csp_pt_rec_cav_ap_height, 18., 18. * error_tolerance);
    ASSERT_NEAR(rec_aspect, 1.349, 1.349 * error_tolerance);
    ASSERT_NEAR(piping_length, 502.991, 502.991 * error_tolerance);
    ASSERT_NEAR(piping_loss_tot, 5130.51, 5130.51 * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case4) {
	double error_tolerance = 0.01;
	ssc_data_t data = ssc_data_create();
	auto data_vtab = static_cast<var_table*>(data);

	data_vtab->assign("P_ref", 115.);
	data_vtab->assign("design_eff", 0.412);
	data_vtab->assign("tshours", 10.);
	data_vtab->assign("T_htf_hot_des", 574.);
	data_vtab->assign("T_htf_cold_des", 290.);
	data_vtab->assign("rec_htf", 17);
	std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
	util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
	data_vtab->assign("field_fl_props", field_fl_props);
	data_vtab->assign("h_tank_min", 1.);
	data_vtab->assign("h_tank", 12.);
	data_vtab->assign("tank_pairs", 1.);
	data_vtab->assign("u_tank", 0.4);

	int errors = run_module(data, "ui_tes_calcs");
	EXPECT_FALSE(errors);

	double q_tes = data_vtab->as_number("q_tes");
	double tes_avail_vol = data_vtab->as_number("tes_avail_vol");
	double vol_tank = data_vtab->as_number("vol_tank");
	double csp_pt_tes_tank_diameter = data_vtab->as_number("csp_pt_tes_tank_diameter");
	double q_dot_tes_est = data_vtab->as_number("q_dot_tes_est");
	double csp_pt_tes_htf_density = data_vtab->as_number("csp_pt_tes_htf_density");
	ASSERT_NEAR(q_tes, 2791.3, 2791.3 * error_tolerance);
	ASSERT_NEAR(tes_avail_vol, 12986., 12986. * error_tolerance);
	ASSERT_NEAR(vol_tank, 14166., 14166. * error_tolerance);
	ASSERT_NEAR(csp_pt_tes_tank_diameter, 38.8, 38.8 * error_tolerance);
	ASSERT_NEAR(q_dot_tes_est, 0.73, 0.73 * error_tolerance);
	ASSERT_NEAR(csp_pt_tes_htf_density, 1808.48, 1808.48 * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case4b) {
    double error_tolerance = 0.01;
    ssc_data_t data = ssc_data_create();
    auto data_vtab = static_cast<var_table*>(data);

    data_vtab->assign("P_ref", 115.);
    data_vtab->assign("design_eff", 0.412);
    data_vtab->assign("tshours", 10.);
    data_vtab->assign("T_htf_hot_des", 574.);
    data_vtab->assign("T_htf_cold_des", 290.);
    data_vtab->assign("rec_htf", 17);
    std::vector<double> field_fluid_properties{ 1, 7, 0, 0, 0, 0, 0, 0, 0 };
    util::matrix_t<double> field_fl_props(1, 9, &field_fluid_properties);
    data_vtab->assign("field_fl_props", field_fl_props);
    data_vtab->assign("h_tank_min", 1.);
    data_vtab->assign("h_tank", 12.);
    data_vtab->assign("tank_pairs", 1.);
    data_vtab->assign("u_tank", 0.4);

    int errors = run_module(data, "ui_tes_calcs");
    EXPECT_FALSE(errors);

    double q_tes = data_vtab->as_number("q_tes");
    double tes_avail_vol = data_vtab->as_number("tes_avail_vol");
    double vol_tank = data_vtab->as_number("vol_tank");
    double csp_pt_tes_tank_diameter = data_vtab->as_number("csp.pt.tes.tank_diameter");
    double q_dot_tes_est = data_vtab->as_number("q_dot_tes_est");
    double csp_pt_tes_htf_density = data_vtab->as_number("csp.pt.tes.htf_density");
    ASSERT_NEAR(q_tes, 2791.3, 2791.3 * error_tolerance);
    ASSERT_NEAR(tes_avail_vol, 12986., 12986. * error_tolerance);
    ASSERT_NEAR(vol_tank, 14166., 14166. * error_tolerance);
    ASSERT_NEAR(csp_pt_tes_tank_diameter, 38.8, 38.8 * error_tolerance);
    ASSERT_NEAR(q_dot_tes_est, 0.73, 0.73 * error_tolerance);
    ASSERT_NEAR(csp_pt_tes_htf_density, 1808.48, 1808.48 * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case5) {
	double error_tolerance = 0.01;
	var_table* vd = new var_table;
	vd->assign("bop_par", 0.);
	vd->assign("bop_par_f", 1.);
	vd->assign("bop_par_0", 0.);
	vd->assign("bop_par_1", 0.483);
	vd->assign("bop_par_2", 0.);
	vd->assign("p_ref", 115.);
	vd->assign("aux_par", 0.023);
	vd->assign("aux_par_f", 1.);
	vd->assign("aux_par_0", 0.483);
	vd->assign("aux_par_1", 0.571);
	vd->assign("aux_par_2", 0.);
	vd->assign("disp_wlim_maxspec", 1.);
	vd->assign("constant", 4.);

	MSPT_System_Control_Equations(vd);

	double csp_pt_par_calc_bop = vd->lookup("csp_pt_par_calc_bop")->num;
	double csp_pt_par_calc_aux = vd->lookup("csp_pt_par_calc_aux")->num;
	double disp_wlim_max = vd->lookup("disp_wlim_max")->num;
	util::matrix_t<ssc_number_t> wlim_series = vd->lookup("wlim_series")->num;
	ASSERT_NEAR(csp_pt_par_calc_bop, 0., 0. * error_tolerance);
	ASSERT_NEAR(csp_pt_par_calc_aux, 2.78783, 2.78783 * error_tolerance);
	ASSERT_NEAR(disp_wlim_max, 0.96, 0.96 * error_tolerance);
	ASSERT_NEAR(wlim_series.ncells(), 8760, 0.);
	ASSERT_NEAR(wlim_series.at(0, 0), 960., 960. * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case5b) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("bop_par", 0.);
    vd->assign("bop_par_f", 1.);
    vd->assign("bop_par_0", 0.);
    vd->assign("bop_par_1", 0.483);
    vd->assign("bop_par_2", 0.);
    vd->assign("p_ref", 115.);
    vd->assign("aux_par", 0.023);
    vd->assign("aux_par_f", 1.);
    vd->assign("aux_par_0", 0.483);
    vd->assign("aux_par_1", 0.571);
    vd->assign("aux_par_2", 0.);
    vd->assign("disp_wlim_maxspec", 1.);
    vd->assign("constant", 4.);

    MSPT_System_Control_Equations(vd);

    double csp_pt_par_calc_bop = vd->lookup("csp.pt.par.calc.bop")->num;
    double csp_pt_par_calc_aux = vd->lookup("csp.pt.par.calc.aux")->num;
    double disp_wlim_max = vd->lookup("disp_wlim_max")->num;
    util::matrix_t<ssc_number_t> wlim_series = vd->lookup("wlim_series")->num;
    ASSERT_NEAR(csp_pt_par_calc_bop, 0., 0. * error_tolerance);
    ASSERT_NEAR(csp_pt_par_calc_aux, 2.78783, 2.78783 * error_tolerance);
    ASSERT_NEAR(disp_wlim_max, 0.96, 0.96 * error_tolerance);
    ASSERT_NEAR(wlim_series.ncells(), 8760, 0.);
    ASSERT_NEAR(wlim_series.at(0, 0), 960., 960. * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case6) {
	double error_tolerance = 0.01;
	var_table* vd = new var_table;
	vd->assign("d_rec", 17.65);
	vd->assign("rec_height", 21.60);
	vd->assign("receiver_type", 0);
	vd->assign("rec_d_spec", 15.);
	vd->assign("csp_pt_rec_cav_ap_height", 18.);
	vd->assign("p_ref", 115.);
	vd->assign("design_eff", 0.412);
	vd->assign("tshours", 10.);
	vd->assign("demand_var", 0);
	vd->assign("a_sf_ui", 1269055.);
	vd->assign("site_spec_cost", 16.);
	vd->assign("heliostat_spec_cost", 140.);
	vd->assign("cost_sf_fixed", 0.);
	vd->assign("h_tower", 193.458);
	vd->assign("rec_height", 21.6029);
	vd->assign("helio_height", 12.2);
	vd->assign("tower_fixed_cost", 3000000.);
	vd->assign("tower_exp", 0.0113);
	vd->assign("csp_pt_cost_receiver_area", 1269055.);
	vd->assign("rec_ref_cost", 103000000.);
	vd->assign("rec_ref_area", 1571.);
	vd->assign("rec_cost_exp", 0.7);
	vd->assign("csp_pt_cost_storage_mwht", 2791.26);
	vd->assign("tes_spec_cost", 22.);
	vd->assign("csp_pt_cost_power_block_mwe", 115.);
	vd->assign("plant_spec_cost", 1040.);
	vd->assign("bop_spec_cost", 290.);
	vd->assign("fossil_spec_cost", 0.);
	vd->assign("contingency_rate", 7.);
	vd->assign("csp_pt_sf_total_land_area", 1892.);
	vd->assign("nameplate", 104.);
	vd->assign("csp_pt_cost_epc_per_acre", 0.);
	vd->assign("csp_pt_cost_epc_percent", 13.);
	vd->assign("csp_pt_cost_epc_per_watt", 0.);
	vd->assign("csp_pt_cost_epc_fixed", 0.);
	vd->assign("land_spec_cost", 10000.);
	vd->assign("csp_pt_cost_plm_percent", 0.);
	vd->assign("csp_pt_cost_plm_per_watt", 0.);
	vd->assign("csp_pt_cost_plm_fixed", 0.);
	vd->assign("sales_tax_frac", 80.);
	vd->assign("sales_tax_rate", 5.);

	Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd);

	double csp_pt_cost_receiver_area = vd->lookup("csp_pt_cost_receiver_area")->num;
	double csp_pt_cost_storage_mwht = vd->lookup("csp_pt_cost_storage_mwht")->num;
	double csp_pt_cost_power_block_mwe = vd->lookup("csp_pt_cost_power_block_mwe")->num;
	double csp_pt_cost_site_improvements = vd->lookup("csp_pt_cost_site_improvements")->num;
	double csp_pt_cost_heliostats = vd->lookup("csp_pt_cost_heliostats")->num;
	double csp_pt_cost_tower = vd->lookup("csp_pt_cost_tower")->num;
	double csp_pt_cost_receiver = vd->lookup("csp_pt_cost_receiver")->num;
	double csp_pt_cost_storage = vd->lookup("csp_pt_cost_storage")->num;
	double csp_pt_cost_power_block = vd->lookup("csp_pt_cost_power_block")->num;
	double csp_pt_cost_bop = vd->lookup("csp_pt_cost_bop")->num;
	double csp_pt_cost_fossil = vd->lookup("csp_pt_cost_fossil")->num;
	double ui_direct_subtotal = vd->lookup("ui_direct_subtotal")->num;
	double csp_pt_cost_contingency = vd->lookup("csp_pt_cost_contingency")->num;
	double total_direct_cost = vd->lookup("total_direct_cost")->num;
	double csp_pt_cost_epc_total = vd->lookup("csp_pt_cost_epc_total")->num;
	double csp_pt_cost_plm_total = vd->lookup("csp_pt_cost_plm_total")->num;
	double csp_pt_cost_sales_tax_total = vd->lookup("csp_pt_cost_sales_tax_total")->num;
	double total_indirect_cost = vd->lookup("total_indirect_cost")->num;
	double total_installed_cost = vd->lookup("total_installed_cost")->num;
	double csp_pt_cost_installed_per_capacity = vd->lookup("csp_pt_cost_installed_per_capacity")->num;
	ASSERT_NEAR(csp_pt_cost_receiver_area, 1197.86, 1197.86 * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_storage_mwht, 2791.26, 2791.26 * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_power_block_mwe, 115., 115. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_site_improvements, 20304872., 20304872. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_heliostats, 177667632., 177667632. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_tower, 25319156., 25319156. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_receiver, 85191944., 85191944. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_storage, 61407768., 61407768. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_power_block, 119600000., 119600000. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_bop, 33350000., 33350000. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_fossil, 0., 0. * error_tolerance);
	ASSERT_NEAR(ui_direct_subtotal, 522841376., 522841376. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_contingency, 36598896., 36598896. * error_tolerance);
	ASSERT_NEAR(total_direct_cost, 559440256., 559440256. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_epc_total, 72727232., 72727232. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_plm_total, 18920378., 18920378. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_sales_tax_total, 22377610., 22377610. * error_tolerance);
	ASSERT_NEAR(total_indirect_cost, 114025224., 114025224. * error_tolerance);
	ASSERT_NEAR(total_installed_cost, 673465472., 673465472. * error_tolerance);
	ASSERT_NEAR(csp_pt_cost_installed_per_capacity, 6506.91, 6506.91 * error_tolerance);
}

TEST(Mspt_cmod_csp_tower_eqns, Case6b) {
    double error_tolerance = 0.01;
    var_table* vd = new var_table;
    vd->assign("d_rec", 17.65);
    vd->assign("rec_height", 21.60);
    vd->assign("receiver_type", 0);
    vd->assign("rec_d_spec", 15.);
    vd->assign("csp.pt.rec.cav_ap_height", 18.);
    vd->assign("p_ref", 115.);
    vd->assign("design_eff", 0.412);
    vd->assign("tshours", 10.);
    vd->assign("demand_var", 0);
    vd->assign("a_sf_ui", 1269055.);
    vd->assign("site_spec_cost", 16.);
    vd->assign("heliostat_spec_cost", 140.);
    vd->assign("cost_sf_fixed", 0.);
    vd->assign("h_tower", 193.458);
    vd->assign("rec_height", 21.6029);
    vd->assign("helio_height", 12.2);
    vd->assign("tower_fixed_cost", 3000000.);
    vd->assign("tower_exp", 0.0113);
    vd->assign("csp.pt.cost.receiver.area", 1269055.);
    vd->assign("rec_ref_cost", 103000000.);
    vd->assign("rec_ref_area", 1571.);
    vd->assign("rec_cost_exp", 0.7);
    vd->assign("csp.pt.cost.storage_mwht", 2791.26);
    vd->assign("tes_spec_cost", 22.);
    vd->assign("csp.pt.cost.power_block_mwe", 115.);
    vd->assign("plant_spec_cost", 1040.);
    vd->assign("bop_spec_cost", 290.);
    vd->assign("fossil_spec_cost", 0.);
    vd->assign("contingency_rate", 7.);
    vd->assign("csp.pt.sf.total_land_area", 1892.);
    vd->assign("nameplate", 104.);
    vd->assign("csp.pt.cost.epc.per_acre", 0.);
    vd->assign("csp.pt.cost.epc.percent", 13.);
    vd->assign("csp.pt.cost.epc.per_watt", 0.);
    vd->assign("csp.pt.cost.epc.fixed", 0.);
    vd->assign("land_spec_cost", 10000.);
    vd->assign("csp.pt.cost.plm.percent", 0.);
    vd->assign("csp.pt.cost.plm.per_watt", 0.);
    vd->assign("csp.pt.cost.plm.fixed", 0.);
    vd->assign("sales_tax_frac", 80.);
    vd->assign("sales_tax_rate", 5.);

    Tower_SolarPilot_Capital_Costs_MSPT_Equations(vd);

    double csp_pt_cost_receiver_area = vd->lookup("csp.pt.cost.receiver.area")->num;
    double csp_pt_cost_storage_mwht = vd->lookup("csp.pt.cost.storage_mwht")->num;
    double csp_pt_cost_power_block_mwe = vd->lookup("csp.pt.cost.power_block_mwe")->num;
    double csp_pt_cost_site_improvements = vd->lookup("csp.pt.cost.site_improvements")->num;
    double csp_pt_cost_heliostats = vd->lookup("csp.pt.cost.heliostats")->num;
    double csp_pt_cost_tower = vd->lookup("csp.pt.cost.tower")->num;
    double csp_pt_cost_receiver = vd->lookup("csp.pt.cost.receiver")->num;
    double csp_pt_cost_storage = vd->lookup("csp.pt.cost.storage")->num;
    double csp_pt_cost_power_block = vd->lookup("csp.pt.cost.power_block")->num;
    double csp_pt_cost_bop = vd->lookup("csp.pt.cost.bop")->num;
    double csp_pt_cost_fossil = vd->lookup("csp.pt.cost.fossil")->num;
    double ui_direct_subtotal = vd->lookup("ui_direct_subtotal")->num;
    double csp_pt_cost_contingency = vd->lookup("csp.pt.cost.contingency")->num;
    double total_direct_cost = vd->lookup("total_direct_cost")->num;
    double csp_pt_cost_epc_total = vd->lookup("csp.pt.cost.epc.total")->num;
    double csp_pt_cost_plm_total = vd->lookup("csp.pt.cost.plm.total")->num;
    double csp_pt_cost_sales_tax_total = vd->lookup("csp.pt.cost.sales_tax.total")->num;
    double total_indirect_cost = vd->lookup("total_indirect_cost")->num;
    double total_installed_cost = vd->lookup("total_installed_cost")->num;
    double csp_pt_cost_installed_per_capacity = vd->lookup("csp.pt.cost.installed_per_capacity")->num;
    ASSERT_NEAR(csp_pt_cost_receiver_area, 1197.86, 1197.86 * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_storage_mwht, 2791.26, 2791.26 * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_power_block_mwe, 115., 115. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_site_improvements, 20304872., 20304872. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_heliostats, 177667632., 177667632. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_tower, 25319156., 25319156. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_receiver, 85191944., 85191944. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_storage, 61407768., 61407768. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_power_block, 119600000., 119600000. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_bop, 33350000., 33350000. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_fossil, 0., 0. * error_tolerance);
    ASSERT_NEAR(ui_direct_subtotal, 522841376., 522841376. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_contingency, 36598896., 36598896. * error_tolerance);
    ASSERT_NEAR(total_direct_cost, 559440256., 559440256. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_epc_total, 72727232., 72727232. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_plm_total, 18920378., 18920378. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_sales_tax_total, 22377610., 22377610. * error_tolerance);
    ASSERT_NEAR(total_indirect_cost, 114025224., 114025224. * error_tolerance);
    ASSERT_NEAR(total_installed_cost, 673465472., 673465472. * error_tolerance);
    ASSERT_NEAR(csp_pt_cost_installed_per_capacity, 6506.91, 6506.91 * error_tolerance);
}

void CopyVarTableAndGetValue(var_table* vartab, std::string var_name, double* var_value) {
    var_table vartab_copy;
    vartab_copy = *vartab;  // uses copy assignment operator, which is fine
    *var_value = vartab->as_double(var_name);
    //return vartab_copy;     // this copy constructor operator causes a problem
    return;
}

TEST(Mspt_cmod_csp_tower_eqns, VarTableCopyAssignmentOperator) {
    // Get an ssc_data_t with default input values for the molten salt tower model
    ssc_data_t data = ssc_data_create();
    tcsmolten_salt_default(data);

    // Verify var_tables can be copied by first converting the ssc_data_t to a var_table
    var_table *vartab = static_cast<var_table*>(data);

    std::string test_variable_name = "tower_exp";
    double test_value = vartab->as_double(test_variable_name);
    double test_value_from_orig_table_after_copied;

    // If the copying is not in a function like this, there may not be a problem until after the test exits.
    //var_table var_table_copy = CopyVarTableAndGetValue(vartab, test_variable_name, &test_value_from_orig_table_after_copied);
    CopyVarTableAndGetValue(vartab, test_variable_name, &test_value_from_orig_table_after_copied);

    double test_value_from_orig_table_after_copied_and_fun_returned;
    try
    {
        test_value_from_orig_table_after_copied_and_fun_returned = vartab->as_double(test_variable_name);       // throws error
     
    }
    catch (std::exception& e) {
        test_value_from_orig_table_after_copied_and_fun_returned = std::numeric_limits<double>::quiet_NaN();
    }

    ASSERT_DOUBLE_EQ(test_value, test_value_from_orig_table_after_copied);
    ASSERT_DOUBLE_EQ(test_value, test_value_from_orig_table_after_copied_and_fun_returned);
}

/// Test tcsmolten_salt with alternative condenser type: Evaporative
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Evap_Condenser_SingleOwner_cmod_tcsmolten_salt) {
//
//    ssc_data_t data = ssc_data_create();
//    int test_errors = tcsmolten_salt_daggett_evap_condenser(data);
//
//    EXPECT_FALSE(test_errors);
//    if (!test_errors)
//    {
//        ssc_number_t annual_energy;
//        ssc_data_get_number(data, "annual_energy", &annual_energy);
//        EXPECT_NEAR(annual_energy, 571408807.373179, 571408807.373179 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t land_area_base;
//        ssc_data_get_number(data, "land_area_base", &land_area_base);
//        EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t capacity_factor;
//        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//        EXPECT_NEAR(capacity_factor, 63.023494, 63.023494 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_W_cycle_gross;
//        ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//        EXPECT_NEAR(annual_W_cycle_gross, 642428580.492706, 642428580.492706 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t kwh_per_kw;
//        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//        EXPECT_NEAR(kwh_per_kw, 5520.858042, 5520.858042 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t conversion_factor;
//        ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//        EXPECT_NEAR(conversion_factor, 88.945110, 88.945110 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t N_hel;
//        ssc_data_get_number(data, "N_hel", &N_hel);
//        EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t rec_height;
//        ssc_data_get_number(data, "rec_height", &rec_height);
//        EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t A_sf;
//        ssc_data_get_number(data, "A_sf", &A_sf);
//        EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t D_rec;
//        ssc_data_get_number(data, "D_rec", &D_rec);
//        EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t annual_total_water_use;
//        ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//        EXPECT_NEAR(annual_total_water_use, 98221.126175, 98221.126175 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t csp_pt_cost_total_land_area;
//        ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//        EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        ssc_number_t h_tower;
//        ssc_data_get_number(data, "h_tower", &h_tower);
//        EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//        //ssc_number_t VARIABLE;
//        //ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//        //EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//    }
//}

/// Test tcsmolten_salt with alternative condenser type: Hybrid
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Hybrid_Condenser_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_hybrid_condenser(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 571408807.373179, 571408807.373179 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.040000, 1847.040000 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 63.023494, 63.023494 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 642428580.492706, 642428580.492706 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5520.858042, 5520.858042 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 88.945110, 88.945110 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.602900, 21.602900 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1269054.492000, 1269054.492000 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.650000, 17.650000 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 98221.126175, 98221.126175 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.040000, 1892.040000 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458000, 193.458000 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with alternative condenser type: Radiative
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Radiative_Condenser_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_radiative_condenser(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 6.11007e8, 6.11007e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 67.391, 67.391 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.68005e8, 6.68005e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5903.45, 5903.45 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 91.4676, 91.4676 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 97830.1, 97830.1 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 2362.53, 2362.53 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with alternative Location: Tucson, Arizona
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, Rankine_Location_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_Tucson_AZ(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.60538e8, 5.60538e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 61.8245, 61.8245 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.29388e8, 6.29388e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5415.83, 5415.83 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 89.0609, 89.0609 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 96449.7, 96449.7 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with power cycle alternative: User Defined
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, User_Defined_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_UD_default(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.9082e8, 5.9082e8 * m_error_tolerance_hi) << "Annual Energy";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 65.1644, 65.1644 * m_error_tolerance_hi) << "Capacity Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.4659e8, 6.4659e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5708.4, 5708.4 * m_error_tolerance_hi) << "kwh per kw";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 91.3747, 91.3747 * m_error_tolerance_hi) << "Conversion Factor";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//
//		//ssc_number_t VARIABLE;
//		//ssc_data_get_number(data, "VARIABLE", &VARIABLE);
//		//EXPECT_NEAR(VARIABLE, EXP_VAL, EXP_VAL * m_error_tolerance_hi) << "DESCRIPTION";  // choose either m_error_tolerance_lo or m_error_tolerance_hi
//	}
//}

/// Test tcsmolten_salt with alternative power cycle: Super Critical CO2
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, SCO2_Default_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_SCO2_default(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.14776e8, 5.14776e8 * m_error_tolerance_hi) << "Annual Energy";  // choose m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 56.7772, 56.7772 * m_error_tolerance_hi) << "Capacity Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.1858e8, 6.1858e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 4973.68, 4973.68 * m_error_tolerance_hi) << "kwh per kw";  // choose m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 83.219, 83.219 * m_error_tolerance_hi) << "Conversion Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";
//	}
//}

/// Test tcsmolten_salt with alternative power cycle: Super Critical CO2
/// Cycle Configuration alternative: Partial Cooling
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, SCO2_Partial_Cooling_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_SCO2_partial_cooling(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.43316e8, 5.43316e8 * m_error_tolerance_hi) << "Annual Energy";  // choose m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 59.925, 59.925 * m_error_tolerance_hi) << "Capacity Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.15469e8, 6.15469e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 5249.43, 5249.43 * m_error_tolerance_hi) << "kwh per kw";  // choose m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 88.2767, 88.2767 * m_error_tolerance_hi) << "Conversion Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";
//	}
//}

/// Test tcsmolten_salt with alternative power cycle: Super Critical CO2
/// Materials and Flow alternative: Flow pattern 2 instead of 1
/// Rest default configurations with respect to the single owner financial model
//TEST_F(CMTcsMoltenSalt, SCO2_Flow_Pattern_Alternative_SingleOwner_cmod_tcsmolten_salt) {
//
//	ssc_data_t data = ssc_data_create();
//	int test_errors = tcsmolten_salt_daggett_SCO2_flow_pattern_2(data);
//
//	EXPECT_FALSE(test_errors);
//	if (!test_errors)
//	{
//		ssc_number_t annual_energy;
//		ssc_data_get_number(data, "annual_energy", &annual_energy);
//		EXPECT_NEAR(annual_energy, 5.15291e8, 5.15291e8 * m_error_tolerance_hi) << "Annual Energy";  // choose m_error_tolerance_hi
//
//		ssc_number_t land_area_base;
//		ssc_data_get_number(data, "land_area_base", &land_area_base);
//		EXPECT_NEAR(land_area_base, 1847.04, 1847.04 * m_error_tolerance_hi) << "Land Area Base";
//
//		ssc_number_t capacity_factor;
//		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//		EXPECT_NEAR(capacity_factor, 56.834, 56.834 * m_error_tolerance_hi) << "Capacity Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t annual_W_cycle_gross;
//		ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//		EXPECT_NEAR(annual_W_cycle_gross, 6.19302e8, 6.19302e8 * m_error_tolerance_hi) << "Annual W_cycle Gross";  // choose m_error_tolerance_hi
//
//		ssc_number_t kwh_per_kw;
//		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//		EXPECT_NEAR(kwh_per_kw, 4978.66, 4978.66 * m_error_tolerance_hi) << "kwh per kw";  // choose m_error_tolerance_hi
//
//		ssc_number_t conversion_factor;
//		ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//		EXPECT_NEAR(conversion_factor, 83.7374, 83.7374 * m_error_tolerance_hi) << "Conversion Factor";  // choose m_error_tolerance_hi
//
//		ssc_number_t N_hel;
//		ssc_data_get_number(data, "N_hel", &N_hel);
//		EXPECT_NEAR(N_hel, 8790, 8790 * m_error_tolerance_hi) << "Number of Heliostats";
//
//		ssc_number_t rec_height;
//		ssc_data_get_number(data, "rec_height", &rec_height);
//		EXPECT_NEAR(rec_height, 21.6029, 21.6029 * m_error_tolerance_hi) << "Rec Height";
//
//		ssc_number_t A_sf;
//		ssc_data_get_number(data, "A_sf", &A_sf);
//		EXPECT_NEAR(A_sf, 1.26905e6, 1.26905e6 * m_error_tolerance_hi) << "Solar Field Area";
//
//		ssc_number_t D_rec;
//		ssc_data_get_number(data, "D_rec", &D_rec);
//		EXPECT_NEAR(D_rec, 17.65, 17.65 * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//		ssc_number_t annual_total_water_use;
//		ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//		EXPECT_NEAR(annual_total_water_use, 55965.3, 55965.3 * m_error_tolerance_hi) << "Annual Total Water Use";
//
//		ssc_number_t csp_pt_cost_total_land_area;
//		ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//		EXPECT_NEAR(csp_pt_cost_total_land_area, 1892.04, 1892.04 * m_error_tolerance_hi) << "Total Land Area";
//
//		ssc_number_t h_tower;
//		ssc_data_get_number(data, "h_tower", &h_tower);
//		EXPECT_NEAR(h_tower, 193.458, 193.458 * m_error_tolerance_hi) << "Tower Height";
//	}
//}

/// Test series of Advanced Combinatorial Testing System (ACTS) runs
//TEST_F(CMTcsMoltenSalt, ACTS_sCO2_recompression) {
//
//	// Outputs of 6/15 total ACTS tests for the sCO2 model
//	// The other 9 test case scenarios were unable to simulate properly
//	// on the SAM UI.
//
//	//  // ACTS pass/fail summary				F          F          P         P          P          F
//	//  // sCO2 ACTS Test Cases                 2          4          5         7         12         15
//	//  std::vector<double> annual_energys{ 4.47253e8, 4.83719e8, 5.3984e8, 5.29801e8, 5.12115e8, 4.648e8 };
//	//  std::vector<double> land_area_bases{ 1847.04, 1847.04, 1847.04, 1847.04, 1847.04, 1847.04 };
//	//  std::vector<double> capacity_factors{ 49.3297, 53.3518, 59.5417, 58.4344, 56.4837, 51.2651 };
//	//  std::vector<double> annual_W_cycle_grosss{ 5.20554e8, 6.03866e8, 6.21568e8, 6.29351e8, 6.31792e8, 5.49548e8 };
//	//  std::vector<double> kwh_per_kws{ 4321.28, 4673.62, 5215.85, 5118.85, 4947.98, 4490.82 };
//	//  std::vector<double> conversion_factors{ 85.9187, 80.1037, 86.8513, 84.1821, 81.0576, 84.5787 };
//	//  std::vector<double> N_hels{ 8790, 8790, 8790, 8790, 8790, 8790 };
//	//  std::vector<double> rec_heights{ 21.6029, 21.6029, 21.6029, 21.6029, 21.6029, 21.6029 };
//	//  std::vector<double> A_sfs{ 1.26905e6, 1.26905e6, 1.26905e6, 1.26905e6, 1.26905e6, 1.26905e6 };
//	//  std::vector<double> D_recs{ 17.65, 17.65, 17.65, 17.65, 17.65, 17.65 };
//	//  std::vector<double> annual_total_water_uses{ 55965.3, 55965.3, 55965.3, 55965.3, 55965.3, 55965.3 };
//	//  std::vector<double> csp_pt_cost_total_land_areas{ 1892.04, 1892.04, 1892.04, 1892.04, 1892.04, 1892.04 };
//	//  std::vector<double> h_towers{ 193.458, 193.458, 193.458, 193.458, 193.458, 193.458 };
//
//	// Passing ACTS configurations
//	// sCO2 ACTS Test Cases                 5         7         12
//	std::vector<double> annual_energys{ 5.3984e8, 5.29801e8, 5.12115e8 };
//	std::vector<double> land_area_bases{ 1847.04, 1847.04, 1847.04 };
//	std::vector<double> capacity_factors{ 59.5417, 58.4344, 56.4837 };
//	std::vector<double> annual_W_cycle_grosss{ 6.21568e8, 6.29351e8, 6.31792e8 };
//	std::vector<double> kwh_per_kws{ 5215.85, 5118.85, 4947.98 };
//	std::vector<double> conversion_factors{ 86.8513, 84.1821, 81.0576 };
//	std::vector<double> N_hels{ 8790, 8790, 8790 };
//	std::vector<double> rec_heights{ 21.6029, 21.6029, 21.6029 };
//	std::vector<double> A_sfs{ 1.26905e6, 1.26905e6, 1.26905e6 };
//	std::vector<double> D_recs{ 17.65, 17.65, 17.65 };
//	std::vector<double> annual_total_water_uses{ 55965.3, 55965.3, 55965.3 };
//	std::vector<double> csp_pt_cost_total_land_areas{ 1892.04, 1892.04, 1892.04 };
//	std::vector<double> h_towers{ 193.458, 193.458, 193.458 };
//
//	ssc_data_t data = ssc_data_create();
//
//	for (std::vector<double>::size_type i = 0; i != annual_energys.size(); i++) {
//		int test_errors = ACTS_sCO2_testing(data, i);
//
//		EXPECT_FALSE(test_errors);
//		if (!test_errors)
//		{
//			ssc_number_t annual_energy;
//			ssc_data_get_number(data, "annual_energy", &annual_energy);
//			EXPECT_NEAR(annual_energy, annual_energys[i], annual_energys[i] * m_error_tolerance_hi) << "Annual Energy"; // choose m_error_tolerance_hi
//
//			ssc_number_t land_area_base;
//			ssc_data_get_number(data, "land_area_base", &land_area_base);
//			EXPECT_NEAR(land_area_base, land_area_bases[i], land_area_bases[i] * m_error_tolerance_hi) << "Land Area Base";
//
//			ssc_number_t capacity_factor;
//			ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//			EXPECT_NEAR(capacity_factor, capacity_factors[i], capacity_factors[i] * m_error_tolerance_hi) << "Capacity Factor"; // choose m_error_tolerance_hi
//
//			ssc_number_t annual_W_cycle_gross;
//			ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//			EXPECT_NEAR(annual_W_cycle_gross, annual_W_cycle_grosss[i], annual_W_cycle_grosss[i] * m_error_tolerance_hi) << "Annual W_cycle Gross"; // choose m_error_tolerance_hi
//
//			ssc_number_t kwh_per_kw;
//			ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//			EXPECT_NEAR(kwh_per_kw, kwh_per_kws[i], kwh_per_kws[i] * m_error_tolerance_hi) << "kwh per kw"; // choose m_error_tolerance_hi
//
//			ssc_number_t conversion_factor;
//			ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//			EXPECT_NEAR(conversion_factor, conversion_factors[i], conversion_factors[i] * m_error_tolerance_hi) << "Conversion Factor"; // choose m_error_tolerance_hi
//
//			ssc_number_t N_hel;
//			ssc_data_get_number(data, "N_hel", &N_hel);
//			EXPECT_NEAR(N_hel, N_hels[i], N_hels[i] * m_error_tolerance_hi) << "Number of Heliostats";
//
//			ssc_number_t rec_height;
//			ssc_data_get_number(data, "rec_height", &rec_height);
//			EXPECT_NEAR(rec_height, rec_heights[i], rec_heights[i] * m_error_tolerance_hi) << "Rec Height";
//
//			ssc_number_t A_sf;
//			ssc_data_get_number(data, "A_sf", &A_sf);
//			EXPECT_NEAR(A_sf, A_sfs[i], A_sfs[i] * m_error_tolerance_hi) << "Solar Field Area";
//
//			ssc_number_t D_rec;
//			ssc_data_get_number(data, "D_rec", &D_rec);
//			EXPECT_NEAR(D_rec, D_recs[i], D_recs[i] * m_error_tolerance_hi) << "Receiver Outer Diameter";
//
//			ssc_number_t annual_total_water_use;
//			ssc_data_get_number(data, "annual_total_water_use", &annual_total_water_use);
//			EXPECT_NEAR(annual_total_water_use, annual_total_water_uses[i], annual_total_water_uses[i] * m_error_tolerance_hi) << "Annual Total Water Use";
//
//			ssc_number_t csp_pt_cost_total_land_area;
//			ssc_data_get_number(data, "csp.pt.cost.total_land_area", &csp_pt_cost_total_land_area);
//			EXPECT_NEAR(csp_pt_cost_total_land_area, csp_pt_cost_total_land_areas[i], csp_pt_cost_total_land_areas[i] * m_error_tolerance_hi) << "Total Land Area";
//
//			ssc_number_t h_tower;
//			ssc_data_get_number(data, "h_tower", &h_tower);
//			EXPECT_NEAR(h_tower, h_towers[i], h_towers[i] * m_error_tolerance_hi) << "Tower Height";
//
//		}
//	}
//}
