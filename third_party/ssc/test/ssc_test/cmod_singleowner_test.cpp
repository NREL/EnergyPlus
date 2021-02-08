
#include "cmod_singleowner_test.h"
#include "cmod_financial_eqns.h"

#include "gtest/gtest.h"

TEST_F(CMSingleOwner, ResidentialDefault_cmod_swh) {

    int errors = run_module(data, "singleowner");
    ASSERT_EQ(errors, 0);

    ssc_number_t npv;
    ssc_data_get_number(data, "project_return_aftertax_npv", &npv);
    EXPECT_NEAR(npv, -647727751.2, 0.1);

}

TEST(Mspt_cmod_financial_eqns, Case1) {
	double error_tolerance = 0.01;
	var_table* vd = new var_table;
	vd->assign("total_installed_cost", 673465536.);

	vd->assign("const_per_percent1", 100.);
	vd->assign("const_per_upfront_rate1", 1.);
	vd->assign("const_per_months1", 24.);
	vd->assign("const_per_interest_rate1", 4.);

	vd->assign("const_per_percent2", 0.);
	vd->assign("const_per_upfront_rate2", 0.);
	vd->assign("const_per_months2", 0.);
	vd->assign("const_per_interest_rate2", 0.);

	vd->assign("const_per_percent3", 0.);
	vd->assign("const_per_upfront_rate3", 0.);
	vd->assign("const_per_months3", 0.);
	vd->assign("const_per_interest_rate3", 0.);

	vd->assign("const_per_percent4", 0.);
	vd->assign("const_per_upfront_rate4", 0.);
	vd->assign("const_per_months4", 0.);
	vd->assign("const_per_interest_rate4", 0.);

	vd->assign("const_per_percent5", 0.);
	vd->assign("const_per_upfront_rate5", 0.);
	vd->assign("const_per_months5", 0.);
	vd->assign("const_per_interest_rate5", 0.);

	Financial_Construction_Financing_Equations(vd);

	double const_per_principal1 = vd->lookup("const_per_principal1")->num;
	double const_per_interest1 = vd->lookup("const_per_interest1")->num;
	double const_per_total1 = vd->lookup("const_per_total1")->num;

	double const_per_principal2 = vd->lookup("const_per_principal2")->num;
	double const_per_interest2 = vd->lookup("const_per_interest2")->num;
	double const_per_total2 = vd->lookup("const_per_total2")->num;

	double const_per_principal3 = vd->lookup("const_per_principal3")->num;
	double const_per_interest3 = vd->lookup("const_per_interest3")->num;
	double const_per_total3 = vd->lookup("const_per_total3")->num;

	double const_per_principal4 = vd->lookup("const_per_principal4")->num;
	double const_per_interest4 = vd->lookup("const_per_interest4")->num;
	double const_per_total4 = vd->lookup("const_per_total4")->num;

	double const_per_principal5 = vd->lookup("const_per_principal5")->num;
	double const_per_interest5 = vd->lookup("const_per_interest5")->num;
	double const_per_total5 = vd->lookup("const_per_total5")->num;

	double const_per_principal_total = vd->lookup("const_per_principal_total")->num;
	double const_per_percent_total = vd->lookup("const_per_percent_total")->num;
	double construction_financing_cost = vd->lookup("construction_financing_cost")->num;
	double const_per_interest_total = vd->lookup("const_per_interest_total")->num;

	ASSERT_NEAR(const_per_principal1, 673465472., 673465472. * error_tolerance);
	ASSERT_NEAR(const_per_interest1, 26938618., 26938618. * error_tolerance);
	ASSERT_NEAR(const_per_total1, 33673272., 33673272. * error_tolerance);

	ASSERT_NEAR(const_per_principal2, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_interest2, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_total2, 0., 0. * error_tolerance);

	ASSERT_NEAR(const_per_principal3, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_interest3, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_total3, 0., 0. * error_tolerance);

	ASSERT_NEAR(const_per_principal4, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_interest4, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_total4, 0., 0. * error_tolerance);

	ASSERT_NEAR(const_per_principal5, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_interest5, 0., 0. * error_tolerance);
	ASSERT_NEAR(const_per_total5, 0., 0. * error_tolerance);

	ASSERT_NEAR(const_per_percent_total, 100., 100. * error_tolerance);
	ASSERT_NEAR(const_per_principal_total, 673465472., 673465472. * error_tolerance);
	ASSERT_NEAR(const_per_interest_total, 26938618., 26938618. * error_tolerance);
	ASSERT_NEAR(construction_financing_cost, 33673272., 33673272. * error_tolerance);
}

TEST(Mspt_cmod_financial_eqns, Case2) {
	double error_tolerance = 0.01;
	var_table* vd = new var_table;
	vd->assign("system_capacity", 103500.);

	Financial_Capacity_Payments_Equations(vd);

	double cp_system_nameplate = vd->lookup("cp_system_nameplate")->num;
	ASSERT_NEAR(cp_system_nameplate, 103.5, 103.5 * error_tolerance);
}
