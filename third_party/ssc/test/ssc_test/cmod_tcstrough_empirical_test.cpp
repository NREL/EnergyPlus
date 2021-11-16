#include <gtest/gtest.h>
#include "tcstrough_empirical_defaults.h"
#include "csp_common_test.h"
#include "vs_google_test_explorer_namespace.h"

namespace csp_trough {}
using namespace csp_trough;

//========Tests===================================================================================
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, Default_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 344049128, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 39.31, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 403814011, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3444, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 88.75, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Alternative solar field HTF type: Hitec Solar Salt
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, SolarSaltHtf_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    empirical_trough.SetInput("HTFFluid", 18);
    empirical_trough.SetInput("PTSmax", 676.471);
    empirical_trough.SetInput("PFSmax", 342.699);

    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 340377809, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 38.89, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 399556634, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3407, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 88.74, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Alternative solar collector assembly (SCA): EuroTrough ET150
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, EuroTrough_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    empirical_trough.SetInput("Solar_Field_Area", 856740);
    empirical_trough.SetInput("Ave_Focal_Length", 2.1);
    empirical_trough.SetInput("ScaLen", 150);
    empirical_trough.SetInput("SCA_aper", 5.75);
    empirical_trough.SetInput("TrkTwstErr", 0.99);
    empirical_trough.SetInput("MirCln", 0.97);
    empirical_trough.SetInput("HCEdust", { 0.98, 0.98, 0.98, 0.98 });
    empirical_trough.SetInput("RefMirrAper", { 5.75, 5.75, 5.75, 5.75 });
    empirical_trough.SetInput("SfPar", 0.228);
    empirical_trough.SetInput("ChtfPar", 9.013);
    empirical_trough.SetInput("AntiFrPar", 0.9013);

    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 343687390, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 39.27, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 402926405, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3440, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 88.85, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Alternative heat collection element (HCE): Luz Cermet Vacuum, Luz Cermet Hydrogren, and Luz Cermet Broken Glass
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, LuzCermetHce_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    empirical_trough.SetInput("Solar_Field_Area", 1015848);
    empirical_trough.SetInput("HCEBelShad", { 0.971, 0.971, 0.971, 0 });
    empirical_trough.SetInput("HCEEnvTrans", { 0.935, 0.935, 1, 0 });
    empirical_trough.SetInput("HCEabs", { 0.925, 0.925, 0.8, 0 });
    empirical_trough.SetInput("PerfFac", { 1.25, 1.25, 1.25, 0 });
    empirical_trough.SetInput("HCE_A0", { 2.424, 7.0233, 100.05, 0 });
    empirical_trough.SetInput("HCE_A1", { 0.214, 1.275, -0.7351, 0 });
    empirical_trough.SetInput("HCE_A2", { -0.00047461, 0.0015105, -0.008635, 0 });
    empirical_trough.SetInput("HCE_A3", { 6.88e-06, 5.05e-06, 2.67e-05, 0 });
    empirical_trough.SetInput("HCE_A4", { 9.62e-08, 7.03e-08, 6.65e-07, 0 });
    empirical_trough.SetInput("HCE_A5", { -2.2423, -4.284, -99.043, 0 });
    empirical_trough.SetInput("HCE_A6", { 0.032325, 0.39685, 5.1672, 0 });
    empirical_trough.SetInput("SfPar", 0.270);
    empirical_trough.SetInput("ChtfPar", 10.687);
    empirical_trough.SetInput("AntiFrPar", 1.069);

    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 310891768, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 35.53, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 369630914, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3112, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 87.61, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Alternative power cycle: APS Ormat 1MWe 300C
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, PowerCycle_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    empirical_trough.SetInput("Solar_Field_Area", 1599020);
    empirical_trough.SetInput("TurbEffG", 0.2071);
    empirical_trough.SetInput("TurSUE", 0.05);
    empirical_trough.SetInput("T2EPLF0", -0.1594);
    empirical_trough.SetInput("T2EPLF1", 0.9262);
    empirical_trough.SetInput("T2EPLF2", 1.1349);
    empirical_trough.SetInput("T2EPLF3", -1.3606);
    empirical_trough.SetInput("T2EPLF4", 0.4588);
    empirical_trough.SetInput("E2TPLF0", 0.1492);
    empirical_trough.SetInput("E2TPLF1", 0.8522);
    empirical_trough.SetInput("E2TPLF2", -0.3247);
    empirical_trough.SetInput("E2TPLF3", 0.44863);
    empirical_trough.SetInput("E2TPLF4", -0.1256);
    empirical_trough.SetInput("TempCorrF", 0);
    empirical_trough.SetInput("TempCorr0", 1);
    empirical_trough.SetInput("TempCorr1", 0);
    empirical_trough.SetInput("TempCorr2", 0);
    empirical_trough.SetInput("TempCorr3", 0);
    empirical_trough.SetInput("TempCorr4", 0);
    empirical_trough.SetInput("PTSmax", 535.973);
    empirical_trough.SetInput("PFSmax", 543.0467);
    empirical_trough.SetInput("SfPar", 0.42534);
    empirical_trough.SetInput("ChtfPar", 16.8217);
    empirical_trough.SetInput("AntiFrPar", 1.6822);

    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 350313341, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 40.03, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 427283458, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3507, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 85.40, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Alternative thermal stoage fluid type: Therminol VP-1
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, TherminolHtf_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    empirical_trough.SetInput("PTSmax", 676.4706);
    empirical_trough.SetInput("PFSmax", 342.699);

    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 343856219, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 39.29, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 403443026, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3444, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 88.78, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Alternative parasitic electric energy use
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, ParasiticElectric_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    empirical_trough.SetInput("SfPar", 0.117);
    empirical_trough.SetInput("SfParPF", 0.5);
    empirical_trough.SetInput("ChtfPar", 3.231);
    empirical_trough.SetInput("ChtfParPF", 0.35);
    empirical_trough.SetInput("AntiFrPar", 0.323);
    empirical_trough.SetInput("HhtfPar", 0.777);
    empirical_trough.SetInput("HhtfParPF", 0.35);

    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 357850265, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 40.89, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 403814011, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3582, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 92.31, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Alternative location: Phoenix, AZ
NAMESPACE_TEST(csp_trough, EmpiricalTroughCmod, Phoenix_NoFinancial)
{
    ssc_data_t defaults = tcstrough_empirical_defaults();
    CmodUnderTest empirical_trough = CmodUnderTest("tcstrough_empirical", defaults);
    char solar_resource_path_phoenix[512];
    int n = sprintf(solar_resource_path_phoenix, "%s/test/input_cases/trough_empirical_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy.csv", std::getenv("SSCDIR"));
    empirical_trough.SetInput("file_name", solar_resource_path_phoenix);

    int errors = empirical_trough.RunModule();
    EXPECT_FALSE(errors);
    if (!errors) {
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_energy"), 332549669, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("annual_fuel_usage"), 0., kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("capacity_factor"), 38.00, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("annual_W_cycle_gross"), 391115710, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("kwh_per_kw"), 3329, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("conversion_factor"), 88.56, kErrorToleranceLo);
        EXPECT_NEAR_FRAC(empirical_trough.GetOutput("system_heat_rate"), 3.413, kErrorToleranceLo);
        EXPECT_NEAR(empirical_trough.GetOutput("system_use_lifetime_output"), 0., kErrorToleranceLo);
    }
}

// Test series of Advanced Combinatorial Testing System (ACTS) runs
// Parabolic trough (empirical)
//int ACTS_test_empirical(ssc_data_t &data, int test_case)
//{
//	tcstrough_empirical_default(data);
//
//	// Testing level to vector index map
//	std::unordered_map<int, int> idx =
//	{
//		{-1, 0},
//		{ 0, 1},
//		{ 1, 2}
//	};
//
//	// Parameter test range values
//	std::vector<double> Distance_SCA_vals{ 0.5, 1 , 2 };				// Distance Between SCAs in Row		// SAM SSC - "Distance_SCA"			 
//	std::vector<double> NumScas_vals{ 1, 4, 8 };			            // Number of SCAs per Row			// SAM SSC - "NumScas"
//	std::vector<double> DepAngle_vals{ 5, 10, 20 };						// Deploy Angle						// SAM SSC - "DepAngle"	 
//	std::vector<double> Stow_Angle_vals{ 160, 170, 175 };			    // Stow Angle					    // SAM SSC - "Stow_Angle"
//
//	// ACTS transposed covering array           1   2   3   4   5  6   7   8  9             
//	       std::vector<int> Distance_SCA_lvls{ -1, -1, -1,  0,  0, 0,  1,  1, 1 };
//	            std::vector<int> NumScas_lvls{ -1,  0,  1, -1,  0, 1, -1,  0, 1 };
//	           std::vector<int> DepAngle_lvls{  0,  1, -1,  1, -1, 0, -1,  0, 1 };
//	         std::vector<int> Stow_Angle_lvls{  0,  1, -1, -1,  0, 1,  1, -1, 0 };
//
//	// Get test case values from index
//	double Distance_SCA_ACTS = Distance_SCA_vals.at(idx.find(Distance_SCA_lvls.at(test_case))->second);
//	double NumScas_ACTS = NumScas_vals.at(idx.find(NumScas_lvls.at(test_case))->second);
//	double DepAngle_ACTS = DepAngle_vals.at(idx.find(DepAngle_lvls.at(test_case))->second);
//	double Stow_Angle_ACTS = Stow_Angle_vals.at(idx.find(Stow_Angle_lvls.at(test_case))->second);
//
//	// Assigning values to variables 
//	ssc_data_set_number(data, "Distance_SCA", Distance_SCA_ACTS);
//	ssc_data_set_number(data, "NumScas", NumScas_ACTS);
//	ssc_data_set_number(data, "DepAngle", DepAngle_ACTS);
//	ssc_data_set_number(data, "Stow_Angle", Stow_Angle_ACTS);
//
//	int status = run_module(data, "tcstrough_empirical");
//
//	return status;
//}


/// Test series of Advanced Combinatorial Testing System (ACTS) runs 
//TEST_F(CMTcsTroughEmpirical, ACTS_Test_Empirical) {
//
//	std::vector<double> annual_energys{ 3.43731e8, 3.29297e8, 3.30659e8, 3.10436e8, 3.45672e8, 3.46698e8, 3.4736e8, 3.28189e8, 3.25897e8 };
//	std::vector<double> annual_fuel_usages{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
//	std::vector<double> capacity_factors{ 39.278, 37.6286, 37.7843, 35.4734, 39.4998, 39.617, 39.6926, 37.502, 37.2401 };
//	std::vector<double> annual_W_cycle_grosss{ 4.03426e8, 3.87146e8, 3.88914e8, 3.66079e8, 4.05593e8, 4.06767e8, 4.07472e8, 3.86119e8, 3.83311e8 };
//	std::vector<double> kwh_per_kws{ 3440.76, 3296.27, 3309.9, 3107.47, 3460.18, 3470.45, 3477.07, 3285.17, 3262.23 };
//	std::vector<double> conversion_factors{ 88.7533, 88.6016, 88.5637, 88.3337, 88.7773, 88.784, 88.7995, 88.5383, 88.5641 };
//	std::vector<double> system_heat_rates{ 3.413, 3.413, 3.413, 3.413, 3.413, 3.413, 3.413, 3.413, 3.413 };
//	std::vector<double> system_use_lifetime_outputs{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
//
//	ssc_data_t data = ssc_data_create();
//
//	for (std::vector<double>::size_type i = 0; i != annual_energys.size(); i++) {
//		int test_errors = ACTS_test_empirical(data, i);
//
//		EXPECT_FALSE(test_errors);
//		if (!test_errors)
//		{
//			ssc_number_t annual_energy;
//			ssc_data_get_number(data, "annual_energy", &annual_energy);
//			EXPECT_NEAR(annual_energy, annual_energys[i], annual_energys[i] * m_error_tolerance_hi) << "Annual Energy";
//
//			ssc_number_t annual_fuel_usage;
//			ssc_data_get_number(data, "annual_fuel_usage", &annual_fuel_usage);
//			EXPECT_NEAR(annual_fuel_usage, annual_fuel_usages[i], annual_fuel_usages[i] * m_error_tolerance_hi) << "Annual fuel usage";
//
//			ssc_number_t capacity_factor;
//			ssc_data_get_number(data, "capacity_factor", &capacity_factor);
//			EXPECT_NEAR(capacity_factor, capacity_factors[i], capacity_factors[i] * m_error_tolerance_hi) << "Capacity Factor";
//
//			ssc_number_t annual_W_cycle_gross;
//			ssc_data_get_number(data, "annual_W_cycle_gross", &annual_W_cycle_gross);
//			EXPECT_NEAR(annual_W_cycle_gross, annual_W_cycle_grosss[i], annual_W_cycle_grosss[i] * m_error_tolerance_hi) << "Annual W_cycle Gross";
//
//			ssc_number_t kwh_per_kw;
//			ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
//			EXPECT_NEAR(kwh_per_kw, kwh_per_kws[i], kwh_per_kws[i] * m_error_tolerance_hi) << "kwh per kw";
//
//			ssc_number_t conversion_factor;
//			ssc_data_get_number(data, "conversion_factor", &conversion_factor);
//			EXPECT_NEAR(conversion_factor, conversion_factors[i], conversion_factors[i] * m_error_tolerance_hi) << "Conversion Factor";
//
//			ssc_number_t system_heat_rate;
//			ssc_data_get_number(data, "system_heat_rate", &system_heat_rate);
//			EXPECT_NEAR(system_heat_rate, system_heat_rates[i], system_heat_rates[i] * m_error_tolerance_hi) << "System heat rate";
//
//			ssc_number_t system_use_lifetime_output;
//			ssc_data_get_number(data, "system_use_lifetime_output", &system_use_lifetime_output);
//			EXPECT_NEAR(system_use_lifetime_output, system_use_lifetime_outputs[i], system_use_lifetime_outputs[i] * m_error_tolerance_hi) << "Use lifetime output";
//		}
//	}
//}
