#include <gtest/gtest.h>

#include "cmod_pvyield_test.h"
#include "../input_cases/pvyield_cases.h"
#include "../input_cases/weather_inputs.h"

using std::cout;
using std::endl;

/// Test PVSAMv1 with inputs from PVYield
TEST_F(CMPvYieldTimo, DefaultTimoModel_cmod_pvsamv1)
{
    pvyield_no_financial_meteo(data);

    int pvsam_errors = pvyield_test(data);
    EXPECT_FALSE(pvsam_errors);
    //printf("ssc version %d build information %s", ssc_version(), ssc_build_info());

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 7380478, 7380478e-4) << "Annual energy.";

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 20.219496, m_error_tolerance_lo) << "Capacity factor";

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 1764.669, m_error_tolerance_hi) << "Energy yield";

        ssc_number_t performance_ratio;
        ssc_data_get_number(data, "performance_ratio", &performance_ratio);
        EXPECT_NEAR(performance_ratio, -14.485646, m_error_tolerance_lo) << "Energy yield";
    }
}

/// Test PVSAMv1 with inputs from PVYield and user support 80603 with meteo weather file
TEST_F(CMPvYieldTimo, TimoModel80603_meteo_cmod_pvsamv1)
{
    // first set of results for Phoenix and second set for meteo weather file.
    pvyield_user_support_80603_meteo(data);

    int pvsam_errors = pvyield_test_user_support_80603_meteo(data);
    EXPECT_FALSE(pvsam_errors);
    //printf("ssc version %d build information %s", ssc_version(), ssc_build_info());

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 7441557, 7441557e-4) << "Annual energy.";

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 20.399, m_error_tolerance_lo) << "Capacity factor";

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 1779.27, m_error_tolerance_hi) << "Energy yield";

        ssc_number_t performance_ratio;
        ssc_data_get_number(data, "performance_ratio", &performance_ratio);
        EXPECT_NEAR(performance_ratio, -14.6145, m_error_tolerance_lo) << "Energy yield";
    }
}

/// Test PVSAMv1 with inputs from PVYield and user support 80603 AZ weather file
TEST_F(CMPvYieldTimo, TimoModel80603_AZ_cmod_pvsamv1)
{
    // first set of results for Phoenix and second set for meteo weather file.
    pvyield_user_support_80603_AZ(data);

    int pvsam_errors = pvyield_test_user_support_80603_AZ(data);
    EXPECT_FALSE(pvsam_errors);
    //printf("ssc version %d build information %s", ssc_version(), ssc_build_info());

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 8198434, 8198434e-4) << "Annual energy.";

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 22.456, m_error_tolerance_lo) << "Capacity factor";

        ssc_number_t kwh_per_kw;
        ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
        EXPECT_NEAR(kwh_per_kw, 1960.46, m_error_tolerance_hi) << "Energy yield";

        ssc_number_t performance_ratio;
        ssc_data_get_number(data, "performance_ratio", &performance_ratio);
        EXPECT_NEAR(performance_ratio, -14.105, m_error_tolerance_lo) << "Energy yield";
    }
}


/// Test PVSAMv1 with default no-financial model and sytem design page changes
TEST_F(CMPvYieldTimo, NoFinancialModelSystemDesign_cmod_pvsamv1)
{
    pvsamMPPT_nofinancial_default(data);

    // Specify modules and inverters with tracking options
    // Tracking options: Fixed, 1-axis, 2-axis, Azimuth Axis, Seasonal Tilt
    std::map<std::string, double> pairs;
    pairs["subarray1_modules_per_string"] = 6;
    pairs["subarray2_modules_per_string"] = 6;
    pairs["subarray3_modules_per_string"] = 6;
    pairs["subarray4_modules_per_string"] = 6;
    pairs["subarray1_nstrings"] = 49;
    pairs["inverter_count"] = 22;
    pairs["subarray1_track_mode"] = 0;

    std::vector<double> annual_energy_expected = { 183183, 242368, 258372, 216129, 192903 };
    for (int tracking_option = 0; tracking_option != 5; tracking_option++)
    {
        // update tracking option
        pairs["subarray1_track_mode"] = (double)tracking_option;
        int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
        EXPECT_FALSE(pvsam_errors);
        if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, annual_energy_expected[tracking_option], m_error_tolerance_hi) << "Annual energy.";
            //cout << "-----------------------------------------------------" << endl;
            //cout << i << " of " << annual_energy_expected.size() << " track mode Computed annual energy : " << annual_energy << endl;
            //cout << i << " of " << annual_energy_expected.size() << " track mode Expected annual energy : " << annual_energy_expected[i] << endl;
            //cout << "-----------------------------------------------------" << endl;


        }
    }

    // Test fixed-tilt with backtracking
    pairs["subarray1_track_mode"] = 1;
    pairs["subarray1_backtrack"] = 1;

	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 237115, m_error_tolerance_hi) << "Annual energy.";
		//cout << "-----------------------------------------------------" << endl;
		//cout << "Fixed tilt backtracking Computed annual energy : " << annual_energy << endl;
		//cout << "Fixed tilt backtracking  Expected annual energy : " << 237340 << endl;
		//cout << "-----------------------------------------------------" << endl;

    }

    // Test multiple sub-arrays with different tracking, tilt, azimuth, gcr, tracker rotation limit
    pairs["subarray1_nstrings"] = 14;
    pairs["subarray2_enable"] = 1;
    pairs["subarray2_nstrings"] = 15;
    pairs["subarray3_enable"] = 1;
    pairs["subarray3_nstrings"] = 10;
    pairs["subarray4_enable"] = 1;
    pairs["subarray4_nstrings"] = 10;

	annual_energy_expected.clear();
	std::vector<double> subarray1_azimuth = { 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> subarray2_azimuth = { 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> subarray3_azimuth = { 180, 180, 180, 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> subarray4_azimuth = { 180, 180, 180, 180, 180, 180, 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> enable_mismatch = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray1_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray2_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray3_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray4_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray1_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray2_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray3_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray4_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray1_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray2_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray3_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray4_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray1_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray2_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray3_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0 };
	std::vector<double> subarray4_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4 };
	annual_energy_expected = { 167338, 176266, 183183, 166198, 175768, 183183, 171896, 178257, 183183, 171896, 178257,
                               183183, 183183, 183175, 183183, 183183, 183183, 183183, 183183, 183183, 183183, 183183,
                               183183, 183183, 183183, 183183, 177254, 182865, 162398, 176827, 182840, 160903, 178957,
                               182963, 168372, 178957, 182963, 168372, 183183, 183183, 183183, 183183, 183183, 198689,
                               205087, 192620, 186025, 183183, 201406, 206647, 193294, 186227, 183183, 195336, 198838,
                               189925, 185216, 183183, 195336, 198838, 189925, 185216 };

	for (size_t i = 0; i != annual_energy_expected.size(); i++)
	{
		pairs["enable_mismatch_vmax_calc"] = enable_mismatch[i];
		pairs["subarray1_azimuth"] = subarray1_azimuth[i];
		pairs["subarray2_azimuth"] = subarray2_azimuth[i];
		pairs["subarray3_azimuth"] = subarray3_azimuth[i];
		pairs["subarray4_azimuth"] = subarray4_azimuth[i];
		pairs["subarray1_gcr"] = subarray1_gcr[i];
		pairs["subarray2_gcr"] = subarray2_gcr[i];
		pairs["subarray3_gcr"] = subarray3_gcr[i];
		pairs["subarray4_gcr"] = subarray4_gcr[i];
		pairs["subarray1_tilt"] = subarray1_tilt[i];
		pairs["subarray2_tilt"] = subarray2_tilt[i];
		pairs["subarray3_tilt"] = subarray3_tilt[i];
		pairs["subarray4_tilt"] = subarray4_tilt[i];
		pairs["subarray1_rotlim"] = subarray1_rotlim[i];
		pairs["subarray2_rotlim"] = subarray2_rotlim[i];
		pairs["subarray3_rotlim"] = subarray3_rotlim[i];
		pairs["subarray4_rotlim"] = subarray4_rotlim[i];
		pairs["subarray1_track_mode"] = subarray1_track_mode[i];
		pairs["subarray2_track_mode"] = subarray2_track_mode[i];
		pairs["subarray3_track_mode"] = subarray3_track_mode[i];
		pairs["subarray4_track_mode"] = subarray4_track_mode[i];

		pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
		EXPECT_FALSE(pvsam_errors);
		if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, annual_energy_expected[i], m_error_tolerance_hi) << "Index: " << i;

            //cout << "-----------------------------------------------------" << endl;
            //cout << i << " of " << annual_energy_expected.size() << " Computed annual energy : " << annual_energy << endl;
            //cout << i << " of " << annual_energy_expected.size() << " Expected annual energy : " << annual_energy_expected[i] << endl;
            //cout << "-----------------------------------------------------" << endl;

        }
    }
}
