#include <string>
#include <vector>
#include <memory>
#include <cmath>

#include <gtest/gtest.h>
#include "../input_cases/weather_inputs.h"

#include "../ssc/common.h"
#include "../tcs/csp_solver_core.h"
#include "../tcs/csp_solver_mspt_receiver_222.h"
#include "../tcs/csp_solver_mspt_collector_receiver.h"
#include "../tcs/csp_solver_pc_Rankine_indirect_224.h"
#include "../tcs/csp_solver_two_tank_tes.h"
#include "../tcs/csp_solver_tou_block_schedules.h"

/**
 * This class tests the C_csp_weatherreader's functions and ensures that the interface is the
 * same using weatherfile & weatherdata as weather inputs. The test also tests for variable
 * access and memory.
 */


class CspWeatherReaderTest : public ::testing::Test {
protected:
    C_csp_weatherreader wr;
    C_csp_solver_sim_info sim_info;
    double e;		//epsilon for double comparison

    virtual void SetUp() {
        e = 0.0001;
        wr = C_csp_weatherreader();
        wr.m_trackmode = 0;
        wr.m_tilt = 0;
        wr.m_azimuth = 0.0;
    }
};

class UsingFileCaseWeatherReader : public CspWeatherReaderTest {
    string file;
protected:
    void SetUp() {

        char hourly[256];
        int a = sprintf(hourly, "%s/test/input_docs/weather.csv", std::getenv("SSCDIR"));

        wr.m_filename = hourly;
        CspWeatherReaderTest::SetUp();
        sim_info.ms_ts.m_step = 3600;
        sim_info.ms_ts.m_time_start = 0;
        wr.m_weather_data_provider = make_shared<weatherfile>(hourly);
    }
};

class UsingDataCaseWeatherReader : public CspWeatherReaderTest {
    var_data* data;
protected:
    void SetUp() {
        CspWeatherReaderTest::SetUp();
        sim_info.ms_ts.m_step = 3600;
        sim_info.ms_ts.m_time_start = 0;
        data = create_weatherdata_array(8760); // allocates memory for weatherdata
        wr.m_weather_data_provider = make_shared<weatherdata>(data);
    }
    void TearDown() {
        free_weatherdata_array(data);
    }
};

/**
 * Integration tests for CSP's weatherreader class test that the interface & outputs
 * are the same whether the data is taken from a file or as var_data.
 * The test weather data is for Buenos Aires and taken from SAM's solar resource.
 */

 /// Integration Test for C_csp_weatherreader using weatherfile
TEST_F(UsingFileCaseWeatherReader, IntegrationTest_csp_solver_core) {
    wr.init();
    //check header values
    EXPECT_NEAR(wr.m_weather_data_provider->lat(), -34.82, e) << "Values in weather file's m_hdr\n";
    EXPECT_NEAR(wr.m_weather_data_provider->lon(), -58.53, e) << "Values in weather file's m_hdr\n";
    EXPECT_NEAR(wr.m_weather_data_provider->tz(), -3, e) << "Values in weather file's m_hdr\n";
    EXPECT_NEAR(wr.m_weather_data_provider->elev(), 20, e) << "Values in weather file's m_hdr\n";
    EXPECT_EQ(wr.m_weather_data_provider->step_sec(), 3600) << "Values in weather file's m_hdr\n";
    EXPECT_EQ(wr.m_weather_data_provider->nrecords(), 8760) << "Values in weather file's m_hdr\n";

    // check reading first timestep
    sim_info.ms_ts.m_time = 3600;
    wr.timestep_call(sim_info);
    EXPECT_NEAR(wr.ms_outputs.m_twet, 19.1422, e) << "Twet should be calculated in weather_data_provider.read()\n";
    EXPECT_NEAR(wr.ms_outputs.m_tdry, 20.9, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_pres, 1010, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_beam, 0.0, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_wspd, 2.1, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_aod, 0.291, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_solazi, 187.367892, e) << "Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_outputs.m_solzen, 121.750884, e) << "Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_solved_params.m_shift, -13.530000, e) << "Members specific to CSP weather\n";
    EXPECT_FALSE(wr.ms_solved_params.m_leapyear) << "Members specific to CSP weather\n";

    // check read_time_step at hour ending at 10
    wr.read_time_step(10, sim_info);
    EXPECT_EQ(wr.ms_outputs.m_month, 1);
    EXPECT_EQ(wr.ms_outputs.m_day, 1);
    EXPECT_EQ(wr.ms_outputs.m_hour, 10);
    EXPECT_NEAR(wr.ms_outputs.m_twet, 27.4906, e) << "Twet should be calculated in weather_data_provider.read()\n";
    EXPECT_NEAR(wr.ms_outputs.m_tdry, 29.6, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_pres, 1007, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_beam, 566, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_wspd, 1.5, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_aod, 0.291, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_solazi, 79.817137, e) << "Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_outputs.m_solzen, 34.102205, e) << "Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_solved_params.m_shift, -13.530000, e) << "Members specific to CSP weather\n";
    EXPECT_FALSE(wr.ms_solved_params.m_leapyear) << "Members specific to CSP weather\n";

    // check timestep_call at 11th hour for all values
    wr.converged(); // reset number of times the function is called
    sim_info.ms_ts.m_time = 43600;
    wr.timestep_call(sim_info);
    EXPECT_EQ(wr.ms_outputs.m_month, 1);
    EXPECT_EQ(wr.ms_outputs.m_day, 1);
    EXPECT_EQ(wr.ms_outputs.m_hour, 11);
    EXPECT_EQ(wr.ms_outputs.m_minute, 30) << "Originally empty, minute column should be set to 30 by weatherfile";

    EXPECT_TRUE(std::isnan(wr.ms_outputs.m_global)) << "Global not in weatherfile\n";
    EXPECT_NEAR(wr.ms_outputs.m_beam, 602, e);
    EXPECT_NEAR(wr.ms_outputs.m_diffuse, 315, e);
    EXPECT_NEAR(wr.ms_outputs.m_tdry, 30.6, e);
    EXPECT_NEAR(wr.ms_outputs.m_twet, 28.4516, e) << "Twet should be calculated in weather_data_provider.read()\n";
    EXPECT_NEAR(wr.ms_outputs.m_tdew, 19.8, e);
    EXPECT_NEAR(wr.ms_outputs.m_wspd, 2.6, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_wdir, 180, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_rhum, 85, e) << "Rhum is 85 in weatherfile\n";
    EXPECT_NEAR(wr.ms_outputs.m_pres, 1007, e) << "Values copied from weather file\n";
    EXPECT_TRUE(std::isnan(wr.ms_outputs.m_snow)) << "Snow not in weatherfile\n";
    EXPECT_NEAR(wr.ms_outputs.m_albedo, 0.17, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_aod, 0.291, e) << "Values copied from weather file\n";

    EXPECT_NEAR(wr.ms_outputs.m_poa, 871.628310, e) << "Calculated in timestep_call()\n";
    EXPECT_NEAR(wr.ms_outputs.m_solazi, 64.094861, e) << "Calculated in timestep_call()\n";
    EXPECT_NEAR(wr.ms_outputs.m_solzen, 22.387110, e) << "Calculated in timestep_call()\n";
    EXPECT_NEAR(wr.ms_outputs.m_hor_beam, 556.628310, e) << "Calculated in timestep_call()\n";
    EXPECT_NEAR(wr.ms_outputs.m_time_rise, 5.804955, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_time_set, 20.095858, e) << "11th hour\n";
}

/// Integration Test for C_csp_weatherreader using weatherdata
TEST_F(UsingDataCaseWeatherReader, IntegrationTest_csp_solver_core) {
    wr.init();
    //check header values
    ASSERT_NEAR(wr.m_weather_data_provider->lat(), -34.82, e) << "Values in weather data's m_hdr";
    ASSERT_NEAR(wr.m_weather_data_provider->lon(), -58.53, e) << "Values in weather data's m_hdr";
    ASSERT_NEAR(wr.m_weather_data_provider->tz(), -3, e) << "Values in weather data's m_hdr";
    ASSERT_NEAR(wr.m_weather_data_provider->elev(), 20, e) << "Values in weather data's m_hdr";
    ASSERT_EQ(wr.m_weather_data_provider->step_sec(), 3600) << "Values in weather data's m_hdr";
    ASSERT_EQ(wr.m_weather_data_provider->nrecords(), 8760) << "Values in weather data's m_hdr";

    // check reading first timestep
    sim_info.ms_ts.m_time = 3600;
    wr.timestep_call(sim_info);
    EXPECT_NEAR(wr.ms_outputs.m_twet, 19.1422, e) << "1st time step: Twet should be calculated in weather_data_provider.read()\n";
    EXPECT_NEAR(wr.ms_outputs.m_tdry, 20.9, e) << "1st time step: Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_pres, 1010, e) << "1st time step: Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_beam, 0.0, e) << "1st time step: Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_wspd, 2.1, e) << "1st time step: Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_aod, 0.291, e) << "1st time step: Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_solazi, 187.367892, e) << "1st time step: Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_outputs.m_solzen, 121.750884, e) << "1st time step: Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_solved_params.m_shift, -13.530000, e) << "1st time step: Members specific to CSP weather\n";
    EXPECT_FALSE(wr.ms_solved_params.m_leapyear) << "1st time step: Members specific to CSP weather\n";


    // check read_time_step at hour ending at 10
    wr.read_time_step(10, sim_info);
    EXPECT_EQ(wr.ms_outputs.m_month, 1);
    EXPECT_EQ(wr.ms_outputs.m_day, 1);
    EXPECT_EQ(wr.ms_outputs.m_hour, 10);
    EXPECT_NEAR(wr.ms_outputs.m_twet, 27.4906, e) << "Twet should be calculated in weather_data_provider.read()\n";
    EXPECT_NEAR(wr.ms_outputs.m_tdry, 29.6, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_pres, 1007, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_beam, 566, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_wspd, 1.5, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_aod, 0.291, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_solazi, 79.817137, e) << "Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_outputs.m_solzen, 34.102206, e) << "Members specific to CSP weather\n";
    EXPECT_NEAR(wr.ms_solved_params.m_shift, -13.530000, e) << "Members specific to CSP weather\n";
    EXPECT_FALSE(wr.ms_solved_params.m_leapyear) << "Members specific to CSP weather\n";

    // check timestep_call at 11th hour for all values
    wr.converged(); // reset number of times the function is called
    sim_info.ms_ts.m_time = 43600;
    wr.timestep_call(sim_info);
    EXPECT_EQ(wr.ms_outputs.m_month, 1);
    EXPECT_EQ(wr.ms_outputs.m_day, 1);
    EXPECT_EQ(wr.ms_outputs.m_hour, 11);
    EXPECT_EQ(wr.ms_outputs.m_minute, 30) << "Originally empty, minute column should be set to 30 by weatherfile";

    EXPECT_TRUE(std::isnan(wr.ms_outputs.m_global)) << "11th hour, Global not in weatherfile\n";
    EXPECT_NEAR(wr.ms_outputs.m_beam, 602, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_diffuse, 315, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_tdry, 30.6, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_twet, 28.4516, e) << "Twet should be calculated in weather_data_provider.read()\n";
    EXPECT_NEAR(wr.ms_outputs.m_tdew, 19.8, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_wspd, 2.6, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_wdir, 180, e) << "Values copied from weather file\n";
    EXPECT_NEAR(wr.ms_outputs.m_rhum, 85, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_pres, 1007, e) << "11th hour\n";
    EXPECT_TRUE(std::isnan(wr.ms_outputs.m_snow)) << "11th hour, Snow not in weatherfile\n";
    EXPECT_NEAR(wr.ms_outputs.m_albedo, 0.17, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_aod, 0.291, e) << "11th hour\n";

    EXPECT_NEAR(wr.ms_outputs.m_poa, 871.628306, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_solazi, 64.094861, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_solzen, 22.387111, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_hor_beam, 556.628306, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_time_rise, 5.804955, e) << "11th hour\n";
    EXPECT_NEAR(wr.ms_outputs.m_time_set, 20.095858, e) << "11th hour\n";
}

/**
 *	Integration & Execution Time test
 */
class CspSolverCoreTest : public ::testing::Test {
protected:
    C_csp_weatherreader wr;
    C_pt_sf_perf_interp heliostatfield;
    C_mspt_receiver_222 receiver;
    C_csp_collector_receiver* cr;
    C_csp_power_cycle* pc;
    C_pc_Rankine_indirect_224 rankine;
    C_csp_two_tank_tes tes;
    C_csp_tou_block_schedules tou;
    C_csp_solver::S_sim_setup sim_setup;
    C_csp_solver::S_csp_system_params system;
    C_csp_solver* solver;

    void SetUp() {
        pc = &rankine;
        cr = new C_csp_mspt_collector_receiver(heliostatfield, receiver);
        sim_setup.m_sim_time_start = 0;
        sim_setup.m_sim_time_start = 31536000;
        sim_setup.m_report_step = 3600.0;
        solver = new C_csp_solver(wr, *cr, *pc, tes, tou, system, ssc_cmod_update, (void*)0);
    }
};

void set_heliostatfield(C_pt_sf_perf_interp& heliostatfield, string case_type) {
    C_pt_sf_perf_interp::S_params* p = &(heliostatfield.ms_params);
    if (case_type == "default") {
        // Default run type = 1
    }
}

class DefaultCaseCspSolverCore : public CspSolverCoreTest {
protected:

    void SetUp() {
        CspSolverCoreTest::SetUp();
        // adjust heliostatfield parameters
        tou.mc_dispatch_params.m_dispatch_optimize = 1;
        solver->Ssimulate(sim_setup);
    }
};
