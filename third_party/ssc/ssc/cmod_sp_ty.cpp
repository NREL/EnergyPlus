#include "core.h"

#include <string>

#include <iostream>
#include <fstream>

#include <lk_stdlib.h>
#include <lk_absyn.h>

#include "sam_csp_util.h"

#include "lib_weatherfile.h"

#include "lib_irradproc.h"

#include "lib_pvwatts.h"

static var_info _cm_vtab_sp_ty[] = {

    { SSC_INPUT,        SSC_STRING,      "solar_resource_file",    "Weather file in TMY2, TMY3, EPW, or SMW.",  "",       "",   "sp_ty",      "*", "LOCAL_FILE", "" },

    { SSC_OUTPUT,       SSC_NUMBER,      "a",                      "Modified nonideality factor",    "1/V",    "",                      "6 Parameter Solver",      "*",                        "",                      "" },



var_info_invalid };

class cm_sp_ty : public compute_module
{
public:

    cm_sp_ty()
    {
        add_var_info(_cm_vtab_sp_ty);
    }



    double powerout(double dc_nameplate, double ac_nameplate, double inv_eff_percent, double loss_percent, double ts_hour, double poa, double wspd, double tdry)
    {
        // twn 1.28.15: modified from cmod_pvwatts5.cpp

        double wspd_corr = wspd < 0 ? 0 : wspd;

        // module cover
            // twn: assume perfect tracking so aoi = 0
        double tpoa = poa;

        if (tpoa < 0.0) tpoa = 0.0;

        double inoct = 45 + 273.15;
        double height = 5.0;


        pvwatts_celltemp tccalc(inoct, height, ts_hour);

        // cell temperature
        double pvt = tccalc(poa, wspd_corr, tdry);

        // dc power output (Watts)
        double gamma = -0.0047;
        double dc = dc_nameplate * (1.0 + gamma * (pvt - 25.0)) * tpoa / 1000.0;

        // dc losses
        dc = dc * (1 - loss_percent / 100);

        // inverter efficiency
        double etanom = inv_eff_percent / 100.0;
        double etaref = 0.9637;
        double A = -0.0162;
        double B = -0.0059;
        double C = 0.9858;
        double pdc0 = ac_nameplate / etanom;
        double plr = dc / pdc0;
        double ac = 0;

        if (plr > 0)
        { // normal operation
            double eta = (A * plr + B / plr + C) * etanom / etaref;
            ac = dc * eta;
        }

        if (ac > ac_nameplate) // clipping
            ac = ac_nameplate;

        // make sure no negative AC values (no parasitic nighttime losses calculated)
        if (ac < 0) ac = 0;

        return ac;

    }



    void exec()
    {
        // Input Simulation Parameters:
        double DNI_cutoff = 250.0;		            //[W/m2]
        double eta_rec_therm = 0.9;		            //[-] Estimated receiver thermal efficiency
        double eta_cycle = 0.5;						//[-] Estimated power cycle efficiency
        double eta_csp_parasitics = 0.93;           //[-] Receiver parasitics that scale with thermal power (HTF pumping power, BOP, etc)
        double dispatch_price_multiplier = 1.0;     //[-] Annualized additional value of CSP realized by dispatching during peak demand
        double pv_area_frac = 0.95;					//[-] Fraction of back area that contains PV
        double eta_pv = 0.17;						//[-] Design efficiency of PV
        double eta_inverter = 0.97;					//[-] Inverter efficiency
        double eta_pv_losses = 0.946;				//[-] Other pv losses (from detailed PV model Losses page)
        double Q_delivered_CSP_min = 990.0;			//[kWh/m2] Minimum annual thermal energy delivered by heliostat for 'standard' case
        double availability_csp = 0.96;				//[-] Fraction of annual energy calculated assuming continuous operation that can be realized by actual plant considering O&M and other shutdowns
        double A_hel = 20.0;						//[m2] Heliostat structure area
        double A_hel_refl_frac = 0.97;				//[-] Reflective surface ratio
        double pv_cost_frac = 0.5;					//[-] Cost of PV installed on heliostat relative to pv-only installation
        double annual_output_pv = 6009.0;			//[kWh] PVWatts calculated output for 'pv_watts_nameplate' calculated below

        double dc_ac_ratio = 1.1;
        double inv_eff_percent = 97.0;
        double loss_percent = 10.0;

        // Calculated simulation parameters
        double A_pv = A_hel * pv_area_frac;						//[m2]
        double pv_watts_nameplate = A_pv * eta_pv * 1000.0;		//[Wdc]		
        double ac_nameplate = pv_watts_nameplate / dc_ac_ratio;

        // But really, this floats depending on the PV efficiency for the given conditions
        // double eta_hel_breakeven = (pv_area_frac*eta_pv*eta_inverter*eta_pv_losses) / (eta_rec_therm*eta_cycle*eta_csp_parasitics / dispatch_price_multiplier);


        std::string file_dir = "C:/Users/tneises/Documents/2015 LPDP/csp pv/rotate vs static cavity/Analysis with helfieldpos 180 field";

        if (!lk::dir_exists(file_dir.c_str()))
        {
            throw exec_error("SP_ty", util::format("Directory does not exist"));
        }

        std::vector<lk_string> eta_files = lk::dir_list(file_dir, "csv", false);

        int n_eta_files = eta_files.size();
        int n_sol_pos = n_eta_files;

        for (int i = 0; i < n_eta_files; i++)
            eta_files[i] = file_dir + "/" + eta_files[i];

        lk_string opt_line;

        // Go through the optical efficiency file for each solar position
        // Check (post process) that:
        //	1) Each file includes the same number of heliostats
        //	2) A 2D solar position - optical efficiency table can be created.

        std::vector<int> n_heliostats_per_sol_pos(n_sol_pos);
        std::vector<double> azimuth_per_sol_pos(n_sol_pos);
        std::vector<double> zenith_per_sol_pos(n_sol_pos);

        FILE* fp;

        for (int i = 0; i < n_eta_files; i++)
        {
            fp = fopen(eta_files[i].c_str(), "r");

            if (!fp)
            {
                throw exec_error("SP_ty", util::format("there was a problem opening the file"));
            }

            // Burn the first line
            bool line_ok = lk::read_line(fp, opt_line);

            // The second line contains the azimuth angle
            line_ok = lk::read_line(fp, opt_line);

            int comma1 = opt_line.find_first_of(",");
            comma1 = opt_line.find_first_of(",", comma1 + 1);
            if (comma1 == std::string::npos)
            {
                throw exec_error("SP_ty", util::format("file does not seem to have zenith angle"));
            }

            int comma2 = opt_line.find_first_of(",", comma1 + 1);
            if (comma2 == std::string::npos)
            {
                throw exec_error("SP_ty", util::format("file does not seem to have zenith angle"));
            }

            double zenith = 90.0 - std::atof(opt_line.substr(comma1 + 1, comma2 - comma1 - 1));

            // The third line contains the zenith angle
            line_ok = lk::read_line(fp, opt_line);
            comma1 = opt_line.find_first_of(",");
            comma1 = opt_line.find_first_of(",", comma1 + 1);
            if (comma1 == std::string::npos)
            {
                throw exec_error("SP_ty", util::format("file does not seem to have azimuth angle"));
            }

            comma2 = opt_line.find_first_of(",", comma1 + 1);
            if (comma1 == std::string::npos)
            {
                throw exec_error("SP_ty", util::format("file does not seem to have azimuth angle"));
            }

            double azimuth = std::atof(opt_line.substr(comma1 + 1, comma2 - comma1 - 1));

            // Burn 4th - 6th lines
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);

            // 7th - 9th lines are DESIGN solar positions
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);

            // Labels
            line_ok = lk::read_line(fp, opt_line);

            // Now want to count the number of remaining rows to get the number of heliostats
            int n_heliostats = 0;
            while (true)
            {
                if (!lk::read_line(fp, opt_line))
                {
                    break;
                }
                else
                {
                    n_heliostats++;
                }
            }

            // Store important information for each solar position file
            n_heliostats_per_sol_pos[i] = n_heliostats;
            azimuth_per_sol_pos[i] = azimuth;
            zenith_per_sol_pos[i] = zenith;

            fclose(fp);
        }

        // Check that the number of heliostats is equal in all files
        for (int i = 1; i < n_sol_pos; i++)
        {
            if (n_heliostats_per_sol_pos[i] != n_heliostats_per_sol_pos[0])
            {
                throw exec_error("SP_ty", util::format("All files do not contain the same number of heliostats"));
            }
        }
        int n_heliostats_field = n_heliostats_per_sol_pos[0];

        // Check that there are the same number of zenith positions for each azimuth position
        std::vector<double> listed_azimuth(1);
        listed_azimuth[0] = azimuth_per_sol_pos[0];

        std::vector<int> n_zenith_per_az(1);
        n_zenith_per_az[0] = 0;

        std::vector<double> listed_zenith(1);
        listed_zenith[0] = zenith_per_sol_pos[0];

        for (int i = 0; i < n_sol_pos; i++)
        {
            int n_listed_azimuth = listed_azimuth.size();

            bool new_az_pos = true;

            int j;

            for (j = 0; j < n_listed_azimuth; j++)
            {
                if (azimuth_per_sol_pos[i] == listed_azimuth[j])
                {
                    new_az_pos = false;
                    break;
                }
            }

            if (new_az_pos)
            {
                listed_azimuth.push_back(azimuth_per_sol_pos[i]);
                n_zenith_per_az.push_back(1);
            }
            else
            {
                n_zenith_per_az[j]++;
            }

            int n_listed_zenith = listed_zenith.size();

            bool new_zen_pos = true;

            for (int jj = 0; jj < n_listed_zenith; jj++)
            {
                if (zenith_per_sol_pos[i] == listed_zenith[jj])
                {
                    new_zen_pos = false;
                    break;
                }
            }

            if (new_zen_pos)
            {
                listed_zenith.push_back(zenith_per_sol_pos[i]);
            }
        }

        // Need AT LEAST 2 zenith and 3 azimuth positions, with +/- 180 azimuth and 0 zenith covered

        // Need at least 3 azimuth angles
        int n_azimuth = listed_azimuth.size();
        if (n_azimuth < 3)
        {
            throw exec_error("SP_ty", util::format("Need at least 3 azimuth angles"));
        }

        // Need same number of zenith angles for each azimuth angle
        for (int i = 1; i < n_zenith_per_az.size(); i++)
        {
            if (n_zenith_per_az[i] != n_zenith_per_az[0])
            {
                throw exec_error("SP_ty", util::format("Need same number of zenith angles for each azimuth angle"));
            }
        }

        // Need at least 2 azimuth angles
        if (n_zenith_per_az[0] < 2)
        {
            throw exec_error("SP_ty", util::format("Need at least 2 zenith angles"));
        }

        // Zenith of 90 is included
        int n_zenith = listed_zenith.size();
        bool found_zenith_90 = false;
        for (int i = 0; i < n_zenith; i++)
        {
            if (listed_zenith[i] == 0.0)
            {
                found_zenith_90 = true;
                break;
            }
        }

        if (!found_zenith_90)
        {
            throw exec_error("SP_ty", util::format("Need a zenith angle = 0 degrees"));
        }

        // Azimuth angles of 180 AND -180 is included
        bool found_az_180 = false;
        bool found_az_n_180 = false;
        for (int i = 0; i < n_azimuth; i++)
        {
            if (listed_azimuth[i] == 180)
            {
                found_az_180 = true;
            }
            if (listed_azimuth[i] == -180)
            {
                found_az_n_180 = true;
            }
        }

        if (!found_az_180 || !found_az_n_180)
        {
            throw exec_error("SP_ty", util::format("Need azimuth angles =  +/- 180 degrees"));
        }


        std::vector<double> sorted_azimuth = listed_azimuth;
        std::vector<double> sorted_zenith = listed_zenith;

        double max_az = 1000.0;
        double min_az = -1000.0;
        double local_max = max_az;
        int save_index = -1;

        for (int i = 0; i < listed_azimuth.size(); i++)
        {
            for (int j = 0; j < listed_azimuth.size(); j++)
            {
                if (listed_azimuth[j] < local_max && listed_azimuth[j] > min_az)
                {
                    local_max = listed_azimuth[j];
                    save_index = j;
                }
            }
            sorted_azimuth[i] = listed_azimuth[save_index];
            min_az = sorted_azimuth[i];
            local_max = max_az;
        }

        double max_zen = 1000.0;
        double min_zen = -1000.0;
        local_max = max_zen;
        save_index = -1;

        for (int i = 0; i < listed_zenith.size(); i++)
        {
            for (int j = 0; j < listed_zenith.size(); j++)
            {
                if (listed_zenith[j] < local_max && listed_zenith[j] > min_zen)
                {
                    local_max = listed_zenith[j];
                    save_index = j;
                }
            }
            sorted_zenith[i] = listed_zenith[save_index];
            min_zen = sorted_zenith[i];
            local_max = max_zen;
        }

        // If we're saving the optical efficiency for each heliostat for each solar position, then n_az * n_zen * n_heliostats entries
        // vector<double> combined_eta_opt([n_az*n_zen]*n_heliostats);
        // for each file, combined_eta_opt[*index* + row (helio id)] = eta_opt_row
        // so reference index of listed_zenith and listed_azimuth to get index
        util::matrix_t<double> combined_eta_opt(n_sol_pos, n_heliostats_field, 0.0);

        std::vector<double> r_hel(n_heliostats_field, 0.0);
        std::vector<double> az_hel(n_heliostats_field, 0.0);

        // Now, cycle back through the files and populate 'combined_eta_opt'
        for (int i = 0; i < n_eta_files; i++)
        {
            // Copied from above - but can delete lines that read in solar position - this information is already stored

            fp = fopen(eta_files[i].c_str(), "r");

            // Burn the first 9 lines
            bool line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);
            // The third line contains the zenith angle
            line_ok = lk::read_line(fp, opt_line);
            // Burn 4th - 6th lines
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);
            // 7th - 9th lines are DESIGN solar positions
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);
            line_ok = lk::read_line(fp, opt_line);

            // Labels
            line_ok = lk::read_line(fp, opt_line);

            // Now want to count the number of remaining rows to get the number of heliostats
            int n_heliostats = 0;
            double x_pos;
            double y_pos;
            double z_pos;
            double opt_eta_single;

            double az_local;

            while (true)
            {
                if (!lk::read_line(fp, opt_line))
                {
                    break;
                }
                else
                {
                    //int comma1 = -1;
                    //int comma2 = -1;
                    //for( int k = 0; k < 4; k++ )
                    //{
                    //	comma1 = opt_line.find_first_of(",", comma2 + 1);
                    //	comma2 = opt_line.find_first_of(",", comma1 + 1);
                    //}

                    // Get x pos
                    int comma1 = opt_line.find_first_of(",");
                    int comma2 = opt_line.find_first_of(",", comma1 + 1);
                    x_pos = std::atof(opt_line.substr(comma1 + 1, comma2 - comma1 - 1));

                    comma1 = comma2;
                    comma2 = opt_line.find_first_of(",", comma1 + 1);
                    y_pos = std::atof(opt_line.substr(comma1 + 1, comma2 - comma1 - 1));

                    comma1 = comma2;
                    comma2 = opt_line.find_first_of(",", comma1 + 1);
                    z_pos = std::atof(opt_line.substr(comma1 + 1, comma2 - comma1 - 1));

                    if (i == 0)		// heliostat positions are the same in each optical efficiency file, so only need to do this once...
                    {
                        r_hel[n_heliostats] = sqrt(y_pos * y_pos + x_pos * x_pos);
                        if (y_pos == 0.0)
                        {
                            if (x_pos < 0.0)
                                az_local = -90.0;
                            else
                                az_local = 90.0;
                        }
                        else
                        {
                            az_local = atan(x_pos / y_pos) * 180.0 / 3.1412;
                        }
                        az_hel[n_heliostats] = az_local;
                    }

                    comma1 = comma2;
                    comma2 = opt_line.find_first_of(",", comma1 + 1);
                    opt_eta_single = std::atof(opt_line.substr(comma1 + 1, comma2 - comma1 - 1));

                    combined_eta_opt(i, n_heliostats) = opt_eta_single;
                    n_heliostats++;
                }
            }

            fclose(fp);

        }	// Efficiencies defined for each heliostat at each solar position

        double test_radius = std::numeric_limits<double>::quiet_NaN();
        int test_radius_0az_index = -1;

        for (int i = 0; i < n_heliostats_field; i++)
        {
            if (r_hel[i] > 450.0 && r_hel[i] < 550.0 && az_hel[i] == 0.0)
            {
                test_radius = r_hel[i];
                test_radius_0az_index = i;
            }
        }

        if (test_radius != test_radius)
            throw exec_error("sp_ty", "couldn't find radius within band that had az = 0");

        std::vector<int> hel_id_test_radius(0);

        for (int i = 0; i < n_heliostats_field; i++)
        {
            if (abs(r_hel[i] - test_radius) < 0.1)
                hel_id_test_radius.push_back(i);
        }

        int n_heliostats_test_radius = hel_id_test_radius.size();

        std::vector<int> hel_id_test_radius_az_sorted(n_heliostats_test_radius);

        max_az = 1000.0;
        min_az = -1000.0;
        local_max = max_az;
        save_index = -1;

        for (int i = 0; i < n_heliostats_test_radius; i++)
        {
            for (int j = 0; j < n_heliostats_test_radius; j++)
            {
                if (az_hel[hel_id_test_radius[j]] < local_max && az_hel[hel_id_test_radius[j]] > min_az)
                {
                    local_max = az_hel[hel_id_test_radius[j]];
                    save_index = j;
                }
            }
            hel_id_test_radius_az_sorted[i] = hel_id_test_radius[save_index];
            min_az = az_hel[hel_id_test_radius[save_index]];
            local_max = max_az;
        }

        double rec_rotate_span;

        // Start proxy simulation
        for (int index_rec_rot = 18; index_rec_rot < 19; index_rec_rot++)
        {
            rec_rotate_span = 10 * (index_rec_rot);

            std::vector<double> hel_csp_to_rec_annual(n_heliostats_test_radius);
            std::vector<double> csp_available_relval_annual(n_heliostats_test_radius);
            std::vector<double> csp_produced_relval_annual(n_heliostats_test_radius);
            std::vector<double> pv_available_relval_annual(n_heliostats_test_radius);
            std::vector<double> pv_produced_relval_annual(n_heliostats_test_radius);

            std::vector<double> annual_bin0_10(n_heliostats_test_radius);
            std::vector<double> annual_bin11_20(n_heliostats_test_radius);
            std::vector<double> annual_bin21_30(n_heliostats_test_radius);
            std::vector<double> annual_bin31_40(n_heliostats_test_radius);
            std::vector<double> annual_bin41_50(n_heliostats_test_radius);
            std::vector<double> annual_bin51_60(n_heliostats_test_radius);
            std::vector<double> annual_bin61_70(n_heliostats_test_radius);
            std::vector<double> annual_bin71_80(n_heliostats_test_radius);
            std::vector<double> annual_bin81_90(n_heliostats_test_radius);
            std::vector<double> annual_bin91_100(n_heliostats_test_radius);

            hel_csp_to_rec_annual.assign(n_heliostats_test_radius, 0.0);
            csp_available_relval_annual.assign(n_heliostats_test_radius, 0.0);
            csp_produced_relval_annual.assign(n_heliostats_test_radius, 0.0);
            pv_available_relval_annual.assign(n_heliostats_test_radius, 0.0);
            pv_produced_relval_annual.assign(n_heliostats_test_radius, 0.0);

            annual_bin0_10.assign(n_heliostats_test_radius, 0.0);
            annual_bin11_20.assign(n_heliostats_test_radius, 0.0);
            annual_bin21_30.assign(n_heliostats_test_radius, 0.0);
            annual_bin31_40.assign(n_heliostats_test_radius, 0.0);
            annual_bin41_50.assign(n_heliostats_test_radius, 0.0);
            annual_bin51_60.assign(n_heliostats_test_radius, 0.0);
            annual_bin61_70.assign(n_heliostats_test_radius, 0.0);
            annual_bin71_80.assign(n_heliostats_test_radius, 0.0);
            annual_bin81_90.assign(n_heliostats_test_radius, 0.0);
            annual_bin91_100.assign(n_heliostats_test_radius, 0.0);

            // For EACH heliostat at 'test_radius', run an annual simulation with the generic csp model
            for (int index_nh = 0; index_nh < n_heliostats_test_radius; index_nh++)
            {
                // Simulate each hour
                weatherfile wf(as_string("solar_resource_file"));
                if (!wf.ok())
                    throw exec_error("sp_ty", wf.error_message());

                size_t nrec = wf.nrecords;
                size_t step_per_hour = nrec / 8760;
                if (step_per_hour < 1 || step_per_hour > 60 || step_per_hour * 8760 != nrec)
                    throw exec_error("sp_ty", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec));

                double ts_hour = 1.0 / step_per_hour;

                int sun_up_hours = 0;
                int bin0_10 = 0;
                int bin11_20 = 0;
                int bin21_30 = 0;
                int bin31_40 = 0;
                int bin41_50 = 0;
                int bin51_60 = 0;
                int bin61_70 = 0;
                int bin71_80 = 0;
                int bin81_90 = 0;
                int bin91_100 = 0;

                size_t idx = 0;
                size_t hour = 0;
                while (hour < 8760)
                {
                    double sum_opt_eta_dni_product = 0.0;
                    double sum_csp_available_relval_ts = 0.0;
                    double sum_csp_produced_relval_ts = 0.0;
                    double sum_pv_available_relval_ts = 0.0;
                    double sum_pv_produced_relval_ts = 0.0;

                    for (size_t jj = 0; jj < step_per_hour; jj++)
                    {
                        if (!wf.read())
                            throw exec_error("sp_ty", "could not read data line " + util::to_string((int)(idx + 1)) + " in weather file");

                        if (wf.dn < 0 || wf.dn > 1500.0)
                        {
                            log(util::format("invalid beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
                                wf.dn, wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
                            wf.dn = 0;
                        }

                        irrad irr;
                        irr.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute, ts_hour);
                        irr.set_location(wf.lat, wf.lon, wf.tz);
                        irr.set_optional();

                        // These could eventually be inputs...
                        int skymodel = 2;

                        double alb = 0.2;

                        irr.set_sky_model(skymodel, alb);
                        irr.set_beam_diffuse(wf.dn, wf.df);

                        int track_mode = 2;
                        irr.set_surface(2, 0.0, 0.0, 0.0, false, 0.3);


                        int code = irr.calc();
                        if (code != 0)
                            throw exec_error("pvsamv1",
                                util::format("failed to process irradiation on surface %d (code: %d) [y:%d m:%d d:%d h:%d]",
                                    1, code, wf.year, wf.month, wf.day, wf.hour));

                        double solazi, solzen, solalt;
                        int sunup;

                        irr.get_sun(&solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0);

                        double ibeam, iskydiff, ignddiff;

                        irr.get_poa(&ibeam, &iskydiff, &ignddiff, 0, 0, 0);

                        double poa_pv = ibeam + iskydiff + ignddiff;

                        double opt_eta_dni_product;
                        double csp_available_relval_ts;
                        double csp_produced_relval_ts;
                        double pv_available_relval_ts;
                        double pv_produced_relval_ts;

                        if (sunup == 1)
                        {
                            sun_up_hours++;

                            double az_sp;

                            if (solazi > 180.0)
                                az_sp = solazi - 360.0;
                            else
                                az_sp = solazi;

                            double az_sp360 = az_sp;

                            if (az_sp360 < 0.0)
                                az_sp360 += 360;

                            double az_rec360;

                            double min_az360 = 180.0 - 0.5 * rec_rotate_span;
                            double max_az360 = 180.0 + 0.5 * rec_rotate_span;

                            if (az_sp360 < min_az360)
                                az_rec360 = min_az360;
                            else if (az_sp360 > max_az360)
                                az_rec360 = max_az360;
                            else
                                az_rec360 = az_sp360;

                            double az_hel_local360 = az_hel[hel_id_test_radius_az_sorted[index_nh]] + 180.0;

                            double delta_az_hel_rec = az_hel_local360 - az_rec360;

                            int index_hel_proxy = -1;

                            for (int i = 0; i < n_heliostats_test_radius; i++)
                            {
                                if (az_hel[hel_id_test_radius_az_sorted[i]] > delta_az_hel_rec)
                                {
                                    index_hel_proxy = hel_id_test_radius_az_sorted[i];
                                    break;
                                }
                            }

                            double opt_eta_point;

                            if (index_hel_proxy > -1)
                            {
                                double check_az = az_hel[index_hel_proxy];

                                double az_solar_proxy360 = 180.0;

                                if (az_sp360 < min_az360)
                                    az_solar_proxy360 = az_sp360 - min_az360;		// should be negative azimuth angle

                                if (az_sp360 > max_az360)
                                    az_solar_proxy360 = az_sp360 - max_az360;		// should be positive azimuth angle

                                // Try defining an optical efficiency table...
                                OpticalDataTable optical_table;

                                // Set xaxis data values
                                double* xax = new double[n_azimuth];

                                //optical_table.AddXAxis(xax, n_azimuth);

                                for (int i = 0; i < n_azimuth; i++)
                                {
                                    xax[i] = sorted_azimuth[i];
                                }

                                // Set yaxis data values
                                double* yax = new double[n_zenith + 1];
                                for (int i = 0; i < n_zenith; i++)
                                {
                                    yax[i] = sorted_zenith[i];
                                }
                                yax[n_zenith] = 90.0;

                                // Get the data values
                                double* data = new double[(n_zenith + 1) * n_azimuth];
                                for (int i = 0; i < n_sol_pos; i++)
                                {
                                    double z_local = zenith_per_sol_pos[i];
                                    double a_local = azimuth_per_sol_pos[i];

                                    int i_local = -1;
                                    for (int ii = 0; ii < n_azimuth; ii++)
                                    {
                                        if (sorted_azimuth[ii] == a_local)
                                            i_local = ii;
                                    }

                                    int j_local = -1;
                                    for (int ii = 0; ii < n_zenith; ii++)
                                    {
                                        if (sorted_zenith[ii] == z_local)
                                            j_local = ii;
                                    }

                                    data[i_local + (n_azimuth)*j_local] = combined_eta_opt(i, index_hel_proxy);
                                }

                                // Fill in eta_opt = 0 for zenith = 0
                                for (int i = 0; i < n_azimuth; i++)
                                {
                                    data[i + (n_azimuth)*n_zenith] = 0.0;
                                }

                                // Load optical_table class
                                optical_table.AddXAxis(xax, n_azimuth);
                                optical_table.AddYAxis(yax, n_zenith + 1);
                                optical_table.AddData(data);

                                delete[] xax;
                                delete[] yax;
                                delete[] data;

                                opt_eta_point = optical_table.interpolate(az_sp, solzen);
                            }
                            else
                            {
                                opt_eta_point = 0.0;
                            }

                            // Get PV output
                            pv_available_relval_ts = powerout(pv_watts_nameplate, ac_nameplate, inv_eff_percent, loss_percent, ts_hour, poa_pv, wf.wspd, wf.tdry) / 1000.0;	// kWe
                            //double powerout(double dc_nameplate, double ac_nameplate, double inv_eff_percent, double loss_percent, double ts_hour, double poa, double wspd, double tdry)

                            if (wf.dn > DNI_cutoff)
                            {
                                opt_eta_dni_product = opt_eta_point * wf.dn * 0.001;		// kW/m2

                                csp_available_relval_ts = opt_eta_point * wf.dn * 0.001 * eta_rec_therm * eta_cycle * eta_csp_parasitics * dispatch_price_multiplier * A_hel * A_hel_refl_frac;		// kWe

                                if (csp_available_relval_ts > pv_available_relval_ts)
                                {
                                    csp_produced_relval_ts = csp_available_relval_ts;
                                    pv_produced_relval_ts = 0.0;
                                }
                                else
                                {
                                    csp_produced_relval_ts = 0.0;
                                    pv_produced_relval_ts = pv_available_relval_ts;
                                }
                            }
                            else
                            {
                                opt_eta_dni_product = 0.0;

                                csp_available_relval_ts = 0.0;
                                csp_produced_relval_ts = 0.0;

                                pv_produced_relval_ts = pv_available_relval_ts;
                            }

                            opt_eta_point = opt_eta_dni_product / (950.0 * 0.001);		//[-] Normalized output compared to perfect optical efficiency at design DNI

                            // Bin heliostat optical efficiency
                            if (opt_eta_point <= 0.10)
                                bin0_10++;
                            else if (opt_eta_point <= 0.20)
                                bin11_20++;
                            else if (opt_eta_point <= 0.30)
                                bin21_30++;
                            else if (opt_eta_point <= 0.40)
                                bin31_40++;
                            else if (opt_eta_point <= 0.50)
                                bin41_50++;
                            else if (opt_eta_point <= 0.60)
                                bin51_60++;
                            else if (opt_eta_point <= 0.70)
                                bin61_70++;
                            else if (opt_eta_point <= 0.80)
                                bin71_80++;
                            else if (opt_eta_point <= 0.90)
                                bin81_90++;
                            else
                                bin91_100++;

                        }		// End sun-up calculations
                        else
                        {
                            opt_eta_dni_product = 0.0;

                            csp_available_relval_ts = 0.0;		// kW/m2
                            csp_produced_relval_ts = 0.0;

                            pv_available_relval_ts = 0.0;
                            pv_produced_relval_ts = 0.0;
                        }

                        sum_opt_eta_dni_product += opt_eta_dni_product * ts_hour;						// kWh/m2
                        sum_csp_available_relval_ts += csp_available_relval_ts * ts_hour;				// kWeh/m2
                        sum_csp_produced_relval_ts += csp_produced_relval_ts * ts_hour;				// kWeh/m2
                        sum_pv_available_relval_ts += pv_available_relval_ts * ts_hour;				// kWeh/m2
                        sum_pv_produced_relval_ts += pv_produced_relval_ts * ts_hour;					// kWeh/m2


                        idx++;		// Add to weatherfile line counter and repeat
                    }

                    //hel_csp_to_rec_hourly(index_nh, hour) = sum_opt_eta_dni_product;		// kW/m2
                    hel_csp_to_rec_annual[index_nh] += sum_opt_eta_dni_product;			// kW/m

                    //csp_available_relval_hourly(index_nh, hour) = sum_csp_available_relval_ts;
                    csp_available_relval_annual[index_nh] += sum_csp_available_relval_ts;

                    //csp_produced_relval_hourly(index_nh, hour) = sum_csp_produced_relval_ts;
                    csp_produced_relval_annual[index_nh] += sum_csp_produced_relval_ts;

                    //pv_available_relval_hourly(index_nh, hour) = sum_pv_available_relval_ts;
                    pv_available_relval_annual[index_nh] += sum_pv_available_relval_ts;

                    //pv_produced_relval_hourly(index_nh, hour) = sum_pv_produced_relval_ts;
                    pv_produced_relval_annual[index_nh] += sum_pv_produced_relval_ts;



                    hour++;		// Step through another hour and repeat
                }


                double sun_up_total = (double)sun_up_hours;

                annual_bin0_10[index_nh] = (double)bin0_10 / sun_up_total;
                annual_bin11_20[index_nh] = (double)bin11_20 / sun_up_total;
                annual_bin21_30[index_nh] = (double)bin21_30 / sun_up_total;
                annual_bin31_40[index_nh] = (double)bin31_40 / sun_up_total;
                annual_bin41_50[index_nh] = (double)bin41_50 / sun_up_total;
                annual_bin51_60[index_nh] = (double)bin51_60 / sun_up_total;
                annual_bin61_70[index_nh] = (double)bin61_70 / sun_up_total;
                annual_bin71_80[index_nh] = (double)bin71_80 / sun_up_total;
                annual_bin81_90[index_nh] = (double)bin81_90 / sun_up_total;
                annual_bin91_100[index_nh] = (double)bin91_100 / sun_up_total;


            }  // Go to next heliostat

            // Write outputs
            std::string hel_performance_processed = file_dir + "/hel_performance_processed_rotate_" + to_string(rec_rotate_span) + ".txt";

            ofstream hel_output_file(hel_performance_processed.c_str());

            std::string out_line = "heliostat id,azimuth,radius,Q_deliver_hel (kWh_per_m2),CSP_avail_relval (kWeh),CSP_produced_relval (kWeh),PV_avail_relval (kWeh),PV_produced_relval (kWeh),bin0-11,bin11-20,bin21-30,bin31-40,bin41-50,bin51-60,bin61-70,bin71-80,bin81-90,bin91-100\n";

            hel_output_file << out_line;

            for (int i = 0; i < n_heliostats_test_radius; i++)
            {
                hel_output_file << to_string(hel_id_test_radius_az_sorted[i]) + "," + to_string(az_hel[hel_id_test_radius_az_sorted[i]]) +
                    "," + to_string(r_hel[hel_id_test_radius_az_sorted[i]]) +
                    "," + to_string(hel_csp_to_rec_annual[i]) +
                    "," + to_string(csp_available_relval_annual[i] * availability_csp) +
                    "," + to_string(csp_produced_relval_annual[i] * availability_csp) +
                    "," + to_string(pv_available_relval_annual[i]) +
                    "," + to_string(availability_csp * (pv_produced_relval_annual[i] - pv_available_relval_annual[i]) + pv_available_relval_annual[i]) +
                    "," + to_string(annual_bin0_10[i]) +
                    "," + to_string(annual_bin11_20[i]) +
                    "," + to_string(annual_bin21_30[i]) +
                    "," + to_string(annual_bin31_40[i]) +
                    "," + to_string(annual_bin41_50[i]) +
                    "," + to_string(annual_bin51_60[i]) +
                    "," + to_string(annual_bin61_70[i]) +
                    "," + to_string(annual_bin71_80[i]) +
                    "," + to_string(annual_bin81_90[i]) +
                    "," + to_string(annual_bin91_100[i]) + "\n";
            }

        }	// End 'proxy' rotating simulation

        return;

        //util::matrix_t<double> hel_csp_to_rec_hourly(n_heliostats_field, 8760, 0.0);
        //util::matrix_t<double> csp_available_relval_hourly(n_heliostats_field, 8760, 0.0);
        //util::matrix_t<double> csp_produced_relval_hourly(n_heliostats_field, 8760, 0.0);
        //util::matrix_t<double> pv_available_relval_hourly(n_heliostats_field, 8760, 0.0);
        //util::matrix_t<double> pv_produced_relval_hourly(n_heliostats_field, 8760, 0.0);

        std::vector<double> hel_csp_to_rec_annual(n_heliostats_field);
        std::vector<double> csp_available_relval_annual(n_heliostats_field);
        std::vector<double> csp_produced_relval_annual(n_heliostats_field);
        std::vector<double> pv_available_relval_annual(n_heliostats_field);
        std::vector<double> pv_produced_relval_annual(n_heliostats_field);

        std::vector<double> annual_bin0_10(n_heliostats_field);
        std::vector<double> annual_bin11_20(n_heliostats_field);
        std::vector<double> annual_bin21_30(n_heliostats_field);
        std::vector<double> annual_bin31_40(n_heliostats_field);
        std::vector<double> annual_bin41_50(n_heliostats_field);
        std::vector<double> annual_bin51_60(n_heliostats_field);
        std::vector<double> annual_bin61_70(n_heliostats_field);
        std::vector<double> annual_bin71_80(n_heliostats_field);
        std::vector<double> annual_bin81_90(n_heliostats_field);
        std::vector<double> annual_bin91_100(n_heliostats_field);

        hel_csp_to_rec_annual.assign(n_heliostats_field, 0.0);
        csp_available_relval_annual.assign(n_heliostats_field, 0.0);
        csp_produced_relval_annual.assign(n_heliostats_field, 0.0);
        pv_available_relval_annual.assign(n_heliostats_field, 0.0);
        pv_produced_relval_annual.assign(n_heliostats_field, 0.0);

        annual_bin0_10.assign(n_heliostats_field, 0.0);
        annual_bin11_20.assign(n_heliostats_field, 0.0);
        annual_bin21_30.assign(n_heliostats_field, 0.0);
        annual_bin31_40.assign(n_heliostats_field, 0.0);
        annual_bin41_50.assign(n_heliostats_field, 0.0);
        annual_bin51_60.assign(n_heliostats_field, 0.0);
        annual_bin61_70.assign(n_heliostats_field, 0.0);
        annual_bin71_80.assign(n_heliostats_field, 0.0);
        annual_bin81_90.assign(n_heliostats_field, 0.0);
        annual_bin91_100.assign(n_heliostats_field, 0.0);

        // For EACH heliostat, run an annual simulation with the generic csp model
        for (int index_nh = 0; index_nh < n_heliostats_test_radius; index_nh++)
        {
            // Status update!
            if (index_nh % (n_heliostats_test_radius) == 0)
            {
                float percent = 100.0f * ((float)index_nh + 1) / ((float)n_heliostats_test_radius);
                if (!update("", percent, (float)index_nh))
                    throw exec_error("sp_ty", "update failed");
            }


            // Try defining an optical efficiency table...
            OpticalDataTable optical_table;

            // Set xaxis data values
            double* xax = new double[n_azimuth];

            //optical_table.AddXAxis(xax, n_azimuth);

            for (int i = 0; i < n_azimuth; i++)
            {
                xax[i] = sorted_azimuth[i];
            }

            // Set yaxis data values
            double* yax = new double[n_zenith + 1];
            for (int i = 0; i < n_zenith; i++)
            {
                yax[i] = sorted_zenith[i];
            }
            yax[n_zenith] = 90.0;

            // Get the data values
            double* data = new double[(n_zenith + 1) * n_azimuth];
            for (int i = 0; i < n_sol_pos; i++)
            {
                double z_local = zenith_per_sol_pos[i];
                double a_local = azimuth_per_sol_pos[i];

                int i_local = -1;
                for (int ii = 0; ii < n_azimuth; ii++)
                {
                    if (sorted_azimuth[ii] == a_local)
                        i_local = ii;
                }

                int j_local = -1;
                for (int ii = 0; ii < n_zenith; ii++)
                {
                    if (sorted_zenith[ii] == z_local)
                        j_local = ii;
                }

                data[i_local + (n_azimuth)*j_local] = combined_eta_opt(i, hel_id_test_radius_az_sorted[index_nh]);
            }

            // Fill in eta_opt = 0 for zenith = 0
            for (int i = 0; i < n_azimuth; i++)
            {
                data[i + (n_azimuth)*n_zenith] = 0.0;
            }

            // Load optical_table class
            optical_table.AddXAxis(xax, n_azimuth);
            optical_table.AddYAxis(yax, n_zenith + 1);
            optical_table.AddData(data);

            delete[] xax;
            delete[] yax;
            delete[] data;

            // Simulate each hour
            weatherfile wf(as_string("solar_resource_file"));
            if (!wf.ok())
                throw exec_error("sp_ty", wf.error_message());

            size_t nrec = wf.nrecords;
            size_t step_per_hour = nrec / 8760;
            if (step_per_hour < 1 || step_per_hour > 60 || step_per_hour * 8760 != nrec)
                throw exec_error("sp_ty", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec));

            double ts_hour = 1.0 / step_per_hour;

            int sun_up_hours = 0;
            int bin0_10 = 0;
            int bin11_20 = 0;
            int bin21_30 = 0;
            int bin31_40 = 0;
            int bin41_50 = 0;
            int bin51_60 = 0;
            int bin61_70 = 0;
            int bin71_80 = 0;
            int bin81_90 = 0;
            int bin91_100 = 0;

            size_t idx = 0;
            size_t hour = 0;
            while (hour < 8760)
            {
                double sum_opt_eta_dni_product = 0.0;
                double sum_csp_available_relval_ts = 0.0;
                double sum_csp_produced_relval_ts = 0.0;
                double sum_pv_available_relval_ts = 0.0;
                double sum_pv_produced_relval_ts = 0.0;

                for (size_t jj = 0; jj < step_per_hour; jj++)
                {
                    if (!wf.read())
                        throw exec_error("sp_ty", "could not read data line " + util::to_string((int)(idx + 1)) + " in weather file");

                    if (wf.dn < 0 || wf.dn > 1500.0)
                    {
                        log(util::format("invalid beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
                            wf.dn, wf.year, wf.month, wf.day, wf.hour), SSC_WARNING, (float)idx);
                        wf.dn = 0;
                    }

                    irrad irr;
                    irr.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute, ts_hour);
                    irr.set_location(wf.lat, wf.lon, wf.tz);
                    irr.set_optional();

                    // These could eventually be inputs...
                    int skymodel = 2;

                    double alb = 0.2;

                    irr.set_sky_model(skymodel, alb);
                    irr.set_beam_diffuse(wf.dn, wf.df);

                    int track_mode = 2;
                    irr.set_surface(2, 0.0, 0.0, 0.0, false, 0.3);


                    int code = irr.calc();
                    if (code != 0)
                        throw exec_error("pvsamv1",
                            util::format("failed to process irradiation on surface %d (code: %d) [y:%d m:%d d:%d h:%d]",
                                1, code, wf.year, wf.month, wf.day, wf.hour));

                    double solazi, solzen, solalt;
                    int sunup;

                    irr.get_sun(&solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0);

                    double ibeam, iskydiff, ignddiff;

                    irr.get_poa(&ibeam, &iskydiff, &ignddiff, 0, 0, 0);

                    double poa_pv = ibeam + iskydiff + ignddiff;

                    double az_sp;

                    if (solazi > 180.0)
                        az_sp = solazi - 360.0;
                    else
                        az_sp = solazi;

                    double opt_eta_point;
                    if (sunup)
                        opt_eta_point = optical_table.interpolate(az_sp, solzen);
                    else
                        opt_eta_point = 0.0;

                    double opt_eta_dni_product;
                    double csp_available_relval_ts;
                    double csp_produced_relval_ts;
                    double pv_available_relval_ts;
                    double pv_produced_relval_ts;

                    // What about hour while sun is rising? worth considering?

                    if (sunup == 1)
                    {
                        sun_up_hours++;

                        // Get PV output
                        pv_available_relval_ts = powerout(pv_watts_nameplate, ac_nameplate, inv_eff_percent, loss_percent, ts_hour, poa_pv, wf.wspd, wf.tdry) / 1000.0;	// kWe
                        //double powerout(double dc_nameplate, double ac_nameplate, double inv_eff_percent, double loss_percent, double ts_hour, double poa, double wspd, double tdry)

                        if (wf.dn > DNI_cutoff)
                        {
                            opt_eta_dni_product = opt_eta_point * wf.dn * 0.001;		// kW/m2

                            csp_available_relval_ts = opt_eta_point * wf.dn * 0.001 * eta_rec_therm * eta_cycle * eta_csp_parasitics * dispatch_price_multiplier * A_hel * A_hel_refl_frac;		// kWe

                            if (csp_available_relval_ts > pv_available_relval_ts)
                            {
                                csp_produced_relval_ts = csp_available_relval_ts;
                                pv_produced_relval_ts = 0.0;
                            }
                            else
                            {
                                csp_produced_relval_ts = 0.0;
                                pv_produced_relval_ts = pv_available_relval_ts;
                            }
                        }
                        else
                        {
                            opt_eta_dni_product = 0.0;

                            csp_available_relval_ts = 0.0;
                            csp_produced_relval_ts = 0.0;

                            pv_produced_relval_ts = pv_available_relval_ts;
                        }

                        opt_eta_point = opt_eta_dni_product / (950.0 * 0.001);		//[-] Normalized output compared to perfect optical efficiency at design DNI

                        // Bin heliostat optical efficiency
                        if (opt_eta_point <= 0.10)
                            bin0_10++;
                        else if (opt_eta_point <= 0.20)
                            bin11_20++;
                        else if (opt_eta_point <= 0.30)
                            bin21_30++;
                        else if (opt_eta_point <= 0.40)
                            bin31_40++;
                        else if (opt_eta_point <= 0.50)
                            bin41_50++;
                        else if (opt_eta_point <= 0.60)
                            bin51_60++;
                        else if (opt_eta_point <= 0.70)
                            bin61_70++;
                        else if (opt_eta_point <= 0.80)
                            bin71_80++;
                        else if (opt_eta_point <= 0.90)
                            bin81_90++;
                        else
                            bin91_100++;

                    }
                    else
                    {
                        opt_eta_dni_product = 0.0;

                        csp_available_relval_ts = 0.0;		// kW/m2
                        csp_produced_relval_ts = 0.0;

                        pv_available_relval_ts = 0.0;
                        pv_produced_relval_ts = 0.0;
                    }


                    sum_opt_eta_dni_product += opt_eta_dni_product * ts_hour;						// kWh/m2
                    sum_csp_available_relval_ts += csp_available_relval_ts * ts_hour;				// kWeh/m2
                    sum_csp_produced_relval_ts += csp_produced_relval_ts * ts_hour;				// kWeh/m2
                    sum_pv_available_relval_ts += pv_available_relval_ts * ts_hour;				// kWeh/m2
                    sum_pv_produced_relval_ts += pv_produced_relval_ts * ts_hour;					// kWeh/m2


                    idx++;		// Add to weatherfile line counter and repeat
                }

                //hel_csp_to_rec_hourly(index_nh, hour) = sum_opt_eta_dni_product;		// kW/m2
                hel_csp_to_rec_annual[index_nh] += sum_opt_eta_dni_product;			// kW/m

                //csp_available_relval_hourly(index_nh, hour) = sum_csp_available_relval_ts;
                csp_available_relval_annual[index_nh] += sum_csp_available_relval_ts;

                //csp_produced_relval_hourly(index_nh, hour) = sum_csp_produced_relval_ts;
                csp_produced_relval_annual[index_nh] += sum_csp_produced_relval_ts;

                //pv_available_relval_hourly(index_nh, hour) = sum_pv_available_relval_ts;
                pv_available_relval_annual[index_nh] += sum_pv_available_relval_ts;

                //pv_produced_relval_hourly(index_nh, hour) = sum_pv_produced_relval_ts;
                pv_produced_relval_annual[index_nh] += sum_pv_produced_relval_ts;



                hour++;		// Step through another hour and repeat
            }

            double sun_up_total = (double)sun_up_hours;

            annual_bin0_10[index_nh] = (double)bin0_10 / sun_up_total;
            annual_bin11_20[index_nh] = (double)bin11_20 / sun_up_total;
            annual_bin21_30[index_nh] = (double)bin21_30 / sun_up_total;
            annual_bin31_40[index_nh] = (double)bin31_40 / sun_up_total;
            annual_bin41_50[index_nh] = (double)bin41_50 / sun_up_total;
            annual_bin51_60[index_nh] = (double)bin51_60 / sun_up_total;
            annual_bin61_70[index_nh] = (double)bin61_70 / sun_up_total;
            annual_bin71_80[index_nh] = (double)bin71_80 / sun_up_total;
            annual_bin81_90[index_nh] = (double)bin81_90 / sun_up_total;
            annual_bin91_100[index_nh] = (double)bin91_100 / sun_up_total;

        }	// Go to next heliostat


        // Write outputs
        std::string hel_performance_processed = file_dir + "/hel_performance_processed_static.txt";

        ofstream hel_output_file(hel_performance_processed.c_str());

        std::string out_line = "heliostat id,azimuth,radius,Q_deliver_hel (kWh_per_m2),CSP_avail_relval (kWeh),CSP_produced_relval (kWeh),PV_avail_relval (kWeh),PV_produced_relval (kWeh),bin0-11,bin11-20,bin21-30,bin31-40,bin41-50,bin51-60,bin61-70,bin71-80,bin81-90,bin91-100\n";

        hel_output_file << out_line;

        for (int i = 0; i < n_heliostats_test_radius; i++)
        {
            hel_output_file << to_string(hel_id_test_radius_az_sorted[i]) + "," + to_string(az_hel[hel_id_test_radius_az_sorted[i]]) +
                "," + to_string(r_hel[hel_id_test_radius_az_sorted[i]]) +
                "," + to_string(hel_csp_to_rec_annual[i]) +
                "," + to_string(csp_available_relval_annual[i] * availability_csp) +
                "," + to_string(csp_produced_relval_annual[i] * availability_csp) +
                "," + to_string(pv_available_relval_annual[i]) +
                "," + to_string(availability_csp * (pv_produced_relval_annual[i] - pv_available_relval_annual[i]) + pv_available_relval_annual[i]) +
                "," + to_string(annual_bin0_10[i]) +
                "," + to_string(annual_bin11_20[i]) +
                "," + to_string(annual_bin21_30[i]) +
                "," + to_string(annual_bin31_40[i]) +
                "," + to_string(annual_bin41_50[i]) +
                "," + to_string(annual_bin51_60[i]) +
                "," + to_string(annual_bin61_70[i]) +
                "," + to_string(annual_bin71_80[i]) +
                "," + to_string(annual_bin81_90[i]) +
                "," + to_string(annual_bin91_100[i]) + "\n";
        }

        assign("a", 1.23456);

    }	// End of ssc exec()

};

DEFINE_MODULE_ENTRY(sp_ty, "blah blah blah", 1);
