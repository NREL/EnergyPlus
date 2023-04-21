/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <string>
#include <vector>

#include "core.h"

#include "lib_weatherfile.h"
#include "lib_sandia.h"
#include "lib_irradproc.h"

// for adjustment factors
#include "common.h"


#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

static var_info _cm_vtab_hcpv[] = {
    /*VARTYPE           DATATYPE         NAME                               LABEL                                                              UNITS          META       GROUP            REQUIRED_IF             CONSTRAINTS                      UI_HINTS*/
    { SSC_INPUT,        SSC_STRING,      "file_name",                       "Weather file in TMY2, TMY3, EPW, or SMW.",                        "",            "",        "hcpv",          "*",                    "LOCAL_FILE",                    "" },
    { SSC_INPUT, SSC_NUMBER, "system_capacity", "Nameplate capacity", "kW", "", "PVWatts", "*", "", "" },

    { SSC_INPUT,        SSC_NUMBER,      "module_cell_area",                "Single cell area",                                                "cm^2",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_concentration",            "Concentration ratio",                                             "none",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_optical_error",            "Optical error factor",                                            "0..1",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_alignment_error",          "Alignment loss factor",                                           "0..1",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_flutter_loss_coeff",       "Wind flutter loss factor",                                        "0..1 per m/s","",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_a0",                       "Air mass modifier coefficient 0",                                 "none",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_a1",                       "Air mass modifier coefficient 1",                                 "none",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_a2",                       "Air mass modifier coefficient 2",                                 "none",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_a3",                       "Air mass modifier coefficient 3",                                 "none",        "",        "hcpv",          "*",                    "",                              "" },
    { SSC_INPUT,        SSC_NUMBER,      "module_a4",                       "Air mass modifier coefficient 4",                                 "none",        "",        "hcpv",          "*",                    "",                              "" },
    //	{ SSC_INPUT,        SSC_NUMBER,      "module_mjeff1",                   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "xxx",         "",        "hcpv",          "*",                    "",                              "" },
    //	{ SSC_INPUT,        SSC_NUMBER,      "module_mjeff2",                   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "xxx",         "",        "hcpv",          "*",                    "",                              "" },
    //	{ SSC_INPUT,        SSC_NUMBER,      "module_mjeff3",                   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "xxx",         "",        "hcpv",          "*",                    "",                              "" },
    //	{ SSC_INPUT,        SSC_NUMBER,      "module_mjeff4",                   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "xxx",         "",        "hcpv",          "*",                    "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "module_ncells",                   "Number of cells",                                                 "none",        "",        "hcpv",          "*",                    "INTEGER",                       "" },
        { SSC_INPUT,        SSC_ARRAY,       "module_mjeff",                    "Module junction efficiency array",                                "percent",     "",        "hcpv",          "*",                    "",                              "" },
        { SSC_INPUT,        SSC_ARRAY,       "module_rad",                      "POA irradiance array",                                            "W/m^2",       "",        "hcpv",          "*",                    "",                              "" },
        //	{ SSC_INPUT,        SSC_NUMBER,      "module_rad1",                     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "none",        "",        "hcpv",          "*",                    "",                              "" },
        //	{ SSC_INPUT,        SSC_NUMBER,      "module_rad2",                     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "none",        "",        "hcpv",          "*",                    "",                              "" },
        //	{ SSC_INPUT,        SSC_NUMBER,      "module_rad3",                     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "none",        "",        "hcpv",          "*",                    "",                              "" },
        //	{ SSC_INPUT,        SSC_NUMBER,      "module_rad4",                     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "none",        "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "module_reference",                "Index in arrays of the reference condition",                      "none",        "",        "hcpv",          "*",                    "INTEGER",                       "" },
            { SSC_INPUT,        SSC_NUMBER,      "module_a",                        "Equation variable (a), at high irradiance & low wind speed",      "none",        "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "module_b",                        "Equation variable (b), rate at which module temp drops",          "none",        "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "module_dT",                       "Equation variable (dT), temp diff between heat sink & cell",      "C",           "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "module_temp_coeff",               "Temperature coefficient",                                         "%/C",         "",        "hcpv",          "*",                    "",                              "" },

            // sandia grid connected inverter parameters
            //VARTYPE           DATATYPE         NAME                               LABEL                                                                                                                                                                                              UNITS     META       GROUP            REQUIRED_IF             CONSTRAINTS                      UI_HINTS
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_c0",                      "Parameter defining the curvature (parabolic) of the relationship between ac-power and dc-power at the reference operating condition, default value of zero gives a linear relationship, (1/W)",   "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_c1",                      "Empirical coefficient allowing Pdco to vary linearly with dc-voltage input, default value is zero, (1/V)",                                                                                        "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_c2",                      "Empirical coefficient allowing Pso to vary linearly with dc-voltage input, default value is zero, (1/V)",                                                                                         "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_c3",                      "Empirical coefficient allowing Co to vary linearly with dc-voltage input, default value is zero, (1/V)",                                                                                           "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_paco",                    "W maximum ac-power rating for inverter at reference or nominal operating condition, assumed to be an upper limit value, (W)",                                                                   "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_pdco",                    "W dc-power level at which the ac-power rating is achieved at the reference operating condition, (W)",                                                                                            "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_pnt",                     "W ac-power consumed by inverter at night (night tare) to maintain circuitry required to sense PV array voltage, (W)",                                                                             "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_pso",                     "W dc-power required to start the inversion process, or self-consumption by inverter, strongly influences inverter efficiency at low power levels, (W)",                                           "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdco",                    "V (Vnom) dc-voltage level at which the ac-power rating is achieved at the reference operating condition, (V)",                                                                                    "xxx",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdcmax",                  "V (Vdcmax) dc-voltage maximum operating voltage, (V)",                                                                                                                                            "xxx",    "",        "hcpv",          "*",                    "",                              "" },

            // array parameters
            //VARTYPE           DATATYPE         NAME                               LABEL                                                              UNITS     META       GROUP            REQUIRED_IF             CONSTRAINTS                      UI_HINTS
            { SSC_INPUT,        SSC_NUMBER,      "array_modules_per_tracker",       "Modules on each tracker",                                         "none",   "",        "hcpv",          "*",                    "INTEGER",                       "" },
            { SSC_INPUT,        SSC_NUMBER,      "array_num_trackers",              "Number of trackers",                                              "none",   "",        "hcpv",          "*",                    "INTEGER",                       "" },
            { SSC_INPUT,        SSC_NUMBER,      "array_num_inverters",             "Number of inverters",                                             "none",   "",        "hcpv",          "*",                    "",                              "" },

            { SSC_INPUT,        SSC_NUMBER,      "array_wind_stow_speed",           "Allowed wind speed before stowing",                               "m/s",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "array_tracker_power_fraction",    "Single tracker power fraction",                                   "0..1",   "",        "hcpv",          "*",                    "",                              "" },

            { SSC_INPUT,        SSC_NUMBER,      "array_rlim_el_min",               "Tracker minimum elevation angle",                                 "deg",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "array_rlim_el_max",               "Tracker maximum elevation angle",                                 "deg",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "array_rlim_az_min",               "Tracker minimum azimuth angle",                                   "deg",    "",        "hcpv",          "*",                    "",                              "" },
            { SSC_INPUT,        SSC_NUMBER,      "array_rlim_az_max",               "Tracker maximum azimuth angle",                                   "deg",    "",        "hcpv",          "*",                    "",                              "" },

            { SSC_INPUT,        SSC_NUMBER,      "array_enable_azalt_sf",           "Boolean for irradiance derate",                                   "0-1",    "",        "hcpv",          "*",                    "INTEGER",                       "" },
            //{ SSC_INPUT,        SSC_ARRAY,       "array_azalt_sf",                  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",                          "xxx",    "",        "hcpv",          "*",                    "",                              "" },
              { SSC_INPUT,        SSC_MATRIX,      "azaltsf",                         "Azimuth-Altitude Shading Table",                                  "",       "",        "hcpv",          "*",                    "",                              "" },

              // derates and losses
              //VARTYPE           DATATYPE         NAME                               LABEL                                                              UNITS     META       GROUP            REQUIRED_IF             CONSTRAINTS                      UI_HINTS
              { SSC_INPUT,        SSC_ARRAY,       "array_monthly_soiling",           "Monthly soiling factors array",                                   "0..1",   "",        "hcpv",          "*",                    "",                              "" },
              { SSC_INPUT,        SSC_NUMBER,      "array_dc_mismatch_loss",          "DC module mismatch loss factor",                                  "0..1",   "",        "hcpv",          "*",                    "",                              "" },
              { SSC_INPUT,        SSC_NUMBER,      "array_dc_wiring_loss",            "DC Wiring loss factor",                                           "0..1",   "",        "hcpv",          "*",                    "",                              "" },
              { SSC_INPUT,        SSC_NUMBER,      "array_diode_conn_loss",           "Diodes and connections loss factor",                              "0..1",   "",        "hcpv",          "*",                    "",                              "" },
              { SSC_INPUT,        SSC_NUMBER,      "array_ac_wiring_loss",            "AC wiring loss factor",                                           "0..1",   "",        "hcpv",          "*",                    "",                              "" },
              { SSC_INPUT,        SSC_NUMBER,      "array_tracking_error",            "General racking error",                                           "0..1",   "",        "hcpv",          "*",                    "",                              "" },

              // outputs
              //VARTYPE           DATATYPE         NAME                               LABEL                                                              UNITS     META       GROUP            REQUIRED_IF             CONSTRAINTS                      UI_HINTS
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_solazi",                   "Hourly solar azimuth",                                            "deg",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_solzen",                   "Hourly solar zenith",                                             "deg",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_sazi",                     "Tracker azimuth",                                                 "deg",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_stilt",                    "Tracker tilt",                                                    "deg",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_sunup",                    "Sun up? (0/1)",                                                   "0 or 1", "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_beam",                     "Beam irradiance",                                                 "kW/m2",  "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_tdry",                     "Ambient dry bulb temperature",                                    "C",      "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_windspd",                  "Wind speed",                                                      "m/s",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_airmass",                  "Relative air mass",                                               "none",   "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_shading_derate",           "Shading derate",                                                  "none",   "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_poa",                      "POA on cell",                                                     "W/m2",   "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_input_radiation",          "Input radiation",                                                 "kWh",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_tmod",                     "Module backplate temp",                                           "C",      "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_tcell",                    "Cell temperature",                                                "C",      "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_celleff",                  "Cell efficiency",                                                 "%",      "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_modeff",                   "Module efficiency",                                               "%",      "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_dc",                       "DC gross",                                                        "kWh",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_dc_net",                   "DC net",                                                          "kWh",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              { SSC_OUTPUT,        SSC_ARRAY,      "hourly_ac",                       "AC gross",                                                        "kWh",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },
              //	{ SSC_OUTPUT,        SSC_ARRAY,      "hourly_energy",                   "Hourly Energy",                                                   "kWh",    "",        "Hourly",          "*",                    "LENGTH=8760",                              "" },

                  // monthly outputs
                  { SSC_OUTPUT,        SSC_ARRAY,      "monthly_energy",                  "Monthly Energy",                                                  "kWh",    "",        "Monthly",          "*",                   "LENGTH=12",                                 "" },
                  { SSC_OUTPUT,        SSC_ARRAY,      "monthly_beam",                    "Beam irradiance",                                                 "kW/m2",  "",        "Monthly",          "*",                   "LENGTH=12",                                 "" },
                  { SSC_OUTPUT,        SSC_ARRAY,      "monthly_input_radiation",         "Input radiation",                                                 "kWh",    "",        "Monthly",          "*",                   "LENGTH=12",                                 "" },
                  { SSC_OUTPUT,        SSC_ARRAY,      "monthly_dc_net",                  "DC net",                                                          "kWh",    "",        "Monthly",          "*",                   "LENGTH=12",                                 "" },


                  // annual outputs
                  { SSC_OUTPUT,        SSC_NUMBER,     "annual_energy",                   "Annual Energy",                                                   "kWh",    "",        "Annual",          "*",                   "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "annual_beam",                     "Beam irradiance",                                                 "kW/m2",  "",        "Annual",          "*",                   "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "annual_input_radiation",          "Input radiation",                                                 "kWh",    "",        "Annual",          "*",                   "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "annual_dc",                       "DC gross",                                                        "kWh",    "",        "Annual",          "*",                   "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "annual_dc_net",                   "DC net",                                                          "kWh",    "",        "Annual",          "*",                   "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "annual_ac",                       "AC gross",                                                        "kWh",    "",        "Annual",          "*",                   "",                                         "" },

                  // Other single value outputs
                  { SSC_OUTPUT,        SSC_NUMBER,     "tracker_nameplate_watts",         "Tracker nameplate",                                               "watts",  "",        "Miscellaneous",          "*",                    "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "modeff_ref",                      "Module efficiency",                                               "-",      "",        "Miscellaneous",          "*",                    "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "dc_loss_stowing_kwh",             "Annual stowing power loss",                                       "kWh",    "",        "Annual",          "*",                    "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "ac_loss_tracker_kwh",             "Annual tracker power loss",                                       "kWh",    "",        "Annual",          "*",                    "",                                         "" },
                  { SSC_OUTPUT,        SSC_NUMBER,     "dc_nominal",                      "Annual DC nominal",                                               "kWh",    "",        "Annual",          "*",                    "",                                         "" },

                  { SSC_OUTPUT, SSC_NUMBER, "capacity_factor", "Capacity factor", "%", "", "", "*", "", "" },
                  { SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw", "Energy yield", "kWh/kW", "", "", "*", "", "" },


              var_info_invalid };

class cm_hcpv : public compute_module
{
public:

    cm_hcpv()
    {
        add_var_info(_cm_vtab_hcpv);
        // performance adjustment factors
        add_var_info(vtab_adjustment_factors);
        add_var_info(vtab_technology_outputs);
    }

    double eff_interpolate(double irrad, ssc_number_t* rad, ssc_number_t* eff, int count)
    {
        if (irrad < rad[0])
            return eff[0];
        else if (irrad > rad[count - 1])
            return eff[count - 1];

        int i = 1;
        for (i = 1; i < count; i++)
            if (irrad < rad[i]) break;

        int i1 = i - 1;

        double wx = (irrad - rad[i1]) / (rad[i] - rad[i1]);
        return (1 - wx) * eff[i1] + wx * eff[i];
    }

    double azaltinterp(double azimuth, double altitude, const util::matrix_t<ssc_number_t>& azaltvals)
    {
        int r = (int)azaltvals.nrows();
        int c = (int)azaltvals.ncols();

        int i, j;
        double reduc = 1.0;

        if (azimuth < 0 || azimuth > 360 || altitude < 0 || altitude > 90) return reduc;

        int alt_l = 1;
        int azi_l = 1;
        double alt_d = 0;
        double azi_d = 0;
        std::vector<double> x(2);
        std::vector<double> y(2);

        double fQ[2][2] = { { 1.0, 1.0 }, { 1.0, 1.0 } };
        //Mat2D<double> fQ(2, 2, 1.0);

        for (i = 0; i < 2; i++)
        {
            x[i] = 1.0;
            y[i] = 1.0;
        }

        for (i = 1; i < r; i++)
        {
            if ((azaltvals.at(i, 0) - altitude) > 0)
            {
                alt_l = i;
                if (i == r - 1) alt_d = 0;
                else alt_d = azaltvals.at(i, 0) - altitude;

            }
        }

        for (i = 1; i < c; i++)
        {
            if (azimuth - azaltvals.at(0, i) > 0)
            {
                azi_l = i;
                if (i == c - 1) azi_d = 0;
                else azi_d = azimuth - azaltvals.at(0, i);
            }
        }

        if (alt_d == 0 && azi_d == 0) reduc = azaltvals.at(alt_l, azi_l);
        else if (alt_d == 0) reduc = azaltvals.at(alt_l, azi_l) +
            ((azaltvals.at(alt_l, azi_l + 1) - (azaltvals.at(alt_l, azi_l)))
                / (azaltvals.at(0, azi_l + 1) - (azaltvals.at(0, azi_l)))) * azi_d;
        else if (azi_d == 0) reduc = azaltvals.at(alt_l, azi_l) +
            ((azaltvals.at(alt_l + 1, azi_l) - (azaltvals.at(alt_l, azi_l))) /
                (azaltvals.at(alt_l + 1, 0) - (azaltvals.at(alt_l, 0)))) * alt_d;
        else
        {
            for (i = 0; i < 2; i++)
                for (j = 0; j < 2; j++)
                    fQ[i][j] = azaltvals.at(alt_l + i, azi_l + j);

            for (i = 0; i < 2; i++)
            {
                x.at(i) = azaltvals.at(alt_l + i, 0);
                y.at(i) = azaltvals.at(0, azi_l + i);
            }

            if (x.at(1) - x.at(0) == 0 && y.at(1) - y.at(0) == 0) reduc = azaltvals.at(alt_l, azi_l);
            else if (x.at(1) - x.at(0) == 0) reduc = azaltvals.at(alt_l, azi_l) +
                ((azaltvals.at(alt_l, azi_l + 1) - (azaltvals.at(alt_l, azi_l)))
                    / (azaltvals.at(0, azi_l + 1) - (azaltvals.at(0, azi_l)))) * azi_d;
            else if (y.at(1) - y.at(0) == 0) reduc = azaltvals.at(alt_l, azi_l) +
                ((azaltvals.at(alt_l + 1, azi_l) - (azaltvals.at(alt_l, azi_l))) /
                    (azaltvals.at(alt_l + 1, 0) - (azaltvals.at(alt_l, 0)))) * alt_d;
            else reduc = (fQ[0][0] / ((x.at(1) - x.at(0)) * (y.at(1) - y.at(0)))) * (x.at(1) - altitude) * (y.at(1) - azimuth)
                + (fQ[1][0] / ((x.at(1) - x.at(0)) * (y.at(1) - y.at(0)))) * (altitude - x.at(0)) * (y.at(1) - azimuth)
                + (fQ[0][1] / ((x.at(1) - x.at(0)) * (y.at(1) - y.at(0)))) * (x.at(1) - altitude) * (azimuth - y.at(0))
                + (fQ[1][1] / ((x.at(1) - x.at(0)) * (y.at(1) - y.at(0)))) * (altitude - x.at(0)) * (azimuth - y.at(0));
        }

        return reduc;
    }

    void exec()
    {
        // open the weather file
        // define variables consistent across subarrays

        weatherfile wFile(as_string("file_name"));
        if (!wFile.ok()) throw exec_error("hcpv", wFile.message());
        if (wFile.has_message()) log(wFile.message(), SSC_WARNING);

        weather_record wf;

        if (wFile.nrecords() != 8760) throw exec_error("hcpv", "pv simulator only accepts hourly weather data");

        double concen = as_double("module_concentration");
        int ncells = as_integer("module_ncells");
        double cellarea = as_double("module_cell_area") * 0.0001; // convert to m2 
        double modarea = concen * cellarea * ncells; //* m2 *

        size_t rad_count = 0, eff_count = 0;
        ssc_number_t* dnrad = as_array("module_rad", &rad_count);
        ssc_number_t* mjeff = as_array("module_mjeff", &eff_count);
        if (rad_count != eff_count)
            throw exec_error("hcpv", "hcpv model radiation and efficiency arrays must have the same number of values");

        for (size_t i = 0; i < rad_count; i++)
        {
            if (i > 0 && dnrad[i] <= dnrad[i - 1])
                throw exec_error("hcpv", "hcpv model radiation levels must increase monotonically");
        }

        int refidx = as_integer("module_reference");
        if (refidx < 0 || refidx >= (int)rad_count)
            throw exec_error("hcpv", util::format("invalid reference condition, [0..%d] reqd", rad_count - 1));

        double Ib_ref = dnrad[refidx];
        double MJeff_ref = mjeff[refidx];

        double a = as_double("module_a");
        double b = as_double("module_b");
        double dT = as_double("module_dT");
        double gamma = as_double("module_temp_coeff");

        double a0 = as_double("module_a0");
        double a1 = as_double("module_a1");
        double a2 = as_double("module_a2");
        double a3 = as_double("module_a3");
        double a4 = as_double("module_a4");

        double optic_error = as_double("module_optical_error");
        double align_error = as_double("module_alignment_error");
        double flutter_loss = as_double("module_flutter_loss_coeff");

        double mam_ref = a0 + a1 * 1.5 + a2 * 2.25 + a3 * 5.0625 + a4 * 7.59375;
        double modeff_ref = MJeff_ref * optic_error * align_error * mam_ref * (1 - flutter_loss * 4);

        // array
        int modules_per_tracker = as_integer("array_modules_per_tracker");
        int ntrackers = as_integer("array_num_trackers");
        double ninverters = as_double("array_num_inverters");

        double stow_wspd = as_double("array_wind_stow_speed");
        double track_pwr_frac = as_double("array_tracker_power_fraction");

        double tracker_nameplate_watts = modules_per_tracker * modarea * Ib_ref * modeff_ref / 100.0;

        size_t soil_len = 0;
        ssc_number_t* soiling = as_array("array_monthly_soiling", &soil_len); // monthly soiling array
        if (soil_len != 12)
            throw exec_error("hcpv", "soiling derate must have 12 values");

        double mismatch_loss = as_double("array_dc_mismatch_loss");
        double dcwiring_loss = as_double("array_dc_wiring_loss");
        double diodeconn_loss = as_double("array_diode_conn_loss");
        double tracking_err = as_double("array_tracking_error");
        double acwiring_loss = as_double("array_ac_wiring_loss");

        double azmin = as_double("array_rlim_az_min");
        double azmax = as_double("array_rlim_az_max");
        double elmin = as_double("array_rlim_el_min");
        double elmax = as_double("array_rlim_el_max");

        // inverter
        sandia_inverter_t snlinv;
        snlinv.Paco = as_double("inv_snl_paco");
        snlinv.Pdco = as_double("inv_snl_pdco");
        snlinv.Vdco = as_double("inv_snl_vdco");
        snlinv.Pso = as_double("inv_snl_pso");
        snlinv.Pntare = as_double("inv_snl_pnt");
        snlinv.C0 = as_double("inv_snl_c0");
        snlinv.C1 = as_double("inv_snl_c1");
        snlinv.C2 = as_double("inv_snl_c2");
        snlinv.C3 = as_double("inv_snl_c3");

        ssc_number_t* p_solazi = allocate("hourly_solazi", 8760);
        ssc_number_t* p_solzen = allocate("hourly_solzen", 8760);
        ssc_number_t* p_sazi = allocate("hourly_sazi", 8760);
        ssc_number_t* p_stilt = allocate("hourly_stilt", 8760);
        ssc_number_t* p_sunup = allocate("hourly_sunup", 8760);
        ssc_number_t* p_beam = allocate("hourly_beam", 8760);
        ssc_number_t* p_tdry = allocate("hourly_tdry", 8760);
        ssc_number_t* p_wspd = allocate("hourly_windspd", 8760);
        ssc_number_t* p_airmass = allocate("hourly_airmass", 8760);
        ssc_number_t* p_sf = allocate("hourly_shading_derate", 8760);
        ssc_number_t* p_poa = allocate("hourly_poa", 8760);
        ssc_number_t* p_inprad = allocate("hourly_input_radiation", 8760);
        ssc_number_t* p_tmod = allocate("hourly_tmod", 8760);
        ssc_number_t* p_tcell = allocate("hourly_tcell", 8760);
        ssc_number_t* p_celleff = allocate("hourly_celleff", 8760);
        ssc_number_t* p_modeff = allocate("hourly_modeff", 8760);
        ssc_number_t* p_dc = allocate("hourly_dc", 8760);
        ssc_number_t* p_dcnet = allocate("hourly_dc_net", 8760);
        ssc_number_t* p_ac = allocate("hourly_ac", 8760);
        ssc_number_t* p_enet = allocate("gen", 8760); // kWh
//		ssc_number_t *p_gen = allocate("gen", 8760); // kW

        double dc_loss_stowing = 0;
        double ac_loss_tracker = 0;


        adjustment_factors haf(this, "adjust");
        if (!haf.setup())
            throw exec_error("pvwattsv5", "failed to setup adjustment factors: " + haf.error());



        double dTS = 1.0; // hourly timesteps
        int istep = 0, nstep = (int)wFile.nrecords();
        while (wFile.read(&wf) && istep < 8760)
        {
            // send progress update notification to any callback
            if (istep % (nstep / 20) == 0)
                update("", 100.0f * ((float)istep) / ((float)nstep), (float)istep);

            irrad irr;
            irr.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute, dTS);
            irr.set_location(wFile.lat(), wFile.lon(), wFile.tz());
            irr.set_optional(wFile.elev(), wf.pres, wf.tdry);
            irr.set_sky_model(0, 0.2); // isotropic sky, 0.2 albedo (doesn't matter for CPV) and diffuse shading factor not enabled (set to 1.0 by default)
            irr.set_beam_diffuse(wf.dn, wf.df);
            irr.set_surface(2, 0, 0, 90, true, -1, false, 0.0); // 2 axis tracking, other parameters don't matter
            int code = irr.calc();

            if (code < 0)
                throw exec_error("hcpv", util::format("failed to compute irradiation on surface (code: %d)", code));


            double poa;
            irr.get_poa(&poa, 0, 0, 0, 0, 0);

            // apply monthly soiling factor to incident poa
            int midx = wf.month - 1;
            //			double soiling_factor = 1.0;
            if (midx >= 0 && midx < 12)
                poa *= soiling[midx];

            double aoi, stilt, sazi;
            irr.get_angles(&aoi, &stilt, &sazi, 0, 0);

            if (stilt < elmin)
            {
                stilt = elmin;
                poa = 0;
            }

            if (stilt > elmax)
            {
                stilt = elmax;
                poa = 0;
            }


            if (wFile.lat() < 0) // southern hemisphere
            {
                if (sazi < azmin && sazi > azmax)
                {
                    sazi = (sazi < 180) ? azmax : azmin;
                    poa = 0;
                }
            }
            else
            {
                if (sazi < azmin)
                {
                    sazi = azmin;
                    poa = 0;
                }

                if (sazi > azmax)
                {
                    sazi = azmax;
                    poa = 0;
                }
            }

            // get sun position
            double solazi, solzen, solalt;
            int sunup;
            irr.get_sun(&solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0);

            double shad_derate = 1;
            bool en_azaltsf = (as_integer("array_enable_azalt_sf") == 1);


            if (sunup > 0)
            {
                util::matrix_t<ssc_number_t> azaltsf;
                if (!get_matrix("azaltsf", azaltsf))
                    throw exec_error("hcpv", "could not get the azimuth-altitude shading table from the SSC interface");

                // derate irradiance on cell based on optical/alignment errors
                if (en_azaltsf)
                    shad_derate = azaltinterp(solazi, solalt, azaltsf);

                poa *= shad_derate;
                poa *= optic_error;
                poa *= align_error;

                // spectral correction for airmass
                double air_mass = 1 / (cos(solzen * M_PI / 180) + 0.5057 * pow(96.080 - solzen, -1.634));
                air_mass *= exp(-0.0001184 * wFile.elev()); // correction for elevation (m), as applied in Sandia PV model
                double air_mass_modifier = a0 + a1 * air_mass + a2 * pow(air_mass, 2) + a3 * pow(air_mass, 3) + a4 * pow(air_mass, 4);
                poa *= air_mass_modifier;

                // compute module power (max power point)

                double celleff = eff_interpolate(wf.dn, dnrad, mjeff, (int)rad_count);
                double cellpwr = (celleff / 100.0 * poa * concen * cellarea);

                // todo: check reference conditions for 20 'C tdry and 4 m/s wspd
                double tmod = sandia_celltemp_t::sandia_module_temperature(wf.dn, wf.wspd, wf.tdry, 0, a, b);
                double tcell = sandia_celltemp_t::sandia_tcell_from_tmodule(tmod, wf.dn, 0, dT);

                cellpwr += cellpwr * (gamma / 100.0) * (tcell - 20.0);
                if (cellpwr < 0) cellpwr = 0;

                // wind flutter loss
                cellpwr *= (1 - flutter_loss * wf.wspd);

                // array and derates
                double dcgross = cellpwr * ncells * modules_per_tracker * ntrackers;
                double dcv = snlinv.Vdco; // todo: arbitrary DC voltage.  this is "optimal"

                double modeff = 0.0;
                if (poa > 0) modeff = 100 * dcgross / (poa * modarea * modules_per_tracker * ntrackers);

                // dc derates
                double dcpwr = dcgross;

                // stowing
                if (wf.wspd >= stow_wspd)
                {
                    dc_loss_stowing += dcpwr;
                    dcpwr = 0;
                }
                else
                {
                    dcpwr *= mismatch_loss;
                    dcpwr *= dcwiring_loss;
                    dcpwr *= diodeconn_loss;
                    dcpwr *= tracking_err;
                }

                // inverter model
                double _par, _plr, acgross, aceff, cliploss, psoloss, pntloss;
                snlinv.acpower(dcpwr / ninverters, dcv, &acgross, &_par, &_plr, &aceff, &cliploss, &psoloss, &pntloss);
                acgross *= ninverters;

                double acpwr = acgross;

                // ac derates
                acpwr *= acwiring_loss;

                // tracker power
                double tracker_power = ntrackers * track_pwr_frac * tracker_nameplate_watts * dTS; // dTS is the timestep in hours
                ac_loss_tracker += tracker_power;
                acpwr -= tracker_power;

                p_solazi[istep] = (ssc_number_t)solazi;
                p_solzen[istep] = (ssc_number_t)solzen;
                p_sazi[istep] = (ssc_number_t)sazi;
                p_stilt[istep] = (ssc_number_t)stilt;
                p_sunup[istep] = (ssc_number_t)sunup;
                p_airmass[istep] = (ssc_number_t)air_mass;
                p_poa[istep] = (ssc_number_t)poa;
                p_inprad[istep] = (ssc_number_t)(wf.dn * modarea * modules_per_tracker * ntrackers * 0.001); // kWh
                p_tmod[istep] = (ssc_number_t)tmod;
                p_tcell[istep] = (ssc_number_t)tcell;
                p_celleff[istep] = (ssc_number_t)celleff;
                p_modeff[istep] = (ssc_number_t)modeff;
                p_dc[istep] = (ssc_number_t)(dcgross * 0.001); // kwh
                p_dcnet[istep] = (ssc_number_t)(dcpwr * 0.001); // kwh
                p_ac[istep] = (ssc_number_t)(acgross * 0.001); // kwh
                p_enet[istep] = (ssc_number_t)(acpwr * 0.001 * haf(istep)); // kwh
//				p_gen[istep] = (ssc_number_t)(acpwr * 0.001 * haf(istep)); // kwh

            }

            // record at all hours (pass through from weather file)
            p_beam[istep] = (ssc_number_t)wf.dn;
            p_tdry[istep] = (ssc_number_t)wf.tdry;
            p_wspd[istep] = (ssc_number_t)wf.wspd;
            p_sf[istep] = (ssc_number_t)shad_derate;

            istep++;
        }

        if (istep != 8760)
            throw exec_error("hcpv", util::format("failed to simulate all 8760 hours"));

        // annual accumulations
        accumulate_annual("gen", "annual_energy");
        accumulate_annual("hourly_beam", "annual_beam");
        accumulate_annual("hourly_input_radiation", "annual_input_radiation");
        accumulate_annual("hourly_dc", "annual_dc");
        accumulate_annual("hourly_dc_net", "annual_dc_net");
        accumulate_annual("hourly_ac", "annual_ac");

        // monthly accumulations
        accumulate_monthly("gen", "monthly_energy");
        accumulate_monthly("hourly_beam", "monthly_beam");
        accumulate_monthly("hourly_input_radiation", "monthly_input_radiation");
        accumulate_monthly("hourly_dc_net", "monthly_dc_net");

        // Other single value outputs
        assign("tracker_nameplate_watts", var_data((ssc_number_t)tracker_nameplate_watts));
        assign("dc_loss_stowing_kwh", var_data((ssc_number_t)(dc_loss_stowing * 0.001)));
        assign("ac_loss_tracker_kwh", var_data((ssc_number_t)(ac_loss_tracker * 0.001)));
        assign("modeff_ref", var_data((ssc_number_t)modeff_ref));

        ssc_number_t inp_rad = as_number("annual_input_radiation");
        assign("dc_nominal", var_data((ssc_number_t)(modeff_ref * inp_rad / 100.0)));

        // metric outputs moved to technology
        double kWhperkW = 0.0;
        double nameplate = as_double("system_capacity");
        double annual_energy = 0.0;
        for (int i = 0; i < 8760; i++)
            annual_energy += p_enet[i];
        if (nameplate > 0) kWhperkW = annual_energy / nameplate;
        assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
        assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));

    }
};

DEFINE_MODULE_ENTRY(hcpv, "High-X Concentrating PV, SAM component models V.1", 1)
