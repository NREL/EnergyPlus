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

#include "core.h"
#include "common.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_pvwatts.h"
#include "lib_pvshade.h"
#include "lib_util.h"



static var_info _cm_vtab_pvwattsv1[] = {
    /*   VARTYPE           DATATYPE         NAME                         LABEL                                               UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        { SSC_INPUT,        SSC_STRING,      "solar_resource_file",             "local weather file path",                     "",       "",                        "Weather",      "*",                       "LOCAL_FILE",      "" },

        { SSC_INPUT,        SSC_NUMBER,      "albedo",                         "Albedo (ground reflectance)",                 "frac",   "",                        "PVWatts",      "?",                       "",                                         "" },
        { SSC_INPUT,        SSC_NUMBER,      "system_size",                    "Nameplate capacity",                          "kW",     "",                        "PVWatts",      "*",                       "",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "derate",                         "System derate value",                         "frac",   "",                        "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "track_mode",                     "Tracking mode",                               "0/1/2/3","Fixed,1Axis,2Axis,AziAxis","PVWatts",      "*",                       "MIN=0,MAX=3,INTEGER",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "azimuth",                        "Azimuth angle",                               "deg",    "E=90,S=180,W=270",        "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
        { SSC_INPUT,        SSC_NUMBER,      "tilt",                           "Tilt angle",                                  "deg",    "H=0,V=90",                "PVWatts",      "naof:tilt_eq_lat",        "MIN=0,MAX=90",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                    "Tilt=latitude override",                      "0/1",    "",                        "PVWatts",      "na:tilt",                 "BOOLEAN",                                  "" },

        /* shading inputs */
        { SSC_INPUT,        SSC_MATRIX,      "shading:timestep",               "Time step beam shading factors",                 "",       "",                        "PVWatts",      "?",                        "",                              "" },
        { SSC_INPUT,        SSC_MATRIX,      "shading:mxh",                    "Month x Hour beam shading factors",           "",       "",                        "PVWatts",      "?",                        "",                              "" },
        { SSC_INPUT,        SSC_MATRIX,      "shading:azal",                   "Azimuth x altitude beam shading factors",     "",       "",                        "PVWatts",      "?",                        "",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "shading:diff",                   "Diffuse shading factor",                      "",       "",                        "PVWatts",      "?",                        "",                              "" },

        /* advanced parameters */
        { SSC_INPUT,        SSC_NUMBER,      "enable_user_poa",                "Enable user-defined POA irradiance input",    "0/1",    "",                        "PVWatts",      "?=0",                     "BOOLEAN",                                  "" },
        { SSC_INPUT,        SSC_ARRAY,       "user_poa",                       "User-defined POA irradiance",                 "W/m2",   "",                        "PVWatts",      "enable_user_poa=1",       "LENGTH=8760",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "rotlim",                         "Tracker rotation limit (+/- 1 axis)",         "deg",    "",                        "PVWatts",      "?=45.0",                  "MIN=1,MAX=90",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "inoct",                          "Nominal operating cell temperature",          "C",      "",                        "PVWatts",      "?=45.0",                  "POSITIVE",                                 "" },
        { SSC_INPUT,        SSC_NUMBER,      "tref",                           "Reference cell temperature",                  "C",      "",                        "PVWatts",      "?=25.0",                  "POSITIVE",                                 "" },
        { SSC_INPUT,        SSC_NUMBER,      "gamma",                          "Max power temperature coefficient",           "%/C",    "",                        "PVWatts",      "?=-0.5",                  "",                                         "" },
        { SSC_INPUT,        SSC_NUMBER,      "inv_eff",                        "Inverter efficiency at rated power",          "frac",   "",                        "PVWatts",      "?=0.92",                  "MIN=0,MAX=1",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "fd",                             "Diffuse fraction",                            "0..1",   "",                        "PVWatts",      "?=1.0",                   "MIN=0,MAX=1",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "i_ref",                          "Rating condition irradiance",                 "W/m2",   "",                        "PVWatts",      "?=1000",                  "POSITIVE",                                 "" },
        { SSC_INPUT,        SSC_NUMBER,      "poa_cutin",                      "Min reqd irradiance for operation",           "W/m2",   "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },
        { SSC_INPUT,        SSC_NUMBER,      "w_stow",                         "Wind stow speed",                             "m/s",    "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },
        { SSC_INPUT,        SSC_NUMBER,      "concen",                         "Concentration ratio",                         "",       "",                        "PVWatts",      "?=1",                     "MIN=1",                                    "" },
        { SSC_INPUT,        SSC_NUMBER,      "fhconv",                         "Convective heat transfer factor",             "",       "",                        "PVWatts",      "?=1",                     "MIN=0.1",                                  "" },
        { SSC_INPUT,        SSC_NUMBER,      "shade_mode_1x",                  "Tracker self-shading mode",                   "0/1/2",  "0=shading,1=backtrack,2=none","PVWatts",  "?=2",                     "INTEGER,MIN=0,MAX=2",           "" },
        { SSC_INPUT,        SSC_NUMBER,      "gcr",                            "Ground coverage ratio",                       "0..1",   "",                            "PVWatts",  "?=0.3",                   "MIN=0,MAX=3",               "" },
        { SSC_INPUT,        SSC_NUMBER,      "ar_glass",                       "Enable anti-reflective glass coating (beta)",         "0/1",    "",                        "PVWatts",      "?=0",                     "BOOLEAN",                   "" },

        { SSC_INPUT,        SSC_NUMBER,      "u0",                           "thermal model coeff U0",                                  "",    "",                "PVWatts",      "?",        "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "u1",                           "thermal model coeff U0",                                  "",    "",                "PVWatts",      "?",        "",                             "" },



        /* outputs */

        { SSC_OUTPUT,       SSC_ARRAY,       "gh",                             "Global horizontal irradiance",                "W/m2",   "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "dn",                             "Beam irradiance",                             "W/m2",   "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "df",                             "Diffuse irradiance",                          "W/m2",   "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "tamb",                           "Ambient temperature",                         "C",      "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "tdew",                           "Dew point temperature",                       "C",      "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "wspd",                           "Wind speed",                                  "m/s",    "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },

        { SSC_OUTPUT,       SSC_ARRAY,       "poa",                            "Plane of array irradiance",                   "W/m2",   "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "tpoa",                           "Transmitted plane of array irradiance",       "W/m2",   "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "tcell",                          "Module temperature",                          "C",      "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "dc",                             "DC array output",                             "Wdc",    "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "ac",                             "AC system output",                            "Wac",    "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
        //	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",                  "Hourly energy",                               "kWh",  "",                          "Hourly",        "*",                       "LENGTH=8760",                          "" },
            { SSC_OUTPUT,       SSC_ARRAY,       "shad_beam_factor",               "Shading factor for beam radiation",           "",       "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },
            { SSC_OUTPUT,       SSC_ARRAY,       "sunup",                          "Sun up over horizon",                         "0/1",    "",                        "Hourly",        "*",                       "LENGTH=8760",                          "" },

            { SSC_OUTPUT,       SSC_ARRAY,       "poa_monthly",                    "Plane of array irradiance",                   "kWh/m2",   "",                      "Monthly",       "*",                       "LENGTH=12",                          "" },
            { SSC_OUTPUT,       SSC_ARRAY,       "solrad_monthly",                 "Daily average solar irradiance",              "kWh/m2/day","",                     "Monthly",       "*",                       "LENGTH=12",                          "" },
            { SSC_OUTPUT,       SSC_ARRAY,       "dc_monthly",                     "DC array output",                             "kWhdc",    "",                      "Monthly",       "*",                       "LENGTH=12",                          "" },
            { SSC_OUTPUT,       SSC_ARRAY,       "ac_monthly",                     "AC system output",                            "kWhac",    "",                      "Monthly",       "*",                       "LENGTH=12",                          "" },
            { SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",                 "Monthly energy",                              "kWh",      "",                      "Monthly",      "*",                        "LENGTH=12",                          "" },

            { SSC_OUTPUT,       SSC_NUMBER,      "solrad_annual",                  "Daily average solar irradiance",              "kWh/m2/day",    "",                 "Annual",        "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_NUMBER,      "ac_annual",                      "Annual AC system output",                     "kWhac",    "",                      "Annual",        "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",                  "Annual energy",                               "kWh",    "",                        "Annual",        "*",                       "",                          "" },


            { SSC_OUTPUT,       SSC_STRING,      "location",                      "Location ID",                                  "",    "",                           "Location",      "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_STRING,      "city",                          "City",                                         "",    "",                           "Location",      "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_STRING,      "state",                         "State",                                        "",    "",                           "Location",      "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_NUMBER,      "lat",                           "Latitude",                                     "deg", "",                           "Location",      "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_NUMBER,      "lon",                           "Longitude",                                    "deg", "",                           "Location",      "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_NUMBER,      "tz",                            "Time zone",                                    "hr",  "",                           "Location",      "*",                       "",                                   "" },
            { SSC_OUTPUT,       SSC_NUMBER,      "elev",                          "Site elevation",                               "m",   "",                           "Location",      "*",                       "",                                   "" },


            var_info_invalid };

class cm_pvwattsv1 : public compute_module
{
public:

    cm_pvwattsv1()
    {
        add_var_info(_cm_vtab_pvwattsv1);
        add_var_info(vtab_adjustment_factors);
        add_var_info(vtab_technology_outputs);
    }

    void exec()
    {
        const char* file = as_string("solar_resource_file");

        weatherfile wfile(file);
        if (!wfile.ok()) throw exec_error("pvwattsv1", wfile.message());
        if (wfile.has_message()) log(wfile.message(), SSC_WARNING);

        weather_header hdr;
        wfile.header(&hdr);

        double dcrate = as_double("system_size");
        double derate = as_double("derate");
        int track_mode = as_integer("track_mode"); // 0, 1, 2, 3
        double azimuth = as_double("azimuth");
        double tilt = fabs(hdr.lat);
        if (!lookup("tilt_eq_lat") || !as_boolean("tilt_eq_lat"))
            tilt = fabs(as_double("tilt"));

        ssc_number_t* p_user_poa = 0;
        if (as_boolean("enable_user_poa"))
        {
            size_t count = 0;
            p_user_poa = as_array("user_poa", &count);
            if (count != 8760) p_user_poa = 0;
        }

        ssc_number_t* p_gh = allocate("gh", 8760);
        ssc_number_t* p_dn = allocate("dn", 8760);
        ssc_number_t* p_df = allocate("df", 8760);
        ssc_number_t* p_tamb = allocate("tamb", 8760);
        ssc_number_t* p_tdew = allocate("tdew", 8760);
        ssc_number_t* p_wspd = allocate("wspd", 8760);

        ssc_number_t* p_dc = allocate("dc", 8760);
        ssc_number_t* p_ac = allocate("ac", 8760);
        ssc_number_t* p_hourly_energy = allocate("gen", 8760);
        //		ssc_number_t *p_gen = allocate("gen", 8760);
        ssc_number_t* p_tcell = allocate("tcell", 8760);
        ssc_number_t* p_poa = allocate("poa", 8760);
        ssc_number_t* p_tpoa = allocate("tpoa", 8760);

        ssc_number_t* p_shad_beam = allocate("shad_beam_factor", 8760);
        ssc_number_t* p_sunup = allocate("sunup", 8760);


        double U0 = std::numeric_limits<double>::quiet_NaN();
        double U1 = std::numeric_limits<double>::quiet_NaN();
        bool use_faiman_model = false;
        if (is_assigned("u0") && is_assigned("u1"))
        {
            use_faiman_model = true;
            U0 = as_double("u0");
            U1 = as_double("u1");
        }

        /* PV RELATED SPECIFICATIONS */

        double inoct = as_double("inoct") + 273.15; // PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
        double reftem = as_double("tref"); // PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
        double pwrdgr = as_double("gamma") / 100.0; // PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
        double efffp = as_double("inv_eff"); // PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */

        double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
        double tmloss = 1.0 - derate / efffp;  /* All losses except inverter,decimal */
        double rlim = as_double("rotlim");             /* +/- rotation in degrees permitted by physical constraint of tracker */
        double fd = as_double("fd"); // diffuse fraction
        double i_ref = as_double("i_ref"); // reference irradiance for rating condition
        double poa_cutin = as_double("poa_cutin"); // minimum POA irradiance level required for any operation
        double wind_stow = as_double("w_stow"); // maximum wind speed before stowing.  stowing causes all output to be lost
        double concen = 1.0;
        if (is_assigned("concen")) concen = as_double("concen"); // concentration ratio.  used to increase incident irradiance on cells for thermal calculaton
        double fhconv = 1.0;
        if (is_assigned("fhconv")) fhconv = as_double("fhconv"); // convective heat transfer coefficient factor.  used to approximate effect of a heatsink for lcpv
        int shade_mode_1x = 2; // no self shading on 1 axis tracker
        if (is_assigned("shade_mode_1x")) shade_mode_1x = as_integer("shade_mode_1x");
        double gcr = 0.3;
        if (is_assigned("gcr")) gcr = as_double("gcr");

        bool use_ar_glass = false;
        if (is_assigned("ar_glass")) use_ar_glass = as_boolean("ar_glass");

        // check system size
        if (dcrate < 0.1) dcrate = 0.1;

        // bounds of (0,09999, 0.99001) are consistent with online PVWatts http://rredc.nrel.gov/solar/codes_algs/PVWATTS/version1/US/code/pvwattsv1.cgi
        //    if ( derate < 0.09999 || derate > 0.99001 ) // Use if default ac to dc derate factor out of range
        if (derate < 0.0 || derate > 1.0) // Use if default ac to dc derate factor out of range
            derate = 0.77;

        double pcrate = dcrate * 1000.0;      // rated output of inverter in a.c. watts; 6/29/2005
        double refpwr = dcrate * 1000.0;      // nameplate in watts; 6/29/2005

        if (track_mode < 0 || track_mode > 3)
            track_mode = 0;
        if (tilt < 0 || tilt > 90)
            tilt = hdr.lat;
        if (azimuth < 0 || azimuth > 360)
            azimuth = 180.0;


        // read all the shading input data and calculate the hourly factors for use subsequently
        shading_factor_calculator shad;
        if (!shad.setup(this, ""))
            throw exec_error("pvwattsv1", shad.get_error());

        sssky_diffuse_table skydiff_table;
        skydiff_table.init(tilt, gcr);

        adjustment_factors haf(this, "adjust");
        if (!haf.setup())
            throw exec_error("pvwattsv1", "failed to setup adjustment factors: " + haf.error());


        pvwatts_celltemp tccalc(inoct, height, 1.0);

        double fixed_albedo = 0.2;
        bool has_albedo = is_assigned("albedo");
        if (has_albedo)
            fixed_albedo = as_double("albedo");


        weather_record wf;

        int i = 0;
        while (i < 8760)
        {
            if (!wfile.read(&wf))
                throw exec_error("pvwattsv1", "could not read data line " + util::to_string(i + 1) + " of 8760 in weather file");

            irrad irr;
            irr.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute, wfile.step_sec() / 3600.0);
            irr.set_location(hdr.lat, hdr.lon, hdr.tz);
            irr.set_optional(hdr.elev, wf.pres, wf.tdry);

            double alb = 0.2;

            if (has_albedo && fixed_albedo >= 0 && fixed_albedo <= 1.0)
            {
                alb = fixed_albedo;
            }
            else if (wfile.type() == weatherfile::TMY2)
            {
                if (wf.snow > 0 && wf.snow < 150)
                    alb = 0.6;
            }
            else if (wfile.type() == weatherfile::TMY3)
            {
                if (wf.alb >= 0 && wf.alb < 1)
                    alb = wf.alb;
            }

            irr.set_sky_model(2, alb);
            irr.set_beam_diffuse(wf.dn, wf.df);
            irr.set_surface(track_mode, tilt, azimuth, rlim,
                shade_mode_1x == 1, // backtracking mode
                gcr, false, 0.0);

            double ibeam, iskydiff, ignddiff;
            double solazi = 0, solzen = 0, solalt = 0, aoi, stilt, sazi, rot, btd;
            int sunup;

            p_gh[i] = (ssc_number_t)wf.gh;
            p_dn[i] = (ssc_number_t)wf.dn;
            p_df[i] = (ssc_number_t)wf.df;
            p_tamb[i] = (ssc_number_t)wf.tdry;
            p_tdew[i] = (ssc_number_t)wf.tdew;
            p_wspd[i] = (ssc_number_t)wf.wspd;
            p_tcell[i] = (ssc_number_t)wf.tdry;

            int code = irr.calc();

            if (0 != code)
                sunup = 0; // if for some reason the irradiance processor fails, ignore this hour
            else
                irr.get_sun(&solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0);

            p_sunup[i] = (ssc_number_t)sunup;

            p_shad_beam[i] = 1.0f;
            if (shad.fbeam(i, wf.minute, solalt, solazi))
                p_shad_beam[i] = (ssc_number_t)shad.beam_shade_factor();

            if (sunup > 0)
            {
                irr.get_angles(&aoi, &stilt, &sazi, &rot, &btd);
                irr.get_poa(&ibeam, &iskydiff, &ignddiff, 0, 0, 0);

                if (sunup > 0 && track_mode == 1
                    && shade_mode_1x == 0) // selfshaded mode
                {
                    double shad1xf = shadeFraction1x(solazi, solzen, tilt, azimuth, gcr, rot);
                    p_shad_beam[i] *= (ssc_number_t)(1 - shad1xf);

                    if (fd > 0 && shade_mode_1x == 0 && iskydiff > 0)
                    {
                        double reduced_skydiff = iskydiff;
                        double Fskydiff = 1.0;
                        double reduced_gnddiff = ignddiff;
                        double Fgnddiff = 1.0;

                        // worst-case mask angle using calculated surface tilt
                        //double phi0 = 180/3.1415926*atan2( sind( stilt ), 1/gcr - cosd( stilt ) );

                        // calculate sky and gnd diffuse derate factors
                        // based on view factor reductions from self-shading
                        diffuse_reduce(solzen, stilt,
                            wf.dn, wf.df, iskydiff, ignddiff,
                            gcr, alb, 1000, skydiff_table,

                            // outputs (pass by reference)
                            reduced_skydiff, Fskydiff,
                            reduced_gnddiff, Fgnddiff);

                        if (Fskydiff >= 0 && Fskydiff <= 1) iskydiff *= Fskydiff;
                        else log(util::format("sky diffuse reduction factor invalid at hour %d: fskydiff=%lg, stilt=%lg", i, Fskydiff, stilt), SSC_NOTICE, (float)i);

                        if (Fgnddiff >= 0 && Fgnddiff <= 1) ignddiff *= Fgnddiff;
                        else log(util::format("gnd diffuse reduction factor invalid at hour %d: fgnddiff=%lg, stilt=%lg", i, Fgnddiff, stilt), SSC_NOTICE, (float)i);
                    }

                }

                // apply hourly shading factors to beam (if none enabled, factors are 1.0)
                ibeam *= p_shad_beam[i];

                // apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
                iskydiff *= shad.fdiff();

                double poa = ibeam + fd * (iskydiff + ignddiff);

                if (p_user_poa != 0)
                    poa = p_user_poa[i];

                if (poa_cutin > 0 && poa < poa_cutin)
                    poa = 0;

                double wspd_corr = wf.wspd < 0 ? 0 : wf.wspd;

                if (wind_stow > 0 && wf.wspd >= wind_stow)
                    poa = 0;

                // check that the double to boolean conversion fo use_ar_glass works!
                double tpoa = transpoa(poa, wf.dn, aoi * 3.14159265358979 / 180, use_ar_glass);
                double pvt = wf.tdry;

                if (use_faiman_model)
                    pvt = wf.tdry + poa * concen / (U0 + U1 * wspd_corr);
                else
                    pvt = tccalc(poa * concen, wspd_corr, wf.tdry, fhconv);

                double dc = dcpowr(reftem, refpwr, pwrdgr, tmloss, tpoa, pvt, i_ref);
                double ac = dctoac(pcrate, efffp, dc);

                p_poa[i] = (ssc_number_t)poa;
                p_tpoa[i] = (ssc_number_t)tpoa;
                p_tcell[i] = (ssc_number_t)pvt;
                p_dc[i] = (ssc_number_t)dc;
                p_ac[i] = (ssc_number_t)ac;
                p_hourly_energy[i] = (ssc_number_t)(ac * haf(i) * 0.001f);
                //				p_gen[i] = (ssc_number_t)(ac*haf(i) * 0.001f);
            }

            i++;
        }

        ssc_number_t* poam = accumulate_monthly("poa", "poa_monthly", 0.001);
        accumulate_monthly("dc", "dc_monthly", 0.001);
        accumulate_monthly("ac", "ac_monthly", 0.001);
        accumulate_monthly("gen", "monthly_energy");

        ssc_number_t* solrad = allocate("solrad_monthly", 12);
        ssc_number_t solrad_ann = 0;
        for (int m = 0; m < 12; m++)
        {
            solrad[m] = poam[m] / util::nday[m];
            solrad_ann += solrad[m];
        }
        assign("solrad_annual", var_data(solrad_ann / 12));


        accumulate_annual("ac", "ac_annual", 0.001);
        accumulate_annual("gen", "annual_energy");

        assign("location", var_data(hdr.location));
        assign("city", var_data(hdr.city));
        assign("state", var_data(hdr.state));
        assign("lat", var_data((ssc_number_t)hdr.lat));
        assign("lon", var_data((ssc_number_t)hdr.lon));
        assign("tz", var_data((ssc_number_t)hdr.tz));
        assign("elev", var_data((ssc_number_t)hdr.elev));
    }
};

DEFINE_MODULE_ENTRY(pvwattsv1, "PVWatts V.1 - integrated hourly weather reader and PV system simulator.", 2)
