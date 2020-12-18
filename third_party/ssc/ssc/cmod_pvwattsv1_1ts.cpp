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

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_pvwatts.h"

#ifndef DTOR
#define DTOR 0.0174532925
#endif

static var_info _cm_vtab_pvwattsv1_1ts[] = {
    /*   VARTYPE           DATATYPE         NAME                         LABEL                                               UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        { SSC_INPUT,        SSC_NUMBER,      "year",                     "Year",                                        "yr",     "",                        "PVWatts",      "*",                       "",               "" },
        { SSC_INPUT,        SSC_NUMBER,      "month",                    "Month",                                       "mn",     "1-12",                    "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "day",                      "Day",                                         "dy",     "1-days in month",         "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "hour",                     "Hour",                                        "hr",     "0-23",                    "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "minute",                   "Minute",                                      "min",    "0-59",                    "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "lat",                      "Latitude",                                    "deg",    "",                        "PVWatts",      "*",                        "",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "lon",                      "Longitude",                                   "deg",    "",                        "PVWatts",      "*",                        "",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "tz",                       "Time zone",                                   "hr",     "",                        "PVWatts",      "*",                        "",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "beam",                     "Beam normal irradiance",                      "W/m2",   "",                        "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "diffuse",                  "Diffuse irradiance",                          "W/m2",   "",                        "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "tamb",                     "Ambient temperature (dry bulb temperature)",  "C",      "",                        "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "wspd",                     "Wind speed",                                  "m/s",    "",                        "PVWatts",      "*",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "snow",                     "Snow cover",                                  "cm",     "",                        "PVWatts",      "?=0",                     "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "elevation",                "Elevation",                                   "m",      "",                        "PVWatts",      "?",                       "",                          "" },
        { SSC_INPUT,        SSC_NUMBER,      "pressure",                 "Pressure",                                    "millibars","",                      "PVWatts",      "?",                       "",                          "" },

        { SSC_INPUT,        SSC_NUMBER,      "time_step",                "Time step of input data",                     "hr",    "",                         "PVWatts",      "?=1",                     "POSITIVE",                  "" },

        { SSC_INPUT,        SSC_NUMBER,      "system_size",              "Nameplate capacity",                          "kW",     "",                        "PVWatts",      "*",                       "",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "derate",                   "System derate value",                         "frac",   "",                        "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "track_mode",               "Tracking mode",                               "0/1/2/3","Fixed,1Axis,2Axis,AziAxis","PVWatts",      "*",                       "MIN=0,MAX=3,INTEGER",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "azimuth",                  "Azimuth angle",                               "deg",    "E=90,S=180,W=270",        "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
        { SSC_INPUT,        SSC_NUMBER,      "tilt",                     "Tilt angle",                                  "deg",    "H=0,V=90",                "PVWatts",      "naof:tilt_eq_lat",        "MIN=0,MAX=90",                             "" },

        /* advanced parameters: generally recommended to use defaults */
        { SSC_INPUT,        SSC_NUMBER,      "rotlim",                   "Tracker rotation limit (+/- 1 axis)",         "deg",    "",                        "PVWatts",      "?=45.0",                  "MIN=1,MAX=90",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "t_noct",                   "Nominal operating cell temperature",          "C",      "",                        "PVWatts",      "?=45.0",                  "POSITIVE",                                 "" },
        { SSC_INPUT,        SSC_NUMBER,      "t_ref",                    "Reference cell temperature",                  "C",      "",                        "PVWatts",      "?=25.0",                  "POSITIVE",                                 "" },
        { SSC_INPUT,        SSC_NUMBER,      "gamma",                    "Max power temperature coefficient",           "%/C",    "",                        "PVWatts",      "?=-0.5",                  "",                                         "" },
        { SSC_INPUT,        SSC_NUMBER,      "inv_eff",                  "Inverter efficiency at rated power",          "frac",   "",                        "PVWatts",      "?=0.92",                  "MIN=0,MAX=1",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "fd",                       "Diffuse fraction",                            "0..1",   "",                        "PVWatts",      "?=1.0",                   "MIN=0,MAX=1",                              "" },
        { SSC_INPUT,        SSC_NUMBER,      "i_ref",                    "Rating condition irradiance",                 "W/m2",   "",                        "PVWatts",      "?=1000",                  "POSITIVE",                                 "" },
        { SSC_INPUT,        SSC_NUMBER,      "poa_cutin",                "Min reqd irradiance for operation",           "W/m2",   "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },
        { SSC_INPUT,        SSC_NUMBER,      "w_stow",                   "Wind stow speed",                             "m/s",    "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },


        /* input/output variable: tcell & poa from previous time must be given */
        { SSC_INOUT,        SSC_NUMBER,      "tcell",                    "Module temperature",                          "C",      "",                        "PVWatts",      "*",                       "",                          "" },
        { SSC_INOUT,        SSC_NUMBER,      "poa",                      "Plane of array irradiance",                   "W/m2",   "",                        "PVWatts",      "*",                       "",                          "" },

        /* outputs */
        { SSC_OUTPUT,       SSC_NUMBER,      "dc",                      "DC array output",                             "Wdc",    "",                        "PVWatts",      "*",                       "",                          "" },
        { SSC_OUTPUT,       SSC_NUMBER,      "ac",                      "AC system output",                            "Wac",    "",                        "PVWatts",      "*",                       "",                          "" },

    var_info_invalid };

class cm_pvwattsv1_1ts : public compute_module
{
public:

    cm_pvwattsv1_1ts()
    {
        add_var_info(_cm_vtab_pvwattsv1_1ts);
    }

    void exec()
    {

        int year = as_integer("year");
        int month = as_integer("month");
        int day = as_integer("day");
        int hour = as_integer("hour");
        double minute = as_double("minute");
        double lat = as_double("lat");
        double lon = as_double("lon");
        double tz = as_double("tz");
        double beam = as_double("beam");
        double diff = as_double("diffuse");
        double tamb = as_double("tamb");
        double wspd = as_double("wspd");

        //double time_step = as_double("time_step");
        double snow = as_double("snow");

        double dcrate = as_double("system_size");
        double derate = as_double("derate");
        int track_mode = as_integer("track_mode"); // 0, 1, 2, 3
        double azimuth = as_double("azimuth");
        double tilt = fabs(as_double("tilt"));
        double elev, tdry, pres;
        if (!is_assigned("elevation")) {
            elev = 0; //assume 0 meter elevation if none is provided
        }
        else {
            elev = as_double("elevation");
            if (elev < 0 || elev > 5100) {
                throw exec_error("pvwattsv1_1ts", "The elevation input is outside of the expected range. Please make sure that the units are in meters");
            }
        }
        if (!is_assigned("pressure")) {
            pres = 1013.25; //assume 1013.24 millibars site pressure if none is provided
        }
        else {
            pres = as_double("pressure");
            if (pres > 2000 || pres < 500) {
                throw exec_error("pvwattsv1_1ts", "The atmospheric pressure input is outside of the expected range. Please make sure that the units are in millibars");
            }
        }


        /* PV RELATED SPECIFICATIONS */

        double inoct = as_double("t_noct") + 273.15; // PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
        double reftem = as_double("t_ref"); // PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
        double pwrdgr = as_double("gamma") / 100.0; // PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
        double efffp = as_double("inv_eff"); // PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */

        double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
        double tmloss = 1.0 - derate / efffp;  /* All losses except inverter,decimal */
        double rlim = as_double("rotlim");             /* +/- rotation in degrees permitted by physical constraint of tracker */
        double fd = as_double("fd"); // diffuse fraction
        double i_ref = as_double("i_ref"); // reference irradiance for rating condition
        double poa_cutin = as_double("poa_cutin"); // minimum POA irradiance level required for any operation
        double wind_stow = as_double("w_stow"); // maximum wind speed before stowing.  stowing causes all output to be lost

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
            tilt = lat;
        if (azimuth < 0 || azimuth > 360)
            azimuth = 180.0;


        double last_tcell = as_double("tcell");
        double last_poa = as_double("poa");

        pvwatts_celltemp tccalc(inoct, height, 1.0);
        tccalc.set_last_values(last_tcell, last_poa);

        irrad irr;
        irr.set_time(year, month, day, hour, minute, IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET);
        irr.set_location(lat, lon, tz);
        irr.set_optional(elev, pres, tamb);

        double alb = 0.2;
        if (snow > 0 && snow < 150)
            alb = 0.6;

        irr.set_sky_model(2, alb);
        irr.set_beam_diffuse(beam, diff);
        irr.set_surface(track_mode, tilt, azimuth, rlim, true, -1, false, 0.0);

        double ibeam, iskydiff, ignddiff;
        double solazi, solzen, solalt, aoi, stilt, sazi, rot, btd;
        int sunup;

        int code = irr.calc();
        if (code != 0)
            throw exec_error("pvwattsv1_1ts", "failed to calculate POA irradiance with given input parameters");

        double out_poa = 0;
        double out_tcell = tamb;
        double out_dc = 0;
        double out_ac = 0;

        irr.get_sun(&solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0);
        if (sunup > 0)
        {
            irr.get_angles(&aoi, &stilt, &sazi, &rot, &btd);
            irr.get_poa(&ibeam, &iskydiff, &ignddiff, 0, 0, 0);

            double poa = ibeam + fd * (iskydiff + ignddiff);

            if (poa_cutin > 0 && poa < poa_cutin)
                poa = 0;

            double wspd_corr = wspd < 0 ? 0 : wspd;

            if (wind_stow > 0 && wspd >= wind_stow)
                poa = 0;

            double tpoa = transpoa(poa, beam, aoi * 3.14159265358979 / 180, false);
            double pvt = tccalc(poa, wspd_corr, tamb);
            double dc = dcpowr(reftem, refpwr, pwrdgr, tmloss, tpoa, pvt, i_ref);
            double ac = dctoac(pcrate, efffp, dc);

            out_poa = poa;
            out_tcell = pvt;
            out_dc = dc;
            out_ac = ac;
        }


        assign("poa", var_data((ssc_number_t)out_poa));
        assign("tcell", var_data((ssc_number_t)out_tcell));
        assign("dc", var_data((ssc_number_t)out_dc));
        assign("ac", var_data((ssc_number_t)out_ac));
    }
};

DEFINE_MODULE_ENTRY(pvwattsv1_1ts, "pvwattsv1_1ts- single timestep calculation of PV system performance.", 1)
