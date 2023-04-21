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
#include "lib_util.h"
#include "lib_irradproc.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_irradproc[] = {
    /*   VARTYPE           DATATYPE         NAME                           LABEL                              UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

        { SSC_INPUT,        SSC_NUMBER,      "irrad_mode",                 "Irradiance input mode",           "0/1/2",   "Beam+Diff,Global+Beam, Global+Diff",  "Irradiance Processor",      "?=0",                     "INTEGER,MIN=0,MAX=2", ""},

        { SSC_INPUT,        SSC_ARRAY,       "beam",                       "Beam normal irradiance",          "W/m2",   "",                      "Irradiance Processor",      "irrad_mode~2",                        "",                      "" },
        { SSC_INPUT,        SSC_ARRAY,       "diffuse",                    "Diffuse horizontal irradiance",   "W/m2",   "",                      "Irradiance Processor",      "irrad_mode~1",             "LENGTH_EQUAL=beam",     "" },
        { SSC_INPUT,        SSC_ARRAY,       "global",                     "Global horizontal irradiance",    "W/m2",   "",                      "Irradiance Processor",      "irrad_mode~0",              "LENGTH_EQUAL=beam",     "" },

        { SSC_INPUT,        SSC_ARRAY,       "albedo",                     "Ground reflectance (time depend.)","frac",  "0..1",                   "Irradiance Processor",      "?",                        "LENGTH_EQUAL=beam",     "" },
        { SSC_INPUT,        SSC_NUMBER,      "albedo_const",               "Ground reflectance (single value)","frac",  "0..1",                   "Irradiance Processor",      "?=0.2",                    "",                      "" },

        { SSC_INPUT,        SSC_ARRAY,       "year",                       "Year",                             "yr",     "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",               "" },
        { SSC_INPUT,        SSC_ARRAY,       "month",                      "Month",                            "mn",     "1-12",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_INPUT,        SSC_ARRAY,       "day",                        "Day",                              "dy",     "1-days in month",                 "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_INPUT,        SSC_ARRAY,       "hour",                       "Hour",                             "hr",     "0-23",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_INPUT,        SSC_ARRAY,       "minute",                     "Minute",                           "min",    "0-59",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },


        { SSC_INPUT,        SSC_NUMBER,      "lat",                        "Latitude",                         "deg",    "",                      "Irradiance Processor",      "*",                        "",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "lon",                        "Longitude",                        "deg",    "",                      "Irradiance Processor",      "*",                        "",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "tz",                         "Time zone",                        "hr",     "",                      "Irradiance Processor",      "*",                        "",                      "" },

        { SSC_INPUT,        SSC_NUMBER,      "sky_model",                  "Tilted surface irradiance model", "0/1/2", "Isotropic,HDKR,Perez",  "Irradiance Processor",      "?=2",                     "INTEGER,MIN=0,MAX=2", ""},

        { SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "Irradiance Processor",      "*",                       "MIN=0,MAX=2,INTEGER",                      "" },
        { SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "E=90,S=180,W=270",      "Irradiance Processor",      "*",                       "MIN=0,MAX=360",                            "" },
        { SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "H=0,V=90",              "Irradiance Processor",      "?",                       "MIN=0,MAX=90",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "rotlim",                     "Rotational limit on tracker",    "deg",    "",                      "Irradiance Processor",      "?=45",                    "MIN=0,MAX=90",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "backtrack",                  "Enable backtracking",            "0/1",    "",                      "Irradiance Processor",      "?=0",                    "BOOLEAN",                                   "" },
        { SSC_INPUT,        SSC_NUMBER,      "gcr",                        "Ground coverage ratio",          "0..1",   "",                      "Irradiance Processor",      "backtrack=1",              "MIN=0,MAX=1",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "elevation",                  "Elevation",                      "m",      "",                      "Irradiance Processor",        "?",                                 "",                             "" },
        { SSC_INPUT,        SSC_NUMBER,      "tamb",                       "Ambient Temperature (dry bulb temperature)","°C",     "",           "Irradiance Processor",        "?",                                  "",                            "" },
        { SSC_INPUT,        SSC_NUMBER,      "pressure",                   "Pressure",                       "mbars",  "",                      "Irradiance Processor",        "?",                                  "",                            "" },


        { SSC_OUTPUT,       SSC_ARRAY,       "poa_beam",                   "Incident Beam Irradiance",       "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff",                "Incident Sky Diffuse",           "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "poa_gnddiff",                "Incident Ground Reflected Diffuse", "W/m2", "",                     "Irradiance Processor",      "*",                       "",                  "" },

        { SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_iso",            "Incident Diffuse Isotropic Component", "W/m2", "",                  "Irradiance Processor",      "*",                       "",                  "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_cir",            "Incident Diffuse Circumsolar Component", "W/m2", "",                "Irradiance Processor",      "*",                       "",                  "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_hor",            "Incident Diffuse Horizon Brightening Component", "W/m2", "",        "Irradiance Processor",      "*",                       "",                  "" },

        { SSC_OUTPUT,       SSC_ARRAY,       "incidence",                  "Incidence angle to surface",     "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "surf_tilt",                  "Surface tilt angle",             "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "surf_azm",                   "Surface azimuth angle",          "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "axis_rotation",              "Tracking axis rotation angle",   "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "bt_diff",                    "Backtracking difference from ideal rotation",   "deg",    "",       "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },

        { SSC_OUTPUT,       SSC_ARRAY,       "sun_azm",                    "Solar azimuth",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "sun_zen",                    "Solar zenith",                   "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "sun_elv",                    "Sun elevation",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
        { SSC_OUTPUT,       SSC_ARRAY,       "sun_dec",                    "Sun declination",                "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },


    var_info_invalid };

class cm_irradproc : public compute_module
{
public:
    cm_irradproc()
    {
        add_var_info(_cm_vtab_irradproc);
    }

    void exec()
    {
        size_t count;
        ssc_number_t* beam = 0, * glob = 0, * diff = 0;
        int irrad_mode = as_integer("irrad_mode");
        if (irrad_mode == 0) //beam and diffuse
        {
            beam = as_array("beam", &count);
            if (count < 2) throw general_error("need at least 2 data points in irradproc");
            diff = as_array("diffuse", &count);
        }
        else if (irrad_mode == 1) //global and beam
        {
            beam = as_array("beam", &count);
            if (count < 2) throw general_error("need at least 2 data points in irradproc");
            glob = as_array("global", &count);
        }
        else //global and diffuse
        {
            diff = as_array("diffuse", &count);
            if (count < 2) throw general_error("need at least 2 data points in irradproc");
            glob = as_array("global", &count);
        }

        ssc_number_t* year = as_array("year", &count);
        ssc_number_t* month = as_array("month", &count);
        ssc_number_t* day = as_array("day", &count);
        ssc_number_t* hour = as_array("hour", &count);
        ssc_number_t* minute = as_array("minute", &count);

        int sky_model = as_integer("sky_model");

        double lat = as_double("lat");
        double lon = as_double("lon");
        double tz = as_double("tz");
        double elev, tamb, pres;

        if (!is_assigned("elevation")) {
            elev = 0; //assume 0 meter elevation if none is provided
        }
        else {
            elev = as_double("elevation");
            if (elev < 0 || elev > 5100) {
                throw exec_error("irradproc", "The elevation input is outside of the expected range. Please make sure that the units are in meters");
            }
        }
        if (!is_assigned("tamb")) {
            tamb = 15; //assume 15°C average annual temperature if none is provided
        }
        else {
            tamb = as_double("tamb");
            if (tamb > 128 || tamb < -50) {
                throw exec_error("irradproc", "The annual average temperature input is outside of the expected range. Please make sure that the units are in degrees Celsius");
            }
        }
        if (!is_assigned("pressure")) {
            pres = 1013.25; //assume 1013.24 millibars site pressure if none is provided
        }
        else {
            pres = as_double("pressure");
            if (pres > 2000 || pres < 500) {
                throw exec_error("irradproc", "The atmospheric pressure input is outside of the expected range. Please make sure that the units are in millibars");
            }
        }

        double tilt = lat;
        if (is_assigned("tilt"))
            tilt = as_double("tilt");

        double azimuth = as_double("azimuth");
        int track_mode = as_integer("track_mode");
        double rotlim = as_double("rotlim");
        bool en_backtrack = as_boolean("backtrack");
        double gcr = 0; //use a default value since it's needed to be passed into the set_surface function, but isn't used subsequently
        if (is_assigned("gcr")) gcr = as_double("gcr");

        double alb_const = as_double("albedo_const");
        ssc_number_t* albvec = 0;
        if (is_assigned("albedo")) albvec = as_array("albedo", &count);


        // allocate outputs
        ssc_number_t* p_inc = allocate("incidence", count);
        ssc_number_t* p_surftilt = allocate("surf_tilt", count);
        ssc_number_t* p_surfazm = allocate("surf_azm", count);
        ssc_number_t* p_rot = allocate("axis_rotation", count);
        ssc_number_t* p_btdiff = allocate("bt_diff", count);
        ssc_number_t* p_azm = allocate("sun_azm", count);
        ssc_number_t* p_zen = allocate("sun_zen", count);
        ssc_number_t* p_elv = allocate("sun_elv", count);
        ssc_number_t* p_dec = allocate("sun_dec", count);

        ssc_number_t* p_poa_beam = allocate("poa_beam", count);
        ssc_number_t* p_poa_skydiff = allocate("poa_skydiff", count);
        ssc_number_t* p_poa_gnddiff = allocate("poa_gnddiff", count);

        ssc_number_t* p_poa_skydiff_iso = allocate("poa_skydiff_iso", count);
        ssc_number_t* p_poa_skydiff_cir = allocate("poa_skydiff_cir", count);
        ssc_number_t* p_poa_skydiff_hor = allocate("poa_skydiff_hor", count);


        // "temporary" debugging output
        ssc_number_t* p_sunup = allocate("sunup", count);
        ssc_number_t* p_sunrise = allocate("sunrise", count);
        ssc_number_t* p_sunset = allocate("sunset", count);

        for (size_t i = 0; i < count; i++)
        {
            double t_cur = hour[i] + minute[i] / 60.0;
            double delt = 1.0;
            if (i == 0)
            {
                double t_next = hour[i + 1] + minute[i + 1] / 60.0;
                if (t_cur > t_next) t_next += 24;
                delt = t_next - t_cur;
            }
            else
            {
                double t_prev = hour[i - 1] + minute[i - 1] / 60.0;
                if (t_cur < t_prev) t_cur += 24;
                delt = t_cur - t_prev;
            }

            // double precsion issue (15 digits IEEE 754) encountered by Anthony Lopez 4/29/13 for 
            // minutes other than 15,30,45 and 60
            if (fabs(delt - 1.0) < 1e-14) delt = 1.0;

            double alb = alb_const;
            // if we have array of albedo values, use it
            if (albvec != 0 && albvec[i] >= 0 && albvec[i] <= (ssc_number_t)1.0)
                alb = albvec[i];

            irrad x;

            x.set_time((int)year[i], (int)month[i], (int)day[i], (int)hour[i], minute[i], IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET);
            x.set_location(lat, lon, tz);
            x.set_optional(elev, pres, tamb);
            x.set_sky_model(sky_model, alb);
            if (irrad_mode == 1) x.set_global_beam(glob[i], beam[i]);
            else if (irrad_mode == 2) x.set_global_diffuse(glob[i], diff[i]);
            else x.set_beam_diffuse(beam[i], diff[i]);
            x.set_surface(track_mode, tilt, azimuth, rotlim, en_backtrack, gcr, false, 0.0); //last two inputs are to force to a stow angle, which doesn't make sense for irradproc as a standalone cmod

            int code = x.calc();
            if (code < 0)
                throw general_error(util::format("irradiance processor issued error code %d", code));

            double solazi, solzen, solelv, soldec, sunrise, sunset;
            int sunup;

            x.get_sun(&solazi,
                &solzen,
                &solelv,
                &soldec,
                &sunrise,
                &sunset,
                &sunup,
                0,
                0,
                0);

            p_azm[i] = (ssc_number_t)solazi;
            p_zen[i] = (ssc_number_t)solzen;
            p_elv[i] = (ssc_number_t)solelv;
            p_dec[i] = (ssc_number_t)soldec;
            p_sunrise[i] = (ssc_number_t)sunrise;
            p_sunset[i] = (ssc_number_t)sunset;
            p_sunup[i] = (ssc_number_t)sunup;


            double aoi, stilt, sazi, rot, btd;
            x.get_angles(&aoi, &stilt, &sazi, &rot, &btd);

            // assign outputs
            p_inc[i] = (ssc_number_t)aoi;
            p_surftilt[i] = (ssc_number_t)stilt;
            p_surfazm[i] = (ssc_number_t)sazi;
            p_rot[i] = (ssc_number_t)rot;
            p_btdiff[i] = (ssc_number_t)btd;

            double beam, skydiff, gnddiff, iso, cir, hor;
            x.get_poa(&beam, &skydiff, &gnddiff, &iso, &cir, &hor);

            p_poa_beam[i] = (ssc_number_t)beam;
            p_poa_skydiff[i] = (ssc_number_t)skydiff;
            p_poa_gnddiff[i] = (ssc_number_t)gnddiff;
            p_poa_skydiff_iso[i] = (ssc_number_t)iso;
            p_poa_skydiff_cir[i] = (ssc_number_t)cir;
            p_poa_skydiff_hor[i] = (ssc_number_t)hor;

        }
    }
};

DEFINE_MODULE_ENTRY(irradproc, "Irradiance Processor", 1)
