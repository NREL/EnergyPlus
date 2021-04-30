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


#include <limits>
#include <cmath>

#include "core.h"
#include "lib_weatherfile.h"
#include "lib_irradproc.h"

#ifndef M_PI
#define M_PI 3.14159265358979323
#endif

#ifndef DTOR
#define DTOR 0.0174532925
#endif

static var_info _cm_vtab_belpe[] =
{
    /*   VARTYPE			DATATYPE        NAME                LABEL								UNITS		META			GROUP                     REQUIRED_IF	CONSTRAINTS		UI_HINTS*/
        { SSC_INPUT,		SSC_NUMBER,		"en_belpe",			"Enable building load calculator",	"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },

        // load is modified in BELPE. It is passed straight through 
        // if BELPE is disabled, and subhourly loads can be pass through.
        // but BELPE always runs hourly, at least for now....
        { SSC_INOUT,		SSC_ARRAY,		"load",			    "Electricity load (year 1)",        "kW",	    "",				"Load Profile Estimator", "en_belpe=0",	 "",	 "" },

        { SSC_INPUT,        SSC_STRING,		"solar_resource_file","Weather Data file",				"n/a",		"",				"Load Profile Estimator", "en_belpe=1",			"LOCAL_FILE",	"" },
        //	{ SSC_INPUT,        SSC_NUMBER,		"tstep",            "time step",						"hrs",      "",				"Load Profile Estimator", "en_belpe=1",			"",				"Time Step" },
            { SSC_INPUT,        SSC_NUMBER,		"floor_area",		"Building floor area",				"m2",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"Floor area" },
            { SSC_INPUT,        SSC_NUMBER,		"Stories",			"Number of stories",				"#",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"Stories" },

            { SSC_INPUT,		SSC_NUMBER,		"YrBuilt",			"Year built",						"yr",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"" },
            { SSC_INPUT,		SSC_NUMBER,		"Retrofits",		"Energy retrofitted",				"0/1",		"0=No, 1=Yes",	"Load Profile Estimator", "en_belpe=1",			"",				"" },//energy retrofits
            { SSC_INPUT,		SSC_NUMBER,		"Occupants",		"Occupants",						"#",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"" },
            { SSC_INPUT,		SSC_ARRAY,		"Occ_Schedule",		"Hourly occupant schedule",			"frac/hr",	"",				"Load Profile Estimator", "en_belpe=1",			"LENGTH=24",	"" },
            { SSC_INPUT,		SSC_NUMBER,		"THeat",			"Heating setpoint",					"degF",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"" },
            { SSC_INPUT,        SSC_NUMBER,		"TCool",			"Cooling setpoint",					"degF",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"" },
            { SSC_INPUT,		SSC_NUMBER,		"THeatSB",			"Heating setpoint setback",			"degf",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"" },
            { SSC_INPUT,		SSC_NUMBER,		"TCoolSB",			"Cooling setpoint setback",			"degF",		"",				"Load Profile Estimator", "en_belpe=1",			"",				"" },
            { SSC_INPUT,		SSC_ARRAY,		"T_Sched",			"Temperature schedule",				"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"LENGTH=24",	"" },

            { SSC_INPUT,		SSC_NUMBER,		"en_heat",			"Enable electric heat",				"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },
            { SSC_INPUT,		SSC_NUMBER,		"en_cool",			"Enable electric cool",				"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },
            { SSC_INPUT,		SSC_NUMBER,		"en_fridge",		"Enable electric fridge",			"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },
            { SSC_INPUT,		SSC_NUMBER,		"en_range",			"Enable electric range",			"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },
            { SSC_INPUT,		SSC_NUMBER,		"en_dish",			"Enable electric dishwasher",		"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },
            { SSC_INPUT,		SSC_NUMBER,		"en_wash",			"Enable electric washer",			"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },
            { SSC_INPUT,		SSC_NUMBER,		"en_dry",			"Enable electric dryer",			"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },
            { SSC_INPUT,		SSC_NUMBER,		"en_mels",			"Enable misc electric loads",		"0/1",		"",				"Load Profile Estimator", "en_belpe=1",			"BOOLEAN",		"" },

            { SSC_INPUT,		SSC_ARRAY,		"Monthly_util",		"Monthly consumption from utility bill",	"kWh",	"",			"Load Profile Estimator", "en_belpe=1",			"LENGTH=12",	"" },


            //OUTPUTS
        //	{ SSC_OUTPUT,       SSC_ARRAY,		"HVAC_load",		"Electric Load due to HVAC",		"Wh",       "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
        //	{ SSC_OUTPUT,       SSC_ARRAY,		"non_HVAC_load",	"Electric Load due to Non-HVAC",	"Wh",       "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },

        /*
            //DEBUGGING OUTPUTS
            { SSC_OUTPUT,       SSC_ARRAY,		"Rad_N",		"Radiation on North wall",		"W/m2",       "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"Rad_E",		"Radiation on East wall",		"W/m2",       "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"Rad_S",		"Radiation on South wall",		"W/m2",       "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"Rad_W",		"Radiation on West wall",		"W/m2",       "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"QN",			"QN",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"Cair",			"Cair",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"TAnew",		"TAnew",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"TAnewTop",		"TAnewTop",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"TAnewBot",		"TAnewBot",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"UAinf",		"UAinf",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"bar",		"bar",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"bardub",		"bardub",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=8760",	"" },
            { SSC_OUTPUT,       SSC_NUMBER,		"Renvdebug",		"Renvdebug",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"xhvac",		"xhvac",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=12",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"newscale",		"newscale",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=12",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"monthlytotelecactual",		"monthlytotelecactual",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=12",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"monthlytotelec_lpgen",		"monthlytotelec_lpgen",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=12",	"" },
            { SSC_OUTPUT,       SSC_ARRAY,		"monthlyhvac_lpgen",		"monthlyhvac_lpgen",							"",			  "",		"Load Profile Estimator", "en_belpe=1",			"LENGTH=12",	"" },
        */


        var_info_invalid };

// Computes a building load given various building parameters as inputs and a weather file. Algorithm uses global (GHI), direct (DNI), and diffuse (DHI) irradiance components as well as temp and wind speed.
class cm_belpe : public compute_module
{
private:
public:
    cm_belpe()
    {
        add_var_info(_cm_vtab_belpe);
    }

    //SUPPORTING FUNCTIONS********************************************************************************************************************************************************************

    //sums a vector a of length n
    double sum(double* a, int n)
    {
        double acc = 0;
        for (int i = 0; i < n; i++) acc += a[i];
        return acc;
    }

    //sums the elements in vector a from position m to position n
    double sumsub(double* a, int m, int n)
    {
        double acc = 0;
        for (int i = m; i <= n; i++) acc += a[i];
        return acc;
    }

    //sums an 8760 array ("hourly") into an array of 12 monthly sums ("monthly")
    void monthly_sums(ssc_number_t* hourly, ssc_number_t* monthly)
    {
        int c = 0; //8760 counter
        for (int i = 0; i < 12; i++) //each month
        {
            monthly[i] = 0;
            for (size_t d = 0; d < util::nday[i]; d++) // for each day in each month
                for (int h = 0; h < 24; h++) // for each hour in each day
                    monthly[i] += hourly[c++];
        }
    }

    //averages an 8760 array ("hourly") into an array of 12 monthly averages ("monthly")
    void monthly_averages(ssc_number_t* hourly, ssc_number_t* monthly)
    {
        //		int c = 0; //8760 counter
        monthly_sums(hourly, monthly);
        for (int i = 0; i < 12; i++)
            monthly[i] /= (util::nday[i] * 24); //divide the monthly sum by the number of hours in the month for an hourly average		
    }


    //MAIN FUNCTION*******************************************************************************************************************************************************************************
    void exec() override
    {
        //This compute module is going to automatically be run in series between pvsamv1 and utilityrate3 for residential and commercial systems.
        //However, the building load profile option will not be selected by default.
        //The enable flag allows us to exit the program without doing any calculations if the user hasn't selected to use it.
        ssc_number_t en_belpe = as_boolean("en_belpe");
        if (!en_belpe)
        {
            //these inputs are required if en_belpe = 0, so no additional checks are necessary here
            if (!is_assigned("load"))
                throw general_error("variable 'load' is required but not assigned.");

            return; // do not modify input "load"
        }

        //if BELPE is enabled, e_load is overwritten (don't take anything in from the UI- just reallocate)
        ssc_number_t* load = allocate("load", 8760);

        //8760 arrays of month, day, and hour neeeded for lots of calcs, initialize those here
        int month[8760], day[8760], hour[8760];
        int index = 0;
        for (int m = 0; m < 12; m++)
        {
            for (int d = 0; d < (int)util::nday[m]; d++)
            {
                for (int h = 0; h <= 23; h++)
                {
                    month[index] = m;
                    day[index] = d;
                    hour[index] = h;
                    index++;
                }
            }
        }

        // read weather file inputs 		
        const char* file = as_string("solar_resource_file");
        weatherfile wfile(file);
        if (!wfile.ok()) throw exec_error("belpe", wfile.message());
        if (wfile.has_message()) log(wfile.message(), SSC_WARNING);

        //allocate input arrays
        ssc_number_t* T_ambF = allocate("T_ambF", 8760);
        ssc_number_t* VwindMPH = allocate("VwindMPH", 8760);
        ssc_number_t* GHI = allocate("GHI", 8760);
        std::vector<double> RadWallN(8760), RadWallS(8760), RadWallE(8760), RadWallW(8760);

        // allocate output arrays
        ssc_number_t* hvac_load = allocate("HVAC_load", 8760);
        ssc_number_t* non_hvac_load = allocate("non_HVAC_load", 8760);
        ssc_number_t* radn = allocate("Rad_N", 8760);
        ssc_number_t* rade = allocate("Rad_E", 8760);
        ssc_number_t* rads = allocate("Rad_S", 8760);
        ssc_number_t* radw = allocate("Rad_W", 8760);
        /* debugging outputs
        ssc_number_t *QNdebug = allocate("QN", 8760);
        ssc_number_t *Cairdebug = allocate("Cair", 8760);
        ssc_number_t *TAnewdebug = allocate("TAnew", 8760);
        ssc_number_t *TAnewTopdebug = allocate("TAnewTop", 8760);
        ssc_number_t *TAnewBotdebug = allocate("TAnewBot", 8760);
        ssc_number_t *UAinfdebug = allocate("UAinf", 8760);
        ssc_number_t *bardebug = allocate("bar", 8760);
        ssc_number_t *bardubdebug = allocate("bardub", 8760);
        ssc_number_t *xhvacdebug = allocate("xhvac", 12);
        ssc_number_t *newscaledebug = allocate("newscale", 12);
        ssc_number_t *monthlytotelecactualdebug = allocate("monthlytotelecactual", 12);
        ssc_number_t *monthlytotelec_lpgendebug = allocate("monthlytotelec_lpgen", 12);
        ssc_number_t *monthlyhvac_lpgendebug = allocate("monthlyhvac_lpgen", 12);
        */

        weather_header hdr;
        wfile.header(&hdr);

        for (size_t i = 0; i < 8760; i++)
        {
            weather_record wf;
            if (!wfile.read(&wf)) throw exec_error(" belpe", "error reading record in weather file");

            //belpe doesn't work without ghi
            double ghi = wf.gh;
            if (std::isnan(ghi))
                throw exec_error("belpe", "weather file must contain GHI data in order to use the building load calculator");

            //calculate irradiances on four walls of building, needed later
            irrad irr;
            irr.set_location(hdr.lat, hdr.lon, hdr.tz);
            irr.set_optional(hdr.elev, wf.pres, wf.tdry);
            irr.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute, (double)(wfile.step_sec() / 3600.0));
            irr.set_global_beam(wf.gh, wf.dn);	//CHANGE THIS WHEN OPTION TO USE GLOBAL AND DIFFUSE IS INTRODUCED
            irr.set_sky_model(1, 0.2); //using HDKR model and default albedo
            //variables to store irradiance info
            double beam, sky, gnd;
            //North wall
            irr.set_surface(0, 90, 0, 0, 0, 0, false, 0.0);
            irr.calc();
            irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
            RadWallN[i] = beam + sky + gnd;
            //East wall
            irr.set_surface(0, 90, 90, 0, 0, 0, false, 0.0);
            irr.calc();
            irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
            RadWallE[i] = beam + sky + gnd;
            //South wall
            irr.set_surface(0, 90, 180, 0, 0, 0, false, 0.0);
            irr.calc();
            irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
            RadWallS[i] = beam + sky + gnd;
            //West wall
            irr.set_surface(0, 90, 270, 0, 0, 0, false, 0.0);
            irr.calc();
            irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
            RadWallW[i] = beam + sky + gnd;

            //store in output arrays
            radn[i] = (ssc_number_t)RadWallN[i];
            rade[i] = (ssc_number_t)RadWallE[i];
            rads[i] = (ssc_number_t)RadWallS[i];
            radw[i] = (ssc_number_t)RadWallW[i];

            //read and store other weather variables needed in calculations
            T_ambF[i] = (ssc_number_t)(wf.tdry * 1.8 + 32);
            VwindMPH[i] = (ssc_number_t)(wf.wspd * 2.237);
            GHI[i] = (ssc_number_t)wf.gh;
        }

        // calculate average annual temperature
        double T_annual_avg = 0;
        for (size_t i = 0; i < 8760; i++)
            T_annual_avg += T_ambF[i];
        T_annual_avg /= 8760;
        double TGnd = (T_annual_avg - 32) / 1.8; //Deg C because used just for the avg exterior envelope T.

        //radiation pre-processing
        double alphaho_wall = 0.15 / 5.6783;  //From ASHRAE for light colors(dark is x2).Converted to SI units
        std::vector<double> T_solair_walls(8760), T_solair_roof(8760), T_solair(8760), T_solairF(8760);
        for (int i = 0; i < 8760; i++)
        {
            T_solair_walls[i] = 4 * ((T_ambF[i] - 32) / 1.8 + alphaho_wall * (RadWallN[i] + RadWallS[i] + RadWallE[i] + RadWallW[i]) / 4); //N, S, E, W averaged
            T_solair_roof[i] = (T_ambF[i] - 32) / 1.8 + alphaho_wall * GHI[i] - 7;
            T_solair[i] = (T_solair_walls[i] + T_solair_roof[i] + TGnd) / 6; //This is in degrees C. Should GND be separated ?
            T_solairF[i] = T_solair[i] * 1.8 + 32;
        }

        //Timestep is hourly for now.
        //double tstep = as_double("tstep");
        double dT = 1;

        // read building parameter inputs
        double A_Floor = as_double("floor_area"); //ft^2
        double Stories = as_double("Stories");
        double YrBuilt = as_double("YrBuilt");
        double Occupants = as_double("Occupants");
        bool EnergyRetrofits = as_boolean("Retrofits"); // 1 = yes, 0 = no. Governs building construction for older bldgs.

        size_t len_Occ_Schedule = 0;
        ssc_number_t* Occ_Schedule = as_array("Occ_Schedule", &len_Occ_Schedule);

        if (len_Occ_Schedule != 24)
            throw exec_error("belpe", "occupancy schedule needs to have 24 values");

        double THeat = as_double("THeat");
        double TCool = as_double("TCool");
        double THeatSB = as_double("THeatSB");
        double TCoolSB = as_double("TCoolSB");
        size_t len_T_Sched = 0;
        ssc_number_t* T_Sched = as_array("T_Sched", &len_T_Sched);

        if (len_T_Sched != 24) throw exec_error("belpe", "temperature schedule must have 24 values");

        size_t len_monthly_util = 0;
        ssc_number_t* monthly_util = as_array("Monthly_util", &len_monthly_util);
        if (len_monthly_util != 12) throw exec_error("belpe", "Monthly consumption from utility bill must have 12 values");

        ssc_number_t en_heat = as_number("en_heat"); // boolean, so will be 0 or 1
        ssc_number_t en_cool = as_number("en_cool"); // boolean, so will be 0 or 1
        ssc_number_t en_fridge = as_number("en_fridge"); // boolean, so will be 0 or 1
        ssc_number_t en_range = as_number("en_range"); // boolean, so will be 0 or 1
        ssc_number_t en_dish = as_number("en_dish"); // boolean, so will be 0 or 1
        ssc_number_t en_wash = as_number("en_wash"); // boolean, so will be 0 or 1
        ssc_number_t en_dry = as_number("en_dry"); // boolean, so will be 0 or 1
        ssc_number_t en_mels = as_number("en_mels"); // boolean, so will be 0 or 1

        //Possible other input options include color, construction, WWR, bldg L&W, wall
        //height per floor

        // If calibrating to util bills need user to be able to enter vacation.
        //These are bldg AM.defaults -- could also be default in the tool.
        const int N_vacation = 14;
        double VacationMonths[N_vacation] = { 5, 5, 5, 8, 8, 8, 8, 8, 8, 8, 12, 12, 12, 12 };
        double VacationDays[N_vacation] = { 26, 27, 28, 12, 13, 14, 15, 16, 17, 18, 22, 23, 24, 25 };

        //Default Values -- user does NOT change these!
        double H_ceiling = 8; //ft
        double WWR = 0.15;   //Recommended 15 - 18 % ....but analyze, maybe user needs this option
        double NL = 0; // normalized leakage

        //Get ACH using Normalized Leakage area as per Persily, 2006.  As well as
        //LBL leakage model.
        if (A_Floor <= 1600)
        {
            if (YrBuilt < 1940)
                NL = 1.29;
            else if (YrBuilt < 1970)
                NL = 1.03;
            else if (YrBuilt < 1990)
                NL = 0.65;
            else
                NL = 0.31;
        }
        else
        {
            if (YrBuilt < 1940)
                NL = 0.58;
            else if (YrBuilt < 1970)
                NL = 0.49;
            else if (YrBuilt < 1990)
                NL = 0.36;
            else
                NL = 0.24;
        }

        double ELA = NL * A_Floor * 0.0929 / 1000 / pow(Stories, 0.3); //This ? estimated leakage area" is in m^2
        ELA = 1550 * ELA;  //Now it's inches
        double Cs, Cw;
        if (1 == Stories)
        {
            Cs = 0.015;
            Cw = 0.0065;
        }
        else if (Stories == 2)
        {
            Cs = 0.0299;
            Cw = 0.0086;
        }
        else
        {
            Cs = 0.045;
            Cw = 0.0101;
        }

        //This is the end of getting the air changes, for this part of the code

        //Default is stick frame construction. Renv started as defaults from Building America, based solely on age. 
        // But then was scaled to fit BeOpt for the older case (R increase from 4 to 5)
        // Windows are NOT separated out for conduction calcs but I put them here just in case (UWin)
        double Renv, SHGC;
        if (YrBuilt > 1990 || EnergyRetrofits == 1)
        {
            Renv = 16; //These are in IP Units hr*ft ^ 2 * degF / BTU
            //Uwin = 0.4;
            SHGC = 0.25;
        }
        else if (YrBuilt >= 1980)
        {
            Renv = 12;
            //Uwin = 0.4;
            SHGC = 0.25;
        }
        else
        {
            Renv = 5;
            //Uwin = 1;
            SHGC = 0.53; //This basically matches BEOPT 0.76.WHY ? ? ? ? Possibly Bldg AM 0.7 factor for internal shading.MJB suggests shading or diffuse.Says 0.5, 025 fine!
        }
        //assign("Renvdebug", var_data(Renv));

        double Cenv = 2; //BTU / ft^2degF    Note that this is stick frame - more like 10 for masonry, or 18 for heavy masonry(comm)
        double hsurf = 0.68; //Same units as Renv hr*ft ^ 2 * degF / Btu
        double Cmass = 1.6; //BTU / ft^2degF --doesn't change much!

        // C*1.8 + 32 = F

        //Those are sort of all the default values(above)

        ssc_number_t TambFAvg[12];
        monthly_averages(T_ambF, TambFAvg);

        // Is it heating or cooling season ? ? This methodology taken entirely from Building America guidelines
        double HtEn[13] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        double ClEn[13] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; //Because Jul and Aug always have cooling on!BLDG AM //jmf removed these to see if this works in other hemispheres!!
        for (int m = 0; m < 12; m++)
        {
            if (TambFAvg[m] <= 66)
                HtEn[m] = 1;
            else
                ClEn[m] = 1;
        }
        ClEn[12] = ClEn[0];
        HtEn[12] = HtEn[0];

        std::vector<double> HtEnNew(13);
        std::vector<double> ClEnNew(13);

        for (int i = 0; i < 13; i++)
        {
            HtEnNew[i] = HtEn[i];
            ClEnNew[i] = ClEn[i];
        }

        for (int m = 0; m < 12; m++)
        {
            if (ClEn[m] == 0 && ClEn[m + 1] == 1)
            {
                HtEnNew[m + 1] = 1;
                ClEnNew[m] = 1;
            }
            else if (HtEn[m] == 0 && HtEn[m + 1] == 1)
                ClEnNew[m + 1] = 1;
        }


        for (int i = 0; i < 13; i++)
        {
            HtEn[i] = HtEnNew[i];
            ClEn[i] = ClEnNew[i];
        }

        //Determines how much of the solar gain distributed to internal mass and how
        //much to the walls
        double SolMassFrac = 0.2;
        double SolEnvFrac = 1 - SolMassFrac;

        //BUILDING DIMENSIONMS
        //A_wins = A_Floor*WFR; %Assumed distributed evenly on walls
        double A_Wins = sqrt(A_Floor / Stories) * 4 * H_ceiling * Stories * WWR;
        double A_Walls = sqrt(A_Floor / Stories) * 4 * (H_ceiling)*Stories - A_Wins;   //It's a cube
        double Aenv = A_Walls + 2 * A_Floor; //This one includes floor
        double V_bldg = A_Floor * H_ceiling * Stories; //Exclude the plenum from conditioned volume
//		double AIntWall = A_Floor / 2; //Interior partition walls - typical default
        double AIntMass = 0.4 * A_Floor; //Bldg AM default for internal mass
//		double AIntTot = A_Wins + Aenv + AIntWall + AIntMass;
        double Cair = 0.075 * 0.245 * V_bldg * 10; //BTU / degF  Note adjust factor of 10 --MJB

        //INTERNAL LOADS	
        double PerPersonLoad = 220 / 3412.142; //BTU / hr / person to kWh / person(BLDG America for single zone) --sensible only
        //Divide into radiative and convective(heat transfer to mass vs.transfer to air)

        double PPL_rad[24], PPL_conv[24];
        for (int i = 0; i < 24; i++)
        {
            PPL_rad[i] = 0.6 * PerPersonLoad * ceil(Occupants * Occ_Schedule[i]);
            PPL_conv[i] = 0.4 * PerPersonLoad * ceil(Occupants * Occ_Schedule[i]);
        }

        //These are all from building america -- annual kWh loads
        double NBR = round((Occupants - 0.87) / 0.59); //number of bedrooms
        double Load_light = 2000;
        double Load_fridge = 600 * en_fridge;
        double Load_range = (250 + 83 * NBR) * en_range;
        double Load_dw = (87.6 + 29.2 * NBR) * en_dish;
        double Load_wash = (38.8 + 12.9 * NBR) * en_wash;
        double Load_dry = (538.2 + 179.4 * NBR) * en_dry;
        double Load_mels = 1595 + 248 * NBR + .426 * A_Floor * en_mels;

        //Now the HOURLY plug loads, weekday and weekend!These painstakingly
        //scaled from BeOpt and Bldg America.The loads are separated out because
        //user could have some and not others in house
        double FridgeFrac[24] = { 4, 3.9, 3.8, 3.7, 3.6, 3.6, 3.7, 4, 4.1, 4.2, 4, 4, 4.2, 4.2, 4.2, 4.2, 4.5, 4.8, 5, 4.8, 4.6, 4.5, 4.4, 4.2 };
        double DWFrac[24] = { 17, 14, 13, 12, 12, 15, 20, 30, 60, 64, 57, 50, 40, 48, 38, 35, 38, 50, 88, 110, 90, 68, 46, 33 };
        double RangeFrac[24] = { 8, 8, 5, 5, 8, 10, 26, 44, 48, 50, 44, 50, 57, 48, 45, 55, 85, 150, 120, 60, 40, 25, 15, 10 };
        double WasherFrac[24] = { 10, 8, 5, 5, 8, 10, 20, 50, 70, 85, 85, 75, 68, 60, 52, 50, 50, 50, 50, 50, 50, 47, 30, 17 };
        double DryerFrac[24] = { 10, 8, 5, 5, 8, 10, 10, 20, 50, 70, 85, 85, 75, 68, 60, 52, 50, 50, 50, 50, 50, 47, 30, 17 };

        double FridgeHrFrac = Load_fridge / (sum(FridgeFrac, 24) * (1 + 0.1 * 2 / 7));
        double DWHrFrac = Load_dw / (sum(DWFrac, 24) * (1 + 0.1 * 2 / 7));
        double RangeHrFrac = Load_range / (sum(RangeFrac, 24) * (1 + 0.1 * 2 / 7));
        double WasherHrFrac = Load_wash / (sum(WasherFrac, 24) * (1 + 0.1 * 2 / 7));
        double DryerHrFrac = Load_dry / (sum(DryerFrac, 24) * (1 + 0.1 * 2 / 7));

        std::vector<double> FridgeHourly(24);
        std::vector<double> FridgeHourlyWkend(24);
        std::vector<double> DWHourly(24);
        std::vector<double> DWHourlyWkend(24);
        std::vector<double> RangeHourly(24);
        std::vector<double> RangeHourlyWkend(24);
        std::vector<double> WasherHourly(24);
        std::vector<double> WasherHourlyWkend(24);
        std::vector<double> DryerHourly(24);
        std::vector<double> DryerHourlyWkend(24);

        for (int i = 0; i < 24; i++)
        {
            FridgeHourly[i] = FridgeFrac[i] * FridgeHrFrac;
            FridgeHourlyWkend[i] = FridgeHourly[i] * 1.1;
            DWHourly[i] = DWFrac[i] * DWHrFrac;
            DWHourlyWkend[i] = DWHourly[i] * 1.1;
            RangeHourly[i] = RangeFrac[i] * RangeHrFrac;
            RangeHourlyWkend[i] = RangeHourly[i] * 1.1;
            WasherHourly[i] = WasherFrac[i] * WasherHrFrac;
            WasherHourlyWkend[i] = WasherHourly[i] * 1.1;
            DryerHourly[i] = DryerFrac[i] * DryerHrFrac;
            DryerHourlyWkend[i] = DryerHourly[i] * 1.1;

        }

        //Weekday and weekend loads, summed and assumed half radiative / half
        //convective
        std::vector<double> TotalPlugHourlyWkday(24), SensibleEquipRadorConvWkday(24), SensibleEquipRadorConvWkend(24), TotalPlugHourlyWkend(24);
        for (int i = 0; i < 24; i++)
        {
            TotalPlugHourlyWkday[i] = (FridgeHourly[i] + DWHourly[i] + RangeHourly[i] + WasherHourly[i] + DryerHourly[i]) / 365;
            SensibleEquipRadorConvWkday[i] = 0.5 * (FridgeHourly[i] + DWHourly[i] * 0.6 + RangeHourly[i] * 0.4 + WasherHourly[i] * 0.8 + DryerHourly[i] * 0.15) / 365;
            SensibleEquipRadorConvWkend[i] = SensibleEquipRadorConvWkday[i] * 1.1;   //These are just 50 / 50 rad and conv, but the multipliers are BLDG AM sensible load fractions
            TotalPlugHourlyWkend[i] = (FridgeHourlyWkend[i] + DWHourlyWkend[i] + RangeHourlyWkend[i] + WasherHourlyWkend[i] + DryerHourlyWkend[i]) / 365;
        }

        //Vacation!affects ONLY THE LARGE APPLIANCES AND OCCUPANCY as per Bldg AM
        //But why would DW, etc run if nobody home ? I set to just fridge for
        //appliances.
        std::vector<double> TotalPlugHourlyVacay(24), SensibleEquipRadorConvVacay(24);
        for (int i = 0; i < 24; i++)
        {
            TotalPlugHourlyVacay[i] = FridgeHourly[i] / 365;
            SensibleEquipRadorConvVacay[i] = 0.5 * FridgeHourly[i] / 365;
        }

        //Now the Hourly MELS; these use the January BeOpt and then vary
        // (imprecisely)by month based on other months' BeOpts.   They do NOT vary
        //weekday vs.weekend, as per BeOpt.
        double MELSFrac[24] = { 0.441138, 0.406172, 0.401462, 0.395811, 0.380859, 0.425009, 0.491056, 0.521783, 0.441138, 0.375444, 0.384274, 0.384391, 0.377916, 0.390984, 0.4130, 0.435957, 0.515661, 0.626446, 0.680131, 0.702029, 0.726164, 0.709211, 0.613731, 0.533321 };
        double MELSMonthly[12] = { 1, 1, .88, .88, .88, .77, .77, .77, .77, .85, .85, 1 };
        std::vector<double> MELSHourlyJan(24);
        double MELSHrFrac;
        double MELSMonthSum[12];
        double MELSFracSum = sum(MELSFrac, 24);

        for (int i = 0; i < 12; i++)
        {
            MELSMonthSum[i] = MELSFracSum * MELSMonthly[i] * util::nday[i];
        }
        MELSHrFrac = Load_mels / sum(MELSMonthSum, 12);

        for (int i = 0; i < 24; i++)
        {
            MELSHourlyJan[i] = MELSHrFrac * MELSFrac[i];
        }
        //And then the lighting.These again use Jan.BeOpt as a basis, but the
        //fractions vary by hour AND by month.This may be climate dependent but
        //that is ignored for now.The matrix will be rows are months and colums are the time
        //periods, ie 9 - 6, 7 - 3, 4 - 8.
        double LightFrac[24] = { 0.1758680, 0.1055210, 0.070347, 0.070347, 0.070347, 0.076992, 0.165661, 0.345977, 0.330648, 0.169731, 0.13829, 0.137022, 0.137272, 0.140247, 0.158163, 0.230715, 0.418541, 0.710948, 0.931195, 0.88306, 0.790511, 0.675746, 0.509894, 0.354185 };
        double L96 = sumsub(LightFrac, 0, 5) + sumsub(LightFrac, 20, 23);
        double L73 = sumsub(LightFrac, 6, 14);
        double L48 = sumsub(LightFrac, 15, 19);
        double Lightz[12][3] = { { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 } };
        //LightMonHr = [1	1	1
        // 1	0.77	0.76
        // 1	0.52	0.55
        // 1	0.51	0.28
        // 1	0.41	0.21
        // 1	0.39	0.19
        // 1	0.41	0.19
        // 1	0.47	0.23
        // 1	0.61	0.36
        // 1	0.82	0.57
        // 1	0.84	1.02
        // 1	1.04	1.14
        //];
        double LightMonHr[12][3] = {
            { 1, 1.05, 1.05 },
            { 1, 0.9, 1 },
            {1, 0.6, 0.75},
            {1, 0.61, 0.3},
            {1, 0.45, 0.10},
            {1, 0.42, 0.10},
            {1, 0.43, 0.10},
            {1, 0.51, 0.14},
            {1, 0.62, 0.38},
            {1, 0.82, 0.52},
            {1, 0.84, 1.02},
            {1, 1.04, 1.14}
        };

        double LightUse[12][3];
        for (int i = 0; i < 12; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                LightUse[i][j] = Lightz[i][j] * LightMonHr[i][j];
            }
        }

        std::vector<double> MonthlyDailyLightUse(12);

        for (int i = 0; i < 12; i++)
        {
            MonthlyDailyLightUse[i] = sum(LightUse[i], 3);
        }

        double AnnualLightUseHrs = 0;
        for (int i = 0; i < 12; i++)
        {
            AnnualLightUseHrs += MonthlyDailyLightUse[i] * util::nday[i];
        }

        double  LightHrFrac = Load_light / AnnualLightUseHrs;
        std::vector<double> LightHourlyJan(24);
        for (int i = 0; i < 24; i++)
        {
            LightHourlyJan[i] = LightHrFrac * LightFrac[i];
        }

        //NEED UNITS!!!!!!
        //END INTERNAL ELEC LOADS

        //THIS IS TO FIGURE OUT THE HVAC FAN USAGE WITH GAS HEATER
        //Capacity of gas heater....really should be more climate - dependent
        double GasHeat_capacity;
        if (YrBuilt > 1980 || EnergyRetrofits == 1)
        {
            //n_heat = 0.78 HVAC sys efficiency for gas forced air
            GasHeat_capacity = 35 * A_Floor / 1000; //kBTU / h
        }
        else
        {
            //n_heat = 0.65 
            GasHeat_capacity = 40 * A_Floor / 1000;
        };

        //Aux elec for gas heater(fans, etc)
        double AuxHeat;
        if (en_heat == 0) // assume gas heater if user says heat is not electric
        //This is Bldg AM number :
            AuxHeat = 9.2 * GasHeat_capacity; //kWh annual, should divide over running hours in the end
        else
            AuxHeat = 0;
        //END HEATING FANS

        //COOLING SEER
        double SEER;
        if (YrBuilt >= 2005 || EnergyRetrofits == 1)
            // Note that I don't separate out fans/pumps, though BeOPT does!?  But my
            //predictions are *still* high.
            SEER = 13;
        else
            SEER = 10;
        //END COOLING SEER

        //day of the week: Days 1-7.
        int D = 1; //somehow I am one day off BEOPT so compensating here(this is days of the week)
        // TMY DEFAULT IS MONDAY!!!!!!!
        //Sol - Air -- This part is ALL SI -- get effective envelope temperatures for the heat transfer.
        std::vector<double> Vacay(8760), Hset(8760), Cset(8760);
        std::vector<double> Tmass(8760), Tair(8760), Tsurf(8760);
        double Heaton[8760];
        //All the initial loads - divided into radiatinve & convective

        std::vector<double> EquipElecHrLoad(8760), EquipRadHrLoad(8760), EquipConvHrLoad(8760), MELSElecHrLoad(8760), MELSRadHrLoad(8760);
        std::vector<double> MELSConvHrLoad(8760), LightElecHrLoad(8760), LightRadHrLoad(8760), LightConvHrLoad(8760), PPLRadHrLoad(8760), PPLConvHrLoad(8760);
        std::vector<double> TAnew(8760), TSnew(8760), TMnew(8760);
        std::vector<double> QInt_Rad(8760), QInt_Conv(8760), Q_SolWin(8760);
        std::vector<double> CFM(8760), UAInf(8760), QInf(8760), QG(8760);
        std::vector<double> QN(8760), QHV2(8760), Tdiff(8760);


        //std::vector<double> HourlyNonHVACLoad(8760);

        //MAIN 8760 LOOP STARTS HERE********************************************************************************************************************************************************************

        for (int j = 0; j < 8763; j++) //need to re-run the first three hours, since they depend on previous hours. Therefore, run this loop **8762** times.
        //for (int j = 0; j < 8760; j++)
        {
            //counters to enable re-running the first two hours
            int i = j;
            int iprev = i - 1;
            int inext = i + 1;
            bool flag = false; //flag to pre-set first hour ONLY the first time 
            if (i == 8759) //hour 8760
                inext = 0; //loop back to first hour
            else if (i == 8760) //re-running first hour
            {
                iprev = 8759; //loop back to last hour
                inext = 1;
                i = 0; //overwrite first hour values
                flag = true; //do not pre-set first hour this time
            }
            else if (i == 8761) //re-running second hour
            {
                iprev = 0;
                inext = 2;
                i = 1; //overwrite second hour values
            }
            else if (i == 8762) //re-running third hour
            {
                iprev = 1;
                inext = 3;
                i = 2; //overwrite third hour values
            }

            //start the actual algorithm
            int Hr = hour[i];
            int NextHr = hour[inext];

            //The day of the week (to figure out weekends)
            if (Hr == 0) //first hour of a new day
            {
                D = D + 1; //increment the day of the week
                if (D > 7)
                    D = 1;
            }
            int Mon = month[i];
            int Dy = day[i];
            int NextMon = month[inext];
            int NextDay = day[inext];

            //Are we on vacation ?
            for (int v = 0; v < N_vacation; v++)
            {
                if (Mon == (VacationMonths[v] - 1) && Dy == (VacationDays[v] - 1)) //need to subtract 1 from VacationMonths and VacationDays because Mon and Dy are 0 subscripted (Jan=0)
                    Vacay[i] = 1;

                if (NextMon == (VacationMonths[v] - 1) && NextDay == (VacationDays[v] - 1))
                    Vacay[inext] = 1;
            }

            //First the setpoints for heating / cooling
            if (Vacay[i] == 0 && T_Sched[Hr] == 1)
            {
                Hset[i] = THeat;
                Cset[i] = TCool;
            }
            else  //setback if on vacation or if temperature schedule says so
            {
                Hset[i] = THeatSB;
                Cset[i] = TCoolSB;
            };

            if (i == 0 && flag == false) // First hour has to be preset.It's January so I assume it's heating. ONLY PRESET FIRST HOUR ON THE FIRST RUN THROUGH.
            {
                Tmass[i] = Hset[i];
                Tair[i] = Hset[i];
                Tsurf[i] = Hset[i];
                Heaton[i] = HtEn[1];
                //All the initial loads - divided into radiatinve & convective
                EquipElecHrLoad[i] = TotalPlugHourlyWkend[Hr];
                EquipRadHrLoad[i] = SensibleEquipRadorConvWkend[Hr];
                EquipConvHrLoad[i] = SensibleEquipRadorConvWkend[Hr];
                MELSElecHrLoad[i] = MELSHourlyJan[Hr];
                MELSRadHrLoad[i] = MELSElecHrLoad[i] * 0.5 * 0.734;
                MELSConvHrLoad[i] = MELSElecHrLoad[i] * 0.5 * 0.734;
                LightElecHrLoad[i] = LightHourlyJan[Hr];
                LightRadHrLoad[i] = LightElecHrLoad[i] * 0.7;
                LightConvHrLoad[i] = LightElecHrLoad[i] * 0.3;
                PPLRadHrLoad[i] = PPL_rad[Hr];
                PPLConvHrLoad[i] = PPL_conv[Hr];
                if (Vacay[i] == 1) // Less loads if on vacation
                {
                    EquipElecHrLoad[i] = TotalPlugHourlyVacay[Hr];
                    EquipRadHrLoad[i] = SensibleEquipRadorConvVacay[Hr];
                    EquipConvHrLoad[i] = EquipRadHrLoad[i];
                    PPLRadHrLoad[i] = 0;
                    PPLConvHrLoad[i] = 0;
                }
            }
            /*
            if (i == 8760) // assign vals for hour "8761" which is just part of the euler forward calculation
            {
                EquipElecHrLoad[inext] = EquipElecHrLoad[1];
                EquipRadHrLoad[inext] = SensibleEquipRadorConvWkend[1];
                EquipConvHrLoad[inext] = SensibleEquipRadorConvWkend[1];
                MELSElecHrLoad[inext] = MELSHourlyJan[1];
                MELSRadHrLoad[inext] = MELSElecHrLoad[1] * 0.5*0.734;
                MELSConvHrLoad[inext] = MELSElecHrLoad[1] * 0.5*0.734;
                LightElecHrLoad[inext] = LightHourlyJan[1];
                LightRadHrLoad[inext] = LightElecHrLoad[1] * 0.7;
                LightConvHrLoad[inext] = LightElecHrLoad[1] * 0.3;
                PPLRadHrLoad[inext] = PPL_rad[1];
                PPLConvHrLoad[inext] = PPL_conv[1];
                // assign i+1 values for hour 8760 if Jan 1 is a vacation day
                for (int v = 0; v < N_vacation; v++)
                {
                    if (VacationMonths[v] == 1 && VacationDays[v] == 1)
                    {
                        EquipElecHrLoad[inext] = TotalPlugHourlyVacay[1];
                        EquipRadHrLoad[inext] = SensibleEquipRadorConvVacay[1];
                        PPLRadHrLoad[inext] = 0;
                        PPLConvHrLoad[inext] = 0;
                    }
                }
            }
            */
            if (i > 0 || flag == true) // These are the new values for each temperature, which were determined previous timestep. USE PREVIOUS TIMESTEP VALUES FOR RE-RUNNING FIRST TWO HOURS.
            {
                Tair[i] = TAnew[iprev];
                Tsurf[i] = TSnew[iprev];
                Tmass[i] = TMnew[iprev];
            }

            //Interior gains
            //Equipment load -- depends on whether weekday or weekend
            if (Vacay[inext] == 1) // Vacation
            {
                EquipElecHrLoad[inext] = TotalPlugHourlyVacay[NextHr];
                EquipRadHrLoad[inext] = SensibleEquipRadorConvVacay[NextHr];
                EquipConvHrLoad[inext] = SensibleEquipRadorConvVacay[NextHr];
            }
            else if ((D == 2 && Hr < 23) || (D == 7 && Hr == 23) || D == 1) // weekend!(hour inext)
            {
                EquipElecHrLoad[inext] = TotalPlugHourlyWkend[NextHr];
                EquipRadHrLoad[inext] = SensibleEquipRadorConvWkend[NextHr];   //These are just 50 / 50 rad and conv, but the multipliers are BLDG AM sensible load fractions
                EquipConvHrLoad[inext] = SensibleEquipRadorConvWkend[NextHr];
            }
            else //weekday(hour inext)
            {
                EquipElecHrLoad[inext] = TotalPlugHourlyWkday[NextHr];
                EquipRadHrLoad[inext] = SensibleEquipRadorConvWkday[NextHr];   //These are just 50 / 50 rad and conv, but the multipliers are BLDG AM sensible load fractions
                EquipConvHrLoad[inext] = SensibleEquipRadorConvWkday[NextHr];
            }

            //Next the MELS
            MELSElecHrLoad[inext] = MELSHourlyJan[NextHr] * MELSMonthly[NextMon];
            MELSRadHrLoad[inext] = MELSElecHrLoad[inext] * 0.734 * 0.5;
            MELSConvHrLoad[inext] = MELSElecHrLoad[inext] * 0.734 * 0.5;

            //And the lighting
            int ind = 0;
            if (NextHr > 19 || NextHr < 6)
                ind = 0;
            else if (NextHr > 5 && NextHr < 15)
                ind = 1;
            else ind = 2;

            LightElecHrLoad[inext] = LightHourlyJan[NextHr] * LightMonHr[NextMon][ind];
            LightRadHrLoad[inext] = LightElecHrLoad[inext] * 0.7;
            LightConvHrLoad[inext] = LightElecHrLoad[inext] * 0.3;

            //And finally the people!
            if (Vacay[inext] == 1)
            {
                PPLRadHrLoad[inext] = 0;
                PPLConvHrLoad[inext] = 0;
            }
            else
            {
                PPLRadHrLoad[inext] = PPL_rad[NextHr];
                PPLConvHrLoad[inext] = PPL_conv[NextHr];
            }

            //Convert internal gains to BTU / hr(radiative and convective)
            QInt_Rad[inext] = 3412.142 * (PPLRadHrLoad[inext] + LightRadHrLoad[inext] + MELSRadHrLoad[inext] + EquipRadHrLoad[inext]);
            QInt_Conv[inext] = 3412.142 * (PPLConvHrLoad[inext] + LightConvHrLoad[inext] + MELSConvHrLoad[inext] + EquipConvHrLoad[inext]);

            //Solar Gains - distributed to envelope evenly(all walls, ceil, floor)
            // and to internal mass, with fractions of each denoted above.
            Q_SolWin[inext] = SHGC * (RadWallE[inext] + RadWallW[inext] + RadWallN[inext] + RadWallS[inext]) / 4 * A_Wins / 10.764; //This is now Watts
            Q_SolWin[inext] = Q_SolWin[inext] * 3.412;  //And now it's BTU/hr

            //Get the infiltration(cheating - using this hr's Tair; is assumed similar to prev hour's)
            CFM[inext] = ELA * sqrt(Cs * fabs(T_ambF[inext] - Tair[i]) + Cw * pow(VwindMPH[inext], 2)) * 0.67;
            UAInf[inext] = CFM[inext] * 60 * 0.018;
            QInf[i] = UAInf[i] * (T_ambF[i] - Tair[i]); //This would be BTU / hr -- all convective
            QG[i] = QInt_Conv[i] + QInf[i]; //Same!

            //Calculate the new air temperature in the space(euler forward)
            double bar = 1 + dT / Cenv / Renv + dT / hsurf / Cenv;
            double bardub = 1 + dT / Cmass / hsurf;
            double TAnewBot = 1 + UAInf[inext] * dT / Cair + dT / Cair * AIntMass / hsurf - pow(dT, 2) * AIntMass / Cair / Cmass / hsurf / hsurf / bardub + Aenv * dT / Cair / hsurf - dT * Aenv / Cair / Cenv / hsurf / hsurf / bar;
            double TAnewTop = Tair[i] + dT / Cair * (QInt_Conv[inext] + UAInf[inext] * T_ambF[inext]) + dT * AIntMass * (Tmass[i] + SolMassFrac * (Q_SolWin[inext] / AIntMass + QInt_Rad[inext] / AIntMass)) / Cair / hsurf / bardub + Aenv * dT / Cair / hsurf / bar * (Tsurf[i] + dT * T_solairF[inext] / Cenv / Renv + SolEnvFrac * (Q_SolWin[inext] / Aenv + QInt_Rad[inext] / Aenv));
            TAnew[i] = TAnewTop / TAnewBot;

            //Plug loads (non-HVAC)
            double HourlyNonHVACLoad = (LightElecHrLoad[i] + MELSElecHrLoad[i] + EquipElecHrLoad[i]); //kWh
            if (j < 8760) //do not reassign non_hvac load on the second loop through
                non_hvac_load[i] = (ssc_number_t)HourlyNonHVACLoad * 1000; //Wh

            //Set HVAC max to avoid weird spikes in the calculated load.
            //These are educated guesses.They only apply to 1 - 2 hours per year so I think it's ok that they're not particularly detailed
            double HeatMaxBTU = (A_Floor * 60 > 10000) ? A_Floor * 60 : 10000;
            double CoolMaxBTU = (A_Floor * 20 > 10000) ? A_Floor * 20 : 10000;

            //Now for the HVAC controls
            if (Cset[i] >= TAnew[i] && Hset[i] <= TAnew[i]) // Temperature is ok, no HVAC
            {
                Heaton[i] = 0;
                QN[inext] = 0;
                QHV2[inext] = 0;
            }
            else if (Cset[i] <= TAnew[i]) // Cooling temperature requirement met
            {
                if (ClEn[NextMon] == 0) // This is if not in cooling season!
                {
                    Heaton[i] = 0;
                    QN[inext] = 0;
                    QHV2[inext] = 0;
                }
                else    //Actually Cooling
                {
                    Heaton[i] = 0;
                    Tdiff[i] = TAnew[i] - Cset[i];
                    TAnew[i] = Cset[i];
                    QN[inext] = Cair / dT / 1 * (TAnew[i] * TAnewBot - TAnewTop);  //BTU
                    if (ClEn[Mon] == 0 && ClEn[NextMon] == 1)
                        QN[inext] = -1 * ((fabs(QN[inext]) < CoolMaxBTU) ? fabs(QN[inext]) : CoolMaxBTU);
                    QHV2[inext] = QN[i] / SEER * en_cool;
                }
            }
            else if (HtEn[NextMon] == 0) // Heating temperature met, but not in season
            {
                Heaton[i] = 0;
                QN[inext] = 0;
                QHV2[inext] = 0;
            }
            else //really heating
            {
                Heaton[i] = 1;
                Tdiff[i] = Hset[i] - TAnew[i];
                TAnew[i] = Hset[i];
                QN[inext] = Cair / dT * (TAnew[i] * TAnewBot - TAnewTop);  //BTU
                if (HtEn[Mon] == 0 && HtEn[NextMon] == 1)
                    QN[inext] = (QN[inext] < HeatMaxBTU) ? QN[inext] : HeatMaxBTU;
                QHV2[inext] = (QN[i] * 0.2931) * en_heat; //Wh
            }
            TMnew[i] = (Tmass[i] + dT / Cmass * (TAnew[i] / hsurf + SolMassFrac * (Q_SolWin[i] + QInt_Rad[i]) / AIntMass)) / bardub;
            TSnew[i] = (Tsurf[i] + dT / Cenv * (SolEnvFrac * (Q_SolWin[i] / Aenv + QInt_Rad[i] / Aenv) + T_solairF[inext] / Renv + TAnew[i] / hsurf)) / bar;

            //HVAC Loads completed
            hvac_load[i] = (ssc_number_t)fabs(QHV2[i]); //Wh

            //Total load for the hour
            load[i] = hvac_load[i] + non_hvac_load[i]; //Wh

            /*//Debugging outputs
            QNdebug[i] = QN[i];
            Cairdebug[i] = Cair;
            TAnewdebug[i] = TAnew[i];
            TAnewTopdebug[i] = TAnewTop;
            TAnewBotdebug[i] = TAnewBot;
            UAinfdebug[i] = UAInf[i];
            bardebug[i] = bar;
            bardubdebug[i] = bardub;
            */
            /*
            int dayz = day[i];
            int hourz = hour[i];
            double loadz = load[i];
            double hvacz = hvac_load[i];
            double non_hvacz = non_hvac_load[i];
            double qhvz = QHV2[i];
            int stophere = 1;
            */

        }

        //Aux heating fans(if gas heat)
        double HrsHeat = sum(Heaton, 8760);
        double AuxHeatPerHr;
        std::vector<double> fan_load(8760);
        if (HrsHeat != 0)
        {
            AuxHeatPerHr = AuxHeat / HrsHeat / 1000; //This is Wh
            for (size_t i = 0; i < 8760; i++)
            {
                if (Heaton[i])
                {
                    //hvac_load[i] += AuxHeatPerHr; //Wh
                    //load[i] = hvac_load[i] + non_hvac_load[i]; //Wh
                    fan_load[i] = AuxHeatPerHr; //Wh
                }
            }
        }

        //SCALING FUNCTION STARTS HERE********************************************************************************************************************************************************************
        ssc_number_t monthly_load[12], monthly_hvac_load[12];
        std::vector<double> monthly_diff(12), monthly_scale(12);
        //compute monthly load sums for scaling
        monthly_sums(load, monthly_load);
        monthly_sums(hvac_load, monthly_hvac_load);
        // compute the monthly difference in load and the load scaling factor for that month
        for (int i = 0; i < 12; i++)
        {
            monthly_diff[i] = monthly_load[i] - monthly_util[i] * 1000; //utility bill is input as kWh, needs to be converted to Wh for comparison
            if (monthly_load[i] != 0)
                monthly_scale[i] = monthly_diff[i] / monthly_load[i];
            else //avoid divide by zero issues
                monthly_scale[i] = 0;
        }
        int min_diff_month = 0;
        double min_diff = fabs(monthly_diff[0]);
        for (int i = 1; i < 12; i++)
        {
            if (fabs(monthly_diff[i]) < min_diff)
            {
                min_diff = fabs(monthly_diff[i]); //lowest energy difference of the months
                min_diff_month = i; //and the month that it happens in
            }
        }
        double closest_scale_avg = monthly_scale[min_diff_month]; //scaling factor corresponding to the lowest energy difference month
        std::vector<double> x_hvac(12);
        for (int i = 0; i < 12; i++)
        {
            if (monthly_hvac_load[i] < 5) //error checking to avoid negative spikes
                monthly_hvac_load[i] = 0;
            x_hvac[i] = (monthly_diff[i] - (monthly_load[i] * closest_scale_avg)) / monthly_hvac_load[i]; //hvac fraction of scaling
            //new error checking from Sara 11/21
            if (x_hvac[i] > 0.9) x_hvac[i] = 0.9;
            if (x_hvac[i] < -1) x_hvac[i] = -1;
            //end new error checking from Sara
            //xhvacdebug[i] = x_hvac[i];
        }
        //new error checking from Sara
        std::vector<double> NewScale(12);
        for (int z = 0; z < 12; z++)
        {
            NewScale[z] = (monthly_load[z] - monthly_util[z] * 1000 - x_hvac[z] * monthly_hvac_load[z]) / monthly_load[z]; //monthly utility is in kWh
            /*
            newscaledebug[z] = NewScale[z];
            monthlytotelecactualdebug[z] = monthly_util[z];
            monthlytotelec_lpgendebug[z] = monthly_load[z];
            monthlyhvac_lpgendebug[z] = monthly_hvac_load[z];
            */
        }
        //end new error checking from Sara

        //loop through 8760 and scale according to what month it's in
        // also check for any negative values and convert to kWh
        int nneg = 0;
        for (int i = 0; i < 8760; i++)
        {
            if (monthly_hvac_load[month[i]] > 0)
                load[i] = (ssc_number_t)(load[i] * (1 - NewScale[month[i]]) - x_hvac[month[i]] * hvac_load[i]); //new from Sara 11/21
            else
                load[i] = load[i] * (ssc_number_t)(1 - monthly_scale[month[i]]);

            if (monthly_util[month[i]] == 0) //set all loads for the month to zero if the input month was zero
                load[i] = 0;


            if (load[i] < 0) //error checking for negative loads
            {
                load[i] = 0;
                nneg++;
            }

            load[i] *= (ssc_number_t)0.001; // convert to kWh
        }

        if (nneg > 0)
        {
            log(util::format("The building electric load profile estimator calculated negative loads for %d hours. "
                "Loads for these hours were set to zero; however, this may indicate a problem with your inputs.",
                nneg), SSC_WARNING);
        }
    }
};

DEFINE_MODULE_ENTRY(belpe, "Estimates an electric load profile given basic building characteristics and a weather file", 1)
