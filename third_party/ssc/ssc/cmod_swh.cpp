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

#include <math.h>

#include "common.h"

#include "core.h"
#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_time.h"
#include "lib_util.h"

/* -------------------------------------

v10a
- deleted commented out code
- moved some comments

3 Mode Model
"SIMPLIFIED SOLAR WATER HEATER SIMULATION
 USING ON A MULTI-MODE TANK MODEL"
Craig Christensen, Jeff Maguire, Jay Burch, Nick DiOrio

Technical reference: Duffie and Beckman (D&B) "Solar Engineering of Thermal Processes" 3rd Edition 2006
Time Marching Method: Implicit Euler
Conduction Between Nodes: Off

SUBHOURLY VERSION !!!
Uses inputs from TRNSYS subhourly outputs for:
1. Irradiation quantities on tilted surface
2. Weather -> Ambient temperature, mains temperature
3. Angle of Incidence

Still outputs hourly quantities

7/7/2015 - Nick DiOrio - modified FprimeUL to use single collector not full system area
-------------------------------------- */

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_swh[] = {
	/*   VARTYPE           DATATYPE         NAME                      LABEL                              UNITS     META                                  GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",   "local weather file path",             "",        "",                                  "Solar Resource",   "?",                      "LOCAL_FILE",                         "" },
    { SSC_INPUT,        SSC_TABLE,       "solar_resource_data",   "Weather data",                        "",        "dn,df,tdry,wspd,lat,lon,tz",        "Solar Resource",   "?",                       "",                              "" },

	{ SSC_INPUT,        SSC_ARRAY,       "scaled_draw",           "Hot water draw",                      "kg/hr",   "",                                  "SWH",              "*",                      "LENGTH=8760",						 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",       "Nameplate capacity",                  "kW",      "",                                  "SWH",              "*",                      "", "" },
	{ SSC_INPUT,        SSC_ARRAY,       "load",                  "Electricity load (year 1)",           "kW",      "",                                  "SWH",              "",                       "", "" },
    { SSC_INPUT,        SSC_ARRAY,       "load_escalation",       "Annual load escalation",              "%/year",  "",                                  "SWH",             "?=0",                    "",                              "" },


	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                  "Collector tilt",                      "deg",     "",                                  "SWH",              "*",                      "MIN=0,MAX=90",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",               "Collector azimuth",                   "deg",     "90=E,180=S",                        "SWH",              "*",                      "MIN=0,MAX=360",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",                "Ground reflectance factor",           "0..1",    "",                                  "SWH",              "*",                      "FACTOR",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",            "Irradiance input mode",               "0/1/2",   "Beam+Diff,Global+Beam,Global+Diff", "SWH",              "?=0",                    "INTEGER,MIN=0,MAX=2",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",             "Tilted surface irradiance model",     "0/1/2",   "Isotropic,HDKR,Perez",  "SWH",      "?=1",                                        "INTEGER,MIN=0,MAX=2",                "" },

	{ SSC_INPUT,        SSC_MATRIX,      "shading:timestep",      "Time step beam shading loss",          "%",      "",                                  "SWH",              "?",                       "",                                  "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading:mxh",           "Month x Hour beam shading loss",       "%",      "",                                  "SWH",              "?",                       "",                                  "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading:azal",          "Azimuth x altitude beam shading loss", "%",      "",                                  "SWH",              "?",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading:diff",          "Diffuse shading loss",                 "%",      "",                                  "SWH",              "?",                       "",                                  "" },


	{ SSC_INPUT,        SSC_NUMBER,      "mdot",                  "Total system mass flow rate",          "kg/s",   "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ncoll",                 "Number of collectors",                 "",       "",                                  "SWH",              "*",                       "POSITIVE,INTEGER",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fluid",			      "Working fluid in system",              "",       "Water,Glycol",                      "SWH",              "*",                       "INTEGER,MIN=0,MAX=1",               "" },

	{ SSC_INPUT,        SSC_NUMBER,      "area_coll",             "Single collector area",                "m2",     "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "FRta",                  "FRta",                                 "",       "",                                  "SWH",              "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "FRUL",                  "FRUL",                                 "",       "",                                  "SWH",              "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "iam",                   "Incidence angle modifier",             "",       "",                                  "SWH",              "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "test_fluid",            "Fluid used in collector test",         "",       "Water,Glycol",                      "SWH",              "*",                       "INTEGER,MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "test_flow",             "Flow rate used in collector test",     "kg/s",   "",                                  "SWH",              "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "pipe_length",           "Length of piping in system",           "m",      "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_diam",             "Pipe diameter",                        "m",      "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_k",                "Pipe insulation conductivity",         "W/m-C", "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_insul",            "Pipe insulation thickness",            "m",      "",                                  "SWH",              "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "tank_h2d_ratio",        "Solar tank height to diameter ratio",  "",       "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "U_tank",                "Solar tank heat loss coefficient",     "W/m2K",  "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "V_tank",                "Solar tank volume",                    "m3",     "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hx_eff",                "Heat exchanger effectiveness",         "0..1",   "",                                  "SWH",              "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "T_room",                "Temperature around solar tank",        "C",      "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_tank_max",            "Max temperature in solar tank",        "C",      "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_set",                 "Set temperature",                      "C",      "",                                  "SWH",              "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "pump_power",            "Pump power",                           "W",      "",                                  "SWH",              "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pump_eff",              "Pumping efficiency",                   "%",      "",                                  "SWH",              "*",                       "PERCENT",                           "" },

	{ SSC_INPUT,        SSC_NUMBER,      "use_custom_mains",      "Use custom mains",                     "%",      "",                                  "SWH",              "*",                       "INTEGER,MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "custom_mains",          "Custom mains",						  "C",      "",                                  "SWH",              "*",                       "LENGTH=8760",                       "" },

	{ SSC_INPUT,        SSC_NUMBER,      "use_custom_set",		  "Use custom set points",                "%",      "",                                  "SWH",              "*",                       "INTEGER,MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "custom_set",            "Custom set points",					  "C",      "",                                  "SWH",              "*",                       "LENGTH=8760",                       "" },



	{ SSC_OUTPUT,       SSC_ARRAY,       "beam",                  "Irradiance - Beam",                    "W/m2",   "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "diffuse",               "Irradiance - Diffuse",                 "W/m2",   "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "I_incident",            "Irradiance - Incident",                "W/m2",   "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "I_transmitted",         "Irradiance - Transmitted",             "W/m2",   "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "shading_loss",          "Shading losses",                       "%",      "",                                  "Time Series",      "*",                        "",                                 "" },


	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_transmitted",         "Q transmitted",                        "kW",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_useful",              "Q useful",                             "kW",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_deliv",               "Q delivered",                          "kW",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_loss",                "Q loss",                               "kW",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_aux",                 "Q auxiliary",                          "kW",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_auxonly",             "Q auxiliary only",                     "kW",      "",                                  "Time Series",      "*",                        "",                                 "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "P_pump",                "P pump",                               "kW",      "",                                 "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_amb",                 "T ambient",						      "C",		"",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_cold",                "T cold",                               "C",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_deliv",               "T delivered",                          "C",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_hot",                 "T hot",                                "C",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_mains",               "T mains",						      "C",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank",                "T tank",                               "C",      "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "V_hot",                 "V hot",                                "m3",     "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "V_cold",                "V cold",                               "m3",     "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "draw",                  "Hot water draw",                       "kg/hr",  "",                                  "Time Series",      "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "mode",                  "Operation mode",                       "",       "1,2,3,4",                           "Time Series",      "*",                        "",                                 "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_Q_deliv",		  "Q delivered",                         "kWh",     "",                                  "Monthly",          "*",                        "LENGTH=12",                        "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_Q_aux",		  "Q auxiliary",                         "kWh",     "",                                  "Monthly",          "*",                        "LENGTH=12",                        "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_Q_auxonly",	  "Q auxiliary only",                    "kWh",     "",                                  "Monthly",          "*",                        "LENGTH=12",                        "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",		  "System energy",                       "kWh",     "",                                  "Monthly",          "*",                        "LENGTH=12",                        "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_Q_deliv",		  "Q delivered",                         "kWh",     "",                                  "Annual",           "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_Q_aux",		  "Q auxiliary",                         "kWh",     "",                                  "Annual",           "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_Q_auxonly",	  "Q auxiliary only",                    "kWh",     "",                                  "Annual",           "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",		  "System energy",                       "kWh",     "",                                  "Annual",           "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "solar_fraction",		  "Solar fraction",                      "",        "",                                  "Annual",           "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",       "Capacity factor",                     "%",       "",                                  "Annual",           "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",            "First year kWh/kW",                   "kWh/kW",  "",                                  "Annual",           "*",                        "",                                 "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ts_shift_hours",        "Time offset for interpreting time series outputs",  "hours", "",                      "Miscellaneous",    "*",                        "",                                 "" },


	var_info_invalid };

class cm_swh : public compute_module
{
public:

	cm_swh()
	{
		add_var_info(_cm_vtab_swh);
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec() override
	{
		const double watt_to_kw = 0.001f;

        std::unique_ptr<weather_data_provider> wdprov;
        if (is_assigned("solar_resource_file"))
        {
            const char *file = as_string("solar_resource_file");
            wdprov = std::unique_ptr<weather_data_provider>(new weatherfile(file));

            auto *wfile = dynamic_cast<weatherfile*>(wdprov.get());
            if (!wfile->ok()) throw exec_error("swh", wfile->message());
            if (wfile->has_message()) log(wfile->message(), SSC_WARNING);
        }
        else if (is_assigned("solar_resource_data"))
        {
            wdprov = std::unique_ptr<weather_data_provider>(new weatherdata(lookup("solar_resource_data")));
        }
        else
            throw exec_error("swh", "no weather data supplied");


		/* **********************************************************************
		Read user specified system parameters from compute engine
		********************************************************************** */

		// assumes instantaneous values, unless hourly file with no minute column specified
		double ts_shift_hours = 0.0;
		bool instantaneous = true;
		if ( wdprov->has_data_column( weather_data_provider::MINUTE ) )
		{
			// if we have an file with a minute column, then
			// the starting time offset equals the time
			// of the first record (for correct plotting)
			// this holds true even for hourly data with a minute column
			weather_record rec;
			if ( wdprov->read( &rec ) )
				ts_shift_hours = rec.minute/60.0;

			wdprov->rewind();
		}
		else if ( wdprov->nrecords() == 8760 )
		{
			// hourly file with no minute data column.  assume
			// integrated/averaged values and use mid point convention for interpreting results
			instantaneous = false;
			ts_shift_hours = 0.5;
		}
		else
			throw exec_error("swh", "subhourly weather files must specify the minute for each record" );

		assign( "ts_shift_hours", var_data( (ssc_number_t)ts_shift_hours ) );

		adjustment_factors haf( this, "adjust" );
		if ( !haf.setup() )
			throw exec_error("swh", "failed to setup adjustment factors: " + haf.error() );


		shading_factor_calculator shad;
		if ( !shad.setup( this, "" ) )
			throw exec_error( "swh", shad.get_error() );

		/* constant fluid properties */
		double Cp_water = 4182.; // Cp_water@40'C (J/kg.K)
		double rho_water = 1000.; // 992.2; // density of water, kg/m3 @ 40'C
		double Cp_glycol = 3400; // 3705; // Cp_glycol

		/* sky model properties */
		double albedo = as_double("albedo"); // ground reflectance fraction
		double tilt = as_double("tilt"); // collector tilt in degrees
		double azimuth = as_double("azimuth"); // collector azimuth in degrees  (180=south, 90=east)
		int irrad_mode = as_integer("irrad_mode"); // 0=beam&diffuse, 1=total&beam, 2=total&diffuse
		int sky_model = as_integer("sky_model"); // 0=isotropic, 1=hdkr, 2=perez

		/* extract arrays */
		size_t len;
		ssc_number_t *draw = as_array("scaled_draw", &len);
		if (len != 8760) throw exec_error("swh", "draw profile must have 8760 values");

		ssc_number_t *custom_mains = as_array("custom_mains", &len);
		if (len != 8760) throw exec_error("swh", "custom mains profile must have 8760 values");

		ssc_number_t *custom_set = as_array("custom_set", &len);
		if (len != 8760) throw exec_error("swh", "custom set temperature profile must have 8760 values");

		/* working fluid settings */
		int ifluid = as_integer("fluid"); // 0=water, 1=glycol
		double fluid_cp = (ifluid == 0) ? Cp_water : Cp_glycol;  // working fluid specific heat in J/kgK

		int itest = as_integer("test_fluid"); // 0=water, 1=glycol
		double test_cp = (itest == 0) ? Cp_water : Cp_glycol;  // test fluid specific heat in J/kgK
		double test_flow = as_double("test_flow"); // collector test flow rate (kg/s)

		/* collector properties */
		double mdot_total = as_double("mdot"); // total system mass flow rate (kg/s)
		double area_total = as_double("area_coll") * as_integer("ncoll"); // total solar collector area (m2)
		double area_coll = as_double("area_coll");

		double FRta = as_double("FRta"); // FR(ta)_n (D&B pp 291) (dimensionless) collector heat removal factor * effective transmittance-absorption product (intercept on efficiency curve); indication of how energy is absorbed.
		double FRUL = as_double("FRUL"); // FRUL (D&B pp 291) (W/m2.C)  collector heat removal factor * collector heat loss coefficient (slope of efficiency curve); indication of how energy is lost.
		double iam = as_double("iam"); // incidence angle modifier coefficient (D&B pp 297) (unitless)

		/* pipe properties */
		double pipe_diam = as_double("pipe_diam"); // pipe diameter in system (m)
		double pipe_k = as_double("pipe_k"); // pipe insulation conductivity (W/m2.C)
		double pipe_insul = as_double("pipe_insul");  // pipe insulation thickness (m)
		double pipe_length = as_double("pipe_length"); // length of piping in system (m)

		/* tank properties */
		double tank_h2d_ratio = as_double("tank_h2d_ratio"); // ratio of tank height to diameter (dimensionless)
		double U_tank = as_double("U_tank"); // W/m2.C storage tank heat loss coefficient (U-value)
		double V_tank = as_double("V_tank"); // solar tank volume (m3)
		double tank_radius = pow(V_tank / (2 * M_PI*tank_h2d_ratio), 0.33333333);
		double tank_height = tank_radius * 2 * tank_h2d_ratio;
		double tank_area = 2 * M_PI*tank_radius*tank_radius + 2 * M_PI*tank_radius*tank_height; // 2*pi*R^2 + 2*pi*r*h
		double UA_tank = tank_area * U_tank;
		double tank_cross_section = M_PI*tank_radius*tank_radius;
		double m_tank = rho_water*V_tank;

		/* pipe, and heat exchange properties */
		double Eff_hx = as_double("hx_eff"); // heat exchanger effectiveness (0..1)
		double pump_watts = as_double("pump_power"); // pump size in Watts
		double pump_eff = as_double("pump_eff"); // pumping efficiency

		// pipe U calculation (W/m2.C), http://en.wikipedia.org/wiki/Heat_transfer_coefficient (of pipe wall, h_wall = 2*k/(i*ln(o/i)) )
		double pipe_od = pipe_diam + pipe_insul * 2;
		double U_pipe = 2 * pipe_k / (pipe_od * ::log(pipe_od / pipe_diam)); //  **TODO** CHECK whether should be pipe_diam*log(pipe_od/pipe_diam) in denominator
		double UA_pipe = U_pipe * M_PI * pipe_od * pipe_length; // W/'C

		/* temperature properties */
		double T_room = as_double("T_room"); // ambient temperature in mechanical room or location of storage tank, hx, etc
		double T_tank_max = as_double("T_tank_max"); // max temp of water in storage tank
		double T_set = as_double("T_set"); // hot water set point temperature


		/* **********************************************************************
		Initialize data storage, read weather file, set draw profile
		********************************************************************** */

		weather_header hdr;
		wdprov->header( &hdr );

		weather_record wf;

		size_t nrec = wdprov->nrecords();
		size_t step_per_hour = nrec/8760;
		if ( step_per_hour < 1 || step_per_hour > 60 || step_per_hour*8760 != nrec )
			throw exec_error( "swh", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec ) );

		double ts_hour = 1.0/step_per_hour;
		double ts_sec = 3600.0/step_per_hour;

		ssc_number_t *Beam = allocate("beam", nrec);
		ssc_number_t *Diffuse = allocate("diffuse", nrec);
		ssc_number_t *T_amb = allocate("T_amb", nrec);
		ssc_number_t *T_mains = allocate("T_mains", nrec);
		ssc_number_t *out_Draw = allocate("draw", nrec);
		ssc_number_t *I_incident = allocate("I_incident", nrec);
		ssc_number_t *I_transmitted = allocate("I_transmitted", nrec);
		ssc_number_t *shading_loss = allocate("shading_loss", nrec);

		ssc_number_t *out_Q_transmitted = allocate("Q_transmitted", nrec);
		ssc_number_t *out_Q_useful = allocate("Q_useful", nrec);
		ssc_number_t *out_Q_deliv = allocate("Q_deliv", nrec);
		ssc_number_t *out_Q_loss = allocate("Q_loss", nrec);
		ssc_number_t *out_T_tank = allocate("T_tank", nrec);
		ssc_number_t *out_T_deliv = allocate("T_deliv", nrec);
		ssc_number_t *out_P_pump = allocate("P_pump", nrec);
		ssc_number_t *out_Q_aux = allocate("Q_aux", nrec);
		ssc_number_t *out_Q_auxonly = allocate("Q_auxonly", nrec);

		ssc_number_t *out_V_hot = allocate("V_hot", nrec);
		ssc_number_t *out_V_cold = allocate("V_cold", nrec);
		ssc_number_t *out_T_hot = allocate("T_hot", nrec);
		ssc_number_t *out_T_cold = allocate("T_cold", nrec);

		ssc_number_t *out_energy = allocate("gen", nrec);

		ssc_number_t *Mode = allocate("mode", nrec);

		double temp_sum = 0.0;
		size_t temp_count = 0;
		double monthly_avg_temp[12];
		size_t monthly_avg_count[12];
		for (size_t i = 0; i < 12; i++)
		{
			monthly_avg_temp[i] = 0.0;
			monthly_avg_count[i] = 0;
		}

		size_t idx=0;
		for (size_t hour = 0; hour<8760; hour++)
		{
			for( size_t jj=0;jj<step_per_hour;jj++)
			{
                if (!wdprov->read(&wf)) {
                    throw exec_error("swh", util::format("error reading from weather file at position %d", (int)idx));
                }

				Beam[idx] = (ssc_number_t)wf.dn;
				Diffuse[idx] = (ssc_number_t)wf.df;
				T_amb[idx] = (ssc_number_t)wf.tdry;
				T_mains[idx] = 0.;
				I_incident[idx] = 0;
				I_transmitted[idx] = 0;

				// accumulate for averaging
				temp_sum += T_amb[idx];
				temp_count++;

				int imonth = util::month_of((double)hour) - 1;
				monthly_avg_temp[ imonth ] += T_amb[idx];
				monthly_avg_count[ imonth ]++;

				/* **********************************************************************
				Process radiation (Isotropic model), calculate Incident[i] through cover
				********************************************************************** */



				irrad tt;
				if (irrad_mode == 0) tt.set_beam_diffuse(wf.dn, wf.df);
				else if (irrad_mode == 2) tt.set_global_diffuse(wf.gh, wf.df);
				else tt.set_global_beam(wf.gh, wf.dn);
				tt.set_location(hdr.lat, hdr.lon, hdr.tz);
                tt.set_optional(hdr.elev, wf.pres, wf.tdry);
				tt.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute,
					instantaneous ? IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET : ts_hour );
				tt.set_sky_model(sky_model /* isotropic=0, hdkr=1, perez=2 */, albedo );
				tt.set_surface(0, tilt, azimuth, 0, 0, 0, false, 0.0);
				tt.calc();

				double poa[3];
				tt.get_poa(&poa[0], &poa[1], &poa[2], 0, 0, 0);
				I_incident[idx] = (ssc_number_t)(poa[0] + poa[1] + poa[2]); // total PoA on surface

				double solalt = 0;
				double solazi = 0;
				tt.get_sun( &solazi, 0, &solalt, 0, 0, 0, 0, 0, 0, 0 );

				double aoi = 0;
				tt.get_angles(&aoi, 0, 0, 0, 0); // note: angles returned in degrees

				// -------------------------------------
				// calculate transmittance through cover
				double Kta_d = 0.0;
				double Kta_b = 0.0;
				double Kta_g = 0.0;

				// incidence angle modifier (IAM) for beam (D&B eqn 6.17.10 pp 297)
				if (aoi <= 60.0) Kta_b = 1 - iam*(1 / cos(aoi*M_PI / 180) - 1);
				else if (aoi > 60.0 && aoi <= 90.0)  Kta_b = (1 - iam)*(aoi - 90.0)*M_PI / 180;
				if (Kta_b < 0) Kta_b = 0;


				// effective incidence angle for sky diffuse radiation (D&B eqn 5.4.2 pp 215)
				double theta_eff_diffuse = 59.7*M_PI / 180 - 0.1388*tilt*M_PI / 180 + 0.001497*tilt*M_PI / 180 * tilt*M_PI / 180;
				double cos_theta_eff_diffuse = cos(theta_eff_diffuse);

				// incidence angle modifier (IAM) for diffuse (D&B eqn 6.17.10 pp 297)
				if (theta_eff_diffuse <= M_PI / 3.) Kta_d = 1 - iam*(1 / cos_theta_eff_diffuse - 1);
				else if (theta_eff_diffuse > M_PI / 3. && theta_eff_diffuse <= M_PI / .2) Kta_d = (1 - iam)*(theta_eff_diffuse - M_PI / 2.);
				if (Kta_d < 0) Kta_d = 0;


				// effective incidence angle modifier for ground reflected radiation (D&B eqn 5.4.1 pp 215)
				double theta_eff_ground = 90 * M_PI / 180 - 0.5788*tilt*M_PI / 180 + 0.002693*tilt*M_PI / 180 * tilt*M_PI / 180;
				double cos_theta_eff_ground = cos(theta_eff_ground);

				// incidence angle modifier (IAM) for ground reflected radiation (D&B eqn 6.17.10 pp 297)
				if (theta_eff_ground <= M_PI / 3) Kta_g = 1 - iam*(1 / cos_theta_eff_ground - 1);
				else if (theta_eff_ground > M_PI / 3 && theta_eff_ground <= M_PI / 2) Kta_g = (1 - iam)*(theta_eff_ground - M_PI / 2.);
				if (Kta_g < 0) Kta_g = 0;

				// sub hourly update
				double beam_loss_factor = 1.0;
				if (shad.fbeam(hour, wf.minute, solalt, solazi))
					beam_loss_factor = shad.beam_shade_factor();


				shading_loss[idx] = (ssc_number_t) (1-beam_loss_factor)*100;
                double shade_loss_factor = shad.fdiff();

				I_transmitted[idx] = (ssc_number_t)(
					Kta_b*poa[0]*beam_loss_factor +
					Kta_d*poa[1]*shade_loss_factor +
					Kta_g*poa[2]);

				idx++;
			}
		}

		// Compute Mains Temperature based on user input
		int use_custom_mains = as_integer("use_custom_mains");
		if (use_custom_mains)
		{
			size_t iidx=0;
			for (size_t hr = 0; hr < 8760; hr++)
				for( size_t jj=0;jj<step_per_hour;jj++ )
					T_mains[iidx++] = (ssc_number_t)(custom_mains[hr]);
		}
		else
		{

			// Algorithm for calculating mains water temperature from paper
			// ASES 2007 (J.Burch & C.Christensen)
			// "Towards Development of an Algorithm for Mains Water Temperature"
			// Verified against code in TRNSYS Type 15 Weather Reader
			double min_monthly_avg = 1e99;
			double max_monthly_avg = -1e99;
			for (size_t m = 0; m < 12; m++)
			{
				monthly_avg_temp[m] = monthly_avg_temp[m] / monthly_avg_count[m];
				if (monthly_avg_temp[m] < min_monthly_avg) min_monthly_avg = monthly_avg_temp[m];
				if (monthly_avg_temp[m] > max_monthly_avg) max_monthly_avg = monthly_avg_temp[m];
			}

			double avg_temp_high_f = 32. + 1.8 * max_monthly_avg; //F
			double avg_temp_low_f = 32. + 1.8 * min_monthly_avg; // F
			double annual_avg_temp = (temp_sum / temp_count) * 1.8 + 32.; // F

			double mains_ratio = 0.4 + 0.01*(annual_avg_temp - 44.); // F
			double lag = 35. - (annual_avg_temp - 44.); // F

			/* **********************************************************************
			Calculate hourly mains water temperature
			********************************************************************** */
			size_t iidx=0;
			double tmain = 0;
			for ( size_t i=0; i<8760; i++)
			{
				// calculate hour of day  ( goes 1..24 )
				// and julian day  ( goes 1..365 )

				// (Julian day is used in the Julian date (JD) system of time measurement for scientific use by
				// the astronomy community, presenting the interval of time in days and fractions of a day since
				// January 1, 4713 BC Greenwich noon - WIKIPEDIA)
				int julian_day = (int)(((double)(i + 1)) / 24);
				if ((double)julian_day != (((double)(i + 1)) / 24.0))
				{
					julian_day++;
				}
				if (wdprov->lat() > 0.)
				{
					tmain = (annual_avg_temp + 6. + mains_ratio * ((avg_temp_high_f - avg_temp_low_f) / 2.)
						* sin(M_PI / 180 * (0.986*(julian_day - 15 - lag) - 90.)));
				}
				else
				{
					tmain = (annual_avg_temp + 6. + mains_ratio * ((avg_temp_high_f - avg_temp_low_f) / 2.)
						* sin(M_PI / 180 * (0.986*(julian_day - 15 - lag) + 90.)));
				}
				tmain = ((tmain - 32) / 1.8); // convert to 'C

				// load into mains temp array
                for (size_t jj = 0; jj < step_per_hour; jj++) {
                    T_mains[iidx++] = (ssc_number_t)tmain;
                }
			}
		}

		/* **********************************************************************
			Determine set temperatures based on user input
			********************************************************************** */
		int use_custom_set = as_integer("use_custom_set");
		double T_set_array[8760];
		if (use_custom_set == 0)
		{
            for (size_t i = 0; i < 8760; i++) {
                T_set_array[i] = T_set;
            }
		}
		else
		{
            for (size_t i = 0; i < 8760; i++) {
                T_set_array[i] = custom_set[i];
            }
		}

		/* **********************************************************************
		Calculate additional SWH system parameters
		********************************************************************** */

		/* set initial conditions on some simulation variables */
		double T_hot_prev = T_mains[0] + 40.; // initial hot temp 40'C above ambient
		double T_cold_prev = T_mains[0];
		double Q_tankloss = 0;
		double Q_useful_prev = 0.0;
		double V_hot_prev = 0.8 * V_tank;
		double V_cold_prev = V_tank-V_hot_prev;
		double T_tank_prev = (V_hot_prev/V_tank)*T_hot_prev + (V_cold_prev/V_tank)*T_cold_prev; // weighted average tank temperature (initial)
		double T_deliv_prev = 0.0;
		double T_bot_prev = T_mains[0];

		/* *********************************************************************************************
		Calculate SHW performance: Q_useful, Q_deliv, T_deliv, T_tank, Q_pump, Q_aux, Q_auxonly, energy_net (Q_saved)
		*********************************************************************************************** */
		int mode = 0;
		double annual_kwh = 0.0;
		size_t hour = 0;
		idx = 0;
		for (hour = 0; hour < 8760; hour++)
		{
            #define NSTATUS_UPDATES 50  // set this to the number of times a progress update should be issued for the simulation
			if ( hour % (8760/NSTATUS_UPDATES) == 0 )
			{
				float percent = 100.0f * ((float)hour+1) / ((float)8760);
                if (!update("", percent, (float)hour)) {
                    throw exec_error("swh", "simulation canceled at hour " + util::to_string(hour + 1.0));
                }
			}

			for( size_t jj=0;jj<step_per_hour;jj++ )
			{
				// Note -> "use" in this case simply refers to one element of a vector, not to a modified value
				double I_incident_use = I_incident[idx];
				double T_amb_use = T_amb[idx];
				double T_mains_use = T_mains[idx];

				// hourly arrays
				double mdot_mix = draw[hour]*(1.0/3600.0);
				double T_set_use = T_set_array[hour];

				// at beginning of this timestep, temp values are the same as end of last timestep
				double T_tank = T_tank_prev;
				double Q_useful = Q_useful_prev;
				double T_deliv = T_deliv_prev;
				double V_hot = V_hot_prev;
				double V_cold = V_tank-V_hot;
				double T_hot = T_hot_prev;
				double T_cold = T_cold_prev;
				double T_bot = T_bot_prev;
				double T_top = T_hot_prev;

				double mdotCp_use = mdot_total * fluid_cp; // mass flow rate (kg/s) * Cp_fluid (J/kg.K)
				double mdotCp_test = test_flow * test_cp; // test flow (kg/s) * Cp_test

				/* Flow rate corrections to FRta, FRUL (D&B pp 307) */
				double FprimeUL = -mdotCp_test / area_coll * ::log( 1 - FRUL*area_coll/mdotCp_test ); // D&B eqn 6.20.4
				double r = ( mdotCp_use/area_total*(1-exp(-area_total*FprimeUL/mdotCp_use)) ) / FRUL; // D&B eqn 6.20.3
				double FRta_use = FRta * r; // FRta_use = value for this time step
				double FRUL_use = FRUL * r; // FRUL_use = value for this time step

                // IT LOOKS LIKE THE PIPING LOSSES ARE DOUBLE COUNTED HERE.
                // UA_pipe is for the whole system, while in these equations they should be split between inlet and outlet
				/* Pipe loss adjustment (D&B pp 430) */
				FRta_use = FRta_use / (1 + UA_pipe / mdotCp_use); // D&B eqn 10.3.9
				FRUL_use = FRUL_use * ((1 - UA_pipe / mdotCp_use + 2 * UA_pipe / (area_total*FRUL_use)) / (1 + UA_pipe / mdotCp_use)); // D&B eqn 10.3.10

				/* Heat exchanger adjustment (D&B pp 427) */
				double FR_ratio = 1/( 1 + (area_total*FRUL_use/mdotCp_use)*(mdotCp_use/(Eff_hx*mdotCp_use)-1)); // D&B eqn 10.2.3
				FRta_use = FRta_use * FR_ratio;
				FRUL_use = FRUL_use * FR_ratio;

				// Compute Q_useful
				if (Q_useful_prev > 0.)
				{
					Q_useful = area_total*( FRta_use*I_transmitted[idx] - FRUL_use*(T_bot - T_amb_use)); // D&B eqn 6.8.1
				}
				else Q_useful = area_total*(FRta_use*I_transmitted[idx] - FRUL_use*(T_tank_prev - T_amb_use) );
				// T_tank_prev is used, because use of T_cold_prev can cause the system to oscillate on and off

                if (I_incident_use < 0.0) {
                    Q_useful = 0;
                }

				double dT_collector = Q_useful/mdotCp_use;

	            // Charging -- solar system operating
				if (Q_useful > 0.)
				{
					double V_hot_next = V_hot_prev + (ts_sec*mdot_total/rho_water);
					if (V_hot_next < V_tank)
					{
		                // Mode 1 Transition -- solar system operating
						mode = 1;
						// Warm water from the collector loop into the top of the solar tank causes mixing with hot water below
						T_tank = (T_tank_prev*m_tank*Cp_water + ts_sec*(Q_useful + UA_tank*T_room
							+ mdot_mix*Cp_water*(T_mains_use-0.33*dT_collector)))/(m_tank*Cp_water + ts_sec*(UA_tank + mdot_mix*Cp_water));
						if (T_tank > T_tank_max) T_tank = T_tank_max;
						Q_tankloss = UA_tank * (T_tank - T_room);
						// uses UA_tank; ignores difference between losses from V_hot and V_cold during this transition period
						V_hot = V_hot_prev + ts_sec*mdot_total/rho_water;
						V_cold = V_tank - V_hot;
						T_hot = (T_hot_prev*V_hot_prev + ts_sec*(mdot_total/rho_water)*(T_cold_prev + dT_collector))/V_hot;
						T_cold = (V_tank/V_cold)*T_tank - (V_hot/V_cold)*T_hot;
						T_top = T_hot;
						T_bot = T_cold;
						T_deliv = T_top;
					}
					else
					{
		                // Mode 2 Charging -- solar system operating
						mode = 2;
						// Energy balance calculated based on average tank temperature (single node); tank top and bottom temperatures estimated based on collector dT
						// Implicit Euler calculation
						T_tank = (T_tank_prev*m_tank*Cp_water + ts_sec*(Q_useful + UA_tank*T_room
							+ mdot_mix*Cp_water*(T_mains_use-0.33*dT_collector)))/(m_tank*Cp_water + ts_sec*(UA_tank + mdot_mix*Cp_water));
						if (T_tank > T_tank_max) T_tank = T_tank_max;
						Q_tankloss = UA_tank * (T_tank - T_room);
						// Based on typical TRNSYS 100-node tank simulation results, the temperature difference asymmetry of Tbot and Ttop from Ttank (0.67 and 0.33 rather than 0.5 and 0.5)
						// accounts for the extra low temperature of the bottom node caused by incoming mains water
						T_top = T_tank + 0.33*dT_collector;
						T_bot = T_tank - 0.67*dT_collector;
						T_hot = T_top;
						T_cold = T_bot;
						T_deliv = T_top;
					}
				}
				else
				{
		            // Mode 3 Discharging -- solar system not operating
					mode = 3;
					// 2-node plug flow
					double hotLoss = 0.0; double coldLoss = 0.0;
					double A_cold = 0.0; double A_hot = 0.0;
					if (Q_useful_prev > 0.)
					// If previous timestep had solar collection, so assume whole tank is hot
					{
						V_hot_prev = V_tank;
						T_hot_prev = T_tank;
					}
					// Hot node calculations
					V_hot = V_hot_prev - mdot_mix*ts_sec/rho_water;
					if (V_hot < 0) {
						V_hot = 0;
                    }

					if (V_hot == 0)	// cold water drawn into the bottom of the tank in previous timesteps has completely flushed hot water from the tank
					{
						T_hot = T_hot_prev;
					}
					else
					{
						double h_hot = V_hot_prev/tank_cross_section;
						A_hot = tank_cross_section + 2*M_PI*tank_radius*h_hot;
						double m_hot = V_hot_prev*rho_water;
						T_hot = ((T_hot_prev * Cp_water * m_hot) + (ts_sec*U_tank*A_hot * T_room))/((m_hot*Cp_water) + (ts_sec*U_tank*A_hot)); // IMPLICIT NON-STEADY (Euler)
					}
					hotLoss = U_tank * A_hot * (T_hot - T_room);

					// Cold node calculations
					V_cold = V_tank-V_hot;
					if (V_cold_prev == 0 || V_cold == 0)
					{
						T_cold = T_cold_prev;
					}
					else
					{
						double h_cold = V_cold/tank_cross_section;
						A_cold = tank_cross_section + 2*M_PI*tank_radius*h_cold;
						double m_cold = rho_water*V_cold;
						T_cold = ((T_cold_prev*m_cold*Cp_water) + (ts_sec*U_tank*A_cold*T_room) + (ts_sec*mdot_mix*Cp_water*T_mains_use))
							/((m_cold*Cp_water) + (ts_sec*A_cold*U_tank) + (mdot_mix*ts_sec*Cp_water) ); // IMPLICIT NON-STEADY
					}
					coldLoss = U_tank*A_cold*(T_cold - T_room);

					Q_tankloss = hotLoss + coldLoss;
					T_tank = (V_hot / V_tank) * T_hot + (V_cold / V_tank) * T_cold;
					T_top = T_tank + 0.33*dT_collector;
					T_bot = T_tank - 0.67*dT_collector;
					// T_top = T_hot
					// T_bot = T_cold
                    if (V_hot > 0) {
                        T_deliv = T_hot;
                    }
                    else {
                        T_deliv = T_cold;
                    }
				}

				// calculate pumping losses (pump size is user entered) -
				double P_pump = (Q_useful > 0 && I_incident_use >= 0.0) ? pump_watts / pump_eff : 0.0;

				// compute energy delivered
				double Q_deliv = mdot_mix* Cp_water *(T_deliv - T_mains_use);

				// amount of auxiliary energy needed to bring delivered water to set temperature
				double Q_aux = mdot_mix * Cp_water * (T_set_use - T_deliv);
                if (Q_aux < 0) {
                    Q_aux = 0.0;
                }

				// amount of energy needed to bring T_mains to set temperature (without SHW)
				double Q_auxonly = mdot_mix * Cp_water * (T_set_use - T_mains_use);

                if (Q_auxonly < 0) {
                    Q_auxonly = 0.0;
                }

				// Energy saved by SHW system is difference between aux only system and shw+aux system - the pump losses
				double Q_saved = Q_auxonly - Q_aux - P_pump;

				// save some values for next timestep
				Q_useful_prev = Q_useful;
				T_tank_prev = T_tank;
				V_hot_prev = V_hot;
				V_cold_prev = V_tank-V_hot;
				T_deliv_prev = T_deliv;
				T_hot_prev = T_hot;
				T_cold_prev = T_cold;
				T_bot_prev = T_bot;

				// Zero out Q_useful if <0
                if (Q_useful < 0) {
                    Q_useful = 0.0;
                }

				// save output variables - convert Q values to kWh
				out_Q_transmitted[idx] = (ssc_number_t)(I_transmitted[idx] * area_total * watt_to_kw);
				out_Q_useful[idx] = (ssc_number_t)(Q_useful* watt_to_kw);
				out_Q_deliv[idx] = (ssc_number_t)(Q_deliv* watt_to_kw); //this is currently being output from a financial model as "Hourly Energy Delivered", they are equivalent
				out_Q_loss[idx] = (ssc_number_t)(Q_tankloss* watt_to_kw);
				out_T_tank[idx] = (ssc_number_t)T_tank;
				out_T_deliv[idx] = (ssc_number_t)T_deliv;
				out_P_pump[idx] = (ssc_number_t)(P_pump* watt_to_kw);
				out_Q_aux[idx] = (ssc_number_t)(Q_aux* watt_to_kw);
				out_Q_auxonly[idx] = (ssc_number_t)(Q_auxonly* watt_to_kw);
				out_T_hot[idx] = (ssc_number_t)T_hot;
				out_T_cold[idx] = (ssc_number_t)T_cold;
				out_V_hot[idx] = (ssc_number_t)V_hot;
				out_V_cold[idx] = (ssc_number_t)V_cold;
				out_Draw[idx] = draw[hour]; // pass to outputs for visualization
				Mode[idx] = (ssc_number_t)mode; // save mode for debugging

				out_energy[idx] =  (ssc_number_t)( Q_saved * ts_hour * haf(hour) * watt_to_kw); // kWh energy, with adjustment factors applied

				// accumulate hourly and annual energy
				annual_kwh += out_energy[idx];
				idx++;
			}
		}

		// if an electric load exists, the amount of energy saved cannot exceed it, since can't export savings
		if (is_assigned("load")) {
			std::vector<ssc_number_t> load_year_one, load_lifetime;
			load_year_one = as_vector_ssc_number_t("load");
            size_t analysis_period = 1;
            std::vector<double> scaleFactors(analysis_period, 1.0);
			size_t n_rec_single_year = 0;
			double dt_hour_gen = 0.0;
            double interpolation_factor = 1.0;
			single_year_to_lifetime_interpolated<ssc_number_t>(false, analysis_period, (size_t)wdprov->nrecords(),
				load_year_one, scaleFactors, interpolation_factor, load_lifetime, n_rec_single_year, dt_hour_gen);

			for (size_t i = 0; i < load_lifetime.size(); i++) {
				if (out_energy[i] > load_lifetime[i]) {
					out_energy[i] = load_lifetime[i];
				}
			}
		}

		accumulate_monthly( "Q_deliv", "monthly_Q_deliv", ts_hour );
		accumulate_monthly( "Q_aux", "monthly_Q_aux", ts_hour );
		accumulate_monthly( "Q_auxonly", "monthly_Q_auxonly", ts_hour );
		accumulate_monthly("gen", "monthly_energy");

		accumulate_annual( "Q_deliv", "annual_Q_deliv", ts_hour );
		accumulate_annual( "Q_aux", "annual_Q_aux", ts_hour );
		double auxonly = accumulate_annual( "Q_auxonly", "annual_Q_auxonly", ts_hour );
		double deliv = accumulate_annual("gen", "annual_energy");

		assign("solar_fraction", var_data( (ssc_number_t)(deliv/auxonly) ));

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		kWhperkW = annual_kwh / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
	}

};


DEFINE_MODULE_ENTRY( swh, "Solar water heating model using multi-mode tank node model.", 10 )
