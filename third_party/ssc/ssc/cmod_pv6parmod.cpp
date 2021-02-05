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

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

#include "core.h"

#include "lib_cec6par.h"
#include "lib_irradproc.h"

static var_info _cm_vtab_pv6parmod[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,       "poa_beam",                   "Incident direct normal radiation","W/m2",  "",                      "Weather",      "*",                       "",                        "" },
	{ SSC_INPUT,        SSC_ARRAY,       "poa_skydiff",                "Incident sky diffuse radiation",  "W/m2",  "",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "poa_gnddiff",                "Incident ground diffuse irradiance","W/m2","",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "tdry",                       "Dry bulb temperature",           "'C",     "",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wspd",                       "Wind speed",                     "m/s",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wdir",                       "Wind direction",                 "deg",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sun_zen",                    "Sun zenith angle",               "deg",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "incidence",                  "Incidence angle to surface",     "deg",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "surf_tilt",                  "Surface tilt angle",             "deg",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=poa_beam",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "elev",                       "Site elevation",                 "m",      "",                    "Weather",      "*",                        "",                      "" },
	

	{ SSC_INPUT,        SSC_ARRAY,       "opvoltage",               "Module operating voltage",       "Volt",    "",                     "CEC 6 Parameter PV Module Model",      "?"                        "",              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "area",                    "Module area",                    "m2",      "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Vmp",                     "Maximum power point voltage",    "V",       "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Imp",                     "Maximum power point current",    "A",       "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Voc",                     "Open circuit voltage",           "V",       "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Isc",                     "Short circuit current",          "A",       "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "alpha_isc",               "Temp coeff of current at SC",    "A/'C",    "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "beta_voc",                "Temp coeff of voltage at OC",    "V/'C",    "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gamma_pmp",               "Temp coeff of power at MP",      "%/'C",    "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tnoct",                   "NOCT cell temperature",          "'C",      "",                     "CEC 6 Parameter PV Module Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "a",                       "Modified nonideality factor",    "1/V",    "",                      "CEC 6 Parameter PV Module Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Il",                      "Light current",                  "A",      "",                      "CEC 6 Parameter PV Module Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Io",                      "Saturation current",             "A",      "",                      "CEC 6 Parameter PV Module Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rs",                      "Series resistance",              "ohm",    "",                      "CEC 6 Parameter PV Module Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rsh",                     "Shunt resistance",               "ohm",    "",                      "CEC 6 Parameter PV Module Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Adj",                     "OC SC temp coeff adjustment",    "%",      "",                      "CEC 6 Parameter PV Module Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "standoff",                "Mounting standoff option",       "0..6",   "0=bipv, 1= >3.5in, 2=2.5-3.5in, 3=1.5-2.5in, 4=0.5-1.5in, 5= <0.5in, 6=ground/rack",   "CEC 6 Parameter PV Module Model",      "?=6",     "INTEGER,MIN=0,MAX=6",     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "height",                  "System installation height",     "0/1",    "0=less than 22ft, 1=more than 22ft",                                                   "CEC 6 Parameter PV Module Model",      "?=0",     "INTEGER,MIN=0,MAX=1",     "" },

	
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                      "Cell temperature",               "'C",     "",                   "CEC 6 Parameter PV Module Model",      "*",                       "LENGTH_EQUAL=poa_beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc_voltage",                 "DC module voltage",              "Volt",   "",                   "CEC 6 Parameter PV Module Model",      "*",                       "LENGTH_EQUAL=poa_beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc_current",                 "DC module current",              "Ampere", "",                   "CEC 6 Parameter PV Module Model",      "*",                       "LENGTH_EQUAL=poa_beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eff",                        "Conversion efficiency",          "0..1",   "",                   "CEC 6 Parameter PV Module Model",      "*",                       "LENGTH_EQUAL=poa_beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                         "DC power output",                "Watt",   "",                   "CEC 6 Parameter PV Module Model",      "*",                       "LENGTH_EQUAL=poa_beam",                          "" },

var_info_invalid };

class cm_pv6parmod : public compute_module
{
private:
public:
	cm_pv6parmod()
	{
		add_var_info( _cm_vtab_pv6parmod );
	}

	void exec( )
	{
		size_t arr_len;
		ssc_number_t *p_poabeam = as_array( "poa_beam", &arr_len );
		ssc_number_t *p_poaskydiff = as_array( "poa_skydiff", &arr_len );
		ssc_number_t *p_poagnddiff = as_array( "poa_gnddiff", &arr_len );
		ssc_number_t *p_tdry = as_array( "tdry", &arr_len );
		ssc_number_t *p_wspd = as_array( "wspd", &arr_len );
		ssc_number_t *p_wdir = as_array( "wdir", &arr_len );
		ssc_number_t *p_inc = as_array( "incidence", &arr_len );
		ssc_number_t *p_zen = as_array( "sun_zen", &arr_len );
		ssc_number_t *p_stilt = as_array( "surf_tilt", &arr_len );
		double site_elevation = as_double("elev");

		cec6par_module_t mod;
		mod.Area = as_double("area");
		mod.Vmp = as_double("Vmp");
		mod.Imp = as_double("Imp");
		mod.Voc = as_double("Voc");
		mod.Isc = as_double("Isc");
		mod.alpha_isc = as_double("alpha_isc");
		mod.beta_voc = as_double("beta_voc");
		mod.a = as_double("a");
		mod.Il = as_double("Il");
		mod.Io = as_double("Io");
		mod.Rs = as_double("Rs");
		mod.Rsh = as_double("Rsh");
		mod.Adj = as_double("Adj");

		noct_celltemp_t tc;
		tc.Tnoct = as_double("tnoct");

		int standoff = as_integer("standoff");
		tc.standoff_tnoct_adj = 0;
		switch(standoff)
		{ //source for standoff adjustment constants: https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/1985/850330.pdf page 12
		case 2: tc.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
		case 3: tc.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
		case 4: tc.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
		case 5: tc.standoff_tnoct_adj = 18; break; // less than 0.5 inches
			// note: all others, standoff_tnoct_adj = 0;
		}

		int height = as_integer("height");
		tc.ffv_wind = 0.51;
		if ( height == 1 )
			tc.ffv_wind = 0.61;

		ssc_number_t *opvoltage = 0;
		if ( is_assigned("opvoltage") )
		{
			size_t opvlen = 0;
			opvoltage = as_array( "opvoltage", &opvlen );
			if ( opvlen != arr_len )
				throw general_error("operating voltage array must be same length as input vectors");
		}

		ssc_number_t *p_tcell = allocate("tcell", arr_len);
		ssc_number_t *p_volt = allocate("dc_voltage", arr_len);
		ssc_number_t *p_amp = allocate("dc_current", arr_len);
		ssc_number_t *p_eff = allocate("eff", arr_len);
		ssc_number_t *p_dc = allocate("dc", arr_len);

		for (size_t i = 0; i < arr_len; i++ )
		{
			pvinput_t in;
			in.Ibeam = (double) p_poabeam[i];
			in.Idiff = (double) p_poaskydiff[i];
			in.Ignd = (double) p_poagnddiff[i];
			in.Tdry = (double) p_tdry[i];
			in.Wspd = (double) p_wspd[i];
			in.Wdir = (double) p_wdir[i];
			in.Zenith = (double) p_zen[i];
			in.IncAng = (double) p_inc[i];
			in.Elev = site_elevation;
			in.Tilt = (double) p_stilt[i];

			pvoutput_t out;

			double opv = -1; // by default, calculate MPPT
			if ( opvoltage != 0 )
				opv = opvoltage[i];

			double tcell = in.Tdry;
			if (! tc( in, mod, opv, tcell ) ) throw general_error("error calculating cell temperature", (float)i);
			if (! mod( in, tcell, opv, out ) ) throw general_error( "error calculating module power and temperature with given parameters", (float) i);

			p_tcell[i] = (ssc_number_t)out.CellTemp;
			p_volt[i] = (ssc_number_t)out.Voltage;
			p_amp[i] = (ssc_number_t)out.Current;
			p_eff[i] = (ssc_number_t)out.Efficiency;
			p_dc[i] = (ssc_number_t)out.Power;
		}
	}
};

DEFINE_MODULE_ENTRY( pv6parmod, "CEC 6 Parameter PV module model performance calculator.  Does not include weather file reading or irradiance processing, or inverter (DC to AC) modeling.", 1 )
