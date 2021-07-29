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

#include "lib_pvwatts.h"
#include "lib_irradproc.h"

static var_info _cm_vtab_pvwatts[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,       "beam",				       "Direct normal radiation",         "W/m2",  "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "poa_beam",                   "Incident direct normal radiation","W/m2",  "",                      "Weather",      "*",                       "LENGTH_EQUAL=beam",                        "" },
	{ SSC_INPUT,        SSC_ARRAY,       "poa_skydiff",                "Incident sky diffuse radiation",  "W/m2",  "",                      "Weather",      "*",                       "LENGTH_EQUAL=beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "poa_gnddiff",                "Incident ground diffuse irradiance","W/m2","",                      "Weather",      "*",                       "LENGTH_EQUAL=beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "tdry",                       "Dry bulb temperature",           "'C",     "",                      "Weather",      "*",                       "LENGTH_EQUAL=beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wspd",                       "Wind speed",                     "m/s",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=beam",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "incidence",                  "Incidence angle to surface",     "deg",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=beam",                    "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "step",                       "Time step of input data",        "sec",    "",                       "PVWatts",     "?=3600",                       "POSITIVE",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                "Nameplate capacity",             "kW",     "",                      "PVWatts",      "*",                       "MIN=0.5,MAX=100000",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "System derate value",            "frac",   "",                      "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inoct",                     "Nominal operating cell temperature", "'C", "",                      "PVWatts",      "?=45.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "t_ref",                      "Reference cell temperature",     "'C",     "",                      "PVWatts",      "?=25.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gamma",                      "Max power temperature coefficient", "%/'C", "",                     "PVWatts",      "?=-0.5",                  "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_eff",                    "Inverter efficiency at rated power", "frac", "",                    "PVWatts",      "?=0.92",                  "MIN=0,MAX=1",                              "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                      "Cell temperature",               "'C",     "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                         "DC array output",                "kWhdc",  "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                         "AC system output",               "kWhac",  "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=beam",                          "" },

var_info_invalid };

class cm_pvwattsv1_poa : public compute_module
{
private:
public:
	cm_pvwattsv1_poa()
	{
		add_var_info( _cm_vtab_pvwatts );
	}

	void exec( )
	{
		size_t arr_len;
		ssc_number_t *p_beam = as_array( "beam", &arr_len );
		ssc_number_t *p_poabeam = as_array( "poa_beam", &arr_len );
		ssc_number_t *p_poaskydiff = as_array( "poa_skydiff", &arr_len );
		ssc_number_t *p_poagnddiff = as_array( "poa_gnddiff", &arr_len );
		ssc_number_t *p_tdry = as_array( "tdry", &arr_len );
		ssc_number_t *p_wspd = as_array( "wspd", &arr_len );
		ssc_number_t *p_inc = as_array( "incidence", &arr_len );
		
		double watt_spec = 1000.0 * as_double("system_size");
		double derate = as_double("derate");

		double tstephr = as_double("step")/3600.0; // get timestep of data in hours

		ssc_number_t *p_tcell = allocate("tcell", arr_len);
		ssc_number_t *p_dc = allocate("dc", arr_len);
		ssc_number_t *p_ac = allocate("ac", arr_len);

		/* PV RELATED SPECIFICATIONS */

		// note: these are normally hard-wired PVwatts constants, but made 
		// available for this most "advanced" of pvwatts implementations...  (5 aug 2011, apd)
		double inoct = as_double("inoct") + 273.15; // PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
		double reftem = as_double("t_ref"); // PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
		double pwrdgr = as_double("gamma") / 100.0; // PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
		double efffp = as_double("inv_eff"); // PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */

		double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
		double tmloss = 1.0 - derate/efffp;  /* All losses except inverter,decimal */

		pvwatts_celltemp Tccalc(inoct, height, tstephr);

		for (size_t i = 0; i < arr_len; i++ )
		{
			double poa = p_poabeam[i] + p_poaskydiff[i] + p_poagnddiff[i];
			if ( poa > 0 )
			{
				double tpoa = 0;
				if (p_beam[i] > 0)	
					tpoa = transpoa( poa, p_beam[i], p_inc[i]*M_PI/180.0, false );  /* have valid poa and dn, calculate transmitted through glass cover */
				else
					tpoa = poa; /* default to dn 0 or bad value - assume no glass cover on module */
				
				double tcell = Tccalc( poa, p_wspd[i], p_tdry[i] );
				double dc = dcpowr( reftem, watt_spec, pwrdgr, tmloss, tpoa, tcell, 1000.0 );
				double ac = dctoac( watt_spec, efffp, dc );

				p_tcell[i] = (ssc_number_t)tcell;
				p_dc[i] = (ssc_number_t)dc;
				p_ac[i] = (ssc_number_t)ac;
			}
			else
			{
				/* night time */
				p_tcell[i] = p_tdry[i];
				p_dc[i] = (ssc_number_t)0.0;
				p_ac[i] = (ssc_number_t)0.0;
			}
		}
	}
};

DEFINE_MODULE_ENTRY( pvwattsv1_poa, "PVWatts system performance calculator.  Does not include weather file reading or irradiance processing - user must supply arrays of precalculated POA irradiance data.", 1 )
