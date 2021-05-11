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
#include "lib_snowmodel.h"
#include <iostream>
#include <cmath>
#include <string>

/**********************************************************************************
************************************************************************************
**
**	11 March 2015
**
**	Implementation of Bill Marion's snow model [1] to C++ for use in SAM
**	Original author: David Severin Ryberg
**
**
**********************************************************************************
**  References:
**  1: Marion, Bill, et al. "Measured and modeled photovoltaic system 
**     energy losses from snow for Colorado and Wisconsin locations."
**     Solar Energy 97 (2013): 112-121.
**
**********************************************************************************
***********************************************************************************/


static var_info _cm_vtab_snowmodel[] = 
{	
/*   VARTYPE           DATATYPE         NAME						LABEL						UNITS               META                GROUP            REQUIRED_IF    CONSTRAINTS                     UI_HINTS*/
	//{ SSC_INPUT,        SSC_ARRAY,      "subarray1_poa_eff_beam",   "Plane of Array Incidence", "W/m^2",			"",					"PV Snow Model", "*",           "LENGTH=8760",            "" },
	{ SSC_INPUT,        SSC_ARRAY,      "subarray1_poa_shaded",   "Plane of Array Incidence",	"W/m^2",			"",					"PV Snow Model", "*",           "LENGTH=8760",            "" },
	{ SSC_INPUT,        SSC_ARRAY,      "wspd",						"Wind Speed",				"m/s",				"",					"PV Snow Model", "*",           "LENGTH=8760",            "" },
	{ SSC_INPUT,        SSC_ARRAY,      "hourly_gen",			"Hourly Energy",			"kwh",				"",					"Time Series",		"*",           "LENGTH=8760",            "" },
	{ SSC_INPUT,        SSC_ARRAY,      "tdry",						"Ambient Temperature",		"Degrees Celsius",	"",					"PV Snow Model", "*",           "LENGTH=8760",            "" },
	{ SSC_INPUT,        SSC_ARRAY,      "subarray1_surf_tilt",		"Surface Tilt",				"Degrees",			"",					"PV Snow Model", "*",           "LENGTH=8760",            "" },
	{ SSC_INPUT,		SSC_ARRAY,		"sunup",					"Sun up over horizon",		"0/1",				"",					"Time Series",		"*",		"",							"" },
	{ SSC_INPUT,        SSC_ARRAY,      "snowdepth",				"Snow Depth",				"cm",				"",					"PV Snow Model", "*",           "LENGTH=8760",            "" },
	{ SSC_INPUT,        SSC_NUMBER,     "subarray1_nmody",			"Number of Modules in a Row","",				"",					"PV Snow Model", "*",           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "subarray1_tilt",			"Base tilt",				"Degrees",			"",					"PV Snow Model", "*",           "",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,     "subarray1_track_mode",		"Tracking Mode",			"",					"",					"PV Snow Model", "*",           "",                             "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,      "hourly_energy_before_snow","Hourly Energy Without Snow Loss","kwh",		"",                 "Time Series",		"*",           "",			"" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "monthly_energy_before_snow","Monthly Energy Without Snow Loss","kwh",		"",                 "Monthly",			"*",           "",			"" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "annual_energy_before_snow","Annual Energy Without Snow Losses","kwh",		"",                 "Annual",			"*",           "",								"" },
	//{ SSC_OUTPUT,       SSC_ARRAY,      "snow_loss",				"PV loss due to snow",		"",					"",                 "PV Snow Model", "*",           "",			"" },
	//{ SSC_OUTPUT,       SSC_ARRAY,      "snow_fall_flag",			"Snow Fall Flag",			"",					"",                 "PV Snow Model", "*",           "",			"" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "hourly_gen",			"Hourly Energy",			"kwh",				"",                 "Time Series",		"*",           "",			"" },
	{ SSC_OUTPUT,       SSC_ARRAY,      "monthly_energy",			"Monthly Energy",			"kwh",				"",                 "Monthly",			"*",           "",			"" },
	{ SSC_OUTPUT,       SSC_NUMBER,     "annual_energy",			"Annual Energy",			"kwh",				"",                 "Annual",			"*",           "",								"" },
	//{ SSC_OUTPUT,       SSC_ARRAY,      "smout",			"Hourly Energy",			"kwh",				"",                 "Time Series",		"*",           "",			"" },


var_info_invalid };

class cm_snowmodel : public compute_module 
{
private:

public:
	cm_snowmodel()
	{ 
		add_var_info( _cm_vtab_snowmodel );
	}

	void exec( )
	{
		//int N = 0;
		size_t num_steps;

		// Define Input Arrays and variables
		//ssc_number_t *poa  = as_array( "subarray1_poa_eff_beam", &num_steps );	// Plane of array Irradiance
		ssc_number_t *poa  = as_array( "subarray1_poa_shaded", &num_steps );	// Plane of array Irradiance
		ssc_number_t *wSpd = as_array( "wspd", &num_steps );					// Wind Speed
		ssc_number_t *hrEn = as_array( "hourly_gen", &num_steps );			// Hourly Energy
		ssc_number_t *tAmb = as_array( "tdry", &num_steps );					// Ambient Temperature
		ssc_number_t *tilt = as_array( "subarray1_surf_tilt", &num_steps );		// Surface Tilt
		ssc_number_t *sDep = as_array( "snowdepth", &num_steps );				// Snow Depth
		ssc_number_t *sunup = as_array( "sunup", &num_steps );					// Sun up flag
		int nmody = as_integer("subarray1_nmody");								// The number of modules in a row
		int baseTilt = as_integer("subarray1_tilt");							// The tilt for static systems
		//int trackMode = as_integer("subarray1_track_mode");						// The systems tracking mode (0 -> static, 1 -> 1 axis tracking)

		// Define output arrays and variables
		ssc_number_t *hrEn_b4Snow = allocate( "hourly_energy_before_snow", num_steps);	// Hourly Energy with Snow Modeld

		/* NOTE: All input arrays must have a length of 8760 */
		pvsnowmodel snowModule;
		if (!snowModule.setup(nmody, (float)baseTilt)){
			if (snowModule.good) log(snowModule.msg, SSC_WARNING);
			else{
				log(snowModule.msg, SSC_ERROR);
				return;
			}	
		}

		float loss; 

		for (int i = 0; i < 8760; i++){
			if (!snowModule.getLoss(poa[i], tilt[i], wSpd[i], tAmb[i], sDep[i], (int)sunup[i], 1.0, loss)){
				if (snowModule.good) log(snowModule.msg, SSC_WARNING);
				else{
					log(snowModule.msg, SSC_ERROR);
					return;
				}
			}

			hrEn_b4Snow[i] = hrEn[i]; 
			hrEn[i] = hrEn[i] * (1 - loss);

		}

		// accumulate monthly and annual values

		accumulate_annual("hourly_energy_before_snow", "annual_energy_before_snow");
		accumulate_monthly("hourly_energy_before_snow", "monthly_energy_before_snow");
		accumulate_annual("hourly_gen", "annual_energy");
		accumulate_monthly("hourly_gen", "monthly_energy");

		/*ssc_number_t *smOut = allocate( "smout", num_steps);	// Hourly Energy with Snow Modeld
		for (int i = 0; i < 8760; i++){
			smOut[i] = snowModule.coverage[i];
		}*/

		return;
	}
};

DEFINE_MODULE_ENTRY( snowmodel, "Estimates the Detrimental Effects due to Snow Fall", 1 )
