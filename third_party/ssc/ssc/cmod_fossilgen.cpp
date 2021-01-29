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

static var_info _cm_vtab_fossilgen[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "nameplate",                  "Nameplate generation capacity", "kW",     "",                      "Fossil",        "*",                       "POSITIVE",                      "" },		
	{ SSC_INPUT,        SSC_NUMBER,      "capacity_factor",            "Capacity factor",               "%",      "",                      "Fossil",        "*",                       "MIN=0,MAX=100",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "System derate",                 "frac",   "",                      "Fossil",        "*",                       "MIN=0,MAX=1",                   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "conv_eff",                   "Conversion efficiency",         "%",      "",                      "Fossil",        "*",                       "MIN=0,MAX=100",                 "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "e_net",                      "AC Generation",                 "kWh",    "",                      "Fossil",        "*",                       "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "fuel_usage",                 "Annual fuel usage",             "kWht",   "",                      "Fossil",        "*",                       "",                              "" },

var_info_invalid };

class cm_fossilgen : public compute_module
{
public:
	cm_fossilgen()
	{
		add_var_info( _cm_vtab_fossilgen );
	}

	void exec( )
	{

		ssc_number_t output = 8760*as_number("nameplate")
			* as_number("capacity_factor") / 100
			* (1 - as_number("derate")/100);

		ssc_number_t *e = allocate("e_net", 8760);

		// assume constant generation in each hour of the year
		for (size_t i=0;i<8760;i++)	e[i] = output/8760;

	
		assign( "fuel_usage", 
			var_data(output * 100 / as_number("conv_eff")) );
	}
};

DEFINE_MODULE_ENTRY( fossilgen, "Generic fossil fuel generator - capacity factor based approach", 1 )
