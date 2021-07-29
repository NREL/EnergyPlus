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

static var_info vtab_iph_to_lcoefcr[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                             UNITS     META      GROUP          REQUIRED_IF    CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,       SSC_NUMBER,      "annual_electricity_consumption",  "Annual electricity consumptoin w/ avail derate",            "kWe-hr", "",   "IPH LCOH",     "*",       "",   "" },
	{ SSC_INPUT,       SSC_NUMBER,      "electricity_rate",                "Cost of electricity used to operate pumps/trackers",        "$/kWe",  "",   "IPH LCOH",     "*",       "",   "" },

	{ SSC_INOUT,       SSC_NUMBER,      "fixed_operating_cost",     "Annual fixed operating cost",    "$/kW",   "",       "Simple LCOE", "*",           "",         "" },

var_info_invalid };

class cm_iph_to_lcoefcr : public compute_module
{
private:
public:
	
	cm_iph_to_lcoefcr()
	{
		add_var_info( vtab_iph_to_lcoefcr );
	}

	void exec( )
	{
		ssc_number_t foc = as_number("fixed_operating_cost");		//[$]
		
		assign("fixed_operating_cost", foc + as_number("electricity_rate")*as_number("annual_electricity_consumption"));		
	}
	
};

DEFINE_MODULE_ENTRY( iph_to_lcoefcr, "Convert annual energy to kWt-hr and adjust fixed cost to include electric parasitic costs.", 1 )
