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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"

using namespace std;

enum{
	I_W_CYCLE_GROSS,
	I_W_PAR_SF_TOT,
	I_W_PAR_COOLING,

	O_W_NET,

	//Include N_max
	N_MAX
};

tcsvarinfo sam_mw_lf_type261_Wnet_variables[] = {
	{ TCS_INPUT,          TCS_NUMBER,     I_W_CYCLE_GROSS,          "W_cycle_gross",                                            "Electrical source - Power cycle gross output",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,     I_W_PAR_SF_TOT,           "W_par_sf_tot",                                             "Total solar field parasitics"                            "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,     I_W_PAR_COOLING,          "W_par_cooling",                                            "Parasitics from power cycle cooling",                    "MW",             "",             "",             "" },

	{ TCS_OUTPUT,         TCS_NUMBER,     O_W_NET,                  "W_net",												    "Net electricity generation (or usage) by the plant",           "MW",             "",             "",             "" },
	
	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};

class sam_mw_lf_type261_Wnet : public tcstypeinterface
{
private:

public:

	sam_mw_lf_type261_Wnet(tcscontext *cxt, tcstypeinfo *ti)
		: tcstypeinterface(cxt, ti)
	{
		
	}

	virtual ~sam_mw_lf_type261_Wnet()
	{
	}

	virtual int init()
	{		
		// --Initialization call-- 
		return 0;
	}

	virtual int call(double /*time*/, double /*step*/, int /*ncall*/)
	{

		double W_dot_pb_gross = value( I_W_CYCLE_GROSS );
		double W_dot_sf_par = value( I_W_PAR_SF_TOT );
		double W_dot_cooling_par = value( I_W_PAR_COOLING );

		value( O_W_NET, W_dot_pb_gross - W_dot_sf_par - W_dot_cooling_par );
		
		return 0;
	}

	virtual int converged(double /*time*/)
	{

		return 0;
	}
	
	
};


TCS_IMPLEMENT_TYPE( sam_mw_lf_type261_Wnet, "Net electricity calculator for the Physical Trough", "Mike Wagner", 1, sam_mw_lf_type261_Wnet_variables, NULL, 1 );