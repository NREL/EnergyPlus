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

enum {	I_POA,
		I_TINLET,
		I_AREA,
		I_CP,
		I_FR,
		I_UL,
		I_TAMB,
		I_MDOT,

		O_TCOLLREF,
		O_TOUTLET,

		N_MAX };

tcsvarinfo solarcollector_variables[] = {

	{ TCS_INPUT,   TCS_NUMBER,   I_POA,      "poa",         "Incident solar irradiance on surface", "W/m2",        "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_TINLET,   "tinlet",      "Inlet fluid temperature",              "'C",          "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_AREA,     "area",        "Collector area",                       "m2",          "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_CP,       "cp",          "Heat capacity of fluid",               "J/kg.K",      "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_FR,       "fr",          "Constant efficiency factor",           "",            "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_UL,       "ul",          "Constant loss coefficient",            "W/K",         "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_TAMB,     "tamb",        "Ambient temperature",                  "'C",          "",      "",     "" },
	{ TCS_INPUT,   TCS_NUMBER,   I_MDOT,     "mdot",        "Mass flow rate of fluid",              "kg/s",        "",      "",     "" },
	
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TCOLLREF,  "tcollref",       "Outlet temp at design flow rate",  "'C",          "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TOUTLET,  "toutlet",     "Outlet fluid temperature",             "'C",          "",      "",     "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class solarcollector : public tcstypeinterface
{
private:
public:
	solarcollector( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
	}

	virtual ~solarcollector()
	{
	}

	virtual int init()
	{
		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{
		double Ti = value( I_TINLET );
		double A = value( I_AREA );
		double Cp = value( I_CP );
		double Fr = value( I_FR );
		double UL = value( I_UL );
		double S = value( I_POA );
		double Ta = value( I_TAMB );
		double mdot = value ( I_MDOT );

		double To = Ti;
		if ( mdot*Cp != 0 )
			To = Ti + A*Fr/( mdot*Cp ) * ( S - UL*(Ti-Ta) );

		value( O_TOUTLET, To );
		value( O_TCOLLREF, Ti + A*Fr/( 0.19*4186 ) * ( S-UL*(Ti-Ta) ) );

		return 0;
	}
};


TCS_IMPLEMENT_TYPE( solarcollector, "Basic solar thermal collector", "Aron Dobos", 1, solarcollector_variables, NULL, 0 )
