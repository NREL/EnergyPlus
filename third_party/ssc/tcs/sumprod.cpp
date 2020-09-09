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

enum {
	I_A,
	I_B,

	O_S,
	O_P,

	N_MAX };

tcsvarinfo sumprod_variables[] = {
	// vartype    datatype    index   name     label    units   meta   group   default_value
	{ TCS_INPUT,  TCS_NUMBER, I_A,    "a",     "Data 1",   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_B,    "b",     "Data 2",   "",      "",      "",     "" },

	{ TCS_OUTPUT, TCS_NUMBER, O_S,    "sum",      "Result of A+B",    "",     "",      "",     "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_P,    "product",  "Result of A*B",    "",     "",      "",     "" },
	
	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};

class sumprod : public tcstypeinterface
{
private:
public:
	sumprod( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
	}

	virtual ~sumprod()
	{
		// free any memory
	}

	virtual int init()
	{
		return 0;
	}
	
	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{
		double a = value( I_A );
		double b = value( I_B );
		
		if ( a < -999 )
		{
			message( TCS_ERROR, "invalid value for a: %lg", a);
			return -1;
		}

		value( O_S, a+b );
		value( O_P, a*b );

		return 0;
	}
};

TCS_IMPLEMENT_TYPE( sumprod, "Sums and Products", "Aron", 123, sumprod_variables, NULL, 0 )