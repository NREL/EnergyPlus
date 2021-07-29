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
#include <cmath>
#include <limits>
#include <stddef.h>
#include "lib_pvinv.h"



partload_inverter_t::partload_inverter_t( )
{
	Paco = Pdco = Pntare = std::numeric_limits<double>::quiet_NaN();
}

bool partload_inverter_t::acpower(
	/* inputs */
	double Pdc,     /* Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/

								 /* outputs */
	double *Pac,    /* AC output power (Wac) */
	double *Ppar,   /* AC parasitic power consumption (Wac) */
	double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
	double *Eff,	    /* Conversion efficiency (0..1) */
	double *Pcliploss, /* Power loss due to clipping loss (Wac) */
	double *Pntloss /* Power loss due to night time tare loss (Wac) */
)
{
	//pass through inputs to the multiple MPPT function as an array with only one entry
	std::vector<double> Pdc_vec;
	Pdc_vec.push_back(Pdc);
	if (!acpower(Pdc_vec, Pac, Ppar, Plr, Eff, Pcliploss, Pntloss))
		return false;

	return true;
}

bool partload_inverter_t::acpower(
	/* inputs */
	std::vector<double> Pdc,     /* Vector of Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/

	/* outputs */
	double *Pac,    /* AC output power (Wac) */
	double *Ppar,   /* AC parasitic power consumption (Wac) */
	double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
	double *Eff,	    /* Conversion efficiency (0..1) */
	double *Pcliploss, /* Power loss due to clipping loss (Wac) */
	double *Pntloss /* Power loss due to night time tare loss (Wac) */
	)
{
	double Pdc_total = 0;
	for (size_t m = 0; m < Pdc.size(); m++)
		Pdc_total += Pdc[m];
	if ( Pdco <= 0 ) return false;

	// handle limits - can send error back or record out of range values
//	if ( Pdc < 0 ) Pdc = 0;
//	if ( Pdc > Pdco ) Pdc = Pdco;

	// linear interpolation based on Pdc/Pdco and *Partload and *Efficiency arrays
	double x = 100.0 * Pdc_total / Pdco; // percentages in partload ratio

	int n = (int)Partload.size();

	bool ascnd = (Partload[n-1] > Partload[0]); // check ascending order
	int ndx;
	int nu = n;
	int nl = 0;

	// Numerical Recipes in C p.117
	while ( (nu-nl) > 1 )
	{
		ndx = (nu + nl) >> 1; // divide by 2
		if ( (x >= Partload[ndx]) == ascnd )
			nl = ndx;
		else 
			nu = ndx;
	}
	if ( x == Partload[0] )
		ndx = 0;
	else if ( x == Partload[n-1] )
		ndx = n-1;
	else
		ndx = nl;

	// check in range
	if (ndx >= (n-1))
		ndx = n-2;
	if ( ndx < 0 ) 
		ndx =0;

	// x between Partload[ndx] and Partload[ndx-1]
	if ( x > Partload[ndx] )
		*Eff = Efficiency[ndx] + ((Efficiency[ndx+1] - Efficiency[ndx]) / 
									(Partload[ndx+1] - Partload[ndx] )) * (x - Partload[ndx]);
	else
		*Eff = Efficiency[ndx];

	if ( *Eff < 0.0 ) *Eff = 0.0;

	*Eff /= 100.0; // user data in percentages

	*Pac = *Eff * Pdc_total;
	*Ppar = 0.0;

	// night time power loss Wac
	*Pntloss = 0.0;
	if (Pdc_total <= 0.0)
	{
		*Pac = -Pntare;
		*Ppar = Pntare;
		*Pntloss = Pntare;
	}

	// clipping loss Wac
	*Pcliploss = 0.0;
	double PacNoClip = *Pac;
	if ( *Pac > Paco )	
	{
		*Pac = Paco;
		*Pcliploss = PacNoClip - *Pac;
	}

	*Plr = Pdc_total / Pdco;

	return true;
}



