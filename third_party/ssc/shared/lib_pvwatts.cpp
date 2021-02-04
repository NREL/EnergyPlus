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

/* PVWATTSV.C 
    10/18/2010 This PVWatts version was received from Ray George,
	 for integration into SSC.  Supports sub-hourly calculation.

	8/22/2007 This is a modification of PVWEB2BI.C so that PVWATTS
	can be run for time intervals less than 60 minutes. List of modifications
	follows:
	1. Added float variable step, equals time step of data, value of 60 or less
	2. Solar position determined at the time stamp minus step/2. If elevation
		> 0.5 deg, then PV calculations performed. Time stamp read in will
		need to include minute when read (variable int min)
	3. Functions celltemp, dcpower, and acpower changed to operate on single
		time stamp data rather than 24 hour data. Variables are now non-array.
	4. Data written to file: Year, Month, Day, Hour, Minute, AC power(W).

	PVWEB2BI.C Version for international data sets   2/9/06
	Reads an array of monthly albedo values from a file instead of computing
	from snowcover.
	Reads tmy data from comma delimited files.
	Reads electric cost data in new format and units.
	Default azimuth set based on if north or south hemisphere
	If latitude below equator, set default tilt to abs value


	PVWEB2b.c sets the tmloss = derate/efffp, and pcrate = dcrate, no longer
	need to normalize output to system size because system is now simulated
	for the system input, not 4 kWac. 4/21/05

	PVWEB2.C Changed from inputting an a.c. rating to inputting a d.c. rating
	and a dc to ac derate factor. a.c. rating = d.c. rating x derate factor.
	Reads inputs from c:\pvweb\pvsystm2.dat. Soiling factor is now in derate
	factor. 4/15/05

	PVWEB.C Version of PV simulation software for testing of code for end
	purpose of being available on the web.   12/7/98

	Added function transpoa to account for reflection losses. 12/8/98
	Added soiling factor of 1% loss and changed array height to 5m. 12/22/98

	Changed temperature degradation from -0.004 to -0.005, increased dc rating
	to accomodate  3/3/99

	Changed rating to 4000 Wac at STC, required dc rating change to 4503.9
	and changed inverter rating to 4500 W.   5/26/99

	Changed soiling loss from 1% to 3%     9/16/99 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "lib_pvwatts.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif


double transpoa( double poa, double dn, double inc, bool ar_glass )
{  
	/* Calculates the irradiance transmitted thru a PV module cover. Uses King
		polynomial coefficients for glass from 2nd World Conference Paper,
		July 6-10, 1998.                         Bill Marion 12/8/1998 */
	double b0=1.0,
		b1=-2.438e-3,
		b2=3.103e-4,
		b3=-1.246e-5,
		b4=2.112e-7,
		b5=-1.359e-9;

	if ( ar_glass )
	{
		// SNL parameters for SPR-E20-327, ar glass
		// added december 2013:  NOTE! need to update based on analysis of what is a "typical" AR glass coating
		b0 = 1.0002;
		b1 = -0.000213;
		b2 = 3.63416e-005;
		b3 = -2.175e-006;
		b4 = 5.2796e-008;
		b5 = -4.4351e-010;
	}

	inc = inc/0.017453293;
	if( inc > 50.0 && inc < 90.0 ) /* Adjust for relection between 50 and 90 degrees */
		{
		double x = b0 + b1*inc + b2*inc*inc + b3*inc*inc*inc + b4*inc*inc*inc*inc
			 + b5*inc*inc*inc*inc*inc;
		poa = poa - ( 1.0 - x )*dn*cos(inc*0.017453293);
		if( poa < 0.0 )
			poa = 0.0;
		}
	return(poa);
}
pvwatts_celltemp::pvwatts_celltemp( double _inoct, double _height, double _dTimeHrs)
{
	/* constants */
	boltz = 0.00000005669;
	cap = 0;
	capo = 11000.0;
	convrt = 0;
	absorb=0.83;
	emmis=0.84;
	tgrat=0;
	tgrnd=0;
	xlen=0.5;

	/* configuration parameters */
	inoct = _inoct;
	height = _height;

	/* initial values */
	dtime=12.0;
	suno=0.0;
	tmodo=293.15;

	/* convective coefficient at noct */
	windmd=1.0;
	tave=(inoct+293.15)/2.0;
	denair=0.003484*101325.0/tave;
	visair=0.24237e-6*pow(tave,0.76)/denair;
	conair=2.1695e-4*pow(tave,0.84);
	reynld=windmd*xlen/visair;
	hforce=0.8600/pow(reynld,0.5)*denair*windmd*1007.0/pow(0.71,0.67);
	grashf=9.8/tave*(inoct-293.15)*pow(xlen,3.0)/pow(visair,2.0)*0.5;
	hfree=0.21*pow(grashf*0.71,0.32)*conair/xlen;
	hconv=pow(pow(hfree,3.0)+pow(hforce,3.0),1.0/3.0);

			/* Determine the ground temperature ratio and the ratio of
				the total convection to the top side convection */
	hgrnd=emmis*boltz*(pow(inoct,2.0)+pow(293.15,2.0))*(inoct+293.15);
	backrt=( absorb*800.0-emmis*boltz*(pow(inoct,4.0)-pow(282.21,4.0))
				-hconv*(inoct-293.15) )/((hgrnd+hconv)*(inoct-293.15));
	tgrnd=pow(pow(inoct,4.0)-backrt*(pow(inoct,4.0)-pow(293.15,4.0)),0.25);
	if( tgrnd > inoct)
		tgrnd=inoct;
	if( tgrnd < 293.15)
		tgrnd=293.15;
	tgrat=(tgrnd-293.15)/(inoct-293.15);
	convrt=(absorb*800.0-emmis*boltz*(2.0*pow(inoct,4.0)-pow(282.21,4.0)
				-pow(tgrnd,4.0)))/(hconv*(inoct-293.15));

			/* Adjust the capacitance of the module based on the inoct */
	cap=capo;
	if( inoct > 321.15)
		cap=cap*(1.0+(inoct-321.15)/12.0);

	dtime = _dTimeHrs; /* set time step */
}

double pvwatts_celltemp::operator() ( double poa2, double ws2, double ambt2, double fhconv )
{
	double celltemp = ambt2;
		
	/* If poa is gt 0 then compute cell temp, else set to 999 */
	if( poa2 > 0.0 )
	{        /* Initialize local variables for insolation and temp */
		tamb=ambt2+273.15;
		suun=poa2*absorb;
		tsky=0.68*(0.0552*pow(tamb,1.5))+0.32*tamb;  /* Estimate sky temperature */

		/*  Estimate wind speed at module height - use technique developed by
				menicucci and hall (sand84-2530) */
		windmd=ws2*pow(height/9.144,0.2) + 0.0001;
									/* Find overall convective coefficient */
		tmod=tmodo;
		for(j=0;j<=9;j++)
		{
			tave=(tmod+tamb)/2.0;
			denair=0.003484*101325.0/tave;
			visair=0.24237e-6*pow(tave,0.76)/denair;
			conair=2.1695e-4*pow(tave,0.84);
			reynld=windmd*xlen/visair;
			hforce=0.8600/pow(reynld,0.5)*denair*windmd*1007.0/pow(0.71,0.67);
			if(reynld > 1.2e5)
				hforce=0.0282/pow(reynld,0.2)*denair*windmd*1007.0/pow(0.71,0.4);
			grashf=9.8/tave*fabs(tmod-tamb)*pow(xlen,3.0)/pow(visair,2.0)*0.5;
			hfree=0.21*pow(grashf*0.71,0.32)*conair/xlen;
			hconv=fhconv*convrt*pow(pow(hfree,3.0)+pow(hforce,3.0),1.0/3.0);
					/* Solve the heat transfer equation */
			hsky=emmis*boltz*(pow(tmod,2.0)+pow(tsky,2.0))*(tmod+tsky);
			tgrnd=tamb+tgrat*(tmod-tamb);
			hgrnd=emmis*boltz*(tmod*tmod+tgrnd*tgrnd)*(tmod+tgrnd);
			eigen=-(hconv+hsky+hgrnd)/cap*dtime*3600.0;
			ex=0.0;
			if(eigen > -10.0)
				ex=exp(eigen);
			tmod=tmodo*ex+((1.0-ex)*(hconv*tamb+hsky*tsky+hgrnd*tgrnd
					+suno+(suun-suno)/eigen)+suun-suno)/(hconv+hsky+hgrnd);
		}
			
		tmodo=tmod;  /* Save the new values as initial values for the next hour */
		suno=suun;
			
		celltemp = tmod-273.15;  /* PV module temperature in degrees C */
	}
	else
	{
		/* sun down, save module temp = ambient, poa = 0  (apd 2/24/2012) */
		tmodo = ambt2+273.15;
		suno = 0;
	}

	return celltemp;
}

void pvwatts_celltemp::set_last_values( double Tc, double poa )
{
	tmodo = Tc+273.15;
	suno = poa*absorb;
}
										/* Function to determine DC power */
double dcpowr(double reftem,double refpwr,double pwrdgr,double tmloss,double poa,double pvt, double iref)
{        /* Modified 8/22/07 to pass non-array variables */
/* This function was converted from a PVFORM version 3.3 subroutine but
	uses reference array power ratings instead of reference array
	efficiencies and array sizes to determine dc power.

	Following discussion is original from PVFORM:
	this routine computes the dcpower from the array given a computed
	cell temperature and poa radiation.  it uses a standard power
	degredation technique in which the array efficiency is assumed
	to decrease at a linear rate as a function of temperature rise.
	in most cases the rate of change of efficiency is about .4%perdeg c.

	The code adjusts the array effic if the insolation
	is less than 125w per m2.  the adjustment was suggested by
	fuentes based on observations of plots of effic vs insol at
	several of snla pv field sites.  when insol is less than 125
	the effic is adjusted down at a rate that is porportional
	to that that is observed in the measured field data.  this
	algorithm assumes that the effic is zero at insol of zero.
	this is not true but is a reasonable assumption for a performance
	model.  the net effect of this improvement ranges from less than
	1% in alb to about 2.2% in caribou.  the effect is to reduce
	the overall performace of a fixed tilt system.  tracking
	systems show no measurable diff in performance with respect to
	this power system adjustment.

	passed variables:
		poa = plane of array irradiances (W per m2) for each hour of day
		pvt = temperature of PV cells (deg C)
		reftem =  reference temperature (deg C)
		refpwr =  reference power (W) at reftem and iref W per m2 irradiance
		pwrdgr =  power degradation due to temperature, decimal fraction
					(si approx. -0.004, negative means efficiency decreases with
					increasing temperature)
		tmloss =  mismatch and line loss, decimal fraction

	returned variables:
		dc = dc power in watts

	local variables :
		dcpwr1 = dc power(W) from array before mismatch losses      */

	double dcpwr1,dc;

	if( poa > 125.0 )
		dcpwr1=refpwr*(1.0+pwrdgr*(pvt-reftem))*poa/iref;
	else if( poa > 0.1 )
		dcpwr1=refpwr*(1.0+pwrdgr*(pvt-reftem))*0.008*poa*poa/iref;
	else
		dcpwr1=0.0;

	dc = dcpwr1*(1.0-tmloss);   /* adjust for mismatch and line loss */
	return(dc);

}

double dctoac(double pcrate,double efffp,double dc)
{
/* Revised 8/22/07 to work with single time stamp (non-array) data.
	This function was converted from a PVFORM version 3.3 subroutine
	this routine computes the ac energy from the inverter system.
	it uses a model developed by leeman and menicucci of snla.
	the model is based on efficiency changes of typical pcu systems
	as a function of the load on the system.  these efficiency changes
	were determined through numerous measurements made at snla.
	the model is determined by fitting a curve through a set of pcu
	efficiency measurements ranging from inputs of 10% of full power
	to 100% of full power.  the equation is a 3rd order polynomial.
	between 10% and 0% a linear change is assumed ranging to an efficiency
	of -1.5% at 0% input power.

	passed variables:
		dc = dc power(W)
		pcrate = rated output of inverter in ac watts
		efffp = efficiency of inverter at full power, decimal fraction (such as 0.10)
	local variables :
		dcrtng = equivalent dc rating of pcu
		effrf = efficiency of pcu after adjustment
		percfl = percent of full load the inverter is operating at
		rateff = ratio of eff at full load / ref eff at full load
	returned variable:
		ac = ac power(W)  */
	double dcrtng,effrf,percfl,rateff,ac;

/*   Compute the ratio of the effic at full load given by the user and
	  the reference effic at full load. this will be used later to compute
	  the pcu effic for the exact conditions specified by the user.  */

	rateff=efffp/0.91;

/*   The pc rating is an ac rating so convert it to dc by dividing it
	  by the effic at 100% power. */

	dcrtng=pcrate/efffp;

	if( dc > 0.0 )
		{        /* Determine the reference efficiency based on the
						percentage of full load at input. */
		percfl=dc/dcrtng;
		if ( percfl <= 1.0 )
			{   /* if the percent of full power falls in the range of .1 to 1. then
					 use polynomial to estimate effic, else use linear equation. */
			if( percfl >= 0.1 )
				{
				effrf=0.774+(0.663*percfl)+(-0.952*percfl*percfl)+(0.426*percfl*percfl*percfl);
				if(effrf > 0.925)
					effrf=0.925;
				}
			else       /* percent of full power less than 0.1 */
				{
				effrf=(8.46*percfl)-0.015;
				if(effrf < 0.0)
					effrf=0.0;
				}
			/* compute the actual effic of the pc by adjusting according
				to the user input of effic at max power then compute power. */
			effrf=effrf*rateff;
			ac=dc*effrf;
			}
		else
			ac=pcrate;  /* On an overload condition set power to rated pc power */
		}
	else         /* dc in equals 0 */
		ac = 0.0;

	return(ac);
}
