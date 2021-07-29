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

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_pvwatts.h"
#include "lib_pvshade.h"
#include "lib_util.h"


/**********************************************************************************
************************************************************************************
**
**	12 January 2008
**
**	Port of PVWatts to C++ for SAMSIM
**	Original source: (pvwattzv1_chris.c obtained Dec 2008 from Chris Helm)
**  Modified:
**   - source code formatting
**   - weather file reading (support for tm3(csv), tm2, and epw formats)
**
**	First revision, Aron Dobos
**
**********************************************************************************
***********************************************************************************/

/* PVWATTS.C Version for UNIX. Changed file names to comply with locations
	on the NREL UNIX system. Replaced sun[8] with sunn[8]. Replaced function
	call strnicmp with strncmp.  3/16/99

	PVWEB.C Version of PV simulation software for testing of code for end
	purpose of being available on the web.   12/7/98

	Added function transpoa to account for reflection losses. 12/8/98
	Added soiling factor of 1% loss and changed array height to 5m. 12/22/98
	Changed temperature degradation from -0.004 to -0.005, increased dc rating
	to accomodate   3/3/99

	Changed rating to 4000 Wac at STC, required dc rating change to 4503.9
	and changed inverter rating to 4500 W.   5/26/99 */

/*
************************************************************************************
** pvwattzv1_hr.c
**
** Formerly pvwattz_hr.c. renamed and editted to run from new pvwattzv1.cgi 
** 2005.06.14.
**
** Specifically for calculating and ouputting the PVWATTS hourly preformance data.
**  "Called" by pvwattz.1.c
**  Mary Anderberg
**  2004.07.14
************************************************************************************
**
** This is the C CGI program version of Bill Marion's PVWATTS
**
** Used David Martin's shuttlemap.c for inspiration, pointers, etc.,...
**
** Note: Here *wban is a pointer to facilitate input to CGI from form. 
** Whereas char swban[6] is read in from file station.num in B.Marion's original code 
** (see /rredc01/bmarion for original pvwatts.c and input files), then char wban[6]
** read from the *.tm2 (TMY2) file; in this CGI (pvwatts.cgi) *wban is input from 
** the site form (e.g., Birmingham.html) and char swban[6] is read from the *.tm2 file.
** The array char cwban[6] is the same in both versions.
** No changes have been made to the calculations code.
**
** Mary Anderberg
**
**	July 1999
*********************
**
** Changed line 381 from  tpoa[i] = 0.99*tpoa[i] to tpoa[i] = 0.97*tpoa[i]
**
** Mary Anderberg
** 17 September 1999
**
*********************
**
**	Changed character array name[] in line 108
**	 from "/rredc06/rredc/nsrdb/tmy2/txt/dos/XXXXX/tm2"
**	 to	  "/kepler/rredc/solar/old_data/nsrdb/tmy2/txt/dos/XXXXX.tm2"
**
**	and adjusted array size from 44 to 58.
**  In line 1967 changed "name+34" to "name+48".
**
**	Moving RReDC from old Sparc 10 (delphi) to new Ultra 10S (kepler).
**
**	Mary Anderberg
**	6 March 2000
**
******************************************************************************
**
**	Changed character array name[] in line 122 (formerly 108)
**	 from "/kepler/rredc/solar/old_data/nsrdb/tmy2/txt/dos/XXXXX.tm2"
**	  to  "/dirk/rredc/solar/old_data/nsrdb/tmy2/txt/dos/XXXXX.tm2"
**
**	and adjusted array size from 58 to 56.
**  
**	Moving RReDC from Ultra 10S (kepler) to dirk.
**
**	Mary Anderberg
**	4 March 2002
**
********************************************************************************
**
**  Added option to output to print hourly PV performance data (AC power.)
**      Output is printed to a new browser window, leaving the monthly output 
**      table in the pernent window.
**
**  Created second CGI, pvwatts_hr.cgi, to be called by pvwatts.cgi (compiled
**  from pvwattz.1.c ).
**  Monthly averages not computed or printed in this version; rather, a new
**  browser window is opened and the second CGI run in it, calculating and
**  printing out hourly PV perfromance data based on earlier inputs.
**
**  Mary Anderberg
**  16 July 2004
**
******************************************************************************
**
** Changed rlim=90.0 to rlim=45.0 in declaration at line 161, as per Bill Marion's 
** request. Cahnge was made in "regual;r version 1 code on 12 September 2006.
**
** Mary Anderberg
** 20 April 2007
**
******************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

static int nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};


static double transpoa( double poa,double dn,double inc )
{  /* Calculates the irradiance transmitted thru a PV module cover. Uses King
		polynomial coefficients for glass from 2nd World Conference Paper,
		July 6-10, 1998.                         Bill Marion 12/8/1998 */

	double b0 = 1.0, b1 = -2.438e-3, b2 = 3.103e-4, b3 = -1.246e-5, b4 = 2.112e-7,
		b5 = -1.359e-9, x;

	inc = inc/DTOR;
	if( inc > 50.0 && inc < 90.0 ) /* Adjust for relection between 50 and 90 degrees */
		{
		x = b0 + b1*inc + b2*inc*inc + b3*inc*inc*inc + b4*inc*inc*inc*inc
			 + b5*inc*inc*inc*inc*inc;
		poa = poa - ( 1.0 - x )*dn*cos(inc*DTOR);
		if( poa < 0.0 )
			poa = 0.0;
		}
	return(poa);
}

					  /* Defines function to calculate cell temperature */
static void celltemp(double inoct,double height,double poa[24],double ambt[24],double wind[24],double pvt[24] )
{   /* Modified 7/28/98 to pass array variables */
/* This function was converted from a PVFORM version 3.3 subroutine
	this routine estimates the array temperature given the poa radiation,
	ambient temperature, and wind speed.  it uses an advanced cell temp
	model developed by m fuentes at snla.  if the poa insolation is eq
	zero then set cell temp = 999.

	passed array variables:
		poa[24] = plane of array irradiances (W/m2) for each hour of day
		ambt[24] = ambient temperatures (deg C)
		wind[24] = wind speeds (m/s)
		pvt[24] = temperature of PV cells (deg C)

	passed variables:
		inoct = installed nominal operating cell temperature (deg K)
		height = average array height (meters)

c  local variables :
c     absorb = absorbtivity
c     backrt = ratio of actual backside heat xfer to theoretical of rack mount
c     boltz = boltzmann's constant
c     cap = capacitance per unit area of module
c     capo = capacitance per unit area of rack mounted module
c     conair = conductivity of air
c     convrt = ratio of total convective heat xfer coef to topside hxc
c     denair = density of air
c     dtime = time step
c     eigen = product of eigen value and time step
c     emmis = emmisivity
		ex = ?
c     grashf = grashoffs number
c     hconv = convective coeff of module (both sides)
c     hforce = forced convective coeff of top side
c     hfree = free convective coeff of top side
c     hgrnd = radiative heat xfer coeff from module to ground
		hsky = ?
c     iflagc = flag to check if routine has been executed
c     reynld = reynolds number
c     sun = insolation at start of time step
c     suno = previous hours insolation
c     tamb = ambient temp
c     tave = average of amb and cell temp
c     tgrat = ratio of grnd temp above amb to cell temp above amb
c     tgrnd = temperature of ground
c     tmod = computed cell temp
c     tmodo = cell temp for previous time step
c     tsky = sky temp
c     visair = viscosity of air
c     windmd = wind speed at module height
c     xlen = hydrodynamic length of module              */

	int i,j,iflagc=0;
	double absorb=0.83,backrt,boltz=5.669e-8,cap=0,capo=11000.0,conair,convrt=0,denair;
	double dtime,eigen,emmis=0.84,grashf,hconv,hforce,hfree,hgrnd,reynld,sunn;
	double suno,tamb,tave,tgrat=0,tgrnd,tmod,tmodo,tsky,visair,windmd,xlen=0.5;
	double hsky,ex;

/* Set time step to a large number for very first calc. After
	that set time step to 1 (1 hr). Also set prev poa and prev
	module temp for first time through                  */

	dtime=12.0;
	suno=0.0;
	tmodo=293.15;

/* Compute convective coeff, grnd temp ratio, and mod capac one time */

	if( iflagc != 1 )
		{
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
		iflagc=1;
		}

	for(i=0;i<=23;i++)         /* Loop through 24 hours of data */
		{        /* if poa is gt 0 then compute cell temp, else set to 999 */
		if( poa[i] > 0.0 )
			{        /* Initialize local variables for insolation and temp */
			tamb=ambt[i]+273.15;
			sunn=poa[i]*absorb;
			tsky=0.68*(0.0552*pow(tamb,1.5))+0.32*tamb;  /* Estimate sky temperature */

			/*  Estimate wind speed at module height - use technique developed by
				 menicucci and hall (sand84-2530) */
			windmd=wind[i]*pow(height/9.144,0.2) + 0.0001;
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
				hconv=convrt*pow(pow(hfree,3.0)+pow(hforce,3.0),1.0/3.0);
						/* Solve the heat transfer equation */
				hsky=emmis*boltz*(pow(tmod,2.0)+pow(tsky,2.0))*(tmod+tsky);
				tgrnd=tamb+tgrat*(tmod-tamb);
				hgrnd=emmis*boltz*(tmod*tmod+tgrnd*tgrnd)*(tmod+tgrnd);
				eigen=-(hconv+hsky+hgrnd)/cap*dtime*3600.0;
				ex=0.0;
				if(eigen > -10.0)
					ex=exp(eigen);
				tmod=tmodo*ex+((1.0-ex)*(hconv*tamb+hsky*tsky+hgrnd*tgrnd
					  +suno+(sunn-suno)/eigen)+sunn-suno)/(hconv+hsky+hgrnd);
				}
			tmodo=tmod;  /* Save the new values as initial values for the next hour */
			suno=sunn;
			dtime=1.0;

			pvt[i]=tmod-273.15;  /* PV module temperature in degrees C */
			}
		else
			pvt[i] = 999.0;      /* Default temp for zero irradiance */
		}
}
										/* Function to determine DC power */
static void dcpowr(double reftem,double refpwr,double pwrdgr,double tmloss,double poa[24],double pvt[24],double dc[24])
{        /* Modified 7/28/98 to pass array variables */
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

	passed array variables:
		poa[24] = plane of array irradiances (W per m2) for each hour of day
		pvt[24] = temperature of PV cells (deg C)
		dc[24] = dc power (W)

	passed variables:
		reftem =  reference temperature (deg C)
		refpwr =  reference power (W) at reftem and 1000 W per m2 irradiance
		pwrdgr =  power degradation due to temperature, decimal fraction
					(si approx. -0.004, negative means efficiency decreases with
					increasing temperature)
		tmloss =  mismatch and line loss, decimal fraction

	local variables :
		dcpwr1 = dc power(W) from array before mismatch losses      */

	int i;
	double dcpwr1;

	for(i=0;i<=23;i++)    /* Compute dc power for each hour */
		{
		if( poa[i] > 125.0 )
			dcpwr1=refpwr*(1.0+pwrdgr*(pvt[i]-reftem))*poa[i]/1000.0;
		else if( poa[i] > 0.1 )
			dcpwr1=refpwr*(1.0+pwrdgr*(pvt[i]-reftem))*0.008*poa[i]*poa[i]/1000.0;
		else
			dcpwr1=0.0;

		dc[i]=dcpwr1*(1.0-tmloss);   /* adjust for mismatch and line loss */
		}
}

static void dctoac(double pcrate,double efffp,double dc[24],double ac[24])
{
/* This function was converted from a PVFORM version 3.3 subroutine
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

	passed array variables:
		dc[24] = dc power(W)
		ac[24] = ac power(W)
	passed variables:
		pcrate = rated output of inverter in ac watts
		efffp = efficiency of inverter at full power, decimal fraction (such as 0.10)
	local variables :
		dcrtng = equivalent dc rating of pcu
		effrf = efficiency of pcu after adjustment
		percfl = percent of full load the inverter is operating at
		rateff = ratio of eff at full load / ref eff at full load      */

	int i;
	double dcrtng,effrf,percfl,rateff;

/*   Compute the ratio of the effic at full load given by the user and
	  the reference effic at full load. this will be used later to compute
	  the pcu effic for the exact conditions specified by the user.  */

	rateff=efffp/0.91;

/*   The pc rating is an ac rating so convert it to dc by dividing it
	  by the effic at 100% power. */

	dcrtng=pcrate/efffp;

	for(i=0;i<24;i++)      /* Calculate ac power for each hour */
		{
		if( dc[i] > 0.0 )
		{        /* Determine the reference efficiency based on the
							percentage of full load at input. */
			percfl=dc[i]/dcrtng;
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
				ac[i]=dc[i]*effrf;
				}
			else
			{
				ac[i]=pcrate;  /* On an overload condition set power to rated pc power */
			}
		}
		else         /* dc in equals 0 */
			ac[i] = 0.0;
		}
}

static int julian(int yr,int month,int day)    /* Calculates julian day of year */
{
	int i = 1, jday = 0, k;

	if( yr%4 == 0 )                      /* For leap years */
		k = 1;
	else
		k = 0;

	while( i < month )
		{
		jday = jday + nday[i-1];
		i++;
		}
	if( month > 2 )
		jday = jday + k + day;
	else
		jday = jday + day;
	return(jday);
}


static void solarpos_v0(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[8])
{
/* This function is based on a paper by Michalsky published in Solar Energy
	Vol. 40, No. 3, pp. 227-235, 1988. It calculates solar position for the
	time and location passed to the function based on the Astronomical
	Almanac's Algorithm for the period 1950-2050. For data averaged over an
	interval, the appropriate time passed is the midpoint of the interval.
	(Example: For hourly data averaged from 10 to 11, the time passed to the
	function should be 10 hours and 30 minutes). The exception is when the time
	interval includes a sunrise or sunset. For these intervals, the appropriate
	time should be the midpoint of the portion of the interval when the sun is
	above the horizon. (Example: For hourly data averaged from 7 to 8 with a
	sunrise time of 7:30, the time passed to the function should be 7 hours and
	and 45 minutes).

	Revised 5/15/98. Replaced algorithm for solar azimuth with one by Iqbal
	so latitudes below the equator are correctly handled. Also put in checks
	to allow an elevation of 90 degrees without crashing the program and prevented
	elevation from exceeding 90 degrees after refraction correction.

	This function calls the function julian to get the julian day of year.

	List of Parameters Passed to Function:
	year   = year (e.g. 1986)
	month  = month of year (e.g. 1=Jan)
	day    = day of month
	hour   = hour of day, local standard time, (1-24, or 0-23)
	minute = minutes past the hour, local standard time
	lat    = latitude in degrees, north positive
	lng    = longitude in degrees, east positive
	tz     = time zone, west longitudes negative

	sunn[]  = array of elements to return sun parameters to calling function
	sunn[0] = azm = sun azimuth in radians, measured east from north, 0 to 2*pi
	sunn[1] = 0.5*pi - elv = sun zenith in radians, 0 to pi
	sunn[2] = elv = sun elevation in radians, -pi/2 to pi/2
	sunn[3] = dec = sun declination in radians
	sunn[4] = sunrise in local standard time (hrs), not corrected for refraction
	sunn[5] = sunset in local standard time (hrs), not corrected for refraction
	sunn[6] = Eo = eccentricity correction factor
	sunn[7] = tst = true solar time (hrs)                */

	int jday,delta,leap;                           /* Local variables */
	double zulu,jd,time,mnlong,mnanom,
			eclong,oblqec,num,den,ra,dec,gmst,lmst,ha,elv,azm,refrac,
			E,ws,sunrise,sunset,Eo,tst;
	double arg;

	jday = julian(year,month,day);       /* Get julian day of year */
	zulu = hour + minute/60.0 - tz;      /* Convert local time to zulu time */
	if( zulu < 0.0 )                     /* Force time between 0-24 hrs */
		{                                 /* Adjust julian day if needed */
		zulu = zulu + 24.0;
		jday = jday - 1;
		}
	else if( zulu > 24.0 )
		{
		zulu = zulu - 24.0;
		jday = jday + 1;
		}
	delta = year - 1949;
	leap = delta/4;
	jd = 32916.5 + delta*365 + leap + jday + zulu/24.0;
	time = jd - 51545.0;     /* Time in days referenced from noon 1 Jan 2000 */

	mnlong = 280.46 + 0.9856474*time;
	mnlong = fmod(mnlong, 360.0);         /* Finds floating point remainder */
	if( mnlong < 0.0 )
		mnlong = mnlong + 360.0;          /* Mean longitude between 0-360 deg */

	mnanom = 357.528 + 0.9856003*time;
	mnanom = fmod(mnanom, 360.0);
	if( mnanom < 0.0 )
		mnanom = mnanom + 360.0;
	mnanom = mnanom*DTOR;             /* Mean anomaly between 0-2pi radians */

	eclong = mnlong + 1.915*sin(mnanom) + 0.020*sin(2.0*mnanom);
	eclong = fmod(eclong, 360.0);
	if( eclong < 0.0 )
		eclong = eclong + 360.0;
	eclong = eclong*DTOR;       /* Ecliptic longitude between 0-2pi radians */

	oblqec = ( 23.439 - 0.0000004*time )*DTOR;   /* Obliquity of ecliptic in radians */
	num = cos(oblqec)*sin(eclong);
	den = cos(eclong);
	ra  = atan(num/den);                         /* Right ascension in radians */
	if( den < 0.0 )
		ra = ra + M_PI;
	else if( num < 0.0 )
		ra = ra + 2.0*M_PI;

	dec = asin( sin(oblqec)*sin(eclong) );       /* Declination in radians */

	gmst = 6.697375 + 0.0657098242*time + zulu;
	gmst = fmod(gmst, 24.0);
	if( gmst < 0.0 )
		gmst = gmst + 24.0;         /* Greenwich mean sidereal time in hours */

	lmst = gmst + lng/15.0;
	lmst = fmod(lmst, 24.0);
	if( lmst < 0.0 )
		lmst = lmst + 24.0;
	lmst = lmst*15.0*DTOR;         /* Local mean sidereal time in radians */

	ha = lmst - ra;
	if( ha < -M_PI)
		ha = ha + 2* M_PI;
	else if( ha > M_PI)
		ha = ha - 2* M_PI;             /* Hour angle in radians between -pi and pi */

	lat = lat*DTOR;                /* Change latitude to radians */

	arg = sin(dec)*sin(lat) + cos(dec)*cos(lat)*cos(ha);  /* For elevation in radians */
	if( arg > 1.0 )
		elv = M_PI /2.0;
	else if( arg < -1.0 )
		elv = -M_PI /2.0;
	else
		elv = asin(arg);

	if( cos(elv) == 0.0 )
		{
		azm = M_PI;         /* Assign azimuth = 180 deg if elv = 90 or -90 */
		}
	else
		{                 /* For solar azimuth in radians per Iqbal */
		arg = ((sin(elv)*sin(lat)-sin(dec))/(cos(elv)*cos(lat))); /* for azimuth */
		if( arg > 1.0 )
			azm = 0.0;              /* Azimuth(radians)*/
		else if( arg < -1.0 )
			azm = M_PI;
		else
			azm = acos(arg);

		if( ( ha <= 0.0 && ha >= -M_PI) || ha >= M_PI)
			azm = M_PI - azm;
		else
			azm = M_PI + azm;
		}

	elv = elv/DTOR;          /* Change to degrees for atmospheric correction */
	if( elv > -0.56 )
		refrac = 3.51561*( 0.1594 + 0.0196*elv + 0.00002*elv*elv )/( 1.0 + 0.505*elv + 0.0845*elv*elv );
	else
		refrac = 0.56;
	if( elv + refrac > 90.0 )
		elv = 90.0*DTOR;
	else
		elv = ( elv + refrac )*DTOR ; /* Atmospheric corrected elevation(radians) */

	E = ( mnlong - ra/DTOR )/15.0;       /* Equation of time in hours */
	if( E < - 0.33 )   /* Adjust for error occuring if mnlong and ra are in quadrants I and IV */
		E = E + 24.0;
	else if( E > 0.33 )
		E = E - 24.0;

	arg = -tan(lat)*tan(dec);
	if( arg >= 1.0 )
		ws = 0.0;                         /* No sunrise, continuous nights */
	else if( arg <= -1.0 )
		ws = M_PI;                          /* No sunset, continuous days */
	else
		ws = acos(arg);                   /* Sunrise hour angle in radians */

					/* Sunrise and sunset in local standard time */
	sunrise = 12.0 - (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;
	sunset  = 12.0 + (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;

	Eo = 1.00014 - 0.01671*cos(mnanom) - 0.00014*cos(2.0*mnanom);  /* Earth-sun distance (AU) */
	Eo = 1.0/(Eo*Eo);                    /* Eccentricity correction factor */

	tst = hour + minute/60.0 + (lng/15.0 - tz) + E;  /* True solar time (hr) */

	sunn[0] = azm;                        /* Variables returned in array sunn[] */
	sunn[1] = 0.5*M_PI - elv;               /*  Zenith */
	sunn[2] = elv;
	sunn[3] = dec;
	sunn[4] = sunrise;
	sunn[5] = sunset;
	sunn[6] = Eo;
	sunn[7] = tst;
	//sunn[8] = zulu;
}

static void incident2(int mode,double tilt,double sazm,double rlim,double zen,double azm,double angle[3])
{
/* This function calculates the incident angle of direct beam radiation to a
	surface for a given sun position, latitude, and surface orientation. The
	modes available are fixed tilt, 1-axis tracking, and 2-axis tracking.
	Azimuth angles are for N=0 or 2pi, E=pi/2, S=pi, and W=3pi/2.  8/13/98

	List of Parameters Passed to Function:
	mode   = 0 for fixed-tilt, 1 for 1-axis tracking, 2 for 2-axis tracking
	tilt   = tilt angle of surface from horizontal in degrees (mode 0),
				or tilt angle of tracker axis from horizontal in degrees (mode 1),
				MUST BE FROM 0 to 90 degrees.
	sazm   = surface azimuth in degrees of collector (mode 0), or surface
				azimuth of tracker axis (mode 1) with axis azimuth directed from
				raised to lowered end of axis if axis tilted.
	rlim   = plus or minus rotation in degrees permitted by physical constraints
				of tracker, range is 0 to 180 degrees.
	zen    = sun zenith in radians, MUST BE LESS THAN PI/2
	azm    = sun azimuth in radians, measured east from north

	Parameters Returned:
	angle[]  = array of elements to return angles to calling function
	angle[0] = inc  = incident angle in radians
	angle[1] = tilt = tilt angle of surface from horizontal in radians
	angle[2] = sazm = surface azimuth in radians, measured east from north  */

	/* Local variables: rot is the angle that the collector is rotated about the
	axis when viewed from the raised end of the 1-axis tracker. If rotated
	counter clockwise the angle is negative. Range is -180 to +180 degrees.
	When xsazm = azm : rot = 0, tilt = xtilt, and sazm = xsazm = azm  */

	double arg,inc=0,xsazm,xtilt,rot;

	switch ( mode )
		{
		case 0:                 /* Fixed-Tilt */
			tilt = tilt*DTOR;    /* Change tilt and surface azimuth to radians */
			sazm = sazm*DTOR;
			arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
			if( arg < -1.0 )
				inc = M_PI;
			else if( arg > 1.0  )
				inc = 0.0;
			else
				inc = acos(arg);
			break;
		case 1:                 /* One-Axis Tracking */
			xtilt = tilt*DTOR;   /* Change axis tilt, surface azimuth, and rotation limit to radians */
			xsazm = sazm*DTOR;
			rlim  = rlim*DTOR;
									/* Find rotation angle of axis for peak tracking */
			if( fabs( cos(xtilt) ) < 0.001745 )    /* 89.9 to 90.1 degrees */
				{          /* For vertical axis only */
				if( xsazm <= M_PI)
					{
					if( azm <= xsazm + M_PI)
						rot = azm - xsazm;
					else
						rot = azm - xsazm - 2.0*M_PI;
					}
				else        /* For xsazm > pi */
					{
					if( azm >= xsazm - M_PI)
						rot = azm - xsazm;
					else
						rot = azm - xsazm + 2.0*M_PI;
					}
				}
			else          /* For other than vertical axis */
				{
				arg = sin(zen)*sin(azm-xsazm)/
						( sin(zen)*cos(azm-xsazm)*sin(xtilt) + cos(zen)*cos(xtilt) );
				if( arg < -99999.9 )
					rot = -M_PI /2.0;
				else if( arg > 99999.9 )
					rot = M_PI /2.0;
				else
					rot = atan(arg);
								/* Put rot in II or III quadrant if needed */
				if( xsazm <= M_PI)
					{
					if( azm > xsazm && azm <= xsazm + M_PI)
						{     /* Ensure positive rotation */
						if( rot < 0.0 )
							rot = M_PI + rot;   /* Put in II quadrant: 90 to 180 deg */
						}
					else
						{     /* Ensure negative rotation  */
						if( rot > 0.0 )
							rot = rot - M_PI;   /* Put in III quadrant: -90 to -180 deg */
						}
					}
				else        /* For xsazm > pi */
					{
					if( azm < xsazm && azm >= xsazm - M_PI)
						{     /* Ensure negative rotation  */
						if( rot > 0.0 )
							rot = rot - M_PI;   /* Put in III quadrant: -90 to -180 deg */
						}
					else
						{     /* Ensure positive rotation */
						if( rot < 0.0 )
							rot = M_PI + rot;   /* Put in II quadrant: 90 to 180 deg */
						}
					}
				}
	  /*    printf("rot=%6.1f azm=%6.1f xsazm=%6.1f xtilt=%6.1f zen=%6.1f<BR>",rot/DTOR,azm/DTOR,xsazm/DTOR,xtilt/DTOR,zen/DTOR);  */

			if( rot < -rlim ) /* Do not let rotation exceed physical constraints */
				rot = -rlim;
			else if( rot > rlim )
				rot = rlim;

									/* Find tilt angle for the tracking surface */
			arg = cos(xtilt)*cos(rot);
			if( arg < -1.0 )
				tilt = M_PI;
			else if( arg > 1.0  )
				tilt = 0.0;
			else
				tilt = acos(arg);
									/* Find surface azimuth for the tracking surface */
			if( tilt == 0.0 )
				sazm = M_PI;     /* Assign any value if tilt is zero */
			else
				{
				arg = sin(rot)/sin(tilt);
				if( arg < -1.0 )
					sazm = 1.5*M_PI + xsazm;
				else if( arg > 1.0  )
					sazm = 0.5*M_PI + xsazm;
				else if( rot < -0.5*M_PI)
					sazm = xsazm - M_PI - asin(arg);
				else if( rot > 0.5*M_PI)
					sazm = xsazm + M_PI - asin(arg);
				else
					sazm = asin(arg) + xsazm;
				if( sazm > 2.0*M_PI)       /* Keep between 0 and 2pi */
					sazm = sazm - 2.0*M_PI;
				else if( sazm < 0.0 )
					sazm = sazm + 2.0*M_PI;
				}
		/* printf("zen=%6.1f azm-sazm=%6.1f tilt=%6.1f arg=%7.4f<BR>",zen/DTOR,(azm-sazm)/DTOR,tilt/DTOR,arg); */
									/* Find incident angle */
			arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
			if( arg < -1.0 )
				inc = M_PI;
			else if( arg > 1.0  )
				inc = 0.0;
			else
				inc = acos(arg);
			break;
		case 2:                 /* Two-Axis Tracking */
			tilt = zen;
			sazm = azm;
			inc = 0.0;
			break;
		}
	angle[0] = inc;           /* Variables returned in array angle[] */
	angle[1] = tilt;
	angle[2] = sazm;
}

static double perez( double dn,double df,double alb,double inc,double tilt,double zen )
{
/* Defines the Perez function for calculating values of diffuse + direct
	solar radiation + ground reflected radiation for a tilted surface
	and returns the total plane-of-array irradiance(poa).  Function does
	not check all input for valid entries; consequently, this should be
	done before calling the function.  (Reference: Perez et al, Solar
	Energy Vol. 44, No.5, pp.271-289,1990.) Based on original FORTRAN
	program by Howard Bisner.

	Modified 6/10/98 so that for zenith angles between 87.5 and 90.0 degrees,
	the diffuse radiation is treated as isotropic instead of 0.0.

	List of Parameters Passed to Function:
	dn     = direct normal radiation (W/m2)
	df     = diffuse horizontal radiation (W/m2)
	alb    = surface albedo (decimal fraction)
	inc    = incident angle of direct beam radiation to surface in radians
	tilt   = surface tilt angle from horizontal in radians
	zen    = sun zenith angle in radians

	Variable Returned
	poa    = plane-of-array irradiance (W/m2), sum of direct beam and sky
				and ground-reflected diffuse */

													/* Local variables */
	double F11R[8] = { -0.0083117, 0.1299457, 0.3296958, 0.5682053,
							 0.8730280, 1.1326077, 1.0601591, 0.6777470 };
	double F12R[8] = {  0.5877285, 0.6825954, 0.4868735, 0.1874525,
							-0.3920403, -1.2367284, -1.5999137, -0.3272588 };
	double F13R[8] = { -0.0620636, -0.1513752, -0.2210958, -0.2951290,
							-0.3616149, -0.4118494, -0.3589221, -0.2504286 };
	double F21R[8] = { -0.0596012, -0.0189325, 0.0554140, 0.1088631,
							 0.2255647, 0.2877813, 0.2642124, 0.1561313 };
	double F22R[8] = {  0.0721249, 0.0659650, -0.0639588, -0.1519229,
							-0.4620442, -0.8230357, -1.1272340, -1.3765031 };
	double F23R[8] = { -0.0220216, -0.0288748, -0.0260542, -0.0139754,
							 0.0012448, 0.0558651, 0.1310694, 0.2506212 };
	double EPSBINS[7] = { 1.065, 1.23, 1.5, 1.95, 2.8, 4.5, 6.2 };
	double B2=0.000005534,EPS,T,D,DELTA,A,B,C,ZH,F1,F2,
			COSINC,poa,x;
	double CZ,ZC,ZENITH,AIRMASS;
	int i;

	if ( dn < 0.0 )           /* Negative values may be measured if cloudy */
		dn = 0.0;
	if ( zen < 0.0 || zen > 1.5271631 ) /* Zen not between 0 and 87.5 deg */
		{
		if( df < 0.0 )
			df = 0.0;
		if ( cos(inc) > 0.0 && zen < 1.5707963 )  /* Zen between 87.5 and 90 */
			{                                      /* and incident < 90 deg   */
			poa = df*( 1.0 + cos(tilt) )/2.0 + dn*cos(inc);
			return(poa);
			}
		else
			{
			poa = df*( 1.0 + cos(tilt) )/2.0;   /* Isotropic diffuse only */
			return(poa);
			}
		}
	else                      /* Zen between 0 and 87.5 deg */
		{
		CZ = cos(zen);
		ZH = ( CZ > 0.0871557 ) ? CZ:0.0871557;    /* Maximum of 85 deg */
		D = df;                /* Horizontal diffuse radiation */
		if ( D <= 0.0 )        /* Diffuse is zero or less      */
			{
			if ( cos(inc) > 0.0 )    /* Incident < 90 deg */
				{
				poa = 0.0 + dn*cos(inc);
				return(poa);
				}
			else
				{
				poa = 0.0;
				return(poa);
				}
			}
		else                   /* Diffuse is greater than zero */
			{
			ZENITH = zen/DTOR;
			AIRMASS = 1.0 / (CZ + 0.15 * pow(93.9 - ZENITH, -1.253) );
			DELTA = D * AIRMASS / 1367.0;
			T = pow(ZENITH,3.0);
			EPS = (dn + D) / D;
			EPS = (EPS + T*B2) / (1.0 + T*B2);
			i=0;
			while ( i < 7 && EPS > EPSBINS[i] )
				i++;
			x = F11R[i] + F12R[i]*DELTA + F13R[i]*zen;
			F1 = ( 0.0 > x ) ? 0.0:x;
			F2 = F21R[i] + F22R[i]*DELTA + F23R[i]*zen;
			COSINC = cos(inc);
			if( COSINC < 0.0 )
				ZC = 0.0;
			else
				ZC = COSINC;
			A = D*( 1.0 + cos(tilt) )/2.0;
			B = ZC/ZH*D - A;
			C = D*sin(tilt);
			poa = A + F1*B + F2*C + alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0 + dn*ZC;
			return(poa);
			}
		}
}


/**************** C++ PV WATTS CODE *****************/

static var_info _cm_vtab_pvwattsv0[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                               UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",                      "local weather file path",                     "",       "",                        "Weather",      "*",                       "LOCAL_FILE",      "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                    "Nameplate capacity",                          "kW",     "",                        "PVWatts",      "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                         "System derate value",                         "frac",   "",                        "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                     "Tracking mode",                               "0/1/2/3","Fixed,1Axis,2Axis,AziAxis","PVWatts",      "*",                       "MIN=0,MAX=3,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                        "Azimuth angle",                               "deg",    "E=90,S=180,W=270",        "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                           "Tilt angle",                                  "deg",    "H=0,V=90",                "PVWatts",      "naof:tilt_eq_lat",        "MIN=0,MAX=90",                             "" },
	
	/* outputs */

	{ SSC_OUTPUT,       SSC_ARRAY,       "dn",                             "Beam irradiance",                             "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "df",                             "Diffuse irradiance",                          "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tamb",                           "Ambient temperature",                         "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tdew",                           "Dew point temperature",                       "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wspd",                           "Wind speed",                                  "m/s",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                            "Plane of array irradiance",                   "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                          "Module temperature",                          "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                             "DC array output",                             "Wdc",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                             "AC system output",                            "Wac",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sunup",                          "Sun up over horizon",                         "0/1",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },

	var_info_invalid };

class cm_pvwattsv0 : public compute_module
{
public:
	
	cm_pvwattsv0()
	{
		add_var_info( _cm_vtab_pvwattsv0 );
	}

	void exec( )
	{
		const char *file = as_string("file_name");

		weatherfile wfile( file );
		if (!wfile.ok()) throw exec_error("pvwattsv1", wfile.message());
		if( wfile.has_message() ) log( wfile.message(), SSC_WARNING);
		
		double dcrate = as_double("system_size"); /* DC rating */
		double derate = as_double("derate"); /* Derate factor */
		int mode = as_integer("track_mode"); /* array mode */
		double tilt = as_double("tilt"); /* Tilt (deg) */
		double sazm = as_double("azimuth"); /* Azimuth (deg) */

		double tmp,tmp2,inoct,height;

		int yr,mn,dy;
		int i,m,n,sunup[24],beghr,endhr,jday;
		int cur_hour;

		double lat,lng,tz,minute,sunn[8],angle[3],sunrise,sunset;
		double dn[24], df[24], alb;
		double poa[24],ambt[24],wind[24],pvt[24],dc[24],ac[24],tpoa[24];
		double reftem,refpwr,pwrdgr,tmloss,pcrate,efffp;
		double rlim = 45.0;     // mod. 2007-04-20 (see Mods list)


		ssc_number_t *p_dn = allocate("dn", 8760);
		ssc_number_t *p_df = allocate("df", 8760);
		ssc_number_t *p_tamb = allocate("tamb", 8760);
		ssc_number_t *p_wspd = allocate("wspd", 8760);

		ssc_number_t *p_dc = allocate("dc", 8760);
		ssc_number_t *p_ac = allocate("ac", 8760);
		ssc_number_t *p_tcell = allocate("tcell", 8760);
		ssc_number_t *p_poa = allocate("poa", 8760);

		ssc_number_t *p_sunup = allocate("sunup", 8760);

	
		/*************		 PV RELATED SPECIFICATIONS          ********************/
		inoct = 45.0 + 273.15;     /* Installed normal operating cell temperature (deg K) */
		height = 5.0;              /* Average array height (meters) */
		reftem = 25.0;             /* Reference module temperature (deg C) */
		pwrdgr = -0.005;           /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
		efffp = 0.92;              /* Efficiency of inverter at rated output (decimal fraction) */

		weather_header hdr;
		wfile.header( &hdr );

		lat = (double)hdr.lat;         /* In degrees */
		lng = (double)hdr.lon;
		tz = (double)hdr.tz;

		if ( dcrate < 0.49999 || dcrate > 99999.9 )  // Use defaults if dcrate out of range
			dcrate = 4.0;
	// bounds of (0,09999, 0.99001) are consistent with online PVWatts http://rredc.nrel.gov/solar/codes_algs/PVWATTS/version1/US/code/pvwattsv1.cgi
	//    if ( derate < 0.09999 || derate > 0.99001 ) // Use if default ac to dc derate factor out of range
		if ( derate < 0.0 || derate > 1.0 ) // Use if default ac to dc derate factor out of range
			derate = 0.77;
        
		pcrate = dcrate * 1000.0;      // rated output of inverter in a.c. watts; 6/29/2005
		refpwr = dcrate * 1000.0;      // nameplate in watts; 6/29/2005
		tmloss = 1.0 - derate/efffp;   // All losses except inverter, decimal; 6/29/2005


		if( mode < 0 || mode > 2 )
			mode = 0;
		if( tilt < -0.0001 || tilt > 90.0001 )
			tilt = lat;
		if( sazm < -0.0001 || sazm > 360.0001 )
			sazm = 180.0;



		cur_hour = 0;
		jday = 0;
		yr=mn=dy=0;

		weather_record wf;

		for(m=0;m<12;m++)   /* Loop thru a year of data a month at a time */
		{
			for(n=1;n<=nday[m];n++)    /* For each day of month */
			{
				jday++;                 /* Increment julian day */
				for(i=0;i<=23;i++)      /* Read a day of data and initialize */
				{
					wfile.read( &wf );

					yr = wf.year;
					mn = wf.month;
					dy = wf.day;
					dn[i] = wf.dn;
					df[i] = wf.df;
					ambt[i] = wf.tdry;
					wind[i] = wf.wspd;

					poa[i]=0.0;             /* Plane-of-array radiation */
					tpoa[i]=0.0;            /* Transmitted radiation */
					dc[i]=0.0;              /* DC power */
					ac[i]=0.0;              /* AC power */
					sunup[i] = 0;
				}
			
				if ( wf.snow <= 0 || wf.snow > 150 )
					alb = 0.2;   /* Set Albedo, fixed value, as in pyWatts (c.helm)*/
				else
					alb = 0.6;

				solarpos_v0(yr,mn,dy,12,0.0,lat,lng,tz,sunn);/* Find sunrise and sunset */
				sunrise = sunn[4];       /* In local standard times */
				sunset = sunn[5];        /* not corrected for refraction */
			
				beghr = (int)sunrise + 24;    /* Add an offset since sunrise can be negative */
				endhr = (int)(sunset - 0.01) + 24; /* Add an offset to make it track with sunrise */
			
				if( sunset - sunrise > 0.01 )
				{
					for( i=beghr;i<=endhr;i++ )
						sunup[i%24] = 1;           /* Assign value of 1 for daytime hrs */
				}

				beghr %= 24;                     /* Remove offsets */
				endhr %= 24;

				if( sunrise < 0.0 )
					sunrise += 24.0;    /* Translate to clock hours */
				if( sunset > 24.0 )
					sunset -= 24.0;

				for( i=0;i<=23;i++ )
				{
					if( sunup[i] )
					{
						minute = 30.0;    /* Assume the midpoint of the hour,except... */
						if( beghr != endhr )
						{
							if( i == beghr )     /* Find effective time during hr */
								minute = 60.0*( 1.0 - 0.5*( (double)i + 1.0 - sunrise ) );
							else if( i == endhr )
								minute = 60.0*0.5*( sunset - (double)i );
						
							solarpos_v0(yr,mn,dy,i,minute,lat,lng,tz,sunn);
						}
						else if( i == beghr && fabs(sunset-sunrise) > 0.01 ) /* Same sunup/sunset hour */
						{  
							/* Fudge the azimuth and zenith as the mean of sunup periods */
							if( sunset > sunrise )  /* Rises and sets in same winter hour */
							{                    /* For zenith at mid-height */
								minute = 60.0*( sunrise + 0.25*( sunset - sunrise ) - (double)i );
								solarpos_v0(yr,mn,dy,i,minute,lat,lng,tz,sunn);
								tmp = sunn[1];        /* Save zenith */
															/* For azimuth at midpoint */
								minute = 60.0*( sunrise + 0.5*( sunset - sunrise ) - (double)i );
								solarpos_v0(yr,mn,dy,i,minute,lat,lng,tz,sunn);
								sunn[1] = tmp;
							}
							else     /* Sets and rises in same summer hour */
							{
								tmp = 0.0;
								tmp2 = 0.0;
								minute = 60.0*( 1.0 - 0.5*( (double)i + 1.0 - sunrise ) );
								solarpos_v0(yr,mn,dy,i,minute,lat,lng,tz,sunn);
								tmp += sunn[1];
								if( sunn[0]/DTOR < 180.0 )
									sunn[0] = sunn[0] + 360.0*DTOR;
								tmp2 += sunn[0];
								minute = 60.0*0.5*( sunset - (double)i );
								solarpos_v0(yr,mn,dy,i,minute,lat,lng,tz,sunn);
								tmp += sunn[1];
								tmp2 += sunn[0];
								sunn[1] = tmp/2.0;    /* Zenith angle */
								sunn[0] = tmp2/2.0;   /* Azimuth angle */
								if( sunn[0]/DTOR > 360.0 )
									sunn[0] = sunn[0] - 360.0*DTOR;
							}
						}
						else  /* Midnight sun or not a sunrise/sunup sunset hour */
							solarpos_v0(yr,mn,dy,i,minute,lat,lng,tz,sunn);
                    
						incident2(mode,tilt,sazm,rlim,sunn[1],sunn[0],angle); /* Calculate incident angle */
						poa[i] = perez( dn[i],df[i],alb,angle[0],angle[1],sunn[1] ); /* Incident solar radiation */
						tpoa[i] = transpoa( poa[i],dn[i],angle[0]);  /* Radiation transmitted thru module cover */

					}                    /* End of sunup[i] if loop */
				}                       /* End of for i++ loop (24 hours)*/

				celltemp(inoct,height,poa,ambt,wind,pvt);
				dcpowr(reftem,refpwr,pwrdgr,tmloss,tpoa,pvt,dc);

				dctoac(pcrate,efffp,dc,ac);

				for (i=0; i<24 && cur_hour < 8760; i++)
				{
					p_dn[cur_hour] = (ssc_number_t) dn[i];
					p_df[cur_hour] = (ssc_number_t) df[i];
					p_tamb[cur_hour] = (ssc_number_t) ambt[i];
					p_wspd[cur_hour] = (ssc_number_t) wind[i];

					p_dc[cur_hour] = (ssc_number_t) dc[i];
					p_ac[cur_hour] = (ssc_number_t) ac[i];
					p_tcell[cur_hour] = (ssc_number_t) pvt[i];
					p_poa[cur_hour] = (ssc_number_t) poa[i];

					p_sunup[cur_hour] = (ssc_number_t) sunup[i];
				
					cur_hour++;

		
				}
			}
		}
			
	}
};


DEFINE_MODULE_ENTRY( pvwattsv0, "PVWatts V.0 - TMY2 pvwatts version v1, consistent with online tool.", 0 )
