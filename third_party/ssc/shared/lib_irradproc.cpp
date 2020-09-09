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

#include <iomanip>
#include <iostream>
#include <limits>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include "lib_irradproc.h"
#include "lib_pv_incidence_modifier.h"
#include "lib_util.h"
#include "lib_weatherfile.h"

static const int __nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

/// Compute the Julian day of year
static int julian(int yr,int month,int day)    
{
	int i=1,jday=0,k;

	if( yr%4 == 0 )                      /* For leap years */
		k = 1;
	else
		k = 0;

	while( i < month )
		{
		jday = jday + __nday[i-1];
		i++;
		}
	if( month > 2 )
		jday = jday + k + day;
	else
		jday = jday + day;
	return(jday);
}
/// Compute the day of year
static int day_of_year( int month, int day_of_month ) /* returns 1-365 */
{
	int i=1,iday=0;

	while ( i < month )
		iday += __nday[i++ - 1];

	return iday + day_of_month;
}

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9])
{
/* 
	Revised 5/15/98. Replaced algorithm for solar azimuth with one by Iqbal
	so latitudes below the equator are correctly handled. Also put in checks
	to allow an elevation of 90 degrees without crashing the program and prevented
	elevation from exceeding 90 degrees after refraction correction.
 */

	int jday,delta,leap;                           /* Local variables */
	double zulu,jd,time,mnlong,mnanom,
			eclong,oblqec,num,den,ra,dec,gmst,lmst,ha,elv,azm,refrac,
			E,ws,sunrise,sunset,Eo,tst;
	double arg,hextra,Gon,zen;

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
	mnlong = fmod((double)mnlong,360.0);         /* Finds doubleing point remainder */
	if( mnlong < 0.0 )
		mnlong = mnlong + 360.0;          /* Mean longitude between 0-360 deg */

	mnanom = 357.528 + 0.9856003*time;
	mnanom = fmod((double)mnanom,360.0);
	if( mnanom < 0.0 )
		mnanom = mnanom + 360.0;
	mnanom = mnanom*DTOR;             /* Mean anomaly between 0-2pi radians */

	eclong = mnlong + 1.915*sin(mnanom) + 0.020*sin(2.0*mnanom);
	eclong = fmod((double)eclong,360.0);
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
	gmst = fmod((double)gmst,24.0);
	if( gmst < 0.0 )
		gmst = gmst + 24.0;         /* Greenwich mean sidereal time in hours */

	lmst = gmst + lng/15.0;
	lmst = fmod((double)lmst,24.0);
	if( lmst < 0.0 )
		lmst = lmst + 24.0;
	lmst = lmst*15.0*DTOR;         /* Local mean sidereal time in radians */

	ha = lmst - ra;
	if( ha < -M_PI )
		ha = ha + 2*M_PI;
	else if( ha > M_PI )
		ha = ha - 2*M_PI;             /* Hour angle in radians between -pi and pi */

	lat = lat*DTOR;                /* Change latitude to radians */

	arg = sin(dec)*sin(lat) + cos(dec)*cos(lat)*cos(ha);  /* For elevation in radians */
	if( arg > 1.0 )
		elv = M_PI/2.0;
	else if( arg < -1.0 )
		elv = -M_PI/2.0;
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

		if( ( ha <= 0.0 && ha >= -M_PI) || ha >= M_PI )
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
	if (arg >= 1.0)  // No sunrise, continuous nights
	{
		ws = 0.0;                        
		sunrise = 100.0; //make sunrise and sunset sufficiently large that even if they get rolled by 24 hours, they're still out of the bounds 0-24
		sunset = -100.0;
	}
	else if (arg <= -1.0) // No sunset, continuous days
	{
		ws = M_PI;                          
		sunrise = -100.0; //make sunrise and sunset sufficiently large that even if they get rolled by 24 hours, they're still out of the bounds 0-24
		sunset = 100.0;
	}
	else
	{
		ws = acos(arg); // Sunrise hour angle in radians
		// Sunrise and sunset in local standard time
		sunrise = 12.0 - (ws / DTOR) / 15.0 - (lng / 15.0 - tz) - E; //sunrise in units of hours (e.g. 5.25 = 5:15 am)
		sunset = 12.0 + (ws / DTOR) / 15.0 - (lng / 15.0 - tz) - E; //sunset in units of hours (e.g. 18.75 = 6:45 pm)
		//now a bunch of error checks to try to correctly catch weird behavior
		//both sunrise and sunset may be shifted by 24 hours (example: Fiji- positive tz negative lng), if so, roll them both back
		if (sunrise > 24.0 && sunset > 24.0)
		{
			sunrise -= 24.0;
			sunset -= 24.0;
		}
		//no examples of the opposing case, but let's catch it anyways, just in case
		if (sunrise < 0.0 && sunset < 0.0)
		{
			sunrise += 24.0;
			sunset += 24.0;
		}
	}

	Eo = 1.00014 - 0.01671*cos(mnanom) - 0.00014*cos(2.0*mnanom);  /* Earth-sun distance (AU) */
	Eo = 1.0/(Eo*Eo);                    /* Eccentricity correction factor */

	tst = hour + minute/60.0 + (lng/15.0 - tz) + E;  /* True solar time (hr) */
	
	/* 25aug2011 apd: addition of calculation of horizontal extraterrestrial irradiance */
	zen = 0.5*M_PI - elv;
	Gon = 1367*(1+0.033*cos( 360.0/365.0*day_of_year(month,day)*M_PI/180 )); /* D&B eq 1.4.1a, using solar constant=1367 W/m2 */
	if (zen > 0 && zen < M_PI/2) /* if sun is up */
		hextra = Gon*cos(zen); /* elevation is incidence angle (zen=90-elv) with horizontal */
	else if (zen == 0)
		hextra = Gon;
	else
		hextra = 0.0;

	sunn[0] = azm;                        /* Variables returned in array sunn[] */
	sunn[1] = zen;               /*  Zenith */
	sunn[2] = elv;
	sunn[3] = dec;
	sunn[4] = sunrise;
	sunn[5] = sunset;
	sunn[6] = Eo;
	sunn[7] = tst;
	sunn[8] = hextra;
}


void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, bool force_to_stow, double stow_angle_deg, double angle[5])
{
	// Azimuth angles are for N=0 or 2pi, E=pi/2, S=pi, and W=3pi/2.  8/13/98

	/* Local variables: rot is the angle that the collector is rotated about the
	axis when viewed from the raised end of the 1-axis tracker. If rotated
	counter clockwise the angle is negative. Range is -180 to +180 degrees.
	When xsazm = azm : rot = 0, tilt = xtilt, and sazm = xsazm = azm  */

	double arg,inc=0,xsazm,xtilt,rot=0,btdiff=0;

	if (mode == 4)
		mode = 0; //treat timeseries tilt as fixed tilt for each timestep

	switch ( mode )
		{
		case 0:              /* Fixed-Tilt, */
		case 3:              /* or Azimuth Axis*/
			tilt = tilt*DTOR;    /* Change tilt and surface azimuth to radians */
			sazm = (mode==0) ? sazm*DTOR : azm; /* either fixed surface azimuth or solar azimuth */
			arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
			rot = 0;
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
				if( xsazm <= M_PI )
					{
					if( azm <= xsazm + M_PI )
						rot = azm - xsazm;
					else
						rot = azm - xsazm - 2.0*M_PI;
					}
				else        /* For xsazm > pi */
					{
					if( azm >= xsazm - M_PI )
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
					rot = -M_PI/2.0;
				else if( arg > 99999.9 )
					rot = M_PI/2.0;
				else
					rot = atan(arg);
								/* Put rot in II or III quadrant if needed */
				if( xsazm <= M_PI )
					{
					if( azm > xsazm && azm <= xsazm + M_PI )
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
					if( azm < xsazm && azm >= xsazm - M_PI )
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
	  /*    printf("rot=%6.1f azm=%6.1f xsazm=%6.1f xtilt=%6.1f zen=%6.1f\n",rot/DTOR,azm/DTOR,xsazm/DTOR,xtilt/DTOR,zen/DTOR);  */

			if( rot < -rlim ) /* Do not let rotation exceed physical constraints */
				rot = -rlim;
			else if( rot > rlim )
				rot = rlim;

			//optionally force the tracker to a "stow" angle if specified
			if (force_to_stow)
			{
				rot = stow_angle_deg * DTOR;
				//do not report angle difference for backtracking if forced to stow, since this isn't backtracking
			}

			// apd: added 21jan2012 to enable backtracking for 1 axis arrays using 3D iterative method
			// coded originally by intern M.Kasberg summer 2011
			else if ( en_backtrack )
			{
				// find backtracking rotation angle
				double backrot = backtrack( azm*180/M_PI, zen*180/M_PI, // solar azimuth, zenith (deg)
					tilt, sazm, // axis tilt, axis azimuth (deg)
					rlim*180/M_PI, gcr, // rotation limit, GCR
					rot*180/M_PI ); // ideal rotation angle

				btdiff = backrot - rot*180/M_PI; // log the difference (degrees)
				btdiff *= M_PI/180; // convert output to radians
				rot = backrot * M_PI/180; // convert backtracked rotation angle to radians
			}


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
				else if( rot < -0.5*M_PI )
					sazm = xsazm - M_PI - asin(arg);
				else if( rot > 0.5*M_PI )
					sazm = xsazm + M_PI - asin(arg);
				else
					sazm = asin(arg) + xsazm;
				if( sazm > 2.0*M_PI )       /* Keep between 0 and 2pi */
					sazm = sazm - 2.0*M_PI;
				else if( sazm < 0.0 )
					sazm = sazm + 2.0*M_PI;
				}
		/* printf("zen=%6.1f azm-sazm=%6.1f tilt=%6.1f arg=%7.4f\n",zen/DTOR,(azm-sazm)/DTOR,tilt/DTOR,arg); */
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
			rot = 0.0;
			break;
		}
	angle[0] = inc;           /* Variables returned in array angle[] */
	angle[1] = tilt;
	angle[2] = sazm;
	angle[3] = rot;
	angle[4] = btdiff;
}

#define SMALL 1e-6

void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be null */ )
{
/* added aug2011 by aron dobos. Defines Hay, Davies, Klutcher, Reindl model for diffuse irradiance on a tilted surface
	
	List of Parameters Passed to Function:
	hextra = extraterrestrial irradiance on horizontal surface (W/m2)
	dn     = direct normal radiation (W/m2)
	df     = diffuse horizontal radiation (W/m2)
	alb    = surface albedo (decimal fraction)
	inc    = incident angle of direct beam radiation to surface in radians
	tilt   = surface tilt angle from horizontal in radians
	zen    = sun zenith angle in radians

	Variable Returned
	poa    = plane-of-array irradiances (W/m2)
				poa[0]: incident beam
				poa[1]: incident sky diffuse
				poa[2]: incident ground diffuse 
								
	diffc   = diffuse components, if an array is provided
				diffc[0] = isotropic
				diffc[1] = circumsolar
				diffc[2] = horizon brightening*/

	double hb = dn*cos(zen); /* beam irradiance on horizontal */
	double ht = hb+df; /* total irradiance on horizontal */
	if (ht < SMALL) ht = SMALL;
	if (hextra < SMALL) hextra = SMALL;

	double Rb = cos(inc)/cos(zen); /* ratio of beam on surface to beam on horizontal (D&B eq 1.8.1) */
	double Ai = hb/hextra; /* anisotropy index, term for forward scattering circumsolar diffuse (D&B eq 2.16.3) */
	double f = sqrt(hb/ht); /* modulating factor for horizontal brightening correction */
	double s3 = pow( sin( tilt*0.5 ), 3 ); /* horizontal brightening correction */

	/* see ESTIMATING DIFFUSE RADIATION ON HORIZONTAL SURFACES AND TOTAL RADIATION ON TILTED SURFACES
		 Master's Thesis, Douglas T Reindl, 1988, U.Wisc-Madison, Solar Energy Laboratory, http://sel.me.wisc.edu/publications/theses/reindl88.zip */

	double cir = df*Ai*Rb;
	double iso = df*(1-Ai)*0.5*(1+cos(tilt));
	double isohor = df*(1.0-Ai)*0.5*(1.0+cos(tilt))*(1.0+f*s3);

	poa[0] = dn*cos(inc);
	poa[1] = isohor+cir;
	poa[2] = (hb+df)*alb*(1.0-cos(tilt))/2.0;

	//prevent from returning negative poa values, added by jmf 7/28/14
	if (poa[0] < 0) poa[0] = 0;
	if (poa[1] < 0) poa[1] = 0;
	if (poa[2] < 0) poa[2] = 0;
	
	if (diffc != 0)
	{
		diffc[0] = iso;
		diffc[1] = cir;
		diffc[2] = isohor-iso;
	}
}

double Min( double v1, double v2){

	// Check if both are NAN

	if( v1 != v1 && v2!=v2 ) return NAN;

	if( v1 <= v2) return v1;
	else return v2;
}
double Max( double v1, double v2){

	// Check if both are NAN

	if( v1 != v1 && v2!=v2 ) return NAN;

	if( v1 >= v2) return v1;
	else return v2;
}

double GTI_DIRINT( const double poa[3], const double inc[3], double zen, double tilt, double ext, double alb, int doy, double tDew, double elev, double& dnOut, double& dfOut, double& ghOut, double poaCompOut[3]){
	
	double diff = 1E6;
	double bestDiff = 1E6;
	double Ktp=0;
	double GTI[] = { poa[0], poa[1], poa[2] };

	double Ci[30] = {1., 1., 1., 0.5, 0.5,
					 0.5, 0.5, 0.5, 0.5, 0.5,
					 0.25, 0.25, 0.25, 0.25, 0.25, 
					 0.25, 0.25, 0.25, 0.25, 0.25,
					 0.125, 0.125, 0.125, 0.125, 0.125, 
					 0.125, 0.125, 0.125, 0.125, 0.125};

	double poa_tmp[3], diffc_tmp[3], poaBest[3] = {0, 0, 0};
	
	// Begin iterative solution for Kt
//	double Io = 1367.0 * (1.0 + 0.033 * cos(0.0172142 * doy));    // Extraterrestrial dn (Taken from DIRINT Model)
	double cz = cos(zen);
	int i = 0;
	
	while (fabs(diff) > 1.0 && i++ < 30 ){

		// Calculate Kt using GTI and Eq. 2
//		double Kt_inc = GTI[1] / (Io * Max(0.065, cos(inc[1])));

		//Calculate DNI using Kt and DIRINT Eq.s
		double dn_tmp;
		double Ktp_tmp = ModifiedDISC( GTI, inc, tDew, elev, doy, dn_tmp);

		//Calculate DHI using Eq. 3
		double df_tmp = GTI[1] * Max( 0.065, cz) / Max(0.065, cos(inc[1])) - dn_tmp*cz;
		// if( df_tmp < 0 ) df_tmp = 0; //jmf removed 11/30/18 to allow error to be reported by poaDecomp if calculated dn is negative

		//Model POA using Perez model and find diff from GTI (Model - GTI)
		perez( ext, dn_tmp, df_tmp, alb, inc[1], tilt, zen, poa_tmp, diffc_tmp );

		//Compare modeled POA to measured POA
		diff = ( poa_tmp[0] + poa_tmp[1] + poa_tmp[2]) - poa[1];   

		//Check for best Difference. If found, save results
		if ( fabs(diff) < fabs(bestDiff)){
			bestDiff = diff;
			Ktp = Ktp_tmp;
			dnOut = dn_tmp;
			dfOut = df_tmp;
			poaBest[0] = poa_tmp[0];
			poaBest[1] = poa_tmp[1];
			poaBest[2] = poa_tmp[2];
		}

		// Adjust GTI using Eq. 4
		// Apply the same change to previous/ subsequent GTI's as well (based on Bill's email)
		GTI[0] = Max( 1.0, GTI[0] - Ci[i] * diff);
		GTI[1] = Max( 1.0, GTI[1] - Ci[i] * diff);
		GTI[2] = Max( 1.0, GTI[2] - Ci[i] * diff);

	}

	poaCompOut[0] = poaBest[0];
	poaCompOut[1] = poaBest[1];
	poaCompOut[2] = poaBest[2];

	ghOut = dnOut * cos(inc[1]) + dfOut;

	return Ktp;
}

int poaDecomp( double , double angle[], double sun[], double alb, poaDecompReq *pA, double &dn, double &df, double &gh, double poa[3], double diffc[3]){

	int errorcode = 0; //code to return whether the decomposition method succeeded or failed

	/* Decomposes POA into direct normal and diffuse irradiances */

	double r90(M_PI/2), r80( 80.0/180*M_PI ), r65(65.0/180*M_PI);

	if ( angle[0] < r90 ){  // Check if incident angle if greater than 90 degrees
		
		double gti[] = { pA->POA[ pA->i-1 ], pA->POA[ pA->i ],pA->POA[ pA->i+1 ]};
		double inc[] = { pA->inc[ pA->i-1 ], pA->inc[ pA->i ], pA->inc[ pA->i+1 ]};

		GTI_DIRINT( gti, inc, sun[1], angle[1], sun[8], alb, pA->doy, pA->tDew, pA->elev, dn, df, gh, poa );

	} else {

		size_t stepsInDay = 24;
		if( pA->stepScale == 'm'){
			stepsInDay *= 60 / (unsigned int)pA->stepSize;
		}
		 
		size_t noon = pA->dayStart + stepsInDay/2;
		size_t start, stop;
		// Check for a morning value or evening, set looping bounds accordingly
		if( pA->i < noon ){ // Calculate morning value
			start = pA->dayStart;
			stop = noon;
		} else {
			start = noon;
			stop = pA->dayStart + stepsInDay;
		}


		// Determine an average Kt prime value
		int count = 0;
		double avgKtp = 0;

		for( size_t j = start; j < stop; j++ ){


			if( (pA->inc[j] < r80) && (pA->inc[j] > r65) ){
				count++;
				double gti[] = { pA->POA[ j-1 ], pA->POA[ j ], pA->POA[ j+1 ]};
				double inc[] = { pA->inc[ j-1 ], pA->inc[ j ], pA->inc[ j+1 ]};

				double dnTmp, dfTmp, ghTmp, poaTmp[3];
				avgKtp += GTI_DIRINT( gti, inc, pA->zen[j], pA->tilt[j], pA->exTer[j], alb, pA->doy, pA->tDew, pA->elev, dnTmp, dfTmp, ghTmp, poaTmp );
			}
		}

		avgKtp /= count;

		//Calculate Kt
		double am = Min(15.25, 1.0 / (cos(sun[1]) + 0.15 * (pow(93.9 - sun[1]*180/M_PI, -1.253)))); // air mass
        double ktpam = am * exp(-0.0001184 * pA->elev);
		double Kt = avgKtp *( 1.031 * exp( -1.4/ (0.9 + 9.4/ktpam) ) + 0.1);

		//Calculate DNI using DIRINT
		double Kt_[3]  = {-999, Kt,               -999 };
		double Ktp_[3] = {-999, avgKtp,           -999 };
		double gti[3]  = {-999, pA->POA[ pA->i ], -999 };
		double zen[3]  = {-999, sun[1]          , -999 }; // Might need to be Zenith angle instead of inciden

		ModifiedDISC( Kt_, Ktp_, gti, zen, pA->tDew, pA->elev, pA->doy, dn);
		
		// Calculate DHI and GHI
		double ct = cos(angle[1]);
		df = (2* pA->POA[pA->i] - dn*cos(sun[1])*alb*(1-ct)) / (1 + ct + alb*(1-ct)) ;
		gh = dn * cos( angle[0] ) + df;

		// Get component poa from Perez
		perez( sun[8], dn, df, alb, angle[0], angle[1], sun[1], poa, diffc );

	}

	//Check for bad values and return an error code as applicable
	if (gh < 0)
	{
		gh = 0;
		errorcode = 42;
	}
	if (df < 0) //check for df before gh because gh is only calculated using dn and df, so is least likely to be the actual culprit
	{
		df = 0;
		errorcode = 41;
	}
	if (dn < 0) //check for dn last because it is the most likely culprit of the problem
	{
		dn = 0;
		errorcode = 40;
	}

	return errorcode;
}

void isotropic( double , double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] )
{
/* added aug2011 by aron dobos. Defines isotropic sky model for diffuse irradiance on a tilted surface
	
	List of Parameters Passed to Function:
	hextra = extraterrestrial irradiance on horizontal surface (W/m2) (unused for isotropic sky)
	dn     = direct normal radiation (W/m2)
	df     = diffuse horizontal radiation (W/m2)
	alb    = surface albedo (decimal fraction)
	inc    = incident angle of direct beam radiation to surface in radians
	tilt   = surface tilt angle from horizontal in radians
	zen    = sun zenith angle in radians

	Variable Returned
	poa    = plane-of-array irradiances (W/m2)
				poa[0]: incident beam
				poa[1]: incident sky diffuse
				poa[2]: incident ground diffuse 
				
	diffc   = diffuse components, if an array is provided
				diffc[0] = isotro	angle[]  = array of elements to return angles to calling function
	angle[0] = inc  = incident angle in radians
	angle[1] = tilt = tilt angle of surface from horizontal in radians
	angle[2] = sazm = surface azimuth in radians, measured east from north
	angle[3] = rot = tracking axis rotation angle in radians, measured from surface normal of unrotating axis (only for 1 axis trackers)
	angle[4] = btdiff = (rot - ideal_rot) will be zero except in case of backtracking for 1 axis trackingpic
				diffc[1] = circumsolar
				diffc[2] = horizon brightening
				*/

	poa[0] = dn*cos(inc);
	poa[1] = df*(1.0+cos(tilt))/2.0;
	poa[2] = (dn*cos(zen)+df)*alb*(1.0-cos(tilt))/2.0;

	//prevent from returning negative poa values, added by jmf 7/28/14
	if (poa[0] < 0) poa[0] = 0;
	if (poa[1] < 0) poa[1] = 0;
	if (poa[2] < 0) poa[2] = 0;

	if (diffc != 0)
	{
		diffc[0] = poa[1];
		diffc[1] = 0; // no circumsolar
		diffc[2] = 0; // no horizon brightening
	}
}

void perez( double , double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] )
{
/* 
	Based on original FORTRAN program by Howard Bisner.
	Total POA is poa[0]+poa[1]+poa[2]
	Modified aug2011 by aron dobos to split out beam, diffuse, ground for output.
	Modified 6/10/98 so that for zenith angles between 87.5 and 90.0 degrees,
	the diffuse radiation is treated as isotropic instead of 0.0.

*/

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
	double B2=0.000005534,
		EPS,T,D,DELTA,A,B,C,ZH,F1,F2,COSINC,x;
	double CZ,ZC,ZENITH,AIRMASS;

	int i;

	if ( diffc != 0 )
		diffc[0] = diffc[1] = diffc[2] = 0.0;

	if ( dn < 0.0 )           /* Negative values may be measured if cloudy */
		dn = 0.0;

	if ( zen < 0.0 || zen > 1.5271631 ) /* Zen not between 0 and 87.5 deg */
		{
		if( df < 0.0 )
			df = 0.0;
		if ( cos(inc) > 0.0 && zen < 1.5707963 )  /* Zen between 87.5 and 90 */
			{                                      /* and incident < 90 deg   */
			poa[0] = dn * cos(inc);
			poa[1] = df*( 1.0 + cos(tilt) )/2.0;
			poa[2] = 0.0;

			if (diffc != 0) diffc[0] = poa[1]; /* isotropic only */
			return;
			}
		else
			{
			poa[0] = 0;
			poa[1] = df*( 1.0 + cos(tilt) )/2.0;   /* Isotropic diffuse only */
			poa[2] = 0.0;
			
			if (diffc != 0) diffc[0] = poa[1]; /* isotropic only */
			return;
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
				poa[0] = dn*cos(inc);
				poa[1] = 0.0;
				poa[2] = 0.0;
				return;
				}
			else
				{
				poa[0] = 0;
				poa[1] = 0;
				poa[2] = 0;
				return;
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

			// apd 7oct2011: reorganized from original pvwatts code
			// see duffie&beckman 2006, eqn 2.16.14
			A = D*(1-F1)*( 1.0 + cos(tilt) )/2.0; // isotropic diffuse
			B = D*F1*ZC/ZH; // circumsolar diffuse
			C = D*F2*sin(tilt); // horizon brightness term
						
			if (diffc != 0)
			{
				diffc[0] = A;
				diffc[1] = B;
				diffc[2] = C;
			}
			
			// original PVWatts: poa = A + F1*B + F2*C + alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0 + dn*ZC;
			poa[0] = dn*ZC; // beam
			poa[1] = A + B + C; // total sky diffuse
			poa[2] = alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0; // ground diffuse
			return;
			}
		}
}

void irrad::setup()
{
	year = month = day = hour = -999;
	minute = delt = latitudeDegrees = longitudeDegrees = timezone = -999;
	globalHorizontal = directNormal = diffuseHorizontal = -999;

	for (int i = 0; i < 9; i++) {
		sunAnglesRadians[i] = std::numeric_limits<double>::quiet_NaN();
	}
	surfaceAnglesRadians[0] = surfaceAnglesRadians[1] = surfaceAnglesRadians[2] = surfaceAnglesRadians[3] = surfaceAnglesRadians[4] = std::numeric_limits<double>::quiet_NaN();
	planeOfArrayIrradianceFront[0] = planeOfArrayIrradianceFront[1] = planeOfArrayIrradianceFront[2] = diffuseIrradianceFront[0] = diffuseIrradianceFront[1] = diffuseIrradianceFront[2] = std::numeric_limits<double>::quiet_NaN();
	planeOfArrayIrradianceRear[0] = planeOfArrayIrradianceRear[1] = planeOfArrayIrradianceRear[2] = diffuseIrradianceRear[0] = diffuseIrradianceRear[1] = diffuseIrradianceRear[2] = std::numeric_limits<double>::quiet_NaN();
	timeStepSunPosition[0] = timeStepSunPosition[1] = timeStepSunPosition[2] = -999;
	planeOfArrayIrradianceRearAverage = 0;

	calculatedDirectNormal = directNormal;
	calculatedDiffuseHorizontal = 0.0;
}
irrad::irrad()
{
	setup();
}
irrad::irrad(weather_record wf, weather_header hdr, 
	int skyModelIn, int radiationModeIn, int trackModeIn,
	bool useWeatherFileAlbedo, bool instantaneousWeather, bool backtrackingEnabled, bool forceToStowIn,
	double dtHour, double tiltDegreesIn, double azimuthDegreesIn, double trackerRotationLimitDegreesIn, double stowAngleDegreesIn, 
	double groundCoverageRatioIn, std::vector<double> monthlyTiltDegrees, std::vector<double> userSpecifiedAlbedo, 
	poaDecompReq * poaAllIn) : 
	skyModel(skyModelIn), radiationMode(radiationModeIn), trackingMode(trackModeIn), enableBacktrack(backtrackingEnabled), forceToStow(forceToStowIn),
	delt(dtHour), tiltDegrees(tiltDegreesIn), surfaceAzimuthDegrees(azimuthDegreesIn), rotationLimitDegrees(trackerRotationLimitDegreesIn),
	stowAngleDegrees(stowAngleDegreesIn), groundCoverageRatio(groundCoverageRatioIn), poaAll(poaAllIn)
{
	setup();
	int month_idx = wf.month - 1;
	if (useWeatherFileAlbedo && std::isfinite(wf.alb) && wf.alb > 0 && wf.alb < 1) {
		albedo = wf.alb;
	}
	else if (month_idx >= 0 && month_idx < 12) {
		albedo = userSpecifiedAlbedo[month_idx];
	}
	
	set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute,
		instantaneousWeather ? IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET : dtHour);
	set_location(hdr.lat, hdr.lon, hdr.tz);
	set_sky_model(skyModel, albedo);

	if (radiationMode == irrad::DN_DF) set_beam_diffuse(wf.dn, wf.df);
	else if (radiationMode == irrad::DN_GH) set_global_beam(wf.gh, wf.dn);
	else if (radiationMode == irrad::GH_DF) set_global_diffuse(wf.gh, wf.df);
	else if (radiationMode == irrad::POA_R) set_poa_reference(wf.poa, poaAllIn);
	else if (radiationMode == irrad::POA_P) set_poa_pyranometer(wf.poa, poaAllIn);

	if (trackingMode == TRACKING::SEASONAL_TILT) {
		tiltDegrees = monthlyTiltDegrees[month_idx];
		trackingMode = TRACKING::FIXED_TILT;
	}
}

int irrad::check()
{
	if (year < 0 || month < 0 || day < 0 || hour < 0 || minute < 0 || delt > 1) return -1;
	if ( latitudeDegrees < -90 || latitudeDegrees > 90 || longitudeDegrees < -180 || longitudeDegrees > 180 || timezone < -15 || timezone > 15 ) return -2;
	if ( radiationMode < irrad::DN_DF || radiationMode > irrad::POA_P || skyModel < 0 || skyModel > 2 ) return -3;
	if ( trackingMode < 0 || trackingMode > 4 ) return -4;
	if ( radiationMode == irrad::DN_DF && (directNormal < 0 || directNormal > irrad::irradiationMax || diffuseHorizontal < 0 || diffuseHorizontal > 1500)) return -5;
	if ( radiationMode == irrad::DN_GH && (globalHorizontal < 0 || globalHorizontal > 1500 || directNormal < 0 || directNormal > 1500)) return -6;
	if ( albedo < 0 || albedo > 1 ) return -7;
	if ( tiltDegrees < 0 || tiltDegrees > 90 ) return -8;
	if ( surfaceAzimuthDegrees < 0 || surfaceAzimuthDegrees >= 360 ) return -9;
	if ( rotationLimitDegrees < -90 || rotationLimitDegrees > 90 ) return -10;
	if (stowAngleDegrees < -90 || stowAngleDegrees > 90) return -12;
	if ( radiationMode == irrad::GH_DF && (globalHorizontal < 0 || globalHorizontal > 1500 || diffuseHorizontal < 0 || diffuseHorizontal > 1500)) return -11;
	return 0;
}

double irrad::getAlbedo()
{
	return albedo;
}

double irrad::get_sunpos_calc_hour()
{
	return ((double)timeStepSunPosition[0]) + ((double)timeStepSunPosition[1])/60.0;
}

void irrad::get_sun( double *solazi,
	double *solzen,
	double *solelv,
	double *soldec,
	double *sunrise,
	double *sunset,
	int *sunup,
	double *eccfac,
	double *tst,
	double *hextra )
{
	if ( solazi != 0 ) *solazi = sunAnglesRadians[0] * (180/M_PI);
	if ( solzen != 0 ) *solzen = sunAnglesRadians[1] * (180/M_PI);
	if ( solelv != 0 ) *solelv = sunAnglesRadians[2] * (180/M_PI);
	if ( soldec != 0 ) *soldec = sunAnglesRadians[3] * (180/M_PI);
	if ( sunrise != 0 ) *sunrise = sunAnglesRadians[4];
	if ( sunset != 0 ) *sunset = sunAnglesRadians[5];
	if ( sunup != 0 ) *sunup = timeStepSunPosition[2];
	if ( eccfac != 0 ) *eccfac = sunAnglesRadians[6];
	if ( tst != 0 ) *tst = sunAnglesRadians[7];
	if ( hextra != 0 ) *hextra = sunAnglesRadians[8];
}

void irrad::get_angles( double *aoi,
	double *surftilt,
	double *surfazi,
	double *axisrot,
	double *btdiff )
{
	if (aoi != 0) *aoi = surfaceAnglesRadians[0] * (180 / M_PI);
	if ( surftilt != 0 ) *surftilt = surfaceAnglesRadians[1] * (180/M_PI);
	if ( surfazi != 0 ) *surfazi = surfaceAnglesRadians[2] * (180/M_PI);
	if ( axisrot != 0 ) *axisrot = surfaceAnglesRadians[3] * (180/M_PI);
	if ( btdiff != 0 ) *btdiff = surfaceAnglesRadians[4] * (180/M_PI);
}
	
void irrad::get_poa( double *beam, double *skydiff, double *gnddiff,
	double *isotrop, double *circum, double *horizon )
{
	if ( beam != 0 ) *beam = planeOfArrayIrradianceFront[0];
	if ( skydiff != 0 ) *skydiff = planeOfArrayIrradianceFront[1];
	if ( gnddiff != 0 ) *gnddiff = planeOfArrayIrradianceFront[2];
	if ( isotrop != 0 ) *isotrop = diffuseIrradianceFront[0];
	if ( circum != 0 ) *circum = diffuseIrradianceFront[1];
	if ( horizon != 0 ) *horizon = diffuseIrradianceFront[2];
}

double irrad::get_poa_rear()
{
	return planeOfArrayIrradianceRearAverage;
}

void irrad::get_irrad (double *ghi, double *dni, double *dhi){
	*ghi = globalHorizontal;
	*dni = directNormal;
	*dhi = diffuseHorizontal;
}

void irrad::set_time( int y, int m, int d, int h, double min, double delt_hr )
{
	this->year = y;
	this->month = m;
	this->day = d;
	this->hour = h;
	this->minute = min;
	this->delt = delt_hr;
}

void irrad::set_location( double latDegrees, double longDegrees, double tz )
{
	this->latitudeDegrees = latDegrees;
	this->longitudeDegrees = longDegrees;
	this->timezone = tz;
}

void irrad::set_sky_model( int sm, double alb )
{
	this->skyModel = sm;
	this->albedo = alb;
}

void irrad::set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool enBacktrack, double gcr, bool forceToStowFlag, double stowAngle )
{
	this->trackingMode = tracking;
	if (tracking == 4)
		this->trackingMode = 0; //treat timeseries tilt as fixed tilt
	this->tiltDegrees = tilt_deg;
	this->surfaceAzimuthDegrees = azimuth_deg;
	this->rotationLimitDegrees = rotlim_deg;
	this->forceToStow = forceToStowFlag;
	this->stowAngleDegrees = stowAngle;
	this->enableBacktrack = enBacktrack;
	this->groundCoverageRatio = gcr;
}
	
void irrad::set_beam_diffuse( double beam, double diffuse )
{
	this->directNormal = beam;
	this->diffuseHorizontal = diffuse;
	this->radiationMode = irrad::DN_DF;
}

void irrad::set_global_beam( double global, double beam )
{
	this->globalHorizontal = global;
	this->directNormal = beam;
	this->radiationMode = irrad::DN_GH;
}

void irrad::set_global_diffuse(double global, double diffuse)
{
	this->globalHorizontal = global;
	this->diffuseHorizontal = diffuse;
	this->radiationMode = irrad::GH_DF;
}

void irrad::set_poa_reference( double poaIrradianceFront, poaDecompReq* pA){
	this->weatherFilePOA = poaIrradianceFront;
	this->radiationMode = irrad::POA_R;
	this->poaAll = pA;
}
void irrad::set_poa_pyranometer( double poaIrradianceFront, poaDecompReq* pA ){
	this->weatherFilePOA = poaIrradianceFront;
	this->radiationMode = irrad::POA_P;
	this->poaAll = pA;
}
void irrad::set_sun_component(size_t index, double value)
{
	if (index < sizeof(sunAnglesRadians) / sizeof(sunAnglesRadians[0])) {
		sunAnglesRadians[index] = value;
	}
}

int irrad::calc()
{
	int code = check();
	if ( code < 0 )
		return -100+code;
/*
	calculates effective sun position at current timestep, with delt specified in hours

	sunAnglesRadians: results from solarpos
	timeStepSunPosition: [0]  effective hour of day used for sun position
			[1]  effective minute of hour used for sun position
			[2]  is sun up?  (0=no, 1=midday, 2=sunup, 3=sundown)
	surfaceAnglesRadians: result from incidence
	planeOfArrayIrradianceFront: result from sky model
	diff: broken out diffuse components from sky model
*/	
	double t_cur = hour + minute/60.0;

	// calculate sunrise and sunset hours in local standard time for the current day
	solarpos( year, month, day, 12, 0.0, latitudeDegrees, longitudeDegrees, timezone, sunAnglesRadians );

	double t_sunrise = sunAnglesRadians[4];
	double t_sunset = sunAnglesRadians[5];

	if (t_sunset > 24.0 && t_sunset != 100.0) //sunset is legitimately the next day but we're not in endless days, so recalculate sunset from the previous day
	{
		double sunanglestemp[9];
		if (day > 1) //simply decrement day during month
			solarpos(year, month, day - 1, 12, 0.0, latitudeDegrees, longitudeDegrees, timezone, sunanglestemp);
		else if (month > 1) //on the 1st of the month, need to switch to the last day of previous month
			solarpos(year, month - 1, __nday[month - 2], 12, 0.0, latitudeDegrees, longitudeDegrees, timezone, sunanglestemp); //month is 1-indexed and __nday is 0 indexed
		else //on the first day of the year, need to switch to Dec 31 of last year
			solarpos(year - 1, 12, 31, 12, 0.0, latitudeDegrees, longitudeDegrees, timezone, sunanglestemp);
		//on the last day of endless days, sunset is returned as 100 (hour angle too large for calculation), so use today's sunset time as a proxy
		if (sunanglestemp[5] == 100.0)
			t_sunset -= 24.0;		
		//if sunset from yesterday WASN'T today, then it's ok to leave sunset > 24, which will cause the sun to rise today and not set today
		else if (sunanglestemp[5] >= 24.0)
			t_sunset = sunanglestemp[5] - 24.0;
	}

	if (t_sunrise < 0.0 && t_sunrise != -100.0) //sunrise is legitimately the previous day but we're not in endless days, so recalculate for next day
	{
		double sunanglestemp[9];
		if (day < __nday[month - 1]) //simply increment the day during the month, month is 1-indexed and __nday is 0-indexed
			solarpos(year, month, day + 1, 12, 0.0, latitudeDegrees, longitudeDegrees, timezone, sunanglestemp);
		else if (month < 12) //on the last day of the month, need to switch to the first day of the next month
			solarpos(year, month + 1, 1, 12, 0.0, latitudeDegrees, longitudeDegrees, timezone, sunanglestemp);
		else //on the last day of the year, need to switch to Jan 1 of the next year
			solarpos(year + 1, 1, 1, 12, 0.0, latitudeDegrees, longitudeDegrees, timezone, sunanglestemp);
		//on the last day of endless days, sunrise would be returned as -100 (hour angle too large for calculations), so use today's sunrise time as a proxy
		if (sunanglestemp[4] == -100.0)
			t_sunrise += 24.0;		
		//if sunrise from tomorrow isn't today, then it's ok to leave sunrise < 0, which will cause the sun to set at the right time and not rise until tomorrow
		else if (sunanglestemp[4] < 0.0)
			t_sunrise = sunanglestemp[4] + 24.0;
	}

	// recall: if delt <= 0.0, do not interpolate sunrise and sunset hours, just use specified time stamp
	// time step encompasses the sunrise
	if ( delt > 0 && t_cur >= t_sunrise - delt/2.0 && t_cur < t_sunrise + delt/2.0 )
	{
		double t_calc = (t_sunrise + (t_cur+delt/2.0))/2.0; // midpoint of sunrise and end of timestep
		int hr_calc = (int)t_calc;
		double min_calc = (t_calc-hr_calc)*60.0;

		timeStepSunPosition[0] = hr_calc;
		timeStepSunPosition[1] = (int)min_calc;
				
		solarpos( year, month, day, hr_calc, min_calc, latitudeDegrees, longitudeDegrees, timezone, sunAnglesRadians );

		timeStepSunPosition[2] = 2;				
	}
	// timestep encompasses the sunset
	else if ( delt > 0 && t_cur > t_sunset - delt/2.0 && t_cur <= t_sunset + delt/2.0 )
	{
		double t_calc = ( (t_cur-delt/2.0) + t_sunset )/2.0; // midpoint of beginning of timestep and sunset
		int hr_calc = (int)t_calc;
		double min_calc = (t_calc-hr_calc)*60.0;

		timeStepSunPosition[0] = hr_calc;
		timeStepSunPosition[1] = (int)min_calc;
				
		solarpos( year, month, day, hr_calc, min_calc, latitudeDegrees, longitudeDegrees, timezone, sunAnglesRadians );

		timeStepSunPosition[2] = 3;
	}
	// timestep is not sunrise nor sunset, but sun is up  (calculate position at provided t_cur)
	else if ( (t_sunrise < t_sunset && t_cur >= t_sunrise && t_cur <= t_sunset) || //this captures normal daylight cases
		(t_sunrise > t_sunset && (t_cur <= t_sunset || t_cur >= t_sunrise)) ) //this captures cases where sunset (from previous day) is 1:30AM, sunrise 2:30AM, in arctic circle
	{				
		timeStepSunPosition[0] = hour;
		timeStepSunPosition[1] = (int)minute;
		solarpos( year, month, day, hour, minute, latitudeDegrees, longitudeDegrees, timezone, sunAnglesRadians );
		timeStepSunPosition[2] = 1;
	}
	else
	{	
		// sun is down, assign sundown values
		solarpos(year, month, day, hour, minute, latitudeDegrees, longitudeDegrees, timezone, sunAnglesRadians);
		timeStepSunPosition[0] = hour;
		timeStepSunPosition[1] = (int)minute;
		timeStepSunPosition[2] = 0;
	}

			
	planeOfArrayIrradianceFront[0]=planeOfArrayIrradianceFront[1]=planeOfArrayIrradianceFront[2] = 0;
	diffuseIrradianceFront[0]=diffuseIrradianceFront[1]=diffuseIrradianceFront[2] = 0;
	surfaceAnglesRadians[0]=surfaceAnglesRadians[1]=surfaceAnglesRadians[2]=surfaceAnglesRadians[3]=surfaceAnglesRadians[4] = 0;

	// do irradiance calculations if sun is up
	if (timeStepSunPosition[2] > 0)
	{				
		// compute incidence angles onto fixed or tracking surface
		incidence( trackingMode, tiltDegrees, surfaceAzimuthDegrees, rotationLimitDegrees, sunAnglesRadians[1], sunAnglesRadians[0], 
			enableBacktrack, groundCoverageRatio, forceToStow, stowAngleDegrees, surfaceAnglesRadians );

		if(radiationMode < irrad::POA_R){
			double hextra = sunAnglesRadians[8];
			double hbeam = directNormal*cos( sunAnglesRadians[1] ); // calculated beam on horizontal surface: sunAnglesRadians[1]=zenith
				
			// check beam irradiance against extraterrestrial irradiance
			if ( hbeam > hextra )
			{
				//beam irradiance on horizontal W/m2 exceeded calculated extraterrestrial irradiance
				return -1;
			}

			// compute beam and diffuse inputs on horizontal based on irradiance inputs mode
			if (radiationMode == irrad::DN_DF)  // Beam+Diffuse
			{
				calculatedDiffuseHorizontal = diffuseHorizontal;
				calculatedDirectNormal = directNormal;
			}
			else if (radiationMode == irrad::DN_GH) // Total+Beam
			{
				calculatedDiffuseHorizontal = globalHorizontal - hbeam;
				if (calculatedDiffuseHorizontal < 0) calculatedDiffuseHorizontal = 0;
				calculatedDirectNormal = directNormal;
			}
			else if (radiationMode == irrad::GH_DF) //Total+Diffuse
			{
				calculatedDiffuseHorizontal = diffuseHorizontal;
				calculatedDirectNormal = (globalHorizontal - diffuseHorizontal) / cos(sunAnglesRadians[1]); //compute beam from total, diffuse, and zenith angle
				if (calculatedDirectNormal > irrad::irradiationMax) calculatedDirectNormal = irrad::irradiationMax;
				if (calculatedDirectNormal < 0) calculatedDirectNormal = 0;
			}
			else
				return -2; // just in case of a weird error


			// compute incident irradiance on tilted surface
			switch( skyModel )
			{
			case 0:
				isotropic( hextra, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, surfaceAnglesRadians[0], surfaceAnglesRadians[1], sunAnglesRadians[1], planeOfArrayIrradianceFront, diffuseIrradianceFront );
				break;
			case 1:
				hdkr( hextra, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, surfaceAnglesRadians[0], surfaceAnglesRadians[1], sunAnglesRadians[1], planeOfArrayIrradianceFront, diffuseIrradianceFront );
				break;
			default:
				perez( hextra, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, surfaceAnglesRadians[0], surfaceAnglesRadians[1], sunAnglesRadians[1], planeOfArrayIrradianceFront, diffuseIrradianceFront );
				break;
			} 
		} 
		else { // Sev 2015/09/11 - perform a POA decomp.
			int errorcode = poaDecomp( weatherFilePOA, surfaceAnglesRadians, sunAnglesRadians, albedo, poaAll, directNormal, diffuseHorizontal, globalHorizontal, planeOfArrayIrradianceFront, diffuseIrradianceFront);
			calculatedDirectNormal = directNormal;
			calculatedDiffuseHorizontal = diffuseHorizontal;
			return errorcode; //this will return 0 if successful, otherwise 40, 41, or 42 if calculated decomposed dni, dhi, or ghi are negative
		}
	} 
	else { globalHorizontal=0; directNormal=0; diffuseHorizontal=0;} //sun is below horizon

	return 0;

}

int irrad::calc_rear_side(double transmissionFactor, double groundClearanceHeight, double slopeLength)
{
	// do irradiance calculations if sun is up
	if (timeStepSunPosition[2] > 0)
	{

		double tiltRadian = surfaceAnglesRadians[1];		// The tracked angle in radians

		// Update ground clearance height for HSAT
		if (this->trackingMode == 1) {
			groundClearanceHeight = groundClearanceHeight - (0.5 * slopeLength) * sin(fabs(tiltRadian));
		}

		// System geometry
		double rowToRow = slopeLength / this->groundCoverageRatio;		// Row to row spacing between the front of one row to the front of the next row
		double clearanceGround = groundClearanceHeight;					// The normalized clearance from the bottom edge of module to ground
		double distanceBetweenRows = rowToRow - cos(tiltRadian);	    // The normalized distance from the read of module to front of module in next row
		double verticalHeight = slopeLength * sin(tiltRadian);
		double horizontalLength = slopeLength * cos(tiltRadian);

		// Determine the factors for points on the ground from the leading edge of one row of PV panels to the edge of the next row of panels behind
		std::vector<double> rearSkyConfigFactors, frontSkyConfigFactors;
		this->getSkyConfigurationFactors(rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, rearSkyConfigFactors, frontSkyConfigFactors);

		// Determine if ground is shading from direct beam radio for points on the ground from leading edge of PV panels to leading edge of next row behind
		double pvBackShadeFraction, pvFrontShadeFraction, maxShadow;
		pvBackShadeFraction = pvFrontShadeFraction = maxShadow = 0;
		std::vector<int> rearGroundShade, frontGroundShade;
		this->getGroundShadeFactors(rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, sunAnglesRadians[0], sunAnglesRadians[2], rearGroundShade, frontGroundShade, maxShadow, pvBackShadeFraction, pvFrontShadeFraction);

		// Get the rear ground GHI
		std::vector<double> rearGroundGHI, frontGroundGHI;
		this->getGroundGHI(transmissionFactor, rearSkyConfigFactors, frontSkyConfigFactors, rearGroundShade, frontGroundShade, rearGroundGHI, frontGroundGHI);

		// Calculate the irradiance on the front of the PV module (to get front reflected)
		std::vector<double> frontIrradiancePerCellrow, frontReflected;
		double frontAverageIrradiance = 0;
		getFrontSurfaceIrradiances(pvFrontShadeFraction, rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, frontGroundGHI, frontIrradiancePerCellrow, frontAverageIrradiance, frontReflected);

		// Calculate the irradiance on the back of the PV module
		std::vector<double> rearIrradiancePerCellrow;
		double rearAverageIrradiance = 0;
		getBackSurfaceIrradiances(pvBackShadeFraction, rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, rearGroundGHI, frontGroundGHI, frontReflected, rearIrradiancePerCellrow, rearAverageIrradiance);
		planeOfArrayIrradianceRearAverage = rearAverageIrradiance;
	}
	return true;
}

void irrad::getSkyConfigurationFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> & rearSkyConfigFactors, std::vector<double> & frontSkyConfigFactors)
{
	// Calculate sky configuration factors using 100 intervals
	size_t intervals = 100;
	double deltaInterval = static_cast<double>(rowToRow / intervals);
	double x = -deltaInterval / 2.0;

	for (size_t i = 0; i != intervals; i++)
	{
		x += deltaInterval;
		double angleA = atan((verticalHeight + clearanceGround) / (2.0 * rowToRow + horizontalLength - x));
		if (angleA < 0.0) {
			angleA += M_PI;
		}
		double angleB = atan(clearanceGround / (2.0 * rowToRow - x));
		if (angleB < 0.0) {
			angleB += M_PI;
		}
		double beta1 = fmax(angleA, angleB);

		double angleC = atan((verticalHeight + clearanceGround) / (rowToRow + horizontalLength - x));
		if (angleC < 0.0) {
			angleC += M_PI;
		}
		double angleD = atan(clearanceGround / (rowToRow - x));
		if (angleD < 0.0) {
			angleD += M_PI;
		}
		double beta2 = fmin(angleC, angleD);
		double beta3 = fmax(angleC, angleD);

		double beta4 = atan((verticalHeight + clearanceGround) / (horizontalLength - x));
		if (beta4 < 0.0) {
			beta4 += M_PI;
		}

		double beta5 = atan(clearanceGround / (-x));
		if (beta5 < 0.0) {
			beta5 += M_PI;
		}

		double beta6 = atan((verticalHeight + clearanceGround) / (-distanceBetweenRows - x));
		if (beta6 < 0.0) {
			beta6 += M_PI;
		}
		double sky1, sky2, sky3, skyAll;
		sky1 = sky2 = sky3 = skyAll = 0;
		if (beta2 > beta1) {
			sky1 = 0.5 * (cos(beta1) - cos(beta2));
		}
		if (beta4 > beta3) {
			sky2 = 0.5 * (cos(beta3) - cos(beta4));
		}
		if (beta6 > beta5) {
			sky3 = 0.5 * (cos(beta5) - cos(beta6));
		}
		skyAll = sky1 + sky2 + sky3;

		rearSkyConfigFactors.push_back(skyAll);
		frontSkyConfigFactors.push_back(skyAll);
	}
}

void irrad::getGroundShadeFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, double solarAzimuthRadians, double solarElevationRadians, std::vector<int> & rearGroundShade, std::vector<int> & frontGroundShade, double & maxShadow, double & pvBackSurfaceShadeFraction, double & pvFrontSurfaceShadeFraction)
{
	// calculate ground shade factors using 100 intervals
	size_t intervals = 100;
	double deltaInterval = static_cast<double>(rowToRow / intervals);
	double surfaceAzimuthAngleRadians = surfaceAnglesRadians[2];
	double shadingStart1, shadingStart2, shadingEnd1, shadingEnd2;
	shadingStart1 = shadingStart2 = shadingEnd1 = shadingEnd2 = pvBackSurfaceShadeFraction = 0;

	// Horizontal length of shadow perpindicular to row from top of module to bottom of module
	double Lh = (verticalHeight / tan(solarElevationRadians)) * cos(surfaceAzimuthAngleRadians - solarAzimuthRadians);

	//  Horizontal length of shadow perpindicular to row from top of module to ground level
	double Lhc = ((clearanceGround + verticalHeight) / tan(solarElevationRadians)) * cos(surfaceAzimuthAngleRadians - solarAzimuthRadians);

	// Horizontal length of shadow perpindicular to row from bottom of module to ground level
	double Lc = (clearanceGround / tan(solarElevationRadians)) * cos(surfaceAzimuthAngleRadians - solarAzimuthRadians);

	// Front side of PV module partially shaded, back completely shaded, ground completely shaded
	if (Lh > distanceBetweenRows) {
		pvFrontSurfaceShadeFraction = (Lh - distanceBetweenRows) / (Lh + horizontalLength);
		pvBackSurfaceShadeFraction = 1.0;
		shadingStart1 = 0.0;
		shadingEnd1 = rowToRow;
	}
	// Back side of PV module partially shaded, front completely shaded, ground completely shaded
	else if (Lh < -(rowToRow + horizontalLength)) {
		pvFrontSurfaceShadeFraction = 1.0;
		pvBackSurfaceShadeFraction = (Lh + rowToRow + horizontalLength) / (Lh + horizontalLength);
		shadingStart1 = 0.0;
		shadingEnd1 = rowToRow;
	}
	// Assume ground is partially shaded
	else
	{
		if (Lhc >= 0)
		{
			pvFrontSurfaceShadeFraction = 0.0;
			pvBackSurfaceShadeFraction = 1.0;
			double shadowStart = Lc;
			double shadowEnd = Lhc + horizontalLength;
			while (shadowStart > rowToRow)
			{
				shadowStart -= rowToRow;
				shadowEnd -= rowToRow;
			}
			shadingStart1 = shadowStart;
			shadingEnd1 = shadowEnd;
			if (shadingEnd1 > rowToRow)
			{
				shadingEnd1 = rowToRow;
				shadingStart2 = 0.0;
				shadingEnd2 = shadowEnd - rowToRow;
				// ground completely shaded
				if (shadingEnd2 > shadingStart1)
				{
					shadingStart1 = 0.0;
					shadingEnd1 = rowToRow;
				}
			}
		}
		// Shadow to front of row, either front or back might be shaded, depending on tilt and other factors
		else
		{
			double shadowStart = 0.0;
			double shadowEnd = 0.0;
			if (Lc < Lhc + horizontalLength)
			{
				pvFrontSurfaceShadeFraction = 0.0;
				pvBackSurfaceShadeFraction = 1.0;
				shadowStart = Lc;
				shadowEnd = Lhc + horizontalLength;
			}
			else
			{
				pvFrontSurfaceShadeFraction = 1.0;
				pvBackSurfaceShadeFraction = 0.0;
				shadowStart = Lhc + horizontalLength;
				shadowEnd = Lc;

			}
			while (shadowStart < 0.0)
			{
				shadowStart += rowToRow;
				shadowEnd += rowToRow;
			}

			shadingStart1 = shadowStart;
			shadingEnd1 = shadowEnd;

			if (shadingEnd1 > rowToRow)
			{
				shadingEnd1 = rowToRow;
				shadingStart2 = 0.0;
				shadingEnd2 = shadowEnd - rowToRow;
				if (shadingEnd2 > shadingStart1)
				{
					shadingStart1 = 0.0;
					shadingEnd1 = rowToRow;
				}
			}
		}

	}
	double x = -deltaInterval / 2.0;
	for (size_t i = 0; i != intervals; i++)
	{
		x += deltaInterval;
		if ((x >= shadingStart1 && x < shadingEnd1) || (x >= shadingStart2 && x < shadingEnd2))
		{
			rearGroundShade.push_back(1);
			frontGroundShade.push_back(1);
		}
		else
		{
			rearGroundShade.push_back(0);
			frontGroundShade.push_back(0);
		}
	}
	maxShadow = fmax(shadingStart1, shadingEnd1);
}

void irrad::getGroundGHI(double transmissionFactor, std::vector<double> rearSkyConfigFactors, std::vector<double> frontSkyConfigFactors, std::vector<int> rearGroundShade, std::vector<int> frontGroundShade, std::vector<double> & rearGroundGHI, std::vector<double> & frontGroundGHI)
{
	// Calculate the diffuse components of irradiance
	perez(0, calculatedDirectNormal, calculatedDiffuseHorizontal,albedo, sunAnglesRadians[1], 0.0, sunAnglesRadians[1], planeOfArrayIrradianceRear, diffuseIrradianceRear);
	double incidentBeam = planeOfArrayIrradianceRear[0];
	double isotropicDiffuse = diffuseIrradianceRear[0];
	double circumsolarDiffuse = diffuseIrradianceRear[1];

	// Sum the irradiance components for each of the ground segments to the front and rear of the front of the PV row
	for (size_t i = 0; i != 100; i++)
	{
		// Add diffuse sky component viewed by ground
		rearGroundGHI.push_back(rearSkyConfigFactors[i] * isotropicDiffuse);
		frontGroundGHI.push_back(frontSkyConfigFactors[i] * isotropicDiffuse);

		if (rearGroundShade[i] == 0)
		{
			// Add beam and circumsolar component if not shaded
			rearGroundGHI[i] += incidentBeam + circumsolarDiffuse;
		}
		else
		{
			// Add beam and circumsolar component transmitted thru module spacing if shaded
			rearGroundGHI[i] += (incidentBeam + circumsolarDiffuse) * transmissionFactor;
		}
		if (frontGroundShade[i] == 0)
		{
			frontGroundGHI[i] += incidentBeam + circumsolarDiffuse;
		}
		else
		{
			frontGroundGHI[i] += (incidentBeam + circumsolarDiffuse) * transmissionFactor;
		}
	}
}

void irrad::getFrontSurfaceIrradiances(double pvFrontShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> frontGroundGHI, std::vector<double> & frontIrradiance, double & frontAverageIrradiance, std::vector<double> & frontReflected)
{
	// front surface assumed to be glass
	double n2 = 1.526;

	size_t intervals = 100;
	double solarAzimuthRadians = sunAnglesRadians[0];
	double solarZenithRadians = sunAnglesRadians[1];
	double tiltRadians = surfaceAnglesRadians[1]; 
	double surfaceAzimuthRadians = surfaceAnglesRadians[2];

	// Average GHI on ground under PV array for cases when x projection exceed 2*rtr
	double averageGroundGHI = 0.0;
	for (size_t i = 0; i != frontGroundGHI.size(); i++)
		averageGroundGHI += frontGroundGHI[i] / frontGroundGHI.size();

	// Calculate diffuse isotropic irradiance for a horizontal surface
	double * poa = planeOfArrayIrradianceRear;
	double * diffc = diffuseIrradianceRear;
	perez(0, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, solarZenithRadians, 0, solarZenithRadians, poa, diffc);
	double isotropicSkyDiffuse = diffc[0];

	// Calculate components for a 90 degree tilt 
	double angleTmp[5] = { 0,0,0,0,0 };
	incidence(0, 90.0, 180.0, 45.0, solarZenithRadians, solarAzimuthRadians, this->enableBacktrack, this->groundCoverageRatio, this->forceToStow, this->stowAngleDegrees, angleTmp);
	perez(0, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, angleTmp[0], angleTmp[1], solarZenithRadians, poa, diffc);
	double horizonDiffuse = diffc[2];

	// Calculate x,y coordinates of bottom and top edges of PV row in back of desired PV row so that portions of sky and ground viewed by the 
	// PV cell may be determined. Origin of x-y axis is the ground point below the lower front edge of the desired PV row. The row in back of 
	// the desired row is in the positive x direction.
	double PbotX = -rowToRow;                        // x value for point on bottom edge of PV module/panel of row in front of (in PV panel slope lengths)
	double PbotY = clearanceGround;                  // y value for point on bottom edge of PV module/panel of row in front of (in PV panel slope lengths)
	double PtopX = -distanceBetweenRows;			 // x value for point on top edge of PV module/panel of row in front of (in PV panel slope lengths)
	double PtopY = verticalHeight + clearanceGround; // y value for point on top edge of PV module/panel of row in front of (in PV panel slope lengths)

	// Calculate diffuse and direct component irradiances for each cell row (assuming 6 rows)
	size_t cellRows = 6;
	for (size_t i = 0; i != cellRows; i++)
	{
		// Calculate diffuse irradiances and reflected amounts for each cell row over its field of view of 180 degrees, 
		// beginning with the angle providing the upper most view of the sky (j=0)
		double PcellX = horizontalLength * (i + 0.5) / ((double)cellRows);				   // x value for location of PV cell with OFFSET FOR SARA REFERENCE CELLS     4/26/2016
		double PcellY = clearanceGround + verticalHeight * (i + 0.5) / ((double)cellRows); // y value for location of PV cell with OFFSET FOR SARA REFERENCE CELLS     4/26/2016
		double elevationAngleUp = atan((PtopY - PcellY) / (PcellX - PtopX));          // Elevation angle up from PV cell to top of PV module/panel, radians
		double elevationAngleDown = atan((PcellY - PbotY) / (PcellX - PbotX));        // Elevation angle down from PV cell to bottom of PV module/panel, radians
		size_t iStopIso = (size_t)round((M_PI - tiltRadians - elevationAngleUp) / DTOR);							   // Last whole degree in arc range that sees sky, first is 0
		size_t iHorBright = (size_t)round(fmax(0.0, 6.0 - elevationAngleUp / DTOR));	   			       // Number of whole degrees for which horizon brightening occurs
		size_t iStartGrd = (size_t)round((M_PI - tiltRadians + elevationAngleDown) / DTOR);                          // First whole degree in arc range that sees ground, last is 180

		frontIrradiance.push_back(0.);
		frontReflected.push_back(0.);
		double reflectanceNormalIncidence = pow((n2 - 1.0) / (n2 + 1.0), 2.0);

		// Add sky diffuse component and horizon brightening if present
		for (size_t j = 0; j != iStopIso; j++)
		{
			frontIrradiance[i] += 0.5 * (cos(j * DTOR) - cos((j + 1)*DTOR)) * MarionAOICorrectionFactorsGlass[j] * isotropicSkyDiffuse;
			frontReflected[i] += 0.5 * (cos(j * DTOR) - cos((j + 1)*DTOR)) * isotropicSkyDiffuse * (1.0 - MarionAOICorrectionFactorsGlass[j] * (1.0 - reflectanceNormalIncidence));

			if ((iStopIso - j) <= iHorBright)
			{
				frontIrradiance[i] += 0.5 * (cos(j * DTOR) - cos((j + 1) * DTOR)) * MarionAOICorrectionFactorsGlass[j] * horizonDiffuse / 0.052246; // 0.052246 = 0.5 * [cos(84) - cos(90)]
				frontReflected[i] += 0.5 * (cos(j * DTOR) - cos((j + 1) * DTOR)) * (horizonDiffuse / 0.052246) * (1.0 - MarionAOICorrectionFactorsGlass[j] * (1.0 - reflectanceNormalIncidence));
			}
		}


		// Add ground reflected component
		for (size_t j = iStartGrd; j < 180; j++)
		{
			double startElevationDown = (j - iStartGrd) * DTOR + elevationAngleDown;
			double stopElevationDown = (j + 1 - iStartGrd) * DTOR + elevationAngleDown;
			double projectedX1 = PcellX - PcellY / tan(startElevationDown);
			double projectedX2 = PcellX - PcellY / tan(stopElevationDown);
			double actualGroundGHI = 0.0;

			if (fabs(projectedX1 - projectedX2) > 0.99 * rowToRow)
			{
				// Use average value if projection approximates the rtr      
				actualGroundGHI = averageGroundGHI;
			}
			else
			{
				projectedX1 = intervals * projectedX1 / rowToRow;
				projectedX2 = intervals * projectedX2 / rowToRow;

				// offset so array indexes are positive
				while (projectedX1 < 0.0 || projectedX2 < 0.0)
				{
					projectedX1 += intervals;
					projectedX2 += intervals;
				}
				
				size_t index1 = static_cast<size_t>(projectedX1);
				size_t index2 = static_cast<size_t>(projectedX2);

				if (index1 == index2)
				{
					actualGroundGHI = frontGroundGHI[index1];		
				}
				else
				{
					// Sum irradiances on the ground if projects are in different groundGHI elements
					for (size_t k = index1; k <= index2; k++)
					{
						if (k == index1)
						{
							actualGroundGHI += frontGroundGHI[k] * (k + 1.0 - projectedX1);
						}
						else if (k == index2)
						{
							if (k < intervals) {
								actualGroundGHI += frontGroundGHI[k] * (projectedX2 - k);
							} 
							else {
								actualGroundGHI += frontGroundGHI[k - 100] * (projectedX2 - k);
							}
						}
						else
						{
							if (k < intervals) {
								actualGroundGHI += frontGroundGHI[k];
							}
							else {
								actualGroundGHI += frontGroundGHI[k - 100];
							}
						}
					}
					// Irradiance on the ground in the 1-degree field of view
					actualGroundGHI /= projectedX2 - projectedX1;
				}
			}
			frontIrradiance[i] += 0.5 * (cos(j * DTOR) - cos((j + 1) * DTOR)) * MarionAOICorrectionFactorsGlass[j] * actualGroundGHI * this->albedo;
			frontReflected[i] += 0.5 * (cos(j * DTOR) - cos((j + 1) * DTOR)) * actualGroundGHI * this->albedo * (1.0 - MarionAOICorrectionFactorsGlass[j] * (1.0 - reflectanceNormalIncidence));
		}
		// Calculate and add direct and circumsolar irradiance components
		incidence(0, tiltRadians * RTOD, surfaceAzimuthRadians * RTOD, 45.0, solarZenithRadians, solarAzimuthRadians, this->enableBacktrack, this->groundCoverageRatio, 
			this->forceToStow, this->stowAngleDegrees, surfaceAnglesRadians);
		perez(0, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, surfaceAnglesRadians[0], surfaceAnglesRadians[1], solarZenithRadians, poa, diffc);

		double cellShade = pvFrontShadeFraction * cellRows - i;

		// Fully shaded if >1, no shade if < 0, otherwise fractionally shaded
		if (cellShade > 1.0) {
			cellShade = 1.0;
		}
		else if (cellShade < 0.0) {
			cellShade = 0.0;
		}

		// Cell not shaded entirely and incidence angle < 90 degrees 
		if (cellShade < 1.0 && surfaceAnglesRadians[0] < M_PI / 2.0)
		{
			double cor = iamSjerpsKoomen(n2, surfaceAnglesRadians[0]);
			frontIrradiance[i] += (1.0 - cellShade) * (poa[0] + diffc[1]) * cor;
		}
		frontAverageIrradiance += frontIrradiance[i] / cellRows;
	}
}

void irrad::getBackSurfaceIrradiances(double pvBackShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double , double horizontalLength, std::vector<double> rearGroundGHI, std::vector<double> frontGroundGHI, std::vector<double> frontReflected, std::vector<double> & rearIrradiance, double & rearAverageIrradiance)
{
	// front surface assumed to be glass
	double n2 = 1.526;

	size_t intervals = 100;
	double solarAzimuthRadians = sunAnglesRadians[0];
	double solarZenithRadians = sunAnglesRadians[1];
	double tiltRadians = surfaceAnglesRadians[1];
	double surfaceAzimuthRadians = surfaceAnglesRadians[2];

	// Average GHI on ground under PV array for cases when x projection exceed 2*rtr
	double averageGroundGHI = 0.0;          
	for (size_t i = 0; i != rearGroundGHI.size(); i++)
		averageGroundGHI += rearGroundGHI[i] / rearGroundGHI.size();

	// Calculate diffuse isotropic irradiance for a horizontal surface
	perez(0, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, solarZenithRadians, 0, solarZenithRadians, planeOfArrayIrradianceRear, diffuseIrradianceRear);
	double isotropicSkyDiffuse = diffuseIrradianceRear[0];

	// Calculate components for a 90 degree tilt 
	double surfaceAnglesRadians90[5] = { 0,0,0,0,0 };
	incidence(0, 90.0, 180.0, 45.0, solarZenithRadians, solarAzimuthRadians, this->enableBacktrack, this->groundCoverageRatio, this->forceToStow, this->stowAngleDegrees, surfaceAnglesRadians90);
	perez(0, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, surfaceAnglesRadians90[0], surfaceAnglesRadians90[1], solarZenithRadians, planeOfArrayIrradianceRear, diffuseIrradianceRear);
	double horizonDiffuse = diffuseIrradianceRear[2];

	// Calculate x,y coordinates of bottom and top edges of PV row in back of desired PV row so that portions of sky and ground viewed by the 
	// PV cell may be determined. Origin of x-y axis is the ground point below the lower front edge of the desired PV row. The row in back of 
	// the desired row is in the positive x direction.
	double PbotX = rowToRow;                         // x value for point on bottom edge of PV module/panel of row in back of (in PV panel slope lengths)
	double PbotY = clearanceGround;                  // y value for point on bottom edge of PV module/panel of row in back of (in PV panel slope lengths)
	double PtopX = rowToRow + horizontalLength;      // x value for point on top edge of PV module/panel of row in back of (in PV panel slope lengths)
	double PtopY = verticalHeight + clearanceGround; // y value for point on top edge of PV module/panel of row in back of (in PV panel slope lengths)

	// Calculate diffuse and direct component irradiances for each cell row (assuming 6 rows)
	size_t cellRows = 6;
	for (size_t i = 0; i != cellRows; i++)
	{
		// Calculate diffuse irradiances and reflected amounts for each cell row over its field of view of 180 degrees, 
		// beginning with the angle providing the upper most view of the sky (j=0)
		double PcellX = horizontalLength * (i + 0.5) / ((double)cellRows);				   // x value for location of PV cell with OFFSET FOR SARA REFERENCE CELLS     4/26/2016
		double PcellY = clearanceGround + verticalHeight * (i + 0.5) / ((double)cellRows); // y value for location of PV cell with OFFSET FOR SARA REFERENCE CELLS     4/26/2016
		double elevationAngleUp = atan((PtopY - PcellY) / (PtopX - PcellX));          // Elevation angle up from PV cell to top of PV module/panel, radians
		double elevationAngleDown = atan((PcellY - PbotY) / (PbotX - PcellX));        // Elevation angle down from PV cell to bottom of PV module/panel, radians
		size_t iStopIso = (size_t)round((tiltRadians - elevationAngleUp) / DTOR);							   // Last whole degree in arc range that sees sky, first is 0
		size_t iHorBright = (size_t)round(fmax(0.0, 6.0 - elevationAngleUp / DTOR));	   			       // Number of whole degrees for which horizon brightening occurs
		size_t iStartGrd = (size_t)round((tiltRadians + elevationAngleDown) / DTOR);                          // First whole degree in arc range that sees ground, last is 180

		rearIrradiance.push_back(0);
		for (size_t j = 0; j != iStopIso; j++)
		{
			rearIrradiance[i] += 0.5 * (cos(j * DTOR) - cos((j + 1)*DTOR)) * MarionAOICorrectionFactorsGlass[j]* isotropicSkyDiffuse;
			if ((iStopIso - j) <= iHorBright)
			{
				rearIrradiance[i] += 0.5 * (cos(j * DTOR) - cos((j + 1) * DTOR)) * MarionAOICorrectionFactorsGlass[j]* horizonDiffuse / 0.052264; // 0.052246 = 0.5 * [cos(84) - cos(90)]
			}
		}

		// Add relections from PV module front surfaces
		for (size_t j = iStopIso; j < iStartGrd; j++)
		{
			double diagonalDistance = (PbotX - PcellX) / cos(elevationAngleDown);
			double startAlpha = -(double)(j - iStopIso) * DTOR + elevationAngleUp + elevationAngleDown;
			double stopAlpha = -(double)(j + 1 - iStopIso) * DTOR + elevationAngleUp + elevationAngleDown;
			double m = diagonalDistance * sin(startAlpha);
			double theta = M_PI - elevationAngleDown - (M_PI / 2.0 - startAlpha) - tiltRadians;
			double projectedX2 = m / cos(theta);

			m = diagonalDistance * sin(stopAlpha);
			theta = M_PI - elevationAngleDown - (M_PI / 2.0 - stopAlpha) - tiltRadians;
			double projectedX1 = m / cos(theta);
			projectedX1 = fmax(0.0, projectedX1);

			double PVreflectedIrradiance = 0.0;
			double deltaCell = 1.0 / cellRows;
			double tolerance = 0.0001;
			for (size_t k = 0; k < cellRows; k++)
			{
				double cellBottom = k * deltaCell;
				double cellTop = (k + 1) * deltaCell;
				double cellLengthSeen = 0.0;

				if (cellBottom >= projectedX1 - tolerance && cellTop <= projectedX2 + tolerance) {
					cellLengthSeen = cellTop - cellBottom;
				}
				else if (cellBottom <= projectedX1 + tolerance && cellTop >= projectedX2 - tolerance) {
					cellLengthSeen = projectedX2 - projectedX1;
				}
				else if (cellBottom >= projectedX1 - tolerance && projectedX2 > cellBottom - tolerance && cellTop >= projectedX2 - tolerance) {
					cellLengthSeen = projectedX2 - cellBottom;
				}
				else if (cellBottom <= projectedX1 + tolerance && projectedX1 < cellTop + tolerance && cellTop <= projectedX2 + tolerance) {
					cellLengthSeen = cellTop - projectedX1;
				}

				PVreflectedIrradiance += cellLengthSeen * frontReflected[k];
			}
			PVreflectedIrradiance /= projectedX2 - projectedX1;
			rearIrradiance[i] += 0.5 * (cos(j * DTOR) - cos((j + 1) * DTOR)) * MarionAOICorrectionFactorsGlass[j] * PVreflectedIrradiance;
		}


		// Add ground reflected component
		for (size_t j = iStartGrd; j < 180; j++)
		{
			double startElevationDown = (double)(j - iStartGrd) * DTOR + elevationAngleDown;
			double stopElevationDown = (double)(j + 1 - iStartGrd) * DTOR + elevationAngleDown;
			double projectedX2 = PcellX + PcellY / tan(startElevationDown);
			double projectedX1 = PcellX + PcellY / tan(stopElevationDown);
			double actualGroundGHI = 0.0;

			if (fabs(projectedX1 - projectedX2) > 0.99 * rowToRow)
			{
				// Use average value if projection approximates the rtr      
				actualGroundGHI = averageGroundGHI;
			}
			else
			{
				projectedX1 = intervals * projectedX1 / rowToRow;
				projectedX2 = intervals * projectedX2 / rowToRow;

				// offset so array indexed are less than number of intervals
				while (projectedX1 >= intervals || projectedX2 >= intervals)
				{
					projectedX1 -= intervals;
					projectedX2 -= intervals;
				}
				while (projectedX1 < -(int)intervals || projectedX2 < -(int)intervals)
				{
					projectedX1 += intervals;
					projectedX2 += intervals;
				}
				int index1 = static_cast<int>(projectedX1 + intervals) - (int)intervals;
				int index2 = static_cast<int>(projectedX2 + intervals) - (int)intervals;

				if (index1 == index2)
				{
					if (index1 < 0){
						actualGroundGHI = frontGroundGHI[index1 + 100];
					}
					else {
						actualGroundGHI = rearGroundGHI[index1];
					}
				}
				else
				{
					// Sum irradiances on the ground if projects are in different groundGHI elements
					for (int k = index1; k <= index2; k++)
					{
						if (k == index1)
						{
							if (k < 0) {
								actualGroundGHI += frontGroundGHI[k + intervals] * (k + 1.0 - projectedX1);
							}
							else {
								actualGroundGHI += rearGroundGHI[k] * (k + 1.0 - projectedX1);
							}
						}
						else if (k == index2)
						{
							if (k < 0) {
								actualGroundGHI += frontGroundGHI[k + intervals] * (projectedX2 - k);
							}
							else {
								actualGroundGHI += rearGroundGHI[k] * (projectedX2 - k);
							}
						}
						else
						{
							if (k < 0) {
								actualGroundGHI += frontGroundGHI[k + 100];
							}
							else {
								actualGroundGHI += rearGroundGHI[k];
							}
						}
					}
					// Irradiance on the ground in the 1-degree field of view
					actualGroundGHI /= projectedX2 - projectedX1;
				}
			}
			rearIrradiance[i] += 0.5 * (cos(j * DTOR) - cos((j + 1) * DTOR)) * MarionAOICorrectionFactorsGlass[j] * actualGroundGHI * this->albedo;
		}
		// Calculate and add direct and circumsolar irradiance components
		incidence(0, 180.0 - tiltRadians * RTOD, (surfaceAzimuthRadians * RTOD - 180.0), 45.0, solarZenithRadians, solarAzimuthRadians, this->enableBacktrack, 
			this->groundCoverageRatio, this->forceToStow, this->stowAngleDegrees, surfaceAnglesRadians);
		perez(0, calculatedDirectNormal, calculatedDiffuseHorizontal, albedo, surfaceAnglesRadians[0], surfaceAnglesRadians[1], solarZenithRadians, planeOfArrayIrradianceRear, diffuseIrradianceRear);

		double cellShade = pvBackShadeFraction * cellRows - i;
		
		// Fully shaded if >1, no shade if < 0, otherwise fractionally shaded
		if (cellShade > 1.0) {
			cellShade = 1.0;
		}
		else if (cellShade < 0.0) {
			cellShade = 0.0;
		}

		// Cell not shaded entirely and incidence angle < 90 degrees 
		if (cellShade < 1.0 && surfaceAnglesRadians[0] < M_PI / 2.0)
		{
			double iamMod = iamSjerpsKoomen(n2, surfaceAnglesRadians[0]);
			rearIrradiance[i] += (1.0 - cellShade) * (planeOfArrayIrradianceRear[0] + diffuseIrradianceRear[1]) * iamMod;
		}
		rearAverageIrradiance += rearIrradiance[i] / cellRows;
	}
}

static double vec_dot(double a[3], double b[3])
{
	return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

static void vec_cross(double a[3], double b[3], double result[3])
{
	result[0] = a[1] * b[2] - a[2] * b[1];
	result[1] = a[2] * b[0] - a[0] * b[2];
	result[2] = a[0] * b[1] - a[1] * b[0];
}

//a - b
static void vec_diff( double a[3], double b[3], double result[3] )
{
	result[0] = a[0] - b[0];
	result[1] = a[1] - b[1];
	result[2] = a[2] - b[2];
}

static void get_vertices( double axis_tilt, double axis_azimuth, double gcr,				 
				 double vertices[3][4][3], double rotation)
{
	//Get panel vertices for flat panels, no tilt or azimuth, 
	//ordered ccw starting from x+
	//vertices[0] is panel 0
	//vertices[0][1] is corner 1 on panel 0
	//vertices[0][1][2] is coordinate 2(z) for corner 1 on panel 0.
	//All are 0-indexed.  0=x, 1=y, 2=z.

	double width = 1.0;
	double row_spacing = 1.0/gcr - 1.0;
	double length = 10.0;

	for (int i=0; i<3; i++)
	{
		vertices[i][0][0] = width/2 + i*(row_spacing + width);
		vertices[i][0][1] = 0;
		vertices[i][0][2] = 0;
		
		vertices[i][1][0] = width/2 + i*(row_spacing + width);
		vertices[i][1][1] = length;
		vertices[i][1][2] = 0;
			
		vertices[i][2][0] = -width/2 + i*(row_spacing + width);
		vertices[i][2][1] = length;
		vertices[i][2][2] = 0;
		
		vertices[i][3][0] = -width/2 + i*(row_spacing + width);
		vertices[i][3][1] = 0;
		vertices[i][3][2] = 0;
	}
	//We now have vertices for flat panels spaced evenly along the x+ axis.
	
	//Rotate each panel by rotation angle 
	for (int i=0; i<3; i++)
	{
		//Move so that we rotate about y-axis.
		//Perform rotation, then move back.
		double offset = i*(row_spacing + width);
		
		vertices[i][0][0] = vertices[i][0][0] - offset;
		vertices[i][1][0] = vertices[i][1][0] - offset;
		vertices[i][2][0] = vertices[i][2][0] - offset;
		vertices[i][3][0] = vertices[i][3][0] - offset;
		
		//Rotation matrix T is 
		//  cos 0 sin
		//   0  1  0
		// -sin 0 cos
		// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
		for(int j=0; j<4; j++)
		{
			//When we do calculations for new coords, they all depend on old coords.
			double oldVertX = vertices[i][j][0]; //Z coord depends on original y coord.
			double oldVertZ = vertices[i][j][2];
			vertices[i][j][0] = oldVertX * cosd(rotation) + oldVertZ * sind(rotation);
			vertices[i][j][2] = oldVertX * -sind(rotation) + oldVertZ * cosd(rotation);
		}
		
		//Translate back to original location after rotation is complete.
		vertices[i][0][0] = vertices[i][0][0] + offset;
		vertices[i][1][0] = vertices[i][1][0] + offset;
		vertices[i][2][0] = vertices[i][2][0] + offset;
		vertices[i][3][0] = vertices[i][3][0] + offset;
	}
	
	
	//Now globally rotate all coords by axis tilt
	for (int i=0; i<3; i++)
	{
		//Move to rotate about x axis.
		//Perform rotation, then move back.
		double offset = length;
		
		vertices[i][0][1] = vertices[i][0][1] - offset;
		vertices[i][1][1] = vertices[i][1][1] - offset;
		vertices[i][2][1] = vertices[i][2][1] - offset;
		vertices[i][3][1] = vertices[i][3][1] - offset;
		
		//Rotation matrix T is 
		// 1   0   0
		// 0  cos sin
		// 0 -sin cos
		// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
		for (int j=0; j<4; j++)
		{
			//When we do calculations for new coords, they all depend on old coords.
			double oldVertY = vertices[i][j][1]; //Z coord depends on original y coord.
			double oldVertZ = vertices[i][j][2];
			vertices[i][j][1] = oldVertY * cosd(axis_tilt) + oldVertZ * sind(axis_tilt);
			vertices[i][j][2] = oldVertY * -sind(axis_tilt) + oldVertZ * cosd(axis_tilt);
		}
		
		vertices[i][0][1] = vertices[i][0][1] + offset;
		vertices[i][1][1] = vertices[i][1][1] + offset;
		vertices[i][2][1] = vertices[i][2][1] + offset;
		vertices[i][3][1] = vertices[i][3][1] + offset;
	}
	
	
	//Now globally rotate all coords by axis azimuth
	for (int i=0; i<3; i++)
	{
		//We are rotating about the Z axis, so we don't need to translate.
		
		//Rotation matrix T is 
		//  cos sin 0
		// -sin cos 0
		//   0   0  1
		// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
		for (int j=0; j<4; j++)
		{
			//When we do calculations for new coords, they all depend on old coords.
			double oldVertX = vertices[i][j][0]; //Z coord depends on original y coord.
			double oldVertY = vertices[i][j][1];
			vertices[i][j][0] = oldVertX * cosd(axis_azimuth) + oldVertY * sind(axis_azimuth);
			vertices[i][j][1] = oldVertX * -sind(axis_azimuth) + oldVertY * cosd(axis_azimuth);
		}
	}
}



static void sun_unit( double sazm, double szen, double sun[3] )
{	
	//Get unit vector in direction of sun
	double solalt = 90 - szen;
		
	if ( sazm >= 0 && sazm <= 90 )
	{
		sun[0] = cosd(solalt)*sind(sazm);
		sun[1] = cosd(solalt)*cosd(sazm);
	}
	else if ( sazm > 90 && sazm <= 180 )
	{
		sun[0] = cosd(solalt)*sind(180-sazm);
		sun[1] = -cosd(solalt)*cosd(180-sazm);
	}
	else if ( sazm > 180 && sazm <= 270 )
	{
		sun[0] = -cosd(solalt)*sind(sazm-180);
		sun[1] = -cosd(solalt)*cosd(sazm-180);
	}
	else
	{
		sun[0] = -cosd(solalt)*sind(360-sazm);
		sun[1] = cosd(solalt)*cosd(360-sazm);
	}
	
	sun[2] = sind(solalt);
		
	//normalize
	double magnitude = sqrt(sun[0]*sun[0] + sun[1]*sun[1] + sun[2]*sun[2]);
	sun[0] = sun[0] / magnitude;
	sun[1] = sun[1] / magnitude;
	sun[2] = sun[2] / magnitude;
}


double shadeFraction1x( double solazi, double solzen,
						 double axis_tilt, double axis_azimuth, 
						 double gcr, double rotation )
{
	//Get unit vector in direction of sun
	
	double sun[3];
	sun_unit( solazi, solzen, sun );

	//For now, assume array has at least 3 rows.
	//This way we can use index 1 and it has a panel on both sides.
	
	//Get our vertices for our array.
	double verts[3][4][3]; //To allocate
	get_vertices( axis_tilt, axis_azimuth, gcr, verts, rotation );
	
	//Find which panel is in the direction of the sun by using dot product.
	//toPrev is a vector from panel 1 to panel 0.
	//toNext is a vector from panel 1 to panel 2.
	//The sun is in the direction of the panel whose vector has a larger positive
	//dot product with the sun direction vector.
	//Store the panel in the direction of the sun from panel 1 in the variable iPanel.
	int iPanel = 0;
	double toPrev[3];
	toPrev[0] = verts[0][0][0] - verts[1][0][0];
	toPrev[1] = verts[0][0][1] - verts[1][0][1];
	toPrev[2] = verts[0][0][2] - verts[1][0][2];
	double toNext[3];
	toNext[0] = verts[2][0][0] - verts[1][0][0];
	toNext[1] = verts[2][0][1] - verts[1][0][1];
	toNext[2] = verts[2][0][2] - verts[1][0][2];
	if (vec_dot(toPrev, sun) < vec_dot(toNext, sun)) iPanel = 2;
	
	
	//Get midpoint of edge of panel 1 on the sun side.
	//This edge is on the same side of panel 1 as iPanel.
	//Store midpoint in midP.
	double midP[3];
	for (int i=0; i<3; i++)
	{
		if (iPanel == 0) midP[i] = (verts[1][2][i] + verts[1][3][i]) / 2;
		else midP[i] = (verts[1][0][i] + verts[1][1][i]) / 2;
	}
	
	//Get normal vector to plane for iPanel.
	//This is easy - just get two vectors in the plane of iPanel and cross them.
	//Use the vectors along the edges out of vertex 0.  That is, edge01 and edge03.
	//Store the result in a vector called normal.
	double normal[3],a1[3],a2[3];
	vec_diff(verts[iPanel][1], verts[iPanel][0], a1);
	vec_diff(verts[iPanel][3], verts[iPanel][0], a2);
	vec_cross(a1, a2, normal);
	
	
	//We want to find the point of intersection of the ray that starts at midP
	//and goes in the direction of the sun.
	//Assume sun is at infinity in direction 'sun'.
	//First make sure that this ray has a unique intersection point.
	double sunDot = vec_dot(normal, sun);
	if (fabs(sunDot) < 0.001) return 0; //sun vector lies in plane
	
	//Now, vector pDir goes from midP to any point in the plane of iPanel.  Vertex 0, say.
	//Project pDir onto the normal to iPanel, and project the sun vector onto the normal vector.
	//The ratio of these projections tells us how fat to move along the sun vector, starting
	//from midP, to get a point in the plane of iPanel.
	double pDir[3];
	vec_diff(verts[iPanel][0], midP, pDir);
	double t = vec_dot(normal, pDir) / vec_dot(normal, sun);
	if (t < 0) return 0; // sun has set (is behind array).
	
	double intersectP[3];
	for (int i=0; i<3; i++)
	{
		intersectP[i] = midP[i] + t * sun[i];
	}
	//intersectP is along the ray from midP to the sun, and lies in the (infinite) plane of iPanel.
	
	//Figure out if intersectP is inside the bounds of the iPanel.
	//This is simple.  If intersectP is on the same side of the edge as the panel for both edges,
	//then it is in the panel.
	//Find a vector from edge to intersectP and take dot product with vector from edge through panel.
	//If dot product is positive, intersectP is on the panel side of that edge.
	//Reuse a1 and a2 from above (as temporary vectors).
	vec_diff(verts[iPanel][3], verts[iPanel][0], a1);
	vec_diff(intersectP, verts[iPanel][0], a2);
	if (vec_dot(a1, a2) < 0) return 0; //intersect is outside panel bounds.
	
	vec_diff(verts[iPanel][0], verts[iPanel][3], a1);
	vec_diff(intersectP, verts[iPanel][3], a2);
	if (vec_dot(a1, a2) < 0) return 0; //intersect is outside panel bounds.
	
	// Now we know the panel is shaded, so compute geometric shade fraction	
	double mu[3] = { 0, 0, 0 }; // upper edge midpoint on adjacent panel
	double ml[3] = { 0, 0, 0 }; // lower edge midpoint on adjacent panel
	for (int i=0; i<3; i++)
	{
		if (iPanel == 2) mu[i] = (verts[iPanel][2][i] + verts[iPanel][3][i]) / 2;
		else             mu[i] = (verts[iPanel][0][i] + verts[iPanel][1][i]) / 2;
		
		if (iPanel == 2) ml[i] = (verts[iPanel][0][i] + verts[iPanel][1][i]) / 2;
		else             ml[i] = (verts[iPanel][2][i] + verts[iPanel][3][i]) / 2;
		
	}
	
	vec_diff(intersectP, mu, a1);
	vec_diff(ml, mu, a2);
	double maga2 = vec_dot(a2,a2);
	double Ab = vec_dot(a1, a2)/maga2;  // geometric shading fraction [0..1]

	return Ab;
}


//Find optimum angle using backtracking.
double backtrack( double solazi, double solzen, 
				 double axis_tilt, double axis_azimuth, 
				 double rotlim, double gcr, double rotation )
{
	//Now do backtracking.
	//This is very straightforward - decrease the rotation as long as we are in shade.
	int iter = 0;
	while(shadeFraction1x( solazi, solzen, axis_tilt, axis_azimuth, gcr, rotation) > 0 && ++iter < 100)
	{
		//Move closer to flat.
		if (rotation > 0)
		{
			if ( fabs(rotation-1) > fabs(rotlim) )
				break;
			rotation = rotation - 1;
		}	
		else
		{
			if ( fabs(rotation+1) > fabs(rotlim) )
				break;
			rotation = rotation + 1;
		}
	}
	return rotation;
}


// Begin modified DISC code

double cm[6][6][7][5] = 
    {{{{ 0.385230, 0.385230, 0.385230, 0.462880, 0.317440 },
    { 0.338390, 0.338390, 0.221270, 0.316730, 0.503650 },
    { 0.235680, 0.235680, 0.241280, 0.157830, 0.269440 },
    { 0.830130, 0.830130, 0.171970, 0.841070, 0.457370 },
    { 0.548010, 0.548010, 0.478000, 0.966880, 1.036370 },
    { 0.548010, 0.548010, 1.000000, 3.012370, 1.976540 },
    { 0.582690, 0.582690, 0.229720, 0.892710, 0.569950 }},

    {{ 0.131280, 0.131280, 0.385460, 0.511070, 0.127940 },
    { 0.223710, 0.223710, 0.193560, 0.304560, 0.193940 },
    { 0.229970, 0.229970, 0.275020, 0.312730, 0.244610 },
    { 0.090100, 0.184580, 0.260500, 0.687480, 0.579440 },
    { 0.131530, 0.131530, 0.370190, 1.380350, 1.052270 },
    { 1.116250, 1.116250, 0.928030, 3.525490, 2.316920 },
    { 0.090100, 0.237000, 0.300040, 0.812470, 0.664970 }},

    {{ 0.587510, 0.130000, 0.400000, 0.537210, 0.832490 },
    { 0.306210, 0.129830, 0.204460, 0.500000, 0.681640 },
    { 0.224020, 0.260620, 0.334080, 0.501040, 0.350470 },
    { 0.421540, 0.753970, 0.750660, 3.706840, 0.983790 },
    { 0.706680, 0.373530, 1.245670, 0.864860, 1.992630 },
    { 4.864400, 0.117390, 0.265180, 0.359180, 3.310820 },
    { 0.392080, 0.493290, 0.651560, 1.932780, 0.898730 }},

    {{ 0.126970, 0.126970, 0.126970, 0.126970, 0.126970 },
    { 0.810820, 0.810820, 0.810820, 0.810820, 0.810820 },
    { 3.241680, 2.500000, 2.291440, 2.291440, 2.291440 },
    { 4.000000, 3.000000, 2.000000, 0.975430, 1.965570 },
    { 12.494170, 12.494170, 8.000000, 5.083520, 8.792390 },
    { 21.744240, 21.744240, 21.744240, 21.744240, 21.744240 },
    { 3.241680, 12.494170, 1.620760, 1.375250, 2.331620 }},

    {{ 0.126970, 0.126970, 0.126970, 0.126970, 0.126970 },
    { 0.810820, 0.810820, 0.810820, 0.810820, 0.810820 },
    { 3.241680, 2.500000, 2.291440, 2.291440, 2.291440 },
    { 4.000000, 3.000000, 2.000000, 0.975430, 1.965570 },
    { 12.494170, 12.494170, 8.000000, 5.083520, 8.792390 },
    { 21.744240, 21.744240, 21.744240, 21.744240, 21.744240 },
    { 3.241680, 12.494170, 1.620760, 1.375250, 2.331620 }},

    {{ 0.126970, 0.126970, 0.126970, 0.126970, 0.126970 },
    { 0.810820, 0.810820, 0.810820, 0.810820, 0.810820 },
    { 3.241680, 2.500000, 2.291440, 2.291440, 2.291440 },
    { 4.000000, 3.000000, 2.000000, 0.975430, 1.965570 },
    { 12.494170, 12.494170, 8.000000, 5.083520, 8.792390 },
    { 21.744240, 21.744240, 21.744240, 21.744240, 21.744240 },
    { 3.241680, 12.494170, 1.620760, 1.375250, 2.331620 }}},

    {{{ 0.337440, 0.337440, 0.969110, 1.097190, 1.116080 },
    { 0.337440, 0.337440, 0.969110, 1.116030, 0.623900 },
    { 0.337440, 0.337440, 1.530590, 1.024420, 0.908480 },
    { 0.584040, 0.584040, 0.847250, 0.914940, 1.289300 },
    { 0.337440, 0.337440, 0.310240, 1.435020, 1.852830 },
    { 0.337440, 0.337440, 1.015010, 1.097190, 2.117230 },
    { 0.337440, 0.337440, 0.969110, 1.145730, 1.476400 }},

    {{ 0.300000, 0.300000, 0.700000, 1.100000, 0.796940 },
    { 0.219870, 0.219870, 0.526530, 0.809610, 0.649300 },
    { 0.386650, 0.386650, 0.119320, 0.576120, 0.685460 },
    { 0.746730, 0.399830, 0.470970, 0.986530, 0.785370 },
    { 0.575420, 0.936700, 1.649200, 1.495840, 1.335590 },
    { 1.319670, 4.002570, 1.276390, 2.644550, 2.518670 },
    { 0.665190, 0.678910, 1.012360, 1.199940, 0.986580 }},

    {{ 0.378870, 0.974060, 0.500000, 0.491880, 0.665290 },
    { 0.105210, 0.263470, 0.407040, 0.553460, 0.582590 },
    { 0.312900, 0.345240, 1.144180, 0.854790, 0.612280 },
    { 0.119070, 0.365120, 0.560520, 0.793720, 0.802600 },
    { 0.781610, 0.837390, 1.270420, 1.537980, 1.292950 },
    { 1.152290, 1.152290, 1.492080, 1.245370, 2.177100 },
    { 0.424660, 0.529550, 0.966910, 1.033460, 0.958730 }},

    {{ 0.310590, 0.714410, 0.252450, 0.500000, 0.607600 },
    { 0.975190, 0.363420, 0.500000, 0.400000, 0.502800 },
    { 0.175580, 0.196250, 0.476360, 1.072470, 0.490510 },
    { 0.719280, 0.698620, 0.657770, 1.190840, 0.681110 },
    { 0.426240, 1.464840, 0.678550, 1.157730, 0.978430 },
    { 2.501120, 1.789130, 1.387090, 2.394180, 2.394180 },
    { 0.491640, 0.677610, 0.685610, 1.082400, 0.735410 }},

    {{ 0.597000, 0.500000, 0.300000, 0.310050, 0.413510 },
    { 0.314790, 0.336310, 0.400000, 0.400000, 0.442460 },
    { 0.166510, 0.460440, 0.552570, 1.000000, 0.461610 },
    { 0.401020, 0.559110, 0.403630, 1.016710, 0.671490 },
    { 0.400360, 0.750830, 0.842640, 1.802600, 1.023830 },
    { 3.315300, 1.510380, 2.443650, 1.638820, 2.133990 },
    { 0.530790, 0.745850, 0.693050, 1.458040, 0.804500 }},

    {{ 0.597000, 0.500000, 0.300000, 0.310050, 0.800920 },
    { 0.314790, 0.336310, 0.400000, 0.400000, 0.237040 },
    { 0.166510, 0.460440, 0.552570, 1.000000, 0.581990 },
    { 0.401020, 0.559110, 0.403630, 1.016710, 0.898570 },
    { 0.400360, 0.750830, 0.842640, 1.802600, 3.400390 },
    { 3.315300, 1.510380, 2.443650, 1.638820, 2.508780 },
    { 0.204340, 1.157740, 2.003080, 2.622080, 1.409380 }}},

    {{{ 1.242210, 1.242210, 1.242210, 1.242210, 1.242210 },
    { 0.056980, 0.056980, 0.656990, 0.656990, 0.925160 },
    { 0.089090, 0.089090, 1.040430, 1.232480, 1.205300 },
    { 1.053850, 1.053850, 1.399690, 1.084640, 1.233340 },
    { 1.151540, 1.151540, 1.118290, 1.531640, 1.411840 },
    { 1.494980, 1.494980, 1.700000, 1.800810, 1.671600 },
    { 1.018450, 1.018450, 1.153600, 1.321890, 1.294670 }},

    {{ 0.700000, 0.700000, 1.023460, 0.700000, 0.945830 },
    { 0.886300, 0.886300, 1.333620, 0.800000, 1.066620 },
    { 0.902180, 0.902180, 0.954330, 1.126690, 1.097310 },
    { 1.095300, 1.075060, 1.176490, 1.139470, 1.096110 },
    { 1.201660, 1.201660, 1.438200, 1.256280, 1.198060 },
    { 1.525850, 1.525850, 1.869160, 1.985410, 1.911590 },
    { 1.288220, 1.082810, 1.286370, 1.166170, 1.119330 }},

    {{ 0.600000, 1.029910, 0.859890, 0.550000, 0.813600 },
    { 0.604450, 1.029910, 0.859890, 0.656700, 0.928840 },
    { 0.455850, 0.750580, 0.804930, 0.823000, 0.911000 },
    { 0.526580, 0.932310, 0.908620, 0.983520, 0.988090 },
    { 1.036110, 1.100690, 0.848380, 1.035270, 1.042380 },
    { 1.048440, 1.652720, 0.900000, 2.350410, 1.082950 },
    { 0.817410, 0.976160, 0.861300, 0.974780, 1.004580 }},

    {{ 0.782110, 0.564280, 0.600000, 0.600000, 0.665740 },
    { 0.894480, 0.680730, 0.541990, 0.800000, 0.669140 },
    { 0.487460, 0.818950, 0.841830, 0.872540, 0.709040 },
    { 0.709310, 0.872780, 0.908480, 0.953290, 0.844350 },
    { 0.863920, 0.947770, 0.876220, 1.078750, 0.936910 },
    { 1.280350, 0.866720, 0.769790, 1.078750, 0.975130 },
    { 0.725420, 0.869970, 0.868810, 0.951190, 0.829220 }},

    {{ 0.791750, 0.654040, 0.483170, 0.409000, 0.597180 },
    { 0.566140, 0.948990, 0.971820, 0.653570, 0.718550 },
    { 0.648710, 0.637730, 0.870510, 0.860600, 0.694300 },
    { 0.637630, 0.767610, 0.925670, 0.990310, 0.847670 },
    { 0.736380, 0.946060, 1.117590, 1.029340, 0.947020 },
    { 1.180970, 0.850000, 1.050000, 0.950000, 0.888580 },
    { 0.700560, 0.801440, 0.961970, 0.906140, 0.823880 }},

    {{ 0.500000, 0.500000, 0.586770, 0.470550, 0.629790 },
    { 0.500000, 0.500000, 1.056220, 1.260140, 0.658140 },
    { 0.500000, 0.500000, 0.631830, 0.842620, 0.582780 },
    { 0.554710, 0.734730, 0.985820, 0.915640, 0.898260 },
    { 0.712510, 1.205990, 0.909510, 1.078260, 0.885610 },
    { 1.899260, 1.559710, 1.000000, 1.150000, 1.120390 },
    { 0.653880, 0.793120, 0.903320, 0.944070, 0.796130 }}},

    {{{ 1.000000, 1.000000, 1.050000, 1.170380, 1.178090 },
    { 0.960580, 0.960580, 1.059530, 1.179030, 1.131690 },
    { 0.871470, 0.871470, 0.995860, 1.141910, 1.114600 },
    { 1.201590, 1.201590, 0.993610, 1.109380, 1.126320 },
    { 1.065010, 1.065010, 0.828660, 0.939970, 1.017930 },
    { 1.065010, 1.065010, 0.623690, 1.119620, 1.132260 },
    { 1.071570, 1.071570, 0.958070, 1.114130, 1.127110 }},

    {{ 0.950000, 0.973390, 0.852520, 1.092200, 1.096590 },
    { 0.804120, 0.913870, 0.980990, 1.094580, 1.042420 },
    { 0.737540, 0.935970, 0.999940, 1.056490, 1.050060 },
    { 1.032980, 1.034540, 0.968460, 1.032080, 1.015780 },
    { 0.900000, 0.977210, 0.945960, 1.008840, 0.969960 },
    { 0.600000, 0.750000, 0.750000, 0.844710, 0.899100 },
    { 0.926800, 0.965030, 0.968520, 1.044910, 1.032310 }},

    {{ 0.850000, 1.029710, 0.961100, 1.055670, 1.009700 },
    { 0.818530, 0.960010, 0.996450, 1.081970, 1.036470 },
    { 0.765380, 0.953500, 0.948260, 1.052110, 1.000140 },
    { 0.775610, 0.909610, 0.927800, 0.987800, 0.952100 },
    { 1.000990, 0.881880, 0.875950, 0.949100, 0.893690 },
    { 0.902370, 0.875960, 0.807990, 0.942410, 0.917920 },
    { 0.856580, 0.928270, 0.946820, 1.032260, 0.972990 }},

    {{ 0.750000, 0.857930, 0.983800, 1.056540, 0.980240 },
    { 0.750000, 0.987010, 1.013730, 1.133780, 1.038250 },
    { 0.800000, 0.947380, 1.012380, 1.091270, 0.999840 },
    { 0.800000, 0.914550, 0.908570, 0.999190, 0.915230 },
    { 0.778540, 0.800590, 0.799070, 0.902180, 0.851560 },
    { 0.680190, 0.317410, 0.507680, 0.388910, 0.646710 },
    { 0.794920, 0.912780, 0.960830, 1.057110, 0.947950 }},

    {{ 0.750000, 0.833890, 0.867530, 1.059890, 0.932840 },
    { 0.979700, 0.971470, 0.995510, 1.068490, 1.030150 },
    { 0.858850, 0.987920, 1.043220, 1.108700, 1.044900 },
    { 0.802400, 0.955110, 0.911660, 1.045070, 0.944470 },
    { 0.884890, 0.766210, 0.885390, 0.859070, 0.818190 },
    { 0.615680, 0.700000, 0.850000, 0.624620, 0.669300 },
    { 0.835570, 0.946150, 0.977090, 1.049350, 0.979970 }},

    {{ 0.689220, 0.809600, 0.900000, 0.789500, 0.853990 },
    { 0.854660, 0.852840, 0.938200, 0.923110, 0.955010 },
    { 0.938600, 0.932980, 1.010390, 1.043950, 1.041640 },
    { 0.843620, 0.981300, 0.951590, 0.946100, 0.966330 },
    { 0.694740, 0.814690, 0.572650, 0.400000, 0.726830 },
    { 0.211370, 0.671780, 0.416340, 0.297290, 0.498050 },
    { 0.843540, 0.882330, 0.911760, 0.898420, 0.960210 }}},

    {{{ 1.054880, 1.075210, 1.068460, 1.153370, 1.069220 },
    { 1.000000, 1.062220, 1.013470, 1.088170, 1.046200 },
    { 0.885090, 0.993530, 0.942590, 1.054990, 1.012740 },
    { 0.920000, 0.950000, 0.978720, 1.020280, 0.984440 },
    { 0.850000, 0.908500, 0.839940, 0.985570, 0.962180 },
    { 0.800000, 0.800000, 0.810080, 0.950000, 0.961550 },
    { 1.038590, 1.063200, 1.034440, 1.112780, 1.037800 }},

    {{ 1.017610, 1.028360, 1.058960, 1.133180, 1.045620 },
    { 0.920000, 0.998970, 1.033590, 1.089030, 1.022060 },
    { 0.912370, 0.949930, 0.979770, 1.020420, 0.981770 },
    { 0.847160, 0.935300, 0.930540, 0.955050, 0.946560 },
    { 0.880260, 0.867110, 0.874130, 0.972650, 0.883420 },
    { 0.627150, 0.627150, 0.700000, 0.774070, 0.845130 },
    { 0.973700, 1.006240, 1.026190, 1.071960, 1.017240 }},

    {{ 1.028710, 1.017570, 1.025900, 1.081790, 1.024240 },
    { 0.924980, 0.985500, 1.014100, 1.092210, 0.999610 },
    { 0.828570, 0.934920, 0.994950, 1.024590, 0.949710 },
    { 0.900810, 0.901330, 0.928830, 0.979570, 0.913100 },
    { 0.761030, 0.845150, 0.805360, 0.936790, 0.853460 },
    { 0.626400, 0.546750, 0.730500, 0.850000, 0.689050 },
    { 0.957630, 0.985480, 0.991790, 1.050220, 0.987900 }},

    {{ 0.992730, 0.993880, 1.017150, 1.059120, 1.017450 },
    { 0.975610, 0.987160, 1.026820, 1.075440, 1.007250 },
    { 0.871090, 0.933190, 0.974690, 0.979840, 0.952730 },
    { 0.828750, 0.868090, 0.834920, 0.905510, 0.871530 },
    { 0.781540, 0.782470, 0.767910, 0.764140, 0.795890 },
    { 0.743460, 0.693390, 0.514870, 0.630150, 0.715660 },
    { 0.934760, 0.957870, 0.959640, 0.972510, 0.981640 }},

    {{ 0.965840, 0.941240, 0.987100, 1.022540, 1.011160 },
    { 0.988630, 0.994770, 0.976590, 0.950000, 1.034840 },
    { 0.958200, 1.018080, 0.974480, 0.920000, 0.989870 },
    { 0.811720, 0.869090, 0.812020, 0.850000, 0.821050 },
    { 0.682030, 0.679480, 0.632450, 0.746580, 0.738550 },
    { 0.668290, 0.445860, 0.500000, 0.678920, 0.696510 },
    { 0.926940, 0.953350, 0.959050, 0.876210, 0.991490 }},

    {{ 0.948940, 0.997760, 0.850000, 0.826520, 0.998470 },
    { 1.017860, 0.970000, 0.850000, 0.700000, 0.988560 },
    { 1.000000, 0.950000, 0.850000, 0.606240, 0.947260 },
    { 1.000000, 0.746140, 0.751740, 0.598390, 0.725230 },
    { 0.922210, 0.500000, 0.376800, 0.517110, 0.548630 },
    { 0.500000, 0.450000, 0.429970, 0.404490, 0.539940 },
    { 0.960430, 0.881630, 0.775640, 0.596350, 0.937680 }}},

    {{{ 1.030000, 1.040000, 1.000000, 1.000000, 1.049510 },
    { 1.050000, 0.990000, 0.990000, 0.950000, 0.996530 },
    { 1.050000, 0.990000, 0.990000, 0.820000, 0.971940 },
    { 1.050000, 0.790000, 0.880000, 0.820000, 0.951840 },
    { 1.000000, 0.530000, 0.440000, 0.710000, 0.928730 },
    { 0.540000, 0.470000, 0.500000, 0.550000, 0.773950 },
    { 1.038270, 0.920180, 0.910930, 0.821140, 1.034560 }},

    {{ 1.041020, 0.997520, 0.961600, 1.000000, 1.035780 },
    { 0.948030, 0.980000, 0.900000, 0.950360, 0.977460 },
    { 0.950000, 0.977250, 0.869270, 0.800000, 0.951680 },
    { 0.951870, 0.850000, 0.748770, 0.700000, 0.883850 },
    { 0.900000, 0.823190, 0.727450, 0.600000, 0.839870 },
    { 0.850000, 0.805020, 0.692310, 0.500000, 0.788410 },
    { 1.010090, 0.895270, 0.773030, 0.816280, 1.011680 }},

    {{ 1.022450, 1.004600, 0.983650, 1.000000, 1.032940 },
    { 0.943960, 0.999240, 0.983920, 0.905990, 0.978150 },
    { 0.936240, 0.946480, 0.850000, 0.850000, 0.930320 },
    { 0.816420, 0.885000, 0.644950, 0.817650, 0.865310 },
    { 0.742960, 0.765690, 0.561520, 0.700000, 0.827140 },
    { 0.643870, 0.596710, 0.474460, 0.600000, 0.651200 },
    { 0.971740, 0.940560, 0.714880, 0.864380, 1.001650 }},

    {{ 0.995260, 0.977010, 1.000000, 1.000000, 1.035250 },
    { 0.939810, 0.975250, 0.939980, 0.950000, 0.982550 },
    { 0.876870, 0.879440, 0.850000, 0.900000, 0.917810 },
    { 0.873480, 0.873450, 0.751470, 0.850000, 0.863040 },
    { 0.761470, 0.702360, 0.638770, 0.750000, 0.783120 },
    { 0.734080, 0.650000, 0.600000, 0.650000, 0.715660 },
    { 0.942160, 0.919100, 0.770340, 0.731170, 0.995180 }},

    {{ 0.952560, 0.916780, 0.920000, 0.900000, 1.005880 },
    { 0.928620, 0.994420, 0.900000, 0.900000, 0.983720 },
    { 0.913070, 0.850000, 0.850000, 0.800000, 0.924280 },
    { 0.868090, 0.807170, 0.823550, 0.600000, 0.844520 },
    { 0.769570, 0.719870, 0.650000, 0.550000, 0.733500 },
    { 0.580250, 0.650000, 0.600000, 0.500000, 0.628850 },
    { 0.904770, 0.852650, 0.708370, 0.493730, 0.949030 }},

    {{ 0.911970, 0.800000, 0.800000, 0.800000, 0.956320 },
    { 0.912620, 0.682610, 0.750000, 0.700000, 0.950110 },
    { 0.653450, 0.659330, 0.700000, 0.600000, 0.856110 },
    { 0.648440, 0.600000, 0.641120, 0.500000, 0.695780 },
    { 0.570000, 0.550000, 0.598800, 0.400000, 0.560150 },
    { 0.475230, 0.500000, 0.518640, 0.339970, 0.520230 },
    { 0.743440, 0.592190, 0.603060, 0.316930, 0.794390 }}}};

double ModifiedDISC(const double g[3], const double z[3], double td, double alt, int doy, double &dn) // aka DIRINT model
{
    // Modification history:
	// 25/10/2015 Converted to C++ for use in SAM by David Severin Ryberg
    // 4/14/2015 Corrected error in incrementing i,j, and k array indices 
    // 7/16/13. Converted by Bill Marion to C#. The 7/5/91 version provided by Daryl that this
    //  is based on was significantly different than the 6/26/91 I had got from Martin years ago in that
    //  the section on bin interpolating for clear stable cases had been removed.
	// 6/28/2013. Converted to C# from Howard Bisner FORTRAN77 code
    // 5/24/91. Richard's code to do linear interpolation between highest kt' bins added by RS. 
    //  RS fixed some typos in Richard's untested code.
    // 6/10/91. Corrected bin interpolation near label 141 [divide by 0.007 and zbin2 not zbin]
    // 6/26/91: Richard perez: Modification of DKT1 calculation
    //  to avoid very low sun distorsion caused by questionable     
    //  cosine response of pyranometers    
    // 7/5/91:  RS: lines extending beyond col 72 fixed.
    //  Questionable use of x**-y changed to x**(-y)
    //  Made reference to intrinsic dmax1 agree with type

	double cz[3], zenith[3], kt[3], am[3], ktpam[3], kt1[3];

    double ktbin[5] = { 0.24, 0.4, 0.56, 0.7, 0.8 };
    double zbin[5] = { 25.0, 40.0, 55.0, 70.0, 80.0 };
    double dktbin[5] = { 0.015, 0.035, 0.07, 0.15, 0.3 };
    double wbin[3] = { 1.0, 2.0, 3.0 };
    double rtod = 57.295779513082316;
    double a, b, c, w, knc, bmax, dkt1, io;

    //double dn = 0.0;
    if (g[1] >= 1.0 && cos(z[1]) > 0.0)
    {   // Model only if present global >= 1 and present zenith < 90 deg
        io = 1367.0 * (1.0 + 0.033 * cos(0.0172142 * doy));    // Extraterrestrial dn
        int j = 0, k = 2, i = 0, l = 0;
        if (g[0] < -998.0 || z[0] < -998.0)
        {   // Prehour global and zenith were passed missing -999.0
            j = 1;
            kt1[0] = -999.0;
        }
        if (g[2] < -998.0 || z[2] < -998.0)
        {   // Posthour global and zenith were passed missing -999.0
            k = 1;
            kt1[2] = -999.0;
        }
        for (i = j; i <= k; i++)
        {   // For each of the 3 hours that have data, find kt prime
            cz[i] = cos(z[i]); // Cosine of zenith angle
            if (cz[i] < 0.0)
                kt1[i] = -999.0;
            else
            {
                zenith[i] = z[i] * rtod;
                kt[i] = g[i] / (io * Max(0.065, cz[i]));   // Kt
                am[i] = Min(15.25, 1.0 / (cz[i] + 0.15 * (pow(93.9 - zenith[i], -1.253))));
                ktpam[i] = am[i] * exp(-0.0001184 * alt);
                kt1[i] = kt[i] / (1.031 * exp(-1.4 / (0.9 + 9.4 / ktpam[i])) + 0.1);   // Kt prime
            }
        }
        if (kt[1] <= 0.6)
        {
            a = 0.512 - 1.56 * kt[1] + 2.286 * pow(kt[1], 2.0) - 2.22 * pow(kt[1], 3.0);
            b = 0.37 + 0.962 * kt[1];
            c = -0.28 + 0.932 * kt[1] - 2.048 * pow(kt[1], 2.0);
        }
        else
        {
            a = -5.743 + 21.77 * kt[1] - 27.49 * pow(kt[1], 2.0) + 11.56 * pow(kt[1], 3.0);
            b = 41.40 - 118.5 * kt[1] + 66.05 * pow(kt[1], 2.0) + 31.9 * pow(kt[1], 3.0);
            c = -47.01 + 184.2 * kt[1] - 222.0 * pow(kt[1], 2.0) + 73.81 * pow(kt[1], 3.0);
        }
        knc = 0.866 - 0.122 * am[1] + 0.0121 * pow(am[1], 2.0) - 0.000653 * pow(am[1], 3.0) + 0.000014 * pow(am[1], 4.0);
        bmax = io * (knc - (a + b * exp(c * am[1])));
        if (kt1[0] < -998.0 && kt1[2] < -998.0)
            k = 6;
        else
        {
            if (kt1[0] < -998.0 || zenith[0] >= 85.0)
                dkt1 = fabs(kt1[2] - kt1[1]);
            else if (kt1[2] < -998.0 || zenith[2] >= 85.0)
                dkt1 = fabs(kt1[1] - kt1[0]);
            else
                dkt1 = 0.5 * (fabs(kt1[1] - kt1[0]) + fabs(kt1[2] - kt1[1]));
            
            k = 0;
            //while (k < 4 && dkt1 >= dktbin[k]) 
            while (k < 5 && dkt1 >= dktbin[k])      // Error fix 4/14/2015
                k++;
        }
        i = 0;
        //while (i < 4 && kt1[1] >= ktbin[i]) 
        while (i < 5 && kt1[1] >= ktbin[i])         // Error fix 4/14/2015
            i++;
        j = 0;
        //while (j < 4 && zenith[1] >= zbin[j]) 
        while (j < 5 && zenith[1] >= zbin[j])       // Error fix 4/14/2015
            j++;
        if (td < -998.0)
            l = 4;  // l = letter "l'
        else
        {
            w = exp(-0.075 + 0.07 * td);
            l = 0;
            while (l < 3 && w >= wbin[l])
                l++;
        }
        dn = bmax * cm[i][j][k][l];
        // dn = Max(0.0, dn); //jmf removed 11/30/18 to allow error to be reported by poaDecomp if calculated dn is negative
    }   // End of if present global >= 1

    return kt1[1];
}   // End of ModifiedDISC



void ModifiedDISC(const double kt[3], const double kt1[3], const double g[3], const double z[3], double td, double /*alt*/, int doy, double &dn) // aka DIRINT model
{
	// Calculates direct normal (beam) radiation from global horizontal radiation.
	double cz[3], zenith[3], am[3];
	double ktbin[5] = { 0.24, 0.4, 0.56, 0.7, 0.8 };
	double zbin[5] = { 25.0, 40.0, 55.0, 70.0, 80.0 };
	double dktbin[5] = { 0.015, 0.035, 0.07, 0.15, 0.3 };
	double wbin[3] = { 1.0, 2.0, 3.0 };
	double rtod = 57.295779513082316;
	double a, b, c, w, knc, bmax, dkt1, io;

	//double dn = 0.0;
	if (g[1] >= 1.0 && cos(z[1]) > 0.0)
	{   // Model only if present global >= 1 and present zenith < 90 deg

		//std::cout << "yes!\n";

		io = 1367.0 * (1.0 + 0.033 * cos(0.0172142 * doy));    // Extraterrestrial dn
		int j = 0, k = 2, i = 0, l = 0;

		for (i = j; i <= k; i++)
		{   // For each of the 3 hours that have data, find kt prime
			cz[i] = cos(z[i]); // Cosine of zenith angle
			zenith[i] = z[i] * rtod;
			am[i] = Min(15.25, 1.0 / (cz[i] + 0.15 * (pow(93.9 - zenith[i], -1.253))));
		}
		if (kt[1] <= 0.6)
		{
			a = 0.512 - 1.56 * kt[1] + 2.286 * pow(kt[1], 2.0) - 2.22 * pow(kt[1], 3.0);
			b = 0.37 + 0.962 * kt[1];
			c = -0.28 + 0.932 * kt[1] - 2.048 * pow(kt[1], 2.0);
		}
		else
		{
			a = -5.743 + 21.77 * kt[1] - 27.49 * pow(kt[1], 2.0) + 11.56 * pow(kt[1], 3.0);
			b = 41.40 - 118.5 * kt[1] + 66.05 * pow(kt[1], 2.0) + 31.9 * pow(kt[1], 3.0);
			c = -47.01 + 184.2 * kt[1] - 222.0 * pow(kt[1], 2.0) + 73.81 * pow(kt[1], 3.0);
		}
		knc = 0.866 - 0.122 * am[1] + 0.0121 * pow(am[1], 2.0) - 0.000653 * pow(am[1], 3.0) + 0.000014 * pow(am[1], 4.0);
		bmax = io * (knc - (a + b * exp(c * am[1])));
		//std::cout << "Kt: " << kt[1] << std::endl;
		//std::cout << io << " " << knc << " " << a << " " << b << " " << c << " " << am[1] << std::endl;


		if (kt1[0] < -998.0 && kt1[2] < -998.0)
			k = 6;
		else
		{
			if (kt1[0] < -998.0 || zenith[0] >= 85.0)
				dkt1 = fabs(kt1[2] - kt1[1]);
			else if (kt1[2] < -998.0 || zenith[2] >= 85.0)
				dkt1 = fabs(kt1[1] - kt1[0]);
			else
				dkt1 = 0.5 * (fabs(kt1[1] - kt1[0]) + fabs(kt1[2] - kt1[1]));

			k = 0;
			//while (k < 4 && dkt1 >= dktbin[k]) 
			while (k < 5 && dkt1 >= dktbin[k])      // Error fix 4/14/2015
				k++;
		}
		i = 0;
		//while (i < 4 && kt1[1] >= ktbin[i]) 
		while (i < 5 && kt1[1] >= ktbin[i])         // Error fix 4/14/2015
			i++;
		j = 0;
		//while (j < 4 && zenith[1] >= zbin[j]) 
		while (j < 5 && zenith[1] >= zbin[j])       // Error fix 4/14/2015
			j++;
		if (td < -998.0)
			l = 4;  // l = letter "l'
		else
		{
			w = exp(-0.075 + 0.07 * td);
			l = 0;
			while (l < 3 && w >= wbin[l])
				l++;
		}

		dn = bmax * cm[i][j][k][l];
		// dn = Max(0.0, bmax * cm[i][j][k][l]); //jmf removed 11/30/18 to allow error to be reported by poaDecomp if calculated dn is negative
		//std::cout << dn << " " << bmax << " " << cm[i][j][k][l] << std::endl;
	}   // End of if present global >= 1
	else
		dn = 0;
	return;
}   // End of ModifiedDISC
