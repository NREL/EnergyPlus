/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */
/**************************************************************
 * C Language Implementation of DOE2.1d and Superlite 3.0
 * Daylighting Algorithms with new Complex Fenestration System
 * analysis algorithms.
 *
 * The original DOE2 daylighting algorithms and implementation
 * in FORTRAN were developed by F.C. Winkelmann at the
 * Lawrence Berkeley National Laboratory.
 *
 * The original Superlite algorithms and implementation in FORTRAN
 * were developed by Michael Modest and Jong-Jin Kim
 * under contract with Lawrence Berkeley National Laboratory.
 **************************************************************/

// This work was supported by the Assistant Secretary for Energy Efficiency
// and Renewable Energy, Office of Building Technologies,
// Building Systems and Materials Division of the
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

/*
NOTICE: The Government is granted for itself and others acting on its behalf
a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce,
prepare derivative works, and perform publicly and display publicly.
Beginning five (5) years after (date permission to assert copyright was obtained),
subject to two possible five year renewals, the Government is granted for itself
and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
license in this data to reproduce, prepare derivative works, distribute copies to
the public, perform publicly and display publicly, and to permit others to do so.
NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
*/
#pragma warning(disable:4786)

// Standard includes
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <map>
#include <limits>
#include <algorithm> // for min/max
using namespace std;

// BGL includes
#include "BGL.h"
namespace BGL = BldgGeomLib;

// includes
#include "CONST.H"
#include "DBCONST.H"
#include "DEF.H"

// WLC includes
#include "NodeMesh2.h"
#include "WLCSurface.h"
#include "helpers.h"
#include "hemisphiral.h"
#include "btdf.h"
#include "CFSSystem.h"
#include "CFSSurface.h"

// includes
#include "DOE2DL.H"
#include "TOOLS.H"
#include "WxTMY2.h"
#include "ECM.H"
#include "SOL.H"

/******************************** subroutine dillum *******************************/
/* Calculates daylight illuminance levels (fc) for combined overcast sky, */
/* clear sky, and clear sun components at each reference point defined in the */
/* REFPT structure within BLDG structure. */
/* Determines electric lighting fractional power required to meet aggregate design */
/* set point illuminance for each zone. */
/* Calculates monthly and annual average hourly fractional electric lighting reductions */
/* due to daylight over the given run period. */
/* Run period is defined in the RUN_DATA structure. */
/* Based on code contained in DOE2.1D DAYCLC subroutine. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine dillum *******************************/
int	dillum(
	double cloud_fraction,	/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	BLDG *bldg_ptr,			/* building data structure pointer */
	SUN_DATA *sun_ptr,		/* pointer to sun data structure */
	RUN_DATA *run_ptr,		/* pointer to runtime data structure */
	int wx_flag,			/* weather availability flag */
	FILE *wxfile_ptr,		/* TMY2 weather file pointer */
	ofstream* pofdmpfile)	/* ptr to dump file */
{
	int monlength[MONTHS];	/* number of days in each month array */
	int dayofyr;			/* sequential day of year */
	int dayofweek;			/* sequential day of week (1=Mon to 7=Sun) */
	SUN1_DATA sun1_data;	/* sun1 data structure for sun1() subroutine */
	SUN2_DATA sun2_data;	/* sun2 data structure for sun2() subroutine */
	double solic[MONTHS];	/* extraterrestrial illum for first of each month */
	double chilsk[HOURS];	/* clear sky horiz illum sky component */
	double chilsu[HOURS];	/* clear sky horiz illum sun component */
	double ohilsk[HOURS];	/* overcast sky horiz illum sky component */
	double cdirlw[HOURS];	/* luminous efficacy for direct solar radiation from clear sky */
	double cdiflw[HOURS];	/* luminous efficacy for diffuse radiation from clear sky */
	double odiflw[HOURS];	/* luminous efficacy for diffuse radiation from overcast sky */
	double hisunf;	/* current hour clear sky horiz illum sun component */
	double chiskf;	/* current hour clear sky horiz illum sky component */
	double ohiskf;	/* current hour overcast sky horiz illum sky component */
	double phsun, thsun;		/* sun altitude and azimuth (radians) */
	double phsmin, phsmax, phsdel;	/* sun altitude limits and increment */
	double thsmin, thsmax, thsdel;	/* sun azimuth limits and increment */
	double phratio, thratio;	/* sun position alt and azm interpolation displacement ratios */
	int iphs, iths;	/* sun position alt and azm interpolation indexes */
	int imon;	/* month loop index (jan = 0) */
	int iday;	/* day loop index (month begins at iday = 1) */
	int ihr;	/* hour loop index (Midnite to 1AM = 0) */
	int izone, irp;	/* loop indexes */
	int iday1, iday2;	/* indexes */
	int anndays, mondays;	/* accumulators for annual and monthly average calcs */
	double lt_frac, lt_reduc;	/* temp light fraction and reduction vars */

	int iReturnVal = 0;		/* return value holder */

	/* Initialize month lengths array. */
	int iFebDays = 28;
	/* Does wx file contain leap year data? */
	// currently hardwired to NO
	if (0)
		iFebDays = 29;
	init_monlength(monlength, iFebDays);

	/* Set limits of sun position angles. */
	phsmin = sun_ptr->phsmin;
	thsmin = sun_ptr->thsmin;

	/* For non-standard number of sun altitudes minimum altitude is passed into dcof(). */
	/* Reset minimum altitude for standard number of sun altitudes. */
	if (sun_ptr->nphs == NPHS) {
		phsmin = 10.;
		if (fabs(bldg_ptr->lat) >= 48.0) phsmin = 5.;
	}
	/* Maximum altitude and altitude angle increment for sun positions. */
	if (sun_ptr->nphs == 1) {
		phsmax = phsmin;
		phsdel = 0.;
	}
	else {
		phsmax = min(90.0,113.5-fabs(bldg_ptr->lat));
		phsdel = (phsmax - phsmin) / ((double)(sun_ptr->nphs-1));
	}

	/* For non-standard number of sun azimuths minimum azimuth is passed into dcof(). */
	/* Reset minimum azimuth and azm angle increment for standard number of sun azimuths. */
	if (sun_ptr->nths == NTHS) {
		thsmin = -110.;
		/* minimum solar azimuth for southern hemisphere */
		if (bldg_ptr->lat < 0.0) thsmin = 70.;
	}
	/* Azimuth angle increment for sun positions. */
	if (sun_ptr->nths == 1) thsdel = 0.;
	else thsdel = fabs(2.0 * thsmin) / (sun_ptr->nths - 1);
	thsmax = thsmin + thsdel * (double)(NTHS-1);

	/* Calculate extraterrestrial direct normal solar illuminance (lum/ft2) */
	/* for the first day of each month. */
	dsolic(solic);

	/* Calculate sequential beginning day of year for this run (Jan 01 = 1) */
	dayofyr = 0;
	for (imon=0; imon<MONTHS; imon++) {
		if (imon == (run_ptr->mon_begin -1)) {
			dayofyr += run_ptr->day_begin;
			break;
		}
		dayofyr += monlength[imon];
	}
	/* Decrement dayofyr in anticipation of first Day Loop increment */
	dayofyr -= 1;

	/* Get day of week for first day of run */
	get_day_of_week(&dayofweek,run_ptr);
	/* Decrement dayofweek in anticipation of first Day Loop increment */
	dayofweek -= 1;

	/* Calculate sequential begin and end days of year for all lighting schedules */
	calc_sched_days(bldg_ptr,run_ptr);

	/* Output power reduction factor headings. */
	*pofdmpfile << "\n";

	/* Init number of hours in year for annual average hourly electric lighting reduction. */
	anndays = 0;

	/* Month Loop */
	for (imon=(run_ptr->mon_begin-1); imon<run_ptr->mon_end; imon++) {
		/* Output month corrected so that Jan=1 to Dec=12. */
		*pofdmpfile << "Month: " << imon+1 << "\n";

		/* Init number of days in month for monthly average hourly electric lighting reduction. */
		mondays = 0;

		/* Calculate beginning and ending days for the day loop. */
		if (imon == (run_ptr->mon_begin -1)) iday1 = run_ptr->day_begin;
		else iday1 = 1;
		if (imon == (run_ptr->mon_end -1)) iday2 = run_ptr->day_end;
		else iday2 = monlength[imon];

		/* Init monthly availability arrays */
		init_avail(chilsk,chilsu,ohilsk,cdirlw,cdiflw,odiflw);

		/* Day Loop */
		for (iday=iday1; iday<=iday2; iday++) {
			/* Increment day of year */
			dayofyr += 1;

			/* Increment day of week */
			dayofweek += 1;
			if (dayofweek > 7) dayofweek = 1;

			/* NOTE: For applications without solar data only do calcs for one day each */
			/* month but, continue to loop through days for proper increment of dayofyr */
			/* and dayofweek */
			if ((wx_flag == 0) && (iday > iday1)) continue;

			/* Output day. */
			*pofdmpfile << "Day: " << iday << " DayofWeek " << dayofweek << "\n";

			/* Count number of days simulated in month for monthly average */
			/* hourly electric lighting reduction. */
			mondays += 1;

			/* Get lighting schedule index for each zone for current day */
            int iGetSchedRetVal;
			if ((iGetSchedRetVal = get_sched(bldg_ptr,dayofyr,dayofweek)) < 0) {
                // If errors were detected then return now, else register warnings and continue processing
                if (iGetSchedRetVal != -10) {
					*pofdmpfile << "ERROR: DElight Zone Lighting Schedule not found for at least one Zone\n";
					return(-1);
                }
                else {
                    iReturnVal = -10;
                }
            }

			/* Get daily solar quantities */
			sun1(dayofyr,&sun1_data,bldg_ptr);

			/* Hour Loop */
			for (ihr=0; ihr<HOURS; ihr++) {
				/* Output dmpfile hour. */
				*pofdmpfile << "Hour: " << ihr+1 << "\n";

				/* Get hourly solar quantities */
                int iSun2RetVal;
				if ((iSun2RetVal = sun2(imon,iday,ihr,&sun1_data,&sun2_data,bldg_ptr,wx_flag,wxfile_ptr)) < 0) {
                    // If errors were detected then return now, else register warnings and continue processing
                    if (iSun2RetVal != -10) {
					    *pofdmpfile << "ERROR: DElight Bad return from sun2(), return from dillum()\n";
					    return(-1);
                    }
                    else {
                        iReturnVal = -10;
                    }
                }

                /* Is sun not up? */
				if (sun2_data.isunup == 0) {
					/* Output 100% power required for current hour (1 to 24) */
					/* For each zone */
					for (izone=0; izone<bldg_ptr->nzones; izone++) {
						/* Output to dump file */
						*pofdmpfile << "Zone [" << bldg_ptr->zone[izone]->name << " PRF = " << 1.0 << " Percent Savings = " << 0.0 << "\n";
					}
					continue;
				}


				/* Calc solar alt and azm and interpolation ratios and bound indexes */
				calc_sun(&phsun,&thsun,&phratio,&thratio,&iphs,&iths,&sun2_data,phsmin,phsmax,phsdel,thsmin,thsmax,thsdel,bldg_ptr);

				/* Output sun position. */
				*pofdmpfile << "Sun Altitude: " << phsun/DTOR << " Sun Azimuth: " << thsun/DTOR << "\n";

				/* NOTE: If weather data is not available, set cloudiness fraction equal to */
				/* value passed into dillum(); */
				if (wx_flag == 0) sun2_data.cldamt = (int)(cloud_fraction * 10.);

				/* Calc hourly values for each hour sun is up for first day of each month */
				if (iday == iday1) {
					/* Calc exterior daylight availability factors */
                    int iDavailRetVal;
					if ((iDavailRetVal = davail(&chilsk[ihr],&chilsu[ihr],&ohilsk[ihr],&cdirlw[ihr],&cdiflw[ihr],&odiflw[ihr],imon,phsun,thsun,solic,bldg_ptr,pofdmpfile)) < 0) {
                        // If errors were detected then return now, else register warnings and continue processing
                        if (iDavailRetVal != -10) {
					        *pofdmpfile << "ERROR: DElight Bad return from davail(), return from dillum()\n";
					        return(-1);
                        }
                        else {
                            iReturnVal = -10;
                        }
                    }
				}

				/* Calc current hour illum on an unobstructed exterior horizontal surface */
                int iDextilRetVal;
				if ((iDextilRetVal = dextil(&hisunf,&chiskf,&ohiskf,wx_flag,chilsu[ihr],chilsk[ihr],ohilsk[ihr],phsun,solic,imon,bldg_ptr,&sun2_data,pofdmpfile)) < 0) {
                    // If errors were detected then return now, else register warnings and continue processing
                    if (iDextilRetVal != -10) {
                        *pofdmpfile << "\n";
					    *pofdmpfile << "ERROR: DElight Bad return from dextil(), return from dillum()\n";
					    return(-1);
                    }
                    else {
                        iReturnVal = -10;
                    }
                }

				/* Zone Loop */
				for (izone=0; izone<bldg_ptr->nzones; izone++) {
					/* Find daylight illuminance level */
					/* at each ref pt in current zone. */
					dintil(bldg_ptr->zone[izone],imon,ihr,hisunf,chiskf,ohiskf,iphs,iths,phratio,thratio);

					/* Calculate lighting power reduction factor due to daylighting. */
					/* 	PRF = 1.0 => full power required */
					/* 	PRF = 0.0 => no power required */
                    int iDltsysRetVal;
				    if ((iDltsysRetVal = dltsys(bldg_ptr->zone[izone],&sun2_data,pofdmpfile)) < 0) {
                        // If errors were detected then return now, else register warnings and continue processing
                        if (iDltsysRetVal != -10) {
					        *pofdmpfile << "ERROR: DElight Bad return from dltsys(), return from dillum()\n";
					        return(-1);
                        }
                        else {
                            iReturnVal = -10;
                        }
                    }

					/* Calculate hourly fractional electric lighting energy requirement, */
					/* accounting for electric lighting schedule. */
					lt_frac = bldg_ptr->zone[izone]->frac_power * bldg_ptr->zone[izone]->ltsch[bldg_ptr->zone[izone]->ltsch_id]->frac[ihr];

					/* Calculate hourly fractional electric lighting energy reduction. */
					lt_reduc = 1.0 - lt_frac;

					/* Output power reduction factor and electric lighting savings for this zone. */
					*pofdmpfile << "Zone [" << bldg_ptr->zone[izone]->name << " PRF = " << bldg_ptr->zone[izone]->frac_power << " Percent Savings = " << (lt_reduc*100.0) << "\n";

					/* Accumulate monthly hourly fractional electric lighting energy reduction. */
					bldg_ptr->zone[izone]->lt_reduc[imon][ihr] += lt_reduc;

				}	/* end of Zone Loop */
			}	/* end of Hour Loop */
		}	/* end of Day Loop */

		/* Calculate monthly average hourly fractional electric lighting energy reduction, */
		/* and daylight illuminances at each ref pt. */
		for (izone=0; izone<bldg_ptr->nzones; izone++) {
			for (ihr=0; ihr<HOURS; ihr++) {
				/* Accumulate annual hourly fractional electric lighting energy reduction. */
				bldg_ptr->zone[izone]->annual_reduc[ihr] += bldg_ptr->zone[izone]->lt_reduc[imon][ihr];
				if (mondays != 0) bldg_ptr->zone[izone]->lt_reduc[imon][ihr] /= (double)mondays;
				for (irp=0; irp<bldg_ptr->zone[izone]->nrefpts; irp++) {
					if (mondays != 0) {
						bldg_ptr->zone[izone]->ref_pt[irp]->day_illum[imon][ihr] /= (double)mondays;
					}
				}
			}
		}
		/* Accumulate annual number of hours simulated for annual average reduction calcs. */
		anndays += mondays;
	}	/* end of Month Loop */

	/* Calculate annual average hourly fractional electric lighting energy reduction. */
	for (izone=0; izone<bldg_ptr->nzones; izone++) {
		for (ihr=0; ihr<HOURS; ihr++) {
			if (anndays != 0) bldg_ptr->zone[izone]->annual_reduc[ihr] /= (double)anndays;
		}
	}

	return(iReturnVal);
}

/******************************** subroutine davail *******************************/
/* Calculates availability of natural light for daylighting simulation. */
/* Determines sun and sky illuminance on an exterior horizontal surface for clear */
/* and overcast CIE skies (lumens/ft2). */
/* Called once each hour that sun is up for one day per month. */
/* Also determines lumens/watt conversion factors for direct and diffuse solar */
/* radiation, based on CIE conventions, from CIE clear sky (cdirlw,cdiflw), */
/* and for diffuse radiation from CIE overcast sky (odiflw). */
/* 12/17/98 mod now calls dlumef() for use when there is NOT wx data. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine davail *******************************/
int	davail(
	double *chilsk_ptr,	/* clear sky horiz illum sky component */
	double *chilsu_ptr,	/* clear sky horiz illum sun component */
	double *ohilsk_ptr,	/* overcast sky horiz illum sky component */
	double *cdirlw_ptr,	/* luminous efficacy for direct solar radiation from clear sky */
	double *cdiflw_ptr,	/* luminous efficacy for diffuse radiation from clear sky */
	double *odiflw_ptr,	/* luminous efficacy for diffuse radiation from overcast sky */
	int imon,			/* current month */
	double phsun,		/* sun altitude */
	double thsun,		/* sun azimuth */
	double solic[MONTHS],/* extraterrestrial illum for first day of each month */
	BLDG *bldg_ptr,		/* building data structure pointer */
	ofstream* pofdmpfile)	/* ptr to dump file */
{
	double zenl;	/* zenith luminance for given month and sun altitude (Kcd/m2) */
	double tfac;	/* turbidity factor for given month and sun altitude */

    // Init return value
    int iReturnVal = 0;

	/* Get clear sky zenith luminance, moisture, and turbidity coef for current month. */
	dzenlm(&zenl,&tfac,imon,bldg_ptr,phsun);

	/* Get exterior horiz illum from sky and sun for clear and overcast sky.  */
    int iDhillRetVal;
	if ((iDhillRetVal = dhill(chilsk_ptr,chilsu_ptr,ohilsk_ptr,bldg_ptr,imon,phsun,thsun,zenl,tfac,solic,pofdmpfile)) < 0) {
        // If errors were detected then return now, else register warnings and continue processing
        if (iDhillRetVal != -10) {
			*pofdmpfile << "ERROR: DElight Bad return from dhill(), return from davail()\n";
			return(-1);
        }
        else {
            iReturnVal = -10;
        }
    }

    /* Get lumens/watt factors based on CIE conventions. */
	dlumef(cdirlw_ptr,cdiflw_ptr,odiflw_ptr,imon,phsun,bldg_ptr);

	return(iReturnVal);
}

/******************************** subroutine dlumef *******************************/
/* Called by davail(). */
/* Calculates luminous efficacy (lumens/watt) of direct, clear sky diffuse, */
/* and overcast sky diffuse solar radiation. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine dlumef *******************************/
int	dlumef(
	double *cdirlw_ptr,	/* luminous efficacy for direct solar radiation from clear sky */
	double *cdiflw_ptr,	/* luminous efficacy for diffuse radiation from clear sky */
	double *odiflw_ptr,	/* luminous efficacy for diffuse radiation from overcast sky */
	int imon,			/* current month */
	double phsun,		/* sun altitude */
	BLDG *bldg_ptr)		/* building data structure pointer */
{
	double beta, w, bc;	/* atmos turbidity and moisture coefs for given month */

	/* lumens/watt for direct solar radiation, clear sky (fit to tabulated values */
	/* of direct normal luminous efficacy vs solar altitude, turbidity factor, */
	/* moisture -- Aydinli, The Availability fo Solar Radiation and Daylight, */
	/* Table 4, Oct. 1981). */

	/* Set turbidity coef and atmos moisture for specified month. */
	beta = bldg_ptr->atmtur[imon];
	w = bldg_ptr->atmmoi[imon] * 2.54;

	/* Restrict beta to range of Aydinli values (0 to 0.2) */
	bc = min(0.2,beta);

	*cdirlw_ptr = (99.0 + 4.7 * w - 52.4 * bc) * (1.0 - exp((24.0 * bc - 8.0) * phsun));

	/* lumens/watt for diffuse radiation from clear sky (from Aydinli) */
	*cdiflw_ptr = 125.4;

	/* lumens/watt for diffuse radiation from overcast sky (from Dogniaux and Lemoine) */
	*odiflw_ptr = 110.;

	return(0);
}

/******************************** subroutine dplumef *******************************/
/* Called by dextil() if weather data is available. */
// Determines sky diffuse and direct normal luminous efficacy (lumens/watt) from
// measured diffuse horizontal and direct normal irradiance.
// Uses the method described in "Modeling daylight availability and irradiance
// components from direct and global irradiance," R. Perez, P. Ineichen, R. Seals,
// J. Michalsky and R. Stewart, Solar Energy 44, 271-289, 1990.
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine dplumef *******************************/
int	dplumef(
	double* pdirlw_ptr,	/* Perez luminous efficacy for direct solar radiation from clear sky */
	double* pdiflw_ptr,	/* Perez luminous efficacy for diffuse radiation from sky */
	double bscc,		/* diffuse horiz radiation from measured data (Btu/ft2-h) */
	double rdncc,		/* measured direct solar radiation (Btu/ft2-h) */
	double phsun,		/* sun altitude (radians) */
	double solic[MONTHS],/* extraterrestrial illum for first day of each month */
	int imon,			/* current month */
	SUN2_DATA *sun2_ptr,/* pointer to sun2 data structure */
	BLDG *bldg_ptr,		/* building data structure pointer */
	ofstream* pofdmpfile)/* dump file */
{
	// Diffuse luminous efficacy coefficients.
	double apdiflw[8] = {97.24, 107.22, 104.97, 102.39, 100.71, 106.42, 141.88, 152.23};
	double bpdiflw[8] = {-0.46, 1.15,   2.96,   5.59,   5.94,   3.83,   1.90,   0.35};
	double cpdiflw[8] = {12.00, 0.59,   -5.53,  -13.95, -22.75, -36.15, -53.24, -45.27};
	double dpdiflw[8] = {-8.91, -3.95,  -8.77,  -13.90, -23.74, -28.83, -14.03, -7.98};

	// Direct luminous efficacy coefficients.
	double apdirlw[8] = {57.20, 98.99,  109.83, 110.34, 106.36, 107.19, 105.75, 101.18};
	double bpdirlw[8] = {-4.55, -3.46,  -4.90,  -5.84,  -3.97,  -1.25,  0.77,   1.58};
	double cpdirlw[8] = {-2.98, -1.21,  -1.71,  -1.99,  -1.75,  -1.51,  -1.26,  -1.10};
	double dpdirlw[8] = {117.12,12.38,  -8.81,  -4.56,  -6.16,  -26.73, -34.44, -8.29};

	// Solar zenith angle (radians).
	double zenith_angle = PIOVR2 - phsun;

	// Sky clearness.
	// EPS near 1 is overcast sky.
	// EPS >= 6 is clear sky.
	double z3k = 1.041 * zenith_angle * zenith_angle * zenith_angle;
	double eps = ((bscc + rdncc) / (bscc + 0.0001) + z3k) / (1.0 + z3k);

	// Relative optical air mass corrected for building altitude in kilometers.
	double phsun_deg = phsun / DTOR;
	double lop = phsun_deg + 3.885;
	double powlop;
	if (lop < 0.0) {
		*pofdmpfile << "ERROR: DElight Invalid sun altitude (" << phsun_deg << " passed to dplumef()\n";
		return(-1);
	}
	else powlop = pow(lop,1.253);
	double sphsun = sin(phsun);
	double air_mass = (1.0 - 0.1 * bldg_ptr->alt / 3281.0) / (sphsun + 0.15 / powlop);

	// Sky brightness.
	// Solic is extraterrestrial direct normal illuminance (lum/ft2).
	// 27.463 = 93.73*0.293 = extraterrestrial luminous efficacy in lum/btuh.
	double del = (bscc * air_mass / solic[imon]) * 27.463;

	// Sky clearness bin.
	int ieps;
	if (eps <= 1.065) ieps = 0;
	else if ((eps > 1.065) && (eps <= 1.23)) ieps = 1;
	else if ((eps > 1.23) && (eps <= 1.5)) ieps = 2;
	else if ((eps > 1.5) && (eps <= 1.95)) ieps = 3;
	else if ((eps > 1.95) && (eps <= 2.8)) ieps = 4;
	else if ((eps > 2.8) && (eps <= 4.5)) ieps = 5;
	else if ((eps > 4.5) && (eps <= 6.2)) ieps = 6;
	else ieps = 7;

	// Atmospheric moisture (cm).
	double wch = exp(0.0389 * (sun2_ptr->dewpt - 32.0) - 0.075);

	// Sky diffuse luminous efficacy (lumens/watt).
	if (del <= 0.0) *pdiflw_ptr = 0.0;
	else *pdiflw_ptr = apdiflw[ieps] + bpdiflw[ieps]*wch + cpdiflw[ieps]*cos(zenith_angle) + dpdiflw[ieps]*log(del);

	// Direct normal luminous efficacy (lumens/watt).
	if (del <= 0.0) *pdirlw_ptr = 0.0;
	else *pdirlw_ptr = max(0.0,(apdirlw[ieps] + bpdirlw[ieps]*wch + cpdirlw[ieps]*exp(5.73*zenith_angle-5.0) + dpdirlw[ieps]*del));

	return(0);
}

/****************************** subroutine init_avail *****************************/
/* Initializes monthly availability arrays. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine init_avail *****************************/
int init_avail(
	double chilsk[HOURS],	/* clear sky horiz illum sky component */
	double chilsu[HOURS],	/* clear sky horiz illum sun component */
	double ohilsk[HOURS],	/* overcast sky horiz illum sky component */
	double cdirlw[HOURS],	/* luminous efficacy for direct solar radiation from clear sky */
	double cdiflw[HOURS],	/* luminous efficacy for diffuse radiation from clear sky */
	double odiflw[HOURS])	/* luminous efficacy for diffuse radiation from overcast sky */
{
	int ihr;	/* hour index */

	for (ihr=0; ihr<HOURS; ihr++) {
		chilsk[ihr] = 0.;
		chilsu[ihr] = 0.;
		ohilsk[ihr] = 0.;
		cdirlw[ihr] = 0.;
		cdiflw[ihr] = 0.;
		odiflw[ihr] = 0.;
	}

	return(0);
}

/******************************** subroutine dextil *******************************/
/* Calculates current hour clear and overcast illuminance on an unobstructed */
/* horizontal surface (lumens/ft2). */
/* These illuminances are calculated from measured solar data if available */
/* from a weather file. */
/* Otherwise, illuminances are taken from davail(). */
/* 12/17/98 mods to incorporate Perez model for determining luminous efficacies */
/* when hourly solar data (irradiances) and dewpoint temp are available. */
// Previous method is retained for use when no wx data is available.
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine dextil *******************************/
int	dextil(
	double *hisunf_ptr,	/* current hour clear sky horiz illum sun component */
	double *chiskf_ptr,	/* current hour clear sky horiz illum sky component */
	double *ohiskf_ptr,	/* current hour overcast sky horiz illum sky component */
	int wx_flag,	/* weather data availability flag (1=avail) */
	double chilsu,	/* current hour clear sky horiz illum sun component from davail() */
	double chilsk,	/* current hour clear sky horiz illum sky component from davail() */
	double ohilsk,	/* current hour overcast sky horiz illum sky component from davail() */
	//double cdirlw,	/* CIE luminous efficacy for direct solar radiation from clear sky */
	//double cdiflw,	/* CIE luminous efficacy for diffuse radiation from clear sky */
	//double odiflw,	/* CIE luminous efficacy for diffuse radiation from overcast sky */
	double phsun,		/* sun altitude (radians) */
	double solic[MONTHS],/* extraterrestrial illum for first day of each month */
	int imon,			/* current month */
	BLDG *bldg_ptr,		/* building data structure pointer */
	SUN2_DATA *sun2_ptr,/* pointer to sun2 data structure */
	ofstream* pofdmpfile)/* dump file */
{
	double etacld;	/* cloudiness factor */
	double cr;		/* fractional cloud amount */
	double rdncc;	/* measured direct solar radiation */
	double bscc;		/* diffuse horiz radiation from measured data */
	double sdirh, sdifh;	/* direct and diffuse horiz illum */

	int iReturnVal = 0;	// return value holder

	/* Cloudiness factor, etacld, which is used to interpolate between */
	/* clear and overcast conditions. */
	cr = sun2_ptr->cldamt / 10.;
	if (cr > 0.2) etacld = 1.0 - (cr - 0.2) * 1.25;
	else etacld = 1.;

	/* Calculate illums when no wx data is available. */
	if (wx_flag == 0) {
		/* Direct horizontal illuminance */
		*hisunf_ptr = (1.0 - cr) * chilsu;

		/* Normalize clear and overcast portions of diffuse horiz illum. */
		*chiskf_ptr = etacld * chilsk;
		*ohiskf_ptr = (1.0 - etacld) * ohilsk;
	}
	else {	// Calculate illums from measured solar data if available.
		/* Calc intermediate vars RDNCC and BSCC from sun2_data */
		rdncc = sun2_ptr->dirsol;
		bscc = sun2_ptr->solrad - sun2_ptr->dirsol * sun2_ptr->raycos[2];
		if (bscc < 0.0) bscc = 0.;

		/* Calc direct and diffuse horiz illum from measured solar data (0.293 converts Btu/ft2-h to W/ft2) */
		sdirh = rdncc * sun2_ptr->raycos[2] * 0.293;
		sdifh = bscc * 0.293;

		// Get luminous efficacies calculated from measured solar data using Perez model.
		double pdirlw;	/* Perez luminous efficacy for direct solar radiation from clear sky */
		double pdiflw;	/* Perez luminous efficacy for diffuse radiation from sky */
        int iDplumefRetVal;
	    if ((iDplumefRetVal = dplumef(&pdiflw, &pdirlw, bscc, rdncc, phsun, solic, imon, sun2_ptr, bldg_ptr, pofdmpfile)) < 0) {
            // If errors were detected then return now, else register warnings and continue processing
            if (iDplumefRetVal != -10) {
			    *pofdmpfile << "ERROR: DElight Bad return from dplumef(), return from dextil()\n";
			    return(-1);
            }
            else {
                iReturnVal = -10;
            }
        }

        // Diffuse horiz illuminance (lumens/ft2).
		// Assume same diffuse efficacy for clear and overcast parts of sky.
		*chiskf_ptr = sdifh * etacld * pdiflw;
		*ohiskf_ptr = sdifh * (1.0 - etacld) * pdiflw;

		// Direct horiz illuminance (lumens/ft2).
		*hisunf_ptr = sdirh * pdirlw;
	}

	return(iReturnVal);
}

/******************************** subroutine dintil *******************************/
/* Calculates hourly daylight illuminance at each reference point in a daylit zone. */
/* Accumulates monthly and hourly total daylight illuminance for later monthly */
/* average calculations. */
/* Assumes no window shades. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine dintil *******************************/
int	dintil(
	ZONE *zone_ptr,	/* bldg->zone data structure pointer */
	int imon,		/* current month */
	int ihr,		/* current hour */
	double hisunf,	/* current hour clear sky horiz illum sun component */
	double chiskf,	/* current hour clear sky horiz illum sky component */
	double ohiskf,	/* current hour overcast sky horiz illum sky component */
	int iphs,		/* sun altitude interpolation lower bound index */
	int iths,		/* sun azimuth interpolation lower bound index */
	double phratio,	/* sun altitude interpolation displacement ratio */
	double thratio)	/* sun azimuth interpolation displacement ratio */
{
	int irp;				/* ref pt loop index */
	int ip_lo, ip_hi;		/* sun altitude low and high interpolation indexes */
	int it_lo, it_hi;		/* sun azimuth low and high interpolation indexes */
	double lower, upper;		/* temp interpolation lower and upper values */
	double skyfac, sunfac;	/* clear sky interpolated factors */

	/* Set low and high alt and azm indexes */
	ip_lo = iphs;
	if (iphs != (NPHS-1)) ip_hi = iphs + 1;
	else ip_hi = iphs;
	it_lo = iths;
	if (iths != (NTHS-1)) it_hi = iths + 1;
	else it_hi = iths;

	for (irp=0; irp<zone_ptr->nrefpts; irp++) {
		/* Interpolate clear sky daylight factors */
		upper = (zone_ptr->ref_pt[irp]->dfsky[ip_hi][it_hi] - zone_ptr->ref_pt[irp]->dfsky[ip_hi][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsky[ip_hi][it_lo];
		lower = (zone_ptr->ref_pt[irp]->dfsky[ip_lo][it_hi] - zone_ptr->ref_pt[irp]->dfsky[ip_lo][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsky[ip_lo][it_lo];
		skyfac = (upper - lower) * phratio + lower;
		upper = (zone_ptr->ref_pt[irp]->dfsun[ip_hi][it_hi] - zone_ptr->ref_pt[irp]->dfsun[ip_hi][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsun[ip_hi][it_lo];
		lower = (zone_ptr->ref_pt[irp]->dfsun[ip_lo][it_hi] - zone_ptr->ref_pt[irp]->dfsun[ip_lo][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsun[ip_lo][it_lo];
		sunfac = (upper - lower) * phratio + lower;

		/* Multiply daylight factors by appropriate exterior horizontal illuminance */
		zone_ptr->ref_pt[irp]->daylight = sunfac * hisunf + skyfac * chiskf + zone_ptr->ref_pt[irp]->dfskyo * ohiskf;

		/* Accumulate daylight illuminance totals for later monthly avg calcs */
		zone_ptr->ref_pt[irp]->day_illum[imon][ihr] += zone_ptr->ref_pt[irp]->daylight;
	}

	return(0);
}

/******************************** subroutine calc_sun *******************************/
/* Calculates hourly solar altitude and azimuth. */
/* Also, calculates displacement ratios and boundary indexes for use in daylight */
/* factor interpolation. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine calc_sun *******************************/
int calc_sun(
	double *phsun_ptr,	/* sun position altitude */
	double *thsun_ptr,	/* sun position azimuth */
	double *phratio_ptr,	/* sun position altitude interpolation displacement ratio */
	double *thratio_ptr,	/* sun position azimuth interpolation displacement ratio */
	int *iphs_ptr,		/* sun position altitude interpolation index */
	int *iths_ptr,		/* sun position azimuth interpolation index */
	SUN2_DATA *sun2_ptr,	/* pointer to sun2 data structure */
	double phsmin,		/* minimum sun altitude used in dcof() */
	double phsmax,		/* maximum sun altitude used in dcof() */
	double phsdel,		/* sun altitude increment used in dcof() */
	double thsmin,		/* minimum sun azimuth used in dcof() */
	double thsmax,		/* maximum sun azimuth used in dcof() */
	double thsdel,		/* sun azimuth increment used in dcof() */
	BLDG *bldg_ptr)		/* building data structure pointer */
{
	double phsund, thsund;	/* sun alt and azm (degrees) */
	double phs, ths;			/* sun index vars */

	/* Calc sun alt and azm */
	*phsun_ptr = 1.5708 - acos(sun2_ptr->raycos[2]);
	phsund = *phsun_ptr / DTOR;
	thsund = atan2(sun2_ptr->raycos[1],sun2_ptr->raycos[0]) / DTOR;
	/* Convert thsund to coord sys in which S=0 and E=90 */
	thsund += 90.0 - bldg_ptr->azm / DTOR;
	/* Restrict thsund to -180 to 180 interval */
	if (thsund > -180.0) thsund += 360.;
	if (thsund > 180.0) thsund -= 360.0 * (1.0 + floor(thsund/540.0));
	*thsun_ptr = thsund * DTOR;

	/* Calc alt and azm interpolation indexes and displacement ratios */
	/* Restrict alt and azm to dcof() bounds */
	if (phsund < phsmin) phsund = phsmin;
	if (phsund > phsmax) phsund = phsmax;
	if (thsund < thsmin) thsund = thsmin;
	if (thsund > thsmax) thsund = thsmax;
	/* alt and azm lower interpolation indexes */
	phs = (phsund - phsmin) / phsdel;
	ths = (thsund - thsmin) / thsdel;
	*iphs_ptr = (int)floor(phs);
	*iths_ptr = (int)floor(ths);
	/* alt and azm interpolation displacement ratios */
	*phratio_ptr = phs - (double)(*iphs_ptr);
	*thratio_ptr = ths - (double)(*iths_ptr);

	return(0);
}

/******************************** subroutine dltsys *******************************/
/* Calculates total zonal lighting power reduction factor due to daylighting for */
/* different lighting control systems. */
/* Power reduction factor is the fraction of full power that the lighting system */
/* must be on to provide set point illumination over entire zone. */
/*	1.0 = max lighting input power required */
/*	0.0 = no lighting input power required */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine dltsys *******************************/
int dltsys(
	ZONE *zone_ptr,			/* bldg->zone data structure pointer */
	SUN2_DATA *sun2_ptr,	/* pointer to sun2 data structure */
	ofstream* pofdmpfile)	/* dump file */
{
	int irp, istep;		/* loop indexes */
	double zftot;		/* total zone fraction */
	double fl;			/* temp ref pt fractional light var */
	double fp;			/* temp ref pt fractional power var */
	double step_size;	/* step size for stepped control system */
	double xran;			/* random number generated by ran0() */
	int idum;			/* seed for ran0() */

    // Init return value
    int iReturnVal = 0;

	/* Init power reduction factor and total zone fraction */
	zone_ptr->frac_power = 0.;
	zftot = 0.;

	/* Init random number generator sequence by passing it a negative value for seed */
	idum = -1;
	xran = ran0(&idum);

	/* Calc step size for stepped control system */
	if (zone_ptr->lt_ctrl_steps != 0) step_size = 1.0 / (double)(zone_ptr->lt_ctrl_steps);
	else step_size = 1.;

	/* Loop over reference points */
	for (irp=0; irp<zone_ptr->nrefpts; irp++) {
		/* Output reference point daylight illuminance (lux). */
//		*pofdmpfile << zone_ptr->name << "," << zone_ptr->ref_pt[irp]->name << "," << zone_ptr->ref_pt[irp]->daylight*10.763915 << "\n";
		*pofdmpfile << zone_ptr->ref_pt[irp]->daylight*10.763915 << "\n";

		/* If this reference point does not control a lighting system then skip it */
		if (zone_ptr->ref_pt[irp]->lt_ctrl_type == 0) continue;

		/* If this reference point does not control some fraction of a lighting system then skip it */
		if (zone_ptr->ref_pt[irp]->zone_frac <= 0.0) continue;

		/* If this reference point has a 0 or negative lighting set point then skip it */
		if (zone_ptr->ref_pt[irp]->lt_set_pt <= 0.0) continue;

		/* accumulate total zone fraction */
		zftot += zone_ptr->ref_pt[irp]->zone_frac;

		/* Fractional light output required to meet setpoint */
        if (zone_ptr->ref_pt[irp]->daylight > zone_ptr->ref_pt[irp]->lt_set_pt) {
            fl = 0.;
        }
        else {
            fl = (zone_ptr->ref_pt[irp]->lt_set_pt - zone_ptr->ref_pt[irp]->daylight) / zone_ptr->ref_pt[irp]->lt_set_pt;
        }

		/* Fractional input power required to meet setpoint */

		/* Continuously dimmable system with linear power curve */
		if ((zone_ptr->ref_pt[irp]->lt_ctrl_type == 1) || (zone_ptr->ref_pt[irp]->lt_ctrl_type == 3)) {
			fp = 1.;
            if (fl <= zone_ptr->min_light) {
                fp = zone_ptr->min_power;
                // For lighting control type 3, turn lights completely off if fractional light input power is less than minimum
                if ((zone_ptr->ref_pt[irp]->lt_ctrl_type == 3) && (fl < zone_ptr->min_light)) {
                    fp = 0.0;
                }
            }
			if ((fl > zone_ptr->min_light) && (fl < 1.0)) fp = (fl + (1.0 - fl) * zone_ptr->min_power - zone_ptr->min_light) / (1.0 - zone_ptr->min_light);
		}
		/* Stepped system */
		else if (zone_ptr->ref_pt[irp]->lt_ctrl_type == 2) {
			fp = 0.;
			if (zone_ptr->ref_pt[irp]->daylight < zone_ptr->ref_pt[irp]->lt_set_pt) {
				for (istep=1; istep<=zone_ptr->lt_ctrl_steps; istep++) {
					fp = istep * step_size;
					if (fp >= fl) break;
				}
			}
			if (zone_ptr->ref_pt[irp]->daylight == 0.0) fp = 1.;

			/* Manual operation */
			if (zone_ptr->lt_ctrl_prob < 1.0) {
				/* Occupant sets lights one level too high a fraction of the time */
				/* equal to 1.0 - lt_ctrl_prob. */
				xran = ran0(&idum);
				if (xran >= zone_ptr->lt_ctrl_prob) {
					if (fp < 1.0) fp += step_size;
				}
			}
		}
		/* Unknown system */
		else {
			*pofdmpfile << "WARNING: DElight Unknown light dimming system type specified for reference point " << zone_ptr->ref_pt[irp]->name << "\n";
			*pofdmpfile << "WARNING: Dimming will be ignored at this reference point.\n";
            iReturnVal = -10;
			fp = 1.;
		}

		/* Correct for fraction of hour that sun is down */
		fp = fp * sun2_ptr->fsunup + (1.0 - sun2_ptr->fsunup);

		/* Store this individual ref pt power reduction factor */
		zone_ptr->ref_pt[irp]->frac_power = fp;

		/* Accumulate net lighting power reduction factor for entire zone */
		zone_ptr->frac_power += fp * zone_ptr->ref_pt[irp]->zone_frac;
	}

	/* Correct for fraction of zone (1-zftot) not controlled by the ref pts. */
	/* For this fraction (which is usually zero), the lighting is unaffected */
	/* and the power reduction factor is therefore 1.0. */
	zone_ptr->frac_power += 1.0 - zftot;

	return(iReturnVal);
}
