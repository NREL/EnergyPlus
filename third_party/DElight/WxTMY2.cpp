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
 *
 * Note that the routines in this module are not part of DOE2.
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
#include <vector>
#include <map>
#include <fstream>
#include <cstring>
#include <limits>

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
#include "WxTMY2.h"

/************************ subroutine read_wx_tmy2_hdr ***********************/
/* Reads header lines from raw ASCII TMY2 weather file. */
/* Stores required data in bldg data structure. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/************************ subroutine read_wx_tmy2_hdr ***********************/
int	read_wx_tmy2_hdr(
	BLDG *bldg_ptr,	/* pointer to bldg structure */
	FILE *wxfile)	/* TMY2 weather file pointer */
{
    // Declare temp variables for fscanf
    char wban[7];
    char city[30];
    char state[4];
    char lon[4];	// N = North
    char lat[4];	// W = West
    int lat_deg;
    int lat_min;
    int long_deg;
    int long_min;
	int elevation;
	int timezone;	// TMY2 negative values => behind Universal Time => East of Prime Meridian
						// DOE2 negative values => West of Prime Meridian

    // Read in the header info
    if(fscanf (wxfile, "%s %s %s %d %s %d %d %s %d %d %d",
            wban,
            city,
            state,
            &timezone,
            lat,
            &lat_deg,
            &lat_min,
            lon,
            &long_deg,
            &long_min,
            &elevation
            ) != 11)
    {
      // Not everything was set properly
      return -1;
    }


	/* Convert to DOE2 expected units and store data in bldg data structure */
    // Latitude
	bldg_ptr->lat = (double)lat_deg + ((double)lat_min / 60.0f);
    if (strcmp(lat,"S") == 0)
        bldg_ptr->lat = -(bldg_ptr->lat);

    // Longitude
    bldg_ptr->lon = (double)long_deg + ((double)long_min / 60.0f);
    if (strcmp(lon,"E") == 0) {
      bldg_ptr->lon = -(bldg_ptr->lon);
    }

	// Altitude
	bldg_ptr->alt = (double)elevation / 0.3048;

	// Time zone
	bldg_ptr->timezone = (double)timezone * -1.0;

	return(0);
}

/************************ subroutine read_wx_tmy2_hr ************************/
/* Reads hourly data lines from raw ASCII TMY2 weather file. */
/* Stores required data in sun2 data structure. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/************************ subroutine read_wx_tmy2_hr ************************/
int read_wx_tmy2_hr(
	int imon,	/* current month (begins at 0) */
	int iday,	/* current day (begins at 1) */
	int ihr,	/* current hour (begins at 0) */
	SUN2_DATA *sun2_ptr,	/* pointer to sun2 data structure */
	FILE *wxfile)	/* weather file pointer */
{

    // Weather variables.
    int extra_horiz_rad;
    int extra_direct_norm_rad;
    int global_horiz_rad;	// totl horiz solar rad (Wh/m2)
    int direct_norm_rad;	// direct normal solar rad (Wh/m2)
    int diffuse_horiz_rad;
    int global_horiz_illum;
    int direct_norm_illum;
    int diffuse_horiz_illum;
    int zenith_illum;
    int total_sky_cover;	// hourly cloud amount (in tenths)
    int opaque_sky_cover;
    int dry_bulb_temp;
    int dew_pt_temp;		// hourly dewpoint temperature (tenths of degreeC)
    int rel_humidity;
    int atm_pressure;
    int wind_dir;
    int wind_speed;
    int visibility;
    long ceiling_ht;

    // present weather condition
    int observed;
    int thunder;
    int rain;
    int rain_squall;
    int snow;
    int snow_squall;
    int sleet;
    int fog;
    int smoke;
    int ice_pellets;


    int precipitation;
    int aerosol_opt_depth;
    int snow_depth;
    int days_since_snow;

    // Dummy variables to hold the src flag for each data item.  We don't use
    // this information.
    char ghr, dnr, dhr, ghi, dni, dhi, zi, tsc, osc, dbt, dpt, rh, ap;
    char ws, wd, vis, ch, pre, aod, sd, dss;

    // Dummy variables to hold the uncertainty flag for each data item.
    // We don't used this information either.
    int ughr, udnr, udhr, ughi, udni, udhi, uzi, utsc, uosc, udbt, udpt, urh, uap;
    int uws, uwd, uvis, uch, upre, uaod, usd, udss;

    // Variables to hold the year, month, day, hour for each data item.
    int yr;		// not used
	int month;	/* input data line month (begins at 1) */
	int day;	/* input data line day (begins at 1) */
	int hour;	/* input data line hour (begins at 1) */

	/* read wx hourly data line until matching month/day/hour are found */
    do
    {
        int nset = fscanf ( wxfile, "%2d%2d%2d%2d%4d%4d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%2d%1s%1d%2d%1s%1d%4d%1s%1d%4d%1s%1d%3d%1s%1d%4d%1s%1d%3d%1s%1d%3d%1s%1d%4d%1s%1d%5ld%1s%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%3d%1s%1d%3d%1s%1d%3d%1s%1d%2d%1s%1d",
                 &yr,
                 &month,
                 &day,
                 &hour,

                 &extra_horiz_rad,
                 &extra_direct_norm_rad,

                 &global_horiz_rad,	// totl horiz solar rad (Wh/m2)
                 &ghr,
                 &ughr,

                 &direct_norm_rad,	// direct normal solar rad (Wh/m2)
                 &dnr,
                 &udnr,

                 &diffuse_horiz_rad,
                 &dhr,
                 &udhr,

                 &global_horiz_illum,
                 &ghi,
                 &ughi,

                 &direct_norm_illum,
                 &dni,
                 &udni,

                 &diffuse_horiz_illum,
                 &dhi,
                 &udhi,

                 &zenith_illum,
                 &zi,
                 &uzi,

                 &total_sky_cover,	// hourly cloud amount (in tenths)
                 &tsc,
                 &utsc,

                 &opaque_sky_cover,
                 &osc,
                 &uosc,

                 &dry_bulb_temp,
                 &dbt,
                 &udbt,

                 &dew_pt_temp,		// hourly dewpoint temperature (tenths of degreeC)
                 &dpt,
                 &udpt,

                 &rel_humidity,
                 &rh,
                 &urh,

                 &atm_pressure,
                 &ap,
                 &uap,

                 &wind_dir,
                 &wd,
                 &uwd,

                 &wind_speed,
                 &ws,
                 &uws,

                 &visibility,
                 &vis,
                 &uvis,

                 &ceiling_ht,
                 &ch,
                 &uch,

                 // present weather condition
                 &observed,
                 &thunder,
                 &rain,
                 &rain_squall,
                 &snow,
                 &snow_squall,
                 &sleet,
                 &fog,
                 &smoke,
                 &ice_pellets,


                 &precipitation,
                 &pre,
                 &upre,

                 &aerosol_opt_depth,
                 &aod,
                 &uaod,

                 &snow_depth,
                 &sd,
                 &usd,

                 &days_since_snow,
                 &dss,
                 &udss

               );//end of fscanf function
        if (nset != 80) {return -1;}
    }
    while(!((month == (imon+1)) && (day == iday) && (hour == (ihr+1))));

	// Store input values in sun2 data structure.
	// Convert TMY2 solar irradiance from W/m2 to Btu/ft2-h units expected by daylighting calcs
	// using 0.3170 multiplier from DOE2 TMY2 wx processor.  Note that TMY2 docs state that sol
	// rad is stored in Wh/m2, but I assume that since the value is always for one hour this is
	// equivalent to W/m2. Also note that DOE2 wx processor rounds these converted values using
	// IROUND, for which I will use ceil() assuming sol rad is always positive (rounds up).
	// Convert Dewpoint temperature to degreeF.
	sun2_ptr->solrad = ceil(global_horiz_rad * 0.3170);
	sun2_ptr->dirsol = ceil(direct_norm_rad * 0.3170);
	sun2_ptr->cldamt = total_sky_cover;
	sun2_ptr->dewpt = (dew_pt_temp / 10.0) * 1.8 + 32.0;

	return(0);
}
