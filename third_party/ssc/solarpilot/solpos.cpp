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

/*============================================================================
*    Contains:
*        S_solpos     (computes solar position and intensity
*                      from time and place)
*
*            INPUTS:     (via posdata struct) year, daynum, hour,
*                        minute, second, latitude, longitude, timezone,
*                        intervl
*            OPTIONAL:   (via posdata struct) month, day, press, temp, tilt,
*                        aspect, function
*            OUTPUTS:    EVERY variable in the struct posdata
*                            (defined in solpos.h)
*
*                       NOTE: Certain conditions exist during which some of
*                       the output variables are undefined or cannot be
*                       calculated.  In these cases, the variables are
*                       returned with flag values indicating such.  In other
*                       cases, the variables may return a realistic, though
*                       invalid, value. These variables and the flag values
*                       or invalid conditions are listed below:
*
*                       amass     -1.0 at zenetr angles greater than 93.0
*                                 degrees
*                       ampress   -1.0 at zenetr angles greater than 93.0
*                                 degrees
*                       azim      invalid at zenetr angle 0.0 or latitude
*                                 +/-90.0 or at night
*                       elevetr   limited to -9 degrees at night
*                       etr       0.0 at night
*                       etrn      0.0 at night
*                       etrtilt   0.0 when cosinc is less than 0
*                       prime     invalid at zenetr angles greater than 93.0
*                                 degrees
*                       sretr     +/- 2999.0 during periods of 24 hour sunup or
*                                 sundown
*                       ssetr     +/- 2999.0 during periods of 24 hour sunup or
*                                 sundown
*                       ssha      invalid at the North and South Poles
*                       unprime   invalid at zenetr angles greater than 93.0
*                                 degrees
*                       zenetr    limited to 99.0 degrees at night
*
*        S_init       (optional initialization for all input parameters in
*                      the posdata struct)
*           INPUTS:     struct posdata*
*           OUTPUTS:    struct posdata*
*
*                     (Note: initializes the required S_solpos INPUTS above
*                      to out-of-bounds conditions, forcing the user to
*                      supply the parameters; initializes the OPTIONAL
*                      S_solpos inputs above to nominal values.)
*
*       S_decode      (optional utility for decoding the S_solpos return code)
*           INPUTS:     long integer S_solpos return value, struct posdata*
*           OUTPUTS:    text to stderr
*
*    Usage:
*         In calling program, just after other 'includes', insert:
*
*              #include "solpos00.h"
*
*         Function calls:
*              S_init(struct posdata*)  [optional]
*              .
*              .
*              [set time and location parameters before S_solpos call]
*              .
*              .
*              int retval = S_solpos(struct posdata*)
*              S_decode(int retval, struct posdata*) [optional]
*                  (Note: you should always look at the S_solpos return
*                   value, which contains error codes. S_decode is one option
*                   for examining these codes.  It can also serve as a
*                   template for building your own application-specific
*                   decoder.)
*
*    Martin Rymes
*    National Renewable Energy Laboratory
*    25 March 1998
*
*    27 April 1999 REVISION:  Corrected leap year in S_date.
*    13 January 2000 REVISION:  SMW converted to structure posdata parameter
*                               and subdivided into functions.
*    01 February 2001 REVISION: SMW corrected ecobli calculation 
*                               (changed sign). Error is small (max 0.015 deg
*                               in calculation of declination angle)
*----------------------------------------------------------------------------*/
#include <math.h>
#include <string.h>
#include <stdio.h>
#include "solpos00.h"

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
* Structures defined for this module
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
struct trigdata /* used to pass calculated values locally */
{
    double cd;       /* cosine of the declination */
    double ch;       /* cosine of the hour angle */
    double cl;       /* cosine of the latitude */
    double sd;       /* sine of the declination */
    double sl;       /* sine of the latitude */
};


/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
* Temporary global variables used only in this file:
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
  static int  month_days[2][13] = { { 0,   0,  31,  59,  90, 120, 151,
                                       181, 212, 243, 273, 304, 334 },
                                    { 0,   0,  31,  60,  91, 121, 152,
                                       182, 213, 244, 274, 305, 335 } };
                   /* cumulative number of days prior to beginning of month */

  static double degrad = 57.295779513; /* converts from radians to degrees */
  static double raddeg = 0.0174532925; /* converts from degrees to radians */

/*============================================================================
*    Local function prototypes
============================================================================*/
static long int validate ( struct posdata *pdat);
static void dom2doy( struct posdata *pdat );
static void doy2dom( struct posdata *pdat );
static void geometry ( struct posdata *pdat );
static void zen_no_ref ( struct posdata *pdat, struct trigdata *tdat );
static void ssha( struct posdata *pdat, struct trigdata *tdat );
static void sbcf( struct posdata *pdat, struct trigdata *tdat );
static void tst( struct posdata *pdat );
static void srss( struct posdata *pdat );
static void sazm( struct posdata *pdat, struct trigdata *tdat );
static void refrac( struct posdata *pdat );
static void amass( struct posdata *pdat );
static void prime( struct posdata *pdat );
static void etr( struct posdata *pdat );
static void tilt( struct posdata *pdat );
static void localtrig( struct posdata *pdat, struct trigdata *tdat );

/*============================================================================
*    Long integer function S_solpos, adapted from the VAX solar libraries
*
*    This function calculates the apparent solar position and the
*    intensity of the sun (theoretical maximum solar energy) from
*    time and place on Earth.
*
*    Requires (from the struct posdata parameter):
*        Date and time:
*            year
*            daynum   (requirement depends on the S_DOY switch)
*            month    (requirement depends on the S_DOY switch)
*            day      (requirement depends on the S_DOY switch)
*            hour
*            minute
*            second
*            interval  DEFAULT 0
*        Location:
*            latitude
*            longitude
*        Location/time adjuster:
*            timezone
*        Atmospheric pressure and temperature:
*            press     DEFAULT 1013.0 mb
*            temp      DEFAULT 10.0 degrees C
*        Tilt of flat surface that receives solar energy:
*            aspect    DEFAULT 180 (South)
*            tilt      DEFAULT 0 (Horizontal)
*        Function Switch (codes defined in solpos.h)
*            function  DEFAULT S_ALL
*
*    Returns (via the struct posdata parameter):
*        everything defined in the struct posdata in solpos.h.
*----------------------------------------------------------------------------*/
long S_solpos (struct posdata *pdat)
{
  long int retval;

  struct trigdata trigdat, *tdat;

  tdat = &trigdat;   /* point to the structure */

  /* initialize the trig structure */
  tdat->sd = -999.0; /* flag to force calculation of trig data */
  tdat->cd =    1.0;
  tdat->ch =    1.0; /* set the rest of these to something safe */
  tdat->cl =    1.0;
  tdat->sl =    1.0;

  if ((retval = validate ( pdat )) != 0) /* validate the inputs */
    return retval;


  if ( pdat->function & L_DOY )
    doy2dom( pdat );                /* convert input doy to month-day */
  else
    dom2doy( pdat );                /* convert input month-day to doy */

  if ( pdat->function & L_GEOM )
    geometry( pdat );               /* do basic geometry calculations */

  if ( pdat->function & L_ZENETR )  /* etr at non-refracted zenith angle */
    zen_no_ref( pdat, tdat );

  if ( pdat->function & L_SSHA )    /* Sunset hour calculation */
    ssha( pdat, tdat );

  if ( pdat->function & L_SBCF )    /* Shadowband correction factor */
    sbcf( pdat, tdat );

  if ( pdat->function & L_TST )     /* true solar time */
    tst( pdat );

  if ( pdat->function & L_SRSS )    /* sunrise/sunset calculations */
    srss( pdat );

  if ( pdat->function & L_SOLAZM )  /* solar azimuth calculations */
    sazm( pdat, tdat );

  if ( pdat->function & L_REFRAC )  /* atmospheric refraction calculations */
    refrac( pdat );

  if ( pdat->function & L_AMASS )   /* airmass calculations */
    amass( pdat );

  if ( pdat->function & L_PRIME )   /* kt-prime/unprime calculations */
    prime( pdat );

  if ( pdat->function & L_ETR )     /* ETR and ETRN (refracted) */
    etr( pdat );

  if ( pdat->function & L_TILT )    /* tilt calculations */
    tilt( pdat );

    return 0;
}


/*============================================================================
*    Void function S_init
*
*    This function initiates all of the input parameters in the struct
*    posdata passed to S_solpos().  Initialization is either to nominal
*    values or to out of range values, which forces the calling program to
*    specify parameters.
*
*    NOTE: This function is optional if you initialize ALL input parameters
*          in your calling code.  Note that the required parameters of date
*          and location are deliberately initialized out of bounds to force
*          the user to enter real-world values.
*
*    Requires: Pointer to a posdata structure, members of which are
*           initialized.
*
*    Returns: Void
*----------------------------------------------------------------------------*/
void S_init(struct posdata *pdat)
{
  pdat->day       =    -99;   /* Day of month (May 27 = 27, etc.) */
  pdat->daynum    =   -999;   /* Day number (day of year; Feb 1 = 32 ) */
  pdat->hour      =    -99;   /* Hour of day, 0 - 23 */
  pdat->minute    =    -99;   /* Minute of hour, 0 - 59 */
  pdat->month     =    -99;   /* Month number (Jan = 1, Feb = 2, etc.) */
  pdat->second    =    -99;   /* Second of minute, 0 - 59 */
  pdat->year      =    -99;   /* 4-digit year */
  pdat->interval  =      0;   /* instantaneous measurement interval */
  pdat->aspect    =  180.0;   /* Azimuth of panel surface (direction it
                                    faces) N=0, E=90, S=180, W=270 */
  pdat->latitude  =  -99.0;   /* Latitude, degrees north (south negative) */
  pdat->longitude = -999.0;   /* Longitude, degrees east (west negative) */
  pdat->press     = 1013.0;   /* Surface pressure, millibars */
  pdat->solcon    = 1367.0;   /* Solar constant, 1367 W/sq m */
  pdat->temp      =   15.0;   /* Ambient dry-bulb temperature, degrees C */
  pdat->tilt      =    0.0;   /* Degrees tilt from horizontal of panel */
  pdat->timezone  =  -99.0;   /* Time zone, east (west negative). */
  pdat->sbwid     =    7.6;   /* Eppley shadow band width */
  pdat->sbrad     =   31.7;   /* Eppley shadow band radius */
  pdat->sbsky     =   0.04;   /* Drummond factor for partly cloudy skies */
  pdat->function  =  S_ALL;   /* compute all parameters */
}


/*============================================================================
*    Local long int function validate
*
*    Validates the input parameters
*----------------------------------------------------------------------------*/
static long int validate ( struct posdata *pdat)
{

  long int retval = 0;  /* start with no errors */

  /* No absurd dates, please. */
  if ( pdat->function & L_GEOM )
  {
    if ( (pdat->year < 1950) || (pdat->year > 2050) ) /* limits of algoritm */
      retval |= (1L << S_YEAR_ERROR);
    if ( !(pdat->function & S_DOY) && ((pdat->month < 1) || (pdat->month > 12)))
      retval |= (1L << S_MONTH_ERROR);
    if ( !(pdat->function & S_DOY) && ((pdat->day < 1) || (pdat->day > 31)) )
      retval |= (1L << S_DAY_ERROR);
    if ( (pdat->function & S_DOY) && ((pdat->daynum < 1) || (pdat->daynum > 366)) )
      retval |= (1L << S_DOY_ERROR);

    /* No absurd times, please. */
    if ( (pdat->hour < 0) || (pdat->hour > 24) )
      retval |= (1L << S_HOUR_ERROR);
    if ( (pdat->minute < 0) || (pdat->minute > 59) )
      retval |= (1L << S_MINUTE_ERROR);
    if ( (pdat->second < 0) || (pdat->second > 59) )
      retval |= (1L << S_SECOND_ERROR);
    if ( (pdat->hour == 24) && (pdat->minute > 0) ) /* no more than 24 hrs */
      retval |= ( (1L << S_HOUR_ERROR) | (1L << S_MINUTE_ERROR) );
    if ( (pdat->hour == 24) && (pdat->second > 0) ) /* no more than 24 hrs */
      retval |= ( (1L << S_HOUR_ERROR) | (1L << S_SECOND_ERROR) );
    if ( fabs (pdat->timezone) > 12.0 )
      retval |= (1L << S_TZONE_ERROR);
    if ( (pdat->interval < 0) || (pdat->interval > 28800) )
      retval |= (1L << S_INTRVL_ERROR);

    /* No absurd locations, please. */
    if ( fabs (pdat->longitude) > 180.0 )
      retval |= (1L << S_LON_ERROR);
    if ( fabs (pdat->latitude) > 90.0 )
      retval |= (1L << S_LAT_ERROR);
  }

  /* No silly temperatures or pressures, please. */
  if ( (pdat->function & L_REFRAC) && (fabs (pdat->temp) > 100.0) )
    retval |= (1L << S_TEMP_ERROR);
  if ( (pdat->function & L_REFRAC) &&
    (pdat->press < 0.0 || pdat->press > 2000.0) ){
    retval |= (1L << S_PRESS_ERROR);
  }

  /* No out of bounds tilts, please */
  if ( (pdat->function & L_TILT) && (fabs (pdat->tilt) > 180.0) )
    retval |= (1L << S_TILT_ERROR);
  if ( (pdat->function & L_TILT) && (fabs (pdat->aspect) > 360.0) )
    retval |= (1L << S_ASPECT_ERROR);

  /* No oddball shadowbands, please */
  if ( (pdat->function & L_SBCF) &&
       (pdat->sbwid < 1.0 || pdat->sbwid > 100.0) ){
    retval |= (1L << S_SBWID_ERROR);
  }
  if ( (pdat->function & L_SBCF) &&
       (pdat->sbrad < 1.0 || pdat->sbrad > 100.0) ){
    retval |= (1L << S_SBRAD_ERROR);
  }
  if ( (pdat->function & L_SBCF) && ( fabs (pdat->sbsky) > 1.0) )
    retval |= (1L << S_SBSKY_ERROR);

  return retval;
}


/*============================================================================
*    Local Void function dom2doy
*
*    Converts day-of-month to day-of-year
*
*    Requires (from struct posdata parameter):
*            year
*            month
*            day
*
*    Returns (via the struct posdata parameter):
*            year
*            daynum
*----------------------------------------------------------------------------*/
static void dom2doy( struct posdata *pdat )
{
  pdat->daynum = pdat->day + month_days[0][pdat->month];

  /* (adjust for leap year) */
  if ( ((pdat->year % 4) == 0) &&
         ( ((pdat->year % 100) != 0) || ((pdat->year % 400) == 0) ) &&
         (pdat->month > 2) )
      pdat->daynum += 1;
}


/*============================================================================
*    Local void function doy2dom
*
*    This function computes the month/day from the day number.
*
*    Requires (from struct posdata parameter):
*        Year and day number:
*            year
*            daynum
*
*    Returns (via the struct posdata parameter):
*            year
*            month
*            day
*----------------------------------------------------------------------------*/
static void doy2dom(struct posdata *pdat)
{
  int  imon;  /* Month (month_days) array counter */
  int  leap;  /* leap year switch */

    /* Set the leap year switch */
    if ( ((pdat->year % 4) == 0) &&
         ( ((pdat->year % 100) != 0) || ((pdat->year % 400) == 0) ) )
        leap = 1;
    else
        leap = 0;

    /* Find the month */
    imon = 12;
    while ( pdat->daynum <= month_days [leap][imon] )
        --imon;

    /* Set the month and day of month */
    pdat->month = imon;
    pdat->day   = pdat->daynum - month_days[leap][imon];
}


/*============================================================================
*    Local Void function geometry
*
*    Does the underlying geometry for a given time and location
*----------------------------------------------------------------------------*/
static void geometry ( struct posdata *pdat )
{
  double bottom;      /* denominator (bottom) of the fraction */
  double c2;          /* cosine of d2 */
  double cd;          /* cosine of the day angle or delination */
  double d2;          /* pdat->dayang times two */
  double delta;       /* difference between current year and 1949 */
  double s2;          /* sine of d2 */
  double sd;          /* sine of the day angle */
  double top;         /* numerator (top) of the fraction */
  int   leap;        /* leap year counter */

  /* Day angle */
      /*  Iqbal, M.  1983.  An Introduction to Solar Radiation.
            Academic Press, NY., page 3 */
     pdat->dayang = 360.0 * ( pdat->daynum - 1 ) / 365.0;

    /* Earth radius vector * solar constant = solar energy */
        /*  Spencer, J. W.  1971.  Fourier series representation of the
            position of the sun.  Search 2 (5), page 172 */
    sd     = sin (raddeg * pdat->dayang);
    cd     = cos (raddeg * pdat->dayang);
    d2     = 2.0 * pdat->dayang;
    c2     = cos (raddeg * d2);
    s2     = sin (raddeg * d2);

    pdat->erv  = 1.000110 + 0.034221 * cd + 0.001280 * sd;
    pdat->erv  += 0.000719 * c2 + 0.000077 * s2;

    /* Universal Coordinated (Greenwich standard) time */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->utime =
        pdat->hour * 3600.0 +
        pdat->minute * 60.0 +
        pdat->second -
        (double)pdat->interval / 2.0;
    pdat->utime = pdat->utime / 3600.0 - pdat->timezone;

    /* Julian Day minus 2,400,000 days (to eliminate roundoff errors) */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */

    /* No adjustment for century non-leap years since this function is
       bounded by 1950 - 2050 */
    delta    = pdat->year - 1949;
    leap     = (int) ( delta / 4.0 );
    pdat->julday =
        32916.5 + delta * 365.0 + leap + pdat->daynum + pdat->utime / 24.0;

    /* Time used in the calculation of ecliptic coordinates */
    /* Noon 1 JAN 2000 = 2,400,000 + 51,545 days Julian Date */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->ectime = pdat->julday - 51545.0;

    /* Mean longitude */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->mnlong  = 280.460 + 0.9856474 * pdat->ectime;

    /* (dump the multiples of 360, so the answer is between 0 and 360) */
    pdat->mnlong -= 360.0 * (int) ( pdat->mnlong / 360.0 );
    if ( pdat->mnlong < 0.0 )
        pdat->mnlong += 360.0;

    /* Mean anomaly */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->mnanom  = 357.528 + 0.9856003 * pdat->ectime;

    /* (dump the multiples of 360, so the answer is between 0 and 360) */
    pdat->mnanom -= 360.0 * (int) ( pdat->mnanom / 360.0 );
    if ( pdat->mnanom < 0.0 )
        pdat->mnanom += 360.0;

    /* Ecliptic longitude */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->eclong  = pdat->mnlong + 1.915 * sin ( pdat->mnanom * raddeg ) +
                    0.020 * sin ( 2.0 * pdat->mnanom * raddeg );

    /* (dump the multiples of 360, so the answer is between 0 and 360) */
    pdat->eclong -= 360.0 * (int) ( pdat->eclong / 360.0 );
    if ( pdat->eclong < 0.0 )
        pdat->eclong += 360.0;

    /* Obliquity of the ecliptic */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */

    /* 02 Feb 2001 SMW corrected sign in the following line */
/*  pdat->ecobli = 23.439 + 4.0e-07 * pdat->ectime;     */
    pdat->ecobli = 23.439 - 4.0e-07 * pdat->ectime;

    /* Declination */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->declin = degrad * asin ( sin (pdat->ecobli * raddeg) *
                               sin (pdat->eclong * raddeg) );

    /* Right ascension */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    top      =  cos ( raddeg * pdat->ecobli ) * sin ( raddeg * pdat->eclong );
    bottom   =  cos ( raddeg * pdat->eclong );

    pdat->rascen =  degrad * atan2 ( top, bottom );

    /* (make it a positive angle) */
    if ( pdat->rascen < 0.0 )
        pdat->rascen += 360.0;

    /* Greenwich mean sidereal time */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->gmst  = 6.697375 + 0.0657098242 * pdat->ectime + pdat->utime;

    /* (dump the multiples of 24, so the answer is between 0 and 24) */
    pdat->gmst -= 24.0 * (int) ( pdat->gmst / 24.0 );
    if ( pdat->gmst < 0.0 )
        pdat->gmst += 24.0;

    /* Local mean sidereal time */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->lmst  = pdat->gmst * 15.0 + pdat->longitude;

    /* (dump the multiples of 360, so the answer is between 0 and 360) */
    pdat->lmst -= 360.0 * (int) ( pdat->lmst / 360.0 );
    if ( pdat->lmst < 0.)
        pdat->lmst += 360.0;

    /* Hour angle */
        /*  Michalsky, J.  1988.  The Astronomical Almanac's algorithm for
            approximate solar position (1950-2050).  Solar Energy 40 (3),
            pp. 227-235. */
    pdat->hrang = pdat->lmst - pdat->rascen;

    /* (force it between -180 and 180 degrees) */
    if ( pdat->hrang < -180.0 )
        pdat->hrang += 360.0;
    else if ( pdat->hrang > 180.0 )
        pdat->hrang -= 360.0;
}


/*============================================================================
*    Local Void function zen_no_ref
*
*    ETR solar zenith angle
*       Iqbal, M.  1983.  An Introduction to Solar Radiation.
*            Academic Press, NY., page 15
*----------------------------------------------------------------------------*/
static void zen_no_ref ( struct posdata *pdat, struct trigdata *tdat )
{
  double cz;          /* cosine of the solar zenith angle */

    localtrig( pdat, tdat );
    cz = tdat->sd * tdat->sl + tdat->cd * tdat->cl * tdat->ch;

    /* (watch out for the roundoff errors) */
    if ( fabs (cz) > 1.0 ) {
        if ( cz >= 0.0 )
            cz =  1.0;
        else
            cz = -1.0;
    }

    pdat->zenetr   = acos ( cz ) * degrad;

    /* (limit the degrees below the horizon to 9 [+90 -> 99]) */
    if ( pdat->zenetr > 99.0 )
        pdat->zenetr = 99.0;

    pdat->elevetr = 90.0 - pdat->zenetr;
}


/*============================================================================
*    Local Void function ssha
*
*    Sunset hour angle, degrees
*       Iqbal, M.  1983.  An Introduction to Solar Radiation.
*            Academic Press, NY., page 16
*----------------------------------------------------------------------------*/
static void ssha( struct posdata *pdat, struct trigdata *tdat )
{
  double cssha;       /* cosine of the sunset hour angle */
  double cdcl;        /* ( cd * cl ) */

    localtrig( pdat, tdat );
    cdcl    = tdat->cd * tdat->cl;

    if ( fabs ( cdcl ) >= 0.001 ) {
        cssha = -tdat->sl * tdat->sd / cdcl;

        /* This keeps the cosine from blowing on roundoff */
        if ( cssha < -1.0  )
            pdat->ssha = 180.0;
        else if ( cssha > 1.0 )
            pdat->ssha = 0.0;
        else
            pdat->ssha = degrad * acos ( cssha );
    }
    else if ( ((pdat->declin >= 0.0) && (pdat->latitude > 0.0 )) ||
              ((pdat->declin <  0.0) && (pdat->latitude < 0.0 )) )
        pdat->ssha = 180.0;
    else
        pdat->ssha = 0.0;
}


/*============================================================================
*    Local Void function sbcf
*
*    Shadowband correction factor
*       Drummond, A. J.  1956.  A contribution to absolute pyrheliometry.
*            Q. J. R. Meteorol. Soc. 82, pp. 481-493
*----------------------------------------------------------------------------*/
static void sbcf( struct posdata *pdat, struct trigdata *tdat )
{
  double p, t1, t2;   /* used to compute sbcf */

    localtrig( pdat, tdat );
    p       = 0.6366198 * pdat->sbwid / pdat->sbrad * pow (tdat->cd,3);
    t1      = tdat->sl * tdat->sd * pdat->ssha * raddeg;
    t2      = tdat->cl * tdat->cd * sin ( pdat->ssha * raddeg );
    pdat->sbcf = pdat->sbsky + 1.0 / ( 1.0 - p * ( t1 + t2 ) );

}


/*============================================================================
*    Local Void function tst
*
*    TST -> True Solar Time = local standard time + TSTfix, time
*      in minutes from midnight.
*        Iqbal, M.  1983.  An Introduction to Solar Radiation.
*            Academic Press, NY., page 13
*----------------------------------------------------------------------------*/
static void tst( struct posdata *pdat )
{
    pdat->tst    = ( 180.0 + pdat->hrang ) * 4.0;
    pdat->tstfix =
        pdat->tst -
        (double)pdat->hour * 60.0 -
        pdat->minute -
        (double)pdat->second / 60.0 +
        (double)pdat->interval / 120.0; /* add back half of the interval */

    /* bound tstfix to this day */
    while ( pdat->tstfix >  720.0 )
        pdat->tstfix -= 1440.0;
    while ( pdat->tstfix < -720.0 )
        pdat->tstfix += 1440.0;

    pdat->eqntim =
        pdat->tstfix + 60.0 * pdat->timezone - 4.0 * pdat->longitude;

}


/*============================================================================
*    Local Void function srss
*
*    Sunrise and sunset times (minutes from midnight)
*----------------------------------------------------------------------------*/
static void srss( struct posdata *pdat )
{
    if ( pdat->ssha <= 1.0 ) {
        pdat->sretr   =  2999.0;
        pdat->ssetr   = -2999.0;
    }
    else if ( pdat->ssha >= 179.0 ) {
        pdat->sretr   = -2999.0;
        pdat->ssetr   =  2999.0;
    }
    else {
        pdat->sretr   = 720.0 - 4.0 * pdat->ssha - pdat->tstfix;
        pdat->ssetr   = 720.0 + 4.0 * pdat->ssha - pdat->tstfix;
    }
}


/*============================================================================
*    Local Void function sazm
*
*    Solar azimuth angle
*       Iqbal, M.  1983.  An Introduction to Solar Radiation.
*            Academic Press, NY., page 15
*----------------------------------------------------------------------------*/
static void sazm( struct posdata *pdat, struct trigdata *tdat )
{
  double ca;          /* cosine of the solar azimuth angle */
  double ce;          /* cosine of the solar elevation */
  double cecl;        /* ( ce * cl ) */
  double se;          /* sine of the solar elevation */

    localtrig( pdat, tdat );
    ce         = cos ( raddeg * pdat->elevetr );
    se         = sin ( raddeg * pdat->elevetr );

    pdat->azim     = 180.0;
    cecl       = ce * tdat->cl;
    if ( fabs ( cecl ) >= 0.001 ) {
        ca     = ( se * tdat->sl - tdat->sd ) / cecl;
        if ( ca > 1.0 )
            ca = 1.0;
        else if ( ca < -1.0 )
            ca = -1.0;

        pdat->azim = 180.0 - acos ( ca ) * degrad;
        if ( pdat->hrang > 0 )
            pdat->azim  = 360.0 - pdat->azim;
    }
}


/*============================================================================
*    Local Int function refrac
*
*    Refraction correction, degrees
*        Zimmerman, John C.  1981.  Sun-pointing programs and their
*            accuracy.
*            SAND81-0761, Experimental Systems Operation Division 4721,
*            Sandia National Laboratories, Albuquerque, NM.
*----------------------------------------------------------------------------*/
static void refrac( struct posdata *pdat )
{
  double prestemp;    /* temporary pressure/temperature correction */
  double refcor;      /* temporary refraction correction */
  double tanelev;     /* tangent of the solar elevation angle */

    /* If the sun is near zenith, the algorithm bombs; refraction near 0 */
    if ( pdat->elevetr > 85.0 )
        refcor = 0.0;

    /* Otherwise, we have refraction */
    else {
        tanelev = tan ( raddeg * pdat->elevetr );
        if ( pdat->elevetr >= 5.0 )
            refcor  = 58.1 / tanelev -
                      0.07 / ( pow (tanelev,3) ) +
                      0.000086 / ( pow (tanelev,5) );
        else if ( pdat->elevetr >= -0.575 )
            refcor  = 1735.0 +
                      pdat->elevetr * ( -518.2 + pdat->elevetr * ( 103.4 +
                      pdat->elevetr * ( -12.79 + pdat->elevetr * 0.711 ) ) );
        else
            refcor  = -20.774 / tanelev;

        prestemp    =
            ( pdat->press * 283.0 ) / ( 1013.0 * ( 273.0 + pdat->temp ) );
        refcor     *= prestemp / 3600.0;
    }

    /* Refracted solar elevation angle */
    pdat->elevref = pdat->elevetr + refcor;

    /* (limit the degrees below the horizon to 9) */
    if ( pdat->elevref < -9.0 )
        pdat->elevref = -9.0;

    /* Refracted solar zenith angle */
    pdat->zenref  = 90.0 - pdat->elevref;
    pdat->coszen  = cos( raddeg * pdat->zenref );
}


/*============================================================================
*    Local Void function  amass
*
*    Airmass
*       Kasten, F. and Young, A.  1989.  Revised optical air mass
*            tables and approximation formula.  Applied Optics 28 (22),
*            pp. 4735-4738
*----------------------------------------------------------------------------*/
static void amass( struct posdata *pdat )
{
    if ( pdat->zenref > 93.0 )
    {
        pdat->amass   = -1.0;
        pdat->ampress = -1.0;
    }
    else
    {
        pdat->amass =
            1.0 / ( cos (raddeg * pdat->zenref) + 0.50572 *
            pow ((96.07995 - pdat->zenref),-1.6364) );

        pdat->ampress   = pdat->amass * pdat->press / 1013.0;
    }
}


/*============================================================================
*    Local Void function prime
*
*    Prime and Unprime
*    Prime  converts Kt to normalized Kt', etc.
*       Unprime deconverts Kt' to Kt, etc.
*            Perez, R., P. Ineichen, Seals, R., & Zelenka, A.  1990.  Making
*            full use of the clearness index for parameterizing hourly
*            insolation conditions. Solar Energy 45 (2), pp. 111-114
*----------------------------------------------------------------------------*/
static void prime( struct posdata *pdat )
{
    pdat->unprime = 1.031 * exp ( -1.4 / ( 0.9 + 9.4 / pdat->amass ) ) + 0.1;
    pdat->prime   = 1.0 / pdat->unprime;
}


/*============================================================================
*    Local Void function etr
*
*    Extraterrestrial (top-of-atmosphere) solar irradiance
*----------------------------------------------------------------------------*/
static void etr( struct posdata *pdat )
{
    if ( pdat->coszen > 0.0 ) {
        pdat->etrn = pdat->solcon * pdat->erv;
        pdat->etr  = pdat->etrn * pdat->coszen;
    }
    else {
        pdat->etrn = 0.0;
        pdat->etr  = 0.0;
    }
}


/*============================================================================
*    Local Void function localtrig
*
*    Does trig on internal variable used by several functions
*----------------------------------------------------------------------------*/
static void localtrig( struct posdata *pdat, struct trigdata *tdat )
{
/* define masks to prevent calculation of uninitialized variables */
#define SD_MASK ( L_ZENETR | L_SSHA | S_SBCF | S_SOLAZM )
#define SL_MASK ( L_ZENETR | L_SSHA | S_SBCF | S_SOLAZM )
#define CL_MASK ( L_ZENETR | L_SSHA | S_SBCF | S_SOLAZM )
#define CD_MASK ( L_ZENETR | L_SSHA | S_SBCF )
#define CH_MASK ( L_ZENETR )

    if ( tdat->sd < -900.0 )  /* sd was initialized -999 as flag */
    {
      tdat->sd = 1.0;  /* reflag as having completed calculations */
      if ( pdat->function | CD_MASK )
        tdat->cd = cos ( raddeg * pdat->declin );
      if ( pdat->function | CH_MASK )
        tdat->ch = cos ( raddeg * pdat->hrang );
      if ( pdat->function | CL_MASK )
        tdat->cl = cos ( raddeg * pdat->latitude );
      if ( pdat->function | SD_MASK )
        tdat->sd = sin ( raddeg * pdat->declin );
      if ( pdat->function | SL_MASK )
        tdat->sl = sin ( raddeg * pdat->latitude );
    }
}


/*============================================================================
*    Local Void function tilt
*
*    ETR on a tilted surface
*----------------------------------------------------------------------------*/
static void tilt( struct posdata *pdat )
{
  double ca;          /* cosine of the solar azimuth angle */
  double cp;          /* cosine of the panel aspect */
  double ct;          /* cosine of the panel tilt */
  double sa;          /* sine of the solar azimuth angle */
  double sp;          /* sine of the panel aspect */
  double st;          /* sine of the panel tilt */
  double sz;          /* sine of the refraction corrected solar zenith angle */


    /* Cosine of the angle between the sun and a tipped flat surface,
       useful for calculating solar energy on tilted surfaces */
    ca      = cos ( raddeg * pdat->azim );
    cp      = cos ( raddeg * pdat->aspect );
    ct      = cos ( raddeg * pdat->tilt );
    sa      = sin ( raddeg * pdat->azim );
    sp      = sin ( raddeg * pdat->aspect );
    st      = sin ( raddeg * pdat->tilt );
    sz      = sin ( raddeg * pdat->zenref );
    pdat->cosinc  = pdat->coszen * ct + sz * st * ( ca * cp + sa * sp );

    if ( pdat->cosinc > 0.0 )
        pdat->etrtilt = pdat->etrn * pdat->cosinc;
    else
        pdat->etrtilt = 0.0;

}


/*============================================================================
*    Void function S_decode
*
*    This function decodes the error codes from S_solpos return value
*
*    Requires the long integer return value from S_solpos
*
*    Returns descriptive text to stderr
*----------------------------------------------------------------------------*/
void S_decode(long code, struct posdata *pdat)
{
  if ( code & (1L << S_YEAR_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the year: %d [1950-2050]\n",
      pdat->year);
  if ( code & (1L << S_MONTH_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the month: %d\n",
      pdat->month);
  if ( code & (1L << S_DAY_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the day-of-month: %d\n",
      pdat->day);
  if ( code & (1L << S_DOY_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the day-of-year: %d\n",
      pdat->daynum);
  if ( code & (1L << S_HOUR_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the hour: %d\n",
      pdat->hour);
  if ( code & (1L << S_MINUTE_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the minute: %d\n",
      pdat->minute);
  if ( code & (1L << S_SECOND_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the second: %d\n",
      pdat->second);
  if ( code & (1L << S_TZONE_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the time zone: %f\n",
      pdat->timezone);
  if ( code & (1L << S_INTRVL_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the interval: %d\n",
      pdat->interval);
  if ( code & (1L << S_LAT_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the latitude: %f\n",
      pdat->latitude);
  if ( code & (1L << S_LON_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the longitude: %f\n",
      pdat->longitude);
  if ( code & (1L << S_TEMP_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the temperature: %f\n",
      pdat->temp);
  if ( code & (1L << S_PRESS_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the pressure: %f\n",
      pdat->press);
  if ( code & (1L << S_TILT_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the tilt: %f\n",
      pdat->tilt);
  if ( code & (1L << S_ASPECT_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the aspect: %f\n",
      pdat->aspect);
  if ( code & (1L << S_SBWID_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the shadowband width: %f\n",
      pdat->sbwid);
  if ( code & (1L << S_SBRAD_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the shadowband radius: %f\n",
      pdat->sbrad);
  if ( code & (1L << S_SBSKY_ERROR) )
    fprintf(stderr, "S_decode ==> Please fix the shadowband sky factor: %f\n",
      pdat->sbsky);
}

