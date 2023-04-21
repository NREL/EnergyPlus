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

#ifndef _SOLPOS_
#define _SOLPOS_ 1

/*============================================================================
*
*    NAME:  solpos00.h
*
*    Contains:
*        S_solpos     (computes the solar position and intensity
*                      from time and place)
*            INPUTS:     (from posdata)
*                          year, month, day, hour, minute, second,
*                          latitude, longitude, timezone, interval
*            OPTIONAL:   (from posdata; defaults from S_init function)
*                            press   DEFAULT 1013.0 (standard pressure)
*                            temp    DEFAULT   10.0 (standard temperature)
*                            tilt    DEFAULT    0.0 (horizontal panel)
*                            aspect  DEFAULT  180.0 (South-facing panel)
*                            sbwid   DEFAULT    7.6 (shadowband width)
*                            sbrad   DEFAULT   31.7 (shadowband radius)
*                            sbsky   DEFAULT   0.04 (shadowband sky factor)
*
*            OUTPUTS:    (posdata) daynum, amass, ampress, azim, cosinc,
*                        elevref, etr, etrn, etrtilt, prime,
*                        sbcf, sretr, ssetr, unprime, zenref
*
*            RETURNS:   Long int status code (defined in solpos.h)
*
*    Usage:
*         In calling program, along with other 'includes', insert:
*
*              #include "solpos.h"
*
*    Martin Rymes
*    National Renewable Energy Laboratory
*    25 March 1998
*----------------------------------------------------------------------------*/

/*============================================================================
*
*     Define the function codes
*
*----------------------------------------------------------------------------*/
#define L_DOY    0x0001
#define L_GEOM   0x0002
#define L_ZENETR 0x0004
#define L_SSHA   0x0008
#define L_SBCF   0x0010
#define L_TST    0x0020
#define L_SRSS   0x0040
#define L_SOLAZM 0x0080
#define L_REFRAC 0x0100
#define L_AMASS  0x0200
#define L_PRIME  0x0400
#define L_TILT   0x0800
#define L_ETR    0x1000
#define L_ALL    0xFFFF

/*============================================================================
*
*     Define the bit-wise masks for each function
*
*----------------------------------------------------------------------------*/
#define S_DOY    ( L_DOY                          )
#define S_GEOM   ( L_GEOM   | S_DOY               )
#define S_ZENETR ( L_ZENETR | S_GEOM              )
#define S_SSHA   ( L_SSHA   | S_GEOM              )
#define S_SBCF   ( L_SBCF   | S_SSHA              )
#define S_TST    ( L_TST    | S_GEOM              )
#define S_SRSS   ( L_SRSS   | S_SSHA   | S_TST    )
#define S_SOLAZM ( L_SOLAZM | S_ZENETR            )
#define S_REFRAC ( L_REFRAC | S_ZENETR            )
#define S_AMASS  ( L_AMASS  | S_REFRAC            )
#define S_PRIME  ( L_PRIME  | S_AMASS             )
#define S_TILT   ( L_TILT   | S_SOLAZM | S_REFRAC )
#define S_ETR    ( L_ETR    | S_REFRAC            )
#define S_ALL    ( L_ALL                          )


/*============================================================================
*
*     Enumerate the error codes
*     (Bit positions are from least significant to most significant)
*
*----------------------------------------------------------------------------*/
/*          Code          Bit       Parameter            Range
      ===============     ===  ===================  =============   */
enum {S_YEAR_ERROR,    /*  0   year                  1950 -  2050   */
      S_MONTH_ERROR,   /*  1   month                    1 -    12   */
      S_DAY_ERROR,     /*  2   day-of-month             1 -    31   */
      S_DOY_ERROR,     /*  3   day-of-year              1 -   366   */
      S_HOUR_ERROR,    /*  4   hour                     0 -    24   */
      S_MINUTE_ERROR,  /*  5   minute                   0 -    59   */
      S_SECOND_ERROR,  /*  6   second                   0 -    59   */
      S_TZONE_ERROR,   /*  7   time zone              -12 -    12   */
      S_INTRVL_ERROR,  /*  8   interval (seconds)       0 - 28800   */
      S_LAT_ERROR,     /*  9   latitude               -90 -    90   */
      S_LON_ERROR,     /* 10   longitude             -180 -   180   */
      S_TEMP_ERROR,    /* 11   temperature (deg. C)  -100 -   100   */
      S_PRESS_ERROR,   /* 12   pressure (millibars)     0 -  2000   */
      S_TILT_ERROR,    /* 13   tilt                   -90 -    90   */
      S_ASPECT_ERROR,  /* 14   aspect                -360 -   360   */
      S_SBWID_ERROR,   /* 15   shadow band width (cm)   1 -   100   */
      S_SBRAD_ERROR,   /* 16   shadow band radius (cm)  1 -   100   */
      S_SBSKY_ERROR};  /* 17   shadow band sky factor  -1 -     1   */

struct posdata
{
  /***** ALPHABETICAL LIST OF COMMON VARIABLES *****/
                           /* Each comment begins with a 1-column letter code:
                              I:  INPUT variable
                              O:  OUTPUT variable
                              T:  TRANSITIONAL variable used in the algorithm,
                                  of interest only to the solar radiation
                                  modelers, and available to you because you
                                  may be one of them.

                              The FUNCTION column indicates which sub-function
                              within solpos must be switched on using the
                              "function" parameter to calculate the desired
                              output variable.  All function codes are
                              defined in the solpos.h file.  The default
                              S_ALL switch calculates all output variables.
                              Multiple functions may be or'd to create a
                              composite function switch.  For example,
                              (S_TST | S_SBCF). Specifying only the functions
                              for required output variables may allow solpos
                              to execute more quickly.

                              The S_DOY mask works as a toggle between the
                              input date represented as a day number (daynum)
                              or as month and day.  To set the switch (to
                              use daynum input), the function is or'd; to
                              clear the switch (to use month and day input),
                              the function is inverted and and'd.

                              For example:
                                  pdat->function |= S_DOY (sets daynum input)
                                  pdat->function &= ~S_DOY (sets month and day input)

                              Whichever date form is used, S_solpos will
                              calculate and return the variables(s) of the
                              other form.  See the soltest.c program for
                              other examples. */

  /* VARIABLE        I/O  Function    Description */
  /* -------------  ----  ----------  ---------------------------------------*/

  int   day;       /* I/O: S_DOY      Day of month (May 27 = 27, etc.)
                                        solpos will CALCULATE this by default,
                                        or will optionally require it as input
                                        depending on the setting of the S_DOY
                                        function switch. */
  int   daynum;    /* I/O: S_DOY      Day number (day of year; Feb 1 = 32 )
                                        solpos REQUIRES this by default, but
                                        will optionally calculate it from
                                        month and day depending on the setting
                                        of the S_DOY function switch. */
  int   function;  /* I:              Switch to choose functions for desired
                                        output. */
  int   hour;      /* I:              Hour of day, 0 - 23, DEFAULT = 12 */
  int   interval;  /* I:              Interval of a measurement period in
                                        seconds.  Forces solpos to use the
                                        time and date from the interval
                                        midpoint. The INPUT time (hour,
                                        minute, and second) is assumed to
                                        be the END of the measurement
                                        interval. */
  int   minute;    /* I:              Minute of hour, 0 - 59, DEFAULT = 0 */
  int   month;     /* I/O: S_DOY      Month number (Jan = 1, Feb = 2, etc.)
                                        solpos will CALCULATE this by default,
                                        or will optionally require it as input
                                        depending on the setting of the S_DOY
                                        function switch. */
  int   second;    /* I:              Second of minute, 0 - 59, DEFAULT = 0 */
  int   year;      /* I:              4-digit year (2-digit year is NOT
                                       allowed */

  /***** FLOATS *****/

  double amass;      /* O:  S_AMASS    Relative optical airmass */
  double ampress;    /* O:  S_AMASS    Pressure-corrected airmass */
  double aspect;     /* I:             Azimuth of panel surface (direction it
                                        faces) N=0, E=90, S=180, W=270,
                                        DEFAULT = 180 */
  double azim;       /* O:  S_SOLAZM   Solar azimuth angle:  N=0, E=90, S=180,
                                        W=270 */
  double cosinc;     /* O:  S_TILT     Cosine of solar incidence angle on
                                        panel */
  double coszen;     /* O:  S_REFRAC   Cosine of refraction corrected solar
                                        zenith angle */
  double dayang;     /* T:  S_GEOM     Day angle (daynum*360/year-length)
                                        degrees */
  double declin;     /* T:  S_GEOM     Declination--zenith angle of solar noon
                                        at equator, degrees NORTH */
  double eclong;     /* T:  S_GEOM     Ecliptic longitude, degrees */
  double ecobli;     /* T:  S_GEOM     Obliquity of ecliptic */
  double ectime;     /* T:  S_GEOM     Time of ecliptic calculations */
  double elevetr;    /* O:  S_ZENETR   Solar elevation, no atmospheric
                                        correction (= ETR) */
  double elevref;    /* O:  S_REFRAC   Solar elevation angle,
                                        deg. from horizon, refracted */
  double eqntim;     /* T:  S_TST      Equation of time (TST - LMT), minutes */
  double erv;        /* T:  S_GEOM     Earth radius vector
                                        (multiplied to solar constant) */
  double etr;        /* O:  S_ETR      Extraterrestrial (top-of-atmosphere)
                                        W/sq m global horizontal solar
                                        irradiance */
  double etrn;       /* O:  S_ETR      Extraterrestrial (top-of-atmosphere)
                                        W/sq m direct normal solar
                                        irradiance */
  double etrtilt;    /* O:  S_TILT     Extraterrestrial (top-of-atmosphere)
                                        W/sq m global irradiance on a tilted
                                        surface */
  double gmst;       /* T:  S_GEOM     Greenwich mean sidereal time, hours */
  double hrang;      /* T:  S_GEOM     Hour angle--hour of sun from solar noon,
                                        degrees WEST */
  double julday;     /* T:  S_GEOM     Julian Day of 1 JAN 2000 minus
                                        2,400,000 days (in order to regain
                                        single precision) */
  double latitude;   /* I:             Latitude, degrees north (south negative) */
  double longitude;  /* I:             Longitude, degrees east (west negative) */
  double lmst;       /* T:  S_GEOM     Local mean sidereal time, degrees */
  double mnanom;     /* T:  S_GEOM     Mean anomaly, degrees */
  double mnlong;     /* T:  S_GEOM     Mean longitude, degrees */
  double rascen;     /* T:  S_GEOM     Right ascension, degrees */
  double press;      /* I:             Surface pressure, millibars, used for
                                        refraction correction and ampress */
  double prime;      /* O:  S_PRIME    Factor that normalizes Kt, Kn, etc. */
  double sbcf;       /* O:  S_SBCF     Shadow-band correction factor */
  double sbwid;      /* I:             Shadow-band width (cm) */
  double sbrad;      /* I:             Shadow-band radius (cm) */
  double sbsky;      /* I:             Shadow-band sky factor */
  double solcon;     /* I:             Solar constant (NREL uses 1367 W/sq m) */
  double ssha;       /* T:  S_SRHA     Sunset(/rise) hour angle, degrees */
  double sretr;      /* O:  S_SRSS     Sunrise time, minutes from midnight,
                                        local, WITHOUT refraction */
  double ssetr;      /* O:  S_SRSS     Sunset time, minutes from midnight,
                                        local, WITHOUT refraction */
  double temp;       /* I:             Ambient dry-bulb temperature, degrees C,
                                        used for refraction correction */
  double tilt;       /* I:             Degrees tilt from horizontal of panel */
  double timezone;   /* I:             Time zone, east (west negative).
                                      USA:  Mountain = -7, Central = -6, etc. */
  double tst;        /* T:  S_TST      True solar time, minutes from midnight */
  double tstfix;     /* T:  S_TST      True solar time - local standard time */
  double unprime;    /* O:  S_PRIME    Factor that denormalizes Kt', Kn', etc. */
  double utime;      /* T:  S_GEOM     Universal (Greenwich) standard time */
  double zenetr;     /* T:  S_ZENETR   Solar zenith angle, no atmospheric
                                        correction (= ETR) */
  double zenref;     /* O:  S_REFRAC   Solar zenith angle, deg. from zenith,
                                        refracted */
};

/* For users that wish to access individual functions, the following table
lists all output and transition variables, the L_ mask for the function
that calculates them, and all the input variables required by that function.
The function variable is set to the L_ mask, which will force S_solpos to
only call the required function.  L_ masks may be ORed as desired.

VARIABLE      Mask       Required Variables
---------  ----------  ---------------------------------------
 amass      L_AMASS    zenref, press
 ampress    L_AMASS    zenref, press
 azim       L_SOLAZM   elevetr, declin, latitude, hrang
 cosinc     L_TILT     azim, aspect, tilt, zenref, coszen,etrn
 coszen     L_REFRAC   elevetr, press, temp
 dayang     L_GEOM     All date, time, and location inputs
 declin     L_GEOM     All date, time, and location inputs
 eclong     L_GEOM     All date, time, and location inputs
 ecobli     L_GEOM     All date, time, and location inputs
 ectime     L_GEOM     All date, time, and location inputs
 elevetr    L_ZENETR   declin, latitude, hrang
 elevref    L_REFRAC   elevetr, press, temp
 eqntim     L_TST      hrang, hour, minute, second, interval
 erv        L_GEOM     All date, time, and location inputs
 etr        L_ETR      coszen, solcon, erv
 etrn       L_ETR      coszen, solcon, erv
 etrtilt    L_TILT     azim, aspect, tilt, zenref, coszen, etrn
 gmst       L_GEOM     All date, time, and location inputs
 hrang      L_GEOM     All date, time, and location inputs
 julday     L_GEOM     All date, time, and location inputs
 lmst       L_GEOM     All date, time, and location inputs
 mnanom     L_GEOM     All date, time, and location inputs
 mnlong     L_GEOM     All date, time, and location inputs
 rascen     L_GEOM     All date, time, and location inputs
 prime      L_PRIME    amass
 sbcf       L_SBCF     latitude, declin, ssha, sbwid, sbrad, sbsky
 ssha       L_SRHA     latitude, declin
 sretr      L_SRSS     ssha, tstfix
 ssetr      L_SRSS     ssha, tstfix
 tst        L_TST      hrang, hour, minute, second, interval
 tstfix     L_TST      hrang, hour, minute, second, interval
 unprime    L_PRIME    amass
 utime      L_GEOM     All date, time, and location inputs
 zenetr     L_ZENETR   declination, latitude, hrang
 zenref     L_REFRAC   elevetr, press, temp
*/


/*============================================================================
*    Long int function S_solpos, adapted from the NREL VAX solar libraries
*
*    This function calculates the apparent solar position and intensity
*    (theoretical maximum solar energy) based on the date, time, and
*    location on Earth. (DEFAULT values are from the optional S_posinit
*    function.)
*
*    Requires:
*        Date and time:
*            year
*            month  (optional without daynum)
*            day    (optional without daynum)
*            daynum
*            hour
*            minute
*            second
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
*        Shadow band parameters:
*            sbwid     DEFAULT 7.6 cm
*            sbrad     DEFAULT 31.7 cm
*            sbsky     DEFAULT 0.04
*        Functionality
*            function  DEFAULT S_ALL (all output parameters computed)
*
*    Returns:
*        everything defined at the top of this listing.
*----------------------------------------------------------------------------*/
long S_solpos (struct posdata *pdat);

/*============================================================================
*    Void function S_init
*
*    This function initiates all of the input functions to S_Solpos().
*    NOTE: This function is optional if you initialize all input parameters
*          in your calling code.
*
*    Requires: Pointer to a posdata structure, members of which are
*           initialized.
*
*    Returns: Void
*
*----------------------------------------------------------------------------*/
void S_init(struct posdata *pdat);


/*============================================================================
*    Void function S_decode
*
*    This function decodes the error codes from S_solpos return value
*
*    INPUTS: Long integer S_solpos return value, struct posdata*
*
*    OUTPUTS: Descriptive text of errors to stderr
*----------------------------------------------------------------------------*/
void S_decode(long code, struct posdata *pdat);

#endif
