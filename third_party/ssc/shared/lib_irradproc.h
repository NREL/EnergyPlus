/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __irradproc_h
#define __irradproc_h

#include <memory>

#include "lib_weatherfile.h"

struct poaDecompReq;

/**
* \file
* \brief File containing calculations for front-side and rear-side irradiance of tracked-tilted surfaces.
*/

/** @defgroup solarpos Old solarpos calculation group
*   The solarpos function calculates the sun position given the local standard time and location.
*   This function is based on a paper by Michalsky published in Solar Energy
*	Vol. 40, No. 3, pp. 227-235, 1988. It calculates solar position for the
*	time and location passed to the function based on the Astronomical
*	Almanac's Algorithm for the period 1950-2050. For data averaged over an
*	interval, the appropriate time passed is the midpoint of the interval.
*	(Example: For hourly data averaged from 10 to 11, the time passed to the
*	function should be 10 hours and 30 minutes). The exception is when the time
*	interval includes a sunrise or sunset. For these intervals, the appropriate
*	time should be the midpoint of the portion of the interval when the sun is
*	above the horizon. (Example: For hourly data averaged from 7 to 8 with a
*	sunrise time of 7:30, the time passed to the function should be 7 hours and
*	and 45 minutes).
*   @{
*/
/*
*
* \param[in] year (e.g. 1986)
* \param[in] month (1-12)
* \param[in] day day of month
* \param[in] hour hour of day local standard time (1-24)
* \param[in] minute minutes pas the hour, local standard time (1-24)
* \param[in] lat latitude in degrees, north positive
* \param[in] lng longitude in degrees, east positive
* \param[in] tz time zone, west longitudes negative
* \param[out] sunn array of elements to return sun parameters to calling function
* \param[out] sunn[0] sun azimuth in radians, measured east from north, 0 to 2*pi
* \param[out] sunn[1] sun zenith in radians, 0 to pi
* \param[out] sunn[2] sun elevation in radians, -pi/2 to pi/2
* \param[out] sunn[3] sun declination in radians
* \param[out] sunn[4] sunrise in local standard time (hrs), not corrected for refraction
* \param[out] sunn[5] sunset in local standard time (hrs), not corrected for refraction
* \param[out] sunn[6] eccentricity correction factor
* \param[out] sunn[7] true solar time (hrs)
* \param[out] sunn[8] extraterrestrial solar irradiance on horizontal at particular time (W/m2)
*/
void solarpos(int year, int month, int day, int hour, double minute, double lat, double lng, double tz, double sunn[9]);
/** @} */ //end of solarpos group

/** @defgroup solarpos_spa Solar Position Algorithm group/
*   The functions in this group are used to calculate the sun's position at any given timestep
*   based on the Solar Position Algorithm reported by Reda and Andreas in NREL/TP-560-34302, 2008.
*   The functions used in this implementation were taken from a C code implementation provided at https://midcdmz.nrel.gov/spa/
*   @{
*/
/**
*   limit_degrees function calculates the degree value of a parameter within 0-360°
*
* \param[in] degrees unconstrained angle in degrees
* \param[out] limited degree value of angle constrained to 360 degrees
*/
double limit_degrees(double degrees);

/**
*   limit_minutes function calculates the Equation of Time minutes value of a parameter within 0-20 minutes
*
* \param[in] minutes unconstrained EoT value in minutes
* \param[out] limited minutes value within 20 minutes
*/
double limit_minutes(double minutes);

/**
*   limit_degrees180 function calculates the degree value of a parameter within 0-180°
*
* \param[in] degrees unconstrained angle in degrees
* \param[out] limited degree value of angle constrained to 180 degrees
*/
double limit_degrees180(double degrees);

/**
*   limit_zero2one function constrains the value of a parameter within 0-1
*
* \param[in] value unconstrained parameter value
* \param[out] limited value of angle constrained to range 0-1
*/
double limitzero2one(double value);

/**
*   limit_degrees180pm Limit degrees to -180-180° (positive westward from the meridian, negative eastward from the meridian)
*
* \param[in] degrees unconstrained degree values
* \param[out] limited value of angle constrained to range -180 - +180°
*/
double limit_degrees180pm(double degrees);

/**
*   third_order_polynomial function calculates the 3rd order polynomial for given coefficients
*   This function is used to calculate the X values of the Solar Position Algorithm
*
* \param[in] a coefficient for x^3 term
* \param[in] b coefficient for x^2 term
* \param[in] c coefficient for x term
* \param[in] d coefficient to x^0 term
* \param[in] x julian ephemeris century value used as the dependent variable in the polynomial
* \param[out] result resulting value of the third order polynomial
*/
double third_order_polynomial(double a, double b, double c, double d, double x);

/**
*	dayfrac_to_local_hour function calculates the local hour time from the day fraction parameter
*
* \param[in] dayfrac parameter in fraction of days
* \param[in] timezone timezone for the location in question
* \param[out] parameter in local hour time
*/
double dayfrac_to_local_hr(double dayfrac, double timezone);

/**
*   julian_day function calculates the julian day based on time parameter inputs
*
* \param[in] year year value of time stamp
* \param[in] month month value of time stamp
* \param[in] day day value of time stamp
* \param[in] hour hour value of time stamp
* \param[in] minute minute value of time stamp
* \param[in] second minute of time stamp
* \param[in] dut1 delta time value (fillin)
* \param[in tz time zone of time stamp
* \param[out] jd Julian day value of time stamp
*/
double julian_day(int year, int month, int day, int hour, int minute, double second, double dut1, double tz);

/**
*   julian_century function calculates the julian century value for the time stamp
*
* \param[in] jd julian day for the given time stamp found using julian_day
* \param[out] jc julian century value for given time stamp
*/
double julian_century(double jd);

/**
*   julian_ephemeris_day function calculates the julian ephemeris day value for the time stamp
*
* \param[in] jd julian day for the given time stamp found using julian_day
* \param[in] delta_t difference between earth time and terrestrial time (fillin)
* \param[out] jde julian ephemeris day value for given time stamp
*/
double julian_ephemeris_day(double jd, double delta_t);

/**
*   julian_ephemeris_century function calculates the julian ephemeris century value for the time stamp
*
* \param[in] jde julian ephemeris day
* \param[out] jce julian ephemeris century
*/

/**
*   julian_ephemeris_millennium function calculates the julian ephemeris millennium value for the time stamp
*
* \param[in] jce julian ephemeris century
* \param[out] jme julian ephemeris millennium
*/
double julian_ephemeris_millennium(double jce);

/**
*   earth_periodic_term_summation function calculates the sum of the earth heliocentric longitude (L), latitude (B), and radius vector (R) terms (L0, L1, etc.)
*
* \param[in] terms[][3] matrix of L, B, and R terms with number of rows depending on parameter
* \param[in] count number of rows for given parameter set (Letter, Number)
* \param[in] jme juliam ephemeris millennium
* \param[out] sum summation of the calculations for the given parameter inputs
*/
double earth_periodic_term_summation(const double terms[][3], int count, double jme);

/**
*   earth_values function calculates the earth heliocentric longitude (L), latitude (B), and radius vector (R)
*
* \param[in] term_sum[] matrix containing all terms of parameter (L0-L5, R0, etc)
* \param[in] count number of parameters for given variable (example: L0-L5 count = 6)
* \param[in] jme juliam ephemeris millennium
* \param[out] sum summation of the calculations resulting in L, R, and B (depending on inputs)
*/
double earth_values(double term_sum[], int count, double jme);

/**
*   earth_heliocentric_longitude function calculates the earth heliocentric longitude (L) using earth_periodic_term_summation and earth_values
*
* \param[in] jme juliam ephemeris millennium
* \param[out] earth_helio_longitude earth heliocentric longitude constrained to 0-360°
*/
double earth_heliocentric_longitude(double jme);

/**
*   earth_heliocentric_latitude function calculates the earth heliocentric latitude (B) using earth_periodic_term_summation and earth_values
*
* \param[in] jme juliam ephemeris millennium
* \param[out] earth_helio_latitude earth heliocentric latitude in unconstrained degrees
*/
double earth_heliocentric_latitude(double jme);

/**
*   earth_radius vector function calculates the earth radius vector (R) using earth_periodic_term_summation and earth_values
*
* \param[in] jme juliam ephemeris millennium
* \param[out] earth_rad_vector earth radius vector in astronomical units (AU)
*/
double earth_radius_vector(double jme);

/**
*   geocentric_longitude function calculates the geocentric longitude based on the heliocentric longitude (L)
*
* \param[in] l heliocentric longitude
* \param[out] theta geocentric longitude constrained to 0-360°
*/
double geocentric_longitude(double l);

/**
*   geocentric_latitude function calculates the geocentric latitude based on the heliocentric latitude
*
* \param[in] b heliocentric latitude
* \param[out] -b geocentric latitude is the negative of the heliocentric latitude in degrees
*/
double geocentric_latitude(double b);

/**
*   mean_elongation_moon_sun function calculates the mean elongation of the moon from the sun (X_0)
*
* \param[in] jce julian ephemeris century
* \param[out] mean_elong_moon_sun mean elongation from moon to sun in degrees
*/
double mean_elongation_moon_sun(double jce);

/**
*   mean_anomaly_sun function calculates the mean anomaly of the sun (X_1)
*
* \param[in] jce julian ephemeris century
* \param[out] mean_anom_sun mean anomaly of the sun in degrees
*/
double mean_anomaly_moon(double jce);

/**
*   mean_anomaly_sun function calculates the mean anomaly of the moon (X_2)
*
* \param[in] jce julian ephemeris century
* \param[out] mean_anom_moon mean anomaly of the moon in degrees
*/
double mean_anomaly_moon(double jce);

/**
*   argument_latitude_moon function calculates the moon's argument of latitude (X_3)
*
* \param[in] jce julian ephemeris century
* \param[out] argument_lat_moon moon's argument of latitude in degrees
*/
double argument_latitude_moon(double jce);

/**
*   ascending_latitude_moon function  Calculates the longitude of the ascending node of the moon’s mean orbit on the
*	ecliptic, measured from the mean equinox of the date, (X_4) (in degrees),
*
* \param[in] jce julian ephemeris century
* \param[out] ascending_long_moon longitude of the ascending node of the moon's mean orbit on the ecliptic in degrees
*/
double ascending_longitude_moon(double jce);

/**
*   xy_term_summation function calculates the summation of the X_i and Y_i components in the nutation calculations
*
* \param[in] i integer used to specify row of Y terms from report table
* \param[in] x[5] matrix containing X terms (X_0-X_4) used in summation
* \param[out] sum summation of X and Y terms for the given Y row
*/
double xy_term_summation(int i, double x[5]);

/**
*   nutation_longitude_and_obliquity function calculates the nutation longitude and obliquity by using xy_term_summation to sum the results for each row of Y
*
* \param[in] jce julian ephemeris century
* \param[in] x[5] matrix containing X terms (X_0-X_4) used in summation
* \param[out] delta_values[0] nutation of longitude in degrees
* \param[out] delta_values[1] nutation of obliquity in degrees
*/
void nutation_longitude_and_obliquity(double jce, double x[5], double delta_values[2]);

/**
*   ecliptic_mean_obliquity function calculates the mean obliquity of the ecliptic in arc seconds
*
* \param[in] jme julian ephemeris millennium
* \param[out] eclip_mean_obliquity mean obliquity of the ecliptic in arc seconds
*/
double ecliptic_mean_obliquity(double jme);

/**
*   ecliptic_true_obliquity function calculates the true obliquity of the ecliptic in degrees
*
* \param[in] delta_epsilon nutation of the longitude in degrees
* \param[in] epsilon0 elciptic mean obliquity in arc seconds
* \param[out] eclip_true_obliquity true obliquity of the ecliptic in degrees
*/
double ecliptic_true_obliquity(double delta_epsilon, double epsilon0);

/**
*   aberration_correction function calculates the aberration correction (delta_Tau) in degrees
*
* \param[in] r heliocentric radius vector R
* \param[out] delta_tau aberration correction in degrees
*/
double aberration_correction(double r);

/**
*   apparent_sun_longitude function calculates the apparent sun longitude (lambda) in degrees
*
* \param[in] theta geocentric longitude in degrees
* \param[in] delta_psi nutation in longitude in degrees
* \param[in] delta_epsilon aberration correction in degrees
* \param[out] lambda apparent sun longitude in degrees
*/
double apparent_sun_longitude(double theta, double delta_psi, double delta_tau);

/**
*   greenwhich_mean_sidereal_time function calculates the mean sidereal time at Greenwich at any given time (nu0) in degrees
*
* \param[in] jd julian day
* \param[in] jc julian century
* \param[out] nu0 apparent sidereal time at Greenwich at any given time in degrees
*/
double greenwich_mean_sidereal_time(double jd, double jc);

/**
*   greenwhich_sidereal_time function calculates the apparent sidereal time at Greenwich at any given time (v) in degrees
*
* \param[in] nu0 mean sidereal time at Greenwich at any given time in degrees
* \param[in] delta_psi nutation in longitude in degrees
* \param[in] epsilon true obliquity of the ecliptic in degrees
* \param[out] nu apparent sidereal time at Greenwhich at any given time in degrees
*/
double greenwich_sidereal_time(double nu0, double delta_psi, double epsilon);

/**
*   geocentric_right_ascension function calculates the geocentric sun right ascension (alpha) in degrees
*
* \param[in] lamda apparent sun longitude in degrees
* \param[in] epsilon true obliquity of the ecliptic in degrees
* \param[in] beta geocentric latitude in degrees
* \param[out] alpha geocentric sun right ascension in degrees
*/
double geocentric_right_ascension(double lamda, double epsilon, double beta);

/**
*   geocentric_declination function calculates the geocentric sun declination (delta) in degrees
*
* \param[in] beta geocentric latitude in degrees
* \param[in] epsilon true obliquity of the ecliptic in degrees
* \param[in] lamda apparent sun longitude in degrees
* \param[out] delta geocentric sun declination in degrees
*/
double geocentric_declination(double beta, double epsilon, double lamda);

/**
*   observer_hour_angle function calculates the observer local hour angle (H) in degrees
*
* \param[in] nu apparent sidereal time in Greenwich in degrees
* \param[in] longitude observer geographical longitude in degrees (positive for east of Greenwich)
* \param[in] alpha_deg geocentric sun right ascension in degrees
* \param[out] H observer local hour angle in degrees (constrained to 0-360°)
*/
double observer_hour_angle(double nu, double longitude, double alpha_deg);

/**
*   sun_equatorial_horizontal_parallax function calculates the equatorial horizontal parallax to the sun (zeta) in degrees
*   This function is based on the C code implementation of the Solar Position Algorithm
*   based on a paper by Reda and Andreas published in NREL/TP-
*	560-34302, 2008.
*
* \param[in] r earth radius vector in degrees
* \param[out] xi sun equatorial horizontal parallax in degrees
*/
double sun_equatorial_horizontal_parallax(double r);

/**
*   right_ascension_parallax_and_topocentric_dec function calculates the parallax in the sun right ascension and
*   the topocentric sun declination both in degrees
*
* \param[in] latitude observer geographical latitude in degrees
* \param[in] elevation observer geographical elevation in meters
* \param[in] xi sun equatorial horizontal parallax in degrees
* \param[in] h observer local hour angle in degrees
* \param[in] delta geocentric sun declination in degrees
* \param[out] delta_alpha parallax in the sun right ascension in degrees
* \param[out] delta_prime topocentric sun declination in degrees
*/
void right_ascension_parallax_and_topocentric_dec(double latitude, double elevation,
    double xi, double h, double delta, double delta_alpha_prime[2]);

/**
*   topocentric_right_ascension function calculates the topocentric sun right ascension (alpha_prime) in degrees
*
* \param[in] alpha_deg sun right ascension in degrees
* \param[in] delta_alpha parallax in the sun right ascension in degrees
* \param[out] alpha_prime topocentric sun right ascension in degrees
*/
double topocentric_right_ascension(double alpha_deg, double delta_alpha);

/**
*   topocentric_local_hour_angle function calculates the topocentric local hour angle (H_prime) in degrees
*
* \param[in] h observer local hour angle in degrees (H)
* \param[in] delta_alpha parallax in the sun right ascension in degrees
* \param[out] h_prime topocentric local hour angle in degrees
*/
double topocentric_local_hour_angle(double h, double delta_alpha);

/**
*   topocentric_elevation_angle function calculates the topocentric elevation angle without atmospheric refraction (e0) in degrees
*
* \param[in] latitude observer geographical latitude in degrees
* \param[in] delta_prime topocentric sun declination in degrees
* \param[in] h_prime topocentric local hour angle in degrees
* \param[out] e0 topocentric elevation angle in degrees
*/
double topocentric_elevation_angle(double latitude, double delta_prime, double h_prime);

/**
*   atmospheric_refraction_correction function calculates the atmospheric refraction correction (delta_e) in degrees
*
* \param[in] pressure annual average local pressure (millibar)
* \param[in] temperature annual average local temperature (°C)
* \param[in] atmos_refract (fillin)
* \param[in] e0 topocentric elevation angle without refraction correction in degrees
* \param[out] del_e atmospheric refraction correction in degrees
*/
double atmospheric_refraction_correction(double pressure, double temperature,
    double atmos_refract, double e0);

/**
*   topocentric_elevation_angle_corrected function calculates the refraction corrected topocentric elevation angle (e) in degrees
*
* \param[in] e0 topocentric elevation angle without refraction correction in degrees
* \param[in] delta_e refraction correction in degrees
* \param[out] e topocentric elevation angle in degrees
*/
double topocentric_elevation_angle_corrected(double e0, double delta_e);

/**
*   topocentric_zenith_angle function calculates the topocentric zenith angle (theta) in degrees
*
* \param[in] e topocentric elevation angle
* \param[out] theta topocentric zenith angle in degrees
*/
double topocentric_zenith_angle(double e);

/**
*   topocentric_azimuth_angle_astro function calculates the topocentric astronomers azimuth angle in degrees
*
* \param[in] h_prime topocentric local hour angle in degrees
* \param[in] latitude observer's geographical latitude in degrees
* \param[in]  delta_prime topocentric sun declination in degrees
* \param[out] azimuth_astro topocentric azimuth angle for astronomers in degrees (west of south)
*/
double topocentric_azimuth_angle_astro(double h_prime, double latitude, double delta_prime);

/**
*   topocentric_azimuth_angle function calculates the topocentric azimuth angle in degrees
*
* \param[in] azimuth_astro topocentric azimuth angle for astronomers (west of south) in degrees
* \param[out] azimuth topocentric azimuth angle for solar radiation (east of north) in degrees (constrained 0-360°)
*/
double topocentric_azimuth_angle(double azimuth_astro);

/**
*   surface_incidence_angle function calculates the incidence angle for a surface oriented in any direction (degrees)
*
* \param[in] zenith topocentric zenith angle (theta) in degrees
* \param[in] azimuth_astro topocentric azimuth angle for astronomers (west of south) in degrees
* \param[in] azm_rotation surface azimuth rotation angle (array azimuth) in degrees (positive for west of south)
* \param[in] slope  tilt angle of surface in degrees
* \param[out] azimuth topocentric azimuth angle for solar radiation (east of north) in degrees (constrained 0-360°)
*/
double surface_incidence_angle(double zenith, double azimuth_astro, double azm_rotation, double slope);

/**
*   sun_mean_longitude function calculates the sun's mean longitude in degrees
*
* \param[in] jme julian ephemeris millenium
* \param[out] M sun's mean longitude in degrees
*/
double sun_mean_longitude(double jme);

/**
*   eot function calculates the Equation of Time in degrees
*
* \param[in] m Sun's mean longitude in degrees
* \param[in] alpha geocentric sun right ascension in degrees
* \param[in] del_psi nutation of longitude in degrees
* \param[in]  epsilon true obliquity of the ecliptic in degrees
* \param[out] E equation of time in degrees
*/
double eot(double m, double alpha, double del_psi, double epsilon);

/**
*   approx_sun_transit_time function calculates the approximate sun transit time (m0) in fraction of day
*
* \param[in] alpha_zero geocentric right ascension at the time in question degrees
* \param[in] longitude observer's geographical longitude in degrees
* \param[in] nu apparent sidereal time at Greenwhich at any given time in
* \param[out] m0 approximate sun transit time in fraction of day
*/
double approx_sun_transit_time(double alpha_zero, double longitude, double nu);

/**
*   sun_hour_angle_at_rise_set function calculates the local hour angle corresponding to the sun elevation equal -0.833°
*
* \param[in] latitude observer's geographical latitude in degrees
* \param[in] delta_zero geocentric sun declination in degrees
* \param[in] h0_prime sun elevation equal -0.833°
* \param[out] H0 local hour angle at sunrise and sunset in degrees
*/
double sun_hour_angle_at_rise_set(double latitude, double delta_zero, double h0_prime);

/**
*   approx_sun_rise_and_set function calculates the approximate sunrise and sunset in fraction of day
*
* \param[in] h0 local hour angle at sunrise and sunet in degrees
* \param[out] m_rts[0] sunrise in fraction of day (constrained 0-1)
* \param[out] m_rts[1] sunset in fraction of day (constrained 0-1)
* \param[out] m_rts[2] sun transit in fraction of day (constrained 0-1)
*/
void approx_sun_rise_and_set(double h_0, double m_rts[3]);

/**
*   rts_alpha_delta_prime function calculates the alpha and delta prime for the sunrise and sunset calculations (fillin)
*
* \param[in] n terms n_i for sunrise and sunset calculations
* \param[in] ad[0] JD_MINUS
* \param[in] ad[1] JD_ZERO
* \param[in] ad[2] JD_PLUS
* \param[out] rts_alpha_delta_prime alpha prime or delta prime depending on inputs
*/
double rts_alpha_delta_prime(double n, double ad[3]);

/**
*   rts_sun_altitude function calculates the sun altitude for the sunrise, sunset, and sun transit
*
* \param[in] latitude observer's geographical latitude in degrees
* \param[in] delta_prime delta prime from sunrise and sunset calculations
* \param[in] h_prime local hour angle for sunrise, sunset, or sun transit
* \param[out] rts_sun_altitude sun altitude for the sunrise, sunset, and sun transit in degrees
*/
double rts_sun_altitude(double latitude, double delta_prime, double h_prime);

/**
*   sun_rise_and_set function calculates the sun altitude for the sunrise, sunset, and sun transit
*
* \param[in] m_rts[]
* \param[in] h_rts[]
* \param[in] delta_prime[]
* \param[in] latitude observer's geographical latitude in degrees
* \param[in] h_prime[]
* \param[in] h0_prime
* \param[in] sun
* \param[out] sun_rise_and_set sunrise or sunset depending on inputs
*/
double sun_rise_and_set(double* m_rts, double* h_rts, double* delta_prime, double latitude,
    double* h_prime, double h0_prime, int sun);

/**
*   calculate_spa function combines numerous functions to calculate the Solar Position Algorithm parameters
*
* \param[in] jd julian day calculated outside of function for compatibility with sunrise sunset calcs
* \param[in] lat observer's geograpical latitude in degrees
* \param[in] lng observer's geographical longitude in degrees
* \param[in] alt observer's geographical elevation in meters
* \param[in] pressure average annual air pressure of the site in millibars
* \param[in] temp average annual air temperature in degrees Celsius
* \param[in] delta_t difference between earth rotation time and Terrestrial Time (TT) in seconds (reported annually in Astronomical Almanac)
* \param[in] tilt tilt angle of the surface for angle of incidence calculation (degrees)
* \param[in] azm_rotation azimuth rotation of solar array (measured west of south?) (degrees)
* \param[out] ascension_and_declination stores right ascension and declination values to be passed to sunrise sunset calculations
* \param[out] ascension_and_declination[0] right ascension value in degrees
* \param[out] ascension_and_declination[1] declination value in degrees
* \param[out] needed_values values needed to be taken out of function to pass as outputs to calculate_spa function
* \param[out] needed_values[0] jme julian ephemeris millenium
* \param[out] needed_values[1] eccentricity correction factor
* \param[out] needed_values[2] del_psi nutation in longitude (degrees)
* \param[out] needed_values[3] epsilon true obliquity of the ecliptic (degrees)
* \param[out] needed_values[4] nu apparent sidereal time at Greenwich (degrees)
* \param[out] needed_values[5] delta_prime topocentric declination (degrees)
* \param[out] needed_values[6] e topocentric elevation angle corrected for atmospheric refraction (degrees)
* \param[out] needed_values[7] zenith topocentric zenith angle (degrees)
* \param[out] needed_values[8] azimuth topocentric azimuth angle (degrees)
*/
void calculate_spa(double jd, double lat, double lng, double alt, double pressure, double temp,
    double delta_t, double tilt, double azm_rotation, double ascension_and_declination[2], double needed_values[9]);

/**
*
*	calculate_eot_and_sun_rise_transit_set function calculates Equation of Time (EoT) and sunrise and
*	sunset times based on the Solar Position Algorithm equations in Appendix A
*
* \param[in] jme julian ephemeris millennium
* \param[in] tz time zone
* \param[in] alpha right ascension from calculate spa output (needed_values[])
* \param[in] del_psi nutation in longitude from calculate_spa output (needed_values[1])
* \param[in] epsilon true obliquity of the ecliptic from calculate_spa output (needed_values[2])
* \param[in] jd julian day
* \param[in] year year integer of time stamp
* \param[in] month month integer of time time stamp
* \param[in] day day integer of time stamp
* \param[in] lat observer's geographical latitude in degrees
* \param[in] lng observer's geographical longitude in degrees
* \param[in] alt observer's geographical elevation (altitude) in meters
* \param[in] pressure average annual air pressure in millibars
* \param[in] temp average annual air temperature in degrees Celsius
* \param[in] tilt tilt angle of surface for angle of incidence (AOI) calculation (degrees)
* \param[in] delta_t difference in earth rotation time and Terrestrial Time (TT) in seconds (reported annualy in Astronomical Almanac)
* \param[in] azm_rotation azimuth rotation of array in degrees (measured west of south?)
* \param[in] nu apparent sidereal time in Greenwich (degrees) from calculate_spa output
* \param[out] needed_values values needed to be taken out of function to pass as outputs to calculate_spa function
* \param[out] needed_values[0] E Equation of Time (degrees)
* \param[out] h0 sunrise hour angle (degrees)
* \param[out] needed_values[1] approximate sunrise time (hr)
* \param[out] needed_values[2] approximate sunset time (hr)
*/
void calculate_eot_and_sun_rise_transit_set(double jme, double tz, double alpha, double del_psi, double epsilon, double jd, int year,
    int month, int day, double lat, double lng, double alt, double pressure, double temp, double tilt, double delta_t, double azm_rotation,
    double needed_values[4]);

/**
*   solarpos_spa function calculates the sun position given the local standard time and location.
*   It calculates solar position for the time and location
*	passed to the function based on the Astronomical
*	Almanac's Algorithm for the period -2000-6000. For data averaged over an
*	interval, the appropriate time passed is the midpoint of the interval.
*	(Example: For hourly data averaged from 10 to 11, the time passed to the
*	function should be 10 hours and 30 minutes). The exception is when the time
*	interval includes a sunrise or sunset. For these intervals, the appropriate
*	time should be the midpoint of the portion of the interval when the sun is
*	above the horizon. (Example: For hourly data averaged from 7 to 8 with a
*	sunrise time of 7:30, the time passed to the function should be 7 hours and
*	and 45 minutes).
*
* \param[in] year (e.g. 1986)
* \param[in] month (1-12)
* \param[in] day day of month
* \param[in] hour hour of day local standard time (1-24)
* \param[in] minute minutes past the hour, local standard time (1-24)
* \param[in] second seconds past the minute, local standard time (0-59)
* \param[in] lat latitude in degrees, north positive
* \param[in] lng longitude in degrees, east positive
* \param[in] tz time zone, west longitudes negative
* \param[in] dut1 fractional second difference between UTC and UT which is used to adjust UTC for earth's irregular rotation rate (http://maia.usno.navy.mil/ser7/ser7.dat) (-1 to 1 second)
* \param[in] alt altitude in meters
* \param[in] pressure pressure in millibars
* \param[in] temp ambient temperature (dry-bulb temperature) in degrees C
* \param[in] tilt tilt angle of surface for angle of incidence calculation (degrees)
* \param[in] azm_rotation azimuth rotation of surface (surface azimuth measured from south?)
* \param[out] sunn array of elements to return sun parameters to calling function
* \param[out] sunn[0] sun azimuth in radians, measured east from north, 0 to 2*pi
* \param[out] sunn[1] sun zenith in radians, 0 to pi
* \param[out] sunn[2] sun elevation in radians, -pi/2 to pi/2
* \param[out] sunn[3] sun declination in radians
* \param[out] sunn[4] sunrise in local standard time (hrs), not corrected for refraction
* \param[out] sunn[5] sunset in local standard time (hrs), not corrected for refraction
* \param[out] sunn[6] eccentricity correction factor
* \param[out] sunn[7] true solar time (hrs)
* \param[out] sunn[8] extraterrestrial solar irradiance on horizontal at particular time (W/m2)
*/
void solarpos_spa(int year, int month, int day, int hour, double minute, double second, double lat, double lng, double tz, double dut1, double alt, double pressure, double temp, double tilt, double azm_rotation, double sunn[9]);
/** @} */ // end of solarpos_spa group

/**
* incidence function calculates the incident angle of direct beam radiation to a surface.
* The calculation is done for a given sun position, latitude, and surface orientation.
* The modes available are fixed tilt, 1-axis tracking, and 2-axis tracking.
*
* \param[in] mode 0 for fixed-tilt, 1 for 1-axis tracking, 2 for 2-axis tracking, 3 for azimuth-axis tracking, 4 for timeseries tilt tracking (in "set surface" function, this is set as mode 0)
* \param[in] tilt tilt angle of surface from horizontal in degrees (mode 0), or tilt angle of tracker axis from horizontal in degrees (mode 1), MUST BE FROM 0 to 90 degrees.
* \param[in] sazm surface azimuth in degrees of collector (mode 0), or surface azimuth of tracker axis (mode 1) with axis azimuth directed from raised to lowered end of axis if axis tilted.
* \param[in] rlim plus or minus rotation in degrees permitted by physical constraints of tracker, range is 0 to 180 degrees.
* \param[in] zen sun zenith in radians, MUST BE LESS THAN PI/2
* \param[in] azm sun azimuth in radians, measured east from north
* \param[in] en_backtrack enable backtracking, using Ground coverage ratio ( below )
* \param[in] gcr  ground coverage ratio ( used for backtracking )
* \param[in] force_to_stow: force the single-axis tracking array to the stow angle specified in the next input
* \param[in] stow_angle_deg: the angle to force the single-axis tracking array to stow to, in degrees
* \param[out] angle array of elements to return angles to calling function
* \param[out] angle[0] incident angle in radians
* \param[out] angle[1] tilt angle of surface from horizontal in radians
* \param[out] angle[2] surface azimuth in radians, measured east from north
* \param[out] angle[3] tracking axis rotation angle in radians, measured from surface normal of unrotating axis (only for 1 axis trackers)
* \param[out] angle[4] backtracking difference (rot - ideal_rot) will be zero except in case of backtracking for 1 axis tracking
*/
void incidence(int mode, double tilt, double sazm, double rlim, double zen, double azm, bool en_backtrack, double gcr, bool force_to_stow, double stow_angle_deg, double angle[5]);


/**
* Perez function for calculating values of diffuse + direct
* solar radiation + ground reflected radiation for a tilted surface and returns the total plane-of-array irradiance(poa),
* see also isotropic(), hdkr().
*
* Function does not check all input for valid entries; consequently, this should be
* done before calling the function.  (Reference: Perez et al, Solar Energy Vol. 44, No.5, pp.271-289,1990.)
*
* \param[in] hextra extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
* \param[in] dn direct normal radiation (W/m2)
* \param[in] df diffuse horizontal radiation (W/m2)
* \param[in] alb surface albedo (decimal fraction)
* \param[in] inc incident angle of direct beam radiation to surface in radians
* \param[in] tilt surface tilt angle from horizontal in radians
* \param[in] zen sun zenith angle in radians
* \param[out] poa calculated plane-of-array irradiances (W/m2)
* \param[out] poa[0] incident beam
* \param[out] poa[1] incident sky diffuse
* \param[out] poa[2] incident ground diffuse
* \param[out] diffc diffuse components, if an array is provided
* \param[out] diffc[0] isotropic diffuse
* \param[out] diffc[1] circumsolar diffuse
* \param[out] diffc[2] horizon brightening
*/
void perez(double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */);

/**
* Isotropic sky model for diffuse irradiance on a tilted surface, see also perez(), hdkr().
*
* \param[in] hextra extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
* \param[in] dn direct normal radiation (W/m2)
* \param[in] df diffuse horizontal radiation (W/m2)
* \param[in] alb surface albedo (decimal fraction)
* \param[in] inc incident angle of direct beam radiation to surface in radians
* \param[in] tilt surface tilt angle from horizontal in radians
* \param[in] zen sun zenith angle in radians
* \param[out] poa calculated plane-of-array irradiances (W/m2)
* \param[out] poa[0] incident beam
* \param[out] poa[1] incident sky diffuse
* \param[out] poa[2] incident ground diffuse
* \param[out] diffc diffuse components, if an array is provided
* \param[out] diffc[0] isotropic diffuse
* \param[out] diffc[1] circumsolar diffuse
* \param[out] diffc[2] horizon brightening
*/
void isotropic(double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */);

/**
* HDKR (Hay, Davies, Klutcher, Reindl) model for diffuse irradiance on a tilted surface, see also perez(), isotropic().
*
* \param[in] hextra extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
* \param[in] dn direct normal radiation (W/m2)
* \param[in] df diffuse horizontal radiation (W/m2)
* \param[in] alb surface albedo (decimal fraction)
* \param[in] inc incident angle of direct beam radiation to surface in radians
* \param[in] tilt surface tilt angle from horizontal in radians
* \param[in] zen sun zenith angle in radians
* \param[out] poa calculated plane-of-array irradiances (W/m2)
* \param[out] poa[0] incident beam
* \param[out] poa[1] incident sky diffuse
* \param[out] poa[2] incident ground diffuse
* \param[out] diffc diffuse components
* \param[out] diffc[0] isotropic diffuse
* \param[out] diffc[1] circumsolar diffuse
* \param[out] diffc[2] horizon brightening
*/
void hdkr(double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */);


/**
* poaDecomp is a function to decompose input plane-of-array irradiance into direct normal, diffuse horizontal, and global horizontal.
*
* \param[in] weatherFilePOA Plane of array irradiance measured from weatherfile (W/m2)
* \param[in] angle[] surface angles calculated from incidence()
* \param[in] sun sun position angles calculated from solarpos()
* \param[in] alb albedo (0-1)
* \param[out] pA structure containing data required for the decomposition
* \param[out] dn Direct Normal Irradiance (W/m2)
* \param[out] df Diffuse Horizontal Irradiance (W/m2)
* \param[out] gh Global Horizontal Irradiance (W/m2)
* \param[out] poa calculated plane-of-array irradiances (beam, sky diffuse, ground diffuse) (W/m2)
* \param[out] diffc diffuse components (isotropic, circumsolar, horizon) (W/m2)
*
*/
int poaDecomp(double wfPOA, double angle[], double sun[], double alb, poaDecompReq* pA, double& dn, double& df, double& gh, double poa[3], double diffc[3]);

/**
* ModifiedDISC calculates direct normal (beam) radiation from global horizontal radiation.
*  This function uses a disc beam model to calculate the beam irradiance returned.
*  The argument g is an array of 3 values. The values are the global irradiance of the
*  previous reading, the current reading ,and the next reading in that order. The argument
*  z uses the same format, except the values are the respective solar zenith angles. If any
*  of the g or z values are not available or the previous or next readings did not occur
*  within 1.5 hours of the current reading then the appropriate value or values should be
*  replaced with a -999.0. If the argument td is missing then the value -999.0 should be
*  used in place of the missing argument. The current global irradiance (g[1]) must have a
*  value. If the dew point temperature (td) is missing then td is not used to find an index
*  into the correction matrix (cm), instead a special column in the matrix is used. If the
*  previous global irradiance (g[0]) or solar zenith angle (z[0]) and the next global
*  irradiance (g[2]) or solar zenith angle (z[2]) are missing then delta kt' (dkt1) is not
*  used to find an index into the correction matrix (cm), instead a special column in the
*  matrix is used.
*
* \param[in] g[3] global irradiance array (watts / sq. meter)
* \param[in] z[3] solar zenith angle array (radians)
* \param[in] td dew point temperature (degrees c)
* \param[in] doy julian day of year
* \param[in] alt altitude of site (meters)
* \param[out] dn beam irradiance (W/m2)
*/
double ModifiedDISC(const double g[3], const double z[3], double td, double alt, int doy, double& dn);

/**
* ModifiedDISC calculates direct normal (beam) radiation from global horizontal radiation.
* This is a modification to the orininally provided Modified-DISC model which takes
* as input the four bin variables and GHI and returns the resulting DNI
*
* \param[in] kt[3] incidence angle modifiers for direct, diffuse, global
* \param[in] kt1[3] kt prime
* \param[in] g[3] global irradiance array (watts / sq. meter)
* \param[in] z[3] solar zenith angle array (radians)
* \param[in] td dew point temperature (degrees c)
* \param[in] alt altitude of site (meters)
* \param[in] doy julian day of year
* \param[out] dn beam irradiance (watts / sq. meter) through call-by-reference
*/
void ModifiedDISC(const double kt[3], const double kt1[3], const double g[3], const double z[3], double td, double alt, int doy, double& dn);

/**
* shadeFraction1x calculates the self-shaded fraction of a tracker array. The shaded
* fraction represents the fractional distance (0 to 1) up a tilted row affected by
* direct (beam) shading from the row in front of it.
*
* \param[in] solar_azimuth sun azimuth in degrees, measured east from north
* \param[in] solar_zenith sun zenith in degrees
* \param[in] axis_tilt axis tilt angle from horizontal in degrees
* \param[in] axis_azimuth axis azimuth in degrees, measured east from north
* \param[in] gcr ground coverage ratio of system
* \param[in] rotation tracking axis rotation angle in degrees
* \return fraction shaded (0-1) if system is shaded (0 for unshaded)
*/
double shadeFraction1x(double solar_azimuth, double solar_zenith, double axis_tilt, double axis_azimuth, double gcr, double rotation);

/**
* truetrack calculates the tracker rotation that minimizes the angle of incidence betweem direct irradiance and the module front surface normal
*
* \param[in] solar_azimuth sun azimuth in degrees, measured east from north
* \param[in] solar_zenith sun zenith in degrees
* \param[in] axis_tilt surface tilt angle from horizontal in degrees
* \param[in] axis_azimuth surface azimuth in degrees of collector
* \return true-tracking rotation angle in degrees
*/
double truetrack(double solar_azimuth, double solar_zenith, double axis_tilt, double axis_azimuth);

/**
* backtrack finds the optimum angle to use to reduce self-shading on the front-side of modules using backtracking
*
* \param[in] ideal (true-tracking) axis rotation angle in degrees, not adjusted for physical limits or stow
* \param[in] gcr ground coverage ratio (0-1) of array
* \return updated rotation angle in degrees after backtracking
*/
double backtrack(double truetracking_rotation, double gcr);


/**
* \class irrad
*
*  The irrad class contains data and methods to calculate plane-of-array irradiance for the front and rear side of a module.
*  The class contains supporting methods to compute the sun position, sun angles, and surface angles for tracked systems.
*/
class irrad
{
protected:

    // Position inputs
    double latitudeDegrees;			///< latitude in degrees, north positive
    double longitudeDegrees;		///< longitude in degrees, east positive
    double timezone;				///< time zone, west longitudes negative
    double elevation;               // site elevation (meters)
    double pressure;
    double tamb;

    // Model settings
    int skyModel;					///< sky model selection as defined in \link Irradiance_IO::SKYMODEL 
    int radiationMode;				///< radiation input mode as defined in \link Irradiance_IO::RADMODE
    int trackingMode;				///< the subarray tracking model as defined in \link Subarray_IO::tracking
    bool enableBacktrack;			///< Boolean value for whether backtracking is enabled or not
    bool forceToStow;				///< Boolean value for whether or not a single-axis tracker can be set to a stow angle

    // Time inputs
    int year, month, day, hour;
    double minute, delt;

    // Subarray properties
    double tiltDegrees;				///< Surface tilt of subarray in degrees
    double surfaceAzimuthDegrees;	///< Surface azimuth of subarray in degrees
    double rotationLimitDegrees;	///< Rotation limit for subarray in degrees
    double stowAngleDegrees;		///< Optional stow angle for the subarray in degrees
    double groundCoverageRatio;		///< Ground coverage ratio of subarray
    poaDecompReq* poaAll;			///< Data required to decompose input plane-of-array irradiance

    // Input Front-Side Irradiation components 
    double globalHorizontal;		///< Input global horizontal irradiance (W/m2)
    double directNormal;			///< Input direct normal irradiance (W/m2)
    double diffuseHorizontal;		///< Input diffuse horizontal irradiance (W/m2)
    double weatherFilePOA;			///< Input plane-of-array irradiance (W/m2)
    double albedo;					///< Ground albedo (0-1)

    // Calculated Front-Side Irradiation components
    double calculatedDirectNormal;		///< Calculated direct normal irradiance (W/m2)
    double calculatedDiffuseHorizontal; ///< Calculated diffuse horizontal irradiance (W/m2)

    // Outputs
    double sunAnglesRadians[9];				///< Sun angles in radians calculated from solarpos()	
    double surfaceAnglesRadians[5];			///< Surface angles in radians calculated from incidence()
    double planeOfArrayIrradianceFront[3];	///< Front-side plane-of-array irradiance for beam, sky diffuse, ground diffuse (W/m2)
    double planeOfArrayIrradianceRear[3];	///< Rear-side plane-of-array irradiance for beam, sky diffuse, ground diffuse (W/m2)
    double diffuseIrradianceFront[3];		///< Front-side diffuse irradiance for isotropic, circumsolar, and horizon (W/m2)
    double diffuseIrradianceRear[3];		///< Rear-side diffuse irradiance for isotropic, circumsolar, and horizon (W/m2)
    int timeStepSunPosition[3];				///< [0] effective hour of day used for sun position, [1] effective minute of hour used for sun position, [2] is sun up?  (0=no, 1=midday, 2=sunup, 3=sundown)
    double planeOfArrayIrradianceRearAverage; ///< Average rear side plane-of-array irradiance (W/m2)

public:

    /// Directive to indicate that if delt_hr is less than zero, do not interpolate sunrise and sunset hours
#define IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET (-1.0)

    /// Maximum irradiance allowed (W/m2)
    static const int irradiationMax = 1500;

    static const int dut1 = 0; //Time correction for irregular Earth rotation (leap second); value between -1 and 1, no source for value so left at zero

    /// Default class constructor, calls setup()
    irrad(weather_record wr, weather_header wh,
        int skyModel, int radiationModeIn, int trackModeIn,
        bool useWeatherFileAlbedo, bool instantaneousWeather, bool backtrackingEnabled, bool forceToStowIn,
        double dtHour, double tiltDegrees, double azimuthDegrees, double trackerRotationLimitDegrees, double stowAngleDegreesIn,
        double groundCoverageRatio, std::vector<double> monthlyTiltDegrees, std::vector<double> userSpecifiedAlbedo,
        poaDecompReq* poaAllIn);

    /// Construct the irrad class with an Irradiance_IO() object and Subarray_IO() object
    irrad();

    /// Initialize irrad member data
    void setup();

    /// Validation method to verify member data is within acceptable ranges
    int check();

    /// Set the time for the irradiance processor
    void set_time(int year, int month, int day, int hour, double minute, double delt_hr);

    /// Set the location for the irradiance processor
    void set_location(double lat, double lon, double tz);

    // Set optional parameters for solarpos_spa calculation
    void set_optional(double elev = 0, double pres = 1013.25, double t_amb = 15);

    /// Set the sky model for the irradiance processor, using \link Irradiance_IO::SKYMODEL 
    void set_sky_model(int skymodel, double albedo);

    /// Set the surface orientation for the irradiance processor
    void set_surface(int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr, bool forceToStowFlag, double stowAngle);

    /// Set the direct normal and diffuse horizontal components of irradiation
    void set_beam_diffuse(double beam, double diffuse);

    /// Set the global horizontal and direct normal components of irradiation
    void set_global_beam(double global, double beam);

    /// Set the global horizontal and diffuse horizontal components of irradiation
    void set_global_diffuse(double global, double diffuse);

    /// Set the plane-of-array irradiance from a reference cell
    void set_poa_reference(double poa, poaDecompReq*);

    /// Set the plane-of-array irradiance from a pyronometer
    void set_poa_pyranometer(double poa, poaDecompReq*);

    /// Function to overwrite internally calculated sun position values, primarily to enable testing against other libraries using different sun position calculations
    void set_sun_component(size_t index, double value);

    /// Run the irradiance processor and calculate the plane-of-array irradiance and diffuse components of irradiance
    int calc();

    /// Run the irradiance processor for the rear-side of the surface to calculate rear-side plane-of-array irradiance
    int calc_rear_side(double transmissionFactor, double groundClearanceHeight, double slopeLength);

    /// Return the calculated sun angles, some of which are converted to degrees
    void get_sun(double* solazi,
        double* solzen,
        double* solelv,
        double* soldec,
        double* sunrise,
        double* sunset,
        int* sunup,
        double* eccfac,
        double* tst,
        double* hextra);

    /// Return one component of the sun angles calculated in radians
    double get_sun_component(size_t i) { return sunAnglesRadians[i]; }

    /// Return the calculated surface angles in degrees
    void get_angles(double* aoi,
        double* surftilt,
        double* surfazi,
        double* axisrot,
        double* btdiff);

    /// Return the front-side plane-of-array irradiance and diffuse components of irradiation
    void get_poa(double* beam, double* skydiff, double* gnddiff,
        double* isotrop, double* circum, double* horizon);

    /// Return the rear-side average total plane-of-array irradiance
    double get_poa_rear();

    /// Return the front-side irradiance components
    void get_irrad(double* ghi, double* dni, double* dhi);

    /// Return the effective hour and fraction used for the sun position calculation
    double get_sunpos_calc_hour();

    /// Return the albedo used by the irradiance processor
    double getAlbedo();

    /// Return the sky configuration factors, used by \link calc_rear_side()
    void getSkyConfigurationFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double>& rearSkyConfigFactors, std::vector<double>& frontSkyConfigFactors);

    /// Return the ground-shade factors, describing which segments of the ground are shaded by the array, used by \link calc_rear_side()
    void getGroundShadeFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, double solarAzimuthRadians, double solarElevationRadians, std::vector<int>& rearGroundFactors, std::vector<int>& frontGroundFactors, double& maxShadow, double& pvBackShadeFraction, double& pvFrontShadeFraction);

    /// Return the ground global-horizonal irradiance, used by \link calc_rear_side()
    void getGroundGHI(double transmissionFactor, std::vector<double> rearSkyConfigFactors, std::vector<double> frontSkyConfigFactors, std::vector<int> rearGroundShadeFactors, std::vector<int> frontGroundShadeFactors, std::vector<double>& rearGroundGHI, std::vector<double>& frontGroundGHI);

    /// Return the back surface irradiances, used by \link calc_rear_side()
    void getBackSurfaceIrradiances(double pvBackShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> rearGroundGHI, std::vector<double> frontGroundGHI, std::vector<double> frontReflected, std::vector<double>& rearIrradiance, double& rearAverageIrradiance);

    /// Return the front surface irradiances, used by \link calc_rear_side()
    void getFrontSurfaceIrradiances(double pvBackShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> frontGroundGHI, std::vector<double>& frontIrradiance, double& frontAverageIrradiance, std::vector<double>& frontReflected);

    enum RADMODE { DN_DF, DN_GH, GH_DF, POA_R, POA_P };
    enum SKYMODEL { ISOTROPIC, HDKR, PEREZ };
    enum TRACKING { FIXED_TILT, SINGLE_AXIS, TWO_AXIS, AZIMUTH_AXIS, SEASONAL_TILT };

};

// allow for the poa decomp model to take all daily POA measurements into consideration
struct poaDecompReq {
    poaDecompReq() : i(0), dayStart(0), stepSize(1), stepScale('h'), doy(-1) {}
    size_t i; // Current time index
    size_t dayStart; // time index corresponding to the start of the current day
    double stepSize;
    char stepScale; // indicates whether time steps are hours (h) or minutes (m)
    std::vector<double> POA; // Pointer to entire POA array (will have size 8760 if time step is 1 hour)
    std::vector<double> inc; // Pointer to angle of incident array (same size as POA)
    std::vector<double> tilt;
    std::vector<double> zen;
    std::vector<double> exTer;
    double tDew;
    int doy;
    double elev;
};

#endif
