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

/**
*   solarpos function calculates the sun position given the local standard time and location.
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
void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9]);

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
void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, bool force_to_stow, double stow_angle_deg, double angle[5]);


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
void perez( double hextra, double dn,double df,double alb,double inc,double tilt,double zen, double poa[3], double diffc[3] /* can be NULL */ );

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
void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );

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
void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );


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
int poaDecomp( double wfPOA, double angle[], double sun[], double alb, poaDecompReq* pA, double &dn, double &df, double &gh, double poa[3], double diffc[3]);

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
double ModifiedDISC(const double g[3], const double z[3], double td, double alt, int doy, double &dn);

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
void ModifiedDISC(const double kt[3], const double kt1[3], const double g[3], const double z[3], double td, double alt, int doy, double &dn);

/**
* shadeFraction1x calculates if the system is shaded based on the sun position and surface position
*
* \param[in] solazi sun azimuth in radians, measured east from north, 0 to 2*pi
* \param[in] solzen sun zenith in radians, 0 to pi
* \param[in] tilt surface tilt angle from horizontal in radians
* \param[in] azimuth surface azimuth in degrees of collector
* \param[in] gcr grount coverage ratio of system
* \param[in] rotation tracking axis rotation angle in degrees
* \return fraction shaded (0-1) if system is shaded (0 for unshaded)
*/
double shadeFraction1x(double solazi, double solzen, double tilt, double azimuth, double gcr, double rotation);

/**
* backtrack finds the optimum angle to use to reduce self-shading on the front-side of modules using backtracking
*
* \param[in] solazi sun azimuth in radians, measured east from north, 0 to 2*pi
* \param[in] solzen sun zenith in radians, 0 to pi
* \param[in] tilt surface tilt angle from horizontal in radians
* \param[in] azimuth surface azimuth in degrees of collector
* \param[in] rotlim plus or minus rotation in degrees permitted by physical constraints of tracker, range is 0 to 180 degrees.
* \param[in] gcr grount coverage ratio of system
* \param[in] rotation tracking axis rotation angle in degrees
* \return updated rotation angle in degrees after backtracking
*/
double backtrack(double solazi, double solzen, double tilt, double azimuth, double rotlim, double gcr, double rotation);


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
	poaDecompReq * poaAll;			///< Data required to decompose input plane-of-array irradiance

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

	/// Default class constructor, calls setup()
	irrad(weather_record wr, weather_header wh,
		int skyModel, int radiationModeIn, int trackModeIn,
		bool useWeatherFileAlbedo, bool instantaneousWeather, bool backtrackingEnabled, bool forceToStowIn,
		double dtHour, double tiltDegrees, double azimuthDegrees, double trackerRotationLimitDegrees, double stowAngleDegreesIn, 
		double groundCoverageRatio, std::vector<double> monthlyTiltDegrees, std::vector<double> userSpecifiedAlbedo,
		poaDecompReq * poaAllIn);

	/// Construct the irrad class with an Irradiance_IO() object and Subarray_IO() object
	irrad();

	/// Initialize irrad member data
	void setup();

	/// Validation method to verify member data is within acceptable ranges
	int check();
	
	/// Set the time for the irradiance processor
	void set_time( int year, int month, int day, int hour, double minute, double delt_hr );

	/// Set the location for the irradiance processor
	void set_location( double lat, double lon, double tz);

	/// Set the sky model for the irradiance processor, using \link Irradiance_IO::SKYMODEL 
	void set_sky_model( int skymodel, double albedo );

	/// Set the surface orientation for the irradiance processor
	void set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr, bool forceToStowFlag, double stowAngle);

	/// Set the direct normal and diffuse horizontal components of irradiation
	void set_beam_diffuse( double beam, double diffuse );

	/// Set the global horizontal and direct normal components of irradiation
	void set_global_beam( double global, double beam );

	/// Set the global horizontal and diffuse horizontal components of irradiation
	void set_global_diffuse(double global, double diffuse);

	/// Set the plane-of-array irradiance from a reference cell
	void set_poa_reference( double poa, poaDecompReq* );

	/// Set the plane-of-array irradiance from a pyronometer
	void set_poa_pyranometer( double poa, poaDecompReq* );

	/// Function to overwrite internally calculated sun position values, primarily to enable testing against other libraries using different sun position calculations
	void set_sun_component(size_t index, double value);

	/// Run the irradiance processor and calculate the plane-of-array irradiance and diffuse components of irradiance
	int calc();

	/// Run the irradiance processor for the rear-side of the surface to calculate rear-side plane-of-array irradiance
	int calc_rear_side(double transmissionFactor, double groundClearanceHeight, double slopeLength);
	
	/// Return the calculated sun angles, some of which are converted to degrees
	void get_sun( double *solazi,
		double *solzen,
		double *solelv,
		double *soldec,
		double *sunrise,
		double *sunset,
		int *sunup,
		double *eccfac,
		double *tst,
		double *hextra );

	/// Return one component of the sun angles calculated in radians
	double get_sun_component(size_t i) { return sunAnglesRadians[i]; }

	/// Return the calculated surface angles in degrees
	void get_angles( double *aoi,
		double *surftilt,
		double *surfazi,
		double *axisrot,
		double *btdiff );

	/// Return the front-side plane-of-array irradiance and diffuse components of irradiation
	void get_poa( double *beam, double *skydiff, double *gnddiff,
		double *isotrop, double *circum, double *horizon );

	/// Return the rear-side average total plane-of-array irradiance
	double get_poa_rear();

	/// Return the front-side irradiance components
	void get_irrad (double *ghi, double *dni, double *dhi);

	/// Return the effective hour and fraction used for the sun position calculation
	double get_sunpos_calc_hour();

	/// Return the albedo used by the irradiance processor
	double getAlbedo();

	/// Return the sky configuration factors, used by \link calc_rear_side()
	void getSkyConfigurationFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> & rearSkyConfigFactors, std::vector<double> & frontSkyConfigFactors);

	/// Return the ground-shade factors, describing which segments of the ground are shaded by the array, used by \link calc_rear_side()
	void getGroundShadeFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, double solarAzimuthRadians, double solarElevationRadians, std::vector<int> & rearGroundFactors, std::vector<int> & frontGroundFactors, double & maxShadow, double & pvBackShadeFraction, double & pvFrontShadeFraction);

	/// Return the ground global-horizonal irradiance, used by \link calc_rear_side()
	void getGroundGHI(double transmissionFactor, std::vector<double> rearSkyConfigFactors, std::vector<double> frontSkyConfigFactors, std::vector<int> rearGroundShadeFactors, std::vector<int> frontGroundShadeFactors, std::vector<double> & rearGroundGHI, std::vector<double> & frontGroundGHI);

	/// Return the back surface irradiances, used by \link calc_rear_side()
	void getBackSurfaceIrradiances(double pvBackShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> rearGroundGHI, std::vector<double> frontGroundGHI, std::vector<double> frontReflected, std::vector<double> & rearIrradiance, double & rearAverageIrradiance);

	/// Return the front surface irradiances, used by \link calc_rear_side()
	void getFrontSurfaceIrradiances(double pvBackShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> frontGroundGHI, std::vector<double> & frontIrradiance, double & frontAverageIrradiance, std::vector<double> & frontReflected);

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
