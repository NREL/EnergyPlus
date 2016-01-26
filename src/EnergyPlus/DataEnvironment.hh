// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef DataEnvironment_hh_INCLUDED
#define DataEnvironment_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataEnvironment {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern Real64 const EarthRadius; // Radius of the Earth (m)
	extern Real64 const AtmosphericTempGradient; // Standard atmospheric air temperature gradient (K/m)
	extern Real64 const SunIsUpValue; // if Cos Zenith Angle of the sun is >= this value, the sun is "up"
	extern Real64 const StdPressureSeaLevel; // Standard barometric pressure at sea level (Pa)

	// DERIVED TYPE DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Real64 BeamSolarRad; // Current beam normal solar irradiance
	extern bool EMSBeamSolarRadOverrideOn; // EMS flag for beam normal solar irradiance
	extern Real64 EMSBeamSolarRadOverrideValue; // EMS override value for beam normal solar irradiance
	extern int DayOfMonth; // Current day of the month
	extern int DayOfMonthTomorrow; // Tomorrow's day of the month
	extern int DayOfWeek; // Current day of the week (Sunday=1, Monday=2, ...)
	extern int DayOfWeekTomorrow; // Tomorrow's day of the week (Sunday=1, Monday=2, ...)
	extern int DayOfYear; // Current day of the year (01JAN=1, 02JAN=2, ...)
	extern int DayOfYear_Schedule; // Schedule manager always assumes leap years...
	extern Real64 DifSolarRad; // Current sky diffuse solar horizontal irradiance
	extern bool EMSDifSolarRadOverrideOn; // EMS flag for sky diffuse solar horizontal irradiance
	extern Real64 EMSDifSolarRadOverrideValue; // EMS override value for sky diffuse solar horizontal irradiance
	extern int DSTIndicator; // Daylight Saving Time Indicator (1=yes, 0=no) for Today
	extern Real64 Elevation; // Elevation of this building site
	extern bool EndMonthFlag; // Set to true on last day of month
	extern Real64 GndReflectanceForDayltg; // Ground visible reflectance for use in daylighting calc
	extern Real64 GndReflectance; // Ground visible reflectance from input
	extern Real64 GndSolarRad; // Current ground reflected radiation
	extern Real64 GroundTemp; // Current ground temperature {C}
	extern Real64 GroundTempKelvin; // Current ground temperature {K}
	extern Real64 GroundTempFC; // Current ground temperature defined for F or C factor method {C}
	extern Real64 GroundTemp_Surface; // Current surface ground temperature {C}
	extern Real64 GroundTemp_Deep; // Current deep ground temperature
	extern int HolidayIndex; // Indicates whether current day is a holiday and if so what type
	// HolidayIndex=(0-no holiday, 1-holiday type 1, ...)
	extern int HolidayIndexTomorrow; // Tomorrow's Holiday Index
	extern bool IsRain; // Surfaces are wet for this time interval
	extern bool IsSnow; // Snow on the ground for this time interval
	extern Real64 Latitude; // Latitude of building location
	extern Real64 Longitude; // Longitude of building location
	extern int Month; // Current calendar month
	extern int MonthTomorrow; // Tomorrow's calendar month
	extern Real64 OutBaroPress; // Current outdoor air barometric pressure
	extern Real64 OutDryBulbTemp; // Current outdoor air dry bulb temperature
	extern bool EMSOutDryBulbOverrideOn; // EMS flag for outdoor air dry bulb temperature
	extern Real64 EMSOutDryBulbOverrideValue; // EMS override value for outdoor air dry bulb temperature
	extern Real64 OutHumRat; // Current outdoor air humidity ratio
	extern Real64 OutRelHum; // Current outdoor relative humidity [%]
	extern Real64 OutRelHumValue; // Current outdoor relative humidity value [0.0-1.0]
	extern bool EMSOutRelHumOverrideOn; // EMS flag for outdoor relative humidity value
	extern Real64 EMSOutRelHumOverrideValue; // EMS override value for outdoor relative humidity value
	extern Real64 OutEnthalpy; // Current outdoor enthalpy
	extern Real64 OutAirDensity; // Current outdoor air density
	extern Real64 OutWetBulbTemp; // Current outdoor air wet bulb temperature
	extern Real64 OutDewPointTemp; // Current outdoor dewpoint temperature
	extern bool EMSOutDewPointTempOverrideOn; // EMS flag for outdoor dewpoint temperature
	extern Real64 EMSOutDewPointTempOverrideValue; // EMS override value for outdoor dewpoint temperature
	extern Real64 SkyTemp; // Current sky temperature {C}
	extern Real64 SkyTempKelvin; // Current sky temperature {K}
	extern Real64 LiquidPrecipitation; // Current liquid precipitation amount (rain) {m}
	extern bool SunIsUp; // True when Sun is over horizon, False when not
	extern Real64 WindDir; // Current outdoor air wind direction
	extern bool EMSWindDirOverrideOn; // EMS flag for outdoor air wind direction
	extern Real64 EMSWindDirOverrideValue; // EMS override value for outdoor air wind direction
	extern Real64 WindSpeed; // Current outdoor air wind speed
	extern bool EMSWindSpeedOverrideOn; // EMS flag for outdoor air wind speed
	extern Real64 EMSWindSpeedOverrideValue; // EMS override value for outdoor air wind speed
	extern Real64 WaterMainsTemp; // Current water mains temperature
	extern int Year; // Current calendar year of the simulation
	extern int YearTomorrow; // Tomorrow's calendar year of the simulation
	extern Array1D< Real64 > SOLCOS; // Solar direction cosines at current time step
	extern Real64 CloudFraction; // Fraction of sky covered by clouds
	extern Real64 HISKF; // Exterior horizontal illuminance from sky (lux).
	extern Real64 HISUNF; // Exterior horizontal beam illuminance (lux)
	extern Real64 HISUNFnorm; // Exterior beam normal illuminance (lux)
	extern Real64 PDIRLW; // Luminous efficacy (lum/W) of beam solar radiation
	extern Real64 PDIFLW; // Luminous efficacy (lum/W) of sky diffuse solar radiation
	extern Real64 SkyClearness; // Sky clearness (see subr. DayltgLuminousEfficacy)
	extern Real64 SkyBrightness; // Sky brightness (see subr. DayltgLuminousEfficacy)
	extern Real64 StdBaroPress; // Standard "atmospheric pressure" based on elevation (ASHRAE HOF p6.1)
	extern Real64 StdRhoAir; // Standard "rho air" set in WeatherManager - based on StdBaroPress at elevation
	extern Real64 rhoAirSTP; // Standard density of dry air at 101325 Pa, 20.0C temperaure
	extern Real64 TimeZoneNumber; // Time Zone Number of building location
	extern Real64 TimeZoneMeridian; // Standard Meridian of TimeZone
	extern std::string EnvironmentName; // Current environment name (longer for weather file names)
	extern std::string WeatherFileLocationTitle; // Location Title from Weather File
	extern std::string CurMnDyHr; // Current Month/Day/Hour timestamp info
	extern std::string CurMnDy; // Current Month/Day timestamp info
	extern int CurEnvirNum; // current environment number
	extern int TotDesDays; // Total number of Design days to Setup
	extern int TotRunDesPersDays; // Total number of Run Design Periods [Days] (Weather data) to Setup
	extern int CurrentOverallSimDay; // Count of current simulation day in total of all sim days
	extern int TotalOverallSimDays; // Count of all possible simulation days in all environments
	extern int MaxNumberSimYears; // Maximum number of simulation years requested in all RunPeriod statements
	extern int RunPeriodStartDayOfWeek; // Day of week of the first day of the run period. (or design day - day of week)

	extern Real64 CosSolarDeclinAngle; // Cosine of the solar declination angle
	extern Real64 EquationOfTime; // Value of the equation of time formula
	extern Real64 SinLatitude; // Sine of Latitude
	extern Real64 CosLatitude; // Cosine of Latitude
	extern Real64 SinSolarDeclinAngle; // Sine of the solar declination angle
	extern Real64 TS1TimeOffset; // offset when TS=1 for solar calculations

	extern Real64 WeatherFileWindModCoeff; // =(WindBLHeight/WindSensorHeight)**WindExp for conditions at the weather station
	extern Real64 WeatherFileTempModCoeff; // =AtmosphericTempGradient*EarthRadius*SensorHeight/(EarthRadius+SensorHeight)

	extern Real64 SiteWindExp; // Exponent for the wind velocity profile at the site
	extern Real64 SiteWindBLHeight; // Boundary layer height for the wind velocity profile at the site (m)
	extern Real64 SiteTempGradient; // Air temperature gradient coefficient (K/m)

	extern bool GroundTempObjInput; // Ground temperature object input
	extern bool GroundTemp_SurfaceObjInput; // Surface ground temperature object input
	extern bool GroundTemp_DeepObjInput; // Deep ground temperature object input
	extern bool FCGroundTemps;
	extern bool DisplayWeatherMissingDataWarnings; // Display missing/out of range weather warnings
	extern bool IgnoreSolarRadiation; // TRUE if all solar radiation is to be ignored
	extern bool IgnoreBeamRadiation; // TRUE if beam (aka direct normal) radiation is to be ignored
	extern bool IgnoreDiffuseRadiation; // TRUE if diffuse horizontal radiation is to be ignored

	extern bool PrintEnvrnStampWarmup;
	extern bool PrintEnvrnStampWarmupPrinted;

	extern bool RunPeriodEnvironment; // True if Run Period, False if DesignDay
	extern std::string EnvironmentStartEnd; // Start/End dates for Environment
	extern bool CurrentYearIsLeapYear; // true when current year is leap year (convoluted logic dealing with
	// whether weather file allows leap years, runperiod inputs.

	// SUBROUTINE SPECIFICATIONS FOR MODULE DataEnvironment:
	//PUBLIC OutBaroPressAt
	//PUBLIC OutAirDensityAt

	// Functions

	// Clears the global data in DataEnvironment.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	Real64
	OutDryBulbTempAt( Real64 const Z ); // Height above ground (m)

	Real64
	OutWetBulbTempAt( Real64 const Z ); // Height above ground (m)

	Real64
	OutDewPointTempAt( Real64 const Z ); // Height above ground (m)

	Real64
	WindSpeedAt( Real64 const Z ); // Height above ground (m)

	Real64
	OutBaroPressAt( Real64 const Z ); // Height above ground (m)

	void
	SetOutBulbTempAt_error(
		std::string const & Settings,
		Real64 const max_height,
		std::string const & SettingsName
	);

} // DataEnvironment

} // EnergyPlus

#endif
