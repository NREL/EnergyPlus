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

// C++ Headers
#include <cmath>
#include <cstdio>
#include <string>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Time_Date.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <WeatherManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataStringGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataReportingFlags.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <ThermalComfort.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace WeatherManager {

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   May 1997
	//       MODIFIED       December 1998, FW; December 1999, LKL.
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contains all of the weather handling routines for
	// EnergyPlus.  That includes getting user input, defining design day
	// weather, retrieving data from weather files, and supplying the
	// outdoor environment for each time step.

	// METHODOLOGY EMPLOYED:
	// Setting up the design days is similar to BLAST/IBLAST.  Reading the
	// BLAST weather files is similar to that code in BLAST/IBLAST.  The EnergyPlus
	// Weather file (EPW) is new code.

	// REFERENCES:
	// (I)BLAST legacy code, internal Reverse Engineering documentation,
	// and internal Evolutionary Engineering documentation.

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace GroundTemperatureManager;
	using namespace DataReportingFlags;
	using DataSystemVariables::iASCII_CR;
	using DataSystemVariables::iUnicode_end;

	using General::ProcessDateString; // , ValidateMonthDay
	using General::RoundSigDigits;
	using namespace Psychrometrics;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Following are Date Types read in from EPW file or IDF
	int const InvalidDate( -1 );
	int const MonthDay( 1 );
	int const NthDayInMonth( 2 );
	int const LastDayInMonth( 3 );

	int const ScheduleMethod( 1 ); // Constant for water mains temperatures calculation methods
	int const CorrelationMethod( 2 ); // Constant for water mains temperatures calculation methods

	int const InvalidWeatherFile( 0 );
	int const EPlusWeatherFile( 1 );

	int const ASHRAE_ClearSky( 0 ); // Design Day solar model ASHRAE ClearSky (default)
	int const Zhang_Huang( 1 ); // Design Day solar model Zhang Huang
	int const SolarModel_Schedule( 2 ); // Design Day solar model (beam and diffuse) from user entered schedule
	int const ASHRAE_Tau( 3 ); // Design Day solar model ASHRAE tau (per 2009 HOF)

	int const DDHumIndType_WetBulb( 0 ); // Design Day Humidity Indicating Type = Wetbulb (default)
	int const DDHumIndType_DewPoint( 1 ); // Design Day Humidity Indicating Type = Dewpoint
	int const DDHumIndType_Enthalpy( 2 ); // Design Day Humidity Indicating Type = Enthalpy
	int const DDHumIndType_HumRatio( 3 ); // Design Day Humidity Indicating Type = Humidity Ratio
	int const DDHumIndType_RelHumSch( 4 ); // Design Day Humidity Indicating Type = relhum schedule
	int const DDHumIndType_WBProfDef( 5 ); // Design Day Humidity Indicating Type = Wetbulb default profile
	int const DDHumIndType_WBProfDif( 6 ); // Design Day Humidity Indicating Type = Wetbulb difference profile
	int const DDHumIndType_WBProfMul( 7 ); // Design Day Humidity Indicating Type = Wetbulb multiplier profile
	int const DDHumIndType_Count( 8 ); // # of DDHumIndTypes

	int const DDDBRangeType_Default( 0 ); // Design Day DryBulb Range Type = Default Multipliers
	int const DDDBRangeType_Multiplier( 1 ); // Design Day DryBulb Range Type = Multiplier Schedule
	int const DDDBRangeType_Difference( 2 ); // Design Day DryBulb Range Type = Difference Schedule
	int const DDDBRangeType_Profile( 3 ); // Design Day DryBulb Range Type = Temperature Profile

	int const WP_ScheduleValue( 1 ); // User entered Schedule value for Weather Property
	int const WP_DryBulbDelta( 2 ); // User entered DryBulb difference Schedule value for Weather Property
	int const WP_DewPointDelta( 3 ); // User entered Dewpoint difference Schedule value for Weather Property
	int const WP_SkyTAlgorithmA( 4 ); // place holder

	int const GregorianToJulian( 1 ); // JGDate argument for Gregorian to Julian Date conversion
	int const JulianToGregorian( 2 ); // JGDate argument for Julian to Gregorian Date conversion

	Real64 const Sigma( 5.6697e-8 ); // Stefan-Boltzmann constant
	Real64 const TKelvin( KelvinConv ); // conversion from Kelvin to Celsius

	static std::string const BlankString;
	Array1D_string const DaysOfWeek( 7, { "SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY" } );

	bool Debugout( false );

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	int YearofSim( 1 ); // The Present year of Simulation.
	int const NumDaysInYear( 365 );
	int EnvironmentReportNbr( 0 ); // Report number for the environment stamp
	std::string EnvironmentReportChr; // Report number for the environment stamp (character -- for printing)
	int TimeStampReportNbr( 0 ); // Report number for the time stamp
	std::string TimeStampReportChr; // Report number for the time stamp (character -- for printing)
	int WeatherDataReport( 0 ); // Report number for the weather data
	bool WeatherFileExists( false ); // Set to true if a weather file exists
	std::string LocationTitle; // Location Title from input File
	bool LocationGathered( false ); // flag to show if Location exists on Input File (we assume one is there and
	// correct on weather file)
	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool GetBranchInputOneTimeFlag( true );
		bool GetEnvironmentFirstCall( true );
		bool PrntEnvHeaders( true );
	}
	Real64 WeatherFileLatitude( 0.0 );
	Real64 WeatherFileLongitude( 0.0 );
	Real64 WeatherFileTimeZone( 0.0 );
	Real64 WeatherFileElevation( 0.0 );
	int WeatherFileUnitNumber; // File unit number for the weather file
	Array1D< Real64 > GroundTempsFCFromEPWHeader( 12, 0.0 ); // F or C factor method
	Array1D< Real64 > GroundReflectances( 12, 0.2 ); // User Specified Ground Reflectances !EPTeam: Using DP causes big diffs
	Real64 SnowGndRefModifier( 1.0 ); // Modifier to ground reflectance during snow
	Real64 SnowGndRefModifierForDayltg( 1.0 ); // Modifier to ground reflectance during snow for daylighting
	int WaterMainsTempsMethod( 0 ); // Water mains temperature calculation method
	int WaterMainsTempsSchedule( 0 ); // Water mains temperature schedule
	Real64 WaterMainsTempsAnnualAvgAirTemp( 0.0 ); // Annual average outdoor air temperature (C)
	Real64 WaterMainsTempsMaxDiffAirTemp( 0.0 ); // Maximum difference in monthly average outdoor air temperatures (deltaC)
	bool wthFCGroundTemps( false );
	Real64 RainAmount( 0.0 );
	Real64 SnowAmount( 0.0 );

	int TotRunPers( 0 ); // Total number of Run Periods (Weather data) to Setup
	int TotRunDesPers( 0 ); // Total number of Run Design Periods (Weather data) to Setup

	int NumSpecialDays( 0 );
	Array1D_int SpecialDayTypes( 366, 0 ); // To hold holiday types given in input file
	Array1D_int WeekDayTypes( 366, 0 ); // To hold Week day types using specified first day
	Array1D_int DSTIndex( 366, 0 ); // To hold DST Index based on weather file or input

	int NumDataPeriods( 0 );

	int NumIntervalsPerHour( 1 );

	bool UseDaylightSaving( true ); // True if user says to use Weather File specified DaylightSaving Period
	bool UseSpecialDays( true ); // True if user says to use Weather File specified Special Days for current RunPeriod
	bool UseRainValues( true ); // True if rain values from weather file are to be used
	bool UseSnowValues( true ); // True if snow values from weather file are to be used
	bool EPWDaylightSaving( false ); // True if a DaylightSaving Time Period is input (EPW files)
	bool IDFDaylightSaving( false ); // True if a DaylightSaving Time Period is input (IDF files)
	bool DaylightSavingIsActive( false ); // True if a DaylightSavingPeriod should be used for Environment
	bool WFAllowsLeapYears( false ); // True if the Weather File (WF) header has "Yes" for Leap Years
	int WFLeapYearInd( 0 ); // Indicator for current Weather file "Leap Year", used in DayOfYear calculations and others.
	int curSimDayForEndOfRunPeriod( 0 ); // normal=number days in sim, but different when repeating runperiods or multi-year files
	int Envrn( 0 ); // Counter for environments
	int NumOfEnvrn( 0 ); // Number of environments to be simulated
	int NumEPWTypExtSets( 0 ); // Number of Typical/Extreme on weather file.
	int NumWPSkyTemperatures( 0 ); // Number of WeatherProperty:SkyTemperature items in input file

	Array2D_bool TodayIsRain; // Rain indicator, true=rain
	Array2D_bool TodayIsSnow; // Snow indicator, true=snow
	Array2D< Real64 > TodayRainAmount; // ficitious indicator of Rain
	Array2D< Real64 > TodaySnowAmount; // ficitious indicator of Snow
	Array2D< Real64 > TodayOutDryBulbTemp; // Dry bulb temperature of outside air
	Array2D< Real64 > TodayOutWetBulbTemp; // Wet bulb temperature of outside air
	Array2D< Real64 > TodayOutDewPointTemp; // Dew Point Temperature of outside air
	Array2D< Real64 > TodayOutBaroPress; // Barometric pressure of outside air
	Array2D< Real64 > TodayOutHumRat; // Humidity ratio of outside air
	Array2D< Real64 > TodayOutRelHum; // Relative Humidity of outside air
	Array2D< Real64 > TodayWindSpeed; // Wind speed of outside air
	Array2D< Real64 > TodayWindDir; // Wind direction of outside air
	Array2D< Real64 > TodaySkyTemp; // Sky temperature
	Array2D< Real64 > TodayHorizIRSky; // Horizontal IR from Sky
	Array2D< Real64 > TodayBeamSolarRad; // Direct normal solar irradiance
	Array2D< Real64 > TodayDifSolarRad; // Sky diffuse horizontal solar irradiance
	Array2D< Real64 > TodayAlbedo; // Albedo
	Array2D< Real64 > TodayLiquidPrecip; // Liquid Precipitation Depth (mm)

	Array2D_bool TomorrowIsRain; // Rain indicator, true=rain
	Array2D_bool TomorrowIsSnow; // Snow indicator, true=snow
	Array2D< Real64 > TomorrowRainAmount; // ficitious indicator of Rain
	Array2D< Real64 > TomorrowSnowAmount; // ficitious indicator of Snow
	Array2D< Real64 > TomorrowOutDryBulbTemp; // Dry bulb temperature of outside air
	Array2D< Real64 > TomorrowOutDewPointTemp; // Dew Point Temperature of outside air
	Array2D< Real64 > TomorrowOutBaroPress; // Barometric pressure of outside air
	Array2D< Real64 > TomorrowOutRelHum; // Relative Humidity of outside air
	Array2D< Real64 > TomorrowWindSpeed; // Wind speed of outside air
	Array2D< Real64 > TomorrowWindDir; // Wind direction of outside air
	Array2D< Real64 > TomorrowSkyTemp; // Sky temperature
	Array2D< Real64 > TomorrowHorizIRSky; // Horizontal IR from Sky
	Array2D< Real64 > TomorrowBeamSolarRad; // Direct normal solar irradiance
	Array2D< Real64 > TomorrowDifSolarRad; // Sky diffuse horizontal solar irradiance
	Array2D< Real64 > TomorrowAlbedo; // Albedo
	Array2D< Real64 > TomorrowLiquidPrecip; // Liquid Precipitation Depth

	Array3D< Real64 > DDDBRngModifier; // Design Day Dry-bulb Temperature Range Modifier
	Array3D< Real64 > DDHumIndModifier; // Design Day relative humidity values
	//   or wet-bulb modifiers (per HumIndType)
	Array3D< Real64 > DDBeamSolarValues; // Design Day Beam Solar Values
	Array3D< Real64 > DDDiffuseSolarValues; // Design Day Relative Humidity Values

	Array3D< Real64 > DDSkyTempScheduleValues; // Sky temperature - DesignDay input

	int RptIsRain( 0 ); // Rain Report Value
	int RptIsSnow( 0 ); // Snow Report Value
	int RptDayType( 0 ); // DayType Report Value

	Real64 HrAngle( 0.0 ); // Current Hour Angle
	Real64 SolarAltitudeAngle( 0.0 ); // Angle of Solar Altitude (degrees)
	Real64 SolarAzimuthAngle( 0.0 ); // Angle of Solar Azimuth (degrees)
	Real64 HorizIRSky( 0.0 ); // Horizontal Infrared Radiation Intensity (W/m2)
	Real64 TimeStepFraction( 0.0 ); // Fraction of hour each time step represents
	Array1D< Real64 > SPSiteDryBulbRangeModScheduleValue; // reporting Drybulb Temperature Range Modifier Schedule Value
	Array1D< Real64 > SPSiteHumidityConditionScheduleValue; // reporting Humidity Condition Schedule Value
	Array1D< Real64 > SPSiteBeamSolarScheduleValue; // reporting Beam Solar Schedule Value
	Array1D< Real64 > SPSiteDiffuseSolarScheduleValue; // reporting Diffuse Solar Schedule Value
	Array1D< Real64 > SPSiteSkyTemperatureScheduleValue; // reporting SkyTemperature Modifier Schedule Value
	Array1D_int SPSiteScheduleNamePtr; // SP Site Schedule Name Ptrs
	Array1D_string SPSiteScheduleUnits; // SP Site Schedule Units
	int NumSPSiteScheduleNamePtrs( 0 ); // Number of SP Site Schedules (DesignDay only)
	int NumMissing( 0 ); // Number of hours of missing data
	Array1D< Real64 > Interpolation; // Interpolation values based on Number of Time Steps in Hour
	Array1D< Real64 > SolarInterpolation; // Solar Interpolation values based on
	//      Number of Time Steps in Hour
	Array1D_int EndDayOfMonth( 12, { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 } );
	bool ErrorInWeatherFile( false ); // Set to TRUE when there is a problem with dates
	int LeapYearAdd( 0 ); // Set during environment if leap year is active (adds 1 to number days in Feb)
	bool DatesShouldBeReset( false ); // True when weekdays should be reset
	bool StartDatesCycleShouldBeReset( false ); // True when start dates on repeat should be reset
	bool Jan1DatesShouldBeReset( false ); // True if Jan 1 should signal reset of dates
	bool RPReadAllWeatherData( false ); // True if need to read all weather data prior to simulation

	// SUBROUTINE SPECIFICATIONS FOR MODULE WeatherManager
	//PUBLIC  ProcessDateString
	// Get Input from Input File

	// Object Data
	DayWeatherVariables TodayVariables; // Today's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year for weather data | Year of weather data | Month of weather data | Day of month for weather data | Day of week for weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday indicator (0=no holiday, non-zero=holiday type) | Sine of the solar declination angle | Cosine of the solar declination angle | Value of the equation of time formula
	DayWeatherVariables TomorrowVariables; // Tomorrow's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year for weather data | Year of weather data | Month of weather data | Day of month for weather data | Day of week for weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday indicator (0=no holiday, non-zero=holiday type) | Sine of the solar declination angle | Cosine of the solar declination angle | Value of the equation of time formula
	Array1D< DayWeatherVariables > DesignDay; // Design day environments
	MissingData Missing; // Dry Bulb Temperature (C) | Dew Point Temperature (C) | Relative Humidity (%) | Atmospheric Pressure (Pa) | Wind Direction (deg) | Wind Speed/Velocity (m/s) | Total Sky Cover (tenths) | Opaque Sky Cover (tenths) | Visibility (km) | Ceiling Height (m) | Precipitable Water (mm) | Aerosol Optical Depth | Snow Depth (cm) | Number of Days since last snow | Albedo | Rain/Liquid Precipitation (mm)
	MissingDataCounts Missed;
	RangeDataCounts OutOfRange;
	Array1D< DesignDayData > DesDayInput; // Design day Input Data
	Array1D< EnvironmentData > Environment; // Environment data
	Array1D< RunPeriodData > RunPeriodInput;
	Array1D< RunPeriodData > RunPeriodDesignInput;
	Array1D< TypicalExtremeData > TypicalExtremePeriods;
	DaylightSavingPeriodData EPWDST; // Daylight Saving Period Data from EPW file
	DaylightSavingPeriodData IDFDST; // Daylight Saving Period Data from IDF file
	DaylightSavingPeriodData DST; // Daylight Saving Period Data, if active
	Array1D< WeatherProperties > WPSkyTemperature;
	Array1D< SpecialDayData > SpecialDays;
	Array1D< DataPeriodData > DataPeriods;

	std::shared_ptr< BaseGroundTempsModel > siteShallowGroundTempsPtr;
	std::shared_ptr< BaseGroundTempsModel > siteBuildingSurfaceGroundTempsPtr;
	std::shared_ptr< BaseGroundTempsModel > siteFCFactorMethodGroundTempsPtr;
	std::shared_ptr< BaseGroundTempsModel > siteDeepGroundTempsPtr;

	static gio::Fmt fmtA( "(A)" );
	static gio::Fmt fmtAN( "(A,$)" );

	// MODULE SUBROUTINES:

	// Functions
	void
	clear_state()
	{
		Debugout = false ;
		YearofSim = 1 ; // The Present year of Simulation.
		EnvironmentReportNbr = 0 ; // Report number for the environment stamp
		EnvironmentReportChr = ""; // Report number for the environment stamp (character -- for printing)
		TimeStampReportNbr = 0; // Report number for the time stamp
		TimeStampReportChr = ""; // Report number for the time stamp (character -- for printing)
		WeatherDataReport = 0 ; // Report number for the weather data
		WeatherFileExists = false ; // Set to true if a weather file exists
		LocationTitle = ""; // Location Title from input File
		LocationGathered = false; // flag to show if Location exists on Input File (we assume one is

		GetBranchInputOneTimeFlag = true ;
		GetEnvironmentFirstCall = true ;
		PrntEnvHeaders = true ;
		WeatherFileLatitude = 0.0 ;
		WeatherFileLongitude = 0.0 ;
		WeatherFileTimeZone = 0.0 ;
		WeatherFileElevation = 0.0 ;
		WeatherFileUnitNumber = 0 ; // File unit number for the weather file
		siteShallowGroundTempsPtr.reset();
		siteBuildingSurfaceGroundTempsPtr.reset();
		siteFCFactorMethodGroundTempsPtr.reset();
		siteDeepGroundTempsPtr.reset();
		GroundTempsFCFromEPWHeader = Array1D< Real64 > ( 12, 0.0 );
		GroundReflectances	= Array1D< Real64 >( 12, 0.2 );

		SnowGndRefModifier = 1.0 ; // Modifier to ground reflectance during snow
		SnowGndRefModifierForDayltg = 1.0 ; // Modifier to ground reflectance during snow for daylighting
		WaterMainsTempsMethod = 0 ; // Water mains temperature calculation method
		WaterMainsTempsSchedule = 0 ; // Water mains temperature schedule
		WaterMainsTempsAnnualAvgAirTemp = 0.0 ; // Annual average outdoor air temperature (C)
		WaterMainsTempsMaxDiffAirTemp = 0.0 ; // Maximum difference in monthly average outdoor air temperatures (deltaC)
		wthFCGroundTemps = false;
		RainAmount =  0.0 ;
		SnowAmount = 0.0 ;
		TotRunPers =  0 ; // Total number of Run Periods (Weather data) to Setup
		TotRunDesPers = 0 ; // Total number of Run Design Periods (Weather data) to Setup
		NumSpecialDays = 0 ;

		SpecialDayTypes	= Array1D< int >(366, 0 );
		WeekDayTypes	= Array1D< int >(366, 0 );
		DSTIndex		= Array1D< int >(366, 0 );

		NumDataPeriods = 0;
		NumIntervalsPerHour = 1;
		UseDaylightSaving = true ; // True if user says to use Weather File specified DaylightSaving Period
		UseSpecialDays = true; // True if user says to use Weather File specified Special Days for current RunPeriod
		UseRainValues = true ; // True if rain values from weather file are to be used
		UseSnowValues = true ; // True if snow values from weather file are to be used
		EPWDaylightSaving = false ; // True if a DaylightSaving Time Period is input (EPW files)
		IDFDaylightSaving = false ; // True if a DaylightSaving Time Period is input (IDF files)
		DaylightSavingIsActive = false ; // True if a DaylightSavingPeriod should be used for Environment
		WFAllowsLeapYears = false; // True if the Weather File (WF) header has "Yes" for Leap Years
		WFLeapYearInd =  0 ; // Indicator for current Weather file "Leap Year", used in DayOfYear calculations and others.
		curSimDayForEndOfRunPeriod = 0 ; // normal=number days in sim, but different when repeating runperiods or multi-year files
		Envrn = 0 ; // Counter for environments
		NumOfEnvrn = 0 ; // Number of environments to be simulated
		NumEPWTypExtSets = 0 ; // Number of Typical/Extreme on weather file.
		NumWPSkyTemperatures = 0 ; // Number of WeatherProperty:SkyTemperature items in input file
		TodayIsRain.deallocate(); // Rain indicator, true=rain
		TodayIsSnow.deallocate(); // Snow indicator, true=snow
		TodayRainAmount.deallocate(); // ficitious indicator of Rain
		TodaySnowAmount.deallocate(); // ficitious indicator of Snow
		TodayOutDryBulbTemp.deallocate(); // Dry bulb temperature of outside air
		TodayOutWetBulbTemp.deallocate(); // Wet bulb temperature of outside air
		TodayOutDewPointTemp.deallocate(); // Dew Point Temperature of outside air
		TodayOutBaroPress.deallocate(); // Barometric pressure of outside air
		TodayOutHumRat.deallocate(); // Humidity ratio of outside air
		TodayOutRelHum.deallocate(); // Relative Humidity of outside air
		TodayWindSpeed.deallocate(); // Wind speed of outside air
		TodayWindDir.deallocate(); // Wind direction of outside air
		TodaySkyTemp.deallocate(); // Sky temperature
		TodayHorizIRSky.deallocate(); // Horizontal IR from Sky
		TodayBeamSolarRad.deallocate(); // Direct normal solar irradiance
		TodayDifSolarRad.deallocate(); // Sky diffuse horizontal solar irradiance
		TodayAlbedo.deallocate(); // Albedo
		TodayLiquidPrecip.deallocate(); // Liquid Precipitation Depth (mm)
		TomorrowIsRain.deallocate(); // Rain indicator, true=rain
		TomorrowIsSnow.deallocate(); // Snow indicator, true=snow
		TomorrowRainAmount.deallocate(); // ficitious indicator of Rain
		TomorrowSnowAmount.deallocate(); // ficitious indicator of Snow
		TomorrowOutDryBulbTemp.deallocate(); // Dry bulb temperature of outside air
		TomorrowOutDewPointTemp.deallocate(); // Dew Point Temperature of outside air
		TomorrowOutBaroPress.deallocate(); // Barometric pressure of outside air
		TomorrowOutRelHum.deallocate(); // Relative Humidity of outside air
		TomorrowWindSpeed.deallocate(); // Wind speed of outside air
		TomorrowWindDir.deallocate(); // Wind direction of outside air
		TomorrowSkyTemp.deallocate(); // Sky temperature
		TomorrowHorizIRSky.deallocate(); // Horizontal IR from Sky
		TomorrowBeamSolarRad.deallocate(); // Direct normal solar irradiance
		TomorrowDifSolarRad.deallocate(); // Sky diffuse horizontal solar irradiance
		TomorrowAlbedo.deallocate(); // Albedo
		TomorrowLiquidPrecip.deallocate(); // Liquid Precipitation Depth
		DDDBRngModifier.deallocate(); // Design Day Dry-bulb Temperature Range Modifier
		DDHumIndModifier.deallocate(); // Design Day relative humidity values
		DDBeamSolarValues.deallocate(); // Design Day Beam Solar Values
		DDDiffuseSolarValues.deallocate(); // Design Day Relative Humidity Values
		DDSkyTempScheduleValues.deallocate(); // Sky temperature - DesignDay input
		RptIsRain = 0 ; // Rain Report Value
		RptIsSnow = 0 ; // Snow Report Value
		RptDayType = 0 ; // DayType Report Value

		HrAngle =  0.0 ; // Current Hour Angle
		SolarAltitudeAngle = 0.0 ; // Angle of Solar Altitude (degrees)
		SolarAzimuthAngle = 0.0 ; // Angle of Solar Azimuth (degrees)
		HorizIRSky = 0.0 ; // Horizontal Infrared Radiation Intensity (W/m2)
		TimeStepFraction = 0.0 ; // Fraction of hour each time step represents
		SPSiteDryBulbRangeModScheduleValue.deallocate(); // reporting Drybulb Temperature Range Modifier Schedule Value
		SPSiteHumidityConditionScheduleValue.deallocate(); // reporting Humidity Condition Schedule Value
		SPSiteBeamSolarScheduleValue.deallocate(); // reporting Beam Solar Schedule Value
		SPSiteDiffuseSolarScheduleValue.deallocate(); // reporting Diffuse Solar Schedule Value
		SPSiteSkyTemperatureScheduleValue.deallocate(); // reporting SkyTemperature Modifier Schedule Value
		SPSiteScheduleNamePtr.deallocate(); // SP Site Schedule Name Ptrs
		SPSiteScheduleUnits.deallocate(); // SP Site Schedule Units
		NumSPSiteScheduleNamePtrs = 0 ; // Number of SP Site Schedules (DesignDay only)
		NumMissing = 0 ; // Number of hours of missing data
		Interpolation.deallocate(); // Interpolation values based on Number of Time Steps in Hour
		SolarInterpolation.deallocate(); // Solar Interpolation values based on

		ErrorInWeatherFile = false ; // Set to TRUE when there is a problem with dates
		LeapYearAdd = 0 ;
		DatesShouldBeReset = false;
		StartDatesCycleShouldBeReset = false; // True when start dates on repeat should be reset
		Jan1DatesShouldBeReset = false; // True if Jan 1 should signal reset of dates
		TodayVariables = DayWeatherVariables();
		TomorrowVariables = DayWeatherVariables();
		DesignDay.deallocate();
		Missing = MissingData();
		Missed = MissingDataCounts();
		OutOfRange = RangeDataCounts();
		DesDayInput.deallocate(); // Design day Input Data
		Environment.deallocate(); // Environment data
		RunPeriodInput.deallocate();
		RunPeriodDesignInput.deallocate();
		TypicalExtremePeriods.deallocate();

		EPWDST.StDateType = 0 ;
		EPWDST.StWeekDay = 0 ;
		EPWDST.StMon = 0 ;
		EPWDST.StDay = 0 ;
		EPWDST.EnDateType = 0 ;
		EPWDST.EnMon = 0 ;
		EPWDST.EnDay = 0 ;
		EPWDST.EnWeekDay = 0 ;

		IDFDST.StDateType = 0 ;
		IDFDST.StWeekDay = 0 ;
		IDFDST.StMon = 0 ;
		IDFDST.StDay = 0 ;
		IDFDST.EnDateType = 0 ;
		IDFDST.EnMon = 0 ;
		IDFDST.EnDay = 0 ;
		IDFDST.EnWeekDay = 0 ;

		DST.StDateType = 0 ;
		DST.StWeekDay = 0 ;
		DST.StMon = 0 ;
		DST.StDay = 0 ;
		DST.EnDateType = 0 ;
		DST.EnMon = 0 ;
		DST.EnDay = 0 ;
		DST.EnWeekDay = 0 ;
		WPSkyTemperature.deallocate();
		SpecialDays.deallocate();
		DataPeriods.deallocate();

	} //clear_state, for unit tests

	void
	ManageWeather()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 1997
		//       MODIFIED       June 1997 (general clean-up)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver of the weather manager module.
		// It controls the assignment of weather related global variables as
		// well as the reads and writes for weather information.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus "manager" methodology.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static bool PrintEnvrnStamp( false ); // Set to true when the environment header should be printed

		// FLOW:

		InitializeWeather( PrintEnvrnStamp );

		SetCurrentWeather();

		ReportWeatherAndTimeInformation( PrintEnvrnStamp );

	}

	void
	ResetEnvironmentCounter()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine provides an easy method to assure that the environment
		// counter (used by GetNextEnvironment) is reset before SetupSimulation or
		// Simulating.  May not be necessary, but just in case.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		Envrn = 0;

	}

	void
	GetNextEnvironment(
		bool & Available, // true if there is another environment, false if the end
		bool & ErrorsFound // will be set to true if severe errors are found in inputs
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is called from the outer simulation manager and determines
		// if another environment is available in the "run list" or if the end has been
		// reached.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::InvJulianDay;
		using General::JulianDay;
		using General::BetweenDates;
		using namespace DataSystemVariables;

		using DataHeatBalance::AdaptiveComfortRequested_ASH55;
		using DataHeatBalance::AdaptiveComfortRequested_CEN15251;
		using ThermalComfort::CalcThermalComfortAdaptiveASH55;
		using ThermalComfort::CalcThermalComfortAdaptiveCEN15251;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetNextEnvironment: " );
		static gio::Fmt EnvironFormat( "('! <Environment>,Environment Name,Environment Type, Start Date, End Date,',    ' Start DayOfWeek, Duration {#days}, Source:Start DayOfWeek, ',        ' Use Daylight Saving, Use Holidays, Apply Weekend Holiday Rule, ',    ' Use Rain Values, Use Snow Values',/,                                 '! <Environment:Special Days>, Special Day Name, Special Day Type, Source, ',  'Start Date, Duration {#days}',/,                                      '! <Environment:Daylight Saving>, Daylight Saving Indicator, Source,',           ' Start Date, End Date',/,                                           '! <Environment:WarmupDays>, NumberofWarmupDays')" );
		static gio::Fmt EnvNameFormat( "('Environment',12(',',A))" );
		static gio::Fmt EnvDSTNFormat( "('Environment:Daylight Saving,No,',A)" );
		static gio::Fmt EnvDSTYFormat( "('Environment:Daylight Saving,Yes',3(',',A))" );
		static gio::Fmt EnvSpDyFormat( "('Environment:Special Days',4(',',A),',',I3)" );
		static gio::Fmt DateFormat( "(I2.2,'/',I2.2)" );
		static gio::Fmt DateFormatwithYear( "(I2.2,'/',I2.2,'/',I4.4)" );
		static Array1D_string const SpecialDayNames( 5, { "Holiday", "SummerDesignDay", "WinterDesignDay", "CustomDay1", "CustomDay2" } );
		static Array1D_string const ValidDayNames( 12, { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Holiday", "SummerDesignDay", "WinterDesignDay", "CustomDay1", "CustomDay2" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//////////// hoisted into namespace changed to GetBranchInputOneTimeFlag////////////
		//	static bool GetInputFlag( true ); // Set to true before execution starts changed to GetEnvironmentInputOneTimeFlag
		//	static bool FirstCall( true ); // changed to GetEnvironmentFirstCall
		//static bool PrntEnvHeaders( true );
		////////////////////////////////////////////////
		int Loop;
		std::string StDate;
		std::string EnDate;
		std::string string;
		std::string cTotalEnvDays;
		int NumDays;
		int DSTActStMon;
		int DSTActStDay;
		int DSTActEnMon;
		int DSTActEnDay;
		int RunStJDay;
		int RunEnJDay;
		bool OkRun;
		int ThisWeekDay;
		int TWeekDay;
		Array1D_int MonWeekDay( 12 );
		Array1D_int ActEndDayOfMonth( 12 );
		int JDay5Start;
		int JDay5End;
		std::string Source;
		std::string ApWkRule;
		std::string AlpUseDST;
		std::string AlpUseSpec;
		std::string AlpUseRain;
		std::string AlpUseSnow;
		std::string kindOfRunPeriod;
		Real64 GrossApproxAvgDryBulb;

		if ( BeginSimFlag && GetEnvironmentFirstCall ) {

			PrintEndDataDictionary = true;

			ReportOutputFileHeaders(); // Write the output file header information

			// SetupOutputVariables, CurrentModuleObject='All Simulations'

			SetupOutputVariable( "Site Outdoor Air Drybulb Temperature [C]", OutDryBulbTemp, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Outdoor Air Dewpoint Temperature [C]", OutDewPointTemp, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Outdoor Air Wetbulb Temperature [C]", OutWetBulbTemp, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Outdoor Air Humidity Ratio [kgWater/kgDryAir]", OutHumRat, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Outdoor Air Relative Humidity [%]", OutRelHum, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Outdoor Air Barometric Pressure [Pa]", OutBaroPress, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Wind Speed [m/s]", WindSpeed, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Wind Direction [deg]", WindDir, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Sky Temperature [C]", SkyTemp, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Horizontal Infrared Radiation Rate per Area [W/m2]", HorizIRSky, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Diffuse Solar Radiation Rate per Area [W/m2]", DifSolarRad, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Direct Solar Radiation Rate per Area [W/m2]", BeamSolarRad, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Precipitation Depth [m]", LiquidPrecipitation, "Zone", "Sum", "Environment" );
			SetupOutputVariable( "Site Ground Reflected Solar Radiation Rate per Area [W/m2]", GndSolarRad, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Ground Temperature [C]", GroundTemp, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Surface Ground Temperature [C]", GroundTemp_Surface, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Deep Ground Temperature [C]", GroundTemp_Deep, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Simple Factor Model Ground Temperature [C]", GroundTempFC, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Outdoor Air Enthalpy [J/kg]", OutEnthalpy, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Outdoor Air Density [kg/m3]", OutAirDensity, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Solar Azimuth Angle [deg]", SolarAzimuthAngle, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Solar Altitude Angle [deg]", SolarAltitudeAngle, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Solar Hour Angle [deg]", HrAngle, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Rain Status []", RptIsRain, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Snow on Ground Status []", RptIsSnow, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Exterior Horizontal Sky Illuminance [lux]", HISKF, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Exterior Horizontal Beam Illuminance [lux]", HISUNF, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Exterior Beam Normal Illuminance [lux]", HISUNFnorm, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Sky Diffuse Solar Radiation Luminous Efficacy [lum/W]", PDIFLW, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Beam Solar Radiation Luminous Efficacy [lum/W]", PDIRLW, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Daylighting Model Sky Clearness []", SkyClearness, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Daylighting Model Sky Brightness []", SkyBrightness, "Zone", "Average", "Environment" );
			SetupOutputVariable( "Site Daylight Saving Time Status []", DSTIndicator, "Zone", "State", "Environment" );
			SetupOutputVariable( "Site Day Type Index []", RptDayType, "Zone", "State", "Environment" );
			SetupOutputVariable( "Site Mains Water Temperature [C]", WaterMainsTemp, "Zone", "Average", "Environment" );

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "Weather Data", "Environment", "Outdoor Dry Bulb", "[C]", EMSOutDryBulbOverrideOn, EMSOutDryBulbOverrideValue );
				SetupEMSActuator( "Weather Data", "Environment", "Outdoor Dew Point", "[C]", EMSOutDewPointTempOverrideOn, EMSOutDewPointTempOverrideValue );
				SetupEMSActuator( "Weather Data", "Environment", "Outdoor Relative Humidity", "[%]", EMSOutRelHumOverrideOn, EMSOutRelHumOverrideValue );
				SetupEMSActuator( "Weather Data", "Environment", "Diffuse Solar", "[W/m2]", EMSDifSolarRadOverrideOn, EMSDifSolarRadOverrideValue );
				SetupEMSActuator( "Weather Data", "Environment", "Direct Solar", "[W/m2]", EMSBeamSolarRadOverrideOn, EMSBeamSolarRadOverrideValue );
				SetupEMSActuator( "Weather Data", "Environment", "Wind Speed", "[m/s]", EMSWindSpeedOverrideOn, EMSWindSpeedOverrideValue );
				SetupEMSActuator( "Weather Data", "Environment", "Wind Direction", "[deg]", EMSWindDirOverrideOn, EMSWindDirOverrideValue );
			}

			GetEnvironmentFirstCall = false;

		} // ... end of BeginSimFlag IF-THEN block.

		if ( GetBranchInputOneTimeFlag ) {

			SetupInterpolationValues();
			TimeStepFraction = 1.0 / double( NumOfTimeStepInHour );
			rhoAirSTP = Psychrometrics::PsyRhoAirFnPbTdbW( StdPressureSeaLevel, constant_twenty, constant_zero );
			OpenWeatherFile( ErrorsFound ); // moved here because of possibility of special days on EPW file
			CloseWeatherFile();
			ReadUserWeatherInput();
			AllocateWeatherData();
			if ( NumIntervalsPerHour != 1 ) {
				if ( NumIntervalsPerHour != NumOfTimeStepInHour ) {
					ShowSevereError( RoutineName + "Number of intervals per hour on Weather file does not match specified number of Time Steps Per Hour" );
					ErrorsFound = true;
				}
			}
			GetBranchInputOneTimeFlag = false;
			Envrn = 0;
			if ( NumOfEnvrn > 0 ) {
				ResolveLocationInformation( ErrorsFound ); // Obtain weather related info from input file
				CheckLocationValidity();
				if ( ( Environment( NumOfEnvrn ).KindOfEnvrn != ksDesignDay ) && ( Environment( NumOfEnvrn ).KindOfEnvrn != ksHVACSizeDesignDay ) ) {
					CheckWeatherFileValidity();
				}
				if ( ErrorsFound ) {
					ShowSevereError( RoutineName + "No location specified, program will terminate." );
				}
			} else {
				ErrorsFound = true;
				ShowSevereError( RoutineName + "No Design Days or Run Period(s) specified, program will terminate." );
			}
			if ( DDOnly && TotDesDays == 0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + "Requested Design Days only (DDOnly) but no Design Days specified, program will terminate." );
			}
			if ( ReverseDD && TotDesDays == 1 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + "Requested Reverse Design Days (ReverseDD) but only 1 Design Day specified, program will terminate." );
			}
			CurrentOverallSimDay = 0;
			TotalOverallSimDays = 0;
			MaxNumberSimYears = 1;
			for ( Loop = 1; Loop <= NumOfEnvrn; ++Loop ) {
				TotalOverallSimDays += Environment( Loop ).TotalDays;
				if ( Environment( Loop ).KindOfEnvrn == ksRunPeriodWeather ) {
					MaxNumberSimYears = max( MaxNumberSimYears, Environment( Loop ).NumSimYears );
				}
			}
			DisplaySimDaysProgress( CurrentOverallSimDay, TotalOverallSimDays );
		}

		CloseWeatherFile(); // will only close if opened.
		++Envrn;
		DatesShouldBeReset = false;
		if ( Envrn > NumOfEnvrn ) {
			Available = false;
			Envrn = 0;
			CurEnvirNum = 0;
		} else {
			KindOfSim = Environment( Envrn ).KindOfEnvrn;
			DayOfYear = Environment( Envrn ).StartJDay;
			DayOfMonth = Environment( Envrn ).StartDay;
			Month = Environment( Envrn ).StartMonth;
			NumOfDayInEnvrn = Environment( Envrn ).TotalDays; // Set day loop maximum from DataGlobals
			if ( ! DoingSizing && ! KickOffSimulation ) {
				if ( AdaptiveComfortRequested_ASH55 || AdaptiveComfortRequested_CEN15251 ) {
					if ( KindOfSim == ksDesignDay ) {
						if ( DoDesDaySim ) {
							ShowWarningError( RoutineName + "Adaptive Comfort being reported during design day." );
							GrossApproxAvgDryBulb = ( DesDayInput( Envrn ).MaxDryBulb + ( DesDayInput( Envrn ).MaxDryBulb - DesDayInput( Envrn ).DailyDBRange ) ) / 2.0;
							if ( AdaptiveComfortRequested_ASH55 ) CalcThermalComfortAdaptiveASH55( true, false, GrossApproxAvgDryBulb );
							if ( AdaptiveComfortRequested_CEN15251 ) CalcThermalComfortAdaptiveCEN15251( true, false, GrossApproxAvgDryBulb );
						}
					} else {
						if ( DoWeathSim || DoDesDaySim ) {
							if ( AdaptiveComfortRequested_ASH55 ) CalcThermalComfortAdaptiveASH55( true, true, 0.0 );
							if ( AdaptiveComfortRequested_CEN15251 ) CalcThermalComfortAdaptiveCEN15251( true, true, 0.0 );
						}
					}
				}
			}
			if ( Envrn > TotDesDays && WeatherFileExists ) {
				OpenEPlusWeatherFile( ErrorsFound, false );
			}
			Available = true;
			if ( ( KindOfSim == ksRunPeriodWeather ) && ( ! WeatherFileExists && DoWeathSim ) ) {
				if ( ! DoingSizing && ! KickOffSimulation ) {
					ShowSevereError( "Weather Simulation requested, but no weather file attached." );
					ErrorsFound = true;
				}
				if ( ! DoingHVACSizingSimulations) Envrn = 0;
				Available = false;
			} else if ( ( KindOfSim == ksRunPeriodWeather ) && ( ! WeatherFileExists && ! DoWeathSim ) ) {
				Available = false;
				if ( ! DoingHVACSizingSimulations) Envrn = 0;
			} else if ( ( KindOfSim == ksRunPeriodWeather ) && DoingSizing ) {
				Available = false;
				Envrn = 0;
			}

			if ( ! ErrorsFound && Available && Envrn > 0 ) {
				EnvironmentName = Environment( Envrn ).Title;
				CurEnvirNum = Envrn;
				RunPeriodStartDayOfWeek = 0;
				if ( ( DoDesDaySim && ( KindOfSim != ksRunPeriodWeather ) ) || ( ( KindOfSim == ksRunPeriodWeather ) && DoWeathSim ) ) {
					if ( PrntEnvHeaders && DoWeatherInitReporting ) {
						gio::write( OutputFileInits, EnvironFormat );
						PrntEnvHeaders = false;
					}

					{ auto const SELECT_CASE_var( KindOfSim );

					if ( ( SELECT_CASE_var == ksRunPeriodWeather ) || ( SELECT_CASE_var == ksRunPeriodDesign ) ) {
						kindOfRunPeriod = Environment( Envrn ).cKindOfEnvrn;
						if ( KindOfSim == ksRunPeriodWeather ) {
							RunPeriodEnvironment = true;
						} else {
							RunPeriodEnvironment = false;
						}
						ActEndDayOfMonth = EndDayOfMonth;
						CurrentYearIsLeapYear = Environment( Envrn ).IsLeapYear;
						if ( CurrentYearIsLeapYear && WFAllowsLeapYears ) {
							LeapYearAdd = 1;
						} else {
							LeapYearAdd = 0;
						}
						if ( CurrentYearIsLeapYear ) {
							ActEndDayOfMonth( 2 ) = EndDayOfMonth( 2 ) + LeapYearAdd;
						}
						UseDaylightSaving = Environment( Envrn ).UseDST;
						UseSpecialDays = Environment( Envrn ).UseHolidays;
						UseRainValues = Environment( Envrn ).UseRain;
						UseSnowValues = Environment( Envrn ).UseSnow;

						OkRun = false;
						ThisWeekDay = 0;
						for ( Loop = 1; Loop <= NumDataPeriods; ++Loop ) {
							if ( ! Environment( Envrn ).ActualWeather ) {
								RunStJDay = JulianDay( DataPeriods( Loop ).StMon, DataPeriods( Loop ).StDay, LeapYearAdd );
								RunEnJDay = JulianDay( DataPeriods( Loop ).EnMon, DataPeriods( Loop ).EnDay, LeapYearAdd );
								if ( ! BetweenDates( Environment( Envrn ).StartJDay, RunStJDay, RunEnJDay ) ) continue;
								if ( ! BetweenDates( Environment( Envrn ).EndJDay, RunStJDay, RunEnJDay ) ) continue;
								OkRun = true;
								if ( RunStJDay > Environment( Envrn ).StartJDay ) {
									NumDays = RunStJDay - Environment( Envrn ).StartJDay;
								} else {
									NumDays = Environment( Envrn ).StartJDay - RunStJDay;
								}
								ThisWeekDay = mod( DataPeriods( Loop ).WeekDay + NumDays - 1, 7 ) + 1;
								break;
							} else { // Actual Weather
								RunStJDay = DataPeriods( Loop ).DataStJDay;
								RunEnJDay = DataPeriods( Loop ).DataEnJDay;
								if ( ! DataPeriods( Loop ).HasYearData ) {
									ShowSevereError( "GetNextEnvironment: Runperiod:CustomRange has been entered but weatherfile DATA PERIOD does not have year included in start/end date." );
									ShowContinueError( "...to match the RunPeriod, the DATA PERIOD should be mm/dd/yyyy for both." );
								}
								if ( ! BetweenDates( Environment( Envrn ).StartDate, RunStJDay, RunEnJDay ) ) continue;
								if ( ! BetweenDates( Environment( Envrn ).EndDate, RunStJDay, RunEnJDay ) ) continue;
								OkRun = true;
								if ( RunStJDay > Environment( Envrn ).StartDate ) {
									NumDays = RunStJDay - Environment( Envrn ).StartDate;
								} else {
									NumDays = Environment( Envrn ).StartDate - RunStJDay;
								}
								ThisWeekDay = mod( DataPeriods( Loop ).WeekDay + NumDays - 1, 7 ) + 1;
								break;
							}
						}

						if ( ! OkRun ) {
							if ( ! Environment( Envrn ).ActualWeather ) {
								gio::write( StDate, DateFormat ) << Environment( Envrn ).StartMonth << Environment( Envrn ).StartDay;
								gio::write( EnDate, DateFormat ) << Environment( Envrn ).EndMonth << Environment( Envrn ).EndDay;
								ShowSevereError( RoutineName + "Runperiod [mm/dd] (Start=" + StDate + ",End=" + EnDate + ") requested not within Data Period(s) from Weather File" );
							} else {
								gio::write( StDate, DateFormatwithYear ) << Environment( Envrn ).StartMonth << Environment( Envrn ).StartDay << Environment( Envrn ).StartYear;
								gio::write( EnDate, DateFormatwithYear ) << Environment( Envrn ).EndMonth << Environment( Envrn ).EndDay << Environment( Envrn ).EndYear;
								ShowSevereError( RoutineName + "Runperiod [mm/dd/yyyy] (Start=" + StDate + ",End=" + EnDate + ") requested not within Data Period(s) from Weather File" );
							}
							gio::write( StDate, DateFormat ) << DataPeriods( 1 ).StMon << DataPeriods( 1 ).StDay;
							gio::write( EnDate, DateFormat ) << DataPeriods( 1 ).EnMon << DataPeriods( 1 ).EnDay;
							if ( DataPeriods( 1 ).StYear > 0 ) {
								string = RoundSigDigits( DataPeriods( 1 ).StYear );
								StDate += "/" + string;
							} else {
								StDate += "/<noyear>";
							}
							if ( DataPeriods( 1 ).EnYear > 0 ) {
								string = RoundSigDigits( DataPeriods( 1 ).EnYear );
								EnDate += "/" + string;
							} else {
								EnDate += "/<noyear>";
							}
							if ( NumDataPeriods == 1 ) {
								ShowContinueError( "Weather Data Period (Start=" + StDate + ",End=" + EnDate );
							} else {
								ShowContinueError( "Multiple Weather Data Periods 1st (Start=" + StDate + ",End=" + EnDate + ')' );
							}
							ShowFatalError( RoutineName + "Program terminates due to preceding condition." );
						}

						// Following builds Environment start/end for ASHRAE 55 warnings
						gio::write( StDate, DateFormat ) << Environment( Envrn ).StartMonth << Environment( Envrn ).StartDay;
						gio::write( EnDate, DateFormat ) << Environment( Envrn ).EndMonth << Environment( Envrn ).EndDay;
						if ( Environment( Envrn ).ActualWeather ) {
							string = RoundSigDigits( Environment( Envrn ).StartYear );
							StDate += "/" + string;
							string = RoundSigDigits( Environment( Envrn ).EndYear );
							EnDate += "/" + string;
						} else if ( Environment( Envrn ).CurrentYear > 0 && Environment( Envrn ).TreatYearsAsConsecutive ) {
							string = RoundSigDigits( Environment( Envrn ).CurrentYear );
							StDate += "/" + string;
							string = RoundSigDigits( Environment( Envrn ).CurrentYear + Environment( Envrn ).NumSimYears );
							EnDate += "/" + string;
						}
						EnvironmentStartEnd = StDate + " - " + EnDate;

						if ( DoWeatherInitReporting ) {
							if ( Environment( Envrn ).UseDST ) {
								AlpUseDST = "Yes";
							} else {
								AlpUseDST = "No";
							}
							if ( Environment( Envrn ).UseHolidays ) {
								AlpUseSpec = "Yes";
							} else {
								AlpUseSpec = "No";
							}
							if ( Environment( Envrn ).ApplyWeekendRule ) {
								ApWkRule = "Yes";
							} else {
								ApWkRule = "No";
							}
							if ( Environment( Envrn ).UseRain ) {
								AlpUseRain = "Yes";
							} else {
								AlpUseRain = "No";
							}
							if ( Environment( Envrn ).UseSnow ) {
								AlpUseSnow = "Yes";
							} else {
								AlpUseSnow = "No";
							}
							cTotalEnvDays = RoundSigDigits( Environment( Envrn ).TotalDays );
							if ( Environment( Envrn ).DayOfWeek == 0 ) { // Uses Weather file start
								gio::write( OutputFileInits, EnvNameFormat ) << Environment( Envrn ).Title << kindOfRunPeriod << StDate << EnDate << ValidDayNames( ThisWeekDay ) << cTotalEnvDays << "UseWeatherFile" << AlpUseDST << AlpUseSpec << ApWkRule << AlpUseRain << AlpUseSnow;
								TWeekDay = ThisWeekDay;
								MonWeekDay = DataPeriods( Loop ).MonWeekDay;
							} else {
								gio::write( OutputFileInits, EnvNameFormat ) << Environment( Envrn ).Title << kindOfRunPeriod << StDate << EnDate << ValidDayNames( Environment( Envrn ).DayOfWeek ) << cTotalEnvDays << "Use RunPeriod Specified Day" << AlpUseDST << AlpUseSpec << ApWkRule << AlpUseRain << AlpUseSnow;
								TWeekDay = Environment( Envrn ).DayOfWeek;
								MonWeekDay = Environment( Envrn ).MonWeekDay;
							}
						} else { // just in case
							if ( Environment( Envrn ).DayOfWeek == 0 ) { // Uses Weather file start
								TWeekDay = ThisWeekDay;
								MonWeekDay = DataPeriods( Loop ).MonWeekDay;
							} else {
								TWeekDay = Environment( Envrn ).DayOfWeek;
								MonWeekDay = Environment( Envrn ).MonWeekDay;
							}
						}

						if ( ! DoingSizing && ! KickOffSimulation ) {
							if ( ( KindOfSim == ksRunPeriodWeather && DoWeathSim ) ) {
								if ( AdaptiveComfortRequested_ASH55 || AdaptiveComfortRequested_CEN15251 ) {
									if ( WFAllowsLeapYears ) {
										ShowSevereError( RoutineName + "AdaptiveComfort Reporting does not work correctly with leap years in weather files." );
										ErrorsFound = true;
									}
									if ( NumDataPeriods != 1 ) {
										ShowSevereError( RoutineName + "AdaptiveComfort Reporting does not work correctly with multiple dataperiods in weather files." );
										ErrorsFound = true;
									}
									if ( DataPeriods( 1 ).StMon == 1 && DataPeriods( 1 ).StDay == 1 ) {
										RunStJDay = JulianDay( DataPeriods( 1 ).StMon, DataPeriods( 1 ).StDay, LeapYearAdd );
										RunEnJDay = JulianDay( DataPeriods( 1 ).EnMon, DataPeriods( 1 ).EnDay, LeapYearAdd );
										if ( RunEnJDay - RunStJDay + 1 != 365 ) {
											ShowSevereError( RoutineName + "AdaptiveComfort Reporting does not work correctly with weather files that do not contain 365 days." );
											ErrorsFound = true;
										}
									} else {
										ShowSevereError( RoutineName + "AdaptiveComfort Reporting does not work correctly with weather files that do not start on 1 January." );
										ErrorsFound = true;
									}
									if ( NumIntervalsPerHour != 1 ) {
										ShowSevereError( RoutineName + "AdaptiveComfort Reporting does not work correctly with weather files that have multiple interval records per hour." );
										ErrorsFound = true;
									}
								}
							}
						}

						// Only need to set Week days for Run Days
						RunPeriodStartDayOfWeek = TWeekDay;
						WeekDayTypes = 0;
						JDay5Start = JulianDay( Environment( Envrn ).StartMonth, Environment( Envrn ).StartDay, LeapYearAdd );
						JDay5End = JulianDay( Environment( Envrn ).EndMonth, Environment( Envrn ).EndDay, LeapYearAdd );
						if ( JDay5End >= JDay5Start ) {
							curSimDayForEndOfRunPeriod = DayOfSim + ( JDay5End - JDay5Start ) + LeapYearAdd;
						} else {
							curSimDayForEndOfRunPeriod = DayOfSim + JulianDay( 12, 31, LeapYearAdd ) - JDay5Start + JDay5End;
						}
						Loop = JDay5Start;
						while ( true ) {
							WeekDayTypes( Loop ) = TWeekDay;
							TWeekDay = mod( TWeekDay, 7 ) + 1;
							++Loop;
							if ( Loop > 366 ) Loop = 1;
							if ( Loop == JDay5End ) break;
						}

						if ( UseDaylightSaving ) {
							if ( EPWDaylightSaving ) {
								DaylightSavingIsActive = true;
							}
						} else {
							DaylightSavingIsActive = false;
						}
						if ( IDFDaylightSaving ) {
							DaylightSavingIsActive = true;
						}
						Environment( Envrn ).SetWeekDays = false;
						if ( Environment( Envrn ).ActualWeather ) {
							curSimDayForEndOfRunPeriod = Environment( Envrn ).TotalDays;
						}
						if ( DaylightSavingIsActive ) {
							SetDSTDateRanges( MonWeekDay, DSTIndex, DSTActStMon, DSTActStDay, DSTActEnMon, DSTActEnDay );
						}

						SetSpecialDayDates( MonWeekDay );

						if ( Environment( Envrn ).StartMonth != 1 || Environment( Envrn ).StartDay != 1 ) {
							StartDatesCycleShouldBeReset = true;
							Jan1DatesShouldBeReset = true;
						}

						if ( Environment( Envrn ).StartMonth == 1 && Environment( Envrn ).StartDay == 1 ) {
							StartDatesCycleShouldBeReset = false;
							Jan1DatesShouldBeReset = true;
						}

						if ( Environment( Envrn ).ActualWeather ) {
							StartDatesCycleShouldBeReset = false;
							Jan1DatesShouldBeReset = true;
						}

						// Report Actual Dates for Daylight Saving and Special Days
						if ( ! KickOffSimulation ) {
							Source = BlankString;
							if ( UseDaylightSaving ) {
								if ( EPWDaylightSaving ) {
									Source = "WeatherFile";
								}
							} else {
								Source = "RunPeriod Object";
							}
							if ( IDFDaylightSaving ) {
								Source = "InputFile";
							}
							if ( DaylightSavingIsActive && DoWeatherInitReporting ) {
								gio::write( StDate, DateFormat ) << DSTActStMon << DSTActStDay;
								gio::write( EnDate, DateFormat ) << DSTActEnMon << DSTActEnDay;
								gio::write( OutputFileInits, EnvDSTYFormat ) << Source << StDate << EnDate;
							} else if ( DoOutputReporting ) {
								gio::write( OutputFileInits, EnvDSTNFormat ) << Source;
							}
							for ( Loop = 1; Loop <= NumSpecialDays; ++Loop ) {
								if ( SpecialDays( Loop ).WthrFile && UseSpecialDays && DoWeatherInitReporting ) {
									gio::write( StDate, DateFormat ) << SpecialDays( Loop ).ActStMon << SpecialDays( Loop ).ActStDay;
									gio::write( OutputFileInits, EnvSpDyFormat ) << SpecialDays( Loop ).Name << SpecialDayNames( SpecialDays( Loop ).DayType ) << "WeatherFile" << StDate << SpecialDays( Loop ).Duration;
								}
								if ( ! SpecialDays( Loop ).WthrFile && DoWeatherInitReporting ) {
									gio::write( StDate, DateFormat ) << SpecialDays( Loop ).ActStMon << SpecialDays( Loop ).ActStDay;
									gio::write( OutputFileInits, EnvSpDyFormat ) << SpecialDays( Loop ).Name << SpecialDayNames( SpecialDays( Loop ).DayType ) << "InputFile" << StDate << SpecialDays( Loop ).Duration;
								}
							}
						}

					} else if (SELECT_CASE_var == ksDesignDay || SELECT_CASE_var == ksHVACSizeDesignDay ) { // Design Day
						RunPeriodEnvironment = false;
						gio::write(StDate, DateFormat) << DesDayInput(Environment(Envrn).DesignDayNum).Month << DesDayInput(Environment(Envrn).DesignDayNum).DayOfMonth;
						EnDate = StDate;
						if (DesDayInput(Environment(Envrn).DesignDayNum).DayType <= 7 && DoWeatherInitReporting) {
							gio::write(OutputFileInits, EnvNameFormat) << Environment(Envrn).Title << "SizingPeriod:DesignDay" << StDate << EnDate << DaysOfWeek(DesDayInput(Environment(Envrn).DesignDayNum).DayType) << "1" << "N/A" << "N/A" << "N/A" << "N/A" << "N/A" << "N/A";
						} else if ( DoWeatherInitReporting ) {
							gio::write(OutputFileInits, EnvNameFormat) << Environment(Envrn).Title << "SizingPeriod:DesignDay" << StDate << EnDate << SpecialDayNames(DesDayInput(Environment(Envrn).DesignDayNum).DayType - 7) << "1" << "N/A" << "N/A" << "N/A" << "N/A" << "N/A" << "N/A";
						}
						if (DesDayInput(Environment(Envrn).DesignDayNum).DSTIndicator == 0 && DoWeatherInitReporting) {
							gio::write( OutputFileInits, EnvDSTNFormat ) << "SizingPeriod:DesignDay";
						} else if ( DoWeatherInitReporting ) {
							gio::write( OutputFileInits, EnvDSTYFormat ) << "SizingPeriod:DesignDay" << StDate << EnDate;
						}

					}}

				}
			} // ErrorsFound
		}

		if ( ErrorsFound && ! DoingSizing && ! KickOffSimulation ) {
			ShowSevereError( RoutineName + "Errors found in getting a new environment" );
			Available = false;
		} else if ( ErrorsFound ) {
			Available = false;
		}

	}

	void
	AddDesignSetToEnvironmentStruct(
		int const HVACSizingIterCount
	)
	{
		// SUBROUTINE INFORMATION:

		using DataGlobals::ksDesignDay;
		using DataGlobals::ksRunPeriodDesign;
		using DataGlobals::ksHVACSizeDesignDay;
		using DataGlobals::ksHVACSizeRunPeriodDesign;

		int OrigNumOfEnvrn;

		OrigNumOfEnvrn = NumOfEnvrn;
		for ( int i = 1; i <= OrigNumOfEnvrn; ++i ) {
			if ( Environment(i).KindOfEnvrn == ksDesignDay) {
				Environment.redimension(++NumOfEnvrn);
				Environment(NumOfEnvrn) = Environment(i); // copy over seed data from current array element
				Environment(NumOfEnvrn).SeedEnvrnNum = i;
				Environment(NumOfEnvrn).KindOfEnvrn = ksHVACSizeDesignDay;
				Environment(NumOfEnvrn).Title = Environment(i).Title + " HVAC Sizing Pass " + RoundSigDigits( HVACSizingIterCount );
				Environment(NumOfEnvrn).HVACSizingIterationNum = HVACSizingIterCount;
			} else if (Environment(i).KindOfEnvrn == ksRunPeriodDesign) {
				Environment.redimension(++NumOfEnvrn);
				Environment(NumOfEnvrn) = Environment(i); // copy over seed data
				Environment(NumOfEnvrn).SeedEnvrnNum = i;
				Environment(NumOfEnvrn).KindOfEnvrn = ksHVACSizeRunPeriodDesign;
				Environment(NumOfEnvrn).Title = Environment(i).Title + " HVAC Sizing Pass " + RoundSigDigits( HVACSizingIterCount );
				Environment(NumOfEnvrn).HVACSizingIterationNum = HVACSizingIterCount;
			}
		}  // for each loop over Environment data strucure

	}

	void
	SetupWeekDaysByMonth(
		int const StMon,
		int const StDay,
		int const StWeekDay,
		Array1A_int WeekDays
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the weekday for each month based on the start date and
		// weekday specified for that date.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		WeekDays.dim( 12 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int CurWeekDay;

		// Set 1st day of Start Month
		CurWeekDay = StWeekDay;
		for ( Loop = 1; Loop <= StDay - 1; ++Loop ) {
			--CurWeekDay;
			if ( CurWeekDay == 0 ) CurWeekDay = 7;
		}

		WeekDays( StMon ) = CurWeekDay;
		for ( Loop = StMon + 1; Loop <= 12; ++Loop ) {

			{ auto const SELECT_CASE_var( Loop );
			if ( SELECT_CASE_var == 2 ) {
				CurWeekDay += EndDayOfMonth( 1 );
				while ( CurWeekDay > 7 ) {
					CurWeekDay -= 7;
				}
				WeekDays( Loop ) = CurWeekDay;

			} else if ( SELECT_CASE_var == 3 ) {
				CurWeekDay += EndDayOfMonth( Loop - 1 ) + LeapYearAdd;
				while ( CurWeekDay > 7 ) {
					CurWeekDay -= 7;
				}
				WeekDays( Loop ) = CurWeekDay;

			} else if ( ( SELECT_CASE_var >= 4 ) && ( SELECT_CASE_var <= 12 ) ) {
				CurWeekDay += EndDayOfMonth( Loop - 1 );
				while ( CurWeekDay > 7 ) {
					CurWeekDay -= 7;
				}
				WeekDays( Loop ) = CurWeekDay;

			}}
		}

		if ( any_eq( WeekDays, 0 ) ) {
			// need to start at StMon and go backwards.
			// EndDayOfMonth is also "days" in month.  (without leapyear day in February)
			CurWeekDay = StWeekDay;
			for ( Loop = 1; Loop <= StDay - 1; ++Loop ) {
				--CurWeekDay;
				if ( CurWeekDay == 0 ) CurWeekDay = 7;
			}

			for ( Loop = StMon - 1; Loop >= 1; --Loop ) {

				{ auto const SELECT_CASE_var( Loop );

				if ( SELECT_CASE_var == 1 ) {
					CurWeekDay -= EndDayOfMonth( 1 );
					while ( CurWeekDay <= 0 ) {
						CurWeekDay += 7;
					}
					WeekDays( Loop ) = CurWeekDay;

				} else if ( SELECT_CASE_var == 2 ) {
					CurWeekDay = CurWeekDay - EndDayOfMonth( 2 ) + LeapYearAdd;
					while ( CurWeekDay <= 0 ) {
						CurWeekDay += 7;
					}
					WeekDays( Loop ) = CurWeekDay;

				} else if ( ( SELECT_CASE_var >= 3 ) && ( SELECT_CASE_var <= 12 ) ) {
					CurWeekDay -= EndDayOfMonth( Loop );
					while ( CurWeekDay <= 0 ) {
						CurWeekDay += 7;
					}
					WeekDays( Loop ) = CurWeekDay;
				}}

			}

		}

	}

	void
	ResetWeekDaysByMonth(
		Array1A_int WeekDays,
		int const LeapYearAdd,
		int const StartMonth,
		int const StartMonthDay,
		int const EndMonth,
		int const EndMonthDay,
		bool const Rollover,
		Optional_bool_const MidSimReset
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine resets the weekday for each month based on the current weekday
		// and previous weekdays per month.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		WeekDays.dim( 12 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_int WeekDaysCopy( 12 );
		int Loop;
		int CurWeekDay;
		bool ResetMidSimulation;

		ResetMidSimulation = false;
		if ( present( MidSimReset ) ) ResetMidSimulation = MidSimReset;

		WeekDaysCopy = WeekDays;
		if ( ! ResetMidSimulation ) {
			if ( Rollover ) {
				if ( StartMonth == 1 ) {
					CurWeekDay = WeekDays( 12 ) + EndDayOfMonth( 12 ) + StartMonthDay - 1;
				} else {
					CurWeekDay = WeekDays( EndMonth ) + EndMonthDay;
				}
			} else { // restart at same as before
				CurWeekDay = WeekDays( StartMonth );
			}
			while ( CurWeekDay > 7 ) {
				CurWeekDay -= 7;
			}

			WeekDays = 0;
			WeekDays( StartMonth ) = CurWeekDay;
			for ( Loop = StartMonth + 1; Loop <= 12; ++Loop ) {
				{ auto const SELECT_CASE_var( Loop );
				if ( SELECT_CASE_var == 2 ) {
					CurWeekDay += EndDayOfMonth( 1 );
					while ( CurWeekDay > 7 ) {
						CurWeekDay -= 7;
					}
					WeekDays( Loop ) = CurWeekDay;

				} else if ( SELECT_CASE_var == 3 ) {
					CurWeekDay += EndDayOfMonth( Loop - 1 ) + LeapYearAdd;
					while ( CurWeekDay > 7 ) {
						CurWeekDay -= 7;
					}
					WeekDays( Loop ) = CurWeekDay;

				} else if ( ( SELECT_CASE_var >= 4 ) && ( SELECT_CASE_var <= 12 ) ) {
					CurWeekDay += EndDayOfMonth( Loop - 1 );
					while ( CurWeekDay > 7 ) {
						CurWeekDay -= 7;
					}
					WeekDays( Loop ) = CurWeekDay;
				}}
			}

			if ( any_eq( WeekDays, 0 ) ) {
				// need to start at StMon and go backwards.
				// EndDayOfMonth is also "days" in month.  (without leapyear day in February)
				CurWeekDay = WeekDays( StartMonth );
				for ( Loop = 1; Loop <= StartMonthDay - 1; ++Loop ) {
					--CurWeekDay;
					if ( CurWeekDay == 0 ) CurWeekDay = 7;
				}

				for ( Loop = StartMonth - 1; Loop >= 1; --Loop ) {

					{ auto const SELECT_CASE_var( Loop );

					if ( SELECT_CASE_var == 1 ) {
						CurWeekDay -= EndDayOfMonth( 1 );
						while ( CurWeekDay <= 0 ) {
							CurWeekDay += 7;
						}
						WeekDays( Loop ) = CurWeekDay;

					} else if ( SELECT_CASE_var == 2 ) {
						CurWeekDay = CurWeekDay - EndDayOfMonth( 2 ) + LeapYearAdd;
						while ( CurWeekDay <= 0 ) {
							CurWeekDay += 7;
						}
						WeekDays( Loop ) = CurWeekDay;

					} else if ( ( SELECT_CASE_var >= 3 ) && ( SELECT_CASE_var <= 12 ) ) {
						CurWeekDay -= EndDayOfMonth( Loop );
						while ( CurWeekDay <= 0 ) {
							CurWeekDay += 7;
						}
						WeekDays( Loop ) = CurWeekDay;
					}}

				}

			}

		} else {
			if ( Rollover ) {
				if ( StartMonth == 1 ) {
					CurWeekDay = WeekDays( 12 ) + EndDayOfMonth( 12 ) + StartMonthDay - 1;
				} else {
					CurWeekDay = WeekDays( EndMonth ) + EndMonthDay;
				}
			} else { // restart at same as before
				CurWeekDay = WeekDays( StartMonth );
			}
			while ( CurWeekDay > 7 ) {
				CurWeekDay -= 7;
			}
			WeekDays = 0;
			if ( StartMonth != 1 ) {
				CurWeekDay = WeekDaysCopy( 12 ) + EndDayOfMonth( 12 );
				while ( CurWeekDay > 7 ) {
					CurWeekDay -= 7;
				}
				WeekDays( 1 ) = CurWeekDay;
				CurWeekDay += EndDayOfMonth( 1 );
				while ( CurWeekDay > 7 ) {
					CurWeekDay -= 7;
				}
				WeekDays( 2 ) = CurWeekDay;
				CurWeekDay += EndDayOfMonth( 2 ) + LeapYearAdd;
				while ( CurWeekDay > 7 ) {
					CurWeekDay -= 7;
				}
				WeekDays( 3 ) = CurWeekDay;
				for ( Loop = 4; Loop <= 12; ++Loop ) {
					CurWeekDay += EndDayOfMonth( Loop - 1 );
					while ( CurWeekDay > 7 ) {
						CurWeekDay -= 7;
					}
					WeekDays( Loop ) = CurWeekDay;
				}
			} else {
				WeekDays = 0;
				WeekDays( StartMonth ) = CurWeekDay;
				for ( Loop = StartMonth + 1; Loop <= 12; ++Loop ) {
					{ auto const SELECT_CASE_var( Loop );
					if ( SELECT_CASE_var == 2 ) {
						CurWeekDay += EndDayOfMonth( 1 );
						while ( CurWeekDay > 7 ) {
							CurWeekDay -= 7;
						}
						WeekDays( Loop ) = CurWeekDay;

					} else if ( SELECT_CASE_var == 3 ) {
						CurWeekDay += EndDayOfMonth( Loop - 1 ) + LeapYearAdd;
						while ( CurWeekDay > 7 ) {
							CurWeekDay -= 7;
						}
						WeekDays( Loop ) = CurWeekDay;

					} else if ( ( SELECT_CASE_var >= 4 ) && ( SELECT_CASE_var <= 12 ) ) {
						CurWeekDay += EndDayOfMonth( Loop - 1 );
						while ( CurWeekDay > 7 ) {
							CurWeekDay -= 7;
						}
						WeekDays( Loop ) = CurWeekDay;
					}}
				}

				if ( any_eq( WeekDays, 0 ) ) {
					// need to start at StMon and go backwards.
					// EndDayOfMonth is also "days" in month.  (without leapyear day in February)
					CurWeekDay = WeekDays( StartMonth );
					for ( Loop = 1; Loop <= StartMonthDay - 1; ++Loop ) {
						--CurWeekDay;
						if ( CurWeekDay == 0 ) CurWeekDay = 7;
					}

					for ( Loop = StartMonth - 1; Loop >= 1; --Loop ) {

						{ auto const SELECT_CASE_var( Loop );

						if ( SELECT_CASE_var == 1 ) {
							CurWeekDay -= EndDayOfMonth( 1 );
							while ( CurWeekDay <= 0 ) {
								CurWeekDay += 7;
							}
							WeekDays( Loop ) = CurWeekDay;

						} else if ( SELECT_CASE_var == 2 ) {
							CurWeekDay = CurWeekDay - EndDayOfMonth( 2 ) + LeapYearAdd;
							while ( CurWeekDay <= 0 ) {
								CurWeekDay += 7;
							}
							WeekDays( Loop ) = CurWeekDay;

						} else if ( ( SELECT_CASE_var >= 3 ) && ( SELECT_CASE_var <= 12 ) ) {
							CurWeekDay -= EndDayOfMonth( Loop );
							while ( CurWeekDay <= 0 ) {
								CurWeekDay += 7;
							}
							WeekDays( Loop ) = CurWeekDay;
						}}

					}

				}
			}
		}

	}

	void
	SetDSTDateRanges(
		Array1S_int MonWeekDay, // Weekday of each day 1 of month
		Array1S_int DSTIndex, // DST Index for each julian day (1:366)
		Optional_int DSTActStMon,
		Optional_int DSTActStDay,
		Optional_int DSTActEnMon,
		Optional_int DSTActEnDay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// With multiple year weather files (or repeating weather files that rollover day),
		// need to set DST (Daylight Saving Time) dates at start of environment or year.
		// DST is only projected for one year.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::JulianDay;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SetDSTDateRanges: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ActStartMonth; // Actual Start Month
		int ActStartDay; // Actual Start Day of Month
		int ActEndMonth; // Actual End Month
		int ActEndDay; // Actual End Day of Month
		int ThisDay; // Day of Month
		int JDay;
		int JDay1;
		bool ErrorsFound;
		Array1D_int ActEndDayOfMonth( 12 );

		ErrorsFound = false;
		ActEndDayOfMonth = EndDayOfMonth;
		ActEndDayOfMonth( 2 ) = EndDayOfMonth( 2 ) + LeapYearAdd;
		if ( DST.StDateType == MonthDay ) {
			ActStartMonth = DST.StMon;
			ActStartDay = DST.StDay;
		} else if ( DST.StDateType == NthDayInMonth ) {
			ThisDay = DST.StWeekDay - MonWeekDay( DST.StMon ) + 1;
			while ( ThisDay <= 0 ) {
				ThisDay += 7;
			}
			ThisDay += 7 * ( DST.StDay - 1 );
			if ( ThisDay > ActEndDayOfMonth( DST.StMon ) ) {
				ShowSevereError( RoutineName + "Determining DST: DST Start Date, Nth Day of Month, not enough Nths" );
				ErrorsFound = true;
			} else {
				ActStartMonth = DST.StMon;
				ActStartDay = ThisDay;
			}
		} else { // LastWeekDayInMonth
			ThisDay = DST.StWeekDay - MonWeekDay( DST.StMon ) + 1;
			while ( ThisDay + 7 <= ActEndDayOfMonth( DST.StMon ) ) {
				ThisDay += 7;
			}
			ActStartMonth = DST.StMon;
			ActStartDay = ThisDay;
		}

		if ( DST.EnDateType == MonthDay ) {
			ActEndMonth = DST.EnMon;
			ActEndDay = DST.EnDay;
		} else if ( DST.EnDateType == NthDayInMonth ) {
			ThisDay = DST.EnWeekDay - MonWeekDay( DST.EnMon ) + 1;
			while ( ThisDay <= 0 ) {
				ThisDay += 7;
			}
			ThisDay += 7 * ( DST.EnDay - 1 );
			if ( ThisDay > ActEndDayOfMonth( DST.EnMon ) ) {
				ActEndMonth = 0; // Suppress uninitialized warning
				ActEndDay = 0; // Suppress uninitialized warning
				ShowSevereError( RoutineName + "Determining DST: DST End Date, Nth Day of Month, not enough Nths" );
				ErrorsFound = true;
			} else {
				ActEndMonth = DST.EnMon;
				ActEndDay = ThisDay;
			}
		} else { // LastWeekDayInMonth
			ThisDay = DST.EnWeekDay - MonWeekDay( DST.EnMon ) + 1;
			while ( ThisDay + 7 <= ActEndDayOfMonth( DST.EnMon ) ) {
				ThisDay += 7;
			}
			ActEndMonth = DST.EnMon;
			ActEndDay = ThisDay;
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Program terminates due to preceding condition(s)." );
		}

		if ( present( DSTActStMon ) ) {
			DSTActStMon = ActStartMonth;
			DSTActStDay = ActStartDay;
			DSTActEnMon = ActEndMonth;
			DSTActEnDay = ActEndDay;
		}

		DSTIndex = 0;
		JDay = JulianDay( ActStartMonth, ActStartDay, LeapYearAdd );
		JDay1 = JulianDay( ActEndMonth, ActEndDay, LeapYearAdd );
		if ( JDay1 >= JDay ) {
			DSTIndex( {JDay,JDay1} ) = 1;
		} else {
			DSTIndex( {JDay,366} ) = 1;
			DSTIndex( {1,JDay1} ) = 1;
		}

	}

	void
	SetSpecialDayDates( Array1S_int MonWeekDay ) // Weekday of each day 1 of month
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// With multiple year weather files (or repeating weather files that rollover day),
		// need to set Special Day dates at start of environment or year.
		// Special Days are only projected for one year.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::InvJulianDay;
		using General::JulianDay;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SetSpecialDayDates: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int ThisDay;
		int JDay;
		int JDay1;
		int Loop1;
		bool ErrorsFound;
		Array1D_int ActEndDayOfMonth( 12 );

		ErrorsFound = false;
		ActEndDayOfMonth = EndDayOfMonth;
		ActEndDayOfMonth( 2 ) = EndDayOfMonth( 2 ) + LeapYearAdd;
		SpecialDayTypes = 0;
		for ( Loop = 1; Loop <= NumSpecialDays; ++Loop ) {
			if ( SpecialDays( Loop ).WthrFile && ! UseSpecialDays ) continue;
			if ( SpecialDays( Loop ).DateType <= MonthDay ) {
				JDay = JulianDay( SpecialDays( Loop ).Month, SpecialDays( Loop ).Day, LeapYearAdd );
				if ( SpecialDays( Loop ).Duration == 1 && Environment( Envrn ).ApplyWeekendRule ) {
					if ( WeekDayTypes( JDay ) == 1 ) {
						// Sunday, must go to Monday
						++JDay;
						if ( JDay == 366 && LeapYearAdd == 0 ) JDay = 1;
					} else if ( WeekDayTypes( JDay ) == 7 ) {
						++JDay;
						if ( JDay == 366 && LeapYearAdd == 0 ) JDay = 1;
						++JDay;
						if ( JDay == 366 && LeapYearAdd == 0 ) JDay = 1;
					}
				}
				InvJulianDay( JDay, SpecialDays( Loop ).ActStMon, SpecialDays( Loop ).ActStDay, LeapYearAdd );
			} else if ( SpecialDays( Loop ).DateType == NthDayInMonth ) {
				if ( SpecialDays( Loop ).WeekDay >= MonWeekDay( SpecialDays( Loop ).Month ) ) {
					ThisDay = SpecialDays( Loop ).WeekDay - MonWeekDay( SpecialDays( Loop ).Month ) + 1;
				} else {
					ThisDay = SpecialDays( Loop ).WeekDay - MonWeekDay( SpecialDays( Loop ).Month ) + 1 + 7;
				}
				ThisDay += 7 * ( SpecialDays( Loop ).Day - 1 );
				if ( ThisDay > ActEndDayOfMonth( SpecialDays( Loop ).Month ) ) {
					ShowSevereError( RoutineName + "Special Day Date, Nth Day of Month, not enough Nths, for SpecialDay=" + SpecialDays( Loop ).Name );
					ErrorsFound = true;
					continue;
				}
				SpecialDays( Loop ).ActStMon = SpecialDays( Loop ).Month;
				SpecialDays( Loop ).ActStDay = ThisDay;
				JDay = JulianDay( SpecialDays( Loop ).Month, ThisDay, LeapYearAdd );
			} else { // LastWeekDayInMonth
				ThisDay = SpecialDays( Loop ).WeekDay - MonWeekDay( SpecialDays( Loop ).Month ) + 1;
				while ( ThisDay + 7 <= ActEndDayOfMonth( SpecialDays( Loop ).Month ) ) {
					ThisDay += 7;
				}
				SpecialDays( Loop ).ActStMon = SpecialDays( Loop ).Month;
				SpecialDays( Loop ).ActStDay = ThisDay;
				JDay = JulianDay( SpecialDays( Loop ).Month, ThisDay, LeapYearAdd );
			}
			if ( SpecialDayTypes( JDay ) != 0 ) {
				ShowWarningError( RoutineName + "Special Day definition (" + SpecialDays( Loop ).Name + ") is overwriting previously entered special day period" );
				if ( UseSpecialDays ) {
					ShowContinueError( "...This could be caused by definitions on the Weather File." );
				}
				ShowContinueError( "...This could be caused by duplicate definitions in the Input File." );
			}
			JDay1 = JDay - 1;
			for ( Loop1 = 0; Loop1 <= SpecialDays( Loop ).Duration - 1; ++Loop1 ) {
				++JDay1;
				if ( JDay1 == 366 && LeapYearAdd == 0 ) JDay1 = 1;
				if ( JDay1 == 367 ) JDay1 = 1;
				SpecialDayTypes( JDay1 ) = SpecialDays( Loop ).DayType;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Program terminates due to preceding condition(s)." );
		}

	}

	void
	InitializeWeather( bool & PrintEnvrnStamp ) // Set to true when the environment header should be printed
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver of the weather initializations.
		// Most of the weather handling can be described as "initializations"
		// so most of the work is done via this subroutine.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using General::InvJulianDay;
		using General::JulianDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int FirstSimDayofYear; // Variable which tells when to skip the day in a multi year simulation.

		static bool FirstCall( true ); // Some things should only be done once
		//  LOGICAL, SAVE :: SetYear=.TRUE.
		int JDay5Start;
		int JDay5End;
		int TWeekDay;

		// FLOW:

		if ( BeginSimFlag && FirstCall ) {

			FirstCall = false;
			EndMonthFlag = false;

		} // ... end of BeginSimFlag IF-THEN block.

		if ( BeginEnvrnFlag ) {

			//Call and setup the Design Day environment
			if ( Environment( Envrn ).KindOfEnvrn != ksRunPeriodWeather ) {
				if (Environment(Envrn).DesignDayNum > 0) {
					SetUpDesignDay(Environment(Envrn).DesignDayNum);
					EnvironmentName = Environment(Envrn).Title;
				}
			}

			NumMissing = 0; // Only used in Weather file environments
			// Start over missing values with each environment
			Missing.StnPres = StdBaroPress; // Initial "missing" value
			Missing.DryBulb = 6.0; // Initial "missing" value
			Missing.DewPoint = 3.0; // Initial "missing" value
			Missing.RelHumid = 50.0; // Initial "missing" value
			Missing.WindSpd = 2.5; // Initial "missing" value
			Missing.WindDir = 180; // Initial "missing" value
			Missing.TotSkyCvr = 5; // Initial "missing" value
			Missing.OpaqSkyCvr = 5; // Initial "missing" value
			Missing.Visibility = 777.7; // Initial "missing" value
			Missing.Ceiling = 77777; // Initial "missing" value
			Missing.PrecipWater = 0; // Initial "missing" value
			Missing.AerOptDepth = 0.0; // Initial "missing" value
			Missing.SnowDepth = 0; // Initial "missing" value
			Missing.DaysLastSnow = 88; // Initial "missing" value
			Missing.Albedo = 0.0; // Initial "missing" value
			Missing.LiquidPrecip = 0.0; // Initial "missing" value
			// Counts set to 0 for each environment
			Missed.StnPres = 0;
			Missed.DryBulb = 0;
			Missed.DewPoint = 0;
			Missed.RelHumid = 0;
			Missed.WindSpd = 0;
			Missed.WindDir = 0;
			Missed.TotSkyCvr = 0;
			Missed.OpaqSkyCvr = 0;
			Missed.Visibility = 0;
			Missed.Ceiling = 0;
			Missed.PrecipWater = 0;
			Missed.AerOptDepth = 0;
			Missed.SnowDepth = 0;
			Missed.DaysLastSnow = 0;
			Missed.Albedo = 0;
			Missed.LiquidPrecip = 0;
			Missed.WeathCodes = 0;
			Missed.DirectRad = 0;
			Missed.DiffuseRad = 0;
			// Counts set to 0 for each environment
			OutOfRange.StnPres = 0;
			OutOfRange.DryBulb = 0;
			OutOfRange.DewPoint = 0;
			OutOfRange.RelHumid = 0;
			OutOfRange.WindSpd = 0;
			OutOfRange.WindDir = 0;
			OutOfRange.DirectRad = 0;
			OutOfRange.DiffuseRad = 0;

			if ( !RPReadAllWeatherData ) {
				PrintEnvrnStamp = true; // Set this to true so that on first non-warmup day (only) the environment header will print out
			}

			//    WeekDayCount=0  ! Reset weekday count (weather periods only)
			for ( Loop = 1; Loop <= NumSpecialDays; ++Loop ) {
				SpecialDays( Loop ).Used = false;
			}

			if ((KindOfSim != ksDesignDay) && (KindOfSim != ksHVACSizeDesignDay)) {
				ReadWeatherForDay( 1, Envrn, false ); // Read first day's weather
			} else {
				TomorrowVariables = DesignDay(Environment(Envrn).DesignDayNum);
			}

		} // ... end of BeginEnvrnFlag IF-THEN block.

		if ( BeginDayFlag ) {

			// Check Holidays, Daylight Saving Time, Ground Temperatures, etc.

			UpdateWeatherData(); // Update daily weather info

			// Read tomorrow's weather only if necessary.  This means that the
			// simulation is out of warmup, is using a weather tape for this
			// environment, and is not on the last day (day after last day is
			// assumed to be equal to last day).

			// Following code checks whether the present day of simulation matches the start month and start day.
			// In a multi year simulation with run period less than 365, we need to position the weather line
			// appropriately.

			if ((!WarmupFlag) && ((Environment(Envrn).KindOfEnvrn != ksDesignDay) && (Environment(Envrn).KindOfEnvrn != ksHVACSizeDesignDay))) {
				if ( DayOfSim < NumOfDayInEnvrn ) {
					if ( DayOfSim == curSimDayForEndOfRunPeriod ) {
						curSimDayForEndOfRunPeriod += Environment( Envrn ).RawSimDays;
						if ( StartDatesCycleShouldBeReset ) {
							ResetWeekDaysByMonth( Environment( Envrn ).MonWeekDay, LeapYearAdd, Environment( Envrn ).StartMonth, Environment( Envrn ).StartDay, Environment( Envrn ).EndMonth, Environment( Envrn ).EndDay, Environment( Envrn ).RollDayTypeOnRepeat );
							if ( DaylightSavingIsActive ) {
								SetDSTDateRanges( Environment( Envrn ).MonWeekDay, DSTIndex );
							}
							SetSpecialDayDates( Environment( Envrn ).MonWeekDay );
						}
						++YearofSim;
						FirstSimDayofYear = 1;
						ReadWeatherForDay( FirstSimDayofYear, Envrn, false ); // Read tomorrow's weather
					} else {
						ReadWeatherForDay( DayOfSim + 1, Envrn, false ); // Read tomorrow's weather
					}
				}
			}

			if ( DayOfMonth == EndDayOfMonth( Month ) ) {
				EndMonthFlag = true;
			}

			// Set Tomorrow's date data
			MonthTomorrow = TomorrowVariables.Month;
			DayOfMonthTomorrow = TomorrowVariables.DayOfMonth;
			DayOfWeekTomorrow = TomorrowVariables.DayOfWeek;
			HolidayIndexTomorrow = TomorrowVariables.HolidayIndex;
			YearTomorrow = TomorrowVariables.Year;

			if ( Environment( Envrn ).KindOfEnvrn == ksRunPeriodWeather ) {
				if ( Month == 1 && DayOfMonth == 1 && Environment( Envrn ).ActualWeather ) {
					if ( DatesShouldBeReset ) {
						if ( Environment( Envrn ).TreatYearsAsConsecutive ) {
							++Environment( Envrn ).CurrentYear;
							Environment( Envrn ).IsLeapYear = IsLeapYear( Environment( Envrn ).CurrentYear );
							CurrentYearIsLeapYear = Environment( Envrn ).IsLeapYear;
							if ( CurrentYearIsLeapYear ) {
								if ( WFAllowsLeapYears ) {
									LeapYearAdd = 1;
								} else {
									LeapYearAdd = 0;
								}
							} else {
								LeapYearAdd = 0;
							}
							// need to reset MonWeekDay and WeekDayTypes
							if ( ! CurrentYearIsLeapYear ) {
								JDay5Start = JulianDay( Environment( Envrn ).StartMonth, Environment( Envrn ).StartDay, 0 );
								JDay5End = JulianDay( Environment( Envrn ).EndMonth, Environment( Envrn ).EndDay, 0 );
							} else {
								JDay5Start = JulianDay( Environment( Envrn ).StartMonth, Environment( Envrn ).StartDay, LeapYearAdd );
								JDay5End = JulianDay( Environment( Envrn ).EndMonth, Environment( Envrn ).EndDay, LeapYearAdd );
							}
							if ( ! Environment( Envrn ).ActualWeather ) curSimDayForEndOfRunPeriod = DayOfSim + Environment( Envrn ).RawSimDays + LeapYearAdd - 1;

							Loop = JDay5Start;
							TWeekDay = DayOfWeek;
							while ( true ) {
								WeekDayTypes( Loop ) = TWeekDay;
								TWeekDay = mod( TWeekDay, 7 ) + 1;
								++Loop;
								if ( Loop > 366 ) Loop = 1;
								if ( Loop == JDay5End ) break;
							}
							ResetWeekDaysByMonth( Environment( Envrn ).MonWeekDay, LeapYearAdd, Environment( Envrn ).StartMonth, Environment( Envrn ).StartDay, Environment( Envrn ).EndMonth, Environment( Envrn ).EndDay, Environment( Envrn ).RollDayTypeOnRepeat );
							if ( DaylightSavingIsActive ) {
								SetDSTDateRanges( Environment( Envrn ).MonWeekDay, DSTIndex );
							}
							SetSpecialDayDates( Environment( Envrn ).MonWeekDay );
						}
					}
				} else if ( ( Month == 1 && DayOfMonth == 1 ) && DatesShouldBeReset && ( Jan1DatesShouldBeReset ) ) {
					if ( Environment( Envrn ).TreatYearsAsConsecutive ) {
						++Environment( Envrn ).CurrentYear;
						Environment( Envrn ).IsLeapYear = IsLeapYear( Environment( Envrn ).CurrentYear );
						CurrentYearIsLeapYear = Environment( Envrn ).IsLeapYear;
						if ( CurrentYearIsLeapYear && ! WFAllowsLeapYears ) CurrentYearIsLeapYear = false;
						if ( DayOfSim < curSimDayForEndOfRunPeriod && CurrentYearIsLeapYear ) ++curSimDayForEndOfRunPeriod;
					}
					if ( CurrentYearIsLeapYear ) {
						if ( WFAllowsLeapYears ) {
							LeapYearAdd = 1;
						} else {
							LeapYearAdd = 0;
						}
					} else {
						LeapYearAdd = 0;
					}

					if ( DayOfSim < curSimDayForEndOfRunPeriod ) {
						if ( Environment( Envrn ).RollDayTypeOnRepeat || CurrentYearIsLeapYear ) {
							ResetWeekDaysByMonth( Environment( Envrn ).MonWeekDay, LeapYearAdd, Environment( Envrn ).StartMonth, Environment( Envrn ).StartDay, Environment( Envrn ).EndMonth, Environment( Envrn ).EndDay, Environment( Envrn ).RollDayTypeOnRepeat, true );
						} else {
							ResetWeekDaysByMonth( Environment( Envrn ).MonWeekDay, LeapYearAdd, Environment( Envrn ).StartMonth, Environment( Envrn ).StartDay, Environment( Envrn ).EndMonth, Environment( Envrn ).EndDay, Environment( Envrn ).RollDayTypeOnRepeat, false );
						}
						if ( DaylightSavingIsActive ) {
							SetDSTDateRanges( Environment( Envrn ).MonWeekDay, DSTIndex );
						}
						SetSpecialDayDates( Environment( Envrn ).MonWeekDay );
					}
				}
				//      SetYear=.FALSE.
			}
		} // ... end of BeginDayFlag IF-THEN block.

		if ( ! BeginDayFlag && ! WarmupFlag && ( Month != Environment( Envrn ).StartMonth || DayOfMonth != Environment( Envrn ).StartDay ) && ! DatesShouldBeReset && Environment( Envrn ).KindOfEnvrn == ksRunPeriodWeather ) {
			//    SetYear=.TRUE.
			DatesShouldBeReset = true;
		}

		if (EndEnvrnFlag && (Environment(Envrn).KindOfEnvrn != ksDesignDay) && (Environment(Envrn).KindOfEnvrn != ksHVACSizeDesignDay)) {
			gio::rewind( WeatherFileUnitNumber );
			SkipEPlusWFHeader();
			ReportMissing_RangeData();
		}

		// set the EndDesignDayEnvrnsFlag (dataGlobals)
		// True at the end of the last design day environment (last time step of last hour of last day of environ which is a design day)
		// added to address CR7562
		EndDesignDayEnvrnsFlag = false;
		if ( EndEnvrnFlag ) {
			if ( Envrn < NumOfEnvrn ) {
				if ( Environment( Envrn ).KindOfEnvrn != Environment( Envrn + 1 ).KindOfEnvrn ) {
					EndDesignDayEnvrnsFlag = true;
				}
			} else {
				// if the last environment set the flag to true.
				EndDesignDayEnvrnsFlag = true;
			}
		}

	}

	void
	UpdateWeatherData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates all of the daily weather data in the local
		// module level variables and the global variables.
		// This subroutine will temporarily transfer the weather data for the
		// current day to the old data structure contained in envdat.inc until
		// enough reengineering has taken place to eliminate the need for this
		// include.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		//unused          integer :: myhr

		TodayVariables = TomorrowVariables; // Transfer Tomorrow's Daily Weather Variables to Today

		if ( BeginEnvrnFlag ) {
			PreviousHour = 24;
		}

		TodayIsRain = TomorrowIsRain;
		TodayIsSnow = TomorrowIsSnow;
		TodayOutDryBulbTemp = TomorrowOutDryBulbTemp;
		TodayOutDewPointTemp = TomorrowOutDewPointTemp;
		TodayOutBaroPress = TomorrowOutBaroPress;
		TodayOutRelHum = TomorrowOutRelHum;
		TodayWindSpeed = TomorrowWindSpeed;
		TodayWindDir = TomorrowWindDir;
		TodaySkyTemp = TomorrowSkyTemp;
		TodayHorizIRSky = TomorrowHorizIRSky;
		TodayBeamSolarRad = TomorrowBeamSolarRad;
		TodayDifSolarRad = TomorrowDifSolarRad;
		TodayLiquidPrecip = TomorrowLiquidPrecip;

		// Update Global Data

		DayOfYear = TodayVariables.DayOfYear;
		Year = TodayVariables.Year;
		Month = TodayVariables.Month;
		DayOfMonth = TodayVariables.DayOfMonth;
		DayOfWeek = TodayVariables.DayOfWeek;
		//  WeekDayCount(DayOfWeek)=WeekDayCount(DayOfWeek)+1
		HolidayIndex = TodayVariables.HolidayIndex;
		if ( HolidayIndex > 0 ) {
			RptDayType = 7 + HolidayIndex;
		} else {
			RptDayType = DayOfWeek;
		}
		DSTIndicator = TodayVariables.DaylightSavingIndex;
		EquationOfTime = TodayVariables.EquationOfTime;
		CosSolarDeclinAngle = TodayVariables.CosSolarDeclinAngle;
		SinSolarDeclinAngle = TodayVariables.SinSolarDeclinAngle;

	}

	void
	SetCurrentWeather()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   March 1990
		//       MODIFIED       Aug94 (LKL) Fixed improper weighting
		//                      Nov98 (FCW) Added call to get exterior illuminances
		//                      Jan02 (FCW) Changed how ground reflectance for daylighting is set
		//                      Mar12 (LKL) Changed settings for leap years/ current years.
		//       RE-ENGINEERED  Apr97,May97 (RKS)

		// PURPOSE OF THIS SUBROUTINE:
		// The purpose of this subroutine is to interpolate the hourly
		// environment data for the sub-hourly time steps in EnergyPlus.  In
		// other words, this subroutine puts the current weather conditions
		// into the proper variables.  Rather than using the same data for
		// each time step, environment data is interpolated as a continuum
		// throughout the day.

		// METHODOLOGY EMPLOYED:
		// The current hour (HourOfDay) as well as the next hour are used
		// to come up with environment data per time step interval.  Method
		// used is to assign a weighting for the current hour's data and
		// (1-that weighting) to the next hour's data.  Actual method is:  if
		// the current time step is 15 minutes into hour, the interpolated dry
		// bulb temperature should be 3/4*dry bulb temperature of current hour
		// and 1/4*dry bulb temperature of next environment hourly data.  At
		// day boundary (current hour = 24), the next hour is hour 1 of next
		// weather data day (Tomorrow%).

		// REFERENCES:
		// INTERPOL(IBLAST) legacy code.

		// Using/Aliasing
		using General::JulianDay;
		using ScheduleManager::UpdateScheduleValues;
		using InputProcessor::SameString;
		using namespace GroundTemperatureManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static char time_stamp[ 10 ];
		static char day_stamp[ 6 ];
		static std::string const RoutineName( "SetCurrentWeather" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int NextHour;
		Real64 TempVal;
		Real64 TempDPVal;

		// FLOW:

		NextHour = HourOfDay + 1;

		if ( HourOfDay == 24 ) {
			NextHour = 1;
		}

		if ( HourOfDay == 1 ) {
			DayOfYear_Schedule = JulianDay( Month, DayOfMonth, 1 );
		}

		UpdateScheduleValues();

		std::sprintf( time_stamp, "%02d/%02d %02d:", Month, DayOfMonth, HourOfDay - 1 );
		CurMnDyHr = time_stamp;
		std::sprintf( day_stamp, "%02d/%02d", Month, DayOfMonth );
		CurMnDy = day_stamp;

		WeightNow = Interpolation( TimeStep );
		WeightPreviousHour = 1.0 - WeightNow;

		CurrentTime = ( HourOfDay - 1 ) + TimeStep * ( TimeStepFraction );
		SimTimeSteps = ( DayOfSim - 1 ) * 24 * NumOfTimeStepInHour + ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;

		GroundTemp = siteBuildingSurfaceGroundTempsPtr->getGroundTempAtTimeInMonths( 0, Month );
		GroundTempKelvin = GroundTemp + KelvinConv;
		GroundTempFC = siteFCFactorMethodGroundTempsPtr->getGroundTempAtTimeInMonths( 0, Month );
		GroundTemp_Surface = siteShallowGroundTempsPtr->getGroundTempAtTimeInMonths( 0, Month );
		GroundTemp_Deep = siteDeepGroundTempsPtr->getGroundTempAtTimeInMonths( 0, Month );
		GndReflectance = GroundReflectances( Month );
		GndReflectanceForDayltg = GndReflectance;

		CalcWaterMainsTemp();

		// Determine if Sun is up or down, set Solar Cosine values for time step.
		DetermineSunUpDown( SOLCOS );
		if ( SunIsUp && SolarAltitudeAngle < 0.0 ) {
			ShowFatalError( "SetCurrentWeather: At " + CurMnDyHr + " Sun is Up but Solar Altitude Angle is < 0.0" );
		}

		OutDryBulbTemp = TodayOutDryBulbTemp( TimeStep, HourOfDay );
		if ( EMSOutDryBulbOverrideOn ) OutDryBulbTemp = EMSOutDryBulbOverrideValue;
		OutBaroPress = TodayOutBaroPress( TimeStep, HourOfDay );
		OutDewPointTemp = TodayOutDewPointTemp( TimeStep, HourOfDay );
		if ( EMSOutDewPointTempOverrideOn ) OutDewPointTemp = EMSOutDewPointTempOverrideValue;
		OutRelHum = TodayOutRelHum( TimeStep, HourOfDay );
		OutRelHumValue = OutRelHum / 100.0;
		if ( EMSOutRelHumOverrideOn ) {
			OutRelHumValue = EMSOutRelHumOverrideValue / 100.0;
			OutRelHum = EMSOutRelHumOverrideValue;
		}

		// Humidity Ratio and Wet Bulb are derived
		OutHumRat = PsyWFnTdbRhPb( OutDryBulbTemp, OutRelHumValue, OutBaroPress, RoutineName );
		OutWetBulbTemp = PsyTwbFnTdbWPb( OutDryBulbTemp, OutHumRat, OutBaroPress );
		if ( OutDryBulbTemp < OutWetBulbTemp ) {
			OutWetBulbTemp = OutDryBulbTemp;
			TempVal = PsyWFnTdbTwbPb( OutDryBulbTemp, OutWetBulbTemp, OutBaroPress );
			TempDPVal = PsyTdpFnWPb( TempVal, OutBaroPress );
			OutDewPointTemp = TempDPVal;
		}

		if ( OutDewPointTemp > OutWetBulbTemp ) {
			OutDewPointTemp = OutWetBulbTemp;
		}

		if ( ( KindOfSim == ksDesignDay ) || ( KindOfSim == ksHVACSizeDesignDay ) ) {
			SPSiteDryBulbRangeModScheduleValue = -999.0; // N/A Drybulb Temperature Range Modifier Schedule Value
			SPSiteHumidityConditionScheduleValue = -999.0; // N/A Humidity Condition Schedule Value
			SPSiteBeamSolarScheduleValue = -999.0; // N/A Beam Solar Schedule Value
			SPSiteDiffuseSolarScheduleValue = -999.0; // N/A Diffuse Solar Schedule Value
			SPSiteSkyTemperatureScheduleValue = -999.0; // N/A SkyTemperature Modifier Schedule Value

			int const envrnDayNum( Environment( Envrn ).DesignDayNum );
			if ( DesDayInput( envrnDayNum ).DBTempRangeType != DDDBRangeType_Default ) {
				SPSiteDryBulbRangeModScheduleValue( envrnDayNum ) = DDDBRngModifier( TimeStep, HourOfDay, envrnDayNum );
			}
			int const humIndType( DesDayInput( envrnDayNum ).HumIndType );
			if ( humIndType == DDHumIndType_WBProfDef || humIndType == DDHumIndType_WBProfDif || humIndType == DDHumIndType_WBProfMul ) {
				SPSiteHumidityConditionScheduleValue( envrnDayNum ) = DDHumIndModifier( TimeStep, HourOfDay, envrnDayNum );
			} else if ( humIndType == DDHumIndType_RelHumSch ) {
				SPSiteHumidityConditionScheduleValue( envrnDayNum ) = DDHumIndModifier( TimeStep, HourOfDay, envrnDayNum );
			}
			if ( DesDayInput( envrnDayNum ).SolarModel == SolarModel_Schedule ) {
				SPSiteBeamSolarScheduleValue( envrnDayNum ) = DDBeamSolarValues( TimeStep, HourOfDay, envrnDayNum );
				SPSiteDiffuseSolarScheduleValue( envrnDayNum ) = DDDiffuseSolarValues( TimeStep, HourOfDay, envrnDayNum );
			}
			if ( Environment( Envrn ).WP_Type1 != 0 ) {
				SPSiteSkyTemperatureScheduleValue( envrnDayNum ) = DDSkyTempScheduleValues( TimeStep, HourOfDay, envrnDayNum );
			}
		} else if ( TotDesDays > 0 ) {
			SPSiteDryBulbRangeModScheduleValue = -999.0; // N/A Drybulb Temperature Range Modifier Schedule Value
			SPSiteHumidityConditionScheduleValue = -999.0; // N/A Humidity Condition Schedule Value
			SPSiteBeamSolarScheduleValue = -999.0; // N/A Beam Solar Schedule Value
			SPSiteDiffuseSolarScheduleValue = -999.0; // N/A Diffuse Solar Schedule Value
			SPSiteSkyTemperatureScheduleValue = -999.0; // N/A SkyTemperature Modifier Schedule Value
		}

		WindSpeed = TodayWindSpeed( TimeStep, HourOfDay );
		if ( EMSWindSpeedOverrideOn ) WindSpeed = EMSWindSpeedOverrideValue;
		WindDir = TodayWindDir( TimeStep, HourOfDay );
		if ( EMSWindDirOverrideOn ) WindDir = EMSWindDirOverrideValue;
		HorizIRSky = TodayHorizIRSky( TimeStep, HourOfDay );
		SkyTemp = TodaySkyTemp( TimeStep, HourOfDay );
		SkyTempKelvin = SkyTemp + KelvinConv;
		DifSolarRad = TodayDifSolarRad( TimeStep, HourOfDay );
		if ( EMSDifSolarRadOverrideOn ) DifSolarRad = EMSDifSolarRadOverrideValue;
		BeamSolarRad = TodayBeamSolarRad( TimeStep, HourOfDay );
		if ( EMSBeamSolarRadOverrideOn ) BeamSolarRad = EMSBeamSolarRadOverrideValue;
		LiquidPrecipitation = TodayLiquidPrecip( TimeStep, HourOfDay ) / 1000.0; // convert from mm to m

		if ( UseRainValues ) {
			IsRain = TodayIsRain( TimeStep, HourOfDay ); //.or. LiquidPrecipitation >= .8d0)  ! > .8 mm
		} else {
			IsRain = false;
		}
		if ( UseSnowValues ) {
			IsSnow = TodayIsSnow( TimeStep, HourOfDay );
		} else {
			IsSnow = false;
		}

		if ( IsSnow ) {
			GndReflectance = max( min( GndReflectance * SnowGndRefModifier, 1.0 ), 0.0 );
			GndReflectanceForDayltg = max( min( GndReflectanceForDayltg * SnowGndRefModifierForDayltg, 1.0 ), 0.0 );
		}

		GndSolarRad = max( ( BeamSolarRad * SOLCOS( 3 ) + DifSolarRad ) * GndReflectance, 0.0 );

		if ( ! SunIsUp ) {
			DifSolarRad = 0.0;
			BeamSolarRad = 0.0;
			GndSolarRad = 0.0;
		}

		// Calc some values
		OutEnthalpy = PsyHFnTdbW( OutDryBulbTemp, OutHumRat );
		OutAirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, OutDryBulbTemp, OutHumRat );

		// Make sure outwetbulbtemp is valid.  And that no error occurs here.
		if ( OutDryBulbTemp < OutWetBulbTemp ) OutWetBulbTemp = OutDryBulbTemp;

		//                                      VALIDITY TEST.
		if ( OutDewPointTemp > OutWetBulbTemp ) {
			OutDewPointTemp = OutWetBulbTemp;
		}
		// Get exterior daylight illuminance for daylighting calculation

		DayltgCurrentExtHorizIllum();

		if ( ! IsRain ) {
			RptIsRain = 0;
		} else {
			RptIsRain = 1;
		}

		if ( ! IsSnow ) {
			RptIsSnow = 0;
		} else {
			RptIsSnow = 1;
		}

	}

	void
	ReadWeatherForDay(
		int const DayToRead, // =1 when starting out, otherwise signifies next day
		int const Environ, // Environment being simulated
		bool const BackSpaceAfterRead // True if weather file is to be backspaced after read
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the driving routine behind reading the weather data.
		// Theoretically, several kinds of weather files could be read here.  As
		// distributed only EPW files are allowed.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		ReadEPlusWeatherForDay( DayToRead, Environ, BackSpaceAfterRead );

	}

	void
	ReadEPlusWeatherForDay(
		int const DayToRead, // =1 when starting out, otherwise signifies next day
		int const Environ, // Environment being simulated
		bool const BackSpaceAfterRead // True if weather file is to be backspaced after read
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 1999
		//       MODIFIED       March 2012; add actual weather read.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the appropriate day of EPW weather data.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::RangeCheck;
		using General::JulianDay;
		using General::RoundSigDigits;
		using ScheduleManager::GetScheduleValuesForDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );
		static gio::Fmt YMDHFmt( "(I4.4,2('/',I2.2),1X,I2.2,':',I2.2)" );
		static gio::Fmt YMDHFmt1( "(I4.4,2('/',I2.2),1X,'hour=',I2.2,' - expected hour=',I2.2)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Hour;
		int TS;
		int WYear;
		int WMonth;
		int WDay;
		int WHour;
		int WMinute;
		Real64 DryBulb;
		Real64 DewPoint;
		Real64 RelHum;
		Real64 AtmPress;
		Real64 ETHoriz;
		Real64 ETDirect;
		Real64 IRHoriz;
		Real64 GLBHoriz;
		Real64 DirectRad;
		Real64 DiffuseRad;
		Real64 GLBHorizIllum;
		Real64 DirectNrmIllum;
		Real64 DiffuseHorizIllum;
		Real64 ZenLum;
		Real64 WindDir;
		Real64 WindSpeed;
		Real64 TotalSkyCover;
		Real64 OpaqueSkyCover;
		Real64 Visibility;
		Real64 CeilHeight;
		Real64 PrecipWater;
		Real64 AerosolOptDepth;
		Real64 SnowDepth;
		Real64 DaysSinceLastSnow;
		Real64 Albedo;
		Real64 LiquidPrecip;
		int PresWeathObs;
		Array1D_int PresWeathConds( 9 );
		std::string WeatherDataLine;
		bool Ready;
		int CurTimeStep;
		int Item;

		Real64 A;
		Real64 B;
		Real64 C;
		Real64 AVSC;
		Real64 SkyTemp;
		static int CurDayOfWeek;
		static bool UseDayOfWeek;
		bool SkipThisDay; // Used when LeapYear is/is not in effect
		bool TryAgain;
		int ReadStatus;
		int NumRewinds;
		std::string BadRecord;
		bool ErrorsFound;
		static Real64 CurTime;
		Real64 HourRep;
		int OSky;
		Real64 TDewK;
		Real64 ESky;
		bool ErrorFound;
		std::string ErrOut;
		static bool LastHourSet; // for Interpolation
		int NxtHour;
		Real64 WtNow;
		Real64 WtPrevHour;
		Real64 WgtHourNow;
		Real64 WgtPrevHour;
		Real64 WgtNextHour;
		static Real64 LastHrOutDryBulbTemp;
		static Real64 LastHrOutDewPointTemp;
		static Real64 LastHrOutBaroPress;
		static Real64 LastHrOutRelHum;
		static Real64 LastHrWindSpeed;
		static Real64 LastHrWindDir;
		static Real64 LastHrSkyTemp;
		static Real64 LastHrHorizIRSky;
		static Real64 LastHrBeamSolarRad;
		static Real64 LastHrDifSolarRad;
		static Real64 LastHrAlbedo;
		static Real64 LastHrLiquidPrecip;
		static Real64 NextHrBeamSolarRad;
		static Real64 NextHrDifSolarRad;
		static Real64 NextHrLiquidPrecip;
		bool RecordDateMatch;

		struct HourlyWeatherData
		{
			// Members
			Array1D_bool IsRain; // Rain indicator, true=rain
			Array1D_bool IsSnow; // Snow indicator, true=snow
			Array1D< Real64 > OutDryBulbTemp; // Hourly dry bulb temperature of outside air
			Array1D< Real64 > OutDewPointTemp; // Hourly Dew Point Temperature of outside air
			Array1D< Real64 > OutBaroPress; // Hourly barometric pressure of outside air
			Array1D< Real64 > OutRelHum; // Hourly relative humidity
			Array1D< Real64 > WindSpeed; // Hourly wind speed of outside air
			Array1D< Real64 > WindDir; // Hourly wind direction of outside air
			Array1D< Real64 > SkyTemp; // Hourly sky temperature
			Array1D< Real64 > HorizIRSky; // Hourly Horizontal Infrared Radiation Intensity
			Array1D< Real64 > BeamSolarRad; // Hourly direct normal solar irradiance
			Array1D< Real64 > DifSolarRad; // Hourly sky diffuse horizontal solar irradiance
			Array1D< Real64 > Albedo; // Albedo
			Array1D< Real64 > LiquidPrecip; // Liquid Precipitation

			// Default Constructor
			HourlyWeatherData() :
				IsRain( 24, false ),
				IsSnow( 24, false ),
				OutDryBulbTemp( 24, 0.0 ),
				OutDewPointTemp( 24, 0.0 ),
				OutBaroPress( 24, 0.0 ),
				OutRelHum( 24, 0.0 ),
				WindSpeed( 24, 0.0 ),
				WindDir( 24, 0.0 ),
				SkyTemp( 24, 0.0 ),
				HorizIRSky( 24, 0.0 ),
				BeamSolarRad( 24, 0.0 ),
				DifSolarRad( 24, 0.0 ),
				Albedo( 24, 0.0 ),
				LiquidPrecip( 24, 0.0 )
			{}

		};

		// Object Data
		HourlyWeatherData Wthr;

		if ( DayToRead == 1 ) {

			// Checks whether Weather file contains just one year of data. If yes then rewind and position to first
			// day of weather file. The rest of code appropriately positions to the start day.

			Ready = false;
			NumRewinds = 0;
			//     Must position file to proper day
			//     File already position to first data record
			//          Set Current Day of Week to "start of Data Period"
			CurTime = 1.0 / double( NumIntervalsPerHour );
			CurDayOfWeek = DataPeriods( 1 ).WeekDay - 1;
			WYear = 0;
			WMonth = 0;
			WDay = 0;
			WHour = 0;
			WMinute = 0;
			LastHourSet = false;
			while ( ! Ready ) {
				{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> WeatherDataLine; ReadStatus = flags.ios(); }
				if ( ReadStatus == 0 ) {
					// Reduce ugly code
					InterpretWeatherDataLine( WeatherDataLine, ErrorFound, WYear, WMonth, WDay, WHour, WMinute, DryBulb, DewPoint, RelHum, AtmPress, ETHoriz, ETDirect, IRHoriz, GLBHoriz, DirectRad, DiffuseRad, GLBHorizIllum, DirectNrmIllum, DiffuseHorizIllum, ZenLum, WindDir, WindSpeed, TotalSkyCover, OpaqueSkyCover, Visibility, CeilHeight, PresWeathObs, PresWeathConds, PrecipWater, AerosolOptDepth, SnowDepth, DaysSinceLastSnow, Albedo, LiquidPrecip );
				} else if ( ReadStatus < 0 ) {
					if ( NumRewinds > 0 ) {
						ShowSevereError( "Multiple rewinds on EPW while searching for first day" );
					} else {
						gio::rewind( WeatherFileUnitNumber );
						++NumRewinds;
						SkipEPlusWFHeader();
						{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> WeatherDataLine; ReadStatus = flags.ios(); }
						InterpretWeatherDataLine( WeatherDataLine, ErrorFound, WYear, WMonth, WDay, WHour, WMinute, DryBulb, DewPoint, RelHum, AtmPress, ETHoriz, ETDirect, IRHoriz, GLBHoriz, DirectRad, DiffuseRad, GLBHorizIllum, DirectNrmIllum, DiffuseHorizIllum, ZenLum, WindDir, WindSpeed, TotalSkyCover, OpaqueSkyCover, Visibility, CeilHeight, PresWeathObs, PresWeathConds, PrecipWater, AerosolOptDepth, SnowDepth, DaysSinceLastSnow, Albedo, LiquidPrecip );

					}
				}
				if ( ReadStatus != 0 ) {
					BadRecord = RoundSigDigits( WYear ) + '/' + RoundSigDigits( WMonth ) + '/' + RoundSigDigits( WDay ) + BlankString + RoundSigDigits( WHour ) + ':' + RoundSigDigits( WMinute );
					gio::write( ErrOut, fmtLD ) << ReadStatus;
					strip( ErrOut );
					ShowFatalError( "Error occured on EPW while searching for first day, stopped at " + BadRecord + " IO Error=" + RoundSigDigits( ReadStatus ), OutputFileStandard );
				}
				if ( CurDayOfWeek <= 7 ) {
					CurDayOfWeek = mod( CurDayOfWeek, 7 ) + 1;
				}
				if ( WMonth == Environment( Environ ).StartMonth && WDay == Environment( Environ ).StartDay && ! Environment( Environ ).MatchYear ) {
					RecordDateMatch = true;
				} else if ( WMonth == Environment( Environ ).StartMonth && WDay == Environment( Environ ).StartDay && Environment( Environ ).MatchYear && WYear == Environment( Environ ).StartYear ) {
					RecordDateMatch = true;
				} else {
					RecordDateMatch = false;
				}
				if ( RecordDateMatch ) {
					gio::backspace( WeatherFileUnitNumber );
					Ready = true;
					if ( CurDayOfWeek <= 7 ) {
						--CurDayOfWeek;
					}
					// Do the range checks on the first set of fields -- no others.
					ErrorsFound = false;
					if ( DryBulb >= 99.9 ) RangeCheck( ErrorsFound, "DryBulb Temperature", "WeatherFile", "Severe", ">= -90", ( DryBulb >= -90.0 ), "<= 70", ( DryBulb <= 70.0 ), RoundSigDigits( DryBulb, 2 ), WeatherFileLocationTitle );
					if ( DewPoint < 99.9 ) RangeCheck( ErrorsFound, "DewPoint Temperature", "WeatherFile", "Severe", ">= -90", ( DewPoint >= -90.0 ), "<= 70", ( DewPoint <= 70.0 ), RoundSigDigits( DewPoint, 2 ), WeatherFileLocationTitle );
					if ( RelHum < 999.0 ) RangeCheck( ErrorsFound, "Relative Humidity", "WeatherFile", "Severe", "> 0", ( RelHum >= 0.0 ), "<= 110", ( RelHum <= 110.0 ), RoundSigDigits( RelHum, 0 ), WeatherFileLocationTitle );
					if ( AtmPress < 999999.0 ) RangeCheck( ErrorsFound, "Atmospheric Pressure", "WeatherFile", "Severe", "> 31000", ( AtmPress > 31000.0 ), "<=120000", ( AtmPress <= 120000.0 ), RoundSigDigits( AtmPress, 0 ), WeatherFileLocationTitle );
					if ( DirectRad < 9999.0 ) RangeCheck( ErrorsFound, "Direct Radiation", "WeatherFile", "Severe", ">= 0", ( DirectRad >= 0.0 ), _, _, _, WeatherFileLocationTitle );
					if ( DiffuseRad < 9999.0 ) RangeCheck( ErrorsFound, "Diffuse Radiation", "WeatherFile", "Severe", ">= 0", ( DiffuseRad >= 0.0 ), _, _, _, WeatherFileLocationTitle );
					if ( WindDir < 999.0 ) RangeCheck( ErrorsFound, "Wind Direction", "WeatherFile", "Severe", ">=0", ( WindDir >= 0.0 ), "<=360", ( WindDir <= 360.0 ), RoundSigDigits( WindDir, 0 ), WeatherFileLocationTitle );
					if ( WindSpeed < 999.0 ) RangeCheck( ErrorsFound, "Wind Speed", "WeatherFile", "Severe", ">=0", ( WindSpeed >= 0.0 ), "<=40", ( WindSpeed <= 40.0 ), RoundSigDigits( WindSpeed, 2 ), WeatherFileLocationTitle );
					if ( ErrorsFound ) {
						ShowSevereError( "Out of Range errors found with initial day of WeatherFile" );
					}
				} else {
					//  Must skip this day
					for ( Item = 2; Item <= NumIntervalsPerHour; ++Item ) {
						{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> WeatherDataLine; ReadStatus = flags.ios(); }
						if ( ReadStatus != 0 ) {
							gio::read( WeatherDataLine, fmtLD ) >> WYear >> WMonth >> WDay >> WHour >> WMinute;
							BadRecord = RoundSigDigits( WYear ) + '/' + RoundSigDigits( WMonth ) + '/' + RoundSigDigits( WDay ) + BlankString + RoundSigDigits( WHour ) + ':' + RoundSigDigits( WMinute );
							ShowFatalError( "Error occured on EPW while searching for first day, stopped at " + BadRecord + " IO Error=" + RoundSigDigits( ReadStatus ), OutputFileStandard );
						}
					}
					for ( Item = 1; Item <= 23 * NumIntervalsPerHour; ++Item ) {
						{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> WeatherDataLine; ReadStatus = flags.ios(); }
						if ( ReadStatus != 0 ) {
							gio::read( WeatherDataLine, fmtLD ) >> WYear >> WMonth >> WDay >> WHour >> WMinute;
							BadRecord = RoundSigDigits( WYear ) + '/' + RoundSigDigits( WMonth ) + '/' + RoundSigDigits( WDay ) + BlankString + RoundSigDigits( WHour ) + ':' + RoundSigDigits( WMinute );
							ShowFatalError( "Error occured on EPW while searching for first day, stopped at " + BadRecord + " IO Error=" + RoundSigDigits( ReadStatus ), OutputFileStandard );
						}
					}
				}
			}

			// Positioned to proper day
			if ( ! KickOffSimulation && ! DoingSizing && Environment( Environ ).KindOfEnvrn == ksRunPeriodWeather ) {
				++Environment( Environ ).CurrentCycle;
				if ( ! Environment( Environ ).RollDayTypeOnRepeat ) {
					SetDayOfWeekInitialValues( Environment( Environ ).DayOfWeek, CurDayOfWeek, UseDayOfWeek );
					if ( DaylightSavingIsActive ) {
						SetDSTDateRanges( Environment( Envrn ).MonWeekDay, DSTIndex );
					}
					SetSpecialDayDates( Environment( Envrn ).MonWeekDay );
				} else if ( Environment( Environ ).CurrentCycle == 1 ) {
					SetDayOfWeekInitialValues( Environment( Environ ).DayOfWeek, CurDayOfWeek, UseDayOfWeek );
					Environment( Environ ).SetWeekDays = true;
					if ( DaylightSavingIsActive ) {
						SetDSTDateRanges( Environment( Envrn ).MonWeekDay, DSTIndex );
					}
					SetSpecialDayDates( Environment( Envrn ).MonWeekDay );
				} else {
					CurDayOfWeek = DayOfWeekTomorrow;
				}
			} else {
				SetDayOfWeekInitialValues( Environment( Environ ).DayOfWeek, CurDayOfWeek, UseDayOfWeek );
			}
		}

		TryAgain = true;
		SkipThisDay = false;

		while ( TryAgain ) {

			TryAgain = false;

			TomorrowOutDryBulbTemp = 0.0;
			TomorrowOutDewPointTemp = 0.0;
			TomorrowOutBaroPress = 0.0;
			TomorrowOutRelHum = 0.0;
			TomorrowWindSpeed = 0.0;
			TomorrowWindDir = 0.0;
			TomorrowSkyTemp = 0.0;
			TomorrowHorizIRSky = 0.0;
			TomorrowBeamSolarRad = 0.0;
			TomorrowDifSolarRad = 0.0;
			TomorrowAlbedo = 0.0;
			TomorrowLiquidPrecip = 0.0;
			TomorrowIsRain = false;
			TomorrowIsSnow = false;

			for ( Hour = 1; Hour <= 24; ++Hour ) {
				for ( CurTimeStep = 1; CurTimeStep <= NumIntervalsPerHour; ++CurTimeStep ) {
					HourRep = double( Hour - 1 ) + ( CurTime * double( CurTimeStep ) );
					{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> WeatherDataLine; ReadStatus = flags.ios(); }
					if ( ReadStatus != 0 ) WeatherDataLine = BlankString;
					if ( WeatherDataLine == BlankString ) {
						if ( Hour == 1 ) {
							ReadStatus = -1;
						} else {
							ReadStatus = 99;
						}
					}
					if ( ReadStatus == 0 ) {
						InterpretWeatherDataLine( WeatherDataLine, ErrorFound, WYear, WMonth, WDay, WHour, WMinute, DryBulb, DewPoint, RelHum, AtmPress, ETHoriz, ETDirect, IRHoriz, GLBHoriz, DirectRad, DiffuseRad, GLBHorizIllum, DirectNrmIllum, DiffuseHorizIllum, ZenLum, WindDir, WindSpeed, TotalSkyCover, OpaqueSkyCover, Visibility, CeilHeight, PresWeathObs, PresWeathConds, PrecipWater, AerosolOptDepth, SnowDepth, DaysSinceLastSnow, Albedo, LiquidPrecip );
					} else { // ReadStatus /=0
						if ( ReadStatus < 0 && NumDataPeriods == 1 ) { // Standard End-of-file, rewind and position to first day...
							if ( DataPeriods( 1 ).NumDays >= NumDaysInYear ) {
								gio::rewind( WeatherFileUnitNumber );
								SkipEPlusWFHeader();
								{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> WeatherDataLine; ReadStatus = flags.ios(); }

								InterpretWeatherDataLine( WeatherDataLine, ErrorFound, WYear, WMonth, WDay, WHour, WMinute, DryBulb, DewPoint, RelHum, AtmPress, ETHoriz, ETDirect, IRHoriz, GLBHoriz, DirectRad, DiffuseRad, GLBHorizIllum, DirectNrmIllum, DiffuseHorizIllum, ZenLum, WindDir, WindSpeed, TotalSkyCover, OpaqueSkyCover, Visibility, CeilHeight, PresWeathObs, PresWeathConds, PrecipWater, AerosolOptDepth, SnowDepth, DaysSinceLastSnow, Albedo, LiquidPrecip );
							} else {
								BadRecord = RoundSigDigits( WYear ) + '/' + RoundSigDigits( WMonth ) + '/' + RoundSigDigits( WDay ) + BlankString + RoundSigDigits( WHour ) + ':' + RoundSigDigits( WMinute );
								ShowFatalError( "End-of-File encountered after " + BadRecord + ", starting from first day of Weather File would not be \"next day\"" );
							}
						} else {
							BadRecord = RoundSigDigits( WYear ) + '/' + RoundSigDigits( WMonth ) + '/' + RoundSigDigits( WDay ) + BlankString + RoundSigDigits( WHour ) + ':' + RoundSigDigits( WMinute );
							ShowFatalError( "Unexpected error condition in middle of reading EPW file, stopped at " + BadRecord, OutputFileStandard );
						}
					}

					if ( Hour != WHour ) {
						BadRecord = RoundSigDigits( WYear ) + '/' + RoundSigDigits( WMonth ) + '/' + RoundSigDigits( WDay ) + BlankString + RoundSigDigits( WHour ) + ':' + RoundSigDigits( WMinute );
						ShowFatalError( "Unexpected error condition in middle of reading EPW file, " + BadRecord, OutputFileStandard );
					}

					//         Set possible missing values
					if ( ETHoriz < 0.0 ) ETHoriz = 9999.0;
					if ( ETDirect < 0.0 ) ETDirect = 9999.0;
					if ( IRHoriz <= 0.0 ) IRHoriz = 9999.0;
					if ( GLBHoriz < 0.0 ) GLBHoriz = 9999.0;
					if ( DisplayWeatherMissingDataWarnings ) {
						if ( DirectRad >= 9999.0 ) {
							++Missed.DirectRad;
						}
						if ( DiffuseRad >= 9999.0 ) {
							Missed.DiffuseRad = Missed.DirectRad + 1;
						}
						if ( DirectRad < 0.0 ) {
							DirectRad = 9999.0;
							++OutOfRange.DirectRad;
						}
						if ( DiffuseRad < 0.0 ) {
							DiffuseRad = 9999.0;
							++OutOfRange.DiffuseRad;
						}
					}
					if ( GLBHorizIllum < 0.0 ) GLBHorizIllum = 999999.0;
					if ( DirectNrmIllum < 0.0 ) DirectNrmIllum = 999999.0;
					if ( DiffuseHorizIllum < 0.0 ) DiffuseHorizIllum = 999999.0;
					if ( ZenLum < 0.0 ) ZenLum = 99999.0;
					if ( AtmPress < 0.0 ) AtmPress = 999999.0;
					if ( WindSpeed < 0.0 ) WindSpeed = 999.0;
					if ( WindDir < -360.0 || WindDir > 360.0 ) WindDir = 999.0;
					if ( TotalSkyCover < 0.0 ) TotalSkyCover = 99.0;
					if ( RelHum < 0.0 ) RelHum = 999.0;
					if ( OpaqueSkyCover < 0.0 ) OpaqueSkyCover = 99.0;
					if ( Visibility < 0.0 ) Visibility = 9999.0;
					if ( CeilHeight < 0.0 ) CeilHeight = 9999.0;
					if ( PresWeathObs < 0 ) PresWeathObs = 9.0;
					if ( PrecipWater < 0.0 ) PrecipWater = 999.0;
					if ( AerosolOptDepth < 0.0 ) AerosolOptDepth = 999.0;
					if ( SnowDepth < 0.0 ) SnowDepth = 999.0;
					if ( DaysSinceLastSnow < 0.0 ) DaysSinceLastSnow = 99.0;
					if ( Albedo < 0.0 ) Albedo = 999.0;
					if ( LiquidPrecip < 0.0 ) LiquidPrecip = 999.0;

					if ( Hour == 1 && CurTimeStep == 1 ) {
						if ( WMonth == 2 && WDay == 29 && ( ! CurrentYearIsLeapYear || ! WFAllowsLeapYears ) ) {
							EndDayOfMonth( 2 ) = 28;
							SkipThisDay = true;
							TryAgain = true;
							ShowWarningError( "ReadEPlusWeatherForDay: Feb29 data encountered but will not be processed." );
							if ( ! WFAllowsLeapYears ) {
								ShowContinueError( "...WeatherFile does not allow Leap Years. HOLIDAYS/DAYLIGHT SAVINGS header must indicate \"Yes\"." );
							}
							continue;
						} else if ( WMonth == 2 && WDay == 29 && CurrentYearIsLeapYear && WFAllowsLeapYears ) {
							TryAgain = false;
							SkipThisDay = false;
						} else {
							TryAgain = false;
							SkipThisDay = false;
						}

						TomorrowVariables.Year = WYear;
						TomorrowVariables.Month = WMonth;
						TomorrowVariables.DayOfMonth = WDay;
						TomorrowVariables.DayOfYear = JulianDay( WMonth, WDay, LeapYearAdd );
						TomorrowVariables.DayOfYear_Schedule = JulianDay(WMonth, WDay, 1);
						CalculateDailySolarCoeffs( TomorrowVariables.DayOfYear, A, B, C, AVSC, TomorrowVariables.EquationOfTime, TomorrowVariables.SinSolarDeclinAngle, TomorrowVariables.CosSolarDeclinAngle );
						if ( CurDayOfWeek <= 7 ) {
							CurDayOfWeek = mod( CurDayOfWeek, 7 ) + 1;
						}
						TomorrowVariables.DayOfWeek = CurDayOfWeek;
						TomorrowVariables.DaylightSavingIndex = DSTIndex( TomorrowVariables.DayOfYear );
						TomorrowVariables.HolidayIndex = SpecialDayTypes( TomorrowVariables.DayOfYear );
					}

					if ( SkipThisDay ) continue;

					// Check out missing values

					if ( DryBulb >= 99.9 ) {
						DryBulb = Missing.DryBulb;
						++Missed.DryBulb;
					}
					if ( DryBulb < -90.0 || DryBulb > 70.0 ) {
						++OutOfRange.DryBulb;
					}

					if ( DewPoint >= 99.9 ) {
						DewPoint = Missing.DewPoint;
						++Missed.DewPoint;
					}
					if ( DewPoint < -90.0 || DewPoint > 70.0 ) {
						++OutOfRange.DewPoint;
					}

					if ( RelHum >= 999.0 ) {
						RelHum = Missing.RelHumid;
						++Missed.RelHumid;
					}
					if ( RelHum < 0.0 || RelHum > 110.0 ) {
						++OutOfRange.RelHumid;
					}

					if ( AtmPress >= 999999.0 ) {
						AtmPress = Missing.StnPres;
						++Missed.StnPres;
					}
					if ( AtmPress <= 31000.0 || AtmPress > 120000.0 ) {
						++OutOfRange.StnPres;
						AtmPress = Missing.StnPres;
					}

					if ( WindDir >= 999.0 ) {
						WindDir = Missing.WindDir;
						++Missed.WindDir;
					}
					if ( WindDir < 0.0 || WindDir > 360.0 ) {
						++OutOfRange.WindDir;
					}

					if ( WindSpeed >= 999.0 ) {
						WindSpeed = Missing.WindSpd;
						++Missed.WindSpd;
					}
					if ( WindSpeed < 0.0 || WindSpeed > 40.0 ) {
						++OutOfRange.WindSpd;
					}

					if ( TotalSkyCover >= 99.0 ) {
						TotalSkyCover = Missing.TotSkyCvr;
						++Missed.TotSkyCvr;
					}

					if ( OpaqueSkyCover >= 99.0 ) {
						OpaqueSkyCover = Missing.OpaqSkyCvr;
						++Missed.OpaqSkyCvr;
					}

					// Some values are not used within EnergyPlus, don't keep stats on their missing data points.

					//        IF (Visibility >= 9999.0d0) THEN
					//          Visibility=Missing%Visibility
					//          Missed%Visibility=Missed%Visibility+1
					//        ENDIF

					//        IF (CeilHeight >= 99999.0d0) THEN
					//          CeilHeight=Missing%Ceiling
					//         Missed%Ceiling=Missed%Ceiling+1
					//        ENDIF

					//        IF (PrecipWater >= 999.0d0) THEN
					//          PrecipWater=Missing%PrecipWater
					//          Missed%PrecipWater=Missed%PrecipWater+1
					//        ENDIF

					//        IF (AerosolOptDepth >= 0.999d0) THEN
					//          AerosolOptDepth=Missing%AerOptDepth
					//         Missed%AerOptDepth=Missed%AerOptDepth+1
					//        ENDIF

					if ( SnowDepth >= 999.0 ) {
						SnowDepth = Missing.SnowDepth;
						++Missed.SnowDepth;
					}

					if ( Albedo >= 999.0 ) {
						Albedo = Missing.Albedo;
						++Missed.Albedo;
					}

					if ( LiquidPrecip >= 999.0 ) {
						LiquidPrecip = Missing.LiquidPrecip;
						++Missed.LiquidPrecip;
					}

					//        IF (DaysSinceLastSnow >= 99) THEN
					//          DaysSinceLastSnow=Missing%DaysLastSnow
					//          Missed%DaysLastSnow=Missed%DaysLastSnow+1
					//        ENDIF

					TomorrowOutDryBulbTemp( CurTimeStep, Hour ) = DryBulb;
					TomorrowOutDewPointTemp( CurTimeStep, Hour ) = DewPoint;
					TomorrowOutBaroPress( CurTimeStep, Hour ) = AtmPress;
					TomorrowOutRelHum( CurTimeStep, Hour ) = RelHum;
					RelHum *= 0.01;
					TomorrowWindSpeed( CurTimeStep, Hour ) = WindSpeed;
					TomorrowWindDir( CurTimeStep, Hour ) = WindDir;
					TomorrowLiquidPrecip( CurTimeStep, Hour ) = LiquidPrecip;
					TomorrowHorizIRSky( CurTimeStep, Hour ) = IRHoriz;

					if ( Environment( Envrn ).WP_Type1 == 0 ) {
						// Calculate sky temperature, use IRHoriz if not missing
						if ( IRHoriz >= 9999.0 ) {
							// Missing, use sky cover
							OSky = OpaqueSkyCover;
							TDewK = min( DryBulb, DewPoint ) + TKelvin;
							ESky = ( 0.787 + 0.764 * std::log( TDewK / TKelvin ) ) * ( 1.0 + 0.0224 * OSky - 0.0035 * pow_2( OSky ) + 0.00028 * pow_3( OSky ) );
							SkyTemp = ( DryBulb + TKelvin ) * root_4( ESky ) - TKelvin;
						} else { // Valid IR from Sky
							SkyTemp = root_4( IRHoriz / Sigma ) - TKelvin;
						}
					} else {
						SkyTemp = 0.0; // dealt with later
					}

					TomorrowSkyTemp( CurTimeStep, Hour ) = SkyTemp;

					if ( ETHoriz >= 9999.0 ) ETHoriz = 0.0;
					if ( ETDirect >= 9999.0 ) ETDirect = 0.0;
					if ( GLBHoriz >= 9999.0 ) GLBHoriz = 0.0;
					if ( DirectRad >= 9999.0 ) DirectRad = 0.0;
					if ( DiffuseRad >= 9999.0 ) DiffuseRad = 0.0;
					if ( GLBHorizIllum >= 999900.0 ) GLBHorizIllum = 0.0;
					if ( DirectNrmIllum >= 999900.0 ) DirectNrmIllum = 0.0;
					if ( DiffuseHorizIllum >= 999900.0 ) DiffuseHorizIllum = 0.0;
					if ( ZenLum >= 99990.0 ) ZenLum = 0.0;
					if ( IgnoreSolarRadiation ) {
						GLBHoriz = 0.0;
						DirectRad = 0.0;
						DiffuseRad = 0.0;
					}
					if ( IgnoreBeamRadiation ) {
						DirectRad = 0.0;
					}
					if ( IgnoreDiffuseRadiation ) {
						DiffuseRad = 0.0;
					}

					TomorrowBeamSolarRad( CurTimeStep, Hour ) = DirectRad;
					TomorrowDifSolarRad( CurTimeStep, Hour ) = DiffuseRad;

					TomorrowIsRain( CurTimeStep, Hour ) = false;
					if ( PresWeathObs == 0 ) {
						if ( PresWeathConds( 1 ) < 9 || PresWeathConds( 2 ) < 9 || PresWeathConds( 3 ) < 9 ) TomorrowIsRain( CurTimeStep, Hour ) = true;
					} else {
						TomorrowIsRain( CurTimeStep, Hour ) = false;
					}
					TomorrowIsSnow( CurTimeStep, Hour ) = ( SnowDepth > 0.0 );

					// default if rain but none on weather file
					if ( TomorrowIsRain( CurTimeStep, Hour ) && TomorrowLiquidPrecip( CurTimeStep, Hour ) == 0.0 ) TomorrowLiquidPrecip( CurTimeStep, Hour ) = 2.0; // 2mm in an hour ~ .08 inch

					Missing.DryBulb = DryBulb;
					Missing.DewPoint = DewPoint;
					Missing.RelHumid = RelHum * 100.0;
					Missing.StnPres = AtmPress;
					Missing.WindDir = WindDir;
					Missing.WindSpd = WindSpeed;
					Missing.TotSkyCvr = TotalSkyCover;
					Missing.OpaqSkyCvr = OpaqueSkyCover;
					Missing.Visibility = Visibility;
					Missing.Ceiling = CeilHeight;
					Missing.PrecipWater = PrecipWater;
					Missing.AerOptDepth = AerosolOptDepth;
					Missing.SnowDepth = SnowDepth;
					Missing.DaysLastSnow = DaysSinceLastSnow;
					Missing.Albedo = Albedo;
					//        Missing%LiquidPrecip=LiquidPrecip

				} // CurTimeStep Loop

			} // Hour Loop

		} // Try Again While Loop

		if ( BackSpaceAfterRead ) {
			gio::backspace( WeatherFileUnitNumber );
		}

		if ( NumIntervalsPerHour == 1 && NumOfTimeStepInHour > 1 ) {
			// Create interpolated weather for timestep orientation
			// First copy ts=1 (hourly) from data arrays to Wthr structure
			for ( Hour = 1; Hour <= 24; ++Hour ) {
				Wthr.OutDryBulbTemp( Hour ) = TomorrowOutDryBulbTemp( 1, Hour );
				Wthr.OutDewPointTemp( Hour ) = TomorrowOutDewPointTemp( 1, Hour );
				Wthr.OutBaroPress( Hour ) = TomorrowOutBaroPress( 1, Hour );
				Wthr.OutRelHum( Hour ) = TomorrowOutRelHum( 1, Hour );
				Wthr.WindSpeed( Hour ) = TomorrowWindSpeed( 1, Hour );
				Wthr.WindDir( Hour ) = TomorrowWindDir( 1, Hour );
				Wthr.SkyTemp( Hour ) = TomorrowSkyTemp( 1, Hour );
				Wthr.HorizIRSky( Hour ) = TomorrowHorizIRSky( 1, Hour );
				Wthr.BeamSolarRad( Hour ) = TomorrowBeamSolarRad( 1, Hour );
				Wthr.DifSolarRad( Hour ) = TomorrowDifSolarRad( 1, Hour );
				Wthr.IsRain( Hour ) = TomorrowIsRain( 1, Hour );
				Wthr.IsSnow( Hour ) = TomorrowIsSnow( 1, Hour );
				Wthr.Albedo( Hour ) = TomorrowAlbedo( 1, Hour );
				Wthr.LiquidPrecip( Hour ) = TomorrowLiquidPrecip( 1, Hour );
			}

			if ( ! LastHourSet ) {
				// For first day of weather, all time steps of the first hour will be
				// equal to the first hour's value.
				LastHrOutDryBulbTemp = Wthr.OutDryBulbTemp( 24 );
				LastHrOutDewPointTemp = Wthr.OutDewPointTemp( 24 );
				LastHrOutBaroPress = Wthr.OutBaroPress( 24 );
				LastHrOutRelHum = Wthr.OutRelHum( 24 );
				LastHrWindSpeed = Wthr.WindSpeed( 24 );
				LastHrWindDir = Wthr.WindDir( 24 );
				LastHrSkyTemp = Wthr.SkyTemp( 24 );
				LastHrHorizIRSky = Wthr.HorizIRSky( 24 );
				LastHrBeamSolarRad = Wthr.BeamSolarRad( 24 );
				LastHrDifSolarRad = Wthr.DifSolarRad( 24 );
				LastHrAlbedo = Wthr.Albedo( 24 );
				LastHrLiquidPrecip = Wthr.LiquidPrecip( 24 );
				LastHourSet = true;
			}

			for ( Hour = 1; Hour <= 24; ++Hour ) {

				NxtHour = Hour + 1;
				if ( Hour == 24 ) {
					NxtHour = 1;
				}
				NextHrBeamSolarRad = Wthr.BeamSolarRad( NxtHour );
				NextHrDifSolarRad = Wthr.DifSolarRad( NxtHour );
				NextHrLiquidPrecip = Wthr.LiquidPrecip( NxtHour );

				for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {

					WtNow = Interpolation( TS );
					WtPrevHour = 1.0 - WtNow;

					// Do Solar "weighting"

					WgtHourNow = SolarInterpolation( TS );

					if ( NumOfTimeStepInHour == 1 ) {
						WgtNextHour = 1.0 - WgtHourNow;
						WgtPrevHour = 0.0;
					} else {
						if ( WgtHourNow == 1.0 ) {
							//  It's at the half hour
							WgtNextHour = 0.0;
							WgtPrevHour = 0.0;
						} else if ( TS * TimeStepFraction < 0.5 ) {
							WgtNextHour = 0.0;
							WgtPrevHour = 1.0 - WgtHourNow;
						} else { // After the half hour
							WgtPrevHour = 0.0;
							WgtNextHour = 1.0 - WgtHourNow;
						}
					}

					TomorrowOutDryBulbTemp( TS, Hour ) = LastHrOutDryBulbTemp * WtPrevHour + Wthr.OutDryBulbTemp( Hour ) * WtNow;
					TomorrowOutBaroPress( TS, Hour ) = LastHrOutBaroPress * WtPrevHour + Wthr.OutBaroPress( Hour ) * WtNow;
					TomorrowOutDewPointTemp( TS, Hour ) = LastHrOutDewPointTemp * WtPrevHour + Wthr.OutDewPointTemp( Hour ) * WtNow;
					TomorrowOutRelHum( TS, Hour ) = LastHrOutRelHum * WtPrevHour + Wthr.OutRelHum( Hour ) * WtNow;
					TomorrowWindSpeed( TS, Hour ) = LastHrWindSpeed * WtPrevHour + Wthr.WindSpeed( Hour ) * WtNow;
					TomorrowWindDir( TS, Hour ) = LastHrWindDir * WtPrevHour + Wthr.WindDir( Hour ) * WtNow;
					TomorrowHorizIRSky( TS, Hour ) = LastHrHorizIRSky * WtPrevHour + Wthr.HorizIRSky( Hour ) * WtNow;
					if ( Environment( Environ ).WP_Type1 == 0 ) {
						TomorrowSkyTemp( TS, Hour ) = LastHrSkyTemp * WtPrevHour + Wthr.SkyTemp( Hour ) * WtNow;
					}
					TomorrowDifSolarRad( TS, Hour ) = LastHrDifSolarRad * WgtPrevHour + Wthr.DifSolarRad( Hour ) * WgtHourNow + NextHrDifSolarRad * WgtNextHour;
					TomorrowBeamSolarRad( TS, Hour ) = LastHrBeamSolarRad * WgtPrevHour + Wthr.BeamSolarRad( Hour ) * WgtHourNow + NextHrBeamSolarRad * WgtNextHour;

					TomorrowLiquidPrecip( TS, Hour ) = LastHrLiquidPrecip * WtPrevHour + Wthr.LiquidPrecip( Hour ) * WtNow;
					TomorrowLiquidPrecip( TS, Hour ) /= double( NumOfTimeStepInHour );

					TomorrowIsRain( TS, Hour ) = TomorrowLiquidPrecip( TS, Hour ) >= ( 0.8 / double( NumOfTimeStepInHour ) ); //Wthr%IsRain(Hour)
					TomorrowIsSnow( TS, Hour ) = Wthr.IsSnow( Hour );
				} // End of TS Loop

				LastHrOutDryBulbTemp = Wthr.OutDryBulbTemp( Hour );
				LastHrOutDewPointTemp = Wthr.OutDewPointTemp( Hour );
				LastHrOutBaroPress = Wthr.OutBaroPress( Hour );
				LastHrOutRelHum = Wthr.OutRelHum( Hour );
				LastHrWindSpeed = Wthr.WindSpeed( Hour );
				LastHrWindDir = Wthr.WindDir( Hour );
				LastHrHorizIRSky = Wthr.HorizIRSky( Hour );
				LastHrSkyTemp = Wthr.SkyTemp( Hour );
				LastHrBeamSolarRad = Wthr.BeamSolarRad( Hour );
				LastHrDifSolarRad = Wthr.DifSolarRad( Hour );
				LastHrAlbedo = Wthr.Albedo( Hour );
				LastHrLiquidPrecip = Wthr.LiquidPrecip( Hour );

			} // End of Hour Loop

			if ( Environment( Environ ).WP_Type1 != 0 ) {
				{ auto const SELECT_CASE_var( WPSkyTemperature( Environment( Environ ).WP_Type1 ).CalculationType );

				if ( SELECT_CASE_var == WP_ScheduleValue ) {
					GetScheduleValuesForDay( WPSkyTemperature( Environment( Environ ).WP_Type1 ).SchedulePtr, TomorrowSkyTemp, TomorrowVariables.DayOfYear_Schedule, CurDayOfWeek );
				} else if ( SELECT_CASE_var == WP_DryBulbDelta ) {
					GetScheduleValuesForDay( WPSkyTemperature( Environment( Environ ).WP_Type1 ).SchedulePtr, TomorrowSkyTemp, TomorrowVariables.DayOfYear_Schedule, CurDayOfWeek );
					for ( Hour = 1; Hour <= 24; ++Hour ) {
						for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
							TomorrowSkyTemp( TS, Hour ) = TomorrowOutDryBulbTemp( TS, Hour ) - TomorrowSkyTemp( TS, Hour );
						}
					}

				} else if ( SELECT_CASE_var == WP_DewPointDelta ) {
					GetScheduleValuesForDay( WPSkyTemperature( Environment( Environ ).WP_Type1 ).SchedulePtr, TomorrowSkyTemp, TomorrowVariables.DayOfYear_Schedule, CurDayOfWeek );
					for ( Hour = 1; Hour <= 24; ++Hour ) {
						for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
							TomorrowSkyTemp( TS, Hour ) = TomorrowOutDewPointTemp( TS, Hour ) - TomorrowSkyTemp( TS, Hour );
						}
					}

				} else {

				}}

			}
		}

	}

	void
	SetDayOfWeekInitialValues(
		int const EnvironDayOfWeek, // Starting Day of Week for the (Weather) RunPeriod (User Input)
		int & CurDayOfWeek, // Current Day of Week
		bool & UseDayOfWeek // hmmm does not appear to be used anywhere.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set of begin day of week for an environment.  Similar sets but slightly different
		// conditions.  Improve code readability by having three routine calls instead of three
		// IF blocks.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( EnvironDayOfWeek != 0 ) {
			if ( EnvironDayOfWeek <= 7 ) {
				CurDayOfWeek = EnvironDayOfWeek - 1;
			} else {
				CurDayOfWeek = EnvironDayOfWeek;
			}
			UseDayOfWeek = false;
		} else {
			UseDayOfWeek = true;
		}

	}

	void
	InterpretWeatherDataLine(
		std::string & Line,
		bool & ErrorFound,
		int & WYear,
		int & WMonth,
		int & WDay,
		int & WHour,
		int & WMinute,
		Real64 & RField1, // DryBulb
		Real64 & RField2, // DewPoint
		Real64 & RField3, // RelHum
		Real64 & RField4, // AtmPress
		Real64 & RField5, // ETHoriz
		Real64 & RField6, // ETDirect
		Real64 & RField7, // IRHoriz
		Real64 & RField8, // GLBHoriz
		Real64 & RField9, // DirectRad
		Real64 & RField10, // DiffuseRad
		Real64 & RField11, // GLBHorizIllum
		Real64 & RField12, // DirectNrmIllum
		Real64 & RField13, // DiffuseHorizIllum
		Real64 & RField14, // ZenLum
		Real64 & RField15, // WindDir
		Real64 & RField16, // WindSpeed
		Real64 & RField17, // TotalSkyCover
		Real64 & RField18, // OpaqueSkyCover
		Real64 & RField19, // Visibility
		Real64 & RField20, // CeilHeight
		int & WObs, // PresWeathObs
		Array1A_int WCodesArr, // PresWeathConds
		Real64 & RField22, // PrecipWater
		Real64 & RField23, // AerosolOptDepth
		Real64 & RField24, // SnowDepth
		Real64 & RField25, // DaysSinceLastSnow
		Real64 & RField26, // Albedo
		Real64 & RField27 // LiquidPrecip
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine interprets the EPW weather data line because comma delimited fields
		// may cause problems with some compilers.  (Particularly character variables in
		// comma delimited lines.

		// METHODOLOGY EMPLOYED:
		// Field by field interpretation, eliminating the "data source field" which is also
		// likely to contain blanks.  Note that the "Weatherconditions" must be a 9 character
		// alpha field with no intervening blanks.

		// REFERENCES:
		// CALL InterpretWeatherDataLine(WeatherDataLine,ErrorFound,WYear,WMonth,WDay,WHour,WMinute,  &
		//       DryBulb,DewPoint,RelHum,AtmPress,ETHoriz,ETDirect,IRHoriz,GLBHoriz,            &
		//       DirectRad,DiffuseRad,GLBHorizIllum,DirectNrmIllum,DiffuseHorizIllum,ZenLum,    &
		//       WindDir,WindSpeed,TotalSkyCover,OpaqueSkyCover,Visibility,CeilHeight,          &
		//       PresWeathObs,PresWeathConds,PrecipWater,AerosolOptDepth,SnowDepth,DaysSinceLastSnow,
		//       Albedo,LiquidPrecipDepth)

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		WCodesArr.dim( 9 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const ValidDigits( "0123456789" );
		static gio::Fmt fmtLD( "*" );
		static gio::Fmt fmt9I1( "(9I1)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string::size_type Pos;
		std::string PresWeathCodes;
		Real64 RYear;
		Real64 RMonth;
		Real64 RDay;
		Real64 RHour;
		Real64 RMinute;
		std::string DateError;
		Real64 RField21;
		int Count;
		static int LCount( 0 );
		bool DateInError;

		++LCount;
		ErrorFound = false;
		std::string const SaveLine = Line; // in case of errors

		// Do the first five.  (To get to the DataSource field)
		{ IOFlags flags; gio::read( Line, fmtLD, flags ) >> RYear >> RMonth >> RDay >> RHour >> RMinute; if ( flags.err() ) goto Label900; }
		WYear = nint( RYear );
		WMonth = nint( RMonth );
		WDay = nint( RDay );
		WHour = nint( RHour );
		WMinute = nint( RMinute );

		DateInError = false;
		if ( WMonth >= 1 && WMonth <= 12 ) {
			// Month number is valid
			if ( WMonth != 2 ) {
				if ( WDay > EndDayOfMonth( WMonth ) ) {
					DateInError = true;
				}
			} else if ( WDay > EndDayOfMonth( WMonth ) + 1 ) { // Whether actually used is determined by calling routine.
				DateInError = true;
			}
		} else {
			DateInError = true;
		}

		if ( DateInError ) {
			ShowSevereError( "Reading Weather Data Line, Invalid Date, Year=" + RoundSigDigits( WYear ) + ", Month=" + RoundSigDigits( WMonth ) + ", Day=" + RoundSigDigits( WDay ) );
			ShowFatalError( "Program terminates due to previous condition." );
		}

		Pos = index( Line, ',' ); // WYear
		if ( Pos == std::string::npos ) goto Label902;
		Line.erase( 0, Pos + 1 );
		Pos = index( Line, ',' ); // WMonth
		Line.erase( 0, Pos + 1 );
		Pos = index( Line, ',' ); // WDay
		Line.erase( 0, Pos + 1 );
		Pos = index( Line, ',' ); // WHour
		Line.erase( 0, Pos + 1 );
		Pos = index( Line, ',' ); // WMinute
		Line.erase( 0, Pos + 1 );

		// Data Source/Integrity field -- ignore
		Pos = index( Line, ',' );
		Line.erase( 0, Pos + 1 );

		// Now read more numerics with List Directed I/O (note there is another "character" field lurking)
		{ IOFlags flags; gio::read( Line, fmtLD, flags ) >> RField1 >> RField2 >> RField3 >> RField4 >> RField5 >> RField6 >> RField7 >> RField8 >> RField9 >> RField10 >> RField11 >> RField12 >> RField13 >> RField14 >> RField15 >> RField16 >> RField17 >> RField18 >> RField19 >> RField20 >> RField21; if ( flags.err() ) goto Label901; }
		for ( Count = 1; Count <= 21; ++Count ) {
			Pos = index( Line, ',' );
			Line.erase( 0, Pos + 1 );
		}
		Pos = index( Line, ',' );
		if ( Pos != std::string::npos && Pos != 0 ) {
			PresWeathCodes = Line.substr( 0, Pos );
		} else {
			PresWeathCodes = "999999999";
		}
		Line.erase( 0, Pos + 1 );
		Pos = index( Line, ',' );
		if ( Pos != std::string::npos ) {
			if ( Pos != 0 ) {
				{ IOFlags flags; gio::read( Line.substr( 0, Pos ), fmtLD, flags ) >> RField22; if ( flags.err() ) goto Label901; }
			} else {
				RField22 = 999.0;
			}
			Line.erase( 0, Pos + 1 );
			Pos = index( Line, ',' );
			if ( Pos != std::string::npos ) {
				if ( Pos != 0 ) {
					{ IOFlags flags; gio::read( Line.substr( 0, Pos ), fmtLD, flags ) >> RField23; if ( flags.err() ) goto Label901; }
				} else {
					RField23 = 999.0;
				}
				Line.erase( 0, Pos + 1 );
				Pos = index( Line, ',' );
				if ( Pos != std::string::npos ) {
					if ( Pos != 0 ) {
						{ IOFlags flags; gio::read( Line.substr( 0, Pos ), fmtLD, flags ) >> RField24; if ( flags.err() ) goto Label901; }
					} else {
						RField24 = 999.0;
					}
					Line.erase( 0, Pos + 1 );
					Pos = index( Line, ',' );
					if ( Pos != std::string::npos ) {
						if ( Pos != 0 ) {
							{ IOFlags flags; gio::read( Line.substr( 0, Pos ), fmtLD, flags ) >> RField25; if ( flags.err() ) goto Label901; }
						} else {
							RField25 = 999.0;
						}
						Line.erase( 0, Pos + 1 );
						Pos = index( Line, ',' );
						if ( Pos != std::string::npos ) {
							if ( Pos != 0 ) {
								{ IOFlags flags; gio::read( Line.substr( 0, Pos ), fmtLD, flags ) >> RField26; if ( flags.err() ) goto Label901; }
							} else {
								RField26 = 999.0;
							}
							Line.erase( 0, Pos + 1 );
							Pos = index( Line, ',' );
							if ( Pos != std::string::npos ) {
								if ( Pos != 0 ) {
									{ IOFlags flags; gio::read( Line.substr( 0, Pos ), fmtLD, flags ) >> RField27; if ( flags.err() ) goto Label901; }
								} else {
									RField27 = 999.0;
								}
								Line.erase( 0, Pos + 1 );
								Pos = index( Line, ',' );
							} else {
								RField27 = 999.0;
							}
						} else {
							RField26 = 999.0;
							RField27 = 999.0;
						}
					} else {
						{ IOFlags flags; gio::read( Line, fmtLD, flags ) >> RField25; if ( flags.err() ) goto Label901; }
						RField26 = 999.0;
						RField27 = 999.0;
					}
				} else {
					{ IOFlags flags; gio::read( Line, fmtLD, flags ) >> RField24; if ( flags.err() ) goto Label901; }
					RField25 = 999.0;
					RField26 = 999.0;
					RField27 = 999.0;
				}
			} else {
				{ IOFlags flags; gio::read( Line, fmtLD, flags ) >> RField23; if ( flags.err() ) goto Label901; }
				RField24 = 999.0;
				RField25 = 999.0;
				RField26 = 999.0;
				RField27 = 999.0;
			}
		} else {
			{ IOFlags flags; gio::read( Line, fmtLD, flags ) >> RField22; if ( flags.err() ) goto Label901; }
			RField23 = 999.0;
			RField24 = 999.0;
			RField25 = 999.0;
			RField26 = 999.0;
			RField27 = 999.0;
		}
		//  READ(Line,*,err=903,end=903) RField22,RField23,RField24,RField25

		WObs = nint( RField21 );
		if ( WObs == 0 ) { // Obs Indicator indicates Weather Codes valid
			// Check for miscellaneous characters
			Pos = index( PresWeathCodes, '\'' );
			while ( Pos != std::string::npos ) {
				PresWeathCodes[ Pos ] = ' ';
				Pos = index( PresWeathCodes, '\'' );
			}
			Pos = index( PresWeathCodes, '"' );
			while ( Pos != std::string::npos ) {
				PresWeathCodes[ Pos ] = ' ';
				Pos = index( PresWeathCodes, '"' );
			}
			strip( PresWeathCodes );
			if ( len( PresWeathCodes ) == 9 ) {
				for ( Pos = 0; Pos < 9; ++Pos ) {
					if ( ! has( ValidDigits, PresWeathCodes[ Pos ] ) ) PresWeathCodes[ Pos ] = '9';
				}
				gio::read( PresWeathCodes, fmt9I1 ) >> WCodesArr;
			} else {
				++Missed.WeathCodes;
				WCodesArr = 9;
			}
		} else {
			WCodesArr = 9;
		}

		return;

Label900: ;
		ShowSevereError( "Invalid Date info in Weather Line" );
		ShowContinueError( "Entire Data Line=" + SaveLine );
		ShowFatalError( "Error in Reading Weather Data" );

Label901: ;
		gio::write( DateError, "(I4,'/',I2,'/',I2,' Hour#=',I2,' Min#=',I2)" ) << WYear << WMonth << WDay << WHour << WMinute;
		ShowSevereError( "Invalid Weather Line at date=" + DateError );
		ShowContinueError( "Full Data Line=" + SaveLine );
		ShowContinueError( "Remainder of line=" + Line );
		ShowFatalError( "Error in Reading Weather Data" );

Label902: ;
		gio::write( DateError, "(I4,'/',I2,'/',I2,' Hour#=',I2,' Min#=',I2)" ) << WYear << WMonth << WDay << WHour << WMinute;
		ShowSevereError( "Invalid Weather Line (no commas) at date=" + DateError );
		ShowContinueError( "Full Data Line=" + SaveLine );
		ShowContinueError( "Remainder of line=" + Line );
		ShowFatalError( "Error in Reading Weather Data" );

//Label903: ;
//		gio::write( DateError, "(I4,'/',I2,'/',I2,' Hour#=',I2,' Min#=',I2)" ) << WYear << WMonth << WDay << WHour << WMinute;
//		ShowSevereError( "Invalid Weather Line at date=" + DateError );
//		ShowContinueError( "Full Data Line=" + SaveLine );
//		ShowContinueError( "Partial line read; Remainder of line=" + Line );
//		ShowFatalError( "Error in Reading Weather Data" );

	}

	void
	SetUpDesignDay( int const EnvrnNum ) // Environment number passed into the routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 1977
		//       MODIFIED       June 1997 (RKS); May 2013 (LKL) add temperature profile for drybulb.
		//       RE-ENGINEERED  August 2003;LKL -- to generate timestep weather for design days.

		// PURPOSE OF THIS SUBROUTINE:
		// This purpose of this subroutine is to convert the user supplied input
		// values for the design day parameters into an entire weather day
		// record.  This now bypasses any file I/O by keeping all of the
		// weather day record information in the local module level derived type
		// called DesignDay.

		// METHODOLOGY EMPLOYED:
		// Methodology incorporates the design day setup from Tarp as appropriate.

		// REFERENCES:
		// ASHRAE Handbook of Fundamentals?

		// Using/Aliasing
		using General::RoundSigDigits;
		using General::JulianDay;
		using InputProcessor::SameString;
		using ScheduleManager::GetSingleDayScheduleValues;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const GlobalSolarConstant( 1367.0 );
		Real64 const ZHGlobalSolarConstant( 1355.0 );
		static gio::Fmt EnvDDHdFormat( "('! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, ',   'Temp Range {dC}, Temp Range Ind Type, ',   'Hum Ind Value at Max Temp, Hum Ind Type,Pressure {Pa}, ',   'Wind Direction {deg CW from N}, ',    'Wind Speed {m/s}, Clearness, Rain, Snow')" );
		static gio::Fmt EnvDDayFormat( "('Environment:Design Day Data,')" );
		static gio::Fmt DDayMiscHdFormat( "('! <Environment:Design_Day_Misc>,DayOfYear,ASHRAE A Coeff,',   'ASHRAE B Coeff,ASHRAE C Coeff,Solar Constant-Annual Variation,',   'Eq of Time {minutes}, Solar Declination Angle {deg}, Solar Model')" );
		static gio::Fmt DDayMiscFormat( "('Environment:Design_Day_Misc,',I3,',')" );
		static gio::Fmt MnDyFmt( "(I2.2,'/',I2.2)" );
		Real64 const ZhangHuangModCoeff_C0( 0.5598 ); // 37.6865d0
		Real64 const ZhangHuangModCoeff_C1( 0.4982 ); // 13.9263d0
		Real64 const ZhangHuangModCoeff_C2( -0.6762 ); // -20.2354d0
		Real64 const ZhangHuangModCoeff_C3( 0.02842 ); // 0.9695d0
		Real64 const ZhangHuangModCoeff_C4( -0.00317 ); // -0.2046d0
		Real64 const ZhangHuangModCoeff_C5( 0.014 ); // -0.0980d0
		Real64 const ZhangHuangModCoeff_D( -17.853 ); // -10.8568d0
		Real64 const ZhangHuangModCoeff_K( 0.843 ); // 49.3112d0
		static std::string const RoutineNamePsyWFnTdbTwbPb( "SetUpDesignDay:PsyWFnTdbTwbPb" );
		static std::string const RoutineNamePsyWFnTdpPb( "SetUpDesignDay:PsyWFnTdpPb" );
		static std::string const RoutineNamePsyWFnTdbH( "SetUpDesignDay:PsyWFnTdbH" );
		static std::string const WeatherManager( "WeatherManager" );
		static std::string const RoutineNameLong( "WeatherManager.cc subroutine SetUpDesignDay" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Hour;
		int TS;
		Real64 A; // Apparent solar irradiation at air mass = 0
		Real64 AVSC; // Annual variation in the solar constant
		Real64 B; // Atmospheric extinction coefficient
		Real64 C; // ASHRAE diffuse radiation factor
		Real64 ETR; // radiation of an extraterrestrial normal surface, W/m2
		Real64 HO; // Radiation on an extraterrestial horizontal surface
		Real64 KT; // Radiation ratio
		Array1D< Real64 > SUNCOS( 3 ); // Sun direction cosines
		int CurrentYear;
		int OSky; // Opaque Sky Cover (tenths)
		Real64 HumidityRatio; // Humidity Ratio -- when constant for day
		Real64 TDewK; // Dewpoint in Kelvin
		Real64 ESky; // Emissivitity of Sky
		Real64 CosZenith; // Cosine of Zenith Angle of Sun
		Real64 TotHoriz; // Total Radiation on Horizontal Surface
		Real64 GndReflet; // Ground Reflectivity
		Real64 CurTime; // For Solar Calcs
		Real64 WetBulb; // For calculating
		Real64 DBRange; // working copy of dry-bulb daily range, C (or 1 if input is difference)
		Real64 WBRange; // working copy of wet-bulb daily range. C (or 1 if input is difference)

		Array1D_int Date0( 8 );
		static bool PrintDDHeader;
		std::string AlpUseRain;
		std::string AlpUseSnow;
		bool ConstantHumidityRatio;
		Real64 OutHumRat;
		std::string StringOut;
		bool SaveWarmupFlag;
		Real64 GloHorzRad;
		Real64 ClearnessIndex_kt;
		Real64 ClearnessIndex_ktc;
		Real64 ClearnessIndex_kds;
		Real64 SinSolarAltitude;
		Real64 TotSkyCover;
		int Hour1Ago;
		int Hour3Ago;
		Real64 BeamRad; // working calculated beam and diffuse rad, W/m2
		Real64 DiffRad;
		Real64 testval;
		//     For reporting purposes, set year to current system year

		struct HourlyWeatherData
		{
			// Members
			Array1D< Real64 > BeamSolarRad; // Hourly direct normal solar irradiance
			Array1D< Real64 > DifSolarRad; // Hourly sky diffuse horizontal solar irradiance

			// Default Constructor
			HourlyWeatherData() :
				BeamSolarRad( 24, 0.0 ),
				DifSolarRad( 24, 0.0 )
			{}

		};

		// Object Data
		HourlyWeatherData Wthr;

		SaveWarmupFlag = WarmupFlag;
		WarmupFlag = true;

		date_and_time( _, _, _, Date0 );
		CurrentYear = Date0( 1 );

		if ( BeginSimFlag ) {
			PrintDDHeader = true;
		}

		DesignDay( EnvrnNum ).Year = CurrentYear; // f90 date_and_time implemented. full 4 digit year !+ 1900
		DesignDay( EnvrnNum ).Month = DesDayInput( EnvrnNum ).Month;
		DesignDay( EnvrnNum ).DayOfMonth = DesDayInput( EnvrnNum ).DayOfMonth;
		DesignDay( EnvrnNum ).DayOfYear = JulianDay( DesignDay( EnvrnNum ).Month, DesignDay( EnvrnNum ).DayOfMonth, 0 );
		gio::write( CurMnDy, MnDyFmt ) << DesDayInput( EnvrnNum ).Month << DesDayInput( EnvrnNum ).DayOfMonth;
		//EnvironmentName = DesDayInput( EnvrnNum ).Title;
		RunPeriodEnvironment = false;
		// Following builds Environment start/end for ASHRAE 55 warnings
		EnvironmentStartEnd = CurMnDy + " - " + CurMnDy;

		// Check that barometric pressure is within range
		if ( DesDayInput( EnvrnNum ).PressureEntered ) {
			if ( std::abs( ( DesDayInput( EnvrnNum ).PressBarom - StdBaroPress ) / StdBaroPress ) > 0.1 ) { // 10% off
				ShowWarningError( "SetUpDesignDay: Entered DesignDay Barometric Pressure=" + RoundSigDigits( DesDayInput( EnvrnNum ).PressBarom, 0 ) + " differs by more than 10% from Standard Barometric Pressure=" + RoundSigDigits( StdBaroPress, 0 ) + '.' );
				ShowContinueError( "...occurs in DesignDay=" + EnvironmentName + ", Standard Pressure (based on elevation) will be used." );
				DesDayInput( EnvrnNum ).PressBarom = StdBaroPress;
			}
		} else {
			DesDayInput( EnvrnNum ).PressBarom = StdBaroPress;
		}

		// verify that design WB or DP <= design DB
		if ( DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_DewPoint && DesDayInput( EnvrnNum ).DewPointNeedsSet ) {
			// dew-point
			testval = PsyWFnTdbRhPb( DesDayInput( EnvrnNum ).MaxDryBulb, 1.0, DesDayInput( EnvrnNum ).PressBarom );
			DesDayInput( EnvrnNum ).HumIndValue = PsyTdpFnWPb( testval, DesDayInput( EnvrnNum ).PressBarom );
		}

		// Day of week defaults to Monday, if day type specified, then that is used.
		DesignDay( EnvrnNum ).DayOfWeek = 2;
		if ( DesDayInput( EnvrnNum ).DayType <= 7 ) DesignDay( EnvrnNum ).DayOfWeek = DesDayInput( EnvrnNum ).DayType;

		// set Holiday as indicated by user input
		DesignDay( EnvrnNum ).HolidayIndex = 0;
		if ( DesDayInput( EnvrnNum ).DayType > 7 ) DesignDay( EnvrnNum ).HolidayIndex = DesDayInput( EnvrnNum ).DayType - 7;

		DesignDay( EnvrnNum ).DaylightSavingIndex = DesDayInput( EnvrnNum ).DSTIndicator;

		//  Set up Solar parameters for day
		CalculateDailySolarCoeffs( DesignDay( EnvrnNum ).DayOfYear, A, B, C, AVSC, DesignDay( EnvrnNum ).EquationOfTime, DesignDay( EnvrnNum ).SinSolarDeclinAngle, DesignDay( EnvrnNum ).CosSolarDeclinAngle );

		if ( PrintDDHeader && DoWeatherInitReporting ) {
			gio::write( OutputFileInits, EnvDDHdFormat );
			gio::write( OutputFileInits, DDayMiscHdFormat );
			PrintDDHeader = false;
		}
		if ( DoWeatherInitReporting ) {
			if (DesDayInput(EnvrnNum).RainInd == 1) {
				AlpUseRain = "Yes";
			} else {
				AlpUseRain = "No";
			}
			if (DesDayInput(EnvrnNum).SnowInd == 1) {
				AlpUseSnow = "Yes";
			} else {
				AlpUseSnow = "No";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, EnvDDayFormat, flags ); }
			StringOut = RoundSigDigits(DesDayInput(EnvrnNum).MaxDryBulb, 2);
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits(DesDayInput(EnvrnNum).DailyDBRange, 2);
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = ",";
			if (DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType_Default) {
				StringOut = "DefaultMultipliers,";
			} else if (DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType_Multiplier) {
				StringOut = "MultiplierSchedule,";
			} else if (DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType_Profile) {
				StringOut = "TemperatureProfile,";
			} else if (DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType_Difference) {
				StringOut = "DifferenceSchedule,";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut; }
			if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_WetBulb) {
				StringOut = "Wetbulb," + RoundSigDigits(DesDayInput(EnvrnNum).HumIndValue, 2) + " {C},";
			} else if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_DewPoint) {
				StringOut = "Dewpoint," + RoundSigDigits(DesDayInput(EnvrnNum).HumIndValue, 2) + " {C},";
			} else if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_Enthalpy) {
				StringOut = "Enthalpy," + RoundSigDigits(DesDayInput(EnvrnNum).HumIndValue, 2) + " {kJ/kg},";
			} else if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_HumRatio) {
				StringOut = "HumidityRatio," + RoundSigDigits(DesDayInput(EnvrnNum).HumIndValue, 4) + " {},";
			} else if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_RelHumSch) {
				StringOut = "Schedule,<schedule values from 0.0 to 100.0 {percent},";
			} else if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_WBProfDef) {
				StringOut = "WetBulbProfileDefaultMultipliers," + RoundSigDigits( DesDayInput( Envrn ).HumIndValue, 2 ) + " {C},";
			} else if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_WBProfDif) {
				StringOut = "WetBulbProfileDifferenceSchedule," + RoundSigDigits( DesDayInput( EnvrnNum ).HumIndValue, 2 ) + " {C},";
			} else if (DesDayInput(EnvrnNum).HumIndType == DDHumIndType_WBProfMul) {
				StringOut = "WetBulbProfileMultiplierSchedule," + RoundSigDigits( DesDayInput( EnvrnNum ).HumIndValue, 2 ) + " {C},";
			}
			StringOut = RoundSigDigits(DesDayInput(EnvrnNum).PressBarom, 0);
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits(DesDayInput(EnvrnNum).WindDir, 0);
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits(DesDayInput(EnvrnNum).WindSpeed, 1);
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits(DesDayInput(EnvrnNum).SkyClear, 2);
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			gio::write( OutputFileInits, fmtA ) << AlpUseRain + ',' + AlpUseSnow;

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, DDayMiscFormat, flags ) << DesignDay( EnvrnNum ).DayOfYear; }
			StringOut = RoundSigDigits( A, 1 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( B, 4 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( C, 4 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( AVSC, 1 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( DesignDay( EnvrnNum ).EquationOfTime * 60.0, 2 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( std::asin( DesignDay( EnvrnNum ).SinSolarDeclinAngle ) / DegToRadians, 1 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( DesDayInput( EnvrnNum ).SolarModel == ASHRAE_ClearSky ) {
				StringOut = "ASHRAEClearSky";
			} else if ( DesDayInput( EnvrnNum ).SolarModel == Zhang_Huang ) {
				StringOut = "ZhangHuang";
			} else if ( DesDayInput( EnvrnNum ).SolarModel == SolarModel_Schedule ) {
				StringOut = "User supplied beam/diffuse from schedules";
			} else if ( DesDayInput( EnvrnNum ).SolarModel == ASHRAE_Tau ) {
				StringOut = "ASHRAETau";
			} else {
				StringOut = "unknown";
			}
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}

		// Must set up weather values for Design Day.  User can specify the "humidity indicator" as
		// Wetbulb, DewPoint or input the relative humidity schedule.  For both wetbulb and dewpoint indicators, the
		// humidity for the day will be constant, using the drybulb (max) and humidity indicator temperature to
		// set the values.  For the scheduled values, these are already set in the DDxxx array.

		CurrentTime = 25.0;

		{ auto const SELECT_CASE_var( DesDayInput( EnvrnNum ).HumIndType );

		if ( SELECT_CASE_var == DDHumIndType_WetBulb ) {
			HumidityRatio = PsyWFnTdbTwbPb( DesDayInput( EnvrnNum ).MaxDryBulb, DesDayInput( EnvrnNum ).HumIndValue, DesDayInput( EnvrnNum ).PressBarom, RoutineNamePsyWFnTdbTwbPb );
			ConstantHumidityRatio = true;

		} else if ( SELECT_CASE_var == DDHumIndType_DewPoint ) {
			HumidityRatio = PsyWFnTdpPb( DesDayInput( EnvrnNum ).HumIndValue, DesDayInput( EnvrnNum ).PressBarom, RoutineNamePsyWFnTdpPb );
			ConstantHumidityRatio = true;

		} else if ( SELECT_CASE_var == DDHumIndType_HumRatio ) {
			HumidityRatio = DesDayInput( EnvrnNum ).HumIndValue;
			ConstantHumidityRatio = true;

		} else if ( SELECT_CASE_var == DDHumIndType_Enthalpy ) {
			HumidityRatio = PsyWFnTdbH( DesDayInput( EnvrnNum ).MaxDryBulb, DesDayInput( EnvrnNum ).HumIndValue * 1000.0, RoutineNamePsyWFnTdbH );
			ConstantHumidityRatio = true;

		} else if ( SELECT_CASE_var == DDHumIndType_RelHumSch ) {
			// nothing to do -- DDHumIndModifier already contains the scheduled Relative Humidity
			ConstantHumidityRatio = false;
			TomorrowOutRelHum = DDHumIndModifier( _, _, EnvrnNum );

		} else if ( ( SELECT_CASE_var == DDHumIndType_WBProfDef ) || ( SELECT_CASE_var == DDHumIndType_WBProfDif ) || ( SELECT_CASE_var == DDHumIndType_WBProfMul ) ) {
			ConstantHumidityRatio = false;

		} else {
			ShowSevereError( "SetUpDesignDay: Invalid Humidity Indicator type" );
			ShowContinueError( "Occurred in Design Day=" + DesDayInput( EnvrnNum ).Title );

		}}

		if ( DesDayInput( EnvrnNum ).RainInd != 0 ) {
			TomorrowIsRain( _, _ ) = true;
			OSky = 10;
			TomorrowLiquidPrecip = 3.0;
		} else {
			TomorrowIsRain( _, _ ) = false;
			OSky = 0;
			TomorrowLiquidPrecip = 0.0;
		}

		if ( DesDayInput( EnvrnNum ).SnowInd == 0 ) {
			TomorrowIsSnow( _, _ ) = false;
			GndReflet = 0.2;
		} else { // Snow
			TomorrowIsSnow( _, _ ) = true;
			GndReflet = 0.7;
		}

		// Some values are constant

		TomorrowOutBaroPress( _, _ ) = DesDayInput( EnvrnNum ).PressBarom;
		TomorrowWindSpeed( _, _ ) = DesDayInput( EnvrnNum ).WindSpeed;
		TomorrowWindDir( _, _ ) = DesDayInput( EnvrnNum ).WindDir;
		TomorrowAlbedo = 0.0;

		// resolve daily ranges
		if ( DesDayInput( EnvrnNum ).DBTempRangeType == DDDBRangeType_Difference ) {
			DBRange = 1.0; // use unscaled multiplier values if difference
		} else if ( DesDayInput( EnvrnNum ).DBTempRangeType == DDDBRangeType_Profile ) {
			DBRange = 0.0;
		} else {
			DBRange = DesDayInput( EnvrnNum ).DailyDBRange;
		}
		if ( DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfDif ) {
			WBRange = 1.0; // use unscaled multiplier values if difference
		} else {
			WBRange = DesDayInput( EnvrnNum ).DailyWBRange;
		}

		for ( Hour = 1; Hour <= 24; ++Hour ) {
			for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {

				if ( DesDayInput( EnvrnNum ).DBTempRangeType != DDDBRangeType_Profile ) {
					// dry-bulb profile
					TomorrowOutDryBulbTemp( TS, Hour ) = DesDayInput( EnvrnNum ).MaxDryBulb - DDDBRngModifier( TS, Hour, EnvrnNum ) * DBRange;
				} else { // DesDayInput(EnvrnNum)%DBTempRangeType == DDDBRangeType_Profile
					TomorrowOutDryBulbTemp( TS, Hour ) = DDDBRngModifier( TS, Hour, EnvrnNum );
				}

				// wet-bulb - generate from profile, humidity ratio, or dew point
				if ( DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfDef || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfDif || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfMul ) {
					WetBulb = DesDayInput( EnvrnNum ).HumIndValue - DDHumIndModifier( TS, Hour, EnvrnNum ) * WBRange;
					WetBulb = min( WetBulb, TomorrowOutDryBulbTemp( TS, Hour ) ); // WB must be <= DB
					OutHumRat = PsyWFnTdbTwbPb( TomorrowOutDryBulbTemp( TS, Hour ), WetBulb, DesDayInput( EnvrnNum ).PressBarom );
					TomorrowOutDewPointTemp( TS, Hour ) = PsyTdpFnWPb( OutHumRat, DesDayInput( EnvrnNum ).PressBarom );
					TomorrowOutRelHum( TS, Hour ) = PsyRhFnTdbWPb( TomorrowOutDryBulbTemp( TS, Hour ), OutHumRat, DesDayInput( EnvrnNum ).PressBarom, WeatherManager ) * 100.0;
				} else if ( ConstantHumidityRatio ) {
					//  Need Dew Point Temperature.  Use Relative Humidity to get Humidity Ratio, unless Humidity Ratio is constant
					//BG 9-26-07  moved following inside this IF statment; when HumIndType is 'Schedule' HumidityRatio wasn't being initialized
					WetBulb = PsyTwbFnTdbWPb( TomorrowOutDryBulbTemp( TS, Hour ), HumidityRatio, DesDayInput( EnvrnNum ).PressBarom, RoutineNameLong );

					OutHumRat = PsyWFnTdpPb( TomorrowOutDryBulbTemp( TS, Hour ), DesDayInput( EnvrnNum ).PressBarom );
					if ( HumidityRatio > OutHumRat ) {
						WetBulb = TomorrowOutDryBulbTemp( TS, Hour );
					} else {
						OutHumRat = PsyWFnTdbTwbPb( TomorrowOutDryBulbTemp( TS, Hour ), WetBulb, DesDayInput( EnvrnNum ).PressBarom );
					}
					TomorrowOutDewPointTemp( TS, Hour ) = PsyTdpFnWPb( OutHumRat, DesDayInput( EnvrnNum ).PressBarom );
					TomorrowOutRelHum( TS, Hour ) = PsyRhFnTdbWPb( TomorrowOutDryBulbTemp( TS, Hour ), OutHumRat, DesDayInput( EnvrnNum ).PressBarom, WeatherManager ) * 100.0;
				} else {
					HumidityRatio = PsyWFnTdbRhPb( TomorrowOutDryBulbTemp( TS, Hour ), DDHumIndModifier( TS, Hour, EnvrnNum ) / 100.0, DesDayInput( EnvrnNum ).PressBarom );
					// TomorrowOutRelHum values set earlier
					TomorrowOutDewPointTemp( TS, Hour ) = PsyTdpFnWPb( HumidityRatio, DesDayInput( EnvrnNum ).PressBarom );
				}

				// Determine Sky Temp ==>
				// Function of DryBulb, DewPoint, OpaqueSkyCover
				// Calculate Sky IR
				//HIR = ESKY * SIGMA * (TOUT**4)
				//where
				//HIR = horizontal IR intensity (W/m2)
				//ESKY = sky emissivity
				//SIGMA = Stefan-Boltzmann constant = 5.6697e-8 W/m2-K4
				//TOUT = drybulb temperature (K)
				//The sky emissivity is given by
				//ESKY = [0.787 + 0.764*ln(TDEW/273)]*[1 + 0.0224*N - 0.0035*(N**2) + 0.00028*(N**3)]
				//where
				//TDEW = dewpoint temperature (K)
				//N = opaque sky cover (tenths)
				//Example: Clear sky (N=0), TOUT = 273+20=293K, TDEW = 273+10=283K:
				//ESKY = 0.787 + 0.764*0.036 = 0.815
				//HIR = 0.815*5.6697e-8*(293**4) = 340.6 W/m2

				//References:
				//George N. Walton, "Thermal Analysis Research Program Reference Manual,"
				//NBSIR 83-2655, March 1983, p. 21.
				//G. Clark and C. Allen, "The Estimation of Atmospheric Radiation for Clear and
				//Cloudy Skies," Proc. 2nd National Passive Solar Conference (AS/ISES), 1978, pp. 675-678.

				if ( Environment( EnvrnNum ).WP_Type1 == 0 ) {
					TDewK = min( TomorrowOutDryBulbTemp( TS, Hour ), TomorrowOutDewPointTemp( TS, Hour ) ) + TKelvin;
					ESky = ( 0.787 + 0.764 * std::log( ( TDewK ) / TKelvin ) ) * ( 1.0 + 0.0224 * OSky - 0.0035 * pow_2( OSky ) + 0.00028 * pow_3( OSky ) );
					TomorrowHorizIRSky( TS, Hour ) = ESky * Sigma * pow_4( TomorrowOutDryBulbTemp( TS, Hour ) + TKelvin );
					TomorrowSkyTemp( TS, Hour ) = ( TomorrowOutDryBulbTemp( TS, Hour ) + TKelvin ) * root_4( ESky ) - TKelvin;
				} else {
					TDewK = min( TomorrowOutDryBulbTemp( TS, Hour ), TomorrowOutDewPointTemp( TS, Hour ) ) + TKelvin;
					ESky = ( 0.787 + 0.764 * std::log( ( TDewK ) / TKelvin ) ) * ( 1.0 + 0.0224 * OSky - 0.0035 * pow_2( OSky ) + 0.00028 * pow_3( OSky ) );
					TomorrowHorizIRSky( TS, Hour ) = ESky * Sigma * pow_4( TomorrowOutDryBulbTemp( TS, Hour ) + TKelvin );
				}

				// Generate solar values for timestep
				//    working results = BeamRad and DiffRad
				//    stored to program globals at end of loop
				if ( DesDayInput( EnvrnNum ).SolarModel == SolarModel_Schedule ) {
					// scheduled: set value unconditionally (whether sun up or not)
					BeamRad = DDBeamSolarValues( TS, Hour, EnvrnNum );
					DiffRad = DDDiffuseSolarValues( TS, Hour, EnvrnNum );
				} else {

					// calc time = fractional hour of day
					if ( NumOfTimeStepInHour != 1 ) {
						CurTime = double( Hour - 1 ) + double( TS ) * TimeStepFraction;
					} else {
						CurTime = double( Hour ) + TS1TimeOffset;
					}

					CalculateSunDirectionCosines( CurTime, DesignDay( EnvrnNum ).EquationOfTime, DesignDay( EnvrnNum ).SinSolarDeclinAngle, DesignDay( EnvrnNum ).CosSolarDeclinAngle, SUNCOS );
					CosZenith = SUNCOS( 3 );
					if ( CosZenith < SunIsUpValue ) {
						BeamRad = 0.0;
						DiffRad = 0.0;
					} else {
						SinSolarAltitude = SUNCOS( 3 );

						{ auto const SELECT_CASE_var( DesDayInput( EnvrnNum ).SolarModel );

						if ( SELECT_CASE_var == ASHRAE_ClearSky ) {
							TotHoriz = DesDayInput( EnvrnNum ).SkyClear * A * ( C + CosZenith ) * std::exp( -B / CosZenith );
							HO = GlobalSolarConstant * AVSC * CosZenith;
							KT = TotHoriz / HO;
							KT = min( KT, 0.75 );
							DiffRad = TotHoriz * ( 1.0045 + KT * ( 0.04349 + KT * ( -3.5227 + 2.6313 * KT ) ) );
							if ( DesDayInput( EnvrnNum ).SkyClear > 0.70 ) DiffRad = TotHoriz * C / ( C + CosZenith );
							BeamRad = ( TotHoriz - DiffRad ) / CosZenith;
							DiffRad = max( 0.0, DiffRad );
							BeamRad = max( 0.0, BeamRad );

						} else if ( SELECT_CASE_var == ASHRAE_Tau ) {
							ETR = GlobalSolarConstant * AVSC; // extraterrestrial normal irrad, W/m2
							ASHRAETauModel( ETR, CosZenith, DesDayInput( EnvrnNum ).TauB, DesDayInput( EnvrnNum ).TauD, BeamRad, DiffRad, GloHorzRad );

						} else if ( SELECT_CASE_var == Zhang_Huang ) {
							Hour3Ago = mod( Hour + 20, 24 ) + 1; // hour 3 hours before
							TotSkyCover = max( 1.0 - DesDayInput( EnvrnNum ).SkyClear, 0.0 );
							GloHorzRad = ( ZHGlobalSolarConstant * SinSolarAltitude * ( ZhangHuangModCoeff_C0 + ZhangHuangModCoeff_C1 * TotSkyCover + ZhangHuangModCoeff_C2 * pow_2( TotSkyCover ) + ZhangHuangModCoeff_C3 * ( TomorrowOutDryBulbTemp( TS, Hour ) - TomorrowOutDryBulbTemp( TS, Hour3Ago ) ) + ZhangHuangModCoeff_C4 * TomorrowOutRelHum( TS, Hour ) + ZhangHuangModCoeff_C5 * TomorrowWindSpeed( TS, Hour ) ) + ZhangHuangModCoeff_D ) / ZhangHuangModCoeff_K;
							GloHorzRad = max( GloHorzRad, 0.0 );
							ClearnessIndex_kt = GloHorzRad / ( GlobalSolarConstant * SinSolarAltitude );
							//          ClearnessIndex_kt=DesDayInput(EnvrnNum)%SkyClear
							ClearnessIndex_ktc = 0.4268 + 0.1934 * SinSolarAltitude;
							if ( ClearnessIndex_kt < ClearnessIndex_ktc ) {
								ClearnessIndex_kds = ( 3.996 - 3.862 * SinSolarAltitude + 1.54 * pow_2( SinSolarAltitude ) ) * pow_3( ClearnessIndex_kt );
							} else {
								ClearnessIndex_kds = ClearnessIndex_kt - ( 1.107 + 0.03569 * SinSolarAltitude + 1.681 * pow_2( SinSolarAltitude ) ) * pow_3( 1.0 - ClearnessIndex_kt );
							}
							// Calculate direct normal radiation, W/m2
							BeamRad = ZHGlobalSolarConstant * SinSolarAltitude * ClearnessIndex_kds * ( ( 1.0 - ClearnessIndex_kt ) / ( 1.0 - ClearnessIndex_kds ) );
							// Calculation diffuse horizontal radiation, W/m2
							DiffRad = ZHGlobalSolarConstant * SinSolarAltitude * ( ( ClearnessIndex_kt - ClearnessIndex_kds ) / ( 1.0 - ClearnessIndex_kds ) );

						} else {
						}}
					}
				}

				// override result to 0 per environment var (for testing)
				if ( IgnoreSolarRadiation || IgnoreBeamRadiation ) BeamRad = 0.0;
				if ( IgnoreSolarRadiation || IgnoreDiffuseRadiation ) DiffRad = 0.0;

				TomorrowBeamSolarRad( TS, Hour ) = BeamRad;
				TomorrowDifSolarRad( TS, Hour ) = DiffRad;

			} // Timestep (TS) Loop
		} // Hour Loop

		// back-fill hour values from timesteps
		// hour values = integrated over hour ending at time of hour
		// insurance: hourly values not known to be needed
		for ( Hour = 1; Hour <= 24; ++Hour ) {
			Hour1Ago = mod( Hour + 22, 24 ) + 1;
			BeamRad = ( TomorrowBeamSolarRad( NumOfTimeStepInHour, Hour1Ago ) + TomorrowBeamSolarRad( NumOfTimeStepInHour, Hour ) ) / 2.0;
			DiffRad = ( TomorrowDifSolarRad( NumOfTimeStepInHour, Hour1Ago ) + TomorrowDifSolarRad( NumOfTimeStepInHour, Hour ) ) / 2.0;
			if ( NumOfTimeStepInHour > 1 ) {
				BeamRad += sum( TomorrowBeamSolarRad( {1,NumOfTimeStepInHour - 1}, Hour ) );
				DiffRad += sum( TomorrowDifSolarRad( {1,NumOfTimeStepInHour - 1}, Hour ) );
			}
			Wthr.BeamSolarRad( Hour ) = BeamRad / NumOfTimeStepInHour;
			Wthr.DifSolarRad( Hour ) = DiffRad / NumOfTimeStepInHour;
		}

		if ( Environment( EnvrnNum ).WP_Type1 != 0 ) {

			{ auto const SELECT_CASE_var( WPSkyTemperature( Environment( EnvrnNum ).WP_Type1 ).CalculationType );

			if ( SELECT_CASE_var == WP_ScheduleValue ) {
				GetSingleDayScheduleValues( WPSkyTemperature( Environment( EnvrnNum ).WP_Type1 ).SchedulePtr, TomorrowSkyTemp );
				DDSkyTempScheduleValues( _, _, EnvrnNum ) = TomorrowSkyTemp;
			} else if ( SELECT_CASE_var == WP_DryBulbDelta ) {
				GetSingleDayScheduleValues( WPSkyTemperature( Environment( EnvrnNum ).WP_Type1 ).SchedulePtr, TomorrowSkyTemp );
				DDSkyTempScheduleValues( _, _, EnvrnNum ) = TomorrowSkyTemp;
				for ( Hour = 1; Hour <= 24; ++Hour ) {
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						TomorrowSkyTemp( TS, Hour ) = TomorrowOutDryBulbTemp( TS, Hour ) - TomorrowSkyTemp( TS, Hour );
					}
				}

			} else if ( SELECT_CASE_var == WP_DewPointDelta ) {
				GetSingleDayScheduleValues( WPSkyTemperature( Environment( EnvrnNum ).WP_Type1 ).SchedulePtr, TomorrowSkyTemp );
				DDSkyTempScheduleValues( _, _, EnvrnNum ) = TomorrowSkyTemp;
				for ( Hour = 1; Hour <= 24; ++Hour ) {
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						TomorrowSkyTemp( TS, Hour ) = TomorrowOutDewPointTemp( TS, Hour ) - TomorrowSkyTemp( TS, Hour );
					}
				}

			} else {

			}}

		}

		WarmupFlag = SaveWarmupFlag;

	}

	//------------------------------------------------------------------------------

	Real64
	AirMass( Real64 const CosZen ) // COS( solar zenith), 0 - 1
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         C Barnaby
		//       DATE WRITTEN   Nov 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate relative air mass using Kasten and Young approximation

		// METHODOLOGY EMPLOYED:
		// Eqn (16), ASHRAE HOF 2009, p. 14.9

		// REFERENCES:
		// ASHRAE HOF 2009 Chapter 14
		// Kasten, F and T. Young.  1989.  Revised optical air mass tables
		//   and approximating formula.  Applied Optics 28:4735-4738.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 AirMass;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SunAltD;

		if ( CosZen <= 0.001 ) {
			AirMass = 37.07837343; // limit value calc'd with Excel
			//  value increases little as CosZen -> 0
		} else if ( CosZen >= 1.0 ) {
			AirMass = 1.0;
		} else {
			// note: COS( Zen) = SIN( Alt)
			SunAltD = std::asin( CosZen ) / DegToRadians; // altitude, degrees
			AirMass = 1.0 / ( CosZen + 0.50572 * std::pow( 6.07995 + SunAltD, -1.6364 ) );
		}
		return AirMass;
	}

	//------------------------------------------------------------------------------

	void
	ASHRAETauModel(
		Real64 const ETR, // extraterrestrial normal irradiance, W/m2
		Real64 const CosZen, // COS( solar zenith angle), 0 - 1
		Real64 const TauB, // beam tau factor
		Real64 const TauD, // dif tau factor
		Real64 & IDirN, // returned: direct (beam) irradiance on normal surface, W/m2
		Real64 & IDifH, // returned: diffuse irradiance on horiz surface, W/m2
		Real64 & IGlbH // returned: global irradiance on horiz surface, W/m2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         C Barnaby
		//       DATE WRITTEN   Nov 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate clear-sky direct and diffuse irradiance using ASHRAE "tau" model

		// METHODOLOGY EMPLOYED:
		// Eqns (17-18), ASHRAE HOF 2009, p. 14.9

		// REFERENCES:
		// ASHRAE HOF 2009 Chapter 14

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 AB; // air mass exponents
		Real64 AD;
		Real64 M; // air mass

		if ( CosZen < SunIsUpValue || TauB <= 0.0 || TauD <= 0.0 ) {
			IDirN = 0.0;
			IDifH = 0.0;
			IGlbH = 0.0;
		} else {
			AB = 1.219 - 0.043 * TauB - 0.151 * TauD - 0.204 * TauB * TauD;
			AD = 0.202 + 0.852 * TauB - 0.007 * TauD - 0.357 * TauB * TauD;
			M = AirMass( CosZen );
			IDirN = ETR * std::exp( -TauB * std::pow( M, AB ) );
			IDifH = ETR * std::exp( -TauD * std::pow( M, AD ) );
			IGlbH = IDirN * CosZen + IDifH;
		}

	}

	void
	AllocateWeatherData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine allocates the weather data structures (Today, Tomorrow,
		// Design Day) to the proper number of "time steps in hour" requested by the user.
		// Interpolation of data is done later after either setting up the design day (hourly
		// data) or reading in hourly weather data.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		TodayIsRain.allocate( NumOfTimeStepInHour, 24 );
		TodayIsRain = false;
		TodayIsSnow.allocate( NumOfTimeStepInHour, 24 );
		TodayIsSnow = false;
		TodayOutDryBulbTemp.allocate( NumOfTimeStepInHour, 24 );
		TodayOutDryBulbTemp = 0.0;
		TodayOutDewPointTemp.allocate( NumOfTimeStepInHour, 24 );
		TodayOutDewPointTemp = 0.0;
		TodayOutBaroPress.allocate( NumOfTimeStepInHour, 24 );
		TodayOutBaroPress = 0.0;
		TodayOutRelHum.allocate( NumOfTimeStepInHour, 24 );
		TodayOutRelHum = 0.0;
		TodayWindSpeed.allocate( NumOfTimeStepInHour, 24 );
		TodayWindSpeed = 0.0;
		TodayWindDir.allocate( NumOfTimeStepInHour, 24 );
		TodayWindDir = 0.0;
		TodaySkyTemp.allocate( NumOfTimeStepInHour, 24 );
		TodaySkyTemp = 0.0;
		TodayHorizIRSky.allocate( NumOfTimeStepInHour, 24 );
		TodayHorizIRSky = 0.0;
		TodayBeamSolarRad.allocate( NumOfTimeStepInHour, 24 );
		TodayBeamSolarRad = 0.0;
		TodayDifSolarRad.allocate( NumOfTimeStepInHour, 24 );
		TodayDifSolarRad = 0.0;
		TodayAlbedo.allocate( NumOfTimeStepInHour, 24 );
		TodayAlbedo = 0.0;
		TodayLiquidPrecip.allocate( NumOfTimeStepInHour, 24 );
		TodayLiquidPrecip = 0.0;

		TomorrowIsRain.allocate( NumOfTimeStepInHour, 24 );
		TomorrowIsRain = false;
		TomorrowIsSnow.allocate( NumOfTimeStepInHour, 24 );
		TomorrowIsSnow = false;
		TomorrowOutDryBulbTemp.allocate( NumOfTimeStepInHour, 24 );
		TomorrowOutDryBulbTemp = 0.0;
		TomorrowOutDewPointTemp.allocate( NumOfTimeStepInHour, 24 );
		TomorrowOutDewPointTemp = 0.0;
		TomorrowOutBaroPress.allocate( NumOfTimeStepInHour, 24 );
		TomorrowOutBaroPress = 0.0;
		TomorrowOutRelHum.allocate( NumOfTimeStepInHour, 24 );
		TomorrowOutRelHum = 0.0;
		TomorrowWindSpeed.allocate( NumOfTimeStepInHour, 24 );
		TomorrowWindSpeed = 0.0;
		TomorrowWindDir.allocate( NumOfTimeStepInHour, 24 );
		TomorrowWindDir = 0.0;
		TomorrowSkyTemp.allocate( NumOfTimeStepInHour, 24 );
		TomorrowSkyTemp = 0.0;
		TomorrowHorizIRSky.allocate( NumOfTimeStepInHour, 24 );
		TomorrowHorizIRSky = 0.0;
		TomorrowBeamSolarRad.allocate( NumOfTimeStepInHour, 24 );
		TomorrowBeamSolarRad = 0.0;
		TomorrowDifSolarRad.allocate( NumOfTimeStepInHour, 24 );
		TomorrowDifSolarRad = 0.0;
		TomorrowAlbedo.allocate( NumOfTimeStepInHour, 24 );
		TomorrowAlbedo = 0.0;
		TomorrowLiquidPrecip.allocate( NumOfTimeStepInHour, 24 );
		TomorrowLiquidPrecip = 0.0;

	}

	void
	CalculateDailySolarCoeffs(
		int const DayOfYear, // Day of year (1 - 366)
		Real64 & A, // ASHRAE "A" - Apparent solar irradiation at air mass = 0 [W/M**2]
		Real64 & B, // ASHRAE "B" - Atmospheric extinction coefficient
		Real64 & C, // ASHRAE "C" - Diffuse radiation factor
		Real64 & AnnVarSolConstant, // Annual variation in the solar constant
		Real64 & EquationOfTime, // Equation of Time
		Real64 & SineSolarDeclination, // Sine of Solar Declination
		Real64 & CosineSolarDeclination // Cosine of Solar Declination
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   May 1985
		//       MODIFIED       1999 for EnergyPlus
		//       RE-ENGINEERED  2001; LKL; Remove need for English -> SI conversion
		//                      Implement Tarp "fix" for Southern Hemisphere

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the daily solar coefficients used in other
		// calculations.  Specifically, this routine computes values of the solar declination, equation
		// of time, and ashrae sky coefficients a, b, and c for a given
		// day of the year.

		// METHODOLOGY EMPLOYED:
		// The method is the same as that recommended in the ASHRAE loads
		// algorithms manual, except that the fourier series expressions
		// have been extended by two terms for greater accuracy.
		// coefficients for the new expressions were determined at USACERL
		// using data from the cited references.

		// REFERENCES:
		// J. L. Threlkeld, "Thermal Environmental Engineering", 1970,
		// p.316, for declination and equation of time.
		// "ASHRAE Handbook of Fundamentals", 1972, p.387 for sky
		// coefficients a, b, and c.
		// See SUN3 in SolarShading. See SUN2 in BLAST.  See SUN3 in Tarp.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DayCorrection( Pi * 2.0 / 366.0 );
		static Array1D< Real64 > const SineSolDeclCoef( 9, { 0.00561800, 0.0657911, -0.392779, 0.00064440, -0.00618495, -0.00010101, -0.00007951, -0.00011691, 0.00002096 } ); // Fitted coefficients of Fourier series | Sine of declination coefficients
		static Array1D< Real64 > const EqOfTimeCoef( 9, { 0.00021971, -0.122649, 0.00762856, -0.156308, -0.0530028, -0.00388702, -0.00123978, -0.00270502, -0.00167992 } ); // Fitted coefficients of Fourier Series | Equation of Time coefficients
		static Array1D< Real64 > const ASHRAE_A_Coef( 9, { 1161.6685, 1.1554, 77.3575, -0.5359, -3.7622, 0.9875, -3.3924, -1.7445, 1.1198 } ); // Fitted coefficients of Fourier Series | ASHRAE A Factor coefficients
		// English (original) units:
		//              368.49341,.366502,24.538624,-.169983,-1.193417,            &
		//              .313261,-1.076093,-.543376,.355197 ,                       &

		static Array1D< Real64 > const ASHRAE_B_Coef( 9, { 0.171631, -0.00400448, -0.0344923, 0.00000209, 0.00325428, -0.00085429, 0.00229562, 0.0009034, -0.0011867 } ); // Fitted coefficients of Fourier Series | ASHRAE B Factor coefficients
		static Array1D< Real64 > const ASHRAE_C_Coef( 9, { 0.0905151, -0.00322522, -0.0407966, 0.000104164, 0.00745899, -0.00086461, 0.0013111, 0.000808275, -0.00170515 } ); // Fitted coefficients of Fourier Series | ASHRAE C Factor coefficients

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 X; // Day of Year in Radians (Computed from Input DayOfYear)
		Real64 CosX; // COS(X)
		Real64 SinX; // SIN(X)

		X = DayCorrection * DayOfYear; // Convert Julian date (Day of Year) to angle X

		// Calculate sines and cosines of X
		SinX = std::sin( X );
		CosX = std::cos( X );

		SineSolarDeclination = SineSolDeclCoef( 1 ) + SineSolDeclCoef( 2 ) * SinX + SineSolDeclCoef( 3 ) * CosX + SineSolDeclCoef( 4 ) * ( SinX * CosX * 2.0 ) + SineSolDeclCoef( 5 ) * ( pow_2( CosX ) - pow_2( SinX ) ) + SineSolDeclCoef( 6 ) * ( SinX * ( pow_2( CosX ) - pow_2( SinX ) ) + CosX * ( SinX * CosX * 2.0 ) ) + SineSolDeclCoef( 7 ) * ( CosX * ( pow_2( CosX ) - pow_2( SinX ) ) - SinX * ( SinX * CosX * 2.0 ) ) + SineSolDeclCoef( 8 ) * ( 2.0 * ( SinX * CosX * 2.0 ) * ( pow_2( CosX ) - pow_2( SinX ) ) ) + SineSolDeclCoef( 9 ) * ( pow_2( pow_2( CosX ) - pow_2( SinX ) ) - pow_2( SinX * CosX * 2.0 ) );
		CosineSolarDeclination = std::sqrt( 1.0 - pow_2( SineSolarDeclination ) );

		EquationOfTime = EqOfTimeCoef( 1 ) + EqOfTimeCoef( 2 ) * SinX + EqOfTimeCoef( 3 ) * CosX + EqOfTimeCoef( 4 ) * ( SinX * CosX * 2.0 ) + EqOfTimeCoef( 5 ) * ( pow_2( CosX ) - pow_2( SinX ) ) + EqOfTimeCoef( 6 ) * ( SinX * ( pow_2( CosX ) - pow_2( SinX ) ) + CosX * ( SinX * CosX * 2.0 ) ) + EqOfTimeCoef( 7 ) * ( CosX * ( pow_2( CosX ) - pow_2( SinX ) ) - SinX * ( SinX * CosX * 2.0 ) ) + EqOfTimeCoef( 8 ) * ( 2.0 * ( SinX * CosX * 2.0 ) * ( pow_2( CosX ) - pow_2( SinX ) ) ) + EqOfTimeCoef( 9 ) * ( pow_2( pow_2( CosX ) - pow_2( SinX ) ) - pow_2( SinX * CosX * 2.0 ) );

		AnnVarSolConstant = 1.000047 + 0.000352615 * SinX + 0.0334454 * CosX;

		A = ASHRAE_A_Coef( 1 ) + ASHRAE_A_Coef( 2 ) * SinX + ASHRAE_A_Coef( 3 ) * CosX + ASHRAE_A_Coef( 4 ) * ( SinX * CosX * 2.0 ) + ASHRAE_A_Coef( 5 ) * ( pow_2( CosX ) - pow_2( SinX ) ) + ASHRAE_A_Coef( 6 ) * ( SinX * ( pow_2( CosX ) - pow_2( SinX ) ) + CosX * ( SinX * CosX * 2.0 ) ) + ASHRAE_A_Coef( 7 ) * ( CosX * ( pow_2( CosX ) - pow_2( SinX ) ) - SinX * ( SinX * CosX * 2.0 ) ) + ASHRAE_A_Coef( 8 ) * ( 2.0 * ( SinX * CosX * 2.0 ) * ( pow_2( CosX ) - pow_2( SinX ) ) ) + ASHRAE_A_Coef( 9 ) * ( pow_2( pow_2( CosX ) - pow_2( SinX ) ) - pow_2( SinX * CosX * 2.0 ) );

		//                        Compute B and C coefficients

		if ( Latitude < 0.0 ) {
			//                            If in southern hemisphere, compute B and C with a six month time shift.
			X -= Pi;
			SinX = std::sin( X );
			CosX = std::cos( X );
		}

		B = ASHRAE_B_Coef( 1 ) + ASHRAE_B_Coef( 2 ) * SinX + ASHRAE_B_Coef( 3 ) * CosX + ASHRAE_B_Coef( 4 ) * ( SinX * CosX * 2.0 ) + ASHRAE_B_Coef( 5 ) * ( pow_2( CosX ) - pow_2( SinX ) ) + ASHRAE_B_Coef( 6 ) * ( SinX * ( pow_2( CosX ) - pow_2( SinX ) ) + CosX * ( SinX * CosX * 2.0 ) ) + ASHRAE_B_Coef( 7 ) * ( CosX * ( pow_2( CosX ) - pow_2( SinX ) ) - SinX * ( SinX * CosX * 2.0 ) ) + ASHRAE_B_Coef( 8 ) * ( 2.0 * ( SinX * CosX * 2.0 ) * ( pow_2( CosX ) - pow_2( SinX ) ) ) + ASHRAE_B_Coef( 9 ) * ( pow_2( pow_2( CosX ) - pow_2( SinX ) ) - pow_2( SinX * CosX * 2.0 ) );

		C = ASHRAE_C_Coef( 1 ) + ASHRAE_C_Coef( 2 ) * SinX + ASHRAE_C_Coef( 3 ) * CosX + ASHRAE_C_Coef( 4 ) * ( SinX * CosX * 2.0 ) + ASHRAE_C_Coef( 5 ) * ( pow_2( CosX ) - pow_2( SinX ) ) + ASHRAE_C_Coef( 6 ) * ( SinX * ( pow_2( CosX ) - pow_2( SinX ) ) + CosX * ( SinX * CosX * 2.0 ) ) + ASHRAE_C_Coef( 7 ) * ( CosX * ( pow_2( CosX ) - pow_2( SinX ) ) - SinX * ( SinX * CosX * 2.0 ) ) + ASHRAE_C_Coef( 8 ) * ( 2.0 * ( SinX * CosX * 2.0 ) * ( pow_2( CosX ) - pow_2( SinX ) ) ) + ASHRAE_C_Coef( 9 ) * ( pow_2( pow_2( CosX ) - pow_2( SinX ) ) - pow_2( SinX * CosX * 2.0 ) );

	}

	void
	CalculateSunDirectionCosines(
		Real64 const TimeValue, // Current Time of Day
		Real64 const EqOfTime, // Equation of Time
		Real64 const SinSolDeclin, // Sine of Solar Declination
		Real64 const CosSolDeclin, // Cosine of Solar Declination
		Array1A< Real64 > SUNCOS
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   May 1975
		//       MODIFIED       1999 for EnergyPlus
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine computes the solar direction cosines for hourly
		// radiation calculations.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// "NECAP Engineering Manual", 1974, p.3-117

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		SUNCOS.dim( 3 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 COSH; // Cosine of hour angle
		Real64 H; // Hour angle (before noon = +)

		//                                      COMPUTE THE HOUR ANGLE
		H = ( 15.0 * ( 12.0 - ( TimeValue + EqOfTime ) ) + ( TimeZoneMeridian - Longitude ) ) * DegToRadians;
		COSH = std::cos( H );
		//                                      COMPUTE THE COSINE OF THE
		//                                      SOLAR ZENITH ANGLE.
		//                                      This is also the Sine of the Solar Altitude Angle

		SUNCOS( 3 ) = SinSolDeclin * SinLatitude + CosSolDeclin * CosLatitude * COSH;

		if ( SUNCOS( 3 ) >= SunIsUpValue ) { // If Sun above horizon, compute other direction cosines
			SUNCOS( 2 ) = SinSolDeclin * CosLatitude - CosSolDeclin * SinLatitude * COSH;
			SUNCOS( 1 ) = CosSolDeclin * std::sin( H );
		} else { // Sun is down, set to 0.0
			SUNCOS( 1 ) = 0.0;
			SUNCOS( 2 ) = 0.0;
		}

	}

	void
	DetermineSunUpDown( Array1A< Real64 > SunDirectionCosines )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines if the sun is up or down for the current
		// hour/timestep.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Sun routines from IBLAST, authored by Walton.

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		SunDirectionCosines.dim( 3 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 H; // Hour angle (before noon = +)
		Real64 SinAltitude;
		Real64 SolarAltitude;
		Real64 SolarAzimuth;
		Real64 SolarZenith;
		Real64 CosAzimuth;
		Real64 CosZenith;
		//  REAL(r64) HAngle

		// COMPUTE THE HOUR ANGLE

		if ( NumOfTimeStepInHour != 1 ) {
			HrAngle = ( 15.0 * ( 12.0 - ( CurrentTime + TodayVariables.EquationOfTime ) ) + ( TimeZoneMeridian - Longitude ) );
		} else {
			HrAngle = ( 15.0 * ( 12.0 - ( ( CurrentTime + TS1TimeOffset ) + TodayVariables.EquationOfTime ) ) + ( TimeZoneMeridian - Longitude ) );
		}
		H = HrAngle * DegToRadians;

		// Compute the Cosine of the Solar Zenith (Altitude) Angle.
		CosZenith = SinLatitude * TodayVariables.SinSolarDeclinAngle + CosLatitude * TodayVariables.CosSolarDeclinAngle * std::cos( H );

		SolarZenith = std::acos( CosZenith );
		SinAltitude = CosLatitude * TodayVariables.CosSolarDeclinAngle * std::cos( H ) + SinLatitude * TodayVariables.SinSolarDeclinAngle;
		SolarAltitude = std::asin( SinAltitude );
		CosAzimuth = -( SinLatitude * CosZenith - TodayVariables.SinSolarDeclinAngle ) / ( CosLatitude * std::sin( SolarZenith ) );
		// Following because above can yield invalid cos value.  (e.g. at south pole)
		CosAzimuth = max( CosAzimuth, -1.0 );
		CosAzimuth = min( 1.0, CosAzimuth );
		SolarAzimuth = std::acos( CosAzimuth );

		SolarAltitudeAngle = SolarAltitude / DegToRadians;
		SolarAzimuthAngle = SolarAzimuth / DegToRadians;
		if ( HrAngle < 0.0 ) {
			SolarAzimuthAngle = 360.0 - SolarAzimuthAngle;
		}

		SunDirectionCosines( 3 ) = CosZenith;
		if ( CosZenith < SunIsUpValue ) {
			SunIsUp = false;
			SunDirectionCosines( 2 ) = 0.0;
			SunDirectionCosines( 1 ) = 0.0;
		} else {
			SunIsUp = true;
			SunDirectionCosines( 2 ) = TodayVariables.SinSolarDeclinAngle * CosLatitude - TodayVariables.CosSolarDeclinAngle * SinLatitude * std::cos( H );
			SunDirectionCosines( 1 ) = TodayVariables.CosSolarDeclinAngle * std::sin( H );
		}

	}

	void
	OpenWeatherFile( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 1999
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks to see if a weather file and what kind of weather file
		// exists in the working directory and calls appropriate routines to
		// open the files and set up for use.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		{ IOFlags flags; gio::inquire( DataStringGlobals::inputWeatherFileName, flags ); WeatherFileExists = flags.exists(); }

		if ( WeatherFileExists ) {
			OpenEPlusWeatherFile( ErrorsFound, true );
		}

	}

	void
	OpenEPlusWeatherFile(
		bool & ErrorsFound, // Will be set to true if errors found
		bool const ProcessHeader // Set to true when headers should be processed (rather than just read)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   June 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine opens the EnergyPlus Weather File (in.epw) and processes
		// the initial header records.

		// METHODOLOGY EMPLOYED:
		// List directed reads, as possible.

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const Header( 8, { "LOCATION", "DESIGN CONDITIONS", "TYPICAL/EXTREME PERIODS", "GROUND TEMPERATURES", "HOLIDAYS/DAYLIGHT SAVING", "COMMENTS 1", "COMMENTS 2", "DATA PERIODS" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string Line;
		int HdLine;
		bool StillLooking;
		int endcol;
		bool EPWOpen;
		int unitnumber;

		{ IOFlags flags; gio::inquire( DataStringGlobals::inputWeatherFileName, flags ); unitnumber = flags.unit(); EPWOpen = flags.open(); }
		if ( EPWOpen ) gio::close( unitnumber );

		WeatherFileUnitNumber = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "read" ); gio::open( WeatherFileUnitNumber, DataStringGlobals::inputWeatherFileName, flags ); if ( flags.err() ) goto Label9999; }

		if ( ProcessHeader ) {
			// Read in Header Information

			// Headers should come in order
			HdLine = 1; // Look for first Header
			StillLooking = true;
			while ( StillLooking ) {
				{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> Line; if ( flags.end() ) goto Label9998; }
				endcol = len( Line );
				if ( endcol > 0 ) {
					if ( int( Line[ endcol - 1 ] ) == iUnicode_end ) {
						goto Label9997;
					}
				}
				std::string::size_type const Pos = FindNonSpace( Line );
				std::string::size_type const HdPos = index( Line, Header( HdLine ) );
				if ( Pos != HdPos ) continue;
				//      line=MakeUPPERCase(line)
				ProcessEPWHeader( Header( HdLine ), Line, ErrorsFound );
				++HdLine;
				if ( HdLine == 9 ) StillLooking = false;
			}
		} else { // Header already processed, just read
			SkipEPlusWFHeader();
		}

		return;

Label9997: ;
		ShowSevereError( "OpenWeatherFile: EPW Weather File appears to be a Unicode or binary file.", OutputFileStandard );
		ShowContinueError( "...This file cannot be read by this program. Please save as PC or Unix file and try again" );
		ShowFatalError( "Program terminates due to previous condition." );

Label9998: ;
		ShowFatalError( "OpenWeatherFile: Unexpected End-of-File on EPW Weather file, while reading header information, looking for header=" + Header( HdLine ), OutputFileStandard );

Label9999: ;
		ShowFatalError( "OpenWeatherFile: Could not OPEN EPW Weather File", OutputFileStandard );

	}

	void
	CloseWeatherFile()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine closes the open weather file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool EPWOpen;
		int unitnumber;

		//  Make sure it's open

		{ IOFlags flags; gio::inquire( DataStringGlobals::inputWeatherFileName, flags ); unitnumber = flags.unit(); EPWOpen = flags.open(); }
		if ( EPWOpen ) gio::close( unitnumber );

	}

	void
	ResolveLocationInformation( bool & ErrorsFound ) // Set to true if no location evident
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is currently the main interface between the old data
		// structure on the BLAST Weather file and the new data structure contained
		// in this module.  At some point, this subroutine will be converted
		// to read information directly from the new input file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt LocHdFormat( "('! <Site:Location>, Location Name, Latitude {N+/S- Deg}, Longitude {E+/W- Deg}, ',   ' Time Zone Number {GMT+/-}, Elevation {m}, ',   ' Standard Pressure at Elevation {Pa}, Standard RhoAir at Elevation')" );
		static gio::Fmt LocFormat( "('Site:Location',7(',',A))" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		if ( Environment( NumOfEnvrn ).KindOfEnvrn == ksRunPeriodWeather && WeatherFileExists ) {
			if ( LocationGathered ) {
				// See if "matching" location
				if ( std::abs( Latitude - WeatherFileLatitude ) > 1.0 || std::abs( Longitude - WeatherFileLongitude ) > 1.0 || std::abs( TimeZoneNumber - WeatherFileTimeZone ) > 0.0 || std::abs( Elevation - WeatherFileElevation ) / max( Elevation, 1.0 ) > 0.10 ) {
					ShowWarningError( "Weather file location will be used rather than entered (IDF) Location object." );
					ShowContinueError( "..Location object=" + LocationTitle );
					ShowContinueError( "..Weather File Location=" + WeatherFileLocationTitle );
					ShowContinueError( "..due to location differences, Latitude difference=[" + RoundSigDigits( std::abs( Latitude - WeatherFileLatitude ), 2 ) + "] degrees, Longitude difference=[" + RoundSigDigits( std::abs( Longitude - WeatherFileLongitude ), 2 ) + "] degrees." );
					ShowContinueError( "..Time Zone difference=[" + RoundSigDigits( std::abs( TimeZoneNumber - WeatherFileTimeZone ), 1 ) + "] hour(s), Elevation difference=[" + RoundSigDigits( std::abs( ( Elevation - WeatherFileElevation ) / max( Elevation, 1.0 ) ) * 100.0, 2 ) + "] percent, [" + RoundSigDigits( std::abs( Elevation - WeatherFileElevation ), 2 ) + "] meters." );
				}
			}

			LocationTitle = WeatherFileLocationTitle;
			Latitude = WeatherFileLatitude;
			Longitude = WeatherFileLongitude;
			TimeZoneNumber = WeatherFileTimeZone;
			Elevation = WeatherFileElevation;
		} else if ( ! LocationGathered ) {
			LocationTitle = "Not Entered";
			ShowSevereError( "No Location given. Must have location information for simulation." );
			ErrorsFound = true;
		}

		if ( ! ErrorsFound ) {
			StdBaroPress = StdPressureSeaLevel * std::pow( 1.0 - 2.25577e-05 * Elevation, 5.2559 );
			StdRhoAir = PsyRhoAirFnPbTdbW( StdBaroPress, constant_twenty, constant_zero );
			// Write Final Location Information to the initialization output file
			gio::write( OutputFileInits, LocHdFormat );
			gio::write( OutputFileInits, LocFormat ) << LocationTitle << RoundSigDigits( Latitude, 2 ) << RoundSigDigits( Longitude, 2 ) << RoundSigDigits( TimeZoneNumber, 2 ) << RoundSigDigits( Elevation, 2 ) << RoundSigDigits( StdBaroPress, 0 ) << RoundSigDigits( StdRhoAir, 4 );
		}

	}

	void
	CheckLocationValidity()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is checks to see whether the user specified location
		// or the weather file location (if one exists) is valid.  The standard
		// time meridian is also calculated and compared to the user supplied
		// or weather file time zone number.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Legacy subroutine CKBLDE.

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool LocationError; // Set to true if there is a problem detected
		Real64 StdTimeMerid; // Standard time meridian
		Real64 DiffCalc; // Difference between Standard Time Meridian and TimeZone

		// FLOW:

		LocationError = false;

		if ( ( Latitude == -999.0 ) && ( Longitude == -999.0 ) && ( TimeZoneNumber != -999.0 ) ) {
			ShowSevereError( "No location specified" );
			LocationError = true;
		}

		if ( ( Latitude < -90.0 ) || ( Latitude > 90.0 ) ) {
			ShowSevereError( "Latitude must be between -90 and 90; Entered=" + RoundSigDigits( Latitude, 2 ) );
			LocationError = true;
		}

		if ( ( Longitude < -180.0 ) || ( Longitude > 180.0 ) ) {
			ShowSevereError( "Longitude must be between -180 and 180; Entered=" + RoundSigDigits( Longitude, 2 ) );
			LocationError = true;
		}

		if ( ( TimeZoneNumber < -12.00 ) || ( TimeZoneNumber > 14.00 ) ) {
			ShowSevereError( "Time Zone must be between -12 and +14; Entered=" + RoundSigDigits( TimeZoneNumber, 2 ) );
			LocationError = true;
		}

		StdTimeMerid = GetSTM( Longitude ); // Obtain the standard time meridian.

		// Bias at +/- 12 for StdTimeMerid
		//  IF (StdTimeMerid == -12.0 .and. TimeZoneNumber > 0) THEN
		//    StdTimeMerid=12.0
		//  ELSEIF (StdTimeMerid == 12.0 .and. TimeZoneNumber < 0) THEN
		//    StdTimeMerid=-12.0
		//  ENDIF

		// Compare the standard time meridian with the time zone number.  If
		// different, notify the user.  If StdTimeMerid couldn't be calculated,
		// produce an error message.

		if ( StdTimeMerid >= -12.0 && StdTimeMerid <= 12.0 ) {
			if ( TimeZoneNumber != StdTimeMerid ) {
				DiffCalc = std::abs( TimeZoneNumber - StdTimeMerid );
				if ( DiffCalc > 1.0 && DiffCalc < 24.0 ) {
					if ( DiffCalc < 3.0 ) {
						ShowWarningError( "Standard Time Meridian and Time Zone differ by more than 1, Difference=\"" + RoundSigDigits( DiffCalc, 1 ) + "\"" );
						ShowContinueError( "Solar Positions may be incorrect" );
					} else {
						ShowSevereError( "Standard Time Meridian and Time Zone differ by more than 2, Difference=\"" + RoundSigDigits( DiffCalc, 1 ) + "\"" );
						ShowContinueError( "Solar Positions will be incorrect" );
						//          LocationError=.TRUE.
					}
				}
			}
		} else {
			ShowSevereError( "Unable to calculate the standard time meridian" );
			LocationError = true;
		}

		// Error handling:  if there are any errors in the location information
		// the simulation must be terminated

		if ( LocationError ) {
			ShowFatalError( "Due to previous error condition, simulation terminated" );
		}

		if ( TimeZoneNumber <= 12.00 ) {
			TimeZoneMeridian = TimeZoneNumber * 15.0;
		} else {
			TimeZoneMeridian = TimeZoneNumber * 15.0 - 360.0;
		}
		SinLatitude = std::sin( DegToRadians * Latitude );
		CosLatitude = std::cos( DegToRadians * Latitude );

		if ( Latitude == 0.0 && Longitude == 0.0 && TimeZoneNumber == 0.0 ) {
			ShowWarningError( "Did you realize that you have Latitude=0.0, Longitude=0.0 and TimeZone=0.0?  Your building site is in the middle of the Atlantic Ocean." );
		}

	}

	void
	CheckWeatherFileValidity()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 1977
		//       MODIFIED       June 1997 (RKS)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine contains a portion of the legacy subroutine CKBLDE.
		// The main purpose of this routine is to check the validity of the
		// weather dates provided by the user and the attached weather file.
		// These functions may eventually be pushed to an interface.  This
		// routine also sends the weather file header information at the
		// Environment derived type.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Legacy subroutine CKBLDE.

		// Using/Aliasing
		using General::JulianDay;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		ErrorInWeatherFile = false;
		if ( ! WeatherFileExists ) { // No weather file exists but the user requested one--print error message

			if ( DoWeathSim ) {
				ShowWarningError( "Weather Environment(s) requested, but no weather file found" );
				ErrorInWeatherFile = true;
			}

		} // ... end of WeatherFileExists IF-THEN

	}

	void
	ReportOutputFileHeaders()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine prints out the necessary header information required
		// by the EnergyPlus output file format.  This subroutine can be
		// replicated in any other modules which must send data to the output
		// file.

		// METHODOLOGY EMPLOYED:
		// For each report, the report flag integer must be saved from the
		// global report number counter.  Then, the report counter must be
		// incremented.  Finally, the header information for the report must
		// be sent to the output file.

		// REFERENCES:
		// EnergyPlus Output Description document.

		// Using/Aliasing
		using OutputProcessor::TimeStepStampReportNbr;
		using OutputProcessor::DailyStampReportNbr;
		using OutputProcessor::MonthlyStampReportNbr;
		using OutputProcessor::RunPeriodStampReportNbr;
		using OutputProcessor::TimeStepStampReportChr;
		using OutputProcessor::DailyStampReportChr;
		using OutputProcessor::MonthlyStampReportChr;
		using OutputProcessor::RunPeriodStampReportChr;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt IntFmt( "(I3)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Format descriptor for the environment title
		static gio::Fmt EnvironmentFormat( "(a,',5,Environment Title[],Latitude[deg],Longitude[deg],Time Zone[],Elevation[m]')" );
		static gio::Fmt TimeStepFormat( "(a,',6,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType')" );
		static gio::Fmt DailyFormat( "(a,',3,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily ',A,' Requested')" );
		static gio::Fmt MonthlyFormat( "(a,',2,Cumulative Days of Simulation[],Month[]  ! When Monthly ',A,' Requested')" );
		static gio::Fmt RunPeriodFormat( "(a,',1,Cumulative Days of Simulation[] ! When Run Period ',A,' Requested')" );

		// FLOW:

		AssignReportNumber( EnvironmentReportNbr );
		if ( EnvironmentReportNbr != 1 ) { //  problem
			ShowFatalError( "ReportOutputFileHeaders: Assigned report number for Environment title is not 1.  Contact Support." );
		}
		gio::write( EnvironmentReportChr, IntFmt ) << EnvironmentReportNbr;
		strip( EnvironmentReportChr );
		gio::write( OutputFileStandard, EnvironmentFormat ) << EnvironmentReportChr;
		gio::write( OutputFileMeters, EnvironmentFormat ) << EnvironmentReportChr;

		AssignReportNumber( TimeStepStampReportNbr );
		gio::write( TimeStepStampReportChr, IntFmt ) << TimeStepStampReportNbr;
		strip( TimeStepStampReportChr );
		gio::write( OutputFileStandard, TimeStepFormat ) << TimeStepStampReportChr;
		gio::write( OutputFileMeters, TimeStepFormat ) << TimeStepStampReportChr;

		AssignReportNumber( DailyStampReportNbr );
		gio::write( DailyStampReportChr, IntFmt ) << DailyStampReportNbr;
		strip( DailyStampReportChr );
		gio::write( OutputFileStandard, DailyFormat ) << DailyStampReportChr << "Report Variables";
		gio::write( OutputFileMeters, DailyFormat ) << DailyStampReportChr << "Meters";

		AssignReportNumber( MonthlyStampReportNbr );
		gio::write( MonthlyStampReportChr, IntFmt ) << MonthlyStampReportNbr;
		strip( MonthlyStampReportChr );
		gio::write( OutputFileStandard, MonthlyFormat ) << MonthlyStampReportChr << "Report Variables";
		gio::write( OutputFileMeters, MonthlyFormat ) << MonthlyStampReportChr << "Meters";

		AssignReportNumber( RunPeriodStampReportNbr );
		gio::write( RunPeriodStampReportChr, IntFmt ) << RunPeriodStampReportNbr;
		strip( RunPeriodStampReportChr );
		gio::write( OutputFileStandard, RunPeriodFormat ) << RunPeriodStampReportChr << "Report Variables";
		gio::write( OutputFileMeters, RunPeriodFormat ) << RunPeriodStampReportChr << "Meters";

	}

	void
	ReportWeatherAndTimeInformation( bool & PrintEnvrnStamp ) // Set to true when the environment header should be printed
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver of the weather reporting.  This
		// routine is also responsible for printing the time and environment
		// stamps.

		// METHODOLOGY EMPLOYED:
		// Reporting is only done for non-warmup days.  The environment stamp
		// is only reported at the beginning of an environment, but after the
		// warmup days (to allow all modules to print the report headers to the
		// output file.  This is controlled by the PrintEnvrnStamp variable
		// which is passed in and reset if necessary.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//unused0909  USE DataSystemVariables, ONLY: ReportDuringWarmup

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt EndOfHeaderFormat( "('End of Data Dictionary')" ); // End of data dictionary marker
		static gio::Fmt EnvironmentStampFormat( "(a,',',a,3(',',f7.2),',',f7.2)" ); // Format descriptor for environ stamp
		//  CHARACTER(len=*), PARAMETER :: TimeStampFormat = "(i3,',',i4,',',i2,',',i2,',',i2)" ! Format descriptor for the date/time stamp

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

		// Report the time stamp and the current weather to the output file

		if ( ! WarmupFlag && ! RPReadAllWeatherData ) { // Write the required output information

			// The first time through in a non-warmup day, the environment header
			// must be printed.  This must be done here and not in the generic
			// BeginEnvrnFlag block above because other modules in the simulation
			// must also print out header information.  This can be done during
			// the simulation warmup if the environment stamp printing is delayed
			// until the warmup is completed.  The stamp should only be printed once
			// per environment (set/reset of PrintEnvrnStamp).  In addition, before
			// the first environment, the end of the header block flag must also be
			// sent to the output file.

			if ( PrintEnvrnStamp ) {

				if ( PrintEndDataDictionary && DoOutputReporting ) {
					gio::write( OutputFileStandard, EndOfHeaderFormat );
					gio::write( OutputFileMeters, EndOfHeaderFormat );
					PrintEndDataDictionary = false;
				}
				if ( DoOutputReporting ) {
					std::string const & Title( Environment( Envrn ).Title );
					gio::write( OutputFileStandard, EnvironmentStampFormat ) << EnvironmentReportChr << Title << Latitude << Longitude << TimeZoneNumber << Elevation;
					gio::write( OutputFileMeters, EnvironmentStampFormat ) << EnvironmentReportChr << Title << Latitude << Longitude << TimeZoneNumber << Elevation;
					PrintEnvrnStamp = false;
				}

			}
		} // ... end of .NOT.WarmupFlag IF-THEN block.

	}

	void
	ReadUserWeatherInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   September 1997
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver of the weather manager module.
		// It controls the assignment of weather related global variables as
		// well as the reads and writes for retrieving weather information.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using namespace DataSystemVariables;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Env; // Environment Loop Counter
		static bool ErrorsFound( false );
		int RPD1;
		int RPD2;
		int RP; // number of run periods
		int RPAW; // number of run periods, actual weather

		// FLOW:

		//Get the number of design days and annual runs from user inpout
		TotDesDays = GetNumObjectsFound( "SizingPeriod:DesignDay" );
		RPD1 = GetNumObjectsFound( "SizingPeriod:WeatherFileDays" );
		RPD2 = GetNumObjectsFound( "SizingPeriod:WeatherFileConditionType" );
		RP = GetNumObjectsFound( "RunPeriod" );
		RPAW = GetNumObjectsFound( "RunPeriod:CustomRange" );
		TotRunPers = RP + RPAW;
		NumOfEnvrn = TotDesDays + TotRunPers + RPD1 + RPD2;
		if ( TotRunPers > 0 ) {
			WeathSimReq = true;
		} else {
			WeathSimReq = false;
		}

		SPSiteScheduleNamePtr.allocate( TotDesDays * 5 );
		SPSiteScheduleUnits.allocate( TotDesDays * 5 );

		SPSiteScheduleNamePtr = 0;
		SPSiteScheduleUnits = BlankString;

		//Allocate the Design Day and Environment array to the # of DD's or/and
		// Annual runs on input file
		DesignDay.allocate( TotDesDays );
		Environment.allocate( NumOfEnvrn );

		// Set all Environments to DesignDay and then the weather environment will be set
		//  in the get annual run data subroutine
		for ( Env = 1; Env <= TotDesDays; ++Env ) {
			Environment( Env ).KindOfEnvrn = ksDesignDay;
		}
		for ( Env = 1; Env <= RPD1 + RPD2; ++Env ) {
			if ( ! DDOnly ) {
				Environment( TotDesDays + Env ).KindOfEnvrn = ksRunPeriodDesign;
			} else {
				Environment( TotDesDays + Env ).KindOfEnvrn = ksRunPeriodWeather;
			}
		}
		for ( Env = 1; Env <= TotRunPers; ++Env ) {
			Environment( TotDesDays + RPD1 + RPD2 + Env ).KindOfEnvrn = ksRunPeriodWeather;
		}

		if ( TotDesDays >= 1 ) {
			GetDesignDayData( TotDesDays, ErrorsFound );
		}

		if ( RPD1 >= 1 || RPD2 >= 1 ) {
			GetRunPeriodDesignData( ErrorsFound );
		}

		//the last environment(s) is designated the weather environment if an annual run
		// is selected.  All of the design systems is done from the design day info
		// which will have to be completed to run the annual run.
		if ( TotRunPers >= 1 || FullAnnualRun ) {
			GetRunPeriodData( TotRunPers, ErrorsFound );
		}

		if ( RPD1 >= 1 || RPD2 >= 1 || TotRunPers >= 1 || FullAnnualRun ) {
			GetSpecialDayPeriodData( ErrorsFound );
			GetDSTData( ErrorsFound );
			if ( IDFDaylightSaving ) {
				DST = IDFDST;
			}
		}

		GetLocationInfo( ErrorsFound );

		GetGroundTemps( ErrorsFound );

		GetGroundReflectances( ErrorsFound );

		GetSnowGroundRefModifiers( ErrorsFound );

		GetWaterMainsTemperatures( ErrorsFound );

		GetWeatherStation( ErrorsFound );

		SetupEnvironmentTypes();

		GetWeatherProperties( ErrorsFound );

		// Deallocate ones used for schedule pointers
		SPSiteScheduleNamePtr.deallocate();
		SPSiteScheduleUnits.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( "GetWeatherInput: Above errors cause termination" );
		}

	}

	void
	GetRunPeriodData(
		int & TotRunPers, // Total number of Run Periods requested
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   October 1997
		//       MODIFIED       February 1999, Add multiple run periods, Change name.
		//                      March 2012, LKL, Add features to object; New "actual weather" object;
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the run period info from User input and the
		//  simulation dates

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using InputProcessor::GetNumObjectsFound;
		using General::JulianDay;
		using General::TrimSigDigits;
		using namespace DataSystemVariables;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlpha; // Number of alphas being input
		int NumNumeric; // Number of numbers being input
		int IOStat; // IO Status when calling get input subroutine
		int Loop;
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int Count;
		int RP; // number of run periods
		int RPAW; // number of run periods, actual weather
		int Ptr;
		int LocalLeapYearAdd;

		// Object Data

		// FLOW:
		RP = GetNumObjectsFound( "RunPeriod" );
		RPAW = GetNumObjectsFound( "RunPeriod:CustomRange" );

		//Call Input Get routine to retrieve annual run data
		RunPeriodInput.allocate( TotRunPers );

		cCurrentModuleObject = "RunPeriod";
		Count = 0;
		if ( ! WFAllowsLeapYears ) {
			LocalLeapYearAdd = 0;
		} else {
			LocalLeapYearAdd = 1;
		}
		for ( Loop = 1; Loop <= RP; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumeric, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( ! lAlphaFieldBlanks( 1 ) ) {
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), RunPeriodInput, &RunPeriodData::Title, Count, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
			}

			++Count;
			RunPeriodInput( Loop ).Title = cAlphaArgs( 1 );

			//set the start and end day of month from user input
			// N1 , \field Begin Month
			// N2 , \field Begin Day of Month
			// N3 , \field End Month
			// N4 , \field End Day of Month
			RunPeriodInput( Loop ).StartMonth = int( rNumericArgs( 1 ) );
			RunPeriodInput( Loop ).StartDay = int( rNumericArgs( 2 ) );
			RunPeriodInput( Loop ).EndMonth = int( rNumericArgs( 3 ) );
			RunPeriodInput( Loop ).EndDay = int( rNumericArgs( 4 ) );

			//  N5,  \field Number of Times Runperiod to be Repeated
			if ( int( rNumericArgs( 5 ) ) == 0 ) {
				RunPeriodInput( Loop ).NumSimYears = 1;
			} else {
				RunPeriodInput( Loop ).NumSimYears = int( rNumericArgs( 5 ) );
			}

			//  N6;  \field Start Year
			if ( int( rNumericArgs( 6 ) ) == 0 ) {
				RunPeriodInput( Loop ).BeginYear = AutoCalculate;
				RunPeriodInput( Loop ).TreatYearsAsConsecutive = false;
			} else {
				RunPeriodInput( Loop ).BeginYear = int( rNumericArgs( 6 ) );
				RunPeriodInput( Loop ).TreatYearsAsConsecutive = true;
			}

			if ( FullAnnualRun && Loop == 1 ) {
				RunPeriodInput( Loop ).StartMonth = 1;
				RunPeriodInput( Loop ).StartDay = 1;
				RunPeriodInput( Loop ).EndMonth = 12;
				RunPeriodInput( Loop ).EndDay = 31;
			}

			{ auto const SELECT_CASE_var( RunPeriodInput( Loop ).StartMonth );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( RunPeriodInput( Loop ).StartDay > 31 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( RunPeriodInput( Loop ).StartDay > 30 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( RunPeriodInput( Loop ).StartDay > 28 + LocalLeapYearAdd ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + "]." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + ']' );
				ErrorsFound = true;
			}}

			{ auto const SELECT_CASE_var( RunPeriodInput( Loop ).EndMonth );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( RunPeriodInput( Loop ).EndDay > 31 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 4 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 3 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( RunPeriodInput( Loop ).EndDay > 30 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 4 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 3 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( RunPeriodInput( Loop ).EndDay > 28 + LocalLeapYearAdd ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 4 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 3 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + "]." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cNumericFieldNames( 3 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + ']' );
				ErrorsFound = true;
			}}

			// A2 , \field Day of Week for Start Day
			if ( lAlphaFieldBlanks( 2 ) || cAlphaArgs( 2 ) == "USEWEATHERFILE" ) {
				RunPeriodInput( Loop ).DayOfWeek = 0; // Defaults to Day of Week from Weather File
			} else {
				RunPeriodInput( Loop ).DayOfWeek = FindItemInList( cAlphaArgs( 2 ), DaysOfWeek, 7 );
				if ( RunPeriodInput( Loop ).DayOfWeek == 0 ) {
					ShowWarningError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 2 ) + " invalid (Day of Week) [" + cAlphaArgs( 2 ) + " for Start is not Valid, DayofWeek from WeatherFile will be used." );
				}
			}

			// A3,  \field Use Weather File Holidays and Special Days
			if ( lAlphaFieldBlanks( 3 ) || SameString( cAlphaArgs( 3 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseHolidays = true;
			} else if ( SameString( cAlphaArgs( 3 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseHolidays = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 3 ) + " invalid [" + cAlphaArgs( 3 ) + ']' );
				ErrorsFound = true;
			}

			// A4,  \field Use Weather File Daylight Saving Period
			if ( lAlphaFieldBlanks( 4 ) || SameString( cAlphaArgs( 4 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseDST = true;
			} else if ( SameString( cAlphaArgs( 4 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseDST = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 4 ) + " invalid [" + cAlphaArgs( 4 ) + ']' );
				ErrorsFound = true;
			}

			// A5,  \field Apply Weekend Holiday Rule
			if ( lAlphaFieldBlanks( 5 ) || SameString( cAlphaArgs( 5 ), "YES" ) ) {
				RunPeriodInput( Loop ).ApplyWeekendRule = true;
			} else if ( SameString( cAlphaArgs( 5 ), "NO" ) ) {
				RunPeriodInput( Loop ).ApplyWeekendRule = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 5 ) + " invalid [" + cAlphaArgs( 5 ) + ']' );
				ErrorsFound = true;
			}

			// A6,  \field Use Weather File Rain Indicators
			if ( lAlphaFieldBlanks( 6 ) || SameString( cAlphaArgs( 6 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseRain = true;
			} else if ( SameString( cAlphaArgs( 6 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseRain = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 6 ) + " invalid [" + cAlphaArgs( 6 ) + ']' );
				ErrorsFound = true;
			}

			// A7,  \field Use Weather File Snow Indicators
			if ( lAlphaFieldBlanks( 7 ) || SameString( cAlphaArgs( 7 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseSnow = true;
			} else if ( SameString( cAlphaArgs( 7 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseSnow = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 7 ) + " invalid [" + cAlphaArgs( 7 ) + ']' );
				ErrorsFound = true;
			}

			// A8,  \field Increment Day of Week on repeat
			if ( lAlphaFieldBlanks( 8 ) || SameString( cAlphaArgs( 8 ), "YES" ) ) {
				RunPeriodInput( Loop ).RollDayTypeOnRepeat = true;
			} else if ( SameString( cAlphaArgs( 8 ), "NO" ) ) {
				RunPeriodInput( Loop ).RollDayTypeOnRepeat = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ' ' + cAlphaFieldNames( 8 ) + " invalid [" + cAlphaArgs( 8 ) + ']' );
				ErrorsFound = true;
			}

			//calculate the annual start and end dates from the user inputted month and day
			RunPeriodInput( Loop ).StartDate = JulianDay( RunPeriodInput( Loop ).StartMonth, RunPeriodInput( Loop ).StartDay, LeapYearAdd );
			RunPeriodInput( Loop ).EndDate = JulianDay( RunPeriodInput( Loop ).EndMonth, RunPeriodInput( Loop ).EndDay, LeapYearAdd );
			RunPeriodInput( Loop ).MonWeekDay = 0;
			if ( RunPeriodInput( Loop ).DayOfWeek != 0 && ! ErrorsFound ) {
				SetupWeekDaysByMonth( RunPeriodInput( Loop ).StartMonth, RunPeriodInput( Loop ).StartDay, RunPeriodInput( Loop ).DayOfWeek, RunPeriodInput( Loop ).MonWeekDay );
			}
		}

		cCurrentModuleObject = "RunPeriod:CustomRange";
		Count = 0;
		for ( Ptr = 1; Ptr <= RPAW; ++Ptr ) {
			GetObjectItem( cCurrentModuleObject, Ptr, cAlphaArgs, NumAlpha, rNumericArgs, NumNumeric, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( ! lAlphaFieldBlanks( 1 ) ) {
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), RunPeriodInput, &RunPeriodData::Title, Count, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
			}

			++Count;
			Loop = RP + Ptr;
			RunPeriodInput( Loop ).Title = cAlphaArgs( 1 );

			//set the start and end day of month from user input
			// N1 , \field Begin Month
			// N2 , \field Begin Day of Month
			// N3,  \field Start Year
			// N4 , \field End Month
			// N5 , \field End Day of Month
			// N6,  \field End Year
			RunPeriodInput( Loop ).StartMonth = int( rNumericArgs( 1 ) );
			RunPeriodInput( Loop ).StartDay = int( rNumericArgs( 2 ) );
			RunPeriodInput( Loop ).StartYear = int( rNumericArgs( 3 ) );
			RunPeriodInput( Loop ).EndMonth = int( rNumericArgs( 4 ) );
			RunPeriodInput( Loop ).EndDay = int( rNumericArgs( 5 ) );
			RunPeriodInput( Loop ).EndYear = int( rNumericArgs( 6 ) );
			RunPeriodInput( Loop ).TreatYearsAsConsecutive = true;

			if ( FullAnnualRun && Loop == 1 ) {
				RunPeriodInput( Loop ).StartMonth = 1;
				RunPeriodInput( Loop ).StartDay = 1;
				RunPeriodInput( Loop ).EndMonth = 12;
				RunPeriodInput( Loop ).EndDay = 31;
			}

			{ auto const SELECT_CASE_var( RunPeriodInput( Loop ).StartMonth );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( RunPeriodInput( Loop ).StartDay > 31 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( RunPeriodInput( Loop ).StartDay > 30 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( RunPeriodInput( Loop ).StartDay > 28 + LeapYearAdd ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + "]." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ' ' + cNumericFieldNames( 2 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + ']' );
				ErrorsFound = true;
			}}

			{ auto const SELECT_CASE_var( RunPeriodInput( Loop ).EndMonth );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( RunPeriodInput( Loop ).EndDay > 31 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 4 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 3 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( RunPeriodInput( Loop ).EndDay > 30 ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 4 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 3 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + "]." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( RunPeriodInput( Loop ).EndDay > 28 + LeapYearAdd ) {
					ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + ", " + cNumericFieldNames( 4 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndDay ) + ']' );
					ShowContinueError( "Indicated " + cNumericFieldNames( 3 ) + "=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + "]." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cNumericFieldNames( 3 ) + " invalid=[" + TrimSigDigits( RunPeriodInput( Loop ).EndMonth ) + ']' );
				ErrorsFound = true;
			}}

			// A2 , \field Day of Week for Start Day
			if ( lAlphaFieldBlanks( 2 ) || cAlphaArgs( 2 ) == "USEWEATHERFILE" ) {
				RunPeriodInput( Loop ).DayOfWeek = 0; // Defaults to Day of Week from Weather File
			} else {
				RunPeriodInput( Loop ).DayOfWeek = FindItemInList( cAlphaArgs( 2 ), DaysOfWeek, 7 );
				if ( RunPeriodInput( Loop ).DayOfWeek == 0 ) {
					ShowWarningError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 2 ) + " invalid (Day of Week) [" + cAlphaArgs( 2 ) + " for Start is not Valid, DayofWeek from WeatherFile will be used." );
				}
			}

			// A3,  \field Use Weather File Holidays and Special Days
			if ( lAlphaFieldBlanks( 3 ) || SameString( cAlphaArgs( 3 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseHolidays = true;
			} else if ( SameString( cAlphaArgs( 3 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseHolidays = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 3 ) + " invalid [" + cAlphaArgs( 3 ) + ']' );
				ErrorsFound = true;
			}

			// A4,  \field Use Weather File Daylight Saving Period
			if ( lAlphaFieldBlanks( 4 ) || SameString( cAlphaArgs( 4 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseDST = true;
			} else if ( SameString( cAlphaArgs( 4 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseDST = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 4 ) + " invalid [" + cAlphaArgs( 4 ) + ']' );
				ErrorsFound = true;
			}

			// A5,  \field Apply Weekend Holiday Rule
			if ( lAlphaFieldBlanks( 5 ) || SameString( cAlphaArgs( 5 ), "YES" ) ) {
				RunPeriodInput( Loop ).ApplyWeekendRule = true;
			} else if ( SameString( cAlphaArgs( 5 ), "NO" ) ) {
				RunPeriodInput( Loop ).ApplyWeekendRule = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 5 ) + " invalid [" + cAlphaArgs( 5 ) + ']' );
				ErrorsFound = true;
			}

			// A6,  \field Use Weather File Rain Indicators
			if ( lAlphaFieldBlanks( 6 ) || SameString( cAlphaArgs( 6 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseRain = true;
			} else if ( SameString( cAlphaArgs( 6 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseRain = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 6 ) + " invalid [" + cAlphaArgs( 6 ) + ']' );
				ErrorsFound = true;
			}

			// A7,  \field Use Weather File Snow Indicators
			if ( lAlphaFieldBlanks( 7 ) || SameString( cAlphaArgs( 7 ), "YES" ) ) {
				RunPeriodInput( Loop ).UseSnow = true;
			} else if ( SameString( cAlphaArgs( 7 ), "NO" ) ) {
				RunPeriodInput( Loop ).UseSnow = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 7 ) + " invalid [" + cAlphaArgs( 7 ) + ']' );
				ErrorsFound = true;
			}

			//calculate the annual start and end days from the user inputted month and day
			RunPeriodInput( Loop ).ActualWeather = true;
			JGDate( GregorianToJulian, RunPeriodInput( Loop ).StartDate, RunPeriodInput( Loop ).StartYear, RunPeriodInput( Loop ).StartMonth, RunPeriodInput( Loop ).StartDay );
			JGDate( GregorianToJulian, RunPeriodInput( Loop ).EndDate, RunPeriodInput( Loop ).EndYear, RunPeriodInput( Loop ).EndMonth, RunPeriodInput( Loop ).EndDay );
			RunPeriodInput( Loop ).MonWeekDay = 0;
			if ( RunPeriodInput( Loop ).DayOfWeek != 0 && ! ErrorsFound ) {
				SetupWeekDaysByMonth( RunPeriodInput( Loop ).StartMonth, RunPeriodInput( Loop ).StartDay, RunPeriodInput( Loop ).DayOfWeek, RunPeriodInput( Loop ).MonWeekDay );
			}
		}

		if ( TotRunPers == 0 && FullAnnualRun ) {
			ShowWarningError( "No Run Periods input but Full Annual Simulation selected.  Adding Run Period to 1/1 through 12/31." );
			Environment.redimension( ++NumOfEnvrn );
			Environment( NumOfEnvrn ).KindOfEnvrn = ksRunPeriodWeather;
			TotRunPers = 1;
			WeathSimReq = true;
			RunPeriodInput.allocate( TotRunPers );
			RunPeriodInput( 1 ).StartDate = JulianDay( RunPeriodInput( 1 ).StartMonth, RunPeriodInput( 1 ).StartDay, LeapYearAdd );
			RunPeriodInput( 1 ).EndDate = JulianDay( RunPeriodInput( 1 ).EndMonth, RunPeriodInput( 1 ).EndDay, LeapYearAdd );
			RunPeriodInput( 1 ).MonWeekDay = 0;
			if ( RunPeriodInput( 1 ).DayOfWeek != 0 && ! ErrorsFound ) {
				SetupWeekDaysByMonth( RunPeriodInput( 1 ).StartMonth, RunPeriodInput( 1 ).StartDay, RunPeriodInput( 1 ).DayOfWeek, RunPeriodInput( 1 ).MonWeekDay );
			}
		}
	}

	void
	GetRunPeriodDesignData( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the run period design info from User input and the
		//  simulation dates

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItem;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using General::JulianDay;
		using General::TrimSigDigits;
		using namespace DataSystemVariables;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const ValidNames( 12, { "SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "HOLIDAY", "SUMMERDESIGNDAY", "WINTERDESIGNDAY", "CUSTOMDAY1", "CUSTOMDAY2" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // Number of alphas being input
		int NumNumerics; // Number of Numerics being input
		int IOStat; // IO Status when calling get input subroutine
		int Loop;
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int RPD1;
		int RPD2;
		int Count;
		int WhichPeriod;
		//unused1208  CHARACTER(len=MaxNameLength) :: ThisObject

		// FLOW:
		//Call Input Get routine to retrieve annual run data
		RPD1 = GetNumObjectsFound( "SizingPeriod:WeatherFileDays" );
		RPD2 = GetNumObjectsFound( "SizingPeriod:WeatherFileConditionType" );
		TotRunDesPers = RPD1 + RPD2;

		RunPeriodDesignInput.allocate( RPD1 + RPD2 );

		Count = 0;
		cCurrentModuleObject = "SizingPeriod:WeatherFileDays";
		for ( Loop = 1; Loop <= RPD1; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumerics, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), RunPeriodDesignInput, &RunPeriodData::Title, Count, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			++Count;
			RunPeriodDesignInput( Count ).Title = cAlphaArgs( 1 );
			RunPeriodDesignInput( Count ).PeriodType = "User Selected WeatherFile RunPeriod (Design)";

			//set the start and end day of month from user input
			RunPeriodDesignInput( Count ).StartMonth = int( rNumericArgs( 1 ) );
			RunPeriodDesignInput( Count ).StartDay = int( rNumericArgs( 2 ) );
			RunPeriodDesignInput( Count ).EndMonth = int( rNumericArgs( 3 ) );
			RunPeriodDesignInput( Count ).EndDay = int( rNumericArgs( 4 ) );

			{ auto const SELECT_CASE_var( RunPeriodDesignInput( Count ).StartMonth );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( RunPeriodDesignInput( Count ).StartDay > 31 ) {
					ShowSevereError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cNumericFieldNames( 2 ) + " invalid (Day of Month) [" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( RunPeriodDesignInput( Count ).StartDay > 30 ) {
					ShowSevereError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cNumericFieldNames( 2 ) + " invalid (Day of Month) [" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( RunPeriodDesignInput( Count ).StartDay > 28 + LeapYearAdd ) {
					ShowSevereError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cNumericFieldNames( 2 ) + " invalid (Day of Month) [" + TrimSigDigits( RunPeriodInput( Loop ).StartDay ) + ']' );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cNumericFieldNames( 1 ) + " invalid (Month) [" + TrimSigDigits( RunPeriodInput( Loop ).StartMonth ) + ']' );
				ErrorsFound = true;
			}}

			if ( lAlphaFieldBlanks( 2 ) ) {
				RunPeriodDesignInput( Count ).DayOfWeek = 2; // Defaults to Monday
			} else {
				RunPeriodDesignInput( Count ).DayOfWeek = FindItemInList( cAlphaArgs( 2 ), ValidNames, 12 );
				if ( RunPeriodDesignInput( Count ).DayOfWeek == 0 || RunPeriodDesignInput( Count ).DayOfWeek == 8 ) {
					ShowWarningError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cAlphaFieldNames( 1 ) + " invalid (Day of Week) [" + cAlphaArgs( 1 ) + " for Start is not Valid, Monday will be Used." );
					RunPeriodDesignInput( Count ).DayOfWeek = 2; // Defaults to Monday
				}
			}

			if ( lAlphaFieldBlanks( 3 ) || SameString( cAlphaArgs( 3 ), "YES" ) ) {
				RunPeriodDesignInput( Count ).UseDST = true;
			} else if ( SameString( cAlphaArgs( 3 ), "NO" ) ) {
				RunPeriodDesignInput( Count ).UseDST = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 3 ) + " invalid [" + cAlphaArgs( 3 ) + ']' );
				ErrorsFound = true;
			}

			if ( lAlphaFieldBlanks( 4 ) || SameString( cAlphaArgs( 4 ), "YES" ) ) {
				RunPeriodDesignInput( Count ).UseRain = true;
				RunPeriodDesignInput( Count ).UseSnow = true;
			} else if ( SameString( cAlphaArgs( 4 ), "NO" ) ) {
				RunPeriodDesignInput( Count ).UseRain = false;
				RunPeriodDesignInput( Count ).UseSnow = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 4 ) + " invalid [" + cAlphaArgs( 4 ) + ']' );
				ErrorsFound = true;
			}

			//calculate the annual start and end days from the user inputted month and day
			RunPeriodDesignInput( Count ).StartDate = JulianDay( RunPeriodDesignInput( Count ).StartMonth, RunPeriodDesignInput( Count ).StartDay, LeapYearAdd );
			RunPeriodDesignInput( Count ).EndDate = JulianDay( RunPeriodDesignInput( Count ).EndMonth, RunPeriodDesignInput( Count ).EndDay, LeapYearAdd );
			if ( RunPeriodDesignInput( Count ).StartDate <= RunPeriodDesignInput( Count ).EndDate ) {
				RunPeriodDesignInput( Count ).TotalDays = ( RunPeriodDesignInput( Count ).EndDate - RunPeriodDesignInput( Count ).StartDate + 1 ) * RunPeriodDesignInput( Count ).NumSimYears;
			} else {
				RunPeriodDesignInput( Count ).TotalDays = ( JulianDay( 12, 31, LeapYearAdd ) - RunPeriodDesignInput( Count ).StartDate + 1 + RunPeriodDesignInput( Count ).EndDate ) * RunPeriodDesignInput( Count ).NumSimYears;
			}
			RunPeriodDesignInput( Count ).MonWeekDay = 0;
			if ( RunPeriodDesignInput( 1 ).DayOfWeek != 0 && ! ErrorsFound ) {
				SetupWeekDaysByMonth( RunPeriodDesignInput( 1 ).StartMonth, RunPeriodDesignInput( 1 ).StartDay, RunPeriodDesignInput( 1 ).DayOfWeek, RunPeriodDesignInput( 1 ).MonWeekDay );
			}
		}

		cCurrentModuleObject = "SizingPeriod:WeatherFileConditionType";
		for ( Loop = 1; Loop <= RPD2; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumerics, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), RunPeriodDesignInput, &RunPeriodData::Title, Count, IsNotOK, IsBlank, cCurrentModuleObject + " Title" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			++Count;
			RunPeriodDesignInput( Count ).Title = cAlphaArgs( 1 );
			RunPeriodDesignInput( Count ).PeriodType = "User Selected WeatherFile Typical/Extreme Period (Design)=" + cAlphaArgs( 2 );

			// Period Selection
			if ( ! lAlphaFieldBlanks( 2 ) ) {
				WhichPeriod = FindItem( cAlphaArgs( 2 ), TypicalExtremePeriods, &TypicalExtremeData::MatchValue );
				if ( WhichPeriod != 0 ) {
					RunPeriodDesignInput( Count ).StartDay = TypicalExtremePeriods( WhichPeriod ).StartDay;
					RunPeriodDesignInput( Count ).StartMonth = TypicalExtremePeriods( WhichPeriod ).StartMonth;
					RunPeriodDesignInput( Count ).StartDate = TypicalExtremePeriods( WhichPeriod ).StartJDay;
					RunPeriodDesignInput( Count ).EndDay = TypicalExtremePeriods( WhichPeriod ).EndDay;
					RunPeriodDesignInput( Count ).EndMonth = TypicalExtremePeriods( WhichPeriod ).EndMonth;
					RunPeriodDesignInput( Count ).EndDate = TypicalExtremePeriods( WhichPeriod ).EndJDay;
					RunPeriodDesignInput( Count ).TotalDays = TypicalExtremePeriods( WhichPeriod ).TotalDays;
				} else {
					WhichPeriod = FindItem( cAlphaArgs( 2 ), TypicalExtremePeriods, &TypicalExtremeData::MatchValue1 );
					if ( WhichPeriod != 0 ) {
						RunPeriodDesignInput( Count ).StartDay = TypicalExtremePeriods( WhichPeriod ).StartDay;
						RunPeriodDesignInput( Count ).StartMonth = TypicalExtremePeriods( WhichPeriod ).StartMonth;
						RunPeriodDesignInput( Count ).StartDate = TypicalExtremePeriods( WhichPeriod ).StartJDay;
						RunPeriodDesignInput( Count ).EndDay = TypicalExtremePeriods( WhichPeriod ).EndDay;
						RunPeriodDesignInput( Count ).EndMonth = TypicalExtremePeriods( WhichPeriod ).EndMonth;
						RunPeriodDesignInput( Count ).EndDate = TypicalExtremePeriods( WhichPeriod ).EndJDay;
						RunPeriodDesignInput( Count ).TotalDays = TypicalExtremePeriods( WhichPeriod ).TotalDays;
						ShowWarningError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) + " matched to " + TypicalExtremePeriods( WhichPeriod ).MatchValue );
					} else {
						WhichPeriod = FindItem( cAlphaArgs( 2 ), TypicalExtremePeriods, &TypicalExtremeData::MatchValue2 );
						if ( WhichPeriod != 0 ) {
							RunPeriodDesignInput( Count ).StartDay = TypicalExtremePeriods( WhichPeriod ).StartDay;
							RunPeriodDesignInput( Count ).StartMonth = TypicalExtremePeriods( WhichPeriod ).StartMonth;
							RunPeriodDesignInput( Count ).StartDate = TypicalExtremePeriods( WhichPeriod ).StartJDay;
							RunPeriodDesignInput( Count ).EndDay = TypicalExtremePeriods( WhichPeriod ).EndDay;
							RunPeriodDesignInput( Count ).EndMonth = TypicalExtremePeriods( WhichPeriod ).EndMonth;
							RunPeriodDesignInput( Count ).EndDate = TypicalExtremePeriods( WhichPeriod ).EndJDay;
							RunPeriodDesignInput( Count ).TotalDays = TypicalExtremePeriods( WhichPeriod ).TotalDays;
							ShowWarningError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) + " matched to " + TypicalExtremePeriods( WhichPeriod ).MatchValue );
						} else {
							ShowSevereError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cAlphaFieldNames( 2 ) + " invalid (not on Weather File)=" + cAlphaArgs( 2 ) );
							ErrorsFound = true;
						}
					}
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cAlphaFieldNames( 2 ) + " invalid (blank)." );
				ErrorsFound = true;
			}

			if ( lAlphaFieldBlanks( 3 ) ) {
				RunPeriodDesignInput( Count ).DayOfWeek = 2; // Defaults to Monday
			} else {
				RunPeriodDesignInput( Count ).DayOfWeek = FindItemInList( cAlphaArgs( 3 ), ValidNames, 12 );
				if ( RunPeriodDesignInput( Count ).DayOfWeek == 0 || RunPeriodDesignInput( Count ).DayOfWeek == 8 ) {
					ShowWarningError( cCurrentModuleObject + ": object=" + RunPeriodDesignInput( Count ).Title + ' ' + cAlphaFieldNames( 3 ) + " invalid (Day of Week) [" + cAlphaArgs( 3 ) + " for Start is not Valid, Monday will be Used." );
					RunPeriodDesignInput( Count ).DayOfWeek = 2; // Defaults to Monday
				}
			}

			if ( lAlphaFieldBlanks( 4 ) || SameString( cAlphaArgs( 4 ), "YES" ) ) {
				RunPeriodDesignInput( Count ).UseDST = true;
			} else if ( SameString( cAlphaArgs( 4 ), "NO" ) ) {
				RunPeriodDesignInput( Count ).UseDST = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 4 ) + " invalid [" + cAlphaArgs( 4 ) + ']' );
				ErrorsFound = true;
			}

			if ( lAlphaFieldBlanks( 5 ) || SameString( cAlphaArgs( 5 ), "YES" ) ) {
				RunPeriodDesignInput( Count ).UseRain = true;
				RunPeriodDesignInput( Count ).UseSnow = true;
			} else if ( SameString( cAlphaArgs( 5 ), "NO" ) ) {
				RunPeriodDesignInput( Count ).UseRain = false;
				RunPeriodDesignInput( Count ).UseSnow = false;
			} else {
				ShowSevereError( cCurrentModuleObject + ": object #" + TrimSigDigits( Loop ) + cAlphaFieldNames( 5 ) + " invalid [" + cAlphaArgs( 5 ) + ']' );
				ErrorsFound = true;
			}
			RunPeriodDesignInput( 1 ).MonWeekDay = 0;
			if ( RunPeriodDesignInput( 1 ).DayOfWeek != 0 && ! ErrorsFound ) {
				SetupWeekDaysByMonth( RunPeriodDesignInput( 1 ).StartMonth, RunPeriodDesignInput( 1 ).StartDay, RunPeriodDesignInput( 1 ).DayOfWeek, RunPeriodDesignInput( 1 ).MonWeekDay );
			}

		}

	}

	void
	GetSpecialDayPeriodData( bool & ErrorsFound ) // will be set to true if severe errors are found in inputs
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads any special day period data from the IDF and
		// processes it into the data structure that will drive the values
		// in the SpecialDayTypes array.

		// METHODOLOGY EMPLOYED:
		// Processes the following IDD definition:
		// SpecialDayPeriod,
		//      \memo This object sets up holidays/special days to be used during weather file
		//      \memo run periods.  (These are not used with DesignDay objects.)
		//      \memo Depending on the value in the run period, days on the weather file may also
		//      \memo be used.  However, the weather file specification will take precedence over
		//      \memo any specification shown here.  (No error message on duplicate days or overlapping
		//      \memo days).
		//  A1, \field Holiday Name
		//  A2, \field StartDate
		//      \memo  Dates can be several formats:
		//      \memo  <number>/<number>  (month/day)
		//      \memo  <number> Month
		//      \memo  Month <number>
		//      \memo Months are January, February, March, April, May, June, July, August, September, October, November, December
		//      \memo Months can be the first 3 letters of the month
		//        \note will eventually allow: 3 Monday April (meaning 3rd Monday in April)
		//  N1, \field duration (number of days)
		//  A3; \field SpecialDayType
		//        \note SpecialDayType selects the schedules appropriate for each day so labeled
		//        \type choice
		//        \key Holiday
		//        \key SummerDesignDay
		//        \key WinterDesignDay
		//        \key CustomDay1
		//        \key CustomDay2

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const ValidDayTypes( 5, { "HOLIDAY", "SUMMERDESIGNDAY", "WINTERDESIGNDAY", "CUSTOMDAY1", "CUSTOMDAY2" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string AlphArray( 3 );
		int NumAlphas;
		Array1D< Real64 > Duration( 1 );
		int NumNumbers;
		int NumSpecDays;
		int Count;
		int Loop;
		int PMonth;
		int PDay;
		int PWeekDay;
		int DateType;
		int IOStat;
		int DayType;
		static bool IsNotOK( false ); // Flag to verify name
		static bool IsBlank( false ); // Flag for blank name

		cCurrentModuleObject = "RunPeriodControl:SpecialDays";
		NumSpecDays = GetNumObjectsFound( cCurrentModuleObject );
		if ( allocated( SpecialDays ) ) { // EPW already allocated the array
			Count = NumSpecialDays - NumSpecDays + 1;
		} else {
			SpecialDays.allocate( NumSpecDays );
			NumSpecialDays = NumSpecDays;
			Count = 1;
		}

		for ( Loop = 1; Loop <= NumSpecDays; ++Loop ) {

			GetObjectItem( cCurrentModuleObject, Loop, AlphArray, NumAlphas, Duration, NumNumbers, IOStat );

			VerifyName( AlphArray( 1 ), SpecialDays, Count - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}

			SpecialDays( Count ).Name = AlphArray( 1 );

			ProcessDateString( AlphArray( 2 ), PMonth, PDay, PWeekDay, DateType, ErrorsFound );
			if ( DateType == MonthDay ) {
				SpecialDays( Count ).DateType = DateType;
				SpecialDays( Count ).Month = PMonth;
				SpecialDays( Count ).Day = PDay;
				SpecialDays( Count ).WeekDay = 0;
				SpecialDays( Count ).CompDate = PMonth * 32 + PDay;
				SpecialDays( Count ).WthrFile = false;
			} else if ( DateType != InvalidDate ) {
				SpecialDays( Count ).DateType = DateType;
				SpecialDays( Count ).Month = PMonth;
				SpecialDays( Count ).Day = PDay;
				SpecialDays( Count ).WeekDay = PWeekDay;
				SpecialDays( Count ).CompDate = 0;
				SpecialDays( Count ).WthrFile = false;
			} else if ( DateType == InvalidDate ) {
				ShowSevereError( cCurrentModuleObject + ": " + AlphArray( 1 ) + " Invalid " + cAlphaFieldNames( 2 ) + '=' + AlphArray( 2 ) );
				ErrorsFound = true;
			}

			if ( Duration( 1 ) > 0 ) {
				SpecialDays( Count ).Duration = int( Duration( 1 ) );
			} else {
				ShowSevereError( cCurrentModuleObject + ": " + AlphArray( 1 ) + " Invalid " + cNumericFieldNames( 1 ) + '=' + TrimSigDigits( Duration( 1 ), 0 ) );
				ErrorsFound = true;
			}

			DayType = FindItemInList( AlphArray( 3 ), ValidDayTypes, 5 );
			if ( DayType == 0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + AlphArray( 1 ) + " Invalid " + cAlphaFieldNames( 3 ) + '=' + AlphArray( 3 ) );
				ErrorsFound = true;
			} else {
				SpecialDays( Count ).DayType = DayType;
			}
			++Count;
		}

		//CALL CalcSpecialDayTypes

	}

	void
	CalcSpecialDayTypes()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine creates the array of Special Day types used during
		// the simulation.

		// METHODOLOGY EMPLOYED:
		// Sets up the SpecialDayTypes array that then is used during simulation.
		// Uses WFLeapYearInd to indicate Leap Year simulation runs.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::JulianDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int Loop1;
		int JDay;
		int Warn;

		SpecialDayTypes = 0; // Initialize/Reset Special Day Types array

		for ( Loop = 1; Loop <= NumSpecialDays; ++Loop ) {

			if ( SpecialDays( Loop ).WthrFile ) continue;

			Warn = 0;

			JDay = JulianDay( SpecialDays( Loop ).Month, SpecialDays( Loop ).Day, LeapYearAdd ) - 1;

			for ( Loop1 = 1; Loop1 <= SpecialDays( Loop ).Duration; ++Loop1 ) {
				++JDay;
				if ( JDay > 366 ) {
					ShowWarningError( "SpecialDay=" + SpecialDays( Loop ).Name + " causes index of more than 366, ignoring those beyond 366" );
				} else {
					if ( SpecialDayTypes( JDay ) != 0 && Warn == 0 ) {
						ShowWarningError( "SpecialDay=" + SpecialDays( Loop ).Name + " attempted overwrite of previous set special day" );
						Warn = 1;
					} else if ( SpecialDayTypes( JDay ) == 0 ) {
						SpecialDayTypes( JDay ) = SpecialDays( Loop ).DayType;
					}
				}
			}
		}

	}

	void
	GetDSTData( bool & ErrorsFound ) // will be set to true if severe errors are found in inputs
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets a possible "Daylight Saving Period" from the IDF.  Using this
		// will overwrite any prior DST data.

		// METHODOLOGY EMPLOYED:
		// Processes the following IDD definition:
		// DaylightSavingPeriod,
		//      \memo This object sets up the Daylight Saving period for any RunPeriod.
		//      \memo Ignores any DaylightSavingperiod values on the weather file and uses this definition.
		//      \memo (These are not used with DesignDay objects.)
		//  A1, \field StartDate
		//  A2, \field EndDate
		//      \memo  Dates can be several formats:
		//      \memo  <number>/<number>  (month/day)
		//      \memo  <number> <Month>
		//      \memo  <Month> <number>
		//      \memo <Nth> <Weekday> in <Month)
		//      \memo Last <WeekDay> in <Month>
		//      \memo <Month> can be January, February, March, April, May, June, July, August, September,
		// October, November, December
		//      \memo Months can be the first 3 letters of the month
		//      \memo <Weekday> can be Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday
		//      \memo <Nth> can be 1 or 1st, 2 or 2nd, etc. up to 5(?)

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumFound;
		int NumAlphas;
		int IOStat;
		int NumNumbers;

		cCurrentModuleObject = "RunPeriodControl:DaylightSavingTime";
		NumFound = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumFound == 1 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( NumAlphas != 2 ) {
				ShowSevereError( cCurrentModuleObject + ": Insufficient fields, must have Start AND End Dates" );
				ErrorsFound = true;
			} else { // Correct number of arguments
				ProcessDateString( cAlphaArgs( 1 ), IDFDST.StMon, IDFDST.StDay, IDFDST.StWeekDay, IDFDST.StDateType, ErrorsFound );
				if ( IDFDST.StDateType == InvalidDate ) {
					ShowSevereError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				ProcessDateString( cAlphaArgs( 2 ), IDFDST.EnMon, IDFDST.EnDay, IDFDST.EnWeekDay, IDFDST.EnDateType, ErrorsFound );
				if ( IDFDST.EnDateType == InvalidDate ) {
					ShowSevereError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ErrorsFound = true;
				}
				IDFDaylightSaving = true;
			}
		} else if ( NumFound > 1 ) {
			ShowSevereError( cCurrentModuleObject + ": Too many objects in Input File, only one allowed." );
			ErrorsFound = true;
		}

	}

	void
	GetDesignDayData(
		int & TotDesDays, // Total number of Design days to Setup
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   September 1997
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine retrieves the design day info from user input file
		//  which is later to be used in the Setup Design Day Routine.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// SizingPeriod:DesignDay,
		//   A1, \field Name
		//   N1,  \field Month
		//   N2,  \field Day of Month
		//   A2,  \field Day Type
		//   N3,  \field Maximum Dry-Bulb Temperature
		//   N4,  \field Daily Dry-Bulb Temperature Range
		//   A3,  \field Dry-Bulb Temperature Range Modifier Type
		//   A4,  \field Dry-Bulb Temperature Range Modifier Day Schedule Name
		//   A5,  \field Humidity Condition Type
		//   N5,  \field Wetbulb or DewPoint at Maximum Dry-Bulb
		//   A6,  \field Humidity Condition Day Schedule Name
		//   N6,  \field Humidity Ratio at Maximum Dry-Bulb
		//   N7,  \field Enthalpy at Maximum Dry-Bulb  !will require units transition.
		//   N8,  \field Daily Wet-Bulb Temperature Range
		//   N9,  \field Barometric Pressure
		//   N10, \field Wind Speed
		//   N11, \field Wind Direction
		//   A7,  \field Rain Indicator
		//   A8,  \field Snow Indicator
		//   A9,  \field Daylight Saving Time Indicator
		//   A10, \field Solar Model Indicator
		//   A11, \field Beam Solar Day Schedule Name
		//   A12, \field Diffuse Solar Day Schedule Name
		//   N12, \field ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)
		//   N13, \field ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)
		//   N14; \field Sky Clearness

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::RangeCheck;
		using InputProcessor::SameString;
		using General::RoundSigDigits;
		using General::FindNumberInList;
		using ScheduleManager::GetDayScheduleIndex;
		using ScheduleManager::GetSingleDayScheduleValues;
		using ScheduleManager::CheckDayScheduleValueMinMax;
		using namespace DataSystemVariables;
		using namespace OutputReportPredefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const ValidNames( 12, { "SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "HOLIDAY", "SUMMERDESIGNDAY", "WINTERDESIGNDAY", "CUSTOMDAY1", "CUSTOMDAY2" } );
		static Array1D_string const HumidityIndicatingType( {0,DDHumIndType_Count-1}, { "Wetbulb [C]", "Dewpoint [C]", "Enthalpy [J/kg]", "Humidity Ratio []", "Schedule []", "WetBulbProfileDefaultMultipliers []", "WetBulbProfileDifferenceSchedule []", "WetBulbProfileMultiplierSchedule []" } );

		//  REAL(r64), PARAMETER, DIMENSION(24) :: DefaultTempRangeMult=(/ .87d0,.92d0,.96d0,.99d0,1.0d0,.98d0,.93d0,  &
		//                   .84d0,.71d0,.56d0,.39d0,.23d0, .11d0,.03d0,.00d0,.03d0,.10d0,.21d0,.34d0,.47d0,.58d0,.68d0,.76d0,.82d0 /)
		// Below are the 2009 fractions, HOF, Chap 14, Table 6
		static Array1D< Real64 > const DefaultTempRangeMult( 24, { 0.88, 0.92, 0.95, 0.98, 1.0, 0.98, 0.91, 0.74, 0.55, 0.38, 0.23, 0.13, 0.05, 0.00, 0.00, 0.06, 0.14, 0.24, 0.39, 0.50, 0.59, 0.68, 0.75, 0.82 } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EnvrnNum; // Environment Loop to pass to Design Day Setup Routine
		int NumAlpha; // Number of material alpha names being passed
		int NumNumerics; // Number of material properties being passed
		int IOStat; // IO Status when calling get input subroutine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int HrLoop;
		int TSLoop;
		Real64 LastHrValue;
		Real64 WNow;
		Real64 WPrev;
		Real64 testval;
		bool errFlag;
		int DDLoop;
		std::string envTitle;
		std::string units;
		int schPtr;
		bool MaxDryBulbEntered;
		bool PressureEntered;

		// FLOW:

		DesDayInput.allocate( TotDesDays ); // Allocate the array to the # of DD's
		DDDBRngModifier.allocate( NumOfTimeStepInHour, 24, TotDesDays );
		DDDBRngModifier = 0.0;
		DDHumIndModifier.allocate( NumOfTimeStepInHour, 24, TotDesDays );
		DDHumIndModifier = 0.0;
		DDBeamSolarValues.allocate( NumOfTimeStepInHour, 24, TotDesDays );
		DDBeamSolarValues = 0.0;
		DDDiffuseSolarValues.allocate( NumOfTimeStepInHour, 24, TotDesDays );
		DDDiffuseSolarValues = 0.0;
		DDSkyTempScheduleValues.allocate( NumOfTimeStepInHour, 24, TotDesDays );
		DDSkyTempScheduleValues = 0.0;

		SPSiteDryBulbRangeModScheduleValue.dimension( TotDesDays, 0.0 );
		SPSiteHumidityConditionScheduleValue.dimension( TotDesDays, 0.0 );
		SPSiteBeamSolarScheduleValue.dimension( TotDesDays, 0.0 );
		SPSiteDiffuseSolarScheduleValue.dimension( TotDesDays, 0.0 );
		SPSiteSkyTemperatureScheduleValue.dimension( TotDesDays, 0.0 );

		if ( ReverseDD && TotDesDays <= 1 ) {
			ShowSevereError( "GetDesignDayData: Reverse Design Day requested but # Design Days <=1" );
		}

		cCurrentModuleObject = "SizingPeriod:DesignDay";
		for ( DDLoop = 1; DDLoop <= TotDesDays; ++DDLoop ) {

			if ( ReverseDD ) {
				if ( DDLoop == 1 && TotDesDays > 1 ) {
					EnvrnNum = 2;
				} else if ( DDLoop == 2 ) {
					EnvrnNum = 1;
				} else {
					EnvrnNum = DDLoop;
				}
			} else {
				EnvrnNum = DDLoop;
			}

			//Call Input Get routine to retrieve design day data
			MaxDryBulbEntered = false;
			PressureEntered = false;
			GetObjectItem( cCurrentModuleObject, DDLoop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumerics, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			//   A1, \field Name
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), DesDayInput, &DesignDayData::Title, EnvrnNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			DesDayInput( EnvrnNum ).Title = cAlphaArgs( 1 ); // Environment name
			Environment( EnvrnNum ).Title = DesDayInput( EnvrnNum ).Title;

			//   N3,  \field Maximum Dry-Bulb Temperature
			//   N4,  \field Daily Dry-Bulb Temperature Range
			//   N9,  \field Barometric Pressure
			//   N10, \field Wind Speed
			//   N11, \field Wind Direction
			DesDayInput( EnvrnNum ).MaxDryBulb = rNumericArgs( 3 ); // Maximum Dry-Bulb Temperature (C)
			if ( ! lNumericFieldBlanks( 3 ) ) MaxDryBulbEntered = true;
			DesDayInput( EnvrnNum ).DailyDBRange = rNumericArgs( 4 ); // Daily dry-bulb temperature range (deltaC)
			DesDayInput( EnvrnNum ).PressBarom = rNumericArgs( 9 ); // Atmospheric/Barometric Pressure (Pascals)
			if ( ! lNumericFieldBlanks( 9 ) ) PressureEntered = true;
			DesDayInput( EnvrnNum ).PressureEntered = PressureEntered;
			DesDayInput( EnvrnNum ).WindSpeed = rNumericArgs( 10 ); // Wind Speed (m/s)
			DesDayInput( EnvrnNum ).WindDir = mod( rNumericArgs( 11 ), 360.0 ); // Wind Direction
			// (degrees clockwise from North, N=0, E=90, S=180, W=270)
			//   N1,  \field Month
			//   N2,  \field Day of Month
			//   N12, \field ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)
			//   N13, \field ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)
			//   N8,  \field Daily Wet-Bulb Temperature Range
			DesDayInput( EnvrnNum ).Month = int( rNumericArgs( 1 ) ); // Month of Year ( 1 - 12 )
			DesDayInput( EnvrnNum ).DayOfMonth = int( rNumericArgs( 2 ) ); // Day of Month ( 1 - 31 )
			DesDayInput( EnvrnNum ).TauB = rNumericArgs( 12 ); // beam tau >= 0
			DesDayInput( EnvrnNum ).TauD = rNumericArgs( 13 ); // diffuse tau >= 0
			DesDayInput( EnvrnNum ).DailyWBRange = rNumericArgs( 8 ); // Daily wet-bulb temperature range (deltaC)

			//   N14; \field Sky Clearness
			DesDayInput( EnvrnNum ).SkyClear = rNumericArgs( 14 ); // Sky Clearness (0 to 1)

			//   A7,  \field Rain Indicator
			if ( SameString( cAlphaArgs( 7 ), "Yes" ) || SameString( cAlphaArgs( 7 ), "1" ) ) {
				DesDayInput( EnvrnNum ).RainInd = 1;
			} else if ( SameString( cAlphaArgs( 7 ), "No" ) || SameString( cAlphaArgs( 7 ), "0" ) || lAlphaFieldBlanks( 7 ) ) {
				DesDayInput( EnvrnNum ).RainInd = 0;
			} else {
				ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid field: " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\"." );
				ShowContinueError( "\"No\" will be used." );
				DesDayInput( EnvrnNum ).RainInd = 0;
			}

			//   A8,  \field Snow Indicator
			if ( SameString( cAlphaArgs( 8 ), "Yes" ) || SameString( cAlphaArgs( 8 ), "1" ) ) {
				DesDayInput( EnvrnNum ).SnowInd = 1;
			} else if ( SameString( cAlphaArgs( 8 ), "No" ) || SameString( cAlphaArgs( 8 ), "0" ) || lAlphaFieldBlanks( 8 ) ) {
				DesDayInput( EnvrnNum ).SnowInd = 0;
			} else {
				ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid field: " + cAlphaFieldNames( 8 ) + "=\"" + cAlphaArgs( 8 ) + "\"." );
				ShowContinueError( "\"No\" will be used." );
				DesDayInput( EnvrnNum ).SnowInd = 0;
			}

			//   A3,  \field Dry-Bulb Temperature Range Modifier Type
			// check DB profile input
			if ( lAlphaFieldBlanks( 3 ) ) {
				cAlphaArgs( 3 ) = "DefaultMultipliers";
				DesDayInput( EnvrnNum ).DBTempRangeType = DDDBRangeType_Default;
			} else if ( SameString( cAlphaArgs( 3 ), "Multiplier" ) || SameString( cAlphaArgs( 3 ), "MultiplierSchedule" ) ) {
				cAlphaArgs( 3 ) = "MultiplierSchedule";
				DesDayInput( EnvrnNum ).DBTempRangeType = DDDBRangeType_Multiplier;
				units = "[]";
			} else if ( SameString( cAlphaArgs( 3 ), "Difference" ) || SameString( cAlphaArgs( 3 ), "Delta" ) || SameString( cAlphaArgs( 3 ), "DifferenceSchedule" ) || SameString( cAlphaArgs( 3 ), "DeltaSchedule" ) ) {
				cAlphaArgs( 3 ) = "DifferenceSchedule";
				DesDayInput( EnvrnNum ).DBTempRangeType = DDDBRangeType_Difference;
				units = "[deltaC]";
			} else if ( SameString( cAlphaArgs( 3 ), "DefaultMultipliers" ) ) {
				cAlphaArgs( 3 ) = "DefaultMultipliers";
				DesDayInput( EnvrnNum ).DBTempRangeType = DDDBRangeType_Default;
				// Validate Temperature - Daily range
			} else if ( SameString( cAlphaArgs( 3 ), "TemperatureProfileSchedule" ) ) {
				cAlphaArgs( 3 ) = "TemperatureProfileSchedule";
				DesDayInput( EnvrnNum ).DBTempRangeType = DDDBRangeType_Profile;
				units = "[C]";
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
				ShowContinueError( "..invalid field: " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ErrorsFound = true;
				cAlphaArgs( 3 ) = "invalid field";
				DesDayInput( EnvrnNum ).DBTempRangeType = DDDBRangeType_Default;
			}

			if ( DesDayInput( EnvrnNum ).DBTempRangeType != DDDBRangeType_Profile && ! MaxDryBulbEntered && cAlphaArgs( 3 ) != "invalid field" ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
				ShowContinueError( "..invalid blank field: " + cNumericFieldNames( 3 ) );
				ShowContinueError( "..this field is required when " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ErrorsFound = true;
			}

			// Assume either "multiplier" option will make full use of range...
			if ( DesDayInput( EnvrnNum ).DBTempRangeType != DDDBRangeType_Difference && DesDayInput( EnvrnNum ).DBTempRangeType != DDDBRangeType_Profile ) {
				testval = DesDayInput( EnvrnNum ).MaxDryBulb - DesDayInput( EnvrnNum ).DailyDBRange;
				errFlag = false;
				RangeCheck( errFlag, cAlphaFieldNames( 3 ), cCurrentModuleObject, "Severe", ">= -90", ( testval >= -90.0 ), "<= 70", ( testval <= 70.0 ), _, DesDayInput( EnvrnNum ).Title );
				if ( errFlag ) {
					ErrorsFound = true;
				}
			}

			//   A4,  \field Dry-Bulb Temperature Range Modifier Day Schedule Name
			if ( DesDayInput( EnvrnNum ).DBTempRangeType != DDDBRangeType_Default ) {
				if ( ! lAlphaFieldBlanks( 4 ) ) {
					DesDayInput( EnvrnNum ).TempRangeSchPtr = GetDayScheduleIndex( cAlphaArgs( 4 ) );
					if ( DesDayInput( EnvrnNum ).TempRangeSchPtr == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
						ShowContinueError( "..invalid field: " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
						ErrorsFound = true;
					} else {
						GetSingleDayScheduleValues( DesDayInput( EnvrnNum ).TempRangeSchPtr, DDDBRngModifier( _, _, EnvrnNum ) );
						schPtr = FindNumberInList( DesDayInput( EnvrnNum ).TempRangeSchPtr, SPSiteScheduleNamePtr, NumSPSiteScheduleNamePtrs );
						if ( schPtr == 0 ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).TempRangeSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Drybulb Temperature Range Modifier Schedule Value " + units, SPSiteDryBulbRangeModScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 4 ) );
						} else if ( SPSiteScheduleUnits( schPtr ) != units ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).TempRangeSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Drybulb Temperature Range Modifier Schedule Value " + units, SPSiteDryBulbRangeModScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 4 ) );
						}
						if ( cAlphaArgs( 3 ) == "MultiplierSchedule" ) {
							if ( ! CheckDayScheduleValueMinMax( DesDayInput( EnvrnNum ).TempRangeSchPtr, 0.0, ">=", 1.0, "<=" ) ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
								ShowContinueError( "..invalid field: " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
								ShowContinueError( "..Specified [Schedule] Dry-bulb Range Multiplier Values are not within [0.0, 1.0]" );
								ErrorsFound = true;
							}
						} else if ( cAlphaArgs( 3 ) == "DifferenceSchedule" ) { // delta, must be > 0.0
							if ( ! CheckDayScheduleValueMinMax( DesDayInput( EnvrnNum ).TempRangeSchPtr, 0.0, ">=" ) ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
								ShowContinueError( "..invalid field: " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
								ShowSevereError( "Some [Schedule] Dry-bulb Range Difference Values are < 0.0 [would make max larger]." );
								ErrorsFound = true;
							}
						}
						if ( cAlphaArgs( 3 ) == "TemperatureProfileSchedule" ) {
							testval = maxval( DDDBRngModifier( _, _, EnvrnNum ) );
							if ( MaxDryBulbEntered ) {
								ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", data override." );
								ShowContinueError( ".." + cNumericFieldNames( 3 ) + "=[" + RoundSigDigits( DesDayInput( EnvrnNum ).MaxDryBulb, 2 ) + "] will be overwritten." );
								ShowContinueError( ".." + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
								ShowContinueError( "..with max value=[" + RoundSigDigits( testval, 2 ) + "]." );
							}
							DesDayInput( EnvrnNum ).MaxDryBulb = testval;
						}
						testval = maxval( DDDBRngModifier( _, _, EnvrnNum ) );
						testval = DesDayInput( EnvrnNum ).MaxDryBulb - testval;
						errFlag = false;
						RangeCheck( errFlag, cAlphaFieldNames( 4 ), cCurrentModuleObject, "Severe", ">= -90", ( testval >= -90.0 ), "<= 70", ( testval <= 70.0 ), _, DesDayInput( EnvrnNum ).Title );
						if ( errFlag ) {
							ErrorsFound = true;
						}
					}
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cAlphaFieldNames( 4 ) + " is blank." );
					ShowContinueError( "..required when " + cAlphaFieldNames( 3 ) + " indicates \"SCHEDULE\"." );
					ErrorsFound = true;
				}
			} else {
				// Default dry-bulb temperature Range
				LastHrValue = DefaultTempRangeMult( 24 );
				for ( HrLoop = 1; HrLoop <= 24; ++HrLoop ) {
					for ( TSLoop = 1; TSLoop <= NumOfTimeStepInHour; ++TSLoop ) {
						WNow = Interpolation( TSLoop );
						WPrev = 1.0 - WNow;
						DDDBRngModifier( TSLoop, HrLoop, EnvrnNum ) = LastHrValue * WPrev + DefaultTempRangeMult( HrLoop ) * WNow;
					}
					LastHrValue = DefaultTempRangeMult( HrLoop );
				}
			}

			//   A5,  \field Humidity Condition Type
			if ( SameString( cAlphaArgs( 5 ), "WetBulb" ) ) {
				cAlphaArgs( 5 ) = "WetBulb";
				//   N5,  \field Wetbulb or DewPoint at Maximum Dry-Bulb
				if ( ! lNumericFieldBlanks( 5 ) ) {
					DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 5 ); // Humidity Indicating Conditions at Max Dry-Bulb
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 5 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
				errFlag = false;
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_WetBulb;
				RangeCheck( errFlag, cAlphaFieldNames( 5 ) + " - Wet-Bulb", cCurrentModuleObject, "Severe", ">= -90", ( DesDayInput( EnvrnNum ).HumIndValue >= -90.0 ), "<= 70", ( DesDayInput( EnvrnNum ).HumIndValue <= 70.0 ), _, DesDayInput( EnvrnNum ).Title );
				if ( errFlag ) {
					//        CALL ShowContinueError(TRIM(cCurrentModuleObject)//': Occured in '//TRIM(DesDayInput(EnvrnNum)%Title))
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( 5 ), "DewPoint" ) ) {
				cAlphaArgs( 5 ) = "DewPoint";
				if ( ! lNumericFieldBlanks( 5 ) ) {
					DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 5 ); // Humidity Indicating Conditions at Max Dry-Bulb
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 5 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
				errFlag = false;
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_DewPoint;
				RangeCheck( errFlag, cAlphaFieldNames( 5 ) + " - Dew-Point", cCurrentModuleObject, "Severe", ">= -90", ( DesDayInput( EnvrnNum ).HumIndValue >= -90.0 ), "<= 70", ( DesDayInput( EnvrnNum ).HumIndValue <= 70.0 ), _, DesDayInput( EnvrnNum ).Title );
				if ( errFlag ) {
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( 5 ), "HumidityRatio" ) ) {
				cAlphaArgs( 5 ) = "HumidityRatio";
				//   N6,  \field Humidity Ratio at Maximum Dry-Bulb
				if ( ! lNumericFieldBlanks( 6 ) ) {
					DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 6 ); // Humidity Indicating Conditions at Max Dry-Bulb
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 6 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
				errFlag = false;
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_HumRatio;
				RangeCheck( errFlag, cAlphaFieldNames( 5 ) + " - Humidity-Ratio", cCurrentModuleObject, "Severe", ">= 0", ( DesDayInput( EnvrnNum ).HumIndValue >= 0.0 ), "<= .03", ( DesDayInput( EnvrnNum ).HumIndValue <= 0.03 ), _, DesDayInput( EnvrnNum ).Title );
				if ( errFlag ) {
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( 5 ), "Enthalpy" ) ) {
				cAlphaArgs( 5 ) = "Enthalpy";
				//   N7,  \field Enthalpy at Maximum Dry-Bulb  !will require units transition.
				if ( ! lNumericFieldBlanks( 7 ) ) {
					DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 7 ); // Humidity Indicating Conditions at Max Dry-Bulb
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 7 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
				errFlag = false;
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_Enthalpy;
				RangeCheck( errFlag, cAlphaFieldNames( 5 ) + " - Enthalpy", "SizingPeriod:DesignDay", "Severe", ">= 0.0", ( DesDayInput( EnvrnNum ).HumIndValue >= 0.0 ), "<= 130000", ( DesDayInput( EnvrnNum ).HumIndValue <= 130000.0 ), _, DesDayInput( EnvrnNum ).Title );
				if ( errFlag ) {
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( 5 ), "RelativeHumiditySchedule" ) ) {
				cAlphaArgs( 5 ) = "RelativeHumiditySchedule";
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_RelHumSch;
				units = "[%]";
			} else if ( SameString( cAlphaArgs( 5 ), "WetBulbProfileMultiplierSchedule" ) ) {
				cAlphaArgs( 5 ) = "WetBulbProfileMultiplierSchedule";
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_WBProfMul;
				units = "[]";
				if ( ! lNumericFieldBlanks( 5 ) ) {
					DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 5 ); // Humidity Indicating Conditions at Max Dry-Bulb
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 5 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( 5 ), "WetBulbProfileDifferenceSchedule" ) ) {
				cAlphaArgs( 5 ) = "WetBulbProfileDifferenceSchedule";
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_WBProfDif;
				units = "[]";
				if ( ! lNumericFieldBlanks( 5 ) ) {
					DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 5 ); // Humidity Indicating Conditions at Max Dry-Bulb
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 5 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( 5 ), "WetBulbProfileDefaultMultipliers" ) ) {
				cAlphaArgs( 5 ) = "WetBulbProfileDefaultMultipliers";
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_WBProfDef;
				if ( ! lNumericFieldBlanks( 5 ) ) {
					DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 5 ); // Humidity Indicating Conditions at Max Dry-Bulb
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 5 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ErrorsFound = true;
				}
			} else {
				ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
				ShowContinueError( "..invalid field: " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
				ShowContinueError( "WetBulb will be used. Maximum Dry Bulb will be used as WetBulb at Maximum Dry Bulb." );
				cAlphaArgs( 5 ) = "WetBulb";
				DesDayInput( EnvrnNum ).HumIndType = DDHumIndType_WetBulb;
				DesDayInput( EnvrnNum ).HumIndValue = rNumericArgs( 3 );
			}

			// resolve humidity schedule if needed
			//   A6,  \field Humidity Condition Day Schedule Name
			if ( DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_RelHumSch || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfMul || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfDif ) {
				if ( lAlphaFieldBlanks( 6 ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cAlphaFieldNames( 6 ) + " is blank." );
					ShowContinueError( "..field is required when " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
					ErrorsFound = true;
				} else {
					DesDayInput( EnvrnNum ).HumIndSchPtr = GetDayScheduleIndex( cAlphaArgs( 6 ) );
					if ( DesDayInput( EnvrnNum ).HumIndSchPtr == 0 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
						ShowContinueError( "..invalid field: " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
						ShowContinueError( "Default Humidity will be used (constant for day using Humidity Indicator Temp)." );
						// reset HumIndType ?
					} else {

						GetSingleDayScheduleValues( DesDayInput( EnvrnNum ).HumIndSchPtr, DDHumIndModifier( _, _, EnvrnNum ) );

						schPtr = FindNumberInList( DesDayInput( EnvrnNum ).HumIndSchPtr, SPSiteScheduleNamePtr, NumSPSiteScheduleNamePtrs );
						if ( schPtr == 0 ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).HumIndSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Humidity Condition Schedule Value " + units, SPSiteHumidityConditionScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 6 ) );
						} else if ( SPSiteScheduleUnits( schPtr ) != units ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).HumIndSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Humidity Condition Schedule Value " + units, SPSiteHumidityConditionScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 6 ) );
						}

						{ auto const SELECT_CASE_var( DesDayInput( EnvrnNum ).HumIndType );

						if ( SELECT_CASE_var == DDHumIndType_RelHumSch ) {
							if ( ! CheckDayScheduleValueMinMax( DesDayInput( EnvrnNum ).HumIndSchPtr, 0.0, ">=", 100.0, "<=" ) ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
								ShowContinueError( "..invalid field: " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
								ShowContinueError( "Specified [Scheduled] Relative Humidity Values are not within [0.0, 100.0]" );
								ErrorsFound = true;
							}

						} else if ( SELECT_CASE_var == DDHumIndType_WBProfMul ) {
							// multiplier: use schedule value, check 0 <= v <= 1
							if ( ! CheckDayScheduleValueMinMax( DesDayInput( EnvrnNum ).HumIndSchPtr, 0.0, ">=", 1.0, "<=" ) ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
								ShowContinueError( "..invalid field: " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
								ShowContinueError( "..Specified [Schedule] Wet-bulb Profile Range Multiplier Values are not within [0.0, 1.0]" );
								ErrorsFound = true;
							}

						} else if ( SELECT_CASE_var == DDHumIndType_WBProfDif ) {
							if ( ! CheckDayScheduleValueMinMax( DesDayInput( EnvrnNum ).HumIndSchPtr, 0.0, ">=" ) ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
								ShowContinueError( "..invalid field: " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
								ShowSevereError( "Some [Schedule] Wet-bulb Profile Difference Values are < 0.0 [would make max larger]." );
								ErrorsFound = true;
							}
						}}
					}
				}

			} else if ( DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfDef ) {
				// re WetBulbProfileDefaultMultipliers
				LastHrValue = DefaultTempRangeMult( 24 );
				for ( HrLoop = 1; HrLoop <= 24; ++HrLoop ) {
					for ( TSLoop = 1; TSLoop <= NumOfTimeStepInHour; ++TSLoop ) {
						WNow = Interpolation( TSLoop );
						WPrev = 1.0 - WNow;
						DDHumIndModifier( TSLoop, HrLoop, EnvrnNum ) = LastHrValue * WPrev + DefaultTempRangeMult( HrLoop ) * WNow;
					}
					LastHrValue = DefaultTempRangeMult( HrLoop );
				}
				// ELSE missing case?
			}

			// verify that design WB or DP <= design DB
			if ( DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_DewPoint || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WetBulb || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfMul || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfDef || DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_WBProfDif ) {
				if ( DesDayInput( EnvrnNum ).HumIndValue > DesDayInput( EnvrnNum ).MaxDryBulb ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", range check data." );
					ShowContinueError( "..Humidity Indicator Temperature at Max Temperature=" + RoundSigDigits( DesDayInput( EnvrnNum ).HumIndValue, 1 ) + " > Max DryBulb=" + RoundSigDigits( DesDayInput( EnvrnNum ).MaxDryBulb, 1 ) );
					ShowContinueError( ".." + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ShowContinueError( "..Conditions for day will be set to Relative Humidity = 100%" );
					if ( DesDayInput( EnvrnNum ).HumIndType == DDHumIndType_DewPoint ) {
						DesDayInput( EnvrnNum ).DewPointNeedsSet = true;
					} else {
						// wet-bulb
						DesDayInput( EnvrnNum ).HumIndValue = DesDayInput( EnvrnNum ).MaxDryBulb;
					}
				}
			}

			//   A10, \field Solar Model Indicator
			if ( lAlphaFieldBlanks( 10 ) ) {
				DesDayInput( EnvrnNum ).SolarModel = ASHRAE_ClearSky;
			} else if ( SameString( cAlphaArgs( 10 ), "ASHRAEClearSky" ) || SameString( cAlphaArgs( 10 ), "CLEARSKY" ) ) {
				DesDayInput( EnvrnNum ).SolarModel = ASHRAE_ClearSky;
			} else if ( SameString( cAlphaArgs( 10 ), "ZhangHuang" ) ) {
				DesDayInput( EnvrnNum ).SolarModel = Zhang_Huang;
			} else if ( SameString( cAlphaArgs( 10 ), "ASHRAETau" ) ) {
				DesDayInput( EnvrnNum ).SolarModel = ASHRAE_Tau;
			} else if ( SameString( cAlphaArgs( 10 ), "Schedule" ) ) {
				DesDayInput( EnvrnNum ).SolarModel = SolarModel_Schedule;
			} else {
				ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
				ShowContinueError( "..invalid field: " + cAlphaFieldNames( 10 ) + "=\"" + cAlphaArgs( 10 ) + "\"." );
				ShowContinueError( "Model used will be ASHRAE ClearSky" );
				DesDayInput( EnvrnNum ).SolarModel = ASHRAE_ClearSky;
			}

			if ( DesDayInput( EnvrnNum ).SolarModel == SolarModel_Schedule ) {
				//   A11, \field Beam Solar Day Schedule Name
				if ( ! lAlphaFieldBlanks( 11 ) ) {
					DesDayInput( EnvrnNum ).BeamSolarSchPtr = GetDayScheduleIndex( cAlphaArgs( 11 ) );
					if ( DesDayInput( EnvrnNum ).BeamSolarSchPtr == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
						ShowContinueError( "..invalid field: " + cAlphaFieldNames( 11 ) + "=\"" + cAlphaArgs( 11 ) + "\"." );
						ShowContinueError( "..Required when " + cAlphaFieldNames( 10 ) + " indicates \"Schedule\"." );
						ErrorsFound = true;
					} else {
						GetSingleDayScheduleValues( DesDayInput( EnvrnNum ).BeamSolarSchPtr, DDBeamSolarValues( _, _, EnvrnNum ) );
						schPtr = FindNumberInList( DesDayInput( EnvrnNum ).BeamSolarSchPtr, SPSiteScheduleNamePtr, NumSPSiteScheduleNamePtrs );
						units = "[W/m2]";
						if ( schPtr == 0 ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).BeamSolarSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Beam Solar Schedule Value " + units, SPSiteBeamSolarScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 11 ) );
						} else if ( SPSiteScheduleUnits( schPtr ) != units ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).BeamSolarSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Beam Solar Schedule Value " + units, SPSiteBeamSolarScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 11 ) );
						}
						if ( ! CheckDayScheduleValueMinMax( DesDayInput( EnvrnNum ).BeamSolarSchPtr, 0.0, ">=" ) ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
							ShowContinueError( "..invalid field: " + cAlphaFieldNames( 11 ) + "=\"" + cAlphaArgs( 11 ) + "\"." );
							ShowContinueError( "..Specified [Schedule] Values are not >= 0.0" );
							ErrorsFound = true;
						}
					}
				} else { // should have entered beam schedule
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cAlphaFieldNames( 11 ) + " is blank." );
					ErrorsFound = true;
				}
				//   A12, \field Diffuse Solar Day Schedule Name
				if ( ! lAlphaFieldBlanks( 12 ) ) {
					DesDayInput( EnvrnNum ).DiffuseSolarSchPtr = GetDayScheduleIndex( cAlphaArgs( 12 ) );
					if ( DesDayInput( EnvrnNum ).DiffuseSolarSchPtr == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
						ShowContinueError( "..invalid field: " + cAlphaFieldNames( 12 ) + "=\"" + cAlphaArgs( 12 ) + "\"." );
						ShowContinueError( "..Required when " + cAlphaFieldNames( 10 ) + " indicates \"Schedule\"." );
						ErrorsFound = true;
					} else {
						GetSingleDayScheduleValues( DesDayInput( EnvrnNum ).DiffuseSolarSchPtr, DDDiffuseSolarValues( _, _, EnvrnNum ) );
						schPtr = FindNumberInList( DesDayInput( EnvrnNum ).DiffuseSolarSchPtr, SPSiteScheduleNamePtr, NumSPSiteScheduleNamePtrs );
						units = "[W/m2]";
						if ( schPtr == 0 ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).DiffuseSolarSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Diffuse Solar Schedule Value " + units, SPSiteDiffuseSolarScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 12 ) );
						} else if ( SPSiteScheduleUnits( schPtr ) != units ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = DesDayInput( EnvrnNum ).DiffuseSolarSchPtr;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Diffuse Solar Schedule Value " + units, SPSiteDiffuseSolarScheduleValue( EnvrnNum ), "Zone", "Average", cAlphaArgs( 12 ) );
						}
						if ( ! CheckDayScheduleValueMinMax( DesDayInput( EnvrnNum ).DiffuseSolarSchPtr, 0.0, ">=" ) ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
							ShowContinueError( "..invalid field: " + cAlphaFieldNames( 12 ) + "=\"" + cAlphaArgs( 12 ) + "\"." );
							ShowContinueError( "..Specified [Schedule] Values are not >= 0.0" );
							ErrorsFound = true;
						}
					}
				} else { // should have entered diffuse schedule
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cAlphaFieldNames( 12 ) + " is blank." );
					ErrorsFound = true;
				}
			}

			if ( DesDayInput( EnvrnNum ).SolarModel == ASHRAE_ClearSky ) {
				if ( lNumericFieldBlanks( 14 ) ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( "..invalid field: " + cNumericFieldNames( 14 ) + " is blank." );
					ShowContinueError( "..Zero clear sky (no solar) will be used." );
				}
			}

			// Validate Design Day Month

			{ auto const SELECT_CASE_var( DesDayInput( EnvrnNum ).Month );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( DesDayInput( EnvrnNum ).DayOfMonth > 31 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( ".. invalid field: " + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( DesDayInput( EnvrnNum ).DayOfMonth ) + "], Month=[" + RoundSigDigits( DesDayInput( EnvrnNum ).Month ) + "]." );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( DesDayInput( EnvrnNum ).DayOfMonth > 30 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( ".. invalid " + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( DesDayInput( EnvrnNum ).DayOfMonth ) + "], Month=[" + RoundSigDigits( DesDayInput( EnvrnNum ).Month ) + "]." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( DesDayInput( EnvrnNum ).DayOfMonth > 28 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
					ShowContinueError( ".. invalid " + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( DesDayInput( EnvrnNum ).DayOfMonth ) + "], Month=[" + RoundSigDigits( DesDayInput( EnvrnNum ).Month ) + "]." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
				ShowContinueError( ".. invalid " + cNumericFieldNames( 1 ) + " invalid (Month) [" + RoundSigDigits( DesDayInput( EnvrnNum ).Month ) + "]." );
				ErrorsFound = true;
			}}

			//   A9,  \field Daylight Saving Time Indicator
			if ( SameString( cAlphaArgs( 9 ), "Yes" ) || SameString( cAlphaArgs( 9 ), "1" ) ) {
				DesDayInput( EnvrnNum ).DSTIndicator = 1;
			} else if ( SameString( cAlphaArgs( 9 ), "No" ) || SameString( cAlphaArgs( 9 ), "0" ) || lAlphaFieldBlanks( 9 ) ) {
				DesDayInput( EnvrnNum ).DSTIndicator = 0;
			} else {
				ShowWarningError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
				ShowContinueError( "..invalid field: " + cAlphaFieldNames( 9 ) + "=\"" + cAlphaArgs( 9 ) + "\". \"No\" will be used." );
				DesDayInput( EnvrnNum ).DSTIndicator = 0;
			}

			//   A2,  \field Day Type
			DesDayInput( EnvrnNum ).DayType = FindItemInList( cAlphaArgs( 2 ), ValidNames, 12 );
			if ( DesDayInput( EnvrnNum ).DayType == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + DesDayInput( EnvrnNum ).Title + "\", invalid data." );
				ShowContinueError( "..invalid field: " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "Valid values are Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Holiday,SummerDesignDay,WinterDesignDay,CustomDay1,CustomDay2." );
				ErrorsFound = true;
			}

			Environment( EnvrnNum ).Title = DesDayInput( EnvrnNum ).Title;
			Environment( EnvrnNum ).KindOfEnvrn = ksDesignDay;
			Environment( EnvrnNum ).DesignDayNum = EnvrnNum;
			Environment( EnvrnNum ).RunPeriodDesignNum = 0;
			Environment( EnvrnNum ).TotalDays = 1;
			Environment( EnvrnNum ).StartMonth = DesDayInput( EnvrnNum ).Month;
			Environment( EnvrnNum ).StartDay = DesDayInput( EnvrnNum ).DayOfMonth;
			Environment( EnvrnNum ).EndMonth = Environment( EnvrnNum ).StartMonth;
			Environment( EnvrnNum ).EndDay = Environment( EnvrnNum ).StartDay;
			Environment( EnvrnNum ).DayOfWeek = 0;
			Environment( EnvrnNum ).UseDST = false;
			Environment( EnvrnNum ).UseHolidays = false;
			Environment( EnvrnNum ).StartJDay = DesignDay( EnvrnNum ).DayOfYear;
			Environment( EnvrnNum ).EndJDay = Environment( EnvrnNum ).StartJDay;

			//create predefined report on design day
			envTitle = DesDayInput( EnvrnNum ).Title;
			PreDefTableEntry( pdchDDmaxDB, envTitle, DesDayInput( EnvrnNum ).MaxDryBulb );
			PreDefTableEntry( pdchDDrange, envTitle, DesDayInput( EnvrnNum ).DailyDBRange );
			if ( DesDayInput( EnvrnNum ).HumIndType != DDHumIndType_RelHumSch ) {
				PreDefTableEntry( pdchDDhumid, envTitle, DesDayInput( EnvrnNum ).HumIndValue );
			} else {
				PreDefTableEntry( pdchDDhumid, envTitle, "N/A" );
			}
			PreDefTableEntry( pdchDDhumTyp, envTitle, HumidityIndicatingType( DesDayInput( EnvrnNum ).HumIndType ) );
			PreDefTableEntry( pdchDDwindSp, envTitle, DesDayInput( EnvrnNum ).WindSpeed );
			PreDefTableEntry( pdchDDwindDr, envTitle, DesDayInput( EnvrnNum ).WindDir );
		}

	}

	void
	GetLocationInfo( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   October 1997
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the location info from the IDF file; latitude,
		//  longitude and time zone number.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LocNumAlpha; // Number of alpha names being passed
		int LocNumProp; // Number of properties being passed
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string LocNames( 1 ); // Temp Array to transfer location info
		Array1D< Real64 > LocProps( 4 ); // Temporary array to transfer location info
		int NumLocations;

		// FLOW:
		cCurrentModuleObject = "Site:Location";
		NumLocations = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumLocations > 1 ) {
			ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
			ErrorsFound = true;
		}

		if ( NumLocations == 1 ) {
			//Call Input Get routine to retrieve Location information
			GetObjectItem( cCurrentModuleObject, 1, LocNames, LocNumAlpha, LocProps, LocNumProp, IOStat );

			//set latitude, longitude, and time zone number variables
			LocationTitle = LocNames( 1 );
			Latitude = LocProps( 1 );
			Longitude = LocProps( 2 );
			TimeZoneNumber = LocProps( 3 );
			Elevation = LocProps( 4 );
			LocationGathered = true;
		}

	}

	void
	GetWeatherProperties( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Weather properties are an advanced concept for simulation.  Primarily, these properties are
		// used in the test suite runs that have specific requirements for certain properties (such as
		// sky temperature).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// WeatherProperty:SkyTemperature,
		//        \memo This object is used to override internal sky temperature calculations.
		//   A1,  \field Name
		//        \reference DesignDays
		//        \note leave blank for RunPeriods (until we name them)
		//        \note This field references the applicable design day or runperiod(s) if left blank.
		//   A2,  \field Calculation Type
		//        \type choice
		//        \key ScheduleValue
		//        \key DifferenceScheduleDryBulbValue
		//        \key DifferenceScheduleDewPointValue
		//        \key AlgorithmA
		//   A3;  \field Schedule Name
		//        \type object-list
		//        \object-list DayScheduleNames
		//        \object-list ScheduleNames

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::GetDayScheduleIndex;
		using namespace DataIPShortCuts;
		using General::FindNumberInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetWeatherProperties:" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Item;
		int IOStat;
		int NumAlpha;
		int NumNumerics;
		bool IsNotOK;
		bool IsBlank;
		int Found;
		int envFound;
		int Count;
		int schPtr;
		bool MultipleEnvironments;
		std::string units;

		cCurrentModuleObject = "WeatherProperty:SkyTemperature";
		NumWPSkyTemperatures = GetNumObjectsFound( cCurrentModuleObject );

		WPSkyTemperature.allocate( NumWPSkyTemperatures ); // by default, not used.

		for ( Item = 1; Item <= NumWPSkyTemperatures; ++Item ) {
			MultipleEnvironments = false;
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlpha, rNumericArgs, NumNumerics, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			{ auto const SELECT_CASE_var( cAlphaArgs( 1 ) );
			if ( SELECT_CASE_var == "" ) {
				Found = 0;
				for ( Count = 1; Count <= NumOfEnvrn; ++Count ) {
					if ( Environment( Count ).KindOfEnvrn != ksRunPeriodWeather ) continue;
					if ( Environment( Count ).WP_Type1 != 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", indicated Environment Name already assigned." );
						if ( ! Environment( Count ).Title.empty() ) {
							ShowContinueError( "...Environment=\"" + Environment( Count ).Title + "\", already using " + cCurrentModuleObject + "=\"" + WPSkyTemperature( Environment( Count ).WP_Type1 ).Name + "\"." );
						} else {
							ShowContinueError( "... Runperiod Environment, already using " + cCurrentModuleObject + "=\"" + WPSkyTemperature( Environment( Count ).WP_Type1 ).Name + "\"." );
						}
						ErrorsFound = true;
					} else {
						Environment( Count ).WP_Type1 = Item;
						Found = Count;
					}
				}
				MultipleEnvironments = true;
				if ( Found == 0 ) {
					ShowWarningError( "GetWeatherProperties: WeatherProperty:SkyTemperature=blank, no run periods found." );
					ShowContinueError( "...SkyTemperature will not be applied." );
					continue;
				}
			} else { // really a name
				Found = FindItemInList( cAlphaArgs( 1 ), Environment, &EnvironmentData::Title );
				envFound = Found;
				if ( Found == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid Environment Name referenced." );
					ShowContinueError( "...remainder of object not processed." );
					ErrorsFound = true;
					continue;
				} else {
					if ( Environment( Found ).WP_Type1 != 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", indicated Environment Name already assigned." );
						ShowContinueError( "...Environment=\"" + Environment( Found ).Title + "\", already using " + cCurrentModuleObject + "=\"" + WPSkyTemperature( Environment( Found ).WP_Type1 ).Name + "\"." );
						ErrorsFound = true;
					} else {
						Environment( Found ).WP_Type1 = Item;
					}
				}
			}}

			if ( ! lAlphaFieldBlanks( 1 ) ) {
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), WPSkyTemperature, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				WPSkyTemperature( Item ).Name = cAlphaArgs( 1 ); // Name
			} else {
				WPSkyTemperature( Item ).Name = "All RunPeriods";
			}
			// Validate Calculation Type.
			if ( SameString( cAlphaArgs( 2 ), "ScheduleValue" ) ) {
				WPSkyTemperature( Item ).CalculationType = WP_ScheduleValue;
				WPSkyTemperature( Item ).IsSchedule = true;
				units = "[C]";
			} else if ( SameString( cAlphaArgs( 2 ), "DifferenceScheduleDryBulbValue" ) ) {
				WPSkyTemperature( Item ).CalculationType = WP_DryBulbDelta;
				WPSkyTemperature( Item ).IsSchedule = true;
				units = "[deltaC]";
			} else if ( SameString( cAlphaArgs( 2 ), "DifferenceScheduleDewPointValue" ) ) {
				WPSkyTemperature( Item ).CalculationType = WP_DewPointDelta;
				WPSkyTemperature( Item ).IsSchedule = true;
				units = "[deltaC]";
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + '.' );
				ShowContinueError( "...entered value=\"" + cAlphaArgs( 2 ) + "\", should be one of: ScheduleValue, DifferenceScheduleDryBulbValue, DifferenceScheduleDewPointValue." );
				ErrorsFound = true;
			}

			WPSkyTemperature( Item ).ScheduleName = cAlphaArgs( 3 );
			if ( Environment( Found ).KindOfEnvrn == ksRunPeriodWeather || Environment( Found ).KindOfEnvrn == ksRunPeriodDesign ) {
				WPSkyTemperature( Item ).ScheduleName = cAlphaArgs( 3 );
				// See if it's a schedule.
				Found = GetScheduleIndex( cAlphaArgs( 3 ) );
				if ( Found == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + '.' );
					ShowContinueError( "...Entered name=\"" + cAlphaArgs( 3 ) + "\"." );
					ShowContinueError( "...Should be a full year schedule (\"Schedule:Year\", \"Schedule:Compact\", \"Schedule:File\", or \"Schedule:Constant\" objects." );
					ErrorsFound = true;
				} else {
					WPSkyTemperature( Item ).IsSchedule = true;
					WPSkyTemperature( Item ).SchedulePtr = Found;
				}
			} else { // See if it's a valid schedule.
				Found = GetDayScheduleIndex( cAlphaArgs( 3 ) );
				if ( Found == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + '.' );
					ShowContinueError( "...Entered name=\"" + cAlphaArgs( 3 ) + "\"." );
					ShowContinueError( "...Should be a single day schedule (\"Schedule:Day:Hourly\", \"Schedule:Day:Interval\", or \"Schedule:Day:List\" objects." );
					ErrorsFound = true;
				} else {
					if ( envFound != 0 ) {
						schPtr = FindNumberInList( Found, SPSiteScheduleNamePtr, NumSPSiteScheduleNamePtrs );
						if ( schPtr == 0 ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = Found;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Sky Temperature Schedule Value " + units, SPSiteSkyTemperatureScheduleValue( envFound ), "Zone", "Average", cAlphaArgs( 3 ) );
						} else if ( SPSiteScheduleUnits( schPtr ) != units ) {
							++NumSPSiteScheduleNamePtrs;
							SPSiteScheduleNamePtr( NumSPSiteScheduleNamePtrs ) = Found;
							SPSiteScheduleUnits( NumSPSiteScheduleNamePtrs ) = units;
							SetupOutputVariable( "Sizing Period Site Sky Temperature Schedule Value " + units, SPSiteSkyTemperatureScheduleValue( envFound ), "Zone", "Average", cAlphaArgs( 3 ) );
						}
						WPSkyTemperature( Item ).IsSchedule = true;
						WPSkyTemperature( Item ).SchedulePtr = Found;
					}
				}
			}
		}

	}

	void
	GetGroundTemps( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   October 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This file reads the Ground Temps from the input file and puts them
		//  in a new variable.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using namespace GroundTemperatureManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Formats
		static gio::Fmt Format_720( "(' ',A,12(', ',F6.2))" );

		// FLOW:
		// Initialize Site:GroundTemperature:BuildingSurface object
		siteBuildingSurfaceGroundTempsPtr = GetGroundTempModelAndInit( "SITE:GROUNDTEMPERATURE:BUILDINGSURFACE", "" );
		if ( siteBuildingSurfaceGroundTempsPtr ){
			ErrorsFound = siteBuildingSurfaceGroundTempsPtr->errorsFound;
		}

		// Initialize Site:GroundTemperature:FCFactorMethod object
		siteFCFactorMethodGroundTempsPtr = GetGroundTempModelAndInit( "SITE:GROUNDTEMPERATURE:FCFACTORMETHOD", "" );
		if ( siteFCFactorMethodGroundTempsPtr ) {
			ErrorsFound = siteFCFactorMethodGroundTempsPtr->errorsFound;
		}

		// Initialize Site:GroundTemperature:Shallow object
		siteShallowGroundTempsPtr = GetGroundTempModelAndInit( "SITE:GROUNDTEMPERATURE:SHALLOW", "" );
		if ( siteShallowGroundTempsPtr ) {
			ErrorsFound = siteShallowGroundTempsPtr->errorsFound;
		}

		// Initialize Site:GroundTemperature:Deep object
		siteDeepGroundTempsPtr = GetGroundTempModelAndInit( "SITE:GROUNDTEMPERATURE:DEEP", "" );
		if ( siteDeepGroundTempsPtr ) {
			ErrorsFound = siteDeepGroundTempsPtr->errorsFound;
		}
	}

	void
	GetGroundReflectances( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This file reads the Ground Reflectances from the input file (optional) and
		// places them in the monthly array.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int GndNumAlpha; // Number of construction alpha names being passed
		int GndNumProp; // dummy variable for properties being passed
		int IOStat; // IO Status when calling get input subroutine
		int I; // Loop counter variable
		Array1D_string GndAlphas; // Construction Alpha names defined
		Array1D< Real64 > GndProps; // Temporary array to transfer ground reflectances

		// Formats
		static gio::Fmt Format_720( "(' Site:GroundReflectance',12(', ',F5.2))" );

		// FLOW:
		cCurrentModuleObject = "Site:GroundReflectance";
		I = GetNumObjectsFound( cCurrentModuleObject );
		if ( I != 0 ) {
			GndProps.allocate( 12 );
			GndAlphas.allocate( 1 );
			if ( I == 1 ) {
				//Get the object names for each construction from the input processor
				GetObjectItem( cCurrentModuleObject, 1, GndAlphas, GndNumAlpha, GndProps, GndNumProp, IOStat );

				if ( GndNumProp < 12 ) {
					ShowSevereError( cCurrentModuleObject + ": Less than 12 values entered." );
					ErrorsFound = true;
				}

				//Assign the ground reflectances to the variable
				GroundReflectances( {1,12} ) = GndProps( {1,12} );

			} else {
				ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
				ErrorsFound = true;
			}
			GndProps.deallocate();
			GndAlphas.deallocate();
		}

		// Write Final Ground Reflectance Information to the initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Site:GroundReflectance>, Months From Jan to Dec {dimensionless}";
		gio::write( OutputFileInits, "(' ',A,$)" ) << "Site:GroundReflectance";
		for ( I = 1; I <= 12; ++I ) gio::write( OutputFileInits, "(', ',F5.2,$)" ) << GroundReflectances( I ); gio::write( OutputFileInits );

	}

	void
	GetSnowGroundRefModifiers( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This file reads the Snow Ground Reflectance Modifiers from the input file (optional) and
		// places them in the variables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int GndNumAlpha; // Number of construction alpha names being passed
		int GndNumProp; // dummy variable for properties being passed
		int IOStat; // IO Status when calling get input subroutine
		int I; // Loop counter variable
		Array1D_string GndAlphas; // Construction Alpha names defined
		Array1D< Real64 > GndProps; // Temporary array to transfer ground reflectances

		// Formats
		static gio::Fmt Format_720( "(' Site:GroundReflectance:SnowModifier',2(', ',F7.3))" );
		static gio::Fmt Format_721( "(A,12(', ',F5.2))" );

		// FLOW:
		cCurrentModuleObject = "Site:GroundReflectance:SnowModifier";
		I = GetNumObjectsFound( cCurrentModuleObject );
		if ( I != 0 ) {
			GndProps.allocate( 2 );
			GndAlphas.allocate( 1 );
			if ( I == 1 ) {
				//Get the object names for each construction from the input processor
				GetObjectItem( cCurrentModuleObject, 1, GndAlphas, GndNumAlpha, GndProps, GndNumProp, IOStat );

				//Assign the ground reflectances to the variable
				SnowGndRefModifier = GndProps( 1 );
				SnowGndRefModifierForDayltg = GndProps( 2 );

			} else {
				ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
				ErrorsFound = true;
			}
			GndProps.deallocate();
			GndAlphas.deallocate();
		}

		// Write Final Ground Reflectance Modifier Information to the initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Site:GroundReflectance:SnowModifier>, Normal, Daylighting {dimensionless}";
		gio::write( OutputFileInits, Format_720 ) << SnowGndRefModifier << SnowGndRefModifierForDayltg;

		gio::write( OutputFileInits, fmtA ) << "! <Site:GroundReflectance:Snow>, Months From Jan to Dec {dimensionless}";
		gio::write( OutputFileInits, fmtAN ) << " Site:GroundReflectance:Snow";
		for ( I = 1; I <= 12; ++I ) gio::write( OutputFileInits, "(', ',F5.2,$)" ) << max( min( GroundReflectances( I ) * SnowGndRefModifier, 1.0 ), 0.0 ); gio::write( OutputFileInits );
		gio::write( OutputFileInits, fmtA ) << "! <Site:GroundReflectance:Snow:Daylighting>, Months From Jan to Dec {dimensionless}";
		gio::write( OutputFileInits, fmtAN ) << " Site:GroundReflectance:Snow:Daylighting";
		for ( I = 1; I <= 12; ++I ) gio::write( OutputFileInits, "(', ',F5.2,$)" ) << max( min( GroundReflectances( I ) * SnowGndRefModifierForDayltg, 1.0 ), 0.0 ); gio::write( OutputFileInits );

	}

	void
	GetWaterMainsTemperatures( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads the input data for the WATER MAINS TEMPERATURES object.

		// METHODOLOGY EMPLOYED:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumObjects;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 2 ); // Character string data
		Array1D< Real64 > NumArray( 2 ); // Numeric data

		// FLOW:
		cCurrentModuleObject = "Site:WaterMainsTemperature";
		NumObjects = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumObjects == 1 ) {
			GetObjectItem( cCurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( SameString( AlphArray( 1 ), "Schedule" ) ) {
				WaterMainsTempsMethod = ScheduleMethod;

				WaterMainsTempsSchedule = GetScheduleIndex( AlphArray( 2 ) );
				if ( WaterMainsTempsSchedule == 0 ) {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + '=' + AlphArray( 2 ) );
					ErrorsFound = true;
				}

			} else if ( SameString( AlphArray( 1 ), "Correlation" ) ) {
				WaterMainsTempsMethod = CorrelationMethod;

				if ( NumNums == 0 ) {
					ShowSevereError( cCurrentModuleObject + ": Missing Annual Average and Maximum Difference fields." );
					ErrorsFound = true;
				} else if ( NumNums == 1 ) {
					ShowSevereError( cCurrentModuleObject + ": Missing Maximum Difference field." );
					ErrorsFound = true;
				} else {
					WaterMainsTempsAnnualAvgAirTemp = NumArray( 1 );
					WaterMainsTempsMaxDiffAirTemp = NumArray( 2 );
				}

			} else {
				ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + '=' + AlphArray( 1 ) );
				ErrorsFound = true;
			}

		} else if ( NumObjects > 1 ) {
			ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
			ErrorsFound = true;
		}

	}

	void
	CalcWaterMainsTemp()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the daily water mains temperature based on input data from the WATER MAINS TEMPERATURES object.

		// METHODOLOGY EMPLOYED:
		// Water mains temperature is either taken from a schedule or calculated by a correlation.  The correlation
		// is fit to Fahrenheit units, so the air temperature values are first convert to F, then mains temperature
		// is calculated and converted back to C.

		// REFERENCES:
		// Correlation developed by Jay Burch and Craig Christensen at NREL, described in:
		// Hendron, R., Anderson, R., Christensen, C., Eastment, M., and Reeves, P.  2004.  "Development of an Energy
		// Savings Benchmark for All Residential End-Uses", Proceedings of SimBuild 2004, IBPSA-USA National Conference,
		// Boulder, CO, August 4 - 6, 2004.

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tavg; // Annual Average Outdoor Air Temperature (F)
		Real64 Tdiff; // Maximum difference in monthly average outdoor air temperatures (deltaF)
		Real64 Ratio; // Value used in correlation
		Real64 Lag; // Value used in correlation
		Real64 Offset; // Value used in correlation

		// FLOW:
		{ auto const SELECT_CASE_var( WaterMainsTempsMethod );

		if ( SELECT_CASE_var == ScheduleMethod ) {
			WaterMainsTemp = GetCurrentScheduleValue( WaterMainsTempsSchedule );

		} else if ( SELECT_CASE_var == CorrelationMethod ) {
			// Convert C to F
			Tavg = WaterMainsTempsAnnualAvgAirTemp * ( 9.0 / 5.0 ) + 32.0;
			Tdiff = WaterMainsTempsMaxDiffAirTemp * ( 9.0 / 5.0 );

			Ratio = 0.4 + 0.01 * ( Tavg - 44.0 );
			Lag = 35.0 - 1.0 * ( Tavg - 44.0 );
			Offset = 6.0;
			int latitude_sign;
			if ( Latitude >= 0 ) {
				latitude_sign = -1;
			} else {
				latitude_sign = 1;
			}

			WaterMainsTemp = Tavg + Offset + Ratio * ( Tdiff / 2.0 ) * latitude_sign * std::cos( ( 0.986 * ( DayOfYear - 15.0 - Lag ) ) * DegToRadians );

			if ( WaterMainsTemp < 32.0 ) WaterMainsTemp = 32.0;

			// Convert F to C
			WaterMainsTemp = ( WaterMainsTemp - 32.0 ) * ( 5.0 / 9.0 );

		} else {
			WaterMainsTemp = 10.0; // 50 F

		}}

	}

	void
	GetWeatherStation( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads the input data for the WEATHER STATION object.

		// METHODOLOGY EMPLOYED:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumObjects;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 1 ); // Character string data
		Array1D< Real64 > NumArray( 4 ); // Numeric data
		Real64 WeatherFileWindSensorHeight; // Height of the wind sensor at the weather station, i.e., weather file
		Real64 WeatherFileWindExp; // Exponent for the wind velocity profile at the weather station
		Real64 WeatherFileWindBLHeight; // Boundary layer height for the wind velocity profile at the weather station (m)
		Real64 WeatherFileTempSensorHeight; // Height of the air temperature sensor at the weather station (m)

		// Formats
		static gio::Fmt Format_720( "('Environment:Weather Station',6(',',A))" );

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// FLOW:
		cCurrentModuleObject = "Site:WeatherStation";
		NumObjects = GetNumObjectsFound( cCurrentModuleObject );

		// Default conditions for a weather station in an open field at a height of 10 m. (These should match the IDD defaults.)
		WeatherFileWindSensorHeight = 10.0;
		WeatherFileWindExp = 0.14;
		WeatherFileWindBLHeight = 270.0;
		WeatherFileTempSensorHeight = 1.5;

		if ( NumObjects == 1 ) {
			GetObjectItem( cCurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat );

			if ( NumNums > 0 ) WeatherFileWindSensorHeight = NumArray( 1 );
			if ( NumNums > 1 ) WeatherFileWindExp = NumArray( 2 );
			if ( NumNums > 2 ) WeatherFileWindBLHeight = NumArray( 3 );
			if ( NumNums > 3 ) WeatherFileTempSensorHeight = NumArray( 4 );

		} else if ( NumObjects > 1 ) {
			ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
			ErrorsFound = true;
		}

		WeatherFileWindModCoeff = std::pow( WeatherFileWindBLHeight / WeatherFileWindSensorHeight, WeatherFileWindExp );
		WeatherFileTempModCoeff = AtmosphericTempGradient * EarthRadius * WeatherFileTempSensorHeight / ( EarthRadius + WeatherFileTempSensorHeight );

		// Write to the initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Environment:Weather Station>,Wind Sensor Height Above Ground {m},Wind Speed Profile Exponent {},Wind Speed Profile Boundary Layer Thickness {m},Air Temperature Sensor Height Above Ground {m},Wind Speed Modifier Coefficient [Internal],Temperature Modifier Coefficient [Internal]";

		gio::write( OutputFileInits, Format_720 ) << RoundSigDigits( WeatherFileWindSensorHeight, 3 ) << RoundSigDigits( WeatherFileWindExp, 3 ) << RoundSigDigits( WeatherFileWindBLHeight, 3 ) << RoundSigDigits( WeatherFileTempSensorHeight, 3 ) << RoundSigDigits( WeatherFileWindModCoeff, 3 ) << RoundSigDigits( WeatherFileTempModCoeff, 3 );

	}

	void
	DayltgCurrentExtHorizIllum()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       Nov98 (FW); Nov 2000 (FW)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// CALCULATES EXTERIOR DAYLIGHT ILLUMINANCE AND LUMINOUS EFFICACY

		// METHODOLOGY EMPLOYED:
		// CALLED by SetCurrentWeather.
		// CALCULATES THE CURRENT-TIME-STEP
		// ILLUMINANCE ON AN UNOBSTRUCTED HORIZONTAL SURFACE FROM THE
		// THE SKY AND FROM DIRECT SUN.

		// REFERENCES:
		// Based on DOE-2.1E subroutine DEXTIL.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 SDIRH; // Exterior horizontal beam irradiance (W/m2)
		Real64 SDIFH; // Exterior horizontal sky diffuse irradiance (W/m2)
		//REAL(r64)   :: PDIRLW                  ! Luminous efficacy (lum/W) of beam solar radiation
		//REAL(r64)   :: PDIFLW                  ! Luminous efficacy (lum/W) of sky diffuse solar radiation

		//              DIRECT AND DIFFUSE HORIZONTAL SOLAR IRRADIANCE (W/M2).
		//              SOLCOS(3), below, is the cosine of the solar zenith angle.
		if ( SunIsUp ) {
			SDIRH = BeamSolarRad * SOLCOS( 3 );
			SDIFH = DifSolarRad;
			//              Fraction of sky covered by clouds
			CloudFraction = pow_2( SDIFH / ( SDIRH + SDIFH + 0.0001 ) );
			//              Luminous efficacy of sky diffuse solar and beam solar (lumens/W);
			//              Horizontal illuminance from sky and horizontal beam illuminance (lux)
			//              obtained from solar quantities on weather file and luminous efficacy.

			DayltgLuminousEfficacy( PDIFLW, PDIRLW );
			HISKF = SDIFH * PDIFLW;
			HISUNF = SDIRH * PDIRLW;
			HISUNFnorm = BeamSolarRad * PDIRLW;
		} else {
			SDIRH = 0.0;
			SDIFH = 0.0;
			CloudFraction = 0.0;
			PDIFLW = 0.0;
			PDIRLW = 0.0;
			HISKF = 0.0;
			HISUNF = 0.0;
			HISUNFnorm = 0.0;
			SkyClearness = 0.0;
			SkyBrightness = 0.0;
		}

	}

	void
	DayltgLuminousEfficacy(
		Real64 & DiffLumEff, // Luminous efficacy of sky diffuse solar radiation (lum/W)
		Real64 & DirLumEff // Luminous efficacy of beam solar radiation (lum/W)
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       August 2009, BG fixed upper bound for sky clearness bin 7
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Uses diffuse horizontal solar irradiance, direct normal solar
		// irradiance, atmospheric moisture and sun position
		// to determine the luminous efficacy in lumens/watt
		// of sky diffuse solar radiation and direct normal solar radiation.
		// Based on an empirical method described in
		// R. Perez, P. Ineichen, R. Seals, J. Michalsky and R. Stewart,
		// "Modeling daylight availability and irradiance components from direct
		// global irradiance components from direct and global irradiance,"
		// Solar Energy 44 (1990) 271-289.

		// Called by DayltgCurrentExtHorizIllum.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D< Real64 > const ADiffLumEff( 8, { 97.24, 107.22, 104.97, 102.39, 100.71, 106.42, 141.88, 152.23 } ); // Diffuse luminous efficacy coefficients
		static Array1D< Real64 > const BDiffLumEff( 8, { -0.46, 1.15, 2.96, 5.59, 5.94, 3.83, 1.90, 0.35 } );
		static Array1D< Real64 > const CDiffLumEff( 8, { 12.00, 0.59, -5.53, -13.95, -22.75, -36.15, -53.24, -45.27 } );
		static Array1D< Real64 > const DDiffLumEff( 8, { -8.91, -3.95, -8.77, -13.90, -23.74, -28.83, -14.03, -7.98 } );
		static Array1D< Real64 > const ADirLumEff( 8, { 57.20, 98.99, 109.83, 110.34, 106.36, 107.19, 105.75, 101.18 } ); // Direct luminous efficacy coefficients
		static Array1D< Real64 > const BDirLumEff( 8, { -4.55, -3.46, -4.90, -5.84, -3.97, -1.25, 0.77, 1.58 } );
		static Array1D< Real64 > const CDirLumEff( 8, { -2.98, -1.21, -1.71, -1.99, -1.75, -1.51, -1.26, -1.10 } );
		static Array1D< Real64 > const DDirLumEff( 8, { 117.12, 12.38, -8.81, -4.56, -6.16, -26.73, -34.44, -8.29 } );
		static Array1D< Real64 > const ExtraDirNormIll( 12, { 131153.0, 130613.0, 128992.0, 126816.0, 124731.0, 123240.0, 122652.0, 123120.0, 124576.0, 126658.0, 128814.0, 130471.0 } ); // Monthly exterrestrial direct normal illuminance (lum/m2)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SunZenith; // Solar zenith angle (radians)
		Real64 SunAltitude; // Solar altitude angle (radians)
		Real64 SinSunAltitude; // Sine of the solar altitude angle
		Real64 Zeta;
		int ISkyClearness; // Sky clearness bin
		Real64 AirMass; // Relative optical air mass
		Real64 AtmosMoisture; // Atmospheric moisture (cm of precipitable water)

		// FLOW:

		SunZenith = std::acos( SOLCOS( 3 ) );
		SunAltitude = PiOvr2 - SunZenith;
		SinSunAltitude = std::sin( SunAltitude );
		//              Clearness of sky. SkyClearness close to 1.0 corresponds to an overcast sky.
		//              SkyClearness > 6 is a clear sky.
		//              DifSolarRad is the diffuse horizontal irradiance.
		//              BeamSolarRad is the direct normal irradiance.
		Zeta = 1.041 * pow_3( SunZenith );
		SkyClearness = ( ( DifSolarRad + BeamSolarRad ) / ( DifSolarRad + 0.0001 ) + Zeta ) / ( 1.0 + Zeta );
		AirMass = ( 1.0 - 0.1 * Elevation / 1000.0 ) / ( SinSunAltitude + 0.15 / std::pow( SunAltitude / DegToRadians + 3.885, 1.253 ) );
		//              In the following, 93.73 is the extraterrestrial luminous efficacy
		SkyBrightness = ( DifSolarRad * 93.73 ) * AirMass / ExtraDirNormIll( Month );
		if ( SkyClearness <= 1.065 ) {
			ISkyClearness = 1;
		} else if ( SkyClearness > 1.065 && SkyClearness <= 1.23 ) {
			ISkyClearness = 2;
		} else if ( SkyClearness > 1.23 && SkyClearness <= 1.50 ) {
			ISkyClearness = 3;
		} else if ( SkyClearness > 1.50 && SkyClearness <= 1.95 ) {
			ISkyClearness = 4;
		} else if ( SkyClearness > 1.95 && SkyClearness <= 2.80 ) {
			ISkyClearness = 5;
		} else if ( SkyClearness > 2.80 && SkyClearness <= 4.50 ) {
			ISkyClearness = 6;
		} else if ( SkyClearness > 4.50 && SkyClearness <= 6.20 ) {
			ISkyClearness = 7;
		} else {
			ISkyClearness = 8;
		}
		AtmosMoisture = std::exp( 0.07 * OutDewPointTemp - 0.075 );
		//              Sky diffuse luminous efficacy
		if ( SkyBrightness <= 0.0 ) {
			DiffLumEff = 0.0;
		} else {
			DiffLumEff = ADiffLumEff( ISkyClearness ) + BDiffLumEff( ISkyClearness ) * AtmosMoisture + CDiffLumEff( ISkyClearness ) * SOLCOS( 3 ) + DDiffLumEff( ISkyClearness ) * std::log( SkyBrightness );
		}
		//              Direct normal luminous efficacy
		if ( SkyBrightness <= 0.0 ) {
			DirLumEff = 0.0;
		} else {
			DirLumEff = max( 0.0, ADirLumEff( ISkyClearness ) + BDirLumEff( ISkyClearness ) * AtmosMoisture + CDirLumEff( ISkyClearness ) * std::exp( 5.73 * SunZenith - 5.0 ) + DDirLumEff( ISkyClearness ) * SkyBrightness );
		}

	}

	Real64
	GetSTM( Real64 const Longitude ) // Longitude from user input
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function determines the "standard time meridian" from the input
		// longitude. Calculates the proper Meridian from Longitude.  This
		// value is needed for weather calculations so that the sun comes
		// up and goes down at the right times.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 GetSTM;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > longl( {-12,12} ); // Lower Longitude value for a Time Zone
		Array1D< Real64 > longh( {-12,12} ); // Upper Longitude value for a Time Zone
		int i; // Loop variable
		Real64 temp; // temporary value used to determine time zone
		Real64 tz; // resultant tz meridian

		GetSTM = 0.0;

		longl( 0 ) = -7.5;
		longh( 0 ) = 7.5;
		for ( i = 1; i <= 12; ++i ) {
			longl( i ) = longl( i - 1 ) + 15.0;
			longh( i ) = longh( i - 1 ) + 15.0;
		}
		for ( i = 1; i <= 12; ++i ) {
			longl( -i ) = longl( -i + 1 ) - 15.0;
			longh( -i ) = longh( -i + 1 ) - 15.0;
		}
		temp = Longitude;
		temp = mod( temp, 360.0 );

		if ( temp > 180.0 ) temp -= 180.0;
		for ( i = -12; i <= 12; ++i ) {
			if ( temp > longl( i ) && temp <= longh( i ) ) {
				tz = i;
				tz = mod( tz, 24.0 );
				GetSTM = tz;
				break;
			}
		}

		return GetSTM;
	}

	void
	ProcessEPWHeader(
		std::string const & HeaderString,
		std::string & Line,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes each header line in the EPW weather file.

		// METHODOLOGY EMPLOYED:
		// File is positioned to the correct line, then backspaced.  This routine
		// reads in the line and processes as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::ProcessNumber;
		using InputProcessor::FindItemInList;
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::SameString;
		using General::JulianDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string Title;
		int Count;
		std::string WMO;
		std::string::size_type Pos;
		Real64 Number;
		bool IOStatus;
		int PMonth;
		int PDay;
		int PWeekDay;
		int PYear;
		int DateType;
		int NumHdArgs;
		bool errFlag;
		std::string ErrNum;
		int CurCount;
		int CurOne;
		int NumEPWHolidays;
		int NumGrndTemps;
		int TropExtremeCount; // because these can show up as "no dry" need to count and separate.
		int actcount;
		bool errflag1;

		// Strip off Header value from Line
		Pos = index( Line, ',' );
		if ( ( Pos == std::string::npos ) && ( ! has_prefixi( HeaderString, "COMMENTS" ) ) ) {
			ShowSevereError( "Invalid Header line in in.epw -- no commas" );
			ShowContinueError( "Line=" + Line );
			ShowFatalError( "Previous conditions cause termination." );
		}
		if ( Pos != std::string::npos ) Line.erase( 0, Pos + 1 );

		{ auto const SELECT_CASE_var( MakeUPPERCase( HeaderString ) );

		if ( SELECT_CASE_var == "LOCATION" ) {

			// LOCATION, A1 [City], A2 [State/Province/Region], A3 [Country],
			// A4 [Source], N1 [WMO], N2 [Latitude],
			// N3 [Longitude], N4 [Time Zone], N5 [Elevation {m}]

			NumHdArgs = 9;
			Count = 1;
			while ( Count <= NumHdArgs ) {
				strip( Line );
				Pos = index( Line, ',' );
				if ( Pos == std::string::npos ) {
					if ( len( Line ) == 0 ) {
						while ( Pos == std::string::npos ) {
							gio::read( WeatherFileUnitNumber, fmtA ) >> Line;
							strip( Line );
							uppercase( Line );
							Pos = index( Line, ',' );
						}
					} else {
						Pos = len( Line );
					}
				}

				{ auto const SELECT_CASE_var1( Count );

				if ( SELECT_CASE_var1 == 1 ) {
					Title = stripped( Line.substr( 0, Pos ) );

				} else if ( ( SELECT_CASE_var1 == 2 ) || ( SELECT_CASE_var1 == 3 ) || ( SELECT_CASE_var1 == 4 ) ) {
					Title = strip( Title ) + ' ' + stripped( Line.substr( 0, Pos ) );

				} else if ( SELECT_CASE_var1 == 5 ) {
					WMO = stripped( Line.substr( 0, Pos ) );
					Title += " WMO#=" + WMO;

				} else if ( ( SELECT_CASE_var1 == 6 ) || ( SELECT_CASE_var1 == 7 ) || ( SELECT_CASE_var1 == 8 ) || ( SELECT_CASE_var1 == 9 ) ) {
					Number = ProcessNumber( Line.substr( 0, Pos ), errFlag );
					if ( ! errFlag ) {
						{ auto const SELECT_CASE_var2( Count );
						if ( SELECT_CASE_var2 == 6 ) {
							WeatherFileLatitude = Number;
						} else if ( SELECT_CASE_var2 == 7 ) {
							WeatherFileLongitude = Number;
						} else if ( SELECT_CASE_var2 == 8 ) {
							WeatherFileTimeZone = Number;
						} else if ( SELECT_CASE_var2 == 9 ) {
							WeatherFileElevation = Number;
						}}
					} else {
						ShowSevereError( "GetEPWHeader:LOCATION, invalid numeric=" + Line.substr( 0, Pos ) );
						ErrorsFound = true;
					}
				}}
				Line.erase( 0, Pos + 1 );
				++Count;
			}
			WeatherFileLocationTitle = stripped( Title );

		} else if ( SELECT_CASE_var == "DESIGN CONDITIONS" ) {
			// No action

		} else if ( SELECT_CASE_var == "TYPICAL/EXTREME PERIODS" ) {
			TropExtremeCount = 0;
			strip( Line );
			Pos = index( Line, ',' );
			if ( Pos == std::string::npos ) {
				if ( len( Line ) == 0 ) {
					while ( Pos == std::string::npos && len( Line ) == 0 ) {
						gio::read( WeatherFileUnitNumber, fmtA ) >> Line;
						strip( Line );
						Pos = index( Line, ',' );
					}
				} else {
					Pos = len( Line );
				}
			}
			NumEPWTypExtSets = ProcessNumber( Line.substr( 0, Pos ), IOStatus );
			Line.erase( 0, Pos + 1 );
			TypicalExtremePeriods.allocate( NumEPWTypExtSets );
			TropExtremeCount = 0;
			Count = 1;
			while ( Count <= NumEPWTypExtSets ) {
				strip( Line );
				Pos = index( Line, ',' );
				if ( Pos != std::string::npos ) {
					TypicalExtremePeriods( Count ).Title = Line.substr( 0, Pos );
					Line.erase( 0, Pos + 1 );
				} else {
					ShowWarningError( "ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)=" + Line.substr( 0, Pos ) );
					ShowContinueError( "...on processing Typical/Extreme period #" + RoundSigDigits( Count ) );
					NumEPWTypExtSets = Count - 1;
					break;
				}
				Pos = index( Line, ',' );
				if ( Pos != std::string::npos ) {
					TypicalExtremePeriods( Count ).TEType = Line.substr( 0, Pos );
					Line.erase( 0, Pos + 1 );
					if ( SameString( TypicalExtremePeriods( Count ).TEType, "EXTREME" ) ) {
						if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO DRY SEASON - WEEK NEAR ANNUAL MAX" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "NoDrySeasonMax";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO DRY SEASON - WEEK NEAR ANNUAL MIN" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "NoDrySeasonMin";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO WET SEASON - WEEK NEAR ANNUAL MAX" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "NoWetSeasonMax";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO WET SEASON - WEEK NEAR ANNUAL MIN" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "NoWetSeasonMin";
							// to account for problems earlier in weather files:
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO DRY" ) ) {
							if ( TropExtremeCount == 0 ) {
								TypicalExtremePeriods( Count ).Title = "No Dry Season - Week Near Annual Max";
								TypicalExtremePeriods( Count ).ShortTitle = "NoDrySeasonMax";
								++TropExtremeCount;
							} else if ( TropExtremeCount == 1 ) {
								TypicalExtremePeriods( Count ).Title = "No Dry Season - Week Near Annual Min";
								TypicalExtremePeriods( Count ).ShortTitle = "NoDrySeasonMin";
								++TropExtremeCount;
							}
						} else { // make new short titles
							if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "SUMMER" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "Summer";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "WINTER" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "Winter";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "TROPICAL HOT" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "TropicalHot";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "TROPICAL COLD" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "TropicalCold";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "AUTUMN" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "Autumn";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO DRY" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "NoDrySeason";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO WET" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "NoWetSeason";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "WET " ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "WetSeason";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "DRY " ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "DrySeason";
							} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "SPRING" ) ) {
								TypicalExtremePeriods( Count ).ShortTitle = "Spring";
							}
						}
					} else { // not extreme
						if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "SUMMER" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "Summer";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "WINTER" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "Winter";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "TROPICAL HOT" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "TropicalHot";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "TROPICAL COLD" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "TropicalCold";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "AUTUMN" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "Autumn";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO DRY" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "NoDrySeason";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "NO WET" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "NoWetSeason";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "WET " ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "WetSeason";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "DRY " ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "DrySeason";
						} else if ( has_prefixi( TypicalExtremePeriods( Count ).Title, "SPRING" ) ) {
							TypicalExtremePeriods( Count ).ShortTitle = "Spring";
						}
					}
				} else {
					ShowWarningError( "ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)=" + TypicalExtremePeriods( Count ).Title + BlankString + Line.substr( 0, Pos ) );
					ShowContinueError( "...on processing Typical/Extreme period #" + RoundSigDigits( Count ) );
					NumEPWTypExtSets = Count - 1;
					break;
				}
				Pos = index( Line, ',' );
				if ( Pos != std::string::npos ) {
					ProcessDateString( Line.substr( 0, Pos ), PMonth, PDay, PWeekDay, DateType, ErrorsFound );
					if ( DateType != InvalidDate ) {
						if ( PMonth != 0 && PDay != 0 ) {
							TypicalExtremePeriods( Count ).StartMonth = PMonth;
							TypicalExtremePeriods( Count ).StartDay = PDay;
						}
					} else {
						ShowSevereError( "ProcessEPWHeader: Invalid Typical/Extreme Periods Start Date Field(WeatherFile)=" + Line.substr( 0, Pos ) );
						ShowContinueError( "...on processing Typical/Extreme period #" + RoundSigDigits( Count ) );
						ErrorsFound = true;
					}
					Line.erase( 0, Pos + 1 );
				}
				Pos = index( Line, ',' );
				if ( Pos != std::string::npos ) {
					ProcessDateString( Line.substr( 0, Pos ), PMonth, PDay, PWeekDay, DateType, ErrorsFound );
					if ( DateType != InvalidDate ) {
						if ( PMonth != 0 && PDay != 0 ) {
							TypicalExtremePeriods( Count ).EndMonth = PMonth;
							TypicalExtremePeriods( Count ).EndDay = PDay;
						}
					} else {
						ShowSevereError( "ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)=" + Line.substr( 0, Pos ) );
						ShowContinueError( "...on processing Typical/Extreme period #" + RoundSigDigits( Count ) );
						ErrorsFound = true;
					}
					Line.erase( 0, Pos + 1 );
				} else { // Pos=0, probably last one
					ProcessDateString( Line, PMonth, PDay, PWeekDay, DateType, ErrorsFound );
					if ( DateType != InvalidDate ) {
						if ( PMonth != 0 && PDay != 0 ) {
							TypicalExtremePeriods( Count ).EndMonth = PMonth;
							TypicalExtremePeriods( Count ).EndDay = PDay;
						}
					} else {
						ShowSevereError( "ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)=" + Line.substr( 0, Pos ) );
						ErrorsFound = true;
					}
				}
				++Count;
			}
			// Process periods to set up other values.
			for ( Count = 1; Count <= NumEPWTypExtSets; ++Count ) {
				// JulianDay (Month,Day,LeapYearValue)
				{ auto const SELECT_CASE_var1( MakeUPPERCase( TypicalExtremePeriods( Count ).ShortTitle ) );
				if ( SELECT_CASE_var1 == "SUMMER" ) {
					if ( SameString( TypicalExtremePeriods( Count ).TEType, "EXTREME" ) ) {
						TypicalExtremePeriods( Count ).MatchValue = "SummerExtreme";
						TypicalExtremePeriods( Count ).MatchValue1 = "TropicalHot";
						TypicalExtremePeriods( Count ).MatchValue2 = "NoDrySeasonMax";
					} else {
						TypicalExtremePeriods( Count ).MatchValue = "SummerTypical";
					}

				} else if ( SELECT_CASE_var1 == "WINTER" ) {
					if ( SameString( TypicalExtremePeriods( Count ).TEType, "EXTREME" ) ) {
						TypicalExtremePeriods( Count ).MatchValue = "WinterExtreme";
						TypicalExtremePeriods( Count ).MatchValue1 = "TropicalCold";
						TypicalExtremePeriods( Count ).MatchValue2 = "NoDrySeasonMin";
					} else {
						TypicalExtremePeriods( Count ).MatchValue = "WinterTypical";
					}

				} else if ( SELECT_CASE_var1 == "AUTUMN" ) {
					TypicalExtremePeriods( Count ).MatchValue = "AutumnTypical";

				} else if ( SELECT_CASE_var1 == "SPRING" ) {
					TypicalExtremePeriods( Count ).MatchValue = "SpringTypical";

				} else if ( SELECT_CASE_var1 == "WETSEASON" ) {
					TypicalExtremePeriods( Count ).MatchValue = "WetSeason";

				} else if ( SELECT_CASE_var1 == "DRYSEASON" ) {
					TypicalExtremePeriods( Count ).MatchValue = "DrySeason";

				} else if ( SELECT_CASE_var1 == "NOWETSEASON" ) {
					TypicalExtremePeriods( Count ).MatchValue = "NoWetSeason";

				} else if ( SELECT_CASE_var1 == "NODRYSEASON" ) {
					TypicalExtremePeriods( Count ).MatchValue = "NoDrySeason";

				} else if ( ( SELECT_CASE_var1 == "NODRYSEASONMAX" ) || ( SELECT_CASE_var1 == "NOWETSEASONMAX" ) ) {
					TypicalExtremePeriods( Count ).MatchValue = TypicalExtremePeriods( Count ).ShortTitle;
					TypicalExtremePeriods( Count ).MatchValue1 = "TropicalHot";
					TypicalExtremePeriods( Count ).MatchValue2 = "SummerExtreme";

				} else if ( ( SELECT_CASE_var1 == "NODRYSEASONMIN" ) || ( SELECT_CASE_var1 == "NOWETSEASONMIN" ) ) {
					TypicalExtremePeriods( Count ).MatchValue = TypicalExtremePeriods( Count ).ShortTitle;
					TypicalExtremePeriods( Count ).MatchValue1 = "TropicalCold";
					TypicalExtremePeriods( Count ).MatchValue2 = "WinterExtreme";

				} else if ( SELECT_CASE_var1 == "TROPICALHOT" ) {
					TypicalExtremePeriods( Count ).MatchValue = "TropicalHot";
					TypicalExtremePeriods( Count ).MatchValue1 = "SummerExtreme";
					TypicalExtremePeriods( Count ).MatchValue2 = "NoDrySeasonMax";

				} else if ( SELECT_CASE_var1 == "TROPICALCOLD" ) {
					TypicalExtremePeriods( Count ).MatchValue = "TropicalCold";
					TypicalExtremePeriods( Count ).MatchValue1 = "WinterExtreme";
					TypicalExtremePeriods( Count ).MatchValue2 = "NoDrySeasonMin";

				} else {
					TypicalExtremePeriods( Count ).MatchValue = "Invalid - no match";

				}}
				TypicalExtremePeriods( Count ).StartJDay = JulianDay( TypicalExtremePeriods( Count ).StartMonth, TypicalExtremePeriods( Count ).StartDay, 0 );
				TypicalExtremePeriods( Count ).EndJDay = JulianDay( TypicalExtremePeriods( Count ).EndMonth, TypicalExtremePeriods( Count ).EndDay, 0 );
				if ( TypicalExtremePeriods( Count ).StartJDay <= TypicalExtremePeriods( Count ).EndJDay ) {
					TypicalExtremePeriods( Count ).TotalDays = TypicalExtremePeriods( Count ).EndJDay - TypicalExtremePeriods( Count ).StartJDay + 1;
				} else {
					TypicalExtremePeriods( Count ).TotalDays = JulianDay( 12, 31, LeapYearAdd ) - TypicalExtremePeriods( Count ).StartJDay + 1 + TypicalExtremePeriods( Count ).EndJDay;
				}
			}

		} else if ( SELECT_CASE_var == "GROUND TEMPERATURES" ) {
			// Added for ground surfaces defined with F or c factor method. TH 7/2009
			// Assume the 0.5 m set of ground temperatures
			// or first set on a weather file, if any.
			Pos = index( Line, ',' );
			if ( Pos != std::string::npos ) {
				NumGrndTemps = ProcessNumber( Line.substr( 0, Pos ), errFlag );
				if ( ! errFlag && NumGrndTemps >= 1 ) {
					Line.erase( 0, Pos + 1 );
					// skip depth, soil conductivity, soil density, soil specific heat
					for ( Count = 1; Count <= 4; ++Count ) {
						Pos = index( Line, ',' );
						if ( Pos == std::string::npos ) {
							Line = BlankString;
							break;
						}
						Line.erase( 0, Pos + 1 );
					}
					GroundTempsFCFromEPWHeader = 0.0;
					actcount = 0;
					for ( Count = 1; Count <= 12; ++Count ) { // take the first set of ground temperatures.
						Pos = index( Line, ',' );
						if ( Pos != std::string::npos ) {
							Number = ProcessNumber( Line.substr( 0, Pos ), errFlag );
							GroundTempsFCFromEPWHeader( Count ) = Number;
							++actcount;
						} else {
							if ( len( Line ) > 0 ) {
								Number = ProcessNumber( Line.substr( 0, Pos ), errFlag );
								GroundTempsFCFromEPWHeader( Count ) = Number;
								++actcount;
							}
							break;
						}
						Line.erase( 0, Pos + 1 );
					}
					if ( actcount == 12 ) wthFCGroundTemps = true;
				}
			}

		} else if ( SELECT_CASE_var == "HOLIDAYS/DAYLIGHT SAVING" ) {
			//A1, \field LeapYear Observed
			// \type choice
			// \key Yes
			// \key No
			// \note Yes if Leap Year will be observed for this file
			// \note No if Leap Year days (29 Feb) should be ignored in this file
			//A2, \field Daylight Saving Start Day
			//A3, \field Daylight Saving End Day
			//N1, \field Number of Holidays
			//A4, \field Holiday 1 Name
			//A5, \field Holiday 1 Day
			// etc.
			// Start with Minimum number of NumHdArgs
			uppercase( Line );
			NumHdArgs = 4;
			Count = 1;
			while ( Count <= NumHdArgs ) {
				strip( Line );
				Pos = index( Line, ',' );
				if ( Pos == std::string::npos ) {
					if ( len( Line ) == 0 ) {
						while ( Pos == std::string::npos ) {
							gio::read( WeatherFileUnitNumber, fmtA ) >> Line;
							strip( Line );
							uppercase( Line );
							Pos = index( Line, ',' );
						}
					} else {
						Pos = len( Line );
					}
				}

				{ auto const SELECT_CASE_var1( Count );

				if ( SELECT_CASE_var1 == 1 ) {
					if ( Line[ 0 ] == 'Y' ) {
						//              LeapYear=.TRUE.
						WFAllowsLeapYears = true;
						WFLeapYearInd = 0; //1
					} else {
						//              LeapYear=.FALSE.
						WFAllowsLeapYears = false;
						WFLeapYearInd = 0;
					}

				} else if ( SELECT_CASE_var1 == 2 ) {
					errflag1 = ErrorsFound;
					ErrorsFound = false;
					ProcessDateString( Line.substr( 0, Pos ), PMonth, PDay, PWeekDay, DateType, ErrorsFound );
					if ( DateType != InvalidDate ) {
						if ( PMonth == 0 && PDay == 0 ) {
							EPWDaylightSaving = false;
						} else {
							EPWDaylightSaving = true;
							EPWDST.StDateType = DateType;
							EPWDST.StMon = PMonth;
							EPWDST.StDay = PDay;
							EPWDST.StWeekDay = PWeekDay;
						}
					} else {
						ErrorsFound = errflag1;
						ShowContinueError( "ProcessEPWHeader: Invalid Daylight Saving Period Start Date Field(WeatherFile)=" + Line.substr( 0, Pos ) );
						ShowContinueError( "...invalid header=" + HeaderString );
						ShowContinueError( "...Setting Weather File DST to false." );
						EPWDaylightSaving = false;
					}

				} else if ( SELECT_CASE_var1 == 3 ) {
					ProcessDateString( Line.substr( 0, Pos ), PMonth, PDay, PWeekDay, DateType, ErrorsFound );
					if ( EPWDaylightSaving ) {
						if ( DateType != InvalidDate ) {
							EPWDST.EnDateType = DateType;
							EPWDST.EnMon = PMonth;
							EPWDST.EnDay = PDay;
							EPWDST.EnWeekDay = PWeekDay;
						} else {
							ShowWarningError( "ProcessEPWHeader: Invalid Daylight Saving Period End Date Field(WeatherFile)=" + Line.substr( 0, Pos ) );
							ShowContinueError( "...Setting Weather File DST to false." );
							EPWDaylightSaving = false;
						}
						DST = EPWDST;
					}

				} else if ( SELECT_CASE_var1 == 4 ) {
					NumEPWHolidays = ProcessNumber( Line.substr( 0, Pos ), IOStatus );
					NumSpecialDays = NumEPWHolidays + GetNumObjectsFound( "RunPeriodControl:SpecialDays" );
					SpecialDays.allocate( NumSpecialDays );
					NumHdArgs = 4 + NumEPWHolidays * 2;
					CurCount = 0;

				} else if ( ( SELECT_CASE_var1 >= 5 ) ) {
					if ( mod( Count, 2 ) != 0 ) {
						++CurCount;
						if ( CurCount > NumSpecialDays ) {
							ShowSevereError( "Too many SpecialDays" );
							ErrorsFound = true;
						} else {
							SpecialDays( CurCount ).Name = Line.substr( 0, Pos );
						}
						// Process name
					} else {
						if ( CurCount <= NumSpecialDays ) {
							// Process date
							ProcessDateString( Line.substr( 0, Pos ), PMonth, PDay, PWeekDay, DateType, ErrorsFound );
							if ( DateType == MonthDay ) {
								SpecialDays( CurCount ).DateType = DateType;
								SpecialDays( CurCount ).Month = PMonth;
								SpecialDays( CurCount ).Day = PDay;
								SpecialDays( CurCount ).WeekDay = 0;
								SpecialDays( CurCount ).CompDate = PMonth * 32 + PDay;
								SpecialDays( CurCount ).Duration = 1;
								SpecialDays( CurCount ).DayType = 1;
								SpecialDays( CurCount ).WthrFile = true;
							} else if ( DateType != InvalidDate ) {
								SpecialDays( CurCount ).DateType = DateType;
								SpecialDays( CurCount ).Month = PMonth;
								SpecialDays( CurCount ).Day = PDay;
								SpecialDays( CurCount ).WeekDay = PWeekDay;
								SpecialDays( CurCount ).CompDate = 0;
								SpecialDays( CurCount ).Duration = 1;
								SpecialDays( CurCount ).DayType = 1;
								SpecialDays( CurCount ).WthrFile = true;
							} else if ( DateType == InvalidDate ) {
								ShowSevereError( "Invalid SpecialDay Date Field(WeatherFile)=" + Line.substr( 0, Pos ) );
								ErrorsFound = true;
							}
						}
					}
				}}
				Line.erase( 0, Pos + 1 );
				++Count;
			}
			for ( Count = 1; Count <= NumEPWTypExtSets; ++Count ) {
				// JulianDay (Month,Day,LeapYearValue)
				TypicalExtremePeriods( Count ).StartJDay = JulianDay( TypicalExtremePeriods( Count ).StartMonth, TypicalExtremePeriods( Count ).StartDay, LeapYearAdd );
				TypicalExtremePeriods( Count ).EndJDay = JulianDay( TypicalExtremePeriods( Count ).EndMonth, TypicalExtremePeriods( Count ).EndDay, LeapYearAdd );
				if ( TypicalExtremePeriods( Count ).StartJDay <= TypicalExtremePeriods( Count ).EndJDay ) {
					TypicalExtremePeriods( Count ).TotalDays = TypicalExtremePeriods( Count ).EndJDay - TypicalExtremePeriods( Count ).StartJDay + 1;
				} else {
					TypicalExtremePeriods( Count ).TotalDays = JulianDay( 12, 31, LeapYearAdd ) - TypicalExtremePeriods( Count ).StartJDay + 1 + TypicalExtremePeriods( Count ).EndJDay;
				}
			}

		} else if ( ( SELECT_CASE_var == "COMMENTS 1" ) || ( SELECT_CASE_var == "COMMENTS 2" ) ) {

		} else if ( SELECT_CASE_var == "DATA PERIODS" ) {
			//     N1, \field Number of Data Periods
			//     N2, \field Number of Records per hour
			//     A1, \field Data Period 1 Name/Description
			//     A2, \field Data Period 1 Start Day of Week
			//       \type choice
			//       \key  Sunday
			//       \key  Monday
			//       \key  Tuesday
			//       \key  Wednesday
			//       \key  Thursday
			//       \key  Friday
			//       \key  Saturday
			//     A3, \field Data Period 1 Start Day
			//     A4, \field Data Period 1 End Day
			uppercase( Line );
			NumHdArgs = 2;
			Count = 1;
			while ( Count <= NumHdArgs ) {
				strip( Line );
				Pos = index( Line, ',' );
				if ( Pos == std::string::npos ) {
					if ( len( Line ) == 0 ) {
						while ( Pos == std::string::npos ) {
							gio::read( WeatherFileUnitNumber, fmtA ) >> Line;
							strip( Line );
							uppercase( Line );
							Pos = index( Line, ',' );
						}
					} else {
						Pos = len( Line );
					}
				}

				{ auto const SELECT_CASE_var1( Count );

				if ( SELECT_CASE_var1 == 1 ) {
					NumDataPeriods = ProcessNumber( Line.substr( 0, Pos ), IOStatus );
					DataPeriods.allocate( NumDataPeriods );
					NumHdArgs += 4 * NumDataPeriods;
					if ( NumDataPeriods > 0 ) {
						for ( auto & e : DataPeriods ) e.NumDays = 0;
					}
					CurCount = 0;

				} else if ( SELECT_CASE_var1 == 2 ) {
					NumIntervalsPerHour = ProcessNumber( Line.substr( 0, Pos ), IOStatus );
					//          IF (NumIntervalsPerHour /= 1) THEN
					//            CALL ShowSevereError('Process EPW: Not ready for more than one interval per hour')
					//            ErrorsFound=.TRUE.
					//          ENDIF

				} else if ( ( SELECT_CASE_var1 >= 3 ) ) {
					CurOne = mod( Count - 3, 4 );

					{ auto const SELECT_CASE_var2( CurOne );

					if ( SELECT_CASE_var2 == 0 ) {
						// Description of Data Period
						++CurCount;
						if ( CurCount > NumDataPeriods ) {
							ShowSevereError( "Too many data periods" );
							ErrorsFound = true;
						} else {
							DataPeriods( CurCount ).Name = Line.substr( 0, Pos );
						}

					} else if ( SELECT_CASE_var2 == 1 ) {
						// Start Day of Week
						if ( CurCount <= NumDataPeriods ) {
							DataPeriods( CurCount ).DayOfWeek = Line.substr( 0, Pos );
							DataPeriods( CurCount ).WeekDay = FindItemInList( DataPeriods( CurCount ).DayOfWeek, DaysOfWeek, 7 );
							if ( DataPeriods( CurCount ).WeekDay == 0 ) {
								gio::write( ErrNum, fmtLD ) << CurCount;
								strip( ErrNum );
								ShowSevereError( "Weather File -- Invalid Start Day of Week for Data Period #" + ErrNum + ", Invalid day=" + DataPeriods( CurCount ).DayOfWeek );
								ErrorsFound = true;
							}
						}

					} else if ( SELECT_CASE_var2 == 2 ) {
						// DataPeriod Start Day
						if ( CurCount <= NumDataPeriods ) {
							ProcessDateString( Line.substr( 0, Pos ), PMonth, PDay, PWeekDay, DateType, ErrorsFound, PYear );
							if ( DateType == MonthDay ) {
								DataPeriods( CurCount ).StMon = PMonth;
								DataPeriods( CurCount ).StDay = PDay;
								DataPeriods( CurCount ).StYear = PYear;
								if ( PYear != 0 ) DataPeriods( CurCount ).HasYearData = true;
							} else {
								ShowSevereError( "Data Periods must be of the form <DayOfYear> or <Month Day> (WeatherFile), found=" + Line.substr( 0, Pos ) );
								ErrorsFound = true;
							}
						}

					} else if ( SELECT_CASE_var2 == 3 ) {
						if ( CurCount <= NumDataPeriods ) {
							ProcessDateString( Line.substr( 0, Pos ), PMonth, PDay, PWeekDay, DateType, ErrorsFound, PYear );
							if ( DateType == MonthDay ) {
								DataPeriods( CurCount ).EnMon = PMonth;
								DataPeriods( CurCount ).EnDay = PDay;
								DataPeriods( CurCount ).EnYear = PYear;
								if ( PYear == 0 && DataPeriods( CurCount ).HasYearData ) {
									ShowWarningError( "Data Period (WeatherFile) - Start Date contains year. End Date does not." );
									ShowContinueError( "...Assuming same year as Start Date for this data." );
									DataPeriods( CurCount ).EnYear = DataPeriods( CurCount ).StYear;
								}
							} else {
								ShowSevereError( "Data Periods must be of the form <DayOfYear> or <Month Day>, (WeatherFile) found=" + Line.substr( 0, Pos ) );
								ErrorsFound = true;
							}
						}
						if ( DataPeriods( CurCount ).StYear == 0 || DataPeriods( CurCount ).EnYear == 0 ) {
							DataPeriods( CurCount ).DataStJDay = JulianDay( DataPeriods( CurCount ).StMon, DataPeriods( CurCount ).StDay, LeapYearAdd );
							DataPeriods( CurCount ).DataEnJDay = JulianDay( DataPeriods( CurCount ).EnMon, DataPeriods( CurCount ).EnDay, LeapYearAdd );
							if ( DataPeriods( CurCount ).DataStJDay <= DataPeriods( CurCount ).DataEnJDay ) {
								DataPeriods( CurCount ).NumDays = DataPeriods( CurCount ).DataEnJDay - DataPeriods( CurCount ).DataStJDay + 1;
							} else {
								DataPeriods( CurCount ).NumDays = ( 365 - DataPeriods( CurCount ).DataStJDay + 1 ) + ( DataPeriods( CurCount ).DataEnJDay - 1 + 1 );
							}
						} else { // weather file has actual year(s)
							JGDate( GregorianToJulian, DataPeriods( CurCount ).DataStJDay, DataPeriods( CurCount ).StYear, DataPeriods( CurCount ).StMon, DataPeriods( CurCount ).StDay );
							JGDate( GregorianToJulian, DataPeriods( CurCount ).DataEnJDay, DataPeriods( CurCount ).EnYear, DataPeriods( CurCount ).EnMon, DataPeriods( CurCount ).EnDay );
							DataPeriods( CurCount ).NumDays = DataPeriods( CurCount ).DataEnJDay - DataPeriods( CurCount ).DataStJDay + 1;
						}
						// Have processed the last item for this, can set up Weekdays for months
						DataPeriods( CurCount ).MonWeekDay = 0;
						if ( ! ErrorsFound ) {
							SetupWeekDaysByMonth( DataPeriods( CurCount ).StMon, DataPeriods( CurCount ).StDay, DataPeriods( CurCount ).WeekDay, DataPeriods( CurCount ).MonWeekDay );
						}

					}}
				}}
				Line.erase( 0, Pos + 1 );
				++Count;
			}

		} else {
			ShowFatalError( "Invalid EPW Header designation found=" + HeaderString );

		}}

	}

	void
	SkipEPlusWFHeader()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine skips the initial header records on the EnergyPlus Weather File (in.epw).

		// METHODOLOGY EMPLOYED:
		// List directed reads, as possible.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::ProcessNumber;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const Header( "DATA PERIODS" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string::size_type Pos;
		std::string Line;
		bool StillLooking;
		int NumHdArgs;
		int Count;
		int CurCount;
		int CurOne;
		int NumPeriods;
		bool IOStatus;

		// Read in Header Information

		// Headers should come in order
		StillLooking = true;
		while ( StillLooking ) {
			{ IOFlags flags; gio::read( WeatherFileUnitNumber, fmtA, flags ) >> Line; if ( flags.end() ) goto Label9998; }
			uppercase( Line );
			if ( has( Line, Header ) ) break;
		}

		// Dummy process Data Periods line
		//  'DATA PERIODS'
		//     N1, \field Number of Data Periods
		//     N2, \field Number of Records per hour
		//     A1, \field Data Period 1 Name/Description
		//     A2, \field Data Period 1 Start Day of Week
		//       \type choice
		//       \key  Sunday
		//       \key  Monday
		//       \key  Tuesday
		//       \key  Wednesday
		//       \key  Thursday
		//       \key  Friday
		//       \key  Saturday
		//     A3, \field Data Period 1 Start Day
		//     A4, \field Data Period 1 End Day
		NumHdArgs = 2;
		Count = 1;
		while ( Count <= NumHdArgs ) {
			strip( Line );
			Pos = index( Line, ',' );
			if ( Pos == std::string::npos ) {
				if ( len( Line ) == 0 ) {
					while ( Pos == std::string::npos ) {
						gio::read( WeatherFileUnitNumber, fmtA ) >> Line;
						strip( Line );
						uppercase( Line );
						Pos = index( Line, ',' );
					}
				} else {
					Pos = len( Line );
				}
			}

			{ auto const SELECT_CASE_var( Count );

			if ( SELECT_CASE_var == 1 ) {
				NumPeriods = ProcessNumber( Line.substr( 0, Pos ), IOStatus );
				NumHdArgs += 4 * NumPeriods;
				CurCount = 0;

			} else if ( SELECT_CASE_var == 2 ) {

			} else if ( ( SELECT_CASE_var >= 3 ) ) {
				CurOne = mod( Count - 3, 4 );

				{ auto const SELECT_CASE_var1( CurOne );

				if ( SELECT_CASE_var1 == 0 ) {
					// Description of Data Period
					++CurCount;

				} else if ( ( SELECT_CASE_var1 >= 1 ) && ( SELECT_CASE_var1 <= 3 ) ) {

				}}
			}}
			Line.erase( 0, Pos + 1 );
			++Count;
		}

		return;

Label9998: ;
		ShowFatalError( "Unexpected End-of-File on EPW Weather file, while reading header information, looking for header=" + Header, OutputFileStandard );

	}

	void
	ReportMissing_RangeData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reports the counts of missing/out of range data
		// for weather file environments.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const MissString( "Missing Data Found on Weather Data File" );
		static gio::Fmt msFmt( "('Missing ',A,', Number of items=',I5)" );
		static std::string const InvString( "Invalid Data Found on Weather Data File" );
		static gio::Fmt ivFmt( "('Invalid ',A,', Number of items=',I5)" );
		static std::string const RangeString( "Out of Range Data Found on Weather Data File" );
		static gio::Fmt rgFmt( "('Out of Range ',A,' [',A,',',A,'], Number of items=',I5)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool MissedHeader;
		bool OutOfRangeHeader;
		std::string ErrString;

		if ( ! DisplayWeatherMissingDataWarnings ) return;

		MissedHeader = false;
		if ( Missed.DryBulb > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Dry Bulb Temperatures\"" << Missed.DryBulb;
			ShowMessage( ErrString );
		}
		if ( Missed.StnPres > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Atmospheric Pressure\"" << Missed.StnPres;
			ShowMessage( ErrString );
		}
		if ( Missed.RelHumid > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Relative Humidity\"" << Missed.RelHumid;
			ShowMessage( ErrString );
		}
		if ( Missed.DewPoint > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Dew Point Temperatures\"" << Missed.DewPoint;
			ShowMessage( ErrString );
		}
		if ( Missed.WindSpd > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Wind Speed\"" << Missed.WindSpd;
			ShowMessage( ErrString );
		}
		if ( Missed.WindDir > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Wind Direction\"" << Missed.WindDir;
			ShowMessage( ErrString );
		}
		if ( Missed.DirectRad > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Direct Radiation\"" << Missed.DirectRad;
			ShowMessage( ErrString );
		}
		if ( Missed.DiffuseRad > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Diffuse Radiation\"" << Missed.DiffuseRad;
			ShowMessage( ErrString );
		}
		//  IF (Missed%Visibility>0) THEN
		//    IF (.not. MissedHeader) THEN
		//      CALL ShowWarningError(MissString)
		//      MissedHeader=.TRUE.
		//    ENDIF
		//    WRITE(ErrString,msFMT) 'Visibility',Missed%Visibility
		//    CALL ShowMessage(ErrString)
		//  ENDIF
		//  IF (Missed%AerOptDepth>0) THEN
		//    IF (.not. MissedHeader) THEN
		//      CALL ShowWarningError(MissString)
		//      MissedHeader=.TRUE.
		//    ENDIF
		//    WRITE(ErrString,msFMT) 'Aerosol Optical Depth',Missed%AerOptDepth
		//    CALL ShowMessage(ErrString)
		//  ENDIF
		if ( Missed.TotSkyCvr > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Total Sky Cover\"" << Missed.TotSkyCvr;
			ShowMessage( ErrString );
		}
		if ( Missed.OpaqSkyCvr > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Opaque Sky Cover\"" << Missed.OpaqSkyCvr;
			ShowMessage( ErrString );
		}
		//  IF (Missed%Ceiling>0) THEN
		//    IF (.not. MissedHeader) THEN
		//      CALL ShowWarningError(MissString)
		//      MissedHeader=.TRUE.
		//    ENDIF
		//    WRITE(ErrString,msFMT) 'Ceiling Height',Missed%Ceiling
		//    CALL ShowMessage(ErrString)
		//  ENDIF
		//  IF (Missed%PrecipWater>0) THEN
		//    IF (.not. MissedHeader) THEN
		//      CALL ShowWarningError(MissString)
		//      MissedHeader=.TRUE.
		//    ENDIF
		//    WRITE(ErrString,msFMT) 'Water Precipitation',Missed%PrecipWater
		//    CALL ShowMessage(ErrString)
		//  ENDIF
		if ( Missed.SnowDepth > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Snow Depth\"" << Missed.SnowDepth;
			ShowMessage( ErrString );
		}
		if ( Missed.WeathCodes > 0 ) {
			ShowWarningError( InvString );
			gio::write( ErrString, ivFmt ) << "\"Weather Codes\" (not equal 9 digits)" << Missed.WeathCodes;
			ShowMessage( ErrString );
		}
		//  IF (Missed%Albedo>0) THEN
		//    IF (.not. MissedHeader) THEN
		//      CALL ShowWarningError(MissString)
		//      MissedHeader=.TRUE.
		//    ENDIF
		//    WRITE(ErrString,msFMT) '"Albedo"',Missed%Albedo
		//    CALL ShowMessage(ErrString)
		//  ENDIF
		if ( Missed.LiquidPrecip > 0 ) {
			if ( ! MissedHeader ) {
				ShowWarningError( MissString );
				MissedHeader = true;
			}
			gio::write( ErrString, msFmt ) << "\"Liquid Precipitation Depth\"" << Missed.LiquidPrecip;
			ShowMessage( ErrString );
		}
		//  IF (Missed%DaysLastSnow>0) THEN
		//    IF (.not. MissedHeader) THEN
		//      CALL ShowWarningError(MissString)
		//      MissedHeader=.TRUE.
		//    ENDIF
		//    WRITE(ErrString,msFMT) 'Days Since Last Snow',Missed%DaysLastSnow
		//    CALL ShowMessage(ErrString)
		//  ENDIF

		OutOfRangeHeader = false;
		if ( OutOfRange.DryBulb > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Dry Bulb Temperatures" << ">=-90" << "<=70" << OutOfRange.DryBulb;
			ShowMessage( ErrString );
		}
		if ( OutOfRange.StnPres > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Atmospheric Pressure" << ">31000" << "<=120000" << OutOfRange.StnPres;
			ShowMessage( ErrString );
			ShowMessage( "Out of Range values set to last good value" );
		}
		if ( OutOfRange.RelHumid > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Relative Humidity" << ">=0" << "<=110" << OutOfRange.RelHumid;
			ShowMessage( ErrString );
		}
		if ( OutOfRange.DewPoint > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Dew Point Temperatures" << ">=-90" << "<=70" << OutOfRange.DewPoint;
			ShowMessage( ErrString );
		}
		if ( OutOfRange.WindSpd > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Wind Speed" << ">=0" << "<=40" << OutOfRange.WindSpd;
			ShowMessage( ErrString );
		}
		if ( OutOfRange.WindDir > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Wind Direction" << ">=0" << "<=360" << OutOfRange.WindDir;
			ShowMessage( ErrString );
		}
		if ( OutOfRange.DirectRad > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Direct Radiation" << ">=0" << "NoLimit" << OutOfRange.DirectRad;
			ShowMessage( ErrString );
		}
		if ( OutOfRange.DiffuseRad > 0 ) {
			if ( ! OutOfRangeHeader ) {
				ShowWarningError( RangeString );
				OutOfRangeHeader = true;
			}
			gio::write( ErrString, rgFmt ) << "Diffuse Radiation" << ">=0" << "NoLimit" << OutOfRange.DiffuseRad;
			ShowMessage( ErrString );
		}

	}

	void
	SetupInterpolationValues()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine creates the "interpolation" values / weights that are used for
		// interpolating weather data from hourly down to the time step level.

		// METHODOLOGY EMPLOYED:
		// Create arrays (InterpolationValues, SolarInterpolationValues) dependent on
		// Number of Time Steps in Hour.  This will be used in the "SetCurrentWeather" procedure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int halfpoint;
		int hpoint;
		int tloop;
		Real64 tweight;
		Real64 tweight1;

		Interpolation.allocate( NumOfTimeStepInHour );
		SolarInterpolation.allocate( NumOfTimeStepInHour );
		Interpolation = 0.0;
		SolarInterpolation = 0.0;
		halfpoint = 0;

		for ( tloop = 1; tloop <= NumOfTimeStepInHour; ++tloop ) {
			if ( NumOfTimeStepInHour == 1 ) {
				tweight = 1.0;
			} else {
				tweight = min( 1.0, ( double( tloop ) / double( NumOfTimeStepInHour ) ) );
			}

			Interpolation( tloop ) = tweight;

		}

		if ( mod( NumOfTimeStepInHour, 2 ) == 0 ) {
			// even number of time steps.
			halfpoint = NumOfTimeStepInHour / 2;
			SolarInterpolation( halfpoint ) = 1.0;
			tweight = 1.0 / double( NumOfTimeStepInHour );
			hpoint = 1;
			for ( tloop = halfpoint + 1; tloop <= NumOfTimeStepInHour; ++tloop ) {
				SolarInterpolation( tloop ) = 1.0 - hpoint * tweight;
				++hpoint;
			}
			hpoint = 1;
			for ( tloop = halfpoint - 1; tloop >= 1; --tloop ) {
				SolarInterpolation( tloop ) = 1.0 - hpoint * tweight;
				++hpoint;
			}
		} else { // odd number of time steps
			if ( NumOfTimeStepInHour == 1 ) {
				SolarInterpolation( 1 ) = 0.5;
			} else if ( NumOfTimeStepInHour == 3 ) {
				tweight = 1.0 / double( NumOfTimeStepInHour );
				SolarInterpolation( 1 ) = 5.0 / 6.0;
				SolarInterpolation( 2 ) = 5.0 / 6.0;
				SolarInterpolation( 3 ) = 0.5;
			} else {
				tweight = 1.0 / double( NumOfTimeStepInHour );
				halfpoint = NumOfTimeStepInHour / 2;
				tweight1 = 1.0 - tweight / 2.0;
				SolarInterpolation( halfpoint ) = tweight1;
				SolarInterpolation( halfpoint + 1 ) = tweight1;
				hpoint = 1;
				for ( tloop = halfpoint + 2; tloop <= NumOfTimeStepInHour; ++tloop ) {
					SolarInterpolation( tloop ) = tweight1 - hpoint * tweight;
					++hpoint;
				}
				hpoint = 1;
				for ( tloop = halfpoint - 1; tloop >= 1; --tloop ) {
					SolarInterpolation( tloop ) = tweight1 - hpoint * tweight;
					++hpoint;
				}
			}
		}

	}

	void
	SetupEnvironmentTypes()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Make sure Environment derived type is set prior to getting
		// Weather Properties

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::InvJulianDay;
		using General::JulianDay;
		using General::BetweenDates;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int Loop1;
		int JDay1;
		int JDay2;
		int LocalLeapYearAdd;

		// Transfer weather file information to the Environment derived type
		Envrn = TotDesDays + 1;

		// Sizing Periods from Weather File
		for ( Loop = 1; Loop <= TotRunDesPers; ++Loop ) {
			auto & env = Environment( Envrn );
			auto & runPer = RunPeriodDesignInput( Loop );

			env.StartMonth = runPer.StartMonth;
			env.StartDay = runPer.StartDay;
			env.StartJDay = JulianDay( runPer.StartMonth, runPer.StartDay, LeapYearAdd );
			env.TotalDays = runPer.TotalDays;
			env.EndMonth = runPer.EndMonth;
			env.EndDay = runPer.EndDay;
			env.EndJDay = JulianDay( runPer.EndMonth, runPer.EndDay, LeapYearAdd );
			env.NumSimYears = runPer.NumSimYears;
			if ( env.StartJDay <= env.EndJDay ) {
				env.TotalDays = ( env.EndJDay - env.StartJDay + 1 ) * env.NumSimYears;
			} else {
				env.TotalDays = ( JulianDay( 12, 31, LeapYearAdd ) - env.StartJDay + 1 + env.EndJDay ) * env.NumSimYears;
			}
			TotRunDesPersDays += env.TotalDays;
			env.UseDST = runPer.UseDST;
			env.UseHolidays = runPer.UseHolidays;
			env.Title = runPer.Title;
			env.cKindOfEnvrn = runPer.PeriodType;
			env.KindOfEnvrn = ksRunPeriodDesign;
			env.DesignDayNum = 0;
			env.RunPeriodDesignNum = Loop;
			env.DayOfWeek = runPer.DayOfWeek;
			env.MonWeekDay = runPer.MonWeekDay;
			env.SetWeekDays = false;
			env.ApplyWeekendRule = runPer.ApplyWeekendRule;
			env.UseRain = runPer.UseRain;
			env.UseSnow = runPer.UseSnow;
			++Envrn;
		}

		// RunPeriods from weather file
		for ( Loop = 1; Loop <= TotRunPers; ++Loop ) { // Run Periods.
			auto & env = Environment( Envrn );
			auto & runPer = RunPeriodInput( Loop );

			env.StartMonth = runPer.StartMonth;
			env.StartDay = runPer.StartDay;
			env.EndMonth = runPer.EndMonth;
			env.EndDay = runPer.EndDay;
			env.NumSimYears = runPer.NumSimYears;
			if ( runPer.ActualWeather ) {
				env.CurrentYear = runPer.StartYear;
				env.IsLeapYear = IsLeapYear( runPer.StartYear );
				env.TreatYearsAsConsecutive = true;
				env.StartYear = runPer.StartYear;
				env.EndYear = runPer.EndYear;
				JGDate( GregorianToJulian, env.StartDate, env.StartYear, env.StartMonth, env.StartDay );
				JGDate( GregorianToJulian, env.EndDate, env.EndYear, env.EndMonth, env.EndDay );
				env.StartJDay = env.StartDate;
				env.EndJDay = env.EndDate;
				env.TotalDays = env.EndDate - env.StartDate + 1;
				env.RawSimDays = env.EndDate - env.StartDate + 1;
				env.MatchYear = true;
				env.ActualWeather = true;
			} else if ( runPer.BeginYear < 100 ) { // std RunPeriod
				env.CurrentYear = 0;
				if ( ! WFAllowsLeapYears ) {
					env.IsLeapYear = false; // explicit set
					LocalLeapYearAdd = 0;
				} else {
					env.IsLeapYear = true; // explicit set
					LocalLeapYearAdd = 1;
				}
				env.TreatYearsAsConsecutive = false;
				env.RollDayTypeOnRepeat = runPer.RollDayTypeOnRepeat;
				env.StartJDay = JulianDay( runPer.StartMonth, runPer.StartDay, LocalLeapYearAdd );
				env.EndJDay = JulianDay( runPer.EndMonth, runPer.EndDay, LocalLeapYearAdd );
				// need message if isleapyear and wfleapyearind=0
				if ( env.StartJDay <= env.EndJDay ) {
					env.RawSimDays = ( env.EndJDay - env.StartJDay + 1 );
					env.TotalDays = ( env.EndJDay - env.StartJDay + 1 ) * env.NumSimYears;
				} else {
					env.RawSimDays = ( JulianDay( 12, 31, LeapYearAdd ) - env.StartJDay + 1 + env.EndJDay );
					env.TotalDays = ( JulianDay( 12, 31, LeapYearAdd ) - env.StartJDay + 1 + env.EndJDay ) * env.NumSimYears;
				}

			} else { // Using Runperiod and StartYear option.
				env.CurrentYear = runPer.BeginYear;
				env.IsLeapYear = IsLeapYear( env.CurrentYear );
				env.TreatYearsAsConsecutive = true;
				env.RollDayTypeOnRepeat = runPer.RollDayTypeOnRepeat;
				env.StartJDay = JulianDay( runPer.StartMonth, runPer.StartDay, LeapYearAdd );
				env.EndJDay = JulianDay( runPer.EndMonth, runPer.EndDay, LeapYearAdd );
				env.TotalDays = 0;
				for ( Loop1 = 1; Loop1 <= env.NumSimYears; ++Loop1 ) {
					if ( ! IsLeapYear( runPer.BeginYear - 1 + Loop1 ) || ! WFAllowsLeapYears ) {
						JDay1 = JulianDay( runPer.StartMonth, runPer.StartDay, 0 );
						JDay2 = JulianDay( runPer.EndMonth, runPer.EndDay, 0 );
						if ( JDay1 <= JDay2 ) {
							if ( Loop1 == 1 ) env.RawSimDays = ( JDay2 - JDay1 + 1 );
							env.TotalDays += ( JDay2 - JDay1 + 1 );
						} else {
							if ( Loop1 == 1 ) env.RawSimDays = JulianDay( 12, 31, 0 ) - JDay1 + 1 + JDay2;
							env.TotalDays += JulianDay( 12, 31, 0 ) - JDay1 + 1 + JDay2;
						}
					} else { // Leap Year
						JDay1 = JulianDay( runPer.StartMonth, runPer.StartDay, 1 );
						JDay2 = JulianDay( runPer.EndMonth, runPer.EndDay, 1 );
						if ( JDay1 <= JDay2 ) {
							env.TotalDays += ( JDay2 - JDay1 + 1 );
						} else {
							env.TotalDays += JulianDay( 12, 31, 1 ) - JDay1 + 1 + JDay2;
						}
					}
				}
			}
			env.UseDST = runPer.UseDST;
			env.UseHolidays = runPer.UseHolidays;
			if ( runPer.Title == BlankString ) {
				env.Title = WeatherFileLocationTitle;
			} else {
				env.Title = runPer.Title;
			}
			if ( env.KindOfEnvrn == ksReadAllWeatherData ) {
				env.cKindOfEnvrn = "ReadAllWeatherDataRunPeriod";
			} else {
				env.cKindOfEnvrn = "WeatherFileRunPeriod";
				env.KindOfEnvrn = ksRunPeriodWeather;
			}
			env.DayOfWeek = runPer.DayOfWeek;
			env.MonWeekDay = runPer.MonWeekDay;
			env.SetWeekDays = false;
			env.ApplyWeekendRule = runPer.ApplyWeekendRule;
			env.UseRain = runPer.UseRain;
			env.UseSnow = runPer.UseSnow;
			++Envrn;
		}

	}

	bool
	IsLeapYear( int const Year )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// From entered year returns true (Yes) if it's a leap year, false (no) if not.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool YesNo;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		YesNo = false;
		if ( mod( Year, 4 ) == 0 ) { // Potential Leap Year
			if ( ! ( mod( Year, 100 ) == 0 && mod( Year, 400 ) != 0 ) ) {
				YesNo = true;
			}
		}
		return YesNo;

	}

	void
	JGDate(
		int const jflag, // indicates direction of conversion,
		int & jdate, // input/output julian date, typically a 7 or 8 digit integer
		int & gyyyy, // input/output gregorian year, should be specified as 4 digits
		int & gmm, // input/output gregorian month
		int & gdd // input/output gregorian day
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine JGDate is a gregorian date to actual julian date
		// converter.  the advantage of storing a julian date in the
		// jdate format rather than a 5 digit format is that any
		// number of days can be add or subtracted to jdate and
		// that result is a proper julian date.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// for discussion of this algorithm,
		// see cacm, vol 11, no 10, oct 1968, page 657

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// 1 --> gregorian (dd/mm/yyyy) to julian
		// 2 --> julian to gregorian.

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int tdate; // integer*4 variable needed for double precision arithmetic
		int tyyyy; // integer*4 variable needed for double precision arithmetic
		int tmm; // integer*4 variable needed for double precision arithmetic
		int tdd; // integer*4 variable needed for double precision arithmetic
		int l; // temporary variable used in conversion.
		int n; // temporary variable used in conversion.

		//                                       gregorian to julian
		if ( jflag == 1 ) {
			tyyyy = gyyyy;
			tmm = gmm;
			tdd = gdd;
			l = ( tmm - 14 ) / 12;
			jdate = tdd - 32075 + 1461 * ( tyyyy + 4800 + l ) / 4 + 367 * ( tmm - 2 - l * 12 ) / 12 - 3 * ( ( tyyyy + 4900 + l ) / 100 ) / 4;

		} else if ( jflag == 2 ) {
			//                                       julian to gregorian
			tdate = jdate;
			l = tdate + 68569;
			n = 4 * l / 146097;
			l -= ( 146097 * n + 3 ) / 4;
			tyyyy = 4000 * ( l + 1 ) / 1461001;
			l = l - 1461 * tyyyy / 4 + 31;
			tmm = 80 * l / 2447;
			tdd = l - 2447 * tmm / 80;
			l = tmm / 11;
			tmm += 2 - 12 * l;
			tyyyy += 100 * ( n - 49 ) + l;
			//c
			gyyyy = tyyyy;
			gdd = tdd;
			gmm = tmm;

		}
		//c

	}

	int
	CalculateDayOfWeek( int const JulianDate ) // from JGDate calculation
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Using Julian date (from jgdate calc), calculate the correct day of week.

		// METHODOLOGY EMPLOYED:
		// Zeller's algorithm.

		// REFERENCES:
		// http://en.wikipedia.org/wiki/Zeller%27s_congruence
		// and other references around the web.

		// USE STATEMENTS:
		// na

		// Return value
		int DayOfWeek; // EnergyPlus convention (1=Sunday, 2=Monday, etc)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int JulDate; // Julian date copy
		int Gyyyy; // Gregorian yyyy
		int Gmm; // Gregorian mm
		int Gdd; // Gregorian dd

		JulDate = JulianDate;
		JGDate( JulianToGregorian, JulDate, Gyyyy, Gmm, Gdd );

		// Jan, Feb are 13, 14 months of previous year
		if ( Gmm < 3 ) {
			Gmm += 12;
			--Gyyyy;
		}

		DayOfWeek = mod( Gdd + ( 13 * ( Gmm + 1 ) / 5 ) + Gyyyy + ( Gyyyy / 4 ) + 6 * ( Gyyyy / 100 ) + ( Gyyyy / 400 ), 7 );
		if ( DayOfWeek == 0 ) DayOfWeek = 7;

		return DayOfWeek;

	}

} // WeatherManager

} // EnergyPlus
