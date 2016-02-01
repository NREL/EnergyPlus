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

#ifndef WeatherManager_hh_INCLUDED
#define WeatherManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace WeatherManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Following are Date Types read in from EPW file or IDF
	extern int const InvalidDate;
	extern int const MonthDay;
	extern int const NthDayInMonth;
	extern int const LastDayInMonth;

	extern int const ScheduleMethod; // Constant for water mains temperatures calculation methods
	extern int const CorrelationMethod; // Constant for water mains temperatures calculation methods

	extern int const InvalidWeatherFile;
	extern int const EPlusWeatherFile;

	extern int const ASHRAE_ClearSky; // Design Day solar model ASHRAE ClearSky (default)
	extern int const Zhang_Huang; // Design Day solar model Zhang Huang
	extern int const SolarModel_Schedule; // Design Day solar model (beam and diffuse) from user entered schedule
	extern int const ASHRAE_Tau; // Design Day solar model ASHRAE tau (per 2009 HOF)

	extern int const DDHumIndType_WetBulb; // Design Day Humidity Indicating Type = Wetbulb (default)
	extern int const DDHumIndType_DewPoint; // Design Day Humidity Indicating Type = Dewpoint
	extern int const DDHumIndType_Enthalpy; // Design Day Humidity Indicating Type = Enthalpy
	extern int const DDHumIndType_HumRatio; // Design Day Humidity Indicating Type = Humidity Ratio
	extern int const DDHumIndType_RelHumSch; // Design Day Humidity Indicating Type = relhum schedule
	extern int const DDHumIndType_WBProfDef; // Design Day Humidity Indicating Type = Wetbulb default profile
	extern int const DDHumIndType_WBProfDif; // Design Day Humidity Indicating Type = Wetbulb difference profile
	extern int const DDHumIndType_WBProfMul; // Design Day Humidity Indicating Type = Wetbulb multiplier profile
	extern int const DDHumIndType_Count; // # of DDHumIndTypes

	extern int const DDDBRangeType_Default; // Design Day DryBulb Range Type = Default Multipliers
	extern int const DDDBRangeType_Multiplier; // Design Day DryBulb Range Type = Multiplier Schedule
	extern int const DDDBRangeType_Difference; // Design Day DryBulb Range Type = Difference Schedule
	extern int const DDDBRangeType_Profile; // Design Day DryBulb Range Type = Temperature Profile

	extern int const WP_ScheduleValue; // User entered Schedule value for Weather Property
	extern int const WP_DryBulbDelta; // User entered DryBulb difference Schedule value for Weather Property
	extern int const WP_DewPointDelta; // User entered Dewpoint difference Schedule value for Weather Property
	extern int const WP_SkyTAlgorithmA; // place holder

	extern int const GregorianToJulian; // JGDate argument for Gregorian to Julian Date conversion
	extern int const JulianToGregorian; // JGDate argument for Julian to Gregorian Date conversion

	extern Real64 const Sigma; // Stefan-Boltzmann constant
	extern Real64 const TKelvin; // conversion from Kelvin to Celsius

	extern Array1D_string const DaysOfWeek;

	extern bool Debugout;

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int YearofSim; // The Present year of Simulation.
	extern int const NumDaysInYear;
	extern int EnvironmentReportNbr; // Report number for the environment stamp
	extern std::string EnvironmentReportChr; // Report number for the environment stamp (character -- for printing)
	extern int TimeStampReportNbr; // Report number for the time stamp
	extern std::string TimeStampReportChr; // Report number for the time stamp (character -- for printing)
	extern int WeatherDataReport; // Report number for the weather data
	extern bool WeatherFileExists; // Set to true if a weather file exists
	extern std::string LocationTitle; // Location Title from input File
	extern bool LocationGathered; // flag to show if Location exists on Input File (we assume one is there and
	// correct on weather file)

	extern Real64 WeatherFileLatitude;
	extern Real64 WeatherFileLongitude;
	extern Real64 WeatherFileTimeZone;
	extern Real64 WeatherFileElevation;
	extern int WeatherFileUnitNumber; // File unit number for the weather file
	extern Array1D< Real64 > GroundTempsFCFromEPWHeader; // F or C factor method
	extern Array1D< Real64 > GroundReflectances; // User Specified Ground Reflectances
	extern Real64 SnowGndRefModifier; // Modifier to ground reflectance during snow
	extern Real64 SnowGndRefModifierForDayltg; // Modifier to ground reflectance during snow for daylighting
	extern int WaterMainsTempsMethod; // Water mains temperature calculation method
	extern int WaterMainsTempsSchedule; // Water mains temperature schedule
	extern Real64 WaterMainsTempsAnnualAvgAirTemp; // Annual average outdoor air temperature (C)
	extern Real64 WaterMainsTempsMaxDiffAirTemp; // Maximum difference in monthly average outdoor air temperatures (deltaC)
	extern bool wthFCGroundTemps;
	extern Real64 RainAmount;
	extern Real64 SnowAmount;

	extern int TotRunPers; // Total number of Run Periods (Weather data) to Setup
	extern int TotRunDesPers; // Total number of Run Design Periods (Weather data) to Setup

	extern int NumSpecialDays;
	extern Array1D_int SpecialDayTypes; // To hold holiday types given in input file
	extern Array1D_int WeekDayTypes; // To hold Week day types using specified first day
	extern Array1D_int DSTIndex; // To hold DST Index based on weather file or input

	extern int NumDataPeriods;

	extern int NumIntervalsPerHour;

	extern bool UseDaylightSaving; // True if user says to use Weather File specified DaylightSaving Period
	extern bool UseSpecialDays; // True if user says to use Weather File specified Special Days for current RunPeriod
	extern bool UseRainValues; // True if rain values from weather file are to be used
	extern bool UseSnowValues; // True if snow values from weather file are to be used
	extern bool EPWDaylightSaving; // True if a DaylightSaving Time Period is input (EPW files)
	extern bool IDFDaylightSaving; // True if a DaylightSaving Time Period is input (IDF files)
	extern bool DaylightSavingIsActive; // True if a DaylightSavingPeriod should be used for Environment
	extern bool WFAllowsLeapYears; // True if the Weather File (WF) header has "Yes" for Leap Years
	extern int WFLeapYearInd; // Indicator for current Weather file "Leap Year", used in DayOfYear calculations and others.
	extern int curSimDayForEndOfRunPeriod; // normal=number days in sim, but different when repeating runperiods or multi-year files
	extern int Envrn; // Counter for environments
	extern int NumOfEnvrn; // Number of environments to be simulated
	extern int NumEPWTypExtSets; // Number of Typical/Extreme on weather file.
	extern int NumWPSkyTemperatures; // Number of WeatherProperty:SkyTemperature items in input file

	extern Array2D_bool TodayIsRain; // Rain indicator, true=rain
	extern Array2D_bool TodayIsSnow; // Snow indicator, true=snow
	extern Array2D< Real64 > TodayRainAmount; // ficitious indicator of Rain
	extern Array2D< Real64 > TodaySnowAmount; // ficitious indicator of Snow
	extern Array2D< Real64 > TodayOutDryBulbTemp; // Dry bulb temperature of outside air
	extern Array2D< Real64 > TodayOutWetBulbTemp; // Wet bulb temperature of outside air
	extern Array2D< Real64 > TodayOutDewPointTemp; // Dew Point Temperature of outside air
	extern Array2D< Real64 > TodayOutBaroPress; // Barometric pressure of outside air
	extern Array2D< Real64 > TodayOutHumRat; // Humidity ratio of outside air
	extern Array2D< Real64 > TodayOutRelHum; // Relative Humidity of outside air
	extern Array2D< Real64 > TodayWindSpeed; // Wind speed of outside air
	extern Array2D< Real64 > TodayWindDir; // Wind direction of outside air
	extern Array2D< Real64 > TodaySkyTemp; // Sky temperature
	extern Array2D< Real64 > TodayHorizIRSky; // Horizontal IR from Sky
	extern Array2D< Real64 > TodayBeamSolarRad; // Direct normal solar irradiance
	extern Array2D< Real64 > TodayDifSolarRad; // Sky diffuse horizontal solar irradiance
	extern Array2D< Real64 > TodayAlbedo; // Albedo
	extern Array2D< Real64 > TodayLiquidPrecip; // Liquid Precipitation Depth (mm)

	extern Array2D_bool TomorrowIsRain; // Rain indicator, true=rain
	extern Array2D_bool TomorrowIsSnow; // Snow indicator, true=snow
	extern Array2D< Real64 > TomorrowRainAmount; // ficitious indicator of Rain
	extern Array2D< Real64 > TomorrowSnowAmount; // ficitious indicator of Snow
	extern Array2D< Real64 > TomorrowOutDryBulbTemp; // Dry bulb temperature of outside air
	extern Array2D< Real64 > TomorrowOutDewPointTemp; // Dew Point Temperature of outside air
	extern Array2D< Real64 > TomorrowOutBaroPress; // Barometric pressure of outside air
	extern Array2D< Real64 > TomorrowOutRelHum; // Relative Humidity of outside air
	extern Array2D< Real64 > TomorrowWindSpeed; // Wind speed of outside air
	extern Array2D< Real64 > TomorrowWindDir; // Wind direction of outside air
	extern Array2D< Real64 > TomorrowSkyTemp; // Sky temperature
	extern Array2D< Real64 > TomorrowHorizIRSky; // Horizontal IR from Sky
	extern Array2D< Real64 > TomorrowBeamSolarRad; // Direct normal solar irradiance
	extern Array2D< Real64 > TomorrowDifSolarRad; // Sky diffuse horizontal solar irradiance
	extern Array2D< Real64 > TomorrowAlbedo; // Albedo
	extern Array2D< Real64 > TomorrowLiquidPrecip; // Liquid Precipitation Depth

	extern Array3D< Real64 > DDDBRngModifier; // Design Day Dry-bulb Temperature Range Modifier
	extern Array3D< Real64 > DDHumIndModifier; // Design Day relative humidity values
	//   or wet-bulb modifiers (per HumIndType)
	extern Array3D< Real64 > DDBeamSolarValues; // Design Day Beam Solar Values
	extern Array3D< Real64 > DDDiffuseSolarValues; // Design Day Relative Humidity Values

	extern Array3D< Real64 > DDSkyTempScheduleValues; // Sky temperature - DesignDay input

	extern int RptIsRain; // Rain Report Value
	extern int RptIsSnow; // Snow Report Value
	extern int RptDayType; // DayType Report Value

	extern Real64 HrAngle; // Current Hour Angle
	extern Real64 SolarAltitudeAngle; // Angle of Solar Altitude (degrees)
	extern Real64 SolarAzimuthAngle; // Angle of Solar Azimuth (degrees)
	extern Real64 HorizIRSky; // Horizontal Infrared Radiation Intensity (W/m2)
	extern Real64 TimeStepFraction; // Fraction of hour each time step represents
	extern Array1D< Real64 > SPSiteDryBulbRangeModScheduleValue; // reporting Drybulb Temperature Range Modifier Schedule Value
	extern Array1D< Real64 > SPSiteHumidityConditionScheduleValue; // reporting Humidity Condition Schedule Value
	extern Array1D< Real64 > SPSiteBeamSolarScheduleValue; // reporting Beam Solar Schedule Value
	extern Array1D< Real64 > SPSiteDiffuseSolarScheduleValue; // reporting Diffuse Solar Schedule Value
	extern Array1D< Real64 > SPSiteSkyTemperatureScheduleValue; // reporting SkyTemperature Modifier Schedule Value
	extern Array1D_int SPSiteScheduleNamePtr; // SP Site Schedule Name Ptrs
	extern Array1D_string SPSiteScheduleUnits; // SP Site Schedule Units
	extern int NumSPSiteScheduleNamePtrs; // Number of SP Site Schedules (DesignDay only)
	extern int NumMissing; // Number of hours of missing data
	extern Array1D< Real64 > Interpolation; // Interpolation values based on Number of Time Steps in Hour
	extern Array1D< Real64 > SolarInterpolation; // Solar Interpolation values based on
	//      Number of Time Steps in Hour
	extern Array1D_int EndDayOfMonth;
	extern bool ErrorInWeatherFile; // Set to TRUE when there is a problem with dates
	extern int LeapYearAdd; // Set during environment if leap year is active (adds 1 to number days in Feb)
	extern bool DatesShouldBeReset; // True when weekdays should be reset
	extern bool StartDatesCycleShouldBeReset; // True when start dates on repeat should be reset
	extern bool Jan1DatesShouldBeReset; // True if Jan 1 should signal reset of dates
	extern bool RPReadAllWeatherData; // True if need to read all weather data prior to simulation

	// SUBROUTINE SPECIFICATIONS FOR MODULE WeatherManager
	//PUBLIC  ProcessDateString
	// Get Input from Input File

	// Types

	struct EnvironmentData
	{
		// Members
		std::string Title; // Environment name
		std::string cKindOfEnvrn; // kind of environment
		int KindOfEnvrn; // Type of environment (see Parameters for KindOfSim in DataGlobals)
		int DesignDayNum; // index in DesignDay structure and DesignDayInput
		int RunPeriodDesignNum; // for WeatherFileDays, index in  RunPeriodDesign and RunPeriodDesignInput
		int SeedEnvrnNum; // for HVAC sizing sim, new environments are copies of original environments, this is the index for original
		int HVACSizingIterationNum; // environments for HVAC sizing simulations are associated with iteration
		int TotalDays; // Number of days in environment
		int StartJDay; // Day of year of first day of environment
		int StartMonth;
		int StartDay;
		int StartYear;
		int StartDate;
		int EndMonth;
		int EndDay;
		int EndJDay;
		int EndYear;
		int EndDate;
		int DayOfWeek; // Starting Day of Week for the (Weather) RunPeriod (User Input)
		bool UseDST; // True if DaylightSavingTime is used for this RunPeriod
		bool UseHolidays; // True if Holidays are used for this RunPeriod (from WeatherFile)
		bool ApplyWeekendRule; // True if "Weekend Rule" is to be applied to RunPeriod
		bool UseRain; // True if Rain from weather file should be used (set rain to true)
		bool UseSnow; // True if Snow from weather file should be used (set Snow to true)
		Array1D_int MonWeekDay;
		bool SetWeekDays; // true when weekdays will be reset (after first year or on repeat)
		int NumSimYears; // Total Number of times this period to be performed
		int CurrentCycle; // Current cycle through weather file in NumSimYears repeats
		int WP_Type1; // WeatherProperties SkyTemperature Pointer
		int CurrentYear; // Current year
		bool IsLeapYear; // True if current year is leap year.
		bool RollDayTypeOnRepeat; // If repeating run period, increment day type on repeat.
		bool TreatYearsAsConsecutive; // When year rolls over, increment year and recalculate Leap Year
		bool MatchYear; // for actual weather will be true
		bool ActualWeather; // true when using actual weather data
		int RawSimDays; // number of basic sim days.

		// Default Constructor
		EnvironmentData() :
			KindOfEnvrn( 0 ),
			DesignDayNum( 0 ),
			RunPeriodDesignNum( 0 ),
			SeedEnvrnNum( 0 ),
			HVACSizingIterationNum( 0 ),
			TotalDays( 0 ),
			StartJDay( 0 ),
			StartMonth( 0 ),
			StartDay( 0 ),
			StartYear( 0 ),
			StartDate( 0 ),
			EndMonth( 0 ),
			EndDay( 0 ),
			EndJDay( 0 ),
			EndYear( 0 ),
			EndDate( 0 ),
			DayOfWeek( 0 ),
			UseDST( false ),
			UseHolidays( false ),
			ApplyWeekendRule( false ),
			UseRain( true ),
			UseSnow( true ),
			MonWeekDay( 12, 0 ),
			SetWeekDays( false ),
			NumSimYears( 1 ),
			CurrentCycle( 0 ),
			WP_Type1( 0 ),
			CurrentYear( 0 ),
			IsLeapYear( false ),
			RollDayTypeOnRepeat( true ),
			TreatYearsAsConsecutive( true ),
			MatchYear( false ),
			ActualWeather( false ),
			RawSimDays( 0 )
		{}

	};

	struct DesignDayData
	{
		// Members
		std::string Title; // Environment name
		Real64 MaxDryBulb; // Maximum Dry-Bulb Temperature (C)
		Real64 DailyDBRange; // Daily Temperature Range (deltaC)
		Real64 HumIndValue; // Humidity Indicating Value at Max Dry-bulb Temperature
		int HumIndType; // Humidity Indicating type  (see Parameters)
		Real64 PressBarom; // Atmospheric/Barometric Pressure (Pascals)
		Real64 WindSpeed; // Wind Speed (m/s)
		Real64 WindDir; // Wind Direction (degrees clockwise from North, N=0, E=90, S=180, W=270)
		Real64 SkyClear; // Sky Clearness (0 to 1)
		int RainInd; // Rain Indicator (1 = raining and surfaces are wet, else 0)
		int SnowInd; // Snow Indicator (1 = snow on ground, else  0)
		int DayOfMonth; // Day of Month ( 1 - 31 )
		int Month; // Month of Year ( 1 - 12 )
		int DayType; // Day Type Sunday = 1 - Saturday = 7
		int DSTIndicator; // Daylight Saving Time Period Indicator (1=yes, 0=no) for this DesignDay
		int SolarModel; // Solar Model for creating solar values for design day.
		int DBTempRangeType; // Drybulb Range Type (see Parameters)
		int TempRangeSchPtr; // Schedule pointer to a day schedule for dry-bulb temperature range multipliers
		int HumIndSchPtr; // Schedule pointer to a day schedule that specifies
		//    relative humidity (%) or wet-bulb range multipliers per HumIndType
		int BeamSolarSchPtr; // Schedule pointer to a day schedule for beam solar
		int DiffuseSolarSchPtr; // Schedule pointer to a day schedule for diffuse solar
		Real64 TauB; // beam pseudo optical depth for ASHRAE tau model
		Real64 TauD; // diffuse pseudo optical depth for ASHRAE tau model
		Real64 DailyWBRange; // daily range of wetbulb (deltaC)
		bool PressureEntered; // true if a pressure was entered in design day data
		bool DewPointNeedsSet; // true if the Dewpoint humidicating value needs to be set (after location determined)

		// Default Constructor
		DesignDayData() :
			MaxDryBulb( 0.0 ),
			DailyDBRange( 0.0 ),
			HumIndValue( 0.0 ),
			HumIndType( 0 ),
			PressBarom( 0.0 ),
			WindSpeed( 0.0 ),
			WindDir( 0.0 ),
			SkyClear( 0.0 ),
			RainInd( 0 ),
			SnowInd( 0 ),
			DayOfMonth( 0 ),
			Month( 0 ),
			DayType( 0 ),
			DSTIndicator( 0 ),
			SolarModel( 0 ),
			DBTempRangeType( 0 ),
			TempRangeSchPtr( 0 ),
			HumIndSchPtr( 0 ),
			BeamSolarSchPtr( 0 ),
			DiffuseSolarSchPtr( 0 ),
			TauB( 0.0 ),
			TauD( 0.0 ),
			DailyWBRange( 0.0 ),
			PressureEntered( false ),
			DewPointNeedsSet( false )
		{}

	};

	struct RunPeriodData
	{
		// Members
		std::string Title;
		std::string PeriodType;
		int TotalDays; // total number of days in requested period
		int StartMonth;
		int StartDay;
		int StartDate; // Calculated start date (Julian) for a weather file run period
		int StartYear; // entered in "consecutive"/real runperiod object
		int EndMonth;
		int EndDay;
		int EndDate; // Calculated end date (Julian) for a weather file run period
		int EndYear; // entered in "consecutive"/real runperiod object
		int DayOfWeek; // Day of Week that the RunPeriod will start on (User Input)
		bool UseDST; // True if DaylightSavingTime is used for this RunPeriod
		bool UseHolidays; // True if Holidays are used for this RunPeriod (from WeatherFile)
		bool ApplyWeekendRule; // True if "Weekend Rule" is to be applied to RunPeriod
		bool UseRain; // True if Rain from weather file should be used (set rain to true)
		bool UseSnow; // True if Snow from weather file should be used (set Snow to true)
		Array1D_int MonWeekDay;
		int NumSimYears; // Total Number of years of simulation to be performed
		int BeginYear; // Start year entered in regular RunPeriod object
		bool IsLeapYear; // True if Begin Year is leap year.
		bool RollDayTypeOnRepeat; // If repeating run period, increment day type on repeat.
		bool TreatYearsAsConsecutive; // When year rolls over, increment year and recalculate Leap Year
		bool ActualWeather; // true when using actual weather data

		// Default Constructor
		RunPeriodData() :
			TotalDays( 0 ),
			StartMonth( 1 ),
			StartDay( 1 ),
			StartDate( 0 ),
			StartYear( 0 ),
			EndMonth( 12 ),
			EndDay( 31 ),
			EndDate( 0 ),
			EndYear( 0 ),
			DayOfWeek( 0 ),
			UseDST( false ),
			UseHolidays( false ),
			ApplyWeekendRule( false ),
			UseRain( true ),
			UseSnow( true ),
			MonWeekDay( 12, 0 ),
			NumSimYears( 1 ),
			BeginYear( 0 ),
			IsLeapYear( false ),
			RollDayTypeOnRepeat( true ),
			TreatYearsAsConsecutive( true ),
			ActualWeather( false )
		{}

	};

	struct DayWeatherVariables // Derived Type for Storing Weather "Header" Data
	{
		// Members
		int DayOfYear; // Day of year for weather data
		int DayOfYear_Schedule; //Day of year in schedule
		int Year; // Year of weather data
		int Month; // Month of weather data
		int DayOfMonth; // Day of month for weather data
		int DayOfWeek; // Day of week for weather data
		int DaylightSavingIndex; // Daylight Saving Time Period indicator (0=no,1=yes)
		int HolidayIndex; // Holiday indicator (0=no holiday, non-zero=holiday type)
		Real64 SinSolarDeclinAngle; // Sine of the solar declination angle
		Real64 CosSolarDeclinAngle; // Cosine of the solar declination angle
		Real64 EquationOfTime; // Value of the equation of time formula

		// Default Constructor
		DayWeatherVariables() :
			DayOfYear( 0 ),
			DayOfYear_Schedule( 0 ),
			Year( 0 ),
			Month( 0 ),
			DayOfMonth( 0 ),
			DayOfWeek( 0 ),
			DaylightSavingIndex( 0 ),
			HolidayIndex( 0 ),
			SinSolarDeclinAngle( 0.0 ),
			CosSolarDeclinAngle( 0.0 ),
			EquationOfTime( 0.0 )
		{}
	};

	struct SpecialDayData
	{
		// Members
		std::string Name; // Name
		int DateType; // Date type as read in from IDF
		int Month; // Start Month
		int Day; // Start Day of month or Count for DateTypes=NthDayOfMonth
		int WeekDay; // For Date types=NthDayOfMonth and LastDayOfMonth
		int CompDate; // Start Date in "compressed date" format, only if Month/Day
		bool WthrFile; // True if this Special Day came from weather file (EPW)
		int Duration; // Number of days this special Day is used for
		int DayType; // Day Type desigation for this Special Day period
		int ActStMon;
		int ActStDay;
		bool Used; // Set to true in a run period after use (NthDayOfMonth and LastDayOfMonth only)

		// Default Constructor
		SpecialDayData() :
			DateType( 0 ),
			Month( 0 ),
			Day( 0 ),
			WeekDay( 0 ),
			CompDate( 0 ),
			WthrFile( false ),
			Duration( 0 ),
			DayType( 0 ),
			ActStMon( 0 ),
			ActStDay( 0 ),
			Used( false )
		{}

	};

	struct DataPeriodData
	{
		// Members
		std::string Name; // DataPeriod Title
		std::string DayOfWeek; // Start Day of Week for DataPeriod
		int NumYearsData; // Number of years for which data is present in EPW.
		int WeekDay;
		int StMon;
		int StDay;
		int StYear;
		int EnMon;
		int EnDay;
		int EnYear;
		int NumDays;
		Array1D_int MonWeekDay;
		int DataStJDay;
		int DataEnJDay;
		bool HasYearData;

		// Default Constructor
		DataPeriodData() :
			NumYearsData( 1 ),
			WeekDay( 0 ),
			StMon( 0 ),
			StDay( 0 ),
			StYear( 0 ),
			EnMon( 0 ),
			EnDay( 0 ),
			EnYear( 0 ),
			NumDays( 0 ),
			MonWeekDay( 12, 0 ),
			DataStJDay( 0 ),
			DataEnJDay( 0 ),
			HasYearData( false )
		{}

	};

	struct DaylightSavingPeriodData
	{
		// Members
		int StDateType; // Start Date type as from EPW or IDF
		int StWeekDay; // For DateTypes=NthDayOfMonth or LastDayOfMonth
		int StMon; // DaylightSavingTime (DST) Start Month
		int StDay; // DaylightSavingTime (DST) Start Day
		int EnDateType; // End Date type as from EPW or IDF
		int EnMon; // DaylightSavingTime (DST) End Month
		int EnDay; // DaylightSavingTime (DST) End Day
		int EnWeekDay; // For DateTypes=NthDayOfMonth or LastDayOfMonth

		// Default Constructor
		DaylightSavingPeriodData() :
			StDateType( 0 ),
			StWeekDay( 0 ),
			StMon( 0 ),
			StDay( 0 ),
			EnDateType( 0 ),
			EnMon( 0 ),
			EnDay( 0 ),
			EnWeekDay( 0 )
		{}

	};

	struct MissingData // This Derived type carries the default missing data
	{
		// Members
		// for those data elements that would be best replaced
		// with the previous hour's data for missing data.
		Real64 DryBulb; // Dry Bulb Temperature (C)
		Real64 DewPoint; // Dew Point Temperature (C)
		int RelHumid; // Relative Humidity (%)
		Real64 StnPres; // Atmospheric Pressure (Pa)
		int WindDir; // Wind Direction (deg)
		Real64 WindSpd; // Wind Speed/Velocity (m/s)
		int TotSkyCvr; // Total Sky Cover (tenths)
		int OpaqSkyCvr; // Opaque Sky Cover (tenths)
		Real64 Visibility; // Visibility (km)
		int Ceiling; // Ceiling Height (m)
		int PrecipWater; // Precipitable Water (mm)
		Real64 AerOptDepth; // Aerosol Optical Depth
		int SnowDepth; // Snow Depth (cm)
		int DaysLastSnow; // Number of Days since last snow
		Real64 Albedo; // Albedo
		Real64 LiquidPrecip; // Rain/Liquid Precipitation (mm)

		// Default Constructor
		MissingData() :
			DryBulb( 0.0 ),
			DewPoint( 0.0 ),
			RelHumid( 0 ),
			StnPres( 0.0 ),
			WindDir( 0 ),
			WindSpd( 0.0 ),
			TotSkyCvr( 0 ),
			OpaqSkyCvr( 0 ),
			Visibility( 0.0 ),
			Ceiling( 0 ),
			PrecipWater( 0 ),
			AerOptDepth( 0.0 ),
			SnowDepth( 0 ),
			DaysLastSnow( 0 ),
			Albedo( 0.0 ),
			LiquidPrecip( 0.0 )
		{}

	};

	struct MissingDataCounts // This Derived type carries the counts of missing data
	{
		// Members
		// items in the weather reading process.  It will count
		// only items that are on the source file -- not those that
		// are derived from data on the source file.
		// Comments below illustrate the data that is being counted:
		int DryBulb; // Dry Bulb Temperature (C)
		int DewPoint; // Dew Point Temperature (C)
		int RelHumid; // Relative Humidity (%)
		int StnPres; // Atmospheric Pressure (Pa)
		int WindDir; // Wind Direction (deg)
		int WindSpd; // Wind Speed/Velocity (m/s)
		int DirectRad; // Direct Radiation (wh/m2)
		int DiffuseRad; // Diffuse Radiation (wh/m2)
		int TotSkyCvr; // Total Sky Cover (tenths)
		int OpaqSkyCvr; // Opaque Sky Cover (tenths)
		int Visibility; // Visibility (km)
		int Ceiling; // Ceiling Height (m)
		int PrecipWater; // Precipitable Water (mm)
		int AerOptDepth; // Aerosol Optical Depth
		int SnowDepth; // Snow Depth (cm)
		int DaysLastSnow; // Number of Days since last snow
		int WeathCodes; // Weather codes invalid
		int Albedo; // Albedo
		int LiquidPrecip; // Liquid Precip Depth

		// Default Constructor
		MissingDataCounts() :
			DryBulb( 0 ),
			DewPoint( 0 ),
			RelHumid( 0 ),
			StnPres( 0 ),
			WindDir( 0 ),
			WindSpd( 0 ),
			DirectRad( 0 ),
			DiffuseRad( 0 ),
			TotSkyCvr( 0 ),
			OpaqSkyCvr( 0 ),
			Visibility( 0 ),
			Ceiling( 0 ),
			PrecipWater( 0 ),
			AerOptDepth( 0 ),
			SnowDepth( 0 ),
			DaysLastSnow( 0 ),
			WeathCodes( 0 ),
			Albedo( 0 ),
			LiquidPrecip( 0 )
		{}

	};

	struct RangeDataCounts // This Derived type carries the counts of out of range
	{
		// Members
		// items in the weather reading process.  It will count
		// only items that are on the source file -- not those that
		// are derived from data on the source file.
		// Comments below illustrate the data that is being counted:
		int DryBulb; // Dry Bulb Temperature (C)
		int DewPoint; // Dew Point Temperature (C)
		int RelHumid; // Relative Humidity (%)
		int StnPres; // Atmospheric Pressure (Pa)
		int WindDir; // Wind Direction (deg)
		int WindSpd; // Wind Speed/Velocity (m/s)
		int DirectRad; // Direct Radiation (wh/m2)
		int DiffuseRad; // Diffuse Radiation (wh/m2)

		// Default Constructor
		RangeDataCounts() :
			DryBulb( 0 ),
			DewPoint( 0 ),
			RelHumid( 0 ),
			StnPres( 0 ),
			WindDir( 0 ),
			WindSpd( 0 ),
			DirectRad( 0 ),
			DiffuseRad( 0 )
		{}

	};

	struct TypicalExtremeData
	{
		// Members
		std::string Title; // Environment name
		std::string ShortTitle; // Environment name
		std::string MatchValue; // String to be matched for input/running these periods for design.
		std::string MatchValue1; // String to be also matched (synonym)
		std::string MatchValue2; // String to be also matched (synonym)
		std::string TEType; // Typical or Extreme
		int TotalDays; // Number of days in environment
		int StartJDay; // Day of year of first day of environment
		int StartMonth;
		int StartDay;
		int EndMonth;
		int EndDay;
		int EndJDay;

		// Default Constructor
		TypicalExtremeData() :
			TotalDays( 0 ),
			StartJDay( 0 ),
			StartMonth( 0 ),
			StartDay( 0 ),
			EndMonth( 0 ),
			EndDay( 0 ),
			EndJDay( 0 )
		{}

	};

	struct WeatherProperties
	{
		// Members
		std::string Name; // Reference Name
		std::string ScheduleName; // Schedule Name or Algorithm Name
		bool IsSchedule; // Default is using Schedule
		int CalculationType;
		int SchedulePtr; // pointer to schedule when used
		bool UsedForEnvrn;

		// Default Constructor
		WeatherProperties() :
			IsSchedule( true ),
			CalculationType( 0 ),
			SchedulePtr( 0 ),
			UsedForEnvrn( false )
		{}

	};

	// Object Data
	extern DayWeatherVariables TodayVariables; // Today's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year for weather data | Year of weather data | Month of weather data | Day of month for weather data | Day of week for weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday indicator (0=no holiday, non-zero=holiday type) | Sine of the solar declination angle | Cosine of the solar declination angle | Value of the equation of time formula
	extern DayWeatherVariables TomorrowVariables; // Tomorrow's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year for weather data | Year of weather data | Month of weather data | Day of month for weather data | Day of week for weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday indicator (0=no holiday, non-zero=holiday type) | Sine of the solar declination angle | Cosine of the solar declination angle | Value of the equation of time formula
	extern Array1D< DayWeatherVariables > DesignDay; // Design day environments
	extern MissingData Missing; // Dry Bulb Temperature (C) | Dew Point Temperature (C) | Relative Humidity (%) | Atmospheric Pressure (Pa) | Wind Direction (deg) | Wind Speed/Velocity (m/s) | Total Sky Cover (tenths) | Opaque Sky Cover (tenths) | Visibility (km) | Ceiling Height (m) | Precipitable Water (mm) | Aerosol Optical Depth | Snow Depth (cm) | Number of Days since last snow | Albedo | Rain/Liquid Precipitation (mm)
	extern MissingDataCounts Missed;
	extern RangeDataCounts OutOfRange;
	extern Array1D< DesignDayData > DesDayInput; // Design day Input Data
	extern Array1D< EnvironmentData > Environment; // Environment data
	extern Array1D< RunPeriodData > RunPeriodInput;
	extern Array1D< RunPeriodData > RunPeriodDesignInput;
	extern Array1D< TypicalExtremeData > TypicalExtremePeriods;
	extern DaylightSavingPeriodData EPWDST; // Daylight Saving Period Data from EPW file
	extern DaylightSavingPeriodData IDFDST; // Daylight Saving Period Data from IDF file
	extern DaylightSavingPeriodData DST; // Daylight Saving Period Data, if active
	extern Array1D< WeatherProperties > WPSkyTemperature;
	extern Array1D< SpecialDayData > SpecialDays;
	extern Array1D< DataPeriodData > DataPeriods;

	// Functions
	void
	clear_state();

	void
	ManageWeather();

	void
	ResetEnvironmentCounter();

	void
	GetNextEnvironment(
		bool & Available, // true if there is another environment, false if the end
		bool & ErrorsFound // will be set to true if severe errors are found in inputs
	);

	void
	AddDesignSetToEnvironmentStruct(
		int const HVACSizingIterCount // Counter for number of times HVAC Sizing Simulation of Design Period set is being rerun
	);

	void
	SetupWeekDaysByMonth(
		int const StMon,
		int const StDay,
		int const StWeekDay,
		Array1A_int WeekDays
	);

	void
	ResetWeekDaysByMonth(
		Array1A_int WeekDays,
		int const LeapYearAdd,
		int const StartMonth,
		int const StartMonthDay,
		int const EndMonth,
		int const EndMonthDay,
		bool const Rollover,
		Optional_bool_const MidSimReset = _
	);

	void
	SetDSTDateRanges(
		Array1S_int MonWeekDay, // Weekday of each day 1 of month
		Array1S_int DSTIndex, // DST Index for each julian day (1:366)
		Optional_int DSTActStMon = _,
		Optional_int DSTActStDay = _,
		Optional_int DSTActEnMon = _,
		Optional_int DSTActEnDay = _
	);

	void
	SetSpecialDayDates( Array1S_int MonWeekDay ); // Weekday of each day 1 of month

	void
	InitializeWeather( bool & PrintEnvrnStamp ); // Set to true when the environment header should be printed

	void
	UpdateWeatherData();

	void
	SetCurrentWeather();

	void
	ReadWeatherForDay(
		int const DayToRead, // =1 when starting out, otherwise signifies next day
		int const Environ, // Environment being simulated
		bool const BackSpaceAfterRead // True if weather file is to be backspaced after read
	);

	void
	ReadEPlusWeatherForDay(
		int const DayToRead, // =1 when starting out, otherwise signifies next day
		int const Environ, // Environment being simulated
		bool const BackSpaceAfterRead // True if weather file is to be backspaced after read
	);

	void
	SetDayOfWeekInitialValues(
		int const EnvironDayOfWeek, // Starting Day of Week for the (Weather) RunPeriod (User Input)
		int & CurDayOfWeek, // Current Day of Week
		bool & UseDayOfWeek // hmmm does not appear to be used anywhere.
	);

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
	);

	void
	SetUpDesignDay( int const EnvrnNum ); // Environment number passed into the routine

	//------------------------------------------------------------------------------

	Real64
	AirMass( Real64 const CosZen ); // COS( solar zenith), 0 - 1

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
	);

	void
	AllocateWeatherData();

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
	);

	void
	CalculateSunDirectionCosines(
		Real64 const TimeValue, // Current Time of Day
		Real64 const EqOfTime, // Equation of Time
		Real64 const SinSolDeclin, // Sine of Solar Declination
		Real64 const CosSolDeclin, // Cosine of Solar Declination
		Array1A< Real64 > SUNCOS
	);

	void
	DetermineSunUpDown( Array1A< Real64 > SunDirectionCosines );

	void
	OpenWeatherFile( bool & ErrorsFound );

	void
	OpenEPlusWeatherFile(
		bool & ErrorsFound, // Will be set to true if errors found
		bool const ProcessHeader // Set to true when headers should be processed (rather than just read)
	);

	void
	CloseWeatherFile();

	void
	ResolveLocationInformation( bool & ErrorsFound ); // Set to true if no location evident

	void
	CheckLocationValidity();

	void
	CheckWeatherFileValidity();

	void
	ReportOutputFileHeaders();

	void
	ReportWeatherAndTimeInformation( bool & PrintEnvrnStamp ); // Set to true when the environment header should be printed

	void
	ReadUserWeatherInput();

	void
	GetRunPeriodData(
		int & TotRunPers, // Total number of Run Periods requested
		bool & ErrorsFound
	);

	void
	GetRunPeriodDesignData( bool & ErrorsFound );

	void
	GetSpecialDayPeriodData( bool & ErrorsFound ); // will be set to true if severe errors are found in inputs

	void
	CalcSpecialDayTypes();

	void
	GetDSTData( bool & ErrorsFound ); // will be set to true if severe errors are found in inputs

	void
	GetDesignDayData(
		int & TotDesDays, // Total number of Design days to Setup
		bool & ErrorsFound
	);

	void
	GetLocationInfo( bool & ErrorsFound );

	void
	GetWeatherProperties( bool & ErrorsFound );

	void
	GetGroundTemps( bool & ErrorsFound );

	void
	GetGroundReflectances( bool & ErrorsFound );

	void
	GetSnowGroundRefModifiers( bool & ErrorsFound );

	void
	GetWaterMainsTemperatures( bool & ErrorsFound );

	void
	CalcWaterMainsTemp();

	void
	GetWeatherStation( bool & ErrorsFound );

	void
	DayltgCurrentExtHorizIllum();

	void
	DayltgLuminousEfficacy(
		Real64 & DiffLumEff, // Luminous efficacy of sky diffuse solar radiation (lum/W)
		Real64 & DirLumEff // Luminous efficacy of beam solar radiation (lum/W)
	);

	Real64
	GetSTM( Real64 const Longitude ); // Longitude from user input

	void
	ProcessEPWHeader(
		std::string const & HeaderString,
		std::string & Line,
		bool & ErrorsFound
	);

	void
	SkipEPlusWFHeader();

	void
	ReportMissing_RangeData();

	void
	SetupInterpolationValues();

	void
	SetupEnvironmentTypes();

	bool
	IsLeapYear( int const Year );

	void
	JGDate(
		int const jflag, // indicates direction of conversion,
		int & jdate, // input/output julian date, typically a 7 or 8 digit integer
		int & gyyyy, // input/output gregorian year, should be specified as 4 digits
		int & gmm, // input/output gregorian month
		int & gdd // input/output gregorian day
	);

	int
	CalculateDayOfWeek( int const JulianDate ); // from JGDate calculation

} // WeatherManager

} // EnergyPlus

#endif
