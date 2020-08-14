// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#ifndef WeatherManager_hh_INCLUDED
#define WeatherManager_hh_INCLUDED

// C++ Headers
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {
class IOFiles;
struct EnergyPlusData;

namespace WeatherManager {

    // Following are Date Types read in from EPW file or IDF
    enum class DateType
    {
        InvalidDate = -1,
        MonthDay = 1,
        NthDayInMonth = 2,
        LastDayInMonth = 3
    };

    // Water mains temperatures calculation methods
    enum class WaterMainsTempCalcMethod
    {
        Schedule = 1,
        Correlation = 2,
        CorrelationFromWeatherFile = 3
    };

    enum class DesignDaySolarModel
    {
        ASHRAE_ClearSky = 0,     // Design Day solar model ASHRAE ClearSky (default)
        Zhang_Huang = 1,         // Design Day solar model Zhang Huang
        SolarModel_Schedule = 2, // Design Day solar model (beam and diffuse) from user entered schedule
        ASHRAE_Tau = 3,          // Design Day solar model ASHRAE tau (per 2009 HOF)
        ASHRAE_Tau2017 = 4       // Design Day solar model ASHRAE tau (per 2013 and 2017 HOF)
    };

    // Design Day Humidity Indicating Type
    enum class DDHumIndType
    {
        WetBulb = 0,   // Wetbulb (default)
        DewPoint = 1,  // Dewpoint
        Enthalpy = 2,  // Enthalpy
        HumRatio = 3,  // Humidity Ratio
        RelHumSch = 4, // relhum schedule
        WBProfDef = 5, // Wetbulb default profile
        WBProfDif = 6, // Wetbulb difference profile
        WBProfMul = 7  // Wetbulb multiplier profile
    };

    // Design Day DryBulb Range Type
    enum class DDDBRangeType
    {
        Default = 0,    // Default Multipliers
        Multiplier = 1, // Multiplier Schedule
        Difference = 2, // Difference Schedule
        Profile = 3,    // Temperature Profile
    };

    enum class EmissivityCalcType
    {
        ClarkAllenModel = 0,    // Use Clark & Allen model for sky emissivity calculation
        ScheduleValue = 1,      // User entered Schedule value for Weather Property
        DryBulbDelta = 2,       // User entered DryBulb difference Schedule value for Weather Property
        DewPointDelta = 3,      // User entered Dewpoint difference Schedule value for Weather Property
        BruntModel = 4,         // Use Brunt model for sky emissivity calculation
        IdsoModel = 5,          // Use Isdo model for sky emissivity calculation
        BerdahlMartinModel = 6, // Use Martin & Berdahl model for sky emissivity calculation
        SkyTAlgorithmA = 7,     // place holder
    };

    extern int const NumDaysInYear;
    extern bool WeatherFileExists; // Set to true if a weather file exists
    extern bool LocationGathered;  // flag to show if Location exists on Input File (we assume one is there and correct on weather file)

    extern Real64 WeatherFileLatitude;
    extern Real64 WeatherFileLongitude;
    extern Real64 WeatherFileTimeZone;
    extern Real64 WeatherFileElevation;
    extern Array1D<Real64> GroundTempsFCFromEPWHeader;     // F or C factor method
    extern WaterMainsTempCalcMethod WaterMainsTempsMethod; // Water mains temperature calculation method
    extern Real64 WaterMainsTempsAnnualAvgAirTemp;         // Annual average outdoor air temperature (C)
    extern Real64 WaterMainsTempsMaxDiffAirTemp;           // Maximum difference in monthly average outdoor air temperatures (deltaC)
    extern bool wthFCGroundTemps;

    extern int TotRunPers;    // Total number of Run Periods (Weather data) to Setup
    extern int TotRunDesPers; // Total number of Run Design Periods (Weather data) to Setup

    extern int NumSpecialDays;
    extern Array1D_int SpecialDayTypes; // To hold holiday types given in input file
    extern Array1D_int DSTIndex;        // To hold DST Index based on weather file or input

    extern int NumIntervalsPerHour;

    extern bool WFAllowsLeapYears; // True if the Weather File (WF) header has "Yes" for Leap Years
    extern int Envrn;              // Counter for environments
    extern int NumOfEnvrn;         // Number of environments to be simulated

    extern Array2D_bool TodayIsRain;             // Rain indicator, true=rain
    extern Array2D_bool TodayIsSnow;             // Snow indicator, true=snow
    extern Array2D<Real64> TodayRainAmount;      // ficitious indicator of Rain
    extern Array2D<Real64> TodaySnowAmount;      // ficitious indicator of Snow
    extern Array2D<Real64> TodayOutDryBulbTemp;  // Dry bulb temperature of outside air
    extern Array2D<Real64> TodayOutWetBulbTemp;  // Wet bulb temperature of outside air
    extern Array2D<Real64> TodayOutDewPointTemp; // Dew Point Temperature of outside air
    extern Array2D<Real64> TodayOutBaroPress;    // Barometric pressure of outside air
    extern Array2D<Real64> TodayOutHumRat;       // Humidity ratio of outside air
    extern Array2D<Real64> TodayOutRelHum;       // Relative Humidity of outside air
    extern Array2D<Real64> TodayWindSpeed;       // Wind speed of outside air
    extern Array2D<Real64> TodayWindDir;         // Wind direction of outside air
    extern Array2D<Real64> TodaySkyTemp;         // Sky temperature
    extern Array2D<Real64> TodayHorizIRSky;      // Horizontal IR from Sky
    extern Array2D<Real64> TodayBeamSolarRad;    // Direct normal solar irradiance
    extern Array2D<Real64> TodayDifSolarRad;     // Sky diffuse horizontal solar irradiance
    extern Array2D<Real64> TodayAlbedo;          // Albedo
    extern Array2D<Real64> TodayLiquidPrecip;    // Liquid Precipitation Depth (mm)

    extern Array2D_bool TomorrowIsRain;             // Rain indicator, true=rain
    extern Array2D_bool TomorrowIsSnow;             // Snow indicator, true=snow
    extern Array2D<Real64> TomorrowRainAmount;      // ficitious indicator of Rain
    extern Array2D<Real64> TomorrowSnowAmount;      // ficitious indicator of Snow
    extern Array2D<Real64> TomorrowOutDryBulbTemp;  // Dry bulb temperature of outside air
    extern Array2D<Real64> TomorrowOutDewPointTemp; // Dew Point Temperature of outside air
    extern Array2D<Real64> TomorrowOutBaroPress;    // Barometric pressure of outside air
    extern Array2D<Real64> TomorrowOutRelHum;       // Relative Humidity of outside air
    extern Array2D<Real64> TomorrowWindSpeed;       // Wind speed of outside air
    extern Array2D<Real64> TomorrowWindDir;         // Wind direction of outside air
    extern Array2D<Real64> TomorrowSkyTemp;         // Sky temperature
    extern Array2D<Real64> TomorrowHorizIRSky;      // Horizontal IR from Sky
    extern Array2D<Real64> TomorrowBeamSolarRad;    // Direct normal solar irradiance
    extern Array2D<Real64> TomorrowDifSolarRad;     // Sky diffuse horizontal solar irradiance
    extern Array2D<Real64> TomorrowAlbedo;          // Albedo
    extern Array2D<Real64> TomorrowLiquidPrecip;    // Liquid Precipitation Depth

    extern Real64 TimeStepFraction; // Fraction of hour each time step represents
    extern Array1D_int EndDayOfMonth;
    extern int LeapYearAdd;           // Set during environment if leap year is active (adds 1 to number days in Feb)
    extern bool RPReadAllWeatherData; // True if need to read all weather data prior to simulation

    enum class WeekDay
    {
        Sunday = 1,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday
    };

    struct EnvironmentData
    {
        // Members
        std::string Title;          // Environment name
        std::string cKindOfEnvrn;   // kind of environment
        int KindOfEnvrn;            // Type of environment (see Parameters for KindOfSim in DataGlobals)
        int DesignDayNum;           // index in DesignDay structure and DesignDayInput
        int RunPeriodDesignNum;     // for WeatherFileDays, index in  RunPeriodDesign and RunPeriodDesignInput
        int SeedEnvrnNum;           // for HVAC sizing sim, new environments are copies of original environments, this is the index for original
        int HVACSizingIterationNum; // environments for HVAC sizing simulations are associated with iteration
        int TotalDays;              // Number of days in environment
        int StartJDay;              // Day of year of first day of environment
        int StartMonth;
        int StartDay;
        int StartYear;
        int StartDate;
        int EndMonth;
        int EndDay;
        int EndJDay;
        int EndYear;
        int EndDate;
        int DayOfWeek;         // Starting Day of Week for the (Weather) RunPeriod (User Input)
        bool UseDST;           // True if DaylightSavingTime is used for this RunPeriod
        bool UseHolidays;      // True if Holidays are used for this RunPeriod (from WeatherFile)
        bool ApplyWeekendRule; // True if "Weekend Rule" is to be applied to RunPeriod
        bool UseRain;          // True if Rain from weather file should be used (set rain to true)
        bool UseSnow;          // True if Snow from weather file should be used (set Snow to true)
        Array1D_int MonWeekDay;
        bool SetWeekDays;                // true when weekdays will be reset (after first year or on repeat)
        int NumSimYears;                 // Total Number of times this period to be performed
        int CurrentCycle;                // Current cycle through weather file in NumSimYears repeats
        int WP_Type1;                    // WeatherProperties SkyTemperature Pointer
        EmissivityCalcType SkyTempModel; // WeatherProperties SkyTemperature CalculationType
        bool UseWeatherFileHorizontalIR; // If false, horizontal IR and sky temperature are calculated with WP models
        int CurrentYear;                 // Current year
        bool IsLeapYear;                 // True if current year is leap year.
        bool RollDayTypeOnRepeat;        // If repeating run period, increment day type on repeat.
        bool TreatYearsAsConsecutive;    // When year rolls over, increment year and recalculate Leap Year
        bool MatchYear;                  // for actual weather will be true
        bool ActualWeather;              // true when using actual weather data
        int RawSimDays;                  // number of basic sim days.

        // Default Constructor
        EnvironmentData()
            : KindOfEnvrn(0), DesignDayNum(0), RunPeriodDesignNum(0), SeedEnvrnNum(0), HVACSizingIterationNum(0), TotalDays(0), StartJDay(0),
              StartMonth(0), StartDay(0), StartYear(0), StartDate(0), EndMonth(0), EndDay(0), EndJDay(0), EndYear(0), EndDate(0), DayOfWeek(0),
              UseDST(false), UseHolidays(false), ApplyWeekendRule(false), UseRain(true), UseSnow(true), MonWeekDay(12, 0), SetWeekDays(false),
              NumSimYears(1), CurrentCycle(0), WP_Type1(0), SkyTempModel(EmissivityCalcType::ClarkAllenModel), UseWeatherFileHorizontalIR(true),
              CurrentYear(0), IsLeapYear(false), RollDayTypeOnRepeat(true), TreatYearsAsConsecutive(true), MatchYear(false), ActualWeather(false),
              RawSimDays(0)
        {
        }
    };

    struct DesignDayData
    {
        // Members
        std::string Title;              // Environment name
        Real64 MaxDryBulb;              // Maximum Dry-Bulb Temperature (C)
        Real64 DailyDBRange;            // Daily Temperature Range (deltaC)
        Real64 HumIndValue;             // Humidity Indicating Value at Max Dry-bulb Temperature
        DDHumIndType HumIndType;        // Humidity Indicating type  (see Parameters)
        Real64 PressBarom;              // Atmospheric/Barometric Pressure (Pascals)
        Real64 WindSpeed;               // Wind Speed (m/s)
        Real64 WindDir;                 // Wind Direction (degrees clockwise from North, N=0, E=90, S=180, W=270)
        Real64 SkyClear;                // Sky Clearness (0 to 1)
        int RainInd;                    // Rain Indicator (1 = raining and surfaces are wet, else 0)
        int SnowInd;                    // Snow Indicator (1 = snow on ground, else  0)
        int DayOfMonth;                 // Day of Month ( 1 - 31 )
        int Month;                      // Month of Year ( 1 - 12 )
        int DayType;                    // Day Type Sunday = 1 - Saturday = 7
        int DSTIndicator;               // Daylight Saving Time Period Indicator (1=yes, 0=no) for this DesignDay
        DesignDaySolarModel SolarModel; // Solar Model for creating solar values for design day.
        DDDBRangeType DBTempRangeType;  // Drybulb Range Type (see Parameters)
        int TempRangeSchPtr;            // Schedule pointer to a day schedule for dry-bulb temperature range multipliers
        int HumIndSchPtr;               // Schedule pointer to a day schedule that specifies
        //    relative humidity (%) or wet-bulb range multipliers per HumIndType
        int BeamSolarSchPtr;      // Schedule pointer to a day schedule for beam solar
        int DiffuseSolarSchPtr;   // Schedule pointer to a day schedule for diffuse solar
        Real64 TauB;              // beam pseudo optical depth for ASHRAE tau model
        Real64 TauD;              // diffuse pseudo optical depth for ASHRAE tau model
        Real64 DailyWBRange;      // daily range of wetbulb (deltaC)
        bool PressureEntered;     // true if a pressure was entered in design day data
        bool DewPointNeedsSet;    // true if the Dewpoint humidicating value needs to be set (after location determined)
        int maxWarmupDays;        // Maximum warmup days between sizing periods
        bool suppressBegEnvReset; // true if this design day should be run without thermal history being reset at begin environment

        // Default Constructor
        DesignDayData()
            : MaxDryBulb(0.0), DailyDBRange(0.0), HumIndValue(0.0), HumIndType(DDHumIndType::WetBulb), PressBarom(0.0), WindSpeed(0.0), WindDir(0.0),
              SkyClear(0.0), RainInd(0), SnowInd(0), DayOfMonth(0), Month(0), DayType(0), DSTIndicator(0),
              SolarModel(DesignDaySolarModel::ASHRAE_ClearSky), DBTempRangeType(DDDBRangeType::Default), TempRangeSchPtr(0), HumIndSchPtr(0),
              BeamSolarSchPtr(0), DiffuseSolarSchPtr(0), TauB(0.0), TauD(0.0), DailyWBRange(0.0), PressureEntered(false),
              DewPointNeedsSet(false),                      //**Trane:BEG: Sizing Speed Up
              maxWarmupDays(-1), suppressBegEnvReset(false) //**Trane:END: Sizing Speed Up
        {
        }
    };

    struct RunPeriodData
    {
        // Members
        std::string title;
        std::string periodType;
        int totalDays; // total number of days in requested period
        int startMonth;
        int startDay;
        int startJulianDate; // Calculated start date (Julian or ordinal) for a weather file run period
        int startYear;       // entered in "consecutive"/real runperiod object
        int endMonth;
        int endDay;
        int endJulianDate;     // Calculated end date (Julian or ordinal) for a weather file run period
        int endYear;           // entered in "consecutive"/real runperiod object
        int dayOfWeek;         // Day of Week that the RunPeriod will start on (User Input)
        WeekDay startWeekDay;  // Day of the week that the RunPeriod will start on (User Input)
        bool useDST;           // True if DaylightSavingTime is used for this RunPeriod
        bool useHolidays;      // True if Holidays are used for this RunPeriod (from WeatherFile)
        bool applyWeekendRule; // True if "Weekend Rule" is to be applied to RunPeriod
        bool useRain;          // True if Rain from weather file should be used (set rain to true)
        bool useSnow;          // True if Snow from weather file should be used (set Snow to true)
        Array1D_int monWeekDay;
        int numSimYears;              // Total Number of years of simulation to be performed
        bool isLeapYear;              // True if Begin Year is leap year.
        bool RollDayTypeOnRepeat;     // If repeating run period, increment day type on repeat.
        bool TreatYearsAsConsecutive; // When year rolls over, increment year and recalculate Leap Year
        bool actualWeather;           // true when using actual weather data

        // Default Constructor
        RunPeriodData()
            : totalDays(365), startMonth(1), startDay(1), startJulianDate(2457755), startYear(2017), endMonth(12), endDay(31), endJulianDate(2458119),
              endYear(2017), dayOfWeek(1), startWeekDay(WeekDay::Sunday), useDST(false), useHolidays(false), applyWeekendRule(false), useRain(true),
              useSnow(true), monWeekDay{{1, 4, 4, 7, 2, 5, 7, 3, 6, 1, 4, 6}}, numSimYears(1), isLeapYear(false), RollDayTypeOnRepeat(true),
              TreatYearsAsConsecutive(true), actualWeather(false)
        {
        }
    };

    struct DayWeatherVariables // Derived Type for Storing Weather "Header" Data
    {
        // Members
        int DayOfYear;              // Day of year for weather data
        int DayOfYear_Schedule;     // Day of year in schedule
        int Year;                   // Year of weather data
        int Month;                  // Month of weather data
        int DayOfMonth;             // Day of month for weather data
        int DayOfWeek;              // Day of week for weather data
        int DaylightSavingIndex;    // Daylight Saving Time Period indicator (0=no,1=yes)
        int HolidayIndex;           // Holiday indicator (0=no holiday, non-zero=holiday type)
        Real64 SinSolarDeclinAngle; // Sine of the solar declination angle
        Real64 CosSolarDeclinAngle; // Cosine of the solar declination angle
        Real64 EquationOfTime;      // Value of the equation of time formula

        // Default Constructor
        DayWeatherVariables()
            : DayOfYear(0), DayOfYear_Schedule(0), Year(0), Month(0), DayOfMonth(0), DayOfWeek(0), DaylightSavingIndex(0), HolidayIndex(0),
              SinSolarDeclinAngle(0.0), CosSolarDeclinAngle(0.0), EquationOfTime(0.0)
        {
        }
    };

    struct SpecialDayData
    {
        // Members
        std::string Name;                  // Name
        WeatherManager::DateType DateType; // Date type as read in from IDF
        int Month;                         // Start Month
        int Day;                           // Start Day of month or Count for DateTypes=NthDayOfMonth
        int WeekDay;                       // For Date types=NthDayOfMonth and LastDayOfMonth
        int CompDate;                      // Start Date in "compressed date" format, only if Month/Day
        bool WthrFile;                     // True if this Special Day came from weather file (EPW)
        int Duration;                      // Number of days this special Day is used for
        int DayType;                       // Day Type desigation for this Special Day period
        int ActStMon;
        int ActStDay;
        bool Used; // Set to true in a run period after use (NthDayOfMonth and LastDayOfMonth only)

        // Default Constructor
        SpecialDayData()
            : DateType(DateType::InvalidDate), Month(0), Day(0), WeekDay(0), CompDate(0), WthrFile(false), Duration(0), DayType(0), ActStMon(0),
              ActStDay(0), Used(false)
        {
        }
    };

    struct DataPeriodData
    {
        // Members
        std::string Name;      // DataPeriod Title
        std::string DayOfWeek; // Start Day of Week for DataPeriod
        int NumYearsData;      // Number of years for which data is present in EPW.
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
        DataPeriodData()
            : NumYearsData(1), WeekDay(0), StMon(0), StDay(0), StYear(0), EnMon(0), EnDay(0), EnYear(0), NumDays(0), MonWeekDay(12, 0), DataStJDay(0),
              DataEnJDay(0), HasYearData(false)
        {
        }
    };

    struct DaylightSavingPeriodData
    {
        // Members
        DateType StDateType; // Start Date type as from EPW or IDF
        int StWeekDay;       // For DateTypes=NthDayOfMonth or LastDayOfMonth
        int StMon;           // DaylightSavingTime (DST) Start Month
        int StDay;           // DaylightSavingTime (DST) Start Day
        DateType EnDateType; // End Date type as from EPW or IDF
        int EnMon;           // DaylightSavingTime (DST) End Month
        int EnDay;           // DaylightSavingTime (DST) End Day
        int EnWeekDay;       // For DateTypes=NthDayOfMonth or LastDayOfMonth

        // Default Constructor
        DaylightSavingPeriodData()
            : StDateType(DateType::InvalidDate), StWeekDay(0), StMon(0), StDay(0), EnDateType(DateType::InvalidDate), EnMon(0), EnDay(0), EnWeekDay(0)
        {
        }
    };

    // This struct carries the default missing data for those data elements that would be best replaced with the previous hour's data for missing
    // data.
    struct MissingData
    {
        // Members
        Real64 DryBulb;      // Dry Bulb Temperature (C)
        Real64 DewPoint;     // Dew Point Temperature (C)
        int RelHumid;        // Relative Humidity (%)
        Real64 StnPres;      // Atmospheric Pressure (Pa)
        int WindDir;         // Wind Direction (deg)
        Real64 WindSpd;      // Wind Speed/Velocity (m/s)
        int TotSkyCvr;       // Total Sky Cover (tenths)
        int OpaqSkyCvr;      // Opaque Sky Cover (tenths)
        Real64 Visibility;   // Visibility (km)
        int Ceiling;         // Ceiling Height (m)
        int PrecipWater;     // Precipitable Water (mm)
        Real64 AerOptDepth;  // Aerosol Optical Depth
        int SnowDepth;       // Snow Depth (cm)
        int DaysLastSnow;    // Number of Days since last snow
        Real64 Albedo;       // Albedo
        Real64 LiquidPrecip; // Rain/Liquid Precipitation (mm)

        // Default Constructor
        MissingData()
            : DryBulb(0.0), DewPoint(0.0), RelHumid(0), StnPres(0.0), WindDir(0), WindSpd(0.0), TotSkyCvr(0), OpaqSkyCvr(0), Visibility(0.0),
              Ceiling(0), PrecipWater(0), AerOptDepth(0.0), SnowDepth(0), DaysLastSnow(0), Albedo(0.0), LiquidPrecip(0.0)
        {
        }
    };

    // This Derived type carries the counts of missing data items in the weather reading process. It will count only items that are on the source file
    // -- not those that are derived from data on the source file.
    struct MissingDataCounts
    {
        // Members
        // Comments below illustrate the data that is being counted:
        int DryBulb;      // Dry Bulb Temperature (C)
        int DewPoint;     // Dew Point Temperature (C)
        int RelHumid;     // Relative Humidity (%)
        int StnPres;      // Atmospheric Pressure (Pa)
        int WindDir;      // Wind Direction (deg)
        int WindSpd;      // Wind Speed/Velocity (m/s)
        int DirectRad;    // Direct Radiation (wh/m2)
        int DiffuseRad;   // Diffuse Radiation (wh/m2)
        int TotSkyCvr;    // Total Sky Cover (tenths)
        int OpaqSkyCvr;   // Opaque Sky Cover (tenths)
        int Visibility;   // Visibility (km)
        int Ceiling;      // Ceiling Height (m)
        int PrecipWater;  // Precipitable Water (mm)
        int AerOptDepth;  // Aerosol Optical Depth
        int SnowDepth;    // Snow Depth (cm)
        int DaysLastSnow; // Number of Days since last snow
        int WeathCodes;   // Weather codes invalid
        int Albedo;       // Albedo
        int LiquidPrecip; // Liquid Precip Depth

        // Default Constructor
        MissingDataCounts()
            : DryBulb(0), DewPoint(0), RelHumid(0), StnPres(0), WindDir(0), WindSpd(0), DirectRad(0), DiffuseRad(0), TotSkyCvr(0), OpaqSkyCvr(0),
              Visibility(0), Ceiling(0), PrecipWater(0), AerOptDepth(0), SnowDepth(0), DaysLastSnow(0), WeathCodes(0), Albedo(0), LiquidPrecip(0)
        {
        }
    };

    // This Derived type carries the counts of out of range items in the weather reading process. It will count only items that are on the source file
    // -- not those that are derived from data on the source file.
    struct RangeDataCounts
    {
        // Members
        // Comments below illustrate the data that is being counted:
        int DryBulb;    // Dry Bulb Temperature (C)
        int DewPoint;   // Dew Point Temperature (C)
        int RelHumid;   // Relative Humidity (%)
        int StnPres;    // Atmospheric Pressure (Pa)
        int WindDir;    // Wind Direction (deg)
        int WindSpd;    // Wind Speed/Velocity (m/s)
        int DirectRad;  // Direct Radiation (wh/m2)
        int DiffuseRad; // Diffuse Radiation (wh/m2)

        // Default Constructor
        RangeDataCounts() : DryBulb(0), DewPoint(0), RelHumid(0), StnPres(0), WindDir(0), WindSpd(0), DirectRad(0), DiffuseRad(0)
        {
        }
    };

    struct TypicalExtremeData
    {
        // Members
        std::string Title;       // Environment name
        std::string ShortTitle;  // Environment name
        std::string MatchValue;  // String to be matched for input/running these periods for design.
        std::string MatchValue1; // String to be also matched (synonym)
        std::string MatchValue2; // String to be also matched (synonym)
        std::string TEType;      // Typical or Extreme
        int TotalDays;           // Number of days in environment
        int StartJDay;           // Day of year of first day of environment
        int StartMonth;
        int StartDay;
        int EndMonth;
        int EndDay;
        int EndJDay;

        // Default Constructor
        TypicalExtremeData() : TotalDays(0), StartJDay(0), StartMonth(0), StartDay(0), EndMonth(0), EndDay(0), EndJDay(0)
        {
        }
    };

    struct WeatherProperties
    {
        // Members
        std::string Name;         // Reference Name
        std::string ScheduleName; // Schedule Name or Algorithm Name
        bool IsSchedule;          // Default is using Schedule
        EmissivityCalcType CalculationType;
        int SchedulePtr; // pointer to schedule when used
        bool UsedForEnvrn;
        bool UseWeatherFileHorizontalIR; // If false, horizontal IR and sky temperature are calculated with WP models

        // Default Constructor
        WeatherProperties()
            : IsSchedule(true), CalculationType(EmissivityCalcType::ClarkAllenModel), SchedulePtr(0), UsedForEnvrn(false),
              UseWeatherFileHorizontalIR(true)
        {
        }
    };

    struct UnderwaterBoundary
    {
        std::string Name;
        Real64 distanceFromLeadingEdge;
        int OSCMIndex;
        int WaterTempScheduleIndex;
        int VelocityScheduleIndex;
        UnderwaterBoundary() : Name(""), distanceFromLeadingEdge(0.0), OSCMIndex(0), WaterTempScheduleIndex(0), VelocityScheduleIndex(0)
        {
        }
    };
    extern std::vector<UnderwaterBoundary> underwaterBoundaries;

    // Object Data
    // Today's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year for
    // weather data | Year of weather data | Month of weather data | Day of month for weather data | Day of
    // week for weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday indicator (0=no
    // holiday, non-zero=holiday type) | Sine of the solar declination angle | Cosine of the solar
    // declination angle | Value of the equation of time formula
    extern DayWeatherVariables TodayVariables;
    // Tomorrow's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year
    // for weather data | Year of weather data | Month of weather data | Day of month for weather data |
    // Day of week for weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday
    // indicator (0=no holiday, non-zero=holiday type) | Sine of the solar declination angle | Cosine of
    // the solar declination angle | Value of the equation of time formula
    extern DayWeatherVariables TomorrowVariables;
    extern Array1D<DayWeatherVariables> DesignDay; // Design day environments
    // Dry Bulb Temperature (C) | Dew Point Temperature (C) | Relative Humidity (%) | Atmospheric Pressure (Pa) | Wind
    // Direction (deg) | Wind Speed/Velocity (m/s) | Total Sky Cover (tenths) | Opaque Sky Cover (tenths) | Visibility
    // (km) | Ceiling Height (m) | Precipitable Water (mm) | Aerosol Optical Depth | Snow Depth (cm) | Number of Days
    // since last snow | Albedo | Rain/Liquid Precipitation (mm)
    extern MissingData Missing;
    extern MissingDataCounts Missed;
    extern RangeDataCounts OutOfRange;
    extern Array1D<DesignDayData> DesDayInput;   // Design day Input Data
    extern Array1D<EnvironmentData> Environment; // Environment data
    extern Array1D<RunPeriodData> RunPeriodInput;
    extern Array1D<RunPeriodData> RunPeriodDesignInput;
    extern Array1D<TypicalExtremeData> TypicalExtremePeriods;
    extern DaylightSavingPeriodData EPWDST; // Daylight Saving Period Data from EPW file
    extern DaylightSavingPeriodData IDFDST; // Daylight Saving Period Data from IDF file
    extern DaylightSavingPeriodData DST;    // Daylight Saving Period Data, if active
    extern Array1D<WeatherProperties> WPSkyTemperature;
    extern Array1D<SpecialDayData> SpecialDays;
    extern Array1D<DataPeriodData> DataPeriods;

    // Functions
    void clear_state();

    void ManageWeather(EnergyPlusData& state);

    void ResetEnvironmentCounter();

    bool GetNextEnvironment(EnergyPlusData &state, bool &Available, bool &ErrorsFound);

    void
    AddDesignSetToEnvironmentStruct(int HVACSizingIterCount // Counter for number of times HVAC Sizing Simulation of Design Period set is being rerun
    );

    bool CheckIfAnyUnderwaterBoundaries();

    Real64 calculateWaterBoundaryConvectionCoefficient(Real64 curWaterTemp, Real64 curWaterVelocity, Real64 distanceFromLeadingEdge);

    void UpdateUnderwaterBoundaries();

    void ReadVariableLocationOrientation();

    void UpdateLocationAndOrientation();

    void SetupWeekDaysByMonth(int StMon, int StDay, int StWeekDay, Array1D_int &WeekDays);

    void ResetWeekDaysByMonth(Array1D_int &WeekDays,
                              int AddLeapYear,
                              int StartMonth,
                              int StartMonthDay,
                              int EndMonth,
                              int EndMonthDay,
                              bool Rollover,
                              bool MidSimReset = false);

    void SetDSTDateRanges(Array1D_int const &MonWeekDay, // Weekday of each day 1 of month
                          Array1D_int &DSTIdx,           // DST Index for each julian day (1:366)
                          Optional_int DSTActStMon = _,
                          Optional_int DSTActStDay = _,
                          Optional_int DSTActEnMon = _,
                          Optional_int DSTActEnDay = _);

    void SetSpecialDayDates(Array1D_int const &MonWeekDay); // Weekday of each day 1 of month

    void InitializeWeather(IOFiles &ioFiles, bool &printEnvrnStamp); // Set to true when the environment header should be printed

    void UpdateWeatherData();

    void SetCurrentWeather();

    void ReadWeatherForDay(IOFiles &ioFiles,
                           int DayToRead,          // =1 when starting out, otherwise signifies next day
                           int Environ,            // Environment being simulated
                           bool BackSpaceAfterRead // True if weather file is to be backspaced after read
    );

    void ReadEPlusWeatherForDay(IOFiles &ioFiles,
                                int DayToRead,          // =1 when starting out, otherwise signifies next day
                                int Environ,            // Environment being simulated
                                bool BackSpaceAfterRead // True if weather file is to be backspaced after read
    );

    Real64 interpolateWindDirection(Real64 prevHrWindDir, Real64 curHrWindDir, Real64 curHrWeight);

    void SetDayOfWeekInitialValues(int EnvironDayOfWeek, // Starting Day of Week for the (Weather) RunPeriod (User Input)
                                   int &currentDayOfWeek // Current Day of Week
    );

    void ErrorInterpretWeatherDataLine(int WYear, int WMonth, int WDay, int WHour, int WMinute, std::string &SaveLine, std::string &Line);

    void InterpretWeatherDataLine(std::string &Line,
                                  bool &ErrorFound, // True if an error is found, false otherwise
                                  int &WYear,
                                  int &WMonth,
                                  int &WDay,
                                  int &WHour,
                                  int &WMinute,
                                  Real64 &DryBulb,           // DryBulb
                                  Real64 &DewPoint,          // DewPoint
                                  Real64 &RelHum,            // RelHum
                                  Real64 &AtmPress,          // AtmPress
                                  Real64 &ETHoriz,           // ETHoriz
                                  Real64 &ETDirect,          // ETDirect
                                  Real64 &IRHoriz,           // IRHoriz
                                  Real64 &GLBHoriz,          // GLBHoriz
                                  Real64 &DirectRad,         // DirectRad
                                  Real64 &DiffuseRad,        // DiffuseRad
                                  Real64 &GLBHorizIllum,     // GLBHorizIllum
                                  Real64 &DirectNrmIllum,    // DirectNrmIllum
                                  Real64 &DiffuseHorizIllum, // DiffuseHorizIllum
                                  Real64 &ZenLum,            // ZenLum
                                  Real64 &WindDir,           // WindDir
                                  Real64 &WindSpeed,         // WindSpeed
                                  Real64 &TotalSkyCover,     // TotalSkyCover
                                  Real64 &OpaqueSkyCover,    // OpaqueSkyCover
                                  Real64 &Visibility,        // Visibility
                                  Real64 &CeilHeight,        // CeilHeight
                                  int &WObs,                 // PresWeathObs
                                  Array1D_int &WCodesArr,    // PresWeathConds
                                  Real64 &PrecipWater,       // PrecipWater
                                  Real64 &AerosolOptDepth,   // AerosolOptDepth
                                  Real64 &SnowDepth,         // SnowDepth
                                  Real64 &DaysSinceLastSnow, // DaysSinceLastSnow
                                  Real64 &Albedo,            // Albedo
                                  Real64 &LiquidPrecip       // LiquidPrecip
    );

    void SetUpDesignDay(IOFiles &ioFiles, int EnvrnNum); // Environment number passed into the routine

    Real64 AirMass(Real64 CosZen); // COS( solar zenith), 0 - 1

    // Calculate sky temperature from weather data
    Real64 CalcSkyEmissivity(EmissivityCalcType ESkyCalcType, Real64 OSky, Real64 DryBulb, Real64 DewPoint, Real64 RelHum);

    void ASHRAETauModel(DesignDaySolarModel TauModelType, // ASHRAETau solar model type ASHRAE_Tau or ASHRAE_Tau2017
                        Real64 ETR,                       // extraterrestrial normal irradiance, W/m2
                        Real64 CosZen,                    // COS( solar zenith angle), 0 - 1
                        Real64 TauB,                      // beam tau factor
                        Real64 TauD,                      // dif tau factor
                        Real64 &IDirN,                    // returned: direct (beam) irradiance on normal surface, W/m2
                        Real64 &IDifH,                    // returned: diffuse irradiance on horiz surface, W/m2
                        Real64 &IGlbH                     // returned: global irradiance on horiz surface, W/m2
    );

    void AllocateWeatherData();

    void CalculateDailySolarCoeffs(int DayOfYear,                 // Day of year (1 - 366)
                                   Real64 &A,                     // ASHRAE "A" - Apparent solar irradiation at air mass = 0 [W/M**2]
                                   Real64 &B,                     // ASHRAE "B" - Atmospheric extinction coefficient
                                   Real64 &C,                     // ASHRAE "C" - Diffuse radiation factor
                                   Real64 &AnnVarSolConstant,     // Annual variation in the solar constant
                                   Real64 &EquationOfTime,        // Equation of Time
                                   Real64 &SineSolarDeclination,  // Sine of Solar Declination
                                   Real64 &CosineSolarDeclination // Cosine of Solar Declination
    );

    void CalculateSunDirectionCosines(Real64 TimeValue,    // Current Time of Day
                                      Real64 EqOfTime,     // Equation of Time
                                      Real64 SinSolDeclin, // Sine of Solar Declination
                                      Real64 CosSolDeclin, // Cosine of Solar Declination
                                      Array1D<Real64> &SUNCOS);

    void DetermineSunUpDown(Array1D<Real64> &SunDirectionCosines);

    void OpenWeatherFile(EnergyPlusData &state, bool &ErrorsFound);

    void OpenEPlusWeatherFile(EnergyPlusData &state,
                              bool &ErrorsFound, // Will be set to true if errors found
                              bool ProcessHeader // Set to true when headers should be processed (rather than just read)
    );

    void CloseWeatherFile(IOFiles &ioFiles);

    void ResolveLocationInformation(IOFiles &ioFiles, bool &ErrorsFound); // Set to true if no location evident

    void CheckLocationValidity();

    void CheckWeatherFileValidity();

    void ReportOutputFileHeaders(IOFiles &ioFiles);

    void ReportWeatherAndTimeInformation(IOFiles &ioFiles,
                                         bool &printEnvrnStamp); // Set to true when the environment header should be printed

    void ReadUserWeatherInput(EnergyPlusData &state);

    void GetRunPeriodData(int &nRunPeriods, // Total number of Run Periods requested
                          bool &ErrorsFound);

    void GetRunPeriodDesignData(bool &ErrorsFound);

    void GetSpecialDayPeriodData(bool &ErrorsFound); // will be set to true if severe errors are found in inputs

    void CalcSpecialDayTypes();

    void GetDSTData(bool &ErrorsFound); // will be set to true if severe errors are found in inputs

    void GetDesignDayData(int &TotDesDays, // Total number of Design days to Setup
                          bool &ErrorsFound);

    void GetLocationInfo(bool &ErrorsFound);

    void GetWeatherProperties(bool &ErrorsFound);

    void GetGroundTemps(EnergyPlusData &state, bool &ErrorsFound);

    void GetGroundReflectances(IOFiles &ioFiles, bool &ErrorsFound);

    void GetSnowGroundRefModifiers(IOFiles &ioFiles, bool &ErrorsFound);

    void GetWaterMainsTemperatures(bool &ErrorsFound);

    void CalcWaterMainsTemp();

    Real64 WaterMainsTempFromCorrelation(Real64 AnnualOAAvgDryBulbTemp,        // annual average OA drybulb temperature
                                         Real64 MonthlyOAAvgDryBulbTempMaxDiff // monthly daily average OA drybulb temperature maximum difference
    );

    void GetWeatherStation(IOFiles &ioFiles, bool &ErrorsFound);

    void DayltgCurrentExtHorizIllum();

    void DayltgLuminousEfficacy(Real64 &DiffLumEff, // Luminous efficacy of sky diffuse solar radiation (lum/W)
                                Real64 &DirLumEff   // Luminous efficacy of beam solar radiation (lum/W)
    );

    Real64 GetSTM(Real64 Longitude); // Longitude from user input

    void ProcessEPWHeader(IOFiles &ioFiles, std::string const &HeaderString, std::string &Line, bool &ErrorsFound);

    void SkipEPlusWFHeader(IOFiles &ioFiles);

    void ReportMissing_RangeData();

    void SetupInterpolationValues();

    void SetupEnvironmentTypes();

    bool isLeapYear(int Year);

    struct GregorianDate
    {
        int year;
        int month;
        int day;

        GregorianDate(int year, int month, int day) : year(year), month(month), day(day)
        {
        }
    };

    int computeJulianDate(int gyyyy, int gmm, int gdd);

    int computeJulianDate(GregorianDate gdate);

    GregorianDate computeGregorianDate(int jdate);

    WeekDay calculateDayOfWeek(int year, int month, int day);

    int calculateDayOfYear(int Month, int Day, bool leapYear = false);

    bool validMonthDay(int month, int day, int leapYearAdd = 0);

    // derived type for processing and storing Dry-bulb weather or stat file
    struct AnnualMonthlyDryBulbWeatherData
    {
        // Members
        bool OADryBulbWeatherDataProcessed;             // if false stat or weather file OA Dry-bulb temp is not processed yet
        Real64 AnnualAvgOADryBulbTemp;                  // annual average outdoor air temperature (C)
        Real64 MonthlyAvgOADryBulbTempMaxDiff;          // monthly daily average OA drybulb temperature maximum difference (deltaC)
        Array1D<Real64> MonthlyDailyAverageDryBulbTemp; // monthly-daily average outdoor air temperatures (C)

        // Default Constructor
        AnnualMonthlyDryBulbWeatherData()
            : OADryBulbWeatherDataProcessed(false), AnnualAvgOADryBulbTemp(0.0), MonthlyAvgOADryBulbTempMaxDiff(0.0),
              MonthlyDailyAverageDryBulbTemp(12, 0.0)
        {
        }
        void CalcAnnualAndMonthlyDryBulbTemp(IOFiles &ioFiles); // true if this is CorrelationFromWeatherFile
    };

    extern AnnualMonthlyDryBulbWeatherData OADryBulbAverage;

    void ReportWaterMainsTempParameters(IOFiles &ioFiles);

} // namespace WeatherManager

} // namespace EnergyPlus

#endif
