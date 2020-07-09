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
#include <unordered_map>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {
    // Forward declarations
    class BaseGroundTempsModel;
    struct EnergyPlusData;
    class OutputFiles;
    struct WeatherManagerData;

namespace WeatherManager {

    extern Array1D_string const DaysOfWeek;

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
        enum class Type
        {
            DesignDay,
            RunPeriodDesign,
            RunPeriodWeather,
            HVACSizeDesignDay,
            ksHVACSizeRunPeriodDesign,
            ReadAllWeatherData
        };
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
        bool SetWeekDays;             // true when weekdays will be reset (after first year or on repeat)
        int NumSimYears;              // Total Number of times this period to be performed
        int CurrentCycle;             // Current cycle through weather file in NumSimYears repeats
        int WP_Type1;                 // WeatherProperties SkyTemperature Pointer
        int SkyTempModel;       // WeatherProperties SkyTemperature CalculationType
        bool UseWeatherFileHorizontalIR; // If false, horizontal IR and sky temperature are calculated with WP models
        int CurrentYear;              // Current year
        bool IsLeapYear;              // True if current year is leap year.
        bool RollDayTypeOnRepeat;     // If repeating run period, increment day type on repeat.
        bool TreatYearsAsConsecutive; // When year rolls over, increment year and recalculate Leap Year
        bool MatchYear;               // for actual weather will be true
        bool ActualWeather;           // true when using actual weather data
        int RawSimDays;               // number of basic sim days.

        // Default Constructor
        EnvironmentData()
            : KindOfEnvrn(0), DesignDayNum(0), RunPeriodDesignNum(0), SeedEnvrnNum(0), HVACSizingIterationNum(0), TotalDays(0), StartJDay(0),
              StartMonth(0), StartDay(0), StartYear(0), StartDate(0), EndMonth(0), EndDay(0), EndJDay(0), EndYear(0), EndDate(0), DayOfWeek(0),
              UseDST(false), UseHolidays(false), ApplyWeekendRule(false), UseRain(true), UseSnow(true), MonWeekDay(12, 0), SetWeekDays(false),
              NumSimYears(1), CurrentCycle(0), WP_Type1(0), SkyTempModel(0), UseWeatherFileHorizontalIR(true), CurrentYear(0), IsLeapYear(false),
              RollDayTypeOnRepeat(true), TreatYearsAsConsecutive(true), MatchYear(false), ActualWeather(false), RawSimDays(0)
        {
        }
    };

    struct DesignDayData
    {
        // Members
        std::string Title;   // Environment name
        Real64 MaxDryBulb;   // Maximum Dry-Bulb Temperature (C)
        Real64 DailyDBRange; // Daily Temperature Range (deltaC)
        Real64 HumIndValue;  // Humidity Indicating Value at Max Dry-bulb Temperature
        int HumIndType;      // Humidity Indicating type  (see Parameters)
        Real64 PressBarom;   // Atmospheric/Barometric Pressure (Pascals)
        Real64 WindSpeed;    // Wind Speed (m/s)
        Real64 WindDir;      // Wind Direction (degrees clockwise from North, N=0, E=90, S=180, W=270)
        Real64 SkyClear;     // Sky Clearness (0 to 1)
        int RainInd;         // Rain Indicator (1 = raining and surfaces are wet, else 0)
        int SnowInd;         // Snow Indicator (1 = snow on ground, else  0)
        int DayOfMonth;      // Day of Month ( 1 - 31 )
        int Month;           // Month of Year ( 1 - 12 )
        int DayType;         // Day Type Sunday = 1 - Saturday = 7
        int DSTIndicator;    // Daylight Saving Time Period Indicator (1=yes, 0=no) for this DesignDay
        int SolarModel;      // Solar Model for creating solar values for design day.
        int DBTempRangeType; // Drybulb Range Type (see Parameters)
        int TempRangeSchPtr; // Schedule pointer to a day schedule for dry-bulb temperature range multipliers
        int HumIndSchPtr;    // Schedule pointer to a day schedule that specifies
        //    relative humidity (%) or wet-bulb range multipliers per HumIndType
        int BeamSolarSchPtr;    // Schedule pointer to a day schedule for beam solar
        int DiffuseSolarSchPtr; // Schedule pointer to a day schedule for diffuse solar
        Real64 TauB;            // beam pseudo optical depth for ASHRAE tau model
        Real64 TauD;            // diffuse pseudo optical depth for ASHRAE tau model
        Real64 DailyWBRange;    // daily range of wetbulb (deltaC)
        bool PressureEntered;   // true if a pressure was entered in design day data
        bool DewPointNeedsSet;  // true if the Dewpoint humidicating value needs to be set (after location determined)
        int maxWarmupDays;        // Maximum warmup days between sizing periods
        bool suppressBegEnvReset; // true if this design day should be run without thermal history being reset at begin environment

        // Default Constructor
        DesignDayData()
            : MaxDryBulb(0.0), DailyDBRange(0.0), HumIndValue(0.0), HumIndType(0), PressBarom(0.0), WindSpeed(0.0), WindDir(0.0), SkyClear(0.0),
              RainInd(0), SnowInd(0), DayOfMonth(0), Month(0), DayType(0), DSTIndicator(0), SolarModel(0), DBTempRangeType(0), TempRangeSchPtr(0),
              HumIndSchPtr(0), BeamSolarSchPtr(0), DiffuseSolarSchPtr(0), TauB(0.0), TauD(0.0), DailyWBRange(0.0), PressureEntered(false),
              DewPointNeedsSet(false),                       //**Trane:BEG: Sizing Speed Up
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
        std::string Name; // Name
        int DateType;     // Date type as read in from IDF
        int Month;        // Start Month
        int Day;          // Start Day of month or Count for DateTypes=NthDayOfMonth
        int WeekDay;      // For Date types=NthDayOfMonth and LastDayOfMonth
        int CompDate;     // Start Date in "compressed date" format, only if Month/Day
        bool WthrFile;    // True if this Special Day came from weather file (EPW)
        int Duration;     // Number of days this special Day is used for
        int DayType;      // Day Type desigation for this Special Day period
        int ActStMon;
        int ActStDay;
        bool Used; // Set to true in a run period after use (NthDayOfMonth and LastDayOfMonth only)

        // Default Constructor
        SpecialDayData()
            : DateType(0), Month(0), Day(0), WeekDay(0), CompDate(0), WthrFile(false), Duration(0), DayType(0), ActStMon(0), ActStDay(0), Used(false)
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
        int StDateType; // Start Date type as from EPW or IDF
        int StWeekDay;  // For DateTypes=NthDayOfMonth or LastDayOfMonth
        int StMon;      // DaylightSavingTime (DST) Start Month
        int StDay;      // DaylightSavingTime (DST) Start Day
        int EnDateType; // End Date type as from EPW or IDF
        int EnMon;      // DaylightSavingTime (DST) End Month
        int EnDay;      // DaylightSavingTime (DST) End Day
        int EnWeekDay;  // For DateTypes=NthDayOfMonth or LastDayOfMonth

        // Default Constructor
        DaylightSavingPeriodData() : StDateType(0), StWeekDay(0), StMon(0), StDay(0), EnDateType(0), EnMon(0), EnDay(0), EnWeekDay(0)
        {
        }
    };

    struct MissingData // This Derived type carries the default missing data
    {
        // Members
        // for those data elements that would be best replaced
        // with the previous hour's data for missing data.
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

    struct MissingDataCounts // This Derived type carries the counts of missing data
    {
        // Members
        // items in the weather reading process.  It will count
        // only items that are on the source file -- not those that
        // are derived from data on the source file.
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

    struct RangeDataCounts // This Derived type carries the counts of out of range
    {
        // Members
        // items in the weather reading process.  It will count
        // only items that are on the source file -- not those that
        // are derived from data on the source file.
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
        int CalculationType;
        int SchedulePtr; // pointer to schedule when used
        bool UsedForEnvrn;
        bool UseWeatherFileHorizontalIR; // If false, horizontal IR and sky temperature are calculated with WP models

        // Default Constructor
        WeatherProperties() : IsSchedule(true), CalculationType(0), SchedulePtr(0), UsedForEnvrn(false), UseWeatherFileHorizontalIR(true)
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

    // Functions

    void ManageWeather(WeatherManagerData &dataWeatherManager);

    void ResetEnvironmentCounter(WeatherManagerData &dataWeatherManager);

    bool GetNextEnvironment(EnergyPlusData &state, WeatherManagerData &dataWeatherManager, bool &Available, bool &ErrorsFound);

    void AddDesignSetToEnvironmentStruct(WeatherManagerData &dataWeatherManager,
        int const HVACSizingIterCount // Counter for number of times HVAC Sizing Simulation of Design Period set is being rerun
    );

    bool CheckIfAnyUnderwaterBoundaries(WeatherManagerData &dataWeatherManager);

    Real64
    calculateWaterBoundaryConvectionCoefficient(Real64 const curWaterTemp, Real64 const curWaterVelocity, Real64 const distanceFromLeadingEdge);

    void UpdateUnderwaterBoundaries(WeatherManagerData &dataWeatherManager);

    void ReadVariableLocationOrientation();

    void UpdateLocationAndOrientation();

    void SetupWeekDaysByMonth(WeatherManagerData &dataWeatherManager, int const StMon, int const StDay, int const StWeekDay, Array1D_int &WeekDays);

    void ResetWeekDaysByMonth(WeatherManagerData &dataWeatherManager, Array1D_int &WeekDays,
                              int const LeapYearAdd,
                              int const StartMonth,
                              int const StartMonthDay,
                              int const EndMonth,
                              int const EndMonthDay,
                              bool const Rollover,
                              Optional_bool_const MidSimReset = _);

    void SetDSTDateRanges(WeatherManagerData &dataWeatherManager, Array1D_int &MonWeekDay, // Weekday of each day 1 of month
                          Array1D_int &DSTIndex,   // DST Index for each julian day (1:366)
                          Optional_int DSTActStMon = _,
                          Optional_int DSTActStDay = _,
                          Optional_int DSTActEnMon = _,
                          Optional_int DSTActEnDay = _);

    void SetSpecialDayDates(WeatherManagerData &dataWeatherManager, Array1D_int &MonWeekDay); // Weekday of each day 1 of month

    void InitializeWeather(WeatherManagerData &dataWeatherManager, bool &PrintEnvrnStamp); // Set to true when the environment header should be printed

    void UpdateWeatherData(WeatherManagerData &dataWeatherManager);

    void SetCurrentWeather(WeatherManagerData &dataWeatherManager);

    void ReadWeatherForDay(WeatherManagerData &dataWeatherManager, int const DayToRead,          // =1 when starting out, otherwise signifies next day
                           int const Environ,            // Environment being simulated
                           bool const BackSpaceAfterRead // True if weather file is to be backspaced after read
    );

    void ReadEPlusWeatherForDay(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles,
                                int const DayToRead,          // =1 when starting out, otherwise signifies next day
                                int const Environ,            // Environment being simulated
                                bool const BackSpaceAfterRead // True if weather file is to be backspaced after read
    );

    Real64 interpolateWindDirection(Real64 const prevHrWindDir, Real64 const curHrWindDir, Real64 const curHrWeight);

    void SetDayOfWeekInitialValues(int const EnvironDayOfWeek, // Starting Day of Week for the (Weather) RunPeriod (User Input)
                                   int &CurDayOfWeek,          // Current Day of Week
                                   bool &UseDayOfWeek          // hmmm does not appear to be used anywhere.
    );

    void InterpretWeatherDataLine(WeatherManagerData &dataWeatherManager, std::string &Line,
                                  bool &ErrorFound,       // True if an error is found, false otherwise
                                  int &WYear,
                                  int &WMonth,
                                  int &WDay,
                                  int &WHour,
                                  int &WMinute,
                                  Real64 &RField1,        // DryBulb
                                  Real64 &RField2,        // DewPoint
                                  Real64 &RField3,        // RelHum
                                  Real64 &RField4,        // AtmPress
                                  Real64 &RField5,        // ETHoriz
                                  Real64 &RField6,        // ETDirect
                                  Real64 &RField7,        // IRHoriz
                                  Real64 &RField8,        // GLBHoriz
                                  Real64 &RField9,        // DirectRad
                                  Real64 &RField10,       // DiffuseRad
                                  Real64 &RField11,       // GLBHorizIllum
                                  Real64 &RField12,       // DirectNrmIllum
                                  Real64 &RField13,       // DiffuseHorizIllum
                                  Real64 &RField14,       // ZenLum
                                  Real64 &RField15,       // WindDir
                                  Real64 &RField16,       // WindSpeed
                                  Real64 &RField17,       // TotalSkyCover
                                  Real64 &RField18,       // OpaqueSkyCover
                                  Real64 &RField19,       // Visibility
                                  Real64 &RField20,       // CeilHeight
                                  int &WObs,              // PresWeathObs
                                  Array1D_int &WCodesArr, // PresWeathConds
                                  Real64 &RField22,       // PrecipWater
                                  Real64 &RField23,       // AerosolOptDepth
                                  Real64 &RField24,       // SnowDepth
                                  Real64 &RField25,       // DaysSinceLastSnow
                                  Real64 &RField26,       // Albedo
                                  Real64 &RField27        // LiquidPrecip
    );

    void SetUpDesignDay(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, int const EnvrnNum); // Environment number passed into the routine

    //------------------------------------------------------------------------------

    Real64 AirMass(Real64 const CosZen); // COS( solar zenith), 0 - 1

    //------------------------------------------------------------------------------

    Real64 CalcSkyEmissivity(WeatherManagerData &dataWeatherManager, int Envrn, Real64 OSky, Real64 DryBulb, Real64 DewPoint, Real64 RelHum); // Calculate sky temperature from weather data

    void ASHRAETauModel(WeatherManagerData &dataWeatherManager, int const TauModelType, // ASHRAETau solar model type ASHRAE_Tau or ASHRAE_Tau2017
                        Real64 const ETR,       // extraterrestrial normal irradiance, W/m2
                        Real64 const CosZen,    // COS( solar zenith angle), 0 - 1
                        Real64 const TauB,      // beam tau factor
                        Real64 const TauD,      // dif tau factor
                        Real64 &IDirN,          // returned: direct (beam) irradiance on normal surface, W/m2
                        Real64 &IDifH,          // returned: diffuse irradiance on horiz surface, W/m2
                        Real64 &IGlbH           // returned: global irradiance on horiz surface, W/m2
    );

    void AllocateWeatherData(WeatherManagerData &dataWeatherManager);

    void CalculateDailySolarCoeffs(int const DayOfYear,           // Day of year (1 - 366)
                                   Real64 &A,                     // ASHRAE "A" - Apparent solar irradiation at air mass = 0 [W/M**2]
                                   Real64 &B,                     // ASHRAE "B" - Atmospheric extinction coefficient
                                   Real64 &C,                     // ASHRAE "C" - Diffuse radiation factor
                                   Real64 &AnnVarSolConstant,     // Annual variation in the solar constant
                                   Real64 &EquationOfTime,        // Equation of Time
                                   Real64 &SineSolarDeclination,  // Sine of Solar Declination
                                   Real64 &CosineSolarDeclination // Cosine of Solar Declination
    );

    void CalculateSunDirectionCosines(Real64 const TimeValue,    // Current Time of Day
                                      Real64 const EqOfTime,     // Equation of Time
                                      Real64 const SinSolDeclin, // Sine of Solar Declination
                                      Real64 const CosSolDeclin, // Cosine of Solar Declination
                                      Array1D<Real64> &SUNCOS);

    void DetermineSunUpDown(WeatherManagerData &dataWeatherManager, Array1D<Real64> &SunDirectionCosines);

    void OpenWeatherFile(WeatherManagerData &dataWeatherManager, bool &ErrorsFound);

    void OpenEPlusWeatherFile(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles,
                              bool &ErrorsFound,       // Will be set to true if errors found
                              bool const ProcessHeader // Set to true when headers should be processed (rather than just read)
    );

    void CloseWeatherFile();

    void ResolveLocationInformation(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, bool &ErrorsFound); // Set to true if no location evident

    void CheckLocationValidity();

    void CheckWeatherFileValidity(WeatherManagerData &dataWeatherManager);

    void ReportOutputFileHeaders(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles);

    void ReportWeatherAndTimeInformation(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles,
                                         bool &PrintEnvrnStamp); // Set to true when the environment header should be printed

    void ReadUserWeatherInput(EnergyPlusData &state);

    void GetRunPeriodData(WeatherManagerData &dataWeatherManager, int &TotRunPers, // Total number of Run Periods requested
                          bool &ErrorsFound);

    void GetRunPeriodDesignData(WeatherManagerData &dataWeatherManager, bool &ErrorsFound);

    void GetSpecialDayPeriodData(WeatherManagerData &dataWeatherManager, bool &ErrorsFound); // will be set to true if severe errors are found in inputs

    void CalcSpecialDayTypes(WeatherManagerData &dataWeatherManager);

    void GetDSTData(WeatherManagerData &dataWeatherManager, bool &ErrorsFound); // will be set to true if severe errors are found in inputs

    void GetDesignDayData(WeatherManagerData &dataWeatherManager, int &TotDesDays, // Total number of Design days to Setup
                          bool &ErrorsFound);

    void GetLocationInfo(WeatherManagerData &dataWeatherManager, bool &ErrorsFound);

    void GetWeatherProperties(WeatherManagerData &dataWeatherManager, bool &ErrorsFound);

    void GetGroundTemps(EnergyPlusData &state, bool &ErrorsFound);

    void GetGroundReflectances(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, bool &ErrorsFound);

    void GetSnowGroundRefModifiers(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, bool &ErrorsFound);

    void GetWaterMainsTemperatures(WeatherManagerData &dataWeatherManager, bool &ErrorsFound);

    void CalcWaterMainsTemp(WeatherManagerData &dataWeatherManager);

    Real64
    WaterMainsTempFromCorrelation(Real64 const AnnualOAAvgDryBulbTemp,        // annual average OA drybulb temperature
                                  Real64 const MonthlyOAAvgDryBulbTempMaxDiff // monthly daily average OA drybulb temperature maximum difference
    );

    void GetWeatherStation(OutputFiles &outputFiles, bool &ErrorsFound);

    void DayltgCurrentExtHorizIllum();

    void DayltgLuminousEfficacy(Real64 &DiffLumEff, // Luminous efficacy of sky diffuse solar radiation (lum/W)
                                Real64 &DirLumEff   // Luminous efficacy of beam solar radiation (lum/W)
    );

    Real64 GetSTM(Real64 const Longitude); // Longitude from user input

    void ProcessEPWHeader(WeatherManagerData &dataWeatherManager, std::string const &HeaderString,
                          std::string &Line,
                          bool &ErrorsFound         // Set to true when errors were found, unchanged otherwise
    );

    void SkipEPlusWFHeader(WeatherManagerData &dataWeatherManager);

    void ReportMissing_RangeData(WeatherManagerData &dataWeatherManager);

    void SetupInterpolationValues(WeatherManagerData &dataWeatherManager);

    void SetupEnvironmentTypes(WeatherManagerData &dataWeatherManager);

    bool isLeapYear(int const Year);

    int computeJulianDate(int const gyyyy, int const gmm, int const gdd);

    void JGDate(int const jflag, // indicates direction of conversion,
                int &jdate,      // input/output julian date, typically a 7 or 8 digit integer
                int &gyyyy,      // input/output gregorian year, should be specified as 4 digits
                int &gmm,        // input/output gregorian month
                int &gdd         // input/output gregorian day
    );

    int CalculateDayOfWeek(WeatherManagerData &dataWeatherManager, int const JulianDate); // from JGDate calculation

    WeekDay calculateDayOfWeek(int const year, int const month, int day);

    int calculateDayOfYear(int const Month, int const Day);

    int calculateDayOfYear(int const Month, int const Day, bool const leapYear);

    bool validMonthDay(int const month, int const day, int const leapYearAdd = 0);

    struct AnnualMonthlyDryBulbWeatherData // derived type for processing and storing Dry-bulb weather or stat file
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
        void CalcAnnualAndMonthlyDryBulbTemp(WeatherManagerData &dataWeatherManager); // true if this is CorrelationFromWeatherFile
    };

    void ReportWaterMainsTempParameters(WeatherManagerData &dataWeatherManager);

} // namespace WeatherManager

    struct WeatherManagerData : BaseGlobalStruct {

        // Following are Date Types read in from EPW file or IDF
        int const InvalidDate;
        int const MonthDay;
        int const NthDayInMonth;
        int const LastDayInMonth;

        int const ScheduleMethod;                   // Constant for water mains temperatures calculation methods
        int const CorrelationMethod;                // Constant for water mains temperatures calculation methods
        int const CorrelationFromWeatherFileMethod; // Constant for water mains temperatures calculation methods

        int const InvalidWeatherFile;
        int const EPlusWeatherFile;

        int const ASHRAE_ClearSky;     // Design Day solar model ASHRAE ClearSky (default)
        int const Zhang_Huang;         // Design Day solar model Zhang Huang
        int const SolarModel_Schedule; // Design Day solar model (beam and diffuse) from user entered schedule
        int const ASHRAE_Tau;          // Design Day solar model ASHRAE tau (per 2009 HOF)
        int const ASHRAE_Tau2017;      // Design Day solar model ASHRAE tau (per 2013 and 2017 HOF)

        int const DDHumIndType_WetBulb;   // Design Day Humidity Indicating Type = Wetbulb (default)
        int const DDHumIndType_DewPoint;  // Design Day Humidity Indicating Type = Dewpoint
        int const DDHumIndType_Enthalpy;  // Design Day Humidity Indicating Type = Enthalpy
        int const DDHumIndType_HumRatio;  // Design Day Humidity Indicating Type = Humidity Ratio
        int const DDHumIndType_RelHumSch; // Design Day Humidity Indicating Type = relhum schedule
        int const DDHumIndType_WBProfDef; // Design Day Humidity Indicating Type = Wetbulb default profile
        int const DDHumIndType_WBProfDif; // Design Day Humidity Indicating Type = Wetbulb difference profile
        int const DDHumIndType_WBProfMul; // Design Day Humidity Indicating Type = Wetbulb multiplier profile
        int const DDHumIndType_Count;     // # of DDHumIndTypes

        int const DDDBRangeType_Default;    // Design Day DryBulb Range Type = Default Multipliers
        int const DDDBRangeType_Multiplier; // Design Day DryBulb Range Type = Multiplier Schedule
        int const DDDBRangeType_Difference; // Design Day DryBulb Range Type = Difference Schedule
        int const DDDBRangeType_Profile;    // Design Day DryBulb Range Type = Temperature Profile

        int const WP_ClarkAllenModel;     // Use Clark & Allen model for sky emissivity calculation
        int const WP_ScheduleValue;  // User entered Schedule value for Weather Property
        int const WP_DryBulbDelta;   // User entered DryBulb difference Schedule value for Weather Property
        int const WP_DewPointDelta;  // User entered Dewpoint difference Schedule value for Weather Property
        int const WP_BruntModel;          // Use Brunt model for sky emissivity calculation
        int const WP_IdsoModel;           // Use Isdo model for sky emissivity calculation
        int const WP_BerdahlMartinModel;  // Use Martin & Berdahl model for sky emissivity calculation
        int const WP_SkyTAlgorithmA;      // place holder

        int const GregorianToJulian; // JGDate argument for Gregorian to Julian Date conversion
        int const JulianToGregorian; // JGDate argument for Julian to Gregorian Date conversion

        Real64 const Sigma;    // Stefan-Boltzmann constant
        Real64 const TKelvin; // conversion from Kelvin to Celsius

        bool Debugout;

        int YearOfSim;
        int const NumDaysInYear;
        int EnvironmentReportNbr;         // Report number for the environment stamp
        std::string EnvironmentReportChr; // Report number for the environment stamp (character -- for printing)
        int TimeStampReportNbr;           // Report number for the time stamp
        std::string TimeStampReportChr;   // Report number for the time stamp (character -- for printing)
        int WeatherDataReport;            // Report number for the weather data
        bool WeatherFileExists;           // Set to true if a weather file exists
        std::string LocationTitle;        // Location Title from input File
        bool LocationGathered;            // flag to show if Location exists on Input File (we assume one is there and
                                                 // correct on weather file)

        Real64 WeatherFileLatitude;
        Real64 WeatherFileLongitude;
        Real64 WeatherFileTimeZone;
        Real64 WeatherFileElevation;
        int WeatherFileUnitNumber;                  // File unit number for the weather file
        Array1D<Real64> GroundTempsFCFromEPWHeader; // F or C factor method
        Array1D<Real64> GroundReflectances;         // User Specified Ground Reflectances
        Real64 SnowGndRefModifier;                  // Modifier to ground reflectance during snow
        Real64 SnowGndRefModifierForDayltg;         // Modifier to ground reflectance during snow for daylighting
        int WaterMainsTempsMethod;                  // Water mains temperature calculation method
        int WaterMainsTempsSchedule;                // Water mains temperature schedule
        Real64 WaterMainsTempsAnnualAvgAirTemp;     // Annual average outdoor air temperature (C)
        Real64 WaterMainsTempsMaxDiffAirTemp;       // Maximum difference in monthly average outdoor air temperatures (deltaC)
        std::string WaterMainsTempsScheduleName;    // water mains tempeature schedule name
        bool wthFCGroundTemps;
        Real64 RainAmount;
        Real64 SnowAmount;

        int TotRunPers;    // Total number of Run Periods (Weather data) to Setup
        int TotRunDesPers; // Total number of Run Design Periods (Weather data) to Setup

        int NumSpecialDays;
        Array1D_int SpecialDayTypes; // To hold holiday types given in input file
        Array1D_int WeekDayTypes;    // To hold Week day types using specified first day
        Array1D_int DSTIndex;        // To hold DST Index based on weather file or input

        int NumDataPeriods;

        int NumIntervalsPerHour;

        bool UseDaylightSaving;         // True if user says to use Weather File specified DaylightSaving Period
        bool UseSpecialDays;            // True if user says to use Weather File specified Special Days for current RunPeriod
        bool UseRainValues;             // True if rain values from weather file are to be used
        bool UseSnowValues;             // True if snow values from weather file are to be used
        bool EPWDaylightSaving;         // True if a DaylightSaving Time Period is input (EPW files)
        bool IDFDaylightSaving;         // True if a DaylightSaving Time Period is input (IDF files)
        bool DaylightSavingIsActive;    // True if a DaylightSavingPeriod should be used for Environment
        bool WFAllowsLeapYears;         // True if the Weather File (WF) header has "Yes" for Leap Years
        int WFLeapYearInd;              // Indicator for current Weather file "Leap Year", used in DayOfYear calculations and others.
        int curSimDayForEndOfRunPeriod; // normal=number days in sim, but different when repeating runperiods or multi-year files
        int Envrn;                      // Counter for environments
        int NumOfEnvrn;                 // Number of environments to be simulated
        int NumEPWTypExtSets;           // Number of Typical/Extreme on weather file.
        int NumWPSkyTemperatures;       // Number of WeatherProperty:SkyTemperature items in input file

                                        // These were static variables within different functions. They were pulled out into the namespace
                                        // to facilitate easier unit testing of those functions.
                                        // These are purposefully not in the header file as an extern variable. No one outside of this should
                                        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
                                        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool GetBranchInputOneTimeFlag;
        bool GetEnvironmentFirstCall;
        bool PrntEnvHeaders;

        Array2D_bool TodayIsRain;             // Rain indicator, true=rain
        Array2D_bool TodayIsSnow;             // Snow indicator, true=snow
        Array2D<Real64> TodayRainAmount;      // ficitious indicator of Rain
        Array2D<Real64> TodaySnowAmount;      // ficitious indicator of Snow
        Array2D<Real64> TodayOutDryBulbTemp;  // Dry bulb temperature of outside air
        Array2D<Real64> TodayOutWetBulbTemp;  // Wet bulb temperature of outside air
        Array2D<Real64> TodayOutDewPointTemp; // Dew Point Temperature of outside air
        Array2D<Real64> TodayOutBaroPress;    // Barometric pressure of outside air
        Array2D<Real64> TodayOutHumRat;       // Humidity ratio of outside air
        Array2D<Real64> TodayOutRelHum;       // Relative Humidity of outside air
        Array2D<Real64> TodayWindSpeed;       // Wind speed of outside air
        Array2D<Real64> TodayWindDir;         // Wind direction of outside air
        Array2D<Real64> TodaySkyTemp;         // Sky temperature
        Array2D<Real64> TodayHorizIRSky;      // Horizontal IR from Sky
        Array2D<Real64> TodayBeamSolarRad;    // Direct normal solar irradiance
        Array2D<Real64> TodayDifSolarRad;     // Sky diffuse horizontal solar irradiance
        Array2D<Real64> TodayAlbedo;          // Albedo
        Array2D<Real64> TodayLiquidPrecip;    // Liquid Precipitation Depth (mm)

        Array2D_bool TomorrowIsRain;             // Rain indicator, true=rain
        Array2D_bool TomorrowIsSnow;             // Snow indicator, true=snow
        Array2D<Real64> TomorrowRainAmount;      // ficitious indicator of Rain
        Array2D<Real64> TomorrowSnowAmount;      // ficitious indicator of Snow
        Array2D<Real64> TomorrowOutDryBulbTemp;  // Dry bulb temperature of outside air
        Array2D<Real64> TomorrowOutDewPointTemp; // Dew Point Temperature of outside air
        Array2D<Real64> TomorrowOutBaroPress;    // Barometric pressure of outside air
        Array2D<Real64> TomorrowOutRelHum;       // Relative Humidity of outside air
        Array2D<Real64> TomorrowWindSpeed;       // Wind speed of outside air
        Array2D<Real64> TomorrowWindDir;         // Wind direction of outside air
        Array2D<Real64> TomorrowSkyTemp;         // Sky temperature
        Array2D<Real64> TomorrowHorizIRSky;      // Horizontal IR from Sky
        Array2D<Real64> TomorrowBeamSolarRad;    // Direct normal solar irradiance
        Array2D<Real64> TomorrowDifSolarRad;     // Sky diffuse horizontal solar irradiance
        Array2D<Real64> TomorrowAlbedo;          // Albedo
        Array2D<Real64> TomorrowLiquidPrecip;    // Liquid Precipitation Depth

        Array3D<Real64> DDDBRngModifier;  // Design Day Dry-bulb Temperature Range Modifier
        Array3D<Real64> DDHumIndModifier; // Design Day relative humidity values
                                          //   or wet-bulb modifiers (per HumIndType)
        Array3D<Real64> DDBeamSolarValues;    // Design Day Beam Solar Values
        Array3D<Real64> DDDiffuseSolarValues; // Design Day Relative Humidity Values

        Array3D<Real64> DDSkyTempScheduleValues; // Sky temperature - DesignDay input

        int RptIsRain;  // Rain Report Value
        int RptIsSnow;  // Snow Report Value
        int RptDayType; // DayType Report Value

        Real64 HrAngle;                                  // Current Hour Angle
        Real64 SolarAltitudeAngle;                       // Angle of Solar Altitude (degrees)
        Real64 SolarAzimuthAngle;                        // Angle of Solar Azimuth (degrees)
        Real64 HorizIRSky;                               // Horizontal Infrared Radiation Intensity (W/m2)
        Real64 TimeStepFraction;                         // Fraction of hour each time step represents
        Array1D<Real64> SPSiteDryBulbRangeModScheduleValue;   // reporting Drybulb Temperature Range Modifier Schedule Value
        Array1D<Real64> SPSiteHumidityConditionScheduleValue; // reporting Humidity Condition Schedule Value
        Array1D<Real64> SPSiteBeamSolarScheduleValue;         // reporting Beam Solar Schedule Value
        Array1D<Real64> SPSiteDiffuseSolarScheduleValue;      // reporting Diffuse Solar Schedule Value
        Array1D<Real64> SPSiteSkyTemperatureScheduleValue;    // reporting SkyTemperature Modifier Schedule Value
        Array1D_int SPSiteScheduleNamePtr;                    // SP Site Schedule Name Ptrs
        Array1D_string SPSiteScheduleUnits;                   // SP Site Schedule Units
        int NumSPSiteScheduleNamePtrs;                     // Number of SP Site Schedules (DesignDay only)
        int NumMissing;                                    // Number of hours of missing data
        Array1D<Real64> Interpolation;                        // Interpolation values based on Number of Time Steps in Hour
        Array1D<Real64> SolarInterpolation;                   // Solar Interpolation values based on
                                                              //      Number of Time Steps in Hour
        Array1D_int EndDayOfMonth;
        bool ErrorInWeatherFile;           // Set to TRUE when there is a problem with dates
        int LeapYearAdd;                       // Set during environment if leap year is active (adds 1 to number days in Feb)
        bool DatesShouldBeReset;           // True when weekdays should be reset
        bool StartDatesCycleShouldBeReset; // True when start dates on repeat should be reset
        bool Jan1DatesShouldBeReset;       // True if Jan 1 should signal reset of dates
        bool RPReadAllWeatherData;         // True if need to read all weather data prior to simulation


        WeatherManager::DayWeatherVariables TodayVariables; // Today's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year for weather
                                            // data | Year of weather data | Month of weather data | Day of month for weather data | Day of week for
                                            // weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday indicator (0=no holiday,
                                            // non-zero=holiday type) | Sine of the solar declination angle | Cosine of the solar declination angle |
                                            // Value of the equation of time formula
        WeatherManager::DayWeatherVariables TomorrowVariables;  // Tomorrow's daily weather variables | Derived Type for Storing Weather "Header" Data | Day of year for
                                                // weather data | Year of weather data | Month of weather data | Day of month for weather data | Day of
                                                // week for weather data | Daylight Saving Time Period indicator (0=no,1=yes) | Holiday indicator (0=no
                                                // holiday, non-zero=holiday type) | Sine of the solar declination angle | Cosine of the solar declination
                                                // angle | Value of the equation of time formula
        Array1D<WeatherManager::DayWeatherVariables> DesignDay; // Design day environments
        WeatherManager::MissingData Missing; // Dry Bulb Temperature (C) | Dew Point Temperature (C) | Relative Humidity (%) | Atmospheric Pressure (Pa) | Wind Direction
                             // (deg) | Wind Speed/Velocity (m/s) | Total Sky Cover (tenths) | Opaque Sky Cover (tenths) | Visibility (km) | Ceiling
                             // Height (m) | Precipitable Water (mm) | Aerosol Optical Depth | Snow Depth (cm) | Number of Days since last snow | Albedo |
                             // Rain/Liquid Precipitation (mm)
        WeatherManager::MissingDataCounts Missed;
        WeatherManager::RangeDataCounts OutOfRange;
        Array1D<WeatherManager::DesignDayData> DesDayInput;   // Design day Input Data
        Array1D<WeatherManager::EnvironmentData> Environment; // Environment data
        Array1D<WeatherManager::RunPeriodData> RunPeriodInput;
        std::unordered_map<std::string, std::string> RunPeriodInputUniqueNames;
        Array1D<WeatherManager::RunPeriodData> RunPeriodDesignInput;
        std::unordered_map<std::string, std::string> RunPeriodDesignInputUniqueNames;
        Array1D<WeatherManager::TypicalExtremeData> TypicalExtremePeriods;
        WeatherManager::DaylightSavingPeriodData EPWDST; // Daylight Saving Period Data from EPW file
        WeatherManager::DaylightSavingPeriodData IDFDST; // Daylight Saving Period Data from IDF file
        WeatherManager::DaylightSavingPeriodData DST;    // Daylight Saving Period Data, if active
        Array1D<WeatherManager::WeatherProperties> WPSkyTemperature;
        Array1D<WeatherManager::SpecialDayData> SpecialDays;
        Array1D<WeatherManager::DataPeriodData> DataPeriods;

        std::vector<WeatherManager::UnderwaterBoundary> underwaterBoundaries;
        WeatherManager::AnnualMonthlyDryBulbWeatherData OADryBulbAverage; // processes outside air drybulb temperature

        std::shared_ptr<BaseGroundTempsModel> siteShallowGroundTempsPtr;
        std::shared_ptr<BaseGroundTempsModel> siteBuildingSurfaceGroundTempsPtr;
        std::shared_ptr<BaseGroundTempsModel> siteFCFactorMethodGroundTempsPtr;
        std::shared_ptr<BaseGroundTempsModel> siteDeepGroundTempsPtr;

        void clear_state() override
        {
            Debugout = false;
            YearOfSim = 1;             // The Present year of Simulation.
            EnvironmentReportNbr = 0;  // Report number for the environment stamp
            EnvironmentReportChr = ""; // Report number for the environment stamp (character -- for printing)
            TimeStampReportNbr = 0;    // Report number for the time stamp
            TimeStampReportChr = "";   // Report number for the time stamp (character -- for printing)
            WeatherDataReport = 0;     // Report number for the weather data
            WeatherFileExists = false; // Set to true if a weather file exists
            LocationTitle = "";        // Location Title from input File
            LocationGathered = false;  // flag to show if Location exists on Input File (we assume one is

            GetBranchInputOneTimeFlag = true;
            GetEnvironmentFirstCall = true;
            PrntEnvHeaders = true;

            WeatherFileLatitude = 0.0;
            WeatherFileLongitude = 0.0;
            WeatherFileTimeZone = 0.0;
            WeatherFileElevation = 0.0;
            WeatherFileUnitNumber = 0; // File unit number for the weather file
            siteShallowGroundTempsPtr.reset();
            siteBuildingSurfaceGroundTempsPtr.reset();
            siteFCFactorMethodGroundTempsPtr.reset();
            siteDeepGroundTempsPtr.reset();
            GroundTempsFCFromEPWHeader = Array1D<Real64>(12, 0.0);
            GroundReflectances = Array1D<Real64>(12, 0.2);

            SnowGndRefModifier = 1.0;              // Modifier to ground reflectance during snow
            SnowGndRefModifierForDayltg = 1.0;     // Modifier to ground reflectance during snow for daylighting
            WaterMainsTempsMethod = 0;             // Water mains temperature calculation method
            WaterMainsTempsSchedule = 0;           // Water mains temperature schedule
            WaterMainsTempsAnnualAvgAirTemp = 0.0; // Annual average outdoor air temperature (C)
            WaterMainsTempsMaxDiffAirTemp = 0.0;   // Maximum difference in monthly average outdoor air temperatures (deltaC)
            WaterMainsTempsScheduleName = "";      // water mains tempeature schedule name
            wthFCGroundTemps = false;
            RainAmount = 0.0;
            SnowAmount = 0.0;
            TotRunPers = 0;    // Total number of Run Periods (Weather data) to Setup
            TotRunDesPers = 0; // Total number of Run Design Periods (Weather data) to Setup
            NumSpecialDays = 0;

            SpecialDayTypes = Array1D<int>(366, 0);
            WeekDayTypes = Array1D<int>(366, 0);
            DSTIndex = Array1D<int>(366, 0);

            NumDataPeriods = 0;
            NumIntervalsPerHour = 1;
            UseDaylightSaving = true;             // True if user says to use Weather File specified DaylightSaving Period
            UseSpecialDays = true;                // True if user says to use Weather File specified Special Days for current RunPeriod
            UseRainValues = true;                 // True if rain values from weather file are to be used
            UseSnowValues = true;                 // True if snow values from weather file are to be used
            EPWDaylightSaving = false;            // True if a DaylightSaving Time Period is input (EPW files)
            IDFDaylightSaving = false;            // True if a DaylightSaving Time Period is input (IDF files)
            DaylightSavingIsActive = false;       // True if a DaylightSavingPeriod should be used for Environment
            WFAllowsLeapYears = false;            // True if the Weather File (WF) header has "Yes" for Leap Years
            WFLeapYearInd = 0;                    // Indicator for current Weather file "Leap Year", used in DayOfYear calculations and others.
            curSimDayForEndOfRunPeriod = 0;       // normal=number days in sim, but different when repeating runperiods or multi-year files
            Envrn = 0;                            // Counter for environments
            NumOfEnvrn = 0;                       // Number of environments to be simulated
            NumEPWTypExtSets = 0;                 // Number of Typical/Extreme on weather file.
            NumWPSkyTemperatures = 0;             // Number of WeatherProperty:SkyTemperature items in input file
            TodayIsRain.deallocate();             // Rain indicator, true=rain
            TodayIsSnow.deallocate();             // Snow indicator, true=snow
            TodayRainAmount.deallocate();         // ficitious indicator of Rain
            TodaySnowAmount.deallocate();         // ficitious indicator of Snow
            TodayOutDryBulbTemp.deallocate();     // Dry bulb temperature of outside air
            TodayOutWetBulbTemp.deallocate();     // Wet bulb temperature of outside air
            TodayOutDewPointTemp.deallocate();    // Dew Point Temperature of outside air
            TodayOutBaroPress.deallocate();       // Barometric pressure of outside air
            TodayOutHumRat.deallocate();          // Humidity ratio of outside air
            TodayOutRelHum.deallocate();          // Relative Humidity of outside air
            TodayWindSpeed.deallocate();          // Wind speed of outside air
            TodayWindDir.deallocate();            // Wind direction of outside air
            TodaySkyTemp.deallocate();            // Sky temperature
            TodayHorizIRSky.deallocate();         // Horizontal IR from Sky
            TodayBeamSolarRad.deallocate();       // Direct normal solar irradiance
            TodayDifSolarRad.deallocate();        // Sky diffuse horizontal solar irradiance
            TodayAlbedo.deallocate();             // Albedo
            TodayLiquidPrecip.deallocate();       // Liquid Precipitation Depth (mm)
            TomorrowIsRain.deallocate();          // Rain indicator, true=rain
            TomorrowIsSnow.deallocate();          // Snow indicator, true=snow
            TomorrowRainAmount.deallocate();      // ficitious indicator of Rain
            TomorrowSnowAmount.deallocate();      // ficitious indicator of Snow
            TomorrowOutDryBulbTemp.deallocate();  // Dry bulb temperature of outside air
            TomorrowOutDewPointTemp.deallocate(); // Dew Point Temperature of outside air
            TomorrowOutBaroPress.deallocate();    // Barometric pressure of outside air
            TomorrowOutRelHum.deallocate();       // Relative Humidity of outside air
            TomorrowWindSpeed.deallocate();       // Wind speed of outside air
            TomorrowWindDir.deallocate();         // Wind direction of outside air
            TomorrowSkyTemp.deallocate();         // Sky temperature
            TomorrowHorizIRSky.deallocate();      // Horizontal IR from Sky
            TomorrowBeamSolarRad.deallocate();    // Direct normal solar irradiance
            TomorrowDifSolarRad.deallocate();     // Sky diffuse horizontal solar irradiance
            TomorrowAlbedo.deallocate();          // Albedo
            TomorrowLiquidPrecip.deallocate();    // Liquid Precipitation Depth
            DDDBRngModifier.deallocate();         // Design Day Dry-bulb Temperature Range Modifier
            DDHumIndModifier.deallocate();        // Design Day relative humidity values
            DDBeamSolarValues.deallocate();       // Design Day Beam Solar Values
            DDDiffuseSolarValues.deallocate();    // Design Day Relative Humidity Values
            DDSkyTempScheduleValues.deallocate(); // Sky temperature - DesignDay input
            RptIsRain = 0;                        // Rain Report Value
            RptIsSnow = 0;                        // Snow Report Value
            RptDayType = 0;                       // DayType Report Value

            HrAngle = 0.0;                                     // Current Hour Angle
            SolarAltitudeAngle = 0.0;                          // Angle of Solar Altitude (degrees)
            SolarAzimuthAngle = 0.0;                           // Angle of Solar Azimuth (degrees)
            HorizIRSky = 0.0;                                  // Horizontal Infrared Radiation Intensity (W/m2)
            TimeStepFraction = 0.0;                            // Fraction of hour each time step represents
            SPSiteDryBulbRangeModScheduleValue.deallocate();   // reporting Drybulb Temperature Range Modifier Schedule Value
            SPSiteHumidityConditionScheduleValue.deallocate(); // reporting Humidity Condition Schedule Value
            SPSiteBeamSolarScheduleValue.deallocate();         // reporting Beam Solar Schedule Value
            SPSiteDiffuseSolarScheduleValue.deallocate();      // reporting Diffuse Solar Schedule Value
            SPSiteSkyTemperatureScheduleValue.deallocate();    // reporting SkyTemperature Modifier Schedule Value
            SPSiteScheduleNamePtr.deallocate();                // SP Site Schedule Name Ptrs
            SPSiteScheduleUnits.deallocate();                  // SP Site Schedule Units
            NumSPSiteScheduleNamePtrs = 0;                     // Number of SP Site Schedules (DesignDay only)
            NumMissing = 0;                                    // Number of hours of missing data
            Interpolation.deallocate();                        // Interpolation values based on Number of Time Steps in Hour
            SolarInterpolation.deallocate();                   // Solar Interpolation values based on

            ErrorInWeatherFile = false; // Set to TRUE when there is a problem with dates
            LeapYearAdd = 0;
            DatesShouldBeReset = false;
            StartDatesCycleShouldBeReset = false; // True when start dates on repeat should be reset
            Jan1DatesShouldBeReset = false;       // True if Jan 1 should signal reset of dates
            TodayVariables = WeatherManager::DayWeatherVariables();
            TomorrowVariables = WeatherManager::DayWeatherVariables();
            DesignDay.deallocate();
            Missing = WeatherManager::MissingData();
            Missed = WeatherManager::MissingDataCounts();
            OutOfRange = WeatherManager::RangeDataCounts();
            DesDayInput.deallocate(); // Design day Input Data
            Environment.deallocate(); // Environment data
            RunPeriodInput.deallocate();
            RunPeriodInputUniqueNames.clear();
            RunPeriodDesignInput.deallocate();
            RunPeriodDesignInputUniqueNames.clear();
            TypicalExtremePeriods.deallocate();

            EPWDST.StDateType = 0;
            EPWDST.StWeekDay = 0;
            EPWDST.StMon = 0;
            EPWDST.StDay = 0;
            EPWDST.EnDateType = 0;
            EPWDST.EnMon = 0;
            EPWDST.EnDay = 0;
            EPWDST.EnWeekDay = 0;

            IDFDST.StDateType = 0;
            IDFDST.StWeekDay = 0;
            IDFDST.StMon = 0;
            IDFDST.StDay = 0;
            IDFDST.EnDateType = 0;
            IDFDST.EnMon = 0;
            IDFDST.EnDay = 0;
            IDFDST.EnWeekDay = 0;

            DST.StDateType = 0;
            DST.StWeekDay = 0;
            DST.StMon = 0;
            DST.StDay = 0;
            DST.EnDateType = 0;
            DST.EnMon = 0;
            DST.EnDay = 0;
            DST.EnWeekDay = 0;
            WPSkyTemperature.deallocate();
            SpecialDays.deallocate();
            DataPeriods.deallocate();

            underwaterBoundaries.clear();
        }

        // Default Constructor
        WeatherManagerData()
            : InvalidDate(-1), MonthDay(1), NthDayInMonth(2), LastDayInMonth(3), ScheduleMethod(1), 
              CorrelationMethod(2), CorrelationFromWeatherFileMethod(3), InvalidWeatherFile(0), 
              EPlusWeatherFile(1), ASHRAE_ClearSky(0), Zhang_Huang(1), SolarModel_Schedule(2), 
              ASHRAE_Tau(3), ASHRAE_Tau2017(4), DDHumIndType_WetBulb(0), DDHumIndType_DewPoint(1), 
              DDHumIndType_Enthalpy(2), DDHumIndType_HumRatio(3), DDHumIndType_RelHumSch(4), 
              DDHumIndType_WBProfDef(5), DDHumIndType_WBProfDif(6), DDHumIndType_WBProfMul(7), 
              DDHumIndType_Count(8), DDDBRangeType_Default(0), DDDBRangeType_Multiplier(1), 
              DDDBRangeType_Difference(2), DDDBRangeType_Profile(3), WP_ClarkAllenModel(0),
              WP_ScheduleValue(1), WP_DryBulbDelta(2), WP_DewPointDelta(3), WP_BruntModel(4), 
              WP_IdsoModel(5), WP_BerdahlMartinModel(6), WP_SkyTAlgorithmA(7), GregorianToJulian(1), 
              JulianToGregorian(2), Sigma(5.6697e-8), TKelvin(DataGlobals::KelvinConv), Debugout(false),
              YearOfSim(1), NumDaysInYear(365), EnvironmentReportNbr(0), EnvironmentReportChr(""), 
              TimeStampReportNbr(0), TimeStampReportChr(""), WeatherDataReport(0), WeatherFileExists(false),
              LocationGathered(false), WeatherFileLatitude(0.0), WeatherFileLongitude(0.0), 
              WeatherFileTimeZone(0.0), WeatherFileElevation(0.0), WeatherFileUnitNumber(0), 
              GroundTempsFCFromEPWHeader(12, 0.0), GroundReflectances(12, 0.2), SnowGndRefModifier(1.0), 
              SnowGndRefModifierForDayltg(1.0), WaterMainsTempsMethod(0), WaterMainsTempsSchedule(0), 
              WaterMainsTempsAnnualAvgAirTemp(0.0), WaterMainsTempsMaxDiffAirTemp(0.0), WaterMainsTempsScheduleName(""), 
              wthFCGroundTemps(false), RainAmount(0.0), SnowAmount(0.0), TotRunPers(0), TotRunDesPers(0), 
              NumSpecialDays(0), SpecialDayTypes(366, 0), WeekDayTypes(366, 0), DSTIndex(366, 0), 
              NumDataPeriods(0), NumIntervalsPerHour(1), UseDaylightSaving(true), UseSpecialDays(true), 
              UseRainValues(true), UseSnowValues(true), EPWDaylightSaving(false), IDFDaylightSaving(false),
              DaylightSavingIsActive(false), WFAllowsLeapYears(false), WFLeapYearInd(0), 
              curSimDayForEndOfRunPeriod(0), Envrn(0), NumOfEnvrn(0), NumEPWTypExtSets(0), 
              NumWPSkyTemperatures(0), GetBranchInputOneTimeFlag(true), GetEnvironmentFirstCall(true), 
              PrntEnvHeaders(true), RptIsRain(0), RptIsSnow(0), RptDayType(0), HrAngle(0.0), 
              SolarAltitudeAngle(0.0), SolarAzimuthAngle(0.0), HorizIRSky(0.0), TimeStepFraction(0.0),
              NumSPSiteScheduleNamePtrs(0), NumMissing(0), 
              EndDayOfMonth(12, {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}), 
              ErrorInWeatherFile(false), LeapYearAdd(0), DatesShouldBeReset(false), 
              StartDatesCycleShouldBeReset(false), Jan1DatesShouldBeReset(false), RPReadAllWeatherData(false)
        {
        }
    };

} // namespace EnergyPlus

#endif
