// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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

#ifndef DataEnvironment_hh_INCLUDED
#define DataEnvironment_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DataEnvironment {

    Real64 constexpr EarthRadius(6356000.0);          // Radius of the Earth (m)
    Real64 constexpr AtmosphericTempGradient(0.0065); // Standard atmospheric air temperature gradient (K/m)
    Real64 constexpr SunIsUpValue(0.00001);           // if Cos Zenith Angle of the sun is >= this value, the sun is "up"
    Real64 constexpr StdPressureSeaLevel(101325.0);   // Standard barometric pressure at sea level (Pa)

    Real64 OutDryBulbTempAt(EnergyPlusData &state, Real64 Z); // Height above ground (m)

    Real64 OutWetBulbTempAt(EnergyPlusData &state, Real64 Z); // Height above ground (m)

    Real64 OutDewPointTempAt(EnergyPlusData &state, Real64 Z); // Height above ground (m)

    Real64 WindSpeedAt(EnergyPlusData &state, Real64 Z); // Height above ground (m)

    Real64 OutBaroPressAt(EnergyPlusData &state, Real64 Z); // Height above ground (m)

    void SetOutBulbTempAt_error(EnergyPlusData &state, std::string const &Settings, Real64 max_height, std::string const &SettingsName);

} // namespace DataEnvironment

struct EnvironmentData : BaseGlobalStruct {

    Real64 BeamSolarRad = 0.0;                          // Current beam normal solar irradiance
    bool EMSBeamSolarRadOverrideOn  = false;            // EMS flag for beam normal solar irradiance
    Real64 EMSBeamSolarRadOverrideValue = 0.0;          // EMS override value for beam normal solar irradiance
    int DayOfMonth = 0;                                 // Current day of the month
    int DayOfMonthTomorrow = 0;                         // Tomorrow's day of the month
    int DayOfWeek = 0;                                  // Current day of the week (Sunday=1, Monday=2, ...)
    int DayOfWeekTomorrow = 0;                          // Tomorrow's day of the week (Sunday=1, Monday=2, ...)
    int DayOfYear = 0;                                  // Current day of the year (01JAN=1, 02JAN=2, ...)
    int DayOfYear_Schedule = 0;                         // Schedule manager always assumes leap years...
    Real64 DifSolarRad = 0.0;                           // Current sky diffuse solar horizontal irradiance
    bool EMSDifSolarRadOverrideOn = false;              // EMS flag for sky diffuse solar horizontal irradiance
    Real64 EMSDifSolarRadOverrideValue = 0.0;           // EMS override value for sky diffuse solar horizontal irradiance
    int DSTIndicator = 0;                               // Daylight Saving Time Indicator (1=yes, 0=no) for Today
    Real64 Elevation = 0.0;                             // Elevation of this building site
    bool EndMonthFlag = false;                          // Set to true on last day of month
    bool EndYearFlag = false;                           // Set to true on the last day of year
    Real64 GndReflectanceForDayltg = 0.0;               // Ground visible reflectance for use in daylighting calc
    Real64 GndReflectance = 0.0;                        // Ground visible reflectance from input
    Real64 GndSolarRad = 0.0;                           // Current ground reflected radiation
    Real64 GroundTemp = 0.0;                            // Current ground temperature {C}
    Real64 GroundTempKelvin = 0.0;                      // Current ground temperature {K}
    Real64 GroundTempFC = 0.0;                          // Current ground temperature defined for F or C factor method {C}
    Real64 GroundTemp_Surface = 0.0;                    // Current surface ground temperature {C}
    Real64 GroundTemp_Deep = 0.0;                       // Current deep ground temperature
    int HolidayIndex = 0;                               // Indicates whether current day is a holiday and if so what type - HolidayIndex=(0-no holiday, 1-holiday type 1, ...)
    int HolidayIndexTomorrow = 0;                       // Tomorrow's Holiday Index
    bool IsRain = false;                                // Surfaces are wet for this time interval
    bool IsSnow = false;                                // Snow on the ground for this time interval
    Real64 Latitude = 0.0;                              // Latitude of building location
    Real64 Longitude = 0.0;                             // Longitude of building location
    int Month = 0;                                      // Current calendar month
    int MonthTomorrow = 0;                              // Tomorrow's calendar month
    Real64 OutBaroPress = 0.0;                          // Current outdoor air barometric pressure
    Real64 OutDryBulbTemp = 0.0;                        // Current outdoor air dry bulb temperature
    bool EMSOutDryBulbOverrideOn = false;               // EMS flag for outdoor air dry bulb temperature
    Real64 EMSOutDryBulbOverrideValue = 0.0;            // EMS override value for outdoor air dry bulb temperature
    Real64 OutHumRat = 0.0;                             // Current outdoor air humidity ratio
    Real64 OutRelHum = 0.0;                             // Current outdoor relative humidity [%]
    Real64 OutRelHumValue = 0.0;                        // Current outdoor relative humidity value [0.0-1.0]
    bool EMSOutRelHumOverrideOn = false;                // EMS flag for outdoor relative humidity value
    Real64 EMSOutRelHumOverrideValue = 0.0;             // EMS override value for outdoor relative humidity value
    Real64 OutEnthalpy = 0.0;                           // Current outdoor enthalpy
    Real64 OutAirDensity = 0.0;                         // Current outdoor air density
    Real64 OutWetBulbTemp = 0.0;                        // Current outdoor air wet bulb temperature
    Real64 OutDewPointTemp = 0.0;                       // Current outdoor dewpoint temperature
    bool EMSOutDewPointTempOverrideOn = false;          // EMS flag for outdoor dewpoint temperature
    Real64 EMSOutDewPointTempOverrideValue = 0.0;       // EMS override value for outdoor dewpoint temperature
    Real64 SkyTemp = 0.0;                               // Current sky temperature {C}
    Real64 SkyTempKelvin = 0.0;                         // Current sky temperature {K}
    Real64 LiquidPrecipitation = 0.0;                   // Current liquid precipitation amount (rain) {m}
    bool SunIsUp = false;                               // True when Sun is over horizon, False when not
    bool PreviousSolRadPositive = false;                // True when Sun is over horizon at the previous timestep, and BeamSolarRad + GndSolarRad + DifSolarRad > 0.0
    Real64 WindDir = 0.0;                               // Current outdoor air wind direction
    bool EMSWindDirOverrideOn = false;                  // EMS flag for outdoor air wind direction
    Real64 EMSWindDirOverrideValue = 0.0;               // EMS override value for outdoor air wind direction
    Real64 WindSpeed = 0.0;                             // Current outdoor air wind speed
    bool EMSWindSpeedOverrideOn = false;                // EMS flag for outdoor air wind speed
    Real64 EMSWindSpeedOverrideValue = false;           // EMS override value for outdoor air wind speed
    Real64 WaterMainsTemp = 0.0;                        // Current water mains temperature
    int Year = 0;                                       // Current calendar year of the simulation from the weather file
    int YearTomorrow = 0;                               // Tomorrow's calendar year of the simulation
    Array1D<Real64> SOLCOS = Array1D<Real64>(3);        // Solar direction cosines at current time step
    Real64 CloudFraction = 0.0;                         // Fraction of sky covered by clouds
    Real64 HISKF = 0.0;                                 // Exterior horizontal illuminance from sky (lux).
    Real64 HISUNF = 0.0;                                // Exterior horizontal beam illuminance (lux)
    Real64 HISUNFnorm = 0.0;                            // Exterior beam normal illuminance (lux)
    Real64 PDIRLW = 0.0;                                // Luminous efficacy (lum/W) of beam solar radiation
    Real64 PDIFLW = 0.0;                                // Luminous efficacy (lum/W) of sky diffuse solar radiation
    Real64 SkyClearness = 0.0;                          // Sky clearness (see subr. DayltgLuminousEfficacy)
    Real64 SkyBrightness = 0.0;                         // Sky brightness (see subr. DayltgLuminousEfficacy)
    Real64 TotalCloudCover = 5.0;                       // Total Sky Cover (tenth of sky)
    Real64 OpaqueCloudCover = 5.0;                      // Opaque Sky Cover (tenth of sky)
    Real64 StdBaroPress = DataEnvironment::StdPressureSeaLevel; // Standard "atmospheric pressure" based on elevation (ASHRAE HOF p6.1)
    Real64 StdRhoAir = 0.0;                             // Standard "rho air" set in WeatherManager - based on StdBaroPress
    Real64 rhoAirSTP = 0.0;                             // Standard density of dry air at 101325 Pa, 20.0C temperature
    Real64 TimeZoneNumber = 0.0;                        // Time Zone Number of building location
    Real64 TimeZoneMeridian = 0.0;                      // Standard Meridian of TimeZone
    std::string EnvironmentName;                        // Current environment name (longer for weather file names)
    std::string WeatherFileLocationTitle;               // Location Title from Weather File
    std::string CurMnDyHr;                              // Current Month/Day/Hour timestamp info
    std::string CurMnDy;                                // Current Month/Day timestamp info
    std::string CurMnDyYr;                              // Current Month/Day/Year timestamp info
    int CurEnvirNum = 0;                                // current environment number
    int TotDesDays = 0;                                 // Total number of Design days to Setup
    int TotRunDesPersDays = 0;                          // Total number of Run Design Periods [Days] (Weather data) to Setup
    int CurrentOverallSimDay = 0;                       // Count of current simulation day in total of all sim days
    int TotalOverallSimDays = 0;                        // Count of all possible simulation days in all environments
    int MaxNumberSimYears = 0;                          // Maximum number of simulation years requested in all RunPeriod statements
    int RunPeriodStartDayOfWeek = 0;                    // Day of week of the first day of the run period. (or design day - day of week)
    Real64 CosSolarDeclinAngle = 0.0;                   // Cosine of the solar declination angle
    Real64 EquationOfTime = 0.0;                        // Value of the equation of time formula
    Real64 SinLatitude = 0.0;                           // Sine of Latitude
    Real64 CosLatitude = 0.0;                           // Cosine of Latitude
    Real64 SinSolarDeclinAngle = 0.0;                   // Sine of the solar declination angle
    Real64 TS1TimeOffset = -0.5;                        // offset when TS=1 for solar calculations
    Real64 WeatherFileWindModCoeff = 1.5863;            // =(WindBLHeight/WindSensorHeight)**WindExp for conditions at the weather station
    Real64 WeatherFileTempModCoeff = 0.0;               // =AtmosphericTempGradient*EarthRadius*SensorHeight/(EarthRadius+SensorHeight)
    Real64 SiteWindExp = 0.22;                          // Exponent for the wind velocity profile at the site
    Real64 SiteWindBLHeight = 370.0;                    // Boundary layer height for the wind velocity profile at the site (m)
    Real64 SiteTempGradient = 0.0065;                   // Air temperature gradient coefficient (K/m)
    bool GroundTempObjInput = false;                    // Ground temperature object input
    bool GroundTemp_SurfaceObjInput = false;            // Surface ground temperature object input
    bool GroundTemp_DeepObjInput = false;               // Deep ground temperature object input
    bool FCGroundTemps = false;
    bool DisplayWeatherMissingDataWarnings = false;     // Display missing/out of range weather warnings
    bool IgnoreSolarRadiation = false;                  // TRUE if all solar radiation is to be ignored
    bool IgnoreBeamRadiation = false;                   // TRUE if beam (aka direct normal) radiation is to be ignored
    bool IgnoreDiffuseRadiation = false;                // TRUE if diffuse horizontal radiation is to be ignored
    bool PrintEnvrnStampWarmup = false;
    bool PrintEnvrnStampWarmupPrinted = false;
    bool RunPeriodEnvironment = false;                  // True if Run Period, False if DesignDay
    std::string EnvironmentStartEnd;                    // Start/End dates for Environment
    bool CurrentYearIsLeapYear = false;                 // true when current year is leap year (convoluted logic dealing with whether weather file allows leap years, runperiod inputs.
    int varyingLocationSchedIndexLat = 0;
    int varyingLocationSchedIndexLong = 0;
    int varyingOrientationSchedIndex = 0;
    bool forceBeginEnvResetSuppress = false;            // for PerformancePrecisionTradeoffs

    void clear_state() override
    {
        this->BeamSolarRad = 0.0;
        this->EMSBeamSolarRadOverrideOn = false;
        this->EMSBeamSolarRadOverrideValue = 0.0;
        this->DayOfMonth = 0;
        this->DayOfMonthTomorrow = 0;
        this->DayOfWeek = 0;
        this->DayOfWeekTomorrow = 0;
        this->DayOfYear = 0;
        this->DayOfYear_Schedule = 0;
        this->DifSolarRad = 0.0;
        this->EMSBeamSolarRadOverrideOn = false;
        this->EMSBeamSolarRadOverrideValue = 0.0;
        this->DSTIndicator = 0;
        this->Elevation = 0.0;
        this->EndMonthFlag = false;
        this->EndYearFlag = false;
        this->GndReflectanceForDayltg = 0.0;
        this->GndReflectance = 0.0;
        this->GndSolarRad = 0.0;
        this->GroundTemp = 0.0;
        this->GroundTempKelvin = 0.0;
        this->GroundTempFC = 0.0;
        this->GroundTemp_Surface = 0.0;
        this->GroundTemp_Deep = 0.0;
        this->HolidayIndex = 0;
        this->HolidayIndexTomorrow = 0;
        this->IsRain = false;
        this->IsSnow = false;
        this->Latitude = 0.0;
        this->Longitude = 0.0;
        this->Month = 0;
        this->MonthTomorrow = 0;
        this->OutBaroPress = 0.0;
        this->OutDryBulbTemp = 0.0;
        this->EMSOutDryBulbOverrideOn = false;
        this->EMSOutDryBulbOverrideValue = 0.0;
        this->OutHumRat = 0.0;
        this->OutRelHum = 0.0;
        this->OutRelHumValue = 0.0;
        this->EMSOutRelHumOverrideOn = false;
        this->EMSOutRelHumOverrideValue = 0.0;
        this->OutEnthalpy = 0.0;
        this->OutAirDensity = 0.0;
        this->OutWetBulbTemp = 0.0;
        this->OutDewPointTemp = 0.0;
        this->EMSOutDewPointTempOverrideOn = false;
        this->EMSOutDewPointTempOverrideValue = 0.0;
        this->SkyTemp = 0.0;
        this->SkyTempKelvin = 0.0;
        this->LiquidPrecipitation = 0.0;
        this->SunIsUp = false;
        this->WindDir = 0.0;
        this->EMSWindDirOverrideOn = false;
        this->EMSWindDirOverrideValue = 0.0;
        this->WindSpeed = 0.0;
        this->EMSWindSpeedOverrideOn = false;
        this->EMSWindSpeedOverrideValue = false;
        this->WaterMainsTemp = 0.0;
        this->Year = 0;
        this->YearTomorrow = 0;
        this->SOLCOS = Array1D<Real64>(3);
        this->CloudFraction = 0.0;
        this->HISKF = 0.0;
        this->HISUNF = 0.0;
        this->HISUNFnorm = 0.0;
        this->PDIRLW = 0.0;
        this->PDIFLW = 0.0;
        this->SkyClearness = 0.0;
        this->SkyBrightness = 0.0;
        this->TotalCloudCover = 5.0;
        this->OpaqueCloudCover = 5.0;
        this->StdBaroPress = DataEnvironment::StdPressureSeaLevel;
        this->StdRhoAir = 0.0;
        this->rhoAirSTP = 0.0;
        this->TimeZoneNumber = 0.0;
        this->TimeZoneMeridian = 0.0;
        this->EnvironmentName.clear();
        this->WeatherFileLocationTitle.clear();
        this->CurMnDyHr.clear();
        this->CurMnDy.clear();
        this->CurMnDyYr.clear();
        this->CurEnvirNum = 0;
        this->TotDesDays = 0;
        this->TotRunDesPersDays = 0;
        this->CurrentOverallSimDay = 0;
        this->TotalOverallSimDays = 0;
        this->MaxNumberSimYears = 0;
        this->RunPeriodStartDayOfWeek = 0;
        this->CosSolarDeclinAngle = 0.0;
        this->EquationOfTime = 0.0;
        this->SinLatitude = 0.0;
        this->CosLatitude = 0.0;
        this->SinSolarDeclinAngle = 0.0;
        this->TS1TimeOffset = -0.5;
        this->WeatherFileWindModCoeff = 1.5863;
        this->WeatherFileTempModCoeff = 0.0;
        this->SiteWindExp = 0.22;
        this->SiteWindBLHeight = 370.0;
        this->SiteTempGradient = 0.0065;
        this->GroundTempObjInput = false;
        this->GroundTemp_SurfaceObjInput = false;
        this->GroundTemp_DeepObjInput = false;
        this->FCGroundTemps = false;
        this->DisplayWeatherMissingDataWarnings = false;
        this->IgnoreSolarRadiation = false;
        this->IgnoreBeamRadiation = false;
        this->IgnoreDiffuseRadiation = false;
        this->PrintEnvrnStampWarmup = false;
        this->PrintEnvrnStampWarmupPrinted = false;
        this->RunPeriodEnvironment = false;
        this->EnvironmentStartEnd.clear();
        this->CurrentYearIsLeapYear = false;
        this->varyingLocationSchedIndexLat = 0;
        this->varyingLocationSchedIndexLong = 0;
        this->varyingOrientationSchedIndex = 0;
        this->forceBeginEnvResetSuppress = false;
    }
};

} // namespace EnergyPlus

#endif
