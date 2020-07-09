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

// C++ Headers
#include <array>
#include <cmath>
#include <cstdio>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/time.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/GroundTemperatureModeling/BaseGroundTemperatureModel.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Vectors.hh>
#include <EnergyPlus/WeatherManager.hh>

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

    static std::string const BlankString;

    Array1D_string const DaysOfWeek(7, {"SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY"});
    std::map<std::string, WeekDay> weekDayLookUp{{"SUNDAY", WeekDay::Sunday},
                                                 {"MONDAY", WeekDay::Monday},
                                                 {"TUESDAY", WeekDay::Tuesday},
                                                 {"WEDNESDAY", WeekDay::Wednesday},
                                                 {"THURSDAY", WeekDay::Thursday},
                                                 {"FRIDAY", WeekDay::Friday},
                                                 {"SATURDAY", WeekDay::Saturday}};

    void ManageWeather(WeatherManagerData &dataWeatherManager)
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

        static bool PrintEnvrnStamp(false); // Set to true when the environment header should be printed

        InitializeWeather(dataWeatherManager, PrintEnvrnStamp);

        SetCurrentWeather(dataWeatherManager);

        ReportWeatherAndTimeInformation(dataWeatherManager, OutputFiles::getSingleton(), PrintEnvrnStamp);
    }

    void ResetEnvironmentCounter(WeatherManagerData &dataWeatherManager)
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

        dataWeatherManager.Envrn = 0;
    }

    bool CheckIfAnyUnderwaterBoundaries(WeatherManagerData &dataWeatherManager)
    {
        bool errorsFound = false;
        int NumAlpha = 0, NumNumber = 0, IOStat = 0;
        DataIPShortCuts::cCurrentModuleObject = "SurfaceProperty:Underwater";
        int Num = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);
        for (int i = 1; i <= Num; i++) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          i,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlpha,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNumber,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            dataWeatherManager.underwaterBoundaries.push_back(UnderwaterBoundary());
            dataWeatherManager.underwaterBoundaries[i - 1].Name = DataIPShortCuts::cAlphaArgs(1);
            dataWeatherManager.underwaterBoundaries[i - 1].distanceFromLeadingEdge = DataIPShortCuts::rNumericArgs(1);
            dataWeatherManager.underwaterBoundaries[i - 1].OSCMIndex = UtilityRoutines::FindItemInList(dataWeatherManager.underwaterBoundaries[i - 1].Name, DataSurfaces::OSCM);
            if (dataWeatherManager.underwaterBoundaries[i - 1].OSCMIndex <= 0) {
                ShowSevereError("Could not match underwater boundary condition object with an Other Side Conditions Model input object.");
                errorsFound = true;
            }
            dataWeatherManager.underwaterBoundaries[i - 1].WaterTempScheduleIndex = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
            if (dataWeatherManager.underwaterBoundaries[i - 1].WaterTempScheduleIndex == 0) {
                ShowSevereError("Water temperature schedule for \"SurfaceProperty:Underwater\" named \"" + dataWeatherManager.underwaterBoundaries[i - 1].Name +
                                "\" not found");
                errorsFound = true;
            }
            if (DataIPShortCuts::lAlphaFieldBlanks(3)) {
                // that's OK, we can have a blank schedule, the water will just have no free stream velocity
                dataWeatherManager.underwaterBoundaries[i - 1].VelocityScheduleIndex = 0;
            } else {
                dataWeatherManager.underwaterBoundaries[i - 1].VelocityScheduleIndex = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(3));
                if (dataWeatherManager.underwaterBoundaries[i - 1].WaterTempScheduleIndex == 0) {
                    ShowSevereError("Free streawm velocity schedule for \"SurfaceProperty:Underwater\" named \"" + dataWeatherManager.underwaterBoundaries[i - 1].Name +
                                    "\" not found");
                    errorsFound = true;
                }
            }
            if (errorsFound) break;
        }
        if (errorsFound) {
            ShowFatalError("Previous input problems cause program termination");
        }
        return (Num > 0);
    }

    Real64
    calculateWaterBoundaryConvectionCoefficient(Real64 const curWaterTemp, Real64 const freeStreamVelocity, Real64 const distanceFromLeadingEdge)
    {
        Real64 const waterKinematicViscosity = 1e-6; // m2/s
        Real64 const waterPrandtlNumber = 6;         // -
        Real64 const waterThermalConductivity = 0.6; // W/mK
        // do some calculation for forced convection from the leading edge of the ship
        Real64 const localReynoldsNumber = freeStreamVelocity * distanceFromLeadingEdge / waterKinematicViscosity;
        Real64 const localNusseltNumber = 0.0296 * pow(localReynoldsNumber, 0.8) * pow(waterPrandtlNumber, 1.0 / 3.0);
        Real64 const localConvectionCoeff = localNusseltNumber * waterThermalConductivity / distanceFromLeadingEdge;

        // do some calculations for natural convection from the bottom of the ship
        Real64 const distanceFromBottomOfHull = 12; // meters, assumed for now
                                                    // this Prandtl correction is from Incropera & Dewitt, Intro to HT, eq 9.20
        Real64 const prandtlCorrection =
            (0.75 * pow(waterPrandtlNumber, 0.5)) / pow(0.609 + 1.221 * pow(waterPrandtlNumber, 0.5) + 1.238 * waterPrandtlNumber, 0.25);
        // calculate the Grashof number
        Real64 const gravity = 9.81;          // m/s2
        Real64 const beta = 0.000214;         // water thermal expansion coefficient, from engineeringtoolbox.com, 1/C
        Real64 const assumedSurfaceTemp = 25; // Grashof requires a surface temp, this should suffice
        Real64 const localGrashofNumber =
            (gravity * beta * (assumedSurfaceTemp - curWaterTemp) * pow(distanceFromBottomOfHull, 3)) / pow(waterKinematicViscosity, 2);
        Real64 const localNusseltFreeConvection = pow(localGrashofNumber / 4, 0.25) * prandtlCorrection;
        Real64 const localConvectionCoeffFreeConv = localNusseltFreeConvection * waterThermalConductivity / distanceFromBottomOfHull;
        return max(localConvectionCoeff, localConvectionCoeffFreeConv);
    }

    void UpdateUnderwaterBoundaries(WeatherManagerData &dataWeatherManager)
    {
        for (auto &thisBoundary : dataWeatherManager.underwaterBoundaries) {
            Real64 const curWaterTemp = ScheduleManager::GetCurrentScheduleValue(thisBoundary.WaterTempScheduleIndex); // C
            Real64 freeStreamVelocity = 0;
            if (thisBoundary.VelocityScheduleIndex > 0) {
                freeStreamVelocity = ScheduleManager::GetCurrentScheduleValue(thisBoundary.VelocityScheduleIndex); // m/s
            }
            DataSurfaces::OSCM(thisBoundary.OSCMIndex).TConv = curWaterTemp;
            DataSurfaces::OSCM(thisBoundary.OSCMIndex).HConv =
                WeatherManager::calculateWaterBoundaryConvectionCoefficient(curWaterTemp, freeStreamVelocity, thisBoundary.distanceFromLeadingEdge);
            DataSurfaces::OSCM(thisBoundary.OSCMIndex).TRad = curWaterTemp;
            DataSurfaces::OSCM(thisBoundary.OSCMIndex).HRad = 0.0;
        }
    }

    void ReadVariableLocationOrientation()
    {
        int NumAlpha = 0, NumNumber = 0, IOStat = 0;
        DataIPShortCuts::cCurrentModuleObject = "Site:VariableLocation";
        if (inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject) == 0) return;
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                      1,
                                      DataIPShortCuts::cAlphaArgs,
                                      NumAlpha,
                                      DataIPShortCuts::rNumericArgs,
                                      NumNumber,
                                      IOStat,
                                      DataIPShortCuts::lNumericFieldBlanks,
                                      DataIPShortCuts::lAlphaFieldBlanks,
                                      DataIPShortCuts::cAlphaFieldNames,
                                      DataIPShortCuts::cNumericFieldNames);
        DataEnvironment::varyingLocationSchedIndexLat = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(1));
        DataEnvironment::varyingLocationSchedIndexLong = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(2));
        DataEnvironment::varyingOrientationSchedIndex = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(3));
    }

    void UpdateLocationAndOrientation()
    {
        if (DataEnvironment::varyingLocationSchedIndexLat > 0) {
            DataEnvironment::Latitude = ScheduleManager::GetCurrentScheduleValue(DataEnvironment::varyingLocationSchedIndexLat);
        }
        if (DataEnvironment::varyingLocationSchedIndexLong > 0) {
            DataEnvironment::Longitude = ScheduleManager::GetCurrentScheduleValue(DataEnvironment::varyingLocationSchedIndexLong);
        }
        CheckLocationValidity();
        if (DataEnvironment::varyingOrientationSchedIndex > 0) {
            DataHeatBalance::BuildingAzimuth = mod(ScheduleManager::GetCurrentScheduleValue(DataEnvironment::varyingOrientationSchedIndex), 360.0);
            SurfaceGeometry::CosBldgRelNorth =
                std::cos(-(DataHeatBalance::BuildingAzimuth + DataHeatBalance::BuildingRotationAppendixG) * DataGlobals::DegToRadians);
            SurfaceGeometry::SinBldgRelNorth =
                std::sin(-(DataHeatBalance::BuildingAzimuth + DataHeatBalance::BuildingRotationAppendixG) * DataGlobals::DegToRadians);
            for (size_t SurfNum = 1; SurfNum < DataSurfaces::Surface.size(); ++SurfNum) {
                for (int n = 1; n <= DataSurfaces::Surface(SurfNum).Sides; ++n) {
                    Real64 Xb = DataSurfaces::Surface(SurfNum).Vertex(n).x;
                    Real64 Yb = DataSurfaces::Surface(SurfNum).Vertex(n).y;
                    DataSurfaces::Surface(SurfNum).NewVertex(n).x = Xb * SurfaceGeometry::CosBldgRelNorth - Yb * SurfaceGeometry::SinBldgRelNorth;
                    DataSurfaces::Surface(SurfNum).NewVertex(n).y = Xb * SurfaceGeometry::SinBldgRelNorth + Yb * SurfaceGeometry::CosBldgRelNorth;
                    DataSurfaces::Surface(SurfNum).NewVertex(n).z = DataSurfaces::Surface(SurfNum).Vertex(n).z;
                }
                Vectors::CreateNewellSurfaceNormalVector(DataSurfaces::Surface(SurfNum).NewVertex,
                                                         DataSurfaces::Surface(SurfNum).Sides,
                                                         DataSurfaces::Surface(SurfNum).NewellSurfaceNormalVector);
                Real64 SurfWorldAz = 0.0;
                Real64 SurfTilt = 0.0;
                Vectors::DetermineAzimuthAndTilt(DataSurfaces::Surface(SurfNum).NewVertex,
                                                 DataSurfaces::Surface(SurfNum).Sides,
                                                 SurfWorldAz,
                                                 SurfTilt,
                                                 DataSurfaces::Surface(SurfNum).lcsx,
                                                 DataSurfaces::Surface(SurfNum).lcsy,
                                                 DataSurfaces::Surface(SurfNum).lcsz,
                                                 DataSurfaces::Surface(SurfNum).GrossArea,
                                                 DataSurfaces::Surface(SurfNum).NewellSurfaceNormalVector);
                DataSurfaces::Surface(SurfNum).Azimuth = SurfWorldAz;
                DataSurfaces::Surface(SurfNum).SinAzim = std::sin(SurfWorldAz * DegToRadians);
                DataSurfaces::Surface(SurfNum).CosAzim = std::cos(SurfWorldAz * DegToRadians);
                DataSurfaces::Surface(SurfNum).OutNormVec = DataSurfaces::Surface(SurfNum).NewellSurfaceNormalVector;
            }
        }
    }

    bool GetNextEnvironment(EnergyPlusData &state, WeatherManagerData &dataWeatherManager, bool &Available, bool &ErrorsFound)
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

        // Using/Aliasing
        using General::BetweenDates;
        using namespace DataSystemVariables;
        using DataHeatBalance::AdaptiveComfortRequested_ASH55;
        using DataHeatBalance::AdaptiveComfortRequested_CEN15251;
        using ThermalComfort::CalcThermalComfortAdaptiveASH55;
        using ThermalComfort::CalcThermalComfortAdaptiveCEN15251;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetNextEnvironment: ");
        static constexpr auto EnvNameFormat("Environment,{},{},{},{},{},{},{},{},{},{},{},{},{}\n");
        static constexpr auto EnvDSTNFormat("Environment:Daylight Saving,No,{}\n");
        static constexpr auto EnvDSTYFormat("Environment:Daylight Saving,Yes,{},{},{}\n");
        static constexpr auto DateFormat("{:02}/{:02}");
        static constexpr auto DateFormatWithYear("{:02}/{:02}/{:04}");
        static Array1D_string const SpecialDayNames(5, {"Holiday", "SummerDesignDay", "WinterDesignDay", "CustomDay1", "CustomDay2"});
        static Array1D_string const ValidDayNames(12,
                                                  {"Sunday",
                                                   "Monday",
                                                   "Tuesday",
                                                   "Wednesday",
                                                   "Thursday",
                                                   "Friday",
                                                   "Saturday",
                                                   "Holiday",
                                                   "SummerDesignDay",
                                                   "WinterDesignDay",
                                                   "CustomDay1",
                                                   "CustomDay2"});
        static Array1D_string const SkyTempModelNames(7,
                                                      {"Clark and Allen",
                                                       "Schedule Value",
                                                       "DryBulb Difference Schedule Value",
                                                       "Dewpoint Difference Schedule Value",
                                                       "Brunt",
                                                       "Idso",
                                                       "Berdahl and Martin"});
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //////////// hoisted into namespace changed to GetBranchInputOneTimeFlag////////////
        //	static bool GetInputFlag( true ); // Set to true before execution starts changed to GetEnvironmentInputOneTimeFlag
        //	static bool FirstCall( true ); // changed to GetEnvironmentFirstCall
        // static bool PrntEnvHeaders( true );
        ////////////////////////////////////////////////
        int Loop;
        std::string StDate;
        std::string EnDate;
        std::string string;
        std::string cTotalEnvDays;
        std::string skyTempModel;
        int DSTActStMon;
        int DSTActStDay;
        int DSTActEnMon;
        int DSTActEnDay;
        int RunStJDay;
        int RunEnJDay;
        bool OkRun;
        int TWeekDay;
        Array1D_int MonWeekDay(12);
        Array1D_int ActEndDayOfMonth(12);
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

        if (BeginSimFlag && dataWeatherManager.GetEnvironmentFirstCall) {

            PrintEndDataDictionary = true;

            ReportOutputFileHeaders(dataWeatherManager, state.outputFiles); // Write the output file header information

            // Setup Output Variables, CurrentModuleObject='All Simulations'

            SetupOutputVariable("Site Outdoor Air Drybulb Temperature", OutputProcessor::Unit::C, OutDryBulbTemp, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Outdoor Air Dewpoint Temperature", OutputProcessor::Unit::C, OutDewPointTemp, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Outdoor Air Wetbulb Temperature", OutputProcessor::Unit::C, OutWetBulbTemp, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Outdoor Air Humidity Ratio", OutputProcessor::Unit::kgWater_kgDryAir, OutHumRat, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Outdoor Air Relative Humidity", OutputProcessor::Unit::Perc, OutRelHum, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Outdoor Air Barometric Pressure", OutputProcessor::Unit::Pa, OutBaroPress, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Wind Speed", OutputProcessor::Unit::m_s, WindSpeed, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Wind Direction", OutputProcessor::Unit::deg, WindDir, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Sky Temperature", OutputProcessor::Unit::C, SkyTemp, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Horizontal Infrared Radiation Rate per Area", OutputProcessor::Unit::W_m2, dataWeatherManager.HorizIRSky, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Diffuse Solar Radiation Rate per Area", OutputProcessor::Unit::W_m2, DifSolarRad, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Direct Solar Radiation Rate per Area", OutputProcessor::Unit::W_m2, BeamSolarRad, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Precipitation Depth", OutputProcessor::Unit::m, LiquidPrecipitation, "Zone", "Sum", "Environment");
            SetupOutputVariable(
                "Site Ground Reflected Solar Radiation Rate per Area", OutputProcessor::Unit::W_m2, GndSolarRad, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Ground Temperature", OutputProcessor::Unit::C, GroundTemp, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Surface Ground Temperature", OutputProcessor::Unit::C, GroundTemp_Surface, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Deep Ground Temperature", OutputProcessor::Unit::C, GroundTemp_Deep, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Simple Factor Model Ground Temperature", OutputProcessor::Unit::C, GroundTempFC, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Outdoor Air Enthalpy", OutputProcessor::Unit::J_kg, OutEnthalpy, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Outdoor Air Density", OutputProcessor::Unit::kg_m3, OutAirDensity, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Solar Azimuth Angle", OutputProcessor::Unit::deg, dataWeatherManager.SolarAzimuthAngle, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Solar Altitude Angle", OutputProcessor::Unit::deg, dataWeatherManager.SolarAltitudeAngle, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Solar Hour Angle", OutputProcessor::Unit::deg, dataWeatherManager.HrAngle, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Rain Status", OutputProcessor::Unit::None, dataWeatherManager.RptIsRain, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Snow on Ground Status", OutputProcessor::Unit::None, dataWeatherManager.RptIsSnow, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Exterior Horizontal Sky Illuminance", OutputProcessor::Unit::lux, HISKF, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Exterior Horizontal Beam Illuminance", OutputProcessor::Unit::lux, HISUNF, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Exterior Beam Normal Illuminance", OutputProcessor::Unit::lux, HISUNFnorm, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Sky Diffuse Solar Radiation Luminous Efficacy", OutputProcessor::Unit::lum_W, PDIFLW, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Beam Solar Radiation Luminous Efficacy", OutputProcessor::Unit::lum_W, PDIRLW, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Daylighting Model Sky Clearness", OutputProcessor::Unit::None, SkyClearness, "Zone", "Average", "Environment");
            SetupOutputVariable(
                "Site Daylighting Model Sky Brightness", OutputProcessor::Unit::None, SkyBrightness, "Zone", "Average", "Environment");
            SetupOutputVariable("Site Daylight Saving Time Status", OutputProcessor::Unit::None, DSTIndicator, "Zone", "State", "Environment");
            SetupOutputVariable("Site Day Type Index", OutputProcessor::Unit::None, dataWeatherManager.RptDayType, "Zone", "State", "Environment");
            SetupOutputVariable("Site Mains Water Temperature", OutputProcessor::Unit::C, WaterMainsTemp, "Zone", "Average", "Environment");

            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("Weather Data", "Environment", "Outdoor Dry Bulb", "[C]", EMSOutDryBulbOverrideOn, EMSOutDryBulbOverrideValue);
                SetupEMSActuator(
                    "Weather Data", "Environment", "Outdoor Dew Point", "[C]", EMSOutDewPointTempOverrideOn, EMSOutDewPointTempOverrideValue);
                SetupEMSActuator(
                    "Weather Data", "Environment", "Outdoor Relative Humidity", "[%]", EMSOutRelHumOverrideOn, EMSOutRelHumOverrideValue);
                SetupEMSActuator("Weather Data", "Environment", "Diffuse Solar", "[W/m2]", EMSDifSolarRadOverrideOn, EMSDifSolarRadOverrideValue);
                SetupEMSActuator("Weather Data", "Environment", "Direct Solar", "[W/m2]", EMSBeamSolarRadOverrideOn, EMSBeamSolarRadOverrideValue);
                SetupEMSActuator("Weather Data", "Environment", "Wind Speed", "[m/s]", EMSWindSpeedOverrideOn, EMSWindSpeedOverrideValue);
                SetupEMSActuator("Weather Data", "Environment", "Wind Direction", "[deg]", EMSWindDirOverrideOn, EMSWindDirOverrideValue);
            }

            dataWeatherManager.GetEnvironmentFirstCall = false;

        } // ... end of BeginSimFlag IF-THEN block.

        if (dataWeatherManager.GetBranchInputOneTimeFlag) {

            SetupInterpolationValues(dataWeatherManager);
            dataWeatherManager.TimeStepFraction = 1.0 / double(NumOfTimeStepInHour);
            rhoAirSTP = Psychrometrics::PsyRhoAirFnPbTdbW(StdPressureSeaLevel, constant_twenty, constant_zero);
            OpenWeatherFile(dataWeatherManager, ErrorsFound); // moved here because of possibility of special days on EPW file
            CloseWeatherFile();
            ReadUserWeatherInput(state);
            AllocateWeatherData(dataWeatherManager);
            if (dataWeatherManager.NumIntervalsPerHour != 1) {
                if (dataWeatherManager.NumIntervalsPerHour != NumOfTimeStepInHour) {
                    ShowSevereError(RoutineName +
                                    "Number of intervals per hour on Weather file does not match specified number of Time Steps Per Hour");
                    ErrorsFound = true;
                }
            }
            dataWeatherManager.GetBranchInputOneTimeFlag = false;
            dataWeatherManager.Envrn = 0;
            if (dataWeatherManager.NumOfEnvrn > 0) {
                ResolveLocationInformation(dataWeatherManager, state.outputFiles, ErrorsFound); // Obtain weather related info from input file
                CheckLocationValidity();
                if ((dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).KindOfEnvrn != ksDesignDay) && (dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).KindOfEnvrn != ksHVACSizeDesignDay)) {
                    CheckWeatherFileValidity(dataWeatherManager);
                }
                if (ErrorsFound) {
                    ShowSevereError(RoutineName + "No location specified, program will terminate.");
                }
            } else {
                ErrorsFound = true;
                ShowSevereError(RoutineName + "No Design Days or Run Period(s) specified, program will terminate.");
            }
            if (DDOnly && TotDesDays == 0) {
                ErrorsFound = true;
                ShowSevereError(RoutineName + "Requested Design Days only (DDOnly) but no Design Days specified, program will terminate.");
            }
            if (ReverseDD && TotDesDays == 1) {
                ErrorsFound = true;
                ShowSevereError(RoutineName + "Requested Reverse Design Days (ReverseDD) but only 1 Design Day specified, program will terminate.");
            }

            // Throw a Fatal now that we have said it'll terminalte
            if (ErrorsFound) {
                CloseWeatherFile(); // will only close if opened.
                ShowFatalError(RoutineName + "Errors found in Weater Data Input. Program terminates.");
            }

            CurrentOverallSimDay = 0;
            TotalOverallSimDays = 0;
            MaxNumberSimYears = 1;
            for (Loop = 1; Loop <= dataWeatherManager.NumOfEnvrn; ++Loop) {
                TotalOverallSimDays += dataWeatherManager.Environment(Loop).TotalDays;
                if (dataWeatherManager.Environment(Loop).KindOfEnvrn == ksRunPeriodWeather) {
                    MaxNumberSimYears = max(MaxNumberSimYears, dataWeatherManager.Environment(Loop).NumSimYears);
                }
            }
            DisplaySimDaysProgress(CurrentOverallSimDay, TotalOverallSimDays);
        }

        CloseWeatherFile(); // will only close if opened.
        ++dataWeatherManager.Envrn;
        dataWeatherManager.DatesShouldBeReset = false;
        if (dataWeatherManager.Envrn > dataWeatherManager.NumOfEnvrn) {
            Available = false;
            dataWeatherManager.Envrn = 0;
            CurEnvirNum = 0;
        } else {
            KindOfSim = dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn;
            DayOfYear = dataWeatherManager.Environment(dataWeatherManager.Envrn).StartJDay;
            DayOfMonth = dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay;
            DataGlobals::CalendarYear = dataWeatherManager.Environment(dataWeatherManager.Envrn).StartYear;
            DataGlobals::CalendarYearChr = std::to_string(DataGlobals::CalendarYear);
            Month = dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth;
            NumOfDayInEnvrn = dataWeatherManager.Environment(dataWeatherManager.Envrn).TotalDays; // Set day loop maximum from DataGlobals
            if (!DoingSizing && !KickOffSimulation) {
                if (AdaptiveComfortRequested_ASH55 || AdaptiveComfortRequested_CEN15251) {
                    if (KindOfSim == ksDesignDay) {
                        if (DoDesDaySim) {
                            ShowWarningError(RoutineName + "Adaptive Comfort being reported during design day.");
                            GrossApproxAvgDryBulb =
                                (dataWeatherManager.DesDayInput(dataWeatherManager.Envrn).MaxDryBulb + (dataWeatherManager.DesDayInput(dataWeatherManager.Envrn).MaxDryBulb - dataWeatherManager.DesDayInput(dataWeatherManager.Envrn).DailyDBRange)) / 2.0;
                            if (AdaptiveComfortRequested_ASH55) CalcThermalComfortAdaptiveASH55(true, false, GrossApproxAvgDryBulb);
                            if (AdaptiveComfortRequested_CEN15251) CalcThermalComfortAdaptiveCEN15251(true, false, GrossApproxAvgDryBulb);
                        }
                    } else {
                        if (DoWeathSim || DoDesDaySim) {
                            if (AdaptiveComfortRequested_ASH55) CalcThermalComfortAdaptiveASH55(true, true, 0.0);
                            if (AdaptiveComfortRequested_CEN15251) CalcThermalComfortAdaptiveCEN15251(true, true, 0.0);
                        }
                    }
                }
            }
            if (dataWeatherManager.Envrn > TotDesDays && dataWeatherManager.WeatherFileExists) {
                OpenEPlusWeatherFile(dataWeatherManager, state.outputFiles, ErrorsFound, false);
            }
            Available = true;
            if ((KindOfSim == ksRunPeriodWeather) && (!dataWeatherManager.WeatherFileExists && DoWeathSim)) {
                if (!DoingSizing && !KickOffSimulation) {
                    ShowSevereError("Weather Simulation requested, but no weather file attached.");
                    ErrorsFound = true;
                }
                if (!DoingHVACSizingSimulations) dataWeatherManager.Envrn = 0;
                Available = false;
            } else if ((KindOfSim == ksRunPeriodWeather) && (!dataWeatherManager.WeatherFileExists && !DoWeathSim)) {
                Available = false;
                if (!DoingHVACSizingSimulations) dataWeatherManager.Envrn = 0;
            } else if ((KindOfSim == ksRunPeriodWeather) && DoingSizing) {
                Available = false;
                dataWeatherManager.Envrn = 0;
            }

            if (!ErrorsFound && Available && dataWeatherManager.Envrn > 0) {
                EnvironmentName = dataWeatherManager.Environment(dataWeatherManager.Envrn).Title;
                CurEnvirNum = dataWeatherManager.Envrn;
                RunPeriodStartDayOfWeek = 0;
                if ((DoDesDaySim && (KindOfSim != ksRunPeriodWeather)) || ((KindOfSim == ksRunPeriodWeather) && DoWeathSim)) {
                    if (dataWeatherManager.PrntEnvHeaders && DoWeatherInitReporting) {
                        static constexpr auto EnvironFormat(
                            "! <Environment>,Environment Name,Environment Type, Start Date, End Date, Start DayOfWeek, Duration {#days}, "
                            "Source:Start DayOfWeek,  Use Daylight Saving, Use Holidays, Apply Weekend Holiday Rule,  Use Rain Values, Use Snow "
                            "Values, Sky Temperature Model\n! <Environment:Special Days>, Special Day Name, Special Day Type, Source, Start Date, Duration {#days}\n! "
                            "<Environment:Daylight Saving>, Daylight Saving Indicator, Source, Start Date, End Date\n! <Environment:WarmupDays>, "
                            "NumberofWarmupDays");
                        print(state.outputFiles.eio, "{}\n", EnvironFormat);
                        dataWeatherManager.PrntEnvHeaders = false;
                    }

                    {
                        auto const SELECT_CASE_var(KindOfSim);

                        if ((SELECT_CASE_var == ksRunPeriodWeather) || (SELECT_CASE_var == ksRunPeriodDesign)) {
                            kindOfRunPeriod = dataWeatherManager.Environment(dataWeatherManager.Envrn).cKindOfEnvrn;
                            if (KindOfSim == ksRunPeriodWeather) {
                                RunPeriodEnvironment = true;
                            } else {
                                RunPeriodEnvironment = false;
                            }
                            ActEndDayOfMonth = dataWeatherManager.EndDayOfMonth;
                            CurrentYearIsLeapYear = dataWeatherManager.Environment(dataWeatherManager.Envrn).IsLeapYear;
                            if (CurrentYearIsLeapYear && dataWeatherManager.WFAllowsLeapYears) {
                                dataWeatherManager.LeapYearAdd = 1;
                            } else {
                                dataWeatherManager.LeapYearAdd = 0;
                            }
                            if (CurrentYearIsLeapYear) {
                                ActEndDayOfMonth(2) = dataWeatherManager.EndDayOfMonth(2) + dataWeatherManager.LeapYearAdd;
                            }
                            dataWeatherManager.UseDaylightSaving = dataWeatherManager.Environment(dataWeatherManager.Envrn).UseDST;
                            dataWeatherManager.UseSpecialDays = dataWeatherManager.Environment(dataWeatherManager.Envrn).UseHolidays;
                            dataWeatherManager.UseRainValues = dataWeatherManager.Environment(dataWeatherManager.Envrn).UseRain;
                            dataWeatherManager.UseSnowValues = dataWeatherManager.Environment(dataWeatherManager.Envrn).UseSnow;

                            bool missingLeap(false); // Defer acting on anything found here until after the other range checks (see below)
                            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).ActualWeather && !dataWeatherManager.WFAllowsLeapYears) {
                                for (int year = dataWeatherManager.Environment(dataWeatherManager.Envrn).StartYear; year <= dataWeatherManager.Environment(dataWeatherManager.Envrn).EndYear; year++) {
                                    if (isLeapYear(year)) {
                                        ShowSevereError(
                                            RoutineName + "Weatherfile does not support leap years but runperiod includes a leap year (" +
                                            std::to_string(year) + ")");
                                        missingLeap = true;
                                    }
                                }
                            }

                            OkRun = false;

                            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).ActualWeather) {
                                // Actual weather
                                for (auto &dataperiod : dataWeatherManager.DataPeriods) {
                                    int runStartJulian = dataperiod.DataStJDay;
                                    int runEndJulian = dataperiod.DataEnJDay;
                                    if (!dataperiod.HasYearData) {
                                        ShowSevereError(RoutineName + "Actual weather runperiod has been entered but weatherfile DATA PERIOD "
                                                        "does not have year included in start/end date.");
                                        ShowContinueError("...to match the RunPeriod, the DATA PERIOD should be mm/dd/yyyy for both, or");
                                        ShowContinueError("...set \"Treat Weather as Actual\" to \"No\".");
                                    }
                                    if (!BetweenDates(dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDate, runStartJulian, runEndJulian)) continue;
                                    if (!BetweenDates(dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDate, runStartJulian, runEndJulian)) continue;
                                    OkRun = true;
                                    break;
                                }
                            } else {
                                // Typical (or just non-actual) weather
                                for (auto &dataperiod : dataWeatherManager.DataPeriods) {
                                    // Since this is not actual weather, there may be issues with this calculation
                                    // Assume the weather data starts the same year as the simulation, so LeapYearAdd is what
                                    // should be used.
                                    int runStartOrdinal = General::OrdinalDay(dataperiod.StMon, dataperiod.StDay, dataWeatherManager.LeapYearAdd);
                                    // This one is harder, leave as is for now. What about multiple years of data?
                                    int runEndOrdinal = General::OrdinalDay(dataperiod.EnMon, dataperiod.EnDay, dataWeatherManager.LeapYearAdd);
                                    if (runStartOrdinal == 1 && (runEndOrdinal == 366 || runEndOrdinal == 365)) {
                                        // Complete year(s) of weather data, will wrap around
                                        OkRun = true;
                                        break;
                                    }
                                    if (!BetweenDates(dataWeatherManager.Environment(dataWeatherManager.Envrn).StartJDay, runStartOrdinal, runEndOrdinal)) continue;
                                    if (!BetweenDates(dataWeatherManager.Environment(dataWeatherManager.Envrn).EndJDay, runStartOrdinal, runEndOrdinal)) continue;
                                    OkRun = true;
                                }
                            }

                            if (!OkRun) {
                                if (!dataWeatherManager.Environment(dataWeatherManager.Envrn).ActualWeather) {
                                    StDate = format(DateFormat, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay);
                                    EnDate = format(DateFormat, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay);
                                    ShowSevereError(RoutineName + "Runperiod [mm/dd] (Start=" + StDate + ",End=" + EnDate +
                                                    ") requested not within Data Period(s) from Weather File");
                                } else {
                                    StDate = format(DateFormatWithYear, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartYear);
                                    EnDate = format(DateFormatWithYear, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndYear);
                                    ShowSevereError(RoutineName + "Runperiod [mm/dd/yyyy] (Start=" + StDate + ",End=" + EnDate +
                                                    ") requested not within Data Period(s) from Weather File");
                                }
                                StDate = format(DateFormat, dataWeatherManager.DataPeriods(1).StMon, dataWeatherManager.DataPeriods(1).StDay);
                                EnDate = format(DateFormat, dataWeatherManager.DataPeriods(1).EnMon, dataWeatherManager.DataPeriods(1).EnDay);
                                if (dataWeatherManager.DataPeriods(1).StYear > 0) {
                                    string = RoundSigDigits(dataWeatherManager.DataPeriods(1).StYear);
                                    StDate += "/" + string;
                                } else {
                                    StDate += "/<noyear>";
                                }
                                if (dataWeatherManager.DataPeriods(1).EnYear > 0) {
                                    string = RoundSigDigits(dataWeatherManager.DataPeriods(1).EnYear);
                                    EnDate += "/" + string;
                                } else {
                                    EnDate += "/<noyear>";
                                }
                                if (dataWeatherManager.NumDataPeriods == 1) {
                                    ShowContinueError("Weather Data Period (Start=" + StDate + ",End=" + EnDate + ')');
                                } else {
                                    ShowContinueError("Multiple Weather Data Periods 1st (Start=" + StDate + ",End=" + EnDate + ')');
                                }
                                ShowFatalError(RoutineName + "Program terminates due to preceding condition.");
                            }

                            if (missingLeap) {
                                // Bail out now if we still need to
                                ShowFatalError(RoutineName + "Program terminates due to preceding condition.");
                            }

                            // Following builds Environment start/end for ASHRAE 55 warnings
                            StDate = format(DateFormat, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay);
                            EnDate = format(DateFormat, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay);
                            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn == ksRunPeriodWeather) {
                                StDate += "/" + RoundSigDigits(dataWeatherManager.Environment(dataWeatherManager.Envrn).StartYear);
                                EnDate += "/" + RoundSigDigits(dataWeatherManager.Environment(dataWeatherManager.Envrn).EndYear);
                            }
                            EnvironmentStartEnd = StDate + " - " + EnDate;

                            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).DayOfWeek == 0) { // Use Sunday
                                TWeekDay = 1;
                                MonWeekDay = dataWeatherManager.DataPeriods(Loop).MonWeekDay;
                            } else {
                                TWeekDay = dataWeatherManager.Environment(dataWeatherManager.Envrn).DayOfWeek;
                                MonWeekDay = dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay;
                            }

                            if (DoWeatherInitReporting) {
                                if (dataWeatherManager.Environment(dataWeatherManager.Envrn).UseDST) {
                                    AlpUseDST = "Yes";
                                } else {
                                    AlpUseDST = "No";
                                }
                                if (dataWeatherManager.Environment(dataWeatherManager.Envrn).UseHolidays) {
                                    AlpUseSpec = "Yes";
                                } else {
                                    AlpUseSpec = "No";
                                }
                                if (dataWeatherManager.Environment(dataWeatherManager.Envrn).ApplyWeekendRule) {
                                    ApWkRule = "Yes";
                                } else {
                                    ApWkRule = "No";
                                }
                                if (dataWeatherManager.Environment(dataWeatherManager.Envrn).UseRain) {
                                    AlpUseRain = "Yes";
                                } else {
                                    AlpUseRain = "No";
                                }
                                if (dataWeatherManager.Environment(dataWeatherManager.Envrn).UseSnow) {
                                    AlpUseSnow = "Yes";
                                } else {
                                    AlpUseSnow = "No";
                                }
                                cTotalEnvDays = RoundSigDigits(dataWeatherManager.Environment(dataWeatherManager.Envrn).TotalDays);
                                skyTempModel = SkyTempModelNames(dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel + 1);

                                print(state.outputFiles.eio,
                                      EnvNameFormat,
                                      dataWeatherManager.Environment(dataWeatherManager.Envrn).Title,
                                      kindOfRunPeriod,
                                      StDate,
                                      EnDate,
                                      ValidDayNames(TWeekDay),
                                      cTotalEnvDays,
                                      "Use RunPeriod Specified Day",
                                      AlpUseDST,
                                      AlpUseSpec,
                                      ApWkRule,
                                      AlpUseRain,
                                      AlpUseSnow,
                                      skyTempModel);
                            }

                            if (!DoingSizing && !KickOffSimulation) {
                                if ((KindOfSim == ksRunPeriodWeather && DoWeathSim)) {
                                    if (AdaptiveComfortRequested_ASH55 || AdaptiveComfortRequested_CEN15251) {
                                        if (dataWeatherManager.WFAllowsLeapYears) {
                                            ShowSevereError(RoutineName +
                                                            "AdaptiveComfort Reporting does not work correctly with leap years in weather files.");
                                            ErrorsFound = true;
                                        }
                                        if (dataWeatherManager.NumDataPeriods != 1) {
                                            ShowSevereError(
                                                RoutineName +
                                                "AdaptiveComfort Reporting does not work correctly with multiple dataperiods in weather files.");
                                            ErrorsFound = true;
                                        }
                                        if (dataWeatherManager.DataPeriods(1).StMon == 1 && dataWeatherManager.DataPeriods(1).StDay == 1) {
                                            RunStJDay = General::OrdinalDay(dataWeatherManager.DataPeriods(1).StMon, dataWeatherManager.DataPeriods(1).StDay, dataWeatherManager.LeapYearAdd);
                                            RunEnJDay = General::OrdinalDay(dataWeatherManager.DataPeriods(1).EnMon, dataWeatherManager.DataPeriods(1).EnDay, dataWeatherManager.LeapYearAdd);
                                            if (RunEnJDay - RunStJDay + 1 != 365) {
                                                ShowSevereError(RoutineName + "AdaptiveComfort Reporting does not work correctly with weather files "
                                                                              "that do not contain 365 days.");
                                                ErrorsFound = true;
                                            }
                                        } else {
                                            ShowSevereError(RoutineName + "AdaptiveComfort Reporting does not work correctly with weather files that "
                                                                          "do not start on 1 January.");
                                            ErrorsFound = true;
                                        }
                                        if (dataWeatherManager.NumIntervalsPerHour != 1) {
                                            ShowSevereError(RoutineName + "AdaptiveComfort Reporting does not work correctly with weather files that "
                                                                          "have multiple interval records per hour.");
                                            ErrorsFound = true;
                                        }
                                    }
                                }
                            }

                            // Only need to set Week days for Run Days
                            RunPeriodStartDayOfWeek = TWeekDay;
                            dataWeatherManager.WeekDayTypes = 0;
                            JDay5Start = General::OrdinalDay(dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay, dataWeatherManager.LeapYearAdd);
                            JDay5End = General::OrdinalDay(dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay, dataWeatherManager.LeapYearAdd);

                            dataWeatherManager.curSimDayForEndOfRunPeriod = dataWeatherManager.Environment(dataWeatherManager.Envrn).TotalDays;

                            Loop = JDay5Start;
                            while (true) {
                                dataWeatherManager.WeekDayTypes(Loop) = TWeekDay;
                                TWeekDay = mod(TWeekDay, 7) + 1;
                                ++Loop;
                                if (Loop > 366) Loop = 1;
                                if (Loop == JDay5End) break;
                            }

                            if (dataWeatherManager.UseDaylightSaving) {
                                if (dataWeatherManager.EPWDaylightSaving) {
                                    dataWeatherManager.DaylightSavingIsActive = true;
                                }
                            } else {
                                dataWeatherManager.DaylightSavingIsActive = false;
                            }
                            if (dataWeatherManager.IDFDaylightSaving) {
                                dataWeatherManager.DaylightSavingIsActive = true;
                            }
                            dataWeatherManager.Environment(dataWeatherManager.Envrn).SetWeekDays = false;

                            if (dataWeatherManager.DaylightSavingIsActive) {
                                SetDSTDateRanges(dataWeatherManager, MonWeekDay, dataWeatherManager.DSTIndex, DSTActStMon, DSTActStDay, DSTActEnMon, DSTActEnDay);
                            }

                            SetSpecialDayDates(dataWeatherManager, MonWeekDay);

                            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth != 1 || dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay != 1) {
                                dataWeatherManager.StartDatesCycleShouldBeReset = true;
                                dataWeatherManager.Jan1DatesShouldBeReset = true;
                            }

                            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth == 1 && dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay == 1) {
                                dataWeatherManager.StartDatesCycleShouldBeReset = false;
                                dataWeatherManager.Jan1DatesShouldBeReset = true;
                            }

                            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).ActualWeather) {
                                dataWeatherManager.StartDatesCycleShouldBeReset = false;
                                dataWeatherManager.Jan1DatesShouldBeReset = true;
                            }

                            // Report Actual Dates for Daylight Saving and Special Days
                            if (!KickOffSimulation) {
                                Source = BlankString;
                                if (dataWeatherManager.UseDaylightSaving) {
                                    if (dataWeatherManager.EPWDaylightSaving) {
                                        Source = "WeatherFile";
                                    }
                                } else {
                                    Source = "RunPeriod Object";
                                }
                                if (dataWeatherManager.IDFDaylightSaving) {
                                    Source = "InputFile";
                                }
                                if (dataWeatherManager.DaylightSavingIsActive && DoWeatherInitReporting) {
                                    StDate = format(DateFormat, DSTActStMon, DSTActStDay);
                                    EnDate = format(DateFormat, DSTActEnMon, DSTActEnDay);
                                    print(state.outputFiles.eio, EnvDSTYFormat, Source, StDate, EnDate);
                                } else if (DoOutputReporting) {
                                    print(state.outputFiles.eio, EnvDSTNFormat, Source);
                                }
                                for (Loop = 1; Loop <= dataWeatherManager.NumSpecialDays; ++Loop) {
                                    static constexpr auto EnvSpDyFormat("Environment:Special Days,{},{},{},{},{:3}");
                                    if (dataWeatherManager.SpecialDays(Loop).WthrFile && dataWeatherManager.UseSpecialDays && DoWeatherInitReporting) {
                                        StDate = format(DateFormat, dataWeatherManager.SpecialDays(Loop).ActStMon, dataWeatherManager.SpecialDays(Loop).ActStDay);
                                        print(state.outputFiles.eio,
                                              EnvSpDyFormat,
                                              dataWeatherManager.SpecialDays(Loop).Name,
                                              SpecialDayNames(dataWeatherManager.SpecialDays(Loop).DayType),
                                              "WeatherFile",
                                              StDate,
                                            dataWeatherManager.SpecialDays(Loop).Duration);
                                    }
                                    if (!dataWeatherManager.SpecialDays(Loop).WthrFile && DoWeatherInitReporting) {
                                        StDate = format(DateFormat, dataWeatherManager.SpecialDays(Loop).ActStMon, dataWeatherManager.SpecialDays(Loop).ActStDay);
                                        print(state.outputFiles.eio,
                                              EnvSpDyFormat,
                                              dataWeatherManager.SpecialDays(Loop).Name,
                                              SpecialDayNames(dataWeatherManager.SpecialDays(Loop).DayType),
                                              "InputFile",
                                              StDate,
                                            dataWeatherManager.SpecialDays(Loop).Duration);
                                    }
                                }
                            }

                        } else if (SELECT_CASE_var == ksDesignDay || SELECT_CASE_var == ksHVACSizeDesignDay) { // Design Day
                            RunPeriodEnvironment = false;
                            StDate = format(DateFormat, dataWeatherManager.DesDayInput(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum).Month, dataWeatherManager.DesDayInput(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum).DayOfMonth);
                            EnDate = StDate;
                            if (dataWeatherManager.DesDayInput(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum).DayType <= 7 && DoWeatherInitReporting) {

                                print(state.outputFiles.eio,
                                      EnvNameFormat,
                                      dataWeatherManager.Environment(dataWeatherManager.Envrn).Title,
                                      "SizingPeriod:DesignDay",
                                      StDate,
                                      EnDate,
                                      DaysOfWeek(dataWeatherManager.DesDayInput(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum).DayType),
                                      "1",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      SkyTempModelNames(dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel + 1));
                            } else if (DoWeatherInitReporting) {
                                print(state.outputFiles.eio,
                                      EnvNameFormat,
                                      dataWeatherManager.Environment(dataWeatherManager.Envrn).Title,
                                      "SizingPeriod:DesignDay",
                                      StDate,
                                      EnDate,
                                      SpecialDayNames(dataWeatherManager.DesDayInput(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum).DayType - 7),
                                      "1",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      "N/A",
                                      SkyTempModelNames(dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel + 1));
                            }
                            if (dataWeatherManager.DesDayInput(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum).DSTIndicator == 0 && DoWeatherInitReporting) {
                                print(state.outputFiles.eio, EnvDSTNFormat, "SizingPeriod:DesignDay");
                            } else if (DoWeatherInitReporting) {
                                print(state.outputFiles.eio, EnvDSTYFormat, "SizingPeriod:DesignDay", StDate, EnDate);
                            }
                        }
                    }
                }
            } // ErrorsFound
        }

        if (ErrorsFound && !DoingSizing && !KickOffSimulation) {
            ShowSevereError(RoutineName + "Errors found in getting a new environment");
            Available = false;
        } else if (ErrorsFound) {
            Available = false;
        }
        return Available && !ErrorsFound;
    }

    void AddDesignSetToEnvironmentStruct(WeatherManagerData &dataWeatherManager, int const HVACSizingIterCount)
    {
        // SUBROUTINE INFORMATION:

        using DataGlobals::ksDesignDay;
        using DataGlobals::ksHVACSizeDesignDay;
        using DataGlobals::ksHVACSizeRunPeriodDesign;
        using DataGlobals::ksRunPeriodDesign;

        int OrigNumOfEnvrn;

        OrigNumOfEnvrn = dataWeatherManager.NumOfEnvrn;
        for (int i = 1; i <= OrigNumOfEnvrn; ++i) {
            if (dataWeatherManager.Environment(i).KindOfEnvrn == ksDesignDay) {
                dataWeatherManager.Environment.redimension(++dataWeatherManager.NumOfEnvrn);
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn) = dataWeatherManager.Environment(i); // copy over seed data from current array element
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).SeedEnvrnNum = i;
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).KindOfEnvrn = ksHVACSizeDesignDay;
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).Title = dataWeatherManager.Environment(i).Title + " HVAC Sizing Pass " + RoundSigDigits(HVACSizingIterCount);
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).HVACSizingIterationNum = HVACSizingIterCount;
            } else if (dataWeatherManager.Environment(i).KindOfEnvrn == ksRunPeriodDesign) {
                dataWeatherManager.Environment.redimension(++dataWeatherManager.NumOfEnvrn);
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn) = dataWeatherManager.Environment(i); // copy over seed data
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).SeedEnvrnNum = i;
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).KindOfEnvrn = ksHVACSizeRunPeriodDesign;
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).Title = dataWeatherManager.Environment(i).Title + " HVAC Sizing Pass " + RoundSigDigits(HVACSizingIterCount);
                dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).HVACSizingIterationNum = HVACSizingIterCount;
            }
        } // for each loop over Environment data strucure
    }

    void SetupWeekDaysByMonth(WeatherManagerData &dataWeatherManager, int const StMon, int const StDay, int const StWeekDay, Array1D_int &WeekDays)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the weekday for each month based on the start date and
        // weekday specified for that date.

        // Argument array dimensioning
        EP_SIZE_CHECK(WeekDays, 12);

        int Loop;
        int CurWeekDay;

        // Set 1st day of Start Month
        CurWeekDay = StWeekDay;
        for (Loop = 1; Loop <= StDay - 1; ++Loop) {
            --CurWeekDay;
            if (CurWeekDay == 0) CurWeekDay = 7;
        }

        WeekDays(StMon) = CurWeekDay;
        for (Loop = StMon + 1; Loop <= 12; ++Loop) {

            {
                auto const SELECT_CASE_var(Loop);
                if (SELECT_CASE_var == 2) {
                    CurWeekDay += dataWeatherManager.EndDayOfMonth(1);
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(Loop) = CurWeekDay;

                } else if (SELECT_CASE_var == 3) {
                    CurWeekDay += dataWeatherManager.EndDayOfMonth(Loop - 1) + dataWeatherManager.LeapYearAdd;
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(Loop) = CurWeekDay;

                } else if ((SELECT_CASE_var >= 4) && (SELECT_CASE_var <= 12)) {
                    CurWeekDay += dataWeatherManager.EndDayOfMonth(Loop - 1);
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(Loop) = CurWeekDay;
                }
            }
        }

        if (any_eq(WeekDays, 0)) {
            // need to start at StMon and go backwards.
            // EndDayOfMonth is also "days" in month.  (without leapyear day in February)
            CurWeekDay = StWeekDay;
            for (Loop = 1; Loop <= StDay - 1; ++Loop) {
                --CurWeekDay;
                if (CurWeekDay == 0) CurWeekDay = 7;
            }

            for (Loop = StMon - 1; Loop >= 1; --Loop) {

                {
                    auto const SELECT_CASE_var(Loop);

                    if (SELECT_CASE_var == 1) {
                        CurWeekDay -= dataWeatherManager.EndDayOfMonth(1);
                        while (CurWeekDay <= 0) {
                            CurWeekDay += 7;
                        }
                        WeekDays(Loop) = CurWeekDay;

                    } else if (SELECT_CASE_var == 2) {
                        CurWeekDay = CurWeekDay - dataWeatherManager.EndDayOfMonth(2) + dataWeatherManager.LeapYearAdd;
                        while (CurWeekDay <= 0) {
                            CurWeekDay += 7;
                        }
                        WeekDays(Loop) = CurWeekDay;

                    } else if ((SELECT_CASE_var >= 3) && (SELECT_CASE_var <= 12)) {
                        CurWeekDay -= dataWeatherManager.EndDayOfMonth(Loop);
                        while (CurWeekDay <= 0) {
                            CurWeekDay += 7;
                        }
                        WeekDays(Loop) = CurWeekDay;
                    }
                }
            }
        }
    }

    void ResetWeekDaysByMonth(WeatherManagerData &dataWeatherManager, Array1D_int &WeekDays,
                              int const LeapYearAdd,
                              int const StartMonth,
                              int const StartMonthDay,
                              int const EndMonth,
                              int const EndMonthDay,
                              bool const Rollover,
                              Optional_bool_const MidSimReset)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine resets the weekday for each month based on the current weekday
        // and previous weekdays per month.

        // Argument array dimensioning
        EP_SIZE_CHECK(WeekDays, 12);

        Array1D_int WeekDaysCopy(12);
        int Loop;
        int CurWeekDay;
        bool ResetMidSimulation;

        ResetMidSimulation = false;
        if (present(MidSimReset)) ResetMidSimulation = MidSimReset;

        WeekDaysCopy = WeekDays;
        if (!ResetMidSimulation) {
            if (Rollover) {
                if (StartMonth == 1) {
                    CurWeekDay = WeekDays(12) + dataWeatherManager.EndDayOfMonth(12) + StartMonthDay - 1;
                } else {
                    CurWeekDay = WeekDays(EndMonth) + EndMonthDay;
                }
            } else { // restart at same as before
                CurWeekDay = WeekDays(StartMonth);
            }
            while (CurWeekDay > 7) {
                CurWeekDay -= 7;
            }

            WeekDays = 0;
            WeekDays(StartMonth) = CurWeekDay;
            for (Loop = StartMonth + 1; Loop <= 12; ++Loop) {
                {
                    auto const SELECT_CASE_var(Loop);
                    if (SELECT_CASE_var == 2) {
                        CurWeekDay += dataWeatherManager.EndDayOfMonth(1);
                        while (CurWeekDay > 7) {
                            CurWeekDay -= 7;
                        }
                        WeekDays(Loop) = CurWeekDay;

                    } else if (SELECT_CASE_var == 3) {
                        CurWeekDay += dataWeatherManager.EndDayOfMonth(Loop - 1) + LeapYearAdd;
                        while (CurWeekDay > 7) {
                            CurWeekDay -= 7;
                        }
                        WeekDays(Loop) = CurWeekDay;

                    } else if ((SELECT_CASE_var >= 4) && (SELECT_CASE_var <= 12)) {
                        CurWeekDay += dataWeatherManager.EndDayOfMonth(Loop - 1);
                        while (CurWeekDay > 7) {
                            CurWeekDay -= 7;
                        }
                        WeekDays(Loop) = CurWeekDay;
                    }
                }
            }

            if (any_eq(WeekDays, 0)) {
                // need to start at StMon and go backwards.
                // EndDayOfMonth is also "days" in month.  (without leapyear day in February)
                CurWeekDay = WeekDays(StartMonth);
                for (Loop = 1; Loop <= StartMonthDay - 1; ++Loop) {
                    --CurWeekDay;
                    if (CurWeekDay == 0) CurWeekDay = 7;
                }

                for (Loop = StartMonth - 1; Loop >= 1; --Loop) {

                    {
                        auto const SELECT_CASE_var(Loop);

                        if (SELECT_CASE_var == 1) {
                            CurWeekDay -= dataWeatherManager.EndDayOfMonth(1);
                            while (CurWeekDay <= 0) {
                                CurWeekDay += 7;
                            }
                            WeekDays(Loop) = CurWeekDay;

                        } else if (SELECT_CASE_var == 2) {
                            CurWeekDay = CurWeekDay - dataWeatherManager.EndDayOfMonth(2) + LeapYearAdd;
                            while (CurWeekDay <= 0) {
                                CurWeekDay += 7;
                            }
                            WeekDays(Loop) = CurWeekDay;

                        } else if ((SELECT_CASE_var >= 3) && (SELECT_CASE_var <= 12)) {
                            CurWeekDay -= dataWeatherManager.EndDayOfMonth(Loop);
                            while (CurWeekDay <= 0) {
                                CurWeekDay += 7;
                            }
                            WeekDays(Loop) = CurWeekDay;
                        }
                    }
                }
            }

        } else {
            if (Rollover) {
                if (StartMonth == 1) {
                    CurWeekDay = WeekDays(12) + dataWeatherManager.EndDayOfMonth(12) + StartMonthDay - 1;
                } else {
                    CurWeekDay = WeekDays(EndMonth) + EndMonthDay;
                }
            } else { // restart at same as before
                CurWeekDay = WeekDays(StartMonth);
            }
            while (CurWeekDay > 7) {
                CurWeekDay -= 7;
            }
            WeekDays = 0;
            if (StartMonth != 1) {
                CurWeekDay = WeekDaysCopy(12) + dataWeatherManager.EndDayOfMonth(12);
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(1) = CurWeekDay;
                CurWeekDay += dataWeatherManager.EndDayOfMonth(1);
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(2) = CurWeekDay;
                CurWeekDay += dataWeatherManager.EndDayOfMonth(2) + LeapYearAdd;
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(3) = CurWeekDay;
                for (Loop = 4; Loop <= 12; ++Loop) {
                    CurWeekDay += dataWeatherManager.EndDayOfMonth(Loop - 1);
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(Loop) = CurWeekDay;
                }
            } else {
                WeekDays = 0;
                WeekDays(StartMonth) = CurWeekDay;
                for (Loop = StartMonth + 1; Loop <= 12; ++Loop) {
                    {
                        auto const SELECT_CASE_var(Loop);
                        if (SELECT_CASE_var == 2) {
                            CurWeekDay += dataWeatherManager.EndDayOfMonth(1);
                            while (CurWeekDay > 7) {
                                CurWeekDay -= 7;
                            }
                            WeekDays(Loop) = CurWeekDay;

                        } else if (SELECT_CASE_var == 3) {
                            CurWeekDay += dataWeatherManager.EndDayOfMonth(Loop - 1) + LeapYearAdd;
                            while (CurWeekDay > 7) {
                                CurWeekDay -= 7;
                            }
                            WeekDays(Loop) = CurWeekDay;

                        } else if ((SELECT_CASE_var >= 4) && (SELECT_CASE_var <= 12)) {
                            CurWeekDay += dataWeatherManager.EndDayOfMonth(Loop - 1);
                            while (CurWeekDay > 7) {
                                CurWeekDay -= 7;
                            }
                            WeekDays(Loop) = CurWeekDay;
                        }
                    }
                }

                if (any_eq(WeekDays, 0)) {
                    // need to start at StMon and go backwards.
                    // EndDayOfMonth is also "days" in month.  (without leapyear day in February)
                    CurWeekDay = WeekDays(StartMonth);
                    for (Loop = 1; Loop <= StartMonthDay - 1; ++Loop) {
                        --CurWeekDay;
                        if (CurWeekDay == 0) CurWeekDay = 7;
                    }

                    for (Loop = StartMonth - 1; Loop >= 1; --Loop) {

                        {
                            auto const SELECT_CASE_var(Loop);

                            if (SELECT_CASE_var == 1) {
                                CurWeekDay -= dataWeatherManager.EndDayOfMonth(1);
                                while (CurWeekDay <= 0) {
                                    CurWeekDay += 7;
                                }
                                WeekDays(Loop) = CurWeekDay;

                            } else if (SELECT_CASE_var == 2) {
                                CurWeekDay = CurWeekDay - dataWeatherManager.EndDayOfMonth(2) + LeapYearAdd;
                                while (CurWeekDay <= 0) {
                                    CurWeekDay += 7;
                                }
                                WeekDays(Loop) = CurWeekDay;

                            } else if ((SELECT_CASE_var >= 3) && (SELECT_CASE_var <= 12)) {
                                CurWeekDay -= dataWeatherManager.EndDayOfMonth(Loop);
                                while (CurWeekDay <= 0) {
                                    CurWeekDay += 7;
                                }
                                WeekDays(Loop) = CurWeekDay;
                            }
                        }
                    }
                }
            }
        }
    }

    void SetDSTDateRanges(WeatherManagerData &dataWeatherManager, Array1D_int &MonWeekDay, // Weekday of each day 1 of month
                          Array1D_int &DSTIndex,   // DST Index for each julian day (1:366)
                          Optional_int DSTActStMon,
                          Optional_int DSTActStDay,
                          Optional_int DSTActEnMon,
                          Optional_int DSTActEnDay)
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

        static std::string const RoutineName("SetDSTDateRanges: ");

        int ActStartMonth; // Actual Start Month
        int ActStartDay;   // Actual Start Day of Month
        int ActEndMonth;   // Actual End Month
        int ActEndDay;     // Actual End Day of Month
        int ThisDay;       // Day of Month
        int JDay;
        int JDay1;
        bool ErrorsFound;
        Array1D_int ActEndDayOfMonth(12);

        ErrorsFound = false;
        ActEndDayOfMonth = dataWeatherManager.EndDayOfMonth;
        ActEndDayOfMonth(2) = dataWeatherManager.EndDayOfMonth(2) + dataWeatherManager.LeapYearAdd;
        if (dataWeatherManager.DST.StDateType == dataWeatherManager.MonthDay) {
            ActStartMonth = dataWeatherManager.DST.StMon;
            ActStartDay = dataWeatherManager.DST.StDay;
        } else if (dataWeatherManager.DST.StDateType == dataWeatherManager.NthDayInMonth) {
            ThisDay = dataWeatherManager.DST.StWeekDay - MonWeekDay(dataWeatherManager.DST.StMon) + 1;
            while (ThisDay <= 0) {
                ThisDay += 7;
            }
            ThisDay += 7 * (dataWeatherManager.DST.StDay - 1);
            if (ThisDay > ActEndDayOfMonth(dataWeatherManager.DST.StMon)) {
                ShowSevereError(RoutineName + "Determining DST: DST Start Date, Nth Day of Month, not enough Nths");
                ErrorsFound = true;
            } else {
                ActStartMonth = dataWeatherManager.DST.StMon;
                ActStartDay = ThisDay;
            }
        } else { // LastWeekDayInMonth
            ThisDay = dataWeatherManager.DST.StWeekDay - MonWeekDay(dataWeatherManager.DST.StMon) + 1;
            while (ThisDay + 7 <= ActEndDayOfMonth(dataWeatherManager.DST.StMon)) {
                ThisDay += 7;
            }
            ActStartMonth = dataWeatherManager.DST.StMon;
            ActStartDay = ThisDay;
        }

        if (dataWeatherManager.DST.EnDateType == dataWeatherManager.MonthDay) {
            ActEndMonth = dataWeatherManager.DST.EnMon;
            ActEndDay = dataWeatherManager.DST.EnDay;
        } else if (dataWeatherManager.DST.EnDateType == dataWeatherManager.NthDayInMonth) {
            ThisDay = dataWeatherManager.DST.EnWeekDay - MonWeekDay(dataWeatherManager.DST.EnMon) + 1;
            while (ThisDay <= 0) {
                ThisDay += 7;
            }
            ThisDay += 7 * (dataWeatherManager.DST.EnDay - 1);
            if (ThisDay > ActEndDayOfMonth(dataWeatherManager.DST.EnMon)) {
                ActEndMonth = 0; // Suppress uninitialized warning
                ActEndDay = 0;   // Suppress uninitialized warning
                ShowSevereError(RoutineName + "Determining DST: DST End Date, Nth Day of Month, not enough Nths");
                ErrorsFound = true;
            } else {
                ActEndMonth = dataWeatherManager.DST.EnMon;
                ActEndDay = ThisDay;
            }
        } else { // LastWeekDayInMonth
            ThisDay = dataWeatherManager.DST.EnWeekDay - MonWeekDay(dataWeatherManager.DST.EnMon) + 1;
            while (ThisDay + 7 <= ActEndDayOfMonth(dataWeatherManager.DST.EnMon)) {
                ThisDay += 7;
            }
            ActEndMonth = dataWeatherManager.DST.EnMon;
            ActEndDay = ThisDay;
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Program terminates due to preceding condition(s).");
        }

        if (present(DSTActStMon)) {
            DSTActStMon = ActStartMonth;
            DSTActStDay = ActStartDay;
            DSTActEnMon = ActEndMonth;
            DSTActEnDay = ActEndDay;
        }

        DSTIndex = 0;
        JDay = General::OrdinalDay(ActStartMonth, ActStartDay, dataWeatherManager.LeapYearAdd);
        JDay1 = General::OrdinalDay(ActEndMonth, ActEndDay, dataWeatherManager.LeapYearAdd);
        if (JDay1 >= JDay) {
            DSTIndex({JDay, JDay1}) = 1;
        } else {
            DSTIndex({JDay, 366}) = 1;
            DSTIndex({1, JDay1}) = 1;
        }
    }

    void SetSpecialDayDates(WeatherManagerData &dataWeatherManager, Array1D_int &MonWeekDay) // Weekday of each day 1 of month
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

        static std::string const RoutineName("SetSpecialDayDates: ");

        int Loop;
        int ThisDay;
        int JDay;
        int JDay1;
        int Loop1;
        bool ErrorsFound;
        Array1D_int ActEndDayOfMonth(12);

        ErrorsFound = false;
        ActEndDayOfMonth = dataWeatherManager.EndDayOfMonth;
        ActEndDayOfMonth(2) = dataWeatherManager.EndDayOfMonth(2) + dataWeatherManager.LeapYearAdd;
        dataWeatherManager.SpecialDayTypes = 0;
        for (Loop = 1; Loop <= dataWeatherManager.NumSpecialDays; ++Loop) {
            if (dataWeatherManager.SpecialDays(Loop).WthrFile && !dataWeatherManager.UseSpecialDays) continue;
            if (dataWeatherManager.SpecialDays(Loop).DateType <= dataWeatherManager.MonthDay) {
                JDay = General::OrdinalDay(dataWeatherManager.SpecialDays(Loop).Month, dataWeatherManager.SpecialDays(Loop).Day, dataWeatherManager.LeapYearAdd);
                if (dataWeatherManager.SpecialDays(Loop).Duration == 1 && dataWeatherManager.Environment(dataWeatherManager.Envrn).ApplyWeekendRule) {
                    if (dataWeatherManager.WeekDayTypes(JDay) == 1) {
                        // Sunday, must go to Monday
                        ++JDay;
                        if (JDay == 366 && dataWeatherManager.LeapYearAdd == 0) JDay = 1;
                    } else if (dataWeatherManager.WeekDayTypes(JDay) == 7) {
                        ++JDay;
                        if (JDay == 366 && dataWeatherManager.LeapYearAdd == 0) JDay = 1;
                        ++JDay;
                        if (JDay == 366 && dataWeatherManager.LeapYearAdd == 0) JDay = 1;
                    }
                }
                General::InvOrdinalDay(JDay, dataWeatherManager.SpecialDays(Loop).ActStMon, dataWeatherManager.SpecialDays(Loop).ActStDay, dataWeatherManager.LeapYearAdd);
            } else if (dataWeatherManager.SpecialDays(Loop).DateType == dataWeatherManager.NthDayInMonth) {
                if (dataWeatherManager.SpecialDays(Loop).WeekDay >= MonWeekDay(dataWeatherManager.SpecialDays(Loop).Month)) {
                    ThisDay = dataWeatherManager.SpecialDays(Loop).WeekDay - MonWeekDay(dataWeatherManager.SpecialDays(Loop).Month) + 1;
                } else {
                    ThisDay = dataWeatherManager.SpecialDays(Loop).WeekDay - MonWeekDay(dataWeatherManager.SpecialDays(Loop).Month) + 1 + 7;
                }
                ThisDay += 7 * (dataWeatherManager.SpecialDays(Loop).Day - 1);
                if (ThisDay > ActEndDayOfMonth(dataWeatherManager.SpecialDays(Loop).Month)) {
                    ShowSevereError(RoutineName + "Special Day Date, Nth Day of Month, not enough Nths, for SpecialDay=" + dataWeatherManager.SpecialDays(Loop).Name);
                    ErrorsFound = true;
                    continue;
                }
                dataWeatherManager.SpecialDays(Loop).ActStMon = dataWeatherManager.SpecialDays(Loop).Month;
                dataWeatherManager.SpecialDays(Loop).ActStDay = ThisDay;
                JDay = General::OrdinalDay(dataWeatherManager.SpecialDays(Loop).Month, ThisDay, dataWeatherManager.LeapYearAdd);
            } else { // LastWeekDayInMonth
                ThisDay = dataWeatherManager.SpecialDays(Loop).WeekDay - MonWeekDay(dataWeatherManager.SpecialDays(Loop).Month) + 1;
                while (ThisDay + 7 <= ActEndDayOfMonth(dataWeatherManager.SpecialDays(Loop).Month)) {
                    ThisDay += 7;
                }
                dataWeatherManager.SpecialDays(Loop).ActStMon = dataWeatherManager.SpecialDays(Loop).Month;
                dataWeatherManager.SpecialDays(Loop).ActStDay = ThisDay;
                JDay = General::OrdinalDay(dataWeatherManager.SpecialDays(Loop).Month, ThisDay, dataWeatherManager.LeapYearAdd);
            }
            if (dataWeatherManager.SpecialDayTypes(JDay) != 0) {
                ShowWarningError(RoutineName + "Special Day definition (" + dataWeatherManager.SpecialDays(Loop).Name +
                                 ") is overwriting previously entered special day period");
                if (dataWeatherManager.UseSpecialDays) {
                    ShowContinueError("...This could be caused by definitions on the Weather File.");
                }
                ShowContinueError("...This could be caused by duplicate definitions in the Input File.");
            }
            JDay1 = JDay - 1;
            for (Loop1 = 0; Loop1 <= dataWeatherManager.SpecialDays(Loop).Duration - 1; ++Loop1) {
                ++JDay1;
                if (JDay1 == 366 && dataWeatherManager.LeapYearAdd == 0) JDay1 = 1;
                if (JDay1 == 367) JDay1 = 1;
                dataWeatherManager.SpecialDayTypes(JDay1) = dataWeatherManager.SpecialDays(Loop).DayType;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Program terminates due to preceding condition(s).");
        }
    }

    void InitializeWeather(WeatherManagerData &dataWeatherManager, bool &PrintEnvrnStamp) // Set to true when the environment header should be printed
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

        int Loop;
        int FirstSimDayofYear; // Variable which tells when to skip the day in a multi year simulation.

        static bool FirstCall(true);                 // Some things should only be done once
        static bool WaterMainsParameterReport(true); // should only be done once
        //  LOGICAL, SAVE :: SetYear=.TRUE.
        int JDay5Start;
        int JDay5End;
        int TWeekDay;

        // FLOW:

        if (BeginSimFlag && FirstCall) {

            FirstCall = false;
            EndMonthFlag = false;

        } // ... end of BeginSimFlag IF-THEN block.

        if (BeginEnvrnFlag) {

            // Call and setup the Design Day environment
            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn != ksRunPeriodWeather) {
                if (dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum > 0) {
                    SetUpDesignDay(dataWeatherManager, OutputFiles::getSingleton(), dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum);
                    EnvironmentName = dataWeatherManager.Environment(dataWeatherManager.Envrn).Title;
                }
            }

            dataWeatherManager.NumMissing = 0; // Only used in Weather file environments
            // Start over missing values with each environment
            dataWeatherManager.Missing.StnPres = StdBaroPress; // Initial "missing" value
            dataWeatherManager.Missing.DryBulb = 6.0;          // Initial "missing" value
            dataWeatherManager.Missing.DewPoint = 3.0;         // Initial "missing" value
            dataWeatherManager.Missing.RelHumid = 50.0;        // Initial "missing" value
            dataWeatherManager.Missing.WindSpd = 2.5;          // Initial "missing" value
            dataWeatherManager.Missing.WindDir = 180;          // Initial "missing" value
            dataWeatherManager.Missing.TotSkyCvr = 5;          // Initial "missing" value
            dataWeatherManager.Missing.OpaqSkyCvr = 5;         // Initial "missing" value
            dataWeatherManager.Missing.Visibility = 777.7;     // Initial "missing" value
            dataWeatherManager.Missing.Ceiling = 77777;        // Initial "missing" value
            dataWeatherManager.Missing.PrecipWater = 0;        // Initial "missing" value
            dataWeatherManager.Missing.AerOptDepth = 0.0;      // Initial "missing" value
            dataWeatherManager.Missing.SnowDepth = 0;          // Initial "missing" value
            dataWeatherManager.Missing.DaysLastSnow = 88;      // Initial "missing" value
            dataWeatherManager.Missing.Albedo = 0.0;           // Initial "missing" value
            dataWeatherManager.Missing.LiquidPrecip = 0.0;     // Initial "missing" value
            // Counts set to 0 for each environment
            dataWeatherManager.Missed.StnPres = 0;
            dataWeatherManager.Missed.DryBulb = 0;
            dataWeatherManager.Missed.DewPoint = 0;
            dataWeatherManager.Missed.RelHumid = 0;
            dataWeatherManager.Missed.WindSpd = 0;
            dataWeatherManager.Missed.WindDir = 0;
            dataWeatherManager.Missed.TotSkyCvr = 0;
            dataWeatherManager.Missed.OpaqSkyCvr = 0;
            dataWeatherManager.Missed.Visibility = 0;
            dataWeatherManager.Missed.Ceiling = 0;
            dataWeatherManager.Missed.PrecipWater = 0;
            dataWeatherManager.Missed.AerOptDepth = 0;
            dataWeatherManager.Missed.SnowDepth = 0;
            dataWeatherManager.Missed.DaysLastSnow = 0;
            dataWeatherManager.Missed.Albedo = 0;
            dataWeatherManager.Missed.LiquidPrecip = 0;
            dataWeatherManager.Missed.WeathCodes = 0;
            dataWeatherManager.Missed.DirectRad = 0;
            dataWeatherManager.Missed.DiffuseRad = 0;
            // Counts set to 0 for each environment
            dataWeatherManager.OutOfRange.StnPres = 0;
            dataWeatherManager.OutOfRange.DryBulb = 0;
            dataWeatherManager.OutOfRange.DewPoint = 0;
            dataWeatherManager.OutOfRange.RelHumid = 0;
            dataWeatherManager.OutOfRange.WindSpd = 0;
            dataWeatherManager.OutOfRange.WindDir = 0;
            dataWeatherManager.OutOfRange.DirectRad = 0;
            dataWeatherManager.OutOfRange.DiffuseRad = 0;

            if (!dataWeatherManager.RPReadAllWeatherData) {
                PrintEnvrnStamp = true; // Set this to true so that on first non-warmup day (only) the environment header will print out
            }

            //    WeekDayCount=0  ! Reset weekday count (weather periods only)
            for (Loop = 1; Loop <= dataWeatherManager.NumSpecialDays; ++Loop) {
                dataWeatherManager.SpecialDays(Loop).Used = false;
            }

            if ((KindOfSim != ksDesignDay) && (KindOfSim != ksHVACSizeDesignDay)) {
                ReadWeatherForDay(dataWeatherManager, 1, dataWeatherManager.Envrn, false); // Read first day's weather
            } else {
                dataWeatherManager.TomorrowVariables = dataWeatherManager.DesignDay(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum);
            }

        } // ... end of BeginEnvrnFlag IF-THEN block.

        if (BeginDayFlag) {

            // Check Holidays, Daylight Saving Time, Ground Temperatures, etc.

            UpdateWeatherData(dataWeatherManager); // Update daily weather info

            // Read tomorrow's weather only if necessary.  This means that the
            // simulation is out of warmup, is using a weather tape for this
            // environment, and is not on the last day (day after last day is
            // assumed to be equal to last day).

            // Following code checks whether the present day of simulation matches the start month and start day.
            // In a multi year simulation with run period less than 365, we need to position the weather line
            // appropriately.

            if ((!WarmupFlag) && ((dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn != ksDesignDay) && (dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn != ksHVACSizeDesignDay))) {
                if (DayOfSim < NumOfDayInEnvrn) {
                    if (DayOfSim == dataWeatherManager.curSimDayForEndOfRunPeriod) {
                        dataWeatherManager.curSimDayForEndOfRunPeriod += dataWeatherManager.Environment(dataWeatherManager.Envrn).RawSimDays;
                        if (dataWeatherManager.StartDatesCycleShouldBeReset) {
                            ResetWeekDaysByMonth(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay,
                                                 dataWeatherManager.LeapYearAdd,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).RollDayTypeOnRepeat);
                            if (dataWeatherManager.DaylightSavingIsActive) {
                                SetDSTDateRanges(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay, dataWeatherManager.DSTIndex);
                            }
                            SetSpecialDayDates(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay);
                        }
                        ++dataWeatherManager.YearOfSim;
                        FirstSimDayofYear = 1;
                        ReadWeatherForDay(dataWeatherManager, FirstSimDayofYear, dataWeatherManager.Envrn, false); // Read tomorrow's weather
                    } else {
                        ReadWeatherForDay(dataWeatherManager, DayOfSim + 1, dataWeatherManager.Envrn, false); // Read tomorrow's weather
                    }
                }
            }

            EndYearFlag = false;
            if (DayOfMonth == dataWeatherManager.EndDayOfMonth(Month)) {
                EndMonthFlag = true;
                EndYearFlag = (Month == 12);
            }

            // Set Tomorrow's date data
            MonthTomorrow = dataWeatherManager.TomorrowVariables.Month;
            DayOfMonthTomorrow = dataWeatherManager.TomorrowVariables.DayOfMonth;
            DayOfWeekTomorrow = dataWeatherManager.TomorrowVariables.DayOfWeek;
            HolidayIndexTomorrow = dataWeatherManager.TomorrowVariables.HolidayIndex;
            YearTomorrow = dataWeatherManager.TomorrowVariables.Year;

            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn == ksRunPeriodWeather) {
                if (Month == 1 && DayOfMonth == 1 && dataWeatherManager.Environment(dataWeatherManager.Envrn).ActualWeather) {
                    if (dataWeatherManager.DatesShouldBeReset) {
                        if (dataWeatherManager.Environment(dataWeatherManager.Envrn).TreatYearsAsConsecutive) {
                            ++dataWeatherManager.Environment(dataWeatherManager.Envrn).CurrentYear;
                            dataWeatherManager.Environment(dataWeatherManager.Envrn).IsLeapYear = isLeapYear(dataWeatherManager.Environment(dataWeatherManager.Envrn).CurrentYear);
                            CurrentYearIsLeapYear = dataWeatherManager.Environment(dataWeatherManager.Envrn).IsLeapYear;
                            if (CurrentYearIsLeapYear) {
                                if (dataWeatherManager.WFAllowsLeapYears) {
                                    dataWeatherManager.LeapYearAdd = 1;
                                } else {
                                    dataWeatherManager.LeapYearAdd = 0;
                                }
                            } else {
                                dataWeatherManager.LeapYearAdd = 0;
                            }
                            // need to reset MonWeekDay and WeekDayTypes
                            if (!CurrentYearIsLeapYear) {
                                JDay5Start = General::OrdinalDay(dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay, 0);
                                JDay5End = General::OrdinalDay(dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay, 0);
                            } else {
                                JDay5Start = General::OrdinalDay(dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay, dataWeatherManager.LeapYearAdd);
                                JDay5End = General::OrdinalDay(dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth, dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay, dataWeatherManager.LeapYearAdd);
                            }
                            if (!dataWeatherManager.Environment(dataWeatherManager.Envrn).ActualWeather)
                                dataWeatherManager.curSimDayForEndOfRunPeriod = DayOfSim + dataWeatherManager.Environment(dataWeatherManager.Envrn).RawSimDays + dataWeatherManager.LeapYearAdd - 1;

                            Loop = JDay5Start;
                            TWeekDay = DayOfWeek;
                            while (true) {
                                dataWeatherManager.WeekDayTypes(Loop) = TWeekDay;
                                TWeekDay = mod(TWeekDay, 7) + 1;
                                ++Loop;
                                if (Loop > 366) Loop = 1;
                                if (Loop == JDay5End) break;
                            }
                            ResetWeekDaysByMonth(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay,
                                                 dataWeatherManager.LeapYearAdd,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).RollDayTypeOnRepeat);
                            if (dataWeatherManager.DaylightSavingIsActive) {
                                SetDSTDateRanges(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay, dataWeatherManager.DSTIndex);
                            }
                            SetSpecialDayDates(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay);
                        }
                    }
                } else if ((Month == 1 && DayOfMonth == 1) && dataWeatherManager.DatesShouldBeReset && (dataWeatherManager.Jan1DatesShouldBeReset)) {
                    if (dataWeatherManager.Environment(dataWeatherManager.Envrn).TreatYearsAsConsecutive) {
                        ++dataWeatherManager.Environment(dataWeatherManager.Envrn).CurrentYear;
                        dataWeatherManager.Environment(dataWeatherManager.Envrn).IsLeapYear = isLeapYear(dataWeatherManager.Environment(dataWeatherManager.Envrn).CurrentYear);
                        CurrentYearIsLeapYear = dataWeatherManager.Environment(dataWeatherManager.Envrn).IsLeapYear;
                        if (CurrentYearIsLeapYear && !dataWeatherManager.WFAllowsLeapYears) CurrentYearIsLeapYear = false;
                        if (DayOfSim < dataWeatherManager.curSimDayForEndOfRunPeriod && CurrentYearIsLeapYear) ++dataWeatherManager.curSimDayForEndOfRunPeriod;
                    }
                    if (CurrentYearIsLeapYear) {
                        if (dataWeatherManager.WFAllowsLeapYears) {
                            dataWeatherManager.LeapYearAdd = 1;
                        } else {
                            dataWeatherManager.LeapYearAdd = 0;
                        }
                    } else {
                        dataWeatherManager.LeapYearAdd = 0;
                    }

                    if (DayOfSim < dataWeatherManager.curSimDayForEndOfRunPeriod) {
                        if (dataWeatherManager.Environment(dataWeatherManager.Envrn).RollDayTypeOnRepeat || CurrentYearIsLeapYear) {
                            ResetWeekDaysByMonth(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay,
                                                 dataWeatherManager.LeapYearAdd,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).RollDayTypeOnRepeat,
                                                 true);
                        } else {
                            ResetWeekDaysByMonth(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay,
                                                 dataWeatherManager.LeapYearAdd,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndMonth,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).EndDay,
                                                 dataWeatherManager.Environment(dataWeatherManager.Envrn).RollDayTypeOnRepeat,
                                                 false);
                        }
                        if (dataWeatherManager.DaylightSavingIsActive) {
                            SetDSTDateRanges(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay, dataWeatherManager.DSTIndex);
                        }
                        SetSpecialDayDates(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay);
                    }
                }
                //      SetYear=.FALSE.
            }
        } // ... end of BeginDayFlag IF-THEN block.

        if (!BeginDayFlag && !WarmupFlag && (Month != dataWeatherManager.Environment(dataWeatherManager.Envrn).StartMonth || DayOfMonth != dataWeatherManager.Environment(dataWeatherManager.Envrn).StartDay) &&
            !dataWeatherManager.DatesShouldBeReset && dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn == ksRunPeriodWeather) {
            //    SetYear=.TRUE.
            dataWeatherManager.DatesShouldBeReset = true;
        }

        if (EndEnvrnFlag && (dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn != ksDesignDay) && (dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn != ksHVACSizeDesignDay)) {
            ObjexxFCL::gio::rewind(dataWeatherManager.WeatherFileUnitNumber);
            SkipEPlusWFHeader(dataWeatherManager);
            ReportMissing_RangeData(dataWeatherManager);
        }

        // set the EndDesignDayEnvrnsFlag (dataGlobals)
        // True at the end of the last design day environment (last time step of last hour of last day of environ which is a design day)
        // added to address CR7562
        EndDesignDayEnvrnsFlag = false;
        if (EndEnvrnFlag) {
            if (dataWeatherManager.Envrn < dataWeatherManager.NumOfEnvrn) {
                if (dataWeatherManager.Environment(dataWeatherManager.Envrn).KindOfEnvrn != dataWeatherManager.Environment(dataWeatherManager.Envrn + 1).KindOfEnvrn) {
                    EndDesignDayEnvrnsFlag = true;
                }
            } else {
                // if the last environment set the flag to true.
                EndDesignDayEnvrnsFlag = true;
            }
        }

        if (WaterMainsParameterReport) {
            // this is done only once
            if (dataWeatherManager.WaterMainsTempsMethod == dataWeatherManager.CorrelationFromWeatherFileMethod) {
                if (!dataWeatherManager.OADryBulbAverage.OADryBulbWeatherDataProcessed) {
                    dataWeatherManager.OADryBulbAverage.CalcAnnualAndMonthlyDryBulbTemp(dataWeatherManager);
                }
            }
            // reports to eio file
            ReportWaterMainsTempParameters(dataWeatherManager);
            WaterMainsParameterReport = false;
        }
    }

    void UpdateWeatherData(WeatherManagerData &dataWeatherManager)
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

        dataWeatherManager.TodayVariables = dataWeatherManager.TomorrowVariables; // Transfer Tomorrow's Daily Weather Variables to Today

        if (BeginEnvrnFlag) {
            PreviousHour = 24;
        }

        dataWeatherManager.TodayIsRain = dataWeatherManager.TomorrowIsRain;
        dataWeatherManager.TodayIsSnow = dataWeatherManager.TomorrowIsSnow;
        dataWeatherManager.TodayOutDryBulbTemp = dataWeatherManager.TomorrowOutDryBulbTemp;
        dataWeatherManager.TodayOutDewPointTemp = dataWeatherManager.TomorrowOutDewPointTemp;
        dataWeatherManager.TodayOutBaroPress = dataWeatherManager.TomorrowOutBaroPress;
        dataWeatherManager.TodayOutRelHum = dataWeatherManager.TomorrowOutRelHum;
        dataWeatherManager.TodayWindSpeed = dataWeatherManager.TomorrowWindSpeed;
        dataWeatherManager.TodayWindDir = dataWeatherManager.TomorrowWindDir;
        dataWeatherManager.TodaySkyTemp = dataWeatherManager.TomorrowSkyTemp;
        dataWeatherManager.TodayHorizIRSky = dataWeatherManager.TomorrowHorizIRSky;
        dataWeatherManager.TodayBeamSolarRad = dataWeatherManager.TomorrowBeamSolarRad;
        dataWeatherManager.TodayDifSolarRad = dataWeatherManager.TomorrowDifSolarRad;
        dataWeatherManager.TodayLiquidPrecip = dataWeatherManager.TomorrowLiquidPrecip;

        // Update Global Data

        DayOfYear = dataWeatherManager.TodayVariables.DayOfYear;
        Year = dataWeatherManager.TodayVariables.Year;
        Month = dataWeatherManager.TodayVariables.Month;
        DayOfMonth = dataWeatherManager.TodayVariables.DayOfMonth;
        DayOfWeek = dataWeatherManager.TodayVariables.DayOfWeek;
        //  WeekDayCount(DayOfWeek)=WeekDayCount(DayOfWeek)+1
        HolidayIndex = dataWeatherManager.TodayVariables.HolidayIndex;
        if (HolidayIndex > 0) {
            dataWeatherManager.RptDayType = 7 + HolidayIndex;
        } else {
            dataWeatherManager.RptDayType = DayOfWeek;
        }
        DSTIndicator = dataWeatherManager.TodayVariables.DaylightSavingIndex;
        EquationOfTime = dataWeatherManager.TodayVariables.EquationOfTime;
        CosSolarDeclinAngle = dataWeatherManager.TodayVariables.CosSolarDeclinAngle;
        SinSolarDeclinAngle = dataWeatherManager.TodayVariables.SinSolarDeclinAngle;
    }

    void SetCurrentWeather(WeatherManagerData &dataWeatherManager)
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
        using ScheduleManager::UpdateScheduleValues;
        using namespace GroundTemperatureManager;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static char time_stamp[10];
        static char day_stamp[6];
        static char day_year_stamp[11];
        static std::string const RoutineName("SetCurrentWeather");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static int NextHour;
        Real64 TempVal;
        Real64 TempDPVal;

        // FLOW:

        NextHour = HourOfDay + 1;

        if (HourOfDay == 24) { // Should investigate whether EndDayFlag is always set here and use that instead
            NextHour = 1;
        }

        if (HourOfDay == 1) { // Should investigate whether BeginDayFlag is always set here and use that instead
            DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
        }

        UpdateScheduleValues();

        std::sprintf(time_stamp, "%02d/%02d %02hu", Month, DayOfMonth, (unsigned short)(HourOfDay - 1));

        CurMnDyHr = time_stamp;
        std::sprintf(day_stamp, "%02d/%02d", Month, DayOfMonth);
        CurMnDy = day_stamp;
        std::sprintf(day_year_stamp, "%02d/%02d/%04d", Month, DayOfMonth, DataGlobals::CalendarYear);
        DataEnvironment::CurMnDyYr = day_year_stamp;

        WeightNow = dataWeatherManager.Interpolation(TimeStep);
        WeightPreviousHour = 1.0 - WeightNow;

        CurrentTime = (HourOfDay - 1) + TimeStep * (dataWeatherManager.TimeStepFraction);
        SimTimeSteps = (DayOfSim - 1) * 24 * NumOfTimeStepInHour + (HourOfDay - 1) * NumOfTimeStepInHour + TimeStep;

        GroundTemp = dataWeatherManager.siteBuildingSurfaceGroundTempsPtr->getGroundTempAtTimeInMonths(dataWeatherManager, 0, Month);
        GroundTempKelvin = GroundTemp + KelvinConv;
        GroundTempFC = dataWeatherManager.siteFCFactorMethodGroundTempsPtr->getGroundTempAtTimeInMonths(dataWeatherManager, 0, Month);
        GroundTemp_Surface = dataWeatherManager.siteShallowGroundTempsPtr->getGroundTempAtTimeInMonths(dataWeatherManager, 0, Month);
        GroundTemp_Deep = dataWeatherManager.siteDeepGroundTempsPtr->getGroundTempAtTimeInMonths(dataWeatherManager, 0, Month);
        GndReflectance = dataWeatherManager.GroundReflectances(Month);
        GndReflectanceForDayltg = GndReflectance;

        CalcWaterMainsTemp(dataWeatherManager);

        // Determine if Sun is up or down, set Solar Cosine values for time step.
        DetermineSunUpDown(dataWeatherManager, SOLCOS);
        if (SunIsUp && dataWeatherManager.SolarAltitudeAngle < 0.0) {
            ShowFatalError("SetCurrentWeather: At " + CurMnDyHr + " Sun is Up but Solar Altitude Angle is < 0.0");
        }

        OutDryBulbTemp = dataWeatherManager.TodayOutDryBulbTemp(TimeStep, HourOfDay);
        if (EMSOutDryBulbOverrideOn) OutDryBulbTemp = EMSOutDryBulbOverrideValue;
        OutBaroPress = dataWeatherManager.TodayOutBaroPress(TimeStep, HourOfDay);
        OutDewPointTemp = dataWeatherManager.TodayOutDewPointTemp(TimeStep, HourOfDay);
        if (EMSOutDewPointTempOverrideOn) OutDewPointTemp = EMSOutDewPointTempOverrideValue;
        OutRelHum = dataWeatherManager.TodayOutRelHum(TimeStep, HourOfDay);
        OutRelHumValue = OutRelHum / 100.0;
        if (EMSOutRelHumOverrideOn) {
            OutRelHumValue = EMSOutRelHumOverrideValue / 100.0;
            OutRelHum = EMSOutRelHumOverrideValue;
        }

        // Humidity Ratio and Wet Bulb are derived
        OutHumRat = PsyWFnTdbRhPb(OutDryBulbTemp, OutRelHumValue, OutBaroPress, RoutineName);
        OutWetBulbTemp = PsyTwbFnTdbWPb(OutDryBulbTemp, OutHumRat, OutBaroPress);
        if (OutDryBulbTemp < OutWetBulbTemp) {
            OutWetBulbTemp = OutDryBulbTemp;
            TempVal = PsyWFnTdbTwbPb(OutDryBulbTemp, OutWetBulbTemp, OutBaroPress);
            TempDPVal = PsyTdpFnWPb(TempVal, OutBaroPress);
            OutDewPointTemp = TempDPVal;
        }

        if (OutDewPointTemp > OutWetBulbTemp) {
            OutDewPointTemp = OutWetBulbTemp;
        }

        if ((KindOfSim == ksDesignDay) || (KindOfSim == ksHVACSizeDesignDay)) {
            dataWeatherManager.SPSiteDryBulbRangeModScheduleValue = -999.0;   // N/A Drybulb Temperature Range Modifier Schedule Value
            dataWeatherManager.SPSiteHumidityConditionScheduleValue = -999.0; // N/A Humidity Condition Schedule Value
            dataWeatherManager.SPSiteBeamSolarScheduleValue = -999.0;         // N/A Beam Solar Schedule Value
            dataWeatherManager.SPSiteDiffuseSolarScheduleValue = -999.0;      // N/A Diffuse Solar Schedule Value
            dataWeatherManager.SPSiteSkyTemperatureScheduleValue = -999.0;    // N/A SkyTemperature Modifier Schedule Value

            int const envrnDayNum(dataWeatherManager.Environment(dataWeatherManager.Envrn).DesignDayNum);
            if (dataWeatherManager.DesDayInput(envrnDayNum).DBTempRangeType != dataWeatherManager.DDDBRangeType_Default) {
                dataWeatherManager.SPSiteDryBulbRangeModScheduleValue(envrnDayNum) = dataWeatherManager.DDDBRngModifier(TimeStep, HourOfDay, envrnDayNum);
            }
            int const humIndType(dataWeatherManager.DesDayInput(envrnDayNum).HumIndType);
            if (humIndType == dataWeatherManager.DDHumIndType_WBProfDef || humIndType == dataWeatherManager.DDHumIndType_WBProfDif || humIndType == dataWeatherManager.DDHumIndType_WBProfMul) {
                dataWeatherManager.SPSiteHumidityConditionScheduleValue(envrnDayNum) = dataWeatherManager.DDHumIndModifier(TimeStep, HourOfDay, envrnDayNum);
            } else if (humIndType == dataWeatherManager.DDHumIndType_RelHumSch) {
                dataWeatherManager.SPSiteHumidityConditionScheduleValue(envrnDayNum) = dataWeatherManager.DDHumIndModifier(TimeStep, HourOfDay, envrnDayNum);
            }
            if (dataWeatherManager.DesDayInput(envrnDayNum).SolarModel == dataWeatherManager.SolarModel_Schedule) {
                dataWeatherManager.SPSiteBeamSolarScheduleValue(envrnDayNum) = dataWeatherManager.DDBeamSolarValues(TimeStep, HourOfDay, envrnDayNum);
                dataWeatherManager.SPSiteDiffuseSolarScheduleValue(envrnDayNum) = dataWeatherManager.DDDiffuseSolarValues(TimeStep, HourOfDay, envrnDayNum);
            }
            if (dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel <= 3 || dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel >= 1) {
                dataWeatherManager.SPSiteSkyTemperatureScheduleValue(envrnDayNum) = dataWeatherManager.DDSkyTempScheduleValues(TimeStep, HourOfDay, envrnDayNum);
            }
        } else if (TotDesDays > 0) {
            dataWeatherManager.SPSiteDryBulbRangeModScheduleValue = -999.0;   // N/A Drybulb Temperature Range Modifier Schedule Value
            dataWeatherManager.SPSiteHumidityConditionScheduleValue = -999.0; // N/A Humidity Condition Schedule Value
            dataWeatherManager.SPSiteBeamSolarScheduleValue = -999.0;         // N/A Beam Solar Schedule Value
            dataWeatherManager.SPSiteDiffuseSolarScheduleValue = -999.0;      // N/A Diffuse Solar Schedule Value
            dataWeatherManager.SPSiteSkyTemperatureScheduleValue = -999.0;    // N/A SkyTemperature Modifier Schedule Value
        }

        WindSpeed = dataWeatherManager.TodayWindSpeed(TimeStep, HourOfDay);
        if (EMSWindSpeedOverrideOn) WindSpeed = EMSWindSpeedOverrideValue;
        WindDir = dataWeatherManager.TodayWindDir(TimeStep, HourOfDay);
        if (EMSWindDirOverrideOn) WindDir = EMSWindDirOverrideValue;
        dataWeatherManager.HorizIRSky = dataWeatherManager.TodayHorizIRSky(TimeStep, HourOfDay);
        SkyTemp = dataWeatherManager.TodaySkyTemp(TimeStep, HourOfDay);
        SkyTempKelvin = SkyTemp + KelvinConv;
        DifSolarRad = dataWeatherManager.TodayDifSolarRad(TimeStep, HourOfDay);
        if (EMSDifSolarRadOverrideOn) DifSolarRad = EMSDifSolarRadOverrideValue;
        BeamSolarRad = dataWeatherManager.TodayBeamSolarRad(TimeStep, HourOfDay);
        if (EMSBeamSolarRadOverrideOn) BeamSolarRad = EMSBeamSolarRadOverrideValue;
        LiquidPrecipitation = dataWeatherManager.TodayLiquidPrecip(TimeStep, HourOfDay) / 1000.0; // convert from mm to m

        if (dataWeatherManager.UseRainValues) {
            IsRain = dataWeatherManager.TodayIsRain(TimeStep, HourOfDay); //.or. LiquidPrecipitation >= .8d0)  ! > .8 mm
        } else {
            IsRain = false;
        }
        if (dataWeatherManager.UseSnowValues) {
            IsSnow = dataWeatherManager.TodayIsSnow(TimeStep, HourOfDay);
        } else {
            IsSnow = false;
        }

        if (IsSnow) {
            GndReflectance = max(min(GndReflectance * dataWeatherManager.SnowGndRefModifier, 1.0), 0.0);
            GndReflectanceForDayltg = max(min(GndReflectanceForDayltg * dataWeatherManager.SnowGndRefModifierForDayltg, 1.0), 0.0);
        }

        GndSolarRad = max((BeamSolarRad * SOLCOS(3) + DifSolarRad) * GndReflectance, 0.0);

        if (!SunIsUp) {
            DifSolarRad = 0.0;
            BeamSolarRad = 0.0;
            GndSolarRad = 0.0;
        }

        // Calc some values
        OutEnthalpy = PsyHFnTdbW(OutDryBulbTemp, OutHumRat);
        OutAirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, OutDryBulbTemp, OutHumRat);

        // Make sure outwetbulbtemp is valid.  And that no error occurs here.
        if (OutDryBulbTemp < OutWetBulbTemp) OutWetBulbTemp = OutDryBulbTemp;

        //                                      VALIDITY TEST.
        if (OutDewPointTemp > OutWetBulbTemp) {
            OutDewPointTemp = OutWetBulbTemp;
        }
        // Get exterior daylight illuminance for daylighting calculation

        DayltgCurrentExtHorizIllum();

        if (!IsRain) {
            dataWeatherManager.RptIsRain = 0;
        } else {
            dataWeatherManager.RptIsRain = 1;
        }

        if (!IsSnow) {
            dataWeatherManager.RptIsSnow = 0;
        } else {
            dataWeatherManager.RptIsSnow = 1;
        }
    }

    void ReadWeatherForDay(WeatherManagerData &dataWeatherManager, int const DayToRead,          // =1 when starting out, otherwise signifies next day
                           int const Environ,            // Environment being simulated
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

        ReadEPlusWeatherForDay(dataWeatherManager, OutputFiles::getSingleton(), DayToRead, Environ, BackSpaceAfterRead);
    }

    void ReadEPlusWeatherForDay(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles,
                                int const DayToRead,          // =1 when starting out, otherwise signifies next day
                                int const Environ,            // Environment being simulated
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

        // Using/Aliasing
        using General::RoundSigDigits;
        using ScheduleManager::GetScheduleValuesForDay;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtA("(A)");
        static ObjexxFCL::gio::Fmt fmtLD("*");
        static ObjexxFCL::gio::Fmt YMDHFmt("(I4.4,2('/',I2.2),1X,I2.2,':',I2.2)");
        static ObjexxFCL::gio::Fmt YMDHFmt1("(I4.4,2('/',I2.2),1X,'hour=',I2.2,' - expected hour=',I2.2)");

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
        Array1D_int PresWeathConds(9);
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
        Real64 ESky;
        bool ErrorFound;
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
            Array1D_bool IsRain;             // Rain indicator, true=rain
            Array1D_bool IsSnow;             // Snow indicator, true=snow
            Array1D<Real64> OutDryBulbTemp;  // Hourly dry bulb temperature of outside air
            Array1D<Real64> OutDewPointTemp; // Hourly Dew Point Temperature of outside air
            Array1D<Real64> OutBaroPress;    // Hourly barometric pressure of outside air
            Array1D<Real64> OutRelHum;       // Hourly relative humidity
            Array1D<Real64> WindSpeed;       // Hourly wind speed of outside air
            Array1D<Real64> WindDir;         // Hourly wind direction of outside air
            Array1D<Real64> SkyTemp;         // Hourly sky temperature
            Array1D<Real64> HorizIRSky;      // Hourly Horizontal Infrared Radiation Intensity
            Array1D<Real64> BeamSolarRad;    // Hourly direct normal solar irradiance
            Array1D<Real64> DifSolarRad;     // Hourly sky diffuse horizontal solar irradiance
            Array1D<Real64> Albedo;          // Albedo
            Array1D<Real64> LiquidPrecip;    // Liquid Precipitation

            // Default Constructor
            HourlyWeatherData()
                : IsRain(24, false), IsSnow(24, false), OutDryBulbTemp(24, 0.0), OutDewPointTemp(24, 0.0), OutBaroPress(24, 0.0), OutRelHum(24, 0.0),
                  WindSpeed(24, 0.0), WindDir(24, 0.0), SkyTemp(24, 0.0), HorizIRSky(24, 0.0), BeamSolarRad(24, 0.0), DifSolarRad(24, 0.0),
                  Albedo(24, 0.0), LiquidPrecip(24, 0.0)
            {
            }
        };

        // Object Data
        HourlyWeatherData Wthr;

        if (DayToRead == 1) {

            // Checks whether Weather file contains just one year of data. If yes then rewind and position to first
            // day of weather file. The rest of code appropriately positions to the start day.

            Ready = false;
            NumRewinds = 0;
            //     Must position file to proper day
            //     File already position to first data record
            //          Set Current Day of Week to "start of Data Period"
            CurTime = 1.0 / double(dataWeatherManager.NumIntervalsPerHour);
            CurDayOfWeek = dataWeatherManager.DataPeriods(1).WeekDay - 1;
            WYear = 0;
            WMonth = 0;
            WDay = 0;
            WHour = 0;
            WMinute = 0;
            LastHourSet = false;
            while (!Ready) {
                {
                    IOFlags flags;
                    ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> WeatherDataLine;
                    ReadStatus = flags.ios();
                }
                if (ReadStatus == 0) {
                    // Reduce ugly code
                    InterpretWeatherDataLine(dataWeatherManager, WeatherDataLine,
                                             ErrorFound,
                                             WYear,
                                             WMonth,
                                             WDay,
                                             WHour,
                                             WMinute,
                                             DryBulb,
                                             DewPoint,
                                             RelHum,
                                             AtmPress,
                                             ETHoriz,
                                             ETDirect,
                                             IRHoriz,
                                             GLBHoriz,
                                             DirectRad,
                                             DiffuseRad,
                                             GLBHorizIllum,
                                             DirectNrmIllum,
                                             DiffuseHorizIllum,
                                             ZenLum,
                                             WindDir,
                                             WindSpeed,
                                             TotalSkyCover,
                                             OpaqueSkyCover,
                                             Visibility,
                                             CeilHeight,
                                             PresWeathObs,
                                             PresWeathConds,
                                             PrecipWater,
                                             AerosolOptDepth,
                                             SnowDepth,
                                             DaysSinceLastSnow,
                                             Albedo,
                                             LiquidPrecip);
                } else if (ReadStatus < 0) {
                    if (NumRewinds > 0) {
                        std::string date = std::to_string(dataWeatherManager.Environment(Environ).StartMonth) + '/' + std::to_string(dataWeatherManager.Environment(Environ).StartDay);
                        if (dataWeatherManager.Environment(Environ).MatchYear) {
                            date += '/' + std::to_string(dataWeatherManager.Environment(Environ).StartYear);
                        }
                        ShowSevereError("Multiple rewinds on EPW while searching for first day " + date);
                    } else {
                        ObjexxFCL::gio::rewind(dataWeatherManager.WeatherFileUnitNumber);
                        ++NumRewinds;
                        SkipEPlusWFHeader(dataWeatherManager);
                        {
                            IOFlags flags;
                            ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> WeatherDataLine;
                            ReadStatus = flags.ios();
                        }
                        InterpretWeatherDataLine(dataWeatherManager, WeatherDataLine,
                                                 ErrorFound,
                                                 WYear,
                                                 WMonth,
                                                 WDay,
                                                 WHour,
                                                 WMinute,
                                                 DryBulb,
                                                 DewPoint,
                                                 RelHum,
                                                 AtmPress,
                                                 ETHoriz,
                                                 ETDirect,
                                                 IRHoriz,
                                                 GLBHoriz,
                                                 DirectRad,
                                                 DiffuseRad,
                                                 GLBHorizIllum,
                                                 DirectNrmIllum,
                                                 DiffuseHorizIllum,
                                                 ZenLum,
                                                 WindDir,
                                                 WindSpeed,
                                                 TotalSkyCover,
                                                 OpaqueSkyCover,
                                                 Visibility,
                                                 CeilHeight,
                                                 PresWeathObs,
                                                 PresWeathConds,
                                                 PrecipWater,
                                                 AerosolOptDepth,
                                                 SnowDepth,
                                                 DaysSinceLastSnow,
                                                 Albedo,
                                                 LiquidPrecip);
                    }
                }
                if (ReadStatus != 0) {
                    BadRecord = RoundSigDigits(WYear) + '/' + RoundSigDigits(WMonth) + '/' + RoundSigDigits(WDay) + ' ' + RoundSigDigits(WHour) +
                                ':' + RoundSigDigits(WMinute);
                    ShowFatalError("Error occurred on EPW while searching for first day, stopped at " + BadRecord +
                                       " IO Error=" + RoundSigDigits(ReadStatus),
                                   OptionalOutputFileRef{outputFiles.eso});
                }
                if (CurDayOfWeek <= 7) {
                    CurDayOfWeek = mod(CurDayOfWeek, 7) + 1;
                }
                if (WMonth == dataWeatherManager.Environment(Environ).StartMonth && WDay == dataWeatherManager.Environment(Environ).StartDay && !dataWeatherManager.Environment(Environ).MatchYear) {
                    RecordDateMatch = true;
                } else if (WMonth == dataWeatherManager.Environment(Environ).StartMonth && WDay == dataWeatherManager.Environment(Environ).StartDay && dataWeatherManager.Environment(Environ).MatchYear &&
                           WYear == dataWeatherManager.Environment(Environ).StartYear) {
                    RecordDateMatch = true;
                } else {
                    RecordDateMatch = false;
                }
                if (RecordDateMatch) {
                    ObjexxFCL::gio::backspace(dataWeatherManager.WeatherFileUnitNumber);
                    Ready = true;
                    if (CurDayOfWeek <= 7) {
                        --CurDayOfWeek;
                    }
                    // Do the range checks on the first set of fields -- no others.
                    ErrorsFound = false;
                    if (DryBulb >= 99.9)
                        inputProcessor->rangeCheck(ErrorsFound,
                                                   "DryBulb Temperature",
                                                   "WeatherFile",
                                                   "Severe",
                                                   ">= -90",
                                                   (DryBulb >= -90.0),
                                                   "<= 70",
                                                   (DryBulb <= 70.0),
                                                   RoundSigDigits(DryBulb, 2),
                                                   WeatherFileLocationTitle);
                    if (DewPoint < 99.9)
                        inputProcessor->rangeCheck(ErrorsFound,
                                                   "DewPoint Temperature",
                                                   "WeatherFile",
                                                   "Severe",
                                                   ">= -90",
                                                   (DewPoint >= -90.0),
                                                   "<= 70",
                                                   (DewPoint <= 70.0),
                                                   RoundSigDigits(DewPoint, 2),
                                                   WeatherFileLocationTitle);
                    if (RelHum < 999.0)
                        inputProcessor->rangeCheck(ErrorsFound,
                                                   "Relative Humidity",
                                                   "WeatherFile",
                                                   "Severe",
                                                   "> 0",
                                                   (RelHum >= 0.0),
                                                   "<= 110",
                                                   (RelHum <= 110.0),
                                                   RoundSigDigits(RelHum, 0),
                                                   WeatherFileLocationTitle);
                    if (AtmPress < 999999.0)
                        inputProcessor->rangeCheck(ErrorsFound,
                                                   "Atmospheric Pressure",
                                                   "WeatherFile",
                                                   "Severe",
                                                   "> 31000",
                                                   (AtmPress > 31000.0),
                                                   "<=120000",
                                                   (AtmPress <= 120000.0),
                                                   RoundSigDigits(AtmPress, 0),
                                                   WeatherFileLocationTitle);
                    if (DirectRad < 9999.0)
                        inputProcessor->rangeCheck(
                            ErrorsFound, "Direct Radiation", "WeatherFile", "Severe", ">= 0", (DirectRad >= 0.0), _, _, _, WeatherFileLocationTitle);
                    if (DiffuseRad < 9999.0)
                        inputProcessor->rangeCheck(ErrorsFound,
                                                   "Diffuse Radiation",
                                                   "WeatherFile",
                                                   "Severe",
                                                   ">= 0",
                                                   (DiffuseRad >= 0.0),
                                                   _,
                                                   _,
                                                   _,
                                                   WeatherFileLocationTitle);
                    if (WindDir < 999.0)
                        inputProcessor->rangeCheck(ErrorsFound,
                                                   "Wind Direction",
                                                   "WeatherFile",
                                                   "Severe",
                                                   ">=0",
                                                   (WindDir >= 0.0),
                                                   "<=360",
                                                   (WindDir <= 360.0),
                                                   RoundSigDigits(WindDir, 0),
                                                   WeatherFileLocationTitle);
                    if (WindSpeed < 999.0)
                        inputProcessor->rangeCheck(ErrorsFound,
                                                   "Wind Speed",
                                                   "WeatherFile",
                                                   "Severe",
                                                   ">=0",
                                                   (WindSpeed >= 0.0),
                                                   "<=40",
                                                   (WindSpeed <= 40.0),
                                                   RoundSigDigits(WindSpeed, 2),
                                                   WeatherFileLocationTitle);
                    if (ErrorsFound) {
                        ShowSevereError("Out of Range errors found with initial day of WeatherFile");
                    }
                } else {
                    //  Must skip this day
                    for (Item = 2; Item <= dataWeatherManager.NumIntervalsPerHour; ++Item) {
                        {
                            IOFlags flags;
                            ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> WeatherDataLine;
                            ReadStatus = flags.ios();
                        }
                        if (ReadStatus != 0) {
                            ObjexxFCL::gio::read(WeatherDataLine, fmtLD) >> WYear >> WMonth >> WDay >> WHour >> WMinute;
                            BadRecord = RoundSigDigits(WYear) + '/' + RoundSigDigits(WMonth) + '/' + RoundSigDigits(WDay) + BlankString +
                                        RoundSigDigits(WHour) + ':' + RoundSigDigits(WMinute);
                            ShowFatalError("Error occurred on EPW while searching for first day, stopped at " + BadRecord +
                                               " IO Error=" + RoundSigDigits(ReadStatus),
                                           OptionalOutputFileRef{outputFiles.eso});
                        }
                    }
                    for (Item = 1; Item <= 23 * dataWeatherManager.NumIntervalsPerHour; ++Item) {
                        {
                            IOFlags flags;
                            ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> WeatherDataLine;
                            ReadStatus = flags.ios();
                        }
                        if (ReadStatus != 0) {
                            ObjexxFCL::gio::read(WeatherDataLine, fmtLD) >> WYear >> WMonth >> WDay >> WHour >> WMinute;
                            BadRecord = RoundSigDigits(WYear) + '/' + RoundSigDigits(WMonth) + '/' + RoundSigDigits(WDay) + BlankString +
                                        RoundSigDigits(WHour) + ':' + RoundSigDigits(WMinute);
                            ShowFatalError("Error occurred on EPW while searching for first day, stopped at " + BadRecord +
                                               " IO Error=" + RoundSigDigits(ReadStatus),
                                           OptionalOutputFileRef{outputFiles.eso});
                        }
                    }
                }
            }

            // Positioned to proper day
            if (!KickOffSimulation && !DoingSizing && dataWeatherManager.Environment(Environ).KindOfEnvrn == ksRunPeriodWeather) {
                ++dataWeatherManager.Environment(Environ).CurrentCycle;
                if (!dataWeatherManager.Environment(Environ).RollDayTypeOnRepeat) {
                    SetDayOfWeekInitialValues(dataWeatherManager.Environment(Environ).DayOfWeek, CurDayOfWeek, UseDayOfWeek);
                    if (dataWeatherManager.DaylightSavingIsActive) {
                        SetDSTDateRanges(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay, dataWeatherManager.DSTIndex);
                    }
                    SetSpecialDayDates(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay);
                } else if (dataWeatherManager.Environment(Environ).CurrentCycle == 1) {
                    SetDayOfWeekInitialValues(dataWeatherManager.Environment(Environ).DayOfWeek, CurDayOfWeek, UseDayOfWeek);
                    dataWeatherManager.Environment(Environ).SetWeekDays = true;
                    if (dataWeatherManager.DaylightSavingIsActive) {
                        SetDSTDateRanges(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay, dataWeatherManager.DSTIndex);
                    }
                    SetSpecialDayDates(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).MonWeekDay);
                } else {
                    CurDayOfWeek = DayOfWeekTomorrow;
                }
            } else {
                SetDayOfWeekInitialValues(dataWeatherManager.Environment(Environ).DayOfWeek, CurDayOfWeek, UseDayOfWeek);
            }
        }

        TryAgain = true;
        SkipThisDay = false;

        while (TryAgain) {

            TryAgain = false;

            dataWeatherManager.TomorrowOutDryBulbTemp = 0.0;
            dataWeatherManager.TomorrowOutDewPointTemp = 0.0;
            dataWeatherManager.TomorrowOutBaroPress = 0.0;
            dataWeatherManager.TomorrowOutRelHum = 0.0;
            dataWeatherManager.TomorrowWindSpeed = 0.0;
            dataWeatherManager.TomorrowWindDir = 0.0;
            dataWeatherManager.TomorrowSkyTemp = 0.0;
            dataWeatherManager.TomorrowHorizIRSky = 0.0;
            dataWeatherManager.TomorrowBeamSolarRad = 0.0;
            dataWeatherManager.TomorrowDifSolarRad = 0.0;
            dataWeatherManager.TomorrowAlbedo = 0.0;
            dataWeatherManager.TomorrowLiquidPrecip = 0.0;
            dataWeatherManager.TomorrowIsRain = false;
            dataWeatherManager.TomorrowIsSnow = false;

            for (Hour = 1; Hour <= 24; ++Hour) {
                for (CurTimeStep = 1; CurTimeStep <= dataWeatherManager.NumIntervalsPerHour; ++CurTimeStep) {
                    HourRep = double(Hour - 1) + (CurTime * double(CurTimeStep));
                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> WeatherDataLine;
                        ReadStatus = flags.ios();
                    }
                    if (ReadStatus != 0) WeatherDataLine = BlankString;
                    if (WeatherDataLine == BlankString) {
                        if (Hour == 1) {
                            ReadStatus = -1;
                        } else {
                            ReadStatus = 99;
                        }
                    }
                    if (ReadStatus == 0) {
                        InterpretWeatherDataLine(dataWeatherManager, WeatherDataLine,
                                                 ErrorFound,
                                                 WYear,
                                                 WMonth,
                                                 WDay,
                                                 WHour,
                                                 WMinute,
                                                 DryBulb,
                                                 DewPoint,
                                                 RelHum,
                                                 AtmPress,
                                                 ETHoriz,
                                                 ETDirect,
                                                 IRHoriz,
                                                 GLBHoriz,
                                                 DirectRad,
                                                 DiffuseRad,
                                                 GLBHorizIllum,
                                                 DirectNrmIllum,
                                                 DiffuseHorizIllum,
                                                 ZenLum,
                                                 WindDir,
                                                 WindSpeed,
                                                 TotalSkyCover,
                                                 OpaqueSkyCover,
                                                 Visibility,
                                                 CeilHeight,
                                                 PresWeathObs,
                                                 PresWeathConds,
                                                 PrecipWater,
                                                 AerosolOptDepth,
                                                 SnowDepth,
                                                 DaysSinceLastSnow,
                                                 Albedo,
                                                 LiquidPrecip);
                    } else {                                         // ReadStatus /=0
                        if (ReadStatus < 0 && dataWeatherManager.NumDataPeriods == 1) { // Standard End-of-file, rewind and position to first day...
                            if (dataWeatherManager.DataPeriods(1).NumDays >= dataWeatherManager.NumDaysInYear) {
                                ObjexxFCL::gio::rewind(dataWeatherManager.WeatherFileUnitNumber);
                                SkipEPlusWFHeader(dataWeatherManager);
                                {
                                    IOFlags flags;
                                    ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> WeatherDataLine;
                                    ReadStatus = flags.ios();
                                }

                                InterpretWeatherDataLine(dataWeatherManager, WeatherDataLine,
                                                         ErrorFound,
                                                         WYear,
                                                         WMonth,
                                                         WDay,
                                                         WHour,
                                                         WMinute,
                                                         DryBulb,
                                                         DewPoint,
                                                         RelHum,
                                                         AtmPress,
                                                         ETHoriz,
                                                         ETDirect,
                                                         IRHoriz,
                                                         GLBHoriz,
                                                         DirectRad,
                                                         DiffuseRad,
                                                         GLBHorizIllum,
                                                         DirectNrmIllum,
                                                         DiffuseHorizIllum,
                                                         ZenLum,
                                                         WindDir,
                                                         WindSpeed,
                                                         TotalSkyCover,
                                                         OpaqueSkyCover,
                                                         Visibility,
                                                         CeilHeight,
                                                         PresWeathObs,
                                                         PresWeathConds,
                                                         PrecipWater,
                                                         AerosolOptDepth,
                                                         SnowDepth,
                                                         DaysSinceLastSnow,
                                                         Albedo,
                                                         LiquidPrecip);
                            } else {
                                BadRecord = RoundSigDigits(WYear) + '/' + RoundSigDigits(WMonth) + '/' + RoundSigDigits(WDay) + BlankString +
                                            RoundSigDigits(WHour) + ':' + RoundSigDigits(WMinute);
                                ShowFatalError("End-of-File encountered after " + BadRecord +
                                               ", starting from first day of Weather File would not be \"next day\"");
                            }
                        } else {
                            BadRecord = RoundSigDigits(WYear) + '/' + RoundSigDigits(WMonth) + '/' + RoundSigDigits(WDay) + BlankString +
                                        RoundSigDigits(WHour) + ':' + RoundSigDigits(WMinute);
                            ShowFatalError("Unexpected error condition in middle of reading EPW file, stopped at " + BadRecord, OptionalOutputFileRef{outputFiles.eso});
                        }
                    }

                    if (Hour != WHour) {
                        BadRecord = RoundSigDigits(WYear) + '/' + RoundSigDigits(WMonth) + '/' + RoundSigDigits(WDay) + BlankString +
                                    RoundSigDigits(WHour) + ':' + RoundSigDigits(WMinute);
                        ShowFatalError("Unexpected error condition in middle of reading EPW file, " + BadRecord, OptionalOutputFileRef{outputFiles.eso});
                    }

                    //         Set possible missing values
                    if (ETHoriz < 0.0) ETHoriz = 9999.0;
                    if (ETDirect < 0.0) ETDirect = 9999.0;
                    if (IRHoriz <= 0.0) IRHoriz = 9999.0;
                    if (GLBHoriz < 0.0) GLBHoriz = 9999.0;
                    if (DisplayWeatherMissingDataWarnings) {
                        if (DirectRad >= 9999.0) {
                            ++dataWeatherManager.Missed.DirectRad;
                        }
                        if (DiffuseRad >= 9999.0) {
                            dataWeatherManager.Missed.DiffuseRad = dataWeatherManager.Missed.DirectRad + 1;
                        }
                        if (DirectRad < 0.0) {
                            DirectRad = 9999.0;
                            ++dataWeatherManager.OutOfRange.DirectRad;
                        }
                        if (DiffuseRad < 0.0) {
                            DiffuseRad = 9999.0;
                            ++dataWeatherManager.OutOfRange.DiffuseRad;
                        }
                    }
                    if (GLBHorizIllum < 0.0) GLBHorizIllum = 999999.0;
                    if (DirectNrmIllum < 0.0) DirectNrmIllum = 999999.0;
                    if (DiffuseHorizIllum < 0.0) DiffuseHorizIllum = 999999.0;
                    if (ZenLum < 0.0) ZenLum = 99999.0;
                    if (AtmPress < 0.0) AtmPress = 999999.0;
                    if (WindSpeed < 0.0) WindSpeed = 999.0;
                    if (WindDir < -360.0 || WindDir > 360.0) WindDir = 999.0;
                    if (TotalSkyCover < 0.0) TotalSkyCover = 99.0;
                    if (RelHum < 0.0) RelHum = 999.0;
                    if (OpaqueSkyCover < 0.0) OpaqueSkyCover = 99.0;
                    if (Visibility < 0.0) Visibility = 9999.0;
                    if (CeilHeight < 0.0) CeilHeight = 9999.0;
                    if (PresWeathObs < 0) PresWeathObs = 9.0;
                    if (PrecipWater < 0.0) PrecipWater = 999.0;
                    if (AerosolOptDepth < 0.0) AerosolOptDepth = 999.0;
                    if (SnowDepth < 0.0) SnowDepth = 999.0;
                    if (DaysSinceLastSnow < 0.0) DaysSinceLastSnow = 99.0;
                    if (Albedo < 0.0) Albedo = 999.0;
                    if (LiquidPrecip < 0.0) LiquidPrecip = 999.0;

                    if (Hour == 1 && CurTimeStep == 1) {
                        if (WMonth == 2 && WDay == 29 && (!CurrentYearIsLeapYear || !dataWeatherManager.WFAllowsLeapYears)) {
                            dataWeatherManager.EndDayOfMonth(2) = 28;
                            SkipThisDay = true;
                            TryAgain = true;
                            ShowWarningError("ReadEPlusWeatherForDay: Feb29 data encountered but will not be processed.");
                            if (!dataWeatherManager.WFAllowsLeapYears) {
                                ShowContinueError(
                                    "...WeatherFile does not allow Leap Years. HOLIDAYS/DAYLIGHT SAVINGS header must indicate \"Yes\".");
                            }
                            continue;
                        } else if (WMonth == 2 && WDay == 29 && CurrentYearIsLeapYear && dataWeatherManager.WFAllowsLeapYears) {
                            TryAgain = false;
                            SkipThisDay = false;
                        } else {
                            TryAgain = false;
                            SkipThisDay = false;
                        }

                        if (dataWeatherManager.Environment(Environ).ActualWeather && CurrentYearIsLeapYear) {
                            if (WMonth == 3 && WDay == 1 && DataEnvironment::Month == 2 && DataEnvironment::DayOfMonth == 28) {
                                ShowFatalError("ReadEPlusWeatherForDay: Current year is a leap year, but Feb29 data is missing.");
                            }
                        }

                        dataWeatherManager.TomorrowVariables.Year = WYear;
                        dataWeatherManager.TomorrowVariables.Month = WMonth;
                        dataWeatherManager.TomorrowVariables.DayOfMonth = WDay;
                        dataWeatherManager.TomorrowVariables.DayOfYear = General::OrdinalDay(WMonth, WDay, dataWeatherManager.LeapYearAdd);
                        dataWeatherManager.TomorrowVariables.DayOfYear_Schedule = General::OrdinalDay(WMonth, WDay, 1);
                        CalculateDailySolarCoeffs(dataWeatherManager.TomorrowVariables.DayOfYear,
                                                  A,
                                                  B,
                                                  C,
                                                  AVSC,
                                                  dataWeatherManager.TomorrowVariables.EquationOfTime,
                                                  dataWeatherManager.TomorrowVariables.SinSolarDeclinAngle,
                                                  dataWeatherManager.TomorrowVariables.CosSolarDeclinAngle);
                        if (CurDayOfWeek <= 7) {
                            CurDayOfWeek = mod(CurDayOfWeek, 7) + 1;
                        }
                        dataWeatherManager.TomorrowVariables.DayOfWeek = CurDayOfWeek;
                        dataWeatherManager.TomorrowVariables.DaylightSavingIndex = dataWeatherManager.DSTIndex(dataWeatherManager.TomorrowVariables.DayOfYear);
                        dataWeatherManager.TomorrowVariables.HolidayIndex = dataWeatherManager.SpecialDayTypes(dataWeatherManager.TomorrowVariables.DayOfYear);
                    }

                    if (SkipThisDay) continue;

                    // Check out missing values

                    if (DryBulb >= 99.9) {
                        DryBulb = dataWeatherManager.Missing.DryBulb;
                        ++dataWeatherManager.Missed.DryBulb;
                    }
                    if (DryBulb < -90.0 || DryBulb > 70.0) {
                        ++dataWeatherManager.OutOfRange.DryBulb;
                    }

                    if (DewPoint >= 99.9) {
                        DewPoint = dataWeatherManager.Missing.DewPoint;
                        ++dataWeatherManager.Missed.DewPoint;
                    }
                    if (DewPoint < -90.0 || DewPoint > 70.0) {
                        ++dataWeatherManager.OutOfRange.DewPoint;
                    }

                    if (RelHum >= 999.0) {
                        RelHum = dataWeatherManager.Missing.RelHumid;
                        ++dataWeatherManager.Missed.RelHumid;
                    }
                    if (RelHum < 0.0 || RelHum > 110.0) {
                        ++dataWeatherManager.OutOfRange.RelHumid;
                    }

                    if (AtmPress >= 999999.0) {
                        AtmPress = dataWeatherManager.Missing.StnPres;
                        ++dataWeatherManager.Missed.StnPres;
                    }
                    if (AtmPress <= 31000.0 || AtmPress > 120000.0) {
                        ++dataWeatherManager.OutOfRange.StnPres;
                        AtmPress = dataWeatherManager.Missing.StnPres;
                    }

                    if (WindDir >= 999.0) {
                        WindDir = dataWeatherManager.Missing.WindDir;
                        ++dataWeatherManager.Missed.WindDir;
                    }
                    if (WindDir < 0.0 || WindDir > 360.0) {
                        ++dataWeatherManager.OutOfRange.WindDir;
                    }

                    if (WindSpeed >= 999.0) {
                        WindSpeed = dataWeatherManager.Missing.WindSpd;
                        ++dataWeatherManager.Missed.WindSpd;
                    }
                    if (WindSpeed < 0.0 || WindSpeed > 40.0) {
                        ++dataWeatherManager.OutOfRange.WindSpd;
                    }

                    if (TotalSkyCover >= 99.0) {
                        TotalSkyCover = dataWeatherManager.Missing.TotSkyCvr;
                        ++dataWeatherManager.Missed.TotSkyCvr;
                    }

                    if (OpaqueSkyCover >= 99.0) {
                        OpaqueSkyCover = dataWeatherManager.Missing.OpaqSkyCvr;
                        ++dataWeatherManager.Missed.OpaqSkyCvr;
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

                    if (SnowDepth >= 999.0) {
                        SnowDepth = dataWeatherManager.Missing.SnowDepth;
                        ++dataWeatherManager.Missed.SnowDepth;
                    }

                    if (Albedo >= 999.0) {
                        Albedo = dataWeatherManager.Missing.Albedo;
                        ++dataWeatherManager.Missed.Albedo;
                    }

                    if (LiquidPrecip >= 999.0) {
                        LiquidPrecip = dataWeatherManager.Missing.LiquidPrecip;
                        ++dataWeatherManager.Missed.LiquidPrecip;
                    }

                    //        IF (DaysSinceLastSnow >= 99) THEN
                    //          DaysSinceLastSnow=Missing%DaysLastSnow
                    //          Missed%DaysLastSnow=Missed%DaysLastSnow+1
                    //        ENDIF

                    dataWeatherManager.TomorrowOutDryBulbTemp(CurTimeStep, Hour) = DryBulb;
                    dataWeatherManager.TomorrowOutDewPointTemp(CurTimeStep, Hour) = DewPoint;
                    dataWeatherManager.TomorrowOutBaroPress(CurTimeStep, Hour) = AtmPress;
                    dataWeatherManager.TomorrowOutRelHum(CurTimeStep, Hour) = RelHum;
                    RelHum *= 0.01;
                    dataWeatherManager.TomorrowWindSpeed(CurTimeStep, Hour) = WindSpeed;
                    dataWeatherManager.TomorrowWindDir(CurTimeStep, Hour) = WindDir;
                    dataWeatherManager.TomorrowLiquidPrecip(CurTimeStep, Hour) = LiquidPrecip;

                    ESky = CalcSkyEmissivity(dataWeatherManager, dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel, OpaqueSkyCover, DryBulb, DewPoint, RelHum);
                    if (!dataWeatherManager.Environment(dataWeatherManager.Envrn).UseWeatherFileHorizontalIR || IRHoriz >= 9999.0) {
                        dataWeatherManager.TomorrowHorizIRSky(CurTimeStep, Hour) = ESky * dataWeatherManager.Sigma * pow_4(DryBulb + dataWeatherManager.TKelvin);
                    } else {
                        dataWeatherManager.TomorrowHorizIRSky(CurTimeStep, Hour) = IRHoriz;
                    }

                    if (dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel > 3 || dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel == 0) {
                        // Calculate sky temperature, use IRHoriz if not missing
                        if (!dataWeatherManager.Environment(dataWeatherManager.Envrn).UseWeatherFileHorizontalIR || IRHoriz >= 9999.0) {
                            // Missing or user defined to not use IRHoriz from weather, using sky cover and clear sky emissivity
                            SkyTemp = (DryBulb + dataWeatherManager.TKelvin) * root_4(ESky) - dataWeatherManager.TKelvin;
                        } else {
                            // Valid IR from weather files
                            SkyTemp = root_4(IRHoriz / dataWeatherManager.Sigma) - dataWeatherManager.TKelvin;
                        }
                    } else {
                        SkyTemp = 0.0; // dealt with later
                    }

                    dataWeatherManager.TomorrowSkyTemp(CurTimeStep, Hour) = SkyTemp;

                    if (ETHoriz >= 9999.0) ETHoriz = 0.0;
                    if (ETDirect >= 9999.0) ETDirect = 0.0;
                    if (GLBHoriz >= 9999.0) GLBHoriz = 0.0;
                    if (DirectRad >= 9999.0) DirectRad = 0.0;
                    if (DiffuseRad >= 9999.0) DiffuseRad = 0.0;
                    if (GLBHorizIllum >= 999900.0) GLBHorizIllum = 0.0;
                    if (DirectNrmIllum >= 999900.0) DirectNrmIllum = 0.0;
                    if (DiffuseHorizIllum >= 999900.0) DiffuseHorizIllum = 0.0;
                    if (ZenLum >= 99990.0) ZenLum = 0.0;
                    if (IgnoreSolarRadiation) {
                        GLBHoriz = 0.0;
                        DirectRad = 0.0;
                        DiffuseRad = 0.0;
                    }
                    if (IgnoreBeamRadiation) {
                        DirectRad = 0.0;
                    }
                    if (IgnoreDiffuseRadiation) {
                        DiffuseRad = 0.0;
                    }

                    dataWeatherManager.TomorrowBeamSolarRad(CurTimeStep, Hour) = DirectRad;
                    dataWeatherManager.TomorrowDifSolarRad(CurTimeStep, Hour) = DiffuseRad;

                    dataWeatherManager.TomorrowIsRain(CurTimeStep, Hour) = false;
                    if (PresWeathObs == 0) {
                        if (PresWeathConds(1) < 9 || PresWeathConds(2) < 9 || PresWeathConds(3) < 9) dataWeatherManager.TomorrowIsRain(CurTimeStep, Hour) = true;
                    } else {
                        dataWeatherManager.TomorrowIsRain(CurTimeStep, Hour) = false;
                    }
                    dataWeatherManager.TomorrowIsSnow(CurTimeStep, Hour) = (SnowDepth > 0.0);

                    // default if rain but none on weather file
                    if (dataWeatherManager.TomorrowIsRain(CurTimeStep, Hour) && dataWeatherManager.TomorrowLiquidPrecip(CurTimeStep, Hour) == 0.0)
                        dataWeatherManager.TomorrowLiquidPrecip(CurTimeStep, Hour) = 2.0; // 2mm in an hour ~ .08 inch

                    dataWeatherManager.Missing.DryBulb = DryBulb;
                    dataWeatherManager.Missing.DewPoint = DewPoint;
                    dataWeatherManager.Missing.RelHumid = RelHum * 100.0;
                    dataWeatherManager.Missing.StnPres = AtmPress;
                    dataWeatherManager.Missing.WindDir = WindDir;
                    dataWeatherManager.Missing.WindSpd = WindSpeed;
                    dataWeatherManager.Missing.TotSkyCvr = TotalSkyCover;
                    dataWeatherManager.Missing.OpaqSkyCvr = OpaqueSkyCover;
                    dataWeatherManager.Missing.Visibility = Visibility;
                    dataWeatherManager.Missing.Ceiling = CeilHeight;
                    dataWeatherManager.Missing.PrecipWater = PrecipWater;
                    dataWeatherManager.Missing.AerOptDepth = AerosolOptDepth;
                    dataWeatherManager.Missing.SnowDepth = SnowDepth;
                    dataWeatherManager.Missing.DaysLastSnow = DaysSinceLastSnow;
                    dataWeatherManager.Missing.Albedo = Albedo;
                    //        Missing%LiquidPrecip=LiquidPrecip

                } // CurTimeStep Loop

            } // Hour Loop

        } // Try Again While Loop

        if (BackSpaceAfterRead) {
            ObjexxFCL::gio::backspace(dataWeatherManager.WeatherFileUnitNumber);
        }

        if (dataWeatherManager.NumIntervalsPerHour == 1 && NumOfTimeStepInHour > 1) {
            // Create interpolated weather for timestep orientation
            // First copy ts=1 (hourly) from data arrays to Wthr structure
            for (Hour = 1; Hour <= 24; ++Hour) {
                Wthr.OutDryBulbTemp(Hour) = dataWeatherManager.TomorrowOutDryBulbTemp(1, Hour);
                Wthr.OutDewPointTemp(Hour) = dataWeatherManager.TomorrowOutDewPointTemp(1, Hour);
                Wthr.OutBaroPress(Hour) = dataWeatherManager.TomorrowOutBaroPress(1, Hour);
                Wthr.OutRelHum(Hour) = dataWeatherManager.TomorrowOutRelHum(1, Hour);
                Wthr.WindSpeed(Hour) = dataWeatherManager.TomorrowWindSpeed(1, Hour);
                Wthr.WindDir(Hour) = dataWeatherManager.TomorrowWindDir(1, Hour);
                Wthr.SkyTemp(Hour) = dataWeatherManager.TomorrowSkyTemp(1, Hour);
                Wthr.HorizIRSky(Hour) = dataWeatherManager.TomorrowHorizIRSky(1, Hour);
                Wthr.BeamSolarRad(Hour) = dataWeatherManager.TomorrowBeamSolarRad(1, Hour);
                Wthr.DifSolarRad(Hour) = dataWeatherManager.TomorrowDifSolarRad(1, Hour);
                Wthr.IsRain(Hour) = dataWeatherManager.TomorrowIsRain(1, Hour);
                Wthr.IsSnow(Hour) = dataWeatherManager.TomorrowIsSnow(1, Hour);
                Wthr.Albedo(Hour) = dataWeatherManager.TomorrowAlbedo(1, Hour);
                Wthr.LiquidPrecip(Hour) = dataWeatherManager.TomorrowLiquidPrecip(1, Hour);
            }

            if (!LastHourSet) {
                // For first day of weather, all time steps of the first hour will be
                // equal to the first hour's value.
                LastHrOutDryBulbTemp = Wthr.OutDryBulbTemp(24);
                LastHrOutDewPointTemp = Wthr.OutDewPointTemp(24);
                LastHrOutBaroPress = Wthr.OutBaroPress(24);
                LastHrOutRelHum = Wthr.OutRelHum(24);
                LastHrWindSpeed = Wthr.WindSpeed(24);
                LastHrWindDir = Wthr.WindDir(24);
                LastHrSkyTemp = Wthr.SkyTemp(24);
                LastHrHorizIRSky = Wthr.HorizIRSky(24);
                LastHrBeamSolarRad = Wthr.BeamSolarRad(24);
                LastHrDifSolarRad = Wthr.DifSolarRad(24);
                LastHrAlbedo = Wthr.Albedo(24);
                LastHrLiquidPrecip = Wthr.LiquidPrecip(24);
                LastHourSet = true;
            }

            for (Hour = 1; Hour <= 24; ++Hour) {

                NxtHour = Hour + 1;
                if (Hour == 24) {
                    NxtHour = 1;
                }
                NextHrBeamSolarRad = Wthr.BeamSolarRad(NxtHour);
                NextHrDifSolarRad = Wthr.DifSolarRad(NxtHour);
                NextHrLiquidPrecip = Wthr.LiquidPrecip(NxtHour);

                for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {

                    WtNow = dataWeatherManager.Interpolation(TS);
                    WtPrevHour = 1.0 - WtNow;

                    // Do Solar "weighting"

                    WgtHourNow = dataWeatherManager.SolarInterpolation(TS);

                    if (NumOfTimeStepInHour == 1) {
                        WgtNextHour = 1.0 - WgtHourNow;
                        WgtPrevHour = 0.0;
                    } else {
                        if (WgtHourNow == 1.0) {
                            //  It's at the half hour
                            WgtNextHour = 0.0;
                            WgtPrevHour = 0.0;
                        } else if (TS * dataWeatherManager.TimeStepFraction < 0.5) {
                            WgtNextHour = 0.0;
                            WgtPrevHour = 1.0 - WgtHourNow;
                        } else { // After the half hour
                            WgtPrevHour = 0.0;
                            WgtNextHour = 1.0 - WgtHourNow;
                        }
                    }

                    dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour) = LastHrOutDryBulbTemp * WtPrevHour + Wthr.OutDryBulbTemp(Hour) * WtNow;
                    dataWeatherManager.TomorrowOutBaroPress(TS, Hour) = LastHrOutBaroPress * WtPrevHour + Wthr.OutBaroPress(Hour) * WtNow;
                    dataWeatherManager.TomorrowOutDewPointTemp(TS, Hour) = LastHrOutDewPointTemp * WtPrevHour + Wthr.OutDewPointTemp(Hour) * WtNow;
                    dataWeatherManager.TomorrowOutRelHum(TS, Hour) = LastHrOutRelHum * WtPrevHour + Wthr.OutRelHum(Hour) * WtNow;
                    dataWeatherManager.TomorrowWindSpeed(TS, Hour) = LastHrWindSpeed * WtPrevHour + Wthr.WindSpeed(Hour) * WtNow;
                    dataWeatherManager.TomorrowWindDir(TS, Hour) = interpolateWindDirection(LastHrWindDir, Wthr.WindDir(Hour), WtNow);
                    dataWeatherManager.TomorrowHorizIRSky(TS, Hour) = LastHrHorizIRSky * WtPrevHour + Wthr.HorizIRSky(Hour) * WtNow;
                    if (dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel > 3 || dataWeatherManager.Environment(dataWeatherManager.Envrn).SkyTempModel == 0) {
                        dataWeatherManager.TomorrowSkyTemp(TS, Hour) = LastHrSkyTemp * WtPrevHour + Wthr.SkyTemp(Hour) * WtNow;
                    }
                    dataWeatherManager.TomorrowDifSolarRad(TS, Hour) =
                        LastHrDifSolarRad * WgtPrevHour + Wthr.DifSolarRad(Hour) * WgtHourNow + NextHrDifSolarRad * WgtNextHour;
                    dataWeatherManager.TomorrowBeamSolarRad(TS, Hour) =
                        LastHrBeamSolarRad * WgtPrevHour + Wthr.BeamSolarRad(Hour) * WgtHourNow + NextHrBeamSolarRad * WgtNextHour;

                    dataWeatherManager.TomorrowLiquidPrecip(TS, Hour) = LastHrLiquidPrecip * WtPrevHour + Wthr.LiquidPrecip(Hour) * WtNow;
                    dataWeatherManager.TomorrowLiquidPrecip(TS, Hour) /= double(NumOfTimeStepInHour);

                    dataWeatherManager.TomorrowIsRain(TS, Hour) = dataWeatherManager.TomorrowLiquidPrecip(TS, Hour) >= (0.8 / double(NumOfTimeStepInHour)); // Wthr%IsRain(Hour)
                    dataWeatherManager.TomorrowIsSnow(TS, Hour) = Wthr.IsSnow(Hour);
                } // End of TS Loop

                LastHrOutDryBulbTemp = Wthr.OutDryBulbTemp(Hour);
                LastHrOutDewPointTemp = Wthr.OutDewPointTemp(Hour);
                LastHrOutBaroPress = Wthr.OutBaroPress(Hour);
                LastHrOutRelHum = Wthr.OutRelHum(Hour);
                LastHrWindSpeed = Wthr.WindSpeed(Hour);
                LastHrWindDir = Wthr.WindDir(Hour);
                LastHrHorizIRSky = Wthr.HorizIRSky(Hour);
                LastHrSkyTemp = Wthr.SkyTemp(Hour);
                LastHrBeamSolarRad = Wthr.BeamSolarRad(Hour);
                LastHrDifSolarRad = Wthr.DifSolarRad(Hour);
                LastHrAlbedo = Wthr.Albedo(Hour);
                LastHrLiquidPrecip = Wthr.LiquidPrecip(Hour);

            } // End of Hour Loop
        }

        if (dataWeatherManager.Environment(Environ).WP_Type1 != 0) {
            {
                auto const SELECT_CASE_var(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(Environ).WP_Type1).CalculationType);

                if (SELECT_CASE_var == dataWeatherManager.WP_ScheduleValue) {
                    GetScheduleValuesForDay(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(Environ).WP_Type1).SchedulePtr,
                                            dataWeatherManager.TomorrowSkyTemp,
                                            dataWeatherManager.TomorrowVariables.DayOfYear_Schedule,
                                            CurDayOfWeek);
                } else if (SELECT_CASE_var == dataWeatherManager.WP_DryBulbDelta) {
                    GetScheduleValuesForDay(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(Environ).WP_Type1).SchedulePtr,
                                            dataWeatherManager.TomorrowSkyTemp,
                                            dataWeatherManager.TomorrowVariables.DayOfYear_Schedule,
                                            CurDayOfWeek);
                    for (Hour = 1; Hour <= 24; ++Hour) {
                        for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                            dataWeatherManager.TomorrowSkyTemp(TS, Hour) = dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour) - dataWeatherManager.TomorrowSkyTemp(TS, Hour);
                        }
                    }

                } else if (SELECT_CASE_var == dataWeatherManager.WP_DewPointDelta) {
                    GetScheduleValuesForDay(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(Environ).WP_Type1).SchedulePtr,
                                            dataWeatherManager.TomorrowSkyTemp,
                                            dataWeatherManager.TomorrowVariables.DayOfYear_Schedule,
                                            CurDayOfWeek);
                    for (Hour = 1; Hour <= 24; ++Hour) {
                        for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                            dataWeatherManager.TomorrowSkyTemp(TS, Hour) = dataWeatherManager.TomorrowOutDewPointTemp(TS, Hour) - dataWeatherManager.TomorrowSkyTemp(TS, Hour);
                        }
                    }

                } else {
                }
            }
        }
    }

    Real64 interpolateWindDirection(Real64 const prevHrWindDir, Real64 const curHrWindDir, Real64 const curHrWeight)
    {
        // adapted from http://stackoverflow.com/questions/2708476/rotation-interpolation
        Real64 curAng = curHrWindDir;
        Real64 prevAng = prevHrWindDir;
        Real64 diff = std::abs(curAng - prevAng);
        if (diff > 180.) {
            if (curAng > prevAng) {
                prevAng += 360.;
            } else {
                curAng += 360.;
            }
        }
        Real64 interpAng = prevAng + (curAng - prevAng) * curHrWeight;
        return (fmod(interpAng, 360.)); // fmod is float modulus function
    }

    Real64 CalcSkyEmissivity(WeatherManagerData &dataWeatherManager, int ESkyCalcType, Real64 OSky, Real64 DryBulb, Real64 DewPoint, Real64 RelHum){
        // Calculate Sky Emissivity
        // References:
        // M. Li, Y. Jiang and C. F. M. Coimbra,
        // "On the determination of atmospheric longwave irradiance under all-sky conditions,"
        // Solar Energy 144, 2017, pp. 40–48,
        // G. Clark and C. Allen, "The Estimation of Atmospheric Radiation for Clear and
        // Cloudy Skies," Proc. 2nd National Passive Solar Conference (AS/ISES), 1978, pp. 675-678.

        Real64 ESky;

        if (ESkyCalcType == dataWeatherManager.WP_BruntModel) {
            double const PartialPress = RelHum * PsyPsatFnTemp(DryBulb) * 0.01;
            ESky = 0.618 + 0.056 * pow(PartialPress, 0.5);
        } else if (ESkyCalcType == dataWeatherManager.WP_IdsoModel) {
            double const PartialPress = RelHum * PsyPsatFnTemp(DryBulb) * 0.01;
            ESky = 0.685 + 0.000032 * PartialPress * exp(1699 / (DryBulb + dataWeatherManager.TKelvin));
        } else if (ESkyCalcType == dataWeatherManager.WP_BerdahlMartinModel) {
            double const TDewC = min(DryBulb, DewPoint);
            ESky = 0.758 + 0.521 * (TDewC / 100) + 0.625 * pow_2(TDewC / 100);
        } else {
            ESky = 0.787 + 0.764 * std::log((min(DryBulb, DewPoint) + dataWeatherManager.TKelvin) / dataWeatherManager.TKelvin);
        }
        ESky = ESky * (1.0 + 0.0224 * OSky - 0.0035 * pow_2(OSky) + 0.00028 * pow_3(OSky));
        return ESky;
    }


    void SetDayOfWeekInitialValues(int const EnvironDayOfWeek, // Starting Day of Week for the (Weather) RunPeriod (User Input)
                                   int &CurDayOfWeek,          // Current Day of Week
                                   bool &UseDayOfWeek          // hmmm does not appear to be used anywhere.
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

        if (EnvironDayOfWeek != 0) {
            if (EnvironDayOfWeek <= 7) {
                CurDayOfWeek = EnvironDayOfWeek - 1;
            } else {
                CurDayOfWeek = EnvironDayOfWeek;
            }
            UseDayOfWeek = false;
        } else {
            UseDayOfWeek = true;
        }
    }

    void InterpretWeatherDataLine(WeatherManagerData &dataWeatherManager, std::string &Line,
                                  bool &ErrorFound, // True if an error is found, false otherwise
                                  int &WYear,
                                  int &WMonth,
                                  int &WDay,
                                  int &WHour,
                                  int &WMinute,
                                  Real64 &RField1,       // DryBulb
                                  Real64 &RField2,       // DewPoint
                                  Real64 &RField3,       // RelHum
                                  Real64 &RField4,       // AtmPress
                                  Real64 &RField5,       // ETHoriz
                                  Real64 &RField6,       // ETDirect
                                  Real64 &RField7,       // IRHoriz
                                  Real64 &RField8,       // GLBHoriz
                                  Real64 &RField9,       // DirectRad
                                  Real64 &RField10,      // DiffuseRad
                                  Real64 &RField11,      // GLBHorizIllum
                                  Real64 &RField12,      // DirectNrmIllum
                                  Real64 &RField13,      // DiffuseHorizIllum
                                  Real64 &RField14,      // ZenLum
                                  Real64 &RField15,      // WindDir
                                  Real64 &RField16,      // WindSpeed
                                  Real64 &RField17,      // TotalSkyCover
                                  Real64 &RField18,      // OpaqueSkyCover
                                  Real64 &RField19,      // Visibility
                                  Real64 &RField20,      // CeilHeight
                                  int &WObs,             // PresWeathObs
                                  Array1D_int &WCodesArr, // PresWeathConds
                                  Real64 &RField22,      // PrecipWater
                                  Real64 &RField23,      // AerosolOptDepth
                                  Real64 &RField24,      // SnowDepth
                                  Real64 &RField25,      // DaysSinceLastSnow
                                  Real64 &RField26,      // Albedo
                                  Real64 &RField27       // LiquidPrecip
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

        // Argument array dimensioning
        EP_SIZE_CHECK(WCodesArr, 9);

        static std::string const ValidDigits("0123456789");
        static ObjexxFCL::gio::Fmt fmtLD("*");
        static ObjexxFCL::gio::Fmt fmt9I1("(9I1)");

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
        static int LCount(0);
        bool DateInError;

        ++LCount;
        ErrorFound = false;
        std::string const SaveLine = Line; // in case of errors

        // Do the first five.  (To get to the DataSource field)
        {
            IOFlags flags;
            ObjexxFCL::gio::read(Line, fmtLD, flags) >> RYear >> RMonth >> RDay >> RHour >> RMinute;
            if (flags.err()) goto Label900;
        }
        WYear = nint(RYear);
        WMonth = nint(RMonth);
        WDay = nint(RDay);
        WHour = nint(RHour);
        WMinute = nint(RMinute);

        DateInError = false;
        if (WMonth >= 1 && WMonth <= 12) {
            // Month number is valid
            if (WMonth != 2) {
                if (WDay > dataWeatherManager.EndDayOfMonth(WMonth)) {
                    DateInError = true;
                }
            } else if (WDay > dataWeatherManager.EndDayOfMonth(WMonth) + 1) { // Whether actually used is determined by calling routine.
                DateInError = true;
            }
        } else {
            DateInError = true;
        }

        if (DateInError) {
            ShowSevereError("Reading Weather Data Line, Invalid Date, Year=" + RoundSigDigits(WYear) + ", Month=" + RoundSigDigits(WMonth) +
                            ", Day=" + RoundSigDigits(WDay));
            ShowFatalError("Program terminates due to previous condition.");
        }

        Pos = index(Line, ','); // WYear
        if (Pos == std::string::npos) goto Label902;
        Line.erase(0, Pos + 1);
        Pos = index(Line, ','); // WMonth
        Line.erase(0, Pos + 1);
        Pos = index(Line, ','); // WDay
        Line.erase(0, Pos + 1);
        Pos = index(Line, ','); // WHour
        Line.erase(0, Pos + 1);
        Pos = index(Line, ','); // WMinute
        Line.erase(0, Pos + 1);

        // Data Source/Integrity field -- ignore
        Pos = index(Line, ',');
        Line.erase(0, Pos + 1);

        // Now read more numerics with List Directed I/O (note there is another "character" field lurking)
        {
            IOFlags flags;
            ObjexxFCL::gio::read(Line, fmtLD, flags) >> RField1 >> RField2 >> RField3 >> RField4 >> RField5 >> RField6 >> RField7 >> RField8 >>
                RField9 >> RField10 >> RField11 >> RField12 >> RField13 >> RField14 >> RField15 >> RField16 >> RField17 >> RField18 >> RField19 >>
                RField20 >> RField21;
            if (flags.err()) goto Label901;
        }
        for (Count = 1; Count <= 21; ++Count) {
            Pos = index(Line, ',');
            Line.erase(0, Pos + 1);
        }
        Pos = index(Line, ',');
        if (Pos != std::string::npos && Pos != 0) {
            PresWeathCodes = Line.substr(0, Pos);
        } else {
            PresWeathCodes = "999999999";
        }
        Line.erase(0, Pos + 1);
        Pos = index(Line, ',');
        if (Pos != std::string::npos) {
            if (Pos != 0) {
                {
                    IOFlags flags;
                    ObjexxFCL::gio::read(Line.substr(0, Pos), fmtLD, flags) >> RField22;
                    if (flags.err()) goto Label901;
                }
            } else {
                RField22 = 999.0;
            }
            Line.erase(0, Pos + 1);
            Pos = index(Line, ',');
            if (Pos != std::string::npos) {
                if (Pos != 0) {
                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(Line.substr(0, Pos), fmtLD, flags) >> RField23;
                        if (flags.err()) goto Label901;
                    }
                } else {
                    RField23 = 999.0;
                }
                Line.erase(0, Pos + 1);
                Pos = index(Line, ',');
                if (Pos != std::string::npos) {
                    if (Pos != 0) {
                        {
                            IOFlags flags;
                            ObjexxFCL::gio::read(Line.substr(0, Pos), fmtLD, flags) >> RField24;
                            if (flags.err()) goto Label901;
                        }
                    } else {
                        RField24 = 999.0;
                    }
                    Line.erase(0, Pos + 1);
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        if (Pos != 0) {
                            {
                                IOFlags flags;
                                ObjexxFCL::gio::read(Line.substr(0, Pos), fmtLD, flags) >> RField25;
                                if (flags.err()) goto Label901;
                            }
                        } else {
                            RField25 = 999.0;
                        }
                        Line.erase(0, Pos + 1);
                        Pos = index(Line, ',');
                        if (Pos != std::string::npos) {
                            if (Pos != 0) {
                                {
                                    IOFlags flags;
                                    ObjexxFCL::gio::read(Line.substr(0, Pos), fmtLD, flags) >> RField26;
                                    if (flags.err()) goto Label901;
                                }
                            } else {
                                RField26 = 999.0;
                            }
                            Line.erase(0, Pos + 1);
                            Pos = index(Line, ',');
                            if (Pos != std::string::npos) {
                                if (Pos != 0) {
                                    {
                                        IOFlags flags;
                                        ObjexxFCL::gio::read(Line.substr(0, Pos), fmtLD, flags) >> RField27;
                                        if (flags.err()) goto Label901;
                                    }
                                } else {
                                    RField27 = 999.0;
                                }
                                Line.erase(0, Pos + 1);
                                Pos = index(Line, ',');
                            } else {
                                RField27 = 999.0;
                            }
                        } else {
                            RField26 = 999.0;
                            RField27 = 999.0;
                        }
                    } else {
                        {
                            IOFlags flags;
                            ObjexxFCL::gio::read(Line, fmtLD, flags) >> RField25;
                            if (flags.err()) goto Label901;
                        }
                        RField26 = 999.0;
                        RField27 = 999.0;
                    }
                } else {
                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(Line, fmtLD, flags) >> RField24;
                        if (flags.err()) goto Label901;
                    }
                    RField25 = 999.0;
                    RField26 = 999.0;
                    RField27 = 999.0;
                }
            } else {
                {
                    IOFlags flags;
                    ObjexxFCL::gio::read(Line, fmtLD, flags) >> RField23;
                    if (flags.err()) goto Label901;
                }
                RField24 = 999.0;
                RField25 = 999.0;
                RField26 = 999.0;
                RField27 = 999.0;
            }
        } else {
            {
                IOFlags flags;
                ObjexxFCL::gio::read(Line, fmtLD, flags) >> RField22;
                if (flags.err()) goto Label901;
            }
            RField23 = 999.0;
            RField24 = 999.0;
            RField25 = 999.0;
            RField26 = 999.0;
            RField27 = 999.0;
        }
        //  READ(Line,*,err=903,end=903) RField22,RField23,RField24,RField25

        WObs = nint(RField21);
        if (WObs == 0) { // Obs Indicator indicates Weather Codes valid
            // Check for miscellaneous characters
            Pos = index(PresWeathCodes, '\'');
            while (Pos != std::string::npos) {
                PresWeathCodes[Pos] = ' ';
                Pos = index(PresWeathCodes, '\'');
            }
            Pos = index(PresWeathCodes, '"');
            while (Pos != std::string::npos) {
                PresWeathCodes[Pos] = ' ';
                Pos = index(PresWeathCodes, '"');
            }
            strip(PresWeathCodes);
            if (len(PresWeathCodes) == 9) {
                for (Pos = 0; Pos < 9; ++Pos) {
                    if (!has(ValidDigits, PresWeathCodes[Pos])) PresWeathCodes[Pos] = '9';
                }
                ObjexxFCL::gio::read(PresWeathCodes, fmt9I1) >> WCodesArr;
            } else {
                ++dataWeatherManager.Missed.WeathCodes;
                WCodesArr = 9;
            }
        } else {
            WCodesArr = 9;
        }

        return;

    Label900:;
        ShowSevereError("Invalid Date info in Weather Line");
        ShowContinueError("Entire Data Line=" + SaveLine);
        ShowFatalError("Error in Reading Weather Data");

    Label901:;
        ShowSevereError(format("Invalid Weather Line at date={:4}/{:2}/{:2} Hour#={:2} Min#={:2}", WYear, WMonth, WDay, WHour, WMinute));
        ShowContinueError("Full Data Line=" + SaveLine);
        ShowContinueError("Remainder of line=" + Line);
        ShowFatalError("Error in Reading Weather Data");

    Label902:;
        ShowSevereError(format("Invalid Weather Line (no commas) at date={:4}/{:2}/{:2} Hour#={:2} Min#={:2}", WYear, WMonth, WDay, WHour, WMinute));
        ShowContinueError("Full Data Line=" + SaveLine);
        ShowContinueError("Remainder of line=" + Line);
        ShowFatalError("Error in Reading Weather Data");

        // Label903: ;
        //		gio::write( DateError, "(I4,'/',I2,'/',I2,' Hour#=',I2,' Min#=',I2)" ) << WYear << WMonth << WDay << WHour << WMinute;
        //		ShowSevereError( "Invalid Weather Line at date=" + DateError );
        //		ShowContinueError( "Full Data Line=" + SaveLine );
        //		ShowContinueError( "Partial line read; Remainder of line=" + Line );
        //		ShowFatalError( "Error in Reading Weather Data" );
    }

    void SetUpDesignDay(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, int const EnvrnNum) // Environment number passed into the routine
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
        using ScheduleManager::GetSingleDayScheduleValues;

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 GlobalSolarConstant(1367.0);
        constexpr Real64 ZHGlobalSolarConstant(1355.0);



        Real64 const ZhangHuangModCoeff_C0(0.5598);   // 37.6865d0
        Real64 const ZhangHuangModCoeff_C1(0.4982);   // 13.9263d0
        Real64 const ZhangHuangModCoeff_C2(-0.6762);  // -20.2354d0
        Real64 const ZhangHuangModCoeff_C3(0.02842);  // 0.9695d0
        Real64 const ZhangHuangModCoeff_C4(-0.00317); // -0.2046d0
        Real64 const ZhangHuangModCoeff_C5(0.014);    // -0.0980d0
        Real64 const ZhangHuangModCoeff_D(-17.853);   // -10.8568d0
        Real64 const ZhangHuangModCoeff_K(0.843);     // 49.3112d0
        static std::string const RoutineNamePsyWFnTdbTwbPb("SetUpDesignDay:PsyWFnTdbTwbPb");
        static std::string const RoutineNamePsyWFnTdpPb("SetUpDesignDay:PsyWFnTdpPb");
        static std::string const RoutineNamePsyWFnTdbH("SetUpDesignDay:PsyWFnTdbH");
        static std::string const WeatherManager("WeatherManager");
        static std::string const RoutineNameLong("WeatherManager.cc subroutine SetUpDesignDay");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Hour;
        int TS;
        Real64 A;                  // Apparent solar irradiation at air mass = 0
        Real64 AVSC;               // Annual variation in the solar constant
        Real64 B;                  // Atmospheric extinction coefficient
        Real64 C;                  // ASHRAE diffuse radiation factor
        Real64 ETR;                // radiation of an extraterrestrial normal surface, W/m2
        Real64 HO;                 // Radiation on an extraterrestial horizontal surface
        Real64 KT;                 // Radiation ratio
        Array1D<Real64> SUNCOS(3); // Sun direction cosines
        int CurrentYear;
        int OSky;             // Opaque Sky Cover (tenths)
        Real64 HumidityRatio; // Humidity Ratio -- when constant for day
        Real64 ESky;          // Emissivitity of Sky
        Real64 CosZenith;     // Cosine of Zenith Angle of Sun
        Real64 TotHoriz;      // Total Radiation on Horizontal Surface
        Real64 GndReflet;     // Ground Reflectivity
        Real64 CurTime;       // For Solar Calcs
        Real64 WetBulb;       // For calculating
        Real64 DBRange;       // working copy of dry-bulb daily range, C (or 1 if input is difference)
        Real64 WBRange;       // working copy of wet-bulb daily range. C (or 1 if input is difference)

        Array1D_int Date0(8);
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
            Array1D<Real64> BeamSolarRad; // Hourly direct normal solar irradiance
            Array1D<Real64> DifSolarRad;  // Hourly sky diffuse horizontal solar irradiance

            // Default Constructor
            HourlyWeatherData() : BeamSolarRad(24, 0.0), DifSolarRad(24, 0.0)
            {
            }
        };

        // Object Data
        HourlyWeatherData Wthr;

        SaveWarmupFlag = WarmupFlag;
        WarmupFlag = true;

        date_and_time(_, _, _, Date0);
        CurrentYear = Date0(1);

        if (BeginSimFlag) {
            PrintDDHeader = true;
        }

        dataWeatherManager.DesignDay(EnvrnNum).Year = CurrentYear; // f90 date_and_time implemented. full 4 digit year !+ 1900
        dataWeatherManager.DesignDay(EnvrnNum).Month = dataWeatherManager.DesDayInput(EnvrnNum).Month;
        dataWeatherManager.DesignDay(EnvrnNum).DayOfMonth = dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth;
        dataWeatherManager.DesignDay(EnvrnNum).DayOfYear = General::OrdinalDay(dataWeatherManager.DesignDay(EnvrnNum).Month, dataWeatherManager.DesignDay(EnvrnNum).DayOfMonth, 0);
        static constexpr auto MnDyFmt("{:02}/{:02}");
        CurMnDy = format(MnDyFmt, dataWeatherManager.DesDayInput(EnvrnNum).Month, dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth);
        // EnvironmentName = DesDayInput( EnvrnNum ).Title;
        RunPeriodEnvironment = false;
        // Following builds Environment start/end for ASHRAE 55 warnings
        EnvironmentStartEnd = CurMnDy + " - " + CurMnDy;

        // Check that barometric pressure is within range
        if (dataWeatherManager.DesDayInput(EnvrnNum).PressureEntered) {
            if (std::abs((dataWeatherManager.DesDayInput(EnvrnNum).PressBarom - StdBaroPress) / StdBaroPress) > 0.1) { // 10% off
                ShowWarningError("SetUpDesignDay: Entered DesignDay Barometric Pressure=" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).PressBarom, 0) +
                                 " differs by more than 10% from Standard Barometric Pressure=" + RoundSigDigits(StdBaroPress, 0) + '.');
                ShowContinueError("...occurs in DesignDay=" + EnvironmentName + ", Standard Pressure (based on elevation) will be used.");
                dataWeatherManager.DesDayInput(EnvrnNum).PressBarom = StdBaroPress;
            }
        } else {
            dataWeatherManager.DesDayInput(EnvrnNum).PressBarom = StdBaroPress;
        }

        // verify that design WB or DP <= design DB
        if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_DewPoint && dataWeatherManager.DesDayInput(EnvrnNum).DewPointNeedsSet) {
            // dew-point
            testval = PsyWFnTdbRhPb(dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb, 1.0, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
            dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = PsyTdpFnWPb(testval, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
        }

        // Day of week defaults to Monday, if day type specified, then that is used.
        dataWeatherManager.DesignDay(EnvrnNum).DayOfWeek = 2;
        if (dataWeatherManager.DesDayInput(EnvrnNum).DayType <= 7) dataWeatherManager.DesignDay(EnvrnNum).DayOfWeek = dataWeatherManager.DesDayInput(EnvrnNum).DayType;

        // set Holiday as indicated by user input
        dataWeatherManager. DesignDay(EnvrnNum).HolidayIndex = 0;
        if (dataWeatherManager.DesDayInput(EnvrnNum).DayType > 7) dataWeatherManager.DesignDay(EnvrnNum).HolidayIndex = dataWeatherManager.DesDayInput(EnvrnNum).DayType - 7;

        dataWeatherManager.DesignDay(EnvrnNum).DaylightSavingIndex = dataWeatherManager.DesDayInput(EnvrnNum).DSTIndicator;

        //  Set up Solar parameters for day
        CalculateDailySolarCoeffs(dataWeatherManager.DesignDay(EnvrnNum).DayOfYear,
                                  A,
                                  B,
                                  C,
                                  AVSC,
                                  dataWeatherManager.DesignDay(EnvrnNum).EquationOfTime,
                                  dataWeatherManager.DesignDay(EnvrnNum).SinSolarDeclinAngle,
                                  dataWeatherManager.DesignDay(EnvrnNum).CosSolarDeclinAngle);

        if (PrintDDHeader && DoWeatherInitReporting) {
            static constexpr auto EnvDDHdFormat(
                "! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, Temp Range {dC}, Temp Range Ind Type, "
                "Hum Ind Type, Hum Ind Value at Max Temp, Hum Ind Units, Pressure {Pa}, Wind Direction {deg CW from N}, Wind "
                "Speed {m/s}, Clearness, Rain, Snow");
            print(outputFiles.eio, "{}\n", EnvDDHdFormat);
            static constexpr auto DDayMiscHdFormat(
                "! <Environment:Design Day Misc>,DayOfYear,ASHRAE A Coeff,ASHRAE B Coeff,ASHRAE C Coeff,Solar "
                "Constant-Annual Variation,Eq of Time {minutes}, Solar Declination Angle {deg}, Solar Model");
            print(outputFiles.eio, "{}\n", DDayMiscHdFormat);
            PrintDDHeader = false;
        }
        if (DoWeatherInitReporting) {
            if (dataWeatherManager.DesDayInput(EnvrnNum).RainInd == 1) {
                AlpUseRain = "Yes";
            } else {
                AlpUseRain = "No";
            }
            if (dataWeatherManager.DesDayInput(EnvrnNum).SnowInd == 1) {
                AlpUseSnow = "Yes";
            } else {
                AlpUseSnow = "No";
            }
            print(outputFiles.eio, "Environment:Design Day Data,");
            print(outputFiles.eio, "{:.2R},", dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb);
            print(outputFiles.eio, "{:.2R},", dataWeatherManager.DesDayInput(EnvrnNum).DailyDBRange);

            StringOut = ",";
            if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType == dataWeatherManager.DDDBRangeType_Default) {
                StringOut = "DefaultMultipliers,";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType == dataWeatherManager.DDDBRangeType_Multiplier) {
                StringOut = "MultiplierSchedule,";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType == dataWeatherManager.DDDBRangeType_Profile) {
                StringOut = "TemperatureProfile,";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType == dataWeatherManager.DDDBRangeType_Difference) {
                StringOut = "DifferenceSchedule,";
            }
            print(outputFiles.eio, "{}", StringOut);


            // Hum Ind Type, Hum Ind Value at Max Temp, Hum Ind Units
            if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WetBulb) {
                StringOut = "Wetbulb," + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, 2) + ",{C},";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_DewPoint) {
                StringOut = "Dewpoint," + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, 2) + ",{C},";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_Enthalpy) {
                StringOut = "Enthalpy," + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, 2) + ",{J/kgDryAir},";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_HumRatio) {
                StringOut = "HumidityRatio," + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, 4) + ",{kgWater/kgDryAir},";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_RelHumSch) {
                StringOut = "Schedule,<schedule values from 0.0 to 100.0>,{percent},";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDef) {
                StringOut = "WetBulbProfileDefaultMultipliers," + RoundSigDigits(dataWeatherManager.DesDayInput(dataWeatherManager.Envrn).HumIndValue, 2) + ",{C},";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDif) {
                StringOut = "WetBulbProfileDifferenceSchedule," + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, 2) + ",{C},";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfMul) {
                StringOut = "WetBulbProfileMultiplierSchedule," + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, 2) + ",{C},";
            }
            print(outputFiles.eio, "{}", StringOut);
            print(outputFiles.eio, "{:.0R},", dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
            print(outputFiles.eio, "{:.0R},", dataWeatherManager.DesDayInput(EnvrnNum).WindDir);
            print(outputFiles.eio, "{:.1R},", dataWeatherManager.DesDayInput(EnvrnNum).WindSpeed);
            print(outputFiles.eio, "{:.2R},", dataWeatherManager.DesDayInput(EnvrnNum).SkyClear);

            print(outputFiles.eio, "{},{}\n", AlpUseRain, AlpUseSnow);

            static constexpr auto DDayMiscFormat("Environment:Design Day Misc,{:3},");
            print(outputFiles.eio, DDayMiscFormat, dataWeatherManager.DesignDay(EnvrnNum).DayOfYear);
            print(outputFiles.eio, "{:.1R},", A);
            print(outputFiles.eio, "{:.4R},", B);
            print(outputFiles.eio, "{:.4R},", C);
            print(outputFiles.eio, "{:.1R},", AVSC);
            print(outputFiles.eio, "{:.2R},", dataWeatherManager.DesignDay(EnvrnNum).EquationOfTime * 60.0);
            print(outputFiles.eio, "{:.1R},", std::asin(dataWeatherManager.DesignDay(EnvrnNum).SinSolarDeclinAngle) / DegToRadians);

            if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.ASHRAE_ClearSky) {
                StringOut = "ASHRAEClearSky";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.Zhang_Huang) {
                StringOut = "ZhangHuang";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.SolarModel_Schedule) {
                StringOut = "User supplied beam/diffuse from schedules";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.ASHRAE_Tau) {
                StringOut = "ASHRAETau";
            } else if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.ASHRAE_Tau2017) {
                StringOut = "ASHRAETau2017";
            } else {
                StringOut = "unknown";
            }
            print(outputFiles.eio, "{}\n", StringOut);
        }

        // Must set up weather values for Design Day.  User can specify the "humidity indicator" as
        // Wetbulb, DewPoint or input the relative humidity schedule.  For both wetbulb and dewpoint indicators, the
        // humidity for the day will be constant, using the drybulb (max) and humidity indicator temperature to
        // set the values.  For the scheduled values, these are already set in the DDxxx array.

        CurrentTime = 25.0;

        {
            auto const SELECT_CASE_var(dataWeatherManager.DesDayInput(EnvrnNum).HumIndType);

            if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_WetBulb) {
                HumidityRatio = PsyWFnTdbTwbPb(
                    dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb, dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom, RoutineNamePsyWFnTdbTwbPb);
                ConstantHumidityRatio = true;

            } else if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_DewPoint) {
                HumidityRatio = PsyWFnTdpPb(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom, RoutineNamePsyWFnTdpPb);
                ConstantHumidityRatio = true;

            } else if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_HumRatio) {
                HumidityRatio = dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue;
                ConstantHumidityRatio = true;

            } else if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_Enthalpy) {
                // HumIndValue is already in J/kg, so no conversions needed
                HumidityRatio = PsyWFnTdbH(dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb, dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, RoutineNamePsyWFnTdbH);
                ConstantHumidityRatio = true;

            } else if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_RelHumSch) {
                // nothing to do -- DDHumIndModifier already contains the scheduled Relative Humidity
                ConstantHumidityRatio = false;
                dataWeatherManager.TomorrowOutRelHum = dataWeatherManager.DDHumIndModifier(_, _, EnvrnNum);

            } else if ((SELECT_CASE_var == dataWeatherManager.DDHumIndType_WBProfDef) || (SELECT_CASE_var == dataWeatherManager.DDHumIndType_WBProfDif) ||
                       (SELECT_CASE_var == dataWeatherManager.DDHumIndType_WBProfMul)) {
                ConstantHumidityRatio = false;

            } else {
                ShowSevereError("SetUpDesignDay: Invalid Humidity Indicator type");
                ShowContinueError("Occurred in Design Day=" + dataWeatherManager.DesDayInput(EnvrnNum).Title);
            }
        }

        if (dataWeatherManager.DesDayInput(EnvrnNum).RainInd != 0) {
            dataWeatherManager.TomorrowIsRain(_, _) = true;
            OSky = 10;
            dataWeatherManager.TomorrowLiquidPrecip = 3.0;
        } else {
            dataWeatherManager.TomorrowIsRain(_, _) = false;
            OSky = 0;
            dataWeatherManager.TomorrowLiquidPrecip = 0.0;
        }

        if (dataWeatherManager.DesDayInput(EnvrnNum).SnowInd == 0) {
            dataWeatherManager.TomorrowIsSnow(_, _) = false;
            GndReflet = 0.2;
        } else { // Snow
            dataWeatherManager.TomorrowIsSnow(_, _) = true;
            GndReflet = 0.7;
        }

        // Some values are constant

        dataWeatherManager.TomorrowOutBaroPress(_, _) = dataWeatherManager.DesDayInput(EnvrnNum).PressBarom;
        dataWeatherManager.TomorrowWindSpeed(_, _) = dataWeatherManager.DesDayInput(EnvrnNum).WindSpeed;
        dataWeatherManager.TomorrowWindDir(_, _) = dataWeatherManager.DesDayInput(EnvrnNum).WindDir;
        dataWeatherManager.TomorrowAlbedo = 0.0;

        // resolve daily ranges
        if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType == dataWeatherManager.DDDBRangeType_Difference) {
            DBRange = 1.0; // use unscaled multiplier values if difference
        } else if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType == dataWeatherManager.DDDBRangeType_Profile) {
            DBRange = 0.0;
        } else {
            DBRange = dataWeatherManager.DesDayInput(EnvrnNum).DailyDBRange;
        }
        if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDif) {
            WBRange = 1.0; // use unscaled multiplier values if difference
        } else {
            WBRange = dataWeatherManager.DesDayInput(EnvrnNum).DailyWBRange;
        }

        for (Hour = 1; Hour <= 24; ++Hour) {
            for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {

                if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType != dataWeatherManager.DDDBRangeType_Profile) {
                    // dry-bulb profile
                    dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour) = dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb - dataWeatherManager.DDDBRngModifier(TS, Hour, EnvrnNum) * DBRange;
                } else { // dataWeatherManager.DesDayInput(EnvrnNum)%DBTempRangeType == DDDBRangeType_Profile
                    dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour) = dataWeatherManager.DDDBRngModifier(TS, Hour, EnvrnNum);
                }

                // wet-bulb - generate from profile, humidity ratio, or dew point
                if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDef || dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDif ||
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfMul) {
                    WetBulb = dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue - dataWeatherManager.DDHumIndModifier(TS, Hour, EnvrnNum) * WBRange;
                    WetBulb = min(WetBulb, dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour)); // WB must be <= DB
                    OutHumRat = PsyWFnTdbTwbPb(dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), WetBulb, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
                    dataWeatherManager.TomorrowOutDewPointTemp(TS, Hour) = PsyTdpFnWPb(OutHumRat, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
                    dataWeatherManager.TomorrowOutRelHum(TS, Hour) =
                        PsyRhFnTdbWPb(dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), OutHumRat, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom, WeatherManager) * 100.0;
                } else if (ConstantHumidityRatio) {
                    //  Need Dew Point Temperature.  Use Relative Humidity to get Humidity Ratio, unless Humidity Ratio is constant
                    // BG 9-26-07  moved following inside this IF statment; when HumIndType is 'Schedule' HumidityRatio wasn't being initialized
                    WetBulb = PsyTwbFnTdbWPb(dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), HumidityRatio, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom, RoutineNameLong);

                    OutHumRat = PsyWFnTdpPb(dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
                    if (HumidityRatio > OutHumRat) {
                        WetBulb = dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour);
                    } else {
                        OutHumRat = PsyWFnTdbTwbPb(dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), WetBulb, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
                    }
                    dataWeatherManager.TomorrowOutDewPointTemp(TS, Hour) = PsyTdpFnWPb(OutHumRat, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
                    dataWeatherManager.TomorrowOutRelHum(TS, Hour) =
                        PsyRhFnTdbWPb(dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), OutHumRat, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom, WeatherManager) * 100.0;
                } else {
                    HumidityRatio = PsyWFnTdbRhPb(
                        dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), dataWeatherManager.DDHumIndModifier(TS, Hour, EnvrnNum) / 100.0, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
                    dataWeatherManager.TomorrowOutRelHum(TS, Hour) =
                            PsyRhFnTdbWPb(dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour), HumidityRatio, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom, WeatherManager) * 100.0;
                    // dataWeatherManager.TomorrowOutRelHum values set earlier
                    dataWeatherManager.TomorrowOutDewPointTemp(TS, Hour) = PsyTdpFnWPb(HumidityRatio, dataWeatherManager.DesDayInput(EnvrnNum).PressBarom);
                }

                // Determine Sky Temp ==>
                // Function of DryBulb, DewPoint, OpaqueSkyCover
                // Calculate Sky IR
                // HIR = ESKY * SIGMA * (TOUT**4)
                // where
                // HIR = horizontal IR intensity (W/m2)
                // ESKY = sky emissivity
                // SIGMA = Stefan-Boltzmann constant = 5.6697e-8 W/m2-K4
                // TOUT = drybulb temperature (K)
                // The sky emissivity is given by
                // ESKY = [0.787 + 0.764*ln(TDEW/273)]*[1 + 0.0224*N - 0.0035*(N**2) + 0.00028*(N**3)]
                // where
                // TDEW = dewpoint temperature (K)
                // N = opaque sky cover (tenths)
                // Example: Clear sky (N=0), TOUT = 273+20=293K, TDEW = 273+10=283K:
                // ESKY = 0.787 + 0.764*0.036 = 0.815
                // HIR = 0.815*5.6697e-8*(293**4) = 340.6 W/m2

                // References:
                // George N. Walton, "Thermal Analysis Research Program Reference Manual,"
                // NBSIR 83-2655, March 1983, p. 21.
                // G. Clark and C. Allen, "The Estimation of Atmospheric Radiation for Clear and
                // Cloudy Skies," Proc. 2nd National Passive Solar Conference (AS/ISES), 1978, pp. 675-678.

                double DryBulb = dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour);
                double RelHum = dataWeatherManager.TomorrowOutRelHum(TS, Hour) * 0.01;
                ESky = CalcSkyEmissivity(dataWeatherManager, dataWeatherManager.Environment(EnvrnNum).SkyTempModel, OSky, DryBulb, dataWeatherManager.TomorrowOutDewPointTemp(TS, Hour), RelHum);
                dataWeatherManager.TomorrowHorizIRSky(TS, Hour) = ESky * dataWeatherManager.Sigma * pow_4(DryBulb + dataWeatherManager.TKelvin);

                if (dataWeatherManager.Environment(EnvrnNum).SkyTempModel > 3 || dataWeatherManager.Environment(EnvrnNum).SkyTempModel == 0) {
                    // Design day not scheduled
                    dataWeatherManager.TomorrowSkyTemp(TS, Hour) = (DryBulb + dataWeatherManager.TKelvin) * root_4(ESky) - dataWeatherManager.TKelvin;
                }
                // Generate solar values for timestep
                //    working results = BeamRad and DiffRad
                //    stored to program globals at end of loop
                if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.SolarModel_Schedule) {
                    // scheduled: set value unconditionally (whether sun up or not)
                    BeamRad = dataWeatherManager.DDBeamSolarValues(TS, Hour, EnvrnNum);
                    DiffRad = dataWeatherManager.DDDiffuseSolarValues(TS, Hour, EnvrnNum);
                } else {

                    // calc time = fractional hour of day
                    if (NumOfTimeStepInHour != 1) {
                        CurTime = double(Hour - 1) + double(TS) * dataWeatherManager.TimeStepFraction;
                    } else {
                        CurTime = double(Hour) + TS1TimeOffset;
                    }

                    CalculateSunDirectionCosines(CurTime,
                                                 dataWeatherManager.DesignDay(EnvrnNum).EquationOfTime,
                                                 dataWeatherManager.DesignDay(EnvrnNum).SinSolarDeclinAngle,
                                                 dataWeatherManager.DesignDay(EnvrnNum).CosSolarDeclinAngle,
                                                 SUNCOS);
                    CosZenith = SUNCOS(3);
                    if (CosZenith < SunIsUpValue) {
                        BeamRad = 0.0;
                        DiffRad = 0.0;
                    } else {
                        SinSolarAltitude = SUNCOS(3);

                        {
                            auto const SELECT_CASE_var(dataWeatherManager.DesDayInput(EnvrnNum).SolarModel);

                            if (SELECT_CASE_var == dataWeatherManager.ASHRAE_ClearSky) {
                                Real64 Exponent = B / CosZenith;
                                if (Exponent > 700.0) {
                                    TotHoriz = 0.0;
                                } else {
                                    TotHoriz = dataWeatherManager.DesDayInput(EnvrnNum).SkyClear * A * (C + CosZenith) * std::exp(-B / CosZenith);
                                }
                                HO = GlobalSolarConstant * AVSC * CosZenith;
                                KT = TotHoriz / HO;
                                KT = min(KT, 0.75);
                                DiffRad = TotHoriz * (1.0045 + KT * (0.04349 + KT * (-3.5227 + 2.6313 * KT)));
                                if (dataWeatherManager.DesDayInput(EnvrnNum).SkyClear > 0.70) DiffRad = TotHoriz * C / (C + CosZenith);
                                BeamRad = (TotHoriz - DiffRad) / CosZenith;
                                DiffRad = max(0.0, DiffRad);
                                BeamRad = max(0.0, BeamRad);

                            } else if (SELECT_CASE_var == dataWeatherManager.ASHRAE_Tau || SELECT_CASE_var == dataWeatherManager.ASHRAE_Tau2017) {
                                ETR = GlobalSolarConstant * AVSC; // extraterrestrial normal irrad, W/m2
                                ASHRAETauModel(dataWeatherManager, dataWeatherManager.DesDayInput(EnvrnNum).SolarModel,
                                               ETR,
                                               CosZenith,
                                               dataWeatherManager.DesDayInput(EnvrnNum).TauB,
                                               dataWeatherManager.DesDayInput(EnvrnNum).TauD,
                                               BeamRad,
                                               DiffRad,
                                               GloHorzRad);

                            } else if (SELECT_CASE_var == dataWeatherManager.Zhang_Huang) {
                                Hour3Ago = mod(Hour + 20, 24) + 1; // hour 3 hours before
                                TotSkyCover = max(1.0 - dataWeatherManager.DesDayInput(EnvrnNum).SkyClear, 0.0);
                                GloHorzRad =
                                    (ZHGlobalSolarConstant * SinSolarAltitude *
                                         (ZhangHuangModCoeff_C0 + ZhangHuangModCoeff_C1 * TotSkyCover + ZhangHuangModCoeff_C2 * pow_2(TotSkyCover) +
                                          ZhangHuangModCoeff_C3 * (dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour) - dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour3Ago)) +
                                          ZhangHuangModCoeff_C4 * dataWeatherManager.TomorrowOutRelHum(TS, Hour) + ZhangHuangModCoeff_C5 * dataWeatherManager.TomorrowWindSpeed(TS, Hour)) +
                                     ZhangHuangModCoeff_D) /
                                    ZhangHuangModCoeff_K;
                                GloHorzRad = max(GloHorzRad, 0.0);
                                ClearnessIndex_kt = GloHorzRad / (GlobalSolarConstant * SinSolarAltitude);
                                //          ClearnessIndex_kt=DesDayInput(EnvrnNum)%SkyClear
                                ClearnessIndex_ktc = 0.4268 + 0.1934 * SinSolarAltitude;
                                if (ClearnessIndex_kt < ClearnessIndex_ktc) {
                                    ClearnessIndex_kds =
                                        (3.996 - 3.862 * SinSolarAltitude + 1.54 * pow_2(SinSolarAltitude)) * pow_3(ClearnessIndex_kt);
                                } else {
                                    ClearnessIndex_kds = ClearnessIndex_kt - (1.107 + 0.03569 * SinSolarAltitude + 1.681 * pow_2(SinSolarAltitude)) *
                                                                                 pow_3(1.0 - ClearnessIndex_kt);
                                }
                                // Calculate direct normal radiation, W/m2
                                BeamRad = ZHGlobalSolarConstant * SinSolarAltitude * ClearnessIndex_kds *
                                          ((1.0 - ClearnessIndex_kt) / (1.0 - ClearnessIndex_kds));
                                // Calculation diffuse horizontal radiation, W/m2
                                DiffRad = ZHGlobalSolarConstant * SinSolarAltitude *
                                          ((ClearnessIndex_kt - ClearnessIndex_kds) / (1.0 - ClearnessIndex_kds));

                            } else {
                            }
                        }
                    }
                }

                // override result to 0 per environment var (for testing)
                if (IgnoreSolarRadiation || IgnoreBeamRadiation) BeamRad = 0.0;
                if (IgnoreSolarRadiation || IgnoreDiffuseRadiation) DiffRad = 0.0;

                dataWeatherManager.TomorrowBeamSolarRad(TS, Hour) = BeamRad;
                dataWeatherManager.TomorrowDifSolarRad(TS, Hour) = DiffRad;

            } // Timestep (TS) Loop
        }     // Hour Loop

        // back-fill hour values from timesteps
        // hour values = integrated over hour ending at time of hour
        // insurance: hourly values not known to be needed
        for (Hour = 1; Hour <= 24; ++Hour) {
            Hour1Ago = mod(Hour + 22, 24) + 1;
            BeamRad = (dataWeatherManager.TomorrowBeamSolarRad(NumOfTimeStepInHour, Hour1Ago) + dataWeatherManager.TomorrowBeamSolarRad(NumOfTimeStepInHour, Hour)) / 2.0;
            DiffRad = (dataWeatherManager.TomorrowDifSolarRad(NumOfTimeStepInHour, Hour1Ago) + dataWeatherManager.TomorrowDifSolarRad(NumOfTimeStepInHour, Hour)) / 2.0;
            if (NumOfTimeStepInHour > 1) {
                BeamRad += sum(dataWeatherManager.TomorrowBeamSolarRad({1, NumOfTimeStepInHour - 1}, Hour));
                DiffRad += sum(dataWeatherManager.TomorrowDifSolarRad({1, NumOfTimeStepInHour - 1}, Hour));
            }
            Wthr.BeamSolarRad(Hour) = BeamRad / NumOfTimeStepInHour;
            Wthr.DifSolarRad(Hour) = DiffRad / NumOfTimeStepInHour;
        }

        if (dataWeatherManager.Environment(EnvrnNum).WP_Type1 != 0) {

            {
                auto const SELECT_CASE_var(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(EnvrnNum).WP_Type1).CalculationType);

                if (SELECT_CASE_var == dataWeatherManager.WP_ScheduleValue) {
                    GetSingleDayScheduleValues(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(EnvrnNum).WP_Type1).SchedulePtr, dataWeatherManager.TomorrowSkyTemp);
                    dataWeatherManager.DDSkyTempScheduleValues(_, _, EnvrnNum) = dataWeatherManager.TomorrowSkyTemp;
                } else if (SELECT_CASE_var == dataWeatherManager.WP_DryBulbDelta) {
                    GetSingleDayScheduleValues(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(EnvrnNum).WP_Type1).SchedulePtr, dataWeatherManager.TomorrowSkyTemp);
                    dataWeatherManager.DDSkyTempScheduleValues(_, _, EnvrnNum) = dataWeatherManager.TomorrowSkyTemp;
                    for (Hour = 1; Hour <= 24; ++Hour) {
                        for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                            dataWeatherManager.TomorrowSkyTemp(TS, Hour) = dataWeatherManager.TomorrowOutDryBulbTemp(TS, Hour) - dataWeatherManager.TomorrowSkyTemp(TS, Hour);
                        }
                    }

                } else if (SELECT_CASE_var == dataWeatherManager.WP_DewPointDelta) {
                    GetSingleDayScheduleValues(dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(EnvrnNum).WP_Type1).SchedulePtr, dataWeatherManager.TomorrowSkyTemp);
                    dataWeatherManager.DDSkyTempScheduleValues(_, _, EnvrnNum) = dataWeatherManager.TomorrowSkyTemp;
                    for (Hour = 1; Hour <= 24; ++Hour) {
                        for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                            dataWeatherManager.TomorrowSkyTemp(TS, Hour) = dataWeatherManager.TomorrowOutDewPointTemp(TS, Hour) - dataWeatherManager.TomorrowSkyTemp(TS, Hour);
                        }
                    }

                } else {
                }
            }
        }

        WarmupFlag = SaveWarmupFlag;
    }

    //------------------------------------------------------------------------------

    Real64 AirMass(Real64 const CosZen) // COS( solar zenith), 0 - 1
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

        // Return value
        Real64 AirMass;

        Real64 SunAltD;

        if (CosZen <= 0.001) {
            AirMass = 37.07837343; // limit value calc'd with Excel
                                   //  value increases little as CosZen -> 0
        } else if (CosZen >= 1.0) {
            AirMass = 1.0;
        } else {
            // note: COS( Zen) = SIN( Alt)
            SunAltD = std::asin(CosZen) / DegToRadians; // altitude, degrees
            AirMass = 1.0 / (CosZen + 0.50572 * std::pow(6.07995 + SunAltD, -1.6364));
        }
        return AirMass;
    }

    //------------------------------------------------------------------------------

    void ASHRAETauModel(WeatherManagerData &dataWeatherManager, int const TauModelType, // ASHRAETau solar model type ASHRAE_Tau or ASHRAE_Tau2017
                        Real64 const ETR,       // extraterrestrial normal irradiance, W/m2
                        Real64 const CosZen,    // COS( solar zenith angle), 0 - 1
                        Real64 const TauB,      // beam tau factor
                        Real64 const TauD,      // dif tau factor
                        Real64 &IDirN,          // returned: direct (beam) irradiance on normal surface, W/m2
                        Real64 &IDifH,          // returned: diffuse irradiance on horiz surface, W/m2
                        Real64 &IGlbH           // returned: global irradiance on horiz surface, W/m2
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
        // Eqns (19-20), ASHRAE HOF 2013 p. 14.9 and 2017 p. 14.10

        // REFERENCES:
        // ASHRAE HOF 2009 Chapter 14

        Real64 AB; // air mass exponents
        Real64 AD;
        Real64 M; // air mass

        if (CosZen < SunIsUpValue || TauB <= 0.0 || TauD <= 0.0) {
            IDirN = 0.0;
            IDifH = 0.0;
            IGlbH = 0.0;
        } else {
            if (TauModelType == dataWeatherManager.ASHRAE_Tau) {
                AB = 1.219 - 0.043 * TauB - 0.151 * TauD - 0.204 * TauB * TauD;
                AD = 0.202 + 0.852 * TauB - 0.007 * TauD - 0.357 * TauB * TauD;
            } else {
                // TauModelType == ASHRAE_Tau2017
                AB = 1.454 - 0.406 * TauB - 0.268 * TauD + 0.021 * TauB * TauD;
                AD = 0.507 + 0.205 * TauB - 0.080 * TauD - 0.190 * TauB * TauD;
            }
            M = AirMass(CosZen);
            IDirN = ETR * std::exp(-TauB * std::pow(M, AB));
            IDifH = ETR * std::exp(-TauD * std::pow(M, AD));
            IGlbH = IDirN * CosZen + IDifH;
        }
    }

    void AllocateWeatherData(WeatherManagerData &dataWeatherManager)
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

        dataWeatherManager.TodayIsRain.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayIsRain = false;
        dataWeatherManager.TodayIsSnow.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayIsSnow = false;
        dataWeatherManager.TodayOutDryBulbTemp.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayOutDryBulbTemp = 0.0;
        dataWeatherManager.TodayOutDewPointTemp.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayOutDewPointTemp = 0.0;
        dataWeatherManager.TodayOutBaroPress.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayOutBaroPress = 0.0;
        dataWeatherManager.TodayOutRelHum.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayOutRelHum = 0.0;
        dataWeatherManager.TodayWindSpeed.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayWindSpeed = 0.0;
        dataWeatherManager.TodayWindDir.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayWindDir = 0.0;
        dataWeatherManager.TodaySkyTemp.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodaySkyTemp = 0.0;
        dataWeatherManager.TodayHorizIRSky.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayHorizIRSky = 0.0;
        dataWeatherManager.TodayBeamSolarRad.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayBeamSolarRad = 0.0;
        dataWeatherManager.TodayDifSolarRad.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayDifSolarRad = 0.0;
        dataWeatherManager.TodayAlbedo.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayAlbedo = 0.0;
        dataWeatherManager.TodayLiquidPrecip.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TodayLiquidPrecip = 0.0;

        dataWeatherManager.TomorrowIsRain.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowIsRain = false;
        dataWeatherManager.TomorrowIsSnow.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowIsSnow = false;
        dataWeatherManager.TomorrowOutDryBulbTemp.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowOutDryBulbTemp = 0.0;
        dataWeatherManager.TomorrowOutDewPointTemp.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowOutDewPointTemp = 0.0;
        dataWeatherManager.TomorrowOutBaroPress.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowOutBaroPress = 0.0;
        dataWeatherManager.TomorrowOutRelHum.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowOutRelHum = 0.0;
        dataWeatherManager.TomorrowWindSpeed.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowWindSpeed = 0.0;
        dataWeatherManager.TomorrowWindDir.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowWindDir = 0.0;
        dataWeatherManager.TomorrowSkyTemp.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowSkyTemp = 0.0;
        dataWeatherManager.TomorrowHorizIRSky.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowHorizIRSky = 0.0;
        dataWeatherManager.TomorrowBeamSolarRad.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowBeamSolarRad = 0.0;
        dataWeatherManager.TomorrowDifSolarRad.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowDifSolarRad = 0.0;
        dataWeatherManager.TomorrowAlbedo.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowAlbedo = 0.0;
        dataWeatherManager.TomorrowLiquidPrecip.allocate(NumOfTimeStepInHour, 24);
        dataWeatherManager.TomorrowLiquidPrecip = 0.0;
    }

    void CalculateDailySolarCoeffs(int const DayOfYear,           // Day of year (1 - 366)
                                   Real64 &A,                     // ASHRAE "A" - Apparent solar irradiation at air mass = 0 [W/M**2]
                                   Real64 &B,                     // ASHRAE "B" - Atmospheric extinction coefficient
                                   Real64 &C,                     // ASHRAE "C" - Diffuse radiation factor
                                   Real64 &AnnVarSolConstant,     // Annual variation in the solar constant
                                   Real64 &EquationOfTime,        // Equation of Time
                                   Real64 &SineSolarDeclination,  // Sine of Solar Declination
                                   Real64 &CosineSolarDeclination // Cosine of Solar Declination
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

        Real64 const DayCorrection(Pi * 2.0 / 366.0);

        static Array1D<Real64> const SineSolDeclCoef(
            9, {0.00561800, 0.0657911, -0.392779, 0.00064440, -0.00618495, -0.00010101, -0.00007951, -0.00011691, 0.00002096}); // Fitted coefficients
                                                                                                                                // of Fourier series |
                                                                                                                                // Sine of declination
                                                                                                                                // coefficients
        static Array1D<Real64> const EqOfTimeCoef(
            9, {0.00021971, -0.122649, 0.00762856, -0.156308, -0.0530028, -0.00388702, -0.00123978, -0.00270502, -0.00167992}); // Fitted coefficients
                                                                                                                                // of Fourier Series |
                                                                                                                                // Equation of Time
                                                                                                                                // coefficients
        static Array1D<Real64> const ASHRAE_A_Coef(
            9, {1161.6685, 1.1554, 77.3575, -0.5359, -3.7622, 0.9875, -3.3924, -1.7445, 1.1198}); // Fitted coefficients of Fourier Series | ASHRAE A
                                                                                                  // Factor coefficients
        // English (original) units:
        //              368.49341,.366502,24.538624,-.169983,-1.193417,            &
        //              .313261,-1.076093,-.543376,.355197 ,                       &

        static Array1D<Real64> const ASHRAE_B_Coef(
            9, {0.171631, -0.00400448, -0.0344923, 0.00000209, 0.00325428, -0.00085429, 0.00229562, 0.0009034, -0.0011867}); // Fitted coefficients of
                                                                                                                             // Fourier Series |
                                                                                                                             // ASHRAE B Factor
                                                                                                                             // coefficients
        static Array1D<Real64> const ASHRAE_C_Coef(
            9, {0.0905151, -0.00322522, -0.0407966, 0.000104164, 0.00745899, -0.00086461, 0.0013111, 0.000808275, -0.00170515}); // Fitted
                                                                                                                                 // coefficients of
                                                                                                                                 // Fourier Series |
                                                                                                                                 // ASHRAE C Factor
                                                                                                                                 // coefficients

        Real64 X;    // Day of Year in Radians (Computed from Input DayOfYear)
        Real64 CosX; // COS(X)
        Real64 SinX; // SIN(X)

        X = DayCorrection * DayOfYear; // Convert Julian date (Day of Year) to angle X

        // Calculate sines and cosines of X
        SinX = std::sin(X);
        CosX = std::cos(X);

        SineSolarDeclination = SineSolDeclCoef(1) + SineSolDeclCoef(2) * SinX + SineSolDeclCoef(3) * CosX + SineSolDeclCoef(4) * (SinX * CosX * 2.0) +
                               SineSolDeclCoef(5) * (pow_2(CosX) - pow_2(SinX)) +
                               SineSolDeclCoef(6) * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
                               SineSolDeclCoef(7) * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
                               SineSolDeclCoef(8) * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
                               SineSolDeclCoef(9) * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));
        CosineSolarDeclination = std::sqrt(1.0 - pow_2(SineSolarDeclination));

        EquationOfTime = EqOfTimeCoef(1) + EqOfTimeCoef(2) * SinX + EqOfTimeCoef(3) * CosX + EqOfTimeCoef(4) * (SinX * CosX * 2.0) +
                         EqOfTimeCoef(5) * (pow_2(CosX) - pow_2(SinX)) +
                         EqOfTimeCoef(6) * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
                         EqOfTimeCoef(7) * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
                         EqOfTimeCoef(8) * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
                         EqOfTimeCoef(9) * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));

        AnnVarSolConstant = 1.000047 + 0.000352615 * SinX + 0.0334454 * CosX;

        A = ASHRAE_A_Coef(1) + ASHRAE_A_Coef(2) * SinX + ASHRAE_A_Coef(3) * CosX + ASHRAE_A_Coef(4) * (SinX * CosX * 2.0) +
            ASHRAE_A_Coef(5) * (pow_2(CosX) - pow_2(SinX)) + ASHRAE_A_Coef(6) * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
            ASHRAE_A_Coef(7) * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
            ASHRAE_A_Coef(8) * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
            ASHRAE_A_Coef(9) * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));

        //                        Compute B and C coefficients

        if (Latitude < 0.0) {
            //                            If in southern hemisphere, compute B and C with a six month time shift.
            X -= Pi;
            SinX = std::sin(X);
            CosX = std::cos(X);
        }

        B = ASHRAE_B_Coef(1) + ASHRAE_B_Coef(2) * SinX + ASHRAE_B_Coef(3) * CosX + ASHRAE_B_Coef(4) * (SinX * CosX * 2.0) +
            ASHRAE_B_Coef(5) * (pow_2(CosX) - pow_2(SinX)) + ASHRAE_B_Coef(6) * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
            ASHRAE_B_Coef(7) * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
            ASHRAE_B_Coef(8) * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
            ASHRAE_B_Coef(9) * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));

        C = ASHRAE_C_Coef(1) + ASHRAE_C_Coef(2) * SinX + ASHRAE_C_Coef(3) * CosX + ASHRAE_C_Coef(4) * (SinX * CosX * 2.0) +
            ASHRAE_C_Coef(5) * (pow_2(CosX) - pow_2(SinX)) + ASHRAE_C_Coef(6) * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
            ASHRAE_C_Coef(7) * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
            ASHRAE_C_Coef(8) * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
            ASHRAE_C_Coef(9) * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));
    }

    void CalculateSunDirectionCosines(Real64 const TimeValue,    // Current Time of Day
                                      Real64 const EqOfTime,     // Equation of Time
                                      Real64 const SinSolDeclin, // Sine of Solar Declination
                                      Real64 const CosSolDeclin, // Cosine of Solar Declination
                                      Array1D<Real64> &SUNCOS)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   May 1975
        //       MODIFIED       1999 for EnergyPlus
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine computes the solar direction cosines for hourly
        // radiation calculations.

        // REFERENCES:
        // "NECAP Engineering Manual", 1974, p.3-117

        // Argument array dimensioning
        EP_SIZE_CHECK(SUNCOS, 3);

        Real64 COSH; // Cosine of hour angle
        Real64 H;    // Hour angle (before noon = +)

        //                                      COMPUTE THE HOUR ANGLE
        H = (15.0 * (12.0 - (TimeValue + EqOfTime)) + (TimeZoneMeridian - Longitude)) * DegToRadians;
        COSH = std::cos(H);
        //                                      COMPUTE THE COSINE OF THE
        //                                      SOLAR ZENITH ANGLE.
        //                                      This is also the Sine of the Solar Altitude Angle

        SUNCOS(3) = SinSolDeclin * SinLatitude + CosSolDeclin * CosLatitude * COSH;

        if (SUNCOS(3) >= SunIsUpValue) { // If Sun above horizon, compute other direction cosines
            SUNCOS(2) = SinSolDeclin * CosLatitude - CosSolDeclin * SinLatitude * COSH;
            SUNCOS(1) = CosSolDeclin * std::sin(H);
        } else { // Sun is down, set to 0.0
            SUNCOS(1) = 0.0;
            SUNCOS(2) = 0.0;
        }
    }

    void DetermineSunUpDown(WeatherManagerData &dataWeatherManager, Array1D<Real64> &SunDirectionCosines)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines if the sun is up or down for the current
        // hour/timestep.

        // REFERENCES:
        // Sun routines from IBLAST, authored by Walton.

        // Argument array dimensioning
        EP_SIZE_CHECK(SunDirectionCosines, 3);

        Real64 H; // Hour angle (before noon = +)
        Real64 SinAltitude;
        Real64 SolarAltitude;
        Real64 SolarAzimuth;
        Real64 SolarZenith;
        Real64 CosAzimuth;
        Real64 CosZenith;
        //  REAL(r64) HAngle

        // COMPUTE THE HOUR ANGLE

        if (NumOfTimeStepInHour != 1) {
            dataWeatherManager.HrAngle = (15.0 * (12.0 - (CurrentTime + dataWeatherManager.TodayVariables.EquationOfTime)) + (TimeZoneMeridian - Longitude));
        } else {
            dataWeatherManager.HrAngle = (15.0 * (12.0 - ((CurrentTime + TS1TimeOffset) + dataWeatherManager.TodayVariables.EquationOfTime)) + (TimeZoneMeridian - Longitude));
        }
        H = dataWeatherManager.HrAngle * DegToRadians;

        // Compute the Cosine of the Solar Zenith (Altitude) Angle.
        CosZenith = SinLatitude * dataWeatherManager.TodayVariables.SinSolarDeclinAngle + CosLatitude * dataWeatherManager.TodayVariables.CosSolarDeclinAngle * std::cos(H);

        SolarZenith = std::acos(CosZenith);
        SinAltitude = CosLatitude * dataWeatherManager.TodayVariables.CosSolarDeclinAngle * std::cos(H) + SinLatitude * dataWeatherManager.TodayVariables.SinSolarDeclinAngle;
        SolarAltitude = std::asin(SinAltitude);
        CosAzimuth = -(SinLatitude * CosZenith - dataWeatherManager.TodayVariables.SinSolarDeclinAngle) / (CosLatitude * std::sin(SolarZenith));
        // Following because above can yield invalid cos value.  (e.g. at south pole)
        CosAzimuth = max(CosAzimuth, -1.0);
        CosAzimuth = min(1.0, CosAzimuth);
        SolarAzimuth = std::acos(CosAzimuth);

        dataWeatherManager.SolarAltitudeAngle = SolarAltitude / DegToRadians;
        dataWeatherManager.SolarAzimuthAngle = SolarAzimuth / DegToRadians;
        if (dataWeatherManager.HrAngle < 0.0) {
            dataWeatherManager.SolarAzimuthAngle = 360.0 - dataWeatherManager.SolarAzimuthAngle;
        }

        SunDirectionCosines(3) = CosZenith;
        if (CosZenith < SunIsUpValue) {
            SunIsUp = false;
            SunDirectionCosines(2) = 0.0;
            SunDirectionCosines(1) = 0.0;
        } else {
            SunIsUp = true;
            SunDirectionCosines(2) =
                dataWeatherManager.TodayVariables.SinSolarDeclinAngle * CosLatitude - dataWeatherManager.TodayVariables.CosSolarDeclinAngle * SinLatitude * std::cos(H);
            SunDirectionCosines(1) = dataWeatherManager.TodayVariables.CosSolarDeclinAngle * std::sin(H);
        }
    }

    void OpenWeatherFile(WeatherManagerData &dataWeatherManager, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   June 1999
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine checks to see if a weather file and what kind of weather file
        // exists in the working directory and calls appropriate routines to
        // open the files and set up for use.

        {
            IOFlags flags;
            ObjexxFCL::gio::inquire(DataStringGlobals::inputWeatherFileName, flags);
            dataWeatherManager.WeatherFileExists = flags.exists();
        }

        if (dataWeatherManager.WeatherFileExists) {
            OpenEPlusWeatherFile(dataWeatherManager, OutputFiles::getSingleton(), ErrorsFound, true);
        }
    }

    void OpenEPlusWeatherFile(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles,
                              bool &ErrorsFound,       // Will be set to true if errors found
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

        static ObjexxFCL::gio::Fmt fmtA("(A)");

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const Header(8,
                                           {"LOCATION",
                                            "DESIGN CONDITIONS",
                                            "TYPICAL/EXTREME PERIODS",
                                            "GROUND TEMPERATURES",
                                            "HOLIDAYS/DAYLIGHT SAVING",
                                            "COMMENTS 1",
                                            "COMMENTS 2",
                                            "DATA PERIODS"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string Line;
        int HdLine;
        bool StillLooking;
        int endcol;
        bool EPWOpen;
        int unitnumber;

        {
            IOFlags flags;
            ObjexxFCL::gio::inquire(DataStringGlobals::inputWeatherFileName, flags);
            unitnumber = flags.unit();
            EPWOpen = flags.open();
        }
        if (EPWOpen) ObjexxFCL::gio::close(unitnumber);

        dataWeatherManager.WeatherFileUnitNumber = GetNewUnitNumber();
        {
            IOFlags flags;
            flags.ACTION("read");
            ObjexxFCL::gio::open(dataWeatherManager.WeatherFileUnitNumber, DataStringGlobals::inputWeatherFileName, flags);
            if (flags.err()) goto Label9999;
        }

        if (ProcessHeader) {
            // Read in Header Information

            // Headers should come in order
            HdLine = 1; // Look for first Header
            StillLooking = true;
            while (StillLooking) {
                {
                    IOFlags flags;
                    ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> Line;
                    if (flags.end()) goto Label9998;
                }
                endcol = len(Line);
                if (endcol > 0) {
                    if (int(Line[endcol - 1]) == iUnicode_end) {
                        goto Label9997;
                    }
                }
                std::string::size_type const Pos = FindNonSpace(Line);
                std::string::size_type const HdPos = index(Line, Header(HdLine));
                if (Pos != HdPos) continue;
                //      line=UtilityRoutines::MakeUPPERCase(line)
                ProcessEPWHeader(dataWeatherManager, Header(HdLine), Line, ErrorsFound);
                ++HdLine;
                if (HdLine == 9) StillLooking = false;
            }
        } else { // Header already processed, just read
            SkipEPlusWFHeader(dataWeatherManager);
        }

        return;

    Label9997:;
        ShowSevereError("OpenWeatherFile: EPW Weather File appears to be a Unicode or binary file.", OptionalOutputFileRef(outputFiles.eso));
        ShowContinueError("...This file cannot be read by this program. Please save as PC or Unix file and try again");
        ShowFatalError("Program terminates due to previous condition.");

    Label9998:;
        ShowFatalError("OpenWeatherFile: Unexpected End-of-File on EPW Weather file, while reading header information, looking for header=" +
                           Header(HdLine),
                       OptionalOutputFileRef(outputFiles.eso));

    Label9999:;
        ShowFatalError("OpenWeatherFile: Could not OPEN EPW Weather File", OptionalOutputFileRef(outputFiles.eso));
    }

    void CloseWeatherFile()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine closes the open weather file.

        bool EPWOpen;
        int unitnumber;

        //  Make sure it's open

        {
            IOFlags flags;
            ObjexxFCL::gio::inquire(DataStringGlobals::inputWeatherFileName, flags);
            unitnumber = flags.unit();
            EPWOpen = flags.open();
        }
        if (EPWOpen) ObjexxFCL::gio::close(unitnumber);
    }

    void ResolveLocationInformation(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, bool &ErrorsFound) // Set to true if no location evident
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

        using General::RoundSigDigits;

        if (dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).KindOfEnvrn == ksRunPeriodWeather && dataWeatherManager.WeatherFileExists) {
            if (dataWeatherManager.LocationGathered) {
                // See if "matching" location
                if (std::abs(Latitude - dataWeatherManager.WeatherFileLatitude) > 1.0 || std::abs(Longitude - dataWeatherManager.WeatherFileLongitude) > 1.0 ||
                    std::abs(TimeZoneNumber - dataWeatherManager.WeatherFileTimeZone) > 0.0 || std::abs(Elevation - dataWeatherManager.WeatherFileElevation) / max(Elevation, 1.0) > 0.10) {
                    ShowWarningError("Weather file location will be used rather than entered (IDF) Location object.");
                    ShowContinueError("..Location object=" + dataWeatherManager.LocationTitle);
                    ShowContinueError("..Weather File Location=" + WeatherFileLocationTitle);
                    ShowContinueError("..due to location differences, Latitude difference=[" +
                                      RoundSigDigits(std::abs(Latitude - dataWeatherManager.WeatherFileLatitude), 2) + "] degrees, Longitude difference=[" +
                                      RoundSigDigits(std::abs(Longitude - dataWeatherManager.WeatherFileLongitude), 2) + "] degrees.");
                    ShowContinueError("..Time Zone difference=[" + RoundSigDigits(std::abs(TimeZoneNumber - dataWeatherManager.WeatherFileTimeZone), 1) +
                                      "] hour(s), Elevation difference=[" +
                                      RoundSigDigits(std::abs((Elevation - dataWeatherManager.WeatherFileElevation) / max(Elevation, 1.0)) * 100.0, 2) + "] percent, [" +
                                      RoundSigDigits(std::abs(Elevation - dataWeatherManager.WeatherFileElevation), 2) + "] meters.");
                }
            }

            dataWeatherManager.LocationTitle = WeatherFileLocationTitle;
            Latitude = dataWeatherManager.WeatherFileLatitude;
            Longitude = dataWeatherManager.WeatherFileLongitude;
            TimeZoneNumber = dataWeatherManager.WeatherFileTimeZone;
            Elevation = dataWeatherManager.WeatherFileElevation;
        } else if (!dataWeatherManager.LocationGathered) {
            dataWeatherManager.LocationTitle = "Not Entered";
            ShowSevereError("No Location given. Must have location information for simulation.");
            ErrorsFound = true;
        }

        if (!ErrorsFound) {
            StdBaroPress = StdPressureSeaLevel * std::pow(1.0 - 2.25577e-05 * Elevation, 5.2559);
            StdRhoAir = PsyRhoAirFnPbTdbW(StdBaroPress, constant_twenty, constant_zero);
            // Write Final Location Information to the initialization output file
            static constexpr auto LocHdFormat("! <Site:Location>, Location Name, Latitude {N+/S- Deg}, Longitude {E+/W- Deg},  Time Zone Number "
                                              "{GMT+/-}, Elevation {m},  Standard Pressure at Elevation {Pa}, Standard RhoAir at Elevation\n");
            print(outputFiles.eio, "{}", LocHdFormat);

            static constexpr auto LocFormat("Site:Location,{},{:.2R},{:.2R},{:.2R},{:.2R},{:.0R},{:.4R}\n");
            print(outputFiles.eio, LocFormat, dataWeatherManager.LocationTitle, Latitude, Longitude, TimeZoneNumber, Elevation, StdBaroPress, StdRhoAir);
        }
    }

    void CheckLocationValidity()
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

        // REFERENCES:
        // Legacy subroutine CKBLDE.

        // Using/Aliasing
        using General::RoundSigDigits;

        bool LocationError;  // Set to true if there is a problem detected
        Real64 StdTimeMerid; // Standard time meridian
        Real64 DiffCalc;     // Difference between Standard Time Meridian and TimeZone

        LocationError = false;

        if ((Latitude == -999.0) && (Longitude == -999.0) && (TimeZoneNumber != -999.0)) {
            ShowSevereError("No location specified");
            LocationError = true;
        }

        if ((Latitude < -90.0) || (Latitude > 90.0)) {
            ShowSevereError("Latitude must be between -90 and 90; Entered=" + RoundSigDigits(Latitude, 2));
            LocationError = true;
        }

        if ((Longitude < -180.0) || (Longitude > 180.0)) {
            ShowSevereError("Longitude must be between -180 and 180; Entered=" + RoundSigDigits(Longitude, 2));
            LocationError = true;
        }

        if ((TimeZoneNumber < -12.00) || (TimeZoneNumber > 14.00)) {
            ShowSevereError("Time Zone must be between -12 and +14; Entered=" + RoundSigDigits(TimeZoneNumber, 2));
            LocationError = true;
        }

        StdTimeMerid = GetSTM(Longitude); // Obtain the standard time meridian.

        // Bias at +/- 12 for StdTimeMerid
        //  IF (StdTimeMerid == -12.0 .and. TimeZoneNumber > 0) THEN
        //    StdTimeMerid=12.0
        //  ELSEIF (StdTimeMerid == 12.0 .and. TimeZoneNumber < 0) THEN
        //    StdTimeMerid=-12.0
        //  ENDIF

        // Compare the standard time meridian with the time zone number.  If
        // different, notify the user.  If StdTimeMerid couldn't be calculated,
        // produce an error message.

        if (DataEnvironment::varyingLocationSchedIndexLat > 0 || DataEnvironment::varyingLocationSchedIndexLong > 0) {
            // don't do any warnings, the building is moving
        } else if (StdTimeMerid >= -12.0 && StdTimeMerid <= 12.0) {
            if (TimeZoneNumber != StdTimeMerid) {
                DiffCalc = std::abs(TimeZoneNumber - StdTimeMerid);
                if (DiffCalc > 1.0 && DiffCalc < 24.0) {
                    if (DiffCalc < 3.0) {
                        ShowWarningError("Standard Time Meridian and Time Zone differ by more than 1, Difference=\"" + RoundSigDigits(DiffCalc, 1) +
                                         "\"");
                        ShowContinueError("Solar Positions may be incorrect");
                    } else {
                        ShowSevereError("Standard Time Meridian and Time Zone differ by more than 2, Difference=\"" + RoundSigDigits(DiffCalc, 1) +
                                        "\"");
                        ShowContinueError("Solar Positions will be incorrect");
                        //          LocationError=.TRUE.
                    }
                }
            }
        } else {
            ShowSevereError("Unable to calculate the standard time meridian");
            LocationError = true;
        }

        // Error handling:  if there are any errors in the location information
        // the simulation must be terminated

        if (LocationError) {
            ShowFatalError("Due to previous error condition, simulation terminated");
        }

        if (TimeZoneNumber <= 12.00) {
            TimeZoneMeridian = TimeZoneNumber * 15.0;
        } else {
            TimeZoneMeridian = TimeZoneNumber * 15.0 - 360.0;
        }
        SinLatitude = std::sin(DegToRadians * Latitude);
        CosLatitude = std::cos(DegToRadians * Latitude);

        if (Latitude == 0.0 && Longitude == 0.0 && TimeZoneNumber == 0.0) {
            ShowWarningError("Did you realize that you have Latitude=0.0, Longitude=0.0 and TimeZone=0.0?  Your building site is in the middle of "
                             "the Atlantic Ocean.");
        }
    }

    void CheckWeatherFileValidity(WeatherManagerData &dataWeatherManage)
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

        // REFERENCES:
        // Legacy subroutine CKBLDE.

        dataWeatherManage.ErrorInWeatherFile = false;
        if (!dataWeatherManage.WeatherFileExists) { // No weather file exists but the user requested one--print error message

            if (DoWeathSim) {
                ShowWarningError("Weather Environment(s) requested, but no weather file found");
                dataWeatherManage.ErrorInWeatherFile = true;
            }

        } // ... end of WeatherFileExists IF-THEN
    }

    void ReportOutputFileHeaders(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 1997
        //       MODIFIED       December 2017; Jason DeGraw
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

        // Format descriptor for the environment title
        static ObjexxFCL::gio::Fmt A("(a)");
        static std::string EnvironmentString(",5,Environment Title[],Latitude[deg],Longitude[deg],Time Zone[],Elevation[m]");
        static std::string TimeStepString(
            ",8,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType");
        static std::string DailyString(",5,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily ");
        static std::string MonthlyString(",2,Cumulative Days of Simulation[],Month[]  ! When Monthly ");
        static std::string RunPeriodString(",1,Cumulative Days of Simulation[] ! When Run Period ");
        static std::string YearlyString(",1,Calendar Year of Simulation[] ! When Annual ");

        AssignReportNumber(dataWeatherManager.EnvironmentReportNbr);
        if (dataWeatherManager.EnvironmentReportNbr != 1) { //  problem
            ShowFatalError("ReportOutputFileHeaders: Assigned report number for Environment title is not 1.  Contact Support.");
        }
        dataWeatherManager.EnvironmentReportChr = std::to_string(dataWeatherManager.EnvironmentReportNbr);
        strip(dataWeatherManager.EnvironmentReportChr);
        print(outputFiles.eso, "{}{}\n", dataWeatherManager.EnvironmentReportChr, EnvironmentString);
        print(outputFiles.mtr, "{}{}\n", dataWeatherManager.EnvironmentReportChr, EnvironmentString);

        AssignReportNumber(OutputProcessor::TimeStepStampReportNbr);
        OutputProcessor::TimeStepStampReportChr = std::to_string(OutputProcessor::TimeStepStampReportNbr);
        strip(OutputProcessor::TimeStepStampReportChr);
        print(outputFiles.eso, "{}{}\n", OutputProcessor::TimeStepStampReportChr, TimeStepString);
        print(outputFiles.mtr, "{}{}\n", OutputProcessor::TimeStepStampReportChr, TimeStepString);

        AssignReportNumber(OutputProcessor::DailyStampReportNbr);
        OutputProcessor::DailyStampReportChr = std::to_string(OutputProcessor::DailyStampReportNbr);
        strip(OutputProcessor::DailyStampReportChr);
        print(outputFiles.eso, "{}{}{}\n", OutputProcessor::DailyStampReportChr, DailyString, "Report Variables Requested");
        print(outputFiles.mtr, "{}{}{}\n", OutputProcessor::DailyStampReportChr, DailyString, "Meters Requested");

        AssignReportNumber(OutputProcessor::MonthlyStampReportNbr);
        OutputProcessor::MonthlyStampReportChr = std::to_string(OutputProcessor::MonthlyStampReportNbr);
        strip(OutputProcessor::MonthlyStampReportChr);
        print(outputFiles.eso, "{}{}{}\n", OutputProcessor::MonthlyStampReportChr, MonthlyString, "Report Variables Requested");
        print(outputFiles.mtr, "{}{}{}\n", OutputProcessor::MonthlyStampReportChr, MonthlyString, "Meters Requested");

        AssignReportNumber(OutputProcessor::RunPeriodStampReportNbr);
        OutputProcessor::RunPeriodStampReportChr = std::to_string(OutputProcessor::RunPeriodStampReportNbr);
        strip(OutputProcessor::RunPeriodStampReportChr);
        print(outputFiles.eso, "{}{}{}\n", OutputProcessor::RunPeriodStampReportChr, RunPeriodString, "Report Variables Requested");
        print(outputFiles.mtr, "{}{}{}\n", OutputProcessor::RunPeriodStampReportChr, RunPeriodString, "Meters Requested");

        AssignReportNumber(OutputProcessor::YearlyStampReportNbr);
        OutputProcessor::YearlyStampReportChr = std::to_string(OutputProcessor::YearlyStampReportNbr);
        strip(OutputProcessor::YearlyStampReportChr);
        print(outputFiles.eso, "{}{}{}\n", OutputProcessor::YearlyStampReportChr, YearlyString, "Report Variables Requested");
        print(outputFiles.mtr, "{}{}{}\n", OutputProcessor::YearlyStampReportChr, YearlyString, "Meters Requested");
    }

    void ReportWeatherAndTimeInformation(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, bool &PrintEnvrnStamp) // Set to true when the environment header should be printed
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

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  CHARACTER(len=*), PARAMETER :: TimeStampFormat = "(i3,',',i4,',',i2,',',i2,',',i2)" ! Format descriptor for the date/time stamp

        // Report the time stamp and the current weather to the output file

        if (!WarmupFlag && !dataWeatherManager.RPReadAllWeatherData) { // Write the required output information

            // The first time through in a non-warmup day, the environment header
            // must be printed.  This must be done here and not in the generic
            // BeginEnvrnFlag block above because other modules in the simulation
            // must also print out header information.  This can be done during
            // the simulation warmup if the environment stamp printing is delayed
            // until the warmup is completed.  The stamp should only be printed once
            // per environment (set/reset of PrintEnvrnStamp).  In addition, before
            // the first environment, the end of the header block flag must also be
            // sent to the output file.

            if (PrintEnvrnStamp) {

                if (PrintEndDataDictionary && DoOutputReporting) {
                    static constexpr auto EndOfHeaderString("End of Data Dictionary"); // End of data dictionary marker
                    print(outputFiles.eso, "{}\n", EndOfHeaderString);
                    print(outputFiles.mtr, "{}\n", EndOfHeaderString);
                    PrintEndDataDictionary = false;
                }
                if (DoOutputReporting) {
                    std::string const &Title(dataWeatherManager.Environment(dataWeatherManager.Envrn).Title);
                    static constexpr auto EnvironmentStampFormatStr("{},{},{:7.2F},{:7.2F},{:7.2F},{:7.2F}\n"); // Format descriptor for environ stamp
                    print(outputFiles.eso, EnvironmentStampFormatStr, dataWeatherManager.EnvironmentReportChr, Title, Latitude, Longitude, TimeZoneNumber, Elevation);
                    print(outputFiles.mtr, EnvironmentStampFormatStr, dataWeatherManager.EnvironmentReportChr, Title, Latitude, Longitude, TimeZoneNumber, Elevation);
                    PrintEnvrnStamp = false;
                }
            }
        } // ... end of .NOT.WarmupFlag IF-THEN block.
    }

    void ReadUserWeatherInput(EnergyPlusData &state)
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

        // Using/Aliasing
        using namespace DataSystemVariables;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Env; // Environment Loop Counter
        static bool ErrorsFound(false);
        int RPD1;
        int RPD2;

        // Get the number of design days and annual runs from user inpout
        TotDesDays = inputProcessor->getNumObjectsFound("SizingPeriod:DesignDay");
        RPD1 = inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileDays");
        RPD2 = inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileConditionType");
        state.dataWeatherManager.TotRunPers = inputProcessor->getNumObjectsFound("RunPeriod");
        state.dataWeatherManager.NumOfEnvrn = TotDesDays + state.dataWeatherManager.TotRunPers + RPD1 + RPD2;
        if (state.dataWeatherManager.TotRunPers > 0) {
            WeathSimReq = true;
        } else {
            WeathSimReq = false;
        }

        state.dataWeatherManager.SPSiteScheduleNamePtr.allocate(TotDesDays * 5);
        state.dataWeatherManager.SPSiteScheduleUnits.allocate(TotDesDays * 5);

        state.dataWeatherManager.SPSiteScheduleNamePtr = 0;
        state.dataWeatherManager.SPSiteScheduleUnits = BlankString;

        // Allocate the Design Day and Environment array to the # of DD's or/and
        // Annual runs on input file
        state.dataWeatherManager.DesignDay.allocate(TotDesDays);
        state.dataWeatherManager.Environment.allocate(state.dataWeatherManager.NumOfEnvrn);

        // Set all Environments to DesignDay and then the weather environment will be set
        //  in the get annual run data subroutine
        for (Env = 1; Env <= TotDesDays; ++Env) {
            state.dataWeatherManager.Environment(Env).KindOfEnvrn = ksDesignDay;
        }
        for (Env = 1; Env <= RPD1 + RPD2; ++Env) {
            if (!DDOnly) {
                state.dataWeatherManager.Environment(TotDesDays + Env).KindOfEnvrn = ksRunPeriodDesign;
            } else {
                state.dataWeatherManager.Environment(TotDesDays + Env).KindOfEnvrn = ksRunPeriodWeather;
            }
        }
        for (Env = 1; Env <= state.dataWeatherManager.TotRunPers; ++Env) {
            state.dataWeatherManager.Environment(TotDesDays + RPD1 + RPD2 + Env).KindOfEnvrn = ksRunPeriodWeather;
        }

        if (TotDesDays >= 1) {
            GetDesignDayData(state.dataWeatherManager, TotDesDays, ErrorsFound);
        }

        if (RPD1 >= 1 || RPD2 >= 1) {
            GetRunPeriodDesignData(state.dataWeatherManager, ErrorsFound);
        }

        // the last environment(s) is designated the weather environment if an annual run
        // is selected.  All of the design systems is done from the design day info
        // which will have to be completed to run the annual run.
        if (state.dataWeatherManager.TotRunPers >= 1 || FullAnnualRun) {
            GetRunPeriodData(state.dataWeatherManager, state.dataWeatherManager.TotRunPers, ErrorsFound);
        }

        if (FullAnnualRun) {
            // GetRunPeriodData may have reset the value of TotRunPers
            state.dataWeatherManager.NumOfEnvrn = TotDesDays + state.dataWeatherManager.TotRunPers + RPD1 + RPD2;
        }

        if (RPD1 >= 1 || RPD2 >= 1 || state.dataWeatherManager.TotRunPers >= 1 || FullAnnualRun) {
            GetSpecialDayPeriodData(state.dataWeatherManager, ErrorsFound);
            GetDSTData(state.dataWeatherManager, ErrorsFound);
            if (state.dataWeatherManager.IDFDaylightSaving) {
                state.dataWeatherManager.DST = state.dataWeatherManager.IDFDST;
            }
        }

        GetLocationInfo(state.dataWeatherManager, ErrorsFound);

        GetGroundTemps(state, ErrorsFound);

        GetGroundReflectances(state.dataWeatherManager, state.outputFiles, ErrorsFound);

        GetSnowGroundRefModifiers(state.dataWeatherManager, state.outputFiles, ErrorsFound);

        GetWaterMainsTemperatures(state.dataWeatherManager, ErrorsFound);

        GetWeatherStation(state.outputFiles, ErrorsFound);

        SetupEnvironmentTypes(state.dataWeatherManager);

        GetWeatherProperties(state.dataWeatherManager, ErrorsFound);

        // Deallocate ones used for schedule pointers
        state.dataWeatherManager.SPSiteScheduleNamePtr.deallocate();
        state.dataWeatherManager.SPSiteScheduleUnits.deallocate();

        if (ErrorsFound) {
            ShowFatalError("GetWeatherInput: Above errors cause termination");
        }
    }

    static int findYearForWeekday(int const month, int const day, WeekDay const weekday)
    {
        // Find a year that goes with a month/day and a weekday. A lookup table is used with the most recent year that includes
        // the date with the weekday specified.

        // Tu, W, Th, F, Sa, Su, M, Tu, W, Th, F, Sa, Su
        static std::array<int, 13> defaultYear{{2013, 2014, 2015, 2010, 2011, 2017, 2007, 2013, 2014, 2015, 2010, 2011, 2017}};

        int rem = calculateDayOfYear(month, day) % 7;
        return defaultYear[static_cast<int>(weekday) - rem + 5]; // static_cast<int>(weekday) - rem + 1 + 4
    }

    static int findLeapYearForWeekday(int const month, int const day, WeekDay const weekday)
    {
        // Find a leap year that goes with a month/day and a weekday. A lookup table is used with the most recent year that includes
        // the date with the weekday specified.

        // Tu, W, Th, F, Sa, Su, M, Tu, W, Th, F, Sa, Su
        static std::array<int, 13> defaultLeapYear{{2008, 1992, 2004, 2016, 2000, 2012, 1996, 2008, 1992, 2004, 2016, 2000, 2012}};

        int rem = calculateDayOfYear(month, day, true) % 7;
        return defaultLeapYear[static_cast<int>(weekday) - rem + 5]; // static_cast<int>(weekday) - rem + 1 + 4
    }

    void GetRunPeriodData(WeatherManagerData &dataWeatherManager, int &TotRunPers, // Total number of Run Periods requested
                          bool &ErrorsFound)
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

        // Using/Aliasing
        using General::TrimSigDigits;
        using namespace DataSystemVariables;
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;   // Number of alphas being input
        int NumNumeric; // Number of numbers being input
        int IOStat;     // IO Status when calling get input subroutine
        int Loop;
        int Count;

        // Call Input Get routine to retrieve annual run data
        dataWeatherManager.RunPeriodInput.allocate(TotRunPers);
        dataWeatherManager.RunPeriodInputUniqueNames.reserve(static_cast<unsigned>(TotRunPers));

        cCurrentModuleObject = "RunPeriod";
        Count = 0;
        // if ( ! WFAllowsLeapYears ) {
        //   LocalLeapYearAdd = 0;
        // } else {
        //  LocalLeapYearAdd = 1;
        // }
        for (Loop = 1; Loop <= TotRunPers; ++Loop) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumeric,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            // A1, \field Name
            if (!lAlphaFieldBlanks(1)) {
                GlobalNames::VerifyUniqueInterObjectName(
                    dataWeatherManager.RunPeriodInputUniqueNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            }

            ++Count;
            // Loop = RP + Ptr;
            // Note JM 2018-11-20: IDD allows blank name, but input processor will create a name such as "RUNPERIOD 1" anyways
            // which is fine for our reporting below
            dataWeatherManager.RunPeriodInput(Loop).title = cAlphaArgs(1);

            // set the start and end day of month from user input
            // N1 , \field Begin Month
            // N2 , \field Begin Day of Month
            // N3,  \field Start Year
            // N4 , \field End Month
            // N5 , \field End Day of Month
            // N6,  \field End Year
            dataWeatherManager.RunPeriodInput(Loop).startMonth = int(rNumericArgs(1));
            dataWeatherManager.RunPeriodInput(Loop).startDay = int(rNumericArgs(2));
            dataWeatherManager.RunPeriodInput(Loop).startYear = int(rNumericArgs(3));
            dataWeatherManager.RunPeriodInput(Loop).endMonth = int(rNumericArgs(4));
            dataWeatherManager.RunPeriodInput(Loop).endDay = int(rNumericArgs(5));
            dataWeatherManager.RunPeriodInput(Loop).endYear = int(rNumericArgs(6));
            dataWeatherManager.RunPeriodInput(Loop).TreatYearsAsConsecutive = true;

            if (FullAnnualRun && Loop == 1) {
                dataWeatherManager.RunPeriodInput(Loop).startMonth = 1;
                dataWeatherManager.RunPeriodInput(Loop).startDay = 1;
                dataWeatherManager.RunPeriodInput(Loop).endMonth = 12;
                dataWeatherManager.RunPeriodInput(Loop).endDay = 31;
            }

            // Validate year inputs
            if (dataWeatherManager.RunPeriodInput(Loop).startYear == 0) {
                if (dataWeatherManager.RunPeriodInput(Loop).endYear != 0) { // Have to have an input start year to input an end year
                    ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title +
                                    ", end year cannot be specified if the start year is not.");
                    ErrorsFound = true;
                }
            } else if (dataWeatherManager.RunPeriodInput(Loop).startYear < 1583) { // Bail on the proleptic Gregorian calendar
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", start year (" +
                                std::to_string(dataWeatherManager.RunPeriodInput(Loop).startYear) + ") is too early, please choose a date after 1582.");
                ErrorsFound = true;
            }

            if (dataWeatherManager.RunPeriodInput(Loop).endYear != 0 && dataWeatherManager.RunPeriodInput(Loop).startYear > dataWeatherManager.RunPeriodInput(Loop).endYear) {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", start year (" +
                                std::to_string(dataWeatherManager.RunPeriodInput(Loop).startYear) + ") is after the end year (" +
                                std::to_string(dataWeatherManager.RunPeriodInput(Loop).endYear) + ").");
                ErrorsFound = true;
            }

            // A2 , \field Day of Week for Start Day
            bool inputWeekday = false;
            if (!lAlphaFieldBlanks(2)) { // Have input
                auto result = weekDayLookUp.find(cAlphaArgs(2));
                if (result == weekDayLookUp.end()) {
                    ShowWarningError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + cAlphaFieldNames(2) +
                                     " invalid (Day of Week) [" + cAlphaArgs(2) + "] for Start is not valid, Sunday will be used.");
                    dataWeatherManager.RunPeriodInput(Loop).startWeekDay = WeekDay::Sunday;
                } else {
                    dataWeatherManager.RunPeriodInput(Loop).startWeekDay = result->second;
                    inputWeekday = true;
                }
            } else { // No input, set the default as Sunday. This may get overriden below
                dataWeatherManager.RunPeriodInput(Loop).startWeekDay = WeekDay::Sunday;
            }

            // Validate the dates now that the weekday field has been looked at
            if (dataWeatherManager.RunPeriodInput(Loop).startMonth == 2 && dataWeatherManager.RunPeriodInput(Loop).startDay == 29) {
                // Requested start date is a leap year
                if (dataWeatherManager.RunPeriodInput(Loop).startYear == 0) { // No input starting year
                    if (inputWeekday) {
                        dataWeatherManager.RunPeriodInput(Loop).startYear =
                            findLeapYearForWeekday(dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay, dataWeatherManager.RunPeriodInput(Loop).startWeekDay);
                    } else {
                        // 2012 is the default year, 1/1 is a Sunday
                        dataWeatherManager.RunPeriodInput(Loop).startYear = 2012;
                        dataWeatherManager.RunPeriodInput(Loop).startWeekDay =
                            calculateDayOfWeek(dataWeatherManager.RunPeriodInput(Loop).startYear, dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay);
                    }
                } else {                                               // Have an input start year
                    if (!isLeapYear(dataWeatherManager.RunPeriodInput(Loop).startYear)) { // Start year is not a leap year
                        ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", start year (" +
                                        std::to_string(dataWeatherManager.RunPeriodInput(Loop).startYear) +
                                        ") is not a leap year but the requested start date is 2/29.");
                        ErrorsFound = true;
                    } else { // Start year is a leap year
                        WeekDay weekday =
                            calculateDayOfWeek(dataWeatherManager.RunPeriodInput(Loop).startYear, dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay);
                        if (inputWeekday) { // Check for correctness of input
                            if (weekday != dataWeatherManager.RunPeriodInput(Loop).startWeekDay) {
                                ShowWarningError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", start weekday (" +
                                                 cAlphaArgs(2) + ") does not match the start year (" +
                                                 std::to_string(dataWeatherManager.RunPeriodInput(Loop).startYear) + "), corrected to " +
                                                 DaysOfWeek(static_cast<int>(weekday)) + ".");
                                dataWeatherManager.RunPeriodInput(Loop).startWeekDay = weekday;
                            }
                        } else { // Set the weekday if it was not input
                            dataWeatherManager.RunPeriodInput(Loop).startWeekDay = weekday;
                        }
                    }
                }
            } else {
                // Non leap-day start date
                if (!validMonthDay(dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay)) {
                    ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", Invalid input start month/day (" +
                                    TrimSigDigits(dataWeatherManager.RunPeriodInput(Loop).startMonth) + '/' + TrimSigDigits(dataWeatherManager.RunPeriodInput(Loop).startDay) + ')');
                    ErrorsFound = true;
                } else {                                       // Month/day is valid
                    if (dataWeatherManager.RunPeriodInput(Loop).startYear == 0) { // No input starting year
                        if (inputWeekday) {
                            dataWeatherManager.RunPeriodInput(Loop).startYear =
                                findYearForWeekday(dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay, dataWeatherManager.RunPeriodInput(Loop).startWeekDay);
                        } else {
                            // 2017 is the default year, 1/1 is a Sunday
                            dataWeatherManager.RunPeriodInput(Loop).startYear = 2017;
                            dataWeatherManager.RunPeriodInput(Loop).startWeekDay =
                                calculateDayOfWeek(dataWeatherManager.RunPeriodInput(Loop).startYear, dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay);
                        }
                    } else { // Have an input starting year
                        WeekDay weekday =
                            calculateDayOfWeek(dataWeatherManager.RunPeriodInput(Loop).startYear, dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay);
                        if (inputWeekday) { // Check for correctness of input
                            if (weekday != dataWeatherManager.RunPeriodInput(Loop).startWeekDay) {
                                ShowWarningError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", start weekday (" +
                                                 cAlphaArgs(2) + ") does not match the start year (" +
                                                 std::to_string(dataWeatherManager.RunPeriodInput(Loop).startYear) + "), corrected to " +
                                                 DaysOfWeek(static_cast<int>(weekday)) + ".");
                                dataWeatherManager.RunPeriodInput(Loop).startWeekDay = weekday;
                            }
                        } else { // Set the weekday if it was not input
                            dataWeatherManager.RunPeriodInput(Loop).startWeekDay = weekday;
                        }
                    }
                }
            }

            // Compute the Julian date of the start date
            dataWeatherManager.RunPeriodInput(Loop).startJulianDate =
                computeJulianDate(dataWeatherManager.RunPeriodInput(Loop).startYear, dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay);

            // Validate the end date
            if (dataWeatherManager.RunPeriodInput(Loop).endMonth == 2 && dataWeatherManager.RunPeriodInput(Loop).endDay == 29) {
                // Requested end date is a leap year
                if (dataWeatherManager.RunPeriodInput(Loop).endYear == 0) { // No input end year
                    if (isLeapYear(dataWeatherManager.RunPeriodInput(Loop).startYear) && dataWeatherManager.RunPeriodInput(Loop).startMonth < 3) {
                        // The run period is from some date on or before 2/29 through 2/29
                        dataWeatherManager.RunPeriodInput(Loop).endYear = dataWeatherManager.RunPeriodInput(Loop).startYear;
                    } else {
                        // There might be a better approach here, but for now just loop forward for the next leap year
                        for (int yr = dataWeatherManager.RunPeriodInput(Loop).startYear + 1; yr < dataWeatherManager.RunPeriodInput(Loop).startYear + 10; yr++) {
                            if (isLeapYear(yr)) {
                                dataWeatherManager.RunPeriodInput(Loop).endYear = yr;
                                break;
                            }
                        }
                    }
                } else {                                             // Have an input end year
                    if (!isLeapYear(dataWeatherManager.RunPeriodInput(Loop).endYear)) { // End year is not a leap year
                        ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", end year (" +
                                        std::to_string(dataWeatherManager.RunPeriodInput(Loop).startYear) + ") is not a leap year but the requested end date is 2/29.");
                        ErrorsFound = true;
                    } else {
                        dataWeatherManager.RunPeriodInput(Loop).endJulianDate =
                            computeJulianDate(dataWeatherManager.RunPeriodInput(Loop).endYear, dataWeatherManager.RunPeriodInput(Loop).endMonth, dataWeatherManager.RunPeriodInput(Loop).endDay);
                        if (dataWeatherManager.RunPeriodInput(Loop).startJulianDate > dataWeatherManager.RunPeriodInput(Loop).endJulianDate) {
                            ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", start Julian date (" +
                                            std::to_string(dataWeatherManager.RunPeriodInput(Loop).startJulianDate) + ") is after the end Julian date (" +
                                            std::to_string(dataWeatherManager.RunPeriodInput(Loop).endJulianDate) + ").");
                            ErrorsFound = true;
                        }
                    }
                }
            } else {
                // Non leap-day end date
                if (!validMonthDay(dataWeatherManager.RunPeriodInput(Loop).endMonth, dataWeatherManager.RunPeriodInput(Loop).endDay)) {
                    ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", Invalid input end month/day (" +
                                    TrimSigDigits(dataWeatherManager.RunPeriodInput(Loop).startMonth) + '/' + TrimSigDigits(dataWeatherManager.RunPeriodInput(Loop).startDay) + ')');
                    ErrorsFound = true;
                } else {                                     // Month/day is valid
                    if (dataWeatherManager.RunPeriodInput(Loop).endYear == 0) { // No input end year
                        // Assume same year as start year
                        dataWeatherManager.RunPeriodInput(Loop).endYear = dataWeatherManager.RunPeriodInput(Loop).startYear;
                        dataWeatherManager.RunPeriodInput(Loop).endJulianDate =
                            computeJulianDate(dataWeatherManager.RunPeriodInput(Loop).endYear, dataWeatherManager.RunPeriodInput(Loop).endMonth, dataWeatherManager.RunPeriodInput(Loop).endDay);
                        if (dataWeatherManager.RunPeriodInput(Loop).startJulianDate > dataWeatherManager.RunPeriodInput(Loop).endJulianDate) {
                            dataWeatherManager.RunPeriodInput(Loop).endJulianDate = 0; // Force recalculation later
                            dataWeatherManager.RunPeriodInput(Loop).endYear += 1;
                        }
                    } else { // Have an input end year
                        dataWeatherManager.RunPeriodInput(Loop).endJulianDate =
                            computeJulianDate(dataWeatherManager.RunPeriodInput(Loop).endYear, dataWeatherManager.RunPeriodInput(Loop).endMonth, dataWeatherManager.RunPeriodInput(Loop).endDay);
                        if (dataWeatherManager.RunPeriodInput(Loop).startJulianDate > dataWeatherManager.RunPeriodInput(Loop).endJulianDate) {
                            ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + ", start Julian date (" +
                                            std::to_string(dataWeatherManager.RunPeriodInput(Loop).startJulianDate) + ") is after the end Julian date (" +
                                            std::to_string(dataWeatherManager.RunPeriodInput(Loop).endJulianDate) + ").");
                            ErrorsFound = true;
                        }
                    }
                }
            }

            if (dataWeatherManager.RunPeriodInput(Loop).endJulianDate == 0) {
                dataWeatherManager.RunPeriodInput(Loop).endJulianDate =
                    computeJulianDate(dataWeatherManager.RunPeriodInput(Loop).endYear, dataWeatherManager.RunPeriodInput(Loop).endMonth, dataWeatherManager.RunPeriodInput(Loop).endDay);
            }

            dataWeatherManager.RunPeriodInput(Loop).numSimYears = dataWeatherManager.RunPeriodInput(Loop).endYear - dataWeatherManager.RunPeriodInput(Loop).startYear + 1;

            // A3,  \field Use Weather File Holidays and Special Days
            if (lAlphaFieldBlanks(3) || UtilityRoutines::SameString(cAlphaArgs(3), "YES")) {
                dataWeatherManager.RunPeriodInput(Loop).useHolidays = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "NO")) {
                dataWeatherManager.RunPeriodInput(Loop).useHolidays = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + cAlphaFieldNames(3) + " invalid [" + cAlphaArgs(3) +
                                ']');
                ErrorsFound = true;
            }

            // A4,  \field Use Weather File Daylight Saving Period
            if (lAlphaFieldBlanks(4) || UtilityRoutines::SameString(cAlphaArgs(4), "YES")) {
                dataWeatherManager.RunPeriodInput(Loop).useDST = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(4), "NO")) {
                dataWeatherManager.RunPeriodInput(Loop).useDST = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + cAlphaFieldNames(4) + " invalid [" + cAlphaArgs(4) +
                                ']');
                ErrorsFound = true;
            }

            // A5,  \field Apply Weekend Holiday Rule
            if (lAlphaFieldBlanks(5) || UtilityRoutines::SameString(cAlphaArgs(5), "YES")) {
                dataWeatherManager.RunPeriodInput(Loop).applyWeekendRule = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "NO")) {
                dataWeatherManager.RunPeriodInput(Loop).applyWeekendRule = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + cAlphaFieldNames(5) + " invalid [" + cAlphaArgs(5) +
                                ']');
                ErrorsFound = true;
            }

            // A6,  \field Use Weather File Rain Indicators
            if (lAlphaFieldBlanks(6) || UtilityRoutines::SameString(cAlphaArgs(6), "YES")) {
                dataWeatherManager.RunPeriodInput(Loop).useRain = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(6), "NO")) {
                dataWeatherManager.RunPeriodInput(Loop).useRain = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + cAlphaFieldNames(6) + " invalid [" + cAlphaArgs(6) +
                                ']');
                ErrorsFound = true;
            }

            // A7,  \field Use Weather File Snow Indicators
            if (lAlphaFieldBlanks(7) || UtilityRoutines::SameString(cAlphaArgs(7), "YES")) {
                dataWeatherManager.RunPeriodInput(Loop).useSnow = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(7), "NO")) {
                dataWeatherManager.RunPeriodInput(Loop).useSnow = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + cAlphaFieldNames(7) + " invalid [" + cAlphaArgs(7) +
                                ']');
                ErrorsFound = true;
            }

            // A8,  \field Treat Weather as Actual
            if (lAlphaFieldBlanks(8) || UtilityRoutines::SameString(cAlphaArgs(8), "NO")) {
                dataWeatherManager.RunPeriodInput(Loop).actualWeather = false;
            } else if (UtilityRoutines::SameString(cAlphaArgs(8), "YES")) {
                dataWeatherManager.RunPeriodInput(Loop).actualWeather = true;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodInput(Loop).title + cAlphaFieldNames(8) + " invalid [" + cAlphaArgs(8) +
                                ']');
                ErrorsFound = true;
            }

            dataWeatherManager.RunPeriodInput(Loop).dayOfWeek = static_cast<int>(dataWeatherManager.RunPeriodInput(Loop).startWeekDay);
            dataWeatherManager.RunPeriodInput(Loop).isLeapYear = isLeapYear(dataWeatherManager.RunPeriodInput(Loop).startYear);

            // calculate the annual start and end days from the user inputted month and day
            dataWeatherManager.RunPeriodInput(Loop).monWeekDay = 0;
            if (dataWeatherManager.RunPeriodInput(Loop).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(dataWeatherManager,
                    dataWeatherManager.RunPeriodInput(Loop).startMonth, dataWeatherManager.RunPeriodInput(Loop).startDay, dataWeatherManager.RunPeriodInput(Loop).dayOfWeek, dataWeatherManager.RunPeriodInput(Loop).monWeekDay);
            }
        }

        if (TotRunPers == 0 && FullAnnualRun) {
            ShowWarningError("No Run Periods input but Full Annual Simulation selected.  Adding Run Period to 1/1 through 12/31.");
            dataWeatherManager.Environment.redimension(++dataWeatherManager.NumOfEnvrn);
            dataWeatherManager.Environment(dataWeatherManager.NumOfEnvrn).KindOfEnvrn = ksRunPeriodWeather;
            TotRunPers = 1;
            WeathSimReq = true;
            dataWeatherManager.RunPeriodInput.allocate(TotRunPers);
            dataWeatherManager.RunPeriodInput(1).startJulianDate = General::OrdinalDay(dataWeatherManager.RunPeriodInput(1).startMonth, dataWeatherManager.RunPeriodInput(1).startDay, dataWeatherManager.LeapYearAdd);
            dataWeatherManager.RunPeriodInput(1).endJulianDate = General::OrdinalDay(dataWeatherManager.RunPeriodInput(1).endMonth, dataWeatherManager.RunPeriodInput(1).endDay, dataWeatherManager.LeapYearAdd);
            dataWeatherManager.RunPeriodInput(1).monWeekDay = 0;
            if (dataWeatherManager.RunPeriodInput(1).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(dataWeatherManager,
                    dataWeatherManager.RunPeriodInput(1).startMonth, dataWeatherManager.RunPeriodInput(1).startDay, dataWeatherManager.RunPeriodInput(1).dayOfWeek, dataWeatherManager.RunPeriodInput(1).monWeekDay);
            }
        } else if (TotRunPers > 1 && FullAnnualRun) {
            TotRunPers = 1;
        }
    }

    void GetRunPeriodDesignData(WeatherManagerData &dataWeatherManager, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the run period design info from User input and the
        //  simulation dates

        // Using/Aliasing
        using General::TrimSigDigits;
        using namespace DataSystemVariables;
        using namespace DataIPShortCuts;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const ValidNames(12,
                                               {"SUNDAY",
                                                "MONDAY",
                                                "TUESDAY",
                                                "WEDNESDAY",
                                                "THURSDAY",
                                                "FRIDAY",
                                                "SATURDAY",
                                                "HOLIDAY",
                                                "SUMMERDESIGNDAY",
                                                "WINTERDESIGNDAY",
                                                "CUSTOMDAY1",
                                                "CUSTOMDAY2"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;   // Number of alphas being input
        int NumNumerics; // Number of Numerics being input
        int IOStat;      // IO Status when calling get input subroutine
        int Loop;
        int RPD1;
        int RPD2;
        int Count;
        int WhichPeriod;
        // unused1208  CHARACTER(len=MaxNameLength) :: ThisObject

        // FLOW:
        // Call Input Get routine to retrieve annual run data
        RPD1 = inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileDays");
        RPD2 = inputProcessor->getNumObjectsFound("SizingPeriod:WeatherFileConditionType");
        dataWeatherManager.TotRunDesPers = RPD1 + RPD2;

        dataWeatherManager.RunPeriodDesignInput.allocate(RPD1 + RPD2);
        dataWeatherManager.RunPeriodDesignInputUniqueNames.reserve(static_cast<unsigned>(RPD1 + RPD2));

        Count = 0;
        cCurrentModuleObject = "SizingPeriod:WeatherFileDays";
        for (Loop = 1; Loop <= RPD1; ++Loop) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumerics,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(
                dataWeatherManager.RunPeriodDesignInputUniqueNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            // Increment Count
            ++Count;
            dataWeatherManager.RunPeriodDesignInput(Count).title = cAlphaArgs(1);
            dataWeatherManager.RunPeriodDesignInput(Count).periodType = "User Selected WeatherFile RunPeriod (Design)";

            // set the start and end day of month from user input
            dataWeatherManager.RunPeriodDesignInput(Count).startMonth = int(rNumericArgs(1));
            dataWeatherManager.RunPeriodDesignInput(Count).startDay = int(rNumericArgs(2));
            dataWeatherManager.RunPeriodDesignInput(Count).endMonth = int(rNumericArgs(3));
            dataWeatherManager.RunPeriodDesignInput(Count).endDay = int(rNumericArgs(4));

            {
                auto const SELECT_CASE_var(dataWeatherManager.RunPeriodDesignInput(Count).startMonth);

                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 7) || (SELECT_CASE_var == 8) ||
                    (SELECT_CASE_var == 10) || (SELECT_CASE_var == 12)) {
                    if (dataWeatherManager.RunPeriodDesignInput(Count).startDay > 31) {
                        ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cNumericFieldNames(2) +
                                        " invalid (Day of Month) [" + TrimSigDigits(dataWeatherManager.RunPeriodDesignInput(Count).startDay) + ']');
                        ErrorsFound = true;
                    }
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 9) || (SELECT_CASE_var == 11)) {
                    if (dataWeatherManager.RunPeriodDesignInput(Count).startDay > 30) {
                        ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cNumericFieldNames(2) +
                                        " invalid (Day of Month) [" + TrimSigDigits(dataWeatherManager.RunPeriodDesignInput(Count).startDay) + ']');
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == 2) {
                    if (dataWeatherManager.RunPeriodDesignInput(Count).startDay > 28 + dataWeatherManager.LeapYearAdd) {
                        ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cNumericFieldNames(2) +
                                        " invalid (Day of Month) [" + TrimSigDigits(dataWeatherManager.RunPeriodDesignInput(Count).startDay) + ']');
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cNumericFieldNames(1) +
                                    " invalid (Month) [" + TrimSigDigits(dataWeatherManager.RunPeriodDesignInput(Count).startMonth) + ']');
                    ErrorsFound = true;
                }
            }

            if (lAlphaFieldBlanks(2)) {
                dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
            } else {
                dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek = UtilityRoutines::FindItemInList(cAlphaArgs(2), ValidNames, 12);
                if (dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek == 0 || dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek == 8) {
                    ShowWarningError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(1) +
                                     " invalid (Day of Week) [" + cAlphaArgs(1) + " for Start is not Valid, Monday will be Used.");
                    dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
                }
            }

            if (lAlphaFieldBlanks(3) || UtilityRoutines::SameString(cAlphaArgs(3), "YES")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useDST = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "NO")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useDST = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(3) + " invalid [" +
                                cAlphaArgs(3) + ']');
                ErrorsFound = true;
            }

            if (lAlphaFieldBlanks(4) || UtilityRoutines::SameString(cAlphaArgs(4), "YES")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useRain = true;
                dataWeatherManager.RunPeriodDesignInput(Count).useSnow = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(4), "NO")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useRain = false;
                dataWeatherManager.RunPeriodDesignInput(Count).useSnow = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(4) + " invalid [" +
                                cAlphaArgs(4) + ']');
                ErrorsFound = true;
            }

            // calculate the annual start and end days from the user inputted month and day
            dataWeatherManager.RunPeriodDesignInput(Count).startJulianDate =
                General::OrdinalDay(dataWeatherManager.RunPeriodDesignInput(Count).startMonth, dataWeatherManager.RunPeriodDesignInput(Count).startDay, dataWeatherManager.LeapYearAdd);
            dataWeatherManager.RunPeriodDesignInput(Count).endJulianDate =
                General::OrdinalDay(dataWeatherManager.RunPeriodDesignInput(Count).endMonth, dataWeatherManager.RunPeriodDesignInput(Count).endDay, dataWeatherManager.LeapYearAdd);
            if (dataWeatherManager.RunPeriodDesignInput(Count).startJulianDate <= dataWeatherManager.RunPeriodDesignInput(Count).endJulianDate) {
                dataWeatherManager.RunPeriodDesignInput(Count).totalDays =
                    (dataWeatherManager.RunPeriodDesignInput(Count).endJulianDate - dataWeatherManager.RunPeriodDesignInput(Count).startJulianDate + 1) *
                    dataWeatherManager.RunPeriodDesignInput(Count).numSimYears;
            } else {
                dataWeatherManager.RunPeriodDesignInput(Count).totalDays = (General::OrdinalDay(12, 31, dataWeatherManager.LeapYearAdd) - dataWeatherManager.RunPeriodDesignInput(Count).startJulianDate + 1 +
                                                         dataWeatherManager.RunPeriodDesignInput(Count).endJulianDate) *
                                                        dataWeatherManager.RunPeriodDesignInput(Count).numSimYears;
            }
            dataWeatherManager.RunPeriodDesignInput(Count).monWeekDay = 0;
            if (dataWeatherManager.RunPeriodDesignInput(1).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(dataWeatherManager, dataWeatherManager.RunPeriodDesignInput(1).startMonth,
                                     dataWeatherManager.RunPeriodDesignInput(1).startDay,
                                     dataWeatherManager.RunPeriodDesignInput(1).dayOfWeek,
                                     dataWeatherManager.RunPeriodDesignInput(1).monWeekDay);
            }
        }

        cCurrentModuleObject = "SizingPeriod:WeatherFileConditionType";
        for (Loop = 1; Loop <= RPD2; ++Loop) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumerics,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(
                dataWeatherManager.RunPeriodDesignInputUniqueNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

            // Increment count
            ++Count;
            dataWeatherManager.RunPeriodDesignInput(Count).title = cAlphaArgs(1);
            dataWeatherManager.RunPeriodDesignInput(Count).periodType = "User Selected WeatherFile Typical/Extreme Period (Design)=" + cAlphaArgs(2);

            // Period Selection
            if (!lAlphaFieldBlanks(2)) {
                WhichPeriod = UtilityRoutines::FindItem(cAlphaArgs(2), dataWeatherManager.TypicalExtremePeriods, &TypicalExtremeData::MatchValue);
                if (WhichPeriod != 0) {
                    dataWeatherManager.RunPeriodDesignInput(Count).startDay = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartDay;
                    dataWeatherManager.RunPeriodDesignInput(Count).startMonth = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartMonth;
                    dataWeatherManager.RunPeriodDesignInput(Count).startJulianDate = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartJDay;
                    dataWeatherManager.RunPeriodDesignInput(Count).endDay = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndDay;
                    dataWeatherManager.RunPeriodDesignInput(Count).endMonth = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndMonth;
                    dataWeatherManager.RunPeriodDesignInput(Count).endJulianDate = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndJDay;
                    dataWeatherManager.RunPeriodDesignInput(Count).totalDays = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).TotalDays;
                } else {
                    WhichPeriod = UtilityRoutines::FindItem(cAlphaArgs(2), dataWeatherManager.TypicalExtremePeriods, &TypicalExtremeData::MatchValue1);
                    if (WhichPeriod != 0) {
                        dataWeatherManager.RunPeriodDesignInput(Count).startDay = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartDay;
                        dataWeatherManager.RunPeriodDesignInput(Count).startMonth = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartMonth;
                        dataWeatherManager.RunPeriodDesignInput(Count).startJulianDate = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartJDay;
                        dataWeatherManager.RunPeriodDesignInput(Count).endDay = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndDay;
                        dataWeatherManager.RunPeriodDesignInput(Count).endMonth = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndMonth;
                        dataWeatherManager.RunPeriodDesignInput(Count).endJulianDate = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndJDay;
                        dataWeatherManager.RunPeriodDesignInput(Count).totalDays = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).TotalDays;
                        ShowWarningError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(2) + '=' +
                                         cAlphaArgs(2) + " matched to " + dataWeatherManager.TypicalExtremePeriods(WhichPeriod).MatchValue);
                    } else {
                        WhichPeriod = UtilityRoutines::FindItem(cAlphaArgs(2), dataWeatherManager.TypicalExtremePeriods, &TypicalExtremeData::MatchValue2);
                        if (WhichPeriod != 0) {
                            dataWeatherManager.RunPeriodDesignInput(Count).startDay = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartDay;
                            dataWeatherManager.RunPeriodDesignInput(Count).startMonth = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartMonth;
                            dataWeatherManager.RunPeriodDesignInput(Count).startJulianDate = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).StartJDay;
                            dataWeatherManager.RunPeriodDesignInput(Count).endDay = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndDay;
                            dataWeatherManager.RunPeriodDesignInput(Count).endMonth = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndMonth;
                            dataWeatherManager.RunPeriodDesignInput(Count).endJulianDate = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).EndJDay;
                            dataWeatherManager.RunPeriodDesignInput(Count).totalDays = dataWeatherManager.TypicalExtremePeriods(WhichPeriod).TotalDays;
                            ShowWarningError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(2) +
                                             '=' + cAlphaArgs(2) + " matched to " + dataWeatherManager.TypicalExtremePeriods(WhichPeriod).MatchValue);
                        } else {
                            ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(2) +
                                            " invalid (not on Weather File)=" + cAlphaArgs(2));
                            ErrorsFound = true;
                        }
                    }
                }
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(2) +
                                " invalid (blank).");
                ErrorsFound = true;
            }

            if (lAlphaFieldBlanks(3)) {
                dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
            } else {
                dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek = UtilityRoutines::FindItemInList(cAlphaArgs(3), ValidNames, 12);
                if (dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek == 0 || dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek == 8) {
                    ShowWarningError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(3) +
                                     " invalid (Day of Week) [" + cAlphaArgs(3) + " for Start is not Valid, Monday will be Used.");
                    dataWeatherManager.RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
                }
            }

            if (lAlphaFieldBlanks(4) || UtilityRoutines::SameString(cAlphaArgs(4), "YES")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useDST = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(4), "NO")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useDST = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(4) + " invalid [" +
                                cAlphaArgs(4) + ']');
                ErrorsFound = true;
            }

            if (lAlphaFieldBlanks(5) || UtilityRoutines::SameString(cAlphaArgs(5), "YES")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useRain = true;
                dataWeatherManager.RunPeriodDesignInput(Count).useSnow = true;
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "NO")) {
                dataWeatherManager.RunPeriodDesignInput(Count).useRain = false;
                dataWeatherManager.RunPeriodDesignInput(Count).useSnow = false;
            } else {
                ShowSevereError(cCurrentModuleObject + ": object=" + dataWeatherManager.RunPeriodDesignInput(Count).title + ' ' + cAlphaFieldNames(5) + " invalid [" +
                                cAlphaArgs(5) + ']');
                ErrorsFound = true;
            }
            dataWeatherManager.RunPeriodDesignInput(1).monWeekDay = 0;
            if (dataWeatherManager.RunPeriodDesignInput(1).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(dataWeatherManager, dataWeatherManager.RunPeriodDesignInput(1).startMonth,
                                     dataWeatherManager.RunPeriodDesignInput(1).startDay,
                                     dataWeatherManager.RunPeriodDesignInput(1).dayOfWeek,
                                     dataWeatherManager.RunPeriodDesignInput(1).monWeekDay);
            }
        }
    }

    void GetSpecialDayPeriodData(WeatherManagerData &dataWeatherManager, bool &ErrorsFound) // will be set to true if severe errors are found in inputs
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

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using General::TrimSigDigits;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const ValidDayTypes(5, {"HOLIDAY", "SUMMERDESIGNDAY", "WINTERDESIGNDAY", "CUSTOMDAY1", "CUSTOMDAY2"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string AlphArray(3);
        int NumAlphas;
        Array1D<Real64> Duration(1);
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

        cCurrentModuleObject = "RunPeriodControl:SpecialDays";
        NumSpecDays = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (allocated(dataWeatherManager.SpecialDays)) { // EPW already allocated the array
            Count = dataWeatherManager.NumSpecialDays - NumSpecDays + 1;
        } else {
            dataWeatherManager.SpecialDays.allocate(NumSpecDays);
            dataWeatherManager.NumSpecialDays = NumSpecDays;
            Count = 1;
        }

        for (Loop = 1; Loop <= NumSpecDays; ++Loop) {

            inputProcessor->getObjectItem(cCurrentModuleObject, Loop, AlphArray, NumAlphas, Duration, NumNumbers, IOStat);
            UtilityRoutines::IsNameEmpty(AlphArray(1), cCurrentModuleObject, ErrorsFound);
            dataWeatherManager.SpecialDays(Count).Name = AlphArray(1);

            ProcessDateString(AlphArray(2), PMonth, PDay, PWeekDay, DateType, ErrorsFound);
            if (DateType == dataWeatherManager.MonthDay) {
                dataWeatherManager.SpecialDays(Count).DateType = DateType;
                dataWeatherManager.SpecialDays(Count).Month = PMonth;
                dataWeatherManager.SpecialDays(Count).Day = PDay;
                dataWeatherManager.SpecialDays(Count).WeekDay = 0;
                dataWeatherManager.SpecialDays(Count).CompDate = PMonth * 32 + PDay;
                dataWeatherManager.SpecialDays(Count).WthrFile = false;
            } else if (DateType != dataWeatherManager.InvalidDate) {
                dataWeatherManager.SpecialDays(Count).DateType = DateType;
                dataWeatherManager.SpecialDays(Count).Month = PMonth;
                dataWeatherManager.SpecialDays(Count).Day = PDay;
                dataWeatherManager.SpecialDays(Count).WeekDay = PWeekDay;
                dataWeatherManager.SpecialDays(Count).CompDate = 0;
                dataWeatherManager.SpecialDays(Count).WthrFile = false;
            } else if (DateType == dataWeatherManager.InvalidDate) {
                ShowSevereError(cCurrentModuleObject + ": " + AlphArray(1) + " Invalid " + cAlphaFieldNames(2) + '=' + AlphArray(2));
                ErrorsFound = true;
            }

            if (Duration(1) > 0) {
                dataWeatherManager.SpecialDays(Count).Duration = int(Duration(1));
            } else {
                ShowSevereError(cCurrentModuleObject + ": " + AlphArray(1) + " Invalid " + cNumericFieldNames(1) + '=' +
                                TrimSigDigits(Duration(1), 0));
                ErrorsFound = true;
            }

            DayType = UtilityRoutines::FindItemInList(AlphArray(3), ValidDayTypes, 5);
            if (DayType == 0) {
                ShowSevereError(cCurrentModuleObject + ": " + AlphArray(1) + " Invalid " + cAlphaFieldNames(3) + '=' + AlphArray(3));
                ErrorsFound = true;
            } else {
                dataWeatherManager.SpecialDays(Count).DayType = DayType;
            }
            ++Count;
        }

        // CALL CalcSpecialDayTypes
    }

    void CalcSpecialDayTypes(WeatherManagerData &dataWeatherManager)
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

        int Loop;
        int Loop1;
        int JDay;
        int Warn;

        dataWeatherManager.SpecialDayTypes = 0; // Initialize/Reset Special Day Types array

        for (Loop = 1; Loop <= dataWeatherManager.NumSpecialDays; ++Loop) {

            if (dataWeatherManager.SpecialDays(Loop).WthrFile) continue;

            Warn = 0;

            JDay = General::OrdinalDay(dataWeatherManager.SpecialDays(Loop).Month, dataWeatherManager.SpecialDays(Loop).Day, dataWeatherManager.LeapYearAdd) - 1;

            for (Loop1 = 1; Loop1 <= dataWeatherManager.SpecialDays(Loop).Duration; ++Loop1) {
                ++JDay;
                if (JDay > 366) {
                    ShowWarningError("SpecialDay=" + dataWeatherManager.SpecialDays(Loop).Name + " causes index of more than 366, ignoring those beyond 366");
                } else {
                    if (dataWeatherManager.SpecialDayTypes(JDay) != 0 && Warn == 0) {
                        ShowWarningError("SpecialDay=" + dataWeatherManager.SpecialDays(Loop).Name + " attempted overwrite of previous set special day");
                        Warn = 1;
                    } else if (dataWeatherManager.SpecialDayTypes(JDay) == 0) {
                        dataWeatherManager.SpecialDayTypes(JDay) = dataWeatherManager.SpecialDays(Loop).DayType;
                    }
                }
            }
        }
    }

    void GetDSTData(WeatherManagerData &dataWeatherManager, bool &ErrorsFound) // will be set to true if severe errors are found in inputs
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

        // Using/Aliasing
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumFound;
        int NumAlphas;
        int IOStat;
        int NumNumbers;

        cCurrentModuleObject = "RunPeriodControl:DaylightSavingTime";
        NumFound = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumFound == 1) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          1,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            if (NumAlphas != 2) {
                ShowSevereError(cCurrentModuleObject + ": Insufficient fields, must have Start AND End Dates");
                ErrorsFound = true;
            } else { // Correct number of arguments
                ProcessDateString(cAlphaArgs(1), dataWeatherManager.IDFDST.StMon, dataWeatherManager.IDFDST.StDay, dataWeatherManager.IDFDST.StWeekDay, dataWeatherManager.IDFDST.StDateType, ErrorsFound);
                if (dataWeatherManager.IDFDST.StDateType == dataWeatherManager.InvalidDate) {
                    ShowSevereError(cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                    ErrorsFound = true;
                }
                ProcessDateString(cAlphaArgs(2), dataWeatherManager.IDFDST.EnMon, dataWeatherManager.IDFDST.EnDay, dataWeatherManager.IDFDST.EnWeekDay, dataWeatherManager.IDFDST.EnDateType, ErrorsFound);
                if (dataWeatherManager.IDFDST.EnDateType == dataWeatherManager.InvalidDate) {
                    ShowSevereError(cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                    ErrorsFound = true;
                }
                dataWeatherManager.IDFDaylightSaving = true;
            }
        } else if (NumFound > 1) {
            ShowSevereError(cCurrentModuleObject + ": Too many objects in Input File, only one allowed.");
            ErrorsFound = true;
        }
    }

    void GetDesignDayData(WeatherManagerData &dataWeatherManager, int &TotDesDays, // Total number of Design days to Setup
                          bool &ErrorsFound)
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
        using General::FindNumberInList;
        using General::RoundSigDigits;
        using ScheduleManager::CheckDayScheduleValueMinMax;
        using ScheduleManager::GetDayScheduleIndex;
        using ScheduleManager::GetSingleDayScheduleValues;
        using namespace DataSystemVariables;
        using namespace OutputReportPredefined;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const ValidNames(12,
                                               {"SUNDAY",
                                                "MONDAY",
                                                "TUESDAY",
                                                "WEDNESDAY",
                                                "THURSDAY",
                                                "FRIDAY",
                                                "SATURDAY",
                                                "HOLIDAY",
                                                "SUMMERDESIGNDAY",
                                                "WINTERDESIGNDAY",
                                                "CUSTOMDAY1",
                                                "CUSTOMDAY2"});
        static Array1D_string const HumidityIndicatingType({0, dataWeatherManager.DDHumIndType_Count - 1},
                                                           {"Wetbulb [C]",
                                                            "Dewpoint [C]",
                                                            "Enthalpy [J/kg]",
                                                            "Humidity Ratio []",
                                                            "Schedule []",
                                                            "WetBulbProfileDefaultMultipliers []",
                                                            "WetBulbProfileDifferenceSchedule []",
                                                            "WetBulbProfileMultiplierSchedule []"});

        //  REAL(r64), PARAMETER, DIMENSION(24) :: DefaultTempRangeMult=(/ .87d0,.92d0,.96d0,.99d0,1.0d0,.98d0,.93d0,  &
        //                   .84d0,.71d0,.56d0,.39d0,.23d0, .11d0,.03d0,.00d0,.03d0,.10d0,.21d0,.34d0,.47d0,.58d0,.68d0,.76d0,.82d0 /)
        // Below are the 2009 fractions, HOF, Chap 14, Table 6
        static Array1D<Real64> const DefaultTempRangeMult(24, {0.88, 0.92, 0.95, 0.98, 1.0,  0.98, 0.91, 0.74, 0.55, 0.38, 0.23, 0.13,
                                                               0.05, 0.00, 0.00, 0.06, 0.14, 0.24, 0.39, 0.50, 0.59, 0.68, 0.75, 0.82});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int EnvrnNum;    // Environment Loop to pass to Design Day Setup Routine
        int NumAlpha;    // Number of material alpha names being passed
        int NumNumerics; // Number of material properties being passed
        int IOStat;      // IO Status when calling get input subroutine
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
        OutputProcessor::Unit unitType;
        int schPtr;
        bool MaxDryBulbEntered;
        bool PressureEntered;

        dataWeatherManager.DesDayInput.allocate(TotDesDays); // Allocate the array to the # of DD's
        dataWeatherManager.DDDBRngModifier.allocate(NumOfTimeStepInHour, 24, TotDesDays);
        dataWeatherManager.DDDBRngModifier = 0.0;
        dataWeatherManager.DDHumIndModifier.allocate(NumOfTimeStepInHour, 24, TotDesDays);
        dataWeatherManager.DDHumIndModifier = 0.0;
        dataWeatherManager.DDBeamSolarValues.allocate(NumOfTimeStepInHour, 24, TotDesDays);
        dataWeatherManager.DDBeamSolarValues = 0.0;
        dataWeatherManager.DDDiffuseSolarValues.allocate(NumOfTimeStepInHour, 24, TotDesDays);
        dataWeatherManager.DDDiffuseSolarValues = 0.0;
        dataWeatherManager.DDSkyTempScheduleValues.allocate(NumOfTimeStepInHour, 24, TotDesDays);
        dataWeatherManager.DDSkyTempScheduleValues = 0.0;

        dataWeatherManager.SPSiteDryBulbRangeModScheduleValue.dimension(TotDesDays, 0.0);
        dataWeatherManager.SPSiteHumidityConditionScheduleValue.dimension(TotDesDays, 0.0);
        dataWeatherManager.SPSiteBeamSolarScheduleValue.dimension(TotDesDays, 0.0);
        dataWeatherManager.SPSiteDiffuseSolarScheduleValue.dimension(TotDesDays, 0.0);
        dataWeatherManager.SPSiteSkyTemperatureScheduleValue.dimension(TotDesDays, 0.0);

        if (ReverseDD && TotDesDays <= 1) {
            ShowSevereError("GetDesignDayData: Reverse Design Day requested but # Design Days <=1");
        }

        cCurrentModuleObject = "SizingPeriod:DesignDay";
        for (DDLoop = 1; DDLoop <= TotDesDays; ++DDLoop) {

            if (ReverseDD) {
                if (DDLoop == 1 && TotDesDays > 1) {
                    EnvrnNum = 2;
                } else if (DDLoop == 2) {
                    EnvrnNum = 1;
                } else {
                    EnvrnNum = DDLoop;
                }
            } else {
                EnvrnNum = DDLoop;
            }

            // Call Input Get routine to retrieve design day data
            MaxDryBulbEntered = false;
            PressureEntered = false;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          DDLoop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumerics,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            dataWeatherManager.DesDayInput(EnvrnNum).Title = cAlphaArgs(1); // Environment name
            dataWeatherManager.Environment(EnvrnNum).Title = dataWeatherManager.DesDayInput(EnvrnNum).Title;

            //   N3,  \field Maximum Dry-Bulb Temperature
            //   N4,  \field Daily Dry-Bulb Temperature Range
            //   N9,  \field Barometric Pressure
            //   N10, \field Wind Speed
            //   N11, \field Wind Direction
            dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb = rNumericArgs(3); // Maximum Dry-Bulb Temperature (C)
            if (!lNumericFieldBlanks(3)) MaxDryBulbEntered = true;
            dataWeatherManager.DesDayInput(EnvrnNum).DailyDBRange = rNumericArgs(4); // Daily dry-bulb temperature range (deltaC)
            dataWeatherManager.DesDayInput(EnvrnNum).PressBarom = rNumericArgs(9);   // Atmospheric/Barometric Pressure (Pascals)
            if (!lNumericFieldBlanks(9)) PressureEntered = true;
            dataWeatherManager.DesDayInput(EnvrnNum).PressureEntered = PressureEntered;
            dataWeatherManager.DesDayInput(EnvrnNum).WindSpeed = rNumericArgs(10);           // Wind Speed (m/s)
            dataWeatherManager.DesDayInput(EnvrnNum).WindDir = mod(rNumericArgs(11), 360.0); // Wind Direction
            // (degrees clockwise from North, N=0, E=90, S=180, W=270)
            //   N1,  \field Month
            //   N2,  \field Day of Month
            //   N12, \field ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)
            //   N13, \field ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)
            //   N8,  \field Daily Wet-Bulb Temperature Range
            dataWeatherManager.DesDayInput(EnvrnNum).Month = int(rNumericArgs(1));      // Month of Year ( 1 - 12 )
            dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth = int(rNumericArgs(2)); // Day of Month ( 1 - 31 )
            dataWeatherManager.DesDayInput(EnvrnNum).TauB = rNumericArgs(12);           // beam tau >= 0
            dataWeatherManager.DesDayInput(EnvrnNum).TauD = rNumericArgs(13);           // diffuse tau >= 0
            dataWeatherManager.DesDayInput(EnvrnNum).DailyWBRange = rNumericArgs(8);    // Daily wet-bulb temperature range (deltaC)

            //   N14; \field Sky Clearness
            dataWeatherManager.DesDayInput(EnvrnNum).SkyClear = rNumericArgs(14); // Sky Clearness (0 to 1)

            //   N15, \field Maximum Warmup Days Between Sizing Periods
            if (lNumericFieldBlanks(15)) {
                // Default to -1 if not input
                dataWeatherManager.DesDayInput(EnvrnNum).maxWarmupDays = -1;
            } else {
                dataWeatherManager.DesDayInput(EnvrnNum).maxWarmupDays = int(rNumericArgs(15));
            }
            //   A13, \field Begin Environment Reset Mode
            if (lAlphaFieldBlanks(13)) {
                dataWeatherManager.DesDayInput(EnvrnNum).suppressBegEnvReset = false;
            } else {
                if (UtilityRoutines::SameString(cAlphaArgs(13), "FullResetAtBeginEnvironment")) {
                    dataWeatherManager.DesDayInput(EnvrnNum).suppressBegEnvReset = false;
                } else if (UtilityRoutines::SameString(cAlphaArgs(13), "SuppressThermalResetAtBeginEnvironment")) {
                    dataWeatherManager.DesDayInput(EnvrnNum).suppressBegEnvReset = true;
                }
            }
            // for PerformancePrecisionTradeoffs
            if (DataEnvironment::forceBeginEnvResetSuppress) {
                dataWeatherManager.DesDayInput(EnvrnNum).suppressBegEnvReset = true;
            }
            //   A7,  \field Rain Indicator
            if (UtilityRoutines::SameString(cAlphaArgs(7), "Yes")) {
                dataWeatherManager.DesDayInput(EnvrnNum).RainInd = 1;
            } else if (UtilityRoutines::SameString(cAlphaArgs(7), "No") || lAlphaFieldBlanks(7)) {
                dataWeatherManager.DesDayInput(EnvrnNum).RainInd = 0;
            } else {
                ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid field: " + cAlphaFieldNames(7) + "=\"" +
                                 cAlphaArgs(7) + "\".");
                ShowContinueError("\"No\" will be used.");
                dataWeatherManager.DesDayInput(EnvrnNum).RainInd = 0;
            }

            //   A8,  \field Snow Indicator
            if (UtilityRoutines::SameString(cAlphaArgs(8), "Yes")) {
                dataWeatherManager.DesDayInput(EnvrnNum).SnowInd = 1;
            } else if (UtilityRoutines::SameString(cAlphaArgs(8), "No") || lAlphaFieldBlanks(8)) {
                dataWeatherManager.DesDayInput(EnvrnNum).SnowInd = 0;
            } else {
                ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid field: " + cAlphaFieldNames(8) + "=\"" +
                                 cAlphaArgs(8) + "\".");
                ShowContinueError("\"No\" will be used.");
                dataWeatherManager.DesDayInput(EnvrnNum).SnowInd = 0;
            }

            //   A3,  \field Dry-Bulb Temperature Range Modifier Type
            // check DB profile input
            if (lAlphaFieldBlanks(3)) {
                cAlphaArgs(3) = "DefaultMultipliers";
                dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType = dataWeatherManager.DDDBRangeType_Default;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "Multiplier") || UtilityRoutines::SameString(cAlphaArgs(3), "MultiplierSchedule")) {
                cAlphaArgs(3) = "MultiplierSchedule";
                dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType = dataWeatherManager.DDDBRangeType_Multiplier;
                units = "[]";
                unitType = OutputProcessor::Unit::None;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "Difference") || UtilityRoutines::SameString(cAlphaArgs(3), "Delta") ||
                       UtilityRoutines::SameString(cAlphaArgs(3), "DifferenceSchedule") ||
                       UtilityRoutines::SameString(cAlphaArgs(3), "DeltaSchedule")) {
                cAlphaArgs(3) = "DifferenceSchedule";
                dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType = dataWeatherManager.DDDBRangeType_Difference;
                units = "[deltaC]";
                unitType = OutputProcessor::Unit::deltaC;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "DefaultMultipliers")) {
                cAlphaArgs(3) = "DefaultMultipliers";
                dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType = dataWeatherManager.DDDBRangeType_Default;
                // Validate Temperature - Daily range
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "TemperatureProfileSchedule")) {
                cAlphaArgs(3) = "TemperatureProfileSchedule";
                dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType = dataWeatherManager.DDDBRangeType_Profile;
                units = "[C]";
                unitType = OutputProcessor::Unit::C;
            } else {
                ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                ShowContinueError("..invalid field: " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                ErrorsFound = true;
                cAlphaArgs(3) = "invalid field";
                dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType = dataWeatherManager.DDDBRangeType_Default;
            }

            if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType != dataWeatherManager.DDDBRangeType_Profile && !MaxDryBulbEntered && cAlphaArgs(3) != "invalid field") {
                ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                ShowContinueError("..invalid blank field: " + cNumericFieldNames(3));
                ShowContinueError("..this field is required when " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                ErrorsFound = true;
            }

            // Assume either "multiplier" option will make full use of range...
            if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType != dataWeatherManager.DDDBRangeType_Difference && dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType != dataWeatherManager.DDDBRangeType_Profile) {
                testval = dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb - dataWeatherManager.DesDayInput(EnvrnNum).DailyDBRange;
                errFlag = false;
                inputProcessor->rangeCheck(errFlag,
                                           cAlphaFieldNames(3),
                                           cCurrentModuleObject,
                                           "Severe",
                                           ">= -90",
                                           (testval >= -90.0),
                                           "<= 70",
                                           (testval <= 70.0),
                                           _,
                                           dataWeatherManager.DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            }

            //   A4,  \field Dry-Bulb Temperature Range Modifier Day Schedule Name
            if (dataWeatherManager.DesDayInput(EnvrnNum).DBTempRangeType != dataWeatherManager.DDDBRangeType_Default) {
                if (!lAlphaFieldBlanks(4)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr = GetDayScheduleIndex(cAlphaArgs(4));
                    if (dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                        ShowContinueError("..invalid field: " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                        ErrorsFound = true;
                    } else {
                        GetSingleDayScheduleValues(dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr, dataWeatherManager.DDDBRngModifier(_, _, EnvrnNum));
                        schPtr = FindNumberInList(dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr, dataWeatherManager.SPSiteScheduleNamePtr, dataWeatherManager.NumSPSiteScheduleNamePtrs);
                        if (schPtr == 0) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Drybulb Temperature Range Modifier Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteDryBulbRangeModScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(4));
                        } else if (dataWeatherManager.SPSiteScheduleUnits(schPtr) != units) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Drybulb Temperature Range Modifier Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteDryBulbRangeModScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(4));
                        }
                        if (cAlphaArgs(3) == "MultiplierSchedule") {
                            if (!CheckDayScheduleValueMinMax(dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr, 0.0, ">=", 1.0, "<=")) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                ShowContinueError("..invalid field: " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                                ShowContinueError("..Specified [Schedule] Dry-bulb Range Multiplier Values are not within [0.0, 1.0]");
                                ErrorsFound = true;
                            }
                        } else if (cAlphaArgs(3) == "DifferenceSchedule") { // delta, must be > 0.0
                            if (!CheckDayScheduleValueMinMax(dataWeatherManager.DesDayInput(EnvrnNum).TempRangeSchPtr, 0.0, ">=")) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                ShowContinueError("..invalid field: " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                                ShowSevereError("Some [Schedule] Dry-bulb Range Difference Values are < 0.0 [would make max larger].");
                                ErrorsFound = true;
                            }
                        }
                        if (cAlphaArgs(3) == "TemperatureProfileSchedule") {
                            testval = maxval(dataWeatherManager.DDDBRngModifier(_, _, EnvrnNum));
                            if (MaxDryBulbEntered) {
                                ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", data override.");
                                ShowContinueError(".." + cNumericFieldNames(3) + "=[" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb, 2) +
                                                  "] will be overwritten.");
                                ShowContinueError(".." + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                                ShowContinueError("..with max value=[" + RoundSigDigits(testval, 2) + "].");
                            }
                            dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb = testval;
                        }
                        testval = maxval(dataWeatherManager.DDDBRngModifier(_, _, EnvrnNum));
                        testval = dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb - testval;
                        errFlag = false;
                        inputProcessor->rangeCheck(errFlag,
                                                   cAlphaFieldNames(4),
                                                   cCurrentModuleObject,
                                                   "Severe",
                                                   ">= -90",
                                                   (testval >= -90.0),
                                                   "<= 70",
                                                   (testval <= 70.0),
                                                   _,
                                                   dataWeatherManager.DesDayInput(EnvrnNum).Title);
                        if (errFlag) {
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cAlphaFieldNames(4) + " is blank.");
                    ShowContinueError("..required when " + cAlphaFieldNames(3) + " indicates \"SCHEDULE\".");
                    ErrorsFound = true;
                }
            } else {
                // Default dry-bulb temperature Range
                LastHrValue = DefaultTempRangeMult(24);
                for (HrLoop = 1; HrLoop <= 24; ++HrLoop) {
                    for (TSLoop = 1; TSLoop <= NumOfTimeStepInHour; ++TSLoop) {
                        WNow = dataWeatherManager.Interpolation(TSLoop);
                        WPrev = 1.0 - WNow;
                        dataWeatherManager.DDDBRngModifier(TSLoop, HrLoop, EnvrnNum) = LastHrValue * WPrev + DefaultTempRangeMult(HrLoop) * WNow;
                    }
                    LastHrValue = DefaultTempRangeMult(HrLoop);
                }
            }

            //   A5,  \field Humidity Condition Type
            if (UtilityRoutines::SameString(cAlphaArgs(5), "WetBulb")) {
                cAlphaArgs(5) = "WetBulb";
                //   N5,  \field Wetbulb or DewPoint at Maximum Dry-Bulb
                if (!lNumericFieldBlanks(5)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(5) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                errFlag = false;
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_WetBulb;
                inputProcessor->rangeCheck(errFlag,
                                           cAlphaFieldNames(5) + " - Wet-Bulb",
                                           cCurrentModuleObject,
                                           "Severe",
                                           ">= -90",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue >= -90.0),
                                           "<= 70",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue <= 70.0),
                                           _,
                                           dataWeatherManager.DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    //        CALL ShowContinueError(TRIM(cCurrentModuleObject)//': Occured in '//TRIM(dataWeatherManager.DesDayInput(EnvrnNum)%Title))
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "DewPoint")) {
                cAlphaArgs(5) = "DewPoint";
                if (!lNumericFieldBlanks(5)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(5) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                errFlag = false;
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_DewPoint;
                inputProcessor->rangeCheck(errFlag,
                                           cAlphaFieldNames(5) + " - Dew-Point",
                                           cCurrentModuleObject,
                                           "Severe",
                                           ">= -90",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue >= -90.0),
                                           "<= 70",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue <= 70.0),
                                           _,
                                           dataWeatherManager.DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "HumidityRatio")) {
                cAlphaArgs(5) = "HumidityRatio";
                //   N6,  \field Humidity Ratio at Maximum Dry-Bulb
                if (!lNumericFieldBlanks(6)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(6); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(6) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                errFlag = false;
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_HumRatio;
                inputProcessor->rangeCheck(errFlag,
                                           cAlphaFieldNames(5) + " - Humidity-Ratio",
                                           cCurrentModuleObject,
                                           "Severe",
                                           ">= 0",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue >= 0.0),
                                           "<= .03",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue <= 0.03),
                                           _,
                                           dataWeatherManager.DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "Enthalpy")) {
                cAlphaArgs(5) = "Enthalpy";
                //   N7,  \field Enthalpy at Maximum Dry-Bulb {J/kg}.
                if (!lNumericFieldBlanks(7)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(7); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(7) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                errFlag = false;
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_Enthalpy;
                inputProcessor->rangeCheck(errFlag,
                                           cAlphaFieldNames(5) + " - Enthalpy",
                                           "SizingPeriod:DesignDay",
                                           "Severe",
                                           ">= 0.0",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue >= 0.0),
                                           "<= 130000",
                                           (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue <= 130000.0),
                                           _,
                                           dataWeatherManager.DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "RelativeHumiditySchedule")) {
                cAlphaArgs(5) = "RelativeHumiditySchedule";
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_RelHumSch;
                units = "[%]";
                unitType = OutputProcessor::Unit::Perc;
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "WetBulbProfileMultiplierSchedule")) {
                cAlphaArgs(5) = "WetBulbProfileMultiplierSchedule";
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_WBProfMul;
                units = "[]";
                unitType = OutputProcessor::Unit::None;
                if (!lNumericFieldBlanks(5)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(5) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "WetBulbProfileDifferenceSchedule")) {
                cAlphaArgs(5) = "WetBulbProfileDifferenceSchedule";
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_WBProfDif;
                units = "[]";
                unitType = OutputProcessor::Unit::None;
                if (!lNumericFieldBlanks(5)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(5) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(5), "WetBulbProfileDefaultMultipliers")) {
                cAlphaArgs(5) = "WetBulbProfileDefaultMultipliers";
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_WBProfDef;
                if (!lNumericFieldBlanks(5)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(5) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
            } else {
                ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                ShowContinueError("..invalid field: " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                ShowContinueError("WetBulb will be used. Maximum Dry Bulb will be used as WetBulb at Maximum Dry Bulb.");
                cAlphaArgs(5) = "WetBulb";
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType = dataWeatherManager.DDHumIndType_WetBulb;
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = rNumericArgs(3);
            }

            // resolve humidity schedule if needed
            //   A6,  \field Humidity Condition Day Schedule Name
            if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_RelHumSch || dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfMul ||
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDif) {
                if (lAlphaFieldBlanks(6)) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cAlphaFieldNames(6) + " is blank.");
                    ShowContinueError("..field is required when " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                } else {
                    dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr = GetDayScheduleIndex(cAlphaArgs(6));
                    if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr == 0) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                        ShowContinueError("..invalid field: " + cAlphaFieldNames(6) + "=\"" + cAlphaArgs(6) + "\".");
                        ShowContinueError("Default Humidity will be used (constant for day using Humidity Indicator Temp).");
                        // reset HumIndType ?
                    } else {

                        GetSingleDayScheduleValues(dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr, dataWeatherManager.DDHumIndModifier(_, _, EnvrnNum));

                        schPtr = FindNumberInList(dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr, dataWeatherManager.SPSiteScheduleNamePtr, dataWeatherManager.NumSPSiteScheduleNamePtrs);
                        if (schPtr == 0) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Humidity Condition Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteHumidityConditionScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(6));
                        } else if (dataWeatherManager.SPSiteScheduleUnits(schPtr) != units) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Humidity Condition Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteHumidityConditionScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(6));
                        }

                        {
                            auto const SELECT_CASE_var(dataWeatherManager.DesDayInput(EnvrnNum).HumIndType);

                            if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_RelHumSch) {
                                if (!CheckDayScheduleValueMinMax(dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr, 0.0, ">=", 100.0, "<=")) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                    ShowContinueError("..invalid field: " + cAlphaFieldNames(6) + "=\"" + cAlphaArgs(6) + "\".");
                                    ShowContinueError("Specified [Scheduled] Relative Humidity Values are not within [0.0, 100.0]");
                                    ErrorsFound = true;
                                }

                            } else if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_WBProfMul) {
                                // multiplier: use schedule value, check 0 <= v <= 1
                                if (!CheckDayScheduleValueMinMax(dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr, 0.0, ">=", 1.0, "<=")) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                    ShowContinueError("..invalid field: " + cAlphaFieldNames(6) + "=\"" + cAlphaArgs(6) + "\".");
                                    ShowContinueError("..Specified [Schedule] Wet-bulb Profile Range Multiplier Values are not within [0.0, 1.0]");
                                    ErrorsFound = true;
                                }

                            } else if (SELECT_CASE_var == dataWeatherManager.DDHumIndType_WBProfDif) {
                                if (!CheckDayScheduleValueMinMax(dataWeatherManager.DesDayInput(EnvrnNum).HumIndSchPtr, 0.0, ">=")) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                    ShowContinueError("..invalid field: " + cAlphaFieldNames(6) + "=\"" + cAlphaArgs(6) + "\".");
                                    ShowSevereError("Some [Schedule] Wet-bulb Profile Difference Values are < 0.0 [would make max larger].");
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                }

            } else if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDef) {
                // re WetBulbProfileDefaultMultipliers
                LastHrValue = DefaultTempRangeMult(24);
                for (HrLoop = 1; HrLoop <= 24; ++HrLoop) {
                    for (TSLoop = 1; TSLoop <= NumOfTimeStepInHour; ++TSLoop) {
                        WNow = dataWeatherManager.Interpolation(TSLoop);
                        WPrev = 1.0 - WNow;
                        dataWeatherManager.DDHumIndModifier(TSLoop, HrLoop, EnvrnNum) = LastHrValue * WPrev + DefaultTempRangeMult(HrLoop) * WNow;
                    }
                    LastHrValue = DefaultTempRangeMult(HrLoop);
                }
                // ELSE missing case?
            }

            // verify that design WB or DP <= design DB
            if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_DewPoint || dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WetBulb ||
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfMul || dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDef ||
                dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_WBProfDif) {
                if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue > dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", range check data.");
                    ShowContinueError("..Humidity Indicator Temperature at Max Temperature=" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue, 1) +
                                      " > Max DryBulb=" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb, 1));
                    ShowContinueError(".." + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ShowContinueError("..Conditions for day will be set to Relative Humidity = 100%");
                    if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType == dataWeatherManager.DDHumIndType_DewPoint) {
                        dataWeatherManager.DesDayInput(EnvrnNum).DewPointNeedsSet = true;
                    } else {
                        // wet-bulb
                        dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue = dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb;
                    }
                }
            }

            //   A10, \field Solar Model Indicator
            if (lAlphaFieldBlanks(10)) {
                dataWeatherManager.DesDayInput(EnvrnNum).SolarModel = dataWeatherManager.ASHRAE_ClearSky;
            } else if (UtilityRoutines::SameString(cAlphaArgs(10), "ASHRAEClearSky") || UtilityRoutines::SameString(cAlphaArgs(10), "CLEARSKY")) {
                dataWeatherManager.DesDayInput(EnvrnNum).SolarModel = dataWeatherManager.ASHRAE_ClearSky;
            } else if (UtilityRoutines::SameString(cAlphaArgs(10), "ZhangHuang")) {
                dataWeatherManager.DesDayInput(EnvrnNum).SolarModel = dataWeatherManager.Zhang_Huang;
            } else if (UtilityRoutines::SameString(cAlphaArgs(10), "ASHRAETau")) {
                dataWeatherManager.DesDayInput(EnvrnNum).SolarModel = dataWeatherManager.ASHRAE_Tau;
            } else if (UtilityRoutines::SameString(cAlphaArgs(10), "ASHRAETau2017")) {
                dataWeatherManager.DesDayInput(EnvrnNum).SolarModel = dataWeatherManager.ASHRAE_Tau2017;
            } else if (UtilityRoutines::SameString(cAlphaArgs(10), "Schedule")) {
                dataWeatherManager.DesDayInput(EnvrnNum).SolarModel = dataWeatherManager.SolarModel_Schedule;
            } else {
                ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                ShowContinueError("..invalid field: " + cAlphaFieldNames(10) + "=\"" + cAlphaArgs(10) + "\".");
                ShowContinueError("Model used will be ASHRAE ClearSky");
                dataWeatherManager.DesDayInput(EnvrnNum).SolarModel = dataWeatherManager.ASHRAE_ClearSky;
            }

            if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.SolarModel_Schedule) {
                //   A11, \field Beam Solar Day Schedule Name
                if (!lAlphaFieldBlanks(11)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).BeamSolarSchPtr = GetDayScheduleIndex(cAlphaArgs(11));
                    if (dataWeatherManager.DesDayInput(EnvrnNum).BeamSolarSchPtr == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                        ShowContinueError("..invalid field: " + cAlphaFieldNames(11) + "=\"" + cAlphaArgs(11) + "\".");
                        ShowContinueError("..Required when " + cAlphaFieldNames(10) + " indicates \"Schedule\".");
                        ErrorsFound = true;
                    } else {
                        GetSingleDayScheduleValues(dataWeatherManager.DesDayInput(EnvrnNum).BeamSolarSchPtr, dataWeatherManager.DDBeamSolarValues(_, _, EnvrnNum));
                        schPtr = FindNumberInList(dataWeatherManager.DesDayInput(EnvrnNum).BeamSolarSchPtr, dataWeatherManager.SPSiteScheduleNamePtr, dataWeatherManager.NumSPSiteScheduleNamePtrs);
                        units = "[W/m2]";
                        unitType = OutputProcessor::Unit::W_m2;
                        if (schPtr == 0) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).BeamSolarSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Beam Solar Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteBeamSolarScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(11));
                        } else if (dataWeatherManager.SPSiteScheduleUnits(schPtr) != units) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).BeamSolarSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Beam Solar Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteBeamSolarScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(11));
                        }
                        if (!CheckDayScheduleValueMinMax(dataWeatherManager.DesDayInput(EnvrnNum).BeamSolarSchPtr, 0.0, ">=")) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                            ShowContinueError("..invalid field: " + cAlphaFieldNames(11) + "=\"" + cAlphaArgs(11) + "\".");
                            ShowContinueError("..Specified [Schedule] Values are not >= 0.0");
                            ErrorsFound = true;
                        }
                    }
                } else { // should have entered beam schedule
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cAlphaFieldNames(11) + " is blank.");
                    ErrorsFound = true;
                }
                //   A12, \field Diffuse Solar Day Schedule Name
                if (!lAlphaFieldBlanks(12)) {
                    dataWeatherManager.DesDayInput(EnvrnNum).DiffuseSolarSchPtr = GetDayScheduleIndex(cAlphaArgs(12));
                    if (dataWeatherManager.DesDayInput(EnvrnNum).DiffuseSolarSchPtr == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                        ShowContinueError("..invalid field: " + cAlphaFieldNames(12) + "=\"" + cAlphaArgs(12) + "\".");
                        ShowContinueError("..Required when " + cAlphaFieldNames(10) + " indicates \"Schedule\".");
                        ErrorsFound = true;
                    } else {
                        GetSingleDayScheduleValues(dataWeatherManager.DesDayInput(EnvrnNum).DiffuseSolarSchPtr, dataWeatherManager.DDDiffuseSolarValues(_, _, EnvrnNum));
                        schPtr = FindNumberInList(dataWeatherManager.DesDayInput(EnvrnNum).DiffuseSolarSchPtr, dataWeatherManager.SPSiteScheduleNamePtr, dataWeatherManager.NumSPSiteScheduleNamePtrs);
                        units = "[W/m2]";
                        unitType = OutputProcessor::Unit::W_m2;
                        if (schPtr == 0) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).DiffuseSolarSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Diffuse Solar Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteDiffuseSolarScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(12));
                        } else if (dataWeatherManager.SPSiteScheduleUnits(schPtr) != units) {
                            ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                            dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = dataWeatherManager.DesDayInput(EnvrnNum).DiffuseSolarSchPtr;
                            dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable("Sizing Period Site Diffuse Solar Schedule Value",
                                                unitType,
                                                dataWeatherManager.SPSiteDiffuseSolarScheduleValue(EnvrnNum),
                                                "Zone",
                                                "Average",
                                                cAlphaArgs(12));
                        }
                        if (!CheckDayScheduleValueMinMax(dataWeatherManager.DesDayInput(EnvrnNum).DiffuseSolarSchPtr, 0.0, ">=")) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                            ShowContinueError("..invalid field: " + cAlphaFieldNames(12) + "=\"" + cAlphaArgs(12) + "\".");
                            ShowContinueError("..Specified [Schedule] Values are not >= 0.0");
                            ErrorsFound = true;
                        }
                    }
                } else { // should have entered diffuse schedule
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cAlphaFieldNames(12) + " is blank.");
                    ErrorsFound = true;
                }
            }

            if (dataWeatherManager.DesDayInput(EnvrnNum).SolarModel == dataWeatherManager.ASHRAE_ClearSky) {
                if (lNumericFieldBlanks(14)) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError("..invalid field: " + cNumericFieldNames(14) + " is blank.");
                    ShowContinueError("..Zero clear sky (no solar) will be used.");
                }
            }

            // Validate Design Day Month

            {
                auto const SELECT_CASE_var(dataWeatherManager.DesDayInput(EnvrnNum).Month);

                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 7) || (SELECT_CASE_var == 8) ||
                    (SELECT_CASE_var == 10) || (SELECT_CASE_var == 12)) {
                    if (dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth > 31) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                        ShowContinueError(".. invalid field: " + cNumericFieldNames(2) + "=[" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth) +
                                          "], Month=[" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).Month) + "].");
                        ErrorsFound = true;
                    }
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 9) || (SELECT_CASE_var == 11)) {
                    if (dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth > 30) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                        ShowContinueError(".. invalid " + cNumericFieldNames(2) + "=[" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth) +
                                          "], Month=[" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).Month) + "].");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == 2) {
                    if (dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth > 28) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                        ShowContinueError(".. invalid " + cNumericFieldNames(2) + "=[" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth) +
                                          "], Month=[" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).Month) + "].");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                    ShowContinueError(".. invalid " + cNumericFieldNames(1) + " invalid (Month) [" + RoundSigDigits(dataWeatherManager.DesDayInput(EnvrnNum).Month) +
                                      "].");
                    ErrorsFound = true;
                }
            }

            //   A9,  \field Daylight Saving Time Indicator
            if (UtilityRoutines::SameString(cAlphaArgs(9), "Yes")) {
                dataWeatherManager.DesDayInput(EnvrnNum).DSTIndicator = 1;
            } else if (UtilityRoutines::SameString(cAlphaArgs(9), "No") || lAlphaFieldBlanks(9)) {
                dataWeatherManager.DesDayInput(EnvrnNum).DSTIndicator = 0;
            } else {
                ShowWarningError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                ShowContinueError("..invalid field: " + cAlphaFieldNames(9) + "=\"" + cAlphaArgs(9) + "\". \"No\" will be used.");
                dataWeatherManager.DesDayInput(EnvrnNum).DSTIndicator = 0;
            }

            //   A2,  \field Day Type
            dataWeatherManager.DesDayInput(EnvrnNum).DayType = UtilityRoutines::FindItemInList(cAlphaArgs(2), ValidNames, 12);
            if (dataWeatherManager.DesDayInput(EnvrnNum).DayType == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + dataWeatherManager.DesDayInput(EnvrnNum).Title + "\", invalid data.");
                ShowContinueError("..invalid field: " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                ShowContinueError("Valid values are "
                                  "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Holiday,SummerDesignDay,WinterDesignDay,CustomDay1,"
                                  "CustomDay2.");
                ErrorsFound = true;
            }

            dataWeatherManager.Environment(EnvrnNum).Title = dataWeatherManager.DesDayInput(EnvrnNum).Title;
            dataWeatherManager.Environment(EnvrnNum).KindOfEnvrn = ksDesignDay;
            dataWeatherManager.Environment(EnvrnNum).DesignDayNum = EnvrnNum;
            dataWeatherManager.Environment(EnvrnNum).RunPeriodDesignNum = 0;
            dataWeatherManager.Environment(EnvrnNum).TotalDays = 1;
            dataWeatherManager.Environment(EnvrnNum).StartMonth = dataWeatherManager.DesDayInput(EnvrnNum).Month;
            dataWeatherManager.Environment(EnvrnNum).StartDay = dataWeatherManager.DesDayInput(EnvrnNum).DayOfMonth;
            dataWeatherManager.Environment(EnvrnNum).EndMonth = dataWeatherManager.Environment(EnvrnNum).StartMonth;
            dataWeatherManager.Environment(EnvrnNum).EndDay = dataWeatherManager.Environment(EnvrnNum).StartDay;
            dataWeatherManager.Environment(EnvrnNum).DayOfWeek = 0;
            dataWeatherManager.Environment(EnvrnNum).UseDST = false;
            dataWeatherManager.Environment(EnvrnNum).UseHolidays = false;
            dataWeatherManager.Environment(EnvrnNum).StartJDay = dataWeatherManager.DesignDay(EnvrnNum).DayOfYear;
            dataWeatherManager.Environment(EnvrnNum).EndJDay = dataWeatherManager.Environment(EnvrnNum).StartJDay;

            // create predefined report on design day
            envTitle = dataWeatherManager.DesDayInput(EnvrnNum).Title;
            PreDefTableEntry(pdchDDmaxDB, envTitle, dataWeatherManager.DesDayInput(EnvrnNum).MaxDryBulb);
            PreDefTableEntry(pdchDDrange, envTitle, dataWeatherManager.DesDayInput(EnvrnNum).DailyDBRange);
            if (dataWeatherManager.DesDayInput(EnvrnNum).HumIndType != dataWeatherManager.DDHumIndType_RelHumSch) {
                PreDefTableEntry(pdchDDhumid, envTitle, dataWeatherManager.DesDayInput(EnvrnNum).HumIndValue);
            } else {
                PreDefTableEntry(pdchDDhumid, envTitle, "N/A");
            }
            PreDefTableEntry(pdchDDhumTyp, envTitle, HumidityIndicatingType(dataWeatherManager.DesDayInput(EnvrnNum).HumIndType));
            PreDefTableEntry(pdchDDwindSp, envTitle, dataWeatherManager.DesDayInput(EnvrnNum).WindSpeed);
            PreDefTableEntry(pdchDDwindDr, envTitle, dataWeatherManager.DesDayInput(EnvrnNum).WindDir);
        }
    }

    void GetLocationInfo(WeatherManagerData &dataWeatherManager, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   October 1997
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the location info from the IDF file; latitude,
        //  longitude and time zone number.

        // Using/Aliasing
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int LocNumAlpha;             // Number of alpha names being passed
        int LocNumProp;              // Number of properties being passed
        int IOStat;                  // IO Status when calling get input subroutine
        Array1D_string LocNames(1);  // Temp Array to transfer location info
        Array1D<Real64> LocProps(4); // Temporary array to transfer location info
        int NumLocations;

        // FLOW:
        cCurrentModuleObject = "Site:Location";
        NumLocations = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumLocations > 1) {
            ShowSevereError(cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
            ErrorsFound = true;
        }

        if (NumLocations == 1) {
            // Call Input Get routine to retrieve Location information
            inputProcessor->getObjectItem(cCurrentModuleObject, 1, LocNames, LocNumAlpha, LocProps, LocNumProp, IOStat);

            // set latitude, longitude, and time zone number variables
            dataWeatherManager.LocationTitle = LocNames(1);
            Latitude = LocProps(1);
            Longitude = LocProps(2);
            TimeZoneNumber = LocProps(3);
            Elevation = LocProps(4);
            dataWeatherManager.LocationGathered = true;
        }
    }

    void GetWeatherProperties(WeatherManagerData &dataWeatherManager, bool &ErrorsFound)
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
        using ScheduleManager::GetDayScheduleIndex;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataIPShortCuts;
        using General::FindNumberInList;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetWeatherProperties:");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item;
        int IOStat;
        int NumAlpha;
        int NumNumerics;
        int Found;
        int envFound;
        int Count;
        int schPtr;
        bool MultipleEnvironments;
        std::string units;
        OutputProcessor::Unit unitType;

        cCurrentModuleObject = "WeatherProperty:SkyTemperature";
        dataWeatherManager.NumWPSkyTemperatures = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        dataWeatherManager.WPSkyTemperature.allocate(dataWeatherManager.NumWPSkyTemperatures); // by default, not used.

        for (Item = 1; Item <= dataWeatherManager.NumWPSkyTemperatures; ++Item) {
            MultipleEnvironments = false;
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumerics,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            {
                auto const SELECT_CASE_var(cAlphaArgs(1));
                if (SELECT_CASE_var == "") {
                    Found = 0;
                    for (Count = 1; Count <= dataWeatherManager.NumOfEnvrn; ++Count) {
                        if (dataWeatherManager.Environment(Count).KindOfEnvrn != ksRunPeriodWeather) continue;
                        if (dataWeatherManager.Environment(Count).WP_Type1 != 0) {
                            ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                            "\", indicated Environment Name already assigned.");
                            if (!dataWeatherManager.Environment(Count).Title.empty()) {
                                ShowContinueError("...Environment=\"" + dataWeatherManager.Environment(Count).Title + "\", already using " + cCurrentModuleObject +
                                                  "=\"" + dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(Count).WP_Type1).Name + "\".");
                            } else {
                                ShowContinueError("... Runperiod Environment, already using " + cCurrentModuleObject + "=\"" +
                                                  dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(Count).WP_Type1).Name + "\".");
                            }
                            ErrorsFound = true;
                        } else {
                            dataWeatherManager.Environment(Count).WP_Type1 = Item;
                            Found = Count;
                        }
                    }
                    MultipleEnvironments = true;
                    if (Found == 0) {
                        ShowWarningError("GetWeatherProperties: WeatherProperty:SkyTemperature=blank, no run periods found.");
                        ShowContinueError("...SkyTemperature will not be applied.");
                        continue;
                    }
                } else { // really a name
                    Found = UtilityRoutines::FindItemInList(cAlphaArgs(1), dataWeatherManager.Environment, &EnvironmentData::Title);
                    envFound = Found;
                    if (Found == 0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid Environment Name referenced.");
                        ShowContinueError("...remainder of object not processed.");
                        ErrorsFound = true;
                        continue;
                    } else {
                        if (dataWeatherManager.Environment(Found).WP_Type1 != 0) {
                            ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                            "\", indicated Environment Name already assigned.");
                            ShowContinueError("...Environment=\"" + dataWeatherManager.Environment(Found).Title + "\", already using " + cCurrentModuleObject + "=\"" +
                                              dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(Found).WP_Type1).Name + "\".");
                            ErrorsFound = true;
                        } else {
                            dataWeatherManager.Environment(Found).WP_Type1 = Item;
                        }
                    }
                }
            }

            if (!lAlphaFieldBlanks(1)) {
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                dataWeatherManager.WPSkyTemperature(Item).Name = cAlphaArgs(1); // Name
            } else {
                dataWeatherManager.WPSkyTemperature(Item).Name = "All RunPeriods";
            }
            // Validate Calculation Type.
            if (UtilityRoutines::SameString(cAlphaArgs(2), "ScheduleValue")) {
                dataWeatherManager.WPSkyTemperature(Item).CalculationType = dataWeatherManager.WP_ScheduleValue;
                dataWeatherManager.WPSkyTemperature(Item).IsSchedule = true;
                units = "[C]";
                unitType = OutputProcessor::Unit::C;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "DifferenceScheduleDryBulbValue")) {
                dataWeatherManager.WPSkyTemperature(Item).CalculationType = dataWeatherManager.WP_DryBulbDelta;
                dataWeatherManager.WPSkyTemperature(Item).IsSchedule = true;
                units = "[deltaC]";
                unitType = OutputProcessor::Unit::deltaC;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "DifferenceScheduleDewPointValue")) {
                dataWeatherManager.WPSkyTemperature(Item).CalculationType = dataWeatherManager.WP_DewPointDelta;
                dataWeatherManager.WPSkyTemperature(Item).IsSchedule = true;
                units = "[deltaC]";
                unitType = OutputProcessor::Unit::deltaC;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "Brunt")) {
                dataWeatherManager.WPSkyTemperature(Item).CalculationType = dataWeatherManager.WP_BruntModel;
                dataWeatherManager.WPSkyTemperature(Item).IsSchedule = false;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "Idso")) {
                dataWeatherManager.WPSkyTemperature(Item).CalculationType = dataWeatherManager.WP_IdsoModel;
                dataWeatherManager.WPSkyTemperature(Item).IsSchedule = false;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "BerdahlMartin")) {
                dataWeatherManager.WPSkyTemperature(Item).CalculationType = dataWeatherManager.WP_BerdahlMartinModel;
                dataWeatherManager.WPSkyTemperature(Item).IsSchedule = false;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "ClarkAllen")) {
                dataWeatherManager.WPSkyTemperature(Item).CalculationType = dataWeatherManager.WP_ClarkAllenModel;
                dataWeatherManager.WPSkyTemperature(Item).IsSchedule = false;
            } else {
                ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + '.');
                ShowContinueError("...entered value=\"" + cAlphaArgs(2) +
                                  "\", should be one of: ScheduleValue, DifferenceScheduleDryBulbValue, DifferenceScheduleDewPointValue.");
                ErrorsFound = true;
            }

            if (dataWeatherManager.WPSkyTemperature(Item).IsSchedule) {
                dataWeatherManager.WPSkyTemperature(Item).ScheduleName = cAlphaArgs(3);
                if (dataWeatherManager.Environment(Found).KindOfEnvrn == ksRunPeriodWeather ||
                    dataWeatherManager.Environment(Found).KindOfEnvrn == ksRunPeriodDesign) {
                    dataWeatherManager.WPSkyTemperature(Item).ScheduleName = cAlphaArgs(3);
                    // See if it's a schedule.
                    Found = GetScheduleIndex(cAlphaArgs(3));
                    if (Found == 0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " +
                                        cAlphaFieldNames(3) + '.');
                        ShowContinueError("...Entered name=\"" + cAlphaArgs(3) + "\".");
                        ShowContinueError(
                                "...Should be a full year schedule (\"Schedule:Year\", \"Schedule:Compact\", \"Schedule:File\", or "
                                "\"Schedule:Constant\" objects.");
                        ErrorsFound = true;
                    } else {
                        dataWeatherManager.WPSkyTemperature(Item).IsSchedule = true;
                        dataWeatherManager.WPSkyTemperature(Item).SchedulePtr = Found;
                    }
                } else { // See if it's a valid schedule.
                    Found = GetDayScheduleIndex(cAlphaArgs(3));
                    if (Found == 0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " +
                                        cAlphaFieldNames(3) + '.');
                        ShowContinueError("...Entered name=\"" + cAlphaArgs(3) + "\".");
                        ShowContinueError(
                                "...Should be a single day schedule (\"Schedule:Day:Hourly\", \"Schedule:Day:Interval\", or \"Schedule:Day:List\" objects.");
                        ErrorsFound = true;
                    } else {
                        if (envFound != 0) {
                            schPtr = FindNumberInList(Found, dataWeatherManager.SPSiteScheduleNamePtr, dataWeatherManager.NumSPSiteScheduleNamePtrs);
                            if (schPtr == 0) {
                                ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                                dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = Found;
                                dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                                SetupOutputVariable("Sizing Period Site Sky Temperature Schedule Value", unitType,
                                    dataWeatherManager.SPSiteSkyTemperatureScheduleValue(envFound), "Zone", "Average",
                                                    cAlphaArgs(3));
                            } else if (dataWeatherManager.SPSiteScheduleUnits(schPtr) != units) {
                                ++dataWeatherManager.NumSPSiteScheduleNamePtrs;
                                dataWeatherManager.SPSiteScheduleNamePtr(dataWeatherManager.NumSPSiteScheduleNamePtrs) = Found;
                                dataWeatherManager.SPSiteScheduleUnits(dataWeatherManager.NumSPSiteScheduleNamePtrs) = units;
                                SetupOutputVariable("Sizing Period Site Sky Temperature Schedule Value", unitType,
                                    dataWeatherManager.SPSiteSkyTemperatureScheduleValue(envFound), "Zone", "Average",
                                                    cAlphaArgs(3));
                            }
                            dataWeatherManager.WPSkyTemperature(Item).IsSchedule = true;
                            dataWeatherManager.WPSkyTemperature(Item).SchedulePtr = Found;
                        }
                    }
                }
            }

            if (!dataWeatherManager.WPSkyTemperature(Item).IsSchedule && !lAlphaFieldBlanks(4)) {
                if (UtilityRoutines::SameString(cAlphaArgs(4), "Yes")) {
                    dataWeatherManager.WPSkyTemperature(Item).UseWeatherFileHorizontalIR = true;
                } else if (UtilityRoutines::SameString(cAlphaArgs(4), "No")) {
                    dataWeatherManager.WPSkyTemperature(Item).UseWeatherFileHorizontalIR = false;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(4) + '.');
                    ShowContinueError("...entered value=\"" + cAlphaArgs(4) + "\", should be Yes or No.");
                    ErrorsFound = true;
                }
            } else {
                dataWeatherManager.WPSkyTemperature(Item).UseWeatherFileHorizontalIR = true;
            }
        }
        for (int envrn = 1; envrn <= dataWeatherManager.NumOfEnvrn; ++envrn) {
            if (dataWeatherManager.Environment(envrn).WP_Type1 != 0 && dataWeatherManager.NumWPSkyTemperatures >= dataWeatherManager.Environment(envrn).WP_Type1){
                dataWeatherManager.Environment(envrn).SkyTempModel = dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(envrn).WP_Type1).CalculationType;
                dataWeatherManager.Environment(envrn).UseWeatherFileHorizontalIR = dataWeatherManager.WPSkyTemperature(dataWeatherManager.Environment(envrn).WP_Type1).UseWeatherFileHorizontalIR;
            }
        }
    }

    void GetGroundTemps(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   October 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This file reads the Ground Temps from the input file and puts them
        //  in a new variable.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using namespace GroundTemperatureManager;

        // Formats
        static ObjexxFCL::gio::Fmt Format_720("(' ',A,12(', ',F6.2))");

        // FLOW:
        // Initialize Site:GroundTemperature:BuildingSurface object
        state.dataWeatherManager.siteBuildingSurfaceGroundTempsPtr = GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:BUILDINGSURFACE", "");
        if (state.dataWeatherManager.siteBuildingSurfaceGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager.siteBuildingSurfaceGroundTempsPtr->errorsFound ? true : ErrorsFound;
        }

        // Initialize Site:GroundTemperature:FCFactorMethod object
        state.dataWeatherManager.siteFCFactorMethodGroundTempsPtr = GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:FCFACTORMETHOD", "");
        if (state.dataWeatherManager.siteFCFactorMethodGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager.siteFCFactorMethodGroundTempsPtr->errorsFound ? true : ErrorsFound;
        }

        // Initialize Site:GroundTemperature:Shallow object
        state.dataWeatherManager.siteShallowGroundTempsPtr = GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:SHALLOW", "");
        if (state.dataWeatherManager.siteShallowGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager.siteShallowGroundTempsPtr->errorsFound ? true : ErrorsFound;
        }

        // Initialize Site:GroundTemperature:Deep object
        state.dataWeatherManager.siteDeepGroundTempsPtr = GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:DEEP", "");
        if (state.dataWeatherManager.siteDeepGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager.siteDeepGroundTempsPtr->errorsFound ? true : ErrorsFound;
        }
    }

    void GetGroundReflectances(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This file reads the Ground Reflectances from the input file (optional) and
        // places them in the monthly array.

        // Using/Aliasing
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int GndNumAlpha;          // Number of construction alpha names being passed
        int GndNumProp;           // dummy variable for properties being passed
        int IOStat;               // IO Status when calling get input subroutine
        int I;                    // Loop counter variable
        Array1D_string GndAlphas; // Construction Alpha names defined
        Array1D<Real64> GndProps; // Temporary array to transfer ground reflectances

        // Formats
        static ObjexxFCL::gio::Fmt Format_720("(' Site:GroundReflectance',12(', ',F5.2))");

        // FLOW:
        cCurrentModuleObject = "Site:GroundReflectance";
        I = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (I != 0) {
            GndProps.allocate(12);
            GndAlphas.allocate(1);
            if (I == 1) {
                // Get the object names for each construction from the input processor
                inputProcessor->getObjectItem(cCurrentModuleObject, 1, GndAlphas, GndNumAlpha, GndProps, GndNumProp, IOStat);

                if (GndNumProp < 12) {
                    ShowSevereError(cCurrentModuleObject + ": Less than 12 values entered.");
                    ErrorsFound = true;
                }

                // Assign the ground reflectances to the variable
                dataWeatherManager.GroundReflectances({1, 12}) = GndProps({1, 12});

            } else {
                ShowSevereError(cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
                ErrorsFound = true;
            }
            GndProps.deallocate();
            GndAlphas.deallocate();
        }

        // Write Final Ground Reflectance Information to the initialization output file
        print(outputFiles.eio, "{}\n",
               "! "
               "<Site:GroundReflectance>,Jan{dimensionless},Feb{dimensionless},Mar{dimensionless},Apr{dimensionless},"
               "May{dimensionless},Jun{dimensionless},Jul{dimensionless},Aug{dimensionless},Sep{dimensionless},Oct{"
               "dimensionless},Nov{dimensionless},Dec{dimensionless}");

        print(outputFiles.eio, " Site:GroundReflectance");
        for (I = 1; I <= 12; ++I) {
            print(outputFiles.eio, ", {:5.2F}", dataWeatherManager.GroundReflectances(I));
        }
        print(outputFiles.eio, "\n");
    }

    void GetSnowGroundRefModifiers(WeatherManagerData &dataWeatherManager, OutputFiles &outputFiles, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This file reads the Snow Ground Reflectance Modifiers from the input file (optional) and
        // places them in the variables.

        // Using/Aliasing
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int GndNumAlpha;          // Number of construction alpha names being passed
        int GndNumProp;           // dummy variable for properties being passed
        int IOStat;               // IO Status when calling get input subroutine
        int I;                    // Loop counter variable
        Array1D_string GndAlphas; // Construction Alpha names defined
        Array1D<Real64> GndProps; // Temporary array to transfer ground reflectances

        // Formats
       static ObjexxFCL::gio::Fmt Format_721("(A,12(', ',F5.2))");

        // FLOW:
        cCurrentModuleObject = "Site:GroundReflectance:SnowModifier";
        I = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (I != 0) {
            GndProps.allocate(2);
            GndAlphas.allocate(1);
            if (I == 1) {
                // Get the object names for each construction from the input processor
                inputProcessor->getObjectItem(cCurrentModuleObject, 1, GndAlphas, GndNumAlpha, GndProps, GndNumProp, IOStat);

                // Assign the ground reflectances to the variable
                dataWeatherManager.SnowGndRefModifier = GndProps(1);
                dataWeatherManager.SnowGndRefModifierForDayltg = GndProps(2);

            } else {
                ShowSevereError(cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
                ErrorsFound = true;
            }
            GndProps.deallocate();
            GndAlphas.deallocate();
        }

        // Write Final Ground Reflectance Modifier Information to the initialization output file
        print(outputFiles.eio, "{}\n", "! <Site:GroundReflectance:SnowModifier>, Normal, Daylighting {dimensionless}");
        static constexpr auto Format_720(" Site:GroundReflectance:SnowModifier, {:7.3F}, {:7.3F}\n");
        print(outputFiles.eio, Format_720, dataWeatherManager.SnowGndRefModifier, dataWeatherManager.SnowGndRefModifierForDayltg);

        print(outputFiles.eio, "{}\n",
               "! "
               "<Site:GroundReflectance:Snow>,Jan{dimensionless},Feb{dimensionless},Mar{dimensionless},Apr{"
               "dimensionless},May{dimensionless},Jun{dimensionless},Jul{dimensionless},Aug{dimensionless},Sep{"
               "dimensionless},Oct{dimensionless},Nov{dimensionless},Dec{dimensionless}");
        print(outputFiles.eio, "{}"," Site:GroundReflectance:Snow");
        for (I = 1; I <= 12; ++I) {
            print(outputFiles.eio, ", {:5.2F}", max(min(dataWeatherManager.GroundReflectances(I) * dataWeatherManager.SnowGndRefModifier, 1.0), 0.0));
        }
        print(outputFiles.eio, "\n");
        print(outputFiles.eio, "{}\n",
              "! "
              "<Site:GroundReflectance:Snow:Daylighting>,Jan{dimensionless},Feb{dimensionless},Mar{dimensionless},Apr{"
               "dimensionless},May{dimensionless},Jun{dimensionless},Jul{dimensionless},Aug{dimensionless},Sep{"
               "dimensionless},Oct{dimensionless},Nov{dimensionless},Dec{dimensionless}");
        print(outputFiles.eio, " Site:GroundReflectance:Snow:Daylighting");
        for (I = 1; I <= 12; ++I) {
            print(outputFiles.eio, ", {:5.2F}", max(min(dataWeatherManager.GroundReflectances(I) * dataWeatherManager.SnowGndRefModifierForDayltg, 1.0), 0.0));
        }
        print(outputFiles.eio, "\n");
    }

    void GetWaterMainsTemperatures(WeatherManagerData &dataWeatherManager, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads the input data for the WATER MAINS TEMPERATURES object.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumObjects;
        int NumAlphas;               // Number of elements in the alpha array
        int NumNums;                 // Number of elements in the numeric array
        int IOStat;                  // IO Status when calling get input subroutine
        Array1D_string AlphArray(2); // Character string data
        Array1D<Real64> NumArray(2); // Numeric data

        // FLOW:
        cCurrentModuleObject = "Site:WaterMainsTemperature";
        NumObjects = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumObjects == 1) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          1,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (UtilityRoutines::SameString(AlphArray(1), "Schedule")) {
                dataWeatherManager.WaterMainsTempsMethod = dataWeatherManager.ScheduleMethod;
                dataWeatherManager.WaterMainsTempsScheduleName = AlphArray(2);
                dataWeatherManager.WaterMainsTempsSchedule = GetScheduleIndex(AlphArray(2));
                if (dataWeatherManager.WaterMainsTempsSchedule == 0) {
                    ShowSevereError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(2) + '=' + AlphArray(2));
                    ErrorsFound = true;
                }

            } else if (UtilityRoutines::SameString(AlphArray(1), "Correlation")) {
                dataWeatherManager.WaterMainsTempsMethod = dataWeatherManager.CorrelationMethod;

                if (NumNums == 0) {
                    ShowSevereError(cCurrentModuleObject + ": Missing Annual Average and Maximum Difference fields.");
                    ErrorsFound = true;
                } else if (NumNums == 1) {
                    ShowSevereError(cCurrentModuleObject + ": Missing Maximum Difference field.");
                    ErrorsFound = true;
                } else {
                    dataWeatherManager.WaterMainsTempsAnnualAvgAirTemp = NumArray(1);
                    dataWeatherManager.WaterMainsTempsMaxDiffAirTemp = NumArray(2);
                }
            } else if (UtilityRoutines::SameString(AlphArray(1), "CorrelationFromWeatherFile")) {
                dataWeatherManager.WaterMainsTempsMethod = dataWeatherManager.CorrelationFromWeatherFileMethod;
            } else {
                ShowSevereError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(1) + '=' + AlphArray(1));
                ErrorsFound = true;
            }

        } else if (NumObjects > 1) {
            ShowSevereError(cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
            ErrorsFound = true;
        }
    }

    void CalcWaterMainsTemp(WeatherManagerData &dataWeatherManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2005
        //       MODIFIED       June 2018, B. Nigusse
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the daily water mains temperature based on input data from the WATER MAINS TEMPERATURES object.

        // METHODOLOGY EMPLOYED:
        // Water mains temperature is either taken from a schedule or calculated by a correlation.  The correlation
        // is fit to Fahrenheit units, so the air temperature values are first convert to F, then mains temperature
        // is calculated and converted back to C.

        using ScheduleManager::GetCurrentScheduleValue;

        {
            auto const SELECT_CASE_var(dataWeatherManager.WaterMainsTempsMethod);

            if (SELECT_CASE_var == dataWeatherManager.ScheduleMethod) {
                WaterMainsTemp = GetCurrentScheduleValue(dataWeatherManager.WaterMainsTempsSchedule);
            } else if (SELECT_CASE_var == dataWeatherManager.CorrelationMethod) {
                WaterMainsTemp = WaterMainsTempFromCorrelation(dataWeatherManager.WaterMainsTempsAnnualAvgAirTemp, dataWeatherManager.WaterMainsTempsMaxDiffAirTemp);
            } else if (SELECT_CASE_var == dataWeatherManager.CorrelationFromWeatherFileMethod) {
                if (dataWeatherManager.OADryBulbAverage.OADryBulbWeatherDataProcessed) {
                    WaterMainsTemp =
                        WaterMainsTempFromCorrelation(dataWeatherManager.OADryBulbAverage.AnnualAvgOADryBulbTemp, dataWeatherManager.OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff);
                } else {
                    WaterMainsTemp = 10.0; // 50 F
                }
            } else {
                WaterMainsTemp = 10.0; // 50 F
            }
        }
    }

    Real64 WaterMainsTempFromCorrelation(Real64 const AnnualOAAvgDryBulbTemp, Real64 const MonthlyOAAvgDryBulbTempMaxDiff)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2005
        //       MODIFIED       na B Nigusse June 2018 (Refactored)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the daily water mains temperature based on input data from the WATER MAINS TEMPERATURES object.

        // METHODOLOGY EMPLOYED:
        // Water mains temperature calculated by a correlation.  The correlation is fit to Fahrenheit units, so the
        // air temperature values are first convert to F, then mains temperature is calculated and converted back to C.
        // used for Calculated Method: 'Correlation' and 'CorrelationFromWeatherFile'.

        // REFERENCES:
        // Correlation developed by Jay Burch and Craig Christensen at NREL, described in:
        // Hendron, R., Anderson, R., Christensen, C., Eastment, M., and Reeves, P.  2004.  "Development of an Energy
        // Savings Benchmark for All Residential End-Uses", Proceedings of SimBuild 2004, IBPSA-USA National Conference,
        // Boulder, CO, August 4 - 6, 2004.

        // Using/Aliasing

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Tavg;                          // Annual Average Outdoor Air Temperature (F)
        Real64 Tdiff;                         // Maximum difference in monthly average outdoor air temperatures (deltaF)
        Real64 Ratio;                         // Value used in correlation
        Real64 Lag;                           // Value used in correlation
        Real64 Offset;                        // Value used in correlation
        Real64 CurrentWaterMainsTemp;         // calculated water main temp (F)

        // FLOW:
        Tavg = AnnualOAAvgDryBulbTemp * (9.0 / 5.0) + 32.0;
        Tdiff = MonthlyOAAvgDryBulbTempMaxDiff * (9.0 / 5.0);

        Ratio = 0.4 + 0.01 * (Tavg - 44.0);
        Lag = 35.0 - 1.0 * (Tavg - 44.0);
        Offset = 6.0;
        int latitude_sign;
        if (Latitude >= 0) {
            latitude_sign = 1;
        } else {
            latitude_sign = -1;
        }

        CurrentWaterMainsTemp = Tavg + Offset + Ratio * (Tdiff / 2.0) * latitude_sign * std::sin((0.986 * (DayOfYear - 15.0 - Lag) - 90) * DegToRadians);

        if (CurrentWaterMainsTemp < 32.0) CurrentWaterMainsTemp = 32.0;

        // Convert F to C
        return (CurrentWaterMainsTemp - 32.0) * (5.0 / 9.0);
    }
    void GetWeatherStation(OutputFiles &outputFiles, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads the input data for the WEATHER STATION object.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using General::RoundSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumObjects;
        int NumAlphas;                      // Number of elements in the alpha array
        int NumNums;                        // Number of elements in the numeric array
        int IOStat;                         // IO Status when calling get input subroutine
        Array1D_string AlphArray(1);        // Character string data
        Array1D<Real64> NumArray(4);        // Numeric data
        Real64 WeatherFileWindSensorHeight; // Height of the wind sensor at the weather station, i.e., weather file
        Real64 WeatherFileWindExp;          // Exponent for the wind velocity profile at the weather station
        Real64 WeatherFileWindBLHeight;     // Boundary layer height for the wind velocity profile at the weather station (m)
        Real64 WeatherFileTempSensorHeight; // Height of the air temperature sensor at the weather station (m)


        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // FLOW:
        cCurrentModuleObject = "Site:WeatherStation";
        NumObjects = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        // Default conditions for a weather station in an open field at a height of 10 m. (These should match the IDD defaults.)
        WeatherFileWindSensorHeight = 10.0;
        WeatherFileWindExp = 0.14;
        WeatherFileWindBLHeight = 270.0;
        WeatherFileTempSensorHeight = 1.5;

        if (NumObjects == 1) {
            inputProcessor->getObjectItem(cCurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat);

            if (NumNums > 0) WeatherFileWindSensorHeight = NumArray(1);
            if (NumNums > 1) WeatherFileWindExp = NumArray(2);
            if (NumNums > 2) WeatherFileWindBLHeight = NumArray(3);
            if (NumNums > 3) WeatherFileTempSensorHeight = NumArray(4);

        } else if (NumObjects > 1) {
            ShowSevereError(cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
            ErrorsFound = true;
        }

        WeatherFileWindModCoeff = std::pow(WeatherFileWindBLHeight / WeatherFileWindSensorHeight, WeatherFileWindExp);
        WeatherFileTempModCoeff = AtmosphericTempGradient * EarthRadius * WeatherFileTempSensorHeight / (EarthRadius + WeatherFileTempSensorHeight);

        // Write to the initialization output file
        print(outputFiles.eio, "{}\n",
               "! <Environment:Weather Station>,Wind Sensor Height Above Ground {m},Wind Speed Profile Exponent "
               "{},Wind Speed Profile Boundary Layer Thickness {m},Air Temperature Sensor Height Above Ground {m},Wind "
               "Speed Modifier Coefficient-Internal,Temperature Modifier Coefficient-Internal");

        // Formats
        static constexpr auto Format_720("Environment:Weather Station,{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R}\n");
        print(outputFiles.eio,
              Format_720,
              WeatherFileWindSensorHeight,
              WeatherFileWindExp,
              WeatherFileWindBLHeight,
              WeatherFileTempSensorHeight,
              WeatherFileWindModCoeff,
              WeatherFileTempModCoeff);
    }

    void DayltgCurrentExtHorizIllum()
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
        // REAL(r64)   :: PDIRLW                  ! Luminous efficacy (lum/W) of beam solar radiation
        // REAL(r64)   :: PDIFLW                  ! Luminous efficacy (lum/W) of sky diffuse solar radiation

        //              DIRECT AND DIFFUSE HORIZONTAL SOLAR IRRADIANCE (W/M2).
        //              SOLCOS(3), below, is the cosine of the solar zenith angle.
        if (SunIsUp) {
            SDIRH = BeamSolarRad * SOLCOS(3);
            SDIFH = DifSolarRad;
            //              Fraction of sky covered by clouds
            CloudFraction = pow_2(SDIFH / (SDIRH + SDIFH + 0.0001));
            //              Luminous efficacy of sky diffuse solar and beam solar (lumens/W);
            //              Horizontal illuminance from sky and horizontal beam illuminance (lux)
            //              obtained from solar quantities on weather file and luminous efficacy.

            DayltgLuminousEfficacy(PDIFLW, PDIRLW);
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

    void DayltgLuminousEfficacy(Real64 &DiffLumEff, // Luminous efficacy of sky diffuse solar radiation (lum/W)
                                Real64 &DirLumEff   // Luminous efficacy of beam solar radiation (lum/W)
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
        static Array1D<Real64> const ADiffLumEff(
            8, {97.24, 107.22, 104.97, 102.39, 100.71, 106.42, 141.88, 152.23}); // Diffuse luminous efficacy coefficients
        static Array1D<Real64> const BDiffLumEff(8, {-0.46, 1.15, 2.96, 5.59, 5.94, 3.83, 1.90, 0.35});
        static Array1D<Real64> const CDiffLumEff(8, {12.00, 0.59, -5.53, -13.95, -22.75, -36.15, -53.24, -45.27});
        static Array1D<Real64> const DDiffLumEff(8, {-8.91, -3.95, -8.77, -13.90, -23.74, -28.83, -14.03, -7.98});
        static Array1D<Real64> const ADirLumEff(
            8, {57.20, 98.99, 109.83, 110.34, 106.36, 107.19, 105.75, 101.18}); // Direct luminous efficacy coefficients
        static Array1D<Real64> const BDirLumEff(8, {-4.55, -3.46, -4.90, -5.84, -3.97, -1.25, 0.77, 1.58});
        static Array1D<Real64> const CDirLumEff(8, {-2.98, -1.21, -1.71, -1.99, -1.75, -1.51, -1.26, -1.10});
        static Array1D<Real64> const DDirLumEff(8, {117.12, 12.38, -8.81, -4.56, -6.16, -26.73, -34.44, -8.29});
        static Array1D<Real64> const ExtraDirNormIll(12,
                                                     {131153.0,
                                                      130613.0,
                                                      128992.0,
                                                      126816.0,
                                                      124731.0,
                                                      123240.0,
                                                      122652.0,
                                                      123120.0,
                                                      124576.0,
                                                      126658.0,
                                                      128814.0,
                                                      130471.0}); // Monthly exterrestrial direct normal illuminance (lum/m2)

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 SunZenith;      // Solar zenith angle (radians)
        Real64 SunAltitude;    // Solar altitude angle (radians)
        Real64 SinSunAltitude; // Sine of the solar altitude angle
        Real64 Zeta;
        int ISkyClearness;    // Sky clearness bin
        Real64 AirMass;       // Relative optical air mass
        Real64 AtmosMoisture; // Atmospheric moisture (cm of precipitable water)

        // FLOW:

        SunZenith = std::acos(SOLCOS(3));
        SunAltitude = PiOvr2 - SunZenith;
        SinSunAltitude = std::sin(SunAltitude);
        //              Clearness of sky. SkyClearness close to 1.0 corresponds to an overcast sky.
        //              SkyClearness > 6 is a clear sky.
        //              DifSolarRad is the diffuse horizontal irradiance.
        //              BeamSolarRad is the direct normal irradiance.
        Zeta = 1.041 * pow_3(SunZenith);
        SkyClearness = ((DifSolarRad + BeamSolarRad) / (DifSolarRad + 0.0001) + Zeta) / (1.0 + Zeta);
        AirMass = (1.0 - 0.1 * Elevation / 1000.0) / (SinSunAltitude + 0.15 / std::pow(SunAltitude / DegToRadians + 3.885, 1.253));
        //              In the following, 93.73 is the extraterrestrial luminous efficacy
        SkyBrightness = (DifSolarRad * 93.73) * AirMass / ExtraDirNormIll(Month);
        if (SkyClearness <= 1.065) {
            ISkyClearness = 1;
        } else if (SkyClearness > 1.065 && SkyClearness <= 1.23) {
            ISkyClearness = 2;
        } else if (SkyClearness > 1.23 && SkyClearness <= 1.50) {
            ISkyClearness = 3;
        } else if (SkyClearness > 1.50 && SkyClearness <= 1.95) {
            ISkyClearness = 4;
        } else if (SkyClearness > 1.95 && SkyClearness <= 2.80) {
            ISkyClearness = 5;
        } else if (SkyClearness > 2.80 && SkyClearness <= 4.50) {
            ISkyClearness = 6;
        } else if (SkyClearness > 4.50 && SkyClearness <= 6.20) {
            ISkyClearness = 7;
        } else {
            ISkyClearness = 8;
        }
        AtmosMoisture = std::exp(0.07 * OutDewPointTemp - 0.075);
        //              Sky diffuse luminous efficacy
        if (SkyBrightness <= 0.0) {
            DiffLumEff = 0.0;
        } else {
            DiffLumEff = ADiffLumEff(ISkyClearness) + BDiffLumEff(ISkyClearness) * AtmosMoisture + CDiffLumEff(ISkyClearness) * SOLCOS(3) +
                         DDiffLumEff(ISkyClearness) * std::log(SkyBrightness);
        }
        //              Direct normal luminous efficacy
        if (SkyBrightness <= 0.0) {
            DirLumEff = 0.0;
        } else {
            DirLumEff = max(0.0,
                            ADirLumEff(ISkyClearness) + BDirLumEff(ISkyClearness) * AtmosMoisture +
                                CDirLumEff(ISkyClearness) * std::exp(5.73 * SunZenith - 5.0) + DDirLumEff(ISkyClearness) * SkyBrightness);
        }
    }

    Real64 GetSTM(Real64 const Longitude) // Longitude from user input
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
        Array1D<Real64> longl({-12, 12}); // Lower Longitude value for a Time Zone
        Array1D<Real64> longh({-12, 12}); // Upper Longitude value for a Time Zone
        int i;                            // Loop variable
        Real64 temp;                      // temporary value used to determine time zone
        Real64 tz;                        // resultant tz meridian

        GetSTM = 0.0;

        longl(0) = -7.5;
        longh(0) = 7.5;
        for (i = 1; i <= 12; ++i) {
            longl(i) = longl(i - 1) + 15.0;
            longh(i) = longh(i - 1) + 15.0;
        }
        for (i = 1; i <= 12; ++i) {
            longl(-i) = longl(-i + 1) - 15.0;
            longh(-i) = longh(-i + 1) - 15.0;
        }
        temp = Longitude;
        temp = mod(temp, 360.0);

        if (temp > 180.0) temp -= 180.0;
        for (i = -12; i <= 12; ++i) {
            if (temp > longl(i) && temp <= longh(i)) {
                tz = i;
                tz = mod(tz, 24.0);
                GetSTM = tz;
                break;
            }
        }

        return GetSTM;
    }

    void ProcessEPWHeader(WeatherManagerData &dataWeatherManager, std::string const &HeaderString, std::string &Line, bool &ErrorsFound)
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

        // Using/Aliasing

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtA("(A)");
        static ObjexxFCL::gio::Fmt fmtLD("*");

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
        int CurCount;
        int CurOne;
        int NumEPWHolidays;
        int NumGrndTemps;
        int TropExtremeCount; // because these can show up as "no dry" need to count and separate.
        int actcount;
        bool errflag1; // Local ErrFlag for call to ProcessDateString

        // Strip off Header value from Line
        Pos = index(Line, ',');
        if ((Pos == std::string::npos) && (!has_prefixi(HeaderString, "COMMENTS"))) {
            ShowSevereError("Invalid Header line in in.epw -- no commas");
            ShowContinueError("Line=" + Line);
            ShowFatalError("Previous conditions cause termination.");
        }
        if (Pos != std::string::npos) Line.erase(0, Pos + 1);

        {
            auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(HeaderString));

            if (SELECT_CASE_var == "LOCATION") {

                // LOCATION, A1 [City], A2 [State/Province/Region], A3 [Country],
                // A4 [Source], N1 [WMO], N2 [Latitude],
                // N3 [Longitude], N4 [Time Zone], N5 [Elevation {m}]

                NumHdArgs = 9;
                Count = 1;
                while (Count <= NumHdArgs) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos == std::string::npos) {
                        if (len(Line) == 0) {
                            while (Pos == std::string::npos) {
                                ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA) >> Line;
                                strip(Line);
                                uppercase(Line);
                                Pos = index(Line, ',');
                            }
                        } else {
                            Pos = len(Line);
                        }
                    }

                    {
                        auto const SELECT_CASE_var1(Count);

                        if (SELECT_CASE_var1 == 1) {
                            Title = stripped(Line.substr(0, Pos));

                        } else if ((SELECT_CASE_var1 == 2) || (SELECT_CASE_var1 == 3) || (SELECT_CASE_var1 == 4)) {
                            Title = strip(Title) + ' ' + stripped(Line.substr(0, Pos));

                        } else if (SELECT_CASE_var1 == 5) {
                            WMO = stripped(Line.substr(0, Pos));
                            Title += " WMO#=" + WMO;

                        } else if ((SELECT_CASE_var1 == 6) || (SELECT_CASE_var1 == 7) || (SELECT_CASE_var1 == 8) || (SELECT_CASE_var1 == 9)) {
                            Number = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                            if (!errFlag) {
                                {
                                    auto const SELECT_CASE_var2(Count);
                                    if (SELECT_CASE_var2 == 6) {
                                        dataWeatherManager.WeatherFileLatitude = Number;
                                    } else if (SELECT_CASE_var2 == 7) {
                                        dataWeatherManager.WeatherFileLongitude = Number;
                                    } else if (SELECT_CASE_var2 == 8) {
                                        dataWeatherManager.WeatherFileTimeZone = Number;
                                    } else if (SELECT_CASE_var2 == 9) {
                                        dataWeatherManager.WeatherFileElevation = Number;
                                    }
                                }
                            } else {
                                ShowSevereError("GetEPWHeader:LOCATION, invalid numeric=" + Line.substr(0, Pos));
                                ErrorsFound = true;
                            }
                        }
                    }
                    Line.erase(0, Pos + 1);
                    ++Count;
                }
                WeatherFileLocationTitle = stripped(Title);

            } else if (SELECT_CASE_var == "DESIGN CONDITIONS") {
                // No action

            } else if (SELECT_CASE_var == "TYPICAL/EXTREME PERIODS") {
                TropExtremeCount = 0;
                strip(Line);
                Pos = index(Line, ',');
                if (Pos == std::string::npos) {
                    if (len(Line) == 0) {
                        while (Pos == std::string::npos && len(Line) == 0) {
                            ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA) >> Line;
                            strip(Line);
                            Pos = index(Line, ',');
                        }
                    } else {
                        Pos = len(Line);
                    }
                }
                dataWeatherManager.NumEPWTypExtSets = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                Line.erase(0, Pos + 1);
                dataWeatherManager.TypicalExtremePeriods.allocate(dataWeatherManager.NumEPWTypExtSets);
                TropExtremeCount = 0;
                Count = 1;
                while (Count <= dataWeatherManager.NumEPWTypExtSets) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        dataWeatherManager.TypicalExtremePeriods(Count).Title = Line.substr(0, Pos);
                        Line.erase(0, Pos + 1);
                    } else {
                        ShowWarningError("ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)=" + Line.substr(0, Pos));
                        ShowContinueError("...on processing Typical/Extreme period #" + RoundSigDigits(Count));
                        dataWeatherManager.NumEPWTypExtSets = Count - 1;
                        break;
                    }
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        dataWeatherManager.TypicalExtremePeriods(Count).TEType = Line.substr(0, Pos);
                        Line.erase(0, Pos + 1);
                        if (UtilityRoutines::SameString(dataWeatherManager.TypicalExtremePeriods(Count).TEType, "EXTREME")) {
                            if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO DRY SEASON - WEEK NEAR ANNUAL MAX")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoDrySeasonMax";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO DRY SEASON - WEEK NEAR ANNUAL MIN")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoDrySeasonMin";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO WET SEASON - WEEK NEAR ANNUAL MAX")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoWetSeasonMax";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO WET SEASON - WEEK NEAR ANNUAL MIN")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoWetSeasonMin";
                                // to account for problems earlier in weather files:
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO DRY")) {
                                if (TropExtremeCount == 0) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).Title = "No Dry Season - Week Near Annual Max";
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoDrySeasonMax";
                                    ++TropExtremeCount;
                                } else if (TropExtremeCount == 1) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).Title = "No Dry Season - Week Near Annual Min";
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoDrySeasonMin";
                                    ++TropExtremeCount;
                                }
                            } else { // make new short titles
                                if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "SUMMER")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Summer";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "WINTER")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Winter";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "TROPICAL HOT")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "TropicalHot";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "TROPICAL COLD")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "TropicalCold";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "AUTUMN")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Autumn";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO DRY")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoDrySeason";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO WET")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoWetSeason";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "WET ")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "WetSeason";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "DRY ")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "DrySeason";
                                } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "SPRING")) {
                                    dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Spring";
                                }
                            }
                        } else { // not extreme
                            if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "SUMMER")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Summer";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "WINTER")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Winter";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "TROPICAL HOT")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "TropicalHot";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "TROPICAL COLD")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "TropicalCold";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "AUTUMN")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Autumn";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO DRY")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoDrySeason";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "NO WET")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "NoWetSeason";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "WET ")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "WetSeason";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "DRY ")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "DrySeason";
                            } else if (has_prefixi(dataWeatherManager.TypicalExtremePeriods(Count).Title, "SPRING")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle = "Spring";
                            }
                        }
                    } else {
                        ShowWarningError("ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)=" +
                                         dataWeatherManager.TypicalExtremePeriods(Count).Title + BlankString + Line.substr(0, Pos));
                        ShowContinueError("...on processing Typical/Extreme period #" + RoundSigDigits(Count));
                        dataWeatherManager.NumEPWTypExtSets = Count - 1;
                        break;
                    }
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        ProcessDateString(Line.substr(0, Pos), PMonth, PDay, PWeekDay, DateType, ErrorsFound);
                        if (DateType != dataWeatherManager.InvalidDate) {
                            if (PMonth != 0 && PDay != 0) {
                                dataWeatherManager.TypicalExtremePeriods(Count).StartMonth = PMonth;
                                dataWeatherManager.TypicalExtremePeriods(Count).StartDay = PDay;
                            }
                        } else {
                            ShowSevereError("ProcessEPWHeader: Invalid Typical/Extreme Periods Start Date Field(WeatherFile)=" + Line.substr(0, Pos));
                            ShowContinueError("...on processing Typical/Extreme period #" + RoundSigDigits(Count));
                            ErrorsFound = true;
                        }
                        Line.erase(0, Pos + 1);
                    }
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        ProcessDateString(Line.substr(0, Pos), PMonth, PDay, PWeekDay, DateType, ErrorsFound);
                        if (DateType != dataWeatherManager.InvalidDate) {
                            if (PMonth != 0 && PDay != 0) {
                                dataWeatherManager.TypicalExtremePeriods(Count).EndMonth = PMonth;
                                dataWeatherManager.TypicalExtremePeriods(Count).EndDay = PDay;
                            }
                        } else {
                            ShowSevereError("ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)=" + Line.substr(0, Pos));
                            ShowContinueError("...on processing Typical/Extreme period #" + RoundSigDigits(Count));
                            ErrorsFound = true;
                        }
                        Line.erase(0, Pos + 1);
                    } else { // Pos=0, probably last one
                        ProcessDateString(Line, PMonth, PDay, PWeekDay, DateType, ErrorsFound);
                        if (DateType != dataWeatherManager.InvalidDate) {
                            if (PMonth != 0 && PDay != 0) {
                                dataWeatherManager.TypicalExtremePeriods(Count).EndMonth = PMonth;
                                dataWeatherManager.TypicalExtremePeriods(Count).EndDay = PDay;
                            }
                        } else {
                            ShowSevereError("ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)=" + Line.substr(0, Pos));
                            ErrorsFound = true;
                        }
                    }
                    ++Count;
                }
                // Process periods to set up other values.
                for (Count = 1; Count <= dataWeatherManager.NumEPWTypExtSets; ++Count) {
                    // JulianDay (Month,Day,LeapYearValue)
                    {
                        auto const SELECT_CASE_var1(UtilityRoutines::MakeUPPERCase(dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle));
                        if (SELECT_CASE_var1 == "SUMMER") {
                            if (UtilityRoutines::SameString(dataWeatherManager.TypicalExtremePeriods(Count).TEType, "EXTREME")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "SummerExtreme";
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue1 = "TropicalHot";
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue2 = "NoDrySeasonMax";
                            } else {
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "SummerTypical";
                            }

                        } else if (SELECT_CASE_var1 == "WINTER") {
                            if (UtilityRoutines::SameString(dataWeatherManager.TypicalExtremePeriods(Count).TEType, "EXTREME")) {
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "WinterExtreme";
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue1 = "TropicalCold";
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue2 = "NoDrySeasonMin";
                            } else {
                                dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "WinterTypical";
                            }

                        } else if (SELECT_CASE_var1 == "AUTUMN") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "AutumnTypical";

                        } else if (SELECT_CASE_var1 == "SPRING") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "SpringTypical";

                        } else if (SELECT_CASE_var1 == "WETSEASON") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "WetSeason";

                        } else if (SELECT_CASE_var1 == "DRYSEASON") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "DrySeason";

                        } else if (SELECT_CASE_var1 == "NOWETSEASON") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "NoWetSeason";

                        } else if (SELECT_CASE_var1 == "NODRYSEASON") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "NoDrySeason";

                        } else if ((SELECT_CASE_var1 == "NODRYSEASONMAX") || (SELECT_CASE_var1 == "NOWETSEASONMAX")) {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle;
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue1 = "TropicalHot";
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue2 = "SummerExtreme";

                        } else if ((SELECT_CASE_var1 == "NODRYSEASONMIN") || (SELECT_CASE_var1 == "NOWETSEASONMIN")) {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = dataWeatherManager.TypicalExtremePeriods(Count).ShortTitle;
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue1 = "TropicalCold";
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue2 = "WinterExtreme";

                        } else if (SELECT_CASE_var1 == "TROPICALHOT") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "TropicalHot";
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue1 = "SummerExtreme";
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue2 = "NoDrySeasonMax";

                        } else if (SELECT_CASE_var1 == "TROPICALCOLD") {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "TropicalCold";
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue1 = "WinterExtreme";
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue2 = "NoDrySeasonMin";

                        } else {
                            dataWeatherManager.TypicalExtremePeriods(Count).MatchValue = "Invalid - no match";
                        }
                    }
                    dataWeatherManager.TypicalExtremePeriods(Count).StartJDay =
                        General::OrdinalDay(dataWeatherManager.TypicalExtremePeriods(Count).StartMonth, dataWeatherManager.TypicalExtremePeriods(Count).StartDay, 0);
                    dataWeatherManager.TypicalExtremePeriods(Count).EndJDay =
                        General::OrdinalDay(dataWeatherManager.TypicalExtremePeriods(Count).EndMonth, dataWeatherManager.TypicalExtremePeriods(Count).EndDay, 0);
                    if (dataWeatherManager.TypicalExtremePeriods(Count).StartJDay <= dataWeatherManager.TypicalExtremePeriods(Count).EndJDay) {
                        dataWeatherManager.TypicalExtremePeriods(Count).TotalDays = dataWeatherManager.TypicalExtremePeriods(Count).EndJDay - dataWeatherManager.TypicalExtremePeriods(Count).StartJDay + 1;
                    } else {
                        dataWeatherManager.TypicalExtremePeriods(Count).TotalDays = General::OrdinalDay(12, 31, dataWeatherManager.LeapYearAdd) - dataWeatherManager.TypicalExtremePeriods(Count).StartJDay +
                                                                 1 + dataWeatherManager.TypicalExtremePeriods(Count).EndJDay;
                    }
                }

            } else if (SELECT_CASE_var == "GROUND TEMPERATURES") {
                // Added for ground surfaces defined with F or c factor method. TH 7/2009
                // Assume the 0.5 m set of ground temperatures
                // or first set on a weather file, if any.
                Pos = index(Line, ',');
                if (Pos != std::string::npos) {
                    NumGrndTemps = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                    if (!errFlag && NumGrndTemps >= 1) {
                        Line.erase(0, Pos + 1);
                        // skip depth, soil conductivity, soil density, soil specific heat
                        for (Count = 1; Count <= 4; ++Count) {
                            Pos = index(Line, ',');
                            if (Pos == std::string::npos) {
                                Line = BlankString;
                                break;
                            }
                            Line.erase(0, Pos + 1);
                        }
                        dataWeatherManager.GroundTempsFCFromEPWHeader = 0.0;
                        actcount = 0;
                        for (Count = 1; Count <= 12; ++Count) { // take the first set of ground temperatures.
                            Pos = index(Line, ',');
                            if (Pos != std::string::npos) {
                                Number = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                                dataWeatherManager.GroundTempsFCFromEPWHeader(Count) = Number;
                                ++actcount;
                            } else {
                                if (len(Line) > 0) {
                                    Number = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                                    dataWeatherManager.GroundTempsFCFromEPWHeader(Count) = Number;
                                    ++actcount;
                                }
                                break;
                            }
                            Line.erase(0, Pos + 1);
                        }
                        if (actcount == 12) dataWeatherManager.wthFCGroundTemps = true;
                    }
                }

            } else if (SELECT_CASE_var == "HOLIDAYS/DAYLIGHT SAVING") {
                // A1, \field LeapYear Observed
                // \type choice
                // \key Yes
                // \key No
                // \note Yes if Leap Year will be observed for this file
                // \note No if Leap Year days (29 Feb) should be ignored in this file
                // A2, \field Daylight Saving Start Day
                // A3, \field Daylight Saving End Day
                // N1, \field Number of Holidays
                // A4, \field Holiday 1 Name
                // A5, \field Holiday 1 Day
                // etc.
                // Start with Minimum number of NumHdArgs
                uppercase(Line);
                NumHdArgs = 4;
                Count = 1;
                while (Count <= NumHdArgs) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos == std::string::npos) {
                        if (len(Line) == 0) {
                            while (Pos == std::string::npos) {
                                ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA) >> Line;
                                strip(Line);
                                uppercase(Line);
                                Pos = index(Line, ',');
                            }
                        } else {
                            Pos = len(Line);
                        }
                    }

                    {
                        auto const SELECT_CASE_var1(Count);

                        if (SELECT_CASE_var1 == 1) {
                            if (Line[0] == 'Y') {
                                //              LeapYear=.TRUE.
                                dataWeatherManager.WFAllowsLeapYears = true;
                                dataWeatherManager.WFLeapYearInd = 0; // 1
                            } else {
                                //              LeapYear=.FALSE.
                                dataWeatherManager.WFAllowsLeapYears = false;
                                dataWeatherManager.WFLeapYearInd = 0;
                            }

                        } else if (SELECT_CASE_var1 == 2) {
                            // In this section, we call ProcessDateString, and if that fails, we can recover from it
                            // by setting DST to false, so we don't affect ErrorsFound

                            // call ProcessDateString with local bool (unused)
                            ProcessDateString(Line.substr(0, Pos), PMonth, PDay, PWeekDay, DateType, errflag1);
                            if (DateType != dataWeatherManager.InvalidDate) {
                                // ErrorsFound is still false after ProcessDateString
                                if (PMonth == 0 && PDay == 0) {
                                    dataWeatherManager.EPWDaylightSaving = false;
                                } else {
                                    dataWeatherManager.EPWDaylightSaving = true;
                                    dataWeatherManager.EPWDST.StDateType = DateType;
                                    dataWeatherManager.EPWDST.StMon = PMonth;
                                    dataWeatherManager.EPWDST.StDay = PDay;
                                    dataWeatherManager.EPWDST.StWeekDay = PWeekDay;
                                }
                            } else {
                                // ErrorsFound is untouched
                                ShowContinueError("ProcessEPWHeader: Invalid Daylight Saving Period Start Date Field(WeatherFile)=" +
                                                  Line.substr(0, Pos));
                                ShowContinueError("...invalid header=" + HeaderString);
                                ShowContinueError("...Setting Weather File DST to false.");
                                dataWeatherManager.EPWDaylightSaving = false;
                            }

                        } else if (SELECT_CASE_var1 == 3) {
                            ProcessDateString(Line.substr(0, Pos), PMonth, PDay, PWeekDay, DateType, ErrorsFound);
                            if (dataWeatherManager.EPWDaylightSaving) {
                                if (DateType != dataWeatherManager.InvalidDate) {
                                    dataWeatherManager.EPWDST.EnDateType = DateType;
                                    dataWeatherManager.EPWDST.EnMon = PMonth;
                                    dataWeatherManager.EPWDST.EnDay = PDay;
                                    dataWeatherManager.EPWDST.EnWeekDay = PWeekDay;
                                } else {
                                    ShowWarningError("ProcessEPWHeader: Invalid Daylight Saving Period End Date Field(WeatherFile)=" +
                                                     Line.substr(0, Pos));
                                    ShowContinueError("...Setting Weather File DST to false.");
                                    dataWeatherManager.EPWDaylightSaving = false;
                                }
                                dataWeatherManager.DST = dataWeatherManager.EPWDST;
                            }

                        } else if (SELECT_CASE_var1 == 4) {
                            NumEPWHolidays = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                            dataWeatherManager.NumSpecialDays = NumEPWHolidays + inputProcessor->getNumObjectsFound("RunPeriodControl:SpecialDays");
                            dataWeatherManager.SpecialDays.allocate(dataWeatherManager.NumSpecialDays);
                            NumHdArgs = 4 + NumEPWHolidays * 2;
                            CurCount = 0;

                        } else if ((SELECT_CASE_var1 >= 5)) {
                            if (mod(Count, 2) != 0) {
                                ++CurCount;
                                if (CurCount > dataWeatherManager.NumSpecialDays) {
                                    ShowSevereError("Too many SpecialDays");
                                    ErrorsFound = true;
                                } else {
                                    dataWeatherManager.SpecialDays(CurCount).Name = Line.substr(0, Pos);
                                }
                                // Process name
                            } else {
                                if (CurCount <= dataWeatherManager.NumSpecialDays) {
                                    // Process date
                                    ProcessDateString(Line.substr(0, Pos), PMonth, PDay, PWeekDay, DateType, ErrorsFound);
                                    if (DateType == dataWeatherManager.MonthDay) {
                                        dataWeatherManager.SpecialDays(CurCount).DateType = DateType;
                                        dataWeatherManager.SpecialDays(CurCount).Month = PMonth;
                                        dataWeatherManager.SpecialDays(CurCount).Day = PDay;
                                        dataWeatherManager.SpecialDays(CurCount).WeekDay = 0;
                                        dataWeatherManager.SpecialDays(CurCount).CompDate = PMonth * 32 + PDay;
                                        dataWeatherManager.SpecialDays(CurCount).Duration = 1;
                                        dataWeatherManager.SpecialDays(CurCount).DayType = 1;
                                        dataWeatherManager.SpecialDays(CurCount).WthrFile = true;
                                    } else if (DateType != dataWeatherManager.InvalidDate) {
                                        dataWeatherManager.SpecialDays(CurCount).DateType = DateType;
                                        dataWeatherManager.SpecialDays(CurCount).Month = PMonth;
                                        dataWeatherManager.SpecialDays(CurCount).Day = PDay;
                                        dataWeatherManager.SpecialDays(CurCount).WeekDay = PWeekDay;
                                        dataWeatherManager.SpecialDays(CurCount).CompDate = 0;
                                        dataWeatherManager.SpecialDays(CurCount).Duration = 1;
                                        dataWeatherManager.SpecialDays(CurCount).DayType = 1;
                                        dataWeatherManager.SpecialDays(CurCount).WthrFile = true;
                                    } else if (DateType == dataWeatherManager.InvalidDate) {
                                        ShowSevereError("Invalid SpecialDay Date Field(WeatherFile)=" + Line.substr(0, Pos));
                                        ErrorsFound = true;
                                    }
                                }
                            }
                        }
                    }
                    Line.erase(0, Pos + 1);
                    ++Count;
                }
                for (Count = 1; Count <= dataWeatherManager.NumEPWTypExtSets; ++Count) {
                    // General::OrdinalDay (Month,Day,LeapYearValue)
                    dataWeatherManager.TypicalExtremePeriods(Count).StartJDay =
                        General::OrdinalDay(dataWeatherManager.TypicalExtremePeriods(Count).StartMonth, dataWeatherManager.TypicalExtremePeriods(Count).StartDay, dataWeatherManager.LeapYearAdd);
                    dataWeatherManager.TypicalExtremePeriods(Count).EndJDay =
                        General::OrdinalDay(dataWeatherManager.TypicalExtremePeriods(Count).EndMonth, dataWeatherManager.TypicalExtremePeriods(Count).EndDay, dataWeatherManager.LeapYearAdd);
                    if (dataWeatherManager.TypicalExtremePeriods(Count).StartJDay <= dataWeatherManager.TypicalExtremePeriods(Count).EndJDay) {
                        dataWeatherManager.TypicalExtremePeriods(Count).TotalDays = dataWeatherManager.TypicalExtremePeriods(Count).EndJDay - dataWeatherManager.TypicalExtremePeriods(Count).StartJDay + 1;
                    } else {
                        dataWeatherManager.TypicalExtremePeriods(Count).TotalDays = General::OrdinalDay(12, 31, dataWeatherManager.LeapYearAdd) - dataWeatherManager.TypicalExtremePeriods(Count).StartJDay +
                                                                 1 + dataWeatherManager.TypicalExtremePeriods(Count).EndJDay;
                    }
                }

            } else if ((SELECT_CASE_var == "COMMENTS 1") || (SELECT_CASE_var == "COMMENTS 2")) {

            } else if (SELECT_CASE_var == "DATA PERIODS") {
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
                uppercase(Line);
                NumHdArgs = 2;
                Count = 1;
                while (Count <= NumHdArgs) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos == std::string::npos) {
                        if (len(Line) == 0) {
                            while (Pos == std::string::npos) {
                                ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA) >> Line;
                                strip(Line);
                                uppercase(Line);
                                Pos = index(Line, ',');
                            }
                        } else {
                            Pos = len(Line);
                        }
                    }

                    {
                        auto const SELECT_CASE_var1(Count);

                        if (SELECT_CASE_var1 == 1) {
                            dataWeatherManager.NumDataPeriods = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                            dataWeatherManager.DataPeriods.allocate(dataWeatherManager.NumDataPeriods);
                            NumHdArgs += 4 * dataWeatherManager.NumDataPeriods;
                            if (dataWeatherManager.NumDataPeriods > 0) {
                                for (auto &e : dataWeatherManager.DataPeriods)
                                    e.NumDays = 0;
                            }
                            CurCount = 0;

                        } else if (SELECT_CASE_var1 == 2) {
                            dataWeatherManager.NumIntervalsPerHour = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                            //          IF (NumIntervalsPerHour /= 1) THEN
                            //            CALL ShowSevereError('Process EPW: Not ready for more than one interval per hour')
                            //            ErrorsFound=.TRUE.
                            //          ENDIF

                        } else if ((SELECT_CASE_var1 >= 3)) {
                            CurOne = mod(Count - 3, 4);

                            {
                                auto const SELECT_CASE_var2(CurOne);

                                if (SELECT_CASE_var2 == 0) {
                                    // Description of Data Period
                                    ++CurCount;
                                    if (CurCount > dataWeatherManager.NumDataPeriods) {
                                        ShowSevereError("Too many data periods");
                                        ErrorsFound = true;
                                    } else {
                                        dataWeatherManager.DataPeriods(CurCount).Name = Line.substr(0, Pos);
                                    }

                                } else if (SELECT_CASE_var2 == 1) {
                                    // Start Day of Week
                                    if (CurCount <= dataWeatherManager.NumDataPeriods) {
                                        dataWeatherManager.DataPeriods(CurCount).DayOfWeek = Line.substr(0, Pos);
                                        dataWeatherManager.DataPeriods(CurCount).WeekDay =
                                            UtilityRoutines::FindItemInList(dataWeatherManager.DataPeriods(CurCount).DayOfWeek, DaysOfWeek, 7);
                                        if (dataWeatherManager.DataPeriods(CurCount).WeekDay == 0) {
                                            ShowSevereError(
                                                fmt::format("Weather File -- Invalid Start Day of Week for Data Period #{}, Invalid day={}",
                                                            CurCount,
                                                            dataWeatherManager.DataPeriods(CurCount).DayOfWeek));
                                            ErrorsFound = true;
                                        }
                                    }

                                } else if (SELECT_CASE_var2 == 2) {
                                    // DataPeriod Start Day
                                    if (CurCount <= dataWeatherManager.NumDataPeriods) {
                                        ProcessDateString(Line.substr(0, Pos), PMonth, PDay, PWeekDay, DateType, ErrorsFound, PYear);
                                        if (DateType == dataWeatherManager.MonthDay) {
                                            dataWeatherManager.DataPeriods(CurCount).StMon = PMonth;
                                            dataWeatherManager.DataPeriods(CurCount).StDay = PDay;
                                            dataWeatherManager.DataPeriods(CurCount).StYear = PYear;
                                            if (PYear != 0) dataWeatherManager.DataPeriods(CurCount).HasYearData = true;
                                        } else {
                                            ShowSevereError("Data Periods must be of the form <DayOfYear> or <Month Day> (WeatherFile), found=" +
                                                            Line.substr(0, Pos));
                                            ErrorsFound = true;
                                        }
                                    }

                                } else if (SELECT_CASE_var2 == 3) {
                                    if (CurCount <= dataWeatherManager.NumDataPeriods) {
                                        ProcessDateString(Line.substr(0, Pos), PMonth, PDay, PWeekDay, DateType, ErrorsFound, PYear);
                                        if (DateType == dataWeatherManager.MonthDay) {
                                            dataWeatherManager.DataPeriods(CurCount).EnMon = PMonth;
                                            dataWeatherManager.DataPeriods(CurCount).EnDay = PDay;
                                            dataWeatherManager.DataPeriods(CurCount).EnYear = PYear;
                                            if (PYear == 0 && dataWeatherManager.DataPeriods(CurCount).HasYearData) {
                                                ShowWarningError("Data Period (WeatherFile) - Start Date contains year. End Date does not.");
                                                ShowContinueError("...Assuming same year as Start Date for this data.");
                                                dataWeatherManager.DataPeriods(CurCount).EnYear = dataWeatherManager.DataPeriods(CurCount).StYear;
                                            }
                                        } else {
                                            ShowSevereError("Data Periods must be of the form <DayOfYear> or <Month Day>, (WeatherFile) found=" +
                                                            Line.substr(0, Pos));
                                            ErrorsFound = true;
                                        }
                                    }
                                    if (dataWeatherManager.DataPeriods(CurCount).StYear == 0 || dataWeatherManager.DataPeriods(CurCount).EnYear == 0) {
                                        dataWeatherManager.DataPeriods(CurCount).DataStJDay =
                                            General::OrdinalDay(dataWeatherManager.DataPeriods(CurCount).StMon, dataWeatherManager.DataPeriods(CurCount).StDay, dataWeatherManager.LeapYearAdd);
                                        dataWeatherManager.DataPeriods(CurCount).DataEnJDay =
                                            General::OrdinalDay(dataWeatherManager.DataPeriods(CurCount).EnMon, dataWeatherManager.DataPeriods(CurCount).EnDay, dataWeatherManager.LeapYearAdd);
                                        if (dataWeatherManager.DataPeriods(CurCount).DataStJDay <= dataWeatherManager.DataPeriods(CurCount).DataEnJDay) {
                                            dataWeatherManager.DataPeriods(CurCount).NumDays = dataWeatherManager.DataPeriods(CurCount).DataEnJDay - dataWeatherManager.DataPeriods(CurCount).DataStJDay + 1;
                                        } else {
                                            dataWeatherManager.DataPeriods(CurCount).NumDays =
                                                (365 - dataWeatherManager.DataPeriods(CurCount).DataStJDay + 1) + (dataWeatherManager.DataPeriods(CurCount).DataEnJDay - 1 + 1);
                                        }
                                    } else { // weather file has actual year(s)
                                        JGDate(dataWeatherManager.GregorianToJulian,
                                               dataWeatherManager.DataPeriods(CurCount).DataStJDay,
                                               dataWeatherManager.DataPeriods(CurCount).StYear,
                                               dataWeatherManager.DataPeriods(CurCount).StMon,
                                               dataWeatherManager.DataPeriods(CurCount).StDay);
                                        JGDate(dataWeatherManager.GregorianToJulian,
                                               dataWeatherManager.DataPeriods(CurCount).DataEnJDay,
                                               dataWeatherManager.DataPeriods(CurCount).EnYear,
                                               dataWeatherManager.DataPeriods(CurCount).EnMon,
                                               dataWeatherManager.DataPeriods(CurCount).EnDay);
                                        dataWeatherManager.DataPeriods(CurCount).NumDays = dataWeatherManager.DataPeriods(CurCount).DataEnJDay - dataWeatherManager.DataPeriods(CurCount).DataStJDay + 1;
                                    }
                                    // Have processed the last item for this, can set up Weekdays for months
                                    dataWeatherManager.DataPeriods(CurCount).MonWeekDay = 0;
                                    if (!ErrorsFound) {
                                        SetupWeekDaysByMonth(dataWeatherManager, dataWeatherManager.DataPeriods(CurCount).StMon,
                                                             dataWeatherManager.DataPeriods(CurCount).StDay,
                                                             dataWeatherManager.DataPeriods(CurCount).WeekDay,
                                                             dataWeatherManager.DataPeriods(CurCount).MonWeekDay);
                                    }
                                }
                            }
                        }
                    }
                    Line.erase(0, Pos + 1);
                    ++Count;
                }

            } else {
                ShowFatalError("Invalid EPW Header designation found=" + HeaderString);
            }
        }
    }

    void SkipEPlusWFHeader(WeatherManagerData &dataWeatherManager)
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

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtA("(A)");
        static std::string const Header("DATA PERIODS");

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
        while (StillLooking) {
            {
                IOFlags flags;
                ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA, flags) >> Line;
                if (flags.end()) goto Label9998;
            }
            uppercase(Line);
            if (has(Line, Header)) break;
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
        while (Count <= NumHdArgs) {
            strip(Line);
            Pos = index(Line, ',');
            if (Pos == std::string::npos) {
                if (len(Line) == 0) {
                    while (Pos == std::string::npos) {
                        ObjexxFCL::gio::read(dataWeatherManager.WeatherFileUnitNumber, fmtA) >> Line;
                        strip(Line);
                        uppercase(Line);
                        Pos = index(Line, ',');
                    }
                } else {
                    Pos = len(Line);
                }
            }

            {
                auto const SELECT_CASE_var(Count);

                if (SELECT_CASE_var == 1) {
                    NumPeriods = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                    NumHdArgs += 4 * NumPeriods;
                    CurCount = 0;

                } else if (SELECT_CASE_var == 2) {

                } else if ((SELECT_CASE_var >= 3)) {
                    CurOne = mod(Count - 3, 4);

                    {
                        auto const SELECT_CASE_var1(CurOne);

                        if (SELECT_CASE_var1 == 0) {
                            // Description of Data Period
                            ++CurCount;

                        } else if ((SELECT_CASE_var1 >= 1) && (SELECT_CASE_var1 <= 3)) {
                        }
                    }
                }
            }
            Line.erase(0, Pos + 1);
            ++Count;
        }

        return;

    Label9998:;
        ShowFatalError("Unexpected End-of-File on EPW Weather file, while reading header information, looking for header=" + Header,
                       OptionalOutputFileRef{OutputFiles::getSingleton().eso});
    }

    void ReportMissing_RangeData(WeatherManagerData &dataWeatherManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports the counts of missing/out of range data
        // for weather file environments.

        static std::string const MissString("Missing Data Found on Weather Data File");
        static constexpr auto msFmt("Missing {}, Number of items={:5}");
        static std::string const InvString("Invalid Data Found on Weather Data File");
        static constexpr auto ivFmt("Invalid {}, Number of items={:5}");
        static std::string const RangeString("Out of Range Data Found on Weather Data File");
        static constexpr auto rgFmt("Out of Range {} [{},{}], Number of items={:5}");

        bool MissedHeader;
        bool OutOfRangeHeader;

        if (!DisplayWeatherMissingDataWarnings) return;

        MissedHeader = false;
        if (dataWeatherManager.Missed.DryBulb > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Dry Bulb Temperatures\"", dataWeatherManager.Missed.DryBulb));
        }
        if (dataWeatherManager.Missed.StnPres > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Atmospheric Pressure\"", dataWeatherManager.Missed.StnPres));
        }
        if (dataWeatherManager.Missed.RelHumid > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Relative Humidity\"", dataWeatherManager.Missed.RelHumid));
        }
        if (dataWeatherManager.Missed.DewPoint > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Dew Point Temperatures\"", dataWeatherManager.Missed.DewPoint));
        }
        if (dataWeatherManager.Missed.WindSpd > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Wind Speed\"", dataWeatherManager.Missed.WindSpd));
        }
        if (dataWeatherManager.Missed.WindDir > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Wind Direction\"", dataWeatherManager.Missed.WindDir));
        }
        if (dataWeatherManager.Missed.DirectRad > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Direct Radiation\"", dataWeatherManager.Missed.DirectRad));
        }
        if (dataWeatherManager.Missed.DiffuseRad > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Diffuse Radiation\"", dataWeatherManager.Missed.DiffuseRad));
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
        if (dataWeatherManager.Missed.TotSkyCvr > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Total Sky Cover\"", dataWeatherManager.Missed.TotSkyCvr));
        }
        if (dataWeatherManager.Missed.OpaqSkyCvr > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Opaque Sky Cover\"", dataWeatherManager.Missed.OpaqSkyCvr));
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
        if (dataWeatherManager.Missed.SnowDepth > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Snow Depth\"", dataWeatherManager.Missed.SnowDepth));
        }
        if (dataWeatherManager.Missed.WeathCodes > 0) {
            ShowWarningError(InvString);
            ShowMessage(format(ivFmt, "\"Weather Codes\" (not equal 9 digits)", dataWeatherManager.Missed.WeathCodes));
        }
        //  IF (Missed%Albedo>0) THEN
        //    IF (.not. MissedHeader) THEN
        //      CALL ShowWarningError(MissString)
        //      MissedHeader=.TRUE.
        //    ENDIF
        //    WRITE(ErrString,msFMT) '"Albedo"',Missed%Albedo
        //    CALL ShowMessage(ErrString)
        //  ENDIF
        if (dataWeatherManager.Missed.LiquidPrecip > 0) {
            if (!MissedHeader) {
                ShowWarningError(MissString);
                MissedHeader = true;
            }
            ShowMessage(format(msFmt, "\"Liquid Precipitation Depth\"", dataWeatherManager.Missed.LiquidPrecip));
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
        if (dataWeatherManager.OutOfRange.DryBulb > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Dry Bulb Temperatures", ">=-90", "<=70", dataWeatherManager.OutOfRange.DryBulb));
        }
        if (dataWeatherManager.OutOfRange.StnPres > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Atmospheric Pressure", ">31000", "<=120000", dataWeatherManager.OutOfRange.StnPres));
            ShowMessage("Out of Range values set to last good value");
        }
        if (dataWeatherManager.OutOfRange.RelHumid > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Relative Humidity", ">=0", "<=110", dataWeatherManager.OutOfRange.RelHumid));
        }
        if (dataWeatherManager.OutOfRange.DewPoint > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Dew Point Temperatures", ">=-90", "<=70", dataWeatherManager.OutOfRange.DewPoint));
        }
        if (dataWeatherManager.OutOfRange.WindSpd > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Wind Speed", ">=0", "<=40", dataWeatherManager.OutOfRange.WindSpd));
        }
        if (dataWeatherManager.OutOfRange.WindDir > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Wind Direction", ">=0", "<=360", dataWeatherManager.OutOfRange.WindDir));
        }
        if (dataWeatherManager.OutOfRange.DirectRad > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Direct Radiation", ">=0", "NoLimit", dataWeatherManager.OutOfRange.DirectRad));
        }
        if (dataWeatherManager.OutOfRange.DiffuseRad > 0) {
            if (!OutOfRangeHeader) {
                ShowWarningError(RangeString);
                OutOfRangeHeader = true;
            }
            ShowMessage(format(rgFmt, "Diffuse Radiation", ">=0", "NoLimit", dataWeatherManager.OutOfRange.DiffuseRad));
        }
    }

    void SetupInterpolationValues(WeatherManagerData &dataWeatherManager)
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

        int halfpoint;
        int hpoint;
        int tloop;
        Real64 tweight;
        Real64 tweight1;

        dataWeatherManager.Interpolation.allocate(NumOfTimeStepInHour);
        dataWeatherManager.SolarInterpolation.allocate(NumOfTimeStepInHour);
        dataWeatherManager.Interpolation = 0.0;
        dataWeatherManager.SolarInterpolation = 0.0;
        halfpoint = 0;

        for (tloop = 1; tloop <= NumOfTimeStepInHour; ++tloop) {
            if (NumOfTimeStepInHour == 1) {
                tweight = 1.0;
            } else {
                tweight = min(1.0, (double(tloop) / double(NumOfTimeStepInHour)));
            }

            dataWeatherManager.Interpolation(tloop) = tweight;
        }

        if (mod(NumOfTimeStepInHour, 2) == 0) {
            // even number of time steps.
            halfpoint = NumOfTimeStepInHour / 2;
            dataWeatherManager.SolarInterpolation(halfpoint) = 1.0;
            tweight = 1.0 / double(NumOfTimeStepInHour);
            hpoint = 1;
            for (tloop = halfpoint + 1; tloop <= NumOfTimeStepInHour; ++tloop) {
                dataWeatherManager.SolarInterpolation(tloop) = 1.0 - hpoint * tweight;
                ++hpoint;
            }
            hpoint = 1;
            for (tloop = halfpoint - 1; tloop >= 1; --tloop) {
                dataWeatherManager.SolarInterpolation(tloop) = 1.0 - hpoint * tweight;
                ++hpoint;
            }
        } else { // odd number of time steps
            if (NumOfTimeStepInHour == 1) {
                dataWeatherManager.SolarInterpolation(1) = 0.5;
            } else if (NumOfTimeStepInHour == 3) {
                tweight = 1.0 / double(NumOfTimeStepInHour);
                dataWeatherManager.SolarInterpolation(1) = 5.0 / 6.0;
                dataWeatherManager.SolarInterpolation(2) = 5.0 / 6.0;
                dataWeatherManager.SolarInterpolation(3) = 0.5;
            } else {
                tweight = 1.0 / double(NumOfTimeStepInHour);
                halfpoint = NumOfTimeStepInHour / 2;
                tweight1 = 1.0 - tweight / 2.0;
                dataWeatherManager.SolarInterpolation(halfpoint) = tweight1;
                dataWeatherManager.SolarInterpolation(halfpoint + 1) = tweight1;
                hpoint = 1;
                for (tloop = halfpoint + 2; tloop <= NumOfTimeStepInHour; ++tloop) {
                    dataWeatherManager.SolarInterpolation(tloop) = tweight1 - hpoint * tweight;
                    ++hpoint;
                }
                hpoint = 1;
                for (tloop = halfpoint - 1; tloop >= 1; --tloop) {
                    dataWeatherManager.SolarInterpolation(tloop) = tweight1 - hpoint * tweight;
                    ++hpoint;
                }
            }
        }
    }

    void SetupEnvironmentTypes(WeatherManagerData &dataWeatherManager)
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
        int LocalLeapYearAdd;

        // Transfer weather file information to the Environment derived type
        dataWeatherManager.Envrn = TotDesDays + 1;

        // Sizing Periods from Weather File
        for (Loop = 1; Loop <= dataWeatherManager.TotRunDesPers; ++Loop) {
            auto &env = dataWeatherManager.Environment(dataWeatherManager.Envrn);
            auto &runPer = dataWeatherManager.RunPeriodDesignInput(Loop);

            env.StartMonth = runPer.startMonth;
            env.StartDay = runPer.startDay;
            env.StartJDay = General::OrdinalDay(runPer.startMonth, runPer.startDay, dataWeatherManager.LeapYearAdd);
            env.TotalDays = runPer.totalDays;
            env.EndMonth = runPer.endMonth;
            env.EndDay = runPer.endDay;
            env.EndJDay = General::OrdinalDay(runPer.endMonth, runPer.endDay, dataWeatherManager.LeapYearAdd);
            env.NumSimYears = runPer.numSimYears;
            if (env.StartJDay <= env.EndJDay) {
                env.TotalDays = (env.EndJDay - env.StartJDay + 1) * env.NumSimYears;
            } else {
                env.TotalDays = (General::OrdinalDay(12, 31, dataWeatherManager.LeapYearAdd) - env.StartJDay + 1 + env.EndJDay) * env.NumSimYears;
            }
            TotRunDesPersDays += env.TotalDays;
            env.UseDST = runPer.useDST;
            env.UseHolidays = runPer.useHolidays;
            env.Title = runPer.title;
            env.cKindOfEnvrn = runPer.periodType;
            env.KindOfEnvrn = ksRunPeriodDesign;
            env.DesignDayNum = 0;
            env.RunPeriodDesignNum = Loop;
            env.DayOfWeek = runPer.dayOfWeek;
            env.MonWeekDay = runPer.monWeekDay;
            env.SetWeekDays = false;
            env.ApplyWeekendRule = runPer.applyWeekendRule;
            env.UseRain = runPer.useRain;
            env.UseSnow = runPer.useSnow;
            ++dataWeatherManager.Envrn;
        }

        // RunPeriods from weather file
        for (Loop = 1; Loop <= dataWeatherManager.TotRunPers; ++Loop) { // Run Periods.
            auto &env = dataWeatherManager.Environment(dataWeatherManager.Envrn);
            auto &runPer = dataWeatherManager.RunPeriodInput(Loop);

            env.StartMonth = runPer.startMonth;
            env.StartDay = runPer.startDay;
            env.StartYear = runPer.startYear;
            env.EndMonth = runPer.endMonth;
            env.EndDay = runPer.endDay;
            env.EndYear = runPer.endYear;
            env.NumSimYears = runPer.numSimYears;
            env.CurrentYear = runPer.startYear;
            env.IsLeapYear = runPer.isLeapYear;
            env.TreatYearsAsConsecutive = true;
            if (runPer.actualWeather) {
                // This will require leap years to be present, thus Julian days can be used for all the calculations
                env.StartJDay = env.StartDate = runPer.startJulianDate;
                env.EndJDay = env.EndDate = runPer.endJulianDate;
                env.TotalDays = env.EndDate - env.StartDate + 1;
                env.RawSimDays = env.EndDate - env.StartDate + 1;
                env.MatchYear = true;
                env.ActualWeather = true;
            } else { // std RunPeriod
                env.RollDayTypeOnRepeat = runPer.RollDayTypeOnRepeat;
                if (env.StartYear == env.EndYear) {
                    // Short-circuit all the calculations, we're in a single year
                    LocalLeapYearAdd = 0;
                    if (isLeapYear(env.StartYear)) {
                        // If a leap year is supported by the weather file, do it.
                        if (dataWeatherManager.WFAllowsLeapYears) {
                            env.IsLeapYear = true; // explicit set, this might be unwise
                            LocalLeapYearAdd = 1;
                        } else {
                            env.IsLeapYear = false; // explicit set, this might be unwise
                        }
                    }
                    env.StartJDay = General::OrdinalDay(runPer.startMonth, runPer.startDay, LocalLeapYearAdd);
                    env.EndJDay = General::OrdinalDay(runPer.endMonth, runPer.endDay, LocalLeapYearAdd);
                    env.RawSimDays = (env.EndJDay - env.StartJDay + 1);
                    env.TotalDays = env.RawSimDays;
                } else {
                    // Environment crosses year boundaries
                    env.RollDayTypeOnRepeat = runPer.RollDayTypeOnRepeat;
                    env.StartJDay = General::OrdinalDay(runPer.startMonth, runPer.startDay, runPer.isLeapYear ? 1 : 0);
                    env.EndJDay = General::OrdinalDay(runPer.endMonth, runPer.endDay, isLeapYear(runPer.endYear) && dataWeatherManager.WFAllowsLeapYears ? 1 : 0);
                    env.TotalDays = 366 - env.StartJDay + env.EndJDay + 365 * std::max(env.NumSimYears - 2, 0);
                    if (dataWeatherManager.WFAllowsLeapYears) {
                        // First year
                        if (env.StartJDay < 59) {
                            if (isLeapYear(env.StartYear)) {
                                ++env.TotalDays;
                            }
                        }
                        // Middle years
                        for (int yr = env.StartYear + 1; yr < env.EndYear; ++yr) {
                            if (isLeapYear(yr)) {
                                ++env.TotalDays;
                            }
                        }
                        // Last year not needed, the end ordinal date will take this into account
                    }
                    env.RawSimDays = env.TotalDays;
                }
            }
            env.UseDST = runPer.useDST;
            env.UseHolidays = runPer.useHolidays;
            if (runPer.title == BlankString) {
                env.Title = WeatherFileLocationTitle;
            } else {
                env.Title = runPer.title;
            }
            if (env.KindOfEnvrn == ksReadAllWeatherData) {
                env.cKindOfEnvrn = "ReadAllWeatherDataRunPeriod";
            } else {
                env.cKindOfEnvrn = "WeatherFileRunPeriod";
                env.KindOfEnvrn = ksRunPeriodWeather;
            }
            env.DayOfWeek = runPer.dayOfWeek;
            env.MonWeekDay = runPer.monWeekDay;
            env.SetWeekDays = false;
            env.ApplyWeekendRule = runPer.applyWeekendRule;
            env.UseRain = runPer.useRain;
            env.UseSnow = runPer.useSnow;
            ++dataWeatherManager.Envrn;
        }
    }

    bool isLeapYear(int const Year)
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

        if (mod(Year, 4) == 0) { // Potential Leap Year
            if (!(mod(Year, 100) == 0 && mod(Year, 400) != 0)) {
                return true;
            }
        }
        return false;
    }

    void JGDate(int const jflag, // indicates direction of conversion,
                int &jdate,      // input/output julian date, typically a 7 or 8 digit integer
                int &gyyyy,      // input/output gregorian year, should be specified as 4 digits
                int &gmm,        // input/output gregorian month
                int &gdd         // input/output gregorian day
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
        int tmm;   // integer*4 variable needed for double precision arithmetic
        int tdd;   // integer*4 variable needed for double precision arithmetic
        int l;     // temporary variable used in conversion.
        int n;     // temporary variable used in conversion.

        //                                       gregorian to julian
        if (jflag == 1) {
            tyyyy = gyyyy;
            tmm = gmm;
            tdd = gdd;
            l = (tmm - 14) / 12;
            jdate = tdd - 32075 + 1461 * (tyyyy + 4800 + l) / 4 + 367 * (tmm - 2 - l * 12) / 12 - 3 * ((tyyyy + 4900 + l) / 100) / 4;

        } else if (jflag == 2) {
            //                                       julian to gregorian
            tdate = jdate;
            l = tdate + 68569;
            n = 4 * l / 146097;
            l -= (146097 * n + 3) / 4;
            tyyyy = 4000 * (l + 1) / 1461001;
            l = l - 1461 * tyyyy / 4 + 31;
            tmm = 80 * l / 2447;
            tdd = l - 2447 * tmm / 80;
            l = tmm / 11;
            tmm += 2 - 12 * l;
            tyyyy += 100 * (n - 49) + l;
            // c
            gyyyy = tyyyy;
            gdd = tdd;
            gmm = tmm;
        }
        // c
    }

    int computeJulianDate(int const gyyyy, // input/output gregorian year, should be specified as 4 digits
                          int const gmm,   // input/output gregorian month
                          int const gdd    // input/output gregorian day
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason DeGraw
        //       DATE WRITTEN   10/25/2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Split the former JGDate function in two. Convert a gregorian
        // date to actual julian date.  the advantage of storing a julian date
        // in the jdate format rather than a 5 digit format is that any
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
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int tyyyy; // integer*4 variable needed for double precision arithmetic
        int tmm;   // integer*4 variable needed for double precision arithmetic
        int tdd;   // integer*4 variable needed for double precision arithmetic
        int l;     // temporary variable used in conversion.

        tyyyy = gyyyy;
        tmm = gmm;
        tdd = gdd;
        l = (tmm - 14) / 12;
        return tdd - 32075 + 1461 * (tyyyy + 4800 + l) / 4 + 367 * (tmm - 2 - l * 12) / 12 - 3 * ((tyyyy + 4900 + l) / 100) / 4;
    }

    int CalculateDayOfWeek(WeatherManagerData &dataWeatherManager, int const JulianDate) // from JGDate calculation
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

        // Return value
        int DayOfWeek; // EnergyPlus convention (1=Sunday, 2=Monday, etc)

        int JulDate; // Julian date copy
        int Gyyyy;   // Gregorian yyyy
        int Gmm;     // Gregorian mm
        int Gdd;     // Gregorian dd

        JulDate = JulianDate;
        JGDate(dataWeatherManager.JulianToGregorian, JulDate, Gyyyy, Gmm, Gdd);

        // Jan, Feb are 13, 14 months of previous year
        if (Gmm < 3) {
            Gmm += 12;
            --Gyyyy;
        }

        DayOfWeek = mod(Gdd + (13 * (Gmm + 1) / 5) + Gyyyy + (Gyyyy / 4) + 6 * (Gyyyy / 100) + (Gyyyy / 400), 7);
        if (DayOfWeek == 0) DayOfWeek = 7;

        return DayOfWeek;
    }

    WeekDay calculateDayOfWeek(int const year, int const month, int const day)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2012
        //       MODIFIED       October 2017, Jason DeGraw
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculate the correct day of week.

        // METHODOLOGY EMPLOYED:
        // Zeller's algorithm.

        // REFERENCES:
        // http://en.wikipedia.org/wiki/Zeller%27s_congruence
        // and other references around the web.

        // USE STATEMENTS:
        // na

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na
        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Gyyyy(year); // Gregorian yyyy
        int Gmm(month);  // Gregorian mm

        // Jan, Feb are 13, 14 months of previous year
        if (Gmm < 3) {
            Gmm += 12;
            --Gyyyy;
        }

        DayOfWeek = mod(day + (13 * (Gmm + 1) / 5) + Gyyyy + (Gyyyy / 4) + 6 * (Gyyyy / 100) + (Gyyyy / 400), 7);
        if (DayOfWeek == 0) DayOfWeek = 7;

        return static_cast<WeekDay>(DayOfWeek);
    }

    int calculateDayOfYear(int const Month, int const Day)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Jason DeGraw
        //       DATE WRITTEN   October 10, 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Compute the day of the year for non-leap years.

        // METHODOLOGY EMPLOYED:
        // Lookup table.

        // REFERENCES:
        // NA

        // USE STATEMENTS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        static std::array<int, 12> daysbefore{{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334}};

        // Could probably do some bounds checking here, but for now assume the month is in [1, 12]
        return daysbefore[Month - 1] + Day;
    }

    int calculateDayOfYear(int const Month, int const Day, bool const leapYear)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Jason DeGraw
        //       DATE WRITTEN   October 10, 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Compute the day of the year for leap and non-leap years.

        // METHODOLOGY EMPLOYED:
        // Lookup table.

        static std::array<int, 12> daysbefore{{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334}};
        static std::array<int, 12> daysbeforeleap{{0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335}};

        // Could probably do some bounds checking here, but for now assume the month is in [1, 12]
        if (leapYear) {
            return daysbeforeleap[Month - 1] + Day;
        } else {
            return daysbefore[Month - 1] + Day;
        }
    }

    bool validMonthDay(int const month, int const day, int const leapYearAdd)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Jason DeGraw
        //       DATE WRITTEN   October 31, 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Determine if a month/day+leapyear combination is valid.

        // METHODOLOGY EMPLOYED:
        // Lookup table.

        switch (month) {
        case 1:
        case 3:
        case 5:
        case 7:
        case 8:
        case 10:
        case 12:
            if (day > 31) {
                return false;
            }
            break;
        case 4:
        case 6:
        case 9:
        case 11:
            if (day > 30) {
                return false;
            }
            break;
        case 2:
            if (day > 28 + leapYearAdd) {
                return false;
            }
            break;
        default:
            return false;
        }
        return true;
    }

    void AnnualMonthlyDryBulbWeatherData::CalcAnnualAndMonthlyDryBulbTemp(WeatherManagerData &dataWeatherManager)
    {

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates monthly daily average outdoor air drybulb temperature from
        // either weather (*.EPW) file or reads monthly daily average outdoor air
        // drybulb temperature from STAT (*.stat) for use to autosize main water
        // temperature.

        // METHODOLOGY EMPLOYED:
        // Opens and processes the weather or stat file only once

        // Using/Aliasing
        using OutputReportTabular::GetColumnUsingTabs;
        using OutputReportTabular::StrToReal;

        static ObjexxFCL::gio::Fmt fmtA("(A)");

        Real64 HourlyDryBulbTemp;                                  // hourly outside air dry-bulb temperature read from weather file
        Real64 MonthlyDailyDryBulbMin(200.0);                      // monthly-daily minimum outside air dry-bulb temperature
        Real64 MonthlyDailyDryBulbMax(-200.0);                     // monthly-daily maximum outside air dry-bulb temperature
        Real64 MonthlyDailyDryBulbAvg(0.0);                        // monthly-daily average outside air dry-bulb temperature
        Real64 AnnualDailyAverageDryBulbTempSum(0.0);              // annual sum of daily average outside air dry-bulb temperature
        static Real64 DailyAverageDryBulbTemp(0.0);                // daily average outside air dry-bulb temperature
        static Array1D<Real64> MonthlyAverageDryBulbTemp(12, 0.0); // monthly-daily average outside air temperature
        static Array1D<int> EndDayOfMonthLocal(12, 0);             // number of days in each month
        std::string lineIn;
        std::string lineAvg;
        std::string epwLine;
        std::string::size_type pos;
        int AnnualNumberOfDays(0);
        int i;
        int j;
        int ind;
        int readStat;
        int statFile;
        int epwFile;
        bool statFileExists;
        bool epwFileExists;
        bool epwHasLeapYear(false);

        if (!dataWeatherManager.OADryBulbAverage.OADryBulbWeatherDataProcessed) {
            {
                {
                    IOFlags flags;
                    ObjexxFCL::gio::inquire(DataStringGlobals::inStatFileName, flags);
                    statFileExists = flags.exists();
                }
                {
                    IOFlags flags;
                    ObjexxFCL::gio::inquire(DataStringGlobals::inputWeatherFileName, flags);
                    epwFileExists = flags.exists();
                }
                readStat = 0;
            }
            readStat = 0;
            if (statFileExists) {
                statFile = GetNewUnitNumber();
                {
                    IOFlags flags;
                    flags.ACTION("READ");
                    ObjexxFCL::gio::open(statFile, DataStringGlobals::inStatFileName, flags);
                    readStat = flags.ios();
                }
                if (readStat != 0) {
                    ShowSevereError("CalcAnnualAndMonthlyDryBulbTemp: Could not open file " + DataStringGlobals::inStatFileName +
                                    " for input (read).");
                    ShowContinueError("Water Mains Temperature will be set to a fixed deafult value of 10.0 C.");
                    return;
                }
                while (readStat == 0) {
                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(statFile, fmtA, flags) >> lineIn;
                        readStat = flags.ios();
                    }
                    if (has(lineIn, "Monthly Statistics for Dry Bulb temperatures")) {
                        for (i = 1; i <= 7; ++i) {
                            {
                                IOFlags flags;
                                ObjexxFCL::gio::read(statFile, fmtA, flags);
                                readStat = flags.ios();
                            }
                        }
                        {
                            IOFlags flags;
                            ObjexxFCL::gio::read(statFile, fmtA, flags) >> lineAvg;
                            readStat = flags.ios();
                        }
                        break;
                    }
                }
                ObjexxFCL::gio::close(statFile);
                AnnualNumberOfDays = 0;
                for (i = 1; i <= 12; ++i) {
                    MonthlyAverageDryBulbTemp(i) = StrToReal(GetColumnUsingTabs(lineAvg, i + 2));
                    AnnualDailyAverageDryBulbTempSum += MonthlyAverageDryBulbTemp(i) * dataWeatherManager.EndDayOfMonth(i);
                    MonthlyDailyDryBulbMin = min(MonthlyDailyDryBulbMin, MonthlyAverageDryBulbTemp(i));
                    MonthlyDailyDryBulbMax = max(MonthlyDailyDryBulbMax, MonthlyAverageDryBulbTemp(i));
                    AnnualNumberOfDays += dataWeatherManager.EndDayOfMonth(i);
                }
                dataWeatherManager.OADryBulbAverage.AnnualAvgOADryBulbTemp = AnnualDailyAverageDryBulbTempSum / AnnualNumberOfDays;
                dataWeatherManager.OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff = MonthlyDailyDryBulbMax - MonthlyDailyDryBulbMin;
                dataWeatherManager.OADryBulbAverage.MonthlyDailyAverageDryBulbTemp = MonthlyAverageDryBulbTemp;
                dataWeatherManager.OADryBulbAverage.OADryBulbWeatherDataProcessed = true;
            } else if (epwFileExists) {
                epwFile = GetNewUnitNumber();
                {
                    IOFlags flags;
                    flags.ACTION("READ");
                    ObjexxFCL::gio::open(epwFile, DataStringGlobals::inputWeatherFileName, flags);
                    readStat = flags.ios();
                }
                if (readStat != 0) {
                    ShowSevereError("CalcAnnualAndMonthlyDryBulbTemp: Could not open file " + DataStringGlobals::inputWeatherFileName +
                                    " for input (read).");
                    ShowContinueError("Water Mains Temperature will be set to a fixed deafult value of 10.0 C.");
                    return;
                }
                for (i = 1; i <= 8; ++i) { // Headers
                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(epwFile, fmtA, flags) >> epwLine;
                        readStat = flags.ios();
                    }
                    if (i == 5) {
                        // HOLIDAYS/DAYLIGHT SAVINGS,Yes,0,0,0
                        pos = index(epwLine, ',');
                        epwLine.erase(0, pos + 1);
                        pos = index(epwLine, ',');
                        std::string LeapYear = UtilityRoutines::MakeUPPERCase(epwLine.substr(0, pos));
                        if (LeapYear[0] == 'Y') {
                            epwHasLeapYear = true;
                        }
                    }
                }
                EndDayOfMonthLocal = dataWeatherManager.EndDayOfMonth;
                if (epwHasLeapYear) {
                    // increase number of days for february by one day if weather data has leap year
                    EndDayOfMonthLocal(2) = EndDayOfMonthLocal(2) + 1;
                }
                int DayNum;
                int DaysCountOfMonth;
                for (i = 1; i <= 12; ++i) {
                    MonthlyDailyDryBulbAvg = 0.0;
                    DaysCountOfMonth = EndDayOfMonthLocal(i);
                    for (DayNum = 1; DayNum <= DaysCountOfMonth; ++DayNum) {
                        DailyAverageDryBulbTemp = 0.0;
                        for (j = 1; j <= 24; ++j) {
                            {
                                IOFlags flags;
                                ObjexxFCL::gio::read(epwFile, fmtA, flags) >> epwLine;
                                readStat = flags.ios();
                            }
                            for (ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine, ',');
                                epwLine.erase(0, pos + 1);
                            }
                            pos = index(epwLine, ',');
                            HourlyDryBulbTemp = StrToReal(epwLine.substr(0, pos));
                            DailyAverageDryBulbTemp += (HourlyDryBulbTemp / 24.0);
                        }
                        AnnualDailyAverageDryBulbTempSum += DailyAverageDryBulbTemp;
                        MonthlyDailyDryBulbAvg += (DailyAverageDryBulbTemp / DaysCountOfMonth);
                    }
                    MonthlyAverageDryBulbTemp(i) = MonthlyDailyDryBulbAvg;
                    MonthlyDailyDryBulbMin = min(MonthlyDailyDryBulbMin, MonthlyDailyDryBulbAvg);
                    MonthlyDailyDryBulbMax = max(MonthlyDailyDryBulbMax, MonthlyDailyDryBulbAvg);
                }
                ObjexxFCL::gio::close(epwFile);
                // calculate annual average outdoor air dry-bulb temperature and monthly daily average
                // outdoor air temperature maximum difference
                AnnualNumberOfDays = 365;
                if (epwHasLeapYear) AnnualNumberOfDays = AnnualNumberOfDays + 1;
                dataWeatherManager.OADryBulbAverage.AnnualAvgOADryBulbTemp = AnnualDailyAverageDryBulbTempSum / AnnualNumberOfDays;
                dataWeatherManager.OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff = MonthlyDailyDryBulbMax - MonthlyDailyDryBulbMin;
                dataWeatherManager.OADryBulbAverage.MonthlyDailyAverageDryBulbTemp = MonthlyAverageDryBulbTemp;
                dataWeatherManager.OADryBulbAverage.OADryBulbWeatherDataProcessed = true;
            } else {
                ShowSevereError("CalcAnnualAndMonthlyDryBulbTemp: weather file or stat file does not exist.");
                ShowContinueError("Weather file: " + DataStringGlobals::inputWeatherFileName + ".");
                ShowContinueError("Stat file: " + DataStringGlobals::inStatFileName + ".");
                ShowContinueError("Water Mains Monthly Temperature cannot be calculated using CorrelationFromWeatherFile method.");
                ShowContinueError("Instead a fixed default value of 10.0 C will be used.");
            }
        }
    }

    void ReportWaterMainsTempParameters(WeatherManagerData &dataWeatherManager)
    {
        // PURPOSE OF THIS SUBROUTINE:
        // report site water mains temperature object user inputs and/or parameters calculated
        // from weather or stat file

        // USE STATEMENTS:
        using General::RoundSigDigits;
        using namespace ObjexxFCL::gio;

        Array1D_string const cCalculationMethod({1, 3}, {"Schedule", "Correlation", "CorrelationFromWeatherFile"});

        if (!OutputFiles::getSingleton().eio.good()) {
            return;
        }

        std::stringstream ss;
        auto *eiostream = &ss;

        // Write annual average OA temperature and maximum difference in monthly-daily average outdoor air temperature
        *eiostream << "! <Site Water Mains Temperature Information>"
                      ",Calculation Method{}"
                      ",Water Mains Temperature Schedule Name{}"
                      ",Annual Average Outdoor Air Temperature{C}"
                      ",Maximum Difference In Monthly Average Outdoor Air Temperatures{deltaC}"
                      ",Fixed Default Water Mains Temperature{C}\n";

        {
            auto const SELECT_CASE_var(dataWeatherManager.WaterMainsTempsMethod);
            if (SELECT_CASE_var == dataWeatherManager.ScheduleMethod) {
                *eiostream << "Site Water Mains Temperature Information,";
                *eiostream << cCalculationMethod(dataWeatherManager.WaterMainsTempsMethod) << "," << dataWeatherManager.WaterMainsTempsScheduleName << ",";
                *eiostream << RoundSigDigits(dataWeatherManager.WaterMainsTempsAnnualAvgAirTemp, 2) << "," << RoundSigDigits(dataWeatherManager.WaterMainsTempsMaxDiffAirTemp, 2) << ",";
                *eiostream << "NA\n";
            } else if (SELECT_CASE_var == dataWeatherManager.CorrelationMethod) {
                *eiostream << "Site Water Mains Temperature Information,";
                *eiostream << cCalculationMethod(dataWeatherManager.WaterMainsTempsMethod) << ","
                           << "NA"
                           << ",";
                *eiostream << RoundSigDigits(dataWeatherManager.WaterMainsTempsAnnualAvgAirTemp, 2) << "," << RoundSigDigits(dataWeatherManager.WaterMainsTempsMaxDiffAirTemp, 2) << ",";
                *eiostream << "NA\n";
            } else if (SELECT_CASE_var == dataWeatherManager.CorrelationFromWeatherFileMethod) {
                if (dataWeatherManager.OADryBulbAverage.OADryBulbWeatherDataProcessed) {
                    *eiostream << "Site Water Mains Temperature Information,";
                    *eiostream << cCalculationMethod(dataWeatherManager.WaterMainsTempsMethod) << ","
                               << "NA"
                               << ",";
                    *eiostream << RoundSigDigits(dataWeatherManager.OADryBulbAverage.AnnualAvgOADryBulbTemp, 2) << ","
                               << RoundSigDigits(dataWeatherManager.OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff, 2) << ","
                               << "NA\n";
                } else {
                    *eiostream << "Site Water Mains Temperature Information,";
                    *eiostream << "FixedDefault"
                               << ","
                               << "NA"
                               << ","
                               << "NA"
                               << ","
                               << "NA"
                               << "," << RoundSigDigits(10.0, 1) << '\n';
                }
            } else {
                *eiostream << "Site Water Mains Temperature Information,";
                *eiostream << "FixedDefault"
                           << ","
                           << "NA"
                           << ","
                           << "NA"
                           << ","
                           << "NA"
                           << "," << RoundSigDigits(10.0, 1) << '\n';
            }
        }

        print(OutputFiles::getSingleton().eio, "{}", ss.str());
    }
} // namespace WeatherManager

} // namespace EnergyPlus
