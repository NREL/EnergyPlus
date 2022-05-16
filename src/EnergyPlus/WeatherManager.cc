// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/time.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/StringUtilities.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Vectors.hh>
#include <EnergyPlus/WaterManager.hh>
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

    // Data

    Array1D_string const DaysOfWeek(7, {"SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY"}); // NOLINT(cert-err58-cpp)
    std::map<std::string, WeekDay> weekDayLookUp{{"SUNDAY", WeekDay::Sunday},                                           // NOLINT(cert-err58-cpp)
                                                 {"MONDAY", WeekDay::Monday},
                                                 {"TUESDAY", WeekDay::Tuesday},
                                                 {"WEDNESDAY", WeekDay::Wednesday},
                                                 {"THURSDAY", WeekDay::Thursday},
                                                 {"FRIDAY", WeekDay::Friday},
                                                 {"SATURDAY", WeekDay::Saturday}};

    // Functions

    void ManageWeather(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 1997
        //       MODIFIED       June 1997 (general clean-up)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main driver of the weather manager module.
        // It controls the assignment of weather related global variables as
        // well as the reads and writes for weather information.

        InitializeWeather(state, state.dataWeatherManager->PrintEnvrnStamp);

        bool anyEMSRan = false;
        // Cannot call this during sizing, because EMS will not initialize properly until after simulation kickoff
        if (!state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
            EMSManager::ManageEMS(state,
                                  EMSManager::EMSCallFrom::BeginZoneTimestepBeforeSetCurrentWeather,
                                  anyEMSRan,
                                  ObjexxFCL::Optional_int_const()); // calling point
        }
        SetCurrentWeather(state);

        ReportWeatherAndTimeInformation(state, state.dataWeatherManager->PrintEnvrnStamp);
    }

    void ResetEnvironmentCounter(EnergyPlusData &state)
    {
        state.dataWeatherManager->Envrn = 0;
    }

    bool CheckIfAnyUnderwaterBoundaries(EnergyPlusData &state)
    {
        bool errorsFound = false;
        int NumAlpha = 0, NumNumber = 0, IOStat = 0;
        state.dataIPShortCut->cCurrentModuleObject = "SurfaceProperty:Underwater";
        int Num = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        for (int i = 1; i <= Num; i++) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     i,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlpha,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            state.dataWeatherManager->underwaterBoundaries.emplace_back();
            auto &underwaterBoundary{state.dataWeatherManager->underwaterBoundaries[i - 1]};
            underwaterBoundary.Name = state.dataIPShortCut->cAlphaArgs(1);
            underwaterBoundary.distanceFromLeadingEdge = state.dataIPShortCut->rNumericArgs(1);
            underwaterBoundary.OSCMIndex = UtilityRoutines::FindItemInList(underwaterBoundary.Name, state.dataSurface->OSCM);
            if (underwaterBoundary.OSCMIndex <= 0) {
                ShowSevereError(state, "Could not match underwater boundary condition object with an Other Side Conditions Model input object.");
                errorsFound = true;
            }
            underwaterBoundary.WaterTempScheduleIndex = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (underwaterBoundary.WaterTempScheduleIndex == 0) {
                ShowSevereError(state,
                                R"(Water temperature schedule for "SurfaceProperty:Underwater" named ")" + underwaterBoundary.Name + "\" not found");
                errorsFound = true;
            }
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                // that's OK, we can have a blank schedule, the water will just have no free stream velocity
                underwaterBoundary.VelocityScheduleIndex = 0;
            } else {
                underwaterBoundary.VelocityScheduleIndex = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                if (underwaterBoundary.WaterTempScheduleIndex == 0) {
                    ShowSevereError(state,
                                    R"(Free stream velocity schedule for "SurfaceProperty:Underwater" named ")" + underwaterBoundary.Name +
                                        "\" not found");
                    errorsFound = true;
                }
            }
            if (errorsFound) break;
        }
        if (errorsFound) {
            ShowFatalError(state, "Previous input problems cause program termination");
        }
        return (Num > 0);
    }

    Real64
    calculateWaterBoundaryConvectionCoefficient(Real64 const curWaterTemp, Real64 const freeStreamVelocity, Real64 const distanceFromLeadingEdge)
    {
        Real64 constexpr waterKinematicViscosity = 1e-6; // m2/s
        Real64 constexpr waterPrandtlNumber = 6;         // -
        Real64 constexpr waterThermalConductivity = 0.6; // W/mK
        // do some calculation for forced convection from the leading edge of the ship
        Real64 const localReynoldsNumber = freeStreamVelocity * distanceFromLeadingEdge / waterKinematicViscosity;
        Real64 const localNusseltNumber = 0.0296 * pow(localReynoldsNumber, 0.8) * pow(waterPrandtlNumber, 1.0 / 3.0);
        Real64 const localConvectionCoeff = localNusseltNumber * waterThermalConductivity / distanceFromLeadingEdge;

        // do some calculations for natural convection from the bottom of the ship
        Real64 constexpr distanceFromBottomOfHull = 12; // meters, assumed for now
                                                        // this Prandtl correction is from Incropera & Dewitt, Intro to HT, eq 9.20
        Real64 const prandtlCorrection =
            (0.75 * pow(waterPrandtlNumber, 0.5)) / pow(0.609 + 1.221 * pow(waterPrandtlNumber, 0.5) + 1.238 * waterPrandtlNumber, 0.25);
        // calculate the Grashof number
        Real64 constexpr gravity = 9.81;          // m/s2
        Real64 constexpr beta = 0.000214;         // water thermal expansion coefficient, from engineeringtoolbox.com, 1/C
        Real64 constexpr assumedSurfaceTemp = 25; // Grashof requires a surface temp, this should suffice
        Real64 const localGrashofNumber =
            (gravity * beta * (assumedSurfaceTemp - curWaterTemp) * pow(distanceFromBottomOfHull, 3)) / pow(waterKinematicViscosity, 2);
        Real64 const localNusseltFreeConvection = pow(localGrashofNumber / 4, 0.25) * prandtlCorrection;
        Real64 const localConvectionCoeffFreeConv = localNusseltFreeConvection * waterThermalConductivity / distanceFromBottomOfHull;
        return max(localConvectionCoeff, localConvectionCoeffFreeConv);
    }

    void UpdateUnderwaterBoundaries(EnergyPlusData &state)
    {
        for (auto &thisBoundary : state.dataWeatherManager->underwaterBoundaries) {
            Real64 const curWaterTemp = ScheduleManager::GetCurrentScheduleValue(state, thisBoundary.WaterTempScheduleIndex); // C
            Real64 freeStreamVelocity = 0;
            if (thisBoundary.VelocityScheduleIndex > 0) {
                freeStreamVelocity = ScheduleManager::GetCurrentScheduleValue(state, thisBoundary.VelocityScheduleIndex); // m/s
            }
            state.dataSurface->OSCM(thisBoundary.OSCMIndex).TConv = curWaterTemp;
            state.dataSurface->OSCM(thisBoundary.OSCMIndex).HConv =
                WeatherManager::calculateWaterBoundaryConvectionCoefficient(curWaterTemp, freeStreamVelocity, thisBoundary.distanceFromLeadingEdge);
            state.dataSurface->OSCM(thisBoundary.OSCMIndex).TRad = curWaterTemp;
            state.dataSurface->OSCM(thisBoundary.OSCMIndex).HRad = 0.0;
        }
    }

    void ReadVariableLocationOrientation(EnergyPlusData &state)
    {
        int NumAlpha = 0, NumNumber = 0, IOStat = 0;
        state.dataIPShortCut->cCurrentModuleObject = "Site:VariableLocation";
        if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject) == 0) return;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlpha,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        state.dataEnvrn->varyingLocationSchedIndexLat = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(1));
        state.dataEnvrn->varyingLocationSchedIndexLong = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
        state.dataEnvrn->varyingOrientationSchedIndex = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
    }

    void UpdateLocationAndOrientation(EnergyPlusData &state)
    {
        if (state.dataEnvrn->varyingLocationSchedIndexLat > 0) {
            state.dataEnvrn->Latitude = ScheduleManager::GetCurrentScheduleValue(state, state.dataEnvrn->varyingLocationSchedIndexLat);
        }
        if (state.dataEnvrn->varyingLocationSchedIndexLong > 0) {
            state.dataEnvrn->Longitude = ScheduleManager::GetCurrentScheduleValue(state, state.dataEnvrn->varyingLocationSchedIndexLong);
        }
        CheckLocationValidity(state);
        if (state.dataEnvrn->varyingOrientationSchedIndex > 0) {
            state.dataHeatBal->BuildingAzimuth =
                mod(ScheduleManager::GetCurrentScheduleValue(state, state.dataEnvrn->varyingOrientationSchedIndex), 360.0);
            state.dataSurfaceGeometry->CosBldgRelNorth =
                std::cos(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SinBldgRelNorth =
                std::sin(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);
            for (size_t SurfNum = 1; SurfNum < state.dataSurface->Surface.size(); ++SurfNum) {
                for (int n = 1; n <= state.dataSurface->Surface(SurfNum).Sides; ++n) {
                    Real64 Xb = state.dataSurface->Surface(SurfNum).Vertex(n).x;
                    Real64 Yb = state.dataSurface->Surface(SurfNum).Vertex(n).y;
                    state.dataSurface->Surface(SurfNum).NewVertex(n).x =
                        Xb * state.dataSurfaceGeometry->CosBldgRelNorth - Yb * state.dataSurfaceGeometry->SinBldgRelNorth;
                    state.dataSurface->Surface(SurfNum).NewVertex(n).y =
                        Xb * state.dataSurfaceGeometry->SinBldgRelNorth + Yb * state.dataSurfaceGeometry->CosBldgRelNorth;
                    state.dataSurface->Surface(SurfNum).NewVertex(n).z = state.dataSurface->Surface(SurfNum).Vertex(n).z;
                }
                Vectors::CreateNewellSurfaceNormalVector(state.dataSurface->Surface(SurfNum).NewVertex,
                                                         state.dataSurface->Surface(SurfNum).Sides,
                                                         state.dataSurface->Surface(SurfNum).NewellSurfaceNormalVector);
                Real64 SurfWorldAz = 0.0;
                Real64 SurfTilt = 0.0;
                Vectors::DetermineAzimuthAndTilt(state.dataSurface->Surface(SurfNum).NewVertex,
                                                 state.dataSurface->Surface(SurfNum).Sides,
                                                 SurfWorldAz,
                                                 SurfTilt,
                                                 state.dataSurface->Surface(SurfNum).lcsx,
                                                 state.dataSurface->Surface(SurfNum).lcsy,
                                                 state.dataSurface->Surface(SurfNum).lcsz,
                                                 state.dataSurface->Surface(SurfNum).GrossArea,
                                                 state.dataSurface->Surface(SurfNum).NewellSurfaceNormalVector);
                state.dataSurface->Surface(SurfNum).Azimuth = SurfWorldAz;
                state.dataSurface->Surface(SurfNum).SinAzim = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians);
                state.dataSurface->Surface(SurfNum).CosAzim = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians);
                state.dataSurface->Surface(SurfNum).OutNormVec = state.dataSurface->Surface(SurfNum).NewellSurfaceNormalVector;
            }
        }
    }

    bool GetNextEnvironment(EnergyPlusData &state, bool &Available, bool &ErrorsFound)
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

        static constexpr std::string_view RoutineName("GetNextEnvironment: ");
        static constexpr std::string_view EnvNameFormat("Environment,{},{},{},{},{},{},{},{},{},{},{},{},{}\n");
        static constexpr std::string_view EnvDSTNFormat("Environment:Daylight Saving,No,{}\n");
        static constexpr std::string_view EnvDSTYFormat("Environment:Daylight Saving,Yes,{},{},{}\n");
        static constexpr std::string_view DateFormat("{:02}/{:02}");
        static constexpr std::string_view DateFormatWithYear("{:02}/{:02}/{:04}");
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
        static std::map<EmissivityCalcType, std::string> const SkyTempModelNames{
            {EmissivityCalcType::ClarkAllenModel, "Clark and Allen"},
            {EmissivityCalcType::ScheduleValue, "Schedule Value"},
            {EmissivityCalcType::DryBulbDelta, "DryBulb Difference Schedule Value"},
            {EmissivityCalcType::DewPointDelta, "Dewpoint Difference Schedule Value"},
            {EmissivityCalcType::BruntModel, "Brunt"},
            {EmissivityCalcType::IdsoModel, "Idso"},
            {EmissivityCalcType::BerdahlMartinModel, "Berdahl and Martin"}};
        std::string StDate;
        std::string EnDate;
        int DSTActStMon;
        int DSTActStDay;
        int DSTActEnMon;
        int DSTActEnDay;

        if (state.dataGlobal->BeginSimFlag && state.dataWeatherManager->GetEnvironmentFirstCall) {

            state.dataReportFlag->PrintEndDataDictionary = true;

            ReportOutputFileHeaders(state); // Write the output file header information

            // Setup Output Variables, CurrentModuleObject='All Simulations'

            SetupOutputVariable(state,
                                "Site Outdoor Air Drybulb Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->OutDryBulbTemp,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Outdoor Air Dewpoint Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->OutDewPointTemp,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Outdoor Air Wetbulb Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->OutWetBulbTemp,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Outdoor Air Humidity Ratio",
                                OutputProcessor::Unit::kgWater_kgDryAir,
                                state.dataEnvrn->OutHumRat,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Outdoor Air Relative Humidity",
                                OutputProcessor::Unit::Perc,
                                state.dataEnvrn->OutRelHum,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Outdoor Air Barometric Pressure",
                                OutputProcessor::Unit::Pa,
                                state.dataEnvrn->OutBaroPress,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Wind Speed",
                                OutputProcessor::Unit::m_s,
                                state.dataEnvrn->WindSpeed,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Wind Direction",
                                OutputProcessor::Unit::deg,
                                state.dataEnvrn->WindDir,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Sky Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->SkyTemp,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Horizontal Infrared Radiation Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataWeatherManager->HorizIRSky,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Diffuse Solar Radiation Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataEnvrn->DifSolarRad,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Direct Solar Radiation Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataEnvrn->BeamSolarRad,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Liquid Precipitation Depth",
                                OutputProcessor::Unit::m,
                                state.dataEnvrn->LiquidPrecipitation,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Precipitation Rate",
                                OutputProcessor::Unit::m_s,
                                state.dataWaterData->RainFall.CurrentRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Precipitation Depth",
                                OutputProcessor::Unit::m,
                                state.dataWaterData->RainFall.CurrentAmount,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Ground Reflected Solar Radiation Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                state.dataEnvrn->GndSolarRad,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Ground Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->GroundTemp,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Surface Ground Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->GroundTemp_Surface,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Deep Ground Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->GroundTemp_Deep,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Simple Factor Model Ground Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->GroundTempFC,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Total Sky Cover",
                                OutputProcessor::Unit::None,
                                state.dataEnvrn->TotalCloudCover,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Opaque Sky Cover",
                                OutputProcessor::Unit::None,
                                state.dataEnvrn->OpaqueCloudCover,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Outdoor Air Enthalpy",
                                OutputProcessor::Unit::J_kg,
                                state.dataEnvrn->OutEnthalpy,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Outdoor Air Density",
                                OutputProcessor::Unit::kg_m3,
                                state.dataEnvrn->OutAirDensity,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Solar Azimuth Angle",
                                OutputProcessor::Unit::deg,
                                state.dataWeatherManager->SolarAzimuthAngle,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Solar Altitude Angle",
                                OutputProcessor::Unit::deg,
                                state.dataWeatherManager->SolarAltitudeAngle,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Solar Hour Angle",
                                OutputProcessor::Unit::deg,
                                state.dataWeatherManager->HrAngle,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Rain Status",
                                OutputProcessor::Unit::None,
                                state.dataWeatherManager->RptIsRain,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Snow on Ground Status",
                                OutputProcessor::Unit::None,
                                state.dataWeatherManager->RptIsSnow,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Exterior Horizontal Sky Illuminance",
                                OutputProcessor::Unit::lux,
                                state.dataEnvrn->HISKF,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Exterior Horizontal Beam Illuminance",
                                OutputProcessor::Unit::lux,
                                state.dataEnvrn->HISUNF,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Exterior Beam Normal Illuminance",
                                OutputProcessor::Unit::lux,
                                state.dataEnvrn->HISUNFnorm,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Sky Diffuse Solar Radiation Luminous Efficacy",
                                OutputProcessor::Unit::lum_W,
                                state.dataEnvrn->PDIFLW,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Beam Solar Radiation Luminous Efficacy",
                                OutputProcessor::Unit::lum_W,
                                state.dataEnvrn->PDIRLW,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Daylighting Model Sky Clearness",
                                OutputProcessor::Unit::None,
                                state.dataEnvrn->SkyClearness,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Daylighting Model Sky Brightness",
                                OutputProcessor::Unit::None,
                                state.dataEnvrn->SkyBrightness,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Daylight Saving Time Status",
                                OutputProcessor::Unit::None,
                                state.dataEnvrn->DSTIndicator,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Day Type Index",
                                OutputProcessor::Unit::None,
                                state.dataWeatherManager->RptDayType,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                "Environment");
            SetupOutputVariable(state,
                                "Site Mains Water Temperature",
                                OutputProcessor::Unit::C,
                                state.dataEnvrn->WaterMainsTemp,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                "Environment");

            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "Weather Data",
                                 "Environment",
                                 "Outdoor Dry Bulb",
                                 "[C]",
                                 state.dataEnvrn->EMSOutDryBulbOverrideOn,
                                 state.dataEnvrn->EMSOutDryBulbOverrideValue);
                SetupEMSActuator(state,
                                 "Weather Data",
                                 "Environment",
                                 "Outdoor Dew Point",
                                 "[C]",
                                 state.dataEnvrn->EMSOutDewPointTempOverrideOn,
                                 state.dataEnvrn->EMSOutDewPointTempOverrideValue);
                SetupEMSActuator(state,
                                 "Weather Data",
                                 "Environment",
                                 "Outdoor Relative Humidity",
                                 "[%]",
                                 state.dataEnvrn->EMSOutRelHumOverrideOn,
                                 state.dataEnvrn->EMSOutRelHumOverrideValue);
                SetupEMSActuator(state,
                                 "Weather Data",
                                 "Environment",
                                 "Diffuse Solar",
                                 "[W/m2]",
                                 state.dataEnvrn->EMSDifSolarRadOverrideOn,
                                 state.dataEnvrn->EMSDifSolarRadOverrideValue);
                SetupEMSActuator(state,
                                 "Weather Data",
                                 "Environment",
                                 "Direct Solar",
                                 "[W/m2]",
                                 state.dataEnvrn->EMSBeamSolarRadOverrideOn,
                                 state.dataEnvrn->EMSBeamSolarRadOverrideValue);
                SetupEMSActuator(state,
                                 "Weather Data",
                                 "Environment",
                                 "Wind Speed",
                                 "[m/s]",
                                 state.dataEnvrn->EMSWindSpeedOverrideOn,
                                 state.dataEnvrn->EMSWindSpeedOverrideValue);
                SetupEMSActuator(state,
                                 "Weather Data",
                                 "Environment",
                                 "Wind Direction",
                                 "[deg]",
                                 state.dataEnvrn->EMSWindDirOverrideOn,
                                 state.dataEnvrn->EMSWindDirOverrideValue);
            }
            state.dataWeatherManager->GetEnvironmentFirstCall = false;

        } // ... end of DataGlobals::BeginSimFlag IF-THEN block.

        if (state.dataWeatherManager->GetBranchInputOneTimeFlag) {

            SetupInterpolationValues(state);
            state.dataWeatherManager->TimeStepFraction = 1.0 / double(state.dataGlobal->NumOfTimeStepInHour);
            state.dataEnvrn->rhoAirSTP = Psychrometrics::PsyRhoAirFnPbTdbW(
                state, DataEnvironment::StdPressureSeaLevel, DataPrecisionGlobals::constant_twenty, DataPrecisionGlobals::constant_zero);
            OpenWeatherFile(state, ErrorsFound); // moved here because of possibility of special days on EPW file
            CloseWeatherFile(state);
            ReadUserWeatherInput(state);
            AllocateWeatherData(state);
            if (state.dataWeatherManager->NumIntervalsPerHour != 1) {
                if (state.dataWeatherManager->NumIntervalsPerHour != state.dataGlobal->NumOfTimeStepInHour) {
                    ShowSevereError(state,
                                    std::string{RoutineName} +
                                        "Number of intervals per hour on Weather file does not match specified number of Time Steps Per Hour");
                    ErrorsFound = true;
                }
            }
            state.dataWeatherManager->GetBranchInputOneTimeFlag = false;
            state.dataWeatherManager->Envrn = 0;
            if (state.dataWeatherManager->NumOfEnvrn > 0) {
                ResolveLocationInformation(state, ErrorsFound); // Obtain weather related info from input file
                CheckLocationValidity(state);
                if ((state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).KindOfEnvrn !=
                     DataGlobalConstants::KindOfSim::DesignDay) &&
                    (state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).KindOfEnvrn !=
                     DataGlobalConstants::KindOfSim::HVACSizeDesignDay)) {
                    CheckWeatherFileValidity(state);
                }
                if (ErrorsFound) {
                    ShowSevereError(state, std::string{RoutineName} + "No location specified, program will terminate.");
                }
            } else {
                ErrorsFound = true;
                ShowSevereError(state, std::string{RoutineName} + "No Design Days or Run Period(s) specified, program will terminate.");
            }
            if (state.dataSysVars->DDOnly && state.dataEnvrn->TotDesDays == 0) {
                ErrorsFound = true;
                ShowSevereError(state,
                                std::string{RoutineName} +
                                    "Requested Design Days only (DataSystemVariables::DDOnly) but no Design Days specified, program will terminate.");
            }
            if (state.dataSysVars->ReverseDD && state.dataEnvrn->TotDesDays == 1) {
                ErrorsFound = true;
                ShowSevereError(
                    state,
                    std::string{RoutineName} +
                        "Requested Reverse Design Days (DataSystemVariables::ReverseDD) but only 1 Design Day specified, program will terminate.");
            }

            // Throw a Fatal now that we have said it'll terminalte
            if (ErrorsFound) {
                CloseWeatherFile(state); // will only close if opened.
                ShowFatalError(state, std::string{RoutineName} + "Errors found in Weather Data Input. Program terminates.");
            }

            state.dataEnvrn->CurrentOverallSimDay = 0;
            state.dataEnvrn->TotalOverallSimDays = 0;
            state.dataEnvrn->MaxNumberSimYears = 1;
            for (int i = 1; i <= state.dataWeatherManager->NumOfEnvrn; ++i) {
                state.dataEnvrn->TotalOverallSimDays += state.dataWeatherManager->Environment(i).TotalDays;
                if (state.dataWeatherManager->Environment(i).KindOfEnvrn == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                    state.dataEnvrn->MaxNumberSimYears =
                        max(state.dataEnvrn->MaxNumberSimYears, state.dataWeatherManager->Environment(i).NumSimYears);
                }
            }
            DisplaySimDaysProgress(state, state.dataEnvrn->CurrentOverallSimDay, state.dataEnvrn->TotalOverallSimDays);
        }

        CloseWeatherFile(state); // will only close if opened.
        ++state.dataWeatherManager->Envrn;
        state.dataWeatherManager->DatesShouldBeReset = false;
        if (state.dataWeatherManager->Envrn > state.dataWeatherManager->NumOfEnvrn) {
            Available = false;
            state.dataWeatherManager->Envrn = 0;
            state.dataEnvrn->CurEnvirNum = 0;
        } else {
            state.dataGlobal->KindOfSim = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn;
            state.dataEnvrn->DayOfYear = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartJDay;
            state.dataEnvrn->DayOfMonth = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay;
            state.dataGlobal->CalendarYear = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartYear;
            state.dataGlobal->CalendarYearChr = fmt::to_string(state.dataGlobal->CalendarYear);
            state.dataEnvrn->Month = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth;
            state.dataGlobal->NumOfDayInEnvrn =
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).TotalDays; // Set day loop maximum from DataGlobals
            if (!state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
                if (state.dataHeatBal->AdaptiveComfortRequested_ASH55 || state.dataHeatBal->AdaptiveComfortRequested_CEN15251) {
                    if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::DesignDay) {
                        if (state.dataGlobal->DoDesDaySim) {
                            ShowWarningError(state, std::string{RoutineName} + "Adaptive Comfort being reported during design day.");
                            Real64 GrossApproxAvgDryBulb = (state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Envrn).MaxDryBulb +
                                                            (state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Envrn).MaxDryBulb -
                                                             state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Envrn).DailyDBRange)) /
                                                           2.0;
                            if (state.dataHeatBal->AdaptiveComfortRequested_ASH55)
                                ThermalComfort::CalcThermalComfortAdaptiveASH55(state, true, false, GrossApproxAvgDryBulb);
                            if (state.dataHeatBal->AdaptiveComfortRequested_CEN15251)
                                ThermalComfort::CalcThermalComfortAdaptiveCEN15251(state, true, false, GrossApproxAvgDryBulb);
                        }
                    } else {
                        if (state.dataGlobal->DoWeathSim || state.dataGlobal->DoDesDaySim) {
                            if (state.dataHeatBal->AdaptiveComfortRequested_ASH55)
                                ThermalComfort::CalcThermalComfortAdaptiveASH55(state, true, true, 0.0);
                            if (state.dataHeatBal->AdaptiveComfortRequested_CEN15251)
                                ThermalComfort::CalcThermalComfortAdaptiveCEN15251(state, true, true, 0.0);
                        }
                    }
                }
            }
            if (state.dataWeatherManager->Envrn > state.dataEnvrn->TotDesDays && state.dataWeatherManager->WeatherFileExists) {
                OpenEPlusWeatherFile(state, ErrorsFound, false);
            }
            Available = true;
            if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) &&
                (!state.dataWeatherManager->WeatherFileExists && state.dataGlobal->DoWeathSim)) {
                if (!state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
                    ShowSevereError(state, "Weather Simulation requested, but no weather file attached.");
                    ErrorsFound = true;
                }
                if (!state.dataGlobal->DoingHVACSizingSimulations) state.dataWeatherManager->Envrn = 0;
                Available = false;
            } else if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) &&
                       (!state.dataWeatherManager->WeatherFileExists && !state.dataGlobal->DoWeathSim)) {
                Available = false;
                if (!state.dataGlobal->DoingHVACSizingSimulations) state.dataWeatherManager->Envrn = 0;
            } else if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) && state.dataGlobal->DoingSizing) {
                Available = false;
                state.dataWeatherManager->Envrn = 0;
            }

            if (!ErrorsFound && Available && state.dataWeatherManager->Envrn > 0) {
                state.dataEnvrn->EnvironmentName = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).Title;
                state.dataEnvrn->CurEnvirNum = state.dataWeatherManager->Envrn;
                state.dataEnvrn->RunPeriodStartDayOfWeek = 0;
                if ((state.dataGlobal->DoDesDaySim && (state.dataGlobal->KindOfSim != DataGlobalConstants::KindOfSim::RunPeriodWeather)) ||
                    ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) && state.dataGlobal->DoWeathSim)) {
                    if (state.dataWeatherManager->PrntEnvHeaders && state.dataReportFlag->DoWeatherInitReporting) {
                        static constexpr std::string_view EnvironFormat(
                            "! <Environment>,Environment Name,Environment Type, Start Date, End Date, Start DayOfWeek, Duration {#days}, "
                            "Source:Start DayOfWeek,  Use Daylight Saving, Use Holidays, Apply Weekend Holiday Rule,  Use Rain Values, Use Snow "
                            "Values, Sky Temperature Model\n! <Environment:Special Days>, Special Day Name, Special Day Type, Source, Start Date, "
                            "Duration {#days}\n! "
                            "<Environment:Daylight Saving>, Daylight Saving Indicator, Source, Start Date, End Date\n! <Environment:WarmupDays>, "
                            "NumberofWarmupDays");
                        print(state.files.eio, "{}\n", EnvironFormat);
                        state.dataWeatherManager->PrntEnvHeaders = false;
                    }

                    if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) ||
                        (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodDesign)) {
                        std::string kindOfRunPeriod = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).cKindOfEnvrn;
                        state.dataEnvrn->RunPeriodEnvironment = state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather;
                        Array1D_int ActEndDayOfMonth(12);
                        ActEndDayOfMonth = state.dataWeatherManager->EndDayOfMonth;
                        state.dataEnvrn->CurrentYearIsLeapYear = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).IsLeapYear;
                        if (state.dataEnvrn->CurrentYearIsLeapYear && state.dataWeatherManager->WFAllowsLeapYears) {
                            state.dataWeatherManager->LeapYearAdd = 1;
                        } else {
                            state.dataWeatherManager->LeapYearAdd = 0;
                        }
                        if (state.dataEnvrn->CurrentYearIsLeapYear) {
                            ActEndDayOfMonth(2) = state.dataWeatherManager->EndDayOfMonth(2) + state.dataWeatherManager->LeapYearAdd;
                        }
                        state.dataWeatherManager->UseDaylightSaving = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseDST;
                        state.dataWeatherManager->UseSpecialDays = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseHolidays;
                        state.dataWeatherManager->UseRainValues = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseRain;
                        state.dataWeatherManager->UseSnowValues = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseSnow;

                        bool missingLeap(false); // Defer acting on anything found here until after the other range checks (see below)
                        if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ActualWeather &&
                            !state.dataWeatherManager->WFAllowsLeapYears) {
                            for (int year = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartYear;
                                 year <= state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndYear;
                                 year++) {
                                if (isLeapYear(year)) {
                                    ShowSevereError(state,
                                                    format("{}Weatherfile does not support leap years but runperiod includes a leap year ({})",
                                                           RoutineName,
                                                           year));
                                    missingLeap = true;
                                }
                            }
                        }

                        bool OkRun = false;

                        if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ActualWeather) {
                            // Actual weather
                            for (auto &dataperiod : state.dataWeatherManager->DataPeriods) {
                                int runStartJulian = dataperiod.DataStJDay;
                                int runEndJulian = dataperiod.DataEnJDay;
                                if (!dataperiod.HasYearData) {
                                    ShowSevereError(state,
                                                    std::string{RoutineName} +
                                                        "Actual weather runperiod has been entered but weatherfile DATA PERIOD "
                                                        "does not have year included in start/end date.");
                                    ShowContinueError(state, "...to match the RunPeriod, the DATA PERIOD should be mm/dd/yyyy for both, or");
                                    ShowContinueError(state, R"(...set "Treat Weather as Actual" to "No".)");
                                }
                                if (!General::BetweenDates(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDate,
                                                           runStartJulian,
                                                           runEndJulian))
                                    continue;
                                if (!General::BetweenDates(
                                        state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDate, runStartJulian, runEndJulian))
                                    continue;
                                OkRun = true;
                                break;
                            }
                        } else {
                            // Typical (or just non-actual) weather
                            for (auto &dataperiod : state.dataWeatherManager->DataPeriods) {
                                // Since this is not actual weather, there may be issues with this calculation
                                // Assume the weather data starts the same year as the simulation, so LeapYearAdd is what
                                // should be used.
                                int runStartOrdinal = General::OrdinalDay(dataperiod.StMon, dataperiod.StDay, state.dataWeatherManager->LeapYearAdd);
                                // This one is harder, leave as is for now. What about multiple years of data?
                                int runEndOrdinal = General::OrdinalDay(dataperiod.EnMon, dataperiod.EnDay, state.dataWeatherManager->LeapYearAdd);
                                if (runStartOrdinal == 1 && (runEndOrdinal == 366 || runEndOrdinal == 365)) {
                                    // Complete year(s) of weather data, will wrap around
                                    OkRun = true;
                                    break;
                                }
                                if (!General::BetweenDates(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartJDay,
                                                           runStartOrdinal,
                                                           runEndOrdinal))
                                    continue;
                                if (!General::BetweenDates(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndJDay,
                                                           runStartOrdinal,
                                                           runEndOrdinal))
                                    continue;
                                OkRun = true;
                            }
                        }

                        if (!OkRun) {
                            if (!state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ActualWeather) {
                                StDate = format(DateFormat,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay);
                                EnDate = format(DateFormat,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay);
                                ShowSevereError(state,
                                                std::string{RoutineName} + "Runperiod [mm/dd] (Start=" + StDate + ",End=" + EnDate +
                                                    ") requested not within Data Period(s) from Weather File");
                            } else {
                                StDate = format(DateFormatWithYear,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartYear);
                                EnDate = format(DateFormatWithYear,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay,
                                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndYear);
                                ShowSevereError(state,
                                                std::string{RoutineName} + "Runperiod [mm/dd/yyyy] (Start=" + StDate + ",End=" + EnDate +
                                                    ") requested not within Data Period(s) from Weather File");
                            }
                            StDate =
                                format(DateFormat, state.dataWeatherManager->DataPeriods(1).StMon, state.dataWeatherManager->DataPeriods(1).StDay);
                            EnDate =
                                format(DateFormat, state.dataWeatherManager->DataPeriods(1).EnMon, state.dataWeatherManager->DataPeriods(1).EnDay);
                            if (state.dataWeatherManager->DataPeriods(1).StYear > 0) {
                                StDate += format("/{}", state.dataWeatherManager->DataPeriods(1).StYear);
                            } else {
                                StDate += "/<noyear>";
                            }
                            if (state.dataWeatherManager->DataPeriods(1).EnYear > 0) {
                                EnDate += format("/{}", state.dataWeatherManager->DataPeriods(1).EnYear);
                            } else {
                                EnDate += "/<noyear>";
                            }
                            if (state.dataWeatherManager->NumDataPeriods == 1) {
                                ShowContinueError(state, "Weather Data Period (Start=" + StDate + ",End=" + EnDate + ')');
                            } else {
                                ShowContinueError(state, "Multiple Weather Data Periods 1st (Start=" + StDate + ",End=" + EnDate + ')');
                            }
                            ShowFatalError(state, std::string{RoutineName} + "Program terminates due to preceding condition.");
                        }

                        if (missingLeap) {
                            // Bail out now if we still need to
                            ShowFatalError(state, std::string{RoutineName} + "Program terminates due to preceding condition.");
                        }

                        // Following builds Environment start/end for ASHRAE 55 warnings
                        StDate = format(DateFormat,
                                        state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                        state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay);
                        EnDate = format(DateFormat,
                                        state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                        state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay);
                        if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn ==
                            DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                            StDate += format("/{}", state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartYear);
                            EnDate += format("/{}", state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndYear);
                        }
                        state.dataEnvrn->EnvironmentStartEnd = StDate + " - " + EnDate;
                        state.dataEnvrn->StartYear = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartYear;
                        state.dataEnvrn->EndYear = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndYear;

                        int TWeekDay = (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DayOfWeek == 0)
                                           ? 1
                                           : state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DayOfWeek;
                        auto const &MonWeekDay = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay;

                        if (state.dataReportFlag->DoWeatherInitReporting) {
                            std::string const AlpUseDST =
                                (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseDST) ? "Yes" : "No";
                            std::string const AlpUseSpec =
                                (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseHolidays) ? "Yes" : "No";
                            std::string const ApWkRule =
                                (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ApplyWeekendRule) ? "Yes" : "No";
                            std::string const AlpUseRain =
                                (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseRain) ? "Yes" : "No";
                            std::string const AlpUseSnow =
                                (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseSnow) ? "Yes" : "No";

                            print(state.files.eio,
                                  EnvNameFormat,
                                  state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).Title,
                                  kindOfRunPeriod,
                                  StDate,
                                  EnDate,
                                  ValidDayNames(TWeekDay),
                                  fmt::to_string(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).TotalDays),
                                  "Use RunPeriod Specified Day",
                                  AlpUseDST,
                                  AlpUseSpec,
                                  ApWkRule,
                                  AlpUseRain,
                                  AlpUseSnow,
                                  SkyTempModelNames.at(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel));
                        }

                        if (!state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
                            if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather && state.dataGlobal->DoWeathSim)) {
                                if (state.dataHeatBal->AdaptiveComfortRequested_ASH55 || state.dataHeatBal->AdaptiveComfortRequested_CEN15251) {
                                    if (state.dataWeatherManager->WFAllowsLeapYears) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} +
                                                            "AdaptiveComfort Reporting does not work correctly with leap years in weather files.");
                                        ErrorsFound = true;
                                    }
                                    if (state.dataWeatherManager->NumDataPeriods != 1) {
                                        ShowSevereError(
                                            state,
                                            std::string{RoutineName} +
                                                "AdaptiveComfort Reporting does not work correctly with multiple dataperiods in weather files.");
                                        ErrorsFound = true;
                                    }
                                    if (state.dataWeatherManager->DataPeriods(1).StMon == 1 && state.dataWeatherManager->DataPeriods(1).StDay == 1) {
                                        int RunStJDay = General::OrdinalDay(state.dataWeatherManager->DataPeriods(1).StMon,
                                                                            state.dataWeatherManager->DataPeriods(1).StDay,
                                                                            state.dataWeatherManager->LeapYearAdd);
                                        int RunEnJDay = General::OrdinalDay(state.dataWeatherManager->DataPeriods(1).EnMon,
                                                                            state.dataWeatherManager->DataPeriods(1).EnDay,
                                                                            state.dataWeatherManager->LeapYearAdd);
                                        if (RunEnJDay - RunStJDay + 1 != 365) {
                                            ShowSevereError(state,
                                                            std::string{RoutineName} +
                                                                "AdaptiveComfort Reporting does not work correctly with weather files "
                                                                "that do not contain 365 days.");
                                            ErrorsFound = true;
                                        }
                                    } else {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} +
                                                            "AdaptiveComfort Reporting does not work correctly with weather files that "
                                                            "do not start on 1 January.");
                                        ErrorsFound = true;
                                    }
                                    if (state.dataWeatherManager->NumIntervalsPerHour != 1) {
                                        ShowSevereError(state,
                                                        std::string{RoutineName} +
                                                            "AdaptiveComfort Reporting does not work correctly with weather files that "
                                                            "have multiple interval records per hour.");
                                        ErrorsFound = true;
                                    }
                                }
                            }
                        }

                        // Only need to set Week days for Run Days
                        state.dataEnvrn->RunPeriodStartDayOfWeek = TWeekDay;
                        state.dataWeatherManager->WeekDayTypes = 0;
                        int JDay5Start = General::OrdinalDay(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay,
                                                             state.dataWeatherManager->LeapYearAdd);
                        int JDay5End = General::OrdinalDay(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                                           state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay,
                                                           state.dataWeatherManager->LeapYearAdd);

                        state.dataWeatherManager->curSimDayForEndOfRunPeriod =
                            state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).TotalDays;

                        {
                            int i = JDay5Start;
                            while (true) {
                                state.dataWeatherManager->WeekDayTypes(i) = TWeekDay;
                                TWeekDay = mod(TWeekDay, 7) + 1;
                                ++i;
                                if (i > 366) i = 1;
                                if (i == JDay5End) break;
                            }
                        }

                        if (state.dataWeatherManager->UseDaylightSaving) {
                            if (state.dataWeatherManager->EPWDaylightSaving) {
                                state.dataWeatherManager->DaylightSavingIsActive = true;
                            }
                        } else {
                            state.dataWeatherManager->DaylightSavingIsActive = false;
                        }
                        if (state.dataWeatherManager->IDFDaylightSaving) {
                            state.dataWeatherManager->DaylightSavingIsActive = true;
                        }
                        state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SetWeekDays = false;

                        if (state.dataWeatherManager->DaylightSavingIsActive) {
                            SetDSTDateRanges(
                                state, MonWeekDay, state.dataWeatherManager->DSTIndex, DSTActStMon, DSTActStDay, DSTActEnMon, DSTActEnDay);
                        }

                        SetSpecialDayDates(state, MonWeekDay);

                        if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth != 1 ||
                            state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay != 1) {
                            state.dataWeatherManager->StartDatesCycleShouldBeReset = true;
                            state.dataWeatherManager->Jan1DatesShouldBeReset = true;
                        }

                        if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth == 1 &&
                            state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay == 1) {
                            state.dataWeatherManager->StartDatesCycleShouldBeReset = false;
                            state.dataWeatherManager->Jan1DatesShouldBeReset = true;
                        }

                        if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ActualWeather) {
                            state.dataWeatherManager->StartDatesCycleShouldBeReset = false;
                            state.dataWeatherManager->Jan1DatesShouldBeReset = true;
                        }

                        // Report Actual Dates for Daylight Saving and Special Days
                        if (!state.dataGlobal->KickOffSimulation) {
                            std::string Source;
                            if (state.dataWeatherManager->UseDaylightSaving) {
                                if (state.dataWeatherManager->EPWDaylightSaving) {
                                    Source = "WeatherFile";
                                }
                            } else {
                                Source = "RunPeriod Object";
                            }
                            if (state.dataWeatherManager->IDFDaylightSaving) {
                                Source = "InputFile";
                            }
                            if (state.dataWeatherManager->DaylightSavingIsActive && state.dataReportFlag->DoWeatherInitReporting) {
                                StDate = format(DateFormat, DSTActStMon, DSTActStDay);
                                EnDate = format(DateFormat, DSTActEnMon, DSTActEnDay);
                                print(state.files.eio, EnvDSTYFormat, Source, StDate, EnDate);
                            } else if (state.dataGlobal->DoOutputReporting) {
                                print(state.files.eio, EnvDSTNFormat, Source);
                            }
                            for (int i = 1; i <= state.dataWeatherManager->NumSpecialDays; ++i) {
                                static constexpr std::string_view EnvSpDyFormat("Environment:Special Days,{},{},{},{},{:3}");
                                if (state.dataWeatherManager->SpecialDays(i).WthrFile && state.dataWeatherManager->UseSpecialDays &&
                                    state.dataReportFlag->DoWeatherInitReporting) {
                                    StDate = format(DateFormat,
                                                    state.dataWeatherManager->SpecialDays(i).ActStMon,
                                                    state.dataWeatherManager->SpecialDays(i).ActStDay);
                                    print(state.files.eio,
                                          EnvSpDyFormat,
                                          state.dataWeatherManager->SpecialDays(i).Name,
                                          SpecialDayNames(state.dataWeatherManager->SpecialDays(i).DayType),
                                          "WeatherFile",
                                          StDate,
                                          state.dataWeatherManager->SpecialDays(i).Duration);
                                }
                                if (!state.dataWeatherManager->SpecialDays(i).WthrFile && state.dataReportFlag->DoWeatherInitReporting) {
                                    StDate = format(DateFormat,
                                                    state.dataWeatherManager->SpecialDays(i).ActStMon,
                                                    state.dataWeatherManager->SpecialDays(i).ActStDay);
                                    print(state.files.eio,
                                          EnvSpDyFormat,
                                          state.dataWeatherManager->SpecialDays(i).Name,
                                          SpecialDayNames(state.dataWeatherManager->SpecialDays(i).DayType),
                                          "InputFile",
                                          StDate,
                                          state.dataWeatherManager->SpecialDays(i).Duration);
                                }
                            }
                        }

                    } else if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::DesignDay ||
                               state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay) { // Design Day
                        state.dataEnvrn->RunPeriodEnvironment = false;
                        StDate = format(
                            DateFormat,
                            state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                                .Month,
                            state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                                .DayOfMonth);
                        EnDate = StDate;
                        if (state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                                    .DayType <= 7 &&
                            state.dataReportFlag->DoWeatherInitReporting) {

                            print(state.files.eio,
                                  EnvNameFormat,
                                  state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).Title,
                                  "SizingPeriod:DesignDay",
                                  StDate,
                                  EnDate,
                                  DaysOfWeek(state.dataWeatherManager
                                                 ->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                                                 .DayType),
                                  "1",
                                  "N/A",
                                  "N/A",
                                  "N/A",
                                  "N/A",
                                  "N/A",
                                  "N/A",
                                  SkyTempModelNames.at(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel));
                        } else if (state.dataReportFlag->DoWeatherInitReporting) {
                            print(
                                state.files.eio,
                                EnvNameFormat,
                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).Title,
                                "SizingPeriod:DesignDay",
                                StDate,
                                EnDate,
                                SpecialDayNames(state.dataWeatherManager
                                                    ->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                                                    .DayType -
                                                7),
                                "1",
                                "N/A",
                                "N/A",
                                "N/A",
                                "N/A",
                                "N/A",
                                "N/A",
                                SkyTempModelNames.at(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel));
                        }
                        if (state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum)
                                    .DSTIndicator == 0 &&
                            state.dataReportFlag->DoWeatherInitReporting) {
                            print(state.files.eio, EnvDSTNFormat, "SizingPeriod:DesignDay");
                        } else if (state.dataReportFlag->DoWeatherInitReporting) {
                            print(state.files.eio, EnvDSTYFormat, "SizingPeriod:DesignDay", StDate, EnDate);
                        }
                    }
                }
            } // ErrorsFound
        }

        if (ErrorsFound && !state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
            ShowSevereError(state, std::string{RoutineName} + "Errors found in getting a new environment");
            Available = false;
        } else if (ErrorsFound) {
            Available = false;
        }
        return Available && !ErrorsFound;
    }

    void AddDesignSetToEnvironmentStruct(EnergyPlusData &state, int const HVACSizingIterCount)
    {
        int OrigNumOfEnvrn{state.dataWeatherManager->NumOfEnvrn};

        for (int i = 1; i <= OrigNumOfEnvrn; ++i) {
            if (state.dataWeatherManager->Environment(i).KindOfEnvrn == DataGlobalConstants::KindOfSim::DesignDay) {
                state.dataWeatherManager->Environment.redimension(++state.dataWeatherManager->NumOfEnvrn);
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn) =
                    state.dataWeatherManager->Environment(i); // copy over seed data from current array element
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).SeedEnvrnNum = i;
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).KindOfEnvrn =
                    DataGlobalConstants::KindOfSim::HVACSizeDesignDay;
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).Title =
                    format("{} HVAC Sizing Pass {}", state.dataWeatherManager->Environment(i).Title, HVACSizingIterCount);
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).HVACSizingIterationNum = HVACSizingIterCount;
            } else if (state.dataWeatherManager->Environment(i).KindOfEnvrn == DataGlobalConstants::KindOfSim::RunPeriodDesign) {
                state.dataWeatherManager->Environment.redimension(++state.dataWeatherManager->NumOfEnvrn);
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn) =
                    state.dataWeatherManager->Environment(i); // copy over seed data
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).SeedEnvrnNum = i;
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).KindOfEnvrn =
                    DataGlobalConstants::KindOfSim::HVACSizeRunPeriodDesign;
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).Title =
                    format("{} HVAC Sizing Pass {}", state.dataWeatherManager->Environment(i).Title, HVACSizingIterCount);
                state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).HVACSizingIterationNum = HVACSizingIterCount;
            }
        } // for each loop over Environment data strucure
    }

    void SetupWeekDaysByMonth(EnergyPlusData &state, int const StMon, int const StDay, int const StWeekDay, Array1D_int &WeekDays)
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
        EP_SIZE_CHECK(WeekDays, 12); // NOLINT(misc-static-assert)

        // Set 1st day of Start Month
        int CurWeekDay{StWeekDay};
        for (int i = 1; i <= StDay - 1; ++i) {
            --CurWeekDay;
            if (CurWeekDay == 0) CurWeekDay = 7;
        }

        WeekDays(StMon) = CurWeekDay;
        for (int i = StMon + 1; i <= 12; ++i) {

            if (i == 2) {
                CurWeekDay += state.dataWeatherManager->EndDayOfMonth(1);
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(i) = CurWeekDay;
            } else if (i == 3) {
                CurWeekDay += state.dataWeatherManager->EndDayOfMonth(i - 1) + state.dataWeatherManager->LeapYearAdd;
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(i) = CurWeekDay;
            } else if ((i >= 4) && (i <= 12)) {
                CurWeekDay += state.dataWeatherManager->EndDayOfMonth(i - 1);
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(i) = CurWeekDay;
            }
        }

        if (any_eq(WeekDays, 0)) {
            // need to start at StMon and go backwards.
            // EndDayOfMonth is also "days" in month.  (without leapyear day in February)
            CurWeekDay = StWeekDay;
            for (int i = 1; i <= StDay - 1; ++i) {
                --CurWeekDay;
                if (CurWeekDay == 0) CurWeekDay = 7;
            }

            for (int i = StMon - 1; i >= 1; --i) {

                if (i == 1) {
                    CurWeekDay -= state.dataWeatherManager->EndDayOfMonth(1);
                    while (CurWeekDay <= 0) {
                        CurWeekDay += 7;
                    }
                    WeekDays(i) = CurWeekDay;
                } else if (i == 2) {
                    CurWeekDay = CurWeekDay - state.dataWeatherManager->EndDayOfMonth(2) + state.dataWeatherManager->LeapYearAdd;
                    while (CurWeekDay <= 0) {
                        CurWeekDay += 7;
                    }
                    WeekDays(i) = CurWeekDay;
                } else if ((i >= 3) && (i <= 12)) {
                    CurWeekDay -= state.dataWeatherManager->EndDayOfMonth(i);
                    while (CurWeekDay <= 0) {
                        CurWeekDay += 7;
                    }
                    WeekDays(i) = CurWeekDay;
                }
            }
        }
    }
#pragma clang diagnostic pop

    void ResetWeekDaysByMonth(EnergyPlusData &state,
                              Array1D_int &WeekDays,
                              int const AddLeapYear,
                              int const StartMonth,
                              int const StartMonthDay,
                              int const EndMonth,
                              int const EndMonthDay,
                              bool const Rollover,
                              bool const MidSimReset)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine resets the weekday for each month based on the current weekday
        // and previous weekdays per month.

        EP_SIZE_CHECK(WeekDays, 12); // NOLINT(misc-static-assert)

        Array1D_int WeekDaysCopy(12);
        int CurWeekDay;

        WeekDaysCopy = WeekDays;
        if (!MidSimReset) {
            if (Rollover) {
                if (StartMonth == 1) {
                    CurWeekDay = WeekDays(12) + state.dataWeatherManager->EndDayOfMonth(12) + StartMonthDay - 1;
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
            for (int i = StartMonth + 1; i <= 12; ++i) {
                if (i == 2) {
                    CurWeekDay += state.dataWeatherManager->EndDayOfMonth(1);
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(i) = CurWeekDay;
                } else if (i == 3) {
                    CurWeekDay += state.dataWeatherManager->EndDayOfMonth(i - 1) + AddLeapYear;
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(i) = CurWeekDay;
                } else if ((i >= 4) && (i <= 12)) {
                    CurWeekDay += state.dataWeatherManager->EndDayOfMonth(i - 1);
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(i) = CurWeekDay;
                }
            }

            if (any_eq(WeekDays, 0)) {
                // need to start at StMon and go backwards.
                // EndDayOfMonth is also "days" in month.  (without leapyear day in February)
                CurWeekDay = WeekDays(StartMonth);
                for (int i = 1; i <= StartMonthDay - 1; ++i) {
                    --CurWeekDay;
                    if (CurWeekDay == 0) CurWeekDay = 7;
                }

                for (int i = StartMonth - 1; i >= 1; --i) {

                    if (i == 1) {
                        CurWeekDay -= state.dataWeatherManager->EndDayOfMonth(1);
                        while (CurWeekDay <= 0) {
                            CurWeekDay += 7;
                        }
                        WeekDays(i) = CurWeekDay;
                    } else if (i == 2) {
                        CurWeekDay = CurWeekDay - state.dataWeatherManager->EndDayOfMonth(2) + AddLeapYear;
                        while (CurWeekDay <= 0) {
                            CurWeekDay += 7;
                        }
                        WeekDays(i) = CurWeekDay;
                    } else if ((i >= 3) && (i <= 12)) {
                        CurWeekDay -= state.dataWeatherManager->EndDayOfMonth(i);
                        while (CurWeekDay <= 0) {
                            CurWeekDay += 7;
                        }
                        WeekDays(i) = CurWeekDay;
                    }
                }
            }

        } else {
            if (Rollover) {
                if (StartMonth == 1) {
                    CurWeekDay = WeekDays(12) + state.dataWeatherManager->EndDayOfMonth(12) + StartMonthDay - 1;
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
                CurWeekDay = WeekDaysCopy(12) + state.dataWeatherManager->EndDayOfMonth(12);
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(1) = CurWeekDay;
                CurWeekDay += state.dataWeatherManager->EndDayOfMonth(1);
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(2) = CurWeekDay;
                CurWeekDay += state.dataWeatherManager->EndDayOfMonth(2) + AddLeapYear;
                while (CurWeekDay > 7) {
                    CurWeekDay -= 7;
                }
                WeekDays(3) = CurWeekDay;
                for (int i = 4; i <= 12; ++i) {
                    CurWeekDay += state.dataWeatherManager->EndDayOfMonth(i - 1);
                    while (CurWeekDay > 7) {
                        CurWeekDay -= 7;
                    }
                    WeekDays(i) = CurWeekDay;
                }
            } else {
                WeekDays = 0;
                WeekDays(StartMonth) = CurWeekDay;
                for (int i = StartMonth + 1; i <= 12; ++i) {
                    if (i == 2) {
                        CurWeekDay += state.dataWeatherManager->EndDayOfMonth(1);
                        while (CurWeekDay > 7) {
                            CurWeekDay -= 7;
                        }
                        WeekDays(i) = CurWeekDay;
                    } else if (i == 3) {
                        CurWeekDay += state.dataWeatherManager->EndDayOfMonth(i - 1) + AddLeapYear;
                        while (CurWeekDay > 7) {
                            CurWeekDay -= 7;
                        }
                        WeekDays(i) = CurWeekDay;
                    } else if ((i >= 4) && (i <= 12)) {
                        CurWeekDay += state.dataWeatherManager->EndDayOfMonth(i - 1);
                        while (CurWeekDay > 7) {
                            CurWeekDay -= 7;
                        }
                        WeekDays(i) = CurWeekDay;
                    }
                }

                if (any_eq(WeekDays, 0)) {
                    // need to start at StMon and go backwards.
                    // EndDayOfMonth is also "days" in month.  (without leapyear day in February)
                    CurWeekDay = WeekDays(StartMonth);
                    for (int i = 1; i <= StartMonthDay - 1; ++i) {
                        --CurWeekDay;
                        if (CurWeekDay == 0) CurWeekDay = 7;
                    }

                    for (int i = StartMonth - 1; i >= 1; --i) {

                        if (i == 1) {
                            CurWeekDay -= state.dataWeatherManager->EndDayOfMonth(1);
                            while (CurWeekDay <= 0) {
                                CurWeekDay += 7;
                            }
                            WeekDays(i) = CurWeekDay;
                        } else if (i == 2) {
                            CurWeekDay = CurWeekDay - state.dataWeatherManager->EndDayOfMonth(2) + AddLeapYear;
                            while (CurWeekDay <= 0) {
                                CurWeekDay += 7;
                            }
                            WeekDays(i) = CurWeekDay;
                        } else if ((i >= 3) && (i <= 12)) {
                            CurWeekDay -= state.dataWeatherManager->EndDayOfMonth(i);
                            while (CurWeekDay <= 0) {
                                CurWeekDay += 7;
                            }
                            WeekDays(i) = CurWeekDay;
                        }
                    }
                }
            }
        }
    }

    void SetDSTDateRanges(EnergyPlusData &state,
                          Array1D_int const &MonWeekDay, // Weekday of each day 1 of month
                          Array1D_int &DSTIdx,           // DST Index for each julian day (1:366)
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

        static constexpr std::string_view RoutineName("SetDSTDateRanges: ");

        int ActStartMonth; // Actual Start Month
        int ActStartDay;   // Actual Start Day of Month
        int ActEndMonth;   // Actual End Month
        int ActEndDay;     // Actual End Day of Month
        Array1D_int ActEndDayOfMonth(12);

        bool ErrorsFound = false;
        ActEndDayOfMonth = state.dataWeatherManager->EndDayOfMonth;
        ActEndDayOfMonth(2) = state.dataWeatherManager->EndDayOfMonth(2) + state.dataWeatherManager->LeapYearAdd;
        if (state.dataWeatherManager->DST.StDateType == DateType::MonthDay) {
            ActStartMonth = state.dataWeatherManager->DST.StMon;
            ActStartDay = state.dataWeatherManager->DST.StDay;
        } else if (state.dataWeatherManager->DST.StDateType == DateType::NthDayInMonth) {
            int ThisDay = state.dataWeatherManager->DST.StWeekDay - MonWeekDay(state.dataWeatherManager->DST.StMon) + 1;
            while (ThisDay <= 0) {
                ThisDay += 7;
            }
            ThisDay += 7 * (state.dataWeatherManager->DST.StDay - 1);
            if (ThisDay > ActEndDayOfMonth(state.dataWeatherManager->DST.StMon)) {
                ShowSevereError(state, std::string{RoutineName} + "Determining DST: DST Start Date, Nth Day of Month, not enough Nths");
                ErrorsFound = true;
            } else {
                ActStartMonth = state.dataWeatherManager->DST.StMon;
                ActStartDay = ThisDay;
            }
        } else { // LastWeekDayInMonth
            int ThisDay = state.dataWeatherManager->DST.StWeekDay - MonWeekDay(state.dataWeatherManager->DST.StMon) + 1;
            while (ThisDay + 7 <= ActEndDayOfMonth(state.dataWeatherManager->DST.StMon)) {
                ThisDay += 7;
            }
            ActStartMonth = state.dataWeatherManager->DST.StMon;
            ActStartDay = ThisDay;
        }

        if (state.dataWeatherManager->DST.EnDateType == DateType::MonthDay) {
            ActEndMonth = state.dataWeatherManager->DST.EnMon;
            ActEndDay = state.dataWeatherManager->DST.EnDay;
        } else if (state.dataWeatherManager->DST.EnDateType == DateType::NthDayInMonth) {
            int ThisDay = state.dataWeatherManager->DST.EnWeekDay - MonWeekDay(state.dataWeatherManager->DST.EnMon) + 1;
            while (ThisDay <= 0) {
                ThisDay += 7;
            }
            ThisDay += 7 * (state.dataWeatherManager->DST.EnDay - 1);
            if (ThisDay > ActEndDayOfMonth(state.dataWeatherManager->DST.EnMon)) {
                ActEndMonth = 0; // Suppress uninitialized warning
                ActEndDay = 0;   // Suppress uninitialized warning
                ShowSevereError(state, std::string{RoutineName} + "Determining DST: DST End Date, Nth Day of Month, not enough Nths");
                ErrorsFound = true;
            } else {
                ActEndMonth = state.dataWeatherManager->DST.EnMon;
                ActEndDay = ThisDay;
            }
        } else { // LastWeekDayInMonth
            int ThisDay = state.dataWeatherManager->DST.EnWeekDay - MonWeekDay(state.dataWeatherManager->DST.EnMon) + 1;
            while (ThisDay + 7 <= ActEndDayOfMonth(state.dataWeatherManager->DST.EnMon)) {
                ThisDay += 7;
            }
            ActEndMonth = state.dataWeatherManager->DST.EnMon;
            ActEndDay = ThisDay;
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Program terminates due to preceding condition(s).");
        }

        if (present(DSTActStMon)) {
            DSTActStMon = ActStartMonth;
            DSTActStDay = ActStartDay;
            DSTActEnMon = ActEndMonth;
            DSTActEnDay = ActEndDay;
        }

        DSTIdx = 0;
        int JDay = General::OrdinalDay(ActStartMonth, ActStartDay, state.dataWeatherManager->LeapYearAdd);
        int JDay1 = General::OrdinalDay(ActEndMonth, ActEndDay, state.dataWeatherManager->LeapYearAdd);
        if (JDay1 >= JDay) {
            DSTIdx({JDay, JDay1}) = 1;
        } else {
            DSTIdx({JDay, 366}) = 1;
            DSTIdx({1, JDay1}) = 1;
        }
    }

    void SetSpecialDayDates(EnergyPlusData &state, Array1D_int const &MonWeekDay) // Weekday of each day 1 of month
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

        static constexpr std::string_view RoutineName("SetSpecialDayDates: ");

        int JDay;
        Array1D_int ActEndDayOfMonth(12);

        bool ErrorsFound = false;
        ActEndDayOfMonth = state.dataWeatherManager->EndDayOfMonth;
        ActEndDayOfMonth(2) = state.dataWeatherManager->EndDayOfMonth(2) + state.dataWeatherManager->LeapYearAdd;
        state.dataWeatherManager->SpecialDayTypes = 0;
        for (int i = 1; i <= state.dataWeatherManager->NumSpecialDays; ++i) {
            if (state.dataWeatherManager->SpecialDays(i).WthrFile && !state.dataWeatherManager->UseSpecialDays) continue;
            if (state.dataWeatherManager->SpecialDays(i).DateType <= DateType::MonthDay) {
                JDay = General::OrdinalDay(state.dataWeatherManager->SpecialDays(i).Month,
                                           state.dataWeatherManager->SpecialDays(i).Day,
                                           state.dataWeatherManager->LeapYearAdd);
                if (state.dataWeatherManager->SpecialDays(i).Duration == 1 &&
                    state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ApplyWeekendRule) {
                    if (state.dataWeatherManager->WeekDayTypes(JDay) == 1) {
                        // Sunday, must go to Monday
                        ++JDay;
                        if (JDay == 366 && state.dataWeatherManager->LeapYearAdd == 0) JDay = 1;
                    } else if (state.dataWeatherManager->WeekDayTypes(JDay) == 7) {
                        ++JDay;
                        if (JDay == 366 && state.dataWeatherManager->LeapYearAdd == 0) JDay = 1;
                        ++JDay;
                        if (JDay == 366 && state.dataWeatherManager->LeapYearAdd == 0) JDay = 1;
                    }
                }
                General::InvOrdinalDay(JDay,
                                       state.dataWeatherManager->SpecialDays(i).ActStMon,
                                       state.dataWeatherManager->SpecialDays(i).ActStDay,
                                       state.dataWeatherManager->LeapYearAdd);
            } else if (state.dataWeatherManager->SpecialDays(i).DateType == DateType::NthDayInMonth) {
                int ThisDay = state.dataWeatherManager->SpecialDays(i).WeekDay - MonWeekDay(state.dataWeatherManager->SpecialDays(i).Month) + 1;
                if (state.dataWeatherManager->SpecialDays(i).WeekDay < MonWeekDay(state.dataWeatherManager->SpecialDays(i).Month)) {
                    ThisDay += 7;
                }
                ThisDay += 7 * (state.dataWeatherManager->SpecialDays(i).Day - 1);
                if (ThisDay > ActEndDayOfMonth(state.dataWeatherManager->SpecialDays(i).Month)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "Special Day Date, Nth Day of Month, not enough Nths, for SpecialDay=" +
                                        state.dataWeatherManager->SpecialDays(i).Name);
                    ErrorsFound = true;
                    continue;
                }
                state.dataWeatherManager->SpecialDays(i).ActStMon = state.dataWeatherManager->SpecialDays(i).Month;
                state.dataWeatherManager->SpecialDays(i).ActStDay = ThisDay;
                JDay = General::OrdinalDay(state.dataWeatherManager->SpecialDays(i).Month, ThisDay, state.dataWeatherManager->LeapYearAdd);
            } else { // LastWeekDayInMonth
                int ThisDay = state.dataWeatherManager->SpecialDays(i).WeekDay - MonWeekDay(state.dataWeatherManager->SpecialDays(i).Month) + 1;
                while (ThisDay + 7 <= ActEndDayOfMonth(state.dataWeatherManager->SpecialDays(i).Month)) {
                    ThisDay += 7;
                }
                state.dataWeatherManager->SpecialDays(i).ActStMon = state.dataWeatherManager->SpecialDays(i).Month;
                state.dataWeatherManager->SpecialDays(i).ActStDay = ThisDay;
                JDay = General::OrdinalDay(state.dataWeatherManager->SpecialDays(i).Month, ThisDay, state.dataWeatherManager->LeapYearAdd);
            }
            if (state.dataWeatherManager->SpecialDayTypes(JDay) != 0) {
                ShowWarningError(state,
                                 std::string{RoutineName} + "Special Day definition (" + state.dataWeatherManager->SpecialDays(i).Name +
                                     ") is overwriting previously entered special day period");
                if (state.dataWeatherManager->UseSpecialDays) {
                    ShowContinueError(state, "...This could be caused by definitions on the Weather File.");
                }
                ShowContinueError(state, "...This could be caused by duplicate definitions in the Input File.");
            }
            int JDay1 = JDay - 1;
            for (int j = 0; j <= state.dataWeatherManager->SpecialDays(i).Duration - 1; ++j) {
                ++JDay1;
                if (JDay1 == 366 && state.dataWeatherManager->LeapYearAdd == 0) JDay1 = 1;
                if (JDay1 == 367) JDay1 = 1;
                state.dataWeatherManager->SpecialDayTypes(JDay1) = state.dataWeatherManager->SpecialDays(i).DayType;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Program terminates due to preceding condition(s).");
        }
    }

    void InitializeWeather(EnergyPlusData &state, bool &printEnvrnStamp) // Set to true when the environment header should be printed
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

        if (state.dataGlobal->BeginSimFlag && state.dataWeatherManager->FirstCall) {

            state.dataWeatherManager->FirstCall = false;
            state.dataEnvrn->EndMonthFlag = false;

        } // ... end of DataGlobals::BeginSimFlag IF-THEN block.

        if (state.dataGlobal->BeginEnvrnFlag) {

            // Call and setup the Design Day environment
            if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn !=
                DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum > 0) {
                    SetUpDesignDay(state, state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum);
                    state.dataEnvrn->EnvironmentName = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).Title;
                }
            }

            // Only used in Weather file environments
            // Start over missing values with each environment
            state.dataWeatherManager->Missing.StnPres = state.dataEnvrn->StdBaroPress; // Initial "missing" value
            state.dataWeatherManager->Missing.DryBulb = 6.0;                           // Initial "missing" value
            state.dataWeatherManager->Missing.DewPoint = 3.0;                          // Initial "missing" value
            state.dataWeatherManager->Missing.RelHumid = 50.0;                         // Initial "missing" value
            state.dataWeatherManager->Missing.WindSpd = 2.5;                           // Initial "missing" value
            state.dataWeatherManager->Missing.WindDir = 180;                           // Initial "missing" value
            state.dataWeatherManager->Missing.TotSkyCvr = 5;                           // Initial "missing" value
            state.dataWeatherManager->Missing.OpaqSkyCvr = 5;                          // Initial "missing" value
            state.dataWeatherManager->Missing.Visibility = 777.7;                      // Initial "missing" value
            state.dataWeatherManager->Missing.Ceiling = 77777;                         // Initial "missing" value
            state.dataWeatherManager->Missing.PrecipWater = 0;                         // Initial "missing" value
            state.dataWeatherManager->Missing.AerOptDepth = 0.0;                       // Initial "missing" value
            state.dataWeatherManager->Missing.SnowDepth = 0;                           // Initial "missing" value
            state.dataWeatherManager->Missing.DaysLastSnow = 88;                       // Initial "missing" value
            state.dataWeatherManager->Missing.Albedo = 0.0;                            // Initial "missing" value
            state.dataWeatherManager->Missing.LiquidPrecip = 0.0;                      // Initial "missing" value
            // Counts set to 0 for each environment
            state.dataWeatherManager->Missed.StnPres = 0;
            state.dataWeatherManager->Missed.DryBulb = 0;
            state.dataWeatherManager->Missed.DewPoint = 0;
            state.dataWeatherManager->Missed.RelHumid = 0;
            state.dataWeatherManager->Missed.WindSpd = 0;
            state.dataWeatherManager->Missed.WindDir = 0;
            state.dataWeatherManager->Missed.TotSkyCvr = 0;
            state.dataWeatherManager->Missed.OpaqSkyCvr = 0;
            state.dataWeatherManager->Missed.Visibility = 0;
            state.dataWeatherManager->Missed.Ceiling = 0;
            state.dataWeatherManager->Missed.PrecipWater = 0;
            state.dataWeatherManager->Missed.AerOptDepth = 0;
            state.dataWeatherManager->Missed.SnowDepth = 0;
            state.dataWeatherManager->Missed.DaysLastSnow = 0;
            state.dataWeatherManager->Missed.Albedo = 0;
            state.dataWeatherManager->Missed.LiquidPrecip = 0;
            state.dataWeatherManager->Missed.WeathCodes = 0;
            state.dataWeatherManager->Missed.DirectRad = 0;
            state.dataWeatherManager->Missed.DiffuseRad = 0;
            // Counts set to 0 for each environment
            state.dataWeatherManager->OutOfRange.StnPres = 0;
            state.dataWeatherManager->OutOfRange.DryBulb = 0;
            state.dataWeatherManager->OutOfRange.DewPoint = 0;
            state.dataWeatherManager->OutOfRange.RelHumid = 0;
            state.dataWeatherManager->OutOfRange.WindSpd = 0;
            state.dataWeatherManager->OutOfRange.WindDir = 0;
            state.dataWeatherManager->OutOfRange.DirectRad = 0;
            state.dataWeatherManager->OutOfRange.DiffuseRad = 0;
            state.dataWeatherManager->IsRainThreshold = 0.8 / double(state.dataGlobal->NumOfTimeStepInHour); // [mm]

            if (!state.dataWeatherManager->RPReadAllWeatherData) {
                printEnvrnStamp = true; // Set this to true so that on first non-warmup day (only) the environment header will print out
            }

            for (int i = 1; i <= state.dataWeatherManager->NumSpecialDays; ++i) {
                state.dataWeatherManager->SpecialDays(i).Used = false;
            }

            if ((state.dataGlobal->KindOfSim != DataGlobalConstants::KindOfSim::DesignDay) &&
                (state.dataGlobal->KindOfSim != DataGlobalConstants::KindOfSim::HVACSizeDesignDay)) {
                ReadWeatherForDay(state, 1, state.dataWeatherManager->Envrn, false); // Read first day's weather
            } else {
                state.dataWeatherManager->TomorrowVariables =
                    state.dataWeatherManager->DesignDay(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum);
            }

        } // ... end of DataGlobals::BeginEnvrnFlag IF-THEN block.

        if (state.dataGlobal->BeginDayFlag) {

            // Check Holidays, Daylight Saving Time, Ground Temperatures, etc.

            UpdateWeatherData(state); // Update daily weather info

            // Read tomorrow's weather only if necessary.  This means that the
            // simulation is out of warmup, is using a weather tape for this
            // environment, and is not on the last day (day after last day is
            // assumed to be equal to last day).

            // Following code checks whether the present day of simulation matches the start month and start day.
            // In a multi year simulation with run period less than 365, we need to position the weather line
            // appropriately.

            if ((!state.dataGlobal->WarmupFlag) &&
                ((state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn != DataGlobalConstants::KindOfSim::DesignDay) &&
                 (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn !=
                  DataGlobalConstants::KindOfSim::HVACSizeDesignDay))) {
                if (state.dataGlobal->DayOfSim < state.dataGlobal->NumOfDayInEnvrn) {
                    if (state.dataGlobal->DayOfSim == state.dataWeatherManager->curSimDayForEndOfRunPeriod) {
                        state.dataWeatherManager->curSimDayForEndOfRunPeriod +=
                            state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).RawSimDays;
                        if (state.dataWeatherManager->StartDatesCycleShouldBeReset) {
                            ResetWeekDaysByMonth(state,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                                 state.dataWeatherManager->LeapYearAdd,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).RollDayTypeOnRepeat);
                            if (state.dataWeatherManager->DaylightSavingIsActive) {
                                SetDSTDateRanges(state,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                                 state.dataWeatherManager->DSTIndex);
                            }
                            SetSpecialDayDates(state, state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay);
                        }
                        ++state.dataWeatherManager->YearOfSim;
                        ReadWeatherForDay(state, 1, state.dataWeatherManager->Envrn, false); // Read tomorrow's weather
                    } else {
                        ReadWeatherForDay(state, state.dataGlobal->DayOfSim + 1, state.dataWeatherManager->Envrn, false); // Read tomorrow's weather
                    }
                }
            }

            state.dataEnvrn->EndYearFlag = false;
            if (state.dataEnvrn->DayOfMonth == state.dataWeatherManager->EndDayOfMonth(state.dataEnvrn->Month)) {
                state.dataEnvrn->EndMonthFlag = true;
                state.dataEnvrn->EndYearFlag = (state.dataEnvrn->Month == 12);
            }

            // Set Tomorrow's date data
            state.dataEnvrn->MonthTomorrow = state.dataWeatherManager->TomorrowVariables.Month;
            state.dataEnvrn->DayOfMonthTomorrow = state.dataWeatherManager->TomorrowVariables.DayOfMonth;
            state.dataEnvrn->DayOfWeekTomorrow = state.dataWeatherManager->TomorrowVariables.DayOfWeek;
            state.dataEnvrn->HolidayIndexTomorrow = state.dataWeatherManager->TomorrowVariables.HolidayIndex;
            state.dataEnvrn->YearTomorrow = state.dataWeatherManager->TomorrowVariables.Year;

            if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn ==
                DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                if (state.dataEnvrn->Month == 1 && state.dataEnvrn->DayOfMonth == 1 &&
                    state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ActualWeather) {
                    if (state.dataWeatherManager->DatesShouldBeReset) {
                        if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).TreatYearsAsConsecutive) {
                            ++state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).CurrentYear;
                            state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).IsLeapYear =
                                isLeapYear(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).CurrentYear);
                            state.dataEnvrn->CurrentYearIsLeapYear =
                                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).IsLeapYear;
                            if (state.dataEnvrn->CurrentYearIsLeapYear) {
                                if (state.dataWeatherManager->WFAllowsLeapYears) {
                                    state.dataWeatherManager->LeapYearAdd = 1;
                                } else {
                                    state.dataWeatherManager->LeapYearAdd = 0;
                                }
                            } else {
                                state.dataWeatherManager->LeapYearAdd = 0;
                            }
                            // need to reset MonWeekDay and WeekDayTypes
                            int JDay5Start = General::OrdinalDay(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay,
                                                                 state.dataWeatherManager->LeapYearAdd);
                            int JDay5End = General::OrdinalDay(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                                               state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay,
                                                               state.dataWeatherManager->LeapYearAdd);
                            if (!state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).ActualWeather)
                                state.dataWeatherManager->curSimDayForEndOfRunPeriod =
                                    state.dataGlobal->DayOfSim + state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).RawSimDays +
                                    state.dataWeatherManager->LeapYearAdd - 1;

                            {
                                int i = JDay5Start;
                                int TWeekDay = state.dataEnvrn->DayOfWeek;
                                while (true) {
                                    state.dataWeatherManager->WeekDayTypes(i) = TWeekDay;
                                    TWeekDay = mod(TWeekDay, 7) + 1;
                                    ++i;
                                    if (i > 366) i = 1;
                                    if (i == JDay5End) break;
                                }
                            }
                            ResetWeekDaysByMonth(state,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                                 state.dataWeatherManager->LeapYearAdd,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).RollDayTypeOnRepeat);
                            if (state.dataWeatherManager->DaylightSavingIsActive) {
                                SetDSTDateRanges(state,
                                                 state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                                 state.dataWeatherManager->DSTIndex);
                            }
                            SetSpecialDayDates(state, state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay);
                        }
                    }
                } else if ((state.dataEnvrn->Month == 1 && state.dataEnvrn->DayOfMonth == 1) && state.dataWeatherManager->DatesShouldBeReset &&
                           (state.dataWeatherManager->Jan1DatesShouldBeReset)) {
                    if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).TreatYearsAsConsecutive) {
                        ++state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).CurrentYear;
                        state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).IsLeapYear =
                            isLeapYear(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).CurrentYear);
                        state.dataEnvrn->CurrentYearIsLeapYear = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).IsLeapYear;
                        if (state.dataEnvrn->CurrentYearIsLeapYear && !state.dataWeatherManager->WFAllowsLeapYears)
                            state.dataEnvrn->CurrentYearIsLeapYear = false;
                        if (state.dataGlobal->DayOfSim < state.dataWeatherManager->curSimDayForEndOfRunPeriod &&
                            state.dataEnvrn->CurrentYearIsLeapYear)
                            ++state.dataWeatherManager->curSimDayForEndOfRunPeriod;
                    }
                    if (state.dataEnvrn->CurrentYearIsLeapYear) {
                        if (state.dataWeatherManager->WFAllowsLeapYears) {
                            state.dataWeatherManager->LeapYearAdd = 1;
                        } else {
                            state.dataWeatherManager->LeapYearAdd = 0;
                        }
                    } else {
                        state.dataWeatherManager->LeapYearAdd = 0;
                    }

                    if (state.dataGlobal->DayOfSim < state.dataWeatherManager->curSimDayForEndOfRunPeriod) {
                        ResetWeekDaysByMonth(state,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                             state.dataWeatherManager->LeapYearAdd,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndMonth,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).EndDay,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).RollDayTypeOnRepeat,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).RollDayTypeOnRepeat ||
                                                 state.dataEnvrn->CurrentYearIsLeapYear);
                        if (state.dataWeatherManager->DaylightSavingIsActive) {
                            SetDSTDateRanges(state,
                                             state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                             state.dataWeatherManager->DSTIndex);
                        }
                        SetSpecialDayDates(state, state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay);
                    }
                }
            }
        } // ... end of DataGlobals::BeginDayFlag IF-THEN block.

        if (!state.dataGlobal->BeginDayFlag && !state.dataGlobal->WarmupFlag &&
            (state.dataEnvrn->Month != state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartMonth ||
             state.dataEnvrn->DayOfMonth != state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).StartDay) &&
            !state.dataWeatherManager->DatesShouldBeReset &&
            state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
            state.dataWeatherManager->DatesShouldBeReset = true;
        }

        if (state.dataGlobal->EndEnvrnFlag &&
            (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn != DataGlobalConstants::KindOfSim::DesignDay) &&
            (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn !=
             DataGlobalConstants::KindOfSim::HVACSizeDesignDay)) {
            state.files.inputWeatherFile.rewind();
            SkipEPlusWFHeader(state);
            ReportMissing_RangeData(state);
        }

        // set the EndDesignDayEnvrnsFlag (dataGlobal)
        // True at the end of the last design day environment (last time step of last hour of last day of environ which is a design day)
        state.dataGlobal->EndDesignDayEnvrnsFlag = false;
        if (state.dataGlobal->EndEnvrnFlag) {
            if (state.dataWeatherManager->Envrn < state.dataWeatherManager->NumOfEnvrn) {
                if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).KindOfEnvrn !=
                    state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn + 1).KindOfEnvrn) {
                    state.dataGlobal->EndDesignDayEnvrnsFlag = true;
                }
            } else {
                // if the last environment set the flag to true.
                state.dataGlobal->EndDesignDayEnvrnsFlag = true;
            }
        }

        if (state.dataWeatherManager->WaterMainsParameterReport) {
            // this is done only once
            if (state.dataWeatherManager->WaterMainsTempsMethod == WaterMainsTempCalcMethod::CorrelationFromWeatherFile) {
                if (!state.dataWeatherManager->OADryBulbAverage.OADryBulbWeatherDataProcessed) {
                    state.dataWeatherManager->OADryBulbAverage.CalcAnnualAndMonthlyDryBulbTemp(state);
                }
            }
            // reports to eio file
            ReportWaterMainsTempParameters(state);
            state.dataWeatherManager->WaterMainsParameterReport = false;
        }
    }

    void UpdateWeatherData(EnergyPlusData &state)
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

        state.dataWeatherManager->TodayVariables =
            state.dataWeatherManager->TomorrowVariables; // Transfer Tomorrow's Daily Weather Variables to Today

        if (state.dataGlobal->BeginEnvrnFlag) {
            state.dataGlobal->PreviousHour = 24;
        }

        state.dataWeatherManager->TodayIsRain = state.dataWeatherManager->TomorrowIsRain;
        state.dataWeatherManager->TodayIsSnow = state.dataWeatherManager->TomorrowIsSnow;
        state.dataWeatherManager->TodayOutDryBulbTemp = state.dataWeatherManager->TomorrowOutDryBulbTemp;
        state.dataWeatherManager->TodayOutDewPointTemp = state.dataWeatherManager->TomorrowOutDewPointTemp;
        state.dataWeatherManager->TodayOutBaroPress = state.dataWeatherManager->TomorrowOutBaroPress;
        state.dataWeatherManager->TodayOutRelHum = state.dataWeatherManager->TomorrowOutRelHum;
        state.dataWeatherManager->TodayWindSpeed = state.dataWeatherManager->TomorrowWindSpeed;
        state.dataWeatherManager->TodayWindDir = state.dataWeatherManager->TomorrowWindDir;
        state.dataWeatherManager->TodaySkyTemp = state.dataWeatherManager->TomorrowSkyTemp;
        state.dataWeatherManager->TodayHorizIRSky = state.dataWeatherManager->TomorrowHorizIRSky;
        state.dataWeatherManager->TodayBeamSolarRad = state.dataWeatherManager->TomorrowBeamSolarRad;
        state.dataWeatherManager->TodayDifSolarRad = state.dataWeatherManager->TomorrowDifSolarRad;
        state.dataWeatherManager->TodayLiquidPrecip = state.dataWeatherManager->TomorrowLiquidPrecip;
        state.dataWeatherManager->TodayTotalSkyCover = state.dataWeatherManager->TomorrowTotalSkyCover;
        state.dataWeatherManager->TodayOpaqueSkyCover = state.dataWeatherManager->TomorrowOpaqueSkyCover;

        // Update Global Data

        state.dataEnvrn->DayOfYear = state.dataWeatherManager->TodayVariables.DayOfYear;
        state.dataEnvrn->Year = state.dataWeatherManager->TodayVariables.Year;
        state.dataEnvrn->Month = state.dataWeatherManager->TodayVariables.Month;
        state.dataEnvrn->DayOfMonth = state.dataWeatherManager->TodayVariables.DayOfMonth;
        state.dataEnvrn->DayOfWeek = state.dataWeatherManager->TodayVariables.DayOfWeek;
        state.dataEnvrn->HolidayIndex = state.dataWeatherManager->TodayVariables.HolidayIndex;
        if (state.dataEnvrn->HolidayIndex > 0) {
            state.dataWeatherManager->RptDayType = 7 + state.dataEnvrn->HolidayIndex;
        } else {
            state.dataWeatherManager->RptDayType = state.dataEnvrn->DayOfWeek;
        }
        state.dataEnvrn->DSTIndicator = state.dataWeatherManager->TodayVariables.DaylightSavingIndex;
        state.dataEnvrn->EquationOfTime = state.dataWeatherManager->TodayVariables.EquationOfTime;
        state.dataEnvrn->CosSolarDeclinAngle = state.dataWeatherManager->TodayVariables.CosSolarDeclinAngle;
        state.dataEnvrn->SinSolarDeclinAngle = state.dataWeatherManager->TodayVariables.SinSolarDeclinAngle;
    }

    void SetCurrentWeather(EnergyPlusData &state)
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
        // The current hour (DataGlobals::HourOfDay) as well as the next hour are used
        // to come up with environment data per time step interval.  Method
        // used is to assign a weighting for the current hour's data and
        // (1-that weighting) to the next hour's data.  Actual method is:  if
        // the current time step is 15 minutes into hour, the interpolated dry
        // bulb temperature should be 3/4*dry bulb temperature of current hour
        // and 1/4*dry bulb temperature of next environment hourly data.  At
        // day boundary (current hour = 24), the next hour is hour 1 of next
        // weather data day (Tomorrow%).

        static constexpr std::string_view RoutineName("SetCurrentWeather");

        state.dataWeatherManager->NextHour = state.dataGlobal->HourOfDay + 1;

        if (state.dataGlobal->HourOfDay == 24) { // Should investigate whether EndDayFlag is always set here and use that instead
            state.dataWeatherManager->NextHour = 1;
        }

        if (state.dataGlobal->HourOfDay == 1) { // Should investigate whether DataGlobals::BeginDayFlag is always set here and use that instead
            state.dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, 1);
        }

        ScheduleManager::UpdateScheduleValues(state);

        state.dataEnvrn->CurMnDyHr = fmt::format(
            "{:02d}/{:02d} {:02d}", state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, (unsigned short)(state.dataGlobal->HourOfDay - 1));
        state.dataEnvrn->CurMnDy = fmt::format("{:02d}/{:02d}", state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth);
        state.dataEnvrn->CurMnDyYr =
            fmt::format("{:02d}/{:02d}/{:04d}", state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->CalendarYear);

        state.dataGlobal->WeightNow = state.dataWeatherManager->Interpolation(state.dataGlobal->TimeStep);
        state.dataGlobal->WeightPreviousHour = 1.0 - state.dataGlobal->WeightNow;

        state.dataGlobal->CurrentTime = (state.dataGlobal->HourOfDay - 1) + state.dataGlobal->TimeStep * (state.dataWeatherManager->TimeStepFraction);
        state.dataGlobal->SimTimeSteps = (state.dataGlobal->DayOfSim - 1) * 24 * state.dataGlobal->NumOfTimeStepInHour +
                                         (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour + state.dataGlobal->TimeStep;

        state.dataEnvrn->GroundTemp =
            state.dataWeatherManager->siteBuildingSurfaceGroundTempsPtr->getGroundTempAtTimeInMonths(state, 0, state.dataEnvrn->Month);
        state.dataEnvrn->GroundTempKelvin = state.dataEnvrn->GroundTemp + DataGlobalConstants::KelvinConv;
        state.dataEnvrn->GroundTempFC =
            state.dataWeatherManager->siteFCFactorMethodGroundTempsPtr->getGroundTempAtTimeInMonths(state, 0, state.dataEnvrn->Month);
        state.dataEnvrn->GroundTemp_Surface =
            state.dataWeatherManager->siteShallowGroundTempsPtr->getGroundTempAtTimeInMonths(state, 0, state.dataEnvrn->Month);
        state.dataEnvrn->GroundTemp_Deep =
            state.dataWeatherManager->siteDeepGroundTempsPtr->getGroundTempAtTimeInMonths(state, 0, state.dataEnvrn->Month);
        state.dataEnvrn->GndReflectance = state.dataWeatherManager->GroundReflectances(state.dataEnvrn->Month);
        state.dataEnvrn->GndReflectanceForDayltg = state.dataEnvrn->GndReflectance;

        CalcWaterMainsTemp(state);

        // Determine if Sun is up or down, set Solar Cosine values for time step.
        DetermineSunUpDown(state, state.dataEnvrn->SOLCOS);
        if (state.dataEnvrn->SunIsUp && state.dataWeatherManager->SolarAltitudeAngle < 0.0) {
            ShowFatalError(state, "SetCurrentWeather: At " + state.dataEnvrn->CurMnDyHr + " Sun is Up but Solar Altitude Angle is < 0.0");
        }

        state.dataEnvrn->OutDryBulbTemp = state.dataWeatherManager->TodayOutDryBulbTemp(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        if (state.dataEnvrn->EMSOutDryBulbOverrideOn) state.dataEnvrn->OutDryBulbTemp = state.dataEnvrn->EMSOutDryBulbOverrideValue;
        state.dataEnvrn->OutBaroPress = state.dataWeatherManager->TodayOutBaroPress(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        state.dataEnvrn->OutDewPointTemp = state.dataWeatherManager->TodayOutDewPointTemp(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        if (state.dataEnvrn->EMSOutDewPointTempOverrideOn) state.dataEnvrn->OutDewPointTemp = state.dataEnvrn->EMSOutDewPointTempOverrideValue;
        state.dataEnvrn->OutRelHum = state.dataWeatherManager->TodayOutRelHum(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        state.dataEnvrn->OutRelHumValue = state.dataEnvrn->OutRelHum / 100.0;
        if (state.dataEnvrn->EMSOutRelHumOverrideOn) {
            state.dataEnvrn->OutRelHumValue = state.dataEnvrn->EMSOutRelHumOverrideValue / 100.0;
            state.dataEnvrn->OutRelHum = state.dataEnvrn->EMSOutRelHumOverrideValue;
        }

        // Humidity Ratio and Wet Bulb are derived
        state.dataEnvrn->OutHumRat = Psychrometrics::PsyWFnTdbRhPb(
            state, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutRelHumValue, state.dataEnvrn->OutBaroPress, RoutineName);
        state.dataEnvrn->OutWetBulbTemp =
            Psychrometrics::PsyTwbFnTdbWPb(state, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);
        if (state.dataEnvrn->OutDryBulbTemp < state.dataEnvrn->OutWetBulbTemp) {
            state.dataEnvrn->OutWetBulbTemp = state.dataEnvrn->OutDryBulbTemp;
            Real64 TempVal = Psychrometrics::PsyWFnTdbTwbPb(
                state, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutWetBulbTemp, state.dataEnvrn->OutBaroPress);
            state.dataEnvrn->OutDewPointTemp = Psychrometrics::PsyTdpFnWPb(state, TempVal, state.dataEnvrn->OutBaroPress);
        }

        if (state.dataEnvrn->OutDewPointTemp > state.dataEnvrn->OutWetBulbTemp) {
            state.dataEnvrn->OutDewPointTemp = state.dataEnvrn->OutWetBulbTemp;
        }

        if ((state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::DesignDay) ||
            (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay)) {
            state.dataWeatherManager->SPSiteDryBulbRangeModScheduleValue = -999.0;   // N/A Drybulb Temperature Range Modifier Schedule Value
            state.dataWeatherManager->SPSiteHumidityConditionScheduleValue = -999.0; // N/A Humidity Condition Schedule Value
            state.dataWeatherManager->SPSiteBeamSolarScheduleValue = -999.0;         // N/A Beam Solar Schedule Value
            state.dataWeatherManager->SPSiteDiffuseSolarScheduleValue = -999.0;      // N/A Diffuse Solar Schedule Value
            state.dataWeatherManager->SPSiteSkyTemperatureScheduleValue = -999.0;    // N/A SkyTemperature Modifier Schedule Value

            int const envrnDayNum(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).DesignDayNum);
            if (state.dataWeatherManager->DesDayInput(envrnDayNum).DBTempRangeType != DDDBRangeType::Default) {
                state.dataWeatherManager->SPSiteDryBulbRangeModScheduleValue(envrnDayNum) =
                    state.dataWeatherManager->DDDBRngModifier(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, envrnDayNum);
            }
            DDHumIndType const &humIndType{state.dataWeatherManager->DesDayInput(envrnDayNum).HumIndType};
            if (humIndType == DDHumIndType::WBProfDef || humIndType == DDHumIndType::WBProfDif || humIndType == DDHumIndType::WBProfMul ||
                humIndType == DDHumIndType::RelHumSch) {
                state.dataWeatherManager->SPSiteHumidityConditionScheduleValue(envrnDayNum) =
                    state.dataWeatherManager->DDHumIndModifier(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, envrnDayNum);
            }
            if (state.dataWeatherManager->DesDayInput(envrnDayNum).SolarModel == DesignDaySolarModel::SolarModel_Schedule) {
                state.dataWeatherManager->SPSiteBeamSolarScheduleValue(envrnDayNum) =
                    state.dataWeatherManager->DDBeamSolarValues(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, envrnDayNum);
                state.dataWeatherManager->SPSiteDiffuseSolarScheduleValue(envrnDayNum) =
                    state.dataWeatherManager->DDDiffuseSolarValues(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, envrnDayNum);
            }
            if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::ScheduleValue ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::DryBulbDelta ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::DewPointDelta) {
                state.dataWeatherManager->SPSiteSkyTemperatureScheduleValue(envrnDayNum) =
                    state.dataWeatherManager->DDSkyTempScheduleValues(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay, envrnDayNum);
            }
        } else if (state.dataEnvrn->TotDesDays > 0) {
            state.dataWeatherManager->SPSiteDryBulbRangeModScheduleValue = -999.0;   // N/A Drybulb Temperature Range Modifier Schedule Value
            state.dataWeatherManager->SPSiteHumidityConditionScheduleValue = -999.0; // N/A Humidity Condition Schedule Value
            state.dataWeatherManager->SPSiteBeamSolarScheduleValue = -999.0;         // N/A Beam Solar Schedule Value
            state.dataWeatherManager->SPSiteDiffuseSolarScheduleValue = -999.0;      // N/A Diffuse Solar Schedule Value
            state.dataWeatherManager->SPSiteSkyTemperatureScheduleValue = -999.0;    // N/A SkyTemperature Modifier Schedule Value
        }

        state.dataEnvrn->WindSpeed = state.dataWeatherManager->TodayWindSpeed(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        if (state.dataEnvrn->EMSWindSpeedOverrideOn) state.dataEnvrn->WindSpeed = state.dataEnvrn->EMSWindSpeedOverrideValue;
        state.dataEnvrn->WindDir = state.dataWeatherManager->TodayWindDir(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        if (state.dataEnvrn->EMSWindDirOverrideOn) state.dataEnvrn->WindDir = state.dataEnvrn->EMSWindDirOverrideValue;
        state.dataWeatherManager->HorizIRSky = state.dataWeatherManager->TodayHorizIRSky(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        state.dataEnvrn->SkyTemp = state.dataWeatherManager->TodaySkyTemp(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        state.dataEnvrn->SkyTempKelvin = state.dataEnvrn->SkyTemp + DataGlobalConstants::KelvinConv;
        state.dataEnvrn->DifSolarRad = state.dataWeatherManager->TodayDifSolarRad(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        if (state.dataEnvrn->EMSDifSolarRadOverrideOn) state.dataEnvrn->DifSolarRad = state.dataEnvrn->EMSDifSolarRadOverrideValue;
        state.dataEnvrn->BeamSolarRad = state.dataWeatherManager->TodayBeamSolarRad(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        if (state.dataEnvrn->EMSBeamSolarRadOverrideOn) state.dataEnvrn->BeamSolarRad = state.dataEnvrn->EMSBeamSolarRadOverrideValue;
        state.dataEnvrn->LiquidPrecipitation =
            state.dataWeatherManager->TodayLiquidPrecip(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay) / 1000.0; // convert from mm to m
        if ((state.dataEnvrn->RunPeriodEnvironment) && (!state.dataGlobal->WarmupFlag)) {
            int month = state.dataEnvrn->Month;
            state.dataWaterData->RainFall.MonthlyTotalPrecInWeather.at(month - 1) += state.dataEnvrn->LiquidPrecipitation * 1000.0;
            if ((state.dataEnvrn->LiquidPrecipitation > 0) && (state.dataGlobal->TimeStep == 1)) {
                state.dataWaterData->RainFall.numRainyHoursInWeather.at(month - 1) += 1;
            }
        }

        WaterManager::UpdatePrecipitation(state);

        state.dataEnvrn->TotalCloudCover = state.dataWeatherManager->TodayTotalSkyCover(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        state.dataEnvrn->OpaqueCloudCover = state.dataWeatherManager->TodayOpaqueSkyCover(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);

        if (state.dataWeatherManager->UseRainValues) {
            // It is set as LiquidPrecipitation >= .8 mm here: state.dataWeatherManager->TomorrowLiquidPrecip(ts, hour) >=
            // state.dataWeatherManager->IsRainThreshold;
            state.dataEnvrn->IsRain = state.dataWeatherManager->TodayIsRain(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
            if (state.dataWaterData->RainFall.ModeID == DataWater::RainfallMode::RainSchedDesign && state.dataEnvrn->RunPeriodEnvironment) {
                // CurrentAmount unit: m
                state.dataEnvrn->IsRain = state.dataWaterData->RainFall.CurrentAmount >= (state.dataWeatherManager->IsRainThreshold / 1000.0);
            }
        } else {
            state.dataEnvrn->IsRain = false;
        }
        if (state.dataWeatherManager->UseSnowValues) {
            state.dataEnvrn->IsSnow = state.dataWeatherManager->TodayIsSnow(state.dataGlobal->TimeStep, state.dataGlobal->HourOfDay);
        } else {
            state.dataEnvrn->IsSnow = false;
        }

        if (state.dataEnvrn->IsSnow) {
            state.dataEnvrn->GndReflectance = max(min(state.dataEnvrn->GndReflectance * state.dataWeatherManager->SnowGndRefModifier, 1.0), 0.0);
            state.dataEnvrn->GndReflectanceForDayltg =
                max(min(state.dataEnvrn->GndReflectanceForDayltg * state.dataWeatherManager->SnowGndRefModifierForDayltg, 1.0), 0.0);
        }

        state.dataEnvrn->GndSolarRad =
            max((state.dataEnvrn->BeamSolarRad * state.dataEnvrn->SOLCOS(3) + state.dataEnvrn->DifSolarRad) * state.dataEnvrn->GndReflectance, 0.0);

        if (!state.dataEnvrn->SunIsUp) {
            state.dataEnvrn->DifSolarRad = 0.0;
            state.dataEnvrn->BeamSolarRad = 0.0;
            state.dataEnvrn->GndSolarRad = 0.0;
        }

        state.dataEnvrn->OutEnthalpy = Psychrometrics::PsyHFnTdbW(state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);
        state.dataEnvrn->OutAirDensity =
            Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);

        if (state.dataEnvrn->OutDryBulbTemp < state.dataEnvrn->OutWetBulbTemp) state.dataEnvrn->OutWetBulbTemp = state.dataEnvrn->OutDryBulbTemp;
        if (state.dataEnvrn->OutDewPointTemp > state.dataEnvrn->OutWetBulbTemp) state.dataEnvrn->OutDewPointTemp = state.dataEnvrn->OutWetBulbTemp;

        DayltgCurrentExtHorizIllum(state);

        if (!state.dataEnvrn->IsRain) {
            state.dataWeatherManager->RptIsRain = 0;
        } else {
            state.dataWeatherManager->RptIsRain = 1;
        }

        if (!state.dataEnvrn->IsSnow) {
            state.dataWeatherManager->RptIsSnow = 0;
        } else {
            state.dataWeatherManager->RptIsSnow = 1;
        }
    }

    void ReadWeatherForDay(EnergyPlusData &state,
                           int const DayToRead,          // =1 when starting out, otherwise signifies next day
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

        ReadEPlusWeatherForDay(state, DayToRead, Environ, BackSpaceAfterRead);
    }

    void ReadEPlusWeatherForDay(EnergyPlusData &state,
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
            Array1D<Real64> TotalSkyCover;   // Total Sky Cover
            Array1D<Real64> OpaqueSkyCover;  // Opaque Sky Cover

            // Default Constructor
            HourlyWeatherData()
                : IsRain(24, false), IsSnow(24, false), OutDryBulbTemp(24, 0.0), OutDewPointTemp(24, 0.0), OutBaroPress(24, 0.0), OutRelHum(24, 0.0),
                  WindSpeed(24, 0.0), WindDir(24, 0.0), SkyTemp(24, 0.0), HorizIRSky(24, 0.0), BeamSolarRad(24, 0.0), DifSolarRad(24, 0.0),
                  Albedo(24, 0.0), LiquidPrecip(24, 0.0), TotalSkyCover(24, 0.0), OpaqueSkyCover(24, 0.0)
            {
            }
        };

        HourlyWeatherData Wthr;

        if (DayToRead == 1) {

            // Checks whether Weather file contains just one year of data. If yes then rewind and position to first
            // day of weather file. The rest of code appropriately positions to the start day.

            bool Ready = false;
            int NumRewinds = 0;
            //     Must position file to proper day
            //     File already position to first data record
            //          Set Current Day of Week to "start of Data Period"
            state.dataWeatherManager->ReadEPlusWeatherCurTime = 1.0 / double(state.dataWeatherManager->NumIntervalsPerHour);
            state.dataWeatherManager->CurDayOfWeek = state.dataWeatherManager->DataPeriods(1).WeekDay - 1;
            WYear = 0;
            WMonth = 0;
            WDay = 0;
            WHour = 0;
            WMinute = 0;
            state.dataWeatherManager->LastHourSet = false;
            InputFile::ReadResult<std::string> WeatherDataLine{"", true, false};
            while (!Ready) {
                WeatherDataLine.update(state.files.inputWeatherFile.readLine());
                if (WeatherDataLine.good) {
                    bool ErrorFound;
                    InterpretWeatherDataLine(state,
                                             WeatherDataLine.data,
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
                } else if (WeatherDataLine.eof) {
                    if (NumRewinds > 0) {
                        std::string date = fmt::to_string(state.dataWeatherManager->Environment(Environ).StartMonth) + '/' +
                                           fmt::to_string(state.dataWeatherManager->Environment(Environ).StartDay);
                        if (state.dataWeatherManager->Environment(Environ).MatchYear) {
                            date += '/' + fmt::to_string(state.dataWeatherManager->Environment(Environ).StartYear);
                        }
                        ShowSevereError(state, "Multiple rewinds on EPW while searching for first day " + date);
                    } else {
                        state.files.inputWeatherFile.rewind();
                        ++NumRewinds;
                        SkipEPlusWFHeader(state);
                        WeatherDataLine.update(state.files.inputWeatherFile.readLine());
                        bool ErrorFound;
                        InterpretWeatherDataLine(state,
                                                 WeatherDataLine.data,
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
                if (!WeatherDataLine.good) {
                    ShowFatalError(state,
                                   format("Error occurred on EPW while searching for first day, stopped at {}/{}/{} {}:{} IO Error='{}'",
                                          WYear,
                                          WMonth,
                                          WDay,
                                          WHour,
                                          WMinute,
                                          state.files.inputWeatherFile.error_state_to_string()),
                                   OptionalOutputFileRef{state.files.eso});
                }
                if (state.dataWeatherManager->CurDayOfWeek <= 7) {
                    state.dataWeatherManager->CurDayOfWeek = mod(state.dataWeatherManager->CurDayOfWeek, 7) + 1;
                }
                bool RecordDateMatch =
                    (WMonth == state.dataWeatherManager->Environment(Environ).StartMonth &&
                     WDay == state.dataWeatherManager->Environment(Environ).StartDay && !state.dataWeatherManager->Environment(Environ).MatchYear) ||
                    (WMonth == state.dataWeatherManager->Environment(Environ).StartMonth &&
                     WDay == state.dataWeatherManager->Environment(Environ).StartDay && state.dataWeatherManager->Environment(Environ).MatchYear &&
                     WYear == state.dataWeatherManager->Environment(Environ).StartYear);
                if (RecordDateMatch) {
                    state.files.inputWeatherFile.backspace();
                    Ready = true;
                    if (state.dataWeatherManager->CurDayOfWeek <= 7) {
                        --state.dataWeatherManager->CurDayOfWeek;
                    }
                    // Do the range checks on the first set of fields -- no others.
                    bool ErrorsFound = false;
                    if (DryBulb >= 99.9)
                        state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                              ErrorsFound,
                                                                              "DryBulb Temperature",
                                                                              "WeatherFile",
                                                                              "Severe",
                                                                              ">= -90",
                                                                              (DryBulb >= -90.0),
                                                                              "<= 70",
                                                                              (DryBulb <= 70.0),
                                                                              format("{:.2R}", DryBulb),
                                                                              state.dataEnvrn->WeatherFileLocationTitle);
                    if (DewPoint < 99.9)
                        state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                              ErrorsFound,
                                                                              "DewPoint Temperature",
                                                                              "WeatherFile",
                                                                              "Severe",
                                                                              ">= -90",
                                                                              (DewPoint >= -90.0),
                                                                              "<= 70",
                                                                              (DewPoint <= 70.0),
                                                                              format("{:.2R}", DewPoint),
                                                                              state.dataEnvrn->WeatherFileLocationTitle);
                    if (RelHum < 999.0)
                        state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                              ErrorsFound,
                                                                              "Relative Humidity",
                                                                              "WeatherFile",
                                                                              "Severe",
                                                                              "> 0",
                                                                              (RelHum >= 0.0),
                                                                              "<= 110",
                                                                              (RelHum <= 110.0),
                                                                              format("{:.0R}", RelHum),
                                                                              state.dataEnvrn->WeatherFileLocationTitle);
                    if (AtmPress < 999999.0)
                        state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                              ErrorsFound,
                                                                              "Atmospheric Pressure",
                                                                              "WeatherFile",
                                                                              "Severe",
                                                                              "> 31000",
                                                                              (AtmPress > 31000.0),
                                                                              "<=120000",
                                                                              (AtmPress <= 120000.0),
                                                                              format("{:.0R}", AtmPress),
                                                                              state.dataEnvrn->WeatherFileLocationTitle);
                    if (DirectRad < 9999.0)
                        state.dataInputProcessing->inputProcessor->lowerRangeCheck(state,
                                                                                   ErrorsFound,
                                                                                   "Direct Radiation",
                                                                                   "WeatherFile",
                                                                                   "Severe",
                                                                                   ">= 0",
                                                                                   (DirectRad >= 0.0),
                                                                                   {},
                                                                                   state.dataEnvrn->WeatherFileLocationTitle);
                    if (DiffuseRad < 9999.0)
                        state.dataInputProcessing->inputProcessor->lowerRangeCheck(state,
                                                                                   ErrorsFound,
                                                                                   "Diffuse Radiation",
                                                                                   "WeatherFile",
                                                                                   "Severe",
                                                                                   ">= 0",
                                                                                   (DiffuseRad >= 0.0),
                                                                                   {},
                                                                                   state.dataEnvrn->WeatherFileLocationTitle);
                    if (WindDir < 999.0)
                        state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                              ErrorsFound,
                                                                              "Wind Direction",
                                                                              "WeatherFile",
                                                                              "Severe",
                                                                              ">=0",
                                                                              (WindDir >= 0.0),
                                                                              "<=360",
                                                                              (WindDir <= 360.0),
                                                                              format("{:.0R}", WindDir),
                                                                              state.dataEnvrn->WeatherFileLocationTitle);
                    if (WindSpeed < 999.0)
                        state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                              ErrorsFound,
                                                                              "Wind Speed",
                                                                              "WeatherFile",
                                                                              "Severe",
                                                                              ">=0",
                                                                              (WindSpeed >= 0.0),
                                                                              "<=40",
                                                                              (WindSpeed <= 40.0),
                                                                              format("{:.2R}", WindSpeed),
                                                                              state.dataEnvrn->WeatherFileLocationTitle);
                    if (ErrorsFound) {
                        ShowSevereError(state, "Out of Range errors found with initial day of WeatherFile");
                    }
                } else {
                    //  Must skip this day
                    for (int i = 2; i <= state.dataWeatherManager->NumIntervalsPerHour; ++i) {
                        WeatherDataLine.update(state.files.inputWeatherFile.readLine());
                        if (!WeatherDataLine.good) {
                            readList(WeatherDataLine.data, WYear, WMonth, WDay, WHour, WMinute);
                            ShowFatalError(state,
                                           format("Error occurred on EPW while searching for first day, stopped at {}/{}/{} {}:{} IO Error='{}'",
                                                  WYear,
                                                  WMonth,
                                                  WDay,
                                                  WHour,
                                                  WMinute,
                                                  state.files.inputWeatherFile.error_state_to_string()),
                                           OptionalOutputFileRef{state.files.eso});
                        }
                    }
                    for (int i = 1; i <= 23 * state.dataWeatherManager->NumIntervalsPerHour; ++i) {
                        WeatherDataLine.update(state.files.inputWeatherFile.readLine());
                        if (!WeatherDataLine.good) {
                            readList(WeatherDataLine.data, WYear, WMonth, WDay, WHour, WMinute);
                            ShowFatalError(state,
                                           format("Error occurred on EPW while searching for first day, stopped at {}/{}/{} {}:{} IO Error='{}'",
                                                  WYear,
                                                  WMonth,
                                                  WDay,
                                                  WHour,
                                                  WMinute,
                                                  state.files.inputWeatherFile.error_state_to_string()),
                                           OptionalOutputFileRef{state.files.eso});
                        }
                    }
                }
            }

            // Positioned to proper day
            if (!state.dataGlobal->KickOffSimulation && !state.dataGlobal->DoingSizing &&
                state.dataWeatherManager->Environment(Environ).KindOfEnvrn == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                ++state.dataWeatherManager->Environment(Environ).CurrentCycle;
                if (!state.dataWeatherManager->Environment(Environ).RollDayTypeOnRepeat) {
                    SetDayOfWeekInitialValues(state.dataWeatherManager->Environment(Environ).DayOfWeek, state.dataWeatherManager->CurDayOfWeek);
                    if (state.dataWeatherManager->DaylightSavingIsActive) {
                        SetDSTDateRanges(state,
                                         state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                         state.dataWeatherManager->DSTIndex);
                    }
                    SetSpecialDayDates(state, state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay);
                } else if (state.dataWeatherManager->Environment(Environ).CurrentCycle == 1) {
                    SetDayOfWeekInitialValues(state.dataWeatherManager->Environment(Environ).DayOfWeek, state.dataWeatherManager->CurDayOfWeek);
                    state.dataWeatherManager->Environment(Environ).SetWeekDays = true;
                    if (state.dataWeatherManager->DaylightSavingIsActive) {
                        SetDSTDateRanges(state,
                                         state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay,
                                         state.dataWeatherManager->DSTIndex);
                    }
                    SetSpecialDayDates(state, state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).MonWeekDay);
                } else {
                    state.dataWeatherManager->CurDayOfWeek = state.dataEnvrn->DayOfWeekTomorrow;
                }
            } else {
                SetDayOfWeekInitialValues(state.dataWeatherManager->Environment(Environ).DayOfWeek, state.dataWeatherManager->CurDayOfWeek);
            }
        }

        bool TryAgain = true;
        bool SkipThisDay = false;

        while (TryAgain) {

            TryAgain = false;

            state.dataWeatherManager->TomorrowOutDryBulbTemp = 0.0;
            state.dataWeatherManager->TomorrowOutDewPointTemp = 0.0;
            state.dataWeatherManager->TomorrowOutBaroPress = 0.0;
            state.dataWeatherManager->TomorrowOutRelHum = 0.0;
            state.dataWeatherManager->TomorrowWindSpeed = 0.0;
            state.dataWeatherManager->TomorrowWindDir = 0.0;
            state.dataWeatherManager->TomorrowSkyTemp = 0.0;
            state.dataWeatherManager->TomorrowHorizIRSky = 0.0;
            state.dataWeatherManager->TomorrowBeamSolarRad = 0.0;
            state.dataWeatherManager->TomorrowDifSolarRad = 0.0;
            state.dataWeatherManager->TomorrowAlbedo = 0.0;
            state.dataWeatherManager->TomorrowLiquidPrecip = 0.0;
            state.dataWeatherManager->TomorrowIsRain = false;
            state.dataWeatherManager->TomorrowIsSnow = false;

            for (int hour = 1; hour <= 24; ++hour) {
                for (int CurTimeStep = 1; CurTimeStep <= state.dataWeatherManager->NumIntervalsPerHour; ++CurTimeStep) {
                    auto WeatherDataLine = state.files.inputWeatherFile.readLine();
                    if (!WeatherDataLine.good) WeatherDataLine.data.clear();
                    if (WeatherDataLine.data.empty()) {
                        if (hour == 1) {
                            WeatherDataLine.eof = true;
                            WeatherDataLine.good = false;
                        } else {
                            WeatherDataLine.good = false;
                        }
                    }
                    if (WeatherDataLine.good) {
                        bool ErrorFound;
                        InterpretWeatherDataLine(state,
                                                 WeatherDataLine.data,
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
                    } else { // ReadStatus /=0
                        if (WeatherDataLine.eof &&
                            state.dataWeatherManager->NumDataPeriods == 1) { // Standard End-of-file, rewind and position to first day...
                            if (state.dataWeatherManager->DataPeriods(1).NumDays >= state.dataWeatherManager->NumDaysInYear) {
                                state.files.inputWeatherFile.rewind();
                                SkipEPlusWFHeader(state);
                                WeatherDataLine.update(state.files.inputWeatherFile.readLine());
                                bool ErrorFound;
                                InterpretWeatherDataLine(state,
                                                         WeatherDataLine.data,
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
                                ShowFatalError(state,
                                               format("End-of-File encountered after {}/{}/{} {}:{}, starting from first day of Weather File would "
                                                      "not be \"next day\"",
                                                      WYear,
                                                      WMonth,
                                                      WDay,
                                                      WHour,
                                                      WMinute));
                            }
                        } else {
                            ShowFatalError(state,
                                           format("Unexpected error condition in middle of reading EPW file, stopped at {}/{}/{} {}:{}",
                                                  WYear,
                                                  WMonth,
                                                  WDay,
                                                  WHour,
                                                  WMinute),
                                           OptionalOutputFileRef{state.files.eso});
                        }
                    }

                    if (hour != WHour) {
                        ShowFatalError(state,
                                       format("Unexpected error condition in middle of reading EPW file, stopped at {}/{}/{} {}:{}",
                                              WYear,
                                              WMonth,
                                              WDay,
                                              WHour,
                                              WMinute),
                                       OptionalOutputFileRef{state.files.eso});
                    }

                    //         Set possible missing values
                    if (ETHoriz < 0.0) ETHoriz = 9999.0;
                    if (ETDirect < 0.0) ETDirect = 9999.0;
                    if (IRHoriz <= 0.0) IRHoriz = 9999.0;
                    if (GLBHoriz < 0.0) GLBHoriz = 9999.0;
                    if (state.dataEnvrn->DisplayWeatherMissingDataWarnings) {
                        if (DirectRad >= 9999.0) {
                            ++state.dataWeatherManager->Missed.DirectRad;
                        }
                        if (DiffuseRad >= 9999.0) {
                            state.dataWeatherManager->Missed.DiffuseRad = state.dataWeatherManager->Missed.DirectRad + 1;
                        }
                        if (DirectRad < 0.0) {
                            DirectRad = 9999.0;
                            ++state.dataWeatherManager->OutOfRange.DirectRad;
                        }
                        if (DiffuseRad < 0.0) {
                            DiffuseRad = 9999.0;
                            ++state.dataWeatherManager->OutOfRange.DiffuseRad;
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
                    if (PresWeathObs < 0) PresWeathObs = 9;
                    if (PrecipWater < 0.0) PrecipWater = 999.0;
                    if (AerosolOptDepth < 0.0) AerosolOptDepth = 999.0;
                    if (SnowDepth < 0.0) SnowDepth = 999.0;
                    if (DaysSinceLastSnow < 0.0) DaysSinceLastSnow = 99.0;
                    if (Albedo < 0.0) Albedo = 999.0;
                    if (LiquidPrecip < 0.0) LiquidPrecip = 999.0;

                    if (hour == 1 && CurTimeStep == 1) {
                        if (WMonth == 2 && WDay == 29 && (!state.dataEnvrn->CurrentYearIsLeapYear || !state.dataWeatherManager->WFAllowsLeapYears)) {
                            state.dataWeatherManager->EndDayOfMonth(2) = 28;
                            SkipThisDay = true;
                            TryAgain = true;
                            ShowWarningError(state, "ReadEPlusWeatherForDay: Feb29 data encountered but will not be processed.");
                            if (!state.dataWeatherManager->WFAllowsLeapYears) {
                                ShowContinueError(
                                    state, "...WeatherFile does not allow Leap Years. HOLIDAYS/DAYLIGHT SAVINGS header must indicate \"Yes\".");
                            }
                            continue;
                        } else {
                            TryAgain = false;
                            SkipThisDay = false;
                        }

                        if (state.dataWeatherManager->Environment(Environ).ActualWeather && state.dataEnvrn->CurrentYearIsLeapYear) {
                            if (WMonth == 3 && WDay == 1 && state.dataEnvrn->Month == 2 && state.dataEnvrn->DayOfMonth == 28) {
                                ShowFatalError(state, "ReadEPlusWeatherForDay: Current year is a leap year, but Feb29 data is missing.");
                            }
                        }

                        state.dataWeatherManager->TomorrowVariables.Year = WYear;
                        state.dataWeatherManager->TomorrowVariables.Month = WMonth;
                        state.dataWeatherManager->TomorrowVariables.DayOfMonth = WDay;
                        state.dataWeatherManager->TomorrowVariables.DayOfYear =
                            General::OrdinalDay(WMonth, WDay, state.dataWeatherManager->LeapYearAdd);
                        state.dataWeatherManager->TomorrowVariables.DayOfYear_Schedule = General::OrdinalDay(WMonth, WDay, 1);
                        Real64 A;
                        Real64 B;
                        Real64 C;
                        Real64 AVSC;
                        CalculateDailySolarCoeffs(state,
                                                  state.dataWeatherManager->TomorrowVariables.DayOfYear,
                                                  A,
                                                  B,
                                                  C,
                                                  AVSC,
                                                  state.dataWeatherManager->TomorrowVariables.EquationOfTime,
                                                  state.dataWeatherManager->TomorrowVariables.SinSolarDeclinAngle,
                                                  state.dataWeatherManager->TomorrowVariables.CosSolarDeclinAngle);
                        if (state.dataWeatherManager->CurDayOfWeek <= 7) {
                            state.dataWeatherManager->CurDayOfWeek = mod(state.dataWeatherManager->CurDayOfWeek, 7) + 1;
                        }
                        state.dataWeatherManager->TomorrowVariables.DayOfWeek = state.dataWeatherManager->CurDayOfWeek;
                        state.dataWeatherManager->TomorrowVariables.DaylightSavingIndex =
                            state.dataWeatherManager->DSTIndex(state.dataWeatherManager->TomorrowVariables.DayOfYear);
                        state.dataWeatherManager->TomorrowVariables.HolidayIndex =
                            state.dataWeatherManager->SpecialDayTypes(state.dataWeatherManager->TomorrowVariables.DayOfYear);
                    }

                    if (SkipThisDay) continue;

                    // Check out missing values

                    if (DryBulb >= 99.9) {
                        DryBulb = state.dataWeatherManager->Missing.DryBulb;
                        ++state.dataWeatherManager->Missed.DryBulb;
                    }
                    if (DryBulb < -90.0 || DryBulb > 70.0) {
                        ++state.dataWeatherManager->OutOfRange.DryBulb;
                    }

                    if (DewPoint >= 99.9) {
                        DewPoint = state.dataWeatherManager->Missing.DewPoint;
                        ++state.dataWeatherManager->Missed.DewPoint;
                    }
                    if (DewPoint < -90.0 || DewPoint > 70.0) {
                        ++state.dataWeatherManager->OutOfRange.DewPoint;
                    }

                    if (RelHum >= 999.0) {
                        RelHum = state.dataWeatherManager->Missing.RelHumid;
                        ++state.dataWeatherManager->Missed.RelHumid;
                    }
                    if (RelHum < 0.0 || RelHum > 110.0) {
                        ++state.dataWeatherManager->OutOfRange.RelHumid;
                    }

                    if (AtmPress >= 999999.0) {
                        AtmPress = state.dataWeatherManager->Missing.StnPres;
                        ++state.dataWeatherManager->Missed.StnPres;
                    }
                    if (AtmPress <= 31000.0 || AtmPress > 120000.0) {
                        ++state.dataWeatherManager->OutOfRange.StnPres;
                        AtmPress = state.dataWeatherManager->Missing.StnPres;
                    }

                    if (WindDir >= 999.0) {
                        WindDir = state.dataWeatherManager->Missing.WindDir;
                        ++state.dataWeatherManager->Missed.WindDir;
                    }
                    if (WindDir < 0.0 || WindDir > 360.0) {
                        ++state.dataWeatherManager->OutOfRange.WindDir;
                    }

                    if (WindSpeed >= 999.0) {
                        WindSpeed = state.dataWeatherManager->Missing.WindSpd;
                        ++state.dataWeatherManager->Missed.WindSpd;
                    }
                    if (WindSpeed < 0.0 || WindSpeed > 40.0) {
                        ++state.dataWeatherManager->OutOfRange.WindSpd;
                    }

                    if (TotalSkyCover >= 99.0) {
                        TotalSkyCover = state.dataWeatherManager->Missing.TotSkyCvr;
                        ++state.dataWeatherManager->Missed.TotSkyCvr;
                    }

                    if (OpaqueSkyCover >= 99.0) {
                        OpaqueSkyCover = state.dataWeatherManager->Missing.OpaqSkyCvr;
                        ++state.dataWeatherManager->Missed.OpaqSkyCvr;
                    }

                    if (SnowDepth >= 999.0) {
                        SnowDepth = state.dataWeatherManager->Missing.SnowDepth;
                        ++state.dataWeatherManager->Missed.SnowDepth;
                    }

                    if (Albedo >= 999.0) {
                        Albedo = state.dataWeatherManager->Missing.Albedo;
                        ++state.dataWeatherManager->Missed.Albedo;
                    }

                    if (LiquidPrecip >= 999.0) {
                        LiquidPrecip = state.dataWeatherManager->Missing.LiquidPrecip;
                        ++state.dataWeatherManager->Missed.LiquidPrecip;
                    }

                    state.dataWeatherManager->TomorrowOutDryBulbTemp(CurTimeStep, hour) = DryBulb;
                    state.dataWeatherManager->TomorrowOutDewPointTemp(CurTimeStep, hour) = DewPoint;
                    state.dataWeatherManager->TomorrowOutBaroPress(CurTimeStep, hour) = AtmPress;
                    state.dataWeatherManager->TomorrowOutRelHum(CurTimeStep, hour) = RelHum;
                    RelHum *= 0.01;
                    state.dataWeatherManager->TomorrowWindSpeed(CurTimeStep, hour) = WindSpeed;
                    state.dataWeatherManager->TomorrowWindDir(CurTimeStep, hour) = WindDir;
                    state.dataWeatherManager->TomorrowLiquidPrecip(CurTimeStep, hour) = LiquidPrecip;
                    state.dataWeatherManager->TomorrowTotalSkyCover(CurTimeStep, hour) = TotalSkyCover;
                    state.dataWeatherManager->TomorrowOpaqueSkyCover(CurTimeStep, hour) = OpaqueSkyCover;

                    calcSky(state,
                            state.dataWeatherManager->TomorrowHorizIRSky(CurTimeStep, hour),
                            state.dataWeatherManager->TomorrowSkyTemp(CurTimeStep, hour),
                            OpaqueSkyCover,
                            DryBulb,
                            DewPoint,
                            RelHum,
                            IRHoriz);

                    if (ETHoriz >= 9999.0) ETHoriz = 0.0;
                    if (ETDirect >= 9999.0) ETDirect = 0.0;
                    if (GLBHoriz >= 9999.0) GLBHoriz = 0.0;
                    if (DirectRad >= 9999.0) DirectRad = 0.0;
                    if (DiffuseRad >= 9999.0) DiffuseRad = 0.0;
                    if (GLBHorizIllum >= 999900.0) GLBHorizIllum = 0.0;
                    if (DirectNrmIllum >= 999900.0) DirectNrmIllum = 0.0;
                    if (DiffuseHorizIllum >= 999900.0) DiffuseHorizIllum = 0.0;
                    if (ZenLum >= 99990.0) ZenLum = 0.0;
                    if (state.dataEnvrn->IgnoreSolarRadiation) {
                        GLBHoriz = 0.0;
                        DirectRad = 0.0;
                        DiffuseRad = 0.0;
                    }
                    if (state.dataEnvrn->IgnoreBeamRadiation) {
                        DirectRad = 0.0;
                    }
                    if (state.dataEnvrn->IgnoreDiffuseRadiation) {
                        DiffuseRad = 0.0;
                    }

                    state.dataWeatherManager->TomorrowBeamSolarRad(CurTimeStep, hour) = DirectRad;
                    state.dataWeatherManager->TomorrowDifSolarRad(CurTimeStep, hour) = DiffuseRad;

                    state.dataWeatherManager->TomorrowIsRain(CurTimeStep, hour) = false;
                    if (PresWeathObs == 0) {
                        if (PresWeathConds(1) < 9 || PresWeathConds(2) < 9 || PresWeathConds(3) < 9)
                            state.dataWeatherManager->TomorrowIsRain(CurTimeStep, hour) = true;
                    } else {
                        state.dataWeatherManager->TomorrowIsRain(CurTimeStep, hour) = false;
                    }
                    state.dataWeatherManager->TomorrowIsSnow(CurTimeStep, hour) = (SnowDepth > 0.0);

                    // default if rain but none on weather file
                    if (state.dataWeatherManager->TomorrowIsRain(CurTimeStep, hour) &&
                        state.dataWeatherManager->TomorrowLiquidPrecip(CurTimeStep, hour) == 0.0)
                        state.dataWeatherManager->TomorrowLiquidPrecip(CurTimeStep, hour) = 2.0; // 2mm in an hour ~ .08 inch

                    state.dataWeatherManager->Missing.DryBulb = DryBulb;
                    state.dataWeatherManager->Missing.DewPoint = DewPoint;
                    state.dataWeatherManager->Missing.RelHumid = static_cast<int>(std::round(RelHum * 100.0));
                    state.dataWeatherManager->Missing.StnPres = AtmPress;
                    state.dataWeatherManager->Missing.WindDir = WindDir;
                    state.dataWeatherManager->Missing.WindSpd = WindSpeed;
                    state.dataWeatherManager->Missing.TotSkyCvr = TotalSkyCover;
                    state.dataWeatherManager->Missing.OpaqSkyCvr = OpaqueSkyCover;
                    state.dataWeatherManager->Missing.Visibility = Visibility;
                    state.dataWeatherManager->Missing.Ceiling = CeilHeight;
                    state.dataWeatherManager->Missing.PrecipWater = PrecipWater;
                    state.dataWeatherManager->Missing.AerOptDepth = AerosolOptDepth;
                    state.dataWeatherManager->Missing.SnowDepth = SnowDepth;
                    state.dataWeatherManager->Missing.DaysLastSnow = DaysSinceLastSnow;
                    state.dataWeatherManager->Missing.Albedo = Albedo;

                } // CurTimeStep Loop

            } // Hour Loop

        } // Try Again While Loop

        if (BackSpaceAfterRead) {
            state.files.inputWeatherFile.backspace();
        }

        if (state.dataWeatherManager->NumIntervalsPerHour == 1 && state.dataGlobal->NumOfTimeStepInHour > 1) {
            // Create interpolated weather for timestep orientation
            // First copy ts=1 (hourly) from data arrays to Wthr structure
            for (int hour = 1; hour <= 24; ++hour) {
                Wthr.OutDryBulbTemp(hour) = state.dataWeatherManager->TomorrowOutDryBulbTemp(1, hour);
                Wthr.OutDewPointTemp(hour) = state.dataWeatherManager->TomorrowOutDewPointTemp(1, hour);
                Wthr.OutBaroPress(hour) = state.dataWeatherManager->TomorrowOutBaroPress(1, hour);
                Wthr.OutRelHum(hour) = state.dataWeatherManager->TomorrowOutRelHum(1, hour);
                Wthr.WindSpeed(hour) = state.dataWeatherManager->TomorrowWindSpeed(1, hour);
                Wthr.WindDir(hour) = state.dataWeatherManager->TomorrowWindDir(1, hour);
                Wthr.SkyTemp(hour) = state.dataWeatherManager->TomorrowSkyTemp(1, hour);
                Wthr.HorizIRSky(hour) = state.dataWeatherManager->TomorrowHorizIRSky(1, hour);
                Wthr.BeamSolarRad(hour) = state.dataWeatherManager->TomorrowBeamSolarRad(1, hour);
                Wthr.DifSolarRad(hour) = state.dataWeatherManager->TomorrowDifSolarRad(1, hour);
                Wthr.IsRain(hour) = state.dataWeatherManager->TomorrowIsRain(1, hour);
                Wthr.IsSnow(hour) = state.dataWeatherManager->TomorrowIsSnow(1, hour);
                Wthr.Albedo(hour) = state.dataWeatherManager->TomorrowAlbedo(1, hour);
                Wthr.LiquidPrecip(hour) = state.dataWeatherManager->TomorrowLiquidPrecip(1, hour);
                Wthr.TotalSkyCover(hour) = state.dataWeatherManager->TomorrowTotalSkyCover(1, hour);
                Wthr.OpaqueSkyCover(hour) = state.dataWeatherManager->TomorrowOpaqueSkyCover(1, hour);
            }

            if (!state.dataWeatherManager->LastHourSet) {
                // For first day of weather, all time steps of the first hour will be
                // equal to the first hour's value.
                // 2021-06: An additional input is added to here to allow the user to have chosen which hour to use
                int HrUsedtoInterp = state.dataWeatherManager->Environment(Environ).firstHrInterpUseHr1 ? 1 : 24;

                state.dataWeatherManager->LastHrOutDryBulbTemp = Wthr.OutDryBulbTemp(HrUsedtoInterp);
                state.dataWeatherManager->LastHrOutDewPointTemp = Wthr.OutDewPointTemp(HrUsedtoInterp);
                state.dataWeatherManager->LastHrOutBaroPress = Wthr.OutBaroPress(HrUsedtoInterp);
                state.dataWeatherManager->LastHrOutRelHum = Wthr.OutRelHum(HrUsedtoInterp);
                state.dataWeatherManager->LastHrWindSpeed = Wthr.WindSpeed(HrUsedtoInterp);
                state.dataWeatherManager->LastHrWindDir = Wthr.WindDir(HrUsedtoInterp);
                state.dataWeatherManager->LastHrSkyTemp = Wthr.SkyTemp(HrUsedtoInterp);
                state.dataWeatherManager->LastHrHorizIRSky = Wthr.HorizIRSky(HrUsedtoInterp);
                state.dataWeatherManager->LastHrBeamSolarRad = Wthr.BeamSolarRad(HrUsedtoInterp);
                state.dataWeatherManager->LastHrDifSolarRad = Wthr.DifSolarRad(HrUsedtoInterp);
                state.dataWeatherManager->LastHrAlbedo = Wthr.Albedo(HrUsedtoInterp);
                state.dataWeatherManager->LastHrLiquidPrecip = Wthr.LiquidPrecip(HrUsedtoInterp);
                state.dataWeatherManager->LastHrTotalSkyCover = Wthr.TotalSkyCover(HrUsedtoInterp);
                state.dataWeatherManager->LastHrOpaqueSkyCover = Wthr.OpaqueSkyCover(HrUsedtoInterp);

                state.dataWeatherManager->LastHourSet = true;
            }

            for (int hour = 1; hour <= 24; ++hour) {

                int NxtHour = hour + 1;
                if (hour == 24) {
                    NxtHour = 1;
                }
                state.dataWeatherManager->NextHrBeamSolarRad = Wthr.BeamSolarRad(NxtHour);
                state.dataWeatherManager->NextHrDifSolarRad = Wthr.DifSolarRad(NxtHour);
                state.dataWeatherManager->NextHrLiquidPrecip = Wthr.LiquidPrecip(NxtHour);

                for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {

                    Real64 WtNow = state.dataWeatherManager->Interpolation(ts);
                    Real64 WtPrevHour = 1.0 - WtNow;

                    // Do Solar "weighting"

                    Real64 WgtHourNow = state.dataWeatherManager->SolarInterpolation(ts);
                    Real64 WgtPrevHour;
                    Real64 WgtNextHour;

                    if (state.dataGlobal->NumOfTimeStepInHour == 1) {
                        WgtNextHour = 1.0 - WgtHourNow;
                        WgtPrevHour = 0.0;
                    } else {
                        if (WgtHourNow == 1.0) {
                            //  It's at the half hour
                            WgtNextHour = 0.0;
                            WgtPrevHour = 0.0;
                        } else if (ts * state.dataWeatherManager->TimeStepFraction < 0.5) {
                            WgtNextHour = 0.0;
                            WgtPrevHour = 1.0 - WgtHourNow;
                        } else { // After the half hour
                            WgtPrevHour = 0.0;
                            WgtNextHour = 1.0 - WgtHourNow;
                        }
                    }

                    state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour) =
                        state.dataWeatherManager->LastHrOutDryBulbTemp * WtPrevHour + Wthr.OutDryBulbTemp(hour) * WtNow;
                    state.dataWeatherManager->TomorrowOutBaroPress(ts, hour) =
                        state.dataWeatherManager->LastHrOutBaroPress * WtPrevHour + Wthr.OutBaroPress(hour) * WtNow;
                    state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour) =
                        state.dataWeatherManager->LastHrOutDewPointTemp * WtPrevHour + Wthr.OutDewPointTemp(hour) * WtNow;
                    state.dataWeatherManager->TomorrowOutRelHum(ts, hour) =
                        state.dataWeatherManager->LastHrOutRelHum * WtPrevHour + Wthr.OutRelHum(hour) * WtNow;
                    state.dataWeatherManager->TomorrowWindSpeed(ts, hour) =
                        state.dataWeatherManager->LastHrWindSpeed * WtPrevHour + Wthr.WindSpeed(hour) * WtNow;
                    state.dataWeatherManager->TomorrowWindDir(ts, hour) =
                        interpolateWindDirection(state.dataWeatherManager->LastHrWindDir, Wthr.WindDir(hour), WtNow);
                    state.dataWeatherManager->TomorrowTotalSkyCover(ts, hour) =
                        state.dataWeatherManager->LastHrTotalSkyCover * WtPrevHour + Wthr.TotalSkyCover(hour) * WtNow;
                    state.dataWeatherManager->TomorrowOpaqueSkyCover(ts, hour) =
                        state.dataWeatherManager->LastHrOpaqueSkyCover * WtPrevHour + Wthr.OpaqueSkyCover(hour) * WtNow;
                    // Sky emissivity now takes interpolated timestep inputs rather than interpolated calculation esky results
                    calcSky(state,
                            state.dataWeatherManager->TomorrowHorizIRSky(ts, hour),
                            state.dataWeatherManager->TomorrowSkyTemp(ts, hour),
                            state.dataWeatherManager->TomorrowOpaqueSkyCover(ts, hour),
                            state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                            state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour),
                            state.dataWeatherManager->TomorrowOutRelHum(ts, hour) * 0.01,
                            state.dataWeatherManager->LastHrHorizIRSky * WtPrevHour + Wthr.HorizIRSky(hour) * WtNow);

                    state.dataWeatherManager->TomorrowDifSolarRad(ts, hour) = state.dataWeatherManager->LastHrDifSolarRad * WgtPrevHour +
                                                                              Wthr.DifSolarRad(hour) * WgtHourNow +
                                                                              state.dataWeatherManager->NextHrDifSolarRad * WgtNextHour;
                    state.dataWeatherManager->TomorrowBeamSolarRad(ts, hour) = state.dataWeatherManager->LastHrBeamSolarRad * WgtPrevHour +
                                                                               Wthr.BeamSolarRad(hour) * WgtHourNow +
                                                                               state.dataWeatherManager->NextHrBeamSolarRad * WgtNextHour;

                    state.dataWeatherManager->TomorrowLiquidPrecip(ts, hour) =
                        state.dataWeatherManager->LastHrLiquidPrecip * WtPrevHour + Wthr.LiquidPrecip(hour) * WtNow;
                    state.dataWeatherManager->TomorrowLiquidPrecip(ts, hour) /= double(state.dataGlobal->NumOfTimeStepInHour);
                    state.dataWeatherManager->TomorrowIsRain(ts, hour) =
                        state.dataWeatherManager->TomorrowLiquidPrecip(ts, hour) >= state.dataWeatherManager->IsRainThreshold; // Wthr%IsRain(Hour)
                    state.dataWeatherManager->TomorrowIsSnow(ts, hour) = Wthr.IsSnow(hour);
                } // End of TS Loop

                state.dataWeatherManager->LastHrOutDryBulbTemp = Wthr.OutDryBulbTemp(hour);
                state.dataWeatherManager->LastHrOutDewPointTemp = Wthr.OutDewPointTemp(hour);
                state.dataWeatherManager->LastHrOutBaroPress = Wthr.OutBaroPress(hour);
                state.dataWeatherManager->LastHrOutRelHum = Wthr.OutRelHum(hour);
                state.dataWeatherManager->LastHrWindSpeed = Wthr.WindSpeed(hour);
                state.dataWeatherManager->LastHrWindDir = Wthr.WindDir(hour);
                state.dataWeatherManager->LastHrHorizIRSky = Wthr.HorizIRSky(hour);
                state.dataWeatherManager->LastHrSkyTemp = Wthr.SkyTemp(hour);
                state.dataWeatherManager->LastHrBeamSolarRad = Wthr.BeamSolarRad(hour);
                state.dataWeatherManager->LastHrDifSolarRad = Wthr.DifSolarRad(hour);
                state.dataWeatherManager->LastHrAlbedo = Wthr.Albedo(hour);
                state.dataWeatherManager->LastHrLiquidPrecip = Wthr.LiquidPrecip(hour);
                state.dataWeatherManager->LastHrTotalSkyCover = Wthr.TotalSkyCover(hour);
                state.dataWeatherManager->LastHrOpaqueSkyCover = Wthr.OpaqueSkyCover(hour);
            } // End of Hour Loop
        }

        if (state.dataWeatherManager->Environment(Environ).WP_Type1 != 0) {
            switch (state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(Environ).WP_Type1).CalculationType) {
            case EmissivityCalcType::ScheduleValue:
                ScheduleManager::GetScheduleValuesForDay(
                    state,
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(Environ).WP_Type1).SchedulePtr,
                    state.dataWeatherManager->TomorrowSkyTemp,
                    state.dataWeatherManager->TomorrowVariables.DayOfYear_Schedule,
                    state.dataWeatherManager->CurDayOfWeek);
                break;
            case EmissivityCalcType::DryBulbDelta:
                ScheduleManager::GetScheduleValuesForDay(
                    state,
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(Environ).WP_Type1).SchedulePtr,
                    state.dataWeatherManager->TomorrowSkyTemp,
                    state.dataWeatherManager->TomorrowVariables.DayOfYear_Schedule,
                    state.dataWeatherManager->CurDayOfWeek);
                for (int hour = 1; hour <= 24; ++hour) {
                    for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {
                        state.dataWeatherManager->TomorrowSkyTemp(ts, hour) =
                            state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour) - state.dataWeatherManager->TomorrowSkyTemp(ts, hour);
                    }
                }
                break;
            case EmissivityCalcType::DewPointDelta:
                ScheduleManager::GetScheduleValuesForDay(
                    state,
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(Environ).WP_Type1).SchedulePtr,
                    state.dataWeatherManager->TomorrowSkyTemp,
                    state.dataWeatherManager->TomorrowVariables.DayOfYear_Schedule,
                    state.dataWeatherManager->CurDayOfWeek);
                for (int hour = 1; hour <= 24; ++hour) {
                    for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {
                        state.dataWeatherManager->TomorrowSkyTemp(ts, hour) =
                            state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour) - state.dataWeatherManager->TomorrowSkyTemp(ts, hour);
                    }
                }
                break;
            default:
                break;
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

    Real64 CalcSkyEmissivity(EnergyPlusData &state,
                             EmissivityCalcType const ESkyCalcType,
                             Real64 const OSky,
                             Real64 const DryBulb,
                             Real64 const DewPoint,
                             Real64 const RelHum)
    {
        // Calculate Sky Emissivity
        // References:
        // M. Li, Y. Jiang and C. F. M. Coimbra,
        // "On the determination of atmospheric longwave irradiance under all-sky conditions,"
        // Solar Energy 144, 2017, pp. 40–48,
        // G. Clark and C. Allen, "The Estimation of Atmospheric Radiation for Clear and
        // Cloudy Skies," Proc. 2nd National Passive Solar Conference (AS/ISES), 1978, pp. 675-678.

        Real64 ESky;

        if (ESkyCalcType == EmissivityCalcType::BruntModel) {
            double const PartialPress = RelHum * Psychrometrics::PsyPsatFnTemp(state, DryBulb) * 0.01;
            ESky = 0.618 + 0.056 * pow(PartialPress, 0.5);
        } else if (ESkyCalcType == EmissivityCalcType::IdsoModel) {
            double const PartialPress = RelHum * Psychrometrics::PsyPsatFnTemp(state, DryBulb) * 0.01;
            ESky = 0.685 + 0.000032 * PartialPress * exp(1699 / (DryBulb + DataGlobalConstants::KelvinConv));
        } else if (ESkyCalcType == EmissivityCalcType::BerdahlMartinModel) {
            double const TDewC = min(DryBulb, DewPoint);
            ESky = 0.758 + 0.521 * (TDewC / 100) + 0.625 * pow_2(TDewC / 100);
        } else {
            ESky = 0.787 + 0.764 * std::log((min(DryBulb, DewPoint) + DataGlobalConstants::KelvinConv) / DataGlobalConstants::KelvinConv);
        }
        ESky = ESky * (1.0 + 0.0224 * OSky - 0.0035 * pow_2(OSky) + 0.00028 * pow_3(OSky));
        return ESky;
    }

    void SetDayOfWeekInitialValues(int const EnvironDayOfWeek, // Starting Day of Week for the (Weather) RunPeriod (User Input)
                                   int &currentDayOfWeek       // Current Day of Week
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
                currentDayOfWeek = EnvironDayOfWeek - 1;
            } else {
                currentDayOfWeek = EnvironDayOfWeek;
            }
        }
    }

    void ErrorInterpretWeatherDataLine(EnergyPlusData &state,
                                       int const WYear,
                                       int const WMonth,
                                       int const WDay,
                                       int const WHour,
                                       int const WMinute,
                                       std::string_view SaveLine,
                                       std::string_view Line)
    {
        ShowSevereError(state, fmt::format("Invalid Weather Line at date={:4}/{:2}/{:2} Hour#={:2} Min#={:2}", WYear, WMonth, WDay, WHour, WMinute));
        ShowContinueError(state, fmt::format("Full Data Line={}", SaveLine));
        ShowContinueError(state, fmt::format("Remainder of line={}", Line));
        ShowFatalError(state, "Error in Reading Weather Data");
    }

    void InterpretWeatherDataLine(EnergyPlusData &state,
                                  std::string_view Line,
                                  bool &ErrorFound, // True if an error is found, false otherwise
                                  int &WYear,
                                  int &WMonth,
                                  int &WDay,
                                  int &WHour,
                                  int &WMinute,
                                  Real64 &DryBulb,
                                  Real64 &DewPoint,
                                  Real64 &RelHum,
                                  Real64 &AtmPress,
                                  Real64 &ETHoriz,
                                  Real64 &ETDirect,
                                  Real64 &IRHoriz,
                                  Real64 &GLBHoriz,
                                  Real64 &DirectRad,
                                  Real64 &DiffuseRad,
                                  Real64 &GLBHorizIllum,
                                  Real64 &DirectNrmIllum,
                                  Real64 &DiffuseHorizIllum,
                                  Real64 &ZenLum,
                                  Real64 &WindDir,
                                  Real64 &WindSpeed,
                                  Real64 &TotalSkyCover,
                                  Real64 &OpaqueSkyCover,
                                  Real64 &Visibility,
                                  Real64 &CeilHeight,
                                  int &WObs,              // PresWeathObs
                                  Array1D_int &WCodesArr, // PresWeathConds
                                  Real64 &PrecipWater,
                                  Real64 &AerosolOptDepth,
                                  Real64 &SnowDepth,
                                  Real64 &DaysSinceLastSnow,
                                  Real64 &Albedo,
                                  Real64 &LiquidPrecip)
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

        EP_SIZE_CHECK(WCodesArr, 9); // NOLINT(misc-static-assert)

        static constexpr std::string_view ValidDigits("0123456789");

        std::string_view::size_type pos = 0;
        std::string_view current_line = Line;

        ErrorFound = false;

        // Do the first five.  (To get to the DataSource field)
        {
            auto nth_pos = nth_occurrence(current_line, ',', 5);
            const bool succeeded = readList(current_line.substr(pos, nth_pos), WYear, WMonth, WDay, WHour, WMinute);
            if (!succeeded) {
                ShowSevereError(state, "Invalid Date info in Weather Line");
                ShowContinueError(state, fmt::format("Entire Data Line={}", Line));
                ShowFatalError(state, "Error in Reading Weather Data");
            }
        }

        bool DateInError = false;
        if (WMonth >= 1 && WMonth <= 12) {
            // Month number is valid
            if (WMonth != 2) {
                if (WDay > state.dataWeatherManager->EndDayOfMonth(WMonth)) {
                    DateInError = true;
                }
            } else if (WDay > state.dataWeatherManager->EndDayOfMonth(WMonth) + 1) { // Whether actually used is determined by calling routine.
                DateInError = true;
            }
        } else {
            DateInError = true;
        }

        if (DateInError) {
            ShowSevereError(state, format("Reading Weather Data Line, Invalid Date, Year={}, Month={}, Day={}", WYear, WMonth, WDay));
            ShowFatalError(state, "Program terminates due to previous condition.");
        }

        pos = index(Line, ','); // WYear
        if (pos == std::string::npos) {
            ShowSevereError(
                state, format("Invalid Weather Line (no commas) at date={:4}/{:2}/{:2} Hour#={:2} Min#={:2}", WYear, WMonth, WDay, WHour, WMinute));
            ShowContinueError(state, fmt::format("Full Data Line={}", Line));
            ShowFatalError(state, "Error in Reading Weather Data");
        }
        current_line.remove_prefix(nth_occurrence(Line, ',', 6)); // remove WYear,WMonth,WDay,WHour,WMinute,Data Source/Integrity

        // Now read more numerics with List Directed I/O (note there is another "character" field lurking)
        Real64 RField21;
        {
            auto nth_pos = nth_occurrence(current_line, ',', 21);

            const bool succeeded = readList(current_line.substr(0, nth_pos),
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
                                            RField21);

            if (!succeeded) ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
            current_line.remove_prefix(nth_pos + 1);
        }
        pos = index(current_line, ',');
        std::string PresWeathCodes;
        if (pos != std::string::npos && pos != 0) {
            PresWeathCodes = current_line.substr(0, pos);
        } else {
            PresWeathCodes = "999999999";
        }
        current_line.remove_prefix(pos + 1);
        pos = index(current_line, ',');
        if (pos != std::string::npos) {
            if (pos != 0) {
                bool error = false;
                PrecipWater = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                if (error) {
                    ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                }
            } else {
                PrecipWater = 999.0;
            }
            current_line.remove_prefix(pos + 1);
            pos = index(current_line, ',');
            if (pos != std::string::npos) {
                if (pos != 0) {
                    bool error = false;
                    AerosolOptDepth = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                    if (error) {
                        ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                    }
                } else {
                    AerosolOptDepth = 999.0;
                }
                current_line.remove_prefix(pos + 1);
                pos = index(current_line, ',');
                if (pos != std::string::npos) {
                    if (pos != 0) {
                        bool error = false;
                        SnowDepth = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                        if (error) {
                            ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                        }
                    } else {
                        SnowDepth = 999.0;
                    }
                    current_line.remove_prefix(pos + 1);
                    pos = index(current_line, ',');
                    if (pos != std::string::npos) {
                        if (pos != 0) {
                            bool error = false;
                            DaysSinceLastSnow = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                            if (error) {
                                ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                            }
                        } else {
                            DaysSinceLastSnow = 999.0;
                        }
                        current_line.remove_prefix(pos + 1);
                        pos = index(current_line, ',');
                        if (pos != std::string::npos) {
                            if (pos != 0) {
                                bool error = false;
                                Albedo = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                                if (error) {
                                    ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                                }
                            } else {
                                Albedo = 999.0;
                            }
                            current_line.remove_prefix(pos + 1);
                            pos = index(current_line, ',');
                            if (pos != std::string::npos) {
                                if (pos != 0) {
                                    bool error = false;
                                    LiquidPrecip = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                                    if (error) {
                                        ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                                    }
                                } else {
                                    LiquidPrecip = 999.0;
                                }
                                current_line.remove_prefix(pos + 1);
                                pos = index(current_line, ',');
                            } else {
                                LiquidPrecip = 999.0;
                            }
                        } else {
                            Albedo = 999.0;
                            LiquidPrecip = 999.0;
                        }
                    } else {
                        bool error = false;
                        DaysSinceLastSnow = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                        if (error) {
                            ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                        }
                        Albedo = 999.0;
                        LiquidPrecip = 999.0;
                    }
                } else {
                    bool error = false;
                    SnowDepth = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                    if (error) {
                        ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                    }
                    DaysSinceLastSnow = 999.0;
                    Albedo = 999.0;
                    LiquidPrecip = 999.0;
                }
            } else {
                bool error = false;
                AerosolOptDepth = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
                if (error) {
                    ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
                }
                SnowDepth = 999.0;
                DaysSinceLastSnow = 999.0;
                Albedo = 999.0;
                LiquidPrecip = 999.0;
            }
        } else {
            bool error = false;
            PrecipWater = UtilityRoutines::ProcessNumber(current_line.substr(0, pos), error);
            if (error) {
                ErrorInterpretWeatherDataLine(state, WYear, WMonth, WDay, WHour, WMinute, Line, current_line);
            }
            AerosolOptDepth = 999.0;
            SnowDepth = 999.0;
            DaysSinceLastSnow = 999.0;
            Albedo = 999.0;
            LiquidPrecip = 999.0;
        }

        WObs = nint(RField21);
        if (WObs == 0) { // Obs Indicator indicates Weather Codes valid
            // Check for miscellaneous characters
            pos = index(PresWeathCodes, '\'');
            while (pos != std::string::npos) {
                PresWeathCodes[pos] = ' ';
                pos = index(PresWeathCodes, '\'');
            }
            pos = index(PresWeathCodes, '"');
            while (pos != std::string::npos) {
                PresWeathCodes[pos] = ' ';
                pos = index(PresWeathCodes, '"');
            }
            strip(PresWeathCodes);
            if (len(PresWeathCodes) == 9) {
                for (pos = 0; pos < 9; ++pos) {
                    if (!has(ValidDigits, PresWeathCodes[pos])) PresWeathCodes[pos] = '9';
                }

                // we are trying to read a string of 9 integers with no spaces, each
                // into its own integer, like:
                // "123456789"
                // becomes
                // std::vector<int>{1,2,3,4,5,6,7,8,9};
                auto reader = stringReader(PresWeathCodes);
                for (auto &value : WCodesArr) {
                    char c[2]{};          // a string of 2 characters, init both to 0
                    reader >> c[0];       // read next char into the first byte
                    value = std::atoi(c); // convert this short string into the appropriate int to read
                }
            } else {
                ++state.dataWeatherManager->Missed.WeathCodes;
                WCodesArr = 9;
            }
        } else {
            WCodesArr = 9;
        }
    }

    void SetUpDesignDay(EnergyPlusData &state, int const EnvrnNum) // Environment number passed into the routine
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

        constexpr Real64 GlobalSolarConstant(1367.0);
        constexpr Real64 ZHGlobalSolarConstant(1355.0);

        Real64 constexpr ZhangHuangModCoeff_C0(0.5598);   // 37.6865d0
        Real64 constexpr ZhangHuangModCoeff_C1(0.4982);   // 13.9263d0
        Real64 constexpr ZhangHuangModCoeff_C2(-0.6762);  // -20.2354d0
        Real64 constexpr ZhangHuangModCoeff_C3(0.02842);  // 0.9695d0
        Real64 constexpr ZhangHuangModCoeff_C4(-0.00317); // -0.2046d0
        Real64 constexpr ZhangHuangModCoeff_C5(0.014);    // -0.0980d0
        Real64 constexpr ZhangHuangModCoeff_D(-17.853);   // -10.8568d0
        Real64 constexpr ZhangHuangModCoeff_K(0.843);     // 49.3112d0
        static constexpr std::string_view RoutineNamePsyWFnTdbTwbPb("SetUpDesignDay:PsyWFnTdbTwbPb");
        static constexpr std::string_view RoutineNamePsyWFnTdpPb("SetUpDesignDay:PsyWFnTdpPb");
        static constexpr std::string_view RoutineNamePsyWFnTdbH("SetUpDesignDay:PsyWFnTdbH");
        static constexpr std::string_view WeatherManager("WeatherManager");
        static constexpr std::string_view RoutineNameLong("WeatherManager.cc subroutine SetUpDesignDay");

        std::string StringOut;
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

        bool SaveWarmupFlag = state.dataGlobal->WarmupFlag;
        state.dataGlobal->WarmupFlag = true;

        Array1D_int Date0(8);
        date_and_time(_, _, _, Date0);
        int CurrentYear = Date0(1);

        if (state.dataGlobal->BeginSimFlag) {
            state.dataWeatherManager->PrintDDHeader = true;
        }

        state.dataWeatherManager->DesignDay(EnvrnNum).Year = CurrentYear; // f90 date_and_time implemented. full 4 digit year !+ 1900
        state.dataWeatherManager->DesignDay(EnvrnNum).Month = state.dataWeatherManager->DesDayInput(EnvrnNum).Month;
        state.dataWeatherManager->DesignDay(EnvrnNum).DayOfMonth = state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth;
        state.dataWeatherManager->DesignDay(EnvrnNum).DayOfYear =
            General::OrdinalDay(state.dataWeatherManager->DesignDay(EnvrnNum).Month, state.dataWeatherManager->DesignDay(EnvrnNum).DayOfMonth, 0);
        static constexpr std::string_view MnDyFmt("{:02}/{:02}");
        state.dataEnvrn->CurMnDy =
            format(MnDyFmt, state.dataWeatherManager->DesDayInput(EnvrnNum).Month, state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth);
        // EnvironmentName = DesDayInput( EnvrnNum ).Title;
        state.dataEnvrn->RunPeriodEnvironment = false;
        // Following builds Environment start/end for ASHRAE 55 warnings
        state.dataEnvrn->EnvironmentStartEnd = state.dataEnvrn->CurMnDy + " - " + state.dataEnvrn->CurMnDy;

        // Check that barometric pressure is within range
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).PressureEntered) {
            if (std::abs((state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom - state.dataEnvrn->StdBaroPress) /
                         state.dataEnvrn->StdBaroPress) > 0.1) { // 10% off
                ShowWarningError(state,
                                 format("SetUpDesignDay: Entered DesignDay Barometric Pressure={:.0R} differs by more than 10% from Standard "
                                        "Barometric Pressure={:.0R}.",
                                        state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom,
                                        state.dataEnvrn->StdBaroPress));
                ShowContinueError(
                    state, "...occurs in DesignDay=" + state.dataEnvrn->EnvironmentName + ", Standard Pressure (based on elevation) will be used.");
                state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom = state.dataEnvrn->StdBaroPress;
            }
        } else {
            state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom = state.dataEnvrn->StdBaroPress;
        }

        // verify that design WB or DP <= design DB
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::DewPoint &&
            state.dataWeatherManager->DesDayInput(EnvrnNum).DewPointNeedsSet) {
            // dew-point
            Real64 testval = Psychrometrics::PsyWFnTdbRhPb(
                state, state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb, 1.0, state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
            state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                Psychrometrics::PsyTdpFnWPb(state, testval, state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
        }

        // Day of week defaults to Monday, if day type specified, then that is used.
        state.dataWeatherManager->DesignDay(EnvrnNum).DayOfWeek = 2;
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).DayType <= 7)
            state.dataWeatherManager->DesignDay(EnvrnNum).DayOfWeek = state.dataWeatherManager->DesDayInput(EnvrnNum).DayType;

        // set Holiday as indicated by user input
        state.dataWeatherManager->DesignDay(EnvrnNum).HolidayIndex = 0;
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).DayType > 7)
            state.dataWeatherManager->DesignDay(EnvrnNum).HolidayIndex = state.dataWeatherManager->DesDayInput(EnvrnNum).DayType - 7;

        state.dataWeatherManager->DesignDay(EnvrnNum).DaylightSavingIndex = state.dataWeatherManager->DesDayInput(EnvrnNum).DSTIndicator;

        //  Set up Solar parameters for day
        Real64 A;    // Apparent solar irradiation at air mass = 0
        Real64 B;    // Atmospheric extinction coefficient
        Real64 C;    // ASHRAE diffuse radiation factor
        Real64 AVSC; // Annual variation in the solar constant
        CalculateDailySolarCoeffs(state,
                                  state.dataWeatherManager->DesignDay(EnvrnNum).DayOfYear,
                                  A,
                                  B,
                                  C,
                                  AVSC,
                                  state.dataWeatherManager->DesignDay(EnvrnNum).EquationOfTime,
                                  state.dataWeatherManager->DesignDay(EnvrnNum).SinSolarDeclinAngle,
                                  state.dataWeatherManager->DesignDay(EnvrnNum).CosSolarDeclinAngle);

        if (state.dataWeatherManager->PrintDDHeader && state.dataReportFlag->DoWeatherInitReporting) {
            static constexpr std::string_view EnvDDHdFormat(
                "! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, Temp Range {dC}, Temp Range Ind Type, "
                "Hum Ind Type, Hum Ind Value at Max Temp, Hum Ind Units, Pressure {Pa}, Wind Direction {deg CW from N}, Wind "
                "Speed {m/s}, Clearness, Rain, Snow");
            print(state.files.eio, "{}\n", EnvDDHdFormat);
            static constexpr std::string_view DDayMiscHdFormat(
                "! <Environment:Design Day Misc>,DayOfYear,ASHRAE A Coeff,ASHRAE B Coeff,ASHRAE C Coeff,Solar "
                "Constant-Annual Variation,Eq of Time {minutes}, Solar Declination Angle {deg}, Solar Model");
            print(state.files.eio, "{}\n", DDayMiscHdFormat);
            state.dataWeatherManager->PrintDDHeader = false;
        }
        if (state.dataReportFlag->DoWeatherInitReporting) {
            std::string const AlpUseRain = (state.dataWeatherManager->DesDayInput(EnvrnNum).RainInd == 1) ? "Yes" : "No";
            std::string const AlpUseSnow = (state.dataWeatherManager->DesDayInput(EnvrnNum).SnowInd == 1) ? "Yes" : "No";
            print(state.files.eio, "Environment:Design Day Data,");
            print(state.files.eio, "{:.2R},", state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb);
            print(state.files.eio, "{:.2R},", state.dataWeatherManager->DesDayInput(EnvrnNum).DailyDBRange);

            StringOut = ",";
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType::Default) {
                StringOut = "DefaultMultipliers,";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType::Multiplier) {
                StringOut = "MultiplierSchedule,";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType::Profile) {
                StringOut = "TemperatureProfile,";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType::Difference) {
                StringOut = "DifferenceSchedule,";
            }
            print(state.files.eio, "{}", StringOut);

            // Hum Ind Type, Hum Ind Value at Max Temp, Hum Ind Units
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WetBulb) {
                StringOut = format("Wetbulb,{:.2R},{{C}},", state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue);
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::DewPoint) {
                StringOut = format("Dewpoint,{:.2R},{{C}},", state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue);
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::Enthalpy) {
                StringOut = format("Enthalpy,{:.2R},{{J/kgDryAir}},", state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue);
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::HumRatio) {
                StringOut = format("HumidityRatio,{:.4R},{{kgWater/kgDryAir}},", state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue);
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::RelHumSch) {
                StringOut = "Schedule,<schedule values from 0.0 to 100.0>,{percent},";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDef) {
                StringOut = format("WetBulbProfileDefaultMultipliers,{:.2R},{{C}},",
                                   state.dataWeatherManager->DesDayInput(state.dataWeatherManager->Envrn).HumIndValue);
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDif) {
                StringOut = format("WetBulbProfileDifferenceSchedule,{:.2R},{{C}},", state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue);
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfMul) {
                StringOut = format("WetBulbProfileMultiplierSchedule,{:.2R},{{C}},", state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue);
            }
            print(state.files.eio, "{}", StringOut);
            print(state.files.eio, "{:.0R},", state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
            print(state.files.eio, "{:.0R},", state.dataWeatherManager->DesDayInput(EnvrnNum).WindDir);
            print(state.files.eio, "{:.1R},", state.dataWeatherManager->DesDayInput(EnvrnNum).WindSpeed);
            print(state.files.eio, "{:.2R},", state.dataWeatherManager->DesDayInput(EnvrnNum).SkyClear);

            print(state.files.eio, "{},{}\n", AlpUseRain, AlpUseSnow);

            static constexpr std::string_view DDayMiscFormat("Environment:Design Day Misc,{:3},");
            print(state.files.eio, DDayMiscFormat, state.dataWeatherManager->DesignDay(EnvrnNum).DayOfYear);
            print(state.files.eio, "{:.1R},", A);
            print(state.files.eio, "{:.4R},", B);
            print(state.files.eio, "{:.4R},", C);
            print(state.files.eio, "{:.1R},", AVSC);
            print(state.files.eio, "{:.2R},", state.dataWeatherManager->DesignDay(EnvrnNum).EquationOfTime * 60.0);
            print(state.files.eio,
                  "{:.1R},",
                  std::asin(state.dataWeatherManager->DesignDay(EnvrnNum).SinSolarDeclinAngle) / DataGlobalConstants::DegToRadians);

            if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::ASHRAE_ClearSky) {
                StringOut = "ASHRAEClearSky";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::Zhang_Huang) {
                StringOut = "ZhangHuang";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::SolarModel_Schedule) {
                StringOut = "User supplied beam/diffuse from schedules";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::ASHRAE_Tau) {
                StringOut = "ASHRAETau";
            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::ASHRAE_Tau2017) {
                StringOut = "ASHRAETau2017";
            } else {
                StringOut = "unknown";
            }
            print(state.files.eio, "{}\n", StringOut);
        }

        // Must set up weather values for Design Day.  User can specify the "humidity indicator" as
        // Wetbulb, DewPoint or input the relative humidity schedule.  For both wetbulb and dewpoint indicators, the
        // humidity for the day will be constant, using the drybulb (max) and humidity indicator temperature to
        // set the values.  For the scheduled values, these are already set in the DDxxx array.

        state.dataGlobal->CurrentTime = 25.0;
        Real64 HumidityRatio; // Humidity Ratio -- when constant for day
        bool ConstantHumidityRatio;

        switch (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType) {
        case DDHumIndType::WetBulb:
            HumidityRatio = Psychrometrics::PsyWFnTdbTwbPb(state,
                                                           state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb,
                                                           state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue,
                                                           state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom,
                                                           RoutineNamePsyWFnTdbTwbPb);
            ConstantHumidityRatio = true;
            break;
        case DDHumIndType::DewPoint:
            HumidityRatio = Psychrometrics::PsyWFnTdpPb(state,
                                                        state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue,
                                                        state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom,
                                                        RoutineNamePsyWFnTdpPb);
            ConstantHumidityRatio = true;
            break;
        case DDHumIndType::HumRatio:
            HumidityRatio = state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue;
            ConstantHumidityRatio = true;
            break;
        case DDHumIndType::Enthalpy:
            // HumIndValue is already in J/kg, so no conversions needed
            HumidityRatio = Psychrometrics::PsyWFnTdbH(state,
                                                       state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb,
                                                       state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue,
                                                       RoutineNamePsyWFnTdbH);
            ConstantHumidityRatio = true;
            break;
        case DDHumIndType::RelHumSch:
            // nothing to do -- DDHumIndModifier already contains the scheduled Relative Humidity
            ConstantHumidityRatio = false;
            state.dataWeatherManager->TomorrowOutRelHum = state.dataWeatherManager->DDHumIndModifier(_, _, EnvrnNum);
            break;
        case DDHumIndType::WBProfDef:
        case DDHumIndType::WBProfDif:
        case DDHumIndType::WBProfMul:
            ConstantHumidityRatio = false;
            break;
        default:
            ShowSevereError(state, "SetUpDesignDay: Invalid Humidity Indicator type");
            ShowContinueError(state, "Occurred in Design Day=" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title);
            break;
        }

        int OSky; // Opaque Sky Cover (tenths)
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).RainInd != 0) {
            state.dataWeatherManager->TomorrowIsRain(_, _) = true;
            OSky = 10;
            state.dataWeatherManager->TomorrowLiquidPrecip = 3.0;
        } else {
            state.dataWeatherManager->TomorrowIsRain(_, _) = false;
            OSky = 0;
            state.dataWeatherManager->TomorrowLiquidPrecip = 0.0;
        }

        Real64 GndReflet; // Ground Reflectivity
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).SnowInd == 0) {
            state.dataWeatherManager->TomorrowIsSnow(_, _) = false;
            GndReflet = 0.2;
        } else { // Snow
            state.dataWeatherManager->TomorrowIsSnow(_, _) = true;
            GndReflet = 0.7;
        }

        // Some values are constant

        state.dataWeatherManager->TomorrowOutBaroPress(_, _) = state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom;
        state.dataWeatherManager->TomorrowWindSpeed(_, _) = state.dataWeatherManager->DesDayInput(EnvrnNum).WindSpeed;
        state.dataWeatherManager->TomorrowWindDir(_, _) = state.dataWeatherManager->DesDayInput(EnvrnNum).WindDir;
        state.dataWeatherManager->TomorrowAlbedo = 0.0;

        // resolve daily ranges
        Real64 DBRange; // working copy of dry-bulb daily range, C (or 1 if input is difference)
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType::Difference) {
            DBRange = 1.0; // use unscaled multiplier values if difference
        } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType == DDDBRangeType::Profile) {
            DBRange = 0.0;
        } else {
            DBRange = state.dataWeatherManager->DesDayInput(EnvrnNum).DailyDBRange;
        }
        Real64 WBRange; // working copy of wet-bulb daily range. C (or 1 if input is difference)
        if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDif) {
            WBRange = 1.0; // use unscaled multiplier values if difference
        } else {
            WBRange = state.dataWeatherManager->DesDayInput(EnvrnNum).DailyWBRange;
        }

        for (int hour = 1; hour <= 24; ++hour) {
            for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {

                if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType != DDDBRangeType::Profile) {
                    // dry-bulb profile
                    state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour) =
                        state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb -
                        state.dataWeatherManager->DDDBRngModifier(ts, hour, EnvrnNum) * DBRange;
                } else { // DesDayInput(EnvrnNum)%DBTempRangeType == DDDBRangeType::Profile
                    state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour) = state.dataWeatherManager->DDDBRngModifier(ts, hour, EnvrnNum);
                }

                // wet-bulb - generate from profile, humidity ratio, or dew point
                if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDef ||
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDif ||
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfMul) {
                    Real64 WetBulb = state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue -
                                     state.dataWeatherManager->DDHumIndModifier(ts, hour, EnvrnNum) * WBRange;
                    WetBulb = min(WetBulb, state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour)); // WB must be <= DB
                    Real64 OutHumRat = Psychrometrics::PsyWFnTdbTwbPb(state,
                                                                      state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                                      WetBulb,
                                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
                    state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour) =
                        Psychrometrics::PsyTdpFnWPb(state, OutHumRat, state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
                    state.dataWeatherManager->TomorrowOutRelHum(ts, hour) =
                        Psychrometrics::PsyRhFnTdbWPb(state,
                                                      state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                      OutHumRat,
                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom,
                                                      WeatherManager) *
                        100.0;
                } else if (ConstantHumidityRatio) {
                    //  Need Dew Point Temperature.  Use Relative Humidity to get Humidity Ratio, unless Humidity Ratio is constant
                    // BG 9-26-07  moved following inside this IF statment; when HumIndType is 'Schedule' HumidityRatio wasn't being initialized
                    Real64 WetBulb = Psychrometrics::PsyTwbFnTdbWPb(state,
                                                                    state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                                    HumidityRatio,
                                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom,
                                                                    RoutineNameLong);

                    Real64 OutHumRat = Psychrometrics::PsyWFnTdpPb(state,
                                                                   state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                                   state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
                    if (HumidityRatio > OutHumRat) {
                        WetBulb = state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour);
                    } else {
                        OutHumRat = Psychrometrics::PsyWFnTdbTwbPb(state,
                                                                   state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                                   WetBulb,
                                                                   state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
                    }
                    state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour) =
                        Psychrometrics::PsyTdpFnWPb(state, OutHumRat, state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
                    state.dataWeatherManager->TomorrowOutRelHum(ts, hour) =
                        Psychrometrics::PsyRhFnTdbWPb(state,
                                                      state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                      OutHumRat,
                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom,
                                                      WeatherManager) *
                        100.0;
                } else {
                    HumidityRatio = Psychrometrics::PsyWFnTdbRhPb(state,
                                                                  state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                                  state.dataWeatherManager->DDHumIndModifier(ts, hour, EnvrnNum) / 100.0,
                                                                  state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
                    state.dataWeatherManager->TomorrowOutRelHum(ts, hour) =
                        Psychrometrics::PsyRhFnTdbWPb(state,
                                                      state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour),
                                                      HumidityRatio,
                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom,
                                                      WeatherManager) *
                        100.0;
                    // TomorrowOutRelHum values set earlier
                    state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour) =
                        Psychrometrics::PsyTdpFnWPb(state, HumidityRatio, state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom);
                }

                double DryBulb = state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour);
                double RelHum = state.dataWeatherManager->TomorrowOutRelHum(ts, hour) * 0.01;
                Real64 ESky = CalcSkyEmissivity(state,
                                                state.dataWeatherManager->Environment(EnvrnNum).SkyTempModel,
                                                OSky,
                                                DryBulb,
                                                state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour),
                                                RelHum); // Emissivitity of Sky
                state.dataWeatherManager->TomorrowHorizIRSky(ts, hour) =
                    ESky * state.dataWeatherManager->Sigma * pow_4(DryBulb + DataGlobalConstants::KelvinConv);

                if (state.dataWeatherManager->Environment(EnvrnNum).SkyTempModel == EmissivityCalcType::BruntModel ||
                    state.dataWeatherManager->Environment(EnvrnNum).SkyTempModel == EmissivityCalcType::IdsoModel ||
                    state.dataWeatherManager->Environment(EnvrnNum).SkyTempModel == EmissivityCalcType::BerdahlMartinModel ||
                    state.dataWeatherManager->Environment(EnvrnNum).SkyTempModel == EmissivityCalcType::SkyTAlgorithmA ||
                    state.dataWeatherManager->Environment(EnvrnNum).SkyTempModel == EmissivityCalcType::ClarkAllenModel) {
                    // Design day not scheduled
                    state.dataWeatherManager->TomorrowSkyTemp(ts, hour) =
                        (DryBulb + DataGlobalConstants::KelvinConv) * root_4(ESky) - DataGlobalConstants::KelvinConv;
                }
                // Generate solar values for timestep
                //    working results = BeamRad and DiffRad
                //    stored to program globals at end of loop
                Real64 BeamRad;
                Real64 DiffRad;
                if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::SolarModel_Schedule) {
                    // scheduled: set value unconditionally (whether sun up or not)
                    BeamRad = state.dataWeatherManager->DDBeamSolarValues(ts, hour, EnvrnNum);
                    DiffRad = state.dataWeatherManager->DDDiffuseSolarValues(ts, hour, EnvrnNum);
                } else {

                    // calc time = fractional hour of day
                    Real64 CurTime;
                    if (state.dataGlobal->NumOfTimeStepInHour != 1) {
                        CurTime = double(hour - 1) + double(ts) * state.dataWeatherManager->TimeStepFraction;
                    } else {
                        CurTime = double(hour) + state.dataEnvrn->TS1TimeOffset;
                    }

                    Array1D<Real64> SUNCOS(3); // Sun direction cosines
                    CalculateSunDirectionCosines(state,
                                                 CurTime,
                                                 state.dataWeatherManager->DesignDay(EnvrnNum).EquationOfTime,
                                                 state.dataWeatherManager->DesignDay(EnvrnNum).SinSolarDeclinAngle,
                                                 state.dataWeatherManager->DesignDay(EnvrnNum).CosSolarDeclinAngle,
                                                 SUNCOS);
                    Real64 CosZenith = SUNCOS(3); // Cosine of Zenith Angle of Sun
                    if (CosZenith < DataEnvironment::SunIsUpValue) {
                        BeamRad = 0.0;
                        DiffRad = 0.0;
                    } else {
                        Real64 SinSolarAltitude = SUNCOS(3);

                        switch (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel) {
                        case DesignDaySolarModel::ASHRAE_ClearSky: {
                            Real64 Exponent = B / CosZenith;
                            Real64 TotHoriz; // Total Radiation on Horizontal Surface
                            if (Exponent > 700.0) {
                                TotHoriz = 0.0;
                            } else {
                                TotHoriz = state.dataWeatherManager->DesDayInput(EnvrnNum).SkyClear * A * (C + CosZenith) * std::exp(-B / CosZenith);
                            }
                            // Radiation on an extraterrestial horizontal surface
                            Real64 HO = GlobalSolarConstant * AVSC * CosZenith;
                            Real64 KT = TotHoriz / HO; // Radiation ratio
                            KT = min(KT, 0.75);
                            DiffRad = TotHoriz * (1.0045 + KT * (0.04349 + KT * (-3.5227 + 2.6313 * KT)));
                            if (state.dataWeatherManager->DesDayInput(EnvrnNum).SkyClear > 0.70) DiffRad = TotHoriz * C / (C + CosZenith);
                            BeamRad = (TotHoriz - DiffRad) / CosZenith;
                            DiffRad = max(0.0, DiffRad);
                            BeamRad = max(0.0, BeamRad);

                        } break;
                        case DesignDaySolarModel::ASHRAE_Tau:
                        case DesignDaySolarModel::ASHRAE_Tau2017: {
                            Real64 ETR = GlobalSolarConstant * AVSC; // radiation of an extraterrestrial normal surface, W/m2
                            Real64 GloHorzRad;
                            ASHRAETauModel(state,
                                           state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel,
                                           ETR,
                                           CosZenith,
                                           state.dataWeatherManager->DesDayInput(EnvrnNum).TauB,
                                           state.dataWeatherManager->DesDayInput(EnvrnNum).TauD,
                                           BeamRad,
                                           DiffRad,
                                           GloHorzRad);
                        } break;
                        case DesignDaySolarModel::Zhang_Huang: {
                            int Hour3Ago = mod(hour + 20, 24) + 1; // hour 3 hours before
                            Real64 const TotSkyCover = max(1.0 - state.dataWeatherManager->DesDayInput(EnvrnNum).SkyClear, 0.0);
                            Real64 GloHorzRad =
                                (ZHGlobalSolarConstant * SinSolarAltitude *
                                     (ZhangHuangModCoeff_C0 + ZhangHuangModCoeff_C1 * TotSkyCover + ZhangHuangModCoeff_C2 * pow_2(TotSkyCover) +
                                      ZhangHuangModCoeff_C3 * (state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour) -
                                                               state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, Hour3Ago)) +
                                      ZhangHuangModCoeff_C4 * state.dataWeatherManager->TomorrowOutRelHum(ts, hour) +
                                      ZhangHuangModCoeff_C5 * state.dataWeatherManager->TomorrowWindSpeed(ts, hour)) +
                                 ZhangHuangModCoeff_D) /
                                ZhangHuangModCoeff_K;
                            GloHorzRad = max(GloHorzRad, 0.0);
                            Real64 ClearnessIndex_kt = GloHorzRad / (GlobalSolarConstant * SinSolarAltitude);
                            //          ClearnessIndex_kt=DesDayInput(EnvrnNum)%SkyClear
                            Real64 ClearnessIndex_ktc = 0.4268 + 0.1934 * SinSolarAltitude;
                            Real64 ClearnessIndex_kds;
                            if (ClearnessIndex_kt < ClearnessIndex_ktc) {
                                ClearnessIndex_kds = (3.996 - 3.862 * SinSolarAltitude + 1.54 * pow_2(SinSolarAltitude)) * pow_3(ClearnessIndex_kt);
                            } else {
                                ClearnessIndex_kds = ClearnessIndex_kt - (1.107 + 0.03569 * SinSolarAltitude + 1.681 * pow_2(SinSolarAltitude)) *
                                                                             pow_3(1.0 - ClearnessIndex_kt);
                            }
                            // Calculate direct normal radiation, W/m2
                            BeamRad = ZHGlobalSolarConstant * SinSolarAltitude * ClearnessIndex_kds *
                                      ((1.0 - ClearnessIndex_kt) / (1.0 - ClearnessIndex_kds));
                            // Calculation diffuse horizontal radiation, W/m2
                            DiffRad =
                                ZHGlobalSolarConstant * SinSolarAltitude * ((ClearnessIndex_kt - ClearnessIndex_kds) / (1.0 - ClearnessIndex_kds));

                        } break;
                        default:
                            break;
                        }
                    }
                }

                // override result to 0 per environment var (for testing)
                if (state.dataEnvrn->IgnoreSolarRadiation || state.dataEnvrn->IgnoreBeamRadiation) BeamRad = 0.0;
                if (state.dataEnvrn->IgnoreSolarRadiation || state.dataEnvrn->IgnoreDiffuseRadiation) DiffRad = 0.0;

                state.dataWeatherManager->TomorrowBeamSolarRad(ts, hour) = BeamRad;
                state.dataWeatherManager->TomorrowDifSolarRad(ts, hour) = DiffRad;

            } // Timestep (TS) Loop
        }     // Hour Loop

        // back-fill hour values from timesteps
        // hour values = integrated over hour ending at time of hour
        // insurance: hourly values not known to be needed
        for (int hour = 1; hour <= 24; ++hour) {
            int Hour1Ago = mod(hour + 22, 24) + 1;
            Real64 BeamRad = (state.dataWeatherManager->TomorrowBeamSolarRad(state.dataGlobal->NumOfTimeStepInHour, Hour1Ago) +
                              state.dataWeatherManager->TomorrowBeamSolarRad(state.dataGlobal->NumOfTimeStepInHour, hour)) /
                             2.0;
            Real64 DiffRad = (state.dataWeatherManager->TomorrowDifSolarRad(state.dataGlobal->NumOfTimeStepInHour, Hour1Ago) +
                              state.dataWeatherManager->TomorrowDifSolarRad(state.dataGlobal->NumOfTimeStepInHour, hour)) /
                             2.0;
            if (state.dataGlobal->NumOfTimeStepInHour > 1) {
                BeamRad += sum(state.dataWeatherManager->TomorrowBeamSolarRad({1, state.dataGlobal->NumOfTimeStepInHour - 1}, hour));
                DiffRad += sum(state.dataWeatherManager->TomorrowDifSolarRad({1, state.dataGlobal->NumOfTimeStepInHour - 1}, hour));
            }
            Wthr.BeamSolarRad(hour) = BeamRad / state.dataGlobal->NumOfTimeStepInHour;
            Wthr.DifSolarRad(hour) = DiffRad / state.dataGlobal->NumOfTimeStepInHour;
        }

        if (state.dataWeatherManager->Environment(EnvrnNum).WP_Type1 != 0) {

            switch (state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(EnvrnNum).WP_Type1).CalculationType) {
            case EmissivityCalcType::ScheduleValue:
                ScheduleManager::GetSingleDayScheduleValues(
                    state,
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(EnvrnNum).WP_Type1).SchedulePtr,
                    state.dataWeatherManager->TomorrowSkyTemp);
                state.dataWeatherManager->DDSkyTempScheduleValues(_, _, EnvrnNum) = state.dataWeatherManager->TomorrowSkyTemp;
                break;
            case EmissivityCalcType::DryBulbDelta:
                ScheduleManager::GetSingleDayScheduleValues(
                    state,
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(EnvrnNum).WP_Type1).SchedulePtr,
                    state.dataWeatherManager->TomorrowSkyTemp);
                state.dataWeatherManager->DDSkyTempScheduleValues(_, _, EnvrnNum) = state.dataWeatherManager->TomorrowSkyTemp;
                for (int hour = 1; hour <= 24; ++hour) {
                    for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {
                        state.dataWeatherManager->TomorrowSkyTemp(ts, hour) =
                            state.dataWeatherManager->TomorrowOutDryBulbTemp(ts, hour) - state.dataWeatherManager->TomorrowSkyTemp(ts, hour);
                    }
                }
                break;
            case EmissivityCalcType::DewPointDelta:
                ScheduleManager::GetSingleDayScheduleValues(
                    state,
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(EnvrnNum).WP_Type1).SchedulePtr,
                    state.dataWeatherManager->TomorrowSkyTemp);
                state.dataWeatherManager->DDSkyTempScheduleValues(_, _, EnvrnNum) = state.dataWeatherManager->TomorrowSkyTemp;
                for (int hour = 1; hour <= 24; ++hour) {
                    for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {
                        state.dataWeatherManager->TomorrowSkyTemp(ts, hour) =
                            state.dataWeatherManager->TomorrowOutDewPointTemp(ts, hour) - state.dataWeatherManager->TomorrowSkyTemp(ts, hour);
                    }
                }
                break;
            default:
                break;
            }
        }

        state.dataGlobal->WarmupFlag = SaveWarmupFlag;
    }

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

        Real64 AirMass;
        Real64 SunAltD;

        if (CosZen <= 0.001) {
            AirMass = 37.07837343; // limit value calc'd with Excel
                                   //  value increases little as CosZen -> 0
        } else if (CosZen >= 1.0) {
            AirMass = 1.0;
        } else {
            // note: COS( Zen) = SIN( Alt)
            SunAltD = std::asin(CosZen) / DataGlobalConstants::DegToRadians; // altitude, degrees
            AirMass = 1.0 / (CosZen + 0.50572 * std::pow(6.07995 + SunAltD, -1.6364));
        }
        return AirMass;
    }

    //------------------------------------------------------------------------------

    void ASHRAETauModel([[maybe_unused]] EnergyPlusData &state,
                        DesignDaySolarModel const TauModelType, // ASHRAETau solar model type ASHRAE_Tau or ASHRAE_Tau2017
                        Real64 const ETR,                       // extraterrestrial normal irradiance, W/m2
                        Real64 const CosZen,                    // COS( solar zenith angle), 0 - 1
                        Real64 const TauB,                      // beam tau factor
                        Real64 const TauD,                      // dif tau factor
                        Real64 &IDirN,                          // returned: direct (beam) irradiance on normal surface, W/m2
                        Real64 &IDifH,                          // returned: diffuse irradiance on horiz surface, W/m2
                        Real64 &IGlbH                           // returned: global irradiance on horiz surface, W/m2
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

        if (CosZen < DataEnvironment::SunIsUpValue || TauB <= 0.0 || TauD <= 0.0) {
            IDirN = 0.0;
            IDifH = 0.0;
            IGlbH = 0.0;
        } else {
            if (TauModelType == DesignDaySolarModel::ASHRAE_Tau) {
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

    void AllocateWeatherData(EnergyPlusData &state)
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

        state.dataWeatherManager->TodayIsRain.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayIsRain = false;
        state.dataWeatherManager->TodayIsSnow.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayIsSnow = false;
        state.dataWeatherManager->TodayOutDryBulbTemp.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayOutDryBulbTemp = 0.0;
        state.dataWeatherManager->TodayOutDewPointTemp.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayOutDewPointTemp = 0.0;
        state.dataWeatherManager->TodayOutBaroPress.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayOutBaroPress = 0.0;
        state.dataWeatherManager->TodayOutRelHum.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayOutRelHum = 0.0;
        state.dataWeatherManager->TodayWindSpeed.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayWindSpeed = 0.0;
        state.dataWeatherManager->TodayWindDir.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayWindDir = 0.0;
        state.dataWeatherManager->TodaySkyTemp.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodaySkyTemp = 0.0;
        state.dataWeatherManager->TodayHorizIRSky.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayHorizIRSky = 0.0;
        state.dataWeatherManager->TodayBeamSolarRad.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayBeamSolarRad = 0.0;
        state.dataWeatherManager->TodayDifSolarRad.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayDifSolarRad = 0.0;
        state.dataWeatherManager->TodayAlbedo.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayAlbedo = 0.0;
        state.dataWeatherManager->TodayLiquidPrecip.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayLiquidPrecip = 0.0;
        state.dataWeatherManager->TodayTotalSkyCover.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayTotalSkyCover = 0.0;
        state.dataWeatherManager->TodayOpaqueSkyCover.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TodayOpaqueSkyCover = 0.0;

        state.dataWeatherManager->TomorrowIsRain.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowIsRain = false;
        state.dataWeatherManager->TomorrowIsSnow.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowIsSnow = false;
        state.dataWeatherManager->TomorrowOutDryBulbTemp.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowOutDryBulbTemp = 0.0;
        state.dataWeatherManager->TomorrowOutDewPointTemp.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowOutDewPointTemp = 0.0;
        state.dataWeatherManager->TomorrowOutBaroPress.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowOutBaroPress = 0.0;
        state.dataWeatherManager->TomorrowOutRelHum.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowOutRelHum = 0.0;
        state.dataWeatherManager->TomorrowWindSpeed.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowWindSpeed = 0.0;
        state.dataWeatherManager->TomorrowWindDir.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowWindDir = 0.0;
        state.dataWeatherManager->TomorrowSkyTemp.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowSkyTemp = 0.0;
        state.dataWeatherManager->TomorrowHorizIRSky.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowHorizIRSky = 0.0;
        state.dataWeatherManager->TomorrowBeamSolarRad.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowBeamSolarRad = 0.0;
        state.dataWeatherManager->TomorrowDifSolarRad.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowDifSolarRad = 0.0;
        state.dataWeatherManager->TomorrowAlbedo.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowAlbedo = 0.0;
        state.dataWeatherManager->TomorrowLiquidPrecip.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowLiquidPrecip = 0.0;
        state.dataWeatherManager->TomorrowTotalSkyCover.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowTotalSkyCover = 0.0;
        state.dataWeatherManager->TomorrowOpaqueSkyCover.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
        state.dataWeatherManager->TomorrowOpaqueSkyCover = 0.0;
    }

    void CalculateDailySolarCoeffs(EnergyPlusData &state,
                                   int const DayOfYear,           // Day of year (1 - 366)
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

        Real64 const DayCorrection(DataGlobalConstants::Pi * 2.0 / 366.0);

        // Fitted coefficients of Fourier series | Sine of declination coefficients
        static constexpr std::array<Real64, 9> SineSolDeclCoef = {
            0.00561800, 0.0657911, -0.392779, 0.00064440, -0.00618495, -0.00010101, -0.00007951, -0.00011691, 0.00002096};
        // Fitted coefficients of Fourier Series | Equation of Time coefficients
        static constexpr std::array<Real64, 9> EqOfTimeCoef = {
            0.00021971, -0.122649, 0.00762856, -0.156308, -0.0530028, -0.00388702, -0.00123978, -0.00270502, -0.00167992};
        // Fitted coefficients of Fourier Series | ASHRAE A Factor coefficients
        static constexpr std::array<Real64, 9> ASHRAE_A_Coef = {1161.6685, 1.1554, 77.3575, -0.5359, -3.7622, 0.9875, -3.3924, -1.7445, 1.1198};
        // Fitted coefficients of Fourier Series | ASHRAE B Factor coefficients
        static constexpr std::array<Real64, 9> ASHRAE_B_Coef = {
            0.171631, -0.00400448, -0.0344923, 0.00000209, 0.00325428, -0.00085429, 0.00229562, 0.0009034, -0.0011867};
        // Fitted coefficients of Fourier Series | ASHRAE C Factor coefficients
        static constexpr std::array<Real64, 9> ASHRAE_C_Coef = {
            0.0905151, -0.00322522, -0.0407966, 0.000104164, 0.00745899, -0.00086461, 0.0013111, 0.000808275, -0.00170515};

        // Day of Year in Radians (Computed from Input DayOfYear)
        Real64 X = DayCorrection * DayOfYear; // Convert Julian date (Day of Year) to angle X

        // Calculate sines and cosines of X
        Real64 SinX = std::sin(X);
        Real64 CosX = std::cos(X);

        SineSolarDeclination = SineSolDeclCoef[0] + SineSolDeclCoef[1] * SinX + SineSolDeclCoef[2] * CosX + SineSolDeclCoef[3] * (SinX * CosX * 2.0) +
                               SineSolDeclCoef[4] * (pow_2(CosX) - pow_2(SinX)) +
                               SineSolDeclCoef[5] * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
                               SineSolDeclCoef[6] * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
                               SineSolDeclCoef[7] * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
                               SineSolDeclCoef[8] * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));
        CosineSolarDeclination = std::sqrt(1.0 - pow_2(SineSolarDeclination));

        EquationOfTime = EqOfTimeCoef[0] + EqOfTimeCoef[1] * SinX + EqOfTimeCoef[2] * CosX + EqOfTimeCoef[3] * (SinX * CosX * 2.0) +
                         EqOfTimeCoef[4] * (pow_2(CosX) - pow_2(SinX)) +
                         EqOfTimeCoef[5] * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
                         EqOfTimeCoef[6] * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
                         EqOfTimeCoef[7] * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
                         EqOfTimeCoef[8] * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));

        AnnVarSolConstant = 1.000047 + 0.000352615 * SinX + 0.0334454 * CosX;

        A = ASHRAE_A_Coef[0] + ASHRAE_A_Coef[1] * SinX + ASHRAE_A_Coef[2] * CosX + ASHRAE_A_Coef[3] * (SinX * CosX * 2.0) +
            ASHRAE_A_Coef[4] * (pow_2(CosX) - pow_2(SinX)) + ASHRAE_A_Coef[5] * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
            ASHRAE_A_Coef[6] * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
            ASHRAE_A_Coef[7] * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
            ASHRAE_A_Coef[8] * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));

        // Compute B and C coefficients

        if (state.dataEnvrn->Latitude < 0.0) {
            // If in southern hemisphere, compute B and C with a six month time shift.
            X -= DataGlobalConstants::Pi;
            SinX = std::sin(X);
            CosX = std::cos(X);
        }

        B = ASHRAE_B_Coef[0] + ASHRAE_B_Coef[1] * SinX + ASHRAE_B_Coef[2] * CosX + ASHRAE_B_Coef[3] * (SinX * CosX * 2.0) +
            ASHRAE_B_Coef[4] * (pow_2(CosX) - pow_2(SinX)) + ASHRAE_B_Coef[5] * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
            ASHRAE_B_Coef[6] * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
            ASHRAE_B_Coef[7] * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
            ASHRAE_B_Coef[8] * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));

        C = ASHRAE_C_Coef[0] + ASHRAE_C_Coef[1] * SinX + ASHRAE_C_Coef[2] * CosX + ASHRAE_C_Coef[3] * (SinX * CosX * 2.0) +
            ASHRAE_C_Coef[4] * (pow_2(CosX) - pow_2(SinX)) + ASHRAE_C_Coef[5] * (SinX * (pow_2(CosX) - pow_2(SinX)) + CosX * (SinX * CosX * 2.0)) +
            ASHRAE_C_Coef[6] * (CosX * (pow_2(CosX) - pow_2(SinX)) - SinX * (SinX * CosX * 2.0)) +
            ASHRAE_C_Coef[7] * (2.0 * (SinX * CosX * 2.0) * (pow_2(CosX) - pow_2(SinX))) +
            ASHRAE_C_Coef[8] * (pow_2(pow_2(CosX) - pow_2(SinX)) - pow_2(SinX * CosX * 2.0));
    }

    void CalculateSunDirectionCosines(EnergyPlusData &state,
                                      Real64 const TimeValue,    // Current Time of Day
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

        EP_SIZE_CHECK(SUNCOS, 3); // NOLINT(misc-static-assert)

        // COMPUTE THE HOUR ANGLE
        Real64 H = (15.0 * (12.0 - (TimeValue + EqOfTime)) + (state.dataEnvrn->TimeZoneMeridian - state.dataEnvrn->Longitude)) *
                   DataGlobalConstants::DegToRadians;
        Real64 COSH = std::cos(H);
        // COMPUTE THE COSINE OF THE SOLAR ZENITH ANGLE.
        // This is also the Sine of the Solar Altitude Angle

        SUNCOS(3) = SinSolDeclin * state.dataEnvrn->SinLatitude + CosSolDeclin * state.dataEnvrn->CosLatitude * COSH;

        if (SUNCOS(3) >= DataEnvironment::SunIsUpValue) { // If Sun above horizon, compute other direction cosines
            SUNCOS(2) = SinSolDeclin * state.dataEnvrn->CosLatitude - CosSolDeclin * state.dataEnvrn->SinLatitude * COSH;
            SUNCOS(1) = CosSolDeclin * std::sin(H);
        } else { // Sun is down, set to 0.0
            SUNCOS(1) = 0.0;
            SUNCOS(2) = 0.0;
        }
    }

    void DetermineSunUpDown(EnergyPlusData &state, Array1D<Real64> &SunDirectionCosines)
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

        EP_SIZE_CHECK(SunDirectionCosines, 3); // NOLINT(misc-static-assert)

        // COMPUTE THE HOUR ANGLE
        if (state.dataGlobal->NumOfTimeStepInHour != 1) {
            state.dataWeatherManager->HrAngle =
                (15.0 * (12.0 - (state.dataGlobal->CurrentTime + state.dataWeatherManager->TodayVariables.EquationOfTime)) +
                 (state.dataEnvrn->TimeZoneMeridian - state.dataEnvrn->Longitude));
        } else {
            state.dataWeatherManager->HrAngle = (15.0 * (12.0 - ((state.dataGlobal->CurrentTime + state.dataEnvrn->TS1TimeOffset) +
                                                                 state.dataWeatherManager->TodayVariables.EquationOfTime)) +
                                                 (state.dataEnvrn->TimeZoneMeridian - state.dataEnvrn->Longitude));
        }
        Real64 H = state.dataWeatherManager->HrAngle * DataGlobalConstants::DegToRadians;

        // Compute the Cosine of the Solar Zenith (Altitude) Angle.
        Real64 CosZenith = state.dataEnvrn->SinLatitude * state.dataWeatherManager->TodayVariables.SinSolarDeclinAngle +
                           state.dataEnvrn->CosLatitude * state.dataWeatherManager->TodayVariables.CosSolarDeclinAngle * std::cos(H);

        Real64 SolarZenith = std::acos(CosZenith);
        Real64 SinAltitude = state.dataEnvrn->CosLatitude * state.dataWeatherManager->TodayVariables.CosSolarDeclinAngle * std::cos(H) +
                             state.dataEnvrn->SinLatitude * state.dataWeatherManager->TodayVariables.SinSolarDeclinAngle;
        Real64 SolarAltitude = std::asin(SinAltitude);
        Real64 CosAzimuth = -(state.dataEnvrn->SinLatitude * CosZenith - state.dataWeatherManager->TodayVariables.SinSolarDeclinAngle) /
                            (state.dataEnvrn->CosLatitude * std::sin(SolarZenith));
        // Following because above can yield invalid cos value.  (e.g. at south pole)
        CosAzimuth = max(CosAzimuth, -1.0);
        CosAzimuth = min(1.0, CosAzimuth);
        Real64 SolarAzimuth = std::acos(CosAzimuth);

        state.dataWeatherManager->SolarAltitudeAngle = SolarAltitude / DataGlobalConstants::DegToRadians;
        state.dataWeatherManager->SolarAzimuthAngle = SolarAzimuth / DataGlobalConstants::DegToRadians;
        if (state.dataWeatherManager->HrAngle < 0.0) {
            state.dataWeatherManager->SolarAzimuthAngle = 360.0 - state.dataWeatherManager->SolarAzimuthAngle;
        }

        SunDirectionCosines(3) = CosZenith;
        if (CosZenith < DataEnvironment::SunIsUpValue) {
            state.dataEnvrn->SunIsUp = false;
            SunDirectionCosines(2) = 0.0;
            SunDirectionCosines(1) = 0.0;
        } else {
            state.dataEnvrn->SunIsUp = true;
            SunDirectionCosines(2) = state.dataWeatherManager->TodayVariables.SinSolarDeclinAngle * state.dataEnvrn->CosLatitude -
                                     state.dataWeatherManager->TodayVariables.CosSolarDeclinAngle * state.dataEnvrn->SinLatitude * std::cos(H);
            SunDirectionCosines(1) = state.dataWeatherManager->TodayVariables.CosSolarDeclinAngle * std::sin(H);
        }
    }

    void OpenWeatherFile(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   June 1999
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine checks to see if a weather file and what kind of weather file
        // exists in the working directory and calls appropriate routines to
        // open the files and set up for use.

        state.dataWeatherManager->WeatherFileExists = FileSystem::fileExists(state.files.inputWeatherFilePath.filePath);
        if (state.dataWeatherManager->WeatherFileExists) {
            OpenEPlusWeatherFile(state, ErrorsFound, true);
        }
    }

    void OpenEPlusWeatherFile(EnergyPlusData &state,
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

        static Array1D_string const Header(8,
                                           {"LOCATION",
                                            "DESIGN CONDITIONS",
                                            "TYPICAL/EXTREME PERIODS",
                                            "GROUND TEMPERATURES",
                                            "HOLIDAYS/DAYLIGHT SAVING",
                                            "COMMENTS 1",
                                            "COMMENTS 2",
                                            "DATA PERIODS"});

        state.files.inputWeatherFile.close();
        state.files.inputWeatherFile.filePath = state.files.inputWeatherFilePath.filePath;
        state.files.inputWeatherFile.open();
        if (!state.files.inputWeatherFile.good()) {
            ShowFatalError(state, "OpenWeatherFile: Could not OPEN EPW Weather File", OptionalOutputFileRef(state.files.eso));
        }

        if (ProcessHeader) {
            // Read in Header Information

            // Headers should come in order
            int HdLine = 1; // Look for first Header
            bool StillLooking = true;
            while (StillLooking) {
                auto Line = state.files.inputWeatherFile.readLine();
                if (Line.eof) {
                    ShowFatalError(
                        state,
                        "OpenWeatherFile: Unexpected End-of-File on EPW Weather file, while reading header information, looking for header=" +
                            Header(HdLine),
                        OptionalOutputFileRef(state.files.eso));
                }

                int endcol = len(Line.data);
                if (endcol > 0) {
                    if (int(Line.data[endcol - 1]) == DataSystemVariables::iUnicode_end) {
                        ShowSevereError(state,
                                        "OpenWeatherFile: EPW Weather File appears to be a Unicode or binary file.",
                                        OptionalOutputFileRef(state.files.eso));
                        ShowContinueError(state, "...This file cannot be read by this program. Please save as PC or Unix file and try again");
                        ShowFatalError(state, "Program terminates due to previous condition.");
                    }
                }
                std::string::size_type const Pos = FindNonSpace(Line.data);
                std::string::size_type const HdPos = index(Line.data, Header(HdLine));
                if (Pos != HdPos) continue;
                ProcessEPWHeader(state, Header(HdLine), Line.data, ErrorsFound);
                ++HdLine;
                if (HdLine == 9) StillLooking = false;
            }
        } else { // Header already processed, just read
            SkipEPlusWFHeader(state);
        }
    }

    void CloseWeatherFile(EnergyPlusData &state)
    {
        state.files.inputWeatherFile.close();
    }

    void ResolveLocationInformation(EnergyPlusData &state, bool &ErrorsFound) // Set to true if no location evident
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

        if (state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).KindOfEnvrn ==
                DataGlobalConstants::KindOfSim::RunPeriodWeather &&
            state.dataWeatherManager->WeatherFileExists) {
            if (state.dataWeatherManager->LocationGathered) {
                // See if "matching" location
                if (std::abs(state.dataEnvrn->Latitude - state.dataWeatherManager->WeatherFileLatitude) > 1.0 ||
                    std::abs(state.dataEnvrn->Longitude - state.dataWeatherManager->WeatherFileLongitude) > 1.0 ||
                    std::abs(state.dataEnvrn->TimeZoneNumber - state.dataWeatherManager->WeatherFileTimeZone) > 0.0 ||
                    std::abs(state.dataEnvrn->Elevation - state.dataWeatherManager->WeatherFileElevation) / max(state.dataEnvrn->Elevation, 1.0) >
                        0.10) {
                    ShowWarningError(state, "Weather file location will be used rather than entered (IDF) Location object.");
                    ShowContinueError(state, "..Location object=" + state.dataWeatherManager->LocationTitle);
                    ShowContinueError(state, "..Weather File Location=" + state.dataEnvrn->WeatherFileLocationTitle);
                    ShowContinueError(
                        state,
                        format("..due to location differences, Latitude difference=[{:.2R}] degrees, Longitude difference=[{:.2R}] degrees.",
                               std::abs(state.dataEnvrn->Latitude - state.dataWeatherManager->WeatherFileLatitude),
                               std::abs(state.dataEnvrn->Longitude - state.dataWeatherManager->WeatherFileLongitude)));
                    ShowContinueError(state,
                                      format("..Time Zone difference=[{:.1R}] hour(s), Elevation difference=[{:.2R}] percent, [{:.2R}] meters.",
                                             std::abs(state.dataEnvrn->TimeZoneNumber - state.dataWeatherManager->WeatherFileTimeZone),
                                             std::abs((state.dataEnvrn->Elevation - state.dataWeatherManager->WeatherFileElevation) /
                                                      max(state.dataEnvrn->Elevation, 1.0) * 100.0),
                                             std::abs(state.dataEnvrn->Elevation - state.dataWeatherManager->WeatherFileElevation)));
                }
            }

            state.dataWeatherManager->LocationTitle = state.dataEnvrn->WeatherFileLocationTitle;
            state.dataEnvrn->Latitude = state.dataWeatherManager->WeatherFileLatitude;
            state.dataEnvrn->Longitude = state.dataWeatherManager->WeatherFileLongitude;
            state.dataEnvrn->TimeZoneNumber = state.dataWeatherManager->WeatherFileTimeZone;
            state.dataEnvrn->Elevation = state.dataWeatherManager->WeatherFileElevation;
        } else if (!state.dataWeatherManager->LocationGathered) {
            state.dataWeatherManager->LocationTitle = "Not Entered";
            ShowSevereError(state, "No Location given. Must have location information for simulation.");
            ErrorsFound = true;
        }

        if (!ErrorsFound) {
            state.dataEnvrn->StdBaroPress = DataEnvironment::StdPressureSeaLevel * std::pow(1.0 - 2.25577e-05 * state.dataEnvrn->Elevation, 5.2559);
            state.dataEnvrn->StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->StdBaroPress, DataPrecisionGlobals::constant_twenty, DataPrecisionGlobals::constant_zero);
            // Write Final Location Information to the initialization output file
            static constexpr std::string_view LocHdFormat(
                "! <Site:Location>, Location Name, Latitude {N+/S- Deg}, Longitude {E+/W- Deg},  Time Zone Number "
                "{GMT+/-}, Elevation {m},  Standard Pressure at Elevation {Pa}, Standard RhoAir at Elevation\n");
            print(state.files.eio, "{}", LocHdFormat);

            static constexpr std::string_view LocFormat("Site:Location,{},{:.2R},{:.2R},{:.2R},{:.2R},{:.0R},{:.4R}\n");
            print(state.files.eio,
                  LocFormat,
                  state.dataWeatherManager->LocationTitle,
                  state.dataEnvrn->Latitude,
                  state.dataEnvrn->Longitude,
                  state.dataEnvrn->TimeZoneNumber,
                  state.dataEnvrn->Elevation,
                  state.dataEnvrn->StdBaroPress,
                  state.dataEnvrn->StdRhoAir);
        }
    }

    void CheckLocationValidity(EnergyPlusData &state)
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

        bool LocationError = false; // Set to true if there is a problem detected

        if ((state.dataEnvrn->Latitude == -999.0) && (state.dataEnvrn->Longitude == -999.0) && (state.dataEnvrn->TimeZoneNumber != -999.0)) {
            ShowSevereError(state, "No location specified");
            LocationError = true;
        }

        if ((state.dataEnvrn->Latitude < -90.0) || (state.dataEnvrn->Latitude > 90.0)) {
            ShowSevereError(state, format("Latitude must be between -90 and 90; Entered={:.2R}", state.dataEnvrn->Latitude));
            LocationError = true;
        }

        if ((state.dataEnvrn->Longitude < -180.0) || (state.dataEnvrn->Longitude > 180.0)) {
            ShowSevereError(state, format("Longitude must be between -180 and 180; Entered={:.2R}", state.dataEnvrn->Longitude));
            LocationError = true;
        }

        if ((state.dataEnvrn->TimeZoneNumber < -12.00) || (state.dataEnvrn->TimeZoneNumber > 14.00)) {
            ShowSevereError(state, format("Time Zone must be between -12 and +14; Entered={:.2R}", state.dataEnvrn->TimeZoneNumber));
            LocationError = true;
        }

        Real64 const StdTimeMerid = GetSTM(state.dataEnvrn->Longitude); // Standard time meridian.

        // Compare the standard time meridian with the time zone number.  If
        // different, notify the user.  If StdTimeMerid couldn't be calculated,
        // produce an error message.

        if (state.dataEnvrn->varyingLocationSchedIndexLat > 0 || state.dataEnvrn->varyingLocationSchedIndexLong > 0) {
            // don't do any warnings, the building is moving
        } else if (StdTimeMerid >= -12.0 && StdTimeMerid <= 12.0) {
            if (state.dataEnvrn->TimeZoneNumber != StdTimeMerid) {
                // Difference between Standard Time Meridian and TimeZone
                Real64 const DiffCalc = std::abs(state.dataEnvrn->TimeZoneNumber - StdTimeMerid);
                if (DiffCalc > 1.0 && DiffCalc < 24.0) {
                    if (DiffCalc < 3.0) {
                        ShowWarningError(state,
                                         format("Standard Time Meridian and Time Zone differ by more than 1, Difference=\"{:.1R}\"", DiffCalc));
                        ShowContinueError(state, "Solar Positions may be incorrect");
                    } else {
                        ShowSevereError(state, format("Standard Time Meridian and Time Zone differ by more than 2, Difference=\"{:.1R}\"", DiffCalc));
                        ShowContinueError(state, "Solar Positions will be incorrect");
                        //          LocationError=.TRUE.
                    }
                }
            }
        } else {
            ShowSevereError(state, "Unable to calculate the standard time meridian");
            LocationError = true;
        }

        // Error handling:  if there are any errors in the location information
        // the simulation must be terminated

        if (LocationError) {
            ShowFatalError(state, "Due to previous error condition, simulation terminated");
        }

        if (state.dataEnvrn->TimeZoneNumber <= 12.00) {
            state.dataEnvrn->TimeZoneMeridian = state.dataEnvrn->TimeZoneNumber * 15.0;
        } else {
            state.dataEnvrn->TimeZoneMeridian = state.dataEnvrn->TimeZoneNumber * 15.0 - 360.0;
        }
        state.dataEnvrn->SinLatitude = std::sin(DataGlobalConstants::DegToRadians * state.dataEnvrn->Latitude);
        state.dataEnvrn->CosLatitude = std::cos(DataGlobalConstants::DegToRadians * state.dataEnvrn->Latitude);

        if (state.dataEnvrn->Latitude == 0.0 && state.dataEnvrn->Longitude == 0.0 && state.dataEnvrn->TimeZoneNumber == 0.0) {
            ShowWarningError(state,
                             "Did you realize that you have Latitude=0.0, Longitude=0.0 and TimeZone=0.0?  Your building site is in the middle of "
                             "the Atlantic Ocean.");
        }
    }

    void CheckWeatherFileValidity(EnergyPlusData &state)
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

        if (!state.dataWeatherManager->WeatherFileExists) { // No weather file exists but the user requested one--print error message

            if (state.dataGlobal->DoWeathSim) {
                ShowWarningError(state, "Weather Environment(s) requested, but no weather file found");
            }

        } // ... end of WeatherFileExists IF-THEN
    }

    void ReportOutputFileHeaders(EnergyPlusData &state)
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

        static constexpr std::string_view EnvironmentString(",5,Environment Title[],Latitude[deg],Longitude[deg],Time Zone[],Elevation[m]");
        static constexpr std::string_view TimeStepString(
            ",8,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType");
        static constexpr std::string_view DailyString(
            ",5,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily ");
        static constexpr std::string_view MonthlyString(",2,Cumulative Days of Simulation[],Month[]  ! When Monthly ");
        static constexpr std::string_view RunPeriodString(",1,Cumulative Days of Simulation[] ! When Run Period ");
        static constexpr std::string_view YearlyString(",1,Calendar Year of Simulation[] ! When Annual ");

        AssignReportNumber(state, state.dataWeatherManager->EnvironmentReportNbr);
        if (state.dataWeatherManager->EnvironmentReportNbr != 1) { //  problem
            ShowFatalError(state, "ReportOutputFileHeaders: Assigned report number for Environment title is not 1.  Contact Support.");
        }
        state.dataWeatherManager->EnvironmentReportChr = fmt::to_string(state.dataWeatherManager->EnvironmentReportNbr);
        strip(state.dataWeatherManager->EnvironmentReportChr);
        print(state.files.eso, "{}{}\n", state.dataWeatherManager->EnvironmentReportChr, EnvironmentString);
        print(state.files.mtr, "{}{}\n", state.dataWeatherManager->EnvironmentReportChr, EnvironmentString);

        AssignReportNumber(state, state.dataOutputProcessor->TimeStepStampReportNbr);
        state.dataOutputProcessor->TimeStepStampReportChr = fmt::to_string(state.dataOutputProcessor->TimeStepStampReportNbr);
        strip(state.dataOutputProcessor->TimeStepStampReportChr);
        print(state.files.eso, "{}{}\n", state.dataOutputProcessor->TimeStepStampReportChr, TimeStepString);
        print(state.files.mtr, "{}{}\n", state.dataOutputProcessor->TimeStepStampReportChr, TimeStepString);

        AssignReportNumber(state, state.dataOutputProcessor->DailyStampReportNbr);
        state.dataOutputProcessor->DailyStampReportChr = fmt::to_string(state.dataOutputProcessor->DailyStampReportNbr);
        strip(state.dataOutputProcessor->DailyStampReportChr);
        print(state.files.eso, "{}{}{}\n", state.dataOutputProcessor->DailyStampReportChr, DailyString, "Report Variables Requested");
        print(state.files.mtr, "{}{}{}\n", state.dataOutputProcessor->DailyStampReportChr, DailyString, "Meters Requested");

        AssignReportNumber(state, state.dataOutputProcessor->MonthlyStampReportNbr);
        state.dataOutputProcessor->MonthlyStampReportChr = fmt::to_string(state.dataOutputProcessor->MonthlyStampReportNbr);
        strip(state.dataOutputProcessor->MonthlyStampReportChr);
        print(state.files.eso, "{}{}{}\n", state.dataOutputProcessor->MonthlyStampReportChr, MonthlyString, "Report Variables Requested");
        print(state.files.mtr, "{}{}{}\n", state.dataOutputProcessor->MonthlyStampReportChr, MonthlyString, "Meters Requested");

        AssignReportNumber(state, state.dataOutputProcessor->RunPeriodStampReportNbr);
        state.dataOutputProcessor->RunPeriodStampReportChr = fmt::to_string(state.dataOutputProcessor->RunPeriodStampReportNbr);
        strip(state.dataOutputProcessor->RunPeriodStampReportChr);
        print(state.files.eso, "{}{}{}\n", state.dataOutputProcessor->RunPeriodStampReportChr, RunPeriodString, "Report Variables Requested");
        print(state.files.mtr, "{}{}{}\n", state.dataOutputProcessor->RunPeriodStampReportChr, RunPeriodString, "Meters Requested");

        AssignReportNumber(state, state.dataOutputProcessor->YearlyStampReportNbr);
        state.dataOutputProcessor->YearlyStampReportChr = fmt::to_string(state.dataOutputProcessor->YearlyStampReportNbr);
        strip(state.dataOutputProcessor->YearlyStampReportChr);
        print(state.files.eso, "{}{}{}\n", state.dataOutputProcessor->YearlyStampReportChr, YearlyString, "Report Variables Requested");
        print(state.files.mtr, "{}{}{}\n", state.dataOutputProcessor->YearlyStampReportChr, YearlyString, "Meters Requested");
    }

    void ReportWeatherAndTimeInformation(EnergyPlusData &state, bool &printEnvrnStamp) // Set to true when the environment header should be printed
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

        // Report the time stamp and the current weather to the output file

        if (!state.dataGlobal->WarmupFlag && !state.dataWeatherManager->RPReadAllWeatherData) { // Write the required output information

            // The first time through in a non-warmup day, the environment header
            // must be printed.  This must be done here and not in the generic
            // DataGlobals::BeginEnvrnFlag block above because other modules in the simulation
            // must also print out header information.  This can be done during
            // the simulation warmup if the environment stamp printing is delayed
            // until the warmup is completed.  The stamp should only be printed once
            // per environment (set/reset of PrintEnvrnStamp).  In addition, before
            // the first environment, the end of the header block flag must also be
            // sent to the output file.

            if (printEnvrnStamp) {

                if (state.dataReportFlag->PrintEndDataDictionary && state.dataGlobal->DoOutputReporting) {
                    static constexpr std::string_view EndOfHeaderString("End of Data Dictionary"); // End of data dictionary marker
                    print(state.files.eso, "{}\n", EndOfHeaderString);
                    print(state.files.mtr, "{}\n", EndOfHeaderString);
                    state.dataReportFlag->PrintEndDataDictionary = false;
                }
                if (state.dataGlobal->DoOutputReporting) {
                    std::string const &Title(state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).Title);
                    static constexpr std::string_view EnvironmentStampFormatStr(
                        "{},{},{:7.2F},{:7.2F},{:7.2F},{:7.2F}\n"); // Format descriptor for environ stamp
                    print(state.files.eso,
                          EnvironmentStampFormatStr,
                          state.dataWeatherManager->EnvironmentReportChr,
                          Title,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);
                    print(state.files.mtr,
                          EnvironmentStampFormatStr,
                          state.dataWeatherManager->EnvironmentReportChr,
                          Title,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);
                    printEnvrnStamp = false;
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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false);

        // Get the number of design days and annual runs from user inpout
        state.dataEnvrn->TotDesDays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:DesignDay");
        int RPD1 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileDays");
        int RPD2 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileConditionType");
        state.dataWeatherManager->TotRunPers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "RunPeriod");
        state.dataWeatherManager->NumOfEnvrn = state.dataEnvrn->TotDesDays + state.dataWeatherManager->TotRunPers + RPD1 + RPD2;
        state.dataGlobal->WeathSimReq = state.dataWeatherManager->TotRunPers > 0;

        state.dataWeatherManager->SPSiteScheduleNamePtr.allocate(state.dataEnvrn->TotDesDays * 5);
        state.dataWeatherManager->SPSiteScheduleUnits.allocate(state.dataEnvrn->TotDesDays * 5);

        state.dataWeatherManager->SPSiteScheduleNamePtr = 0;
        state.dataWeatherManager->SPSiteScheduleUnits = "";

        // Allocate the Design Day and Environment array to the # of DD's or/and
        // Annual runs on input file
        state.dataWeatherManager->DesignDay.allocate(state.dataEnvrn->TotDesDays);
        state.dataWeatherManager->Environment.allocate(state.dataWeatherManager->NumOfEnvrn);

        // Set all Environments to DesignDay and then the weather environment will be set
        //  in the get annual run data subroutine
        for (int Env = 1; Env <= state.dataEnvrn->TotDesDays; ++Env) {
            state.dataWeatherManager->Environment(Env).KindOfEnvrn = DataGlobalConstants::KindOfSim::DesignDay;
        }
        for (int Env = 1; Env <= RPD1 + RPD2; ++Env) {
            if (!state.dataSysVars->DDOnly) {
                state.dataWeatherManager->Environment(state.dataEnvrn->TotDesDays + Env).KindOfEnvrn =
                    DataGlobalConstants::KindOfSim::RunPeriodDesign;
            } else {
                state.dataWeatherManager->Environment(state.dataEnvrn->TotDesDays + Env).KindOfEnvrn =
                    DataGlobalConstants::KindOfSim::RunPeriodWeather;
            }
        }
        for (int Env = 1; Env <= state.dataWeatherManager->TotRunPers; ++Env) {
            state.dataWeatherManager->Environment(state.dataEnvrn->TotDesDays + RPD1 + RPD2 + Env).KindOfEnvrn =
                DataGlobalConstants::KindOfSim::RunPeriodWeather;
        }

        if (state.dataEnvrn->TotDesDays >= 1) {
            GetDesignDayData(state, state.dataEnvrn->TotDesDays, ErrorsFound);
        }

        if (RPD1 >= 1 || RPD2 >= 1) {
            GetRunPeriodDesignData(state, ErrorsFound);
        }

        // the last environment(s) is designated the weather environment if an annual run
        // is selected.  All of the design systems is done from the design day info
        // which will have to be completed to run the annual run.
        if (state.dataWeatherManager->TotRunPers >= 1 || state.dataSysVars->FullAnnualRun) {
            GetRunPeriodData(state, state.dataWeatherManager->TotRunPers, ErrorsFound);
        }

        if (state.dataSysVars->FullAnnualRun) {
            // GetRunPeriodData may have reset the value of TotRunPers
            state.dataWeatherManager->NumOfEnvrn = state.dataEnvrn->TotDesDays + state.dataWeatherManager->TotRunPers + RPD1 + RPD2;
        }

        if (RPD1 >= 1 || RPD2 >= 1 || state.dataWeatherManager->TotRunPers >= 1 || state.dataSysVars->FullAnnualRun) {
            GetSpecialDayPeriodData(state, ErrorsFound);
            GetDSTData(state, ErrorsFound);
            if (state.dataWeatherManager->IDFDaylightSaving) {
                state.dataWeatherManager->DST = state.dataWeatherManager->IDFDST;
            }
        }

        GetLocationInfo(state, ErrorsFound);

        GetGroundTemps(state, ErrorsFound);

        GetGroundReflectances(state, ErrorsFound);

        GetSnowGroundRefModifiers(state, ErrorsFound);

        GetWaterMainsTemperatures(state, ErrorsFound);

        GetWeatherStation(state, ErrorsFound);

        SetupEnvironmentTypes(state);

        GetWeatherProperties(state, ErrorsFound);

        // Deallocate ones used for schedule pointers
        state.dataWeatherManager->SPSiteScheduleNamePtr.deallocate();
        state.dataWeatherManager->SPSiteScheduleUnits.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, "GetWeatherInput: Above errors cause termination");
        }
    }

    static int findYearForWeekday(int const month, int const day, WeekDay const weekday)
    {
        // Find a year that goes with a month/day and a weekday. A lookup table is used with the most recent year that includes
        // the date with the weekday specified.

        // Tu, W, Th, F, Sa, Su, M, Tu, W, Th, F, Sa, Su
        static std::array<int, 13> const defaultYear{{2013, 2014, 2015, 2010, 2011, 2017, 2007, 2013, 2014, 2015, 2010, 2011, 2017}};

        int rem = calculateDayOfYear(month, day) % 7;
        return defaultYear[static_cast<int>(weekday) - rem + 5]; // static_cast<int>(weekday) - rem + 1 + 4
    }

    static int findLeapYearForWeekday(int const month, int const day, WeekDay const weekday)
    {
        // Find a leap year that goes with a month/day and a weekday. A lookup table is used with the most recent year that includes
        // the date with the weekday specified.

        // Tu, W, Th, F, Sa, Su, M, Tu, W, Th, F, Sa, Su
        static std::array<int, 13> const defaultLeapYear{{2008, 1992, 2004, 2016, 2000, 2012, 1996, 2008, 1992, 2004, 2016, 2000, 2012}};

        int rem = calculateDayOfYear(month, day, true) % 7;
        return defaultLeapYear[static_cast<int>(weekday) - rem + 5]; // static_cast<int>(weekday) - rem + 1 + 4
    }

    void GetRunPeriodData(EnergyPlusData &state,
                          int &nRunPeriods, // Total number of Run Periods requested
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

        // Call Input Get routine to retrieve annual run data
        state.dataWeatherManager->RunPeriodInput.allocate(nRunPeriods);
        state.dataWeatherManager->RunPeriodInputUniqueNames.reserve(static_cast<unsigned>(nRunPeriods));

        state.dataIPShortCut->cCurrentModuleObject = "RunPeriod";
        int Count = 0;
        int NumAlpha;   // Number of alphas being input
        int NumNumeric; // Number of numbers being input
        int IOStat;     // IO Status when calling get input subroutine
        for (int i = 1; i <= nRunPeriods; ++i) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     i,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlpha,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumeric,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            // A1, \field Name
            if (!state.dataIPShortCut->lAlphaFieldBlanks(1)) {
                GlobalNames::VerifyUniqueInterObjectName(state,
                                                         state.dataWeatherManager->RunPeriodInputUniqueNames,
                                                         state.dataIPShortCut->cAlphaArgs(1),
                                                         state.dataIPShortCut->cCurrentModuleObject,
                                                         state.dataIPShortCut->cAlphaFieldNames(1),
                                                         ErrorsFound);
            }

            ++Count;
            // Loop = RP + Ptr;
            // Note JM 2018-11-20: IDD allows blank name, but input processor will create a name such as "RUNPERIOD 1" anyways
            // which is fine for our reporting below
            state.dataWeatherManager->RunPeriodInput(i).title = state.dataIPShortCut->cAlphaArgs(1);

            // set the start and end day of month from user input
            // N1 , \field Begin Month
            // N2 , \field Begin Day of Month
            // N3,  \field Start Year
            // N4 , \field End Month
            // N5 , \field End Day of Month
            // N6,  \field End Year
            state.dataWeatherManager->RunPeriodInput(i).startMonth = int(state.dataIPShortCut->rNumericArgs(1));
            state.dataWeatherManager->RunPeriodInput(i).startDay = int(state.dataIPShortCut->rNumericArgs(2));
            state.dataWeatherManager->RunPeriodInput(i).startYear = int(state.dataIPShortCut->rNumericArgs(3));
            state.dataWeatherManager->RunPeriodInput(i).endMonth = int(state.dataIPShortCut->rNumericArgs(4));
            state.dataWeatherManager->RunPeriodInput(i).endDay = int(state.dataIPShortCut->rNumericArgs(5));
            state.dataWeatherManager->RunPeriodInput(i).endYear = int(state.dataIPShortCut->rNumericArgs(6));
            state.dataWeatherManager->RunPeriodInput(i).TreatYearsAsConsecutive = true;

            if (state.dataSysVars->FullAnnualRun && i == 1) {
                state.dataWeatherManager->RunPeriodInput(i).startMonth = 1;
                state.dataWeatherManager->RunPeriodInput(i).startDay = 1;
                state.dataWeatherManager->RunPeriodInput(i).endMonth = 12;
                state.dataWeatherManager->RunPeriodInput(i).endDay = 31;
            }

            // Validate year inputs
            if (state.dataWeatherManager->RunPeriodInput(i).startYear == 0) {
                if (state.dataWeatherManager->RunPeriodInput(i).endYear != 0) { // Have to have an input start year to input an end year
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                        ", end year cannot be specified if the start year is not.");
                    ErrorsFound = true;
                }
            } else if (state.dataWeatherManager->RunPeriodInput(i).startYear < 1583) { // Bail on the proleptic Gregorian calendar
                ShowSevereError(state,
                                format("{}: object={}, start year ({}) is too early, please choose a date after 1582.",
                                       state.dataIPShortCut->cCurrentModuleObject,
                                       state.dataWeatherManager->RunPeriodInput(i).title,
                                       state.dataWeatherManager->RunPeriodInput(i).startYear));
                ErrorsFound = true;
            }

            if (state.dataWeatherManager->RunPeriodInput(i).endYear != 0 &&
                state.dataWeatherManager->RunPeriodInput(i).startYear > state.dataWeatherManager->RunPeriodInput(i).endYear) {
                ShowSevereError(state,
                                format("{}: object={}, start year ({}) is after the end year ({}).",
                                       state.dataIPShortCut->cCurrentModuleObject,
                                       state.dataWeatherManager->RunPeriodInput(i).title,
                                       state.dataWeatherManager->RunPeriodInput(i).startYear,
                                       state.dataWeatherManager->RunPeriodInput(i).endYear));
                ErrorsFound = true;
            }

            // A2 , \field Day of Week for Start Day
            bool inputWeekday = false;
            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) { // Have input
                auto result = weekDayLookUp.find(state.dataIPShortCut->cAlphaArgs(2));
                if (result == weekDayLookUp.end()) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                         state.dataIPShortCut->cAlphaFieldNames(2) + " invalid (Day of Week) [" +
                                         state.dataIPShortCut->cAlphaArgs(2) + "] for Start is not valid, Sunday will be used.");
                    state.dataWeatherManager->RunPeriodInput(i).startWeekDay = WeekDay::Sunday;
                } else {
                    state.dataWeatherManager->RunPeriodInput(i).startWeekDay = result->second;
                    inputWeekday = true;
                }
            } else { // No input, set the default as Sunday. This may get overriden below
                state.dataWeatherManager->RunPeriodInput(i).startWeekDay = WeekDay::Sunday;
            }

            // Validate the dates now that the weekday field has been looked at
            if (state.dataWeatherManager->RunPeriodInput(i).startMonth == 2 && state.dataWeatherManager->RunPeriodInput(i).startDay == 29) {
                // Requested start date is a leap year
                if (state.dataWeatherManager->RunPeriodInput(i).startYear == 0) { // No input starting year
                    if (inputWeekday) {
                        state.dataWeatherManager->RunPeriodInput(i).startYear =
                            findLeapYearForWeekday(state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                                   state.dataWeatherManager->RunPeriodInput(i).startDay,
                                                   state.dataWeatherManager->RunPeriodInput(i).startWeekDay);
                    } else {
                        // 2012 is the default year, 1/1 is a Sunday
                        state.dataWeatherManager->RunPeriodInput(i).startYear = 2012;
                        state.dataWeatherManager->RunPeriodInput(i).startWeekDay =
                            calculateDayOfWeek(state,
                                               state.dataWeatherManager->RunPeriodInput(i).startYear,
                                               state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                               state.dataWeatherManager->RunPeriodInput(i).startDay);
                    }
                } else {                                                                      // Have an input start year
                    if (!isLeapYear(state.dataWeatherManager->RunPeriodInput(i).startYear)) { // Start year is not a leap year
                        ShowSevereError(state,
                                        format("{}: object={}, start year ({}) is not a leap year but the requested start date is 2/29.",
                                               state.dataIPShortCut->cCurrentModuleObject,
                                               state.dataWeatherManager->RunPeriodInput(i).title,
                                               state.dataWeatherManager->RunPeriodInput(i).startYear));
                        ErrorsFound = true;
                    } else { // Start year is a leap year
                        WeekDay weekday = calculateDayOfWeek(state,
                                                             state.dataWeatherManager->RunPeriodInput(i).startYear,
                                                             state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                                             state.dataWeatherManager->RunPeriodInput(i).startDay);
                        if (inputWeekday) { // Check for correctness of input
                            if (weekday != state.dataWeatherManager->RunPeriodInput(i).startWeekDay) {
                                ShowWarningError(state,
                                                 format("{}: object={}, start weekday ({}) does not match the start year ({}), corrected to {}.",
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataWeatherManager->RunPeriodInput(i).title,
                                                        state.dataIPShortCut->cAlphaArgs(2),
                                                        state.dataWeatherManager->RunPeriodInput(i).startYear,
                                                        DaysOfWeek(static_cast<int>(weekday))));
                                state.dataWeatherManager->RunPeriodInput(i).startWeekDay = weekday;
                            }
                        } else { // Set the weekday if it was not input
                            state.dataWeatherManager->RunPeriodInput(i).startWeekDay = weekday;
                        }
                    }
                }
            } else {
                // Non leap-day start date
                if (!validMonthDay(state.dataWeatherManager->RunPeriodInput(i).startMonth, state.dataWeatherManager->RunPeriodInput(i).startDay)) {
                    ShowSevereError(state,
                                    format("{}: object={}, Invalid input start month/day ({}/{})",
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataWeatherManager->RunPeriodInput(i).title,
                                           state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                           state.dataWeatherManager->RunPeriodInput(i).startDay));
                    ErrorsFound = true;
                } else {                                                              // Month/day is valid
                    if (state.dataWeatherManager->RunPeriodInput(i).startYear == 0) { // No input starting year
                        if (inputWeekday) {
                            state.dataWeatherManager->RunPeriodInput(i).startYear =
                                findYearForWeekday(state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                                   state.dataWeatherManager->RunPeriodInput(i).startDay,
                                                   state.dataWeatherManager->RunPeriodInput(i).startWeekDay);
                        } else {
                            // 2017 is the default year, 1/1 is a Sunday
                            state.dataWeatherManager->RunPeriodInput(i).startYear = 2017;
                            state.dataWeatherManager->RunPeriodInput(i).startWeekDay =
                                calculateDayOfWeek(state,
                                                   state.dataWeatherManager->RunPeriodInput(i).startYear,
                                                   state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                                   state.dataWeatherManager->RunPeriodInput(i).startDay);
                        }
                    } else { // Have an input starting year
                        WeekDay weekday = calculateDayOfWeek(state,
                                                             state.dataWeatherManager->RunPeriodInput(i).startYear,
                                                             state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                                             state.dataWeatherManager->RunPeriodInput(i).startDay);
                        if (inputWeekday) { // Check for correctness of input
                            if (weekday != state.dataWeatherManager->RunPeriodInput(i).startWeekDay) {
                                ShowWarningError(state,
                                                 format("{}: object={}, start weekday ({}) does not match the start year ({}), corrected to {}.",
                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                        state.dataWeatherManager->RunPeriodInput(i).title,
                                                        state.dataIPShortCut->cAlphaArgs(2),
                                                        state.dataWeatherManager->RunPeriodInput(i).startYear,
                                                        DaysOfWeek(static_cast<int>(weekday))));
                                state.dataWeatherManager->RunPeriodInput(i).startWeekDay = weekday;
                            }
                        } else { // Set the weekday if it was not input
                            state.dataWeatherManager->RunPeriodInput(i).startWeekDay = weekday;
                        }
                    }
                }
            }

            // Compute the Julian date of the start date
            state.dataWeatherManager->RunPeriodInput(i).startJulianDate = computeJulianDate(state.dataWeatherManager->RunPeriodInput(i).startYear,
                                                                                            state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                                                                            state.dataWeatherManager->RunPeriodInput(i).startDay);

            // Validate the end date
            if (state.dataWeatherManager->RunPeriodInput(i).endMonth == 2 && state.dataWeatherManager->RunPeriodInput(i).endDay == 29) {
                // Requested end date is a leap year
                if (state.dataWeatherManager->RunPeriodInput(i).endYear == 0) { // No input end year
                    if (isLeapYear(state.dataWeatherManager->RunPeriodInput(i).startYear) &&
                        state.dataWeatherManager->RunPeriodInput(i).startMonth < 3) {
                        // The run period is from some date on or before 2/29 through 2/29
                        state.dataWeatherManager->RunPeriodInput(i).endYear = state.dataWeatherManager->RunPeriodInput(i).startYear;
                    } else {
                        // There might be a better approach here, but for now just loop forward for the next leap year
                        for (int yr = state.dataWeatherManager->RunPeriodInput(i).startYear + 1;
                             yr < state.dataWeatherManager->RunPeriodInput(i).startYear + 10;
                             yr++) {
                            if (isLeapYear(yr)) {
                                state.dataWeatherManager->RunPeriodInput(i).endYear = yr;
                                break;
                            }
                        }
                    }
                } else {                                                                    // Have an input end year
                    if (!isLeapYear(state.dataWeatherManager->RunPeriodInput(i).endYear)) { // End year is not a leap year
                        ShowSevereError(state,
                                        format("{}: object={}, end year ({}) is not a leap year but the requested end date is 2/29.",
                                               state.dataIPShortCut->cCurrentModuleObject,
                                               state.dataWeatherManager->RunPeriodInput(i).title,
                                               state.dataWeatherManager->RunPeriodInput(i).startYear));
                        ErrorsFound = true;
                    } else {
                        state.dataWeatherManager->RunPeriodInput(i).endJulianDate =
                            computeJulianDate(state.dataWeatherManager->RunPeriodInput(i).endYear,
                                              state.dataWeatherManager->RunPeriodInput(i).endMonth,
                                              state.dataWeatherManager->RunPeriodInput(i).endDay);
                        if (state.dataWeatherManager->RunPeriodInput(i).startJulianDate > state.dataWeatherManager->RunPeriodInput(i).endJulianDate) {
                            ShowSevereError(state,
                                            format("{}: object={}, start Julian date ({}) is after the end Julian date ({}).",
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataWeatherManager->RunPeriodInput(i).title,
                                                   state.dataWeatherManager->RunPeriodInput(i).startJulianDate,
                                                   state.dataWeatherManager->RunPeriodInput(i).endJulianDate));
                            ErrorsFound = true;
                        }
                    }
                }
            } else {
                // Non leap-day end date
                if (!validMonthDay(state.dataWeatherManager->RunPeriodInput(i).endMonth, state.dataWeatherManager->RunPeriodInput(i).endDay)) {
                    ShowSevereError(state,
                                    format("{}: object={}, Invalid input end month/day ({}/{})",
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataWeatherManager->RunPeriodInput(i).title,
                                           state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                           state.dataWeatherManager->RunPeriodInput(i).startDay));
                    ErrorsFound = true;
                } else {                                                            // Month/day is valid
                    if (state.dataWeatherManager->RunPeriodInput(i).endYear == 0) { // No input end year
                        // Assume same year as start year
                        state.dataWeatherManager->RunPeriodInput(i).endYear = state.dataWeatherManager->RunPeriodInput(i).startYear;
                        state.dataWeatherManager->RunPeriodInput(i).endJulianDate =
                            computeJulianDate(state.dataWeatherManager->RunPeriodInput(i).endYear,
                                              state.dataWeatherManager->RunPeriodInput(i).endMonth,
                                              state.dataWeatherManager->RunPeriodInput(i).endDay);
                        if (state.dataWeatherManager->RunPeriodInput(i).startJulianDate > state.dataWeatherManager->RunPeriodInput(i).endJulianDate) {
                            state.dataWeatherManager->RunPeriodInput(i).endJulianDate = 0; // Force recalculation later
                            state.dataWeatherManager->RunPeriodInput(i).endYear += 1;
                        }
                    } else { // Have an input end year
                        state.dataWeatherManager->RunPeriodInput(i).endJulianDate =
                            computeJulianDate(state.dataWeatherManager->RunPeriodInput(i).endYear,
                                              state.dataWeatherManager->RunPeriodInput(i).endMonth,
                                              state.dataWeatherManager->RunPeriodInput(i).endDay);
                        if (state.dataWeatherManager->RunPeriodInput(i).startJulianDate > state.dataWeatherManager->RunPeriodInput(i).endJulianDate) {
                            ShowSevereError(state,
                                            format("{}: object={}, start Julian date ({}) is after the end Julian date ({}).",
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataWeatherManager->RunPeriodInput(i).title,
                                                   state.dataWeatherManager->RunPeriodInput(i).startJulianDate,
                                                   state.dataWeatherManager->RunPeriodInput(i).endJulianDate));
                            ErrorsFound = true;
                        }
                    }
                }
            }

            if (state.dataWeatherManager->RunPeriodInput(i).endJulianDate == 0) {
                state.dataWeatherManager->RunPeriodInput(i).endJulianDate = computeJulianDate(state.dataWeatherManager->RunPeriodInput(i).endYear,
                                                                                              state.dataWeatherManager->RunPeriodInput(i).endMonth,
                                                                                              state.dataWeatherManager->RunPeriodInput(i).endDay);
            }

            state.dataWeatherManager->RunPeriodInput(i).numSimYears =
                state.dataWeatherManager->RunPeriodInput(i).endYear - state.dataWeatherManager->RunPeriodInput(i).startYear + 1;

            // A3,  \field Use Weather File Holidays and Special Days
            if (state.dataIPShortCut->lAlphaFieldBlanks(3) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "YES")) {
                state.dataWeatherManager->RunPeriodInput(i).useHolidays = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "NO")) {
                state.dataWeatherManager->RunPeriodInput(i).useHolidays = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " invalid [" + state.dataIPShortCut->cAlphaArgs(3) + ']');
                ErrorsFound = true;
            }

            // A4,  \field Use Weather File Daylight Saving Period
            if (state.dataIPShortCut->lAlphaFieldBlanks(4) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "YES")) {
                state.dataWeatherManager->RunPeriodInput(i).useDST = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "NO")) {
                state.dataWeatherManager->RunPeriodInput(i).useDST = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " invalid [" + state.dataIPShortCut->cAlphaArgs(4) + ']');
                ErrorsFound = true;
            }

            // A5,  \field Apply Weekend Holiday Rule
            if (state.dataIPShortCut->lAlphaFieldBlanks(5) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "YES")) {
                state.dataWeatherManager->RunPeriodInput(i).applyWeekendRule = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "NO")) {
                state.dataWeatherManager->RunPeriodInput(i).applyWeekendRule = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + " invalid [" + state.dataIPShortCut->cAlphaArgs(5) + ']');
                ErrorsFound = true;
            }

            // A6,  \field Use Weather File Rain Indicators
            if (state.dataIPShortCut->lAlphaFieldBlanks(6) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "YES")) {
                state.dataWeatherManager->RunPeriodInput(i).useRain = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "NO")) {
                state.dataWeatherManager->RunPeriodInput(i).useRain = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                    state.dataIPShortCut->cAlphaFieldNames(6) + " invalid [" + state.dataIPShortCut->cAlphaArgs(6) + ']');
                ErrorsFound = true;
            }

            // A7,  \field Use Weather File Snow Indicators
            if (state.dataIPShortCut->lAlphaFieldBlanks(7) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "YES")) {
                state.dataWeatherManager->RunPeriodInput(i).useSnow = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "NO")) {
                state.dataWeatherManager->RunPeriodInput(i).useSnow = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                    state.dataIPShortCut->cAlphaFieldNames(7) + " invalid [" + state.dataIPShortCut->cAlphaArgs(7) + ']');
                ErrorsFound = true;
            }

            // A8,  \field Treat Weather as Actual
            if (state.dataIPShortCut->lAlphaFieldBlanks(8) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "NO")) {
                state.dataWeatherManager->RunPeriodInput(i).actualWeather = false;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "YES")) {
                state.dataWeatherManager->RunPeriodInput(i).actualWeather = true;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": object=" + state.dataWeatherManager->RunPeriodInput(i).title +
                                    state.dataIPShortCut->cAlphaFieldNames(8) + " invalid [" + state.dataIPShortCut->cAlphaArgs(8) + ']');
                ErrorsFound = true;
            }

            // A9,  \field First Hour Interpolation Starting Values
            if (state.dataIPShortCut->lAlphaFieldBlanks(9) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "Hour24")) {
                state.dataWeatherManager->RunPeriodInput(i).firstHrInterpUsingHr1 = false;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Hour1")) {
                state.dataWeatherManager->RunPeriodInput(i).firstHrInterpUsingHr1 = true;
            } else {
                // fail-safe default
                state.dataWeatherManager->RunPeriodInput(i).firstHrInterpUsingHr1 = false;
            }

            state.dataWeatherManager->RunPeriodInput(i).dayOfWeek = static_cast<int>(state.dataWeatherManager->RunPeriodInput(i).startWeekDay);
            state.dataWeatherManager->RunPeriodInput(i).isLeapYear = isLeapYear(state.dataWeatherManager->RunPeriodInput(i).startYear);

            // calculate the annual start and end days from the user inputted month and day
            state.dataWeatherManager->RunPeriodInput(i).monWeekDay = 0;
            if (state.dataWeatherManager->RunPeriodInput(i).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(state,
                                     state.dataWeatherManager->RunPeriodInput(i).startMonth,
                                     state.dataWeatherManager->RunPeriodInput(i).startDay,
                                     state.dataWeatherManager->RunPeriodInput(i).dayOfWeek,
                                     state.dataWeatherManager->RunPeriodInput(i).monWeekDay);
            }
        }

        if (nRunPeriods == 0 && state.dataSysVars->FullAnnualRun) {
            ShowWarningError(state, "No Run Periods input but Full Annual Simulation selected.  Adding Run Period to 1/1 through 12/31.");
            state.dataWeatherManager->Environment.redimension(++state.dataWeatherManager->NumOfEnvrn);
            state.dataWeatherManager->Environment(state.dataWeatherManager->NumOfEnvrn).KindOfEnvrn =
                DataGlobalConstants::KindOfSim::RunPeriodWeather;
            nRunPeriods = 1;
            state.dataGlobal->WeathSimReq = true;
            state.dataWeatherManager->RunPeriodInput.allocate(nRunPeriods);
            state.dataWeatherManager->RunPeriodInput(1).startJulianDate = General::OrdinalDay(state.dataWeatherManager->RunPeriodInput(1).startMonth,
                                                                                              state.dataWeatherManager->RunPeriodInput(1).startDay,
                                                                                              state.dataWeatherManager->LeapYearAdd);
            state.dataWeatherManager->RunPeriodInput(1).endJulianDate = General::OrdinalDay(state.dataWeatherManager->RunPeriodInput(1).endMonth,
                                                                                            state.dataWeatherManager->RunPeriodInput(1).endDay,
                                                                                            state.dataWeatherManager->LeapYearAdd);
            state.dataWeatherManager->RunPeriodInput(1).monWeekDay = 0;
            if (state.dataWeatherManager->RunPeriodInput(1).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(state,
                                     state.dataWeatherManager->RunPeriodInput(1).startMonth,
                                     state.dataWeatherManager->RunPeriodInput(1).startDay,
                                     state.dataWeatherManager->RunPeriodInput(1).dayOfWeek,
                                     state.dataWeatherManager->RunPeriodInput(1).monWeekDay);
            }
        } else if (nRunPeriods > 1 && state.dataSysVars->FullAnnualRun) {
            nRunPeriods = 1;
        }
    }

    void GetRunPeriodDesignData(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the run period design info from User input and the
        //  simulation dates

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

        // Call Input Get routine to retrieve annual run data
        int RPD1 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileDays");
        int RPD2 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "SizingPeriod:WeatherFileConditionType");
        state.dataWeatherManager->TotRunDesPers = RPD1 + RPD2;

        state.dataWeatherManager->RunPeriodDesignInput.allocate(RPD1 + RPD2);
        state.dataWeatherManager->RunPeriodDesignInputUniqueNames.reserve(static_cast<unsigned>(RPD1 + RPD2));

        int Count = 0;
        state.dataIPShortCut->cCurrentModuleObject = "SizingPeriod:WeatherFileDays";
        for (int i = 1; i <= RPD1; ++i) {
            int NumAlphas;   // Number of alphas being input
            int NumNumerics; // Number of Numerics being input
            int IOStat;      // IO Status when calling get input subroutine
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     i,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumerics,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataWeatherManager->RunPeriodDesignInputUniqueNames,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            ++Count;
            state.dataWeatherManager->RunPeriodDesignInput(Count).title = state.dataIPShortCut->cAlphaArgs(1);
            state.dataWeatherManager->RunPeriodDesignInput(Count).periodType = "User Selected WeatherFile RunPeriod (Design)";

            // set the start and end day of month from user input
            state.dataWeatherManager->RunPeriodDesignInput(Count).startMonth = int(state.dataIPShortCut->rNumericArgs(1));
            state.dataWeatherManager->RunPeriodDesignInput(Count).startDay = int(state.dataIPShortCut->rNumericArgs(2));
            state.dataWeatherManager->RunPeriodDesignInput(Count).endMonth = int(state.dataIPShortCut->rNumericArgs(3));
            state.dataWeatherManager->RunPeriodDesignInput(Count).endDay = int(state.dataIPShortCut->rNumericArgs(4));

            switch (state.dataWeatherManager->RunPeriodDesignInput(Count).startMonth) {
            case 1:
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
            case 12:
                if (state.dataWeatherManager->RunPeriodDesignInput(Count).startDay > 31) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject +
                                        ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                        format("{} invalid (Day of Month) [{}]",
                                               state.dataIPShortCut->cNumericFieldNames(2),
                                               state.dataWeatherManager->RunPeriodDesignInput(Count).startDay));
                    ErrorsFound = true;
                }
                break;
            case 4:
            case 6:
            case 9:
            case 11:
                if (state.dataWeatherManager->RunPeriodDesignInput(Count).startDay > 30) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject +
                                        ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                        format("{} invalid (Day of Month) [{}]",
                                               state.dataIPShortCut->cNumericFieldNames(2),
                                               state.dataWeatherManager->RunPeriodDesignInput(Count).startDay));
                    ErrorsFound = true;
                }
                break;
            case 2:
                if (state.dataWeatherManager->RunPeriodDesignInput(Count).startDay > 28 + state.dataWeatherManager->LeapYearAdd) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject +
                                        ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                        format("{} invalid (Day of Month) [{}]",
                                               state.dataIPShortCut->cNumericFieldNames(2),
                                               state.dataWeatherManager->RunPeriodDesignInput(Count).startDay));
                    ErrorsFound = true;
                }
                break;
            default:
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                    format("{} invalid (Month) [{}]",
                                           state.dataIPShortCut->cNumericFieldNames(1),
                                           state.dataWeatherManager->RunPeriodDesignInput(Count).startMonth));
                ErrorsFound = true;
                break;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
            } else {
                state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), ValidNames, 12);
                if (state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek == 0 ||
                    state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek == 8) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject +
                                         ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                         state.dataIPShortCut->cAlphaFieldNames(1) + " invalid (Day of Week) [" +
                                         state.dataIPShortCut->cAlphaArgs(1) + " for Start is not Valid, Monday will be Used.");
                    state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
                }
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(3) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "YES")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useDST = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "NO")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useDST = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + " invalid [" + state.dataIPShortCut->cAlphaArgs(3) + ']');
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(4) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "YES")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useRain = true;
                state.dataWeatherManager->RunPeriodDesignInput(Count).useSnow = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "NO")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useRain = false;
                state.dataWeatherManager->RunPeriodDesignInput(Count).useSnow = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " invalid [" + state.dataIPShortCut->cAlphaArgs(4) + ']');
                ErrorsFound = true;
            }

            // calculate the annual start and end days from the user inputted month and day
            state.dataWeatherManager->RunPeriodDesignInput(Count).startJulianDate =
                General::OrdinalDay(state.dataWeatherManager->RunPeriodDesignInput(Count).startMonth,
                                    state.dataWeatherManager->RunPeriodDesignInput(Count).startDay,
                                    state.dataWeatherManager->LeapYearAdd);
            state.dataWeatherManager->RunPeriodDesignInput(Count).endJulianDate =
                General::OrdinalDay(state.dataWeatherManager->RunPeriodDesignInput(Count).endMonth,
                                    state.dataWeatherManager->RunPeriodDesignInput(Count).endDay,
                                    state.dataWeatherManager->LeapYearAdd);
            if (state.dataWeatherManager->RunPeriodDesignInput(Count).startJulianDate <=
                state.dataWeatherManager->RunPeriodDesignInput(Count).endJulianDate) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).totalDays =
                    (state.dataWeatherManager->RunPeriodDesignInput(Count).endJulianDate -
                     state.dataWeatherManager->RunPeriodDesignInput(Count).startJulianDate + 1) *
                    state.dataWeatherManager->RunPeriodDesignInput(Count).numSimYears;
            } else {
                state.dataWeatherManager->RunPeriodDesignInput(Count).totalDays =
                    (General::OrdinalDay(12, 31, state.dataWeatherManager->LeapYearAdd) -
                     state.dataWeatherManager->RunPeriodDesignInput(Count).startJulianDate + 1 +
                     state.dataWeatherManager->RunPeriodDesignInput(Count).endJulianDate) *
                    state.dataWeatherManager->RunPeriodDesignInput(Count).numSimYears;
            }
            state.dataWeatherManager->RunPeriodDesignInput(Count).monWeekDay = 0;
            if (state.dataWeatherManager->RunPeriodDesignInput(1).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(state,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).startMonth,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).startDay,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).dayOfWeek,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).monWeekDay);
            }
        }

        state.dataIPShortCut->cCurrentModuleObject = "SizingPeriod:WeatherFileConditionType";
        for (int i = 1; i <= RPD2; ++i) {
            int NumAlphas;   // Number of alphas being input
            int NumNumerics; // Number of Numerics being input
            int IOStat;      // IO Status when calling get input subroutine
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     i,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumerics,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataWeatherManager->RunPeriodDesignInputUniqueNames,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);

            ++Count;
            state.dataWeatherManager->RunPeriodDesignInput(Count).title = state.dataIPShortCut->cAlphaArgs(1);
            state.dataWeatherManager->RunPeriodDesignInput(Count).periodType =
                "User Selected WeatherFile Typical/Extreme Period (Design)=" + state.dataIPShortCut->cAlphaArgs(2);

            // Period Selection
            if (!state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                int WhichPeriod = UtilityRoutines::FindItem(
                    state.dataIPShortCut->cAlphaArgs(2), state.dataWeatherManager->TypicalExtremePeriods, &TypicalExtremeData::MatchValue);
                if (WhichPeriod != 0) {
                    state.dataWeatherManager->RunPeriodDesignInput(Count).startDay =
                        state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartDay;
                    state.dataWeatherManager->RunPeriodDesignInput(Count).startMonth =
                        state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartMonth;
                    state.dataWeatherManager->RunPeriodDesignInput(Count).startJulianDate =
                        state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartJDay;
                    state.dataWeatherManager->RunPeriodDesignInput(Count).endDay =
                        state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndDay;
                    state.dataWeatherManager->RunPeriodDesignInput(Count).endMonth =
                        state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndMonth;
                    state.dataWeatherManager->RunPeriodDesignInput(Count).endJulianDate =
                        state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndJDay;
                    state.dataWeatherManager->RunPeriodDesignInput(Count).totalDays =
                        state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).TotalDays;
                } else {
                    WhichPeriod = UtilityRoutines::FindItem(
                        state.dataIPShortCut->cAlphaArgs(2), state.dataWeatherManager->TypicalExtremePeriods, &TypicalExtremeData::MatchValue1);
                    if (WhichPeriod != 0) {
                        state.dataWeatherManager->RunPeriodDesignInput(Count).startDay =
                            state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartDay;
                        state.dataWeatherManager->RunPeriodDesignInput(Count).startMonth =
                            state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartMonth;
                        state.dataWeatherManager->RunPeriodDesignInput(Count).startJulianDate =
                            state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartJDay;
                        state.dataWeatherManager->RunPeriodDesignInput(Count).endDay =
                            state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndDay;
                        state.dataWeatherManager->RunPeriodDesignInput(Count).endMonth =
                            state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndMonth;
                        state.dataWeatherManager->RunPeriodDesignInput(Count).endJulianDate =
                            state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndJDay;
                        state.dataWeatherManager->RunPeriodDesignInput(Count).totalDays =
                            state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).TotalDays;
                        ShowWarningError(state,
                                         state.dataIPShortCut->cCurrentModuleObject +
                                             ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                             state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2) + " matched to " +
                                             state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).MatchValue);
                    } else {
                        WhichPeriod = UtilityRoutines::FindItem(
                            state.dataIPShortCut->cAlphaArgs(2), state.dataWeatherManager->TypicalExtremePeriods, &TypicalExtremeData::MatchValue2);
                        if (WhichPeriod != 0) {
                            state.dataWeatherManager->RunPeriodDesignInput(Count).startDay =
                                state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartDay;
                            state.dataWeatherManager->RunPeriodDesignInput(Count).startMonth =
                                state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartMonth;
                            state.dataWeatherManager->RunPeriodDesignInput(Count).startJulianDate =
                                state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).StartJDay;
                            state.dataWeatherManager->RunPeriodDesignInput(Count).endDay =
                                state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndDay;
                            state.dataWeatherManager->RunPeriodDesignInput(Count).endMonth =
                                state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndMonth;
                            state.dataWeatherManager->RunPeriodDesignInput(Count).endJulianDate =
                                state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).EndJDay;
                            state.dataWeatherManager->RunPeriodDesignInput(Count).totalDays =
                                state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).TotalDays;
                            ShowWarningError(state,
                                             state.dataIPShortCut->cCurrentModuleObject +
                                                 ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                                 state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2) +
                                                 " matched to " + state.dataWeatherManager->TypicalExtremePeriods(WhichPeriod).MatchValue);
                        } else {
                            ShowSevereError(state,
                                            state.dataIPShortCut->cCurrentModuleObject +
                                                ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                                state.dataIPShortCut->cAlphaFieldNames(2) +
                                                " invalid (not on Weather File)=" + state.dataIPShortCut->cAlphaArgs(2));
                            ErrorsFound = true;
                        }
                    }
                }
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + " invalid (blank).");
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
            } else {
                state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), ValidNames, 12);
                if (state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek == 0 ||
                    state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek == 8) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject +
                                         ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                         state.dataIPShortCut->cAlphaFieldNames(3) + " invalid (Day of Week) [" +
                                         state.dataIPShortCut->cAlphaArgs(3) + " for Start is not Valid, Monday will be Used.");
                    state.dataWeatherManager->RunPeriodDesignInput(Count).dayOfWeek = 2; // Defaults to Monday
                }
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(4) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "YES")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useDST = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "NO")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useDST = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " invalid [" + state.dataIPShortCut->cAlphaArgs(4) + ']');
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->lAlphaFieldBlanks(5) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "YES")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useRain = true;
                state.dataWeatherManager->RunPeriodDesignInput(Count).useSnow = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "NO")) {
                state.dataWeatherManager->RunPeriodDesignInput(Count).useRain = false;
                state.dataWeatherManager->RunPeriodDesignInput(Count).useSnow = false;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": object=" + state.dataWeatherManager->RunPeriodDesignInput(Count).title + ' ' +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + " invalid [" + state.dataIPShortCut->cAlphaArgs(5) + ']');
                ErrorsFound = true;
            }
            state.dataWeatherManager->RunPeriodDesignInput(1).monWeekDay = 0;
            if (state.dataWeatherManager->RunPeriodDesignInput(1).dayOfWeek != 0 && !ErrorsFound) {
                SetupWeekDaysByMonth(state,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).startMonth,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).startDay,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).dayOfWeek,
                                     state.dataWeatherManager->RunPeriodDesignInput(1).monWeekDay);
            }
        }
    }

    void GetSpecialDayPeriodData(EnergyPlusData &state, bool &ErrorsFound) // will be set to true if severe errors are found in inputs
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

        static Array1D_string const ValidDayTypes(5, {"HOLIDAY", "SUMMERDESIGNDAY", "WINTERDESIGNDAY", "CUSTOMDAY1", "CUSTOMDAY2"});

        state.dataIPShortCut->cCurrentModuleObject = "RunPeriodControl:SpecialDays";
        int NumSpecDays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        int Count;
        if (allocated(state.dataWeatherManager->SpecialDays)) { // EPW already allocated the array
            Count = state.dataWeatherManager->NumSpecialDays - NumSpecDays + 1;
        } else {
            state.dataWeatherManager->SpecialDays.allocate(NumSpecDays);
            state.dataWeatherManager->NumSpecialDays = NumSpecDays;
            Count = 1;
        }

        for (int i = 1; i <= NumSpecDays; ++i) {

            Array1D_string AlphArray(3);
            int NumAlphas;
            Array1D<Real64> Duration(1);
            int NumNumbers;
            int IOStat;
            state.dataInputProcessing->inputProcessor->getObjectItem(
                state, state.dataIPShortCut->cCurrentModuleObject, i, AlphArray, NumAlphas, Duration, NumNumbers, IOStat);
            UtilityRoutines::IsNameEmpty(state, AlphArray(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);
            state.dataWeatherManager->SpecialDays(Count).Name = AlphArray(1);

            int PMonth;
            int PDay;
            int PWeekDay;
            DateType dateType;
            General::ProcessDateString(state, AlphArray(2), PMonth, PDay, PWeekDay, dateType, ErrorsFound);
            if (dateType == DateType::MonthDay) {
                state.dataWeatherManager->SpecialDays(Count).DateType = dateType;
                state.dataWeatherManager->SpecialDays(Count).Month = PMonth;
                state.dataWeatherManager->SpecialDays(Count).Day = PDay;
                state.dataWeatherManager->SpecialDays(Count).WeekDay = 0;
                state.dataWeatherManager->SpecialDays(Count).CompDate = PMonth * 32 + PDay;
                state.dataWeatherManager->SpecialDays(Count).WthrFile = false;
            } else if (dateType != DateType::Invalid) {
                state.dataWeatherManager->SpecialDays(Count).DateType = dateType;
                state.dataWeatherManager->SpecialDays(Count).Month = PMonth;
                state.dataWeatherManager->SpecialDays(Count).Day = PDay;
                state.dataWeatherManager->SpecialDays(Count).WeekDay = PWeekDay;
                state.dataWeatherManager->SpecialDays(Count).CompDate = 0;
                state.dataWeatherManager->SpecialDays(Count).WthrFile = false;
            } else if (dateType == DateType::Invalid) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": " + AlphArray(1) + " Invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + '=' + AlphArray(2));
                ErrorsFound = true;
            }

            if (Duration(1) > 0) {
                state.dataWeatherManager->SpecialDays(Count).Duration = int(Duration(1));
            } else {
                ShowSevereError(state,
                                format("{}: {} Invalid {}={:.0T}",
                                       state.dataIPShortCut->cCurrentModuleObject,
                                       AlphArray(1),
                                       state.dataIPShortCut->cNumericFieldNames(1),
                                       Duration(1)));
                ErrorsFound = true;
            }

            int DayType = UtilityRoutines::FindItemInList(AlphArray(3), ValidDayTypes, 5);
            if (DayType == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": " + AlphArray(1) + " Invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + '=' + AlphArray(3));
                ErrorsFound = true;
            } else {
                state.dataWeatherManager->SpecialDays(Count).DayType = DayType;
            }
            ++Count;
        }
    }

    void CalcSpecialDayTypes(EnergyPlusData &state)
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

        state.dataWeatherManager->SpecialDayTypes = 0; // Initialize/Reset Special Day Types array

        for (int i = 1; i <= state.dataWeatherManager->NumSpecialDays; ++i) {

            if (state.dataWeatherManager->SpecialDays(i).WthrFile) continue;

            int Warn = 0;

            int JDay = General::OrdinalDay(state.dataWeatherManager->SpecialDays(i).Month,
                                           state.dataWeatherManager->SpecialDays(i).Day,
                                           state.dataWeatherManager->LeapYearAdd) -
                       1;

            for (int j = 1; j <= state.dataWeatherManager->SpecialDays(i).Duration; ++j) {
                ++JDay;
                if (JDay > 366) {
                    ShowWarningError(state,
                                     "SpecialDay=" + state.dataWeatherManager->SpecialDays(i).Name +
                                         " causes index of more than 366, ignoring those beyond 366");
                } else {
                    if (state.dataWeatherManager->SpecialDayTypes(JDay) != 0 && Warn == 0) {
                        ShowWarningError(state,
                                         "SpecialDay=" + state.dataWeatherManager->SpecialDays(i).Name +
                                             " attempted overwrite of previous set special day");
                        Warn = 1;
                    } else if (state.dataWeatherManager->SpecialDayTypes(JDay) == 0) {
                        state.dataWeatherManager->SpecialDayTypes(JDay) = state.dataWeatherManager->SpecialDays(i).DayType;
                    }
                }
            }
        }
    }

    void GetDSTData(EnergyPlusData &state, bool &ErrorsFound) // will be set to true if severe errors are found in inputs
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

        state.dataIPShortCut->cCurrentModuleObject = "RunPeriodControl:DaylightSavingTime";
        int NumFound = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (NumFound == 1) {
            int NumAlphas;
            int IOStat;
            int NumNumbers;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     1,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (NumAlphas != 2) {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Insufficient fields, must have Start AND End Dates");
                ErrorsFound = true;
            } else { // Correct number of arguments
                General::ProcessDateString(state,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataWeatherManager->IDFDST.StMon,
                                           state.dataWeatherManager->IDFDST.StDay,
                                           state.dataWeatherManager->IDFDST.StWeekDay,
                                           state.dataWeatherManager->IDFDST.StDateType,
                                           ErrorsFound);
                if (state.dataWeatherManager->IDFDST.StDateType == DateType::Invalid) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                        state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                General::ProcessDateString(state,
                                           state.dataIPShortCut->cAlphaArgs(2),
                                           state.dataWeatherManager->IDFDST.EnMon,
                                           state.dataWeatherManager->IDFDST.EnDay,
                                           state.dataWeatherManager->IDFDST.EnWeekDay,
                                           state.dataWeatherManager->IDFDST.EnDateType,
                                           ErrorsFound);
                if (state.dataWeatherManager->IDFDST.EnDateType == DateType::Invalid) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' +
                                        state.dataIPShortCut->cAlphaArgs(2));
                    ErrorsFound = true;
                }
                state.dataWeatherManager->IDFDaylightSaving = true;
            }
        } else if (NumFound > 1) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Too many objects in Input File, only one allowed.");
            ErrorsFound = true;
        }
    }

    void GetDesignDayData(EnergyPlusData &state,
                          int &TotDesDays, // Total number of Design days to Setup
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
        static std::map<DDHumIndType, std::string> const DDHumIndTypeStringRep{{DDHumIndType::WetBulb, "Wetbulb [C]"},
                                                                               {DDHumIndType::DewPoint, "Dewpoint [C]"},
                                                                               {DDHumIndType::Enthalpy, "Enthalpy [J/kg]"},
                                                                               {DDHumIndType::HumRatio, "Humidity Ratio []"},
                                                                               {DDHumIndType::RelHumSch, "Schedule []"},
                                                                               {DDHumIndType::WBProfDef, "WetBulbProfileDefaultMultipliers []"},
                                                                               {DDHumIndType::WBProfDif, "WetBulbProfileDifferenceSchedule []"},
                                                                               {DDHumIndType::WBProfMul, "WetBulbProfileMultiplierSchedule []"}};

        // Below are the 2009 fractions, HOF, Chap 14, Table 6
        static constexpr std::array<Real64, 24> DefaultTempRangeMult = {0.88, 0.92, 0.95, 0.98, 1.0,  0.98, 0.91, 0.74, 0.55, 0.38, 0.23, 0.13,
                                                                        0.05, 0.00, 0.00, 0.06, 0.14, 0.24, 0.39, 0.50, 0.59, 0.68, 0.75, 0.82};

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string units;
        OutputProcessor::Unit unitType;

        state.dataWeatherManager->DesDayInput.allocate(TotDesDays); // Allocate the array to the # of DD's
        state.dataWeatherManager->DDDBRngModifier.allocate(state.dataGlobal->NumOfTimeStepInHour, 24, TotDesDays);
        state.dataWeatherManager->DDDBRngModifier = 0.0;
        state.dataWeatherManager->DDHumIndModifier.allocate(state.dataGlobal->NumOfTimeStepInHour, 24, TotDesDays);
        state.dataWeatherManager->DDHumIndModifier = 0.0;
        state.dataWeatherManager->DDBeamSolarValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24, TotDesDays);
        state.dataWeatherManager->DDBeamSolarValues = 0.0;
        state.dataWeatherManager->DDDiffuseSolarValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24, TotDesDays);
        state.dataWeatherManager->DDDiffuseSolarValues = 0.0;
        state.dataWeatherManager->DDSkyTempScheduleValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24, TotDesDays);
        state.dataWeatherManager->DDSkyTempScheduleValues = 0.0;

        state.dataWeatherManager->SPSiteDryBulbRangeModScheduleValue.dimension(TotDesDays, 0.0);
        state.dataWeatherManager->SPSiteHumidityConditionScheduleValue.dimension(TotDesDays, 0.0);
        state.dataWeatherManager->SPSiteBeamSolarScheduleValue.dimension(TotDesDays, 0.0);
        state.dataWeatherManager->SPSiteDiffuseSolarScheduleValue.dimension(TotDesDays, 0.0);
        state.dataWeatherManager->SPSiteSkyTemperatureScheduleValue.dimension(TotDesDays, 0.0);

        if (state.dataSysVars->ReverseDD && TotDesDays <= 1) {
            ShowSevereError(state, "GetDesignDayData: Reverse Design Day requested but # Design Days <=1");
        }

        state.dataIPShortCut->cCurrentModuleObject = "SizingPeriod:DesignDay";
        for (int i = 1; i <= TotDesDays; ++i) {

            int EnvrnNum;
            if (state.dataSysVars->ReverseDD) {
                if (i == 1 && TotDesDays > 1) {
                    EnvrnNum = 2;
                } else if (i == 2) {
                    EnvrnNum = 1;
                } else {
                    EnvrnNum = i;
                }
            } else {
                EnvrnNum = i;
            }

            // Call Input Get routine to retrieve design day data
            bool MaxDryBulbEntered = false;
            bool PressureEntered = false;
            int NumAlpha;    // Number of material alpha names being passed
            int NumNumerics; // Number of material properties being passed
            int IOStat;      // IO Status when calling get input subroutine
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     i,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlpha,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumerics,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);
            state.dataWeatherManager->DesDayInput(EnvrnNum).Title = state.dataIPShortCut->cAlphaArgs(1); // Environment name
            state.dataWeatherManager->Environment(EnvrnNum).Title = state.dataWeatherManager->DesDayInput(EnvrnNum).Title;

            //   N3,  \field Maximum Dry-Bulb Temperature
            //   N4,  \field Daily Dry-Bulb Temperature Range
            //   N9,  \field Barometric Pressure
            //   N10, \field Wind Speed
            //   N11, \field Wind Direction
            state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb = state.dataIPShortCut->rNumericArgs(3); // Maximum Dry-Bulb Temperature (C)
            if (!state.dataIPShortCut->lNumericFieldBlanks(3)) MaxDryBulbEntered = true;
            state.dataWeatherManager->DesDayInput(EnvrnNum).DailyDBRange =
                state.dataIPShortCut->rNumericArgs(4); // Daily dry-bulb temperature range (deltaC)
            state.dataWeatherManager->DesDayInput(EnvrnNum).PressBarom =
                state.dataIPShortCut->rNumericArgs(9); // Atmospheric/Barometric Pressure (Pascals)
            if (!state.dataIPShortCut->lNumericFieldBlanks(9)) PressureEntered = true;
            state.dataWeatherManager->DesDayInput(EnvrnNum).PressureEntered = PressureEntered;
            state.dataWeatherManager->DesDayInput(EnvrnNum).WindSpeed = state.dataIPShortCut->rNumericArgs(10);           // Wind Speed (m/s)
            state.dataWeatherManager->DesDayInput(EnvrnNum).WindDir = mod(state.dataIPShortCut->rNumericArgs(11), 360.0); // Wind Direction
            // (degrees clockwise from North, N=0, E=90, S=180, W=270)
            //   N1,  \field Month
            //   N2,  \field Day of Month
            //   N12, \field ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)
            //   N13, \field ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)
            //   N8,  \field Daily Wet-Bulb Temperature Range
            state.dataWeatherManager->DesDayInput(EnvrnNum).Month = int(state.dataIPShortCut->rNumericArgs(1));      // Month of Year ( 1 - 12 )
            state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth = int(state.dataIPShortCut->rNumericArgs(2)); // Day of Month ( 1 - 31 )
            state.dataWeatherManager->DesDayInput(EnvrnNum).TauB = state.dataIPShortCut->rNumericArgs(12);           // beam tau >= 0
            state.dataWeatherManager->DesDayInput(EnvrnNum).TauD = state.dataIPShortCut->rNumericArgs(13);           // diffuse tau >= 0
            state.dataWeatherManager->DesDayInput(EnvrnNum).DailyWBRange =
                state.dataIPShortCut->rNumericArgs(8); // Daily wet-bulb temperature range (deltaC)

            //   N14; \field Sky Clearness
            state.dataWeatherManager->DesDayInput(EnvrnNum).SkyClear = state.dataIPShortCut->rNumericArgs(14); // Sky Clearness (0 to 1)

            //   N15, \field Maximum Warmup Days Between Sizing Periods
            if (state.dataIPShortCut->lNumericFieldBlanks(15)) {
                // Default to -1 if not input
                state.dataWeatherManager->DesDayInput(EnvrnNum).maxWarmupDays = -1;
            } else {
                state.dataWeatherManager->DesDayInput(EnvrnNum).maxWarmupDays = int(state.dataIPShortCut->rNumericArgs(15));
            }
            //   A13, \field Begin Environment Reset Mode
            if (state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).suppressBegEnvReset = false;
            } else {
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(13), "FullResetAtBeginEnvironment")) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).suppressBegEnvReset = false;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(13), "SuppressThermalResetAtBeginEnvironment")) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).suppressBegEnvReset = true;
                }
            }
            // for PerformancePrecisionTradeoffs
            if (state.dataEnvrn->forceBeginEnvResetSuppress) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).suppressBegEnvReset = true;
            }
            //   A7,  \field Rain Indicator
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "Yes")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).RainInd = 1;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "No") || state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).RainInd = 0;
            } else {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                     "\", invalid field: " + state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) +
                                     "\".");
                ShowContinueError(state, "\"No\" will be used.");
                state.dataWeatherManager->DesDayInput(EnvrnNum).RainInd = 0;
            }

            //   A8,  \field Snow Indicator
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "Yes")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).SnowInd = 1;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(8), "No") || state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).SnowInd = 0;
            } else {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                     "\", invalid field: " + state.dataIPShortCut->cAlphaFieldNames(8) + "=\"" + state.dataIPShortCut->cAlphaArgs(8) +
                                     "\".");
                ShowContinueError(state, "\"No\" will be used.");
                state.dataWeatherManager->DesDayInput(EnvrnNum).SnowInd = 0;
            }

            //   A3,  \field Dry-Bulb Temperature Range Modifier Type
            // check DB profile input
            if (state.dataIPShortCut->lAlphaFieldBlanks(3) ||
                UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "DefaultMultipliers")) {
                state.dataIPShortCut->cAlphaArgs(3) = "DefaultMultipliers";
                state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType = DDDBRangeType::Default;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "Multiplier") ||
                       UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MultiplierSchedule")) {
                state.dataIPShortCut->cAlphaArgs(3) = "MultiplierSchedule";
                state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType = DDDBRangeType::Multiplier;
                units = "[]";
                unitType = OutputProcessor::Unit::None;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "Difference") ||
                       UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "Delta") ||
                       UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "DifferenceSchedule") ||
                       UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "DeltaSchedule")) {
                state.dataIPShortCut->cAlphaArgs(3) = "DifferenceSchedule";
                state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType = DDDBRangeType::Difference;
                units = "[deltaC]";
                unitType = OutputProcessor::Unit::deltaC;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "TemperatureProfileSchedule")) {
                state.dataIPShortCut->cAlphaArgs(3) = "TemperatureProfileSchedule";
                state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType = DDDBRangeType::Profile;
                units = "[C]";
                unitType = OutputProcessor::Unit::C;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                    "\", invalid data.");
                ShowContinueError(
                    state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
                ErrorsFound = true;
                state.dataIPShortCut->cAlphaArgs(3) = "invalid field";
                state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType = DDDBRangeType::Default;
            }

            if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType != DDDBRangeType::Profile && !MaxDryBulbEntered &&
                state.dataIPShortCut->cAlphaArgs(3) != "invalid field") {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                    "\", invalid data.");
                ShowContinueError(state, "..invalid blank field: " + state.dataIPShortCut->cNumericFieldNames(3));
                ShowContinueError(state,
                                  "..this field is required when " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" +
                                      state.dataIPShortCut->cAlphaArgs(3) + "\".");
                ErrorsFound = true;
            }

            // Assume either "multiplier" option will make full use of range...
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType != DDDBRangeType::Difference &&
                state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType != DDDBRangeType::Profile) {
                Real64 testval =
                    state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb - state.dataWeatherManager->DesDayInput(EnvrnNum).DailyDBRange;
                bool errFlag = false;
                state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                      errFlag,
                                                                      state.dataIPShortCut->cAlphaFieldNames(3),
                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                      "Severe",
                                                                      ">= -90",
                                                                      (testval >= -90.0),
                                                                      "<= 70",
                                                                      (testval <= 70.0),
                                                                      {},
                                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            }

            //   A4,  \field Dry-Bulb Temperature Range Modifier Day Schedule Name
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).DBTempRangeType != DDDBRangeType::Default) {
                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).TempRangeSchPtr =
                        ScheduleManager::GetDayScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
                    if (state.dataWeatherManager->DesDayInput(EnvrnNum).TempRangeSchPtr == 0) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                            "\", invalid data.");
                        ShowContinueError(state,
                                          "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" +
                                              state.dataIPShortCut->cAlphaArgs(4) + "\".");
                        ErrorsFound = true;
                    } else {
                        ScheduleManager::GetSingleDayScheduleValues(state,
                                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).TempRangeSchPtr,
                                                                    state.dataWeatherManager->DDDBRngModifier(_, _, EnvrnNum));
                        int schPtr = General::FindNumberInList(state.dataWeatherManager->DesDayInput(EnvrnNum).TempRangeSchPtr,
                                                               state.dataWeatherManager->SPSiteScheduleNamePtr,
                                                               state.dataWeatherManager->NumSPSiteScheduleNamePtrs);
                        if ((schPtr == 0) || (state.dataWeatherManager->SPSiteScheduleUnits(schPtr) != units)) {
                            ++state.dataWeatherManager->NumSPSiteScheduleNamePtrs;
                            state.dataWeatherManager->SPSiteScheduleNamePtr(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) =
                                state.dataWeatherManager->DesDayInput(EnvrnNum).TempRangeSchPtr;
                            state.dataWeatherManager->SPSiteScheduleUnits(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable(state,
                                                "Sizing Period Site Drybulb Temperature Range Modifier Schedule Value",
                                                unitType,
                                                state.dataWeatherManager->SPSiteDryBulbRangeModScheduleValue(EnvrnNum),
                                                OutputProcessor::SOVTimeStepType::Zone,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataIPShortCut->cAlphaArgs(4));
                        }
                        if (state.dataIPShortCut->cAlphaArgs(3) == "MultiplierSchedule") {
                            if (!ScheduleManager::CheckDayScheduleValueMinMax(
                                    state, state.dataWeatherManager->DesDayInput(EnvrnNum).TempRangeSchPtr, 0.0, false, 1.0, false)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                ShowContinueError(state,
                                                  "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" +
                                                      state.dataIPShortCut->cAlphaArgs(4) + "\".");
                                ShowContinueError(state, "..Specified [Schedule] Dry-bulb Range Multiplier Values are not within [0.0, 1.0]");
                                ErrorsFound = true;
                            }
                        } else if (state.dataIPShortCut->cAlphaArgs(3) == "DifferenceSchedule") { // delta, must be > 0.0
                            if (!ScheduleManager::CheckDayScheduleValueMinMax(
                                    state, state.dataWeatherManager->DesDayInput(EnvrnNum).TempRangeSchPtr, 0.0, false)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                ShowContinueError(state,
                                                  "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" +
                                                      state.dataIPShortCut->cAlphaArgs(4) + "\".");
                                ShowSevereError(state, "Some [Schedule] Dry-bulb Range Difference Values are < 0.0 [would make max larger].");
                                ErrorsFound = true;
                            }
                        }
                        if (state.dataIPShortCut->cAlphaArgs(3) == "TemperatureProfileSchedule") {
                            Real64 testval = maxval(state.dataWeatherManager->DDDBRngModifier(_, _, EnvrnNum));
                            if (MaxDryBulbEntered) {
                                ShowWarningError(state,
                                                 state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                     state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", data override.");
                                ShowContinueError(state,
                                                  format("..{}=[{:.2R}] will be overwritten.",
                                                         state.dataIPShortCut->cNumericFieldNames(3),
                                                         state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb));
                                ShowContinueError(
                                    state, ".." + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
                                ShowContinueError(state, format("..with max value=[{:.2R}].", testval));
                            }
                            state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb = testval;
                        }
                        Real64 testval = maxval(state.dataWeatherManager->DDDBRngModifier(_, _, EnvrnNum));
                        testval = state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb - testval;
                        bool errFlag = false;
                        state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                              errFlag,
                                                                              state.dataIPShortCut->cAlphaFieldNames(4),
                                                                              state.dataIPShortCut->cCurrentModuleObject,
                                                                              "Severe",
                                                                              ">= -90",
                                                                              (testval >= -90.0),
                                                                              "<= 70",
                                                                              (testval <= 70.0),
                                                                              {},
                                                                              state.dataWeatherManager->DesDayInput(EnvrnNum).Title);
                        if (errFlag) {
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(4) + " is blank.");
                    ShowContinueError(state, "..required when " + state.dataIPShortCut->cAlphaFieldNames(3) + " indicates \"SCHEDULE\".");
                    ErrorsFound = true;
                }
            } else {
                // Default dry-bulb temperature Range
                Real64 LastHrValue = DefaultTempRangeMult[23];
                for (int hour = 1; hour <= 24; ++hour) {
                    for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {
                        Real64 WNow = state.dataWeatherManager->Interpolation(ts);
                        Real64 WPrev = 1.0 - WNow;
                        state.dataWeatherManager->DDDBRngModifier(ts, hour, EnvrnNum) = LastHrValue * WPrev + DefaultTempRangeMult[hour - 1] * WNow;
                    }
                    LastHrValue = DefaultTempRangeMult[hour - 1];
                }
            }

            //   A5,  \field Humidity Condition Type
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "WetBulb")) {
                state.dataIPShortCut->cAlphaArgs(5) = "WetBulb";
                //   N5,  \field Wetbulb or DewPoint at Maximum Dry-Bulb
                if (!state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                        state.dataIPShortCut->rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(5) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                bool errFlag = false;
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::WetBulb;
                state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                      errFlag,
                                                                      state.dataIPShortCut->cAlphaFieldNames(5) + " - Wet-Bulb",
                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                      "Severe",
                                                                      ">= -90",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue >= -90.0),
                                                                      "<= 70",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue <= 70.0),
                                                                      {},
                                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    //        CALL ShowContinueError(state, TRIM(state.dataIPShortCut->cCurrentModuleObject)//': Occured in
                    //        '//TRIM(DesDayInput(EnvrnNum)%Title))
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "DewPoint")) {
                state.dataIPShortCut->cAlphaArgs(5) = "DewPoint";
                if (!state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                        state.dataIPShortCut->rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(5) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                bool errFlag = false;
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::DewPoint;
                state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                      errFlag,
                                                                      state.dataIPShortCut->cAlphaFieldNames(5) + " - Dew-Point",
                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                      "Severe",
                                                                      ">= -90",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue >= -90.0),
                                                                      "<= 70",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue <= 70.0),
                                                                      {},
                                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "HumidityRatio")) {
                state.dataIPShortCut->cAlphaArgs(5) = "HumidityRatio";
                //   N6,  \field Humidity Ratio at Maximum Dry-Bulb
                if (!state.dataIPShortCut->lNumericFieldBlanks(6)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                        state.dataIPShortCut->rNumericArgs(6); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(6) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                bool errFlag = false;
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::HumRatio;
                state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                      errFlag,
                                                                      state.dataIPShortCut->cAlphaFieldNames(5) + " - Humidity-Ratio",
                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                      "Severe",
                                                                      ">= 0",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue >= 0.0),
                                                                      "<= .03",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue <= 0.03),
                                                                      {},
                                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "Enthalpy")) {
                state.dataIPShortCut->cAlphaArgs(5) = "Enthalpy";
                //   N7,  \field Enthalpy at Maximum Dry-Bulb {J/kg}.
                if (!state.dataIPShortCut->lNumericFieldBlanks(7)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                        state.dataIPShortCut->rNumericArgs(7); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(7) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
                bool errFlag = false;
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::Enthalpy;
                state.dataInputProcessing->inputProcessor->rangeCheck(state,
                                                                      errFlag,
                                                                      state.dataIPShortCut->cAlphaFieldNames(5) + " - Enthalpy",
                                                                      "SizingPeriod:DesignDay",
                                                                      "Severe",
                                                                      ">= 0.0",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue >= 0.0),
                                                                      "<= 130000",
                                                                      (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue <= 130000.0),
                                                                      {},
                                                                      state.dataWeatherManager->DesDayInput(EnvrnNum).Title);
                if (errFlag) {
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "RelativeHumiditySchedule")) {
                state.dataIPShortCut->cAlphaArgs(5) = "RelativeHumiditySchedule";
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::RelHumSch;
                units = "[%]";
                unitType = OutputProcessor::Unit::Perc;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "WetBulbProfileMultiplierSchedule")) {
                state.dataIPShortCut->cAlphaArgs(5) = "WetBulbProfileMultiplierSchedule";
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::WBProfMul;
                units = "[]";
                unitType = OutputProcessor::Unit::None;
                if (!state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                        state.dataIPShortCut->rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(5) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "WetBulbProfileDifferenceSchedule")) {
                state.dataIPShortCut->cAlphaArgs(5) = "WetBulbProfileDifferenceSchedule";
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::WBProfDif;
                units = "[]";
                unitType = OutputProcessor::Unit::None;
                if (!state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                        state.dataIPShortCut->rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(5) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "WetBulbProfileDefaultMultipliers")) {
                state.dataIPShortCut->cAlphaArgs(5) = "WetBulbProfileDefaultMultipliers";
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::WBProfDef;
                if (!state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue =
                        state.dataIPShortCut->rNumericArgs(5); // Humidity Indicating Conditions at Max Dry-Bulb
                } else {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(5) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }
            } else {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                     "\", invalid data.");
                ShowContinueError(
                    state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\".");
                ShowContinueError(state, "WetBulb will be used. Maximum Dry Bulb will be used as WetBulb at Maximum Dry Bulb.");
                state.dataIPShortCut->cAlphaArgs(5) = "WetBulb";
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType = DDHumIndType::WetBulb;
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue = state.dataIPShortCut->rNumericArgs(3);
            }

            // resolve humidity schedule if needed
            //   A6,  \field Humidity Condition Day Schedule Name
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::RelHumSch ||
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfMul ||
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDif) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(6) + " is blank.");
                    ShowContinueError(state,
                                      "..field is required when " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                } else {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr =
                        ScheduleManager::GetDayScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
                    if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr == 0) {
                        ShowWarningError(state,
                                         state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                             "\", invalid data.");
                        ShowContinueError(state,
                                          "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" +
                                              state.dataIPShortCut->cAlphaArgs(6) + "\".");
                        ShowContinueError(state, "Default Humidity will be used (constant for day using Humidity Indicator Temp).");
                        // reset HumIndType ?
                    } else {

                        ScheduleManager::GetSingleDayScheduleValues(state,
                                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr,
                                                                    state.dataWeatherManager->DDHumIndModifier(_, _, EnvrnNum));

                        int schPtr = General::FindNumberInList(state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr,
                                                               state.dataWeatherManager->SPSiteScheduleNamePtr,
                                                               state.dataWeatherManager->NumSPSiteScheduleNamePtrs);
                        if ((schPtr == 0) || (state.dataWeatherManager->SPSiteScheduleUnits(schPtr) != units)) {
                            ++state.dataWeatherManager->NumSPSiteScheduleNamePtrs;
                            state.dataWeatherManager->SPSiteScheduleNamePtr(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) =
                                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr;
                            state.dataWeatherManager->SPSiteScheduleUnits(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable(state,
                                                "Sizing Period Site Humidity Condition Schedule Value",
                                                unitType,
                                                state.dataWeatherManager->SPSiteHumidityConditionScheduleValue(EnvrnNum),
                                                OutputProcessor::SOVTimeStepType::Zone,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataIPShortCut->cAlphaArgs(6));
                        }

                        switch (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType) {
                        case DDHumIndType::RelHumSch:
                            if (!ScheduleManager::CheckDayScheduleValueMinMax(
                                    state, state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr, 0.0, false, 100.0, false)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                ShowContinueError(state,
                                                  "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" +
                                                      state.dataIPShortCut->cAlphaArgs(6) + "\".");
                                ShowContinueError(state, "Specified [Scheduled] Relative Humidity Values are not within [0.0, 100.0]");
                                ErrorsFound = true;
                            }
                            break;
                        case DDHumIndType::WBProfMul:
                            // multiplier: use schedule value, check 0 <= v <= 1
                            if (!ScheduleManager::CheckDayScheduleValueMinMax(
                                    state, state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr, 0.0, false, 1.0, false)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                ShowContinueError(state,
                                                  "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" +
                                                      state.dataIPShortCut->cAlphaArgs(6) + "\".");
                                ShowContinueError(state, "..Specified [Schedule] Wet-bulb Profile Range Multiplier Values are not within [0.0, 1.0]");
                                ErrorsFound = true;
                            }
                            break;
                        case DDHumIndType::WBProfDif:
                            if (!ScheduleManager::CheckDayScheduleValueMinMax(
                                    state, state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndSchPtr, 0.0, false)) {
                                ShowSevereError(state,
                                                state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", invalid data.");
                                ShowContinueError(state,
                                                  "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" +
                                                      state.dataIPShortCut->cAlphaArgs(6) + "\".");
                                ShowSevereError(state, "Some [Schedule] Wet-bulb Profile Difference Values are < 0.0 [would make max larger].");
                                ErrorsFound = true;
                            }
                        default:
                            break;
                        }
                    }
                }

            } else if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDef) {
                // re WetBulbProfileDefaultMultipliers
                Real64 LastHrValue = DefaultTempRangeMult[23];
                for (int hour = 1; hour <= 24; ++hour) {
                    for (int ts = 1; ts <= state.dataGlobal->NumOfTimeStepInHour; ++ts) {
                        Real64 WNow = state.dataWeatherManager->Interpolation(ts);
                        Real64 WPrev = 1.0 - WNow;
                        state.dataWeatherManager->DDHumIndModifier(ts, hour, EnvrnNum) = LastHrValue * WPrev + DefaultTempRangeMult[hour - 1] * WNow;
                    }
                    LastHrValue = DefaultTempRangeMult[hour - 1];
                }
            }

            // verify that design WB or DP <= design DB
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::DewPoint ||
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WetBulb ||
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfMul ||
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDef ||
                state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::WBProfDif) {
                if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue > state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                         "\", range check data.");
                    ShowContinueError(state,
                                      format("..Humidity Indicator Temperature at Max Temperature={:.1R} > Max DryBulb={:.1R}",
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue,
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb));
                    ShowContinueError(state, ".." + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\".");
                    ShowContinueError(state, "..Conditions for day will be set to Relative Humidity = 100%");
                    if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType == DDHumIndType::DewPoint) {
                        state.dataWeatherManager->DesDayInput(EnvrnNum).DewPointNeedsSet = true;
                    } else {
                        // wet-bulb
                        state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue = state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb;
                    }
                }
            }

            //   A10, \field Solar Model Indicator
            if (state.dataIPShortCut->lAlphaFieldBlanks(10) || UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "ASHRAEClearSky") ||
                UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "CLEARSKY")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel = DesignDaySolarModel::ASHRAE_ClearSky;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "ZhangHuang")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel = DesignDaySolarModel::Zhang_Huang;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "ASHRAETau")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel = DesignDaySolarModel::ASHRAE_Tau;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "ASHRAETau2017")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel = DesignDaySolarModel::ASHRAE_Tau2017;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "Schedule")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel = DesignDaySolarModel::SolarModel_Schedule;
            } else {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                     "\", invalid data.");
                ShowContinueError(
                    state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(10) + "=\"" + state.dataIPShortCut->cAlphaArgs(10) + "\".");
                ShowContinueError(state, "Model used will be ASHRAE ClearSky");
                state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel = DesignDaySolarModel::ASHRAE_ClearSky;
            }

            if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::SolarModel_Schedule) {
                //   A11, \field Beam Solar Day Schedule Name
                if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).BeamSolarSchPtr =
                        ScheduleManager::GetDayScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
                    if (state.dataWeatherManager->DesDayInput(EnvrnNum).BeamSolarSchPtr == 0) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                            "\", invalid data.");
                        ShowContinueError(state,
                                          "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(11) + "=\"" +
                                              state.dataIPShortCut->cAlphaArgs(11) + "\".");
                        ShowContinueError(state, "..Required when " + state.dataIPShortCut->cAlphaFieldNames(10) + " indicates \"Schedule\".");
                        ErrorsFound = true;
                    } else {
                        ScheduleManager::GetSingleDayScheduleValues(state,
                                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).BeamSolarSchPtr,
                                                                    state.dataWeatherManager->DDBeamSolarValues(_, _, EnvrnNum));
                        int schPtr = General::FindNumberInList(state.dataWeatherManager->DesDayInput(EnvrnNum).BeamSolarSchPtr,
                                                               state.dataWeatherManager->SPSiteScheduleNamePtr,
                                                               state.dataWeatherManager->NumSPSiteScheduleNamePtrs);
                        units = "[W/m2]";
                        unitType = OutputProcessor::Unit::W_m2;
                        if ((schPtr == 0) || (state.dataWeatherManager->SPSiteScheduleUnits(schPtr) != units)) {
                            ++state.dataWeatherManager->NumSPSiteScheduleNamePtrs;
                            state.dataWeatherManager->SPSiteScheduleNamePtr(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) =
                                state.dataWeatherManager->DesDayInput(EnvrnNum).BeamSolarSchPtr;
                            state.dataWeatherManager->SPSiteScheduleUnits(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable(state,
                                                "Sizing Period Site Beam Solar Schedule Value",
                                                unitType,
                                                state.dataWeatherManager->SPSiteBeamSolarScheduleValue(EnvrnNum),
                                                OutputProcessor::SOVTimeStepType::Zone,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataIPShortCut->cAlphaArgs(11));
                        }
                        if (!ScheduleManager::CheckDayScheduleValueMinMax(
                                state, state.dataWeatherManager->DesDayInput(EnvrnNum).BeamSolarSchPtr, 0.0, false)) {
                            ShowSevereError(state,
                                            state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", invalid data.");
                            ShowContinueError(state,
                                              "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(11) + "=\"" +
                                                  state.dataIPShortCut->cAlphaArgs(11) + "\".");
                            ShowContinueError(state, "..Specified [Schedule] Values are not >= 0.0");
                            ErrorsFound = true;
                        }
                    }
                } else { // should have entered beam schedule
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(11) + " is blank.");
                    ErrorsFound = true;
                }
                //   A12, \field Diffuse Solar Day Schedule Name
                if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                    state.dataWeatherManager->DesDayInput(EnvrnNum).DiffuseSolarSchPtr =
                        ScheduleManager::GetDayScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(12));
                    if (state.dataWeatherManager->DesDayInput(EnvrnNum).DiffuseSolarSchPtr == 0) {
                        ShowSevereError(state,
                                        state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                            "\", invalid data.");
                        ShowContinueError(state,
                                          "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(12) + "=\"" +
                                              state.dataIPShortCut->cAlphaArgs(12) + "\".");
                        ShowContinueError(state, "..Required when " + state.dataIPShortCut->cAlphaFieldNames(10) + " indicates \"Schedule\".");
                        ErrorsFound = true;
                    } else {
                        ScheduleManager::GetSingleDayScheduleValues(state,
                                                                    state.dataWeatherManager->DesDayInput(EnvrnNum).DiffuseSolarSchPtr,
                                                                    state.dataWeatherManager->DDDiffuseSolarValues(_, _, EnvrnNum));
                        int schPtr = General::FindNumberInList(state.dataWeatherManager->DesDayInput(EnvrnNum).DiffuseSolarSchPtr,
                                                               state.dataWeatherManager->SPSiteScheduleNamePtr,
                                                               state.dataWeatherManager->NumSPSiteScheduleNamePtrs);
                        units = "[W/m2]";
                        unitType = OutputProcessor::Unit::W_m2;
                        if ((schPtr == 0) || (state.dataWeatherManager->SPSiteScheduleUnits(schPtr) != units)) {
                            ++state.dataWeatherManager->NumSPSiteScheduleNamePtrs;
                            state.dataWeatherManager->SPSiteScheduleNamePtr(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) =
                                state.dataWeatherManager->DesDayInput(EnvrnNum).DiffuseSolarSchPtr;
                            state.dataWeatherManager->SPSiteScheduleUnits(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) = units;
                            SetupOutputVariable(state,
                                                "Sizing Period Site Diffuse Solar Schedule Value",
                                                unitType,
                                                state.dataWeatherManager->SPSiteDiffuseSolarScheduleValue(EnvrnNum),
                                                OutputProcessor::SOVTimeStepType::Zone,
                                                OutputProcessor::SOVStoreType::Average,
                                                state.dataIPShortCut->cAlphaArgs(12));
                        }
                        if (!ScheduleManager::CheckDayScheduleValueMinMax(
                                state, state.dataWeatherManager->DesDayInput(EnvrnNum).DiffuseSolarSchPtr, 0.0, false)) {
                            ShowSevereError(state,
                                            state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                state.dataWeatherManager->DesDayInput(EnvrnNum).Title + "\", invalid data.");
                            ShowContinueError(state,
                                              "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(12) + "=\"" +
                                                  state.dataIPShortCut->cAlphaArgs(12) + "\".");
                            ShowContinueError(state, "..Specified [Schedule] Values are not >= 0.0");
                            ErrorsFound = true;
                        }
                    }
                } else { // should have entered diffuse schedule
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(12) + " is blank.");
                    ErrorsFound = true;
                }
            }

            if (state.dataWeatherManager->DesDayInput(EnvrnNum).SolarModel == DesignDaySolarModel::ASHRAE_ClearSky) {
                if (state.dataIPShortCut->lNumericFieldBlanks(14)) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                         "\", invalid data.");
                    ShowContinueError(state, "..invalid field: " + state.dataIPShortCut->cNumericFieldNames(14) + " is blank.");
                    ShowContinueError(state, "..Zero clear sky (no solar) will be used.");
                }
            }

            // Validate Design Day Month

            switch (state.dataWeatherManager->DesDayInput(EnvrnNum).Month) {
            case 1:
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
            case 12:
                if (state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth > 31) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state,
                                      format(".. invalid field: {}=[{}], Month=[{}].",
                                             state.dataIPShortCut->cNumericFieldNames(2),
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth,
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).Month));
                    ErrorsFound = true;
                }
                break;
            case 4:
            case 6:
            case 9:
            case 11:
                if (state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth > 30) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state,
                                      format(".. invalid {}=[{}], Month=[{}].",
                                             state.dataIPShortCut->cNumericFieldNames(2),
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth,
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).Month));
                    ErrorsFound = true;
                }
                break;
            case 2:
                if (state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth > 28) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                        "\", invalid data.");
                    ShowContinueError(state,
                                      format(".. invalid {}=[{}], Month=[{}].",
                                             state.dataIPShortCut->cNumericFieldNames(2),
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth,
                                             state.dataWeatherManager->DesDayInput(EnvrnNum).Month));
                    ErrorsFound = true;
                }
                break;
            default:
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                    "\", invalid data.");
                ShowContinueError(state,
                                  format(".. invalid {} invalid (Month) [{}].",
                                         state.dataIPShortCut->cNumericFieldNames(1),
                                         state.dataWeatherManager->DesDayInput(EnvrnNum).Month));
                ErrorsFound = true;
                break;
            }

            //   A9,  \field Daylight Saving Time Indicator
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "Yes")) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).DSTIndicator = 1;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "No") || state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                state.dataWeatherManager->DesDayInput(EnvrnNum).DSTIndicator = 0;
            } else {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                     "\", invalid data.");
                ShowContinueError(state,
                                  "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(9) + "=\"" + state.dataIPShortCut->cAlphaArgs(9) +
                                      R"(". "No" will be used.)");
                state.dataWeatherManager->DesDayInput(EnvrnNum).DSTIndicator = 0;
            }

            //   A2,  \field Day Type
            state.dataWeatherManager->DesDayInput(EnvrnNum).DayType =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), ValidNames, 12);
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).DayType == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataWeatherManager->DesDayInput(EnvrnNum).Title +
                                    "\", invalid data.");
                ShowContinueError(
                    state, "..invalid field: " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ShowContinueError(state,
                                  "Valid values are "
                                  "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Holiday,SummerDesignDay,WinterDesignDay,CustomDay1,"
                                  "CustomDay2.");
                ErrorsFound = true;
            }

            state.dataWeatherManager->Environment(EnvrnNum).Title = state.dataWeatherManager->DesDayInput(EnvrnNum).Title;
            state.dataWeatherManager->Environment(EnvrnNum).KindOfEnvrn = DataGlobalConstants::KindOfSim::DesignDay;
            state.dataWeatherManager->Environment(EnvrnNum).DesignDayNum = EnvrnNum;
            state.dataWeatherManager->Environment(EnvrnNum).RunPeriodDesignNum = 0;
            state.dataWeatherManager->Environment(EnvrnNum).TotalDays = 1;
            state.dataWeatherManager->Environment(EnvrnNum).StartMonth = state.dataWeatherManager->DesDayInput(EnvrnNum).Month;
            state.dataWeatherManager->Environment(EnvrnNum).StartDay = state.dataWeatherManager->DesDayInput(EnvrnNum).DayOfMonth;
            state.dataWeatherManager->Environment(EnvrnNum).EndMonth = state.dataWeatherManager->Environment(EnvrnNum).StartMonth;
            state.dataWeatherManager->Environment(EnvrnNum).EndDay = state.dataWeatherManager->Environment(EnvrnNum).StartDay;
            state.dataWeatherManager->Environment(EnvrnNum).DayOfWeek = 0;
            state.dataWeatherManager->Environment(EnvrnNum).UseDST = false;
            state.dataWeatherManager->Environment(EnvrnNum).UseHolidays = false;
            state.dataWeatherManager->Environment(EnvrnNum).StartJDay = state.dataWeatherManager->DesignDay(EnvrnNum).DayOfYear;
            state.dataWeatherManager->Environment(EnvrnNum).EndJDay = state.dataWeatherManager->Environment(EnvrnNum).StartJDay;

            // create predefined report on design day
            std::string envTitle = state.dataWeatherManager->DesDayInput(EnvrnNum).Title;
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDDmaxDB, envTitle, state.dataWeatherManager->DesDayInput(EnvrnNum).MaxDryBulb);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDDrange, envTitle, state.dataWeatherManager->DesDayInput(EnvrnNum).DailyDBRange);
            if (state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType != DDHumIndType::RelHumSch) {
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchDDhumid, envTitle, state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndValue);
            } else {
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchDDhumid, envTitle, "N/A");
            }
            OutputReportPredefined::PreDefTableEntry(state,
                                                     state.dataOutRptPredefined->pdchDDhumTyp,
                                                     envTitle,
                                                     DDHumIndTypeStringRep.at(state.dataWeatherManager->DesDayInput(EnvrnNum).HumIndType));
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDDwindSp, envTitle, state.dataWeatherManager->DesDayInput(EnvrnNum).WindSpeed);
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchDDwindDr, envTitle, state.dataWeatherManager->DesDayInput(EnvrnNum).WindDir);
        }
    }

    void GetLocationInfo(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   October 1997
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the location info from the IDF file; latitude,
        //  longitude and time zone number.

        state.dataIPShortCut->cCurrentModuleObject = "Site:Location";
        int const NumLocations = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (NumLocations > 1) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
            ErrorsFound = true;
        }

        if (NumLocations == 1) {
            int LocNumAlpha;             // Number of alpha names being passed
            int LocNumProp;              // Number of properties being passed
            int IOStat;                  // IO Status when calling get input subroutine
            Array1D_string LocNames(1);  // Temp Array to transfer location info
            Array1D<Real64> LocProps(4); // Temporary array to transfer location info
            // Call Input Get routine to retrieve Location information
            state.dataInputProcessing->inputProcessor->getObjectItem(
                state, state.dataIPShortCut->cCurrentModuleObject, 1, LocNames, LocNumAlpha, LocProps, LocNumProp, IOStat);

            // set latitude, longitude, and time zone number variables
            state.dataWeatherManager->LocationTitle = LocNames(1);
            state.dataEnvrn->Latitude = LocProps(1);
            state.dataEnvrn->Longitude = LocProps(2);
            state.dataEnvrn->TimeZoneNumber = LocProps(3);
            state.dataEnvrn->Elevation = LocProps(4);
            state.dataWeatherManager->LocationGathered = true;
        }
    }

    void GetWeatherProperties(EnergyPlusData &state, bool &ErrorsFound)
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

        static constexpr std::string_view RoutineName("GetWeatherProperties:");

        int Found;
        int envFound;

        state.dataIPShortCut->cCurrentModuleObject = "WeatherProperty:SkyTemperature";
        state.dataWeatherManager->NumWPSkyTemperatures =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        state.dataWeatherManager->WPSkyTemperature.allocate(state.dataWeatherManager->NumWPSkyTemperatures); // by default, not used.

        for (int i = 1; i <= state.dataWeatherManager->NumWPSkyTemperatures; ++i) {
            int IOStat;
            int NumAlpha;
            int NumNumerics;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     i,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlpha,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumerics,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            if (state.dataIPShortCut->cAlphaArgs(1).empty()) {
                Found = 0;
                for (int j = 1; j <= state.dataWeatherManager->NumOfEnvrn; ++j) {
                    if (state.dataWeatherManager->Environment(j).KindOfEnvrn != DataGlobalConstants::KindOfSim::RunPeriodWeather) continue;
                    if (state.dataWeatherManager->Environment(j).WP_Type1 != 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(1) + "\", indicated Environment Name already assigned.");
                        if (!state.dataWeatherManager->Environment(j).Title.empty()) {
                            ShowContinueError(state,
                                              "...Environment=\"" + state.dataWeatherManager->Environment(j).Title + "\", already using " +
                                                  state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                  state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(j).WP_Type1).Name +
                                                  "\".");
                        } else {
                            ShowContinueError(state,
                                              "... Runperiod Environment, already using " + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                  state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(j).WP_Type1).Name +
                                                  "\".");
                        }
                        ErrorsFound = true;
                    } else {
                        state.dataWeatherManager->Environment(j).WP_Type1 = i;
                        Found = j;
                    }
                }
                if (Found == 0) {
                    ShowWarningError(state, "GetWeatherProperties: WeatherProperty:SkyTemperature=blank, no run periods found.");
                    ShowContinueError(state, "...SkyTemperature will not be applied.");
                    continue;
                }
            } else { // really a name
                Found = UtilityRoutines::FindItemInList(
                    state.dataIPShortCut->cAlphaArgs(1), state.dataWeatherManager->Environment, &EnvironmentData::Title);
                envFound = Found;
                if (Found == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\", invalid Environment Name referenced.");
                    ShowContinueError(state, "...remainder of object not processed.");
                    ErrorsFound = true;
                    continue;
                } else {
                    if (state.dataWeatherManager->Environment(Found).WP_Type1 != 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(1) + "\", indicated Environment Name already assigned.");
                        ShowContinueError(state,
                                          "...Environment=\"" + state.dataWeatherManager->Environment(Found).Title + "\", already using " +
                                              state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                              state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(Found).WP_Type1).Name +
                                              "\".");
                        ErrorsFound = true;
                    } else {
                        state.dataWeatherManager->Environment(Found).WP_Type1 = i;
                    }
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(1)) {
                UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);
                state.dataWeatherManager->WPSkyTemperature(i).Name = state.dataIPShortCut->cAlphaArgs(1); // Name
            } else {
                state.dataWeatherManager->WPSkyTemperature(i).Name = "All RunPeriods";
            }
            // Validate Calculation Type.
            std::string units;
            OutputProcessor::Unit unitType;
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "ScheduleValue")) {
                state.dataWeatherManager->WPSkyTemperature(i).CalculationType = EmissivityCalcType::ScheduleValue;
                state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = true;
                units = "[C]";
                unitType = OutputProcessor::Unit::C;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "DifferenceScheduleDryBulbValue")) {
                state.dataWeatherManager->WPSkyTemperature(i).CalculationType = EmissivityCalcType::DryBulbDelta;
                state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = true;
                units = "[deltaC]";
                unitType = OutputProcessor::Unit::deltaC;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "DifferenceScheduleDewPointValue")) {
                state.dataWeatherManager->WPSkyTemperature(i).CalculationType = EmissivityCalcType::DewPointDelta;
                state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = true;
                units = "[deltaC]";
                unitType = OutputProcessor::Unit::deltaC;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "Brunt")) {
                state.dataWeatherManager->WPSkyTemperature(i).CalculationType = EmissivityCalcType::BruntModel;
                state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = false;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "Idso")) {
                state.dataWeatherManager->WPSkyTemperature(i).CalculationType = EmissivityCalcType::IdsoModel;
                state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = false;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "BerdahlMartin")) {
                state.dataWeatherManager->WPSkyTemperature(i).CalculationType = EmissivityCalcType::BerdahlMartinModel;
                state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = false;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "ClarkAllen")) {
                state.dataWeatherManager->WPSkyTemperature(i).CalculationType = EmissivityCalcType::ClarkAllenModel;
                state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = false;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '.');
                ShowContinueError(state,
                                  "...entered value=\"" + state.dataIPShortCut->cAlphaArgs(2) +
                                      "\", should be one of: ScheduleValue, DifferenceScheduleDryBulbValue, DifferenceScheduleDewPointValue.");
                ErrorsFound = true;
            }

            if (state.dataWeatherManager->WPSkyTemperature(i).IsSchedule) {
                state.dataWeatherManager->WPSkyTemperature(i).ScheduleName = state.dataIPShortCut->cAlphaArgs(3);
                if (state.dataWeatherManager->Environment(Found).KindOfEnvrn == DataGlobalConstants::KindOfSim::RunPeriodWeather ||
                    state.dataWeatherManager->Environment(Found).KindOfEnvrn == DataGlobalConstants::KindOfSim::RunPeriodDesign) {
                    state.dataWeatherManager->WPSkyTemperature(i).ScheduleName = state.dataIPShortCut->cAlphaArgs(3);
                    // See if it's a schedule.
                    Found = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                    if (Found == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(1) + "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + '.');
                        ShowContinueError(state, "...Entered name=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
                        ShowContinueError(state,
                                          "...Should be a full year schedule (\"Schedule:Year\", \"Schedule:Compact\", \"Schedule:File\", or "
                                          "\"Schedule:Constant\" objects.");
                        ErrorsFound = true;
                    } else {
                        state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = true;
                        state.dataWeatherManager->WPSkyTemperature(i).SchedulePtr = Found;
                    }
                } else { // See if it's a valid schedule.
                    Found = ScheduleManager::GetDayScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                    if (Found == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(1) + "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + '.');
                        ShowContinueError(state, "...Entered name=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
                        ShowContinueError(
                            state,
                            R"(...Should be a single day schedule ("Schedule:Day:Hourly", "Schedule:Day:Interval", or "Schedule:Day:List" objects.)");
                        ErrorsFound = true;
                    } else {
                        if (envFound != 0) {
                            int schPtr = General::FindNumberInList(
                                Found, state.dataWeatherManager->SPSiteScheduleNamePtr, state.dataWeatherManager->NumSPSiteScheduleNamePtrs);
                            if ((schPtr == 0) || (state.dataWeatherManager->SPSiteScheduleUnits(schPtr) != units)) {
                                ++state.dataWeatherManager->NumSPSiteScheduleNamePtrs;
                                state.dataWeatherManager->SPSiteScheduleNamePtr(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) = Found;
                                state.dataWeatherManager->SPSiteScheduleUnits(state.dataWeatherManager->NumSPSiteScheduleNamePtrs) = units;
                                SetupOutputVariable(state,
                                                    "Sizing Period Site Sky Temperature Schedule Value",
                                                    unitType,
                                                    state.dataWeatherManager->SPSiteSkyTemperatureScheduleValue(envFound),
                                                    OutputProcessor::SOVTimeStepType::Zone,
                                                    OutputProcessor::SOVStoreType::Average,
                                                    state.dataIPShortCut->cAlphaArgs(3));
                            }
                            state.dataWeatherManager->WPSkyTemperature(i).IsSchedule = true;
                            state.dataWeatherManager->WPSkyTemperature(i).SchedulePtr = Found;
                        }
                    }
                }
            }

            if (!state.dataWeatherManager->WPSkyTemperature(i).IsSchedule && !state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Yes")) {
                    state.dataWeatherManager->WPSkyTemperature(i).UseWeatherFileHorizontalIR = true;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "No")) {
                    state.dataWeatherManager->WPSkyTemperature(i).UseWeatherFileHorizontalIR = false;
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\", invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + '.');
                    ShowContinueError(state, "...entered value=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\", should be Yes or No.");
                    ErrorsFound = true;
                }
            } else {
                state.dataWeatherManager->WPSkyTemperature(i).UseWeatherFileHorizontalIR = true;
            }
        }
        for (int envrn = 1; envrn <= state.dataWeatherManager->NumOfEnvrn; ++envrn) {
            if (state.dataWeatherManager->Environment(envrn).WP_Type1 != 0 &&
                state.dataWeatherManager->NumWPSkyTemperatures >= state.dataWeatherManager->Environment(envrn).WP_Type1) {
                state.dataWeatherManager->Environment(envrn).SkyTempModel =
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(envrn).WP_Type1).CalculationType;
                state.dataWeatherManager->Environment(envrn).UseWeatherFileHorizontalIR =
                    state.dataWeatherManager->WPSkyTemperature(state.dataWeatherManager->Environment(envrn).WP_Type1).UseWeatherFileHorizontalIR;
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

        // Initialize Site:GroundTemperature:BuildingSurface object
        state.dataWeatherManager->siteBuildingSurfaceGroundTempsPtr =
            GroundTemperatureManager::GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:BUILDINGSURFACE", "");
        if (state.dataWeatherManager->siteBuildingSurfaceGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager->siteBuildingSurfaceGroundTempsPtr->errorsFound || ErrorsFound;
        }

        // Initialize Site:GroundTemperature:FCFactorMethod object
        state.dataWeatherManager->siteFCFactorMethodGroundTempsPtr =
            GroundTemperatureManager::GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:FCFACTORMETHOD", "");
        if (state.dataWeatherManager->siteFCFactorMethodGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager->siteFCFactorMethodGroundTempsPtr->errorsFound || ErrorsFound;
        }

        // Initialize Site:GroundTemperature:Shallow object
        state.dataWeatherManager->siteShallowGroundTempsPtr =
            GroundTemperatureManager::GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:SHALLOW", "");
        if (state.dataWeatherManager->siteShallowGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager->siteShallowGroundTempsPtr->errorsFound || ErrorsFound;
        }

        // Initialize Site:GroundTemperature:Deep object
        state.dataWeatherManager->siteDeepGroundTempsPtr =
            GroundTemperatureManager::GetGroundTempModelAndInit(state, "SITE:GROUNDTEMPERATURE:DEEP", "");
        if (state.dataWeatherManager->siteDeepGroundTempsPtr) {
            ErrorsFound = state.dataWeatherManager->siteDeepGroundTempsPtr->errorsFound || ErrorsFound;
        }
    }

    void GetGroundReflectances(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This file reads the Ground Reflectances from the input file (optional) and
        // places them in the monthly array.

        state.dataIPShortCut->cCurrentModuleObject = "Site:GroundReflectance";
        int nObjs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        if (nObjs != 0) {
            Array1D_string GndAlphas(1);  // Construction Alpha names defined
            Array1D<Real64> GndProps(12); // Temporary array to transfer ground reflectances
            if (nObjs == 1) {
                int GndNumAlpha; // Number of construction alpha names being passed
                int GndNumProp;  // dummy variable for properties being passed
                int IOStat;      // IO Status when calling get input subroutine
                // Get the object names for each construction from the input processor
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, state.dataIPShortCut->cCurrentModuleObject, 1, GndAlphas, GndNumAlpha, GndProps, GndNumProp, IOStat);

                if (GndNumProp < 12) {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Less than 12 values entered.");
                    ErrorsFound = true;
                }

                // Assign the ground reflectances to the variable
                state.dataWeatherManager->GroundReflectances({1, 12}) = GndProps({1, 12});

            } else {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
                ErrorsFound = true;
            }
        }

        // Write Final Ground Reflectance Information to the initialization output file
        print(state.files.eio,
              "{}\n",
              "! "
              "<Site:GroundReflectance>,Jan{dimensionless},Feb{dimensionless},Mar{dimensionless},Apr{dimensionless},"
              "May{dimensionless},Jun{dimensionless},Jul{dimensionless},Aug{dimensionless},Sep{dimensionless},Oct{"
              "dimensionless},Nov{dimensionless},Dec{dimensionless}");

        print(state.files.eio, " Site:GroundReflectance");
        for (int i = 1; i <= 12; ++i) {
            print(state.files.eio, ", {:5.2F}", state.dataWeatherManager->GroundReflectances(i));
        }
        print(state.files.eio, "\n");
    }

    void GetSnowGroundRefModifiers(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This file reads the Snow Ground Reflectance Modifiers from the input file (optional) and
        // places them in the variables.

        state.dataIPShortCut->cCurrentModuleObject = "Site:GroundReflectance:SnowModifier";
        int nObjs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);
        if (nObjs != 0) {
            Array1D_string GndAlphas(1); // Construction Alpha names defined
            Array1D<Real64> GndProps(2); // Temporary array to transfer ground reflectances
            if (nObjs == 1) {
                int GndNumAlpha; // Number of construction alpha names being passed
                int GndNumProp;  // dummy variable for properties being passed
                int IOStat;      // IO Status when calling get input subroutine
                // Get the object names for each construction from the input processor
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, state.dataIPShortCut->cCurrentModuleObject, 1, GndAlphas, GndNumAlpha, GndProps, GndNumProp, IOStat);

                // Assign the ground reflectances to the variable
                state.dataWeatherManager->SnowGndRefModifier = GndProps(1);
                state.dataWeatherManager->SnowGndRefModifierForDayltg = GndProps(2);

            } else {
                ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
                ErrorsFound = true;
            }
        }

        // Write Final Ground Reflectance Modifier Information to the initialization output file
        print(state.files.eio, "{}\n", "! <Site:GroundReflectance:SnowModifier>, Normal, Daylighting {dimensionless}");
        static constexpr std::string_view Format_720(" Site:GroundReflectance:SnowModifier, {:7.3F}, {:7.3F}\n");
        print(state.files.eio, Format_720, state.dataWeatherManager->SnowGndRefModifier, state.dataWeatherManager->SnowGndRefModifierForDayltg);

        print(state.files.eio,
              "{}\n",
              "! "
              "<Site:GroundReflectance:Snow>,Jan{dimensionless},Feb{dimensionless},Mar{dimensionless},Apr{"
              "dimensionless},May{dimensionless},Jun{dimensionless},Jul{dimensionless},Aug{dimensionless},Sep{"
              "dimensionless},Oct{dimensionless},Nov{dimensionless},Dec{dimensionless}");
        print(state.files.eio, "{}", " Site:GroundReflectance:Snow");
        for (int i = 1; i <= 12; ++i) {
            print(state.files.eio,
                  ", {:5.2F}",
                  max(min(state.dataWeatherManager->GroundReflectances(i) * state.dataWeatherManager->SnowGndRefModifier, 1.0), 0.0));
        }
        print(state.files.eio, "\n");
        print(state.files.eio,
              "{}\n",
              "! "
              "<Site:GroundReflectance:Snow:Daylighting>,Jan{dimensionless},Feb{dimensionless},Mar{dimensionless},Apr{"
              "dimensionless},May{dimensionless},Jun{dimensionless},Jul{dimensionless},Aug{dimensionless},Sep{"
              "dimensionless},Oct{dimensionless},Nov{dimensionless},Dec{dimensionless}");
        print(state.files.eio, " Site:GroundReflectance:Snow:Daylighting");
        for (nObjs = 1; nObjs <= 12; ++nObjs) {
            print(state.files.eio,
                  ", {:5.2F}",
                  max(min(state.dataWeatherManager->GroundReflectances(nObjs) * state.dataWeatherManager->SnowGndRefModifierForDayltg, 1.0), 0.0));
        }
        print(state.files.eio, "\n");
    }

    void GetWaterMainsTemperatures(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads the input data for the WATER MAINS TEMPERATURES object.

        state.dataIPShortCut->cCurrentModuleObject = "Site:WaterMainsTemperature";
        int NumObjects = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (NumObjects == 1) {
            int NumAlphas;               // Number of elements in the alpha array
            int NumNums;                 // Number of elements in the numeric array
            int IOStat;                  // IO Status when calling get input subroutine
            Array1D_string AlphArray(2); // Character string data
            Array1D<Real64> NumArray(2); // Numeric data
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     1,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            if (UtilityRoutines::SameString(AlphArray(1), "Schedule")) {
                state.dataWeatherManager->WaterMainsTempsMethod = WaterMainsTempCalcMethod::Schedule;
                state.dataWeatherManager->WaterMainsTempsScheduleName = AlphArray(2);
                state.dataWeatherManager->WaterMainsTempsSchedule = ScheduleManager::GetScheduleIndex(state, AlphArray(2));
                if (state.dataWeatherManager->WaterMainsTempsSchedule == 0) {
                    ShowSevereError(state,
                                    state.dataIPShortCut->cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' +
                                        AlphArray(2));
                    ErrorsFound = true;
                }

            } else if (UtilityRoutines::SameString(AlphArray(1), "Correlation")) {
                state.dataWeatherManager->WaterMainsTempsMethod = WaterMainsTempCalcMethod::Correlation;

                if (NumNums == 0) {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Missing Annual Average and Maximum Difference fields.");
                    ErrorsFound = true;
                } else if (NumNums == 1) {
                    ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Missing Maximum Difference field.");
                    ErrorsFound = true;
                } else {
                    state.dataWeatherManager->WaterMainsTempsAnnualAvgAirTemp = NumArray(1);
                    state.dataWeatherManager->WaterMainsTempsMaxDiffAirTemp = NumArray(2);
                }
            } else if (UtilityRoutines::SameString(AlphArray(1), "CorrelationFromWeatherFile")) {
                state.dataWeatherManager->WaterMainsTempsMethod = WaterMainsTempCalcMethod::CorrelationFromWeatherFile;
            } else {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                    AlphArray(1));
                ErrorsFound = true;
            }

        } else if (NumObjects > 1) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
            ErrorsFound = true;
        }
    }

    void CalcWaterMainsTemp(EnergyPlusData &state)
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

        switch (state.dataWeatherManager->WaterMainsTempsMethod) {
        case WaterMainsTempCalcMethod::Schedule:
            state.dataEnvrn->WaterMainsTemp = ScheduleManager::GetCurrentScheduleValue(state, state.dataWeatherManager->WaterMainsTempsSchedule);
            break;
        case WaterMainsTempCalcMethod::Correlation:
            state.dataEnvrn->WaterMainsTemp = WaterMainsTempFromCorrelation(
                state, state.dataWeatherManager->WaterMainsTempsAnnualAvgAirTemp, state.dataWeatherManager->WaterMainsTempsMaxDiffAirTemp);
            break;
        case WaterMainsTempCalcMethod::CorrelationFromWeatherFile:
            if (state.dataWeatherManager->OADryBulbAverage.OADryBulbWeatherDataProcessed) {
                state.dataEnvrn->WaterMainsTemp =
                    WaterMainsTempFromCorrelation(state,
                                                  state.dataWeatherManager->OADryBulbAverage.AnnualAvgOADryBulbTemp,
                                                  state.dataWeatherManager->OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff);
            } else {
                state.dataEnvrn->WaterMainsTemp = 10.0; // 50 F
            }
            break;
        default:
            state.dataEnvrn->WaterMainsTemp = 10.0; // 50 F
            break;
        }
    }

    Real64 WaterMainsTempFromCorrelation(EnergyPlusData &state, Real64 const AnnualOAAvgDryBulbTemp, Real64 const MonthlyOAAvgDryBulbTempMaxDiff)
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

        // Annual Average Outdoor Air Temperature (F)
        Real64 const Tavg = AnnualOAAvgDryBulbTemp * (9.0 / 5.0) + 32.0;
        // Maximum difference in monthly average outdoor air temperatures (deltaF)
        Real64 const Tdiff = MonthlyOAAvgDryBulbTempMaxDiff * (9.0 / 5.0);

        Real64 const Ratio = 0.4 + 0.01 * (Tavg - 44.0);
        Real64 const Lag = 35.0 - 1.0 * (Tavg - 44.0);
        Real64 constexpr Offset = 6.0;
        int const latitude_sign = (state.dataEnvrn->Latitude >= 0) ? 1 : -1;

        // calculated water main temp (F)
        Real64 CurrentWaterMainsTemp = Tavg + Offset +
                                       Ratio * (Tdiff / 2.0) * latitude_sign *
                                           std::sin((0.986 * (state.dataEnvrn->DayOfYear - 15.0 - Lag) - 90) * DataGlobalConstants::DegToRadians);

        if (CurrentWaterMainsTemp < 32.0) CurrentWaterMainsTemp = 32.0;

        // Convert F to C
        return (CurrentWaterMainsTemp - 32.0) * (5.0 / 9.0);
    }
    void GetWeatherStation(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads the input data for the WEATHER STATION object.

        state.dataIPShortCut->cCurrentModuleObject = "Site:WeatherStation";
        int const NumObjects = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        // Default conditions for a weather station in an open field at a height of 10 m. (These should match the IDD defaults.)
        Real64 WeatherFileWindSensorHeight = 10.0; // Height of the wind sensor at the weather station, i.e., weather file
        Real64 WeatherFileWindExp = 0.14;          // Exponent for the wind velocity profile at the weather station
        Real64 WeatherFileWindBLHeight = 270.0;    // Boundary layer height for the wind velocity profile at the weather station (m)
        Real64 WeatherFileTempSensorHeight = 1.5;  // Height of the air temperature sensor at the weather station (m)

        if (NumObjects == 1) {
            int NumAlphas;               // Number of elements in the alpha array
            int NumNums;                 // Number of elements in the numeric array
            int IOStat;                  // IO Status when calling get input subroutine
            Array1D_string AlphArray(1); // Character string data
            Array1D<Real64> NumArray(4); // Numeric data
            state.dataInputProcessing->inputProcessor->getObjectItem(
                state, state.dataIPShortCut->cCurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat);

            if (NumNums > 0) WeatherFileWindSensorHeight = NumArray(1);
            if (NumNums > 1) WeatherFileWindExp = NumArray(2);
            if (NumNums > 2) WeatherFileWindBLHeight = NumArray(3);
            if (NumNums > 3) WeatherFileTempSensorHeight = NumArray(4);

        } else if (NumObjects > 1) {
            ShowSevereError(state, state.dataIPShortCut->cCurrentModuleObject + ": Too many objects entered. Only one allowed.");
            ErrorsFound = true;
        }

        state.dataEnvrn->WeatherFileWindModCoeff = std::pow(WeatherFileWindBLHeight / WeatherFileWindSensorHeight, WeatherFileWindExp);
        state.dataEnvrn->WeatherFileTempModCoeff = DataEnvironment::AtmosphericTempGradient * DataEnvironment::EarthRadius *
                                                   WeatherFileTempSensorHeight / (DataEnvironment::EarthRadius + WeatherFileTempSensorHeight);

        // Write to the initialization output file
        print(state.files.eio,
              "{}\n",
              "! <Environment:Weather Station>,Wind Sensor Height Above Ground {m},Wind Speed Profile Exponent "
              "{},Wind Speed Profile Boundary Layer Thickness {m},Air Temperature Sensor Height Above Ground {m},Wind "
              "Speed Modifier Coefficient-Internal,Temperature Modifier Coefficient-Internal");

        // Formats
        static constexpr std::string_view Format_720("Environment:Weather Station,{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R}\n");
        print(state.files.eio,
              Format_720,
              WeatherFileWindSensorHeight,
              WeatherFileWindExp,
              WeatherFileWindBLHeight,
              WeatherFileTempSensorHeight,
              state.dataEnvrn->WeatherFileWindModCoeff,
              state.dataEnvrn->WeatherFileTempModCoeff);
    }

    void DayltgCurrentExtHorizIllum(EnergyPlusData &state)
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

        // SOLCOS(3), below, is the cosine of the solar zenith angle.
        if (state.dataEnvrn->SunIsUp) {
            // Exterior horizontal beam irradiance (W/m2)
            Real64 SDIRH = state.dataEnvrn->BeamSolarRad * state.dataEnvrn->SOLCOS(3);
            // Exterior horizontal sky diffuse irradiance (W/m2)
            Real64 SDIFH = state.dataEnvrn->DifSolarRad;
            // Fraction of sky covered by clouds
            state.dataEnvrn->CloudFraction = pow_2(SDIFH / (SDIRH + SDIFH + 0.0001));
            // Luminous efficacy of sky diffuse solar and beam solar (lumens/W);
            // Horizontal illuminance from sky and horizontal beam illuminance (lux)
            // obtained from solar quantities on weather file and luminous efficacy.

            DayltgLuminousEfficacy(state, state.dataEnvrn->PDIFLW, state.dataEnvrn->PDIRLW);
            state.dataEnvrn->HISKF = SDIFH * state.dataEnvrn->PDIFLW;
            state.dataEnvrn->HISUNF = SDIRH * state.dataEnvrn->PDIRLW;
            state.dataEnvrn->HISUNFnorm = state.dataEnvrn->BeamSolarRad * state.dataEnvrn->PDIRLW;
        } else {
            state.dataEnvrn->CloudFraction = 0.0;
            state.dataEnvrn->PDIFLW = 0.0;
            state.dataEnvrn->PDIRLW = 0.0;
            state.dataEnvrn->HISKF = 0.0;
            state.dataEnvrn->HISUNF = 0.0;
            state.dataEnvrn->HISUNFnorm = 0.0;
            state.dataEnvrn->SkyClearness = 0.0;
            state.dataEnvrn->SkyBrightness = 0.0;
        }
    }

    void DayltgLuminousEfficacy(EnergyPlusData &state,
                                Real64 &DiffLumEff, // Luminous efficacy of sky diffuse solar radiation (lum/W)
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

        Real64 const SunZenith = std::acos(state.dataEnvrn->SOLCOS(3));     // Solar zenith angle (radians)
        Real64 const SunAltitude = DataGlobalConstants::PiOvr2 - SunZenith; // Solar altitude angle (radians)
        Real64 const SinSunAltitude = std::sin(SunAltitude);
        // Clearness of sky. SkyClearness close to 1.0 corresponds to an overcast sky.
        // SkyClearness > 6 is a clear sky.
        // DifSolarRad is the diffuse horizontal irradiance.
        // BeamSolarRad is the direct normal irradiance.
        Real64 const Zeta = 1.041 * pow_3(SunZenith);
        state.dataEnvrn->SkyClearness =
            ((state.dataEnvrn->DifSolarRad + state.dataEnvrn->BeamSolarRad) / (state.dataEnvrn->DifSolarRad + 0.0001) + Zeta) / (1.0 + Zeta);
        // Relative optical air mass
        Real64 const AirMass = (1.0 - 0.1 * state.dataEnvrn->Elevation / 1000.0) /
                               (SinSunAltitude + 0.15 / std::pow(SunAltitude / DataGlobalConstants::DegToRadians + 3.885, 1.253));
        // In the following, 93.73 is the extraterrestrial luminous efficacy
        state.dataEnvrn->SkyBrightness = (state.dataEnvrn->DifSolarRad * 93.73) * AirMass / ExtraDirNormIll(state.dataEnvrn->Month);
        int ISkyClearness; // Sky clearness bin
        if (state.dataEnvrn->SkyClearness <= 1.065) {
            ISkyClearness = 1;
        } else if (state.dataEnvrn->SkyClearness <= 1.23) {
            ISkyClearness = 2;
        } else if (state.dataEnvrn->SkyClearness <= 1.50) {
            ISkyClearness = 3;
        } else if (state.dataEnvrn->SkyClearness <= 1.95) {
            ISkyClearness = 4;
        } else if (state.dataEnvrn->SkyClearness <= 2.80) {
            ISkyClearness = 5;
        } else if (state.dataEnvrn->SkyClearness <= 4.50) {
            ISkyClearness = 6;
        } else if (state.dataEnvrn->SkyClearness <= 6.20) {
            ISkyClearness = 7;
        } else {
            ISkyClearness = 8;
        }
        // Atmospheric moisture (cm of precipitable water)
        Real64 const AtmosMoisture = std::exp(0.07 * state.dataEnvrn->OutDewPointTemp - 0.075);
        // Sky diffuse luminous efficacy
        if (state.dataEnvrn->SkyBrightness <= 0.0) {
            DiffLumEff = 0.0;
        } else {
            DiffLumEff = ADiffLumEff(ISkyClearness) + BDiffLumEff(ISkyClearness) * AtmosMoisture +
                         CDiffLumEff(ISkyClearness) * state.dataEnvrn->SOLCOS(3) +
                         DDiffLumEff(ISkyClearness) * std::log(state.dataEnvrn->SkyBrightness);
        }
        // Direct normal luminous efficacy
        if (state.dataEnvrn->SkyBrightness <= 0.0) {
            DirLumEff = 0.0;
        } else {
            DirLumEff =
                max(0.0,
                    ADirLumEff(ISkyClearness) + BDirLumEff(ISkyClearness) * AtmosMoisture +
                        CDirLumEff(ISkyClearness) * std::exp(5.73 * SunZenith - 5.0) + DDirLumEff(ISkyClearness) * state.dataEnvrn->SkyBrightness);
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

        Real64 GetSTM;

        Array1D<Real64> longl({-12, 12}); // Lower Longitude value for a Time Zone
        Array1D<Real64> longh({-12, 12}); // Upper Longitude value for a Time Zone

        GetSTM = 0.0;

        longl(0) = -7.5;
        longh(0) = 7.5;
        for (int i = 1; i <= 12; ++i) {
            longl(i) = longl(i - 1) + 15.0;
            longh(i) = longh(i - 1) + 15.0;
        }
        for (int i = 1; i <= 12; ++i) {
            longl(-i) = longl(-i + 1) - 15.0;
            longh(-i) = longh(-i + 1) - 15.0;
        }
        Real64 temp = mod(Longitude, 360.0);
        if (temp > 180.0) temp -= 180.0;
        Real64 tz; // resultant tz meridian
        for (int i = -12; i <= 12; ++i) {
            if (temp > longl(i) && temp <= longh(i)) {
                tz = i;
                tz = mod(i, 24.0);
                GetSTM = tz;
                break;
            }
        }

        return GetSTM;
    }

    void ProcessEPWHeader(EnergyPlusData &state, std::string const &HeaderString, std::string &Line, bool &ErrorsFound)
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

        WeatherManager::DateType dateType;
        int NumHdArgs;

        // Strip off Header value from Line
        std::string::size_type Pos = index(Line, ',');
        if ((Pos == std::string::npos) && (!has_prefixi(HeaderString, "COMMENTS"))) {
            ShowSevereError(state, "Invalid Header line in in.epw -- no commas");
            ShowContinueError(state, "Line=" + Line);
            ShowFatalError(state, "Previous conditions cause termination.");
        }
        if (Pos != std::string::npos) Line.erase(0, Pos + 1);

        {
            auto const HeaderStringUppercase(UtilityRoutines::MakeUPPERCase(HeaderString));

            if (HeaderStringUppercase == "LOCATION") {

                // LOCATION, A1 [City], A2 [State/Province/Region], A3 [Country],
                // A4 [Source], N1 [WMO], N2 [Latitude],
                // N3 [Longitude], N4 [Time Zone], N5 [Elevation {m}]

                NumHdArgs = 9;
                for (int i = 1; i <= NumHdArgs; ++i) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos == std::string::npos) {
                        if (len(Line) == 0) {
                            while (Pos == std::string::npos) {
                                Line = state.files.inputWeatherFile.readLine().data;
                                strip(Line);
                                uppercase(Line);
                                Pos = index(Line, ',');
                            }
                        } else {
                            Pos = len(Line);
                        }
                    }

                    switch (i) {
                    case 1:
                        state.dataWeatherManager->EPWHeaderTitle = stripped(Line.substr(0, Pos));
                        break;
                    case 2:
                    case 3:
                    case 4:
                        state.dataWeatherManager->EPWHeaderTitle =
                            strip(state.dataWeatherManager->EPWHeaderTitle) + ' ' + stripped(Line.substr(0, Pos));
                        break;
                    case 5:
                        state.dataWeatherManager->EPWHeaderTitle += " WMO#=" + stripped(Line.substr(0, Pos));
                        break;
                    case 6:
                    case 7:
                    case 8:
                    case 9: {
                        bool errFlag;
                        Real64 const Number = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                        if (!errFlag) {
                            switch (i) {
                            case 6:
                                state.dataWeatherManager->WeatherFileLatitude = Number;
                                break;
                            case 7:
                                state.dataWeatherManager->WeatherFileLongitude = Number;
                                break;
                            case 8:
                                state.dataWeatherManager->WeatherFileTimeZone = Number;
                                break;
                            case 9:
                                state.dataWeatherManager->WeatherFileElevation = Number;
                                break;
                            default:
                                break;
                            }
                        }
                    } break;
                    default:
                        ShowSevereError(state, "GetEPWHeader:LOCATION, invalid numeric=" + Line.substr(0, Pos));
                        ErrorsFound = true;
                        break;
                    }
                    Line.erase(0, Pos + 1);
                }
                state.dataEnvrn->WeatherFileLocationTitle = stripped(state.dataWeatherManager->EPWHeaderTitle);

            } else if (HeaderStringUppercase == "TYPICAL/EXTREME PERIODS") {
                strip(Line);
                Pos = index(Line, ',');
                if (Pos == std::string::npos) {
                    if (len(Line) == 0) {
                        while (Pos == std::string::npos && len(Line) == 0) {
                            Line = state.files.inputWeatherFile.readLine().data;
                            strip(Line);
                            Pos = index(Line, ',');
                        }
                    } else {
                        Pos = len(Line);
                    }
                }
                bool IOStatus;
                state.dataWeatherManager->NumEPWTypExtSets = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                Line.erase(0, Pos + 1);
                state.dataWeatherManager->TypicalExtremePeriods.allocate(state.dataWeatherManager->NumEPWTypExtSets);
                int TropExtremeCount = 0;
                for (int i = 1; i <= state.dataWeatherManager->NumEPWTypExtSets; ++i) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        state.dataWeatherManager->TypicalExtremePeriods(i).Title = Line.substr(0, Pos);
                        Line.erase(0, Pos + 1);
                    } else {
                        ShowWarningError(state, "ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)=" + Line.substr(0, Pos));
                        ShowContinueError(state, format("...on processing Typical/Extreme period #{}", i));
                        state.dataWeatherManager->NumEPWTypExtSets = i - 1;
                        break;
                    }
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        state.dataWeatherManager->TypicalExtremePeriods(i).TEType = Line.substr(0, Pos);
                        Line.erase(0, Pos + 1);
                        if (UtilityRoutines::SameString(state.dataWeatherManager->TypicalExtremePeriods(i).TEType, "EXTREME")) {
                            if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "NO DRY SEASON - WEEK NEAR ANNUAL MAX")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoDrySeasonMax";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title,
                                                   "NO DRY SEASON - WEEK NEAR ANNUAL MIN")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoDrySeasonMin";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title,
                                                   "NO WET SEASON - WEEK NEAR ANNUAL MAX")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoWetSeasonMax";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title,
                                                   "NO WET SEASON - WEEK NEAR ANNUAL MIN")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoWetSeasonMin";
                                // to account for problems earlier in weather files:
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "NO DRY")) {
                                if (TropExtremeCount == 0) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).Title = "No Dry Season - Week Near Annual Max";
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoDrySeasonMax";
                                    ++TropExtremeCount;
                                } else if (TropExtremeCount == 1) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).Title = "No Dry Season - Week Near Annual Min";
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoDrySeasonMin";
                                    ++TropExtremeCount;
                                }
                            } else { // make new short titles
                                if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "SUMMER")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Summer";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "WINTER")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Winter";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "TROPICAL HOT")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "TropicalHot";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "TROPICAL COLD")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "TropicalCold";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "AUTUMN")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Autumn";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "NO DRY")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoDrySeason";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "NO WET")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoWetSeason";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "WET ")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "WetSeason";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "DRY ")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "DrySeason";
                                } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "SPRING")) {
                                    state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Spring";
                                }
                            }
                        } else { // not extreme
                            if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "SUMMER")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Summer";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "WINTER")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Winter";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "TROPICAL HOT")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "TropicalHot";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "TROPICAL COLD")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "TropicalCold";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "AUTUMN")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Autumn";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "NO DRY")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoDrySeason";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "NO WET")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "NoWetSeason";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "WET ")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "WetSeason";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "DRY ")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "DrySeason";
                            } else if (has_prefixi(state.dataWeatherManager->TypicalExtremePeriods(i).Title, "SPRING")) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle = "Spring";
                            }
                        }
                    } else {
                        ShowWarningError(state,
                                         "ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)=" +
                                             state.dataWeatherManager->TypicalExtremePeriods(i).Title + " " + Line.substr(0, Pos));
                        ShowContinueError(state, format("...on processing Typical/Extreme period #{}", i));
                        state.dataWeatherManager->NumEPWTypExtSets = i - 1;
                        break;
                    }
                    int PMonth;
                    int PDay;
                    int PWeekDay;
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        General::ProcessDateString(state, Line.substr(0, Pos), PMonth, PDay, PWeekDay, dateType, ErrorsFound);
                        if (dateType != DateType::Invalid) {
                            if (PMonth != 0 && PDay != 0) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).StartMonth = PMonth;
                                state.dataWeatherManager->TypicalExtremePeriods(i).StartDay = PDay;
                            }
                        } else {
                            ShowSevereError(state,
                                            "ProcessEPWHeader: Invalid Typical/Extreme Periods Start Date Field(WeatherFile)=" + Line.substr(0, Pos));
                            ShowContinueError(state, format("...on processing Typical/Extreme period #{}", i));
                            ErrorsFound = true;
                        }
                        Line.erase(0, Pos + 1);
                    }
                    Pos = index(Line, ',');
                    if (Pos != std::string::npos) {
                        General::ProcessDateString(state, Line.substr(0, Pos), PMonth, PDay, PWeekDay, dateType, ErrorsFound);
                        if (dateType != DateType::Invalid) {
                            if (PMonth != 0 && PDay != 0) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).EndMonth = PMonth;
                                state.dataWeatherManager->TypicalExtremePeriods(i).EndDay = PDay;
                            }
                        } else {
                            ShowSevereError(state,
                                            "ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)=" + Line.substr(0, Pos));
                            ShowContinueError(state, format("...on processing Typical/Extreme period #{}", i));
                            ErrorsFound = true;
                        }
                        Line.erase(0, Pos + 1);
                    } else { // Pos=0, probably last one
                        General::ProcessDateString(state, Line, PMonth, PDay, PWeekDay, dateType, ErrorsFound);
                        if (dateType != DateType::Invalid) {
                            if (PMonth != 0 && PDay != 0) {
                                state.dataWeatherManager->TypicalExtremePeriods(i).EndMonth = PMonth;
                                state.dataWeatherManager->TypicalExtremePeriods(i).EndDay = PDay;
                            }
                        } else {
                            ShowSevereError(state,
                                            "ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)=" + Line.substr(0, Pos));
                            ErrorsFound = true;
                        }
                    }
                }
                // Process periods to set up other values.
                for (int i = 1; i <= state.dataWeatherManager->NumEPWTypExtSets; ++i) {
                    // JulianDay (Month,Day,LeapYearValue)
                    auto const ExtremePeriodTitle(UtilityRoutines::MakeUPPERCase(state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle));
                    if (ExtremePeriodTitle == "SUMMER") {
                        if (UtilityRoutines::SameString(state.dataWeatherManager->TypicalExtremePeriods(i).TEType, "EXTREME")) {
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "SummerExtreme";
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue1 = "TropicalHot";
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue2 = "NoDrySeasonMax";
                        } else {
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "SummerTypical";
                        }

                    } else if (ExtremePeriodTitle == "WINTER") {
                        if (UtilityRoutines::SameString(state.dataWeatherManager->TypicalExtremePeriods(i).TEType, "EXTREME")) {
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "WinterExtreme";
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue1 = "TropicalCold";
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue2 = "NoDrySeasonMin";
                        } else {
                            state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "WinterTypical";
                        }

                    } else if (ExtremePeriodTitle == "AUTUMN") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "AutumnTypical";

                    } else if (ExtremePeriodTitle == "SPRING") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "SpringTypical";

                    } else if (ExtremePeriodTitle == "WETSEASON") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "WetSeason";

                    } else if (ExtremePeriodTitle == "DRYSEASON") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "DrySeason";

                    } else if (ExtremePeriodTitle == "NOWETSEASON") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "NoWetSeason";

                    } else if (ExtremePeriodTitle == "NODRYSEASON") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "NoDrySeason";

                    } else if ((ExtremePeriodTitle == "NODRYSEASONMAX") || (ExtremePeriodTitle == "NOWETSEASONMAX")) {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle;
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue1 = "TropicalHot";
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue2 = "SummerExtreme";

                    } else if ((ExtremePeriodTitle == "NODRYSEASONMIN") || (ExtremePeriodTitle == "NOWETSEASONMIN")) {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = state.dataWeatherManager->TypicalExtremePeriods(i).ShortTitle;
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue1 = "TropicalCold";
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue2 = "WinterExtreme";

                    } else if (ExtremePeriodTitle == "TROPICALHOT") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "TropicalHot";
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue1 = "SummerExtreme";
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue2 = "NoDrySeasonMax";

                    } else if (ExtremePeriodTitle == "TROPICALCOLD") {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "TropicalCold";
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue1 = "WinterExtreme";
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue2 = "NoDrySeasonMin";

                    } else {
                        state.dataWeatherManager->TypicalExtremePeriods(i).MatchValue = "Invalid - no match";
                    }
                    state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay =
                        General::OrdinalDay(state.dataWeatherManager->TypicalExtremePeriods(i).StartMonth,
                                            state.dataWeatherManager->TypicalExtremePeriods(i).StartDay,
                                            0);
                    state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay = General::OrdinalDay(
                        state.dataWeatherManager->TypicalExtremePeriods(i).EndMonth, state.dataWeatherManager->TypicalExtremePeriods(i).EndDay, 0);
                    if (state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay <= state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay) {
                        state.dataWeatherManager->TypicalExtremePeriods(i).TotalDays = state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay -
                                                                                       state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay +
                                                                                       1;
                    } else {
                        state.dataWeatherManager->TypicalExtremePeriods(i).TotalDays =
                            General::OrdinalDay(12, 31, state.dataWeatherManager->LeapYearAdd) -
                            state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay + 1 +
                            state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay;
                    }
                }

            } else if (HeaderStringUppercase == "GROUND TEMPERATURES") {
                // Added for ground surfaces defined with F or c factor method. TH 7/2009
                // Assume the 0.5 m set of ground temperatures
                // or first set on a weather file, if any.
                Pos = index(Line, ',');
                if (Pos != std::string::npos) {
                    bool errFlag;
                    int NumGrndTemps = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                    if (!errFlag && NumGrndTemps >= 1) {
                        Line.erase(0, Pos + 1);
                        // skip depth, soil conductivity, soil density, soil specific heat
                        for (int i = 1; i <= 4; ++i) {
                            Pos = index(Line, ',');
                            if (Pos == std::string::npos) {
                                Line.clear();
                                break;
                            }
                            Line.erase(0, Pos + 1);
                        }
                        state.dataWeatherManager->GroundTempsFCFromEPWHeader = 0.0;
                        int actcount = 0;
                        for (int i = 1; i <= 12; ++i) { // take the first set of ground temperatures.
                            Pos = index(Line, ',');
                            if (Pos != std::string::npos) {
                                state.dataWeatherManager->GroundTempsFCFromEPWHeader(i) =
                                    UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                                ++actcount;
                            } else {
                                if (len(Line) > 0) {
                                    state.dataWeatherManager->GroundTempsFCFromEPWHeader(i) =
                                        UtilityRoutines::ProcessNumber(Line.substr(0, Pos), errFlag);
                                    ++actcount;
                                }
                                break;
                            }
                            Line.erase(0, Pos + 1);
                        }
                        if (actcount == 12) state.dataWeatherManager->wthFCGroundTemps = true;
                    }
                }

            } else if (HeaderStringUppercase == "HOLIDAYS/DAYLIGHT SAVING") {
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
                for (int i = 1; i <= NumHdArgs; ++i) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos == std::string::npos) {
                        if (len(Line) == 0) {
                            while (Pos == std::string::npos) {
                                Line = state.files.inputWeatherFile.readLine().data;
                                strip(Line);
                                uppercase(Line);
                                Pos = index(Line, ',');
                            }
                        } else {
                            Pos = len(Line);
                        }
                    }

                    int PMonth;
                    int PDay;
                    int PWeekDay;
                    bool IOStatus;
                    int CurCount = 0;
                    if (i == 1) {
                        state.dataWeatherManager->WFAllowsLeapYears = (Line[0] == 'Y');
                    } else if (i == 2) {
                        // In this section, we call ProcessDateString, and if that fails, we can recover from it
                        // by setting DST to false, so we don't affect ErrorsFound

                        // call ProcessDateString with local bool (unused)
                        bool errflag1;
                        General::ProcessDateString(state, Line.substr(0, Pos), PMonth, PDay, PWeekDay, dateType, errflag1);
                        if (dateType != DateType::Invalid) {
                            // ErrorsFound is still false after ProcessDateString
                            if (PMonth == 0 && PDay == 0) {
                                state.dataWeatherManager->EPWDaylightSaving = false;
                            } else {
                                state.dataWeatherManager->EPWDaylightSaving = true;
                                state.dataWeatherManager->EPWDST.StDateType = dateType;
                                state.dataWeatherManager->EPWDST.StMon = PMonth;
                                state.dataWeatherManager->EPWDST.StDay = PDay;
                                state.dataWeatherManager->EPWDST.StWeekDay = PWeekDay;
                            }
                        } else {
                            // ErrorsFound is untouched
                            ShowContinueError(
                                state, "ProcessEPWHeader: Invalid Daylight Saving Period Start Date Field(WeatherFile)=" + Line.substr(0, Pos));
                            ShowContinueError(state, "...invalid header=" + HeaderString);
                            ShowContinueError(state, "...Setting Weather File DST to false.");
                            state.dataWeatherManager->EPWDaylightSaving = false;
                        }

                    } else if (i == 3) {
                        General::ProcessDateString(state, Line.substr(0, Pos), PMonth, PDay, PWeekDay, dateType, ErrorsFound);
                        if (state.dataWeatherManager->EPWDaylightSaving) {
                            if (dateType != DateType::Invalid) {
                                state.dataWeatherManager->EPWDST.EnDateType = dateType;
                                state.dataWeatherManager->EPWDST.EnMon = PMonth;
                                state.dataWeatherManager->EPWDST.EnDay = PDay;
                                state.dataWeatherManager->EPWDST.EnWeekDay = PWeekDay;
                            } else {
                                ShowWarningError(
                                    state, "ProcessEPWHeader: Invalid Daylight Saving Period End Date Field(WeatherFile)=" + Line.substr(0, Pos));
                                ShowContinueError(state, "...Setting Weather File DST to false.");
                                state.dataWeatherManager->EPWDaylightSaving = false;
                            }
                            state.dataWeatherManager->DST = state.dataWeatherManager->EPWDST;
                        }

                    } else if (i == 4) {
                        int NumEPWHolidays = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                        state.dataWeatherManager->NumSpecialDays =
                            NumEPWHolidays + state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "RunPeriodControl:SpecialDays");
                        state.dataWeatherManager->SpecialDays.allocate(state.dataWeatherManager->NumSpecialDays);
                        NumHdArgs = 4 + NumEPWHolidays * 2;

                    } else if ((i >= 5)) {
                        if (mod(i, 2) != 0) {
                            ++CurCount;
                            if (CurCount > state.dataWeatherManager->NumSpecialDays) {
                                ShowSevereError(state, "Too many SpecialDays");
                                ErrorsFound = true;
                            } else {
                                state.dataWeatherManager->SpecialDays(CurCount).Name = Line.substr(0, Pos);
                            }
                            // Process name
                        } else {
                            if (CurCount <= state.dataWeatherManager->NumSpecialDays) {
                                // Process date
                                General::ProcessDateString(state, Line.substr(0, Pos), PMonth, PDay, PWeekDay, dateType, ErrorsFound);
                                if (dateType == DateType::MonthDay) {
                                    state.dataWeatherManager->SpecialDays(CurCount).DateType = dateType;
                                    state.dataWeatherManager->SpecialDays(CurCount).Month = PMonth;
                                    state.dataWeatherManager->SpecialDays(CurCount).Day = PDay;
                                    state.dataWeatherManager->SpecialDays(CurCount).WeekDay = 0;
                                    state.dataWeatherManager->SpecialDays(CurCount).CompDate = PMonth * 32 + PDay;
                                    state.dataWeatherManager->SpecialDays(CurCount).Duration = 1;
                                    state.dataWeatherManager->SpecialDays(CurCount).DayType = 1;
                                    state.dataWeatherManager->SpecialDays(CurCount).WthrFile = true;
                                } else if (dateType != DateType::Invalid) {
                                    state.dataWeatherManager->SpecialDays(CurCount).DateType = dateType;
                                    state.dataWeatherManager->SpecialDays(CurCount).Month = PMonth;
                                    state.dataWeatherManager->SpecialDays(CurCount).Day = PDay;
                                    state.dataWeatherManager->SpecialDays(CurCount).WeekDay = PWeekDay;
                                    state.dataWeatherManager->SpecialDays(CurCount).CompDate = 0;
                                    state.dataWeatherManager->SpecialDays(CurCount).Duration = 1;
                                    state.dataWeatherManager->SpecialDays(CurCount).DayType = 1;
                                    state.dataWeatherManager->SpecialDays(CurCount).WthrFile = true;
                                } else if (dateType == DateType::Invalid) {
                                    ShowSevereError(state, "Invalid SpecialDay Date Field(WeatherFile)=" + Line.substr(0, Pos));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                    Line.erase(0, Pos + 1);
                }
                for (int i = 1; i <= state.dataWeatherManager->NumEPWTypExtSets; ++i) {
                    // General::OrdinalDay (Month,Day,LeapYearValue)
                    state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay =
                        General::OrdinalDay(state.dataWeatherManager->TypicalExtremePeriods(i).StartMonth,
                                            state.dataWeatherManager->TypicalExtremePeriods(i).StartDay,
                                            state.dataWeatherManager->LeapYearAdd);
                    state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay =
                        General::OrdinalDay(state.dataWeatherManager->TypicalExtremePeriods(i).EndMonth,
                                            state.dataWeatherManager->TypicalExtremePeriods(i).EndDay,
                                            state.dataWeatherManager->LeapYearAdd);
                    if (state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay <= state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay) {
                        state.dataWeatherManager->TypicalExtremePeriods(i).TotalDays = state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay -
                                                                                       state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay +
                                                                                       1;
                    } else {
                        state.dataWeatherManager->TypicalExtremePeriods(i).TotalDays =
                            General::OrdinalDay(12, 31, state.dataWeatherManager->LeapYearAdd) -
                            state.dataWeatherManager->TypicalExtremePeriods(i).StartJDay + 1 +
                            state.dataWeatherManager->TypicalExtremePeriods(i).EndJDay;
                    }
                }

            } else if ((HeaderStringUppercase == "COMMENTS 1") || (HeaderStringUppercase == "COMMENTS 2") ||
                       (HeaderStringUppercase == "DESIGN CONDITIONS")) {
                // no action
            } else if (HeaderStringUppercase == "DATA PERIODS") {
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
                int CurCount = 0;
                for (int i = 1; i <= NumHdArgs; ++i) {
                    strip(Line);
                    Pos = index(Line, ',');
                    if (Pos == std::string::npos) {
                        if (len(Line) == 0) {
                            while (Pos == std::string::npos) {
                                Line = state.files.inputWeatherFile.readLine().data;
                                strip(Line);
                                uppercase(Line);
                                Pos = index(Line, ',');
                            }
                        } else {
                            Pos = len(Line);
                        }
                    }

                    bool IOStatus;
                    if (i == 1) {
                        state.dataWeatherManager->NumDataPeriods = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                        state.dataWeatherManager->DataPeriods.allocate(state.dataWeatherManager->NumDataPeriods);
                        NumHdArgs += 4 * state.dataWeatherManager->NumDataPeriods;
                        if (state.dataWeatherManager->NumDataPeriods > 0) {
                            for (auto &e : state.dataWeatherManager->DataPeriods)
                                e.NumDays = 0;
                        }

                    } else if (i == 2) {
                        state.dataWeatherManager->NumIntervalsPerHour = UtilityRoutines::ProcessNumber(Line.substr(0, Pos), IOStatus);
                    } else if (i >= 3) {
                        int const CurOne = mod(i - 3, 4);
                        int PMonth;
                        int PDay;
                        int PWeekDay;
                        int PYear;
                        if (CurOne == 0) {
                            // Description of Data Period
                            ++CurCount;
                            if (CurCount > state.dataWeatherManager->NumDataPeriods) {
                                ShowSevereError(state, "Too many data periods");
                                ErrorsFound = true;
                            } else {
                                state.dataWeatherManager->DataPeriods(CurCount).Name = Line.substr(0, Pos);
                            }

                        } else if (CurOne == 1) {
                            // Start Day of Week
                            if (CurCount <= state.dataWeatherManager->NumDataPeriods) {
                                state.dataWeatherManager->DataPeriods(CurCount).DayOfWeek = Line.substr(0, Pos);
                                state.dataWeatherManager->DataPeriods(CurCount).WeekDay =
                                    UtilityRoutines::FindItemInList(state.dataWeatherManager->DataPeriods(CurCount).DayOfWeek, DaysOfWeek, 7);
                                if (state.dataWeatherManager->DataPeriods(CurCount).WeekDay == 0) {
                                    ShowSevereError(state,
                                                    fmt::format("Weather File -- Invalid Start Day of Week for Data Period #{}, Invalid day={}",
                                                                CurCount,
                                                                state.dataWeatherManager->DataPeriods(CurCount).DayOfWeek));
                                    ErrorsFound = true;
                                }
                            }

                        } else if (CurOne == 2) {
                            // DataPeriod Start Day
                            if (CurCount <= state.dataWeatherManager->NumDataPeriods) {
                                General::ProcessDateString(state, Line.substr(0, Pos), PMonth, PDay, PWeekDay, dateType, ErrorsFound, PYear);
                                if (dateType == DateType::MonthDay) {
                                    state.dataWeatherManager->DataPeriods(CurCount).StMon = PMonth;
                                    state.dataWeatherManager->DataPeriods(CurCount).StDay = PDay;
                                    state.dataWeatherManager->DataPeriods(CurCount).StYear = PYear;
                                    if (PYear != 0) state.dataWeatherManager->DataPeriods(CurCount).HasYearData = true;
                                } else {
                                    ShowSevereError(state,
                                                    "Data Periods must be of the form <DayOfYear> or <Month Day> (WeatherFile), found=" +
                                                        Line.substr(0, Pos));
                                    ErrorsFound = true;
                                }
                            }

                        } else if (CurOne == 3) {
                            if (CurCount <= state.dataWeatherManager->NumDataPeriods) {
                                General::ProcessDateString(state, Line.substr(0, Pos), PMonth, PDay, PWeekDay, dateType, ErrorsFound, PYear);
                                if (dateType == DateType::MonthDay) {
                                    state.dataWeatherManager->DataPeriods(CurCount).EnMon = PMonth;
                                    state.dataWeatherManager->DataPeriods(CurCount).EnDay = PDay;
                                    state.dataWeatherManager->DataPeriods(CurCount).EnYear = PYear;
                                    if (PYear == 0 && state.dataWeatherManager->DataPeriods(CurCount).HasYearData) {
                                        ShowWarningError(state, "Data Period (WeatherFile) - Start Date contains year. End Date does not.");
                                        ShowContinueError(state, "...Assuming same year as Start Date for this data.");
                                        state.dataWeatherManager->DataPeriods(CurCount).EnYear =
                                            state.dataWeatherManager->DataPeriods(CurCount).StYear;
                                    }
                                } else {
                                    ShowSevereError(state,
                                                    "Data Periods must be of the form <DayOfYear> or <Month Day>, (WeatherFile) found=" +
                                                        Line.substr(0, Pos));
                                    ErrorsFound = true;
                                }
                            }
                            if (state.dataWeatherManager->DataPeriods(CurCount).StYear == 0 ||
                                state.dataWeatherManager->DataPeriods(CurCount).EnYear == 0) {
                                state.dataWeatherManager->DataPeriods(CurCount).DataStJDay =
                                    General::OrdinalDay(state.dataWeatherManager->DataPeriods(CurCount).StMon,
                                                        state.dataWeatherManager->DataPeriods(CurCount).StDay,
                                                        state.dataWeatherManager->LeapYearAdd);
                                state.dataWeatherManager->DataPeriods(CurCount).DataEnJDay =
                                    General::OrdinalDay(state.dataWeatherManager->DataPeriods(CurCount).EnMon,
                                                        state.dataWeatherManager->DataPeriods(CurCount).EnDay,
                                                        state.dataWeatherManager->LeapYearAdd);
                                if (state.dataWeatherManager->DataPeriods(CurCount).DataStJDay <=
                                    state.dataWeatherManager->DataPeriods(CurCount).DataEnJDay) {
                                    state.dataWeatherManager->DataPeriods(CurCount).NumDays =
                                        state.dataWeatherManager->DataPeriods(CurCount).DataEnJDay -
                                        state.dataWeatherManager->DataPeriods(CurCount).DataStJDay + 1;
                                } else {
                                    state.dataWeatherManager->DataPeriods(CurCount).NumDays =
                                        (365 - state.dataWeatherManager->DataPeriods(CurCount).DataStJDay + 1) +
                                        (state.dataWeatherManager->DataPeriods(CurCount).DataEnJDay - 1 + 1);
                                }
                            } else { // weather file has actual year(s)
                                auto &dataPeriod{state.dataWeatherManager->DataPeriods(CurCount)};
                                dataPeriod.DataStJDay = computeJulianDate(dataPeriod.StYear, dataPeriod.StMon, dataPeriod.StDay);
                                dataPeriod.DataEnJDay = computeJulianDate(dataPeriod.EnYear, dataPeriod.EnMon, dataPeriod.EnDay);
                                dataPeriod.NumDays = dataPeriod.DataEnJDay - dataPeriod.DataStJDay + 1;
                            }
                            // Have processed the last item for this, can set up Weekdays for months
                            state.dataWeatherManager->DataPeriods(CurCount).MonWeekDay = 0;
                            if (!ErrorsFound) {
                                SetupWeekDaysByMonth(state,
                                                     state.dataWeatherManager->DataPeriods(CurCount).StMon,
                                                     state.dataWeatherManager->DataPeriods(CurCount).StDay,
                                                     state.dataWeatherManager->DataPeriods(CurCount).WeekDay,
                                                     state.dataWeatherManager->DataPeriods(CurCount).MonWeekDay);
                            }
                        }
                    }
                    Line.erase(0, Pos + 1);
                }

            } else {
                ShowFatalError(state, "Invalid EPW Header designation found=" + HeaderString);
            }
        }
    }

    void SkipEPlusWFHeader(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   August 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine skips the initial header records on the EnergyPlus Weather File (in.epw).

        static constexpr std::string_view Header("DATA PERIODS");

        // Read in Header Information
        InputFile::ReadResult<std::string> Line{"", true, false};

        // Headers should come in order
        while (true) {
            Line = state.files.inputWeatherFile.readLine();
            if (Line.eof) {
                ShowFatalError(state,
                               "Unexpected End-of-File on EPW Weather file, while reading header information, looking for header=" +
                                   std::string{Header},
                               OptionalOutputFileRef{state.files.eso});
            }
            uppercase(Line.data);
            if (has(Line.data, Header)) break;
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

        int NumHdArgs = 2;
        int CurCount = 0;
        for (int i = 1; i <= NumHdArgs; ++i) {
            strip(Line.data);
            std::string::size_type Pos = index(Line.data, ',');
            if (Pos == std::string::npos) {
                if (len(Line.data) == 0) {
                    while (Pos == std::string::npos) {
                        Line = state.files.inputWeatherFile.readLine();
                        strip(Line.data);
                        uppercase(Line.data);
                        Pos = index(Line.data, ',');
                    }
                } else {
                    Pos = len(Line.data);
                }
            }

            if (i == 1) {
                bool IOStatus;
                int const NumPeriods = UtilityRoutines::ProcessNumber(Line.data.substr(0, Pos), IOStatus);
                NumHdArgs += 4 * NumPeriods;
            } else if ((i >= 3)) {
                if (mod(i - 3, 4) == 0) ++CurCount;
            }
            Line.data.erase(0, Pos + 1);
        }
    }

    void ReportMissing_RangeData(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports the counts of missing/out of range data
        // for weather file environments.

        static constexpr std::string_view MissString("Missing Data Found on Weather Data File");
        static constexpr std::string_view msFmt("Missing {}, Number of items={:5}");
        static constexpr std::string_view InvString("Invalid Data Found on Weather Data File");
        static constexpr std::string_view ivFmt("Invalid {}, Number of items={:5}");
        static constexpr std::string_view RangeString("Out of Range Data Found on Weather Data File");
        static constexpr std::string_view rgFmt("Out of Range {} [{},{}], Number of items={:5}");

        if (!state.dataEnvrn->DisplayWeatherMissingDataWarnings) return;

        bool MissedHeader = false;
        auto missedHeaderCheck{[&](Real64 const value, std::string const &description) {
            if (value > 0) {
                if (!MissedHeader) {
                    ShowWarningError(state, std::string{MissString});
                    MissedHeader = true;
                }
                ShowMessage(state, format(msFmt, "\"" + description + "\"", value));
            }
        }};

        missedHeaderCheck(state.dataWeatherManager->Missed.DryBulb, "Dry Bulb Temperature");
        missedHeaderCheck(state.dataWeatherManager->Missed.StnPres, "Atmospheric Pressure");
        missedHeaderCheck(state.dataWeatherManager->Missed.RelHumid, "Relative Humidity");
        missedHeaderCheck(state.dataWeatherManager->Missed.DewPoint, "Dew Point Temperatures");
        missedHeaderCheck(state.dataWeatherManager->Missed.WindSpd, "Wind Speed");
        missedHeaderCheck(state.dataWeatherManager->Missed.WindDir, "Wind Direction");
        missedHeaderCheck(state.dataWeatherManager->Missed.DirectRad, "Direct Radiation");
        missedHeaderCheck(state.dataWeatherManager->Missed.DiffuseRad, "Diffuse Radiation");
        missedHeaderCheck(state.dataWeatherManager->Missed.TotSkyCvr, "Total Sky Cover");
        missedHeaderCheck(state.dataWeatherManager->Missed.OpaqSkyCvr, "Opaque Sky Cover");
        missedHeaderCheck(state.dataWeatherManager->Missed.SnowDepth, "Snow Depth");
        if (state.dataWeatherManager->Missed.WeathCodes > 0) {
            ShowWarningError(state, std::string{InvString});
            ShowMessage(state, format(ivFmt, "\"Weather Codes\" (not equal 9 digits)", state.dataWeatherManager->Missed.WeathCodes));
        }
        missedHeaderCheck(state.dataWeatherManager->Missed.LiquidPrecip, "Liquid Precipitation Depth");

        bool OutOfRangeHeader = false;
        auto outOfRangeHeaderCheck{
            [&](Real64 const value, std::string_view description, std::string_view rangeLow, std::string_view rangeHigh, std::string_view extraMsg) {
                if (value > 0) {
                    if (!OutOfRangeHeader) {
                        ShowWarningError(state, std::string{RangeString});
                        OutOfRangeHeader = true;
                    }
                    ShowMessage(state, EnergyPlus::format(rgFmt, description, rangeLow, rangeHigh, value));
                    if (!extraMsg.empty()) ShowMessage(state, std::string{extraMsg});
                }
            }};
        outOfRangeHeaderCheck(state.dataWeatherManager->OutOfRange.DryBulb, "Dry Bulb Temperatures", ">=-90", "<=70", "");
        outOfRangeHeaderCheck(
            state.dataWeatherManager->OutOfRange.StnPres, "Atmospheric Pressure", ">31000", "<=120000", "Out of Range values set to last good value");
        outOfRangeHeaderCheck(state.dataWeatherManager->OutOfRange.RelHumid, "Relative Humidity", ">=0", "<=110", "");
        outOfRangeHeaderCheck(state.dataWeatherManager->OutOfRange.DewPoint, "Dew Point Temperatures", ">=-90", "<=70", "");
        outOfRangeHeaderCheck(state.dataWeatherManager->OutOfRange.WindSpd, "Wind Speed", ">=0", "<=40", "");
        outOfRangeHeaderCheck(state.dataWeatherManager->OutOfRange.WindDir, "Wind Direction", ">=0", "<=360", "");
        outOfRangeHeaderCheck(state.dataWeatherManager->OutOfRange.DirectRad, "Direct Radiation", ">=0", "NoLimit", "");
        outOfRangeHeaderCheck(state.dataWeatherManager->OutOfRange.DiffuseRad, "Diffuse Radiation", ">=0", "NoLimit", "");
    }

    void SetupInterpolationValues(EnergyPlusData &state)
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

        int halfpoint = 0;

        state.dataWeatherManager->Interpolation.allocate(state.dataGlobal->NumOfTimeStepInHour);
        state.dataWeatherManager->SolarInterpolation.allocate(state.dataGlobal->NumOfTimeStepInHour);
        state.dataWeatherManager->Interpolation = 0.0;
        state.dataWeatherManager->SolarInterpolation = 0.0;

        for (int tloop = 1; tloop <= state.dataGlobal->NumOfTimeStepInHour; ++tloop) {
            state.dataWeatherManager->Interpolation(tloop) =
                (state.dataGlobal->NumOfTimeStepInHour == 1) ? 1.0 : min(1.0, (double(tloop) / double(state.dataGlobal->NumOfTimeStepInHour)));
        }

        if (mod(state.dataGlobal->NumOfTimeStepInHour, 2) == 0) {
            // even number of time steps.
            halfpoint = state.dataGlobal->NumOfTimeStepInHour / 2;
            state.dataWeatherManager->SolarInterpolation(halfpoint) = 1.0;
            Real64 tweight = 1.0 / double(state.dataGlobal->NumOfTimeStepInHour);
            for (int tloop = halfpoint + 1, hpoint = 1; tloop <= state.dataGlobal->NumOfTimeStepInHour; ++tloop, ++hpoint) {
                state.dataWeatherManager->SolarInterpolation(tloop) = 1.0 - hpoint * tweight;
            }
            for (int tloop = halfpoint - 1, hpoint = 1; tloop >= 1; --tloop, ++hpoint) {
                state.dataWeatherManager->SolarInterpolation(tloop) = 1.0 - hpoint * tweight;
            }
        } else { // odd number of time steps
            if (state.dataGlobal->NumOfTimeStepInHour == 1) {
                state.dataWeatherManager->SolarInterpolation(1) = 0.5;
            } else if (state.dataGlobal->NumOfTimeStepInHour == 3) {
                state.dataWeatherManager->SolarInterpolation(1) = 5.0 / 6.0;
                state.dataWeatherManager->SolarInterpolation(2) = 5.0 / 6.0;
                state.dataWeatherManager->SolarInterpolation(3) = 0.5;
            } else {
                Real64 tweight = 1.0 / double(state.dataGlobal->NumOfTimeStepInHour);
                halfpoint = state.dataGlobal->NumOfTimeStepInHour / 2;
                Real64 tweight1 = 1.0 - tweight / 2.0;
                state.dataWeatherManager->SolarInterpolation(halfpoint) = tweight1;
                state.dataWeatherManager->SolarInterpolation(halfpoint + 1) = tweight1;
                for (int tloop = halfpoint + 2, hpoint = 1; tloop <= state.dataGlobal->NumOfTimeStepInHour; ++tloop, ++hpoint) {
                    state.dataWeatherManager->SolarInterpolation(tloop) = tweight1 - hpoint * tweight;
                }
                for (int tloop = halfpoint - 1, hpoint = 1; tloop >= 1; --tloop, ++hpoint) {
                    state.dataWeatherManager->SolarInterpolation(tloop) = tweight1 - hpoint * tweight;
                }
            }
        }
    }

    void SetupEnvironmentTypes(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Make sure Environment derived type is set prior to getting
        // Weather Properties

        // Transfer weather file information to the Environment derived type
        state.dataWeatherManager->Envrn = state.dataEnvrn->TotDesDays + 1;

        // Sizing Periods from Weather File
        for (int i = 1; i <= state.dataWeatherManager->TotRunDesPers; ++i) {
            auto &env = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn);
            auto &runPer = state.dataWeatherManager->RunPeriodDesignInput(i);

            env.StartMonth = runPer.startMonth;
            env.StartDay = runPer.startDay;
            env.StartJDay = General::OrdinalDay(runPer.startMonth, runPer.startDay, state.dataWeatherManager->LeapYearAdd);
            env.TotalDays = runPer.totalDays;
            env.EndMonth = runPer.endMonth;
            env.EndDay = runPer.endDay;
            env.EndJDay = General::OrdinalDay(runPer.endMonth, runPer.endDay, state.dataWeatherManager->LeapYearAdd);
            env.NumSimYears = runPer.numSimYears;
            if (env.StartJDay <= env.EndJDay) {
                env.TotalDays = (env.EndJDay - env.StartJDay + 1) * env.NumSimYears;
            } else {
                env.TotalDays =
                    (General::OrdinalDay(12, 31, state.dataWeatherManager->LeapYearAdd) - env.StartJDay + 1 + env.EndJDay) * env.NumSimYears;
            }
            state.dataEnvrn->TotRunDesPersDays += env.TotalDays;
            env.UseDST = runPer.useDST;
            env.UseHolidays = runPer.useHolidays;
            env.Title = runPer.title;
            env.cKindOfEnvrn = runPer.periodType;
            env.KindOfEnvrn = DataGlobalConstants::KindOfSim::RunPeriodDesign;
            env.DesignDayNum = 0;
            env.RunPeriodDesignNum = i;
            env.DayOfWeek = runPer.dayOfWeek;
            env.MonWeekDay = runPer.monWeekDay;
            env.SetWeekDays = false;
            env.ApplyWeekendRule = runPer.applyWeekendRule;
            env.UseRain = runPer.useRain;
            env.UseSnow = runPer.useSnow;
            env.firstHrInterpUseHr1 = runPer.firstHrInterpUsingHr1; // this will just the default
            ++state.dataWeatherManager->Envrn;
        }

        // RunPeriods from weather file
        for (int i = 1; i <= state.dataWeatherManager->TotRunPers; ++i) { // Run Periods.
            auto &env = state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn);
            auto &runPer = state.dataWeatherManager->RunPeriodInput(i);

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
                    int LocalLeapYearAdd = 0;
                    if (isLeapYear(env.StartYear)) {
                        // If a leap year is supported by the weather file, do it.
                        if (state.dataWeatherManager->WFAllowsLeapYears) {
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
                    env.EndJDay = General::OrdinalDay(
                        runPer.endMonth, runPer.endDay, isLeapYear(runPer.endYear) && state.dataWeatherManager->WFAllowsLeapYears ? 1 : 0);
                    env.TotalDays = 366 - env.StartJDay + env.EndJDay + 365 * std::max(env.NumSimYears - 2, 0);
                    if (state.dataWeatherManager->WFAllowsLeapYears) {
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
            if (runPer.title.empty()) {
                env.Title = state.dataEnvrn->WeatherFileLocationTitle;
            } else {
                env.Title = runPer.title;
            }
            if (env.KindOfEnvrn == DataGlobalConstants::KindOfSim::ReadAllWeatherData) {
                env.cKindOfEnvrn = "ReadAllWeatherDataRunPeriod";
            } else {
                env.cKindOfEnvrn = "WeatherFileRunPeriod";
                env.KindOfEnvrn = DataGlobalConstants::KindOfSim::RunPeriodWeather;
            }
            env.DayOfWeek = runPer.dayOfWeek;
            env.MonWeekDay = runPer.monWeekDay;
            env.SetWeekDays = false;
            env.ApplyWeekendRule = runPer.applyWeekendRule;
            env.UseRain = runPer.useRain;
            env.UseSnow = runPer.useSnow;
            env.firstHrInterpUseHr1 = runPer.firstHrInterpUsingHr1; // first hour interpolation choice
            ++state.dataWeatherManager->Envrn;
        }
    }

    bool isLeapYear(int const Year)
    {
        // true if it's a leap year, false if not.

        if (mod(Year, 4) == 0) { // Potential Leap Year
            if (!(mod(Year, 100) == 0 && mod(Year, 400) != 0)) {
                return true;
            }
        }
        return false;
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

        // REFERENCES:
        // for discussion of this algorithm,
        // see cacm, vol 11, no 10, oct 1968, page 657

        int tyyyy = gyyyy;
        int tmm = gmm;
        int tdd = gdd;
        int l = (tmm - 14) / 12;
        return tdd - 32075 + 1461 * (tyyyy + 4800 + l) / 4 + 367 * (tmm - 2 - l * 12) / 12 - 3 * ((tyyyy + 4900 + l) / 100) / 4;
    }

    int computeJulianDate(GregorianDate const gdate)
    {
        return computeJulianDate(gdate.year, gdate.month, gdate.day);
    }

    GregorianDate computeGregorianDate(int const jdate)
    {
        int tdate = jdate;
        int l = tdate + 68569;
        int n = 4 * l / 146097;
        l -= (146097 * n + 3) / 4;
        int tyyyy = 4000 * (l + 1) / 1461001;
        l = l - 1461 * tyyyy / 4 + 31;
        int tmm = 80 * l / 2447;
        int tdd = l - 2447 * tmm / 80;
        l = tmm / 11;
        tmm += 2 - 12 * l;
        tyyyy += 100 * (n - 49) + l;
        return {tyyyy, tmm, tdd};
    }

    WeekDay calculateDayOfWeek(EnergyPlusData &state, int const year, int const month, int const day)
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

        int Gyyyy(year); // Gregorian yyyy
        int Gmm(month);  // Gregorian mm

        // Jan, Feb are 13, 14 months of previous year
        if (Gmm < 3) {
            Gmm += 12;
            --Gyyyy;
        }

        state.dataEnvrn->DayOfWeek = mod(day + (13 * (Gmm + 1) / 5) + Gyyyy + (Gyyyy / 4) + 6 * (Gyyyy / 100) + (Gyyyy / 400), 7);
        if (state.dataEnvrn->DayOfWeek == 0) state.dataEnvrn->DayOfWeek = 7;

        return static_cast<WeekDay>(state.dataEnvrn->DayOfWeek);
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

        static std::array<int, 12> const daysbefore{{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334}};
        static std::array<int, 12> const daysbeforeleap{{0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335}};

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

    void AnnualMonthlyDryBulbWeatherData::CalcAnnualAndMonthlyDryBulbTemp(EnergyPlusData &state)
    {

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates monthly daily average outdoor air drybulb temperature from
        // either weather (*.EPW) file or reads monthly daily average outdoor air
        // drybulb temperature from STAT (*.stat) for use to autosize main water
        // temperature.

        Real64 MonthlyDailyDryBulbMin(200.0);               // monthly-daily minimum outside air dry-bulb temperature
        Real64 MonthlyDailyDryBulbMax(-200.0);              // monthly-daily maximum outside air dry-bulb temperature
        Real64 AnnualDailyAverageDryBulbTempSum(0.0);       // annual sum of daily average outside air dry-bulb temperature
        Array1D<Real64> MonthlyAverageDryBulbTemp(12, 0.0); // monthly-daily average outside air temperature

        if (!this->OADryBulbWeatherDataProcessed) {
            const auto statFileExists = FileSystem::fileExists(state.files.inStatFilePath.filePath);
            const auto epwFileExists = FileSystem::fileExists(state.files.inputWeatherFilePath.filePath);
            if (statFileExists) {
                auto statFile = state.files.inStatFilePath.try_open();
                if (!statFile.good()) {
                    ShowSevereError(state,
                                    "CalcAnnualAndMonthlyDryBulbTemp: Could not open file " + statFile.filePath.string() + " for input (read).");
                    ShowContinueError(state, "Water Mains Temperature will be set to a fixed default value of 10.0 C.");
                    return;
                }

                std::string lineAvg;
                while (statFile.good()) {
                    auto lineIn = statFile.readLine();
                    if (has(lineIn.data, "Monthly Statistics for Dry Bulb temperatures")) {
                        for (int i = 1; i <= 7; ++i) {
                            lineIn = statFile.readLine();
                        }
                        lineIn = statFile.readLine();
                        lineAvg = lineIn.data;
                        break;
                    }
                }
                if (lineAvg.empty()) {
                    ShowSevereError(state,
                                    "CalcAnnualAndMonthlyDryBulbTemp: Stat file '" + statFile.filePath.string() +
                                        "' does not have Monthly Statistics for Dry Bulb temperatures.");
                    ShowContinueError(state, "Water Mains Temperature will be set to a fixed default value of 10.0 C.");
                    return;
                } else if (lineAvg.find("Daily Avg") == std::string::npos) {
                    ShowSevereError(state,
                                    "CalcAnnualAndMonthlyDryBulbTemp: Stat file '" + statFile.filePath.string() +
                                        "' does not have the 'Daily Avg' line in the Monthly Statistics for Dry Bulb temperatures.");
                    ShowContinueError(state, "Water Mains Temperature will be set to a fixed default value of 10.0 C.");
                    return;
                } else {
                    int AnnualNumberOfDays = 0;
                    for (int i = 1; i <= 12; ++i) {
                        MonthlyAverageDryBulbTemp(i) = OutputReportTabular::StrToReal(OutputReportTabular::GetColumnUsingTabs(lineAvg, i + 2));
                        AnnualDailyAverageDryBulbTempSum += MonthlyAverageDryBulbTemp(i) * state.dataWeatherManager->EndDayOfMonth(i);
                        MonthlyDailyDryBulbMin = min(MonthlyDailyDryBulbMin, MonthlyAverageDryBulbTemp(i));
                        MonthlyDailyDryBulbMax = max(MonthlyDailyDryBulbMax, MonthlyAverageDryBulbTemp(i));
                        AnnualNumberOfDays += state.dataWeatherManager->EndDayOfMonth(i);
                    }
                    this->AnnualAvgOADryBulbTemp = AnnualDailyAverageDryBulbTempSum / AnnualNumberOfDays;
                    this->MonthlyAvgOADryBulbTempMaxDiff = MonthlyDailyDryBulbMax - MonthlyDailyDryBulbMin;
                    this->MonthlyDailyAverageDryBulbTemp = MonthlyAverageDryBulbTemp;
                    this->OADryBulbWeatherDataProcessed = true;
                }
            } else if (epwFileExists) {
                auto epwFile = state.files.inputWeatherFilePath.try_open();
                bool epwHasLeapYear(false);
                if (!epwFile.good()) {
                    ShowSevereError(state,
                                    "CalcAnnualAndMonthlyDryBulbTemp: Could not open file " + epwFile.filePath.string() + " for input (read).");
                    ShowContinueError(state, "Water Mains Temperature will be set to a fixed default value of 10.0 C.");
                    return;
                }
                for (int i = 1; i <= 8; ++i) { // Headers
                    auto epwLine = epwFile.readLine();

                    if (i == 5) {
                        // HOLIDAYS/DAYLIGHT SAVINGS,Yes,0,0,0
                        std::string::size_type pos = index(epwLine.data, ',');
                        epwLine.data.erase(0, pos + 1);
                        pos = index(epwLine.data, ',');
                        std::string LeapYear = UtilityRoutines::MakeUPPERCase(epwLine.data.substr(0, pos));
                        if (LeapYear[0] == 'Y') {
                            epwHasLeapYear = true;
                        }
                    }
                }
                Array1D<int> EndDayOfMonthLocal;
                EndDayOfMonthLocal = state.dataWeatherManager->EndDayOfMonth;
                if (epwHasLeapYear) {
                    // increase number of days for february by one day if weather data has leap year
                    EndDayOfMonthLocal(2) = EndDayOfMonthLocal(2) + 1;
                }
                int DayNum;
                int DaysCountOfMonth;
                for (int i = 1; i <= 12; ++i) {
                    Real64 MonthlyDailyDryBulbAvg = 0.0;
                    DaysCountOfMonth = EndDayOfMonthLocal(i);
                    for (DayNum = 1; DayNum <= DaysCountOfMonth; ++DayNum) {
                        Real64 DailyAverageDryBulbTemp = 0.0;
                        std::string::size_type pos;
                        for (int j = 1; j <= 24; ++j) {
                            auto epwLine = epwFile.readLine();
                            for (int ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine.data, ',');
                                epwLine.data.erase(0, pos + 1);
                            }
                            pos = index(epwLine.data, ',');
                            Real64 HourlyDryBulbTemp = OutputReportTabular::StrToReal(epwLine.data.substr(0, pos));
                            DailyAverageDryBulbTemp += (HourlyDryBulbTemp / 24.0);
                        }
                        AnnualDailyAverageDryBulbTempSum += DailyAverageDryBulbTemp;
                        MonthlyDailyDryBulbAvg += (DailyAverageDryBulbTemp / DaysCountOfMonth);
                    }
                    MonthlyAverageDryBulbTemp(i) = MonthlyDailyDryBulbAvg;
                    MonthlyDailyDryBulbMin = min(MonthlyDailyDryBulbMin, MonthlyDailyDryBulbAvg);
                    MonthlyDailyDryBulbMax = max(MonthlyDailyDryBulbMax, MonthlyDailyDryBulbAvg);
                }
                // calculate annual average outdoor air dry-bulb temperature and monthly daily average
                // outdoor air temperature maximum difference
                int AnnualNumberOfDays = 365;
                if (epwHasLeapYear) AnnualNumberOfDays++;
                this->AnnualAvgOADryBulbTemp = AnnualDailyAverageDryBulbTempSum / AnnualNumberOfDays;
                this->MonthlyAvgOADryBulbTempMaxDiff = MonthlyDailyDryBulbMax - MonthlyDailyDryBulbMin;
                this->MonthlyDailyAverageDryBulbTemp = MonthlyAverageDryBulbTemp;
                this->OADryBulbWeatherDataProcessed = true;
            } else {
                ShowSevereError(state, "CalcAnnualAndMonthlyDryBulbTemp: weather file or stat file does not exist.");
                ShowContinueError(state, "Weather file: " + state.files.inputWeatherFilePath.filePath.string() + ".");
                ShowContinueError(state, "Stat file: " + state.files.inStatFilePath.filePath.string() + ".");
                ShowContinueError(state, "Water Mains Monthly Temperature cannot be calculated using CorrelationFromWeatherFile method.");
                ShowContinueError(state, "Instead a fixed default value of 10.0 C will be used.");
            }
        }
    }

    void ReportWaterMainsTempParameters(EnergyPlusData &state)
    {
        // PURPOSE OF THIS SUBROUTINE:
        // report site water mains temperature object user inputs and/or parameters calculated
        // from weather or stat file

        std::map<WeatherManager::WaterMainsTempCalcMethod, std::string> const calcMethodMap{
            {WeatherManager::WaterMainsTempCalcMethod::Schedule, "Schedule"},
            {WeatherManager::WaterMainsTempCalcMethod::Correlation, "Correlation"},
            {WeatherManager::WaterMainsTempCalcMethod::CorrelationFromWeatherFile, "CorrelationFromWeatherFile"}};

        if (!state.files.eio.good()) {
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

        switch (state.dataWeatherManager->WaterMainsTempsMethod) {
        case WaterMainsTempCalcMethod::Schedule:
            *eiostream << "Site Water Mains Temperature Information,";
            *eiostream << calcMethodMap.at(state.dataWeatherManager->WaterMainsTempsMethod) << ","
                       << state.dataWeatherManager->WaterMainsTempsScheduleName << ",";
            *eiostream << format("{:.2R}", state.dataWeatherManager->WaterMainsTempsAnnualAvgAirTemp) << ","
                       << format("{:.2R}", state.dataWeatherManager->WaterMainsTempsMaxDiffAirTemp) << ",";
            *eiostream << "NA\n";
            break;
        case WaterMainsTempCalcMethod::Correlation:
            *eiostream << "Site Water Mains Temperature Information,";
            *eiostream << calcMethodMap.at(state.dataWeatherManager->WaterMainsTempsMethod) << ","
                       << "NA"
                       << ",";
            *eiostream << format("{:.2R}", state.dataWeatherManager->WaterMainsTempsAnnualAvgAirTemp) << ","
                       << format("{:.2R}", state.dataWeatherManager->WaterMainsTempsMaxDiffAirTemp) << ",";
            *eiostream << "NA\n";
            break;
        case WaterMainsTempCalcMethod::CorrelationFromWeatherFile:
            if (state.dataWeatherManager->OADryBulbAverage.OADryBulbWeatherDataProcessed) {
                *eiostream << "Site Water Mains Temperature Information,";
                *eiostream << calcMethodMap.at(state.dataWeatherManager->WaterMainsTempsMethod) << ","
                           << "NA"
                           << ",";
                *eiostream << format("{:.2R}", state.dataWeatherManager->OADryBulbAverage.AnnualAvgOADryBulbTemp) << ","
                           << format("{:.2R}", state.dataWeatherManager->OADryBulbAverage.MonthlyAvgOADryBulbTempMaxDiff) << ","
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
                           << "," << format("{:.1R}", 10.0) << '\n';
            }
            break;
        default:
            *eiostream << "Site Water Mains Temperature Information,";
            *eiostream << "FixedDefault"
                       << ","
                       << "NA"
                       << ","
                       << "NA"
                       << ","
                       << "NA"
                       << "," << format("{:.1R}", 10.0) << '\n';
            break;
        }

        print(state.files.eio, "{}", ss.str());
    }

    void calcSky(EnergyPlusData &state,
                 Real64 &HorizIRSky,
                 Real64 &SkyTemp,
                 Real64 OpaqueSkyCover,
                 Real64 DryBulb,
                 Real64 DewPoint,
                 Real64 RelHum,
                 Real64 IRHoriz)
    {
        Real64 ESky;

        if (IRHoriz <= 0.0) IRHoriz = 9999.0;

        if (!state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).UseWeatherFileHorizontalIR || IRHoriz >= 9999.0) {
            // Missing or user defined to not use IRHoriz from weather, using sky cover and clear sky emissivity
            ESky = CalcSkyEmissivity(state,
                                     state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel,
                                     OpaqueSkyCover,
                                     DryBulb,
                                     DewPoint,
                                     RelHum);
            HorizIRSky = ESky * state.dataWeatherManager->Sigma * pow_4(DryBulb + DataGlobalConstants::KelvinConv);
            if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::BruntModel ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::IdsoModel ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::BerdahlMartinModel ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::SkyTAlgorithmA ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::ClarkAllenModel) {
                SkyTemp = (DryBulb + DataGlobalConstants::KelvinConv) * root_4(ESky) - DataGlobalConstants::KelvinConv;
            } else {
                SkyTemp = 0.0; // dealt with later
            }
        } else {
            // Valid IR from weather files
            HorizIRSky = IRHoriz;
            if (state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::BruntModel ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::IdsoModel ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::BerdahlMartinModel ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::SkyTAlgorithmA ||
                state.dataWeatherManager->Environment(state.dataWeatherManager->Envrn).SkyTempModel == EmissivityCalcType::ClarkAllenModel) {
                SkyTemp = root_4(IRHoriz / state.dataWeatherManager->Sigma) - DataGlobalConstants::KelvinConv;
            } else {
                SkyTemp = 0.0; // dealt with later
            }
        }
    }

} // namespace WeatherManager

} // namespace EnergyPlus
