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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/EarthTube.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::EarthTube {
// Module containing the data for Earth Tube system

// MODULE INFORMATION:
//       AUTHOR         Kwang Ho Lee
//       DATE WRITTEN   November 2005

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithyms required to manage the EarthTube System Component

// REFERENCES:
// 1. M. Krarti, "Analytical Model to Predict Annual Soil Surface Temperature Variation",
// Journal of Solar Energy Engineering 117, 1995, pp 91-99
// 2. K. Labs In: J. Cook, editor, "Passive Cooling",
// Cambridge Massachusetts, MIT Press, 1989, pp 206-212

constexpr std::array<std::string_view, static_cast<int>(Ventilation::Num)> ventilationNamesUC = {"NATURAL", "INTAKE", "EXHAUST"};
constexpr std::array<std::string_view, static_cast<int>(SoilType::Num)> soilTypeNamesUC = {
    "HEAVYANDSATURATED", "HEAVYANDDAMP", "HEAVYANDDRY", "LIGHTANDDRY"};

void ManageEarthTube(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the simulation of EarthTube unit.
    // This driver manages the calls to all of
    // the other drivers and simulation algorithms.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound(false);

    // Obtains and Allocates heat balance related parameters from input file
    if (state.dataEarthTube->GetInputFlag) {
        GetEarthTube(state, ErrorsFound);
        state.dataEarthTube->GetInputFlag = false;
    }

    if (state.dataEarthTube->EarthTubeSys.empty()) return;

    CalcEarthTube(state);

    ReportEarthTube(state);
}

void GetEarthTube(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine obtains input data for EarthTube units and
    // stores it in the EarthTube data structure.

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 constexpr EarthTubeTempLimit(100.0); // degrees Celsius

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlpha;
    int NumNumber;
    int IOStat;
    int Loop;
    Array1D_bool RepVarSet;

    auto &Zone(state.dataHeatBal->Zone);

    RepVarSet.dimension(state.dataGlobal->NumOfZones, true);

    // Following used for reporting
    state.dataEarthTube->ZnRptET.allocate(state.dataGlobal->NumOfZones);

    std::string cCurrentModuleObject = "ZoneEarthtube";
    int TotEarthTube = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    state.dataEarthTube->EarthTubeSys.allocate(TotEarthTube);

    for (Loop = 1; Loop <= TotEarthTube; ++Loop) {
        auto &thisEarthTube = state.dataEarthTube->EarthTubeSys(Loop);
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlpha,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        // First Alpha is Zone Name
        thisEarthTube.ZonePtr = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), Zone);
        if (thisEarthTube.ZonePtr == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaFieldNames(1) + " not found=" + state.dataIPShortCut->cAlphaArgs(1));
            ErrorsFound = true;
        }

        // Second Alpha is Schedule Name
        thisEarthTube.SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
        if (thisEarthTube.SchedPtr == 0) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                ShowSevereError(state,
                                cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaFieldNames(2) + " is required, missing for " +
                                    state.dataIPShortCut->cAlphaFieldNames(1) + '=' + state.dataIPShortCut->cAlphaArgs(1));
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                    " entered=" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                    state.dataIPShortCut->cAlphaArgs(1));
            }
            ErrorsFound = true;
        }

        // Overall parameters and their limits
        thisEarthTube.DesignLevel = state.dataIPShortCut->rNumericArgs(1);

        thisEarthTube.MinTemperature = state.dataIPShortCut->rNumericArgs(2);
        if ((thisEarthTube.MinTemperature < -EarthTubeTempLimit) || (thisEarthTube.MinTemperature > EarthTubeTempLimit)) {
            ShowSevereError(state,
                            format("{}: {}={} must have a minimum temperature between -{:.0R}C and {:.0R}C",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   EarthTubeTempLimit,
                                   EarthTubeTempLimit));
            ShowContinueError(state, format("Entered value={:.0R}", thisEarthTube.MinTemperature));
            ErrorsFound = true;
        }

        thisEarthTube.MaxTemperature = state.dataIPShortCut->rNumericArgs(3);
        if ((thisEarthTube.MaxTemperature < -EarthTubeTempLimit) || (thisEarthTube.MaxTemperature > EarthTubeTempLimit)) {
            ShowSevereError(state,
                            format("{}: {}={} must have a maximum temperature between -{:.0R}C and {:.0R}C",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   EarthTubeTempLimit,
                                   EarthTubeTempLimit));
            ShowContinueError(state, format("Entered value={:.0R}", thisEarthTube.MaxTemperature));
            ErrorsFound = true;
        }

        thisEarthTube.DelTemperature = state.dataIPShortCut->rNumericArgs(4); //  3/12/03  Negative del temp now allowed COP

        // if we have a blank, then just set it to the Natural type, otherwise, search on it
        if (state.dataIPShortCut->cAlphaArgs(3).empty()) {
            thisEarthTube.FanType = Ventilation::Natural;
        } else {
            thisEarthTube.FanType = static_cast<Ventilation>(getEnumerationValue(ventilationNamesUC, state.dataIPShortCut->cAlphaArgs(3)));
            if (thisEarthTube.FanType == Ventilation::Invalid) {
                ShowSevereError(state,
                                cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' + state.dataIPShortCut->cAlphaArgs(1) +
                                    ", " + state.dataIPShortCut->cAlphaFieldNames(3) + " invalid=" + state.dataIPShortCut->cAlphaArgs(3));
                ErrorsFound = true;
            }
        }

        thisEarthTube.FanPressure = state.dataIPShortCut->rNumericArgs(5);
        if (thisEarthTube.FanPressure < 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(5),
                                   thisEarthTube.FanPressure));
            ErrorsFound = true;
        }

        thisEarthTube.FanEfficiency = state.dataIPShortCut->rNumericArgs(6);
        if ((thisEarthTube.FanEfficiency <= 0.0) || (thisEarthTube.FanEfficiency > 1.0)) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be greater than zero and less than or equal to one, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(6),
                                   thisEarthTube.FanEfficiency));
            ErrorsFound = true;
        }

        thisEarthTube.r1 = state.dataIPShortCut->rNumericArgs(7);
        if (thisEarthTube.r1 <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(7),
                                   thisEarthTube.r1));
            ErrorsFound = true;
        }

        thisEarthTube.r2 = state.dataIPShortCut->rNumericArgs(8);
        if (thisEarthTube.r2 <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(8),
                                   thisEarthTube.r2));
            ErrorsFound = true;
        }

        thisEarthTube.r3 = 2.0 * thisEarthTube.r1;

        thisEarthTube.PipeLength = state.dataIPShortCut->rNumericArgs(9);
        if (thisEarthTube.PipeLength <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(9),
                                   thisEarthTube.PipeLength));
            ErrorsFound = true;
        }

        thisEarthTube.PipeThermCond = state.dataIPShortCut->rNumericArgs(10);
        if (thisEarthTube.PipeThermCond <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(10),
                                   thisEarthTube.PipeThermCond));
            ErrorsFound = true;
        }

        thisEarthTube.z = state.dataIPShortCut->rNumericArgs(11);
        if (thisEarthTube.z <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(11),
                                   thisEarthTube.z));
            ErrorsFound = true;
        }
        if (thisEarthTube.z <= (thisEarthTube.r1 + thisEarthTube.r2 + thisEarthTube.r3)) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be greater than 3*{} + {} entered value={:.2R} ref sum={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(11),
                                   state.dataIPShortCut->cNumericFieldNames(7),
                                   state.dataIPShortCut->cNumericFieldNames(8),
                                   thisEarthTube.z,
                                   thisEarthTube.r1 + thisEarthTube.r2 + thisEarthTube.r3));
            ErrorsFound = true;
        }

        auto soilType = static_cast<SoilType>(getEnumerationValue(soilTypeNamesUC, state.dataIPShortCut->cAlphaArgs(4)));
        constexpr std::array<Real64, static_cast<int>(SoilType::Num)> thermalDiffusivity = {0.0781056, 0.055728, 0.0445824, 0.024192};
        constexpr std::array<Real64, static_cast<int>(SoilType::Num)> thermalConductivity = {2.42, 1.3, 0.865, 0.346};
        if (soilType == SoilType::Invalid) {
            ShowSevereError(state,
                            cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' + state.dataIPShortCut->cAlphaArgs(1) +
                                ", " + state.dataIPShortCut->cAlphaFieldNames(4) + " invalid=" + state.dataIPShortCut->cAlphaArgs(4));
            ErrorsFound = true;
        } else {
            thisEarthTube.SoilThermDiff = thermalDiffusivity[static_cast<int>(soilType)];
            thisEarthTube.SoilThermCond = thermalConductivity[static_cast<int>(soilType)];
        }

        thisEarthTube.AverSoilSurTemp = state.dataIPShortCut->rNumericArgs(12);
        thisEarthTube.ApmlSoilSurTemp = state.dataIPShortCut->rNumericArgs(13);
        thisEarthTube.SoilSurPhaseConst = int(state.dataIPShortCut->rNumericArgs(14));

        // Override any user input for cases where natural ventilation is being used
        if (thisEarthTube.FanType == Ventilation::Natural) {
            thisEarthTube.FanPressure = 0.0;
            thisEarthTube.FanEfficiency = 1.0;
        }

        thisEarthTube.ConstantTermCoef = state.dataIPShortCut->rNumericArgs(15);
        thisEarthTube.TemperatureTermCoef = state.dataIPShortCut->rNumericArgs(16);
        thisEarthTube.VelocityTermCoef = state.dataIPShortCut->rNumericArgs(17);
        thisEarthTube.VelocitySQTermCoef = state.dataIPShortCut->rNumericArgs(18);

        if (thisEarthTube.ZonePtr > 0) {
            if (RepVarSet(thisEarthTube.ZonePtr)) {
                RepVarSet(thisEarthTube.ZonePtr) = false;
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeHeatLoss,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::NonState,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeHeatLossRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeHeatGain,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::NonState,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeHeatGainRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Air Flow Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeVolume,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::NonState,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Current Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeVolFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Standard Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeVolFlowRateStd,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Air Flow Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeMass,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::NonState,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeMassFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Water Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeWaterMassFlowRate,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Fan Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeFanElec,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::NonState,
                                    Zone(thisEarthTube.ZonePtr).Name,
                                    _,
                                    "Electricity",
                                    _,
                                    _,
                                    "Building");
                SetupOutputVariable(state,
                                    "Earth Tube Fan Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeFanElecPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Inlet Air Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeAirTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Ground Interface Temperature",
                                    OutputProcessor::Unit::C,
                                    thisEarthTube.GroundTempz1z2t,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Outdoor Air Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeOATreatmentPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Inlet Wet Bulb Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeWetBulbTemp,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Inlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    state.dataEarthTube->ZnRptET(thisEarthTube.ZonePtr).EarthTubeHumRat,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::State,
                                    Zone(thisEarthTube.ZonePtr).Name);
            }
        }
    }

    CheckEarthTubesInZones(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

    if (ErrorsFound) {
        ShowFatalError(state, cCurrentModuleObject + ": Errors getting input.  Program terminates.");
    }
}

void CheckEarthTubesInZones(EnergyPlusData &state,
                            std::string const &ZoneName, // name of zone for error reporting
                            std::string_view FieldName,  // name of earth tube in input
                            bool &ErrorsFound            // Found a problem
)
{
    // Check to make sure there is only one earth tube statement per zone
    int numEarthTubes = (int)state.dataEarthTube->EarthTubeSys.size();
    for (int Loop = 1; Loop <= numEarthTubes - 1; ++Loop) {
        for (int Loop1 = Loop + 1; Loop1 <= numEarthTubes; ++Loop1) {
            if (state.dataEarthTube->EarthTubeSys(Loop).ZonePtr == state.dataEarthTube->EarthTubeSys(Loop1).ZonePtr) {
                ShowSevereError(state, ZoneName + " has more than one " + std::string{FieldName} + " associated with it.");
                ShowContinueError(state,
                                  "Only one " + std::string{FieldName} + " is allowed per zone.  Check the definitions of " + std::string{FieldName});
                ShowContinueError(state, "in your input file and make sure that there is only one defined for each zone.");
                ErrorsFound = true;
            }
        }
    }
}

void CalcEarthTube(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the components making up the EarthTube unit.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int NZ;
    Real64 Process1;        // Variable Used in the Middle of the Calculation
    Real64 GroundTempz1z2t; // Average Ground Temperature between Depth z1 and z2 at time t

    Real64 AirThermCond;         // Thermal Conductivity of Air (W/mC)
    Real64 AirKinemVisco;        // Kinematic Viscosity of Air (m2/s)
    Real64 AirThermDiffus;       // Thermal Diffusivity of Air (m2/s)
    Real64 Re;                   // Reynolds Number for Flow Inside Pipe
    Real64 Pr;                   // Prandtl Number for Flow Inside Pipe
    Real64 Nu;                   // Nusselt Number for Flow Inside Pipe
    Real64 fa;                   // Friction Factor of Pipe
    Real64 PipeHeatTransCoef;    // Convective Heat Transfer Coefficient at Inner Pipe Surface
    Real64 Rc;                   // Thermal Resistance due to Convection between Air and Pipe Inner Surface
    Real64 Rp;                   // Thermal Resistance due to Conduction between Pipe Inner and Outer Surface
    Real64 Rs;                   // Thermal Resistance due to Conduction between Pipe Outer Surface and Soil
    Real64 Rt;                   // Total Thermal Resistance between Pipe Air and Soil
    Real64 OverallHeatTransCoef; // Overall Heat Transfer Coefficient of Earth Tube
    Real64 AverPipeAirVel;       // Average Pipe Air Velocity (m/s)
    Real64 AirMassFlowRate;      // Actual Mass Flow Rate of Air inside Pipe
    Real64 AirSpecHeat;          // Specific Heat of Air
    Real64 AirDensity;           // Density of Air

    Real64 EVF;
    state.dataHeatBalFanSys->MCPTE = 0.0;
    state.dataHeatBalFanSys->MCPE = 0.0;
    state.dataHeatBalFanSys->EAMFL = 0.0;
    state.dataHeatBalFanSys->EAMFLxHumRat = 0.0;

    int numEarthTubes = (int)state.dataEarthTube->EarthTubeSys.size();
    for (Loop = 1; Loop <= numEarthTubes; ++Loop) {
        auto &thisEarthTube = state.dataEarthTube->EarthTubeSys(Loop);
        NZ = thisEarthTube.ZonePtr;
        thisEarthTube.FanPower = 0.0;
        // Skip this if the zone is below the minimum temperature limit
        if (state.dataHeatBalFanSys->MAT(NZ) < thisEarthTube.MinTemperature) continue;
        // Skip this if the zone is above the maximum temperature limit
        if (state.dataHeatBalFanSys->MAT(NZ) > thisEarthTube.MaxTemperature) continue;
        // Skip if below the temperature difference limit
        if (std::abs(state.dataHeatBalFanSys->MAT(NZ) - state.dataEnvrn->OutDryBulbTemp) < thisEarthTube.DelTemperature) continue;

        AirDensity =
            Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);
        AirSpecHeat = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        EVF = thisEarthTube.DesignLevel * ScheduleManager::GetCurrentScheduleValue(state, thisEarthTube.SchedPtr);
        state.dataHeatBalFanSys->MCPE(NZ) =
            EVF * AirDensity * AirSpecHeat *
            (thisEarthTube.ConstantTermCoef +
             std::abs(state.dataEnvrn->OutDryBulbTemp - state.dataHeatBalFanSys->MAT(NZ)) * thisEarthTube.TemperatureTermCoef +
             state.dataEnvrn->WindSpeed * (thisEarthTube.VelocityTermCoef + state.dataEnvrn->WindSpeed * thisEarthTube.VelocitySQTermCoef));

        state.dataHeatBalFanSys->EAMFL(NZ) = state.dataHeatBalFanSys->MCPE(NZ) / AirSpecHeat;
        if (thisEarthTube.FanEfficiency > 0.0) {
            thisEarthTube.FanPower = state.dataHeatBalFanSys->EAMFL(NZ) * thisEarthTube.FanPressure / (thisEarthTube.FanEfficiency * AirDensity);
        }

        AverPipeAirVel = EVF / DataGlobalConstants::Pi / pow_2(thisEarthTube.r1);
        AirMassFlowRate = EVF * AirDensity;

        // Calculation of Average Ground Temperature between Depth z1 and z2 at time t
        GroundTempz1z2t = thisEarthTube.AverSoilSurTemp -
                          thisEarthTube.ApmlSoilSurTemp *
                              std::exp(-thisEarthTube.z * std::sqrt(DataGlobalConstants::Pi / 365.0 / thisEarthTube.SoilThermDiff)) *
                              std::cos(2.0 * DataGlobalConstants::Pi / 365.0 *
                                       (state.dataEnvrn->DayOfYear - thisEarthTube.SoilSurPhaseConst -
                                        thisEarthTube.z / 2.0 * std::sqrt(365.0 / DataGlobalConstants::Pi / thisEarthTube.SoilThermDiff)));
        thisEarthTube.GroundTempz1z2t = GroundTempz1z2t;

        // Calculation of Convective Heat Transfer Coefficient at Inner Pipe Surface
        AirThermCond = 0.02442 + 0.6992 * state.dataEnvrn->OutDryBulbTemp / 10000.0;
        AirKinemVisco = (0.1335 + 0.000925 * state.dataEnvrn->OutDryBulbTemp) / 10000.0;
        AirThermDiffus = (0.0014 * state.dataEnvrn->OutDryBulbTemp + 0.1872) / 10000.0;
        Re = 2.0 * thisEarthTube.r1 * AverPipeAirVel / AirKinemVisco;
        Pr = AirKinemVisco / AirThermDiffus;
        if (Re <= 2300.0) {
            Nu = 3.66;
        } else if (Re <= 4000.0) {
            fa = std::pow(1.58 * std::log(Re) - 3.28, -2);
            Process1 = (fa / 2.0) * (Re - 1000.0) * Pr / (1.0 + 12.7 * std::sqrt(fa / 2.0) * (std::pow(Pr, 2.0 / 3.0) - 1.0));
            Nu = (Process1 - 3.66) / (1700.0) * Re + (4000.0 * 3.66 - 2300.0 * Process1) / 1700.0;
        } else {
            fa = std::pow(1.58 * std::log(Re) - 3.28, -2);
            Nu = (fa / 2.0) * (Re - 1000.0) * Pr / (1.0 + 12.7 * std::sqrt(fa / 2.0) * (std::pow(Pr, 2.0 / 3.0) - 1.0));
        }
        PipeHeatTransCoef = Nu * AirThermCond / 2.0 / thisEarthTube.r1;

        // Calculation of Thermal Resistance and Overall Heat Transfer Coefficient
        Rc = 1.0 / 2.0 / DataGlobalConstants::Pi / thisEarthTube.r1 / PipeHeatTransCoef;
        Rp = std::log((thisEarthTube.r1 + thisEarthTube.r2) / thisEarthTube.r1) / 2.0 / DataGlobalConstants::Pi / thisEarthTube.PipeThermCond;
        Rs = std::log((thisEarthTube.r1 + thisEarthTube.r2 + thisEarthTube.r3) / (thisEarthTube.r1 + thisEarthTube.r2)) / 2.0 /
             DataGlobalConstants::Pi / thisEarthTube.SoilThermCond;
        Rt = Rc + Rp + Rs;
        OverallHeatTransCoef = 1.0 / Rt;

        if (AirMassFlowRate * AirSpecHeat == 0.0) {
            thisEarthTube.InsideAirTemp = GroundTempz1z2t;

        } else {

            // Calculation of Pipe Outlet Air Temperature
            if (state.dataEnvrn->OutDryBulbTemp > GroundTempz1z2t) {
                Process1 = (std::log(std::abs(state.dataEnvrn->OutDryBulbTemp - GroundTempz1z2t)) * AirMassFlowRate * AirSpecHeat -
                            OverallHeatTransCoef * thisEarthTube.PipeLength) /
                           (AirMassFlowRate * AirSpecHeat);
                thisEarthTube.InsideAirTemp = std::exp(Process1) + GroundTempz1z2t;
            } else if (state.dataEnvrn->OutDryBulbTemp == GroundTempz1z2t) {
                thisEarthTube.InsideAirTemp = GroundTempz1z2t;
            } else {
                Process1 = (std::log(std::abs(state.dataEnvrn->OutDryBulbTemp - GroundTempz1z2t)) * AirMassFlowRate * AirSpecHeat -
                            OverallHeatTransCoef * thisEarthTube.PipeLength) /
                           (AirMassFlowRate * AirSpecHeat);
                thisEarthTube.InsideAirTemp = GroundTempz1z2t - std::exp(Process1);
            }
        }

        thisEarthTube.CalcEarthTubeHumRat(state, NZ);
    }
}

void EarthTubeData::CalcEarthTubeHumRat(EnergyPlusData &state, int const NZ)
{ // Zone number (index)

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005
    //       MODIFIED       Rick Strand, June 2017 (made this a separate subroutine)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine determines the leaving humidity ratio for the EarthTube
    // and calculates parameters associated with humidity ratio.

    Real64 InsideDewPointTemp = Psychrometrics::PsyTdpFnWPb(state, state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);
    Real64 InsideHumRat;

    if (this->InsideAirTemp >= InsideDewPointTemp) {
        InsideHumRat = state.dataEnvrn->OutHumRat;
        Real64 const InsideEnthalpy = Psychrometrics::PsyHFnTdbW(this->InsideAirTemp, state.dataEnvrn->OutHumRat);
        // Intake fans will add some heat to the air, raising the temperature for an intake fan...
        if (this->FanType == Ventilation::Intake) {
            Real64 OutletAirEnthalpy;
            if (state.dataHeatBalFanSys->EAMFL(NZ) == 0.0) {
                OutletAirEnthalpy = InsideEnthalpy;
            } else {
                OutletAirEnthalpy = InsideEnthalpy + this->FanPower / state.dataHeatBalFanSys->EAMFL(NZ);
            }
            this->AirTemp = Psychrometrics::PsyTdbFnHW(OutletAirEnthalpy, state.dataEnvrn->OutHumRat);
        } else {
            this->AirTemp = this->InsideAirTemp;
        }
        state.dataHeatBalFanSys->MCPTE(NZ) = state.dataHeatBalFanSys->MCPE(NZ) * this->AirTemp;

    } else {
        InsideHumRat = Psychrometrics::PsyWFnTdpPb(state, this->InsideAirTemp, state.dataEnvrn->OutBaroPress);
        Real64 const InsideEnthalpy = Psychrometrics::PsyHFnTdbW(this->InsideAirTemp, InsideHumRat);
        // Intake fans will add some heat to the air, raising the temperature for an intake fan...
        if (this->FanType == Ventilation::Intake) {
            Real64 OutletAirEnthalpy;
            if (state.dataHeatBalFanSys->EAMFL(NZ) == 0.0) {
                OutletAirEnthalpy = InsideEnthalpy;
            } else {
                OutletAirEnthalpy = InsideEnthalpy + this->FanPower / state.dataHeatBalFanSys->EAMFL(NZ);
            }
            this->AirTemp = Psychrometrics::PsyTdbFnHW(OutletAirEnthalpy, InsideHumRat);
        } else {
            this->AirTemp = this->InsideAirTemp;
        }
        state.dataHeatBalFanSys->MCPTE(NZ) = state.dataHeatBalFanSys->MCPE(NZ) * this->AirTemp;
    }

    this->HumRat = InsideHumRat;
    this->WetBulbTemp = Psychrometrics::PsyTwbFnTdbWPb(state, this->InsideAirTemp, InsideHumRat, state.dataEnvrn->OutBaroPress);
    state.dataHeatBalFanSys->EAMFLxHumRat(NZ) = state.dataHeatBalFanSys->EAMFL(NZ) * InsideHumRat;
}

void ReportEarthTube(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005
    //       MODIFIED       B. Griffith April 2010 added output reports

    // PURPOSE OF THIS SUBROUTINE: This subroutine fills remaining report variables.

    Real64 const ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    for (int ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...
        auto &thisZone = state.dataEarthTube->ZnRptET(ZoneLoop);
        auto &zoneMCPE = state.dataHeatBalFanSys->MCPE(ZoneLoop);
        auto &zoneTemp = state.dataHeatBalFanSys->ZT(ZoneLoop);

        // Break the infiltration load into heat gain and loss components.
        Real64 const AirDensity =
            Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);
        Real64 const CpAir = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        thisZone.EarthTubeVolume = (zoneMCPE / CpAir / AirDensity) * ReportingConstant;
        thisZone.EarthTubeMass = (zoneMCPE / CpAir) * ReportingConstant;
        thisZone.EarthTubeVolFlowRate = zoneMCPE / CpAir / AirDensity;
        thisZone.EarthTubeVolFlowRateStd = zoneMCPE / CpAir / state.dataEnvrn->StdRhoAir;
        thisZone.EarthTubeMassFlowRate = zoneMCPE / CpAir;
        thisZone.EarthTubeWaterMassFlowRate = state.dataHeatBalFanSys->EAMFLxHumRat(ZoneLoop);

        thisZone.EarthTubeFanElec = 0.0;
        thisZone.EarthTubeAirTemp = 0.0;
        for (auto &thisEarthTube : state.dataEarthTube->EarthTubeSys) {
            if (thisEarthTube.ZonePtr == ZoneLoop) {
                thisZone.EarthTubeFanElec = thisEarthTube.FanPower * ReportingConstant;
                thisZone.EarthTubeFanElecPower = thisEarthTube.FanPower;

                // Break the EarthTube load into heat gain and loss components.
                if (zoneTemp > thisEarthTube.AirTemp) {
                    thisZone.EarthTubeHeatLoss = zoneMCPE * (zoneTemp - thisEarthTube.AirTemp) * ReportingConstant;
                    thisZone.EarthTubeHeatLossRate = zoneMCPE * (zoneTemp - thisEarthTube.AirTemp);
                    thisZone.EarthTubeHeatGain = 0.0;
                    thisZone.EarthTubeHeatGainRate = 0.0;
                } else if (zoneTemp <= thisEarthTube.AirTemp) {
                    thisZone.EarthTubeHeatGain = zoneMCPE * (thisEarthTube.AirTemp - zoneTemp) * ReportingConstant;
                    thisZone.EarthTubeHeatGainRate = zoneMCPE * (thisEarthTube.AirTemp - zoneTemp);
                    thisZone.EarthTubeHeatLoss = 0.0;
                    thisZone.EarthTubeHeatLossRate = 0.0;
                }

                thisZone.EarthTubeAirTemp = thisEarthTube.AirTemp;
                thisZone.EarthTubeWetBulbTemp = thisEarthTube.WetBulbTemp;
                thisZone.EarthTubeHumRat = thisEarthTube.HumRat;
                thisZone.EarthTubeOATreatmentPower = zoneMCPE * (thisEarthTube.AirTemp - state.dataEnvrn->OutDryBulbTemp);
                break; // DO loop
            }
        }

    } // ... end of zone loads report variable update loop.
}

} // namespace EnergyPlus::EarthTube
