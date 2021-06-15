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

// Using/Aliasing
using namespace DataEnvironment;
using namespace DataHeatBalFanSys;
using namespace DataHeatBalance;
using namespace DataSurfaces;
using namespace Psychrometrics;

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

    if (state.dataEarthTube->TotEarthTube == 0) return;

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

    // Using/Aliasing

    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleValuesForDay;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const EarthTubeTempLimit(100.0); // degrees Celsius

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
    state.dataEarthTube->TotEarthTube = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    state.dataEarthTube->EarthTubeSys.allocate(state.dataEarthTube->TotEarthTube);

    for (Loop = 1; Loop <= state.dataEarthTube->TotEarthTube; ++Loop) {
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
        state.dataEarthTube->EarthTubeSys(Loop).ZonePtr = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), Zone);
        if (state.dataEarthTube->EarthTubeSys(Loop).ZonePtr == 0) {
            ShowSevereError(
                state, cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaFieldNames(1) + " not found=" + state.dataIPShortCut->cAlphaArgs(1));
            ErrorsFound = true;
        }

        // Second Alpha is Schedule Name
        state.dataEarthTube->EarthTubeSys(Loop).SchedName = state.dataIPShortCut->cAlphaArgs(2);
        state.dataEarthTube->EarthTubeSys(Loop).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
        if (state.dataEarthTube->EarthTubeSys(Loop).SchedPtr == 0) {
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
        state.dataEarthTube->EarthTubeSys(Loop).DesignLevel = state.dataIPShortCut->rNumericArgs(1);

        state.dataEarthTube->EarthTubeSys(Loop).MinTemperature = state.dataIPShortCut->rNumericArgs(2);
        if ((state.dataEarthTube->EarthTubeSys(Loop).MinTemperature < -EarthTubeTempLimit) ||
            (state.dataEarthTube->EarthTubeSys(Loop).MinTemperature > EarthTubeTempLimit)) {
            ShowSevereError(state,
                            format("{}: {}={} must have a minimum temperature between -{:.0R}C and {:.0R}C",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   EarthTubeTempLimit,
                                   EarthTubeTempLimit));
            ShowContinueError(state, format("Entered value={:.0R}", state.dataEarthTube->EarthTubeSys(Loop).MinTemperature));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).MaxTemperature = state.dataIPShortCut->rNumericArgs(3);
        if ((state.dataEarthTube->EarthTubeSys(Loop).MaxTemperature < -EarthTubeTempLimit) ||
            (state.dataEarthTube->EarthTubeSys(Loop).MaxTemperature > EarthTubeTempLimit)) {
            ShowSevereError(state,
                            format("{}: {}={} must have a maximum temperature between -{:.0R}C and {:.0R}C",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   EarthTubeTempLimit,
                                   EarthTubeTempLimit));
            ShowContinueError(state, format("Entered value={:.0R}", state.dataEarthTube->EarthTubeSys(Loop).MaxTemperature));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).DelTemperature = state.dataIPShortCut->rNumericArgs(4); //  3/12/03  Negative del temp now allowed COP

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3)); // Fan type character input-->convert to integer
            if (SELECT_CASE_var == "EXHAUST") {
                state.dataEarthTube->EarthTubeSys(Loop).FanType = EarthTubeVentilation::Exhaust;
            } else if (SELECT_CASE_var == "INTAKE") {
                state.dataEarthTube->EarthTubeSys(Loop).FanType = EarthTubeVentilation::Intake;
            } else if ((SELECT_CASE_var == "NATURAL") || (SELECT_CASE_var == "NONE") || (SELECT_CASE_var.empty())) {
                state.dataEarthTube->EarthTubeSys(Loop).FanType = EarthTubeVentilation::Natural;
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' + state.dataIPShortCut->cAlphaArgs(1) +
                                    ", " + state.dataIPShortCut->cAlphaFieldNames(3) + " invalid=" + state.dataIPShortCut->cAlphaArgs(3));
                ErrorsFound = true;
            }
        }

        state.dataEarthTube->EarthTubeSys(Loop).FanPressure = state.dataIPShortCut->rNumericArgs(5);
        if (state.dataEarthTube->EarthTubeSys(Loop).FanPressure < 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(5),
                                   state.dataEarthTube->EarthTubeSys(Loop).FanPressure));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).FanEfficiency = state.dataIPShortCut->rNumericArgs(6);
        if ((state.dataEarthTube->EarthTubeSys(Loop).FanEfficiency <= 0.0) || (state.dataEarthTube->EarthTubeSys(Loop).FanEfficiency > 1.0)) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be greater than zero and less than or equal to one, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(6),
                                   state.dataEarthTube->EarthTubeSys(Loop).FanEfficiency));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).r1 = state.dataIPShortCut->rNumericArgs(7);
        if (state.dataEarthTube->EarthTubeSys(Loop).r1 <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(7),
                                   state.dataEarthTube->EarthTubeSys(Loop).r1));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).r2 = state.dataIPShortCut->rNumericArgs(8);
        if (state.dataEarthTube->EarthTubeSys(Loop).r2 <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(8),
                                   state.dataEarthTube->EarthTubeSys(Loop).r2));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).r3 = 2.0 * state.dataEarthTube->EarthTubeSys(Loop).r1;

        state.dataEarthTube->EarthTubeSys(Loop).PipeLength = state.dataIPShortCut->rNumericArgs(9);
        if (state.dataEarthTube->EarthTubeSys(Loop).PipeLength <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(9),
                                   state.dataEarthTube->EarthTubeSys(Loop).PipeLength));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).PipeThermCond = state.dataIPShortCut->rNumericArgs(10);
        if (state.dataEarthTube->EarthTubeSys(Loop).PipeThermCond <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(10),
                                   state.dataEarthTube->EarthTubeSys(Loop).PipeThermCond));
            ErrorsFound = true;
        }

        state.dataEarthTube->EarthTubeSys(Loop).z = state.dataIPShortCut->rNumericArgs(11);
        if (state.dataEarthTube->EarthTubeSys(Loop).z <= 0.0) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be positive, entered value={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(11),
                                   state.dataEarthTube->EarthTubeSys(Loop).z));
            ErrorsFound = true;
        }
        if (state.dataEarthTube->EarthTubeSys(Loop).z <=
            (state.dataEarthTube->EarthTubeSys(Loop).r1 + state.dataEarthTube->EarthTubeSys(Loop).r2 + state.dataEarthTube->EarthTubeSys(Loop).r3)) {
            ShowSevereError(state,
                            format("{}: {}={}, {} must be greater than 3*{} + {} entered value={:.2R} ref sum={:.2R}",
                                   cCurrentModuleObject,
                                   state.dataIPShortCut->cAlphaFieldNames(1),
                                   state.dataIPShortCut->cAlphaArgs(1),
                                   state.dataIPShortCut->cNumericFieldNames(11),
                                   state.dataIPShortCut->cNumericFieldNames(7),
                                   state.dataIPShortCut->cNumericFieldNames(8),
                                   state.dataEarthTube->EarthTubeSys(Loop).z,
                                   state.dataEarthTube->EarthTubeSys(Loop).r1 + state.dataEarthTube->EarthTubeSys(Loop).r2 +
                                       state.dataEarthTube->EarthTubeSys(Loop).r3));
            ErrorsFound = true;
        }

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(4)); // Soil type character input --> convert to number
            if (SELECT_CASE_var == "HEAVYANDSATURATED") {
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermDiff = 0.0781056;
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermCond = 2.42;
            } else if (SELECT_CASE_var == "HEAVYANDDAMP") {
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermDiff = 0.055728;
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermCond = 1.3;
            } else if (SELECT_CASE_var == "HEAVYANDDRY") {
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermDiff = 0.0445824;
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermCond = 0.865;
            } else if (SELECT_CASE_var == "LIGHTANDDRY") {
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermDiff = 0.024192;
                state.dataEarthTube->EarthTubeSys(Loop).SoilThermCond = 0.346;
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + ": " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' + state.dataIPShortCut->cAlphaArgs(1) +
                                    ", " + state.dataIPShortCut->cAlphaFieldNames(4) + " invalid=" + state.dataIPShortCut->cAlphaArgs(4));
                ErrorsFound = true;
            }
        }

        state.dataEarthTube->EarthTubeSys(Loop).AverSoilSurTemp = state.dataIPShortCut->rNumericArgs(12);
        state.dataEarthTube->EarthTubeSys(Loop).ApmlSoilSurTemp = state.dataIPShortCut->rNumericArgs(13);
        state.dataEarthTube->EarthTubeSys(Loop).SoilSurPhaseConst = int(state.dataIPShortCut->rNumericArgs(14));

        // Override any user input for cases where natural ventilation is being used
        if (state.dataEarthTube->EarthTubeSys(Loop).FanType == EarthTubeVentilation::Natural) {
            state.dataEarthTube->EarthTubeSys(Loop).FanPressure = 0.0;
            state.dataEarthTube->EarthTubeSys(Loop).FanEfficiency = 1.0;
        }

        state.dataEarthTube->EarthTubeSys(Loop).ConstantTermCoef = state.dataIPShortCut->rNumericArgs(15);
        state.dataEarthTube->EarthTubeSys(Loop).TemperatureTermCoef = state.dataIPShortCut->rNumericArgs(16);
        state.dataEarthTube->EarthTubeSys(Loop).VelocityTermCoef = state.dataIPShortCut->rNumericArgs(17);
        state.dataEarthTube->EarthTubeSys(Loop).VelocitySQTermCoef = state.dataIPShortCut->rNumericArgs(18);

        if (state.dataEarthTube->EarthTubeSys(Loop).ZonePtr > 0) {
            if (RepVarSet(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr)) {
                RepVarSet(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr) = false;
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeHeatLoss,
                                    "System",
                                    "NonState",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeHeatLossRate,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeHeatGain,
                                    "System",
                                    "NonState",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeHeatGainRate,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Air Flow Volume",
                                    OutputProcessor::Unit::m3,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeVolume,
                                    "System",
                                    "NonState",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Current Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeVolFlowRate,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Standard Density Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeVolFlowRateStd,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Air Flow Mass",
                                    OutputProcessor::Unit::kg,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeMass,
                                    "System",
                                    "NonState",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeMassFlowRate,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Water Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeWaterMassFlowRate,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Fan Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeFanElec,
                                    "System",
                                    "NonState",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name,
                                    _,
                                    "Electricity",
                                    _,
                                    _,
                                    "Building");
                SetupOutputVariable(state,
                                    "Earth Tube Fan Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeFanElecPower,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Inlet Air Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeAirTemp,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Ground Interface Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataEarthTube->EarthTubeSys(Loop).GroundTempz1z2t,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Outdoor Air Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeOATreatmentPower,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Inlet Wet Bulb Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeWetBulbTemp,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
                SetupOutputVariable(state,
                                    "Earth Tube Zone Inlet Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    state.dataEarthTube->ZnRptET(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).EarthTubeHumRat,
                                    "System",
                                    "State",
                                    Zone(state.dataEarthTube->EarthTubeSys(Loop).ZonePtr).Name);
            }
        }
    }

    CheckEarthTubesInZones(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

    if (ErrorsFound) {
        ShowFatalError(state, cCurrentModuleObject + ": Errors getting input.  Program terminates.");
    }
}

void CheckEarthTubesInZones(EnergyPlusData &state,
                            std::string const &ZoneName,  // name of zone for error reporting
                            std::string_view FieldName, // name of earth tube in input
                            bool &ErrorsFound             // Found a problem
)
{
    // Check to make sure there is only one earth tube statement per zone
    for (int Loop = 1; Loop <= state.dataEarthTube->TotEarthTube - 1; ++Loop) {
        for (int Loop1 = Loop + 1; Loop1 <= state.dataEarthTube->TotEarthTube; ++Loop1) {
            if (state.dataEarthTube->EarthTubeSys(Loop).ZonePtr == state.dataEarthTube->EarthTubeSys(Loop1).ZonePtr) {
                ShowSevereError(state, ZoneName + " has more than one " + std::string{FieldName} + " associated with it.");
                ShowContinueError(state, "Only one " + std::string{FieldName} + " is allowed per zone.  Check the definitions of " + std::string{FieldName});
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

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;

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

    for (Loop = 1; Loop <= state.dataEarthTube->TotEarthTube; ++Loop) {

        NZ = state.dataEarthTube->EarthTubeSys(Loop).ZonePtr;
        state.dataEarthTube->EarthTubeSys(Loop).FanPower = 0.0;
        // Skip this if the zone is below the minimum temperature limit
        if (state.dataHeatBalFanSys->MAT(NZ) < state.dataEarthTube->EarthTubeSys(Loop).MinTemperature) continue;
        // Skip this if the zone is above the maximum temperature limit
        if (state.dataHeatBalFanSys->MAT(NZ) > state.dataEarthTube->EarthTubeSys(Loop).MaxTemperature) continue;
        // Skip if below the temperature difference limit
        if (std::abs(state.dataHeatBalFanSys->MAT(NZ) - state.dataEnvrn->OutDryBulbTemp) < state.dataEarthTube->EarthTubeSys(Loop).DelTemperature)
            continue;

        AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);
        AirSpecHeat = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        EVF = state.dataEarthTube->EarthTubeSys(Loop).DesignLevel * GetCurrentScheduleValue(state, state.dataEarthTube->EarthTubeSys(Loop).SchedPtr);
        state.dataHeatBalFanSys->MCPE(NZ) =
            EVF * AirDensity * AirSpecHeat *
            (state.dataEarthTube->EarthTubeSys(Loop).ConstantTermCoef +
             std::abs(state.dataEnvrn->OutDryBulbTemp - state.dataHeatBalFanSys->MAT(NZ)) *
                 state.dataEarthTube->EarthTubeSys(Loop).TemperatureTermCoef +
             state.dataEnvrn->WindSpeed * (state.dataEarthTube->EarthTubeSys(Loop).VelocityTermCoef +
                                           state.dataEnvrn->WindSpeed * state.dataEarthTube->EarthTubeSys(Loop).VelocitySQTermCoef));

        state.dataHeatBalFanSys->EAMFL(NZ) = state.dataHeatBalFanSys->MCPE(NZ) / AirSpecHeat;
        if (state.dataEarthTube->EarthTubeSys(Loop).FanEfficiency > 0.0) {
            state.dataEarthTube->EarthTubeSys(Loop).FanPower = state.dataHeatBalFanSys->EAMFL(NZ) *
                                                               state.dataEarthTube->EarthTubeSys(Loop).FanPressure /
                                                               (state.dataEarthTube->EarthTubeSys(Loop).FanEfficiency * AirDensity);
        }

        AverPipeAirVel = EVF / DataGlobalConstants::Pi / pow_2(state.dataEarthTube->EarthTubeSys(Loop).r1);
        AirMassFlowRate = EVF * AirDensity;

        // Calculation of Average Ground Temperature between Depth z1 and z2 at time t
        GroundTempz1z2t = state.dataEarthTube->EarthTubeSys(Loop).AverSoilSurTemp -
                          state.dataEarthTube->EarthTubeSys(Loop).ApmlSoilSurTemp *
                              std::exp(-state.dataEarthTube->EarthTubeSys(Loop).z *
                                       std::sqrt(DataGlobalConstants::Pi / 365.0 / state.dataEarthTube->EarthTubeSys(Loop).SoilThermDiff)) *
                              std::cos(2.0 * DataGlobalConstants::Pi / 365.0 *
                                       (state.dataEnvrn->DayOfYear - state.dataEarthTube->EarthTubeSys(Loop).SoilSurPhaseConst -
                                        state.dataEarthTube->EarthTubeSys(Loop).z / 2.0 *
                                            std::sqrt(365.0 / DataGlobalConstants::Pi / state.dataEarthTube->EarthTubeSys(Loop).SoilThermDiff)));
        state.dataEarthTube->EarthTubeSys(Loop).GroundTempz1z2t = GroundTempz1z2t;

        // Calculation of Convective Heat Transfer Coefficient at Inner Pipe Surface
        AirThermCond = 0.02442 + 0.6992 * state.dataEnvrn->OutDryBulbTemp / 10000.0;
        AirKinemVisco = (0.1335 + 0.000925 * state.dataEnvrn->OutDryBulbTemp) / 10000.0;
        AirThermDiffus = (0.0014 * state.dataEnvrn->OutDryBulbTemp + 0.1872) / 10000.0;
        Re = 2.0 * state.dataEarthTube->EarthTubeSys(Loop).r1 * AverPipeAirVel / AirKinemVisco;
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
        PipeHeatTransCoef = Nu * AirThermCond / 2.0 / state.dataEarthTube->EarthTubeSys(Loop).r1;

        // Calculation of Thermal Resistance and Overall Heat Transfer Coefficient
        Rc = 1.0 / 2.0 / DataGlobalConstants::Pi / state.dataEarthTube->EarthTubeSys(Loop).r1 / PipeHeatTransCoef;
        Rp = std::log((state.dataEarthTube->EarthTubeSys(Loop).r1 + state.dataEarthTube->EarthTubeSys(Loop).r2) /
                      state.dataEarthTube->EarthTubeSys(Loop).r1) /
             2.0 / DataGlobalConstants::Pi / state.dataEarthTube->EarthTubeSys(Loop).PipeThermCond;
        Rs = std::log((state.dataEarthTube->EarthTubeSys(Loop).r1 + state.dataEarthTube->EarthTubeSys(Loop).r2 +
                       state.dataEarthTube->EarthTubeSys(Loop).r3) /
                      (state.dataEarthTube->EarthTubeSys(Loop).r1 + state.dataEarthTube->EarthTubeSys(Loop).r2)) /
             2.0 / DataGlobalConstants::Pi / state.dataEarthTube->EarthTubeSys(Loop).SoilThermCond;
        Rt = Rc + Rp + Rs;
        OverallHeatTransCoef = 1.0 / Rt;

        if (AirMassFlowRate * AirSpecHeat == 0.0) {
            state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp = GroundTempz1z2t;

        } else {

            // Calculation of Pipe Outlet Air Temperature
            if (state.dataEnvrn->OutDryBulbTemp > GroundTempz1z2t) {
                Process1 = (std::log(std::abs(state.dataEnvrn->OutDryBulbTemp - GroundTempz1z2t)) * AirMassFlowRate * AirSpecHeat -
                            OverallHeatTransCoef * state.dataEarthTube->EarthTubeSys(Loop).PipeLength) /
                           (AirMassFlowRate * AirSpecHeat);
                state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp = std::exp(Process1) + GroundTempz1z2t;
            } else if (state.dataEnvrn->OutDryBulbTemp == GroundTempz1z2t) {
                state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp = GroundTempz1z2t;
            } else {
                Process1 = (std::log(std::abs(state.dataEnvrn->OutDryBulbTemp - GroundTempz1z2t)) * AirMassFlowRate * AirSpecHeat -
                            OverallHeatTransCoef * state.dataEarthTube->EarthTubeSys(Loop).PipeLength) /
                           (AirMassFlowRate * AirSpecHeat);
                state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp = GroundTempz1z2t - std::exp(Process1);
            }
        }

        CalcEarthTubeHumRat(state, Loop, NZ);
    }
}

void CalcEarthTubeHumRat(EnergyPlusData &state,
                         int const Loop, // EarthTube number (index)
                         int const NZ    // Zone number (index)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005
    //       MODIFIED       Rick Strand, June 2017 (made this a separate subroutine)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine determines the leaving humidity ratio for the EarthTube
    // and calculates parameters associated with humidity ratio.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 InsideEnthalpy;
    Real64 OutletAirEnthalpy;
    Real64 InsideDewPointTemp;
    Real64 InsideHumRat;

    InsideDewPointTemp = PsyTdpFnWPb(state, state.dataEnvrn->OutHumRat, state.dataEnvrn->OutBaroPress);

    if (state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp >= InsideDewPointTemp) {
        InsideHumRat = state.dataEnvrn->OutHumRat;
        InsideEnthalpy = PsyHFnTdbW(state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp, state.dataEnvrn->OutHumRat);
        // Intake fans will add some heat to the air, raising the temperature for an intake fan...
        if (state.dataEarthTube->EarthTubeSys(Loop).FanType == EarthTubeVentilation::Intake) {
            if (state.dataHeatBalFanSys->EAMFL(NZ) == 0.0) {
                OutletAirEnthalpy = InsideEnthalpy;
            } else {
                OutletAirEnthalpy = InsideEnthalpy + state.dataEarthTube->EarthTubeSys(Loop).FanPower / state.dataHeatBalFanSys->EAMFL(NZ);
            }
            state.dataEarthTube->EarthTubeSys(Loop).AirTemp = PsyTdbFnHW(OutletAirEnthalpy, state.dataEnvrn->OutHumRat);
        } else {
            state.dataEarthTube->EarthTubeSys(Loop).AirTemp = state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp;
        }
        state.dataHeatBalFanSys->MCPTE(NZ) = state.dataHeatBalFanSys->MCPE(NZ) * state.dataEarthTube->EarthTubeSys(Loop).AirTemp;

    } else {
        InsideHumRat = PsyWFnTdpPb(state, state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp, state.dataEnvrn->OutBaroPress);
        InsideEnthalpy = PsyHFnTdbW(state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp, InsideHumRat);
        // Intake fans will add some heat to the air, raising the temperature for an intake fan...
        if (state.dataEarthTube->EarthTubeSys(Loop).FanType == EarthTubeVentilation::Intake) {
            if (state.dataHeatBalFanSys->EAMFL(NZ) == 0.0) {
                OutletAirEnthalpy = InsideEnthalpy;
            } else {
                OutletAirEnthalpy = InsideEnthalpy + state.dataEarthTube->EarthTubeSys(Loop).FanPower / state.dataHeatBalFanSys->EAMFL(NZ);
            }
            state.dataEarthTube->EarthTubeSys(Loop).AirTemp = PsyTdbFnHW(OutletAirEnthalpy, InsideHumRat);
        } else {
            state.dataEarthTube->EarthTubeSys(Loop).AirTemp = state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp;
        }
        state.dataHeatBalFanSys->MCPTE(NZ) = state.dataHeatBalFanSys->MCPE(NZ) * state.dataEarthTube->EarthTubeSys(Loop).AirTemp;
    }

    state.dataEarthTube->EarthTubeSys(Loop).HumRat = InsideHumRat;
    state.dataEarthTube->EarthTubeSys(Loop).WetBulbTemp =
        PsyTwbFnTdbWPb(state, state.dataEarthTube->EarthTubeSys(Loop).InsideAirTemp, InsideHumRat, state.dataEnvrn->OutBaroPress);
    state.dataHeatBalFanSys->EAMFLxHumRat(NZ) = state.dataHeatBalFanSys->EAMFL(NZ) * InsideHumRat;
}

void ReportEarthTube(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Kwang Ho Lee
    //       DATE WRITTEN   November 2005
    //       MODIFIED       B. Griffith April 2010 added output reports

    // PURPOSE OF THIS SUBROUTINE: This subroutine fills remaining report variables.

    // Using/Aliasing
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneLoop;     // Counter for the # of zones (nz)
    int EarthTubeNum; // Counter for EarthTube statements
    Real64 AirDensity;
    Real64 CpAir;
    Real64 ReportingConstant; // reporting constant for this module

    ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

    for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...

        // Break the infiltration load into heat gain and loss components.
        AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);
        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeVolume = (state.dataHeatBalFanSys->MCPE(ZoneLoop) / CpAir / AirDensity) * ReportingConstant;
        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeMass = (state.dataHeatBalFanSys->MCPE(ZoneLoop) / CpAir) * ReportingConstant;
        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeVolFlowRate = state.dataHeatBalFanSys->MCPE(ZoneLoop) / CpAir / AirDensity;
        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeVolFlowRateStd = state.dataHeatBalFanSys->MCPE(ZoneLoop) / CpAir / state.dataEnvrn->StdRhoAir;
        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeMassFlowRate = state.dataHeatBalFanSys->MCPE(ZoneLoop) / CpAir;
        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeWaterMassFlowRate = state.dataHeatBalFanSys->EAMFLxHumRat(ZoneLoop);

        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeFanElec = 0.0;
        state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeAirTemp = 0.0;
        for (EarthTubeNum = 1; EarthTubeNum <= state.dataEarthTube->TotEarthTube; ++EarthTubeNum) {
            if (state.dataEarthTube->EarthTubeSys(EarthTubeNum).ZonePtr == ZoneLoop) {
                state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeFanElec =
                    state.dataEarthTube->EarthTubeSys(EarthTubeNum).FanPower * ReportingConstant;
                state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeFanElecPower = state.dataEarthTube->EarthTubeSys(EarthTubeNum).FanPower;

                // Break the EarthTube load into heat gain and loss components.

                if (state.dataHeatBalFanSys->ZT(ZoneLoop) > state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp) {

                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatLoss =
                        state.dataHeatBalFanSys->MCPE(ZoneLoop) *
                        (state.dataHeatBalFanSys->ZT(ZoneLoop) - state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp) * ReportingConstant;
                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatLossRate =
                        state.dataHeatBalFanSys->MCPE(ZoneLoop) *
                        (state.dataHeatBalFanSys->ZT(ZoneLoop) - state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp);
                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatGain = 0.0;
                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatGainRate = 0.0;

                } else if (state.dataHeatBalFanSys->ZT(ZoneLoop) <= state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp) {

                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatGain =
                        state.dataHeatBalFanSys->MCPE(ZoneLoop) *
                        (state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp - state.dataHeatBalFanSys->ZT(ZoneLoop)) * ReportingConstant;
                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatGainRate =
                        state.dataHeatBalFanSys->MCPE(ZoneLoop) *
                        (state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp - state.dataHeatBalFanSys->ZT(ZoneLoop));
                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatLoss = 0.0;
                    state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHeatLossRate = 0.0;
                }

                state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeAirTemp = state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp;
                state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeWetBulbTemp = state.dataEarthTube->EarthTubeSys(EarthTubeNum).WetBulbTemp;
                state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeHumRat = state.dataEarthTube->EarthTubeSys(EarthTubeNum).HumRat;
                state.dataEarthTube->ZnRptET(ZoneLoop).EarthTubeOATreatmentPower =
                    state.dataHeatBalFanSys->MCPE(ZoneLoop) *
                    (state.dataEarthTube->EarthTubeSys(EarthTubeNum).AirTemp - state.dataEnvrn->OutDryBulbTemp);
                break; // DO loop
            }
        }

    } // ... end of zone loads report variable update loop.
}

} // namespace EnergyPlus::EarthTube
