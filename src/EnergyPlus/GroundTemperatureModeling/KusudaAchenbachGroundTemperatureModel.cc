// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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
#include <memory>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <EnergyPlus/GroundTemperatureModeling/KusudaAchenbachGroundTemperatureModel.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace EnergyPlus {

//******************************************************************************

// Kusuda model factory
std::shared_ptr<KusudaGroundTempsModel> KusudaGroundTempsModel::KusudaGTMFactory(EnergyPlusData &state, std::string objectName)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Matt Mitchell
    //       DATE WRITTEN   Summer 2015

    // PURPOSE OF THIS SUBROUTINE:
    // Reads input and creates instance of Kusuda ground temps model

    // Locals
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool found = false;
    int NumNums;
    int NumAlphas;
    int IOStat;

    // New shared pointer for this model object
    std::shared_ptr<KusudaGroundTempsModel> thisModel(new KusudaGroundTempsModel());

    GroundTempObjType objType = GroundTempObjType::KusudaGroundTemp;

    std::string_view const cCurrentModuleObject = GroundTemperatureManager::groundTempModelNamesUC[static_cast<int>(objType)];
    int numCurrModels = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    for (int modelNum = 1; modelNum <= numCurrModels; ++modelNum) {

        state.dataInputProcessing->inputProcessor->getObjectItem(
            state, cCurrentModuleObject, modelNum, state.dataIPShortCut->cAlphaArgs, NumAlphas, state.dataIPShortCut->rNumericArgs, NumNums, IOStat);

        if (objectName == state.dataIPShortCut->cAlphaArgs(1)) {

            // Read input into object here
            thisModel->objectName = state.dataIPShortCut->cAlphaArgs(1);
            thisModel->objectType = objType;
            thisModel->groundThermalDiffisivity =
                state.dataIPShortCut->rNumericArgs(1) / (state.dataIPShortCut->rNumericArgs(2) * state.dataIPShortCut->rNumericArgs(3));

            bool useGroundTempDataForKusuda =
                state.dataIPShortCut->rNumericArgs(4) || state.dataIPShortCut->rNumericArgs(5) || state.dataIPShortCut->rNumericArgs(6);

            if (useGroundTempDataForKusuda) {
                // Use Kusuda Parameters
                thisModel->aveGroundTemp = state.dataIPShortCut->rNumericArgs(4);
                thisModel->aveGroundTempAmplitude = state.dataIPShortCut->rNumericArgs(5);
                thisModel->phaseShiftInSecs = state.dataIPShortCut->rNumericArgs(6) * Constant::SecsInDay;
            } else {
                // Use data from Site:GroundTemperature:Shallow to generate parameters

                int monthsInYear(12);
                int avgDaysInMonth(30);
                int monthOfMinSurfTemp(0);
                Real64 averageGroundTemp(0);
                Real64 amplitudeOfGroundTemp(0);
                Real64 phaseShiftOfMinGroundTempDays(0);
                Real64 minSurfTemp(100);  // Set high month 1 temp will be lower and actually get updated
                Real64 maxSurfTemp(-100); // Set low initially but will get updated

                std::shared_ptr<BaseGroundTempsModel> shallowObj = GroundTemperatureManager::GetGroundTempModelAndInit(
                    state,
                    static_cast<std::string>(
                        GroundTemperatureManager::groundTempModelNamesUC[static_cast<int>(GroundTempObjType::SiteShallowGroundTemp)]),
                    "");

                for (int monthIndex = 1; monthIndex <= 12; ++monthIndex) {
                    Real64 currMonthTemp = shallowObj->getGroundTempAtTimeInMonths(state, 0.0, monthIndex);

                    // Calculate Average Ground Temperature for all 12 months of the year:
                    averageGroundTemp += currMonthTemp;

                    // Need max temp, min temp, and month of min surf temp to set amplitude and month of min surf temp
                    if (currMonthTemp <= minSurfTemp) {
                        monthOfMinSurfTemp = monthIndex;
                        minSurfTemp = currMonthTemp;
                    }

                    if (currMonthTemp >= maxSurfTemp) {
                        maxSurfTemp = currMonthTemp;
                    }
                }

                averageGroundTemp /= monthsInYear;

                amplitudeOfGroundTemp = (maxSurfTemp - minSurfTemp) / 2.0;

                phaseShiftOfMinGroundTempDays = monthOfMinSurfTemp * avgDaysInMonth;

                // Assign to KA Model
                thisModel->aveGroundTemp = averageGroundTemp;
                thisModel->aveGroundTempAmplitude = amplitudeOfGroundTemp;
                thisModel->phaseShiftInSecs = phaseShiftOfMinGroundTempDays * Constant::SecsInDay;
            }

            found = true;
            break;
        }
    }

    if (found) {
        state.dataGrndTempModelMgr->groundTempModels.push_back(thisModel);
        return thisModel;
    } else {
        ShowFatalError(state,
                       fmt::format("{}--Errors getting input for ground temperature model",
                                   GroundTemperatureManager::groundTempModelNames[static_cast<int>(objType)]));
        return nullptr;
    }
}

//******************************************************************************

Real64 KusudaGroundTempsModel::getGroundTemp(EnergyPlusData &state)
{
    // AUTHOR         Matt Mitchell
    // DATE WRITTEN   June 2015

    // PURPOSE OF THIS FUNCTION:
    // Returns a ground temperature

    // METHODOLOGY EMPLOYED:
    // Kusuda and Achenbach correlation is used

    Real64 const secsInYear = Constant::SecsInDay * state.dataWeather->NumDaysInYear;

    Real64 term1 = -depth * std::sqrt(Constant::Pi / (secsInYear * groundThermalDiffisivity));
    Real64 term2 = (2 * Constant::Pi / secsInYear) *
                   (simTimeInSeconds - phaseShiftInSecs - (depth / 2) * std::sqrt(secsInYear / (Constant::Pi * groundThermalDiffisivity)));

    return aveGroundTemp - aveGroundTempAmplitude * std::exp(term1) * std::cos(term2);
}

//******************************************************************************

Real64 KusudaGroundTempsModel::getGroundTempAtTimeInSeconds(EnergyPlusData &state, Real64 const _depth, Real64 const _seconds)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Matt Mitchell
    //       DATE WRITTEN   Summer 2015

    // PURPOSE OF THIS SUBROUTINE:
    // Returns the ground temperature when input time is in seconds

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 const secondsInYear = state.dataWeather->NumDaysInYear * Constant::SecsInDay;

    depth = _depth;

    simTimeInSeconds = _seconds;

    if (simTimeInSeconds > secondsInYear) {
        simTimeInSeconds = remainder(simTimeInSeconds, secondsInYear);
    }

    // Get and return ground temperature
    return getGroundTemp(state);
}

//******************************************************************************

Real64 KusudaGroundTempsModel::getGroundTempAtTimeInMonths(EnergyPlusData &state, Real64 const _depth, int const _month)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Matt Mitchell
    //       DATE WRITTEN   Summer 2015

    // PURPOSE OF THIS SUBROUTINE:
    // Returns the ground temperature when input time is in months

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 const aveSecondsInMonth = (state.dataWeather->NumDaysInYear / 12) * Constant::SecsInDay;
    Real64 const secondsPerYear = state.dataWeather->NumDaysInYear * Constant::SecsInDay;

    depth = _depth;

    simTimeInSeconds = aveSecondsInMonth * ((_month - 1) + 0.5);

    if (simTimeInSeconds > secondsPerYear) {
        simTimeInSeconds = remainder(simTimeInSeconds, secondsPerYear);
    }

    // Get and return ground temperature
    return getGroundTemp(state);
}

//******************************************************************************

} // namespace EnergyPlus
