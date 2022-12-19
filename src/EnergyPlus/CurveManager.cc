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
#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// Third-party Headers
#include <fast_float/fast_float.h>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace Curve {
    // Module containing the Curve Manager routines

    // MODULE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2000
    //       MODIFIED       January 2006, Rick Strand, added a curve type (quadratic-linear)
    //                      July 2006, L. Gu, added a new curve type (bicubic)

    //                      July 2006, Brent Griffith, added triquadratic curve
    //                       RR added exponential curve
    //                      May 2009 Brent griffith add EMS actuator registry and override (for custom equations)
    //                      August 2010, Richard Raustad, FSEC, added Table:* objects
    //                      August 2014, Rick Strand, added a curve type (cubic-linear)
    //                      Future Improvements:
    //                          Subroutine PerformanceTableObject is not really needed (and is probably slower)
    //                          since Subroutine TableLookupObject can do the same thing. The difference
    //                          is that Sub PerformanceTableObject does a linear interpolation without extrapolation.
    //                          More math is also involved. Sub TableLookupObject can also do this if a) the limits
    //                          of the input data use the boundaries of the tabular data, b) the arrays are corrected
    //                          to use this other subroutine, and c) the Number of Interpolation Points is set to 2.
    //                      22Aug2010 Craig Wray, added new curves for fan component model:
    //                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
    //                          RectangularHyperbola2, ExponentialDecay
    //                      March 2012, Atefe Makhmalbaf and Heejin Cho, added a new curve type (QuadLinear)
    //                      Jan 2021 Yueyue, added a new curve type (QuintLinear)
    //                      Aug.  2014, Rongpeng Zhang, added a new curve type (ChillerPartLoadWithLift)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To provide the capabilities of getting the curve data from the input,
    // validating it, and storing it in such a manner that the curve manager
    // can provide the simulation with performance curve output.

    std::map<std::string, Btwxt::Method> BtwxtManager::interpMethods = // NOLINT(cert-err58-cpp)
        {{"Linear", Btwxt::Method::LINEAR}, {"Cubic", Btwxt::Method::CUBIC}};

    std::map<std::string, Btwxt::Method> BtwxtManager::extrapMethods = // NOLINT(cert-err58-cpp)
        {{"Linear", Btwxt::Method::LINEAR}, {"Constant", Btwxt::Method::CONSTANT}};

    // Functions
    void BtwxtMessageCallback(const Btwxt::MsgLevel messageType, const std::string message, void *contextPtr)
    {
        std::pair<EnergyPlusData *, std::string> contextPair = *(std::pair<EnergyPlusData *, std::string> *)contextPtr;
        std::string fullMessage = format("{}: {}", contextPair.second, message);
        if (messageType == Btwxt::MsgLevel::MSG_ERR) {
            ShowSevereError(*contextPair.first, fullMessage);
            ShowFatalError(*contextPair.first, "Btwxt: Errors discovered, program terminates.");
        } else {
            if (static_cast<int>(messageType) >= Btwxt::LOG_LEVEL) {
                if (messageType == Btwxt::MsgLevel::MSG_WARN) {
                    ShowWarningError(*contextPair.first, fullMessage);
                } else {
                    ShowMessage(*contextPair.first, fullMessage);
                }
            }
        }
    }

    void ResetPerformanceCurveOutput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   August 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        // PURPOSE OF THIS SUBROUTINE:
        // Reset curve outputs prior to simulating air loops, plant loops, etc.
        // This allows the report variable for curve/table objects to show an inactive state.

        for (auto &c : state.dataCurveManager->PerfCurve) {
            c.output = DataLoopNode::SensedNodeFlagValue;
            for (auto &i : c.inputs) {
                i = DataLoopNode::SensedNodeFlagValue;
            }
        }
    }

    Real64 CurveValue(EnergyPlusData &state,
                      int const CurveIndex,        // index of curve in curve array
                      Real64 const Var1,           // 1st independent variable
                      Optional<Real64 const> Var2, // 2nd independent variable
                      Optional<Real64 const> Var3, // 3rd independent variable
                      Optional<Real64 const> Var4, // 4th independent variable
                      Optional<Real64 const> Var5, // 5th independent variable
                      Optional<Real64 const> Var6  // 6th independent variable
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   May 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given the curve index and the values of 1 or 2 independent variables,
        // calls the curve or table routine to return the value of an equipment performance curve or table.

        // Return value
        Real64 CurveValue(0.0);

        // need to be careful on where and how resetting curve outputs to some "inactive value" is done
        // EMS can intercept curves and modify output
        if (state.dataGlobal->BeginEnvrnFlag && state.dataCurveManager->CurveValueMyBeginTimeStepFlag) {
            ResetPerformanceCurveOutput(state);
            state.dataCurveManager->CurveValueMyBeginTimeStepFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataCurveManager->CurveValueMyBeginTimeStepFlag = true;
        }

        if ((CurveIndex <= 0) || (CurveIndex > state.dataCurveManager->NumCurves)) {
            ShowFatalError(state, "CurveValue: Invalid curve passed.");
        }

        auto &thisCurve = state.dataCurveManager->PerfCurve(CurveIndex);
        switch (thisCurve.interpolationType) {
        case InterpType::EvaluateCurveToLimits:
            CurveValue = PerformanceCurveObject(state, CurveIndex, Var1, Var2, Var3, Var4, Var5);
            break;
        case InterpType::BtwxtMethod:
            CurveValue = BtwxtTableInterpolation(state, CurveIndex, Var1, Var2, Var3, Var4, Var5, Var6);
            break;
        default:
            assert(false);
        }

        if (thisCurve.EMSOverrideOn) CurveValue = thisCurve.EMSOverrideCurveValue;

        thisCurve.output = CurveValue;
        thisCurve.inputs[0] = Var1;
        if (present(Var2)) thisCurve.inputs[1] = Var2;
        if (present(Var3)) thisCurve.inputs[2] = Var3;
        if (present(Var4)) thisCurve.inputs[3] = Var4;
        if (present(Var5)) thisCurve.inputs[4] = Var5;
        if (present(Var6)) thisCurve.inputs[5] = Var6;

        return CurveValue;
    }

    void GetCurveInput(EnergyPlusData &state)
    {
        // wrapper for GetInput to allow unit testing when fatal inputs are detected - follow pattern from GetSetPointManagerInputs()
        bool GetInputErrorsFound = false;

        GetCurveInputData(state, GetInputErrorsFound);
        state.dataCurveManager->GetCurvesInputFlag = false;

        if (GetInputErrorsFound) {
            ShowFatalError(state, "GetCurveInput: Errors found in getting Curve Objects.  Preceding condition(s) cause termination.");
        }
    }

    void GetCurveInputData(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       January 2006, Rick Strand, added a curve type (quadratic-linear)
        //                      July 2006, L. Gu, added a curve type (bicubic)
        //                      July 2006, BG added triquadratic.
        //                      April 2008, LL Added Linear Curve; July 2008, restructure for easier renaming
        //                      Feb 2009, R. Raustad - FSEC, added exponent curve
        //                      22Aug2010 Craig Wray, added new curves for fan component model:
        //                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
        //                          RectangularHyperbola2, ExponentialDecay
        //                      Aug.  2014, Rongpeng Zhang, added a new curve type (ChillerPartLoadWithLift)
        //                      Jan. 2017, Jason DeGraw, added WPC input into tables
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for EnergyPlus equipment performance curves

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string Alphas(14);       // Alpha items for object
        Array1D<Real64> Numbers(10000);  // Numeric items for object
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        int IOStatus;                    // Used in GetObjectItem
        std::string CurrentModuleObject; // for ease in renaming.
        int MaxTableNums(0);             // Maximum number of numeric input fields in Tables
        //   certain object in the input file

        // Find the number of each type of curve (note: Current Module object not used here, must rename manually)

        int const NumBiQuad = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Biquadratic");
        int const NumCubic = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Cubic");
        int const NumQuartic = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Quartic");
        int const NumQuad = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Quadratic");
        int const NumQLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:QuadLinear");
        int const NumQuintLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:QuintLinear");
        int const NumQuadLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:QuadraticLinear");
        int const NumCubicLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:CubicLinear");
        int const NumLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Linear");
        int const NumBicubic = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Bicubic");
        int const NumTriQuad = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Triquadratic");
        int const NumExponent = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Exponent");
        int const NumTableLookup = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Table:Lookup");
        int const NumFanPressRise = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:FanPressureRise");
        int const NumExpSkewNorm = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:ExponentialSkewNormal");
        int const NumSigmoid = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Sigmoid");
        int const NumRectHyper1 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:RectangularHyperbola1");
        int const NumRectHyper2 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:RectangularHyperbola2");
        int const NumExpDecay = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:ExponentialDecay");
        int const NumDoubleExpDecay = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:DoubleExponentialDecay");
        int const NumChillerPartLoadWithLift =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:ChillerPartLoadWithLift"); // zrp_Aug2014

        int const NumWPCValTab =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirflowNetwork:MultiZone:WindPressureCoefficientValues");

        state.dataCurveManager->NumCurves = NumBiQuad + NumCubic + NumQuad + NumQuadLinear + NumCubicLinear + NumLinear + NumBicubic + NumTriQuad +
                                            NumExponent + NumQuartic + NumTableLookup + NumFanPressRise + NumExpSkewNorm + NumSigmoid +
                                            NumRectHyper1 + NumRectHyper2 + NumExpDecay + NumDoubleExpDecay + NumQLinear + NumQuintLinear +
                                            NumChillerPartLoadWithLift + NumWPCValTab;

        // allocate the data structure
        state.dataCurveManager->PerfCurve.allocate(state.dataCurveManager->NumCurves);
        state.dataCurveManager->UniqueCurveNames.reserve(state.dataCurveManager->NumCurves);
        // initialize the array

        int CurveNum = 0; // keep track of the current curve index in the main curve array

        // Loop over biquadratic curves and load data
        CurrentModuleObject = "Curve:Biquadratic";
        for (int CurveIndex = 1; CurveIndex <= NumBiQuad; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;

            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            // could add checks for blank numeric fields, and use field names for errors.
            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::BiQuadratic;
            thisCurve.numDims = 2;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 6; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(7);
            thisCurve.inputLimits[0].max = Numbers(8);
            thisCurve.inputLimits[1].min = Numbers(9);
            thisCurve.inputLimits[1].max = Numbers(10);
            if (NumNumbers > 10 && !state.dataIPShortCut->lNumericFieldBlanks(11)) {
                thisCurve.outputLimits.min = Numbers(11);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 11 && !state.dataIPShortCut->lNumericFieldBlanks(12)) {
                thisCurve.outputLimits.max = Numbers(12);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         Numbers(8)));
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(9),
                                         Numbers(9),
                                         state.dataIPShortCut->cNumericFieldNames(10),
                                         Numbers(10)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for Y is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over ChillerPartLoadWithLift curves and load data //zrp_Aug2014
        CurrentModuleObject = "Curve:ChillerPartLoadWithLift";
        for (int CurveIndex = 1; CurveIndex <= NumChillerPartLoadWithLift; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);

            thisCurve.curveType = CurveType::ChillerPartLoadWithLift;
            thisCurve.numDims = 3;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;

            for (int in = 0; in < 12; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }

            thisCurve.inputLimits[0].min = Numbers(13);
            thisCurve.inputLimits[0].max = Numbers(14);
            thisCurve.inputLimits[1].min = Numbers(15);
            thisCurve.inputLimits[1].max = Numbers(16);
            thisCurve.inputLimits[2].min = Numbers(17);
            thisCurve.inputLimits[2].max = Numbers(18);

            if (NumNumbers > 18 && !state.dataIPShortCut->lNumericFieldBlanks(19)) {
                thisCurve.outputLimits.min = Numbers(19);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 19 && !state.dataIPShortCut->lNumericFieldBlanks(20)) {
                thisCurve.outputLimits.max = Numbers(20);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for Y is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, format("In {} named {} the OInput Unit Type for Z is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveOutputTypeValid(Alphas(5))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over cubic curves and load data
        CurrentModuleObject = "Curve:Cubic";
        for (int CurveIndex = 1; CurveIndex <= NumCubic; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            ++CurveNum;
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::Cubic;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 4; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(5);
            thisCurve.inputLimits[0].max = Numbers(6);
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                thisCurve.outputLimits.min = Numbers(7);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                thisCurve.outputLimits.max = Numbers(8);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5),
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         Numbers(6)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over quadrinomial curves and load data
        CurrentModuleObject = "Curve:Quartic";
        for (int CurveIndex = 1; CurveIndex <= NumQuartic; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::Quartic;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 5; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(6);
            thisCurve.inputLimits[0].max = Numbers(7);
            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                thisCurve.outputLimits.min = Numbers(8);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                thisCurve.outputLimits.max = Numbers(9);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(6) > Numbers(7)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         Numbers(6),
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over quadratic curves and load data
        CurrentModuleObject = "Curve:Quadratic";
        for (int CurveIndex = 1; CurveIndex <= NumQuad; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::Quadratic;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 3; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(4);
            thisCurve.inputLimits[0].max = Numbers(5);
            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                thisCurve.outputLimits.min = Numbers(6);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                thisCurve.outputLimits.max = Numbers(7);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(4),
                                         Numbers(4),
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over quadratic-linear curves and load data
        CurrentModuleObject = "Curve:QuadraticLinear";
        for (int CurveIndex = 1; CurveIndex <= NumQuadLinear; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::QuadraticLinear;
            thisCurve.numDims = 2;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 6; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(7);
            thisCurve.inputLimits[0].max = Numbers(8);
            thisCurve.inputLimits[1].min = Numbers(9);
            thisCurve.inputLimits[1].max = Numbers(10);
            if (NumNumbers > 10 && !state.dataIPShortCut->lNumericFieldBlanks(11)) {
                thisCurve.outputLimits.min = Numbers(11);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 11 && !state.dataIPShortCut->lNumericFieldBlanks(12)) {
                thisCurve.outputLimits.max = Numbers(12);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         Numbers(8)));
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(9),
                                         Numbers(9),
                                         state.dataIPShortCut->cNumericFieldNames(10),
                                         Numbers(10)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for Y is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over cubic-linear curves and load data
        CurrentModuleObject = "Curve:CubicLinear";
        for (int CurveIndex = 1; CurveIndex <= NumCubicLinear; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::CubicLinear;
            thisCurve.numDims = 2;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 6; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(7);
            thisCurve.inputLimits[0].max = Numbers(8);
            thisCurve.inputLimits[1].min = Numbers(9);
            thisCurve.inputLimits[1].max = Numbers(10);
            if (NumNumbers > 10 && !state.dataIPShortCut->lNumericFieldBlanks(11)) {
                thisCurve.outputLimits.min = Numbers(11);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 11 && !state.dataIPShortCut->lNumericFieldBlanks(12)) {
                thisCurve.outputLimits.max = Numbers(12);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         Numbers(8)));
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(9),
                                         Numbers(9),
                                         state.dataIPShortCut->cNumericFieldNames(10),
                                         Numbers(10)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for Y is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over linear curves and load data
        CurrentModuleObject = "Curve:Linear";
        for (int CurveIndex = 1; CurveIndex <= NumLinear; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::Linear;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 2; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(3);
            thisCurve.inputLimits[0].max = Numbers(4);
            if (NumNumbers > 4 && !state.dataIPShortCut->lNumericFieldBlanks(5)) {
                thisCurve.outputLimits.min = Numbers(5);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                thisCurve.outputLimits.max = Numbers(6);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(3) > Numbers(4)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(3),
                                         Numbers(3),
                                         state.dataIPShortCut->cNumericFieldNames(4),
                                         Numbers(4)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over bicubic curves and load data
        CurrentModuleObject = "Curve:Bicubic";
        for (int CurveIndex = 1; CurveIndex <= NumBicubic; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::BiCubic;
            thisCurve.numDims = 2;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 10; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(11);
            thisCurve.inputLimits[0].max = Numbers(12);
            thisCurve.inputLimits[1].min = Numbers(13);
            thisCurve.inputLimits[1].max = Numbers(14);
            if (NumNumbers > 14 && !state.dataIPShortCut->lNumericFieldBlanks(15)) {
                thisCurve.outputLimits.min = Numbers(15);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 15 && !state.dataIPShortCut->lNumericFieldBlanks(16)) {
                thisCurve.outputLimits.max = Numbers(16);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(11) > Numbers(12)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(11),
                                         Numbers(11),
                                         state.dataIPShortCut->cNumericFieldNames(12),
                                         Numbers(12)));
                ErrorsFound = true;
            }
            if (Numbers(13) > Numbers(14)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(13),
                                         Numbers(13),
                                         state.dataIPShortCut->cNumericFieldNames(14),
                                         Numbers(14)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for Y is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over Triquadratic curves and load data
        CurrentModuleObject = "Curve:Triquadratic";
        for (int CurveIndex = 1; CurveIndex <= NumTriQuad; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::TriQuadratic;
            thisCurve.numDims = 3;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            thisCurve.tri2ndOrder[0] = Numbers(1);
            thisCurve.tri2ndOrder[1] = Numbers(2);
            thisCurve.tri2ndOrder[2] = Numbers(3);
            thisCurve.tri2ndOrder[3] = Numbers(4);
            thisCurve.tri2ndOrder[4] = Numbers(5);
            thisCurve.tri2ndOrder[5] = Numbers(6);
            thisCurve.tri2ndOrder[6] = Numbers(7);
            thisCurve.tri2ndOrder[7] = Numbers(8);
            thisCurve.tri2ndOrder[8] = Numbers(9);
            thisCurve.tri2ndOrder[9] = Numbers(10);
            thisCurve.tri2ndOrder[10] = Numbers(11);
            thisCurve.tri2ndOrder[11] = Numbers(12);
            thisCurve.tri2ndOrder[12] = Numbers(13);
            thisCurve.tri2ndOrder[13] = Numbers(14);
            thisCurve.tri2ndOrder[14] = Numbers(15);
            thisCurve.tri2ndOrder[15] = Numbers(16);
            thisCurve.tri2ndOrder[16] = Numbers(17);
            thisCurve.tri2ndOrder[17] = Numbers(18);
            thisCurve.tri2ndOrder[18] = Numbers(19);
            thisCurve.tri2ndOrder[19] = Numbers(20);
            thisCurve.tri2ndOrder[20] = Numbers(21);
            thisCurve.tri2ndOrder[21] = Numbers(22);
            thisCurve.tri2ndOrder[22] = Numbers(23);
            thisCurve.tri2ndOrder[23] = Numbers(24);
            thisCurve.tri2ndOrder[24] = Numbers(25);
            thisCurve.tri2ndOrder[25] = Numbers(26);
            thisCurve.tri2ndOrder[26] = Numbers(27);
            thisCurve.inputLimits[0].min = Numbers(28);
            thisCurve.inputLimits[0].max = Numbers(29);
            thisCurve.inputLimits[1].min = Numbers(30);
            thisCurve.inputLimits[1].max = Numbers(31);
            thisCurve.inputLimits[2].min = Numbers(32);
            thisCurve.inputLimits[2].max = Numbers(33);
            if (NumNumbers > 33 && !state.dataIPShortCut->lNumericFieldBlanks(34)) {
                thisCurve.outputLimits.min = Numbers(34);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 34 && !state.dataIPShortCut->lNumericFieldBlanks(35)) {
                thisCurve.outputLimits.max = Numbers(35);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(28) > Numbers(29)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(28),
                                         Numbers(28),
                                         state.dataIPShortCut->cNumericFieldNames(29),
                                         Numbers(29)));
                ErrorsFound = true;
            }
            if (Numbers(30) > Numbers(31)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(30),
                                         Numbers(30),
                                         state.dataIPShortCut->cNumericFieldNames(31),
                                         Numbers(31)));
                ErrorsFound = true;
            }
            if (Numbers(32) > Numbers(33)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(32),
                                         Numbers(32),
                                         state.dataIPShortCut->cNumericFieldNames(33),
                                         Numbers(33)));
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for Y is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveInputTypeValid(Alphas(4))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for Z is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveOutputTypeValid(Alphas(5))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over quad linear curves and load data
        CurrentModuleObject = "Curve:QuadLinear";
        for (int CurveIndex = 1; CurveIndex <= NumQLinear; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::QuadLinear;
            thisCurve.numDims = 4;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 5; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(6);
            thisCurve.inputLimits[0].max = Numbers(7);
            thisCurve.inputLimits[1].min = Numbers(8);
            thisCurve.inputLimits[1].max = Numbers(9);
            thisCurve.inputLimits[2].min = Numbers(10);
            thisCurve.inputLimits[2].max = Numbers(11);
            thisCurve.inputLimits[3].min = Numbers(12);
            thisCurve.inputLimits[3].max = Numbers(13);

            if (NumNumbers > 13 && !state.dataIPShortCut->lNumericFieldBlanks(14)) {
                thisCurve.outputLimits.min = Numbers(14);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 14 && !state.dataIPShortCut->lNumericFieldBlanks(15)) {
                thisCurve.outputLimits.max = Numbers(15);
                thisCurve.outputLimits.maxPresent = true;
            }

            constexpr int NumVar = 4;
            std::string VarNames[NumVar] = {"w", "x", "y", "z"};
            for (int i = 1; i <= NumVar; ++i) {
                int MinIndex = 2 * i + 4;
                int MaxIndex = MinIndex + 1;
                if (Numbers(MinIndex) > Numbers(MaxIndex)) { // error
                    ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                    ShowContinueError(state,
                                      format("{} [{:.R2}] > {} [{.R2}]",
                                             state.dataIPShortCut->cNumericFieldNames(MinIndex),
                                             Numbers(MinIndex),
                                             state.dataIPShortCut->cNumericFieldNames(MaxIndex),
                                             Numbers(MaxIndex)));
                    ErrorsFound = true;
                }
                int InputTypeIndex = i + 1;
                if (NumAlphas >= InputTypeIndex) {
                    if (!IsCurveInputTypeValid(Alphas(InputTypeIndex))) {
                        ShowWarningError(
                            state, format("In {} named {} the Input Unit Type for {} is invalid.", CurrentModuleObject, Alphas(1), VarNames[i]));
                    }
                }
            }
            if (NumAlphas >= 6) {
                if (!IsCurveOutputTypeValid(Alphas(6))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over quint linear curves and load data
        CurrentModuleObject = "Curve:QuintLinear";
        for (int CurveIndex = 1; CurveIndex <= NumQuintLinear; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::QuintLinear;
            thisCurve.numDims = 5;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 6; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(7);
            thisCurve.inputLimits[0].max = Numbers(8);
            thisCurve.inputLimits[1].min = Numbers(9);
            thisCurve.inputLimits[1].max = Numbers(10);
            thisCurve.inputLimits[2].min = Numbers(11);
            thisCurve.inputLimits[2].max = Numbers(12);
            thisCurve.inputLimits[3].min = Numbers(13);
            thisCurve.inputLimits[3].max = Numbers(14);
            thisCurve.inputLimits[4].min = Numbers(15);
            thisCurve.inputLimits[4].max = Numbers(16);
            if (NumNumbers > 16 && !state.dataIPShortCut->lNumericFieldBlanks(17)) {
                thisCurve.outputLimits.min = Numbers(17);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 17 && !state.dataIPShortCut->lNumericFieldBlanks(18)) {
                thisCurve.outputLimits.max = Numbers(18);
                thisCurve.outputLimits.maxPresent = true;
            }

            constexpr int NumVar = 5;
            std::string VarNames[NumVar] = {"v", "w", "x", "y", "z"};
            for (int i = 1; i <= NumVar; ++i) {
                int MinIndex = 2 * i + 5;
                int MaxIndex = MinIndex + 1;
                if (Numbers(MinIndex) > Numbers(MaxIndex)) { // error
                    ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                    ShowContinueError(state,
                                      format("{} [{:.R2}] > {} [{.R2}]",
                                             state.dataIPShortCut->cNumericFieldNames(MinIndex),
                                             Numbers(MinIndex),
                                             state.dataIPShortCut->cNumericFieldNames(MaxIndex),
                                             Numbers(MaxIndex)));
                    ErrorsFound = true;
                }
                int InputTypeIndex = i + 1;
                if (NumAlphas >= InputTypeIndex) {
                    if (!IsCurveInputTypeValid(Alphas(InputTypeIndex))) {
                        ShowWarningError(
                            state, format("In {} named {} the Input Unit Type for {} is invalid.", CurrentModuleObject, Alphas(1), VarNames[i]));
                    }
                }
            }
            if (NumAlphas >= 7) {
                if (!IsCurveOutputTypeValid(Alphas(7))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over Exponent curves and load data
        CurrentModuleObject = "Curve:Exponent";
        for (int CurveIndex = 1; CurveIndex <= NumExponent; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::Exponent;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 3; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(4);
            thisCurve.inputLimits[0].max = Numbers(5);
            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                thisCurve.outputLimits.min = Numbers(6);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                thisCurve.outputLimits.max = Numbers(7);
                thisCurve.outputLimits.maxPresent = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        }

        // Loop over Fan Pressure Rise curves and load data
        CurrentModuleObject = "Curve:FanPressureRise";
        for (int CurveIndex = 1; CurveIndex <= NumFanPressRise; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::FanPressureRise;
            thisCurve.numDims = 2;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 4; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(5);
            thisCurve.inputLimits[0].max = Numbers(6);
            thisCurve.inputLimits[1].min = Numbers(7);
            thisCurve.inputLimits[1].max = Numbers(8);

            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                thisCurve.outputLimits.min = Numbers(9);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 9 && !state.dataIPShortCut->lNumericFieldBlanks(10)) {
                thisCurve.outputLimits.max = Numbers(10);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5),
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         Numbers(6)));
                ErrorsFound = true;
            }
            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         Numbers(8)));
                ErrorsFound = true;
            }

        } // Fan Pressure Rise

        // Loop over Exponential Skew Normal curves and load data
        CurrentModuleObject = "Curve:ExponentialSkewNormal";
        for (int CurveIndex = 1; CurveIndex <= NumExpSkewNorm; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::ExponentialSkewNormal;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 4; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(5);
            thisCurve.inputLimits[0].max = Numbers(6);

            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                thisCurve.outputLimits.min = Numbers(7);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                thisCurve.outputLimits.max = Numbers(8);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5),
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         Numbers(6)));
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        } // Exponential Skew Normal

        // Loop over Sigmoid curves and load data
        CurrentModuleObject = "Curve:Sigmoid";
        for (int CurveIndex = 1; CurveIndex <= NumSigmoid; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::Sigmoid;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 5; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(6);
            thisCurve.inputLimits[0].max = Numbers(7);

            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                thisCurve.outputLimits.min = Numbers(8);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                thisCurve.outputLimits.max = Numbers(9);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(6) > Numbers(7)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         Numbers(6),
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7)));
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        } // Sigmoid

        // Loop over Rectangular Hyperbola Type 1 curves and load data
        CurrentModuleObject = "Curve:RectangularHyperbola1";
        for (int CurveIndex = 1; CurveIndex <= NumRectHyper1; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::RectangularHyperbola1;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 3; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(4);
            thisCurve.inputLimits[0].max = Numbers(5);

            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                thisCurve.outputLimits.min = Numbers(6);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                thisCurve.outputLimits.max = Numbers(7);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(4),
                                         Numbers(4),
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5)));
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        } // Rectangular Hyperbola Type 1

        // Loop over Rectangular Hyperbola Type 2 curves and load data
        CurrentModuleObject = "Curve:RectangularHyperbola2";
        for (int CurveIndex = 1; CurveIndex <= NumRectHyper2; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::RectangularHyperbola2;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 3; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(4);
            thisCurve.inputLimits[0].max = Numbers(5);

            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                thisCurve.outputLimits.min = Numbers(6);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                thisCurve.outputLimits.max = Numbers(7);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(4),
                                         Numbers(4),
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5)));
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        } // Rectangular Hyperbola Type 2

        // Loop over Exponential Decay curves and load data
        CurrentModuleObject = "Curve:ExponentialDecay";
        for (int CurveIndex = 1; CurveIndex <= NumExpDecay; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::ExponentialDecay;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 3; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(4);
            thisCurve.inputLimits[0].max = Numbers(5);

            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                thisCurve.outputLimits.min = Numbers(6);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                thisCurve.outputLimits.max = Numbers(7);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(4),
                                         Numbers(4),
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5)));
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        } // Exponential Decay

        // ykt July,2011 Loop over DoubleExponential Decay curves and load data
        CurrentModuleObject = "Curve:DoubleExponentialDecay";
        for (int CurveIndex = 1; CurveIndex <= NumDoubleExpDecay; ++CurveIndex) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     CurveIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataCurveManager->UniqueCurveNames,
                                                     Alphas(1),
                                                     CurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
            ++CurveNum;
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

            thisCurve.Name = Alphas(1);
            thisCurve.curveType = CurveType::DoubleExponentialDecay;
            thisCurve.numDims = 1;
            thisCurve.interpolationType = InterpType::EvaluateCurveToLimits;
            for (int in = 0; in < 5; ++in) {
                thisCurve.coeff[in] = Numbers(in + 1);
            }
            thisCurve.inputLimits[0].min = Numbers(6);
            thisCurve.inputLimits[0].max = Numbers(7);

            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                thisCurve.outputLimits.min = Numbers(8);
                thisCurve.outputLimits.minPresent = true;
            }
            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                thisCurve.outputLimits.max = Numbers(9);
                thisCurve.outputLimits.maxPresent = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, format("In {} named {} the Input Unit Type for X is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, format("In {} named {} the Output Unit Type is invalid.", CurrentModuleObject, Alphas(1)));
                }
            }
        } // Exponential Decay

        // Loop over wind pressure coefficient tables and load data
        if (NumWPCValTab > 0) {
            // Get the angle values
            CurrentModuleObject = "AirflowNetwork:MultiZone:WindPressureCoefficientArray";
            int numOfCPArray = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

            if (numOfCPArray != 1) {
                ShowSevereError(state,
                                "GetCurveInput: Currently exactly one (\"1\") " + CurrentModuleObject +
                                    " object per simulation is required when using the AirflowNetwork model.");
                ErrorsFound = true;
            } else if (numOfCPArray == 1) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         1,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         _,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                std::string wpcName = Alphas(1); // Name of CP array
                int numWindDir = NumNumbers;
                std::vector<Real64> windDirs(numWindDir);

                Real64 dirMin = 0;
                Real64 dirMax = 0;
                for (int j = 1; j <= NumNumbers; ++j) { // Wind direction
                    windDirs[j - 1] = Numbers(j);
                    dirMin = std::min(dirMin, Numbers(j));
                    dirMax = std::max(dirMax, Numbers(j));
                    if (j > 1) {
                        if (windDirs[j - 2] >= windDirs[j - 1]) {
                            ShowSevereError(state, format("GetCurveInput: An {} object ", CurrentModuleObject));
                            ShowContinueError(state,
                                              "has either the same values for two consecutive wind directions, or a lower wind direction value after "
                                              "a higher wind direction value.");
                            ShowContinueError(state, "Wind direction values must be entered in ascending order.");
                            ShowContinueError(state,
                                              format("{} = {:.2R} {} = {:.2R}",
                                                     state.dataIPShortCut->cNumericFieldNames(j),
                                                     windDirs[j - 2],
                                                     state.dataIPShortCut->cNumericFieldNames[j + 1],
                                                     windDirs[j - 1]));
                            ErrorsFound = true;
                        }
                    }
                }
                // Check that the first table value is zero
                if (dirMin != 0.0) {
                    ShowSevereError(state, format("GetCurveInput: An {} object ", CurrentModuleObject));
                    ShowContinueError(state, format("has a nonzero minimum value of {:.2R}", dirMin));
                    ShowContinueError(state, "Wind direction values must begin at zero.");
                    ErrorsFound = true;
                }

                // Now that we have the directions, we can read the tables themselves
                CurrentModuleObject = "AirflowNetwork:MultiZone:WindPressureCoefficientValues";
                for (int index = 1; index <= NumWPCValTab; ++index) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             CurrentModuleObject,
                                                                             index,
                                                                             Alphas,
                                                                             NumAlphas,
                                                                             Numbers,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             state.dataIPShortCut->lNumericFieldBlanks,
                                                                             _,
                                                                             state.dataIPShortCut->cAlphaFieldNames,
                                                                             state.dataIPShortCut->cNumericFieldNames);
                    ++CurveNum;
                    GlobalNames::VerifyUniqueInterObjectName(state,
                                                             state.dataCurveManager->UniqueCurveNames,
                                                             Alphas(1),
                                                             CurrentModuleObject,
                                                             state.dataIPShortCut->cAlphaFieldNames(1),
                                                             ErrorsFound);

                    // Ensure the CP array name should be the same as the name of AirflowNetwork:MultiZone:WindPressureCoefficientArray
                    if (!UtilityRoutines::SameString(Alphas(2), wpcName)) {
                        ShowSevereError(state,
                                        format("GetCurveInput: Invalid {} = {} in {} = ",
                                               state.dataIPShortCut->cAlphaFieldNames(2),
                                               Alphas(2),
                                               CurrentModuleObject));
                        ShowContinueError(state, "The valid name is " + wpcName);
                        ErrorsFound = true;
                    }

                    auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

                    thisCurve.Name = Alphas(1);
                    thisCurve.numDims = 1;

                    thisCurve.interpolationType = InterpType::BtwxtMethod;

                    std::string contextString = format("{} \"{}\"", CurrentModuleObject, Alphas(1));
                    std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
                    Btwxt::setMessageCallback(Curve::BtwxtMessageCallback, &callbackPair);

                    thisCurve.inputLimits[0].min = 0.0;
                    thisCurve.inputLimits[0].minPresent = true;
                    thisCurve.inputLimits[0].max = 360.0;
                    thisCurve.inputLimits[0].maxPresent = true;

                    thisCurve.outputLimits.min = -1.0;
                    thisCurve.outputLimits.minPresent = true;
                    thisCurve.outputLimits.max = 1.0;
                    thisCurve.outputLimits.maxPresent = true;

                    MaxTableNums = NumNumbers;
                    if (NumNumbers != numWindDir) {
                        ShowSevereError(state, format("GetCurveInput: For {}: ", CurrentModuleObject));
                        ShowContinueError(state,
                                          format("The number of data entries must match the number of wind directions given in the wind pressure "
                                                 "coefficient array. Number of data entries = {}",
                                                 NumNumbers));
                        ErrorsFound = true;
                    } else {
                        std::vector<double> axis;
                        std::vector<double> lookupValues;

                        for (int TableDataIndex = 1; TableDataIndex <= MaxTableNums; ++TableDataIndex) {
                            axis.push_back(windDirs[TableDataIndex - 1]);
                            lookupValues.push_back(Numbers(TableDataIndex));
                        }
                        if (axis[axis.size() - 1] < 360.0) {
                            axis.push_back(360.0);
                            lookupValues.push_back(Numbers(1));
                        }

                        std::vector<Btwxt::GridAxis> gridAxes;
                        gridAxes.emplace_back(axis, Btwxt::Method::LINEAR, Btwxt::Method::LINEAR, std::pair<double, double>{0.0, 360.0});

                        auto gridIndex = state.dataCurveManager->btwxtManager.addGrid(Alphas(1), Btwxt::GriddedData(gridAxes));
                        thisCurve.TableIndex = gridIndex;
                        thisCurve.GridValueIndex = state.dataCurveManager->btwxtManager.addOutputValues(gridIndex, lookupValues);
                    }
                }
            }
        }

        // Create case insensitive references to independent variable input data
        int numIndVars = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Table:IndependentVariable");
        if (numIndVars > 0) {
            // Set Btwxt Message Callback
            auto const &indVarInstances = state.dataInputProcessing->inputProcessor->getObjectInstances("Table:IndependentVariable");
            for (auto &instance : indVarInstances.items()) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed("Table:IndependentVariable", thisObjectName);
                state.dataCurveManager->btwxtManager.independentVarRefs.emplace(UtilityRoutines::MakeUPPERCase(thisObjectName), fields);
            }
        }

        // Create GridSpaces from Independent Variable List
        int numIndVarLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Table:IndependentVariableList");
        std::map<std::string, std::vector<std::pair<double, double>>>
            varListLimits; // ugly, but this is needed for legacy behavior (otherwise limits are reset by Btwxt if they are within bounds).
        std::map<std::string, std::vector<double>> varListNormalizeTargets;
        if (numIndVarLists > 0) {
            auto const indVarListInstances = state.dataInputProcessing->inputProcessor->getObjectInstances("Table:IndependentVariableList");
            for (auto &instance : indVarListInstances.items()) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed("Table:IndependentVariableList", thisObjectName);
                std::string varListName = UtilityRoutines::MakeUPPERCase(thisObjectName);

                std::vector<Btwxt::GridAxis> gridAxes;

                // Loop through independent variables in list and add them to the grid
                for (auto indVar : fields.at("independent_variables")) {
                    std::string indVarName = UtilityRoutines::MakeUPPERCase(indVar.at("independent_variable_name").get<std::string>());
                    std::string contextString = format("Table:IndependentVariable \"{}\"", indVarName);
                    std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
                    Btwxt::setMessageCallback(Curve::BtwxtMessageCallback, &callbackPair);

                    // Find independent variable input data
                    if (state.dataCurveManager->btwxtManager.independentVarRefs.count(indVarName)) {
                        // If found, read data
                        auto const &indVarInstance = state.dataCurveManager->btwxtManager.independentVarRefs.at(indVarName);

                        // TODO: Actually use this to define output variable units
                        if (indVarInstance.count("unit_type")) {
                            std::string unitType = indVarInstance.at("unit_type").get<std::string>();
                            if (!IsCurveOutputTypeValid(unitType)) {
                                ShowSevereError(state, format("{}: Unit Type [{}] is invalid", contextString, unitType));
                            }
                        }

                        std::vector<double> axis;

                        if (indVarInstance.count("external_file_name")) {
                            std::string tmp = indVarInstance.at("external_file_name").get<std::string>();
                            fs::path filePath(tmp);
                            if (!indVarInstance.count("external_file_column_number")) {
                                ShowSevereError(state,
                                                format("{}: No column number defined for external file \"{}\"", contextString, filePath.string()));
                                ErrorsFound = true;
                            }
                            if (!indVarInstance.count("external_file_starting_row_number")) {
                                ShowSevereError(
                                    state, format("{}: No starting row number defined for external file \"{}\"", contextString, filePath.string()));
                                ErrorsFound = true;
                            }

                            std::size_t colNum = indVarInstance.at("external_file_column_number").get<std::size_t>() - 1;
                            std::size_t rowNum = indVarInstance.at("external_file_starting_row_number").get<std::size_t>() - 1;

                            if (!state.dataCurveManager->btwxtManager.tableFiles.count(filePath)) {
                                TableFile tableFile;
                                ErrorsFound |= tableFile.load(state, filePath);
                                state.dataCurveManager->btwxtManager.tableFiles.emplace(filePath, tableFile);
                            }

                            if (ErrorsFound) continue; // Unable to load file so continue on to see if there are other errors before fataling

                            axis = state.dataCurveManager->btwxtManager.tableFiles[filePath].getArray(state, {colNum, rowNum});

                            // remove NANs
                            axis.erase(std::remove_if(axis.begin(), axis.end(), [](const double &x) { return std::isnan(x); }), axis.end());

                            // sort
                            std::sort(axis.begin(), axis.end());

                            // remove duplicates
                            axis.erase(std::unique(axis.begin(), axis.end()), axis.end());

                        } else if (indVarInstance.count("values")) {
                            for (auto const &value : indVarInstance.at("values")) {
                                axis.push_back(value.at("value").get<Real64>());
                            }
                        } else {
                            ShowSevereError(state, format("{}: No values defined.", contextString));
                            ErrorsFound = true;
                        }

                        Btwxt::Method interpMethod, extrapMethod;
                        if (indVarInstance.count("interpolation_method")) {
                            interpMethod = Curve::BtwxtManager::interpMethods.at(indVarInstance.at("interpolation_method").get<std::string>());
                        } else {
                            interpMethod = Btwxt::Method::CUBIC;
                        }

                        if (indVarInstance.count("extrapolation_method")) {
                            if (indVarInstance.at("extrapolation_method") == "Unavailable") {
                                ShowSevereError(state, format("{}: Extrapolation method \"Unavailable\" is not yet available.", contextString));
                                ErrorsFound = true;
                            }
                            extrapMethod = Curve::BtwxtManager::extrapMethods.at(indVarInstance.at("extrapolation_method").get<std::string>());
                        } else {
                            extrapMethod = Btwxt::Method::LINEAR;
                        }

                        double min_grid_value = *std::min_element(axis.begin(), axis.end());
                        double max_grid_value = *std::max_element(axis.begin(), axis.end());

                        double min_val, max_val;
                        if (indVarInstance.count("minimum_value")) {
                            min_val = indVarInstance.at("minimum_value").get<Real64>();
                        } else {
                            min_val = min_grid_value;
                        }

                        if (indVarInstance.count("maximum_value")) {
                            max_val = indVarInstance.at("maximum_value").get<Real64>();
                        } else {
                            max_val = max_grid_value;
                        }

                        varListLimits[varListName].emplace_back(min_val, max_val);

                        Real64 normalizationRefValue;
                        if (indVarInstance.count("normalization_reference_value")) {
                            normalizationRefValue = indVarInstance.at("normalization_reference_value").get<Real64>();
                        } else {
                            normalizationRefValue = std::numeric_limits<double>::quiet_NaN();
                        }

                        varListNormalizeTargets[varListName].push_back(normalizationRefValue);

                        // reset limits passed to Btwxt to avoid warnings related to different handling of limits
                        min_val = min(min_val, min_grid_value);
                        max_val = max(max_val, max_grid_value);

                        gridAxes.emplace_back(axis, extrapMethod, interpMethod, std::pair<double, double>{min_val, max_val});

                    } else {
                        // Independent variable does not exist
                        ShowSevereError(state, format("{}: No Table:IndependentVariable found.", contextString));
                        ErrorsFound = true;
                    }
                }
                // Add grid to btwxtManager
                state.dataCurveManager->btwxtManager.addGrid(UtilityRoutines::MakeUPPERCase(thisObjectName), Btwxt::GriddedData(gridAxes));
            }
        }

        int numTblLookups = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Table:Lookup");
        if (numTblLookups > 0) {
            auto const lookupInstances = state.dataInputProcessing->inputProcessor->getObjectInstances("Table:Lookup");
            for (auto &instance : lookupInstances.items()) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed("Table:Lookup", thisObjectName);
                ++CurveNum;
                auto &thisCurve = state.dataCurveManager->PerfCurve(CurveNum);

                thisCurve.Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisCurve.interpolationType = InterpType::BtwxtMethod;

                std::string indVarListName = UtilityRoutines::MakeUPPERCase(fields.at("independent_variable_list_name").get<std::string>());

                std::string contextString = format("Table:Lookup \"{}\"", thisCurve.Name);
                std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
                Btwxt::setMessageCallback(Curve::BtwxtMessageCallback, &callbackPair);

                // TODO: Actually use this to define output variable units
                if (fields.count("output_unit_type")) {
                    std::string unitType = fields.at("output_unit_type").get<std::string>();
                    if (!IsCurveOutputTypeValid(unitType)) {
                        ShowSevereError(state, format("{}: Output Unit Type [{}] is invalid", contextString, unitType));
                    }
                }

                int gridIndex = state.dataCurveManager->btwxtManager.getGridIndex(state, indVarListName, ErrorsFound);
                thisCurve.TableIndex = gridIndex;
                int numDims = state.dataCurveManager->btwxtManager.getNumGridDims(gridIndex);
                thisCurve.numDims = numDims;

                for (int i = 1; i <= std::min(6, numDims); ++i) {
                    double vMin, vMax;
                    std::tie(vMin, vMax) = varListLimits.at(indVarListName)[i - 1];
                    if (i == 1) {
                        thisCurve.inputLimits[0].min = vMin;
                        thisCurve.inputLimits[0].max = vMax;
                    } else if (i == 2) {
                        thisCurve.inputLimits[1].min = vMin;
                        thisCurve.inputLimits[1].max = vMax;
                    } else if (i == 3) {
                        thisCurve.inputLimits[2].min = vMin;
                        thisCurve.inputLimits[2].max = vMax;
                    } else if (i == 4) {
                        thisCurve.inputLimits[3].min = vMin;
                        thisCurve.inputLimits[3].max = vMax;
                    } else if (i == 5) {
                        thisCurve.inputLimits[4].min = vMin;
                        thisCurve.inputLimits[4].max = vMax;
                    } else if (i == 6) {
                        thisCurve.inputLimits[5].min = vMin;
                        thisCurve.inputLimits[5].max = vMax;
                    }
                }

                if (fields.count("minimum_output")) {
                    thisCurve.outputLimits.min = fields.at("minimum_output").get<Real64>();
                    thisCurve.outputLimits.minPresent = true;
                } else {
                    thisCurve.outputLimits.min = -DBL_MAX;
                    thisCurve.outputLimits.minPresent = false;
                }

                if (fields.count("maximum_output")) {
                    thisCurve.outputLimits.max = fields.at("maximum_output").get<Real64>();
                    thisCurve.outputLimits.maxPresent = true;
                } else {
                    thisCurve.outputLimits.max = DBL_MAX;
                    thisCurve.outputLimits.maxPresent = false;
                }

                // Normalize data
                Real64 normalizationDivisor = 1.0;
                enum NormalizationMethod
                {
                    NM_NONE,
                    NM_DIVISOR_ONLY,
                    NM_AUTO_WITH_DIVISOR
                };
                NormalizationMethod normalizeMethod = NM_NONE;
                if (fields.count("normalization_method")) {
                    if (UtilityRoutines::SameString(fields.at("normalization_method").get<std::string>(), "DIVISORONLY")) {
                        normalizeMethod = NM_DIVISOR_ONLY;
                    } else if (UtilityRoutines::SameString(fields.at("normalization_method").get<std::string>(), "AUTOMATICWITHDIVISOR")) {
                        normalizeMethod = NM_AUTO_WITH_DIVISOR;
                    }
                }

                if (normalizeMethod != NM_NONE && fields.count("normalization_divisor")) {
                    normalizationDivisor = fields.at("normalization_divisor").get<Real64>();
                }

                std::vector<double> lookupValues;
                if (fields.count("external_file_name")) {
                    std::string tmp = fields.at("external_file_name").get<std::string>();
                    fs::path filePath(tmp);

                    if (!fields.count("external_file_column_number")) {
                        ShowSevereError(state, format("{}: No column number defined for external file \"{}\"", contextString, filePath.string()));
                        ErrorsFound = true;
                    }
                    if (!fields.count("external_file_starting_row_number")) {
                        ShowSevereError(state,
                                        format("{}: No starting row number defined for external file \"{}\"", contextString, filePath.string()));
                        ErrorsFound = true;
                    }

                    std::size_t colNum = fields.at("external_file_column_number").get<std::size_t>() - 1;
                    std::size_t rowNum = fields.at("external_file_starting_row_number").get<std::size_t>() - 1;

                    if (!state.dataCurveManager->btwxtManager.tableFiles.count(filePath)) {
                        TableFile tableFile;
                        ErrorsFound |= tableFile.load(state, filePath);
                        state.dataCurveManager->btwxtManager.tableFiles.emplace(filePath, tableFile);
                    }

                    if (ErrorsFound) continue; // Unable to load file so continue on to see if there are other errors before fataling

                    lookupValues = state.dataCurveManager->btwxtManager.tableFiles[filePath].getArray(state, {colNum, rowNum});

                    // remove NANs
                    lookupValues.erase(std::remove_if(lookupValues.begin(), lookupValues.end(), [](const double &x) { return std::isnan(x); }),
                                       lookupValues.end());

                } else if (fields.count("values")) {
                    for (auto value : fields.at("values")) {
                        lookupValues.push_back(value.at("output_value").get<Real64>() / normalizationDivisor);
                    }
                } else {
                    ShowSevereError(state, format("{}: No values defined.", contextString));
                    ErrorsFound = true;
                }

                thisCurve.GridValueIndex = state.dataCurveManager->btwxtManager.addOutputValues(gridIndex, lookupValues);

                if (normalizeMethod == NM_AUTO_WITH_DIVISOR) {
                    auto const normalizeTarget = varListNormalizeTargets.at(indVarListName);

                    bool pointsSpecified = false;
                    bool pointsUnspecified = false;
                    for (auto value : normalizeTarget) {
                        if (std::isnan(value)) {
                            pointsUnspecified = true;
                        } else {
                            pointsSpecified = true;
                        }
                    }
                    if (pointsSpecified && pointsUnspecified) {
                        ShowSevereError(state,
                                        format("{}: Table is to be normalized using AutomaticWithDivisor, but not all independent variables define a "
                                               "normalization reference value. Make sure either:",
                                               contextString));
                        ShowContinueError(state, "  Make sure either:");
                        ShowContinueError(state, "    a) a normalization reference value is defined for each independent variable, or");
                        ShowContinueError(state, "    b) no normalization reference values are defined.");
                        ErrorsFound = true;
                    } else if (pointsSpecified) {
                        // normalizeGridValues normalizes curve values to 1.0 at the normalization target, and returns the scalar needed to perform
                        // this normalization. The result is multiplied by the input normalizationDivisor again for the AutomaticWithDivisor case, in
                        // which normalizeGridValues returns a compound scalar.
                        normalizationDivisor = state.dataCurveManager->btwxtManager.normalizeGridValues(
                                                   gridIndex, thisCurve.GridValueIndex, normalizeTarget, normalizationDivisor) *
                                               normalizationDivisor;
                    }
                }

                if ((normalizeMethod == NM_DIVISOR_ONLY) || (normalizeMethod == NM_AUTO_WITH_DIVISOR)) {
                    if (thisCurve.outputLimits.maxPresent) {
                        thisCurve.outputLimits.max = thisCurve.outputLimits.max / normalizationDivisor;
                    }
                    if (thisCurve.outputLimits.minPresent) {
                        thisCurve.outputLimits.min = thisCurve.outputLimits.min / normalizationDivisor;
                    }
                }
            }
        }
        state.dataCurveManager->btwxtManager.tableFiles.clear();
    }

    int BtwxtManager::getGridIndex(EnergyPlusData &state, std::string &indVarListName, bool &ErrorsFound)
    {
        int gridIndex = -1;
        if (gridMap.count(indVarListName)) {
            gridIndex = gridMap.at(indVarListName);
        } else {
            // Independent variable list does not exist
            ShowSevereError(state, format("Table:Lookup \"{}\" : No Table:IndependentVariableList found.", indVarListName));
            ErrorsFound = true;
        }
        return gridIndex;
    }

    int BtwxtManager::addOutputValues(int gridIndex, std::vector<double> values)
    {
        return (int)grids[gridIndex].add_value_table(values);
    }

    int BtwxtManager::getNumGridDims(int gridIndex)
    {
        return (int)grids[gridIndex].get_ndims();
    }

    double BtwxtManager::getGridValue(int gridIndex, int outputIndex, const std::vector<double> &target)
    {
        return grids[gridIndex](target)[outputIndex];
    }

    double BtwxtManager::normalizeGridValues(int gridIndex, int outputIndex, const std::vector<double> &target, const double scalar)
    {
        return grids[gridIndex].normalize_values_at_target(outputIndex, target, scalar);
    }

    void BtwxtManager::clear()
    {
        grids.clear();
        gridMap.clear();
        independentVarRefs.clear();
        tableFiles.clear();
    }

    bool TableFile::load(EnergyPlusData &state, fs::path const &path)
    {
        this->filePath = path;
        std::string contextString = "CurveManager::TableFile::load: ";
        fs::path fullPath = DataSystemVariables::CheckForActualFilePath(state, path, contextString);
        if (fullPath.empty()) {
            // Note: we return 'ErrorsFound' apparently
            return true;
        }
        std::ifstream file(fullPath);
        std::string line;
        numRows = 0;
        numColumns = 0;
        while (getline(file, line)) {
            ++numRows;
            std::size_t pos(0);
            std::size_t colNum(1);
            while ((pos = line.find(',')) != std::string::npos) {
                if (colNum > numColumns) {
                    numColumns = colNum;
                    contents.resize(numColumns);
                }
                contents[colNum - 1].push_back(line.substr(0, pos));
                line.erase(0, pos + 1);
                ++colNum;
            }
            // Anything after the last comma
            if (!line.empty()) {
                if (colNum > numColumns) {
                    numColumns = colNum;
                    contents.resize(numColumns);
                }
                contents[colNum - 1].push_back(line);
                ++colNum;
            }
            // flesh out columns if row ends early
            while (colNum <= numColumns) {
                contents[colNum - 1].emplace_back("");
                ++colNum;
            }
        }
        return false;
    }

    std::vector<double> &TableFile::getArray(EnergyPlusData &state, std::pair<std::size_t, std::size_t> colAndRow)
    {
        if (!arrays.count(colAndRow)) {
            // create the column from the data if it doesn't exist already
            std::size_t col = colAndRow.first;  // 0 indexed
            std::size_t row = colAndRow.second; // 0 indexed
            auto &content = contents[col];
            if (col >= numColumns) {
                ShowFatalError(
                    state, format("File \"{}\" : Requested column ({}) exceeds the number of columns ({}).", filePath.string(), col + 1, numColumns));
            }
            if (row >= numRows) {
                ShowFatalError(
                    state, format("File \"{}\" : Requested starting row ({}) exceeds the number of rows ({}).", filePath.string(), row + 1, numRows));
            }
            std::vector<double> array(numRows - row);
            std::transform(content.begin() + row, content.end(), array.begin(), [](std::string_view str) {
                // Convert strings to double
                auto first_char = str.find_first_not_of(' ');
                if (first_char != std::string_view::npos) {
                    str.remove_prefix(first_char);
                }
                double result = 0;
                auto answer = fast_float::from_chars(str.data(), str.data() + str.size(), result);
                if (answer.ec != std::errc()) {
                    return std::numeric_limits<double>::quiet_NaN();
                }
                return result;
            });
            arrays[colAndRow] = array;
        }
        return arrays.at(colAndRow);
    }

    void InitCurveReporting(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Setting up of curve output variables caused errors in some files. Thus, separating the setup
        // from the getinput.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        for (auto &thisCurve : state.dataCurveManager->PerfCurve) {
            for (int dim = 1; dim <= thisCurve.numDims; ++dim) {
                std::string numStr = fmt::to_string(dim);
                SetupOutputVariable(state,
                                    format("Performance Curve Input Variable {} Value", numStr),
                                    OutputProcessor::Unit::None,
                                    thisCurve.inputs[dim - 1],
                                    OutputProcessor::SOVTimeStepType::HVAC,
                                    OutputProcessor::SOVStoreType::Average,
                                    thisCurve.Name);
            }
            // set the output up last so it shows up after the input in the csv file
            SetupOutputVariable(state,
                                "Performance Curve Output Value",
                                OutputProcessor::Unit::None,
                                thisCurve.output,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Average,
                                thisCurve.Name);
        }

        for (auto &thisPressCurve : state.dataBranchAirLoopPlant->PressureCurve) {
            SetupOutputVariable(state,
                                "Performance Curve Input Variable 1 Value",
                                OutputProcessor::Unit::None,
                                thisPressCurve.CurveInput1,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Average,
                                thisPressCurve.Name);
            SetupOutputVariable(state,
                                "Performance Curve Input Variable 2 Value",
                                OutputProcessor::Unit::None,
                                thisPressCurve.CurveInput2,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Average,
                                thisPressCurve.Name);
            SetupOutputVariable(state,
                                "Performance Curve Input Variable 3 Value",
                                OutputProcessor::Unit::None,
                                thisPressCurve.CurveInput3,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Average,
                                thisPressCurve.Name);
            SetupOutputVariable(state,
                                "Performance Curve Output Value",
                                OutputProcessor::Unit::None,
                                thisPressCurve.CurveOutput,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Average,
                                thisPressCurve.Name);
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) { // provide hook for possible EMS control
            for (auto &thisCurve : state.dataCurveManager->PerfCurve) {
                SetupEMSActuator(
                    state, "Curve", thisCurve.Name, "Curve Result", "[unknown]", thisCurve.EMSOverrideOn, thisCurve.EMSOverrideCurveValue);
            } // All performance curves
        }
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) { // provide hook for possible EMS control
            for (auto &thisPressCurve : state.dataBranchAirLoopPlant->PressureCurve) {
                SetupEMSActuator(state,
                                 "Curve",
                                 thisPressCurve.Name,
                                 "Curve Result",
                                 "[unknown]",
                                 thisPressCurve.EMSOverrideOn,
                                 thisPressCurve.EMSOverrideCurveValue);
            } // All pressure curves
        }
    }

    Real64 PerformanceCurveObject(EnergyPlusData &state,
                                  int const CurveIndex,        // index of curve in curve array
                                  Real64 const Var1,           // 1st independent variable
                                  Optional<Real64 const> Var2, // 2nd independent variable
                                  Optional<Real64 const> Var3, // 3rd independent variable
                                  Optional<Real64 const> Var4, // 4th independent variable
                                  Optional<Real64 const> Var5  // 5th independent variable
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Lixing Gu, July 2006; B. Griffith July 2006
        //                      22Aug2010 Craig Wray, added new curves for fan component model:
        //                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
        //                          RectangularHyperbola2, ExponentialDecay

        //       RE-ENGINEERED  Autodesk: Performance tuning

        // PURPOSE OF THIS FUNCTION:
        // Given the curve index and the values of 1 or 2 independent variables,
        // returns the value of an equipment performance curve.

        // Return value
        Real64 CurveValue;

        static Real64 const sqrt_2_inv(1.0 / std::sqrt(2.0)); // would be constexpr-able if std::sqrt was constexpr, but not yet

        auto const &curve(state.dataCurveManager->PerfCurve(CurveIndex));

        Real64 const V1(max(min(Var1, curve.inputLimits[0].max), curve.inputLimits[0].min)); // 1st independent variable after limits imposed
        Real64 const V2(Var2.present() ? max(min(Var2, curve.inputLimits[1].max), curve.inputLimits[1].min)
                                       : 0.0); // 2nd independent variable after limits imposed
        Real64 const V3(Var3.present() ? max(min(Var3, curve.inputLimits[2].max), curve.inputLimits[2].min)
                                       : 0.0); // 3rd independent variable after limits imposed
        Real64 const V4(Var4.present() ? max(min(Var4, curve.inputLimits[3].max), curve.inputLimits[3].min)
                                       : 0.0); // 4th independent variable after limits imposed
        Real64 const V5(Var5.present() ? max(min(Var5, curve.inputLimits[4].max), curve.inputLimits[4].min)
                                       : 0.0); // 5th independent variable after limits imposed

        switch (curve.curveType) {
        case CurveType::Linear: {
            CurveValue = curve.coeff[0] + V1 * curve.coeff[1];
        } break;
        case CurveType::Quadratic: {
            CurveValue = curve.coeff[0] + V1 * (curve.coeff[1] + V1 * curve.coeff[2]);
        } break;
        case CurveType::QuadLinear: {
            CurveValue = curve.coeff[0] + V1 * curve.coeff[1] + V2 * curve.coeff[2] + V3 * curve.coeff[3] + V4 * curve.coeff[4];
        } break;
        case CurveType::QuintLinear: {
            CurveValue = curve.coeff[0] + V1 * curve.coeff[1] + V2 * curve.coeff[2] + V3 * curve.coeff[3] + V4 * curve.coeff[4] + V5 * curve.coeff[5];
        } break;
        case CurveType::Cubic: {
            CurveValue = curve.coeff[0] + V1 * (curve.coeff[1] + V1 * (curve.coeff[2] + V1 * curve.coeff[3]));
        } break;
        case CurveType::Quartic: {
            CurveValue = curve.coeff[0] + V1 * (curve.coeff[1] + V1 * (curve.coeff[2] + V1 * (curve.coeff[3] + V1 * curve.coeff[4])));
        } break;
        case CurveType::BiQuadratic: {
            CurveValue =
                curve.coeff[0] + V1 * (curve.coeff[1] + V1 * curve.coeff[2]) + V2 * (curve.coeff[3] + V2 * curve.coeff[4]) + V1 * V2 * curve.coeff[5];
        } break;
        case CurveType::QuadraticLinear: {
            CurveValue =
                (curve.coeff[0] + V1 * (curve.coeff[1] + V1 * curve.coeff[2])) + (curve.coeff[3] + V1 * (curve.coeff[4] + V1 * curve.coeff[5])) * V2;
        } break;
        case CurveType::CubicLinear: {
            CurveValue =
                (curve.coeff[0] + V1 * (curve.coeff[1] + V1 * (curve.coeff[2] + V1 * curve.coeff[3]))) + (curve.coeff[4] + V1 * curve.coeff[5]) * V2;
        } break;
        case CurveType::BiCubic: {
            CurveValue = curve.coeff[0] + V1 * curve.coeff[1] + V1 * V1 * curve.coeff[2] + V2 * curve.coeff[3] + V2 * V2 * curve.coeff[4] +
                         V1 * V2 * curve.coeff[5] + V1 * V1 * V1 * curve.coeff[6] + V2 * V2 * V2 * curve.coeff[7] + V1 * V1 * V2 * curve.coeff[8] +
                         V1 * V2 * V2 * curve.coeff[9];
        } break;
        case CurveType::ChillerPartLoadWithLift: {
            CurveValue = curve.coeff[0] + curve.coeff[1] * V1 + curve.coeff[2] * V1 * V1 + curve.coeff[3] * V2 + curve.coeff[4] * V2 * V2 +
                         curve.coeff[5] * V1 * V2 + curve.coeff[6] * V1 * V1 * V1 + curve.coeff[7] * V2 * V2 * V2 + curve.coeff[8] * V1 * V1 * V2 +
                         curve.coeff[9] * V1 * V2 * V2 + curve.coeff[10] * V1 * V1 * V2 * V2 + curve.coeff[11] * V3 * V2 * V2 * V2;
        } break;
        case CurveType::TriQuadratic: {
            auto const &Tri2ndOrder(curve.tri2ndOrder);
            auto const V1s(V1 * V1);
            auto const V2s(V2 * V2);
            auto const V3s(V3 * V3);
            CurveValue = Tri2ndOrder[0] + Tri2ndOrder[1] * V1s + Tri2ndOrder[2] * V1 + Tri2ndOrder[3] * V2s + Tri2ndOrder[4] * V2 +
                         Tri2ndOrder[5] * V3s + Tri2ndOrder[6] * V3 + Tri2ndOrder[7] * V1s * V2s + Tri2ndOrder[8] * V1 * V2 +
                         Tri2ndOrder[9] * V1 * V2s + Tri2ndOrder[10] * V1s * V2 + Tri2ndOrder[11] * V1s * V3s + Tri2ndOrder[12] * V1 * V3 +
                         Tri2ndOrder[13] * V1 * V3s + Tri2ndOrder[14] * V1s * V3 + Tri2ndOrder[15] * V2s * V3s + Tri2ndOrder[16] * V2 * V3 +
                         Tri2ndOrder[17] * V2 * V3s + Tri2ndOrder[18] * V2s * V3 + Tri2ndOrder[19] * V1s * V2s * V3s +
                         Tri2ndOrder[20] * V1s * V2s * V3 + Tri2ndOrder[21] * V1s * V2 * V3s + Tri2ndOrder[22] * V1 * V2s * V3s +
                         Tri2ndOrder[23] * V1s * V2 * V3 + Tri2ndOrder[24] * V1 * V2s * V3 + Tri2ndOrder[25] * V1 * V2 * V3s +
                         Tri2ndOrder[26] * V1 * V2 * V3;
        } break;
        case CurveType::Exponent: {
            CurveValue = curve.coeff[0] + curve.coeff[1] * std::pow(V1, curve.coeff[2]);
        } break;
        case CurveType::FanPressureRise: {
            CurveValue = V1 * (curve.coeff[0] * V1 + curve.coeff[1] + curve.coeff[2] * std::sqrt(V2)) + curve.coeff[3] * V2;
        } break;
        case CurveType::ExponentialSkewNormal: {
            Real64 CoeffZ1 = (V1 - curve.coeff[0]) / curve.coeff[1];
            Real64 CoeffZ2 = (curve.coeff[3] * V1 * std::exp(curve.coeff[2] * V1) - curve.coeff[0]) / curve.coeff[1];
            Real64 CoeffZ3 = -curve.coeff[0] / curve.coeff[1];
            Real64 CurveValueNumer = std::exp(-0.5 * (CoeffZ1 * CoeffZ1)) * (1.0 + sign(1.0, CoeffZ2) * std::erf(std::abs(CoeffZ2) * sqrt_2_inv));
            Real64 CurveValueDenom = std::exp(-0.5 * (CoeffZ3 * CoeffZ3)) * (1.0 + sign(1.0, CoeffZ3) * std::erf(std::abs(CoeffZ3) * sqrt_2_inv));
            CurveValue = CurveValueNumer / CurveValueDenom;
        } break;
        case CurveType::Sigmoid: {
            Real64 CurveValueExp = std::exp((curve.coeff[2] - V1) / curve.coeff[3]);
            CurveValue = curve.coeff[0] + curve.coeff[1] / std::pow(1.0 + CurveValueExp, curve.coeff[4]);
        } break;
        case CurveType::RectangularHyperbola1: {
            Real64 CurveValueNumer = curve.coeff[0] * V1;
            Real64 CurveValueDenom = curve.coeff[1] + V1;
            CurveValue = (CurveValueNumer / CurveValueDenom) + curve.coeff[2];
        } break;
        case CurveType::RectangularHyperbola2: {
            Real64 CurveValueNumer = curve.coeff[0] * V1;
            Real64 CurveValueDenom = curve.coeff[1] + V1;
            CurveValue = (CurveValueNumer / CurveValueDenom) + (curve.coeff[2] * V1);
        } break;
        case CurveType::ExponentialDecay: {
            CurveValue = curve.coeff[0] + curve.coeff[1] * std::exp(curve.coeff[2] * V1);
        } break;
        case CurveType::DoubleExponentialDecay: {
            CurveValue = curve.coeff[0] + curve.coeff[1] * std::exp(curve.coeff[2] * V1) + curve.coeff[3] * std::exp(curve.coeff[4] * V1);
        } break;
        default: {
            CurveValue = 0.0;
        } break;
        }

        if (curve.outputLimits.minPresent) CurveValue = max(CurveValue, curve.outputLimits.min);
        if (curve.outputLimits.maxPresent) CurveValue = min(CurveValue, curve.outputLimits.max);

        return CurveValue;
    }

    Real64 BtwxtTableInterpolation(EnergyPlusData &state,
                                   int const CurveIndex,        // index of curve in curve array
                                   Real64 const Var1,           // 1st independent variable
                                   Optional<Real64 const> Var2, // 2nd independent variable
                                   Optional<Real64 const> Var3, // 3rd independent variable
                                   Optional<Real64 const> Var4, // 4th independent variable
                                   Optional<Real64 const> Var5, // 5th independent variable
                                   Optional<Real64 const> Var6  // 6th independent variable
    )
    {
        auto &thisCurve = state.dataCurveManager->PerfCurve(CurveIndex);
        // TODO: Generalize for N-dims
        Real64 var = Var1;
        var = max(min(var, thisCurve.inputLimits[0].max), thisCurve.inputLimits[0].min);
        std::vector<double> target{var};
        if (present(Var2)) {
            var = Var2;
            var = max(min(var, thisCurve.inputLimits[1].max), thisCurve.inputLimits[1].min);
            target.push_back(var);
        }
        if (present(Var3)) {
            var = Var3;
            var = max(min(var, thisCurve.inputLimits[2].max), thisCurve.inputLimits[2].min);
            target.push_back(var);
        }
        if (present(Var4)) {
            var = Var4;
            var = max(min(var, thisCurve.inputLimits[3].max), thisCurve.inputLimits[3].min);
            target.push_back(var);
        }
        if (present(Var5)) {
            var = Var5;
            var = max(min(var, thisCurve.inputLimits[4].max), thisCurve.inputLimits[4].min);
            target.push_back(var);
        }
        if (present(Var6)) {
            var = Var6;
            var = max(min(var, thisCurve.inputLimits[5].max), thisCurve.inputLimits[5].min);
            target.push_back(var);
        }

        std::string contextString = format("Table:Lookup \"{}\"", thisCurve.Name);
        std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
        Btwxt::setMessageCallback(Curve::BtwxtMessageCallback, &callbackPair);
        Real64 TableValue = state.dataCurveManager->btwxtManager.getGridValue(thisCurve.TableIndex, thisCurve.GridValueIndex, target);

        if (thisCurve.outputLimits.minPresent) TableValue = max(TableValue, thisCurve.outputLimits.min);
        if (thisCurve.outputLimits.maxPresent) TableValue = min(TableValue, thisCurve.outputLimits.max);

        return TableValue;
    }

    bool IsCurveInputTypeValid(std::string const &InInputType) // index of curve in curve array
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   Oct 2009
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Returns true if the input unit type is valid

        // TODO: Enum and constexpr std::array lookup
        if (len(InInputType) > 0) {
            if (UtilityRoutines::SameString(InInputType, "DIMENSIONLESS")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "TEMPERATURE")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "PRESSURE")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "VOLUMETRICFLOW")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "MASSFLOW")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "POWER")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "DISTANCE")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "WAVELENGTH")) {
                return true;
            } else if (UtilityRoutines::SameString(InInputType, "ANGLE")) {
                return true;
            } else {
                return false;
            }
        } else {
            return true; // if not used it is valid
        }
    }

    bool IsCurveOutputTypeValid(std::string const &InOutputType) // index of curve in curve array
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   Oct 2009
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Returns true if the output unit type is valid

        // TODO: Enum and constexpr std::array lookup
        if (UtilityRoutines::SameString(InOutputType, "DIMENSIONLESS")) {
            return true;
        } else if (UtilityRoutines::SameString(InOutputType, "PRESSURE")) {
            return true;
        } else if (UtilityRoutines::SameString(InOutputType, "TEMPERATURE")) {
            return true;
        } else if (UtilityRoutines::SameString(InOutputType, "CAPACITY")) {
            return true;
        } else if (UtilityRoutines::SameString(InOutputType, "POWER")) {
            return true;
        } else {
            return false;
        }
    }

    bool CheckCurveDims(EnergyPlusData &state,
                        int const CurveIndex,
                        std::vector<int> validDims,
                        const std::string_view routineName,
                        std::string_view objectType,
                        std::string_view objectName,
                        std::string_view curveFieldText)
    {
        // Returns true if errors found
        auto &thisCurve = state.dataCurveManager->PerfCurve(CurveIndex);
        int curveDim = thisCurve.numDims;
        if (std::find(validDims.begin(), validDims.end(), curveDim) != validDims.end()) {
            // Compatible
            return false;
        } else {
            // Not compatible
            ShowSevereError(state, fmt::format("{}{}=\"{}\"", routineName, objectType, objectName));
            ShowContinueError(state, format("...Invalid curve for {}.", curveFieldText));
            std::string validString = fmt::to_string(validDims[0]);
            for (std::size_t i = 1; i < validDims.size(); i++) {
                validString += format(" or {}", validDims[i]);
            }
            std::string plural1 = curveDim > 1 ? "s" : "";
            std::string plural2 = validDims[validDims.size() - 1] > 1 ? "s" : "";
            ShowContinueError(state, format("...Input curve=\"{}\" has {} dimension{}.", thisCurve.Name, curveDim, plural1));
            ShowContinueError(state, format("...Curve type must have {} dimension{}.", validString, plural2));
            return true;
        }
    }

    std::string GetCurveName(EnergyPlusData &state, int const CurveIndex) // index of curve in curve array
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse
        //       DATE WRITTEN   May 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given a curve index, returns the curve name

        if (CurveIndex > 0) {
            return state.dataCurveManager->PerfCurve(CurveIndex).Name;
        } else {
            return "";
        }
    }

    int GetCurveIndex(EnergyPlusData &state, std::string const &CurveName) // name of the curve
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given a curve name, returns the curve index

        // METHODOLOGY EMPLOYED:
        // uses UtilityRoutines::FindItemInList( to search the curve array for the curve name

        // First time GetCurveIndex is called, get the input for all the performance curves
        if (state.dataCurveManager->GetCurvesInputFlag) {
            GetCurveInput(state);
            GetPressureSystemInput(state);
            state.dataCurveManager->GetCurvesInputFlag = false;
        }

        if (state.dataCurveManager->NumCurves > 0) {
            return UtilityRoutines::FindItemInList(CurveName, state.dataCurveManager->PerfCurve);
        } else {
            return 0;
        }
    }

    // This utility function grabs a curve index and performs the
    // error checking

    int GetCurveCheck(EnergyPlusData &state,
                      std::string const &alph, // curve name
                      bool &errFlag,
                      std::string const &ObjName // parent object of curve
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   March 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides a simple call to both return a curve index as well
        // as check for validity and produce an error message.

        // Return value
        int GetCurveCheckOut;

        GetCurveCheckOut = GetCurveIndex(state, alph); // convert curve name to pointer
        if (GetCurveCheckOut == 0) {
            ShowSevereError(state, format("Curve Not Found for Object=\"{}\" :: {}", ObjName, alph));
            errFlag = true;
        }
        return GetCurveCheckOut;
    }

    void GetCurveMinMaxValues(EnergyPlusData &state,
                              int const CurveIndex,     // index of curve in curve array
                              Real64 &Var1Min,          // Minimum values of 1st independent variable
                              Real64 &Var1Max,          // Maximum values of 1st independent variable
                              Optional<Real64> Var2Min, // Minimum values of 2nd independent variable
                              Optional<Real64> Var2Max, // Maximum values of 2nd independent variable
                              Optional<Real64> Var3Min, // Minimum values of 3rd independent variable
                              Optional<Real64> Var3Max, // Maximum values of 3rd independent variable
                              Optional<Real64> Var4Min, // Minimum values of 4th independent variable
                              Optional<Real64> Var4Max, // Maximum values of 4th independent variable
                              Optional<Real64> Var5Min, // Minimum values of 5th independent variable
                              Optional<Real64> Var5Max, // Maximum values of 5th independent variable
                              Optional<Real64> Var6Min, // Minimum values of 6th independent variable
                              Optional<Real64> Var6Max  // Maximum values of 6th independent variable
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July 2006
        //       MODIFIED       B. Griffith Aug 2006 add third independent variable
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given the curve index, returns the minimum and maximum values specified in the input
        // for the independent variables of the performance curve.

        auto &thisCurve = state.dataCurveManager->PerfCurve(CurveIndex);
        Var1Min = thisCurve.inputLimits[0].min;
        Var1Max = thisCurve.inputLimits[0].max;
        if (present(Var2Min)) Var2Min = thisCurve.inputLimits[1].min;
        if (present(Var2Max)) Var2Max = thisCurve.inputLimits[1].max;
        if (present(Var3Min)) Var3Min = thisCurve.inputLimits[2].min;
        if (present(Var3Max)) Var3Max = thisCurve.inputLimits[2].max;
        if (present(Var4Min)) Var4Min = thisCurve.inputLimits[3].min;
        if (present(Var4Max)) Var4Max = thisCurve.inputLimits[3].max;
        if (present(Var5Min)) Var5Min = thisCurve.inputLimits[4].min;
        if (present(Var5Max)) Var5Max = thisCurve.inputLimits[4].max;
        if (present(Var6Min)) Var6Min = thisCurve.inputLimits[5].min;
        if (present(Var6Max)) Var6Max = thisCurve.inputLimits[5].max;
    }

    void SetCurveOutputMinMaxValues(EnergyPlusData &state,
                                    int const CurveIndex,            // index of curve in curve array
                                    bool &ErrorsFound,               // TRUE when errors occur
                                    Optional<Real64 const> CurveMin, // Minimum value of curve output
                                    Optional<Real64 const> CurveMax  // Maximum values of curve output
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   Feb 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given the curve index, sets the minimum and maximum possible value for this curve.
        // Certain curve types have set limits (e.g., PLF curve should not be greater than 1 or less than 0.7).

        if (CurveIndex > 0 && CurveIndex <= state.dataCurveManager->NumCurves) {
            auto &thisCurve = state.dataCurveManager->PerfCurve(CurveIndex);
            if (present(CurveMin)) {
                thisCurve.outputLimits.min = CurveMin;
                thisCurve.outputLimits.minPresent = true;
            }
            if (present(CurveMax)) {
                thisCurve.outputLimits.max = CurveMax;
                thisCurve.outputLimits.maxPresent = true;
            }
        } else {
            ShowSevereError(
                state,
                format("SetCurveOutputMinMaxValues: CurveIndex=[{}] not in range of curves=[1:{}].", CurveIndex, state.dataCurveManager->NumCurves));
            ErrorsFound = true;
        }
    }

    void GetPressureSystemInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   August 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Currently it just reads the input for pressure curve objects

        // METHODOLOGY EMPLOYED:
        // General EnergyPlus Methodology

        // SUBROUTINE PARAMETER DEFINITIONS:
        std::string_view constexpr CurveObjectName = "Curve:Functional:PressureDrop";

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumPressure;
        Array1D_string Alphas(1);   // Alpha items for object
        Array1D<Real64> Numbers(5); // Numeric items for object
        int NumAlphas;              // Number of Alphas for each GetObjectItem call
        int NumNumbers;             // Number of Numbers for each GetObjectItem call
        int IOStatus;               // Used in GetObjectItem
        bool ErrsFound(false);      // Set to true if errors in input, fatal at end of routine
        int CurveNum;

        NumPressure = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurveObjectName);
        state.dataBranchAirLoopPlant->PressureCurve.allocate(NumPressure);
        for (CurveNum = 1; CurveNum <= NumPressure; ++CurveNum) {
            auto &thisCurve = state.dataBranchAirLoopPlant->PressureCurve(CurveNum);
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurveObjectName,
                                                                     CurveNum,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(
                state, state.dataCurveManager->UniqueCurveNames, Alphas(1), CurveObjectName, state.dataIPShortCut->cAlphaFieldNames(1), ErrsFound);
            thisCurve.Name = Alphas(1);
            thisCurve.EquivDiameter = Numbers(1);
            thisCurve.MinorLossCoeff = Numbers(2);
            thisCurve.EquivLength = Numbers(3);
            thisCurve.EquivRoughness = Numbers(4);
            if (NumNumbers > 4 && !state.dataIPShortCut->lNumericFieldBlanks(5)) {
                if (Numbers(5) != 0.0) {
                    thisCurve.ConstantFPresent = true;
                    thisCurve.ConstantF = Numbers(5);
                }
            }
        }

        if (ErrsFound) {
            ShowFatalError(state, "GetPressureCurveInput: Errors found in Curve Objects.  Preceding condition(s) cause termination.");
        }
    }

    void GetPressureCurveTypeAndIndex(EnergyPlusData &state,
                                      std::string const &PressureCurveName, // name of the curve
                                      DataBranchAirLoopPlant::PressureCurveType &PressureCurveType,
                                      int &PressureCurveIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   August 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Given a curve name, returns the curve type and index

        // METHODOLOGY EMPLOYED:
        // Curve types are:
        //  PressureCurveType::Invalid     = pressure name was given, but curve is not available
        //  PressureCurveType::None        = no pressure curve for this branch
        //  PressureCurveType::Pressure    = pressure curve based on friction/minor loss
        //  PressureCurveType::Generic     = curvemanager held curve which is function of flow rate

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TempCurveIndex;
        CurveType GenericCurveType;

        // If input is not gotten, go ahead and get it now
        if (state.dataCurveManager->GetCurvesInputFlag) {
            GetCurveInput(state);
            GetPressureSystemInput(state);
            state.dataCurveManager->GetCurvesInputFlag = false;
        }

        // Initialize
        PressureCurveType = DataBranchAirLoopPlant::PressureCurveType::None;
        PressureCurveIndex = 0;

        // Try to retrieve a curve manager object
        TempCurveIndex = GetCurveIndex(state, PressureCurveName);

        // See if it is valid
        if (TempCurveIndex > 0) {
            // We have to check the type of curve to make sure it is single independent variable type
            GenericCurveType = state.dataCurveManager->PerfCurve(TempCurveIndex).curveType;
            {
                if (state.dataCurveManager->PerfCurve(TempCurveIndex).numDims == 1) {
                    PressureCurveType = DataBranchAirLoopPlant::PressureCurveType::Generic;
                    PressureCurveIndex = TempCurveIndex;
                } else {
                    ShowSevereError(state, "Plant Pressure Simulation: Found error for curve: " + PressureCurveName);
                    ShowContinueError(state, format("Curve type detected: {}", Curve::objectNames[static_cast<int>(GenericCurveType)]));
                    ShowContinueError(state, "Generic curves should be single independent variable such that DeltaP = f(mdot)");
                    ShowContinueError(state, " Therefore they should be of type: Linear, Quadratic, Cubic, Quartic, or Exponent");
                    ShowFatalError(state, "Errors in pressure simulation input cause program termination");
                }
            }
            return;
        }

        // Then try to retrieve a pressure curve object
        if (allocated(state.dataBranchAirLoopPlant->PressureCurve)) {
            if (size(state.dataBranchAirLoopPlant->PressureCurve) > 0) {
                TempCurveIndex = UtilityRoutines::FindItemInList(PressureCurveName, state.dataBranchAirLoopPlant->PressureCurve);
            } else {
                TempCurveIndex = 0;
            }
        }

        // See if it is valid
        if (TempCurveIndex > 0) {
            PressureCurveType = DataBranchAirLoopPlant::PressureCurveType::Pressure;
            PressureCurveIndex = TempCurveIndex;
            return;
        }

        // If we made it here, we didn't find either type of match

        // Last check, see if it is blank:
        if (PressureCurveName.empty()) {
            PressureCurveType = DataBranchAirLoopPlant::PressureCurveType::None;
            return;
        }

        // At this point, we had a non-blank user entry with no match
        PressureCurveType = DataBranchAirLoopPlant::PressureCurveType::Invalid;
    }

    Real64
    PressureCurveValue(EnergyPlusData &state, int const PressureCurveIndex, Real64 const MassFlow, Real64 const Density, Real64 const Viscosity)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   August 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This will evaluate the pressure drop for components which use pressure information

        // METHODOLOGY EMPLOYED:
        // Friction factor pressure drop equation:
        // DP = [f*(L/D) + K] * (rho * V^2) / 2

        auto &curve = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex);

        // Intermediate calculations
        Real64 const CrossSectArea = (DataGlobalConstants::Pi / 4.0) * pow_2(curve.EquivDiameter);
        Real64 const Velocity = MassFlow / (Density * CrossSectArea);
        Real64 const ReynoldsNumber = Density * curve.EquivDiameter * Velocity / Viscosity; // assuming mu here
        Real64 const RoughnessRatio = curve.EquivRoughness / curve.EquivDiameter;

        // update curve bookkeeping
        curve.CurveInput1 = MassFlow;
        curve.CurveInput2 = Density;
        curve.CurveInput3 = Velocity;

        // If we don't have any flow then exit out
        if (MassFlow < DataBranchAirLoopPlant::MassFlowTolerance) {
            curve.CurveOutput = 0.0;
            return 0.0;
        }

        // Calculate the friction factor and pressure drop
        Real64 FrictionFactor = curve.ConstantFPresent ? curve.ConstantF : CalculateMoodyFrictionFactor(state, ReynoldsNumber, RoughnessRatio);
        Real64 PressureCurveValue = curve.EMSOverrideOn ? curve.EMSOverrideCurveValue
                                                        : (FrictionFactor * (curve.EquivLength / curve.EquivDiameter) + curve.MinorLossCoeff) *
                                                              (Density * pow_2(Velocity)) / 2.0;
        curve.CurveOutput = PressureCurveValue;
        return PressureCurveValue;
    }

    Real64 CalculateMoodyFrictionFactor(EnergyPlusData &state, Real64 const ReynoldsNumber, Real64 const RoughnessRatio)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   August 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This will evaluate the moody friction factor based on Reynolds number and roughness ratio

        // METHODOLOGY EMPLOYED:
        // General empirical correlations for friction factor based on Moody Chart data

        // REFERENCES:
        // Haaland, SE (1983). "Simple and Explicit Formulas for the Friction Factor in Turbulent Flow".
        //   Trans. ASIVIE, J. of Fluids Engineering 103: 89-90.

        // Check for no flow or invalid roughness before calculating values
        if (ReynoldsNumber == 0.0 || RoughnessRatio == 0.0) {
            return 0.0;
        }

        // Calculate the friction factor
        Real64 const Term1 = std::pow(RoughnessRatio / 3.7, 1.11);
        Real64 const Term2 = 6.9 / ReynoldsNumber;
        Real64 const Term3 = -1.8 * std::log10(Term1 + Term2);
        if (Term3 != 0.0) {
            return std::pow(Term3, -2.0);
        } else {
            if (!state.dataCurveManager->FrictionFactorErrorHasOccurred) {
                ShowSevereError(state, "Plant Pressure System: Error in moody friction factor calculation");
                ShowContinueError(state,
                                  format("Current Conditions: Roughness Ratio={:.7R}; Reynolds Number={:.1R}", RoughnessRatio, ReynoldsNumber));
                ShowContinueError(state, "These conditions resulted in an unhandled numeric issue.");
                ShowContinueError(state, "Please contact EnergyPlus support/development team to raise an alert about this issue");
                ShowContinueError(state, "This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations");
                state.dataCurveManager->FrictionFactorErrorHasOccurred = true;
            }
            return 0.04;
        }
    }

    void checkCurveIsNormalizedToOne(EnergyPlusData &state,
                                     std::string const &callingRoutineObj, // calling routine with object type
                                     std::string const &objectName,        // parent object where curve is used
                                     int const curveIndex,                 // index to curve object
                                     std::string const &cFieldName,        // object field name
                                     std::string const &cFieldValue,       // user input curve name
                                     Real64 const Var1,                    // required 1st independent variable
                                     Optional<Real64 const> Var2)          // 2nd independent variable
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   May 2017

        // PURPOSE OF THIS FUNCTION:
        // checks that curve output is within 10% of 1 at curve rating point

        if (curveIndex > 0) {
            Real64 const CurveVal = CurveValue(state, curveIndex, Var1, Var2);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, format("{}=\"{}\" curve values", callingRoutineObj, objectName));
                ShowContinueError(state, format("... {} = {} output is not equal to 1.0 (+ or - 10%) at rated conditions.", cFieldName, cFieldValue));
                ShowContinueError(state, format("... Curve output at rated conditions = {:.3T}", CurveVal));
            }
        }
    }

} // namespace Curve

} // namespace EnergyPlus
