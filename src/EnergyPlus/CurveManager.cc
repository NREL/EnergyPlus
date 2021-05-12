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
#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

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

namespace CurveManager {
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
        std::string fullMessage = contextPair.second + ": " + message;
        if (messageType == Btwxt::MsgLevel::MSG_ERR) {
            ShowSevereError(*contextPair.first, fullMessage);
            ShowFatalError(*contextPair.first, "Btwxt: Errors discovered, program terminates.");
        } else {
            if (static_cast<int>(messageType) >= Btwxt::LOG_LEVEL) {
                if (messageType == Btwxt::MsgLevel::MSG_WARN) {
                    ShowWarningError(*contextPair.first, fullMessage);
                } else if (messageType == Btwxt::MsgLevel::MSG_INFO) {
                    ShowMessage(*contextPair.first, fullMessage);
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

        using DataLoopNode::SensedNodeFlagValue;

        for (int CurveIndex = 1; CurveIndex <= state.dataCurveManager->NumCurves; ++CurveIndex) {
            state.dataCurveManager->PerfCurve(CurveIndex).CurveOutput = SensedNodeFlagValue;
            state.dataCurveManager->PerfCurve(CurveIndex).CurveInput1 = SensedNodeFlagValue;
            state.dataCurveManager->PerfCurve(CurveIndex).CurveInput2 = SensedNodeFlagValue;
            state.dataCurveManager->PerfCurve(CurveIndex).CurveInput3 = SensedNodeFlagValue;
            state.dataCurveManager->PerfCurve(CurveIndex).CurveInput4 = SensedNodeFlagValue;
            state.dataCurveManager->PerfCurve(CurveIndex).CurveInput5 = SensedNodeFlagValue;
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

        // need to be careful on where and how resetting curve outputs to some "iactive value" is done
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

        {
            auto const SELECT_CASE_var(state.dataCurveManager->PerfCurve(CurveIndex).InterpolationType);
            if (SELECT_CASE_var == InterpTypeEnum::EvaluateCurveToLimits) {
                CurveValue = PerformanceCurveObject(state, CurveIndex, Var1, Var2, Var3, Var4, Var5, Var6);
            } else if (SELECT_CASE_var == InterpTypeEnum::BtwxtMethod) {
                CurveValue = BtwxtTableInterpolation(state, CurveIndex, Var1, Var2, Var3, Var4, Var5, Var6);
            } else {
                ShowFatalError(state, "CurveValue: Invalid Interpolation Type");
            }
        }

        if (state.dataCurveManager->PerfCurve(CurveIndex).EMSOverrideOn)
            CurveValue = state.dataCurveManager->PerfCurve(CurveIndex).EMSOverrideCurveValue;

        state.dataCurveManager->PerfCurve(CurveIndex).CurveOutput = CurveValue;
        state.dataCurveManager->PerfCurve(CurveIndex).CurveInput1 = Var1;
        if (present(Var2)) state.dataCurveManager->PerfCurve(CurveIndex).CurveInput2 = Var2;
        if (present(Var3)) state.dataCurveManager->PerfCurve(CurveIndex).CurveInput3 = Var3;
        if (present(Var4)) state.dataCurveManager->PerfCurve(CurveIndex).CurveInput4 = Var4;
        if (present(Var5)) state.dataCurveManager->PerfCurve(CurveIndex).CurveInput5 = Var5;
        if (present(Var6)) state.dataCurveManager->PerfCurve(CurveIndex).CurveInput6 = Var6;

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
        int NumBiQuad;                  // Number of biquadratic curve objects in the input data file
        int NumCubic;                   // Number of cubic curve objects in the input data file
        int NumQuartic;                 // Number of quartic (4th order polynomial) objects in the input data file
        int NumQuad;                    // Number of quadratic curve objects in the input data file
        int NumQuadLinear;              // Number of quadratic linear curve objects in the input data file
        int NumCubicLinear;             // Number of cubic linear curve objects in the input file
        int NumQLinear;                 // Number of quad linear curve objects in the input data file
        int NumQuintLinear;             // Number of quint linear curve objects in the input data file
        int NumLinear;                  // Number of linear curve objects in the input data file
        int NumBicubic;                 // Number of bicubic curve objects in the input data file
        int NumTriQuad;                 // Number of triquadratic curve objects in the input file
        int NumExponent;                // Number of exponent curve objects in the input file
        int NumWPCValTab;               // Number of wind pressure coefficient value table objects in the input file
        int NumChillerPartLoadWithLift; // Number of ChillerPartLoadWithLift curve objects in the input data file
        int NumFanPressRise;            // Number of fan pressure rise curve objects in the input file
        int NumExpSkewNorm;             // Number of exponential skew normal curve objects in the input file
        int NumSigmoid;                 // Number of sigmoid curve objects in the input file
        int NumRectHyper1;              // Number of rectangular hyperbola Type 1 curve objects in the input file
        int NumRectHyper2;              // Number of rectangular hyperbola Type 2 curve objects in the input file
        int NumExpDecay;                // Number of exponential decay curve objects in the input file
        int NumDoubleExpDecay;
        int CurveIndex;                 // do loop index
        int CurveNum;                   // current curve number
        Array1D_string Alphas(14);      // Alpha items for object
        Array1D<Real64> Numbers(10000); // Numeric items for object
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call
        int IOStatus;                   // Used in GetObjectItem
        int NumTableLookup;
        std::string CurrentModuleObject; // for ease in renaming.
        int MaxTableNums(0);             // Maximum number of numeric input fields in Tables
        //   certain object in the input file

        // Find the number of each type of curve (note: Current Module object not used here, must rename manually)

        NumBiQuad = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Biquadratic");
        NumCubic = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Cubic");
        NumQuartic = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Quartic");
        NumQuad = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Quadratic");
        NumQLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:QuadLinear");
        NumQuintLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:QuintLinear");
        NumQuadLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:QuadraticLinear");
        NumCubicLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:CubicLinear");
        NumLinear = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Linear");
        NumBicubic = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Bicubic");
        NumTriQuad = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Triquadratic");
        NumExponent = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Exponent");
        NumTableLookup = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Table:Lookup");
        NumFanPressRise = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:FanPressureRise");
        NumExpSkewNorm = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:ExponentialSkewNormal");
        NumSigmoid = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:Sigmoid");
        NumRectHyper1 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:RectangularHyperbola1");
        NumRectHyper2 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:RectangularHyperbola2");
        NumExpDecay = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:ExponentialDecay");
        NumDoubleExpDecay = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:DoubleExponentialDecay");
        NumChillerPartLoadWithLift =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Curve:ChillerPartLoadWithLift"); // zrp_Aug2014

        NumWPCValTab = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirflowNetwork:MultiZone:WindPressureCoefficientValues");

        state.dataCurveManager->NumCurves = NumBiQuad + NumCubic + NumQuad + NumQuadLinear + NumCubicLinear + NumLinear + NumBicubic + NumTriQuad +
                                            NumExponent + NumQuartic + NumTableLookup + NumFanPressRise + NumExpSkewNorm + NumSigmoid +
                                            NumRectHyper1 + NumRectHyper2 + NumExpDecay + NumDoubleExpDecay + NumQLinear + NumQuintLinear +
                                            NumChillerPartLoadWithLift + NumWPCValTab;

        // allocate the data structure
        state.dataCurveManager->PerfCurve.allocate(state.dataCurveManager->NumCurves);
        state.dataCurveManager->UniqueCurveNames.reserve(state.dataCurveManager->NumCurves);
        // initialize the array

        CurveNum = 0;
        // Loop over biquadratic curves and load data
        CurrentModuleObject = "Curve:Biquadratic";
        for (CurveIndex = 1; CurveIndex <= NumBiQuad; ++CurveIndex) {
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

            // could add checks for blank numeric fields, and use field names for errors.
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::BiQuadratic;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff6 = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(8);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(9);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(10);
            if (NumNumbers > 10 && !state.dataIPShortCut->lNumericFieldBlanks(11)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(11);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 11 && !state.dataIPShortCut->lNumericFieldBlanks(12)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(12);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         Numbers(8)));
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over ChillerPartLoadWithLift curves and load data //zrp_Aug2014
        CurrentModuleObject = "Curve:ChillerPartLoadWithLift";
        for (CurveIndex = 1; CurveIndex <= NumChillerPartLoadWithLift; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);

            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::ChillerPartLoadWithLift;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 3;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;

            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff6 = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff7 = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff8 = Numbers(8);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff9 = Numbers(9);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff10 = Numbers(10);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff11 = Numbers(11);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff12 = Numbers(12);

            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(13);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(14);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(15);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(16);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Min = Numbers(17);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Max = Numbers(18);

            if (NumNumbers > 18 && !state.dataIPShortCut->lNumericFieldBlanks(19)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(19);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 19 && !state.dataIPShortCut->lNumericFieldBlanks(20)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(20);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the OInput Unit Type for Z is invalid.");
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveOutputTypeValid(Alphas(5))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over cubic curves and load data
        CurrentModuleObject = "Curve:Cubic";
        for (CurveIndex = 1; CurveIndex <= NumCubic; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::Cubic;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(6);
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(7);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(8);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quadrinomial curves and load data
        CurrentModuleObject = "Curve:Quartic";
        for (CurveIndex = 1; CurveIndex <= NumQuartic; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::Quartic;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(7);
            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(8);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(9);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(6) > Numbers(7)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quadratic curves and load data
        CurrentModuleObject = "Curve:Quadratic";
        for (CurveIndex = 1; CurveIndex <= NumQuad; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::Quadratic;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(5);
            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(6);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(7);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quadratic-linear curves and load data
        CurrentModuleObject = "Curve:QuadraticLinear";
        for (CurveIndex = 1; CurveIndex <= NumQuadLinear; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::QuadraticLinear;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff6 = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(8);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(9);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(10);
            if (NumNumbers > 10 && !state.dataIPShortCut->lNumericFieldBlanks(11)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(11);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 11 && !state.dataIPShortCut->lNumericFieldBlanks(12)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(12);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         Numbers(8)));
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over cubic-linear curves and load data
        CurrentModuleObject = "Curve:CubicLinear";
        for (CurveIndex = 1; CurveIndex <= NumCubicLinear; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::CubicLinear;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff6 = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(8);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(9);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(10);
            if (NumNumbers > 10 && !state.dataIPShortCut->lNumericFieldBlanks(11)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(11);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 11 && !state.dataIPShortCut->lNumericFieldBlanks(12)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(12);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         Numbers(7),
                                         state.dataIPShortCut->cNumericFieldNames(8),
                                         Numbers(8)));
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over linear curves and load data
        CurrentModuleObject = "Curve:Linear";
        for (CurveIndex = 1; CurveIndex <= NumLinear; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::Linear;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(4);
            if (NumNumbers > 4 && !state.dataIPShortCut->lNumericFieldBlanks(5)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(5);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(6);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(3) > Numbers(4)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over bicubic curves and load data
        CurrentModuleObject = "Curve:Bicubic";
        for (CurveIndex = 1; CurveIndex <= NumBicubic; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::BiCubic;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff6 = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff7 = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff8 = Numbers(8);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff9 = Numbers(9);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff10 = Numbers(10);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(11);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(12);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(13);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(14);
            if (NumNumbers > 14 && !state.dataIPShortCut->lNumericFieldBlanks(15)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(15);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 15 && !state.dataIPShortCut->lNumericFieldBlanks(16)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(16);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(11) > Numbers(12)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(11),
                                         Numbers(11),
                                         state.dataIPShortCut->cNumericFieldNames(12),
                                         Numbers(12)));
                ErrorsFound = true;
            }
            if (Numbers(13) > Numbers(14)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over Triquadratic curves and load data
        CurrentModuleObject = "Curve:Triquadratic";
        for (CurveIndex = 1; CurveIndex <= NumTriQuad; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::TriQuadratic;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 3;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Tri2ndOrder.allocate(1);
            for (auto &e : state.dataCurveManager->PerfCurve(CurveNum).Tri2ndOrder) {
                e.CoeffA0 = Numbers(1);
                e.CoeffA1 = Numbers(2);
                e.CoeffA2 = Numbers(3);
                e.CoeffA3 = Numbers(4);
                e.CoeffA4 = Numbers(5);
                e.CoeffA5 = Numbers(6);
                e.CoeffA6 = Numbers(7);
                e.CoeffA7 = Numbers(8);
                e.CoeffA8 = Numbers(9);
                e.CoeffA9 = Numbers(10);
                e.CoeffA10 = Numbers(11);
                e.CoeffA11 = Numbers(12);
                e.CoeffA12 = Numbers(13);
                e.CoeffA13 = Numbers(14);
                e.CoeffA14 = Numbers(15);
                e.CoeffA15 = Numbers(16);
                e.CoeffA16 = Numbers(17);
                e.CoeffA17 = Numbers(18);
                e.CoeffA18 = Numbers(19);
                e.CoeffA19 = Numbers(20);
                e.CoeffA20 = Numbers(21);
                e.CoeffA21 = Numbers(22);
                e.CoeffA22 = Numbers(23);
                e.CoeffA23 = Numbers(24);
                e.CoeffA24 = Numbers(25);
                e.CoeffA25 = Numbers(26);
                e.CoeffA26 = Numbers(27);
            }
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(28);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(29);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(30);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(31);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Min = Numbers(32);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Max = Numbers(33);
            if (NumNumbers > 33 && !state.dataIPShortCut->lNumericFieldBlanks(34)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(34);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 34 && !state.dataIPShortCut->lNumericFieldBlanks(35)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(35);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(28) > Numbers(29)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(28),
                                         Numbers(28),
                                         state.dataIPShortCut->cNumericFieldNames(29),
                                         Numbers(29)));
                ErrorsFound = true;
            }
            if (Numbers(30) > Numbers(31)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(state,
                                  format("{} [{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(30),
                                         Numbers(30),
                                         state.dataIPShortCut->cNumericFieldNames(31),
                                         Numbers(31)));
                ErrorsFound = true;
            }
            if (Numbers(32) > Numbers(33)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveInputTypeValid(Alphas(4))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Z is invalid.");
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveOutputTypeValid(Alphas(5))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quad linear curves and load data
        CurrentModuleObject = "Curve:QuadLinear";
        for (CurveIndex = 1; CurveIndex <= NumQLinear; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::QuadLinear;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 4;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(8);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(9);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Min = Numbers(10);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Max = Numbers(11);
            state.dataCurveManager->PerfCurve(CurveNum).Var4Min = Numbers(12);
            state.dataCurveManager->PerfCurve(CurveNum).Var4Max = Numbers(13);

            if (NumNumbers > 13 && !state.dataIPShortCut->lNumericFieldBlanks(14)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(14);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 14 && !state.dataIPShortCut->lNumericFieldBlanks(15)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(15);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            const int NumVar = 4;
            std::string VarNames[NumVar] = {"w", "x", "y", "z"};
            for (int i = 1; i <= NumVar; ++i) {
                int MinIndex = 2 * i + 4;
                int MaxIndex = MinIndex + 1;
                if (Numbers(MinIndex) > Numbers(MaxIndex)) { // error
                    ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                            state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for " + VarNames[i] + " is invalid.");
                    }
                }
            }
            if (NumAlphas >= 6) {
                if (!IsCurveOutputTypeValid(Alphas(6))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quint linear curves and load data
        CurrentModuleObject = "Curve:QuintLinear";
        for (CurveIndex = 1; CurveIndex <= NumQuintLinear; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::QuintLinear;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 5;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff6 = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(8);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(9);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(10);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Min = Numbers(11);
            state.dataCurveManager->PerfCurve(CurveNum).Var3Max = Numbers(12);
            state.dataCurveManager->PerfCurve(CurveNum).Var4Min = Numbers(13);
            state.dataCurveManager->PerfCurve(CurveNum).Var4Max = Numbers(14);
            state.dataCurveManager->PerfCurve(CurveNum).Var5Min = Numbers(15);
            state.dataCurveManager->PerfCurve(CurveNum).Var5Max = Numbers(16);
            if (NumNumbers > 16 && !state.dataIPShortCut->lNumericFieldBlanks(17)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(17);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 17 && !state.dataIPShortCut->lNumericFieldBlanks(18)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(18);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            const int NumVar = 5;
            std::string VarNames[NumVar] = {"v", "w", "x", "y", "z"};
            for (int i = 1; i <= NumVar; ++i) {
                int MinIndex = 2 * i + 5;
                int MaxIndex = MinIndex + 1;
                if (Numbers(MinIndex) > Numbers(MaxIndex)) { // error
                    ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                            state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for " + VarNames[i] + " is invalid.");
                    }
                }
            }
            if (NumAlphas >= 7) {
                if (!IsCurveOutputTypeValid(Alphas(7))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over Exponent curves and load data
        CurrentModuleObject = "Curve:Exponent";
        for (CurveIndex = 1; CurveIndex <= NumExponent; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::Exponent;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(5);
            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(6);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(7);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over Fan Pressure Rise curves and load data - udated 15Sep2010 for unit types
        CurrentModuleObject = "Curve:FanPressureRise";
        for (CurveIndex = 1; CurveIndex <= NumFanPressRise; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::FanPressureRise;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Min = Numbers(7);
            state.dataCurveManager->PerfCurve(CurveNum).Var2Max = Numbers(8);

            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(9);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 9 && !state.dataIPShortCut->lNumericFieldBlanks(10)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(10);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(state,
                                  format("{}[{:.R2}] > {} [{.R2}]",
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         Numbers(5),
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         Numbers(6)));
                ErrorsFound = true;
            }
            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
        for (CurveIndex = 1; CurveIndex <= NumExpSkewNorm; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::ExponentialSkewNormal;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(6);

            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(7);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(8);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Exponential Skew Normal

        // Loop over Sigmoid curves and load data
        CurrentModuleObject = "Curve:Sigmoid";
        for (CurveIndex = 1; CurveIndex <= NumSigmoid; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::Sigmoid;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(7);

            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(8);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(9);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(6) > Numbers(7)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Sigmoid

        // Loop over Rectangular Hyperbola Type 1 curves and load data
        CurrentModuleObject = "Curve:RectangularHyperbola1";
        for (CurveIndex = 1; CurveIndex <= NumRectHyper1; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::RectangularHyperbola1;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(5);

            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(6);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(7);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Rectangular Hyperbola Type 1

        // Loop over Rectangular Hyperbola Type 2 curves and load data
        CurrentModuleObject = "Curve:RectangularHyperbola2";
        for (CurveIndex = 1; CurveIndex <= NumRectHyper2; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::RectangularHyperbola2;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(5);

            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(6);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(7);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Rectangular Hyperbola Type 2

        // Loop over Exponential Decay curves and load data
        CurrentModuleObject = "Curve:ExponentialDecay";
        for (CurveIndex = 1; CurveIndex <= NumExpDecay; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::ExponentialDecay;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(5);

            if (NumNumbers > 5 && !state.dataIPShortCut->lNumericFieldBlanks(6)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(6);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !state.dataIPShortCut->lNumericFieldBlanks(7)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(7);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Exponential Decay

        // ykt July,2011 Loop over DoubleExponential Decay curves and load data
        CurrentModuleObject = "Curve:DoubleExponentialDecay";
        for (CurveIndex = 1; CurveIndex <= NumDoubleExpDecay; ++CurveIndex) {
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
            state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
            state.dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::DoubleExponentialDecay;
            state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
            state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
            state.dataCurveManager->PerfCurve(CurveNum).Coeff1 = Numbers(1);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff2 = Numbers(2);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff3 = Numbers(3);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff4 = Numbers(4);
            state.dataCurveManager->PerfCurve(CurveNum).Coeff5 = Numbers(5);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Min = Numbers(6);
            state.dataCurveManager->PerfCurve(CurveNum).Var1Max = Numbers(7);

            if (NumNumbers > 7 && !state.dataIPShortCut->lNumericFieldBlanks(8)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMin = Numbers(8);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 8 && !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                state.dataCurveManager->PerfCurve(CurveNum).CurveMax = Numbers(9);
                state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
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
                            ShowSevereError(state, "GetCurveInput: An " + CurrentModuleObject + " object ");
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
                    ShowSevereError(state, "GetCurveInput: An " + CurrentModuleObject + " object ");
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
                                        "GetCurveInput: Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + " = " + Alphas(2) + " in " +
                                            CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError(state, "The valid name is " + wpcName);
                        ErrorsFound = true;
                    }

                    state.dataCurveManager->PerfCurve(CurveNum).Name = Alphas(1);
                    state.dataCurveManager->PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
                    state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;

                    state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::BtwxtMethod;

                    std::string contextString = CurrentModuleObject + " \"" + Alphas(1) + "\"";
                    std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
                    Btwxt::setMessageCallback(CurveManager::BtwxtMessageCallback, &callbackPair);

                    state.dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
                    state.dataCurveManager->PerfCurve(CurveNum).Var1MinPresent = true;
                    state.dataCurveManager->PerfCurve(CurveNum).Var1Max = 360.0;
                    state.dataCurveManager->PerfCurve(CurveNum).Var1MaxPresent = true;

                    state.dataCurveManager->PerfCurve(CurveNum).CurveMin = -1.0;
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;

                    state.dataCurveManager->PerfCurve(CurveNum).CurveMax = 1.0;
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;

                    MaxTableNums = NumNumbers;
                    if (NumNumbers != numWindDir) {
                        ShowSevereError(state, "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
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
                        state.dataCurveManager->PerfCurve(CurveNum).TableIndex = gridIndex;
                        state.dataCurveManager->PerfCurve(CurveNum).GridValueIndex =
                            state.dataCurveManager->btwxtManager.addOutputValues(gridIndex, lookupValues);
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
                    std::string indVarName = UtilityRoutines::MakeUPPERCase(indVar.at("independent_variable_name"));
                    std::string contextString = "Table:IndependentVariable \"" + indVarName + "\"";
                    std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
                    Btwxt::setMessageCallback(CurveManager::BtwxtMessageCallback, &callbackPair);

                    // Find independent variable input data
                    if (state.dataCurveManager->btwxtManager.independentVarRefs.count(indVarName)) {
                        // If found, read data
                        auto const &indVarInstance = state.dataCurveManager->btwxtManager.independentVarRefs.at(indVarName);

                        // TODO: Actually use this to define output variable units
                        if (indVarInstance.count("unit_type")) {
                            std::string unitType = indVarInstance.at("unit_type");
                            if (!IsCurveOutputTypeValid(unitType)) {
                                ShowSevereError(state, contextString + ": Unit Type [" + unitType + "] is invalid");
                            }
                        }

                        std::vector<double> axis;

                        if (indVarInstance.count("external_file_name")) {
                            std::string tmp = indVarInstance.at("external_file_name");
                            fs::path filePath(tmp);
                            if (!indVarInstance.count("external_file_column_number")) {
                                ShowSevereError(state, contextString + ": No column number defined for external file \"" + filePath.string() + "\"");
                                ErrorsFound = true;
                            }
                            if (!indVarInstance.count("external_file_starting_row_number")) {
                                ShowSevereError(state,
                                                contextString + ": No starting row number defined for external file \"" + filePath.string() + "\"");
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
                            for (auto value : indVarInstance.at("values")) {
                                axis.push_back(value.at("value"));
                            }
                        } else {
                            ShowSevereError(state, contextString + ": No values defined.");
                            ErrorsFound = true;
                        }

                        Btwxt::Method interpMethod, extrapMethod;
                        if (indVarInstance.count("interpolation_method")) {
                            interpMethod = CurveManager::BtwxtManager::interpMethods.at(indVarInstance.at("interpolation_method"));
                        } else {
                            interpMethod = Btwxt::Method::CUBIC;
                        }

                        if (indVarInstance.count("extrapolation_method")) {
                            if (indVarInstance.at("extrapolation_method") == "Unavailable") {
                                ShowSevereError(state, contextString + ": Extrapolation method \"Unavailable\" is not yet available.");
                                ErrorsFound = true;
                            }
                            extrapMethod = CurveManager::BtwxtManager::extrapMethods.at(indVarInstance.at("extrapolation_method"));
                        } else {
                            extrapMethod = Btwxt::Method::LINEAR;
                        }

                        double min_grid_value = *std::min_element(axis.begin(), axis.end());
                        double max_grid_value = *std::max_element(axis.begin(), axis.end());

                        double min_val, max_val;
                        if (indVarInstance.count("minimum_value")) {
                            min_val = indVarInstance.at("minimum_value");
                        } else {
                            min_val = min_grid_value;
                        }

                        if (indVarInstance.count("maximum_value")) {
                            max_val = indVarInstance.at("maximum_value");
                        } else {
                            max_val = max_grid_value;
                        }

                        varListLimits[varListName].push_back({min_val, max_val});

                        Real64 normalizationRefValue;
                        if (indVarInstance.count("normalization_reference_value")) {
                            normalizationRefValue = indVarInstance.at("normalization_reference_value");
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
                        ShowSevereError(state, contextString + ": No Table:IndependentVariable found.");
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
                state.dataCurveManager->PerfCurve(CurveNum).Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                state.dataCurveManager->PerfCurve(CurveNum).ObjectType = "Table:Lookup";
                state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::BtwxtMethod;

                std::string indVarListName = UtilityRoutines::MakeUPPERCase(fields.at("independent_variable_list_name"));

                std::string contextString = "Table:Lookup \"" + state.dataCurveManager->PerfCurve(CurveNum).Name + "\"";
                std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
                Btwxt::setMessageCallback(CurveManager::BtwxtMessageCallback, &callbackPair);

                // TODO: Actually use this to define output variable units
                if (fields.count("output_unit_type")) {
                    std::string unitType = fields.at("output_unit_type");
                    if (!IsCurveOutputTypeValid(unitType)) {
                        ShowSevereError(state, contextString + ": Output Unit Type [" + unitType + "] is invalid");
                    }
                }

                int gridIndex = state.dataCurveManager->btwxtManager.getGridIndex(state, indVarListName, ErrorsFound);
                state.dataCurveManager->PerfCurve(CurveNum).TableIndex = gridIndex;
                int numDims = state.dataCurveManager->btwxtManager.getNumGridDims(gridIndex);
                state.dataCurveManager->PerfCurve(CurveNum).NumDims = numDims;

                for (int i = 1; i <= std::min(6, numDims); ++i) {
                    double vMin, vMax;
                    std::tie(vMin, vMax) = varListLimits.at(indVarListName)[i - 1];
                    if (i == 1) {
                        state.dataCurveManager->PerfCurve(CurveNum).Var1Min = vMin;
                        state.dataCurveManager->PerfCurve(CurveNum).Var1Max = vMax;
                    } else if (i == 2) {
                        state.dataCurveManager->PerfCurve(CurveNum).Var2Min = vMin;
                        state.dataCurveManager->PerfCurve(CurveNum).Var2Max = vMax;
                    } else if (i == 3) {
                        state.dataCurveManager->PerfCurve(CurveNum).Var3Min = vMin;
                        state.dataCurveManager->PerfCurve(CurveNum).Var3Max = vMax;
                    } else if (i == 4) {
                        state.dataCurveManager->PerfCurve(CurveNum).Var4Min = vMin;
                        state.dataCurveManager->PerfCurve(CurveNum).Var4Max = vMax;
                    } else if (i == 5) {
                        state.dataCurveManager->PerfCurve(CurveNum).Var5Min = vMin;
                        state.dataCurveManager->PerfCurve(CurveNum).Var5Max = vMax;
                    } else if (i == 6) {
                        state.dataCurveManager->PerfCurve(CurveNum).Var6Min = vMin;
                        state.dataCurveManager->PerfCurve(CurveNum).Var6Max = vMax;
                    }
                }

                if (fields.count("minimum_output")) {
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMin = fields.at("minimum_output");
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = true;
                } else {
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMin = -DBL_MAX;
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent = false;
                }

                if (fields.count("maximum_output")) {
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMax = fields.at("maximum_output");
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = true;
                } else {
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMax = DBL_MAX;
                    state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent = false;
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
                    if (UtilityRoutines::SameString(fields.at("normalization_method"), "DIVISORONLY")) {
                        normalizeMethod = NM_DIVISOR_ONLY;
                    } else if (UtilityRoutines::SameString(fields.at("normalization_method"), "AUTOMATICWITHDIVISOR")) {
                        normalizeMethod = NM_AUTO_WITH_DIVISOR;
                    }
                }

                if (normalizeMethod != NM_NONE && fields.count("normalization_divisor")) {
                    normalizationDivisor = fields.at("normalization_divisor");
                }

                std::vector<double> lookupValues;
                if (fields.count("external_file_name")) {
                    std::string tmp = fields.at("external_file_name");
                    fs::path filePath(tmp);

                    if (!fields.count("external_file_column_number")) {
                        ShowSevereError(state, contextString + ": No column number defined for external file \"" + filePath.string() + "\"");
                        ErrorsFound = true;
                    }
                    if (!fields.count("external_file_starting_row_number")) {
                        ShowSevereError(state, contextString + ": No starting row number defined for external file \"" + filePath.string() + "\"");
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
                    ShowSevereError(state, contextString + ": No values defined.");
                    ErrorsFound = true;
                }

                state.dataCurveManager->PerfCurve(CurveNum).GridValueIndex =
                    state.dataCurveManager->btwxtManager.addOutputValues(gridIndex, lookupValues);

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
                                        contextString + ": Table is to be normalized using AutomaticWithDivisor, but not all independent variables "
                                                        "define a normalization reference value. Make sure either:");
                        ShowContinueError(state, "  Make sure either:");
                        ShowContinueError(state, "    a) a normalization reference value is defined for each independent variable, or");
                        ShowContinueError(state, "    b) no normalization reference values are defined.");
                        ErrorsFound = true;
                    } else if (pointsSpecified) {
                        // normalizeGridValues normalizes curve values to 1.0 at the normalization target, and returns the scalar needed to perform
                        // this normalization. The result is multiplied by the input normalizationDivisor again for the AutomaticWithDivisor case, in
                        // which normalizeGridValues returns a compound scalar.
                        normalizationDivisor =
                            state.dataCurveManager->btwxtManager.normalizeGridValues(
                                gridIndex, state.dataCurveManager->PerfCurve(CurveNum).GridValueIndex, normalizeTarget, normalizationDivisor) *
                            normalizationDivisor;
                    }
                }

                if ((normalizeMethod == NM_DIVISOR_ONLY) || (normalizeMethod == NM_AUTO_WITH_DIVISOR)) {
                    if (state.dataCurveManager->PerfCurve(CurveNum).CurveMaxPresent) {
                        state.dataCurveManager->PerfCurve(CurveNum).CurveMax =
                            state.dataCurveManager->PerfCurve(CurveNum).CurveMax / normalizationDivisor;
                    }
                    if (state.dataCurveManager->PerfCurve(CurveNum).CurveMinPresent) {
                        state.dataCurveManager->PerfCurve(CurveNum).CurveMin =
                            state.dataCurveManager->PerfCurve(CurveNum).CurveMin / normalizationDivisor;
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
            ShowSevereError(state, "Table:Lookup \"" + indVarListName + "\" : No Table:IndependentVariableList found.");
            ErrorsFound = true;
        }
        return gridIndex;
    }

    std::pair<double, double> BtwxtManager::getGridAxisLimits(int gridIndex, int axisIndex)
    {
        return grids[gridIndex].get_axis_limits(axisIndex);
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
        std::string line("");
        numRows = 0;
        numColumns = 0;
        while (getline(file, line)) {
            ++numRows;
            std::size_t pos(0);
            std::size_t colNum(1);
            while ((pos = line.find(",")) != std::string::npos) {
                if (colNum > numColumns) {
                    numColumns = colNum;
                    contents.resize(numColumns);
                }
                contents[colNum - 1].push_back(line.substr(0, pos));
                line.erase(0, pos + 1);
                ++colNum;
            }
            // Anything after the last comma
            if (line.size() > 0) {
                if (colNum > numColumns) {
                    numColumns = colNum;
                    contents.resize(numColumns);
                }
                contents[colNum - 1].push_back(line);
                ++colNum;
            }
            // flesh out columns if row ends early
            while (colNum <= numColumns) {
                contents[colNum - 1].push_back("");
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
            std::transform(content.begin() + row, content.end(), array.begin(), [](const std::string &str) {
                // Convert strings to double
                // see https://stackoverflow.com/a/16575025/1344457
                char *pEnd;
                double ret = std::strtod(&str[0], &pEnd);
                if (*pEnd == '\r') {
                    std::string st = str;
                    st.pop_back();
                    ret = std::strtod(&st[0], &pEnd);
                }
                if (*pEnd || str.size() == 0) {
                    return std::numeric_limits<double>::quiet_NaN();
                } else {
                    return ret;
                }
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
        int CurveIndex;

        for (CurveIndex = 1; CurveIndex <= state.dataCurveManager->NumCurves; ++CurveIndex) {
            for (int dim = 1; dim <= state.dataCurveManager->PerfCurve(CurveIndex).NumDims; ++dim) {
                std::string numStr = fmt::to_string(dim);

                // TODO: Make CurveInput an Array for better looping here...
                switch (dim) {
                case 1:
                    SetupOutputVariable(state,
                                        "Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        state.dataCurveManager->PerfCurve(CurveIndex).CurveInput1,
                                        "HVAC",
                                        "Average",
                                        state.dataCurveManager->PerfCurve(CurveIndex).Name);
                    break;
                case 2:
                    SetupOutputVariable(state,
                                        "Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        state.dataCurveManager->PerfCurve(CurveIndex).CurveInput2,
                                        "HVAC",
                                        "Average",
                                        state.dataCurveManager->PerfCurve(CurveIndex).Name);
                    break;
                case 3:
                    SetupOutputVariable(state,
                                        "Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        state.dataCurveManager->PerfCurve(CurveIndex).CurveInput3,
                                        "HVAC",
                                        "Average",
                                        state.dataCurveManager->PerfCurve(CurveIndex).Name);
                    break;
                case 4:
                    SetupOutputVariable(state,
                                        "Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        state.dataCurveManager->PerfCurve(CurveIndex).CurveInput4,
                                        "HVAC",
                                        "Average",
                                        state.dataCurveManager->PerfCurve(CurveIndex).Name);
                    break;
                case 5:
                    SetupOutputVariable(state,
                                        "Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        state.dataCurveManager->PerfCurve(CurveIndex).CurveInput5,
                                        "HVAC",
                                        "Average",
                                        state.dataCurveManager->PerfCurve(CurveIndex).Name);
                    break;
                case 6:
                    SetupOutputVariable(state,
                                        "Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        state.dataCurveManager->PerfCurve(CurveIndex).CurveInput6,
                                        "HVAC",
                                        "Average",
                                        state.dataCurveManager->PerfCurve(CurveIndex).Name);
                    break;
                default:
                    // Warning?
                    break;
                }
            }
            // set the output up last so it shows up after the input in the csv file
            SetupOutputVariable(state,
                                "Performance Curve Output Value",
                                OutputProcessor::Unit::None,
                                state.dataCurveManager->PerfCurve(CurveIndex).CurveOutput,
                                "HVAC",
                                "Average",
                                state.dataCurveManager->PerfCurve(CurveIndex).Name);
        }

        for (CurveIndex = 1; CurveIndex <= state.dataBranchAirLoopPlant->NumPressureCurves; ++CurveIndex) {
            SetupOutputVariable(state,
                                "Performance Curve Input Variable 1 Value",
                                OutputProcessor::Unit::None,
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).CurveInput1,
                                "HVAC",
                                "Average",
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).Name);
            SetupOutputVariable(state,
                                "Performance Curve Input Variable 2 Value",
                                OutputProcessor::Unit::None,
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).CurveInput2,
                                "HVAC",
                                "Average",
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).Name);
            SetupOutputVariable(state,
                                "Performance Curve Input Variable 3 Value",
                                OutputProcessor::Unit::None,
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).CurveInput3,
                                "HVAC",
                                "Average",
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).Name);
            SetupOutputVariable(state,
                                "Performance Curve Output Value",
                                OutputProcessor::Unit::None,
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).CurveOutput,
                                "HVAC",
                                "Average",
                                state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).Name);
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) { // provide hook for possible EMS control
            for (CurveIndex = 1; CurveIndex <= state.dataCurveManager->NumCurves; ++CurveIndex) {
                SetupEMSActuator(state,
                                 "Curve",
                                 state.dataCurveManager->PerfCurve(CurveIndex).Name,
                                 "Curve Result",
                                 "[unknown]",
                                 state.dataCurveManager->PerfCurve(CurveIndex).EMSOverrideOn,
                                 state.dataCurveManager->PerfCurve(CurveIndex).EMSOverrideCurveValue);
            } // All performance curves
        }
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) { // provide hook for possible EMS control
            for (CurveIndex = 1; CurveIndex <= state.dataBranchAirLoopPlant->NumPressureCurves; ++CurveIndex) {
                SetupEMSActuator(state,
                                 "Curve",
                                 state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).Name,
                                 "Curve Result",
                                 "[unknown]",
                                 state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).EMSOverrideOn,
                                 state.dataBranchAirLoopPlant->PressureCurve(CurveIndex).EMSOverrideCurveValue);
            } // All pressure curves
        }
    }

    Real64 PerformanceCurveObject(EnergyPlusData &state,
                                  int const CurveIndex,                        // index of curve in curve array
                                  Real64 const Var1,                           // 1st independent variable
                                  Optional<Real64 const> Var2,                 // 2nd independent variable
                                  Optional<Real64 const> Var3,                 // 3rd independent variable
                                  Optional<Real64 const> Var4,                 // 4th independent variable
                                  Optional<Real64 const> Var5,                 // 5th independent variable
                                  [[maybe_unused]] Optional<Real64 const> Var6 // 6th independent variable
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

        static Real64 const sqrt_2_inv(1.0 / std::sqrt(2.0));

        Real64 CoeffZ1;         // Coefficient Z1 in exponential skew normal curve
        Real64 CoeffZ2;         // Coefficient Z2 in exponential skew normal curve
        Real64 CoeffZ3;         // Coefficient Z3 in exponential skew normal curve
        Real64 CurveValueNumer; // Numerator in in exponential skew normal curve
        Real64 CurveValueDenom; // Numerator in in exponential skew normal curve
        Real64 CurveValueExp;   // Exponential term in sigmoid curve
        auto const &Curve(state.dataCurveManager->PerfCurve(CurveIndex));

        Real64 const V1(max(min(Var1, Curve.Var1Max), Curve.Var1Min));                        // 1st independent variable after limits imposed
        Real64 const V2(Var2.present() ? max(min(Var2, Curve.Var2Max), Curve.Var2Min) : 0.0); // 2nd independent variable after limits imposed
        Real64 const V3(Var3.present() ? max(min(Var3, Curve.Var3Max), Curve.Var3Min) : 0.0); // 3rd independent variable after limits imposed
        Real64 const V4(Var4.present() ? max(min(Var4, Curve.Var4Max), Curve.Var4Min) : 0.0); // 4th independent variable after limits imposed
        Real64 const V5(Var5.present() ? max(min(Var5, Curve.Var5Max), Curve.Var5Min) : 0.0); // 5th independent variable after limits imposed
        // Real64 const V6(Var6.present() ? max(min(Var6, Curve.Var6Max), Curve.Var6Min) : 0.0); // 6th independent variable after limits imposed

        {
            auto const SELECT_CASE_var(Curve.CurveType);
            if (SELECT_CASE_var == CurveTypeEnum::Linear) {
                CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2;
            } else if (SELECT_CASE_var == CurveTypeEnum::Quadratic) {
                CurveValue = Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * Curve.Coeff3);
            } else if (SELECT_CASE_var == CurveTypeEnum::QuadLinear) {
                CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2 + V2 * Curve.Coeff3 + V3 * Curve.Coeff4 + V4 * Curve.Coeff5;
            } else if (SELECT_CASE_var == CurveTypeEnum::QuintLinear) {
                CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2 + V2 * Curve.Coeff3 + V3 * Curve.Coeff4 + V4 * Curve.Coeff5 + V5 * Curve.Coeff6;
            } else if (SELECT_CASE_var == CurveTypeEnum::Cubic) {
                CurveValue = Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * (Curve.Coeff3 + V1 * Curve.Coeff4));
            } else if (SELECT_CASE_var == CurveTypeEnum::Quartic) {
                CurveValue = Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * (Curve.Coeff3 + V1 * (Curve.Coeff4 + V1 * Curve.Coeff5)));
            } else if (SELECT_CASE_var == CurveTypeEnum::BiQuadratic) {
                CurveValue =
                    Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * Curve.Coeff3) + V2 * (Curve.Coeff4 + V2 * Curve.Coeff5) + V1 * V2 * Curve.Coeff6;
            } else if (SELECT_CASE_var == CurveTypeEnum::QuadraticLinear) {
                CurveValue = (Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * Curve.Coeff3)) + (Curve.Coeff4 + V1 * (Curve.Coeff5 + V1 * Curve.Coeff6)) * V2;
            } else if (SELECT_CASE_var == CurveTypeEnum::CubicLinear) {
                CurveValue = (Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * (Curve.Coeff3 + V1 * Curve.Coeff4))) + (Curve.Coeff5 + V1 * Curve.Coeff6) * V2;
            } else if (SELECT_CASE_var == CurveTypeEnum::BiCubic) {
                CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2 + V1 * V1 * Curve.Coeff3 + V2 * Curve.Coeff4 + V2 * V2 * Curve.Coeff5 +
                             V1 * V2 * Curve.Coeff6 + V1 * V1 * V1 * Curve.Coeff7 + V2 * V2 * V2 * Curve.Coeff8 + V1 * V1 * V2 * Curve.Coeff9 +
                             V1 * V2 * V2 * Curve.Coeff10;
            } else if (SELECT_CASE_var == CurveTypeEnum::ChillerPartLoadWithLift) {
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * V1 + Curve.Coeff3 * V1 * V1 + Curve.Coeff4 * V2 + Curve.Coeff5 * V2 * V2 +
                             Curve.Coeff6 * V1 * V2 + Curve.Coeff7 * V1 * V1 * V1 + Curve.Coeff8 * V2 * V2 * V2 + Curve.Coeff9 * V1 * V1 * V2 +
                             Curve.Coeff10 * V1 * V2 * V2 + Curve.Coeff11 * V1 * V1 * V2 * V2 + Curve.Coeff12 * V3 * V2 * V2 * V2;
            } else if (SELECT_CASE_var == CurveTypeEnum::TriQuadratic) {
                auto const &Tri2ndOrder(Curve.Tri2ndOrder(1));
                auto const V1s(V1 * V1);
                auto const V2s(V2 * V2);
                auto const V3s(V3 * V3);
                CurveValue = Tri2ndOrder.CoeffA0 + Tri2ndOrder.CoeffA1 * V1s + Tri2ndOrder.CoeffA2 * V1 + Tri2ndOrder.CoeffA3 * V2s +
                             Tri2ndOrder.CoeffA4 * V2 + Tri2ndOrder.CoeffA5 * V3s + Tri2ndOrder.CoeffA6 * V3 + Tri2ndOrder.CoeffA7 * V1s * V2s +
                             Tri2ndOrder.CoeffA8 * V1 * V2 + Tri2ndOrder.CoeffA9 * V1 * V2s + Tri2ndOrder.CoeffA10 * V1s * V2 +
                             Tri2ndOrder.CoeffA11 * V1s * V3s + Tri2ndOrder.CoeffA12 * V1 * V3 + Tri2ndOrder.CoeffA13 * V1 * V3s +
                             Tri2ndOrder.CoeffA14 * V1s * V3 + Tri2ndOrder.CoeffA15 * V2s * V3s + Tri2ndOrder.CoeffA16 * V2 * V3 +
                             Tri2ndOrder.CoeffA17 * V2 * V3s + Tri2ndOrder.CoeffA18 * V2s * V3 + Tri2ndOrder.CoeffA19 * V1s * V2s * V3s +
                             Tri2ndOrder.CoeffA20 * V1s * V2s * V3 + Tri2ndOrder.CoeffA21 * V1s * V2 * V3s + Tri2ndOrder.CoeffA22 * V1 * V2s * V3s +
                             Tri2ndOrder.CoeffA23 * V1s * V2 * V3 + Tri2ndOrder.CoeffA24 * V1 * V2s * V3 + Tri2ndOrder.CoeffA25 * V1 * V2 * V3s +
                             Tri2ndOrder.CoeffA26 * V1 * V2 * V3;
            } else if (SELECT_CASE_var == CurveTypeEnum::Exponent) {
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::pow(V1, Curve.Coeff3);
            } else if (SELECT_CASE_var == CurveTypeEnum::FanPressureRise) {
                CurveValue = V1 * (Curve.Coeff1 * V1 + Curve.Coeff2 + Curve.Coeff3 * std::sqrt(V2)) + Curve.Coeff4 * V2;
            } else if (SELECT_CASE_var == CurveTypeEnum::ExponentialSkewNormal) {
                CoeffZ1 = (V1 - Curve.Coeff1) / Curve.Coeff2;
                CoeffZ2 = (Curve.Coeff4 * V1 * std::exp(Curve.Coeff3 * V1) - Curve.Coeff1) / Curve.Coeff2;
                CoeffZ3 = -Curve.Coeff1 / Curve.Coeff2;
                CurveValueNumer = std::exp(-0.5 * (CoeffZ1 * CoeffZ1)) * (1.0 + sign(1.0, CoeffZ2) * std::erf(std::abs(CoeffZ2) * sqrt_2_inv));
                CurveValueDenom = std::exp(-0.5 * (CoeffZ3 * CoeffZ3)) * (1.0 + sign(1.0, CoeffZ3) * std::erf(std::abs(CoeffZ3) * sqrt_2_inv));
                CurveValue = CurveValueNumer / CurveValueDenom;
            } else if (SELECT_CASE_var == CurveTypeEnum::Sigmoid) {
                CurveValueExp = std::exp((Curve.Coeff3 - V1) / Curve.Coeff4);
                CurveValue = Curve.Coeff1 + Curve.Coeff2 / std::pow(1.0 + CurveValueExp, Curve.Coeff5);
            } else if (SELECT_CASE_var == CurveTypeEnum::RectangularHyperbola1) {
                CurveValueNumer = Curve.Coeff1 * V1;
                CurveValueDenom = Curve.Coeff2 + V1;
                CurveValue = (CurveValueNumer / CurveValueDenom) + Curve.Coeff3;
            } else if (SELECT_CASE_var == CurveTypeEnum::RectangularHyperbola2) {
                CurveValueNumer = Curve.Coeff1 * V1;
                CurveValueDenom = Curve.Coeff2 + V1;
                CurveValue = (CurveValueNumer / CurveValueDenom) + (Curve.Coeff3 * V1);
            } else if (SELECT_CASE_var == CurveTypeEnum::ExponentialDecay) {
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::exp(Curve.Coeff3 * V1);
            } else if (SELECT_CASE_var == CurveTypeEnum::DoubleExponentialDecay) {
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::exp(Curve.Coeff3 * V1) + Curve.Coeff4 * std::exp(Curve.Coeff5 * V1);
            } else {
                CurveValue = 0.0;
            }
        }

        if (Curve.CurveMinPresent) CurveValue = max(CurveValue, Curve.CurveMin);
        if (Curve.CurveMaxPresent) CurveValue = min(CurveValue, Curve.CurveMax);

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
        // TODO: Generalize for N-dims
        Real64 var = Var1;
        var = max(min(var, state.dataCurveManager->PerfCurve(CurveIndex).Var1Max), state.dataCurveManager->PerfCurve(CurveIndex).Var1Min);
        std::vector<double> target{var};
        if (present(Var2)) {
            var = Var2;
            var = max(min(var, state.dataCurveManager->PerfCurve(CurveIndex).Var2Max), state.dataCurveManager->PerfCurve(CurveIndex).Var2Min);
            target.push_back(var);
        }
        if (present(Var3)) {
            var = Var3;
            var = max(min(var, state.dataCurveManager->PerfCurve(CurveIndex).Var3Max), state.dataCurveManager->PerfCurve(CurveIndex).Var3Min);
            target.push_back(var);
        }
        if (present(Var4)) {
            var = Var4;
            var = max(min(var, state.dataCurveManager->PerfCurve(CurveIndex).Var4Max), state.dataCurveManager->PerfCurve(CurveIndex).Var4Min);
            target.push_back(var);
        }
        if (present(Var5)) {
            var = Var5;
            var = max(min(var, state.dataCurveManager->PerfCurve(CurveIndex).Var5Max), state.dataCurveManager->PerfCurve(CurveIndex).Var5Min);
            target.push_back(var);
        }
        if (present(Var6)) {
            var = Var6;
            var = max(min(var, state.dataCurveManager->PerfCurve(CurveIndex).Var6Max), state.dataCurveManager->PerfCurve(CurveIndex).Var6Min);
            target.push_back(var);
        }

        std::string contextString = "Table:Lookup \"" + state.dataCurveManager->PerfCurve(CurveIndex).Name + "\"";
        std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
        Btwxt::setMessageCallback(CurveManager::BtwxtMessageCallback, &callbackPair);
        Real64 TableValue = state.dataCurveManager->btwxtManager.getGridValue(
            state.dataCurveManager->PerfCurve(CurveIndex).TableIndex, state.dataCurveManager->PerfCurve(CurveIndex).GridValueIndex, target);

        if (state.dataCurveManager->PerfCurve(CurveIndex).CurveMinPresent)
            TableValue = max(TableValue, state.dataCurveManager->PerfCurve(CurveIndex).CurveMin);
        if (state.dataCurveManager->PerfCurve(CurveIndex).CurveMaxPresent)
            TableValue = min(TableValue, state.dataCurveManager->PerfCurve(CurveIndex).CurveMax);

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

        // Return value
        bool IsCurveInputTypeValid;

        if (len(InInputType) > 0) {
            if (UtilityRoutines::SameString(InInputType, "DIMENSIONLESS")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "TEMPERATURE")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "PRESSURE")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "VOLUMETRICFLOW")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "MASSFLOW")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "POWER")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "DISTANCE")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "WAVELENGTH")) {
                IsCurveInputTypeValid = true;
            } else if (UtilityRoutines::SameString(InInputType, "ANGLE")) {
                IsCurveInputTypeValid = true;
            } else {
                IsCurveInputTypeValid = false;
            }
        } else {
            IsCurveInputTypeValid = true; // if not used it is valid
        }
        return IsCurveInputTypeValid;
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

        // Return value
        bool IsCurveOutputTypeValid;

        if (UtilityRoutines::SameString(InOutputType, "DIMENSIONLESS")) {
            IsCurveOutputTypeValid = true;
        } else if (UtilityRoutines::SameString(InOutputType, "PRESSURE")) {
            IsCurveOutputTypeValid = true;
        } else if (UtilityRoutines::SameString(InOutputType, "TEMPERATURE")) {
            IsCurveOutputTypeValid = true;
        } else if (UtilityRoutines::SameString(InOutputType, "CAPACITY")) {
            IsCurveOutputTypeValid = true;
        } else if (UtilityRoutines::SameString(InOutputType, "POWER")) {
            IsCurveOutputTypeValid = true;
        } else {
            IsCurveOutputTypeValid = false;
        }
        return IsCurveOutputTypeValid;
    }

    bool CheckCurveDims(EnergyPlusData &state,
                        int const CurveIndex,
                        std::vector<int> validDims,
                        std::string_view routineName,
                        const std::string &objectType,
                        const std::string &objectName,
                        const std::string &curveFieldText)
    {
        // Returns true if errors found
        int curveDim = state.dataCurveManager->PerfCurve(CurveIndex).NumDims;
        if (std::find(validDims.begin(), validDims.end(), curveDim) != validDims.end()) {
            // Compatible
            return false;
        } else {
            // Not compatible
            ShowSevereError(state, fmt::format("{}{}=\"{}\"", routineName, objectType, objectName));
            ShowContinueError(state, "...Invalid curve for " + curveFieldText + ".");
            std::string validString = fmt::to_string(validDims[0]);
            for (std::size_t i = 1; i < validDims.size(); i++) {
                validString += format(" or {}", validDims[i]);
            }
            std::string plural1 = curveDim > 1 ? "s" : "";
            std::string plural2 = validDims[validDims.size() - 1] > 1 ? "s" : "";
            ShowContinueError(
                state, format("...Input curve=\"{}\" has {} dimension{}.", state.dataCurveManager->PerfCurve(CurveIndex).Name, curveDim, plural1));
            ShowContinueError(state, "...Curve type must have " + validString + " dimension" + plural2 + ".");
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

        // Return value
        std::string GetCurveName;

        if (CurveIndex > 0) {
            GetCurveName = state.dataCurveManager->PerfCurve(CurveIndex).Name;
        } else {
            GetCurveName = "";
        }
        return GetCurveName;
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

        // Return value
        int GetCurveIndex;

        // First time GetCurveIndex is called, get the input for all the performance curves
        if (state.dataCurveManager->GetCurvesInputFlag) {
            GetCurveInput(state);
            GetPressureSystemInput(state);
            state.dataCurveManager->GetCurvesInputFlag = false;
        }

        if (state.dataCurveManager->NumCurves > 0) {
            GetCurveIndex = UtilityRoutines::FindItemInList(CurveName, state.dataCurveManager->PerfCurve);
        } else {
            GetCurveIndex = 0;
        }

        return GetCurveIndex;
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
            ShowSevereError(state, "Curve Not Found for Object=\"" + ObjName + "\" :: " + alph);
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

        Var1Min = state.dataCurveManager->PerfCurve(CurveIndex).Var1Min;
        Var1Max = state.dataCurveManager->PerfCurve(CurveIndex).Var1Max;
        if (present(Var2Min)) Var2Min = state.dataCurveManager->PerfCurve(CurveIndex).Var2Min;
        if (present(Var2Max)) Var2Max = state.dataCurveManager->PerfCurve(CurveIndex).Var2Max;
        if (present(Var3Min)) Var3Min = state.dataCurveManager->PerfCurve(CurveIndex).Var3Min;
        if (present(Var3Max)) Var3Max = state.dataCurveManager->PerfCurve(CurveIndex).Var3Max;
        if (present(Var4Min)) Var4Min = state.dataCurveManager->PerfCurve(CurveIndex).Var4Min;
        if (present(Var4Max)) Var4Max = state.dataCurveManager->PerfCurve(CurveIndex).Var4Max;
        if (present(Var5Min)) Var5Min = state.dataCurveManager->PerfCurve(CurveIndex).Var5Min;
        if (present(Var5Max)) Var5Max = state.dataCurveManager->PerfCurve(CurveIndex).Var5Max;
        if (present(Var6Min)) Var6Min = state.dataCurveManager->PerfCurve(CurveIndex).Var6Min;
        if (present(Var6Max)) Var6Max = state.dataCurveManager->PerfCurve(CurveIndex).Var6Max;
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

        // Using/Aliasing

        if (CurveIndex > 0 && CurveIndex <= state.dataCurveManager->NumCurves) {

            if (present(CurveMin)) {
                state.dataCurveManager->PerfCurve(CurveIndex).CurveMin = CurveMin;
                state.dataCurveManager->PerfCurve(CurveIndex).CurveMinPresent = true;
            }

            if (present(CurveMax)) {
                state.dataCurveManager->PerfCurve(CurveIndex).CurveMax = CurveMax;
                state.dataCurveManager->PerfCurve(CurveIndex).CurveMaxPresent = true;
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

        // Using/Aliasing

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr auto CurveObjectName = "Curve:Functional:PressureDrop";

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
            state.dataBranchAirLoopPlant->PressureCurve(CurveNum).Name = Alphas(1);
            state.dataBranchAirLoopPlant->PressureCurve(CurveNum).EquivDiameter = Numbers(1);
            state.dataBranchAirLoopPlant->PressureCurve(CurveNum).MinorLossCoeff = Numbers(2);
            state.dataBranchAirLoopPlant->PressureCurve(CurveNum).EquivLength = Numbers(3);
            state.dataBranchAirLoopPlant->PressureCurve(CurveNum).EquivRoughness = Numbers(4);
            if (NumNumbers > 4 && !state.dataIPShortCut->lNumericFieldBlanks(5)) {
                if (Numbers(5) != 0.0) {
                    state.dataBranchAirLoopPlant->PressureCurve(CurveNum).ConstantFPresent = true;
                    state.dataBranchAirLoopPlant->PressureCurve(CurveNum).ConstantF = Numbers(5);
                }
            }
        }

        state.dataBranchAirLoopPlant->NumPressureCurves = NumPressure;

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
        //  PressureCurveType::Error       = pressure name was given, but curve is not available
        //  PressureCurveType::None        = no pressure curve for this branch
        //  PressureCurveType::Pressure    = pressure curve based on friction/minor loss
        //  PressureCurveType::Generic     = curvemanager held curve which is function of flow rate

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TempCurveIndex;
        std::string GenericCurveType;

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
            GenericCurveType = state.dataCurveManager->PerfCurve(TempCurveIndex).ObjectType;
            {
                if (state.dataCurveManager->PerfCurve(TempCurveIndex).NumDims == 1) {
                    PressureCurveType = DataBranchAirLoopPlant::PressureCurveType::Generic;
                    PressureCurveIndex = TempCurveIndex;
                } else {
                    ShowSevereError(state, "Plant Pressure Simulation: Found error for curve: " + PressureCurveName);
                    ShowContinueError(state, "Curve type detected: " + GenericCurveType);
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
        PressureCurveType = DataBranchAirLoopPlant::PressureCurveType::Error;
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

        // Using/Aliasing
        // Return value
        Real64 PressureCurveValue;

        Real64 Diameter;
        Real64 MinorLossCoeff;
        Real64 Length;
        Real64 Roughness;
        bool IsConstFPresent;
        Real64 ConstantF;
        Real64 FrictionFactor;
        Real64 CrossSectArea;
        Real64 Velocity;
        Real64 ReynoldsNumber;
        Real64 RoughnessRatio;

        // Retrieve data from structure
        Diameter = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).EquivDiameter;
        MinorLossCoeff = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).MinorLossCoeff;
        Length = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).EquivLength;
        Roughness = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).EquivRoughness;
        IsConstFPresent = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).ConstantFPresent;
        ConstantF = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).ConstantF;

        // Intermediate calculations
        CrossSectArea = (DataGlobalConstants::Pi / 4.0) * pow_2(Diameter);
        Velocity = MassFlow / (Density * CrossSectArea);
        ReynoldsNumber = Density * Diameter * Velocity / Viscosity; // assuming mu here
        RoughnessRatio = Roughness / Diameter;

        // If we don't have any flow then exit out
        if (MassFlow < DataBranchAirLoopPlant::MassFlowTolerance) {
            PressureCurveValue = 0.0;
            state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveInput1 = MassFlow;
            state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveInput2 = Density;
            state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveInput3 = Velocity;
            state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveOutput = 0.0;
            return PressureCurveValue;
        }

        // Calculate the friction factor
        if (IsConstFPresent) { // use the constant value
            FrictionFactor = ConstantF;
        } else { // must calculate f
            FrictionFactor = CalculateMoodyFrictionFactor(state, ReynoldsNumber, RoughnessRatio);
        }

        // Pressure drop calculation
        PressureCurveValue = (FrictionFactor * (Length / Diameter) + MinorLossCoeff) * (Density * pow_2(Velocity)) / 2.0;

        if (state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).EMSOverrideOn)
            PressureCurveValue = state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).EMSOverrideCurveValue;

        state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveInput1 = MassFlow;
        state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveInput2 = Density;
        state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveInput3 = Velocity;
        state.dataBranchAirLoopPlant->PressureCurve(PressureCurveIndex).CurveOutput = PressureCurveValue;

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

        // Using/Aliasing

        // Return value
        Real64 CalculateMoodyFrictionFactor;

        Real64 Term1;
        Real64 Term2;
        Real64 Term3;
        std::string RR;
        std::string Re;

        // Check for no flow before calculating values
        if (ReynoldsNumber == 0.0) {
            CalculateMoodyFrictionFactor = 0.0;
            return CalculateMoodyFrictionFactor;
        }

        // Check for no roughness also here
        if (RoughnessRatio == 0.0) {
            CalculateMoodyFrictionFactor = 0.0;
            return CalculateMoodyFrictionFactor;
        }

        // Calculate the friction factor
        Term1 = std::pow(RoughnessRatio / 3.7, 1.11);
        Term2 = 6.9 / ReynoldsNumber;
        Term3 = -1.8 * std::log10(Term1 + Term2);
        if (Term3 != 0.0) {
            CalculateMoodyFrictionFactor = std::pow(Term3, -2.0);
        } else {
            if (!state.dataCurveManager->FrictionFactorErrorHasOccurred) {
                RR = format("{:.7R}", RoughnessRatio);
                Re = format("{:.1R}", ReynoldsNumber);
                ShowSevereError(state, "Plant Pressure System: Error in moody friction factor calculation");
                ShowContinueError(state, "Current Conditions: Roughness Ratio=" + RR + "; Reynolds Number=" + Re);
                ShowContinueError(state, "These conditions resulted in an unhandled numeric issue.");
                ShowContinueError(state, "Please contact EnergyPlus support/development team to raise an alert about this issue");
                ShowContinueError(state, "This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations");
                state.dataCurveManager->FrictionFactorErrorHasOccurred = true;
            }
            CalculateMoodyFrictionFactor = 0.04;
        }

        return CalculateMoodyFrictionFactor;
    }

    void checkCurveIsNormalizedToOne(EnergyPlusData &state,
                                     std::string const callingRoutineObj, // calling routine with object type
                                     std::string const objectName,        // parent object where curve is used
                                     int const curveIndex,                // index to curve object
                                     std::string const cFieldName,        // object field name
                                     std::string const cFieldValue,       // user input curve name
                                     Real64 const Var1,                   // required 1st independent variable
                                     Optional<Real64 const> Var2,         // 2nd independent variable
                                     Optional<Real64 const> Var3,         // 3rd independent variable
                                     Optional<Real64 const> Var4,         // 4th independent variable
                                     Optional<Real64 const> Var5,         // 5th independent variable
                                     Optional<Real64 const> Var6          // 6th independent variable
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   May 2017

        // PURPOSE OF THIS FUNCTION:
        // checks that curve output is within 10% of 1 at curve rating point

        // Locals
        Real64 CurveVal;

        if (curveIndex > 0) {
            CurveVal = CurveValue(state, curveIndex, Var1, Var2, Var3, Var4, Var5, Var6);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(state, callingRoutineObj + "=\"" + objectName + "\" curve values");
                ShowContinueError(state, "... " + cFieldName + " = " + cFieldValue + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                ShowContinueError(state, format("... Curve output at rated conditions = {:.3T}", CurveVal));
            }
        }
    }

} // namespace CurveManager

} // namespace EnergyPlus
