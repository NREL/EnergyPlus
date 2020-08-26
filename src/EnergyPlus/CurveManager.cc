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
#include <cmath>
#include <string>
#include <algorithm>
#include <fstream>
#include <limits>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
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
    //                      Aug.  2014, Rongpeng Zhang, added a new curve type (ChillerPartLoadWithLift)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To provide the capabilities of getting the curve data from the input,
    // validating it, and storing it in such a manner that the curve manager
    // can provide the simulation with performance curve output.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // OTHER NOTES:

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using DataGlobals::AnyEnergyManagementSystemInModel;
    using namespace DataBranchAirLoopPlant;

    // Use statements for access to subroutines in other modules

    // Data
    // MODULE PARAMETER DEFINITIONS
    static std::string const BlankString;

    using json = nlohmann::json;


    // Curve Type parameters, these can differ from object types (e.g. a CurveType_TableOneIV can be linear, quadratic, etc)
    int const Linear(1);
    int const BiLinear(2);
    int const Quadratic(3);
    int const BiQuadratic(4);
    int const Cubic(5);
    int const QuadraticLinear(6);
    int const BiCubic(7);
    int const TriQuadratic(8);
    int const Exponent(9);
    int const Quartic(10);
    int const FanPressureRise(11);
    int const ExponentialSkewNormal(12);
    int const Sigmoid(13);
    int const RectangularHyperbola1(14);
    int const RectangularHyperbola2(15);
    int const ExponentialDecay(16);
    int const DoubleExponentialDecay(17);
    int const QuadLinear(18);
    int const CubicLinear(19);
    int const ChillerPartLoadWithLift(20);

    // Interpolation Types
    int const EvaluateCurveToLimits(1);
    int const BtwxtMethod(2);

    std::map<std::string, Btwxt::Method>  BtwxtManager::interpMethods =
            {{"Linear", Btwxt::Method::LINEAR}, {"Cubic", Btwxt::Method::CUBIC}};

    std::map<std::string, Btwxt::Method>  BtwxtManager::extrapMethods =
            {{"Linear", Btwxt::Method::LINEAR}, {"Constant", Btwxt::Method::CONSTANT}};

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    int NumCurves(0);              // Autodesk Was used unintialized in InitCurveReporting
    bool GetCurvesInputFlag(true); // First time, input is "gotten"

    // SUBROUTINE SPECIFICATIONS FOR MODULE

    // Object Data
    Array1D<PerfomanceCurveData> PerfCurve;
    BtwxtManager btwxtManager;
    std::unordered_map<std::string, std::string> UniqueCurveNames;
    bool CurveValueMyBeginTimeStepFlag;
    bool FrictionFactorErrorHasOccurred = false;

    // Functions
    void BtwxtMessageCallback(
        const Btwxt::MsgLevel messageType,
        const std::string message,
        void *contextPtr
    ) {
        std::string fullMessage = *(std::string*)contextPtr + ": " + message;
        if (messageType == Btwxt::MsgLevel::MSG_ERR) {
            ShowSevereError(fullMessage);
            ShowFatalError("Btwxt: Errors discovered, program terminates.");
        } else {
            if (static_cast<int>(messageType) >= Btwxt::LOG_LEVEL) {
                if (messageType == Btwxt::MsgLevel::MSG_WARN) {
                    ShowWarningError(fullMessage);
                } else if (messageType == Btwxt::MsgLevel::MSG_INFO) {
                    ShowMessage(fullMessage);
                } else {
                    ShowMessage(fullMessage);
                }
            }
        }
    }

// Clears the global data in CurveManager.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        NumCurves = 0;
        GetCurvesInputFlag = true;
        UniqueCurveNames.clear();
        PerfCurve.deallocate();
        btwxtManager.clear();
        CurveValueMyBeginTimeStepFlag = true;  // uhh, this static wasn't initialized inside the CurveValue function
        FrictionFactorErrorHasOccurred = false;
    }

    void ResetPerformanceCurveOutput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   August 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        // PURPOSE OF THIS SUBROUTINE:
        // Reset curve outputs prior to simulating air loops, plant loops, etc.
        // This allows the report variable for curve/table objects to show an inactive state.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataLoopNode::SensedNodeFlagValue;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        // SUBROUTINE ARGUMENT DEFINITIONS:
        int CurveIndex;

        for (CurveIndex = 1; CurveIndex <= NumCurves; ++CurveIndex) {
            PerfCurve(CurveIndex).CurveOutput = SensedNodeFlagValue;
            PerfCurve(CurveIndex).CurveInput1 = SensedNodeFlagValue;
            PerfCurve(CurveIndex).CurveInput2 = SensedNodeFlagValue;
            PerfCurve(CurveIndex).CurveInput3 = SensedNodeFlagValue;
            PerfCurve(CurveIndex).CurveInput4 = SensedNodeFlagValue;
            PerfCurve(CurveIndex).CurveInput5 = SensedNodeFlagValue;
        }
    }

    Real64 CurveValue(int const CurveIndex,        // index of curve in curve array
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::BeginEnvrnFlag;

        // Return value
        Real64 CurveValue(0.0);

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // need to be careful on where and how resetting curve outputs to some "iactive value" is done
        // EMS can intercept curves and modify output
        if (BeginEnvrnFlag && CurveValueMyBeginTimeStepFlag) {
            ResetPerformanceCurveOutput();
            CurveValueMyBeginTimeStepFlag = false;
        }

        if (!BeginEnvrnFlag) {
            CurveValueMyBeginTimeStepFlag = true;
        }

        if ((CurveIndex <= 0) || (CurveIndex > NumCurves)) {
            ShowFatalError("CurveValue: Invalid curve passed.");
        }

        {
            auto const SELECT_CASE_var(PerfCurve(CurveIndex).InterpolationType);
            if (SELECT_CASE_var == EvaluateCurveToLimits) {
                CurveValue = PerformanceCurveObject(CurveIndex, Var1, Var2, Var3);
            } else if (SELECT_CASE_var == BtwxtMethod) {
                CurveValue = BtwxtTableInterpolation(CurveIndex, Var1, Var2, Var3, Var4, Var5, Var6);
            } else {
                ShowFatalError("CurveValue: Invalid Interpolation Type");
            }
        }

        if (PerfCurve(CurveIndex).EMSOverrideOn) CurveValue = PerfCurve(CurveIndex).EMSOverrideCurveValue;

        PerfCurve(CurveIndex).CurveOutput = CurveValue;
        PerfCurve(CurveIndex).CurveInput1 = Var1;
        if (present(Var2)) PerfCurve(CurveIndex).CurveInput2 = Var2;
        if (present(Var3)) PerfCurve(CurveIndex).CurveInput3 = Var3;
        if (present(Var4)) PerfCurve(CurveIndex).CurveInput4 = Var4;
        if (present(Var5)) PerfCurve(CurveIndex).CurveInput5 = Var5;
        if (present(Var6)) PerfCurve(CurveIndex).CurveInput6 = Var6;

        return CurveValue;
    }

    void GetCurveInput()
    {
        // wrapper for GetInput to allow unit testing when fatal inputs are detected - follow pattern from GetSetPointManagerInputs()
        bool GetInputErrorsFound = false;

        GetCurveInputData(GetInputErrorsFound);
        GetCurvesInputFlag = false;

        if (GetInputErrorsFound) {
            ShowFatalError("GetCurveInput: Errors found in getting Curve Objects.  Preceding condition(s) cause termination.");
        }
    }

    void GetCurveInputData(bool &ErrorsFound)
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

        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics
        using General::RoundSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumBiQuad;                   // Number of biquadratic curve objects in the input data file
        int NumCubic;                    // Number of cubic curve objects in the input data file
        int NumQuartic;                  // Number of quartic (4th order polynomial) objects in the input data file
        int NumQuad;                     // Number of quadratic curve objects in the input data file
        int NumQuadLinear;               // Number of quadratic linear curve objects in the input data file
        int NumCubicLinear;              // Number of cubic linear curve objects in the input file
        int NumQLinear;                  // Number of quad linear curve objects in the input data file
        int NumLinear;                   // Number of linear curve objects in the input data file
        int NumBicubic;                  // Number of bicubic curve objects in the input data file
        int NumTriQuad;                  // Number of triquadratic curve objects in the input file
        int NumExponent;                 // Number of exponent curve objects in the input file
        int NumWPCValTab;                // Number of wind pressure coefficient value table objects in the input file
        int NumChillerPartLoadWithLift;  // Number of ChillerPartLoadWithLift curve objects in the input data file
        int NumFanPressRise;             // cpw22Aug2010 Number of fan pressure rise curve objects in the input file
        int NumExpSkewNorm;              // cpw22Aug2010 Number of exponential skew normal curve objects in the input file
        int NumSigmoid;                  // cpw22Aug2010 Number of sigmoid curve objects in the input file
        int NumRectHyper1;               // cpw22Aug2010 Number of rectangular hyperbola Type 1 curve objects in the input file
        int NumRectHyper2;               // cpw22Aug2010 Number of rectangular hyperbola Type 2 curve objects in the input file
        int NumExpDecay;                 // cpw22Aug2010 Number of exponential decay curve objects in the input file
        int NumDoubleExpDecay;           // ykt July 2011
        int CurveIndex;                  // do loop index
        int CurveNum;                    // current curve number
        Array1D_string Alphas(14);       // Alpha items for object
        Array1D<Real64> Numbers(10000);  // Numeric items for object
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        int IOStatus;                    // Used in GetObjectItem
        int NumTableLookup;
        std::string CurrentModuleObject; // for ease in renaming.
        static int MaxTableNums(0);      // Maximum number of numeric input fields in Tables
        //   certain object in the input file

        std::string FileName; // name of external table data file

        // Find the number of each type of curve (note: Current Module object not used here, must rename manually)

        NumBiQuad = inputProcessor->getNumObjectsFound("Curve:Biquadratic");
        NumCubic = inputProcessor->getNumObjectsFound("Curve:Cubic");
        NumQuartic = inputProcessor->getNumObjectsFound("Curve:Quartic");
        NumQuad = inputProcessor->getNumObjectsFound("Curve:Quadratic");
        NumQLinear = inputProcessor->getNumObjectsFound("Curve:QuadLinear");
        NumQuadLinear = inputProcessor->getNumObjectsFound("Curve:QuadraticLinear");
        NumCubicLinear = inputProcessor->getNumObjectsFound("Curve:CubicLinear");
        NumLinear = inputProcessor->getNumObjectsFound("Curve:Linear");
        NumBicubic = inputProcessor->getNumObjectsFound("Curve:Bicubic");
        NumTriQuad = inputProcessor->getNumObjectsFound("Curve:Triquadratic");
        NumExponent = inputProcessor->getNumObjectsFound("Curve:Exponent");
        NumTableLookup = inputProcessor->getNumObjectsFound("Table:Lookup");
        NumFanPressRise = inputProcessor->getNumObjectsFound("Curve:FanPressureRise");                    // cpw22Aug2010
        NumExpSkewNorm = inputProcessor->getNumObjectsFound("Curve:ExponentialSkewNormal");               // cpw22Aug2010
        NumSigmoid = inputProcessor->getNumObjectsFound("Curve:Sigmoid");                                 // cpw22Aug2010
        NumRectHyper1 = inputProcessor->getNumObjectsFound("Curve:RectangularHyperbola1");                // cpw22Aug2010
        NumRectHyper2 = inputProcessor->getNumObjectsFound("Curve:RectangularHyperbola2");                // cpw22Aug2010
        NumExpDecay = inputProcessor->getNumObjectsFound("Curve:ExponentialDecay");                       // cpw22Aug2010
        NumDoubleExpDecay = inputProcessor->getNumObjectsFound("Curve:DoubleExponentialDecay");           // ykt July 2011
        NumChillerPartLoadWithLift = inputProcessor->getNumObjectsFound("Curve:ChillerPartLoadWithLift"); // zrp_Aug2014

        NumWPCValTab = inputProcessor->getNumObjectsFound("AirflowNetwork:MultiZone:WindPressureCoefficientValues");

        NumCurves = NumBiQuad + NumCubic + NumQuad + NumQuadLinear + NumCubicLinear + NumLinear + NumBicubic + NumTriQuad + NumExponent + NumQuartic +
                    NumTableLookup + NumFanPressRise + NumExpSkewNorm + NumSigmoid + NumRectHyper1 + NumRectHyper2 +
                    NumExpDecay + NumDoubleExpDecay + NumQLinear + NumChillerPartLoadWithLift + NumWPCValTab;

        // allocate the data structure
        PerfCurve.allocate(NumCurves);
        UniqueCurveNames.reserve(NumCurves);
        // initialize the array

        CurveNum = 0;
        // Loop over biquadratic curves and load data
        CurrentModuleObject = "Curve:Biquadratic";
        for (CurveIndex = 1; CurveIndex <= NumBiQuad; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;

            // could add checks for blank numeric fields, and use field names for errors.
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = BiQuadratic;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 2;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Coeff6 = Numbers(6);
            PerfCurve(CurveNum).Var1Min = Numbers(7);
            PerfCurve(CurveNum).Var1Max = Numbers(8);
            PerfCurve(CurveNum).Var2Min = Numbers(9);
            PerfCurve(CurveNum).Var2Max = Numbers(10);
            if (NumNumbers > 10 && !lNumericFieldBlanks(11)) {
                PerfCurve(CurveNum).CurveMin = Numbers(11);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 11 && !lNumericFieldBlanks(12)) {
                PerfCurve(CurveNum).CurveMax = Numbers(12);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(7) + " [" + RoundSigDigits(Numbers(7), 2) + "] > " + cNumericFieldNames(8) + " [" +
                                  RoundSigDigits(Numbers(8), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(9) + " [" + RoundSigDigits(Numbers(9), 2) + "] > " + cNumericFieldNames(10) + " [" +
                                  RoundSigDigits(Numbers(10), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over ChillerPartLoadWithLift curves and load data //zrp_Aug2014
        CurrentModuleObject = "Curve:ChillerPartLoadWithLift";
        for (CurveIndex = 1; CurveIndex <= NumChillerPartLoadWithLift; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);

            PerfCurve(CurveNum).CurveType = ChillerPartLoadWithLift;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 3;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;

            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Coeff6 = Numbers(6);
            PerfCurve(CurveNum).Coeff7 = Numbers(7);
            PerfCurve(CurveNum).Coeff8 = Numbers(8);
            PerfCurve(CurveNum).Coeff9 = Numbers(9);
            PerfCurve(CurveNum).Coeff10 = Numbers(10);
            PerfCurve(CurveNum).Coeff11 = Numbers(11);
            PerfCurve(CurveNum).Coeff12 = Numbers(12);

            PerfCurve(CurveNum).Var1Min = Numbers(13);
            PerfCurve(CurveNum).Var1Max = Numbers(14);
            PerfCurve(CurveNum).Var2Min = Numbers(15);
            PerfCurve(CurveNum).Var2Max = Numbers(16);
            PerfCurve(CurveNum).Var3Min = Numbers(17);
            PerfCurve(CurveNum).Var3Max = Numbers(18);

            if (NumNumbers > 18 && !lNumericFieldBlanks(19)) {
                PerfCurve(CurveNum).CurveMin = Numbers(19);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 19 && !lNumericFieldBlanks(20)) {
                PerfCurve(CurveNum).CurveMax = Numbers(20);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the OInput Unit Type for Z is invalid.");
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveOutputTypeValid(Alphas(5))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over cubic curves and load data
        CurrentModuleObject = "Curve:Cubic";
        for (CurveIndex = 1; CurveIndex <= NumCubic; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ++CurveNum;
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = Cubic;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Var1Min = Numbers(5);
            PerfCurve(CurveNum).Var1Max = Numbers(6);
            if (NumNumbers > 6 && !lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).CurveMin = Numbers(7);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 7 && !lNumericFieldBlanks(8)) {
                PerfCurve(CurveNum).CurveMax = Numbers(8);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(5) + '[' + RoundSigDigits(Numbers(5), 2) + "] > " + cNumericFieldNames(6) + " [" +
                                  RoundSigDigits(Numbers(6), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quadrinomial curves and load data
        CurrentModuleObject = "Curve:Quartic";
        for (CurveIndex = 1; CurveIndex <= NumQuartic; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = Quartic;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Var1Min = Numbers(6);
            PerfCurve(CurveNum).Var1Max = Numbers(7);
            if (NumNumbers > 7 && !lNumericFieldBlanks(8)) {
                PerfCurve(CurveNum).CurveMin = Numbers(8);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 8 && !lNumericFieldBlanks(9)) {
                PerfCurve(CurveNum).CurveMax = Numbers(9);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(6) > Numbers(7)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(6) + '[' + RoundSigDigits(Numbers(6), 2) + "] > " + cNumericFieldNames(7) + " [" +
                                  RoundSigDigits(Numbers(7), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quadratic curves and load data
        CurrentModuleObject = "Curve:Quadratic";
        for (CurveIndex = 1; CurveIndex <= NumQuad; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = Quadratic;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Var1Min = Numbers(4);
            PerfCurve(CurveNum).Var1Max = Numbers(5);
            if (NumNumbers > 5 && !lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).CurveMin = Numbers(6);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).CurveMax = Numbers(7);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(4) + " [" + RoundSigDigits(Numbers(4), 2) + "] > " + cNumericFieldNames(5) + " [" +
                                  RoundSigDigits(Numbers(5), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quadratic-linear curves and load data
        CurrentModuleObject = "Curve:QuadraticLinear";
        for (CurveIndex = 1; CurveIndex <= NumQuadLinear; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = QuadraticLinear;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 2;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Coeff6 = Numbers(6);
            PerfCurve(CurveNum).Var1Min = Numbers(7);
            PerfCurve(CurveNum).Var1Max = Numbers(8);
            PerfCurve(CurveNum).Var2Min = Numbers(9);
            PerfCurve(CurveNum).Var2Max = Numbers(10);
            if (NumNumbers > 10 && !lNumericFieldBlanks(11)) {
                PerfCurve(CurveNum).CurveMin = Numbers(11);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 11 && !lNumericFieldBlanks(12)) {
                PerfCurve(CurveNum).CurveMax = Numbers(12);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(7) + " [" + RoundSigDigits(Numbers(7), 2) + "] > " + cNumericFieldNames(8) + " [" +
                                  RoundSigDigits(Numbers(8), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(9) + " [" + RoundSigDigits(Numbers(9), 2) + "] > " + cNumericFieldNames(10) + " [" +
                                  RoundSigDigits(Numbers(10), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over cubic-linear curves and load data
        CurrentModuleObject = "Curve:CubicLinear";
        for (CurveIndex = 1; CurveIndex <= NumCubicLinear; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = CubicLinear;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 2;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Coeff6 = Numbers(6);
            PerfCurve(CurveNum).Var1Min = Numbers(7);
            PerfCurve(CurveNum).Var1Max = Numbers(8);
            PerfCurve(CurveNum).Var2Min = Numbers(9);
            PerfCurve(CurveNum).Var2Max = Numbers(10);
            if (NumNumbers > 10 && !lNumericFieldBlanks(11)) {
                PerfCurve(CurveNum).CurveMin = Numbers(11);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 11 && !lNumericFieldBlanks(12)) {
                PerfCurve(CurveNum).CurveMax = Numbers(12);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(7) + " [" + RoundSigDigits(Numbers(7), 2) + "] > " + cNumericFieldNames(8) + " [" +
                                  RoundSigDigits(Numbers(8), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(9) + " [" + RoundSigDigits(Numbers(9), 2) + "] > " + cNumericFieldNames(10) + " [" +
                                  RoundSigDigits(Numbers(10), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over linear curves and load data
        CurrentModuleObject = "Curve:Linear";
        for (CurveIndex = 1; CurveIndex <= NumLinear; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = Linear;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Var1Min = Numbers(3);
            PerfCurve(CurveNum).Var1Max = Numbers(4);
            if (NumNumbers > 4 && !lNumericFieldBlanks(5)) {
                PerfCurve(CurveNum).CurveMin = Numbers(5);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 5 && !lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).CurveMax = Numbers(6);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(3) > Numbers(4)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(3) + " [" + RoundSigDigits(Numbers(3), 2) + "] > " + cNumericFieldNames(4) + " [" +
                                  RoundSigDigits(Numbers(4), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over bicubic curves and load data
        CurrentModuleObject = "Curve:Bicubic";
        for (CurveIndex = 1; CurveIndex <= NumBicubic; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = BiCubic;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 2;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Coeff6 = Numbers(6);
            PerfCurve(CurveNum).Coeff7 = Numbers(7);
            PerfCurve(CurveNum).Coeff8 = Numbers(8);
            PerfCurve(CurveNum).Coeff9 = Numbers(9);
            PerfCurve(CurveNum).Coeff10 = Numbers(10);
            PerfCurve(CurveNum).Var1Min = Numbers(11);
            PerfCurve(CurveNum).Var1Max = Numbers(12);
            PerfCurve(CurveNum).Var2Min = Numbers(13);
            PerfCurve(CurveNum).Var2Max = Numbers(14);
            if (NumNumbers > 14 && !lNumericFieldBlanks(15)) {
                PerfCurve(CurveNum).CurveMin = Numbers(15);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 15 && !lNumericFieldBlanks(16)) {
                PerfCurve(CurveNum).CurveMax = Numbers(16);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(11) > Numbers(12)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(11) + " [" + RoundSigDigits(Numbers(11), 2) + "] > " + cNumericFieldNames(12) + " [" +
                                  RoundSigDigits(Numbers(12), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(13) > Numbers(14)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(13) + " [" + RoundSigDigits(Numbers(13), 2) + "] > " + cNumericFieldNames(14) + " [" +
                                  RoundSigDigits(Numbers(14), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveOutputTypeValid(Alphas(4))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over Triquadratic curves and load data
        CurrentModuleObject = "Curve:Triquadratic";
        for (CurveIndex = 1; CurveIndex <= NumTriQuad; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = TriQuadratic;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 3;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Tri2ndOrder.allocate(1);
            for (auto &e : PerfCurve(CurveNum).Tri2ndOrder) {
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
            PerfCurve(CurveNum).Var1Min = Numbers(28);
            PerfCurve(CurveNum).Var1Max = Numbers(29);
            PerfCurve(CurveNum).Var2Min = Numbers(30);
            PerfCurve(CurveNum).Var2Max = Numbers(31);
            PerfCurve(CurveNum).Var3Min = Numbers(32);
            PerfCurve(CurveNum).Var3Max = Numbers(33);
            if (NumNumbers > 33 && !lNumericFieldBlanks(34)) {
                PerfCurve(CurveNum).CurveMin = Numbers(34);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 34 && !lNumericFieldBlanks(35)) {
                PerfCurve(CurveNum).CurveMax = Numbers(35);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(28) > Numbers(29)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(28) + " [" + RoundSigDigits(Numbers(28), 2) + "] > " + cNumericFieldNames(29) + " [" +
                                  RoundSigDigits(Numbers(29), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(30) > Numbers(31)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(30) + " [" + RoundSigDigits(Numbers(30), 2) + "] > " + cNumericFieldNames(31) + " [" +
                                  RoundSigDigits(Numbers(31), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(32) > Numbers(33)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(32) + " [" + RoundSigDigits(Numbers(32), 2) + "] > " + cNumericFieldNames(33) + " [" +
                                  RoundSigDigits(Numbers(33), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveInputTypeValid(Alphas(4))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Z is invalid.");
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveOutputTypeValid(Alphas(5))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // Loop over quad linear curves and load data
        CurrentModuleObject = "Curve:QuadLinear";
        for (CurveIndex = 1; CurveIndex <= NumQLinear; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = QuadLinear;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 4;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Var1Min = Numbers(6);
            PerfCurve(CurveNum).Var1Max = Numbers(7);
            PerfCurve(CurveNum).Var2Min = Numbers(8);
            PerfCurve(CurveNum).Var2Max = Numbers(9);
            PerfCurve(CurveNum).Var3Min = Numbers(10);
            PerfCurve(CurveNum).Var3Max = Numbers(11);
            PerfCurve(CurveNum).Var4Min = Numbers(12);
            PerfCurve(CurveNum).Var4Max = Numbers(13);

            if (NumNumbers > 13 && !lNumericFieldBlanks(14)) {
                PerfCurve(CurveNum).CurveMin = Numbers(14);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 14 && !lNumericFieldBlanks(15)) {
                PerfCurve(CurveNum).CurveMax = Numbers(15);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(6) > Numbers(7)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(6) + " [" + RoundSigDigits(Numbers(6), 2) + "] > " + cNumericFieldNames(7) + " [" +
                                  RoundSigDigits(Numbers(7), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(8) > Numbers(9)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(8) + " [" + RoundSigDigits(Numbers(8), 2) + "] > " + cNumericFieldNames(9) + " [" +
                                  RoundSigDigits(Numbers(9), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(10) > Numbers(11)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(10) + " [" + RoundSigDigits(Numbers(10), 2) + "] > " + cNumericFieldNames(11) + " [" +
                                  RoundSigDigits(Numbers(11), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(12) > Numbers(13)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(12) + " [" + RoundSigDigits(Numbers(12), 2) + "] > " + cNumericFieldNames(13) + " [" +
                                  RoundSigDigits(Numbers(13), 2) + ']');
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for W is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveInputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 4) {
                if (!IsCurveInputTypeValid(Alphas(4))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Y is invalid.");
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveInputTypeValid(Alphas(5))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for Z is invalid.");
                }
            }
            if (NumAlphas >= 6) {
                if (!IsCurveOutputTypeValid(Alphas(6))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }
        // Loop over Exponent curves and load data
        CurrentModuleObject = "Curve:Exponent";
        for (CurveIndex = 1; CurveIndex <= NumExponent; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = Exponent;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Var1Min = Numbers(4);
            PerfCurve(CurveNum).Var1Max = Numbers(5);
            if (NumNumbers > 5 && !lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).CurveMin = Numbers(6);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).CurveMax = Numbers(7);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }
            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        }

        // cpw22Aug2010 Loop over Fan Pressure Rise curves and load data - udated 15Sep2010 for unit types
        CurrentModuleObject = "Curve:FanPressureRise";
        for (CurveIndex = 1; CurveIndex <= NumFanPressRise; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = FanPressureRise;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 2;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Var1Min = Numbers(5);
            PerfCurve(CurveNum).Var1Max = Numbers(6);
            PerfCurve(CurveNum).Var2Min = Numbers(7);
            PerfCurve(CurveNum).Var2Max = Numbers(8);

            if (NumNumbers > 8 && !lNumericFieldBlanks(9)) {
                PerfCurve(CurveNum).CurveMin = Numbers(9);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 9 && !lNumericFieldBlanks(10)) {
                PerfCurve(CurveNum).CurveMax = Numbers(10);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(5) + '[' + RoundSigDigits(Numbers(5), 2) + "] > " + cNumericFieldNames(6) + " [" +
                                  RoundSigDigits(Numbers(6), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(7) + '[' + RoundSigDigits(Numbers(7), 2) + "] > " + cNumericFieldNames(8) + " [" +
                                  RoundSigDigits(Numbers(8), 2) + ']');
                ErrorsFound = true;
            }

        } // Fan Pressure Rise

        // cpw22Aug2010 Loop over Exponential Skew Normal curves and load data
        CurrentModuleObject = "Curve:ExponentialSkewNormal";
        for (CurveIndex = 1; CurveIndex <= NumExpSkewNorm; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = ExponentialSkewNormal;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Var1Min = Numbers(5);
            PerfCurve(CurveNum).Var1Max = Numbers(6);

            if (NumNumbers > 6 && !lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).CurveMin = Numbers(7);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 7 && !lNumericFieldBlanks(8)) {
                PerfCurve(CurveNum).CurveMax = Numbers(8);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(5) + '[' + RoundSigDigits(Numbers(5), 2) + "] > " + cNumericFieldNames(6) + " [" +
                                  RoundSigDigits(Numbers(6), 2) + ']');
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Exponential Skew Normal

        // cpw22Aug2010 Loop over Sigmoid curves and load data
        CurrentModuleObject = "Curve:Sigmoid";
        for (CurveIndex = 1; CurveIndex <= NumSigmoid; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = Sigmoid;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Var1Min = Numbers(6);
            PerfCurve(CurveNum).Var1Max = Numbers(7);

            if (NumNumbers > 7 && !lNumericFieldBlanks(8)) {
                PerfCurve(CurveNum).CurveMin = Numbers(8);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 8 && !lNumericFieldBlanks(9)) {
                PerfCurve(CurveNum).CurveMax = Numbers(9);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(6) > Numbers(7)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(6) + '[' + RoundSigDigits(Numbers(6), 2) + "] > " + cNumericFieldNames(7) + " [" +
                                  RoundSigDigits(Numbers(7), 2) + ']');
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Sigmoid

        // cpw22Aug2010 Loop over Rectangular Hyperbola Type 1 curves and load data
        CurrentModuleObject = "Curve:RectangularHyperbola1";
        for (CurveIndex = 1; CurveIndex <= NumRectHyper1; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = RectangularHyperbola1;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Var1Min = Numbers(4);
            PerfCurve(CurveNum).Var1Max = Numbers(5);

            if (NumNumbers > 5 && !lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).CurveMin = Numbers(6);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).CurveMax = Numbers(7);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(4) + '[' + RoundSigDigits(Numbers(4), 2) + "] > " + cNumericFieldNames(5) + " [" +
                                  RoundSigDigits(Numbers(5), 2) + ']');
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Rectangular Hyperbola Type 1

        // cpw22Aug2010 Loop over Rectangular Hyperbola Type 2 curves and load data
        CurrentModuleObject = "Curve:RectangularHyperbola2";
        for (CurveIndex = 1; CurveIndex <= NumRectHyper2; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = RectangularHyperbola2;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Var1Min = Numbers(4);
            PerfCurve(CurveNum).Var1Max = Numbers(5);

            if (NumNumbers > 5 && !lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).CurveMin = Numbers(6);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).CurveMax = Numbers(7);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(4) + '[' + RoundSigDigits(Numbers(4), 2) + "] > " + cNumericFieldNames(5) + " [" +
                                  RoundSigDigits(Numbers(5), 2) + ']');
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Rectangular Hyperbola Type 2

        // cpw22Aug2010 Loop over Exponential Decay curves and load data
        CurrentModuleObject = "Curve:ExponentialDecay";
        for (CurveIndex = 1; CurveIndex <= NumExpDecay; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = ExponentialDecay;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Var1Min = Numbers(4);
            PerfCurve(CurveNum).Var1Max = Numbers(5);

            if (NumNumbers > 5 && !lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).CurveMin = Numbers(6);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 6 && !lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).CurveMax = Numbers(7);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Numbers(4) > Numbers(5)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(4) + '[' + RoundSigDigits(Numbers(4), 2) + "] > " + cNumericFieldNames(5) + " [" +
                                  RoundSigDigits(Numbers(5), 2) + ']');
                ErrorsFound = true;
            }

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Exponential Decay

        // ykt July,2011 Loop over DoubleExponential Decay curves and load data
        CurrentModuleObject = "Curve:DoubleExponentialDecay";
        for (CurveIndex = 1; CurveIndex <= NumDoubleExpDecay; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).CurveType = DoubleExponentialDecay;
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
            PerfCurve(CurveNum).Coeff1 = Numbers(1);
            PerfCurve(CurveNum).Coeff2 = Numbers(2);
            PerfCurve(CurveNum).Coeff3 = Numbers(3);
            PerfCurve(CurveNum).Coeff4 = Numbers(4);
            PerfCurve(CurveNum).Coeff5 = Numbers(5);
            PerfCurve(CurveNum).Var1Min = Numbers(6);
            PerfCurve(CurveNum).Var1Max = Numbers(7);

            if (NumNumbers > 7 && !lNumericFieldBlanks(8)) {
                PerfCurve(CurveNum).CurveMin = Numbers(8);
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (NumNumbers > 8 && !lNumericFieldBlanks(9)) {
                PerfCurve(CurveNum).CurveMax = Numbers(9);
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            //  IF (Numbers(4) > Numbers(5)) THEN  ! error
            //    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
            //    CALL ShowContinueError(TRIM(cNumericFieldNames(4))//'['//TRIM(RoundSigDigits(Numbers(4),2))//'] > '//  &
            //       TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//']')
            //    ErrorsFound=.TRUE.
            //  ENDIF

            if (NumAlphas >= 2) {
                if (!IsCurveInputTypeValid(Alphas(2))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Input Unit Type for X is invalid.");
                }
            }
            if (NumAlphas >= 3) {
                if (!IsCurveOutputTypeValid(Alphas(3))) {
                    ShowWarningError("In " + CurrentModuleObject + " named " + Alphas(1) + " the Output Unit Type is invalid.");
                }
            }
        } // Exponential Decay

        // Loop over wind pressure coefficient tables and load data
        if (NumWPCValTab > 0) {
            // Get the angle values
            CurrentModuleObject = "AirflowNetwork:MultiZone:WindPressureCoefficientArray";
            int numOfCPArray = inputProcessor->getNumObjectsFound(CurrentModuleObject);

            if (numOfCPArray != 1) {
                ShowSevereError("GetCurveInput: Currently exactly one (\"1\") " + CurrentModuleObject +
                                " object per simulation is required when using the AirflowNetwork model.");
                ErrorsFound = true;
            } else if (numOfCPArray == 1) {
                inputProcessor->getObjectItem(CurrentModuleObject,
                                              1,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericFieldBlanks,
                                              _,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

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
                            ShowSevereError("GetCurveInput: An " + CurrentModuleObject + " object ");
                            ShowContinueError("has either the same values for two consecutive wind directions, or a lower wind direction value after "
                                              "a higher wind direction value.");
                            ShowContinueError("Wind direction values must be entered in ascending order.");
                            ShowContinueError(cNumericFieldNames(j) + " = " + RoundSigDigits(windDirs[j - 2], 2) + ' ' + cNumericFieldNames[j + 1] +
                                              " = " + RoundSigDigits(windDirs[j - 1], 2));
                            ErrorsFound = true;
                        }
                    }
                }
                // Check that the first table value is zero
                if (dirMin != 0.0) {
                    ShowSevereError("GetCurveInput: An " + CurrentModuleObject + " object ");
                    ShowContinueError("has a nonzero minimum value of " + RoundSigDigits(dirMin, 2));
                    ShowContinueError("Wind direction values must begin at zero.");
                    ErrorsFound = true;
                }

                // Now that we have the directions, we can read the tables themselves
                CurrentModuleObject = "AirflowNetwork:MultiZone:WindPressureCoefficientValues";
                for (int index = 1; index <= NumWPCValTab; ++index) {
                    inputProcessor->getObjectItem(CurrentModuleObject,
                                                  index,
                                                  Alphas,
                                                  NumAlphas,
                                                  Numbers,
                                                  NumNumbers,
                                                  IOStatus,
                                                  lNumericFieldBlanks,
                                                  _,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                    ++CurveNum;
                    GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

                    // Ensure the CP array name should be the same as the name of AirflowNetwork:MultiZone:WindPressureCoefficientArray
                    if (!UtilityRoutines::SameString(Alphas(2), wpcName)) {
                        ShowSevereError("GetCurveInput: Invalid " + cAlphaFieldNames(2) + " = " + Alphas(2) + " in " + CurrentModuleObject + " = " +
                                        Alphas(1));
                        ShowContinueError("The valid name is " + wpcName);
                        ErrorsFound = true;
                    }

                    PerfCurve(CurveNum).Name = Alphas(1);
                    PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
                    PerfCurve(CurveNum).NumDims = 1;

                    PerfCurve(CurveNum).InterpolationType = BtwxtMethod;

                    std::string contextString = CurrentModuleObject + " \"" + Alphas(1) + "\"";
                    Btwxt::setMessageCallback(CurveManager::BtwxtMessageCallback, &contextString);

                    PerfCurve(CurveNum).Var1Min = 0.0;
                    PerfCurve(CurveNum).Var1MinPresent = true;
                    PerfCurve(CurveNum).Var1Max = 360.0;
                    PerfCurve(CurveNum).Var1MaxPresent = true;

                    PerfCurve(CurveNum).CurveMin = -1.0;
                    PerfCurve(CurveNum).CurveMinPresent = true;

                    PerfCurve(CurveNum).CurveMax = 1.0;
                    PerfCurve(CurveNum).CurveMaxPresent = true;

                    MaxTableNums = NumNumbers;
                    if (NumNumbers != numWindDir) {
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError("The number of data entries must match the number of wind directions given in the wind pressure "
                                          "coefficient array. Number of data entries = " +
                                          RoundSigDigits(NumNumbers));
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
                        gridAxes.emplace_back(axis, Btwxt::Method::LINEAR, Btwxt::Method::LINEAR, std::pair<double, double> {0.0, 360.0});

                        auto gridIndex = btwxtManager.addGrid(Alphas(1), Btwxt::GriddedData(gridAxes));
                        PerfCurve(CurveNum).TableIndex = gridIndex;
                        PerfCurve(CurveNum).GridValueIndex = btwxtManager.addOutputValues(gridIndex, lookupValues);
                    }
                }
            }
        }

        // Create case insensitive references to independent variable input data
        int numIndVars = inputProcessor->getNumObjectsFound("Table:IndependentVariable");
        if (numIndVars > 0) {
            // Set Btwxt Message Callback
            auto const &indVarInstances = inputProcessor->getObjectInstances("Table:IndependentVariable");
            for (auto &instance : indVarInstances.items()) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed("Table:IndependentVariable", thisObjectName);
                btwxtManager.independentVarRefs.emplace(UtilityRoutines::MakeUPPERCase(thisObjectName),fields);
            }
        }

        // Create GridSpaces from Independent Variable List
        int numIndVarLists = inputProcessor->getNumObjectsFound("Table:IndependentVariableList");
        std::map<std::string, std::vector<std::pair<double, double>>>
            varListLimits; // ugly, but this is needed for legacy behavior (otherwise limits are reset by Btwxt if they are within bounds).
        std::map<std::string, std::vector<double>> varListNormalizeTargets;
        if (numIndVarLists > 0) {
            auto const indVarListInstances = inputProcessor->getObjectInstances("Table:IndependentVariableList");
            for (auto instance : indVarListInstances.items()) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed("Table:IndependentVariableList", thisObjectName);
                std::string varListName = UtilityRoutines::MakeUPPERCase(thisObjectName);

                std::vector<Btwxt::GridAxis > gridAxes;

                // Loop through independent variables in list and add them to the grid
                for (auto indVar : fields.at("independent_variables")) {
                    std::string indVarName = UtilityRoutines::MakeUPPERCase(indVar.at("independent_variable_name"));
                    std::string contextString = "Table:IndependentVariable \"" + indVarName + "\"";
                    Btwxt::setMessageCallback(BtwxtMessageCallback, &contextString);

                    // Find independent variable input data
                    if (btwxtManager.independentVarRefs.count(indVarName)) {
                        // If found, read data
                        auto const &indVarInstance = btwxtManager.independentVarRefs.at(indVarName);

                        // TODO: Actually use this to define output variable units
                        if (indVarInstance.count("unit_type")) {
                            std::string unitType = indVarInstance.at("unit_type");
                            if (!IsCurveOutputTypeValid(unitType)) {
                                ShowSevereError(contextString + ": Unit Type [" + unitType + "] is invalid");
                            }
                        }

                        std::vector<double> axis;

                        if (indVarInstance.count("external_file_name")) {
                            std::string filePath = indVarInstance.at("external_file_name");
                            if (!indVarInstance.count("external_file_column_number")) {
                                ShowSevereError(contextString + ": No column number defined for external file \"" + filePath + "\"");
                                ErrorsFound = true;
                            }
                            if (!indVarInstance.count("external_file_starting_row_number")) {
                                ShowSevereError(contextString + ": No starting row number defined for external file \"" + filePath + "\"");
                                ErrorsFound = true;
                            }

                            std::size_t colNum = indVarInstance.at("external_file_column_number").get<std::size_t>() - 1;
                            std::size_t rowNum = indVarInstance.at("external_file_starting_row_number").get<std::size_t>() - 1;

                            if (!btwxtManager.tableFiles.count(filePath)) {
                                btwxtManager.tableFiles.emplace(filePath, TableFile{IOFiles::getSingleton(), filePath});
                            }

                            axis = btwxtManager.tableFiles[filePath].getArray({colNum, rowNum});

                            // remove NANs
                            axis.erase(std::remove_if(axis.begin(), axis.end(), [](const double &x){return std::isnan(x);}), axis.end());

                            // sort
                            std::sort(axis.begin(), axis.end());

                            // remove duplicates
                            axis.erase(std::unique(axis.begin(), axis.end()), axis.end());

                        } else if (indVarInstance.count("values")) {
                            for (auto value : indVarInstance.at("values")) {
                                axis.push_back(value.at("value"));
                            }
                        } else {
                            ShowSevereError(contextString + ": No values defined.");
                            ErrorsFound = true;
                        }

                        Btwxt::Method interpMethod, extrapMethod;
                        if (indVarInstance.count("interpolation_method")){
                            interpMethod = BtwxtManager::interpMethods.at(indVarInstance.at("interpolation_method"));
                        } else {
                            interpMethod = Btwxt::Method::CUBIC;
                        }

                        if (indVarInstance.count("extrapolation_method")) {
                            if (indVarInstance.at("extrapolation_method") == "Unavailable") {
                                ShowSevereError(contextString + ": Extrapolation method \"Unavailable\" is not yet available.");
                                ErrorsFound = true;
                            }
                            extrapMethod = BtwxtManager::extrapMethods.at(indVarInstance.at("extrapolation_method"));
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

                        varListLimits[varListName].push_back({min_val,max_val});

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

                        gridAxes.emplace_back(axis, extrapMethod, interpMethod, std::pair<double, double> {min_val, max_val});

                    } else {
                        // Independent variable does not exist
                        ShowSevereError(contextString + ": No Table:IndependentVariable found.");
                        ErrorsFound = true;
                    }

                }
                // Add grid to btwxtManager
                btwxtManager.addGrid(UtilityRoutines::MakeUPPERCase(thisObjectName), Btwxt::GriddedData(gridAxes));
            }
        }

        int numTblLookups = inputProcessor->getNumObjectsFound("Table:Lookup");
        if (numTblLookups > 0) {
            auto const lookupInstances = inputProcessor->getObjectInstances("Table:Lookup");
            for (auto instance : lookupInstances.items()) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed("Table:Lookup", thisObjectName);
                ++CurveNum;
                PerfCurve(CurveNum).Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                PerfCurve(CurveNum).ObjectType = "Table:Lookup";
                PerfCurve(CurveNum).InterpolationType = BtwxtMethod;

                std::string
                    indVarListName = UtilityRoutines::MakeUPPERCase(fields.at("independent_variable_list_name"));

                std::string contextString = "Table:Lookup \"" + PerfCurve(CurveNum).Name + "\"";
                Btwxt::setMessageCallback(BtwxtMessageCallback, &contextString);

                // TODO: Actually use this to define output variable units
                if (fields.count("output_unit_type")) {
                    std::string unitType = fields.at("output_unit_type");
                    if (!IsCurveOutputTypeValid(unitType)) {
                        ShowSevereError(contextString + ": Output Unit Type [" + unitType + "] is invalid");
                    }
                }

                int gridIndex = btwxtManager.getGridIndex(indVarListName, ErrorsFound);
                PerfCurve(CurveNum).TableIndex = gridIndex;
                int numDims = btwxtManager.getNumGridDims(gridIndex);
                PerfCurve(CurveNum).NumDims = numDims;

                for (int i = 1; i <= std::min(6, numDims); ++i) {
                    double vMin, vMax;
                    std::tie(vMin, vMax) = varListLimits.at(indVarListName)[i - 1];
                    if (i==1) {
                        PerfCurve(CurveNum).Var1Min = vMin;
                        PerfCurve(CurveNum).Var1Max = vMax;
                    } else if (i==2) {
                        PerfCurve(CurveNum).Var2Min = vMin;
                        PerfCurve(CurveNum).Var2Max = vMax;
                    } else if (i==3) {
                        PerfCurve(CurveNum).Var3Min = vMin;
                        PerfCurve(CurveNum).Var3Max = vMax;
                    } else if (i==4) {
                        PerfCurve(CurveNum).Var4Min = vMin;
                        PerfCurve(CurveNum).Var4Max = vMax;
                    } else if (i==5) {
                        PerfCurve(CurveNum).Var5Min = vMin;
                        PerfCurve(CurveNum).Var5Max = vMax;
                    } else if (i==6) {
                        PerfCurve(CurveNum).Var6Min = vMin;
                        PerfCurve(CurveNum).Var6Max = vMax;
                    }
                }

                if (fields.count("minimum_output")) {
                    PerfCurve(CurveNum).CurveMin = fields.at("minimum_output");
                    PerfCurve(CurveNum).CurveMinPresent = true;
                } else {
                    PerfCurve(CurveNum).CurveMin = -DBL_MAX;
                    PerfCurve(CurveNum).CurveMinPresent = false;
                }

                if (fields.count("maximum_output")) {
                    PerfCurve(CurveNum).CurveMax = fields.at("maximum_output");
                    PerfCurve(CurveNum).CurveMaxPresent = true;
                } else {
                    PerfCurve(CurveNum).CurveMax = DBL_MAX;
                    PerfCurve(CurveNum).CurveMaxPresent = false;
                }

                // Normalize data
                Real64 normalizationDivisor = 1.0;
                enum NormalizationMethod {NM_NONE, NM_DIVISOR_ONLY, NM_AUTO_WITH_DIVISOR};
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
                    std::string filePath = fields.at("external_file_name");

                    if (!fields.count("external_file_column_number")) {
                        ShowSevereError(contextString + ": No column number defined for external file \"" + filePath + "\"");
                        ErrorsFound = true;
                    }
                    if (!fields.count("external_file_starting_row_number")) {
                        ShowSevereError(contextString + ": No starting row number defined for external file \"" + filePath + "\"");
                        ErrorsFound = true;
                    }

                    std::size_t colNum = fields.at("external_file_column_number").get<std::size_t>() - 1;
                    std::size_t rowNum = fields.at("external_file_starting_row_number").get<std::size_t>() - 1;

                    if (!btwxtManager.tableFiles.count(filePath)) {
                        btwxtManager.tableFiles.emplace(filePath, TableFile{IOFiles::getSingleton(), filePath});
                    }

                    lookupValues = btwxtManager.tableFiles[filePath].getArray({colNum, rowNum});

                    // remove NANs
                    lookupValues.erase(std::remove_if(lookupValues.begin(), lookupValues.end(),  [](const double &x){return std::isnan(x);}), lookupValues.end());

                } else if (fields.count("values")) {
                    for (auto value : fields.at("values")) {
                        lookupValues.push_back(value.at("output_value").get<Real64>() / normalizationDivisor);
                    }
                } else {
                    ShowSevereError(contextString + ": No values defined.");
                    ErrorsFound = true;
                }

                PerfCurve(CurveNum).GridValueIndex = btwxtManager.addOutputValues(gridIndex, lookupValues);

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
                        ShowSevereError(
                            contextString +
                            ": Table is to be normalized using AutomaticWithDivisor, but not all independent variables define a normalization reference value. Make sure either:");
                        ShowContinueError("  Make sure either:");
                        ShowContinueError("    a) a normalization reference value is defined for each independent variable, or");
                        ShowContinueError("    b) no normalization reference values are defined.");
                        ErrorsFound = true;
                    } else if (pointsSpecified) {
                        // normalizeGridValues normalizes curve values to 1.0 at the normalization target, and returns the scalar needed to perform this normalization.
                        // The result is multiplied by the input normalizationDivisor again for the AutomaticWithDivisor case, in which normalizeGridValues returns a compound scalar.
                        normalizationDivisor = btwxtManager.normalizeGridValues(gridIndex, PerfCurve(CurveNum).GridValueIndex, normalizeTarget, normalizationDivisor)*normalizationDivisor;
                    }
                }

                if ((normalizeMethod == NM_DIVISOR_ONLY) || (normalizeMethod == NM_AUTO_WITH_DIVISOR)) {
                    if (PerfCurve(CurveNum).CurveMaxPresent) {
                        PerfCurve(CurveNum).CurveMax = PerfCurve(CurveNum).CurveMax / normalizationDivisor;
                    }
                    if (PerfCurve(CurveNum).CurveMinPresent) {
                        PerfCurve(CurveNum).CurveMin = PerfCurve(CurveNum).CurveMin / normalizationDivisor;
                    }
                }
            }
        }
        btwxtManager.tableFiles.clear();
    }

    int BtwxtManager::getGridIndex(std::string indVarListName, bool &ErrorsFound) {
        int gridIndex = -1;
        if (gridMap.count(indVarListName)) {
            gridIndex = gridMap.at(indVarListName);
        } else {
            // Independent variable list does not exist
            ShowSevereError("Table:Lookup \"" + indVarListName + "\" : No Table:IndependentVariableList found.");
            ErrorsFound = true;
        }
        return gridIndex;
    }

    std::pair<double, double> BtwxtManager::getGridAxisLimits(int gridIndex, int axisIndex) {
        return grids[gridIndex].get_axis_limits(axisIndex);
    }

    int BtwxtManager::addOutputValues(int gridIndex, std::vector<double> values)
    {
        return (int)grids[gridIndex].add_value_table(values);
    }

    int BtwxtManager::getNumGridDims(int gridIndex) {
        return (int)grids[gridIndex].get_ndims();
    }

    double BtwxtManager::getGridValue(int gridIndex, int outputIndex, const std::vector<double> target) {
        return grids[gridIndex](target)[outputIndex];
    }

    double BtwxtManager::normalizeGridValues(int gridIndex, int outputIndex, const std::vector<double> target, const double scalar) {
        return grids[gridIndex].normalize_values_at_target(outputIndex, target, scalar);
    }

    void BtwxtManager::clear() {
        grids.clear();
        gridMap.clear();
        independentVarRefs.clear();
        tableFiles.clear();
    }

    TableFile::TableFile(IOFiles &ioFiles, std::string path)
    {
        load(ioFiles, path);
    }

    void TableFile::load(IOFiles &ioFiles, std::string path)
    {
        filePath = path;
        bool fileFound;
        std::string fullPath;
        DataSystemVariables::CheckForActualFileName(ioFiles, path, fileFound, fullPath);
        if (!fileFound) {
            ShowFatalError("File \"" + filePath + "\" : File not found.");
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
    }

    std::vector<double>& TableFile::getArray(std::pair<std::size_t, std::size_t> colAndRow) {
        if (!arrays.count(colAndRow)) {
            // create the column from the data if it doesn't exist already
            std::size_t col = colAndRow.first;  // 0 indexed
            std::size_t row = colAndRow.second;  // 0 indexed
            auto &content = contents[col];
            if (col >= numColumns) {
                ShowFatalError("File \"" + filePath + "\" : Requested column (" + General::RoundSigDigits(col+1) + ") exceeds the number of columns (" + General::RoundSigDigits(numColumns) + ").");
            }
            if (row >= numRows) {
                ShowFatalError("File \"" + filePath + "\" : Requested starting row (" + General::RoundSigDigits(row+1) + ") exceeds the number of rows (" + General::RoundSigDigits(numRows) + ").");
            }
            std::vector<double> array(numRows - row);
            std::transform(content.begin() + row, content.end(), array.begin(), [](const std::string &str) {
                // Convert strings to double
                // see https://stackoverflow.com/a/16575025/1344457
                char *pEnd;
                double ret = std::strtod(&str[0], &pEnd);
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

    void InitCurveReporting()
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

        for (CurveIndex = 1; CurveIndex <= NumCurves; ++CurveIndex) {
            for (int dim = 1; dim <= PerfCurve(CurveIndex).NumDims; ++dim) {
                std::string numStr = std::to_string(dim);

                // TODO: Make CurveInput an Array for better looping here...
                switch (dim) {
                case 1:
                    SetupOutputVariable("Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        PerfCurve(CurveIndex).CurveInput1,
                                        "HVAC",
                                        "Average",
                                        PerfCurve(CurveIndex).Name);
                    break;
                case 2:
                    SetupOutputVariable("Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        PerfCurve(CurveIndex).CurveInput2,
                                        "HVAC",
                                        "Average",
                                        PerfCurve(CurveIndex).Name);
                    break;
                case 3:
                    SetupOutputVariable("Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        PerfCurve(CurveIndex).CurveInput3,
                                        "HVAC",
                                        "Average",
                                        PerfCurve(CurveIndex).Name);
                    break;
                case 4:
                    SetupOutputVariable("Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        PerfCurve(CurveIndex).CurveInput4,
                                        "HVAC",
                                        "Average",
                                        PerfCurve(CurveIndex).Name);
                    break;
                case 5:
                    SetupOutputVariable("Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        PerfCurve(CurveIndex).CurveInput5,
                                        "HVAC",
                                        "Average",
                                        PerfCurve(CurveIndex).Name);
                    break;
                case 6:
                    SetupOutputVariable("Performance Curve Input Variable " + numStr + " Value",
                                        OutputProcessor::Unit::None,
                                        PerfCurve(CurveIndex).CurveInput6,
                                        "HVAC",
                                        "Average",
                                        PerfCurve(CurveIndex).Name);
                    break;
                default:
                    // Warning?
                    break;
                }
            }
            // set the output up last so it shows up after the input in the csv file
            SetupOutputVariable("Performance Curve Output Value",
                                OutputProcessor::Unit::None,
                                PerfCurve(CurveIndex).CurveOutput,
                                "HVAC",
                                "Average",
                                PerfCurve(CurveIndex).Name);
        }

        for (CurveIndex = 1; CurveIndex <= NumPressureCurves; ++CurveIndex) {
            SetupOutputVariable("Performance Curve Input Variable 1 Value",
                                OutputProcessor::Unit::None,
                                PressureCurve(CurveIndex).CurveInput1,
                                "HVAC",
                                "Average",
                                PressureCurve(CurveIndex).Name);
            SetupOutputVariable("Performance Curve Input Variable 2 Value",
                                OutputProcessor::Unit::None,
                                PressureCurve(CurveIndex).CurveInput2,
                                "HVAC",
                                "Average",
                                PressureCurve(CurveIndex).Name);
            SetupOutputVariable("Performance Curve Input Variable 3 Value",
                                OutputProcessor::Unit::None,
                                PressureCurve(CurveIndex).CurveInput3,
                                "HVAC",
                                "Average",
                                PressureCurve(CurveIndex).Name);
            SetupOutputVariable("Performance Curve Output Value",
                                OutputProcessor::Unit::None,
                                PressureCurve(CurveIndex).CurveOutput,
                                "HVAC",
                                "Average",
                                PressureCurve(CurveIndex).Name);
        }

        if (AnyEnergyManagementSystemInModel) { // provide hook for possible EMS control
            for (CurveIndex = 1; CurveIndex <= NumCurves; ++CurveIndex) {
                SetupEMSActuator("Curve",
                                 PerfCurve(CurveIndex).Name,
                                 "Curve Result",
                                 "[unknown]",
                                 PerfCurve(CurveIndex).EMSOverrideOn,
                                 PerfCurve(CurveIndex).EMSOverrideCurveValue);
            } // All performance curves
        }
        if (AnyEnergyManagementSystemInModel) { // provide hook for possible EMS control
            for (CurveIndex = 1; CurveIndex <= NumPressureCurves; ++CurveIndex) {
                SetupEMSActuator("Curve",
                                 PressureCurve(CurveIndex).Name,
                                 "Curve Result",
                                 "[unknown]",
                                 PressureCurve(CurveIndex).EMSOverrideOn,
                                 PressureCurve(CurveIndex).EMSOverrideCurveValue);
            } // All pressure curves
        }
    }

    Real64 PerformanceCurveObject(int const CurveIndex,        // index of curve in curve array
                                  Real64 const Var1,           // 1st independent variable
                                  Optional<Real64 const> Var2, // 2nd independent variable
                                  Optional<Real64 const> Var3, // 3rd independent variable
                                  Optional<Real64 const> Var4  // 4th independent variable
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 CurveValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        static Real64 const sqrt_2_inv(1.0 / std::sqrt(2.0));

        Real64 CoeffZ1;         // cpw22Aug2010 Coefficient Z1 in exponential skew normal curve
        Real64 CoeffZ2;         // cpw22Aug2010 Coefficient Z2 in exponential skew normal curve
        Real64 CoeffZ3;         // cpw22Aug2010 Coefficient Z3 in exponential skew normal curve
        Real64 CurveValueNumer; // cpw22Aug2010 Numerator in in exponential skew normal curve
        Real64 CurveValueDenom; // cpw22Aug2010 Numerator in in exponential skew normal curve
        Real64 CurveValueExp;   // cpw22Aug2010 Exponential term in sigmoid curve
        auto const &Curve(PerfCurve(CurveIndex));

        Real64 const V1(max(min(Var1, Curve.Var1Max), Curve.Var1Min));                        // 1st independent variable after limits imposed
        Real64 const V2(Var2.present() ? max(min(Var2, Curve.Var2Max), Curve.Var2Min) : 0.0); // 2nd independent variable after limits imposed
        Real64 const V3(Var3.present() ? max(min(Var3, Curve.Var3Max), Curve.Var3Min) : 0.0); // 3rd independent variable after limits imposed
        Real64 const V4(Var4.present() ? max(min(Var4, Curve.Var4Max), Curve.Var4Min) : 0.0); // 4th independent variable after limits imposed

        {
            auto const SELECT_CASE_var(Curve.CurveType);
            if (SELECT_CASE_var == Linear) {
                CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2;
            } else if (SELECT_CASE_var == Quadratic) {
                CurveValue = Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * Curve.Coeff3);
            } else if (SELECT_CASE_var == QuadLinear) {
                CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2 + V2 * Curve.Coeff3 + V3 * Curve.Coeff4 + V4 * Curve.Coeff5;
            } else if (SELECT_CASE_var == Cubic) {
                CurveValue = Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * (Curve.Coeff3 + V1 * Curve.Coeff4));
            } else if (SELECT_CASE_var == Quartic) {
                CurveValue = Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * (Curve.Coeff3 + V1 * (Curve.Coeff4 + V1 * Curve.Coeff5)));
            } else if (SELECT_CASE_var == BiQuadratic) {
                CurveValue =
                    Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * Curve.Coeff3) + V2 * (Curve.Coeff4 + V2 * Curve.Coeff5) + V1 * V2 * Curve.Coeff6;
            } else if (SELECT_CASE_var == QuadraticLinear) {
                CurveValue = (Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * Curve.Coeff3)) + (Curve.Coeff4 + V1 * (Curve.Coeff5 + V1 * Curve.Coeff6)) * V2;
            } else if (SELECT_CASE_var == CubicLinear) {
                CurveValue = (Curve.Coeff1 + V1 * (Curve.Coeff2 + V1 * (Curve.Coeff3 + V1 * Curve.Coeff4))) + (Curve.Coeff5 + V1 * Curve.Coeff6) * V2;
            } else if (SELECT_CASE_var == BiCubic) {
                CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2 + V1 * V1 * Curve.Coeff3 + V2 * Curve.Coeff4 + V2 * V2 * Curve.Coeff5 +
                             V1 * V2 * Curve.Coeff6 + V1 * V1 * V1 * Curve.Coeff7 + V2 * V2 * V2 * Curve.Coeff8 + V1 * V1 * V2 * Curve.Coeff9 +
                             V1 * V2 * V2 * Curve.Coeff10;
            } else if (SELECT_CASE_var == ChillerPartLoadWithLift) {
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * V1 + Curve.Coeff3 * V1 * V1 + Curve.Coeff4 * V2 + Curve.Coeff5 * V2 * V2 +
                             Curve.Coeff6 * V1 * V2 + Curve.Coeff7 * V1 * V1 * V1 + Curve.Coeff8 * V2 * V2 * V2 + Curve.Coeff9 * V1 * V1 * V2 +
                             Curve.Coeff10 * V1 * V2 * V2 + Curve.Coeff11 * V1 * V1 * V2 * V2 + Curve.Coeff12 * V3 * V2 * V2 * V2;
            } else if (SELECT_CASE_var == TriQuadratic) {
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
            } else if (SELECT_CASE_var == Exponent) {
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::pow(V1, Curve.Coeff3);
            } else if (SELECT_CASE_var == FanPressureRise) { // cpw22Aug2010 Added Fan Pressure Rise curve
                CurveValue = V1 * (Curve.Coeff1 * V1 + Curve.Coeff2 + Curve.Coeff3 * std::sqrt(V2)) + Curve.Coeff4 * V2;
            } else if (SELECT_CASE_var == ExponentialSkewNormal) { // cpw22Aug2010 Added Exponential Skew Normal curve
                CoeffZ1 = (V1 - Curve.Coeff1) / Curve.Coeff2;
                CoeffZ2 = (Curve.Coeff4 * V1 * std::exp(Curve.Coeff3 * V1) - Curve.Coeff1) / Curve.Coeff2;
                CoeffZ3 = -Curve.Coeff1 / Curve.Coeff2;
                //    CurveValueNumer = EXP(-0.5d0 * CoeffZ1**2) * (1.0d0 + SIGN(1.0d0,CoeffZ2) * ErfFunction(ABS(CoeffZ2)/SQRT(2.0d0)))
                //    CurveValueDenom = EXP(-0.5d0 * CoeffZ3**2) * (1.0d0 + SIGN(1.0d0,CoeffZ3) * ErfFunction(ABS(CoeffZ3)/SQRT(2.0d0)))
                CurveValueNumer = std::exp(-0.5 * (CoeffZ1 * CoeffZ1)) * (1.0 + sign(1.0, CoeffZ2) * std::erf(std::abs(CoeffZ2) * sqrt_2_inv));
                CurveValueDenom = std::exp(-0.5 * (CoeffZ3 * CoeffZ3)) * (1.0 + sign(1.0, CoeffZ3) * std::erf(std::abs(CoeffZ3) * sqrt_2_inv));
                CurveValue = CurveValueNumer / CurveValueDenom;
            } else if (SELECT_CASE_var == Sigmoid) { // cpw22Aug2010 Added Sigmoid curve
                CurveValueExp = std::exp((Curve.Coeff3 - V1) / Curve.Coeff4);
                CurveValue = Curve.Coeff1 + Curve.Coeff2 / std::pow(1.0 + CurveValueExp, Curve.Coeff5);
            } else if (SELECT_CASE_var == RectangularHyperbola1) { // cpw22Aug2010 Added Rectangular Hyperbola Type 1 curve
                CurveValueNumer = Curve.Coeff1 * V1;
                CurveValueDenom = Curve.Coeff2 + V1;
                CurveValue = (CurveValueNumer / CurveValueDenom) + Curve.Coeff3;
            } else if (SELECT_CASE_var == RectangularHyperbola2) { // cpw22Aug2010 Added Rectangular Hyperbola Type 2 curve
                CurveValueNumer = Curve.Coeff1 * V1;
                CurveValueDenom = Curve.Coeff2 + V1;
                CurveValue = (CurveValueNumer / CurveValueDenom) + (Curve.Coeff3 * V1);
            } else if (SELECT_CASE_var == ExponentialDecay) { // cpw22Aug2010 Added Exponential Decay curve
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::exp(Curve.Coeff3 * V1);
            } else if (SELECT_CASE_var == DoubleExponentialDecay) { // ykt Jul 2011 Added Double Exponential Decay curve
                CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::exp(Curve.Coeff3 * V1) + Curve.Coeff4 * std::exp(Curve.Coeff5 * V1);
            } else {
                CurveValue = 0.0;
            }
        }

        if (Curve.CurveMinPresent) CurveValue = max(CurveValue, Curve.CurveMin);
        if (Curve.CurveMaxPresent) CurveValue = min(CurveValue, Curve.CurveMax);

        return CurveValue;
    }

    Real64 BtwxtTableInterpolation(int const CurveIndex,        // index of curve in curve array
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
      var = max(min(var, PerfCurve(CurveIndex).Var1Max), PerfCurve(CurveIndex).Var1Min);
      std::vector<double> target{var};
      if (present(Var2)) {
        var = Var2;
        var = max(min(var, PerfCurve(CurveIndex).Var2Max), PerfCurve(CurveIndex).Var2Min);
        target.push_back(var);
      }
      if (present(Var3)) {
        var = Var3;
        var = max(min(var, PerfCurve(CurveIndex).Var3Max), PerfCurve(CurveIndex).Var3Min);
        target.push_back(var);
      }
      if (present(Var4)) {
        var = Var4;
        var = max(min(var, PerfCurve(CurveIndex).Var4Max), PerfCurve(CurveIndex).Var4Min);
        target.push_back(var);
      }
      if (present(Var5)) {
        var = Var5;
        var = max(min(var, PerfCurve(CurveIndex).Var5Max), PerfCurve(CurveIndex).Var5Min);
        target.push_back(var);
      }
      if (present(Var6)) {
        var = Var6;
        var = max(min(var, PerfCurve(CurveIndex).Var6Max), PerfCurve(CurveIndex).Var6Min);
        target.push_back(var);
      }

      std::string contextString = "Table:Lookup \"" + PerfCurve(CurveIndex).Name + "\"";
      Btwxt::setMessageCallback(BtwxtMessageCallback, &contextString);
      Real64 TableValue = btwxtManager.getGridValue(PerfCurve(CurveIndex).TableIndex,PerfCurve(CurveIndex).GridValueIndex,target);

      if (PerfCurve(CurveIndex).CurveMinPresent) TableValue = max(TableValue, PerfCurve(CurveIndex).CurveMin);
      if (PerfCurve(CurveIndex).CurveMaxPresent) TableValue = min(TableValue, PerfCurve(CurveIndex).CurveMax);

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
            } else if (UtilityRoutines::SameString(InInputType, "PRESSURE")) { // cpw22Aug2010
                IsCurveInputTypeValid = true;                                  // cpw22Aug2010
                // CR8124 Glazer - Need to use volumetricflow and massflow not just flow
                //  ELSEIF (UtilityRoutines::SameString(InInputType,'FLOW')) THEN
                //    IsCurveInputTypeValid = .TRUE.
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
        } else if (UtilityRoutines::SameString(InOutputType, "PRESSURE")) { // cpw22Aug2010
            IsCurveOutputTypeValid = true;                                  // cpw22Aug2010
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

    bool CheckCurveDims(int const CurveIndex,
                        std::vector<int> validDims,
                        std::string routineName,
                        std::string objectType,
                        std::string objectName,
                        std::string curveFieldText)
    {
        // Returns true if errors found
        int curveDim = PerfCurve(CurveIndex).NumDims;
        if (std::find(validDims.begin(),validDims.end(), curveDim) != validDims.end()) {
            // Compatible
            return false;
        } else {
            // Not compatible
            ShowSevereError(routineName + objectType + "=\"" + objectName + "\"");
            ShowContinueError("...Invalid curve for " + curveFieldText + ".");
            std::string validString = std::to_string(validDims[0]);
            for (std::size_t i = 1; i < validDims.size(); i++) {
                validString += " or " + std::to_string(validDims[i]);
            }
            std::string plural1 = curveDim > 1 ? "s" : "";
            std::string plural2 = validDims[validDims.size()-1] > 1 ? "s" : "";
            ShowContinueError("...Input curve=\"" + PerfCurve(CurveIndex).Name + "\" has " + std::to_string(curveDim) + " dimension" + plural1 + ".");
            ShowContinueError("...Curve type must have " + validString + " dimension" + plural2 + ".");
            return true;
        }
    }

    std::string GetCurveName(int const CurveIndex) // index of curve in curve array
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse
        //       DATE WRITTEN   May 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given a curve index, returns the curve name

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        std::string GetCurveName;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na

        if (CurveIndex > 0) {
            GetCurveName = PerfCurve(CurveIndex).Name;
        } else {
            GetCurveName = "";
        }
        return GetCurveName;
    }

    int GetCurveIndex(std::string const &CurveName) // name of the curve
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
        if (GetCurvesInputFlag) {
            GetCurveInput();
            GetPressureSystemInput();
            GetCurvesInputFlag = false;
        }

        if (NumCurves > 0) {
            GetCurveIndex = UtilityRoutines::FindItemInList(CurveName, PerfCurve);
        } else {
            GetCurveIndex = 0;
        }

        return GetCurveIndex;
    }

    // This utility function grabs a curve index and performs the
    // error checking

    int GetCurveCheck(std::string const &alph, // curve name
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int GetCurveCheckOut;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na

        GetCurveCheckOut = GetCurveIndex(alph); // convert curve name to pointer
        if (GetCurveCheckOut == 0) {
            ShowSevereError("Curve Not Found for Object=\"" + ObjName + "\" :: " + alph);
            errFlag = true;
        }
        return GetCurveCheckOut;
    }

    void GetCurveMinMaxValues(int const CurveIndex,     // index of curve in curve array
                              Real64 &Var1Min,          // Minimum values of 1st independent variable
                              Real64 &Var1Max,          // Maximum values of 1st independent variable
                              Optional<Real64> Var2Min, // Minimum values of 2nd independent variable
                              Optional<Real64> Var2Max, // Maximum values of 2nd independent variable
                              Optional<Real64> Var3Min, // Minimum values of 2nd independent variable
                              Optional<Real64> Var3Max  // Maximum values of 2nd independent variable
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:
        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        Var1Min = PerfCurve(CurveIndex).Var1Min;
        Var1Max = PerfCurve(CurveIndex).Var1Max;
        if (present(Var2Min)) Var2Min = PerfCurve(CurveIndex).Var2Min;
        if (present(Var2Max)) Var2Max = PerfCurve(CurveIndex).Var2Max;
        if (present(Var3Min)) Var3Min = PerfCurve(CurveIndex).Var3Min;
        if (present(Var3Max)) Var3Max = PerfCurve(CurveIndex).Var3Max;
    }

    void SetCurveOutputMinMaxValues(int const CurveIndex,            // index of curve in curve array
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::TrimSigDigits;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        if (CurveIndex > 0 && CurveIndex <= NumCurves) {

            if (present(CurveMin)) {
                PerfCurve(CurveIndex).CurveMin = CurveMin;
                PerfCurve(CurveIndex).CurveMinPresent = true;
            }

            if (present(CurveMax)) {
                PerfCurve(CurveIndex).CurveMax = CurveMax;
                PerfCurve(CurveIndex).CurveMaxPresent = true;
            }

        } else {

            ShowSevereError("SetCurveOutputMinMaxValues: CurveIndex=[" + TrimSigDigits(CurveIndex) +
                            "] not in range of curves=[1:" + TrimSigDigits(NumCurves) + "].");
            ErrorsFound = true;
        }
    }

    void GetPressureSystemInput()
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
        using namespace DataIPShortCuts;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const CurveObjectName("Curve:Functional:PressureDrop");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumPressure;
        Array1D_string Alphas(1);     // Alpha items for object
        Array1D<Real64> Numbers(5);   // Numeric items for object
        int NumAlphas;                // Number of Alphas for each GetObjectItem call
        int NumNumbers;               // Number of Numbers for each GetObjectItem call
        int IOStatus;                 // Used in GetObjectItem
        bool ErrsFound(false); // Set to true if errors in input, fatal at end of routine
        int CurveNum;

        NumPressure = inputProcessor->getNumObjectsFound(CurveObjectName);
        PressureCurve.allocate(NumPressure);
        for (CurveNum = 1; CurveNum <= NumPressure; ++CurveNum) {
            inputProcessor->getObjectItem(CurveObjectName,
                                          CurveNum,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurveObjectName, cAlphaFieldNames(1), ErrsFound);
            PressureCurve(CurveNum).Name = Alphas(1);
            PressureCurve(CurveNum).EquivDiameter = Numbers(1);
            PressureCurve(CurveNum).MinorLossCoeff = Numbers(2);
            PressureCurve(CurveNum).EquivLength = Numbers(3);
            PressureCurve(CurveNum).EquivRoughness = Numbers(4);
            if (NumNumbers > 4 && !lNumericFieldBlanks(5)) {
                if (Numbers(5) != 0.0) {
                    PressureCurve(CurveNum).ConstantFPresent = true;
                    PressureCurve(CurveNum).ConstantF = Numbers(5);
                }
            }
        }

        NumPressureCurves = NumPressure;

        if (ErrsFound) {
            ShowFatalError("GetPressureCurveInput: Errors found in Curve Objects.  Preceding condition(s) cause termination.");
        }
    }

    void GetPressureCurveTypeAndIndex(std::string const &PressureCurveName, // name of the curve
                                      int &PressureCurveType,
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
        //  PressureCurve_Error       = pressure name was given, but curve is not available
        //  PressureCurve_None        = no pressure curve for this branch
        //  PressureCurve_Pressure    = pressure curve based on friction/minor loss
        //  PressureCurve_Generic     = curvemanager held curve which is function of flow rate

        // Using/Aliasing
        using DataBranchAirLoopPlant::PressureCurve_Error;
        using DataBranchAirLoopPlant::PressureCurve_Generic;
        using DataBranchAirLoopPlant::PressureCurve_None;
        using DataBranchAirLoopPlant::PressureCurve_Pressure;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int TempCurveIndex;
        bool FoundCurve;
        std::string GenericCurveType;

        // If input is not gotten, go ahead and get it now
        if (GetCurvesInputFlag) {
            GetCurveInput();
            GetPressureSystemInput();
            GetCurvesInputFlag = false;
        }

        // Initialize
        FoundCurve = false;
        PressureCurveType = PressureCurve_None;
        PressureCurveIndex = 0;

        // Try to retrieve a curve manager object
        TempCurveIndex = GetCurveIndex(PressureCurveName);

        // See if it is valid
        if (TempCurveIndex > 0) {
            // We have to check the type of curve to make sure it is single independent variable type
            GenericCurveType = PerfCurve(TempCurveIndex).ObjectType;
            {
                if (PerfCurve(TempCurveIndex).NumDims == 1) {
                    PressureCurveType = PressureCurve_Generic;
                    PressureCurveIndex = TempCurveIndex;
                } else {
                    ShowSevereError("Plant Pressure Simulation: Found error for curve: " + PressureCurveName);
                    ShowContinueError("Curve type detected: " + GenericCurveType);
                    ShowContinueError("Generic curves should be single independent variable such that DeltaP = f(mdot)");
                    ShowContinueError(" Therefore they should be of type: Linear, Quadratic, Cubic, Quartic, or Exponent");
                    ShowFatalError("Errors in pressure simulation input cause program termination");
                }
            }
            return;
        }

        // Then try to retrieve a pressure curve object
        if (allocated(PressureCurve)) {
            if (size(PressureCurve) > 0) {
                TempCurveIndex = UtilityRoutines::FindItemInList(PressureCurveName, PressureCurve);
            } else {
                TempCurveIndex = 0;
            }
        }

        // See if it is valid
        if (TempCurveIndex > 0) {
            PressureCurveType = PressureCurve_Pressure;
            PressureCurveIndex = TempCurveIndex;
            return;
        }

        // If we made it here, we didn't find either type of match

        // Last check, see if it is blank:
        if (PressureCurveName == "") {
            PressureCurveType = PressureCurve_None;
            return;
        }

        // At this point, we had a non-blank user entry with no match
        PressureCurveType = PressureCurve_Error;
        return;
    }

    Real64 PressureCurveValue(int const PressureCurveIndex, Real64 const MassFlow, Real64 const Density, Real64 const Viscosity)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::Pi;

        // Return value
        Real64 PressureCurveValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
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
        Diameter = PressureCurve(PressureCurveIndex).EquivDiameter;
        MinorLossCoeff = PressureCurve(PressureCurveIndex).MinorLossCoeff;
        Length = PressureCurve(PressureCurveIndex).EquivLength;
        Roughness = PressureCurve(PressureCurveIndex).EquivRoughness;
        IsConstFPresent = PressureCurve(PressureCurveIndex).ConstantFPresent;
        ConstantF = PressureCurve(PressureCurveIndex).ConstantF;

        // Intermediate calculations
        CrossSectArea = (Pi / 4.0) * pow_2(Diameter);
        Velocity = MassFlow / (Density * CrossSectArea);
        ReynoldsNumber = Density * Diameter * Velocity / Viscosity; // assuming mu here
        RoughnessRatio = Roughness / Diameter;

        // If we don't have any flow then exit out
        if (MassFlow < MassFlowTolerance) {
            PressureCurveValue = 0.0;
            PressureCurve(PressureCurveIndex).CurveInput1 = MassFlow;
            PressureCurve(PressureCurveIndex).CurveInput2 = Density;
            PressureCurve(PressureCurveIndex).CurveInput3 = Velocity;
            PressureCurve(PressureCurveIndex).CurveOutput = 0.0;
            return PressureCurveValue;
        }

        // Calculate the friction factor
        if (IsConstFPresent) { // use the constant value
            FrictionFactor = ConstantF;
        } else { // must calculate f
            FrictionFactor = CalculateMoodyFrictionFactor(ReynoldsNumber, RoughnessRatio);
        }

        // Pressure drop calculation
        PressureCurveValue = (FrictionFactor * (Length / Diameter) + MinorLossCoeff) * (Density * pow_2(Velocity)) / 2.0;

        if (PressureCurve(PressureCurveIndex).EMSOverrideOn) PressureCurveValue = PressureCurve(PressureCurveIndex).EMSOverrideCurveValue;

        PressureCurve(PressureCurveIndex).CurveInput1 = MassFlow;
        PressureCurve(PressureCurveIndex).CurveInput2 = Density;
        PressureCurve(PressureCurveIndex).CurveInput3 = Velocity;
        PressureCurve(PressureCurveIndex).CurveOutput = PressureCurveValue;

        return PressureCurveValue;
    }

    Real64 CalculateMoodyFrictionFactor(Real64 const ReynoldsNumber, Real64 const RoughnessRatio)
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
        using General::RoundSigDigits;

        // Return value
        Real64 CalculateMoodyFrictionFactor;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
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
            if (!FrictionFactorErrorHasOccurred) {
                RR = RoundSigDigits(RoughnessRatio, 7);
                Re = RoundSigDigits(ReynoldsNumber, 1);
                ShowSevereError("Plant Pressure System: Error in moody friction factor calculation");
                ShowContinueError("Current Conditions: Roughness Ratio=" + RR + "; Reynolds Number=" + Re);
                ShowContinueError("These conditions resulted in an unhandled numeric issue.");
                ShowContinueError("Please contact EnergyPlus support/development team to raise an alert about this issue");
                ShowContinueError("This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations");
                FrictionFactorErrorHasOccurred = true;
            }
            CalculateMoodyFrictionFactor = 0.04;
        }

        return CalculateMoodyFrictionFactor;
    }

    void checkCurveIsNormalizedToOne(std::string const callingRoutineObj, // calling routine with object type
                                     std::string const objectName,        // parent object where curve is used
                                     int const curveIndex,                // index to curve object
                                     std::string const cFieldName,        // object field name
                                     std::string const cFieldValue,       // user input curve name
                                     Real64 const Var1,                   // required 1st independent variable
                                     Optional<Real64 const> Var2,         // 2nd independent variable
                                     Optional<Real64 const> Var3,         // 3rd independent variable
                                     Optional<Real64 const> Var4,         // 4th independent variable
                                     Optional<Real64 const> Var5          // 5th independent variable
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
            CurveVal = CurveValue(curveIndex, Var1, Var2, Var3, Var4, Var5);
            if (CurveVal > 1.10 || CurveVal < 0.90) {
                ShowWarningError(callingRoutineObj + "=\"" + objectName + "\" curve values");
                ShowContinueError("... " + cFieldName + " = " + cFieldValue + " output is not equal to 1.0 (+ or - 10%) at rated conditions.");
                ShowContinueError("... Curve output at rated conditions = " + General::TrimSigDigits(CurveVal, 3));
            }
        }
    }

    //=================================================================================================!

} // namespace CurveManager

} // namespace EnergyPlus
