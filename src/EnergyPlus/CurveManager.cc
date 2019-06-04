// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSystemVariables.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <OutputProcessor.hh>
#include <UtilityRoutines.hh>

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
    //                       1) Merge TableData and TableLookup arrays. Care is needed here since the
    //                          Table:OneIndependentVariable (and Two) use different data patterns.
    //                          For Table:One - a one-to-one correspondence between X and Z
    //                          For Table:Multi - not a one-to-one correspondence between X and Z
    //                          Code does show examples of the translation so each Table object can use
    //                          either interpolation technique.
    //                       2) Subroutine PerformanceTableObject is not really needed (and is probably slower)
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
    int const LinearInterpolationOfTable(1);
    int const LagrangeInterpolationLinearExtrapolation(2);
    int const EvaluateCurveToLimits(3);

    // Data Format
    int const SINGLELINEINDEPENDENTVARIABLEWITHMATRIX(1);

    // Sort Order
    int const ASCENDING(1);
    int const DESCENDING(2);

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    int NumCurves(0);              // Autodesk Was used unintialized in InitCurveReporting
    bool GetCurvesInputFlag(true); // First time, input is "gotten"

    // SUBROUTINE SPECIFICATIONS FOR MODULE

    // Object Data
    Array1D<PerfomanceCurveData> PerfCurve;
    Array1D<PerfCurveTableDataStruct> PerfCurveTableData;
    Array1D<TableDataStruct> TableData;
    Array1D<TableDataStruct> TempTableData;
    Array1D<TableDataStruct> Temp2TableData;
    Array1D<TableLookupData> TableLookup;
    std::unordered_map<std::string, std::string> UniqueCurveNames;

    // Functions

    // Clears the global data in CurveManager.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        NumCurves = 0;
        GetCurvesInputFlag = true;
        UniqueCurveNames.clear();
        PerfCurve.deallocate();
        PerfCurveTableData.deallocate();
        TableData.deallocate();
        TempTableData.deallocate();
        Temp2TableData.deallocate();
        TableLookup.deallocate();
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

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        static bool MyBeginTimeStepFlag;

        // need to be careful on where and how resetting curve outputs to some "iactive value" is done
        // EMS can intercept curves and modify output
        if (BeginEnvrnFlag && MyBeginTimeStepFlag) {
            ResetPerformanceCurveOutput();
            MyBeginTimeStepFlag = false;
        }

        if (!BeginEnvrnFlag) {
            MyBeginTimeStepFlag = true;
        }

        if ((CurveIndex <= 0) || (CurveIndex > NumCurves)) {
            ShowFatalError("CurveValue: Invalid curve passed.");
        }

        {
            auto const SELECT_CASE_var(PerfCurve(CurveIndex).InterpolationType);
            if (SELECT_CASE_var == EvaluateCurveToLimits) {
                CurveValue = PerformanceCurveObject(CurveIndex, Var1, Var2, Var3);
            } else if (SELECT_CASE_var == LinearInterpolationOfTable) {
                CurveValue = PerformanceTableObject(CurveIndex, Var1, Var2, Var3);
            } else if (SELECT_CASE_var == LagrangeInterpolationLinearExtrapolation) {
                CurveValue = TableLookupObject(CurveIndex, Var1, Var2, Var3, Var4, Var5, Var6);
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
        int NumOneVarTab;                // Number of one variable table objects in the input file
        int NumWPCValTab;                // Number of wind pressure coefficient value table objects in the input file
        int NumTwoVarTab;                // Number of two variable table objects in the input file
        int NumChillerPartLoadWithLift;  // Number of ChillerPartLoadWithLift curve objects in the input data file
        int NumMultVarLookup;            // Number of multivariable tables
        int NumLookupTables;             // total number of one, two, and multivariable tables
        int NumFanPressRise;             // cpw22Aug2010 Number of fan pressure rise curve objects in the input file
        int NumExpSkewNorm;              // cpw22Aug2010 Number of exponential skew normal curve objects in the input file
        int NumSigmoid;                  // cpw22Aug2010 Number of sigmoid curve objects in the input file
        int NumRectHyper1;               // cpw22Aug2010 Number of rectangular hyperbola Type 1 curve objects in the input file
        int NumRectHyper2;               // cpw22Aug2010 Number of rectangular hyperbola Type 2 curve objects in the input file
        int NumExpDecay;                 // cpw22Aug2010 Number of exponential decay curve objects in the input file
        int NumDoubleExpDecay;           // ykt July 2011
        int NumTables;                   // Total tables in the input file
        int CurveIndex;                  // do loop index
        int CurveNum;                    // current curve number
        Array1D_string Alphas(14);       // Alpha items for object
        Array1D<Real64> Numbers(10000);  // Numeric items for object
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        int IOStatus;                    // Used in GetObjectItem
        std::string CurrentModuleObject; // for ease in renaming.
        static int MaxTableNums(0);      // Maximum number of numeric input fields in Tables
        static int MaxTableData(0);      // Maximum number of numeric input field pairs in Tables
        static int TotalArgs(0);         // Total number of alpha and numeric arguments (max) for a
        //   certain object in the input file
        static int TableNum(0);        // Index to TableData structure
        static int TableDataIndex(0);  // Loop counter for table data
        static int NumTableEntries(0); // Number of data pairs in table data
        int NumXVar;
        int NumX2Var;
        Array1D<Real64> XVar;
        Array1D<Real64> X2Var;
        int VarIndex;
        int TempVarIndex;
        int TempVarIndex1;
        Real64 MinTableData(999999.0);
        Real64 MaxTableDataValue;
        int NextXVar;
        bool FoundNewData;
        Array1D<Real64> TempArray1;
        Array1D<Real64> TempArray2;
        Array1D<Real64> TempArray3;

        std::string FileName; // name of external table data file
        bool ReadFromFile;    // True if external data file exists

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
        NumMultVarLookup = inputProcessor->getNumObjectsFound("Table:MultiVariableLookup");
        NumFanPressRise = inputProcessor->getNumObjectsFound("Curve:FanPressureRise");                    // cpw22Aug2010
        NumExpSkewNorm = inputProcessor->getNumObjectsFound("Curve:ExponentialSkewNormal");               // cpw22Aug2010
        NumSigmoid = inputProcessor->getNumObjectsFound("Curve:Sigmoid");                                 // cpw22Aug2010
        NumRectHyper1 = inputProcessor->getNumObjectsFound("Curve:RectangularHyperbola1");                // cpw22Aug2010
        NumRectHyper2 = inputProcessor->getNumObjectsFound("Curve:RectangularHyperbola2");                // cpw22Aug2010
        NumExpDecay = inputProcessor->getNumObjectsFound("Curve:ExponentialDecay");                       // cpw22Aug2010
        NumDoubleExpDecay = inputProcessor->getNumObjectsFound("Curve:DoubleExponentialDecay");           // ykt July 2011
        NumChillerPartLoadWithLift = inputProcessor->getNumObjectsFound("Curve:ChillerPartLoadWithLift"); // zrp_Aug2014

        NumOneVarTab = inputProcessor->getNumObjectsFound("Table:OneIndependentVariable");
        NumWPCValTab = inputProcessor->getNumObjectsFound("AirflowNetwork:MultiZone:WindPressureCoefficientValues");
        NumTwoVarTab = inputProcessor->getNumObjectsFound("Table:TwoIndependentVariables");

        NumCurves = NumBiQuad + NumCubic + NumQuad + NumQuadLinear + NumCubicLinear + NumLinear + NumBicubic + NumTriQuad + NumExponent + NumQuartic +
                    NumOneVarTab + NumTwoVarTab + NumMultVarLookup + NumFanPressRise + NumExpSkewNorm + NumSigmoid + NumRectHyper1 + NumRectHyper2 +
                    NumExpDecay + NumDoubleExpDecay + NumQLinear + NumChillerPartLoadWithLift + NumWPCValTab;

        // intermediate count for one and two variable performance tables
        NumTables = NumOneVarTab + NumTwoVarTab + NumWPCValTab;
        // final count for all tables
        NumLookupTables = NumOneVarTab + NumTwoVarTab + NumMultVarLookup + NumWPCValTab;
        if (NumLookupTables > 0) TableLookup.allocate(NumLookupTables);

        if (NumOneVarTab > 0) {
            inputProcessor->getObjectDefMaxArgs("Table:OneIndependentVariable", TotalArgs, NumAlphas, NumNumbers);
            MaxTableNums = max(MaxTableNums, NumNumbers);
            MaxTableData = max(MaxTableData, MaxTableNums);
        }
        if (NumWPCValTab > 0) {
            inputProcessor->getObjectDefMaxArgs("AirflowNetwork:MultiZone:WindPressureCoefficientValues", TotalArgs, NumAlphas, NumNumbers);
            MaxTableNums = max(MaxTableNums, NumNumbers);
            MaxTableData = max(MaxTableData, MaxTableNums);
        }
        if (NumTwoVarTab > 0) {
            inputProcessor->getObjectDefMaxArgs("Table:TwoIndependentVariables", TotalArgs, NumAlphas, NumNumbers);
            MaxTableNums = max(MaxTableNums, NumNumbers);
            MaxTableData = max(MaxTableData, MaxTableNums);
        }

        // allocate the data structure
        PerfCurve.allocate(NumCurves);
        UniqueCurveNames.reserve(NumCurves);
        PerfCurveTableData.allocate(NumLookupTables);
        TableData.allocate(NumLookupTables);
        TempTableData.allocate(NumTables);
        Temp2TableData.allocate(NumTables);
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
        TableNum = 0;

        // Loop over one variable tables and load data
        CurrentModuleObject = "Table:OneIndependentVariable";
        for (CurveIndex = 1; CurveIndex <= NumOneVarTab; ++CurveIndex) {
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
            ++TableNum;
            NumTableEntries = (NumNumbers - 5) / 2;
            TableData(TableNum).X1.allocate(NumTableEntries);
            TableData(TableNum).Y.allocate(NumTableEntries);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 1;
            PerfCurve(CurveNum).TableIndex = TableNum;
            {
                auto const SELECT_CASE_var(Alphas(2));
                if (SELECT_CASE_var == "LINEAR") {
                    PerfCurve(CurveNum).CurveType = Linear;
                    TableLookup(TableNum).InterpolationOrder = 2;
                } else if (SELECT_CASE_var == "QUADRATIC") {
                    PerfCurve(CurveNum).CurveType = Quadratic;
                    TableLookup(TableNum).InterpolationOrder = 3;
                } else if (SELECT_CASE_var == "CUBIC") {
                    PerfCurve(CurveNum).CurveType = Cubic;
                    TableLookup(TableNum).InterpolationOrder = 4;
                } else if (SELECT_CASE_var == "QUARTIC") {
                    PerfCurve(CurveNum).CurveType = Quartic;
                    TableLookup(TableNum).InterpolationOrder = 5;
                } else {
                    if (Alphas(3) == "EVALUATECURVETOLIMITS") {
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cAlphaFieldNames(2) + " [" + Alphas(2) + "] is not a valid choice. ");
                        ErrorsFound = true;
                    }
                }
            }

            {
                auto const SELECT_CASE_var(Alphas(3));
                if (SELECT_CASE_var == "LINEARINTERPOLATIONOFTABLE") {
                    PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;
                } else if (SELECT_CASE_var == "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION") {
                    PerfCurve(CurveNum).InterpolationType = LagrangeInterpolationLinearExtrapolation;
                } else if (SELECT_CASE_var == "EVALUATECURVETOLIMITS") {
                    PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
                } else {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(2) + " [" + Alphas(2) + "] is not a valid choice. ");
                    ErrorsFound = true;
                }
            }

            if (lNumericFieldBlanks(1)) {
                PerfCurve(CurveNum).Var1Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var1Min = Numbers(1);
                PerfCurve(CurveNum).Var1MinPresent = true;
            }
            if (lNumericFieldBlanks(2)) {
                PerfCurve(CurveNum).Var1Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var1Max = Numbers(2);
                PerfCurve(CurveNum).Var1MaxPresent = true;
            }

            if (Numbers(1) > Numbers(2)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(1) + " [" + RoundSigDigits(Numbers(1), 2) + "] > " + cNumericFieldNames(2) + " [" +
                                  RoundSigDigits(Numbers(2), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 4) {
                if (!IsCurveInputTypeValid(Alphas(4))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(4) + " [" + Alphas(4) + "] is invalid");
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveOutputTypeValid(Alphas(5))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(5) + " [" + Alphas(5) + "] is invalid");
                }
            }

            // read this value first to allow normalization of min/max table output fields
            if (!lNumericFieldBlanks(5)) {
                TableData(TableNum).NormalPoint = Numbers(5);
                if (Numbers(5) == 0.0) {
                    ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError("..." + cNumericFieldNames(5) + " [" + RoundSigDigits(Numbers(5), 6) + "] is not a valid choice.");
                    ShowContinueError("...Setting Normalization Reference to 1 and the simulation continues.");
                    TableData(TableNum).NormalPoint = 1.0;
                }
            } else {
                TableData(TableNum).NormalPoint = 1.0;
            }

            if (!lNumericFieldBlanks(3)) {
                PerfCurve(CurveNum).CurveMin = Numbers(3) / TableData(TableNum).NormalPoint;
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (!lNumericFieldBlanks(4)) {
                PerfCurve(CurveNum).CurveMax = Numbers(4) / TableData(TableNum).NormalPoint;
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            MaxTableNums = (NumNumbers - 5) / 2;
            if (mod((NumNumbers - 5), 2) != 0) {
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError("The number of data entries must be evenly divisable by 2. Number of data entries = " +
                                  RoundSigDigits(NumNumbers - 5));
                ErrorsFound = true;
            } else {
                for (TableDataIndex = 1; TableDataIndex <= MaxTableNums; ++TableDataIndex) {
                    TableData(TableNum).X1(TableDataIndex) = Numbers((TableDataIndex - 1) * 2 + 5 + 1);
                    TableData(TableNum).Y(TableDataIndex) = Numbers((TableDataIndex - 1) * 2 + 5 + 2) / TableData(TableNum).NormalPoint;
                }
            }

            // convert raw table data to multidimensional array
            // find number of x variables
            NumXVar = 1;
            NextXVar = 1;
            TempTableData = TableData;
            while (NumXVar <= MaxTableNums) {

                MinTableData = minval(TempTableData(TableNum).X1);
                for (VarIndex = 1; VarIndex <= MaxTableNums; ++VarIndex) {
                    if (TempTableData(TableNum).X1(VarIndex) == MinTableData) {
                        TableData(TableNum).X1(NumXVar) = TempTableData(TableNum).X1(VarIndex);
                        TableData(TableNum).Y(NumXVar) = TempTableData(TableNum).Y(VarIndex);
                        TempTableData(TableNum).X1(VarIndex) = 999999.0;
                        ++NumXVar;
                    }
                }

                NextXVar = NumXVar;
            }

            // move table data to performance curve table data structure
            PerfCurveTableData(TableNum).X1.allocate(NumXVar - 1);
            PerfCurveTableData(TableNum).Y.allocate(1, NumXVar - 1);
            PerfCurveTableData(TableNum).X1 = TableData(TableNum).X1;
            for (VarIndex = 1; VarIndex <= NumXVar - 1; ++VarIndex) {
                PerfCurveTableData(TableNum).Y(1, VarIndex) = TableData(TableNum).Y(VarIndex);
            }

            // create curve objects when regression analysis is required
            if (PerfCurve(CurveNum).InterpolationType == EvaluateCurveToLimits) {
                {
                    auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                    if ((SELECT_CASE_var == Linear) || (SELECT_CASE_var == Quadratic) || (SELECT_CASE_var == Cubic) || (SELECT_CASE_var == Quartic)) {
                        TempArray1 = PerfCurveTableData(TableNum).X1;
                        TempArray2.allocate(size(PerfCurveTableData(TableNum).Y));
                        for (VarIndex = 1; VarIndex <= isize(PerfCurveTableData(TableNum).Y); ++VarIndex) {
                            TempArray2(VarIndex) = PerfCurveTableData(TableNum).Y(1, VarIndex);
                        }
                        SolveRegression(CurveNum, CurrentModuleObject, PerfCurve(CurveNum).Name, TempArray1, TempArray2);
                        TempArray1.deallocate();
                        TempArray2.deallocate();
                    } else {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError("The requested regression analysis is not available at this time. Curve type = " + Alphas(2));
                        PerfCurve(CurveIndex).InterpolationType = LinearInterpolationOfTable;
                    }
                }
                if (!PerfCurve(CurveNum).Var1MinPresent) {
                    PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                }
                if (!PerfCurve(CurveNum).Var1MaxPresent) {
                    PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                }
            }

            // if user enters limits that exceed data range, warn that limits are based on table data
            if (PerfCurve(CurveNum).InterpolationType == LinearInterpolationOfTable) {
                if (PerfCurve(CurveNum).Var1MinPresent) {
                    if (PerfCurve(CurveNum).Var1Min < minval(TableData(TableNum).X1)) {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cNumericFieldNames(1) + " exceeds the data range and will not be used.");
                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(1), 6) +
                                          ", Minimum data range = " + RoundSigDigits(minval(TableData(TableNum).X1), 6));
                        PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                    }
                } else {
                    PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                }
                if (PerfCurve(CurveNum).Var1MaxPresent) {
                    if (PerfCurve(CurveNum).Var1Max > maxval(TableData(TableNum).X1)) {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cNumericFieldNames(2) + " exceeds the data range and will not be used.");
                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(2), 6) +
                                          ", Maximum data range = " + RoundSigDigits(maxval(TableData(TableNum).X1), 6));
                        PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                    }
                } else {
                    PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                }
            }

            // if user does not enter limits, set to min/max in table
            if (PerfCurve(CurveNum).InterpolationType == LagrangeInterpolationLinearExtrapolation) {
                if (!PerfCurve(CurveNum).Var1MinPresent) {
                    PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                }
                if (!PerfCurve(CurveNum).Var1MaxPresent) {
                    PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                }
            }

            // move table data to more compact array to allow interpolation using multivariable lookup table method
            TableLookup(TableNum).NumX1Vars = size(PerfCurveTableData(TableNum).X1);
            TableLookup(TableNum).X1Var.allocate(TableLookup(TableNum).NumX1Vars);
            TableLookup(TableNum).TableLookupZData.allocate(1, 1, 1, 1, 1, size(PerfCurveTableData(TableNum).Y));
            TableLookup(TableNum).X1Var = PerfCurveTableData(TableNum).X1;
            TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, 1, _) = PerfCurveTableData(TableNum).Y(1, _);
        }

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
                    ++TableNum;
                    NumTableEntries = NumNumbers;
                    TableData(TableNum).X1.allocate(NumTableEntries + 1);
                    TableData(TableNum).Y.allocate(NumTableEntries + 1);
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
                    PerfCurve(CurveNum).TableIndex = TableNum;
                    PerfCurve(CurveNum).CurveType = Linear;
                    TableLookup(TableNum).InterpolationOrder = 2;

                    PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;

                    PerfCurve(CurveNum).Var1Min = 0.0;
                    PerfCurve(CurveNum).Var1MinPresent = true;
                    PerfCurve(CurveNum).Var1Max = 360.0;
                    PerfCurve(CurveNum).Var1MaxPresent = true;

                    TableData(TableNum).NormalPoint = 1.0;

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
                        for (TableDataIndex = 1; TableDataIndex <= MaxTableNums; ++TableDataIndex) {
                            TableData(TableNum).X1(TableDataIndex) = windDirs[TableDataIndex - 1];
                            TableData(TableNum).Y(TableDataIndex) = Numbers(TableDataIndex);
                        }
                        TableData(TableNum).X1(MaxTableNums + 1) = 360.0;
                        TableData(TableNum).Y(MaxTableNums + 1) = Numbers(1);
                    }

                    // Convert raw table data to multidimensional array
                    // Find number of x variables
                    MaxTableNums += 1;
                    NumXVar = 1;
                    NextXVar = 1;
                    TempTableData = TableData;
                    while (NumXVar <= MaxTableNums) {

                        MinTableData = minval(TempTableData(TableNum).X1);
                        for (VarIndex = 1; VarIndex <= MaxTableNums; ++VarIndex) {
                            if (TempTableData(TableNum).X1(VarIndex) == MinTableData) {
                                TableData(TableNum).X1(NumXVar) = TempTableData(TableNum).X1(VarIndex);
                                TableData(TableNum).Y(NumXVar) = TempTableData(TableNum).Y(VarIndex);
                                TempTableData(TableNum).X1(VarIndex) = 999999.0;
                                ++NumXVar;
                            }
                        }

                        NextXVar = NumXVar;
                    }

                    // Move table data to performance curve table data structure
                    PerfCurveTableData(TableNum).X1.allocate(NumXVar - 1);
                    PerfCurveTableData(TableNum).Y.allocate(1, NumXVar - 1);
                    PerfCurveTableData(TableNum).X1 = TableData(TableNum).X1;
                    for (VarIndex = 1; VarIndex <= NumXVar - 1; ++VarIndex) {
                        PerfCurveTableData(TableNum).Y(1, VarIndex) = TableData(TableNum).Y(VarIndex);
                    }

                    // move table data to more compact array to allow interpolation using multivariable lookup table method
                    TableLookup(TableNum).NumX1Vars = size(PerfCurveTableData(TableNum).X1);
                    TableLookup(TableNum).X1Var.allocate(TableLookup(TableNum).NumX1Vars);
                    TableLookup(TableNum).TableLookupZData.allocate(1, 1, 1, 1, 1, size(PerfCurveTableData(TableNum).Y));
                    TableLookup(TableNum).X1Var = PerfCurveTableData(TableNum).X1;
                    TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, 1, _) = PerfCurveTableData(TableNum).Y(1, _);
                }
            }
        }
        /*
        for (CurveIndex = 1; CurveIndex <= NumOneVarTab; ++CurveIndex) {
                inputProcessor->getObjectItem(CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks,
        _, cAlphaFieldNames, cNumericFieldNames);
                ++CurveNum;
                ++TableNum;
                NumTableEntries = (NumNumbers - 5) / 2;
                TableData(TableNum).X1.allocate(NumTableEntries);
                TableData(TableNum).Y.allocate(NumTableEntries);
                IsNotOK = false;
                IsBlank = false;
                VerifyName(Alphas(1), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name");
                if (IsNotOK) {
                        ErrorsFound = true;
                        if (IsBlank) Alphas(1) = "xxxxx";
                }
                // Need to verify that this name isn't used in Pressure Curves as well.
                if (NumPressureCurves > 0) {
                        CurveFound = UtilityRoutines::FindItemInList(Alphas(1), PressureCurve);
                        if (CurveFound != 0) {
                                ShowSevereError("GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas(1) + "\", duplicate curve name.");
                                ShowContinueError("...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.");
                                ErrorsFound = true;
                        }
                }
                PerfCurve(CurveNum).Name = Alphas(1);
                PerfCurve(CurveNum).ObjectType = CurveType_TableOneIV;
                PerfCurve(CurveNum).TableIndex = TableNum;
                { auto const SELECT_CASE_var(Alphas(2));
                if (SELECT_CASE_var == "LINEAR") {
                        PerfCurve(CurveNum).CurveType = Linear;
                        TableLookup(TableNum).InterpolationOrder = 2;
                }
                else if (SELECT_CASE_var == "QUADRATIC") {
                        PerfCurve(CurveNum).CurveType = Quadratic;
                        TableLookup(TableNum).InterpolationOrder = 3;
                }
                else if (SELECT_CASE_var == "CUBIC") {
                        PerfCurve(CurveNum).CurveType = Cubic;
                        TableLookup(TableNum).InterpolationOrder = 4;
                }
                else if (SELECT_CASE_var == "QUARTIC") {
                        PerfCurve(CurveNum).CurveType = Quartic;
                        TableLookup(TableNum).InterpolationOrder = 5;
                }
                else if (SELECT_CASE_var == "EXPONENT") {
                        PerfCurve(CurveNum).CurveType = Exponent;
                        TableLookup(TableNum).InterpolationOrder = 4;
                }
                else {
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cAlphaFieldNames(2) + " [" + Alphas(2) + "] is not a valid choice. ");
                        ErrorsFound = true;
                }}

                { auto const SELECT_CASE_var(Alphas(3));
                if (SELECT_CASE_var == "LINEARINTERPOLATIONOFTABLE") {
                        PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;
                }
                else if (SELECT_CASE_var == "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION") {
                        PerfCurve(CurveNum).InterpolationType = LagrangeInterpolationLinearExtrapolation;
                }
                else if (SELECT_CASE_var == "EVALUATECURVETOLIMITS") {
                        PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
                }
                else {
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cAlphaFieldNames(2) + " [" + Alphas(2) + "] is not a valid choice. ");
                        ErrorsFound = true;
                }}

                if (lNumericFieldBlanks(1)) {
                        PerfCurve(CurveNum).Var1Min = -99999999999.0;
                }
                else {
                        PerfCurve(CurveNum).Var1Min = Numbers(1);
                        PerfCurve(CurveNum).Var1MinPresent = true;
                }
                if (lNumericFieldBlanks(2)) {
                        PerfCurve(CurveNum).Var1Max = 99999999999.0;
                }
                else {
                        PerfCurve(CurveNum).Var1Max = Numbers(2);
                        PerfCurve(CurveNum).Var1MaxPresent = true;
                }

                if (Numbers(1) > Numbers(2)) { // error
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cNumericFieldNames(1) + " [" + RoundSigDigits(Numbers(1), 2) + "] > " + cNumericFieldNames(2) + " [" +
        RoundSigDigits(Numbers(2), 2) + ']'); ErrorsFound = true;
                }
                if (NumAlphas >= 4) {
                        if (!IsCurveInputTypeValid(Alphas(4))) {
                                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                                ShowContinueError(cAlphaFieldNames(4) + " [" + Alphas(4) + "] is invalid");
                        }
                }
                if (NumAlphas >= 5) {
                        if (!IsCurveOutputTypeValid(Alphas(5))) {
                                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                                ShowContinueError(cAlphaFieldNames(5) + " [" + Alphas(5) + "] is invalid");
                        }
                }

                // read this value first to allow normalization of min/max table output fields
                if (!lNumericFieldBlanks(5)) {
                        TableData(TableNum).NormalPoint = Numbers(5);
                        if (Numbers(5) == 0.0) {
                                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                                ShowContinueError("..." + cNumericFieldNames(5) + " [" + RoundSigDigits(Numbers(5), 6) + "] is not a valid choice.");
                                ShowContinueError("...Setting Normalization Reference to 1 and the simulation continues.");
                                TableData(TableNum).NormalPoint = 1.0;
                        }
                }
                else {
                        TableData(TableNum).NormalPoint = 1.0;
                }

                if (!lNumericFieldBlanks(3)) {
                        PerfCurve(CurveNum).CurveMin = Numbers(3) / TableData(TableNum).NormalPoint;
                        PerfCurve(CurveNum).CurveMinPresent = true;
                }
                if (!lNumericFieldBlanks(4)) {
                        PerfCurve(CurveNum).CurveMax = Numbers(4) / TableData(TableNum).NormalPoint;
                        PerfCurve(CurveNum).CurveMaxPresent = true;
                }

                MaxTableNums = (NumNumbers - 5) / 2;
                if (mod((NumNumbers - 5), 2) != 0) {
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError("The number of data entries must be evenly divisable by 2. Number of data entries = " +
        RoundSigDigits(NumNumbers - 5)); ErrorsFound = true;
                }
                else {
                        for (TableDataIndex = 1; TableDataIndex <= MaxTableNums; ++TableDataIndex) {
                                TableData(TableNum).X1(TableDataIndex) = Numbers((TableDataIndex - 1) * 2 + 5 + 1);
                                TableData(TableNum).Y(TableDataIndex) = Numbers((TableDataIndex - 1) * 2 + 5 + 2) / TableData(TableNum).NormalPoint;
                        }
                }

                // convert raw table data to multidimensional array
                // find number of x variables
                NumXVar = 1;
                NextXVar = 1;
                TempTableData = TableData;
                while (NumXVar <= MaxTableNums) {

                        MinTableData = minval(TempTableData(TableNum).X1);
                        for (VarIndex = 1; VarIndex <= MaxTableNums; ++VarIndex) {
                                if (TempTableData(TableNum).X1(VarIndex) == MinTableData) {
                                        TableData(TableNum).X1(NumXVar) = TempTableData(TableNum).X1(VarIndex);
                                        TableData(TableNum).Y(NumXVar) = TempTableData(TableNum).Y(VarIndex);
                                        TempTableData(TableNum).X1(VarIndex) = 999999.0;
                                        ++NumXVar;
                                }
                        }

                        NextXVar = NumXVar;

                }

                // move table data to performance curve table data structure
                PerfCurveTableData(TableNum).X1.allocate(NumXVar - 1);
                PerfCurveTableData(TableNum).Y.allocate(1, NumXVar - 1);
                PerfCurveTableData(TableNum).X1 = TableData(TableNum).X1;
                for (VarIndex = 1; VarIndex <= NumXVar - 1; ++VarIndex) {
                        PerfCurveTableData(TableNum).Y(1, VarIndex) = TableData(TableNum).Y(VarIndex);
                }

                // create curve objects when regression analysis is required
                if (PerfCurve(CurveNum).InterpolationType == EvaluateCurveToLimits) {
                        { auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                        if ((SELECT_CASE_var == Linear) || (SELECT_CASE_var == Quadratic) || (SELECT_CASE_var == Cubic) || (SELECT_CASE_var ==
        Quartic) || (SELECT_CASE_var == Exponent)) { TempArray1 = PerfCurveTableData(TableNum).X1;
                                TempArray2.allocate(size(PerfCurveTableData(TableNum).Y));
                                for (VarIndex = 1; VarIndex <= isize(PerfCurveTableData(TableNum).Y); ++VarIndex) {
                                        TempArray2(VarIndex) = PerfCurveTableData(TableNum).Y(1, VarIndex);
                                }
                                SolveRegression(CurveNum, CurrentModuleObject, PerfCurve(CurveNum).Name, TempArray1, TempArray2);
                                TempArray1.deallocate();
                                TempArray2.deallocate();
                        }
                        else {
                                ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                                ShowContinueError("The requested regression analysis is not available at this time. Curve type = " + Alphas(2));
                                PerfCurve(CurveIndex).InterpolationType = LinearInterpolationOfTable;
                        }}
                        if (!PerfCurve(CurveNum).Var1MinPresent) {
                                PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                        }
                        if (!PerfCurve(CurveNum).Var1MaxPresent) {
                                PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                        }
                }

                // if user enters limits that exceed data range, warn that limits are based on table data
                if (PerfCurve(CurveNum).InterpolationType == LinearInterpolationOfTable) {
                        if (PerfCurve(CurveNum).Var1MinPresent) {
                                if (PerfCurve(CurveNum).Var1Min < minval(TableData(TableNum).X1)) {
                                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                                        ShowContinueError(cNumericFieldNames(1) + " exceeds the data range and will not be used.");
                                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(1), 6) + ", Minimum data range = " +
        RoundSigDigits(minval(TableData(TableNum).X1), 6)); PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                                }
                        }
                        else {
                                PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                        }
                        if (PerfCurve(CurveNum).Var1MaxPresent) {
                                if (PerfCurve(CurveNum).Var1Max > maxval(TableData(TableNum).X1)) {
                                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                                        ShowContinueError(cNumericFieldNames(2) + " exceeds the data range and will not be used.");
                                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(2), 6) + ", Maximum data range = " +
        RoundSigDigits(maxval(TableData(TableNum).X1), 6)); PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                                }
                        }
                        else {
                                PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                        }
                }

                // if user does not enter limits, set to min/max in table
                if (PerfCurve(CurveNum).InterpolationType == LagrangeInterpolationLinearExtrapolation) {
                        if (!PerfCurve(CurveNum).Var1MinPresent) {
                                PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                        }
                        if (!PerfCurve(CurveNum).Var1MaxPresent) {
                                PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                        }
                }

                // move table data to more compact array to allow interpolation using multivariable lookup table method
                TableLookup(TableNum).NumIndependentVars = 1;
                TableLookup(TableNum).NumX1Vars = size(PerfCurveTableData(TableNum).X1);
                TableLookup(TableNum).X1Var.allocate(TableLookup(TableNum).NumX1Vars);
                TableLookup(TableNum).TableLookupZData.allocate(1, 1, 1, 1, size(PerfCurveTableData(TableNum).Y));
                TableLookup(TableNum).X1Var = PerfCurveTableData(TableNum).X1;
                TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, _) = PerfCurveTableData(TableNum).Y(1, _);
        }
        */

        // Loop over two variable tables and load data
        CurrentModuleObject = "Table:TwoIndependentVariables";
        for (CurveIndex = 1; CurveIndex <= NumTwoVarTab; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ++CurveNum;
            ++TableNum;
            if (lAlphaFieldBlanks(7)) {
                NumTableEntries = (NumNumbers - 7) / 3;
                TableData(TableNum).X1.allocate(NumTableEntries);
                TableData(TableNum).X2.allocate(NumTableEntries);
                TableData(TableNum).Y.allocate(NumTableEntries);
            }
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).NumDims = 2;
            PerfCurve(CurveNum).TableIndex = TableNum;
            {
                auto const SELECT_CASE_var(Alphas(2));
                if (SELECT_CASE_var == "BICUBIC") {
                    PerfCurve(CurveNum).CurveType = BiCubic;
                    TableLookup(TableNum).InterpolationOrder = 4;
                } else if (SELECT_CASE_var == "BIQUADRATIC") {
                    PerfCurve(CurveNum).CurveType = BiQuadratic;
                    TableLookup(TableNum).InterpolationOrder = 3;
                } else if (SELECT_CASE_var == "QUADRATICLINEAR") {
                    PerfCurve(CurveNum).CurveType = QuadraticLinear;
                    TableLookup(TableNum).InterpolationOrder = 3;
                } else {
                    if (Alphas(3) == "EVALUATECURVETOLIMITS") {
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cAlphaFieldNames(2) + " [" + Alphas(2) + "] is not a valid choice. ");
                        ErrorsFound = true;
                    }
                }
            }
            {
                auto const SELECT_CASE_var(Alphas(3));
                if (SELECT_CASE_var == "LINEARINTERPOLATIONOFTABLE") {
                    PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;
                } else if (SELECT_CASE_var == "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION") {
                    PerfCurve(CurveNum).InterpolationType = LagrangeInterpolationLinearExtrapolation;
                } else if (SELECT_CASE_var == "EVALUATECURVETOLIMITS") {
                    PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
                } else {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(2) + " [" + Alphas(2) + "] is not a valid choice. ");
                    ErrorsFound = true;
                }
            }

            if (lNumericFieldBlanks(1)) {
                PerfCurve(CurveNum).Var1Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var1Min = Numbers(1);
                PerfCurve(CurveNum).Var1MinPresent = true;
            }
            if (lNumericFieldBlanks(2)) {
                PerfCurve(CurveNum).Var1Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var1Max = Numbers(2);
                PerfCurve(CurveNum).Var1MaxPresent = true;
            }
            if (lNumericFieldBlanks(3)) {
                PerfCurve(CurveNum).Var2Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var2Min = Numbers(3);
                PerfCurve(CurveNum).Var2MinPresent = true;
            }
            if (lNumericFieldBlanks(4)) {
                PerfCurve(CurveNum).Var2Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var2Max = Numbers(4);
                PerfCurve(CurveNum).Var2MaxPresent = true;
            }

            if (Numbers(1) > Numbers(2)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(1) + " [" + RoundSigDigits(Numbers(1), 2) + "] > " + cNumericFieldNames(2) + " [" +
                                  RoundSigDigits(Numbers(2), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(3) > Numbers(4)) { // error
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(3) + " [" + RoundSigDigits(Numbers(3), 2) + "] > " + cNumericFieldNames(4) + " [" +
                                  RoundSigDigits(Numbers(4), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 4) {
                if (!IsCurveInputTypeValid(Alphas(4))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(4) + " [" + Alphas(4) + "] is invalid");
                }
            }
            if (NumAlphas >= 5) {
                if (!IsCurveInputTypeValid(Alphas(5))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(5) + " [" + Alphas(5) + "] is invalid");
                }
            }
            if (NumAlphas >= 6) {
                if (!IsCurveOutputTypeValid(Alphas(6))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(6) + " [" + Alphas(6) + "] is invalid");
                }
            }

            if (UtilityRoutines::SameString(Alphas(4), "WAVELENGTH")) {
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1) + ": ");
                ShowContinueError(cAlphaFieldNames(4) + " = WAVELENGTH, and " + cAlphaFieldNames(5) + " = " + Alphas(5));
                ShowContinueError("In order to input correct variable type for optical properties, " + cAlphaFieldNames(4) +
                                  " should be ANGLE, and " + cAlphaFieldNames(5) + " should be WAVELENGTH ");
                ErrorsFound = true;
            }
            if (UtilityRoutines::SameString(Alphas(4), "ANGLE") && !UtilityRoutines::SameString(Alphas(5), "WAVELENGTH")) {
                ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1) + ": ");
                ShowContinueError(cAlphaFieldNames(4) + " = ANGLE, and " + cAlphaFieldNames(5) + " = " + Alphas(5));
                ShowContinueError("In order to input correct variable type for optical properties, " + cAlphaFieldNames(4) +
                                  " should be ANGLE, and " + cAlphaFieldNames(5) + " should be WAVELENGTH ");
                ErrorsFound = true;
            }
            if (UtilityRoutines::SameString(Alphas(4), "ANGLE") && UtilityRoutines::SameString(Alphas(5), "WAVELENGTH")) {
                PerfCurve(CurveNum).OpticalProperty = true;
            }

            if (!lNumericFieldBlanks(7)) {
                TableData(TableNum).NormalPoint = Numbers(7);
                if (Numbers(7) == 0.0) {
                    ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError("..." + cNumericFieldNames(7) + " [" + RoundSigDigits(Numbers(7), 6) + "] is not a valid choice.");
                    ShowContinueError("...Setting Normalization Reference to 1 and the simulation continues.");
                    TableData(TableNum).NormalPoint = 1.0;
                }
            } else {
                TableData(TableNum).NormalPoint = 1.0;
            }

            if (!lNumericFieldBlanks(5)) {
                PerfCurve(CurveNum).CurveMin = Numbers(5) / TableData(TableNum).NormalPoint;
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (!lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).CurveMax = Numbers(6) / TableData(TableNum).NormalPoint;
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (!lAlphaFieldBlanks(7)) {
                ReadFromFile = true;
                FileName = Alphas(7);
                ReadTwoVarTableDataFromFile(CurveNum, FileName, MaxTableNums);
            } else {
                ReadFromFile = false;
                FileName = "";

                MaxTableNums = (NumNumbers - 7) / 3;
                if (MaxTableNums < 4) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError("When data is read from input, the minimum number of data entries must be equal or greater than 12. The "
                                      "current input number is " +
                                      RoundSigDigits(NumNumbers - 7));
                    ErrorsFound = true;
                } else {
                    for (TableDataIndex = 1; TableDataIndex <= 4; ++TableDataIndex) {
                        if (lNumericFieldBlanks((TableDataIndex - 1) * 3 + 7 + 1)) {
                            ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                            ShowContinueError("When data is read from input, this field is required and cannot be blank: " +
                                              cNumericFieldNames((TableDataIndex - 1) * 3 + 7 + 1));
                            ErrorsFound = true;
                        }
                        if (lNumericFieldBlanks((TableDataIndex - 1) * 3 + 7 + 2)) {
                            ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                            ShowContinueError("When data is read from input, this field is required and cannot be blank: " +
                                              cNumericFieldNames((TableDataIndex - 1) * 3 + 7 + 2));
                            ErrorsFound = true;
                        }
                        if (lNumericFieldBlanks((TableDataIndex - 1) * 3 + 7 + 3)) {
                            ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                            ShowContinueError("When data is read from input, this field is required and cannot be blank: " +
                                              cNumericFieldNames((TableDataIndex - 1) * 3 + 7 + 3));
                            ErrorsFound = true;
                        }
                    }
                }
                if (mod((NumNumbers - 7), 3) != 0) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError("The number of data entries must be evenly divisable by 3. Number of data entries = " +
                                      RoundSigDigits(NumNumbers - 7));
                    ErrorsFound = true;
                    TableData(TableNum).X1 = 0.;
                    TableData(TableNum).X2 = 0.;
                    TableData(TableNum).Y = 0.;
                } else {
                    for (TableDataIndex = 1; TableDataIndex <= MaxTableNums; ++TableDataIndex) {
                        TableData(TableNum).X1(TableDataIndex) = Numbers((TableDataIndex - 1) * 3 + 7 + 1);
                        TableData(TableNum).X2(TableDataIndex) = Numbers((TableDataIndex - 1) * 3 + 7 + 2);
                        TableData(TableNum).Y(TableDataIndex) = Numbers((TableDataIndex - 1) * 3 + 7 + 3) / TableData(TableNum).NormalPoint;
                    }
                }
            }

            //  convert raw table data to multidimensional array
            // find number of x variables
            XVar.allocate(MaxTableNums);
            X2Var.allocate(MaxTableNums);
            NumXVar = 1;
            NextXVar = 1;
            XVar(1) = 1.0;
            TempTableData = TableData;
            Temp2TableData = TableData;
            while (NumXVar <= MaxTableNums) {

                MinTableData = minval(TempTableData(TableNum).X1);
                for (VarIndex = 1; VarIndex <= MaxTableNums; ++VarIndex) {
                    if (TempTableData(TableNum).X1(VarIndex) == MinTableData) {
                        TableData(TableNum).X1(NumXVar) = TempTableData(TableNum).X1(VarIndex);
                        TableData(TableNum).X2(NumXVar) = TempTableData(TableNum).X2(VarIndex);
                        TableData(TableNum).Y(NumXVar) = TempTableData(TableNum).Y(VarIndex);
                        TempTableData(TableNum).X1(VarIndex) = 999999.0;
                        ++NumXVar;
                    }
                }
                Temp2TableData(TableNum).X2({NextXVar, NumXVar - 1}) = TableData(TableNum).X2({NextXVar, NumXVar - 1});
                Temp2TableData(TableNum).Y({NextXVar, NumXVar - 1}) = TableData(TableNum).Y({NextXVar, NumXVar - 1});

                for (TempVarIndex = NumXVar - 1; TempVarIndex >= NextXVar; --TempVarIndex) {
                    MaxTableDataValue = -999999.0;
                    for (TempVarIndex1 = NextXVar; TempVarIndex1 <= NumXVar - 1; ++TempVarIndex1) {
                        if (Temp2TableData(TableNum).X2(TempVarIndex1) > MaxTableDataValue) {
                            MaxTableDataValue = Temp2TableData(TableNum).X2(TempVarIndex1);
                        }
                    }

                    for (TempVarIndex1 = NextXVar; TempVarIndex1 <= NumXVar - 1; ++TempVarIndex1) {
                        if (Temp2TableData(TableNum).X2(TempVarIndex1) != MaxTableDataValue) continue;
                        TableData(TableNum).X2(TempVarIndex) = Temp2TableData(TableNum).X2(TempVarIndex1);
                        TableData(TableNum).Y(TempVarIndex) = Temp2TableData(TableNum).Y(TempVarIndex1);
                        Temp2TableData(TableNum).X2(TempVarIndex1) = -999999.0;
                        break;
                    }
                }

                NextXVar = NumXVar;
            }
            // reorganize table data
            NumXVar = 1;
            NumX2Var = 1;
            XVar(1) = TableData(TableNum).X1(1);
            for (VarIndex = 2; VarIndex <= MaxTableNums; ++VarIndex) {
                if (TableData(TableNum).X1(VarIndex) != TableData(TableNum).X1(VarIndex - 1)) {
                    ++NumXVar;
                    XVar(NumXVar) = TableData(TableNum).X1(VarIndex);
                }
            }
            X2Var(1) = TableData(TableNum).X2(1);
            for (VarIndex = 2; VarIndex <= MaxTableNums; ++VarIndex) {
                FoundNewData = true;
                for (TempVarIndex = 1; TempVarIndex <= NumX2Var; ++TempVarIndex) {
                    if (TableData(TableNum).X2(VarIndex) == X2Var(TempVarIndex)) {
                        FoundNewData = false;
                    }
                }
                if (FoundNewData) {
                    ++NumX2Var;
                    X2Var(NumX2Var) = TableData(TableNum).X2(VarIndex);
                }
            }

            // move table data to performance curve table data structure
            PerfCurveTableData(TableNum).X1.allocate(NumXVar);
            PerfCurveTableData(TableNum).X2.allocate(NumX2Var);
            PerfCurveTableData(TableNum).Y.allocate(NumX2Var, NumXVar);
            PerfCurveTableData(TableNum).X1 = -9999999.0;
            PerfCurveTableData(TableNum).X2 = -9999999.0;
            PerfCurveTableData(TableNum).Y = -9999999.0;
            for (VarIndex = 1; VarIndex <= NumXVar; ++VarIndex) {
                PerfCurveTableData(TableNum).X1(VarIndex) = XVar(VarIndex);
                for (TempVarIndex = 1; TempVarIndex <= NumX2Var; ++TempVarIndex) {
                    PerfCurveTableData(TableNum).X2(TempVarIndex) = X2Var(TempVarIndex);
                    for (TempVarIndex1 = 1; TempVarIndex1 <= MaxTableNums; ++TempVarIndex1) {
                        if ((TableData(TableNum).X1(TempVarIndex1) == PerfCurveTableData(TableNum).X1(VarIndex)) &&
                            (TableData(TableNum).X2(TempVarIndex1) == PerfCurveTableData(TableNum).X2(TempVarIndex))) {
                            PerfCurveTableData(TableNum).Y(TempVarIndex, VarIndex) = TableData(TableNum).Y(TempVarIndex1);
                        }
                    }
                }
            }
            XVar.deallocate();
            X2Var.deallocate();

            // create curve objects when regression analysis is required
            if (PerfCurve(CurveNum).InterpolationType == EvaluateCurveToLimits) {
                {
                    auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                    if ((SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                        TempArray1 = TableData(TableNum).X1;
                        TempArray3 = TableData(TableNum).X2;
                        TempArray2.allocate(size(TableData(TableNum).Y));
                        for (VarIndex = 1; VarIndex <= isize(TableData(TableNum).Y); ++VarIndex) {
                            TempArray2(VarIndex) = TableData(TableNum).Y(VarIndex);
                        }
                        SolveRegression(CurveNum, CurrentModuleObject, PerfCurve(CurveNum).Name, TempArray1, TempArray2, TempArray3);
                        TempArray1.deallocate();
                        TempArray2.deallocate();
                        TempArray3.deallocate();
                    } else {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError("The requested regression analysis is not available at this time. Curve type = " + Alphas(2));
                        PerfCurve(CurveIndex).InterpolationType = LinearInterpolationOfTable;
                    }
                }
                if (!PerfCurve(CurveNum).Var1MinPresent) {
                    PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                }
                if (!PerfCurve(CurveNum).Var1MaxPresent) {
                    PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                }
                if (!PerfCurve(CurveNum).Var2MinPresent) {
                    PerfCurve(CurveNum).Var2Min = minval(TableData(TableNum).X2);
                }
                if (!PerfCurve(CurveNum).Var2MaxPresent) {
                    PerfCurve(CurveNum).Var2Max = maxval(TableData(TableNum).X2);
                }
            }

            // if user enters limits that exceed data range, warn that limits are based on table data
            if (PerfCurve(CurveNum).InterpolationType == LinearInterpolationOfTable) {
                if (PerfCurve(CurveNum).Var1MinPresent) {
                    if (PerfCurve(CurveNum).Var1Min < minval(TableData(TableNum).X1)) {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cNumericFieldNames(1) + " exceeds the data range and will not be used.");
                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(1), 6) +
                                          ", Minimum data range = " + RoundSigDigits(minval(TableData(TableNum).X1), 6));
                        PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                    }
                } else {
                    PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                }
                if (PerfCurve(CurveNum).Var1MaxPresent) {
                    if (PerfCurve(CurveNum).Var1Max > maxval(TableData(TableNum).X1)) {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cNumericFieldNames(2) + " exceeds the data range and will not be used.");
                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(2), 6) +
                                          ", Maximum data range = " + RoundSigDigits(maxval(TableData(TableNum).X1), 6));
                        PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                    }
                } else {
                    PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                }
                if (PerfCurve(CurveNum).Var2MinPresent) {
                    if (PerfCurve(CurveNum).Var2Min < minval(TableData(TableNum).X2)) {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cNumericFieldNames(3) + " exceeds the data range and will not be used.");
                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(3), 6) +
                                          ", Minimum data range = " + RoundSigDigits(minval(TableData(TableNum).X2), 6));
                        PerfCurve(CurveNum).Var2Min = minval(TableData(TableNum).X2);
                    }
                } else {
                    PerfCurve(CurveNum).Var2Min = minval(TableData(TableNum).X2);
                }
                if (PerfCurve(CurveNum).Var2MaxPresent) {
                    if (PerfCurve(CurveNum).Var2Max > maxval(TableData(TableNum).X2)) {
                        ShowWarningError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cNumericFieldNames(4) + " exceeds the data range and will not be used.");
                        ShowContinueError(" Entered value = " + RoundSigDigits(Numbers(4), 6) +
                                          ", Maximum data range = " + RoundSigDigits(maxval(TableData(TableNum).X2), 6));
                        PerfCurve(CurveNum).Var2Max = maxval(TableData(TableNum).X2);
                    }
                } else {
                    PerfCurve(CurveNum).Var2Max = maxval(TableData(TableNum).X2);
                }
            }
            // if user does not enter limits, set to min/max in table
            if (PerfCurve(CurveNum).InterpolationType == LagrangeInterpolationLinearExtrapolation) {
                if (!PerfCurve(CurveNum).Var1MinPresent) {
                    PerfCurve(CurveNum).Var1Min = minval(TableData(TableNum).X1);
                }
                if (!PerfCurve(CurveNum).Var1MaxPresent) {
                    PerfCurve(CurveNum).Var1Max = maxval(TableData(TableNum).X1);
                }
                if (!PerfCurve(CurveNum).Var2MinPresent) {
                    PerfCurve(CurveNum).Var2Min = minval(TableData(TableNum).X2);
                }
                if (!PerfCurve(CurveNum).Var2MaxPresent) {
                    PerfCurve(CurveNum).Var2Max = maxval(TableData(TableNum).X2);
                }
            }

            // move table data to more compact array to allow interpolation using multivariable lookup table method
            TableLookup(TableNum).NumX1Vars = size(PerfCurveTableData(TableNum).X1);
            TableLookup(TableNum).NumX2Vars = size(PerfCurveTableData(TableNum).X2);
            TableLookup(TableNum).X1Var.allocate(TableLookup(TableNum).NumX1Vars);
            TableLookup(TableNum).X2Var.allocate(TableLookup(TableNum).NumX2Vars);
            TableLookup(TableNum).TableLookupZData.allocate(
                1, 1, 1, 1, size(PerfCurveTableData(TableNum).Y(_, 1)), size(PerfCurveTableData(TableNum).Y(1, _)));
            TableLookup(TableNum).X1Var = PerfCurveTableData(TableNum).X1;
            TableLookup(TableNum).X2Var = PerfCurveTableData(TableNum).X2;
            TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, _, _) = PerfCurveTableData(TableNum).Y(_, _);
        }

        // Loop over multiple variable tables and load data (strict lookup only - no curve creation
        CurrentModuleObject = "Table:MultiVariableLookup";
        TableNum = NumTables;
        for (CurveIndex = 1; CurveIndex <= NumMultVarLookup; ++CurveIndex) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          CurveIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            GlobalNames::VerifyUniqueInterObjectName(UniqueCurveNames, Alphas(1), CurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);
            ++CurveNum;
            ++TableNum;
            PerfCurve(CurveNum).Name = Alphas(1);
            PerfCurve(CurveNum).ObjectType = CurrentModuleObject;
            PerfCurve(CurveNum).TableIndex = TableNum;
            {
                auto const SELECT_CASE_var(Alphas(2));
                if (SELECT_CASE_var == "LINEARINTERPOLATIONOFTABLE") {
                    PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;
                } else if (SELECT_CASE_var == "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION") {
                    PerfCurve(CurveNum).InterpolationType = LagrangeInterpolationLinearExtrapolation;
                } else if (SELECT_CASE_var == "EVALUATECURVETOLIMITS") {
                    PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
                } else {
                    ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(2) + " [" + Alphas(2) + "] is not a valid choice. ");
                    ErrorsFound = true;
                }
            }

            {
                auto const SELECT_CASE_var(Alphas(3));
                if (SELECT_CASE_var == "LINEAR") {
                    PerfCurve(CurveNum).CurveType = Linear;
                } else if (SELECT_CASE_var == "QUADRATIC") {
                    PerfCurve(CurveNum).CurveType = Quadratic;
                } else if (SELECT_CASE_var == "CUBIC") {
                    PerfCurve(CurveNum).CurveType = Cubic;
                } else if (SELECT_CASE_var == "QUARTIC") {
                    PerfCurve(CurveNum).CurveType = Quartic;
                } else if (SELECT_CASE_var == "BIQUADRATIC") {
                    PerfCurve(CurveNum).CurveType = BiQuadratic;
                } else if (SELECT_CASE_var == "QUADRATICLINEAR") {
                    PerfCurve(CurveNum).CurveType = QuadraticLinear;
                } else {
                    if (Alphas(2) == "EVALUATECURVETOLIMITS") {
                        ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError(cAlphaFieldNames(3) + " [" + Alphas(3) + "] is not a valid choice. ");
                        ErrorsFound = true;
                    }
                }
            }

            {
                auto const SELECT_CASE_var(Alphas(4));
                if (SELECT_CASE_var == "SINGLELINEINDEPENDENTVARIABLEWITHMATRIX") {
                    PerfCurve(CurveNum).DataFormat = SINGLELINEINDEPENDENTVARIABLEWITHMATRIX;
                } else {
                    ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(4) + " [" + Alphas(4) + "] is not a valid choice. ");
                    ErrorsFound = true;
                }
            }

            TableLookup(TableNum).InterpolationOrder = Numbers(1);

            if (!lNumericFieldBlanks(2)) {
                TableData(TableNum).NormalPoint = Numbers(2);
                if (Numbers(2) == 0.0) {
                    ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError("..." + cNumericFieldNames(2) + " [" + RoundSigDigits(Numbers(2), 6) + "] is not a valid choice.");
                    ShowContinueError("...Setting Normalization Reference to 1 and the simulation continues.");
                    TableData(TableNum).NormalPoint = 1.0;
                }
            } else {
                TableData(TableNum).NormalPoint = 1.0;
            }
            if (lNumericFieldBlanks(3)) {
                PerfCurve(CurveNum).Var1Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var1Min = Numbers(3);
                PerfCurve(CurveNum).Var1MinPresent = true;
            }
            if (lNumericFieldBlanks(4)) {
                PerfCurve(CurveNum).Var1Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var1Max = Numbers(4);
                PerfCurve(CurveNum).Var1MaxPresent = true;
            }
            if (lNumericFieldBlanks(5)) {
                PerfCurve(CurveNum).Var2Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var2Min = Numbers(5);
                PerfCurve(CurveNum).Var2MinPresent = true;
            }
            if (lNumericFieldBlanks(6)) {
                PerfCurve(CurveNum).Var2Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var2Max = Numbers(6);
                PerfCurve(CurveNum).Var2MaxPresent = true;
            }
            if (lNumericFieldBlanks(7)) {
                PerfCurve(CurveNum).Var3Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var3Min = Numbers(7);
                PerfCurve(CurveNum).Var3MinPresent = true;
            }
            if (lNumericFieldBlanks(8)) {
                PerfCurve(CurveNum).Var3Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var3Max = Numbers(8);
                PerfCurve(CurveNum).Var3MaxPresent = true;
            }
            if (lNumericFieldBlanks(9)) {
                PerfCurve(CurveNum).Var4Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var4Min = Numbers(9);
                PerfCurve(CurveNum).Var4MinPresent = true;
            }
            if (lNumericFieldBlanks(10)) {
                PerfCurve(CurveNum).Var4Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var4Max = Numbers(10);
                PerfCurve(CurveNum).Var4MaxPresent = true;
            }
            if (lNumericFieldBlanks(11)) {
                PerfCurve(CurveNum).Var5Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var5Min = Numbers(11);
                PerfCurve(CurveNum).Var5MinPresent = true;
            }
            if (lNumericFieldBlanks(12)) {
                PerfCurve(CurveNum).Var5Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var5Max = Numbers(12);
                PerfCurve(CurveNum).Var5MaxPresent = true;
            }
            if (lNumericFieldBlanks(13)) {
                PerfCurve(CurveNum).Var6Min = -99999999999.0;
            } else {
                PerfCurve(CurveNum).Var6Min = Numbers(13);
                PerfCurve(CurveNum).Var6MinPresent = true;
            }
            if (lNumericFieldBlanks(14)) {
                PerfCurve(CurveNum).Var6Max = 99999999999.0;
            } else {
                PerfCurve(CurveNum).Var6Max = Numbers(14);
                PerfCurve(CurveNum).Var6MaxPresent = true;
            }
            if (Numbers(3) > Numbers(4)) { // error
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(3) + " [" + RoundSigDigits(Numbers(3), 2) + "] > " + cNumericFieldNames(4) + " [" +
                                  RoundSigDigits(Numbers(4), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(5) > Numbers(6)) { // error
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(5) + " [" + RoundSigDigits(Numbers(5), 2) + "] > " + cNumericFieldNames(6) + " [" +
                                  RoundSigDigits(Numbers(6), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(7) > Numbers(8)) { // error
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(7) + " [" + RoundSigDigits(Numbers(7), 2) + "] > " + cNumericFieldNames(8) + " [" +
                                  RoundSigDigits(Numbers(8), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(9) > Numbers(10)) { // error
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(9) + " [" + RoundSigDigits(Numbers(9), 2) + "] > " + cNumericFieldNames(10) + " [" +
                                  RoundSigDigits(Numbers(10), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(11) > Numbers(12)) { // error
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(11) + " [" + RoundSigDigits(Numbers(11), 2) + "] > " + cNumericFieldNames(12) + " [" +
                                  RoundSigDigits(Numbers(12), 2) + ']');
                ErrorsFound = true;
            }
            if (Numbers(13) > Numbers(14)) { // error
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError(cNumericFieldNames(13) + " [" + RoundSigDigits(Numbers(13), 2) + "] > " + cNumericFieldNames(14) + " [" +
                                  RoundSigDigits(Numbers(14), 2) + ']');
                ErrorsFound = true;
            }
            if (NumAlphas >= 8) {
                if (!IsCurveInputTypeValid(Alphas(8))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(8) + " [" + Alphas(8) + "] is invalid");
                }
            }
            if (NumAlphas >= 9) {
                if (!IsCurveInputTypeValid(Alphas(9))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(9) + " [" + Alphas(9) + "] is invalid");
                }
            }
            if (NumAlphas >= 10) {
                if (!IsCurveInputTypeValid(Alphas(10))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(10) + " [" + Alphas(10) + "] is invalid");
                }
            }
            if (NumAlphas >= 11) {
                if (!IsCurveInputTypeValid(Alphas(11))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(11) + " [" + Alphas(11) + "] is invalid");
                }
            }
            if (NumAlphas >= 12) {
                if (!IsCurveInputTypeValid(Alphas(12))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(12) + " [" + Alphas(12) + "] is invalid");
                }
            }
            if (NumAlphas >= 13) {
                if (!IsCurveInputTypeValid(Alphas(13))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(13) + " [" + Alphas(13) + "] is invalid");
                }
            }
            if (NumAlphas >= 14) {
                if (!IsCurveOutputTypeValid(Alphas(14))) {
                    ShowSevereError("GetCurveInput: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError(cAlphaFieldNames(14) + " [" + Alphas(14) + "] is invalid");
                }
            }

            if (!lNumericFieldBlanks(15)) {
                PerfCurve(CurveNum).CurveMin = Numbers(15) / TableData(TableNum).NormalPoint;
                PerfCurve(CurveNum).CurveMinPresent = true;
            }
            if (!lNumericFieldBlanks(16)) {
                PerfCurve(CurveNum).CurveMax = Numbers(16) / TableData(TableNum).NormalPoint;
                PerfCurve(CurveNum).CurveMaxPresent = true;
            }

            if (Alphas(6) == "ASCENDING") {
                PerfCurve(CurveNum).X1SortOrder = ASCENDING;
            } else if (Alphas(6) == "DESCENDING") {
                PerfCurve(CurveNum).X1SortOrder = DESCENDING;
            } else {
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError("...Invalid " + cAlphaFieldNames(6) + " = " + Alphas(6));
                ErrorsFound = true;
            }
            if (Alphas(7) == "ASCENDING") {
                PerfCurve(CurveNum).X2SortOrder = ASCENDING;
            } else if (Alphas(7) == "DESCENDING") {
                PerfCurve(CurveNum).X2SortOrder = DESCENDING;
            } else {
                ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError("...Invalid " + cAlphaFieldNames(7) + " = " + Alphas(7));
                ErrorsFound = true;
            }

            if (!lAlphaFieldBlanks(5)) {
                ReadFromFile = true;
                FileName = Alphas(5);
            } else {
                ReadFromFile = false;
                FileName = "";
            }

            ReadTableData(CurveNum, CurrentModuleObject, ReadFromFile, FileName, Alphas, Numbers, NumNumbers, ErrorsFound);

            if (PerfCurve(CurveNum).InterpolationType == EvaluateCurveToLimits) {
                {
                    auto const SELECT_CASE_var(PerfCurve(CurveNum).NumDims);
                    if (SELECT_CASE_var == 1) {
                        TempArray1.allocate(size(TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, 1, _)));
                        TempArray2.allocate(size(TempArray1));
                        TempArray1 = TableLookup(TableNum).X1Var;
                        TempArray2 = TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, 1, _);
                        SolveRegression(CurveNum, CurrentModuleObject, PerfCurve(CurveNum).Name, TempArray1, TempArray2);
                        TempArray1.deallocate();
                        TempArray2.deallocate();

                        // Save array info in performance table arrays in case the performance table routine is selected in regression routine
                        PerfCurveTableData(TableNum).X1.allocate(size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).Y.allocate(1, size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).X1 = TableLookup(TableNum).X1Var;
                        PerfCurveTableData(TableNum).Y(1, _) = TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, 1, _);

                    } else if (SELECT_CASE_var == 2) {
                        TempArray1.allocate(size(TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, _, _)));
                        TempArray2.allocate(size(TempArray1));
                        TempArray3.allocate(size(TempArray1));
                        TableDataIndex = 0;
                        for (VarIndex = 1; VarIndex <= TableLookup(TableNum).NumX1Vars; ++VarIndex) {
                            for (TempVarIndex = 1; TempVarIndex <= TableLookup(TableNum).NumX2Vars; ++TempVarIndex) {
                                ++TableDataIndex;
                                TempArray1(TableDataIndex) = TableLookup(TableNum).X1Var(VarIndex);
                                TempArray2(TableDataIndex) = TableLookup(TableNum).X2Var(TempVarIndex);
                                TempArray3(TableDataIndex) = TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, TempVarIndex, VarIndex);
                            }
                        }
                        SolveRegression(CurveNum, CurrentModuleObject, PerfCurve(CurveNum).Name, TempArray1, TempArray3, TempArray2);
                        TempArray1.deallocate();
                        TempArray2.deallocate();
                        TempArray3.deallocate();
                        // Save array info in performance table arrays in case the performance table routine is selected in regression routine
                        PerfCurveTableData(TableNum).X1.allocate(size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).X2.allocate(size(TableLookup(TableNum).X2Var));
                        PerfCurveTableData(TableNum).Y.allocate(size(TableLookup(TableNum).X2Var), size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).X1 = TableLookup(TableNum).X1Var;
                        PerfCurveTableData(TableNum).X2 = TableLookup(TableNum).X2Var;
                        PerfCurveTableData(TableNum).Y(_, _) = TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, _, _);
                    } else {
                        ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError("...Invalid " + cAlphaFieldNames(2) + " = " + Alphas(2));
                        ShowContinueError("...Choice not allowed with more than 2 indpendent variables.");
                        ErrorsFound = true;
                    }
                }
            } else {
                {
                    auto const SELECT_CASE_var(PerfCurve(CurveNum).NumDims);
                    if (SELECT_CASE_var == 1) {
                        // Save array info in performance table arrays in case the performance table routine is selected in regression routine
                        PerfCurveTableData(TableNum).X1.allocate(size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).Y.allocate(1, size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).X1 = TableLookup(TableNum).X1Var;
                        PerfCurveTableData(TableNum).Y(1, _) = TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, 1, _);
                        // if linear interpolation of table is selected, switch interpolation type
                    } else if (SELECT_CASE_var == 2) {
                        // Save array info in performance table arrays in case the performance table routine is selected in regression routine
                        PerfCurveTableData(TableNum).X1.allocate(size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).X2.allocate(size(TableLookup(TableNum).X2Var));
                        PerfCurveTableData(TableNum).Y.allocate(size(TableLookup(TableNum).X2Var), size(TableLookup(TableNum).X1Var));
                        PerfCurveTableData(TableNum).X1 = TableLookup(TableNum).X1Var;
                        PerfCurveTableData(TableNum).X2 = TableLookup(TableNum).X2Var;
                        PerfCurveTableData(TableNum).Y(_, _) = TableLookup(TableNum).TableLookupZData(1, 1, 1, 1, _, _);
                        // if linear interpolation of table is selected, switch interpolation type
                    } else {
                        // if linear interpolation of table is selected, fatal if more than 2 independent variables
                        if (PerfCurve(CurveNum).InterpolationType == LinearInterpolationOfTable) {
                            ShowSevereError("GetTableInput: For " + CurrentModuleObject + ": " + Alphas(1));
                            ShowContinueError("...Invalid " + cAlphaFieldNames(2) + " = " + Alphas(2));
                            ShowContinueError("...Choice not allowed with more than 2 indpendent variables.");
                            ErrorsFound = true;
                        }
                    }
                }
            }
        }
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

    void ReadTableData(int const CurveNum,
                       std::string &CurrentModuleObject,
                       bool const ReadFromFile,
                       std::string &FileName,
                       Array1S_string Alphas,
                       Array1S<Real64> Numbers,
                       int const NumNumbers,
                       bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   August 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Given the curve index, read the table data.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Using/Aliasing
        using DataGlobals::DisplayAdvancedReportVariables;
        using DataGlobals::OutputFileInits;
        using DataSystemVariables::CheckForActualFileName;
        using DataSystemVariables::GoodIOStatValue;
        using DataSystemVariables::iUnicode_end;
        using DataSystemVariables::TempFullFileName;
        using General::RoundSigDigits;

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // Argument array dimensioning

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt fmtA("(A)");
        static ObjexxFCL::gio::Fmt fmtLD("*");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FileNum(0);
        int endcol;
        int TableNum;
        std::string NextLine; // Line of data
        int DataSetCount(0);  // counter for number of lines read (used in some error messages)
        int ReadStat;         // File read status
        bool EOFonFile;       // True if EOF during file read
        int I;                // Do loop indexes and data set counter
        int J;
        int NumDataSets;
        Real64 Var3; // Temp variables for processing table lookup data
        Real64 Var4;
        Real64 Var5;
        Real64 Var6;
        int Var3Index;
        int Var4Index;
        int Var5Index;
        int Var6Index;
        int NumIVars;
        int TotalDataSets;
        int NumbersOffset;
        int BaseOffset;
        static bool WriteHeaderOnce(true); // eio header file write flag
        std::string CharTableData;         // used to echo each line of table data read in to eio file
        bool EchoTableDataToEio;           // logical set equal to global and used to report to eio file
        bool FileExists;
        IOFlags non_adv;
        non_adv.na_on(); // For non-advancing list-directed output

        // Formats
        static ObjexxFCL::gio::Fmt Format_140("('! Reading external file tabular data for ',A,' \"',A,'\"')");
        static ObjexxFCL::gio::Fmt Format_150("('! Reading tabular data for ',A,' \"',A,'\"')");
        static ObjexxFCL::gio::Fmt Format_110("('! <READING LOOKUP TABLE DATA>')");
        static ObjexxFCL::gio::Fmt Format_130("('READING LOOKUP TABLE DATA')");
        static ObjexxFCL::gio::Fmt Format_131("('END READING LOOKUP TABLE DATA')");
        static ObjexxFCL::gio::Fmt Format_160("(1X,10(I2,:,2X))");

        // Autodesk:Uninit Initialize variables used uninitialized
        TotalDataSets = 0; // Autodesk:Uninit Force default initialization

        EchoTableDataToEio = DisplayAdvancedReportVariables;
        TableNum = PerfCurve(CurveNum).TableIndex;

        if (ReadFromFile) {
            CheckForActualFileName(FileName, FileExists, TempFullFileName);
            if (!FileExists) goto Label999;
            FileNum = GetNewUnitNumber();
            {
                IOFlags flags;
                flags.ACTION("read");
                ObjexxFCL::gio::open(FileNum, TempFullFileName, flags);
                if (flags.err()) goto Label999;
            }
            ObjexxFCL::gio::read(FileNum, fmtA) >> NextLine;
            trim(NextLine);
            endcol = len(NextLine);
            if (endcol == 0) {
                ShowWarningError("ReadTableData: Blank line found in external file = " + FileName);
                ShowContinueError("...Blank lines are not allowed. Will try to read next line.");
                ObjexxFCL::gio::read(FileNum, fmtA) >> NextLine;
                trim(NextLine);
                endcol = len(NextLine);
                if (endcol == 0) {
                    ShowWarningError("ReadTableData: Data not found on second line in external file = " + FileName);
                    ShowContinueError("...Check that file is ASCII text and that file is not locked by other applications.");
                    ShowFatalError("External table data not found. Simulation will terminate.");
                } else {
                    ShowWarningError("Second read attempt found data in external file = " + FileName);
                    ShowFatalError("...Blank lines are not allowed. Simulation will terminate.");
                }
            }
            if (endcol > 0) {
                if (int(NextLine[endcol - 1]) == iUnicode_end) {
                    ShowSevereError("ReadTableData: For Table:MultiVariableLookup \"" + PerfCurve(CurveNum).Name +
                                    "\" external file, appears to be a Unicode or binary file.");
                    ShowContinueError("...This file cannot be read by this program. Please save as PC or Unix file and try again");
                    ShowFatalError("Program terminates due to previous condition.");
                }
            }

            ObjexxFCL::gio::rewind(FileNum);

            {
                IOFlags flags;
                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> NumIVars;
                ReadStat = flags.ios();
            }
            if (NumIVars > 6 || NumIVars < 1) {
                ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError("...Invalid number of independent variables found in external file = " + FileName);
                ShowFatalError("...Only 1 to  independent variables are allowed.");
            }

            ObjexxFCL::gio::rewind(FileNum);

            if (NumIVars == 1) {
                IOFlags flags;
                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> NumIVars >> TableLookup(TableNum).NumX1Vars;
                ReadStat = flags.ios();
            };
            if (NumIVars == 2) {
                IOFlags flags;
                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> NumIVars >> TableLookup(TableNum).NumX1Vars >> TableLookup(TableNum).NumX2Vars;
                ReadStat = flags.ios();
            };
            if (NumIVars == 3) {
                IOFlags flags;
                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> NumIVars >> TableLookup(TableNum).NumX1Vars >> TableLookup(TableNum).NumX2Vars >>
                    TableLookup(TableNum).NumX3Vars;
                ReadStat = flags.ios();
            };
            if (NumIVars == 4) {
                IOFlags flags;
                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> NumIVars >> TableLookup(TableNum).NumX1Vars >> TableLookup(TableNum).NumX2Vars >>
                    TableLookup(TableNum).NumX3Vars >> TableLookup(TableNum).NumX4Vars;
                ReadStat = flags.ios();
            };
            if (NumIVars == 5) {
                IOFlags flags;
                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> NumIVars >> TableLookup(TableNum).NumX1Vars >> TableLookup(TableNum).NumX2Vars >>
                    TableLookup(TableNum).NumX3Vars >> TableLookup(TableNum).NumX4Vars >> TableLookup(TableNum).NumX5Vars;
                ReadStat = flags.ios();
            };
            if (NumIVars == 6) {
                IOFlags flags;
                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> NumIVars >> TableLookup(TableNum).NumX1Vars >> TableLookup(TableNum).NumX2Vars >>
                    TableLookup(TableNum).NumX3Vars >> TableLookup(TableNum).NumX4Vars >> TableLookup(TableNum).NumX5Vars >>
                    TableLookup(TableNum).NumX6Vars;
                ReadStat = flags.ios();
            };

            if (ReadStat < GoodIOStatValue) goto Label1000; // Autodesk:Uninit TotalDataSets was uninitialized after goto jump

            PerfCurve(CurveNum).NumDims = NumIVars;
            // Echo table data for user verification
            if (EchoTableDataToEio) {
                if (WriteHeaderOnce) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_110);
                    WriteHeaderOnce = false;
                }
                ObjexxFCL::gio::write(OutputFileInits, Format_130);
                ObjexxFCL::gio::write(OutputFileInits, Format_140) << CurrentModuleObject << Alphas(1);
            }
        } else {
            if (EchoTableDataToEio) {
                if (WriteHeaderOnce) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_110);
                    WriteHeaderOnce = false;
                }
                ObjexxFCL::gio::write(OutputFileInits, Format_130);
                ObjexxFCL::gio::write(OutputFileInits, Format_150) << CurrentModuleObject << Alphas(1);
            }
            NumIVars = Numbers(17);
            if (NumIVars > 6 || NumIVars < 1) {
                ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                ShowContinueError("...Invalid number of independent variables.");
                ShowFatalError("...Only 1 to 5 independent variables are allowed.");
            }

            BaseOffset = 17;
            PerfCurve(CurveNum).NumDims = NumIVars;
            TableLookup(TableNum).NumX1Vars = Numbers(18);
            if (NumIVars > 1) TableLookup(TableNum).NumX2Vars = Numbers(19);
            if (NumIVars > 2) TableLookup(TableNum).NumX3Vars = Numbers(20);
            if (NumIVars > 3) TableLookup(TableNum).NumX4Vars = Numbers(21); // were these right before?
            if (NumIVars > 4) TableLookup(TableNum).NumX5Vars = Numbers(22);
            if (NumIVars > 5) TableLookup(TableNum).NumX6Vars = Numbers(23);
        }

        if (EchoTableDataToEio) {
            if (NumIVars == 1) {
                ObjexxFCL::gio::write(CharTableData, Format_160) << NumIVars << TableLookup(TableNum).NumX1Vars;
            } else if (NumIVars == 2) {
                ObjexxFCL::gio::write(CharTableData, Format_160) << NumIVars << TableLookup(TableNum).NumX1Vars << TableLookup(TableNum).NumX2Vars;
            } else if (NumIVars == 3) {
                ObjexxFCL::gio::write(CharTableData, Format_160)
                    << NumIVars << TableLookup(TableNum).NumX1Vars << TableLookup(TableNum).NumX2Vars << TableLookup(TableNum).NumX3Vars;
            } else if (NumIVars == 4) {
                ObjexxFCL::gio::write(CharTableData, Format_160) << NumIVars << TableLookup(TableNum).NumX1Vars << TableLookup(TableNum).NumX2Vars
                                                      << TableLookup(TableNum).NumX3Vars << TableLookup(TableNum).NumX4Vars;
            } else if (NumIVars == 5) {
                ObjexxFCL::gio::write(CharTableData, Format_160)
                    << NumIVars << TableLookup(TableNum).NumX1Vars << TableLookup(TableNum).NumX2Vars << TableLookup(TableNum).NumX3Vars
                    << TableLookup(TableNum).NumX4Vars << TableLookup(TableNum).NumX5Vars;
            } else if (NumIVars == 6) {
                ObjexxFCL::gio::write(CharTableData, Format_160)
                    << NumIVars << TableLookup(TableNum).NumX1Vars << TableLookup(TableNum).NumX2Vars << TableLookup(TableNum).NumX3Vars
                    << TableLookup(TableNum).NumX4Vars << TableLookup(TableNum).NumX5Vars << TableLookup(TableNum).NumX6Vars;
            }

            ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
        }

        TableLookup(TableNum).X1Var.allocate(TableLookup(TableNum).NumX1Vars);
        TableLookup(TableNum).X2Var.allocate(TableLookup(TableNum).NumX2Vars);
        TableLookup(TableNum).X3Var.allocate(TableLookup(TableNum).NumX3Vars);
        TableLookup(TableNum).X4Var.allocate(TableLookup(TableNum).NumX4Vars);
        TableLookup(TableNum).X5Var.allocate(TableLookup(TableNum).NumX5Vars);
        TableLookup(TableNum).X6Var.allocate(TableLookup(TableNum).NumX6Vars);

        if (NumIVars > 0) {
            if (ReadFromFile) {

                {
                    IOFlags flags;
                    for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                        ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> TableLookup(TableNum).X1Var(I);
                    }
                    ReadStat = flags.ios();
                }

                if (EchoTableDataToEio) {
                    for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                        ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X1Var(I);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }

                if (ReadStat < GoodIOStatValue) goto Label1000;

            } else {

                if (NumNumbers >= BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars) {
                    for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                        TableLookup(TableNum).X1Var(I) = Numbers(BaseOffset + NumIVars + I);
                    }
                    std::vector<Real64> x1values(TableLookup(TableNum).X1Var.begin(), TableLookup(TableNum).X1Var.end());

                    if (EchoTableDataToEio) {
                        for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                            ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X1Var(I);
                        }
                        ObjexxFCL::gio::write(OutputFileInits);
                    }

                } else {
                    ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError("...The number of numeric inputs is less than expected.");
                    ErrorsFound = true;
                }
            }

            // check to make sure XVars are in increaseing (ascending) order
            for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                if (I == 1) continue;
                if (TableLookup(TableNum).X1Var(I) <= TableLookup(TableNum).X1Var(I - 1)) {
                    ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                    ShowContinueError("...The values for the independent variable X1 must be in increasing (ascending) order.");
                    ErrorsFound = true;
                    break;
                }
            }

            if (NumIVars > 1) {
                if (ReadFromFile) {
                    {
                        IOFlags flags;
                        for (I = 1; I <= TableLookup(TableNum).NumX2Vars; ++I) {
                            ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> TableLookup(TableNum).X2Var(I);
                        }
                        ReadStat = flags.ios();
                    }

                    if (EchoTableDataToEio) {
                        for (I = 1; I <= TableLookup(TableNum).NumX2Vars; ++I) {
                            ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X2Var(I);
                        }
                        ObjexxFCL::gio::write(OutputFileInits);
                    }

                    if (ReadStat < GoodIOStatValue) goto Label1000;

                } else {
                    if (NumNumbers >= BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars) {
                        for (I = 1; I <= TableLookup(TableNum).NumX2Vars; ++I) {
                            TableLookup(TableNum).X2Var(I) = Numbers(BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + I);
                        }

                        if (EchoTableDataToEio) {
                            for (I = 1; I <= TableLookup(TableNum).NumX2Vars; ++I) {
                                ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X2Var(I);
                            }
                            ObjexxFCL::gio::write(OutputFileInits);
                        }

                    } else {
                        ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError("...The number of numeric inputs is less than expected.");
                        ErrorsFound = true;
                    }
                }

                // check to make sure XVars are in increasing (ascending) order
                for (I = 1; I <= TableLookup(TableNum).NumX2Vars; ++I) {
                    if (I == 1) continue;
                    if (TableLookup(TableNum).X2Var(I) <= TableLookup(TableNum).X2Var(I - 1)) {
                        ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                        ShowContinueError("...The values for the independent variable X2 must be in increasing (ascending) order.");
                        ErrorsFound = true;
                        break;
                    }
                }

                if (NumIVars > 2) {
                    if (ReadFromFile) {
                        {
                            IOFlags flags;
                            for (I = 1; I <= TableLookup(TableNum).NumX3Vars; ++I) {
                                ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> TableLookup(TableNum).X3Var(I);
                            }
                            ReadStat = flags.ios();
                        }

                        if (EchoTableDataToEio) {
                            for (I = 1; I <= TableLookup(TableNum).NumX3Vars; ++I) {
                                ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X3Var(I);
                            }
                            ObjexxFCL::gio::write(OutputFileInits);
                        }

                        if (ReadStat < GoodIOStatValue) goto Label1000;

                    } else {
                        if (NumNumbers >= BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars +
                                              TableLookup(TableNum).NumX3Vars) {

                            for (I = 1; I <= TableLookup(TableNum).NumX3Vars; ++I) {
                                TableLookup(TableNum).X3Var(I) =
                                    Numbers(BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars + I);
                            }

                            if (EchoTableDataToEio) {
                                for (I = 1; I <= TableLookup(TableNum).NumX3Vars; ++I) {
                                    ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X3Var(I);
                                }
                                ObjexxFCL::gio::write(OutputFileInits);
                            }

                        } else {
                            ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                            ShowContinueError("...The number of numeric inputs is less than expected.");
                            ErrorsFound = true;
                        }
                    }

                    // check to make sure XVars are in increasing (ascending) order
                    for (I = 1; I <= TableLookup(TableNum).NumX3Vars; ++I) {
                        if (I == 1) continue;
                        if (TableLookup(TableNum).X3Var(I) <= TableLookup(TableNum).X3Var(I - 1)) {
                            ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                            ShowContinueError("...The values for the independent variable X3 must be in increasing (ascending) order.");
                            ErrorsFound = true;
                            break;
                        }
                    }

                    if (NumIVars > 3) {
                        if (ReadFromFile) {

                            {
                                IOFlags flags;

                                for (I = 1; I <= TableLookup(TableNum).NumX4Vars; ++I) {
                                    ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> TableLookup(TableNum).X4Var(I);
                                }
                                ReadStat = flags.ios();
                            }

                            if (EchoTableDataToEio) {
                                TableLookup(TableNum).X4Var(I);
                                for (I = 1; I <= TableLookup(TableNum).NumX4Vars; ++I) {
                                    ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X4Var(I);
                                }
                                ObjexxFCL::gio::write(OutputFileInits);
                            }

                            if (ReadStat < GoodIOStatValue) goto Label1000;

                        } else {
                            if (NumNumbers >= BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars +
                                                  TableLookup(TableNum).NumX3Vars + TableLookup(TableNum).NumX4Vars) {

                                for (I = 1; I <= TableLookup(TableNum).NumX4Vars; ++I) {
                                    TableLookup(TableNum).X4Var(I) = Numbers(BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars +
                                                                             TableLookup(TableNum).NumX2Vars + TableLookup(TableNum).NumX3Vars + I);
                                }

                                if (EchoTableDataToEio) {
                                    for (I = 1; I <= TableLookup(TableNum).NumX4Vars; ++I) {
                                        ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X4Var(I);
                                    }
                                    ObjexxFCL::gio::write(OutputFileInits);
                                }

                            } else {
                                ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                                ShowContinueError("...The number of numeric inputs is less than expected.");
                                ErrorsFound = true;
                            }
                        }

                        // check to make sure XVars are in increaseing (ascending) order
                        for (I = 1; I <= TableLookup(TableNum).NumX4Vars; ++I) {
                            if (I == 1) continue;
                            if (TableLookup(TableNum).X4Var(I) <= TableLookup(TableNum).X4Var(I - 1)) {
                                ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                                ShowContinueError("...The values for the independent variable X4 must be in increasing (ascending) order.");
                                ErrorsFound = true;
                                break;
                            }
                        }

                        if (NumIVars > 4) {
                            if (ReadFromFile) {
                                {
                                    IOFlags flags;
                                    for (I = 1; I <= TableLookup(TableNum).NumX5Vars; ++I) {
                                        ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> TableLookup(TableNum).X5Var(I);
                                    }
                                    ReadStat = flags.ios();
                                }

                                if (EchoTableDataToEio) {
                                    for (I = 1; I <= TableLookup(TableNum).NumX5Vars; ++I) {
                                        ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X5Var(I);
                                    }
                                    ObjexxFCL::gio::write(OutputFileInits);
                                }

                                if (ReadStat < GoodIOStatValue) goto Label1000;

                            } else {
                                if (NumNumbers >= BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars +
                                                      TableLookup(TableNum).NumX3Vars + TableLookup(TableNum).NumX4Vars +
                                                      TableLookup(TableNum).NumX5Vars) {

                                    for (I = 1; I <= TableLookup(TableNum).NumX5Vars; ++I) {
                                        TableLookup(TableNum).X5Var(I) =
                                            Numbers(BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars +
                                                    TableLookup(TableNum).NumX3Vars + TableLookup(TableNum).NumX4Vars + I);
                                    }

                                    if (EchoTableDataToEio) {
                                        for (I = 1; I <= TableLookup(TableNum).NumX5Vars; ++I) {
                                            ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X5Var(I);
                                        }
                                        ObjexxFCL::gio::write(OutputFileInits);
                                    }

                                } else {
                                    ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                                    ShowContinueError("...The number of numeric inputs is less than expected.");
                                    ErrorsFound = true;
                                }
                            }
                            // check to make sure XVars are in increaseing (ascending) order
                            for (I = 1; I <= TableLookup(TableNum).NumX5Vars; ++I) {
                                if (I == 1) continue;
                                if (TableLookup(TableNum).X5Var(I) <= TableLookup(TableNum).X5Var(I - 1)) {
                                    ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                                    ShowContinueError("...The values for the independent variable X5 must be in increasing (ascending) order.");
                                    ErrorsFound = true;
                                    break;
                                }
                            }

                            if (NumIVars > 5) { // something is wrong with this structure here...
                                if (ReadFromFile) {
                                    {
                                        IOFlags flags;
                                        for (I = 1; I <= TableLookup(TableNum).NumX6Vars; ++I) {
                                            ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> TableLookup(TableNum).X6Var(I);
                                        }
                                        ReadStat = flags.ios();
                                    }

                                    if (EchoTableDataToEio) {
                                        for (I = 1; I <= TableLookup(TableNum).NumX6Vars; ++I) {
                                            ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X6Var(I);
                                        }
                                        ObjexxFCL::gio::write(OutputFileInits);
                                    }

                                    if (ReadStat < GoodIOStatValue) goto Label1000;

                                } else {
                                    if (NumNumbers >= BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars +
                                                          TableLookup(TableNum).NumX3Vars + TableLookup(TableNum).NumX4Vars +
                                                          TableLookup(TableNum).NumX5Vars + TableLookup(TableNum).NumX6Vars) {

                                        for (I = 1; I <= TableLookup(TableNum).NumX6Vars; ++I) {
                                            TableLookup(TableNum).X6Var(I) =
                                                Numbers(BaseOffset + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars +
                                                        TableLookup(TableNum).NumX3Vars + TableLookup(TableNum).NumX4Vars +
                                                        TableLookup(TableNum).NumX5Vars + I);
                                        }

                                        if (EchoTableDataToEio) {
                                            for (I = 1; I <= TableLookup(TableNum).NumX6Vars; ++I) {
                                                ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv) << TableLookup(TableNum).X6Var(I);
                                            }
                                            ObjexxFCL::gio::write(OutputFileInits);
                                        }

                                    } else {
                                        ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                                        ShowContinueError("...The number of numeric inputs is less than expected.");
                                        ErrorsFound = true;
                                    }
                                }
                                // check to make sure XVars are in increaseing (ascending) order
                                for (I = 1; I <= TableLookup(TableNum).NumX6Vars; ++I) {
                                    if (I == 1) continue;
                                    if (TableLookup(TableNum).X6Var(I) <= TableLookup(TableNum).X6Var(I - 1)) {
                                        ShowSevereError("ReadTableData: For " + CurrentModuleObject + ": " + Alphas(1));
                                        ShowContinueError("...The values for the independent variable X6 must be in increasing (ascending) order.");
                                        ErrorsFound = true;
                                        break;
                                    }
                                }
                                TableLookup(TableNum).TableLookupZData.allocate(TableLookup(TableNum).NumX6Vars,
                                                                                TableLookup(TableNum).NumX5Vars,
                                                                                TableLookup(TableNum).NumX4Vars,
                                                                                TableLookup(TableNum).NumX3Vars,
                                                                                TableLookup(TableNum).NumX2Vars,
                                                                                TableLookup(TableNum).NumX1Vars);
                            } else {
                                TableLookup(TableNum).TableLookupZData.allocate(1,
                                                                                TableLookup(TableNum).NumX5Vars,
                                                                                TableLookup(TableNum).NumX4Vars,
                                                                                TableLookup(TableNum).NumX3Vars,
                                                                                TableLookup(TableNum).NumX2Vars,
                                                                                TableLookup(TableNum).NumX1Vars);
                            }
                        } else {
                            TableLookup(TableNum).TableLookupZData.allocate(1,
                                                                            1,
                                                                            TableLookup(TableNum).NumX4Vars,
                                                                            TableLookup(TableNum).NumX3Vars,
                                                                            TableLookup(TableNum).NumX2Vars,
                                                                            TableLookup(TableNum).NumX1Vars);
                        }
                    } else {
                        TableLookup(TableNum).TableLookupZData.allocate(
                            1, 1, 1, TableLookup(TableNum).NumX3Vars, TableLookup(TableNum).NumX2Vars, TableLookup(TableNum).NumX1Vars);
                    }
                } else {
                    TableLookup(TableNum).TableLookupZData.allocate(1, 1, 1, 1, TableLookup(TableNum).NumX2Vars, TableLookup(TableNum).NumX1Vars);
                }
            } else {
                TableLookup(TableNum).TableLookupZData.allocate(1, 1, 1, 1, 1, TableLookup(TableNum).NumX1Vars);
            }
        }

        TotalDataSets = 1;
        DataSetCount = 0;
        if (NumIVars == 3) TotalDataSets = TableLookup(TableNum).NumX3Vars;
        if (NumIVars == 4) TotalDataSets = TableLookup(TableNum).NumX3Vars * TableLookup(TableNum).NumX4Vars;
        if (NumIVars == 5) TotalDataSets = TableLookup(TableNum).NumX3Vars * TableLookup(TableNum).NumX4Vars * TableLookup(TableNum).NumX5Vars;
        if (NumIVars == 6)
            TotalDataSets =
                TableLookup(TableNum).NumX3Vars * TableLookup(TableNum).NumX4Vars * TableLookup(TableNum).NumX5Vars * TableLookup(TableNum).NumX6Vars;

        NumbersOffset = 17 + NumIVars + TableLookup(TableNum).NumX1Vars + TableLookup(TableNum).NumX2Vars + TableLookup(TableNum).NumX3Vars +
                        TableLookup(TableNum).NumX4Vars + TableLookup(TableNum).NumX5Vars + TableLookup(TableNum).NumX6Vars + 1;

        // initialize NumX2Vars to 1 so the DO loops work correctly
        if (NumIVars == 1) TableLookup(TableNum).NumX2Vars = 1;

        for (NumDataSets = 1; NumDataSets <= TotalDataSets; ++NumDataSets) {

            if (NumIVars == 3) {
                if (ReadFromFile) {

                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> Var3;
                        ReadStat = flags.ios();
                    }

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }

                    if (ReadStat < GoodIOStatValue) goto Label1000;

                } else {
                    Var3 = Numbers(NumbersOffset);
                    ++NumbersOffset;

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }
                }
            } else if (NumIVars == 4) {
                if (ReadFromFile) {

                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> Var3 >> Var4;
                        ReadStat = flags.ios();
                    }

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3 << "  " << Var4;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }

                    if (ReadStat < GoodIOStatValue) goto Label1000;

                } else {
                    Var3 = Numbers(NumbersOffset);
                    Var4 = Numbers(NumbersOffset + 1);
                    NumbersOffset += 2;

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3 << "  " << Var4;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }
                }
            } else if (NumIVars == 5) {
                if (ReadFromFile) {

                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> Var3 >> Var4 >> Var5;
                        ReadStat = flags.ios();
                    }

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3 << "  " << Var4 << "  " << Var5;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }

                    if (ReadStat < GoodIOStatValue) goto Label1000;

                } else {
                    Var3 = Numbers(NumbersOffset);
                    Var4 = Numbers(NumbersOffset + 1);
                    Var5 = Numbers(NumbersOffset + 2);
                    NumbersOffset += 3;

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3 << "  " << Var4 << "  " << Var5;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }
                }
            } else if (NumIVars == 6) {
                if (ReadFromFile) {

                    {
                        IOFlags flags;
                        ObjexxFCL::gio::read(FileNum, fmtLD, flags) >> Var3 >> Var4 >> Var5 >> Var6;
                        ReadStat = flags.ios();
                    }

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3 << "  " << Var4 << "  " << Var5 << " " << Var6;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }

                    if (ReadStat < GoodIOStatValue) goto Label1000;

                } else {
                    Var3 = Numbers(NumbersOffset);
                    Var4 = Numbers(NumbersOffset + 1);
                    Var5 = Numbers(NumbersOffset + 2);
                    Var6 = Numbers(NumbersOffset + 3);
                    NumbersOffset += 4;

                    if (EchoTableDataToEio) {
                        ObjexxFCL::gio::write(CharTableData, fmtLD) << Var3 << "  " << Var4 << "  " << Var5 << " " << Var6;
                        ObjexxFCL::gio::write(OutputFileInits, fmtA) << trim(CharTableData);
                    }
                }
            }

            if (NumIVars > 2) {
                Var3Index = 0;
                // match the independent variable values to the allowed values to find the index. Input must match allowed values.
                for (I = 1; I <= TableLookup(TableNum).NumX3Vars; ++I) {
                    if (Var3 != TableLookup(TableNum).X3Var(I)) continue;
                    Var3Index = I;
                    break;
                }
                if (Var3Index == 0) {
                    ShowSevereError("GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve(CurveNum).Name + "\"");
                    ShowContinueError("...The value of the 3rd independent variable (" + RoundSigDigits(Var3, 9) +
                                      ") does not match the values listed as valid entries for this independent variable.");
                    ShowContinueError("...Valid entries are: ");
                    if (TableLookup(TableNum).NumX3Vars >= 1) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(1), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 2) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(2), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 3) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(3), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 4) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(4), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 5) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(5), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 6) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(6), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 7) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(7), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 8) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(8), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 9) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(9), 9));
                    if (TableLookup(TableNum).NumX3Vars >= 10) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X3Var(10), 9));
                    ShowContinueError("...This occurs for data set = " + RoundSigDigits(DataSetCount + 1) + " in data file = " + FileName);
                    ErrorsFound = true;
                    Var3Index = 1;
                }
            } else {
                Var3Index = 1;
            }

            if (NumIVars > 3) {
                Var4Index = 0;
                // match the independent variable values to the allowed values to find the index. Input must match allowed values.
                for (I = 1; I <= TableLookup(TableNum).NumX4Vars; ++I) {
                    if (Var4 != TableLookup(TableNum).X4Var(I)) continue;
                    Var4Index = I;
                    break;
                }
                if (Var4Index == 0) {
                    ShowSevereError("GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve(CurveNum).Name + "\"");
                    ShowContinueError("...The value of the 4th independent variable (" + RoundSigDigits(Var4, 9) +
                                      ") does not match the values listed as valid entries for this independent variable.");
                    ShowContinueError("...Valid entries are: ");
                    if (TableLookup(TableNum).NumX4Vars >= 1) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(1), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 2) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(2), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 3) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(3), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 4) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(4), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 5) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(5), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 6) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(6), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 7) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(7), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 8) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(8), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 9) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(9), 9));
                    if (TableLookup(TableNum).NumX4Vars >= 10) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X4Var(10), 9));
                    ShowContinueError("...This occurs for data set = " + RoundSigDigits(DataSetCount + 1) + " in data file = " + FileName);
                    ErrorsFound = true;
                    Var4Index = 1;
                }
            } else {
                Var4Index = 1;
            }

            if (NumIVars > 4) {
                Var5Index = 0;
                // match the independent variable values to the allowed values to find the index. Input must match allowed values.
                for (I = 1; I <= TableLookup(TableNum).NumX5Vars; ++I) {
                    if (Var5 != TableLookup(TableNum).X5Var(I)) continue;
                    Var5Index = I;
                    break;
                }
                if (Var5Index == 0 && NumIVars > 4) {
                    ShowSevereError("GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve(CurveNum).Name + "\"");
                    ShowContinueError("...The value of the 5th independent variable (" + RoundSigDigits(Var5, 9) +
                                      ") does not match the values listed as valid entries for this independent variable.");
                    ShowContinueError("...Valid entries are: ");
                    if (TableLookup(TableNum).NumX5Vars >= 1) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(1), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 2) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(2), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 3) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(3), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 4) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(4), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 5) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(5), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 6) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(6), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 7) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(7), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 8) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(8), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 9) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(9), 9));
                    if (TableLookup(TableNum).NumX5Vars >= 10) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X5Var(10), 9));
                    ShowContinueError("...This occurs for data set = " + RoundSigDigits(DataSetCount + 1) + " in data file = " + FileName);
                    ErrorsFound = true;
                    Var5Index = 1;
                }
            } else {
                Var5Index = 1;
            }

            if (NumIVars > 5) {
                Var6Index = 0;
                // match the independent variable values to the allowed values to find the index. Input must match allowed values.
                for (I = 1; I <= TableLookup(TableNum).NumX6Vars; ++I) {
                    if (Var6 != TableLookup(TableNum).X6Var(I)) continue;
                    Var6Index = I;
                    break;
                }
                if (Var6Index == 0 && NumIVars > 5) {
                    ShowSevereError("GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve(CurveNum).Name + "\"");
                    ShowContinueError("...The value of the 6th independent variable (" + RoundSigDigits(Var6, 9) +
                                      ") does not match the values listed as valid entries for this independent variable.");
                    ShowContinueError("...Valid entries are: ");
                    if (TableLookup(TableNum).NumX6Vars >= 1) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(1), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 2) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(2), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 3) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(3), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 4) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(4), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 5) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(5), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 6) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(6), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 7) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(7), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 8) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(8), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 9) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(9), 9));
                    if (TableLookup(TableNum).NumX6Vars >= 10) ShowContinueError("..." + RoundSigDigits(TableLookup(TableNum).X6Var(10), 9));
                    ShowContinueError("...This occurs for data set = " + RoundSigDigits(DataSetCount + 1) + " in data file = " + FileName);
                    ErrorsFound = true;
                    Var6Index = 1;
                }
            } else {
                Var6Index = 1;
            }

            // now read in X1 | X2 matrix data set
            if (PerfCurve(CurveNum).X1SortOrder == ASCENDING) {
                if (PerfCurve(CurveNum).X2SortOrder == ASCENDING) {
                    for (J = 1; J <= TableLookup(TableNum).NumX2Vars; ++J) {
                        if (ReadFromFile) {

                            {
                                IOFlags flags;
                                for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                                    ObjexxFCL::gio::read(FileNum, fmtLD, flags) >>
                                        TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I);
                                }
                                ReadStat = flags.ios();
                            }

                            if (ReadStat < GoodIOStatValue) goto Label1000;

                        } else {

                            for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                                TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I) = Numbers(NumbersOffset);
                                ++NumbersOffset;
                            }
                        }
                    }
                } else { // PerfCurve(CurveNum)%X2SortOrder == DESCENDING

                    for (J = TableLookup(TableNum).NumX2Vars; J >= 1; --J) {

                        if (ReadFromFile) {

                            {
                                IOFlags flags;
                                for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                                    ObjexxFCL::gio::read(FileNum, fmtLD, flags) >>
                                        TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I);
                                }
                                ReadStat = flags.ios();
                            }

                            if (ReadStat < GoodIOStatValue) goto Label1000;

                        } else {

                            for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                                TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I) =
                                    Numbers(NumbersOffset) / TableData(TableNum).NormalPoint;
                                ++NumbersOffset;
                            }
                        }
                    }
                }
            } else { // PerfCurve(CurveNum)%X1SortOrder == DESCENDING
                if (PerfCurve(CurveNum).X2SortOrder == ASCENDING) {

                    for (J = 1; J <= TableLookup(TableNum).NumX2Vars; ++J) {

                        if (ReadFromFile) {

                            {
                                IOFlags flags;
                                for (I = TableLookup(TableNum).NumX1Vars; I >= 1; --I) {
                                    ObjexxFCL::gio::read(FileNum, fmtLD, flags) >>
                                        TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I);
                                }
                                ReadStat = flags.ios();
                            }

                            if (ReadStat < GoodIOStatValue) goto Label1000;

                        } else {

                            for (I = TableLookup(TableNum).NumX1Vars; I >= 1; --I) {
                                TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I) = Numbers(NumbersOffset);
                                ++NumbersOffset;
                            }
                        }
                    }
                } else {
                    for (J = TableLookup(TableNum).NumX2Vars; J >= 1; --J) {

                        if (ReadFromFile) {

                            {
                                IOFlags flags;
                                for (I = TableLookup(TableNum).NumX1Vars; I >= 1; --I) {
                                    ObjexxFCL::gio::read(FileNum, fmtLD, flags) >>
                                        TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I);
                                }
                                ReadStat = flags.ios();
                            }

                            if (ReadStat < GoodIOStatValue) goto Label1000;

                        } else {

                            for (I = TableLookup(TableNum).NumX1Vars; I >= 1; --I) {
                                TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I) = Numbers(NumbersOffset);
                                ++NumbersOffset;
                            }
                        }
                    }
                }
            }

            for (J = 1; J <= TableLookup(TableNum).NumX2Vars; ++J) {

                // write data to eio file in ascending order
                if (EchoTableDataToEio) {
                    for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                        ObjexxFCL::gio::write(OutputFileInits, fmtLD, non_adv)
                            << TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }

                // normalize the data according to the user entered normal point
                for (I = 1; I <= TableLookup(TableNum).NumX1Vars; ++I) {
                    TableLookup(TableNum).TableLookupZData(Var6Index, Var5Index, Var4Index, Var3Index, J, I) /= TableData(TableNum).NormalPoint;
                }
            }

            ++DataSetCount;
        }

        if (EchoTableDataToEio) {
            ObjexxFCL::gio::write(OutputFileInits, Format_131);
        }

    Label1000:;
        EOFonFile = true;
        if (ReadFromFile) ObjexxFCL::gio::close(FileNum);

        if (TotalDataSets < DataSetCount) {
            ShowSevereError("GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve(CurveNum).Name + "\"");
            ShowContinueError("...The required number of data sets (" + RoundSigDigits(TotalDataSets) +
                              ") is less than the number determined by the number and count of independent variables (" +
                              RoundSigDigits(DataSetCount) + ").");
        }

        return;

    Label999:;
        ShowSevereError("CurveManager: SearchTableDataFile: Could not open Table Data File, expecting it as file name = " + FileName);
        ShowContinueError("Certain run environments require a full path to be included with the file name in the input field.");
        ShowContinueError("Try again with putting full path and file name in the field.");
        ShowFatalError("Program terminates due to these conditions.");
    }

    Real64 DLAG(Real64 const XX,
                Real64 const YY,
                Array1S<Real64> X,
                Array1S<Real64> Y,
                Array2S<Real64> Z,
                int const NX,
                int const NY,
                int const M,
                int &IEXTX,
                int &IEXTY)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         AUTHOR: F.D.HAMMERLING, COMPUTING TECHNOLOGY CENTER, ORNL
        //       DATE WRITTEN   2010
        //       MODIFIED       Richard Raustad, FSEC
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //         DLAG IS A DOUBLE-PRECISION, TWO-DIMENSIONAL,
        //         LAGRANGIAN INTERPOLATION
        //           INPUT VARIABLES:
        //             XX        X-COORDINATE OF THE DESIRED INTERPOLATED POINT
        //             YY        Y-COORDINATE OF THE DESIRED INTERPOLATED POINT
        //             X         SINGLY DIMENSIONED ARRAY OF X-COORDINATES
        //             Y         SINGLY DIMENSIONED ARRAY OF Y-COORDINATES
        //             Z         DOUBLY DIMENSIONED ARRAY OF FUNCTION VALUES,
        //                       I.E. Z(I,J) = F( X(I), Y(J) )
        //             NX        NUMBER OF ELEMENTS IN THE X-ARRAY
        //             NY        NUMBER OF ELEMENTS IN THE Y-ARRAY
        //             M         THE SQUARE ROOT OF THE NUMBER OF POINTS TO BE
        //                       CONSIDERED IN THE INTERPOLATION - NUMBER OF
        //                       POINTS IN EACH DIRECTION
        //             ID        THE FIRST DIMENSION OF THE Z-ARRAY (AT LEAST NX)
        //           OUTPUT VARIABLES:
        //             IEXTX     =1, IF EXTRAPOLATION OCCURED ABOVE THE X-ARRAY
        //                       =0, IF INTERPOLATION OCCURED
        //                       =-1, IF EXTRAPOLATION OCCURED BELOW THE X-ARRAY
        //             IEXTY     SAME FOR THE Y-ARRAY AS IEXTX IS FOR THE X-ARRAY
        //      THIS PROGRAM WAS MODIFIED AUGUST 1984 BY CJ EMERSON TO INSURE
        //      THAT ITERATIVE CALLS TO DLAG USE THE SAME POINTS FOR INTERPOLATION.
        //      ISXPT(ISYPT) ARE CHOSEN FROM THE UPPER END OF THE INTERVAL.
        //      ALSO THE PROGRAM WAS MODIFIED SO THAT EXTRAPOLATION ALWAYS USES
        //      AT MOST TWO POINTS.
        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 DLAG;

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // DIMENSION Z(ID,NY),X(NX),Y(NY),XLAG(100)
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:!
        int I;
        int ISXPT(0);
        int IEXPT(0);
        int J;
        int ISYPT(0);
        int IEYPT(0);
        int K;
        int M1;
        Real64 MIDX;
        Real64 MIDY;

        //       INITIALIZE
        bool QUITX = false;
        bool QUITY = false;
        IEXTX = 0;
        IEXTY = 0;
        //       The following code has been upgraded to current Fortran standards
        //       See Starteam Revision 33, August 17, 2010 for legacy code if comparison is needed
        //       FIND THE RANGE OF INTERPOLATION ALONG X
        M1 = M;               // number of points to be interpolated (X direction is first)
        if (M1 > NX) M1 = NX; // limit to number of X points if necessary

        //       loop through X data and find the first x-coordinate less than the interpolated point
        //       if the interpolation point is less than or greater than the X data then linearly extrapolate
        //       linear extrapolation uses only 2 points (M1=2)
        for (I = 1; I <= NX; ++I) {
            if (XX - X(I) < 0.0) {
                MIDX = I; // found X point just greater than interpolation point
                if (MIDX == 1) {
                    IEXTX = -1;         // extrapolating at the lower bound of x
                    if (M1 > 2) M1 = 2; // limit to linear extrapolation
                }
                ISXPT = MIDX - ((M1 + 1) / 2); // calculate starting point in X array
                if (ISXPT <= 0) ISXPT = 1;     // limit to first element in X array
                IEXPT = ISXPT + M1 - 1;        // calculate ending point in X array
                if (IEXPT > NX) {
                    ISXPT = NX - M1 + 1; // if upper X array boundary exceeded, recalculate starting point
                    IEXPT = NX;          // limit ending point to upper boundary of X array
                }
                break;
            } else if (XX - X(I) == 0.0) { // interpolation point is equal to element in X array
                QUITX = true;              // exact interpolation point found in X array, do not interpolate
                break;
            } else if (I == NX) {    // interpolation point is greater than max X value
                IEXTX = 1;           // extrapolating at the upper bound of X
                if (M1 > 2) M1 = 2;  // limit to linear extrapolation
                ISXPT = NX - M1 + 1; // calculate starting point in X array
                IEXPT = NX;          // ending point equals upper bound of X array
                break;
            }
        }

        M1 = M;               // number of points to be interpolated (Y direction is second)
        if (M1 > NY) M1 = NY; // limit to number of Y points if necessary

        for (J = 1; J <= NY; ++J) {
            if (YY - Y(J) < 0.0) {
                MIDY = J; // found Y point just greater than interpolation point
                if (MIDY <= 1) {
                    IEXTY = -1;         // extrapolating at the lower bound of y
                    if (M1 > 2) M1 = 2; // limit to linear extrapolation
                }
                ISYPT = MIDY - ((M1 + 1) / 2); // calculate starting point in Y array
                if (ISYPT <= 0) ISYPT = 1;     // limit to first element in array
                IEYPT = ISYPT + M1 - 1;        // calculate ending point in X array
                if (IEYPT > NY) {
                    ISYPT = NY - M1 + 1; // if upper Y array boundary exceeded, recalculate starting point
                    IEYPT = NY;          // limit ending point to upper boundary of Y array
                }
                break;
            } else if (YY - Y(J) == 0.0) { // interpolation point is equal to element in Y array
                QUITY = true;              // exact interpolation point found in Y array, do not interpolate
                break;
            } else if (J == NY) {    // interpolation point is greater than max Y value
                IEXTY = 1;           // extrapolating at the upper bound of Y
                if (M1 > 2) M1 = 2;  // limit to linear extrapolation
                ISYPT = NY - M1 + 1; // calculate starting point in Y array
                IEYPT = NY;          // ending point equals upper bound of Y array
                break;
            }
        }

        if (QUITX && QUITY) {
            DLAG = Z(J, I);           // found exact X and Y point in Z array
        } else if (QUITX && !QUITY) { // only interpolate in Y direction
            Array1D<Real64> XLAG(IEYPT);
            for (int l = ISYPT; l <= IEYPT; ++l) {
                XLAG(l) = Z(l, I); // store X's at each Y (I = midpoint of array from above)
            }
            Interpolate_Lagrange(YY, XLAG, Y, ISYPT, IEYPT, DLAG);    // now interpolate these X's
        } else if (!QUITX && QUITY) {                                 // only interpolate in X direction
            Interpolate_Lagrange(XX, Z(J, _), X, ISXPT, IEXPT, DLAG); // (:,J) interpolate X array at fixed Y (J here)
        } else {                                                      // else interpolate in X and Y directions
            Array1D<Real64> XLAG(IEYPT);
            for (K = ISYPT; K <= IEYPT; ++K) {
                Interpolate_Lagrange(XX, Z(K, _), X, ISXPT, IEXPT, XLAG(K)); // (:,K) interpolate X array at all Y's (K here)
            }
            Interpolate_Lagrange(YY, XLAG, Y, ISYPT, IEYPT, DLAG); // final interpolation of X array
        }

        return DLAG;
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

    Real64 PerformanceTableObject(int const CurveIndex,        // index of curve in curve array
                                  Real64 const Var1,           // 1st independent variable
                                  Optional<Real64 const> Var2, // 2nd independent variable
                                  Optional<Real64 const> Var3  // 3rd independent variable
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   May 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given the curve index and the values of 1 or 2 independent variables,
        // returns the value of an equipment performance table lookup.

        // Return value
        Real64 TableValue;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        Real64 V1; // 1st independent variable after limits imposed
        Real64 V2; // 2nd independent variable after limits imposed
        Real64 V3; // 3rd independent variable after limits imposed
        Real64 TempX1Low;
        Real64 TempX1High;
        Real64 TempX2Low;
        Real64 TempX2High;
        // INTEGER   :: ATempX1LowPtr(1)
        // INTEGER   :: ATempX1HighPtr(1)
        // INTEGER   :: ATempX2LowPtr(1)
        // INTEGER   :: ATempX2HighPtr(1)
        int TempX1LowPtr(0);
        int TempX1HighPtr(0);
        int TempX2LowPtr(0);
        int TempX2HighPtr(0);
        Real64 X1Frac;
        Real64 X2Frac;
        Real64 X1ValLow;
        Real64 X1ValHigh;
        // INTEGER   :: MaxSizeArray
        int X1Val;
        int X2Val;
        int TableIndex;

        TableIndex = PerfCurve(CurveIndex).TableIndex;

        V1 = max(min(Var1, PerfCurve(CurveIndex).Var1Max), PerfCurve(CurveIndex).Var1Min);

        if (present(Var2)) {
            V2 = max(min(Var2, PerfCurve(CurveIndex).Var2Max), PerfCurve(CurveIndex).Var2Min);
        } else {
            V2 = 0.0;
        }

        if (present(Var3)) {
            V3 = max(min(Var3, PerfCurve(CurveIndex).Var3Max), PerfCurve(CurveIndex).Var3Min);
        } else {
            V3 = 0.0;
        }

        {
            auto const SELECT_CASE_var(PerfCurve(CurveIndex).NumDims);
            if (SELECT_CASE_var == 1) {

                TempX1Low = minval(PerfCurveTableData(TableIndex).X1);
                TempX1High = maxval(PerfCurveTableData(TableIndex).X1);
                if (V1 <= TempX1Low) {
                    TempX1LowPtr = 1;
                    TempX1HighPtr = 1;
                } else if (V1 >= TempX1High) {
                    TempX1LowPtr = size(PerfCurveTableData(TableIndex).X1);
                    TempX1HighPtr = TempX1LowPtr;
                } else {
                    for (X1Val = 1; X1Val <= isize(PerfCurveTableData(TableIndex).X1); ++X1Val) {
                        if (V1 >= PerfCurveTableData(TableIndex).X1(X1Val)) TempX1LowPtr = X1Val;
                    }
                    if (V1 == PerfCurveTableData(TableIndex).X1(TempX1LowPtr)) {
                        TempX1HighPtr = TempX1LowPtr;
                    } else {
                        TempX1HighPtr = TempX1LowPtr + 1;
                    }
                }
                if (TempX1LowPtr == TempX1HighPtr) {
                    TableValue = PerfCurveTableData(TableIndex).Y(1, TempX1LowPtr);
                } else {
                    X1Frac = (V1 - PerfCurveTableData(TableIndex).X1(TempX1LowPtr)) /
                             (PerfCurveTableData(TableIndex).X1(TempX1HighPtr) - PerfCurveTableData(TableIndex).X1(TempX1LowPtr));
                    TableValue = X1Frac * PerfCurveTableData(TableIndex).Y(1, TempX1HighPtr) +
                                 (1 - X1Frac) * PerfCurveTableData(TableIndex).Y(1, TempX1LowPtr);
                }

            } else if (SELECT_CASE_var == 2) {

                TempX1Low = minval(PerfCurveTableData(TableIndex).X1);
                TempX1High = maxval(PerfCurveTableData(TableIndex).X1);
                if (V1 <= TempX1Low) {
                    TempX1LowPtr = 1;
                    TempX1HighPtr = 1;
                } else if (V1 >= TempX1High) {
                    TempX1LowPtr = size(PerfCurveTableData(TableIndex).X1);
                    TempX1HighPtr = TempX1LowPtr;
                } else {
                    for (X1Val = 1; X1Val <= isize(PerfCurveTableData(TableIndex).X1); ++X1Val) {
                        if (V1 >= PerfCurveTableData(TableIndex).X1(X1Val)) TempX1LowPtr = X1Val;
                    }
                    if (V1 == PerfCurveTableData(TableIndex).X1(TempX1LowPtr)) {
                        TempX1HighPtr = TempX1LowPtr;
                    } else {
                        TempX1HighPtr = TempX1LowPtr + 1;
                    }
                }
                TempX2Low = minval(PerfCurveTableData(TableIndex).X2);
                TempX2High = maxval(PerfCurveTableData(TableIndex).X2);

                if (V2 <= TempX2Low) {
                    TempX2LowPtr = 1;
                    TempX2HighPtr = 1;
                } else if (V2 >= TempX2High) {
                    TempX2LowPtr = size(PerfCurveTableData(TableIndex).X2);
                    TempX2HighPtr = TempX2LowPtr;
                } else {
                    for (X2Val = 1; X2Val <= isize(PerfCurveTableData(TableIndex).X2); ++X2Val) {
                        if (V2 >= PerfCurveTableData(TableIndex).X2(X2Val)) TempX2LowPtr = X2Val;
                    }
                    if (V2 == PerfCurveTableData(TableIndex).X2(TempX2LowPtr)) {
                        TempX2HighPtr = TempX2LowPtr;
                    } else {
                        TempX2HighPtr = TempX2LowPtr + 1;
                    }
                }

                if (TempX1LowPtr == TempX1HighPtr) {
                    if (TempX2LowPtr == TempX2HighPtr) {
                        TableValue = PerfCurveTableData(TableIndex).Y(TempX2LowPtr, TempX1LowPtr);
                    } else {
                        X2Frac = (V2 - PerfCurveTableData(TableIndex).X2(TempX2LowPtr)) /
                                 (PerfCurveTableData(TableIndex).X2(TempX2HighPtr) - PerfCurveTableData(TableIndex).X2(TempX2LowPtr));
                        TableValue = X2Frac * PerfCurveTableData(TableIndex).Y(TempX2HighPtr, TempX1LowPtr) +
                                     (1 - X2Frac) * PerfCurveTableData(TableIndex).Y(TempX2LowPtr, TempX1LowPtr);
                    }
                } else {
                    X1Frac = (V1 - PerfCurveTableData(TableIndex).X1(TempX1LowPtr)) /
                             (PerfCurveTableData(TableIndex).X1(TempX1HighPtr) - PerfCurveTableData(TableIndex).X1(TempX1LowPtr));
                    if (TempX2LowPtr == TempX2HighPtr) {
                        TableValue = X1Frac * PerfCurveTableData(TableIndex).Y(TempX2LowPtr, TempX1HighPtr) +
                                     (1 - X1Frac) * PerfCurveTableData(TableIndex).Y(TempX2LowPtr, TempX1LowPtr);
                    } else {
                        X1ValLow = X1Frac * PerfCurveTableData(TableIndex).Y(TempX2LowPtr, TempX1HighPtr) +
                                   (1 - X1Frac) * PerfCurveTableData(TableIndex).Y(TempX2LowPtr, TempX1LowPtr);
                        X1ValHigh = X1Frac * PerfCurveTableData(TableIndex).Y(TempX2HighPtr, TempX1HighPtr) +
                                    (1 - X1Frac) * PerfCurveTableData(TableIndex).Y(TempX2HighPtr, TempX1LowPtr);
                        X2Frac = (V2 - PerfCurveTableData(TableIndex).X2(TempX2LowPtr)) /
                                 (PerfCurveTableData(TableIndex).X2(TempX2HighPtr) - PerfCurveTableData(TableIndex).X2(TempX2LowPtr));
                        TableValue = X2Frac * X1ValHigh + (1 - X2Frac) * X1ValLow;
                    }
                }

            } else {
                TableValue = 0.0;
                ShowSevereError("Errors found in table output calculation for " + PerfCurve(CurveIndex).Name);
                ShowContinueError("...Possible causes are selection of Interpolation Method or Type or Number of Independent Variables or Points.");
                ShowFatalError("PerformanceTableObject: Previous error causes program termination.");
            }
        }

        if (PerfCurve(CurveIndex).CurveMinPresent) TableValue = max(TableValue, PerfCurve(CurveIndex).CurveMin);
        if (PerfCurve(CurveIndex).CurveMaxPresent) TableValue = min(TableValue, PerfCurve(CurveIndex).CurveMax);

        return TableValue;
    }

    Real64 TableLookupObject(int const CurveIndex,        // index of curve in curve array
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
        // returns the value of an equipment performance table lookup.

        // Return value
        Real64 TableValue;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        Real64 V1; // 1st independent variable after limits imposed
        Real64 V2; // 2nd independent variable after limits imposed
        Real64 V3; // 3rd independent variable after limits imposed
        Real64 V4; // 4th independent variable after limits imposed
        Real64 V5; // 5th independent variable after limits imposed
        Real64 V6; // 6th independent variable after limits imposed
        int NX;
        int NY;
        int NV3;
        int NV4;
        int NV5;
        int NV6;
        int TableIndex;
        // REAL(r64), ALLOCATABLE, DIMENSION(:)     :: ONEDVALS
        Array2D<Real64> TWODVALS;
        Array3D<Real64> THREEDVALS;
        Array4D<Real64> FOURDVALS;
        Array1D<Real64> VALSX;
        Array1D<Real64> VALSY;
        Array1D<Real64> VALSV3;
        Array1D<Real64> VALSV4;
        Array1D<Real64> VALSV5;
        Array1D<Real64> VALSV6;
        // REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: HPVAL
        // REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: HPVALS
        // REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DVLTRN
        // REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: FiveDArray
        // REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: FourDArray
        // REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: ThreeDArray
        // REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: TwoDArray
        // REAL(r64), ALLOCATABLE, DIMENSION(:) :: OneDArray
        int IV3;
        int IV4;
        int IV5;
        int IV6;
        int IEXTX;
        int IEXTY;
        int IEXTV3;
        int IEXTV4;
        int IEXTV5;
        int IEXTV6;
        int NUMPT;

        TableIndex = PerfCurve(CurveIndex).TableIndex;

        V1 = max(min(Var1, PerfCurve(CurveIndex).Var1Max), PerfCurve(CurveIndex).Var1Min);

        if (present(Var2)) {
            V2 = max(min(Var2, PerfCurve(CurveIndex).Var2Max), PerfCurve(CurveIndex).Var2Min);
            if (PerfCurve(CurveIndex).NumDims < 2) {
                if (PerfCurve(CurveIndex).NumIVHighErrorIndex == 0) {
                    ShowWarningError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name +
                                     "\"");
                    ShowContinueError("...Excess number of independent variables (2) passed to subroutine when only 1 is required. The excess "
                                      "arguments are ignored.");
                    PerfCurve(CurveIndex).NumIVHighErrorIndex += 1;
                }
            }
        } else {
            if (PerfCurve(CurveIndex).NumDims > 1) {
                if (PerfCurve(CurveIndex).NumIVLowErrorIndex == 0) {
                    ShowSevereError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name + "\"");
                    ShowContinueError("...Insufficient number of independent variables (1) passed to subroutine when at least 2 are required.");
                }
                ShowRecurringWarningErrorAtEnd(PerfCurve(CurveIndex).ObjectType + " \"" + PerfCurve(CurveIndex).Name +
                                                   "\": Insufficient number of independent variables warning continues...",
                                               PerfCurve(CurveIndex).NumIVLowErrorIndex,
                                               1.0,
                                               1.0);
            }
            V2 = 0.0;
        }

        if (present(Var3)) {
            V3 = max(min(Var3, PerfCurve(CurveIndex).Var3Max), PerfCurve(CurveIndex).Var3Min);
            if (PerfCurve(CurveIndex).NumDims < 3) {
                if (PerfCurve(CurveIndex).NumIVHighErrorIndex == 0) {
                    ShowWarningError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name +
                                     "\"");
                    ShowContinueError("...Excess number of independent variables (3) passed to subroutine when 2 or less are required. The excess "
                                      "arguments are ignored.");
                    PerfCurve(CurveIndex).NumIVHighErrorIndex += 1;
                }
            }
        } else {
            if (PerfCurve(CurveIndex).NumDims > 2) {
                if (PerfCurve(CurveIndex).NumIVLowErrorIndex == 0) {
                    ShowSevereError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name + "\"");
                    ShowContinueError("...Insufficient number of independent variables (2) passed to subroutine when at least 3 are required.");
                }
                ShowRecurringWarningErrorAtEnd(PerfCurve(CurveIndex).ObjectType + " \"" + PerfCurve(CurveIndex).Name +
                                                   "\": Insufficient number of independent variables warning continues...",
                                               PerfCurve(CurveIndex).NumIVLowErrorIndex,
                                               2.0,
                                               2.0);
            }
            V3 = 0.0;
        }

        if (present(Var4)) {
            V4 = max(min(Var4, PerfCurve(CurveIndex).Var4Max), PerfCurve(CurveIndex).Var4Min);
            if (PerfCurve(CurveIndex).NumDims < 4) {
                if (PerfCurve(CurveIndex).NumIVHighErrorIndex == 0) {
                    ShowWarningError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name +
                                     "\"");
                    ShowContinueError("...Excess number of independent variables (4) passed to subroutine when 3 or less are required. The excess "
                                      "arguments are ignored.");
                    PerfCurve(CurveIndex).NumIVHighErrorIndex += 1;
                }
            }
        } else {
            if (PerfCurve(CurveIndex).NumDims > 3) {
                if (PerfCurve(CurveIndex).NumIVLowErrorIndex == 0) {
                    ShowSevereError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name + "\"");
                    ShowContinueError("...Insufficient number of independent variables (3) passed to subroutine when at least 4 are required.");
                }
                ShowRecurringWarningErrorAtEnd(PerfCurve(CurveIndex).ObjectType + " \"" + PerfCurve(CurveIndex).Name +
                                                   "\": Insufficient number of independent variables warning continues...",
                                               PerfCurve(CurveIndex).NumIVLowErrorIndex,
                                               3.0,
                                               3.0);
            }
            V4 = 0.0;
        }

        if (present(Var5)) {
            V5 = max(min(Var5, PerfCurve(CurveIndex).Var5Max), PerfCurve(CurveIndex).Var5Min);
            if (PerfCurve(CurveIndex).NumDims < 5) {
                if (PerfCurve(CurveIndex).NumIVHighErrorIndex == 0) {
                    ShowWarningError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name +
                                     "\"");
                    ShowContinueError("...Excess number of independent variables (5) passed to subroutine when 4 or less are required. The excess "
                                      "arguments are ignored.");
                    PerfCurve(CurveIndex).NumIVHighErrorIndex += 1;
                }
            }
        } else {
            if (PerfCurve(CurveIndex).NumDims > 4) {
                if (PerfCurve(CurveIndex).NumIVLowErrorIndex == 0) {
                    ShowSevereError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name + "\"");
                    ShowContinueError("...Insufficient number of independent variables (4) passed to subroutine when at least 5 are required.");
                }
                ShowRecurringWarningErrorAtEnd(PerfCurve(CurveIndex).ObjectType + " \"" + PerfCurve(CurveIndex).Name +
                                                   "\": Insufficient number of independent variables warning continues...",
                                               PerfCurve(CurveIndex).NumIVLowErrorIndex,
                                               4.0,
                                               4.0);
            }
            V5 = 0.0;
        }

        if (present(Var6)) {
            V6 = max(min(Var6, PerfCurve(CurveIndex).Var6Max), PerfCurve(CurveIndex).Var6Min);
            if (PerfCurve(CurveIndex).NumDims < 6) {
                if (PerfCurve(CurveIndex).NumIVHighErrorIndex == 0) {
                    ShowSevereError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name + "\"");
                    ShowContinueError("...Excess number of independent variables (6) passed to subroutine when 5 or less are required.");
                }
                ShowRecurringWarningErrorAtEnd(PerfCurve(CurveIndex).ObjectType + " \"" + PerfCurve(CurveIndex).Name +
                                                   "\": Excess number of independent variables warning continues...",
                                               PerfCurve(CurveIndex).NumIVHighErrorIndex,
                                               6.0,
                                               6.0);
            }
        } else {
            if (PerfCurve(CurveIndex).NumDims > 5) {
                if (PerfCurve(CurveIndex).NumIVLowErrorIndex == 0) {
                    ShowSevereError("TableLookupObject: " + PerfCurve(CurveIndex).ObjectType + "\"" + PerfCurve(CurveIndex).Name + "\"");
                    ShowContinueError("...Insufficient number of independent variables (5) passed to subroutine when at least 6 are required.");
                }
                ShowRecurringWarningErrorAtEnd(PerfCurve(CurveIndex).ObjectType + " \"" + PerfCurve(CurveIndex).Name +
                                                   "\": Insufficient number of independent variables warning continues...",
                                               PerfCurve(CurveIndex).NumIVLowErrorIndex,
                                               5.0,
                                               5.0);
            }
            V6 = 0.0;
        }

        {
            auto const SELECT_CASE_var(PerfCurve(CurveIndex).NumDims);
            if (SELECT_CASE_var == 1) {
                NX = TableLookup(TableIndex).NumX1Vars;
                NY = 1;
                NUMPT = TableLookup(TableIndex).InterpolationOrder;
                VALSX.allocate(NX);
                VALSX = TableLookup(TableIndex).X1Var;
                TableValue =
                    DLAG(V1, VALSX(1), VALSX, VALSX, TableLookup(TableIndex).TableLookupZData(1, 1, 1, 1, _, _), NX, NY, NUMPT, IEXTX, IEXTY);
                VALSX.deallocate();
            } else if (SELECT_CASE_var == 2) {
                NX = TableLookup(TableIndex).NumX1Vars;
                NY = TableLookup(TableIndex).NumX2Vars;
                NUMPT = TableLookup(TableIndex).InterpolationOrder;
                VALSX.allocate(NX);
                VALSX = TableLookup(TableIndex).X1Var;
                VALSY.allocate(NY);
                VALSY = TableLookup(TableIndex).X2Var;
                TableValue = DLAG(V1, V2, VALSX, VALSY, TableLookup(TableIndex).TableLookupZData(1, 1, 1, 1, _, _), NX, NY, NUMPT, IEXTX, IEXTY);
                VALSX.deallocate();
                VALSY.deallocate();
            } else if (SELECT_CASE_var == 3) {
                NX = TableLookup(TableIndex).NumX1Vars;
                NY = TableLookup(TableIndex).NumX2Vars;
                NV3 = TableLookup(TableIndex).NumX3Vars;
                NUMPT = TableLookup(TableIndex).InterpolationOrder;
                VALSX.allocate(NX);
                VALSX = TableLookup(TableIndex).X1Var;
                VALSY.allocate(NY);
                VALSY = TableLookup(TableIndex).X2Var;
                VALSV3.allocate(NV3);
                VALSV3 = TableLookup(TableIndex).X3Var;
                TWODVALS.allocate(1, NV3);
                // perform 2-D interpolation of X (V1) and Y (V2) and save in 2-D array
                for (IV3 = 1; IV3 <= NV3; ++IV3) {
                    TWODVALS(1, IV3) =
                        DLAG(V1, V2, VALSX, VALSY, TableLookup(TableIndex).TableLookupZData(1, 1, 1, IV3, _, _), NX, NY, NUMPT, IEXTX, IEXTY);
                }
                if (NV3 == 1) {
                    TableValue = TWODVALS(1, 1);
                } else {
                    TableValue = DLAG(V3, 1.0, VALSV3, VALSV3, TWODVALS, NV3, 1, NUMPT, IEXTV3, IEXTV4);
                }
                TWODVALS.deallocate();
                VALSX.deallocate();
                VALSY.deallocate();
                VALSV3.deallocate();
            } else if (SELECT_CASE_var == 4) {
                NX = TableLookup(TableIndex).NumX1Vars;
                NY = TableLookup(TableIndex).NumX2Vars;
                NV3 = TableLookup(TableIndex).NumX3Vars;
                NV4 = TableLookup(TableIndex).NumX4Vars;
                NUMPT = TableLookup(TableIndex).InterpolationOrder;
                VALSX.allocate(NX);
                VALSX = TableLookup(TableIndex).X1Var;
                VALSY.allocate(NY);
                VALSY = TableLookup(TableIndex).X2Var;
                VALSV3.allocate(NV3);
                VALSV3 = TableLookup(TableIndex).X3Var;
                VALSV4.allocate(NV4);
                VALSV4 = TableLookup(TableIndex).X4Var;
                TWODVALS.allocate(NV4, NV3);
                // perform 2-D interpolation of X (V1) and Y (V2) and save in 2-D array
                for (IV4 = 1; IV4 <= NV4; ++IV4) {
                    for (IV3 = 1; IV3 <= NV3; ++IV3) {
                        TWODVALS(IV4, IV3) =
                            DLAG(V1, V2, VALSX, VALSY, TableLookup(TableIndex).TableLookupZData(1, 1, IV4, IV3, _, _), NX, NY, NUMPT, IEXTX, IEXTY);
                    }
                }
                // final interpolation of 2-D array in V3 and V4
                TableValue = DLAG(V3, V4, VALSV3, VALSV4, TWODVALS, NV3, NV4, NUMPT, IEXTV3, IEXTV4);
                TWODVALS.deallocate();
                VALSX.deallocate();
                VALSY.deallocate();
                VALSV3.deallocate();
                VALSV4.deallocate();
            } else if (SELECT_CASE_var == 5) {
                NX = TableLookup(TableIndex).NumX1Vars;
                NY = TableLookup(TableIndex).NumX2Vars;
                NV3 = TableLookup(TableIndex).NumX3Vars;
                NV4 = TableLookup(TableIndex).NumX4Vars;
                NV5 = TableLookup(TableIndex).NumX5Vars;
                NUMPT = TableLookup(TableIndex).InterpolationOrder;
                VALSX.allocate(NX);
                VALSX = TableLookup(TableIndex).X1Var;
                VALSY.allocate(NY);
                VALSY = TableLookup(TableIndex).X2Var;
                VALSV3.allocate(NV3);
                VALSV3 = TableLookup(TableIndex).X3Var;
                VALSV4.allocate(NV4);
                VALSV4 = TableLookup(TableIndex).X4Var;
                VALSV5.allocate(NV5);
                VALSV5 = TableLookup(TableIndex).X5Var;
                THREEDVALS.allocate(NV5, NV4, NV3);
                for (IV5 = 1; IV5 <= NV5; ++IV5) {
                    for (IV4 = 1; IV4 <= NV4; ++IV4) {
                        for (IV3 = 1; IV3 <= NV3; ++IV3) {
                            THREEDVALS(IV5, IV4, IV3) = DLAG(
                                V1, V2, VALSX, VALSY, TableLookup(TableIndex).TableLookupZData(1, IV5, IV4, IV3, _, _), NX, NY, NUMPT, IEXTX, IEXTY);
                        }
                    }
                }
                TWODVALS.allocate(1, NV5);
                for (IV5 = 1; IV5 <= NV5; ++IV5) {
                    TWODVALS(1, IV5) = DLAG(V3, V4, VALSV3, VALSV4, THREEDVALS(IV5, _, _), NV3, NV4, NUMPT, IEXTX, IEXTY);
                }
                if (NV5 == 1) {
                    TableValue = TWODVALS(1, 1);
                } else {
                    TableValue = DLAG(V5, 1.0, VALSV5, VALSV5, TWODVALS, NV5, 1, NUMPT, IEXTV5, IEXTV4);
                }
                TWODVALS.deallocate();
                THREEDVALS.deallocate();
                VALSX.deallocate();
                VALSY.deallocate();
                VALSV3.deallocate();
                VALSV4.deallocate();
                VALSV5.deallocate();
            } else if (SELECT_CASE_var == 6) {
                NX = TableLookup(TableIndex).NumX1Vars;
                NY = TableLookup(TableIndex).NumX2Vars;
                NV3 = TableLookup(TableIndex).NumX3Vars;
                NV4 = TableLookup(TableIndex).NumX4Vars;
                NV5 = TableLookup(TableIndex).NumX5Vars;
                NV6 = TableLookup(TableIndex).NumX6Vars;
                NUMPT = TableLookup(TableIndex).InterpolationOrder;
                VALSX.allocate(NX);
                VALSX = TableLookup(TableIndex).X1Var;
                VALSY.allocate(NY);
                VALSY = TableLookup(TableIndex).X2Var;
                VALSV3.allocate(NV3);
                VALSV3 = TableLookup(TableIndex).X3Var;
                VALSV4.allocate(NV4);
                VALSV4 = TableLookup(TableIndex).X4Var;
                VALSV5.allocate(NV5);
                VALSV5 = TableLookup(TableIndex).X5Var;
                VALSV6.allocate(NV6);
                VALSV6 = TableLookup(TableIndex).X6Var;
                // perform 2-D interpolation of 6D array at V1, V2, and store in 4D array
                FOURDVALS.allocate(NV6, NV5, NV4, NV3);
                for (IV6 = 1; IV6 <= NV6; ++IV6) {
                    for (IV5 = 1; IV5 <= NV5; ++IV5) {
                        for (IV4 = 1; IV4 <= NV4; ++IV4) {
                            for (IV3 = 1; IV3 <= NV3; ++IV3) {
                                FOURDVALS(IV6, IV5, IV4, IV3) = DLAG(V1,
                                                                     V2,
                                                                     VALSX,
                                                                     VALSY,
                                                                     TableLookup(TableIndex).TableLookupZData(IV6, IV5, IV4, IV3, _, _),
                                                                     NX,
                                                                     NY,
                                                                     NUMPT,
                                                                     IEXTX,
                                                                     IEXTY);
                            }
                        }
                    }
                }
                // perform 2-D interpolation of 4D array at V3, V4, and store in 2D array
                TWODVALS.allocate(NV6, NV5);
                for (IV6 = 1; IV6 <= NV6; ++IV6) {
                    for (IV5 = 1; IV5 <= NV5; ++IV5) {
                        TWODVALS(IV6, IV5) = DLAG(V3, V4, VALSV3, VALSV4, FOURDVALS(IV6, IV5, _, _), NV3, NV4, NUMPT, IEXTV3, IEXTV4);
                    }
                }
                // perform 2-D interpolation of 2D array to find final value
                TableValue = DLAG(V5, V6, VALSV5, VALSV6, TWODVALS, NV5, NV6, NUMPT, IEXTV5, IEXTV6);
                TWODVALS.deallocate();
                FOURDVALS.deallocate();
                VALSX.deallocate();
                VALSY.deallocate();
                VALSV3.deallocate();
                VALSV4.deallocate();
                VALSV5.deallocate();
            } else {
                TableValue = 0.0;
                ShowSevereError("Errors found in table output calculation for " + PerfCurve(CurveIndex).Name);
                ShowContinueError("...Possible causes are selection of Interpolation Method or Type or Number of Independent Variables or Points.");
                ShowFatalError("PerformanceTableObject: Previous error causes program termination.");
            }
        }

        if (PerfCurve(CurveIndex).CurveMinPresent) TableValue = max(TableValue, PerfCurve(CurveIndex).CurveMin);
        if (PerfCurve(CurveIndex).CurveMaxPresent) TableValue = min(TableValue, PerfCurve(CurveIndex).CurveMax);

        return TableValue;
    }

    void SolveRegression(int &CurveNum,                      // index to performance curve
                         std::string &TableType,             // tabular data object type
                         std::string &CurveName,             // performance curve name
                         Array1S<Real64> RawDataX,           // table data X values (1st independent variable)
                         Array1S<Real64> RawDataY,           // table data Y values (dependent variables)
                         Optional<Array1S<Real64>> RawDataX2 // table data X2 values (2nd independent variable)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Given the curve index and the values of 1 or 2 independent variables,
        // calls the curve or table routine to return the value of an equipment performance curve or table.
        // The solution requires use of linear algebra and forms a matrix of sums of the data set.
        // For a linear equation of the form Z = a + bX + cX^2, the general solution is as follows.
        // Phi = SUM1ToN[Zi - f(Xi)]^2 = SUM1ToN[Zi - (a+bX+cX^2)]^2 = minimum
        // substitue Y = X^2 in the equations above.
        // then set up the partials of Phi with respect to a, the partial of Phi with respect to b, etc.
        // PartialPhiRespectToa = 2 * SUM1ToN[1*(Zi-(a+bXi+cYi))] = 0
        // PartialPhiRespectTob = 2 * SUM1ToN[Xi(Zi-(a+bXi+cYi))] = 0
        // PartialPhiRespectToc = 2 * SUM1ToN[Yi(Zi-(a+bXi+cYi))] = 0
        // then set up the square matrix by solving the above partials.
        // SUM1ToN(Zi)   = a * SUM1ToN(1)  + b * SUM1ToN(Xi)   + c * SUM1ToN(Yi)
        // SUM1ToN(ZiXi) = a * SUM1ToN(Xi) + b * SUM1ToN(Xi)^2 + c * SUM1ToN(XiYi)
        // SUM1ToN(ZiYi) = a * SUM1ToN(Yi) + b * SUM1ToN(XiYi) + c * SUM1ToN(Yi)^2
        // the matirx (A) is then the 3x3 matrix on the right, with a solution of the 1x3 matrix on the left
        // Note symmetry about the diagonal.
        // (i.e., A(1,2)=A(2,1), A(1,3)=A(3,1), A(3,2)=A(2,3), and diagonal are all squared terms)
        //      _                                          _              _              _
        //     |  SUM1ToN(1)   SUM1ToN(Xi)   SUM1ToN(Yi)    |            |  SUM1ToN(Zi)   |
        // A = |  SUM1ToN(Xi)  SUM1ToN(Xi)^2 SUM1ToN(XiYi)  |  Results = |  SUM1ToN(ZiXi) |
        //     |_ SUM1ToN(Yi)  SUM1ToN(XiYi) SUM1ToN(Yi)^2 _|            |_ SUM1ToN(ZiYi)_|
        // The linear algebra equation is then solved using foward elimination and reverse substitution
        // This solution (Results) provides the coefficients of the associated performance curve (a,b,and c in the eq. above).
        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::DisplayAdvancedReportVariables;
        using DataGlobals::OutputFileInits;
        using General::RoundSigDigits;
        using General::TrimSigDigits;

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 X; // linear algebra equation coefficients
        Real64 X2;
        Real64 Y;
        Real64 V;
        Real64 U;
        Real64 T;
        Real64 Z;
        int MatrixSize; // square matrix array size (MatrixSize,MatrixSize)
        int LoopCount;  // loop counter
        int N;          // loop variables
        int i;
        int j;
        int k;
        Real64 C;        // intermediate calculation of a constant in matrix solution
        Real64 sX;       // sum of the X
        Real64 sX2;      // sum of the X^2
        Real64 sX3;      // sum of the X^3
        Real64 sY;       // sum of the Y
        Real64 sY2;      // sum of the Y^2
        Real64 sV;       // sum of the V
        Real64 sV2;      // sum of the V^2
        Real64 sU;       // sum of the U
        Real64 sU2;      // sum of the U^2
        Real64 sT;       // sum of the T
        Real64 sT2;      // sum of the T^2
        Real64 sXY;      // sum of the XY
        Real64 sXV;      // sum of the XV
        Real64 sXU;      // sum of the XU
        Real64 sXT;      // sum of the XT
        Real64 sYV;      // sum of the TV
        Real64 sYU;      // sum of the YU
        Real64 sYT;      // sum of the YT
        Real64 sVU;      // sum of the VU
        Real64 sVT;      // sum of the VT
        Real64 sUT;      // sum of the UT
        Real64 Results1; // regression coefficient #1
        Real64 Results2; // regression coefficient #2
        Real64 Results3; // regression coefficient #3
        Real64 Results4; // regression coefficient #4
        Real64 Results5; // regression coefficient #5
        Real64 Results6; // regression coefficient #6
        Real64 MinX;     // equation variable min/max statistics
        Real64 MaxX;
        Real64 MinX2;
        Real64 MaxX2;
        Real64 MinY;
        Real64 MaxY;
        Real64 Mean; // statistical parameters
        Real64 RSquared;
        Real64 StandardError;
        Real64 Est(0.0);
        Array1D<Real64> Results; // performance curve coefficients
        Array2D<Real64> A;       // linear algebra matrix
        std::string StrCurve;    // string representation of curve type
        static bool WriteHeaderOnce(true);
        bool EchoTableDataToEio; // logical set equal to global and used to report to eio file

        // Formats
        static ObjexxFCL::gio::Fmt Format_110("('! <CREATING NEW CURVE OBJECT>')");
        static ObjexxFCL::gio::Fmt Format_130("('CREATING NEW CURVE OBJECT')");
        static ObjexxFCL::gio::Fmt Format_140("('! Input as ',A,' \"',A,'\"')");
        static ObjexxFCL::gio::Fmt Format_150("('! RSquared       = ',A)");
        static ObjexxFCL::gio::Fmt Format_160("('! Standard Error = ',A)");
        static ObjexxFCL::gio::Fmt Format_170("('! Sample Size    = ',A)");
        static ObjexxFCL::gio::Fmt Format_180("('Curve:',A,',')");
        static ObjexxFCL::gio::Fmt Format_190("('FromTable_',A,',  !- Name')");
        static ObjexxFCL::gio::Fmt Format_200("('  ',A,',  !- Coefficient1 Constant')");
        static ObjexxFCL::gio::Fmt Format_210("('  ',A,',  !- Coefficient2 x')");
        static ObjexxFCL::gio::Fmt Format_300("('  ',A,',  !- Minimum Value of x')");
        static ObjexxFCL::gio::Fmt Format_310("('  ',A,',  !- Maximum Value of x')");
        static ObjexxFCL::gio::Fmt Format_340("('  ',A,',  !- Minimum Curve Output')");
        static ObjexxFCL::gio::Fmt Format_350("('  ',A,';  !- Maximum Curve Output')");
        static ObjexxFCL::gio::Fmt Format_360("('END CREATING NEW CURVE OBJECT')");
        static ObjexxFCL::gio::Fmt Format_220("('  ',A,',  !- Coefficient3 x**2')");
        static ObjexxFCL::gio::Fmt Format_230("('  ',A,',  !- !- Coefficient4 x**3')");
        static ObjexxFCL::gio::Fmt Format_240("('  ',A,',  !- Coefficient4 y')");
        static ObjexxFCL::gio::Fmt Format_250("('  ',A,',  !- !- Coefficient5 x**4')");
        static ObjexxFCL::gio::Fmt Format_260("('  ',A,',  !- Coefficient5 y**2')");
        static ObjexxFCL::gio::Fmt Format_270("('  ',A,',  !- Coefficient5 xy')");
        static ObjexxFCL::gio::Fmt Format_280("('  ',A,',  !- Coefficient6 x*y')");
        static ObjexxFCL::gio::Fmt Format_290("('  ',A,',  !- Coefficient6 x**2y')");
        static ObjexxFCL::gio::Fmt Format_320("('  ',A,',  !- Minimum Value of y')");
        static ObjexxFCL::gio::Fmt Format_330("('  ',A,',  !- Maximum Value of y')");

        EchoTableDataToEio = DisplayAdvancedReportVariables;

        {
            auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
            if (SELECT_CASE_var == Linear) {
                MatrixSize = 2;
                StrCurve = "Linear";
            } else if (SELECT_CASE_var == Quadratic) {
                MatrixSize = 3;
                StrCurve = "Quadratic";
            } else if (SELECT_CASE_var == Cubic) {
                MatrixSize = 4;
                StrCurve = "Cubic";
            } else if (SELECT_CASE_var == Quartic) {
                MatrixSize = 5;
                StrCurve = "Quartic";
            } else if (SELECT_CASE_var == BiQuadratic) {
                MatrixSize = 6;
                StrCurve = "BiQuadratic";
            } else if (SELECT_CASE_var == QuadraticLinear) {
                MatrixSize = 6;
                StrCurve = "QuadraticLinear";
            } else {
                ShowSevereError(PerfCurve(CurveNum).ObjectType + ": \"" + PerfCurve(CurveNum).Name + "\"");
                ShowContinueError("Invalid curve type for regression.");
                return;
            }
        }

        if (isize(RawDataX) < MatrixSize) {
            ShowSevereError(PerfCurve(CurveNum).ObjectType + ": \"" + PerfCurve(CurveNum).Name + "\"");
            ShowContinueError("Insufficient data to calculate regression coefficients.");
            ShowContinueError("Required data pairs = " + RoundSigDigits(MatrixSize));
            ShowContinueError("Entered data pairs  = " + RoundSigDigits(size(RawDataX)));
            ShowContinueError("Setting interpolation type equal to LinearInterpolationOfTable and simulation continues.");
            PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;
            return;
        }

        Results.dimension(MatrixSize, 0.0);
        A.allocate(MatrixSize, MatrixSize);
        //   ' Sum data
        N = 0;
        sX = 0.0;
        sX2 = 0.0;
        sY = 0.0;
        sY2 = 0.0;
        sV = 0.0;
        sV2 = 0.0;
        sU = 0.0;
        sU2 = 0.0;
        sT = 0.0;
        sT2 = 0.0;
        sXY = 0.0;
        sXV = 0.0;
        sXU = 0.0;
        sXT = 0.0;
        sYV = 0.0;
        sYU = 0.0;
        sYT = 0.0;
        sVU = 0.0;
        sVT = 0.0;
        sUT = 0.0;
        Results = 0.0;
        Results1 = 0.0;
        Results2 = 0.0;
        Results3 = 0.0;
        Results4 = 0.0;
        Results5 = 0.0;
        Results6 = 0.0;
        X2 = 0.0;
        Y = 0.0;
        V = 0.0;
        U = 0.0;
        T = 0.0;
        for (LoopCount = 1; LoopCount <= isize(RawDataX); ++LoopCount) {
            X = RawDataX(LoopCount);
            if (present(RawDataX2)) X2 = RawDataX2()(LoopCount);
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if ((SELECT_CASE_var == Linear) || (SELECT_CASE_var == Quadratic) || (SELECT_CASE_var == Cubic) || (SELECT_CASE_var == Quartic)) {
                    Y = X * X;
                    V = X * Y;
                    U = X * V;
                } else if (SELECT_CASE_var == BiQuadratic) {
                    Y = X * X;
                    V = X2;
                    U = V * V;
                    T = X * X2;
                } else if (SELECT_CASE_var == QuadraticLinear) {
                    Y = X * X;
                    V = X2;
                    U = X * V;
                    T = Y * X2;
                } else {
                }
            }
            Z = RawDataY(LoopCount);
            ++N;               // Count
            sX += X;           // Sum X
            sX2 += X * X;      // Sum X*X
            sY += Y;           // Sum Y
            sY2 += Y * Y;      // Sum Y*Y
            sV += V;           // Sum V
            sV2 += V * V;      // Sum V*V
            sU += U;           // Sum U
            sU2 += U * U;      // Sum U*U
            sT += T;           // Sum T
            sT2 += T * T;      // Sum T*T
            sXY += X * Y;      // Sum XY
            sXV += X * V;      // Sum XV
            sXU += X * U;      // Sum XU
            sXT += X * T;      // Sum XT
            sYV += Y * V;      // Sum YV
            sYU += Y * U;      // Sum YU
            sYT += Y * T;      // Sum YT
            sVU += V * U;      // Sum VU
            sVT += V * T;      // Sum VT
            sUT += U * T;      // Sum UT
            Results1 += Z;     // Sum Z
            Results2 += Z * X; // Sum ZX
            Results3 += Z * Y; // Sum ZY
            Results4 += Z * V; // Sum ZV
            Results5 += Z * U; // Sum ZU
            Results6 += Z * T; // Sum ZT
        }

        Results(1) = Results1;
        Results(2) = Results2;
        {
            auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
            if (SELECT_CASE_var == Linear) {
            } else if (SELECT_CASE_var == Quadratic) {
                Results(3) = Results3;
            } else if (SELECT_CASE_var == Cubic) {
                Results(3) = Results3;
                Results(4) = Results4;
            } else if (SELECT_CASE_var == Quartic) {
                Results(3) = Results3;
                Results(4) = Results4;
                Results(5) = Results5;
            } else if ((SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                Results(3) = Results3;
                Results(4) = Results4;
                Results(5) = Results5;
                Results(6) = Results6;
            }
        }

        Mean = Results(1) / N;

        //    ' Form "A" Matrix
        A(1, 1) = double(N);
        A(2, 1) = sX;
        A(2, 2) = sX2;
        {
            auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
            if (SELECT_CASE_var == Linear) {
            } else if (SELECT_CASE_var == Quadratic) {
                A(3, 1) = sY;
                A(3, 2) = sXY;
                A(3, 3) = sY2;
            } else if (SELECT_CASE_var == Cubic) {
                A(3, 1) = sY;
                A(4, 1) = sV;
                A(3, 2) = sXY;
                A(4, 2) = sXV;
                A(3, 3) = sY2;
                A(4, 3) = sYV;
                A(4, 4) = sV2;
            } else if (SELECT_CASE_var == Quartic) {
                A(3, 1) = sY;
                A(4, 1) = sV;
                A(5, 1) = sU;
                A(3, 2) = sXY;
                A(4, 2) = sXV;
                A(5, 2) = sXU;
                A(3, 3) = sY2;
                A(4, 3) = sYV;
                A(5, 3) = sYU;
                A(4, 4) = sV2;
                A(5, 4) = sVU;
                A(5, 5) = sU2;
            } else if ((SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                A(3, 1) = sY;
                A(4, 1) = sV;
                A(5, 1) = sU;
                A(6, 1) = sT;
                A(3, 2) = sXY;
                A(4, 2) = sXV;
                A(5, 2) = sXU;
                A(6, 2) = sXT;
                A(3, 3) = sY2;
                A(4, 3) = sYV;
                A(5, 3) = sYU;
                A(6, 3) = sYT;
                A(4, 4) = sV2;
                A(5, 4) = sVU;
                A(6, 4) = sVT;
                A(5, 5) = sU2;
                A(6, 5) = sUT;
                A(6, 6) = sT2;
            } else {
            }
        }

        //  copy elements to bottom half of symmetrical square matrix
        for (i = 1; i <= MatrixSize - 1; ++i) {
            for (j = i + 1; j <= MatrixSize; ++j) {
                A(i, j) = A(j, i);
            }
        }

        //   Forward Eliminiation
        for (i = 1; i <= MatrixSize - 1; ++i) {
            if (A(i, i) == 0.0) {
                ShowSevereError("SolveRegression: Zero value on the diagonal.");
                ShowContinueError("Setting interpolation type equal to LinearInterpolationOfTable and simulation continues.");
                PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;
                return;
            }
            for (j = i + 1; j <= MatrixSize; ++j) {
                //      find the ratio of the element to the one above it
                C = A(i, j) / A(i, i);
                //      replace the element by reducing it by the ratio multiplied by the element above it
                //      this makes the bottom half of symmetrical square matix 0's
                for (k = i; k <= MatrixSize; ++k) {
                    A(k, j) -= C * A(k, i);
                }
                Results(j) -= C * Results(i);
            }
        }

        //    ' Back Substitution
        if (A(MatrixSize, MatrixSize) == 0.0) {
            ShowSevereError("SolveRegression: Zero value on the diagonal end point.");
            ShowContinueError("Setting interpolation type equal to LinearInterpolationOfTable and simulation continues.");
            PerfCurve(CurveNum).InterpolationType = LinearInterpolationOfTable;
            return;
        }
        //  now starting at the lower right corner of the matrix solve for the last coefficient
        Results(MatrixSize) /= A(MatrixSize, MatrixSize);
        //  substitute that coefficient back into the equation above it and solve for the 2nd to last coefficient
        //  proceed until all coefficients are found
        for (i = MatrixSize - 1; i >= 1; --i) {
            C = Results(i);
            for (j = 1; j <= MatrixSize - i; ++j) {
                C -= A(i + j, i) * Results(i + j);
            }
            Results(i) = C / A(i, i);
        }

        //  calculate the regression statistics
        sX = 0.0;
        sX2 = 0.0;
        sX3 = 0.0;
        MinX = 9999999.0;
        MaxX = -9999999.0;
        MinX2 = 9999999.0;
        MaxX2 = -9999999.0;
        MinY = 9999999.0;
        MaxY = -9999999.0;
        for (LoopCount = 1; LoopCount <= N; ++LoopCount) {
            X = RawDataX(LoopCount);
            if (present(RawDataX2)) X2 = RawDataX2()(LoopCount);
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if ((SELECT_CASE_var == Linear) || (SELECT_CASE_var == Quadratic) || (SELECT_CASE_var == Cubic) || (SELECT_CASE_var == Quartic)) {
                    Y = X * X;
                    V = X * Y;
                    U = X * V;
                } else if (SELECT_CASE_var == BiQuadratic) {
                    Y = X * X;
                    V = X2;
                    U = V * V;
                    T = X * X2;
                } else if (SELECT_CASE_var == QuadraticLinear) {
                    Y = X * X;
                    V = X2;
                    U = X * V;
                    T = Y * X2;
                } else {
                }
            }
            Z = RawDataY(LoopCount);
            if (MinX > X) MinX = X;
            if (MaxX < X) MaxX = X;
            if (MinX2 > X2) MinX2 = X2;
            if (MaxX2 < X2) MaxX2 = X2;
            if (MinY > Z) MinY = Z;
            if (MaxY < Z) MaxY = Z;

            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if (SELECT_CASE_var == Linear) {
                    Est = Results(1) + X * Results(2);
                } else if (SELECT_CASE_var == Quadratic) {
                    Est = Results(1) + X * Results(2) + Y * Results(3);
                } else if (SELECT_CASE_var == Cubic) {
                    Est = Results(1) + X * Results(2) + Y * Results(3) + V * Results(4);
                } else if (SELECT_CASE_var == Quartic) {
                    Est = Results(1) + X * Results(2) + Y * Results(3) + V * Results(4) + U * Results(5);
                } else if ((SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                    Est = Results(1) + X * Results(2) + Y * Results(3) + V * Results(4) + U * Results(5) + T * Results(6);
                } else {
                }
            }
            sX += (Est - Mean) * (Est - Mean);
            sX2 += (Z - Mean) * (Z - Mean);
            sX3 += (Z - Est) * (Z - Est);
        }
        if (sX2 != 0.0) {
            RSquared = sX / sX2;
        } else {
            RSquared = 0.0;
        }
        if (N > MatrixSize) {
            StandardError = std::sqrt(sX3 / (N - MatrixSize));
        } else {
            StandardError = 0.0;
        }

        {
            auto const SELECT_CASE_var(PerfCurve(CurveNum).InterpolationType);
            if (SELECT_CASE_var == LinearInterpolationOfTable) {
            } else if (SELECT_CASE_var == EvaluateCurveToLimits) {
                if (PerfCurve(CurveNum).Var1MinPresent) {
                    MinX = PerfCurve(CurveNum).Var1Min;
                }
                if (PerfCurve(CurveNum).Var1MaxPresent) {
                    MaxX = PerfCurve(CurveNum).Var1Max;
                }
                if (PerfCurve(CurveNum).Var2MinPresent) {
                    MinX2 = PerfCurve(CurveNum).Var2Min;
                }
                if (PerfCurve(CurveNum).Var2MaxPresent) {
                    MaxX2 = PerfCurve(CurveNum).Var2Max;
                }
                if (PerfCurve(CurveNum).CurveMinPresent) {
                    MinY = PerfCurve(CurveNum).CurveMin;
                }
                if (PerfCurve(CurveNum).CurveMaxPresent) {
                    MaxY = PerfCurve(CurveNum).CurveMax;
                }
            } else {
            }
        }

        // echo new curve object to eio file
        if (EchoTableDataToEio) {
            if (WriteHeaderOnce) {
                ObjexxFCL::gio::write(OutputFileInits, Format_110);
                WriteHeaderOnce = false;
            }

            ObjexxFCL::gio::write(OutputFileInits, Format_130);
            ObjexxFCL::gio::write(OutputFileInits, Format_140) << TableType << CurveName;
            ObjexxFCL::gio::write(OutputFileInits, Format_150) << RoundSigDigits(RSquared, 10);
            ObjexxFCL::gio::write(OutputFileInits, Format_160) << RoundSigDigits(StandardError, 10);
            ObjexxFCL::gio::write(OutputFileInits, Format_170) << TrimSigDigits(N);
            ObjexxFCL::gio::write(OutputFileInits, Format_180) << StrCurve;
            ObjexxFCL::gio::write(OutputFileInits, Format_190) << CurveName;
            ObjexxFCL::gio::write(OutputFileInits, Format_200) << RoundSigDigits(Results(1), 10);
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if ((SELECT_CASE_var == Linear) || (SELECT_CASE_var == Quadratic) || (SELECT_CASE_var == Cubic) || (SELECT_CASE_var == Quartic) ||
                    (SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_210) << RoundSigDigits(Results(2), 10);
                } else {
                }
            }
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if ((SELECT_CASE_var == Quadratic) || (SELECT_CASE_var == Cubic) || (SELECT_CASE_var == Quartic) ||
                    (SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_220) << RoundSigDigits(Results(3), 10);
                } else {
                }
            }
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if ((SELECT_CASE_var == Cubic) || (SELECT_CASE_var == Quartic)) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_230) << RoundSigDigits(Results(4), 10);
                } else if ((SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_240) << RoundSigDigits(Results(4), 10);
                } else {
                }
            }
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if (SELECT_CASE_var == Quartic) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_250) << RoundSigDigits(Results(5), 10);
                } else if (SELECT_CASE_var == BiQuadratic) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_260) << RoundSigDigits(Results(5), 10);
                } else if (SELECT_CASE_var == QuadraticLinear) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_270) << RoundSigDigits(Results(5), 10);
                } else {
                }
            }
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if (SELECT_CASE_var == BiQuadratic) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_280) << RoundSigDigits(Results(6), 10);
                } else if (SELECT_CASE_var == QuadraticLinear) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_290) << RoundSigDigits(Results(6), 10);
                } else {
                }
            }
            ObjexxFCL::gio::write(OutputFileInits, Format_300) << RoundSigDigits(MinX, 10);
            ObjexxFCL::gio::write(OutputFileInits, Format_310) << RoundSigDigits(MaxX, 10);
            {
                auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
                if (SELECT_CASE_var == Quartic) {
                } else if ((SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                    ObjexxFCL::gio::write(OutputFileInits, Format_320) << RoundSigDigits(MinX2, 10);
                    ObjexxFCL::gio::write(OutputFileInits, Format_330) << RoundSigDigits(MaxX2, 10);
                } else {
                }
            }
            ObjexxFCL::gio::write(OutputFileInits, Format_340) << RoundSigDigits(MinY, 10);
            ObjexxFCL::gio::write(OutputFileInits, Format_350) << RoundSigDigits(MaxY, 10);
            ObjexxFCL::gio::write(OutputFileInits, Format_360);
        }

        // save results in performance curve structure
        {
            auto const SELECT_CASE_var(PerfCurve(CurveNum).CurveType);
            if (SELECT_CASE_var == Linear) {
                PerfCurve(CurveNum).Coeff1 = Results(1);
                PerfCurve(CurveNum).Coeff2 = Results(2);
            } else if (SELECT_CASE_var == Quadratic) {
                PerfCurve(CurveNum).Coeff1 = Results(1);
                PerfCurve(CurveNum).Coeff2 = Results(2);
                PerfCurve(CurveNum).Coeff3 = Results(3);
            } else if (SELECT_CASE_var == Cubic) {
                PerfCurve(CurveNum).Coeff1 = Results(1);
                PerfCurve(CurveNum).Coeff2 = Results(2);
                PerfCurve(CurveNum).Coeff3 = Results(3);
                PerfCurve(CurveNum).Coeff4 = Results(4);
            } else if (SELECT_CASE_var == Quartic) {
                PerfCurve(CurveNum).Coeff1 = Results(1);
                PerfCurve(CurveNum).Coeff2 = Results(2);
                PerfCurve(CurveNum).Coeff3 = Results(3);
                PerfCurve(CurveNum).Coeff4 = Results(4);
                PerfCurve(CurveNum).Coeff5 = Results(5);
            } else if ((SELECT_CASE_var == BiQuadratic) || (SELECT_CASE_var == QuadraticLinear)) {
                PerfCurve(CurveNum).Coeff1 = Results(1);
                PerfCurve(CurveNum).Coeff2 = Results(2);
                PerfCurve(CurveNum).Coeff3 = Results(3);
                PerfCurve(CurveNum).Coeff4 = Results(4);
                PerfCurve(CurveNum).Coeff5 = Results(5);
                PerfCurve(CurveNum).Coeff6 = Results(6);
            } else {
            }
        }

        PerfCurve(CurveNum).Var1Min = MinX;
        PerfCurve(CurveNum).Var1Max = MaxX;
        PerfCurve(CurveNum).Var2Min = MinX2;
        PerfCurve(CurveNum).Var2Max = MaxX2;

        A.deallocate();
        Results.deallocate();
    }

    void Interpolate_Lagrange(Real64 const DataPoint,        // point used for interpolating output (x)
                              Array1S<Real64> FunctionArray, // array of output data (Y's)
                              Array1S<Real64> Ordinate,      // array of input data (X's)
                              int const ISPT,                // the starting point in the interpolated array
                              int const IEPT,                // the ending point in the interpolated array
                              Real64 &ALAG                   // the interpolated output (y or F(x) in equation above)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Solves the lagrange polynomial equation for interpolation. For second-order:
        // F(x) = y1 * ((x-x2)(x-x3) / (x1-x2)(x1-x3)) +
        //        y2 * ((x-x1)(x-x3) / (x2-x1)(x2-x3)) +
        //        y3 * ((x-x1)(x-x2) / (x3-x1)(x3-x2))
        // where xn, yn represent data points 1-n, and x represents the interpolation point.
        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //        DIMENSION FunctionAry(IEPT),Ordinate(IEPT)
        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Lagrange; // intermediate variable

        ALAG = 0.0;
        for (int J = ISPT; J <= IEPT; ++J) {
            Lagrange = 1.0;
            Real64 const Ordinate_J(Ordinate(J));
            for (int K = ISPT; K <= IEPT; ++K) {
                if (K != J) {
                    Lagrange *= ((DataPoint - Ordinate(K)) / (Ordinate_J - Ordinate(K)));
                }
            }
            ALAG += Lagrange * FunctionArray(J);
        }
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
    double GetNormalPoint(int const CurveIndex)
    {

        if (CurveIndex > 0 && CurveIndex <= NumCurves && PerfCurve(CurveIndex).TableIndex > 0) {
            const int tableIndex = PerfCurve(CurveIndex).TableIndex;
            return TableData(tableIndex).NormalPoint;
        } else {
            std::string s = std::to_string(CurveIndex);
            ShowWarningError("GetNormalPoint: CurveIndex not in range of curves, CurveIndex requested  " + s);
            return -1;
        }
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
        static bool ErrsFound(false); // Set to true if errors in input, fatal at end of routine
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
        static bool FrictionFactorErrorHasOccurred(false);

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

    int GetCurveInterpolationMethodNum(int const CurveIndex) // index of curve in curve array
    {

        // PURPOSE OF THIS FUNCTION:
        // get the interpolation type integer identifier for tables

        // Return value
        int TableInterpolationMethodNum;

        if (CurveIndex > 0) {
            TableInterpolationMethodNum = PerfCurve(CurveIndex).InterpolationType;
        } else {
            TableInterpolationMethodNum = 0;
        }

        return TableInterpolationMethodNum;
    }

    void ReadTwoVarTableDataFromFile(int const CurveNum, std::string &FileName, int &lineNum)
    {

        // PURPOSE OF THIS FUNCTION:
        // get data from an external file used to retrieve optical properties

        // METHODOLOGY EMPLOYED:
        // Recommended by NREL to read data

        // Using/Aliasing
        using DataSystemVariables::CheckForActualFileName;
        using DataSystemVariables::TempFullFileName;
        using General::splitString;

        int TableNum;
        bool FileExists;
        std::string line;

        CheckForActualFileName(FileName, FileExists, TempFullFileName);
        if (!FileExists) {
            ShowSevereError("CurveManager::ReadTwoVarTableDataFromFile: Could not open Table Data File, expecting it as file name = " + FileName);
            ShowContinueError("Certain run environments require a full path to be included with the file name in the input field.");
            ShowContinueError("Try again with putting full path and file name in the field.");
            ShowFatalError("Program terminates due to these conditions.");
        }

        TableNum = PerfCurve(CurveNum).TableIndex;
        std::ifstream infile(TempFullFileName);
        TableData(TableNum).X1.allocate(0);
        TableData(TableNum).X2.allocate(0);
        TableData(TableNum).Y.allocate(0);
        lineNum = 0;
        while (std::getline(infile, line)) {
            std::vector<std::string> strings = splitString(line, ',');
            lineNum++;
            if (strings.size() > 2) {
                TableData(TableNum).X1.push_back(std::stod(strings[0]));
                TableData(TableNum).X2.push_back(std::stod(strings[1]));
                TableData(TableNum).Y.push_back(std::stod(strings[2]));
            }
        }
        if (lineNum == 0) {
            ShowSevereError("CurveManager::ReadTwoVarTableDataFromFile: The data file is empty as file name = " + FileName);
            ShowFatalError("Program terminates due to these conditions.");
        }
    }

    void SetSameIndeVariableValues(int const TransCurveIndex, int const FRefleCurveIndex, int const BRefleCurveIndex)
    {

        // PURPOSE OF THIS FUNCTION:
        // Set up indenpendent varilable values the same in 3 table data

        // Using/Aliasing
        int X1TableNum;
        int X2TableNum;
        Array1D<Real64> XX1;
        Array1D<Real64> XX2;
        int i;
        int j;
        int k;
        int l;
        int m;
        Real64 XX;
        Real64 X1;
        Real64 X2;
        Real64 YY;
        Array1D<int> Tables;
        int TableNum;

        Tables.allocate(3);
        const Real64 tol = 1.0e-5;
        bool found;

        Tables(1) = PerfCurve(TransCurveIndex).TableIndex;
        Tables(2) = PerfCurve(FRefleCurveIndex).TableIndex;
        Tables(3) = PerfCurve(BRefleCurveIndex).TableIndex;

        // Set up independent variable size to cover all independent variable values in 3 tables
        for (TableNum = 1; TableNum <= int(Tables.size()); TableNum++) {
            if (TableNum == 1) {
                X1TableNum = TableLookup(Tables(TableNum)).NumX1Vars;
                X2TableNum = TableLookup(Tables(TableNum)).NumX2Vars;

                XX1.allocate(X1TableNum);
                XX2.allocate(X2TableNum);

                for (i = 1; i <= X1TableNum; i++) {
                    XX1(i) = TableLookup(Tables(TableNum)).X1Var(i);
                }
                for (i = 1; i <= X2TableNum; i++) {
                    XX2(i) = TableLookup(Tables(TableNum)).X2Var(i);
                }

            } else {
                // Add common wavelengths and angles
                X1TableNum = TableLookup(Tables(TableNum)).NumX1Vars;
                X2TableNum = TableLookup(Tables(TableNum)).NumX2Vars;
                for (i = 1; i <= X1TableNum; i++) {
                    XX = TableLookup(Tables(TableNum)).X1Var(i);
                    found = false;
                    for (j = 1; j <= int(XX1.size()); j++) {
                        if (fabs(XX1(j) - XX) < tol) {
                            found = true;
                        }
                    }
                    if (!found) {
                        XX1.push_back(XX);
                    }
                }
                for (i = 1; i <= X2TableNum; i++) {
                    XX = TableLookup(Tables(TableNum)).X2Var(i);
                    found = false;
                    for (j = 1; j <= int(XX2.size()); j++) {
                        if (fabs(XX2(j) - XX) < tol) {
                            found = true;
                        }
                    }
                    if (!found) {
                        XX2.push_back(XX);
                    }
                }
            }
        }

        // ascend sort
        std::sort(XX1.begin(), XX1.end());
        std::sort(XX2.begin(), XX2.end());

        for (TableNum = 1; TableNum <= int(Tables.size()); TableNum++) {
            if (int(XX2.size()) > TableLookup(Tables(TableNum)).NumX2Vars) {
                l = 0;
                for (i = 1; i <= int(XX2.size()); i++) {
                    if (fabs(XX2(i) - TableLookup(Tables(TableNum)).X2Var(i - l)) > tol) {
                        for (j = 2; j <= int(TableData(Tables(TableNum)).X2.size()); j++) {
                            if (TableData(Tables(TableNum)).X2(j - 1) < XX2(i) && TableData(Tables(TableNum)).X2(j) > XX2(i)) {
                                TableData(Tables(TableNum)).X1.push_back(TableData(Tables(TableNum)).X1(j));
                                TableData(Tables(TableNum)).X2.push_back(XX2(i));
                                YY = TableData(Tables(TableNum)).Y(j - 1) +
                                     (XX2(i) - TableData(Tables(TableNum)).X2(j - 1)) /
                                         (TableData(Tables(TableNum)).X2(j) - TableData(Tables(TableNum)).X2(j - 1)) *
                                         (TableData(Tables(TableNum)).Y(j) - TableData(Tables(TableNum)).Y(j - 1));
                                TableData(Tables(TableNum)).Y.push_back(YY);
                                X1 = TableData(Tables(TableNum)).X1(int(TableData(Tables(TableNum)).X2.size()));
                                X2 = TableData(Tables(TableNum)).X2(int(TableData(Tables(TableNum)).X2.size()));
                                YY = TableData(Tables(TableNum)).Y(int(TableData(Tables(TableNum)).X2.size()));
                                for (k = int(TableData(Tables(TableNum)).X2.size()) - 1; k >= j; k--) {
                                    TableData(Tables(TableNum)).X1(k + 1) = TableData(Tables(TableNum)).X1(k);
                                    TableData(Tables(TableNum)).X2(k + 1) = TableData(Tables(TableNum)).X2(k);
                                    TableData(Tables(TableNum)).Y(k + 1) = TableData(Tables(TableNum)).Y(k);
                                }
                                TableData(Tables(TableNum)).X1(j) = X1;
                                TableData(Tables(TableNum)).X2(j) = X2;
                                TableData(Tables(TableNum)).Y(j) = YY;
                            }
                        }
                        l++;
                    }
                }
            }

            if (int(XX1.size()) > TableLookup(Tables(TableNum)).NumX1Vars) {
                l = 0;
                for (i = 1; i <= int(XX1.size()); i++) {
                    if (fabs(XX1(i) - TableLookup(Tables(TableNum)).X1Var(i - l)) > tol) {
                        m = int(TableData(Tables(TableNum)).X1.size());
                        for (k = 1; k <= int(XX2.size()); k++) {
                            TableData(Tables(TableNum)).X1.push_back(TableData(Tables(TableNum)).X1(m - int(XX2.size()) + 1));
                            TableData(Tables(TableNum)).X2.push_back(TableData(Tables(TableNum)).X2(m - int(XX2.size()) + 1));
                            TableData(Tables(TableNum)).Y.push_back(TableData(Tables(TableNum)).Y(m - int(XX2.size()) + 1));
                        }
                        for (j = m / int(XX2.size()); j >= i; j--) {
                            for (k = 1; k <= int(XX2.size()); k++) {
                                TableData(Tables(TableNum)).X1(j * int(XX2.size()) + k) =
                                    TableData(Tables(TableNum)).X1((j - 1) * int(XX2.size()) + k);
                                TableData(Tables(TableNum)).X2(j * int(XX2.size() + k)) =
                                    TableData(Tables(TableNum)).X2((j - 1) * int(XX2.size()) + k);
                                TableData(Tables(TableNum)).Y(j * int(XX2.size()) + k) = TableData(Tables(TableNum)).Y((j - 1) * int(XX2.size()) + k);
                            }
                        }
                        YY = (XX1(i) - TableLookup(Tables(TableNum)).X1Var(i - l - 1)) /
                             (TableLookup(Tables(TableNum)).X1Var(i - l) - TableLookup(Tables(TableNum)).X1Var(i - l - 1));
                        for (k = 1; k <= int(XX2.size()); k++) {
                            TableData(Tables(TableNum)).X1((i - 1) * int(XX2.size()) + k) = XX1(i);
                            TableData(Tables(TableNum)).X2((i - 1) * int(XX2.size()) + k) = XX2(k);
                            TableData(Tables(TableNum)).Y((i - 1) * int(XX2.size()) + k) =
                                TableData(Tables(TableNum)).Y((i - 2) * int(XX2.size()) + k) +
                                YY * (TableData(Tables(TableNum)).Y(i * int(XX2.size()) + k) -
                                      TableData(Tables(TableNum)).Y((i - 2) * int(XX2.size()) + k));
                        }
                        l++;
                    }
                }
            }
        }

        // Re-organize performance curve table data structure
        for (TableNum = 1; TableNum <= int(Tables.size()); TableNum++) {
            PerfCurveTableData(Tables(TableNum)).X1.allocate(int(XX1.size()));
            PerfCurveTableData(Tables(TableNum)).X2.allocate(int(XX2.size()));
            PerfCurveTableData(Tables(TableNum)).Y.allocate(int(XX2.size()), int(XX1.size()));
            PerfCurveTableData(Tables(TableNum)).X1 = -9999999.0;
            PerfCurveTableData(Tables(TableNum)).X2 = -9999999.0;
            PerfCurveTableData(Tables(TableNum)).Y = -9999999.0;
            for (i = 1; i <= int(XX1.size()); ++i) {
                PerfCurveTableData(Tables(TableNum)).X1(i) = XX1(i);
                for (j = 1; j <= int(XX2.size()); ++j) {
                    PerfCurveTableData(Tables(TableNum)).X2(j) = XX2(j);
                    for (k = 1; k <= int(XX1.size()) * int(XX2.size()); ++k) {
                        if ((TableData(Tables(TableNum)).X1(k) == PerfCurveTableData(Tables(TableNum)).X1(i)) &&
                            (TableData(Tables(TableNum)).X2(k) == PerfCurveTableData(Tables(TableNum)).X2(j))) {
                            PerfCurveTableData(Tables(TableNum)).Y(j, i) = TableData(Tables(TableNum)).Y(k);
                        }
                    }
                }
            }
        }

        // Re-organize TableLookup data structure
        for (TableNum = 1; TableNum <= int(Tables.size()); TableNum++) {
            TableLookup(Tables(TableNum)).NumX1Vars = size(PerfCurveTableData(Tables(TableNum)).X1);
            TableLookup(Tables(TableNum)).NumX2Vars = size(PerfCurveTableData(Tables(TableNum)).X2);
            TableLookup(Tables(TableNum)).X1Var.allocate(TableLookup(Tables(TableNum)).NumX1Vars);
            TableLookup(Tables(TableNum)).X2Var.allocate(TableLookup(Tables(TableNum)).NumX2Vars);
            TableLookup(Tables(TableNum))
                .TableLookupZData.allocate(
                    1, 1, 1, 1, size(PerfCurveTableData(Tables(TableNum)).Y(_, 1)), size(PerfCurveTableData(Tables(TableNum)).Y(1, _)));
            TableLookup(Tables(TableNum)).X1Var = PerfCurveTableData(Tables(TableNum)).X1;
            TableLookup(Tables(TableNum)).X2Var = PerfCurveTableData(Tables(TableNum)).X2;
            TableLookup(Tables(TableNum)).TableLookupZData(1, 1, 1, 1, _, _) = PerfCurveTableData(Tables(TableNum)).Y(_, _);
        }

        XX1.deallocate();
        XX2.deallocate();
    }

    void SetCommonIncidentAngles(int const ConstrNum, int const NGlass, int &TotalIPhi, Array1A_int Tables)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu, FSEC
        //       DATE WRITTEN   Feb. 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Set up common incident angle values in all materials with spectral and angular data in the same construction

        // Using/Aliasing``
        using DataHeatBalance::Construct;
        using DataHeatBalance::Material;

        int X1Num;
        Real64 XX;
        Real64 YY;
        int i;
        int j;
        int k;
        int l;
        int m;
        int TabNum;
        int TabOpt;
        int waveSize;
        Array1D<Real64> XX1;
        const Real64 tol = 1.0e-5;
        bool found;
        bool Anglefound;
        int TableNum;

        Tables.dimension(NGlass);

        Anglefound = false;

        j = 0;
        for (i = 1; i <= NGlass; i++) {
            if (Tables(i) > 0) j++;
        }
        if (j == 0) return;

        if (TotalIPhi < 10) {
            XX1.allocate(10);
            for (k = 1; k <= 10; k++) {
                XX1(k) = (k - 1) * 10.0;
            }
        }

        for (i = 1; i <= NGlass; i++) {
            if (Tables(i) > 0) {
                if (!XX1.allocated()) {
                    TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(i))).GlassSpecAngTransDataPtr;
                    X1Num = TableLookup(PerfCurve(TableNum).TableIndex).NumX1Vars;
                    XX1.allocate(X1Num);
                    for (j = 1; j <= int(XX1.size()); j++) {
                        XX1(j) = TableLookup(PerfCurve(TableNum).TableIndex).X1Var(j);
                    }
                } else {
                    TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(i))).GlassSpecAngTransDataPtr;
                    X1Num = TableLookup(PerfCurve(TableNum).TableIndex).NumX1Vars;
                    if (int(XX1.size()) != X1Num) Anglefound = true;
                    for (j = 1; j <= X1Num; j++) {
                        XX = TableLookup(PerfCurve(TableNum).TableIndex).X1Var(j);
                        found = false;
                        for (k = 1; k <= int(XX1.size()); k++) {
                            if (fabs(XX1(k) - XX) < tol) {
                                found = true;
                            }
                        }
                        if (!found) {
                            XX1.push_back(XX);
                            Anglefound = true;
                        }
                    }
                }
            }
        }

        if (!Anglefound) return;

        // ascend sort
        std::sort(XX1.begin(), XX1.end());

        for (TabNum = 1; TabNum <= int(Tables.size()); TabNum++) {
            if (Tables(TabNum) == 0) continue;
            for (TabOpt = 1; TabOpt <= 3; TabOpt++) {
                if (TabOpt == 1) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngTransDataPtr;
                if (TabOpt == 2) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngFRefleDataPtr;
                if (TabOpt == 3) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngBRefleDataPtr;
                waveSize = TableLookup(PerfCurve(TableNum).TableIndex).NumX2Vars;
                if (int(XX1.size()) > TableLookup(PerfCurve(TableNum).TableIndex).NumX1Vars) {
                    l = 0;
                    for (i = 1; i <= int(XX1.size()); i++) {
                        if (fabs(XX1(i) - TableLookup(PerfCurve(TableNum).TableIndex).X1Var(i - l)) > tol) {
                            m = int(TableData(PerfCurve(TableNum).TableIndex).X1.size());
                            for (k = 1; k <= waveSize; k++) {
                                TableData(PerfCurve(TableNum).TableIndex)
                                    .X1.push_back(TableData(PerfCurve(TableNum).TableIndex).X1(m - waveSize + 1));
                                TableData(PerfCurve(TableNum).TableIndex)
                                    .X2.push_back(TableData(PerfCurve(TableNum).TableIndex).X2(m - waveSize + 1));
                                TableData(PerfCurve(TableNum).TableIndex).Y.push_back(TableData(PerfCurve(TableNum).TableIndex).Y(m - waveSize + 1));
                            }
                            for (j = m / waveSize; j >= i; j--) {
                                for (k = 1; k <= waveSize; k++) {
                                    TableData(PerfCurve(TableNum).TableIndex).X1(j * waveSize + k) =
                                        TableData(PerfCurve(TableNum).TableIndex).X1((j - 1) * waveSize + k);
                                    TableData(PerfCurve(TableNum).TableIndex).X2(j * waveSize + k) =
                                        TableData(PerfCurve(TableNum).TableIndex).X2((j - 1) * waveSize + k);
                                    TableData(PerfCurve(TableNum).TableIndex).Y(j * waveSize + k) =
                                        TableData(PerfCurve(TableNum).TableIndex).Y((j - 1) * waveSize + k);
                                }
                            }
                            YY = (XX1(i) - TableLookup(PerfCurve(TableNum).TableIndex).X1Var(i - l - 1)) /
                                 (TableLookup(PerfCurve(TableNum).TableIndex).X1Var(i - l) -
                                  TableLookup(PerfCurve(TableNum).TableIndex).X1Var(i - l - 1));
                            for (k = 1; k <= waveSize; k++) {
                                TableData(PerfCurve(TableNum).TableIndex).X1((i - 1) * waveSize + k) = XX1(i);
                                TableData(PerfCurve(TableNum).TableIndex).Y((i - 1) * waveSize + k) =
                                    TableData(PerfCurve(TableNum).TableIndex).Y((i - 2) * waveSize + k) +
                                    YY * (TableData(PerfCurve(TableNum).TableIndex).Y(i * waveSize + k) -
                                          TableData(PerfCurve(TableNum).TableIndex).Y((i - 2) * waveSize + k));
                            }
                            l++;
                        }
                    }
                }
            }
        }

        // Re-organize performance curve table data structure
        for (TabNum = 1; TabNum <= int(Tables.size()); TabNum++) {
            if (Tables(TabNum) == 0) continue;
            for (TabOpt = 1; TabOpt <= 3; TabOpt++) {
                if (TabOpt == 1) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngTransDataPtr;
                if (TabOpt == 2) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngFRefleDataPtr;
                if (TabOpt == 3) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngBRefleDataPtr;
                if (int(XX1.size()) == TableLookup(PerfCurve(TableNum).TableIndex).NumX1Vars) continue;
                waveSize = TableLookup(PerfCurve(TableNum).TableIndex).NumX2Vars;
                PerfCurveTableData(PerfCurve(TableNum).TableIndex).X1.allocate(int(XX1.size()));
                PerfCurveTableData(PerfCurve(TableNum).TableIndex).Y.allocate(waveSize, int(XX1.size()));
                PerfCurveTableData(PerfCurve(TableNum).TableIndex).X1 = -9999999.0;
                PerfCurveTableData(PerfCurve(TableNum).TableIndex).X2 = -9999999.0;
                PerfCurveTableData(PerfCurve(TableNum).TableIndex).Y = -9999999.0;
                for (i = 1; i <= int(XX1.size()); ++i) {
                    PerfCurveTableData(PerfCurve(TableNum).TableIndex).X1(i) = XX1(i);
                    for (j = 1; j <= waveSize; ++j) {
                        for (k = 1; k <= int(XX1.size()) * waveSize; ++k) {
                            if ((TableData(PerfCurve(TableNum).TableIndex).X1(k) == PerfCurveTableData(PerfCurve(TableNum).TableIndex).X1(i)) &&
                                (TableData(PerfCurve(TableNum).TableIndex).X2(k) == PerfCurveTableData(PerfCurve(TableNum).TableIndex).X2(j))) {
                                PerfCurveTableData(PerfCurve(TableNum).TableIndex).Y(j, i) = TableData(PerfCurve(TableNum).TableIndex).Y(k);
                            }
                        }
                    }
                }
            }
        }

        // Re-organize TableLookup data structure
        for (TabNum = 1; TabNum <= int(Tables.size()); TabNum++) {
            if (Tables(TabNum) == 0) continue;
            for (TabOpt = 1; TabOpt <= 3; TabOpt++) {
                if (TabOpt == 1) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngTransDataPtr;
                if (TabOpt == 2) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngFRefleDataPtr;
                if (TabOpt == 3) TableNum = Material(Construct(ConstrNum).LayerPoint(Tables(TabNum))).GlassSpecAngBRefleDataPtr;
                if (int(XX1.size()) == TableLookup(PerfCurve(TableNum).TableIndex).NumX1Vars) continue;
                TableLookup(PerfCurve(TableNum).TableIndex).NumX1Vars = size(PerfCurveTableData(PerfCurve(TableNum).TableIndex).X1);
                TableLookup(PerfCurve(TableNum).TableIndex).NumX2Vars = size(PerfCurveTableData(PerfCurve(TableNum).TableIndex).X2);
                TableLookup(PerfCurve(TableNum).TableIndex).X1Var.allocate(TableLookup(PerfCurve(TableNum).TableIndex).NumX1Vars);
                TableLookup(PerfCurve(TableNum).TableIndex).X2Var.allocate(TableLookup(PerfCurve(TableNum).TableIndex).NumX2Vars);
                TableLookup(PerfCurve(TableNum).TableIndex)
                    .TableLookupZData.allocate(1,
                                               1,
                                               1,
                                               1,
                                               size(PerfCurveTableData(PerfCurve(TableNum).TableIndex).Y(_, 1)),
                                               size(PerfCurveTableData(PerfCurve(TableNum).TableIndex).Y(1, _)));
                TableLookup(PerfCurve(TableNum).TableIndex).X1Var = PerfCurveTableData(PerfCurve(TableNum).TableIndex).X1;
                TableLookup(PerfCurve(TableNum).TableIndex).X2Var = PerfCurveTableData(PerfCurve(TableNum).TableIndex).X2;
                TableLookup(PerfCurve(TableNum).TableIndex).TableLookupZData(1, 1, 1, 1, _, _) =
                    PerfCurveTableData(PerfCurve(TableNum).TableIndex).Y(_, _);
            }
        }

        if (TotalIPhi != int(XX1.size())) {
            TotalIPhi = int(XX1.size());
        }

        XX1.deallocate();
    }
    //=================================================================================================!

} // namespace CurveManager

} // namespace EnergyPlus
