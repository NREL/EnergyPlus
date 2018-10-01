// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#ifndef CurveManager_hh_INCLUDED
#define CurveManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array5D.hh>
#include <ObjexxFCL/Array6D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace CurveManager {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS
    extern std::string const Blank;

    // Curve Type parameters, these can differ from object types (e.g. a CurveType_TableOneIV can be linear, quadratic, etc)
    extern int const Linear;
    extern int const BiLinear;
    extern int const Quadratic;
    extern int const BiQuadratic;
    extern int const Cubic;
    extern int const QuadraticLinear;
    extern int const BiCubic;
    extern int const TriQuadratic;
    extern int const Exponent;
    extern int const Quartic;
    extern int const FanPressureRise;
    extern int const ExponentialSkewNormal;
    extern int const Sigmoid;
    extern int const RectangularHyperbola1;
    extern int const RectangularHyperbola2;
    extern int const ExponentialDecay;
    extern int const DoubleExponentialDecay;
    extern int const QuadLinear;
    extern int const CubicLinear;
    extern int const ChillerPartLoadWithLift;

    // Interpolation Types
    extern int const LinearInterpolationOfTable;
    extern int const LagrangeInterpolationLinearExtrapolation;
    extern int const EvaluateCurveToLimits;

    // Data Format
    extern int const SINGLELINEINDEPENDENTVARIABLEWITHMATRIX;

    // Sort Order
    extern int const ASCENDING;
    extern int const DESCENDING;

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    extern int NumCurves;
    extern bool GetCurvesInputFlag; // First time, input is "gotten"

    // SUBROUTINE SPECIFICATIONS FOR MODULE

    // Types

    struct TriQuadraticCurveDataStruct
    {
        // Members
        // this structure is for 27 coefficient full triquadratic (!)
        Real64 CoeffA0;
        Real64 CoeffA1;
        Real64 CoeffA2;
        Real64 CoeffA3;
        Real64 CoeffA4;
        Real64 CoeffA5;
        Real64 CoeffA6;
        Real64 CoeffA7;
        Real64 CoeffA8;
        Real64 CoeffA9;
        Real64 CoeffA10;
        Real64 CoeffA11;
        Real64 CoeffA12;
        Real64 CoeffA13;
        Real64 CoeffA14;
        Real64 CoeffA15;
        Real64 CoeffA16;
        Real64 CoeffA17;
        Real64 CoeffA18;
        Real64 CoeffA19;
        Real64 CoeffA20;
        Real64 CoeffA21;
        Real64 CoeffA22;
        Real64 CoeffA23;
        Real64 CoeffA24;
        Real64 CoeffA25;
        Real64 CoeffA26;

        // Default Constructor
        TriQuadraticCurveDataStruct()
            : CoeffA0(0.0), CoeffA1(0.0), CoeffA2(0.0), CoeffA3(0.0), CoeffA4(0.0), CoeffA5(0.0), CoeffA6(0.0), CoeffA7(0.0), CoeffA8(0.0),
              CoeffA9(0.0), CoeffA10(0.0), CoeffA11(0.0), CoeffA12(0.0), CoeffA13(0.0), CoeffA14(0.0), CoeffA15(0.0), CoeffA16(0.0), CoeffA17(0.0),
              CoeffA18(0.0), CoeffA19(0.0), CoeffA20(0.0), CoeffA21(0.0), CoeffA22(0.0), CoeffA23(0.0), CoeffA24(0.0), CoeffA25(0.0), CoeffA26(0.0)
        {
        }
    };

    struct TableDataStruct
    {
        // Members
        Real64 NormalPoint;
        Array1D<Real64> X1;
        Array1D<Real64> X2;
        Array1D<Real64> Y;

        // Default Constructor
        TableDataStruct() : NormalPoint(1.0)
        {
        }
    };

    struct PerfCurveTableDataStruct
    {
        // Members
        Array1D<Real64> X1;
        Array1D<Real64> X2;
        Array2D<Real64> Y;

        // Default Constructor
        PerfCurveTableDataStruct()
        {
        }
    };

    struct PerfomanceCurveData
    {
        // Members
        std::string Name;                                 // Curve Name
        std::string ObjectType;                           // Curve object type
        int CurveType;                                    // Curve type (see parameter definitions above)
        int InterpolationType;                            // table interpolation method
        int DataFormat;                                   // format of tabular data
        int TableIndex;                                   // Index to tablular data (0 if a standard curve object)
        int NumDims;                                      // Number of dimensions (AKA, independent variables)
        int NumIVLowErrorIndex;                           // Index to table object error message for too few IV's
        int NumIVHighErrorIndex;                          // Index to table object error message for too many IV's
        int X1SortOrder;                                  // sort order for table data for X1
        int X2SortOrder;                                  // sort order for table data for X2
        Real64 Coeff1;                                    // constant coefficient
        Real64 Coeff2;                                    // linear coeff (1st independent variable)
        Real64 Coeff3;                                    // quadratic coeff (1st independent variable)
        Real64 Coeff4;                                    // linear coeff (2nd ind var) or cubic coeff
        Real64 Coeff5;                                    // quadratic coeff (2nd independent variable)
        Real64 Coeff6;                                    // cross coeff (1st & 2nd ind var)
        Real64 Coeff7;                                    // cubic coeff for bicubic (1st ind var)
        Real64 Coeff8;                                    // cubic coeff for bicubic (2nd ind var)
        Real64 Coeff9;                                    // cross coeff for bicubic (1st quadratic & 2nd linear)
        Real64 Coeff10;                                   // cross coeff for bicubic (1st linear & 2nd quadratic)
        Real64 Coeff11;                                   // cross coeff
        Real64 Coeff12;                                   // cross coeff
        Real64 Var1Max;                                   // maximum of 1st independent variable
        Real64 Var1Min;                                   // minimum of 1st independent variable
        Real64 Var2Max;                                   // maximum of 2nd independent variable
        Real64 Var2Min;                                   // minimum of 2nd independent variable
        Real64 Var3Max;                                   // maximum of 3rd independent variable
        Real64 Var3Min;                                   // minimum of 3rd independent variable
        Real64 Var4Max;                                   // maximum of 4th independent variable
        Real64 Var4Min;                                   // minimum of 4th independent variable
        Real64 Var5Max;                                   // maximum of 5th independent variable
        Real64 Var5Min;                                   // minimum of 5th independent variable
        Real64 Var6Max;                                   // maximum of 6th independent variable
        Real64 Var6Min;                                   // minimum of 6th independent variable
        Real64 CurveMin;                                  // minimum value of curve output
        Real64 CurveMax;                                  // maximum value of curve output
        bool CurveMinPresent;                             // If TRUE, then cap minimum curve output
        bool CurveMaxPresent;                             // if TRUE, then cap maximum curve output
        bool Var1MinPresent;                              // uses data set limit to set Var1Min if false
        bool Var1MaxPresent;                              // uses data set limit to set Var1Max if false
        bool Var2MinPresent;                              // uses data set limit to set Var2Min if false
        bool Var2MaxPresent;                              // uses data set limit to set Var2Max if false
        bool Var3MinPresent;                              // uses data set limit to set Var3Min if false
        bool Var3MaxPresent;                              // uses data set limit to set Var3Max if false
        bool Var4MinPresent;                              // uses data set limit to set Var4Min if false
        bool Var4MaxPresent;                              // uses data set limit to set Var4Max if false
        bool Var5MinPresent;                              // uses data set limit to set Var5Min if false
        bool Var5MaxPresent;                              // uses data set limit to set Var5Max if false
        bool Var6MinPresent;                              // uses data set limit to set Var6Min if false
        bool Var6MaxPresent;                              // uses data set limit to set Var6Max if false
        Array1D<TriQuadraticCurveDataStruct> Tri2ndOrder; // structure for triquadratic curve data
        bool EMSOverrideOn;                               // if TRUE, then EMS is calling to override curve value
        Real64 EMSOverrideCurveValue;                     // Value of curve result EMS is directing to use
        bool OpticalProperty;                             // if TRUE, this table is used to store optical property
        // report variables
        Real64 CurveOutput; // curve output or result
        Real64 CurveInput1; // curve input #1 (e.g., x or X1 variable)
        Real64 CurveInput2; // curve input #2 (e.g., y or X2 variable)
        Real64 CurveInput3; // curve input #3 (e.g., z or X3 variable)
        Real64 CurveInput4; // curve input #4 (e.g., X4 variable)
        Real64 CurveInput5; // curve input #5 (e.g., X5 variable)
        Real64 CurveInput6; // curve input #6 (e.g., X6 variable)

        // Default Constructor
        PerfomanceCurveData()
            : ObjectType(""), CurveType(0), InterpolationType(0), DataFormat(0), TableIndex(0), NumDims(0), NumIVLowErrorIndex(0),
              NumIVHighErrorIndex(0), X1SortOrder(1), X2SortOrder(1), Coeff1(0.0), Coeff2(0.0), Coeff3(0.0), Coeff4(0.0), Coeff5(0.0), Coeff6(0.0),
              Coeff7(0.0), Coeff8(0.0), Coeff9(0.0), Coeff10(0.0), Coeff11(0.0), Coeff12(0.0), Var1Max(0.0), Var1Min(0.0), Var2Max(0.0), Var2Min(0.0),
              Var3Max(0.0), Var3Min(0.0), Var4Max(0.0), Var4Min(0.0), Var5Max(0.0), Var5Min(0.0), Var6Max(0.0), Var6Min(0.0), CurveMin(0.0),
              CurveMax(0.0), CurveMinPresent(false), CurveMaxPresent(false), Var1MinPresent(false), Var1MaxPresent(false), Var2MinPresent(false),
              Var2MaxPresent(false), Var3MinPresent(false), Var3MaxPresent(false), Var4MinPresent(false), Var4MaxPresent(false),
              Var5MinPresent(false), Var5MaxPresent(false), Var6MinPresent(false), Var6MaxPresent(false), EMSOverrideOn(false),
              EMSOverrideCurveValue(0.0), OpticalProperty(false), CurveOutput(0.0), CurveInput1(0.0), CurveInput2(0.0), CurveInput3(0.0),
              CurveInput4(0.0), CurveInput5(0.0), CurveInput6(0.0)
        {
        }
    };

    struct TableLookupData
    {
        // Members
        int InterpolationOrder; // number of points to interpolate (table data only)
        int NumX1Vars;          // Number of variables for independent variable #1
        Array1D<Real64> X1Var;
        int NumX2Vars; // Number of variables for independent variable #2
        Array1D<Real64> X2Var;
        int NumX3Vars; // Number of variables for independent variable #3
        Array1D<Real64> X3Var;
        int NumX4Vars; // Number of variables for independent variable #4
        Array1D<Real64> X4Var;
        int NumX5Vars; // Number of variables for independent variable #5
        Array1D<Real64> X5Var;
        int NumX6Vars; // Number of variables for independent variable #6
        Array1D<Real64> X6Var;
        Array6D<Real64> TableLookupZData;

        // Default Constructor
        TableLookupData()
            : InterpolationOrder(4), NumX1Vars(0), NumX2Vars(0), NumX3Vars(0), NumX4Vars(0), NumX5Vars(0), NumX6Vars(0)
        {
        }
    };

    // Object Data
    extern Array1D<PerfomanceCurveData> PerfCurve;
    extern Array1D<PerfCurveTableDataStruct> PerfCurveTableData;
    extern Array1D<TableDataStruct> TableData;
    extern Array1D<TableDataStruct> TempTableData;
    extern Array1D<TableDataStruct> Temp2TableData;
    extern Array1D<TableLookupData> TableLookup;

    // Functions

    // Clears the global data in CurveManager.
    // Needed for unit tests, should not be normally called.
    void clear_state();

    void ResetPerformanceCurveOutput();

    Real64 CurveValue(int const CurveIndex,            // index of curve in curve array
                      Real64 const Var1,               // 1st independent variable
                      Optional<Real64 const> Var2 = _, // 2nd independent variable
                      Optional<Real64 const> Var3 = _, // 3rd independent variable
                      Optional<Real64 const> Var4 = _, // 4th independent variable
                      Optional<Real64 const> Var5 = _, // 5th independent variable
                      Optional<Real64 const> Var6 = _  // 6th independent variable
    );

    void GetCurveInput();

    void GetCurveInputData(bool &ErrorsFound);

    void InitCurveReporting();

    void ReadTableData(int const CurveNum,
                       std::string &CurrentModuleObject,
                       bool const ReadFromFile,
                       std::string &FileName,
                       Array1S_string Alphas,
                       Array1S<Real64> Numbers,
                       int const NumNumbers,
                       bool &ErrorsFound);

    Real64 DLAG(Real64 const XX,
                Real64 const YY,
                Array1S<Real64> X,
                Array1S<Real64> Y,
                Array2S<Real64> Z,
                int const NX,
                int const NY,
                int const M,
                int &IEXTX,
                int &IEXTY);

    Real64 PerformanceCurveObject(int const CurveIndex,            // index of curve in curve array
                                  Real64 const Var1,               // 1st independent variable
                                  Optional<Real64 const> Var2 = _, // 2nd independent variable
                                  Optional<Real64 const> Var3 = _, // 3rd independent variable
                                  Optional<Real64 const> Var4 = _  // 4th independent variable
    );

    Real64 PerformanceTableObject(int const CurveIndex,            // index of curve in curve array
                                  Real64 const Var1,               // 1st independent variable
                                  Optional<Real64 const> Var2 = _, // 2nd independent variable
                                  Optional<Real64 const> Var3 = _  // 3rd independent variable
    );

    Real64 TableLookupObject(int const CurveIndex,            // index of curve in curve array
                             Real64 const Var1,               // 1st independent variable
                             Optional<Real64 const> Var2 = _, // 2nd independent variable
                             Optional<Real64 const> Var3 = _, // 3rd independent variable
                             Optional<Real64 const> Var4 = _, // 4th independent variable
                             Optional<Real64 const> Var5 = _, // 5th independent variable
                             Optional<Real64 const> Var6 = _);

    void SolveRegression(int &CurveNum,                          // index to performance curve
                         std::string &TableType,                 // tabular data object type
                         std::string &CurveName,                 // performance curve name
                         Array1S<Real64> RawDataX,               // table data X values (1st independent variable)
                         Array1S<Real64> RawDataY,               // table data Y values (dependent variables)
                         Optional<Array1S<Real64>> RawDataX2 = _ // table data X2 values (2nd independent variable)
    );

    void Interpolate_Lagrange(Real64 const DataPoint,        // point used for interpolating output (x)
                              Array1S<Real64> FunctionArray, // array of output data (Y's)
                              Array1S<Real64> Ordinate,      // array of input data (X's)
                              int const ISPT,                // the starting point in the interpolated array
                              int const IEPT,                // the ending point in the interpolated array
                              Real64 &ALAG                   // the interpolated output (y or F(x) in equation above)
    );

    bool IsCurveInputTypeValid(std::string const &InInputType); // index of curve in curve array

    bool IsCurveOutputTypeValid(std::string const &InOutputType); // index of curve in curve array

    bool CheckCurveDims(int const CurveIndex,
                        std::vector<int> validDims,
                        std::string routineName,
                        std::string objectType,
                        std::string objectName,
                        std::string curveFieldText);

    std::string GetCurveName(int const CurveIndex); // index of curve in curve array

    Real64 GetNormalPoint(int const CurveIndex);

    int GetCurveIndex(std::string const &CurveName); // name of the curve

    // This utility function grabs a curve index and performs the
    // error checking

    int GetCurveCheck(std::string const &alph, // curve name
                      bool &errFlag,
                      std::string const &ObjName // parent object of curve
    );

    void GetCurveMinMaxValues(int const CurveIndex,         // index of curve in curve array
                              Real64 &Var1Min,              // Minimum values of 1st independent variable
                              Real64 &Var1Max,              // Maximum values of 1st independent variable
                              Optional<Real64> Var2Min = _, // Minimum values of 2nd independent variable
                              Optional<Real64> Var2Max = _, // Maximum values of 2nd independent variable
                              Optional<Real64> Var3Min = _, // Minimum values of 2nd independent variable
                              Optional<Real64> Var3Max = _  // Maximum values of 2nd independent variable
    );

    void SetCurveOutputMinMaxValues(int const CurveIndex,                // index of curve in curve array
                                    bool &ErrorsFound,                   // TRUE when errors occur
                                    Optional<Real64 const> CurveMin = _, // Minimum value of curve output
                                    Optional<Real64 const> CurveMax = _  // Maximum values of curve output
    );

    void GetPressureSystemInput();

    void GetPressureCurveTypeAndIndex(std::string const &PressureCurveName, // name of the curve
                                      int &PressureCurveType,
                                      int &PressureCurveIndex);

    Real64 PressureCurveValue(int const PressureCurveIndex, Real64 const MassFlow, Real64 const Density, Real64 const Viscosity);

    Real64 CalculateMoodyFrictionFactor(Real64 const ReynoldsNumber, Real64 const RoughnessRatio);

    void checkCurveIsNormalizedToOne(std::string const callingRoutineObj, // calling routine with object type
                                     std::string const objectName,        // parent object where curve is used
                                     int const curveIndex,                // index to curve object
                                     std::string const cFieldName,        // object field name
                                     std::string const cFieldValue,       // user input curve name
                                     Real64 const Var1,                   // required 1st independent variable
                                     Optional<Real64 const> Var2 = _,     // 2nd independent variable
                                     Optional<Real64 const> Var3 = _,     // 3rd independent variable
                                     Optional<Real64 const> Var4 = _,     // 4th independent variable
                                     Optional<Real64 const> Var5 = _      // 5th independent variable
    );

    int GetCurveInterpolationMethodNum(int const CurveIndex); // index of curve in curve array

    void ReadTwoVarTableDataFromFile(int const CurveNum, std::string &FileName, int &lineNum);

    void SetSameIndeVariableValues(int const TransCurveIndex, int const FRefleCurveIndex, int const BRefleCurveIndex);

    void SetCommonIncidentAngles(
        int const ConstrNum,     // Construction number
        int const NGlass,        // The number of glass layers in the construction with index = ConstrNum
        int &TotalIPhi,          // The number of incident angles
        Array1A_int const Tables // Store construction layer number for SpectralAndAngleGlassLayer glass only. Otherwise = 0 for other layers.
    );

    //=================================================================================================!

} // namespace CurveManager

} // namespace EnergyPlus

#endif
