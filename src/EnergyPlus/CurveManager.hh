// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

// C++ Headers
#include <map>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>

#include <nlohmann/json.hpp>

// Btwxt Headers
#include <btwxt.h>
#include <griddeddata.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/FileSystem.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace Curve {

    // Curve Type parameters, these can differ from object types (e.g. a CurveType_TableOneIV can be linear, quadratic, etc)

    enum class CurveType
    {
        Invalid = -1,
        Linear,
        Quadratic,
        BiQuadratic,
        Cubic,
        QuadraticLinear,
        BiCubic,
        TriQuadratic,
        Exponent,
        Quartic,
        FanPressureRise,
        ExponentialSkewNormal,
        Sigmoid,
        RectangularHyperbola1,
        RectangularHyperbola2,
        ExponentialDecay,
        DoubleExponentialDecay,
        QuadLinear,
        QuintLinear,
        CubicLinear,
        ChillerPartLoadWithLift,
        Num
    };

    constexpr std::array<std::string_view, static_cast<int>(CurveType::Num)> objectNames = {
        "Curve:Linear",
        "Curve:Quadratic",
        "Curve:Biquadratic",
        "Curve:Cubic",
        "Curve:QuadLinear",
        "Curve:Bicubic",
        "Curve:Triquadratic",
        "Curve:Exponent",
        "Curve:Quartic",
        "Curve:FanPressureRise",
        "Curve:ExponentialSkewNormal",
        "Curve:Sigmoid",
        "Curve:RectangularHyperbola1",
        "Curve:RectangularHyperbola2",
        "Curve:ExponentialDecay",
        "Curve:DoubleExponentialDecay",
        "Curve:QuadraticLinear",
        "Curve:QuintLinear",
        "Curve:CubicLinear",
        "Curve:ChillerPartLoadWithLift",
    }; // namespace Curve

    enum class InterpType
    {
        Invalid = -1,
        EvaluateCurveToLimits,
        BtwxtMethod,
        Num
    };

    struct Limits
    {
        Real64 min = 0.0;
        Real64 max = 0.0;
        bool minPresent = false;
        bool maxPresent = false;
    };

    struct Curve
    {
        // Basic data
        std::string Name;                         // Curve Name
        CurveType curveType = CurveType::Invalid; // Curve type (see parameter definitions above)
        // Table data stuff
        InterpType interpolationType = InterpType::Invalid; // Table interpolation method
        int TableIndex = 0;     // Index to tabular data (0 if a standard curve object) OR Index of RGI for new Table:Lookup
        int numDims = 0;        // Number of dimensions (AKA, independent variables)
        int GridValueIndex = 0; // Index of output within RGI for new Table:Lookup
        // input coefficients
        std::array<Real64, 27> coeff = {0.0}; // curve coefficients
        // independent variables
        std::array<Real64, 6> inputs = {0.0}; // curve inputs
        std::array<Limits, 6> inputLimits;    // min/max of independent variables
        // dependent (output) variable
        Real64 output = 0.0; // curve output or result
        Limits outputLimits; // min/max of curve output
        // EMS override
        bool EMSOverrideOn = false;         // if TRUE, then EMS is calling to override curve value
        Real64 EMSOverrideCurveValue = 0.0; // Value of curve result EMS is directing to use
        Real64 value(EnergyPlusData &state, Real64 V1);
        Real64 value(EnergyPlusData &state, Real64 V1, Real64 V2);
        Real64 value(EnergyPlusData &state, Real64 V1, Real64 V2, Real64 V3);
        Real64 value(EnergyPlusData &state, Real64 V1, Real64 V2, Real64 V3, Real64 V4);
        Real64 value(EnergyPlusData &state, Real64 V1, Real64 V2, Real64 V3, Real64 V4, Real64 V5);
        Real64 value(EnergyPlusData &state, Real64 V1, Real64 V2, Real64 V3, Real64 V4, Real64 V5, Real64 V6);
        Real64 valueFallback(EnergyPlusData &state, Real64 V1, Real64 V2, Real64 V3, Real64 V4, Real64 V5);
        Real64 BtwxtTableInterpolation(EnergyPlusData &state,
                                       const Real64 Var1 // 1st independent variable
        );
        Real64 BtwxtTableInterpolation(EnergyPlusData &state,
                                       const Real64 Var1, // 1st independent variable
                                       const Real64 Var2  // 2nd independent variable
        );
        Real64 BtwxtTableInterpolation(EnergyPlusData &state,
                                       const Real64 Var1, // 1st independent variable
                                       const Real64 Var2, // 2nd independent variable
                                       const Real64 Var3  // 3rd independent variable
        );
        Real64 BtwxtTableInterpolation(EnergyPlusData &state,
                                       const Real64 Var1, // 1st independent variable
                                       const Real64 Var2, // 2nd independent variable
                                       const Real64 Var3, // 3rd independent variable
                                       const Real64 Var4  // 4th independent variable
        );
        Real64 BtwxtTableInterpolation(EnergyPlusData &state,
                                       const Real64 Var1, // 1st independent variable
                                       const Real64 Var2, // 2nd independent variable
                                       const Real64 Var3, // 3rd independent variable
                                       const Real64 Var4, // 4th independent variable
                                       const Real64 Var5  // 5th independent variable
        );
        Real64 BtwxtTableInterpolation(EnergyPlusData &state,
                                       const Real64 Var1, // 1st independent variable
                                       const Real64 Var2, // 2nd independent variable
                                       const Real64 Var3, // 3rd independent variable
                                       const Real64 Var4, // 4th independent variable
                                       const Real64 Var5, // 5th independent variable
                                       const Real64 Var6);
    };

    // Table file objects
    class TableFile
    {
    public:
        fs::path filePath;
        std::vector<std::vector<std::string>> contents;
        std::map<std::pair<std::size_t, std::size_t>, std::vector<double>> arrays;
        bool load(EnergyPlusData &state, fs::path const &path); // Note: this returns 'True' if ErrorsFound
        std::vector<double> &getArray(EnergyPlusData &state, std::pair<std::size_t, std::size_t> colAndRow);

    private:
        std::size_t numRows = 0u;
        std::size_t numColumns = 0u;
    };

    // Container for Btwxt N-d Objects
    class BtwxtManager
    {
    public:
        // Map RGI collection to string name of independent variable list
        int addGrid(const std::string &indVarListName, Btwxt::GriddedData grid)
        {
            grids.emplace_back(grid);
            gridMap.emplace(indVarListName, grids.size() - 1);
            return static_cast<int>(grids.size()) - 1;
        };
        double normalizeGridValues(int gridIndex, int outputIndex, const std::vector<double> &target, double scalar = 1.0);
        int addOutputValues(int gridIndex, std::vector<double> values);
        int getGridIndex(EnergyPlusData &state, std::string &indVarListName, bool &ErrorsFound);
        int getNumGridDims(int gridIndex);
        double getGridValue(int gridIndex, int outputIndex, const std::vector<double> &target);
        std::map<std::string, const nlohmann::json &> independentVarRefs;
        std::map<fs::path, TableFile> tableFiles;
        void clear();

    private:
        std::map<std::string, std::size_t> gridMap;
        std::vector<Btwxt::RegularGridInterpolator> grids;
    };

    void BtwxtMessageCallback(Btwxt::MsgLevel messageType, std::string message, void *contextPtr);

    void ResetPerformanceCurveOutput(EnergyPlusData &state);

    Real64 CurveValue(EnergyPlusData &state,
                      int CurveIndex, // index of curve in curve array
                      Real64 Var1     // 1st independent variable
    );

    Real64 CurveValue(EnergyPlusData &state,
                      int CurveIndex, // index of curve in curve array
                      Real64 Var1,    // 1st independent variable
                      Real64 Var2     // 1st independent variable
    );

    Real64 CurveValue(EnergyPlusData &state,
                      int CurveIndex, // index of curve in curve array
                      Real64 Var1,    // 1st independent variable
                      Real64 Var2,    // 1st independent variable
                      Real64 Var3     // 1st independent variable
    );

    Real64 CurveValue(EnergyPlusData &state,
                      int CurveIndex, // index of curve in curve array
                      Real64 Var1,    // 1st independent variable
                      Real64 Var2,    // 1st independent variable
                      Real64 Var3,    // 1st independent variable
                      Real64 Var4     // 1st independent variable
    );

    Real64 CurveValue(EnergyPlusData &state,
                      int CurveIndex, // index of curve in curve array
                      Real64 Var1,    // 1st independent variable
                      Real64 Var2,    // 1st independent variable
                      Real64 Var3,    // 1st independent variable
                      Real64 Var4,    // 1st independent variable
                      Real64 Var5     // 1st independent variable
    );

    Real64 CurveValue(EnergyPlusData &state,
                      int CurveIndex, // index of curve in curve array
                      Real64 Var1,    // 1st independent variable
                      Real64 Var2,    // 1st independent variable
                      Real64 Var3,    // 1st independent variable
                      Real64 Var4,    // 1st independent variable
                      Real64 Var5,    // 1st independent variable
                      Real64 Var6     // 1st independent variable
    );

    void GetCurveInput(EnergyPlusData &state);

    void GetCurveInputData(EnergyPlusData &state, bool &ErrorsFound);

    void InitCurveReporting(EnergyPlusData &state);

    bool IsCurveInputTypeValid(std::string const &InInputType); // index of curve in curve array

    bool IsCurveOutputTypeValid(std::string const &InOutputType); // index of curve in curve array

    bool CheckCurveDims(EnergyPlusData &state,
                        int CurveIndex,
                        std::vector<int> validDims,
                        std::string_view routineName,
                        std::string_view objectType,
                        std::string_view objectName,
                        std::string_view curveFieldText);

    std::string GetCurveName(EnergyPlusData &state, int CurveIndex); // index of curve in curve array

    Real64 GetNormalPoint(int CurveIndex);

    int GetCurveIndex(EnergyPlusData &state, std::string const &CurveName); // name of the curve

    // This utility function grabs a curve index and performs the
    // error checking

    int GetCurveCheck(EnergyPlusData &state,
                      std::string const &alph, // curve name
                      bool &errFlag,
                      std::string const &ObjName // parent object of curve
    );

    void GetCurveMinMaxValues(EnergyPlusData &state,
                              int const CurveIndex, // index of curve in curve array
                              Real64 &Var1Min,      // Minimum values of 1st independent variable
                              Real64 &Var1Max       // Maximum values of 1st independent variable
    );

    void GetCurveMinMaxValues(EnergyPlusData &state,
                              int const CurveIndex, // index of curve in curve array
                              Real64 &Var1Min,      // Minimum values of 1st independent variable
                              Real64 &Var1Max,      // Maximum values of 1st independent variable
                              Real64 &Var2Min,      // Minimum values of 2nd independent variable
                              Real64 &Var2Max       // Maximum values of 2nd independent variable
    );

    void GetCurveMinMaxValues(EnergyPlusData &state,
                              int const CurveIndex, // index of curve in curve array
                              Real64 &Var1Min,      // Minimum values of 1st independent variable
                              Real64 &Var1Max,      // Maximum values of 1st independent variable
                              Real64 &Var2Min,      // Minimum values of 2nd independent variable
                              Real64 &Var2Max,      // Maximum values of 2nd independent variable
                              Real64 &Var3Min,      // Minimum values of 3rd independent variable
                              Real64 &Var3Max       // Maximum values of 3rd independent variable
    );

    void GetCurveMinMaxValues(EnergyPlusData &state,
                              int const CurveIndex, // index of curve in curve array
                              Real64 &Var1Min,      // Minimum values of 1st independent variable
                              Real64 &Var1Max,      // Maximum values of 1st independent variable
                              Real64 &Var2Min,      // Minimum values of 2nd independent variable
                              Real64 &Var2Max,      // Maximum values of 2nd independent variable
                              Real64 &Var3Min,      // Minimum values of 3rd independent variable
                              Real64 &Var3Max,      // Maximum values of 3rd independent variable
                              Real64 &Var4Min,      // Minimum values of 4th independent variable
                              Real64 &Var4Max       // Maximum values of 4th independent variable
    );

    void GetCurveMinMaxValues(EnergyPlusData &state,
                              int const CurveIndex, // index of curve in curve array
                              Real64 &Var1Min,      // Minimum values of 1st independent variable
                              Real64 &Var1Max,      // Maximum values of 1st independent variable
                              Real64 &Var2Min,      // Minimum values of 2nd independent variable
                              Real64 &Var2Max,      // Maximum values of 2nd independent variable
                              Real64 &Var3Min,      // Minimum values of 3rd independent variable
                              Real64 &Var3Max,      // Maximum values of 3rd independent variable
                              Real64 &Var4Min,      // Minimum values of 4th independent variable
                              Real64 &Var4Max,      // Maximum values of 4th independent variable
                              Real64 &Var5Min,      // Minimum values of 5th independent variable
                              Real64 &Var5Max       // Maximum values of 5th independent variable
    );

    void GetCurveMinMaxValues(EnergyPlusData &state,
                              int const CurveIndex, // index of curve in curve array
                              Real64 &Var1Min,      // Minimum values of 1st independent variable
                              Real64 &Var1Max,      // Maximum values of 1st independent variable
                              Real64 &Var2Min,      // Minimum values of 2nd independent variable
                              Real64 &Var2Max,      // Maximum values of 2nd independent variable
                              Real64 &Var3Min,      // Minimum values of 3rd independent variable
                              Real64 &Var3Max,      // Maximum values of 3rd independent variable
                              Real64 &Var4Min,      // Minimum values of 4th independent variable
                              Real64 &Var4Max,      // Maximum values of 4th independent variable
                              Real64 &Var5Min,      // Minimum values of 5th independent variable
                              Real64 &Var5Max,      // Maximum values of 5th independent variable
                              Real64 &Var6Min,      // Minimum values of 6th independent variable
                              Real64 &Var6Max       // Maximum values of 6th independent variable
    );

    void SetCurveOutputMinValue(EnergyPlusData &state,
                                int CurveIndex,       // index of curve in curve array
                                bool &ErrorsFound,    // TRUE when errors occur
                                const Real64 CurveMin // Minimum value of curve output
    );

    void SetCurveOutputMaxValue(EnergyPlusData &state,
                                int CurveIndex,       // index of curve in curve array
                                bool &ErrorsFound,    // TRUE when errors occur
                                const Real64 CurveMax // Maximum values of curve output
    );

    void GetPressureSystemInput(EnergyPlusData &state);

    void GetPressureCurveTypeAndIndex(EnergyPlusData &state,
                                      std::string const &PressureCurveName, // name of the curve
                                      DataBranchAirLoopPlant::PressureCurveType &PressureCurveType,
                                      int &PressureCurveIndex);

    Real64 PressureCurveValue(EnergyPlusData &state, int PressureCurveIndex, Real64 MassFlow, Real64 Density, Real64 Viscosity);

    Real64 CalculateMoodyFrictionFactor(EnergyPlusData &state, Real64 ReynoldsNumber, Real64 RoughnessRatio);

    void checkCurveIsNormalizedToOne(EnergyPlusData &state,
                                     const std::string &callingRoutineObj, // calling routine with object type
                                     const std::string &objectName,        // parent object where curve is used
                                     int curveIndex,                       // index to curve object
                                     const std::string &cFieldName,        // object field name
                                     const std::string &cFieldValue,       // user input curve name
                                     Real64 Var1);                         // required 1st independent variable

    void checkCurveIsNormalizedToOne(EnergyPlusData &state,
                                     const std::string &callingRoutineObj, // calling routine with object type
                                     const std::string &objectName,        // parent object where curve is used
                                     int curveIndex,                       // index to curve object
                                     const std::string &cFieldName,        // object field name
                                     const std::string &cFieldValue,       // user input curve name
                                     Real64 Var1,                          // required 1st independent variable
                                     Real64 Var2);                         // 2nd independent variable

} // namespace Curve

struct CurveManagerData : BaseGlobalStruct
{
    int NumCurves = 0;
    bool GetCurvesInputFlag = true;
    bool CurveValueMyBeginTimeStepFlag = false;
    bool FrictionFactorErrorHasOccurred = false;
    bool showFallbackMessage = true;
    EPVector<Curve::Curve *> PerfCurve;
    Curve::BtwxtManager btwxtManager;
    std::unordered_map<std::string, std::string> UniqueCurveNames;

    void allocateCurveVector(int const count)
    {
        this->NumCurves = count;
        for (int curveIndex = 1; curveIndex <= count; curveIndex++)
            this->PerfCurve.push_back(new EnergyPlus::Curve::Curve);
    }

    void clear_state() override
    {
        for (Curve::Curve *p : PerfCurve) {
            delete p;
        }
        new (this) CurveManagerData();
    }
};

} // namespace EnergyPlus

#endif
