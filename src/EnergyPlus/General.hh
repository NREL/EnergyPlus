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

#ifndef General_hh_INCLUDED
#define General_hh_INCLUDED

// C++ Headers
#include <functional>
#include <type_traits>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/HVACSystemRootFindingAlgorithm.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

// Forward declaration
namespace OutputProcessor {
    enum class TimeStepType;
}

namespace WeatherManager {
    enum class DateType;
}

namespace General {

    // the only real solve root function, it is a template that takes a fixed array of parameters and matching residual function
    template <typename Function, typename Payload>
    void SolveRoot(EnergyPlusData &state,
                   Real64 Eps,   // required absolute accuracy
                   int MaxIte,   // maximum number of allowed iterations
                   int &Flag,    // integer storing exit status
                   Real64 &XRes, // value of x that solves f(x,Par) = 0
                   const Function &f,
                   Real64 X_0, // 1st bound of interval that contains the solution
                   Real64 X_1, // 2nd bound of interval that contains the solution
                   const Payload &Par)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
        //                      L. Gu, May 2017 - allow both Bisection and RegulaFalsi

        // PURPOSE OF THIS SUBROUTINE:
        // Find the value of x between x0 and x1 such that f(x,Par)
        // is equal to zero.

        // METHODOLOGY EMPLOYED:
        // Uses the Regula Falsi (false position) method (similar to secant method)

        // REFERENCES:
        // See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
        // 2nd edition, 1992. Page 347 ff.

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // = -2: f(x0) and f(x1) have the same sign
        // = -1: no convergence
        // >  0: number of iterations performed

        // STATIC ASSERTIONS TO HELP DEBUG TEMPLATE COMPILATION ERRORS
        //        static_assert(std::is_member_function_pointer_v<decltype(&Payload::begin)>, "Par does not have a begin() method, is it an array?");
        //        static_assert(std::is_member_function_pointer_v<decltype(&Payload::end)>, "Par does not have a end() method, is it an array?");

        static_assert(std::is_invocable_v<decltype(f), EnergyPlusData &, Real64, const Payload &>,
                      "Function passed in (f) cannot be called with the Payload (Par) passed in, the expected types do not match.");

        Real64 const SMALL(1.e-10);

        Real64 X0;       // present 1st bound
        Real64 X1;       // present 2nd bound
        Real64 XTemp;    // new estimate
        Real64 Y0;       // f at X0
        Real64 Y1;       // f at X1
        Real64 YTemp;    // f at XTemp
        Real64 DY;       // DY = Y0 - Y1
        bool Conv;       // flag, true if convergence is achieved
        bool StopMaxIte; // stop due to exceeding of maximum # of iterations
        bool Cont;       // flag, if true, continue searching
        int NIte;        // number of interations
        int AltIte;      // an accounter used for Alternation choice

        X0 = X_0;
        X1 = X_1;
        XTemp = X0;
        Conv = false;
        StopMaxIte = false;
        Cont = true;
        NIte = 0;
        AltIte = 0;

        Y0 = f(state, X0, Par);
        Y1 = f(state, X1, Par);
        // check initial values
        if (Y0 * Y1 > 0) {
            Flag = -2;
            XRes = X0;
            return;
        }
        XRes = XTemp;

        while (Cont) {

            DY = Y0 - Y1;
            if (std::abs(DY) < SMALL) DY = SMALL;
            if (std::abs(X1 - X0) < SMALL) {
                break;
            }
            // new estimation
            switch (state.dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver) {
            case HVACSystemRootSolverAlgorithm::RegulaFalsi: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
                break;
            }
            case HVACSystemRootSolverAlgorithm::Bisection: {
                XTemp = (X1 + X0) / 2.0;
                break;
            }
            case HVACSystemRootSolverAlgorithm::RegulaFalsiThenBisection: {
                if (NIte > state.dataRootFinder->HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case HVACSystemRootSolverAlgorithm::BisectionThenRegulaFalsi: {
                if (NIte <= state.dataRootFinder->HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case HVACSystemRootSolverAlgorithm::Alternation: {
                if (AltIte > state.dataRootFinder->HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                    if (AltIte >= 2 * state.dataRootFinder->HVACSystemRootFinding.NumOfIter) AltIte = 0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            default: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
            }
            }

            YTemp = f(state, XTemp, Par);

            ++NIte;
            ++AltIte;

            // check convergence
            if (std::abs(YTemp) < Eps) Conv = true;

            if (NIte > MaxIte) StopMaxIte = true;

            if ((!Conv) && (!StopMaxIte)) {
                Cont = true;
            } else {
                Cont = false;
            }

            if (Cont) {

                // reassign values (only if further iteration required)
                if (Y0 < 0.0) {
                    if (YTemp < 0.0) {
                        X0 = XTemp;
                        Y0 = YTemp;
                    } else {
                        X1 = XTemp;
                        Y1 = YTemp;
                    }
                } else {
                    if (YTemp < 0.0) {
                        X1 = XTemp;
                        Y1 = YTemp;
                    } else {
                        X0 = XTemp;
                        Y0 = YTemp;
                    }
                } // ( Y0 < 0 )

            } // (Cont)

        } // Cont

        if (Conv) {
            Flag = NIte;
        } else {
            Flag = -1;
        }
        XRes = XTemp;
    }

    constexpr Real64 InterpGeneral(Real64 const Lower, Real64 const Upper, Real64 const InterpFac)
    {
        return Lower + InterpFac * (Upper - Lower);
    }

    constexpr Real64 InterpProfSlat(Real64 const SlatLower,
                                    Real64 const SlatUpper,
                                    Real64 const ProfLower,
                                    Real64 const ProfUpper,
                                    Real64 const SlatInterpFac,
                                    Real64 const ProfInterpFac)
    {
        Real64 ValA = SlatLower + SlatInterpFac * (SlatUpper - SlatLower);
        Real64 ValB = ProfLower + SlatInterpFac * (ProfUpper - ProfLower);
        return ValA + ProfInterpFac * (ValB - ValA);
    }

    inline Real64 InterpSw(Real64 const SwitchFac, // Switching factor: 0.0 if glazing is unswitched, = 1.0 if fully switched
                           Real64 const A,         // Glazing property in unswitched state
                           Real64 const B          // Glazing property in fully switched state
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   February 1999

        // PURPOSE OF THIS FUNCTION:
        // For switchable glazing, calculates a weighted average of properties
        // A and B

        // Return value
        Real64 InterpSw;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 locSwitchFac;

        locSwitchFac = min(SwitchFac, 1.0);
        locSwitchFac = max(locSwitchFac, 0.0);

        InterpSw = (1.0 - locSwitchFac) * A + locSwitchFac * B;
        return InterpSw;
    }

    Real64 InterpProfAng(Real64 ProfAng,           // Profile angle (rad)
                         Array1S<Real64> PropArray // Array of blind properties
    );

    Real64 InterpSlatAng(Real64 SlatAng,           // Slat angle (rad)
                         bool VarSlats,            // True if slat angle is variable
                         Array1S<Real64> PropArray // Array of blind properties as function of slat angle
    );

    Real64 InterpProfSlatAng(Real64 ProfAng,           // Profile angle (rad)
                             Real64 SlatAng,           // Slat angle (rad)
                             bool VarSlats,            // True if variable-angle slats
                             Array2A<Real64> PropArray // Array of blind properties
    );

    Real64 BlindBeamBeamTrans(Real64 ProfAng,        // Solar profile angle (rad)
                              Real64 SlatAng,        // Slat angle (rad)
                              Real64 SlatWidth,      // Slat width (m)
                              Real64 SlatSeparation, // Slat separation (distance between surfaces of adjacent slats) (m)
                              Real64 SlatThickness   // Slat thickness (m)
    );

    constexpr Real64 POLYF(Real64 const X,          // Cosine of angle of incidence
                           Array1D<Real64> const &A // Polynomial coefficients
    )
    {
        if (X < 0.0 || X > 1.0) {
            return 0.0;
        } else {
            return X * (A(1) + X * (A(2) + X * (A(3) + X * (A(4) + X * (A(5) + X * A(6))))));
        }
    }

    std::string &strip_trailing_zeros(std::string &InputString);

    void MovingAvg(Array1A<Real64> DataIn,      // input data that needs smoothing
                   int NumDataItems,            // number of values in DataIn
                   int NumItemsInAvg,           // number of items in the averaging window
                   Array1A<Real64> SmoothedData // output data after smoothing
    );

    void ProcessDateString(EnergyPlusData &state,
                           std::string const &String,
                           int &PMonth,
                           int &PDay,
                           int &PWeekDay,
                           WeatherManager::DateType &DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
                           bool &ErrorsFound,
                           Optional_int PYear = _);

    void DetermineDateTokens(EnergyPlusData &state,
                             std::string const &String,
                             int &NumTokens,                     // Number of tokens found in string
                             int &TokenDay,                      // Value of numeric field found
                             int &TokenMonth,                    // Value of Month field found (1=Jan, 2=Feb, etc)
                             int &TokenWeekday,                  // Value of Weekday field found (1=Sunday, 2=Monday, etc), 0 if none
                             WeatherManager::DateType &DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
                             bool &ErrorsFound,                  // Set to true if cannot process this string as a date
                             Optional_int TokenYear = _          // Value of Year if one appears to be present and this argument is present
    );

    void ValidateMonthDay(EnergyPlusData &state,
                          std::string const &String, // REAL(r64) string being processed
                          int Day,
                          int Month,
                          bool &ErrorsFound);

    int OrdinalDay(int Month,        // Month, 1..12
                   int Day,          // Day of Month, not validated by month
                   int LeapYearValue // 1 if leap year indicated, 0 if not
    );

    void InvOrdinalDay(int Number, int &PMonth, int &PDay, int LeapYr);

    bool BetweenDates(int TestDate,  // Date to test
                      int StartDate, // Start date in sequence
                      int EndDate    // End date in sequence
    );

    std::string CreateSysTimeIntervalString(EnergyPlusData &state);

    int nthDayOfWeekOfMonth(EnergyPlusData &state,
                            int const &dayOfWeek,  // day of week (Sunday=1, Monday=2, ...)
                            int const &nthTime,    // nth time the day of the week occurs (first monday, third tuesday, ..)
                            int const &monthNumber // January = 1
    );

    Real64 SafeDivide(Real64 a, Real64 b);

    void Iterate(Real64 &ResultX, // ResultX is the final Iteration result passed back to the calling routine
                 Real64 Tol,      // Tolerance for Convergence
                 Real64 X0,       // Current value of X
                 Real64 Y0,       // Current value of the function Y(X)
                 Real64 &X1,      // First Previous values of X
                 Real64 &Y1,      // First Previous values of Y(X1)
                 int Iter,        // Number of iterations
                 int &Cnvg        // Convergence flag  Cnvg = 0:  Not converged
    );

    int FindNumberInList(int WhichNumber, Array1A_int ListOfItems, int NumItems);

    template <typename A> inline int FindNumberInList(int const WhichNumber, MArray1<A, int> const &ListOfItems, int const NumItems)
    {
        return FindNumberInList(WhichNumber, Array1D_int(ListOfItems), NumItems);
    }

    template <typename Container,
              class = typename std::enable_if<
                  !std::is_same<typename Container::value_type, std::string>::value>::type> // Container needs isize() and operator(i) and value_type
    inline int FindNumberInList(int const WhichNumber, Container const &ListOfItems, int Container::value_type::*num_p)
    {
        int FindNumberInList(0);
        for (int Count = 1, NumItems = ListOfItems.isize(); Count <= NumItems; ++Count) {
            if (WhichNumber == ListOfItems(Count).*num_p) {
                FindNumberInList = Count;
                break;
            }
        }
        return FindNumberInList;
    }

    void DecodeMonDayHrMin(int Item,   // word containing encoded month, day, hour, minute
                           int &Month, // month in integer format (1-12)
                           int &Day,   // day in integer format (1-31)
                           int &Hour,  // hour in integer format (1-24)
                           int &Minute // minute in integer format (0:59)
    );

    // TODO: this probably shouldn't be here
    int DetermineMinuteForReporting(EnergyPlusData &state,
                                    OutputProcessor::TimeStepType t_timeStepType); // kind of reporting, Zone Timestep or System

    void EncodeMonDayHrMin(int &Item, // word containing encoded month, day, hour, minute
                           int Month, // month in integer format (1:12)
                           int Day,   // day in integer format (1:31)
                           int Hour,  // hour in integer format (1:24)
                           int Minute // minute in integer format (0:59)
    );

    int LogicalToInteger(bool Flag);

    Real64 GetCurrentHVACTime(EnergyPlusData &state);

    Real64 GetPreviousHVACTime(EnergyPlusData &state);

    std::string CreateHVACTimeIntervalString(EnergyPlusData &state);

    std::string CreateTimeString(Real64 Time); // Time in seconds

    std::string CreateTimeIntervalString(Real64 StartTime, // Start of current interval in seconds
                                         Real64 EndTime    // End of current interval in seconds
    );

    void ParseTime(Real64 Time,    // Time value in seconds
                   int &Hours,     // Number of hours
                   int &Minutes,   // Number of minutes < 60
                   Real64 &Seconds // Number of seconds < 60
    );

    void ScanForReports(EnergyPlusData &state,
                        std::string const &reportName,
                        bool &DoReport,
                        Optional_string_const ReportKey = _,
                        Optional_string Option1 = _,
                        Optional_string Option2 = _);

    inline void ReallocateRealArray(Array1D<Real64> &Array,
                                    int &ArrayMax,     // Current and resultant dimension for Array
                                    int const ArrayInc // increment for redimension
    )
    {
        Array.redimension(ArrayMax += ArrayInc, 0.0);
    }

    void CheckCreatedZoneItemName(EnergyPlusData &state,
                                  std::string_view calledFrom,              // routine called from
                                  std::string const &CurrentObject,         // object being parsed
                                  std::string const &ZoneName,              // Zone Name associated
                                  std::string::size_type MaxZoneNameLength, // maximum length of zonelist zone names
                                  std::string const &ItemName,              // Item name (People, Lights, etc object)
                                  Array1_string const &ItemNames,           // Item Names to check for duplication
                                  int NumItems,                             // Number of items in ItemNames array
                                  std::string &ResultName,                  // Resultant name
                                  bool &errFlag                             // Error flag set to true if error found here.
    );

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    inline void CheckCreatedZoneItemName(EnergyPlusData &state,
                                         std::string_view const calledFrom,              // routine called from
                                         std::string const &CurrentObject,               // object being parsed
                                         std::string const &ZoneName,                    // Zone Name associated
                                         std::string::size_type const MaxZoneNameLength, // maximum length of zonelist zone names
                                         std::string const &ItemName,                    // Item name (People, Lights, etc object)
                                         Container const &Items,                         // Items to check for duplication Names
                                         int const NumItems,                             // Number of items in ItemNames array
                                         std::string &ResultName,                        // Resultant name
                                         bool &errFlag                                   // Error flag set to true if error found here.
    )
    {
        Array1D_string ItemNames(Items.size());
        for (std::size_t i = 0, e = Items.size(); i < e; ++i)
            ItemNames[i] = Items[i].Name;
        CheckCreatedZoneItemName(state, calledFrom, CurrentObject, ZoneName, MaxZoneNameLength, ItemName, ItemNames, NumItems, ResultName, errFlag);
    }

    std::vector<std::string> splitString(const std::string &string, char delimiter);

    inline Real64 epexp(const Real64 numerator, const Real64 denominator)
    {
        if (denominator == 0.0) {
            return 0.0;
        } else {
            return std::exp(numerator / denominator);
        }
    }

} // namespace General

struct GeneralData : BaseGlobalStruct
{
    bool GetReportInput = true;
    bool SurfVert = false;
    bool SurfDet = false;
    bool SurfDetWVert = false;
    bool DXFReport = false;
    bool DXFWFReport = false;
    bool VRMLReport = false;
    bool CostInfo = false;
    bool ViewFactorInfo = false;
    bool Constructions = false;
    bool Materials = false;
    bool LineRpt = false;
    bool VarDict = false;
    bool EMSoutput = false;
    Real64 XNext = 0.0; // used in root finder
    std::string DXFOption1;
    std::string DXFOption2;
    std::string DXFWFOption1;
    std::string DXFWFOption2;
    std::string VRMLOption1;
    std::string VRMLOption2;
    std::string ViewRptOption1;
    std::string LineRptOption1;
    std::string VarDictOption1;
    std::string VarDictOption2;

    void clear_state() override
    {
        this->GetReportInput = true;
        this->SurfVert = false;
        this->SurfDet = false;
        this->SurfDetWVert = false;
        this->DXFReport = false;
        this->DXFWFReport = false;
        this->VRMLReport = false;
        this->CostInfo = false;
        this->ViewFactorInfo = false;
        this->Constructions = false;
        this->Materials = false;
        this->LineRpt = false;
        this->VarDict = false;
        this->EMSoutput = false;
        this->XNext = 0.0;
        this->DXFOption1.clear();
        this->DXFOption2.clear();
        this->DXFWFOption1.clear();
        this->DXFWFOption2.clear();
        this->VRMLOption1.clear();
        this->VRMLOption2.clear();
        this->ViewRptOption1.clear();
        this->LineRptOption1.clear();
        this->VarDictOption1.clear();
        this->VarDictOption2.clear();
    }
};

} // namespace EnergyPlus

#endif
