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
#include <cassert>
#include <cmath>
#include <cstdlib>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>

#if defined(_WIN32) && _MSC_VER < 1900
#define snprintf _snprintf
#endif

namespace EnergyPlus::General {

// Module containing routines for general use

// MODULE INFORMATION:
//       AUTHOR         Fred Buhl, Linda Lawrie
//       DATE WRITTEN   December 2001
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// contains routines (most likely numeric) that may be needed in several parts
// of EnergyPlus

// Using/Aliasing
using DataHVACGlobals::Bisection;

// MODULE PARAMETER DEFINITIONS
static std::string const BlankString;

Real64 InterpProfAng(Real64 const ProfAng,           // Profile angle (rad)
                     Array1S<Real64> const PropArray // Array of blind properties
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   May 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Does profile-angle interpolation of window blind solar-thermal properties

    // METHODOLOGY EMPLOYED:
    // Linear interpolation.

    // Return value
    Real64 InterpProfAng;

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const DeltaAngRad(DataGlobalConstants::Pi / 36.0); // Profile angle increment (rad)

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 InterpFac; // Interpolation factor
    int IAlpha;       // Profile angle index

    // DeltaAng = Pi/36
    if (ProfAng > DataGlobalConstants::PiOvr2 || ProfAng < -DataGlobalConstants::PiOvr2) {
        InterpProfAng = 0.0;
    } else {
        IAlpha = 1 + int((ProfAng + DataGlobalConstants::PiOvr2) / DeltaAngRad);
        InterpFac = (ProfAng - (-DataGlobalConstants::PiOvr2 + DeltaAngRad * (IAlpha - 1))) / DeltaAngRad;
        InterpProfAng = (1.0 - InterpFac) * PropArray(IAlpha) + InterpFac * PropArray(IAlpha + 1);
    }
    return InterpProfAng;
}

Real64 InterpSlatAng(Real64 const SlatAng,           // Slat angle (rad)
                     bool const VarSlats,            // True if slat angle is variable
                     Array1S<Real64> const PropArray // Array of blind properties as function of slat angle
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Dec 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Does slat-angle interpolation of window blind solar-thermal properties that
    // do not depend on profile angle

    // METHODOLOGY EMPLOYED:
    // Linear interpolation.

    // Using/Aliasing
    using DataSurfaces::MaxSlatAngs;

    // Return value
    Real64 InterpSlatAng;

    // FUNCTION PARAMETER DEFINITIONS:
    static Real64 const DeltaAng(DataGlobalConstants::Pi / (double(MaxSlatAngs) - 1.0));
    static Real64 const DeltaAng_inv((double(MaxSlatAngs) - 1.0) / DataGlobalConstants::Pi);

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 InterpFac; // Interpolation factor
    int IBeta;        // Slat angle index
    Real64 SlatAng1;

    if (SlatAng > DataGlobalConstants::Pi || SlatAng < 0.0) {
        SlatAng1 = min(max(SlatAng, 0.0), DataGlobalConstants::Pi);
    } else {
        SlatAng1 = SlatAng;
    }

    if (VarSlats) { // Variable-angle slats
        IBeta = 1 + int(SlatAng1 * DeltaAng_inv);
        InterpFac = (SlatAng1 - DeltaAng * (IBeta - 1)) * DeltaAng_inv;
        InterpSlatAng = PropArray(IBeta) + InterpFac * (PropArray(min(MaxSlatAngs, IBeta + 1)) - PropArray(IBeta));
    } else { // Fixed-angle slats or shade
        InterpSlatAng = PropArray(1);
    }

    return InterpSlatAng;
}

Real64 InterpProfSlatAng(Real64 const ProfAng,           // Profile angle (rad)
                         Real64 const SlatAng,           // Slat angle (rad)
                         bool const VarSlats,            // True if variable-angle slats
                         Array2A<Real64> const PropArray // Array of blind properties
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Dec 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Does simultaneous profile-angle and slat-angle interpolation of window
    // blind solar-thermal properties that depend on profile angle and slat angle

    // METHODOLOGY EMPLOYED:
    // Linear interpolation.

    // Using/Aliasing
    using DataSurfaces::MaxSlatAngs;

    // Return value
    Real64 InterpProfSlatAng;

    // Argument array dimensioning
    PropArray.dim(MaxSlatAngs, 37);

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const DeltaProfAng(DataGlobalConstants::Pi / 36.0);
    Real64 const DeltaSlatAng(DataGlobalConstants::Pi / (double(MaxSlatAngs) - 1.0));

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 ProfAngRatio; // Profile angle interpolation factor
    Real64 SlatAngRatio; // Slat angle interpolation factor
    int IAlpha;          // Profile angle index
    int IBeta;           // Slat angle index
    Real64 Val1;         // Property values at points enclosing the given ProfAngle and SlatAngle
    Real64 Val2;
    Real64 Val3;
    Real64 Val4;
    Real64 ValA; // Property values at given SlatAngle to be interpolated in profile angle
    Real64 ValB;
    Real64 SlatAng1;
    Real64 ProfAng1;

    if (SlatAng > DataGlobalConstants::Pi || SlatAng < 0.0 || ProfAng > DataGlobalConstants::PiOvr2 || ProfAng < -DataGlobalConstants::PiOvr2) {
        SlatAng1 = min(max(SlatAng, 0.0), DataGlobalConstants::Pi);

        // This is not correct, fixed 2/17/2010
        // ProfAng1 = MIN(MAX(SlatAng,-PiOvr2),PiOvr2)
        ProfAng1 = min(max(ProfAng, -DataGlobalConstants::PiOvr2), DataGlobalConstants::PiOvr2);
    } else {
        SlatAng1 = SlatAng;
        ProfAng1 = ProfAng;
    }

    IAlpha = int((ProfAng1 + DataGlobalConstants::PiOvr2) / DeltaProfAng) + 1;
    ProfAngRatio = (ProfAng1 + DataGlobalConstants::PiOvr2 - (IAlpha - 1) * DeltaProfAng) / DeltaProfAng;

    if (VarSlats) { // Variable-angle slats: interpolate in profile angle and slat angle
        IBeta = int(SlatAng1 / DeltaSlatAng) + 1;
        SlatAngRatio = (SlatAng1 - (IBeta - 1) * DeltaSlatAng) / DeltaSlatAng;
        Val1 = PropArray(IBeta, IAlpha);
        Val2 = PropArray(min(MaxSlatAngs, IBeta + 1), IAlpha);
        Val3 = PropArray(IBeta, min(37, IAlpha + 1));
        Val4 = PropArray(min(MaxSlatAngs, IBeta + 1), min(37, IAlpha + 1));
        ValA = Val1 + SlatAngRatio * (Val2 - Val1);
        ValB = Val3 + SlatAngRatio * (Val4 - Val3);
        InterpProfSlatAng = ValA + ProfAngRatio * (ValB - ValA);
    } else { // Fixed-angle slats: interpolate only in profile angle
        Val1 = PropArray(1, IAlpha);
        Val2 = PropArray(1, min(37, IAlpha + 1));
        InterpProfSlatAng = Val1 + ProfAngRatio * (Val2 - Val1);
    }

    return InterpProfSlatAng;
}

Real64 BlindBeamBeamTrans(Real64 const ProfAng,        // Solar profile angle (rad)
                          Real64 const SlatAng,        // Slat angle (rad)
                          Real64 const SlatWidth,      // Slat width (m)
                          Real64 const SlatSeparation, // Slat separation (distance between surfaces of adjacent slats) (m)
                          Real64 const SlatThickness   // Slat thickness (m)
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Jan 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates beam-to-beam transmittance of a window blind

    // METHODOLOGY EMPLOYED:
    // Based on solar profile angle and slat geometry

    // Return value
    Real64 BlindBeamBeamTrans;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 fEdge;      // Slat edge correction factor
    Real64 wbar;       // Intermediate variable
    Real64 gamma;      // Intermediate variable
    Real64 fEdge1;     // Intermediate variable
    Real64 CosProfAng; // Cosine of profile angle

    CosProfAng = std::cos(ProfAng);
    gamma = SlatAng - ProfAng;
    wbar = SlatSeparation;
    if (CosProfAng != 0.0) wbar = SlatWidth * std::cos(gamma) / CosProfAng;
    BlindBeamBeamTrans = max(0.0, 1.0 - std::abs(wbar / SlatSeparation));

    if (BlindBeamBeamTrans > 0.0) {

        // Correction factor that accounts for finite thickness of slats. It is used to modify the
        // blind transmittance to account for reflection and absorption by the slat edges.
        // fEdge is ratio of area subtended by edge of slat to area between tops of adjacent slats.

        fEdge = 0.0;
        fEdge1 = 0.0;
        if (std::abs(std::sin(gamma)) > 0.01) {
            if ((SlatAng > 0.0 && SlatAng <= DataGlobalConstants::PiOvr2 && ProfAng <= SlatAng) ||
                (SlatAng > DataGlobalConstants::PiOvr2 && SlatAng <= DataGlobalConstants::Pi && ProfAng > -(DataGlobalConstants::Pi - SlatAng)))
                fEdge1 = SlatThickness * std::abs(std::sin(gamma)) / ((SlatSeparation + SlatThickness / std::abs(std::sin(SlatAng))) * CosProfAng);
            fEdge = min(1.0, std::abs(fEdge1));
        }
        BlindBeamBeamTrans *= (1.0 - fEdge);
    }

    return BlindBeamBeamTrans;
}

std::string &strip_trailing_zeros(std::string &InputString)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Stuart Mentzer (in-place version of RemoveTrailingZeros by Linda Lawrie)
    //       DATE WRITTEN   July 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Remove trailing fractional zeros from floating point representation strings in place.

    static std::string const ED("ED");
    static std::string const zero_string("0.");

    assert(!has_any_of(InputString, "ed"));       // Pre Not using lowercase exponent letter
    assert(InputString == stripped(InputString)); // Pre Already stripped surrounding spaces

    if (has(InputString, '.') && (!has_any_of(InputString, ED))) { // Has decimal point and no exponent part
        std::string::size_type const pos(InputString.find_last_not_of('0'));
        if (pos + 1 < InputString.length()) {
            switch (pos) { // Handle [+/-].000... format
            case 0u:       // .0*
                InputString = zero_string;
                break;
            case 1u:
                if (InputString[1] == '.') {
                    char const c0(InputString[0]);
                    if ((c0 == '+') || (c0 == '-')) {
                        InputString = zero_string;
                        break;
                    }
                }
                // fallthrough
            default:
                InputString.erase(pos + 1);
            }
        }
    }
    return InputString; // Allows chaining
}

void MovingAvg(Array1A<Real64> const DataIn, // input data that needs smoothing
               int const NumDataItems,       // number of values in DataIn
               int const NumItemsInAvg,      // number of items in the averaging window
               Array1A<Real64> SmoothedData  // output data after smoothing
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Smooth the data in the 1-d array DataIn by averaging over a window NumItemsInAvg
    // wide. Return the results in the 1-d array SmoothedData

    // METHODOLOGY EMPLOYED:
    // Note that DataIn and SmoothedData should have the same size. This is the reponsibility
    // of the calling routine. NumItemsInAvg should be no bigger than the size of DataIn.

    // Argument array dimensioning
    DataIn.dim(NumDataItems);
    SmoothedData.dim(NumDataItems);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array1D<Real64> TempData(3 * NumDataItems); // a scratch array

    for (int i = 1; i <= NumDataItems; ++i) {
        TempData(i) = TempData(NumDataItems + i) = TempData(2 * NumDataItems + i) = DataIn(i);
        SmoothedData(i) = 0.0;
    }

    for (int i = 1; i <= NumDataItems; ++i) {
        for (int j = 1; j <= NumItemsInAvg; ++j) {
            SmoothedData(i) += TempData(NumDataItems + i - NumItemsInAvg + j);
        }
        SmoothedData(i) /= double(NumItemsInAvg);
    }
}

void ProcessDateString(EnergyPlusData &state,
                       std::string const &String,
                       int &PMonth,
                       int &PDay,
                       int &PWeekDay,
                       WeatherManager::DateType &DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
                       bool &ErrorsFound,
                       Optional_int PYear)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine will process a date from a string and determine
    // the proper month and day for that date string.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FstNum;
    bool errFlag;
    int NumTokens;
    int TokenDay;
    int TokenMonth;
    int TokenWeekday;

    FstNum = int(UtilityRoutines::ProcessNumber(String, errFlag));
    DateType = WeatherManager::DateType::InvalidDate;
    if (!errFlag) {
        // Entered single number, do inverse JDay
        if (FstNum == 0) {
            PMonth = 0;
            PDay = 0;
            DateType = WeatherManager::DateType::MonthDay;
        } else if (FstNum < 0 || FstNum > 366) {
            ShowSevereError(state, "Invalid Julian date Entered=" + String);
            ErrorsFound = true;
        } else {
            InvOrdinalDay(FstNum, PMonth, PDay, 0);
            DateType = WeatherManager::DateType::LastDayInMonth;
        }
    } else {
        // Error when processing as number, try x/x
        if (!present(PYear)) {
            DetermineDateTokens(state, String, NumTokens, TokenDay, TokenMonth, TokenWeekday, DateType, ErrorsFound);
        } else {
            int TokenYear = 0;
            DetermineDateTokens(state, String, NumTokens, TokenDay, TokenMonth, TokenWeekday, DateType, ErrorsFound, TokenYear);
            PYear = TokenYear;
        }
        if (DateType == WeatherManager::DateType::MonthDay) {
            PDay = TokenDay;
            PMonth = TokenMonth;
        } else if (DateType == WeatherManager::DateType::NthDayInMonth || DateType == WeatherManager::DateType::LastDayInMonth) {
            // interpret as TokenDay TokenWeekday in TokenMonth
            PDay = TokenDay;
            PMonth = TokenMonth;
            PWeekDay = TokenWeekday;
        }
    }
}

void DetermineDateTokens(EnergyPlusData &state,
                         std::string const &String,
                         int &NumTokens,                     // Number of tokens found in string
                         int &TokenDay,                      // Value of numeric field found
                         int &TokenMonth,                    // Value of Month field found (1=Jan, 2=Feb, etc)
                         int &TokenWeekday,                  // Value of Weekday field found (1=Sunday, 2=Monday, etc), 0 if none
                         WeatherManager::DateType &DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
                         bool &ErrorsFound,                  // Set to true if cannot process this string as a date
                         Optional_int TokenYear              // Value of Year if one appears to be present and this argument is present
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is invoked for date fields that appear to be strings (give
    // error when ProcessNumber is used).

    // METHODOLOGY EMPLOYED:
    // Delete everything that is extraneous to the date information needed.  Process what
    // is left.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static int const NumSingleChars(3);
    static Array1D_string const SingleChars(NumSingleChars, {"/", ":", "-"});
    static int const NumDoubleChars(6);
    static Array1D_string const DoubleChars(NumDoubleChars,
                                            {"ST ", "ND ", "RD ", "TH ", "OF ", "IN "}); // Need trailing spaces: Want thse only at end of words
    static Array1D_string const Months(12, {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"});
    static Array1D_string const Weekdays(7, {"SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"});
    static std::string const Numbers("0123456789");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string CurrentString;
    std::string::size_type Pos;
    int Loop;
    Array1D_string Fields(3);
    int NumField1;
    int NumField2;
    int NumField3;
    bool errFlag;
    bool InternalError;
    bool WkDayInMonth;

    CurrentString = String;
    NumTokens = 0;
    TokenDay = 0;
    TokenMonth = 0;
    TokenWeekday = 0;
    DateType = WeatherManager::DateType::InvalidDate;
    InternalError = false;
    WkDayInMonth = false;
    if (present(TokenYear)) TokenYear = 0;
    // Take out separator characters, other extraneous stuff

    for (Loop = 1; Loop <= NumSingleChars; ++Loop) {
        Pos = index(CurrentString, SingleChars(Loop));
        while (Pos != std::string::npos) {
            CurrentString[Pos] = ' ';
            Pos = index(CurrentString, SingleChars(Loop));
        }
    }

    for (Loop = 1; Loop <= NumDoubleChars; ++Loop) {
        Pos = index(CurrentString, DoubleChars(Loop));
        while (Pos != std::string::npos) {
            CurrentString.replace(Pos, 2, "  ");
            Pos = index(CurrentString, DoubleChars(Loop));
            WkDayInMonth = true;
        }
    }

    strip(CurrentString);
    if (CurrentString == BlankString) {
        ShowSevereError(state, "Invalid date field=" + String);
        ErrorsFound = true;
    } else {
        Loop = 0;
        while (Loop < 3) { // Max of 3 fields
            if (CurrentString == BlankString) break;
            Pos = index(CurrentString, ' ');
            ++Loop;
            if (Pos == std::string::npos) Pos = CurrentString.length();
            Fields(Loop) = CurrentString.substr(0, Pos);
            CurrentString.erase(0, Pos);
            strip(CurrentString);
        }
        if (not_blank(CurrentString)) {
            ShowSevereError(state, "Invalid date field=" + String);
            ErrorsFound = true;
        } else if (Loop == 2) {
            // Field must be Day Month or Month Day (if both numeric, mon / day)
            InternalError = false;
            NumField1 = int(UtilityRoutines::ProcessNumber(Fields(1), errFlag));
            if (errFlag) {
                // Month day, but first field is not numeric, 2nd must be
                NumField2 = int(UtilityRoutines::ProcessNumber(Fields(2), errFlag));
                if (errFlag) {
                    ShowSevereError(state, "Invalid date field=" + String);
                    InternalError = true;
                } else {
                    TokenDay = NumField2;
                }
                TokenMonth = UtilityRoutines::FindItemInList(Fields(1).substr(0, 3), Months, 12);
                ValidateMonthDay(state, String, TokenDay, TokenMonth, InternalError);
                if (!InternalError) {
                    DateType = WeatherManager::DateType::MonthDay;
                } else {
                    ErrorsFound = true;
                }
            } else {
                // Month Day, first field was numeric, if 2nd is, then it's month<num> day<num>
                NumField2 = int(UtilityRoutines::ProcessNumber(Fields(2), errFlag));
                if (!errFlag) {
                    TokenMonth = NumField1;
                    TokenDay = NumField2;
                    ValidateMonthDay(state, String, TokenDay, TokenMonth, InternalError);
                    if (!InternalError) {
                        DateType = WeatherManager::DateType::MonthDay;
                    } else {
                        ErrorsFound = true;
                    }
                } else { // 2nd field was not numeric.  Must be Month
                    TokenDay = NumField1;
                    TokenMonth = UtilityRoutines::FindItemInList(Fields(2).substr(0, 3), Months, 12);
                    ValidateMonthDay(state, String, TokenDay, TokenMonth, InternalError);
                    if (!InternalError) {
                        DateType = WeatherManager::DateType::MonthDay;
                        NumTokens = 2;
                    } else {
                        ErrorsFound = true;
                    }
                }
            }
        } else if (Loop == 3) {
            // Field must be some combination of <num> Weekday Month (if WkDayInMonth true)
            if (WkDayInMonth) {
                NumField1 = int(UtilityRoutines::ProcessNumber(Fields(1), errFlag));
                if (!errFlag) { // the expected result
                    TokenDay = NumField1;
                    TokenWeekday = UtilityRoutines::FindItemInList(Fields(2).substr(0, 3), Weekdays, 7);
                    if (TokenWeekday == 0) {
                        TokenMonth = UtilityRoutines::FindItemInList(Fields(2).substr(0, 3), Months, 12);
                        TokenWeekday = UtilityRoutines::FindItemInList(Fields(3).substr(0, 3), Weekdays, 7);
                        if (TokenMonth == 0 || TokenWeekday == 0) InternalError = true;
                    } else {
                        TokenMonth = UtilityRoutines::FindItemInList(Fields(3).substr(0, 3), Months, 12);
                        if (TokenMonth == 0) InternalError = true;
                    }
                    DateType = WeatherManager::DateType::NthDayInMonth;
                    NumTokens = 3;
                    if (TokenDay < 0 || TokenDay > 5) InternalError = true;
                } else { // first field was not numeric....
                    if (Fields(1) == "LA") {
                        DateType = WeatherManager::DateType::LastDayInMonth;
                        NumTokens = 3;
                        TokenWeekday = UtilityRoutines::FindItemInList(Fields(2).substr(0, 3), Weekdays, 7);
                        if (TokenWeekday == 0) {
                            TokenMonth = UtilityRoutines::FindItemInList(Fields(2).substr(0, 3), Months, 12);
                            TokenWeekday = UtilityRoutines::FindItemInList(Fields(3).substr(0, 3), Weekdays, 7);
                            if (TokenMonth == 0 || TokenWeekday == 0) InternalError = true;
                        } else {
                            TokenMonth = UtilityRoutines::FindItemInList(Fields(3).substr(0, 3), Months, 12);
                            if (TokenMonth == 0) InternalError = true;
                        }
                    } else { // error....
                        ShowSevereError(state, "First date field not numeric, field=" + String);
                    }
                }
            } else { // mm/dd/yyyy or yyyy/mm/dd
                NumField1 = int(UtilityRoutines::ProcessNumber(Fields(1), errFlag));
                NumField2 = int(UtilityRoutines::ProcessNumber(Fields(2), errFlag));
                NumField3 = int(UtilityRoutines::ProcessNumber(Fields(3), errFlag));
                DateType = WeatherManager::DateType::MonthDay;
                // error detection later..
                if (NumField1 > 100) {
                    if (present(TokenYear)) {
                        TokenYear = NumField1;
                    }
                    TokenMonth = NumField2;
                    TokenDay = NumField3;
                } else if (NumField3 > 100) {
                    if (present(TokenYear)) {
                        TokenYear = NumField3;
                    }
                    TokenMonth = NumField1;
                    TokenDay = NumField2;
                }
            }
        } else {
            // Not enough or too many fields
            ShowSevereError(state, "Invalid date field=" + String);
            ErrorsFound = true;
        }
    }

    if (InternalError) {
        DateType = WeatherManager::DateType::InvalidDate;
        ErrorsFound = true;
    }
}

void ValidateMonthDay(EnergyPlusData &state,
                      std::string const &String, // REAL(r64) string being processed
                      int const Day,
                      int const Month,
                      bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine validates a potential Day, Month values, produces an error
    // message when not valid, and sets error flag.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static Array1D_int const EndMonthDay(12, {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31});

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool InternalError;

    InternalError = false;
    if (Month < 1 || Month > 12) InternalError = true;
    if (!InternalError) {
        if (Day < 1 || Day > EndMonthDay(Month)) InternalError = true;
    }
    if (InternalError) {
        ShowSevereError(state, "Invalid Month Day date format=" + String);
        ErrorsFound = true;
    } else {
        ErrorsFound = false;
    }
}

int OrdinalDay(int const Month,        // Month, 1..12
               int const Day,          // Day of Month, not validated by month
               int const LeapYearValue // 1 if leap year indicated, 0 if not
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  from JDAYF in BLAST/IBLAST

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine returns the appropriate Julian Day value for the input
    // Month and Day.

    // Return value
    int JulianDay;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    static Array1D_int const EndDayofMonth(12, {31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365});
    // End day numbers of each month (without Leap Year)

    if (Month == 1) {
        //                                       CASE 1: JANUARY
        JulianDay = Day;

    } else if (Month == 2) {
        //                                       CASE 2: FEBRUARY
        JulianDay = Day + EndDayofMonth(1);

    } else if ((Month >= 3) && (Month <= 12)) {
        //                                       CASE 3: REMAINING MONTHS
        JulianDay = Day + EndDayofMonth(Month - 1) + LeapYearValue;

    } else {
        JulianDay = 0;
    }

    return JulianDay;
}

void InvOrdinalDay(int const Number, int &PMonth, int &PDay, int const LeapYr)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine performs and inverse Julian Day
    // calculation, using an input JulianDay and returning
    // appropriate Month and Day.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static Array1D_int const EndOfMonth({0, 12}, {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365});

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int WMonth;
    int LeapAddPrev;
    int LeapAddCur;

    if (Number < 0 || Number > 366) return;
    for (WMonth = 1; WMonth <= 12; ++WMonth) {
        if (WMonth == 1) {
            LeapAddPrev = 0;
            LeapAddCur = 0;
        } else if (WMonth == 2) {
            LeapAddPrev = 0;
            LeapAddCur = LeapYr;
        } else {
            LeapAddPrev = LeapYr;
            LeapAddCur = LeapYr;
        }
        if (Number > (EndOfMonth(WMonth - 1) + LeapAddPrev) && Number <= (EndOfMonth(WMonth) + LeapAddCur)) break;
    }
    PMonth = WMonth;
    PDay = Number - (EndOfMonth(WMonth - 1) + LeapAddCur);
}

bool BetweenDates(int const TestDate,  // Date to test
                  int const StartDate, // Start date in sequence
                  int const EndDate    // End date in sequence
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   June 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns true if the TestDate is between
    // (StartDate <= TestDate <= EndDate).

    // METHODOLOGY EMPLOYED:
    // The input dates are Julian Day format, year is irrelevant.
    // Thus, if StartDate > EndDate (i.e. StartDate = 1Dec and EndDate = 31Jan),
    // this routine accomodates.

    // REFERENCES:
    // Adapted from BLAST BTWEEN function.

    // Return value
    bool BetweenDates;

    BetweenDates = false; // Default case

    if (StartDate <= EndDate) { // Start Date <= End Date
        if (TestDate >= StartDate && TestDate <= EndDate) BetweenDates = true;
    } else { // EndDate <= StartDate
        if (TestDate <= EndDate || TestDate >= StartDate) BetweenDates = true;
    }

    return BetweenDates;
}

std::string CreateSysTimeIntervalString(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   April 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function creates the current time interval of the system
    // time step.

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // Return value
    std::string OutputString;

    Real64 const FracToMin(60.0);

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 ActualTimeS; // Start of current interval (HVAC time step)
    Real64 ActualTimeE; // End of current interval (HVAC time step)
    int ActualTimeHrS;
    //  INTEGER ActualTimeHrE
    int ActualTimeMinS;

    //  ActualTimeS=INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime)))
    // CR6902  ActualTimeS=INT(CurrentTime-TimeStepZone)+SysTimeElapsed
    // [DC] TODO: Improve display accuracy up to fractional seconds using hh:mm:ss.0 format
    ActualTimeS = state.dataGlobal->CurrentTime - state.dataGlobal->TimeStepZone + SysTimeElapsed;
    ActualTimeE = ActualTimeS + TimeStepSys;
    ActualTimeHrS = int(ActualTimeS);
    //  ActualTimeHrE=INT(ActualTimeE)
    ActualTimeMinS = nint((ActualTimeS - ActualTimeHrS) * FracToMin);

    if (ActualTimeMinS == 60) {
        ++ActualTimeHrS;
        ActualTimeMinS = 0;
    }
    const auto TimeStmpS = format("{:02}:{:02}", ActualTimeHrS, ActualTimeMinS);
    auto minutes = ((ActualTimeE - static_cast<int>(ActualTimeE)) * FracToMin);

    auto TimeStmpE = format("{:02}:{:2.0F}", static_cast<int>(ActualTimeE), minutes);

    if (TimeStmpE[3] == ' ') {
        TimeStmpE[3] = '0';
    }
    OutputString = TimeStmpS + " - " + TimeStmpE;

    return OutputString;
}

// returns the Julian date for the first, second, etc. day of week for a given month
int nthDayOfWeekOfMonth(EnergyPlusData &state,
                        int const &dayOfWeek,  // day of week (Sunday=1, Monday=2, ...)
                        int const &nthTime,    // nth time the day of the week occurs (first monday, third tuesday, ..)
                        int const &monthNumber // January = 1
)
{
    // J. Glazer - August 2017
    int firstDayOfMonth = OrdinalDay(monthNumber, 1, state.dataEnvrn->CurrentYearIsLeapYear);
    int dayOfWeekForFirstDay = (state.dataEnvrn->RunPeriodStartDayOfWeek + firstDayOfMonth - 1) % 7;
    int jdatForNth;
    if (dayOfWeek >= dayOfWeekForFirstDay) {
        jdatForNth = firstDayOfMonth + (dayOfWeek - dayOfWeekForFirstDay) + 7 * (nthTime - 1);
    } else {
        jdatForNth = firstDayOfMonth + ((dayOfWeek + 7) - dayOfWeekForFirstDay) + 7 * (nthTime - 1);
    }
    return jdatForNth;
}

Real64 SafeDivide(Real64 const a, Real64 const b)
{

    // returns a / b while preventing division by zero

    // Return value
    Real64 c;

    // Locals
    Real64 const SMALL(1.E-10);

    if (std::abs(b) >= SMALL) {
        c = a / b;
    } else {
        c = a / sign(SMALL, b);
    }
    return c;
}

void Iterate(Real64 &ResultX,  // ResultX is the final Iteration result passed back to the calling routine
             Real64 const Tol, // Tolerance for Convergence
             Real64 const X0,  // Current value of X
             Real64 const Y0,  // Current value of the function Y(X)
             Real64 &X1,       // First Previous values of X
             Real64 &Y1,       // First Previous values of Y(X1)
             int const Iter,   // Number of iterations
             int &Cnvg         // Convergence flag  Cnvg = 0:  Not converged
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   March 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Iterately solves for the value of X which satisfies Y(X)=0.
    // The subroutine tests for convergence and provides a new guess for the value of the
    // independent variable X.

    // REFERENCES:
    // Linear Correction based on the RegulaFalsi routine in EnergyPlus

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const small(1.e-9); // Small Number used to approximate zero
    Real64 const Perturb(0.1); // Perturbation applied to X to initialize iteration

    Real64 DY; // Linear fit result

    // Check for convergence by comparing change in X
    if (Iter != 1) {
        if (std::abs(X0 - X1) < Tol || Y0 == 0.0) {
            ResultX = X0;
            Cnvg = 1;
            return;
        }
    }

    // Not converged.
    Cnvg = 0;
    if (Iter == 1) {

        // New guess is specified by Perturb
        if (std::abs(X0) > small) {
            ResultX = X0 * (1.0 + Perturb);
        } else {
            ResultX = Perturb;
        }

    } else {

        // New guess calculated from LINEAR FIT of most recent two points
        DY = Y0 - Y1;
        if (std::abs(DY) < small) {
            DY = small;
        }
        // new estimation

        ResultX = (Y0 * X1 - Y1 * X0) / DY;
    }

    X1 = X0;
    Y1 = Y0;
}

int FindNumberInList(int const WhichNumber, Array1A_int const ListOfItems, int const NumItems)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up a number(integer) in a similar list of
    // items and returns the index of the item in the list, if
    // found.

    // Return value
    int FindNumberInList;

    // Argument array dimensioning
    ListOfItems.dim(_);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Count;

    FindNumberInList = 0;

    for (Count = 1; Count <= NumItems; ++Count) {
        if (WhichNumber == ListOfItems(Count)) {
            FindNumberInList = Count;
            break;
        }
    }

    return FindNumberInList;
}

void DecodeMonDayHrMin(int const Item, // word containing encoded month, day, hour, minute
                       int &Month,     // month in integer format (1-12)
                       int &Day,       // day in integer format (1-31)
                       int &Hour,      // hour in integer format (1-24)
                       int &Minute     // minute in integer format (0:59)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine decodes the "packed" integer representation of
    // the Month, Day, Hour, and Minute.  Packed integers are used to
    // save memory allocation.  Original idea for this routine is contained
    // in DECMDH, BLAST code, by Jean Baugh.

    // METHODOLOGY EMPLOYED:
    // Using maximum integer concept the original date can be decoded
    // from the packed single word.  This relies on 4 byte integer representation
    // as a minimum (capable of representing up to 2,147,483,647).

    // SUBROUTINE PARAMETER DEFINITIONS:
    int const DecMon(100 * 100 * 100);
    int const DecDay(100 * 100);
    int const DecHr(100);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int TmpItem;

    TmpItem = Item;
    Month = TmpItem / DecMon;
    TmpItem = (TmpItem - Month * DecMon);
    Day = TmpItem / DecDay;
    TmpItem -= Day * DecDay;
    Hour = TmpItem / DecHr;
    Minute = mod(TmpItem, DecHr);
}

int DetermineMinuteForReporting(EnergyPlusData &state, OutputProcessor::TimeStepType t_timeStepType) // kind of reporting, Zone Timestep or System
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   January 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // When reporting peaks, minutes are used but not necessarily easily calculated.

    // METHODOLOGY EMPLOYED:
    // Could use the access to the minute as OP (OutputProcessor) does but uses
    // external calculation.

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // Return value
    int ActualTimeMin; // calculated Minute for reporting

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const FracToMin(60.0);

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 ActualTimeS; // Start of current interval (HVAC time step)
    Real64 ActualTimeE; // End of current interval (HVAC time step)
    int ActualTimeHrS;

    if (t_timeStepType == OutputProcessor::TimeStepType::TimeStepSystem) {
        ActualTimeS = state.dataGlobal->CurrentTime - state.dataGlobal->TimeStepZone + SysTimeElapsed;
        ActualTimeE = ActualTimeS + TimeStepSys;
        ActualTimeHrS = int(ActualTimeS);
        ActualTimeMin = nint((ActualTimeE - ActualTimeHrS) * FracToMin);
    } else {
        ActualTimeMin = (state.dataGlobal->CurrentTime - int(state.dataGlobal->CurrentTime)) * FracToMin;
    }

    return ActualTimeMin;
}

void EncodeMonDayHrMin(int &Item,       // word containing encoded month, day, hour, minute
                       int const Month, // month in integer format (1:12)
                       int const Day,   // day in integer format (1:31)
                       int const Hour,  // hour in integer format (1:24)
                       int const Minute // minute in integer format (0:59)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine encodes the "packed" integer representation of
    // the Month, Day, Hour, and Minute.  Packed integers are used to
    // save memory allocation.  Original idea for this routine is contained
    // in DECMDH, BLAST code, by Jean Baugh.

    // METHODOLOGY EMPLOYED:
    // Using maximum integer concept the original date can be decoded
    // from the packed single word.  This relies on 4 byte integer representation
    // as a minimum (capable of representing up to 2,147,483,647).

    Item = ((Month * 100 + Day) * 100 + Hour) * 100 + Minute;
}

int LogicalToInteger(bool const Flag)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   November 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine uses an input logical and makes
    // an integer (true=1, false=0)

    // Return value
    int LogicalToInteger;

    if (Flag) {
        LogicalToInteger = 1;
    } else {
        LogicalToInteger = 0;
    }

    return LogicalToInteger;
}

Real64 GetCurrentHVACTime(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   November 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine returns the time in seconds at the end of the current HVAC step.

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // Return value
    Real64 GetCurrentHVACTime;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 CurrentHVACTime;

    // This is the correct formula that does not use MinutesPerSystemTimeStep, which would
    // erronously truncate all sub-minute system time steps down to the closest full minute.
    // Maybe later TimeStepZone, TimeStepSys and SysTimeElapsed could also be specified
    // as real.
    CurrentHVACTime = (state.dataGlobal->CurrentTime - state.dataGlobal->TimeStepZone) + SysTimeElapsed + TimeStepSys;
    GetCurrentHVACTime = CurrentHVACTime * DataGlobalConstants::SecInHour;

    return GetCurrentHVACTime;
}

Real64 GetPreviousHVACTime(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   November 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine returns the time in seconds at the beginning of the current HVAC step.

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;

    // Return value
    Real64 GetPreviousHVACTime;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 PreviousHVACTime;

    // This is the correct formula that does not use MinutesPerSystemTimeStep, which would
    // erronously truncate all sub-minute system time steps down to the closest full minute.
    PreviousHVACTime = (state.dataGlobal->CurrentTime - state.dataGlobal->TimeStepZone) + SysTimeElapsed;
    GetPreviousHVACTime = PreviousHVACTime * DataGlobalConstants::SecInHour;

    return GetPreviousHVACTime;
}

std::string CreateHVACTimeIntervalString(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function creates the time stamp with the current time interval for the HVAC
    // time step.

    // Return value
    std::string OutputString;

    OutputString = CreateTimeIntervalString(GetPreviousHVACTime(state), GetCurrentHVACTime(state));

    return OutputString;
}

std::string CreateTimeString(Real64 const Time) // Time in seconds
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function creates the time stamp string from the time value specified in seconds.
    // Inspired by similar function CreateSysTimeIntervalString() in General.cc
    // However, this function provides better accuracy for sub-minute time steps
    // by also showing information down to the 10th of a second.
    // Note that Time is expected to be specified in REAL(r64).

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Hours;      // Number of hours <= 24
    int Minutes;    // Remaining minutes < 60
    Real64 Seconds; // Remaining seconds < 60

    ParseTime(Time, Hours, Minutes, Seconds);

    // TimeStamp written with formatting
    // "hh:mm:ss.s"
    // 10 chars + null terminator = 11
    // This approach should not normally be used due to the fixed width c-style
    // string but in this case the output string is a fixed size so this is more
    // clear for formatting and faster. If formatted string changes, make sure to
    // add more to buffer.
    char buffer[11];
    int cx = snprintf(buffer, 11, "%02d:%02d:%04.1f", Hours, Minutes, Seconds);

    // Make sure output string is only between 0 and 10 characters so string is
    // not out of bounds of the buffer.
    assert(cx >= 0 && cx < 11);
    // Only done to quiet release compiler warning for unused variable.
    (void)cx;

    return std::string(buffer);
}

std::string CreateTimeIntervalString(Real64 const StartTime, // Start of current interval in seconds
                                     Real64 const EndTime    // End of current interval in seconds
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function creates the time stamp with the current time interval from start and end
    // time values specified in seconds.
    // Inspired by similar function CreateSysTimeIntervalString() in General.cc

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    std::string TimeStmpS; // Character representation of start of interval
    std::string TimeStmpE; // Character representation of end of interval

    TimeStmpS = CreateTimeString(StartTime);
    TimeStmpE = CreateTimeString(EndTime);

    return TimeStmpS + " - " + TimeStmpE;
}

void ParseTime(Real64 const Time, // Time value in seconds
               int &Hours,        // Number of hours
               int &Minutes,      // Number of minutes < 60
               Real64 &Seconds    // Number of seconds < 60
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Dimitri Curtil
    //       DATE WRITTEN   January 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine decomposes a time value specified in seconds
    // into a triplet { hours : minutes : seconds } such that
    // - minutes < 60
    // - seconds < 60

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int const MinToSec(60);
    int const HourToSec(MinToSec * 60);

    // Get number of hours
    // This might undershoot the actual number of hours. See DO WHILE loop.
    Hours = int(Time) / HourToSec;

    // Compute remainder in seconds
    Real64 Remainder = (Time - Hours * HourToSec);

    // Compute minutes
    Minutes = int(Remainder) / MinToSec;

    // Compute remainder in seconds
    Remainder -= Minutes * MinToSec;

    // Compute seconds
    Seconds = Remainder;
}

void ScanForReports(EnergyPlusData &state,
                    std::string const &reportName,
                    bool &DoReport,
                    Optional_string_const ReportKey,
                    Optional_string Option1,
                    Optional_string Option2)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine scans for the global "reports" settings, such as Variable Dictionary,
    // Surfaces (and options), Constructions, etc.

    // METHODOLOGY EMPLOYED:
    // First time routine is called, all the viable combinations/settings for the reports are
    // stored in SAVEd variables.  Later callings will retrieve those.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumReports;
    int RepNum;
    int NumNames;
    int NumNumbers;
    int IOStat;
    auto &DXFOption1 = state.dataGeneral->DXFOption1;
    auto &DXFOption2 = state.dataGeneral->DXFOption2;
    auto &DXFWFOption1 = state.dataGeneral->DXFWFOption1;
    auto &DXFWFOption2 = state.dataGeneral->DXFWFOption2;
    auto &VRMLOption1 = state.dataGeneral->VRMLOption1;
    auto &VRMLOption2 = state.dataGeneral->VRMLOption2;
    auto &ViewRptOption1 = state.dataGeneral->ViewRptOption1;
    auto &LineRptOption1 = state.dataGeneral->LineRptOption1;
    auto &VarDictOption1 = state.dataGeneral->VarDictOption1;
    auto &VarDictOption2 = state.dataGeneral->VarDictOption2;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    if (state.dataGeneral->GetReportInput) {

        cCurrentModuleObject = "Output:Surfaces:List";

        NumReports = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        enum
        {
            EMPTY,
            LINES,
            VERTICES,
            DETAILS,
            DETAILSWITHVERTICES,
            COSTINFO,
            VIEWFACTORINFO,
            DECAYCURVESFROMCOMPONENTLOADSSUMMARY
        };
        std::map<std::string, int> localMap = {{"", EMPTY},
                                               {"LINES", LINES},
                                               {"VERTICES", VERTICES},
                                               {"DETAILS", DETAILS},
                                               {"DETAILED", DETAILS},
                                               {"DETAIL", DETAILS},
                                               {"DETAILSWITHVERTICES", DETAILSWITHVERTICES},
                                               {"DETAILVERTICES", DETAILSWITHVERTICES},
                                               {"COSTINFO", COSTINFO},
                                               {"VIEWFACTORINFO", VIEWFACTORINFO},
                                               {"DECAYCURVESFROMCOMPONENTLOADSSUMMARY", DECAYCURVESFROMCOMPONENTLOADSSUMMARY}};

        for (RepNum = 1; RepNum <= NumReports; ++RepNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     RepNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumNames,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            try {
                int value = localMap[state.dataIPShortCut->cAlphaArgs(1)];
                switch (value) {
                case LINES:
                    state.dataGeneral->LineRpt = true;
                    LineRptOption1 = state.dataIPShortCut->cAlphaArgs(2);
                    break;
                case VERTICES:
                    state.dataGeneral->SurfVert = true;
                    break;
                case DETAILS:
                    state.dataGeneral->SurfDet = true;
                    break;
                case DETAILSWITHVERTICES:
                    state.dataGeneral->SurfDetWVert = true;
                    break;
                case COSTINFO:
                    //   Custom case for reporting surface info for cost estimates (for first costs in opitimzing)
                    state.dataGeneral->CostInfo = true;
                    break;
                case VIEWFACTORINFO: // actual reporting is in HeatBalanceIntRadExchange
                    state.dataGeneral->ViewFactorInfo = true;
                    ViewRptOption1 = state.dataIPShortCut->cAlphaArgs(2);
                    break;
                case DECAYCURVESFROMCOMPONENTLOADSSUMMARY: // Should the Radiant to Convective Decay Curves from the
                                                           // load component report appear in the EIO file
                    state.dataGlobal->ShowDecayCurvesInEIO = true;
                    break;
                default: // including empty
                    ShowWarningError(state, cCurrentModuleObject + ": No " + state.dataIPShortCut->cAlphaFieldNames(1) + " supplied.");
                    ShowContinueError(state,
                                      R"( Legal values are: "Lines", "Vertices", "Details", "DetailsWithVertices", "CostInfo", "ViewFactorIinfo".)");
                }
            } catch (int e) {
                ShowWarningError(state,
                                 cCurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1) + "\" supplied.");
                ShowContinueError(state,
                                  R"( Legal values are: "Lines", "Vertices", "Details", "DetailsWithVertices", "CostInfo", "ViewFactorIinfo".)");
            }
        }

        cCurrentModuleObject = "Output:Surfaces:Drawing";

        NumReports = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (RepNum = 1; RepNum <= NumReports; ++RepNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     RepNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumNames,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(1));

                if (SELECT_CASE_var == "DXF") {
                    state.dataGeneral->DXFReport = true;
                    DXFOption1 = state.dataIPShortCut->cAlphaArgs(2);
                    DXFOption2 = state.dataIPShortCut->cAlphaArgs(3);

                } else if (SELECT_CASE_var == "DXF:WIREFRAME") {
                    state.dataGeneral->DXFWFReport = true;
                    DXFWFOption1 = state.dataIPShortCut->cAlphaArgs(2);
                    DXFWFOption2 = state.dataIPShortCut->cAlphaArgs(3);

                } else if (SELECT_CASE_var == "VRML") {
                    state.dataGeneral->VRMLReport = true;
                    VRMLOption1 = state.dataIPShortCut->cAlphaArgs(2);
                    VRMLOption2 = state.dataIPShortCut->cAlphaArgs(3);

                } else if (SELECT_CASE_var.empty()) {
                    ShowWarningError(state, cCurrentModuleObject + ": No " + state.dataIPShortCut->cAlphaFieldNames(1) + " supplied.");
                    ShowContinueError(state, R"( Legal values are: "DXF", "DXF:WireFrame", "VRML".)");

                } else {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\" supplied.");
                    ShowContinueError(state, R"( Legal values are: "DXF", "DXF:WireFrame", "VRML".)");
                }
            }
        }

        RepNum = state.dataInputProcessing->inputProcessor->getNumSectionsFound("Report Variable Dictionary");
        if (RepNum > 0) {
            state.dataGeneral->VarDict = true;
            VarDictOption1 = "REGULAR";
            VarDictOption2 = "";
        }

        cCurrentModuleObject = "Output:VariableDictionary";

        NumReports = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (RepNum = 1; RepNum <= NumReports; ++RepNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     RepNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumNames,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            state.dataGeneral->VarDict = true;
            VarDictOption1 = state.dataIPShortCut->cAlphaArgs(1);
            VarDictOption2 = state.dataIPShortCut->cAlphaArgs(2);
        }

        cCurrentModuleObject = "Output:Constructions";
        NumReports = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (RepNum = 1; RepNum <= NumReports; ++RepNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     RepNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumNames,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), "CONSTRUCTIONS")) {
                state.dataGeneral->Constructions = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), "MATERIALS")) {
                state.dataGeneral->Materials = true;
            }
            if (NumNames > 1) {
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "CONSTRUCTIONS")) {
                    state.dataGeneral->Constructions = true;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), "MATERIALS")) {
                    state.dataGeneral->Materials = true;
                }
            }
        }

        cCurrentModuleObject = "Output:EnergyManagementSystem";
        NumReports = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (RepNum = 1; RepNum <= NumReports; ++RepNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     RepNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumNames,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            state.dataGeneral->EMSoutput = true;

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(1));

                if (SELECT_CASE_var == "NONE") {
                    state.dataRuntimeLang->OutputEMSActuatorAvailSmall = false;
                    state.dataRuntimeLang->OutputEMSActuatorAvailFull = false;
                } else if (SELECT_CASE_var == "NOTBYUNIQUEKEYNAMES") {
                    state.dataRuntimeLang->OutputEMSActuatorAvailSmall = true;
                    state.dataRuntimeLang->OutputEMSActuatorAvailFull = false;
                } else if (SELECT_CASE_var == "VERBOSE") {
                    state.dataRuntimeLang->OutputEMSActuatorAvailSmall = false;
                    state.dataRuntimeLang->OutputEMSActuatorAvailFull = true;

                } else if (SELECT_CASE_var.empty()) {
                    ShowWarningError(state, cCurrentModuleObject + ": Blank " + state.dataIPShortCut->cAlphaFieldNames(1) + " supplied.");
                    ShowContinueError(state, R"( Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.)");
                    state.dataRuntimeLang->OutputEMSActuatorAvailSmall = false;
                    state.dataRuntimeLang->OutputEMSActuatorAvailFull = false;
                } else {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\" supplied.");
                    ShowContinueError(state, R"( Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.)");
                    state.dataRuntimeLang->OutputEMSActuatorAvailSmall = false;
                    state.dataRuntimeLang->OutputEMSActuatorAvailFull = false;
                }
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(2));

                if (SELECT_CASE_var == "NONE") {
                    state.dataRuntimeLang->OutputEMSInternalVarsFull = false;
                    state.dataRuntimeLang->OutputEMSInternalVarsSmall = false;
                } else if (SELECT_CASE_var == "NOTBYUNIQUEKEYNAMES") {
                    state.dataRuntimeLang->OutputEMSInternalVarsFull = false;
                    state.dataRuntimeLang->OutputEMSInternalVarsSmall = true;
                } else if (SELECT_CASE_var == "VERBOSE") {
                    state.dataRuntimeLang->OutputEMSInternalVarsFull = true;
                    state.dataRuntimeLang->OutputEMSInternalVarsSmall = false;
                } else if (SELECT_CASE_var.empty()) {
                    ShowWarningError(state, cCurrentModuleObject + ": Blank " + state.dataIPShortCut->cAlphaFieldNames(2) + " supplied.");
                    ShowContinueError(state, R"( Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.)");
                    state.dataRuntimeLang->OutputEMSInternalVarsFull = false;
                    state.dataRuntimeLang->OutputEMSInternalVarsSmall = false;
                } else {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\" supplied.");
                    ShowContinueError(state, R"( Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.)");
                    state.dataRuntimeLang->OutputEMSInternalVarsFull = false;
                    state.dataRuntimeLang->OutputEMSInternalVarsSmall = false;
                }
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(3));

                if (SELECT_CASE_var == "NONE") {
                    state.dataRuntimeLang->OutputEMSErrors = false;
                    state.dataRuntimeLang->OutputFullEMSTrace = false;
                } else if (SELECT_CASE_var == "ERRORSONLY") {
                    state.dataRuntimeLang->OutputEMSErrors = true;
                    state.dataRuntimeLang->OutputFullEMSTrace = false;
                } else if (SELECT_CASE_var == "VERBOSE") {
                    state.dataRuntimeLang->OutputFullEMSTrace = true;
                    state.dataRuntimeLang->OutputEMSErrors = true;
                } else if (SELECT_CASE_var.empty()) {
                    ShowWarningError(state, cCurrentModuleObject + ": Blank " + state.dataIPShortCut->cAlphaFieldNames(3) + " supplied.");
                    ShowContinueError(state, R"( Legal values are: "None", "ErrorsOnly", "Verbose". "None" will be used.)");
                    state.dataRuntimeLang->OutputEMSErrors = false;
                    state.dataRuntimeLang->OutputFullEMSTrace = false;
                } else {
                    ShowWarningError(state,
                                     cCurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\" supplied.");
                    ShowContinueError(state, R"( Legal values are: "None", "ErrorsOnly", "Verbose". "None" will be used.)");
                    state.dataRuntimeLang->OutputEMSErrors = false;
                    state.dataRuntimeLang->OutputFullEMSTrace = false;
                }
            }
        }

        state.dataGeneral->GetReportInput = false;
    }

    // Process the Scan Request
    DoReport = false;

    {
        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(reportName));
        if (SELECT_CASE_var == "CONSTRUCTIONS") {
            if (present(ReportKey)) {
                if (UtilityRoutines::SameString(ReportKey, "Constructions")) DoReport = state.dataGeneral->Constructions;
                if (UtilityRoutines::SameString(ReportKey, "Materials")) DoReport = state.dataGeneral->Materials;
            }
        } else if (SELECT_CASE_var == "VIEWFACTORINFO") {
            DoReport = state.dataGeneral->ViewFactorInfo;
            if (present(Option1)) Option1 = ViewRptOption1;
        } else if (SELECT_CASE_var == "VARIABLEDICTIONARY") {
            DoReport = state.dataGeneral->VarDict;
            if (present(Option1)) Option1 = VarDictOption1;
            if (present(Option2)) Option2 = VarDictOption2;
            //    CASE ('SCHEDULES')
            //     DoReport=SchRpt
            //      IF (PRESENT(Option1)) Option1=SchRptOption
        } else if (SELECT_CASE_var == "SURFACES") {
            {
                auto const SELECT_CASE_var1(UtilityRoutines::MakeUPPERCase(ReportKey)); // Autodesk:OPTIONAL ReportKey used without PRESENT check
                if (SELECT_CASE_var1 == "COSTINFO") {
                    DoReport = state.dataGeneral->CostInfo;
                } else if (SELECT_CASE_var1 == "DXF") {
                    DoReport = state.dataGeneral->DXFReport;
                    if (present(Option1)) Option1 = DXFOption1;
                    if (present(Option2)) Option2 = DXFOption2;
                } else if (SELECT_CASE_var1 == "DXF:WIREFRAME") {
                    DoReport = state.dataGeneral->DXFWFReport;
                    if (present(Option1)) Option1 = DXFWFOption1;
                    if (present(Option2)) Option2 = DXFWFOption2;
                } else if (SELECT_CASE_var1 == "VRML") {
                    DoReport = state.dataGeneral->VRMLReport;
                    if (present(Option1)) Option1 = VRMLOption1;
                    if (present(Option2)) Option2 = VRMLOption2;
                } else if (SELECT_CASE_var1 == "VERTICES") {
                    DoReport = state.dataGeneral->SurfVert;
                } else if (SELECT_CASE_var1 == "DETAILS") {
                    DoReport = state.dataGeneral->SurfDet;
                } else if (SELECT_CASE_var1 == "DETAILSWITHVERTICES") {
                    DoReport = state.dataGeneral->SurfDetWVert;
                } else if (SELECT_CASE_var1 == "LINES") {
                    DoReport = state.dataGeneral->LineRpt;
                    if (present(Option1)) Option1 = LineRptOption1;
                } else {
                }
            }
        } else if (SELECT_CASE_var == "ENERGYMANAGEMENTSYSTEM") {
            DoReport = state.dataGeneral->EMSoutput;
        } else {
        }
    }
}

void CheckCreatedZoneItemName(EnergyPlusData &state,
                              std::string_view const calledFrom,              // routine called from
                              std::string const &CurrentObject,               // object being parsed
                              std::string const &ZoneName,                    // Zone Name associated
                              std::string::size_type const MaxZoneNameLength, // maximum length of zonelist zone names
                              std::string const &ItemName,                    // Item name (People, Lights, etc object)
                              Array1_string const &ItemNames,                 // Item Names to check for duplication
                              int const NumItems,                             // Number of items in ItemNames array
                              std::string &ResultName,                        // Resultant name
                              bool &errFlag                                   // Error flag set to true if error found here.
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine checks "global" objects (that is, ones with ZoneList used in the name
    // specification) along with a specific name for the current object for length and duplication
    // with previous objects of that class.

    errFlag = false;
    std::string::size_type const ItemNameLength = len(ItemName);
    std::string::size_type const ItemLength = len(ZoneName) + ItemNameLength;
    ResultName = ZoneName + ' ' + ItemName;
    bool TooLong = false;
    if (ItemLength > DataGlobalConstants::MaxNameLength) {
        ShowWarningError(state, fmt::format("{}{} Combination of ZoneList and Object Name generate a name too long.", calledFrom, CurrentObject));
        ShowContinueError(state, "Object Name=\"" + ItemName + "\".");
        ShowContinueError(state, "ZoneList/Zone Name=\"" + ZoneName + "\".");
        ShowContinueError(
            state,
            format("Item length=[{}] > Maximum Length=[{}]. You may need to shorten the names.", ItemLength, DataGlobalConstants::MaxNameLength));
        ShowContinueError(state,
                          format("Shortening the Object Name by [{}] characters will assure uniqueness for this ZoneList.",
                                 MaxZoneNameLength + 1 + ItemNameLength - DataGlobalConstants::MaxNameLength));
        ShowContinueError(state, "name that will be used (may be needed in reporting)=\"" + ResultName + "\".");
        TooLong = true;
    }

    int FoundItem = UtilityRoutines::FindItemInList(ResultName, ItemNames, NumItems);

    if (FoundItem != 0) {
        ShowSevereError(state, fmt::format("{}{}=\"{}\", Duplicate Generated name encountered.", calledFrom, CurrentObject, ItemName));
        ShowContinueError(state, format("name=\"{}\" has already been generated or entered as {} item=[{}].", ResultName, CurrentObject, FoundItem));
        if (TooLong) ShowContinueError(state, "Duplicate name likely caused by the previous \"too long\" warning.");
        ResultName = "xxxxxxx";
        errFlag = true;
    }
}

// This is from OpenStudio
std::vector<std::string> splitString(const std::string &string, char delimiter)
{
    std::vector<std::string> results;
    if (!string.empty()) { // Only do work if there is work to do
        std::stringstream stream(string);
        std::string substring;
        while (std::getline(stream, substring, delimiter)) { // Loop and fill the results vector
            results.push_back(substring);
        }
        if (*(string.end() - 1) == ',') { // Add an empty string if the last char is the delimiter
            results.emplace_back();
        }
    }
    return results;
}

} // namespace EnergyPlus::General
