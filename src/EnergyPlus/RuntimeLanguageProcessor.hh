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

#ifndef RuntimeLanguageProcessor_hh_INCLUDED
#define RuntimeLanguageProcessor_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace RuntimeLanguageProcessor {

    // Using/Aliasing
    using DataRuntimeLanguage::ErlValueType;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    extern int const MaxErrors;

    // token type parameters for Erl code parsing
    extern int const TokenNumber;     // matches the ValueNumber
    extern int const TokenVariable;   // matches the ValueVariable
    extern int const TokenExpression; // matches the ValueExpression
    extern int const TokenOperator;   // includes basic operators and built-in functions.

    extern int const TokenParenthesis; // parenthesis token

    extern int const ParenthesisLeft;  // indicates left side parenthesis found in parsing
    extern int const ParenthesisRight; // indicates right side parenthesis found in parsing

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE TYPE DECLARATIONS:

    // INTERFACE BLOCK SPECIFICATIONS: na

    // MODULE VARIABLE DECLARATIONS:

    extern bool GetInput;
    extern bool InitializeOnce;
    extern bool MyEnvrnFlag;

    // index pointer references to dynamic built-in variables
    extern int NullVariableNum;
    extern int FalseVariableNum;
    extern int TrueVariableNum;
    extern int OffVariableNum;
    extern int OnVariableNum;
    extern int PiVariableNum;
    extern Array1D_int CurveIndexVariableNums;
    extern Array1D_int ConstructionIndexVariableNums;
    extern int YearVariableNum;
    extern int MonthVariableNum;
    extern int DayOfMonthVariableNum;
    extern int DayOfWeekVariableNum;
    extern int DayOfYearVariableNum;
    extern int HourVariableNum;
    extern int TimeStepsPerHourVariableNum;
    extern int TimeStepNumVariableNum;
    extern int MinuteVariableNum;
    extern int HolidayVariableNum;
    extern int DSTVariableNum;
    extern int CurrentTimeVariableNum;
    extern int SunIsUpVariableNum;
    extern int IsRainingVariableNum;
    extern int SystemTimeStepVariableNum;
    extern int ZoneTimeStepVariableNum;
    extern int CurrentEnvironmentPeriodNum;
    extern int ActualDateAndTimeNum;
    extern int ActualTimeNum;
    extern int WarmUpFlagNum;

    // SUBROUTINE SPECIFICATIONS:

    // Types

    struct TokenType
    {
        // Members
        // structure for token information for parsing Erl code
        int Type;           // token type, eg. TokenNumber
        Real64 Number;      // May want to store all literals as a variable?
        std::string String; // Serves double duty, also saves string version of token for easy debugging
        int Operator;       // indentifies operator or function 1..64
        int Variable;       // points to a variable in ErlVariable structure
        int Parenthesis;    // identifes if token is left or right parenthesis
        int Expression;     // points to an expression in ErlExpression structure
        std::string Error;  // holds token processing error message content

        // Default Constructor
        TokenType() : Type(0), Number(0.0), Operator(0), Variable(0), Parenthesis(0), Expression(0)
        {
        }
    };

    struct RuntimeReportVarType
    {
        // Members
        std::string Name; // name of custom Erl report variable
        int VariableNum;  // pointer to Erl variable associated with custom report variable
        Real64 Value;     // Value registered with output processor for report variable

        // Default Constructor
        RuntimeReportVarType() : VariableNum(0), Value(0.0)
        {
        }
    };

    // Object Data
    extern Array1D<RuntimeReportVarType> RuntimeReportVar;

    // Functions
    void clear_state();

    void InitializeRuntimeLanguage(EnergyPlusData &state);

    void BeginEnvrnInitializeRuntimeLanguage(EnergyPlusData &state);

    void ParseStack(EnergyPlusData &state, int const StackNum);

    int AddInstruction(EnergyPlusData &state,
                       int const StackNum,
                       int const LineNum,
                       DataRuntimeLanguage::ErlKeywordParam Keyword,
                       Optional_int_const Argument1 = _, // Erl variable index
                       Optional_int_const Argument2 = _);

    void AddError(EnergyPlusData &state,
                  int const StackNum,      // index pointer to location in ErlStack structure
                  int const LineNum,       // Erl program line number
                  std::string const &Error // error message to be added to ErlStack
    );

    ErlValueType EvaluateStack(EnergyPlusData &state, int const StackNum);

    void
    WriteTrace(EnergyPlusData &state, int const StackNum, int const InstructionNum, ErlValueType const &ReturnValue, bool const seriousErrorFound);

    //******************************************************************************************

    //  Expression Processor

    //******************************************************************************************

    void ParseExpression(EnergyPlusData &state,
                         std::string const &InString, // String of expression text written in the Runtime Language
                         int const StackNum,          // Parent StackNum??
                         int &ExpressionNum,          // index of expression in structure
                         std::string const &Line      // Actual line from string
    );

    int ProcessTokens(EnergyPlusData &state, const Array1D<TokenType> &TokenIN, int const NumTokensIN, int const StackNum, std::string const &ParsingString);

    int NewExpression(EnergyPlusData &state);

    ErlValueType EvaluateExpression(EnergyPlusData &state, int const ExpressionNum, bool &seriousErrorFound);

    void TodayTomorrowWeather(EnergyPlusData &state,
                              int const FunctionCode, Real64 const Operand1, Real64 const Operand2, Array2D<Real64> &TodayTomorrowWeatherSource, ErlValueType &ReturnVal);

    void TodayTomorrowWeather(EnergyPlusData &state,
                              int const FunctionCode, Real64 const Operand1, Real64 const Operand2, Array2D_bool &TodayTomorrowWeatherSource, ErlValueType &ReturnVal);

    int TodayTomorrowWeather(EnergyPlusData &state, int hour, int timestep, Array2D<Real64> &TodayTomorrowWeatherSource, Real64 &value);

    int TodayTomorrowWeather(EnergyPlusData &state, int hour, int timestep, Array2D<bool> &TodayTomorrowWeatherSource, int &value);

    void GetRuntimeLanguageUserInput(EnergyPlusData &state);

    void ReportRuntimeLanguage(EnergyPlusData &state);

    ErlValueType SetErlValueNumber(Real64 const Number, Optional<ErlValueType const> OrigValue = _);

    ErlValueType StringValue(std::string const &String);

    std::string ValueToString(ErlValueType const &Value);

    int FindEMSVariable(EnergyPlusData &state,
                        std::string const &VariableName, // variable name in Erl
                        int const StackNum);

    int NewEMSVariable(EnergyPlusData &state, std::string const &VariableName, int const StackNum, Optional<ErlValueType const> Value = _);

    void SetupPossibleOperators(EnergyPlusData &state);

    void ExternalInterfaceSetErlVariable(EnergyPlusData &state,
                                         int const varNum,  // The variable index to be written during run time
                                         Real64 const value // The real time value of the vairable to be set
    );

    void ExternalInterfaceInitializeErlVariable(EnergyPlusData &state,
                                                int const varNum,                 // The variable index to be written during run time
                                                ErlValueType const &initialValue, // The initial value
                                                bool const setToNull              // Flag, if true, value will be initialized to Null
    );

    bool isExternalInterfaceErlVariable(EnergyPlusData &state, int const varNum); // The variable index to be written during run time

} // namespace RuntimeLanguageProcessor

struct RuntimeLanguageProcessorData : BaseGlobalStruct {

    void clear_state() override {

    }
};

} // namespace EnergyPlus

#endif
