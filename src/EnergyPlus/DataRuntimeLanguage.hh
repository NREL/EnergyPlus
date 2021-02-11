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

#ifndef DataRuntimeLanguage_hh_INCLUDED
#define DataRuntimeLanguage_hh_INCLUDED

// C++ Headers
#include <functional>
#include <unordered_set>
#include <utility>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Reference.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Data only module for EMS runtime language

namespace DataRuntimeLanguage {

    // Data module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    enum class ErlKeywordParam // keyword parameters for types of Erl statements
    {
        KeywordNone,     // statement type not set
        KeywordReturn,   // Return statement, as in leave program
        KeywordGoto,     // Goto statement, used in parsing to manage IF-ElseIf-Else-EndIf and nesting
        KeywordSet,      // Set statement, as in assign RHS to LHS
        KeywordRun,      // Run statement, used to call a subroutine from a main program
        KeywordIf,       // If statement, begins an IF-ElseIf-Else-EndIf logic block
        KeywordElseIf,   // ElseIf statement, begins an ElseIf block
        KeywordElse,     // Else statement, begins an Else block
        KeywordEndIf,    // EndIf statement, terminates an IF-ElseIf-Else-EndIf logic block
        KeywordWhile,    // While statement, begins a While block
        KeywordEndWhile, // EndWhile statement, terminates a While block
    };

    // MODULE PARAMETER DEFINITIONS:
    int constexpr ValueNull(0);       // Erl entity type, "Null" value
    int constexpr ValueNumber(1);     // Erl entity type,  hard numeric value
    int constexpr ValueString(2);     // Erl entity type,  character data
    int constexpr ValueArray(3);      // Erl entity type,  not used yet, for future array type
    int constexpr ValueVariable(4);   // Erl entity type,  Erl variable
    int constexpr ValueExpression(5); // Erl entity type,  Erl expression
    int constexpr ValueTrend(6);      // Erl entity type,  Erl trend variable
    int constexpr ValueError(7);      // Erl entity type, processing of an expression failed, returned error

    int constexpr PntrReal(301);    // data type for overloaded pointer management, double real
    int constexpr PntrInteger(302); // data type for overloaded pointer management, integer
    int constexpr PntrLogical(303); // data type for overloaded pointer management, logical

    // Parameters for identifying operator types in Erl
    // The number of these parameters indicates the order of precedence
    int constexpr OperatorLiteral(1);         // Just stores a literal value
    int constexpr OperatorNegative(2);        // -  (unary) No LHS?
    int constexpr OperatorDivide(3);          // /
    int constexpr OperatorMultiply(4);        // *
    int constexpr OperatorSubtract(5);        // -  (binary)
    int constexpr OperatorAdd(6);             // +  (binary)
    int constexpr OperatorEqual(7);           // ==
    int constexpr OperatorNotEqual(8);        // <>
    int constexpr OperatorLessOrEqual(9);     // <=
    int constexpr OperatorGreaterOrEqual(10); // >=
    int constexpr OperatorLessThan(11);       // <
    int constexpr OperatorGreaterThan(12);    // >
    int constexpr OperatorRaiseToPower(13);   // ^
    int constexpr OperatorLogicalAND(14);     // &&
    int constexpr OperatorLogicalOR(15);     // ||
    // note there is an important check "> 15" to distinguish operators from functions
    //  so be careful if renumber these parameters.  Binary operator additions should get inserted here rather than appended

    // parameters for built-in Erl functions, these are processed like operators and numbering
    // must be sequential with the operators.
    // math functions
    int constexpr FuncRound(16);    // accessor for Fortran's DNINT()
    int constexpr FuncMod(17);      // accessor for Fortran's MOD()
    int constexpr FuncSin(18);      // accessor for Fortran's SIN()
    int constexpr FuncCos(19);      // accessor for Fortran's COS()
    int constexpr FuncArcSin(20);   // accessor for Fortran's ASIN()
    int constexpr FuncArcCos(21);   // accessor for Fortran's ACOS()
    int constexpr FuncDegToRad(22); // Multiplies degrees by DegToRad
    int constexpr FuncRadToDeg(23); // Divides radians by DegToRad
    int constexpr FuncExp(24);      // accessor for Fortran's EXP()
    int constexpr FuncLn(25);       // accessor for Fortran's LOG()
    int constexpr FuncMax(26);      // accessor for Fortran's MAX()
    int constexpr FuncMin(27);      // accessor for Fortran's MIN()
    int constexpr FuncABS(28);      // accessor for Fortran's ABS()
    int constexpr FuncRandU(29);    // accessor for Fortran's Random_Number() intrinsic, uniform distribution
    int constexpr FuncRandG(30);    // accessor for Gaussian/normal distribution random number
    int constexpr FuncRandSeed(31); // accessor for Fortran's Random_Seed() intrinsic

    // begin psychrometric routines
    int constexpr FuncRhoAirFnPbTdbW(32);    // accessor for E+ psych routine
    int constexpr FuncCpAirFnW(33);       // accessor for E+ psych routine
    int constexpr FuncHfgAirFnWTdb(34);      // accessor for E+ psych routine
    int constexpr FuncHgAirFnWTdb(35);       // accessor for E+ psych routine
    int constexpr FuncTdpFnTdbTwbPb(36);     // accessor for E+ psych routine
    int constexpr FuncTdpFnWPb(37);          // accessor for E+ psych routine
    int constexpr FuncHFnTdbW(38);           // accessor for E+ psych routine
    int constexpr FuncHFnTdbRhPb(39);        // accessor for E+ psych routine
    int constexpr FuncTdbFnHW(40);           // accessor for E+ psych routine
    int constexpr FuncRhovFnTdbRh(41);       // accessor for E+ psych routine
    int constexpr FuncRhovFnTdbRhLBnd0C(42); // accessor for E+ psych routine
    int constexpr FuncRhovFnTdbWPb(43);      // accessor for E+ psych routine
    int constexpr FuncRhFnTdbRhov(44);       // accessor for E+ psych routine
    int constexpr FuncRhFnTdbRhovLBnd0C(45); // accessor for E+ psych routine
    int constexpr FuncRhFnTdbWPb(46);        // accessor for E+ psych routine
    int constexpr FuncTwbFnTdbWPb(47);       // accessor for E+ psych routine
    int constexpr FuncVFnTdbWPb(48);         // accessor for E+ psych routine
    int constexpr FuncWFnTdpPb(49);          // accessor for E+ psych routine
    int constexpr FuncWFnTdbH(50);           // accessor for E+ psych routine
    int constexpr FuncWFnTdbTwbPb(51);       // accessor for E+ psych routine
    int constexpr FuncWFnTdbRhPb(52);        // accessor for E+ psych routine
    int constexpr FuncPsatFnTemp(53);        // accessor for E+ psych routine
    int constexpr FuncTsatFnHPb(54);         // accessor for E+ psych routine
    int constexpr FuncTsatFnPb(55);          // not public in PsychRoutines.cc so not really available in EMS.
    int constexpr FuncCpCW(56);              // accessor for E+ psych routine
    int constexpr FuncCpHW(57);              // accessor for E+ psych routine
    int constexpr FuncRhoH2O(58);            // accessor for E+ psych routine

    // Simulation Management Functions
    int constexpr FuncFatalHaltEp(59);  // accessor for E+ error management, "Fatal" level
    int constexpr FuncSevereWarnEp(60); // accessor for E+ error management, "Severe" level
    int constexpr FuncWarnEp(61);       // accessor for E+ error management, "Warning" level

    // Trend variable handling Functions
    int constexpr FuncTrendValue(62);     // accessor for Erl Trend variables, instance value
    int constexpr FuncTrendAverage(63);   // accessor for Erl Trend variables, average value
    int constexpr FuncTrendMax(64);       // accessor for Erl Trend variables, max value
    int constexpr FuncTrendMin(65);       // accessor for Erl Trend variables, min value
    int constexpr FuncTrendDirection(66); // accessor for Erl Trend variables, slope value
    int constexpr FuncTrendSum(67);       // accessor for Erl Trend variables, sum value

    // Curve and Table access function
    int constexpr FuncCurveValue(68);

    // Weather data query functions
    int constexpr FuncTodayIsRain(69);          // Access TodayIsRain(hour, timestep)
    int constexpr FuncTodayIsSnow(70);          // Access TodayIsSnow(hour, timestep)
    int constexpr FuncTodayOutDryBulbTemp(71);  // Access TodayOutDryBulbTemp(hour, timestep)
    int constexpr FuncTodayOutDewPointTemp(72); // Access TodayOutDewPointTemp(hour, timestep)
    int constexpr FuncTodayOutBaroPress(73);    // Access TodayOutBaroPress(hour, timestep)
    int constexpr FuncTodayOutRelHum(74);       // Access TodayOutRelHum(hour, timestep)
    int constexpr FuncTodayWindSpeed(75);       // Access TodayWindSpeed(hour, timestep)
    int constexpr FuncTodayWindDir(76);         // Access TodayWindDir(hour, timestep)
    int constexpr FuncTodaySkyTemp(77);         // Access TodaySkyTemp(hour, timestep)
    int constexpr FuncTodayHorizIRSky(78);      // Access TodayHorizIRSky(hour, timestep)
    int constexpr FuncTodayBeamSolarRad(79);    // Access TodayBeamSolarRad(hour, timestep)
    int constexpr FuncTodayDifSolarRad(80);     // Access TodayDifSolarRad(hour, timestep)
    int constexpr FuncTodayAlbedo(81);          // Access TodayAlbedo(hour, timestep)
    int constexpr FuncTodayLiquidPrecip(82);    // Access TodayLiquidPrecip(hour, timestep)
    int constexpr FuncTomorrowIsRain(83);          // Access TomorrowIsRain(hour, timestep)
    int constexpr FuncTomorrowIsSnow(84);          // Access TomorrowIsSnow(hour, timestep)
    int constexpr FuncTomorrowOutDryBulbTemp(85);  // Access TomorrowOutDryBulbTemp(hour, timestep)
    int constexpr FuncTomorrowOutDewPointTemp(86); // Access TomorrowOutDewPointTemp(hour, timestep)
    int constexpr FuncTomorrowOutBaroPress(87);    // Access TomorrowOutBaroPress(hour, timestep)
    int constexpr FuncTomorrowOutRelHum(88);       // Access TomorrowOutRelHum(hour, timestep)
    int constexpr FuncTomorrowWindSpeed(89);       // Access TomorrowWindSpeed(hour, timestep)
    int constexpr FuncTomorrowWindDir(90);         // Access TomorrowWindDir(hour, timestep)
    int constexpr FuncTomorrowSkyTemp(91);         // Access TomorrowSkyTemp(hour, timestep)
    int constexpr FuncTomorrowHorizIRSky(92);      // Access TomorrowHorizIRSky(hour, timestep)
    int constexpr FuncTomorrowBeamSolarRad(93);    // Access TomorrowBeamSolarRad(hour, timestep)
    int constexpr FuncTomorrowDifSolarRad(94);     // Access TomorrowDifSolarRad(hour, timestep)
    int constexpr FuncTomorrowAlbedo(95);          // Access TomorrowAlbedo(hour, timestep)
    int constexpr FuncTomorrowLiquidPrecip(96);    // Access TomorrowLiquidPrecip(hour, timestep)

    int constexpr NumPossibleOperators(96); // total number of operators and built-in functions

    int constexpr MaxWhileLoopIterations(1000000); // protect from infinite loop in WHILE loops

    // Types

    struct OutputVarSensorType
    {
        // Members
        std::string Name;          // name of associated Erl Variable
        std::string UniqueKeyName; // unique key name associated with output variable
        std::string OutputVarName; // name of output variable
        bool CheckedOkay;          // set to true once checked out okay
        int Type;                  // type of output var, 1=integer, 2=real, 3=meter
        int Index;                 // ref index in output processor, points to variable
        int VariableNum;           // ref to global variable in runtime language
        int SchedNum;              // ref index ptr to schedule service (filled if Schedule Value)
        //  INTEGER                                 :: VarType       = 0

        // Default Constructor
        OutputVarSensorType() : CheckedOkay(false), Type(0), Index(0), VariableNum(0), SchedNum(0)
        {
        }
    };

    struct InternalVarsAvailableType
    {
        // Members
        // structure for internal data available for use in Erl that are not sourced by output variables
        std::string DataTypeName;    // general internal variable name registered, All uppercase
        std::string UniqueIDName;    // unique id for internal var, All uppercase
        std::string Units;           // registered units, used for reporting and checks.
        int PntrVarTypeUsed;         // data type used: integer (PntrInteger) or real (PntrReal)
        Real64 * RealValue; // POINTER to the REAL value that is being accessed
        int * IntValue;      // POINTER to the Integer value that is being accessed

        // Default Constructor
        InternalVarsAvailableType() : PntrVarTypeUsed(0), RealValue(nullptr), IntValue(nullptr)
        {
        }
    };

    struct InternalVarsUsedType
    {
        // Members
        // structure for internal data that user has selected to use in Erl.
        std::string Name;                 // Erl variable name
        std::string InternalDataTypeName; // general internal variable name, All uppercase
        std::string UniqueIDName;         // unique id for internal var, All uppercase
        bool CheckedOkay;                 // set to true once matched to available internal var
        int ErlVariableNum;               // points to global Erl variable, matches Name
        int InternVarNum;                 // points to index match in EMSInternalVarsAvailable structure

        // Default Constructor
        InternalVarsUsedType() : CheckedOkay(false), ErlVariableNum(0), InternVarNum(0)
        {
        }
    };

    struct EMSActuatorAvailableType
    {
        // Members
        // structure for all the actuators available for use in Erl
        std::string ComponentTypeName; // general actuator name registered, All uppercase
        std::string UniqueIDName;      // unique id for actuator, All uppercase
        std::string ControlTypeName;   // control type id for actuator, All uppercase
        std::string Units;             // control value units, used for reporting and checks.
        int handleCount;               // Number of times you tried to get a handle on this actuator,
                                       // whether from EMS:Actuator or getActuatorHandle (API)
        int PntrVarTypeUsed;           // data type used: integer (PntrInteger), real (PntrReal) or logical (PntrLogical)
        bool * Actuated;     // POINTER to the logical value that signals EMS is actuating
        Real64 * RealValue; // POINTER to the REAL value that is being actuated
        int * IntValue;      // POINTER to the Integer value that is being actuated
        bool * LogValue;     // POINTER to the Logical value that is being actuated

        // Default Constructor
        EMSActuatorAvailableType() : handleCount(0), PntrVarTypeUsed(0), Actuated(nullptr), RealValue(nullptr), IntValue(nullptr), LogValue(nullptr)
        {
        }
    };

    struct ActuatorUsedType
    {
        // Members
        // structure for actuators user selected to use in Erl
        std::string Name;              // Erl variable name
        std::string ComponentTypeName; // general actuator name, All uppercase
        std::string UniqueIDName;      // unique id for actuator, All uppercase
        std::string ControlTypeName;   // control type id for actuator, All uppercase
        bool CheckedOkay;              // set to true once matched to available actuator
        int ErlVariableNum;            // points to global Erl variable, matches Name
        int ActuatorVariableNum;       // points to index match in EMSActuatorAvailable structure

        // Default Constructor
        ActuatorUsedType() : CheckedOkay(false), ErlVariableNum(0), ActuatorVariableNum(0)
        {
        }
    };

    struct EMSProgramCallManagementType
    {
        // Members
        // structure for Erl program calling managers
        std::string Name;          // user defined name for calling manager
        EMSManager::EMSCallFrom CallingPoint; // EMS Calling point for this manager, see parameters emsCallFrom*
        int NumErlPrograms;        // count of total number of Erl programs called by this manager
        Array1D_int ErlProgramARR; // list of integer pointers to Erl programs used by this manager

        // Default Constructor
        EMSProgramCallManagementType() : CallingPoint(EMSManager::EMSCallFrom::Unassigned), NumErlPrograms(0)
        {
        }
    };

    struct ErlValueType
    {
        // Members
        // instance data structure for the values taken by Erl variables, nested structure in ErlVariable
        int Type;           // value type, eg. ValueNumber,
        Real64 Number;      // numeric value instance for Erl variable
        std::string String; // string data types in Erl (not used yet)
        int Variable;       // Pointer to another Erl variable
        //  Might be good to change names to VariableNum and ExpressionNum just to be clear
        int Expression;      // Pointer to another Erl expression (e.g. compound operators)
        bool TrendVariable;  // true if Erl variable is really a trend variable
        int TrendVarPointer; // index to match in TrendVariable structure
        std::string Error;   // holds error message string for reporting
        bool initialized;    // true if number value has been SET (ie. has been on LHS in SET expression)

        // Default Constructor
        ErlValueType() : Type(0), Number(0.0), Variable(0), Expression(0), TrendVariable(false), TrendVarPointer(0), initialized(false)
        {
        }

        // Member Constructor
        ErlValueType(int const Type,            // value type, eg. ValueNumber,
                     Real64 const Number,       // numeric value instance for Erl variable
                     std::string const &String, // string data types in Erl (not used yet)
                     int const Variable,        // Pointer to another Erl variable
                     int const Expression,      // Pointer to another Erl expression (e.g. compound operators)
                     bool const TrendVariable,  // true if Erl variable is really a trend variable
                     int const TrendVarPointer, // index to match in TrendVariable structure
                     std::string const &Error,  // holds error message string for reporting
                     bool const initialized)
            : Type(Type), Number(Number), String(String), Variable(Variable), Expression(Expression), TrendVariable(TrendVariable),
              TrendVarPointer(TrendVarPointer), Error(Error), initialized(initialized)
        {
        }
    };

    struct ErlVariableType
    {
        // Members
        // structure for Erl variables
        std::string Name;            // Erl Variable Name
        int StackNum;                // 0 for global Erl variables, index in ErlStack structure if local
        ErlValueType Value;          // values taken by Erl variables
        bool ReadOnly;               // true if Erl variable is read-only
        bool SetByExternalInterface; // set to true if value is set by ExternalInterface

        // Default Constructor
        ErlVariableType() : StackNum(0), ReadOnly(false), SetByExternalInterface(false)
        {
        }
    };

    struct InstructionType
    {
        // Members
        // nested structure inside ErlStack that holds program instructions
        int LineNum;   // Erl program line number reference
        DataRuntimeLanguage::ErlKeywordParam Keyword; // type of instruction for this line, e.g. KeywordSet, KeywordIf, etc
        int Argument1; // Index to a variable, function, expression, or stack
        int Argument2; // Index to a variable, function, expression, or stack

        // Default Constructor
        InstructionType() : LineNum(0), Keyword(DataRuntimeLanguage::ErlKeywordParam::KeywordNone), Argument1(0), Argument2(0)
        {
        }
    };

    struct ErlStackType // Stores Erl programs in a stack of statements/instructions
    {
        // Members
        std::string Name;                     // Erl program or subroutine name, user defined
        int NumLines;                         // count of lines in Erl program or subroutine
        Array1D_string Line;                  // string array holding lines of Erl code (for processing)
        int NumInstructions;                  // count of program instructions in stack
        Array1D<InstructionType> Instruction; // structure array of program instructions
        int NumErrors;                        // count of errors during stack parsing
        Array1D_string Error;                 // array of error messages from stack parsing

        // Default Constructor
        ErlStackType() : NumLines(0), NumInstructions(0), NumErrors(0)
        {
        }
    };

    struct ErlExpressionType
    {
        // Members
        int Operator;                  // indicates the type of operator or function 1..64
        int NumOperands;               // count of operands in expression
        Array1D<ErlValueType> Operand; // holds Erl values for operands in expression

        // Default Constructor
        ErlExpressionType() : Operator(0), NumOperands(0)
        {
        }
    };

    struct OperatorType
    {
        // Members
        // structure for operators and functions, used to look up information about each operator or function
        std::string Symbol; // string representation of operator or function (for reporting)
        int Code;           // integer code 1..64, identifies operator or function
        int NumOperands;    // count of operands or function arguments.

        // Default Constructor
        OperatorType() : Code(0), NumOperands(0)
        {
        }
    };

    struct TrendVariableType
    {
        // Members
        std::string Name;
        int ErlVariablePointer;       // the Erl variable being logged in trend
        int LogDepth;                 // number of timesteps back
        Array1D<Real64> TrendValARR;  // the main storage of trend data
        Array1D<Real64> tempTrendARR; // temporary holder during push
        Array1D<Real64> TimeARR;      // hours back in time for trend points

        // Default Constructor
        TrendVariableType() : ErlVariablePointer(0), LogDepth(0)
        {
        }
    };

    // EMS Actuator fast duplicate check lookup support
    typedef std::tuple<std::string, std::string, std::string> EMSActuatorKey;
    struct EMSActuatorKey_hash
    {
        inline static void hash_combine(std::size_t &seed, std::string const &s)
        {
            std::hash<std::string> hasher;
            seed ^= hasher(s) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }

        inline std::size_t operator()(EMSActuatorKey const &key) const
        {
            std::size_t seed(0);
            hash_combine(seed, std::get<0>(key));
            hash_combine(seed, std::get<1>(key));
            hash_combine(seed, std::get<2>(key));
            return seed;
        }
    };

    void ValidateEMSVariableName(EnergyPlusData &state,
                                 std::string const &cModuleObject, // the current object name
                                 std::string const &cFieldValue,   // the field value
                                 std::string const &cFieldName,    // the current field name
                                 bool &errFlag,                    // true if errors found in this routine, false otherwise.
                                 bool &ErrorsFound                 // true if errors found in this routine, untouched otherwise.
    );

    void ValidateEMSProgramName(EnergyPlusData &state,
                                std::string const &cModuleObject, // the current object name
                                std::string const &cFieldValue,   // the field value
                                std::string const &cFieldName,    // the current field name
                                std::string const &cSubType,      // sub type = Program or Subroutine
                                bool &errFlag,                    // true if errors found in this routine, false otherwise.
                                bool &ErrorsFound                 // true if errors found in this routine, untouched otherwise.
    );

} // namespace DataRuntimeLanguage

struct RuntimeLanguageData : BaseGlobalStruct {

    int NumProgramCallManagers = 0;                 // count of Erl program managers with calling points
    int NumSensors = 0;                             // count of EMS sensors used in model (data from output variables)
    int numActuatorsUsed = 0;                       // count of EMS actuators used in model
    int numEMSActuatorsAvailable = 0;               // count of EMS actuators available for use in such a model
    int maxEMSActuatorsAvailable = 0;               // count of EMS current maximum actuators available for use in such a model
    int NumInternalVariablesUsed = 0;               // count of EMS internal variables used in model
    int numEMSInternalVarsAvailable = 0;            // count of EMS internal variables available for use in such a model
    int maxEMSInternalVarsAvailable = 0;            // count of EMS current maximum internal variables available for use in such a model
    int varsAvailableAllocInc = 1000;               // allocation increment for variable arrays
    int NumErlPrograms = 0;                         // count of Erl programs in model
    int NumErlSubroutines = 0;                      // count of Erl subroutines in model
    int NumUserGlobalVariables = 0;                 // count of global EMS variables defined by user
    int NumErlVariables = 0;                        // count of Erl variables
    int NumErlStacks = 0;                           // count of Erl program stacks in model. sum of programs and subroutines
    int NumExpressions = 0;                         // count of Erl expressions
    int NumEMSOutputVariables = 0;                  // count of EMS output variables, custom output variables from Erl
    int NumEMSMeteredOutputVariables = 0;           // count of EMS metered output variables, custom meters from Erl
    int NumErlTrendVariables = 0;                   // count of EMS trend variables in model
    int NumEMSCurveIndices = 0;                     // count of EMS curve index variables in model
    int NumEMSConstructionIndices = 0;              // count of EMS construction index variables in model

    //######################################################################################################################################
    // code for ExternalInterface
    int NumExternalInterfaceGlobalVariables = 0;                           // count of ExternalInterface runtime variable
    int NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables = 0; // count of ExternalInterface runtime variable for FMUImport
    // will be updated with values from ExternalInterface
    int NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables = 0; // count of ExternalInterface runtime variable for FMUExport
    // will be updated with values from ExternalInterface
    int NumExternalInterfaceActuatorsUsed = 0;                           // count of ExternalInterface Actuators
    int NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed = 0; // count of ExternalInterface Actuators for FMUImport
    int NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed = 0; // count of ExternalInterface Actuators for FMUExport

    //######################################################################################################################################

    bool OutputEDDFile = false;                     // set to true if user requests EDD output file be written
    bool OutputFullEMSTrace = false;                // how much to write out to trace, if true do verbose for each line
    bool OutputEMSErrors = false;                   // how much to write out to trace, if true include Erl error messages
    bool OutputEMSActuatorAvailFull = false;        // how much to write out to EDD file, if true dump full combinatorial actuator list
    bool OutputEMSActuatorAvailSmall = false;       // how much to write out to EDD file, if true dump actuator list without key names
    bool OutputEMSInternalVarsFull = false;         // how much to write out to EDD file, if true dump full combinatorial internal list
    bool OutputEMSInternalVarsSmall = false;        // how much to write out to EDD file, if true dump internal list without key names

    Array2D_bool EMSConstructActuatorChecked;
    Array2D_bool EMSConstructActuatorIsOkay;

    // Object Data
    Array1D<DataRuntimeLanguage::ErlVariableType> ErlVariable;                        // holds Erl variables in a structure array
    Array1D<DataRuntimeLanguage::ErlStackType> ErlStack;                              // holds Erl programs in separate "stacks"
    Array1D<DataRuntimeLanguage::ErlExpressionType> ErlExpression;                    // holds Erl expressions in structure array
    Array1D<DataRuntimeLanguage::OperatorType> PossibleOperators;                     // hard library of available operators and functions
    Array1D<DataRuntimeLanguage::TrendVariableType> TrendVariable;                    // holds Erl trend variables in a structure array
    Array1D<DataRuntimeLanguage::OutputVarSensorType> Sensor;                         // EMS:SENSOR objects used (from output variables)
    Array1D<DataRuntimeLanguage::EMSActuatorAvailableType> EMSActuatorAvailable;      // actuators that could be used
    Array1D<DataRuntimeLanguage::ActuatorUsedType> EMSActuatorUsed;                   // actuators that are used
    Array1D<DataRuntimeLanguage::InternalVarsAvailableType> EMSInternalVarsAvailable; // internal data that could be used
    Array1D<DataRuntimeLanguage::InternalVarsUsedType> EMSInternalVarsUsed;           // internal data that are used
    Array1D<DataRuntimeLanguage::EMSProgramCallManagementType> EMSProgramCallManager; // program calling managers
    DataRuntimeLanguage::ErlValueType Null = DataRuntimeLanguage::ErlValueType(0, 0.0, "", 0, 0, false, 0, "", true);     // special "null" Erl variable value instance
    DataRuntimeLanguage::ErlValueType False = DataRuntimeLanguage::ErlValueType(0, 0.0, "", 0, 0, false, 0, "", true);    // special "false" Erl variable value instance
    DataRuntimeLanguage::ErlValueType True = DataRuntimeLanguage::ErlValueType(0, 0.0, "", 0, 0, false, 0, "", true);     // special "True" Erl variable value instance, gets reset

    // EMS Actuator fast duplicate check lookup support
    std::unordered_set<std::tuple<std::string, std::string, std::string>, DataRuntimeLanguage::EMSActuatorKey_hash> EMSActuator_lookup; // Fast duplicate lookup structure

    void clear_state() override
    {
        this->NumProgramCallManagers = 0;
        this->NumSensors = 0;
        this->numActuatorsUsed = 0;
        this->numEMSActuatorsAvailable = 0;
        this->maxEMSActuatorsAvailable = 0;
        this->NumInternalVariablesUsed = 0;
        this->numEMSInternalVarsAvailable = 0;
        this->maxEMSInternalVarsAvailable = 0;
        this->varsAvailableAllocInc = 1000;
        this->NumErlPrograms = 0;
        this->NumErlSubroutines = 0;
        this->NumUserGlobalVariables = 0;
        this->NumErlVariables = 0;
        this->NumErlStacks = 0;
        this->NumExpressions = 0;
        this->NumEMSOutputVariables = 0;
        this->NumEMSMeteredOutputVariables = 0;
        this->NumErlTrendVariables = 0;
        this->NumEMSCurveIndices = 0;
        this->NumEMSConstructionIndices = 0;
        this->NumExternalInterfaceGlobalVariables = 0;
        this->NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables = 0;
        this->NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables = 0;
        this->NumExternalInterfaceActuatorsUsed = 0;
        this->NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed = 0;
        this->NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed = 0;
        this->OutputEDDFile = false;
        this->OutputFullEMSTrace = false;
        this->OutputEMSErrors = false;
        this->OutputEMSActuatorAvailFull = false;
        this->OutputEMSActuatorAvailSmall = false;
        this->OutputEMSInternalVarsFull = false;
        this->OutputEMSInternalVarsSmall = false;
        this->EMSConstructActuatorChecked.deallocate();
        this->EMSConstructActuatorIsOkay.deallocate();
        this->ErlVariable.deallocate();
        this->ErlStack.deallocate();
        this->ErlExpression.deallocate();
        this->PossibleOperators.deallocate();
        this->TrendVariable.deallocate();
        this->Sensor.deallocate();
        this->EMSActuatorAvailable.deallocate();
        this->EMSActuatorUsed.deallocate();
        this->EMSInternalVarsAvailable.deallocate();
        this->EMSInternalVarsUsed.deallocate();
        this->EMSProgramCallManager.deallocate();
        this->EMSActuator_lookup.clear();
        this->Null = DataRuntimeLanguage::ErlValueType(0, 0.0, "", 0, 0, false, 0, "", true);
        this->False = DataRuntimeLanguage::ErlValueType(0, 0.0, "", 0, 0, false, 0, "", true);
        this->True = DataRuntimeLanguage::ErlValueType(0, 0.0, "", 0, 0, false, 0, "", true);
    }
};

} // namespace EnergyPlus

#endif
