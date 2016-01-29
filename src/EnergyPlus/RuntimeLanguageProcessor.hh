// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef RuntimeLanguageProcessor_hh_INCLUDED
#define RuntimeLanguageProcessor_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataRuntimeLanguage.hh>

namespace EnergyPlus {

namespace RuntimeLanguageProcessor {

	// Using/Aliasing
	using DataRuntimeLanguage::ErlValueType;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const MaxErrors;

	// keyword parameters for types of Erl statements
	extern int const KeywordNone; // statement type not set
	extern int const KeywordReturn; // Return statement, as in leave program
	extern int const KeywordGoto; // Goto statement, used in parsing to manage IF-ElseIf-Else-EndIf and nesting
	extern int const KeywordSet; // Set statement, as in assign RHS to LHS
	extern int const KeywordRun; // Run statement, used to call a subroutine from a main program
	extern int const KeywordIf; // If statement, begins an IF-ElseIf-Else-EndIf logic block
	extern int const KeywordElseIf; // ElseIf statement, begins an ElseIf block
	extern int const KeywordElse; // Else statement, begins an Else block
	extern int const KeywordEndIf; // EndIf statement, terminates an IF-ElseIf-Else-EndIf logic block
	extern int const KeywordWhile; // While statement, begins a While block
	extern int const KeywordEndWhile; // EndWhile statement, terminates a While block

	// token type parameters for Erl code parsing
	extern int const TokenNumber; // matches the ValueNumber
	extern int const TokenVariable; // matches the ValueVariable
	extern int const TokenExpression; // matches the ValueExpression
	extern int const TokenOperator; // includes basic operators and built-in functions.

	extern int const TokenParenthesis; // parenthesis token

	extern int const ParenthesisLeft; // indicates left side parenthesis found in parsing
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
		int Type; // token type, eg. TokenNumber
		Real64 Number; // May want to store all literals as a variable?
		std::string String; // Serves double duty, also saves string version of token for easy debugging
		int Operator; // indentifies operator or function 1..64
		int Variable; // points to a variable in ErlVariable structure
		int Parenthesis; // identifes if token is left or right parenthesis
		int Expression; // points to an expression in ErlExpression structure
		std::string Error; // holds token processing error message content

		// Default Constructor
		TokenType() :
			Type( 0 ),
			Number( 0.0 ),
			Operator( 0 ),
			Variable( 0 ),
			Parenthesis( 0 ),
			Expression( 0 )
		{}

	};

	struct RuntimeReportVarType
	{
		// Members
		std::string Name; // name of custom Erl report variable
		int VariableNum; // pointer to Erl variable associated with custom report variable
		Real64 Value; // Value registered with output processor for report variable

		// Default Constructor
		RuntimeReportVarType() :
			VariableNum( 0 ),
			Value( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< RuntimeReportVarType > RuntimeReportVar;

	// Functions
	void
	clear_state();

	void
	InitializeRuntimeLanguage();

	void
	BeginEnvrnInitializeRuntimeLanguage();

	void
	ParseStack( int const StackNum );

	int
	AddInstruction(
		int const StackNum,
		int const LineNum,
		int const Keyword,
		Optional_int_const Argument1 = _, // Erl variable index
		Optional_int_const Argument2 = _
	);

	void
	AddError(
		int const StackNum, // index pointer to location in ErlStack structure
		int const LineNum, // Erl program line number
		std::string const & Error // error message to be added to ErlStack
	);

	ErlValueType
	EvaluateStack( int const StackNum );

	void
	WriteTrace(
		int const StackNum,
		int const InstructionNum,
		ErlValueType const & ReturnValue
	);

	//******************************************************************************************

	//  Expression Processor

	//******************************************************************************************

	void
	ParseExpression(
		std::string const & InString, // String of expression text written in the Runtime Language
		int const StackNum, // Parent StackNum??
		int & ExpressionNum, // index of expression in structure
		std::string const & Line // Actual line from string
	);

	int
	ProcessTokens(
		Array1S< TokenType > const TokenIN,
		int const NumTokensIN,
		int const StackNum,
		std::string const & ParsingString
	);

	int
	NewExpression();

	ErlValueType
	EvaluateExpression( int const ExpressionNum );

	void
	GetRuntimeLanguageUserInput();

	void
	ReportRuntimeLanguage();

	std::string
	IntegerToString( int const Number );

	ErlValueType
	SetErlValueNumber(
		Real64 const Number,
		Optional< ErlValueType const > OrigValue = _
	);

	ErlValueType
	StringValue( std::string const & String );

	std::string
	ValueToString( ErlValueType const & Value );

	int
	FindEMSVariable(
		std::string const & VariableName, // variable name in Erl
		int const StackNum
	);

	int
	NewEMSVariable(
		std::string const & VariableName,
		int const StackNum,
		Optional< ErlValueType const > Value = _
	);

	void
	SetupPossibleOperators();

	void
	ExternalInterfaceSetErlVariable(
		int const varNum, // The variable index to be written during run time
		Real64 const value // The real time value of the vairable to be set
	);

	void
	ExternalInterfaceInitializeErlVariable(
		int const varNum, // The variable index to be written during run time
		ErlValueType const & initialValue, // The initial value
		bool const setToNull // Flag, if true, value will be initialized to Null
	);

	bool
	isExternalInterfaceErlVariable( int const varNum ); // The variable index to be written during run time

} // RuntimeLanguageProcessor

} // EnergyPlus

#endif
