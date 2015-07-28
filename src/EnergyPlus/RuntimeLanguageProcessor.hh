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

		// Member Constructor
		TokenType(
			int const Type, // token type, eg. TokenNumber
			Real64 const Number, // May want to store all literals as a variable?
			std::string const & String, // Serves double duty, also saves string version of token for easy debugging
			int const Operator, // indentifies operator or function 1..64
			int const Variable, // points to a variable in ErlVariable structure
			int const Parenthesis, // identifes if token is left or right parenthesis
			int const Expression, // points to an expression in ErlExpression structure
			std::string const & Error // holds token processing error message content
		) :
			Type( Type ),
			Number( Number ),
			String( String ),
			Operator( Operator ),
			Variable( Variable ),
			Parenthesis( Parenthesis ),
			Expression( Expression ),
			Error( Error )
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

		// Member Constructor
		RuntimeReportVarType(
			std::string const & Name, // name of custom Erl report variable
			int const VariableNum, // pointer to Erl variable associated with custom report variable
			Real64 const Value // Value registered with output processor for report variable
		) :
			Name( Name ),
			VariableNum( VariableNum ),
			Value( Value )
		{}

	};

	// Object Data
	extern Array1D< RuntimeReportVarType > RuntimeReportVar;

	// Functions

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

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // RuntimeLanguageProcessor

} // EnergyPlus

#endif
