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
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

//Data only module for EMS runtime language

namespace DataRuntimeLanguage {

	// Using/Aliasing

	// Data
	// module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const ValueNull; // Erl entity type, "Null" value
	extern int const ValueNumber; // Erl entity type,  hard numeric value
	extern int const ValueString; // Erl entity type,  character data
	extern int const ValueArray; // Erl entity type,  not used yet, for future array type
	extern int const ValueVariable; // Erl entity type,  Erl variable
	extern int const ValueExpression; // Erl entity type,  Erl expression
	extern int const ValueTrend; // Erl entity type,  Erl trend variable
	extern int const ValueError; // Erl entity type, processing of an expression failed, returned error

	extern int const PntrReal; // data type for overloaded pointer management, double real
	extern int const PntrInteger; // data type for overloaded pointer management, integer
	extern int const PntrLogical; // data type for overloaded pointer management, logical

	extern int const MaxWhileLoopIterations; // protect from infinite loop in WHILE loops

	// Parameters for identifying operator types in Erl
	// The number of these parameters indicates the order of precedence
	extern int const OperatorLiteral; // Just stores a literal value
	extern int const OperatorNegative; // -  (unary) No LHS?
	extern int const OperatorDivide; // /
	extern int const OperatorMultiply; // *
	extern int const OperatorSubtract; // -  (binary)
	extern int const OperatorAdd; // +  (binary)
	extern int const OperatorEqual; // ==
	extern int const OperatorNotEqual; // <>
	extern int const OperatorLessOrEqual; // <=
	extern int const OperatorGreaterOrEqual; // >=
	extern int const OperatorLessThan; // <
	extern int const OperatorGreaterThan; // >
	extern int const OperatorRaiseToPower; // ^
	extern int const OperatorLogicalAND; // &&
	extern int const OperatiorLogicalOR; // ||
	// note there is an important check "> 15" to distinguish operators from functions
	//  so becareful if renumber these parameters.  Binary operator additions should get inserted here rather than appended

	//parameters for built-in Erl functions, these are processed like operators and numbering
	// must be sequential with the operators.
	// math functions
	extern int const FuncRound; // accessor for Fortran's DNINT()
	extern int const FuncMod; // accessor for Fortran's MOD()
	extern int const FuncSin; // accessor for Fortran's SIN()
	extern int const FuncCos; // accessor for Fortran's COS()
	extern int const FuncArcSin; // accessor for Fortran's ASIN()
	extern int const FuncArcCos; // accessor for Fortran's ACOS()
	extern int const FuncDegToRad; // Multiplies degrees by DegToRad
	extern int const FuncRadToDeg; // Divides radians by DegToRad
	extern int const FuncExp; // accessor for Fortran's EXP()
	extern int const FuncLn; // accessor for Fortran's LOG()
	extern int const FuncMax; // accessor for Fortran's MAX()
	extern int const FuncMin; // accessor for Fortran's MIN()
	extern int const FuncABS; // accessor for Fortran's ABS()
	extern int const FuncRandU; // accessor for Fortran's Random_Number() intrinsic, uniform distribution
	extern int const FuncRandG; // accessor for Gaussian/normal distribution random number
	extern int const FuncRandSeed; // accessor for Fortran's Random_Seed() intrinsic

	// begin psychrometric routines
	extern int const FuncRhoAirFnPbTdbW; // accessor for E+ psych routine
	extern int const FuncCpAirFnWTdb; // accessor for E+ psych routine
	extern int const FuncHfgAirFnWTdb; // accessor for E+ psych routine
	extern int const FuncHgAirFnWTdb; // accessor for E+ psych routine
	extern int const FuncTdpFnTdbTwbPb; // accessor for E+ psych routine
	extern int const FuncTdpFnWPb; // accessor for E+ psych routine
	extern int const FuncHFnTdbW; // accessor for E+ psych routine
	extern int const FuncHFnTdbRhPb; // accessor for E+ psych routine
	extern int const FuncTdbFnHW; // accessor for E+ psych routine
	extern int const FuncRhovFnTdbRh; // accessor for E+ psych routine
	extern int const FuncRhovFnTdbRhLBnd0C; // accessor for E+ psych routine
	extern int const FuncRhovFnTdbWPb; // accessor for E+ psych routine
	extern int const FuncRhFnTdbRhov; // accessor for E+ psych routine
	extern int const FuncRhFnTdbRhovLBnd0C; // accessor for E+ psych routine
	extern int const FuncRhFnTdbWPb; // accessor for E+ psych routine
	extern int const FuncTwbFnTdbWPb; // accessor for E+ psych routine
	extern int const FuncVFnTdbWPb; // accessor for E+ psych routine
	extern int const FuncWFnTdpPb; // accessor for E+ psych routine
	extern int const FuncWFnTdbH; // accessor for E+ psych routine
	extern int const FuncWFnTdbTwbPb; // accessor for E+ psych routine
	extern int const FuncWFnTdbRhPb; // accessor for E+ psych routine
	extern int const FuncPsatFnTemp; // accessor for E+ psych routine
	extern int const FuncTsatFnHPb; // accessor for E+ psych routine
	extern int const FuncTsatFnPb; // not public in PsychRoutines.cc so not really available in EMS.
	extern int const FuncCpCW; // accessor for E+ psych routine
	extern int const FuncCpHW; // accessor for E+ psych routine
	extern int const FuncRhoH2O; // accessor for E+ psych routine

	// Simulation Management Functions
	extern int const FuncFatalHaltEp; // accessor for E+ error management, "Fatal" level
	extern int const FuncSevereWarnEp; // accessor for E+ error management, "Severe" level
	extern int const FuncWarnEp; // accessor for E+ error management, "Warning" level

	// Trend variable handling Functions
	extern int const FuncTrendValue; // accessor for Erl Trend variables, instance value
	extern int const FuncTrendAverage; // accessor for Erl Trend variables, average value
	extern int const FuncTrendMax; // accessor for Erl Trend variables, max value
	extern int const FuncTrendMin; // accessor for Erl Trend variables, min value
	extern int const FuncTrendDirection; // accessor for Erl Trend variables, slope value
	extern int const FuncTrendSum; // accessor for Erl Trend variables, sum value

	// Curve and Table access function
	extern int const FuncCurveValue;

	extern int const NumPossibleOperators; // total number of operators and built-in functions

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_int EMSProgram;

	extern int NumProgramCallManagers; // count of Erl program managers with calling points
	extern int NumSensors; // count of EMS sensors used in model (data from output variables)
	extern int numActuatorsUsed; // count of EMS actuators used in model
	extern int numEMSActuatorsAvailable; // count of EMS actuators available for use in such a model
	extern int maxEMSActuatorsAvailable; // count of EMS current maximum actuators available for use in such a model
	extern int NumInternalVariablesUsed; // count of EMS internal variables used in model
	extern int numEMSInternalVarsAvailable; // count of EMS internal variables available for use in such a model
	extern int maxEMSInternalVarsAvailable; // count of EMS current maximum internal variables available for use in such a model
	extern int varsAvailableAllocInc; // allocation increment for variable arrays

	extern int NumErlPrograms; // count of Erl programs in model
	extern int NumErlSubroutines; // count of Erl subroutines in model
	extern int NumUserGlobalVariables; // count of global EMS variables defined by user
	extern int NumErlVariables; // count of Erl variables
	extern int NumErlStacks; // count of Erl program stacks in model. sum of programs and subroutines
	extern int NumExpressions; // count of Erl expressions
	extern int NumEMSOutputVariables; // count of EMS output variables, custom output variables from Erl
	extern int NumEMSMeteredOutputVariables; // count of EMS metered output variables, custom meters from Erl
	extern int NumErlTrendVariables; // count of EMS trend variables in model
	extern int NumEMSCurveIndices; // count of EMS curve index variables in model
	extern int NumEMSConstructionIndices; // count of EMS construction index variables in model

	//######################################################################################################################################
	//code for ExternalInterface
	extern int NumExternalInterfaceGlobalVariables; // count of ExternalInterface runtime variable
	extern int NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables; // count of ExternalInterface runtime variable for FMUImport
	// will be updated with values from ExternalInterface
	extern int NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables; // count of ExternalInterface runtime variable for FMUExport
	// will be updated with values from ExternalInterface
	extern int NumExternalInterfaceActuatorsUsed; // count of ExternalInterface Actuators
	extern int NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed; // count of ExternalInterface Actuators for FMUImport
	extern int NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed; // count of ExternalInterface Actuators for FMUExport

	//######################################################################################################################################

	extern int OutputEMSFileUnitNum; // file lun handle for open EMS output file
	extern bool OutputEDDFile; // set to true if user requests EDD output file be written
	extern bool OutputFullEMSTrace; // how much to write out to trace, if true do verbose for each line
	extern bool OutputEMSErrors; // how much to write out to trace, if true include Erl error messages
	extern bool OutputEMSActuatorAvailFull; // how much to write out to EDD file, if true dump full combinatorial actuator list
	extern bool OutputEMSActuatorAvailSmall; // how much to write out to EDD file, if true dump actuator list without key names
	extern bool OutputEMSInternalVarsFull; // how much to write out to EDD file, if true dump full combinatorial internal list
	extern bool OutputEMSInternalVarsSmall; // how much to write out to EDD file, if true dump internal list without key names

	extern Array2D_bool EMSConstructActuatorChecked;
	extern Array2D_bool EMSConstructActuatorIsOkay;

	// Types

	struct OutputVarSensorType
	{
		// Members
		std::string Name; // name of associated Erl Variable
		std::string UniqueKeyName; // unique key name associated with output variable
		std::string OutputVarName; // name of output variable
		bool CheckedOkay; // set to true once checked out okay
		int Type; // type of output var, 1=integer, 2=real, 3=meter
		int Index; // ref index in output processor, points to variable
		int VariableNum; // ref to global variable in runtime language
		int SchedNum; // ref index ptr to schedule service (filled if Schedule Value)
		//  INTEGER                                 :: VarType       = 0

		// Default Constructor
		OutputVarSensorType() :
			CheckedOkay( false ),
			Type( 0 ),
			Index( 0 ),
			VariableNum( 0 ),
			SchedNum( 0 )
		{}

	};

	struct InternalVarsAvailableType
	{
		// Members
		// structure for internal data available for use in Erl that are not sourced by output variables
		std::string DataTypeName; // general internal variable name registered, All uppercase
		std::string UniqueIDName; // unique id for internal var, All uppercase
		std::string Units; // registered units, used for reporting and checks.
		int PntrVarTypeUsed; // data type used: integer (PntrInteger) or real (PntrReal)
		Reference< Real64 > RealValue; // fortran POINTER to the REAL value that is being accessed
		Reference_int IntValue; // fortran POINTER to the Integer value that is being accessed

		// Default Constructor
		InternalVarsAvailableType() :
			PntrVarTypeUsed( 0 )
		{}

	};

	struct InternalVarsUsedType
	{
		// Members
		// structure for internal data that user has selected to use in Erl.
		std::string Name; // Erl variable name
		std::string InternalDataTypeName; // general internal variable name, All uppercase
		std::string UniqueIDName; // unique id for internal var, All uppercase
		bool CheckedOkay; // set to true once matched to available internal var
		int ErlVariableNum; // points to global Erl variable, matches Name
		int InternVarNum; // points to index match in EMSInternalVarsAvailable structure

		// Default Constructor
		InternalVarsUsedType() :
			CheckedOkay( false ),
			ErlVariableNum( 0 ),
			InternVarNum( 0 )
		{}

	};

	struct EMSActuatorAvailableType
	{
		// Members
		// structure for all the actuators available for use in Erl
		std::string ComponentTypeName; // general actuator name registered, All uppercase
		std::string UniqueIDName; // unique id for actuator, All uppercase
		std::string ControlTypeName; // control type id for actuator, All uppercase
		std::string Units; // control value units, used for reporting and checks.
		int PntrVarTypeUsed; // data type used: integer (PntrInteger), real (PntrReal)
		// or logical (PntrLogical)
		Reference_bool Actuated; // fortran POINTER to the logical value that signals EMS is actuating
		Reference< Real64 > RealValue; // fortran POINTER to the REAL value that is being actuated
		Reference_int IntValue; // fortran POINTER to the Integer value that is being actuated
		Reference_bool LogValue; // fortran POINTER to the Logical value that is being actuated

		// Default Constructor
		EMSActuatorAvailableType() :
			PntrVarTypeUsed( 0 )
		{}

	};

	struct ActuatorUsedType
	{
		// Members
		// structure for actuators user selected to use in Erl
		std::string Name; // Erl variable name
		std::string ComponentTypeName; // general actuator name, All uppercase
		std::string UniqueIDName; // unique id for actuator, All uppercase
		std::string ControlTypeName; // control type id for actuator, All uppercase
		bool CheckedOkay; // set to true once matched to available actuator
		int ErlVariableNum; // points to global Erl variable, matches Name
		int ActuatorVariableNum; // points to index match in EMSActuatorAvailable structure

		// Default Constructor
		ActuatorUsedType() :
			CheckedOkay( false ),
			ErlVariableNum( 0 ),
			ActuatorVariableNum( 0 )
		{}

	};

	struct EMSProgramCallManagementType
	{
		// Members
		// structure for Erl program calling managers
		std::string Name; // user defined name for calling manager
		int CallingPoint; // EMS Calling point for this manager, see parameters emsCallFrom*
		int NumErlPrograms; // count of total number of Erl programs called by this manager
		Array1D_int ErlProgramARR; // list of integer pointers to Erl programs used by this manager

		// Default Constructor
		EMSProgramCallManagementType() :
			CallingPoint( 0 ),
			NumErlPrograms( 0 )
		{}

	};

	struct ErlValueType
	{
		// Members
		// instance data structure for the values taken by Erl variables, nested structure in ErlVariable
		int Type; // value type, eg. ValueNumber,
		Real64 Number; // numeric value instance for Erl variable
		std::string String; // string data types in Erl (not used yet)
		int Variable; // Pointer to another Erl variable
		//  Might be good to change names to VariableNum and ExpressionNum just to be clear
		int Expression; // Pointer to another Erl expression (e.g. compound operators)
		bool TrendVariable; // true if Erl variable is really a trend variable
		int TrendVarPointer; // index to match in TrendVariable structure
		std::string Error; // holds error message string for reporting

		// Default Constructor
		ErlValueType() :
			Type( 0 ),
			Number( 0.0 ),
			Variable( 0 ),
			Expression( 0 ),
			TrendVariable( false ),
			TrendVarPointer( 0 )
		{}

		// Member Constructor
		ErlValueType(
			int const Type, // value type, eg. ValueNumber,
			Real64 const Number, // numeric value instance for Erl variable
			std::string const & String, // string data types in Erl (not used yet)
			int const Variable, // Pointer to another Erl variable
			int const Expression, // Pointer to another Erl expression (e.g. compound operators)
			bool const TrendVariable, // true if Erl variable is really a trend variable
			int const TrendVarPointer, // index to match in TrendVariable structure
			std::string const & Error // holds error message string for reporting
		) :
			Type( Type ),
			Number( Number ),
			String( String ),
			Variable( Variable ),
			Expression( Expression ),
			TrendVariable( TrendVariable ),
			TrendVarPointer( TrendVarPointer ),
			Error( Error )
		{}

	};

	struct ErlVariableType
	{
		// Members
		// structure for Erl variables
		std::string Name; // Erl Variable Name
		int StackNum; // 0 for global Erl variables, index in ErlStack structure if local
		ErlValueType Value; // values taken by Erl variables
		bool ReadOnly; // true if Erl variable is read-only
		bool SetByExternalInterface; // set to true if value is set by ExternalInterface

		// Default Constructor
		ErlVariableType() :
			StackNum( 0 ),
			ReadOnly( false ),
			SetByExternalInterface( false )
		{}

	};

	struct InstructionType
	{
		// Members
		// nested structure inside ErlStack that holds program instructions
		int LineNum; // Erl program line number reference
		int Keyword; // type of instruction for this line, e.g. KeywordSet, KeywordIf, etc
		int Argument1; // Index to a variable, function, expression, or stack
		int Argument2; // Index to a variable, function, expression, or stack

		// Default Constructor
		InstructionType() :
			LineNum( 0 ),
			Keyword( 0 ),
			Argument1( 0 ),
			Argument2( 0 )
		{}

	};

	struct ErlStackType // Stores Erl programs in a stack of statements/instructions
	{
		// Members
		std::string Name; // Erl program or subroutine name, user defined
		int NumLines; // count of lines in Erl program or subroutine
		Array1D_string Line; // string array holding lines of Erl code (for processing)
		int NumInstructions; // count of program instructions in stack
		Array1D< InstructionType > Instruction; // structure array of program instructions
		int NumErrors; // count of errors during stack parsing
		Array1D_string Error; // array of error messages from stack parsing

		// Default Constructor
		ErlStackType() :
			NumLines( 0 ),
			NumInstructions( 0 ),
			NumErrors( 0 )
		{}

	};

	struct ErlExpressionType
	{
		// Members
		int Operator; // indicates the type of operator or function 1..64
		int NumOperands; // count of operands in expression
		Array1D< ErlValueType > Operand; // holds Erl values for operands in expression

		// Default Constructor
		ErlExpressionType() :
			Operator( 0 ),
			NumOperands( 0 )
		{}

	};

	struct OperatorType
	{
		// Members
		// structure for operators and functions, used to look up information about each operator or function
		std::string Symbol; // string representation of operator or function (for reporting)
		int Code; // integer code 1..64, identifies operator or function
		int NumOperands; // count of operands or function arguments.

		// Default Constructor
		OperatorType() :
			Code( 0 ),
			NumOperands( 0 )
		{}

	};

	struct TrendVariableType
	{
		// Members
		std::string Name;
		int ErlVariablePointer; // the Erl variable being logged in trend
		int LogDepth; // number of timesteps back
		Array1D< Real64 > TrendValARR; // the main storage of trend data
		Array1D< Real64 > tempTrendARR; // temporary holder during push
		Array1D< Real64 > TimeARR; // hours back in time for trend points

		// Default Constructor
		TrendVariableType() :
			ErlVariablePointer( 0 ),
			LogDepth( 0 )
		{}

	};

	// Object Data
	extern Array1D< ErlVariableType > ErlVariable; // holds Erl variables in a structure array
	extern Array1D< ErlStackType > ErlStack; // holds Erl programs in separate "stacks"
	extern Array1D< ErlExpressionType > ErlExpression; // holds Erl expressions in structure array
	extern Array1D< OperatorType > PossibleOperators; // hard library of available operators and functions
	extern Array1D< TrendVariableType > TrendVariable; // holds Erl trend varialbes in a structure array
	extern Array1D< OutputVarSensorType > Sensor; // EMS:SENSOR objects used (from output variables)
	extern Array1D< EMSActuatorAvailableType > EMSActuatorAvailable; // actuators that could be used
	extern Array1D< ActuatorUsedType > EMSActuatorUsed; // actuators that are used
	extern Array1D< InternalVarsAvailableType > EMSInternalVarsAvailable; // internal data that could be used
	extern Array1D< InternalVarsUsedType > EMSInternalVarsUsed; // internal data that are used
	extern Array1D< EMSProgramCallManagementType > EMSProgramCallManager; // program calling managers
	extern ErlValueType Null; // special "null" Erl variable value instance
	extern ErlValueType False; // special "false" Erl variable value instance
	extern ErlValueType True; // special "True" Erl variable value instance, gets reset

	// EMS Actuator fast duplicate check lookup support
	typedef  std::tuple< std::string, std::string, std::string >  EMSActuatorKey;
	struct EMSActuatorKey_hash : public std::unary_function< EMSActuatorKey, std::size_t >
	{
		inline
		static
		void
		hash_combine( std::size_t & seed, std::string const & s )
		{
			std::hash< std::string > hasher;
			seed ^= hasher( s ) + 0x9e3779b9 + ( seed << 6 ) + ( seed >> 2 );
		}

		inline
		std::size_t
		operator ()( EMSActuatorKey const & key ) const
		{
			std::size_t seed( 0 );
			hash_combine( seed, std::get< 0 >( key ) );
			hash_combine( seed, std::get< 1 >( key ) );
			hash_combine( seed, std::get< 2 >( key ) );
			return seed;
		}
	};
	extern std::unordered_set< std::tuple< std::string, std::string, std::string >, EMSActuatorKey_hash > EMSActuator_lookup; // Fast duplicate lookup structure

	// Functions
	void
	clear_state();

	void
	ValidateEMSVariableName(
		std::string const & cModuleObject, // the current object name
		std::string const & cFieldValue, // the field value
		std::string const & cFieldName, // the current field name
		bool & errFlag, // true if errors found in this routine.
		bool & ErrorsFound // true if errors found in this routine.
	);

	void
	ValidateEMSProgramName(
		std::string const & cModuleObject, // the current object name
		std::string const & cFieldValue, // the field value
		std::string const & cFieldName, // the current field name
		std::string const & cSubType, // sub type = Program or Subroutine
		bool & errFlag, // true if errors found in this routine.
		bool & ErrorsFound // true if errors found in this routine.
	);

} // DataRuntimeLanguage

} // EnergyPlus

#endif
