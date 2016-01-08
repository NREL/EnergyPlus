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

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/char.functions.hh>
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/random.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Time_Date.hh>

// EnergyPlus Headers
#include <RuntimeLanguageProcessor.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSystemVariables.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace RuntimeLanguageProcessor {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   June 2006
	//       MODIFIED       Brent Griffith, May - August 2009
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// METHODOLOGY EMPLOYED:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::OutputFileDebug;
	using namespace DataRuntimeLanguage;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const MaxErrors( 20 );

	// keyword parameters for types of Erl statements
	int const KeywordNone( 0 ); // statement type not set
	int const KeywordReturn( 1 ); // Return statement, as in leave program
	int const KeywordGoto( 2 ); // Goto statement, used in parsing to manage IF-ElseIf-Else-EndIf and nesting
	int const KeywordSet( 3 ); // Set statement, as in assign RHS to LHS
	int const KeywordRun( 4 ); // Run statement, used to call a subroutine from a main program
	int const KeywordIf( 5 ); // If statement, begins an IF-ElseIf-Else-EndIf logic block
	int const KeywordElseIf( 6 ); // ElseIf statement, begins an ElseIf block
	int const KeywordElse( 7 ); // Else statement, begins an Else block
	int const KeywordEndIf( 8 ); // EndIf statement, terminates an IF-ElseIf-Else-EndIf logic block
	int const KeywordWhile( 9 ); // While statement, begins a While block
	int const KeywordEndWhile( 10 ); // EndWhile statement, terminates a While block

	// token type parameters for Erl code parsing
	int const TokenNumber( 1 ); // matches the ValueNumber
	int const TokenVariable( 4 ); // matches the ValueVariable
	int const TokenExpression( 5 ); // matches the ValueExpression
	int const TokenOperator( 7 ); // includes basic operators and built-in functions.

	int const TokenParenthesis( 9 ); // parenthesis token

	int const ParenthesisLeft( 10 ); // indicates left side parenthesis found in parsing
	int const ParenthesisRight( 11 ); // indicates right side parenthesis found in parsing

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:

	bool GetInput( true );
	bool InitializeOnce( true );
	bool MyEnvrnFlag( true );
	bool AlreadyDidOnce( false );

	// index pointer references to dynamic built-in variables
	int NullVariableNum( 0 );
	int FalseVariableNum( 0 );
	int TrueVariableNum( 0 );
	int OffVariableNum( 0 );
	int OnVariableNum( 0 );
	int PiVariableNum( 0 );
	Array1D_int CurveIndexVariableNums;
	Array1D_int ConstructionIndexVariableNums;
	int YearVariableNum( 0 );
	int MonthVariableNum( 0 );
	int DayOfMonthVariableNum( 0 );
	int DayOfWeekVariableNum( 0 );
	int DayOfYearVariableNum( 0 );
	int HourVariableNum( 0 );
	int MinuteVariableNum( 0 );
	int HolidayVariableNum( 0 );
	int DSTVariableNum( 0 );
	int CurrentTimeVariableNum( 0 );
	int SunIsUpVariableNum( 0 );
	int IsRainingVariableNum( 0 );
	int SystemTimeStepVariableNum( 0 );
	int ZoneTimeStepVariableNum( 0 );
	int CurrentEnvironmentPeriodNum( 0 );
	int ActualDateAndTimeNum( 0 );
	int ActualTimeNum( 0 );
	int WarmUpFlagNum( 0 );

	static gio::Fmt fmtLD( "*" );
	static gio::Fmt fmtA( "(A)" );

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< RuntimeReportVarType > RuntimeReportVar;

	// MODULE SUBROUTINES:

	// Functions
	void
	clear_state(){
		GetInput =  true ;
		InitializeOnce = true ;
		MyEnvrnFlag = true ;
		AlreadyDidOnce = false;

		NullVariableNum = 0;
		FalseVariableNum = 0;
		TrueVariableNum = 0;
		OffVariableNum = 0;
		OnVariableNum = 0;
		PiVariableNum = 0;
		CurveIndexVariableNums.deallocate();
		ConstructionIndexVariableNums.deallocate();
		YearVariableNum = 0;
		MonthVariableNum = 0;
		DayOfMonthVariableNum = 0;
		DayOfWeekVariableNum = 0;
		DayOfYearVariableNum = 0;
		HourVariableNum = 0;
		MinuteVariableNum = 0;
		HolidayVariableNum = 0;
		DSTVariableNum = 0;
		CurrentTimeVariableNum = 0;
		SunIsUpVariableNum = 0;
		IsRainingVariableNum = 0;
		SystemTimeStepVariableNum = 0;
		ZoneTimeStepVariableNum = 0;
		CurrentEnvironmentPeriodNum = 0;
		ActualDateAndTimeNum = 0;
		ActualTimeNum = 0;
		WarmUpFlagNum = 0;
	
	}

	void
	InitializeRuntimeLanguage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       Rui Zhang February 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// One time run.  Must be run BEFORE anything gets parsed.

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::HourOfDay;
		using DataGlobals::CurrentTime;
		using DataGlobals::TimeStepZone;
		using DataGlobals::WarmupFlag;
		using DataEnvironment::Year;
		using DataEnvironment::Month;
		using DataEnvironment::DayOfMonth;
		using DataEnvironment::DayOfWeek;
		using DataEnvironment::DayOfYear;
		using DataEnvironment::SunIsUp;
		using DataEnvironment::IsRain;
		using DataEnvironment::HolidayIndex;
		using DataEnvironment::DSTIndicator;
		using DataEnvironment::CurEnvirNum;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Real64 tmpCurrentTime( 0.0 );
		static Real64 tmpMinutes( 0.0 );
		static Real64 tmpHours( 0.0 );
		static Real64 tmpCurEnvirNum( 0.0 );
		Array1D_int datevalues( 8 );
		//value(1)   Current year
		//value(2)   Current month
		//value(3)   Current day
		//value(4)   Time difference with respect to UTC in minutes (0-59)
		//value(5)   Hour of the day (0-23)
		//value(6)   Minutes (0-59)
		//value(7)   Seconds (0-59)
		//value(8)   Milliseconds (0-999)

		std::string datestring; // supposedly returns blank when no date available.

		// FLOW:
		if ( InitializeOnce ) {

			False = SetErlValueNumber( 0.0 );
			True = SetErlValueNumber( 1.0 );

			// Create constant built-in variables
			NullVariableNum = NewEMSVariable( "NULL", 0 );
			ErlVariable( NullVariableNum ).Value.Type = ValueNull;
			FalseVariableNum = NewEMSVariable( "FALSE", 0, False );
			TrueVariableNum = NewEMSVariable( "TRUE", 0, True );
			OffVariableNum = NewEMSVariable( "OFF", 0, False );
			OnVariableNum = NewEMSVariable( "ON", 0, True );
			PiVariableNum = NewEMSVariable( "PI", 0, SetErlValueNumber( Pi ) );

			// Create dynamic built-in variables
			YearVariableNum = NewEMSVariable( "YEAR", 0 );
			MonthVariableNum = NewEMSVariable( "MONTH", 0 );
			DayOfMonthVariableNum = NewEMSVariable( "DAYOFMONTH", 0 ); // 'DAYOFMONTH'?
			DayOfWeekVariableNum = NewEMSVariable( "DAYOFWEEK", 0 );
			DayOfYearVariableNum = NewEMSVariable( "DAYOFYEAR", 0 );
			HourVariableNum = NewEMSVariable( "HOUR", 0 );
			MinuteVariableNum = NewEMSVariable( "MINUTE", 0 );
			HolidayVariableNum = NewEMSVariable( "HOLIDAY", 0 );
			DSTVariableNum = NewEMSVariable( "DAYLIGHTSAVINGS", 0 );
			CurrentTimeVariableNum = NewEMSVariable( "CURRENTTIME", 0 );
			SunIsUpVariableNum = NewEMSVariable( "SUNISUP", 0 );
			IsRainingVariableNum = NewEMSVariable( "ISRAINING", 0 );
			SystemTimeStepVariableNum = NewEMSVariable( "SYSTEMTIMESTEP", 0 );
			ZoneTimeStepVariableNum = NewEMSVariable( "ZONETIMESTEP", 0 );
			ErlVariable( ZoneTimeStepVariableNum ).Value = SetErlValueNumber( TimeStepZone );
			CurrentEnvironmentPeriodNum = NewEMSVariable( "CURRENTENVIRONMENT", 0 );
			ActualDateAndTimeNum = NewEMSVariable( "ACTUALDATEANDTIME", 0 );
			ActualTimeNum = NewEMSVariable( "ACTUALTIME", 0 );
			WarmUpFlagNum = NewEMSVariable( "WARMUPFLAG", 0 );

			GetRuntimeLanguageUserInput(); // Load and parse all runtime language objects

			date_and_time( datestring, _, _, datevalues );
			if ( datestring != "" ) {
				ErlVariable( ActualDateAndTimeNum ).Value = SetErlValueNumber( double( sum( datevalues ) ) );
				//datevalues(1)+datevalues(2)+datevalues(3)+  &
				//datevalues(5)+datevalues(6)+datevalues(7)+datevalues(8)
				ErlVariable( ActualTimeNum ).Value = SetErlValueNumber( double( sum( datevalues( {5,8} ) ) ) );
				//datevalues(5)+datevalues(6)+datevalues(7)+datevalues(8)
				//    ELSE
				//      ErlVariable(ActualDateAndTimeNum)%Value  = SetErlValueNumber(REAL(RANDOM_NUMBER(X=509),r64))
				//      ErlVariable(ActualTimeNum)%Value  = SetErlValueNumber(REAL(RANDOM_NUMBER(X=400),r64))
			}

			InitializeOnce = false;
		}

		// Update built-in variables
		ErlVariable( YearVariableNum ).Value = SetErlValueNumber( double( Year ) );
		ErlVariable( MonthVariableNum ).Value = SetErlValueNumber( double( Month ) );
		ErlVariable( DayOfMonthVariableNum ).Value = SetErlValueNumber( double( DayOfMonth ) );
		ErlVariable( DayOfWeekVariableNum ).Value = SetErlValueNumber( double( DayOfWeek ) );
		ErlVariable( DayOfYearVariableNum ).Value = SetErlValueNumber( double( DayOfYear ) );

		ErlVariable( DSTVariableNum ).Value = SetErlValueNumber( double( DSTIndicator ) );
		//DSTadjust = REAL(DSTIndicator, r64)
		tmpHours = double( HourOfDay - 1 ); // no, just stay on 0..23+ DSTadjust ! offset by 1 and daylight savings time
		ErlVariable( HourVariableNum ).Value = SetErlValueNumber( tmpHours );

		if ( TimeStepSys < TimeStepZone ) {
			//CurrentTime is for end of zone timestep, need to account for system timestep
			tmpCurrentTime = CurrentTime - TimeStepZone + SysTimeElapsed + TimeStepSys;
		} else {
			tmpCurrentTime = CurrentTime;
		}
		ErlVariable( CurrentTimeVariableNum ).Value = SetErlValueNumber( tmpCurrentTime );
		tmpMinutes = ( ( tmpCurrentTime - double( HourOfDay - 1 ) ) * 60.0 ); // -1.0 // off by 1
		ErlVariable( MinuteVariableNum ).Value = SetErlValueNumber( tmpMinutes );
		ErlVariable( HolidayVariableNum ).Value = SetErlValueNumber( double( HolidayIndex ) );
		if ( SunIsUp ) {
			ErlVariable( SunIsUpVariableNum ).Value = SetErlValueNumber( 1.0 );
		} else {
			ErlVariable( SunIsUpVariableNum ).Value = SetErlValueNumber( 0.0 );
		}
		if ( IsRain ) {
			ErlVariable( IsRainingVariableNum ).Value = SetErlValueNumber( 1.0 );
		} else {
			ErlVariable( IsRainingVariableNum ).Value = SetErlValueNumber( 0.0 );
		}
		ErlVariable( SystemTimeStepVariableNum ).Value = SetErlValueNumber( TimeStepSys );

		tmpCurEnvirNum = double( CurEnvirNum );
		ErlVariable( CurrentEnvironmentPeriodNum ).Value = SetErlValueNumber( tmpCurEnvirNum );
		if ( WarmupFlag ) {
			ErlVariable( WarmUpFlagNum ).Value = SetErlValueNumber( 1.0 );
		} else {
			ErlVariable( WarmUpFlagNum ).Value = SetErlValueNumber( 0.0 );
		}

	}

	void
	BeginEnvrnInitializeRuntimeLanguage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2010
		//       MODIFIED       B. Griffith, added Sensor initialation
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// re initialize Erl for new simulation environment period

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputProcessor::SetInternalVariableValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ActuatorUsedLoop;
		int EMSActuatorVariableNum;
		int ErlVariableNum;
		int TrendVarNum;
		int SensorNum;
		int TrendDepth;
		int loop;
		bool CycleThisVariable;

		//reinitialize state of Erl variable values to zero, this gets sensors and internal variables used
		for ( ErlVariableNum = 1; ErlVariableNum <= NumErlVariables; ++ErlVariableNum ) {
			//but skip constant built-in variables so don't overwrite them
			if ( ErlVariableNum == NullVariableNum ) continue;
			if ( ErlVariableNum == FalseVariableNum ) continue;
			if ( ErlVariableNum == TrueVariableNum ) continue;
			if ( ErlVariableNum == OffVariableNum ) continue;
			if ( ErlVariableNum == OnVariableNum ) continue;
			if ( ErlVariableNum == PiVariableNum ) continue;
			if ( ErlVariableNum == ZoneTimeStepVariableNum ) continue;
			if ( ErlVariableNum == ActualDateAndTimeNum ) continue;
			if ( ErlVariableNum == ActualTimeNum ) continue;

			// need to preserve curve index variables
			CycleThisVariable = false;
			for ( loop = 1; loop <= NumEMSCurveIndices; ++loop ) {
				if ( ErlVariableNum == CurveIndexVariableNums( loop ) ) CycleThisVariable = true;
			}
			if ( CycleThisVariable ) continue;
			CycleThisVariable = false;
			for ( loop = 1; loop <= NumEMSConstructionIndices; ++loop ) {
				if ( ErlVariableNum == ConstructionIndexVariableNums( loop ) ) CycleThisVariable = true;
			}
			if ( CycleThisVariable ) continue;

			ErlVariable( ErlVariableNum ).Value = SetErlValueNumber( 0.0, ErlVariable( ErlVariableNum ).Value );

		}
		//reinitialize state of actuators
		for ( ActuatorUsedLoop = 1; ActuatorUsedLoop <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed; ++ActuatorUsedLoop ) {
			EMSActuatorVariableNum = EMSActuatorUsed( ActuatorUsedLoop ).ActuatorVariableNum;
			ErlVariableNum = EMSActuatorUsed( ActuatorUsedLoop ).ErlVariableNum;
			ErlVariable( ErlVariableNum ).Value.Type = ValueNull;
			EMSActuatorAvailable( EMSActuatorVariableNum ).Actuated = false;
			{ auto const SELECT_CASE_var( EMSActuatorAvailable( EMSActuatorVariableNum ).PntrVarTypeUsed );
			if ( SELECT_CASE_var == PntrReal ) {
				EMSActuatorAvailable( EMSActuatorVariableNum ).RealValue = 0.0;
			} else if ( SELECT_CASE_var == PntrInteger ) {
				EMSActuatorAvailable( EMSActuatorVariableNum ).IntValue = 0;
			} else if ( SELECT_CASE_var == PntrLogical ) {
				EMSActuatorAvailable( EMSActuatorVariableNum ).LogValue = false;
			}}
		}

		//reinitialize trend variables so old data are purged
		for ( TrendVarNum = 1; TrendVarNum <= NumErlTrendVariables; ++TrendVarNum ) {
			TrendDepth = TrendVariable( TrendVarNum ).LogDepth;
			TrendVariable( TrendVarNum ).TrendValARR( {1,TrendDepth} ) = 0.0;
		}

		// reinitilize sensors
		for ( SensorNum = 1; SensorNum <= NumSensors; ++SensorNum ) {
			SetInternalVariableValue( Sensor( SensorNum ).Type, Sensor( SensorNum ).Index, 0.0, 0 );
		}

	}

	void
	ParseStack( int const StackNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       Brent Griffith June 2009
		//                      Brent Griffith March 2012, add WHILE loops
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Parsing a block of text creates a program stack in DataRuntimeLanguage.
		// This routine only executes once for each Erl program.

		// METHODOLOGY EMPLOYED:
		// Loop over each line of Erl code and parse based on statement keyword

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::ProcessNumber;
		using InputProcessor::FindItemInList;
		using DataSystemVariables::DeveloperFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const IfDepthAllowed( 5 ); // depth of IF block nesting
		int const ELSEIFLengthAllowed( 200 ); // number of ELSEIFs allowed
		int const WhileDepthAllowed( 1 ); // depth of While block nesting

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LineNum;
		int StackNum2;
		std::string::size_type Pos;
		int ExpressionNum;
		int VariableNum;
		std::string Line; // local copy of a single line of Erl program code
		std::string Keyword; // local copy of statement keyword parsed from line (Run, Set, If, etc)
		std::string Remainder; // local copy of what is left for text in the line after keyword
		std::string Expression;
		std::string Variable;
		int NestedIfDepth; // indicates depth into If statement,
		int NestedWhileDepth; // indicates depth into While statement
		int InstructionNum;
		int InstructionNum2;
		int GotoNum;
		Array1D_int SavedIfInstructionNum( IfDepthAllowed ); // index is depth of If statements
		Array2D_int SavedGotoInstructionNum( ELSEIFLengthAllowed, IfDepthAllowed );
		Array1D_int NumGotos( IfDepthAllowed ); // index is depth of If statements,
		int SavedWhileInstructionNum;
		int SavedWhileExpressionNum;
		int NumWhileGotos;
		Array1D_bool ReadyForElse( IfDepthAllowed );
		Array1D_bool ReadyForEndif( IfDepthAllowed );

		//  CHARACTER(len=2*MaxNameLength), DIMENSION(:), ALLOCATABLE :: DummyError

		// FLOW:
		LineNum = 1;
		NestedIfDepth = 0;
		ReadyForElse = false;
		ReadyForEndif = false;
		SavedIfInstructionNum = 0;
		SavedGotoInstructionNum = 0;
		NumGotos = 0;
		NestedWhileDepth = 0;
		SavedWhileInstructionNum = 0;
		SavedWhileExpressionNum = 0;
		NumWhileGotos = 0;

		while ( LineNum <= ErlStack( StackNum ).NumLines ) {

			Line = stripped( ErlStack( StackNum ).Line( LineNum ) );
			if ( len( Line ) == 0 ) {
				++LineNum;
				continue; // Blank lines can be skipped
			}

			Pos = scan( Line, ' ' );
			if ( Pos == std::string::npos ) {
				Pos = len( Line );
				Remainder.clear();
			} else {
				Remainder = stripped( Line.substr( Pos + 1 ) );
			}
			//    Keyword = MakeUPPERCase(Line(1:Pos-1))
			Keyword = Line.substr( 0, Pos );

			{ auto const SELECT_CASE_var( Keyword );

			if ( SELECT_CASE_var == "RETURN" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "RETURN \"" + Line + "\"";
				if ( Remainder.empty() ) {
					InstructionNum = AddInstruction( StackNum, LineNum, KeywordReturn );
				} else {
					ParseExpression( Remainder, StackNum, ExpressionNum, Line );
					InstructionNum = AddInstruction( StackNum, LineNum, KeywordReturn, ExpressionNum );
				}

			} else if ( SELECT_CASE_var == "SET" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "SET \"" + Line + "\"";
				Pos = scan( Remainder, '=' );
				if ( Pos == std::string::npos ) {
					AddError( StackNum, LineNum, "Equal sign missing for the SET instruction." );
				} else if ( Pos == 0 ) {
					AddError( StackNum, LineNum, "Variable name missing for the SET instruction." );
				} else {
					Variable = stripped( Remainder.substr( 0, Pos ) ); // VariableName would be more expressive
					VariableNum = NewEMSVariable( Variable, StackNum );
					// Check for invalid variable name

					if ( Pos + 1 < Remainder.length() ) {
						Expression = stripped( Remainder.substr( Pos + 1 ) );
					} else {
						Expression.clear();
					}
					if ( Expression.empty() ) {
						AddError( StackNum, LineNum, "Expression missing for the SET instruction." );
					} else {
						ParseExpression( Expression, StackNum, ExpressionNum, Line );
						InstructionNum = AddInstruction( StackNum, LineNum, KeywordSet, VariableNum, ExpressionNum );
					}
				}

			} else if ( SELECT_CASE_var == "RUN" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "RUN \"" + Line + "\"";
				if ( Remainder.empty() ) {
					AddError( StackNum, LineNum, "Program or Subroutine name missing for the RUN instruction." );
				} else {
					Pos = scan( Remainder, ' ' );
					if ( Pos == std::string::npos ) Pos = Remainder.length();
					Variable = MakeUPPERCase( stripped( Remainder.substr( 0, Pos ) ) ); // really the subroutine, or reference to instruction set
					StackNum2 = FindItemInList( Variable, ErlStack );
					if ( StackNum2 == 0 ) {
						AddError( StackNum, LineNum, "Program or Subroutine name [" + Variable + "] not found for the RUN instruction." );
					} else {
						InstructionNum = AddInstruction( StackNum, LineNum, KeywordRun, StackNum2 );
					}
				}

			} else if ( SELECT_CASE_var == "IF" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "IF \"" + Line + "\"";
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtLD ) << "NestedIf=" << NestedIfDepth;
				if ( Remainder.empty() ) {
					AddError( StackNum, LineNum, "Expression missing for the IF instruction." );
					ExpressionNum = 0;
				} else {
					Expression = stripped( Remainder );
					ParseExpression( Expression, StackNum, ExpressionNum, Line );
				}

				++NestedIfDepth;
				ReadyForElse( NestedIfDepth ) = true;
				ReadyForEndif( NestedIfDepth ) = true;
				if ( NestedIfDepth > IfDepthAllowed ) {
					AddError( StackNum, LineNum, "Detected IF nested deeper than is allowed; need to terminate an earlier IF instruction." );
					break;
				} else {
					InstructionNum = AddInstruction( StackNum, LineNum, KeywordIf, ExpressionNum ); // Arg2 added at next ELSEIF, ELSE, ENDIF
					SavedIfInstructionNum( NestedIfDepth ) = InstructionNum;
				}

			} else if ( SELECT_CASE_var == "ELSEIF" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ELSEIF \"" + Line + "\"";
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtLD ) << "NestedIf=" << NestedIfDepth;
				if ( NestedIfDepth == 0 ) {
					AddError( StackNum, LineNum, "Starting IF instruction missing for the ELSEIF instruction." );
					break; // Getting strange error on DEALLOCATE for the next instruction that I try to add, so doing EXIT here
				}

				// Complete the preceding block with a GOTO instruction
				InstructionNum = AddInstruction( StackNum, 0, KeywordGoto ); // Arg2 is added at the ENDIF
				++NumGotos( NestedIfDepth );
				if ( NumGotos( NestedIfDepth ) > ELSEIFLengthAllowed ) {
					AddError( StackNum, LineNum, "Detected ELSEIF series that is longer than allowed; terminate earlier IF instruction." );
					break;
				} else {
					SavedGotoInstructionNum( NumGotos( NestedIfDepth ), NestedIfDepth ) = InstructionNum;
				}

				if ( Remainder.empty() ) {
					AddError( StackNum, LineNum, "Expression missing for the ELSEIF instruction." );
					ExpressionNum = 0;
				} else {
					Expression = stripped( Remainder );
					ParseExpression( Expression, StackNum, ExpressionNum, Line );
				}

				InstructionNum = AddInstruction( StackNum, LineNum, KeywordIf, ExpressionNum ); // Arg2 added at next ELSEIF, ELSE, ENDIF
				ErlStack( StackNum ).Instruction( SavedIfInstructionNum( NestedIfDepth ) ).Argument2 = InstructionNum;
				SavedIfInstructionNum( NestedIfDepth ) = InstructionNum;

			} else if ( SELECT_CASE_var == "ELSE" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ELSE \"" + Line + "\"";
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtLD ) << "NestedIf=" << NestedIfDepth;
				if ( NestedIfDepth == 0 ) {
					AddError( StackNum, LineNum, "Starting IF instruction missing for the ELSE instruction." );
					break; // Getting strange error on DEALLOCATE for the next instruction that I try to add, so doing EXIT here
				}
				if ( ! ReadyForElse( NestedIfDepth ) ) {
					AddError( StackNum, LineNum, "ELSE statement without corresponding IF statement." );
				}
				ReadyForElse( NestedIfDepth ) = false;

				// Complete the preceding block with a GOTO instruction
				InstructionNum = AddInstruction( StackNum, 0, KeywordGoto ); // Arg2 is added at the ENDIF
				++NumGotos( NestedIfDepth );
				if ( NumGotos( NestedIfDepth ) > ELSEIFLengthAllowed ) {
					AddError( StackNum, LineNum, "Detected ELSEIF-ELSE series that is longer than allowed." );
					break;
				} else {
					SavedGotoInstructionNum( NumGotos( NestedIfDepth ), NestedIfDepth ) = InstructionNum;
				}

				if ( ! Remainder.empty() ) {
					AddError( StackNum, LineNum, "Nothing is allowed to follow the ELSE instruction." );
				}

				InstructionNum = AddInstruction( StackNum, LineNum, KeywordElse ); // can make this into a KeywordIf?
				ErlStack( StackNum ).Instruction( SavedIfInstructionNum( NestedIfDepth ) ).Argument2 = InstructionNum;
				SavedIfInstructionNum( NestedIfDepth ) = InstructionNum;

			} else if ( SELECT_CASE_var == "ENDIF" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ENDIF \"" + Line + "\"";
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtLD ) << "NestedIf=" << NestedIfDepth;
				if ( NestedIfDepth == 0 ) {
					AddError( StackNum, LineNum, "Starting IF instruction missing for the ENDIF instruction." );
					break; // PE Getting strange error on DEALLOCATE for the next instruction that I try to add, so doing EXIT here
				}

				if ( ! ReadyForEndif( NestedIfDepth ) ) {
					AddError( StackNum, LineNum, "ENDIF statement without corresponding IF stetement." );
				}
				ReadyForEndif( NestedIfDepth ) = false;
				ReadyForElse( NestedIfDepth ) = false;

				if ( ! Remainder.empty() ) {
					AddError( StackNum, LineNum, "Nothing is allowed to follow the ENDIF instruction." );
				}

				InstructionNum = AddInstruction( StackNum, LineNum, KeywordEndIf );
				ErlStack( StackNum ).Instruction( SavedIfInstructionNum( NestedIfDepth ) ).Argument2 = InstructionNum;

				// Go back and complete all of the GOTOs that terminate each IF and ELSEIF block
				for ( GotoNum = 1; GotoNum <= NumGotos( NestedIfDepth ); ++GotoNum ) {
					InstructionNum2 = SavedGotoInstructionNum( GotoNum, NestedIfDepth );
					ErlStack( StackNum ).Instruction( InstructionNum2 ).Argument1 = InstructionNum;
					SavedGotoInstructionNum( GotoNum, NestedIfDepth ) = 0;
				}

				NumGotos( NestedIfDepth ) = 0;
				SavedIfInstructionNum( NestedIfDepth ) = 0;
				--NestedIfDepth;

			} else if ( SELECT_CASE_var == "WHILE" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "WHILE \"" + Line + "\"";
				if ( Remainder.empty() ) {
					AddError( StackNum, LineNum, "Expression missing for the WHILE instruction." );
					ExpressionNum = 0;
				} else {
					Expression = stripped( Remainder );
					ParseExpression( Expression, StackNum, ExpressionNum, Line );
				}

				++NestedWhileDepth;
				if ( NestedWhileDepth > WhileDepthAllowed ) {
					AddError( StackNum, LineNum, "Detected WHILE nested deeper than is allowed; need to terminate an earlier WHILE instruction." );
					break;
				} else {
					InstructionNum = AddInstruction( StackNum, LineNum, KeywordWhile, ExpressionNum );
					SavedWhileInstructionNum = InstructionNum;
					SavedWhileExpressionNum = ExpressionNum;
				}

			} else if ( SELECT_CASE_var == "ENDWHILE" ) {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ENDWHILE \"" + Line + "\"";
				if ( NestedWhileDepth == 0 ) {
					AddError( StackNum, LineNum, "Starting WHILE instruction missing for the ENDWHILE instruction." );
					break;
				}
				if ( ! Remainder.empty() ) {
					AddError( StackNum, LineNum, "Nothing is allowed to follow the ENDWHILE instruction." );
				}

				InstructionNum = AddInstruction( StackNum, LineNum, KeywordEndWhile );
				ErlStack( StackNum ).Instruction( SavedWhileInstructionNum ).Argument2 = InstructionNum;
				ErlStack( StackNum ).Instruction( InstructionNum ).Argument1 = SavedWhileExpressionNum;
				ErlStack( StackNum ).Instruction( InstructionNum ).Argument2 = SavedWhileInstructionNum;

				NestedWhileDepth = 0;
				SavedWhileInstructionNum = 0;
				SavedWhileExpressionNum = 0;

			} else {
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ERROR \"" + Line + "\"";
				AddError( StackNum, LineNum, "Unknown keyword [" + Keyword + "]." );

			}}

			++LineNum;
		} // LineNum

		if ( NestedIfDepth == 1 ) {
			AddError( StackNum, 0, "Missing an ENDIF instruction needed to terminate an earlier IF instruction." );
		} else if ( NestedIfDepth > 1 ) {
			AddError( StackNum, 0, "Missing " + IntegerToString( NestedIfDepth ) + " ENDIF instructions needed to terminate earlier IF instructions." );
		}

		//  ALLOCATE(DummyError(ErlStack(StackNum)%NumErrors))
		//  DummyError = ErlStack(StackNum)%Error

	}

	int
	AddInstruction(
		int const StackNum,
		int const LineNum,
		int const Keyword,
		Optional_int_const Argument1, // Erl variable index
		Optional_int_const Argument2
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Adds an instruction to a stack.

		// METHODOLOGY EMPLOYED:

		// Return value
		int InstructionNum;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data
		ErlStackType TempStack;

		// FLOW:
		if ( ErlStack( StackNum ).NumInstructions == 0 ) {
			ErlStack( StackNum ).Instruction.allocate( 1 );
			ErlStack( StackNum ).NumInstructions = 1;
		} else {
			TempStack = ErlStack( StackNum );
			ErlStack( StackNum ).Instruction.deallocate();
			ErlStack( StackNum ).Instruction.allocate( ErlStack( StackNum ).NumInstructions + 1 );
			ErlStack( StackNum ).Instruction( {1,ErlStack( StackNum ).NumInstructions} ) = TempStack.Instruction( {1,ErlStack( StackNum ).NumInstructions} );
			++ErlStack( StackNum ).NumInstructions;
		}

		InstructionNum = ErlStack( StackNum ).NumInstructions;
		ErlStack( StackNum ).Instruction( InstructionNum ).LineNum = LineNum;
		ErlStack( StackNum ).Instruction( InstructionNum ).Keyword = Keyword;

		if ( present( Argument1 ) ) ErlStack( StackNum ).Instruction( InstructionNum ).Argument1 = Argument1;
		if ( present( Argument2 ) ) ErlStack( StackNum ).Instruction( InstructionNum ).Argument2 = Argument2;

		return InstructionNum;

	}

	void
	AddError(
		int const StackNum, // index pointer to location in ErlStack structure
		int const LineNum, // Erl program line number
		std::string const & Error // error message to be added to ErlStack
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Adds an error message to a stack.

		// METHODOLOGY EMPLOYED:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ErrorNum; // local count of errors for this ErlStack

		// Object Data
		ErlStackType TempStack; // temporary copy of single ErlStack

		// FLOW:
		if ( ErlStack( StackNum ).NumErrors == 0 ) {
			ErlStack( StackNum ).Error.allocate( 1 );
			ErlStack( StackNum ).NumErrors = 1;
		} else {
			TempStack = ErlStack( StackNum );
			ErlStack( StackNum ).Error.deallocate();
			ErlStack( StackNum ).Error.allocate( ErlStack( StackNum ).NumErrors + 1 );
			ErlStack( StackNum ).Error( {1,ErlStack( StackNum ).NumErrors} ) = TempStack.Error( {1,ErlStack( StackNum ).NumErrors} );
			++ErlStack( StackNum ).NumErrors;
		}

		ErrorNum = ErlStack( StackNum ).NumErrors;
		if ( LineNum > 0 ) {
			ErlStack( StackNum ).Error( ErrorNum ) = "Line " + IntegerToString( LineNum ) + ":  " + Error + " \"" + ErlStack( StackNum ).Line( LineNum ) + "\"";
		} else {
			ErlStack( StackNum ).Error( ErrorNum ) = Error;
		}

	}

	ErlValueType
	EvaluateStack( int const StackNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       Brent Griffith, May 2009
		//                      Brent Griffith, March 2012, add While loop support
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Runs a stack with the interpreter.

		// METHODOLOGY EMPLOYED:
		// Using/Aliasing

		// Return value
		ErlValueType ReturnValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InstructionNum;
		int InstructionNum2;
		int ExpressionNum;
		Real64 ReturnValueActual; // for testing
		static int VariableNum;
		int WhileLoopExitCounter; // to avoid infinite loop in While loop

		WhileLoopExitCounter = 0;
		ReturnValue.Type = ValueNumber;
		ReturnValue.Number = 0.0;

		InstructionNum = 1;
		while ( InstructionNum <= ErlStack( StackNum ).NumInstructions ) {

			{ auto const SELECT_CASE_var( ErlStack( StackNum ).Instruction( InstructionNum ).Keyword );

			if ( SELECT_CASE_var == KeywordNone ) {
				// There probably shouldn't be any of these

			} else if ( SELECT_CASE_var == KeywordReturn ) {
				if ( ErlStack( StackNum ).Instruction( InstructionNum ).Argument1 > 0 ) ReturnValue = EvaluateExpression( ErlStack( StackNum ).Instruction( InstructionNum ).Argument1 );

				WriteTrace( StackNum, InstructionNum, ReturnValue );
				break; // RETURN always terminates an instruction stack

			} else if ( SELECT_CASE_var == KeywordSet ) {

				ReturnValue = EvaluateExpression( ErlStack( StackNum ).Instruction( InstructionNum ).Argument2 );
				VariableNum = ErlStack( StackNum ).Instruction( InstructionNum ).Argument1;
				if ( ( ! ErlVariable( VariableNum ).ReadOnly ) && ( ! ErlVariable( VariableNum ).Value.TrendVariable ) ) {
					ErlVariable( VariableNum ).Value = ReturnValue;
				} else if ( ErlVariable( VariableNum ).Value.TrendVariable ) {
					ErlVariable( VariableNum ).Value.Number = ReturnValue.Number;
					ErlVariable( VariableNum ).Value.Error = ReturnValue.Error;
				}

				WriteTrace( StackNum, InstructionNum, ReturnValue );

			} else if ( SELECT_CASE_var == KeywordRun ) {
				ReturnValue.Type = ValueString;
				ReturnValue.String = "";
				WriteTrace( StackNum, InstructionNum, ReturnValue );
				ReturnValue = EvaluateStack( ErlStack( StackNum ).Instruction( InstructionNum ).Argument1 );

			} else if ( ( SELECT_CASE_var == KeywordIf ) || ( SELECT_CASE_var == KeywordElse ) ) { // same???
				ExpressionNum = ErlStack( StackNum ).Instruction( InstructionNum ).Argument1;
				InstructionNum2 = ErlStack( StackNum ).Instruction( InstructionNum ).Argument2;

				if ( ExpressionNum > 0 ) { // could be 0 if this was an ELSE
					ReturnValue = EvaluateExpression( ExpressionNum );
					WriteTrace( StackNum, InstructionNum, ReturnValue );
					if ( ReturnValue.Number == 0.0 ) { //  This is the FALSE case
						// Eventually should handle strings and arrays too
						InstructionNum = InstructionNum2;
						continue;
					}
				} else {
					// KeywordELSE  -- kind of a kludge
					ReturnValue.Type = ValueNumber;
					ReturnValue.Number = 1.0;
					WriteTrace( StackNum, InstructionNum, ReturnValue );
				}

			} else if ( SELECT_CASE_var == KeywordGoto ) {
				InstructionNum = ErlStack( StackNum ).Instruction( InstructionNum ).Argument1;

				// For debug purposes only...
				ReturnValue.Type = ValueString;
				ReturnValue.String = ""; //IntegerToString(InstructionNum)

				continue;
				// PE if this ever went out of bounds, would the DO loop save it?  or need check here?

			} else if ( SELECT_CASE_var == KeywordEndIf ) {
				ReturnValue.Type = ValueString;
				ReturnValue.String = "";
				WriteTrace( StackNum, InstructionNum, ReturnValue );

			} else if ( SELECT_CASE_var == KeywordWhile ) {
				// evaluate expresssion at while, skip to past endwhile if not true
				ExpressionNum = ErlStack( StackNum ).Instruction( InstructionNum ).Argument1;
				InstructionNum2 = ErlStack( StackNum ).Instruction( InstructionNum ).Argument2;
				ReturnValue = EvaluateExpression( ExpressionNum );
				WriteTrace( StackNum, InstructionNum, ReturnValue );
				if ( ReturnValue.Number == 0.0 ) { //  This is the FALSE case
					// Eventually should handle strings and arrays too
					InstructionNum = InstructionNum2;
					// CYCLE
				}
			} else if ( SELECT_CASE_var == KeywordEndWhile ) {

				// reevaluate expression at While and goto there if true, otherwise continue
				ExpressionNum = ErlStack( StackNum ).Instruction( InstructionNum ).Argument1;
				InstructionNum2 = ErlStack( StackNum ).Instruction( InstructionNum ).Argument2;
				ReturnValue = EvaluateExpression( ExpressionNum );
				if ( ( ReturnValue.Number != 0.0 ) && ( WhileLoopExitCounter <= MaxWhileLoopIterations ) ) { //  This is the True case
					// Eventually should handle strings and arrays too
					WriteTrace( StackNum, InstructionNum, ReturnValue ); // duplicative?
					InstructionNum = InstructionNum2;
					++WhileLoopExitCounter;

					continue;
				} else { // false, leave while block
					if ( WhileLoopExitCounter > MaxWhileLoopIterations ) {
						WhileLoopExitCounter = 0;
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Maximum WHILE loop iteration limit reached";
						WriteTrace( StackNum, InstructionNum, ReturnValue );
					} else {
						ReturnValue.Type = ValueNumber;
						ReturnValue.Number = 0.0;
						WriteTrace( StackNum, InstructionNum, ReturnValue );
						WhileLoopExitCounter = 0;
					}
				}
			} else {
				ShowFatalError( "Fatal error in RunStack:  Unknown keyword." );

			}}

			++InstructionNum;
		} // InstructionNum

		ReturnValueActual = ( 4.91 + 632.0 ) / ( 32.0 * ( 4.0 - 10.2 ) ); // must have extra periods

		return ReturnValue;

	}

	void
	WriteTrace(
		int const StackNum,
		int const InstructionNum,
		ErlValueType const & ReturnValue
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       Brent Griffith, May 2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using General::CreateSysTimeIntervalString;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataGlobals::WarmupFlag;
		using DataGlobals::DoingSizing;
		using InputProcessor::ProcessNumber;
		using DataGlobals::OutputFileDebug;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( false );
		int LineNum;
		std::string NameString;
		std::string LineNumString;
		std::string LineString;
		std::string cValueString;
		std::string TimeString;
		std::string DuringWarmup;

		// FLOW:
		if ( ( ! OutputFullEMSTrace ) && ( ! OutputEMSErrors ) ) return;

		if ( ( OutputEMSErrors ) && ( ! OutputFullEMSTrace ) ) {
			//see if error needs to be reported.
			if ( ReturnValue.Type != ValueError ) return;

		}

		if ( ! MyOneTimeFlag ) {
			gio::write( OutputEMSFileUnitNum, fmtA ) << "****  Begin EMS Language Processor Error and Trace Output  *** ";
			gio::write( OutputEMSFileUnitNum, fmtA ) << "<Erl program name, line #, line text, result, occurance timing information ... >";
			MyOneTimeFlag = true;
		}
		// if have not return'd yet then write out full trace

		NameString = ErlStack( StackNum ).Name;
		LineNum = ErlStack( StackNum ).Instruction( InstructionNum ).LineNum;
		LineNumString = IntegerToString( LineNum );
		LineString = ErlStack( StackNum ).Line( LineNum );
		cValueString = ValueToString( ReturnValue );

		// put together timestamp info
		if ( WarmupFlag ) {
			if ( ! DoingSizing ) {
				DuringWarmup = " During Warmup, Occurrence info=";
			} else {
				DuringWarmup = " During Warmup & Sizing, Occurrence info=";
			}
		} else {
			if ( ! DoingSizing ) {
				DuringWarmup = " Occurrence info=";
			} else {
				DuringWarmup = " During Sizing, Occurrence info=";
			}
		}
		TimeString = DuringWarmup + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();

		gio::write( OutputEMSFileUnitNum, fmtA ) << NameString + ",Line " + LineNumString + ',' + LineString + ',' + cValueString + ',' + TimeString;

	}

	//******************************************************************************************

	//  Expression Processor

	//******************************************************************************************

	void
	ParseExpression(
		std::string const & InString, // String of expression text written in the Runtime Language
		int const StackNum, // Parent StackNum??
		int & ExpressionNum, // index of expression in structure
		std::string const & Line // Actual line from string
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       Brent Griffith, May 2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Parsing string into a series of tokens

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using InputProcessor::ProcessNumber;
		using InputProcessor::SameString;
		using DataSystemVariables::DeveloperFlag;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxDoLoopCounts( 500 );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  CHARACTER(len=120), DIMENSION(MaxErrors) :: Error  ! Errors should be stored with the stack
		int NumErrors;
		std::string::size_type Pos;
		std::string StringToken;
		char NextChar;
		bool PeriodFound;
		bool MinusFound;
		bool PlusFound;
		bool MultFound;
		bool DivFound;
		bool ErrorFlag;
		bool OperatorProcessing;
		int CountDoLooping;
		bool LastED; // last character in a numeric was an E or D

		// Object Data
		static Array1D< TokenType > Token;

		// FLOW:
		CountDoLooping = 0;
		NumErrors = 0;
		//  Error = 'No errors.'

		// Break the string into tokens
		int NumTokens( 0 );
		std::string String( InString );

		// Following is a workaround to parse unitary operators as first value in the expression.
		// i.e. Set X = -1
		// this creates Set X = 0-1
		// and seems to work.

		assert( ! String.empty() );
		if ( String[ 0 ] == '-' ) {
			String = "0" + String;
		} else if ( String[ 0 ] == '+' ) {
			String = "0" + String;
		}
		std::string::size_type LastPos( String.length() );
		Pos = 0;
		OperatorProcessing = false; // true when an operator is found until terminated by non-operator
		MinusFound = false;
		MultFound = false;
		DivFound = false;
		while( Pos < LastPos ) {
			++CountDoLooping;
			if ( CountDoLooping > MaxDoLoopCounts ) {
				ShowSevereError( "EMS ParseExpression: Entity=" + ErlStack( StackNum ).Name );
				ShowContinueError( "...Line=" + Line );
				ShowContinueError( "...Failed to process String=\"" + String + "\"." );
				ShowFatalError( "...program terminates due to preceding condition." );
			}
			NextChar = String[ Pos ];
			if ( NextChar == ' ' ) {
				++Pos;
				continue;
			}

			// Extend the token array
			Token.redimension( ++NumTokens );

			// Get the next token
			StringToken = "";
			PeriodFound = false;
			PlusFound = false;
			ErrorFlag = false;
			LastED = false;
			if ( is_any_of( NextChar, "0123456789." ) ) {
				// Parse a number literal token
				++Pos;
				StringToken += NextChar;
				OperatorProcessing = false;
				MultFound = false;
				DivFound = false;

				if( NextChar == '.' ) PeriodFound = true;

				while ( Pos < LastPos ) {
					NextChar = String[ Pos ];
					if ( is_any_of( NextChar, "0123456789.eEdD" ) ) {
						++Pos;
						if ( NextChar == '.' ) {
							if ( PeriodFound ) {
								// ERROR:  two periods appearing in a number literal!
								ShowSevereError( "EMS Parse Expression, for \"" + ErlStack( StackNum ).Name + "\"." );
								ShowContinueError( "...Line=\"" + Line + "\"." );
								ShowContinueError( "...Bad String=\"" + String + "\"." );
								ShowContinueError( "...Two decimal points detected in String." );
								++NumErrors;
								ErrorFlag = true;
								break;
							} else {
								PeriodFound = true;
							}
						}
						if ( is_any_of( NextChar, "eEdD" ) ) {
							StringToken += NextChar;
							if ( LastED ) {
								ShowSevereError( "EMS Parse Expression, for \"" + ErlStack( StackNum ).Name + "\"." );
								ShowContinueError( "...Line=\"" + Line + "\"." );
								ShowContinueError( "...Bad String=\"" + String + "\"." );
								ShowContinueError( "...Two D/E in numeric String." );
								++NumErrors;
								ErrorFlag = true;
								// error
								break;
							} else {
								LastED = true;
							}
						} else {
							StringToken += NextChar;
						}
					} else if ( is_any_of( NextChar, "+-" ) ) { // +/- following an ED is okay.
						if ( LastED ) {
							StringToken += NextChar;
							++Pos;
							LastED = false;
						} else {
							// +/- will be processed on next pass, nothing needs to be done after a numeral
							break;
						}
					} else if ( is_any_of( NextChar, " +-*/^=<>)" ) ) { // Any binary operator is okay
						break; // End of token
					} else {
						// Error: strange sequence of characters:  return TokenString//NextChar   e.g.,  234.44a or 234.44%
						StringToken += NextChar;
						break;
					}
				}

				// Save the number token
				if ( ! ErrorFlag ) {
					Token( NumTokens ).Type = TokenNumber;
					Token( NumTokens ).String = StringToken;
					if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "Number=\"" + StringToken + "\"";
					Token( NumTokens ).Number = ProcessNumber( StringToken, ErrorFlag );
					if ( DeveloperFlag && ErrorFlag ) gio::write( OutputFileDebug, fmtA ) << "Numeric error flagged";
					if ( MinusFound ) {
						Token( NumTokens ).Number = -Token( NumTokens ).Number;
						MinusFound = false;
					}
					if ( ErrorFlag ) {
						// Error: something wrong with this number!
						ShowSevereError( "EMS Parse Expression, for \"" + ErlStack( StackNum ).Name + "\"." );
						ShowContinueError( "...Line=\"" + Line + "\"." );
						ShowContinueError( "...Bad String=\"" + String + "\"." );
						ShowContinueError( "Invalid numeric=\"" + StringToken + "\"." );
						++NumErrors;
					}
				}

			} else if ( is_any_of( NextChar, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" ) ) {
				// Parse an undetermined string token (could be a variable, subroutine, or named operator)
				++Pos;
				StringToken += NextChar;
				OperatorProcessing = false;
				MultFound = false;
				DivFound = false;

				while ( Pos < LastPos ) {
					NextChar = String[ Pos ];
					if ( is_any_of( NextChar, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789" ) ) {
						++Pos;
						StringToken += NextChar;
					} else if ( is_any_of( NextChar, " +-*/^=<>()" ) ) {
						break; // End of token
					} else {
						// Error: bad syntax:  return TokenString//NextChar   e.g.,  var1$ or b%
						break;
					}
				}

				// Save the variable token
				Token( NumTokens ).Type = TokenVariable;
				Token( NumTokens ).String = StringToken;
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "Variable=\"" + StringToken + "\"";
				Token( NumTokens ).Variable = NewEMSVariable( StringToken, StackNum );

			} else if ( is_any_of( NextChar, "+-*/^=<>@|&" ) ) {
				// Parse an operator token
				if ( NextChar == '-' ) {
					StringToken = "-";
					if ( MultFound ) {
						ShowSevereError( "EMS Parse Expression, for \"" + ErlStack( StackNum ).Name + "\"." );
						ShowContinueError( "...Line = \"" + Line + "\"." );
						ShowContinueError( "...Minus sign used on the right side of multiplication sign." );
						ShowContinueError( "...Use parenthesis to wrap appropriate variables. For example, X * ( -Y )." );
						++NumErrors;
						MultFound = false;
					} else if ( DivFound ) {
						ShowSevereError( "EMS Parse Expression, for \"" + ErlStack( StackNum ).Name + "\"." );
						ShowContinueError( "...Line = \"" + Line + "\"." );
						ShowContinueError( "...Minus sign used on the right side of division sign." );
						ShowContinueError( "...Use parenthesis to wrap appropriate variables. For example, X / ( -Y )." );
						++NumErrors;
						DivFound = false;
					} else if ( OperatorProcessing && ( NextChar == '-' ) ) {
						// if operator was deterined last pass and this character is a -, then insert a 0 before the minus and treat as subtraction
						// example: change "Var == -1" to "Var == 0-1" 
						OperatorProcessing = false;
						String.insert( Pos, "0" );
						++LastPos;
						StringToken = "0";
						MultFound = false;
						DivFound = false;
					} else {
						StringToken = NextChar;
						Token( NumTokens ).Type = TokenOperator;
					}
				} else { // any other character process as operator
					StringToken = NextChar;
					Token( NumTokens ).Type = TokenOperator;
				}

				// First check for two character operators:  == <> <= >=
				std::string const cc( String.substr( Pos, 2 ) );
				if ( cc == "==" ) {
					if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 2 ) + "\"";
					Token( NumTokens ).Operator = OperatorEqual;
					Token( NumTokens ).String = String.substr( Pos, 2 );
					OperatorProcessing = true;
					++Pos;
				} else if ( cc == "<>" ) {
					if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 2 ) + "\"";
					Token( NumTokens ).Operator = OperatorNotEqual;
					Token( NumTokens ).String = String.substr( Pos, 2 );
					OperatorProcessing = true;
					++Pos;
				} else if ( cc == "<=" ) {
					if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 2 ) + "\"";
					Token( NumTokens ).Operator = OperatorLessOrEqual;
					Token( NumTokens ).String = String.substr( Pos, 2 );
					OperatorProcessing = true;
					++Pos;
				} else if ( cc == ">=" ) {
					if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 2 ) + "\"";
					Token( NumTokens ).Operator = OperatorGreaterOrEqual;
					Token( NumTokens ).String = String.substr( Pos, 2 );
					OperatorProcessing = true;
					++Pos;
				} else if ( cc == "||" ) {
					if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 2 ) + "\"";
					Token( NumTokens ).Operator = OperatiorLogicalOR;
					Token( NumTokens ).String = String.substr( Pos, 2 );
					OperatorProcessing = true;
					++Pos;
				} else if ( cc == "&&" ) {
					if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 2 ) + "\"";
					Token( NumTokens ).Operator = OperatorLogicalAND;
					Token( NumTokens ).String = String.substr( Pos, 2 );
					OperatorProcessing = true;
					++Pos;
					// next check for builtin functions signaled by "@"
				} else if ( String[ Pos ] == '@' ) {

					if ( SameString( String.substr( Pos, 6 ), "@Round" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 6 ) + "\"";
						Token( NumTokens ).Operator = FuncRound;
						Token( NumTokens ).String = String.substr( Pos, 6 );
						Pos += 5;
					} else if ( SameString( String.substr( Pos, 4 ), "@Mod" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 4 ) + "\"";
						Token( NumTokens ).Operator = FuncMod;
						Token( NumTokens ).String = String.substr( Pos, 4 );
						Pos += 3;
					} else if ( SameString( String.substr( Pos, 4 ), "@Sin" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 4 ) + "\"";
						Token( NumTokens ).Operator = FuncSin;
						Token( NumTokens ).String = String.substr( Pos, 4 );
						Pos += 3;
					} else if ( SameString( String.substr( Pos, 4 ), "@Cos" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 4 ) + "\"";
						Token( NumTokens ).Operator = FuncCos;
						Token( NumTokens ).String = String.substr( Pos, 4 );
						Pos += 3;
					} else if ( SameString( String.substr( Pos, 7 ), "@ArcCos" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 7 ) + "\"";
						Token( NumTokens ).Operator = FuncArcCos;
						Token( NumTokens ).String = String.substr( Pos, 7 );
						Pos += 6;
					} else if ( SameString( String.substr( Pos, 7 ), "@ArcSin" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 7 ) + "\"";
						Token( NumTokens ).Operator = FuncArcSin;
						Token( NumTokens ).String = String.substr( Pos, 7 );
						Pos += 6;
					} else if ( SameString( String.substr( Pos, 9 ), "@DegToRad" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncDegToRad;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 9 ), "@RadToDeg" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncRadToDeg;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 4 ), "@Exp" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 4 ) + "\"";
						Token( NumTokens ).Operator = FuncExp;
						Token( NumTokens ).String = String.substr( Pos, 4 );
						Pos += 3;
					} else if ( SameString( String.substr( Pos, 3 ), "@Ln" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 3 ) + "\"";
						Token( NumTokens ).Operator = FuncLn;
						Token( NumTokens ).String = String.substr( Pos, 3 );
						Pos += 2;
					} else if ( SameString( String.substr( Pos, 4 ), "@Max" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 4 ) + "\"";
						Token( NumTokens ).Operator = FuncMax;
						Token( NumTokens ).String = String.substr( Pos, 4 );
						Pos += 3;
					} else if ( SameString( String.substr( Pos, 4 ), "@Min" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 4 ) + "\"";
						Token( NumTokens ).Operator = FuncMin;
						Token( NumTokens ).String = String.substr( Pos, 4 );
						Pos += 3;
					} else if ( SameString( String.substr( Pos, 4 ), "@Abs" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 4 ) + "\"";
						Token( NumTokens ).Operator = FuncABS;
						Token( NumTokens ).String = String.substr( Pos, 4 );
						Pos += 3;
					} else if ( SameString( String.substr( Pos, 14 ), "@RANDOMUNIFORM" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 14 ) + "\"";
						Token( NumTokens ).Operator = FuncRandU;
						Token( NumTokens ).String = String.substr( Pos, 14 );
						Pos += 13;
					} else if ( SameString( String.substr( Pos, 13 ), "@RANDOMNORMAL" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 13 ) + "\"";
						Token( NumTokens ).Operator = FuncRandG;
						Token( NumTokens ).String = String.substr( Pos, 13 );
						Pos += 12;
					} else if ( SameString( String.substr( Pos, 11 ), "@SEEDRANDOM" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + String.substr( Pos, 11 ) + "\"";
						Token( NumTokens ).Operator = FuncRandSeed;
						Token( NumTokens ).String = String.substr( Pos, 11 );
						Pos += 10;
					} else if ( SameString( String.substr( Pos, 15 ), "@RhoAirFnPbTdbW" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 15 ) + "\"";
						Token( NumTokens ).Operator = FuncRhoAirFnPbTdbW;
						Token( NumTokens ).String = String.substr( Pos, 15 );
						Pos += 14;
					} else if ( SameString( String.substr( Pos, 12 ), "@CpAirFnWTdb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 12 ) + "\"";
						Token( NumTokens ).Operator = FuncCpAirFnWTdb;
						Token( NumTokens ).String = String.substr( Pos, 12 );
						Pos += 11;
					} else if ( SameString( String.substr( Pos, 13 ), "@HfgAirFnWTdb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 13 ) + "\"";
						Token( NumTokens ).Operator = FuncHfgAirFnWTdb;
						Token( NumTokens ).String = String.substr( Pos, 13 );
						Pos += 12;
					} else if ( SameString( String.substr( Pos, 12 ), "@HgAirFnWTdb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 12 ) + "\"";
						Token( NumTokens ).Operator = FuncHgAirFnWTdb;
						Token( NumTokens ).String = String.substr( Pos, 12 );
						Pos += 11;
					} else if ( SameString( String.substr( Pos, 14 ), "@TdpFnTdbTwbPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 14 ) + "\"";
						Token( NumTokens ).Operator = FuncTdpFnTdbTwbPb;
						Token( NumTokens ).String = String.substr( Pos, 14 );
						Pos += 13;
					} else if ( SameString( String.substr( Pos, 9 ), "@TdpFnWPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncTdpFnWPb;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 8 ), "@HFnTdbW" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 8 ) + "\"";
						Token( NumTokens ).Operator = FuncHFnTdbW;
						Token( NumTokens ).String = String.substr( Pos, 8 );
						Pos += 7;
					} else if ( SameString( String.substr( Pos, 11 ), "@HFnTdbRhPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 11 ) + "\"";
						Token( NumTokens ).Operator = FuncHFnTdbRhPb;
						Token( NumTokens ).String = String.substr( Pos, 11 );
						Pos += 10;
					} else if ( SameString( String.substr( Pos, 8 ), "@TdbFnHW" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 8 ) + "\"";
						Token( NumTokens ).Operator = FuncTdbFnHW;
						Token( NumTokens ).String = String.substr( Pos, 8 );
						Pos += 7;
					} else if ( SameString( String.substr( Pos, 12 ), "@RhovFnTdbRh" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 12 ) + "\"";
						Token( NumTokens ).Operator = FuncRhovFnTdbRh;
						Token( NumTokens ).String = String.substr( Pos, 12 );
						Pos += 11;
					} else if ( SameString( String.substr( Pos, 18 ), "@RhovFnTdbRhLBnd0C" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 18 ) + "\"";
						Token( NumTokens ).Operator = FuncRhovFnTdbRhLBnd0C;
						Token( NumTokens ).String = String.substr( Pos, 18 );
						Pos += 17;
					} else if ( SameString( String.substr( Pos, 13 ), "@RhovFnTdbWPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 13 ) + "\"";
						Token( NumTokens ).Operator = FuncRhovFnTdbWPb;
						Token( NumTokens ).String = String.substr( Pos, 13 );
						Pos += 12;
					} else if ( SameString( String.substr( Pos, 12 ), "@RhFnTdbRhov" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 12 ) + "\"";
						Token( NumTokens ).Operator = FuncRhFnTdbRhov;
						Token( NumTokens ).String = String.substr( Pos, 12 );
						Pos += 11;
					} else if ( SameString( String.substr( Pos, 18 ), "@RhFnTdbRhovLBnd0C" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 18 ) + "\"";
						Token( NumTokens ).Operator = FuncRhFnTdbRhovLBnd0C;
						Token( NumTokens ).String = String.substr( Pos, 18 );
						Pos += 17;
					} else if ( SameString( String.substr( Pos, 11 ), "@RhFnTdbWPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 11 ) + "\"";
						Token( NumTokens ).Operator = FuncRhFnTdbWPb;
						Token( NumTokens ).String = String.substr( Pos, 11 );
						Pos += 10;
					} else if ( SameString( String.substr( Pos, 12 ), "@TwbFnTdbWPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 12 ) + "\"";
						Token( NumTokens ).Operator = FuncTwbFnTdbWPb;
						Token( NumTokens ).String = String.substr( Pos, 12 );
						Pos += 11;
					} else if ( SameString( String.substr( Pos, 10 ), "@VFnTdbWPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 10 ) + "\"";
						Token( NumTokens ).Operator = FuncVFnTdbWPb;
						Token( NumTokens ).String = String.substr( Pos, 10 );
						Pos += 9;
					} else if ( SameString( String.substr( Pos, 9 ), "@WFnTdpPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncWFnTdpPb;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 8 ), "@WFnTdbH" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 8 ) + "\"";
						Token( NumTokens ).Operator = FuncWFnTdbH;
						Token( NumTokens ).String = String.substr( Pos, 8 );
						Pos += 7;
					} else if ( SameString( String.substr( Pos, 12 ), "@WFnTdbTwbPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 12 ) + "\"";
						Token( NumTokens ).Operator = FuncWFnTdbTwbPb;
						Token( NumTokens ).String = String.substr( Pos, 12 );
						Pos += 11;
					} else if ( SameString( String.substr( Pos, 11 ), "@WFnTdbRhPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 11 ) + "\"";
						Token( NumTokens ).Operator = FuncWFnTdbRhPb;
						Token( NumTokens ).String = String.substr( Pos, 11 );
						Pos += 10;
					} else if ( SameString( String.substr( Pos, 11 ), "@PsatFnTemp" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 11 ) + "\"";
						Token( NumTokens ).Operator = FuncPsatFnTemp;
						Token( NumTokens ).String = String.substr( Pos, 11 );
						Pos += 10;
					} else if ( SameString( String.substr( Pos, 10 ), "@TsatFnHPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 10 ) + "\"";
						Token( NumTokens ).Operator = FuncTsatFnHPb;
						Token( NumTokens ).String = String.substr( Pos, 10 );
						Pos += 9;
					} else if ( SameString( String.substr( Pos, 9 ), "@TsatFnPb" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncTsatFnPb;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 5 ), "@CpCW" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 5 ) + "\"";
						Token( NumTokens ).Operator = FuncCpCW;
						Token( NumTokens ).String = String.substr( Pos, 5 );
						Pos += 4;
					} else if ( SameString( String.substr( Pos, 5 ), "@CpHW" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 5 ) + "\"";
						Token( NumTokens ).Operator = FuncCpHW;
						Token( NumTokens ).String = String.substr( Pos, 5 );
						Pos += 4;
					} else if ( SameString( String.substr( Pos, 7 ), "@RhoH2O" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 7 ) + "\"";
						Token( NumTokens ).Operator = FuncRhoH2O;
						Token( NumTokens ).String = String.substr( Pos, 7 );
						Pos += 6;
					} else if ( SameString( String.substr( Pos, 12 ), "@FATALHALTEP" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 12 ) + "\"";
						Token( NumTokens ).Operator = FuncFatalHaltEp;
						Token( NumTokens ).String = String.substr( Pos, 12 );
						Pos += 11;
					} else if ( SameString( String.substr( Pos, 13 ), "@SEVEREWARNEP" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 13 ) + "\"";
						Token( NumTokens ).Operator = FuncSevereWarnEp;
						Token( NumTokens ).String = String.substr( Pos, 13 );
						Pos += 12;
					} else if ( SameString( String.substr( Pos, 7 ), "@WARNEP" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 7 ) + "\"";
						Token( NumTokens ).Operator = FuncWarnEp;
						Token( NumTokens ).String = String.substr( Pos, 7 );
						Pos += 6;
					} else if ( SameString( String.substr( Pos, 11 ), "@TRENDVALUE" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 11 ) + "\"";
						Token( NumTokens ).Operator = FuncTrendValue;
						Token( NumTokens ).String = String.substr( Pos, 11 );
						Pos += 10;
					} else if ( SameString( String.substr( Pos, 13 ), "@TRENDAVERAGE" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 13 ) + "\"";
						Token( NumTokens ).Operator = FuncTrendAverage;
						Token( NumTokens ).String = String.substr( Pos, 13 );
						Pos += 12;
					} else if ( SameString( String.substr( Pos, 9 ), "@TRENDMAX" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncTrendMax;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 9 ), "@TRENDMIN" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncTrendMin;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 15 ), "@TRENDDIRECTION" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 15 ) + "\"";
						Token( NumTokens ).Operator = FuncTrendDirection;
						Token( NumTokens ).String = String.substr( Pos, 15 );
						Pos += 14;
					} else if ( SameString( String.substr( Pos, 9 ), "@TRENDSUM" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 9 ) + "\"";
						Token( NumTokens ).Operator = FuncTrendSum;
						Token( NumTokens ).String = String.substr( Pos, 9 );
						Pos += 8;
					} else if ( SameString( String.substr( Pos, 11 ), "@CURVEVALUE" ) ) {
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "FUNCTION \"" + String.substr( Pos, 11 ) + "\"";
						Token( NumTokens ).Operator = FuncCurveValue;
						Token( NumTokens ).String = String.substr( Pos, 11 );
						Pos += 10;
					} else { // throw error
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ERROR \"" + String + "\"";
						ShowFatalError( "EMS Runtime Language: did not find valid input for built-in function =" + String );
					}
				} else {
					// Check for remaining single character operators
					Token( NumTokens ).String = StringToken;
					MultFound = false;
					DivFound = false;

					if( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "OPERATOR \"" + StringToken + "\"";

					if ( StringToken == "+" ) {
						if ( ! OperatorProcessing ) {
							Token( NumTokens ).Operator = OperatorAdd;
							OperatorProcessing = true;
						} else {
							PlusFound = true;
							OperatorProcessing = false;
						}
					} else if ( StringToken == "-" ) {
						if ( ! OperatorProcessing ) {
							Token( NumTokens ).Operator = OperatorSubtract;
							OperatorProcessing = true;
						} else {
							MinusFound = true;
							OperatorProcessing = false;
						}
					} else if ( StringToken == "*" ) {
						Token( NumTokens ).Operator = OperatorMultiply;
						MultFound = true;
						OperatorProcessing = true;
					} else if ( StringToken == "/" ) {
						Token( NumTokens ).Operator = OperatorDivide;
						DivFound = true;
						OperatorProcessing = true;
					} else if ( StringToken == "<" ) {
						Token( NumTokens ).Operator = OperatorLessThan;
						OperatorProcessing = true;
					} else if ( StringToken == ">" ) {
						Token( NumTokens ).Operator = OperatorGreaterThan;
						OperatorProcessing = true;
					} else if ( StringToken == "^" ) {
						Token( NumTokens ).Operator = OperatorRaiseToPower;
						OperatorProcessing = true;
					} else if( StringToken == "0" && ( NextChar == '-' ) ) {
						// process string insert = "0"
						Token( NumTokens ).Type = TokenNumber;
						Token( NumTokens ).String = StringToken;
					} else {
						// Uh OH, this should never happen! throw error
						if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ERROR \"" + StringToken + "\"";
						ShowFatalError( "EMS, caught unexpected token = \"" + StringToken + "\" ; while parsing string=" + String );

					}
				}

				++Pos;

			} else if ( is_any_of( NextChar, "()" ) ) {
				// Parse a parenthesis token
				++Pos;
				StringToken = NextChar;
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "PAREN \"" + StringToken + "\"";
				Token( NumTokens ).Type = TokenParenthesis;
				Token( NumTokens ).String = StringToken;
				if ( NextChar == '(' ) {
					Token( NumTokens ).Parenthesis = ParenthesisLeft;
					OperatorProcessing = true;
				}
				if ( NextChar == ')' ) Token( NumTokens ).Parenthesis = ParenthesisRight;

			} else if ( is_any_of( NextChar, "\"" ) ) {
				// Parse a string literal token
				if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "LITERAL STRING";
				++Pos;

			} else {
				// Error: bad start to the token

			}
		}

		if ( NumErrors > 0 ) {
			if ( DeveloperFlag ) gio::write( OutputFileDebug, fmtA ) << "ERROR OUT";
			ShowFatalError( "EMS, previous errors cause termination." );
		}

		ExpressionNum = ProcessTokens( Token, NumTokens, StackNum, String );

	}

	int
	ProcessTokens(
		Array1S< TokenType > const TokenIN,
		int const NumTokensIN,
		int const StackNum,
		std::string const & ParsingString
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Processes tokens into expressions.

		// METHODOLOGY EMPLOYED:
		// Uses recursion to handle tokens with compound expressions

		// Using/Aliasing
		using InputProcessor::ProcessNumber;

		// Return value
		int ExpressionNum;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Pos;
		int LastPos;
		int TokenNum;
		int NumTokens;
		int Depth;
		int NumSubTokens;
		int NewNumTokens;
		int OperatorNum;
		int NumOperands;
		int ParenthWhileCounter; // used to trap for unbalanced parentheses

		// Object Data
		Array1D< TokenType > Token( TokenIN );
		Array1D< TokenType > SubTokenList;

		// FLOW:
		ExpressionNum = 0;
		NumTokens = NumTokensIN;

		// Process parentheses
		Pos = 0;
		for ( TokenNum = 1; TokenNum <= NumTokens; ++TokenNum ) {
			if ( Token( TokenNum ).Type == TokenParenthesis ) {
				Pos = TokenNum;
				break;
			}
		}

		ParenthWhileCounter = 0;

		while ( ( Pos > 0 ) && ( ParenthWhileCounter < 50 ) ) {
			++ParenthWhileCounter;
			Depth = 0;
			for ( TokenNum = 1; TokenNum <= NumTokens; ++TokenNum ) {
				if ( Token( TokenNum ).Type == TokenParenthesis ) {
					if ( Token( TokenNum ).Parenthesis == ParenthesisLeft ) {
						if ( Depth == 0 ) Pos = TokenNum; // Record position of first left parenthesis
						++Depth;
					}
					if ( Token( TokenNum ).Parenthesis == ParenthesisRight ) {
						--Depth;
						if ( Depth == 0 ) {
							LastPos = TokenNum;
							NumSubTokens = LastPos - Pos - 1;
							SubTokenList.allocate( NumSubTokens );
							SubTokenList( {1,NumSubTokens} ) = Token( {Pos + 1,LastPos - 1} ); // Need to check that these don't exceed bounds
							ExpressionNum = ProcessTokens( SubTokenList, NumSubTokens, StackNum, ParsingString );
							SubTokenList.deallocate();

							// Replace the parenthetical tokens with one expression token
							NewNumTokens = NumTokens - NumSubTokens - 1;
							if ( NewNumTokens > 0 ) {
								if ( LastPos + 1 <= NumTokens ) {
									Token( {Pos+1,NewNumTokens} ) = Token( {LastPos+1,_} );
								}
								Token.redimension( NewNumTokens );
								Token( Pos ).Type = TokenExpression;
								Token( Pos ).Expression = ExpressionNum;
								Token( Pos ).String = "Expr";
								NumTokens = NewNumTokens;
							}

							// Reset loop for next parenthetical set
							break;
						}
					}
				}

			}

			// This repeats code again...  Just checks to see if there are any more parentheses to be found
			Pos = 0;
			for ( TokenNum = 1; TokenNum <= NumTokens; ++TokenNum ) {
				if ( Token( TokenNum ).Type == TokenParenthesis ) {
					Pos = TokenNum;
					break;
				}
			}

		}

		if ( ParenthWhileCounter == 50 ) { // symptom of mismatched parenthesis
			ShowSevereError( "EMS error parsing parentheses, check that parentheses are balanced" );
			ShowContinueError( "String being parsed=\"" + ParsingString + "\"." );
			ShowFatalError( "Program terminates due to preceding error." );
		}

		SetupPossibleOperators(); // includes built-in functions

		// Process operators and builtin functions
		// Loop thru all operators and group expressions in the order of precedence
		for ( OperatorNum = 1; OperatorNum <= NumPossibleOperators; ++OperatorNum ) {

			// Find the next occurrence of the operator
			Pos = 0; //  position in sequence of tokens
			for ( TokenNum = 1; TokenNum <= NumTokens; ++TokenNum ) {
				if ( ( Token( TokenNum ).Type == TokenOperator ) && ( Token( TokenNum ).Operator == OperatorNum ) ) {
					Pos = TokenNum;
					break;
				}
			}

			while ( Pos > 0 ) {
				if ( Pos == 1 ) {
					//if first token is for a built-in function starting with "@" then okay, otherwise the operator needs a LHS
					if ( Token( TokenNum ).Operator > OperatiorLogicalOR ) { // we have a function expression to set up
						ExpressionNum = NewExpression();
						ErlExpression( ExpressionNum ).Operator = OperatorNum;
						NumOperands = PossibleOperators( OperatorNum ).NumOperands;
						ErlExpression( ExpressionNum ).NumOperands = NumOperands;
						ErlExpression( ExpressionNum ).Operand.allocate( NumOperands );

						ErlExpression( ExpressionNum ).Operand( 1 ).Type = Token( Pos + 1 ).Type;
						ErlExpression( ExpressionNum ).Operand( 1 ).Number = Token( Pos + 1 ).Number;
						ErlExpression( ExpressionNum ).Operand( 1 ).Expression = Token( Pos + 1 ).Expression;
						ErlExpression( ExpressionNum ).Operand( 1 ).Variable = Token( Pos + 1 ).Variable;
						if ( Token( Pos + 1 ).Variable > 0 ) {
							ErlExpression( ExpressionNum ).Operand( 1 ).TrendVariable = ErlVariable( Token( Pos + 1 ).Variable ).Value.TrendVariable;
							ErlExpression( ExpressionNum ).Operand( 1 ).TrendVarPointer = ErlVariable( Token( Pos + 1 ).Variable ).Value.TrendVarPointer;
						}
						if ( ( NumOperands >= 2 ) && ( NumTokens >= 3 ) ) {
							ErlExpression( ExpressionNum ).Operand( 2 ).Type = Token( Pos + 2 ).Type;
							ErlExpression( ExpressionNum ).Operand( 2 ).Number = Token( Pos + 2 ).Number;
							ErlExpression( ExpressionNum ).Operand( 2 ).Expression = Token( Pos + 2 ).Expression;
							ErlExpression( ExpressionNum ).Operand( 2 ).Variable = Token( Pos + 2 ).Variable;
						}

						if ( ( NumOperands >= 3 ) && ( NumTokens >= 4 ) ) {
							ErlExpression( ExpressionNum ).Operand( 3 ).Type = Token( Pos + 3 ).Type;
							ErlExpression( ExpressionNum ).Operand( 3 ).Number = Token( Pos + 3 ).Number;
							ErlExpression( ExpressionNum ).Operand( 3 ).Expression = Token( Pos + 3 ).Expression;
							ErlExpression( ExpressionNum ).Operand( 3 ).Variable = Token( Pos + 3 ).Variable;
							if ( ( NumOperands == 3 ) && ( NumTokens - 4 > 0 ) ) { // too many tokens for this non-binary operator
								ShowFatalError( "EMS error parsing tokens, too many for built-in function" );
							}
						}

						if ( ( NumOperands >= 4 ) && ( NumTokens >= 5 ) ) {
							ErlExpression( ExpressionNum ).Operand( 4 ).Type = Token( Pos + 4 ).Type;
							ErlExpression( ExpressionNum ).Operand( 4 ).Number = Token( Pos + 4 ).Number;
							ErlExpression( ExpressionNum ).Operand( 4 ).Expression = Token( Pos + 4 ).Expression;
							ErlExpression( ExpressionNum ).Operand( 4 ).Variable = Token( Pos + 4 ).Variable;
							if ( ( NumOperands == 4 ) && ( NumTokens - 5 > 0 ) ) { // too many tokens for this non-binary operator
								ShowFatalError( "EMS error parsing tokens, too many for built-in function" );
							}
						}

						if ( ( NumOperands == 5 ) && ( NumTokens >= 6 ) ) {
							ErlExpression( ExpressionNum ).Operand( 5 ).Type = Token( Pos + 5 ).Type;
							ErlExpression( ExpressionNum ).Operand( 5 ).Number = Token( Pos + 5 ).Number;
							ErlExpression( ExpressionNum ).Operand( 5 ).Expression = Token( Pos + 5 ).Expression;
							ErlExpression( ExpressionNum ).Operand( 5 ).Variable = Token( Pos + 5 ).Variable;
							if ( ( NumOperands == 5 ) && ( NumTokens - 6 > 0 ) ) { // too many tokens for this non-binary operator
								ShowFatalError( "EMS error parsing tokens, too many for  built-in function" );
							}
						}
						break;
					} else {
						ShowSevereError( "The operator \"" + PossibleOperators( OperatorNum ).Symbol + "\" is missing the left-hand operand!" );
						ShowContinueError( "String being parsed=\"" + ParsingString + "\"." );
						break;
					}
				} else if ( Pos == NumTokens ) {
					ShowSevereError( "The operator \"" + PossibleOperators( OperatorNum ).Symbol + "\" is missing the right-hand operand!" );
					ShowContinueError( "String being parsed=\"" + ParsingString + "\"." );
					break;
				} else {

					ExpressionNum = NewExpression();
					ErlExpression( ExpressionNum ).Operator = OperatorNum;
					NumOperands = PossibleOperators( OperatorNum ).NumOperands;
					ErlExpression( ExpressionNum ).NumOperands = NumOperands;
					ErlExpression( ExpressionNum ).Operand.allocate( NumOperands );

					// PE commment: Need a right-hand and left-hand check for these, not just number of operators
					// Unification of TYPEs would turn these into one-liners

					ErlExpression( ExpressionNum ).Operand( 1 ).Type = Token( Pos - 1 ).Type;
					ErlExpression( ExpressionNum ).Operand( 1 ).Number = Token( Pos - 1 ).Number;
					ErlExpression( ExpressionNum ).Operand( 1 ).Expression = Token( Pos - 1 ).Expression;
					ErlExpression( ExpressionNum ).Operand( 1 ).Variable = Token( Pos - 1 ).Variable;

					if ( NumOperands >= 2 ) {
						ErlExpression( ExpressionNum ).Operand( 2 ).Type = Token( Pos + 1 ).Type;
						ErlExpression( ExpressionNum ).Operand( 2 ).Number = Token( Pos + 1 ).Number;
						ErlExpression( ExpressionNum ).Operand( 2 ).Expression = Token( Pos + 1 ).Expression;
						ErlExpression( ExpressionNum ).Operand( 2 ).Variable = Token( Pos + 1 ).Variable;
					}

					// Replace the three tokens with one expression token
					if ( ( NumOperands == 2 ) && ( NumTokens - 2 > 0 ) ) {
						if ( Pos + 2 <= NumTokens ) {
							Token( {Pos,NumTokens-2} ) = Token( {Pos+2,_} );
						}
						Token( Pos - 1 ).Type = TokenExpression;
						Token( Pos - 1 ).Expression = ExpressionNum;
						Token( Pos - 1 ).String = "Expr";
						NumTokens -= 2;
						Token.redimension( NumTokens );
					}
				}

				// Find the next occurrence of the operator  (this repeats code, but don't have better idea)
				Pos = 0;
				for ( TokenNum = 1; TokenNum <= NumTokens; ++TokenNum ) {
					if ( ( Token( TokenNum ).Type == TokenOperator ) && ( Token( TokenNum ).Operator == OperatorNum ) ) {
						Pos = TokenNum;
						break;
					}
				}

			}

		}

		// Should be down to just one token now
		if ( Token( 1 ).Type == TokenNumber ) {
			ExpressionNum = NewExpression();
			ErlExpression( ExpressionNum ).Operator = OperatorLiteral;
			ErlExpression( ExpressionNum ).NumOperands = 1;
			ErlExpression( ExpressionNum ).Operand.allocate( 1 );
			ErlExpression( ExpressionNum ).Operand( 1 ).Type = Token( 1 ).Type;
			ErlExpression( ExpressionNum ).Operand( 1 ).Number = Token( 1 ).Number;
		} else if ( Token( 1 ).Type == TokenVariable ) {
			ExpressionNum = NewExpression();
			ErlExpression( ExpressionNum ).Operator = OperatorLiteral;
			ErlExpression( ExpressionNum ).NumOperands = 1;
			ErlExpression( ExpressionNum ).Operand.allocate( 1 );
			ErlExpression( ExpressionNum ).Operand( 1 ).Type = Token( 1 ).Type;
			ErlExpression( ExpressionNum ).Operand( 1 ).Variable = Token( 1 ).Variable;
		}

		Token.deallocate();

		return ExpressionNum;

	}

	int
	NewExpression()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Creates a new expression.

		// METHODOLOGY EMPLOYED:

		// Return value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		// Object Data

		// FLOW:
		if ( NumExpressions == 0 ) {
			ErlExpression.allocate( 1 );
			NumExpressions = 1;
		} else {
			ErlExpression.redimension( ++NumExpressions );
		}

		return NumExpressions;

	}

	ErlValueType
	EvaluateExpression( int const ExpressionNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       Brent Griffith, May 2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Evaluates an expression.

		// METHODOLOGY EMPLOYED:

		//USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY : IEEE_IS_NAN ! Use IEEE_IS_NAN when GFortran supports it
		// Using/Aliasing
		using DataGlobals::DegToRadians; // unused, TimeStepZone
		using namespace Psychrometrics;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using CurveManager::CurveValue;

		// Return value
		ErlValueType ReturnValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int thisTrend; // local temporary
		int thisIndex; // local temporary
		Real64 thisAverage; // local temporary
		int loop; // local temporary
		Real64 thisSlope; // local temporary
		Real64 thisMax; // local temporary
		Real64 thisMin; // local temporary
		int OperandNum;
		int SeedN; // number of digits in the number used to seed the generator
		Array1D_int SeedIntARR; // local temporary for random seed
		Real64 tmpRANDU1; // local temporary for uniform random number
		Real64 tmpRANDU2; // local temporary for uniform random number
		Real64 tmpRANDG; // local temporary for gaussian random number
		Real64 UnitCircleTest; // local temporary for Box-Muller algo
		Real64 TestValue; // local temporary

		// Object Data
		Array1D< ErlValueType > Operand;

		static std::string const EMSBuiltInFunction( "EMS Built-In Function" );

		// FLOW:
		ReturnValue.Type = ValueNumber;
		ReturnValue.Number = 0.0;

		if ( ExpressionNum > 0 ) {
			// is there a way to keep these and not allocate and deallocate all the time?
			Operand.allocate( ErlExpression( ExpressionNum ).NumOperands );
			// Reduce operands down to literals
			for ( OperandNum = 1; OperandNum <= ErlExpression( ExpressionNum ).NumOperands; ++OperandNum ) {
				Operand( OperandNum ) = ErlExpression( ExpressionNum ).Operand( OperandNum );
				if ( Operand( OperandNum ).Type == ValueExpression ) {
					Operand( OperandNum ) = EvaluateExpression( Operand( OperandNum ).Expression ); //recursive call
				} else if ( Operand( OperandNum ).Type == ValueVariable ) {
					Operand( OperandNum ) = ErlVariable( Operand( OperandNum ).Variable ).Value;
				}
			}

			// Perform the operation
			{ auto const SELECT_CASE_var( ErlExpression( ExpressionNum ).Operator );

			if ( SELECT_CASE_var == OperatorLiteral ) {
				ReturnValue = Operand( 1 );
			} else if ( SELECT_CASE_var == OperatorNegative ) { // unary minus sign.  parsing does not work yet
				ReturnValue = SetErlValueNumber( -1.0 * Operand( 1 ).Number );
			} else if ( SELECT_CASE_var == OperatorDivide ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( Operand( 2 ).Number == 0.0 ) {
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Divide by zero!";
					} else {
						ReturnValue = SetErlValueNumber( Operand( 1 ).Number / Operand( 2 ).Number );
					}
				}

			} else if ( SELECT_CASE_var == OperatorMultiply ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					ReturnValue = SetErlValueNumber( Operand( 1 ).Number * Operand( 2 ).Number );
				}

			} else if ( SELECT_CASE_var == OperatorSubtract ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					ReturnValue = SetErlValueNumber( Operand( 1 ).Number - Operand( 2 ).Number );
				}

			} else if ( SELECT_CASE_var == OperatorAdd ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					ReturnValue = SetErlValueNumber( Operand( 1 ).Number + Operand( 2 ).Number );
				}

			} else if ( SELECT_CASE_var == OperatorEqual ) {
				if ( Operand( 1 ).Type == Operand( 2 ).Type ) {
					if ( Operand( 1 ).Type == ValueNull ) {
						ReturnValue = True;
					} else if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 1 ).Number == Operand( 2 ).Number ) ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				} else {
					ReturnValue = False;
				}

			} else if ( SELECT_CASE_var == OperatorNotEqual ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( Operand( 1 ).Number != Operand( 2 ).Number ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				}

			} else if ( SELECT_CASE_var == OperatorLessOrEqual ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( Operand( 1 ).Number <= Operand( 2 ).Number ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				}

			} else if ( SELECT_CASE_var == OperatorGreaterOrEqual ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( Operand( 1 ).Number >= Operand( 2 ).Number ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				}
			} else if ( SELECT_CASE_var == OperatorLessThan ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( Operand( 1 ).Number < Operand( 2 ).Number ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				}
			} else if ( SELECT_CASE_var == OperatorGreaterThan ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( Operand( 1 ).Number > Operand( 2 ).Number ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				}

			} else if ( SELECT_CASE_var == OperatorRaiseToPower ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					TestValue = std::pow( Operand( 1 ).Number, Operand( 2 ).Number );
					if ( std::isnan( TestValue ) ) { // Use IEEE_IS_NAN when GFortran supports it
						// throw Error
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Attempted to raise to power with incompatible numbers: " + TrimSigDigits( Operand( 1 ).Number, 6 ) + " raised to " + TrimSigDigits( Operand( 2 ).Number, 6 );
					} else {
						ReturnValue = SetErlValueNumber( TestValue );
					}

				}
			} else if ( SELECT_CASE_var == OperatorLogicalAND ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( ( Operand( 1 ).Number == True.Number ) && ( Operand( 2 ).Number == True.Number ) ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				}
			} else if ( SELECT_CASE_var == OperatiorLogicalOR ) {
				if ( ( Operand( 1 ).Type == ValueNumber ) && ( Operand( 2 ).Type == ValueNumber ) ) {
					if ( ( Operand( 1 ).Number == True.Number ) || ( Operand( 2 ).Number == True.Number ) ) {
						ReturnValue = True;
					} else {
						ReturnValue = False;
					}
				}
			} else if ( SELECT_CASE_var == FuncRound ) {
				ReturnValue = SetErlValueNumber( nint( Operand( 1 ).Number ) );
			} else if ( SELECT_CASE_var == FuncMod ) {
				ReturnValue = SetErlValueNumber( mod( Operand( 1 ).Number, Operand( 2 ).Number ) );
			} else if ( SELECT_CASE_var == FuncSin ) {
				ReturnValue = SetErlValueNumber( std::sin( Operand( 1 ).Number ) );
			} else if ( SELECT_CASE_var == FuncCos ) {
				ReturnValue = SetErlValueNumber( std::cos( Operand( 1 ).Number ) );
			} else if ( SELECT_CASE_var == FuncArcSin ) {
				ReturnValue = SetErlValueNumber( std::asin( Operand( 1 ).Number ) );
			} else if ( SELECT_CASE_var == FuncArcCos ) {
				ReturnValue = SetErlValueNumber( std::acos( Operand( 1 ).Number ) );
			} else if ( SELECT_CASE_var == FuncDegToRad ) {
				ReturnValue = SetErlValueNumber( Operand( 1 ).Number * DegToRadians );
			} else if ( SELECT_CASE_var == FuncRadToDeg ) {
				ReturnValue = SetErlValueNumber( Operand( 1 ).Number / DegToRadians );
			} else if ( SELECT_CASE_var == FuncExp ) {
				if ( Operand( 1 ).Number < 700.0 ) {
					ReturnValue = SetErlValueNumber( std::exp( Operand( 1 ).Number ) );
				} else {
					// throw Error
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Attempted to calculate exponential value of too large a number: " + TrimSigDigits( Operand( 1 ).Number, 4 );
				}
			} else if ( SELECT_CASE_var == FuncLn ) {
				if ( Operand( 1 ).Number > 0.0 ) {
					ReturnValue = SetErlValueNumber( std::log( Operand( 1 ).Number ) );
				} else {
					// throw error,
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Natural Log of zero or less!";
				}
			} else if ( SELECT_CASE_var == FuncMax ) {
				ReturnValue = SetErlValueNumber( max( Operand( 1 ).Number, Operand( 2 ).Number ) );
			} else if ( SELECT_CASE_var == FuncMin ) {
				ReturnValue = SetErlValueNumber( min( Operand( 1 ).Number, Operand( 2 ).Number ) );

			} else if ( SELECT_CASE_var == FuncABS ) {
				ReturnValue = SetErlValueNumber( std::abs( Operand( 1 ).Number ) );
			} else if ( SELECT_CASE_var == FuncRandU ) {
				RANDOM_NUMBER( tmpRANDU1 );
				tmpRANDU1 = Operand( 1 ).Number + ( Operand( 2 ).Number - Operand( 1 ).Number ) * tmpRANDU1;
				ReturnValue = SetErlValueNumber( tmpRANDU1 );
			} else if ( SELECT_CASE_var == FuncRandG ) {
				while ( true ) { // Box-Muller algorithm
					RANDOM_NUMBER( tmpRANDU1 );
					RANDOM_NUMBER( tmpRANDU2 );
					tmpRANDU1 = 2.0 * tmpRANDU1 - 1.0;
					tmpRANDU2 = 2.0 * tmpRANDU2 - 1.0;
					UnitCircleTest = square( tmpRANDU1 ) + square( tmpRANDU2 );
					if ( UnitCircleTest > 0.0 && UnitCircleTest < 1.0 ) break;
				}
				tmpRANDG = std::sqrt( -2.0 * std::log( UnitCircleTest ) / UnitCircleTest );
				tmpRANDG *= tmpRANDU1; // standard normal ran
				//  x     = ran      * sigma             + mean
				tmpRANDG = tmpRANDG * Operand( 2 ).Number + Operand( 1 ).Number;
				tmpRANDG = max( tmpRANDG, Operand( 3 ).Number ); // min limit
				tmpRANDG = min( tmpRANDG, Operand( 4 ).Number ); // max limit
				ReturnValue = SetErlValueNumber( tmpRANDG );
			} else if ( SELECT_CASE_var == FuncRandSeed ) {
				// convert arg to an integer array for the seed.
				RANDOM_SEED( SeedN ); // obtains processor's use size as output
				SeedIntARR.allocate( SeedN );
				for ( loop = 1; loop <= SeedN; ++loop ) {
					if ( loop == 1 ) {
						SeedIntARR( loop ) = std::floor( Operand( 1 ).Number );
					} else {
						SeedIntARR( loop ) = std::floor( Operand( 1 ).Number ) * loop;
					}
				}
				RANDOM_SEED( _, SeedIntARR );
				ReturnValue = SetErlValueNumber( double( SeedIntARR( 1 ) ) ); //just return first number pass as seed
				SeedIntARR.deallocate();
			} else if ( SELECT_CASE_var == FuncRhoAirFnPbTdbW ) {
				ReturnValue = SetErlValueNumber( PsyRhoAirFnPbTdbW( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result =>   density of moist air (kg/m3) | pressure (Pa) | drybulb (C) | Humidity ratio (kg water vapor/kg dry air) | called from
			} else if ( SELECT_CASE_var == FuncCpAirFnWTdb ) {
				ReturnValue = SetErlValueNumber( PsyCpAirFnWTdb( Operand( 1 ).Number, Operand( 2 ).Number ) ); // result =>   heat capacity of air {J/kg-C} | Humidity ratio (kg water vapor/kg dry air) | drybulb (C)
			} else if ( SELECT_CASE_var == FuncHfgAirFnWTdb ) {
				//BG comment these two psych funct seems confusing (?) is this the enthalpy of water in the air?
				ReturnValue = SetErlValueNumber( PsyHfgAirFnWTdb( Operand( 1 ).Number, Operand( 2 ).Number ) ); // result =>   heat of vaporization for moist air {J/kg} | Humidity ratio (kg water vapor/kg dry air) | drybulb (C)
			} else if ( SELECT_CASE_var == FuncHgAirFnWTdb ) {
				// confusing ?  seems like this is really classical Hfg, heat of vaporization
				ReturnValue = SetErlValueNumber( PsyHgAirFnWTdb( Operand( 1 ).Number, Operand( 2 ).Number ) ); // result =>   enthalpy of the gas {units?} | Humidity ratio (kg water vapor/kg dry air) | drybulb (C)
			} else if ( SELECT_CASE_var == FuncTdpFnTdbTwbPb ) {
				ReturnValue = SetErlValueNumber( PsyTdpFnTdbTwbPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result =>   dew-point temperature {C} | drybulb (C) | wetbulb (C) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncTdpFnWPb ) {
				ReturnValue = SetErlValueNumber( PsyTdpFnWPb( Operand( 1 ).Number, Operand( 2 ).Number, EMSBuiltInFunction ) ); // result =>  dew-point temperature {C} | Humidity ratio (kg water vapor/kg dry air) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncHFnTdbW ) {
				ReturnValue = SetErlValueNumber( PsyHFnTdbW( Operand( 1 ).Number, Operand( 2 ).Number ) ); // result =>  enthalpy (J/kg) | drybulb (C) | Humidity ratio (kg water vapor/kg dry air)
			} else if ( SELECT_CASE_var == FuncHFnTdbRhPb ) {
				ReturnValue = SetErlValueNumber( PsyHFnTdbRhPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result =>  enthalpy (J/kg) | drybulb (C) | relative humidity value (0.0 - 1.0) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncTdbFnHW ) {
				ReturnValue = SetErlValueNumber( PsyTdbFnHW( Operand( 1 ).Number, Operand( 2 ).Number ) ); // result =>  dry-bulb temperature {C} | enthalpy (J/kg) | Humidity ratio (kg water vapor/kg dry air)
			} else if ( SELECT_CASE_var == FuncRhovFnTdbRh ) {
				ReturnValue = SetErlValueNumber( PsyRhovFnTdbRh( Operand( 1 ).Number, Operand( 2 ).Number, EMSBuiltInFunction ) ); // result =>  Vapor density in air (kg/m3) | drybulb (C) | relative humidity value (0.0 - 1.0)
			} else if ( SELECT_CASE_var == FuncRhovFnTdbRhLBnd0C ) {
				ReturnValue = SetErlValueNumber( PsyRhovFnTdbRhLBnd0C( Operand( 1 ).Number, Operand( 2 ).Number ) ); // result =>  Vapor density in air (kg/m3) | drybulb (C) | relative humidity value (0.0 - 1.0)
			} else if ( SELECT_CASE_var == FuncRhovFnTdbWPb ) {
				ReturnValue = SetErlValueNumber( PsyRhovFnTdbWPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number ) ); // result =>  Vapor density in air (kg/m3) | drybulb (C) | Humidity ratio (kg water vapor/kg dry air) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncRhFnTdbRhov ) {
				ReturnValue = SetErlValueNumber( PsyRhFnTdbRhov( Operand( 1 ).Number, Operand( 2 ).Number, EMSBuiltInFunction ) ); // result => relative humidity value (0.0-1.0) | drybulb (C) | vapor density in air (kg/m3)
			} else if ( SELECT_CASE_var == FuncRhFnTdbRhovLBnd0C ) {
				ReturnValue = SetErlValueNumber( PsyRhFnTdbRhovLBnd0C( Operand( 1 ).Number, Operand( 2 ).Number, EMSBuiltInFunction ) ); // relative humidity value (0.0-1.0) | drybulb (C) | vapor density in air (kg/m3)
			} else if ( SELECT_CASE_var == FuncRhFnTdbWPb ) {
				ReturnValue = SetErlValueNumber( PsyRhFnTdbWPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result =>  relative humidity value (0.0-1.0) | drybulb (C) | Humidity ratio (kg water vapor/kg dry air) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncTwbFnTdbWPb ) {
				ReturnValue = SetErlValueNumber( PsyTwbFnTdbWPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result=> Temperature Wet-Bulb {C} | drybulb (C) | Humidity ratio (kg water vapor/kg dry air) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncVFnTdbWPb ) {
				ReturnValue = SetErlValueNumber( PsyVFnTdbWPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result=> specific volume {m3/kg} | drybulb (C) | Humidity ratio (kg water vapor/kg dry air) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncWFnTdpPb ) {
				ReturnValue = SetErlValueNumber( PsyWFnTdpPb( Operand( 1 ).Number, Operand( 2 ).Number, EMSBuiltInFunction ) ); // result=> humidity ratio  (kg water vapor/kg dry air) | dew point temperature (C) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncWFnTdbH ) {
				ReturnValue = SetErlValueNumber( PsyWFnTdbH( Operand( 1 ).Number, Operand( 2 ).Number, EMSBuiltInFunction ) ); // result=> humidity ratio  (kg water vapor/kg dry air) | drybulb (C) | enthalpy (J/kg)
			} else if ( SELECT_CASE_var == FuncWFnTdbTwbPb ) {
				ReturnValue = SetErlValueNumber( PsyWFnTdbTwbPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result=> humidity ratio  (kg water vapor/kg dry air) | drybulb (C) | wet-bulb temperature {C} | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncWFnTdbRhPb ) {
				ReturnValue = SetErlValueNumber( PsyWFnTdbRhPb( Operand( 1 ).Number, Operand( 2 ).Number, Operand( 3 ).Number, EMSBuiltInFunction ) ); // result=> humidity ratio  (kg water vapor/kg dry air) | drybulb (C) | relative humidity value (0.0-1.0) | pressure (Pa)
			} else if ( SELECT_CASE_var == FuncPsatFnTemp ) {
				ReturnValue = SetErlValueNumber( PsyPsatFnTemp( Operand( 1 ).Number, EMSBuiltInFunction ) ); // result=> saturation pressure {Pascals} | drybulb (C)
			} else if ( SELECT_CASE_var == FuncTsatFnHPb ) {
				ReturnValue = SetErlValueNumber( PsyTsatFnHPb( Operand( 1 ).Number, Operand( 2 ).Number, EMSBuiltInFunction ) ); // result=> saturation temperature {C} | enthalpy {J/kg} | pressure (Pa)
				//      CASE (FuncTsatFnPb)
				//        ReturnValue = NumberValue( &   ! result=> saturation temperature {C}
				//                        PsyTsatFnPb(Operand(1)%Number, & ! pressure (Pa)
				//                                    'EMS Built-In Function') )
			} else if ( SELECT_CASE_var == FuncCpCW ) {
				ReturnValue = SetErlValueNumber( CPCW( Operand( 1 ).Number ) ); // result => specific heat of water (J/kg-K) = 4180.d0 | temperature (C) unused
			} else if ( SELECT_CASE_var == FuncCpHW ) {
				ReturnValue = SetErlValueNumber( CPHW( Operand( 1 ).Number ) ); // result => specific heat of water (J/kg-K) = 4180.d0 | temperature (C) unused
			} else if ( SELECT_CASE_var == FuncRhoH2O ) {
				ReturnValue = SetErlValueNumber( RhoH2O( Operand( 1 ).Number ) ); // result => density of water (kg/m3) | temperature (C)
			} else if ( SELECT_CASE_var == FuncFatalHaltEp ) {

				ShowSevereError( "EMS user program found serious problem and is halting simulation" );
				ShowContinueErrorTimeStamp( "" );
				ShowFatalError( "EMS user program halted simulation with error code = " + TrimSigDigits( Operand( 1 ).Number, 2 ) );
				ReturnValue = SetErlValueNumber( Operand( 1 ).Number ); // returns back the error code
			} else if ( SELECT_CASE_var == FuncSevereWarnEp ) {

				ShowSevereError( "EMS user program issued severe warning with error code = " + TrimSigDigits( Operand( 1 ).Number, 2 ) );
				ShowContinueErrorTimeStamp( "" );
				ReturnValue = SetErlValueNumber( Operand( 1 ).Number ); // returns back the error code
			} else if ( SELECT_CASE_var == FuncWarnEp ) {

				ShowWarningError( "EMS user program issued warning with error code = " + TrimSigDigits( Operand( 1 ).Number, 2 ) );
				ShowContinueErrorTimeStamp( "" );
				ReturnValue = SetErlValueNumber( Operand( 1 ).Number ); // returns back the error code
			} else if ( SELECT_CASE_var == FuncTrendValue ) {
				// find TrendVariable , first operand is ErlVariable
				if ( Operand( 1 ).TrendVariable ) {
					thisTrend = Operand( 1 ).TrendVarPointer;
					//second operand is number for index
					thisIndex = std::floor( Operand( 2 ).Number );
					if ( thisIndex >= 1 ) {
						if ( thisIndex <= TrendVariable( thisTrend ).LogDepth ) {
							ReturnValue = SetErlValueNumber( TrendVariable( thisTrend ).TrendValARR( thisIndex ), Operand( 1 ) );
						} else {
							ReturnValue.Type = ValueError;
							ReturnValue.Error = "Built-in trend function called with index larger than what is being logged";
						}
					} else {
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Built-in trend function called with index less than 1";
					}
				} else { //not registered as a trend variable
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Variable used with built-in trend function is not associated with a registered trend variable";
				}

			} else if ( SELECT_CASE_var == FuncTrendAverage ) {
				// find TrendVariable , first operand is ErlVariable
				if ( Operand( 1 ).TrendVariable ) {
					thisTrend = Operand( 1 ).TrendVarPointer;
					thisIndex = std::floor( Operand( 2 ).Number );
					if ( thisIndex >= 1 ) {
						if ( thisIndex <= TrendVariable( thisTrend ).LogDepth ) {
							//calculate average
							thisAverage = sum( TrendVariable( thisTrend ).TrendValARR( {1,thisIndex} ) ) / double( thisIndex );
							ReturnValue = SetErlValueNumber( thisAverage, Operand( 1 ) );
						} else {
							ReturnValue.Type = ValueError;
							ReturnValue.Error = "Built-in trend function called with index larger than what is being logged";
						}
					} else {
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Built-in trend function called with index less than 1";
					}
				} else { //not registered as a trend variable
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Variable used with built-in trend function is not associated with a registered trend variable";
				}
			} else if ( SELECT_CASE_var == FuncTrendMax ) {
				if ( Operand( 1 ).TrendVariable ) {
					thisTrend = Operand( 1 ).TrendVarPointer;
					thisIndex = std::floor( Operand( 2 ).Number );
					if ( thisIndex >= 1 ) {
						if ( thisIndex <= TrendVariable( thisTrend ).LogDepth ) {
							thisMax = 0.0;
							if ( thisIndex == 1 ) {
								thisMax = TrendVariable( thisTrend ).TrendValARR( 1 );
							} else {
								for ( loop = 2; loop <= thisIndex; ++loop ) {
									if ( loop == 2 ) {
										thisMax = max( TrendVariable( thisTrend ).TrendValARR( 1 ), TrendVariable( thisTrend ).TrendValARR( 2 ) );
									} else {
										thisMax = max( thisMax, TrendVariable( thisTrend ).TrendValARR( loop ) );
									}
								}
							}
							ReturnValue = SetErlValueNumber( thisMax, Operand( 1 ) );
						} else {
							ReturnValue.Type = ValueError;
							ReturnValue.Error = "Built-in trend function called with index larger than what is being logged";
						}
					} else {
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Built-in trend function called with index less than 1";
					}
				} else { //not registered as a trend variable
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Variable used with built-in trend function is not associated with a registered trend variable";
				}
			} else if ( SELECT_CASE_var == FuncTrendMin ) {
				if ( Operand( 1 ).TrendVariable ) {
					thisTrend = Operand( 1 ).TrendVarPointer;
					thisIndex = std::floor( Operand( 2 ).Number );
					if ( thisIndex >= 1 ) {
						if ( thisIndex <= TrendVariable( thisTrend ).LogDepth ) {
							thisMin = 0.0;
							if ( thisIndex == 1 ) {
								thisMin = TrendVariable( thisTrend ).TrendValARR( 1 );
							} else {
								for ( loop = 2; loop <= thisIndex; ++loop ) {
									if ( loop == 2 ) {
										thisMin = min( TrendVariable( thisTrend ).TrendValARR( 1 ), TrendVariable( thisTrend ).TrendValARR( 2 ) );
									} else {
										thisMin = min( thisMin, TrendVariable( thisTrend ).TrendValARR( loop ) );
									}
								}
							}
							ReturnValue = SetErlValueNumber( thisMin, Operand( 1 ) );

						} else {
							ReturnValue.Type = ValueError;
							ReturnValue.Error = "Built-in trend function called with index larger than what is being logged";
						}

					} else {
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Built-in trend function called with index less than 1";
					}
				} else { //not registered as a trend variable
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Variable used with built-in trend function is not associated with a registered trend variable";
				}
			} else if ( SELECT_CASE_var == FuncTrendDirection ) {
				if ( Operand( 1 ).TrendVariable ) {
					// do a linear least squares fit and get slope of line
					thisTrend = Operand( 1 ).TrendVarPointer;
					thisIndex = std::floor( Operand( 2 ).Number );
					if ( thisIndex >= 1 ) {

						if ( thisIndex <= TrendVariable( thisTrend ).LogDepth ) {
							// closed form solution for slope of linear least squares fit
							thisSlope = ( sum( TrendVariable( thisTrend ).TimeARR( {1,thisIndex} ) ) * sum( TrendVariable( thisTrend ).TrendValARR( {1,thisIndex} ) ) - thisIndex * sum( ( TrendVariable( thisTrend ).TimeARR( {1,thisIndex} ) * TrendVariable( thisTrend ).TrendValARR( {1,thisIndex} ) ) ) ) / ( pow_2( sum( TrendVariable( thisTrend ).TimeARR( {1,thisIndex} ) ) ) - thisIndex * sum( pow( TrendVariable( thisTrend ).TimeARR( {1,thisIndex} ), 2 ) ) );
							ReturnValue = SetErlValueNumber( thisSlope, Operand( 1 ) ); // rate of change per hour
						} else {
							ReturnValue.Type = ValueError;
							ReturnValue.Error = "Built-in trend function called with index larger than what is being logged";
						}

					} else {
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Built-in trend function called with index less than 1";
					}
				} else { //not registered as a trend variable
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Variable used with built-in trend function is not associated with a registered trend variable";
				}
			} else if ( SELECT_CASE_var == FuncTrendSum ) {
				if ( Operand( 1 ).TrendVariable ) {

					thisTrend = Operand( 1 ).TrendVarPointer;
					thisIndex = std::floor( Operand( 2 ).Number );
					if ( thisIndex >= 1 ) {
						if ( thisIndex <= TrendVariable( thisTrend ).LogDepth ) {
							ReturnValue = SetErlValueNumber( sum( TrendVariable( thisTrend ).TrendValARR( {1,thisIndex} ) ), Operand( 1 ) );
						} else {
							ReturnValue.Type = ValueError;
							ReturnValue.Error = "Built-in trend function called with index larger than what is being logged";
						}
					} else {
						ReturnValue.Type = ValueError;
						ReturnValue.Error = "Built-in trend function called with index less than 1";
					}
				} else { //not registered as a trend variable
					ReturnValue.Type = ValueError;
					ReturnValue.Error = "Variable used with built-in trend function is not associated with a registered trend variable";
				}
			} else if ( SELECT_CASE_var == FuncCurveValue ) {
				ReturnValue = SetErlValueNumber( CurveValue( std::floor( Operand( 1 ).Number ), Operand( 2 ).Number, Operand( 3 ).Number, Operand( 4 ).Number, Operand( 5 ).Number, Operand( 6 ).Number ) ); // curve index | X value | Y value, 2nd independent | Z Value, 3rd independent | 4th independent | 5th independent

			} else {
				// throw Error!
				ShowFatalError( "caught unexpected Expression(ExpressionNum)%Operator in EvaluateExpression" );
			}}
			Operand.deallocate();
		}

		return ReturnValue;

	}

	void
	GetRuntimeLanguageUserInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       Brent Griffith April 2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the runtime language objects from the input file.
		// GetInput is called from other modules that reference runtime language objects.
		// The runtime language objects are all loaded in one pass

		// METHODOLOGY EMPLOYED:
		// The runtime language objects are all loaded in one step, names registered, etc.  They are parsed in a second step
		// once all the object names are known.

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;

		using General::TrimSigDigits;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using DataHeatBalance::Construct;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetRuntimeLanguageUserInput: " );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int GlobalNum;
		int StackNum;
		//unused0909  INTEGER    :: NumPrograms
		//unused0909  INTEGER    :: NumFunctions
		int ErrorNum;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool ErrorsFound( false );
		int VariableNum( 0 ); // temporary
		int RuntimeReportVarNum;
		//unused0909  INTEGER    :: Pos
		//unused0909  CHARACTER(len=MaxNameLength) :: VariableName
		bool Found;
		static std::string FreqString; // temporary
		static std::string VarTypeString; // temporary
		static std::string ResourceTypeString;
		static std::string GroupTypeString;
		static std::string EndUseTypeString;
		static std::string EndUseSubCatString;

		int TrendNum;
		int NumTrendSteps;
		int loop;
		int ErlVarLoop;
		int CurveIndexNum;
		static int MaxNumAlphas( 0 ); // argument for call to GetObjectDefMaxArgs
		static int MaxNumNumbers( 0 ); // argument for call to GetObjectDefMaxArgs
		static int TotalArgs( 0 ); // argument for call to GetObjectDefMaxArgs
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		int ConstructNum;
		bool errFlag;
		std::string::size_type lbracket;
		std::string UnitsA;
		std::string UnitsB;
		std::string::size_type ptr;

		// FLOW:
		if ( GetInput ) { // GetInput check is redundant with the InitializeRuntimeLanguage routine
			GetInput = false;

			cCurrentModuleObject = "EnergyManagementSystem:Sensor";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = NumNums;
			MaxNumAlphas = NumAlphas;
			cCurrentModuleObject = "EnergyManagementSystem:Actuator";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "EnergyManagementSystem:Program";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "EnergyManagementSystem:Subroutine";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "EnergyManagementSystem:OutputVariable";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "ExternalInterface:Variable";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "ExternalInterface:Actuator";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			//  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
			//  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
			//  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
			//  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
			cCurrentModuleObject = "EnergyManagementSystem:GlobalVariable";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "EnergyManagementSystem:CurveOrTableIndexVariable";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
			cCurrentModuleObject = "EnergyManagementSystem:ConstructionIndexVariable";
			GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
			MaxNumNumbers = max( MaxNumNumbers, NumNums );
			MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

			cAlphaFieldNames.allocate( MaxNumAlphas );
			cAlphaArgs.allocate( MaxNumAlphas );
			lAlphaFieldBlanks.dimension( MaxNumAlphas, false );
			cNumericFieldNames.allocate( MaxNumNumbers );
			rNumericArgs.dimension( MaxNumNumbers, 0.0 );
			lNumericFieldBlanks.dimension( MaxNumNumbers, false );

			cCurrentModuleObject = "EnergyManagementSystem:GlobalVariable";

			if ( NumUserGlobalVariables + NumExternalInterfaceGlobalVariables > 0 ) {
				for ( GlobalNum = 1; GlobalNum <= NumUserGlobalVariables + NumExternalInterfaceGlobalVariables; ++GlobalNum ) {
					// If we process the ExternalInterface actuators, all we need to do is to change the
					// name of the module object, and add an offset for the variable number
					// This is done in the following IF/THEN section.
					if ( GlobalNum <= NumUserGlobalVariables ) {
						GetObjectItem( cCurrentModuleObject, GlobalNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					} else {
						cCurrentModuleObject = "ExternalInterface:Variable";
						GetObjectItem( cCurrentModuleObject, GlobalNum - NumUserGlobalVariables, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					}

					// loop over each alpha and register variable named as global Erl variable
					for ( ErlVarLoop = 1; ErlVarLoop <= NumAlphas; ++ErlVarLoop ) {
						ValidateEMSVariableName( cCurrentModuleObject, cAlphaArgs( ErlVarLoop ), cAlphaFieldNames( ErlVarLoop ), errFlag, ErrorsFound );
						if ( lAlphaFieldBlanks( ErlVarLoop ) ) {
							ShowWarningError( RoutineName + cCurrentModuleObject );
							ShowContinueError( "Blank " + cAlphaFieldNames( 1 ) );
							ShowContinueError( "Blank entry will be skipped, and the simulation continues" );
						} else if ( ! errFlag ) {
							VariableNum = FindEMSVariable( cAlphaArgs( ErlVarLoop ), 0 );
							// Still need to check for conflicts with program and function names too

							if ( VariableNum > 0 ) {
								ShowSevereError( RoutineName + cCurrentModuleObject + ", invalid entry." );
								ShowContinueError( "Invalid " + cAlphaFieldNames( ErlVarLoop ) + '=' + cAlphaArgs( ErlVarLoop ) );
								ShowContinueError( "Name conflicts with an existing global variable name" );
								ErrorsFound = true;
							} else {
								VariableNum = NewEMSVariable( cAlphaArgs( ErlVarLoop ), 0 );
								if ( GlobalNum > NumUserGlobalVariables ) {
									// Initialize variables for the ExternalInterface variables.
									// This object requires an initial value.
									ExternalInterfaceInitializeErlVariable( VariableNum, SetErlValueNumber( rNumericArgs( 1 ) ), false );
								}

							}
						}
					}
				}
			}

			cCurrentModuleObject = "EnergyManagementSystem:CurveOrTableIndexVariable";
			NumEMSCurveIndices = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumEMSCurveIndices > 0 ) {
				CurveIndexVariableNums.dimension( NumEMSCurveIndices, 0 );
				for ( loop = 1; loop <= NumEMSCurveIndices; ++loop ) {
					GetObjectItem( cCurrentModuleObject, loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					// check if variable name is unique and well formed
					ValidateEMSVariableName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), errFlag, ErrorsFound );
					if ( lAlphaFieldBlanks( 1 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject );
						ShowContinueError( "Blank " + cAlphaFieldNames( 1 ) );
						ShowContinueError( "Blank entry for Erl variable name is not allowed" );
						ErrorsFound = true;
					} else if ( ! errFlag ) {
						VariableNum = FindEMSVariable( cAlphaArgs( 1 ), 0 );
						if ( VariableNum > 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 1 ) );
							ShowContinueError( "Name conflicts with an existing variable name" );
							ErrorsFound = true;
						} else {
							// create new EMS variable
							VariableNum = NewEMSVariable( cAlphaArgs( 1 ), 0 );
							// store variable num
							CurveIndexVariableNums( loop ) = VariableNum;
						}
					}

					CurveIndexNum = GetCurveIndex( cAlphaArgs( 2 ) ); // curve name
					if ( CurveIndexNum == 0 ) {
						if ( lAlphaFieldBlanks( 2 ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " blank field." );
							ShowContinueError( "Blank " + cAlphaFieldNames( 2 ) );
							ShowContinueError( "Blank entry for curve or table name is not allowed" );
						} else {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
							ShowContinueError( "Curve or table was not found." );
						}
						ErrorsFound = true;
					} else {
						// fill Erl variable with curve index
						ErlVariable( VariableNum ).Value = SetErlValueNumber( double( CurveIndexNum ) );
					}
				}

			} // NumEMSCurveIndices > 0

			cCurrentModuleObject = "EnergyManagementSystem:ConstructionIndexVariable";
			NumEMSConstructionIndices = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumEMSConstructionIndices > 0 ) {
				ConstructionIndexVariableNums.dimension( NumEMSConstructionIndices, 0 );
				for ( loop = 1; loop <= NumEMSConstructionIndices; ++loop ) {
					GetObjectItem( cCurrentModuleObject, loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					// check if variable name is unique and well formed
					ValidateEMSVariableName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), errFlag, ErrorsFound );
					if ( lAlphaFieldBlanks( 1 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject );
						ShowContinueError( "Blank " + cAlphaFieldNames( 1 ) );
						ShowContinueError( "Blank entry for Erl variable name is not allowed" );
						ErrorsFound = true;
					} else if ( ! errFlag ) {
						VariableNum = FindEMSVariable( cAlphaArgs( 1 ), 0 );
						if ( VariableNum > 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 1 ) );
							ShowContinueError( "Name conflicts with an existing variable name" );
							ErrorsFound = true;
						} else {
							// create new EMS variable
							VariableNum = NewEMSVariable( cAlphaArgs( 1 ), 0 );
							// store variable num
							ConstructionIndexVariableNums( loop ) = VariableNum;
						}
					} else {
						continue;
					}

					ConstructNum = FindItemInList( cAlphaArgs( 2 ), Construct );

					if ( ConstructNum == 0 ) {
						if ( lAlphaFieldBlanks( 2 ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " blank field." );
							ShowContinueError( "Blank " + cAlphaFieldNames( 2 ) );
							ShowContinueError( "Blank entry for construction name is not allowed" );
						} else {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
							ShowContinueError( "Construction was not found." );
						}
						ErrorsFound = true;
					} else {
						// fill Erl variable with curve index
						ErlVariable( VariableNum ).Value = SetErlValueNumber( double( ConstructNum ) );
					}
				}

			} // NumEMSConstructionIndices > 0

			NumErlStacks = NumErlPrograms + NumErlSubroutines;
			ErlStack.allocate( NumErlStacks );

			if ( NumErlPrograms > 0 ) {
				cCurrentModuleObject = "EnergyManagementSystem:Program";
				for ( StackNum = 1; StackNum <= NumErlPrograms; ++StackNum ) {
					GetObjectItem( cCurrentModuleObject, StackNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), ErlStack, StackNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}

					ValidateEMSProgramName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), "Programs", errFlag, ErrorsFound );
					if ( ! errFlag ) {
						ErlStack( StackNum ).Name = cAlphaArgs( 1 );
					}

					if ( NumAlphas > 1 ) {
						ErlStack( StackNum ).Line.allocate( NumAlphas - 1 );
						ErlStack( StackNum ).NumLines = NumAlphas - 1;
						ErlStack( StackNum ).Line( {1,NumAlphas - 1} ) = cAlphaArgs( {2,NumAlphas} ); // note array assignment
					}

				} // ProgramNum
			}

			if ( NumErlSubroutines > 0 ) {
				cCurrentModuleObject = "EnergyManagementSystem:Subroutine";
				for ( StackNum = NumErlPrograms + 1; StackNum <= NumErlStacks; ++StackNum ) {
					GetObjectItem( cCurrentModuleObject, StackNum - NumErlPrograms, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), ErlStack, StackNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}

					ValidateEMSProgramName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), "Subroutines", errFlag, ErrorsFound );
					if ( ! errFlag ) {
						ErlStack( StackNum ).Name = cAlphaArgs( 1 );
					}

					if ( NumAlphas > 1 ) {
						ErlStack( StackNum ).Line.allocate( NumAlphas - 1 );
						ErlStack( StackNum ).NumLines = NumAlphas - 1;
						ErlStack( StackNum ).Line( {1,NumAlphas - 1} ) = cAlphaArgs( {2,NumAlphas} ); // note array assignment
					}

				}
			}

			cCurrentModuleObject = "EnergyManagementSystem:TrendVariable";
			NumErlTrendVariables = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumErlTrendVariables > 0 ) {
				TrendVariable.allocate( NumErlTrendVariables );
				for ( TrendNum = 1; TrendNum <= NumErlTrendVariables; ++TrendNum ) {
					GetObjectItem( cCurrentModuleObject, TrendNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), TrendVariable, TrendNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}

					ValidateEMSVariableName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), errFlag, ErrorsFound );
					if ( ! errFlag ) {
						TrendVariable( TrendNum ).Name = cAlphaArgs( 1 );
					}

					VariableNum = FindEMSVariable( cAlphaArgs( 2 ), 0 );
					// Still need to check for conflicts with program and function names too
					if ( VariableNum == 0 ) { //did not find it
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
						ShowContinueError( "Did not find a match with an EMS variable name" );
						ErrorsFound = true;
					} else { // found it.
						TrendVariable( TrendNum ).ErlVariablePointer = VariableNum;
						// register the trend pointer in ErlVariable.
						ErlVariable( VariableNum ).Value.TrendVariable = true;
						ErlVariable( VariableNum ).Value.TrendVarPointer = TrendNum;
					}

					NumTrendSteps = std::floor( rNumericArgs( 1 ) );
					if ( NumTrendSteps > 0 ) {
						TrendVariable( TrendNum ).LogDepth = NumTrendSteps;
						//setup data arrays using NumTrendSteps
						TrendVariable( TrendNum ).TrendValARR.allocate( NumTrendSteps );
						TrendVariable( TrendNum ).TrendValARR = 0.0; // array init
						TrendVariable( TrendNum ).tempTrendARR.allocate( NumTrendSteps );
						TrendVariable( TrendNum ).tempTrendARR = 0.0; // array init
						TrendVariable( TrendNum ).TimeARR.allocate( NumTrendSteps );
						//construct time data array for use with other calculations later
						// current time is zero, each value in trend log array is one zone timestep further back in time
						// units are hours.  all terms negative, getting increasingly negative the further back in time
						//  further back in time is higher index in array
						for ( loop = 1; loop <= NumTrendSteps; ++loop ) {
							if ( loop == 1 ) {
								TrendVariable( TrendNum ).TimeARR( loop ) = -TimeStepZone;
								continue;
							} else {
								TrendVariable( TrendNum ).TimeARR( loop ) = TrendVariable( TrendNum ).TimeARR( loop - 1 ) - TimeStepZone; // fractional hours
							}
						}
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cNumericFieldNames( 1 ) + '=' + TrimSigDigits( rNumericArgs( 1 ), 2 ) );
						ShowContinueError( "must be greater than zero" );
						ErrorsFound = true;
					}

				} // trendnum
			}

			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in getting EMS Runtime Language input. Preceding condition causes termination." );
			}

			// Parse the runtime language code
			for ( StackNum = 1; StackNum <= NumErlStacks; ++StackNum ) {
				ParseStack( StackNum );

				if ( ErlStack( StackNum ).NumErrors > 0 ) {
					ShowSevereError( "Errors found parsing EMS Runtime Language program or subroutine = " + ErlStack( StackNum ).Name );
					for ( ErrorNum = 1; ErrorNum <= ErlStack( StackNum ).NumErrors; ++ErrorNum ) {
						ShowContinueError( ErlStack( StackNum ).Error( ErrorNum ) );
					}
					ErrorsFound = true;
				}
			} // StackNum

			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in parsing EMS Runtime Language input. Preceding condition causes termination." );
			}

			if ( ( NumEMSOutputVariables > 0 ) || ( NumEMSMeteredOutputVariables > 0 ) ) {
				RuntimeReportVar.allocate( NumEMSOutputVariables + NumEMSMeteredOutputVariables );
			}

			if ( NumEMSOutputVariables > 0 ) {
				cCurrentModuleObject = "EnergyManagementSystem:OutputVariable";
				for ( RuntimeReportVarNum = 1; RuntimeReportVarNum <= NumEMSOutputVariables; ++RuntimeReportVarNum ) {
					GetObjectItem( cCurrentModuleObject, RuntimeReportVarNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), RuntimeReportVar, RuntimeReportVarNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}

					lbracket = index( cAlphaArgs( 1 ), '[' );
					if ( lbracket == std::string::npos ) {
						UnitsA = "";
						//          if (lAlphaFieldBlanks(6)) then
						//            CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//' no units indicated.')
						//            CALL ShowContinueError('...no units indicated for this variable. [] is assumed.')
						//            cAlphaArgs(1)=TRIM(cAlphaArgs(1))//' []'
						//          endif
						UnitsB = cAlphaArgs( 6 );
						lbracket = index( UnitsB, '[' );
						ptr = index( UnitsB, ']' );
						if ( lbracket != std::string::npos ) {
							UnitsB[ lbracket ] = ' ';
							if ( ptr != std::string::npos ) {
								UnitsB[ ptr ] = ' ';
							}
							strip( UnitsB );
						}
					} else { // units shown on Name field (7.2 and pre versions)
						ptr = index( cAlphaArgs( 1 ), ']' );
						if ( ptr != std::string::npos ) {
							UnitsA = cAlphaArgs( 1 ).substr( lbracket + 1, ptr - lbracket - 1 );
						} else {
							UnitsA = cAlphaArgs( 1 ).substr( lbracket + 1 );
						}
						cAlphaArgs( 1 ).erase( lbracket - 1 );
						UnitsB = cAlphaArgs( 6 );
						lbracket = index( UnitsB, '[' );
						ptr = index( UnitsB, ']' );
						if ( lbracket != std::string::npos ) {
							UnitsB[ lbracket ] = ' ';
							if ( ptr != std::string::npos ) {
								UnitsB[ ptr ] = ' ';
							}
							strip( UnitsB );
						}
						if ( UnitsA != "" && UnitsB != "" ) {
							if ( UnitsA != UnitsB ) {
								ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " mismatched units." );
								ShowContinueError( "...Units entered in " + cAlphaFieldNames( 1 ) + " (deprecated use)=\"" + UnitsA + "\"" );
								ShowContinueError( "..." + cAlphaFieldNames( 6 ) + "=\"" + UnitsB + "\" (will be used)" );
							}
						} else if ( UnitsB == "" && UnitsA != "" ) {
							UnitsB = UnitsA;
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " using deprecated units designation." );
							ShowContinueError( "...Units entered in " + cAlphaFieldNames( 1 ) + " (deprecated use)=\"" + UnitsA + "\"" );
						}
					}
					cAlphaArgs( 1 ) += " [" + UnitsB + ']';

					RuntimeReportVar( RuntimeReportVarNum ).Name = cAlphaArgs( 1 );

					if ( ! lAlphaFieldBlanks( 5 ) ) {
						// Lookup the Runtime Language Context, i.e., PROGRAM, FUNCTION, or global
						Found = false;
						for ( StackNum = 1; StackNum <= NumErlStacks; ++StackNum ) {
							if ( ErlStack( StackNum ).Name == cAlphaArgs( 5 ) ) {
								Found = true;
								break;
							}
						}
						if ( ! Found ) {
							StackNum = 0;
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
							ShowContinueError( "EMS program or subroutine not found." );
							ErrorsFound = true;
						}
					} else {
						StackNum = 0;
					}

					VariableNum = FindEMSVariable( cAlphaArgs( 2 ), StackNum );

					if ( VariableNum == 0 ) {
						if ( lAlphaFieldBlanks( 5 ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
							ShowContinueError( "EMS variable not found among global variables." );
						} else if ( StackNum != 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
							ShowContinueError( "EMS variable not found among local variables in " + cAlphaArgs( 5 ) );
						}
						ErrorsFound = true;
						//        ELSEIF (INDEX('0123456789',cAlphaArgs(2)(1:1)) > 0) THEN
						//            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
						//            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
						//            CALL ShowContinueError('Names used as Erl output variables cannot start with numeric characters.')
						//            ErrorsFound = .TRUE.
					} else {
						RuntimeReportVar( RuntimeReportVarNum ).VariableNum = VariableNum;
					}

					{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );

					if ( SELECT_CASE_var == "AVERAGED" ) {
						VarTypeString = "Average";
					} else if ( SELECT_CASE_var == "SUMMED" ) {
						VarTypeString = "Sum";
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "...valid values are Averaged or Summed." );
						ErrorsFound = true;
					}}

					{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) );

					if ( SELECT_CASE_var == "ZONETIMESTEP" ) {
						FreqString = "Zone";
					} else if ( SELECT_CASE_var == "SYSTEMTIMESTEP" ) {
						FreqString = "System";
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "...valid values are ZoneTimestep or SystemTimestep." );
						ErrorsFound = true;
					}}

					SetupOutputVariable( cAlphaArgs( 1 ), RuntimeReportVar( RuntimeReportVarNum ).Value, FreqString, VarTypeString, "EMS" );
					// Last field is index key, no indexing here so mimic weather output data

				} // RuntimeReportVarNum
			} // NumEMSOutputVariables > 0

			if ( NumEMSMeteredOutputVariables > 0 ) {
				cCurrentModuleObject = "EnergyManagementSystem:MeteredOutputVariable";
				for ( loop = 1; loop <= NumEMSMeteredOutputVariables; ++loop ) {
					RuntimeReportVarNum = NumEMSOutputVariables + loop;
					GetObjectItem( cCurrentModuleObject, loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), RuntimeReportVar, RuntimeReportVarNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}

					lbracket = index( cAlphaArgs( 1 ), '[' );
					if ( lbracket == std::string::npos ) {
						UnitsA = "";
						//          if (lAlphaFieldBlanks(9)) then
						//            CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//' no units indicated.')
						//            CALL ShowContinueError('...no units indicated for this variable. [] is assumed.')
						//            cAlphaArgs(1)=TRIM(cAlphaArgs(1))//' []'
						//          endif
						UnitsB = cAlphaArgs( 9 );
						lbracket = index( UnitsB, '[' );
						ptr = index( UnitsB, ']' );
						if ( lbracket != std::string::npos ) {
							UnitsB[ lbracket ] = ' ';
							if ( ptr != std::string::npos ) {
								UnitsB[ ptr ] = ' ';
							}
							strip( UnitsB );
						}
					} else { // units shown on Name field (7.2 and pre versions)
						ptr = index( cAlphaArgs( 1 ), ']' );
						if ( ptr != std::string::npos ) {
							UnitsA = cAlphaArgs( 1 ).substr( lbracket + 1, ptr - lbracket - 1 );
						} else {
							UnitsA = cAlphaArgs( 1 ).substr( lbracket + 1 );
						}
						cAlphaArgs( 1 ).erase( lbracket - 1 );
						UnitsB = cAlphaArgs( 9 );
						lbracket = index( UnitsB, '[' );
						ptr = index( UnitsB, ']' );
						if ( lbracket != std::string::npos ) {
							UnitsB[ lbracket ] = ' ';
							if ( ptr != std::string::npos ) {
								UnitsB[ ptr ] = ' ';
							}
							strip( UnitsB );
						}
						if ( UnitsA != "" && UnitsB != "" ) {
							if ( UnitsA != UnitsB ) {
								ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " mismatched units." );
								ShowContinueError( "...Units entered in " + cAlphaFieldNames( 1 ) + " (deprecated use)=\"" + UnitsA + "\"" );
								ShowContinueError( "..." + cAlphaFieldNames( 9 ) + "=\"" + UnitsB + "\" (will be used)" );
							}
						} else if ( UnitsB == "" && UnitsA != "" ) {
							UnitsB = UnitsA;
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " using deprecated units designation." );
							ShowContinueError( "...Units entered in " + cAlphaFieldNames( 1 ) + " (deprecated use)=\"" + UnitsA + "\"" );
						}
					}
					cAlphaArgs( 1 ) += " [" + UnitsB + ']';

					RuntimeReportVar( RuntimeReportVarNum ).Name = cAlphaArgs( 1 );

					if ( ! lAlphaFieldBlanks( 4 ) ) {
						// Lookup the Runtime Language Context, i.e., PROGRAM, FUNCTION, or global
						Found = false;
						for ( StackNum = 1; StackNum <= NumErlStacks; ++StackNum ) {
							if ( ErlStack( StackNum ).Name == cAlphaArgs( 4 ) ) {
								Found = true;
								break;
							}
						}
						if ( ! Found ) {
							StackNum = 0;
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
							ShowContinueError( "EMS program or subroutine not found." );
							ErrorsFound = true;
						}
					} else {
						StackNum = 0;
					}

					VariableNum = FindEMSVariable( cAlphaArgs( 2 ), StackNum );
					if ( VariableNum == 0 ) {
						if ( lAlphaFieldBlanks( 4 ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
							ShowContinueError( "EMS variable not found among global variables." );
						} else if ( StackNum != 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
							ShowContinueError( "EMS variable not found among local variables in " + cAlphaArgs( 5 ) );
						}
						ErrorsFound = true;
						//        ELSEIF (INDEX('0123456789',cAlphaArgs(2)(1:1)) > 0) THEN
						//            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
						//            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
						//            CALL ShowContinueError('Names used as Erl output variables cannot start with numeric characters.')
						//            ErrorsFound = .TRUE.
					} else {
						RuntimeReportVar( RuntimeReportVarNum ).VariableNum = VariableNum;
					}

					VarTypeString = "Sum"; // all metered vars are sum type

					{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );

					if ( SELECT_CASE_var == "ZONETIMESTEP" ) {
						FreqString = "Zone";
					} else if ( SELECT_CASE_var == "SYSTEMTIMESTEP" ) {
						FreqString = "System";
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "...valid values are ZoneTimestep or SystemTimestep." );
						ErrorsFound = true;
					}}

					//Resource Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 5 ) );

					if ( SELECT_CASE_var == "ELECTRICITY" ) {
						ResourceTypeString = "Electricity";
					} else if ( SELECT_CASE_var == "NATURALGAS" ) {
						ResourceTypeString = "NaturalGas";
					} else if ( SELECT_CASE_var == "GASOLINE" ) {
						ResourceTypeString = "Gasoline";
					} else if ( SELECT_CASE_var == "DIESEL" ) {
						ResourceTypeString = "Diesel";
					} else if ( SELECT_CASE_var == "COAL" ) {
						ResourceTypeString = "Coal";
					} else if ( SELECT_CASE_var == "FUELOIL#1" ) {
						ResourceTypeString = "FuelOil#1";
					} else if ( SELECT_CASE_var == "FUELOIL#2" ) {
						ResourceTypeString = "FuelOil#2";
					} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
						ResourceTypeString = "OtherFuel1";
					} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
						ResourceTypeString = "OtherFuel2";
					} else if ( SELECT_CASE_var == "PROPANE" ) {
						ResourceTypeString = "Propane";
					} else if ( SELECT_CASE_var == "WATERUSE" ) {
						ResourceTypeString = "Water";
					} else if ( SELECT_CASE_var == "ONSITEWATERPRODUCED" ) {
						ResourceTypeString = "OnSiteWater";
					} else if ( SELECT_CASE_var == "MAINSWATERSUPPLY" ) {
						ResourceTypeString = "MainsWater";
					} else if ( SELECT_CASE_var == "RAINWATERCOLLECTED" ) {
						ResourceTypeString = "RainWater";
					} else if ( SELECT_CASE_var == "WELLWATERDRAWN" ) {
						ResourceTypeString = "WellWater";
					} else if ( SELECT_CASE_var == "CONDENSATEWATERCOLLECTED" ) {
						ResourceTypeString = "Condensate";
					} else if ( SELECT_CASE_var == "ENERGYTRANSFER" ) {
						ResourceTypeString = "EnergyTransfer";
					} else if ( SELECT_CASE_var == "STEAM" ) {
						ResourceTypeString = "Steam";
					} else if ( SELECT_CASE_var == "DISTRICTCOOLING" ) {
						ResourceTypeString = "DistrictCooling";
					} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
						ResourceTypeString = "DistrictHeating";
					} else if ( SELECT_CASE_var == "ELECTRICITYPRODUCEDONSITE" ) {
						ResourceTypeString = "ElectricityProduced";
					} else if ( SELECT_CASE_var == "SOLARWATERHEATING" ) {
						ResourceTypeString = "SolarWater";
					} else if ( SELECT_CASE_var == "SOLARAIRHEATING" ) {
						ResourceTypeString = "SolarAir";
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ErrorsFound = true;
					}}

					//Group Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 6 ) );

					if ( SELECT_CASE_var == "BUILDING" ) {
						GroupTypeString = "Building";
					} else if ( SELECT_CASE_var == "HVAC" ) {
						GroupTypeString = "HVAC";
					} else if ( SELECT_CASE_var == "PLANT" ) {
						GroupTypeString = "Plant";
					} else if (SELECT_CASE_var == "SYSTEM") {
						GroupTypeString = "System";
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ErrorsFound = true;
					}}

					//End Use Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 7 ) );

					if ( SELECT_CASE_var == "HEATING" ) {
						EndUseTypeString = "Heating";
					} else if ( SELECT_CASE_var == "COOLING" ) {
						EndUseTypeString = "Cooling";
					} else if ( SELECT_CASE_var == "INTERIORLIGHTS" ) {
						EndUseTypeString = "InteriorLights";
					} else if ( SELECT_CASE_var == "EXTERIORLIGHTS" ) {
						EndUseTypeString = "ExteriorLights";
					} else if ( SELECT_CASE_var == "INTERIOREQUIPMENT" ) {
						EndUseTypeString = "InteriorEquipment";
					} else if ( SELECT_CASE_var == "EXTERIOREQUIPMENT" ) {
						EndUseTypeString = "ExteriorEquipment";
					} else if ( SELECT_CASE_var == "FANS" ) {
						EndUseTypeString = "Fans";
					} else if ( SELECT_CASE_var == "PUMPS" ) {
						EndUseTypeString = "Pumps";
					} else if ( SELECT_CASE_var == "HEATREJECTION" ) {
						EndUseTypeString = "HeatRejection";
					} else if ( SELECT_CASE_var == "HUMIDIFIER" ) {
						EndUseTypeString = "Humidifier";
					} else if ( SELECT_CASE_var == "HEATRECOVERY" ) {
						EndUseTypeString = "HeatRecovery";
					} else if ( SELECT_CASE_var == "WATERSYSTEMS" ) {
						EndUseTypeString = "WaterSystems";
					} else if ( SELECT_CASE_var == "REFRIGERATION" ) {
						EndUseTypeString = "Refrigeration";
					} else if ( SELECT_CASE_var == "ONSITEGENERATION" ) {
						EndUseTypeString = "Cogeneration";
					} else if ( SELECT_CASE_var == "HEATINGCOILS" ) {
						EndUseTypeString = "HeatingCoils";
					} else if ( SELECT_CASE_var == "COOLINGCOILS" ) {
						EndUseTypeString = "CoolingCoils";
					} else if ( SELECT_CASE_var == "CHILLERS" ) {
						EndUseTypeString = "Chillers";
					} else if ( SELECT_CASE_var == "BOILERS" ) {
						EndUseTypeString = "Boilers";
					} else if ( SELECT_CASE_var == "BASEBOARD" ) {
						EndUseTypeString = "Baseboard";
					} else if ( SELECT_CASE_var == "HEATRECOVERYFORCOOLING" ) {
						EndUseTypeString = "HeatRecoveryForCooling";
					} else if ( SELECT_CASE_var == "HEATRECOVERYFORHEATING" ) {
						EndUseTypeString = "HeatRecoveryForHeating";
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ErrorsFound = true;
					}}
					
					//Additional End Use Types Only Used for EnergyTransfer
					if ( ( ResourceTypeString != "EnergyTransfer" ) && ( EndUseTypeString == "HeatingCoils" || EndUseTypeString == "CoolingCoils" || EndUseTypeString == "Chillers" || EndUseTypeString == "Boilers" || EndUseTypeString == "Baseboard" || EndUseTypeString == "HeatRecoveryForCooling" || EndUseTypeString == "HeatRecoveryForHeating" ) ) {
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid field." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + "=" + cAlphaArgs( 5 ) + " for " + cAlphaFieldNames( 7 ) + "=" + cAlphaArgs( 7 ) );
						ShowContinueError( "Field " + cAlphaFieldNames( 5 ) + " is reset from " + cAlphaArgs( 5 ) + " to EnergyTransfer" );
						ResourceTypeString = "EnergyTransfer";
					}

					if ( ! lAlphaFieldBlanks( 8 ) ) {
						EndUseSubCatString = cAlphaArgs( 8 );

						SetupOutputVariable( cAlphaArgs( 1 ), RuntimeReportVar( RuntimeReportVarNum ).Value, FreqString, VarTypeString, "EMS", _, ResourceTypeString, EndUseTypeString, EndUseSubCatString, GroupTypeString );
					} else { // no subcat
						SetupOutputVariable( cAlphaArgs( 1 ), RuntimeReportVar( RuntimeReportVarNum ).Value, FreqString, VarTypeString, "EMS", _, ResourceTypeString, EndUseTypeString, _, GroupTypeString );
					}

				}
			} // NumEMSMeteredOutputVariables > 0

			cAlphaFieldNames.deallocate();
			cAlphaArgs.deallocate();
			lAlphaFieldBlanks.deallocate();
			cNumericFieldNames.deallocate();
			rNumericArgs.deallocate();
			lNumericFieldBlanks.deallocate();

			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in getting EMS Runtime Language input. Preceding condition causes termination." );
			}

		} // GetInput

	}

	void
	ReportRuntimeLanguage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RuntimeReportVarNum;
		int VariableNum;

		// FLOW:
		for ( RuntimeReportVarNum = 1; RuntimeReportVarNum <= NumEMSOutputVariables + NumEMSMeteredOutputVariables; ++RuntimeReportVarNum ) {
			VariableNum = RuntimeReportVar( RuntimeReportVarNum ).VariableNum;
			if ( ErlVariable( VariableNum ).Value.Type == ValueNumber ) {
				RuntimeReportVar( RuntimeReportVarNum ).Value = ErlVariable( VariableNum ).Value.Number;
			} else {
				RuntimeReportVar( RuntimeReportVarNum ).Value = 0.0;
			}
		}

	}

	std::string
	IntegerToString( int const Number )
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         P Ellis
		//       DATE WRITTEN   unknown
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// convert integer number to a string

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string String;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		gio::write( String, fmtLD ) << Number; // Could add formatting here
		strip( String );

		return String;

	}

	ErlValueType
	SetErlValueNumber(
		Real64 const Number,
		Optional< ErlValueType const > OrigValue
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         P. Ellis
		//       DATE WRITTEN   unknown
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		ErlValueType newValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// na

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		if ( present( OrigValue ) ) { // preserve other parts of structure and only updated Value%Number
			newValue = OrigValue;
			newValue.Number = Number;
		} else {
			newValue.Type = ValueNumber;
			newValue.Number = Number;
		}

		return newValue;

	}

	ErlValueType
	StringValue( std::string const & String )
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         P. Ellis
		//       DATE WRITTEN   unkown
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// convert string to Erl Value structure

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		ErlValueType Value;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// na

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		// FLOW:

		Value.Type = ValueString;
		Value.String = String;

		return Value;
	}

	std::string
	ValueToString( ErlValueType const & Value )
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         P. Ellis
		//       DATE WRITTEN   Unknown
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Return value
		std::string String;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FLOW:
		String = "";

		{ auto const SELECT_CASE_var( Value.Type );
		if ( SELECT_CASE_var == ValueNumber ) {
			if ( Value.Number == 0.0 ) {
				String = "0.0";
			} else {
				String = TrimSigDigits( Value.Number, 6 ); //(String)
			}

		} else if ( SELECT_CASE_var == ValueString ) {
			String = Value.String;

		} else if ( SELECT_CASE_var == ValueArray ) {
			// TBD

		} else if ( SELECT_CASE_var == ValueError ) {
			String = " *** Error: " + Value.Error + " *** ";

		}}

		return String;
	}

	int
	FindEMSVariable(
		std::string const & VariableName, // variable name in Erl
		int const StackNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;

		// Return value
		int VariableNum;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool Found;
		int TrendVarNum;

		// FLOW:
		Found = false;
		std::string const UppercaseName = MakeUPPERCase( VariableName );

		// check in ErlVariables
		for ( VariableNum = 1; VariableNum <= NumErlVariables; ++VariableNum ) {
			if ( ErlVariable( VariableNum ).Name == UppercaseName ) {
				if ( ( ErlVariable( VariableNum ).StackNum == StackNum ) || ( ErlVariable( VariableNum ).StackNum == 0 ) ) {
					Found = true;
					break;
				}
			}
		}

		//check in Trend variables
		for ( TrendVarNum = 1; TrendVarNum <= NumErlTrendVariables; ++TrendVarNum ) {
			if ( TrendVariable( TrendVarNum ).Name == UppercaseName ) {
				VariableNum = TrendVariable( TrendVarNum ).ErlVariablePointer;
				if ( ( ErlVariable( VariableNum ).StackNum == StackNum ) || ( ErlVariable( VariableNum ).StackNum == 0 ) ) {
					Found = true;
					break;
				}
			}
		}

		if ( ! Found ) VariableNum = 0;

		return VariableNum;
	}

	int
	NewEMSVariable(
		std::string const & VariableName,
		int const StackNum,
		Optional< ErlValueType const > Value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Creates new variable if it doesn't exist.  If exists, returns existing variable number.

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;

		// Return value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		// Object Data

		// FLOW:
		int VariableNum = FindEMSVariable( VariableName, StackNum );

		if ( VariableNum == 0 ) { // Variable does not exist anywhere yet
			if ( NumErlVariables == 0 ) {
				ErlVariable.allocate( 1 );
				NumErlVariables = 1;
			} else { // Extend the variable array
				ErlVariable.redimension( ++NumErlVariables );
			}

			// Add the new variable
			VariableNum = NumErlVariables;
			ErlVariable( VariableNum ).Name = MakeUPPERCase( VariableName );
			ErlVariable( VariableNum ).StackNum = StackNum;
			ErlVariable( VariableNum ).Value.Type = ValueNumber; // ErlVariable values are numbers
		}

		if ( present( Value ) ) ErlVariable( VariableNum ).Value = Value;

		return VariableNum;
	}

	void
	SetupPossibleOperators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  setup hard coded list of possible operands

		// METHODOLOGY EMPLOYED:
		// Allocate structure and fill basic info on opertors and operands
		//  operators include built-in functions where operands are function arguments

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( AlreadyDidOnce ) return;

		PossibleOperators.allocate( NumPossibleOperators );

		// Build operator table
		// Order in this table is the order of precedence

		PossibleOperators( OperatorLiteral ).NumOperands = 1;
		PossibleOperators( OperatorLiteral ).Code = OperatorLiteral;

		// not sure how to distinguish from subtract in parsing of tokens, not yet available
		//  PossibleOperators(OperatorNegative)%NumOperands = 1
		//  PossibleOperators(OperatorNegative)%Code        = OperatorNegative
		//  PossibleOperators(OperatorNegative)%Symbol      = '-'

		PossibleOperators( OperatorDivide ).Symbol = "/";
		PossibleOperators( OperatorDivide ).NumOperands = 2;
		PossibleOperators( OperatorDivide ).Code = OperatorDivide;

		PossibleOperators( OperatorMultiply ).Symbol = "*";
		PossibleOperators( OperatorMultiply ).NumOperands = 2;
		PossibleOperators( OperatorMultiply ).Code = OperatorMultiply;

		PossibleOperators( OperatorSubtract ).Symbol = "-";
		PossibleOperators( OperatorSubtract ).NumOperands = 2;
		PossibleOperators( OperatorSubtract ).Code = OperatorSubtract;

		PossibleOperators( OperatorAdd ).Symbol = "+";
		PossibleOperators( OperatorAdd ).NumOperands = 2;
		PossibleOperators( OperatorAdd ).Code = OperatorAdd;

		PossibleOperators( OperatorEqual ).Symbol = "==";
		PossibleOperators( OperatorEqual ).NumOperands = 2;
		PossibleOperators( OperatorEqual ).Code = OperatorEqual;

		PossibleOperators( OperatorNotEqual ).Symbol = "<>";
		PossibleOperators( OperatorNotEqual ).NumOperands = 2;
		PossibleOperators( OperatorNotEqual ).Code = OperatorNotEqual;

		PossibleOperators( OperatorLessOrEqual ).Symbol = "<=";
		PossibleOperators( OperatorLessOrEqual ).NumOperands = 2;
		PossibleOperators( OperatorLessOrEqual ).Code = OperatorLessOrEqual;

		PossibleOperators( OperatorGreaterOrEqual ).Symbol = ">=";
		PossibleOperators( OperatorGreaterOrEqual ).NumOperands = 2;
		PossibleOperators( OperatorGreaterOrEqual ).Code = OperatorGreaterOrEqual;

		PossibleOperators( OperatorLessThan ).Symbol = "<";
		PossibleOperators( OperatorLessThan ).NumOperands = 2;
		PossibleOperators( OperatorLessThan ).Code = OperatorLessThan;

		PossibleOperators( OperatorGreaterThan ).Symbol = ">";
		PossibleOperators( OperatorGreaterThan ).NumOperands = 2;
		PossibleOperators( OperatorGreaterThan ).Code = OperatorGreaterThan;

		PossibleOperators( OperatorRaiseToPower ).Symbol = "^";
		PossibleOperators( OperatorRaiseToPower ).NumOperands = 2;
		PossibleOperators( OperatorRaiseToPower ).Code = OperatorRaiseToPower;

		PossibleOperators( OperatorLogicalAND ).Symbol = "&&";
		PossibleOperators( OperatorLogicalAND ).NumOperands = 2;
		PossibleOperators( OperatorLogicalAND ).Code = OperatorLogicalAND;

		PossibleOperators( OperatiorLogicalOR ).Symbol = "||";
		PossibleOperators( OperatiorLogicalOR ).NumOperands = 2;
		PossibleOperators( OperatiorLogicalOR ).Code = OperatiorLogicalOR;

		PossibleOperators( FuncRound ).Symbol = "@ROUND";
		PossibleOperators( FuncRound ).NumOperands = 1;
		PossibleOperators( FuncRound ).Code = FuncRound;

		PossibleOperators( FuncMod ).Symbol = "@MOD";
		PossibleOperators( FuncMod ).NumOperands = 2;
		PossibleOperators( FuncMod ).Code = FuncMod;

		PossibleOperators( FuncSin ).Symbol = "@SIN";
		PossibleOperators( FuncSin ).NumOperands = 1;
		PossibleOperators( FuncSin ).Code = FuncSin;

		PossibleOperators( FuncCos ).Symbol = "@COS";
		PossibleOperators( FuncCos ).NumOperands = 1;
		PossibleOperators( FuncCos ).Code = FuncCos;

		PossibleOperators( FuncArcSin ).Symbol = "@ARCSIN";
		PossibleOperators( FuncArcSin ).NumOperands = 1;
		PossibleOperators( FuncArcSin ).Code = FuncArcSin;

		PossibleOperators( FuncArcCos ).Symbol = "@ARCCOS";
		PossibleOperators( FuncArcCos ).NumOperands = 1;
		PossibleOperators( FuncArcCos ).Code = FuncArcCos;

		PossibleOperators( FuncDegToRad ).Symbol = "@DEGTORAD";
		PossibleOperators( FuncDegToRad ).NumOperands = 1;
		PossibleOperators( FuncDegToRad ).Code = FuncDegToRad;

		PossibleOperators( FuncRadToDeg ).Symbol = "@RADTODEG";
		PossibleOperators( FuncRadToDeg ).NumOperands = 1;
		PossibleOperators( FuncRadToDeg ).Code = FuncRadToDeg;

		PossibleOperators( FuncExp ).Symbol = "@EXP";
		PossibleOperators( FuncExp ).NumOperands = 1;
		PossibleOperators( FuncExp ).Code = FuncExp;

		PossibleOperators( FuncLn ).Symbol = "@LN";
		PossibleOperators( FuncLn ).NumOperands = 1;
		PossibleOperators( FuncLn ).Code = FuncLn;

		PossibleOperators( FuncMax ).Symbol = "@MAX";
		PossibleOperators( FuncMax ).NumOperands = 2;
		PossibleOperators( FuncMax ).Code = FuncMax;

		PossibleOperators( FuncMin ).Symbol = "@MIN";
		PossibleOperators( FuncMin ).NumOperands = 2;
		PossibleOperators( FuncMin ).Code = FuncMin;

		PossibleOperators( FuncABS ).Symbol = "@ABS";
		PossibleOperators( FuncABS ).NumOperands = 1;
		PossibleOperators( FuncABS ).Code = FuncABS;

		PossibleOperators( FuncRandU ).Symbol = "@RANDOMUNIFORM";
		PossibleOperators( FuncRandU ).NumOperands = 2;
		PossibleOperators( FuncRandU ).Code = FuncRandU;

		PossibleOperators( FuncRandG ).Symbol = "@RANDOMNORMAL";
		PossibleOperators( FuncRandG ).NumOperands = 4;
		PossibleOperators( FuncRandG ).Code = FuncRandG;

		PossibleOperators( FuncRandSeed ).Symbol = "@SEEDRANDOM";
		PossibleOperators( FuncRandSeed ).NumOperands = 1;
		PossibleOperators( FuncRandSeed ).Code = FuncRandSeed;

		PossibleOperators( FuncRhoAirFnPbTdbW ).Symbol = "@RHOAIRFNPBTDBW";
		PossibleOperators( FuncRhoAirFnPbTdbW ).NumOperands = 3;
		PossibleOperators( FuncRhoAirFnPbTdbW ).Code = FuncRhoAirFnPbTdbW;

		PossibleOperators( FuncCpAirFnWTdb ).Symbol = "@CPAIRFNWTDB";
		PossibleOperators( FuncCpAirFnWTdb ).NumOperands = 2;
		PossibleOperators( FuncCpAirFnWTdb ).Code = FuncCpAirFnWTdb;

		PossibleOperators( FuncHfgAirFnWTdb ).Symbol = "@HFGAIRFNWTDB";
		PossibleOperators( FuncHfgAirFnWTdb ).NumOperands = 2;
		PossibleOperators( FuncHfgAirFnWTdb ).Code = FuncHfgAirFnWTdb;

		PossibleOperators( FuncHgAirFnWTdb ).Symbol = "@HGAIRFNWTDB";
		PossibleOperators( FuncHgAirFnWTdb ).NumOperands = 2;
		PossibleOperators( FuncHgAirFnWTdb ).Code = FuncHgAirFnWTdb;

		PossibleOperators( FuncTdpFnTdbTwbPb ).Symbol = "@TDPFNTDBTWBPB";
		PossibleOperators( FuncTdpFnTdbTwbPb ).NumOperands = 3;
		PossibleOperators( FuncTdpFnTdbTwbPb ).Code = FuncTdpFnTdbTwbPb;

		PossibleOperators( FuncTdpFnWPb ).Symbol = "@TDPFNWPB";
		PossibleOperators( FuncTdpFnWPb ).NumOperands = 2;
		PossibleOperators( FuncTdpFnWPb ).Code = FuncTdpFnWPb;

		PossibleOperators( FuncHFnTdbW ).Symbol = "@HFNTDBW";
		PossibleOperators( FuncHFnTdbW ).NumOperands = 2;
		PossibleOperators( FuncHFnTdbW ).Code = FuncHFnTdbW;

		PossibleOperators( FuncHFnTdbRhPb ).Symbol = "@HFNTDBRHPB";
		PossibleOperators( FuncHFnTdbRhPb ).NumOperands = 3;
		PossibleOperators( FuncHFnTdbRhPb ).Code = FuncHFnTdbRhPb;

		PossibleOperators( FuncTdbFnHW ).Symbol = "@TDBFNHW";
		PossibleOperators( FuncTdbFnHW ).NumOperands = 2;
		PossibleOperators( FuncTdbFnHW ).Code = FuncTdbFnHW;

		PossibleOperators( FuncRhovFnTdbRh ).Symbol = "@RHOVFNTDBR";
		PossibleOperators( FuncRhovFnTdbRh ).NumOperands = 2;
		PossibleOperators( FuncRhovFnTdbRh ).Code = FuncRhovFnTdbRh;

		PossibleOperators( FuncRhovFnTdbRhLBnd0C ).Symbol = "@RhovFnTdbRhLBnd0C";
		PossibleOperators( FuncRhovFnTdbRhLBnd0C ).NumOperands = 2;
		PossibleOperators( FuncRhovFnTdbRhLBnd0C ).Code = FuncRhovFnTdbRhLBnd0C;

		PossibleOperators( FuncRhovFnTdbWPb ).Symbol = "@RHOVFNTDBWPB";
		PossibleOperators( FuncRhovFnTdbWPb ).NumOperands = 3;
		PossibleOperators( FuncRhovFnTdbWPb ).Code = FuncRhovFnTdbWPb;

		PossibleOperators( FuncRhFnTdbRhov ).Symbol = "@RHFNTDBRHOV";
		PossibleOperators( FuncRhFnTdbRhov ).NumOperands = 2;
		PossibleOperators( FuncRhFnTdbRhov ).Code = FuncRhFnTdbRhov;

		PossibleOperators( FuncRhFnTdbRhovLBnd0C ).Symbol = "@RHFNTDBRHOVLBND0C";
		PossibleOperators( FuncRhFnTdbRhovLBnd0C ).NumOperands = 2;
		PossibleOperators( FuncRhFnTdbRhovLBnd0C ).Code = FuncRhFnTdbRhovLBnd0C;

		PossibleOperators( FuncRhFnTdbWPb ).Symbol = "@RHFNTDBWPB";
		PossibleOperators( FuncRhFnTdbWPb ).NumOperands = 3;
		PossibleOperators( FuncRhFnTdbWPb ).Code = FuncRhFnTdbWPb;

		PossibleOperators( FuncTwbFnTdbWPb ).Symbol = "@TWBFNTDBWPB";
		PossibleOperators( FuncTwbFnTdbWPb ).NumOperands = 3;
		PossibleOperators( FuncTwbFnTdbWPb ).Code = FuncTwbFnTdbWPb;

		PossibleOperators( FuncVFnTdbWPb ).Symbol = "@VFNTDBWPB";
		PossibleOperators( FuncVFnTdbWPb ).NumOperands = 3;
		PossibleOperators( FuncVFnTdbWPb ).Code = FuncVFnTdbWPb;

		PossibleOperators( FuncWFnTdpPb ).Symbol = "@WFNTDPPB";
		PossibleOperators( FuncWFnTdpPb ).NumOperands = 2;
		PossibleOperators( FuncWFnTdpPb ).Code = FuncWFnTdpPb;

		PossibleOperators( FuncWFnTdbH ).Symbol = "@WFNTDBH";
		PossibleOperators( FuncWFnTdbH ).NumOperands = 2;
		PossibleOperators( FuncWFnTdbH ).Code = FuncWFnTdbH;

		PossibleOperators( FuncWFnTdbTwbPb ).Symbol = "@WFNTDBTWBPB";
		PossibleOperators( FuncWFnTdbTwbPb ).NumOperands = 3;
		PossibleOperators( FuncWFnTdbTwbPb ).Code = FuncWFnTdbTwbPb;

		PossibleOperators( FuncWFnTdbRhPb ).Symbol = "@WFNTDBRHPB";
		PossibleOperators( FuncWFnTdbRhPb ).NumOperands = 4;
		PossibleOperators( FuncWFnTdbRhPb ).Code = FuncWFnTdbRhPb;

		PossibleOperators( FuncPsatFnTemp ).Symbol = "@PSATFNTEMP";
		PossibleOperators( FuncPsatFnTemp ).NumOperands = 1;
		PossibleOperators( FuncPsatFnTemp ).Code = FuncPsatFnTemp;

		PossibleOperators( FuncTsatFnHPb ).Symbol = "@TSATFNHPB";
		PossibleOperators( FuncTsatFnHPb ).NumOperands = 2;
		PossibleOperators( FuncTsatFnHPb ).Code = FuncTsatFnHPb;

		PossibleOperators( FuncTsatFnPb ).Symbol = "@TSATFNPB";
		PossibleOperators( FuncTsatFnPb ).NumOperands = 1;
		PossibleOperators( FuncTsatFnPb ).Code = FuncTsatFnPb;

		PossibleOperators( FuncCpCW ).Symbol = "@CPCW";
		PossibleOperators( FuncCpCW ).NumOperands = 1;
		PossibleOperators( FuncCpCW ).Code = FuncCpCW;

		PossibleOperators( FuncCpHW ).Symbol = "@CPHW";
		PossibleOperators( FuncCpHW ).NumOperands = 1;
		PossibleOperators( FuncCpHW ).Code = FuncCpHW;

		PossibleOperators( FuncRhoH2O ).Symbol = "@RHOH2O";
		PossibleOperators( FuncRhoH2O ).NumOperands = 1;
		PossibleOperators( FuncRhoH2O ).Code = FuncRhoH2O;

		PossibleOperators( FuncFatalHaltEp ).Symbol = "@FATALHALTEP";
		PossibleOperators( FuncFatalHaltEp ).NumOperands = 1;
		PossibleOperators( FuncFatalHaltEp ).Code = FuncFatalHaltEp;

		PossibleOperators( FuncSevereWarnEp ).Symbol = "@SEVEREWARNEP";
		PossibleOperators( FuncSevereWarnEp ).NumOperands = 1;
		PossibleOperators( FuncSevereWarnEp ).Code = FuncSevereWarnEp;

		PossibleOperators( FuncWarnEp ).Symbol = "@WARNEP";
		PossibleOperators( FuncWarnEp ).NumOperands = 1;
		PossibleOperators( FuncWarnEp ).Code = FuncWarnEp;

		PossibleOperators( FuncTrendValue ).Symbol = "@TRENDVALUE";
		PossibleOperators( FuncTrendValue ).NumOperands = 2;
		PossibleOperators( FuncTrendValue ).Code = FuncTrendValue;

		PossibleOperators( FuncTrendAverage ).Symbol = "@TRENDAVERAGE";
		PossibleOperators( FuncTrendAverage ).NumOperands = 2;
		PossibleOperators( FuncTrendAverage ).Code = FuncTrendAverage;

		PossibleOperators( FuncTrendMax ).Symbol = "@TRENDMAX";
		PossibleOperators( FuncTrendMax ).NumOperands = 2;
		PossibleOperators( FuncTrendMax ).Code = FuncTrendMax;

		PossibleOperators( FuncTrendMin ).Symbol = "@TRENDMIN";
		PossibleOperators( FuncTrendMin ).NumOperands = 2;
		PossibleOperators( FuncTrendMin ).Code = FuncTrendMin;

		PossibleOperators( FuncTrendDirection ).Symbol = "@TRENDDIRECTION";
		PossibleOperators( FuncTrendDirection ).NumOperands = 2;
		PossibleOperators( FuncTrendDirection ).Code = FuncTrendDirection;

		PossibleOperators( FuncTrendSum ).Symbol = "@TRENDSUM";
		PossibleOperators( FuncTrendSum ).NumOperands = 2;
		PossibleOperators( FuncTrendSum ).Code = FuncTrendSum;

		PossibleOperators( FuncCurveValue ).Symbol = "@CURVEVALUE";
		PossibleOperators( FuncCurveValue ).NumOperands = 6;
		PossibleOperators( FuncCurveValue ).Code = FuncCurveValue;

		AlreadyDidOnce = true;

	}

	void
	ExternalInterfaceSetErlVariable(
		int const varNum, // The variable index to be written during run time
		Real64 const value // The real time value of the vairable to be set
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rui Zhang
		//       DATE WRITTEN   February 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is the ExternalInterface runtime write ErlVariable function

		// METHODOLOGY EMPLOYED:
		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		ErlVariable( varNum ).Value = SetErlValueNumber( value );

	}

	void
	ExternalInterfaceInitializeErlVariable(
		int const varNum, // The variable index to be written during run time
		ErlValueType const & initialValue, // The initial value
		bool const setToNull // Flag, if true, value will be initialized to Null
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   February 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets flags for ExternalInterface variables

		// METHODOLOGY EMPLOYED:
		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// Set initial value
		if ( setToNull ) {
			ErlVariable( varNum ).Value.Type = ValueNull;
		} else {
			ErlVariable( varNum ).Value = initialValue;
		}

		// Set variables to read-only as we don't want that other programs write to them
		ErlVariable( varNum ).ReadOnly = true;
		// Set flag that it is used by the ExternalInterface. This is needed to make sure that the ExternalInterface
		// interface writes only to ExternalInterface variables, and not to other ErlVariable
		ErlVariable( varNum ).SetByExternalInterface = true;

	}

	bool
	isExternalInterfaceErlVariable( int const varNum ) // The variable index to be written during run time
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   February 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function checks if an Erl name obtained from the xml file
		// is indeed specified as a ExternalInterface variable in the idf file

		// METHODOLOGY EMPLOYED:
		// USE STATEMENTS:

		// Return value
		bool isExternalInterfaceVar; // Set to true if the variable is a ExternalInterface variable

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		isExternalInterfaceVar = ErlVariable( varNum ).SetByExternalInterface;

		return isExternalInterfaceVar;

	}

} // RuntimeLanguageProcessor

} // EnergyPlus
