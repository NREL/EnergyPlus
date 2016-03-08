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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <ScheduleManager.hh>
#include <DataEnvironment.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ScheduleManager {
	// Module containing the Schedule Manager routines

	// MODULE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       January 2003 -- added sub-hourly schedule possibility (and interval scheduling)
	//                      J. Glazer January 2005 -- added Schedule:File
	//                      Michael Wetter February 2010 -- added Schedule for external Interface
	//                      L Lawrie - October 2012 - added sub-hourly option for Schedule:File
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To provide the capabilities of getting the schedule data from the input,
	// validating it, and storing it in such a manner that the schedule manager
	// can provide the scheduling value needs for the simulation.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// Proposal for Schedule Manager in EnergyPlus (Rick Strand)

	// OTHER NOTES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::HourOfDay;
	using DataGlobals::OutputFileInits;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::MinutesPerTimeStep;
	using DataGlobals::TimeStep;
	using DataGlobals::OutputFileDebug;
	using DataEnvironment::MonthTomorrow;
	using DataEnvironment::DayOfWeek;
	using DataEnvironment::DayOfWeekTomorrow;
	using DataEnvironment::DayOfMonthTomorrow;
	using DataEnvironment::HolidayIndex;
	using DataEnvironment::HolidayIndexTomorrow;
	using DataEnvironment::DSTIndicator;

	// Use statements for access to subroutines in other modules

	// Data
	//MODULE PARAMETER DEFINITIONS
	int const MaxDayTypes( 12 );
	static std::string const BlankString;
	Array1D_string const ValidDayTypes( MaxDayTypes, { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Holiday", "SummerDesignDay", "WinterDesignDay", "CustomDay1", "CustomDay2" } );

	int const NumScheduleTypeLimitUnitTypes( 14 );
	Array1D_string const ScheduleTypeLimitUnitTypes( NumScheduleTypeLimitUnitTypes, { "Dimensionless", "Temperature", "DeltaTemperature", "PrecipitationRate", "Angle", "ConvectionCoefficient", "ActivityLevel", "Velocity", "Capacity", "Power", "Availability", "Percent", "Control", "Mode" } );

	int const ScheduleInput_year( 1 );
	int const ScheduleInput_compact( 2 );
	int const ScheduleInput_file( 3 );
	int const ScheduleInput_constant( 4 );
	int const ScheduleInput_external( 5 );

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS

	// MODULE VARIABLE DECLARATIONS:

	//Integer Variables for the Module
	int NumScheduleTypes( 0 );
	int NumDaySchedules( 0 );
	int NumWeekSchedules( 0 );
	int NumSchedules( 0 );

	//Logical Variables for Module
	bool ScheduleInputProcessed( false ); // This is false until the Schedule Input has been processed.
	bool ScheduleDSTSFileWarningIssued( false );

		namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool CheckScheduleValueMinMaxRunOnceOnly (true );
	}

	//Derived Types Variables

	// Object Data
	Array1D< ScheduleTypeData > ScheduleType; // Allowed Schedule Types
	Array1D< DayScheduleData > DaySchedule; // Day Schedule Storage
	Array1D< WeekScheduleData > WeekSchedule; // Week Schedule Storage
	Array1D< ScheduleData > Schedule; // Schedule Storage

	static gio::Fmt fmtLD( "*" );
	static gio::Fmt fmtA( "(A)" );

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	// Clears the global data in ScheduleManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumScheduleTypes = 0;
		NumDaySchedules = 0;
		NumWeekSchedules = 0;
		NumSchedules = 0;
		ScheduleInputProcessed = false;
		ScheduleDSTSFileWarningIssued = false;
		CheckScheduleValueMinMaxRunOnceOnly = true;
		ScheduleType.deallocate();
		DaySchedule.deallocate();
		WeekSchedule.deallocate();
		Schedule.deallocate();
	}

	void
	ProcessScheduleInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       Rui Zhang February 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes the schedules input for EnergyPlus.

		// METHODOLOGY EMPLOYED:
		// Uses the standard get routines in the InputProcessor.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::ProcessNumber;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::SameString;
		using InputProcessor::FindItem;
		using General::ProcessDateString;
		using General::JulianDay;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using namespace DataIPShortCuts;
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharSpace;
		using DataStringGlobals::CharSemicolon;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataSystemVariables::iUnicode_end;
		using DataSystemVariables::TempFullFileName;
		using DataSystemVariables::CheckForActualFileName;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ProcessScheduleInput: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array1D_int DaysInYear( 366 );
		int UnitNumber;
		int LoopIndex;
		int InLoopIndex;
		int DayIndex;
		int WeekIndex;
		Array1D_string Alphas;
		Array1D_string cAlphaFields;
		Array1D_string cNumericFields;
		Array1D< Real64 > Numbers;
		Array1D_bool lAlphaBlanks;
		Array1D_bool lNumericBlanks;
		int NumAlphas;
		int NumNumbers;
		int Status;
		int StartMonth;
		int StartDay;
		int EndMonth;
		int EndDay;
		int StartPointer;
		int EndPointer;
		int NumPointer;
		int Count;
		int CheckIndex;
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool NumErrorFlag;
		int SchedTypePtr;
		std::string CFld; // Character field for error message
		//  CHARACTER(len=20) CFld1        ! Character field for error message
		int NumHrDaySchedules; // Number of "hourly" dayschedules
		int NumIntDaySchedules; // Number of "interval" dayschedules
		int NumExternalInterfaceSchedules; // Number of "PtolemyServer ExternalInterface" "compact" Schedules
		int NumExternalInterfaceFunctionalMockupUnitImportSchedules; // Number of "FunctionalMockupUnitImport ExternalInterface"
		// "compact" Schedules ! added for FMU Import
		int NumExternalInterfaceFunctionalMockupUnitExportSchedules; // Number of "FunctionalMockupUnitExport ExternalInterface"
		// "compact" Schedules ! added for FMU Export
		int NumLstDaySchedules; // Number of "list" dayschedules
		int NumRegDaySchedules; // Number of hourly+interval+list dayschedules
		int NumRegWeekSchedules; // Number of "regular" Weekschedules
		int NumRegSchedules; // Number of "regular" Schedules
		int NumCptWeekSchedules; // Number of "compact" WeekSchedules
		int NumCptSchedules; // Number of "compact" Schedules
		int NumCommaFileSchedules; // Number of Schedule:File schedules
		int NumConstantSchedules; // Number of "constant" schedules
		int TS; // Counter for Num Of Time Steps in Hour
		int Hr; // Hour Counter
		Array2D< Real64 > MinuteValue; // Temporary for processing interval schedules
		Array2D_bool SetMinuteValue; // Temporary for processing interval schedules
		int NumFields;
		int SCount;
		//  LOGICAL RptSchedule
		int RptLevel;
		int CurMinute;
		int MinutesPerItem;
		int NumExpectedItems;
		int MaxNums;
		int MaxAlps;
		int AddWeekSch;
		int AddDaySch;
		Array1D_bool AllDays( MaxDayTypes );
		Array1D_bool TheseDays( MaxDayTypes );
		bool ErrorHere;
		int SchNum;
		int WkCount;
		int DyCount;
		int NumField;
		int PDateType;
		int PWeekDay;
		int ThruField;
		std::string ExtraField;
		int UntilFld;
		int xxcount;
		//  REAL(r64) tempval
		static bool FullYearSet( false );
		static std::string CurrentThrough;
		static std::string LastFor;
		static std::string errmsg;
		int kdy;
		bool FileExists;
		// for SCHEDULE:FILE
		Array1D< Real64 > hourlyFileValues;
		int SchdFile;
		int colCnt;
		int rowCnt;
		int wordStart;
		int wordEnd;
		std::string::size_type sepPos;
		std::string LineIn;
		std::string subString;
		Real64 columnValue;
		int read_stat;
		int iDay;
		int hDay;
		int jHour;
		int kDayType;
		Real64 curHrVal;
		bool errFlag;
		std::string::size_type sPos;
		std::string CurrentModuleObject; // for ease in getting objects
		int MaxNums1;
		std::string::size_type endLine;
		std::string ColumnSep;
		bool firstLine;
		bool FileIntervalInterpolated;
		int rowLimitCount;
		int skiprowCount;
		int curcolCount;
		int numHourlyValues;
		int numerrors;
		int ifld;
		int hrLimitCount;

		MaxNums = 1; // Need at least 1 number because it's used as a local variable in the Schedule Types loop
		MaxAlps = 0;

		CurrentModuleObject = "ScheduleTypeLimits";
		NumScheduleTypes = GetNumObjectsFound( CurrentModuleObject );
		if ( NumScheduleTypes > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Day:Hourly";
		NumHrDaySchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumHrDaySchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Day:Interval";
		NumIntDaySchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumIntDaySchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Day:List";
		NumLstDaySchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumLstDaySchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Week:Daily";
		NumRegWeekSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumRegWeekSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Week:Compact";
		NumCptWeekSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumCptWeekSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Year";
		NumRegSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumRegSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Compact";
		NumCptSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumCptSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas + 1 );
		}
		CurrentModuleObject = "Schedule:File";
		NumCommaFileSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumCommaFileSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "Schedule:Constant";
		NumConstantSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumConstantSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas );
		}
		CurrentModuleObject = "ExternalInterface:Schedule";
		NumExternalInterfaceSchedules = GetNumObjectsFound( CurrentModuleObject );
		// added for FMI
		if ( NumCptSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas + 1 );
		}
		// added for FMU Import
		CurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Schedule";
		NumExternalInterfaceFunctionalMockupUnitImportSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumCptSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas + 1 );
		}
		// added for FMU Export
		CurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Schedule";
		NumExternalInterfaceFunctionalMockupUnitExportSchedules = GetNumObjectsFound( CurrentModuleObject );
		if ( NumCptSchedules > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
			MaxNums = max( MaxNums, NumNumbers );
			MaxAlps = max( MaxAlps, NumAlphas + 1 );
		}
		CurrentModuleObject = "Output:Schedules";
		GetObjectDefMaxArgs( CurrentModuleObject, Count, NumAlphas, NumNumbers );
		MaxNums = max( MaxNums, NumNumbers );
		MaxAlps = max( MaxAlps, NumAlphas );

		Alphas.allocate( MaxAlps ); // Maximum Alphas possible
		cAlphaFields.allocate( MaxAlps );
		cNumericFields.allocate( MaxNums );
		Numbers.dimension( MaxNums, 0.0 ); // Maximum Numbers possible
		lAlphaBlanks.dimension( MaxAlps, true );
		lNumericBlanks.dimension( MaxNums, true );

		// Prescan to determine extra day and week schedules due to compact schedule input
		AddWeekSch = 0;
		AddDaySch = 0;
		CurrentModuleObject = "Schedule:Compact";
		MaxNums1 = 0;
		for ( LoopIndex = 1; LoopIndex <= NumCptSchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status );
			// # 'THROUGH" => Number of additional week schedules
			// # 'FOR' => Number of additional day schedules
			for ( Count = 3; Count <= NumAlphas; ++Count ) {
				if ( has_prefix( Alphas( Count ), "THROUGH" ) ) ++AddWeekSch;
				if ( has_prefix( Alphas( Count ), "FOR" ) ) ++AddDaySch;
				if ( has_prefix( Alphas( Count ), "UNTIL" ) ) ++MaxNums1;
			}
		}
		if ( MaxNums1 > MaxNums ) {
			MaxNums = MaxNums1;
			cNumericFields.deallocate();
			Numbers.deallocate();
			lNumericBlanks.deallocate();
			cNumericFields.allocate( MaxNums );
			Numbers.dimension( MaxNums, 0.0 ); // Maximum Numbers possible
			lNumericBlanks.dimension( MaxNums, true );
		}
		// add week and day schedules for each FILE:COMMA schedule
		AddWeekSch += NumCommaFileSchedules * 366; //number of days/year because need a week for each day
		AddDaySch += NumCommaFileSchedules * 366; //number of days/year
		AddWeekSch += NumConstantSchedules;
		AddDaySch += NumConstantSchedules;
		// add week and day schedules for each ExternalInterface:Schedule schedule
		AddWeekSch += NumExternalInterfaceSchedules * 366; //number of days/year because need a week for each day
		AddDaySch += NumExternalInterfaceSchedules; //one day schedule for ExternalInterface to update during run time
		// added for FMU Import
		// add week and day schedules for each ExternalInterface:FunctionalMockupUnitImport:Schedule
		AddWeekSch += NumExternalInterfaceFunctionalMockupUnitImportSchedules * 366; //number of days/year
		//because need a week for each day
		AddDaySch += NumExternalInterfaceFunctionalMockupUnitImportSchedules; // one day schedule for ExternalInterface
		// to update during run time
		// added for FMU Export
		// add week and day schedules for each ExternalInterface:FunctionalMockupUnitExport:Schedule
		AddWeekSch += NumExternalInterfaceFunctionalMockupUnitExportSchedules * 366; //number of days/year
		//because need a week for each day
		AddDaySch += NumExternalInterfaceFunctionalMockupUnitExportSchedules; // one day schedule for ExternalInterface
		// to update during run time

		// include additional schedules in with count
		NumRegDaySchedules = NumHrDaySchedules + NumIntDaySchedules + NumLstDaySchedules;
		NumDaySchedules = NumRegDaySchedules + AddDaySch;
		NumWeekSchedules = NumRegWeekSchedules + NumCptWeekSchedules + AddWeekSch;
		NumSchedules = NumRegSchedules + NumCptSchedules + NumCommaFileSchedules + NumConstantSchedules + NumExternalInterfaceSchedules + NumExternalInterfaceFunctionalMockupUnitImportSchedules + NumExternalInterfaceFunctionalMockupUnitExportSchedules;

		//!  Most initializations in the schedule data structures are taken care of in
		//!  the definitions (see above)

		ScheduleType.allocate( {0,NumScheduleTypes} );

		DaySchedule.allocate( {0,NumDaySchedules} );
		//    Initialize
		for ( LoopIndex = 0; LoopIndex <= NumDaySchedules; ++LoopIndex ) {
			DaySchedule( LoopIndex ).TSValue.allocate( NumOfTimeStepInHour, 24 );
			for ( Count = 1; Count <= 24; ++Count ) {
				for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
					DaySchedule( LoopIndex ).TSValue( TS, Count ) = 0.0;
				}
			}
		}

		WeekSchedule.allocate( {0,NumWeekSchedules} );

		Schedule.allocate( {-1,NumSchedules} );
		Schedule( -1 ).ScheduleTypePtr = -1;
		Schedule( -1 ).WeekSchedulePointer = 1;
		Schedule( 0 ).ScheduleTypePtr = 0;
		Schedule( 0 ).WeekSchedulePointer = 0;

		UnitNumber = FindUnitNumber( DataStringGlobals::outputAuditFileName );
		gio::write( UnitNumber, fmtLD ) << " Processing Schedule Input -- Start";

		//!! Get Schedule Types

		CurrentModuleObject = "ScheduleTypeLimits";
		for ( LoopIndex = 1; LoopIndex <= NumScheduleTypes; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), ScheduleType( {1,NumScheduleTypes} ), LoopIndex - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			ScheduleType( LoopIndex ).Name = Alphas( 1 );
			if ( lNumericBlanks( 1 ) || lNumericBlanks( 2 ) ) {
				ScheduleType( LoopIndex ).Limited = false;
			} else if ( ! lNumericBlanks( 1 ) && ! lNumericBlanks( 2 ) ) {
				ScheduleType( LoopIndex ).Limited = true;
			}
			if ( ! lNumericBlanks( 1 ) ) {
				ScheduleType( LoopIndex ).Minimum = Numbers( 1 );
			}
			if ( ! lNumericBlanks( 2 ) ) {
				ScheduleType( LoopIndex ).Maximum = Numbers( 2 );
			}
			if ( ScheduleType( LoopIndex ).Limited ) {
				if ( Alphas( 2 ) == "DISCRETE" || Alphas( 2 ) == "INTEGER" ) {
					ScheduleType( LoopIndex ).IsReal = false;
				} else {
					if ( Alphas( 2 ) != "CONTINUOUS" && Alphas( 2 ) != "REAL" ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + ScheduleType( LoopIndex ).Name + "\", invalid " + cAlphaFields( 2 ) + '=' + Alphas( 2 ) );
						ErrorsFound = true;
					}
					ScheduleType( LoopIndex ).IsReal = true;
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! lAlphaBlanks( 3 ) ) {
					ScheduleType( LoopIndex ).UnitType = FindItem( Alphas( 3 ), ScheduleTypeLimitUnitTypes, NumScheduleTypeLimitUnitTypes );
					if ( ScheduleType( LoopIndex ).UnitType == 0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 3 ) + "=\"" + Alphas( 3 ) + "\" is invalid." );
					}
				}
			}
			if ( ScheduleType( LoopIndex ).Limited ) {
				if ( ScheduleType( LoopIndex ).Minimum > ScheduleType( LoopIndex ).Maximum ) {
					if ( ScheduleType( LoopIndex ).IsReal ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cNumericFields( 1 ) + " [" + RoundSigDigits( ScheduleType( LoopIndex ).Minimum, 2 ) + "] > " + cNumericFields( 2 ) + " [" + RoundSigDigits( ScheduleType( LoopIndex ).Maximum, 2 ) + "]." );
						ShowContinueError( "  Other warning/severes about schedule values may appear." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cNumericFields( 1 ) + " [" + RoundSigDigits( ScheduleType( LoopIndex ).Minimum, 0 ) + "] > " + cNumericFields( 2 ) + " [" + RoundSigDigits( ScheduleType( LoopIndex ).Maximum, 0 ) + "]." );
						ShowContinueError( "  Other warning/severes about schedule values may appear." );
					}
				}
			}
		}

		//!! Get Day Schedules (all types)

		//!!=> Get "DAYSCHEDULE" (Hourly)

		Count = 0;
		CurrentModuleObject = "Schedule:Day:Hourly";
		for ( LoopIndex = 1; LoopIndex <= NumHrDaySchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DaySchedule, Count, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++Count;
			DaySchedule( Count ).Name = Alphas( 1 );
			// Validate ScheduleType
			if ( NumScheduleTypes > 0 ) {
				CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
				if ( CheckIndex == 0 ) {
					if ( ! lAlphaBlanks( 2 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
					} else {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
					}
				} else {
					DaySchedule( Count ).ScheduleTypePtr = CheckIndex;
				}
			}
			for ( Hr = 1; Hr <= 24; ++Hr ) {
				DaySchedule( Count ).TSValue( {1,NumOfTimeStepInHour}, Hr ) = Numbers( Hr );
			}
			DaySchedule( Count ).IntervalInterpolated = false;
			SchedTypePtr = DaySchedule( Count ).ScheduleTypePtr;
			if ( ScheduleType( SchedTypePtr ).Limited ) {
				if ( any_lt( DaySchedule( Count ).TSValue, ScheduleType( SchedTypePtr ).Minimum ) || any_gt( DaySchedule( Count ).TSValue, ScheduleType( SchedTypePtr ).Maximum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Values are outside of range for " + cAlphaFields( 2 ) + '=' + Alphas( 2 ) );
				}
			}
			if ( ! ScheduleType( SchedTypePtr ).IsReal ) {
				// Make sure each is integer
				NumErrorFlag = false; // only show error message once
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						if ( DaySchedule( Count ).TSValue( TS, Hr ) != int( DaySchedule( Count ).TSValue( TS, Hr ) ) ) {
							if ( ! NumErrorFlag ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", One or more values are not integer as required by " + cAlphaFields( 2 ) + '=' + Alphas( 2 ) );
								NumErrorFlag = true;
							}
						}
					}
				}
			}
		}

		MinuteValue.allocate( 60, 24 );
		SetMinuteValue.allocate( 60, 24 );

		//!! Get "DaySchedule:Interval"

		CurrentModuleObject = "Schedule:Day:Interval";
		for ( LoopIndex = 1; LoopIndex <= NumIntDaySchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DaySchedule, Count, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++Count;
			DaySchedule( Count ).Name = Alphas( 1 );
			// Validate ScheduleType
			if ( NumScheduleTypes > 0 ) {
				CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
				if ( CheckIndex == 0 ) {
					if ( ! lAlphaBlanks( 2 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
					} else {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
					}
				} else {
					DaySchedule( Count ).ScheduleTypePtr = CheckIndex;
				}
			}
			NumFields = NumAlphas - 3;
			// check to see if numfield=0
			if ( NumFields == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Insufficient data entered for a full schedule day." );
				ShowContinueError( "...Number of interval fields = = [" + RoundSigDigits( NumFields ) + "]." );
				ErrorsFound = true;
			}

			ProcessIntervalFields( Alphas( { 4, _ } ), Numbers, NumFields, NumNumbers, MinuteValue, SetMinuteValue, ErrorsFound, Alphas( 1 ), CurrentModuleObject, (Alphas( 3 ) == "YES") );
			// Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
			if ( Alphas( 3 ) != "NO" && Alphas( 3 ) != "YES" ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "Invalid value for \"" + cAlphaFields( 3 ) + "\" field=\"" + Alphas( 3 ) + "\"" );
				ErrorsFound = true;
			} else if ( Alphas( 3 ) != "YES" ) { // No validation done on the value of the interpolation field
				DaySchedule( Count ).IntervalInterpolated = false;
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					CurMinute = MinutesPerTimeStep;
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						DaySchedule( Count ).TSValue( TS, Hr ) = MinuteValue( CurMinute, Hr );
						CurMinute += MinutesPerTimeStep;
					}
				}
			} else {
				DaySchedule( Count ).IntervalInterpolated = true;
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					SCount = 1;
					CurMinute = MinutesPerTimeStep;
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						DaySchedule( Count ).TSValue( TS, Hr ) = sum( MinuteValue( {SCount,CurMinute}, Hr ) ) / double( MinutesPerTimeStep );
						SCount = CurMinute + 1;
						CurMinute += MinutesPerTimeStep;
					}
				}
			}

			SchedTypePtr = DaySchedule( Count ).ScheduleTypePtr;
			if ( ScheduleType( SchedTypePtr ).Limited ) {
				if ( any_lt( DaySchedule( Count ).TSValue, ScheduleType( SchedTypePtr ).Minimum ) || any_gt( DaySchedule( Count ).TSValue, ScheduleType( SchedTypePtr ).Maximum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Values are outside of range for " + cAlphaFields( 2 ) + '=' + Alphas( 2 ) );
				}
			}
			if ( ! ScheduleType( SchedTypePtr ).IsReal ) {
				// Make sure each is integer
				NumErrorFlag = false; // only show error message once
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						if ( DaySchedule( Count ).TSValue( TS, Hr ) != int( DaySchedule( Count ).TSValue( TS, Hr ) ) ) {
							if ( ! NumErrorFlag ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", , One or more values are not integer as required by " + cAlphaFields( 2 ) + '=' + Alphas( 2 ) );
								NumErrorFlag = true;
							}
						}
					}
				}
			}
		}

		//!! Get "DaySchedule:List"

		CurrentModuleObject = "Schedule:Day:List";
		for ( LoopIndex = 1; LoopIndex <= NumLstDaySchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DaySchedule, Count, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++Count;
			DaySchedule( Count ).Name = Alphas( 1 );
			// Validate ScheduleType
			if ( NumScheduleTypes > 0 ) {
				CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
				if ( CheckIndex == 0 ) {
					if ( ! lAlphaBlanks( 2 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
					} else {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
					}
				} else {
					DaySchedule( Count ).ScheduleTypePtr = CheckIndex;
				}
			}

			// Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
			if ( Alphas( 3 ) != "NO" && Alphas( 3 ) != "YES" ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "Invalid value for \"" + cAlphaFields( 3 ) + "\" field=\"" + Alphas( 3 ) + "\"" );
				ErrorsFound = true;
			} else if ( Alphas( 3 ) != "YES" ) { // No validation done on the value of the interpolation field
				DaySchedule( Count ).IntervalInterpolated = false;
			} else {
				DaySchedule( Count ).IntervalInterpolated = true;
			}

			// check to see if there are any fields
			if ( Numbers( 1 ) <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Insufficient data entered for a full schedule day." );
				ShowContinueError( "...Minutes per Item field = [" + RoundSigDigits( int( Numbers( 1 ) ) ) + "]." );
				ErrorsFound = true;
				continue;
			}
			if ( NumNumbers < 25 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Insufficient data entered for a full schedule day." );
				ShowContinueError( "...Minutes per Item field = [" + RoundSigDigits( int( Numbers( 1 ) ) ) + "] and only [" + RoundSigDigits( NumNumbers - 1 ) + "] to apply to list fields." );
				ErrorsFound = true;
				continue;
			}
			MinutesPerItem = int( Numbers( 1 ) );
			NumExpectedItems = 1440 / MinutesPerItem;
			if ( ( NumNumbers - 1 ) != NumExpectedItems ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + ", Number of Entered Items=" + RoundSigDigits( NumNumbers - 1 ) + " not equal number of expected items=" + RoundSigDigits( NumExpectedItems ) );
				ShowContinueError( "based on " + cNumericFields( 1 ) + " field value=" + RoundSigDigits( MinutesPerItem ) );
				ErrorsFound = true;
				continue;
			}

			if ( mod( 60, MinutesPerItem ) != 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) );
				ShowContinueError( "Requested " + cNumericFields( 1 ) + " field value (" + RoundSigDigits( MinutesPerItem ) + ") not evenly divisible into 60" );
				ErrorsFound = true;
				continue;
			}

			// Number of numbers in the Numbers list okay to process
			Hr = 1;
			CurMinute = MinutesPerItem;
			SCount = 1;
			for ( NumFields = 2; NumFields <= NumNumbers; ++NumFields ) {
				MinuteValue( {SCount,CurMinute}, Hr ) = Numbers( NumFields );
				SCount = CurMinute + 1;
				CurMinute += MinutesPerItem;
				if ( CurMinute > 60 ) {
					CurMinute = MinutesPerItem;
					SCount = 1;
					++Hr;
				}
			}

			// Now parcel into TS Value....

			if ( DaySchedule( Count ).IntervalInterpolated ) {
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					SCount = 1;
					CurMinute = MinutesPerTimeStep;
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						DaySchedule( Count ).TSValue( TS, Hr ) = sum( MinuteValue( {SCount,CurMinute}, Hr ) ) / double( MinutesPerTimeStep );
						SCount = CurMinute + 1;
						CurMinute += MinutesPerTimeStep;
					}
				}
			} else {
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					CurMinute = MinutesPerTimeStep;
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						DaySchedule( Count ).TSValue( TS, Hr ) = MinuteValue( CurMinute, Hr );
						CurMinute += MinutesPerTimeStep;
					}
				}
			}

			SchedTypePtr = DaySchedule( Count ).ScheduleTypePtr;
			if ( ScheduleType( SchedTypePtr ).Limited ) {
				if ( any_lt( DaySchedule( Count ).TSValue, ScheduleType( SchedTypePtr ).Minimum ) || any_gt( DaySchedule( Count ).TSValue, ScheduleType( SchedTypePtr ).Maximum ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Values are outside of range for " + cAlphaFields( 2 ) + '=' + Alphas( 2 ) );
				}
			}
			if ( ! ScheduleType( SchedTypePtr ).IsReal ) {
				// Make sure each is integer
				NumErrorFlag = false; // only show error message once
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						if ( DaySchedule( Count ).TSValue( TS, Hr ) != int( DaySchedule( Count ).TSValue( TS, Hr ) ) ) {
							if ( ! NumErrorFlag ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", , One or more values are not integer as required by " + cAlphaFields( 2 ) + '=' + Alphas( 2 ) );
								NumErrorFlag = true;
							}
						}
					}
				}
			}
		}

		//!! Get Week Schedules - regular

		CurrentModuleObject = "Schedule:Week:Daily";
		for ( LoopIndex = 1; LoopIndex <= NumRegWeekSchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), WeekSchedule( {1,NumRegWeekSchedules} ), LoopIndex - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			WeekSchedule( LoopIndex ).Name = Alphas( 1 );
			// Rest of Alphas are processed into Pointers
			for ( InLoopIndex = 1; InLoopIndex <= MaxDayTypes; ++InLoopIndex ) {
				DayIndex = FindItemInList( Alphas( InLoopIndex + 1 ), DaySchedule( {1,NumRegDaySchedules} ) );
				if ( DayIndex == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( InLoopIndex + 1 ) + " \"" + Alphas( InLoopIndex + 1 ) + "\" not Found", UnitNumber );
					ErrorsFound = true;
				} else {
					WeekSchedule( LoopIndex ).DaySchedulePointer( InLoopIndex ) = DayIndex;
				}
			}
		}

		//!! Get Week Schedules - compact
		Count = NumRegWeekSchedules;
		CurrentModuleObject = "Schedule:Week:Compact";
		for ( LoopIndex = 1; LoopIndex <= NumCptWeekSchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			if ( Count > 0 ) {
				VerifyName( Alphas( 1 ), WeekSchedule( {1,Count} ), Count, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) Alphas( 1 ) = "xxxxx";
				}
			}
			++Count;
			WeekSchedule( Count ).Name = Alphas( 1 );
			AllDays = false;
			// Rest of Alphas are processed into Pointers
			for ( InLoopIndex = 2; InLoopIndex <= NumAlphas; InLoopIndex += 2 ) {
				DayIndex = FindItemInList( Alphas( InLoopIndex + 1 ), DaySchedule( {1,NumRegDaySchedules} ) );
				if ( DayIndex == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( InLoopIndex + 1 ) + " \"" + Alphas( InLoopIndex + 1 ) + "\" not Found", UnitNumber );
					ShowContinueError( "ref: " + cAlphaFields( InLoopIndex ) + " \"" + Alphas( InLoopIndex ) + "\"" );
					ErrorsFound = true;
				} else {
					TheseDays = false;
					ErrorHere = false;
					ProcessForDayTypes( Alphas( InLoopIndex ), TheseDays, AllDays, ErrorHere );
					if ( ErrorHere ) {
						ShowContinueError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) );
						ErrorsFound = true;
					} else {
						for ( Hr = 1; Hr <= MaxDayTypes; ++Hr ) {
							if ( TheseDays( Hr ) ) {
								WeekSchedule( Count ).DaySchedulePointer( Hr ) = DayIndex;
							}
						}
					}
				}
			}
			//  Have processed all named days, check to make sure all given
			if ( ! all( AllDays ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Missing some day assignments" );
				ErrorsFound = true;
			}
		}
		NumRegWeekSchedules = Count;

		//!! Get Schedules (all types)

		//!! Get Regular Schedules

		CurrentModuleObject = "Schedule:Year";
		for ( LoopIndex = 1; LoopIndex <= NumRegSchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), LoopIndex - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Schedule( LoopIndex ).Name = Alphas( 1 );
			Schedule( LoopIndex ).SchType = ScheduleInput_year;
			// Validate ScheduleType
			if ( NumScheduleTypes > 0 ) {
				CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
				if ( CheckIndex == 0 ) {
					if ( ! lAlphaBlanks( 2 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
					} else {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
					}
				} else {
					Schedule( LoopIndex ).ScheduleTypePtr = CheckIndex;
				}
			}
			NumPointer = 0;
			DaysInYear = 0;
			// Rest of Alphas (Weekschedules) are processed into Pointers
			for ( InLoopIndex = 3; InLoopIndex <= NumAlphas; ++InLoopIndex ) {
				WeekIndex = FindItemInList( Alphas( InLoopIndex ), WeekSchedule( {1,NumRegWeekSchedules} ) );
				if ( WeekIndex == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( InLoopIndex ) + "=\"" + Alphas( InLoopIndex ) + "\" not found.", UnitNumber );
					ErrorsFound = true;
				} else {
					// Process for month, day
					StartMonth = int( Numbers( NumPointer + 1 ) );
					StartDay = int( Numbers( NumPointer + 2 ) );
					EndMonth = int( Numbers( NumPointer + 3 ) );
					EndDay = int( Numbers( NumPointer + 4 ) );
					NumPointer += 4;
					StartPointer = JulianDay( StartMonth, StartDay, 1 );
					EndPointer = JulianDay( EndMonth, EndDay, 1 );
					if ( StartPointer <= EndPointer ) {
						for ( Count = StartPointer; Count <= EndPointer; ++Count ) {
							++DaysInYear( Count );
							Schedule( LoopIndex ).WeekSchedulePointer( Count ) = WeekIndex;
						}
					} else {
						for ( Count = StartPointer; Count <= 366; ++Count ) {
							++DaysInYear( Count );
							Schedule( LoopIndex ).WeekSchedulePointer( Count ) = WeekIndex;
						}
						for ( Count = 1; Count <= EndPointer; ++Count ) {
							++DaysInYear( Count );
							Schedule( LoopIndex ).WeekSchedulePointer( Count ) = WeekIndex;
						}
					}
				}
			}
			// Perform Error checks on this item
			// Do special test for Feb 29.  Make equal to Feb 28.
			if ( DaysInYear( 60 ) == 0 ) {
				DaysInYear( 60 ) = DaysInYear( 59 );
				Schedule( LoopIndex ).WeekSchedulePointer( 60 ) = Schedule( LoopIndex ).WeekSchedulePointer( 59 );
			}
			if ( any_eq( DaysInYear, 0 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( LoopIndex ).Name + "\" has missing days in its schedule pointers", UnitNumber );
				ErrorsFound = true;
			}
			if ( any_gt( DaysInYear, 1 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( LoopIndex ).Name + "\" has overlapping days in its schedule pointers", UnitNumber );
				ErrorsFound = true;
			}

			if ( AnyEnergyManagementSystemInModel ) { // setup constant schedules as actuators
				SetupEMSActuator( "Schedule:Year", Schedule( LoopIndex ).Name, "Schedule Value", "[ ]", Schedule( LoopIndex ).EMSActuatedOn, Schedule( LoopIndex ).EMSValue );
			}

		}

		//!! Get Compact Schedules
		//SCHEDULE:COMPACT,
		//   \memo Irregular object.  Does not follow the usual definition for fields.  Fields A3... are:
		//   \memo Through: Date
		//   \memo For: Applicable days (ref: Weekschedule:Compact)
		//   \memo Interpolate: Yes/No (ref: Dayschedule:interval) -- optional, if not used will be "No"
		//   \memo Until: <Time> (ref: Dayschedule:Interval)
		//   \memo <numeric value>
		//   \memo words "Through","For","Interpolate","Until" must be included.
		//  A1 , \field Name
		//       \required-field
		//       \type alpha
		//       \reference ScheduleNames
		//  A2 , \field ScheduleType
		//       \type object-list
		//       \object-list ScheduleTypeNames
		//  A3 , \field Complex Field #1
		//  A4 , \field Complex Field #2
		//  A5 , \field Complex Field #3

		SchNum = NumRegSchedules;
		AddWeekSch = NumRegWeekSchedules;
		AddDaySch = NumRegDaySchedules;
		CurrentModuleObject = "Schedule:Compact";
		for ( LoopIndex = 1; LoopIndex <= NumCptSchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++SchNum;
			Schedule( SchNum ).Name = Alphas( 1 );
			Schedule( SchNum ).SchType = ScheduleInput_compact;
			// Validate ScheduleType
			CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
			if ( CheckIndex == 0 ) {
				if ( ! lAlphaBlanks( 2 ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
				} else {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
				}
			} else {
				Schedule( SchNum ).ScheduleTypePtr = CheckIndex;
			}
			NumPointer = 0;
			DaysInYear = 0;
			// Process the "complex" fields -- so named because they are not a 1:1 correspondence
			// as other objects are
			NumField = 3;
			StartPointer = 1;
			WkCount = 0;
			DyCount = 0;
			FullYearSet = false;
			while ( NumField < NumAlphas ) {
				//   Process "Through"
				if ( ! has_prefix( Alphas( NumField ), "THROUGH:" ) && ! has_prefix( Alphas( NumField ), "THROUGH" ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( SchNum ).Name + "\", Expecting \"Through:\" date" );
					ShowContinueError( "Instead, found entry=" + Alphas( NumField ) );
					ErrorsFound = true;
					goto Through_exit;
				} else {
					if ( Alphas( NumField )[ 7 ] == ':' ) {
						sPos = 8;
					} else {
						sPos = 7;
					}
					Alphas( NumField ).erase( 0, sPos );
					strip( Alphas( NumField ) );
				}
				CurrentThrough = Alphas( NumField );
				ErrorHere = false;
				ProcessDateString( Alphas( NumField ), EndMonth, EndDay, PWeekDay, PDateType, ErrorHere );
				if ( PDateType > 1 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( SchNum ).Name + "\", Invalid \"Through:\" date" );
					ShowContinueError( "Found entry=" + Alphas( NumField ) );
					ErrorsFound = true;
					goto Through_exit;
				} else if ( ErrorHere ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( SchNum ).Name + "\", Invalid \"Through:\" date" );
					ShowContinueError( "Found entry=" + Alphas( NumField ) );
					ErrorsFound = true;
					goto Through_exit;
				} else {
					EndPointer = JulianDay( EndMonth, EndDay, 1 );
					if ( EndPointer == 366 ) {
						if ( FullYearSet ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( SchNum ).Name + "\", New \"Through\" entry when \"full year\" already set" );
							ShowContinueError( "\"Through\" field=" + CurrentThrough );
							ErrorsFound = true;
						}
						FullYearSet = true;
					}
				}
				++WkCount;
				++AddWeekSch;
				gio::write( ExtraField, fmtLD ) << WkCount;
				strip( ExtraField );
				WeekSchedule( AddWeekSch ).Name = Alphas( 1 ) + "_wk_" + ExtraField;
				WeekSchedule( AddWeekSch ).Used = true;
				for ( Hr = StartPointer; Hr <= EndPointer; ++Hr ) {
					Schedule( SchNum ).WeekSchedulePointer( Hr ) = AddWeekSch;
					++DaysInYear( Hr );
				}
				StartPointer = EndPointer + 1;
				ThruField = NumField;
				AllDays = false;
				++NumField;
				while ( NumField < NumAlphas ) { // Continues until next "Through"
					if ( has_prefix( Alphas( NumField ), "THROUGH" ) ) goto For_exit;
					//   "For" must be next, adds to "# Day Schedules"
					if ( has_prefix( Alphas( NumField ), "FOR" ) ) {
						++DyCount;
						++AddDaySch;
						gio::write( ExtraField, fmtLD ) << DyCount;
						strip( ExtraField );
						DaySchedule( AddDaySch ).Name = Alphas( 1 ) + "_dy_" + ExtraField;
						DaySchedule( AddDaySch ).ScheduleTypePtr = Schedule( SchNum ).ScheduleTypePtr;
						DaySchedule( AddDaySch ).Used = true;
						TheseDays = false;
						ErrorHere = false;
						LastFor = Alphas( NumField );
						ProcessForDayTypes( Alphas( NumField ), TheseDays, AllDays, ErrorHere );
						if ( ErrorHere ) {
							ShowContinueError( "ref " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
							ShowContinueError( "ref Through field=" + Alphas( ThruField ) );
							ErrorsFound = true;
						} else {
							for ( Hr = 1; Hr <= MaxDayTypes; ++Hr ) {
								if ( TheseDays( Hr ) ) {
									WeekSchedule( AddWeekSch ).DaySchedulePointer( Hr ) = AddDaySch;
								}
							}
						}
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Looking for \"For\" field, found=" + Alphas( NumField ) );
						ErrorsFound = true;
						//          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
						//               '", Expecting "For:" day types')
						//          CALL ShowContinueError('Instead, found entry='//TRIM(Alphas(NumField)))
						goto Through_exit;
					}
					// Check for "Interpolate"
					++NumField;
					if ( has_prefix( Alphas( NumField ), "INTERPOLATE" ) ) {
						if ( has( Alphas( NumField ), "YES" ) ) {
							DaySchedule( AddDaySch ).IntervalInterpolated = true;
						} else {
							DaySchedule( AddDaySch ).IntervalInterpolated = false;
						}
						++NumField;
					} else {
						if ( ! has_prefix( Alphas( NumField ), "UNTIL" ) ) {
							if ( has( Alphas( NumField ), "YES" ) ) {
								DaySchedule( AddDaySch ).IntervalInterpolated = true;
							} else if ( has( Alphas( NumField ), "NO" ) ) {
								DaySchedule( AddDaySch ).IntervalInterpolated = false;
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Illegal Field entered =" + Alphas( NumField ) );
								ErrorsFound = true;
							}
							++NumField;
						}
					}
					NumNumbers = 0;
					xxcount = 0;
					UntilFld = NumField;
					while ( true ) {
						if ( has_prefix( Alphas( NumField ), "FOR" ) ) break;
						if ( has_prefix( Alphas( NumField ), "THROUGH" ) ) break;
						if ( has_prefix( Alphas( NumField ), "UNTIL" ) ) {
							// Process Until/Value pairs for later processing by other routine.
							++NumField;
							++xxcount;
							++NumNumbers;
							Numbers( NumNumbers ) = ProcessNumber( Alphas( NumField ), ErrorHere );
							if ( ErrorHere ) {
								ShowSevereError( CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
								ShowContinueError( "Until field=[" + Alphas( NumField - 1 ) + "] has illegal value field=[" + Alphas( NumField ) + "]." );
								ErrorsFound = true;
							}
							++NumField;
							Alphas( UntilFld + xxcount ) = Alphas( NumField ); // Incase next is "until"
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Looking for \"Until\" field, found=" + Alphas( NumField ) );
							ErrorsFound = true;
							goto Through_exit;
						}
						if ( Alphas( NumField ).empty() ) break;
					}
					// Process Untils, Numbers
					if ( NumNumbers > 0 ) {
						NumFields = NumNumbers;
						ErrorHere = false;
						ProcessIntervalFields( Alphas( { UntilFld, _ } ), Numbers, NumFields, NumNumbers, MinuteValue, SetMinuteValue, ErrorHere, DaySchedule( AddDaySch ).Name, CurrentModuleObject + " DaySchedule Fields", DaySchedule( AddDaySch ).IntervalInterpolated );
						// Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
						if ( ErrorHere ) {
							ShowContinueError( "ref " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\"" );
							ErrorsFound = true;
						}
						if ( ! DaySchedule( AddDaySch ).IntervalInterpolated ) { // No validation done on the value of the interpolation field
							for ( Hr = 1; Hr <= 24; ++Hr ) {
								CurMinute = MinutesPerTimeStep;
								for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
									DaySchedule( AddDaySch ).TSValue( TS, Hr ) = MinuteValue( CurMinute, Hr );
									CurMinute += MinutesPerTimeStep;
								}
							}
						} else {
							for ( Hr = 1; Hr <= 24; ++Hr ) {
								SCount = 1;
								CurMinute = MinutesPerTimeStep;
								for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
									//                tempval=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
									DaySchedule( AddDaySch ).TSValue( TS, Hr ) = sum( MinuteValue( {SCount,CurMinute}, Hr ) ) / double( MinutesPerTimeStep );
									SCount = CurMinute + 1;
									CurMinute += MinutesPerTimeStep;
								}
							}
						}
					}
				}
				For_exit: ;
				if ( ! all( AllDays ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Schedule( SchNum ).Name + "\" has missing day types in Through=" + CurrentThrough );
					ShowContinueError( "Last \"For\" field=" + LastFor );
					errmsg = "Missing day types=,";
					for ( kdy = 1; kdy <= MaxDayTypes; ++kdy ) {
						if ( AllDays( kdy ) ) continue;
						errmsg.erase( errmsg.length() - 1 );
						errmsg += "\"" + ValidDayTypes( kdy ) + "\",-";
					}
					errmsg.erase( errmsg.length() - 2 );
					ShowContinueError( errmsg );
					ShowContinueError( "Missing day types will have 0.0 as Schedule Values" );
				}
			}
			Through_exit: ;
			if ( DaysInYear( 60 ) == 0 ) {
				DaysInYear( 60 ) = DaysInYear( 59 );
				Schedule( LoopIndex ).WeekSchedulePointer( 60 ) = Schedule( LoopIndex ).WeekSchedulePointer( 59 );
			}
			if ( any_eq( DaysInYear, 0 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( SchNum ).Name + "\" has missing days in its schedule pointers", UnitNumber );
				ErrorsFound = true;
			}
			if ( any_gt( DaysInYear, 1 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Schedule( SchNum ).Name + "\" has overlapping days in its schedule pointers", UnitNumber );
				ErrorsFound = true;
			}

			if ( AnyEnergyManagementSystemInModel ) { // setup constant schedules as actuators
				SetupEMSActuator( "Schedule:Compact", Schedule( SchNum ).Name, "Schedule Value", "[ ]", Schedule( SchNum ).EMSActuatedOn, Schedule( SchNum ).EMSValue );
			}

		}

		//  Schedule:File,
		//   \min-fields 5
		//         \memo A Schedule:File points to a text computer file that has 8760-8784 hours of data.
		//    A1 , \field Name
		//         \required-field
		//         \type alpha
		//         \reference ScheduleNames
		//    A2 , \field Schedule Type Limits Name
		//         \type object-list
		//         \object-list ScheduleTypeLimitsNames
		//    A3 , \field File Name
		//         \required-field
		//         \retaincase
		//    N1 , \field Column Number
		//         \required-field
		//         \type integer
		//         \minimum 1
		//    N2 , \field Rows to Skip at Top
		//         \required-field
		//         \type integer
		//         \minimum 0
		//    N3 , \field Number of Hours of Data
		//         \note 8760 hours does not account for leap years, 8784 does.
		//         \note should be either 8760 or 8784
		//         \default 8760
		//         \minimum 8760
		//         \maximum 8784
		//    A4 , \field Column Separator
		//         \type choice
		//         \key Comma
		//         \key Tab
		//         \key Fixed
		//         \key Semicolon
		//         \default Comma
		//    A5 , \field Interpolate to Timestep
		//         \note when the interval does not match the user specified timestep a "Yes" choice will average between the intervals request (to
		//         \note timestep resolution.  a "No" choice will use the interval value at the simulation timestep without regard to if it matches
		//         \note the boundary or not.
		//         \type choice
		//         \key Yes
		//         \key No
		//         \default No
		//    N4 ; \field Minutes per Item
		//         \note Must be evenly divisible into 60
		//         \type integer
		//         \minimum 1
		//         \maximum 60

		// continue adding to SchNum,AddWeekSch,AddDaySch
		if ( NumCommaFileSchedules > 0 ) {
			hourlyFileValues.allocate( 8784 * 60 ); // sized to accomodate any interval for schedule file.
		}
		CurrentModuleObject = "Schedule:File";
		for ( LoopIndex = 1; LoopIndex <= NumCommaFileSchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++SchNum;
			Schedule( SchNum ).Name = Alphas( 1 );
			Schedule( SchNum ).SchType = ScheduleInput_file;
			// Validate ScheduleType
			if ( NumScheduleTypes > 0 ) {
				CheckIndex = 0;
				if ( ! lAlphaBlanks( 2 ) ) CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
				if ( CheckIndex == 0 ) {
					if ( ! lAlphaBlanks( 2 ) ) {
						ShowWarningError( "ProcessScheduleInput: For " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
					} else {
						ShowWarningError( "For " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
					}
				} else {
					Schedule( SchNum ).ScheduleTypePtr = CheckIndex;
				}
			}
			hourlyFileValues = 0.0; //set default values to zero

			// Numbers(1) - which column
			curcolCount = Numbers( 1 );
			// Numbers(2) - number of rows to skip
			skiprowCount = Numbers( 2 );
			if ( Numbers( 3 ) == 0 ) Numbers( 3 ) = 8760.0;
			if ( Numbers( 3 ) != 8760 && Numbers( 3 ) != 8784 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cNumericFields( 3 ) + " must = 8760 or 8784 (for a leap year)" );
				ShowContinueError( "..Value for field = " + TrimSigDigits( Numbers( 3 ), 0 ) + ", Schedule not processed." );
				ErrorsFound = true;
				continue;
			}

			if ( lAlphaBlanks( 4 ) || SameString( Alphas( 4 ), "comma" ) ) {
				ColumnSep = CharComma;
				Alphas( 4 ) = "comma";
			} else if ( SameString( Alphas( 4 ), "semicolon" ) ) {
				ColumnSep = CharSemicolon;
			} else if ( SameString( Alphas( 4 ), "tab" ) ) {
				ColumnSep = CharTab;
			} else if ( SameString( Alphas( 4 ), "space" ) ) {
				ColumnSep = CharSpace;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 4 ) + " illegal value=\"" + Alphas( 4 ) + "\"." );
				ShowContinueError( "..must be Comma, Semicolon, Tab, or Space." );
				ErrorsFound = true;
				continue;
			}

			// Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
			FileIntervalInterpolated = false;
			if ( lAlphaBlanks( 5 ) ) Alphas( 5 ) = "NO";
			if ( Alphas( 5 ) != "NO" && Alphas( 5 ) != "YES" ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "Invalid value for \"" + cAlphaFields( 5 ) + "\" field=\"" + Alphas( 5 ) + "\"" );
				ErrorsFound = true;
			} else if ( Alphas( 5 ) != "YES" ) { // No validation done on the value of the interpolation field
				FileIntervalInterpolated = false;
			} else {
				FileIntervalInterpolated = true;
			}

			// is it a sub-hourly schedule or not?
			MinutesPerItem = 60;
			if ( NumNumbers > 3 ) {
				MinutesPerItem = int( Numbers( 4 ) );
				NumExpectedItems = 1440 / MinutesPerItem;
				if ( mod( 60, MinutesPerItem ) != 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) );
					ShowContinueError( "Requested " + cNumericFields( 4 ) + " field value (" + RoundSigDigits( MinutesPerItem ) + ") not evenly divisible into 60" );
					ErrorsFound = true;
					continue;
				}
			}

			numHourlyValues = Numbers( 3 );
			rowLimitCount = ( Numbers( 3 ) * 60.0 ) / MinutesPerItem;
			hrLimitCount = 60 / MinutesPerItem;

			//    ! Number of numbers in the Numbers list okay to process
			//    Hr=1
			//    CurMinute=MinutesPerItem
			//    SCount=1
			//    DO NumFields=2,NumNumbers
			//      MinuteValue(Hr,SCount:CurMinute)=Numbers(NumFields)
			//      SCount=CurMinute+1
			//      CurMinute=CurMinute+MinutesPerItem
			//      IF (CurMinute > 60) THEN
			//        CurMinute=MinutesPerItem
			//        SCount=1
			//        Hr=Hr+1
			//      ENDIF
			//    ENDDO
			//    ! Now parcel into TS Value....
			//    IF (DaySchedule(Count)%IntervalInterpolated) THEN
			//      DO Hr=1,24
			//        SCount=1
			//        CurMinute=MinutesPerTimeStep
			//        DO TS=1,NumOfTimeStepInHour
			//          DaySchedule(Count)%TSValue(Hr,TS)=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
			//          SCount=CurMinute+1
			//          CurMinute=CurMinute+MinutesPerTimeStep
			//        ENDDO
			//      ENDDO
			//    ELSE
			//      DO Hr=1,24
			//        CurMinute=MinutesPerTimeStep
			//        DO TS=1,NumOfTimeStepInHour
			//          DaySchedule(Count)%TSValue(Hr,TS)=MinuteValue(Hr,CurMinute)
			//          Curminute=CurMinute+MinutesPerTimeStep
			//        ENDDO
			//      ENDDO
			//    ENDIF

			CheckForActualFileName( Alphas( 3 ), FileExists, TempFullFileName );

			//    INQUIRE(file=Alphas(3),EXIST=FileExists)
			// Setup file reading parameters
			if ( ! FileExists ) {
				DisplayString( "Missing " + Alphas( 3 ) );
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 3 ) + "=\"" + Alphas( 3 ) + "\" not found." );
				ShowContinueError( "Certain run environments require a full path to be included with the file name in the input field." );
				ShowContinueError( "Try again with putting full path and file name in the field." );
				ErrorsFound = true;
			} else {
				SchdFile = GetNewUnitNumber();
				{ IOFlags flags; flags.ACTION( "read" ); gio::open( SchdFile, TempFullFileName, flags ); read_stat = flags.ios(); }
				if ( read_stat != 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 3 ) + "=\"" + Alphas( 3 ) + "\" cannot be opened." );
					ShowContinueError( "... It may be open in another program (such as Excel).  Please close and try again." );
					ShowFatalError( "Program terminates due to previous condition." );
				}
				// check for stripping
				{ IOFlags flags; gio::read( SchdFile, fmtA, flags ) >> LineIn; read_stat = flags.ios(); }
				endLine = len( LineIn );
				if ( endLine > 0 ) {
					if ( int( LineIn[ endLine - 1 ] ) == iUnicode_end ) {
						gio::close( SchdFile );
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 3 ) + "=\"" + Alphas( 3 ) + " appears to be a Unicode or binary file." );
						ShowContinueError( "...This file cannot be read by this program. Please save as PC or Unix file and try again" );
						ShowFatalError( "Program terminates due to previous condition." );
					}
				}
				gio::backspace( SchdFile );

				// skip lines if any need to be skipped.
				numerrors = 0;
				rowCnt = 0;
				read_stat = 0;
				if ( skiprowCount > 0 ) { // Numbers(2) has number of rows to skip
					while ( read_stat == 0 ) { //end of file
						{ IOFlags flags; gio::read( SchdFile, fmtA, flags ) >> LineIn; read_stat = flags.ios(); }
						++rowCnt;
						if ( rowCnt == skiprowCount ) {
							break;
						}
					}
				}

				//  proper number of lines are skipped.  read the file
				// for the rest of the lines read from the file
				rowCnt = 0;
				firstLine = true;
				while ( read_stat == 0 ) { //end of file
					{ IOFlags flags; gio::read( SchdFile, fmtA, flags ) >> LineIn; read_stat = flags.ios(); }
					++rowCnt;
					colCnt = 0;
					wordStart = 0;
					columnValue = 0.0;
					//scan through the line looking for a specific column
					while ( true ) {
						sepPos = index( LineIn, ColumnSep );
						++colCnt;
						if ( sepPos != std::string::npos ) {
							if ( sepPos > 0 ) {
								wordEnd = sepPos - 1;
							} else {
								wordEnd = wordStart;
							}
							subString = LineIn.substr( wordStart, wordEnd - wordStart + 1 );
							//the next word will start after the comma
							wordStart = sepPos + 1;
							//get rid of separator so next INDEX will find next separator
							LineIn.erase( 0, wordStart );
							firstLine = false;
							wordStart = 0;
						} else {
							//no more commas
							subString = LineIn.substr( wordStart );
							if ( firstLine && subString == BlankString ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\" first line does not contain the indicated column separator=" + Alphas( 4 ) + '.' );
								ShowContinueError( "...first 40 characters of line=[" + LineIn.substr( 0, 40 ) + ']' );
								firstLine = false;
							}
							break;
						}
						if ( colCnt == curcolCount ) break;
					}
					if ( colCnt == curcolCount ) {
						columnValue = ProcessNumber( subString, errFlag );
						if ( errFlag ) {
							++numerrors;
							columnValue = 0.0;
						}
					} else {
						columnValue = 0.0;
					}
					hourlyFileValues( rowCnt ) = columnValue;
					if ( rowCnt == rowLimitCount ) break;
				}
				gio::close( SchdFile );

				// schedule values have been filled into the hourlyFileValues array.

				if ( numerrors > 0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\" " + RoundSigDigits( numerrors ) + " records had errors - these values are set to 0." );
					ShowContinueError( "Use Output:Diagnostics,DisplayExtraWarnings; to see individual records in error." );
				}
				if ( rowCnt < rowLimitCount ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\" less than " + RoundSigDigits( numHourlyValues ) + " hourly values read from file." );
					ShowContinueError( "..Number read=" + TrimSigDigits( ( rowCnt * 60 ) / MinutesPerItem ) + '.' );
				}
				if ( rowCnt < rowLimitCount ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\" less than specified hourly values read from file." );
					ShowContinueError( "..Specified Number of Hourly Values=" + TrimSigDigits( numHourlyValues ) + " Actual number of hourly values included=" + TrimSigDigits( ( rowCnt * 60 ) / MinutesPerItem ) );
				}
				// process the data into the normal schedule data structures
				// note -- schedules are ALWAYS 366 days so some special measures have to be done at 29 Feb "day of year" (60)
				iDay = 0;
				hDay = 0;
				ifld = 0;
				while ( true ) {
					// create string of which day of year
					++iDay;
					++hDay;
					if ( iDay > 366 ) break;
					ExtraField = RoundSigDigits( iDay );
					// increment both since a week schedule is being defined for each day so that a day is valid
					// no matter what the day type that is used in a design day.
					++AddWeekSch;
					++AddDaySch;
					// define week schedule
					WeekSchedule( AddWeekSch ).Name = Alphas( 1 ) + "_wk_" + ExtraField;
					// for all day types point the week schedule to the newly defined day schedule
					for ( kDayType = 1; kDayType <= MaxDayTypes; ++kDayType ) {
						WeekSchedule( AddWeekSch ).DaySchedulePointer( kDayType ) = AddDaySch;
					}
					// day schedule
					DaySchedule( AddDaySch ).Name = Alphas( 1 ) + "_dy_" + ExtraField;
					DaySchedule( AddDaySch ).ScheduleTypePtr = Schedule( SchNum ).ScheduleTypePtr;
					// schedule is pointing to the week schedule
					Schedule( SchNum ).WeekSchedulePointer( iDay ) = AddWeekSch;
					if ( MinutesPerItem == 60 ) {
						for ( jHour = 1; jHour <= 24; ++jHour ) {
							++ifld;
							curHrVal = hourlyFileValues( ifld ); // hourlyFileValues((hDay - 1) * 24 + jHour)
							for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
								DaySchedule( AddDaySch ).TSValue( TS, jHour ) = curHrVal;
							}
						}
					} else { // Minutes Per Item < 60
						for ( Hr = 1; Hr <= 24; ++Hr ) {
							CurMinute = MinutesPerItem;
							SCount = 1;
							for ( NumFields = 1; NumFields <= hrLimitCount; ++NumFields ) {
								++ifld;
								MinuteValue( {SCount,CurMinute}, Hr ) = hourlyFileValues( ifld );
								SCount = CurMinute + 1;
								CurMinute += MinutesPerItem;
							}
						}
						if ( FileIntervalInterpolated ) {
							for ( Hr = 1; Hr <= 24; ++Hr ) {
								SCount = 1;
								CurMinute = MinutesPerTimeStep;
								for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
									DaySchedule( AddDaySch ).TSValue( TS, Hr ) = sum( MinuteValue( {SCount,CurMinute}, Hr ) ) / double( MinutesPerTimeStep );
									SCount = CurMinute + 1;
									CurMinute += MinutesPerTimeStep;
								}
							}
						} else {
							for ( Hr = 1; Hr <= 24; ++Hr ) {
								CurMinute = MinutesPerTimeStep;
								for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
									DaySchedule( AddDaySch ).TSValue( TS, Hr ) = MinuteValue( CurMinute, Hr );
									CurMinute += MinutesPerTimeStep;
								}
							}
						}
					}
					if ( iDay == 59 && rowCnt < 8784 * hrLimitCount ) { // 28 Feb
						// Dup 28 Feb to 29 Feb (60)
						++iDay;
						Schedule( SchNum ).WeekSchedulePointer( iDay ) = Schedule( SchNum ).WeekSchedulePointer( iDay - 1 );
					}
				}
			}

			if ( AnyEnergyManagementSystemInModel ) { // setup constant schedules as actuators
				SetupEMSActuator( "Schedule:File", Schedule( SchNum ).Name, "Schedule Value", "[ ]", Schedule( SchNum ).EMSActuatedOn, Schedule( SchNum ).EMSValue );
			}
		}
		if ( NumCommaFileSchedules > 0 ) {
			hourlyFileValues.deallocate();
		}

		MinuteValue.deallocate();
		SetMinuteValue.deallocate();

		// Constant Schedules
		CurrentModuleObject = "Schedule:Constant";
		for ( LoopIndex = 1; LoopIndex <= NumConstantSchedules; ++LoopIndex ) {
			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++SchNum;
			Schedule( SchNum ).Name = Alphas( 1 );
			Schedule( SchNum ).SchType = ScheduleInput_constant;
			// Validate ScheduleType
			if ( NumScheduleTypes > 0 ) {
				CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
				if ( CheckIndex == 0 ) {
					if ( ! lAlphaBlanks( 2 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
					} else {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
					}
				} else {
					Schedule( SchNum ).ScheduleTypePtr = CheckIndex;
				}
			}
			++AddWeekSch;
			++AddDaySch;
			// define week schedule
			WeekSchedule( AddWeekSch ).Name = Alphas( 1 ) + "_wk_";
			// for all day types point the week schedule to the newly defined day schedule
			for ( kDayType = 1; kDayType <= MaxDayTypes; ++kDayType ) {
				WeekSchedule( AddWeekSch ).DaySchedulePointer( kDayType ) = AddDaySch;
			}
			// day schedule
			DaySchedule( AddDaySch ).Name = Alphas( 1 ) + "_dy_";
			DaySchedule( AddDaySch ).ScheduleTypePtr = Schedule( SchNum ).ScheduleTypePtr;
			// schedule is pointing to the week schedule
			Schedule( SchNum ).WeekSchedulePointer = AddWeekSch;
			curHrVal = Numbers( 1 );
			DaySchedule( AddDaySch ).TSValue = Numbers( 1 );

			if ( AnyEnergyManagementSystemInModel ) { // setup constant schedules as actuators
				SetupEMSActuator( "Schedule:Constant", Schedule( SchNum ).Name, "Schedule Value", "[ ]", Schedule( SchNum ).EMSActuatedOn, Schedule( SchNum ).EMSValue );
			}
		}

		CurrentModuleObject = "ExternalInterface:Schedule";
		for ( LoopIndex = 1; LoopIndex <= NumExternalInterfaceSchedules; ++LoopIndex ) {

			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;

			VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" ); // Bug fix
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++SchNum;
			Schedule( SchNum ).Name = Alphas( 1 );
			Schedule( SchNum ).SchType = ScheduleInput_external;

			// Validate ScheduleType
			CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
			if ( CheckIndex == 0 ) {
				if ( ! lAlphaBlanks( 2 ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
				} else {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
				}
			} else {
				Schedule( SchNum ).ScheduleTypePtr = CheckIndex;
			}
			++AddWeekSch;
			WeekSchedule( AddWeekSch ).Name = Alphas( 1 );
			WeekSchedule( AddWeekSch ).Used = true;
			for ( Hr = 1; Hr <= 366; ++Hr ) {
				Schedule( SchNum ).WeekSchedulePointer( Hr ) = AddWeekSch;
			}
			++AddDaySch;
			DaySchedule( AddDaySch ).Name = Alphas( 1 );
			DaySchedule( AddDaySch ).ScheduleTypePtr = Schedule( SchNum ).ScheduleTypePtr;
			DaySchedule( AddDaySch ).Used = true;
			for ( Hr = 1; Hr <= MaxDayTypes; ++Hr ) {
				WeekSchedule( AddWeekSch ).DaySchedulePointer( Hr ) = AddDaySch;
			}
			//   Initialize the ExternalInterface day schedule for the ExternalInterface compact schedule.
			//   It will be overwritten during run time stepping after the warm up period
			if ( NumNumbers < 1 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", initial value is not numeric or is missing. Fix idf file." );
				NumErrorFlag = true;
			}
			ExternalInterfaceSetSchedule( AddDaySch, Numbers( 1 ) );

		}
		// added for FMU Import
		CurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Schedule";
		for ( LoopIndex = 1; LoopIndex <= NumExternalInterfaceFunctionalMockupUnitImportSchedules; ++LoopIndex ) {

			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;

			if ( NumExternalInterfaceSchedules >= 1 ) {
				VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, "The schedule object with the name \"" + Alphas( 1 ) + "\" is defined as an ExternalInterface:Schedule and ExternalInterface:FunctionalMockupUnitImport:To:Schedule. This will cause the schedule to be overwritten by PtolemyServer and FunctionalMockUpUnitImport." );
			} else {
				VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			}
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// END IF
			++SchNum;
			Schedule( SchNum ).Name = Alphas( 1 );
			Schedule( SchNum ).SchType = ScheduleInput_external;

			// Validate ScheduleType
			CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
			if ( CheckIndex == 0 ) {
				if ( ! lAlphaBlanks( 2 ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
				} else {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
				}
			} else {
				Schedule( SchNum ).ScheduleTypePtr = CheckIndex;
			}
			++AddWeekSch;
			WeekSchedule( AddWeekSch ).Name = Alphas( 1 );
			WeekSchedule( AddWeekSch ).Used = true;
			for ( Hr = 1; Hr <= 366; ++Hr ) {
				Schedule( SchNum ).WeekSchedulePointer( Hr ) = AddWeekSch;
			}
			++AddDaySch;
			DaySchedule( AddDaySch ).Name = Alphas( 1 );
			DaySchedule( AddDaySch ).ScheduleTypePtr = Schedule( SchNum ).ScheduleTypePtr;
			DaySchedule( AddDaySch ).Used = true;
			for ( Hr = 1; Hr <= MaxDayTypes; ++Hr ) {
				WeekSchedule( AddWeekSch ).DaySchedulePointer( Hr ) = AddDaySch;
			}
			//   Initialize the ExternalInterface day schedule for the ExternalInterface compact schedule.
			//   It will be overwritten during run time stepping after the warm up period
			if ( NumNumbers < 1 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", initial value is not numeric or is missing. Fix idf file." );
				NumErrorFlag = true;
			}
			ExternalInterfaceSetSchedule( AddDaySch, Numbers( 1 ) );

		}

		// added for FMU Export
		CurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Schedule";
		for ( LoopIndex = 1; LoopIndex <= NumExternalInterfaceFunctionalMockupUnitExportSchedules; ++LoopIndex ) {

			GetObjectItem( CurrentModuleObject, LoopIndex, Alphas, NumAlphas, Numbers, NumNumbers, Status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;

			if ( NumExternalInterfaceSchedules >= 1 ) {
				VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, "The schedule object with the name \"" + Alphas( 1 ) + "\" is defined as an ExternalInterface:Schedule and ExternalInterface:FunctionalMockupUnitExport:To:Schedule. This will cause the schedule to be overwritten by PtolemyServer and FunctionalMockUpUnitExport." );
			} else {
				VerifyName( Alphas( 1 ), Schedule( {1,NumSchedules} ), SchNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			}
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}

			++SchNum;
			Schedule( SchNum ).Name = Alphas( 1 );
			Schedule( SchNum ).SchType = ScheduleInput_external;

			// Validate ScheduleType
			CheckIndex = FindItemInList( Alphas( 2 ), ScheduleType( {1,NumScheduleTypes} ) );
			if ( CheckIndex == 0 ) {
				if ( ! lAlphaBlanks( 2 ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" not found -- will not be validated" );
				} else {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Blank " + cAlphaFields( 2 ) + " input -- will not be validated." );
				}
			} else {
				Schedule( SchNum ).ScheduleTypePtr = CheckIndex;
			}
			++AddWeekSch;
			WeekSchedule( AddWeekSch ).Name = Alphas( 1 );
			WeekSchedule( AddWeekSch ).Used = true;
			for ( Hr = 1; Hr <= 366; ++Hr ) {
				Schedule( SchNum ).WeekSchedulePointer( Hr ) = AddWeekSch;
			}
			++AddDaySch;
			DaySchedule( AddDaySch ).Name = Alphas( 1 );
			DaySchedule( AddDaySch ).ScheduleTypePtr = Schedule( SchNum ).ScheduleTypePtr;
			DaySchedule( AddDaySch ).Used = true;
			for ( Hr = 1; Hr <= MaxDayTypes; ++Hr ) {
				WeekSchedule( AddWeekSch ).DaySchedulePointer( Hr ) = AddDaySch;
			}
			//   Initialize the ExternalInterface day schedule for the ExternalInterface compact schedule.
			//   It will be overwritten during run time stepping after the warm up period
			if ( NumNumbers < 1 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", initial value is not numeric or is missing. Fix idf file." );
				NumErrorFlag = true;
			}
			ExternalInterfaceSetSchedule( AddDaySch, Numbers( 1 ) );

		}

		// Validate by ScheduleLimitsType
		for ( SchNum = 1; SchNum <= NumSchedules; ++SchNum ) {
			NumPointer = Schedule( SchNum ).ScheduleTypePtr;
			if ( ! ScheduleType( NumPointer ).Limited ) continue;
			if ( CheckScheduleValueMinMax( SchNum, ">=", ScheduleType( NumPointer ).Minimum, "<=", ScheduleType( NumPointer ).Maximum ) ) continue;
			ShowSevereError( RoutineName + "Schedule=\"" + Schedule( SchNum ).Name + "\" has values outside its Schedule Type (" + ScheduleType( NumPointer ).Name + ") range" );
			ShowContinueError( "  Minimum should be >=" + RoundSigDigits( ScheduleType( NumPointer ).Minimum, 3 ) + " and Maximum should be <=" + RoundSigDigits( ScheduleType( NumPointer ).Maximum, 3 ) );
			ErrorsFound = true;
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Preceding Errors cause termination." );
		}

		if ( NumScheduleTypes + NumDaySchedules + NumWeekSchedules + NumSchedules > 0 ) { // Report to EIO file
			CurrentModuleObject = "Output:Schedules";
			NumFields = GetNumObjectsFound( CurrentModuleObject );

			//    RptSchedule=.FALSE.
			RptLevel = 1;
			for ( Count = 1; Count <= NumFields; ++Count ) {
				GetObjectItem( CurrentModuleObject, Count, Alphas, NumAlphas, Numbers, NumNumbers, Status );
				//      RptSchedule=.TRUE.

				{ auto const SELECT_CASE_var( Alphas( 1 ) );

				if ( SELECT_CASE_var == "HOURLY" ) {
					RptLevel = 1;
					ReportScheduleDetails( RptLevel );

				} else if ( ( SELECT_CASE_var == "TIMESTEP" ) || ( SELECT_CASE_var == "DETAILED" ) ) {
					RptLevel = 2;
					ReportScheduleDetails( RptLevel );

				} else if ( SELECT_CASE_var == "IDF" ) {
					RptLevel = 3;
					ReportScheduleDetails( RptLevel );

				} else {
					ShowWarningError( RoutineName + "Report for Schedules should specify \"HOURLY\" or \"TIMESTEP\" (\"DETAILED\")" );
					ShowContinueError( "HOURLY report will be done" );
					RptLevel = 1;
					ReportScheduleDetails( RptLevel );

				}}
			}
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		gio::write( UnitNumber, fmtLD ) << " Processing Schedule Input -- Complete";

	}

	void
	ReportScheduleDetails( int const LevelOfDetail ) // =1: hourly; =2: timestep; = 3: make IDF excerpt
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   January 2003
		//       MODIFIED       February 2008 - add IDF outputs (compact schedules)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine puts the details of the Schedules on the .eio file (Inits file).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using General::InvJulianDay;
		using DataGlobals::OutputFileDebug;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const Months( 12, { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" } );
		static Array1D_string const HrField( {0,24}, { "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24" } );
		static gio::Fmt SchTFmt0( "('! Schedule Details Report=',A,' =====================')" );
		static gio::Fmt SchTFmt( "('! <ScheduleType>,Name,Limited? {Yes/No},Minimum,Maximum,',   'Continuous? {Yes/No - Discrete}')" );
		static gio::Fmt SchSFmt( "('! <Schedule>,Name,ScheduleType,{Until Date,WeekSchedule}** Repeated until Dec 31')" );
		static gio::Fmt SchTFmtdata( "('ScheduleTypeLimits',5(',',A))" );
		static gio::Fmt SchWFmtdata( "('Schedule:Week:Daily',',',A,$)" );
		static gio::Fmt CMinFmt( "(I2.2)" );
		static gio::Fmt ThruFmt( "(',Through ',A,1X,I2.2,',',A)" );
		static gio::Fmt SchDFmt0( "('! <DaySchedule>,Name,ScheduleType,Interpolated {Yes/No},Time (HH:MM) =>',$)" );
		static gio::Fmt SchDFmtdata0( "('DaySchedule,',A,',',A,',',A,',',A,$)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count;
		int Hr;
		int TS;
		int NumF;
		int PMon;
		int PDay;
		int iWeek;
		int iDay;
		int DT;
		int iDayP;
		Array1D_string ShowMinute;
		int CurMinute;
		Array1D_string TimeHHMM;
		std::string SchWFmt( "('! <WeekSchedule>,Name" );
		std::string SchDFmt;
		std::string SchDFmtdata;
		std::string YesNo1;
		std::string YesNo2;
		std::string Num1;
		std::string Num2;
		Array2D_string RoundTSValue;

		ShowMinute.allocate( NumOfTimeStepInHour );
		TimeHHMM.allocate( NumOfTimeStepInHour * 24 );
		RoundTSValue.allocate( NumOfTimeStepInHour, 24 );
		ShowMinute = BlankString;
		TimeHHMM = BlankString;
		RoundTSValue = BlankString;

		CurMinute = MinutesPerTimeStep;
		for ( Count = 1; Count <= NumOfTimeStepInHour - 1; ++Count ) {
			gio::write( ShowMinute( Count ), CMinFmt ) << CurMinute;
			CurMinute += MinutesPerTimeStep;
		}
		ShowMinute( NumOfTimeStepInHour ) = "00";

		{ auto const SELECT_CASE_var( LevelOfDetail );

		if ( ( SELECT_CASE_var >= 1 ) && ( SELECT_CASE_var <= 2 ) ) {
			NumF = 1;
			for ( Hr = 1; Hr <= 24; ++Hr ) {
				if ( LevelOfDetail == 2 ) {
					for ( TS = 1; TS <= NumOfTimeStepInHour - 1; ++TS ) {
						TimeHHMM( NumF ) = HrField( Hr - 1 ) + ':' + ShowMinute( TS );
						++NumF;
					}
				}
				TimeHHMM( NumF ) = HrField( Hr ) + ':' + ShowMinute( NumOfTimeStepInHour );
				++NumF;
			}
			--NumF;

			// SchTFmt Schedule Types Header
			if ( LevelOfDetail == 1 ) {
				gio::write( OutputFileInits, SchTFmt0 ) << "Hourly";
				SchDFmt = "(',',A,$)";
				SchDFmtdata = "(',',A,$)";
			} else {
				gio::write( OutputFileInits, SchTFmt0 ) << "Timestep";
				gio::write( Num1, fmtLD ) << NumOfTimeStepInHour * 24;
				strip( Num1 );
				SchDFmt = "(" + Num1 + ",',',A,$)";
				SchDFmtdata = "(" + Num1 + ",',',A,$)";
			}

			gio::write( OutputFileInits, SchTFmt );
			// SchDFmt Header (DaySchedule) builds the appropriate set of commas/times based on detail level
			//      DO Count=1,NumF
			//        SchDFmt=TRIM(SchDFmt)//'A'
			//        IF (Count /= NumF) SchDFmt=TRIM(SchDFmt)//",',',"
			//      ENDDO
			//      SchDFmt=TRIM(SchDFmt)//')'
			gio::write( OutputFileInits, SchDFmt0 );
			for ( Count = 1; Count <= NumF; ++Count ) {
				gio::write( OutputFileInits, SchDFmt ) << TimeHHMM( Count );
			} gio::write( OutputFileInits );
			// SchWFmt Header (WeekSchedule)
			for ( Count = 1; Count <= MaxDayTypes; ++Count ) {
				SchWFmt += "," + ValidDayTypes( Count );
			}
			SchWFmt += "')";
			gio::write( OutputFileInits, SchWFmt );
			gio::write( OutputFileInits, SchSFmt );

			for ( Count = 1; Count <= NumScheduleTypes; ++Count ) {
				if ( ScheduleType( Count ).Limited ) {
					YesNo1 = "Yes";
					Num1 = RoundSigDigits( ScheduleType( Count ).Minimum, 2 );
					strip( Num1 );
					Num2 = RoundSigDigits( ScheduleType( Count ).Maximum, 2 );
					strip( Num2 );
					if ( ScheduleType( Count ).IsReal ) {
						YesNo2 = "Yes";
					} else {
						YesNo2 = "No";
						gio::write( Num1, fmtLD ) << int( ScheduleType( Count ).Minimum );
						strip( Num1 );
						gio::write( Num2, fmtLD ) << int( ScheduleType( Count ).Maximum );
						strip( Num2 );
					}
				} else {
					YesNo1 = "No";
					Num1 = "N/A";
					Num2 = "N/A";
					YesNo2 = "N/A";
				}
				gio::write( OutputFileInits, SchTFmtdata ) << ScheduleType( Count ).Name << YesNo1 << Num1 << Num2 << YesNo2;
			}

			//      WRITE(Num1,*) NumOfTimeStepInHour*24
			//      Num1=ADJUSTL(Num1)
			//      SchDFmtdata=TRIM(SchDFmtdata)//TRIM(Num1)//"(',',A))"
			for ( Count = 1; Count <= NumDaySchedules; ++Count ) {
				if ( DaySchedule( Count ).IntervalInterpolated ) {
					YesNo1 = "Yes";
				} else {
					YesNo1 = "No";
				}
				for ( Hr = 1; Hr <= 24; ++Hr ) {
					for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
						RoundTSValue( TS, Hr ) = RoundSigDigits( DaySchedule( Count ).TSValue( TS, Hr ), 2 );
					}
				}
				if ( LevelOfDetail == 1 ) {
					gio::write( OutputFileInits, SchDFmtdata0 )
						<< DaySchedule( Count ).Name
						<< ScheduleType( DaySchedule( Count ).ScheduleTypePtr ).Name
						<< YesNo1
						<< "Values:";
					for ( Hr = 1; Hr <= 24; ++Hr ) {
						gio::write( OutputFileInits, SchDFmtdata )
							<< RoundTSValue( NumOfTimeStepInHour, Hr );
					} gio::write( OutputFileInits );
				} else if ( LevelOfDetail == 2 ) {
					gio::write( OutputFileInits, SchDFmtdata0 )
						<< DaySchedule( Count ).Name
						<< ScheduleType( DaySchedule( Count ).ScheduleTypePtr ).Name
						<< YesNo1
						<< "Values:";
					for ( Hr = 1; Hr <= 24; ++Hr ) {
						for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
							gio::write( OutputFileInits, SchDFmtdata )
								<< RoundTSValue( TS, Hr );
						}
					} gio::write( OutputFileInits );
				}
			}

			for ( Count = 1; Count <= NumWeekSchedules; ++Count ) {
				gio::write( OutputFileInits, SchWFmtdata )
					<< WeekSchedule( Count ).Name;
				for ( NumF = 1; NumF <= MaxDayTypes; ++NumF ) {
					gio::write( OutputFileInits, "(',',A,$)" )
						<< DaySchedule( WeekSchedule( Count ).DaySchedulePointer( NumF ) ).Name;
				} gio::write( OutputFileInits );
			}

			for ( Count = 1; Count <= NumSchedules; ++Count ) {
				NumF = 1;
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, "('Schedule,',A,',',A)", flags ) << Schedule( Count ).Name << ScheduleType( Schedule( Count ).ScheduleTypePtr ).Name; }
				while ( NumF <= 366 ) {
					TS = Schedule( Count ).WeekSchedulePointer( NumF );
					while ( Schedule( Count ).WeekSchedulePointer( NumF ) == TS && NumF <= 366 ) {
						if ( NumF == 366 ) {
							InvJulianDay( NumF, PMon, PDay, 1 );
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, ThruFmt, flags ) << Months( PMon ) << PDay << WeekSchedule( TS ).Name; }
						}
						++NumF;
						if ( NumF > 366 ) break; // compound If might have a problem unless this included.
					}
					if ( NumF <= 366 ) {
						InvJulianDay( NumF - 1, PMon, PDay, 1 );
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, ThruFmt, flags ) << Months( PMon ) << PDay << WeekSchedule( TS ).Name; }
					}
				}
				gio::write( OutputFileInits, "(1X)" );
			}

		} else if ( SELECT_CASE_var == 3 ) {
			for ( Count = 1; Count <= NumSchedules; ++Count ) {
				gio::write( OutputFileDebug );
				gio::write( OutputFileDebug, fmtA ) << "  Schedule:Compact,";
				gio::write( OutputFileDebug, fmtA ) << "    " + Schedule( Count ).Name + ",           !- Name";
				gio::write( OutputFileDebug, fmtA ) << "    " + ScheduleType( Schedule( Count ).ScheduleTypePtr ).Name + ",          !- ScheduleTypeLimits";
				NumF = 1;
				while ( NumF <= 366 ) {
					TS = Schedule( Count ).WeekSchedulePointer( NumF );
					while ( Schedule( Count ).WeekSchedulePointer( NumF ) == TS && NumF <= 366 ) {
						if ( NumF == 366 ) {
							InvJulianDay( NumF, PMon, PDay, 1 );
							gio::write( OutputFileDebug, fmtA ) << "    Through: " + RoundSigDigits( PMon ) + '/' + RoundSigDigits( PDay ) + ',';
							iDayP = 0;
							for ( DT = 2; DT <= 6; ++DT ) {
								gio::write( OutputFileDebug, fmtA ) << "    For: " + ValidDayTypes( DT ) + ',';
								iWeek = Schedule( Count ).WeekSchedulePointer( NumF - 1 );
								iDay = WeekSchedule( iWeek ).DaySchedulePointer( DT );
								if ( iDay != iDayP ) {
									for ( Hr = 1; Hr <= 24; ++Hr ) {
										gio::write( OutputFileDebug, fmtA ) << "    Until: " + RoundSigDigits( Hr ) + ':' + ShowMinute( NumOfTimeStepInHour ) + ',' + RoundSigDigits( DaySchedule( iDay ).TSValue( NumOfTimeStepInHour, Hr ), 2 ) + ',';
									}
								} else {
									gio::write( OutputFileDebug, fmtA ) << "    Same as previous";
								}
								iDayP = iDay;
							}
							DT = 1;
							gio::write( OutputFileDebug, fmtA ) << "    For: " + ValidDayTypes( DT ) + ',';
							iWeek = Schedule( Count ).WeekSchedulePointer( NumF - 1 );
							iDay = WeekSchedule( iWeek ).DaySchedulePointer( DT );
							if ( iDay != iDayP ) {
								for ( Hr = 1; Hr <= 24; ++Hr ) {
									gio::write( OutputFileDebug, fmtA ) << "    Until: " + RoundSigDigits( Hr ) + ':' + ShowMinute( NumOfTimeStepInHour ) + ',' + RoundSigDigits( DaySchedule( iDay ).TSValue( NumOfTimeStepInHour, Hr ), 2 ) + ',';
								}
							} else {
								gio::write( OutputFileDebug, fmtA ) << "    Same as previous";
							}
							iDayP = iDay;
							for ( DT = 7; DT <= MaxDayTypes; ++DT ) {
								gio::write( OutputFileDebug, fmtA ) << "    For: " + ValidDayTypes( DT ) + ',';
								iWeek = Schedule( Count ).WeekSchedulePointer( NumF - 1 );
								iDay = WeekSchedule( iWeek ).DaySchedulePointer( DT );
								if ( iDay != iDayP ) {
									for ( Hr = 1; Hr <= 24; ++Hr ) {
										gio::write( OutputFileDebug, fmtA ) << "    Until: " + RoundSigDigits( Hr ) + ':' + ShowMinute( NumOfTimeStepInHour ) + ',' + RoundSigDigits( DaySchedule( iDay ).TSValue( NumOfTimeStepInHour, Hr ), 2 ) + ',';
									}
								} else {
									gio::write( OutputFileDebug, fmtA ) << "    Same as previous";
								}
								iDayP = iDay;
							}
						}
						++NumF;
						if ( NumF > 366 ) break; // compound If might have a problem unless this included.
					}
					if ( NumF <= 366 ) {
						InvJulianDay( NumF - 1, PMon, PDay, 1 );
						gio::write( OutputFileDebug, fmtA ) << "    Through: " + RoundSigDigits( PMon ) + '/' + RoundSigDigits( PDay ) + ',';
						iDayP = 0;
						for ( DT = 2; DT <= 6; ++DT ) {
							gio::write( OutputFileDebug, fmtA ) << "    For: " + ValidDayTypes( DT ) + ',';
							iWeek = Schedule( Count ).WeekSchedulePointer( NumF - 1 );
							iDay = WeekSchedule( iWeek ).DaySchedulePointer( DT );
							if ( iDay != iDayP ) {
								for ( Hr = 1; Hr <= 24; ++Hr ) {
									gio::write( OutputFileDebug, fmtA ) << "    Until: " + RoundSigDigits( Hr ) + ':' + ShowMinute( NumOfTimeStepInHour ) + ',' + RoundSigDigits( DaySchedule( iDay ).TSValue( NumOfTimeStepInHour, Hr ), 2 ) + ',';
								}
							} else {
								gio::write( OutputFileDebug, fmtA ) << "    Same as previous";
							}
							iDayP = iDay;
						}
						DT = 1;
						gio::write( OutputFileDebug, fmtA ) << "    For: " + ValidDayTypes( DT ) + ',';
						iWeek = Schedule( Count ).WeekSchedulePointer( NumF - 1 );
						iDay = WeekSchedule( iWeek ).DaySchedulePointer( DT );
						if ( iDay != iDayP ) {
							for ( Hr = 1; Hr <= 24; ++Hr ) {
								gio::write( OutputFileDebug, fmtA ) << "    Until: " + RoundSigDigits( Hr ) + ':' + ShowMinute( NumOfTimeStepInHour ) + ',' + RoundSigDigits( DaySchedule( iDay ).TSValue( NumOfTimeStepInHour, Hr ), 2 ) + ',';
							}
						} else {
							gio::write( OutputFileDebug, fmtA ) << "    Same as previous";
						}
						iDayP = iDay;
						for ( DT = 7; DT <= MaxDayTypes; ++DT ) {
							gio::write( OutputFileDebug, fmtA ) << "    For: " + ValidDayTypes( DT ) + ',';
							iWeek = Schedule( Count ).WeekSchedulePointer( NumF - 1 );
							iDay = WeekSchedule( iWeek ).DaySchedulePointer( DT );
							if ( iDay != iDayP ) {
								for ( Hr = 1; Hr <= 24; ++Hr ) {
									gio::write( OutputFileDebug, fmtA ) << "    Until: " + RoundSigDigits( Hr ) + ':' + ShowMinute( NumOfTimeStepInHour ) + ',' + RoundSigDigits( DaySchedule( iDay ).TSValue( NumOfTimeStepInHour, Hr ), 2 ) + ',';
								}
							} else {
								gio::write( OutputFileDebug, fmtA ) << "    Same as previous";
							}
							iDayP = iDay;
						}
					}
				}
			}

		} else {
		}}

		ShowMinute.deallocate();
		TimeHHMM.deallocate();
		RoundTSValue.deallocate();

	}

	Real64
	GetCurrentScheduleValue( int const ScheduleIndex )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       August 2011; adapt Autodesk changes (time reduction)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the hourly schedule value for the current day.

		// METHODOLOGY EMPLOYED:
		// Use internal Schedule data structure to return value.  Note that missing values in
		// input will equate to 0 indices in arrays -- which has been set up to return legally with
		// 0.0 values.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! ScheduleDSTSFileWarningIssued ) {
			if ( DSTIndicator == 1 ) {
				if ( Schedule( ScheduleIndex ).SchType == ScheduleInput_file ) {
					ShowWarningError( "GetCurrentScheduleValue: Schedule=\"" + Schedule( ScheduleIndex ).Name + "\" is a Schedule:File" );
					ShowContinueError( "...Use of Schedule:File when DaylightSavingTime is in effect is not recommended." );
					ShowContinueError( "...1) Remove RunperiodControl:DaylightSavingTime object or remove DST period from Weather File." );
					ShowContinueError( "...2) Configure other schedules and Schedule:File to account for occupant behavior during DST." );
					ShowContinueError( "...   If you have already done this, you can ignore this message." );
					ShowContinueError( "...When active, DaylightSavingTime will shift all scheduled items by one hour, retaining the same day type as the original." );
					ScheduleDSTSFileWarningIssued = true;
				}
			}
		}

		if ( ScheduleIndex == -1 ) {
			return 1.0;
		} else if ( ScheduleIndex == 0 ) {
			return 0.0;
		} else if ( ! Schedule( ScheduleIndex ).EMSActuatedOn ) {
			return Schedule( ScheduleIndex ).CurrentValue;
		} else {
			return Schedule( ScheduleIndex ).EMSValue;
		}
	}

	void
	UpdateScheduleValues()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2011; adapted from Autodesk (time reduction)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine calculates all the scheduled values as a time reduction measure and
		// stores them in the CurrentValue item of the schedule data structure.

		// METHODOLOGY EMPLOYED:
		// Use internal Schedule data structure to calculate current value.  Note that missing values in
		// input will equate to 0 indices in arrays -- which has been set up to return legally with
		// 0.0 values.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::DayOfYear_Schedule;

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
		int ScheduleIndex;
		int WhichHour;
		int WeekSchedulePointer;
		int DaySchedulePointer;

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		WhichHour = HourOfDay + DSTIndicator;

		for ( ScheduleIndex = 1; ScheduleIndex <= NumSchedules; ++ScheduleIndex ) {

			// Determine which Week Schedule is used
			//  Cant use stored day of year because of leap year inconsistency
			WeekSchedulePointer = Schedule( ScheduleIndex ).WeekSchedulePointer( DayOfYear_Schedule );

			// Now, which day?
			if ( DayOfWeek <= 7 && HolidayIndex > 0 ) {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( 7 + HolidayIndex );
			} else {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( DayOfWeek );
			}

			// Hourly Value
			if ( WhichHour <= 24 ) {
				Schedule( ScheduleIndex ).CurrentValue = DaySchedule( DaySchedulePointer ).TSValue( TimeStep, WhichHour );
			} else if ( TimeStep <= NumOfTimeStepInHour ) {
				Schedule( ScheduleIndex ).CurrentValue = DaySchedule( DaySchedulePointer ).TSValue( TimeStep, WhichHour - 24 );
			} else {
				Schedule( ScheduleIndex ).CurrentValue = DaySchedule( DaySchedulePointer ).TSValue( NumOfTimeStepInHour, WhichHour - 24 );
			}

		}

	}

	Real64
	LookUpScheduleValue(
		int const ScheduleIndex,
		int const ThisHour, // Negative => unspecified
		int const ThisTimeStep  // Negative => unspecified
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   January 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides a method to look up schedule values for any hour, timestep, day
		// of the year (rather than just the "current time").

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::DayOfYear_Schedule;
		using General::JulianDay;

		// Return value
		Real64 LookUpScheduleValue( 0.0 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WeekSchedulePointer;
		int DaySchedulePointer;
		int WhichHour;
		int WhichTimeStep;

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		if ( ScheduleIndex == -1 ) {
			LookUpScheduleValue = 1.0;
			return LookUpScheduleValue;
		} else if ( ScheduleIndex == 0 ) {
			LookUpScheduleValue = 0.0;
			return LookUpScheduleValue;
		}

		if ( ThisHour < 0 ) { // ThisHour unspecified
			LookUpScheduleValue = GetCurrentScheduleValue( ScheduleIndex );

			//  ELSEIF (ThisHour == 0) THEN  ! odd answers when thishour=0 (initialization of shadowing)
			//    LookUpScheduleValue=GetCurrentScheduleValue(ScheduleIndex)

		} else { // ThisHour specified
			//  so, current date, but maybe TimeStep added

			// Determine which Week Schedule is used
			//  Cant use stored day of year because of leap year inconsistency
			WeekSchedulePointer = Schedule( ScheduleIndex ).WeekSchedulePointer( DayOfYear_Schedule );

			// Now, which day?
			if ( DayOfWeek <= 7 && HolidayIndex > 0 ) {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( 7 + HolidayIndex );
			} else {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( DayOfWeek );
			}

			// Hourly Value
			WhichHour = HourOfDay + DSTIndicator;
			if ( WhichHour <= 24 ) {
				LookUpScheduleValue = DaySchedule( DaySchedulePointer ).TSValue( TimeStep, WhichHour );
			} else {
				LookUpScheduleValue = DaySchedule( DaySchedulePointer ).TSValue( TimeStep, WhichHour - 24 );
			}
			WhichHour = ThisHour;
			while ( WhichHour < 1 ) {
				WhichHour += 24;
			}
			if ( WhichHour > 24 ) {
				while ( WhichHour > 24 ) {
					WeekSchedulePointer = Schedule( ScheduleIndex ).WeekSchedulePointer( JulianDay( MonthTomorrow, DayOfMonthTomorrow, 1 ) );
					if ( DayOfWeekTomorrow <= 7 && HolidayIndexTomorrow > 0 ) {
						DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( 7 + HolidayIndexTomorrow );
					} else {
						DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( DayOfWeekTomorrow );
					}
					WhichHour -= 24;
				}
			} else {
				// Determine which Week Schedule is used
				//  Cant use stored day of year because of leap year inconsistency
				WeekSchedulePointer = Schedule( ScheduleIndex ).WeekSchedulePointer( DayOfYear_Schedule );

				// Now, which day?
				if ( DayOfWeek <= 7 && HolidayIndex > 0 ) {
					DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( 7 + HolidayIndex );
				} else {
					DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( DayOfWeek );
				}
			}
			WhichHour += DSTIndicator;
			if ( ThisTimeStep >= 0 ) { // ThisTimeStep specified
				if ( ThisTimeStep == 0 ) {
					WhichTimeStep = NumOfTimeStepInHour;
				} else {
					WhichTimeStep = ThisTimeStep;
				}
				if ( WhichHour <= 24 ) {
					LookUpScheduleValue = DaySchedule( DaySchedulePointer ).TSValue( WhichTimeStep, WhichHour );
				} else if ( ThisTimeStep <= NumOfTimeStepInHour ) {
					LookUpScheduleValue = DaySchedule( DaySchedulePointer ).TSValue( WhichTimeStep, WhichHour - 24 );
				} else {
					LookUpScheduleValue = DaySchedule( DaySchedulePointer ).TSValue( NumOfTimeStepInHour, WhichHour - 24 );
				}
			} else {
				if ( WhichHour <= 24 ) {
					LookUpScheduleValue = DaySchedule( DaySchedulePointer ).TSValue( NumOfTimeStepInHour, WhichHour );
				} else {
					LookUpScheduleValue = DaySchedule( DaySchedulePointer ).TSValue( NumOfTimeStepInHour, WhichHour - 24 );
				}
			}

		}

		return LookUpScheduleValue;

	}

	int
	GetScheduleIndex( std::string const & ScheduleName )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the internal pointer to Schedule "ScheduleName".

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetScheduleIndex;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int DayCtr;
		int WeekCtr;

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		if ( NumSchedules > 0 ) {
			GetScheduleIndex = FindItemInList( ScheduleName, Schedule( {1,NumSchedules} ) );
			if ( GetScheduleIndex > 0 ) {
				if ( ! Schedule( GetScheduleIndex ).Used ) {
					Schedule( GetScheduleIndex ).Used = true;
					for ( WeekCtr = 1; WeekCtr <= 366; ++WeekCtr ) {
						if ( Schedule( GetScheduleIndex ).WeekSchedulePointer( WeekCtr ) > 0 ) {
							WeekSchedule( Schedule( GetScheduleIndex ).WeekSchedulePointer( WeekCtr ) ).Used = true;
							for ( DayCtr = 1; DayCtr <= MaxDayTypes; ++DayCtr ) {
								DaySchedule( WeekSchedule( Schedule( GetScheduleIndex ).WeekSchedulePointer( WeekCtr ) ).DaySchedulePointer( DayCtr ) ).Used = true;
							}
						}
					}
				}
			}
		} else {
			GetScheduleIndex = 0;
		}

		return GetScheduleIndex;

	}

	std::string
	GetScheduleType( int const ScheduleIndex )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the internal pointer to Schedule "ScheduleName".

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		std::string TypeOfSchedule;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int curSchType;

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		if ( ( ScheduleIndex > 0 ) && ( ScheduleIndex <= NumSchedules ) ) {
			curSchType = Schedule( ScheduleIndex ).ScheduleTypePtr;
			if ( ( curSchType > 0 ) && ( curSchType <= NumScheduleTypes ) ) {
				TypeOfSchedule = ScheduleType( curSchType ).Name;
			} else {
				TypeOfSchedule = "";
			}
		} else {
			TypeOfSchedule = "";
		}
		return TypeOfSchedule;
	}

	int
	GetDayScheduleIndex( std::string & ScheduleName )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the internal pointer to Day Schedule "ScheduleName".

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetDayScheduleIndex;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		if ( NumDaySchedules > 0 ) {
			GetDayScheduleIndex = FindItemInList( ScheduleName, DaySchedule( {1,NumDaySchedules} ) );
			if ( GetDayScheduleIndex > 0 ) {
				DaySchedule( GetDayScheduleIndex ).Used = true;
			}
		} else {
			GetDayScheduleIndex = 0;
		}

		return GetDayScheduleIndex;

	}

	void
	GetScheduleValuesForDay(
		int const ScheduleIndex,
		Array2S< Real64 > DayValues,
		Optional_int_const JDay,
		Optional_int_const CurDayofWeek
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns an entire day's worth of schedule values.

		// METHODOLOGY EMPLOYED:
		// Use internal data to fill DayValues array.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::DayOfYear_Schedule;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WeekSchedulePointer;
		int DaySchedulePointer;

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		if ( ScheduleIndex == -1 ) {
			DayValues( {1,NumOfTimeStepInHour}, {1,24} ) = 1.0;
			return;
		} else if ( ScheduleIndex == 0 ) {
			DayValues( {1,NumOfTimeStepInHour}, {1,24} ) = 0.0;
			return;
		}

		// Determine which Week Schedule is used
		if ( ! present( JDay ) ) {
			WeekSchedulePointer = Schedule( ScheduleIndex ).WeekSchedulePointer( DayOfYear_Schedule );
		} else {
			WeekSchedulePointer = Schedule( ScheduleIndex ).WeekSchedulePointer( JDay );
		}

		// Now, which day?
		if ( ! present( CurDayofWeek ) ) {
			if ( DayOfWeek <= 7 && HolidayIndex > 0 ) {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( 7 + HolidayIndex );
			} else {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( DayOfWeek );
			}
		} else if ( CurDayofWeek <= 7 && HolidayIndex > 0 ) {
			DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( 7 + HolidayIndex );
		} else {
			DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( CurDayofWeek );
		}

		// Return Values
		DayValues( {1,NumOfTimeStepInHour}, {1,24} ) = DaySchedule( DaySchedulePointer ).TSValue;

	}

	void
	GetSingleDayScheduleValues(
		int const DayScheduleIndex, // Index of the DaySchedule for values
		Array2S< Real64 > DayValues // Returned set of values
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns an entire day's worth of schedule values for a specified Day Schedule Index item.

		// METHODOLOGY EMPLOYED:
		// Use internal data to fill DayValues array.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		// Return Values
		DayValues( {1,NumOfTimeStepInHour}, {1,24} ) = DaySchedule( DayScheduleIndex ).TSValue;

	}

	void
	ExternalInterfaceSetSchedule(
		int & ScheduleIndex,
		Real64 & Value // The new value for the schedule
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   February 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets all values of the schedule referenced by 'ScheduleIndex'
		// to the value specified by 'Value'. The subroutine is used by the ExternalInterface to
		// write real-time data into a schedule so that EnergyPlus modules can use
		// real-time data by referencing a schedule. This allows overwriting setpoint
		// for supervisory controls or internal gains obtained from real-time occupancy
		// measurements.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::NumOfTimeStepInHour;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int TS; // Counter for Num Of Time Steps in Hour
		int Hr; // Hour Counter

		// Assign the value of the variable
		for ( Hr = 1; Hr <= 24; ++Hr ) {
			for ( TS = 1; TS <= NumOfTimeStepInHour; ++TS ) {
				DaySchedule( ScheduleIndex ).TSValue( TS, Hr ) = Value;
			}
		}
	}

	void
	ProcessIntervalFields(
		Array1S_string const Untils,
		Array1S< Real64 > const Numbers,
		int const NumUntils,
		int const NumNumbers,
		Array2A< Real64 > MinuteValue,
		Array2A_bool SetMinuteValue,
		bool & ErrorsFound,
		std::string const & DayScheduleName, // Name (used for errors)
		std::string const & ErrContext, // Context (used for errors)
		bool useInterpolation  // flag if interpolation is allowed and if warning is issued then if timesteps do not match up
		)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes the "interval" fields with/without optional "until" in front of
		// time (hh:mm).

		// METHODOLOGY EMPLOYED:
		// na.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		MinuteValue.dim( 60, 24 );
		SetMinuteValue.dim( 60, 24 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count;
		std::string::size_type Pos;
		int HHField;
		int MMField;
		int Hr;
		int Min;
		int SHr;
		int SMin;
		int EHr;
		int EMin;
		std::string::size_type sFld;

		MinuteValue = 0.0;
		SetMinuteValue = false;
		SHr = 1;
		SMin = 1;
		EHr = 0;
		EMin = 0;
		sFld = 0;

		if ( NumUntils != NumNumbers ) {
			ShowSevereError( "ProcessScheduleInput: ProcessIntervalFields, number of Time fields does not match number of value fields, " + ErrContext + '=' + DayScheduleName );
			ErrorsFound = true;
			return;
		}

		for ( Count = 1; Count <= NumUntils; ++Count ) {
			Pos = index( Untils( Count ), "UNTIL" );
			if ( Pos == 0 ) {
				if ( Untils( Count )[ 5 ] == ':' ) {
					sFld = 6;
				} else {
					sFld = 5;
				}
				DecodeHHMMField( Untils( Count ).substr( sFld ), HHField, MMField, ErrorsFound, DayScheduleName, Untils( Count ), useInterpolation );
			} else if ( Pos == std::string::npos ) {
				DecodeHHMMField( Untils( Count ), HHField, MMField, ErrorsFound, DayScheduleName, Untils( Count ), useInterpolation );
			} else { // Until found but wasn't first field
				ShowSevereError( "ProcessScheduleInput: ProcessIntervalFields, Invalid \"Until\" field encountered=" + Untils( Count ) );
				ShowContinueError( "Occurred in Day Schedule=" + DayScheduleName );
				ErrorsFound = true;
				continue;
			}
			// Field decoded
			if ( HHField < 0 || HHField > 24 || MMField < 0 || MMField > 60 ) {
				ShowSevereError( "ProcessScheduleInput: ProcessIntervalFields, Invalid \"Until\" field encountered=" + Untils( Count ) );
				ShowContinueError( "Occurred in Day Schedule=" + DayScheduleName );
				ErrorsFound = true;
				continue;
			}
			if ( HHField == 24 && MMField > 0 && MMField < 60 ) {
				ShowWarningError( "ProcessScheduleInput: ProcessIntervalFields, Invalid \"Until\" field encountered=" + Untils( Count ) );
				ShowContinueError( "Occurred in Day Schedule=" + DayScheduleName );
				ShowContinueError( "Terminating the field at 24:00" );
				MMField = 0;
			}

			// Fill in values
			if ( MMField == 0 ) {
				EHr = HHField + 1;
				EMin = 60;
			}
			if ( MMField < 60 ) {
				EHr = HHField + 1;
				EMin = MMField;
			}

			if ( SHr == EHr ) {
				for ( Min = SMin; Min <= EMin; ++Min ) {
					if ( SetMinuteValue( Min, SHr ) ) {
						ShowSevereError( "ProcessScheduleInput: ProcessIntervalFields, Processing time fields, overlapping times detected, " + ErrContext + '=' + DayScheduleName );
						ErrorsFound = true;
						goto UntilLoop_exit;
					}
					MinuteValue( Min, SHr ) = Numbers( Count );
					SetMinuteValue( Min, SHr ) = true;
				}
				SMin = EMin + 1;
				if ( SMin > 60 ) {
					++SHr;
					SMin = 1;
				}
			} else if ( EHr < SHr ) {
				ShowSevereError( "ProcessScheduleInput: ProcessIntervalFields, Processing time fields, overlapping times detected, " + ErrContext + '=' + DayScheduleName );
				ErrorsFound = true;
			} else {
				for ( Min = SMin; Min <= 60; ++Min ) {
					MinuteValue( Min, SHr ) = Numbers( Count );
					SetMinuteValue( Min, SHr ) = true;
				}
				for ( Hr = SHr + 1; Hr <= EHr - 1; ++Hr ) {
					MinuteValue( _, Hr ) = Numbers( Count );
					SetMinuteValue( _, Hr ) = true;
				}
				for ( Min = 1; Min <= EMin; ++Min ) {
					MinuteValue( Min, EHr ) = Numbers( Count );
					SetMinuteValue( Min, EHr ) = true;
				}
				SHr = EHr;
				SMin = EMin + 1;
				if ( SMin > 60 ) {
					++SHr;
					SMin = 1;
				}
			}

		}
		UntilLoop_exit: ;

		if ( ! all( SetMinuteValue ) ) {
			ShowSevereError( "ProcessScheduleInput: ProcessIntervalFields, Processing time fields, incomplete day detected, " + ErrContext + '=' + DayScheduleName );
			ErrorsFound = true;
		}

	}

	void
	DecodeHHMMField(
		std::string const & FieldValue, // Input field value
		int & RetHH, // Returned "hour"
		int & RetMM, // Returned "minute"
		bool & ErrorsFound, // True if errors found in this field
		std::string const & DayScheduleName, // originating day schedule name
		std::string const & FullFieldValue, // Full Input field value
		bool useInterpolation  // flag if interpolation is allowed and if warning is issued then if timesteps do not match up
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K Lawrie
		//       DATE WRITTEN   January 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine decodes a hhmm date field input as part of the "until" time in a schedule
		// representation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt hhmmFormat( "(I2.2)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOS;
		Real64 rRetHH; // real Returned "hour"
		Real64 rRetMM; // real Returned "minute"
		bool nonIntegral;
		std::string hHour;
		std::string mMinute;

		std::string String = stripped( FieldValue );
		std::string::size_type const Pos = index( String, ':' );
		nonIntegral = false;
		if ( Pos == std::string::npos ) {
			ShowSevereError( "ProcessScheduleInput: DecodeHHMMField, Invalid \"until\" field submitted (no : separator in hh:mm)=" + stripped( FullFieldValue ) );
			ShowContinueError( "Occurred in Day Schedule=" + DayScheduleName );
			ErrorsFound = true;
			return;
		} else if ( Pos == 0 ) {
			RetHH = 0;
		} else {
			{ IOFlags flags; gio::read( String.substr( 0, Pos ), fmtLD, flags ) >> rRetHH; IOS = flags.ios(); }
			RetHH = int( rRetHH );
			if ( double( RetHH ) != rRetHH || IOS != 0 || rRetHH < 0.0 ) {
				if ( double( RetHH ) != rRetHH && rRetHH >= 0.0 ) {
					ShowWarningError( "ProcessScheduleInput: DecodeHHMMField, Invalid \"until\" field submitted (non-integer numeric in HH)=" + stripped( FullFieldValue ) );
					ShowContinueError( "Other errors may result. Occurred in Day Schedule=" + DayScheduleName );
					nonIntegral = true;
				} else {
					ShowSevereError( "ProcessScheduleInput: DecodeHHMMField, Invalid \"until\" field submitted (invalid numeric in HH)=" + stripped( FullFieldValue ) );
					ShowContinueError( "Field values must be integer and represent hours:minutes. Occurred in Day Schedule=" + DayScheduleName );
					ErrorsFound = true;
					return;
				}
			}
		}

		String.erase( 0, Pos + 1 );
		{ IOFlags flags; gio::read( String, fmtLD, flags ) >> rRetMM; IOS = flags.ios(); }
		RetMM = int( rRetMM );
		if ( double( RetMM ) != rRetMM || IOS != 0 || rRetMM < 0.0 ) {
			if ( double( RetMM ) != rRetMM && rRetMM >= 0.0 ) {
				ShowWarningError( "ProcessScheduleInput: DecodeHHMMField, Invalid \"until\" field submitted (non-integer numeric in MM)=" + stripped( FullFieldValue ) );
				ShowContinueError( "Other errors may result. Occurred in Day Schedule=" + DayScheduleName );
				nonIntegral = true;
			} else {
				ShowSevereError( "ProcessScheduleInput: DecodeHHMMField, Invalid \"until\" field submitted (invalid numeric in MM)=" + stripped( FullFieldValue ) );
				ShowContinueError( "Field values must be integer and represent hours:minutes. Occurred in Day Schedule=" + DayScheduleName );
				ErrorsFound = true;
				return;
			}
		}

		if ( nonIntegral ) {
			gio::write( hHour, hhmmFormat ) << RetHH;
			gio::write( mMinute, hhmmFormat ) << RetMM;
			ShowContinueError( "Until value to be used will be: " + hHour + ':' + mMinute );
		}
		if ( !useInterpolation ){
				if ( !isMinuteMultipleOfTimestep( RetMM, MinutesPerTimeStep ) ){
				ShowWarningError( "ProcessScheduleInput: DecodeHHMMField, Invalid \"until\" field value is not a multiple of the minutes for each timestep: " + stripped( FullFieldValue ) );
				ShowContinueError( "Other errors may result. Occurred in Day Schedule=" + DayScheduleName );
			}
		}
	}

	bool
	isMinuteMultipleOfTimestep( int minute, int numMinutesPerTimestep ){
			if ( minute != 0 ){
				return ( minute % numMinutesPerTimestep == 0 );
			} else {
				return true;
			}
		}

	void
	ProcessForDayTypes(
		std::string const & ForDayField, // Field containing the "FOR:..."
		Array1A_bool TheseDays, // Array to contain returned "true" days
		Array1A_bool AlReady, // Array of days already done
		bool & ErrorsFound // Will be true if error found.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes a field "For: day types" and returns
		// those day types (can be multiple) from field.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		TheseDays.dim( MaxDayTypes );
		AlReady.dim( MaxDayTypes );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DayT;
		bool OneValid;
		bool DupAssignment;

		OneValid = false;
		DupAssignment = false;
		// Just test for specific days
		if ( has( ForDayField, "WEEKDAY" ) ) {
			TheseDays( {2,6} ) = true;
			if ( any( AlReady( {2,6} ) ) ) {
				DupAssignment = true;
			} else {
				AlReady( {2,6} ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "MONDAY" ) ) {
			TheseDays( 2 ) = true;
			if ( AlReady( 2 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 2 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "TUESDAY" ) ) {
			TheseDays( 3 ) = true;
			if ( AlReady( 3 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 3 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "WEDNESDAY" ) ) {
			TheseDays( 4 ) = true;
			if ( AlReady( 4 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 4 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "THURSDAY" ) ) {
			TheseDays( 5 ) = true;
			if ( AlReady( 5 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 5 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "FRIDAY" ) ) {
			TheseDays( 6 ) = true;
			if ( AlReady( 6 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 6 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "WEEKEND" ) ) {
			TheseDays( 1 ) = true;
			TheseDays( 7 ) = true;
			if ( AlReady( 1 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 1 ) = true;
			}
			if ( AlReady( 7 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 7 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "SATURDAY" ) ) {
			TheseDays( 7 ) = true;
			if ( AlReady( 7 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 7 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "SUNDAY" ) ) {
			TheseDays( 1 ) = true;
			if ( AlReady( 1 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 1 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "CUSTOMDAY1" ) ) {
			TheseDays( 11 ) = true;
			if ( AlReady( 11 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 11 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "CUSTOMDAY2" ) ) {
			TheseDays( 12 ) = true;
			if ( AlReady( 12 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 12 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "ALLDAY" ) ) {
			TheseDays( {1,MaxDayTypes} ) = true;
			if ( any( AlReady ) ) {
				DupAssignment = true;
			} else {
				AlReady = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "HOLIDAY" ) ) {
			TheseDays( 8 ) = true;
			if ( AlReady( 8 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 8 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "SUMMER" ) ) {
			TheseDays( 9 ) = true;
			if ( AlReady( 9 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 9 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "WINTER" ) ) {
			TheseDays( 10 ) = true;
			if ( AlReady( 10 ) ) {
				DupAssignment = true;
			} else {
				AlReady( 10 ) = true;
			}
			OneValid = true;
		}
		if ( has( ForDayField, "ALLOTHERDAY" ) ) {
			for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
				if ( AlReady( DayT ) ) continue;
				TheseDays( DayT ) = true;
				AlReady( DayT ) = true;
			}
			OneValid = true;
		}

		if ( DupAssignment ) {
			ShowSevereError( "ProcessScheduleInput: ProcessForDayTypes, Duplicate assignment attempted in \"for\" days field=" + ForDayField );
			ErrorsFound = true;
		}
		if ( ! OneValid ) {
			ShowSevereError( "ProcessScheduleInput: ProcessForDayTypes, No valid day assignments found in \"for\" days field=" + ForDayField );
			ErrorsFound = true;
		}

	}

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real64 const Minimum // Minimum desired value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

		// METHODOLOGY EMPLOYED:
		// Schedule data structure stores this on first validity check.  If there, then is returned else
		// looks up minimum and maximum values for the schedule and then sets result of function based on
		// requested minimum/maximum checks.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckScheduleValueMinMax;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop; // Loop Control variable
		int DayT; // Day Type Loop control
		int WkSch; // Pointer for WeekSchedule value
		Real64 MinValue( 0.0 ); // For total minimum
		Real64 MaxValue( 0.0 ); // For total maximum
		bool MinValueOk( true );
		bool MaxValueOk( true );

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "CheckScheduleValueMinMax called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			if ( ! Schedule( ScheduleIndex ).MaxMinSet ) { // Set Minimum/Maximums for this schedule
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( 1 );
				MinValue = minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				MaxValue = maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				for ( DayT = 2; DayT <= MaxDayTypes; ++DayT ) {
					MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
				}
				for ( Loop = 2; Loop <= 366; ++Loop ) {
					WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
					for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
						MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
						MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					}
				}
				Schedule( ScheduleIndex ).MaxMinSet = true;
				Schedule( ScheduleIndex ).MinValue = MinValue;
				Schedule( ScheduleIndex ).MaxValue = MaxValue;
			}
		}

		//  Min/max for schedule has been set.  Test.
		MinValueOk = ( Schedule( ScheduleIndex ).MinValue >= Minimum );
		if ( MinString == ">" ) {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue > Minimum );
		} else {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue >= Minimum );
		}

		CheckScheduleValueMinMax = ( MinValueOk && MaxValueOk );

		return CheckScheduleValueMinMax;

	}

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real64 const Minimum, // Minimum desired value
		std::string const & MaxString, // Maximum indicator ('<', ',=')
		Real64 const Maximum // Maximum desired value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

		// METHODOLOGY EMPLOYED:
		// Schedule data structure stores this on first validity check.  If there, then is returned else
		// looks up minimum and maximum values for the schedule and then sets result of function based on
		// requested minimum/maximum checks.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckScheduleValueMinMax;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop; // Loop Control variable
		int DayT; // Day Type Loop control
		int WkSch; // Pointer for WeekSchedule value
		Real64 MinValue( 0.0 ); // For total minimum
		Real64 MaxValue( 0.0 ); // For total maximum
		bool MinValueOk( true );
		bool MaxValueOk( true );
		/////////// hoisted into namespace CheckScheduleValueMinMaxRunOnceOnly////////////
		//static bool RunOnceOnly( true );
		/////////////////////////////////////////////////
		//precompute the dayschedule max and min so that it is not in nested loop
		if ( CheckScheduleValueMinMaxRunOnceOnly ) {
			for ( Loop = 0; Loop <= NumDaySchedules; ++Loop ) {
				DaySchedule( Loop ).TSValMin = minval( DaySchedule( Loop ).TSValue );
				DaySchedule( Loop ).TSValMax = maxval( DaySchedule( Loop ).TSValue );
			}
			CheckScheduleValueMinMaxRunOnceOnly = false;
		}

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "CheckScheduleValueMinMax called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			if ( ! Schedule( ScheduleIndex ).MaxMinSet ) { // Set Minimum/Maximums for this schedule
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( 1 );
				MinValue = DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValMin;
				MaxValue = DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValMax;
				for ( DayT = 2; DayT <= MaxDayTypes; ++DayT ) {
					MinValue = min( MinValue, DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValMin );
					MaxValue = max( MaxValue, DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValMax );
				}
				for ( Loop = 2; Loop <= 366; ++Loop ) {
					WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
					for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
						MinValue = min( MinValue, DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValMin );
						MaxValue = max( MaxValue, DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValMax );
					}
				}
				Schedule( ScheduleIndex ).MaxMinSet = true;
				Schedule( ScheduleIndex ).MinValue = MinValue;
				Schedule( ScheduleIndex ).MaxValue = MaxValue;
			}
		}

		//  Min/max for schedule has been set.  Test.
		if ( MinString == ">" ) {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue > Minimum );
		} else {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue >= Minimum );
		}

		MaxValueOk = ( Schedule( ScheduleIndex ).MaxValue <= Maximum );
		if ( MaxString == "<" ) {
			MaxValueOk = ( Schedule( ScheduleIndex ).MaxValue < Maximum );
		} else {
			MaxValueOk = ( Schedule( ScheduleIndex ).MaxValue <= Maximum );
		}

		CheckScheduleValueMinMax = ( MinValueOk && MaxValueOk );

		return CheckScheduleValueMinMax;

	}

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real32 const Minimum // Minimum desired value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

		// METHODOLOGY EMPLOYED:
		// Schedule data structure stores this on first validity check.  If there, then is returned else
		// looks up minimum and maximum values for the schedule and then sets result of function based on
		// requested minimum/maximum checks.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckScheduleValueMinMax;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop; // Loop Control variable
		int DayT; // Day Type Loop control
		int WkSch; // Pointer for WeekSchedule value
		Real64 MinValue( 0.0 ); // For total minimum
		Real64 MaxValue( 0.0 ); // For total maximum
		bool MinValueOk( true );
		bool MaxValueOk( true );

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "CheckScheduleValueMinMax called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			if ( ! Schedule( ScheduleIndex ).MaxMinSet ) { // Set Minimum/Maximums for this schedule
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( 1 );
				MinValue = minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				MaxValue = maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				for ( DayT = 2; DayT <= MaxDayTypes; ++DayT ) {
					MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
				}
				for ( Loop = 2; Loop <= 366; ++Loop ) {
					WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
					for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
						MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
						MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					}
				}
				Schedule( ScheduleIndex ).MaxMinSet = true;
				Schedule( ScheduleIndex ).MinValue = MinValue;
				Schedule( ScheduleIndex ).MaxValue = MaxValue;
			}
		}

		//  Min/max for schedule has been set.  Test.
		MinValueOk = ( Schedule( ScheduleIndex ).MinValue >= Minimum );
		if ( MinString == ">" ) {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue > Minimum );
		} else {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue >= Minimum );
		}

		CheckScheduleValueMinMax = ( MinValueOk && MaxValueOk );

		return CheckScheduleValueMinMax;

	}

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real32 const Minimum, // Minimum desired value
		std::string const & MaxString, // Maximum indicator ('<', ',=')
		Real32 const Maximum // Maximum desired value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

		// METHODOLOGY EMPLOYED:
		// Schedule data structure stores this on first validity check.  If there, then is returned else
		// looks up minimum and maximum values for the schedule and then sets result of function based on
		// requested minimum/maximum checks.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckScheduleValueMinMax;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop; // Loop Control variable
		int DayT; // Day Type Loop control
		int WkSch; // Pointer for WeekSchedule value
		Real64 MinValue( 0.0 ); // For total minimum
		Real64 MaxValue( 0.0 ); // For total maximum
		bool MinValueOk;
		bool MaxValueOk;

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "CheckScheduleValueMinMax called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			if ( ! Schedule( ScheduleIndex ).MaxMinSet ) { // Set Minimum/Maximums for this schedule
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( 1 );
				MinValue = minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				MaxValue = maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				for ( DayT = 2; DayT <= MaxDayTypes; ++DayT ) {
					MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
				}
				for ( Loop = 2; Loop <= 366; ++Loop ) {
					WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
					for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
						MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
						MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					}
				}
				Schedule( ScheduleIndex ).MaxMinSet = true;
				Schedule( ScheduleIndex ).MinValue = MinValue;
				Schedule( ScheduleIndex ).MaxValue = MaxValue;
			}
		}

		//  Min/max for schedule has been set.  Test.
		MinValueOk = true;
		MaxValueOk = true;
		if ( MinString == ">" ) {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue > Minimum );
		} else {
			MinValueOk = ( Schedule( ScheduleIndex ).MinValue >= Minimum );
		}

		MaxValueOk = ( Schedule( ScheduleIndex ).MaxValue <= Maximum );
		if ( MaxString == "<" ) {
			MaxValueOk = ( Schedule( ScheduleIndex ).MaxValue < Maximum );
		} else {
			MaxValueOk = ( Schedule( ScheduleIndex ).MaxValue <= Maximum );
		}

		CheckScheduleValueMinMax = ( MinValueOk && MaxValueOk );

		return CheckScheduleValueMinMax;

	}

	bool
	CheckScheduleValue(
		int const ScheduleIndex, // Which Schedule being tested
		Real64 const Value // Actual desired value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule value for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex).

		// METHODOLOGY EMPLOYED:
		// This routine is best used with "discrete" schedules.  The routine must traverse all values
		// in the schedule and compares by equality.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckScheduleValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop; // Loop Control variable
		int DayT; // Day Type Loop control
		int WkSch; // Pointer for WeekSchedule value

		CheckScheduleValue = false;

		if ( ScheduleIndex == -1 ) {
			CheckScheduleValue = ( Value == 1.0 );
		} else if ( ScheduleIndex == 0 ) {
			CheckScheduleValue = ( Value == 0.0 );
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "CheckScheduleValue called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			CheckScheduleValue = false;
			for ( Loop = 1; Loop <= 366; ++Loop ) {
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
				for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
					if ( any_eq( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue, Value ) ) {
						CheckScheduleValue = true;
						goto DayLoop_exit;
					}
				}
			}
			DayLoop_exit: ;
		}

		return CheckScheduleValue;

	}

	bool
	CheckScheduleValue(
		int const ScheduleIndex, // Which Schedule being tested
		int const Value // Actual desired value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule value for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex).

		// METHODOLOGY EMPLOYED:
		// This routine is best used with "discrete" schedules.  The routine must traverse all values
		// in the schedule and compares by equality.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckScheduleValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop; // Loop Control variable
		int DayT; // Day Type Loop control
		int WkSch; // Pointer for WeekSchedule value

		CheckScheduleValue = false;
		if ( ScheduleIndex == -1 ) {
			CheckScheduleValue = ( Value == 1 );
		} else if ( ScheduleIndex == 0 ) {
			CheckScheduleValue = ( Value == 0 );
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "CheckScheduleValue called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			for ( Loop = 1; Loop <= 366; ++Loop ) {
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
				for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
					if ( any_eq( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue, double( Value ) ) ) {
						CheckScheduleValue = true;
						goto DayLoop_exit;
					}
				}
			}
			DayLoop_exit: ;
		}

		return CheckScheduleValue;

	}

	bool
	CheckDayScheduleValueMinMax(
		int const ScheduleIndex, // Which Day Schedule being tested
		Real64 const Minimum, // Minimum desired value
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Optional< Real64 const > Maximum, // Maximum desired value
		Optional_string_const MaxString // Maximum indicator ('<', ',=')
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

		// METHODOLOGY EMPLOYED:
		// Schedule data structure stores this on first validity check.  If there, then is returned else
		// looks up minimum and maximum values for the schedule and then sets result of function based on
		// requested minimum/maximum checks.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckDayScheduleValueMinMax;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 MinValue( 0.0 ); // For total minimum
		Real64 MaxValue( 0.0 ); // For total maximum
		bool MinValueOk;
		bool MaxValueOk;

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumDaySchedules ) {
			ShowFatalError( "CheckDayScheduleValueMinMax called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			MinValue = minval( DaySchedule( ScheduleIndex ).TSValue );
			MaxValue = maxval( DaySchedule( ScheduleIndex ).TSValue );
		}

		//  Min/max for schedule has been set.  Test.
		MinValueOk = true;
		MaxValueOk = true;

		if ( MinString == ">" ) {
			MinValueOk = ( MinValue > Minimum );
		} else {
			MinValueOk = ( MinValue >= Minimum );
		}

		if ( present( Maximum ) ) {
			if ( present( MaxString ) ) {
				if ( MaxString() == "<" ) {
					MaxValueOk = ( MaxValue < Maximum );
				} else {
					MaxValueOk = ( MaxValue <= Maximum );
				}
			} else {
				MaxValueOk = ( MaxValue <= Maximum );
			}
		}

		CheckDayScheduleValueMinMax = ( MinValueOk && MaxValueOk );

		return CheckDayScheduleValueMinMax;

	}

	bool
	CheckDayScheduleValueMinMax(
		int const ScheduleIndex, // Which Day Schedule being tested
		Real32 const Minimum, // Minimum desired value
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Optional< Real32 const > Maximum, // Maximum desired value
		Optional_string_const MaxString // Maximum indicator ('<', ',=')
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
		// from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

		// METHODOLOGY EMPLOYED:
		// Schedule data structure stores this on first validity check.  If there, then is returned else
		// looks up minimum and maximum values for the schedule and then sets result of function based on
		// requested minimum/maximum checks.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckDayScheduleValueMinMax;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 MinValue( 0.0 ); // For total minimum
		Real64 MaxValue( 0.0 ); // For total maximum
		bool MinValueOk;
		bool MaxValueOk;

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumDaySchedules ) {
			ShowFatalError( "CheckDayScheduleValueMinMax called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			MinValue = minval( DaySchedule( ScheduleIndex ).TSValue );
			MaxValue = maxval( DaySchedule( ScheduleIndex ).TSValue );
		}

		//  Min/max for schedule has been set.  Test.
		MinValueOk = true;
		MaxValueOk = true;
		if ( MinString == ">" ) {
			MinValueOk = ( MinValue > Minimum );
		} else {
			MinValueOk = ( MinValue >= Minimum );
		}

		if ( present( Maximum ) ) {
			if ( present( MaxString ) ) {
				if ( MaxString() == "<" ) {
					MaxValueOk = ( MaxValue < Maximum );
				} else {
					MaxValueOk = ( MaxValue <= Maximum );
				}
			} else {
				MaxValueOk = ( MaxValue <= Maximum );
			}
		}

		CheckDayScheduleValueMinMax = ( MinValueOk && MaxValueOk );

		return CheckDayScheduleValueMinMax;

	}

	bool
	HasFractionalScheduleValue( int const ScheduleIndex ) // Which Schedule being tested
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns true if the schedule contains fractional
		// values [>0, <1].

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool HasFractions; // True if the schedule has fractional values

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WkSch;
		int DayT;
		int Loop;
		int Hour;
		int TStep;

		if ( ScheduleIndex == -1 || ScheduleIndex == 0 ) {

		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "HasFractionalScheduleValue called with ScheduleIndex out of range" );
		}

		HasFractions = false;

		if ( ScheduleIndex > 0 ) {
			WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( 1 );
			for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
				for ( Hour = 1; Hour <= 24; ++Hour ) {
					for ( TStep = 1; TStep <= NumOfTimeStepInHour; ++TStep ) {
						if ( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue( TStep, Hour ) > 0.0 && DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue( TStep, Hour ) < 1.0 ) {
							HasFractions = true;
							goto DayTLoop_exit;
						}
					}
				}
			}
			DayTLoop_exit: ;
			if ( ! HasFractions ) {
				for ( Loop = 2; Loop <= 366; ++Loop ) {
					WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
					for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
						for ( Hour = 1; Hour <= 24; ++Hour ) {
							for ( TStep = 1; TStep <= NumOfTimeStepInHour; ++TStep ) {
								if ( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue( TStep, Hour ) > 0.0 && DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue( TStep, Hour ) < 1.0 ) {
									HasFractions = true;
									goto DayTLoop2_exit;
								}
							}
						}
					}
					DayTLoop2_exit: ;
				}
			}
		}

		return HasFractions;

	}

	Real64
	GetScheduleMinValue( int const ScheduleIndex ) // Which Schedule being tested
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the minimum value used by a schedule over
		// the entire year.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 MinimumValue; // Minimum value for schedule

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 MinValue( 0.0 );
		Real64 MaxValue( 0.0 );
		int WkSch;
		int DayT;
		int Loop;

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "GetScheduleMinValue called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			if ( ! Schedule( ScheduleIndex ).MaxMinSet ) { // Set Minimum/Maximums for this schedule
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( 1 );
				MinValue = minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				MaxValue = maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				for ( DayT = 2; DayT <= MaxDayTypes; ++DayT ) {
					MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
				}
				for ( Loop = 2; Loop <= 366; ++Loop ) {
					WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
					for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
						MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
						MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					}
				}
				Schedule( ScheduleIndex ).MaxMinSet = true;
				Schedule( ScheduleIndex ).MinValue = MinValue;
				Schedule( ScheduleIndex ).MaxValue = MaxValue;
			}

			//  Min/max for schedule has been set.
			MinimumValue = Schedule( ScheduleIndex ).MinValue;
		} else {
			MinimumValue = MinValue;
		}

		return MinimumValue;

	}

	Real64
	GetScheduleMaxValue( int const ScheduleIndex ) // Which Schedule being tested
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the maximum value used by a schedule over
		// the entire year.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 MaximumValue; // Maximum value for schedule

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 MinValue( 0.0 );
		Real64 MaxValue( 0.0 );
		int WkSch;
		int DayT;
		int Loop;

		if ( ScheduleIndex == -1 ) {
			MinValue = 1.0;
			MaxValue = 1.0;
		} else if ( ScheduleIndex == 0 ) {
			MinValue = 0.0;
			MaxValue = 0.0;
		} else if ( ScheduleIndex < 1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "CheckScheduleMaxValue called with ScheduleIndex out of range" );
		}

		if ( ScheduleIndex > 0 ) {
			if ( ! Schedule( ScheduleIndex ).MaxMinSet ) { // Set Minimum/Maximums for this schedule
				WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( 1 );
				MinValue = minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				MaxValue = maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( 1 ) ).TSValue );
				for ( DayT = 2; DayT <= MaxDayTypes; ++DayT ) {
					MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
				}
				for ( Loop = 2; Loop <= 366; ++Loop ) {
					WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
					for ( DayT = 1; DayT <= MaxDayTypes; ++DayT ) {
						MinValue = min( MinValue, minval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
						MaxValue = max( MaxValue, maxval( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) );
					}
				}
				Schedule( ScheduleIndex ).MaxMinSet = true;
				Schedule( ScheduleIndex ).MinValue = MinValue;
				Schedule( ScheduleIndex ).MaxValue = MaxValue;
			}

			//  Min/max for schedule has been set.

			MaximumValue = Schedule( ScheduleIndex ).MaxValue;
		} else {
			MaximumValue = MaxValue;
		}

		return MaximumValue;

	}

	std::string
	GetScheduleName( int const ScheduleIndex )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the schedule name from the Schedule Index.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string ScheduleName;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		if ( ScheduleIndex > 0 ) {
			ScheduleName = Schedule( ScheduleIndex ).Name;
		} else if ( ScheduleIndex == -1 ) {
			ScheduleName = "Constant-1.0";
		} else if ( ScheduleIndex == 0 ) {
			ScheduleName = "Constant-0.0";
		} else {
			ScheduleName = "N/A-Invalid";
		}

		return ScheduleName;

	}

	void
	ReportScheduleValues()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine puts the proper current schedule values into the "reporting"
		// slot for later reporting.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::DayOfYear_Schedule;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ScheduleIndex;
		int WhichHour;
		static bool DoScheduleReportingSetup( true );
		int WeekSchedulePointer;
		int DaySchedulePointer;

		if ( ! ScheduleInputProcessed ) {
			ProcessScheduleInput();
			ScheduleInputProcessed = true;
		}

		if ( DoScheduleReportingSetup ) { // CurrentModuleObject='Any Schedule'
			for ( ScheduleIndex = 1; ScheduleIndex <= NumSchedules; ++ScheduleIndex ) {
				// Set Up Reporting
				SetupOutputVariable( "Schedule Value []", Schedule( ScheduleIndex ).CurrentValue, "Zone", "Average", Schedule( ScheduleIndex ).Name );
			}
			DoScheduleReportingSetup = false;
		}

		WhichHour = HourOfDay + DSTIndicator;
		for ( ScheduleIndex = 1; ScheduleIndex <= NumSchedules; ++ScheduleIndex ) {
			// Determine which Week Schedule is used
			//  Cant use stored day of year because of leap year inconsistency
			WeekSchedulePointer = Schedule( ScheduleIndex ).WeekSchedulePointer( DayOfYear_Schedule );

			// Now, which day?
			if ( DayOfWeek <= 7 && HolidayIndex > 0 ) {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( 7 + HolidayIndex );
			} else {
				DaySchedulePointer = WeekSchedule( WeekSchedulePointer ).DaySchedulePointer( DayOfWeek );
			}

			// Hourly Value
			if ( WhichHour <= 24 ) {
				Schedule( ScheduleIndex ).CurrentValue = DaySchedule( DaySchedulePointer ).TSValue( TimeStep, WhichHour );
			} else if ( TimeStep <= NumOfTimeStepInHour ) {
				Schedule( ScheduleIndex ).CurrentValue = DaySchedule( DaySchedulePointer ).TSValue( TimeStep, WhichHour - 24 );
			} else {
				Schedule( ScheduleIndex ).CurrentValue = DaySchedule( DaySchedulePointer ).TSValue( NumOfTimeStepInHour, WhichHour - 24 );
			}

			if ( Schedule( ScheduleIndex ).EMSActuatedOn ) {
				Schedule( ScheduleIndex ).CurrentValue = Schedule( ScheduleIndex ).EMSValue;
			}

		}

	}

	void
	ReportOrphanSchedules()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// In response to CR7498, report orphan (unused) schedule items.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::DisplayUnusedSchedules;
		using General::RoundSigDigits;

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
		bool NeedOrphanMessage;
		bool NeedUseMessage;
		int Item;
		int NumCount;

		NeedOrphanMessage = true;
		NeedUseMessage = false;
		NumCount = 0;

		for ( Item = 1; Item <= NumSchedules; ++Item ) {
			if ( Schedule( Item ).Used ) continue;
			if ( NeedOrphanMessage && DisplayUnusedSchedules ) {
				ShowWarningError( "The following schedule names are \"Unused Schedules\".  These schedules are in the idf" );
				ShowContinueError( " file but are never obtained by the simulation and therefore are NOT used." );
				NeedOrphanMessage = false;
			}
			if ( DisplayUnusedSchedules ) {
				ShowMessage( "Schedule:Year or Schedule:Compact or Schedule:File or Schedule:Constant=" + Schedule( Item ).Name );
			} else {
				++NumCount;
			}
		}

		if ( NumCount > 0 ) {
			ShowMessage( "There are " + RoundSigDigits( NumCount ) + " unused schedules in input." );
			NeedUseMessage = true;
		}

		NeedOrphanMessage = true;
		NumCount = 0;

		for ( Item = 1; Item <= NumWeekSchedules; ++Item ) {
			if ( WeekSchedule( Item ).Used ) continue;
			if ( WeekSchedule( Item ).Name == BlankString ) continue;
			if ( NeedOrphanMessage && DisplayUnusedSchedules ) {
				ShowWarningError( "The following week schedule names are \"Unused Schedules\".  These schedules are in the idf" );
				ShowContinueError( " file but are never obtained by the simulation and therefore are NOT used." );
				NeedOrphanMessage = false;
			}
			if ( DisplayUnusedSchedules ) {
				ShowMessage( "Schedule:Week:Daily or Schedule:Week:Compact=" + WeekSchedule( Item ).Name );
			} else {
				++NumCount;
			}
		}

		if ( NumCount > 0 ) {
			ShowMessage( "There are " + RoundSigDigits( NumCount ) + " unused week schedules in input." );
			NeedUseMessage = true;
		}

		NeedOrphanMessage = true;
		NumCount = 0;

		for ( Item = 1; Item <= NumDaySchedules; ++Item ) {
			if ( DaySchedule( Item ).Used ) continue;
			if ( DaySchedule( Item ).Name == BlankString ) continue;
			if ( NeedOrphanMessage && DisplayUnusedSchedules ) {
				ShowWarningError( "The following day schedule names are \"Unused Schedules\".  These schedules are in the idf" );
				ShowContinueError( " file but are never obtained by the simulation and therefore are NOT used." );
				NeedOrphanMessage = false;
			}
			if ( DisplayUnusedSchedules ) {
				ShowMessage( "Schedule:Day:Hourly or Schedule:Day:Interval or Schedule:Day:List=" + DaySchedule( Item ).Name );
			} else {
				++NumCount;
			}
		}

		if ( NumCount > 0 ) {
			ShowMessage( "There are " + RoundSigDigits( NumCount ) + " unused day schedules in input." );
			NeedUseMessage = true;
		}

		if ( NeedUseMessage ) ShowMessage( "Use Output:Diagnostics,DisplayUnusedSchedules; to see them." );

	}

	Real64
	ScheduleAverageHoursPerWeek(
		int const ScheduleIndex, // Which Schedule being tested
		int const StartDayOfWeek, // Day of week for start of year
		bool const isItLeapYear // true if it is a leap year containing February 29
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 2006
		//       MODIFIED       September 2012; Glazer - CR8849
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the "average" hours per week for a schedule over
		// the entire year.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 AverageHoursPerWeek; // Average Hours Per Week

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WkSch;
		int DayT;
		int Loop;
		Real64 TotalHours;
		Real64 WeeksInYear;
		int DaysInYear;

		if ( isItLeapYear ) {
			DaysInYear = 366;
			WeeksInYear = 366.0 / 7.0;
		} else {
			DaysInYear = 365;
			WeeksInYear = 365.0 / 7.0;
		}

		if ( ScheduleIndex < -1 || ScheduleIndex > NumSchedules ) {
			ShowFatalError( "ScheduleAverageHoursPerWeek called with ScheduleIndex out of range" );
		}

		DayT = StartDayOfWeek;
		AverageHoursPerWeek = 0.0;
		TotalHours = 0.0;

		if ( DayT == 0 ) return AverageHoursPerWeek;

		for ( Loop = 1; Loop <= DaysInYear; ++Loop ) {
			WkSch = Schedule( ScheduleIndex ).WeekSchedulePointer( Loop );
			TotalHours += sum( DaySchedule( WeekSchedule( WkSch ).DaySchedulePointer( DayT ) ).TSValue ) / double( NumOfTimeStepInHour );
			++DayT;
			if ( DayT > 7 ) DayT = 1;
		}

		//  Total hours for year have been set.

		AverageHoursPerWeek = TotalHours / WeeksInYear;

		return AverageHoursPerWeek;

	}

	int
	GetNumberOfSchedules()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Greg Stark
		//       DATE WRITTEN   September 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the number of schedules.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NumberOfSchedules;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		NumberOfSchedules = NumSchedules;

		return NumberOfSchedules;

	}

} // ScheduleManager

} // EnergyPlus
