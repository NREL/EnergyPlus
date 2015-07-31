#ifndef ScheduleManager_hh_INCLUDED
#define ScheduleManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ScheduleManager {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const MaxDayTypes;
	extern Array1D_string const ValidDayTypes;

	extern int const NumScheduleTypeLimitUnitTypes;
	extern Array1D_string const ScheduleTypeLimitUnitTypes;

	extern int const ScheduleInput_year;
	extern int const ScheduleInput_compact;
	extern int const ScheduleInput_file;
	extern int const ScheduleInput_constant;
	extern int const ScheduleInput_external;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS

	// MODULE VARIABLE DECLARATIONS:

	//Integer Variables for the Module
	extern int NumScheduleTypes;
	extern int NumDaySchedules;
	extern int NumWeekSchedules;
	extern int NumSchedules;

	//Logical Variables for Module
	extern bool ScheduleInputProcessed; // This is false until the Schedule Input has been processed.
	extern bool ScheduleDSTSFileWarningIssued;

	//Derived Types Variables

	// Types

	struct ScheduleTypeData
	{
		// Members
		std::string Name; // Schedule Type Name
		bool Limited; // True if this Schedule Type has limits
		Real64 Minimum; // Minimum for limited schedule
		Real64 Maximum; // Maximum for limited schedule
		bool IsReal; // True if this is a "real" schedule, false if integer
		int UnitType; // reference ScheduleTypeLimit table

		// Default Constructor
		ScheduleTypeData() :
			Limited( false ),
			Minimum( 0.0 ),
			Maximum( 0.0 ),
			IsReal( true ),
			UnitType( 0 )
		{}

		// Member Constructor
		ScheduleTypeData(
			std::string const & Name, // Schedule Type Name
			bool const Limited, // True if this Schedule Type has limits
			Real64 const Minimum, // Minimum for limited schedule
			Real64 const Maximum, // Maximum for limited schedule
			bool const IsReal, // True if this is a "real" schedule, false if integer
			int const UnitType // reference ScheduleTypeLimit table
		) :
			Name( Name ),
			Limited( Limited ),
			Minimum( Minimum ),
			Maximum( Maximum ),
			IsReal( IsReal ),
			UnitType( UnitType )
		{}

	};

	struct DayScheduleData
	{
		// Members
		std::string Name; // Day Schedule Name
		int ScheduleTypePtr; // Index of Schedule Type
		bool IntervalInterpolated; // Indicator for interval interpolation. If not "interpolated", False.  Else True
		bool Used; // Indicator for this schedule being "used".
		Array2D< Real64 > TSValue; // Value array by simulation timestep
		Real64 TSValMax; // maximum of all TSValue's
		Real64 TSValMin; // minimum of all TSValue's

		// Default Constructor
		DayScheduleData() :
			ScheduleTypePtr( 0 ),
			IntervalInterpolated( false ),
			Used( false ),
			TSValMax( 0.0 ),
			TSValMin( 0.0 )
		{}

		// Member Constructor
		DayScheduleData(
			std::string const & Name, // Day Schedule Name
			int const ScheduleTypePtr, // Index of Schedule Type
			bool const IntervalInterpolated, // Indicator for interval interpolation. If not "interpolated", False.  Else True
			bool const Used, // Indicator for this schedule being "used".
			Array2< Real64 > const & TSValue, // Value array by simulation timestep
			Real64 const TSValMax, // maximum of all TSValue's
			Real64 const TSValMin // minimum of all TSValue's
		) :
			Name( Name ),
			ScheduleTypePtr( ScheduleTypePtr ),
			IntervalInterpolated( IntervalInterpolated ),
			Used( Used ),
			TSValue( TSValue ),
			TSValMax( TSValMax ),
			TSValMin( TSValMin )
		{}

	};

	struct WeekScheduleData
	{
		// Members
		std::string Name; // Week Schedule Name
		bool Used; // Indicator for this schedule being "used".
		Array1D_int DaySchedulePointer; // Index of Day Schedule

		// Default Constructor
		WeekScheduleData() :
			Used( false ),
			DaySchedulePointer( MaxDayTypes, 0 )
		{}

		// Member Constructor
		WeekScheduleData(
			std::string const & Name, // Week Schedule Name
			bool const Used, // Indicator for this schedule being "used".
			Array1_int const & DaySchedulePointer // Index of Day Schedule
		) :
			Name( Name ),
			Used( Used ),
			DaySchedulePointer( MaxDayTypes, DaySchedulePointer )
		{}

	};

	struct ScheduleData
	{
		// Members
		std::string Name; // Schedule Name
		int ScheduleTypePtr; // Index of Schedule Type
		Array1D_int WeekSchedulePointer; // one created for each day of possible simulation
		int SchType; // what kind of object has been input.
		bool Used; // Indicator for this schedule being "used".
		bool MaxMinSet; // Max/min values have been stored for this schedule
		Real64 MaxValue; // Maximum value for this schedule
		Real64 MinValue; // Minimum value for this schedule
		Real64 CurrentValue; // For Reporting
		bool EMSActuatedOn; // indicates if EMS computed
		Real64 EMSValue;

		// Default Constructor
		ScheduleData() :
			ScheduleTypePtr( 0 ),
			WeekSchedulePointer( 366, 0 ),
			SchType( 0 ),
			Used( false ),
			MaxMinSet( false ),
			MaxValue( 0.0 ),
			MinValue( 0.0 ),
			CurrentValue( 0.0 ),
			EMSActuatedOn( false ),
			EMSValue( 0.0 )
		{}

		// Member Constructor
		ScheduleData(
			std::string const & Name, // Schedule Name
			int const ScheduleTypePtr, // Index of Schedule Type
			Array1_int const & WeekSchedulePointer, // one created for each day of possible simulation
			int const SchType, // what kind of object has been input.
			bool const Used, // Indicator for this schedule being "used".
			bool const MaxMinSet, // Max/min values have been stored for this schedule
			Real64 const MaxValue, // Maximum value for this schedule
			Real64 const MinValue, // Minimum value for this schedule
			Real64 const CurrentValue, // For Reporting
			bool const EMSActuatedOn, // indicates if EMS computed
			Real64 const EMSValue
		) :
			Name( Name ),
			ScheduleTypePtr( ScheduleTypePtr ),
			WeekSchedulePointer( 366, WeekSchedulePointer ),
			SchType( SchType ),
			Used( Used ),
			MaxMinSet( MaxMinSet ),
			MaxValue( MaxValue ),
			MinValue( MinValue ),
			CurrentValue( CurrentValue ),
			EMSActuatedOn( EMSActuatedOn ),
			EMSValue( EMSValue )
		{}

	};

	// Object Data
	extern Array1D< ScheduleTypeData > ScheduleType; // Allowed Schedule Types
	extern Array1D< DayScheduleData > DaySchedule; // Day Schedule Storage
	extern Array1D< WeekScheduleData > WeekSchedule; // Week Schedule Storage
	extern Array1D< ScheduleData > Schedule; // Schedule Storage

	// Functions

	// Clears the global data in ScheduleManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	ProcessScheduleInput();

	void
	ReportScheduleDetails( int const LevelOfDetail ); // =1: hourly; =2: timestep; = 3: make IDF excerpt

	Real64
	GetCurrentScheduleValue( int const ScheduleIndex );

	void
	UpdateScheduleValues();

	Real64
	LookUpScheduleValue(
		int const ScheduleIndex,
		int const ThisHour = -1, // Negative => unspecified
		int const ThisTimeStep = -1 // Negative => unspecified
	);

	int
	GetScheduleIndex( std::string const & ScheduleName );

	std::string
	GetScheduleType( int const ScheduleIndex );

	int
	GetDayScheduleIndex( std::string & ScheduleName );

	void
	GetScheduleValuesForDay(
		int const ScheduleIndex,
		Array2S< Real64 > DayValues,
		Optional_int_const JDay = _,
		Optional_int_const CurDayofWeek = _
	);

	void
	GetSingleDayScheduleValues(
		int const DayScheduleIndex, // Index of the DaySchedule for values
		Array2S< Real64 > DayValues // Returned set of values
	);

	void
	ExternalInterfaceSetSchedule(
		int & ScheduleIndex,
		Real64 & Value // The new value for the schedule
	);

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
		std::string const & ErrContext // Context (used for errors)
	);

	void
	DecodeHHMMField(
		std::string const & FieldValue, // Input field value
		int & RetHH, // Returned "hour"
		int & RetMM, // Returned "minute"
		bool & ErrorsFound, // True if errors found in this field
		std::string const & DayScheduleName, // originating day schedule name
		std::string const & FullFieldValue // Full Input field value
	);

	void
	ProcessForDayTypes(
		std::string const & ForDayField, // Field containing the "FOR:..."
		Array1A_bool TheseDays, // Array to contain returned "true" days
		Array1A_bool AlReady, // Array of days already done
		bool & ErrorsFound // Will be true if error found.
	);

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real64 const Minimum // Minimum desired value
	);

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real64 const Minimum, // Minimum desired value
		std::string const & MaxString, // Maximum indicator ('<', ',=')
		Real64 const Maximum // Maximum desired value
	);

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real32 const Minimum // Minimum desired value
	);

	bool
	CheckScheduleValueMinMax(
		int const ScheduleIndex, // Which Schedule being tested
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Real32 const Minimum, // Minimum desired value
		std::string const & MaxString, // Maximum indicator ('<', ',=')
		Real32 const Maximum // Maximum desired value
	);

	bool
	CheckScheduleValue(
		int const ScheduleIndex, // Which Schedule being tested
		Real64 const Value // Actual desired value
	);

	bool
	CheckScheduleValue(
		int const ScheduleIndex, // Which Schedule being tested
		int const Value // Actual desired value
	);

	bool
	CheckDayScheduleValueMinMax(
		int const ScheduleIndex, // Which Day Schedule being tested
		Real64 const Minimum, // Minimum desired value
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Optional< Real64 const > Maximum = _, // Maximum desired value
		Optional_string_const MaxString = _ // Maximum indicator ('<', ',=')
	);

	bool
	CheckDayScheduleValueMinMax(
		int const ScheduleIndex, // Which Day Schedule being tested
		Real32 const Minimum, // Minimum desired value
		std::string const & MinString, // Minimum indicator ('>', '>=')
		Optional< Real32 const > Maximum = _, // Maximum desired value
		Optional_string_const MaxString = _ // Maximum indicator ('<', ',=')
	);

	bool
	HasFractionalScheduleValue( int const ScheduleIndex ); // Which Schedule being tested

	Real64
	GetScheduleMinValue( int const ScheduleIndex ); // Which Schedule being tested

	Real64
	GetScheduleMaxValue( int const ScheduleIndex ); // Which Schedule being tested

	std::string
	GetScheduleName( int const ScheduleIndex );

	void
	ReportScheduleValues();

	void
	ReportOrphanSchedules();

	Real64
	ScheduleAverageHoursPerWeek(
		int const ScheduleIndex, // Which Schedule being tested
		int const StartDayOfWeek, // Day of week for start of year
		bool const isItLeapYear // true if it is a leap year containing February 29
	);

	int
	GetNumberOfSchedules();

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

} // ScheduleManager

} // EnergyPlus

#endif
