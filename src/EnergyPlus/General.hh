#ifndef General_hh_INCLUDED
#define General_hh_INCLUDED

// C++ Headers
#include <functional>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace General {

	// Data
	// This module should not contain variables in the module sense as it is
	// intended strictly to provide "interfaces" to routines used by other
	// parts of the simulation.

	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// na

	//SUBROUTINE SPECIFICATIONS FOR MODULE General
	//PUBLIC  SaveCompDesWaterFlow
	//PUBLIC  ErfFunction

	// Functions

	void
	SolveRegulaFalsi(
		Real64 const Eps, // required absolute accuracy
		int const MaxIte, // maximum number of allowed iterations
		int & Flag, // integer storing exit status
		Real64 & XRes, // value of x that solves f(x [,Par]) = 0
		std::function< Real64( Real64 const, Array1< Real64 > const & ) > f,
		Real64 const X_0, // 1st bound of interval that contains the solution
		Real64 const X_1, // 2nd bound of interval that contains the solution
		Array1< Real64 > const & Par // array with additional parameters used for function evaluation
	);

	void
	SolveRegulaFalsi(
		Real64 const Eps, // required absolute accuracy
		int const MaxIte, // maximum number of allowed iterations
		int & Flag, // integer storing exit status
		Real64 & XRes, // value of x that solves f(x) = 0
		std::function< Real64( Real64 const ) > f,
		Real64 const X_0, // 1st bound of interval that contains the solution
		Real64 const X_1 // 2nd bound of interval that contains the solution
	);

	Real64
	InterpSw(
		Real64 const SwitchFac, // Switching factor: 0.0 if glazing is unswitched, = 1.0 if fully switched
		Real64 const A, // Glazing property in unswitched state
		Real64 const B // Glazing property in fully switched state
	);

	Real64
	InterpBlind(
		Real64 const ProfAng, // Profile angle (rad)
		Array1A< Real64 > const PropArray // Array of blind properties
	);

	Real64
	InterpProfAng(
		Real64 const ProfAng, // Profile angle (rad)
		Array1S< Real64 > const PropArray // Array of blind properties
	);

//	Real64
//	InterpSlatAng(
//		Real64 const SlatAng, // Slat angle (rad)
//		bool const VarSlats, // True if slat angle is variable
//		Array1A< Real64 > const PropArray // Array of blind properties as function of slat angle
//	);

	Real64
	InterpSlatAng(
		Real64 const SlatAng, // Slat angle (rad)
		bool const VarSlats, // True if slat angle is variable
		Array1S< Real64 > const PropArray // Array of blind properties as function of slat angle
	);

	Real64
	InterpProfSlatAng(
		Real64 const ProfAng, // Profile angle (rad)
		Real64 const SlatAng, // Slat angle (rad)
		bool const VarSlats, // True if variable-angle slats
		Array2A< Real64 > const PropArray // Array of blind properties
	);

	Real64
	BlindBeamBeamTrans(
		Real64 const ProfAng, // Solar profile angle (rad)
		Real64 const SlatAng, // Slat angle (rad)
		Real64 const SlatWidth, // Slat width (m)
		Real64 const SlatSeparation, // Slat separation (distance between surfaces of adjacent slats) (m)
		Real64 const SlatThickness // Slat thickness (m)
	);

	Real64
	POLYF(
		Real64 const X, // Cosine of angle of incidence
		Array1A< Real64 > const A // Polynomial coefficients
	);

	Real64
	POLYF(
		Real64 const X, // Cosine of angle of incidence
		Array1< Real64 > const & A // Polynomial coefficients
	);

	Real64
	POLYF(
		Real64 const X, // Cosine of angle of incidence
		Array1S< Real64 > const & A // Polynomial coefficients
	);

	Real64
	POLY1F(
		Real64 & X, // independent variable
		Array1A< Real64 > A, // array of polynomial coefficients
		int & N // number of terms in polynomial
	);

	Real64
	POLY2F(
		Real64 & X, // independent variable
		Array1A< Real64 > A, // array of polynomial coefficients
		int & N // number of terms in polynomial
	);

	std::string
	TrimSigDigits(
		Real64 const RealValue,
		int const SigDigits
	);

	std::string
	TrimSigDigits(
		int const IntegerValue,
		Optional_int_const SigDigits = _ // ignored
	);

	std::string
	RoundSigDigits(
		Real64 const RealValue,
		int const SigDigits
	);

	std::string
	RoundSigDigits(
		int const IntegerValue,
		Optional_int_const SigDigits = _ // ignored
	);

	std::string
	RemoveTrailingZeros( std::string const & InputString );

	std::string &
	strip_trailing_zeros( std::string & InputString );

	void
	MovingAvg(
		Array1A< Real64 > const DataIn, // input data that needs smoothing
		int const NumDataItems, // number of values in DataIn
		int const NumItemsInAvg, // number of items in the averaging window
		Array1A< Real64 > SmoothedData // output data after smoothing
	);

	void
	ProcessDateString(
		std::string const & String,
		int & PMonth,
		int & PDay,
		int & PWeekDay,
		int & DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
		bool & ErrorsFound,
		Optional_int PYear = _
	);

	void
	DetermineDateTokens(
		std::string const & String,
		int & NumTokens, // Number of tokens found in string
		int & TokenDay, // Value of numeric field found
		int & TokenMonth, // Value of Month field found (1=Jan, 2=Feb, etc)
		int & TokenWeekday, // Value of Weekday field found (1=Sunday, 2=Monday, etc), 0 if none
		int & DateType, // DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
		bool & ErrorsFound, // Set to true if cannot process this string as a date
		Optional_int TokenYear = _ // Value of Year if one appears to be present and this argument is present
	);

	void
	ValidateMonthDay(
		std::string const & String, // REAL(r64) string being processed
		int const Day,
		int const Month,
		bool & ErrorsFound
	);

	int
	JulianDay(
		int const Month, // Month, 1..12
		int const Day, // Day of Month, not validated by month
		int const LeapYearValue // 1 if leap year indicated, 0 if not
	);

	void
	InvJulianDay(
		int const Number,
		int & PMonth,
		int & PDay,
		int const LeapYr
	);

	bool
	BetweenDates(
		int const TestDate, // Date to test
		int const StartDate, // Start date in sequence
		int const EndDate // End date in sequence
	);

	std::string
	CreateSysTimeIntervalString();

	Real64
	SafeDivide(
		Real64 const a,
		Real64 const b
	);

	void
	Invert3By3Matrix(
		Array2A< Real64 > const A, // Input 3X3 Matrix
		Array2A< Real64 > InverseA // Output 3X3 Matrix - Inverse Of A
	);

	void
	Iterate(
		Real64 & ResultX, // ResultX is the final Iteration result passed back to the calling routine
		Real64 const Tol, // Tolerance for Convergence
		Real64 const X0, // Current value of X
		Real64 const Y0, // Current value of the function Y(X)
		Real64 & X1, // First Previous values of X
		Real64 & Y1, // First Previous values of Y(X1)
		int const Iter, // Number of iterations
		int & Cnvg // Convergence flag  Cnvg = 0:  Not converged
	);

	int
	FindNumberInList(
		int const WhichNumber,
		Array1A_int const ListOfItems,
		int const NumItems
	);

	template< typename A >
	inline
	int
	FindNumberInList(
		int const WhichNumber,
		MArray1< A, int > const & ListOfItems,
		int const NumItems
	)
	{
		return FindNumberInList( WhichNumber, Array1D_int( ListOfItems ), NumItems );
	}

	void
	DecodeMonDayHrMin(
		int const Item, // word containing encoded month, day, hour, minute
		int & Month, // month in integer format (1-12)
		int & Day, // day in integer format (1-31)
		int & Hour, // hour in integer format (1-24)
		int & Minute // minute in integer format (0:59)
	);

	int
	DetermineMinuteForReporting( int const IndexTypeKey ); // kind of reporting, Zone Timestep or System

	void
	EncodeMonDayHrMin(
		int & Item, // word containing encoded month, day, hour, minute
		int const Month, // month in integer format (1:12)
		int const Day, // day in integer format (1:31)
		int const Hour, // hour in integer format (1:24)
		int const Minute // minute in integer format (0:59)
	);

	int
	LogicalToInteger( bool const Flag );

	Real64
	GetCurrentHVACTime();

	Real64
	GetPreviousHVACTime();

	std::string
	CreateHVACTimeIntervalString();

	std::string
	CreateTimeString( Real64 const Time ); // Time in seconds

	std::string
	CreateTimeIntervalString(
		Real64 const StartTime, // Start of current interval in seconds
		Real64 const EndTime // End of current interval in seconds
	);

	void
	ParseTime(
		Real64 const Time, // Time value in seconds
		int & Hours, // Number of hours
		int & Minutes, // Number of minutes < 60
		Real64 & Seconds // Number of seconds < 60
	);

	void
	ScanForReports(
		std::string const & reportName,
		bool & DoReport,
		Optional_string_const ReportKey = _,
		Optional_string Option1 = _,
		Optional_string Option2 = _
	);

	inline
	void
	ReallocateRealArray(
		Array1D< Real64 > & Array,
		int & ArrayMax, // Current and resultant dimension for Array
		int const ArrayInc // increment for redimension
	)
	{
		Array.redimension( ArrayMax += ArrayInc, 0.0 );
	}

	void
	CheckCreatedZoneItemName(
		std::string const & calledFrom, // routine called from
		std::string const & CurrentObject, // object being parsed
		std::string const & ZoneName, // Zone Name associated
		std::string::size_type const MaxZoneNameLength, // maximum length of zonelist zone names
		std::string const & ItemName, // Item name (People, Lights, etc object)
		Array1S_string const ItemNames, // Item Names to check for duplication
		int const NumItems, // Number of items in ItemNames array
		std::string & ResultName, // Resultant name
		bool & errFlag // Error flag set to true if error found here.
	);

	template< typename A >
	inline
	void
	CheckCreatedZoneItemName(
		std::string const & calledFrom, // routine called from
		std::string const & CurrentObject, // object being parsed
		std::string const & ZoneName, // Zone Name associated
		std::string::size_type const MaxZoneNameLength, // maximum length of zonelist zone names
		std::string const & ItemName, // Item name (People, Lights, etc object)
		MArray1< A, std::string > const & ItemNames, // Item Names to check for duplication
		int const NumItems, // Number of items in ItemNames array
		std::string & ResultName, // Resultant name
		bool & errFlag // Error flag set to true if error found here.
	)
	{
		CheckCreatedZoneItemName( calledFrom, CurrentObject, ZoneName, MaxZoneNameLength, ItemName, Array1D_string( ItemNames ), NumItems, ResultName, errFlag );
	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // General

} // EnergyPlus

#endif
