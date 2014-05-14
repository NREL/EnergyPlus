#ifndef UtilityRoutines_hh_INCLUDED
#define UtilityRoutines_hh_INCLUDED

#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

void
AbortEnergyPlus(
	bool const NoIdf, // Set to true when "noidf" was found
	bool const NoIDD // Set to true when "noidd" was found
);

void
CloseMiscOpenFiles();

void
CloseOutOpenFiles();

void
EndEnergyPlus();

int
GetNewUnitNumber();

int
FindUnitNumber( std::string const & FileName ); // File name to be searched.

void
ConvertCaseToUpper(
	std::string const & InputString, // Input string
	std::string & OutputString // Output string (in UpperCase)
);

void
ConvertCaseToLower(
	std::string const & InputString, // Input string
	std::string & OutputString // Output string (in LowerCase)
);

std::string::size_type
FindNonSpace( std::string const & String ); // String to be scanned

inline
double
second_power( double const & baseValue )
{
	return baseValue * baseValue;
}

inline
double
third_power( double const & baseValue )
{
	return baseValue * baseValue * baseValue;
}

inline
double
fourth_power( double const & baseValue )
{
	double result = baseValue * baseValue;
	return result * result;
}

inline
double
fifth_power( double const & baseValue )
{
	double result = baseValue * baseValue;
	result *= result;
	return result * baseValue;
}

inline
double
sixth_power( double const & baseValue )
{
	double result = baseValue * baseValue;
	result *= result;
	return result * result;
}

inline
double
seventh_power( double const & baseValue )
{
	double result = baseValue * baseValue;
	result *= result;
	result *= result;
	return result * baseValue;
}


/** This function takes a baseValue and raises it to the exponentValue power.
  * This implementation has a speed advantage over std::pow for low integer
  * values of exponentValue where this unrolls the multiplication. Certain
  * compilers may perform this exact optimization, but not all. **/
// inline
// double
// power( double const & baseValue, int const & exponentValue)
// {
// 	if ( exponentValue == 0 ) { return 1.0; }
// 	double result = 1.0;
// 	bool const negative = exponentValue < 0;
// 	switch( negative ? -exponentValue : exponentValue ) {
// 		case 1:
// 			result = baseValue;
// 			break;
// 		case 2:
// 			result = baseValue * baseValue;
// 			break;
// 		case 3:
// 			result = baseValue * baseValue;
// 			result *= baseValue;
// 			break;
// 		case 4:
// 			result = baseValue * baseValue;
// 			result *= result;
// 			break;
// 		case 5:
// 			result = baseValue * baseValue;
// 			result *= result;
// 			result *= baseValue;
// 			break;
// 		case 6:
// 			result = baseValue * baseValue;
// 			result *= result;
// 			result *= result;
// 			break;
// 		case 7:
// 			result = baseValue * baseValue;
// 			result *= result;
// 			result *= result;
// 			result *= baseValue;
// 			break;
// 		default: return std::pow( baseValue, exponentValue );
// 	}
// 	return negative ? 1.0 / result : result;
// }

/** This function overloads std::pow() so std::pow is used. This allows all
  * calls to use std::pow() instead of std::pow in the code. **/
// inline
// double
// power( double const & baseValue, double const & exponentValue)
// {
// 	return std::pow( baseValue, exponentValue );
// }

/** This function overloads std::pow() so std::pow is used. This allows all
  * calls to use std::pow() instead of std::pow in the code. **/
// inline
// float
// power( float const & baseValue, float const & exponentValue)
// {
// 	return std::pow( baseValue, exponentValue );
// }

/** This function overloads std::pow() so std::pow is used. This allows all
  * calls to use std::pow() instead of std::pow in the code. **/
// inline
// long double
// power( long double const & baseValue, long double const & exponentValue)
// {
// 	return std::pow( baseValue, exponentValue );
// }

bool
env_var_on( std::string const & env_var_str );

void
ShowFatalError(
	std::string const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowSevereError(
	std::string const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowSevereMessage(
	std::string const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowContinueError(
	std::string const & Message,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowContinueErrorTimeStamp(
	std::string const & Message,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowMessage(
	std::string const & Message,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowWarningError(
	std::string const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowWarningMessage(
	std::string const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowRecurringSevereErrorAtEnd(
	std::string const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf = _, // Track and report the sum of the values passed to this argument
	std::string const & ReportMaxUnits = "", // optional char string (<=15 length) of units for max value
	std::string const & ReportMinUnits = "", // optional char string (<=15 length) of units for min value
	std::string const & ReportSumUnits = "" // optional char string (<=15 length) of units for sum value
);

void
ShowRecurringWarningErrorAtEnd(
	std::string const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf = _, // Track and report the sum of the values passed to this argument
	std::string const & ReportMaxUnits = "", // optional char string (<=15 length) of units for max value
	std::string const & ReportMinUnits = "", // optional char string (<=15 length) of units for min value
	std::string const & ReportSumUnits = "" // optional char string (<=15 length) of units for sum value
);

void
ShowRecurringContinueErrorAtEnd(
	std::string const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf = _, // Track and report the sum of the values passed to this argument
	std::string const & ReportMaxUnits = "", // optional char string (<=15 length) of units for max value
	std::string const & ReportMinUnits = "", // optional char string (<=15 length) of units for min value
	std::string const & ReportSumUnits = "" // optional char string (<=15 length) of units for sum value
);

void
StoreRecurringErrorMessage(
	std::string const & ErrorMessage, // Message automatically written to "error file" at end of simulation
	int & ErrorMsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ErrorReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ErrorReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ErrorReportSumOf = _, // Track and report the sum of the values passed to this argument
	std::string const & ErrorReportMaxUnits = "", // Units for "max" reporting
	std::string const & ErrorReportMinUnits = "", // Units for "min" reporting
	std::string const & ErrorReportSumUnits = "" // Units for "sum" reporting
);

void
ShowErrorMessage(
	std::string const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
SummarizeErrors();

void
ShowRecurringErrors();

//     NOTICE
//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
//     and The Regents of the University of California through Ernest Orlando Lawrence
//     Berkeley National Laboratory.  All rights reserved.
//     Portions of the EnergyPlus software package have been developed and copyrighted
//     by other individuals, companies and institutions.  These portions have been
//     incorporated into the EnergyPlus software package under license.   For a complete
//     list of contributors, see "Notice" located in EnergyPlus.f90.
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

} // EnergyPlus

#endif
