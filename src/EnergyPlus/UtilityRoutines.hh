#ifndef UtilityRoutines_hh_INCLUDED
#define UtilityRoutines_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Fstring.hh>
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
FindUnitNumber( Fstring const & FileName ); // File name to be searched.

void
ConvertCaseToUpper(
	Fstring const & InputString, // Input string
	Fstring & OutputString // Output string (in UpperCase)
);

void
ConvertCaseToLower(
	Fstring const & InputString, // Input string
	Fstring & OutputString // Output string (in LowerCase)
);

int
FindNonSpace( Fstring const & String ); // String to be scanned

void
ShowFatalError(
	Fstring const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowSevereError(
	Fstring const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowSevereMessage(
	Fstring const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowContinueError(
	Fstring const & Message,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowContinueErrorTimeStamp(
	Fstring const & Message,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowMessage(
	Fstring const & Message,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowWarningError(
	Fstring const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowWarningMessage(
	Fstring const & ErrorMessage,
	Optional_int OutUnit1 = _,
	Optional_int OutUnit2 = _
);

void
ShowRecurringSevereErrorAtEnd(
	Fstring const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf = _, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ReportMaxUnits = _, // optional char string (<=15 length) of units for max value
	Optional_Fstring_const ReportMinUnits = _, // optional char string (<=15 length) of units for min value
	Optional_Fstring_const ReportSumUnits = _ // optional char string (<=15 length) of units for sum value
);

void
ShowRecurringWarningErrorAtEnd(
	Fstring const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf = _, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ReportMaxUnits = _, // optional char string (<=15 length) of units for max value
	Optional_Fstring_const ReportMinUnits = _, // optional char string (<=15 length) of units for min value
	Optional_Fstring_const ReportSumUnits = _ // optional char string (<=15 length) of units for sum value
);

void
ShowRecurringContinueErrorAtEnd(
	Fstring const & Message, // Message automatically written to "error file" at end of simulation
	int & MsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ReportSumOf = _, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ReportMaxUnits = _, // optional char string (<=15 length) of units for max value
	Optional_Fstring_const ReportMinUnits = _, // optional char string (<=15 length) of units for min value
	Optional_Fstring_const ReportSumUnits = _ // optional char string (<=15 length) of units for sum value
);

void
StoreRecurringErrorMessage(
	Fstring const & ErrorMessage, // Message automatically written to "error file" at end of simulation
	int & ErrorMsgIndex, // Recurring message index, if zero, next available index is assigned
	Optional< Real64 const > ErrorReportMaxOf = _, // Track and report the max of the values passed to this argument
	Optional< Real64 const > ErrorReportMinOf = _, // Track and report the min of the values passed to this argument
	Optional< Real64 const > ErrorReportSumOf = _, // Track and report the sum of the values passed to this argument
	Optional_Fstring_const ErrorReportMaxUnits = _, // Units for "max" reporting
	Optional_Fstring_const ErrorReportMinUnits = _, // Units for "min" reporting
	Optional_Fstring_const ErrorReportSumUnits = _ // Units for "sum" reporting
);

void
ShowErrorMessage(
	Fstring const & ErrorMessage,
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
