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

#ifndef UtilityRoutines_hh_INCLUDED
#define UtilityRoutines_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace UtilityRoutines {
	extern bool outputErrorHeader;
}

void
AbortEnergyPlus();

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

template< typename T >
inline
T
pow2( T const & x )
{
	return x * x;
}

template< typename T >
inline
T
pow3( T const & x )
{
	return x * x * x;
}

template< typename T >
inline
T
pow4( T const & x )
{
	T y( x * x );
	return y * y;
}

template< typename T >
inline
T
pow5( T const & x )
{
	T y( x * x );
	y *= y;
	return y * x;
}

template< typename T >
inline
T
pow6( T const & x )
{
	T y( x * x );
	y *= y;
	return y * y;
}

template< typename T >
inline
T
pow7( T const & x )
{
	T y( x * x );
	y *= y;
	y *= y;
	return y * x;
}

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

} // EnergyPlus

#endif
