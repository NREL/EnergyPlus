// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ReportSizingManager.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <OutputReportPredefined.hh>
#include <SQLiteProcedures.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ReportSizingManager {

	// Module containing the routines dealing with the <module_name>

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie<author>
	//       DATE WRITTEN   November 2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Provide module interface for ReportSizingOutput

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// na

	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Functions

	void
	ReportSizingOutput(
		Fstring const & CompType, // the type of the component
		Fstring const & CompName, // the name of the component
		Fstring const & VarDesc, // the description of the input variable
		Real64 const VarValue, // the value from the sizing calculation
		Optional_Fstring_const UsrDesc, // the description of a user-specified variable
		Optional< Real64 const > UsrValue // the value from the user for the desc item
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Decenber 2001
		//       MODIFIED       August 2008, Greg Stark
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes one item of sizing data to the "eio" file..

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::OutputFileInits;
		using namespace OutputReportPredefined;
		using General::RoundSigDigits;
		using namespace SQLiteProcedures;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );

		// Formats
		std::string const Format_990( "('! <Component Sizing Information>, Component Type, Component Name, ','Input Field Description, Value')" );
		std::string const Format_991( "(' Component Sizing Information, ',A,', ',A,', ',A,', ',A)" );

		if ( MyOneTimeFlag ) {
			gio::write( OutputFileInits, Format_990 );
			MyOneTimeFlag = false;
		}

		gio::write( OutputFileInits, Format_991 ) << trim( CompType ) << trim( CompName ) << trim( VarDesc ) << trim( RoundSigDigits( VarValue, 5 ) );
		//add to tabular output reports
		AddCompSizeTableEntry( CompType, CompName, VarDesc, VarValue );

		if ( present( UsrDesc ) && present( UsrValue ) ) {
			gio::write( OutputFileInits, Format_991 ) << trim( CompType ) << trim( CompName ) << trim( UsrDesc ) << trim( RoundSigDigits( UsrValue, 5 ) );
			AddCompSizeTableEntry( CompType, CompName, UsrDesc, UsrValue );
		} else if ( present( UsrDesc ) || present( UsrValue ) ) {
			ShowFatalError( "ReportSizingOutput: (Developer Error) - called with user-specified description or value but not both." );
		}

		// add to SQL output
		if ( WriteOutputToSQLite ) AddSQLiteComponentSizingRecord( CompType, CompName, VarDesc, VarValue );

	}

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

} // ReportSizingManager

} // EnergyPlus
