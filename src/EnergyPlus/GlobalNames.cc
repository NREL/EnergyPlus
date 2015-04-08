// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <GlobalNames.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace GlobalNames {

	// Module containing the routines dealing with matching and assuring that
	// various component types are unique by name (e.g. Chillers).

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module allows for verification of uniqueness (by name) across
	// certain component names (esp. Chillers, Boilers)

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using InputProcessor::FindItemInList;
	using InputProcessor::MakeUPPERCase;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumChillers( 0 );
	int NumBoilers( 0 );
	int NumBaseboards( 0 );
	int NumCoils( 0 );
	int CurMaxChillers( 0 );
	int CurMaxBoilers( 0 );
	int CurMaxBaseboards( 0 );
	int CurMaxCoils( 0 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE GlobalNames:

	// Object Data
	Array1D< ComponentNameData > ChillerNames;
	Array1D< ComponentNameData > BoilerNames;
	Array1D< ComponentNameData > BaseboardNames;
	Array1D< ComponentNameData > CoilNames;

	// Functions

	void
	VerifyUniqueChillerName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// chillers.  If not found in the list, it is added before returning.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;
		if ( NumChillers > 0 ) Found = FindItemInList( NameToVerify, ChillerNames.CompName(), NumChillers );
		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Chiller Type=\"" + ChillerNames( Found ).CompType + "\"." );
			ShowContinueError( "...Current entry is Chiller Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumChillers == 0 ) {
				CurMaxChillers = 4;
				ChillerNames.allocate( CurMaxChillers );
			} else if ( NumChillers == CurMaxChillers ) {
				CurMaxChillers += 4;
				ChillerNames.redimension( CurMaxChillers );
			}
			++NumChillers;
			ChillerNames( NumChillers ).CompType = MakeUPPERCase( TypeToVerify );
			ChillerNames( NumChillers ).CompName = NameToVerify;
		}
	}

	void
	VerifyUniqueBaseboardName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// Baseboards.  If not found in the list, it is added before returning.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;

		if ( NumBaseboards > 0 ) Found = FindItemInList( NameToVerify, BaseboardNames.CompName(), NumBaseboards );

		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Baseboard Type=\"" + BaseboardNames( Found ).CompType + "\"." );
			ShowContinueError( "...Current entry is Baseboard Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumBaseboards == 0 ) {
				CurMaxBaseboards = 4;
				BaseboardNames.allocate( CurMaxBaseboards );
			} else if ( NumBaseboards == CurMaxBaseboards ) {
				CurMaxBaseboards += 4;
				BaseboardNames.redimension( CurMaxBaseboards );
			}
			++NumBaseboards;
			BaseboardNames( NumBaseboards ).CompType = TypeToVerify;
			BaseboardNames( NumBaseboards ).CompName = NameToVerify;
		}

	}

	void
	VerifyUniqueBoilerName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// Boilers.  If not found in the list, it is added before returning.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;

		if ( NumBoilers > 0 ) Found = FindItemInList( NameToVerify, BoilerNames.CompName(), NumBoilers );

		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Boiler Type=\"" + BoilerNames( Found ).CompType + "\"." );
			ShowContinueError( "...Current entry is Boiler Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumBoilers == 0 ) {
				CurMaxBoilers = 4;
				BoilerNames.allocate( CurMaxBoilers );
			} else if ( NumBoilers == CurMaxBoilers ) {
				CurMaxBoilers += 4;
				BoilerNames.redimension( CurMaxBoilers );
			}
			++NumBoilers;
			BoilerNames( NumBoilers ).CompType = TypeToVerify;
			BoilerNames( NumBoilers ).CompName = NameToVerify;
		}

	}

	void
	VerifyUniqueCoilName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name will be unique in the list of
		// Coils.  If not found in the list, it is added before returning.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		ErrorFound = false;
		int Found = 0;

		if ( NumCoils > 0 ) Found = FindItemInList( NameToVerify, CoilNames.CompName(), NumCoils );

		if ( Found != 0 ) {
			ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify + ", Coil Type=\"" + CoilNames( Found ).CompType + "\"" );
			ShowContinueError( "...Current entry is Coil Type=\"" + TypeToVerify + "\"." );
			ErrorFound = true;
		} else {
			if ( NumCoils == 0 ) {
				CurMaxCoils = 4;
				CoilNames.allocate( CurMaxCoils );
			} else if ( NumCoils == CurMaxCoils ) {
				CurMaxCoils += 4;
				CoilNames.redimension( CurMaxCoils );
			}
			++NumCoils;
			CoilNames( NumCoils ).CompType = MakeUPPERCase( TypeToVerify );
			CoilNames( NumCoils ).CompName = NameToVerify;
		}

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

} // GlobalNames

} // EnergyPlus
