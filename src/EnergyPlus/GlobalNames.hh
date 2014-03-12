#ifndef GlobalNames_hh_INCLUDED
#define GlobalNames_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GlobalNames {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumChillers;
	extern int NumBoilers;
	extern int NumBaseboards;
	extern int NumCoils;
	extern int CurMaxChillers;
	extern int CurMaxBoilers;
	extern int CurMaxBaseboards;
	extern int CurMaxCoils;

	// SUBROUTINE SPECIFICATIONS FOR MODULE GlobalNames:

	// Types

	struct ComponentNameData
	{
		// Members
		Fstring CompType; // Component Type
		Fstring CompName; // Component Name (user supplied)

		// Default Constructor
		ComponentNameData() :
			CompType( MaxNameLength ),
			CompName( MaxNameLength )
		{}

		// Member Constructor
		ComponentNameData(
			Fstring const & CompType, // Component Type
			Fstring const & CompName // Component Name (user supplied)
		) :
			CompType( MaxNameLength, CompType ),
			CompName( MaxNameLength, CompName )
		{}

	};

	// Object Data
	extern FArray1D< ComponentNameData > ChillerNames;
	extern FArray1D< ComponentNameData > BoilerNames;
	extern FArray1D< ComponentNameData > BaseboardNames;
	extern FArray1D< ComponentNameData > CoilNames;

	// Functions

	void
	VerifyUniqueChillerName(
		Fstring const & TypeToVerify,
		Fstring const & NameToVerify,
		bool & ErrorFound,
		Fstring const & StringToDisplay
	);

	void
	VerifyUniqueBaseboardName(
		Fstring const & TypeToVerify,
		Fstring const & NameToVerify,
		bool & ErrorFound,
		Fstring const & StringToDisplay
	);

	void
	VerifyUniqueBoilerName(
		Fstring const & TypeToVerify,
		Fstring const & NameToVerify,
		bool & ErrorFound,
		Fstring const & StringToDisplay
	);

	void
	VerifyUniqueCoilName(
		Fstring const & TypeToVerify,
		Fstring const & NameToVerify,
		bool & ErrorFound,
		Fstring const & StringToDisplay
	);

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

} // GlobalNames

} // EnergyPlus

#endif
