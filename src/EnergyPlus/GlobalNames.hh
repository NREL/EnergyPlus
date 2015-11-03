#ifndef GlobalNames_hh_INCLUDED
#define GlobalNames_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GlobalNames {

	// Using/Aliasing

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
	extern int numAirDistUnits; // count of air distribution units

	// SUBROUTINE SPECIFICATIONS FOR MODULE GlobalNames:

	// Types

	struct ComponentNameData
	{
		// Members
		std::string CompType; // Component Type
		std::string CompName; // Component Name (user supplied)

		// Default Constructor
		ComponentNameData()
		{}

		// Member Constructor
		ComponentNameData(
			std::string const & CompType, // Component Type
			std::string const & CompName // Component Name (user supplied)
		) :
			CompType( CompType ),
			CompName( CompName )
		{}

	};

	// Object Data
	extern Array1D< ComponentNameData > ChillerNames;
	extern Array1D< ComponentNameData > BoilerNames;
	extern Array1D< ComponentNameData > BaseboardNames;
	extern Array1D< ComponentNameData > CoilNames;
	extern Array1D< ComponentNameData > aDUNames;

	// Functions

	// for unit tests
	void
	clear_state();

	void
	VerifyUniqueChillerName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	);

	void
	VerifyUniqueBaseboardName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	);

	void
	VerifyUniqueBoilerName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	);

	void
	VerifyUniqueCoilName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	);

	void 
	VerifyUniqueADUName(
		std::string const & TypeToVerify,
		std::string const & NameToVerify,
		bool & ErrorFound,
		std::string const & StringToDisplay
	);

	// Clears the global data in GlobalNames.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // GlobalNames

} // EnergyPlus

#endif
