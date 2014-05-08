#ifndef main_hh_INCLUDED
#define main_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>


	// Functions

	void
	CreateCurrentDateTimeString( std::string & CurrentDateTimeString );

	void
	ReadINIFile(
		int const UnitNumber, // Unit number of the opened INI file
		std::string const & Heading, // Heading for the parameters ('[heading]')
		std::string const & KindofParameter, // Kind of parameter to be found (String)
		std::string & DataOut // Output from the retrieval
	);


#endif
