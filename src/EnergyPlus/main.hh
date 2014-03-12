#ifndef main_hh_INCLUDED
#define main_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>


	// Functions

	void
	CreateCurrentDateTimeString( Fstring & CurrentDateTimeString );

	void
	ReadINIFile(
		int const UnitNumber, // Unit number of the opened INI file
		Fstring const & Heading, // Heading for the parameters ('[heading]')
		Fstring const & KindofParameter, // Kind of parameter to be found (String)
		Fstring & DataOut // Output from the retrieval
	);


#endif
