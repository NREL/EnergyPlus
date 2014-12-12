#ifndef EnergyPlusPgm_hh_INCLUDED
#define EnergyPlusPgm_hh_INCLUDED

#include <EnergyPlusAPI.hh>

// C++ Headers
#include <string>

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

	void ENERGYPLUSLIB_API
	EnergyPlusPgm( std::string filepath = std::string() );

	void ENERGYPLUSLIB_API
	StoreProgressCallback( void ( *f )( int ) );

	void ENERGYPLUSLIB_API
	StoreMessageCallback( void ( *f )( std::string ) );

#endif
