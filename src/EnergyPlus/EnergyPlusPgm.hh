#ifndef EnergyPlusPgm_hh_INCLUDED
#define EnergyPlusPgm_hh_INCLUDED

// C++ Headers
#include <string>

#ifdef _WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT 
#endif

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

	void EXPORT
	EnergyPlusPgm( std::string filepath = std::string() );

	void EXPORT
	StoreProgressCallback( void ( *f )( int ) );

	void EXPORT
	StoreMessageCallback( void ( *f )( std::string ) );

#endif
