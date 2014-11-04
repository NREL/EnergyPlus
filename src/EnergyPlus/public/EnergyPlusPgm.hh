#ifndef EnergyPlusPgm_hh_INCLUDED
#define EnergyPlusPgm_hh_INCLUDED

#include <EnergyPlusAPI.hh>

// C++ Headers
#include <string>

	// Functions

	void
	CreateCurrentDateTimeString( std::string & CurrentDateTimeString );

	void ENERGYPLUSLIB_API
	EnergyPlusPgm( int argc, const char * argv[]);

	void ENERGYPLUSLIB_API
	StoreProgressCallback( void ( *f )( int ) );

	void ENERGYPLUSLIB_API
	StoreMessageCallback( void ( *f )( std::string ) );

#endif
