#include <iostream>
#include "EnergyPlusPgm.hh"

void message_callback_handler( std::string const & message )
{
	std::cout << "EnergyPlusLibrary (message): " << message << std::endl;
}

void progress_callback_handler( int const progress )
{
	std::cout << "EnergyPlusLibrary (progress): " << progress << std::endl;
}

int main( int argc, char* argv[] )
{
	std::cout << "Using EnergyPlus as a library." << std::endl;
	StoreMessageCallback( message_callback_handler );
	StoreProgressCallback( progress_callback_handler );

	if ( argc < 2 ) {
		std::cout << "Call this with a path to run EnergyPlus as the only argument" << std::endl;
		return 1;
	} else {
		EnergyPlusPgm( argv[1] );
	}
}
