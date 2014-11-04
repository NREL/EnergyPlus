#include <iostream>
#include "EnergyPlusPgm.hh"

void message_callback_handler(std::string message)
{
	std::cout << "EnergyPlusLibrary (message): " << message << std::endl;
}

void progress_callback_handler(int progress)
{
	std::cout << "EnergyPlusLibrary (progress): " << progress << std::endl;
}

int main( int argc, const char* argv[] )
{
	std::cout << "Using EnergyPlus as a library." << std::endl;
	StoreMessageCallback(message_callback_handler);
	StoreProgressCallback(progress_callback_handler);
	
	EnergyPlusPgm( argc, argv );
}
