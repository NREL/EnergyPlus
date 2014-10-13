#include <iostream>
#include "main.hh"

void message_callback_handler(std::string message)
{
	std::cout << "HiMessage: " << message << std::endl;
}

void progress_callback_handler(int progress)
{
	std::cout << "Progress: " << progress << std::endl;
}

int main( int argc, char* argv[] ) 
{
	std::cout << "Using EnergyPlus as a library." << std::endl;
	StoreMessageCallback(message_callback_handler);
	StoreProgressCallback(progress_callback_handler);
	
	if ( argc < 2)
	{
		std::cout << "Call this with a path to run EnergyPlus as the only argument" << std::endl;
		return 1;
	} else {
		EnergyPlusPgm( argv[1] );
	}
}
