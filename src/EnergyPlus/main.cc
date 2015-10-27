#include <EnergyPlusPgm.hh>
#include <CommandLineInterface.hh>
#include <iostream>
using EnergyPlus::CommandLineInterface::ProcessArgs;

int
main( int argc, const char * argv[] )
{
	std::cout.setf( std::ios_base::unitbuf );
	ProcessArgs( argc, argv );
	EnergyPlusPgm();
}
