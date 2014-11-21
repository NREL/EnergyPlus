#include <EnergyPlusPgm.hh>
#include <CommandLineInterface.hh>
using EnergyPlus::CommandLineInterface::ProcessArgs;

int
main( int argc, const char * argv[] )
{
	ProcessArgs( argc, argv );
	EnergyPlusPgm();
}
