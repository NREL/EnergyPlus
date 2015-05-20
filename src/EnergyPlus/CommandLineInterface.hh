#ifndef CommandLineInterface_hh_INCLUDED
#define CommandLineInterface_hh_INCLUDED

#include <string>
#include <EnergyPlusAPI.hh>

namespace EnergyPlus{

namespace CommandLineInterface {

 // Process command line arguments
 int
 ENERGYPLUSLIB_API ProcessArgs( int argc, const char * argv[] );

 void
 ReadINIFile(
 	int const UnitNumber, // Unit number of the opened INI file
 	std::string const & Heading, // Heading for the parameters ('[heading]')
 	std::string const & KindofParameter, // Kind of parameter to be found (String)
 	std::string & DataOut // Output from the retrieval
 );

} // CommandLineInterface

} // EnergyPlus

#endif
