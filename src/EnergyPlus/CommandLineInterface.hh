#ifndef CommandLineInterface_hh_INCLUDED
#define CommandLineInterface_hh_INCLUDED

#include <string>
namespace EnergyPlus{

namespace options {


 // Process command line arguments
 int
 ProcessArgs( int argc, const char * argv[] );
 }
}
#endif
