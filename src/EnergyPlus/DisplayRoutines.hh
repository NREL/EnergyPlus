#ifndef DisplayRoutines_hh_INCLUDED
#define DisplayRoutines_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

void
DisplayString( std::string const & String ); // String to be displayed

void
DisplayString( char const * String ); // String to be displayed

void
DisplayNumberAndString(
	int const Number, // number to be displayed
	std::string const & String // String to be displayed
);

void
DisplaySimDaysProgress(
	int const CurrentSimDay, // Current Simulation Day
	int const TotalSimDays // Total number of Simulation Days
);

} // EnergyPlus

#endif
