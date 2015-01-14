#ifndef OutputReports_hh_INCLUDED
#define OutputReports_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

void
ReportSurfaces();

void
LinesOut( std::string const & option );

void
DXFOut(
	std::string & PolygonAction,
	std::string & ColorScheme // Name from user for color scheme or blank
);

void
DXFOutLines( std::string const & ColorScheme );

void
DXFOutWireFrame( std::string const & ColorScheme );

void
DetailsForSurfaces( int const RptType ); // (1=Vertices only, 10=Details only, 11=Details with vertices)

void
CostInfoOut();

void
VRMLOut(
	std::string & PolygonAction,
	std::string & ColorScheme
);

//     NOTICE
//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
//     and The Regents of the University of California through Ernest Orlando Lawrence
//     Berkeley National Laboratory.  All rights reserved.
//     Portions of the EnergyPlus software package have been developed and copyrighted
//     by other individuals, companies and institutions.  These portions have been
//     incorporated into the EnergyPlus software package under license.   For a complete
//     list of contributors, see "Notice" located in main.cc.
//     NOTICE: The U.S. Government is granted for itself and others acting on its
//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
//     reproduce, prepare derivative works, and perform publicly and display publicly.
//     Beginning five (5) years after permission to assert copyright is granted,
//     subject to two possible five year renewals, the U.S. Government is granted for
//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
//     worldwide license in this data to reproduce, prepare derivative works,
//     distribute copies to the public, perform publicly and display publicly, and to
//     permit others to do so.
//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.


} // EnergyPlus

#endif
