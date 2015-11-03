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


} // EnergyPlus

#endif
