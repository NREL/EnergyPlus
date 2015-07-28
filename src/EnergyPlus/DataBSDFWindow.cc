// EnergyPlus Headers
#include <DataBSDFWindow.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataBSDFWindow {

	// Module containing the data definitions dealing with calculating window optical
	// properties from BSDF data

	// MODULE INFORMATION:
	//       AUTHOR         Joseph Klems, Brent Griffith
	//       DATE WRITTEN   August 2011
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// define the data structures to be used in calculating solar
	//  transmittance and absorptance and associated arsenal
	//  of geometry and window state information necessary

	// METHODOLOGY EMPLOYED:
	// Matrix representation of bidirectional transmittance of radiance

	// REFERENCES:
	// to be added--Complex glazing pubs, WINDOW writeup(s)(?)

	// OTHER NOTES:
	// see Joe's draft "Including Non-Specular Fenestrations in EnergyPlus"

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataVectorTypes;

	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:

	int const BasisType_WINDOW( 1 );
	int const BasisType_Custom( 2 );

	int const BasisSymmetry_Axisymmetric( 1 );
	int const BasisSymmetry_None( 2 );

	// Thermal calculations for complex fenestration can be used to generate reports for standard cases
	// noCondition is used when performing timestep calculations
	// summerCondtion will override certain parameters so that produced results are matching standard summer WINDOW (software) results
	// winterCondition will override certain parameters so that produced resuls are matching standard winter WINDOW (software) results
	int const noCondition( 0 );
	int const summerCondition( 1 );
	int const winterCondition( 2 );

	// DERIVED TYPE DEFINITIONS:

	// Structure to keep reference points coefficients for different reference points and illuminance maps

	//Allocation of complex fenestration data:  SurfaceWindow(:)%ComplexFen is a structure of type BSDFWindowDescript
	//defined in DataSurfaces.  ComplexWind(:) is an array of type BSDF WindowGeomDescr defined as a module
	//variable in WindowComplexManager

	// MODULE VARIABLE DECLARATIONS:

	int TotComplexFenStates( 0 ); // Number of complex fenestration construction definitions
	int FirstBSDF( 0 ); // Location of first complex fenestration construction definition in Constr array
	int MaxBkSurf( 20 ); // was 20    Maximum number of back surfaces in solar overlap & interior solar distribution
	int TotThermalModels( 0 ); // Number of thermal models
	//calculation
	Array3D< Real64 > SUNCOSTS( 60, 24, 3 ); // Timestep values of solar direction cosines
	Array2D< Real64 > BSDFTempMtrx; // Temporary matrix for holding axisymmetric input

	// Object Data
	Array1D< BSDFWindowGeomDescr > ComplexWind; // Window geometry structure: set in CalcPerSolarBeam/SolarShading

	//     NOTICE
	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // DataBSDFWindow

} // EnergyPlus
