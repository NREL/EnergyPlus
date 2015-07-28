#ifndef DataSurfaceColors_hh_INCLUDED
#define DataSurfaceColors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataSurfaceColors {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const NumColors;
	extern int const ColorNo_Text;
	extern int const ColorNo_Wall;
	extern int const ColorNo_Window;
	extern int const ColorNo_GlassDoor;
	extern int const ColorNo_Door;
	extern int const ColorNo_Floor;
	extern int const ColorNo_Roof;
	extern int const ColorNo_ShdDetBldg;
	extern int const ColorNo_ShdDetFix;
	extern int const ColorNo_ShdAtt;
	extern int const ColorNo_PV;
	extern int const ColorNo_TDDDome;
	extern int const ColorNo_TDDDiffuser;
	extern int const ColorNo_DaylSensor1;
	extern int const ColorNo_DaylSensor2;

	extern Array1D_int const defaultcolorno; // text | wall | window | glassdoor | door | floor | roof | detached building shade (moves with building) | detached building fixed | attached building shading | PV | TDD:Dome | TDD:Diffuser | Daylight Sensor 1 | Daylight Sensor 2

	extern Array1D_string const colorkeys;

	extern Array1D_int const colorkeyptr;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_int DXFcolorno;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Functions

	bool
	MatchAndSetColorTextString(
		std::string const & String, // string to be matched
		int const SetValue, // value to be used for the color
		Optional_string_const ColorType = _ // for now, must be DXF
	);

	void
	SetUpSchemeColors(
		std::string const & SchemeName,
		Optional_string_const ColorType = _
	);

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

} // DataSurfaceColors

} // EnergyPlus

#endif
