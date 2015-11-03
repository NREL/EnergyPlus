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

} // DataSurfaceColors

} // EnergyPlus

#endif
