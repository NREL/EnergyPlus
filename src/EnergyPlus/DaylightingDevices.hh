#ifndef DaylightingDevices_hh_INCLUDED
#define DaylightingDevices_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DaylightingDevices {

	// Data
	// MODULE PARAMETER DEFINITIONS: na
	// DERIVED TYPE DEFINITIONS: na
	// MODULE VARIABLE TYPE DECLARATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D< Real64 > COSAngle; // List of cosines of incident angle

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	InitDaylightingDevices();

	void
	GetTDDInput();

	void
	GetShelfInput();

	Real64
	CalcPipeTransBeam(
		Real64 const R, // Reflectance of surface, constant (can be made R = f(theta) later)
		Real64 const A, // Aspect ratio, L / d
		Real64 const Theta // Angle of entry in radians
	);

	Real64
	CalcTDDTransSolIso( int const PipeNum ); // TDD pipe object number

	Real64
	CalcTDDTransSolHorizon( int const PipeNum ); // TDD pipe object number

	Real64
	CalcTDDTransSolAniso(
		int const PipeNum, // TDD pipe object number
		Real64 const COSI // Cosine of the incident angle
	);

	Real64
	TransTDD(
		int const PipeNum, // TDD pipe object number
		Real64 const COSI, // Cosine of the incident angle
		int const RadiationType // Radiation type flag
	);

	Real64
	InterpolatePipeTransBeam(
		Real64 const COSI, // Cosine of the incident angle
		Array1A< Real64 > const transBeam // Table of beam transmittance vs. cosine angle
	);

	int
	FindTDDPipe( int const WinNum );

	void
	DistributeTDDAbsorbedSolar();

	void
	CalcViewFactorToShelf( int const ShelfNum ); // Daylighting shelf object number

	void
	FigureTDDZoneGains();

} // DaylightingDevices

} // EnergyPlus

#endif
