#ifndef DataViewFactorInformation_hh_INCLUDED
#define DataViewFactorInformation_hh_INCLUDED

// for std::unique_ptr
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataViewFactorInformation {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	// Types

	struct ZoneViewFactorInformation
	{
		// Members
		std::string Name; // Zone name
		int NumOfSurfaces; // Number of surfaces in the zone
		int NumOfSurfacesVec; // Number of surfaces rounded up to multiple of vector size (2 or 4)
		Array2D< Real64 > F; // View Factors

		// Amir Roth 2015-07-01: ScriptF is a hand-rolled
		// "padded" 2D array, i.e., each row starts at an
		// aligned addresss.  This can/Should be replaced
		// with an Array2DPadded object in the future.

		Array2D< Real64 > ScriptF; // Hottel's ScriptF
		Array1D< Real64 > ScriptFRecvSurfSum; // Sum of ScriptF [ *, Recv ] for each receiving surface
		Array1D< Real64 > Area; // Surface area
		Array1D< Real64 > Emissivity; // Surface emissivity
		Array1D< Real64 > Azimuth; // Azimuth angle of the surface (in degrees)
		Array1D< Real64 > Tilt; // Tilt angle of the surface (in degrees)
		Array1D_int SurfacePtr; // Surface ALLOCATABLE (to Surface derived type)
		Array1D_string Class; // Class of surface (Wall, Roof, etc.)

		// Default Constructor
		ZoneViewFactorInformation() :
			NumOfSurfaces( 0 )
		{}

	};

	// Object Data
	extern Array1D< ZoneViewFactorInformation > ZoneInfo;

} // DataViewFactorInformation

} // EnergyPlus

#endif // DataViewFactorInformation_hh_INCLUDED

