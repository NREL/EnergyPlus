#ifndef DataViewFactorInformation_hh_INCLUDED
#define DataViewFactorInformation_hh_INCLUDED

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
		Array2D< Real64 > F; // View Factors
		Array2D< Real64 > ScriptF; // Hottel's Script F //Tuned Transposed
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

		// Member Constructor
		ZoneViewFactorInformation(
			std::string const & Name, // Zone name
			int const NumOfSurfaces, // Number of surfaces in the zone
			Array2< Real64 > const & F, // View Factors
			Array2< Real64 > const & ScriptF, // Hottel's Script F //Tuned Transposed
			Array1< Real64 > const & Area, // Surface area
			Array1< Real64 > const & Emissivity, // Surface emissivity
			Array1< Real64 > const & Azimuth, // Azimuth angle of the surface (in degrees)
			Array1< Real64 > const & Tilt, // Tilt angle of the surface (in degrees)
			Array1_int const & SurfacePtr, // Surface ALLOCATABLE (to Surface derived type)
			Array1_string const & Class // Class of surface (Wall, Roof, etc.)
		) :
			Name( Name ),
			NumOfSurfaces( NumOfSurfaces ),
			F( F ),
			ScriptF( ScriptF ),
			Area( Area ),
			Emissivity( Emissivity ),
			Azimuth( Azimuth ),
			Tilt( Tilt ),
			SurfacePtr( SurfacePtr ),
			Class( Class )
		{}

	};

	// Object Data
	extern Array1D< ZoneViewFactorInformation > ZoneInfo;

} // DataViewFactorInformation

} // EnergyPlus

#endif
