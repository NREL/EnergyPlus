#ifndef DataViewFactorInformation_hh_INCLUDED
#define DataViewFactorInformation_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataViewFactorInformation {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	// Types

	struct ZoneViewFactorInformation
	{
		// Members
		Fstring Name; // Zone name
		int NumOfSurfaces; // Number of surfaces in the zone
		FArray2D< Real64 > F; // View Factors
		FArray2D< Real64 > ScriptF; // Hottel's Script F
		FArray1D< Real64 > Area; // Surface area
		FArray1D< Real64 > Emissivity; // Surface emissivity
		FArray1D< Real64 > Azimuth; // Azimuth angle of the surface (in degrees)
		FArray1D< Real64 > Tilt; // Tilt angle of the surface (in degrees)
		FArray1D_int SurfacePtr; // Surface ALLOCATABLE (to Surface derived type)
		FArray1D_Fstring Class; // Class of surface (Wall, Roof, etc.)

		// Default Constructor
		ZoneViewFactorInformation() :
			Name( MaxNameLength ),
			NumOfSurfaces( 0 ),
			Class( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		ZoneViewFactorInformation(
			Fstring const & Name, // Zone name
			int const NumOfSurfaces, // Number of surfaces in the zone
			FArray2< Real64 > const & F, // View Factors
			FArray2< Real64 > const & ScriptF, // Hottel's Script F
			FArray1< Real64 > const & Area, // Surface area
			FArray1< Real64 > const & Emissivity, // Surface emissivity
			FArray1< Real64 > const & Azimuth, // Azimuth angle of the surface (in degrees)
			FArray1< Real64 > const & Tilt, // Tilt angle of the surface (in degrees)
			FArray1_int const & SurfacePtr, // Surface ALLOCATABLE (to Surface derived type)
			FArray1_Fstring const & Class // Class of surface (Wall, Roof, etc.)
		) :
			Name( MaxNameLength, Name ),
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
	extern FArray1D< ZoneViewFactorInformation > ZoneInfo;

} // DataViewFactorInformation

} // EnergyPlus

#endif
