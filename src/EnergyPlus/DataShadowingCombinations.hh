#ifndef DataShadowingCombinations_hh_INCLUDED
#define DataShadowingCombinations_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataShadowingCombinations {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	// Types

	struct ShadowingCombinations
	{
		// Members
		bool UseThisSurf; // True when this surface should be used in calculations
		int NumGenSurf; // Number of General surfaces for this surf
		Array1D_int GenSurf; // Array of General Surface Numbers
		int NumBackSurf; // Number of Back (Interior) surfaces for this surf
		Array1D_int BackSurf; // Array of Back (Interior) surface numbers
		int NumSubSurf; // Number of SubSurfaces for this surf
		Array1D_int SubSurf; // Array of SubSurface surface Numbers

		// Default Constructor
		ShadowingCombinations() :
			UseThisSurf( false ),
			NumGenSurf( 0 ),
			NumBackSurf( 0 ),
			NumSubSurf( 0 )
		{}

		// Member Constructor
		ShadowingCombinations(
			bool const UseThisSurf, // True when this surface should be used in calculations
			int const NumGenSurf, // Number of General surfaces for this surf
			Array1_int const & GenSurf, // Array of General Surface Numbers
			int const NumBackSurf, // Number of Back (Interior) surfaces for this surf
			Array1_int const & BackSurf, // Array of Back (Interior) surface numbers
			int const NumSubSurf, // Number of SubSurfaces for this surf
			Array1_int const & SubSurf // Array of SubSurface surface Numbers
		) :
			UseThisSurf( UseThisSurf ),
			NumGenSurf( NumGenSurf ),
			GenSurf( GenSurf ),
			NumBackSurf( NumBackSurf ),
			BackSurf( BackSurf ),
			NumSubSurf( NumSubSurf ),
			SubSurf( SubSurf )
		{}

	};

	// Object Data
	extern Array1D< ShadowingCombinations > ShadowComb;

} // DataShadowingCombinations

} // EnergyPlus

#endif
