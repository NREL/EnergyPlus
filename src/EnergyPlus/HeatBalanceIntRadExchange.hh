#ifndef HeatBalanceIntRadExchange_hh_INCLUDED
#define HeatBalanceIntRadExchange_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

#define EP_HBIRE_SEQ

namespace HeatBalanceIntRadExchange {

	// Data
	// MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int MaxNumOfZoneSurfaces; // Max saved to get large enough space for user input view factors

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceIntRadExchange

	// Functions
	void
	clear_state();

	void
	CalcInteriorRadExchange(
		Array1S< Real64 > const SurfaceTemp, // Current surface temperatures
		int const SurfIterations, // Number of iterations in calling subroutine
		Array1< Real64 > & NetLWRadToSurf, // Net long wavelength radiant exchange from other surfaces
		Optional_int_const ZoneToResimulate = _, // if passed in, then only calculate for this zone
		std::string const & CalledFrom = ""
	);

	void
	InitInteriorRadExchange();

	void
	GetInputViewFactors(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		Array2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		Array1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	);

	void
	GetInputViewFactorsbyName(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		Array2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		Array1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	);

	void
	CalcApproximateViewFactors(
		int const N, // NUMBER OF SURFACES
		Array1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array1A< Real64 > const Azimuth, // Facing angle of the surface (in degrees)
		Array1A< Real64 > const Tilt, // Tilt angle of the surface (in degrees)
		Array2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		Array1A_int const SPtr // pointer to REAL(r64) surface number (for error message)
	);

	void
	FixViewFactors(
		int const N, // NUMBER OF SURFACES
		Array1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		int const ZoneNum, // Zone number being fixe
		Real64 & OriginalCheckValue, // check of SUM(F) - N
		Real64 & FixedCheckValue, // check after fixed of SUM(F) - N
		Real64 & FinalCheckValue, // the one to go with
		int & NumIterations, // number of iterations to fixed
		Real64 & RowSum // RowSum of Fixed
	);

	void
	CalcScriptF(
		int const N, // Number of surfaces
		Array1< Real64 > const & A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array2< Real64 > const & F, // DIRECT VIEW FACTOR MATRIX (N X N)
		Array1< Real64 > & EMISS, // VECTOR OF SURFACE EMISSIVITIES
		Array2< Real64 > & ScriptF // MATRIX OF SCRIPT F FACTORS (N X N) //Tuned Transposed
	);

	void
	CalcMatrixInverse(
		Array2< Real64 > & A, // Matrix: Gets reduced to L\U form
		Array2< Real64 > & I // Returned as inverse matrix
	);

} // HeatBalanceIntRadExchange

} // EnergyPlus

#endif
