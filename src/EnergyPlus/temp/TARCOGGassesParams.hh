#ifndef TARCOGGassesParams_hh_INCLUDED
#define TARCOGGassesParams_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TARCOGGassesParams {

	// Data
	//Max number of gasses
	extern int const maxgas;

	//Standards:
	extern int const ISO15099; // standard = ISO15099
	extern int const EN673; // standard = EN 673 / ISO 10292 Declared
	extern int const EN673Design; // standard = EN 673 / ISO 10292 Design

	extern int const MinStandard; // minimum index for standard
	extern int const MaxStandard; // maximum index for standard

	//REAL(r64), parameter :: pi       = 3.14159265358979323846d0
	//REAL(r64), parameter :: UniversalGasConst = 8314.462175d0 !(J/mol*K)
	extern Real64 const alpha1; // accomodation coefficient for low pressure gas calculations
	extern Real64 const alpha2; // accomodation coefficient for low pressure gas calculations
	extern Real64 const InputDataTolerance; // coefficient used for input data tolerance in case for displaying error message

} // TARCOGGassesParams

} // EnergyPlus

#endif
