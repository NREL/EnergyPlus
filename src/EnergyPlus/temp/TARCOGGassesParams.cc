// EnergyPlus Headers
#include <TARCOGGassesParams.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace TARCOGGassesParams {

	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   August/2011
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Keeps common data used by gasses and tarcog routines

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	//Max number of gasses
	int const maxgas( 10 );

	//Standards:
	int const ISO15099( 1 ); // standard = ISO15099
	int const EN673( 2 ); // standard = EN 673 / ISO 10292 Declared
	int const EN673Design( 3 ); // standard = EN 673 / ISO 10292 Design

	int const MinStandard( 1 ); // minimum index for standard
	int const MaxStandard( 3 ); // maximum index for standard

	//REAL(r64), parameter :: pi       = 3.14159265358979323846d0
	//REAL(r64), parameter :: UniversalGasConst = 8314.462175d0 !(J/mol*K)
	Real64 const alpha1( 0.5 ); // accomodation coefficient for low pressure gas calculations
	Real64 const alpha2( 0.5 ); // accomodation coefficient for low pressure gas calculations
	Real64 const InputDataTolerance( 1.0e-7 ); // coefficient used for input data tolerance in case for displaying error message

	//REAL(r64) :: gcon(maxgas,3), gvis(maxgas,3), gcp(maxgas,3), grho(maxgas,3), wght(maxgas)

	// Gas properties (ISO 15099 - Regression constants from Annex B):
	//DATA gcon / 2.873d-3,   2.285d-3,  9.443d-4,  4.538d-4,  0,0,0,0,0,0, &
	//          & 7.760d-5,   5.149d-5,  2.826d-5,  1.723d-5,  0,0,0,0,0,0, &
	//          & 0.0,        0.0,       0.0,       0.0,       0,0,0,0,0,0/
	//DATA gvis / 3.723d-6,   3.379d-6,  2.213d-6,  1.069d-6,  0,0,0,0,0,0, &
	//          & 4.940d-8,   6.451d-8,  7.777d-8,  7.414d-8,  0,0,0,0,0,0, &
	//          &  0.0,        0.0,       0.0,       0.0,       0,0,0,0,0,0/
	//DATA gcp  / 1.002737d3, 0.521929d3,0.248091d3,0.158340d3,0,0,0,0,0,0, &
	//          & 1.2324d-2,  0,         0,         0,         0,0,0,0,0,0, &
	//          & 0,          0,         0,         0,         0,0,0,0,0,0/

	//  Mollecular weights (ISO 15099 - from Annex B):
	//DATA wght / 28.97d0,      39.948d0,    83.8d0,      131.3d0,     0,0,0,0,0,0/

	//SAVE gcon, gvis, gcp, grho, wght

	//contains

	// GetGasIndex - returns index of a gas (from ISO15099 gas properties list) based on its molecular weight
	//integer function GetGasIndex (molweight)

	//  REAL(r64) :: molweight
	//  integer :: i

	//  GetGasIndex = 0  ! unknown gas

	//  do i = 1, maxgas
	//    if (ABS(molweight-wght(i)).lt.1.0d-5) then
	//      GetGasIndex = i
	//      EXIT  ! exit loop
	//    end if
	//  end do

	//  return

	//end function GetGasIndex

	//     NOTICE
	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // TARCOGGassesParams

} // EnergyPlus
