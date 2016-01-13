// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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

} // TARCOGGassesParams

} // EnergyPlus
