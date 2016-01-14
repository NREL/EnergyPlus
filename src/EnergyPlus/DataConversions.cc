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
#include <DataConversions.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataConversions {

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 1998
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for conversion unit variables which
	// strictly "should not be needed" in EnergyPlus but may be used in a few
	// places (e.g. Conduction Transfer Functions).  We have adopted the "international
	// table" for conversions.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// Original Reference: DARCOM P 706-470, Engineering Design Handbook,
	// Metric Conversion Guide, July 1976.
	// Federal Standard 376B, January 27, 1993.  Preferred metric units
	// for general use by the Federal Government.
	// ASHRAE has similar recommendations.

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	//REAL(r64), PARAMETER:: CFC     =4.184D0            ! Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
	Real64 const CFC( 4.1868 ); // Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
	//  above is listed in July 1976 publication as "International Table"
	Real64 const CFL( 0.3048 ); // Length:         FT * CFL = M
	Real64 const CFM( 0.45359237 ); // Mass:           LB * CFM = KG
	//REAL(r64), PARAMETER:: CFP     =249.082D0          ! Pressure:       IN-H2O * CFP = N/M**2
	// above is listed in July 1976 publication as in-water at 39.2 deg F
	Real64 const CFP( 248.84 ); // Pressure:       IN-H2O * CFP = N/M**2
	//  above is listed in July 1976 publication as in-water at 60 deg F
	Real64 const DELTMP( -32.0 ); // Temperature:    (F + DELTMP) * CFT = C
	Real64 const CFA( CFL * CFL ); // Area:           FT**2 * CFA = M**2
	Real64 const CFT( 5.0 / 9.0 ); // Temperature:    R * CFT = K
	Real64 const CFV( CFA * CFL ); // Volume:         FT**3 * CFV = M**3
	Real64 const CFE( CFC * CFM * CFT / 3.6 ); // Energy:         BTU * CFE = W-HR
	Real64 const CFD( CFM / CFV ); // Density:        LB/FT**3 * CFD = KG/M**3
	Real64 const CFH( CFC * CFT ); // Enthalpy:       BTU/LB * CFH = J/KG
	Real64 const CFK( CFE / ( CFL * CFT ) ); // Conductivity:   BTU/(HR*FT*R) * CFK = W/(M*K)
	Real64 const CFMF( CFM / 3600.0 ); // Mass Flow:      LB/HR * CFMF = KG/SEC
	Real64 const CFQ( CFE ); // Power:          BTU/HR * CFQ = W
	Real64 const CFU( CFK / CFL ); // U-Value:        BTU/(HR*FT**2*R) * CFU = W/(M**2*K)
	// Note:  R-Value = 1/U-Value
	Real64 const CFS( CFL / 60.0 ); // Speed:          FT/MIN * CFS = M/SEC
	Real64 const CFVF( CFV / 60.0 ); // Volume Flow:    FT**3/MIN * CFVF = M**3/SEC
	Real64 const CFHF( CFQ / CFA ); // Heat Flux:      BTU/(HR*FT**2) * CFHF = W/M**2
	Real64 const CFTMP( DELTMP ); // Temperature:    Same as DELTMP

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

} // DataConversions

} // EnergyPlus
