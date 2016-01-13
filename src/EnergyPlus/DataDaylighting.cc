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
#include <DataDaylighting.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataDaylighting {

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie/Fred Winkelmann (Re-engineered by Peter Graham Ellis)
	//       DATE WRITTEN   May 1998
	//       MODIFIED       B.Griffith added interior window data for associated exterior windows
	//       RE-ENGINEERED  April 2003

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for variables used in daylighting which
	// are shared by several modules.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	// Two kinds of reference points: used directly in daylighting, used to show illuminance map of zone
	int const MaxRefPoints( 2 ); // Maximum number of daylighting reference points, 2
	int const MaxMapRefPoints( 2500 ); // Maximum number of Illuminance Map Ref Points

	int const NotInOrAdjZoneExtWin( 0 ); // Exterior window is not in a Daylighting:Detailed zone
	// or in an adjacent zone with a shared interior window
	int const InZoneExtWin( 1 ); // Exterior window is in a Daylighting:Detailed zone
	int const AdjZoneExtWin( 2 ); // Exterior window is in a zone adjacent to a Daylighting:
	// Detailed zone with which it shares an interior window

	int const CalledForRefPoint( 101 );
	int const CalledForMapPoint( 102 );

	// Parameters for "DaylightType"
	int const NoDaylighting( 0 );
	int const DetailedDaylighting( 1 );
	int const DElightDaylighting( 2 );
	Array1D_string const DaylightTypes( 2, { "Daylighting:Controls", "Daylighting:DELight:Controls" } );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	int TotIllumMaps( 0 );
	bool mapResultsToReport( false ); // used when only partial hour has "sun up"
	bool mapResultsReported( false ); // when no map results are ever reported this will still be false
	char MapColSep; // Character for separating map columns (tab, space, comma)

	bool DFSReportSizingDays( false );
	bool DFSReportAllShadowCalculationDays( false );

	// Object Data
	Array1D< ZoneDaylightCalc > ZoneDaylight;
	Array1D< IllumMapData > IllumMap;
	Array1D< MapCalcData > IllumMapCalc;

} // DataDaylighting

} // EnergyPlus
