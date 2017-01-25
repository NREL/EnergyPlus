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
#include <DataSurfaceColors.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataSurfaceColors {

	// Module containing the data dealing with the coloring of surfaces for
	// various outputs (such as DXF)

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   Aug 2007
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Contain the data for surface colors and user settings for DXF and possibly
	// other surface reporting.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const NumColors( 15 );
	int const ColorNo_Text( 1 );
	int const ColorNo_Wall( 2 );
	int const ColorNo_Window( 3 );
	int const ColorNo_GlassDoor( 4 );
	int const ColorNo_Door( 5 );
	int const ColorNo_Floor( 6 );
	int const ColorNo_Roof( 7 );
	int const ColorNo_ShdDetBldg( 8 );
	int const ColorNo_ShdDetFix( 9 );
	int const ColorNo_ShdAtt( 10 );
	int const ColorNo_PV( 11 );
	int const ColorNo_TDDDome( 12 );
	int const ColorNo_TDDDiffuser( 13 );
	int const ColorNo_DaylSensor1( 14 );
	int const ColorNo_DaylSensor2( 15 );

	Array1D_int const defaultcolorno( NumColors, { 3, 43, 143, 143, 45, 8, 15, 195, 9, 13, 174, 143, 143, 10, 5 } ); // text | wall | window | glassdoor | door | floor | roof | detached building shade (moves with building) | detached building fixed | attached building shading | PV | TDD:Dome | TDD:Diffuser | Daylight Sensor 1 | Daylight Sensor 2

	Array1D_string const colorkeys( NumColors, { "Text", "Walls", "Windows", "GlassDoors", "Doors", "Roofs", "Floors", "DetachedBuildingShades", "DetachedFixedShades", "AttachedBuildingShades", "Photovoltaics", "TubularDaylightDomes", "TubularDaylightDiffusers", "DaylightReferencePoint1", "DaylightReferencePoint2" } );

	Array1D_int const colorkeyptr( NumColors, { ColorNo_Text, ColorNo_Wall, ColorNo_Window, ColorNo_GlassDoor, ColorNo_Door, ColorNo_Floor, ColorNo_Roof, ColorNo_ShdDetBldg, ColorNo_ShdDetFix, ColorNo_ShdAtt, ColorNo_PV, ColorNo_TDDDome, ColorNo_TDDDiffuser, ColorNo_DaylSensor1, ColorNo_DaylSensor2 } );

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	Array1D_int DXFcolorno( NumColors, defaultcolorno );

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Functions

	bool
	MatchAndSetColorTextString(
		std::string const & String, // string to be matched
		int const SetValue, // value to be used for the color
		Optional_string_const ColorType // for now, must be DXF
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem; // case insensitive Find

		// Return value
		bool WasSet;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int found;

		WasSet = false;
		found = FindItem( String, colorkeys, NumColors );

		if ( found != 0 ) {
			if ( present( ColorType ) ) {
				if ( ColorType() == "DXF" ) {
					DXFcolorno( colorkeyptr( found ) ) = SetValue;
					WasSet = true;
				} else {
				}
			} else {
				DXFcolorno( colorkeyptr( found ) ) = SetValue;
				WasSet = true;
			}
		}

		return WasSet;

	}

	void
	SetUpSchemeColors(
		std::string const & SchemeName,
		Optional_string_const ColorType
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine resets the colorno array(s) with the entered scheme name as
		// required for reporting.

		// METHODOLOGY EMPLOYED:
		// This routine first sets the color arrays to default.  Then, attempts to find the
		// scheme name in the Input File.  If found, processes that scheme and sets colors.
		// Worst case: the colors remain as default.  Note -- this allocates and deallocates
		// the alphas and numerics required to process the Report:SurfaceColorScheme object.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectDefMaxArgs;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "OutputControl:SurfaceColorScheme" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int numNumbers;
		int numptr;
		int numargs;
		int status;
		Array1D_string cAlphas;
		Array1D_string cAlphaFields;
		Array1D_string cNumericFields;
		Array1D_bool lAlphaBlanks;
		Array1D_bool lNumericBlanks;
		Array1D< Real64 > rNumerics;

		DXFcolorno = defaultcolorno;
		// first see if there is a scheme name
		numptr = GetObjectItemNum( CurrentModuleObject, SchemeName );

		if ( numptr > 0 ) {

			GetObjectDefMaxArgs( CurrentModuleObject, numargs, NumAlphas, numNumbers );

			cAlphas.allocate( NumAlphas );
			cAlphaFields.allocate( NumAlphas );
			lAlphaBlanks.allocate( NumAlphas );
			rNumerics.allocate( numNumbers );
			cNumericFields.allocate( numNumbers );
			lNumericBlanks.allocate( numNumbers );

			cAlphas( {1,NumAlphas} ) = "";
			rNumerics( {1,numNumbers} ) = 0.0;

			GetObjectItem( CurrentModuleObject, numptr, cAlphas, NumAlphas, rNumerics, numNumbers, status, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			for ( numargs = 1; numargs <= numNumbers; ++numargs ) {
				numptr = rNumerics( numargs ); // set to integer
				if ( lNumericBlanks( numargs ) ) {
					if ( ! lAlphaBlanks( numargs + 1 ) ) {
						ShowWarningError( "SetUpSchemeColors: " + cAlphaFields( 1 ) + '=' + SchemeName + ", " + cAlphaFields( numargs + 1 ) + '=' + cAlphas( numargs + 1 ) + ", " + cNumericFields( numargs ) + " was blank.  Default color retained." );
					}
					continue;
				}
				if ( ! MatchAndSetColorTextString( cAlphas( numargs + 1 ), numptr, ColorType ) ) {
					ShowWarningError( "SetUpSchemeColors: " + cAlphaFields( 1 ) + '=' + SchemeName + ", " + cAlphaFields( numargs + 1 ) + '=' + cAlphas( numargs + 1 ) + ", is invalid.  No color set." );
				}
			}

			cAlphas.deallocate();
			cAlphaFields.deallocate();
			lAlphaBlanks.deallocate();
			rNumerics.deallocate();
			cNumericFields.deallocate();
			lNumericBlanks.deallocate();

		} else {
			ShowWarningError( "SetUpSchemeColors: Name=" + SchemeName + " not on input file. Default colors will be used." );
		}

	}

} // DataSurfaceColors

} // EnergyPlus
