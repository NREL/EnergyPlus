// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

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

} // DataSurfaceColors

} // EnergyPlus
