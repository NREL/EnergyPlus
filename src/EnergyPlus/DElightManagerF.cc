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

// C++ Headers
#include <algorithm>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <DElightManagerF.hh>
#include <CommandLineInterface.hh>
#include <DataDaylighting.hh>
#include <DataDElight.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <OutputReportPredefined.hh>
#include <UtilityRoutines.hh>
#include <OutputProcessor.hh>

extern "C" {
#include <DElight/DElightManagerC.h>
}

namespace EnergyPlus {

namespace DElightManagerF {

	// MODULE INFORMATION
	//       AUTHOR         Robert J. Hitchcock
	//       DATE WRITTEN   August 2003
	//       MODIFIED       January 2004
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// Defines INTERFACE Statements for Fortran calls to the DElightManagerC.cpp module.
	// The DElightManager.cpp module in turn defines the C/C++ calls to the DElight DLL.
	// Also, contains subroutines for performing associated operations.

	// METHODOLOGY EMPLOYED:

	// C Language Implementation of DOE2.1d and Superlite 3.0
	// Daylighting Algorithms with new Complex Fenestration System
	// analysis algorithms.
	// The original DOE2 daylighting algorithms and implementation
	// in FORTRAN were developed by F.C. Winkelmann at the
	// Lawrence Berkeley National Laboratory.
	// The original Superlite algorithms and implementation in FORTRAN
	// were developed by Michael Modest and Jong-Jin Kim
	// under contract with Lawrence Berkeley National Laboratory.
	// REFERENCES:

	// "Daylighting Calculation in DOE-2," F.C.Winkelmann, LBL-11353, May 1983
	// "Daylighting Simulation in the DOE-2 Building Energy Analysis Program,"
	// F.C. Winkelmann and S. Selkowitz, Energy and Buildings 8(1985)271-286

	// USE STATEMENTS:
	using namespace DataPrecisionGlobals;
	using namespace DataDElight;

	void
	DElightInputGenerator()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   August 2003
		//       MODIFIED       February 2004 - Changes to accomodate mods in DElight IDD
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine creates a DElight input file from EnergyPlus processed input.

		// USE STATEMENTS:
		using namespace DataGlobals; // Gives access to too many things to keep track of
		using namespace DataHeatBalance; // Gives access to Building, Zone(izone)%var and Lights(ilights) data
		using namespace DataEnvironment; // Gives access to Site data
		using namespace DataSurfaces; // Gives access to Surface data
		using namespace DataStringGlobals; // Gives access to Program Path and Current Time/Date
		using namespace DataIPShortCuts; // Gives access to commonly dimensioned field names, etc for getinput
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using namespace DataDaylighting;
		using namespace OutputReportPredefined;
		using General::RoundSigDigits;
		using InternalHeatGains::GetDesignLightingLevelForZone;
		using InternalHeatGains::CheckLightsReplaceableMinMaxForZone;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const cModuleObjectDElight( "Daylighting:DELight:Controls" );
		static std::string const cModuleObjectCFS( "Daylighting:DELight:ComplexFenestration" );
		static std::string const cModuleObjectRefPt( "Daylighting:DELight:ReferencePoint" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int unit; // Unit number on which to write file
		int iNumDElightZones; // Counter for Thermal Zones with hosted Daylighting:DElight objects
		int iNumOpaqueSurfs; // Counter for opaque surfaces in each zone
		int iSurfaceFirst; // starting loop variable for surfaces
		int iNumWindows; // Counter for windows hosted in each surface
		int iconstruct; // Index for construction type of surfaces
		int iMatlLayer; // Index for the outside (i.e., 1st) Material Layer for a Construction
		int iNumRefPts; // Counter for reference points
		int iLtgCtrlType; // Integer converter for Lighting Control Type
		Real64 rExtVisRefl; // Exterior visible reflectance of a material
		Real64 rLightLevel; // installed lighting level for current zone
		Real64 CosBldgRelNorth; // Cosine of Building rotation
		Real64 SinBldgRelNorth; // Sine of Building rotation
		Real64 CosZoneRelNorth; // Cosine of Zone rotation
		Real64 SinZoneRelNorth; // Sine of Zone rotation
		Real64 Xb; // temp var for transformation calc
		Real64 Yb; // temp var for transformation calc
		Real64 rTotalZoneFraction; // Zone Fraction sum for all RefPts in each Zone
		Array1D< Real64 > RefPt_WCS_Coord( 3 );
		Array1D_string AlphaArrayDElight( 2 );
		Array1D< Real64 > RealNumArrayDElight( 6 );
		int IOSTAT;
		int NumAlphasDElight;
		int NumNumsDElight;
		Array1D_string AlphaArrayCFS( 4 );
		Array1D< Real64 > RealNumArrayCFS( 1 );
		int NumAlphasCFS;
		int NumNumsCFS;
		Array1D_string AlphaArrayRefPt( 2 );
		Array1D< Real64 > RealNumArrayRefPt( 5 );
		int NumAlphasRefPt;
		int NumNumsRefPt;
		Array1D_int iWndoConstIndexes( 100 );
		bool lWndoConstFound; // Flag for non-unique window const index
		std::string cNameWOBlanks; // Name without blanks
		bool ErrorsFound;
		int iHostedCFS;
		bool lWndoIsDoppelganger; // Flag for doppelganger window test
		int iDoppelganger;
		bool ldoTransform;
		Real64 roldAspectRatio;
		Real64 rnewAspectRatio;
		Real64 Xo;
		Real64 XnoRot;
		Real64 Xtrans;
		Real64 Yo;
		Real64 YnoRot;
		Real64 Ytrans;

		// Formats
		static gio::Fmt Format_901( "(\"Version EPlus : DElight input generated from EnergyPlus processed input \",A)" );
		static gio::Fmt Format_902( "(,/,\"Building_Name \",A,/,\"Site_Latitude  \",f12.4,/,\"Site_Longitude \",f12.4,/,\"Site_Altitude  \",f12.4,/,\"Bldg_Azimuth   \",f12.4,/,\"Site_Time_Zone \",f12.4,/,\"Atm_Moisture  0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07\",/,\"Atm_Turbidity 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12 0.12\")" );
		static gio::Fmt Format_903( "(,/,\"ZONES\",/,\"N_Zones \",I4)" );
		static gio::Fmt Format_904( "(,/,\"ZONE DATA\",/,\"Zone \",A,/,\"BldgSystem_Zone_Origin \",f12.4,f12.4,f12.4,/,\"Zone_Azimuth    \",f12.4,/,\"Zone_Multiplier \",I5,/,\"Zone_Floor_Area \",f12.4,/,\"Zone_Volume     \",f12.4,/,\"Zone_Installed_Lighting \",f12.4,/,\"Min_Input_Power    \",f12.4,/,\"Min_Light_Fraction \",f12.4,/,\"Light_Ctrl_Steps   \",I3,/,\"Light_Ctrl_Prob    \",f12.4,/,\"View_Azimuth  0.0\",/,\"Max_Grid_Node_Area \",f12.4)" );
		static gio::Fmt Format_905( "(,/,\"ZONE LIGHTING SCHEDULES\",/,\"N_Lt_Scheds 0\")" );
		static gio::Fmt Format_906( "(,/,\"ZONE SURFACES\",/,\"N_Surfaces \",I4)" );
		static gio::Fmt Format_907( "(,/,\"ZONE SURFACE DATA\",/,\"Surface \",A,/,\"WCS_Azimuth \",f12.4,/,\"WCS_Tilt    \",f12.4,/,\"Vis_Refl    \",f12.4,/,\"Ext_Refl    \",f12.4,/,\"Gnd_Refl     0.2\",/,\"N_WCS_Vertices \",I6)" );
		static gio::Fmt Format_908( "(\"Vertex \",f12.4,f12.4,f12.4)" );
		static gio::Fmt Format_909( "(,/,\"SURFACE WINDOWS\",/,\"N_Windows \",I6)" );
		static gio::Fmt Format_910( "(,/,\"SURFACE WINDOW DATA\",/,\"Window     \",A,/,\"Glass_Type \",I8,/,\"Shade_Flag   0\",/,\"Overhang_Fin_Depth    0.0 0.0 0.0\",/,\"Overhang_Fin_Distance 0.0 0.0 0.0\",/,\"N_WCS_Vertices \",I4)" );
		static gio::Fmt Format_911( "(,/,\"SURFACE CFS\",/,\"N_CFS \",I6)" );
		static gio::Fmt Format_915( "(,/,\"COMPLEX FENESTRATION DATA\",/,\"CFS_Name   \",A,/,\"CFS_Type   \",A,/,\"Fenestration_Rotation \",f12.4,/,\"N_WCS_Vertices \",I4)" );
		static gio::Fmt Format_912( "(,/,\"ZONE REFERENCE POINTS\",/,\"N_Ref_Pts \",I4)" );
		static gio::Fmt Format_913( "(,/,\"ZONE REFERENCE POINT DATA\",/,\"Reference_Point \",A,/,\"RefPt_WCS_Coords \",f12.4,f12.4,f12.4,/,\"Zone_Fraction \",f12.4,/,\"Light_Set_Pt \",f12.4,/,\"Light_Ctrl_Type \",I4)" );
		static gio::Fmt Format_914( "(,/,\"BUILDING SHADES\",/,\"N_BShades 0\")" );
		static gio::Fmt Format_920( "(,/,\"LIBRARY DATA\",/,\"GLASS TYPES\",/,\"N_Glass_Types \",I4)" );
		static gio::Fmt Format_921( "(,/,\"GLASS TYPE DATA\",/,\"Name \",I6,/,\"EPlusDiffuse_Transmittance   \",f12.4,/,\"EPlusDiffuse_Int_Reflectance \",f12.4,/,\"EPlus_Vis_Trans_Coeff_1 \",f17.9,/,\"EPlus_Vis_Trans_Coeff_2 \",f17.9,/,\"EPlus_Vis_Trans_Coeff_3 \",f17.9,/,\"EPlus_Vis_Trans_Coeff_4 \",f17.9,/,\"EPlus_Vis_Trans_Coeff_5 \",f17.9,/,\"EPlus_Vis_Trans_Coeff_6 \",f17.9)" );

		// Init the ErrorsFound flag
		ErrorsFound = false;

		CheckForGeometricTransform( ldoTransform, roldAspectRatio, rnewAspectRatio );

		// Init the counter for Thermal Zones with hosted Daylighting:DElight objects
		iNumDElightZones = 0;

		// Init the counter for Window Construction types for writing to Library Data section of DElight input file
		int iNumWndoConsts = 0;

		// Open a file for writing DElight input from EnergyPlus data
		unit = GetNewUnitNumber();

		// Hardwire file name to eplusout.delightin in the current working directory
		{
			IOFlags flags; flags.ACTION( "write" ); gio::open( unit, outputDelightInFileName, flags );
			if ( flags.err() ) {
				ShowFatalError( "DElightInputGenerator: Could not open file \""+ outputDelightInFileName + "\" for output (write)." );
			}
		}

		// Start of DElight input file
		gio::write( unit, Format_901 ) << CurrentDateTime;

		// Building Data Section retrieved from DataHeatBalance and DataEnvironment modules
		// Remove any blanks from the Building Name for ease of input to DElight
		cNameWOBlanks = ReplaceBlanksWithUnderscores( BuildingName );
		gio::write( unit, Format_902 ) << cNameWOBlanks << Latitude << Longitude << Elevation * M2FT << BuildingAzimuth << TimeZoneNumber;

		// Calc cos and sin of Building Relative North values for later use in transforming Reference Point coordinates
		CosBldgRelNorth = std::cos( - BuildingAzimuth * DegToRadians );
		SinBldgRelNorth = std::sin( - BuildingAzimuth * DegToRadians );

		// Get the set of Daylighting:DElight objects
		int const iNumDElightObjs = GetNumObjectsFound( cModuleObjectDElight ); // Counter for Daylighting:DElight objects

		// Loop through the Daylighting:DElight objects checking for a host Zone
		for ( int iDElight = 1; iDElight <= iNumDElightObjs; ++iDElight ) {

			// Get the data items for the current DElight object
			GetObjectItem( cModuleObjectDElight, iDElight, AlphaArrayDElight, NumAlphasDElight, RealNumArrayDElight, NumNumsDElight, IOSTAT );

			int const izone = FindItemInList( AlphaArrayDElight( 2 ), Zone );
			if ( izone == 0 ) {
				ShowSevereError( "DElightInputGenerator: Illegal Zone Name=" + AlphaArrayDElight( 2 ) );
				ShowContinueError( "..in Daylighting:DElight, User Supplied DElight Zone Name=" + AlphaArrayDElight( 1 ) );
				ErrorsFound = true;
			} else { // valid zone

				// Count the number of DElight Reference Point objects input for this Thermal Zone.
				iNumRefPts = 0;

				// Get the set of all Daylighting:DElight:Reference Point objects in the IDF
				int const iTotNumDElightRefPtObjs = GetNumObjectsFound( cModuleObjectRefPt );

				// Loop through the Daylighting:DElight:Reference Point objects checking for the current DElight Zone host
				rTotalZoneFraction = 0.0; // init Zone Fraction accumulator
				for ( int irefpt = 1; irefpt <= iTotNumDElightRefPtObjs; ++irefpt ) {

					// Get the data items for the current DElight Reference Point object
					GetObjectItem( cModuleObjectRefPt, irefpt, AlphaArrayRefPt, NumAlphasRefPt, RealNumArrayRefPt, NumNumsRefPt, IOSTAT );

					// Is this RefPt hosted by current DElight Zone?
					if ( AlphaArrayRefPt( 2 ) == AlphaArrayDElight( 1 ) ) {

						// Count this correctly hosted RefPt
						++iNumRefPts;

						// Sum Fractions of Zone controlled by RefPt
						rTotalZoneFraction += RealNumArrayRefPt( 4 );

					}

				}

				// Register Error if 0 DElight RefPts have been input for valid DElight object
				if ( iNumRefPts < 1 ) {
					ShowSevereError( "No Reference Points input for DElight Zone =" + AlphaArrayDElight( 1 ) );
					ErrorsFound = true;
				}

				// If this zone already assigned a daylighting type, error
				if ( ZoneDaylight( izone ).DaylightType != NoDaylighting ) {
					ShowSevereError( "GetDElightDaylighting: Attempted to apply DElight Daylighting to a Zone with Previous Daylighting" );
					ShowContinueError( "Error discovered in \"Daylighting:DElight\" for Zone=" + AlphaArrayDElight( 2 ) );
					ShowContinueError( "Previously applied Daylighting Type=" + DaylightTypes( ZoneDaylight( izone ).DaylightType ) );
					ErrorsFound = true;
				}
				// Init the DElight members of the ZoneDaylight structure for this Thermal Zone
				// ZoneDaylight(izone)%TotalDElightRefPts > 0 is the trigger for DElight calcs
				ZoneDaylight( izone ).TotalDElightRefPts = iNumRefPts;

				// ZoneDaylight(izone)%DaylightType is another trigger for DElight calcs
				ZoneDaylight( izone ).DaylightType = DElightDaylighting;

				// Register Warning if more than 100 DElight RefPts have been input for valid DElight object
				if ( iNumRefPts > 100 ) {
					// Restrict to 100 Ref Pt maximum
					ZoneDaylight( izone ).TotalDElightRefPts = 100;
					ShowWarningError( "Maximum of 100 Reference Points exceeded for DElight Zone =" + AlphaArrayDElight( 1 ) );
					ShowWarningError( "  Only first 100 Reference Points included in DElight analysis" );
				}
				ZoneDaylight( izone ).DaylRefPtAbsCoord.allocate( 3, ZoneDaylight( izone ).TotalDElightRefPts );
				ZoneDaylight( izone ).DaylRefPtAbsCoord = 0.0;

				// RJH 2008-03-07: Allocate and Init DaylIllumAtRefPt array for this DElight zone
				ZoneDaylight( izone ).DaylIllumAtRefPt.allocate( ZoneDaylight( izone ).TotalDElightRefPts );
				ZoneDaylight( izone ).DaylIllumAtRefPt = 0.0;
				// following not used in DElight but allocated for convenience
				ZoneDaylight( izone ).GlareIndexAtRefPt.allocate( ZoneDaylight( izone ).TotalDElightRefPts );
				ZoneDaylight( izone ).GlareIndexAtRefPt = 0.0;

				// Register Warning if total Zone Fraction for all DElight RefPts < 1.0
				if ( rTotalZoneFraction < 1.0 ) {
					ShowWarningError( "Total Electric Lighting Zone Fraction less than 1.0 for DElight Zone =" + AlphaArrayDElight( 1 ) );
				}

				// Register Error if total Zone Fraction for all DElight RefPts > 1.0
				if ( rTotalZoneFraction > 1.0 ) {
					ShowSevereError( "Total Electric Lighting Zone Fraction greater than 1.0 for DElight Zone =" + AlphaArrayDElight( 1 ) );
					ErrorsFound = true;
				}

				// Increment counter of Thermal Zones with valid hosted DElight object
				++iNumDElightZones;

			}
		}

		// Get the number of input Complex Fenestration objects for reference throughout this subroutine
		int const iNumDElightCFS = GetNumObjectsFound( cModuleObjectCFS );

		// Zone Data Section
		gio::write( unit, Format_903 ) << iNumDElightZones;

		// Loop through the Daylighting:DElight objects searching for a match to the current Zone
		for ( int iDElight = 1; iDElight <= iNumDElightObjs; ++iDElight ) {

			// Get the data items for the current DElight object
			GetObjectItem( cModuleObjectDElight, iDElight, AlphaArrayDElight, NumAlphasDElight, RealNumArrayDElight, NumNumsDElight, IOSTAT );

			int const izone = FindItemInList( AlphaArrayDElight( 2 ), Zone );
			if ( izone != 0 ) {

				rLightLevel = GetDesignLightingLevelForZone( izone );
				CheckLightsReplaceableMinMaxForZone( izone );

				// Write this Zone to the DElight input file
				// Remove any blanks from the Zone Name for ease of input to DElight
				cNameWOBlanks = ReplaceBlanksWithUnderscores( Zone( izone ).Name );
				gio::write( unit, Format_904 ) << cNameWOBlanks << Zone( izone ).OriginX * M2FT << Zone( izone ).OriginY * M2FT << Zone( izone ).OriginZ * M2FT << Zone( izone ).RelNorth << Zone( izone ).Multiplier * Zone( izone ).ListMultiplier << Zone( izone ).FloorArea * M22FT2 << Zone( izone ).Volume * M32FT3 << rLightLevel / ( Zone( izone ).FloorArea * M22FT2 + 0.00001 ) << RealNumArrayDElight( 2 ) << RealNumArrayDElight( 3 ) << int( RealNumArrayDElight( 4 ) ) << RealNumArrayDElight( 5 ) << RealNumArrayDElight( 6 ) * M22FT2;

				// Calc cos and sin of Zone Relative North values for later use in transforming Reference Point coordinates
				CosZoneRelNorth = std::cos( -Zone( izone ).RelNorth * DegToRadians );
				SinZoneRelNorth = std::sin( -Zone( izone ).RelNorth * DegToRadians );

				// Zone Lighting Schedule Data Section
				// NOTE: Schedules are not required since hourly values are retrieved from EnergyPlus as needed
				gio::write( unit, Format_905 );

				// Zone Surface Data Section
				// Count the number of opaque surfaces bounding the current zone
				iNumOpaqueSurfs = 0;
				iSurfaceFirst = Zone( izone ).SurfaceFirst;
				int const iSurfaceLast = Zone( izone ).SurfaceLast; // ending loop variable for surfaces

				for ( int isurf = iSurfaceFirst; isurf <= iSurfaceLast; ++isurf ) {
					if ( Surface( isurf ).Class == SurfaceClass_Wall ) ++iNumOpaqueSurfs;
					if ( Surface( isurf ).Class == SurfaceClass_Roof ) ++iNumOpaqueSurfs;
					if ( Surface( isurf ).Class == SurfaceClass_Floor ) ++iNumOpaqueSurfs;
				} // Zone Opaque Surface loop

				gio::write( unit, Format_906 ) << iNumOpaqueSurfs;

				// Write each opaque bounding Surface to the DElight input file
				for ( int isurf = iSurfaceFirst; isurf <= iSurfaceLast; ++isurf ) {

					// Only process "opaque bounding" surface types
					if ( ( Surface( isurf ).Class == SurfaceClass_Wall ) || ( Surface( isurf ).Class == SurfaceClass_Roof ) || ( Surface( isurf ).Class == SurfaceClass_Floor ) ) {

						// Get the Construction index for this Surface
						iconstruct = Surface( isurf ).Construction;

						// Is this Surface exposed to the exterior?
						if ( Surface( isurf ).ExtSolar ) {
							// Get the index for the outside (i.e., 1st) Material Layer for this Construction
							iMatlLayer = Construct( iconstruct ).LayerPoint( 1 );
							// Get the outside visible reflectance of this material layer
							// (since Construct(iconstruct)%ReflectVisDiffFront always appears to == 0.0)
							rExtVisRefl = 1.0 - Material( iMatlLayer ).AbsorpVisible;
						} else {
							rExtVisRefl = 0.0;
						}

						// Remove any blanks from the Surface Name for ease of input to DElight
						cNameWOBlanks = ReplaceBlanksWithUnderscores( Surface( isurf ).Name );
						gio::write( unit, Format_907 ) << cNameWOBlanks << Surface( isurf ).Azimuth << Surface( isurf ).Tilt << Construct( iconstruct ).ReflectVisDiffBack << rExtVisRefl << Surface( isurf ).Sides;

						// Write out the vertex coordinates for each vertex
						int const iNumVertices = Surface( isurf ).Sides; // Counter for surface vertices
						for ( int ivert = 1; ivert <= iNumVertices; ++ivert ) {
							gio::write( unit, Format_908 ) << Surface( isurf ).Vertex( ivert ).x * M2FT << Surface( isurf ).Vertex( ivert ).y * M2FT << Surface( isurf ).Vertex( ivert ).z * M2FT;
						}

						// Count each Window hosted by the current opaque bounding Surface
						iNumWindows = 0;
						for ( int iwndo = iSurfaceFirst; iwndo <= iSurfaceLast; ++iwndo ) {
							if ( Surface( iwndo ).Class == SurfaceClass_Window ) {
								if ( Surface( iwndo ).BaseSurfName == Surface( isurf ).Name ) {

									// Error if window has multiplier > 1 since this causes incorrect illuminance calc
									if ( Surface( iwndo ).Multiplier > 1.0 ) {
										ShowSevereError( "Multiplier > 1.0 for window " + Surface( iwndo ).Name + " not allowed since it is in a zone with DElight daylighting." );
										ErrorsFound = true;
									}

									// Error if window has a shading device (blind/shade/screen) since
									// DElight cannot perform dynamic shading device deployment
									if ( Surface( iwndo ).WindowShadingControlPtr > 0 ) {
										ShowSevereError( "Shading Device on window " + Surface( iwndo ).Name + " dynamic control is not supported in a zone with DElight daylighting." );
										ErrorsFound = true;
									}

									// Loop through all Doppelganger Surface Names to ignore these Windows
									lWndoIsDoppelganger = false;
									for ( int iCFS = 1; iCFS <= iNumDElightCFS; ++iCFS ) {

										// Get the data items for the current CFS object
										GetObjectItem( cModuleObjectCFS, iCFS, AlphaArrayCFS, NumAlphasCFS, RealNumArrayCFS, NumNumsCFS, IOSTAT );

										// Is the current Window Surface a Doppelganger?
										if ( Surface( iwndo ).Name == AlphaArrayCFS( 4 ) ) {
											// Ignore this Doppelganger Window
											lWndoIsDoppelganger = true;
										}

									} // CFS object loop A

									if ( ! lWndoIsDoppelganger ) {
										++iNumWindows;
									}

								} // Surface hosts Window test
							} // Window test
						} // Window loop

						gio::write( unit, Format_909 ) << iNumWindows;

						// If the current opaque bounding Surface hosts Windows,
						// then write each hosted Window to the DElight input file
						// and track the Window Construction type for later writing
						if ( iNumWindows > 0 ) {
							for ( int iwndo2 = iSurfaceFirst; iwndo2 <= iSurfaceLast; ++iwndo2 ) {
								if ( Surface( iwndo2 ).Class == SurfaceClass_Window ) {
									if ( Surface( iwndo2 ).BaseSurfName == Surface( isurf ).Name ) {

										// Loop through all Doppelganger Surface Names to ignore these Windows
										lWndoIsDoppelganger = false;
										for ( int iCFS = 1; iCFS <= iNumDElightCFS; ++iCFS ) {

											// Get the data items for the current CFS object
											GetObjectItem( cModuleObjectCFS, iCFS, AlphaArrayCFS, NumAlphasCFS, RealNumArrayCFS, NumNumsCFS, IOSTAT );

											// Is the current Window Surface a Doppelganger?
											if ( Surface( iwndo2 ).Name == AlphaArrayCFS( 4 ) ) {
												// Ignore this Doppelganger Window
												lWndoIsDoppelganger = true;
											}

										} // CFS object loop A

										if ( ! lWndoIsDoppelganger ) {

											// Track unique window construction types here for later writing to
											// the library section of DElight input file

											// Get the Construction index for this Window Surface
											iconstruct = Surface( iwndo2 ).Construction;

											// Has the current Construction index been encountered before?
											lWndoConstFound = false;
											for ( int iconst = 1; iconst <= iNumWndoConsts; ++iconst ) {
												if ( iconstruct == iWndoConstIndexes( iconst ) ) lWndoConstFound = true;
											}
											if ( ! lWndoConstFound ) {
												++iNumWndoConsts;
												iWndoConstIndexes( iNumWndoConsts ) = iconstruct;
											}

											// Write this Window to the DElight input file
											// Remove any blanks from the Window Surface Name for ease of input to DElight
											cNameWOBlanks = ReplaceBlanksWithUnderscores( Surface( iwndo2 ).Name );
											gio::write( unit, Format_910 ) << cNameWOBlanks << iconstruct + 10000 << Surface( iwndo2 ).Sides;
											// Use WndoConstIndex + 10000 as the Glass Type Name
											// to differentiate EPlus glass types within DElight

											// Write out the vertex coordinates for each vertex
											int const iNumVertices = Surface( iwndo2 ).Sides; // Counter for surface vertices
											for ( int ivert = 1; ivert <= iNumVertices; ++ivert ) {
												gio::write( unit, Format_908 ) << Surface( iwndo2 ).Vertex( ivert ).x * M2FT << Surface( iwndo2 ).Vertex( ivert ).y * M2FT << Surface( iwndo2 ).Vertex( ivert ).z * M2FT;
											}
										} //.NOT.lWndoIsDoppelganger

									} // Surface hosts Window2 test
								} // Window2 Class test
							} // Window2 loop
						} // Hosted Windows test

						// Write the number of CFS hosted by the current Opaque Bounding Surface
						iHostedCFS = 0;

						// Loop through the input CFS objects searching for a match to the current Opaque Bounding Surface
						for ( int iCFS = 1; iCFS <= iNumDElightCFS; ++iCFS ) {

							// Get the data items for the current CFS object
							GetObjectItem( cModuleObjectCFS, iCFS, AlphaArrayCFS, NumAlphasCFS, RealNumArrayCFS, NumNumsCFS, IOSTAT );

							// Does the current Opaque Bounding Surface host the current CFS object?
							if ( Surface( isurf ).Name == AlphaArrayCFS( 3 ) ) {
								// Count this hosted CFS
								++iHostedCFS;
							}

						} // CFS object loop 1

						gio::write( unit, Format_911 ) << iHostedCFS;

						// Now write each of the hosted CFS data
						// Loop through the input CFS objects searching for a match to the current Opaque Bounding Surface
						for ( int iCFS = 1; iCFS <= iNumDElightCFS; ++iCFS ) {

							// Get the data items for the current CFS object
							GetObjectItem( cModuleObjectCFS, iCFS, AlphaArrayCFS, NumAlphasCFS, RealNumArrayCFS, NumNumsCFS, IOSTAT );

							// Does the current Opaque Bounding Surface host the current CFS object?
							if ( Surface( isurf ).Name == AlphaArrayCFS( 3 ) ) {

								// Get the Doppelganger surface for this CFS
								iDoppelganger = 0;
								for ( int iwndo3 = iSurfaceFirst; iwndo3 <= iSurfaceLast; ++iwndo3 ) {
									if ( Surface( iwndo3 ).Class == SurfaceClass_Window ) {

										// Is the current Window Surface the Doppelganger for the current CFS?
										if ( Surface( iwndo3 ).Name == AlphaArrayCFS( 4 ) ) {
											// Store the window surface index for future reference
											iDoppelganger = iwndo3;
										}
									}
								}

								// Make sure that a valid Doppelganger surface exists
								if ( iDoppelganger > 0 ) {

									// Write the data for this hosted CFS

									// Remove any blanks from the CFS Name for ease of input to DElight
									cNameWOBlanks = ReplaceBlanksWithUnderscores( AlphaArrayCFS( 1 ) );
									int const iNumVertices = Surface( iDoppelganger ).Sides; // Counter for surface vertices
									gio::write( unit, Format_915 ) << cNameWOBlanks << AlphaArrayCFS( 2 ) << RealNumArrayCFS( 1 ) << iNumVertices;

									// Write out the vertex coordinates for each vertex
									for ( int ivert = 1; ivert <= iNumVertices; ++ivert ) {
										gio::write( unit, Format_908 ) << Surface( iDoppelganger ).Vertex( ivert ).x * M2FT << Surface( iDoppelganger ).Vertex( ivert ).y * M2FT << Surface( iDoppelganger ).Vertex( ivert ).z * M2FT;
									}
								}
								// Register Error if there is no valid Doppelganger for current Complex Fenestration
								if ( iDoppelganger == 0 ) {
									ShowSevereError( "No Doppelganger Window Surface found for Complex Fenestration =" + AlphaArrayCFS( 1 ) );
									ErrorsFound = true;
								}

							} // The current Opaque Bounding Surface hosts the current CFS object?

						} // CFS object loop 2

					} // Opaque Bounding Surface test

				} // Zone Surface loop

				// Write ZONE REFERENCE POINTS
				gio::write( unit, Format_912 ) << ZoneDaylight( izone ).TotalDElightRefPts;

				// Keep an incremental count number of valid DElight Reference Points have been input for this DElight Zone
				iNumRefPts = 0;

				// Get the set of all Daylighting:DElight:Reference Point objects
				int const iTotNumDElightRefPtObjs = GetNumObjectsFound( cModuleObjectRefPt );

				// Loop through the Daylighting:DElight:Reference Point objects checking for the current DElight Zone host
				for ( int irefpt = 1; irefpt <= iTotNumDElightRefPtObjs; ++irefpt ) {

					// Get the data items for the current DElight Reference Point object
					GetObjectItem( cModuleObjectRefPt, irefpt, AlphaArrayRefPt, NumAlphasRefPt, RealNumArrayRefPt, NumNumsRefPt, IOSTAT );

					// Is this RefPt hosted by current DElight Zone?
					if ( AlphaArrayRefPt( 2 ) == AlphaArrayDElight( 1 ) ) {

						// Count this correctly hosted RefPt
						++iNumRefPts;

						// Limit to maximum of 100 RefPts
						if ( iNumRefPts <= 100 ) {

							if ( DaylRefWorldCoordSystem ) {
								RefPt_WCS_Coord( 1 ) = RealNumArrayRefPt( 1 );
								RefPt_WCS_Coord( 2 ) = RealNumArrayRefPt( 2 );
								RefPt_WCS_Coord( 3 ) = RealNumArrayRefPt( 3 );
							} else {
								//Transform reference point coordinates into building coordinate system
								Xb = RealNumArrayRefPt( 1 ) * CosZoneRelNorth - RealNumArrayRefPt( 2 ) * SinZoneRelNorth + Zone( izone ).OriginX;
								Yb = RealNumArrayRefPt( 1 ) * SinZoneRelNorth + RealNumArrayRefPt( 2 ) * CosZoneRelNorth + Zone( izone ).OriginY;
								//Transform into World Coordinate System
								RefPt_WCS_Coord( 1 ) = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
								RefPt_WCS_Coord( 2 ) = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
								RefPt_WCS_Coord( 3 ) = RealNumArrayRefPt( 3 ) + Zone( izone ).OriginZ;
								if ( ldoTransform ) { // Geometry transform
									Xo = RefPt_WCS_Coord( 1 ); // world coordinates.... shifted by relative north angle...
									Yo = RefPt_WCS_Coord( 2 );
									// next derotate the building
									XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
									YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
									// translate
									Xtrans = XnoRot * std::sqrt( rnewAspectRatio / roldAspectRatio );
									Ytrans = YnoRot * std::sqrt( roldAspectRatio / rnewAspectRatio );
									// rerotate
									RefPt_WCS_Coord( 1 ) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;

									RefPt_WCS_Coord( 2 ) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
								}
							}
							ZoneDaylight( izone ).DaylRefPtAbsCoord( {1,3}, iNumRefPts ) = RefPt_WCS_Coord( {1,3} );

							// Validate that Reference Point coordinates are within the host Zone
							if ( RefPt_WCS_Coord( 1 ) < Zone( izone ).MinimumX || RefPt_WCS_Coord( 1 ) > Zone( izone ).MaximumX ) {
								ShowWarningError( "DElightInputGenerator:Reference point X Value outside Zone Min/Max X, Zone=" + Zone( izone ).Name );
								ShowSevereError( "...X Reference Point= " + RoundSigDigits( RefPt_WCS_Coord( 1 ), 2 ) + ", Zone Minimum X= " + RoundSigDigits( Zone( izone ).MinimumX, 2 ) + ", Zone Maximum X= " + RoundSigDigits( Zone( izone ).MaximumX, 2 ) );
								ErrorsFound = true;
							}
							if ( RefPt_WCS_Coord( 2 ) < Zone( izone ).MinimumY || RefPt_WCS_Coord( 2 ) > Zone( izone ).MaximumY ) {
								ShowWarningError( "DElightInputGenerator:Reference point Y Value outside Zone Min/Max Y, Zone=" + Zone( izone ).Name );
								ShowSevereError( "...Y Reference Point= " + RoundSigDigits( RefPt_WCS_Coord( 2 ), 2 ) + ", Zone Minimum Y= " + RoundSigDigits( Zone( izone ).MinimumY, 2 ) + ", Zone Maximum Y= " + RoundSigDigits( Zone( izone ).MaximumY, 2 ) );
								ErrorsFound = true;
							}
							if ( RefPt_WCS_Coord( 3 ) < Zone( izone ).MinimumZ || RefPt_WCS_Coord( 3 ) > Zone( izone ).MaximumZ ) {
								ShowWarningError( "DElightInputGenerator:Reference point Z Value outside Zone Min/Max Z, Zone=" + Zone( izone ).Name );
								ShowSevereError( "...Z Reference Point= " + RoundSigDigits( RefPt_WCS_Coord( 3 ), 2 ) + ", Zone Minimum Z= " + RoundSigDigits( Zone( izone ).MinimumZ, 2 ) + ", Zone Maximum Z= " + RoundSigDigits( Zone( izone ).MaximumZ, 2 ) );
								ErrorsFound = true;
							}

							// Write this RefPt to the DElight input file

							// Remove any blanks from the RefPt Name for ease of input to DElight
							cNameWOBlanks = ReplaceBlanksWithUnderscores( AlphaArrayRefPt( 1 ) );
							iLtgCtrlType = RealNumArrayDElight( 1 );
							//                            write(unit,913) iNumRefPts, &
							gio::write( unit, Format_913 ) << cNameWOBlanks << RefPt_WCS_Coord( 1 ) * M2FT << RefPt_WCS_Coord( 2 ) * M2FT << RefPt_WCS_Coord( 3 ) * M2FT << RealNumArrayRefPt( 4 ) << RealNumArrayRefPt( 5 ) * LUX2FC << iLtgCtrlType;

							//                                        'Reference_Point ',I4,/, &

							// RJH 2008-03-07: Set up DaylIllumAtRefPt for output for this DElight zone RefPt

							// CurrentModuleObject='Daylighting:DELight:ReferencePoint'
							SetupOutputVariable( "Daylighting Reference Point Illuminance [lux]", ZoneDaylight( izone ).DaylIllumAtRefPt( iNumRefPts ), "Zone", "Average", AlphaArrayRefPt( 1 ) );

							// Predefined Reporting For Lighting Summary Report
							PreDefTableEntry( pdchDyLtZone, AlphaArrayRefPt( 1 ), AlphaArrayDElight( 2 ) );
							PreDefTableEntry( pdchDyLtKind, AlphaArrayRefPt( 1 ), "DElight" );
							// (1=continuous, 2=stepped, 3=continuous/off)
							{ auto const SELECT_CASE_var( int( RealNumArrayDElight( 1 ) ) );
							if ( SELECT_CASE_var == 1 ) {
								PreDefTableEntry( pdchDyLtCtrl, AlphaArrayRefPt( 1 ), "Continuous" );
							} else if ( SELECT_CASE_var == 2 ) {
								PreDefTableEntry( pdchDyLtCtrl, AlphaArrayRefPt( 1 ), "Stepped" );
							} else if ( SELECT_CASE_var == 3 ) {
								PreDefTableEntry( pdchDyLtCtrl, AlphaArrayRefPt( 1 ), "Continuous/Off" );
							}}
							PreDefTableEntry( pdchDyLtFrac, AlphaArrayRefPt( 1 ), RealNumArrayRefPt( 4 ) );
							PreDefTableEntry( pdchDyLtWInst, AlphaArrayRefPt( 1 ), rLightLevel );
							PreDefTableEntry( pdchDyLtWCtrl, AlphaArrayRefPt( 1 ), rLightLevel * RealNumArrayRefPt( 4 ) );

						} // Max 100 RefPt test

					} // RefPt in current DElight Zone test

				} // Ref Pt loop
			} // Zone hosts DElight object test
		} // Daylighting:DElight object loop

		// Write BUILDING SHADES
		gio::write( unit, Format_914 );

		// Write LIBRARY DATA
		gio::write( unit, Format_920 ) << iNumWndoConsts;

		// Write GLASS TYPES
		// VisBeamCoeffs are processed in EPlus by POLYF() function
		// Use WndoConstIndex + 10000 as the Glass Type Name to differentiate EPlus glass types within DElight
		for ( int iconst = 1; iconst <= iNumWndoConsts; ++iconst ) {
			gio::write( unit, Format_921 ) << iWndoConstIndexes( iconst ) + 10000 << Construct( iWndoConstIndexes( iconst ) ).TransDiffVis << Construct( iWndoConstIndexes( iconst ) ).ReflectVisDiffBack << Construct( iWndoConstIndexes( iconst ) ).TransVisBeamCoef( 1 ) << Construct( iWndoConstIndexes( iconst ) ).TransVisBeamCoef( 2 ) << Construct( iWndoConstIndexes( iconst ) ).TransVisBeamCoef( 3 ) << Construct( iWndoConstIndexes( iconst ) ).TransVisBeamCoef( 4 ) << Construct( iWndoConstIndexes( iconst ) ).TransVisBeamCoef( 5 ) << Construct( iWndoConstIndexes( iconst ) ).TransVisBeamCoef( 6 );

		} // Glass Type loop

		if ( ErrorsFound ) ShowFatalError( "Problems with Daylighting:DElight input, see previous error messages" );

		gio::close( unit );

		return;

	}

	void
	GenerateDElightDaylightCoefficients(
		Real64 & dLatitude,
		int & iErrorFlag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// The purpose of this subroutine is to provide an envelop to the DElightDaylightCoefficients routine

		delightdaylightcoefficients( dLatitude, &iErrorFlag );

	}

	void
	CheckForGeometricTransform(
		bool & doTransform,
		Real64 & OldAspectRatio,
		Real64 & NewAspectRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// check for geometrytransform in the daylighting access for reference points

		// METHODOLOGY EMPLOYED:
		// once reference points  have been converted to WCS,
		//  change them to reflect a different aspect
		// ratio for the entire building based on user input.

		// USE STATEMENTS:
		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace InputProcessor;
		using DataDaylighting::ZoneDaylight;
		using DataSurfaces::WorldCoordSystem;
		using DataSurfaces::AspectTransform;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "GeometryTransform" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string cAlphas( 1 );
		Array1D< Real64 > rNumerics( 2 );
		int NAlphas;
		int NNum;
		int IOStat;

		//begin execution
		//get user input...
		doTransform = false;
		OldAspectRatio = 1.0;
		NewAspectRatio = 1.0;

		if ( GetNumObjectsFound( CurrentModuleObject ) == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, cAlphas, NAlphas, rNumerics, NNum, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			OldAspectRatio = rNumerics( 1 );
			NewAspectRatio = rNumerics( 2 );
			if ( cAlphas( 1 ) != "XY" ) {
				ShowWarningError( CurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=" + cAlphas( 1 ) + "...ignored." );
			}
			doTransform = true;
			AspectTransform = true;
		}
		if ( WorldCoordSystem ) {
			doTransform = false;
			AspectTransform = false;
		}

	}

	void
	SetupDElightOutput4EPlus()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets up EnergyPlus output variables for DElight related variables.
		// RJH 2008-03-07: Only lighting power reduction factor is setup here
		// Reference Point Daylight Illuminance [lux] is setup within DElightInputGenerator

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// Using/Aliasing
		using namespace DataGlobals; // Gives access to NumOfZones
		using DataDaylighting::ZoneDaylight;
		using DataHeatBalance::Zone; // Gives access to Zone(izone)%var

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const var_name( "Daylighting Lighting Power Multiplier []" );
		static std::string const Zone_str( "Zone" );
		static std::string const Average_str( "Average" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int iZone = 1; iZone <= NumOfZones; ++iZone ) {
			if ( ZoneDaylight( iZone ).TotalDElightRefPts >= 1 ) {
				// Set up lighting power reduction factor for output for this DElight zone
				// CurrentModuleObject='Daylighting:DELight:Controls'
				SetupOutputVariable( var_name, ZoneDaylight( iZone ).ZonePowerReductionFactor, Zone_str, Average_str, Zone( iZone ).Name );
			}
		}

	}

	std::string
	ReplaceBlanksWithUnderscores( std::string const & InputString ) // Input String
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   August 2003
		//       MODIFIED       From MakeUPPERCase function by Linda K. Lawrie
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns a representation of the InputString with blanks replaced with underscores.

		// METHODOLOGY EMPLOYED:
		// Uses the std::replace function from the C++ library

		// REFERENCES:
		// na

		// Using/Aliasing

		// Return value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		std::string ResultString( trimmed( InputString ) );
		std::replace( ResultString.begin(), ResultString.end(), ' ', '_' );
		return ResultString;
	}

	void
	DElightElecLtgCtrl(
		int iNameLength,
		std::string cZoneName,
		Real64 dBldgLat,
		Real64 dHISKF,
		Real64 dHISUNF,
		Real64 dCloudFraction,
		Real64 dSOLCOSX,
		Real64 dSOLCOSY,
		Real64 dSOLCOSZ,
		Real64 & pdPowerReducFac,
		int piErrorFlag
	)
	{
		auto zoneNameArr( getCharArrayFromString( cZoneName ) );
		delightelecltgctrl( iNameLength, &zoneNameArr[0], dBldgLat, dHISKF, dHISUNF, dCloudFraction, dSOLCOSX, dSOLCOSY, dSOLCOSZ, &pdPowerReducFac, &piErrorFlag );
	}

	std::vector< char >
	getCharArrayFromString( std::string const & originalString )
	{
		std::vector< char > returnVal( originalString.begin(), originalString.end() );
		returnVal.push_back( '\0' ); // get null terminated string of chars
		return returnVal;
	}

	std::string
	getStringFromCharArray( std::vector< char > const & originalCharArray )
	{
		return std::string( originalCharArray.begin(), originalCharArray.end() );
	}

} // DELIGHTMANAGERF

} // EnergyPlus
