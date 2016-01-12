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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <OutputReports.hh>
#include <DataDaylighting.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataSurfaceColors.hh>
#include <DataSurfaces.hh>
#include <DXFEarClipping.hh>
#include <General.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

static gio::Fmt fmtLD( "*" );

void
ReportSurfaces()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   February 1999
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine calls several optional routines to report
	// the surfaces to output formats that can render the data
	// into a descriptive picture.

	// METHODOLOGY EMPLOYED:
	// Use a REPORT command to determine if there should be
	// a file created.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataErrorTracking::AskForSurfacesReport;
	using namespace DataSurfaceColors;
	using General::ScanForReports;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int SurfDetails;
	bool SurfVert;
	bool SurfDet;
	bool DXFDone;
	bool VRMLDone;
	std::string Option1;
	std::string Option2;
	bool DoReport;

	AskForSurfacesReport = false;

	SurfDetails = 0;
	SurfVert = false;
	SurfDet = false;
	DXFDone = false;
	VRMLDone = false;
	Option1 = "";
	Option2 = "";

	ScanForReports( "Surfaces", DoReport, "Lines", Option1 );
	if ( DoReport ) LinesOut( Option1 );

	ScanForReports( "Surfaces", DoReport, "Vertices" );
	if ( DoReport ) {
		if ( ! SurfVert ) {
			++SurfDetails;
			SurfVert = true;
		}
	}

	ScanForReports( "Surfaces", DoReport, "Details" );
	if ( DoReport ) {
		if ( ! SurfDet ) {
			SurfDetails += 10;
			SurfDet = true;
		}
	}

	ScanForReports( "Surfaces", DoReport, "DetailsWithVertices" );
	if ( DoReport ) {
		if ( ! SurfDet ) {
			SurfDetails += 10;
			SurfDet = true;
		}
		if ( ! SurfVert ) {
			++SurfDetails;
			SurfVert = true;
		}
	}

	ScanForReports( "Surfaces", DoReport, "DXF", Option1, Option2 );
	if ( DoReport ) {
		if ( ! DXFDone ) {
			if ( Option2 != "" ) {
				SetUpSchemeColors( Option2, "DXF" );
			}
			DXFOut( Option1, Option2 );
			DXFDone = true;
		} else {
			ShowWarningError( "ReportSurfaces: DXF output already generated.  DXF with option=[" + Option1 + "] will not be generated." );
		}
	}

	ScanForReports( "Surfaces", DoReport, "DXF:WireFrame", Option1, Option2 );
	if ( DoReport ) {
		if ( ! DXFDone ) {
			if ( Option2 != "" ) {
				SetUpSchemeColors( Option2, "DXF" );
			}
			DXFOutWireFrame( Option2 );
			DXFDone = true;
		} else {
			ShowWarningError( "ReportSurfaces: DXF output already generated.  DXF:WireFrame will not be generated." );
		}
	}

	ScanForReports( "Surfaces", DoReport, "VRML", Option1, Option2 );
	if ( DoReport ) {
		if ( ! VRMLDone ) {
			VRMLOut( Option1, Option2 );
			VRMLDone = true;
		} else {
			ShowWarningError( "ReportSurfaces: VRML output already generated.  VRML with option=[" + Option1 + "] will not be generated." );
		}
	}

	ScanForReports( "Surfaces", DoReport, "CostInfo" );
	if ( DoReport ) {
		CostInfoOut();
	}

	if ( SurfDet || SurfVert ) {
		DetailsForSurfaces( SurfDetails );
	}

}

void
LinesOut( std::string const & option )
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   March 1999
	//       MODIFIED       March 2006 -- add option for "IDF segments out"
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine produces a file of lines in the surfaces.

	// METHODOLOGY EMPLOYED:
	// Use the surface absolute coordinate information to produce
	// lines.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using General::RoundSigDigits;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static gio::Fmt fmt700( "(5(f10.2,','),f10.2)" );
	static gio::Fmt fmtA( "(A)" );
	static gio::Fmt fmtcoord( "(2X,2(f10.2,','),f10.2,A,A)" );
	static std::string const vertexstring( "X,Y,Z ==> Vertex" );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static bool optiondone( false );
	static std::string lastoption;
	int unit; // Unit number on which to write file
	int surf; // Loop variable for surfaces
	int vert; // Loop counter
	std::string optcommasemi;
	int write_stat;

	if ( TotSurfaces > 0 && ! allocated( Surface ) ) {
		// no error needed, probably in end processing, just return
		return;
	}

	if ( optiondone ) {
		ShowWarningError( "Report of Surfaces/Lines Option has already been completed with option=" + lastoption );
		ShowContinueError( "..option=\"" + option + "\" will not be done this time." );
		return;
	}

	lastoption = option;
	optiondone = true;

	unit = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( unit, DataStringGlobals::outputSlnFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		ShowFatalError( "LinesOut: Could not open file "+ DataStringGlobals::outputSlnFileName +" for output (write)." );
	}

	if ( option != "IDF" ) {
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
			if ( Surface( surf ).Sides == 0 ) continue;
			gio::write( unit, fmtA ) << Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				if ( vert != Surface( surf ).Sides ) {
					gio::write( unit, fmt700 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( vert + 1 ).x << Surface( surf ).Vertex( vert + 1 ).y << Surface( surf ).Vertex( vert + 1 ).z;
				} else {
					gio::write( unit, fmt700 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( 1 ).x << Surface( surf ).Vertex( 1 ).y << Surface( surf ).Vertex( 1 ).z;
				}
			}
		}
	} else {
		gio::write( unit, fmtA ) << " Building North Axis = 0";
		gio::write( unit, fmtA ) << "GlobalGeometryRules,UpperLeftCorner,CounterClockwise,WorldCoordinates;";
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
			if ( Surface( surf ).Sides == 0 ) continue;
			// process heat transfer surfaces
			gio::write( unit, fmtA ) << " Surface=" + cSurfaceClass( Surface( surf ).Class ) + ", Name=" + Surface( surf ).Name + ", Azimuth=" + RoundSigDigits( Surface( surf ).Azimuth, 1 );
			gio::write( unit, fmtA ) << "  " + RoundSigDigits( Surface( surf ).Sides ) + ",  !- Number of (X,Y,Z) groups in this surface";
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				optcommasemi = ",";
				if ( vert == Surface( surf ).Sides ) optcommasemi = ";";
				gio::write( unit, fmtcoord ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << optcommasemi << "  !- " + vertexstring + ' ' + RoundSigDigits( vert );
			}
		}
	}

	gio::close( unit );

}

void
DXFOut(
	std::string & PolygonAction,
	std::string & ColorScheme // Name from user for color scheme or blank
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   March 1999
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine produces a file of DXF objects for the surfaces.

	// METHODOLOGY EMPLOYED:
	// Use the surface absolute coordinate information to produce
	// lines.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataHeatBalance::BuildingName;
	using DataHeatBalance::Zone;
	using namespace DataSurfaces;
	using namespace DataSurfaceColors;
	using DataDaylighting::ZoneDaylight;
	using DataDaylighting::TotIllumMaps;
	using DataDaylighting::IllumMapCalc;
	using DataGlobals::NumOfZones;
	using DataStringGlobals::VerString;
	using namespace DXFEarClipping;
	using General::TrimSigDigits;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static Array1D< Real64 > StemX( 4, -10.0 );
	static Array1D< Real64 > StemY( 4, { 3.0, 3.0, 0.0, 0.0 } );
	static Array1D< Real64 > StemZ( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > Head1X( 4, { -10.0, -10.0, -10.5, -10.5 } );
	static Array1D< Real64 > Head1Y( 4, { 3.0, 3.0, 2.133975, 2.133975 } );
	static Array1D< Real64 > Head1Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > Head2X( 4, { -10.0, -10.0, -9.5, -9.5 } );
	static Array1D< Real64 > Head2Y( 4, { 3.0, 3.0, 2.133975, 2.133975 } );
	static Array1D< Real64 > Head2Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide1X( 4, -10.5 );
	static Array1D< Real64 > NSide1Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide1Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide2X( 4, { -10.5, -10.5, -9.5, -9.5 } );
	static Array1D< Real64 > NSide2Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide2Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide3X( 4, -9.5 );
	static Array1D< Real64 > NSide3Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide3Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	//  integer, dimension(7) :: colorno=(/3,4,5,6,2,8,9/)
	int unit; // Unit number on which to write file
	int surf; // Loop variable for surfaces
	int vert; // Loop counter
	int colorindex; // color index by surface type
	Real64 minx; // minimum x in surface data
	Real64 miny; // minimum y in surface data
	Real64 minz; // minimum z in surface data (for polygon output)
	int zones; // loop counter for zone loop
	std::string ZoneNum;
	std::string TempZoneName;
	std::string::size_type pos;
	std::string ShadeType;
	static bool ThickPolyline( false );
	static bool RegularPolyline( false );
	static std::string PolylineWidth( " 0.55" );
	static bool TriangulateFace( false );
	int ntri;
	int svert;
	int vv0;
	int vv1;
	int vv2;
	int refpt; // for daylighting ref points
	int curcolorno; // again for daylighting ref pts
	int write_stat;
	int mapnum;

	// Object Data
	Array1D< dTriangle > mytriangles;

	// Formats
	static gio::Fmt Format_702( "('  0',/,'SECTION',/,'  2',/,'ENTITIES')" );
	static gio::Fmt Format_707( "('999',/,'DXF created from EnergyPlus')" );
	static gio::Fmt Format_708( "('999',/,A,A,A)" );
	static gio::Fmt Format_800( "('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,' .25',/,'  1',/,'True North',/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,'210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')" );
	static gio::Fmt Format_801( "('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,' .4',/,'  1',/,A,/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,'210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')" );
	static gio::Fmt Format_703_0( "('  0',/,'3DFACE',/,'  8',/,'1',/,' 62',/,I3)" );
	static gio::Fmt Format_703_1( "(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)" );
	static gio::Fmt Format_703_2( "(' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)" );
	static gio::Fmt Format_703_3( "(' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)" );
	static gio::Fmt Format_703_4( "(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)" );
	static gio::Fmt Format_715( "('  0',/,'POLYLINE',/,'  8',/,A,/,' 62',/,I3,/,' 66',/,'  1',/,' 10',/,' 0.0',/,' 20',/,' 0.0',/,' 30',/,f15.5,/,' 70',/,'   9',/,' 40',/,A,/,' 41',/,A)" );
	static gio::Fmt Format_716( "('  0',/,'VERTEX',/,'  8',/,A,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)" );
	static gio::Fmt Format_717( "('  0',/,'SEQEND',/,'  8',/,A)" );
	static gio::Fmt Format_704( "('  0',/,'3DFACE',/,'  8',/,A,/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)" );
	static gio::Fmt Format_704_0("('  0',/,'3DFACE',/,'  8',/,A,/,' 62',/,I3)" );
	static gio::Fmt Format_704_1("(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)" );
	static gio::Fmt Format_704_2("(' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)" );
	static gio::Fmt Format_704_3("(' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)" );
	static gio::Fmt Format_705( "(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)" );
	static gio::Fmt Format_706( "('  0',/,'ENDSEC',/,'  0',/,'EOF')" );
	static gio::Fmt Format_709( "('  0',/,'CIRCLE',/,'  8',/,A,/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,f15.5)" );
	static gio::Fmt Format_710( "('999',/,A)" );

	if ( PolygonAction == "TRIANGULATE3DFACE" || PolygonAction == "TRIANGULATE" || PolygonAction == "" ) {
		TriangulateFace = true;
		RegularPolyline = false;
		ThickPolyline = false;
	} else if ( PolygonAction == "THICKPOLYLINE" ) {
		ThickPolyline = true;
		RegularPolyline = false;
		TriangulateFace = false;
	} else if ( PolygonAction == "REGULARPOLYLINE" ) {
		RegularPolyline = true;
		TriangulateFace = false;
		ThickPolyline = false;
		PolylineWidth = " 0";
	} else {
		ShowWarningError( "DXFOut: Illegal key specified for Surfaces with > 4 sides=" + PolygonAction );
		ShowContinueError( "...Valid keys are: \"ThickPolyline\", \"RegularPolyline\", \"Triangulate3DFace\"." );
		ShowContinueError( "\"Triangulate3DFace\" will be used for any surfaces with > 4 sides." );
		TriangulateFace = true;
		RegularPolyline = false;
		ThickPolyline = false;
	}

	if ( TotSurfaces > 0 && ! allocated( Surface ) ) {
		// no error needed, probably in end processing, just return
		return;
	}

	unit = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( unit, DataStringGlobals::outputDxfFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		ShowFatalError( "DXFOut: Could not open file "+DataStringGlobals::outputDxfFileName+" for output (write)." );
	}

	gio::write( unit, Format_702 ); // Start of Entities section

	gio::write( unit, Format_707 ); // Comment

	gio::write( unit, Format_708 ) << "Program Version" << "," << VerString;

	if ( PolygonAction == "" ) {
		gio::write( unit, Format_708 ) << "Polygon Action" << "," << "ThickPolyline";
	} else {
		gio::write( unit, Format_708 ) << "Polygon Action" << "," << PolygonAction;
	}

	if ( ColorScheme == "" ) {
		gio::write( unit, Format_708 ) << "Color Scheme" << "," << "Default";
	} else {
		gio::write( unit, Format_708 ) << "Color Scheme" << "," << ColorScheme;
	}

	minx = 99999.0;
	miny = 99999.0;
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
		for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
			minx = min( minx, Surface( surf ).Vertex( vert ).x );
			miny = min( miny, Surface( surf ).Vertex( vert ).y );
		}
	}

	for ( vert = 1; vert <= 4; ++vert ) {
		StemX( vert ) += minx;
		StemY( vert ) += miny;
		Head1X( vert ) += minx;
		Head1Y( vert ) += miny;
		Head2X( vert ) += minx;
		Head2Y( vert ) += miny;
		NSide1X( vert ) += minx;
		NSide1Y( vert ) += miny;
		NSide2X( vert ) += minx;
		NSide2Y( vert ) += miny;
		NSide3X( vert ) += minx;
		NSide3Y( vert ) += miny;
	}

	// This writes "True North" above the Arrow Head
	gio::write( unit, Format_710 ) << "Text - True North";
	gio::write( unit, Format_800 ) << DXFcolorno( ColorNo_Text ) << StemX( 1 ) - 1.0 << StemY( 1 ) << StemZ( 1 );

	gio::write( unit, Format_710 ) << "Text - Building Title";
	gio::write( unit, Format_801 ) << DXFcolorno( ColorNo_Text ) << StemX( 1 ) - 4.0 << StemY( 1 ) - 4.0 << StemZ( 1 ) << "Building - " + BuildingName;

	// We want to point the north arrow to true north
	gio::write( unit, Format_710 ) << "North Arrow Stem";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << StemX( 1 ) << StemY( 1 ) << StemZ( 1 );
	gio::write( unit, Format_703_2 ) << StemX( 2 ) << StemY( 2 ) << StemZ( 2 );
	gio::write( unit, Format_703_3 ) << StemX( 3 ) << StemY( 3 ) << StemZ( 3 );
	gio::write( unit, Format_703_4 ) << StemX( 4 ) << StemY( 4 ) << StemZ( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Head 1";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << Head1X( 1 ) << Head1Y( 1 ) << Head1Z( 1 );
	gio::write( unit, Format_703_2 ) << Head1X( 2 ) << Head1Y( 2 ) << Head1Z( 2 );
	gio::write( unit, Format_703_3 ) << Head1X( 3 ) << Head1Y( 3 ) << Head1Z( 3 );
	gio::write( unit, Format_703_4 ) << Head1X( 4 ) << Head1Y( 4 ) << Head1Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Head 2";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << Head2X( 1 ) << Head2Y( 1 ) << Head2Z( 1 );
	gio::write( unit, Format_703_2 ) << Head2X( 2 ) << Head2Y( 2 ) << Head2Z( 2 );
	gio::write( unit, Format_703_3 ) << Head2X( 3 ) << Head2Y( 3 ) << Head2Z( 3 );
	gio::write( unit, Format_703_4 ) << Head2X( 4 ) << Head2Y( 4 ) << Head2Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 1";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide1X( 1 ) << NSide1Y( 1 ) << NSide1Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide1X( 2 ) << NSide1Y( 2 ) << NSide1Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide1X( 3 ) << NSide1Y( 3 ) << NSide1Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide1X( 4 ) << NSide1Y( 4 ) << NSide1Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 2";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide2X( 1 ) << NSide2Y( 1 ) << NSide2Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide2X( 2 ) << NSide2Y( 2 ) << NSide2Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide2X( 3 ) << NSide2Y( 3 ) << NSide2Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide2X( 4 ) << NSide2Y( 4 ) << NSide2Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 3";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide3X( 1 ) << NSide3Y( 1 ) << NSide3Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide3X( 2 ) << NSide3Y( 2 ) << NSide3Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide3X( 3 ) << NSide3Y( 3 ) << NSide3Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide3X( 4 ) << NSide3Y( 4 ) << NSide3Z( 4 );

	gio::write( unit, Format_710 ) << "Zone Names";
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		gio::write( ZoneNum, fmtLD ) << zones;
		strip( ZoneNum );
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		gio::write( unit, Format_710 ) << "Zone=" + ZoneNum + ':' + TempZoneName;
	}

	colorindex = ColorNo_ShdDetFix;
	//  Do all detached shading surfaces first
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).HeatTransSurf ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Shading ) continue;
		if ( Surface( surf ).Sides == 0 ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) colorindex = ColorNo_ShdDetFix;
		if ( Surface( surf ).Class == SurfaceClass_Detached_B ) colorindex = ColorNo_ShdDetBldg;
		if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) {
			ShadeType = "Fixed Shading";
			gio::write( unit, Format_710 ) << "Fixed Shading:" + Surface( surf ).Name;
		} else if ( Surface( surf ).Class == SurfaceClass_Detached_B ) {
			ShadeType = "Building Shading";
			gio::write( unit, Format_710 ) << "Building Shading:" + Surface( surf ).Name;
		}
		if ( Surface( surf ).Sides <= 4 ) {
			gio::write( unit, Format_704_0 )
				<< ShadeType << DXFcolorno( colorindex );
			gio::write( unit, Format_704_1 )
				<< Surface( surf ).Vertex( 1 ).x
				<< Surface( surf ).Vertex( 1 ).y
				<< Surface( surf ).Vertex( 1 ).z;
			gio::write( unit, Format_704_2 )
				<< Surface( surf ).Vertex( 2 ).x
				<< Surface( surf ).Vertex( 2 ).y
				<< Surface( surf ).Vertex( 2 ).z;
			gio::write( unit, Format_704_3 )
				<< Surface( surf ).Vertex( 3 ).x
				<< Surface( surf ).Vertex( 3 ).y
				<< Surface( surf ).Vertex( 3 ).z;
			if ( Surface( surf ).Sides == 3 ) {
				gio::write( unit, Format_705 ) << Surface( surf ).Vertex( 3 ).x << Surface( surf ).Vertex( 3 ).y << Surface( surf ).Vertex( 3 ).z;
			} else {
				gio::write( unit, Format_705 ) << Surface( surf ).Vertex( 4 ).x << Surface( surf ).Vertex( 4 ).y << Surface( surf ).Vertex( 4 ).z;
			}
		} else { // polygon
			if ( ! TriangulateFace ) {
				minz = 99999.0;
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					minz = min( minz, Surface( surf ).Vertex( vert ).z );
				}
				gio::write( unit, Format_715 ) << ShadeType << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					gio::write( unit, Format_716 ) << ShadeType << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
				}
				gio::write( unit, Format_717 ) << ShadeType;
			} else {
				ntri = Triangulate( Surface( surf ).Sides, Surface( surf ).Vertex, mytriangles, Surface( surf ).Azimuth, Surface( surf ).Tilt, Surface( surf ).Name, Surface( surf ).Class );
				for ( svert = 1; svert <= ntri; ++svert ) {
					vv0 = mytriangles( svert ).vv0;
					vv1 = mytriangles( svert ).vv1;
					vv2 = mytriangles( svert ).vv2;
					gio::write( unit, Format_704 ) << ShadeType << DXFcolorno( colorindex ) << Surface( surf ).Vertex( vv0 ).x << Surface( surf ).Vertex( vv0 ).y << Surface( surf ).Vertex( vv0 ).z << Surface( surf ).Vertex( vv1 ).x << Surface( surf ).Vertex( vv1 ).y << Surface( surf ).Vertex( vv1 ).z << Surface( surf ).Vertex( vv2 ).x << Surface( surf ).Vertex( vv2 ).y << Surface( surf ).Vertex( vv2 ).z;
					gio::write( unit, Format_705 ) << Surface( surf ).Vertex( vv2 ).x << Surface( surf ).Vertex( vv2 ).y << Surface( surf ).Vertex( vv2 ).z;
				}
				mytriangles.deallocate();
			}
		}
	}

	// now do zone surfaces, by zone
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( surf = max( Zone( zones ).SurfaceFirst, 1 ); surf <= Zone( zones ).SurfaceLast; ++surf ) {
			if ( Surface( surf ).Sides == 0 ) continue;
			if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
			if ( Surface( surf ).Class == SurfaceClass_Wall ) colorindex = ColorNo_Wall;
			if ( Surface( surf ).Class == SurfaceClass_Roof ) colorindex = ColorNo_Roof;
			if ( Surface( surf ).Class == SurfaceClass_Floor ) colorindex = ColorNo_Floor;
			if ( Surface( surf ).Class == SurfaceClass_Door ) colorindex = ColorNo_Door;
			if ( Surface( surf ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_Window ) colorindex = ColorNo_Window;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_GlassDoor ) colorindex = ColorNo_GlassDoor;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_TDD_Dome ) colorindex = ColorNo_TDDDome;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_TDD_Diffuser ) colorindex = ColorNo_TDDDiffuser;
			}
			if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;

			gio::write( unit, Format_710 ) << Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			if ( Surface( surf ).Sides <= 4 ) {
				gio::write( unit, Format_704_0 )
					<< TempZoneName << DXFcolorno( colorindex );
				gio::write( unit, Format_704_1 )
					<< Surface( surf ).Vertex( 1 ).x
					<< Surface( surf ).Vertex( 1 ).y
					<< Surface( surf ).Vertex( 1 ).z;
				gio::write( unit, Format_704_2 )
					<< Surface( surf ).Vertex( 2 ).x
					<< Surface( surf ).Vertex( 2 ).y
					<< Surface( surf ).Vertex( 2 ).z;
				gio::write( unit, Format_704_3 )
					<< Surface( surf ).Vertex( 3 ).x
					<< Surface( surf ).Vertex( 3 ).y
					<< Surface( surf ).Vertex( 3 ).z;
				if ( Surface( surf ).Sides == 3 ) {
					gio::write( unit, Format_705 ) << Surface( surf ).Vertex( 3 ).x << Surface( surf ).Vertex( 3 ).y << Surface( surf ).Vertex( 3 ).z;
				} else {
					gio::write( unit, Format_705 ) << Surface( surf ).Vertex( 4 ).x << Surface( surf ).Vertex( 4 ).y << Surface( surf ).Vertex( 4 ).z;
				}
			} else { // polygon surface
				if ( ! TriangulateFace ) {
					minz = 99999.0;
					for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
						minz = min( minz, Surface( surf ).Vertex( vert ).z );
					}
					gio::write( unit, Format_715 ) << TempZoneName << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
					for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
						gio::write( unit, Format_716 ) << TempZoneName << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
					}
					gio::write( unit, Format_717 ) << TempZoneName;
				} else {
					ntri = Triangulate( Surface( surf ).Sides, Surface( surf ).Vertex, mytriangles, Surface( surf ).Azimuth, Surface( surf ).Tilt, Surface( surf ).Name, Surface( surf ).Class );
					for ( svert = 1; svert <= ntri; ++svert ) {
						vv0 = mytriangles( svert ).vv0;
						vv1 = mytriangles( svert ).vv1;
						vv2 = mytriangles( svert ).vv2;
						gio::write( unit, Format_704 ) << TempZoneName << DXFcolorno( colorindex ) << Surface( surf ).Vertex( vv0 ).x << Surface( surf ).Vertex( vv0 ).y << Surface( surf ).Vertex( vv0 ).z << Surface( surf ).Vertex( vv1 ).x << Surface( surf ).Vertex( vv1 ).y << Surface( surf ).Vertex( vv1 ).z << Surface( surf ).Vertex( vv2 ).x << Surface( surf ).Vertex( vv2 ).y << Surface( surf ).Vertex( vv2 ).z;
						gio::write( unit, Format_705 ) << Surface( surf ).Vertex( vv2 ).x << Surface( surf ).Vertex( vv2 ).y << Surface( surf ).Vertex( vv2 ).z;
					}
					mytriangles.deallocate();
				}
			}

		}
		// still have to do shading surfaces for zone
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			//if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
			if ( Surface( surf ).Class != SurfaceClass_Shading ) continue;
			if ( Surface( surf ).ZoneName != Zone( zones ).Name ) continue;
			if ( Surface( surf ).Sides == 0 ) continue;
			colorindex = ColorNo_ShdAtt;
			if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
			gio::write( unit, Format_710 ) << Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			if ( Surface( surf ).Sides <= 4 ) {
				gio::write( unit, Format_704_0 )
					<< TempZoneName << DXFcolorno( colorindex );
				gio::write( unit, Format_704_1 )
					<< Surface( surf ).Vertex( 1 ).x
					<< Surface( surf ).Vertex( 1 ).y
					<< Surface( surf ).Vertex( 1 ).z;
				gio::write( unit, Format_704_2 )
					<< Surface( surf ).Vertex( 2 ).x
					<< Surface( surf ).Vertex( 2 ).y
					<< Surface( surf ).Vertex( 2 ).z;
				gio::write( unit, Format_704_3 )
					<< Surface( surf ).Vertex( 3 ).x
					<< Surface( surf ).Vertex( 3 ).y
					<< Surface( surf ).Vertex( 3 ).z;
				if ( Surface( surf ).Sides == 3 ) {
					gio::write( unit, Format_705 ) << Surface( surf ).Vertex( 3 ).x << Surface( surf ).Vertex( 3 ).y << Surface( surf ).Vertex( 3 ).z;
				} else {
					gio::write( unit, Format_705 ) << Surface( surf ).Vertex( 4 ).x << Surface( surf ).Vertex( 4 ).y << Surface( surf ).Vertex( 4 ).z;
				}
			} else { // polygon attached shading
				if ( ! TriangulateFace ) {
					minz = 99999.0;
					for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
						minz = min( minz, Surface( surf ).Vertex( vert ).z );
					}
					gio::write( unit, Format_715 ) << TempZoneName << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
					for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
						gio::write( unit, Format_716 ) << TempZoneName << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
					}
					gio::write( unit, Format_717 ) << TempZoneName;
				} else {
					if ( Surface( surf ).Shape == RectangularOverhang ) {
						ntri = Triangulate( Surface( surf ).Sides, Surface( surf ).Vertex, mytriangles, Surface( surf ).Azimuth, Surface( surf ).Tilt, Surface( surf ).Name, SurfaceClass_Overhang );
					} else {
						ntri = Triangulate( Surface( surf ).Sides, Surface( surf ).Vertex, mytriangles, Surface( surf ).Azimuth, Surface( surf ).Tilt, Surface( surf ).Name, SurfaceClass_Fin );
					}
					for ( svert = 1; svert <= ntri; ++svert ) {
						vv0 = mytriangles( svert ).vv0;
						vv1 = mytriangles( svert ).vv1;
						vv2 = mytriangles( svert ).vv2;
						gio::write( unit, Format_704 ) << TempZoneName << DXFcolorno( colorindex ) << Surface( surf ).Vertex( vv0 ).x << Surface( surf ).Vertex( vv0 ).y << Surface( surf ).Vertex( vv0 ).z << Surface( surf ).Vertex( vv1 ).x << Surface( surf ).Vertex( vv1 ).y << Surface( surf ).Vertex( vv1 ).z << Surface( surf ).Vertex( vv2 ).x << Surface( surf ).Vertex( vv2 ).y << Surface( surf ).Vertex( vv2 ).z;
						gio::write( unit, Format_705 ) << Surface( surf ).Vertex( vv2 ).x << Surface( surf ).Vertex( vv2 ).y << Surface( surf ).Vertex( vv2 ).z;
					}
					mytriangles.deallocate();
				}
			}
		}
	}

	//  711 format('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)
	//  712 format(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
	//             ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)

	// Do any daylighting reference points on layer for zone
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		curcolorno = ColorNo_DaylSensor1;
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( refpt = 1; refpt <= ZoneDaylight( zones ).TotalDaylRefPoints; ++refpt ) {
			gio::write( unit, Format_710 ) << Zone( zones ).Name + ":DayRefPt:" + TrimSigDigits( refpt );
			gio::write( unit, Format_709 ) << TempZoneName << DXFcolorno( curcolorno ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 1, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 2, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 3, refpt ) << 0.2;
			curcolorno = ColorNo_DaylSensor2; // ref pts 2 and later are this color
		}
	}

	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		curcolorno = ColorNo_DaylSensor1;
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( mapnum = 1; mapnum <= TotIllumMaps; ++mapnum ) {
			if ( IllumMapCalc( mapnum ).Zone != zones ) continue;
			for ( refpt = 1; refpt <= IllumMapCalc( mapnum ).TotalMapRefPoints; ++refpt ) {
				gio::write( unit, Format_710 ) << Zone( zones ).Name + ":MapRefPt:" + TrimSigDigits( refpt );
				gio::write( unit, Format_709 ) << TempZoneName << DXFcolorno( curcolorno ) << IllumMapCalc( mapnum ).MapRefPtAbsCoord( 1, refpt ) << IllumMapCalc( mapnum ).MapRefPtAbsCoord( 2, refpt ) << IllumMapCalc( mapnum ).MapRefPtAbsCoord( 3, refpt ) << 0.05;
			}
		}
	}

	// now do DElight reference points
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		curcolorno = ColorNo_DaylSensor1;
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( refpt = 1; refpt <= ZoneDaylight( zones ).TotalDElightRefPts; ++refpt ) {
			gio::write( unit, Format_710 ) << Zone( zones ).Name + ":DEDayRefPt:" + TrimSigDigits( refpt );
			gio::write( unit, Format_709 ) << TempZoneName << DXFcolorno( curcolorno ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 1, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 2, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 3, refpt ) << 0.2;
			curcolorno = ColorNo_DaylSensor2; // ref pts 2 and later are this color
		}
	}

	gio::write( unit, Format_706 );

	gio::close( unit );

}

void
DXFOutLines( std::string const & ColorScheme )
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   August 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine produces a points file of lines in the surfaces.

	// METHODOLOGY EMPLOYED:
	// Use the surface absolute coordinate information to produce
	// lines.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataHeatBalance::BuildingName;
	using DataHeatBalance::Zone;
	using namespace DataSurfaces;
	using namespace DataSurfaceColors;
	using DataDaylighting::ZoneDaylight;
	using DataGlobals::NumOfZones;
	using DataStringGlobals::VerString;
	using General::TrimSigDigits;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static Array1D< Real64 > StemX( 4, -10.0 );
	static Array1D< Real64 > StemY( 4, { 3.0, 3.0, 0.0, 0.0 } );
	static Array1D< Real64 > StemZ( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > Head1X( 4, { -10.0, -10.0, -10.5, -10.5 } );
	static Array1D< Real64 > Head1Y( 4, { 3.0, 3.0, 2.133975, 2.133975 } );
	static Array1D< Real64 > Head1Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > Head2X( 4, { -10.0, -10.0, -9.5, -9.5 } );
	static Array1D< Real64 > Head2Y( 4, { 3.0, 3.0, 2.133975, 2.133975 } );
	static Array1D< Real64 > Head2Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide1X( 4, -10.5 );
	static Array1D< Real64 > NSide1Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide1Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide2X( 4, { -10.5, -10.5, -9.5, -9.5 } );
	static Array1D< Real64 > NSide2Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide2Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide3X( 4, -9.5 );
	static Array1D< Real64 > NSide3Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide3Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	//  integer, dimension(7) :: colorno=(/3,4,5,6,2,8,9/)
	int unit; // Unit number on which to write file
	int surf; // Loop variable for surfaces
	int vert; // Loop counter
	int colorindex; // color index by surface type
	Real64 minx; // minimum x in surface data
	Real64 miny; // minimum y in surface data
	Real64 minz; // minimum z in surface data (for polygon output)
	int zones; // loop counter for zone loop
	std::string ZoneNum;
	std::string TempZoneName;
	std::string::size_type pos;
	std::string ShadeType;
	//unused  character(len=5) :: PolylineWidth=' 0.55'
	std::string cSurfNum;
	int surfcount;
	int sptr;
	int refpt;
	int curcolorno;
	int write_stat;

	// Formats
	static gio::Fmt Format_702( "('  0',/,'SECTION',/,'  2',/,'ENTITIES')" );
	static gio::Fmt Format_707( "('999',/,'DXF created from EnergyPlus')" );
	static gio::Fmt Format_708( "('999',/,A,A,A)" );
	static gio::Fmt Format_800( "('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,' .25',/,'  1',/,'True North',/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,'210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')" );
	static gio::Fmt Format_801( "('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,' .4',/,'  1',/,A,/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,'210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')" );
	static gio::Fmt Format_703_0( "('  0',/,'3DFACE',/,'  8',/,'1',/,' 62',/,I3)" );
	static gio::Fmt Format_703_1( "(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)" );
	static gio::Fmt Format_703_2( "(' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)" );
	static gio::Fmt Format_703_3( "(' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)" );
	static gio::Fmt Format_703_4( "(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)" );
	static gio::Fmt Format_704( "('  0',/,'3DFACE',/,'  8',/,A,/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)" );
	static gio::Fmt Format_705( "(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)" );
	static gio::Fmt Format_711( "('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)" );
	static gio::Fmt Format_712( "(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)" );
	static gio::Fmt Format_706( "('  0',/,'ENDSEC',/,'  0',/,'EOF')" );
	static gio::Fmt Format_709( "('  0',/,'CIRCLE',/,'  8',/,A,/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,f15.5)" );
	static gio::Fmt Format_710( "('999',/,A)" );

	if ( TotSurfaces > 0 && ! allocated( Surface ) ) {
		// no error needed, probably in end processing, just return
		return;
	}

	unit = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( unit, DataStringGlobals::outputDxfFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		ShowFatalError( "DXFOutLines: Could not open file "+DataStringGlobals::outputDxfFileName+" for output (write)." );
	}

	gio::write( unit, Format_702 ); // Start of Entities section

	gio::write( unit, Format_707 ); // Comment

	gio::write( unit, Format_708 ) << "Program Version" << "," << VerString;

	gio::write( unit, Format_708 ) << "DXF using Lines" << ' ' << ' ';

	if ( ColorScheme == "" ) {
		gio::write( unit, Format_708 ) << "Color Scheme" << "," << "Default";
	} else {
		gio::write( unit, Format_708 ) << "Color Scheme" << "," << ColorScheme;
	}

	minx = 99999.0;
	miny = 99999.0;
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
		for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
			minx = min( minx, Surface( surf ).Vertex( vert ).x );
			miny = min( miny, Surface( surf ).Vertex( vert ).y );
		}
	}

	for ( vert = 1; vert <= 4; ++vert ) {
		StemX( vert ) += minx;
		StemY( vert ) += miny;
		Head1X( vert ) += minx;
		Head1Y( vert ) += miny;
		Head2X( vert ) += minx;
		Head2Y( vert ) += miny;
		NSide1X( vert ) += minx;
		NSide1Y( vert ) += miny;
		NSide2X( vert ) += minx;
		NSide2Y( vert ) += miny;
		NSide3X( vert ) += minx;
		NSide3Y( vert ) += miny;
	}

	// This writes "True North" above the Arrow Head
	gio::write( unit, Format_710 ) << "Text - True North";
	gio::write( unit, Format_800 ) << DXFcolorno( ColorNo_Text ) << StemX( 1 ) - 1.0 << StemY( 1 ) << StemZ( 1 );

	gio::write( unit, Format_710 ) << "Text - Building Title";
	gio::write( unit, Format_801 ) << DXFcolorno( ColorNo_Text ) << StemX( 1 ) - 4.0 << StemY( 1 ) - 4.0 << StemZ( 1 ) << "Building - " + BuildingName;

	// We want to point the north arrow to true north
	gio::write( unit, Format_710 ) << "North Arrow Stem";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << StemX( 1 ) << StemY( 1 ) << StemZ( 1 );
	gio::write( unit, Format_703_2 ) << StemX( 2 ) << StemY( 2 ) << StemZ( 2 );
	gio::write( unit, Format_703_3 ) << StemX( 3 ) << StemY( 3 ) << StemZ( 3 );
	gio::write( unit, Format_703_4 ) << StemX( 4 ) << StemY( 4 ) << StemZ( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Head 1";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << Head1X( 1 ) << Head1Y( 1 ) << Head1Z( 1 );
	gio::write( unit, Format_703_2 ) << Head1X( 2 ) << Head1Y( 2 ) << Head1Z( 2 );
	gio::write( unit, Format_703_3 ) << Head1X( 3 ) << Head1Y( 3 ) << Head1Z( 3 );
	gio::write( unit, Format_703_4 ) << Head1X( 4 ) << Head1Y( 4 ) << Head1Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Head 2";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << Head2X( 1 ) << Head2Y( 1 ) << Head2Z( 1 );
	gio::write( unit, Format_703_2 ) << Head2X( 2 ) << Head2Y( 2 ) << Head2Z( 2 );
	gio::write( unit, Format_703_3 ) << Head2X( 3 ) << Head2Y( 3 ) << Head2Z( 3 );
	gio::write( unit, Format_703_4 ) << Head2X( 4 ) << Head2Y( 4 ) << Head2Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 1";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide1X( 1 ) << NSide1Y( 1 ) << NSide1Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide1X( 2 ) << NSide1Y( 2 ) << NSide1Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide1X( 3 ) << NSide1Y( 3 ) << NSide1Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide1X( 4 ) << NSide1Y( 4 ) << NSide1Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 2";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide2X( 1 ) << NSide2Y( 1 ) << NSide2Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide2X( 2 ) << NSide2Y( 2 ) << NSide2Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide2X( 3 ) << NSide2Y( 3 ) << NSide2Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide2X( 4 ) << NSide2Y( 4 ) << NSide2Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 3";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide3X( 1 ) << NSide3Y( 1 ) << NSide3Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide3X( 2 ) << NSide3Y( 2 ) << NSide3Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide3X( 3 ) << NSide3Y( 3 ) << NSide3Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide3X( 4 ) << NSide3Y( 4 ) << NSide3Z( 4 );

	gio::write( unit, Format_710 ) << "Zone Names";
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		gio::write( ZoneNum, fmtLD ) << zones;
		strip( ZoneNum );
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		gio::write( unit, Format_710 ) << "Zone=" + ZoneNum + ':' + TempZoneName;
	}

	//  Do all detached shading surfaces first
	surfcount = 0;
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).HeatTransSurf ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Shading ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) colorindex = ColorNo_ShdDetFix;
		if ( Surface( surf ).Class == SurfaceClass_Detached_B ) colorindex = ColorNo_ShdDetBldg;
		if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) {
			ShadeType = "Fixed Shading";
			gio::write( unit, Format_710 ) << "Fixed Shading:" + Surface( surf ).Name;
		} else if ( Surface( surf ).Class == SurfaceClass_Detached_B ) {
			ShadeType = "Building Shading";
			gio::write( unit, Format_710 ) << "Building Shading:" + Surface( surf ).Name;
		}
		++surfcount;
		gio::write( cSurfNum, fmtLD ) << surfcount;
		strip( cSurfNum );
		ShadeType += "_" + cSurfNum;
		minz = 99999.0;
		for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
			minz = min( minz, Surface( surf ).Vertex( vert ).z );
		}
		if ( Surface( surf ).Sides <= 4 ) {
			//      write(unit,711) TRIM(ShadeType),colorno(colorindex) !,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				if ( vert != Surface( surf ).Sides ) {
					sptr = vert + 1;
				} else {
					sptr = 1;
				}
				gio::write( unit, Format_711 ) << ShadeType << DXFcolorno( colorindex ); //,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
				gio::write( unit, Format_712 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( sptr ).x << Surface( surf ).Vertex( sptr ).y << Surface( surf ).Vertex( sptr ).z;
			}
		} else { // polygon
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				if ( vert != Surface( surf ).Sides ) {
					sptr = vert + 1;
				} else {
					sptr = 1;
				}
				gio::write( unit, Format_711 ) << ShadeType << DXFcolorno( colorindex ); //,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
				gio::write( unit, Format_712 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( sptr ).x << Surface( surf ).Vertex( sptr ).y << Surface( surf ).Vertex( sptr ).z;
			}
		}
	}

	// now do zone surfaces, by zone
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}

		surfcount = 0;
		for ( surf = max( Zone( zones ).SurfaceFirst, 1 ); surf <= Zone( zones ).SurfaceLast; ++surf ) {
			if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
			if ( Surface( surf ).Class == SurfaceClass_Wall ) colorindex = ColorNo_Wall;
			if ( Surface( surf ).Class == SurfaceClass_Roof ) colorindex = ColorNo_Roof;
			if ( Surface( surf ).Class == SurfaceClass_Floor ) colorindex = ColorNo_Floor;
			if ( Surface( surf ).Class == SurfaceClass_Door ) colorindex = ColorNo_Door;
			if ( Surface( surf ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_Window ) colorindex = ColorNo_Window;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_GlassDoor ) colorindex = ColorNo_GlassDoor;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_TDD_Dome ) colorindex = ColorNo_TDDDome;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_TDD_Diffuser ) colorindex = ColorNo_TDDDiffuser;
			}
			if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
			++surfcount;
			++surfcount;
			gio::write( cSurfNum, fmtLD ) << surfcount;
			strip( cSurfNum );

			gio::write( unit, Format_710 ) << Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			TempZoneName += "_" + cSurfNum;
			minz = 99999.0;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				minz = min( minz, Surface( surf ).Vertex( vert ).z );
			}
			if ( Surface( surf ).Sides <= 4 ) {
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					if ( vert != Surface( surf ).Sides ) {
						sptr = vert + 1;
					} else {
						sptr = 1;
					}
					gio::write( unit, Format_711 ) << TempZoneName << DXFcolorno( colorindex ); //,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
					gio::write( unit, Format_712 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( sptr ).x << Surface( surf ).Vertex( sptr ).y << Surface( surf ).Vertex( sptr ).z;
				}
			} else { // polygon
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					if ( vert != Surface( surf ).Sides ) {
						sptr = vert + 1;
					} else {
						sptr = 1;
					}
					gio::write( unit, Format_711 ) << TempZoneName << DXFcolorno( colorindex ); //,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
					gio::write( unit, Format_712 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( sptr ).x << Surface( surf ).Vertex( sptr ).y << Surface( surf ).Vertex( sptr ).z;
				}
			}
			// 715 format('  0',/,'POLYLINE',/,'  8',/,A,/,' 62',/,I3,/,' 66',/,'  1',/,  &
			//    ' 10',/,' 0.0',/,' 20',/,' 0.0',/,' 30',/,f15.5,/,  &
			//    ' 70',/,'   1',/,' 40',/,A,/,' 41',/,A)
			// 716 format('  0',/'VERTEX',/,'  8',/,A,/,  &
			//    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)
			// 717 format('  0',/'SEQEND',/,'  8',/,A)

		}
		// still have to do shading surfaces for zone
		surfcount = 0;
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			//if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
			if ( Surface( surf ).Class != SurfaceClass_Shading ) continue;
			if ( Surface( surf ).ZoneName != Zone( zones ).Name ) continue;
			colorindex = ColorNo_ShdAtt;
			if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
			++surfcount;
			gio::write( cSurfNum, fmtLD ) << surfcount;
			strip( cSurfNum );

			gio::write( unit, Format_710 ) << Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			TempZoneName += "_" + cSurfNum;
			minz = 99999.0;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				minz = min( minz, Surface( surf ).Vertex( vert ).z );
			}
			if ( Surface( surf ).Sides <= 4 ) {
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					if ( vert != Surface( surf ).Sides ) {
						sptr = vert + 1;
					} else {
						sptr = 1;
					}
					gio::write( unit, Format_711 ) << TempZoneName << DXFcolorno( colorindex ); //,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
					gio::write( unit, Format_712 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( sptr ).x << Surface( surf ).Vertex( sptr ).y << Surface( surf ).Vertex( sptr ).z;
				}
			} else { // polygon attached shading
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					if ( vert != Surface( surf ).Sides ) {
						sptr = vert + 1;
					} else {
						sptr = 1;
					}
					gio::write( unit, Format_711 ) << TempZoneName << DXFcolorno( colorindex ); //,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
					gio::write( unit, Format_712 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z << Surface( surf ).Vertex( sptr ).x << Surface( surf ).Vertex( sptr ).y << Surface( surf ).Vertex( sptr ).z;
				}
			}
		}
	}

	// Do any daylighting reference points on layer for zone
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		curcolorno = ColorNo_DaylSensor1;
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( refpt = 1; refpt <= ZoneDaylight( zones ).TotalDaylRefPoints; ++refpt ) {
			gio::write( unit, Format_710 ) << Zone( zones ).Name + ":DayRefPt:" + TrimSigDigits( refpt );
			gio::write( unit, Format_709 ) << TempZoneName << DXFcolorno( curcolorno ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 1, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 2, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 3, refpt ) << 0.2;
			curcolorno = ColorNo_DaylSensor2; // ref pts 2 and later are this color
		}
	}

	// now do DElight reference points
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		curcolorno = ColorNo_DaylSensor1;
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( refpt = 1; refpt <= ZoneDaylight( zones ).TotalDElightRefPts; ++refpt ) {
			gio::write( unit, Format_710 ) << Zone( zones ).Name + ":DEDayRefPt:" + TrimSigDigits( refpt );
			gio::write( unit, Format_709 ) << TempZoneName << DXFcolorno( curcolorno ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 1, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 2, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 3, refpt ) << 0.2;
			curcolorno = ColorNo_DaylSensor2; // ref pts 2 and later are this color
		}
	}

	gio::write( unit, Format_706 );

	gio::close( unit );

}

void
DXFOutWireFrame( std::string const & ColorScheme )
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   August 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine produces a file of DXF objects for the surfaces (all lines -- wireframe).

	// METHODOLOGY EMPLOYED:
	// Use the surface absolute coordinate information to produce
	// lines.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataHeatBalance::BuildingName;
	using DataHeatBalance::Zone;
	using namespace DataSurfaces;
	using namespace DataSurfaceColors;
	using DataDaylighting::ZoneDaylight;
	using DataGlobals::NumOfZones;
	using DataStringGlobals::VerString;
	using General::TrimSigDigits;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static Array1D< Real64 > StemX( 4, -10.0 );
	static Array1D< Real64 > StemY( 4, { 3.0, 3.0, 0.0, 0.0 } );
	static Array1D< Real64 > StemZ( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > Head1X( 4, { -10.0, -10.0, -10.5, -10.5 } );
	static Array1D< Real64 > Head1Y( 4, { 3.0, 3.0, 2.133975, 2.133975 } );
	static Array1D< Real64 > Head1Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > Head2X( 4, { -10.0, -10.0, -9.5, -9.5 } );
	static Array1D< Real64 > Head2Y( 4, { 3.0, 3.0, 2.133975, 2.133975 } );
	static Array1D< Real64 > Head2Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide1X( 4, -10.5 );
	static Array1D< Real64 > NSide1Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide1Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide2X( 4, { -10.5, -10.5, -9.5, -9.5 } );
	static Array1D< Real64 > NSide2Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide2Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	static Array1D< Real64 > NSide3X( 4, -9.5 );
	static Array1D< Real64 > NSide3Y( 4, { 4.5, 4.5, 3.5, 3.5 } );
	static Array1D< Real64 > NSide3Z( 4, { 0.1, 0.0, 0.0, 0.1 } );
	//  integer, dimension(7) :: colorno=(/3,4,5,6,2,8,9/)
	int unit; // Unit number on which to write file
	int surf; // Loop variable for surfaces
	int vert; // Loop counter
	int colorindex; // color index by surface type
	Real64 minx; // minimum x in surface data
	Real64 miny; // minimum y in surface data
	Real64 minz; // minimum z in surface data (for polygon output)
	int zones; // loop counter for zone loop
	std::string ZoneNum;
	std::string TempZoneName;
	std::string SaveZoneName;
	std::string::size_type pos;
	std::string ShadeType;
	static std::string PolylineWidth( " 0.55" );
	std::string cSurfNum;
	int surfcount;
	int refpt;
	int curcolorno;
	int write_stat;

	// Formats
	static gio::Fmt Format_702( "('  0',/,'SECTION',/,'  2',/,'ENTITIES')" );
	static gio::Fmt Format_707( "('999',/,'DXF created from EnergyPlus')" );
	static gio::Fmt Format_708( "('999',/,A,A,A)" );
	static gio::Fmt Format_800( "('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,' .25',/,'  1',/,'True North',/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,'210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')" );
	static gio::Fmt Format_801( "('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,' .4',/,'  1',/,A,/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,'210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')" );
	static gio::Fmt Format_703_0( "('  0',/,'3DFACE',/,'  8',/,'1',/,' 62',/,I3)" );
	static gio::Fmt Format_703_1( "(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)" );
	static gio::Fmt Format_703_2( "(' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)" );
	static gio::Fmt Format_703_3( "(' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)" );
	static gio::Fmt Format_703_4( "(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)" );
	static gio::Fmt Format_715( "('  0',/,'POLYLINE',/,'  8',/,A,/,' 62',/,I3,/,' 66',/,'  1',/,' 10',/,' 0.0',/,' 20',/,' 0.0',/,' 30',/,f15.5,/,' 70',/,'   9',/,' 40',/,A,/,' 41',/,A)" );
	static gio::Fmt Format_716( "('  0',/,'VERTEX',/,'  8',/,A,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)" );
	static gio::Fmt Format_717( "('  0',/,'SEQEND',/,'  8',/,A)" );
	static gio::Fmt Format_704( "('  0',/,'3DFACE',/,'  8',/,A,/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)" );
	static gio::Fmt Format_705( "(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)" );
	static gio::Fmt Format_706( "('  0',/,'ENDSEC',/,'  0',/,'EOF')" );
	static gio::Fmt Format_709( "('  0',/,'CIRCLE',/,'  8',/,A,/,' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,' 40',/,f15.5)" );
	static gio::Fmt Format_710( "('999',/,A)" );

	if ( TotSurfaces > 0 && ! allocated( Surface ) ) {
		// no error needed, probably in end processing, just return
		return;
	}

	unit = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( unit, DataStringGlobals::outputDxfFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		ShowFatalError( "DXFOutWireFrame: Could not open file "+DataStringGlobals::outputDxfFileName+" for output (write)." );
	}

	gio::write( unit, Format_702 ); // Start of Entities section

	gio::write( unit, Format_707 ); // Comment

	gio::write( unit, Format_708 ) << "Program Version" << "," << VerString;

	gio::write( unit, Format_708 ) << "DXF using Wireframe" << ' ' << ' ';

	if ( ColorScheme == "" ) {
		gio::write( unit, Format_708 ) << "Color Scheme" << "," << "Default";
	} else {
		gio::write( unit, Format_708 ) << "Color Scheme" << "," << ColorScheme;
	}

	minx = 99999.0;
	miny = 99999.0;
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
		for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
			minx = min( minx, Surface( surf ).Vertex( vert ).x );
			miny = min( miny, Surface( surf ).Vertex( vert ).y );
		}
	}

	for ( vert = 1; vert <= 4; ++vert ) {
		StemX( vert ) += minx;
		StemY( vert ) += miny;
		Head1X( vert ) += minx;
		Head1Y( vert ) += miny;
		Head2X( vert ) += minx;
		Head2Y( vert ) += miny;
		NSide1X( vert ) += minx;
		NSide1Y( vert ) += miny;
		NSide2X( vert ) += minx;
		NSide2Y( vert ) += miny;
		NSide3X( vert ) += minx;
		NSide3Y( vert ) += miny;
	}

	// This writes "True North" above the Arrow Head
	gio::write( unit, Format_710 ) << "Text - True North";
	gio::write( unit, Format_800 ) << DXFcolorno( ColorNo_Text ) << StemX( 1 ) - 1.0 << StemY( 1 ) << StemZ( 1 );

	gio::write( unit, Format_710 ) << "Text - Building Title";
	gio::write( unit, Format_801 ) << DXFcolorno( ColorNo_Text ) << StemX( 1 ) - 4.0 << StemY( 1 ) - 4.0 << StemZ( 1 ) << "Building - " + BuildingName;

	// We want to point the north arrow to true north
	gio::write( unit, Format_710 ) << "North Arrow Stem";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << StemX( 1 ) << StemY( 1 ) << StemZ( 1 );
	gio::write( unit, Format_703_2 ) << StemX( 2 ) << StemY( 2 ) << StemZ( 2 );
	gio::write( unit, Format_703_3 ) << StemX( 3 ) << StemY( 3 ) << StemZ( 3 );
	gio::write( unit, Format_703_4 ) << StemX( 4 ) << StemY( 4 ) << StemZ( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Head 1";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << Head1X( 1 ) << Head1Y( 1 ) << Head1Z( 1 );
	gio::write( unit, Format_703_2 ) << Head1X( 2 ) << Head1Y( 2 ) << Head1Z( 2 );
	gio::write( unit, Format_703_3 ) << Head1X( 3 ) << Head1Y( 3 ) << Head1Z( 3 );
	gio::write( unit, Format_703_4 ) << Head1X( 4 ) << Head1Y( 4 ) << Head1Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Head 2";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << Head2X( 1 ) << Head2Y( 1 ) << Head2Z( 1 );
	gio::write( unit, Format_703_2 ) << Head2X( 2 ) << Head2Y( 2 ) << Head2Z( 2 );
	gio::write( unit, Format_703_3 ) << Head2X( 3 ) << Head2Y( 3 ) << Head2Z( 3 );
	gio::write( unit, Format_703_4 ) << Head2X( 4 ) << Head2Y( 4 ) << Head2Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 1";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide1X( 1 ) << NSide1Y( 1 ) << NSide1Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide1X( 2 ) << NSide1Y( 2 ) << NSide1Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide1X( 3 ) << NSide1Y( 3 ) << NSide1Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide1X( 4 ) << NSide1Y( 4 ) << NSide1Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 2";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide2X( 1 ) << NSide2Y( 1 ) << NSide2Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide2X( 2 ) << NSide2Y( 2 ) << NSide2Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide2X( 3 ) << NSide2Y( 3 ) << NSide2Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide2X( 4 ) << NSide2Y( 4 ) << NSide2Z( 4 );

	gio::write( unit, Format_710 ) << "North Arrow Side 3";
	gio::write( unit, Format_703_0 ) << DXFcolorno(ColorNo_Text);
	gio::write( unit, Format_703_1 ) << NSide3X( 1 ) << NSide3Y( 1 ) << NSide3Z( 1 );
	gio::write( unit, Format_703_2 ) << NSide3X( 2 ) << NSide3Y( 2 ) << NSide3Z( 2 );
	gio::write( unit, Format_703_3 ) << NSide3X( 3 ) << NSide3Y( 3 ) << NSide3Z( 3 );
	gio::write( unit, Format_703_4 ) << NSide3X( 4 ) << NSide3Y( 4 ) << NSide3Z( 4 );


	gio::write( unit, Format_710 ) << "Zone Names";
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		gio::write( ZoneNum, fmtLD ) << zones;
		strip( ZoneNum );
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		gio::write( unit, Format_710 ) << "Zone=" + ZoneNum + ':' + TempZoneName;
	}

	//  Do all detached shading surfaces first
	surfcount = 0;
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).HeatTransSurf ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Shading ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) colorindex = ColorNo_ShdDetFix;
		if ( Surface( surf ).Class == SurfaceClass_Detached_B ) colorindex = ColorNo_ShdDetBldg;
		if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) {
			ShadeType = "Fixed Shading";
			gio::write( unit, Format_710 ) << "Fixed Shading:" + Surface( surf ).Name;
		} else if ( Surface( surf ).Class == SurfaceClass_Detached_B ) {
			ShadeType = "Building Shading";
			gio::write( unit, Format_710 ) << "Building Shading:" + Surface( surf ).Name;
		}
		++surfcount;
		gio::write( cSurfNum, fmtLD ) << surfcount;
		strip( cSurfNum );
		ShadeType += "_" + cSurfNum;
		minz = 99999.0;
		for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
			minz = min( minz, Surface( surf ).Vertex( vert ).z );
		}
		if ( Surface( surf ).Sides <= 4 ) {
			gio::write( unit, Format_715 ) << ShadeType << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				gio::write( unit, Format_716 ) << ShadeType << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
			}
			gio::write( unit, Format_717 ) << ShadeType;
		} else { // polygon
			gio::write( unit, Format_715 ) << ShadeType << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				gio::write( unit, Format_716 ) << ShadeType << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
			}
			gio::write( unit, Format_717 ) << ShadeType;
		}
	}

	// now do zone surfaces, by zone
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		SaveZoneName = Zone( zones ).Name;
		pos = index( SaveZoneName, ' ' );
		while ( pos != std::string::npos ) {
			SaveZoneName[ pos ] = '_';
			pos = index( SaveZoneName, ' ' );
		}
		pos = index( SaveZoneName, ':' );
		while ( pos != std::string::npos ) {
			SaveZoneName[ pos ] = '_';
			pos = index( SaveZoneName, ':' );
		}

		surfcount = 0;
		for ( surf = max( Zone( zones ).SurfaceFirst, 1 ); surf <= Zone( zones ).SurfaceLast; ++surf ) {
			if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
			if ( Surface( surf ).Class == SurfaceClass_Wall ) colorindex = ColorNo_Wall;
			if ( Surface( surf ).Class == SurfaceClass_Roof ) colorindex = ColorNo_Roof;
			if ( Surface( surf ).Class == SurfaceClass_Floor ) colorindex = ColorNo_Floor;
			if ( Surface( surf ).Class == SurfaceClass_Door ) colorindex = ColorNo_Door;
			if ( Surface( surf ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_Window ) colorindex = ColorNo_Window;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_GlassDoor ) colorindex = ColorNo_GlassDoor;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_TDD_Dome ) colorindex = ColorNo_TDDDome;
				if ( SurfaceWindow( surf ).OriginalClass == SurfaceClass_TDD_Diffuser ) colorindex = ColorNo_TDDDiffuser;
			}
			if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
			++surfcount;
			gio::write( cSurfNum, fmtLD ) << surfcount;
			strip( cSurfNum );

			gio::write( unit, Format_710 ) << Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			TempZoneName = SaveZoneName + '_' + cSurfNum;
			minz = 99999.0;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				minz = min( minz, Surface( surf ).Vertex( vert ).z );
			}
			if ( Surface( surf ).Sides <= 4 ) {
				gio::write( unit, Format_715 ) << TempZoneName << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					gio::write( unit, Format_716 ) << TempZoneName << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
				}
				gio::write( unit, Format_717 ) << TempZoneName;
			} else { // polygon
				gio::write( unit, Format_715 ) << TempZoneName << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					gio::write( unit, Format_716 ) << TempZoneName << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
				}
				gio::write( unit, Format_717 ) << TempZoneName;
			}

		}
		// still have to do shading surfaces for zone
		surfcount = 0;
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			//if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
			if ( Surface( surf ).Class != SurfaceClass_Shading ) continue;
			if ( Surface( surf ).ZoneName != Zone( zones ).Name ) continue;
			colorindex = ColorNo_ShdAtt;
			if ( Surface( surf ).IsPV ) colorindex = ColorNo_PV;
			++surfcount;
			gio::write( cSurfNum, fmtLD ) << surfcount;
			strip( cSurfNum );

			gio::write( unit, Format_710 ) << Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			TempZoneName = SaveZoneName + '_' + cSurfNum;
			minz = 99999.0;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				minz = min( minz, Surface( surf ).Vertex( vert ).z );
			}
			if ( Surface( surf ).Sides <= 4 ) {
				gio::write( unit, Format_715 ) << TempZoneName << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					gio::write( unit, Format_716 ) << TempZoneName << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
				}
				gio::write( unit, Format_717 ) << TempZoneName;
			} else { // polygon attached shading
				gio::write( unit, Format_715 ) << TempZoneName << DXFcolorno( colorindex ) << minz << PolylineWidth << PolylineWidth;
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					gio::write( unit, Format_716 ) << TempZoneName << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
				}
				gio::write( unit, Format_717 ) << TempZoneName;
			}
		}
	}

	//  711 format('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)
	//  712 format(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
	//             ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)

	// Do any daylighting reference points on layer for zone
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		curcolorno = ColorNo_DaylSensor1;
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( refpt = 1; refpt <= ZoneDaylight( zones ).TotalDaylRefPoints; ++refpt ) {
			gio::write( unit, Format_710 ) << Zone( zones ).Name + ":DayRefPt:" + TrimSigDigits( refpt );
			gio::write( unit, Format_709 ) << TempZoneName << DXFcolorno( curcolorno ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 1, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 2, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 3, refpt ) << 0.2;
			curcolorno = ColorNo_DaylSensor2; // ref pts 2 and later are this color
		}
	}

	// now do DElight reference points
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		curcolorno = ColorNo_DaylSensor1;
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( refpt = 1; refpt <= ZoneDaylight( zones ).TotalDElightRefPts; ++refpt ) {
			gio::write( unit, Format_710 ) << Zone( zones ).Name + ":DEDayRefPt:" + TrimSigDigits( refpt );
			gio::write( unit, Format_709 ) << TempZoneName << DXFcolorno( curcolorno ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 1, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 2, refpt ) << ZoneDaylight( zones ).DaylRefPtAbsCoord( 3, refpt ) << 0.2;
			curcolorno = ColorNo_DaylSensor2; // ref pts 2 and later are this color
		}
	}

	gio::write( unit, Format_706 );

	gio::close( unit );

}

void
DetailsForSurfaces( int const RptType ) // (1=Vertices only, 10=Details only, 11=Details with vertices)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   February 2002
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine provides an optional detailed surface report
	// for each surface in the input file.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using DataGlobals::NumOfZones;
	using DataGlobals::OutputFileInits;
	using General::RoundSigDigits;
	using General::TrimSigDigits;
	using ScheduleManager::GetScheduleName;
	using ScheduleManager::GetScheduleMinValue;
	using ScheduleManager::GetScheduleMaxValue;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static Array1D_string const ConvCoeffCalcs( {1,9}, { "ASHRAESimple", "ASHRAETARP", "CeilingDiffuser", "TrombeWall", "TARP", "MoWitt", "DOE-2", "BLAST", "AdaptiveConvectionAlgorithm" } );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int unit; // Unit number on which to write file
	int surf; // Loop variable for surfaces
	int vert; // Loop counter
	int ZoneNum; // Loop counter
	std::string BaseSurfName;
	std::string ConstructionName;
	std::string ScheduleName;
	std::string IntConvCoeffCalc;
	std::string ExtConvCoeffCalc;
	Real64 NominalUwithConvCoeffs;
	std::string cNominalU;
	std::string cNominalUwithConvCoeffs;
	std::string cSchedMin;
	std::string cSchedMax;
	std::string SolarDiffusing;
	int fd;
	std::string AlgoName;

	// Formats
	static gio::Fmt Format_700( "('! <Zone/Shading Surfaces>,<Zone Name>/#Shading Surfaces,# Surfaces')" );
	static gio::Fmt Format_701( "('! <HeatTransfer/Shading/Frame/Divider_Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm')" );
	static gio::Fmt Format_7011( "(',Construction/Transmittance Schedule,Nominal U (w/o film coefs)/Min Schedule Value,','Nominal U (with film coefs)/Max Schedule Value,Solar Diffusing,','Area (Net),Area (Gross),Area (Sunlit Calc),Azimuth,Tilt,~Width,~Height,Reveal,','<ExtBoundCondition>,<ExtConvCoeffCalc>,<IntConvCoeffCalc>,<SunExposure>,<WindExposure>,','ViewFactorToGround,ViewFactorToSky,ViewFactorToGround-IR,ViewFactorToSky-IR,#Sides')" );
	static gio::Fmt Format_7012( "(',#Sides')" );
	static gio::Fmt Format_702( "('! <Units>,,,,,')" );
	static gio::Fmt Format_7021( "(',{W/m2-K}/{},{W/m2-K}/{},{},{m2},{m2},{m2},{deg},{deg},{m},{m},{m},,,,,,,,,,')" );
	static gio::Fmt Format_7022( "(',')" );
	static gio::Fmt Format_703( "(A,',',A,',',I5)" );
	static gio::Fmt Format_704( "(A,'_Surface,',A,',',A,',',A,',',A)" );
	static gio::Fmt Format_7041( "(',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',2(A,','),A)" );
	static gio::Fmt Format_7042( "(',',A)" );
	static gio::Fmt Format_7044( "(',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',2(A,','))" );
	static gio::Fmt Format_7045( "(',,,,,',A,',',A,',',A,',',A,',',A,',',A,',',A)" );
	static gio::Fmt Format_705( "(',',A)" );
	static gio::Fmt Format_706( "(4(',',A),',',A)" );
	static gio::Fmt Format_7061( "(',,,,,,,,,,',A)" );
	static gio::Fmt Format_707( "(',{Vertex 1},,,{Vertex 2},,,{Vertex 3},,,{Vertex 4},,,{etc}')" );
	static gio::Fmt Format_708( "(4(',X {m},Y {m},Z {m}'))" );
	static gio::Fmt Format_709( "(3(',',A))" );
	static gio::Fmt Format_710( "(', Vertices are shown starting at Upper-Left-Corner => Counter-Clockwise => World Coordinates')" );
	static gio::Fmt Format_711( "(1X)" );

	if ( TotSurfaces > 0 && ! allocated( Surface ) ) {
		// no error needed, probably in end processing, just return
		return;
	}

	unit = OutputFileInits;

	//!!!    Write Header lines for report
	if ( RptType == 10 ) { // Details only
		gio::write( unit, Format_700 );
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_701, flags ); }
		gio::write( unit, Format_7011 );
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_702, flags ); }
		gio::write( unit, Format_7021 );
	} else if ( RptType == 11 ) { // Details with Vertices
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_700, flags ); }
		gio::write( unit, Format_710 );
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_701, flags ); }
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7011, flags ); }
		gio::write( unit, Format_707 );
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_702, flags ); }
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7021, flags ); }
		gio::write( unit, Format_708 );
	} else { // Vertices only
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_700, flags ); }
		gio::write( unit, Format_710 );
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_701, flags ); }
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7012, flags ); }
		gio::write( unit, Format_707 );
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_702, flags ); }
		gio::write( unit, Format_708 );
	}

	// Do just "detached" shading first
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).Zone != 0 ) break;
	}
	if ( ( surf - 1 ) > 0 ) {
		gio::write( unit, Format_703 ) << "Shading_Surfaces" << "Number of Shading Surfaces" << surf - 1;
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			if ( Surface( surf ).Zone != 0 ) break;
			AlgoName = "None";
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_704, flags ) << "Shading" << Surface( surf ).Name << cSurfaceClass( Surface( surf ).Class ) << Surface( surf ).BaseSurfName << AlgoName; }
			if ( RptType == 10 ) {
				if ( Surface( surf ).SchedShadowSurfIndex > 0 ) {
					ScheduleName = GetScheduleName( Surface( surf ).SchedShadowSurfIndex );
					cSchedMin = RoundSigDigits( GetScheduleMinValue( Surface( surf ).SchedShadowSurfIndex ), 2 );
					cSchedMax = RoundSigDigits( GetScheduleMaxValue( Surface( surf ).SchedShadowSurfIndex ), 2 );
				} else {
					ScheduleName = "";
					cSchedMin = "0.0";
					cSchedMax = "0.0";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7044, flags ) << ScheduleName << cSchedMin << cSchedMax << ' ' << RoundSigDigits( Surface( surf ).Area, 2 ) << RoundSigDigits( Surface( surf ).GrossArea, 2 ) << RoundSigDigits( Surface( surf ).NetAreaShadowCalc, 2 ) << RoundSigDigits( Surface( surf ).Azimuth, 2 ) << RoundSigDigits( Surface( surf ).Tilt, 2 ) << RoundSigDigits( Surface( surf ).Width, 2 ) << RoundSigDigits( Surface( surf ).Height, 2 ); }
				gio::write( unit, Format_7061 ) << TrimSigDigits( Surface( surf ).Sides );
			} else if ( RptType == 1 ) {
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7042, flags ) << TrimSigDigits( Surface( surf ).Sides ); }
			} else {
				if ( Surface( surf ).SchedShadowSurfIndex > 0 ) {
					ScheduleName = GetScheduleName( Surface( surf ).SchedShadowSurfIndex );
					cSchedMin = RoundSigDigits( GetScheduleMinValue( Surface( surf ).SchedShadowSurfIndex ), 2 );
					cSchedMax = RoundSigDigits( GetScheduleMaxValue( Surface( surf ).SchedShadowSurfIndex ), 2 );
				} else {
					ScheduleName = "";
					cSchedMin = "0.0";
					cSchedMax = "0.0";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7044, flags ) << ScheduleName << cSchedMin << cSchedMax << ' ' << RoundSigDigits( Surface( surf ).Area, 2 ) << RoundSigDigits( Surface( surf ).GrossArea, 2 ) << RoundSigDigits( Surface( surf ).NetAreaShadowCalc, 2 ) << RoundSigDigits( Surface( surf ).Azimuth, 2 ) << RoundSigDigits( Surface( surf ).Tilt, 2 ) << RoundSigDigits( Surface( surf ).Width, 2 ) << RoundSigDigits( Surface( surf ).Height, 2 ); }
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7061, flags ) << TrimSigDigits( Surface( surf ).Sides ); }
			}
			if ( RptType == 10 ) continue;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				if ( vert != Surface( surf ).Sides ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_709, flags ) << RoundSigDigits( Surface( surf ).Vertex( vert ).x, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).y, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).z, 2 ); }
				} else {
					gio::write( unit, Format_709 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).x, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).y, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).z, 2 );
				}
			}
			//  This shouldn't happen with shading surface -- always have vertices
			if ( Surface( surf ).Sides == 0 ) gio::write( unit, Format_711 );
		}
	}

	for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
		gio::write( unit, Format_703 ) << "Zone_Surfaces" << Zone( ZoneNum ).Name << ( Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1 );
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			if ( Surface( surf ).Zone != ZoneNum ) continue;
			SolarDiffusing = "";
			if ( RptType == 10 || RptType == 11 ) { // Details and Details with Vertices
				if ( Surface( surf ).BaseSurf == surf ) {
					BaseSurfName = "";
				} else {
					BaseSurfName = Surface( surf ).BaseSurfName;
				}
				{ auto const SELECT_CASE_var( Surface( surf ).HeatTransferAlgorithm );
				if ( SELECT_CASE_var == HeatTransferModel_None ) {
					AlgoName = "None";
				} else if ( SELECT_CASE_var == HeatTransferModel_CTF ) {
					AlgoName = "CTF - ConductionTransferFunction";
				} else if ( SELECT_CASE_var == HeatTransferModel_CondFD ) {
					AlgoName = "CondFD - ConductionFiniteDifference";
				} else if ( SELECT_CASE_var == HeatTransferModel_EMPD ) {
					AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
				} else if ( SELECT_CASE_var == HeatTransferModel_HAMT ) {
					AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
				} else if ( SELECT_CASE_var == HeatTransferModel_Window5 ) {
					AlgoName = "Window5 Detailed Fenestration";
				} else if ( SELECT_CASE_var == HeatTransferModel_ComplexFenestration ) {
					AlgoName = "Window7 Complex Fenestration";
				} else if ( SELECT_CASE_var == HeatTransferModel_TDD ) {
					AlgoName = "Tubular Daylighting Device";
				}}
				// Default Convection Coefficient Calculation Algorithms
				IntConvCoeffCalc = ConvCoeffCalcs( Zone( ZoneNum ).InsideConvectionAlgo );
				ExtConvCoeffCalc = ConvCoeffCalcs( Zone( ZoneNum ).OutsideConvectionAlgo );

				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_704, flags ) << "HeatTransfer" << Surface( surf ).Name << cSurfaceClass( Surface( surf ).Class ) << BaseSurfName << AlgoName; }

				// NOTE - THIS CODE IS REPEATED IN SurfaceGeometry.cc IN SetupZoneGeometry
				// Calculate Nominal U-value with convection/film coefficients for reporting by adding on
				// prescribed R-values for interior and exterior convection coefficients as found in ASHRAE 90.1-2004, Appendix A
				if ( Surface( surf ).Construction > 0 && Surface( surf ).Construction <= TotConstructs ) {
					cNominalUwithConvCoeffs = "";
					{ auto const SELECT_CASE_var( Surface( surf ).Class );
					if ( SELECT_CASE_var == SurfaceClass_Wall ) {
						// Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
						// Exterior:  vertical, exterior wind exposure, Rcout = 0.17 ft2-F-hr/BTU
						if ( NominalU( Surface( surf ).Construction ) > 0.0 ) {
							NominalUwithConvCoeffs = 1.0 / ( 0.1197548 + ( 1.0 / NominalU( Surface( surf ).Construction ) ) + 0.0299387 );
						} else {
							cNominalUwithConvCoeffs = "[invalid]";
						}
					} else if ( SELECT_CASE_var == SurfaceClass_Floor ) {
						// Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
						// Exterior:  horizontal, semi-exterior (crawlspace), Rcout = 0.46 ft2-F-hr/BTU
						if ( NominalU( Surface( surf ).Construction ) > 0.0 ) {
							NominalUwithConvCoeffs = 1.0 / ( 0.1620212 + ( 1.0 / NominalU( Surface( surf ).Construction ) ) + 0.0810106 );
						} else {
							cNominalUwithConvCoeffs = "[invalid]";
						}
					} else if ( SELECT_CASE_var == SurfaceClass_Roof ) {
						// Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
						// Exterior:  horizontal, semi-exterior (attic), Rcout = 0.46 ft2-F-hr/BTU
						if ( NominalU( Surface( surf ).Construction ) > 0.0 ) {
							NominalUwithConvCoeffs = 1.0 / ( 0.1074271 + ( 1.0 / NominalU( Surface( surf ).Construction ) ) + 0.0810106 );
						} else {
							cNominalUwithConvCoeffs = "[invalid]";
						}
					} else {
						if ( NominalU( Surface( surf ).Construction ) > 0.0 ) {
							NominalUwithConvCoeffs = NominalU( Surface( surf ).Construction );
						} else {
							cNominalUwithConvCoeffs = "[invalid]";
						}
					}}
					if ( cNominalUwithConvCoeffs == "" ) {
						cNominalUwithConvCoeffs = RoundSigDigits( NominalUwithConvCoeffs, 3 );
					} else {
						cNominalUwithConvCoeffs = "[invalid]";
					}
					if ( ( Surface( surf ).Class == SurfaceClass_Window ) || ( Surface( surf ).Class == SurfaceClass_TDD_Dome ) ) {
						// SurfaceClass_Window also covers glass doors and TDD:Diffusers
						cNominalU = "N/A";
						if ( SurfaceWindow( surf ).SolarDiffusing ) {
							SolarDiffusing = "Yes";
						} else {
							SolarDiffusing = "No";
						}
					} else {
						cNominalU = RoundSigDigits( NominalU( Surface( surf ).Construction ), 3 );
					}
				} else {
					cNominalUwithConvCoeffs = "**";
					cNominalU = "**";
					ConstructionName = "**invalid**";
				}

				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7041, flags ) << ConstructionName << cNominalU << cNominalUwithConvCoeffs << SolarDiffusing << RoundSigDigits( Surface( surf ).Area, 2 ) << RoundSigDigits( Surface( surf ).GrossArea, 2 ) << RoundSigDigits( Surface( surf ).NetAreaShadowCalc, 2 ) << RoundSigDigits( Surface( surf ).Azimuth, 2 ) << RoundSigDigits( Surface( surf ).Tilt, 2 ) << RoundSigDigits( Surface( surf ).Width, 2 ) << RoundSigDigits( Surface( surf ).Height, 2 ) << RoundSigDigits( Surface( surf ).Reveal, 2 ); }
				if ( Surface( surf ).IntConvCoeff > 0 ) {
					{ auto const SELECT_CASE_var( UserIntConvectionCoeffs( Surface( surf ).IntConvCoeff ).OverrideType );
					if ( SELECT_CASE_var == ConvCoefValue ) {
						IntConvCoeffCalc = "User Supplied Value";
					} else if ( SELECT_CASE_var == ConvCoefSchedule ) {
						IntConvCoeffCalc = "User Supplied Schedule";
					} else if ( SELECT_CASE_var == ConvCoefUserCurve ) {
						ExtConvCoeffCalc = "User Supplied Curve";
					} else if ( SELECT_CASE_var == ConvCoefSpecifiedModel ) {
						ExtConvCoeffCalc = "User Specified Model";
					}}
				} else if ( Surface( surf ).IntConvCoeff < 0 ) { // not in use yet.
					IntConvCoeffCalc = ConvCoeffCalcs( std::abs( Surface( surf ).IntConvCoeff ) );
				}
				if ( Surface( surf ).ExtConvCoeff > 0 ) {
					{ auto const SELECT_CASE_var( UserExtConvectionCoeffs( Surface( surf ).ExtConvCoeff ).OverrideType );
					if ( SELECT_CASE_var == ConvCoefValue ) {
						ExtConvCoeffCalc = "User Supplied Value";
					} else if ( SELECT_CASE_var == ConvCoefSchedule ) {
						ExtConvCoeffCalc = "User Supplied Schedule";
					} else if ( SELECT_CASE_var == ConvCoefUserCurve ) {
						ExtConvCoeffCalc = "User Supplied Curve";
					} else if ( SELECT_CASE_var == ConvCoefSpecifiedModel ) {
						ExtConvCoeffCalc = "User Specified Model";
					}}
				} else if ( Surface( surf ).ExtConvCoeff < 0 ) {
					ExtConvCoeffCalc = ConvCoeffCalcs( std::abs( Surface( surf ).ExtConvCoeff ) );
				}
				if ( Surface( surf ).ExtBoundCond == ExternalEnvironment ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "ExternalEnvironment"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << ExtConvCoeffCalc; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << IntConvCoeffCalc; }
				} else if ( Surface( surf ).ExtBoundCond == Ground ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "Ground"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "N/A-Ground"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << IntConvCoeffCalc; }
				} else if ( Surface( surf ).ExtBoundCond == GroundFCfactorMethod ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "FCGround"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "N/A-FCGround"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << IntConvCoeffCalc; }
				} else if ( Surface( surf ).ExtBoundCond == OtherSideCoefNoCalcExt || Surface( surf ).ExtBoundCond == OtherSideCoefCalcExt ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << OSC( Surface( surf ).OSCPtr ).Name; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "N/A-OSC"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << IntConvCoeffCalc; }
				} else if ( Surface( surf ).ExtBoundCond == OtherSideCondModeledExt ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << OSCM( Surface( surf ).OSCMPtr ).Name; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "N/A-OSCM"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << IntConvCoeffCalc; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << Surface( surf ).ExtBoundCondName; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "Other/Same Surface Int Conv"; }
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << IntConvCoeffCalc; }
				}
				if ( Surface( surf ).ExtSolar ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "SunExposed"; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "NoSun"; }
				}
				if ( Surface( surf ).ExtWind ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "WindExposed"; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_705, flags ) << "NoWind"; }
				}
				if ( RptType == 10 ) {
					gio::write( unit, Format_706 ) << RoundSigDigits( Surface( surf ).ViewFactorGround, 2 ) << RoundSigDigits( Surface( surf ).ViewFactorSky, 2 ) << RoundSigDigits( Surface( surf ).ViewFactorGroundIR, 2 ) << RoundSigDigits( Surface( surf ).ViewFactorSkyIR, 2 ) << TrimSigDigits( Surface( surf ).Sides );
				} else {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_706, flags ) << RoundSigDigits( Surface( surf ).ViewFactorGround, 2 ) << RoundSigDigits( Surface( surf ).ViewFactorSky, 2 ) << RoundSigDigits( Surface( surf ).ViewFactorGroundIR, 2 ) << RoundSigDigits( Surface( surf ).ViewFactorSkyIR, 2 ) << TrimSigDigits( Surface( surf ).Sides ); }
					for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
						if ( vert != Surface( surf ).Sides ) {
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_709, flags ) << RoundSigDigits( Surface( surf ).Vertex( vert ).x, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).y, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).z, 2 ); }
						} else {
							gio::write( unit, Format_709 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).x, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).y, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).z, 2 );
						}
					}
					if ( Surface( surf ).Sides == 0 ) gio::write( unit, Format_711 );
				}
				// if window, report frame/divider as appropriate
				if ( Surface( surf ).FrameDivider > 0 ) {
					fd = Surface( surf ).FrameDivider;
					if ( FrameDivider( fd ).FrameWidth > 0.0 ) {
						{ auto const SELECT_CASE_var( Surface( surf ).HeatTransferAlgorithm );
						if ( SELECT_CASE_var == HeatTransferModel_None ) {
							AlgoName = "None";
						} else if ( SELECT_CASE_var == HeatTransferModel_CTF ) {
							AlgoName = "CTF - ConductionTransferFunction";
						} else if ( SELECT_CASE_var == HeatTransferModel_CondFD ) {
							AlgoName = "CondFD - ConductionFiniteDifference";
						} else if ( SELECT_CASE_var == HeatTransferModel_EMPD ) {
							AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
						} else if ( SELECT_CASE_var == HeatTransferModel_HAMT ) {
							AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
						} else if ( SELECT_CASE_var == HeatTransferModel_Window5 ) {
							AlgoName = "Window5 Detailed Fenestration";
						} else if ( SELECT_CASE_var == HeatTransferModel_ComplexFenestration ) {
							AlgoName = "Window7 Complex Fenestration";
						} else if ( SELECT_CASE_var == HeatTransferModel_TDD ) {
							AlgoName = "Tubular Daylighting Device";
						}}
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_704, flags ) << "Frame/Divider" << FrameDivider( fd ).Name << "Frame" << Surface( surf ).Name << AlgoName; }
						gio::write( unit, Format_7045 ) << RoundSigDigits( SurfaceWindow( surf ).FrameArea, 2 ) << RoundSigDigits( SurfaceWindow( surf ).FrameArea / Surface( surf ).Multiplier, 2 ) << "*" << "N/A" << "N/A" << RoundSigDigits( FrameDivider( fd ).FrameWidth, 2 ) << "N/A";
					}
					if ( FrameDivider( fd ).DividerWidth > 0.0 ) {
						if ( FrameDivider( fd ).DividerType == DividedLite ) {
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_704, flags ) << "Frame/Divider" << FrameDivider( fd ).Name << "Divider:DividedLite" << Surface( surf ).Name; }
						} else {
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_704, flags ) << "Frame/Divider" << FrameDivider( fd ).Name << "Divider:Suspended" << Surface( surf ).Name; }
						}
						gio::write( unit, Format_7045 ) << RoundSigDigits( SurfaceWindow( surf ).DividerArea, 2 ) << RoundSigDigits( SurfaceWindow( surf ).DividerArea / Surface( surf ).Multiplier, 2 ) << "*" << "N/A" << "N/A" << RoundSigDigits( FrameDivider( fd ).DividerWidth, 2 ) << "N/A";
					}
				}
			} else { // RptType=1  Vertices only
				if ( Surface( surf ).BaseSurf == surf ) {
					BaseSurfName = "";
				} else {
					BaseSurfName = Surface( surf ).BaseSurfName;
				}
				{ auto const SELECT_CASE_var( Surface( surf ).HeatTransferAlgorithm );
				if ( SELECT_CASE_var == HeatTransferModel_None ) {
					AlgoName = "None";
				} else if ( SELECT_CASE_var == HeatTransferModel_CTF ) {
					AlgoName = "CTF - ConductionTransferFunction";
				} else if ( SELECT_CASE_var == HeatTransferModel_CondFD ) {
					AlgoName = "CondFD - ConductionFiniteDifference";
				} else if ( SELECT_CASE_var == HeatTransferModel_EMPD ) {
					AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
				} else if ( SELECT_CASE_var == HeatTransferModel_HAMT ) {
					AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
				} else if ( SELECT_CASE_var == HeatTransferModel_Window5 ) {
					AlgoName = "Window5 Detailed Fenestration";
				} else if ( SELECT_CASE_var == HeatTransferModel_ComplexFenestration ) {
					AlgoName = "Window7 Complex Fenestration";
				} else if ( SELECT_CASE_var == HeatTransferModel_TDD ) {
					AlgoName = "Tubular Daylighting Device";
				}}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_704, flags ) << "HeatTransfer" << Surface( surf ).Name << cSurfaceClass( Surface( surf ).Class ) << BaseSurfName << AlgoName; }
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_7042, flags ) << TrimSigDigits( Surface( surf ).Sides ); }
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					if ( vert != Surface( surf ).Sides ) {
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_709, flags ) << RoundSigDigits( Surface( surf ).Vertex( vert ).x, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).y, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).z, 2 ); }
					} else {
						gio::write( unit, Format_709 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).x, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).y, 2 ) << RoundSigDigits( Surface( surf ).Vertex( vert ).z, 2 );
					}
				}
				if ( Surface( surf ).Sides == 0 ) gio::write( unit, Format_711 );
			}
		} // surfaces
	} // zones

}

void
CostInfoOut()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   April 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine produces a file with information about surfaces.
	// for the purpose of producing first cost estimates to include in objective value functions
	// for design optimization

	// METHODOLOGY EMPLOYED:
	// Access data in DataSurfaces and report

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int unit; // Unit number on which to write file
	int surf; // Loop variable for surfaces
	Array1D_bool uniqueSurf;
	int write_stat;

	// Formats
	static gio::Fmt Format_801( "(I5,',',A,',',A,',',A,',',f14.5,',',f14.5)" );

	if ( TotSurfaces > 0 && ! allocated( Surface ) ) {
		// no error needed, probably in end processing, just return
		return;
	}

	// need to determine unique surfacs... some surfaces are shared by zones and hence doubled
	uniqueSurf.dimension( TotSurfaces, true );

	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).ExtBoundCond > 0 ) {
			if ( Surface( surf ).ExtBoundCond < surf ) { //already cycled through
				uniqueSurf( surf ) = false;
			}

		}
		if ( Surface( surf ).Construction == 0 ) { //throw out others for now
			uniqueSurf( surf ) = false;
		}
	}

	unit = GetNewUnitNumber();
	// .sci = surface cost info
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( unit, DataStringGlobals::outputSciFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		ShowFatalError( "CostInfoOut: Could not open file "+DataStringGlobals::outputSciFileName+" for output (write)." );
	}
	gio::write( unit, fmtLD ) << TotSurfaces << int( count( uniqueSurf ) );
	gio::write( unit, fmtLD ) << "data for surfaces useful for cost information";
	gio::write( unit, fmtLD ) << "Number, Name, Construction, class, area, grossarea";

	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		//if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
		if ( ! uniqueSurf( surf ) ) continue;
		// why the heck are constructions == 0 ?
		if ( Surface( surf ).Construction != 0 ) {
			gio::write( unit, Format_801 ) << surf << Surface( surf ).Name << Construct( Surface( surf ).Construction ).Name << cSurfaceClass( Surface( surf ).Class ) << Surface( surf ).Area << Surface( surf ).GrossArea;
		}

	}

	gio::close( unit );

	uniqueSurf.deallocate();

}

void
VRMLOut(
	std::string & PolygonAction,
	std::string & ColorScheme
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   August 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine produces a file of VRML output for the surfaces.

	// METHODOLOGY EMPLOYED:
	// Use the surface absolute coordinate information to produce
	// lines.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataHeatBalance::BuildingName;
	using DataHeatBalance::Zone;
	using namespace DataSurfaces;
	using DataDaylighting::ZoneDaylight;
	using DataGlobals::NumOfZones;
	using DataStringGlobals::VerString;
	using namespace DXFEarClipping;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	static Array1D_string const colorstring( 7, { "WALL", "WINDOW", "FIXEDSHADE", "SUBSHADE", "ROOF", "FLOOR", "BLDGSHADE" } );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int unit; // Unit number on which to write file
	int surf; // Loop variable for surfaces
	int vert; // Loop counter
	int colorindex; // color index by surface type
	//  REAL(r64) minx                 ! minimum x in surface data
	//  REAL(r64) miny                 ! minimum y in surface data
	//  REAL(r64) minz                 ! minimum z in surface data (for polygon output)
	int zones; // loop counter for zone loop
	std::string ZoneNum;
	std::string TempZoneName;
	std::string::size_type pos;
	std::string ShadeType;
	static bool ThickPolyline( false );
	static bool RegularPolyline( false );
	static std::string PolylineWidth( " 0.55" );
	static bool TriangulateFace( false );
	int ntri;
	int svert;
	int vv0;
	int vv1;
	int vv2;
	std::string csurfnumber;
	std::string csidenumber;
	int write_stat;

	// Object Data
	Array1D< dTriangle > mytriangles;

	// Formats
	static gio::Fmt Format_702( "('#VRML V2.0 utf8')" );
	static gio::Fmt Format_707( "('WorldInfo {',/,3X,'title \"Building - ',A,'\"',/,3X,'info [\"EnergyPlus Program Version ',A,'\"]',/,3X,'info [\"Surface Color Scheme ',A,'\"]',/,'}')" );
	static gio::Fmt Format_800( "('Shape {',/,'appearance DEF ',A,' Appearance {',/,'material Material { diffuseColor ',A,' }',/,'}',/,'}')" );
	static gio::Fmt Format_801( "('Shape {',/,'appearance USE ',A,/,'geometry IndexedFaceSet {',/,'solid TRUE',/,'coord DEF ',A,' Coordinate {',/,'point [')" );
	static gio::Fmt Format_802( "(F15.5,1X,F15.5,1X,F15.5,',')" );
	static gio::Fmt Format_803( "(']',/,'}',/,'coordIndex [')" );
	static gio::Fmt Format_804( "(A)" );
	static gio::Fmt Format_805( "(']',/,'ccw TRUE',/,'solid TRUE',/,'}',/,'}')" );
	static gio::Fmt Format_710( "(A)" );

	if ( PolygonAction == "TRIANGULATE3DFACE" || PolygonAction == "TRIANGULATE" ) {
		TriangulateFace = true;
	} else if ( PolygonAction == "THICKPOLYLINE" || PolygonAction == "" ) {
		ThickPolyline = true;
	} else if ( PolygonAction == "REGULARPOLYLINE" ) {
		RegularPolyline = true;
		PolylineWidth = " 0";
	} else {
		ShowWarningError( "VRMLOut: Illegal key specified for Surfaces with > 4 sides=" + PolygonAction );
		ShowContinueError( "\"TRIANGULATE 3DFACE\" will be used for any surfaces with > 4 sides." );
		TriangulateFace = true;
	}

	if ( TotSurfaces > 0 && ! allocated( Surface ) ) {
		// no error needed, probably in end processing, just return
		return;
	}

	unit = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( unit, DataStringGlobals::outputWrlFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		ShowFatalError( "VRMLOut: Could not open file "+ DataStringGlobals::outputWrlFileName +" for output (write)." );
	}

	gio::write( unit, Format_702 ); // Beginning

	if ( ColorScheme == "" ) {
		gio::write( unit, Format_707 ) << BuildingName << VerString << "Default"; // World Info
	} else {
		gio::write( unit, Format_707 ) << BuildingName << VerString << ColorScheme; // World Info
	}

	gio::write( unit, Format_710 ) << "# Zone Names";
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		gio::write( ZoneNum, fmtLD ) << zones;
		strip( ZoneNum );
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		gio::write( unit, Format_710 ) << "# Zone=" + ZoneNum + ':' + TempZoneName;
	}

	// Define the colors:

	gio::write( unit, Format_800 ) << "FLOOR" << "0.502 0.502 0.502";
	gio::write( unit, Format_800 ) << "ROOF" << "1 1 0";
	gio::write( unit, Format_800 ) << "WALL" << "0 1 0";
	gio::write( unit, Format_800 ) << "WINDOW" << "0 1 1";
	gio::write( unit, Format_800 ) << "DOOR" << "0 1 1";
	gio::write( unit, Format_800 ) << "GLASSDOOR" << "0 1 1";
	gio::write( unit, Format_800 ) << "FIXEDSHADE" << "1 0 1";
	gio::write( unit, Format_800 ) << "BLDGSHADE" << "0 0 1";
	gio::write( unit, Format_800 ) << "SUBSHADE" << "1 0 1";
	gio::write( unit, Format_800 ) << "BACKCOLOR" << "0.502 0.502 0.784";

	//  Do all detached shading surfaces first
	for ( surf = 1; surf <= TotSurfaces; ++surf ) {
		if ( Surface( surf ).HeatTransSurf ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Shading ) continue;
		if ( Surface( surf ).Sides == 0 ) continue;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) colorindex = 3;
		if ( Surface( surf ).Class == SurfaceClass_Detached_B ) colorindex = 7;
		if ( Surface( surf ).Class == SurfaceClass_Detached_F ) {
			ShadeType = "Fixed Shading";
			gio::write( unit, Format_710 ) << "# Fixed Shading:" + Surface( surf ).Name;
		} else if ( Surface( surf ).Class == SurfaceClass_Detached_B ) {
			ShadeType = "Building Shading";
			gio::write( unit, Format_710 ) << "# Building Shading:" + Surface( surf ).Name;
		}
		gio::write( csurfnumber, fmtLD ) << surf;
		strip( csurfnumber );
		gio::write( unit, Format_801 ) << colorstring( colorindex ) << "Surf" + csurfnumber;
		for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
			gio::write( unit, Format_802 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
		}
		gio::write( unit, Format_803 );
		if ( Surface( surf ).Sides <= 4 || ! TriangulateFace ) {
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				gio::write( csidenumber, fmtLD ) << vert - 1;
				strip( csidenumber );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
				if ( vert == Surface( surf ).Sides ) gio::write( unit, Format_804 ) << " -1";
			}
			gio::write( unit, Format_805 );
		} else { // will be >4 sided polygon with triangulate option
			ntri = Triangulate( Surface( surf ).Sides, Surface( surf ).Vertex, mytriangles, Surface( surf ).Azimuth, Surface( surf ).Tilt, Surface( surf ).Name, Surface( surf ).Class );
			for ( svert = 1; svert <= ntri; ++svert ) {
				vv0 = mytriangles( svert ).vv0;
				gio::write( csidenumber, fmtLD ) << vv0 - 1;
				strip( csidenumber );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
				vv1 = mytriangles( svert ).vv1;
				gio::write( csidenumber, fmtLD ) << vv1 - 1;
				strip( csidenumber );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
				vv2 = mytriangles( svert ).vv2;
				gio::write( csidenumber, fmtLD ) << vv2 - 1;
				strip( csidenumber );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
				gio::write( unit, Format_804 ) << " -1";
			}
			gio::write( unit, Format_805 );
			mytriangles.deallocate();
		}
	}
	//  ! now do zone surfaces, by zone
	for ( zones = 1; zones <= NumOfZones; ++zones ) {
		TempZoneName = Zone( zones ).Name;
		pos = index( TempZoneName, ' ' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ' ' );
		}
		pos = index( TempZoneName, ':' );
		while ( pos != std::string::npos ) {
			TempZoneName[ pos ] = '_';
			pos = index( TempZoneName, ':' );
		}
		for ( surf = max( Zone( zones ).SurfaceFirst, 1 ); surf <= Zone( zones ).SurfaceLast; ++surf ) {
			if ( Surface( surf ).Sides == 0 ) continue;
			if ( Surface( surf ).Class == SurfaceClass_IntMass ) continue;
			if ( Surface( surf ).Class == SurfaceClass_Wall ) colorindex = 1;
			if ( Surface( surf ).Class == SurfaceClass_Roof ) colorindex = 5;
			if ( Surface( surf ).Class == SurfaceClass_TDD_Dome ) colorindex = 2;
			if ( Surface( surf ).Class == SurfaceClass_Floor ) colorindex = 6;
			if ( Surface( surf ).Class == SurfaceClass_Window ) colorindex = 2;
			if ( Surface( surf ).Class == SurfaceClass_Door ) colorindex = 2;
			gio::write( csurfnumber, fmtLD ) << surf;
			strip( csurfnumber );
			gio::write( unit, Format_710 ) << "# " + Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			gio::write( unit, Format_801 ) << colorstring( colorindex ) << "Surf" + csurfnumber;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				gio::write( unit, Format_802 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
			}
			gio::write( unit, Format_803 );
			if ( Surface( surf ).Sides <= 4 || ! TriangulateFace ) {
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					gio::write( csidenumber, fmtLD ) << vert - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					if ( vert == Surface( surf ).Sides ) gio::write( unit, Format_804 ) << " -1";
				}
				gio::write( unit, Format_805 );
			} else { // will be >4 sided polygon with triangulate option
				ntri = Triangulate( Surface( surf ).Sides, Surface( surf ).Vertex, mytriangles, Surface( surf ).Azimuth, Surface( surf ).Tilt, Surface( surf ).Name, Surface( surf ).Class );
				for ( svert = 1; svert <= ntri; ++svert ) {
					vv0 = mytriangles( svert ).vv0;
					gio::write( csidenumber, fmtLD ) << vv0 - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					vv1 = mytriangles( svert ).vv1;
					gio::write( csidenumber, fmtLD ) << vv1 - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					vv2 = mytriangles( svert ).vv2;
					gio::write( csidenumber, fmtLD ) << vv2 - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					gio::write( unit, Format_804 ) << " -1";
				}
				gio::write( unit, Format_805 );
				mytriangles.deallocate();
			}
		}
		// still have to do shading surfaces for zone
		colorindex = 4;
		for ( surf = 1; surf <= TotSurfaces; ++surf ) {
			//      !if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
			if ( Surface( surf ).Class != SurfaceClass_Shading ) continue;
			if ( Surface( surf ).ZoneName != Zone( zones ).Name ) continue;
			if ( Surface( surf ).Sides == 0 ) continue;
			gio::write( unit, Format_710 ) << "# " + Surface( surf ).ZoneName + ':' + Surface( surf ).Name;
			gio::write( unit, Format_801 ) << colorstring( colorindex ) << "Surf" + csurfnumber;
			for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
				gio::write( unit, Format_802 ) << Surface( surf ).Vertex( vert ).x << Surface( surf ).Vertex( vert ).y << Surface( surf ).Vertex( vert ).z;
			}
			gio::write( unit, Format_803 );
			if ( Surface( surf ).Sides <= 4 || ! TriangulateFace ) {
				for ( vert = 1; vert <= Surface( surf ).Sides; ++vert ) {
					gio::write( csidenumber, fmtLD ) << vert - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					if ( vert == Surface( surf ).Sides ) gio::write( unit, Format_804 ) << " -1";
				}
				gio::write( unit, Format_805 );
			} else { // will be >4 sided polygon with triangulate option
				ntri = Triangulate( Surface( surf ).Sides, Surface( surf ).Vertex, mytriangles, Surface( surf ).Azimuth, Surface( surf ).Tilt, Surface( surf ).Name, Surface( surf ).Class );
				for ( svert = 1; svert <= ntri; ++svert ) {
					vv0 = mytriangles( svert ).vv0;
					gio::write( csidenumber, fmtLD ) << vv0 - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					vv1 = mytriangles( svert ).vv1;
					gio::write( csidenumber, fmtLD ) << vv1 - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					vv2 = mytriangles( svert ).vv2;
					gio::write( csidenumber, fmtLD ) << vv2 - 1;
					strip( csidenumber );
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( unit, Format_804, flags ) << ' ' + csidenumber; }
					gio::write( unit, Format_804 ) << " -1";
				}
				gio::write( unit, Format_805 );
				mytriangles.deallocate();
			}
		}
	}

	// vrml does not have daylighting reference points included

	gio::close( unit );

}


} // EnergyPlus
