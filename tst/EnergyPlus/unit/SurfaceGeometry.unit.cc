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

// EnergyPlus::SurfaceGeometry Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/HeatBalanceManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::SurfaceGeometry;
using namespace EnergyPlus::HeatBalanceManager;
//using namespace ObjexxFCL;


TEST_F( EnergyPlusFixture, BaseSurfaceRectangularTest )
{

	// Test base surfaces for rectangular shape in ProcessSurfaceVertices

	TotSurfaces = 5;
	MaxVerticesPerSurface = 5;
	Surface.allocate( TotSurfaces );
	ShadeV.allocate( TotSurfaces );
	for ( int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
		Surface( SurfNum ).Vertex.allocate( MaxVerticesPerSurface );
	}

	bool ErrorsFound( false );
	int ThisSurf( 0 );

	// Surface 1 - Rectangle
	ThisSurf = 1;
	Surface( ThisSurf ).Azimuth = 180.0;
	Surface( ThisSurf ).Tilt = 90.0;
	Surface( ThisSurf ).Sides = 4;
	Surface( ThisSurf ).GrossArea = 10.0;

	Surface( ThisSurf ).Vertex( 1 ).x = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 2 ).x = 5.0;
	Surface( ThisSurf ).Vertex( 2 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 2 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 3 ).x = 5.0;
	Surface( ThisSurf ).Vertex( 3 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 3 ).z = 2.0;

	Surface( ThisSurf ).Vertex( 4 ).x = 0.0;
	Surface( ThisSurf ).Vertex( 4 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 4 ).z = 2.0;

	ProcessSurfaceVertices( ThisSurf, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( Rectangle, Surface( ThisSurf ).Shape );

	// Surface 2 - Isosceles Trapezoid
	ThisSurf = 2;
	Surface( ThisSurf ).Azimuth = 180.0;
	Surface( ThisSurf ).Tilt = 90.0;
	Surface( ThisSurf ).Sides = 4;
	Surface( ThisSurf ).GrossArea = 8.0;

	Surface( ThisSurf ).Vertex( 1 ).x = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 2 ).x = 5.0;
	Surface( ThisSurf ).Vertex( 2 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 2 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 3 ).x = 4.0;
	Surface( ThisSurf ).Vertex( 3 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 3 ).z = 2.0;

	Surface( ThisSurf ).Vertex( 4 ).x = 1.0;
	Surface( ThisSurf ).Vertex( 4 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 4 ).z = 2.0;

	ProcessSurfaceVertices( ThisSurf, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( Quadrilateral, Surface( ThisSurf ).Shape );

	// Surface 3 - Parallelogram
	ThisSurf = 3;
	Surface( ThisSurf ).Azimuth = 180.0;
	Surface( ThisSurf ).Tilt = 90.0;
	Surface( ThisSurf ).Sides = 4;
	Surface( ThisSurf ).GrossArea = 10.0;

	Surface( ThisSurf ).Vertex( 1 ).x = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 2 ).x = 5.0;
	Surface( ThisSurf ).Vertex( 2 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 2 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 3 ).x = 7.0;
	Surface( ThisSurf ).Vertex( 3 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 3 ).z = 2.0;

	Surface( ThisSurf ).Vertex( 4 ).x = 2.0;
	Surface( ThisSurf ).Vertex( 4 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 4 ).z = 2.0;

	ProcessSurfaceVertices( ThisSurf, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( Quadrilateral, Surface( ThisSurf ).Shape );

	// Surface 4 - Triangle
	ThisSurf = 4;
	Surface( ThisSurf ).Azimuth = 180.0;
	Surface( ThisSurf ).Tilt = 90.0;
	Surface( ThisSurf ).Sides = 3;
	Surface( ThisSurf ).GrossArea = 10.0;

	Surface( ThisSurf ).Vertex( 1 ).x = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 2 ).x = 5.0;
	Surface( ThisSurf ).Vertex( 2 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 2 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 3 ).x = 0.0;
	Surface( ThisSurf ).Vertex( 3 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 3 ).z = 2.0;

	ProcessSurfaceVertices( ThisSurf, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( Triangle, Surface( ThisSurf ).Shape );

	// Surface 5 - Polygon
	ThisSurf = 5;
	Surface( ThisSurf ).Azimuth = 180.0;
	Surface( ThisSurf ).Tilt = 90.0;
	Surface( ThisSurf ).Sides = 5;
	Surface( ThisSurf ).GrossArea = 10.0;

	Surface( ThisSurf ).Vertex( 1 ).x = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 1 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 2 ).x = 5.0;
	Surface( ThisSurf ).Vertex( 2 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 2 ).z = 0.0;

	Surface( ThisSurf ).Vertex( 3 ).x = 7.0;
	Surface( ThisSurf ).Vertex( 3 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 3 ).z = 2.0;

	Surface( ThisSurf ).Vertex( 4 ).x = 3.0;
	Surface( ThisSurf ).Vertex( 4 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 4 ).z = 5.0;

	Surface( ThisSurf ).Vertex( 5 ).x = 1.0;
	Surface( ThisSurf ).Vertex( 5 ).y = 0.0;
	Surface( ThisSurf ).Vertex( 5 ).z = 3.0;

	ProcessSurfaceVertices( ThisSurf, ErrorsFound );
	EXPECT_FALSE( ErrorsFound );
	EXPECT_EQ( Polygonal, Surface( ThisSurf ).Shape );
}

TEST_F( EnergyPlusFixture, ConfirmCheckSubSurfAzTiltNorm )
{
	SurfaceData BaseSurface;
	SurfaceData SubSurface;
	bool surfaceError;

	//Case 1 - Base surface and subsurface face the same way - should be no error message and no surfaceError
	surfaceError = false;
	BaseSurface.Azimuth = 0.;
	BaseSurface.Tilt = 0.;
	BaseSurface.NewellSurfaceNormalVector.x = 0.;
	BaseSurface.NewellSurfaceNormalVector.y = 0.;
	BaseSurface.NewellSurfaceNormalVector.z = 1.;

	SubSurface.Azimuth = 0.;
	SubSurface.Tilt = 0.;
	SubSurface.NewellSurfaceNormalVector.x = 0.;
	SubSurface.NewellSurfaceNormalVector.y = 0.;
	SubSurface.NewellSurfaceNormalVector.z = 1.;
	checkSubSurfAzTiltNorm( BaseSurface, SubSurface, surfaceError );
	EXPECT_FALSE( surfaceError );
	EXPECT_FALSE( has_err_output() );

	//Case 2 - Base surface and subsurface face the opposite way - should be error message and surfaceError=true
	surfaceError = false;
	SubSurface.Azimuth = 180.;
	SubSurface.Tilt = 180.;
	SubSurface.NewellSurfaceNormalVector.x = 1.;
	SubSurface.NewellSurfaceNormalVector.y = 0.;
	SubSurface.NewellSurfaceNormalVector.z = 0.;
	checkSubSurfAzTiltNorm( BaseSurface, SubSurface, surfaceError );
	EXPECT_TRUE( surfaceError );
	EXPECT_TRUE( has_err_output() );

	//Case 3 - Base surface is horizontal and subsurface is different by 45 degrees azimuth - should be no warning message and surfaceError=false
	surfaceError = false;
	SubSurface.Azimuth = 45.;
	SubSurface.Tilt = 0.;
	SubSurface.NewellSurfaceNormalVector.x = 0.;
	SubSurface.NewellSurfaceNormalVector.y = 1.; // This doesn't match the tilt and azimuth, but want it to be different so tilt and azimuth tests are executed
	SubSurface.NewellSurfaceNormalVector.z = 1.;
	checkSubSurfAzTiltNorm( BaseSurface, SubSurface, surfaceError );
	EXPECT_FALSE( surfaceError );
	EXPECT_FALSE( has_err_output() );

	//Case 4 - Base surface is not horizontal and subsurface is different by 45 degrees azimuth and tilt - should be warning error message but surfaceError=false
	surfaceError = false;
	BaseSurface.Azimuth = 90.;
	BaseSurface.Tilt = 90.;
	BaseSurface.NewellSurfaceNormalVector.x = 1.;
	BaseSurface.NewellSurfaceNormalVector.y = 0.;
	BaseSurface.NewellSurfaceNormalVector.z = 0.;

	SubSurface.Azimuth = 45.;
	SubSurface.Tilt = 45.;
	SubSurface.NewellSurfaceNormalVector.x = 1.;
	SubSurface.NewellSurfaceNormalVector.y = 1.;
	SubSurface.NewellSurfaceNormalVector.z = 1.;
	checkSubSurfAzTiltNorm( BaseSurface, SubSurface, surfaceError );
	EXPECT_FALSE( surfaceError );
	EXPECT_TRUE( has_err_output() );

}

TEST_F( EnergyPlusFixture, SurfaceGeometry_MakeMirrorSurface )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"BuildingSurface:Detailed,",
		" FRONT-1,                  !- Name",
		" WALL,                     !- Surface Type",
		" INT-WALL-1,               !- Construction Name",
		" Space,                    !- Zone Name",
		" Outdoors,                 !- Outside Boundary Condition",
		" ,                         !- Outside Boundary Condition Object",
		" SunExposed,               !- Sun Exposure",
		" WindExposed,              !- Wind Exposure",
		" 0.50000,                  !- View Factor to Ground",
		" 4,                        !- Number of Vertices",
		" 0.0, 0.0, 2.4,            !- X, Y, Z == > Vertex 1 {m}",
		" 0.0, 0.0, 0.0,            !- X, Y, Z == > Vertex 2 {m}",
		" 30.5, 0.0, 0.0,           !- X, Y, Z == > Vertex 3 {m}",
		" 30.5, 0.0, 2.4;           !- X, Y, Z == > Vertex 4 {m}",
		" ",
		"Zone,",
		"  Space,                   !- Name",
		"  0.0000,                  !- Direction of Relative North {deg}",
		"  0.0000,                  !- X Origin {m}",
		"  0.0000,                  !- Y Origin {m}",
		"  0.0000,                  !- Z Origin {m}",
		"  1,                       !- Type",
		"  1,                       !- Multiplier",
		"  2.0,                     !- Ceiling Height {m}",
		"  ,                        !- Volume {m3}",
		"  autocalculate,           !- Floor Area {m2}",
		"  ,                        !- Zone Inside Convection Algorithm",
		"  ,                        !- Zone Outside Convection Algorithm",
		"  Yes;                     !- Part of Total Floor Area",
		" ",
		"Construction,",
		" INT-WALL-1,               !- Name",
		" GP02;                     !- Outside Layer",
		" ",
		"Material,",
		" GP02,                     !- Name",
		" MediumSmooth,             !- Roughness",
		" 1.5900001E-02,            !- Thickness{ m }",
		" 0.1600000,                !- Conductivity{ W / m - K }",
		" 801.0000,                 !- Density{ kg / m3 }",
		" 837.0000,                 !- Specific Heat{ J / kg - K }",
		" 0.9000000,                !- Thermal Absorptance",
		" 0.7500000,                !- Solar Absorptance",
		" 0.7500000;                !- Visible Absorptance",
		" ",
		"  Timestep, 4;",
		" ",
		"BUILDING, Bldg2, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
		" ",
		"SimulationControl, YES, NO, NO, YES, NO;",
		" ",
		"  Site:Location,",
		"    Miami Intl Ap FL USA TMY3 WMO=722020E,    !- Name",
		"    25.82,                   !- Latitude {deg}",
		"    -80.30,                  !- Longitude {deg}",
		"    -5.00,                   !- Time Zone {hr}",
		"    11;                      !- Elevation {m}",
		" ",


	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	bool FoundError = false;
	GetMaterialData( FoundError );
	GetConstructData( FoundError );
	GetZoneData( FoundError ); // Read Zone data from input file
	HeatTransferAlgosUsed.allocate( 1 );
	HeatTransferAlgosUsed( 1 ) = OverallHeatTransferSolutionAlgo;
	SetupZoneGeometry( FoundError ); // this calls GetSurfaceData()

	//compare_err_stream( "" ); // just for debugging

	EXPECT_FALSE( FoundError );

	// test coordinate on existing surface
	EXPECT_EQ( TotSurfaces, 1 );

	EXPECT_EQ( Surface( TotSurfaces ).Name, "FRONT-1" );

	// move surface to SurfaceTmp since MakeMirrorSurface uses that array
	SurfaceTmp.allocate( 10 );
	SurfaceTmp( TotSurfaces ) = Surface( TotSurfaces );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Name, "FRONT-1" );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 1 ).x, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 1 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 1 ).z, 2.4 );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 2 ).x, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 2 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 2 ).z, 0. );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 3 ).x, 30.5 );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 3 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 3 ).z, 0. );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 4 ).x, 30.5 );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 4 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 4 ).z, 2.4 );

	MakeMirrorSurface( TotSurfaces ); 	// This call increments TotSurfaces so the references after this are for the created mirror surface

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Name, "Mir-FRONT-1" );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 1 ).x, 30.5 );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 1 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 1 ).z, 2.4 );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 2 ).x, 30.5 );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 2 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 2 ).z, 0. );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 3 ).x, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 3 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 3 ).z, 0. );

	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 4 ).x, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 4 ).y, 0. );
	EXPECT_EQ( SurfaceTmp( TotSurfaces ).Vertex( 4 ).z, 2.4 );

}



