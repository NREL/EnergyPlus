// EnergyPlus::SurfaceGeometry Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::SurfaceGeometry;
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
