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
