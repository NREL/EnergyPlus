// EnergyPlus::SurfaceOctree Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/SurfaceOctree.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array3D.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::SurfaceGeometry;
using namespace ObjexxFCL;
using Vertex = SurfaceOctreeCube::Vertex;

TEST( SurfaceOctreeTest, Basic )
{
	// Vertices
	Array3D< Vertex > vertices( {0,1}, {0,1}, {0,1} );
	for ( int i = 0; i <= 1; ++i ) {
		for ( int j = 0; j <= 1; ++j ) {
			for ( int k = 0; k <= 1; ++k ) {
				vertices(i,j,k) = Vertex(i,j,k);
			}
		}
	}

	// Surfaces: Simple Unit Cube
	TotSurfaces = 6;
	SurfaceData surface;
	surface.Area = 1.0;
	surface.Sides = 4;
	surface.Vertex.dimension( 4 );
	Surface.dimension( TotSurfaces, surface );
	Surface( 1 ).Vertex = { vertices(0,0,0), vertices(1,0,0), vertices(1,0,1), vertices(0,0,1) };
	Surface( 2 ).Vertex = { vertices(0,1,0), vertices(1,1,0), vertices(1,1,1), vertices(0,1,1) };
	Surface( 3 ).Vertex = { vertices(0,0,0), vertices(0,1,0), vertices(0,1,1), vertices(0,0,1) };
	Surface( 4 ).Vertex = { vertices(1,0,0), vertices(1,1,0), vertices(1,1,1), vertices(1,0,1) };
	Surface( 5 ).Vertex = { vertices(0,0,0), vertices(1,0,0), vertices(1,1,0), vertices(0,1,0) };
	Surface( 6 ).Vertex = { vertices(0,0,1), vertices(1,0,1), vertices(1,1,1), vertices(0,1,1) };
	CalcSurfaceCentroid();

	// Surface octree
	SurfaceOctreeCube const cube( Surface );

	EXPECT_EQ( Vertex( 0.0, 0.0, 0.0 ), cube.l() );
	EXPECT_EQ( Vertex( 0.5, 0.5, 0.5 ), cube.c() );
	EXPECT_EQ( Vertex( 1.0, 1.0, 1.0 ), cube.u() );
	EXPECT_EQ( 1.0, cube.w() );

	// Line/segment/ray intersections with cube enclosing sphere
	{ // Diagonal
		Vertex const a( 0.0 ), b( 1.0 ), dir( ( b - a ).normalize_zero() );
		EXPECT_TRUE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_TRUE( cube.lineIntersectsSphere( a, dir ) );
	}
	{ // Center up half-diagonal
		Vertex const a( 0.5 ), b( 2.0 ), dir( ( b - a ).normalize_zero() );
		EXPECT_TRUE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_TRUE( cube.lineIntersectsSphere( a, dir ) );
	}
	{ // Outward at corner
		Vertex const a( 1.0 ), b( 2.0 ), dir( ( b - a ).normalize_zero() );
		EXPECT_TRUE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_TRUE( cube.lineIntersectsSphere( a, dir ) );
	}
	{ // Tangent at origin
		Vertex const a( -1.0+1.0e-12, 1.0+1.0e-12, 0.0 ), b( 1.0+1.0e-12, -1.0+1.0e-12, 0.0 ), dir( ( b - a ).normalize_zero() );
		EXPECT_TRUE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_TRUE( cube.lineIntersectsSphere( a, dir ) );
	}
	{ // Inward outside but pointing to corner
		Vertex const a( 3.0 ), b( 2.0 ), dir( ( b - a ).normalize_zero() );
		EXPECT_FALSE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_TRUE( cube.lineIntersectsSphere( a, dir ) );
	}
	{ // Just beyond corner
		Vertex const a( 1.0000001 ), b( 2.0000002 ), dir( ( b - a ).normalize_zero() );
		EXPECT_FALSE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_FALSE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_TRUE( cube.lineIntersectsSphere( a, dir ) );
	}
	{ // Way outside cube but along diagonal
		Vertex const a( 5.0 ), b( 9.0 ), dir( ( b - a ).normalize_zero() );
		EXPECT_FALSE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_FALSE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_TRUE( cube.lineIntersectsSphere( a, dir ) );
	}
	{ // Way outside cube with no contact
		Vertex const a( 9.0 ), b( 12.0, 8.0, 4.0 ), dir( ( b - a ).normalize_zero() );
		EXPECT_FALSE( cube.segmentIntersectsSphere( a, b ) );
		EXPECT_FALSE( cube.rayIntersectsSphere( a, dir ) );
		EXPECT_FALSE( cube.lineIntersectsSphere( a, dir ) );
	}

	// Line/segment/ray intersections with cube
	{ // Diagonal
		Vertex const a( 0.0 ), b( 1.0 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir );
		EXPECT_TRUE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_TRUE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}
	{ // Center up half-diagonal
		Vertex const a( 0.5 ), b( 2.0 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir );
		EXPECT_TRUE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_TRUE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}
	{ // Outward at corner
		Vertex const a( 1.0 ), b( 2.0 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir );
		EXPECT_TRUE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_TRUE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}
	{ // Tangent at origin
		Vertex const a( -1.0+1.0e-12, 1.0+1.0e-12, 0.0 ), b( 1.0+1.0e-12, -1.0+1.0e-12, 0.0 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir.x, 1.0 / dir.y, 0.0 );
		EXPECT_TRUE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_TRUE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}
	{ // Inward outside but pointing to corner
		Vertex const a( 3.0 ), b( 2.0 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir );
		EXPECT_FALSE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_TRUE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_TRUE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}
	{ // Just beyond corner
		Vertex const a( 1.0000001 ), b( 2.0000002 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir );
		EXPECT_FALSE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_FALSE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_TRUE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}
	{ // Way outside cube but along diagonal
		Vertex const a( 5.0 ), b( 9.0 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir );
		EXPECT_FALSE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_FALSE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_TRUE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}
	{ // Way outside cube with no contact
		Vertex const a( 9.0 ), b( 12.0, 8.0, 4.0 ), dir( ( b - a ).normalize_zero() ), dir_inv( 1.0 / dir );
		EXPECT_FALSE( cube.segmentIntersectsCube( a, b ) );
		EXPECT_FALSE( cube.rayIntersectsCube( a, dir, dir_inv ) );
		EXPECT_FALSE( cube.lineIntersectsCube( a, dir, dir_inv ) );
	}

	// Clean up
	Surface.deallocate();
	TotSurfaces = 0;
}
