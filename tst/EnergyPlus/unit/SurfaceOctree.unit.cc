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
using namespace EnergyPlus::Octree;
using namespace EnergyPlus::SurfaceGeometry;
using namespace ObjexxFCL;
using Vertex = SurfaceOctreeCube::Vertex;

TEST( SurfaceOctreeCubeTest, Basic )
{
	// Vertices
	using size_type = Array3D< Vertex >::size_type;
	Array3D< Vertex > vertices( {0,1}, {0,1}, {0,1} );
	for ( size_type i = 0; i <= 1; ++i ) {
		for ( size_type j = 0; j <= 1; ++j ) {
			for ( size_type k = 0; k <= 1; ++k ) {
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

	// SurfaceOctreeCube
	SurfaceOctreeCube const box( Surface );

	EXPECT_EQ( Vertex( 0.0, 0.0, 0.0 ), box.l() );
	EXPECT_EQ( Vertex( 0.5, 0.5, 0.5 ), box.c() );
	EXPECT_EQ( Vertex( 1.0, 1.0, 1.0 ), box.u() );
	EXPECT_EQ( 1.0, box.w() );

	// Clean up
	Surface.deallocate();
	TotSurfaces = 0;
}
