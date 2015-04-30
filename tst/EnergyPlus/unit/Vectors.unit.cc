// EnergyPlus::Vectors Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Vectors.hh>
#include <EnergyPlus/UtilityRoutines.hh>

// C++ Headers
#include <cmath>

using namespace EnergyPlus;
using namespace EnergyPlus::Vectors;
using namespace ObjexxFCL;

TEST( VectorsTest, AreaPolygon )
{
	ShowMessage( "Begin Test: VectorsTest, AreaPolygon" );
	Array1D< Vector > a( 4 ); // 3 x 7 rectangle
	a( 1 ).x = a( 1 ).y = a( 1 ).z = 0.0;
	a( 2 ).x = 3.0;
	a( 2 ).y = a( 2 ).z = 0.0;
	a( 3 ).x = 3.0;
	a( 3 ).y = 7.0;
	a( 3 ).z = 0.0;
	a( 4 ).x = 0.0;
	a( 4 ).y = 7.0;
	a( 4 ).z = 0.0;
	EXPECT_EQ( 21.0, AreaPolygon( 4, a ) );
}

TEST( VectorsTest, VecNormalize )
{
	ShowMessage( "Begin Test: VectorsTest, VecNormalize" );
	{
		Vector const v( 3.0, 3.0, 3.0 );
		Vector const n( VecNormalize( v ) );
		Real64 const h( 3.0 / std::sqrt( 27.0 ) );
		EXPECT_DOUBLE_EQ( h, n.x );
		EXPECT_DOUBLE_EQ( h, n.y );
		EXPECT_DOUBLE_EQ( h, n.z );
	}
	{
		Vector const v( 1.0, 3.0, 5.0 );
		Vector const n( VecNormalize( v ) );
		Real64 const f( 1.0 / std::sqrt( 35.0 ) );
		EXPECT_DOUBLE_EQ( f * v.x, n.x );
		EXPECT_DOUBLE_EQ( f * v.y, n.y );
		EXPECT_DOUBLE_EQ( f * v.z, n.z );
	}
}

TEST( VectorsTest, VecRound )
{
	ShowMessage( "Begin Test: VectorsTest, VecRound" );
	Vector v( 11.567, -33.602, 55.981 );
	VecRound( v, 2.0 );
	EXPECT_DOUBLE_EQ( 11.5, v.x );
	EXPECT_DOUBLE_EQ( -33.5, v.y );
	EXPECT_DOUBLE_EQ( 56.0, v.z );
}
