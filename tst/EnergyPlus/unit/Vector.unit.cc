// EnergyPlus::DataVectorTypes::Vector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataVectorTypes;
using namespace ObjexxFCL;

TEST( VectorTest, Basic )
{
	ShowMessage( "Begin Test: VectorTest, Basic" );
	{
		Vector v( 0.0, 0.0, 0.0 );
		EXPECT_EQ( 0.0, v.x );
		EXPECT_EQ( 0.0, v.y );
		EXPECT_EQ( 0.0, v.z );
		EXPECT_EQ( 0.0, v.length() );
		EXPECT_EQ( 0.0, v.length_squared() );
		EXPECT_EQ( 0.0, magnitude( v ) );
		EXPECT_EQ( 0.0, magnitude_squared( v ) );
		v *= 22.0;
		EXPECT_EQ( 0.0, v.x );
		EXPECT_EQ( 0.0, v.y );
		EXPECT_EQ( 0.0, v.z );
		EXPECT_EQ( 0.0, v.length() );
		EXPECT_EQ( 0.0, v.length_squared() );
		EXPECT_EQ( 0.0, magnitude( v ) );
		EXPECT_EQ( 0.0, magnitude_squared( v ) );
		v += 2.0;
		EXPECT_EQ( 2.0, v.x );
		EXPECT_EQ( 2.0, v.y );
		EXPECT_EQ( 2.0, v.z );
		EXPECT_EQ( 12.0, v.length_squared() );
		EXPECT_EQ( 12.0, magnitude_squared( v ) );
		v /= 2.0;
		EXPECT_EQ( 1.0, v.x );
		EXPECT_EQ( 1.0, v.y );
		EXPECT_EQ( 1.0, v.z );
		EXPECT_EQ( 3.0, v.length_squared() );
		EXPECT_EQ( 3.0, magnitude_squared( v ) );
		v -= 1.0;
		EXPECT_EQ( 0.0, v.x );
		EXPECT_EQ( 0.0, v.y );
		EXPECT_EQ( 0.0, v.z );
		EXPECT_EQ( 0.0, v.length() );
		EXPECT_EQ( 0.0, v.length_squared() );
		EXPECT_EQ( 0.0, magnitude( v ) );
		EXPECT_EQ( 0.0, magnitude_squared( v ) );
		Vector u( v );
		EXPECT_EQ( u.x, v.x );
		EXPECT_EQ( u.y, v.y );
		EXPECT_EQ( u.z, v.z );
	}
	{
		Vector v( 3.0 );
		EXPECT_EQ( 3.0, v.x );
		EXPECT_EQ( 3.0, v.y );
		EXPECT_EQ( 3.0, v.z );
		EXPECT_EQ( 27.0, v.length_squared() );
		EXPECT_EQ( 27.0, magnitude_squared( v ) );
		Vector u( v * 2.0 );
		EXPECT_EQ( 6.0, u.x );
		EXPECT_EQ( 6.0, u.y );
		EXPECT_EQ( 6.0, u.z );
		Vector w( -u );
		EXPECT_EQ( -6.0, w.x );
		EXPECT_EQ( -6.0, w.y );
		EXPECT_EQ( -6.0, w.z );
	}
	{
		Vector v( 1.0, 2.0, 3.0 );
		EXPECT_EQ( 1.0, v.x );
		EXPECT_EQ( 2.0, v.y );
		EXPECT_EQ( 3.0, v.z );
		EXPECT_EQ( 14.0, v.length_squared() );
		EXPECT_EQ( 14.0, magnitude_squared( v ) );
		Vector u( 1.0, 2.0, 3.0 );
		EXPECT_EQ( 0.0, distance( u, v ) );
		EXPECT_EQ( 0.0, distance_squared( u, v ) );
		EXPECT_EQ( 14.0, dot( u, v ) );
		Vector x( cross( u, v ) );
		EXPECT_EQ( 0.0, x.x );
		EXPECT_EQ( 0.0, x.y );
		EXPECT_EQ( 0.0, x.z );
	}
	{
		Vector v( 1.0, 2.0, 3.0 );
		EXPECT_EQ( 1.0, v.x );
		EXPECT_EQ( 2.0, v.y );
		EXPECT_EQ( 3.0, v.z );
		EXPECT_EQ( 14.0, v.length_squared() );
		EXPECT_EQ( 14.0, magnitude_squared( v ) );
		Vector u( 2.0 );
		EXPECT_EQ( 2.0, distance_squared( u, v ) );
		EXPECT_EQ( 12.0, dot( u, v ) );
		Vector x( cross( u, v ) );
		EXPECT_EQ( 2.0, x.x );
		EXPECT_EQ( -4.0, x.y );
		EXPECT_EQ( 2.0, x.z );
	}
}
