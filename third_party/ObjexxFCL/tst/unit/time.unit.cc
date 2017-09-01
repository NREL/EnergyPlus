// ObjexxFCL::time Unit Tests
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/time.hh>
#include <ObjexxFCL/char.functions.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <chrono>

using namespace ObjexxFCL;

TEST( TimeTest, Itime )
{
	Array1D< std::int32_t > timearray( 3 );
	ITIME( timearray );
	EXPECT_EQ( timearray.size(), 3u );
	EXPECT_TRUE( 0 <= timearray( 1 ) );
	EXPECT_TRUE( timearray( 1 ) < 24 );
	EXPECT_TRUE( 0 <= timearray( 2 ) );
	EXPECT_TRUE( timearray( 2 ) < 60 );
	EXPECT_TRUE( 0 <= timearray( 3 ) );
	EXPECT_TRUE( timearray( 3 ) < 60 );
}

TEST( TimeTest, Gettim )
{
	{
		std::int64_t h, m, s, c;
		GETTIM( h, m, s, c );
		EXPECT_TRUE( 0 <= h );
		EXPECT_TRUE( h < 24 );
		EXPECT_TRUE( 0 <= m );
		EXPECT_TRUE( m < 60 );
		EXPECT_TRUE( 0 <= s );
		EXPECT_TRUE( s < 60 );
		EXPECT_TRUE( 0 <= c );
		EXPECT_TRUE( c < 100 );
	}

	{
		std::int32_t h, m, s, c;
		GETTIM( h, m, s, c );
		EXPECT_TRUE( 0 <= h );
		EXPECT_TRUE( h < 24 );
		EXPECT_TRUE( 0 <= m );
		EXPECT_TRUE( m < 60 );
		EXPECT_TRUE( 0 <= s );
		EXPECT_TRUE( s < 60 );
		EXPECT_TRUE( 0 <= c );
		EXPECT_TRUE( c < 100 );
	}

	{
		std::int16_t h, m, s, c;
		GETTIM( h, m, s, c );
		EXPECT_TRUE( 0 <= h );
		EXPECT_TRUE( h < 24 );
		EXPECT_TRUE( 0 <= m );
		EXPECT_TRUE( m < 60 );
		EXPECT_TRUE( 0 <= s );
		EXPECT_TRUE( s < 60 );
		EXPECT_TRUE( 0 <= c );
		EXPECT_TRUE( c < 100 );
	}
}

TEST( TimeTest, Time )
{
	std::int64_t const t( TIME() );
	EXPECT_TRUE( 1498458577 < t );

	std::string ts;
	TIME( ts );
	EXPECT_EQ( ts.length(), 8u );
	EXPECT_TRUE( is_digit( ts[ 0 ] ) );
	EXPECT_TRUE( is_digit( ts[ 1 ] ) );
	EXPECT_EQ( ts[ 2 ], ':' );
	EXPECT_TRUE( is_digit( ts[ 3 ] ) );
	EXPECT_TRUE( is_digit( ts[ 4 ] ) );
	EXPECT_EQ( ts[ 5 ], ':' );
	EXPECT_TRUE( is_digit( ts[ 6 ] ) );
	EXPECT_TRUE( is_digit( ts[ 7 ] ) );
}

TEST( TimeTest, Clock )
{
	std::string ts( CLOCK() );
	EXPECT_EQ( ts.length(), 8u );
	EXPECT_TRUE( is_digit( ts[ 0 ] ) );
	EXPECT_TRUE( is_digit( ts[ 1 ] ) );
	EXPECT_EQ( ts[ 2 ], ':' );
	EXPECT_TRUE( is_digit( ts[ 3 ] ) );
	EXPECT_TRUE( is_digit( ts[ 4 ] ) );
	EXPECT_EQ( ts[ 5 ], ':' );
	EXPECT_TRUE( is_digit( ts[ 6 ] ) );
	EXPECT_TRUE( is_digit( ts[ 7 ] ) );
}

TEST( TimeTest, SystemClock )
{
	{
		std::int64_t count, count_rate, count_max;
		SYSTEM_CLOCK_64( count, count_rate, count_max );
		EXPECT_TRUE( 0 <= count );
		EXPECT_TRUE( 0 <= count_rate );
		EXPECT_TRUE( 0 <= count_max );
	}

	{
		std::int32_t count, count_rate, count_max;
		SYSTEM_CLOCK_32( count, count_rate, count_max );
		EXPECT_TRUE( 0 <= count );
		EXPECT_TRUE( 0 <= count_rate );
		EXPECT_TRUE( 0 <= count_max );
		SYSTEM_CLOCK( count, count_rate, count_max );
		EXPECT_TRUE( 0 <= count );
		EXPECT_TRUE( 0 <= count_rate );
		EXPECT_TRUE( 0 <= count_max );
	}

	{
		std::int16_t count, count_rate, count_max;
		SYSTEM_CLOCK_16( count, count_rate, count_max );
		EXPECT_TRUE( 0 <= count );
		EXPECT_TRUE( 0 <= count_rate );
		EXPECT_TRUE( 0 <= count_max );
	}
}

TEST( TimeTest, CpuTime )
{
	double time;
	CPU_TIME( time );
	EXPECT_TRUE( 0.0 <= time );
}

TEST( TimeTest, Clockx )
{
	double time;
	CLOCKX( time );
	EXPECT_TRUE( 0.0 <= time );
}

TEST( TimeTest, Idate )
{
	Array1D< std::int32_t > datearray( 3 );
	IDATE( datearray );
	EXPECT_EQ( datearray.size(), 3u );
	EXPECT_TRUE( 1 <= datearray( 1 ) );
	EXPECT_TRUE( datearray( 1 ) <= 31 );
	EXPECT_TRUE( 1 <= datearray( 2 ) );
	EXPECT_TRUE( datearray( 2 ) <= 12 );
	EXPECT_TRUE( 2000 <= datearray( 3 ) );
	EXPECT_TRUE( datearray( 3 ) <= 9999 );

	{
		std::int64_t m, d, y;
		IDATE( m, d, y );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
		EXPECT_TRUE( 0 <= y );
		EXPECT_TRUE( y <= 99 );
	}

	{
		std::int32_t m, d, y;
		IDATE( m, d, y );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
		EXPECT_TRUE( 0 <= y );
		EXPECT_TRUE( y <= 99 );
	}

	{
		std::int16_t m, d, y;
		IDATE( m, d, y );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
		EXPECT_TRUE( 0 <= y );
		EXPECT_TRUE( y <= 99 );
	}
}

TEST( TimeTest, Idate4 )
{
	Array1D< std::int32_t > datearray( 3 );
	IDATE4( datearray );
	EXPECT_EQ( datearray.size(), 3u );
	EXPECT_TRUE( 1 <= datearray( 1 ) );
	EXPECT_TRUE( datearray( 1 ) <= 31 );
	EXPECT_TRUE( 1 <= datearray( 2 ) );
	EXPECT_TRUE( datearray( 2 ) <= 12 );
	EXPECT_TRUE( 2000 <= datearray( 3 ) );
	EXPECT_TRUE( datearray( 3 ) <= 9999 );

	{
		std::int64_t m, d, y;
		IDATE4( m, d, y );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
		EXPECT_TRUE( 100 <= y );
		EXPECT_TRUE( y <= 9999 );
	}

	{
		std::int32_t m, d, y;
		IDATE4( m, d, y );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
		EXPECT_TRUE( 100 <= y );
		EXPECT_TRUE( y <= 9999 );
	}

	{
		std::int16_t m, d, y;
		IDATE4( m, d, y );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
		EXPECT_TRUE( 100 <= y );
		EXPECT_TRUE( y <= 9999 );
	}
}

TEST( TimeTest, Jdate )
{
	std::string const j( JDATE() );
	EXPECT_EQ( j.length(), 5u );
	for ( int i = 0; i < 5; ++i ) {
		EXPECT_TRUE( is_digit( j[ i ] ) );
	}
}

TEST( TimeTest, Jdate4 )
{
	std::string const j( JDATE4() );
	EXPECT_EQ( j.length(), 7u );
	for ( int i = 0; i < 7; ++i ) {
		EXPECT_TRUE( is_digit( j[ i ] ) );
	}
}

TEST( TimeTest, Getdat )
{
	{
		std::int64_t y, m, d;
		GETDAT( y, m, d );
		EXPECT_TRUE( 2000 <= y );
		EXPECT_TRUE( y <= 9999 );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
	}

	{
		std::int32_t y, m, d;
		GETDAT( y, m, d );
		EXPECT_TRUE( 2000 <= y );
		EXPECT_TRUE( y <= 9999 );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
	}

	{
		std::int16_t y, m, d;
		GETDAT( y, m, d );
		EXPECT_TRUE( 2000 <= y );
		EXPECT_TRUE( y <= 9999 );
		EXPECT_TRUE( 1 <= m );
		EXPECT_TRUE( m <= 12 );
		EXPECT_TRUE( 1 <= d );
		EXPECT_TRUE( d <= 31 );
	}
}

TEST( TimeTest, Date )
{
	{
		std::string const d( DATE() );
		EXPECT_EQ( d.length(), 8u );
		EXPECT_TRUE( is_digit( d[ 0 ] ) );
		EXPECT_TRUE( is_digit( d[ 1 ] ) );
		EXPECT_EQ( d[ 2 ], '/' );
		EXPECT_TRUE( is_digit( d[ 3 ] ) );
		EXPECT_TRUE( is_digit( d[ 4 ] ) );
		EXPECT_EQ( d[ 5 ], '/' );
		EXPECT_TRUE( is_digit( d[ 6 ] ) );
		EXPECT_TRUE( is_digit( d[ 7 ] ) );
	}

	{
		std::string d;
		DATE( d );
		EXPECT_EQ( d.length(), 9u );
		EXPECT_TRUE( is_digit( d[ 0 ] ) );
		EXPECT_TRUE( is_digit( d[ 1 ] ) );
		EXPECT_EQ( d[ 2 ], '-' );
		EXPECT_TRUE( is_alpha( d[ 3 ] ) );
		EXPECT_TRUE( is_alpha( d[ 4 ] ) );
		EXPECT_TRUE( is_alpha( d[ 5 ] ) );
		EXPECT_EQ( d[ 6 ], '-' );
		EXPECT_TRUE( is_digit( d[ 7 ] ) );
		EXPECT_TRUE( is_digit( d[ 8 ] ) );
	}
}

TEST( TimeTest, Date4 )
{
	std::string d;
	DATE4( d );
	EXPECT_EQ( d.length(), 11u );
	EXPECT_TRUE( is_digit( d[ 0 ] ) );
	EXPECT_TRUE( is_digit( d[ 1 ] ) );
	EXPECT_EQ( d[ 2 ], '-' );
	EXPECT_TRUE( is_alpha( d[ 3 ] ) );
	EXPECT_TRUE( is_alpha( d[ 4 ] ) );
	EXPECT_TRUE( is_alpha( d[ 5 ] ) );
	EXPECT_EQ( d[ 6 ], '-' );
	EXPECT_TRUE( is_digit( d[ 7 ] ) );
	EXPECT_TRUE( is_digit( d[ 8 ] ) );
	EXPECT_TRUE( is_digit( d[ 9 ] ) );
	EXPECT_TRUE( is_digit( d[ 10 ] ) );
}

TEST( TimeTest, DateAndTime )
{
	std::string d, t, z;
	Array1D_int v( 8 );
	date_and_time( d, t, z, v );
	EXPECT_EQ( d.length(), 8u );
	for ( int i = 0; i < 8; ++i ) {
		EXPECT_TRUE( is_digit( d[ i ] ) );
	}
	EXPECT_EQ( t.length(), 10u );
	EXPECT_TRUE( is_digit( t[ 0 ] ) );
	EXPECT_TRUE( is_digit( t[ 1 ] ) );
	EXPECT_TRUE( is_digit( t[ 2 ] ) );
	EXPECT_TRUE( is_digit( t[ 3 ] ) );
	EXPECT_TRUE( is_digit( t[ 4 ] ) );
	EXPECT_TRUE( is_digit( t[ 5 ] ) );
	EXPECT_EQ( t[ 6 ], '.' );
	EXPECT_TRUE( is_digit( t[ 7 ] ) );
	EXPECT_TRUE( is_digit( t[ 8 ] ) );
	EXPECT_TRUE( is_digit( t[ 9 ] ) );
	EXPECT_EQ( z.length(), 5u );
	EXPECT_TRUE( ( z[ 0 ] == '+' ) || ( z[ 0 ] == '-' ) );
	EXPECT_TRUE( is_digit( z[ 1 ] ) );
	EXPECT_TRUE( is_digit( z[ 2 ] ) );
	EXPECT_TRUE( is_digit( z[ 3 ] ) );
	EXPECT_TRUE( is_digit( z[ 4 ] ) );
	EXPECT_TRUE( 2000 <= v( 1 ) );
	EXPECT_TRUE( v( 1 ) <= 9999 );
	EXPECT_TRUE( 1 <= v( 2 ) );
	EXPECT_TRUE( v( 2 ) <= 12 );
	EXPECT_TRUE( 1 <= v( 3 ) );
	EXPECT_TRUE( v( 3 ) <= 31 );
	EXPECT_TRUE( v( 4 ) <= 840 ); // +14 h offset is max
	EXPECT_TRUE( 0 <= v( 5 ) );
	EXPECT_TRUE( v( 5 ) <= 59 );
	EXPECT_TRUE( 0 <= v( 6 ) );
	EXPECT_TRUE( v( 6 ) <= 59 );
	EXPECT_TRUE( 0 <= v( 7 ) );
	EXPECT_TRUE( v( 7 ) <= 999 );
}

TEST( TimeTest, Sleep )
{
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	system_clock::time_point const t0( system_clock::now() );
	SLEEP( 0.01 );
	system_clock::time_point const t1( system_clock::now() );
	int const msd( static_cast< int >( duration_cast< milliseconds >( t1.time_since_epoch() - t0.time_since_epoch() ).count() % 1000 ) ); // msec
	EXPECT_TRUE( msd >= 9 );
}
