// ObjexxFCL::numeric Unit Tests
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

// C++ Headers
#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/numeric.hh>
#include <ObjexxFCL/Array1D.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

#define ARRAY_LENGTH(a) ( sizeof(a) / sizeof(a[0]) )

// Spaces in type names don't play well with some of the tests.
typedef  signed char         schar;
typedef  unsigned char       uchar;
typedef  signed short        sshort;
typedef  unsigned short      ushort;
typedef  signed int          sint;
typedef  unsigned int        uint;
typedef  signed long         slong;
typedef  unsigned long       ulong;
typedef  long long           longlong;
typedef  signed long long    slonglong;
typedef  unsigned long long  ulonglong;
typedef  long double         longdouble;

// Stash numeric limits to improve readability
//const bool bool_min = std::numeric_limits< bool >::min();
bool const bool_max( std::numeric_limits< bool >::max() );

//char const char_min( std::numeric_limits< char >::min() );
char const char_max( std::numeric_limits< char >::max() );
//schar const schar_min( std::numeric_limits< schar >::min() );
schar const schar_max( std::numeric_limits< schar >::max() );
//uchar const uchar_min( std::numeric_limits< uchar >::min() );
uchar const uchar_max( std::numeric_limits< uchar >::max() );

//short const short_min( std::numeric_limits< short >::min() );
short const short_max( std::numeric_limits< short >::max() );
//sshort const sshort_min( std::numeric_limits< sshort >::min() );
sshort const sshort_max( std::numeric_limits< sshort >::max() );
//ushort const ushort_min( std::numeric_limits< ushort >::min() );
ushort const ushort_max( std::numeric_limits< ushort >::max() );

//int const int_min( std::numeric_limits< int >::min() );
int const int_max( std::numeric_limits< int >::max() );
//sint const sint_min( std::numeric_limits< sint >::min() );
sint const sint_max( std::numeric_limits< sint >::max() );
//uint const uint_min( std::numeric_limits< uint >::min() );
uint const uint_max( std::numeric_limits< uint >::max() );

//long const long_min( std::numeric_limits< long >::min() );
long const long_max( std::numeric_limits< long >::max() );
//slong const slong_min( std::numeric_limits< slong >::min() );
slong const slong_max( std::numeric_limits< slong >::max() );
//ulong const ulong_min( std::numeric_limits< ulong >::min() );
ulong const ulong_max( std::numeric_limits< ulong >::max() );

//longlong const longlong_min( std::numeric_limits< longlong >::min() );
longlong const longlong_max( std::numeric_limits< longlong >::max() );
//slonglong const slonglong_min( std::numeric_limits< slonglong >::min() );
slonglong const slonglong_max( std::numeric_limits< slonglong >::max() );
//ulonglong const ulonglong_min( std::numeric_limits< ulonglong >::min() );
ulonglong const ulonglong_max( std::numeric_limits< ulonglong >::max() );

float const float_min( std::numeric_limits< float >::min() );
float const float_max( std::numeric_limits< float >::max() );
double const double_min( std::numeric_limits< double >::min() );
double const double_max( std::numeric_limits< double >::max() );
longdouble const longdouble_min( std::numeric_limits< longdouble >::min() );
longdouble const longdouble_max( std::numeric_limits< longdouble >::max() );

TEST( NumericTest, KIND )
{
	EXPECT_EQ( 4, KIND( bool() ) );

	EXPECT_EQ( 1, KIND( std::int8_t() ) );
	EXPECT_EQ( 2, KIND( std::int16_t() ) );
	EXPECT_EQ( 4, KIND( std::int32_t() ) );
	EXPECT_EQ( 8, KIND( std::int64_t() ) );

	EXPECT_EQ( 1, KIND( std::uint8_t() ) );
	EXPECT_EQ( 2, KIND( std::uint16_t() ) );
	EXPECT_EQ( 4, KIND( std::uint32_t() ) );
	EXPECT_EQ( 8, KIND( std::uint64_t() ) );

	EXPECT_EQ( 4, KIND( float() ) );
	EXPECT_EQ( 8, KIND( double() ) );
	long double long_double = 0.0;
	EXPECT_EQ( 16, KIND( long_double ) );

	EXPECT_EQ( 4, KIND( std::complex< float >() ) );
	EXPECT_EQ( 8, KIND( std::complex< double >() ) );
	EXPECT_EQ( 16, KIND( std::complex< long double >() ) );

	EXPECT_EQ( 1, KIND( char() ) );
	EXPECT_EQ( 1, KIND( std::string() ) );
}

TEST( NumericTest, SELECTED_INT_KIND )
{
	const struct {
		int input;
		int output;
	} tests[] = {
	 { 0,  1 },
	 { 1,  1 },
	 { 2,  1 },
	 { 3,  2 },
	 { 4,  2 },
	 { 5,  4 },
	 { 6,  4 },
	 { 7,  4 },
	 { 8,  4 },
	 { 9,  4 },
	 { 10, 8 },
	 { 11, 8 },
	 { 12, 8 },
	 { 13, 8 },
	 { 14, 8 },
	 { 15, 8 },
	 { 16, 8 },
	 { 17, 8 },
	 { 18, 8 },
	 { 19, -1 },
	 { 20, -1 },
	 { 1000, -1 },
	 { -1, 1 },
	 { -1000, 1 }
	};
	int const N( ARRAY_LENGTH(tests) );

	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, SELECTED_INT_KIND( tests[ i ].input ) );
	}
}

TEST( NumericTest, SELECTED_REAL_KIND )
{
	int const min( std::numeric_limits<int>::min() );
	int const max( std::numeric_limits<int>::max() );
	const struct {
		int input1;
		int input2;
		int output;
	} tests[] = {
    { -1,  min, -3 },
    { -1,   -1, -3 },
    { -1,    0, -1 },
    { -1,   37, -1 },
    { -1,   38, -1 },
    { -1,  307, -1 },
    { -1,  308, -1 },
    { -1, 4931, -1 },
    { -1, 4932, -3 },
    { -1,  max, -3 },

    {  0,  min, -2 },
    {  0,   -1, -2 },
    {  0,    0,  4 },
    {  0,   37,  4 },
    {  0,   38,  8 },
    {  0,  307,  8 },
    {  0,  308, 16 },
    {  0, 4931, 16 },
    {  0, 4932, -2 },
    {  0,  max, -2 },

    {  2,   38,  8 },
    { 16,    3, 16 },

    {  6,  min, -2 },
    {  6,   -1, -2 },
    {  6,    0,  4 },
    {  6,   37,  4 },
    {  6,   38,  8 },
    {  6,  307,  8 },
    {  6,  308, 16 },
    {  6, 4931, 16 },
    {  6, 4932, -2 },
    {  6,  max, -2 },

    {  7,  min, -2 },
    {  7,   -1, -2 },
    {  7,    0,  8 },
    {  7,   37,  8 },
    {  7,   38,  8 },
    {  7,  307,  8 },
    {  7,  308, 16 },
    {  7, 4931, 16 },
    {  7, 4932, -2 },
    {  7,  max, -2 },

    { 15,  min, -2 },
    { 15,   -1, -2 },
    { 15,    0,  8 },
    { 15,   37,  8 },
    { 15,   38,  8 },
    { 15,  307,  8 },
    { 15,  308, 16 },
    { 15, 4931, 16 },
    { 15, 4932, -2 },
    { 15,  max, -2 },

    { 16,  min, -2 },
    { 16,   -1, -2 },
    { 16,    0, 16 },
    { 16,   37, 16 },
    { 16,   38, 16 },
    { 16,  307, 16 },
    { 16,  308, 16 },
    { 16, 4931, 16 },
    { 16, 4932, -2 },
    { 16,  max, -2 },

    { 33,  min, -2 },
    { 33,   -1, -2 },
    { 33,    0, 16 },
    { 33,   37, 16 },
    { 33,   38, 16 },
    { 33,  307, 16 },
    { 33,  308, 16 },
    { 33, 4931, 16 },
    { 33, 4932, -2 },
    { 33,  max, -2 },

    { 34,  min, -3 },
    { 34,   -1, -3 },
    { 34,    0, -1 },
    { 34,   37, -1 },
    { 34,   38, -1 },
    { 34,  307, -1 },
    { 34,  308, -1 },
    { 34, 4931, -1 },
    { 34, 4932, -3 },
    { 34,  max, -3 }
	};
	int const N( ARRAY_LENGTH(tests) );

	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, SELECTED_REAL_KIND( tests[ i ].input1, tests[ i ].input2 ) );
	}
}

TEST( NumericTest, SELECTED_CHAR_KIND )
{
	{
		const struct {
			char const * input;
			int output;
		} tests[] = {
		 { "DEFAULT", 1 }, { "ASCII", 1 },
		 { "default", -1 }, { "ascii", -1 }, { "", -1 },
		};
		int const N( ARRAY_LENGTH(tests) );

		for ( int i = 0; i < N; ++i ) {
			EXPECT_EQ( tests[ i ].output, SELECTED_CHAR_KIND( std::string( tests[ i ].input ) ) );
		}
	}

	{
		const struct {
			char input;
			int output;
		} tests[] = {
		 { 'a', -1 }, { 'A', -1 }, { '0', -1 }, { '.', -1 },
		 { ' ', -1 }, { '\n', -1 }, { '\0', -1 },
		};
		int const N( ARRAY_LENGTH(tests) );

		for ( int i = 0; i < N; ++i ) {
			EXPECT_EQ( tests[ i ].output, SELECTED_CHAR_KIND( tests[ i ].input ) );
		}
	}
}

TEST( NumericTest, SIZEOF )
{
	EXPECT_EQ( 1u,  SIZEOF( std::int8_t() ) );
	EXPECT_EQ( 2u, SIZEOF( std::int16_t() ) );
	EXPECT_EQ( 4u, SIZEOF( std::int32_t() ) );
	EXPECT_EQ( 8u, SIZEOF( std::int64_t() ) );

	EXPECT_EQ( 1u,  SIZEOF( std::uint8_t() ) );
	EXPECT_EQ( 2u, SIZEOF( std::uint16_t() ) );
	EXPECT_EQ( 4u, SIZEOF( std::uint32_t() ) );
	EXPECT_EQ( 8u, SIZEOF( std::uint64_t() ) );

	EXPECT_EQ( 3u,  SIZEOF( "Cat" ) );
	EXPECT_EQ( 3u,  SIZEOF( std::string( "Cat" ) ) );

	std::uint8_t a[2];
	EXPECT_EQ( 2u,  SIZEOF( a ) );

	Array1D< float > b( 4 );
	EXPECT_EQ( 16u,  SIZEOF( b ) );
}

TEST( NumericTest, RADIX )
{
	EXPECT_EQ( std::numeric_limits< bool >::radix, RADIX( bool() ) );
	EXPECT_EQ( std::numeric_limits< char >::radix, RADIX( char() ) );
	EXPECT_EQ( std::numeric_limits< schar >::radix, RADIX( schar() ) );
	EXPECT_EQ( std::numeric_limits< uchar >::radix, RADIX( uchar() ) );
	EXPECT_EQ( std::numeric_limits< wchar_t >::radix, RADIX( wchar_t() ) );
	EXPECT_EQ( std::numeric_limits< char16_t >::radix, RADIX( char16_t() ) );
	EXPECT_EQ( std::numeric_limits< char32_t >::radix, RADIX( char32_t() ) );
	EXPECT_EQ( std::numeric_limits< short >::radix, RADIX( short() ) );
	EXPECT_EQ( std::numeric_limits< sshort >::radix, RADIX( sshort() ) );
	EXPECT_EQ( std::numeric_limits< ushort >::radix, RADIX( ushort() ) );
	EXPECT_EQ( std::numeric_limits< int >::radix, RADIX( int() ) );
	EXPECT_EQ( std::numeric_limits< sint >::radix, RADIX( sint() ) );
	EXPECT_EQ( std::numeric_limits< uint >::radix, RADIX( uint() ) );
	EXPECT_EQ( std::numeric_limits< long >::radix, RADIX( long() ) );
	EXPECT_EQ( std::numeric_limits< slong >::radix, RADIX( slong() ) );
	EXPECT_EQ( std::numeric_limits< ulong >::radix, RADIX( ulong() ) );
	EXPECT_EQ( std::numeric_limits< longlong >::radix, RADIX( longlong() ) );
	EXPECT_EQ( std::numeric_limits< slonglong >::radix, RADIX( slonglong() ) );
	EXPECT_EQ( std::numeric_limits< ulonglong >::radix, RADIX( ulonglong() ) );
	EXPECT_EQ( std::numeric_limits< float >::radix, RADIX( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::radix, RADIX( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::radix, RADIX( longdouble() ) );
}

TEST( NumericTest, DIGITS )
{
	EXPECT_EQ( std::numeric_limits< bool >::digits, DIGITS( bool() ) );
	EXPECT_EQ( std::numeric_limits< char >::digits, DIGITS( char() ) );
	EXPECT_EQ( std::numeric_limits< schar >::digits, DIGITS( schar() ) );
	EXPECT_EQ( std::numeric_limits< uchar >::digits, DIGITS( uchar() ) );
	EXPECT_EQ( std::numeric_limits< wchar_t >::digits, DIGITS( wchar_t() ) );
	EXPECT_EQ( std::numeric_limits< char16_t >::digits, DIGITS( char16_t() ) );
	EXPECT_EQ( std::numeric_limits< char32_t >::digits, DIGITS( char32_t() ) );
	EXPECT_EQ( std::numeric_limits< short >::digits, DIGITS( short() ) );
	EXPECT_EQ( std::numeric_limits< sshort >::digits, DIGITS( sshort() ) );
	EXPECT_EQ( std::numeric_limits< ushort >::digits, DIGITS( ushort() ) );
	EXPECT_EQ( std::numeric_limits< int >::digits, DIGITS( int() ) );
	EXPECT_EQ( std::numeric_limits< sint >::digits, DIGITS( sint() ) );
	EXPECT_EQ( std::numeric_limits< uint >::digits, DIGITS( uint() ) );
	EXPECT_EQ( std::numeric_limits< long >::digits, DIGITS( long() ) );
	EXPECT_EQ( std::numeric_limits< slong >::digits, DIGITS( slong() ) );
	EXPECT_EQ( std::numeric_limits< ulong >::digits, DIGITS( ulong() ) );
	EXPECT_EQ( std::numeric_limits< longlong >::digits, DIGITS( longlong() ) );
	EXPECT_EQ( std::numeric_limits< slonglong >::digits, DIGITS( slonglong() ) );
	EXPECT_EQ( std::numeric_limits< ulonglong >::digits, DIGITS( ulonglong() ) );
	EXPECT_EQ( std::numeric_limits< float >::digits, DIGITS( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::digits, DIGITS( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::digits, DIGITS( longdouble() ) );
}

TEST( NumericTest, Huge )
{
	EXPECT_EQ( bool_max, HUGE_( bool() ) );
	EXPECT_EQ( char_max, HUGE_( char() ) );
	EXPECT_EQ( schar_max, HUGE_( schar() ) );
	EXPECT_EQ( uchar_max, HUGE_( uchar() ) );
	EXPECT_EQ( std::numeric_limits< wchar_t >::max(), HUGE_( wchar_t() ) );
	EXPECT_EQ( std::numeric_limits< char16_t >::max(), HUGE_( char16_t() ) );
	EXPECT_EQ( std::numeric_limits< char32_t >::max(), HUGE_( char32_t() ) );
	EXPECT_EQ( short_max, HUGE_( short() ) );
	EXPECT_EQ( sshort_max, HUGE_( sshort() ) );
	EXPECT_EQ( ushort_max, HUGE_( ushort() ) );
	EXPECT_EQ( int_max, HUGE_( int() ) );
	EXPECT_EQ( sint_max, HUGE_( sint() ) );
	EXPECT_EQ( uint_max, HUGE_( uint() ) );
	EXPECT_EQ( long_max, HUGE_( long() ) );
	EXPECT_EQ( slong_max, HUGE_( slong() ) );
	EXPECT_EQ( ulong_max, HUGE_( ulong() ) );
	EXPECT_EQ( longlong_max, HUGE_( longlong() ) );
	EXPECT_EQ( slonglong_max, HUGE_( slonglong() ) );
	EXPECT_EQ( ulonglong_max, HUGE_( ulonglong() ) );
	EXPECT_EQ( float_max, HUGE_( float() ) );
	EXPECT_EQ( double_max, HUGE_( double() ) );
	EXPECT_EQ( longdouble_max, HUGE_( longdouble() ) );
}

TEST( NumericTest, TINY )
{
	EXPECT_EQ( float_min, TINY( float() ) );
	EXPECT_EQ( double_min, TINY( double() ) );
	EXPECT_EQ( longdouble_min, TINY( longdouble() ) );
}

TEST( NumericTest, EPSILON )
{
	EXPECT_EQ( std::numeric_limits< float >::epsilon(), EPSILON( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::epsilon(), EPSILON( double() ) );
	EXPECT_EQ( std::numeric_limits< long double >::epsilon(), EPSILON( longdouble() ) );
}

TEST( NumericTest, PRECISION )
{
	EXPECT_EQ( 6, PRECISION( float() ) );
	EXPECT_EQ( 15, PRECISION( double() ) );
#ifdef __GNUC__
	EXPECT_EQ( 18, PRECISION( longdouble() ) );
#else
#ifdef _WIN32
	EXPECT_EQ( 15, PRECISION( longdouble() ) );
#else
	EXPECT_EQ( 18, PRECISION( longdouble() ) );
#endif
#endif

	EXPECT_EQ( 6, PRECISION( std::complex< float >() ) );
	EXPECT_EQ( 15, PRECISION( std::complex< double >() ) );
#ifdef __GNUC__
	EXPECT_EQ( 18, PRECISION( std::complex< longdouble >() ) );
#else
#ifdef _WIN32
	EXPECT_EQ( 15, PRECISION( std::complex< longdouble >() ) );
#else
	EXPECT_EQ( 18, PRECISION( std::complex< longdouble >() ) );
#endif
#endif
}

TEST( NumericTest, EXPONENT_RANGE )
{
	EXPECT_EQ( 2,  EXPONENT_RANGE( std::int8_t() ) );
	EXPECT_EQ( 4,  EXPONENT_RANGE( std::int16_t() ) );
	EXPECT_EQ( 9,  EXPONENT_RANGE( std::int32_t() ) );
	EXPECT_EQ( 18, EXPONENT_RANGE( std::int64_t() ) );

	EXPECT_EQ( 2,  EXPONENT_RANGE( std::uint8_t() ) );
	EXPECT_EQ( 4,  EXPONENT_RANGE( std::uint16_t() ) );
	EXPECT_EQ( 9,  EXPONENT_RANGE( std::uint32_t() ) );
	EXPECT_EQ( 18, EXPONENT_RANGE( std::uint64_t() ) );

	EXPECT_EQ( 37,   EXPONENT_RANGE( float() ) );
	EXPECT_EQ( 307,  EXPONENT_RANGE( double() ) );
	EXPECT_EQ( 4931, EXPONENT_RANGE( longdouble() ) );

	EXPECT_EQ( 37,   EXPONENT_RANGE( std::complex< float >() ) );
	EXPECT_EQ( 307,  EXPONENT_RANGE( std::complex< double >() ) );
	EXPECT_EQ( 4931, EXPONENT_RANGE( std::complex< long double >() ) );
}

TEST( NumericTest, MINEXPONENT )
{
	EXPECT_EQ( std::numeric_limits< float >::min_exponent, MINEXPONENT( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::min_exponent, MINEXPONENT( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::min_exponent, MINEXPONENT( longdouble() ) );
}

TEST( NumericTest, MAXEXPONENT )
{
	EXPECT_EQ( std::numeric_limits< float >::max_exponent, MAXEXPONENT( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::max_exponent, MAXEXPONENT( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::max_exponent, MAXEXPONENT( longdouble() ) );
}

TEST( NumericTest, EXPONENT )
{
	EXPECT_EQ( 0, EXPONENT( float() ) );
	EXPECT_EQ( -125, EXPONENT( float_min ) );
	EXPECT_EQ( 129, EXPONENT( float_max ) );
	EXPECT_EQ( 0, EXPONENT( double() ) );
	EXPECT_EQ( -1021, EXPONENT( double_min ) );
	EXPECT_EQ( 1025, EXPONENT( double_max ) );
	EXPECT_EQ( 0, EXPONENT( longdouble() ) );
#ifndef __INTEL_COMPILER // Avoid exception
#ifdef _MSC_VER // long double == double
	EXPECT_EQ( -1021, EXPONENT( longdouble_min ) );
	EXPECT_EQ( 1025, EXPONENT( longdouble_max ) );
#else
	EXPECT_EQ( -16381, EXPONENT( longdouble_min ) );
	EXPECT_EQ( 16385, EXPONENT( longdouble_max ) );
#endif
#endif
	EXPECT_EQ( 0, EXPONENT( float() ) );
	EXPECT_EQ( 0, EXPONENT( double() ) );
	EXPECT_EQ( 0, EXPONENT( longdouble() ) );
}

TEST( NumericTest, SET_EXPONENT )
{
	EXPECT_EQ( 0, SET_EXPONENT( float(), 1 ) );
	EXPECT_EQ( 0, SET_EXPONENT( double(), 1 ) );
	EXPECT_EQ( 0, SET_EXPONENT( longdouble(), 1 ) );
}

TEST( NumericTest, SCALE )
{
	EXPECT_EQ( 128.0, SCALE( 4.0, 5 ) );
	EXPECT_EQ( 24.0f, SCALE( 3.0f, 3 ) );
}

TEST( NumericTest, FRACTION )
{
	EXPECT_EQ( 0, FRACTION( float() ) );
	EXPECT_FLOAT_EQ( 0.5, FRACTION( float_min ) );
	EXPECT_FLOAT_EQ( 0.5, FRACTION( float_max ) );
	EXPECT_EQ( 0, FRACTION( double() ) );
	EXPECT_DOUBLE_EQ( 0.5, FRACTION( double_min ) );
	EXPECT_DOUBLE_EQ( 0.5, FRACTION( double_max ) );
	EXPECT_EQ( 0, FRACTION( longdouble() ) );
#ifndef __INTEL_COMPILER
	EXPECT_DOUBLE_EQ( 0.5, FRACTION( longdouble_min ) );
	EXPECT_DOUBLE_EQ( 0.5, FRACTION( longdouble_max ) );
#endif
	EXPECT_EQ( 0, FRACTION( float() ) );
	EXPECT_EQ( 0, FRACTION( double() ) );
	EXPECT_EQ( 0, FRACTION( longdouble() ) );
}

TEST( NumericTest, SPACING )
{
	EXPECT_EQ( SPACING( 0.0 ), TINY( 1.0 ) );
	EXPECT_EQ( SPACING( 1.0 ), EPSILON( 1.0 ) );
}

TEST( NumericTest, RECIPROCAL_RELATIVE_SPACING )
{
	EXPECT_EQ( RECIPROCAL_RELATIVE_SPACING( +0.0 ), 0.0 );
	EXPECT_DOUBLE_EQ( RECIPROCAL_RELATIVE_SPACING( +2.0 ), 4.503599627370496E+15 );
	EXPECT_DOUBLE_EQ( RECIPROCAL_RELATIVE_SPACING( -2.0 ), 4.503599627370496E+15 );
	EXPECT_DOUBLE_EQ( RECIPROCAL_RELATIVE_SPACING( +3.0 ), 6.755399441055744E+15 );
	EXPECT_DOUBLE_EQ( RECIPROCAL_RELATIVE_SPACING( -3.0 ), 6.755399441055744E+15 );
	EXPECT_DOUBLE_EQ( RECIPROCAL_RELATIVE_SPACING( +9.0 ), 5.066549580791808E+15 );
	EXPECT_DOUBLE_EQ( RECIPROCAL_RELATIVE_SPACING( -9.0 ), 5.066549580791808E+15 );
}

TEST( NumericTest, NEAREST )
{
	EXPECT_DOUBLE_EQ( 3.0 + std::pow( 2.0, -22 ), NEAREST( 3.0f, 2.0f ) );
	EXPECT_DOUBLE_EQ( 3.0 + std::pow( 2.0, -22 ), NEAREST( 3.0f, 2.0 ) );
	EXPECT_DOUBLE_EQ( 3.0 - std::pow( 2.0, -22 ), NEAREST( 3.0f, -2.0f ) );
	EXPECT_DOUBLE_EQ( 3.0 - std::pow( 2.0, -22 ), NEAREST( 3.0f, -2.0 ) );
	double const eps( std::pow( 2.0, -52 ) );
	EXPECT_DOUBLE_EQ( 1.0 + eps, NEAREST( 1.0, 1.0 ) );
	EXPECT_DOUBLE_EQ( 1.0 - eps, NEAREST( 1.0, -1.0 )  );
}
