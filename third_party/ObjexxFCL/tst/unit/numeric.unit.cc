// ObjexxFCL::numeric Unit Tests
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
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
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

#define ARRAY_LENGTH(a) (sizeof(a) / sizeof(a[0]))

// Spaces in type names don't play well with some of the tests.
typedef signed char        schar;
typedef unsigned char      uchar;
typedef signed short       sshort;
typedef unsigned short     ushort;
typedef signed int         sint;
typedef unsigned int       uint;
typedef signed long        slong;
typedef unsigned long      ulong;
typedef long long          longlong;
typedef signed long long   slonglong;
typedef unsigned long long ulonglong;
typedef long double        longdouble;

// Stash numeric limits to improve readability
//const bool       bool_min = std::numeric_limits< bool >::min();
const bool       bool_max = std::numeric_limits< bool >::max();

//const char       char_min = std::numeric_limits< char >::min();
const char       char_max = std::numeric_limits< char >::max();
//const schar      schar_min = std::numeric_limits< schar >::min();
const schar      schar_max = std::numeric_limits< schar >::max();
//const uchar      uchar_min = std::numeric_limits< uchar >::min();
const uchar      uchar_max = std::numeric_limits< uchar >::max();

//const short      short_min = std::numeric_limits< short >::min();
const short      short_max = std::numeric_limits< short >::max();
//const sshort     sshort_min = std::numeric_limits< sshort >::min();
const sshort     sshort_max = std::numeric_limits< sshort >::max();
//const ushort     ushort_min = std::numeric_limits< ushort >::min();
const ushort     ushort_max = std::numeric_limits< ushort >::max();

//const int        int_min = std::numeric_limits< int >::min();
const int        int_max = std::numeric_limits< int >::max();
//const sint       sint_min = std::numeric_limits< sint >::min();
const sint       sint_max = std::numeric_limits< sint >::max();
//const uint       uint_min = std::numeric_limits< uint >::min();
const uint       uint_max = std::numeric_limits< uint >::max();

//const long       long_min = std::numeric_limits< long >::min();
const long       long_max = std::numeric_limits< long >::max();
//const slong      slong_min = std::numeric_limits< slong >::min();
const slong      slong_max = std::numeric_limits< slong >::max();
//const ulong      ulong_min = std::numeric_limits< ulong >::min();
const ulong      ulong_max = std::numeric_limits< ulong >::max();

//const longlong   longlong_min = std::numeric_limits< longlong >::min();
const longlong   longlong_max = std::numeric_limits< longlong >::max();
//const slonglong  slonglong_min = std::numeric_limits< slonglong >::min();
const slonglong  slonglong_max = std::numeric_limits< slonglong >::max();
//const ulonglong  ulonglong_min = std::numeric_limits< ulonglong >::min();
const ulonglong  ulonglong_max = std::numeric_limits< ulonglong >::max();

const float      float_min = std::numeric_limits< float >::min();
const float      float_max = std::numeric_limits< float >::max();
const double     double_min = std::numeric_limits< double >::min();
const double     double_max = std::numeric_limits< double >::max();
const longdouble longdouble_min = std::numeric_limits< longdouble >::min();
const longdouble longdouble_max = std::numeric_limits< longdouble >::max();

TEST( NumericTest, kind )
{
	EXPECT_EQ( 4, kind( bool() ) );

	EXPECT_EQ( 1, kind( std::int8_t() ) );
	EXPECT_EQ( 2, kind( std::int16_t() ) );
	EXPECT_EQ( 4, kind( std::int32_t() ) );
	EXPECT_EQ( 8, kind( std::int64_t() ) );

	EXPECT_EQ( 1, kind( std::uint8_t() ) );
	EXPECT_EQ( 2, kind( std::uint16_t() ) );
	EXPECT_EQ( 4, kind( std::uint32_t() ) );
	EXPECT_EQ( 8, kind( std::uint64_t() ) );

	EXPECT_EQ( 4,  kind( float() ) );
	EXPECT_EQ( 8,  kind( double() ) );
	EXPECT_EQ( 16, kind( longdouble() ) );

	EXPECT_EQ( 4,  kind( std::complex<float>() ) );
	EXPECT_EQ( 8,  kind( std::complex<double>() ) );
	EXPECT_EQ( 16, kind( std::complex<long double>() ) );

	EXPECT_EQ( 1, kind( char() ) );
	EXPECT_EQ( 1, kind( std::string() ) );
}

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

	EXPECT_EQ( 4, KIND( std::complex<float>() ) );
	EXPECT_EQ( 8, KIND( std::complex<double>() ) );
	EXPECT_EQ( 16, KIND( std::complex<long double>() ) );

	EXPECT_EQ( 1, KIND( char() ) );
	EXPECT_EQ( 1, KIND( std::string() ) );
}

TEST( NumericTest, selectedIntKind )
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
	const int N = ARRAY_LENGTH(tests);

	// selected_int_kind
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, selected_int_kind( tests[ i ].input ) );
	}
	// SELECTED_INT_KIND
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, SELECTED_INT_KIND( tests[ i ].input ) );
	}
}

TEST( NumericTest, selectedRealKind )
{
	const int min = std::numeric_limits<int>::min();
	const int max = std::numeric_limits<int>::max();
	const struct {
		int input1;
		int input2;
		int output;
	} tests[] = {
	 { -1,  min, -3 }, { -1,   -1, -3 },
	 { -1,    0, -1 }, { -1,   37, -1 },
	 { -1,   38, -1 }, { -1,  307, -1 },
	 { -1,  308, -1 }, { -1, 4931, -1 },
	 { -1, 4932, -3 }, { -1,  max, -3 },

	 {  0, min,  -2 }, {  0,   -1, -2 },
	 {  0,   0,	4 }, {  0,   37,  4 },
	 {  0,  38,	4 }, {  0,  307,  4 },
	 {  0, 308,	4 }, {  0, 4931,  4 },
	 {  0, 4932, -2 }, {  0,  max, -2 },

	 {  6, min,  -2 }, {  6,   -1, -2 },
	 {  6,   0,	4 }, {  6,   37,  4 },
	 {  6,  38,	4 }, {  6,  307,  4 },
	 {  6, 308,	4 }, {  6, 4931,  4 },
	 {  6, 4932, -2 }, {  6,  max, -2 },

	 {  7, min,  -2 }, {  7,   -1, -2 },
	 {  7,   0,	4 }, {  7,   37,  4 },
	 {  7,  38,	8 }, {  7,  307,  8 },
	 {  7, 308,	8 }, {  7, 4931,  8 },
	 {  7, 4932, -2 }, {  7,  max, -2 },

	 { 15, min,  -2 }, { 15,   -1, -2 },
	 { 15,   0,	4 }, { 15,   37,  4 },
	 { 15,  38,	8 }, { 15,  307,  8 },
	 { 15, 308,	8 }, { 15, 4931,  8 },
	 { 15, 4932, -2 }, { 15,  max, -2 },

	 { 16, min,  -2 }, { 16,   -1, -2 },
	 { 16,   0,	4 }, { 16,   37,  4 },
	 { 16,  38,	8 }, { 16,  307,  8 },
	 { 16, 308,  16 }, { 16, 4931, 16 },
	 { 16, 4932, -2 }, { 16,  max, -2 },

	 { 33, min,  -2 }, { 33,   -1, -2 },
	 { 33,   0,	4 }, { 33,   37,  4 },
	 { 33,  38,	8 }, { 33,  307,  8 },
	 { 33, 308,  16 }, { 33, 4931, 16 },
	 { 33, 4932, -2 }, { 33,  max, -2 },

	 { 34, min,  -3 }, { 34,   -1, -3 },
	 { 34,   0,  -1 }, { 34,   37, -1 },
	 { 34,  38,  -1 }, { 34,  307, -1 },
	 { 34, 308,  -1 }, { 34, 4931, -1 },
	 { 34, 4932, -3 }, { 34,  max, -3 }
	};
	const int N = ARRAY_LENGTH(tests);

	// selected_real_kind
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, selected_real_kind( tests[ i ].input1, tests[ i ].input2 ) );
	}
	// SELECTED_REAL_KIND
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, SELECTED_REAL_KIND( tests[ i ].input1, tests[ i ].input2 ) );
	}
}

TEST( NumericTest, selectedCharKind )
{
	const struct {
		const char * input;
		int output;
	} tests[] = {
	 { "DEFAULT", 1 }, { "ASCII", 1 },
	 { "default", -1 }, { "ascii", -1 }, { "", -1 },
	};
	const int N = ARRAY_LENGTH(tests);

	// selected_char_kind( std::string )
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, selected_char_kind( std::string( tests[ i ].input ) ) );
	}

	// SELECTED_CHAR_KIND( std::string )
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, SELECTED_CHAR_KIND( std::string( tests[ i ].input ) ) );
	}
}

TEST( NumericTest, selectedCharKind2 )
{
	const struct {
		char input;
		int output;
	} tests[] = {
	 { 'a', -1 }, { 'A', -1 }, { '0', -1 }, { '.', -1 },
	 { ' ', -1 }, { '\n', -1 }, { '\0', -1 },
	};
	const int N = ARRAY_LENGTH(tests);

	// selected_char_kind(char)
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, selected_char_kind( tests[ i ].input ) );
	}
	// SELECTED_CHAR_KIND(char)
	for ( int i = 0; i < N; ++i ) {
		EXPECT_EQ( tests[ i ].output, SELECTED_CHAR_KIND( tests[ i ].input ) );
	}
}

TEST( NumericTest, radix )
{
	EXPECT_EQ( std::numeric_limits< bool >::radix, radix( bool() ) );
	EXPECT_EQ( std::numeric_limits< char >::radix, radix( char() ) );
	EXPECT_EQ( std::numeric_limits< schar >::radix, radix( schar() ) );
	EXPECT_EQ( std::numeric_limits< uchar >::radix, radix( uchar() ) );
	EXPECT_EQ( std::numeric_limits< wchar_t >::radix, radix( wchar_t() ) );
	EXPECT_EQ( std::numeric_limits< char16_t >::radix, radix( char16_t() ) );
	EXPECT_EQ( std::numeric_limits< char32_t >::radix, radix( char32_t() ) );
	EXPECT_EQ( std::numeric_limits< short >::radix, radix( short() ) );
	EXPECT_EQ( std::numeric_limits< sshort >::radix, radix( sshort() ) );
	EXPECT_EQ( std::numeric_limits< ushort >::radix, radix( ushort() ) );
	EXPECT_EQ( std::numeric_limits< int >::radix, radix( int() ) );
	EXPECT_EQ( std::numeric_limits< sint >::radix, radix( sint() ) );
	EXPECT_EQ( std::numeric_limits< uint >::radix, radix( uint() ) );
	EXPECT_EQ( std::numeric_limits< long >::radix, radix( long() ) );
	EXPECT_EQ( std::numeric_limits< slong >::radix, radix( slong() ) );
	EXPECT_EQ( std::numeric_limits< ulong >::radix, radix( ulong() ) );
	EXPECT_EQ( std::numeric_limits< longlong >::radix, radix( longlong() ) );
	EXPECT_EQ( std::numeric_limits< slonglong >::radix, radix( slonglong() ) );
	EXPECT_EQ( std::numeric_limits< ulonglong >::radix, radix( ulonglong() ) );
	EXPECT_EQ( std::numeric_limits< float >::radix, radix( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::radix, radix( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::radix, radix( longdouble() ) );

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

TEST( NumericTest, digits )
{
	EXPECT_EQ( std::numeric_limits< bool >::digits, digits( bool() ) );
	EXPECT_EQ( std::numeric_limits< char >::digits, digits( char() ) );
	EXPECT_EQ( std::numeric_limits< schar >::digits, digits( schar() ) );
	EXPECT_EQ( std::numeric_limits< uchar >::digits, digits( uchar() ) );
	EXPECT_EQ( std::numeric_limits< wchar_t >::digits, digits( wchar_t() ) );
	EXPECT_EQ( std::numeric_limits< char16_t >::digits, digits( char16_t() ) );
	EXPECT_EQ( std::numeric_limits< char32_t >::digits, digits( char32_t() ) );
	EXPECT_EQ( std::numeric_limits< short >::digits, digits( short() ) );
	EXPECT_EQ( std::numeric_limits< sshort >::digits, digits( sshort() ) );
	EXPECT_EQ( std::numeric_limits< ushort >::digits, digits( ushort() ) );
	EXPECT_EQ( std::numeric_limits< int >::digits, digits( int() ) );
	EXPECT_EQ( std::numeric_limits< sint >::digits, digits( sint() ) );
	EXPECT_EQ( std::numeric_limits< uint >::digits, digits( uint() ) );
	EXPECT_EQ( std::numeric_limits< long >::digits, digits( long() ) );
	EXPECT_EQ( std::numeric_limits< slong >::digits, digits( slong() ) );
	EXPECT_EQ( std::numeric_limits< ulong >::digits, digits( ulong() ) );
	EXPECT_EQ( std::numeric_limits< longlong >::digits, digits( longlong() ) );
	EXPECT_EQ( std::numeric_limits< slonglong >::digits, digits( slonglong() ) );
	EXPECT_EQ( std::numeric_limits< ulonglong >::digits, digits( ulonglong() ) );
	EXPECT_EQ( std::numeric_limits< float >::digits, digits( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::digits, digits( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::digits, digits( longdouble() ) );

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

TEST( NumericTest, range )
{
	EXPECT_EQ( 2,  range( std::int8_t() ) );
	EXPECT_EQ( 4,  range( std::int16_t() ) );
	EXPECT_EQ( 9,  range( std::int32_t() ) );
	EXPECT_EQ( 18, range( std::int64_t() ) );

	EXPECT_EQ( 2,  range( std::uint8_t() ) );
	EXPECT_EQ( 4,  range( std::uint16_t() ) );
	EXPECT_EQ( 9,  range( std::uint32_t() ) );
	EXPECT_EQ( 18, range( std::uint64_t() ) );

	EXPECT_EQ( 37,   range( float() ) );
	EXPECT_EQ( 307,  range( double() ) );
	EXPECT_EQ( 4931, range( longdouble() ) );

	EXPECT_EQ( 37,   range( std::complex<float>() ) );
	EXPECT_EQ( 307,  range( std::complex<double>() ) );
	EXPECT_EQ( 4931, range( std::complex<long double>() ) );
}

TEST( NumericTest, RANGE )
{
	EXPECT_EQ( 2,  RANGE( std::int8_t() ) );
	EXPECT_EQ( 4,  RANGE( std::int16_t() ) );
	EXPECT_EQ( 9,  RANGE( std::int32_t() ) );
	EXPECT_EQ( 18, RANGE( std::int64_t() ) );

	EXPECT_EQ( 2,  RANGE( std::uint8_t() ) );
	EXPECT_EQ( 4,  RANGE( std::uint16_t() ) );
	EXPECT_EQ( 9,  RANGE( std::uint32_t() ) );
	EXPECT_EQ( 18, RANGE( std::uint64_t() ) );

	EXPECT_EQ( 37,   RANGE( float() ) );
	EXPECT_EQ( 307,  RANGE( double() ) );
	EXPECT_EQ( 4931, RANGE( longdouble() ) );

	EXPECT_EQ( 37,   RANGE( std::complex<float>() ) );
	EXPECT_EQ( 307,  RANGE( std::complex<double>() ) );
	EXPECT_EQ( 4931, RANGE( std::complex<long double>() ) );
}

TEST( NumericTest, huge )
{
	EXPECT_EQ( bool_max, huge( bool() ) );
	EXPECT_EQ( char_max, huge( char() ) );
	EXPECT_EQ( schar_max, huge( schar() ) );
	EXPECT_EQ( uchar_max, huge( uchar() ) );
	EXPECT_EQ( std::numeric_limits< wchar_t >::max(), huge( wchar_t() ) );
	EXPECT_EQ( std::numeric_limits< char16_t >::max(), huge( char16_t() ) );
	EXPECT_EQ( std::numeric_limits< char32_t >::max(), huge( char32_t() ) );
	EXPECT_EQ( short_max, huge( short() ) );
	EXPECT_EQ( sshort_max, huge( sshort() ) );
	EXPECT_EQ( ushort_max, huge( ushort() ) );
	EXPECT_EQ( int_max, huge( int() ) );
	EXPECT_EQ( sint_max, huge( sint() ) );
	EXPECT_EQ( uint_max, huge( uint() ) );
	EXPECT_EQ( long_max, huge( long() ) );
	EXPECT_EQ( slong_max, huge( slong() ) );
	EXPECT_EQ( ulong_max, huge( ulong() ) );
	EXPECT_EQ( longlong_max, huge( longlong() ) );
	EXPECT_EQ( slonglong_max, huge( slonglong() ) );
	EXPECT_EQ( ulonglong_max, huge( ulonglong() ) );
	EXPECT_EQ( float_max, huge( float() ) );
	EXPECT_EQ( double_max, huge( double() ) );
	EXPECT_EQ( longdouble_max, huge( longdouble() ) );
}

TEST( NumericTest, tiny )
{
	EXPECT_EQ( float_min, tiny( float() ) );
	EXPECT_EQ( double_min, tiny( double() ) );
	EXPECT_EQ( longdouble_min, tiny( longdouble() ) );

	EXPECT_EQ( float_min, TINY( float() ) );
	EXPECT_EQ( double_min, TINY( double() ) );
	EXPECT_EQ( longdouble_min, TINY( longdouble() ) );
}

TEST( NumericTest, precision )
{
	EXPECT_EQ( 6, precision( float() ) );
	EXPECT_EQ( 15, precision( double() ) );
#ifdef __GNUC__
	EXPECT_EQ( 18, precision( longdouble() ) );
#else
#ifdef _WIN32
	EXPECT_EQ( 15, precision( longdouble() ) );
#else
	EXPECT_EQ( 18, precision( longdouble() ) );
#endif
#endif

	EXPECT_EQ( 6, precision( std::complex<float>() ) );
	EXPECT_EQ( 15, precision( std::complex<double>() ) );
#ifdef __GNUC__
	EXPECT_EQ( 18, precision( std::complex<longdouble>() ) );
#else
#ifdef _WIN32
	EXPECT_EQ( 15, precision( std::complex<longdouble>() ) );
#else
	EXPECT_EQ( 18, precision( std::complex<longdouble>() ) );
#endif
#endif

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

	EXPECT_EQ( 6, PRECISION( std::complex<float>() ) );
	EXPECT_EQ( 15, PRECISION( std::complex<double>() ) );
#ifdef __GNUC__
	EXPECT_EQ( 18, PRECISION( std::complex<longdouble>() ) );
#else
#ifdef _WIN32
	EXPECT_EQ( 15, PRECISION( std::complex<longdouble>() ) );
#else
	EXPECT_EQ( 18, PRECISION( std::complex<longdouble>() ) );
#endif
#endif
}

TEST( NumericTest, minexponent )
{
	EXPECT_EQ( std::numeric_limits< float >::min_exponent, minexponent( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::min_exponent, minexponent( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::min_exponent, minexponent( longdouble() ) );

	EXPECT_EQ( std::numeric_limits< float >::min_exponent, MINEXPONENT( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::min_exponent, MINEXPONENT( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::min_exponent, MINEXPONENT( longdouble() ) );
}

TEST( NumericTest, maxexponent )
{
	EXPECT_EQ( std::numeric_limits< float >::max_exponent, maxexponent( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::max_exponent, maxexponent( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::max_exponent, maxexponent( longdouble() ) );

	EXPECT_EQ( std::numeric_limits< float >::max_exponent, MAXEXPONENT( float() ) );
	EXPECT_EQ( std::numeric_limits< double >::max_exponent, MAXEXPONENT( double() ) );
	EXPECT_EQ( std::numeric_limits< longdouble >::max_exponent, MAXEXPONENT( longdouble() ) );
}

TEST( NumericTest, exponent )
{
	EXPECT_EQ( 0, exponent( float() ) );
	EXPECT_EQ( -125, exponent( float_min ) );
	EXPECT_EQ( 129, exponent( float_max ) );
	EXPECT_EQ( 0, exponent( double() ) );
	EXPECT_EQ( -1021, exponent( double_min ) );
	EXPECT_EQ( 1025, exponent( double_max ) );
	EXPECT_EQ( 0, exponent( longdouble() ) );
#ifndef __INTEL_COMPILER // Avoid exception
#ifdef _MSC_VER // long double == double
	EXPECT_EQ( -1021, exponent( longdouble_min ) );
	EXPECT_EQ( 1025, exponent( longdouble_max ) );
#else
	EXPECT_EQ( -16381, exponent( longdouble_min ) );
	EXPECT_EQ( 16385, exponent( longdouble_max ) );
#endif
#endif
	EXPECT_EQ( 0, EXPONENT( float() ) );
	EXPECT_EQ( 0, EXPONENT( double() ) );
	EXPECT_EQ( 0, EXPONENT( longdouble() ) );
}

TEST( NumericTest, fraction )
{
	EXPECT_EQ( 0, fraction( float() ) );
	EXPECT_FLOAT_EQ( 0.5, fraction( float_min ) );
	EXPECT_FLOAT_EQ( 0.5, fraction( float_max ) );
	EXPECT_EQ( 0, fraction( double() ) );
	EXPECT_DOUBLE_EQ( 0.5, fraction( double_min ) );
	EXPECT_DOUBLE_EQ( 0.5, fraction( double_max ) );
	EXPECT_EQ( 0, fraction( longdouble() ) );
#ifndef __INTEL_COMPILER
	EXPECT_DOUBLE_EQ( 0.5, fraction( longdouble_min ) );
	EXPECT_DOUBLE_EQ( 0.5, fraction( longdouble_max ) );
#endif
	EXPECT_EQ( 0, FRACTION( float() ) );
	EXPECT_EQ( 0, FRACTION( double() ) );
	EXPECT_EQ( 0, FRACTION( longdouble() ) );
}

TEST( NumericTest, setExponent )
{
	EXPECT_EQ( 0, set_exponent( float(), 1 ) );
	EXPECT_EQ( 0, set_exponent( double(), 1 ) );
	EXPECT_EQ( 0, set_exponent( longdouble(), 1 ) );

	EXPECT_EQ( 0, SET_EXPONENT( float(), 1 ) );
	EXPECT_EQ( 0, SET_EXPONENT( double(), 1 ) );
	EXPECT_EQ( 0, SET_EXPONENT( longdouble(), 1 ) );
}
