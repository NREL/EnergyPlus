// ObjexxFCL::Fstring Unit Tests
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Fstring.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( FstringTest, Construction )
{
	{ // Default constructor and assignment
		Fstring s;
		EXPECT_EQ( ' ', s );
		EXPECT_EQ( " ", s );
		EXPECT_EQ( std::string( " " ), s );
		EXPECT_EQ( 1u, s.length() );
		EXPECT_EQ( 1u, s.size() );
		EXPECT_FALSE( s.empty() );
		s = "Dog";
		EXPECT_EQ( "D", s );
		EXPECT_EQ( std::string( "D" ), s );
		EXPECT_TRUE( s != "Cat" );
		EXPECT_EQ( 1u, s.length() );
		s.reassign( "Dog" );
		EXPECT_EQ( "Dog", s );
		EXPECT_EQ( std::string( "Dog" ), s );
		EXPECT_TRUE( s != "Cat" );
		EXPECT_EQ( 3u, s.length() );
	}

	{ // C string and Copy constructors and assignment
		Fstring s( "Fish" );
		Fstring t( s );
		EXPECT_EQ( "Fish", s );
		EXPECT_EQ( s, t );
		EXPECT_EQ( t, s );
		s = "Hook";
		t = s;
		EXPECT_EQ( "Hook", s );
		EXPECT_EQ( s, t );
		EXPECT_EQ( t, s );
		Fstring u( 3, s );
		EXPECT_EQ( "Hoo", u );
		EXPECT_EQ( 3u, u.length() );
	}

	{ // std::string constructors and predicates
		Fstring s( std::string( "Freshwater" ) );
		EXPECT_EQ( "Freshwater", s );
		EXPECT_FALSE( s.empty() );
		EXPECT_FALSE( s.is_blank() );
		EXPECT_TRUE( s.not_blank() );
		EXPECT_TRUE( s.has_any_of( "wxyz" ) );
		EXPECT_FALSE( s.has_any_of( "WXYZ" ) );
		EXPECT_TRUE( s.has_any_of( Fstring( "wxyz" ) ) );
		EXPECT_FALSE( s.has_any_of( Fstring( "WXYZ" ) ) );
		EXPECT_TRUE( s.has_any_of( std::string( "wxyz" ) ) );
		EXPECT_FALSE( s.has_any_of( std::string( "WXYZ" ) ) );
		EXPECT_TRUE( s.has_any_of( 't' ) );
		EXPECT_TRUE( s.has( 't' ) );
		EXPECT_FALSE( s.has_any_of( 'T' ) );
		EXPECT_FALSE( s.has( 'T' ) );
	}

	{ // char constructors and assignment
		Fstring s( 'x' );
		EXPECT_EQ( 'x', s );
		s = 'z';
		EXPECT_EQ( 'z', s );
	}

	{ // Length constructors and assignment
		Fstring s( 5 );
		EXPECT_EQ( 5U, s.length() );
		EXPECT_EQ( "     ", s );
		EXPECT_TRUE( s.is_blank() );
		EXPECT_FALSE( s.not_blank() );
	}

	{ // Miscellaneous constructors
		EXPECT_EQ( "A", Fstring( char( 'A' ) ) );
		EXPECT_EQ( "A", Fstring( (unsigned char)( 'A' ) ) );
		EXPECT_EQ( "A", Fstring( (signed char)( 'A' ) ) );
		EXPECT_EQ( "          ", Fstring( (short int)10 ) );
		EXPECT_EQ( "          ", Fstring( 10 ) );
		EXPECT_EQ( "          ", Fstring( 10l ) );
		EXPECT_EQ( "          ", Fstring( (unsigned short int)10 ) );
		EXPECT_EQ( "          ", Fstring( 10u ) );
		EXPECT_EQ( "          ", Fstring( 10ul ) );
	}
}

TEST( FstringTest, Assignment )
{
	Fstring s( "Fish" );
	EXPECT_EQ( "Fish", s );
	s = "Cat";
	EXPECT_EQ( "Cat", s );
}

TEST( FstringTest, CaseChange )
{
	Fstring s( "Fish" );
	s.lowercase();
	EXPECT_EQ( "fish", s );
	s.uppercase();
	EXPECT_EQ( "FISH", s );
	EXPECT_TRUE( equali( s, Fstring( "fiSh" ) ) );
	EXPECT_EQ( "fish", s.lowercased() );
	EXPECT_EQ( "FISH", s.uppercased() );
}

TEST( FstringTest, JustifyTrim )
{
	Fstring s( "  Fish " );
	EXPECT_EQ( 6u, s.len_trim() );
	EXPECT_EQ( 6u, s.len_trim_whitespace() );
	EXPECT_EQ( 6, len_trim( s ) );
	EXPECT_EQ( 6u, len_trim_whitespace( s ) );
	s.left_justify();
	EXPECT_EQ( "Fish   ", s );
	s.right_justify();
	EXPECT_EQ( "   Fish", s );
	EXPECT_EQ( "Fish   ", s.left_justified() );
	s.left_justify();
	EXPECT_EQ( "   Fish", s.right_justified() );
	s = "Bozo \t ";
	EXPECT_EQ( 6u, s.len_trim() );
	EXPECT_EQ( 4u, s.len_trim_whitespace() );
	EXPECT_EQ( "Bozo \t", s.trimmed() );
	EXPECT_EQ( "Bozo", s.trimmed_whitespace() );
	s.trim_whitespace();
	EXPECT_EQ( "Bozo", s );
}

TEST( FstringTest, Centering )
{
	{
		Fstring s( "Fish  " );
		EXPECT_EQ( " Fish ", s.centered() );
		EXPECT_EQ( " Fish ", s.center() );
		EXPECT_EQ( " Fish ", s );
	}
	{
		Fstring s( "   Dog " );
		EXPECT_EQ( "  Dog  ", s.centered() );
		EXPECT_EQ( "  Dog  ", s.center() );
		EXPECT_EQ( "  Dog  ", s );
	}
}

TEST( FstringTest, Subscripting )
{
	Fstring s( "Cat" );
	EXPECT_EQ( 'C', s[ 1 ] );
	EXPECT_EQ( 'a', s[ 2 ] );
	EXPECT_EQ( 't', s[ 3 ] );
	s[ 1 ] = 'B';
	EXPECT_EQ( "Bat", s );
	EXPECT_DEBUG_DEATH( s[ 4 ], ".*Assertion.*" ); // Out of bounds
}

TEST( FstringTest, Substring )
{
	Fstring s( "Cat" );
	EXPECT_EQ( "at", s( 2 ) );
	EXPECT_EQ( "Ca", s( 1, 2 ) );
	EXPECT_EQ( "at", s( {2} ) );
	EXPECT_EQ( "at", s( {2,_} ) );
	EXPECT_EQ( "Ca", s( {1,2} ) );
	EXPECT_EQ( "Ca", s( {_,2} ) );
//	EXPECT_DEBUG_DEATH( s( 2, 4 ), ".*Assertion.*" ); // Out of bounds: Strict Fortran compliance
	EXPECT_EQ( "at", s( 2, 4 ) ); // Clipped a la Intel Fortran
	EXPECT_EQ( "", s( 4, 9 ) ); // Zero-length string a la Intel Fortran
	EXPECT_EQ( "", s( 1, -5 ) ); // Zero-length string
}

TEST( FstringTest, SubstringAssignment )
{
	Fstring s( "Catalog" );
	s( 6, 7 ) = "ak";
	s( 3, 5 ) = s( 1, 3 ); // Assignment with overlap
	EXPECT_EQ( "CaCatak", s );
}

TEST( FstringTest, Concatenation )
{
	Fstring s( "Cat" );
	Fstring t( "Fish" );
	EXPECT_EQ( "CatFish", s + t );
}

TEST( FstringTest, Comparison )
{
	Fstring const l( "fish" );
	Fstring const m( "Fish" );
	Fstring const u( "FISH" );

	EXPECT_TRUE( l == l );
	EXPECT_TRUE( u == uppercased( l ) );
	EXPECT_FALSE( l == m );
	EXPECT_FALSE( l == u );
	EXPECT_FALSE( m == u );

	EXPECT_TRUE( equal( l, l ) );
	EXPECT_TRUE( equal( u, uppercased( l ) ) );
	EXPECT_TRUE( equal( l, m, false ) );
	EXPECT_TRUE( equal( l, u, false ) );
	EXPECT_TRUE( equal( m, u, false ) );
	EXPECT_FALSE( equal( l, m ) );
	EXPECT_FALSE( equal( l, u ) );
	EXPECT_FALSE( equal( m, u ) );
}

TEST( FstringTest, Index )
{
	Fstring s( "Cats like Fish" );
	Fstring t( "Fish" );
	EXPECT_EQ( 11, index( s, t ) );
	EXPECT_EQ( 4, index( s, 's' ) );
	EXPECT_EQ( 13, index( s, 's', true ) ); // Reverse search
	EXPECT_EQ( 0, INDEX( s, "xyz" ) );
}

TEST( FstringTest, Scan )
{
	Fstring s( "Catfish are fast" );
	Fstring t( "fish" );
	EXPECT_EQ( 4, scan( s, t ) );
	EXPECT_EQ( 15, scan( s, t, true ) ); // Reverse search
	EXPECT_EQ( 0, SCAN( s, "xyz" ) );
}

TEST( FstringTest, Verify )
{
	Fstring s( "Cat" );
	Fstring t( "Cut" );
	EXPECT_EQ( 2, verify( s, t ) );
	EXPECT_EQ( 0, verify( s, "tSaCb" ) );
}
