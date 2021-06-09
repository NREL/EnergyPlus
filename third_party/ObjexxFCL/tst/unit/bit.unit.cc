// ObjexxFCL::bit Unit Tests
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
#include <ObjexxFCL/bit.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <cstdint>

// Using
using std::int8_t;
using std::int16_t;
using std::int32_t;
using std::int64_t;
using std::uint8_t;
using std::uint16_t;
using std::uint32_t;
using std::uint64_t;
using namespace ObjexxFCL;
using namespace ObjexxFCL::bit;

// Notes:
// Template args on bit_* calls is a testing convenience: Template arg deduction is normally used
// 64-bit constants need explicit cast to compile on 32-bit platforms

TEST( BitTest, Basic )
{
	EXPECT_EQ( 16, bit_shift( 4, 2 ) );
	EXPECT_EQ( 2, bit_shift( 4, -1 ) );
	EXPECT_EQ( 1, bit_shift( 4, -2 ) );
	EXPECT_EQ( 6, bit_xor( 5, 3 ) );
	EXPECT_EQ( 4, bit_set( 0, 2 ) );
	EXPECT_EQ( 32, bit_size( 42 ) );
}

TEST( BitTest, BitSize )
{
	EXPECT_EQ( 8, bit_size( int8_t() ) );
	EXPECT_EQ( 8, bit_size( uint8_t() ) );
	EXPECT_EQ( 16, bit_size( int16_t() ) );
	EXPECT_EQ( 16, bit_size( uint16_t() ) );
	EXPECT_EQ( 32, bit_size( int32_t() ) );
	EXPECT_EQ( 32, bit_size( uint32_t() ) );
	EXPECT_EQ( 64, bit_size( int64_t() ) );
	EXPECT_EQ( 64, bit_size( uint64_t() ) );
}

TEST( BitTest, BitOnes )
{
	EXPECT_EQ( bit_ones( 0 ), 0 );
	EXPECT_EQ( bit_ones( 1 ), 1 );
	EXPECT_EQ( bit_ones( 2 ), 1 );
	EXPECT_EQ( bit_ones( 3 ), 2 );
	EXPECT_EQ( bit_ones( 4 ), 1 );
	EXPECT_EQ( bit_ones( 5 ), 2 );
	EXPECT_EQ( bit_ones( 6 ), 2 );
	EXPECT_EQ( bit_ones( 7 ), 3 );
	EXPECT_EQ( bit_ones( 8 ), 1 );
}

TEST( BitTest, BitTest )
{
	EXPECT_TRUE( bit_test< uint8_t >( 0x88, 3 ) );
	EXPECT_TRUE( bit_test< uint8_t >( 0x88, 7 ) );
	EXPECT_TRUE( bit_test< uint16_t >( 0x8080, 7 ) );
	EXPECT_TRUE( bit_test< uint16_t >( 0x8080, 15 ) );
	EXPECT_TRUE( bit_test< uint32_t >( 0x80008000, 15 ) );
	EXPECT_TRUE( bit_test< uint32_t >( 0x80008000, 31 ) );
	EXPECT_TRUE( bit_test( uint64_t( 0x8000000080000000 ), uint64_t( 31 ) ) );
	EXPECT_TRUE( bit_test( uint64_t( 0x8000000080000000 ), uint64_t( 63 ) ) );
}

TEST( BitTest, BitLt )
{
	EXPECT_TRUE( bit_lt( +0, +2 ) );
	EXPECT_TRUE( bit_lt( +1, +2 ) );
	EXPECT_TRUE( bit_lt( +5, +9 ) );
	EXPECT_TRUE( bit_lt( -2, -1 ) );
	EXPECT_TRUE( bit_lt( -9, -5 ) );
	EXPECT_TRUE( bit_lt( +0, -2 ) );
	EXPECT_TRUE( bit_lt( +1, -2 ) );
	EXPECT_TRUE( bit_lt( +5, -9 ) );
	EXPECT_TRUE( bit_lt( +2, -1 ) );
	EXPECT_TRUE( bit_lt( +9, -5 ) );
	EXPECT_FALSE( bit_lt( +2, +2 ) );
	EXPECT_FALSE( bit_lt( +2, +0 ) );
	EXPECT_FALSE( bit_lt( +2, +1 ) );
	EXPECT_FALSE( bit_lt( +9, +5 ) );
	EXPECT_FALSE( bit_lt( -1, -2 ) );
	EXPECT_FALSE( bit_lt( -5, -9 ) );
	EXPECT_FALSE( bit_lt( -2, +0 ) );
	EXPECT_FALSE( bit_lt( -2, +1 ) );
	EXPECT_FALSE( bit_lt( -9, +5 ) );
	EXPECT_FALSE( bit_lt( -1, +2 ) );
	EXPECT_FALSE( bit_lt( -5, +9 ) );
}

TEST( BitTest, BitLe )
{
	EXPECT_TRUE( bit_le( +0, +2 ) );
	EXPECT_TRUE( bit_le( +1, +2 ) );
	EXPECT_TRUE( bit_le( +5, +9 ) );
	EXPECT_TRUE( bit_le( -2, -1 ) );
	EXPECT_TRUE( bit_le( -9, -5 ) );
	EXPECT_TRUE( bit_le( +0, -2 ) );
	EXPECT_TRUE( bit_le( +1, -2 ) );
	EXPECT_TRUE( bit_le( +5, -9 ) );
	EXPECT_TRUE( bit_le( +2, -1 ) );
	EXPECT_TRUE( bit_le( +9, -5 ) );
	EXPECT_TRUE( bit_le( +2, +2 ) );
	EXPECT_FALSE( bit_le( +2, +0 ) );
	EXPECT_FALSE( bit_le( +2, +1 ) );
	EXPECT_FALSE( bit_le( +9, +5 ) );
	EXPECT_FALSE( bit_le( -1, -2 ) );
	EXPECT_FALSE( bit_le( -5, -9 ) );
	EXPECT_FALSE( bit_le( -2, +0 ) );
	EXPECT_FALSE( bit_le( -2, +1 ) );
	EXPECT_FALSE( bit_le( -9, +5 ) );
	EXPECT_FALSE( bit_le( -1, +2 ) );
	EXPECT_FALSE( bit_le( -5, +9 ) );
}

TEST( BitTest, BitGe )
{
	EXPECT_TRUE( bit_ge( +2, +0 ) );
	EXPECT_TRUE( bit_ge( +2, +1 ) );
	EXPECT_TRUE( bit_ge( +9, +5 ) );
	EXPECT_TRUE( bit_ge( -1, -2 ) );
	EXPECT_TRUE( bit_ge( -5, -9 ) );
	EXPECT_TRUE( bit_ge( -2, +0 ) );
	EXPECT_TRUE( bit_ge( -2, +1 ) );
	EXPECT_TRUE( bit_ge( -9, +5 ) );
	EXPECT_TRUE( bit_ge( -1, +2 ) );
	EXPECT_TRUE( bit_ge( -5, +9 ) );
	EXPECT_TRUE( bit_ge( +2, +2 ) );
	EXPECT_FALSE( bit_ge( +0, +2 ) );
	EXPECT_FALSE( bit_ge( +1, +2 ) );
	EXPECT_FALSE( bit_ge( +5, +9 ) );
	EXPECT_FALSE( bit_ge( -2, -1 ) );
	EXPECT_FALSE( bit_ge( -9, -5 ) );
	EXPECT_FALSE( bit_ge( +0, -2 ) );
	EXPECT_FALSE( bit_ge( +1, -2 ) );
	EXPECT_FALSE( bit_ge( +5, -9 ) );
	EXPECT_FALSE( bit_ge( +2, -1 ) );
	EXPECT_FALSE( bit_ge( +9, -5 ) );
}

TEST( BitTest, BitGt )
{
	EXPECT_TRUE( bit_gt( +2, +0 ) );
	EXPECT_TRUE( bit_gt( +2, +1 ) );
	EXPECT_TRUE( bit_gt( +9, +5 ) );
	EXPECT_TRUE( bit_gt( -1, -2 ) );
	EXPECT_TRUE( bit_gt( -5, -9 ) );
	EXPECT_TRUE( bit_gt( -2, +0 ) );
	EXPECT_TRUE( bit_gt( -2, +1 ) );
	EXPECT_TRUE( bit_gt( -9, +5 ) );
	EXPECT_TRUE( bit_gt( -1, +2 ) );
	EXPECT_TRUE( bit_gt( -5, +9 ) );
	EXPECT_FALSE( bit_gt( +2, +2 ) );
	EXPECT_FALSE( bit_gt( +0, +2 ) );
	EXPECT_FALSE( bit_gt( +1, +2 ) );
	EXPECT_FALSE( bit_gt( +5, +9 ) );
	EXPECT_FALSE( bit_gt( -2, -1 ) );
	EXPECT_FALSE( bit_gt( -9, -5 ) );
	EXPECT_FALSE( bit_gt( +0, -2 ) );
	EXPECT_FALSE( bit_gt( +1, -2 ) );
	EXPECT_FALSE( bit_gt( +5, -9 ) );
	EXPECT_FALSE( bit_gt( +2, -1 ) );
	EXPECT_FALSE( bit_gt( +9, -5 ) );
}

TEST( BitTest, BitSet )
{
	EXPECT_EQ( uint8_t( 0x01 ), bit_set( uint8_t( 0 ), uint8_t( 0 ) ) );
	EXPECT_EQ( uint8_t( 0x80 ), bit_set( uint8_t( 0 ), uint8_t( 7 ) ) );
	EXPECT_EQ( uint8_t( 0x01 ), bit_set< uint8_t >( 0, 0 ) );
	EXPECT_EQ( uint8_t( 0x80 ), bit_set< uint8_t >( 0, 7 ) );
	EXPECT_EQ( uint16_t( 0x0100 ), bit_set< uint16_t >( 0, 8 ) );
	EXPECT_EQ( uint16_t( 0x8000 ), bit_set< uint16_t >( 0, 15 ) );
	EXPECT_EQ( uint32_t( 0x00010000 ), bit_set< uint32_t >( 0, 16 ) );
	EXPECT_EQ( uint32_t( 0x80000000 ), bit_set< uint32_t >( 0, 31 ) );
	EXPECT_EQ( uint64_t( 0x0000000100000000 ), bit_set< uint64_t >( 0, 32 ) );
	EXPECT_EQ( uint64_t( 0x8000000000000000 ), bit_set< uint64_t >( 0, 63 ) );
}

TEST( BitTest, BitNot )
{
	uint64_t const lo64( uint64_t( 1 ) << 32 );
	uint64_t const hi64( uint64_t( 1 ) << 63 );

	EXPECT_EQ( 0xFEu, bit_not< uint8_t >( 1 << 0 ) );
	EXPECT_EQ( 0x7Fu, bit_not< uint8_t >( 1 << 7 ) );
	EXPECT_EQ( 0xFEFFu, bit_not< uint16_t >( 1 << 8 ) );
	EXPECT_EQ( 0x7FFFu, bit_not< uint16_t >( 1 << 15 ) );
	EXPECT_EQ( 0xFFFEFFFFu, bit_not< uint32_t >( 1 << 16 ) );
	EXPECT_EQ( 0x7FFFFFFFu, bit_not< uint32_t >( 1 << 31 ) );
	EXPECT_EQ( uint64_t( 0xFFFFFFFEFFFFFFFF ), bit_not< uint64_t >( lo64 ) );
	EXPECT_EQ( uint64_t( 0x7FFFFFFFFFFFFFFF ), bit_not< uint64_t >( hi64 ) );
}

TEST( BitTest, BitAnd )
{
	EXPECT_EQ( 0xA0u, bit_and< uint8_t >( 0xF0, 0xAB ) );
	EXPECT_EQ( 0xAB00u, bit_and< uint16_t >( 0xFF00, 0xABCD ) );
	EXPECT_EQ( 0xABCD0000u, bit_and< uint32_t >( 0xFFFF0000, 0xABCD0123 ) );
	EXPECT_EQ( uint64_t( 0xABCDABCD00000000 ), bit_and( uint64_t( 0xFFFFFFFF00000000 ), uint64_t( 0xABCDABCDABCDABCD ) ) );
}

TEST( BitTest, BitOr )
{
	EXPECT_EQ( 0xFBu, bit_or< uint8_t >( 0xF0, 0xAB ) );
	EXPECT_EQ( 0xFFCDu, bit_or< uint16_t >( 0xFF00, 0xABCD ) );
	EXPECT_EQ( 0xFFFF0123u, bit_or< uint32_t >( 0xFFFF0000, 0xABCD0123 ) );
	EXPECT_EQ( uint64_t( 0xFFFFFFFFABCDABCD ), bit_or( uint64_t( 0xFFFFFFFF00000000 ), uint64_t( 0xABCDABCDABCDABCD ) ) );
}

TEST( BitTest, BitXor )
{
	EXPECT_EQ( 0x0Fu, bit_xor< uint8_t >( 0xF0, 0xFF ) );
	EXPECT_EQ( 0x0F0Fu, bit_xor< uint16_t >( 0xF0F0, 0xFFFF ) );
	EXPECT_EQ( 0x0F0F0F0Fu, bit_xor< uint32_t >( 0xF0F0F0F0, 0xFFFFFFFF ) );
	EXPECT_EQ( uint64_t( 0x0F0F0F0F0F0F0F0F ), bit_xor( uint64_t( 0xF0F0F0F0F0F0F0F0 ), uint64_t( 0xFFFFFFFFFFFFFFFF ) ) );
}

TEST( BitTest, BitShift )
{
	// Signed: Explicit casts because bit patterns are unsigned
	EXPECT_EQ( 0x02, bit_shift( int8_t( 1 ), 1 ) );
	EXPECT_EQ( 0, bit_shift( int8_t( 1 ), -1 ) );
	EXPECT_EQ( int8_t( 0xFE ), bit_shift( int8_t( -1 ), 1 ) );
	EXPECT_EQ( int8_t( 0x7F ), bit_shift( int8_t( -1 ), -1 ) );
	EXPECT_EQ( 0x0002, bit_shift( int16_t( 1 ), 1 ) );
	EXPECT_EQ( 0, bit_shift( int16_t( 1 ), -1 ) );
	EXPECT_EQ( int16_t( 0xFFFE ), bit_shift( int16_t( -1 ), 1 ) );
	EXPECT_EQ( int16_t( 0x7FFF ), bit_shift( int16_t( -1 ), -1 ) );
	EXPECT_EQ( 0x00000002, bit_shift( int32_t( 1 ), 1 ) );
	EXPECT_EQ( 0, bit_shift( int32_t( 1 ), -1 ) );
	EXPECT_EQ( int32_t( 0xFFFFFFFE ), bit_shift( int32_t( -1 ), 1 ) );
	EXPECT_EQ( int32_t( 0x7FFFFFFF ), bit_shift( int32_t( -1 ), -1 ) );
	EXPECT_EQ( int64_t( 0x0000000000000002 ), bit_shift( int64_t( 1 ), 1 ) );
	EXPECT_EQ( int64_t( 0 ), bit_shift( int64_t( 1 ), -1 ) );
	EXPECT_EQ( int64_t( 0xFFFFFFFFFFFFFFFE ), bit_shift( int64_t( -1 ), 1 ) );
	EXPECT_EQ( int64_t( 0x7FFFFFFFFFFFFFFF ), bit_shift( int64_t( -1 ), -1 ) );
	EXPECT_EQ( -2, bit_shift( -1, 1 ) );
	EXPECT_EQ( +3, bit_shift( -1, -30 ) );

	// Unsigned
	EXPECT_EQ( 0x02u, bit_shift( uint8_t( 1 ), 1 ) );
	EXPECT_EQ( 0u, bit_shift( uint8_t( 1 ), -1 ) );
	EXPECT_EQ( 0xFEu, bit_shift( uint8_t( -1 ), 1 ) );
	EXPECT_EQ( 0x7Fu, bit_shift( uint8_t( -1 ), -1 ) );
	EXPECT_EQ( 0x0002u, bit_shift( uint16_t( 1 ), 1 ) );
	EXPECT_EQ( 0u, bit_shift( uint16_t( 1 ), -1 ) );
	EXPECT_EQ( 0xFFFEu, bit_shift( uint16_t( -1 ), 1 ) );
	EXPECT_EQ( 0x7FFFu, bit_shift( uint16_t( -1 ), -1 ) );
	EXPECT_EQ( 0x00000002u, bit_shift( uint32_t( 1 ), 1 ) );
	EXPECT_EQ( 0u, bit_shift( uint32_t( 1 ), -1 ) );
	EXPECT_EQ( 0xFFFFFFFEu, bit_shift( uint32_t( -1 ), 1 ) );
	EXPECT_EQ( 0x7FFFFFFFu, bit_shift( uint32_t( -1 ), -1 ) );
	EXPECT_EQ( uint64_t( 0x0000000000000002 ), bit_shift( uint64_t( 1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0 )                 , bit_shift( uint64_t( 1 ), -1 ) );
	EXPECT_EQ( uint64_t( 0xFFFFFFFFFFFFFFFE ), bit_shift( uint64_t( -1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0x7FFFFFFFFFFFFFFF ), bit_shift( uint64_t( -1 ), -1 ) );
}

TEST( BitTest, BitBits )
{
	EXPECT_EQ( 6u, bit_bits( 12u, 1u, 4u ) );
	EXPECT_EQ( 6, bit_bits( 12, 1, 4 ) );
	EXPECT_EQ( 7u, bit_bits( 14u, 1u, 3u ) );
	EXPECT_EQ( 7, bit_bits( 14, 1, 3 ) );
	EXPECT_EQ( 3u, bit_bits( 14u, 1u, 2u ) );
	EXPECT_EQ( 3, bit_bits( 14, 1, 2 ) );
	EXPECT_EQ( 5u, bit_bits( 10u, 1u, 3u ) );
	EXPECT_EQ( 5, bit_bits( 10, 1, 3 ) );
	EXPECT_EQ( 5u, bit_bits( 10u, 1u, 7u ) );
	EXPECT_EQ( 5, bit_bits( 10, 1, 7 ) );
}

TEST( BitTest, BitMerge )
{
	EXPECT_EQ( bit_merge( +13, +18, +22 ), 4 );
	EXPECT_EQ( bit_merge( +5, +10, +41 ), 3 );
}

TEST( BitTest, BitMove )
{
	EXPECT_EQ( bit_move( 7, 2, 2, 6, 0 ), 5 );
	EXPECT_EQ( bit_move( 13, 2, 2, 6, 0 ), 7 );
}

TEST( BitTest, BitTransfer )
{
	if ( sizeof( float ) == 4u ) {
		int32_t i( 123 );
		float x;
		EXPECT_EQ( i, bit_transfer( bit_transfer( i, x ), i ) );
	}
}
