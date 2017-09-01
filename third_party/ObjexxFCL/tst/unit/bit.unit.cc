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
	EXPECT_EQ( 16, bit_shiftl( 4, 2 ) );
	EXPECT_EQ( 2, bit_shift( 4, -1 ) );
	EXPECT_EQ( 2, bit_shiftr( 4, 1 ) );
	EXPECT_EQ( 1, bit_shift( 4, -2 ) );
	EXPECT_EQ( 1, bit_shiftr( 4, 2 ) );
	EXPECT_EQ( 1, bit_cshift( 2, -1 ) );
	EXPECT_EQ( 1, bit_cshift( 4, -2 ) );
	EXPECT_EQ( 4, bit_cshift( 4, 32 ) );
	EXPECT_EQ( 4, bit_cshift( 4, -32 ) );
	EXPECT_EQ( 16, bit_ashift( 4, 2 ) );
	EXPECT_EQ( -16, bit_ashift( -4, 2 ) );
	EXPECT_EQ( 1, bit_ashift( 4, -2 ) );
	EXPECT_EQ( -1, bit_ashift( -4, -2 ) );
	EXPECT_EQ( 16, bit_ashiftl( 4, 2 ) );
	EXPECT_EQ( -16, bit_ashiftl( -4, 2 ) );
	EXPECT_EQ( 1, bit_ashiftr( 4, 2 ) );
	EXPECT_EQ( -1, bit_ashiftr( -4, 2 ) );
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

TEST( BitTest, BitLeadingZeros )
{
	EXPECT_EQ( bit_leading_zeros( 0 ), 32 );
	EXPECT_EQ( bit_leading_zeros( 1 ), 31 );
	EXPECT_EQ( bit_leading_zeros( 2 ), 30 );
	EXPECT_EQ( bit_leading_zeros( 3 ), 30 );
	EXPECT_EQ( bit_leading_zeros( 4 ), 29 );
	EXPECT_EQ( bit_leading_zeros( 5 ), 29 );
	EXPECT_EQ( bit_leading_zeros( 6 ), 29 );
	EXPECT_EQ( bit_leading_zeros( 7 ), 29 );
	EXPECT_EQ( bit_leading_zeros( 8 ), 28 );
	EXPECT_EQ( bit_leading_zeros( 11 ), 28 );
	EXPECT_EQ( bit_leading_zeros( 15 ), 28 );
	EXPECT_EQ( bit_leading_zeros( 16 ), 27 );
}

TEST( BitTest, BitTrailingZeros )
{
	EXPECT_EQ( bit_trailing_zeros( 0 ), 32 );
	EXPECT_EQ( bit_trailing_zeros( 1 ), 0 );
	EXPECT_EQ( bit_trailing_zeros( 2 ), 1 );
	EXPECT_EQ( bit_trailing_zeros( 3 ), 0 );
	EXPECT_EQ( bit_trailing_zeros( 4 ), 2 );
	EXPECT_EQ( bit_trailing_zeros( 5 ), 0 );
	EXPECT_EQ( bit_trailing_zeros( 6 ), 1 );
	EXPECT_EQ( bit_trailing_zeros( 7 ), 0 );
	EXPECT_EQ( bit_trailing_zeros( 8 ), 3 );
	EXPECT_EQ( bit_trailing_zeros( 9 ), 0 );
	EXPECT_EQ( bit_trailing_zeros( 10 ), 1 );
	EXPECT_EQ( bit_trailing_zeros( 11 ), 0 );
	EXPECT_EQ( bit_trailing_zeros( 12 ), 2 );
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

TEST( BitTest, BitParity )
{
	EXPECT_EQ( bit_parity( 0 ), 0 );
	EXPECT_EQ( bit_parity( 1 ), 1 );
	EXPECT_EQ( bit_parity( 2 ), 1 );
	EXPECT_EQ( bit_parity( 3 ), 0 );
	EXPECT_EQ( bit_parity( 4 ), 1 );
	EXPECT_EQ( bit_parity( 5 ), 0 );
	EXPECT_EQ( bit_parity( 6 ), 0 );
	EXPECT_EQ( bit_parity( 7 ), 1 );
	EXPECT_EQ( bit_parity( 8 ), 1 );
	EXPECT_EQ( bit_parity( 150 ), 0 ); // 0b10010110 in C++14
	EXPECT_EQ( bit_parity( 214 ), 1 ); // 0b11010110 in C++14
}

TEST( BitTest, BitMaskl )
{
	EXPECT_EQ( bit_maskl( 0u ), 0u );
	EXPECT_EQ( bit_maskl( 1u ), 1u << 31 );
	EXPECT_EQ( bit_maskl( 2u ), ( 1u << 31 ) + ( 1u << 30 ) );
	EXPECT_EQ( bit_maskl( 3u ), ( 1u << 31 ) + ( 1u << 30 ) + ( 1u << 29 ) );
	EXPECT_EQ( bit_maskl( 0 ), 0 );
	EXPECT_EQ( bit_maskl( 1 ), 1 << 31 );
	EXPECT_EQ( bit_maskl( 2 ), ( 1 << 31 ) + ( 1 << 30 ) );
	EXPECT_EQ( bit_maskl( 3 ), ( 1 << 31 ) + ( 1 << 30 ) + ( 1 << 29 ) );
}

TEST( BitTest, BitMaskr )
{
	EXPECT_EQ( bit_maskr( 0 ), 0 );
	EXPECT_EQ( bit_maskr( 1 ), 1 );
	EXPECT_EQ( bit_maskr( 2 ), 3 );
	EXPECT_EQ( bit_maskr( 3 ), 7 );
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

TEST( BitTest, BitClr )
{
	EXPECT_EQ( uint8_t( 0xFE ), bit_clr< uint8_t >( ~0, 0 ) );
	EXPECT_EQ( uint8_t( 0x7F ), bit_clr< uint8_t >( ~0, 7 ) );
	EXPECT_EQ( uint16_t( 0xFEFF ), bit_clr< uint16_t >( ~0, 8 ) );
	EXPECT_EQ( uint16_t( 0x7FFF ), bit_clr< uint16_t >( ~0, 15 ) );
	EXPECT_EQ( uint32_t( 0xFFFEFFFF ), bit_clr< uint32_t >( ~0, 16 ) );
	EXPECT_EQ( uint32_t( 0x7FFFFFFF ), bit_clr< uint32_t >( ~0, 31 ) );
	EXPECT_EQ( uint64_t( 0xFFFFFFFEFFFFFFFF ), bit_clr< uint64_t >( ~0, 32 ) );
	EXPECT_EQ( uint64_t( 0x7FFFFFFFFFFFFFFF ), bit_clr< uint64_t >( ~0, 63 ) );
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

TEST( BitTest, BitShiftL )
{
	// Signed: Explicit casts because bit patterns are unsigned
	EXPECT_EQ( 0x02, bit_shiftl( int8_t( 1 ), 1 ) );
	EXPECT_EQ( int8_t( 0xFE ), bit_shiftl( int8_t( -1 ), 1 ) );
	EXPECT_EQ( 0x0002, bit_shiftl( int16_t( 1 ), 1 ) );
	EXPECT_EQ( int16_t( 0xFFFE ), bit_shiftl( int16_t( -1 ), 1 ) );
	EXPECT_EQ( 0x00000002, bit_shiftl( int32_t( 1 ), 1 ) );
	EXPECT_EQ( int32_t( 0xFFFFFFFE ), bit_shiftl( int32_t( -1 ), 1 ) );
	EXPECT_EQ( int64_t( 0x0000000000000002 ), bit_shiftl( int64_t( 1 ), 1 ) );
	EXPECT_EQ( int64_t( 0xFFFFFFFFFFFFFFFE ), bit_shiftl( int64_t( -1 ), 1 ) );
	EXPECT_EQ( -2, bit_shiftl( -1, 1 ) );

	// Unsigned
	EXPECT_EQ( 0x02u, bit_shiftl( uint8_t( 1 ), 1 ) );
	EXPECT_EQ( 0xFEu, bit_shiftl( uint8_t( -1 ), 1 ) );
	EXPECT_EQ( 0x0002u, bit_shiftl( uint16_t( 1 ), 1 ) );
	EXPECT_EQ( 0xFFFEu, bit_shiftl( uint16_t( -1 ), 1 ) );
	EXPECT_EQ( 0x00000002u, bit_shiftl( uint32_t( 1 ), 1 ) );
	EXPECT_EQ( 0xFFFFFFFEu, bit_shiftl( uint32_t( -1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0x0000000000000002 ), bit_shiftl( uint64_t( 1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0xFFFFFFFFFFFFFFFE ), bit_shiftl( uint64_t( -1 ), 1 ) );
}

TEST( BitTest, BitShiftR )
{
	// Signed: Explicit casts because bit patterns are unsigned
	EXPECT_EQ( 0, bit_shiftr( int8_t( 1 ), 1 ) );
	EXPECT_EQ( int8_t( 0x7F ), bit_shiftr( int8_t( -1 ), 1 ) );
	EXPECT_EQ( 0, bit_shiftr( int16_t( 1 ), 1 ) );
	EXPECT_EQ( int16_t( 0x7FFF ), bit_shiftr( int16_t( -1 ), 1 ) );
	EXPECT_EQ( 0, bit_shiftr( int32_t( 1 ), 1 ) );
	EXPECT_EQ( int32_t( 0x7FFFFFFF ), bit_shiftr( int32_t( -1 ), 1 ) );
	EXPECT_EQ( int64_t( 0 ), bit_shiftr( int64_t( 1 ), 1 ) );
	EXPECT_EQ( int64_t( 0x7FFFFFFFFFFFFFFF ), bit_shiftr( int64_t( -1 ), 1 ) );
	EXPECT_EQ(  3, bit_shiftr( -1, 30 ) );

	// Unsigned
	EXPECT_EQ( 0u, bit_shiftr( uint8_t( 1 ), 1 ) );
	EXPECT_EQ( 0x7Fu, bit_shiftr( uint8_t( -1 ), 1 ) );
	EXPECT_EQ( 0u, bit_shiftr( uint16_t( 1 ), 1 ) );
	EXPECT_EQ( 0x7FFFu, bit_shiftr( uint16_t( -1 ), 1 ) );
	EXPECT_EQ( 0u, bit_shiftr( uint32_t( 1 ), 1 ) );
	EXPECT_EQ( 0x7FFFFFFFu, bit_shiftr( uint32_t( -1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0 ), bit_shiftr( uint64_t( 1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0x7FFFFFFFFFFFFFFF ), bit_shiftr( uint64_t( -1 ), 1 ) );
}

TEST( BitTest, BitCShift )
{
	// Signed: Explicit casts because bit patterns are unsigned
	EXPECT_EQ( 0x02, bit_cshift( int8_t( 1 ), 1 ) );
	EXPECT_EQ( int8_t( 0x80 ), bit_cshift( int8_t( 1 ), -1 ) );
	EXPECT_EQ( int8_t( 0xFF ), bit_cshift( int8_t( -1 ), 1 ) );
	EXPECT_EQ( int8_t( 0xFF ), bit_cshift( int8_t( -1 ), -1 ) );
	EXPECT_EQ( 0x0002, bit_cshift( int16_t( 1 ), 1 ) );
	EXPECT_EQ( int16_t( 0x8000 ), bit_cshift( int16_t( 1 ), -1 ) );
	EXPECT_EQ( int16_t( 0xFFFF ), bit_cshift( int16_t( -1 ), 1 ) );
	EXPECT_EQ( int16_t( 0xFFFF ), bit_cshift( int16_t( -1 ), -1 ) );
	EXPECT_EQ( 0x00000002, bit_cshift( int32_t( 1 ), 1 ) );
	EXPECT_EQ( int32_t( 0x80000000 ), bit_cshift( int32_t( 1 ), -1 ) );
	EXPECT_EQ( int32_t( 0xFFFFFFFF ), bit_cshift( int32_t( -1 ), 1 ) );
	EXPECT_EQ( int32_t( 0xFFFFFFFF ), bit_cshift( int32_t( -1 ), -1 ) );
	EXPECT_EQ( int64_t( 0x0000000000000002 ), bit_cshift( int64_t( 1 ), 1 ) );
	EXPECT_EQ( int64_t( 0x8000000000000000 ), bit_cshift( int64_t( 1 ), -1 ) );
	EXPECT_EQ( int64_t( 0xFFFFFFFFFFFFFFFF ), bit_cshift( int64_t( -1 ), 1 ) );
	EXPECT_EQ( int64_t( 0xFFFFFFFFFFFFFFFF ), bit_cshift( int64_t( -1 ), -1 ) );
	EXPECT_EQ( -1, bit_cshift( -1, 1 ) );
	EXPECT_EQ( -1, bit_cshift( -1, -1 ) );

	// Unsigned
	EXPECT_EQ( 0x02u, bit_cshift( uint8_t( 1 ), 1 ) );
	EXPECT_EQ( 0x80u, bit_cshift( uint8_t( 1 ), -1 ) );
	EXPECT_EQ( 0xFFu, bit_cshift( uint8_t( -1 ), 1 ) );
	EXPECT_EQ( 0xFFu, bit_cshift( uint8_t( -1 ), -1 ) );
	EXPECT_EQ( 0x0002u, bit_cshift( uint16_t( 1 ), 1 ) );
	EXPECT_EQ( 0x8000u, bit_cshift( uint16_t( 1 ), -1 ) );
	EXPECT_EQ( 0xFFFFu, bit_cshift( uint16_t( -1 ), 1 ) );
	EXPECT_EQ( 0xFFFFu, bit_cshift( uint16_t( -1 ), -1 ) );
	EXPECT_EQ( 0x00000002u, bit_cshift( uint32_t( 1 ), 1 ) );
	EXPECT_EQ( 0x80000000u, bit_cshift( uint32_t( 1 ), -1 ) );
	EXPECT_EQ( 0xFFFFFFFFu, bit_cshift( uint32_t( -1 ), 1 ) );
	EXPECT_EQ( 0xFFFFFFFFu, bit_cshift( uint32_t( -1 ), -1 ) );
	EXPECT_EQ( uint64_t( 0x0000000000000002 ), bit_cshift( uint64_t( 1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0x8000000000000000 ), bit_cshift( uint64_t( 1 ), -1 ) );
	EXPECT_EQ( uint64_t( 0xFFFFFFFFFFFFFFFF ), bit_cshift( uint64_t( -1 ), 1 ) );
	EXPECT_EQ( uint64_t( 0xFFFFFFFFFFFFFFFF ), bit_cshift( uint64_t( -1 ), -1 ) );
}

TEST( BitTest, BitAshift )
{
	EXPECT_EQ( bit_ashift( +4, -2 ), 1 );
	EXPECT_EQ( bit_ashift( -4, -2 ), -1 );
	EXPECT_EQ( bit_ashift( +4, +2 ), 16 );
	EXPECT_EQ( bit_ashift( -4, +2 ), -16 );
}

TEST( BitTest, BitAshiftl )
{
	EXPECT_EQ( bit_ashiftl( +4, +2 ), 16 );
	EXPECT_EQ( bit_ashiftl( -4, +2 ), -16 );
}

TEST( BitTest, BitAshiftr )
{
	EXPECT_EQ( bit_ashiftr( +4, +2 ), 1 );
	EXPECT_EQ( bit_ashiftr( -4, +2 ), -1 );
}

TEST( BitTest, BitDshiftl )
{
	EXPECT_EQ( bit_dshiftl( 5, 3, 0 ), 5 );
	EXPECT_EQ( bit_dshiftl( 1, 1, 3 ), 8 );
	EXPECT_EQ( bit_dshiftl( 1, 2, 5 ), 32 );
	EXPECT_EQ( bit_dshiftl( 9, 12, 10 ), 9216 );
	EXPECT_EQ( bit_dshiftl( 5, 1 << 15, 10 ), 5120 );
}

TEST( BitTest, BitDshiftr )
{
	EXPECT_EQ( bit_dshiftr( 5, 3, 0 ), 3 );
	EXPECT_EQ( bit_dshiftr( 0u, 1u, 1 ), 0u );
	EXPECT_EQ( bit_dshiftr( 0u, 1u, 1 ), 0u );
	EXPECT_EQ( bit_dshiftr( 1u, 0u, 1 ), 1u << 31 );
	EXPECT_EQ( bit_dshiftr( 1u, 1u, 1 ), 1u << 31 );
	EXPECT_EQ( bit_dshiftr( 1u, 2u, 1 ), ( 1u << 31 ) + 1 );
	EXPECT_EQ( bit_dshiftr( 1u, 3u, 1 ), ( 1u << 31 ) + 1 );
	EXPECT_EQ( bit_dshiftr( 1u, 4u, 1 ), ( 1u << 31 ) + 2 );
	EXPECT_EQ( bit_dshiftr( 1u, 5u, 1 ), ( 1u << 31 ) + 2 );
	EXPECT_EQ( bit_dshiftr( 1u, 6u, 1 ), ( 1u << 31 ) + 3 );
	EXPECT_EQ( bit_dshiftr( 1, 1, 1 ), int( 1u << 31 ) );
	EXPECT_EQ( bit_dshiftr( 1, 2, 1 ), int( 1u << 31 ) + 1 );
	EXPECT_EQ( bit_dshiftr( 1, 3, 1 ), int( 1u << 31 ) + 1 );
	EXPECT_EQ( bit_dshiftr( 1, 4, 1 ), int( 1u << 31 ) + 2 );
	EXPECT_EQ( bit_dshiftr( 1, 5, 1 ), int( 1u << 31 ) + 2 );
	EXPECT_EQ( bit_dshiftr( 1, 6, 1 ), int( 1u << 31 ) + 3 );
	EXPECT_EQ( bit_dshiftr( 1u, 1u, 3 ), 536870912u );
	EXPECT_EQ( bit_dshiftr( -50, -128, 30 ), -197 );
	EXPECT_EQ( bit_dshiftr( 1, 2, 5 ), 134217728 );
	EXPECT_EQ( bit_dshiftr( 9, 12, 10 ), 37748736 );
	EXPECT_EQ( bit_dshiftr( 5, 1 << 15, 10 ), 20971552 );
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
