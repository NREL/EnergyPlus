#ifndef ObjexxFCL_byte_hh_INCLUDED
#define ObjexxFCL_byte_hh_INCLUDED

// byte: One-Byte Signed Integer
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
#include <cassert>
#include <cstddef>
#include <istream>
#include <ostream>

namespace ObjexxFCL {

// byte: One-Byte Integer
class byte
{

public: // Creation

	// Default Constructor
	byte() :
	 b_( static_cast< signed char >( 0 ) )
	{}

	// short Constructor
	explicit
	byte( short int const i ) :
	 b_( static_cast< signed char >( i ) )
	{}

	// int Constructor
	explicit
	byte( int const i ) :
	 b_( static_cast< signed char >( i ) )
	{}

	// long Constructor
	explicit
	byte( long int const i ) :
	 b_( static_cast< signed char >( i ) )
	{}

	// long long Constructor
	explicit
	byte( long long int const i ) :
	 b_( static_cast< signed char >( i ) )
	{}

	// char Constructor
	explicit
	byte( signed char const c ) :
	 b_( c )
	{}

	// Destructor
	~byte()
	{}

public: // Conversion

	// short Conversion
	operator short int() const
	{
		return static_cast< short int >( b_ );
	}

public: // Assignment

	// = short
	byte &
	operator =( short int const i )
	{
		b_ = static_cast< signed char >( i );
		return *this;
	}

	// += short
	byte &
	operator +=( short int const i )
	{
		b_ += i;
		return *this;
	}

	// -= short
	byte &
	operator -=( short int const i )
	{
		b_ -= i;
		return *this;
	}

	// *= short
	byte &
	operator *=( short int const i )
	{
		b_ *= i;
		return *this;
	}

	// /= short
	byte &
	operator /=( short int const i )
	{
		assert( i != 0 );
		b_ /= i;
		return *this;
	}

public: // Incrememt/Decrement

	// ++byte
	byte &
	operator ++()
	{
		++b_;
		return *this;
	}

	// byte++
	byte const
	operator ++( int )
	{
		byte const old( *this );
		++b_;
		return old;
	}

	// --byte
	byte &
	operator --()
	{
		--b_;
		return *this;
	}

	// byte--
	byte const
	operator --( int )
	{
		byte const old( *this );
		--b_;
		return old;
	}

public: // Math

	// +byte
	byte
	operator +() const
	{
		return *this;
	}

	// -byte
	byte
	operator -() const
	{
		return byte( -b_ );
	}

	// byte + byte
	friend
	byte
	operator +( byte const & i, byte const & j )
	{
		return byte( i.b_ + j.b_ );
	}

	// byte - byte
	friend
	byte
	operator -( byte const & i, byte const & j )
	{
		return byte( i.b_ - j.b_ );
	}

	// byte * byte
	friend
	byte
	operator *( byte const & i, byte const & j )
	{
		return byte( i.b_ * j.b_ );
	}

	// byte / byte
	friend
	byte
	operator /( byte const & i, byte const & j )
	{
		assert( j.b_ != 0 );
		return byte( i.b_ / j.b_ );
	}

public: // Bitwise Logical

	// ~byte
	byte
	operator ~() const
	{
		return byte( ~b_ );
	}

	// byte >> std::size_t
	byte
	operator >>( std::size_t const n ) const
	{
		return byte( b_ >> n );
	}

	// byte >> byte
	byte
	operator >>( byte const n ) const
	{
		return byte( b_ >> static_cast< short int >( n ) );
	}

	// byte << std::size_t
	byte
	operator <<( std::size_t const n ) const
	{
		return byte( b_ << n );
	}

	// byte << byte
	byte
	operator <<( byte const n ) const
	{
		return byte( b_ << static_cast< short int >( n ) );
	}

	// &= byte
	byte &
	operator &=( byte const & i )
	{
		b_ &= i.b_;
		return *this;
	}

	// |= byte
	byte &
	operator |=( byte const & i )
	{
		b_ |= i.b_;
		return *this;
	}

	// ^= byte
	byte &
	operator ^=( byte const & i )
	{
		b_ ^= i.b_;
		return *this;
	}

	// byte & byte
	friend
	byte
	operator &( byte const & i, byte const & j )
	{
		return byte( i.b_ & j.b_ );
	}

	// byte | byte
	friend
	byte
	operator |( byte const & i, byte const & j )
	{
		return byte( i.b_ | j.b_ );
	}

	// byte ^ byte
	friend
	byte
	operator ^( byte const & i, byte const & j )
	{
		return byte( i.b_ ^ j.b_ );
	}

public: // Comparison

	// byte == byte
	friend
	bool
	operator ==( byte const & i, byte const & j )
	{
		return ( i.b_ == j.b_ );
	}

	// byte != byte
	friend
	bool
	operator !=( byte const & i, byte const & j )
	{
		return ( i.b_ != j.b_ );
	}

	// byte < byte
	friend
	bool
	operator <( byte const & i, byte const & j )
	{
		return ( i.b_ < j.b_ );
	}

	// byte <= byte
	friend
	bool
	operator <=( byte const & i, byte const & j )
	{
		return ( i.b_ <= j.b_ );
	}

	// byte > byte
	friend
	bool
	operator >( byte const & i, byte const & j )
	{
		return ( i.b_ > j.b_ );
	}

	// byte >= byte
	friend
	bool
	operator >=( byte const & i, byte const & j )
	{
		return ( i.b_ >= j.b_ );
	}

public: // I/O

	// Stream >> byte
	friend
	std::istream &
	operator >>( std::istream & stream, byte & b )
	{
		if ( stream ) {
			short int i;
			stream >> i;
			b.b_ = static_cast< signed char >( i );
		}
		return stream;
	}

	// Stream << byte
	friend
	std::ostream &
	operator <<( std::ostream & stream, byte const & b )
	{
		if ( stream ) {
			stream << static_cast< short int >( b.b_ );
		}
		return stream;
	}

private: // Data

	signed char b_; // Value

}; // byte

// Types
typedef  byte  sbyte;

// byte + byte
byte
operator +( byte const & i, byte const & j );

// byte - byte
byte
operator -( byte const & i, byte const & j );

// byte * byte
byte
operator *( byte const & i, byte const & j );

// byte / byte
byte
operator /( byte const & i, byte const & j );

// byte & byte
byte
operator &( byte const & i, byte const & j );

// byte | byte
byte
operator |( byte const & i, byte const & j );

// byte ^ byte
byte
operator ^( byte const & i, byte const & j );

// byte == byte
bool
operator ==( byte const & i, byte const & j );

// byte != byte
bool
operator !=( byte const & i, byte const & j );

// byte < byte
bool
operator <( byte const & i, byte const & j );

// byte <= byte
bool
operator <=( byte const & i, byte const & j );

// byte > byte
bool
operator >( byte const & i, byte const & j );

// byte >= byte
bool
operator >=( byte const & i, byte const & j );

// Stream >> byte
std::istream &
operator >>( std::istream & stream, byte & b );

// Stream << byte
std::ostream &
operator <<( std::ostream & stream, byte const & b );

} // ObjexxFCL

#endif // ObjexxFCL_byte_hh_INCLUDED
