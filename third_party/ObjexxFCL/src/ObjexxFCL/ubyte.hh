#ifndef ObjexxFCL_ubyte_hh_INCLUDED
#define ObjexxFCL_ubyte_hh_INCLUDED

// ubyte: Unsigned One-Byte Integer
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// C++ Headers
#include <cassert>
#include <cstddef>
#include <istream>
#include <ostream>

namespace ObjexxFCL {

// ubyte: One-Byte Integer
class ubyte
{

public: // Creation

	// Default Constructor
	inline
	ubyte() :
	 b_( static_cast< unsigned char >( 0 ) )
	{}

	// short Constructor
	inline
	explicit
	ubyte( unsigned short int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// short Constructor
	inline
	explicit
	ubyte( short int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// int Constructor
	inline
	explicit
	ubyte( unsigned int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// int Constructor
	inline
	explicit
	ubyte( int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// long Constructor
	inline
	explicit
	ubyte( unsigned long int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// long Constructor
	inline
	explicit
	ubyte( long int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// long long Constructor
	inline
	explicit
	ubyte( unsigned long long int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// long long Constructor
	inline
	explicit
	ubyte( long long int const i ) :
	 b_( static_cast< unsigned char >( i ) )
	{}

	// char Constructor
	inline
	explicit
	ubyte( unsigned char const c ) :
	 b_( c )
	{}

	// char Constructor
	inline
	explicit
	ubyte( char const c ) :
	 b_( c )
	{}

	// Destructor
	inline
	~ubyte()
	{}

public: // Conversion

	// short Conversion
	inline
	operator unsigned short int() const
	{
		return static_cast< unsigned short int >( b_ );
	}

public: // Assignment

	// = short
	inline
	ubyte &
	operator =( unsigned short int const i )
	{
		b_ = static_cast< unsigned char >( i );
		return *this;
	}

	// += short
	inline
	ubyte &
	operator +=( unsigned short int const i )
	{
		b_ += i;
		return *this;
	}

	// -= short
	inline
	ubyte &
	operator -=( unsigned short int const i )
	{
		b_ -= i;
		return *this;
	}

	// *= short
	inline
	ubyte &
	operator *=( unsigned short int const i )
	{
		b_ *= i;
		return *this;
	}

	// /= short
	inline
	ubyte &
	operator /=( unsigned short int const i )
	{
		assert( i != 0u );
		b_ /= i;
		return *this;
	}

public: // Incrememt/Decrement

	// ++ubyte
	inline
	ubyte &
	operator ++()
	{
		++b_;
		return *this;
	}

	// ubyte++
	inline
	ubyte const
	operator ++( int )
	{
		ubyte const old( *this );
		++b_;
		return old;
	}

	// --ubyte
	inline
	ubyte &
	operator --()
	{
		--b_;
		return *this;
	}

	// ubyte--
	inline
	ubyte const
	operator --( int )
	{
		ubyte const old( *this );
		--b_;
		return old;
	}

public: // Math

	// +ubyte
	inline
	ubyte
	operator +() const
	{
		return *this;
	}

	// -ubyte
	inline
	ubyte
	operator -() const
	{
		return ubyte( static_cast< unsigned char const >( -static_cast< short int const >( b_ ) ) );
	}

	// ubyte + ubyte
	friend
	inline
	ubyte
	operator +( ubyte const & i, ubyte const & j )
	{
		return ubyte( i.b_ + j.b_ );
	}

	// ubyte - ubyte
	friend
	inline
	ubyte
	operator -( ubyte const & i, ubyte const & j )
	{
		return ubyte( i.b_ - j.b_ );
	}

	// ubyte * ubyte
	friend
	inline
	ubyte
	operator *( ubyte const & i, ubyte const & j )
	{
		return ubyte( i.b_ * j.b_ );
	}

	// ubyte / ubyte
	friend
	inline
	ubyte
	operator /( ubyte const & i, ubyte const & j )
	{
		assert( j.b_ != 0 );
		return ubyte( i.b_ / j.b_ );
	}

public: // Bitwise Logical

	// ~ubyte
	inline
	ubyte
	operator ~() const
	{
		return ubyte( ~b_ );
	}

	// ubyte >> std::size_t
	inline
	ubyte
	operator >>( std::size_t const n ) const
	{
		return ubyte( b_ >> n );
	}

	// ubyte >> ubyte
	inline
	ubyte
	operator >>( ubyte const n ) const
	{
		return ubyte( b_ >> static_cast< unsigned short int >( n ) );
	}

	// ubyte << std::size_t
	inline
	ubyte
	operator <<( std::size_t const n ) const
	{
		return ubyte( b_ << n );
	}

	// ubyte << ubyte
	inline
	ubyte
	operator <<( ubyte const n ) const
	{
		return ubyte( b_ << static_cast< unsigned short int >( n ) );
	}

	// &= ubyte
	inline
	ubyte &
	operator &=( ubyte const & i )
	{
		b_ &= i.b_;
		return *this;
	}

	// |= ubyte
	inline
	ubyte &
	operator |=( ubyte const & i )
	{
		b_ |= i.b_;
		return *this;
	}

	// ^= ubyte
	inline
	ubyte &
	operator ^=( ubyte const & i )
	{
		b_ ^= i.b_;
		return *this;
	}

	// ubyte & ubyte
	friend
	inline
	ubyte
	operator &( ubyte const & i, ubyte const & j )
	{
		return ubyte( i.b_ & j.b_ );
	}

	// ubyte | ubyte
	friend
	inline
	ubyte
	operator |( ubyte const & i, ubyte const & j )
	{
		return ubyte( i.b_ | j.b_ );
	}

	// ubyte ^ ubyte
	friend
	inline
	ubyte
	operator ^( ubyte const & i, ubyte const & j )
	{
		return ubyte( i.b_ ^ j.b_ );
	}

public: // Comparison

	// ubyte == ubyte
	friend
	inline
	bool
	operator ==( ubyte const & i, ubyte const & j )
	{
		return ( i.b_ == j.b_ );
	}

	// ubyte != ubyte
	friend
	inline
	bool
	operator !=( ubyte const & i, ubyte const & j )
	{
		return ( i.b_ != j.b_ );
	}

	// ubyte < ubyte
	friend
	inline
	bool
	operator <( ubyte const & i, ubyte const & j )
	{
		return ( i.b_ < j.b_ );
	}

	// ubyte <= ubyte
	friend
	inline
	bool
	operator <=( ubyte const & i, ubyte const & j )
	{
		return ( i.b_ <= j.b_ );
	}

	// ubyte > ubyte
	friend
	inline
	bool
	operator >( ubyte const & i, ubyte const & j )
	{
		return ( i.b_ > j.b_ );
	}

	// ubyte >= ubyte
	friend
	inline
	bool
	operator >=( ubyte const & i, ubyte const & j )
	{
		return ( i.b_ >= j.b_ );
	}

public: // I/O

	// Stream >> ubyte
	friend
	inline
	std::istream &
	operator >>( std::istream & stream, ubyte & b )
	{
		if ( stream ) {
			unsigned short int i;
			stream >> i;
			b.b_ = static_cast< unsigned char >( i );
		}
		return stream;
	}

	// Stream << ubyte
	friend
	inline
	std::ostream &
	operator <<( std::ostream & stream, ubyte const & b )
	{
		if ( stream ) {
			stream << static_cast< unsigned short int >( b.b_ );
		}
		return stream;
	}

private: // Data

	unsigned char b_; // Value

}; // ubyte

// ubyte + ubyte
ubyte
operator +( ubyte const & i, ubyte const & j );

// ubyte - ubyte
ubyte
operator -( ubyte const & i, ubyte const & j );

// ubyte * ubyte
ubyte
operator *( ubyte const & i, ubyte const & j );

// ubyte / ubyte
ubyte
operator /( ubyte const & i, ubyte const & j );

// ubyte & ubyte
ubyte
operator &( ubyte const & i, ubyte const & j );

// ubyte | ubyte
ubyte
operator |( ubyte const & i, ubyte const & j );

// ubyte ^ ubyte
ubyte
operator ^( ubyte const & i, ubyte const & j );

// ubyte == ubyte
bool
operator ==( ubyte const & i, ubyte const & j );

// ubyte != ubyte
bool
operator !=( ubyte const & i, ubyte const & j );

// ubyte < ubyte
bool
operator <( ubyte const & i, ubyte const & j );

// ubyte <= ubyte
bool
operator <=( ubyte const & i, ubyte const & j );

// ubyte > ubyte
bool
operator >( ubyte const & i, ubyte const & j );

// ubyte >= ubyte
bool
operator >=( ubyte const & i, ubyte const & j );

// Stream >> ubyte
std::istream &
operator >>( std::istream & stream, ubyte & b );

// Stream << ubyte
std::ostream &
operator <<( std::ostream & stream, ubyte const & b );

} // ObjexxFCL

#endif // ObjexxFCL_ubyte_hh_INCLUDED
