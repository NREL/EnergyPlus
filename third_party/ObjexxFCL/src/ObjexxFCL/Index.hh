#ifndef ObjexxFCL_Index_hh_INCLUDED
#define ObjexxFCL_Index_hh_INCLUDED

// Index: Index Class
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

// ObjexxFCL Headers
#include <ObjexxFCL/Omit.hh>

// C++ Headers
#include <cassert>
#include <iosfwd>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// Index: Index Class
class Index
{

public: // Creation

	// Default Constructor
	inline
	Index() :
	 i_init_( false ),
	 i_( 0 )
	{}

	// Copy Constructor
	inline
	Index( Index const & I ) :
	 i_init_( I.i_init_ ),
	 i_( I.i_ )
	{}

	// Index Constructor
	inline
	Index( int const i ) :
	 i_init_( true ),
	 i_( i )
	{}

	// Omit Constructor
	inline
	Index( Omit ) :
	 i_init_( false ),
	 i_( 0 )
	{}

	// Destructor
	inline
	~Index()
	{}

public: // Assignment

	// Scalar Assignment
	inline
	Index &
	operator =( int const i )
	{
		i_init_ = true;
		i_ = i;
		return *this;
	}

public: // Conversion

	// int Conversion
	inline
	operator int() const
	{
		assert( i_init_ );
		return i_;
	}

public: // Predicate

	// Initialized?
	inline
	bool
	initialized() const
	{
		return i_init_;
	}

public: // Inspector

	// Index
	inline
	int
	i() const
	{
		assert( i_init_ );
		return i_;
	}

public: // Modifier

	// Clear
	inline
	void
	clear()
	{
		i_init_ = false;
		i_ = 1;
	}

	// Index Set
	inline
	Index &
	i( int const i )
	{
		i_init_ = true;
		i_ = i;
		return *this;
	}

	// Index Set if Uninitialized
	inline
	void
	i_def( int const i )
	{
		if ( ! i_init_ ) {
			i_ = i;
			i_init_ = true;
		}
	}

	// Swap
	inline
	void
	swap( Index & I )
	{
		if ( this != &I ) {
			std::swap( i_init_, I.i_init_ );
			std::swap( i_, I.i_ );
		}
	}

private: // Data

	bool i_init_; // Index initialized?
	int i_; // Index

}; // Index

// Functions

// Swap
inline
void
swap( Index & a, Index & b )
{
	a.swap( b );
}

// Comparison

// Index == Index
inline
bool
operator ==( Index const & a, Index const & b )
{
	return ( a.initialized() && b.initialized() ? ( a.i() == b.i() ) : ! ( a.initialized() || b.initialized() ) );
}

// Index != Index
inline
bool
operator !=( Index const & a, Index const & b )
{
	return !( a == b );
}

// Index == int
inline
bool
operator ==( Index const & a, int const b )
{
	return ( a.initialized() && ( a.i() == b ) );
}

// Index != int
inline
bool
operator !=( Index const & a, int const b )
{
	return !( a == b );
}

// int == Index
inline
bool
operator ==( int const a, Index const & b )
{
	return ( b.initialized() && ( a == b.i() ) );
}

// int != Index
inline
bool
operator !=( int const a, Index const & b )
{
	return !( a == b );
}

// I/O

// Stream Input
std::istream &
operator >>( std::istream & stream, Index & a );

// Stream Output
std::ostream &
operator <<( std::ostream & stream, Index const & a );

} // ObjexxFCL

#ifndef NO_STD_SWAP_OVERLOADS

// std::swap Overloads for Efficiency
//
// Technically you cannot add functions overloads to namespace std
// but this works with most compilers and makes it much faster if someone uses
// std::swap instead of swap or ObjexxFCL::swap.

namespace std {

// std::swap( Index, Index )
inline
void
swap( ObjexxFCL::Index & a, ObjexxFCL::Index & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_Index_hh_INCLUDED
