#ifndef ObjexxFCL_DimensionSlice_hh_INCLUDED
#define ObjexxFCL_DimensionSlice_hh_INCLUDED

// DimensionSlice: Dimension Slice Class
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

// ObjexxFCL Headers
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/IndexSlice.hh>

// C++ Headers
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <limits>

namespace ObjexxFCL {

// DimensionSlice: Dimension Slice Class
class DimensionSlice
{

public: // Types

	// STL style
	typedef  std::size_t  size_type;

	// C++ style
	typedef  std::size_t  Size;

public: // Creation

	// Default Constructor
	DimensionSlice() :
	 m_( 1 ),
	 k_( 0 ),
	 u_( 0 )
	{}

	// Index Slice + Multiplier Constructor
	DimensionSlice( IndexSlice const & slice, std::int64_t const multiplier = 1 ) :
	 m_( slice.s() * multiplier ),
	 k_( slice.l() * multiplier - m_ ),
	 u_( slice.isize() )
	{
		assert( slice.initialized() );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) );
	}

	// Index Slice + Multiplier Constructor
	DimensionSlice( IndexSlice const & slice, size_type const multiplier ) :
	 m_( slice.s() * multiplier ),
	 k_( slice.l() * multiplier - m_ ),
	 u_( slice.isize() )
	{
		assert( slice.initialized() );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) ) );
	}

	// Index Slice + Multiplier Constructor
	template< typename M >
	DimensionSlice( IndexSlice const & slice, M const multiplier ) :
	 m_( slice.s() * multiplier ),
	 k_( slice.l() * multiplier - m_ ),
	 u_( slice.isize() )
	{
		assert( slice.initialized() );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) );
	}

	// Index Range + Index Slice + Multiplier Constructor
	DimensionSlice( IndexRange const & range, IndexSlice slice, std::int64_t const multiplier = 1 )
	{
		slice.lud( range.l(), range.u() );
		assert( slice.initialized() );
		assert( slice.empty() || range.contains( slice.l(), slice.last() ) );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) );
		m_ = slice.s() * multiplier;
		k_ = ( slice.l() * multiplier ) - m_;
		u_ = slice.isize();
	}

	// Index Range + Index Slice + Multiplier Constructor
	DimensionSlice( IndexRange const & range, IndexSlice slice, size_type const multiplier )
	{
		slice.lud( range.l(), range.u() );
		assert( slice.initialized() );
		assert( slice.empty() || range.contains( slice.l(), slice.last() ) );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) ) );
		m_ = slice.s() * multiplier;
		k_ = ( slice.l() * multiplier ) - m_;
		u_ = slice.isize();
	}

	// Index Range + Index Slice + Multiplier Constructor
	template< typename M >
	DimensionSlice( IndexRange const & range, IndexSlice slice, M const multiplier )
	{
		slice.lud( range.l(), range.u() );
		assert( slice.initialized() );
		assert( slice.empty() || range.contains( slice.l(), slice.last() ) );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) );
		m_ = slice.s() * multiplier;
		k_ = ( slice.l() * multiplier ) - m_;
		u_ = slice.isize();
	}

	// Upper Index + Index Slice + Multiplier Constructor
	DimensionSlice( int const u, IndexSlice slice, std::int64_t const multiplier = 1 )
	{
		assert( u >= 0 );
		slice.lud( 1, u );
		assert( slice.initialized() );
		assert( slice.empty() || ( in_range( u, slice.l() ) && in_range( u, slice.last() ) ) );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) );
		m_ = slice.s() * multiplier;
		k_ = ( slice.l() * multiplier ) - m_;
		u_ = slice.isize();
	}

	// Upper Index + Index Slice + Multiplier Constructor
	DimensionSlice( int const u, IndexSlice slice, size_type const multiplier )
	{
		assert( u >= 0 );
		slice.lud( 1, u );
		assert( slice.initialized() );
		assert( slice.empty() || ( in_range( u, slice.l() ) && in_range( u, slice.last() ) ) );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) ) );
		m_ = slice.s() * multiplier;
		k_ = ( slice.l() * multiplier ) - m_;
		u_ = slice.isize();
	}

	// Upper Index + Index Slice + Multiplier Constructor
	template< typename M >
	DimensionSlice( int const u, IndexSlice slice, M const multiplier )
	{
		assert( u >= 0 );
		slice.lud( 1, u );
		assert( slice.initialized() );
		assert( slice.empty() || ( in_range( u, slice.l() ) && in_range( u, slice.last() ) ) );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( slice.s() ) );
		m_ = slice.s() * multiplier;
		k_ = ( slice.l() * multiplier ) - m_;
		u_ = slice.isize();
	}

	// Index Range + Multiplier Full Range Constructor
	DimensionSlice( IndexRange const & range, size_type const multiplier = 1 )
	{
		assert( range.bounded() );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() ) );
		m_ = multiplier;
		k_ = ( range.l() - 1 ) * multiplier;
		u_ = range.isize();
	}

	// Destructor
	~DimensionSlice()
	{}

public: // Inspector

	// Multiplier
	std::int64_t
	m() const
	{
		return m_;
	}

	// Constant
	std::int64_t
	k() const
	{
		return k_;
	}

	// Upper Index
	int
	u() const
	{
		return u_;
	}

	// Size
	size_type
	z() const
	{
		return u_;
	}

	// Size
	size_type
	size() const
	{
		return u_;
	}

public: // Modifier

	// Clear
	void
	clear()
	{
		m_ = 1;
		k_ = 0;
		u_ = 0;
	}

protected: // Static Methods

	// Is Index in Range of [1,u]
	static
	bool
	in_range( int const u, int const i )
	{
		return ( ( 1 <= i ) && ( i <= u ) );
	}

private: // Data

	std::int64_t m_; // Multiplier
	std::int64_t k_; // Constant
	int u_; // Upper index

}; // DimensionSlice

} // ObjexxFCL

#endif // ObjexxFCL_DimensionSlice_hh_INCLUDED
