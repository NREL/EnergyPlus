// IndexRange: Index Range Class
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

// C++ Headers
#include <istream>
#include <ostream>

namespace ObjexxFCL {

	// Contains Another IndexRange?
	bool
	IndexRange::contains( IndexRange const & I ) const
	{
		if ( l_ <= u_ ) { // Bounded with positive size
			if ( I.l_ <= I.u_ ) { // I is bounded with positive size
				return ( ( l_ <= I.l_ ) && ( I.u_ <= u_ ) );
			} else if ( I.l_ - 1 == I.u_ ) { // I size is zero
				return true;
			} else { // I is unbounded
				return false;
			}
		} else if ( l_ - 1 == u_ ) { // Zero size
			return ( I.l_ - 1 == I.u_ ); // Only "contains" the empty range
		} else { // Unbounded
			return ( l_ <= I.l_ );
		}
	}

	// Intersects Another IndexRange?
	bool
	IndexRange::intersects( IndexRange const & I ) const
	{
		if ( l_ <= u_ ) { // Bounded with positive size
			if ( I.l_ <= I.u_ ) { // I is bounded with positive size
				return ( ( l_ >= I.l_ ? l_ : I.l_ ) <= ( u_ <= I.u_ ? u_ : I.u_ ) );
			} else if ( I.l_ - 1 == I.u_ ) { // I size is zero
				return false;
			} else { // I is unbounded
				return ( I.l_ <= u_ );
			}
		} else if ( l_ - 1 == u_ ) { // Zero size
			return false; // Intersection with anything is empty
		} else { // Unbounded
			if ( I.l_ <= I.u_ ) { // I is bounded with positive size
				return ( l_ <= I.u_ );
			} else if ( I.l_ - 1 == I.u_ ) { // I size is zero
				return false;
			} else { // I is unbounded
				return true;
			}
		}
	}

	// Expand to Contain Another IndexRange
	IndexRange &
	IndexRange::contain( IndexRange const & I )
	{
		if ( I.positive() ) {
			if ( bounded() ) { // Bounded
				if ( l_ > I.l_ ) l_ = I.l_;
				if ( I.bounded() ) { // I bounded
					if ( u_ < I.u_ ) u_ = I.u_;
					size_ = u_ - l_ + 1;
				} else { // I unbounded: Make this IndexRange unbounded
					u_ = l_ - 2;
					size_ = npos;
				}
			} else { // Unbounded
				if ( l_ > I.l_ ) {
					l_ = I.l_;
					u_ = l_ - 2; // Reset u_ to maintain unbounded state
				}
			}
		}
		assert( legal() );
		return *this;
	}

	// Intersect with Another IndexRange
	IndexRange &
	IndexRange::intersect( IndexRange const & I )
	{
		if ( intersects( I ) ) { // I and this IndexRange have positive size
			if ( l_ <= u_ ) { // Bounded with positive size
				if ( l_ < I.l_ ) l_ = I.l_;
				if ( ( I.l_ <= I.u_ ) && ( u_ > I.u_ ) ) u_ = I.u_;
				size_ = u_ - l_ + 1;
			} else { // Unbounded
				if ( l_ < I.l_ ) l_ = I.l_;
				if ( I.l_ <= I.u_ ) { // I is bounded with positive size
					u_ = I.u_;
					size_ = u_ - l_ + 1;
				} else { // Reset u_ to maintain unbounded state
					u_ = l_ - 2;;
				}
			}
		} else { // Empty intersection: Set zero size
			l_ = 1;
			u_ = 0;
			size_ = 0u;
		}
		assert( legal() );
		return *this;
	}

	// Stream >> IndexRange
	std::istream &
	operator >>( std::istream & stream, IndexRange & I )
	{
		int l, u;
		stream >> l >> u;
		I.assign( l, u );
		return stream;
	}

	// Stream << IndexRange
	std::ostream &
	operator <<( std::ostream & stream, IndexRange const & I )
	{
		if ( I.bounded() ) {
			stream << '[' << I.l() << ':' << I.u() << ']';
		} else { // Unbounded
			stream << '[' << I.l() << ":*]";
		}
		return stream;
	}

	// Static Data Member Definitions
	IndexRange::size_type const IndexRange::npos = static_cast< size_type >( -1 ); // Unbounded "size"
	int const IndexRange::l_min = -( static_cast< int >( ( static_cast< unsigned int >( -1 ) / 2u ) ) - 1 ); // Min lower index
	int const IndexRange::u_max = static_cast< int >( ( static_cast< unsigned int >( -1 ) / 2u ) ); // Max upper index

} // ObjexxFCL
