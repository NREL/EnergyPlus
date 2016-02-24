#ifndef ObjexxFCL_Array6_hh_INCLUDED
#define ObjexxFCL_Array6_hh_INCLUDED

// Array6: Row-Major 6D Array Abstract Base Class
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
#include <ObjexxFCL/Array6.fwd.hh>
#include <ObjexxFCL/Array.hh>
#include <ObjexxFCL/Array6S.hh>
#include <ObjexxFCL/MArray6.hh>

namespace ObjexxFCL {

// Forward
template< typename > class Array6D;
template< typename > class Array6A;

// Array6: Row-Major 6D Array Abstract Base Class
template< typename T >
class Array6 : public Array< T >
{

private: // Types

	typedef  Array< T >  Super;

private: // Friend

	template< typename > friend class Array6;
	template< typename > friend class Array6D;
	template< typename > friend class Array6A;

protected: // Types

	typedef  internal::InitializerSentinel  InitializerSentinel;
	typedef  internal::ProxySentinel  ProxySentinel;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Tail  Tail;
	typedef  typename Super::IR  IR;
	typedef  typename Super::IS  IS;
	typedef  typename Super::DS  DS;

	// STL Style
	typedef  typename Super::value_type  value_type;
	typedef  typename Super::reference  reference;
	typedef  typename Super::const_reference  const_reference;
	typedef  typename Super::pointer  pointer;
	typedef  typename Super::const_pointer  const_pointer;
	typedef  typename Super::iterator  iterator;
	typedef  typename Super::const_iterator  const_iterator;
	typedef  typename Super::reverse_iterator  reverse_iterator;
	typedef  typename Super::const_reverse_iterator  const_reverse_iterator;
	typedef  typename Super::size_type  size_type;
	typedef  typename Super::difference_type  difference_type;

	// C++ Style
	typedef  typename Super::Value  Value;
	typedef  typename Super::Reference  Reference;
	typedef  typename Super::ConstReference  ConstReference;
	typedef  typename Super::Pointer  Pointer;
	typedef  typename Super::ConstPointer  ConstPointer;
	typedef  typename Super::Iterator  Iterator;
	typedef  typename Super::ConstIterator  ConstIterator;
	typedef  typename Super::ReverseIterator  ReverseIterator;
	typedef  typename Super::ConstReverseIterator  ConstReverseIterator;
	typedef  typename Super::Size  Size;
	typedef  typename Super::Difference  Difference;

	using Super::isize;
	using Super::npos;
	using Super::overlap;
	using Super::size;
	using Super::size_of;
	using Super::slice_k;
	using Super::swapB;
	using Super::data_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;

protected: // Creation

	// Default Constructor
	Array6() :
	 z1_( 0u ),
	 z2_( 0u ),
	 z3_( 0u ),
	 z4_( 0u ),
	 z5_( 0u ),
	 z6_( 0u )
	{}

	// Copy Constructor
	Array6( Array6 const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 I6_( a.I6_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ ),
	 z4_( a.z4_ ),
	 z5_( a.z5_ ),
	 z6_( a.z6_ )
	{}

	// Move Constructor
	Array6( Array6 && a ) NOEXCEPT :
	 Super( std::move( a ) ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 I6_( a.I6_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ ),
	 z4_( a.z4_ ),
	 z5_( a.z5_ ),
	 z6_( a.z6_ )
	{
		a.clear_move();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array6( Array6< U > const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 I6_( a.I6_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ ),
	 z4_( a.z4_ ),
	 z5_( a.z5_ ),
	 z6_( a.z6_ )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array6( Array6S< U > const & a ) :
	 Super( a ),
	 I1_( a.u1() ),
	 I2_( a.u2() ),
	 I3_( a.u3() ),
	 I4_( a.u4() ),
	 I5_( a.u5() ),
	 I6_( a.u6() ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// MArray Constructor Template
	template< class A, typename M >
	explicit
	Array6( MArray6< A, M > const & a ) :
	 Super( a ),
	 I1_( a.u1() ),
	 I2_( a.u2() ),
	 I3_( a.u3() ),
	 I4_( a.u4() ),
	 I5_( a.u5() ),
	 I6_( a.u6() ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// IndexRange Constructor
	Array6( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// IndexRange + InitializerSentinel Constructor
	Array6( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, InitializerSentinel const & initialized ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ), initialized ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array6( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, std::initializer_list< U > const l ) :
	 Super( l ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{
		assert( size_of( I1, I2, I3, I4, I5, I6 ) == l.size() );
	}


	// Default Proxy Constructor
	Array6( ProxySentinel const & proxy ) :
	 Super( proxy ),
	 z1_( 0u ),
	 z2_( 0u ),
	 z3_( 0u ),
	 z4_( 0u ),
	 z5_( 0u )
	{}

	// Copy Proxy Constructor
	Array6( Array6 const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 I6_( a.I6_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ ),
	 z4_( a.z4_ ),
	 z5_( a.z5_ ),
	 z6_( a.z6_ )
	{}

	// Slice Proxy Constructor
	Array6( Array6S< T > const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( a.u1() ),
	 I2_( a.u2() ),
	 I3_( a.u3() ),
	 I4_( a.u4() ),
	 I5_( a.u5() ),
	 I6_( a.u6() ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// Base Proxy Constructor
	Array6( Base const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( a.isize() ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( 1 ),
	 I5_( 1 ),
	 I6_( 1 ),
	 z1_( I1_.size() ),
	 z2_( 1u ),
	 z3_( 1u ),
	 z4_( 1u ),
	 z5_( 1u ),
	 z6_( 1u )
	{}

	// Tail Proxy Constructor
	Array6( Tail const & s, ProxySentinel const & proxy ) :
	 Super( s, proxy ),
	 I1_( s.isize() ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( 1 ),
	 I5_( 1 ),
	 I6_( 1 ),
	 z1_( I1_.size() ),
	 z2_( 1u ),
	 z3_( 1u ),
	 z4_( 1u ),
	 z5_( 1u ),
	 z6_( 1u )
	{}

	// Value Proxy Constructor
	Array6( T const & t, ProxySentinel const & proxy ) :
	 Super( t, proxy ),
	 I1_( _ ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( 1 ),
	 I5_( 1 ),
	 I6_( 1 ),
	 z1_( I1_.size() ),
	 z2_( 1u ),
	 z3_( 1u ),
	 z4_( 1u ),
	 z5_( 1u ),
	 z6_( 1u )
	{}

	// Copy + IndexRange Proxy Constructor
	Array6( Array6 const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// Slice + IndexRange Proxy Constructor
	Array6( Array6S< T > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// Base + IndexRange Proxy Constructor
	Array6( Base const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// Tail + IndexRange Proxy Constructor
	Array6( Tail const & s, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, ProxySentinel const & proxy ) :
	 Super( s, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

	// Value + IndexRange Proxy Constructor
	Array6( T const & t, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, ProxySentinel const & proxy ) :
	 Super( t, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() ),
	 z4_( I4_.size() ),
	 z5_( I5_.size() ),
	 z6_( I6_.size() )
	{}

public: // Creation

	// Destructor
	virtual
	~Array6()
	{}

public: // Assignment: Array

	// Copy Assignment
	Array6 &
	operator =( Array6 const & a )
	{
		if ( this != &a ) {
			if ( ( conformable( a ) ) || ( ! dimension_assign( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, a.I6_ ) ) ) {
				Super::operator =( a );
			} else {
				Super::initialize( a );
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator =( Array6< U > const & a )
	{
		if ( ( conformable( a ) ) || ( ! dimension_assign( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, a.I6_ ) ) ) {
			Super::operator =( a );
		} else {
			Super::initialize( a );
		}
		return *this;
	}

	// Slice Assignment
	Array6 &
	operator =( Array6S< T > const & a )
	{
		size_type l( 0u );
		if ( ( conformable( a ) ) || ( ! dimension_assign( a.I1(), a.I2(), a.I3(), a.I4(), a.I5(), a.I6() ) ) ) {
			if ( overlap( a ) ) { // Overlap-safe
				CArrayA< T > c( a.size() );
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
							for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
								for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
									for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
										c[ l ] = a( i1, i2, i3, i4, i5, i6 );
									}
								}
							}
						}
					}
				}
				for ( size_type i = 0; i < c.size(); ++i ) {
					data_[ i ] = c[ i ];
				}
			} else { // Not overlap-safe
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
							for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
								for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
									for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
										data_[ l ] = a( i1, i2, i3, i4, i5, i6 );
									}
								}
							}
						}
					}
				}
			}
		} else {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									new ( data_ + l ) T( a( i1, i2, i3, i4, i5, i6 ) );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator =( Array6S< U > const & a )
	{
		size_type l( 0u );
		if ( ( conformable( a ) ) || ( ! dimension_assign( a.I1(), a.I2(), a.I3(), a.I4(), a.I5(), a.I6() ) ) ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									data_[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		} else {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									new ( data_ + l ) T( a( i1, i2, i3, i4, i5, i6 ) );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	Array6 &
	operator =( MArray6< A, M > const & a )
	{
		size_type l( 0u );
		if ( ( conformable( a ) ) || ( ! dimension_assign( a.I1(), a.I2(), a.I3(), a.I4(), a.I5(), a.I6() ) ) ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									data_[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		} else {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									new ( data_ + l ) T( a( i1, i2, i3, i4, i5, i6 ) );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator =( std::initializer_list< U > const l )
	{
		Super::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator +=( Array6< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator -=( Array6< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator *=( Array6< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator /=( Array6< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator /=( a );
		return *this;
	}

	// += Slice
	Array6 &
	operator +=( Array6S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		if ( overlap( a ) ) { // Overlap-safe
			CArrayA< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									c[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] += c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									data_[ l ] += a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// -= Slice
	Array6 &
	operator -=( Array6S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		if ( overlap( a ) ) { // Overlap-safe
			CArrayA< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									c[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] -= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									data_[ l ] -= a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// *= Slice
	Array6 &
	operator *=( Array6S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		if ( overlap( a ) ) { // Overlap-safe
			CArrayA< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									c[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] *= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									data_[ l ] *= a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// /= Slice
	Array6 &
	operator /=( Array6S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		if ( overlap( a ) ) { // Overlap-safe
			CArrayA< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									assert( a( i1, i2, i3, i4, i5, i6 ) != T( 0 ) );
									c[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] /= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									assert( a( i1, i2, i3, i4, i5, i6 ) != T( 0 ) );
									data_[ l ] /= a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator +=( Array6S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] += a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator -=( Array6S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] -= a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator *=( Array6S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] *= a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	operator /=( Array6S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								assert( a( i1, i2, i3, i4, i5, i6 ) != T( 0 ) );
								data_[ l ] /= a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	Array6 &
	operator +=( MArray6< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] += a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	Array6 &
	operator -=( MArray6< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] -= a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	Array6 &
	operator *=( MArray6< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] *= a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	Array6 &
	operator /=( MArray6< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								assert( a( i1, i2, i3, i4, i5, i6 ) != T( 0 ) );
								data_[ l ] /= a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	and_equals( Array6< U > const & a )
	{
		assert( conformable( a ) );
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	or_equals( Array6< U > const & a )
	{
		assert( conformable( a ) );
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice
	Array6 &
	and_equals( Array6S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		if ( overlap( a ) ) { // Overlap-safe
			CArrayA< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									c[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] && c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									data_[ l ] = data_[ l ] && a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// ||= Slice
	Array6 &
	or_equals( Array6S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		if ( overlap( a ) ) { // Overlap-safe
			CArrayA< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									c[ l ] = a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] || c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
									data_[ l ] = data_[ l ] || a( i1, i2, i3, i4, i5, i6 );
								}
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	and_equals( Array6S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] = data_[ l ] && a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array6 &
	or_equals( Array6S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] = data_[ l ] || a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	Array6 &
	and_equals( MArray6< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] = data_[ l ] && a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	Array6 &
	or_equals( MArray6< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
								data_[ l ] = data_[ l ] || a( i1, i2, i3, i4, i5, i6 );
							}
						}
					}
				}
			}
		}
		return *this;
	}

public: // Assignment: Value

	// = Value
	Array6 &
	operator =( T const & t )
	{
		Super::operator =( t );
		return *this;
	}

	// += Value
	Array6 &
	operator +=( T const & t )
	{
		Super::operator +=( t );
		return *this;
	}

	// -= Value
	Array6 &
	operator -=( T const & t )
	{
		Super::operator -=( t );
		return *this;
	}

	// *= Value
	Array6 &
	operator *=( T const & t )
	{
		Super::operator *=( t );
		return *this;
	}

	// /= Value
	Array6 &
	operator /=( T const & t )
	{
		Super::operator /=( t );
		return *this;
	}

public: // Subscript

	// array( i1, i2, i3, i4, i5, i6 ) const
	T const &
	operator ()( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		assert( contains( i1, i2, i3, i4, i5, i6 ) );
		return sdata_[ ( ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) * z6_ ) + i6 ];
	}

	// array( i1, i2, i3, i4, i5, i6 )
	T &
	operator ()( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 )
	{
		assert( contains( i1, i2, i3, i4, i5, i6 ) );
		return sdata_[ ( ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) * z6_ ) + i6 ];
	}

	// Linear Index
	size_type
	index( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		return ( ( ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) * z6_ ) + i6 ) - shift_;
	}

	// Const Tail Starting at array( i1, i2, i3, i4, i5, i6 )
	Tail const
	a( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		assert( contains( i1, i2, i3, i4, i5, i6 ) );
		size_type const offset( ( ( ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) * z6_ ) + i6 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), ( size_ != npos ? size_ - offset : npos ) );
	}

	// Tail Starting at array( i1, i2, i3, i4, i5, i6 )
	Tail
	a( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 )
	{
		assert( contains( i1, i2, i3, i4, i5, i6 ) );
		size_type const offset( ( ( ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) * z6_ ) + i6 ) - shift_ );
		return Tail( data_ + offset, ( size_ != npos ? size_ - offset : npos ) );
	}

public: // Slice Proxy Generators

	// array( s1, s2, s3, s4, s5, s6 ) const
	Array6S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array6S< T >( data_, -shift_, d1, d2, d3, d4, d5, d6 );
	}

	// array( i1, s2, s3, s4, s5, s6 ) const
	Array5S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array5S< T >( data_, k, d2, d3, d4, d5, d6 );
	}

	// array( s1, i2, s3, s4, s5, s6 ) const
	Array5S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d3, d4, d5, d6 );
	}

	// array( s1, s2, i3, s4, s5, s6 ) const
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d4, d5, d6 );
	}

	// array( s1, s2, s3, i4, s5, s6 ) const
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d3, d5, d6 );
	}

	// array( s1, s2, s3, s4, i5, s6 ) const
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d3, d4, d6 );
	}

	// array( s1, s2, s3, s4, s5, i6 ) const
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d3, d4, d5 );
	}

	// array( i1, i2, s3, s4, s5, s6 ) const
	Array4S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d3, d4, d5, d6 );
	}

	// array( i1, s2, i3, s4, s5, s6 ) const
	Array4S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d4, d5, d6 );
	}

	// array( i1, s2, s3, i4, s5, s6 ) const
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d5, d6 );
	}

	// array( i1, s2, s3, s4, i5, s6 ) const
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d4, d6 );
	}

	// array( i1, s2, s3, s4, s5, i6 ) const
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d4, d5 );
	}

	// array( s1, i2, i3, s4, s5, s6 ) const
	Array4S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d4, d5, d6 );
	}

	// array( s1, i2, s3, i4, s5, s6 ) const
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d5, d6 );
	}

	// array( s1, i2, s3, s4, i5, s6 ) const
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d4, d6 );
	}

	// array( s1, i2, s3, s4, s5, i6 ) const
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d4, d5 );
	}

	// array( s1, s2, i3, i4, s5, s6 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d5, d6 );
	}

	// array( s1, s2, i3, s4, i5, s6 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d4, d6 );
	}

	// array( s1, s2, i3, s4, s5, i6 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d4, d5 );
	}

	// array( s1, s2, s3, i4, i5, s6 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d6 );
	}

	// array( s1, s2, s3, i4, s5, i6 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d5 );
	}

	// array( s1, s2, s3, s4, i5, i6 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d4 );
	}

	// array( i1, i2, i3, s4, s5, s6 ) const
	Array3S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d4, d5, d6 );
	}

	// array( i1, i2, s3, i4, s5, s6 ) const
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d5, d6 );
	}

	// array( i1, i2, s3, s4, i5, s6 ) const
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d4, d6 );
	}

	// array( i1, i2, s3, s4, s5, i6 ) const
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d4, d5 );
	}

	// array( i1, s2, i3, i4, s5, s6 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d5, d6 );
	}

	// array( i1, s2, i3, s4, i5, s6 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d4, d6 );
	}

	// array( i1, s2, i3, s4, s5, i6 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d4, d5 );
	}

	// array( i1, s2, s3, i4, i5, s6 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d6 );
	}

	// array( i1, s2, s3, i4, s5, i6 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d5 );
	}

	// array( i1, s2, s3, s4, i5, i6 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d4 );
	}

	// array( s1, i2, i3, i4, s5, s6 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d5, d6 );
	}

	// array( s1, i2, i3, s4, i5, s6 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d4, d6 );
	}

	// array( s1, i2, i3, s4, s5, i6 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d4, d5 );
	}

	// array( s1, i2, s3, i4, i5, s6 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d6 );
	}

	// array( s1, i2, s3, i4, s5, i6 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d5 );
	}

	// array( s1, i2, s3, s4, i5, i6 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d4 );
	}

	// array( s1, s2, i3, i4, i5, s6 ) const
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d6 );
	}

	// array( s1, s2, i3, i4, s5, i6 ) const
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d5 );
	}

	// array( s1, s2, i3, s4, i5, i6 ) const
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d4 );
	}

	// array( s1, s2, s3, i4, i5, i6 ) const
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d3 );
	}

	// array( s1, s2, i3, i4, i5, i6 ) const
	Array2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, s3, i4, i5, i6 ) const
	Array2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d3 );
	}

	// array( s1, i2, i3, s4, i5, i6 ) const
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d4 );
	}

	// array( s1, i2, i3, i4, s5, i6 ) const
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d5 );
	}

	// array( s1, i2, i3, i4, i5, s6 ) const
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d6 );
	}

	// array( i1, s2, s3, i4, i5, i6 ) const
	Array2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d3 );
	}

	// array( i1, s2, i3, s4, i5, i6 ) const
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d4 );
	}

	// array( i1, s2, i3, i4, s5, i6 ) const
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d5 );
	}

	// array( i1, s2, i3, i4, i5, s6 ) const
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d6 );
	}

	// array( i1, i2, s3, s4, i5, i6 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d4 );
	}

	// array( i1, i2, s3, i4, s5, i6 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d5 );
	}

	// array( i1, i2, s3, i4, i5, s6 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d6 );
	}

	// array( i1, i2, i3, s4, s5, i6 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d4, d5 );
	}

	// array( i1, i2, i3, s4, i5, s6 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d4, d6 );
	}

	// array( i1, i2, i3, i4, s5, s6 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, IS const & s5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d5, d6 );
	}

	// array( s1, i2, i3, i4, i5, i6 ) const
	Array1S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3, i4, i5, i6 ) const
	Array1S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3, i4, i5, i6 ) const
	Array1S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d3 );
	}

	// array( i1, i2, i3, s4, i5, i6 ) const
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, int const i5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d4 );
	}

	// array( i1, i2, i3, i4, s5, i6 ) const
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, IS const & s5, int const i6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d5 );
	}

	// array( i1, i2, i3, i4, i5, s6 ) const
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, int const i5, IS const & s6 ) const
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d6 );
	}

	// array( s1, s2, s3, s4, s5, s6 )
	Array6S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 )
	{
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array6S< T >( data_, -shift_, d1, d2, d3, d4, d5, d6 );
	}

	// array( i1, s2, s3, s4, s5, s6 )
	Array5S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array5S< T >( data_, k, d2, d3, d4, d5, d6 );
	}

	// array( s1, i2, s3, s4, s5, s6 )
	Array5S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d3, d4, d5, d6 );
	}

	// array( s1, s2, i3, s4, s5, s6 )
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d4, d5, d6 );
	}

	// array( s1, s2, s3, i4, s5, s6 )
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d3, d5, d6 );
	}

	// array( s1, s2, s3, s4, i5, s6 )
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d3, d4, d6 );
	}

	// array( s1, s2, s3, s4, s5, i6 )
	Array5S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array5S< T >( data_, k, d1, d2, d3, d4, d5 );
	}

	// array( i1, i2, s3, s4, s5, s6 )
	Array4S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d3, d4, d5, d6 );
	}

	// array( i1, s2, i3, s4, s5, s6 )
	Array4S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d4, d5, d6 );
	}

	// array( i1, s2, s3, i4, s5, s6 )
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d5, d6 );
	}

	// array( i1, s2, s3, s4, i5, s6 )
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d4, d6 );
	}

	// array( i1, s2, s3, s4, s5, i6 )
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d4, d5 );
	}

	// array( s1, i2, i3, s4, s5, s6 )
	Array4S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d4, d5, d6 );
	}

	// array( s1, i2, s3, i4, s5, s6 )
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d5, d6 );
	}

	// array( s1, i2, s3, s4, i5, s6 )
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d4, d6 );
	}

	// array( s1, i2, s3, s4, s5, i6 )
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d4, d5 );
	}

	// array( s1, s2, i3, i4, s5, s6 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d5, d6 );
	}

	// array( s1, s2, i3, s4, i5, s6 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d4, d6 );
	}

	// array( s1, s2, i3, s4, s5, i6 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d4, d5 );
	}

	// array( s1, s2, s3, i4, i5, s6 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d6 );
	}

	// array( s1, s2, s3, i4, s5, i6 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d5 );
	}

	// array( s1, s2, s3, s4, i5, i6 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d4 );
	}

	// array( i1, i2, i3, s4, s5, s6 )
	Array3S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d4, d5, d6 );
	}

	// array( i1, i2, s3, i4, s5, s6 )
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d5, d6 );
	}

	// array( i1, i2, s3, s4, i5, s6 )
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d4, d6 );
	}

	// array( i1, i2, s3, s4, s5, i6 )
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d4, d5 );
	}

	// array( i1, s2, i3, i4, s5, s6 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d5, d6 );
	}

	// array( i1, s2, i3, s4, i5, s6 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d4, d6 );
	}

	// array( i1, s2, i3, s4, s5, i6 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d4, d5 );
	}

	// array( i1, s2, s3, i4, i5, s6 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d6 );
	}

	// array( i1, s2, s3, i4, s5, i6 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d5 );
	}

	// array( i1, s2, s3, s4, i5, i6 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d4 );
	}

	// array( s1, i2, i3, i4, s5, s6 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d5, d6 );
	}

	// array( s1, i2, i3, s4, i5, s6 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d4, d6 );
	}

	// array( s1, i2, i3, s4, s5, i6 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d4, d5 );
	}

	// array( s1, i2, s3, i4, i5, s6 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d6 );
	}

	// array( s1, i2, s3, i4, s5, i6 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d5 );
	}

	// array( s1, i2, s3, s4, i5, i6 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d4 );
	}

	// array( s1, s2, i3, i4, i5, s6 )
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d6 );
	}

	// array( s1, s2, i3, i4, s5, i6 )
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d5 );
	}

	// array( s1, s2, i3, s4, i5, i6 )
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d4 );
	}

	// array( s1, s2, s3, i4, i5, i6 )
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d3 );
	}

	// array( s1, s2, i3, i4, i5, i6 )
	Array2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, s3, i4, i5, i6 )
	Array2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d3 );
	}

	// array( s1, i2, i3, s4, i5, i6 )
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d4 );
	}

	// array( s1, i2, i3, i4, s5, i6 )
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d5 );
	}

	// array( s1, i2, i3, i4, i5, s6 )
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d6 );
	}

	// array( i1, s2, s3, i4, i5, i6 )
	Array2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d3 );
	}

	// array( i1, s2, i3, s4, i5, i6 )
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d4 );
	}

	// array( i1, s2, i3, i4, s5, i6 )
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d5 );
	}

	// array( i1, s2, i3, i4, i5, s6 )
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d6 );
	}

	// array( i1, i2, s3, s4, i5, i6 )
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d4 );
	}

	// array( i1, i2, s3, i4, s5, i6 )
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d5 );
	}

	// array( i1, i2, s3, i4, i5, s6 )
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d6 );
	}

	// array( i1, i2, i3, s4, s5, i6 )
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d4, d5 );
	}

	// array( i1, i2, i3, s4, i5, s6 )
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d4, d6 );
	}

	// array( i1, i2, i3, i4, s5, s6 )
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, IS const & s5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d5, d6 );
	}

	// array( s1, i2, i3, i4, i5, i6 )
	Array1S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3, i4, i5, i6 )
	Array1S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3, i4, i5, i6 )
	Array1S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d3 );
	}

	// array( i1, i2, i3, s4, i5, i6 )
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, int const i5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		k += slice_k( I5_, i5, z );
		DS const d4( I4_, s4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d4 );
	}

	// array( i1, i2, i3, i4, s5, i6 )
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, IS const & s5, int const i6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		k += slice_k( I6_, i6 );
		DS const d5( I5_, s5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d5 );
	}

	// array( i1, i2, i3, i4, i5, s6 )
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, int const i5, IS const & s6 )
	{
		int k( -shift_ );
		size_type z( z6_ );
		DS const d6( I6_, s6 );
		k += slice_k( I5_, i5, z );
		k += slice_k( I4_, i4, z *= z5_ );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d6 );
	}

public: // Predicate

	// Contains Indexed Element?
	bool
	contains( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		return ( I1_.contains( i1 ) && I2_.contains( i2 ) && I3_.contains( i3 ) && I4_.contains( i4 ) && I5_.contains( i5 ) && I6_.contains( i6 ) );
	}

	// Conformable?
	template< typename U >
	bool
	conformable( Array6< U > const & a ) const
	{
		return ( ( z1_ == a.z1_ ) && ( z2_ == a.z2_ ) && ( z3_ == a.z3_ ) && ( z4_ == a.z4_ ) && ( z5_ == a.z5_ ) && ( z6_ == a.z6_ ) );
	}

	// Conformable?
	template< typename U >
	bool
	conformable( Array6S< U > const & a ) const
	{
		return ( ( z1_ == a.size1() ) && ( z2_ == a.size2() ) && ( z3_ == a.size3() ) && ( z4_ == a.size4() ) && ( z5_ == a.size5() ) && ( z6_ == a.size6() ) );
	}

	// Conformable?
	template< class A, typename M >
	bool
	conformable( MArray6< A, M > const & a ) const
	{
		return ( ( z1_ == a.size1() ) && ( z2_ == a.size2() ) && ( z3_ == a.size3() ) && ( z4_ == a.size4() ) && ( z5_ == a.size5() ) && ( z6_ == a.size6() ) );
	}

	// Equal Dimensions?
	template< typename U >
	bool
	equal_dimensions( Array6< U > const & a ) const
	{
		return ( ( I1_ == a.I1_ ) && ( I2_ == a.I2_ ) && ( I3_ == a.I3_ ) && ( I4_ == a.I4_ ) && ( I5_ == a.I5_ ) && ( I6_ == a.I6_ ) );
	}

	// Equal Dimensions?
	template< typename U >
	bool
	equal_dimensions( Array6S< U > const & a ) const
	{
		return ( ( l1() == 1 ) && ( u1() == a.u1() ) && ( l2() == 1 ) && ( u2() == a.u2() ) && ( l3() == 1 ) && ( u3() == a.u3() ) && ( l4() == 1 ) && ( u4() == a.u4() ) && ( l5() == 1 ) && ( u5() == a.u5() ) && ( l6() == 1 ) && ( u6() == a.u6() ) );
	}

	// Equal Dimensions?
	template< class A, typename M >
	bool
	equal_dimensions( MArray6< A, M > const & a ) const
	{
		return ( ( l1() == 1 ) && ( u1() == a.u1() ) && ( l2() == 1 ) && ( u2() == a.u2() ) && ( l3() == 1 ) && ( u3() == a.u3() ) && ( l4() == 1 ) && ( u4() == a.u4() ) && ( l5() == 1 ) && ( u5() == a.u5() ) && ( l6() == 1 ) && ( u6() == a.u6() ) );
	}

public: // Inspector

	// Rank
	int
	rank() const
	{
		return 6;
	}

	// IndexRange of a Dimension
	IR const &
	I( int const d ) const
	{
		switch ( d ) {
		case 1:
			return I1_;
		case 2:
			return I2_;
		case 3:
			return I3_;
		case 4:
			return I4_;
		case 5:
			return I5_;
		case 6:
			return I6_;
		default:
			assert( false );
			return I1_;
		}
	}

	// Lower Index of a Dimension
	int
	l( int const d ) const
	{
		switch ( d ) {
		case 1:
			return l1();
		case 2:
			return l2();
		case 3:
			return l3();
		case 4:
			return l4();
		case 5:
			return l5();
		case 6:
			return l6();
		default:
			assert( false );
			return l1();
		}
	}

	// Upper Index of a Dimension
	int
	u( int const d ) const
	{
		switch ( d ) {
		case 1:
			return u1();
		case 2:
			return u2();
		case 3:
			return u3();
		case 4:
			return u4();
		case 5:
			return u5();
		case 6:
			return u6();
		default:
			assert( false );
			return u1();
		}
	}

	// Size of a Dimension
	size_type
	size( int const d ) const
	{
		switch ( d ) {
		case 1:
			return z1_;
		case 2:
			return z2_;
		case 3:
			return z3_;
		case 4:
			return z4_;
		case 5:
			return z5_;
		case 6:
			return z6_;
		default:
			assert( false );
			return z1_;
		}
	}

	// Size of a Dimension
	int
	isize( int const d ) const
	{
		switch ( d ) {
		case 1:
			return isize1();
		case 2:
			return isize2();
		case 3:
			return isize3();
		case 4:
			return isize4();
		case 5:
			return isize5();
		case 6:
			return isize6();
		default:
			assert( false );
			return isize1();
		}
	}

	// IndexRange of Dimension 1
	IR const &
	I1() const
	{
		return I1_;
	}

	// Lower Index of Dimension 1
	int
	l1() const
	{
		return I1_.l();
	}

	// Upper Index of Dimension 1
	int
	u1() const
	{
		return I1_.u();
	}

	// Size of Dimension 1
	size_type
	size1() const
	{
		return z1_;
	}

	// Size of Dimension 1
	int
	isize1() const
	{
		return static_cast< int >( z1_ );
	}

	// IndexRange of Dimension 2
	IR const &
	I2() const
	{
		return I2_;
	}

	// Lower Index of Dimension 2
	int
	l2() const
	{
		return I2_.l();
	}

	// Upper Index of Dimension 2
	int
	u2() const
	{
		return I2_.u();
	}

	// Size of Dimension 2
	size_type
	size2() const
	{
		return z2_;
	}

	// Size of Dimension 2
	int
	isize2() const
	{
		return static_cast< int >( z2_ );
	}

	// IndexRange of Dimension 3
	IR const &
	I3() const
	{
		return I3_;
	}

	// Lower Index of Dimension 3
	int
	l3() const
	{
		return I3_.l();
	}

	// Upper Index of Dimension 3
	int
	u3() const
	{
		return I3_.u();
	}

	// Size of Dimension 3
	size_type
	size3() const
	{
		return z3_;
	}

	// Size of Dimension 3
	int
	isize3() const
	{
		return static_cast< int >( z3_ );
	}

	// IndexRange of Dimension 4
	IR const &
	I4() const
	{
		return I4_;
	}

	// Lower Index of Dimension 4
	int
	l4() const
	{
		return I4_.l();
	}

	// Upper Index of Dimension 4
	int
	u4() const
	{
		return I4_.u();
	}

	// Size of Dimension 4
	size_type
	size4() const
	{
		return z4_;
	}

	// Size of Dimension 4
	int
	isize4() const
	{
		return static_cast< int >( z4_ );
	}

	// IndexRange of Dimension 5
	IR const &
	I5() const
	{
		return I5_;
	}

	// Lower Index of Dimension 5
	int
	l5() const
	{
		return I5_.l();
	}

	// Upper Index of Dimension 5
	int
	u5() const
	{
		return I5_.u();
	}

	// Size of Dimension 5
	size_type
	size5() const
	{
		return z5_;
	}

	// Size of Dimension 5
	int
	isize5() const
	{
		return static_cast< int >( z5_ );
	}

	// IndexRange of Dimension 6
	IR const &
	I6() const
	{
		return I6_;
	}

	// Lower Index of Dimension 6
	int
	l6() const
	{
		return I6_.l();
	}

	// Upper Index of Dimension 6
	int
	u6() const
	{
		return I6_.u();
	}

	// Size of Dimension 6
	size_type
	size6() const
	{
		return z6_;
	}

	// Size of Dimension 6
	int
	isize6() const
	{
		return static_cast< int >( z6_ );
	}

public: // Modifier

	// Clear
	Array6 &
	clear()
	{
		Super::clear();
		I1_.clear();
		I2_.clear();
		I3_.clear();
		I4_.clear();
		I5_.clear();
		I6_.clear();
		z1_ = z2_ = z3_ = z4_ = z5_ = z6_ = 0u;
		return *this;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	MArray6< Array6 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray6< Array6 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	MArray6< Array6, M >
	ma( M ClassT::* pmem )
	{
		return MArray6< Array6, M >( *this, pmem );
	}

public: // Comparison: Predicate

	// Array6 == Array6
	friend
	bool
	eq( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 != Array6
	friend
	bool
	ne( Array6 const & a, Array6 const & b )
	{
		return ! eq( a, b );
	}

	// Array6 < Array6
	friend
	bool
	lt( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 <= Array6
	friend
	bool
	le( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 > Array6
	friend
	bool
	gt( Array6 const & a, Array6 const & b )
	{
		return lt( b, a );
	}

	// Array6 >= Array6
	friend
	bool
	ge( Array6 const & a, Array6 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any

	// Array6 == Array6
	friend
	bool
	any_eq( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 != Array6
	friend
	bool
	any_ne( Array6 const & a, Array6 const & b )
	{
		return ! eq( a, b );
	}

	// Array6 < Array6
	friend
	bool
	any_lt( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 <= Array6
	friend
	bool
	any_le( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 > Array6
	friend
	bool
	any_gt( Array6 const & a, Array6 const & b )
	{
		return any_lt( b, a );
	}

	// Array6 >= Array6
	friend
	bool
	any_ge( Array6 const & a, Array6 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All

	// Array6 == Array6
	friend
	bool
	all_eq( Array6 const & a, Array6 const & b )
	{
		return eq( a, b );
	}

	// Array6 != Array6
	friend
	bool
	all_ne( Array6 const & a, Array6 const & b )
	{
		return ! any_eq( a, b );
	}

	// Array6 < Array6
	friend
	bool
	all_lt( Array6 const & a, Array6 const & b )
	{
		return lt( a, b );
	}

	// Array6 <= Array6
	friend
	bool
	all_le( Array6 const & a, Array6 const & b )
	{
		return le( a, b );
	}

	// Array6 > Array6
	friend
	bool
	all_gt( Array6 const & a, Array6 const & b )
	{
		return gt( a, b );
	}

	// Array6 >= Array6
	friend
	bool
	all_ge( Array6 const & a, Array6 const & b )
	{
		return ge( a, b );
	}

public: // Comparison: Count

	// Array6 == Array6
	friend
	bool
	count_eq( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 != Array6
	friend
	bool
	count_ne( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ne( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 < Array6
	friend
	bool
	count_lt( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 <= Array6
	friend
	bool
	count_le( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 > Array6
	friend
	bool
	count_gt( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_gt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array6 >= Array6
	friend
	bool
	count_ge( Array6 const & a, Array6 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ge( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

public: // Comparison: Predicate: Slice

	// Array6 == Array6S
	friend
	bool
	eq( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] == b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 != Array6S
	friend
	bool
	ne( Array6 const & a, Array6S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Array6 < Array6S
	friend
	bool
	lt( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] < b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 <= Array6S
	friend
	bool
	le( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] <= b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 > Array6S
	friend
	bool
	gt( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] > b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 >= Array6S
	friend
	bool
	ge( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] >= b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6S == Array6
	friend
	bool
	eq( Array6S< T > const & a, Array6 const & b )
	{
		return eq( b, a );
	}

	// Array6S != Array6
	friend
	bool
	ne( Array6S< T > const & a, Array6 const & b )
	{
		return ne( b, a );
	}

	// Array6S < Array6
	friend
	bool
	lt( Array6S< T > const & a, Array6 const & b )
	{
		return gt( b, a );
	}

	// Array6S <= Array6
	friend
	bool
	le( Array6S< T > const & a, Array6 const & b )
	{
		return ge( b, a );
	}

	// Array6S > Array6
	friend
	bool
	gt( Array6S< T > const & a, Array6 const & b )
	{
		return lt( b, a );
	}

	// Array6S >= Array6
	friend
	bool
	ge( Array6S< T > const & a, Array6 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: Slice

	// Any Array6 == Array6S
	friend
	bool
	any_eq( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] == b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 != Array6S
	friend
	bool
	any_ne( Array6 const & a, Array6S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Any Array6 < Array6S
	friend
	bool
	any_lt( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] < b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 <= Array6S
	friend
	bool
	any_le( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] <= b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 > Array6S
	friend
	bool
	any_gt( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] > b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 >= Array6S
	friend
	bool
	any_ge( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] >= b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6S == Array6
	friend
	bool
	any_eq( Array6S< T > const & a, Array6 const & b )
	{
		return any_eq( b, a );
	}

	// Any Array6S != Array6
	friend
	bool
	any_ne( Array6S< T > const & a, Array6 const & b )
	{
		return any_ne( b, a );
	}

	// Any Array6S < Array6
	friend
	bool
	any_lt( Array6S< T > const & a, Array6 const & b )
	{
		return any_gt( b, a );
	}

	// Any Array6S <= Array6
	friend
	bool
	any_le( Array6S< T > const & a, Array6 const & b )
	{
		return any_ge( b, a );
	}

	// Any Array6S > Array6
	friend
	bool
	any_gt( Array6S< T > const & a, Array6 const & b )
	{
		return any_lt( b, a );
	}

	// Any Array6S >= Array6
	friend
	bool
	any_ge( Array6S< T > const & a, Array6 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: Slice

	// All Array6 == Array6S
	friend
	bool
	all_eq( Array6 const & a, Array6S< T > const & b )
	{
		return eq( a, b );
	}

	// All Array6 != Array6S
	friend
	bool
	all_ne( Array6 const & a, Array6S< T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All Array6 < Array6S
	friend
	bool
	all_lt( Array6 const & a, Array6S< T > const & b )
	{
		return lt( a, b );
	}

	// All Array6 <= Array6S
	friend
	bool
	all_le( Array6 const & a, Array6S< T > const & b )
	{
		return le( a, b );
	}

	// All Array6 > Array6S
	friend
	bool
	all_gt( Array6 const & a, Array6S< T > const & b )
	{
		return gt( a, b );
	}

	// All Array6 >= Array6S
	friend
	bool
	all_ge( Array6 const & a, Array6S< T > const & b )
	{
		return ge( a, b );
	}

	// All Array6S == Array6
	friend
	bool
	all_eq( Array6S< T > const & a, Array6 const & b )
	{
		return all_eq( b, a );
	}

	// All Array6S != Array6
	friend
	bool
	all_ne( Array6S< T > const & a, Array6 const & b )
	{
		return all_ne( b, a );
	}

	// All Array6S < Array6
	friend
	bool
	all_lt( Array6S< T > const & a, Array6 const & b )
	{
		return all_gt( b, a );
	}

	// All Array6S <= Array6
	friend
	bool
	all_le( Array6S< T > const & a, Array6 const & b )
	{
		return all_ge( b, a );
	}

	// All Array6S > Array6
	friend
	bool
	all_gt( Array6S< T > const & a, Array6 const & b )
	{
		return all_lt( b, a );
	}

	// All Array6S >= Array6
	friend
	bool
	all_ge( Array6S< T > const & a, Array6 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: Slice

	// Count Array6 == Array6S
	friend
	size_type
	count_eq( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] == b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 != Array6S
	friend
	size_type
	count_ne( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] != b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 < Array6S
	friend
	size_type
	count_lt( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] < b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 <= Array6S
	friend
	size_type
	count_le( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] <= b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 > Array6S
	friend
	size_type
	count_gt( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] > b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 >= Array6S
	friend
	size_type
	count_ge( Array6 const & a, Array6S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] >= b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6S == Array6
	friend
	size_type
	count_eq( Array6S< T > const & a, Array6 const & b )
	{
		return count_eq( b, a );
	}

	// Count Array6S != Array6
	friend
	size_type
	count_ne( Array6S< T > const & a, Array6 const & b )
	{
		return count_ne( b, a );
	}

	// Count Array6S < Array6
	friend
	size_type
	count_lt( Array6S< T > const & a, Array6 const & b )
	{
		return count_gt( b, a );
	}

	// Count Array6S <= Array6
	friend
	size_type
	count_le( Array6S< T > const & a, Array6 const & b )
	{
		return count_ge( b, a );
	}

	// Count Array6S > Array6
	friend
	size_type
	count_gt( Array6S< T > const & a, Array6 const & b )
	{
		return count_lt( b, a );
	}

	// Count Array6S >= Array6
	friend
	size_type
	count_ge( Array6S< T > const & a, Array6 const & b )
	{
		return count_le( b, a );
	}

public: // Comparison: Predicate: MArray

	// Array6 == MArray6
	template< class A >
	friend
	bool
	eq( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] == b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 != MArray6
	template< class A >
	friend
	bool
	ne( Array6 const & a, MArray6< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Array6 < MArray6
	template< class A >
	friend
	bool
	lt( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] < b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 <= MArray6
	template< class A >
	friend
	bool
	le( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] <= b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 > MArray6
	template< class A >
	friend
	bool
	gt( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] > b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// Array6 >= MArray6
	template< class A >
	friend
	bool
	ge( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( ! ( a[ l ] >= b( i1, i2, i3, i4, i5, i6 ) ) ) return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

	// MArray6 == Array6
	template< class A >
	friend
	bool
	eq( MArray6< A, T > const & a, Array6 const & b )
	{
		return eq( b, a );
	}

	// MArray6 != Array6
	template< class A >
	friend
	bool
	ne( MArray6< A, T > const & a, Array6 const & b )
	{
		return ne( b, a );
	}

	// MArray6 < Array6
	template< class A >
	friend
	bool
	lt( MArray6< A, T > const & a, Array6 const & b )
	{
		return gt( b, a );
	}

	// MArray6 <= Array6
	template< class A >
	friend
	bool
	le( MArray6< A, T > const & a, Array6 const & b )
	{
		return ge( b, a );
	}

	// MArray6 > Array6
	template< class A >
	friend
	bool
	gt( MArray6< A, T > const & a, Array6 const & b )
	{
		return lt( b, a );
	}

	// MArray6 >= Array6
	template< class A >
	friend
	bool
	ge( MArray6< A, T > const & a, Array6 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any Array6 == MArray6
	template< class A >
	friend
	bool
	any_eq( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] == b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 != MArray6
	template< class A >
	friend
	bool
	any_ne( Array6 const & a, MArray6< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any Array6 < MArray6
	template< class A >
	friend
	bool
	any_lt( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] < b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 <= MArray6
	template< class A >
	friend
	bool
	any_le( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] <= b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 > MArray6
	template< class A >
	friend
	bool
	any_gt( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] > b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any Array6 >= MArray6
	template< class A >
	friend
	bool
	any_ge( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] >= b( i1, i2, i3, i4, i5, i6 ) ) return true;
							}
						}
					}
				}
			}
		}
		return false;
	}

	// Any MArray6 == Array6
	template< class A >
	friend
	bool
	any_eq( MArray6< A, T > const & a, Array6 const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray6 != Array6
	template< class A >
	friend
	bool
	any_ne( MArray6< A, T > const & a, Array6 const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray6 < Array6
	template< class A >
	friend
	bool
	any_lt( MArray6< A, T > const & a, Array6 const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray6 <= Array6
	template< class A >
	friend
	bool
	any_le( MArray6< A, T > const & a, Array6 const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray6 > Array6
	template< class A >
	friend
	bool
	any_gt( MArray6< A, T > const & a, Array6 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray6 >= Array6
	template< class A >
	friend
	bool
	any_ge( MArray6< A, T > const & a, Array6 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All Array6 == MArray6
	template< class A >
	friend
	bool
	all_eq( Array6 const & a, MArray6< A, T > const & b )
	{
		return eq( a, b );
	}

	// All Array6 != MArray6
	template< class A >
	friend
	bool
	all_ne( Array6 const & a, MArray6< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All Array6 < MArray6
	template< class A >
	friend
	bool
	all_lt( Array6 const & a, MArray6< A, T > const & b )
	{
		return lt( a, b );
	}

	// All Array6 <= MArray6
	template< class A >
	friend
	bool
	all_le( Array6 const & a, MArray6< A, T > const & b )
	{
		return le( a, b );
	}

	// All Array6 > MArray6
	template< class A >
	friend
	bool
	all_gt( Array6 const & a, MArray6< A, T > const & b )
	{
		return gt( a, b );
	}

	// All Array6 >= MArray6
	template< class A >
	friend
	bool
	all_ge( Array6 const & a, MArray6< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray6 == Array6
	template< class A >
	friend
	bool
	all_eq( MArray6< A, T > const & a, Array6 const & b )
	{
		return all_eq( b, a );
	}

	// All MArray6 != Array6
	template< class A >
	friend
	bool
	all_ne( MArray6< A, T > const & a, Array6 const & b )
	{
		return all_ne( b, a );
	}

	// All MArray6 < Array6
	template< class A >
	friend
	bool
	all_lt( MArray6< A, T > const & a, Array6 const & b )
	{
		return all_gt( b, a );
	}

	// All MArray6 <= Array6
	template< class A >
	friend
	bool
	all_le( MArray6< A, T > const & a, Array6 const & b )
	{
		return all_ge( b, a );
	}

	// All MArray6 > Array6
	template< class A >
	friend
	bool
	all_gt( MArray6< A, T > const & a, Array6 const & b )
	{
		return all_lt( b, a );
	}

	// All MArray6 >= Array6
	template< class A >
	friend
	bool
	all_ge( MArray6< A, T > const & a, Array6 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count Array6 == MArray6
	template< class A >
	friend
	size_type
	count_eq( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] == b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 != MArray6
	template< class A >
	friend
	size_type
	count_ne( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] != b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 < MArray6
	template< class A >
	friend
	size_type
	count_lt( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] < b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 <= MArray6
	template< class A >
	friend
	size_type
	count_le( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] <= b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 > MArray6
	template< class A >
	friend
	size_type
	count_gt( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] > b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count Array6 >= MArray6
	template< class A >
	friend
	size_type
	count_ge( Array6 const & a, MArray6< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0u ), n( 0u );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = b.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = b.u6(); i6 <= e6; ++i6, ++l ) {
								if ( a[ l ] >= b( i1, i2, i3, i4, i5, i6 ) ) ++n;
							}
						}
					}
				}
			}
		}
		return n;
	}

	// Count MArray6 == Array6
	template< class A >
	friend
	size_type
	count_eq( MArray6< A, T > const & a, Array6 const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray6 != Array6
	template< class A >
	friend
	size_type
	count_ne( MArray6< A, T > const & a, Array6 const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray6 < Array6
	template< class A >
	friend
	size_type
	count_lt( MArray6< A, T > const & a, Array6 const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray6 <= Array6
	template< class A >
	friend
	size_type
	count_le( MArray6< A, T > const & a, Array6 const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray6 > Array6
	template< class A >
	friend
	size_type
	count_gt( MArray6< A, T > const & a, Array6 const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray6 >= Array6
	template< class A >
	friend
	size_type
	count_ge( MArray6< A, T > const & a, Array6 const & b )
	{
		return count_le( b, a );
	}

protected: // Functions

	// Dimension by IndexRange
	virtual
	bool
	dimension_assign( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 ) = 0;

	// Clear on Move
	void
	clear_move()
	{
		I1_.clear();
		I2_.clear();
		I3_.clear();
		I4_.clear();
		I5_.clear();
		I6_.clear();
		z1_ = z2_ = z3_ = z4_ = z5_ = z6_ = 0u;
	}

	// Swap
	void
	swap6( Array6 & v )
	{
		swapB( v );
		I1_.swap( v.I1_ );
		I2_.swap( v.I2_ );
		I3_.swap( v.I3_ );
		I4_.swap( v.I4_ );
		I5_.swap( v.I5_ );
		I6_.swap( v.I6_ );
		std::swap( z1_, v.z1_ );
		std::swap( z2_, v.z2_ );
		std::swap( z3_, v.z3_ );
		std::swap( z4_, v.z4_ );
		std::swap( z5_, v.z5_ );
		std::swap( z6_, v.z6_ );
	}

protected: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2
	IR I3_; // Index range of dim 3
	IR I4_; // Index range of dim 4
	IR I5_; // Index range of dim 5
	IR I6_; // Index range of dim 6

	size_type z1_; // Size of dim 1
	size_type z2_; // Size of dim 2
	size_type z3_; // Size of dim 3
	size_type z4_; // Size of dim 4
	size_type z5_; // Size of dim 5
	size_type z6_; // Size of dim 6

}; // Array6

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array6< U > const & a, Array6< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array6< U > const & a, Array6S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array6S< U > const & a, Array6< V > const & b )
{
	return b.conformable( a );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( Array6< U > const & a, MArray6< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray6< A, M > const & a, Array6< V > const & b )
{
	return b.conformable( a );
}

// Equal Dimensions?
template< typename U, typename V >
inline
bool
equal_dimensions( Array6< U > const & a, Array6< V > const & b )
{
	return a.equal_dimensions( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_Array6_hh_INCLUDED
