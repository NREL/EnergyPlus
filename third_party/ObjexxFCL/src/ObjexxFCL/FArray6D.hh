#ifndef ObjexxFCL_FArray6D_hh_INCLUDED
#define ObjexxFCL_FArray6D_hh_INCLUDED

// FArray6D: Fortran-Compatible 6D Array
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
#include <ObjexxFCL/FArray6D.fwd.hh>
#include <ObjexxFCL/FArray6.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DynamicIndexRange.hh>
#include <ObjexxFCL/FArrayInitializer.hh>

namespace ObjexxFCL {

// FArray6D: Fortran-Compatible 6D Array
template< typename T >
class FArray6D : public FArray6< T >, public ObserverMulti
{

private: // Types

	typedef  FArray6< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  typename Super::arg_FArray  arg_FArray;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class FArray6D;
	friend class FArray6P< T >;
	friend class FArray6A< T >;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Tail  Tail;
	typedef  typename Super::IR  SIR;
	typedef  DynamicIndexRange  IR;

	// STL Style
	typedef  typename Base::value_type  value_type;
	typedef  typename Base::reference  reference;
	typedef  typename Base::const_reference  const_reference;
	typedef  typename Base::pointer  pointer;
	typedef  typename Base::const_pointer  const_pointer;
	typedef  typename Base::size_type  size_type;
	typedef  typename Base::difference_type  difference_type;

	// C++ Style
	typedef  typename Base::Value  Value;
	typedef  typename Base::Reference  Reference;
	typedef  typename Base::ConstReference  ConstReference;
	typedef  typename Base::Pointer  Pointer;
	typedef  typename Base::ConstPointer  ConstPointer;
	typedef  typename Base::Size  Size;
	typedef  typename Base::Difference  Difference;

	typedef  FArrayInitializer< T, ObjexxFCL::FArray6D >  Initializer;
	typedef  typename Initializer::Function  InitializerFunction;

	using Super::conformable;
	using Super::isize1;
	using Super::isize2;
	using Super::isize3;
	using Super::isize4;
	using Super::isize5;
	using Super::isize6;
	using Super::l;
	using Super::operator ();
	using Super::reassign;
	using Super::resize;
	using Super::shift_set;
	using Super::size1;
	using Super::size2;
	using Super::size3;
	using Super::size4;
	using Super::size5;
	using Super::size6;
	using Super::size_of;
	using Super::swap6DB;
	using Super::u;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;
	using Super::z1_;
	using Super::z2_;
	using Super::z3_;
	using Super::z4_;
	using Super::z5_;

public: // Creation

	// Default Constructor
	inline
	FArray6D()
	{
		insert_as_observer();
	}

	// Copy Constructor
	inline
	FArray6D( FArray6D const & a ) :
	 Super( a ),
	 ObserverMulti(),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 I6_( a.I6_ )
	{
		insert_as_observer();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray6D( FArray6D< U > const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 I6_( a.I6_ )
	{
		insert_as_observer();
	}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray6D( FArray6< U > const & a ) :
	 Super( a ),
	 I1_( a.I1() ),
	 I2_( a.I2() ),
	 I3_( a.I3() ),
	 I4_( a.I4() ),
	 I5_( a.I5() ),
	 I6_( a.I6() )
	{
		insert_as_observer();
	}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray6D( FArray6S< U > const & a ) :
	 Super( a ),
	 I1_( 1, a.u1() ),
	 I2_( 1, a.u2() ),
	 I3_( 1, a.u3() ),
	 I4_( 1, a.u4() ),
	 I5_( 1, a.u5() ),
	 I6_( 1, a.u6() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
				for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
							for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
								for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
									reassign( l, a( i1, i2, i3, i4, i5, i6 ) );
								}
							}
						}
					}
				}
			}
		}
		insert_as_observer();
	}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	FArray6D( MArray6< A, M > const & a ) :
	 Super( a ),
	 I1_( 1, a.u1() ),
	 I2_( 1, a.u2() ),
	 I3_( 1, a.u3() ),
	 I4_( 1, a.u4() ),
	 I5_( 1, a.u5() ),
	 I6_( 1, a.u6() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				size_type l( 0 );
				for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
								for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
									for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
										reassign( l, a( i1, i2, i3, i4, i5, i6 ) );
									}
								}
							}
						}
					}
				}
			}
		}
		insert_as_observer();
	}

	// Sticky Initializer Value Constructor
	inline
	explicit
	FArray6D( Sticky< T > const & t ) :
	 initializer_( t )
	{
		insert_as_observer();
	}

	// IndexRange Constructor
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Initializer Value Constructor
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, T const & t ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value Constructor
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, Sticky< T > const & t ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value + Initializer Value Constructor
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, Sticky< T > const & t, T const & u ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		operator =( u );
		insert_as_observer();
	}

	// IndexRange + Initializer Function Constructor
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, InitializerFunction const & fxn ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 initializer_( fxn )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, std::initializer_list< U > const l ) :
	 Super( l ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		assert( size_ == l.size() );
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, Sticky< T > const & t, std::initializer_list< U > const l ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 initializer_( t )
	{
		assert( size_ == l.size() );
		setup_real();
		initialize();
		std::copy( l.begin(), l.end(), data_ );
		insert_as_observer();
	}

	// IndexRange + Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, FArray6< U > const & a ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer + Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, Sticky< T > const & t, FArray6< U > const & a ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					data_[ i ] = a[ i ];
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, FArray6S< U > const & a ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			assert( conformable( a ) );
			size_type l( 0 );
			for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
				for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
							for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
								for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
									reassign( l, a( i1, i2, i3, i4, i5, i6 ) );
								}
							}
						}
					}
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + MArray Constructor Template
	template< class A, typename M >
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, MArray6< A, M > const & a ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				size_type l( 0 );
				for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
							for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
								for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
									for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
										reassign( l, a( i1, i2, i3, i4, i5, i6 ) );
									}
								}
							}
						}
					}
				}
			}
		}
		insert_as_observer();
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( FArray6< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + Base Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, FArray< U > const & a ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( size_ == a.size() );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// Base + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray6D( FArray< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 ) :
	 Super( size_of( I1, I2, I3, I4, I5, I6 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 I5_( I5 ),
	 I6_( I6 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( size_ == a.size() );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// Range Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	range( FArray6< U > const & a )
	{
		return FArray6D( a.I1(), a.I2(), a.I3(), a.I4(), a.I5(), a.I6() );
	}

	// Range + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	range( FArray6< U > const & a, T const & t )
	{
		return FArray6D( a.I1(), a.I2(), a.I3(), a.I4(), a.I5(), a.I6(), t );
	}

	// Array Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	shape( FArray6< U > const & a )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	}

	// Array Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	shape( FArray6< U > const & a, T const & t )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), t );
	}

	// Slice Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	shape( FArray6S< U > const & a )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	}

	// Slice Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	shape( FArray6S< U > const & a, T const & t )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), t );
	}

	// MArray Shape Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray6D
	shape( MArray6< A, M > const & a )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	}

	// MArray Shape + Initializer Value Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray6D
	shape( MArray6< A, M > const & a, T const & t )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	one_based( FArray6< U > const & a )
	{
		return FArray6D( a, a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	inline
	static
	FArray6D
	one_based( FArray6S< U > const & a )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), a );
	}

	// One-Based MArray Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray6D
	one_based( MArray6< A, M > const & a )
	{
		return FArray6D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), a );
	}

	// Destructor
	inline
	virtual
	~FArray6D()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray6D &
	operator =( FArray6D const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray6D &
	operator =( Super const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator =( FArray6< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator =( FArray6S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray6D &
	operator =( MArray6< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator +=( FArray6< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator -=( FArray6< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator *=( FArray6< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator /=( FArray6< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator +=( FArray6S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator -=( FArray6S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator *=( FArray6S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	operator /=( FArray6S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray6D &
	operator +=( MArray6< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray6D &
	operator -=( MArray6< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray6D &
	operator *=( MArray6< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray6D &
	operator /=( MArray6< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	and_equals( FArray6< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	or_equals( FArray6< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	and_equals( FArray6S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray6D &
	or_equals( FArray6S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray6D &
	and_equals( MArray6< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray6D &
	or_equals( MArray6< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray6D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray6D &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray6D &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray6D &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray6D &
	operator /=( T const & t )
	{
		Base::operator /=( t );
		return *this;
	}

public: // Subscript

	// Const Tail Starting at array( i1, i2, i3, i4, i5, i6 )
	inline
	Tail const
	a( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) && ( I4_.contains( i4 ) ) && ( I5_.contains( i5 ) ) && ( I6_.contains( i6 ) ) );
		size_type const offset( ( ( ( ( ( ( ( ( ( ( i6 * z5_ ) + i5 ) * z4_ ) + i4 ) * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), data_size_ - offset );
	}

	// Tail Starting at array( i1, i2, i3, i4, i5, i6 )
	inline
	Tail
	a( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 )
	{
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) && ( I4_.contains( i4 ) ) && ( I5_.contains( i5 ) ) && ( I6_.contains( i6 ) ) );
		size_type const offset( ( ( ( ( ( ( ( ( ( ( i6 * z5_ ) + i5 ) * z4_ ) + i4 ) * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( data_ + offset, data_size_ - offset );
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		assert( ( I1_.initialized() ) && ( I2_.initialized() ) && ( I3_.initialized() ) && ( I4_.initialized() ) && ( I5_.initialized() ) && ( I6_.initialized() ) );
		return ( ( ( ( ( ( ( ( ( ( ( i6 * z5_ ) + i5 ) * z4_ ) + i4 ) * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
	}

	// array[ i ] const: Linear Subscript
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// array[ i ]: Linear Subscript
	inline
	T &
	operator []( size_type const i )
	{
		assert( i < size_ );
		return data_[ i ];
	}

public: // Predicate

	// Dimensions Initialized?
	inline
	bool
	dimensions_initialized() const
	{
		return ( ( I1_.initialized() ) && ( I2_.initialized() ) && ( I3_.initialized() ) && ( I4_.initialized() ) && ( I5_.initialized() ) && ( I6_.initialized() ) );
	}

	// Contains Indexed Element?
	inline
	bool
	contains( int const i1, int const i2, int const i3, int const i4, int const i5, int const i6 ) const
	{
		return ( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) && ( I4_.contains( i4 ) ) && ( I5_.contains( i5 ) ) && ( I6_.contains( i6 ) ) );
	}

	// Initializer Active?
	inline
	bool
	initializer_active() const
	{
		return initializer_.is_active();
	}

public: // Inspector

	// IndexRange of Dimension 1
	inline
	IR const &
	I1() const
	{
		return I1_;
	}

	// Lower Index of Dimension 1
	inline
	int
	l1() const
	{
		return I1_.l();
	}

	// Upper Index of Dimension 1
	inline
	int
	u1() const
	{
		return I1_.u();
	}

	// IndexRange of Dimension 2
	inline
	IR const &
	I2() const
	{
		return I2_;
	}

	// Lower Index of Dimension 2
	inline
	int
	l2() const
	{
		return I2_.l();
	}

	// Upper Index of Dimension 2
	inline
	int
	u2() const
	{
		return I2_.u();
	}

	// IndexRange of Dimension 3
	inline
	IR const &
	I3() const
	{
		return I3_;
	}

	// Lower Index of Dimension 3
	inline
	int
	l3() const
	{
		return I3_.l();
	}

	// Upper Index of Dimension 3
	inline
	int
	u3() const
	{
		return I3_.u();
	}

	// IndexRange of Dimension 4
	inline
	IR const &
	I4() const
	{
		return I4_;
	}

	// Lower Index of Dimension 4
	inline
	int
	l4() const
	{
		return I4_.l();
	}

	// Upper Index of Dimension 4
	inline
	int
	u4() const
	{
		return I4_.u();
	}

	// IndexRange of Dimension 5
	inline
	IR const &
	I5() const
	{
		return I5_;
	}

	// Lower Index of Dimension 5
	inline
	int
	l5() const
	{
		return I5_.l();
	}

	// Upper Index of Dimension 5
	inline
	int
	u5() const
	{
		return I5_.u();
	}

	// IndexRange of Dimension 6
	inline
	IR const &
	I6() const
	{
		return I6_;
	}

	// Lower Index of Dimension 6
	inline
	int
	l6() const
	{
		return I6_.l();
	}

	// Upper Index of Dimension 6
	inline
	int
	u6() const
	{
		return I6_.u();
	}

	// Size of Dimension 6
	inline
	size_type
	size6() const
	{
		return I6_.size();
	}

	// Size of Dimension 6
	inline
	int
	isize6() const
	{
		return I6_.isize();
	}

public: // Modifier

	// Clear
	inline
	FArray6D &
	clear()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		I3_.clear_no_notify();
		I4_.clear_no_notify();
		I5_.clear_no_notify();
		I6_.clear_no_notify();
		initializer_.clear();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray6D &
	allocate( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		I4_.assign_no_notify( I4 );
		I5_.assign_no_notify( I5 );
		I6_.assign_no_notify( I6 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray6D &
	allocate( FArray6< U > const & a )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		I4_.assign_no_notify( a.I4() );
		I5_.assign_no_notify( a.I5() );
		I6_.assign_no_notify( a.I6() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Deallocate
	inline
	FArray6D &
	deallocate()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		I3_.clear_no_notify();
		I4_.clear_no_notify();
		I5_.clear_no_notify();
		I6_.clear_no_notify();
		initializer_.clear_nonsticky();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray6D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		I4_.assign_no_notify( I4 );
		I5_.assign_no_notify( I5 );
		I6_.assign_no_notify( I6 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	inline
	FArray6D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, T const & t )
	{
		initializer_ = t;
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		I4_.assign_no_notify( I4 );
		I5_.assign_no_notify( I5 );
		I6_.assign_no_notify( I6 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	inline
	FArray6D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		I4_.assign_no_notify( I4 );
		I5_.assign_no_notify( I5 );
		I6_.assign_no_notify( I6 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray6D &
	dimension( FArray6< U > const & a )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		I4_.assign_no_notify( a.I4() );
		I5_.assign_no_notify( a.I5() );
		I6_.assign_no_notify( a.I6() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	inline
	FArray6D &
	dimension( FArray6< U > const & a, T const & t )
	{
		initializer_ = t;
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		I4_.assign_no_notify( a.I4() );
		I5_.assign_no_notify( a.I5() );
		I6_.assign_no_notify( a.I6() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	inline
	FArray6D &
	dimension( FArray6< U > const & a, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		I4_.assign_no_notify( a.I4() );
		I5_.assign_no_notify( a.I5() );
		I6_.assign_no_notify( a.I6() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Data-Preserving Redimension by IndexRange
	inline
	FArray6D &
	redimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 )
	{
		FArray6D o( I1, I2, I3, I4, I5, I6 );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
				int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
				int const b3( std::max( I3.l(), l3() ) ), e3( std::min( I3.u(), u3() ) );
				int const b4( std::max( I4.l(), l4() ) ), e4( std::min( I4.u(), u4() ) );
				int const b5( std::max( I5.l(), l5() ) ), e5( std::min( I5.u(), u5() ) );
				int const b6( std::max( I6.l(), l6() ) ), e6( std::min( I6.u(), u6() ) );
				for ( int i6 = b6; i6 <= e6; ++i6 ) {
					for ( int i5 = b5; i5 <= e5; ++i5 ) {
						for ( int i4 = b4; i4 <= e4; ++i4 ) {
							for ( int i3 = b3; i3 <= e3; ++i3 ) {
								for ( int i2 = b2; i2 <= e2; ++i2 ) {
									for ( int i1 = b1; i1 <= e1; ++i1 ) {
										o( i1, i2, i3, i4, i5, i6 ) = operator ()( i1, i2, i3, i4, i5, i6 );
									}
								}
							}
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by IndexRange + Fill Value
	inline
	FArray6D &
	redimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6, T const & t )
	{
		FArray6D o( I1, I2, I3, I4, I5, I6, t );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
				int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
				int const b3( std::max( I3.l(), l3() ) ), e3( std::min( I3.u(), u3() ) );
				int const b4( std::max( I4.l(), l4() ) ), e4( std::min( I4.u(), u4() ) );
				int const b5( std::max( I5.l(), l5() ) ), e5( std::min( I5.u(), u5() ) );
				int const b6( std::max( I6.l(), l6() ) ), e6( std::min( I6.u(), u6() ) );
				for ( int i6 = b6; i6 <= e6; ++i6 ) {
					for ( int i5 = b5; i5 <= e5; ++i5 ) {
						for ( int i4 = b4; i4 <= e4; ++i4 ) {
							for ( int i3 = b3; i3 <= e3; ++i3 ) {
								for ( int i2 = b2; i2 <= e2; ++i2 ) {
									for ( int i1 = b1; i1 <= e1; ++i1 ) {
										o( i1, i2, i3, i4, i5, i6 ) = operator ()( i1, i2, i3, i4, i5, i6 );
									}
								}
							}
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array Template
	template< typename U >
	inline
	FArray6D &
	redimension( FArray6< U > const & a )
	{
		FArray6D o( a.I1(), a.I2(), a.I3(), a.I4(), a.I5(), a.I6() );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
				int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
				int const b3( std::max( a.l3(), l3() ) ), e3( std::min( a.u3(), u3() ) );
				int const b4( std::max( a.l4(), l4() ) ), e4( std::min( a.u4(), u4() ) );
				int const b5( std::max( a.l5(), l5() ) ), e5( std::min( a.u5(), u5() ) );
				int const b6( std::max( a.l6(), l6() ) ), e6( std::min( a.u6(), u6() ) );
				for ( int i6 = b6; i6 <= e6; ++i6 ) {
					for ( int i5 = b5; i5 <= e5; ++i5 ) {
						for ( int i4 = b4; i4 <= e4; ++i4 ) {
							for ( int i3 = b3; i3 <= e3; ++i3 ) {
								for ( int i2 = b2; i2 <= e2; ++i2 ) {
									for ( int i1 = b1; i1 <= e1; ++i1 ) {
										o( i1, i2, i3, i4, i5, i6 ) = operator ()( i1, i2, i3, i4, i5, i6 );
									}
								}
							}
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array + Fill Value Template
	template< typename U >
	inline
	FArray6D &
	redimension( FArray6< U > const & a, T const & t )
	{
		FArray6D o( a.I1(), a.I2(), a.I3(), a.I4(), a.I5(), a.I6(), t );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
				int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
				int const b3( std::max( a.l3(), l3() ) ), e3( std::min( a.u3(), u3() ) );
				int const b4( std::max( a.l4(), l4() ) ), e4( std::min( a.u4(), u4() ) );
				int const b5( std::max( a.l5(), l5() ) ), e5( std::min( a.u5(), u5() ) );
				int const b6( std::max( a.l6(), l6() ) ), e6( std::min( a.u6(), u6() ) );
				for ( int i6 = b6; i6 <= e6; ++i6 ) {
					for ( int i5 = b5; i5 <= e5; ++i5 ) {
						for ( int i4 = b4; i4 <= e4; ++i4 ) {
							for ( int i3 = b3; i3 <= e3; ++i3 ) {
								for ( int i2 = b2; i2 <= e2; ++i2 ) {
									for ( int i1 = b1; i1 <= e1; ++i1 ) {
										o( i1, i2, i3, i4, i5, i6 ) = operator ()( i1, i2, i3, i4, i5, i6 );
									}
								}
							}
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Set Initializer Value
	inline
	FArray6D &
	initializer( T const & t )
	{
		initializer_ = t;
		return *this;
	}

	// Set Initializer Function
	inline
	FArray6D &
	initializer( InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		return *this;
	}

	// Clear Initializer
	inline
	FArray6D &
	initializer_clear()
	{
		initializer_.clear();
		return *this;
	}

	// Initialize
	inline
	FArray6D &
	initialize()
	{
		if ( ( initializer_.is_active() ) && ( dimensions_initialized() ) ) {
			if ( initializer_.is_value() ) {
				reassign( initializer_.value() );
			} else if ( initializer_.is_function() ) {
				initializer_.function()( *this );
			}
		}
		return *this;
	}

	// Swap
	inline
	FArray6D &
	swap( FArray6D & v )
	{
		swap6DB( v );
		I1_.swap_no_notify( v.I1_ );
		I2_.swap_no_notify( v.I2_ );
		I3_.swap_no_notify( v.I3_ );
		I4_.swap_no_notify( v.I4_ );
		I5_.swap_no_notify( v.I5_ );
		I6_.swap_no_notify( v.I6_ );
		std::swap( initializer_, v.initializer_ );
		notify(); // So proxy FArrays can reattach
		v.notify(); // So proxy FArrays can reattach
		return *this;
	}

public: // Observer Modifier

	// Update
	inline
	void
	update()
	{
		dimension_real();
		initialize();
	}

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & )
	{}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( SIR const & I1, SIR const & I2, SIR const & I3, SIR const & I4, SIR const & I5, SIR const & I6 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		I4_.assign_no_notify( I4 );
		I5_.assign_no_notify( I5 );
		I6_.assign_no_notify( I6 );
		dimension_real();
		initialize();
		notify();
	}

private: // Functions

	// Setup for IndexRange Constructor
	inline
	void
	setup_real()
	{
		z1_ = I1_.size();
		z2_ = I2_.size();
		z3_ = I3_.size();
		z4_ = I4_.size();
		z5_ = I5_.size();
		if ( dimensions_initialized() ) {
			shift_set( ( ( ( ( ( ( ( ( ( I6_.lz() * z5_ ) + I5_.lz() ) * z4_ ) + I4_.lz() ) * z3_ ) + I3_.lz() ) * z2_ ) + I2_.lz() ) * z1_ ) + I1_.lz() );
		} else {
			shift_set( 0 );
		}
	}

	// Dimension by Current IndexRanges
	inline
	void
	dimension_real()
	{
		z1_ = I1_.size();
		z2_ = I2_.size();
		z3_ = I3_.size();
		z4_ = I4_.size();
		z5_ = I5_.size();
		if ( dimensions_initialized() ) {
			resize( size_of( z1_, z2_, z3_, z4_, z5_, I6_.size() ) );
			shift_set( ( ( ( ( ( ( ( ( ( I6_.lz() * z5_ ) + I5_.lz() ) * z4_ ) + I4_.lz() ) * z3_ ) + I3_.lz() ) * z2_ ) + I2_.lz() ) * z1_ ) + I1_.lz() );
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
			size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
		} else {
			Base::clear();
		}
	}

	// Insert as Observer of the IndexRanges
	inline
	void
	insert_as_observer()
	{
		I1_.insert_observer( *this );
		I2_.insert_observer( *this );
		I3_.insert_observer( *this );
		I4_.insert_observer( *this );
		I5_.insert_observer( *this );
		I6_.insert_observer( *this );
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Remove as Observer of the IndexRanges
	inline
	void
	remove_as_observer()
	{
		I1_.remove_observer( *this );
		I2_.remove_observer( *this );
		I3_.remove_observer( *this );
		I4_.remove_observer( *this );
		I5_.remove_observer( *this );
		I6_.remove_observer( *this );
	}

#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
	// Report size if at least value defined for OBJEXXFCL_FARRAY_SIZE_REPORT
	//  Size is based on sizeof( T ) so T-controlled heap memory is not counted
	inline
	void
	size_report() const
	{
		if ( size_ * sizeof( T ) >= OBJEXXFCL_FARRAY_SIZE_REPORT ) {
			std::cout << "  Index ranges: " << I1_ << ' ' << I2_ << ' ' << I3_ << ' ' << I4_ << ' ' << I5_ << ' ' << I6_ << std::endl;
		}
	}
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT

private: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2
	IR I3_; // Index range of dim 3
	IR I4_; // Index range of dim 4
	IR I5_; // Index range of dim 5
	IR I6_; // Index range of dim 6

	Initializer initializer_; // Array initializer

}; // FArray6D

// Swap
template< typename T >
void
swap( FArray6D< T > & a, FArray6D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental

// Array == Array
template< typename T >
inline
FArray6D< bool >
operator ==( FArray6< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	eq_elemental( a, b, r );
	return r;
}

// Array != Array
template< typename T >
inline
FArray6D< bool >
operator !=( FArray6< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	ne_elemental( a, b, r );
	return r;
}

// Array < Array
template< typename T >
inline
FArray6D< bool >
operator <( FArray6< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	lt_elemental( a, b, r );
	return r;
}

// Array <= Array
template< typename T >
inline
FArray6D< bool >
operator <=( FArray6< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	le_elemental( a, b, r );
	return r;
}

// Array > Array
template< typename T >
inline
FArray6D< bool >
operator >( FArray6< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	gt_elemental( a, b, r );
	return r;
}

// Array >= Array
template< typename T >
inline
FArray6D< bool >
operator >=( FArray6< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	ge_elemental( a, b, r );
	return r;
}

// Array == Value
template< typename T >
inline
FArray6D< bool >
operator ==( FArray6< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	eq_elemental( a, t, r );
	return r;
}

// Array != Value
template< typename T >
inline
FArray6D< bool >
operator !=( FArray6< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	ne_elemental( a, t, r );
	return r;
}

// Array < Value
template< typename T >
inline
FArray6D< bool >
operator <( FArray6< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	lt_elemental( a, t, r );
	return r;
}

// Array <= Value
template< typename T >
inline
FArray6D< bool >
operator <=( FArray6< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	le_elemental( a, t, r );
	return r;
}

// Array > Value
template< typename T >
inline
FArray6D< bool >
operator >( FArray6< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	gt_elemental( a, t, r );
	return r;
}

// Array >= Value
template< typename T >
inline
FArray6D< bool >
operator >=( FArray6< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	ge_elemental( a, t, r );
	return r;
}

// Value == Array
template< typename T >
inline
FArray6D< bool >
operator ==( T const & t, FArray6< T > const & b )
{
	return ( b == t );
}

// Value != Array
template< typename T >
inline
FArray6D< bool >
operator !=( T const & t, FArray6< T > const & b )
{
	return ( b != t );
}

// Value < Array
template< typename T >
inline
FArray6D< bool >
operator <( T const & t, FArray6< T > const & b )
{
	return ( b > t );
}

// Value <= Array
template< typename T >
inline
FArray6D< bool >
operator <=( T const & t, FArray6< T > const & b )
{
	return ( b >= t );
}

// Value > Array
template< typename T >
inline
FArray6D< bool >
operator >( T const & t, FArray6< T > const & b )
{
	return ( b < t );
}

// Value >= Array
template< typename T >
inline
FArray6D< bool >
operator >=( T const & t, FArray6< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: Slice

// Slice == Slice
template< typename T >
inline
FArray6D< bool >
operator ==( FArray6S< T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) == b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice != Slice
template< typename T >
inline
FArray6D< bool >
operator !=( FArray6S< T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) != b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice < Slice
template< typename T >
inline
FArray6D< bool >
operator <( FArray6S< T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) < b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice <= Slice
template< typename T >
inline
FArray6D< bool >
operator <=( FArray6S< T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) <= b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice > Slice
template< typename T >
inline
FArray6D< bool >
operator >( FArray6S< T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) > b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice >= Slice
template< typename T >
inline
FArray6D< bool >
operator >=( FArray6S< T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) >= b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice == Array
template< typename T >
inline
FArray6D< bool >
operator ==( FArray6S< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) == b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice != Array
template< typename T >
inline
FArray6D< bool >
operator !=( FArray6S< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) != b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice < Array
template< typename T >
inline
FArray6D< bool >
operator <( FArray6S< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) < b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice <= Array
template< typename T >
inline
FArray6D< bool >
operator <=( FArray6S< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) <= b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice > Array
template< typename T >
inline
FArray6D< bool >
operator >( FArray6S< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) > b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice >= Array
template< typename T >
inline
FArray6D< bool >
operator >=( FArray6S< T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) >= b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// Array == Slice
template< typename T >
inline
FArray6D< bool >
operator ==( FArray6< T > const & a, FArray6S< T > const & b )
{
	return ( b == a );
}

// Array != Slice
template< typename T >
inline
FArray6D< bool >
operator !=( FArray6< T > const & a, FArray6S< T > const & b )
{
	return ( b != a );
}

// Array < Slice
template< typename T >
inline
FArray6D< bool >
operator <( FArray6< T > const & a, FArray6S< T > const & b )
{
	return ( b > a );
}

// Array <= Slice
template< typename T >
inline
FArray6D< bool >
operator <=( FArray6< T > const & a, FArray6S< T > const & b )
{
	return ( b >= a );
}

// Array > Slice
template< typename T >
inline
FArray6D< bool >
operator >( FArray6< T > const & a, FArray6S< T > const & b )
{
	return ( b < a );
}

// Array >= Slice
template< typename T >
inline
FArray6D< bool >
operator >=( FArray6< T > const & a, FArray6S< T > const & b )
{
	return ( b <= a );
}

// Slice == Value
template< typename T >
inline
FArray6D< bool >
operator ==( FArray6S< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) == t );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice != Value
template< typename T >
inline
FArray6D< bool >
operator !=( FArray6S< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) != t );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice < Value
template< typename T >
inline
FArray6D< bool >
operator <( FArray6S< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) < t );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice <= Value
template< typename T >
inline
FArray6D< bool >
operator <=( FArray6S< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) <= t );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice > Value
template< typename T >
inline
FArray6D< bool >
operator >( FArray6S< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) > t );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice >= Value
template< typename T >
inline
FArray6D< bool >
operator >=( FArray6S< T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) >= t );
						}
					}
				}
			}
		}
	}
	return r;
}

// Value == Slice
template< typename T >
inline
FArray6D< bool >
operator ==( T const & t, FArray6S< T > const & b )
{
	return ( b == t );
}

// Value != Slice
template< typename T >
inline
FArray6D< bool >
operator !=( T const & t, FArray6S< T > const & b )
{
	return ( b != t );
}

// Value < Slice
template< typename T >
inline
FArray6D< bool >
operator <( T const & t, FArray6S< T > const & b )
{
	return ( b > t );
}

// Value <= Slice
template< typename T >
inline
FArray6D< bool >
operator <=( T const & t, FArray6S< T > const & b )
{
	return ( b >= t );
}

// Value > Slice
template< typename T >
inline
FArray6D< bool >
operator >( T const & t, FArray6S< T > const & b )
{
	return ( b < t );
}

// Value >= Slice
template< typename T >
inline
FArray6D< bool >
operator >=( T const & t, FArray6S< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: MArray

// MArray == MArray
template< class A, typename T >
inline
FArray6D< bool >
operator ==( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) == b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray != MArray
template< class A, typename T >
inline
FArray6D< bool >
operator !=( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) != b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray < MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) < b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray <= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <=( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) <= b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray > MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) > b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray >= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >=( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) >= b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray == Array
template< class A, typename T >
inline
FArray6D< bool >
operator ==( MArray6< A, T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) == b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray != Array
template< class A, typename T >
inline
FArray6D< bool >
operator !=( MArray6< A, T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) != b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray < Array
template< class A, typename T >
inline
FArray6D< bool >
operator <( MArray6< A, T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) < b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray <= Array
template< class A, typename T >
inline
FArray6D< bool >
operator <=( MArray6< A, T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) <= b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray > Array
template< class A, typename T >
inline
FArray6D< bool >
operator >( MArray6< A, T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) > b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray >= Array
template< class A, typename T >
inline
FArray6D< bool >
operator >=( MArray6< A, T > const & a, FArray6< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	typename FArray6< T >::size_type l( 0 );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) >= b[ l ] );
						}
					}
				}
			}
		}
	}
	return r;
}

// Array == MArray
template< class A, typename T >
inline
FArray6D< bool >
operator ==( FArray6< T > const & a, MArray6< A, T > const & b )
{
	return ( b == a );
}

// Array != MArray
template< class A, typename T >
inline
FArray6D< bool >
operator !=( FArray6< T > const & a, MArray6< A, T > const & b )
{
	return ( b != a );
}

// Array < MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <( FArray6< T > const & a, MArray6< A, T > const & b )
{
	return ( b > a );
}

// Array <= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <=( FArray6< T > const & a, MArray6< A, T > const & b )
{
	return ( b >= a );
}

// Array > MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >( FArray6< T > const & a, MArray6< A, T > const & b )
{
	return ( b < a );
}

// Array >= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >=( FArray6< T > const & a, MArray6< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Slice
template< class A, typename T >
inline
FArray6D< bool >
operator ==( MArray6< A, T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) == b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray != Slice
template< class A, typename T >
inline
FArray6D< bool >
operator !=( MArray6< A, T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) != b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray < Slice
template< class A, typename T >
inline
FArray6D< bool >
operator <( MArray6< A, T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) < b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray <= Slice
template< class A, typename T >
inline
FArray6D< bool >
operator <=( MArray6< A, T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) <= b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray > Slice
template< class A, typename T >
inline
FArray6D< bool >
operator >( MArray6< A, T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) > b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray >= Slice
template< class A, typename T >
inline
FArray6D< bool >
operator >=( MArray6< A, T > const & a, FArray6S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) >= b( i1, i2, i3, i4, i5, i6 ) );
						}
					}
				}
			}
		}
	}
	return r;
}

// Slice == MArray
template< class A, typename T >
inline
FArray6D< bool >
operator ==( FArray6S< T > const & a, MArray6< A, T > const & b )
{
	return ( b == a );
}

// Slice != MArray
template< class A, typename T >
inline
FArray6D< bool >
operator !=( FArray6S< T > const & a, MArray6< A, T > const & b )
{
	return ( b != a );
}

// Slice < MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <( FArray6S< T > const & a, MArray6< A, T > const & b )
{
	return ( b > a );
}

// Slice <= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <=( FArray6S< T > const & a, MArray6< A, T > const & b )
{
	return ( b >= a );
}

// Slice > MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >( FArray6S< T > const & a, MArray6< A, T > const & b )
{
	return ( b < a );
}

// Slice >= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >=( FArray6S< T > const & a, MArray6< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Value
template< class A, typename T >
inline
FArray6D< bool >
operator ==( MArray6< A, T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) == t );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray != Value
template< class A, typename T >
inline
FArray6D< bool >
operator !=( MArray6< A, T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) != t );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray < Value
template< class A, typename T >
inline
FArray6D< bool >
operator <( MArray6< A, T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) < t );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray <= Value
template< class A, typename T >
inline
FArray6D< bool >
operator <=( MArray6< A, T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) <= t );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray > Value
template< class A, typename T >
inline
FArray6D< bool >
operator >( MArray6< A, T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) > t );
						}
					}
				}
			}
		}
	}
	return r;
}

// MArray >= Value
template< class A, typename T >
inline
FArray6D< bool >
operator >=( MArray6< A, T > const & a, T const & t )
{
	FArray6D< bool > r( FArray6D< bool >::shape( a ) );
	for ( int i6 = 1, e6 = r.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
							r( i1, i2, i3, i4, i5, i6 ) = ( a( i1, i2, i3, i4, i5, i6 ) >= t );
						}
					}
				}
			}
		}
	}
	return r;
}

// Value == MArray
template< class A, typename T >
inline
FArray6D< bool >
operator ==( T const & t, MArray6< A, T > const & b )
{
	return ( b == t );
}

// Value != MArray
template< class A, typename T >
inline
FArray6D< bool >
operator !=( T const & t, MArray6< A, T > const & b )
{
	return ( b != t );
}

// Value < MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <( T const & t, MArray6< A, T > const & b )
{
	return ( b > t );
}

// Value <= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator <=( T const & t, MArray6< A, T > const & b )
{
	return ( b >= t );
}

// Value > MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >( T const & t, MArray6< A, T > const & b )
{
	return ( b < t );
}

// Value >= MArray
template< class A, typename T >
inline
FArray6D< bool >
operator >=( T const & t, MArray6< A, T > const & b )
{
	return ( b <= t );
}

// Generator

// -Array
template< typename T >
inline
FArray6D< T >
operator -( FArray6< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Array + Array
template< typename T >
inline
FArray6D< T >
operator +( FArray6< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Array
template< typename T >
inline
FArray6D< T >
operator -( FArray6< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Array
template< typename T >
inline
FArray6D< T >
operator *( FArray6< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Array
template< typename T >
inline
FArray6D< T >
operator /( FArray6< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Value
template< typename T >
inline
FArray6D< T >
operator +( FArray6< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Array
template< typename T >
inline
FArray6D< T >
operator +( T const & t, FArray6< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += t;
	return r;
}

// Array - Value
template< typename T >
inline
FArray6D< T >
operator -( FArray6< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Array
template< typename T >
inline
FArray6D< T >
operator -( T const & t, FArray6< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Array * Value
template< typename T >
inline
FArray6D< T >
operator *( FArray6< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Array
template< typename T >
inline
FArray6D< T >
operator *( T const & t, FArray6< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Array / Value
template< typename T >
inline
FArray6D< T >
operator /( FArray6< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Array
template< typename T >
inline
FArray6D< T >
operator /( T const & t, FArray6< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Array && Array
template< typename T >
inline
FArray6D< T >
operator &&( FArray6< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Array || Array
template< typename T >
inline
FArray6D< T >
operator ||( FArray6< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: Slice

// -Slice
template< typename T >
inline
FArray6D< T >
operator -( FArray6S< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Slice + Slice
template< typename T >
inline
FArray6D< T >
operator +( FArray6S< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Slice
template< typename T >
inline
FArray6D< T >
operator -( FArray6S< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Slice
template< typename T >
inline
FArray6D< T >
operator *( FArray6S< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Slice
template< typename T >
inline
FArray6D< T >
operator /( FArray6S< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Array
template< typename T >
inline
FArray6D< T >
operator +( FArray6S< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Array
template< typename T >
inline
FArray6D< T >
operator -( FArray6S< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Array
template< typename T >
inline
FArray6D< T >
operator *( FArray6S< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Array
template< typename T >
inline
FArray6D< T >
operator /( FArray6S< T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Slice
template< typename T >
inline
FArray6D< T >
operator +( FArray6< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Slice
template< typename T >
inline
FArray6D< T >
operator -( FArray6< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Slice
template< typename T >
inline
FArray6D< T >
operator *( FArray6< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Slice
template< typename T >
inline
FArray6D< T >
operator /( FArray6< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Value
template< typename T >
inline
FArray6D< T >
operator +( FArray6S< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Slice
template< typename T >
inline
FArray6D< T >
operator +( T const & t, FArray6S< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += t;
	return r;
}

// Slice - Value
template< typename T >
inline
FArray6D< T >
operator -( FArray6S< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Slice
template< typename T >
inline
FArray6D< T >
operator -( T const & t, FArray6S< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Slice * Value
template< typename T >
inline
FArray6D< T >
operator *( FArray6S< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Slice
template< typename T >
inline
FArray6D< T >
operator *( T const & t, FArray6S< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Slice / Value
template< typename T >
inline
FArray6D< T >
operator /( FArray6S< T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Slice
template< typename T >
inline
FArray6D< T >
operator /( T const & t, FArray6S< T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Slice && Slice
template< typename T >
inline
FArray6D< T >
operator &&( FArray6S< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Slice || Slice
template< typename T >
inline
FArray6D< T >
operator ||( FArray6S< T > const & a, FArray6S< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: MArray

// -MArray
template< class A, typename T >
inline
FArray6D< T >
operator -( MArray6< A, T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// MArray + MArray
template< class A, typename T >
inline
FArray6D< T >
operator +( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - MArray
template< class A, typename T >
inline
FArray6D< T >
operator -( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * MArray
template< class A, typename T >
inline
FArray6D< T >
operator *( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / MArray
template< class A, typename T >
inline
FArray6D< T >
operator /( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Array
template< class A, typename T >
inline
FArray6D< T >
operator +( MArray6< A, T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - Array
template< class A, typename T >
inline
FArray6D< T >
operator -( MArray6< A, T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * Array
template< class A, typename T >
inline
FArray6D< T >
operator *( MArray6< A, T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / Array
template< class A, typename T >
inline
FArray6D< T >
operator /( MArray6< A, T > const & a, FArray6< T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + MArray
template< class A, typename T >
inline
FArray6D< T >
operator +( FArray6< T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - MArray
template< class A, typename T >
inline
FArray6D< T >
operator -( FArray6< T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * MArray
template< class A, typename T >
inline
FArray6D< T >
operator *( FArray6< T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / MArray
template< class A, typename T >
inline
FArray6D< T >
operator /( FArray6< T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Value
template< class A, typename T >
inline
FArray6D< T >
operator +( MArray6< A, T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + MArray
template< class A, typename T >
inline
FArray6D< T >
operator +( T const & t, MArray6< A, T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r += t;
	return r;
}

// MArray - Value
template< class A, typename T >
inline
FArray6D< T >
operator -( MArray6< A, T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - MArray
template< class A, typename T >
inline
FArray6D< T >
operator -( T const & t, MArray6< A, T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// MArray * Value
template< class A, typename T >
inline
FArray6D< T >
operator *( MArray6< A, T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * MArray
template< class A, typename T >
inline
FArray6D< T >
operator *( T const & t, MArray6< A, T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r *= t;
	return r;
}

// MArray / Value
template< class A, typename T >
inline
FArray6D< T >
operator /( MArray6< A, T > const & a, T const & t )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / MArray
template< class A, typename T >
inline
FArray6D< T >
operator /( T const & t, MArray6< A, T > const & a )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// MArray && MArray
template< class A, typename T >
inline
FArray6D< T >
operator &&( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// MArray || MArray
template< class A, typename T >
inline
FArray6D< T >
operator ||( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	FArray6D< T > r( FArray6D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

} // ObjexxFCL

#ifndef NO_STD_SWAP_OVERLOADS

// std::swap Overloads for Efficiency
//
// Technically you cannot add template functions overloads to namespace std
// but this works with most compilers and makes it much faster if someone uses
// std::swap instead of swap or ObjexxFCL::swap.  The legal alternative would be
// to add specializations of swap for each anticipated instantiation.

namespace std {

// std::swap( FArray6D, FArray6D )
template< typename T >
inline
void
swap( ObjexxFCL::FArray6D< T > & a, ObjexxFCL::FArray6D< T > & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_FArray6D_hh_INCLUDED
