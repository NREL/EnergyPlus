#ifndef ObjexxFCL_FArray3D_hh_INCLUDED
#define ObjexxFCL_FArray3D_hh_INCLUDED

// FArray3D: Fortran-Compatible 3D Array
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
#include <ObjexxFCL/FArray3D.fwd.hh>
#include <ObjexxFCL/FArray3.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DynamicIndexRange.hh>
#include <ObjexxFCL/FArrayInitializer.hh>

namespace ObjexxFCL {

// FArray3D: Fortran-Compatible 3D Array
template< typename T >
class FArray3D : public FArray3< T >, public ObserverMulti
{

private: // Types

	typedef  FArray3< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  typename Super::arg_FArray  arg_FArray;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class FArray3D;
	friend class FArray3P< T >;
	friend class FArray3A< T >;

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

	typedef  FArrayInitializer< T, ObjexxFCL::FArray3D >  Initializer;
	typedef  typename Initializer::Function  InitializerFunction;

	using Super::conformable;
	using Super::isize1;
	using Super::isize2;
	using Super::isize3;
	using Super::l;
	using Super::operator ();
	using Super::reassign;
	using Super::resize;
	using Super::shift_set;
	using Super::size1;
	using Super::size2;
	using Super::size3;
	using Super::size_of;
	using Super::swap3DB;
	using Super::u;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;
	using Super::z1_;
	using Super::z2_;

public: // Creation

	// Default Constructor
	inline
	FArray3D()
	{
		insert_as_observer();
	}

	// Copy Constructor
	inline
	FArray3D( FArray3D const & a ) :
	 Super( a ),
	 ObserverMulti(),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ )
	{
		insert_as_observer();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray3D( FArray3D< U > const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ )
	{
		insert_as_observer();
	}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray3D( FArray3< U > const & a ) :
	 Super( a ),
	 I1_( a.I1() ),
	 I2_( a.I2() ),
	 I3_( a.I3() )
	{
		insert_as_observer();
	}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray3D( FArray3S< U > const & a ) :
	 Super( a ),
	 I1_( 1, a.u1() ),
	 I2_( 1, a.u2() ),
	 I3_( 1, a.u3() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						reassign( l, a( i1, i2, i3 ) );
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
	FArray3D( MArray3< A, M > const & a ) :
	 Super( a ),
	 I1_( 1, a.u1() ),
	 I2_( 1, a.u2() ),
	 I3_( 1, a.u3() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				size_type l( 0 );
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							reassign( l, a( i1, i2, i3 ) );
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
	FArray3D( Sticky< T > const & t ) :
	 initializer_( t )
	{
		insert_as_observer();
	}

	// IndexRange Constructor
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3 ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
	{
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Initializer Value Constructor
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3, T const & t ) :
	 Super( size_of( I1, I2, I3 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value Constructor
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3, Sticky< T > const & t ) :
	 Super( size_of( I1, I2, I3 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value + Initializer Value Constructor
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3, Sticky< T > const & t, T const & u ) :
	 Super( size_of( I1, I2, I3 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		operator =( u );
		insert_as_observer();
	}

	// IndexRange + Initializer Function Constructor
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3, InitializerFunction const & fxn ) :
	 Super( size_of( I1, I2, I3 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 initializer_( fxn )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3, std::initializer_list< U > const l ) :
	 Super( l ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
	{
		assert( size_of( I1, I2, I3 ) == l.size() );
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3, Sticky< T > const & t, std::initializer_list< U > const l ) :
	 Super( size_of( I1, I2, I3 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
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
	FArray3D( IR const & I1, IR const & I2, IR const & I3, FArray3< U > const & a ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
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
	FArray3D( IR const & I1, IR const & I2, IR const & I3, Sticky< T > const & t, FArray3< U > const & a ) :
	 Super( size_of( I1, I2, I3 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
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
	FArray3D( IR const & I1, IR const & I2, IR const & I3, FArray3S< U > const & a ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			assert( conformable( a ) );
			size_type l( 0 );
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						reassign( l, a( i1, i2, i3 ) );
					}
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + MArray Constructor Template
	template< class A, typename M >
	inline
	FArray3D( IR const & I1, IR const & I2, IR const & I3, MArray3< A, M > const & a ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				size_type l( 0 );
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							reassign( l, a( i1, i2, i3 ) );
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
	FArray3D( FArray3< U > const & a, IR const & I1, IR const & I2, IR const & I3 ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
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
	FArray3D( IR const & I1, IR const & I2, IR const & I3, FArray< U > const & a ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
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
	FArray3D( FArray< U > const & a, IR const & I1, IR const & I2, IR const & I3 ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 )
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
	FArray3D
	range( FArray3< U > const & a )
	{
		return FArray3D( a.I1(), a.I2(), a.I3() );
	}

	// Range + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray3D
	range( FArray3< U > const & a, T const & t )
	{
		return FArray3D( a.I1(), a.I2(), a.I3(), t );
	}

	// Array Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray3D
	shape( FArray3< U > const & a )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3() );
	}

	// Array Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray3D
	shape( FArray3< U > const & a, T const & t )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3(), t );
	}

	// Slice Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray3D
	shape( FArray3S< U > const & a )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3() );
	}

	// Slice Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray3D
	shape( FArray3S< U > const & a, T const & t )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3(), t );
	}

	// MArray Shape Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray3D
	shape( MArray3< A, M > const & a )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3() );
	}

	// MArray Shape + Initializer Value Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray3D
	shape( MArray3< A, M > const & a, T const & t )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3(), t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	inline
	static
	FArray3D
	one_based( FArray3< U > const & a )
	{
		return FArray3D( a, a.isize1(), a.isize2(), a.isize3() );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	inline
	static
	FArray3D
	one_based( FArray3S< U > const & a )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3(), a );
	}

	// One-Based MArray Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray3D
	one_based( MArray3< A, M > const & a )
	{
		return FArray3D( a.isize1(), a.isize2(), a.isize3(), a );
	}

	// Destructor
	inline
	virtual
	~FArray3D()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray3D &
	operator =( FArray3D const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray3D &
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
	FArray3D &
	operator =( FArray3< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator =( FArray3S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray3D &
	operator =( MArray3< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator +=( FArray3< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator -=( FArray3< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator *=( FArray3< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator /=( FArray3< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator +=( FArray3S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator -=( FArray3S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator *=( FArray3S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	operator /=( FArray3S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray3D &
	operator +=( MArray3< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray3D &
	operator -=( MArray3< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray3D &
	operator *=( MArray3< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray3D &
	operator /=( MArray3< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	and_equals( FArray3< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	or_equals( FArray3< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	and_equals( FArray3S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray3D &
	or_equals( FArray3S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray3D &
	and_equals( MArray3< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray3D &
	or_equals( MArray3< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray3D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray3D &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray3D &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray3D &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray3D &
	operator /=( T const & t )
	{
		Base::operator /=( t );
		return *this;
	}

public: // Subscript

	// Const Tail Starting at array( i1, i2, i3 )
	inline
	Tail const
	a( int const i1, int const i2, int const i3 ) const
	{
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) );
		size_type const offset( ( ( ( ( i3 * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), data_size_ - offset );
	}

	// Tail Starting at array( i1, i2, i3 )
	inline
	Tail
	a( int const i1, int const i2, int const i3 )
	{
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) );
		size_type const offset( ( ( ( ( i3 * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( data_ + offset, data_size_ - offset );
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2, int const i3 ) const
	{
		assert( ( I1_.initialized() ) && ( I2_.initialized() ) && ( I3_.initialized() ) );
		return ( ( ( ( ( i3 * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
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
		return ( ( I1_.initialized() ) && ( I2_.initialized() ) && ( I3_.initialized() ) );
	}

	// Contains Indexed Element?
	inline
	bool
	contains( int const i1, int const i2, int const i3 ) const
	{
		return ( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) );
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

	// Size of Dimension 3
	inline
	size_type
	size3() const
	{
		return I3_.size();
	}

	// Size of Dimension 3
	inline
	int
	isize3() const
	{
		return I3_.isize();
	}

public: // Modifier

	// Clear
	inline
	FArray3D &
	clear()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		I3_.clear_no_notify();
		initializer_.clear();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray3D &
	allocate( IR const & I1, IR const & I2, IR const & I3 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray3D &
	allocate( FArray3< U > const & a )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Deallocate
	inline
	FArray3D &
	deallocate()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		I3_.clear_no_notify();
		initializer_.clear_nonsticky();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray3D &
	dimension( IR const & I1, IR const & I2, IR const & I3 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	inline
	FArray3D &
	dimension( IR const & I1, IR const & I2, IR const & I3, T const & t )
	{
		initializer_ = t;
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	inline
	FArray3D &
	dimension( IR const & I1, IR const & I2, IR const & I3, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray3D &
	dimension( FArray3< U > const & a )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	inline
	FArray3D &
	dimension( FArray3< U > const & a, T const & t )
	{
		initializer_ = t;
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	inline
	FArray3D &
	dimension( FArray3< U > const & a, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Data-Preserving Redimension by IndexRange
	inline
	FArray3D &
	redimension( IR const & I1, IR const & I2, IR const & I3 )
	{
		FArray3D o( I1, I2, I3 );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
				int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
				int const b3( std::max( I3.l(), l3() ) ), e3( std::min( I3.u(), u3() ) );
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i2 = b2; i2 <= e2; ++i2 ) {
						for ( int i1 = b1; i1 <= e1; ++i1 ) {
							o( i1, i2, i3 ) = operator ()( i1, i2, i3 );
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by IndexRange + Fill Value
	inline
	FArray3D &
	redimension( IR const & I1, IR const & I2, IR const & I3, T const & t )
	{
		FArray3D o( I1, I2, I3, t );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
				int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
				int const b3( std::max( I3.l(), l3() ) ), e3( std::min( I3.u(), u3() ) );
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i2 = b2; i2 <= e2; ++i2 ) {
						for ( int i1 = b1; i1 <= e1; ++i1 ) {
							o( i1, i2, i3 ) = operator ()( i1, i2, i3 );
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
	FArray3D &
	redimension( FArray3< U > const & a )
	{
		FArray3D o( a.I1(), a.I2(), a.I3() );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
				int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
				int const b3( std::max( a.l3(), l3() ) ), e3( std::min( a.u3(), u3() ) );
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i2 = b2; i2 <= e2; ++i2 ) {
						for ( int i1 = b1; i1 <= e1; ++i1 ) {
							o( i1, i2, i3 ) = operator ()( i1, i2, i3 );
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
	FArray3D &
	redimension( FArray3< U > const & a, T const & t )
	{
		FArray3D o( a.I1(), a.I2(), a.I3(), t );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
				int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
				int const b3( std::max( a.l3(), l3() ) ), e3( std::min( a.u3(), u3() ) );
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i2 = b2; i2 <= e2; ++i2 ) {
						for ( int i1 = b1; i1 <= e1; ++i1 ) {
							o( i1, i2, i3 ) = operator ()( i1, i2, i3 );
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Set Initializer Value
	inline
	FArray3D &
	initializer( T const & t )
	{
		initializer_ = t;
		return *this;
	}

	// Set Initializer Function
	inline
	FArray3D &
	initializer( InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		return *this;
	}

	// Clear Initializer
	inline
	FArray3D &
	initializer_clear()
	{
		initializer_.clear();
		return *this;
	}

	// Initialize
	inline
	FArray3D &
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
	FArray3D &
	swap( FArray3D & v )
	{
		using std::swap;
		swap3DB( v );
		I1_.swap_no_notify( v.I1_ );
		I2_.swap_no_notify( v.I2_ );
		I3_.swap_no_notify( v.I3_ );
		swap( initializer_, v.initializer_ );
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
	dimension_assign( SIR const & I1, SIR const & I2, SIR const & I3 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
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
		if ( dimensions_initialized() ) {
			shift_set( ( ( ( I3_.lz() * z2_ ) + I2_.lz() ) * z1_ ) + I1_.lz() );
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
		if ( dimensions_initialized() ) {
			resize( size_of( z1_, z2_, I3_.size() ) );
			shift_set( ( ( ( I3_.lz() * z2_ ) + I2_.lz() ) * z1_ ) + I1_.lz() );
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
	}

#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
	// Report size if at least value defined for OBJEXXFCL_FARRAY_SIZE_REPORT
	//  Size is based on sizeof( T ) so T-controlled heap memory is not counted
	inline
	void
	size_report() const
	{
		if ( size_ * sizeof( T ) >= OBJEXXFCL_FARRAY_SIZE_REPORT ) {
			std::cout << "  Index ranges: " << I1_ << ' ' << I2_ << ' ' << I3_ << std::endl;
		}
	}
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT

private: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2
	IR I3_; // Index range of dim 3

	Initializer initializer_; // Array initializer

}; // FArray3D

// Swap
template< typename T >
inline
void
swap( FArray3D< T > & a, FArray3D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental

// Array == Array
template< typename T >
inline
FArray3D< bool >
operator ==( FArray3< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	eq_elemental( a, b, r );
	return r;
}

// Array != Array
template< typename T >
inline
FArray3D< bool >
operator !=( FArray3< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	ne_elemental( a, b, r );
	return r;
}

// Array < Array
template< typename T >
inline
FArray3D< bool >
operator <( FArray3< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	lt_elemental( a, b, r );
	return r;
}

// Array <= Array
template< typename T >
inline
FArray3D< bool >
operator <=( FArray3< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	le_elemental( a, b, r );
	return r;
}

// Array > Array
template< typename T >
inline
FArray3D< bool >
operator >( FArray3< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	gt_elemental( a, b, r );
	return r;
}

// Array >= Array
template< typename T >
inline
FArray3D< bool >
operator >=( FArray3< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	ge_elemental( a, b, r );
	return r;
}

// Array == Value
template< typename T >
inline
FArray3D< bool >
operator ==( FArray3< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	eq_elemental( a, t, r );
	return r;
}

// Array != Value
template< typename T >
inline
FArray3D< bool >
operator !=( FArray3< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	ne_elemental( a, t, r );
	return r;
}

// Array < Value
template< typename T >
inline
FArray3D< bool >
operator <( FArray3< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	lt_elemental( a, t, r );
	return r;
}

// Array <= Value
template< typename T >
inline
FArray3D< bool >
operator <=( FArray3< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	le_elemental( a, t, r );
	return r;
}

// Array > Value
template< typename T >
inline
FArray3D< bool >
operator >( FArray3< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	gt_elemental( a, t, r );
	return r;
}

// Array >= Value
template< typename T >
inline
FArray3D< bool >
operator >=( FArray3< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	ge_elemental( a, t, r );
	return r;
}

// Value == Array
template< typename T >
inline
FArray3D< bool >
operator ==( T const & t, FArray3< T > const & b )
{
	return ( b == t );
}

// Value != Array
template< typename T >
inline
FArray3D< bool >
operator !=( T const & t, FArray3< T > const & b )
{
	return ( b != t );
}

// Value < Array
template< typename T >
inline
FArray3D< bool >
operator <( T const & t, FArray3< T > const & b )
{
	return ( b > t );
}

// Value <= Array
template< typename T >
inline
FArray3D< bool >
operator <=( T const & t, FArray3< T > const & b )
{
	return ( b >= t );
}

// Value > Array
template< typename T >
inline
FArray3D< bool >
operator >( T const & t, FArray3< T > const & b )
{
	return ( b < t );
}

// Value >= Array
template< typename T >
inline
FArray3D< bool >
operator >=( T const & t, FArray3< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: Slice

// Slice == Slice
template< typename T >
inline
FArray3D< bool >
operator ==( FArray3S< T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) == b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// Slice != Slice
template< typename T >
inline
FArray3D< bool >
operator !=( FArray3S< T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) != b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// Slice < Slice
template< typename T >
inline
FArray3D< bool >
operator <( FArray3S< T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) < b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// Slice <= Slice
template< typename T >
inline
FArray3D< bool >
operator <=( FArray3S< T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) <= b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// Slice > Slice
template< typename T >
inline
FArray3D< bool >
operator >( FArray3S< T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) > b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// Slice >= Slice
template< typename T >
inline
FArray3D< bool >
operator >=( FArray3S< T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) >= b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// Slice == Array
template< typename T >
inline
FArray3D< bool >
operator ==( FArray3S< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) == b[ l ] );
			}
		}
	}
	return r;
}

// Slice != Array
template< typename T >
inline
FArray3D< bool >
operator !=( FArray3S< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) != b[ l ] );
			}
		}
	}
	return r;
}

// Slice < Array
template< typename T >
inline
FArray3D< bool >
operator <( FArray3S< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) < b[ l ] );
			}
		}
	}
	return r;
}

// Slice <= Array
template< typename T >
inline
FArray3D< bool >
operator <=( FArray3S< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) <= b[ l ] );
			}
		}
	}
	return r;
}

// Slice > Array
template< typename T >
inline
FArray3D< bool >
operator >( FArray3S< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) > b[ l ] );
			}
		}
	}
	return r;
}

// Slice >= Array
template< typename T >
inline
FArray3D< bool >
operator >=( FArray3S< T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) >= b[ l ] );
			}
		}
	}
	return r;
}

// Array == Slice
template< typename T >
inline
FArray3D< bool >
operator ==( FArray3< T > const & a, FArray3S< T > const & b )
{
	return ( b == a );
}

// Array != Slice
template< typename T >
inline
FArray3D< bool >
operator !=( FArray3< T > const & a, FArray3S< T > const & b )
{
	return ( b != a );
}

// Array < Slice
template< typename T >
inline
FArray3D< bool >
operator <( FArray3< T > const & a, FArray3S< T > const & b )
{
	return ( b > a );
}

// Array <= Slice
template< typename T >
inline
FArray3D< bool >
operator <=( FArray3< T > const & a, FArray3S< T > const & b )
{
	return ( b >= a );
}

// Array > Slice
template< typename T >
inline
FArray3D< bool >
operator >( FArray3< T > const & a, FArray3S< T > const & b )
{
	return ( b < a );
}

// Array >= Slice
template< typename T >
inline
FArray3D< bool >
operator >=( FArray3< T > const & a, FArray3S< T > const & b )
{
	return ( b <= a );
}

// Slice == Value
template< typename T >
inline
FArray3D< bool >
operator ==( FArray3S< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) == t );
			}
		}
	}
	return r;
}

// Slice != Value
template< typename T >
inline
FArray3D< bool >
operator !=( FArray3S< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) != t );
			}
		}
	}
	return r;
}

// Slice < Value
template< typename T >
inline
FArray3D< bool >
operator <( FArray3S< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) < t );
			}
		}
	}
	return r;
}

// Slice <= Value
template< typename T >
inline
FArray3D< bool >
operator <=( FArray3S< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) <= t );
			}
		}
	}
	return r;
}

// Slice > Value
template< typename T >
inline
FArray3D< bool >
operator >( FArray3S< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) > t );
			}
		}
	}
	return r;
}

// Slice >= Value
template< typename T >
inline
FArray3D< bool >
operator >=( FArray3S< T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) >= t );
			}
		}
	}
	return r;
}

// Value == Slice
template< typename T >
inline
FArray3D< bool >
operator ==( T const & t, FArray3S< T > const & b )
{
	return ( b == t );
}

// Value != Slice
template< typename T >
inline
FArray3D< bool >
operator !=( T const & t, FArray3S< T > const & b )
{
	return ( b != t );
}

// Value < Slice
template< typename T >
inline
FArray3D< bool >
operator <( T const & t, FArray3S< T > const & b )
{
	return ( b > t );
}

// Value <= Slice
template< typename T >
inline
FArray3D< bool >
operator <=( T const & t, FArray3S< T > const & b )
{
	return ( b >= t );
}

// Value > Slice
template< typename T >
inline
FArray3D< bool >
operator >( T const & t, FArray3S< T > const & b )
{
	return ( b < t );
}

// Value >= Slice
template< typename T >
inline
FArray3D< bool >
operator >=( T const & t, FArray3S< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: MArray

// MArray == MArray
template< class A, typename T >
inline
FArray3D< bool >
operator ==( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) == b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray != MArray
template< class A, typename T >
inline
FArray3D< bool >
operator !=( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) != b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray < MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) < b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray <= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <=( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) <= b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray > MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) > b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray >= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >=( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) >= b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray == Array
template< class A, typename T >
inline
FArray3D< bool >
operator ==( MArray3< A, T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) == b[ l ] );
			}
		}
	}
	return r;
}

// MArray != Array
template< class A, typename T >
inline
FArray3D< bool >
operator !=( MArray3< A, T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) != b[ l ] );
			}
		}
	}
	return r;
}

// MArray < Array
template< class A, typename T >
inline
FArray3D< bool >
operator <( MArray3< A, T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) < b[ l ] );
			}
		}
	}
	return r;
}

// MArray <= Array
template< class A, typename T >
inline
FArray3D< bool >
operator <=( MArray3< A, T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) <= b[ l ] );
			}
		}
	}
	return r;
}

// MArray > Array
template< class A, typename T >
inline
FArray3D< bool >
operator >( MArray3< A, T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) > b[ l ] );
			}
		}
	}
	return r;
}

// MArray >= Array
template< class A, typename T >
inline
FArray3D< bool >
operator >=( MArray3< A, T > const & a, FArray3< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	typename FArray3< T >::size_type l( 0 );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) >= b[ l ] );
			}
		}
	}
	return r;
}

// Array == MArray
template< class A, typename T >
inline
FArray3D< bool >
operator ==( FArray3< T > const & a, MArray3< A, T > const & b )
{
	return ( b == a );
}

// Array != MArray
template< class A, typename T >
inline
FArray3D< bool >
operator !=( FArray3< T > const & a, MArray3< A, T > const & b )
{
	return ( b != a );
}

// Array < MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <( FArray3< T > const & a, MArray3< A, T > const & b )
{
	return ( b > a );
}

// Array <= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <=( FArray3< T > const & a, MArray3< A, T > const & b )
{
	return ( b >= a );
}

// Array > MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >( FArray3< T > const & a, MArray3< A, T > const & b )
{
	return ( b < a );
}

// Array >= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >=( FArray3< T > const & a, MArray3< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Slice
template< class A, typename T >
inline
FArray3D< bool >
operator ==( MArray3< A, T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) == b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray != Slice
template< class A, typename T >
inline
FArray3D< bool >
operator !=( MArray3< A, T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) != b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray < Slice
template< class A, typename T >
inline
FArray3D< bool >
operator <( MArray3< A, T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) < b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray <= Slice
template< class A, typename T >
inline
FArray3D< bool >
operator <=( MArray3< A, T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) <= b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray > Slice
template< class A, typename T >
inline
FArray3D< bool >
operator >( MArray3< A, T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) > b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// MArray >= Slice
template< class A, typename T >
inline
FArray3D< bool >
operator >=( MArray3< A, T > const & a, FArray3S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) >= b( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

// Slice == MArray
template< class A, typename T >
inline
FArray3D< bool >
operator ==( FArray3S< T > const & a, MArray3< A, T > const & b )
{
	return ( b == a );
}

// Slice != MArray
template< class A, typename T >
inline
FArray3D< bool >
operator !=( FArray3S< T > const & a, MArray3< A, T > const & b )
{
	return ( b != a );
}

// Slice < MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <( FArray3S< T > const & a, MArray3< A, T > const & b )
{
	return ( b > a );
}

// Slice <= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <=( FArray3S< T > const & a, MArray3< A, T > const & b )
{
	return ( b >= a );
}

// Slice > MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >( FArray3S< T > const & a, MArray3< A, T > const & b )
{
	return ( b < a );
}

// Slice >= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >=( FArray3S< T > const & a, MArray3< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Value
template< class A, typename T >
inline
FArray3D< bool >
operator ==( MArray3< A, T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) == t );
			}
		}
	}
	return r;
}

// MArray != Value
template< class A, typename T >
inline
FArray3D< bool >
operator !=( MArray3< A, T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) != t );
			}
		}
	}
	return r;
}

// MArray < Value
template< class A, typename T >
inline
FArray3D< bool >
operator <( MArray3< A, T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) < t );
			}
		}
	}
	return r;
}

// MArray <= Value
template< class A, typename T >
inline
FArray3D< bool >
operator <=( MArray3< A, T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) <= t );
			}
		}
	}
	return r;
}

// MArray > Value
template< class A, typename T >
inline
FArray3D< bool >
operator >( MArray3< A, T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) > t );
			}
		}
	}
	return r;
}

// MArray >= Value
template< class A, typename T >
inline
FArray3D< bool >
operator >=( MArray3< A, T > const & a, T const & t )
{
	FArray3D< bool > r( FArray3D< bool >::shape( a ) );
	for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
				r( i1, i2, i3 ) = ( a( i1, i2, i3 ) >= t );
			}
		}
	}
	return r;
}

// Value == MArray
template< class A, typename T >
inline
FArray3D< bool >
operator ==( T const & t, MArray3< A, T > const & b )
{
	return ( b == t );
}

// Value != MArray
template< class A, typename T >
inline
FArray3D< bool >
operator !=( T const & t, MArray3< A, T > const & b )
{
	return ( b != t );
}

// Value < MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <( T const & t, MArray3< A, T > const & b )
{
	return ( b > t );
}

// Value <= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator <=( T const & t, MArray3< A, T > const & b )
{
	return ( b >= t );
}

// Value > MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >( T const & t, MArray3< A, T > const & b )
{
	return ( b < t );
}

// Value >= MArray
template< class A, typename T >
inline
FArray3D< bool >
operator >=( T const & t, MArray3< A, T > const & b )
{
	return ( b <= t );
}

// Generator

// -Array
template< typename T >
inline
FArray3D< T >
operator -( FArray3< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Array + Array
template< typename T >
inline
FArray3D< T >
operator +( FArray3< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Array
template< typename T >
inline
FArray3D< T >
operator -( FArray3< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Array
template< typename T >
inline
FArray3D< T >
operator *( FArray3< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Array
template< typename T >
inline
FArray3D< T >
operator /( FArray3< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Value
template< typename T >
inline
FArray3D< T >
operator +( FArray3< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Array
template< typename T >
inline
FArray3D< T >
operator +( T const & t, FArray3< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += t;
	return r;
}

// Array - Value
template< typename T >
inline
FArray3D< T >
operator -( FArray3< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Array
template< typename T >
inline
FArray3D< T >
operator -( T const & t, FArray3< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Array * Value
template< typename T >
inline
FArray3D< T >
operator *( FArray3< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Array
template< typename T >
inline
FArray3D< T >
operator *( T const & t, FArray3< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Array / Value
template< typename T >
inline
FArray3D< T >
operator /( FArray3< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Array
template< typename T >
inline
FArray3D< T >
operator /( T const & t, FArray3< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Array && Array
template< typename T >
inline
FArray3D< T >
operator &&( FArray3< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Array || Array
template< typename T >
inline
FArray3D< T >
operator ||( FArray3< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: Slice

// -Slice
template< typename T >
inline
FArray3D< T >
operator -( FArray3S< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Slice + Slice
template< typename T >
inline
FArray3D< T >
operator +( FArray3S< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Slice
template< typename T >
inline
FArray3D< T >
operator -( FArray3S< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Slice
template< typename T >
inline
FArray3D< T >
operator *( FArray3S< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Slice
template< typename T >
inline
FArray3D< T >
operator /( FArray3S< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Array
template< typename T >
inline
FArray3D< T >
operator +( FArray3S< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Array
template< typename T >
inline
FArray3D< T >
operator -( FArray3S< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Array
template< typename T >
inline
FArray3D< T >
operator *( FArray3S< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Array
template< typename T >
inline
FArray3D< T >
operator /( FArray3S< T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Slice
template< typename T >
inline
FArray3D< T >
operator +( FArray3< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Slice
template< typename T >
inline
FArray3D< T >
operator -( FArray3< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Slice
template< typename T >
inline
FArray3D< T >
operator *( FArray3< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Slice
template< typename T >
inline
FArray3D< T >
operator /( FArray3< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Value
template< typename T >
inline
FArray3D< T >
operator +( FArray3S< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Slice
template< typename T >
inline
FArray3D< T >
operator +( T const & t, FArray3S< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += t;
	return r;
}

// Slice - Value
template< typename T >
inline
FArray3D< T >
operator -( FArray3S< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Slice
template< typename T >
inline
FArray3D< T >
operator -( T const & t, FArray3S< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Slice * Value
template< typename T >
inline
FArray3D< T >
operator *( FArray3S< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Slice
template< typename T >
inline
FArray3D< T >
operator *( T const & t, FArray3S< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Slice / Value
template< typename T >
inline
FArray3D< T >
operator /( FArray3S< T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Slice
template< typename T >
inline
FArray3D< T >
operator /( T const & t, FArray3S< T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Slice && Slice
template< typename T >
inline
FArray3D< T >
operator &&( FArray3S< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Slice || Slice
template< typename T >
inline
FArray3D< T >
operator ||( FArray3S< T > const & a, FArray3S< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: MArray

// -MArray
template< class A, typename T >
inline
FArray3D< T >
operator -( MArray3< A, T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// MArray + MArray
template< class A, typename T >
inline
FArray3D< T >
operator +( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - MArray
template< class A, typename T >
inline
FArray3D< T >
operator -( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * MArray
template< class A, typename T >
inline
FArray3D< T >
operator *( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / MArray
template< class A, typename T >
inline
FArray3D< T >
operator /( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Array
template< class A, typename T >
inline
FArray3D< T >
operator +( MArray3< A, T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - Array
template< class A, typename T >
inline
FArray3D< T >
operator -( MArray3< A, T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * Array
template< class A, typename T >
inline
FArray3D< T >
operator *( MArray3< A, T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / Array
template< class A, typename T >
inline
FArray3D< T >
operator /( MArray3< A, T > const & a, FArray3< T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + MArray
template< class A, typename T >
inline
FArray3D< T >
operator +( FArray3< T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - MArray
template< class A, typename T >
inline
FArray3D< T >
operator -( FArray3< T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * MArray
template< class A, typename T >
inline
FArray3D< T >
operator *( FArray3< T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / MArray
template< class A, typename T >
inline
FArray3D< T >
operator /( FArray3< T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Value
template< class A, typename T >
inline
FArray3D< T >
operator +( MArray3< A, T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + MArray
template< class A, typename T >
inline
FArray3D< T >
operator +( T const & t, MArray3< A, T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r += t;
	return r;
}

// MArray - Value
template< class A, typename T >
inline
FArray3D< T >
operator -( MArray3< A, T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - MArray
template< class A, typename T >
inline
FArray3D< T >
operator -( T const & t, MArray3< A, T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// MArray * Value
template< class A, typename T >
inline
FArray3D< T >
operator *( MArray3< A, T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * MArray
template< class A, typename T >
inline
FArray3D< T >
operator *( T const & t, MArray3< A, T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r *= t;
	return r;
}

// MArray / Value
template< class A, typename T >
inline
FArray3D< T >
operator /( MArray3< A, T > const & a, T const & t )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / MArray
template< class A, typename T >
inline
FArray3D< T >
operator /( T const & t, MArray3< A, T > const & a )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// MArray && MArray
template< class A, typename T >
inline
FArray3D< T >
operator &&( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// MArray || MArray
template< class A, typename T >
inline
FArray3D< T >
operator ||( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	FArray3D< T > r( FArray3D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

} // ObjexxFCL

#endif // ObjexxFCL_FArray3D_hh_INCLUDED
