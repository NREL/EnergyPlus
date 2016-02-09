#ifndef ObjexxFCL_Reference_hh_INCLUDED
#define ObjexxFCL_Reference_hh_INCLUDED

// Smart Reference Emulating Fortran POINTER
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
#include <ObjexxFCL/Reference.fwd.hh>
#include <ObjexxFCL/IndexRange.hh>

// C++ Headers
#include <cassert>
#include <cstddef>
#include <type_traits>

namespace ObjexxFCL {

// Smart Reference Emulating Fortran POINTER
template< typename T >
class Reference
{

public: // Types

	typedef  T  Value; // Type: Includes const attribute for const argument
	typedef  IndexRange  IR;
	typedef  typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type  Tc;
	typedef  typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type  Tr;

public: // Creation

	// Default Constructor
	Reference() :
	 ptr_( nullptr ),
	 own_( false )
	{}

	// Copy Constructor
	Reference( Reference const & ref ) :
	 ptr_( ref.ptr_ ),
	 own_( false )
	{}

	// Null Pointer Constructor
	Reference( std::nullptr_t ) :
	 ptr_( nullptr ),
	 own_( false )
	{}

	// Value Constructor
	Reference( T const & val ) :
	 ptr_( const_cast< T * >( &val ) ), // Fortran compilers allow modifying INTENT(IN) args via POINTERs
	 own_( false )
	{}

	// Destructor
	~Reference()
	{}

public: // Assignment

	// Copy Assignment
	Reference &
	operator =( Reference const & ref )
	{
		ptr_ = ref.ptr_;
		own_ = false;
		return *this;
	}

	// Value Assignment
	Reference &
	operator =( T const & val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

	// Value Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Reference &
	operator =( U const & val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

public: // Conversion

	// Value Conversion
	operator Tr() const
	{
		assert( ptr_ != nullptr );
		return *ptr_;
	}

	// Value Conversion
	operator T &()
	{
		assert( ptr_ != nullptr );
		return *ptr_;
	}

public: // Operators

	// Value
	Tr
	operator ()() const
	{
		assert( ptr_ != nullptr );
		return *ptr_;
	}

	// Value
	T &
	operator ()()
	{
		assert( ptr_ != nullptr );
		return *ptr_;
	}

	// Attach to Value
	void
	operator >>=( T const & val )
	{
		ptr_ = const_cast< T * >( &val ); // Fortran compilers allow modifying INTENT(IN) args via POINTERs
		own_ = false;
	}

	// Attach to Reference
	void
	operator >>=( Reference & ref )
	{
		ptr_ = ref.ptr_;
		own_ = false;
	}

	// Attach to Null Pointer
	void
	operator >>=( std::nullptr_t )
	{
		ptr_ = nullptr;
		own_ = false;
	}

public: // Properties

	// Associated?
	bool
	associated() const
	{
		return ( ptr_ != nullptr );
	}

	// Associated with a Given Value?
	bool
	associated( T const & val ) const
	{
		return ( ptr_ == &val );
	}

	// Associated with the same Target as Another Reference?
	bool
	associated( Reference const & ref ) const
	{
		return ( ptr_ == ref.ptr_ );
	}

	// Attached?
	bool
	attached() const
	{
		return ( ptr_ != nullptr );
	}

	// Attached to a Given Value?
	bool
	attached( T const & val ) const
	{
		return ( ptr_ == &val );
	}

public: // Modifiers

	// Attach to Value
	void
	attach( T const & val )
	{
		ptr_ = const_cast< T * >( &val ); // Fortran compilers allow modifying INTENT(IN) args via POINTERs
		own_ = false;
	}

	// Detach
	void
	detach()
	{
		ptr_ = nullptr;
		own_ = false;
	}

	// Nullify
	void
	nullify()
	{
		ptr_ = nullptr;
		own_ = false;
	}

	// Clear
	void
	clear()
	{
		ptr_ = nullptr;
		own_ = false;
	}

	void
	allocate()
	{
		ptr_ = new T();
		own_ = true;
	}

	void
	allocate( IR const & I )
	{
		ptr_ = new T( I );
		own_ = true;
	}

	void
	allocate( IR const & I1, IR const & I2 )
	{
		ptr_ = new T( I1, I2 );
		own_ = true;
	}

	void
	allocate( IR const & I1, IR const & I2, IR const & I3 )
	{
		ptr_ = new T( I1, I2, I3 );
		own_ = true;
	}

	void
	allocate( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		ptr_ = new T( I1, I2, I3, I4 );
		own_ = true;
	}

	void
	allocate( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		ptr_ = new T( I1, I2, I3, I4, I5 );
		own_ = true;
	}

	void
	allocate( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 )
	{
		ptr_ = new T( I1, I2, I3, I4, I5, I6 );
		own_ = true;
	}

	void
	deallocate()
	{
		if ( ptr_ && own_ ) delete ptr_;
		ptr_ = nullptr;
		own_ = false;
	}

public: // Comparison

	// Reference == Reference
	friend
	bool
	operator ==( Reference const & a, Reference const & b )
	{
		assert( ( a.ptr_ != nullptr ) && ( b.ptr_ != nullptr ) ); // Fortran disallows use if not associated
		return ( ( a.ptr_ != nullptr ) && ( b.ptr_ != nullptr ) && ( *a.ptr_ == *b.ptr_ ) );
	}

	// Reference != Reference
	friend
	bool
	operator !=( Reference const & a, Reference const & b )
	{
		assert( ( a.ptr_ != nullptr ) && ( b.ptr_ != nullptr ) ); // Fortran disallows use if not associated
		return ( ( a.ptr_ == nullptr ) || ( b.ptr_ == nullptr ) || ( *a.ptr_ != *b.ptr_ ) );
	}

	// Reference == Value
	friend
	bool
	operator ==( Reference const & a, Tc b )
	{
		assert( a.ptr_ != nullptr ); // Fortran disallows use if not associated
		return ( ( a.ptr_ != nullptr ) && ( *a.ptr_ == b ) );
	}

	// Reference != Value
	friend
	bool
	operator !=( Reference const & a, Tc b )
	{
		assert( a.ptr_ != nullptr ); // Fortran disallows use if not associated
		return ( ( a.ptr_ == nullptr ) || ( *a.ptr_ != b ) );
	}

	// Value == Reference
	friend
	bool
	operator ==( Tc a, Reference const & b )
	{
		assert( b.ptr_ != nullptr ); // Fortran disallows use if not associated
		return ( ( b.ptr_ != nullptr ) && ( a == *b.ptr_ ) );
	}

	// Value != Reference
	friend
	bool
	operator !=( Tc a, Reference const & b )
	{
		assert( b.ptr_ != nullptr ); // Fortran disallows use if not associated
		return ( ( b.ptr_ == nullptr ) || ( a != *b.ptr_ ) );
	}

private: // Data

	T * ptr_; // Pointer to object
	bool own_; // Allocated the object?

}; // Reference

// Associated?
template< typename T >
inline
bool
associated( Reference< T > const & r )
{
	return r.associated();
}

// Associated with a Given Value?
template< typename T >
inline
bool
associated( Reference< T > const & r, T const & val )
{
	return r.associated( val );
}

// Associated with a the Same Value?
template< typename T >
inline
bool
associated( Reference< T > const & r1, Reference< T > const & r2 )
{
	return r1.associated( r2 );
}

// Associated?
template< typename T >
inline
bool
ASSOCIATED( Reference< T > const & r )
{
	return r.associated();
}

// Associated with a Given Value?
template< typename T >
inline
bool
ASSOCIATED( Reference< T > const & r, T const & val )
{
	return r.associated( val );
}

// Associated with a the Same Value?
template< typename T >
inline
bool
ASSOCIATED( Reference< T > const & r1, Reference< T > const & r2 )
{
	return r1.associated( r2 );
}

// Nullify
template< typename T >
inline
void
nullify( Reference< T > & r )
{
	r.nullify();
}

// Nullify
template< typename T >
inline
void
NULLIFY( Reference< T > & r )
{
	r.nullify();
}

// Null Pointer
inline
Reference< void * >
null_ptr()
{
	return Reference< void * >();
}

// Null Pointer
inline
Reference< void * >
NULL_PTR()
{
	return Reference< void * >();
}

// Null Pointer of a Reference Type
template< typename T >
inline
Reference< T >
null_ptr( Reference< T > const & )
{
	return Reference< T >();
}

// Null Pointer of a Reference Type
template< typename T >
inline
Reference< T >
NULL_PTR( Reference< T > const & )
{
	return Reference< T >();
}

template< typename T >
inline
void
allocate( Reference< T > & r )
{
	r.allocate();
}

template< typename T >
inline
void
allocate( Reference< T > & r, IndexRange const & I )
{
	r.allocate( I );
}

template< typename T >
inline
void
allocate( Reference< T > & r, IndexRange const & I1, IndexRange const & I2 )
{
	r.allocate( I1, I2 );
}

template< typename T >
inline
void
allocate( Reference< T > & r, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3 )
{
	r.allocate( I1, I2, I3 );
}

template< typename T >
inline
void
allocate( Reference< T > & r, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3, IndexRange const & I4 )
{
	r.allocate( I1, I2, I3, I4 );
}

template< typename T >
inline
void
allocate( Reference< T > & r, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3, IndexRange const & I4, IndexRange const & I5 )
{
	r.allocate( I1, I2, I3, I4, I5 );
}

template< typename T >
inline
void
allocate( Reference< T > & r, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3, IndexRange const & I4, IndexRange const & I5, IndexRange const & I6 )
{
	r.allocate( I1, I2, I3, I4, I5, I6 );
}

template< typename T >
inline
void
deallocate( Reference< T > & r )
{
	r.deallocate();
}

} // ObjexxFCL

#endif // ObjexxFCL_Reference_hh_INCLUDED
