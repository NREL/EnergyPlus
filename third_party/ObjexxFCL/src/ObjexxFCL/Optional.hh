#ifndef ObjexxFCL_Optional_hh_INCLUDED
#define ObjexxFCL_Optional_hh_INCLUDED

// Optional Argument Wrapper
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
#include <ObjexxFCL/Optional.fwd.hh>
#include <ObjexxFCL/Omit.hh>

// C++ Headers
#include <cassert>
#include <type_traits>

namespace ObjexxFCL {

// Optional Argument Wrapper
template< typename T, typename Enable >
class Optional
{

private: // Friend

	template< typename, typename > friend class Optional;

public: // Types

	typedef  T  Value;
	typedef  typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type  Tc;
	typedef  typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type  Tr;

public: // Creation

	// Default Constructor
	Optional() :
	 ptr_( nullptr ),
	 own_( false )
	{}

	// Copy Constructor
	Optional( Optional const & o ) :
	 ptr_( o.own_ ? new T( o() ) : o.ptr_ ),
	 own_( o.own_ )
	{}

	// Optional Constructor Template
	template< typename U, class = typename std::enable_if< std::is_const< T >::value && std::is_same< U, typename std::remove_const< T >::type >::value >::type >
	Optional( Optional< U, Enable > const & o ) :
	 ptr_( o.own_ ? new T( o() ) : o.ptr_ ),
	 own_( o.own_ )
	{}

	// Value Constructor
	Optional( T const & val ) :
	 ptr_( const_cast< T * >( &val ) ),
	 own_( false )
	{}

	// Value Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Optional( U const & val ) :
	 ptr_( new T( val ) ), // Requires Value( U ) constructor
	 own_( true )
	{}

	// rvalue Constructor
	Optional( T && val ) :
	 ptr_( new T( val ) ), // Requires Value copy constructor
	 own_( true )
	{}

	// Omit Constructor
	Optional( Omit ) :
	 ptr_( nullptr ),
	 own_( false )
	{}

	// Destructor
	~Optional()
	{
		if ( own_ ) delete ptr_;
	}

public: // Assignment

	// Copy Assignment
	Optional &
	operator =( Optional const & o )
	{
		if ( this != &o ) {
			if ( own_ ) delete ptr_;
			ptr_ = o.own_ ? new T( o() ) : o.ptr_;
			own_ = o.own_;
		}
		return *this;
	}

	// Value Assignment
	Optional &
	operator =( T const & val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

	// Value Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Optional &
	operator =( U const & val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

	// rvalue Assignment
	Optional &
	operator =( T && val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

	// Omit Assignment
	Optional &
	operator =( Omit )
	{
		if ( own_ ) delete ptr_;
		ptr_ = nullptr;
		own_ = false;
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

public: // Properties

	// Present?
	bool
	present() const
	{
		return ( ptr_ != nullptr );
	}

	// Own?
	bool
	own() const
	{
		return own_;
	}

public: // Modifiers

	// Clear
	void
	clear()
	{
		if ( own_ ) delete ptr_;
		ptr_ = nullptr;
		own_ = false;
	}

public: // Comparison

	// Optional == Optional
	friend
	bool
	operator ==( Optional const & a, Optional const & b )
	{
		return ( ( a.ptr_ != nullptr ) && ( b.ptr_ != nullptr ) ? ( *a.ptr_ == *b.ptr_ ) : ( a.ptr_ == b.ptr_ ) );
	}

	// Optional != Optional
	friend
	bool
	operator !=( Optional const & a, Optional const & b )
	{
		return !( a == b );
	}

	// Optional == Value
	friend
	bool
	operator ==( Optional const & a, Tc b )
	{
		return ( ( a.ptr_ != nullptr ) && ( *a.ptr_ == b ) );
	}

	// Optional != Value
	friend
	bool
	operator !=( Optional const & a, Tc b )
	{
		return !( a == b );
	}

	// Value == Optional
	friend
	bool
	operator ==( Tc a, Optional const & b )
	{
		return ( ( b.ptr_ != nullptr ) && ( a == *b.ptr_ ) );
	}

	// Value != Optional
	friend
	bool
	operator !=( Tc a, Optional const & b )
	{
		return !( a == b );
	}

private: // Data

	T * ptr_; // Pointer to object
	bool own_; // Own the object?

}; // Optional

// Optional Argument Wrapper: Abstract Type Specialization
template< typename T >
class Optional< T, typename std::enable_if< std::is_abstract< T >::value >::type >
{

private: // Friend

	template< typename, typename > friend class Optional;

public: // Types

	typedef  T  Value;
	typedef  typename std::enable_if< std::is_abstract< T >::value >::type  EnableType;
	typedef  typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type  Tc;
	typedef  typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type  Tr;

public: // Creation

	// Default Constructor
	Optional() :
	 ptr_( nullptr )
	{}

	// Copy Constructor
	Optional( Optional const & o ) :
	 ptr_( o.ptr_ )
	{}

	// Optional Constructor Template
	template< typename U, class = typename std::enable_if< std::is_const< T >::value && std::is_same< U, typename std::remove_const< T >::type >::value >::type >
	Optional( Optional< U, EnableType > const & o ) :
	 ptr_( o.ptr_ )
	{}

	// Value Constructor
	Optional( T const & val ) :
	 ptr_( const_cast< T * >( &val ) )
	{}

	// Omit Constructor
	Optional( Omit ) :
	 ptr_( nullptr )
	{}

	// Destructor
	~Optional()
	{}

public: // Assignment

	// Copy Assignment
	Optional &
	operator =( Optional const & o )
	{
		ptr_ = o.ptr_;
		return *this;
	}

	// Value Assignment
	Optional &
	operator =( T const & val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

	// Value Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Optional &
	operator =( U const & val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

	// rvalue Assignment
	Optional &
	operator =( T && val )
	{
		assert( ptr_ != nullptr );
		*ptr_ = val;
		return *this;
	}

	// Omit Assignment
	Optional &
	operator =( Omit )
	{
		ptr_ = nullptr;
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

public: // Properties

	// Present?
	bool
	present() const
	{
		return ( ptr_ != nullptr );
	}

public: // Modifiers

	// Clear
	void
	clear()
	{
		ptr_ = nullptr;
	}

public: // Comparison

	// Optional == Optional
	friend
	bool
	operator ==( Optional const & a, Optional const & b )
	{
		return ( ( a.ptr_ != nullptr ) && ( b.ptr_ != nullptr ) ? ( *a.ptr_ == *b.ptr_ ) : ( a.ptr_ == b.ptr_ ) );
	}

	// Optional != Optional
	friend
	bool
	operator !=( Optional const & a, Optional const & b )
	{
		return !( a == b );
	}

	// Optional == Value
	friend
	bool
	operator ==( Optional const & a, Tc b )
	{
		return ( ( a.ptr_ != nullptr ) && ( *a.ptr_ == b ) );
	}

	// Optional != Value
	friend
	bool
	operator !=( Optional const & a, Tc b )
	{
		return !( a == b );
	}

	// Value == Optional
	friend
	bool
	operator ==( Tc a, Optional const & b )
	{
		return ( ( b.ptr_ != nullptr ) && ( a == *b.ptr_ ) );
	}

	// Value != Optional
	friend
	bool
	operator !=( Tc a, Optional const & b )
	{
		return !( a == b );
	}

private: // Data

	T * ptr_; // Pointer

}; // Optional

// Argument Present?
template< typename T >
inline
bool
present( Optional< T > const & o )
{
	return o.present();
}

// Argument Present?
template< typename T >
inline
bool
PRESENT( Optional< T > const & o )
{
	return o.present();
}

} // ObjexxFCL

#endif // ObjexxFCL_Optional_hh_INCLUDED
