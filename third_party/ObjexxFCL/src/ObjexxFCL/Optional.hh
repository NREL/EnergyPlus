#ifndef ObjexxFCL_Optional_hh_INCLUDED
#define ObjexxFCL_Optional_hh_INCLUDED

// Optional Argument Wrapper
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.fwd.hh>
#include <ObjexxFCL/noexcept.hh>
#include <ObjexxFCL/Omit.hh>

// C++ Headers
#include <cassert>
#include <type_traits>

namespace ObjexxFCL {

// Optional Argument Wrapper
template< typename T, typename Enable >
class Optional;

// Optional Argument Wrapper: Concrete Type Specialization
template< typename T >
class Optional< T, typename std::enable_if< ! std::is_abstract< T >::value >::type >
{

private: // Friend

	template< typename, typename > friend class Optional;

public: // Types

	using Value = T;
	using EnableType = typename std::enable_if< ! std::is_abstract< T >::value >::type;
	using Tc = typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type;
	using Tr = typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type;

public: // Creation

	// Default Constructor
	Optional() = default;

	// Copy Constructor
	Optional( Optional const & o ) :
	 ptr_( o.own_ ? new T( o() ) : o.ptr_ ),
	 own_( o.own_ )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_const< T >::value && std::is_same< U, typename std::remove_const< T >::type >::value >::type >
	Optional( Optional< U, EnableType > const & o ) :
	 ptr_( o.own_ ? new T( o() ) : o.ptr_ ),
	 own_( o.own_ )
	{}

	// Move Constructor
	Optional( Optional && ) NOEXCEPT = default;

	// Value Constructor
	Optional( T const & val ) :
	 ptr_( const_cast< T * >( &val ) )
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
	Optional( Omit )
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

	// Move Assignment
	Optional &
	operator =( Optional && ) NOEXCEPT = default;

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
		*ptr_ = T( val );
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

public: // Property

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

public: // Modifier

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

	T * ptr_{ nullptr }; // Pointer to object
	bool own_{ false }; // Own the object?

}; // Optional

// Optional Argument Wrapper: Abstract Type Specialization
template< typename T >
class Optional< T, typename std::enable_if< std::is_abstract< T >::value >::type >
{

private: // Friend

	template< typename, typename > friend class Optional;

public: // Types

	using Value = T;
	using EnableType = typename std::enable_if< std::is_abstract< T >::value >::type;
	using Tc = typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type;
	using Tr = typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type;

public: // Creation

	// Default Constructor
	Optional() :
	 ptr_( nullptr )
	{}

	// Copy Constructor
	Optional( Optional const & o ) :
	 ptr_( o.ptr_ )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_const< T >::value && std::is_same< U, typename std::remove_const< T >::type >::value >::type >
	Optional( Optional< U, EnableType > const & o ) :
	 ptr_( o.ptr_ )
	{}

	// Value Constructor
	Optional( T const & val ) :
	 ptr_( const_cast< T * >( &val ) )
	{}

	// Omit Constructor
	Optional( Omit )
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

public: // Property

	// Present?
	bool
	present() const
	{
		return ( ptr_ != nullptr );
	}

public: // Modifier

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

	T * ptr_{ nullptr }; // Pointer

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

// Optional Maker
template< typename T >
inline
Optional< T >
make_Optional( T const & val )
{
	return Optional< T >( val );
}

} // ObjexxFCL

#endif // ObjexxFCL_Optional_hh_INCLUDED
