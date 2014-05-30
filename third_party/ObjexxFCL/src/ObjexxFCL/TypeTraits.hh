#ifndef ObjexxFCL_TypeTraits_hh_INCLUDED
#define ObjexxFCL_TypeTraits_hh_INCLUDED

// TypeTraits: Type Traits Template
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
#include <ObjexxFCL/byte.hh>
#include <ObjexxFCL/ubyte.hh>

// C++ Headers
#include <complex>
#include <cstddef>
#include <ios>
#include <limits>
#include <typeinfo>

namespace ObjexxFCL {

template< class A, class B >
inline
bool
same_type_as( A const & a, B const & b )
{
	return ( typeid( a ) == typeid( b ) );
}

template< class A, class B >
inline
bool
SAME_TYPE_AS( A const & a, B const & b )
{
	return ( typeid( a ) == typeid( b ) );
}

template< class A, class B >
inline
bool
extends_type_of( A const & a, B const & b )
{
#ifdef OBJEXXFCL_FULL_EXTENDS_TYPE_OF // Full support for dynamic type of b
	if ( typeid( a ) == typeid( b ) ) { // Simpler than adding double dispatch to all classes
		return true;
	} else if ( a.super() ) { // Requires virtual super() returning pointer to super class
		return extends_type_of( *a.super(), b ); // Recurse up inheritance hierarchy
	} else {
		return false;
	}
#else // Support for static type of b
	assert( typeid( b ) == typeid( B ) ); // Check that we are safe using simple support
#ifdef NDEBUG
	static_cast< void >( b ); // Suppress unused warning
#endif
	return ( dynamic_cast< B const * >( &a ) != nullptr );
#endif
}

template< class A, class B >
inline
bool
EXTENDS_TYPE_OF( A const & a, B const & b )
{
	return extends_type_of( a, b );
}

// is_a: Type Test for const Reference Argument
template< class B, class A >
inline
bool
is_a( A const & a )
{
	return ( dynamic_cast< B const * >( &a ) != nullptr );
}

// is_a: Type Test for non-const Reference Argument
template< class B, class A >
inline
bool
is_a( A & a )
{
	return ( dynamic_cast< B * >( &a ) != nullptr );
}

// is_a: Type Test for const Pointer Argument
template< class B, class A >
inline
bool
is_a( A const * a )
{
	return ( dynamic_cast< B const * >( a ) != nullptr );
}

// is_a: Type Test for non-const Pointer Argument
template< class B, class A >
inline
bool
is_a( A * a )
{
	return ( dynamic_cast< B * >( a ) != nullptr );
}

// TypeTraits: Type Traits Template
template< typename T >
struct TypeTraits
{
	typedef  T  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return traits_type(); // Use default constructor
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0; // No precision for generic types
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 0; // No minimum width for generic types
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 0; // No minimum width for generic types
	}

}; // TypeTraits

// TypeTraits: Type Traits char Specialization
template<>
struct TypeTraits< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return ' ';
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 1;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 1;
	}

}; // TypeTraits

// TypeTraits: Type Traits signed char Specialization
template<>
struct TypeTraits< signed char >
{
	typedef  signed char  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return ' ';
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 1;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 1;
	}

}; // TypeTraits

// TypeTraits: Type Traits unsigned char Specialization
template<>
struct TypeTraits< unsigned char >
{
	typedef  unsigned char  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return ' ';
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 1;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 1;
	}

}; // TypeTraits

// TypeTraits: Type Traits bool Specialization
template<>
struct TypeTraits< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 2;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 2;
	}

}; // TypeTraits

// TypeTraits: Type Traits byte Specialization
template<>
struct TypeTraits< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 7;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 7;
	}

}; // TypeTraits

// TypeTraits: Type Traits ubyte Specialization
template<>
struct TypeTraits< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 7;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 7;
	}

}; // TypeTraits

// TypeTraits: Type Traits short int Specialization
template<>
struct TypeTraits< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 7;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 7;
	}

}; // TypeTraits

// TypeTraits: Type Traits unsigned short int Specialization
template<>
struct TypeTraits< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 7;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 7;
	}

}; // TypeTraits

// TypeTraits: Type Traits int Specialization
template<>
struct TypeTraits< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 12;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 12;
	}

}; // TypeTraits

// TypeTraits: Type Traits unsigned int Specialization
template<>
struct TypeTraits< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 12;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 12;
	}

}; // TypeTraits

// TypeTraits: Type Traits long int Specialization
template<>
struct TypeTraits< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

}; // TypeTraits

// TypeTraits: Type Traits unsigned long int Specialization
template<>
struct TypeTraits< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

}; // TypeTraits

// TypeTraits: Type Traits long long int Specialization
template<>
struct TypeTraits< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

}; // TypeTraits

// TypeTraits: Type Traits unsigned long long int Specialization
template<>
struct TypeTraits< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 0;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 23; // Big enough for 64-bit LP64 representation
	}

}; // TypeTraits

// TypeTraits: Type Traits float Specialization
template<>
struct TypeTraits< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return ( std::numeric_limits< traits_type >::has_signaling_NaN ? std::numeric_limits< traits_type >::signaling_NaN() : std::numeric_limits< traits_type >::max() );
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 8;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 15;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 15;
	}

}; // TypeTraits

// TypeTraits: Type Traits double Specialization
template<>
struct TypeTraits< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return ( std::numeric_limits< traits_type >::has_signaling_NaN ? std::numeric_limits< traits_type >::signaling_NaN() : std::numeric_limits< traits_type >::max() );
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 16;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 23;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 23;
	}

}; // TypeTraits

// TypeTraits: Type Traits long double Specialization
template<>
struct TypeTraits< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return ( std::numeric_limits< traits_type >::has_signaling_NaN ? std::numeric_limits< traits_type >::signaling_NaN() : std::numeric_limits< traits_type >::max() );
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 33; // Big enough for 128-bit representation
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 42; // Big enough for 128-bit representation
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 42; // Big enough for 128-bit representation
	}

}; // TypeTraits

// TypeTraits: Type Traits std::complex< float > Specialization
template<>
struct TypeTraits< std::complex< float > >
{
	typedef  float  value_type;
	typedef  std::complex< float >  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return traits_type( TypeTraits< value_type >::debug_value(), TypeTraits< value_type >::debug_value() );
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 8;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 33;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 33;
	}

}; // TypeTraits

// TypeTraits: Type Traits std::complex< double > Specialization
template<>
struct TypeTraits< std::complex< double > >
{
	typedef  double  value_type;
	typedef  std::complex< double >  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return traits_type( TypeTraits< value_type >::debug_value(), TypeTraits< value_type >::debug_value() );
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 16;
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 49;
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 49;
	}

}; // TypeTraits

// TypeTraits: Type Traits std::complex< long double > Specialization
template<>
struct TypeTraits< std::complex< long double > >
{
	typedef  long double  value_type;
	typedef  std::complex< long double >  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	inline
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	inline
	static
	traits_type
	debug_value()
	{
		return traits_type( TypeTraits< value_type >::debug_value(), TypeTraits< value_type >::debug_value() );
	}

	// Initial Array Value
	inline
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_FARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_FARRAY_INIT_DEBUG
	}

	// Precision
	inline
	static
	std::streamsize
	precision()
	{
		return 33; // Big enough for 128-bit representation
	}

	// Field Width
	inline
	static
	Size
	width()
	{
		return 83; // Big enough for 128-bit representation
	}

	// Field Width
	inline
	static
	int
	iwidth()
	{
		return 83; // Big enough for 128-bit representation
	}

}; // TypeTraits

} // ObjexxFCL

#endif // ObjexxFCL_TypeTraits_hh_INCLUDED
