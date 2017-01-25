#ifndef ObjexxFCL_TypeTraits_hh_INCLUDED
#define ObjexxFCL_TypeTraits_hh_INCLUDED

// TypeTraits: Type Traits Template
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
#include <ObjexxFCL/byte.hh>
#include <ObjexxFCL/ubyte.hh>

// C++ Headers
#include <complex>
#include <cstddef>
#include <ios>
#include <limits>
#include <type_traits>

namespace ObjexxFCL {

template< class A, class B >
inline
bool
same_type_as( A const &, B const & )
{
	return std::is_same< A, B >::value;
}

template< class A, class B >
inline
bool
SAME_TYPE_AS( A const &, B const & )
{
	return std::is_same< A, B >::value;
}

template< class A, class B >
inline
bool
extends_type_of( A const &, B const & )
{
	return std::is_same< A, B >::value || std::is_base_of< B, A >::value;
}

template< class A, class B >
inline
bool
EXTENDS_TYPE_OF( A const &, B const & )
{
	return std::is_same< A, B >::value || std::is_base_of< B, A >::value;
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
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return traits_type(); // Use default constructor
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 0; // Field width
	static int const iwidth = 0; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits char Specialization
template<>
struct TypeTraits< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return ' ';
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 1; // Field width
	static int const iwidth = 1; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits signed char Specialization
template<>
struct TypeTraits< signed char >
{
	typedef  signed char  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return ' ';
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 1; // Field width
	static int const iwidth = 1; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits unsigned char Specialization
template<>
struct TypeTraits< unsigned char >
{
	typedef  unsigned char  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return ' ';
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 1; // Field width
	static int const iwidth = 1; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits bool Specialization
template<>
struct TypeTraits< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 2; // Field width
	static int const iwidth = 2; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits byte Specialization
template<>
struct TypeTraits< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits ubyte Specialization
template<>
struct TypeTraits< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits short int Specialization
template<>
struct TypeTraits< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 275u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits unsigned short int Specialization
template<>
struct TypeTraits< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 275u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits int Specialization
template<>
struct TypeTraits< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 12; // Field width
	static int const iwidth = 12; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 11u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits unsigned int Specialization
template<>
struct TypeTraits< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 12; // Field width
	static int const iwidth = 12; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 11u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits long int Specialization
template<>
struct TypeTraits< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 170u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits unsigned long int Specialization
template<>
struct TypeTraits< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 170u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits long long int Specialization
template<>
struct TypeTraits< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 145u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits unsigned long long int Specialization
template<>
struct TypeTraits< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return std::numeric_limits< traits_type >::max();
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 145u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits float Specialization
template<>
struct TypeTraits< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return ( std::numeric_limits< traits_type >::has_signaling_NaN ? std::numeric_limits< traits_type >::signaling_NaN() : std::numeric_limits< traits_type >::max() );
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 8; // Precision
	static Size const width = 15; // Field width
	static int const iwidth = 15; // Field width
	static Size const loc_2_crossover = 200u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits double Specialization
template<>
struct TypeTraits< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return ( std::numeric_limits< traits_type >::has_signaling_NaN ? std::numeric_limits< traits_type >::signaling_NaN() : std::numeric_limits< traits_type >::max() );
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 16; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits long double Specialization
template<>
struct TypeTraits< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return ( std::numeric_limits< traits_type >::has_signaling_NaN ? std::numeric_limits< traits_type >::signaling_NaN() : std::numeric_limits< traits_type >::max() );
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 33; // Precision
	static Size const width = 42; // Field width
	static int const iwidth = 42; // Field width
	static Size const loc_2_crossover = ( sizeof( traits_type ) >= 16 ? 125u : 150u ); // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = ( sizeof( traits_type ) >= 16 ? 600u : 10u ); // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = ( sizeof( traits_type ) >= 16 ? 400u : 10u ); // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits std::complex< float > Specialization
template<>
struct TypeTraits< std::complex< float > >
{
	typedef  float  value_type;
	typedef  std::complex< float >  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return traits_type( TypeTraits< value_type >::debug_value(), TypeTraits< value_type >::debug_value() );
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 8; // Precision
	static Size const width = 33; // Field width
	static int const iwidth = 33; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits std::complex< double > Specialization
template<>
struct TypeTraits< std::complex< double > >
{
	typedef  double  value_type;
	typedef  std::complex< double >  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return traits_type( TypeTraits< value_type >::debug_value(), TypeTraits< value_type >::debug_value() );
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 16; // Precision
	static Size const width = 49; // Field width
	static int const iwidth = 49; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// TypeTraits: Type Traits std::complex< long double > Specialization
template<>
struct TypeTraits< std::complex< long double > >
{
	typedef  long double  value_type;
	typedef  std::complex< long double >  traits_type;
	typedef  std::size_t  Size;

	// Initial Value
	static
	traits_type
	initial_value()
	{
		return traits_type(); // Use default constructor
	}

	// Debug Value
	static
	traits_type
	debug_value()
	{
		return traits_type( TypeTraits< value_type >::debug_value(), TypeTraits< value_type >::debug_value() );
	}

	// Initial Array Value
	static
	traits_type
	initial_array_value()
	{
#ifdef OBJEXXFCL_ARRAY_INIT_DEBUG
		return debug_value();
#else
		return initial_value();
#endif // OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	static std::streamsize const precision = 33; // Precision
	static Size const width = 83; // Field width
	static int const iwidth = 83; // Field width
	static Size const loc_2_crossover = 100u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

} // ObjexxFCL

#endif // ObjexxFCL_TypeTraits_hh_INCLUDED
