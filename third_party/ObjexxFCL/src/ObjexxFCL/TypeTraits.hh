#ifndef ObjexxFCL_TypeTraits_hh_INCLUDED
#define ObjexxFCL_TypeTraits_hh_INCLUDED

// Type Traits Template
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2020 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

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
is_a( A const & )
{
	return std::is_same< A, B >::value || std::is_base_of< B, A >::value;
}

// is_a: Type Test for non-const Reference Argument
template< class B, class A >
inline
bool
is_a( A & )
{
	return std::is_same< A, B >::value || std::is_base_of< B, A >::value;
}

// is_a: Type Test for const Pointer Argument
template< class B, class A >
inline
bool
is_a( A const * )
{
	return std::is_same< A, B >::value || std::is_base_of< B, A >::value;
}

// is_a: Type Test for non-const Pointer Argument
template< class B, class A >
inline
bool
is_a( A * )
{
	return std::is_same< A, B >::value || std::is_base_of< B, A >::value;
}

// Type Traits Template
template< typename T >
struct TypeTraits
{
	using traits_type = T;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 0; // Field width
	static int const iwidth = 0; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// char Specialization
template<>
struct TypeTraits< char >
{
	using traits_type = char;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 1; // Field width
	static int const iwidth = 1; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// signed char Specialization
template<>
struct TypeTraits< signed char >
{
	using traits_type = signed char;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 1; // Field width
	static int const iwidth = 1; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// unsigned char Specialization
template<>
struct TypeTraits< unsigned char >
{
	using traits_type = unsigned char;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 1; // Field width
	static int const iwidth = 1; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// bool Specialization
template<>
struct TypeTraits< bool >
{
	using traits_type = bool;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 2; // Field width
	static int const iwidth = 2; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// byte Specialization
template<>
struct TypeTraits< byte >
{
	using traits_type = byte;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// ubyte Specialization
template<>
struct TypeTraits< ubyte >
{
	using traits_type = ubyte;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 300u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// short int Specialization
template<>
struct TypeTraits< short int >
{
	using traits_type = short int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 275u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// unsigned short int Specialization
template<>
struct TypeTraits< unsigned short int >
{
	using traits_type = unsigned short int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 7; // Field width
	static int const iwidth = 7; // Field width
	static Size const loc_2_crossover = 275u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 20u; // Array 2D x 2D multiplication crossover
};

// int Specialization
template<>
struct TypeTraits< int >
{
	using traits_type = int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 12; // Field width
	static int const iwidth = 12; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 11u; // Array 2D x 2D multiplication crossover
};

// unsigned int Specialization
template<>
struct TypeTraits< unsigned int >
{
	using traits_type = unsigned int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 12; // Field width
	static int const iwidth = 12; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 11u; // Array 2D x 2D multiplication crossover
};

// long int Specialization
template<>
struct TypeTraits< long int >
{
	using traits_type = long int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 170u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// unsigned long int Specialization
template<>
struct TypeTraits< unsigned long int >
{
	using traits_type = unsigned long int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 170u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// long long int Specialization
template<>
struct TypeTraits< long long int >
{
	using traits_type = long long int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 145u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// unsigned long long int Specialization
template<>
struct TypeTraits< unsigned long long int >
{
	using traits_type = unsigned long long int;
	using Size = std::size_t;

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
#endif
	}

	static std::streamsize const precision = 0; // Precision
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 145u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// float Specialization
template<>
struct TypeTraits< float >
{
	using traits_type = float;
	using Size = std::size_t;

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
#endif
	}

#ifdef OBJEXXFCL_TYPETRAITS_EXTRA_PRECISION
	static std::streamsize const precision = 9; // Precision
#else
	static std::streamsize const precision = 8; // Precision
#endif
	static Size const width = 15; // Field width
	static int const iwidth = 15; // Field width
	static Size const loc_2_crossover = 200u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// double Specialization
template<>
struct TypeTraits< double >
{
	using traits_type = double;
	using Size = std::size_t;

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
#endif
	}

#ifdef OBJEXXFCL_TYPETRAITS_EXTRA_PRECISION
	static std::streamsize const precision = 17; // Precision
#else
	static std::streamsize const precision = 16; // Precision
#endif
	static Size const width = 23; // Field width
	static int const iwidth = 23; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// long double Specialization
template<>
struct TypeTraits< long double >
{
	using traits_type = long double;
	using Size = std::size_t;

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
#endif
	}

#ifdef OBJEXXFCL_TYPETRAITS_EXTRA_PRECISION
	static std::streamsize const precision = 34; // Precision
#else
	static std::streamsize const precision = 33; // Precision
#endif
	static Size const width = 42; // Field width
	static int const iwidth = 42; // Field width
	static Size const loc_2_crossover = ( sizeof( traits_type ) >= 16 ? 125u : 150u ); // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = ( sizeof( traits_type ) >= 16 ? 600u : 10u ); // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = ( sizeof( traits_type ) >= 16 ? 400u : 10u ); // Array 2D x 2D multiplication crossover
};

// std::complex< float > Specialization
template<>
struct TypeTraits< std::complex< float > >
{
	using value_type = float;
	using traits_type = std::complex< float >;
	using Size = std::size_t;

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
#endif
	}

#ifdef OBJEXXFCL_TYPETRAITS_EXTRA_PRECISION
	static std::streamsize const precision = 9; // Precision
#else
	static std::streamsize const precision = 8; // Precision
#endif
	static Size const width = 33; // Field width
	static int const iwidth = 33; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// std::complex< double > Specialization
template<>
struct TypeTraits< std::complex< double > >
{
	using value_type = double;
	using traits_type = std::complex< double >;
	using Size = std::size_t;

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
#endif
	}

#ifdef OBJEXXFCL_TYPETRAITS_EXTRA_PRECISION
	static std::streamsize const precision = 17; // Precision
#else
	static std::streamsize const precision = 16; // Precision
#endif
	static Size const width = 49; // Field width
	static int const iwidth = 49; // Field width
	static Size const loc_2_crossover = 150u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

// std::complex< long double > Specialization
template<>
struct TypeTraits< std::complex< long double > >
{
	using value_type = long double;
	using traits_type = std::complex< long double >;
	using Size = std::size_t;

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
#endif
	}

#ifdef OBJEXXFCL_TYPETRAITS_EXTRA_PRECISION
	static std::streamsize const precision = 34; // Precision
#else
	static std::streamsize const precision = 33; // Precision
#endif
	static Size const width = 83; // Field width
	static int const iwidth = 83; // Field width
	static Size const loc_2_crossover = 100u; // Array 2D min/max location crossover
	static Size const matmul_1_2_crossover = 10u; // Array 1D x 2D multiplication crossover
	static Size const matmul_2_2_crossover = 10u; // Array 2D x 2D multiplication crossover
};

} // ObjexxFCL

#endif // ObjexxFCL_TypeTraits_hh_INCLUDED
