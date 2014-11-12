#ifndef ObjexxFCL_TraitsLD_hh_INCLUDED
#define ObjexxFCL_TraitsLD_hh_INCLUDED

// TraitsLD: List-Directed I/O Type Traits
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

// Notes:
//  Intel Fortran default values used in this version
//  List-directed formatting is not standardized and varies across compilers

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// TraitsLD
template< typename T >
struct TraitsLD
{
	typedef  T  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 0; // No minimum width for generic types
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 0; // No minimum width for generic types
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 0; // No minimum width for generic types
	}

}; // TraitsLD

// TraitsLD: char Specialization
template<>
struct TraitsLD< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 1;
	}

}; // TraitsLD

// TraitsLD: bool Specialization
template<>
struct TraitsLD< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 2;
	}

}; // TraitsLD

// TraitsLD: byte Specialization
template<>
struct TraitsLD< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 5;
	}

}; // TraitsLD

// TraitsLD: ubyte Specialization
template<>
struct TraitsLD< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 5;
	}

}; // TraitsLD

// TraitsLD: short int Specialization
template<>
struct TraitsLD< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 7;
	}

}; // TraitsLD

// TraitsLD: unsigned short int Specialization
template<>
struct TraitsLD< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 7;
	}

}; // TraitsLD

// TraitsLD: int Specialization
template<>
struct TraitsLD< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 12;
	}

}; // TraitsLD

// TraitsLD: unsigned int Specialization
template<>
struct TraitsLD< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 12;
	}

}; // TraitsLD

// TraitsLD: long int Specialization
template<>
struct TraitsLD< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 22;
	}

}; // TraitsLD

// TraitsLD: unsigned long int Specialization
template<>
struct TraitsLD< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 22;
	}

}; // TraitsLD

// TraitsLD: long long int Specialization
template<>
struct TraitsLD< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 22;
	}

}; // TraitsLD

// TraitsLD: unsigned long long int Specialization
template<>
struct TraitsLD< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 22;
	}

}; // TraitsLD

// TraitsLD: float Specialization
template<>
struct TraitsLD< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 15;
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 7;
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 2;
	}

}; // TraitsLD

// TraitsLD: double Specialization
template<>
struct TraitsLD< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 24;
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 15;
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 3;
	}

}; // TraitsLD

// TraitsLD: long double Specialization
template<>
struct TraitsLD< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 43;
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 33;
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 4;
	}

}; // TraitsLD

// TraitsLD: std::complex< float > Specialization
template<>
struct TraitsLD< std::complex< float > >
{
	typedef  std::complex< float >  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 14;
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 7;
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 2;
	}

}; // TraitsLD

// TraitsLD: std::complex< double > Specialization
template<>
struct TraitsLD< std::complex< double > >
{
	typedef  std::complex< double >  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 23;
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 15;
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 3;
	}

}; // TraitsLD

// TraitsLD: std::complex< long double > Specialization
template<>
struct TraitsLD< std::complex< long double > >
{
	typedef  std::complex< long double >  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 42;
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 33;
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 4;
	}

}; // TraitsLD

} // ObjexxFCL

#endif // ObjexxFCL_TraitsLD_hh_INCLUDED
