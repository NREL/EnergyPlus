#ifndef ObjexxFCL_TraitsG_hh_INCLUDED
#define ObjexxFCL_TraitsG_hh_INCLUDED

// TraitsG: G Format I/O Type Traits
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
//  G formatting without w.d is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// TraitsG
template< typename T >
struct TraitsG
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

}; // TraitsG

// TraitsG: char Specialization
template<>
struct TraitsG< char >
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

}; // TraitsG

// TraitsG: bool Specialization
template<>
struct TraitsG< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 12;
	}

}; // TraitsG

// TraitsG: Type Traits byte Specialization
template<>
struct TraitsG< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 7;
	}

}; // TraitsG

// TraitsG: ubyte Specialization
template<>
struct TraitsG< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 7;
	}

}; // TraitsG

// TraitsG: short int Specialization
template<>
struct TraitsG< short int >
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

}; // TraitsG

// TraitsG: unsigned short int Specialization
template<>
struct TraitsG< unsigned short int >
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

}; // TraitsG

// TraitsG: int Specialization
template<>
struct TraitsG< int >
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

}; // TraitsG

// TraitsG: unsigned int Specialization
template<>
struct TraitsG< unsigned int >
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

}; // TraitsG

// TraitsG: long int Specialization
template<>
struct TraitsG< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 23;
	}

}; // TraitsG

// TraitsG: unsigned long int Specialization
template<>
struct TraitsG< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 23;
	}

}; // TraitsG

// TraitsG: long long int Specialization
template<>
struct TraitsG< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 23;
	}

}; // TraitsG

// TraitsG: unsigned long long int Specialization
template<>
struct TraitsG< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 23;
	}

}; // TraitsG

// TraitsG: float Specialization
template<>
struct TraitsG< float >
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

}; // TraitsG

// TraitsG: double Specialization
template<>
struct TraitsG< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 25;
	}

	// Fraction Width
	inline
	static
	Size
	d()
	{
		return 16;
	}

	// Exponent Width
	inline
	static
	Size
	e()
	{
		return 2;
	}

}; // TraitsG

// TraitsG: long double Specialization
template<>
struct TraitsG< long double >
{
	typedef  long double  traits_type;
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
		return 3;
	}

}; // TraitsG

} // ObjexxFCL

#endif // ObjexxFCL_TraitsG_hh_INCLUDED
