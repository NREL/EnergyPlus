#ifndef ObjexxFCL_TraitsE_hh_INCLUDED
#define ObjexxFCL_TraitsE_hh_INCLUDED

// TraitsE: E Format I/O Type Traits
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
//  E formatting without w.d is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// TraitsE
template< typename T >
struct TraitsE
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

}; // TraitsE

// TraitsE: char Specialization
template<>
struct TraitsE< char >
{
	typedef  char  traits_type;
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

}; // TraitsE

// TraitsE: bool Specialization
template<>
struct TraitsE< bool >
{
	typedef  bool  traits_type;
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

}; // TraitsE

// TraitsE: Type Traits byte Specialization
template<>
struct TraitsE< byte >
{
	typedef  byte  traits_type;
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

}; // TraitsE

// TraitsE: ubyte Specialization
template<>
struct TraitsE< ubyte >
{
	typedef  ubyte  traits_type;
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

}; // TraitsE

// TraitsE: short int Specialization
template<>
struct TraitsE< short int >
{
	typedef  short int  traits_type;
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

}; // TraitsE

// TraitsE: unsigned short int Specialization
template<>
struct TraitsE< unsigned short int >
{
	typedef  unsigned short int  traits_type;
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

}; // TraitsE

// TraitsE: int Specialization
template<>
struct TraitsE< int >
{
	typedef  int  traits_type;
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

}; // TraitsE

// TraitsE: unsigned int Specialization
template<>
struct TraitsE< unsigned int >
{
	typedef  unsigned int  traits_type;
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

}; // TraitsE

// TraitsE: long int Specialization
template<>
struct TraitsE< long int >
{
	typedef  long int  traits_type;
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
		return 3;
	}

}; // TraitsE

// TraitsE: unsigned long int Specialization
template<>
struct TraitsE< unsigned long int >
{
	typedef  unsigned long int  traits_type;
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
		return 3;
	}

}; // TraitsE

// TraitsE: long long int Specialization
template<>
struct TraitsE< long long int >
{
	typedef  long long int  traits_type;
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
		return 3;
	}

}; // TraitsE

// TraitsE: unsigned long long int Specialization
template<>
struct TraitsE< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
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
		return 3;
	}

}; // TraitsE

// TraitsE: float Specialization
template<>
struct TraitsE< float >
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

}; // TraitsE

// TraitsE: double Specialization
template<>
struct TraitsE< double >
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

}; // TraitsE

// TraitsE: long double Specialization
template<>
struct TraitsE< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 44;
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

}; // TraitsE

} // ObjexxFCL

#endif // ObjexxFCL_TraitsE_hh_INCLUDED
