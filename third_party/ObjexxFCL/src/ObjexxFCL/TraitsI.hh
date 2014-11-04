#ifndef ObjexxFCL_TraitsI_hh_INCLUDED
#define ObjexxFCL_TraitsI_hh_INCLUDED

// TraitsI: I Format I/O Type Traits
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
//  I formatting without w is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// TraitsI
template< typename T >
struct TraitsI
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

	// Minimum Width
	inline
	static
	Size
	m()
	{
		return 0;
	}

}; // TraitsI

// TraitsI: char Specialization
template<>
struct TraitsI< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 7;
	}

}; // TraitsI

// TraitsI: bool Specialization
template<>
struct TraitsI< bool >
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

}; // TraitsI

// TraitsI: Type Traits byte Specialization
template<>
struct TraitsI< byte >
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

}; // TraitsI

// TraitsI: ubyte Specialization
template<>
struct TraitsI< ubyte >
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

}; // TraitsI

// TraitsI: short int Specialization
template<>
struct TraitsI< short int >
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

}; // TraitsI

// TraitsI: unsigned short int Specialization
template<>
struct TraitsI< unsigned short int >
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

}; // TraitsI

// TraitsI: int Specialization
template<>
struct TraitsI< int >
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

}; // TraitsI

// TraitsI: unsigned int Specialization
template<>
struct TraitsI< unsigned int >
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

}; // TraitsI

// TraitsI: long int Specialization
template<>
struct TraitsI< long int >
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

}; // TraitsI

// TraitsI: unsigned long int Specialization
template<>
struct TraitsI< unsigned long int >
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

}; // TraitsI

// TraitsI: long long int Specialization
template<>
struct TraitsI< long long int >
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

}; // TraitsI

// TraitsI: unsigned long long int Specialization
template<>
struct TraitsI< unsigned long long int >
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

}; // TraitsI

// TraitsI: float Specialization
template<>
struct TraitsI< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 12;
	}

}; // TraitsI

// TraitsI: double Specialization
template<>
struct TraitsI< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 23;
	}

}; // TraitsI

// TraitsI: long double Specialization
template<>
struct TraitsI< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	// Field Width
	inline
	static
	Size
	w()
	{
		return 40;
	}

}; // TraitsI

} // ObjexxFCL

#endif // ObjexxFCL_TraitsI_hh_INCLUDED
