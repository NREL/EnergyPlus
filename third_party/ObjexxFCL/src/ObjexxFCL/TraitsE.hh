#ifndef ObjexxFCL_TraitsE_hh_INCLUDED
#define ObjexxFCL_TraitsE_hh_INCLUDED

// TraitsE: E Format I/O Type Traits
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

	static Size const w = 0; // Field width
	static Size const d = 0; // Fraction width
	static Size const e = 0; // Exponent width
};

// TraitsE: char Specialization
template<>
struct TraitsE< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: bool Specialization
template<>
struct TraitsE< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: Type Traits byte Specialization
template<>
struct TraitsE< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: ubyte Specialization
template<>
struct TraitsE< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: short int Specialization
template<>
struct TraitsE< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: unsigned short int Specialization
template<>
struct TraitsE< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: int Specialization
template<>
struct TraitsE< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: unsigned int Specialization
template<>
struct TraitsE< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: long int Specialization
template<>
struct TraitsE< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
	static Size const e = 3; // Exponent width
};

// TraitsE: unsigned long int Specialization
template<>
struct TraitsE< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
	static Size const e = 3; // Exponent width
};

// TraitsE: long long int Specialization
template<>
struct TraitsE< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
	static Size const e = 3; // Exponent width
};

// TraitsE: unsigned long long int Specialization
template<>
struct TraitsE< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
	static Size const e = 3; // Exponent width
};

// TraitsE: float Specialization
template<>
struct TraitsE< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: double Specialization
template<>
struct TraitsE< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsE: long double Specialization
template<>
struct TraitsE< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 44; // Field width
	static Size const d = 33; // Fraction width
	static Size const e = 3; // Exponent width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsE_hh_INCLUDED
