#ifndef ObjexxFCL_TraitsG_hh_INCLUDED
#define ObjexxFCL_TraitsG_hh_INCLUDED

// TraitsG: G Format I/O Type Traits
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

	static Size const w = 0; // Field width
	static Size const d = 0; // Fraction width
	static Size const e = 0; // Exponent width
};

// TraitsG: char Specialization
template<>
struct TraitsG< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 1; // Field width
};

// TraitsG: bool Specialization
template<>
struct TraitsG< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 12; // Field width
};

// TraitsG: Type Traits byte Specialization
template<>
struct TraitsG< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 7; // Field width
};

// TraitsG: ubyte Specialization
template<>
struct TraitsG< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 7; // Field width
};

// TraitsG: short int Specialization
template<>
struct TraitsG< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 7; // Field width
};

// TraitsG: unsigned short int Specialization
template<>
struct TraitsG< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 7; // Field width
};

// TraitsG: int Specialization
template<>
struct TraitsG< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 12; // Field width
};

// TraitsG: unsigned int Specialization
template<>
struct TraitsG< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 12; // Field width
};

// TraitsG: long int Specialization
template<>
struct TraitsG< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 23; // Field width
};

// TraitsG: unsigned long int Specialization
template<>
struct TraitsG< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 23; // Field width
};

// TraitsG: long long int Specialization
template<>
struct TraitsG< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 23; // Field width
};

// TraitsG: unsigned long long int Specialization
template<>
struct TraitsG< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 23; // Field width
};

// TraitsG: float Specialization
template<>
struct TraitsG< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsG: double Specialization
template<>
struct TraitsG< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsG: long double Specialization
template<>
struct TraitsG< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 42; // Field width
	static Size const d = 33; // Fraction width
	static Size const e = 3; // Exponent width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsG_hh_INCLUDED
