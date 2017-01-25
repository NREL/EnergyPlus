#ifndef ObjexxFCL_TraitsF_hh_INCLUDED
#define ObjexxFCL_TraitsF_hh_INCLUDED

// TraitsF: F Format I/O Type Traits
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
//  F formatting without w.d is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// TraitsF
template< typename T >
struct TraitsF
{
	typedef  T  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 0; // Field width
	static Size const d = 0; // Fraction width
};

// TraitsF: char Specialization
template<>
struct TraitsF< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 1; // Field width
};

// TraitsF: bool Specialization
template<>
struct TraitsF< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
};

// TraitsF: Type Traits byte Specialization
template<>
struct TraitsF< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
};

// TraitsF: ubyte Specialization
template<>
struct TraitsF< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
};

// TraitsF: short int Specialization
template<>
struct TraitsF< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
};

// TraitsF: unsigned short int Specialization
template<>
struct TraitsF< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
};

// TraitsF: int Specialization
template<>
struct TraitsF< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
};

// TraitsF: unsigned int Specialization
template<>
struct TraitsF< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
};

// TraitsF: long int Specialization
template<>
struct TraitsF< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
};

// TraitsF: unsigned long int Specialization
template<>
struct TraitsF< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
};

// TraitsF: long long int Specialization
template<>
struct TraitsF< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
};

// TraitsF: unsigned long long int Specialization
template<>
struct TraitsF< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
};

// TraitsF: float Specialization
template<>
struct TraitsF< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
};

// TraitsF: double Specialization
template<>
struct TraitsF< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
};

// TraitsF: long double Specialization
template<>
struct TraitsF< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 42; // Field width
	static Size const d = 33; // Fraction width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsF_hh_INCLUDED
