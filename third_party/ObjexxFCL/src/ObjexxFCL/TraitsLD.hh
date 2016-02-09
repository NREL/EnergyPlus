#ifndef ObjexxFCL_TraitsLD_hh_INCLUDED
#define ObjexxFCL_TraitsLD_hh_INCLUDED

// TraitsLD: List-Directed I/O Type Traits
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

	static Size const w = 0; // Field width
	static Size const d = 0; // Fraction width
	static Size const e = 0; // Exponent width
};

// TraitsLD: char Specialization
template<>
struct TraitsLD< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 1; // Field width
};

// TraitsLD: bool Specialization
template<>
struct TraitsLD< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 2; // Field width
};

// TraitsLD: byte Specialization
template<>
struct TraitsLD< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 5; // Field width
};

// TraitsLD: ubyte Specialization
template<>
struct TraitsLD< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 5; // Field width
};

// TraitsLD: short int Specialization
template<>
struct TraitsLD< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 7; // Field width
};

// TraitsLD: unsigned short int Specialization
template<>
struct TraitsLD< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 7; // Field width
};

// TraitsLD: int Specialization
template<>
struct TraitsLD< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 12; // Field width
};

// TraitsLD: unsigned int Specialization
template<>
struct TraitsLD< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 12; // Field width
};

// TraitsLD: long int Specialization
template<>
struct TraitsLD< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 22; // Field width
};

// TraitsLD: unsigned long int Specialization
template<>
struct TraitsLD< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 22; // Field width
};

// TraitsLD: long long int Specialization
template<>
struct TraitsLD< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 22; // Field width
};

// TraitsLD: unsigned long long int Specialization
template<>
struct TraitsLD< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 22; // Field width
};

// TraitsLD: float Specialization
template<>
struct TraitsLD< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsLD: double Specialization
template<>
struct TraitsLD< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 24; // Field width
	static Size const d = 15; // Fraction width
	static Size const e = 3; // Exponent width
};

// TraitsLD: long double Specialization
template<>
struct TraitsLD< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 43; // Field width
	static Size const d = 33; // Fraction width
	static Size const e = 4; // Exponent width
};

// TraitsLD: std::complex< float > Specialization
template<>
struct TraitsLD< std::complex< float > >
{
	typedef  std::complex< float >  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 14; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// TraitsLD: std::complex< double > Specialization
template<>
struct TraitsLD< std::complex< double > >
{
	typedef  std::complex< double >  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 23; // Field width
	static Size const d = 15; // Fraction width
	static Size const e = 3; // Exponent width
};

// TraitsLD: std::complex< long double > Specialization
template<>
struct TraitsLD< std::complex< long double > >
{
	typedef  std::complex< long double >  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 42; // Field width
	static Size const d = 33; // Fraction width
	static Size const e = 4; // Exponent width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsLD_hh_INCLUDED
