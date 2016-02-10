#ifndef ObjexxFCL_TraitsB_hh_INCLUDED
#define ObjexxFCL_TraitsB_hh_INCLUDED

// TraitsB: B Format I/O Type Traits
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
//  B formatting without w is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// TraitsB
template< typename T >
struct TraitsB
{
	typedef  T  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 0; // Field width
};

// TraitsB: char Specialization
template<>
struct TraitsB< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 17; // Field width
};

// TraitsB: bool Specialization
template<>
struct TraitsB< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 33; // Field width
};

// TraitsB: Type Traits byte Specialization
template<>
struct TraitsB< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 17; // Field width
};

// TraitsB: ubyte Specialization
template<>
struct TraitsB< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 17; // Field width
};

// TraitsB: short int Specialization
template<>
struct TraitsB< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 17; // Field width
};

// TraitsB: unsigned short int Specialization
template<>
struct TraitsB< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 17; // Field width
};

// TraitsB: int Specialization
template<>
struct TraitsB< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 33; // Field width
};

// TraitsB: unsigned int Specialization
template<>
struct TraitsB< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 33; // Field width
};

// TraitsB: long int Specialization
template<>
struct TraitsB< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 65; // Field width
};

// TraitsB: unsigned long int Specialization
template<>
struct TraitsB< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 65; // Field width
};

// TraitsB: long long int Specialization
template<>
struct TraitsB< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 65; // Field width
};

// TraitsB: unsigned long long int Specialization
template<>
struct TraitsB< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 65; // Field width
};

// TraitsB: float Specialization
template<>
struct TraitsB< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 33; // Field width
};

// TraitsB: double Specialization
template<>
struct TraitsB< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 65; // Field width
};

// TraitsB: long double Specialization
template<>
struct TraitsB< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 129; // Field width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsB_hh_INCLUDED
