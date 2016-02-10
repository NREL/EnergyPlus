#ifndef ObjexxFCL_TraitsA_hh_INCLUDED
#define ObjexxFCL_TraitsA_hh_INCLUDED

// TraitsA: A Format I/O Type Traits
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

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// TraitsA
template< typename T >
struct TraitsA
{
	typedef  T  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 0; // No minimum width for generic types

}; // TraitsA

// TraitsA: char Specialization
template<>
struct TraitsA< char >
{
	typedef  char  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 1;

}; // TraitsA

// TraitsA: bool Specialization
template<>
struct TraitsA< bool >
{
	typedef  bool  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 4;

}; // TraitsA

// TraitsA: Type Traits byte Specialization
template<>
struct TraitsA< byte >
{
	typedef  byte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 1;

}; // TraitsA

// TraitsA: ubyte Specialization
template<>
struct TraitsA< ubyte >
{
	typedef  ubyte  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 1;

}; // TraitsA

// TraitsA: short int Specialization
template<>
struct TraitsA< short int >
{
	typedef  short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 2;

}; // TraitsA

// TraitsA: unsigned short int Specialization
template<>
struct TraitsA< unsigned short int >
{
	typedef  unsigned short int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 2;

}; // TraitsA

// TraitsA: int Specialization
template<>
struct TraitsA< int >
{
	typedef  int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 4;

}; // TraitsA

// TraitsA: unsigned int Specialization
template<>
struct TraitsA< unsigned int >
{
	typedef  unsigned int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 4;

}; // TraitsA

// TraitsA: long int Specialization
template<>
struct TraitsA< long int >
{
	typedef  long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 8;

}; // TraitsA

// TraitsA: unsigned long int Specialization
template<>
struct TraitsA< unsigned long int >
{
	typedef  unsigned long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 8;

}; // TraitsA

// TraitsA: long long int Specialization
template<>
struct TraitsA< long long int >
{
	typedef  long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 8;

}; // TraitsA

// TraitsA: unsigned long long int Specialization
template<>
struct TraitsA< unsigned long long int >
{
	typedef  unsigned long long int  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 8;

}; // TraitsA

// TraitsA: float Specialization
template<>
struct TraitsA< float >
{
	typedef  float  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 4;

}; // TraitsA

// TraitsA: double Specialization
template<>
struct TraitsA< double >
{
	typedef  double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 8;

}; // TraitsA

// TraitsA: long double Specialization
template<>
struct TraitsA< long double >
{
	typedef  long double  traits_type;
	typedef  std::size_t  Size;

	static Size const w = 16;

}; // TraitsA

} // ObjexxFCL

#endif // ObjexxFCL_TraitsA_hh_INCLUDED
