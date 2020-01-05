#ifndef ObjexxFCL_TraitsA_hh_INCLUDED
#define ObjexxFCL_TraitsA_hh_INCLUDED

// A Format I/O Type Traits
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2020 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// Notes:
//  Intel Fortran default values used in this version

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// A Format I/O Type Traits
template< typename T >
struct TraitsA
{
	using traits_type = T;
	using Size = std::size_t;

	static Size const w = 0; // No minimum width for generic types

}; // TraitsA

// char Specialization
template<>
struct TraitsA< char >
{
	using traits_type = char;
	using Size = std::size_t;

	static Size const w = 1;

}; // TraitsA

// bool Specialization
template<>
struct TraitsA< bool >
{
	using traits_type = bool;
	using Size = std::size_t;

	static Size const w = 4;

}; // TraitsA

// byte Specialization
template<>
struct TraitsA< byte >
{
	using traits_type = byte;
	using Size = std::size_t;

	static Size const w = 1;

}; // TraitsA

// ubyte Specialization
template<>
struct TraitsA< ubyte >
{
	using traits_type = ubyte;
	using Size = std::size_t;

	static Size const w = 1;

}; // TraitsA

// short int Specialization
template<>
struct TraitsA< short int >
{
	using traits_type = short int;
	using Size = std::size_t;

	static Size const w = 2;

}; // TraitsA

// unsigned short int Specialization
template<>
struct TraitsA< unsigned short int >
{
	using traits_type = unsigned short int;
	using Size = std::size_t;

	static Size const w = 2;

}; // TraitsA

// int Specialization
template<>
struct TraitsA< int >
{
	using traits_type = int;
	using Size = std::size_t;

	static Size const w = 4;

}; // TraitsA

// unsigned int Specialization
template<>
struct TraitsA< unsigned int >
{
	using traits_type = unsigned int;
	using Size = std::size_t;

	static Size const w = 4;

}; // TraitsA

// long int Specialization
template<>
struct TraitsA< long int >
{
	using traits_type = long int;
	using Size = std::size_t;

	static Size const w = 8;

}; // TraitsA

// unsigned long int Specialization
template<>
struct TraitsA< unsigned long int >
{
	using traits_type = unsigned long int;
	using Size = std::size_t;

	static Size const w = 8;

}; // TraitsA

// long long int Specialization
template<>
struct TraitsA< long long int >
{
	using traits_type = long long int;
	using Size = std::size_t;

	static Size const w = 8;

}; // TraitsA

// unsigned long long int Specialization
template<>
struct TraitsA< unsigned long long int >
{
	using traits_type = unsigned long long int;
	using Size = std::size_t;

	static Size const w = 8;

}; // TraitsA

// float Specialization
template<>
struct TraitsA< float >
{
	using traits_type = float;
	using Size = std::size_t;

	static Size const w = 4;

}; // TraitsA

// double Specialization
template<>
struct TraitsA< double >
{
	using traits_type = double;
	using Size = std::size_t;

	static Size const w = 8;

}; // TraitsA

// long double Specialization
template<>
struct TraitsA< long double >
{
	using traits_type = long double;
	using Size = std::size_t;

	static Size const w = 16;

}; // TraitsA

} // ObjexxFCL

#endif // ObjexxFCL_TraitsA_hh_INCLUDED
