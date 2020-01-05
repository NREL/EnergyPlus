#ifndef ObjexxFCL_TraitsI_hh_INCLUDED
#define ObjexxFCL_TraitsI_hh_INCLUDED

// I Format I/O Type Traits
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
//  I formatting without w is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// I Format I/O Type Traits
template< typename T >
struct TraitsI
{
	using traits_type = T;
	using Size = std::size_t;

	static Size const w = 0; // Field width
};

// char Specialization
template<>
struct TraitsI< char >
{
	using traits_type = char;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// bool Specialization
template<>
struct TraitsI< bool >
{
	using traits_type = bool;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// byte Specialization
template<>
struct TraitsI< byte >
{
	using traits_type = byte;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// ubyte Specialization
template<>
struct TraitsI< ubyte >
{
	using traits_type = ubyte;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// short int Specialization
template<>
struct TraitsI< short int >
{
	using traits_type = short int;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// unsigned short int Specialization
template<>
struct TraitsI< unsigned short int >
{
	using traits_type = unsigned short int;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// int Specialization
template<>
struct TraitsI< int >
{
	using traits_type = int;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// unsigned int Specialization
template<>
struct TraitsI< unsigned int >
{
	using traits_type = unsigned int;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// long int Specialization
template<>
struct TraitsI< long int >
{
	using traits_type = long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// unsigned long int Specialization
template<>
struct TraitsI< unsigned long int >
{
	using traits_type = unsigned long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// long long int Specialization
template<>
struct TraitsI< long long int >
{
	using traits_type = long long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// unsigned long long int Specialization
template<>
struct TraitsI< unsigned long long int >
{
	using traits_type = unsigned long long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// float Specialization
template<>
struct TraitsI< float >
{
	using traits_type = float;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// double Specialization
template<>
struct TraitsI< double >
{
	using traits_type = double;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// long double Specialization
template<>
struct TraitsI< long double >
{
	using traits_type = long double;
	using Size = std::size_t;

	static Size const w = 40; // Field width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsI_hh_INCLUDED
