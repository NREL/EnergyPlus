#ifndef ObjexxFCL_TraitsG_hh_INCLUDED
#define ObjexxFCL_TraitsG_hh_INCLUDED

// G Format I/O Type Traits
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

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

// G Format I/O Type Traits
template< typename T >
struct TraitsG
{
	using traits_type = T;
	using Size = std::size_t;

	static Size const w = 0; // Field width
	static Size const d = 0; // Fraction width
	static Size const e = 0; // Exponent width
};

// char Specialization
template<>
struct TraitsG< char >
{
	using traits_type = char;
	using Size = std::size_t;

	static Size const w = 1; // Field width
};

// bool Specialization
template<>
struct TraitsG< bool >
{
	using traits_type = bool;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// byte Specialization
template<>
struct TraitsG< byte >
{
	using traits_type = byte;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// ubyte Specialization
template<>
struct TraitsG< ubyte >
{
	using traits_type = ubyte;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// short int Specialization
template<>
struct TraitsG< short int >
{
	using traits_type = short int;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// unsigned short int Specialization
template<>
struct TraitsG< unsigned short int >
{
	using traits_type = unsigned short int;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// int Specialization
template<>
struct TraitsG< int >
{
	using traits_type = int;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// unsigned int Specialization
template<>
struct TraitsG< unsigned int >
{
	using traits_type = unsigned int;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// long int Specialization
template<>
struct TraitsG< long int >
{
	using traits_type = long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// unsigned long int Specialization
template<>
struct TraitsG< unsigned long int >
{
	using traits_type = unsigned long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// long long int Specialization
template<>
struct TraitsG< long long int >
{
	using traits_type = long long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// unsigned long long int Specialization
template<>
struct TraitsG< unsigned long long int >
{
	using traits_type = unsigned long long int;
	using Size = std::size_t;

	static Size const w = 23; // Field width
};

// float Specialization
template<>
struct TraitsG< float >
{
	using traits_type = float;
	using Size = std::size_t;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// double Specialization
template<>
struct TraitsG< double >
{
	using traits_type = double;
	using Size = std::size_t;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
	static Size const e = 2; // Exponent width
};

// long double Specialization
template<>
struct TraitsG< long double >
{
	using traits_type = long double;
	using Size = std::size_t;

	static Size const w = 42; // Field width
	static Size const d = 33; // Fraction width
	static Size const e = 3; // Exponent width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsG_hh_INCLUDED
