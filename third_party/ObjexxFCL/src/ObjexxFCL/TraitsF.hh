#ifndef ObjexxFCL_TraitsF_hh_INCLUDED
#define ObjexxFCL_TraitsF_hh_INCLUDED

// F Format I/O Type Traits
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
//  F formatting without w.d is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// F Format I/O Type Traits
template< typename T >
struct TraitsF
{
	using traits_type = T;
	using Size = std::size_t;

	static Size const w = 0; // Field width
	static Size const d = 0; // Fraction width
};

// char Specialization
template<>
struct TraitsF< char >
{
	using traits_type = char;
	using Size = std::size_t;

	static Size const w = 1; // Field width
};

// bool Specialization
template<>
struct TraitsF< bool >
{
	using traits_type = bool;
	using Size = std::size_t;

	static Size const w = 15; // Field width
};

// byte Specialization
template<>
struct TraitsF< byte >
{
	using traits_type = byte;
	using Size = std::size_t;

	static Size const w = 15; // Field width
};

// ubyte Specialization
template<>
struct TraitsF< ubyte >
{
	using traits_type = ubyte;
	using Size = std::size_t;

	static Size const w = 15; // Field width
};

// short int Specialization
template<>
struct TraitsF< short int >
{
	using traits_type = short int;
	using Size = std::size_t;

	static Size const w = 15; // Field width
};

// unsigned short int Specialization
template<>
struct TraitsF< unsigned short int >
{
	using traits_type = unsigned short int;
	using Size = std::size_t;

	static Size const w = 15; // Field width
};

// int Specialization
template<>
struct TraitsF< int >
{
	using traits_type = int;
	using Size = std::size_t;

	static Size const w = 15; // Field width
};

// unsigned int Specialization
template<>
struct TraitsF< unsigned int >
{
	using traits_type = unsigned int;
	using Size = std::size_t;

	static Size const w = 15; // Field width
};

// long int Specialization
template<>
struct TraitsF< long int >
{
	using traits_type = long int;
	using Size = std::size_t;

	static Size const w = 25; // Field width
};

// unsigned long int Specialization
template<>
struct TraitsF< unsigned long int >
{
	using traits_type = unsigned long int;
	using Size = std::size_t;

	static Size const w = 25; // Field width
};

// long long int Specialization
template<>
struct TraitsF< long long int >
{
	using traits_type = long long int;
	using Size = std::size_t;

	static Size const w = 25; // Field width
};

// unsigned long long int Specialization
template<>
struct TraitsF< unsigned long long int >
{
	using traits_type = unsigned long long int;
	using Size = std::size_t;

	static Size const w = 25; // Field width
};

// float Specialization
template<>
struct TraitsF< float >
{
	using traits_type = float;
	using Size = std::size_t;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
};

// double Specialization
template<>
struct TraitsF< double >
{
	using traits_type = double;
	using Size = std::size_t;

	static Size const w = 25; // Field width
	static Size const d = 16; // Fraction width
};

// long double Specialization
template<>
struct TraitsF< long double >
{
	using traits_type = long double;
	using Size = std::size_t;

	static Size const w = 42; // Field width
	static Size const d = 33; // Fraction width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsF_hh_INCLUDED
