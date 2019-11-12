#ifndef ObjexxFCL_TraitsLD_hh_INCLUDED
#define ObjexxFCL_TraitsLD_hh_INCLUDED

// List-Directed I/O Type Traits
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
//  List-directed formatting is not standardized and varies across compilers

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// List-Directed I/O Type Traits
template< typename T >
struct TraitsLD
{
	using traits_type = T;
	using Size = std::size_t;

	static Size const w = 0; // Field width
	static Size const d = 0; // Fraction width
	static Size const e = 0; // Exponent width
};

// char Specialization
template<>
struct TraitsLD< char >
{
	using traits_type = char;
	using Size = std::size_t;

	static Size const w = 1; // Field width
};

// bool Specialization
template<>
struct TraitsLD< bool >
{
	using traits_type = bool;
	using Size = std::size_t;

	static Size const w = 2; // Field width
};

// byte Specialization
template<>
struct TraitsLD< byte >
{
	using traits_type = byte;
	using Size = std::size_t;

	static Size const w = 5; // Field width
};

// ubyte Specialization
template<>
struct TraitsLD< ubyte >
{
	using traits_type = ubyte;
	using Size = std::size_t;

	static Size const w = 5; // Field width
};

// short int Specialization
template<>
struct TraitsLD< short int >
{
	using traits_type = short int;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// unsigned short int Specialization
template<>
struct TraitsLD< unsigned short int >
{
	using traits_type = unsigned short int;
	using Size = std::size_t;

	static Size const w = 7; // Field width
};

// int Specialization
template<>
struct TraitsLD< int >
{
	using traits_type = int;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// unsigned int Specialization
template<>
struct TraitsLD< unsigned int >
{
	using traits_type = unsigned int;
	using Size = std::size_t;

	static Size const w = 12; // Field width
};

// long int Specialization
template<>
struct TraitsLD< long int >
{
	using traits_type = long int;
	using Size = std::size_t;

	static Size const w = 22; // Field width
};

// unsigned long int Specialization
template<>
struct TraitsLD< unsigned long int >
{
	using traits_type = unsigned long int;
	using Size = std::size_t;

	static Size const w = 22; // Field width
};

// long long int Specialization
template<>
struct TraitsLD< long long int >
{
	using traits_type = long long int;
	using Size = std::size_t;

	static Size const w = 22; // Field width
};

// unsigned long long int Specialization
template<>
struct TraitsLD< unsigned long long int >
{
	using traits_type = unsigned long long int;
	using Size = std::size_t;

	static Size const w = 22; // Field width
};

// float Specialization
template<>
struct TraitsLD< float >
{
	using traits_type = float;
	using Size = std::size_t;

	static Size const w = 15; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// double Specialization
template<>
struct TraitsLD< double >
{
	using traits_type = double;
	using Size = std::size_t;

	static Size const w = 24; // Field width
	static Size const d = 15; // Fraction width
	static Size const e = 3; // Exponent width
};

// long double Specialization
template<>
struct TraitsLD< long double >
{
	using traits_type = long double;
	using Size = std::size_t;

	static Size const w = 43; // Field width
	static Size const d = 33; // Fraction width
	static Size const e = 4; // Exponent width
};

// std::complex< float > Specialization
template<>
struct TraitsLD< std::complex< float > >
{
	using traits_type = std::complex< float >;
	using Size = std::size_t;

	static Size const w = 14; // Field width
	static Size const d = 7; // Fraction width
	static Size const e = 2; // Exponent width
};

// std::complex< double > Specialization
template<>
struct TraitsLD< std::complex< double > >
{
	using traits_type = std::complex< double >;
	using Size = std::size_t;

	static Size const w = 23; // Field width
	static Size const d = 15; // Fraction width
	static Size const e = 3; // Exponent width
};

// std::complex< long double > Specialization
template<>
struct TraitsLD< std::complex< long double > >
{
	using traits_type = std::complex< long double >;
	using Size = std::size_t;

	static Size const w = 42; // Field width
	static Size const d = 33; // Fraction width
	static Size const e = 4; // Exponent width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsLD_hh_INCLUDED
