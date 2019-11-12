#ifndef ObjexxFCL_TraitsB_hh_INCLUDED
#define ObjexxFCL_TraitsB_hh_INCLUDED

// B Format I/O Type Traits
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
//  B formatting without w is not standard-compliant

// C++ Headers
#include <complex>
#include <cstddef>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

// B Format I/O Type Traits
template< typename T >
struct TraitsB
{
	using traits_type = T;
	using Size = std::size_t;

	static Size const w = 0; // Field width
};

// char Specialization
template<>
struct TraitsB< char >
{
	using traits_type = char;
	using Size = std::size_t;

	static Size const w = 17; // Field width
};

// bool Specialization
template<>
struct TraitsB< bool >
{
	using traits_type = bool;
	using Size = std::size_t;

	static Size const w = 33; // Field width
};

// byte Specialization
template<>
struct TraitsB< byte >
{
	using traits_type = byte;
	using Size = std::size_t;

	static Size const w = 17; // Field width
};

// ubyte Specialization
template<>
struct TraitsB< ubyte >
{
	using traits_type = ubyte;
	using Size = std::size_t;

	static Size const w = 17; // Field width
};

// short int Specialization
template<>
struct TraitsB< short int >
{
	using traits_type = short int;
	using Size = std::size_t;

	static Size const w = 17; // Field width
};

// unsigned short int Specialization
template<>
struct TraitsB< unsigned short int >
{
	using traits_type = unsigned short int;
	using Size = std::size_t;

	static Size const w = 17; // Field width
};

// int Specialization
template<>
struct TraitsB< int >
{
	using traits_type = int;
	using Size = std::size_t;

	static Size const w = 33; // Field width
};

// unsigned int Specialization
template<>
struct TraitsB< unsigned int >
{
	using traits_type = unsigned int;
	using Size = std::size_t;

	static Size const w = 33; // Field width
};

// long int Specialization
template<>
struct TraitsB< long int >
{
	using traits_type = long int;
	using Size = std::size_t;

	static Size const w = 65; // Field width
};

// unsigned long int Specialization
template<>
struct TraitsB< unsigned long int >
{
	using traits_type = unsigned long int;
	using Size = std::size_t;

	static Size const w = 65; // Field width
};

// long long int Specialization
template<>
struct TraitsB< long long int >
{
	using traits_type = long long int;
	using Size = std::size_t;

	static Size const w = 65; // Field width
};

// unsigned long long int Specialization
template<>
struct TraitsB< unsigned long long int >
{
	using traits_type = unsigned long long int;
	using Size = std::size_t;

	static Size const w = 65; // Field width
};

// float Specialization
template<>
struct TraitsB< float >
{
	using traits_type = float;
	using Size = std::size_t;

	static Size const w = 33; // Field width
};

// double Specialization
template<>
struct TraitsB< double >
{
	using traits_type = double;
	using Size = std::size_t;

	static Size const w = 65; // Field width
};

// long double Specialization
template<>
struct TraitsB< long double >
{
	using traits_type = long double;
	using Size = std::size_t;

	static Size const w = 129; // Field width
};

} // ObjexxFCL

#endif // ObjexxFCL_TraitsB_hh_INCLUDED
