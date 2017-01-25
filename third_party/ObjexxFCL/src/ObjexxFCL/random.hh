#ifndef ObjexxFCL_random_hh_INCLUDED
#define ObjexxFCL_random_hh_INCLUDED

// Random Number Functions
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

// ObjexxFCL Headers
#include <ObjexxFCL/Array1.hh>
#include <ObjexxFCL/Optional.hh>

// C++ Headers
#include <cstdint>

namespace ObjexxFCL {

// Forward
template< typename > class Array;

// Random int on [0,1]
std::int32_t
IRANDM();

// Random int on [0,1]
std::int32_t
IRANDM( int const iflag );

// Random float on [0,1]
float
RANDOM( int const iflag );

// Random double on [0,1]
double
DRANDM( int const iflag );

// Random float on [0,1]
inline
std::int32_t
IRAND( Optional< int const > iflag = _ )
{
	return IRANDM( iflag.present() ? iflag() : 0 );
}

// Random float on [0,1]
inline
float
RAND( Optional< int const > iflag = _ )
{
	return RANDOM( iflag.present() ? iflag() : 0 );
}

// Random double on [0,1]
inline
double
DRAND( Optional< int const > iflag = _ )
{
	return DRANDM( iflag.present() ? iflag() : 0 );
}

// Random float on [0,1]
inline
void
RANDOM_NUMBER( float & harvest )
{
	harvest = RANDOM( 0 );
}

// Random double on [0,1]
inline
void
RANDOM_NUMBER( double & harvest )
{
	harvest = DRANDM( 0 );
}

// Array of Random float on [0,1]
void
RANDOM_NUMBER( Array< float > & harvest );

// Array of Random double on [0,1]
void
RANDOM_NUMBER( Array< double > & harvest );

// Random Seed Set
void
SRAND( int const iseed );

// Random Seed Interface
void
RANDOM_SEED( Optional< int > size = _, Optional< Array1< int > const > put = _, Optional< Array1< int > > get = _ );

} // ObjexxFCL

#endif // ObjexxFCL_random_hh_INCLUDED
