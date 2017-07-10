#ifndef ObjexxFCL_random_hh_INCLUDED
#define ObjexxFCL_random_hh_INCLUDED

// Random Number Functions
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// C++ Headers
#include <cstdint>

namespace ObjexxFCL {

// Forward
template< typename > class Array;

// Random float on [0,1)
void
RANDOM_NUMBER( float & harvest );

// Random double on [0,1)
void
RANDOM_NUMBER( double & harvest );

// Array of Random float on [0,1)
void
RANDOM_NUMBER( Array< float > & harvest );

// Array of Random double on [0,1)
void
RANDOM_NUMBER( Array< double > & harvest );

// Random float on [0,1)
void
RANDOM( float & ranval );

// Random float on [0,1)
float
RANDOM( int const iflag );

// Random float on [0,1)
inline
float
RAND( int const iflag = 0 )
{
	return RANDOM( iflag );
}

// Random float on [0,(2^31)-1)
float
RANF( Optional< int const > iseed = _ );

// Random double on [0,1)
double
DRANDM( int const iflag );

// Random double on [0,1)
inline
double
DRAND( int const iflag )
{
	return DRANDM( iflag );
}

// Random float on [0,1)
void
RANDU( int const i1, int const i2, float & x );

// Random int on [0,(2^15)-1]
std::int32_t
IRANDM();

// Random int on [0,(2^31)-1]
std::int32_t
IRANDM( int const iflag );

// Random int on [0,(2^15)-1]
inline
std::int32_t
IRAND()
{
	return IRANDM();
}

// Random int on [0,(2^31)-1]
inline
std::int32_t
IRAND( int const iflag )
{
	return IRANDM( iflag );
}

// Random Seed Interface
void
RANDOM_SEED(
 Optional< int > size = _,
 Optional< Array1D< int > const > put = _,
 Optional< Array1D< int > > get = _
);

// Random Seed Set
void
SRAND( int const iseed );

} // ObjexxFCL

#endif // ObjexxFCL_random_hh_INCLUDED
