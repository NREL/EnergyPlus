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
#include <ObjexxFCL/random.hh>

// C++ Headers
#include <cmath>
#include <ctime>
#include <random>
#include <vector>

namespace ObjexxFCL {

namespace { // Internal shared global
std::default_random_engine random_generator;
}

// Random float on [0,1)
void
RANDOM_NUMBER( float & harvest )
{
	static std::uniform_real_distribution< float > distribution( 0.0f, 1.0f );
	harvest = distribution( random_generator );
}

// Random double on [0,1)
void
RANDOM_NUMBER( double & harvest )
{
	static std::uniform_real_distribution< double > distribution( 0.0, 1.0 );
	harvest = distribution( random_generator );
}

// Array of Random float on [0,1)
void
RANDOM_NUMBER( Array< float > & harvest )
{
	for ( BArray::size_type i = 0, e = harvest.size(); i < e; ++i ) harvest[ i ] = RANDOM( 0 );
}

// Array of Random double on [0,1)
void
RANDOM_NUMBER( Array< double > & harvest )
{
	for ( BArray::size_type i = 0, e = harvest.size(); i < e; ++i ) harvest[ i ] = DRANDM( 0 );
}

// Random float on [0,1)
void
RANDOM( float & ranval )
{
	static std::uniform_real_distribution< float > distribution( 0.0f, 1.0f );
	ranval = distribution( random_generator );
}

// Random float on [0,1)
float
RANDOM( int const iflag )
{
	static std::uniform_real_distribution< float > distribution( 0.0f, 1.0f );
	if ( iflag == 1 ) { // Reset distribution
		distribution.reset();
	} else if ( iflag != 0 ) { // Reseed generator and reset distribution
		random_generator.seed( iflag );
		distribution.reset();
	}
	return distribution( random_generator );
}

// Random float on [0,(2^31)-1)
float
RANF( Optional< int const > iseed )
{
	static std::uniform_real_distribution< float > distribution( 0.0f, std::pow( 2.0f, 31 ) - 1.0f );
	if ( iseed.present() ) random_generator.seed( iseed() );
	return distribution( random_generator );
}

// Random double on [0,1)
double
DRANDM( int const iflag )
{
	static std::uniform_real_distribution< double > distribution( 0.0, 1.0 );
	if ( iflag == 1 ) { // Reset distribution
		distribution.reset();
	} else if ( iflag != 0 ) { // Reseed generator and reset distribution
		random_generator.seed( iflag );
		distribution.reset();
	}
	return distribution( random_generator );
}

// Random float on [0,1)
void
RANDU( int const i1, int const i2, float & x )
{
	static std::uniform_real_distribution< float > distribution( 0.0f, 1.0f );
	random_generator.seed( i1 * i2 ); // This is not the infamous randu
	x = distribution( random_generator );
}

// Random int on [0,(2^15)-1]
std::int32_t
IRANDM()
{
	static std::uniform_int_distribution< std::int32_t > distribution( 0, 32767 );
	return distribution( random_generator );
}

// Random int on [0,(2^31)-1]
std::int32_t
IRANDM( int const iflag )
{
	static std::uniform_int_distribution< std::int32_t > distribution( 0, 2147483647 );
	if ( iflag == 1 ) { // Reset distribution
		distribution.reset();
	} else if ( iflag != 0 ) { // Reseed generator and reset distribution
		random_generator.seed( iflag );
		distribution.reset();
	}
	return distribution( random_generator );
}

// Random Seed Interface
void
RANDOM_SEED(
 Optional< int > size,
 Optional< Array1D< int > const > put,
 Optional< Array1D< int > > get
)
{
	static std::vector< int > seed_vals{ int( std::time( NULL ) ), int( std::time( NULL ) ) }; // C++ doesn't provide access to the seed values so we cache them here
	if ( size.present() ) {
		assert( ( ! put.present() ) && ( ! get.present() ) ); // At most one arg allowed
		size = static_cast< int >( seed_vals.size() );
	} else if ( put.present() ) {
		assert( ! get.present() ); // At most one arg allowed
		seed_vals.clear();
		for ( BArray::size_type i = 0, e = put().size(); i < e; ++i ) seed_vals.push_back( put()[ i ] );
		std::seed_seq seed_val_seq( seed_vals.begin(), seed_vals.end() );
		random_generator.seed( seed_val_seq ); // Not clear how to know how many seed values the generator is using
	} else if ( get.present() ) {
		get = 0; // In case it has more elements than seed_vals
		for ( std::vector< int >::size_type i = 0, e = std::min( seed_vals.size(), get().size() ); i < e; ++i ) get()[ i ] = seed_vals[ i ];
	} else { // No arguments
		seed_vals.assign( { int( std::time( NULL ) ), int( std::time( NULL ) ) } );
		std::seed_seq seed_val_seq( seed_vals.begin(), seed_vals.end() );
		random_generator.seed( seed_val_seq ); // Not clear how to know how many seed values the generator is using
	}
}

// Random Seed Set
void
SRAND( int const iseed )
{
	random_generator.seed( iseed );
}

} // ObjexxFCL
