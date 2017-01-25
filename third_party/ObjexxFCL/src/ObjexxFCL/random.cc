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
#include <ObjexxFCL/random.hh>

// C++ Headers
#include <ctime>
#include <random>
#include <vector>

namespace ObjexxFCL {

namespace { // Internal shared global
std::default_random_engine random_generator;
}

// Random float on [0,1]
std::int32_t
IRANDM()
{
	static std::uniform_int_distribution< std::int32_t > distribution( 0, 32767 );
	return distribution( random_generator );
}

// Random float on [0,1]
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

// Random float on [0,1]
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

// Random double on [0,1]
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

// Array of Random float on [0,1]
void
RANDOM_NUMBER( Array< float > & harvest )
{
	for ( Array< float >::size_type i = 0, e = harvest.size(); i < e; ++i ) harvest[ i ] = RANDOM( 0 );
}

// Array of Random double on [0,1]
void
RANDOM_NUMBER( Array< double > & harvest )
{
	for ( Array< float >::size_type i = 0, e = harvest.size(); i < e; ++i ) harvest[ i ] = DRANDM( 0 );
}

// Random Seed Set
void
SRAND( int const iseed )
{
	random_generator.seed( iseed );
}

// Random Seed Interface
void
RANDOM_SEED( Optional< int > size, Optional< Array1< int > const > put, Optional< Array1< int > > get )
{
	static std::vector< int > seed_vals{ int( std::time( NULL ) ), int( std::time( NULL ) ) }; // C++ doesn't provide access to the seed values so we cache them here
	if ( size.present() ) {
		assert( ( ! put.present() ) && ( ! get.present() ) ); // At most one arg allowed
		size = static_cast< int >( seed_vals.size() );
	} else if ( put.present() ) {
		assert( ( ! size.present() ) && ( ! get.present() ) ); // At most one arg allowed
		seed_vals.clear();
		for ( Array1< int >::size_type i = 0, e = put().size(); i < e; ++i ) seed_vals.push_back( put()[ i ] );
		std::seed_seq seed_val_seq( seed_vals.begin(), seed_vals.end() );
		random_generator.seed( seed_val_seq ); // Not clear how to know how many seed values the generator is using
	} else if ( get.present() ) {
		assert( ( ! size.present() ) && ( ! put.present() ) ); // At most one arg allowed
		get = 0; // In case it has more elements than seed_vals
		for ( std::vector< int >::size_type i = 0, e = std::min( seed_vals.size(), get().size() ); i < e; ++i ) get()[ i ] = seed_vals[ i ];
	} else { // No arguments
		seed_vals.assign( { int( std::time( NULL ) ), int( std::time( NULL ) ) } );
		std::seed_seq seed_val_seq( seed_vals.begin(), seed_vals.end() );
		random_generator.seed( seed_val_seq ); // Not clear how to know how many seed values the generator is using
	}
}

} // ObjexxFCL
