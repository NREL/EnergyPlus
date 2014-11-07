// EnergyPlus::SortAndStringUtilities Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/SortAndStringUtilities.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SortAndStringUtilities;
using namespace ObjexxFCL;

TEST( SortAndStringUtilitiesTest, Basic )
{
	FArray1D_string Alphas( { "ZEBRA", "LION", "RACOON", "BOA", "LEMUR" } );
	FArray1D_int iAlphas( 5 );
	SetupAndSort( Alphas, iAlphas );
	EXPECT_TRUE( eq( FArray1D_int( { 4, 5, 2, 3, 1 } ), iAlphas ) );
}
