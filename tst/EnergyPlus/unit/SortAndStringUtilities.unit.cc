// EnergyPlus::SortAndStringUtilities Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SortAndStringUtilities;
using namespace ObjexxFCL;

TEST( SortAndStringUtilitiesTest, Basic )
{
	ShowMessage( "Begin Test: SortAndStringUtilitiesTest, Basic" );

	Array1D_string Alphas( { "ZEBRA", "LION", "RACOON", "BOA", "LEMUR" } );
	Array1D_int iAlphas( 5 );
	SetupAndSort( Alphas, iAlphas );
	EXPECT_TRUE( eq( Array1D_int( { 4, 5, 2, 3, 1 } ), iAlphas ) );
}
