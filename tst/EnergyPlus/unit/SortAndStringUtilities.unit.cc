// EnergyPlus::SortAndStringUtilities Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/InputProcessor.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SortAndStringUtilities;
using namespace EnergyPlus::InputProcessor;
using namespace ObjexxFCL;

TEST( SortAndStringUtilitiesTest, Basic )
{
	ShowMessage( "Begin Test: SortAndStringUtilitiesTest, Basic" );

	Array1D_string Alphas( { "ZEBRA", "LION", "RACOON", "BOA", "LEMUR" } );
	Array1D_int iAlphas( 5 );
	SetupAndSort( Alphas, iAlphas );
	EXPECT_TRUE( eq( Array1D_int( { 4, 5, 2, 3, 1 } ), iAlphas ) );
}

TEST( SortAndStringUtilitiesTest, findItemInSortedListUnderscoreTest)
{
	static Array1D_string ListOfObjects; // stored variable names
	static Array1D_int iListOfObjects;
	ListOfObjects.allocate(7);


	ListOfObjects = Array1D_string({			//list which has been incorrectly sorted
		"SYSTEM NODE STANDARD DENSITY VOLUME FLOW RATE",
		"SYSTEM NODE TEMPERATURE",
		"SYSTEM NODE WETBULB TEMPERATURE",
		"S_CCFRAC",
		"T_TRIG",
		"VRF HEAT PUMP CONDENSER INLET TEMPERATURE",
		"VRF HEAT PUMP COOLING COP",
	});

	int NumObjectDefs = ListOfObjects.size();

	iListOfObjects.allocate(NumObjectDefs);
	SetupAndSort(ListOfObjects, iListOfObjects);		//list is resorted

	auto index = FindItemInSortedList("SYSTEM NODE TEMPERATURE", ListOfObjects, NumObjectDefs);

	EXPECT_EQ(3, index);

}
