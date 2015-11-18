// EnergyPlus::PurchasedAirManager (Ideal Loads) Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataSizing.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::PlantUtilities;
using namespace ObjexxFCL;
using namespace DataSizing;

TEST( PlantUtilities, RegisterPlantCompDesignFlowTest1 )
{
	ShowMessage( "Begin Test: PlantUtilities, RegisterPlantCompDesignFlowTest1" );


	// first call just puts first value in array
	int TestNodeNum1 = 123;
	Real64 TestFlowRate1 = 45.6;
	SaveNumPlantComps = 0;
	RegisterPlantCompDesignFlow( TestNodeNum1, TestFlowRate1 );
	EXPECT_EQ( TestNodeNum1, CompDesWaterFlow( 1 ).SupNode );
	EXPECT_EQ( TestFlowRate1, CompDesWaterFlow( 1 ).DesVolFlowRate);

	// second call searches array and since node not found adds an entry to array
	int TestNodeNum2 = 234;
	Real64 TestFlowRate2 = 56.7;
	RegisterPlantCompDesignFlow( TestNodeNum2, TestFlowRate2 );
	EXPECT_EQ( TestNodeNum2, CompDesWaterFlow( 2 ).SupNode );
	EXPECT_EQ( TestFlowRate2, CompDesWaterFlow( 2 ).DesVolFlowRate );

	// third call searches array and since node was found adds an entry to array
	Real64 TestFlowRate3 = 67.8;
	RegisterPlantCompDesignFlow( TestNodeNum1, TestFlowRate3 );
	EXPECT_EQ( TestFlowRate3, CompDesWaterFlow( 1 ).DesVolFlowRate );

	// clean up
	SaveNumPlantComps = 0;
	CompDesWaterFlow.deallocate();

}


