// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataPlant.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataPlant;
using namespace ObjexxFCL;

class DataPlantTest : public testing::Test
{

public:

	DataPlantTest() // Setup global state
	{
		TotNumLoops = 3;
		PlantLoop.allocate( TotNumLoops );
		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
		}
	}

	~DataPlantTest() // Reset global state
	{
		TotNumLoops = 0;
		PlantLoop.clear();
	}

};

TEST_F( DataPlantTest, AnyPlantLoopSidesNeedSim )
{
	EXPECT_TRUE( AnyPlantLoopSidesNeedSim() ); // SimLoopSideNeeded is set to true in default ctor
	SetAllPlantSimFlagsToValue( false ); // Set all SimLoopSideNeeded to false
	EXPECT_FALSE( AnyPlantLoopSidesNeedSim() );
}
