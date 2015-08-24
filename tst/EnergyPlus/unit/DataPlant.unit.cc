// EnergyPlus::DataPlant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/UtilityRoutines.hh>

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
	ShowMessage( "Begin Test: DataPlantTest, AnyPlantLoopSidesNeedSim" );

	EXPECT_TRUE( AnyPlantLoopSidesNeedSim() ); // SimLoopSideNeeded is set to true in default ctor
	SetAllPlantSimFlagsToValue( false ); // Set all SimLoopSideNeeded to false
	EXPECT_FALSE( AnyPlantLoopSidesNeedSim() );
}

TEST( DataPlant, verifyTwoNodeNumsOnSamePlantLoop )
{

	// not using the DataPlantTest base class because of how specific this one is and that one is very general
	if ( PlantLoop.allocated() ) PlantLoop.deallocate();
	TotNumLoops = 2;
	PlantLoop.allocate( 2 );
	PlantLoop( 1 ).LoopSide.allocate(2);
	PlantLoop( 1 ).LoopSide( 1 ).Branch.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 2 ).Branch.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 2 ).LoopSide.allocate(2);
	PlantLoop( 2 ).LoopSide( 1 ).Branch.allocate( 1 );
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 2 ).LoopSide( 2 ).Branch.allocate( 1 );
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp.allocate( 1 );

	// initialize all node numbers to zero
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 0;
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 0;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 0;
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 0;
	
	// specify the node numbers of interest
	int const nodeNumA = 1;
	int const nodeNumB = 2;

	// first test, expected pass
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 1;
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 2;
	EXPECT_TRUE( verifyTwoNodeNumsOnSamePlantLoop( nodeNumA, nodeNumB ) );

	// reset node numbers
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;

	// second test, expected false
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 1;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 2;
	EXPECT_FALSE( verifyTwoNodeNumsOnSamePlantLoop( nodeNumA, nodeNumB ) );

	TotNumLoops = 0;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.deallocate();
	PlantLoop( 1 ).LoopSide( 1 ).Branch.deallocate();
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp.deallocate();
	PlantLoop( 1 ).LoopSide( 2 ).Branch.deallocate();
	PlantLoop( 1 ).LoopSide.deallocate();
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp.deallocate();
	PlantLoop( 2 ).LoopSide( 1 ).Branch.deallocate();
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp.deallocate();
	PlantLoop( 2 ).LoopSide( 2 ).Branch.deallocate();
	PlantLoop( 2 ).LoopSide.deallocate();
	PlantLoop.allocate( 2 );

}
