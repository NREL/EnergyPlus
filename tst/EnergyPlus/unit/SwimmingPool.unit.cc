// EnergyPlus::SwimmingPool Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/SwimmingPool.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SwimmingPool;

TEST( SwimmingPoolTest, MakeUpWaterVolFlow )
{
	ShowMessage( "Begin Test: SwimmingPoolTest, MakeUpWaterVolFlow" );
	
	//Tests for MakeUpWaterVolFlowFunct
	EXPECT_EQ( 500, MakeUpWaterVolFlowFunct(5, 100) );
	EXPECT_NEAR( 0.0255, MakeUpWaterVolFlowFunct(0.001, 25.5), .0001 );
	EXPECT_EQ( -0.45, MakeUpWaterVolFlowFunct(-9, .05) );
	EXPECT_NE(10, MakeUpWaterVolFlowFunct(10, 0) );

	//Tests for MakeUpWaterVolFunct
	EXPECT_EQ( 500, MakeUpWaterVolFunct(5, 100) );
	EXPECT_NEAR( 0.0255, MakeUpWaterVolFunct(0.001, 25.5), .0001 );
	EXPECT_EQ( -0.45, MakeUpWaterVolFunct(-9, .05) );
	EXPECT_NE(10, MakeUpWaterVolFunct(10, 0) );

}
