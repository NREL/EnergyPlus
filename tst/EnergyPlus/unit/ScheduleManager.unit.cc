// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/ScheduleManager.hh>
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ScheduleManager;
using namespace ObjexxFCL;



TEST( ScheduleManagerTest, isMinuteMultipleOfTimestep )
{
	ShowMessage( "Begin Test: ScheduleManagerTest, isMinuteMultipleOfTimestep" );
	// EnergyPlus can accept 1,  2, 3,   4,  5,  6, 10, 12, 15, 20, 30, 60 timesteps per hour which correspond to
	//                      60, 30, 20, 15, 12, 10,  5,  5,  4,  3,  2,  1 minutes per timestep
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 0, 15 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 15, 15 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 30, 15 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 45, 15 ) );

	EXPECT_FALSE( isMinuteMultipleOfTimestep( 22, 15 ) );
	EXPECT_FALSE( isMinuteMultipleOfTimestep( 53, 15 ) );

	EXPECT_TRUE( isMinuteMultipleOfTimestep( 0, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 12, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 24, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 36, 12 ) );
	EXPECT_TRUE( isMinuteMultipleOfTimestep( 48, 12 ) );

	EXPECT_FALSE( isMinuteMultipleOfTimestep( 22, 12 ) );
	EXPECT_FALSE( isMinuteMultipleOfTimestep( 53, 12 ) );
}
