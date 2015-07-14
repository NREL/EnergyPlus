// EnergyPlus::RoomAirModelUserTempPattern Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/RoomAirModelUserTempPattern.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::RoomAirModelUserTempPattern;

TEST( RoomAirModelUserTempPattern, OutdoorDryBulbGradTest )
{
	ShowMessage( "Begin Test: RoomAirModelUserTempPattern, OutdoorDryBulbGradTest" );

	EXPECT_EQ( 8, OutdoorDryBulbGrad(20, 10, 8, 0, 2));
	EXPECT_EQ( 2, OutdoorDryBulbGrad(-5, 10, 8, 0, 2));
	EXPECT_EQ( 2, OutdoorDryBulbGrad( 5, 10, 8, 10, 2));
	EXPECT_NEAR( 4.307, OutdoorDryBulbGrad( 5, 13, 8, 0, 2), .001);
}
