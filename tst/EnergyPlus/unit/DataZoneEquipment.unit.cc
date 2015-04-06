// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataZoneEquipment;
using namespace ObjexxFCL;

TEST( DataZoneEquipment, TestGetSystemNodeNumberForZone )
{

	ShowMessage( "Begin Test: DataZoneEquipment, TestGetSystemNodeNumberForZone" );

	NumOfZones = 2;
	ZoneEquipConfig.allocate( NumOfZones ); 

	ZoneEquipConfig( 1 ).ZoneName = "Zone1";
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;
	ZoneEquipConfig( 1 ).ZoneNode = 1;

	ZoneEquipConfig( 2 ).ZoneName = "Zone2";
	ZoneEquipConfig( 2 ).ActualZoneNum = 2;
	ZoneEquipConfig( 2 ).ZoneNode = 2;

	ZoneEquipInputsFilled = true;

	EXPECT_EQ( 0, GetSystemNodeNumberForZone( "NonExistingZone" ) );
	EXPECT_EQ( 1, GetSystemNodeNumberForZone( "Zone1" ) );

	ZoneEquipConfig.deallocate();
}
