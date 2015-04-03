// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ExteriorEnergyUse;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::ScheduleManager;

TEST( ExteriorEquipmentTest, Test1 )
{

	ShowMessage( "Begin Test: ExteriorEquipmentTest, Test1" );

	NumExteriorLights = 0;
	NumExteriorEqs = 2;
	TimeStepZone = 0.25;
	TimeStepZoneSec = TimeStepZone * SecInHour;
	ExteriorEquipment.allocate( NumExteriorEqs );
	ExteriorEquipment( 1 ).DesignLevel = 1000.0;
	ExteriorEquipment( 2 ).DesignLevel = 0.0;
	ExteriorEquipment( 1 ).SchedPtr = ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value
	ExteriorEquipment( 2 ).SchedPtr = ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value
	ReportExteriorEnergyUse();

	EXPECT_EQ( 1000.0, ExteriorEquipment( 1 ).Power );
	EXPECT_EQ( 0.0, ExteriorEquipment( 2 ).Power );
	EXPECT_EQ( 900000.0, ExteriorEquipment( 1 ).CurrentUse );
	EXPECT_EQ( 0.0, ExteriorEquipment( 2 ).CurrentUse );
}
