// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::HVACStandAloneERV;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;


TEST( SizeStandAloneERVTest, Test1 )
{
	ShowMessage( "Begin Test: SizeStandAloneERVTest, Test1" );

	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	ZoneEquipConfig.allocate( 1 );
	ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;

	Zone.allocate( 1 );
	Zone( 1 ).Name = ZoneEquipConfig( 1 ).ZoneName;
	ZoneEqSizing.allocate( 1 );
	CurZoneEqNum = 1;

	TotPeople = 2; // Total number of people statements
	People.allocate(TotPeople);
	People( 1 ).ZonePtr = 1;
	People( 1 ).NumberOfPeople = 100.0;
	People( 1 ).NumberOfPeoplePtr = ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value
	People( 2 ).ZonePtr = 1;
	People( 2 ).NumberOfPeople = 200.0;
	People( 2 ).NumberOfPeoplePtr = ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value

	StandAloneERV.allocate( 1 );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize;
	StandAloneERV( 1 ).AirVolFlowPerFloorArea = 1.0;
	StandAloneERV( 1 ).AirVolFlowPerOccupant = 0.0;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).FloorArea = 1000.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 1000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize; // Need to reset this for each pass
	StandAloneERV( 1 ).AirVolFlowPerFloorArea = 0.0;
	StandAloneERV( 1 ).AirVolFlowPerOccupant = 10.0;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).FloorArea = 1000.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 3000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize;
	StandAloneERV( 1 ).AirVolFlowPerFloorArea = 1.0;
	StandAloneERV( 1 ).AirVolFlowPerOccupant = 10.0;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).FloorArea = 1000.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 4000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize;
	Zone( 1 ).Multiplier = 5.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 20000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

	// Clean up
	ZoneEquipConfig.deallocate();
	Zone.deallocate();
	ZoneEqSizing.deallocate();
	People.deallocate();
	StandAloneERV.deallocate();


}
