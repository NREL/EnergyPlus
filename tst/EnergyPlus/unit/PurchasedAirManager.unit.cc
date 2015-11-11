// EnergyPlus::PurchasedAirManager (Ideal Loads) Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"


// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::PurchasedAirManager;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHVACGlobals;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;


TEST( SizePurchasedAirTest, Test1 )
{
	ShowMessage( "Begin Test: SizePurchasedAirTest, Test1" );

	int PurchAirNum = 1;
	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }
//	eso_stream = gio::out_stream( OutputFileStandard );

	//ZoneEquipConfig.allocate( 1 );
	//ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	//ZoneEquipConfig( 1 ).ActualZoneNum = 1;

	//Zone.allocate( 1 );
	//Zone( 1 ).Name = ZoneEquipConfig( 1 ).ZoneName;
	ZoneEqSizing.allocate( 1 );
	CurZoneEqNum = 1;
	DataEnvironment::StdRhoAir = 1000; // Prevent divide by zero in ReportSizingManager
	ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 24 );
	CurSysNum = 0;
	ZoneHVACSizing.allocate( 1 );
	ZoneHVACSizing( 1 ).CoolingSAFMethod = SupplyAirFlowRate;
	ZoneHVACSizing( 1 ).HeatingSAFMethod = SupplyAirFlowRate;

	ZoneEqSizing( CurZoneEqNum ).AirVolFlow = 0.0;
	FinalZoneSizing.allocate( 1 );
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 1.0;
	ZoneEqSizing( CurZoneEqNum ).HeatingAirVolFlow = 1.0;
	FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp = 30.0;
	FinalZoneSizing( CurZoneEqNum ).HeatDesTemp = 80.0;
	FinalZoneSizing( CurZoneEqNum ).HeatDesHumRat = 0.008;
	FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow = 0.01;

	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 2.0;
	ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow = 2.0;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 60.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 50.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.008;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.010;
	FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow = 0.0103747425;

	PurchAir.allocate( 10 );
	PurchAirNumericFields.allocate( 10 );
	PurchAirNumericFields( PurchAirNum ).FieldNames.allocate( 8 );
	PurchAirNumericFields( PurchAirNum ).FieldNames( 5 ) = "Maximum Heating Air Flow Rate";
	PurchAirNumericFields( PurchAirNum ).FieldNames( 6 ) = "Maximum Sensible Heating Capacity";
	PurchAirNumericFields( PurchAirNum ).FieldNames( 7 ) = "Maximum Cooling Air Flow Rate";
	PurchAirNumericFields( PurchAirNum ).FieldNames( 8 ) = "Maximum Total Cooling Capacity";

	ZoneEqSizing( CurZoneEqNum ).SizingMethod( HeatingAirflowSizing ) = SupplyAirFlowRate;
	ZoneSizingRunDone = true;

	PurchAir( PurchAirNum ).HeatingLimit = LimitFlowRateAndCapacity;
	PurchAir( PurchAirNum ).MaxHeatVolFlowRate = AutoSize;
	PurchAir( PurchAirNum ).MaxHeatSensCap = AutoSize;
	PurchAir( PurchAirNum ).CoolingLimit = LimitFlowRateAndCapacity;
	PurchAir( PurchAirNum ).MaxCoolVolFlowRate = AutoSize;
	PurchAir( PurchAirNum ).MaxCoolTotCap = AutoSize;
	PurchAir( PurchAirNum ).cObjectName = "ZONEHVAC:IDEALLOADSAIRSYSTEM";
	PurchAir( PurchAirNum ).Name = "Ideal Loads 1";

	// Need this to prevent crash in RequestSizing
	UnitarySysEqSizing.allocate(10);

	SizePurchasedAir( PurchAirNum );
	EXPECT_DOUBLE_EQ( 1.0 , PurchAir( PurchAirNum ).MaxHeatVolFlowRate );
	EXPECT_NEAR( 509.856, PurchAir( PurchAirNum ).MaxHeatSensCap, 0.1 );
	EXPECT_DOUBLE_EQ( 2.0, PurchAir( PurchAirNum ).MaxCoolVolFlowRate );
	EXPECT_NEAR( 160.0, PurchAir( PurchAirNum ).MaxCoolTotCap, 0.1 );

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

	ZoneEqSizing(CurZoneEqNum).SizingMethod.deallocate();
	ZoneEqSizing.deallocate();
	ZoneHVACSizing.deallocate();
	FinalZoneSizing.deallocate();
	PurchAir.deallocate();
	PurchAirNumericFields.deallocate();
	UnitarySysEqSizing.deallocate();

}

TEST_F( EnergyPlusFixture, IdealLoadsAirSystem_GetInput )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"ZoneHVAC:IdealLoadsAirSystem,",
		"ZONE 1 Ideal Loads, !- Name",
		", !- Availability Schedule Name",
		"ZONE 1 INLETS, !- Zone Supply Air Node Name",
		", !- Zone Exhaust Air Node Name",
		"50, !- Maximum Heating Supply Air Temperature{ C }",
		"13, !- Minimum Cooling Supply Air Temperature{ C }",
		"0.015, !- Maximum Heating Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		"0.009, !- Minimum Cooling Supply Air Humidity Ratio{ kgWater / kgDryAir }",
		"NoLimit, !- Heating Limit",
		"autosize, !- Maximum Heating Air Flow Rate{ m3 / s }",
		", !- Maximum Sensible Heating Capacity{ W }",
		"NoLimit, !- Cooling Limit",
		"autosize, !- Maximum Cooling Air Flow Rate{ m3 / s }",
		", !- Maximum Total Cooling Capacity{ W }",
		", !- Heating Availability Schedule Name",
		", !- Cooling Availability Schedule Name",
		"ConstantSupplyHumidityRatio, !- Dehumidification Control Type",
		", !- Cooling Sensible Heat Ratio{ dimensionless }",
		"ConstantSupplyHumidityRatio, !- Humidification Control Type",
		", !- Design Specification Outdoor Air Object Name",
		", !- Outdoor Air Inlet Node Name",
		", !- Demand Controlled Ventilation Type",
		", !- Outdoor Air Economizer Type",
		", !- Heat Recovery Type",
		", !- Sensible Heat Recovery Effectiveness{ dimensionless }",
		";                        !- Latent Heat Recovery Effectiveness{ dimensionless }",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::DoWeathSim = true;

	GetPurchasedAir();

	EXPECT_EQ( PurchasedAirManager::PurchAir.size(), 1u );
	EXPECT_EQ( PurchAir( 1 ).Name, "ZONE 1 IDEAL LOADS" );
	EXPECT_EQ( PurchAir( 1 ).MaxHeatSuppAirTemp, 50.0);
	EXPECT_EQ( PurchAir( 1 ).MinCoolSuppAirTemp, 13.0 );
	EXPECT_EQ( PurchAir( 1 ).MaxHeatSuppAirHumRat, 0.015 );
	EXPECT_EQ( PurchAir( 1 ).MinCoolSuppAirHumRat, 0.009 );
	EXPECT_EQ( PurchAir( 1 ).HeatingLimit, NoLimit );
	EXPECT_EQ( PurchAir( 1 ).CoolingLimit, NoLimit );
	EXPECT_EQ( PurchAir( 1 ).DehumidCtrlType, ConstantSupplyHumidityRatio );
	EXPECT_EQ( PurchAir( 1 ).HumidCtrlType, ConstantSupplyHumidityRatio );

}

