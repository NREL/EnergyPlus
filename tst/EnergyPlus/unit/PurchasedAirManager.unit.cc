// EnergyPlus::PurchasedAirManager (Ideal Loads) Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>


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
