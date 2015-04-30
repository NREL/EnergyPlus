// EnergyPlus::Low Temperature Radiant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::LowTempRadiantSystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;


class LowTempRadiantSystemTest : public ::testing::Test
{

public:

	int RadSysNum;

	// constructor for test fixture class
	LowTempRadiantSystemTest( )
	{
		ElecRadSys.allocate( 1 );
		HydrRadSys.allocate( 1 );
		CFloRadSys.allocate( 1 );
		ElecRadSysNumericFields.allocate( 1 );
		CalcFinalZoneSizing.allocate( 1 );
		ZoneEqSizing.allocate( 1 );
		Zone.allocate( 1 );
		CurZoneEqNum = 1;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneSizingRunDone = true;

		CurSysNum = 0;
		RadSysNum = 1;
		ElecRadSysNumericFields( RadSysNum ).FieldNames.allocate( 1 );
		ElecRadSys( RadSysNum ).Name = "LowTempElectric 1";
		ElecRadSys( RadSysNum ).ZonePtr = 1;
		ElecRadSysNumericFields( RadSysNum ).FieldNames( 1 ) = "Heating Design Capacity";

		int write_stat;
		// Open the Initialization Output File (lifted from SimulationManager.cc)
		OutputFileInits = GetNewUnitNumber( );
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios( ); }
	}

	//destructor
	~LowTempRadiantSystemTest( )
	{
		ElecRadSys.deallocate( );
		HydrRadSys.deallocate( );
		CFloRadSys.deallocate( );
		ElecRadSysNumericFields( 1 ).FieldNames.deallocate( );
		ElecRadSysNumericFields.deallocate( );
		CalcFinalZoneSizing.deallocate( );
		ZoneEqSizing( 1 ).SizingMethod.deallocate( );
		ZoneEqSizing.deallocate( );

		// Close and delete eio output file
		{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }
	}

};

TEST_F( LowTempRadiantSystemTest, SizeLowTempRadiantSystem )
{
	ShowMessage( "Begin Test: LowTempRadiantSystemTest, SizeLowTempRadiantSystem" );

	ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	ElecRadSys( RadSysNum ).HeatingCapMethod = HeatingDesignCapacity;
	ElecRadSys( RadSysNum ).ScaledHeatingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.2;
	SizeLowTempRadiantSystem( RadSysNum, ElectricSystem );
	EXPECT_NEAR( 1200.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );

	ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	ElecRadSys( RadSysNum ).HeatingCapMethod = CapacityPerFloorArea;
	ElecRadSys( RadSysNum ).ScaledHeatingCapacity = 1.5;
	Zone( 1 ).FloorArea = 500.0;
	SizeLowTempRadiantSystem( RadSysNum, ElectricSystem );
	EXPECT_NEAR( 750.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );

	ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	ElecRadSys( RadSysNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
	ElecRadSys( RadSysNum ).ScaledHeatingCapacity = 10.0;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 800.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.1;
	SizeLowTempRadiantSystem( RadSysNum, ElectricSystem );
	EXPECT_NEAR( 8800.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );
}
