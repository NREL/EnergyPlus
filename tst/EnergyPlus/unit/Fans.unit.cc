// EnergyPlus::Fans Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::Fans;

class FansTest : public testing::Test
{

public:

	FansTest() // Setup global state
	{
		CurZoneEqNum = 0;
		CurSysNum = 0;
		CurOASysNum = 0;
		NumFans = 1;
		Fan.allocate( NumFans );
		FanNumericFields.allocate( NumFans );
		FanNumericFields( NumFans ).FieldNames.allocate( 3 );
	}

	~FansTest() // Reset global state
	{
		NumFans = 0;
		Fan.clear();
		FanNumericFields.clear();
	}

};

TEST_F( FansTest, FanSizing )
{

	ShowMessage( "Begin Test: FansTest, FanSizing" );

	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	int FanNum = 1;
	Fan( FanNum ).FanName = "Test Fan";
	Fan( FanNum ).FanType = "Fan:OnOff";
	Fan( FanNum ).FanType_Num = FanType_SimpleOnOff;
	Fan( FanNum ).MaxAirFlowRate = AutoSize;

	FanNumericFields( FanNum ).FieldNames( 3 ) = "Maximum Flow Rate";

	CurZoneEqNum = 0;
	CurSysNum = 0;
	CurOASysNum = 0;

	// DataNonZoneNonAirloopValue must be set when CurZoneEqNum and CurSysNum = 0
	DataNonZoneNonAirloopValue = 1.00635;
	SizeFan( FanNum );
	EXPECT_DOUBLE_EQ( 1.00635, Fan( FanNum ).MaxAirFlowRate );
	DataNonZoneNonAirloopValue = 0.0;

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

}
