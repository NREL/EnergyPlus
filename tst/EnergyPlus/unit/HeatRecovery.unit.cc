// EnergyPlus::Heat Recovery Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatRecovery;

class HeatRecoveryTest : public testing::Test
{

public:

	HeatRecoveryTest() // Setup global state
	{
		CurZoneEqNum = 0;
		CurSysNum = 0;
		CurOASysNum = 0;
		NumHeatExchangers = 1;
		ExchCond.allocate( NumHeatExchangers );
		Node.allocate( 2 );
	}

	~HeatRecoveryTest() // Reset global state
	{
		NumHeatExchangers = 0;
		ExchCond.clear();
		Node.clear();
	}

};

TEST_F( HeatRecoveryTest, HRTest)
{
	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	int ExchNum = 1;
	bool HXUnitOn = false;
	bool FirstHVACIteration = false;
	bool EconomizerFlag = false;
	bool HighHumCtrlFlag = false;

	ExchCond( ExchNum ).Name = "Test Heat Recovery";
	ExchCond( ExchNum ).ExchTypeNum = HX_AIRTOAIR_GENERIC;
	ExchCond( ExchNum ).NomSupAirVolFlow = AutoSize;
	ExchCond( ExchNum ).SupInTemp = 24.0;
	ExchCond( ExchNum ).SecInTemp = 15.0;
	ExchCond( ExchNum ).SupInHumRat = 0.01;
	ExchCond( ExchNum ).SecInHumRat = 0.01;
	ExchCond( ExchNum ).SupInEnth = 24000.0;
	ExchCond( ExchNum ).SecInEnth = 15000.0;
	ExchCond( ExchNum ).SupInletNode = 1;
	ExchCond( ExchNum ).SupOutletNode = 2;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 16.0;

	CurZoneEqNum = 0;
	CurSysNum = 0;
	CurOASysNum = 0;

	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	EXPECT_DOUBLE_EQ( 16.0, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

}
