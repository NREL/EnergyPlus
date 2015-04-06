// EnergyPlus::Heat Recovery Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace DataEnvironment;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatRecovery;
using namespace EnergyPlus::Psychrometrics;

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
		Node.allocate( 4 );
		InitializePsychRoutines();
		OutBaroPress = 101325.0;
	}

	~HeatRecoveryTest() // Reset global state
	{
		CurZoneEqNum = 0;
		CurSysNum = 0;
		CurOASysNum = 0;
		NumHeatExchangers = 0;
		ExchCond.clear();
		Node.clear();
		cached_Twb.clear();
		cached_Psat.clear();
		OutBaroPress = 0.0;
	}

};

TEST_F( HeatRecoveryTest, HRTest)
{
	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	int ExchNum = 1;
	int CompanionCoilNum = 0;
	bool HXUnitOn = false;
	bool FirstHVACIteration = false;
	bool EconomizerFlag = false;
	bool HighHumCtrlFlag = false;
	Real64 Toutlet = 0.0;
	Real64 Tnode = 0.0;
	Real64 SetPointTemp = 19.0;

	CurZoneEqNum = 0;
	CurSysNum = 0;
	CurOASysNum = 0;

	ExchCond( ExchNum ).NomSupAirVolFlow = 1.0;
	ExchCond( ExchNum ).SupInMassFlow = 1.0;
	ExchCond( ExchNum ).SecInMassFlow = 1.0;
	ExchCond( ExchNum ).SupInletNode = 1;
	ExchCond( ExchNum ).SupOutletNode = 2;
	ExchCond( ExchNum ).SecInletNode = 3;
	ExchCond( ExchNum ).SecOutletNode = 4;
	ExchCond( ExchNum ).SchedPtr = -1;
	ExchCond( ExchNum ).HeatEffectSensible75 = 0.75;
	ExchCond( ExchNum ).HeatEffectSensible100 = 0.75;
	ExchCond( ExchNum ).HeatEffectLatent75 = 0.0;
	ExchCond( ExchNum ).HeatEffectLatent100 = 0.0;
	ExchCond( ExchNum ).CoolEffectSensible75 = 0.75;
	ExchCond( ExchNum ).CoolEffectSensible100 = 0.75;
	ExchCond( ExchNum ).CoolEffectLatent75 = 0.0;
	ExchCond( ExchNum ).CoolEffectLatent100 = 0.0;

	ExchCond( ExchNum ).Name = "Test Heat Recovery 1";
	ExchCond( ExchNum ).ExchTypeNum = HX_AIRTOAIR_GENERIC;
	ExchCond( ExchNum ).SupInTemp = 24.0;
	ExchCond( ExchNum ).SecInTemp = 15.0;
	ExchCond( ExchNum ).SupInHumRat = 0.01;
	ExchCond( ExchNum ).SecInHumRat = 0.01;
	ExchCond( ExchNum ).SupInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SupInTemp, ExchCond( ExchNum ).SupInHumRat );
	ExchCond( ExchNum ).SecInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SecInTemp, ExchCond( ExchNum ).SecInHumRat );
	Node( ExchCond( ExchNum ).SupInletNode ).Temp = ExchCond( ExchNum ).SupInTemp;
	Node( ExchCond( ExchNum ).SecInletNode ).Temp = ExchCond( ExchNum ).SecInTemp;
	Node( ExchCond( ExchNum ).SupInletNode ).HumRat = ExchCond( ExchNum ).SupInHumRat;
	Node( ExchCond( ExchNum ).SecInletNode ).HumRat = ExchCond( ExchNum ).SecInHumRat;
	Node( ExchCond( ExchNum ).SupInletNode ).Enthalpy = ExchCond( ExchNum ).SupInEnth;
	Node( ExchCond( ExchNum ).SecInletNode ).Enthalpy = ExchCond( ExchNum ).SecInEnth;
	Node( ExchCond( ExchNum ).SupInletNode ).MassFlowRate = ExchCond( ExchNum ).SupInMassFlow;
	Node( ExchCond( ExchNum ).SecInletNode ).MassFlowRate = ExchCond( ExchNum ).SecInMassFlow;

	// HXUnitOn is false so expect outlet = inlet
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = ExchCond( ExchNum ).SupInTemp;
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = false;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = SetPointTemp;

	// HXUnitOn is true and ControlToTemperatureSetPoint is false so expect outlet = temperature based on effectiveness
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) );
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) );
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = true;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is true so expect outlet = set point temperature
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = SetPointTemp;
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint;
	Tnode = Node( ExchCond( ExchNum ).SupOutletNode ).Temp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).Name = "Test Heat Recovery 2";
	ExchCond( ExchNum ).ExchTypeNum = HX_AIRTOAIR_GENERIC;
	ExchCond( ExchNum ).SupInTemp = 15.0;
	ExchCond( ExchNum ).SecInTemp = 24.0;
	ExchCond( ExchNum ).SupInHumRat = 0.01;
	ExchCond( ExchNum ).SecInHumRat = 0.01;
	ExchCond( ExchNum ).SupInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SupInTemp, ExchCond( ExchNum ).SupInHumRat );
	ExchCond( ExchNum ).SecInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SecInTemp, ExchCond( ExchNum ).SecInHumRat );
	Node( ExchCond( ExchNum ).SupInletNode ).Temp = ExchCond( ExchNum ).SupInTemp;
	Node( ExchCond( ExchNum ).SecInletNode ).Temp = ExchCond( ExchNum ).SecInTemp;
	Node( ExchCond( ExchNum ).SupInletNode ).HumRat = ExchCond( ExchNum ).SupInHumRat;
	Node( ExchCond( ExchNum ).SecInletNode ).HumRat = ExchCond( ExchNum ).SecInHumRat;
	Node( ExchCond( ExchNum ).SupInletNode ).Enthalpy = ExchCond( ExchNum ).SupInEnth;
	Node( ExchCond( ExchNum ).SecInletNode ).Enthalpy = ExchCond( ExchNum ).SecInEnth;
	Node( ExchCond( ExchNum ).SupInletNode ).MassFlowRate = ExchCond( ExchNum ).SupInMassFlow;
	Node( ExchCond( ExchNum ).SecInletNode ).MassFlowRate = ExchCond( ExchNum ).SecInMassFlow;

	// HXUnitOn is false so expect outlet = inlet
	HXUnitOn = false;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ExchCond( ExchNum ).SupInTemp, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = false;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is false so expect outlet = temperature based on effectiveness
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = true;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is true so expect outlet = set point temperature
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

}
