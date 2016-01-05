// EnergyPlus, Copyright (c) 1996-2015, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::Heat Recovery Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReturnAirPathManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace DataEnvironment;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatRecovery;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::SimAirServingZones;
using namespace EnergyPlus::ReturnAirPathManager;


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
	int FanOpMode = 2; // 1 = cycling fan, 2 = constant fan
	Real64 Toutlet = 0.0;
	Real64 Tnode = 0.0;
	Real64 SetPointTemp = 19.0;
	Real64 PartLoadRatio = 0.25;

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
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
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
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) );
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
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
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = SetPointTemp;
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
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
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ExchCond( ExchNum ).SupInTemp, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = false;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is false so expect outlet = temperature based on effectiveness
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = true;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is true so expect outlet = set point temperature
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	// test cycling fan case
	FanOpMode = DataHVACGlobals::CycFanCycCoil;
	Node( ExchCond( ExchNum ).SupInletNode ).MassFlowRate = ExchCond( ExchNum ).SupInMassFlow / 4.0;
	Node( ExchCond( ExchNum ).SecInletNode ).MassFlowRate = ExchCond( ExchNum ).SecInMassFlow / 4.0;
	ExchCond( ExchNum ).ControlToTemperatureSetPoint = false;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag, PartLoadRatio );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

}


TEST_F( EnergyPlusFixture, RotaryHROnManinBranchTest ) {
		int write_stat;
		OutputFileInits = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

		CurZoneEqNum = 0;
		CurSysNum = 0;
		CurOASysNum = 0;

		std::string const idf_objects = delimited_string( {
			" Version,8.4;",
			" Coil:Cooling:Water,",
			"	AHU cooling coil,	!- Name",
			"	AvailSched,			!- Availability Schedule Name",
			"	autosize,			!- Design Water Flow Rate { m3 / s }",
			"	autosize,			!- Design Air Flow Rate { m3 / s }",
			"	autosize,			!- Design Inlet Water Temperature { C }",
			"	autosize,			!- Design Inlet Air Temperature { C }",
			"	autosize,			!- Design Outlet Air Temperature { C }",
			"	autosize,			!- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	autosize,			!- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	Water Inlet Node,	!- Water Inlet Node Name",
			"	Water Outlet Node,  !- Water Outlet Node Name",
			"	AHU mixed air outlet,		!- Air Inlet Node Name",
			"	AHU cooling coil outlet,	!- Air Outlet Node Name",
			"	SimpleAnalysis,		!- Type of Analysis",
			"	CrossFlow;          !- Heat Exchanger Configuration",
			" Coil:Heating:Water,",
			"	AHU Heating coil, !- Name",
			"	AvailSched,       !- Availability Schedule Name",
			"	autosize, !- U - Factor Times Area Value { W / K }",
			"	autosize, !- Maximum Water Flow Rate { m3 / s }",
			"	AHU Heating COil HW Inlet, !- Water Inlet Node Name",
			"	AHU Heating COil HW Outlet, !- Water Outlet Node Name",
			"	AHU cooling coil outlet, !- Air Inlet Node Name",
			"	AHU Heating Coil Outlet, !- Air Outlet Node Name",
			"	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
			"	autosize, !- Rated Capacity { W }",
			"	82.2, !- Rated Inlet Water Temperature { C }",
			"	16.6, !- Rated Inlet Air Temperature { C }",
			"	71.1, !- Rated Outlet Water Temperature { C }",
			"	48.8888888888889, !- Rated Outlet Air Temperature { C }",
			"	1;          !- Rated Ratio for Air and Water Convection",
			" Controller:WaterCoil,",
			"	AHU cooling coil controller, !- Name",
			"	TemperatureAndHumidityRatio,		!- Control Variable",
			"	Reverse,			!- Action",
			"	FLOW,				!- Actuator Variable",
			"	AHU cooling coil outlet,	!- Sensor Node Name",
			"	Water Inlet Node,	!- Actuator Node Name",
			"	autosize,			!- Controller Convergence Tolerance { deltaC }",
			"	autosize,			!- Maximum Actuated Flow { m3 / s }",
			"	0.0;				!- Minimum Actuated Flow { m3 / s }",
			" Controller:WaterCoil,",
			"	AHU Heating coil, !- Name",
			"	Temperature,      !- Control Variable",
			"	Normal, !- Action",
			"	Flow,   !- Actuator Variable",
			"	AHU Heating Coil Outlet,   !- Sensor Node Name",
			"	AHU Heating COil HW Inlet, !- Actuator Node Name",
			"	autosize, !- Controller Convergence Tolerance { deltaC }",
			"	autosize, !- Maximum Actuated Flow { m3 / s }",
			"	0;        !- Minimum Actuated Flow { m3 / s }",
			" Schedule:Compact,",
			"   AvailSched,			!- Name",
			"	Fraction,			!- Schedule Type Limits Name",
			"	Through: 12/31,		!- Field 1",
			"	For: AllDays,		!- Field 2",
			"	Until: 24:00, 1.0;  !- Field 3",
			" AirLoopHVAC:ControllerList,",
			"	AHU controllers,    !- Name",
			" Controller:WaterCoil, !- Controller 1 Object Type",
			"	AHU cooling coil controller, !- Controller 1 Name",
			" Controller:WaterCoil, !- Controller 2 Object Type",
			"	AHU Heating coil;   !- Controller 2 Name",
			" HeatExchanger:AirToAir:SensibleAndLatent,",
			"   enthalpy HX,      !- Name",
			"   AvailSched,       !- Availability Schedule Name",
			"   4.71947443200001, !- Nominal Supply Air Flow Rate { m3 / s }",
			"   0,   !- Sensible Effectiveness at 100 % Heating Air Flow { dimensionless }",
			"   0.5, !- Latent Effectiveness at 100 % Heating Air Flow { dimensionless }",
			"   0,   !- Sensible Effectiveness at 75 % Heating Air Flow { dimensionless }",
			"   0.5, !- Latent Effectiveness at 75 % Heating Air Flow { dimensionless }",
			"   0,   !- Sensible Effectiveness at 100 % Cooling Air Flow { dimensionless }",
			"   0.5, !- Latent Effectiveness at 100 % Cooling Air Flow { dimensionless }",
			"   0,   !- Sensible Effectiveness at 75 % Cooling Air Flow { dimensionless }",
			"   0.5, !- Latent Effectiveness at 75 % Cooling Air Flow { dimensionless }",
			"   AHU Heating Coil Outlet, !- Supply Air Inlet Node Name",
			"   AHU Supply fan Inlet,    !- Supply Air Outlet Node Name",
			"   AHU relief air outlet,   !- Exhaust Air Inlet Node Name",
			"   AHU relief air outlet of ENTHALPY HX, !- Exhaust Air Outlet Node Name",
			"   0,      !- Nominal Electric Power { W }",
			"   No,     !- Supply Air Outlet Temperature Control",
			"   Rotary, !- Heat Exchanger Type",
			"   None,   !- Frost Control Type",
			"  -17.7777777777778, !- Threshold Temperature { C }",
			"   0.083,  !- Initial Defrost Time Fraction { dimensionless }",
			"   0.012,  !- Rate of Defrost Time Fraction Increase { 1 / K }",
			"   Yes;    !- Economizer Lockout",
			" Fan:VariableVolume,",
			"   AHU supply fan, !- Name",
			"   AvailSched,     !- Availability Schedule Name",
			"   0.7,            !- Fan Total Efficiency",
			"   996.355828557053, !- Pressure Rise { Pa }",
			"   autosize, !- Maximum Flow Rate { m3 / s }",
			"   Fraction, !- Fan Power Minimum Flow Rate Input Method",
			"   0,        !- Fan Power Minimum Flow Fraction",
			"   0,        !- Fan Power Minimum Air Flow Rate { m3 / s }",
			"   0.95,     !- Motor Efficiency",
			"   1,        !- Motor In Airstream Fraction",
			"   0.35071223, !- Fan Power Coefficient 1",
			"   0.30850535, !- Fan Power Coefficient 2",
			"  -0.54137364, !- Fan Power Coefficient 3",
			"   0.87198823, !- Fan Power Coefficient 4",
			"   0,          !- Fan Power Coefficient 5",
			"   AHU Supply fan Inlet,  !- Air Inlet Node Name",
			"   AHU Supply fan Outlet, !- Air Outlet Node Name",
			"   General;               !- End - Use Subcategory",
			" Branch,",
			"   AHU Main Branch, !- Name",
			" 	autosize, !- Maximum Flow Rate {m3/s}",
			"	,         !- Pressure Drop Curve Name",
			"   AirLoopHVAC:OutdoorAirSystem, !- Component 1 Object Type",
			"   AHU OA system,           !- Component 1 Name",
			"   AHU air loop inlet,      !- Component 1 Inlet Node Name",
			"   AHU mixed air outlet,    !- Component 1 Outlet Node Name",
			"   Passive,                 !- Component 1 Branch Control Type",
			" Coil:Cooling:water,        !- Component 2 Object Type",
			"   AHU cooling coil,        !- Component 2 Name",
			"   AHU mixed air outlet,    !- Component 2 Inlet Node Name",
			"   AHU cooling coil outlet, !- Component 2 Outlet Node Name",
			"   Passive,                 !- Component 2 Branch Control Type",
			" Coil:Heating:Water,        !- Component 3 Object Type",
			"   AHU Heating coil,        !- Component 3 Name",
			"   AHU cooling coil outlet, !- Component 3 Inlet Node Name",
			"   AHU Heating Coil Outlet, !- Component 3 Outlet Node Name",
			"   Passive,                 !- Component 3 Branch Control Type",
			" HeatExchanger:AirToAir:SensibleAndLatent, !- Component 4 Object Type",
			"   enthalpy HX,             !- Component 4 Name",
			"   AHU Heating Coil Outlet, !- Component 4 Inlet Node Name",
			"   AHU Supply fan Inlet,    !- Component 4 Outlet Node Name",
			"   Passive,                 !- Component 4 Branch Control Type",
			" Fan:VariableVolume,        !- Component 5 Object Type",
			"   AHU Supply Fan,          !- Component 5 Name",
			"   AHU Supply fan Inlet,    !- Component 5 Inlet Node Name",
			"   AHU Supply fan Outlet,   !- Component 5 Outlet Node Name",
			"   Active;                  !- Component 5 Branch Control Type",		
			" AirLoopHVAC,",
			"   AHU,                   !- Name",
			"   AHU controllers,       !- Controller List Name",
			"   ,                      !- Availability Manager List Name",
			"   autosize,              !- Design Supply Air Flow Rate { m3 / s }",
			"   AHU Branches,          !- Branch List Name",
			"   ,                      !- Connector List Name",
			"   AHU air loop inlet,    !- Supply Side Inlet Node Name",
			"   AHU return air outlet, !- Demand Side Outlet Node Name",
			"   AHU Supply Path Inlet, !- Demand Side Inlet Node Names",
			"   AHU Supply fan Outlet; !- Supply Side Outlet Node Names",
			" BranchList,",
			"   AHU Branches,          !- Name",
			"   AHU Main Branch;       !- Branch 1 Name",
			" AirLoopHVAC:ReturnPath,",
			"   AHU return path,       !- Name",
			"   AHU return air outlet, !- Return Air Path Outlet Node Name",
			" AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
			"   AHU zone mixer;        !- Component 1 Name",
			" AirLoopHVAC:ZoneMixer,",
			"   AHU zone mixer,        !- Name",
			"   AHU return air outlet, !- Outlet Node Name",
			"   Main FL1 Return Outlet,!- Inlet 1 Node Name",
			" ZoneHVAC:EquipmentConnections,",
			"   Main FL1,              !- Zone Name",
			"   Main FL1 Equipment,    !- Zone Conditioning Equipment List Name",
			"   Main FL1 Supply inlet, !- Zone Air Inlet Node or NodeList Name",
			"   ,                      !- Zone Air Exhaust Node or NodeList Name",
			"   Main FL1 Zone Air node,!- Zone Air Node Name",
			"   Main FL1 Return Outlet;!- Zone Return Air Node Name",
			" AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
			"   AHU System equipment,  !- Name",
			" OutdoorAir:Mixer,        !- Component 1 Object Type",
			"   AHU OA Mixing Box;     !- Component 1 Name",
			" AirLoopHVAC:OutdoorAirSystem,",
			"   AHU OA System,             !- Name",
			"   AHU OA system controllers, !- Controller List Name",
			"   AHU System equipment;      !- Outdoor Air Equipment List Name",
			" OutdoorAir:Mixer,",
			"   AHU OA Mixing Box,         !- Name",
			"   AHU mixed air outlet,      !- Mixed Air Node Name",
			"   AHU Outside Air HX Outlet, !- Outdoor Air Stream Node Name",
			"   AHU relief air outlet,     !- Relief Air Stream Node Name",
			"   AHU air loop inlet;        !- Return Air Stream Node Name",
			" AirLoopHVAC:ControllerList,",
			"   AHU OA system controllers, !- Name",
			" Controller:OutdoorAir,       !- Controller 1 Object Type",
			"   AHU OA Controller;         !- Controller 1 Name",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetReturnAirPathInput();
		GetAirPathData();		
		ASSERT_EQ( SimAirServingZones::HeatXchngr, PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 4 ).CompType_Num );
		// Close and delete eio output file
		{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }

}
