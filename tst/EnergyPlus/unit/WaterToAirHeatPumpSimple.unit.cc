// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// EnergyPlus::WaterToAirHeatPumpSimple Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::WaterToAirHeatPumpSimple;
using General::RoundSigDigits;

TEST_F( EnergyPlusFixture, WaterToAirHeatPumpSimpleTest_SizeHVACWaterToAir )
{
	// This unit test is intended to check if supply air Humidity ratio used in the cooling sizing calculation is 
	// reset to the minimum of entering mixed air humidity ratio and the user specified supply air design Humidity  
	// ratio such that the total cooling capacity is always greater than or equal to the sensible cooling capacity.
	// This test was added to test bug issue #4893 fix, a defect that resulted in SHR greater than 1.0.

	int HPNum( 1 );

	SysSizingRunDone = true;
	ZoneSizingRunDone = true;
	CurSysNum = 0;
	CurZoneEqNum = 1;

	SimpleWatertoAirHP.allocate( HPNum );
	FinalZoneSizing.allocate( CurZoneEqNum );
	ZoneEqSizing.allocate( CurZoneEqNum );
	DesDayWeath.allocate( 1 );
	DesDayWeath( 1 ).Temp.allocate( 24 );

	SimpleWatertoAirHP( HPNum ).WatertoAirHPType = "COOLING";
	SimpleWatertoAirHP( HPNum ).RatedAirVolFlowRate = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedCapCoolSens = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedWaterVolFlowRate = 0.0;

	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.20;
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.20;
	FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 13.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.0075;
	FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax = 15;
	FinalZoneSizing( CurZoneEqNum ).CoolDDNum = 1;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 25.5;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.0045;
	FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak = 25.5;
	FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak = 0.0045;
	ZoneEqSizing( CurZoneEqNum ).OAVolFlow = 0.0;

	// performance curve coefficients
	SimpleWatertoAirHP( HPNum ).TotalCoolCap1 = -9.149069561;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap2 = 10.878140260;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap3 = -1.718780157;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap4 =  0.746414818;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap5 =  0.0;

	SimpleWatertoAirHP( HPNum ).RatedCOPCool = 5.12;
	SimpleWatertoAirHP( HPNum ).SensCoolCap1 = -5.462690012;
	SimpleWatertoAirHP( HPNum ).SensCoolCap2 = 17.95968138;
	SimpleWatertoAirHP( HPNum ).SensCoolCap3 =-11.87818402;
	SimpleWatertoAirHP( HPNum ).SensCoolCap4 = -0.980163419;
	SimpleWatertoAirHP( HPNum ).SensCoolCap5 =  0.767285761;
	SimpleWatertoAirHP( HPNum ).SensCoolCap6 = 0.0;

	DesDayWeath( 1 ).Temp( 15 ) = 32.0;
	StdBaroPress = 101325.0;
	ZoneEqDXCoil = true;

	WaterToAirHeatPumpSimple::SizeHVACWaterToAir( HPNum );

	// check that the design oulet air humidity ratio did not change
	EXPECT_DOUBLE_EQ( 0.0075, FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat );

	// check that the total cooling capacity is >= the sensible cooling capacity
	EXPECT_GE( SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal, SimpleWatertoAirHP( HPNum ).RatedCapCoolSens );

	if ( SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal != 0.0 ) {
		ShowMessage( "SizeHVACWaterToAir: Rated Sensible Heat Ratio = " + RoundSigDigits( SimpleWatertoAirHP( HPNum ).RatedCapCoolSens / SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal, 2 ) + " [-]" );
	} 

}

TEST_F( EnergyPlusFixture, WaterToAirHeatPumpSimple_TestWaterFlowControl )
{

	std::string const idf_objects = delimited_string( {

	" Coil:Cooling:WaterToAirHeatPump:EquationFit,",
	"   Sys 5 Heat Pump Cooling Mode,  !- Name",
	"   Sys 5 Water to Air Heat Pump Source Side1 Inlet Node,  !- Water Inlet Node Name",
	"   Sys 5 Water to Air Heat Pump Source Side1 Outlet Node,  !- Water Outlet Node Name",
	"   Sys 5 Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
	"   Sys 5 Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
	"   1.0,                     !- Rated Air Flow Rate {m3/s}",
	"   0.0033,                  !- Rated Water Flow Rate {m3/s}",
	"   23125.59,                !- Gross Rated Total Cooling Capacity {W}",
	"   16267,                   !- Gross Rated Sensible Cooling Capacity {W}",
	"   7.007757577,             !- Gross Rated Cooling COP",
	"   -0.68126221,             !- Total Cooling Capacity Coefficient 1",
	"   1.99529297,              !- Total Cooling Capacity Coefficient 2",
	"   -0.93611888,             !- Total Cooling Capacity Coefficient 3",
	"   0.02081177,              !- Total Cooling Capacity Coefficient 4",
	"   0.008438868,             !- Total Cooling Capacity Coefficient 5",
	"   2.24209455,              !- Sensible Cooling Capacity Coefficient 1",
	"   7.28913391,              !- Sensible Cooling Capacity Coefficient 2",
	"   -9.06079896,             !- Sensible Cooling Capacity Coefficient 3",
	"   -0.36729404,             !- Sensible Cooling Capacity Coefficient 4",
	"   0.218826161,             !- Sensible Cooling Capacity Coefficient 5",
	"   0.00901534,              !- Sensible Cooling Capacity Coefficient 6",
	"   -3.20456384,             !- Cooling Power Consumption Coefficient 1",
	"   0.47656454,              !- Cooling Power Consumption Coefficient 2",
	"   3.16734236,              !- Cooling Power Consumption Coefficient 3",
	"   0.10244637,              !- Cooling Power Consumption Coefficient 4",
	"   -0.038132556,            !- Cooling Power Consumption Coefficient 5",
	"   0,                       !- Nominal Time for Condensate Removal to Begin {s}",
	"   0;                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",

	" Coil:Heating:WaterToAirHeatPump:EquationFit,",
	"  Sys 5 Heat Pump Heating Mode,  !- Name",
	"  Sys 5 Water to Air Heat Pump Source Side2 Inlet Node,  !- Water Inlet Node Name",
	"  Sys 5 Water to Air Heat Pump Source Side2 Outlet Node,  !- Water Outlet Node Name",
	"  Sys 5 Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
	"  Sys 5 SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name",
	"  1.0,                     !- Rated Air Flow Rate {m3/s}",
	"  0.0033,                  !- Rated Water Flow Rate {m3/s}",
	"  19156.73,                !- Gross Rated Heating Capacity {W}",
	"  3.167053691,             !- Gross Rated Heating COP",
	"  -5.50102734,             !- Heating Capacity Coefficient 1",
	"  -0.96688754,             !- Heating Capacity Coefficient 2",
	"  7.70755007,              !- Heating Capacity Coefficient 3",
	"  0.031928881,             !- Heating Capacity Coefficient 4",
	"  0.028112522,             !- Heating Capacity Coefficient 5",
	"  -7.47517858,             !- Heating Power Consumption Coefficient 1",
	"  6.40876653,              !- Heating Power Consumption Coefficient 2",
	"  1.99711665,              !- Heating Power Consumption Coefficient 3",
	"  -0.050682973,            !- Heating Power Consumption Coefficient 4",
	"  0.011385145;             !- Heating Power Consumption Coefficient 5",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetSimpleWatertoAirHPInput();

	int HPNum( 1 );
	Real64 DesignAirflow( 2.0 );
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).Temp = 5.0;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).Enthalpy = 44650.0;

	SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate = 15.0;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRate = SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRateMax = SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRateMaxAvail = SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate;

	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).MassFlowRate = DesignAirflow;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).Temp = 26.0;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).HumRat = 0.007;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).Enthalpy = 43970.75;

	TotNumLoops = 2;
	PlantLoop.allocate( TotNumLoops );

	for( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 2 );
		auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}

	PlantLoop( 1 ).Name = "ChilledWaterLoop";
	PlantLoop( 1 ).FluidName = "ChilledWater";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = SimpleWatertoAirHP( HPNum ).Name;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = SimpleWatertoAirHP( HPNum ).WAHPPlantTypeOfNum;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = SimpleWatertoAirHP( HPNum ).WaterInletNodeNum;

	int CompOp( 1 );
	int CyclingScheme( 1 );
	bool FirstHVACIteration( true );
	Real64 MaxONOFFCyclesperHour( 4.0 );
	Real64 HPTimeConstant( 0.1 );
	Real64 FanDelayTime( 60.0 );
	Real64 SensLoad( 38000.0 );
	Real64 LatentLoad( 0.0 );
	Real64 PartLoadRatio( 1.0 );
	Real64 RuntimeFrac( 1.0 );
	Real64 OnOffAirFlowRatio( 1.0 );
	SimpleWatertoAirHP( HPNum ).LoopNum = 1;

	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPCoolingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 15.0 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 5.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 5.221888, 0.00001 );

	PartLoadRatio = 0.5;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPCoolingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 15.0 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 5.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 5.110941, 0.00001 );

	SimpleWatertoAirHP( HPNum ).WaterCyclingMode = WaterCycling;
	PartLoadRatio = 1.0;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPCoolingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 15.0 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 5.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 5.221888, 0.00001 );

	PartLoadRatio = 0.5;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPCoolingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 7.5 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 5.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 5.221888, 0.00001 );

	// test reduced flow at coil water inlet node
	PartLoadRatio = 0.25;
	Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRate = 3.75;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPCoolingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 3.75 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 5.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 5.221888, 0.00001 );
	UpdateSimpleWatertoAirHP( HPNum );
	EXPECT_EQ( Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRate, 3.75 );
	EXPECT_EQ( Node( SimpleWatertoAirHP( HPNum ).WaterOutletNodeNum ).MassFlowRate, 3.75 );
	EXPECT_NEAR( Node( SimpleWatertoAirHP( HPNum ).WaterOutletNodeNum ).Temp, 5.221888, 0.00001 );

	HPNum = 2;
	SimpleWatertoAirHP( HPNum ).LoopNum = 2;
	PlantLoop( 2 ).Name = "HotWaterLoop";
	PlantLoop( 2 ).FluidName = "HotWater";
	PlantLoop( 2 ).FluidIndex = 1;
	PlantLoop( 2 ).FluidName = "WATER";
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = SimpleWatertoAirHP( HPNum ).Name;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = SimpleWatertoAirHP( HPNum ).WAHPPlantTypeOfNum;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = SimpleWatertoAirHP( HPNum ).WaterInletNodeNum;

	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).Temp = 35.0;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).Enthalpy = 43950.0;

	SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate = 15.0;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRate = SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRateMax = SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRateMaxAvail = SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate;

	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).MassFlowRate = DesignAirflow;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).Temp = 15.0;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).HumRat = 0.004;
	DataLoopNode::Node( SimpleWatertoAirHP( HPNum ).AirInletNodeNum ).Enthalpy = PsyHFnTdbW( 15.0, 0.004 );

	SimpleWatertoAirHP( HPNum ).DesignWaterMassFlowRate = 15.0;

	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );

	PartLoadRatio = 1.0;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPHeatingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 15.0 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 35.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 34.514131, 0.00001 );

	PartLoadRatio = 0.5;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPHeatingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 15.0 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 35.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 34.757065, 0.00001 );

	SimpleWatertoAirHP( HPNum ).WaterCyclingMode = WaterCycling;
	PartLoadRatio = 1.0;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPHeatingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 15.0 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 35.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 34.514131, 0.00001 );

	PartLoadRatio = 0.5;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPHeatingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 7.5 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 35.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 34.514131, 0.00001 );

	// test reduced flow at coil water inlet node
	PartLoadRatio = 0.25;
	Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRate = 3.75;
	InitSimpleWatertoAirHP( HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio, FirstHVACIteration );
	CalcHPHeatingSimple( HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).WaterMassFlowRate, 3.75 );
	EXPECT_EQ( SimpleWatertoAirHP( HPNum ).InletWaterTemp, 35.0 );
	EXPECT_NEAR( SimpleWatertoAirHP( HPNum ).OutletWaterTemp, 34.514131, 0.00001 );
	UpdateSimpleWatertoAirHP( HPNum );
	EXPECT_EQ( Node( SimpleWatertoAirHP( HPNum ).WaterInletNodeNum ).MassFlowRate, 3.75 );
	EXPECT_EQ( Node( SimpleWatertoAirHP( HPNum ).WaterOutletNodeNum ).MassFlowRate, 3.75 );
	EXPECT_NEAR( Node( SimpleWatertoAirHP( HPNum ).WaterOutletNodeNum ).Temp, 34.514131, 0.00001 );

}
