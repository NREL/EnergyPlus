// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
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

// EnergyPlus::FanCoilUnits Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <General.hh>
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace ObjexxFCL;
using namespace EnergyPlus;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::FanCoilUnits;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::GlobalNames;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::WaterCoils;

using DataZoneEnergyDemands::ZoneSysEnergyDemand;
using DataZoneEnergyDemands::CurDeadBandOrSetback;
using DataHeatBalFanSys::TempControlType;
using General::TrimSigDigits;
using MixedAir::OAMixer;
using General::JulianDay;

namespace EnergyPlus {
	TEST_F( EnergyPlusFixture, MultiStage4PipeFanCoilHeatingTest ) {

		int FanCoilNum( 1 );
		int ZoneNum( 1 );
		bool FirstHVACIteration( false );
		bool ErrorsFound( false );
		Real64 PartLoadRatio( 1.0 );
		Real64 SpeedRatio( 0.0 );
		Real64 QZnReq( 0.0 );
		Real64 HotWaterMassFlowRate( 0.0 );
		Real64 ColdWaterMassFlowRate( 0.0 );
		Real64 QUnitOut( 0.0 );
		Real64 AirMassFlow( 0.0 );
		Real64 MaxAirMassFlow( 0.0 );

		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::StdRhoAir = 1.20;
		WaterCoils::GetWaterCoilsInputFlag = true;
		NumCoils = 0;
		DataGlobals::NumOfTimeStepInHour = 1;
		DataGlobals::TimeStep = 1;
		DataGlobals::MinutesPerTimeStep = 60;

		InitializePsychRoutines();

		std::string const idf_objects = delimited_string( {
			"	Version,8.3;",
			"	Zone,",
			"	EAST ZONE, !- Name",
			"	0, !- Direction of Relative North { deg }",
			"	0, !- X Origin { m }",
			"	0, !- Y Origin { m }",
			"	0, !- Z Origin { m }",
			"	1, !- Type",
			"	1, !- Multiplier",
			"	autocalculate, !- Ceiling Height { m }",
			"	autocalculate; !- Volume { m3 }",
			"	ZoneHVAC:EquipmentConnections,",
			"	EAST ZONE, !- Zone Name",
			"	Zone1Equipment, !- Zone Conditioning Equipment List Name",
			"	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
			"	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
			"	Zone 1 Node, !- Zone Air Node Name",
			"	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
			"	ZoneHVAC:EquipmentList,",
			"	Zone1Equipment, !- Name",
			"	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
			"	Zone1FanCoil, !- Zone Equipment 1 Name",
			"	1, !- Zone Equipment 1 Cooling Sequence",
			"	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
			"   NodeList,",
			"	Zone1Inlets, !- Name",
			"	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
			"	NodeList,",
			"	Zone1Exhausts, !- Name",
			"	Zone1FanCoilAirInletNode; !- Node 1 Name",
			"	OutdoorAir:NodeList,",
			"	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
			"	OutdoorAir:Mixer,",
			"	Zone1FanCoilOAMixer, !- Name",
			"	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
			"	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
			"	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
			"	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
			"	Schedule:Constant,",
			"	FanAndCoilAvailSched, !- Name",
			"	FRACTION, !- Schedule Type",
			"	1;        !- TimeStep Value",
			"	ScheduleTypeLimits,",
			"	Fraction, !- Name",
			"	0.0, !- Lower Limit Value",
			"	1.0, !- Upper Limit Value",
			"	CONTINUOUS;              !- Numeric Type",
			"   Fan:OnOff,",
			"	Zone1FanCoilFan, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	0.5, !- Fan Total Efficiency",
			"	75.0, !- Pressure Rise { Pa }",
			"	0.6, !- Maximum Flow Rate { m3 / s }",
			"	0.9, !- Motor Efficiency",
			"	1.0, !- Motor In Airstream Fraction",
			"	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
			"	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
			"	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
			"	Coil:Cooling:Water,",
			"	Zone1FanCoilCoolingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Namev",
			"	0.0002, !- Design Water Flow Rate { m3 / s }",
			"	0.5000, !- Design Air Flow Rate { m3 / s }",
			"	7.22,   !- Design Inlet Water Temperature { Cv }",
			"	24.340, !- Design Inlet Air Temperature { C }",
			"	14.000, !- Design Outlet Air Temperature { C }",
			"	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
			"	SimpleAnalysis, !- Type of Analysis",
			"	CrossFlow;               !- Heat Exchanger Configuration",
			"	Coil:Heating:Water,",
			"   Zone1FanCoilHeatingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	150.0,   !- U - Factor Times Area Value { W / K }",
			"	0.00014, !- Maximum Water Flow Rate { m3 / s }",
			"	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
			"	autosize, !- Rated Capacity { W }",
			"	82.2, !- Rated Inlet Water Temperature { C }",
			"	16.6, !- Rated Inlet Air Temperature { C }",
			"	71.1, !- Rated Outlet Water Temperature { C }",
			"	32.2, !- Rated Outlet Air Temperature { C }",
			"	;     !- Rated Ratio for Air and Water Convection",
			"	ZoneHVAC:FourPipeFanCoil,",
			"	Zone1FanCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	MultiSpeedFan, !- Capacity Control Method",
			"	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
			"	0.3, !- Low Speed Supply Air Flow Ratio",
			"	0.6, !- Medium Speed Supply Air Flow Ratio",
			"	0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
			"	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
			"	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
			"	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
			"	Fan:OnOff, !- Supply Air Fan Object Type",
			"	Zone1FanCoilFan, !- Supply Air Fan Name",
			"	Coil:Cooling:Water, !- Cooling Coil Object Type",
			"	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
			"	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
			"	0.001, !- Cooling Convergence Tolerance",
			"	Coil:Heating:Water, !- Heating Coil Object Type",
			"	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
			"	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
			"	0.001; !- Heating Convergence Tolerance",

		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetZoneData( ErrorsFound );
		EXPECT_EQ( "EAST ZONE", Zone( 1 ).Name );

		GetZoneEquipmentData1();
		ProcessScheduleInput();
		ScheduleInputProcessed = true;
		GetFanInput();
		EXPECT_EQ( DataHVACGlobals::FanType_SimpleOnOff, Fan( 1 ).FanType_Num );

		GetFanCoilUnits();
		EXPECT_EQ( "MULTISPEEDFAN", FanCoil( 1 ).CapCtrlMeth );
		EXPECT_EQ( "OUTDOORAIR:MIXER", FanCoil( 1 ).OAMixType );
		EXPECT_EQ( "FAN:ONOFF", FanCoil( 1 ).FanType );
		EXPECT_EQ( "COIL:COOLING:WATER", FanCoil( 1 ).CCoilType );
		EXPECT_EQ( "COIL:HEATING:WATER", FanCoil( 1 ).HCoilType );

		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );

		AirMassFlow = 0.60;
		MaxAirMassFlow = 0.60;
		// heating load only
		ColdWaterMassFlowRate = 0.0;
		HotWaterMassFlowRate = 1.0;

		Node( OAMixer( 1 ).RetNode ).MassFlowRate = AirMassFlow;
		Node( OAMixer( 1 ).RetNode ).MassFlowRateMax = MaxAirMassFlow;

		Node( OAMixer( 1 ).RetNode ).Temp = 22.0;
		Node( OAMixer( 1 ).RetNode ).Enthalpy = 36000;
		Node( OAMixer( 1 ).RetNode ).HumRat = PsyWFnTdbH( Node( OAMixer( 1 ).RetNode ).Temp, Node( OAMixer( 1 ).RetNode ).Enthalpy );

		Node( OAMixer( 1 ).InletNode ).Temp = 10.0;
		Node( OAMixer( 1 ).InletNode ).Enthalpy = 18000;
		Node( OAMixer( 1 ).InletNode ).HumRat = PsyWFnTdbH( Node( OAMixer( 1 ).InletNode ).Temp, Node( OAMixer( 1 ).InletNode ).Enthalpy );

		Node( FanCoil( 1 ).AirInNode ).MassFlowRate = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMin = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMinAvail = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMax = MaxAirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMaxAvail = MaxAirMassFlow;

		FanCoil( 1 ).OutAirMassFlow = 0.0;
		FanCoil( 1 ).MaxAirMassFlow = MaxAirMassFlow;
		Node( FanCoil( 1 ).OutsideAirNode ).MassFlowRateMax = 0.0;

		Fan( 1 ).InletAirMassFlowRate = AirMassFlow;
		Fan( 1 ).MaxAirMassFlowRate = MaxAirMassFlow;

		Node( Fan( 1 ).InletNodeNum ).MassFlowRate = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).UACoilTotal = 470.0;
		WaterCoil( 2 ).UACoilExternal = 611.0;
		WaterCoil( 2 ).UACoilInternal = 2010.0;
		WaterCoil( 2 ).TotCoilOutsideSurfArea = 50.0;

		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).InletWaterMassFlowRate = ColdWaterMassFlowRate;
		WaterCoil( 2 ).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).Temp = 6.0;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		Node( WaterCoil( 1 ).WaterInletNodeNum ).Temp = 60.0;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
		WaterCoil( 1 ).InletWaterMassFlowRate = HotWaterMassFlowRate;
		WaterCoil( 1 ).MaxWaterMassFlowRate = HotWaterMassFlowRate;

		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}

		WaterCoil( 2 ).WaterLoopNum = 1;
		WaterCoil( 2 ).WaterLoopSide = 1;
		WaterCoil( 2 ).WaterLoopBranchNum = 1;
		WaterCoil( 2 ).WaterLoopCompNum = 1;

		WaterCoil( 1 ).WaterLoopNum = 2;
		WaterCoil( 1 ).WaterLoopSide = 1;
		WaterCoil( 1 ).WaterLoopBranchNum = 1;
		WaterCoil( 1 ).WaterLoopCompNum = 1;

		PlantLoop( 2 ).Name = "ChilledWaterLoop";
		PlantLoop( 2 ).FluidName = "ChilledWater";
		PlantLoop( 2 ).FluidIndex = 1;
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 2 ).Name;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_Cooling;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 2 ).WaterInletNodeNum;

		PlantLoop( 1 ).Name = "HotWaterLoop";
		PlantLoop( 1 ).FluidName = "HotWater";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_SimpleHeating;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;

		CoolingLoad = false;
		HeatingLoad = true;
		ZoneSysEnergyDemand.allocate( 1 );
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = 0;
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 4000.0;
		FanCoil( 1 ).SpeedFanSel = 2;
		QUnitOut = 0.0;
		QZnReq = 4000.0;

		MyUAAndFlowCalcFlag.allocate( 2 );
		MyUAAndFlowCalcFlag( 1 ) = true;
		MyUAAndFlowCalcFlag( 2 ) = true;
		DataGlobals::DoingSizing = true;

		LocalTurnFansOff = false;
		LocalTurnFansOn = true;

		DataEnvironment::Month = 1;
		DataEnvironment::DayOfMonth = 21;
		DataGlobals::HourOfDay = 1;
		DataEnvironment::DSTIndicator = 0;
		DataEnvironment::DayOfWeek = 2;
		DataEnvironment::HolidayIndex = 0;
		DataEnvironment::DayOfYear_Schedule = JulianDay( Month, DayOfMonth, 1 );
		UpdateScheduleValues();

		CalcMultiStage4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut );

		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );

		DataGlobals::DoingSizing = false;

		PlantLoop.deallocate();
		ZoneSysEnergyDemand.deallocate();
		FanCoil.deallocate();
		Node.deallocate();
		WaterCoil.deallocate();
		ZoneEquipConfig.deallocate();
		Zone.deallocate();
		CoilNames.deallocate();
	}
	TEST_F( EnergyPlusFixture, MultiStage4PipeFanCoilCoolingTest ) {

		int FanCoilNum( 1 );
		int ZoneNum( 1 );
		bool FirstHVACIteration( false );
		bool ErrorsFound( false );
		Real64 PartLoadRatio( 1.0 );
		Real64 SpeedRatio( 0.0 );
		Real64 QZnReq( 0.0 );
		Real64 HotWaterMassFlowRate( 0.0 );
		Real64 ColdWaterMassFlowRate( 0.0 );
		Real64 QUnitOut( 0.0 );
		Real64 AirMassFlow( 0.0 );
		Real64 MaxAirMassFlow( 0.0 );

		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::StdRhoAir = 1.20;
		WaterCoils::GetWaterCoilsInputFlag = true;
		NumCoils = 0;
		DataGlobals::NumOfTimeStepInHour = 1;
		DataGlobals::TimeStep = 1;
		DataGlobals::MinutesPerTimeStep = 60;

		InitializePsychRoutines();

		std::string const idf_objects = delimited_string( {
			"	Version,8.3;",
			"	Zone,",
			"	EAST ZONE, !- Name",
			"	0, !- Direction of Relative North { deg }",
			"	0, !- X Origin { m }",
			"	0, !- Y Origin { m }",
			"	0, !- Z Origin { m }",
			"	1, !- Type",
			"	1, !- Multiplier",
			"	autocalculate, !- Ceiling Height { m }",
			"	autocalculate; !- Volume { m3 }",
			"	ZoneHVAC:EquipmentConnections,",
			"	EAST ZONE, !- Zone Name",
			"	Zone1Equipment, !- Zone Conditioning Equipment List Name",
			"	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
			"	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
			"	Zone 1 Node, !- Zone Air Node Name",
			"	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
			"	ZoneHVAC:EquipmentList,",
			"	Zone1Equipment, !- Name",
			"	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
			"	Zone1FanCoil, !- Zone Equipment 1 Name",
			"	1, !- Zone Equipment 1 Cooling Sequence",
			"	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
			"   NodeList,",
			"	Zone1Inlets, !- Name",
			"	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
			"	NodeList,",
			"	Zone1Exhausts, !- Name",
			"	Zone1FanCoilAirInletNode; !- Node 1 Name",
			"	OutdoorAir:NodeList,",
			"	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
			"	OutdoorAir:Mixer,",
			"	Zone1FanCoilOAMixer, !- Name",
			"	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
			"	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
			"	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
			"	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
			"	Schedule:Compact,",
			"	FanAndCoilAvailSched, !- Name",
			"	Fraction, !- Schedule Type Limits Name",
			"	Through: 12/31, !- Field 1",
			"	For: AllDays, !- Field 2",
			"	Until: 24:00, 1.0;        !- Field 3",
			"	ScheduleTypeLimits,",
			"	Fraction, !- Name",
			"	0.0, !- Lower Limit Value",
			"	1.0, !- Upper Limit Value",
			"	CONTINUOUS;              !- Numeric Type",
			"   Fan:OnOff,",
			"	Zone1FanCoilFan, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	0.5, !- Fan Total Efficiency",
			"	75.0, !- Pressure Rise { Pa }",
			"	0.6, !- Maximum Flow Rate { m3 / s }",
			"	0.9, !- Motor Efficiency",
			"	1.0, !- Motor In Airstream Fraction",
			"	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
			"	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
			"	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
			"	Coil:Cooling:Water,",
			"	Zone1FanCoilCoolingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Namev",
			"	0.0002, !- Design Water Flow Rate { m3 / s }",
			"	0.5000, !- Design Air Flow Rate { m3 / s }",
			"	7.22,   !- Design Inlet Water Temperature { Cv }",
			"	24.340, !- Design Inlet Air Temperature { C }",
			"	14.000, !- Design Outlet Air Temperature { C }",
			"	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
			"	SimpleAnalysis, !- Type of Analysis",
			"	CrossFlow;               !- Heat Exchanger Configuration",
			"	Coil:Heating:Water,",
			"   Zone1FanCoilHeatingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	150.0,   !- U - Factor Times Area Value { W / K }",
			"	0.00014, !- Maximum Water Flow Rate { m3 / s }",
			"	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
			"	autosize, !- Rated Capacity { W }",
			"	82.2, !- Rated Inlet Water Temperature { C }",
			"	16.6, !- Rated Inlet Air Temperature { C }",
			"	71.1, !- Rated Outlet Water Temperature { C }",
			"	32.2, !- Rated Outlet Air Temperature { C }",
			"	;     !- Rated Ratio for Air and Water Convection",
			"	ZoneHVAC:FourPipeFanCoil,",
			"	Zone1FanCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	MultiSpeedFan, !- Capacity Control Method",
			"	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
			"	0.3, !- Low Speed Supply Air Flow Ratio",
			"	0.6, !- Medium Speed Supply Air Flow Ratio",
			"	0.1, !- Maximum Outdoor Air Flow Rate { m3 / s }",
			"	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
			"	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
			"	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
			"	Fan:OnOff, !- Supply Air Fan Object Type",
			"	Zone1FanCoilFan, !- Supply Air Fan Name",
			"	Coil:Cooling:Water, !- Cooling Coil Object Type",
			"	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
			"	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
			"	0.001, !- Cooling Convergence Tolerance",
			"	Coil:Heating:Water, !- Heating Coil Object Type",
			"	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
			"	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
			"	0.001; !- Heating Convergence Tolerance",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetZoneData( ErrorsFound );
		EXPECT_EQ( "EAST ZONE", Zone( 1 ).Name );

		GetZoneEquipmentData1();
		ProcessScheduleInput();
		ScheduleInputProcessed = true;
		GetFanInput();
		EXPECT_EQ( DataHVACGlobals::FanType_SimpleOnOff, Fan( 1 ).FanType_Num );

		GetFanCoilUnits();
		EXPECT_EQ( "MULTISPEEDFAN", FanCoil( 1 ).CapCtrlMeth );
		EXPECT_EQ( "OUTDOORAIR:MIXER", FanCoil( 1 ).OAMixType );
		EXPECT_EQ( "FAN:ONOFF", FanCoil( 1 ).FanType );
		EXPECT_EQ( "COIL:COOLING:WATER", FanCoil( 1 ).CCoilType );
		EXPECT_EQ( "COIL:HEATING:WATER", FanCoil( 1 ).HCoilType );

		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );

		AirMassFlow = 0.60;
		MaxAirMassFlow = 0.60;

		// cooling load only
		HotWaterMassFlowRate = 0.0;
		ColdWaterMassFlowRate = 1.0;

		Node( OAMixer( 1 ).RetNode ).MassFlowRate = AirMassFlow;
		Node( OAMixer( 1 ).RetNode ).MassFlowRateMax = MaxAirMassFlow;

		Node( OAMixer( 1 ).RetNode ).Temp = 24.0;
		Node( OAMixer( 1 ).RetNode ).Enthalpy = 36000;
		Node( OAMixer( 1 ).RetNode ).HumRat = PsyWFnTdbH( Node( OAMixer( 1 ).RetNode ).Temp, Node( OAMixer( 1 ).RetNode ).Enthalpy );

		Node( OAMixer( 1 ).InletNode ).Temp = 30.0;
		Node( OAMixer( 1 ).InletNode ).Enthalpy = 53000;
		Node( OAMixer( 1 ).InletNode ).HumRat = PsyWFnTdbH( Node( OAMixer( 1 ).InletNode ).Temp, Node( OAMixer( 1 ).InletNode ).Enthalpy );

		Node( FanCoil( 1 ).AirInNode ).MassFlowRate = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMin = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMinAvail = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMax = MaxAirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMaxAvail = MaxAirMassFlow;

		FanCoil( 1 ).OutAirMassFlow = 0.0;
		FanCoil( 1 ).MaxAirMassFlow = MaxAirMassFlow;
		Node( FanCoil( 1 ).OutsideAirNode ).MassFlowRateMax = 0.0;

		Fan( 1 ).InletAirMassFlowRate = AirMassFlow;
		Fan( 1 ).MaxAirMassFlowRate = MaxAirMassFlow;

		Node( Fan( 1 ).InletNodeNum ).MassFlowRate = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).UACoilTotal = 470.0;
		WaterCoil( 2 ).UACoilExternal = 611.0;
		WaterCoil( 2 ).UACoilInternal = 2010.0;
		WaterCoil( 2 ).TotCoilOutsideSurfArea = 50.0;

		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).InletWaterMassFlowRate = ColdWaterMassFlowRate;
		WaterCoil( 2 ).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).Temp = 6.0;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		Node( WaterCoil( 1 ).WaterInletNodeNum ).Temp = 60.0;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
		WaterCoil( 1 ).InletWaterMassFlowRate = HotWaterMassFlowRate;
		WaterCoil( 1 ).MaxWaterMassFlowRate = HotWaterMassFlowRate;

		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}

		WaterCoil( 2 ).WaterLoopNum = 1;
		WaterCoil( 2 ).WaterLoopSide = 1;
		WaterCoil( 2 ).WaterLoopBranchNum = 1;
		WaterCoil( 2 ).WaterLoopCompNum = 1;

		WaterCoil( 1 ).WaterLoopNum = 2;
		WaterCoil( 1 ).WaterLoopSide = 1;
		WaterCoil( 1 ).WaterLoopBranchNum = 1;
		WaterCoil( 1 ).WaterLoopCompNum = 1;

		PlantLoop( 2 ).Name = "ChilledWaterLoop";
		PlantLoop( 2 ).FluidName = "ChilledWater";
		PlantLoop( 2 ).FluidIndex = 1;
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 2 ).Name;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_Cooling;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 2 ).WaterInletNodeNum;

		PlantLoop( 1 ).Name = "HotWaterLoop";
		PlantLoop( 1 ).FluidName = "HotWater";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_SimpleHeating;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;

		HeatingLoad = false;
		CoolingLoad = true;
		ZoneSysEnergyDemand.allocate( 1 );
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -4000.00;
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 0.0;
		FanCoil( 1 ).SpeedFanSel = 2;
		QUnitOut = 0.0;
		QZnReq = -4000.0;

		MyUAAndFlowCalcFlag.allocate( 2 );
		MyUAAndFlowCalcFlag( 1 ) = true;
		MyUAAndFlowCalcFlag( 2 ) = true;
		DataGlobals::DoingSizing = true;

		LocalTurnFansOff = false;
		LocalTurnFansOn = true;

		DataEnvironment::Month = 1;
		DataEnvironment::DayOfMonth = 21;
		DataGlobals::HourOfDay = 1;
		DataEnvironment::DSTIndicator = 0;
		DataEnvironment::DayOfWeek = 2;
		DataEnvironment::HolidayIndex = 0;
		DataEnvironment::DayOfYear_Schedule = JulianDay( Month, DayOfMonth, 1 );
		UpdateScheduleValues();

		CalcMultiStage4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut );

		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );

		DataGlobals::DoingSizing = false;
		PlantLoop.deallocate();
		ZoneSysEnergyDemand.deallocate();
		FanCoil.deallocate();
		Node.deallocate();
		WaterCoil.deallocate();
		ZoneEquipConfig.deallocate();
		Zone.deallocate();
		CoilNames.deallocate();

	}

	TEST_F( EnergyPlusFixture, FanCoil_ASHRAE90VariableFan ) {


		int FanCoilNum( 1 );
		int ZoneNum( 1 );
		bool FirstHVACIteration( false );
		bool ErrorsFound( false );
		Real64 QZnReq( 0.0 );
		Real64 HotWaterMassFlowRate( 0.0 );
		Real64 ColdWaterMassFlowRate( 0.0 );
		Real64 QUnitOut( 0.0 );
		Real64 QLatOut( 0.0 );
		Real64 AirMassFlow( 0.0 );
		Real64 MaxAirMassFlow( 0.0 );

		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::StdRhoAir = 1.20;
		WaterCoils::GetWaterCoilsInputFlag = true;
		NumCoils = 0;
		DataGlobals::NumOfTimeStepInHour = 1;
		DataGlobals::TimeStep = 1;
		DataGlobals::MinutesPerTimeStep = 60;
		DataSizing::CurZoneEqNum = 1;

		InitializePsychRoutines();

		std::string const idf_objects = delimited_string( {
			"	Version,8.3;",
			"	Zone,",
			"	EAST ZONE, !- Name",
			"	0, !- Direction of Relative North { deg }",
			"	0, !- X Origin { m }",
			"	0, !- Y Origin { m }",
			"	0, !- Z Origin { m }",
			"	1, !- Type",
			"	1, !- Multiplier",
			"	autocalculate, !- Ceiling Height { m }",
			"	autocalculate; !- Volume { m3 }",
			"	ZoneHVAC:EquipmentConnections,",
			"	EAST ZONE, !- Zone Name",
			"	Zone1Equipment, !- Zone Conditioning Equipment List Name",
			"	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
			"	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
			"	Zone 1 Node, !- Zone Air Node Name",
			"	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
			"	ZoneHVAC:EquipmentList,",
			"	Zone1Equipment, !- Name",
			"	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
			"	Zone1FanCoil, !- Zone Equipment 1 Name",
			"	1, !- Zone Equipment 1 Cooling Sequence",
			"	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
			"   NodeList,",
			"	Zone1Inlets, !- Name",
			"	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
			"	NodeList,",
			"	Zone1Exhausts, !- Name",
			"	Zone1FanCoilAirInletNode; !- Node 1 Name",
			"	OutdoorAir:NodeList,",
			"	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
			"	OutdoorAir:Mixer,",
			"	Zone1FanCoilOAMixer, !- Name",
			"	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
			"	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
			"	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
			"	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
			"	Schedule:Constant,",
			"	FanAndCoilAvailSched, !- Name",
			"	FRACTION, !- Schedule Type",
			"	1;        !- TimeStep Value",
			"	ScheduleTypeLimits,",
			"	Fraction, !- Name",
			"	0.0, !- Lower Limit Value",
			"	1.0, !- Upper Limit Value",
			"	CONTINUOUS;              !- Numeric Type",
			"   Fan:OnOff,",
			"	Zone1FanCoilFan, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	0.5, !- Fan Total Efficiency",
			"	75.0, !- Pressure Rise { Pa }",
			"	0.6, !- Maximum Flow Rate { m3 / s }",
			"	0.9, !- Motor Efficiency",
			"	1.0, !- Motor In Airstream Fraction",
			"	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
			"	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
			"	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
			"	Coil:Cooling:Water,",
			"	Zone1FanCoilCoolingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Namev",
			"	0.0002, !- Design Water Flow Rate { m3 / s }",
			"	0.5000, !- Design Air Flow Rate { m3 / s }",
			"	7.22,   !- Design Inlet Water Temperature { Cv }",
			"	24.340, !- Design Inlet Air Temperature { C }",
			"	14.000, !- Design Outlet Air Temperature { C }",
			"	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
			"	SimpleAnalysis, !- Type of Analysis",
			"	CrossFlow;               !- Heat Exchanger Configuration",
			"	Coil:Heating:Water,",
			"   Zone1FanCoilHeatingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	150.0,   !- U - Factor Times Area Value { W / K }",
			"	0.00014, !- Maximum Water Flow Rate { m3 / s }",
			"	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
			"	autosize, !- Rated Capacity { W }",
			"	82.2, !- Rated Inlet Water Temperature { C }",
			"	16.6, !- Rated Inlet Air Temperature { C }",
			"	71.1, !- Rated Outlet Water Temperature { C }",
			"	32.2, !- Rated Outlet Air Temperature { C }",
			"	;     !- Rated Ratio for Air and Water Convection",
			"	ZoneHVAC:FourPipeFanCoil,",
			"	Zone1FanCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	ASHRAE90VariableFan, !- Capacity Control Method",
			"	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
			"	0.3, !- Low Speed Supply Air Flow Ratio",
			"	0.6, !- Medium Speed Supply Air Flow Ratio",
			"	0.0, !- Maximum Outdoor Air Flow Rate { m3 / s }",
			"	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
			"	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
			"	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
			"	Fan:OnOff, !- Supply Air Fan Object Type",
			"	Zone1FanCoilFan, !- Supply Air Fan Name",
			"	Coil:Cooling:Water, !- Cooling Coil Object Type",
			"	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
			"	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
			"	0.001, !- Cooling Convergence Tolerance",
			"	Coil:Heating:Water, !- Heating Coil Object Type",
			"	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
			"	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
			"	0.001; !- Heating Convergence Tolerance",

		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetZoneData( ErrorsFound );
		EXPECT_EQ( "EAST ZONE", Zone( 1 ).Name );

		GetZoneEquipmentData1();
		ProcessScheduleInput();
		ScheduleInputProcessed = true;
		GetFanInput();
		EXPECT_EQ( DataHVACGlobals::FanType_SimpleOnOff, Fan( 1 ).FanType_Num );

		GetFanCoilUnits();
		EXPECT_EQ( "ASHRAE90VARIABLEFAN", FanCoil( 1 ).CapCtrlMeth );
		EXPECT_EQ( "OUTDOORAIR:MIXER", FanCoil( 1 ).OAMixType );
		EXPECT_EQ( "FAN:ONOFF", FanCoil( 1 ).FanType );
		EXPECT_EQ( "COIL:COOLING:WATER", FanCoil( 1 ).CCoilType );
		EXPECT_EQ( "COIL:HEATING:WATER", FanCoil( 1 ).HCoilType );

		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );

		AirMassFlow = 0.60;
		MaxAirMassFlow = 0.60;
		// heating load only
		ColdWaterMassFlowRate = 0.0;
		HotWaterMassFlowRate = 1.0;

		Node( OAMixer( 1 ).RetNode ).MassFlowRate = AirMassFlow;
		Node( OAMixer( 1 ).RetNode ).MassFlowRateMax = MaxAirMassFlow;

		Node( OAMixer( 1 ).RetNode ).Temp = 22.0;
		Node( OAMixer( 1 ).RetNode ).Enthalpy = 36000;
		Node( OAMixer( 1 ).RetNode ).HumRat = PsyWFnTdbH( Node( OAMixer( 1 ).RetNode ).Temp, Node( OAMixer( 1 ).RetNode ).Enthalpy );

		Node( OAMixer( 1 ).InletNode ).Temp = 10.0;
		Node( OAMixer( 1 ).InletNode ).Enthalpy = 18000;
		Node( OAMixer( 1 ).InletNode ).HumRat = PsyWFnTdbH( Node( OAMixer( 1 ).InletNode ).Temp, Node( OAMixer( 1 ).InletNode ).Enthalpy );

		Node( FanCoil( 1 ).AirInNode ).MassFlowRate = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMin = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMinAvail = AirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMax = MaxAirMassFlow;
		Node( FanCoil( 1 ).AirInNode ).MassFlowRateMaxAvail = MaxAirMassFlow;

		FanCoil( 1 ).OutAirMassFlow = 0.0;
		FanCoil( 1 ).MaxAirMassFlow = MaxAirMassFlow;
		Node( FanCoil( 1 ).OutsideAirNode ).MassFlowRateMax = 0.0;

		Fan( 1 ).InletAirMassFlowRate = AirMassFlow;
		Fan( 1 ).MaxAirMassFlowRate = MaxAirMassFlow;

		Node( Fan( 1 ).InletNodeNum ).MassFlowRate = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).UACoilTotal = 470.0;
		WaterCoil( 2 ).UACoilExternal = 611.0;
		WaterCoil( 2 ).UACoilInternal = 2010.0;
		WaterCoil( 2 ).TotCoilOutsideSurfArea = 50.0;

		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).InletWaterMassFlowRate = ColdWaterMassFlowRate;
		WaterCoil( 2 ).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).Temp = 6.0;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		Node( WaterCoil( 1 ).WaterInletNodeNum ).Temp = 60.0;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRate = HotWaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = HotWaterMassFlowRate;
		WaterCoil( 1 ).InletWaterMassFlowRate = HotWaterMassFlowRate;
		WaterCoil( 1 ).MaxWaterMassFlowRate = HotWaterMassFlowRate;

		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}

		WaterCoil( 2 ).WaterLoopNum = 1;
		WaterCoil( 2 ).WaterLoopSide = 1;
		WaterCoil( 2 ).WaterLoopBranchNum = 1;
		WaterCoil( 2 ).WaterLoopCompNum = 1;

		WaterCoil( 1 ).WaterLoopNum = 2;
		WaterCoil( 1 ).WaterLoopSide = 1;
		WaterCoil( 1 ).WaterLoopBranchNum = 1;
		WaterCoil( 1 ).WaterLoopCompNum = 1;

		PlantLoop( 2 ).Name = "ChilledWaterLoop";
		PlantLoop( 2 ).FluidName = "ChilledWater";
		PlantLoop( 2 ).FluidIndex = 1;
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 2 ).Name;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_Cooling;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 2 ).WaterInletNodeNum;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = WaterCoil( 2 ).WaterOutletNodeNum;

		PlantLoop( 1 ).Name = "HotWaterLoop";
		PlantLoop( 1 ).FluidName = "HotWater";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_SimpleHeating;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = WaterCoil( 1 ).WaterOutletNodeNum;

		CoolingLoad = false;
		HeatingLoad = true;
		ZoneSysEnergyDemand.allocate( 1 );
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = 0;
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 4000.0;
		FanCoil( 1 ).SpeedFanSel = 2;
		QUnitOut = 0.0;
		QLatOut = 0.0;
		QZnReq = 4000.0;

		MyUAAndFlowCalcFlag.allocate( 2 );
		MyUAAndFlowCalcFlag( 1 ) = true;
		MyUAAndFlowCalcFlag( 2 ) = true;
		DataGlobals::DoingSizing = true;

		LocalTurnFansOff = false;
		LocalTurnFansOn = true;

		DataEnvironment::Month = 1;
		DataEnvironment::DayOfMonth = 21;
		DataGlobals::HourOfDay = 1;
		DataEnvironment::DSTIndicator = 0;
		DataEnvironment::DayOfWeek = 2;
		DataEnvironment::HolidayIndex = 0;
		DataEnvironment::DayOfYear_Schedule = JulianDay( Month, DayOfMonth, 1 );
		UpdateScheduleValues();

		ZoneEqSizing.allocate( 1 );
		CurDeadBandOrSetback.allocate( 1 );
		CurDeadBandOrSetback( 1 ) = false;
		TempControlType.allocate( 1 );
		TempControlType( 1 ) = 4;
		ZoneSizingRunDone = true;
		FinalZoneSizing.allocate( 1 );
		FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.5;
		FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.5;
		FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 30.0;
		FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.01;
		FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTemp = 20.0;
		FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRat = 0.005;
		FinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 4000.0;
		FinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 4000.0;
		StdRhoAir = 1.2;

		BeginEnvrnFlag = true;
		InitFanCoilUnits( FanCoilNum, ZoneNum );
		Sim4PipeFanCoil( FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut );

		// expect full flow and meet capacity
		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );
		EXPECT_NEAR( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow, 0.0000000001 );

		// expect minimum flow and meet capacity
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 1000.0;
		QZnReq = 1000.0;
		Sim4PipeFanCoil( FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut );
		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );
		EXPECT_NEAR( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow * FanCoil( 1 ).LowSpeedRatio, 0.0000000001 );

		// expect modulated flow and meet capacity
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 2500.0;
		QZnReq = 2500.0;
		Sim4PipeFanCoil( FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut );
		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );
		EXPECT_GT( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow * FanCoil( 1 ).LowSpeedRatio );
		EXPECT_LT( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow );

		// expect full flow and meet capacity
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 0.0;
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -4000.0;
		QZnReq = -4000.0;
		Sim4PipeFanCoil( FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut );
		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );
		EXPECT_NEAR( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow, 0.0000000001 );

		// expect full flow and meet capacity
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 0.0;
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -4255.0;
		QZnReq = -4255.0;
		Sim4PipeFanCoil( FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut );
		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );
		EXPECT_NEAR( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow, 0.0000000001 );

		// expect minimum flow and meet capacity
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -1000.0;
		QZnReq = -1000.0;
		Sim4PipeFanCoil( FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut );
		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );
		EXPECT_NEAR( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow * FanCoil( 1 ).LowSpeedRatio, 0.0000000001 );

		// expect modulated flow and meet capacity
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -2500.0;
		QZnReq = -2500.0;
		Sim4PipeFanCoil( FanCoilNum, ZoneNum, ZoneNum, FirstHVACIteration, QUnitOut, QLatOut );
		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );
		EXPECT_GT( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow * FanCoil( 1 ).LowSpeedRatio );
		EXPECT_LT( Node( 1 ).MassFlowRate, FanCoil( 1 ).MaxAirMassFlow );

		DataGlobals::DoingSizing = false;

		PlantLoop.deallocate();
		ZoneSysEnergyDemand.deallocate();
		FanCoil.deallocate();
		Node.deallocate();
		WaterCoil.deallocate();
		ZoneEquipConfig.deallocate();
		Zone.deallocate();
		CoilNames.deallocate();

	}

}
