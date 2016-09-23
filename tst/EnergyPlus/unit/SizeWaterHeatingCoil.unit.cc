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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <General.hh>
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace ObjexxFCL;
using namespace EnergyPlus;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::GlobalNames;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SimAirServingZones;
using namespace EnergyPlus::SingleDuct;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::WaterCoils;
using namespace EnergyPlus::ZoneAirLoopEquipmentManager;

namespace EnergyPlus {
	TEST_F( EnergyPlusFixture, TestSizingRoutineForHotWaterCoils1 ) {

		// Test whether Sys (VAV Terminal Unit) picks up 0.22 min air frac from Sizing:Zone input and also picks up max reheat flow rate
		//	set by the 0.4 max reheat fraction in Sizing:Zone

		bool ErrorsFound( false );

		InitializePsychRoutines();

		std::string const idf_objects = delimited_string( {
			"	Version,8.4;",
			"	Zone,",
			"	SPACE1-1, !- Name",
			"	0, !- Direction of Relative North { deg }",
			"	0, !- X Origin { m }",
			"	0, !- Y Origin { m }",
			"	0, !- Z Origin { m }",
			"	1, !- Type",
			"	1, !- Multiplier",
			"	2.438400269, !- Ceiling Height {m}",
			"	239.247360229; !- Volume {m3}",
			"	Sizing:Zone,",
			"	SPACE1-1, !- Zone or ZoneList Name",
			"	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
			"	14., !- Zone Cooling Design Supply Air Temperature { C }",
			"	, !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
			"	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
			"	50., !- Zone Heating Design Supply Air Temperature { C }",
			"	, !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
			"	0.009, !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
			"	0.004, !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
			"	SZ DSOA SPACE1-1, !- Design Specification Outdoor Air Object Name",
			"	0.0, !- Zone Heating Sizing Factor",
			"	0.0, !- Zone Cooling Sizing Factor",
			"	DesignDayWithLimit, !- Cooling Design Air Flow Method",
			"	, !- Cooling Design Air Flow Rate { m3/s }",
			"	, !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
			"	, !- Cooling Minimum Air Flow { m3/s }",
			"	, !- Cooling Minimum Air Flow Fraction",
			"	DesignDay, !- Heating Design Air Flow Method",
			"	, !- Heating Design Air Flow Rate { m3/s }",
			"	, !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
			"	, !- Heating Maximum Air Flow { m3/s }",
			"	, !- Heating Maximum Air Flow Fraction",
			"	SZ DZAD SPACE1-1;        !- Design Specification Zone Air Distribution Object Name",
			"	DesignSpecification:ZoneAirDistribution,",
			"	SZ DZAD SPACE1-1, !- Name",
			"	1, !- Zone Air Distribution Effectiveness in Cooling Mode { dimensionless }",
			"	1; !- Zone Air Distribution Effectiveness in Heating Mode { dimensionless }",
			"	DesignSpecification:OutdoorAir,",
			"	SZ DSOA SPACE1-1, !- Name",
			"	sum, !- Outdoor Air Method",
			"	0.00236, !- Outdoor Air Flow per Person { m3/s-person }",
			"	0.000305, !- Outdoor Air Flow per Zone Floor Area { m3/s-m2 }",
			"	0.0; !- Outdoor Air Flow per Zone { m3/s }",
			"	ScheduleTypeLimits,",
			"	Fraction, !- Name",
			"	0.0, !- Lower Limit Value",
			"	1.0, !- Upper Limit Value",
			"	CONTINUOUS; !- Numeric Type",
			"	Schedule:Compact,",
			"	ReheatCoilAvailSched, !- Name",
			"	Fraction, !- Schedule Type Limits Name",
			"	Through: 12/31, !- Field 1",
			"	For: AllDays, !- Field 2",
			"	Until: 24:00,1.0; !- Field 3",
			"	ZoneHVAC:EquipmentConnections,",
			"	SPACE1-1, !- Zone Name",
			"	SPACE1-1 Eq, !- Zone Conditioning Equipment List Name",
			"	SPACE1-1 In Node, !- Zone Air Inlet Node or NodeList Name",
			"	, !- Zone Air Exhaust Node or NodeList Name",
			"	SPACE1-1 Node, !- Zone Air Node Name",
			"	SPACE1-1 Out Node; !- Zone Return Air Node Name",
			"	ZoneHVAC:EquipmentList,",
			"	SPACE1-1 Eq, !- Name",
			"	ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
			"	SPACE1-1 ATU, !- Zone Equipment 1 Name",
			"	1, !- Zone Equipment 1 Cooling Sequence",
			"	1; !- Zone Equipment 1 Heating or No - Load Sequence",
			"	ZoneHVAC:AirDistributionUnit,",
			"	SPACE1-1 ATU, !- Name",
			"	SPACE1-1 In Node, !- Air Distribution Unit Outlet Node Name",
			"	AirTerminal:SingleDuct:VAV:Reheat, !- Air Terminal Object Type",
			"	SPACE1-1 VAV Reheat; !- Air Terminal Name",
			"	Coil:Heating:Water,",
			"	SPACE1-1 Zone Coil, !- Name",
			"	ReheatCoilAvailSched, !- Availability Schedule Name",
			"	, !- U-Factor Times Area Value { W/K }",
			"	, !- Maximum Water Flow Rate { m3/s }",
			"	SPACE1-1 Zone Coil Water In Node, !- Water Inlet Node Name",
			"	SPACE1-1 Zone Coil Water Out Node, !- Water Outlet Node Name",
			"	SPACE1-1 Zone Coil Air In Node, !- Air Inlet Node Name",
			"	SPACE1-1 In Node, !- Air Outlet Node Name",
			"	NominalCapacity, !- Performance Input Method",
			"	10000., !- Rated Capacity { W }",
			"	82.2, !- Rated Inlet Water Temperature { C }",
			"	16.6, !- Rated Inlet Air Temperature { C }",
			"	71.1, !- Rated Outlet Water Temperature { C }",
			"	32.2, !- Rated Outlet Air Temperature { C }",
			"	; !- Rated Ratio for Air and Water Convection",
			"	AirTerminal:SingleDuct:VAV:Reheat,",
			"	SPACE1-1 VAV Reheat, !- Name",
			"	ReheatCoilAvailSched, !- Availability Schedule Name",
			"	SPACE1-1 Zone Coil Air In Node, !- Damper Air Outlet Node Name",
			"	SPACE1-1 ATU In Node, !- Air Inlet Node Name",
			"	autosize, !- Maximum Air Flow Rate { m3/s }",
			"	, !- Zone Minimum Air Flow Input Method",
			"	, !- Constant Minimum Air Flow Fraction",
			"	, !- Fixed Minimum Air Flow Rate { m3/s }",
			"	, !- Minimum Air Flow Fraction Schedule Name",
			"	Coil:Heating:Water, !- Reheat Coil Object Type",
			"	SPACE1-1 Zone Coil, !- Reheat Coil Name",
			"	autosize, !- Maximum Hot Water or Steam Flow Rate { m3/s }",
			"	0.0, !- Minimum Hot Water or Steam Flow Rate { m3/s }",
			"	SPACE1-1 In Node, !- Air Outlet Node Name",
			"	0.001, !- Convergence Tolerance",
			"	, !- Damper Heating Action",
			"	, !- Maximum Flow per Zone Floor Area During Reheat { m3/s-m2 }",
			"	; !- Maximum Flow Fraction During Reheat",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		FinalZoneSizing.allocate( 2 );
		TermUnitFinalZoneSizing.allocate( 2 );
		CalcFinalZoneSizing.allocate( 2 );
		TermUnitSizing.allocate( 2 );
		TotNumLoops = 1;
		PlantLoop.allocate( TotNumLoops );
		PlantSizData.allocate( 1 );
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
		GetZoneData( ErrorsFound );
		EXPECT_EQ( "SPACE1-1", Zone( 1 ).Name );
		GetOARequirements(); // get the OA requirements object
		GetZoneAirDistribution(); // get zone air distribution objects
		GetZoneSizingInput();
		GetZoneEquipmentData1();
		ProcessScheduleInput();
		ScheduleInputProcessed = true;
		GetZoneAirLoopEquipment();
		GetSysInput();
		GetWaterCoilInput();
		WaterCoil( 1 ).WaterLoopNum = 1;
		WaterCoil( 1 ).WaterLoopSide = 1;
		WaterCoil( 1 ).WaterLoopBranchNum = 1;
		WaterCoil( 1 ).WaterLoopCompNum = 1;
		PlantLoop( 1 ).Name = "HotWaterLoop";
		PlantLoop( 1 ).FluidName = "HotWater";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_SimpleHeating;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = WaterCoil( 1 ).WaterOutletNodeNum;
		ZoneSizingRunDone = true;
		CurZoneEqNum = 2;
		Zone( 1 ).FloorArea = 99.16;
		TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.28794;
		TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.12046;
		FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.28794;
		FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.12046;
		FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlowFracUsInpFlg = ZoneSizingInput( CurZoneEqNum ).DesCoolMinAirFlowFracUsInpFlg;
		FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( CurZoneEqNum ).DesCoolMinAirFlowFrac;
		CalcFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.12046;
		CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.0;
		TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU = 16.6;
		TermUnitSizing( CurZoneEqNum ).AirVolFlow = 0.12046;
		CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 3191.7;
		CalcFinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak = 21.099;
		CalcFinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak = 0.0038485;
		PlantSizData( 1 ).DeltaT = 11.0;
		FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlow = ZoneSizingInput( CurZoneEqNum ).DesCoolMinAirFlow;
		FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( CurZoneEqNum ).DesCoolMinAirFlowFrac;
		FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlow2 = ZoneSizingInput( CurZoneEqNum ).DesCoolMinAirFlowPerArea * Zone( 1 ).FloorArea;
		FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlowMin = max( FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlow,
			FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlow2,
			FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow * FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlowFrac );
		EXPECT_FALSE( FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlowFracUsInpFlg );
		// EXPECT_DOUBLE_EQ( ZoneSizingInput( CurZoneEqNum ).DesCoolMinAirFlowPerArea, 0.000762 );
		// EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlow, 0.0 );
		// EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlowFrac, 0.2 );
		// EXPECT_NEAR( FinalZoneSizing( CurZoneEqNum ).DesCoolMinAirFlow2, .07351776, 0.000001 );
		// EXPECT_NEAR( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlowMin, .07351776, 0.000001 );
		FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlow = ZoneSizingInput( CurZoneEqNum ).DesHeatMaxAirFlow;
		FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( CurZoneEqNum ).DesHeatMaxAirFlowFrac;
		FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( CurZoneEqNum ).DesHeatMaxAirFlowPerArea;
		FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlow2 = ZoneSizingInput( CurZoneEqNum ).DesHeatMaxAirFlowPerArea * Zone( 1 ).FloorArea;
		FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlowMax = max( FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlow,
			FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlow2, max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow,
			FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow ) * FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlowFrac );
		// EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlow, 0.1415762 );
		// EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlowFrac, 0.3 );
		// EXPECT_DOUBLE_EQ( FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlowPerArea, .002032 );
		// EXPECT_NEAR( FinalZoneSizing( CurZoneEqNum ).DesHeatMaxAirFlow2, 0.196047, 0.000001 );
		Sys( 1 ).ZoneFloorArea = Zone( 1 ).FloorArea;
		SizeSys( 1 );

		Node.deallocate();
		ZoneEquipConfig.deallocate();
		Zone.deallocate();
		FinalZoneSizing.deallocate();
		TermUnitFinalZoneSizing.deallocate();
		CalcFinalZoneSizing.deallocate();
		TermUnitSizing.deallocate();
		Sys.deallocate();

	}

}
