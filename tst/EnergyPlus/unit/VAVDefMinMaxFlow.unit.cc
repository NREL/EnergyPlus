// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <General.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace ObjexxFCL;
using namespace EnergyPlus;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::GlobalNames;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SimAirServingZones;
using namespace EnergyPlus::SingleDuct;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::ZoneAirLoopEquipmentManager;

namespace EnergyPlus {
TEST_F(EnergyPlusFixture, VAVDefMinMaxFlowTestVentEffLimit)
{

    Real64 ZoneOAFrac;   // ratio of outdoor air (divided by distribution effectiveness) to available supply air
    Real64 VozClg;       //   outdoor air flow divided by distribution effectiveness [m3/s]
    Real64 Xs;           // ratio of uncorrected system outdoor air flow rate to the design system supply flow rate
    Real64 SysCoolingEv; // system ventilation effectiveness
    int CtrlZoneNum;     // controlled zone number

    TermUnitFinalZoneSizing.allocate(2);

    Xs = 0.2516;
    ZoneOAFrac = 0.8265;
    VozClg = .06245;
    SysCoolingEv = 1.0 + Xs - ZoneOAFrac;
    CtrlZoneNum = 1;
    TermUnitFinalZoneSizing(CtrlZoneNum).ZoneVentilationEff = 0.7;
    LimitZoneVentEff(Xs, VozClg, CtrlZoneNum, SysCoolingEv);
    EXPECT_DOUBLE_EQ(0.7, SysCoolingEv);
    EXPECT_NEAR(0.5516, TermUnitFinalZoneSizing(CtrlZoneNum).ZpzClgByZone, 0.0001);
    EXPECT_NEAR(0.1132, TermUnitFinalZoneSizing(CtrlZoneNum).DesCoolVolFlowMin, 0.0001);
    ZoneOAFrac = 0.4894;
    VozClg = 0.02759;
    SysCoolingEv = 1.0 + Xs - ZoneOAFrac;
    CtrlZoneNum = 2;
    TermUnitFinalZoneSizing(CtrlZoneNum).ZoneVentilationEff = 0.7;
    LimitZoneVentEff(Xs, VozClg, CtrlZoneNum, SysCoolingEv);
    EXPECT_NEAR(0.7622, SysCoolingEv, .0001);

    TermUnitFinalZoneSizing.deallocate();
}
TEST_F(EnergyPlusFixture, VAVDefMinMaxFlowTestSizing1)
{

    // Test whether Sys (VAV Terminal Unit) picks up 0.22 min air frac from Sizing:Zone input and also picks up max reheat flow rate
    //	set by the 0.4 max reheat fraction in Sizing:Zone

    bool ErrorsFound(false);

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.4;",
        "	Zone,",
        "	SPACE3-1, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	2.438400269, !- Ceiling Height {m}",
        "	239.247360229; !- Volume {m3}",
        "	Sizing:Zone,",
        "	SPACE3-1, !- Zone or ZoneList Name",
        "	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
        "	14., !- Zone Cooling Design Supply Air Temperature { C }",
        "	, !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
        "	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
        "	50., !- Zone Heating Design Supply Air Temperature { C }",
        "	, !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
        "	0.009, !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	0.004, !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	SZ DSOA SPACE3-1, !- Design Specification Outdoor Air Object Name",
        "	0.0, !- Zone Heating Sizing Factor",
        "	0.0, !- Zone Cooling Sizing Factor",
        "	DesignDayWithLimit, !- Cooling Design Air Flow Method",
        "	, !- Cooling Design Air Flow Rate { m3/s }",
        "	0.0, !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	, !- Cooling Minimum Air Flow { m3/s }",
        "	0.22, !- Cooling Minimum Air Flow Fraction",
        "	DesignDay, !- Heating Design Air Flow Method",
        "	, !- Heating Design Air Flow Rate { m3/s }",
        "	, !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	, !- Heating Maximum Air Flow { m3/s }",
        "	0.4, !- Heating Maximum Air Flow Fraction",
        "	SZ DZAD SPACE3-1;        !- Design Specification Zone Air Distribution Object Name",
        "	DesignSpecification:ZoneAirDistribution,",
        "	SZ DZAD SPACE3-1, !- Name",
        "	1, !- Zone Air Distribution Effectiveness in Cooling Mode { dimensionless }",
        "	1; !- Zone Air Distribution Effectiveness in Heating Mode { dimensionless }",
        "	DesignSpecification:OutdoorAir,",
        "	SZ DSOA SPACE3-1, !- Name",
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
        "	SPACE3-1, !- Zone Name",
        "	SPACE3-1 Eq, !- Zone Conditioning Equipment List Name",
        "	SPACE3-1 In Node, !- Zone Air Inlet Node or NodeList Name",
        "	, !- Zone Air Exhaust Node or NodeList Name",
        "	SPACE3-1 Node, !- Zone Air Node Name",
        "	SPACE3-1 Out Node; !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	SPACE3-1 Eq, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
        "	SPACE3-1 ATU, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1; !- Zone Equipment 1 Heating or No - Load Sequence",
        "	ZoneHVAC:AirDistributionUnit,",
        "	SPACE3-1 ATU, !- Name",
        "	SPACE3-1 In Node, !- Air Distribution Unit Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat, !- Air Terminal Object Type",
        "	SPACE3-1 VAV Reheat; !- Air Terminal Name",
        "	Coil:Heating:Fuel,",
        "	SPACE3-1 Zone Coil, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "   NaturalGas,  !- Fuel Type",
        "	0.8, !- Burner Efficiency",
        "	1000, ! Nominal Capacity",
        "	SPACE3-1 Zone Coil Air In Node, !- Air Inlet Node Name",
        "	SPACE3-1 In Node; !- Air Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat,",
        "	SPACE3-1 VAV Reheat, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "	SPACE3-1 Zone Coil Air In Node, !- Damper Air Outlet Node Name",
        "	SPACE3-1 ATU In Node, !- Air Inlet Node Name",
        "	autosize, !- Maximum Air Flow Rate { m3/s }",
        "	, !- Zone Minimum Air Flow Input Method",
        "	, !- Constant Minimum Air Flow Fraction",
        "	, !- Fixed Minimum Air Flow Rate { m3/s }",
        "	, !- Minimum Air Flow Fraction Schedule Name",
        "	Coil:Heating:Fuel, !- Reheat Coil Object Type",
        "	SPACE3-1 Zone Coil, !- Reheat Coil Name",
        "	, !- Maximum Hot Water or Steam Flow Rate { m3/s }",
        "	, !- Minimum Hot Water or Steam Flow Rate { m3/s }",
        "	SPACE3-1 In Node, !- Air Outlet Node Name",
        "	0.001, !- Convergence Tolerance",
        "	ReverseWithLimits, !- Damper Heating Action",
        "	, !- Maximum Flow per Zone Floor Area During Reheat { m3/s-m2 }",
        "	; !- Maximum Flow Fraction During Reheat",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    FinalZoneSizing.allocate(1);
    NumAirTerminalSizingSpec = 1;
    TermUnitFinalZoneSizing.allocate(1);
    CalcFinalZoneSizing.allocate(1);
    TermUnitSizing.allocate(1);
    GetZoneData(ErrorsFound);
    EXPECT_EQ("SPACE3-1", Zone(1).Name);
    GetOARequirements();      // get the OA requirements object
    GetZoneAirDistribution(); // get zone air distribution objects
    GetZoneSizingInput();
    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetZoneAirLoopEquipment();
    GetSysInput();
    ZoneSizingRunDone = true;
    CurZoneEqNum = 1;
    CurTermUnitSizingNum = 1;
    Zone(1).FloorArea = 96.48;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.21081;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    CalcFinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    CalcFinalZoneSizing(CurZoneEqNum).HeatSizingFactor = 1.0;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin =
        max(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2,
            FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow * FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac);
    EXPECT_DOUBLE_EQ(ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac, 0.22);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2, 0.0, 0.000001);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin, 0.22 * 0.21081, 0.000001);
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlowMax =
        max(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2,
            max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) *
                FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac, 0.4);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2, 0.0);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesHeatVolFlowMax, 0.084324, 0.000001);
    Sys(1).ZoneFloorArea = Zone(1).FloorArea;
    UpdateTermUnitFinalZoneSizing(); // Fills the TermUnitFinalZoneSizing array
    SizeSys(1);
    EXPECT_DOUBLE_EQ(Sys(CurZoneEqNum).ZoneMinAirFrac, 0.22);
    EXPECT_NEAR(Sys(CurZoneEqNum).MaxAirVolFlowRateDuringReheat, 0.084324, 0.000001);

    Node.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    FinalZoneSizing.deallocate();
    TermUnitFinalZoneSizing.deallocate();
    CalcFinalZoneSizing.deallocate();
    TermUnitSizing.deallocate();
    Sys.deallocate();
}

TEST_F(EnergyPlusFixture, VAVDefMinMaxFlowTestSizing2)
{

    // Test whether all blank inputs for min cool flow and max heat and reheat flow yield sensible results

    bool ErrorsFound(false);

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.4;",
        "	Zone,",
        "	SPACE3-1, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	2.438400269, !- Ceiling Height {m}",
        "	239.247360229; !- Volume {m3}",
        "	Sizing:Zone,",
        "	SPACE3-1, !- Zone or ZoneList Name",
        "	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
        "	14., !- Zone Cooling Design Supply Air Temperature { C }",
        "	, !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
        "	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
        "	50., !- Zone Heating Design Supply Air Temperature { C }",
        "	, !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
        "	0.009, !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	0.004, !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	SZ DSOA SPACE3-1, !- Design Specification Outdoor Air Object Name",
        "	0.0, !- Zone Heating Sizing Factor",
        "	0.0, !- Zone Cooling Sizing Factor",
        "	, !- Cooling Design Air Flow Method",
        "	, !- Cooling Design Air Flow Rate { m3/s }",
        "	, !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	, !- Cooling Minimum Air Flow { m3/s }",
        "	, !- Cooling Minimum Air Flow Fraction",
        "	, !- Heating Design Air Flow Method",
        "	, !- Heating Design Air Flow Rate { m3/s }",
        "	, !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	, !- Heating Maximum Air Flow { m3/s }",
        "	, !- Heating Maximum Air Flow Fraction",
        "	SZ DZAD SPACE3-1;        !- Design Specification Zone Air Distribution Object Name",
        "	DesignSpecification:ZoneAirDistribution,",
        "	SZ DZAD SPACE3-1, !- Name",
        "	1, !- Zone Air Distribution Effectiveness in Cooling Mode { dimensionless }",
        "	1; !- Zone Air Distribution Effectiveness in Heating Mode { dimensionless }",
        "	DesignSpecification:OutdoorAir,",
        "	SZ DSOA SPACE3-1, !- Name",
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
        "	SPACE3-1, !- Zone Name",
        "	SPACE3-1 Eq, !- Zone Conditioning Equipment List Name",
        "	SPACE3-1 In Node, !- Zone Air Inlet Node or NodeList Name",
        "	, !- Zone Air Exhaust Node or NodeList Name",
        "	SPACE3-1 Node, !- Zone Air Node Name",
        "	SPACE3-1 Out Node; !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	SPACE3-1 Eq, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
        "	SPACE3-1 ATU, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1; !- Zone Equipment 1 Heating or No - Load Sequence",
        "	ZoneHVAC:AirDistributionUnit,",
        "	SPACE3-1 ATU, !- Name",
        "	SPACE3-1 In Node, !- Air Distribution Unit Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat, !- Air Terminal Object Type",
        "	SPACE3-1 VAV Reheat; !- Air Terminal Name",
        "	Coil:Heating:Fuel,",
        "	SPACE3-1 Zone Coil, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "   NaturalGas,  !- Fuel Type",
        "	0.8, !- Burner Efficiency",
        "	1000, ! Nominal Capacity",
        "	SPACE3-1 Zone Coil Air In Node, !- Air Inlet Node Name",
        "	SPACE3-1 In Node; !- Air Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat,",
        "	SPACE3-1 VAV Reheat, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "	SPACE3-1 Zone Coil Air In Node, !- Damper Air Outlet Node Name",
        "	SPACE3-1 ATU In Node, !- Air Inlet Node Name",
        "	autosize, !- Maximum Air Flow Rate { m3/s }",
        "	, !- Zone Minimum Air Flow Input Method",
        "	, !- Constant Minimum Air Flow Fraction",
        "	, !- Fixed Minimum Air Flow Rate { m3/s }",
        "	, !- Minimum Air Flow Fraction Schedule Name",
        "	Coil:Heating:Fuel, !- Reheat Coil Object Type",
        "	SPACE3-1 Zone Coil, !- Reheat Coil Name",
        "	, !- Maximum Hot Water or Steam Flow Rate { m3/s }",
        "	, !- Minimum Hot Water or Steam Flow Rate { m3/s }",
        "	SPACE3-1 In Node, !- Air Outlet Node Name",
        "	0.001, !- Convergence Tolerance",
        "	, !- Damper Heating Action",
        "	, !- Maximum Flow per Zone Floor Area During Reheat { m3/s-m2 }",
        "	; !- Maximum Flow Fraction During Reheat",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    FinalZoneSizing.allocate(1);
    TermUnitFinalZoneSizing.allocate(1);
    NumAirTerminalSizingSpec = 1;
    CalcFinalZoneSizing.allocate(1);
    TermUnitSizing.allocate(1);
    GetZoneData(ErrorsFound);
    EXPECT_EQ("SPACE3-1", Zone(1).Name);
    GetOARequirements();      // get the OA requirements object
    GetZoneAirDistribution(); // get zone air distribution objects
    GetZoneSizingInput();
    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetZoneAirLoopEquipment();
    GetSysInput();
    ZoneSizingRunDone = true;
    CurZoneEqNum = 1;
    CurTermUnitSizingNum = 1;
    Zone(1).FloorArea = 96.48;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.21081;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    CalcFinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    CalcFinalZoneSizing(CurZoneEqNum).HeatSizingFactor = 1.0;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin =
        max(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2,
            FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow * FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac);
    EXPECT_DOUBLE_EQ(ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea, 0.000762);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac, 0.2);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2, .07351776, 0.000001);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin, .07351776, 0.000001);
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlowMax =
        max(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2,
            max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) *
                FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, 0.1415762);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac, 0.3);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea, .002032);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2, 0.196047, 0.000001);
    // EXPECT_NEAR( FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlowMax, 0.084324, 0.000001 );
    Sys(1).ZoneFloorArea = Zone(1).FloorArea;
    UpdateTermUnitFinalZoneSizing(); // Fills the TermUnitFinalZoneSizing array
    SizeSys(1);
    EXPECT_NEAR(Sys(CurZoneEqNum).ZoneMinAirFrac, 0.348739, 0.000001);
    EXPECT_NEAR(Sys(CurZoneEqNum).MaxAirVolFlowRateDuringReheat, 0.196047, 0.000001);

    Node.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    FinalZoneSizing.deallocate();
    TermUnitFinalZoneSizing.deallocate();
    CalcFinalZoneSizing.deallocate();
    TermUnitSizing.deallocate();
    Sys.deallocate();
}

TEST_F(EnergyPlusFixture, VAVDefMinMaxFlowTestSizing3)
{

    // Test user input at the terminal unit level and make sure it overrides any defaults or input coming from Sizing:Zone

    bool ErrorsFound(false);

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.4;",
        "	Zone,",
        "	SPACE3-1, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	2.438400269, !- Ceiling Height {m}",
        "	239.247360229; !- Volume {m3}",
        "	Sizing:Zone,",
        "	SPACE3-1, !- Zone or ZoneList Name",
        "	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
        "	14., !- Zone Cooling Design Supply Air Temperature { C }",
        "	, !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
        "	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
        "	50., !- Zone Heating Design Supply Air Temperature { C }",
        "	, !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
        "	0.009, !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	0.004, !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	SZ DSOA SPACE3-1, !- Design Specification Outdoor Air Object Name",
        "	0.0, !- Zone Heating Sizing Factor",
        "	0.0, !- Zone Cooling Sizing Factor",
        "	DesignDayWithLimit, !- Cooling Design Air Flow Method",
        "	, !- Cooling Design Air Flow Rate { m3/s }",
        "	, !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	, !- Cooling Minimum Air Flow { m3/s }",
        "	0.18, !- Cooling Minimum Air Flow Fraction",
        "	DesignDayWithLimit, !- Heating Design Air Flow Method",
        "	, !- Heating Design Air Flow Rate { m3/s }",
        "	, !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	0.11, !- Heating Maximum Air Flow { m3/s }",
        "	, !- Heating Maximum Air Flow Fraction",
        "	SZ DZAD SPACE3-1;        !- Design Specification Zone Air Distribution Object Name",
        "	DesignSpecification:ZoneAirDistribution,",
        "	SZ DZAD SPACE3-1, !- Name",
        "	1, !- Zone Air Distribution Effectiveness in Cooling Mode { dimensionless }",
        "	1; !- Zone Air Distribution Effectiveness in Heating Mode { dimensionless }",
        "	DesignSpecification:OutdoorAir,",
        "	SZ DSOA SPACE3-1, !- Name",
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
        "	SPACE3-1, !- Zone Name",
        "	SPACE3-1 Eq, !- Zone Conditioning Equipment List Name",
        "	SPACE3-1 In Node, !- Zone Air Inlet Node or NodeList Name",
        "	, !- Zone Air Exhaust Node or NodeList Name",
        "	SPACE3-1 Node, !- Zone Air Node Name",
        "	SPACE3-1 Out Node; !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	SPACE3-1 Eq, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
        "	SPACE3-1 ATU, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1; !- Zone Equipment 1 Heating or No - Load Sequence",
        "	ZoneHVAC:AirDistributionUnit,",
        "	SPACE3-1 ATU, !- Name",
        "	SPACE3-1 In Node, !- Air Distribution Unit Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat, !- Air Terminal Object Type",
        "	SPACE3-1 VAV Reheat; !- Air Terminal Name",
        "	Coil:Heating:Fuel,",
        "	SPACE3-1 Zone Coil, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "   NaturalGas,  !- Fuel Type",
        "	0.8, !- Burner Efficiency",
        "	1000, ! Nominal Capacity",
        "	SPACE3-1 Zone Coil Air In Node, !- Air Inlet Node Name",
        "	SPACE3-1 In Node; !- Air Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat,",
        "	SPACE3-1 VAV Reheat, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "	SPACE3-1 Zone Coil Air In Node, !- Damper Air Outlet Node Name",
        "	SPACE3-1 ATU In Node, !- Air Inlet Node Name",
        "	autosize, !- Maximum Air Flow Rate { m3/s }",
        "	Constant, !- Zone Minimum Air Flow Input Method",
        "	0.22, !- Constant Minimum Air Flow Fraction",
        "	, !- Fixed Minimum Air Flow Rate { m3/s }",
        "	, !- Minimum Air Flow Fraction Schedule Name",
        "	Coil:Heating:Fuel, !- Reheat Coil Object Type",
        "	SPACE3-1 Zone Coil, !- Reheat Coil Name",
        "	, !- Maximum Hot Water or Steam Flow Rate { m3/s }",
        "	, !- Minimum Hot Water or Steam Flow Rate { m3/s }",
        "	SPACE3-1 In Node, !- Air Outlet Node Name",
        "	0.001, !- Convergence Tolerance",
        "	ReverseWithLimits, !- Damper Heating Action",
        "	, !- Maximum Flow per Zone Floor Area During Reheat { m3/s-m2 }",
        "	0.44; !- Maximum Flow Fraction During Reheat",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    FinalZoneSizing.allocate(1);
    TermUnitFinalZoneSizing.allocate(1);
    CalcFinalZoneSizing.allocate(1);
    TermUnitSizing.allocate(1);
    GetZoneData(ErrorsFound);
    EXPECT_EQ("SPACE3-1", Zone(1).Name);
    GetOARequirements();      // get the OA requirements object
    GetZoneAirDistribution(); // get zone air distribution objects
    GetZoneSizingInput();
    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetZoneAirLoopEquipment();
    GetSysInput();
    ZoneSizingRunDone = true;
    CurZoneEqNum = 1;
    CurTermUnitSizingNum = 1;
    Zone(1).FloorArea = 96.48;
    TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow = 0.21081;
    TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow = 0.11341;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.21081;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    CalcFinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    CalcFinalZoneSizing(CurZoneEqNum).HeatSizingFactor = 1.0;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin =
        max(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2,
            FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow * FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac);
    EXPECT_DOUBLE_EQ(ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea, 0.000762);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac, 0.18);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2, .07351776, 0.000001);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin, .07351776, 0.000001);
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlowMax =
        max(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2,
            max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) *
                FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, 0.11);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatVolFlowMax, 0.11);
    Sys(1).ZoneFloorArea = Zone(1).FloorArea;
    SizeSys(1);
    EXPECT_DOUBLE_EQ(Sys(CurZoneEqNum).ZoneMinAirFrac, 0.22);
    EXPECT_NEAR(Sys(CurZoneEqNum).MaxAirVolFlowRateDuringReheat, 0.092756, 0.000001);

    Node.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    FinalZoneSizing.deallocate();
    TermUnitFinalZoneSizing.deallocate();
    CalcFinalZoneSizing.deallocate();
    TermUnitSizing.deallocate();
    Sys.deallocate();
}

TEST_F(EnergyPlusFixture, VAVDefMinMaxFlowTestSizing4)
{

    // Test all blank inputs for min cool flow and max heat and reheat flow and there have been no sizing calculations.
    // What will happen? It is suposed to autocalculate inputs anyway!

    bool ErrorsFound(false);

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.4;",
        "	Zone,",
        "	SPACE3-1, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	2.438400269, !- Ceiling Height {m}",
        "	239.247360229; !- Volume {m3}",
        // "	Sizing:Zone,",
        // "	SPACE3-1, !- Zone or ZoneList Name",
        // "	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
        // "	14., !- Zone Cooling Design Supply Air Temperature { C }",
        // "	, !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
        // "	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
        // "	50., !- Zone Heating Design Supply Air Temperature { C }",
        // "	, !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
        // "	0.009, !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        // "	0.004, !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        // "	SZ DSOA SPACE3-1, !- Design Specification Outdoor Air Object Name",
        // "	0.0, !- Zone Heating Sizing Factor",
        // "	0.0, !- Zone Cooling Sizing Factor",
        // "	, !- Cooling Design Air Flow Method",
        // "	, !- Cooling Design Air Flow Rate { m3/s }",
        // "	, !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
        // "	, !- Cooling Minimum Air Flow { m3/s }",
        // "	, !- Cooling Minimum Air Flow Fraction",
        // "	, !- Heating Design Air Flow Method",
        // "	, !- Heating Design Air Flow Rate { m3/s }",
        // "	, !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
        // "	, !- Heating Maximum Air Flow { m3/s }",
        // "	, !- Heating Maximum Air Flow Fraction",
        // "	SZ DZAD SPACE3-1;        !- Design Specification Zone Air Distribution Object Name",
        // "	DesignSpecification:ZoneAirDistribution,",
        // "	SZ DZAD SPACE3-1, !- Name",
        // "	1, !- Zone Air Distribution Effectiveness in Cooling Mode { dimensionless }",
        // "	1; !- Zone Air Distribution Effectiveness in Heating Mode { dimensionless }",
        // "	DesignSpecification:OutdoorAir,",
        // "	SZ DSOA SPACE3-1, !- Name",
        // "	sum, !- Outdoor Air Method",
        // "	0.00236, !- Outdoor Air Flow per Person { m3/s-person }",
        // "	0.000305, !- Outdoor Air Flow per Zone Floor Area { m3/s-m2 }",
        // "	0.0; !- Outdoor Air Flow per Zone { m3/s }",
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
        "	SPACE3-1, !- Zone Name",
        "	SPACE3-1 Eq, !- Zone Conditioning Equipment List Name",
        "	SPACE3-1 In Node, !- Zone Air Inlet Node or NodeList Name",
        "	, !- Zone Air Exhaust Node or NodeList Name",
        "	SPACE3-1 Node, !- Zone Air Node Name",
        "	SPACE3-1 Out Node; !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	SPACE3-1 Eq, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
        "	SPACE3-1 ATU, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1; !- Zone Equipment 1 Heating or No - Load Sequence",
        "	ZoneHVAC:AirDistributionUnit,",
        "	SPACE3-1 ATU, !- Name",
        "	SPACE3-1 In Node, !- Air Distribution Unit Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat, !- Air Terminal Object Type",
        "	SPACE3-1 VAV Reheat; !- Air Terminal Name",
        "	Coil:Heating:Fuel,",
        "	SPACE3-1 Zone Coil, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "   NaturalGas,  !- Fuel Type",
        "	0.8, !- Burner Efficiency",
        "	1000, ! Nominal Capacity",
        "	SPACE3-1 Zone Coil Air In Node, !- Air Inlet Node Name",
        "	SPACE3-1 In Node; !- Air Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat,",
        "	SPACE3-1 VAV Reheat, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "	SPACE3-1 Zone Coil Air In Node, !- Damper Air Outlet Node Name",
        "	SPACE3-1 ATU In Node, !- Air Inlet Node Name",
        "	0.21081, !- Maximum Air Flow Rate { m3/s }",
        "	, !- Zone Minimum Air Flow Input Method",
        "	, !- Constant Minimum Air Flow Fraction",
        "	, !- Fixed Minimum Air Flow Rate { m3/s }",
        "	, !- Minimum Air Flow Fraction Schedule Name",
        "	Coil:Heating:Fuel, !- Reheat Coil Object Type",
        "	SPACE3-1 Zone Coil, !- Reheat Coil Name",
        "	, !- Maximum Hot Water or Steam Flow Rate { m3/s }",
        "	, !- Minimum Hot Water or Steam Flow Rate { m3/s }",
        "	SPACE3-1 In Node, !- Air Outlet Node Name",
        "	0.001, !- Convergence Tolerance",
        "	, !- Damper Heating Action",
        "	, !- Maximum Flow per Zone Floor Area During Reheat { m3/s-m2 }",
        "	; !- Maximum Flow Fraction During Reheat",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    FinalZoneSizing.allocate(1);
    TermUnitFinalZoneSizing.allocate(1);
    CalcFinalZoneSizing.allocate(1);
    TermUnitSizing.allocate(1);
    GetZoneData(ErrorsFound);
    EXPECT_EQ("SPACE3-1", Zone(1).Name);
    // GetOARequirements(); // get the OA requirements object
    // GetZoneAirDistribution(); // get zone air distribution objects
    // GetZoneSizingInput();
    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetZoneAirLoopEquipment();
    GetSysInput();
    ZoneSizingRunDone = false;
    CurZoneEqNum = 1;
    Zone(1).FloorArea = 96.48;
    Sys(1).ZoneFloorArea = Zone(1).FloorArea;
    SizeSys(1);
    EXPECT_NEAR(Sys(CurZoneEqNum).ZoneMinAirFrac, 0.348739, 0.000001);
    EXPECT_NEAR(Sys(CurZoneEqNum).MaxAirVolFlowRateDuringReheat, 0.196047, 0.000001);

    Node.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    FinalZoneSizing.deallocate();
    TermUnitFinalZoneSizing.deallocate();
    CalcFinalZoneSizing.deallocate();
    TermUnitSizing.deallocate();
    Sys.deallocate();
}

TEST_F(EnergyPlusFixture, VAVDefMinMaxFlowTestSizing5)
{

    // Test FixedFlowRate input for Zone Minimum Air Flow Input Method. Does it pick up the default from Sizing:Zone?
    // Test max heating input in Sizing:Zone - use Heating Maximum Air Flow input and Design Day sizing

    bool ErrorsFound(false);

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
        "	Version,8.4;",
        "	Zone,",
        "	SPACE3-1, !- Name",
        "	0, !- Direction of Relative North { deg }",
        "	0, !- X Origin { m }",
        "	0, !- Y Origin { m }",
        "	0, !- Z Origin { m }",
        "	1, !- Type",
        "	1, !- Multiplier",
        "	2.438400269, !- Ceiling Height {m}",
        "	239.247360229; !- Volume {m3}",
        "	Sizing:Zone,",
        "	SPACE3-1, !- Zone or ZoneList Name",
        "	SupplyAirTemperature, !- Zone Cooling Design Supply Air Temperature Input Method",
        "	14., !- Zone Cooling Design Supply Air Temperature { C }",
        "	, !- Zone Cooling Design Supply Air Temperature Difference { deltaC }",
        "	SupplyAirTemperature, !- Zone Heating Design Supply Air Temperature Input Method",
        "	50., !- Zone Heating Design Supply Air Temperature { C }",
        "	, !- Zone Heating Design Supply Air Temperature Difference { deltaC }",
        "	0.009, !- Zone Cooling Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	0.004, !- Zone Heating Design Supply Air Humidity Ratio { kgWater/kgDryAir }",
        "	SZ DSOA SPACE3-1, !- Design Specification Outdoor Air Object Name",
        "	0.0, !- Zone Heating Sizing Factor",
        "	0.0, !- Zone Cooling Sizing Factor",
        "	DesignDayWithLimit, !- Cooling Design Air Flow Method",
        "	, !- Cooling Design Air Flow Rate { m3/s }",
        "	, !- Cooling Minimum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	, !- Cooling Minimum Air Flow { m3/s }",
        "	0.2, !- Cooling Minimum Air Flow Fraction",
        "	DesignDay, !- Heating Design Air Flow Method",
        "	, !- Heating Design Air Flow Rate { m3/s }",
        "	, !- Heating Maximum Air Flow per Zone Floor Area { m3/s-m2 }",
        "	0.08, !- Heating Maximum Air Flow { m3/s }",
        "	, !- Heating Maximum Air Flow Fraction",
        "	SZ DZAD SPACE3-1;        !- Design Specification Zone Air Distribution Object Name",
        "	DesignSpecification:ZoneAirDistribution,",
        "	SZ DZAD SPACE3-1, !- Name",
        "	1, !- Zone Air Distribution Effectiveness in Cooling Mode { dimensionless }",
        "	1; !- Zone Air Distribution Effectiveness in Heating Mode { dimensionless }",
        "	DesignSpecification:OutdoorAir,",
        "	SZ DSOA SPACE3-1, !- Name",
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
        "	SPACE3-1, !- Zone Name",
        "	SPACE3-1 Eq, !- Zone Conditioning Equipment List Name",
        "	SPACE3-1 In Node, !- Zone Air Inlet Node or NodeList Name",
        "	, !- Zone Air Exhaust Node or NodeList Name",
        "	SPACE3-1 Node, !- Zone Air Node Name",
        "	SPACE3-1 Out Node; !- Zone Return Air Node Name",
        "	ZoneHVAC:EquipmentList,",
        "	SPACE3-1 Eq, !- Name",
        "   SequentialLoad,          !- Load Distribution Scheme",
        "	ZoneHVAC:AirDistributionUnit, !- Zone Equipment 1 Object Type",
        "	SPACE3-1 ATU, !- Zone Equipment 1 Name",
        "	1, !- Zone Equipment 1 Cooling Sequence",
        "	1; !- Zone Equipment 1 Heating or No - Load Sequence",
        "	ZoneHVAC:AirDistributionUnit,",
        "	SPACE3-1 ATU, !- Name",
        "	SPACE3-1 In Node, !- Air Distribution Unit Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat, !- Air Terminal Object Type",
        "	SPACE3-1 VAV Reheat; !- Air Terminal Name",
        "	Coil:Heating:Fuel,",
        "	SPACE3-1 Zone Coil, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "   NaturalGas,  !- Fuel Type",
        "	0.8, !- Burner Efficiency",
        "	1000, ! Nominal Capacity",
        "	SPACE3-1 Zone Coil Air In Node, !- Air Inlet Node Name",
        "	SPACE3-1 In Node; !- Air Outlet Node Name",
        "	AirTerminal:SingleDuct:VAV:Reheat,",
        "	SPACE3-1 VAV Reheat, !- Name",
        "	ReheatCoilAvailSched, !- Availability Schedule Name",
        "	SPACE3-1 Zone Coil Air In Node, !- Damper Air Outlet Node Name",
        "	SPACE3-1 ATU In Node, !- Air Inlet Node Name",
        "	autosize, !- Maximum Air Flow Rate { m3/s }",
        "	FixedFlowRate, !- Zone Minimum Air Flow Input Method",
        "	, !- Constant Minimum Air Flow Fraction",
        "	, !- Fixed Minimum Air Flow Rate { m3/s }",
        "	, !- Minimum Air Flow Fraction Schedule Name",
        "	Coil:Heating:Fuel, !- Reheat Coil Object Type",
        "	SPACE3-1 Zone Coil, !- Reheat Coil Name",
        "	, !- Maximum Hot Water or Steam Flow Rate { m3/s }",
        "	, !- Minimum Hot Water or Steam Flow Rate { m3/s }",
        "	SPACE3-1 In Node, !- Air Outlet Node Name",
        "	0.001, !- Convergence Tolerance",
        "	ReverseWithLimits, !- Damper Heating Action",
        "	, !- Maximum Flow per Zone Floor Area During Reheat { m3/s-m2 }",
        "	; !- Maximum Flow Fraction During Reheat",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    FinalZoneSizing.allocate(1);
    NumAirTerminalSizingSpec = 1;
    TermUnitFinalZoneSizing.allocate(1);
    CalcFinalZoneSizing.allocate(1);
    TermUnitSizing.allocate(1);
    GetZoneData(ErrorsFound);
    EXPECT_EQ("SPACE3-1", Zone(1).Name);
    GetOARequirements();      // get the OA requirements object
    GetZoneAirDistribution(); // get zone air distribution objects
    GetZoneSizingInput();
    GetZoneEquipmentData1();
    ProcessScheduleInput();
    ScheduleInputProcessed = true;
    GetZoneAirLoopEquipment();
    GetSysInput();
    ZoneSizingRunDone = true;
    CurZoneEqNum = 1;
    CurTermUnitSizingNum = 1;
    Zone(1).FloorArea = 96.48;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.21081;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    CalcFinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.11341;
    CalcFinalZoneSizing(CurZoneEqNum).HeatSizingFactor = 1.0;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin =
        max(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2,
            FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow * FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac);
    EXPECT_DOUBLE_EQ(ZoneSizingInput(CurZoneEqNum).DesCoolMinAirFlowPerArea, 0.000762);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlowFrac, 0.2);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolMinAirFlow2, .07351776, 0.000001);
    EXPECT_NEAR(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlowMin, .07351776, 0.000001);
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlow;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowFrac;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2 = ZoneSizingInput(CurZoneEqNum).DesHeatMaxAirFlowPerArea * Zone(1).FloorArea;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlowMax =
        max(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2,
            max(FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow) *
                FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow, 0.08);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowFrac, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlowPerArea, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatMaxAirFlow2, 0.0);
    EXPECT_DOUBLE_EQ(FinalZoneSizing(CurZoneEqNum).DesHeatVolFlowMax, 0.08);
    Sys(1).ZoneFloorArea = Zone(1).FloorArea;
    UpdateTermUnitFinalZoneSizing(); // Fills the TermUnitFinalZoneSizing array
    SizeSys(1);
    EXPECT_DOUBLE_EQ(Sys(CurZoneEqNum).ZoneMinAirFrac, 0.07351776 / 0.21081);
    EXPECT_DOUBLE_EQ(Sys(CurZoneEqNum).MaxAirVolFlowRateDuringReheat, 0.08);

    Node.deallocate();
    ZoneEquipConfig.deallocate();
    Zone.deallocate();
    FinalZoneSizing.deallocate();
    TermUnitFinalZoneSizing.deallocate();
    CalcFinalZoneSizing.deallocate();
    TermUnitSizing.deallocate();
    Sys.deallocate();
}

} // namespace EnergyPlus
