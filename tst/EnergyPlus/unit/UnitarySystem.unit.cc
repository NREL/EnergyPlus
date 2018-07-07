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

#include <stdexcept>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DirectAirManager.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <General.hh>

using namespace EnergyPlus;
using namespace UnitarySystems;

class ZoneUnitarySystemTest : public EnergyPlusFixture
{
public:
    int UnitarySysNum = 1;
    int NumNodes = 1; // number of zone inlet and zone exhaust nodes
    bool ErrorsFound = false;
    Real64 const CpWater = 4180.0;  // For estimating the expected result
    Real64 const RhoWater = 1000.0; // For estimating the expected result

protected:
    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.

        DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize StdRhoAir
        DataEnvironment::OutBaroPress = 101325.0;
        DataGlobals::NumOfZones = 1;
        DataHeatBalance::Zone.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipConfig.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipList.allocate(DataGlobals::NumOfZones);
        DataZoneEquipment::ZoneEquipAvail.dimension(DataGlobals::NumOfZones, DataHVACGlobals::NoAction);
        DataHeatBalance::Zone(1).Name = "EAST ZONE";
        DataZoneEquipment::NumOfZoneEquipLists = 1;
        DataHeatBalance::Zone(1).IsControlled = true;
        DataZoneEquipment::ZoneEquipConfig(1).IsControlled = true;
        DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
        DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "EAST ZONE";
        DataZoneEquipment::ZoneEquipConfig(1).EquipListName = "ZONE2EQUIPMENT";
        DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 20;
        DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 1;
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode.allocate(1);
        DataZoneEquipment::ZoneEquipConfig(1).ReturnNode(1) = 21;
        DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum).SystemZoneNodeNumber =
            DataZoneEquipment::ZoneEquipConfig(1).ZoneNode;
        DataZoneEquipment::ZoneEquipConfig(1).ReturnFlowSchedPtrNum = DataGlobals::ScheduleAlwaysOn;
        DataZoneEquipment::ZoneEquipList(1).Name = "ZONE2EQUIPMENT";
        int maxEquipCount = 1;
        DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes = maxEquipCount;
        DataZoneEquipment::ZoneEquipList(1).EquipType.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipType_Num.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipName.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipIndex = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipData.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).CoolingPriority.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).HeatingPriority.allocate(DataZoneEquipment::ZoneEquipList(1).NumOfEquipTypes);
        DataZoneEquipment::ZoneEquipList(1).EquipType(1) = "UNITARYSYSTEM";
        DataZoneEquipment::ZoneEquipList(1).EquipName(1) = "UNITARY SYSTEM MODEL";
        DataZoneEquipment::ZoneEquipList(1).CoolingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).HeatingPriority(1) = 1;
        DataZoneEquipment::ZoneEquipList(1).EquipType_Num(1) = DataZoneEquipment::ZoneUnitarySystem_Num;
        DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = NumNodes;
        DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitCool.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitHeat.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 2;
        DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = NumNodes;
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(NumNodes);
        DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 1;
        DataZoneEquipment::ZoneEquipConfig(1).EquipListIndex = 1;

        DataSizing::CurSysNum = 0;
        DataSizing::CurZoneEqNum = 1;

        DataSizing::FinalZoneSizing.allocate(1);
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolVolFlow = 1.5;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow = 1.2;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInTemp = 25.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesCoolCoilInHumRat = 0.009;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesTemp = 15.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDesHumRat = 0.006;

        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatCoilInTemp = 20.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesTemp = 30.0;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).HeatDesHumRat = 0.007;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatMassFlow =
            DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).DesHeatVolFlow * DataEnvironment::StdRhoAir;

        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).TimeStepNumAtCoolMax = 1;
        DataSizing::FinalZoneSizing(DataSizing::CurZoneEqNum).CoolDDNum = 1;
        DataSizing::DesDayWeath.allocate(1);
        DataSizing::DesDayWeath(1).Temp.allocate(1);
        DataSizing::DesDayWeath(1).Temp(1) = 35.0;

        DataSizing::ZoneEqSizing.allocate(1);
        DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
        DataSizing::ZoneSizingRunDone = true;

        // set up plant loop
        DataPlant::TotNumLoops = 2;
        DataPlant::PlantLoop.allocate(DataPlant::TotNumLoops);
        DataSizing::PlantSizData.allocate(DataPlant::TotNumLoops);
        int NumPltSizInput = DataPlant::TotNumLoops;

        for (int loopindex = 1; loopindex <= DataPlant::TotNumLoops; ++loopindex) {
            auto &loop(DataPlant::PlantLoop(loopindex));
            loop.LoopSide.allocate(2);
            auto &loopside(DataPlant::PlantLoop(loopindex).LoopSide(1));
            loopside.TotalBranches = 1;
            loopside.Branch.allocate(1);
            auto &loopsidebranch(DataPlant::PlantLoop(loopindex).LoopSide(1).Branch(1));
            loopsidebranch.TotalComponents = 2;
            loopsidebranch.Comp.allocate(2);
        }
        DataPlant::PlantLoop(1).Name = "Hot Water Loop";
        DataPlant::PlantLoop(1).FluidName = "WATER";
        DataPlant::PlantLoop(1).FluidIndex = 1;

        DataPlant::PlantLoop(2).Name = "Chilled Water Loop";
        DataPlant::PlantLoop(2).FluidName = "WATER";
        DataPlant::PlantLoop(2).FluidIndex = 1;

        DataSizing::PlantSizData(1).PlantLoopName = "Hot Water Loop";
        DataSizing::PlantSizData(1).ExitTemp = 80.0;
        DataSizing::PlantSizData(1).DeltaT = 10.0;

        DataSizing::PlantSizData(2).PlantLoopName = "Chilled Water Loop";
        DataSizing::PlantSizData(2).ExitTemp = 6.0;
        DataSizing::PlantSizData(2).DeltaT = 5.0;
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

TEST_F(ZoneUnitarySystemTest, Test_UnitarySys_factory)
{

    std::string const idf_objects = delimited_string({

        "UnitarySystem,",
        "  Unitary System Model,           !- Name",
        "  Setpoint,                       !- Control Type",
        "  East Zone,                      !- Controlling Zone or Thermostat Location",
        "  None,                           !- Dehumidification Control Type",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  Fan:OnOff,                      !- Supply Fan Object Type",
        "  Supply Fan 1,                   !- Supply Fan Name",
        "  BlowThrough,                    !- Fan Placement",
        "  ,                               !- Supply Air Fan Operating Mode Schedule Name",
        "  ,                               !- Heating Coil Object Type",
        "  ,                               !- Heating Coil Name",
        "  ,                               !- DX Heating Coil Sizing Ratio",
        "  Coil:Cooling:DX:MultiSpeed,     !- Cooling Coil Object Type",
        "  DX Cooling Coil,                !- Cooling Coil Name",
        "  No,                             !- Use DOAS DX Cooling Coil",
        "  2.0,                            !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  SensibleOnlyLoadControl,        !- Latent Load Control",
        "  ,                               !- Supplemental Heating Coil Object Type",
        "  ,                               !- Supplemental Heating Coil Name",
        "  ,                               !- Supply Air Flow Rate Method During Cooling Operation",
        "  AutoSize,                       !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Supply air Flow Rate Method During Heating Operation",
        "  AutoSize,                       !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  ,                               !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  AutoSize,                       !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                               !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                               !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                               !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                               !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80.0,                           !- Maximum Supply Air Temperature{ C }",
        "  ,                               !- Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Operation {C}",
        "  ,                               !- Outdoor Dry-Bulb Temperature Sensor Node Name",
        "  ,                               !- Maximum Cycling Rate",
        "  ,                               !- Heat Pump Time Constant",
        "  ,                               !- Fraction of On-Cycle Power Use",
        "  ,                               !- Heat Pump Fan Delay Time",
        "  ,                               !- Ancilliary On-Cycle Electric Power",
        "  ,                               !- Ancilliary Off-Cycle Electric Power",
        "  ,                               !- Design Heat Recovery Water Flow Rate",
        "  ,                               !- Maximum Temperature for Heat Recovery",
        "  ,                               !- Heat Recovery Water Inlet Node Name",
        "  ,                               !- Heat Recovery Water Outlet Node Name",
        "  UnitarySystemPerformance:Multispeed,                     !- Design Specification Multispeed Object Type",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance;  !- Design Specification Multispeed Object Name",

        "UnitarySystemPerformance:Multispeed,",
        "  DX Cool MultiSpd Unitary System MultiSpeed Performance,  !- Name",
        "  1,                              !- Number of Speeds for Heating",
        "  2,                              !- Number of Speeds for Cooling",
        "  No,                             !- Single Mode Operation",
        "  ,                               !- No Load Supply Air Flow Rate Ratio",
        "  1,                              !- Heating Speed 1 Supply Air Flow Ratio",
        "  1,                              !- Cooling Speed 1 Supply Air Flow Ratio",
        "  AutoSize,                       !- Heating Speed 2 Supply Air Flow Ratio",
        "  AutoSize;                       !- Cooling Speed 2 Supply Air Flow Ratio",

        "Fan:OnOff,",
        "  Supply Fan 1,                   !- Name",
        "  AlwaysOne,                      !- Availability Schedule Name",
        "  0.7,                            !- Fan Total Efficiency",
        "  600.0,                          !- Pressure Rise{ Pa }",
        "  AutoSize,                       !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                            !- Motor Efficiency",
        "  1.0,                            !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,              !- Air Inlet Node Name",
        "  Cooling Coil Air Inlet Node;    !- Air Outlet Node Name",

        "Coil:Cooling:DX:MultiSpeed,",
        "  DX Cooling Coil,                !- Name",
        "  ,                               !- Availability Schedule Name",
        "  Cooling Coil Air Inlet Node,    !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,              !- Air Outlet Node Name",
        "  ,                               !- Condenser Air Inlet Node Name",
        "  AirCooled,                      !- Condenser Type",
        "  ,                               !- Minimum Outdoor Dry - Bulb Temperature for Compressor Operation{ C }",
        "  ,                               !- Supply Water Storage Tank Name",
        "  ,                               !- Condensate Collection Water Storage Tank Name",
        "  No,                             !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No,                             !- Apply Latent Degradation to Speeds Greater than 1",
        "  0,                              !- Crankcase Heater Capacity{ W }",
        "  10,                             !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  0,                              !- Basin Heater Capacity{ W / K }",
        "  2,                              !- Basin Heater Setpoint Temperature{ C }",
        "  ,                               !- Basin Heater Operating Schedule Name",
        "  Electricity,                    !- Fuel Type",
        "  2,                              !- Number of Speeds",
        "  AutoSize,                       !- Speed 1 Gross Rated Total Cooling Capacity{ W }",
        "  AutoSize,                       !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  5.12895662368113,               !- Speed 1 Gross Rated Cooling COP{ W / W }",
        "  AutoSize,                       !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  773.3,                          !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  Biquadratic,                    !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Quadratic,                      !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  0,                              !- Speed 1 Nominal Time for Condensate Removal to Begin{ s }",
        "  0,                              !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
        "  0,                              !- Speed 1 Maximum Cycling Rate{ cycles / hr }",
        "  0,                              !- Speed 1 Latent Capacity Time Constant{ s }",
        "  0.5,                            !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  Biquadratic,                    !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  0.9,                            !- Speed 1 Evaporative Condenser Effectiveness{ dimensionless }",
        "  AutoSize,                       !- Speed 1 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  AutoSize,                       !- Speed 1 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  AutoSize,                       !- Speed 2 Gross Rated Total Cooling Capacity{ W }",
        "  AutoSize,                       !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  4.68933177022274,               !- Speed 2 Gross Rated Cooling COP{ W / W }",
        "  AutoSize,                       !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  773.3,                          !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
        "  Biquadratic,                    !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  Biquadratic,                    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  Quadratic,                      !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Quadratic,                      !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  0,                              !- Speed 2 Nominal Time for Condensate Removal to Begin{ s }",
        "  0,                              !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  0,                              !- Speed 2 Maximum Cycling Rate{ cycles / hr }",
        "  0,                              !- Speed 2 Latent Capacity Time Constant{ s }",
        "  0.5,                            !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  Biquadratic,                    !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  0.9,                            !- Speed 2 Evaporative Condenser Effectiveness{ dimensionless }",
        "  AutoSize,                       !- Speed 2 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  AutoSize;                       !- Speed 2 Rated Evaporative Condenser Pump Power Consumption{ W }",

        "ScheduleTypeLimits,",
        "  Any Number;                     !- Name",

        "Schedule:Compact,",
        "  AlwaysOne,                      !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 1.0;              !- Field 3",

        "Schedule:Compact,",
        "  Always 20C,                     !- Name",
        "  Any Number,                     !- Schedule Type Limits Name",
        "  Through: 12/31,                 !- Field 1",
        "  For: AllDays,                   !- Field 2",
        "  Until: 24:00, 20.0;             !- Field 3",

        "SetpointManager:Scheduled,",
        "  Cooling Coil Setpoint Manager,  !- Name",
        "  Temperature,                    !- Control Variable",
        "  Always 20C,                     !- Schedule Name",
        "  Zone 2 Inlet Node;              !- Setpoint Node or NodeList Name",

        "Curve:Quadratic,",
        "  Quadratic,                      !- Name",
        "  0.8,                            !- Coefficient1 Constant",
        "  0.2,                            !- Coefficient2 x",
        "  0.0,                            !- Coefficient3 x**2",
        "  0.5,                            !- Minimum Value of x",
        "  1.5;                            !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Biquadratic,                    !- Name",
        "  0.942587793,                    !- Coefficient1 Constant",
        "  0.009543347,                    !- Coefficient2 x",
        "  0.000683770,                    !- Coefficient3 x**2",
        "  -0.011042676,                   !- Coefficient4 y",
        "  0.000005249,                    !- Coefficient5 y**2",
        "  -0.000009720,                   !- Coefficient6 x*y",
        "  12.77778,                       !- Minimum Value of x",
        "  23.88889,                       !- Maximum Value of x",
        "  18.0,                           !- Minimum Value of y",
        "  46.11111,                       !- Maximum Value of y",
        "  ,                               !- Minimum Curve Output",
        "  ,                               !- Maximum Curve Output",
        "  Temperature,                    !- Input Unit Type for X",
        "  Temperature,                    !- Input Unit Type for Y",
        "  Dimensionless;                  !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool FirstHVACIteration = true;
    std::string compName = "UNITARY SYSTEM MODEL";
    UnitarySys mySys;

    // call the UnitarySystem factory
    int compTypeOfNum = SimAirServingZones::UnitarySystemModel;
    mySys.factory(compTypeOfNum, compName);

    // verify the size of the vector and the processed names
    // 2 UnitarySystem objects
    EXPECT_EQ(1u, unitarySys.size());

    // set a pointer to the first object
    UnitarySys *thisSys = &unitarySys[0];
    // test the object name
    EXPECT_EQ(compName, thisSys->name);

    // calling the factory with an invalid type or name should call ShowFatalError, which will trigger a runtime exception
    // call with a wrong name
    compName = "Test";
    EXPECT_THROW(mySys.factory(compTypeOfNum, compName), std::runtime_error);
    // call with a wrong type
    compName = "Unitary System Model";
    compTypeOfNum = 9;
    EXPECT_THROW(mySys.factory(compTypeOfNum, compName), std::runtime_error);

    // test calling the sim routine
    int AirLoopNum = 1;
    int CompIndex = 0; // zero based index
    bool HeatingActive = false;
    bool CoolingActive = false;
    int OAUnitNum = 0;
    Real64 OAUCoilOutTemp = 0.0;
    bool ZoneEquipFlag = false;
    thisSys->simulate(compName, FirstHVACIteration, AirLoopNum, CompIndex, HeatingActive, CoolingActive, OAUnitNum, OAUCoilOutTemp, ZoneEquipFlag);

    // test calling the init routine
   // mySys.init(FirstHVACIteration);
}
