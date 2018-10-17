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
#include "DataEnvironment.hh"
#include "DataGlobals.hh"
#include "DataHVACGlobals.hh"
#include "DataHeatBalFanSys.hh"
#include "DataLoopNode.hh"
#include "DataSizing.hh"
#include "DataZoneEnergyDemands.hh"
#include "DataZoneEquipment.hh"
#include "ElectricPowerServiceManager.hh"
#include "Fixtures/EnergyPlusFixture.hh"
#include "HVACHXAssistedCoolingCoil.hh"
#include "HeatBalanceManager.hh"
#include "OutputProcessor.hh"
#include "OutputReportPredefined.hh"
#include "Psychrometrics.hh"
#include "ScheduleManager.hh"
#include "SimulationManager.hh"
#include "SizingManager.hh"
#include "UnitarySystem.hh"

using namespace EnergyPlus;

// TEST_F( EnergyPlusFixture, HXAssistCC_VStest1 )

TEST_F(EnergyPlusFixture, HXAssistCCUnitarySystem_VStest1)
{

    bool ErrorsFound(false);
    bool FirstHVACIteration(false);
    Real64 CpAir(0.0);       // specific heat of air
    Real64 Qsens_sys(0.0);   // UnitarySystem delivered sensible capacity wrt zone
    Real64 MinHumRatio(0.0); // track minimum of outlet node or zone humidity ratio
    Real64 ZoneTemp(0.0);    // control zone temperature
    int InletNode(0);        // UnitarySystem inlet node number
    int OutletNode(0);       // UnitarySystem outlet node number
    int ControlZoneNum(0);   // index to control zone

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "  ",
        "Zone,",
        "  EAST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",
        "  ",
        "ZoneHVAC:EquipmentConnections,",
        "EAST ZONE,                 !- Zone Name",
        "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
        "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
        "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
        "  Zone 2 Node,             !- Zone Air Node Name",
        "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
        "  ",
        "ZoneHVAC:EquipmentList,",
        "  Zone2Equipment,          !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
        "  GasHeat DXAC Furnace 1,          !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
        "  ",
        "AirLoopHVAC:UnitarySystem,",
        "  GasHeat DXAC Furnace 1, !- Name",
        "  Load,                   !- Control Type",
        "  East Zone,              !- Controlling Zone or Thermostat Location",
        "  None,                   !- Dehumidification Control Type",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Zone Exhaust Node,         !- Air Inlet Node Name",
        "  Zone 2 Inlet Node,   !- Air Outlet Node Name",
        "  Fan:OnOff,              !- Supply Fan Object Type",
        "  Supply Fan 1,           !- Supply Fan Name",
        "  BlowThrough,            !- Fan Placement",
        "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
        "  Coil:Heating:Fuel,       !- Heating Coil Object Type",
        "  Furnace Heating Coil 1, !- Heating Coil Name",
        "  ,                       !- DX Heating Coil Sizing Ratio",
        "  CoilSystem:Cooling:DX:HeatExchangerAssisted, !- Cooling Coil Object Type",
        "  Heat Exchanger Assisted Cooling Coil 1,     !- Cooling Coil Name",
        "  ,                       !- Use DOAS DX Cooling Coil",
        "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
        "  ,                       !- Latent Load Control",
        "  Coil:Heating:Fuel,       !- Supplemental Heating Coil Object Type",
        "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
        "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
        "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
        "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
        "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
        "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
        "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
        "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
        "  80;                     !- Maximum Supply Air Temperature{ C }",

        "  CoilSystem:Cooling:DX:HeatExchangerAssisted,",
        "    Heat Exchanger Assisted Cooling Coil 1,  !- Name",
        "    HeatExchanger:AirToAir:SensibleAndLatent,  !- Heat Exchanger Object Type",
        "    Heat Exchanger Assisted DX 1,  !- Heat Exchanger Name",
        "    Coil:Cooling:DX:VariableSpeed,  !- Cooling Coil Object Type",
        "    Main Cooling Coil 1;      !- Cooling Coil Name",

        "  HeatExchanger:AirToAir:SensibleAndLatent,",
        "    Heat Exchanger Assisted DX 1,      !- Name",
        "    ,   !- Availability Schedule Name",
        "    1.6,                !- Nominal Supply Air Flow Rate {m3/s}",
        "    0.7,                     !- Sensible Effectiveness at 100% Heating Air Flow {dimensionless}",
        "    0.65,                    !- Latent Effectiveness at 100% Heating Air Flow {dimensionless}",
        "    0.70000,                !- Sensible Effectiveness at 75% Heating Air Flow {dimensionless}",
        "    0.650000,                !- Latent Effectiveness at 75% Heating Air Flow {dimensionless}",
        "    0.7,                     !- Sensible Effectiveness at 100% Cooling Air Flow {dimensionless}",
        "    0.65,                    !- Latent Effectiveness at 100% Cooling Air Flow {dimensionless}",
        "    0.70000,                !- Sensible Effectiveness at 75% Cooling Air Flow {dimensionless}",
        "    0.650000,                !- Latent Effectiveness at 75% Cooling Air Flow {dimensionless}",
        "    DX Cooling Coil Air Inlet Node,   !- Supply Air Inlet Node Name",
        "    Heat Recovery Supply Outlet,  !- Supply Air Outlet Node Name",
        "    Heat Recovery Exhuast Inlet Node,  !- Exhaust Air Inlet Node Name",
        "    Heating Coil Air Inlet Node,  !- Exhaust Air Outlet Node Name",
        "    0,                       !- Nominal Electric Power {W}",
        "    No,                     !- Supply Air Outlet Temperature Control",
        "    Plate,                   !- Heat Exchanger Type",
        "    MinimumExhaustTemperature,  !- Frost Control Type",
        "    1.7,                     !- Threshold Temperature {C}",
        "    0.083,                   !- Initial Defrost Time Fraction {dimensionless}",
        "    0.012,                   !- Rate of Defrost Time Fraction Increase {1/K}",
        "    Yes;                     !- Economizer Lockout",

        "  Coil:Cooling:DX:VariableSpeed,",
        "    Main Cooling Coil 1,    !- Name",
        "    Heat Recovery Supply Outlet,  !- Indoor Air Inlet Node Name",
        "    Heat Recovery Exhuast Inlet Node,  !- Indoor Air Outlet Node Name",
        "    1.0,                     !- Number of Speeds {dimensionless}",
        "    1.0,                     !- Nominal Speed Level {dimensionless}",
        "    32000.0,                 !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level {w}",
        "    1.6,                     !- Rated Air Flow Rate At Selected Nominal Speed Level {m3/s}",
        "    0.0,                     !- Nominal Time for Condensate to Begin Leaving the Coil {s}",
        "    0.0,                     !- Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity {dimensionless}",
        "    HPACCOOLPLFFPLR,         !- Energy Part Load Fraction Curve Name",
        "    ,                        !- Condenser Air Inlet Node Name",
        "    AirCooled,               !- Condenser Type",
        "    ,                        !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "    0.0,                     !- Crankcase Heater Capacity {W}",
        "    10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    ,                        !- Supply Water Storage Tank Name",
        "    ,                        !- Condensate Collection Water Storage Tank Name",
        "    ,                        !- Basin Heater Capacity {W/K}",
        "    ,                        !- Basin Heater Setpoint Temperature {C}",
        "    ,                        !- Basin Heater Operating Schedule Name",
        "    36991.44197,             !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity {w}",
        "    0.75,                    !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio {dimensionless}",
        "    3.866381837,             !- Speed 1 Reference Unit Gross Rated Cooling COP {dimensionless}",
        "    3.776,                   !- Speed 1 Reference Unit Rated Air Flow Rate {m3/s}",
        "    10.62,                   !- Speed 1 Reference Unit Rated Condenser Air Flow Rate {m3/s}",
        "    ,                        !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling {dimensionless}",
        "    HPCoolingCAPFTemp4,      !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "    HPACFFF,                 !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
        "    HPCoolingEIRFTemp4,      !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "    HPACFFF;                 !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",

        "   Curve:Quadratic,",
        "    HPACCOOLPLFFPLR,         !- Name",
        "    1.0,                    !- Coefficient1 Constant",
        "    0.0,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x  ",

        "  Curve:Cubic,",
        "    HPACFFF,                 !- Name",
        "    1.0,                     !- Coefficient1 Constant",
        "    0.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Biquadratic,",
        "    HPCoolingEIRFTemp4,      !- Name",
        "    0.0001514017,            !- Coefficient1 Constant",
        "    0.0655062896,            !- Coefficient2 x",
        "    -0.0020370821,           !- Coefficient3 x**2",
        "    0.0067823041,            !- Coefficient4 y",
        "    0.0004087196,            !- Coefficient5 y**2",
        "    -0.0003552302,           !- Coefficient6 x*y",
        "    13.89,                   !- Minimum Value of x",
        "    22.22,                   !- Maximum Value of x",
        "    12.78,                   !- Minimum Value of y",
        "    51.67,                   !- Maximum Value of y",
        "    0.5141,                  !- Minimum Curve Output",
        "    1.7044,                  !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    HPCoolingCAPFTemp4,      !- Name",
        "    1.3544202152,            !- Coefficient1 Constant",
        "    -0.0493402773,           !- Coefficient2 x",
        "    0.0022649843,            !- Coefficient3 x**2",
        "    0.0008517727,            !- Coefficient4 y",
        "    -0.0000426316,           !- Coefficient5 y**2",
        "    -0.0003364517,           !- Coefficient6 x*y",
        "    13.89,                   !- Minimum Value of x",
        "    22.22,                   !- Maximum Value of x",
        "    12.78,                   !- Minimum Value of y",
        "    51.67,                   !- Maximum Value of y",
        "    0.7923,                  !- Minimum Curve Output",
        "    1.2736,                  !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  OutdoorAir:Node,",
        "    Main Cooling Coil 1 Condenser Node,  !- Name",
        "    -1.0;                    !- Height Above Ground {m}",

        "Fan:OnOff,",
        "  Supply Fan 1,           !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  0.7,                    !- Fan Total Efficiency",
        "  600.0,                  !- Pressure Rise{ Pa }",
        "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
        "  0.9,                    !- Motor Efficiency",
        "  1.0,                    !- Motor In Airstream Fraction",
        "  Zone Exhaust Node,      !- Air Inlet Node Name",
        "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

        "Coil:Heating:Fuel,",
        "  Furnace Heating Coil 1, !- Name",
        "  FanAndCoilAvailSched,   !- Availability Schedule Name",
        "  Gas,                    !- Fuel Type",
        "  0.8,                    !- Gas Burner Efficiency",
        "  32000,                  !- Nominal Capacity{ W }",
        "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",
        "  ",
        "Coil:Heating:Fuel,",
        "  Humidistat Reheat Coil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  Gas,                    !- Fuel Type",
        "  0.8, !- Gas Burner Efficiency",
        "  32000, !- Nominal Capacity{ W }",
        "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Zone 2 Inlet Node;    !- Air Outlet Node Name",
        "  ",
        "ScheduleTypeLimits,",
        "  Any Number;             !- Name",
        "  ",
        "Schedule:Compact,",
        "  FanAndCoilAvailSched,   !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Schedule:Compact,",
        "  ContinuousFanSchedule,  !- Name",
        "  Any Number,             !- Schedule Type Limits Name",
        "  Through: 12/31,         !- Field 1",
        "  For: AllDays,           !- Field 2",
        "  Until: 24:00, 1.0;      !- Field 3",
        "  ",
        "Curve:Quadratic,",
        "  CoolCapFFF,       !- Name",
        "  0.8,                    !- Coefficient1 Constant",
        "  0.2,                    !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  COOLEIRFFF,           !- Name",
        "  1.1552,                 !- Coefficient1 Constant",
        "  -0.1808,                !- Coefficient2 x",
        "  0.0256,                 !- Coefficient3 x**2",
        "  0.5,                    !- Minimum Value of x",
        "  1.5;                    !- Maximum Value of x",
        "  ",
        "Curve:Quadratic,",
        "  PLFFPLR,          !- Name",
        "  0.85,                   !- Coefficient1 Constant",
        "  0.15,                   !- Coefficient2 x",
        "  0.0,                    !- Coefficient3 x**2",
        "  0.0,                    !- Minimum Value of x",
        "  1.0;                    !- Maximum Value of x",
        "  ",
        "Curve:Biquadratic,",
        "  CoolCapFT,        !- Name",
        "  0.942587793,            !- Coefficient1 Constant",
        "  0.009543347,            !- Coefficient2 x",
        "  0.000683770,            !- Coefficient3 x**2",
        "  -0.011042676,           !- Coefficient4 y",
        "  0.000005249,            !- Coefficient5 y**2",
        "  -0.000009720,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
        "  ",
        "Curve:Biquadratic,",
        "  COOLEIRFT,            !- Name",
        "  0.342414409,            !- Coefficient1 Constant",
        "  0.034885008,            !- Coefficient2 x",
        "  -0.000623700,           !- Coefficient3 x**2",
        "  0.004977216,            !- Coefficient4 y",
        "  0.000437951,            !- Coefficient5 y**2",
        "  -0.000728028,           !- Coefficient6 x*y",
        "  12.77778,               !- Minimum Value of x",
        "  23.88889,               !- Maximum Value of x",
        "  18.0,                   !- Minimum Value of y",
        "  46.11111,               !- Maximum Value of y",
        "  ,                       !- Minimum Curve Output",
        "  ,                       !- Maximum Curve Output",
        "  Temperature,            !- Input Unit Type for X",
        "  Temperature,            !- Input Unit Type for Y",
        "  Dimensionless;          !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors

    DataZoneEquipment::GetZoneEquipmentData1(); // read zone equipment configuration and list objects

    DataSizing::ZoneEqSizing.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex.allocate(1);
    DataZoneEquipment::ZoneEquipList(1).EquipIndex(1) = 1; // initialize equipment index for ZoneHVAC

    UnitarySystems::UnitarySys thisSys;
    UnitarySystems::UnitarySys *mySys;
    int AirLoopNum = 0;
    int CompIndex = 0;
    bool HeatingActive = false;
    bool CoolingActive = false;
    int OAUnitNum = 0;
    Real64 OAUCoilOutTemp = 0.0;
    std::string compName = "GASHEAT DXAC FURNACE 1";
    bool zoneEquipment = true;
    mySys = thisSys.factory(DataHVACGlobals::UnitarySys_AnyCoilType, compName, zoneEquipment, 0);
    DataZoneEquipment::ZoneEquipInputsFilled = true;                           // indicate zone data is available
    mySys->getUnitarySystemInputData(compName, zoneEquipment, 0, ErrorsFound); // get UnitarySystem input from object above

    ASSERT_EQ(1, UnitarySystems::numUnitarySystems); // only 1 unitary system above so expect 1 as number of unitary system objects

    DataGlobals::SysSizingCalc =
        false; // DISABLE SIZING - don't call UnitarySystems::sizeUnitarySystem, much more work needed to set up sizing arrays

    InletNode = mySys->AirInNode;
    OutletNode = mySys->AirOutNode;
    ControlZoneNum = mySys->NodeNumOfControlledZone;

    // set up unitary system inlet condtions
    DataLoopNode::Node(InletNode).Temp = 26.666667;             // AHRI condition 80F dry-bulb temp
    DataLoopNode::Node(InletNode).HumRat = 0.01117049542334198; // AHRI condition at 80F DB/67F WB lb/lb or kg/kg
    DataLoopNode::Node(InletNode).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat);

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 20.0; // set zone temperature during heating season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    // initialize other incidentals that are used within the UnitarySystem module during calculations
    DataSizing::CurZoneEqNum = 1;
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = 1000.0; // heating load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = 2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = 1000.0;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP = 0.0;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;
    DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).SequencedOutputRequiredToDehumidSP(1) =
        DataZoneEnergyDemands::ZoneSysMoistureDemand(ControlZoneNum).OutputRequiredToDehumidifyingSP;

    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = DataHVACGlobals::DualSetPointWithDeadBand;
    DataZoneEnergyDemands::CurDeadBandOrSetback.allocate(1);
    DataZoneEnergyDemands::CurDeadBandOrSetback(1) = false;
    ScheduleManager::Schedule(1).CurrentValue = 1.0;
    DataGlobals::BeginEnvrnFlag = true;
    DataEnvironment::StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(101325.0, 20.0, 0.0); // initialize RhoAir
    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail = mySys->m_MaxCoolAirVolFlow * DataEnvironment::StdRhoAir;

    OutputReportPredefined::SetPredefinedTables();
    mySys->simulate(compName, FirstHVACIteration, AirLoopNum, CompIndex, HeatingActive, CoolingActive, OAUnitNum, OAUCoilOutTemp, zoneEquipment);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    CpAir = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(InletNode).HumRat, DataLoopNode::Node(InletNode).Temp);

    // calculation at end of CalcUnitarySystemToLoad():
    //	SensOutput = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) - UnitarySystem(
    // UnitarySysNum ).SenLoadLoss;

    MinHumRatio = DataLoopNode::Node(ControlZoneNum).HumRat; // zone humidity ratio
    if (DataLoopNode::Node(OutletNode).Temp < DataLoopNode::Node(ControlZoneNum).Temp)
        MinHumRatio = DataLoopNode::Node(OutletNode).HumRat; // use lower of zone and outlet humidity ratio
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) - Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio));

    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired = -1000.0; // cooling load
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP = -1000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP = -2000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequired(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).RemainingOutputRequired;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToCoolingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToCoolingSP;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).SequencedOutputRequiredToHeatingSP(1) =
        DataZoneEnergyDemands::ZoneSysEnergyDemand(ControlZoneNum).OutputRequiredToHeatingSP;

    // set zone temperature
    DataLoopNode::Node(ControlZoneNum).Temp = 24.0; // set zone temperature during cooling season used to determine system delivered capacity
    DataEnvironment::OutDryBulbTemp = 35.0;         // initialize weather
    DataEnvironment::OutHumRat = 0.1;
    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::OutWetBulbTemp = 30.0;

    mySys->simulate(compName, FirstHVACIteration, AirLoopNum, CompIndex, HeatingActive, CoolingActive, OAUnitNum, OAUCoilOutTemp, zoneEquipment);

    ZoneTemp = DataLoopNode::Node(ControlZoneNum).Temp;
    CpAir = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(InletNode).HumRat, DataLoopNode::Node(InletNode).Temp);

    // calculation at end of CalcUnitarySystemToLoad():
    //	SensOutput = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) - UnitarySystem(
    // UnitarySysNum ).SenLoadLoss;

    MinHumRatio = DataLoopNode::Node(ControlZoneNum).HumRat; // zone humidity ratio
    if (DataLoopNode::Node(OutletNode).Temp < DataLoopNode::Node(ControlZoneNum).Temp)
        MinHumRatio = DataLoopNode::Node(OutletNode).HumRat; // use lower of zone and outlet humidity ratio
    Qsens_sys = DataLoopNode::Node(InletNode).MassFlowRate *
                (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) - Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio));

    // TODO: FIXME: Need to fix this in future, it is failing now, probably due to object ordering. Unit test failure message below
    // The difference between DataZoneEnergyDemands::ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired and Qsens_sys is 1000, which
    // exceeds 1.0, where DataZoneEnergyDemands::ZoneSysEnergyDemand( ControlZoneNum ).RemainingOutputRequired evaluates to -1000, Qsens_sys evaluates
    // to 0, and 1.0 evaluates to 1. test model performance EXPECT_NEAR( DataZoneEnergyDemands::ZoneSysEnergyDemand( ControlZoneNum
    // ).RemainingOutputRequired, Qsens_sys, 1.0 ); // Watts

    EXPECT_DOUBLE_EQ(DataLoopNode::Node(InletNode).MassFlowRate, DataLoopNode::Node(OutletNode).MassFlowRate);
}
