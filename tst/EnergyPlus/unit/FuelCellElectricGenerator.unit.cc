// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::FuelCellElectricGenerator Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// C++ Headers
#include <memory>
#include <vector>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SizingManager.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, FuelCellTest)
{

    std::string const idf_objects = delimited_string({

       // "Zone,Thermal Zone 1;",

        "Zone,Thermal Zone 1;",

        "ScheduleTypeLimits,",
        "  OnOff,                                  !- Name",
        "  0,                                      !- Lower Limit Value {BasedOnField A3}",
        "  1,                                      !- Upper Limit Value {BasedOnField A3}",
        "  Discrete,                               !- Numeric Type",
        "  availability;                           !- Unit Type",

        "ScheduleTypeLimits,",
        "  Temperature,                            !- Name",
        "  0,                                      !- Lower Limit Value {BasedOnField A3}",
        "  100,                                    !- Upper Limit Value {BasedOnField A3}",
        "  Continuous,                             !- Numeric Type",
        "  temperature;                            !- Unit Type",

        "Schedule:Constant,",
        "  Always On Discrete,                     !- Name",
        "  OnOff,                                  !- Schedule Type Limits Name",
        "  1;                                      !- Hourly Value",

        "Schedule:Constant,",
        "  Fuel Temperature,                       !- Name",
        "  Temperature,                            !- Schedule Type Limits Name",
        "  20;                                     !- Hourly Value",

        "Schedule:Constant,",
        "  Hot_Water_Temperature,                  !- Name",
        "  Temperature,                            !- Schedule Type Limits Name",
        "  55;                                     !- Hourly Value",

        "Schedule:Constant,",
        "  Water Temperature,                      !- Name",
        "  Temperature,                            !- Schedule Type Limits Name",
        "  20;                                     !- Hourly Value",

        "Schedule:Constant,",
        "  Ambient Temperature 22C,                !- Name",
        "  Temperature,                            !- Schedule Type Limits Name",
        "  22;                                     !- Hourly Value",

        "OutdoorAir:Node,",
        "  Model Outdoor Air Node;                 !- Name",

        "PlantLoop,",
        "  SHW Loop,                               !- Name",
        "  Water,                                  !- Fluid Type",
        "  ,                                       !- User Defined Fluid Type",
        "  SHW Loop Operation Schemes,             !- Plant Equipment Operation Scheme Name",
        "  Node 3,                                 !- Loop Temperature Setpoint Node Name",
        "  100,                                    !- Maximum Loop Temperature {C}",
        "  0,                                      !- Minimum Loop Temperature {C}",
        "  Autosize,                               !- Maximum Loop Flow Rate {m3/s}",
        "  0,                                      !- Minimum Loop Flow Rate {m3/s}",
        "  Autocalculate,                          !- Plant Loop Volume {m3}",
        "  Node 2,                                 !- Plant Side Inlet Node Name",
        "  Node 3,                                 !- Plant Side Outlet Node Name",
        "  SHW Loop Supply Branches,               !- Plant Side Branch List Name",
        "  SHW Loop Supply Connector List,         !- Plant Side Connector List Name",
        "  Node 5,                                 !- Demand Side Inlet Node Name",
        "  Node 6,                                 !- Demand Side Outlet Node Name",
        "  SHW Loop Demand Branches,               !- Demand Side Branch List Name",
        "  SHW Loop Demand Connector List,         !- Demand Side Connector List Name",
        "  Optimal,                                !- Load Distribution Scheme",
        "  ,                                       !- Availability Manager List Name",
        "  SingleSetpoint,                         !- Plant Loop Demand Calculation Scheme",
        "  ;                                       !- Common Pipe Simulation",

        "Sizing:Plant,",
        "  SHW Loop,                               !- Plant or Condenser Loop Name",
        "  Heating,                                !- Loop Type",
        "  82,                                     !- Design Loop Exit Temperature {C}",
        "  11,                                     !- Loop Design Temperature Difference {deltaC}",
        "  NonCoincident,                          !- Sizing Option",
        "  1,                                      !- Zone Timesteps in Averaging Window",
        "  None;                                   !- Coincident Sizing Factor Mode",

        "BranchList,",
        "  SHW Loop Supply Branches,               !- Name",
        "  SHW Loop Supply Inlet Branch,           !- Branch Name 1",
        "  SHW Loop Supply Branch 1,               !- Branch Name 2",
        "  SHW Loop Supply Branch 2,               !- Branch Name 3",
        "  SHW Loop Supply Outlet Branch;          !- Branch Name 4",

        "ConnectorList,",
        "  SHW Loop Supply Connector List,         !- Name",
        "  Connector:Splitter,                     !- Connector Object Type 1",
        "  SHW Loop Supply Splitter,               !- Connector Name 1",
        "  Connector:Mixer,                        !- Connector Object Type 2",
        "  SHW Loop Supply Mixer;                  !- Connector Name 2",

        "Connector:Splitter,",
        "  SHW Loop Supply Splitter,               !- Name",
        "  SHW Loop Supply Inlet Branch,           !- Inlet Branch Name",
        "  SHW Loop Supply Branch 1,               !- Outlet Branch Name 1",
        "  SHW Loop Supply Branch 2;               !- Outlet Branch Name 2",

        "Connector:Mixer,",
        "  SHW Loop Supply Mixer,                  !- Name",
        "  SHW Loop Supply Outlet Branch,          !- Outlet Branch Name",
        "  SHW Loop Supply Branch 1,               !- Inlet Branch Name 1",
        "  SHW Loop Supply Branch 2;               !- Inlet Branch Name 2",

        "Branch,",
        "  SHW Loop Supply Inlet Branch,           !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  Pump:ConstantSpeed,                     !- Component Object Type 1",
        "  Pump Constant Speed 1,                  !- Component Name 1",
        "  Node 2,                                 !- Component Inlet Node Name 1",
        "  Node 8;                                 !- Component Outlet Node Name 1",

        "Pump:ConstantSpeed,",
        "  Pump Constant Speed 1,                  !- Name",
        "  Node 2,                                 !- Inlet Node Name",
        "  Node 8,                                 !- Outlet Node Name",
        "  Autosize,                               !- Design Flow Rate {m3/s}",
        "  179352,                                 !- Design Pump Head {Pa}",
        "  Autosize,                               !- Design Power Consumption {W}",
        "  0.9,                                    !- Motor Efficiency",
        "  0,                                      !- Fraction of Motor Inefficiencies to Fluid Stream",
        "  Intermittent,                           !- Pump Control Type",
        "  ,                                       !- Pump Flow Rate Schedule Name",
        "  ,                                       !- Pump Curve Name",
        "  ,                                       !- Impeller Diameter {m}",
        "  ,                                       !- Rotational Speed {rev/min}",
        "  ,                                       !- Zone Name",
        "  ,                                       !- Skin Loss Radiative Fraction",
        "  PowerPerFlowPerPressure,                !- Design Power Sizing Method",
        "  348701.1,                               !- Design Electric Power per Unit Flow Rate {W/(m3/s)}",
        "  1.282051282,                            !- Design Shaft Power per Unit Flow Rate per Unit Head {W-s/m3-Pa}",
        "  General;                                !- End-Use Subcategory",

        "Branch,",
        "  SHW Loop Supply Branch 1,           !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  WaterHeater:Mixed,                      !- Component Object Type 1",
        "  Water Heater Mixed 1,                   !- Component Name 1",
        "  Node 4,                                 !- Component Inlet Node Name 1",
        "  Node 9;                                 !- Component Outlet Node Name 1",

        "WaterHeater:Mixed,",
        "  Water Heater Mixed 1,                   !- Name",
        "  0.3785,                                 !- Tank Volume {m3}",
        "  Hot_Water_Temperature,                  !- Setpoint Temperature Schedule Name",
        "  2,                                      !- Deadband Temperature Difference {deltaC}",
        "  82.22,                                  !- Maximum Temperature Limit {C}",
        "  Cycle,                                  !- Heater Control Type",
        "  845000,                                 !- Heater Maximum Capacity {W}",
        "  ,                                       !- Heater Minimum Capacity {W}",
        "  0,                                      !- Heater Ignition Minimum Flow Rate {m3/s}",
        "  0,                                      !- Heater Ignition Delay {s}",
        "  NaturalGas,                             !- Heater Fuel Type",
        "  0.8,                                    !- Heater Thermal Efficiency",
        "  ,                                       !- Part Load Factor Curve Name",
        "  20,                                     !- Off Cycle Parasitic Fuel Consumption Rate {W}",
        "  NaturalGas,                             !- Off Cycle Parasitic Fuel Type",
        "  0.8,                                    !- Off Cycle Parasitic Heat Fraction to Tank",
        "  0,                                      !- On Cycle Parasitic Fuel Consumption Rate {W}",
        "  NaturalGas,                             !- On Cycle Parasitic Fuel Type",
        "  0,                                      !- On Cycle Parasitic Heat Fraction to Tank",
        "  Schedule,                               !- Ambient Temperature Indicator",
        "  Ambient Temperature 22C,                !- Ambient Temperature Schedule Name",
        "  ,                                       !- Ambient Temperature Zone Name",
        "  ,                                       !- Ambient Temperature Outdoor Air Node Name",
        "  6,                                      !- Off Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "  1,                                      !- Off Cycle Loss Fraction to Zone",
        "  6,                                      !- On Cycle Loss Coefficient to Ambient Temperature {W/K}",
        "  1,                                      !- On Cycle Loss Fraction to Zone",
        "  ,                                       !- Peak Use Flow Rate {m3/s}",
        "  ,                                       !- Use Flow Rate Fraction Schedule Name",
        "  ,                                       !- Cold Water Supply Temperature Schedule Name",
        "  Node 4,                                 !- Use Side Inlet Node Name",
        "  Node 9,                                 !- Use Side Outlet Node Name",
        "  1,                                      !- Use Side Effectiveness",
        "  ,                                       !- Source Side Inlet Node Name",
        "  ,                                       !- Source Side Outlet Node Name",
        "  1,                                      !- Source Side Effectiveness",
        "  Autosize,                               !- Use Side Design Flow Rate {m3/s}",
        "  Autosize,                               !- Source Side Design Flow Rate {m3/s}",
        "  1.5,                                    !- Indirect Water Heating Recovery Time {hr}",
        "  IndirectHeatPrimarySetpoint,            !- Source Side Flow Control Mode",
        "  ,                                       !- Indirect Alternate Setpoint Temperature Schedule Name",
        "  General;                                !- End-Use Subcategory",

        "Branch,",
        "  SHW Loop Supply Branch 2,           !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  Generator:FuelCell:ExhaustGasToWaterHeatExchanger, !- Component Object Type 1",
        "  Generator Fuel Cell Exhaust Gas To Water Heat Exchanger 1, !- Component Name 1",
        "  Node 10,                                !- Component Inlet Node Name 1",
        "  Node 11;                                !- Component Outlet Node Name 1",

        "Generator:FuelCell:ExhaustGasToWaterHeatExchanger,",
        "  Generator Fuel Cell Exhaust Gas To Water Heat Exchanger 1, !- Name",
        "  Node 10,                                !- Heat Recovery Water Inlet Node Name",
        "  Node 11,                                !- Heat Recovery Water Outlet Node Name",
        "  0.0004,                                 !- Heat Recovery Water Maximum Flow Rate {m3/s}",
        "  Generator Fuel Cell Exhaust Gas To Water Heat Exchanger 1 Exhaust Outlet Air Node, !- Exhaust Outlet Air Node Name",
        "  Condensing,                             !- Heat Exchanger Calculation Method",
        "  ,                                       !- Method 1 Heat Exchanger Effectiveness",
        "  83.1,                                   !- Method 2 Parameter hxs0",
        "  4798,                                   !- Method 2 Parameter hxs1",
        "  -138000,                                !- Method 2 Parameter hxs2",
        "  -353800,                                !- Method 2 Parameter hxs3",
        "  515000000,                              !- Method 2 Parameter hxs4",
        "  ,                                       !- Method 3 h0Gas Coefficient",
        "  ,                                       !- Method 3 NdotGasRef Coefficient",
        "  ,                                       !- Method 3 n Coefficient",
        "  ,                                       !- Method 3 Gas Area {m2}",
        "  ,                                       !- Method 3 h0 Water Coefficient",
        "  ,                                       !- Method 3 N dot Water ref Coefficient",
        "  ,                                       !- Method 3 m Coefficient",
        "  ,                                       !- Method 3 Water Area {m2}",
        "  ,                                       !- Method 3 F Adjustment Factor",
        "  0.0031,                                 !- Method 4 hxl1 Coefficient",
        "  1,                                      !- Method 4 hxl2 Coefficient",
        "  35;                                     !- Method 4 Condensation Threshold {C}",

        "Generator:FuelCell,",
        "  Generator Fuel Cell 1,                  !- Name",
        "  Generator Fuel Cell Power Module 1,     !- Power Module Name",
        "  Generator Fuel Cell Air Supply 1,       !- Air Supply Name",
        "  NATURALGAS,                             !- Fuel Supply Name",
        "  Generator Fuel Cell Water Supply 1,     !- Water Supply Name",
        "  Generator Fuel Cell Auxiliary Heater 1, !- Auxiliary Heater Name",
        "  Generator Fuel Cell Exhaust Gas To Water Heat Exchanger 1, !- Heat Exchanger Name",
        "  Generator Fuel Cell Electrical Storage 1, !- Electrical Storage Name",
        "  Generator Fuel Cell Inverter 1;         !- Inverter Name",

        "Generator:FuelCell:PowerModule,",
        "  Generator Fuel Cell Power Module 1,     !- Name",
        "  Annex42,                                !- Efficiency Curve Mode",
        "  Power Module Efficiency Curve,          !- Efficiency Curve Name",
        "  1,                                      !- Nominal Efficiency",
        "  3400,                                   !- Nominal Electrical Power {W}",
        "  0,                                      !- Number of Stops at Start of Simulation",
        "  0,                                      !- Cycling Performance Degradation Coefficient",
        "  0,                                      !- Number of Run Hours at Beginning of Simulation {hr}",
        "  0,                                      !- Accumulated Run Time Degradation Coefficient",
        "  10000,                                  !- Run Time Degradation Initiation Time Threshold {hr}",
        "  1.4,                                    !- Power Up Transient Limit {W/s}",
        "  0.2,                                    !- Power Down Transient Limit {W/s}",
        "  0,                                      !- Start Up Time {s}",
        "  0.2,                                    !- Start Up Fuel {kmol}",
        "  0,                                      !- Start Up Electricity Consumption {J}",
        "  0,                                      !- Start Up Electricity Produced {J}",
        "  0,                                      !- Shut Down Time {s}",
        "  0.2,                                    !- Shut Down Fuel {kmol}",
        "  0,                                      !- Shut Down Electricity Consumption {J}",
        "  0,                                      !- Ancillary Electricity Constant Term",
        "  0,                                      !- Ancillary Electricity Linear Term",
        "  ConstantRate,                           !- Skin Loss Calculation Mode",
        "  Thermal Zone 1,                         !- Zone Name",
        "  0.6392,                                 !- Skin Loss Radiative Fraction",
        "  729,                                    !- Constant Skin Loss Rate {W}",
        "  0,                                      !- Skin Loss U-Factor Times Area Term {W/K}",
        "  Skin Loss Curve,                        !- Skin Loss Quadratic Curve Name",
        "  0.006156,                               !- Dilution Air Flow Rate {kmol/s}",
        "  2307,                                   !- Stack Heat loss to Dilution Air {W}",
        "  Generator Fuel Cell Power Module 1 OA Node, !- Dilution Inlet Air Node Name",
        "  Generator Fuel Cell Power Module 1 Dilution Outlet Air Node, !- Dilution Outlet Air Node Name",
        "  3010,                                   !- Minimum Operating Point {W}",
        "  3728;                                   !- Maximum Operating Point {W}",

        "OutdoorAir:NodeList,",
        "  Generator Fuel Cell Power Module 1 OA Node; !- Node or NodeList Name 1",

        "Generator:FuelCell:AirSupply,",
        "  Generator Fuel Cell Air Supply 1,       !- Name",
        "  Generator Fuel Cell Air Supply 1 OA Node, !- Air Inlet Node Name",
        "  Blower Power Curve,                     !- Blower Power Curve Name",
        "  1,                                      !- Blower Heat Loss Factor",
        "  AirRatiobyStoics,                       !- Air Supply Rate Calculation Mode",
        "  1,                                      !- Stoichiometric Ratio",
        "  Air Rate Function of Electric Power Curve, !- Air Rate Function of Electric Power Curve Name",
        "  0.00283,                                !- Air Rate Air Temperature Coefficient",
        "  ,                                       !- Air Rate Function of Fuel Rate Curve Name",
        "  NoRecovery,                             !- Air Intake Heat Recovery Mode",
        "  AmbientAir,                             !- Air Supply Constituent Mode",
        "  0;                                      !- Number of UserDefined Constituents",

        "OutdoorAir:NodeList,",
        "  Generator Fuel Cell Air Supply 1 OA Node; !- Node or NodeList Name 1",

        "Generator:FuelSupply,",
        "  NATURALGAS,                             !- Name",
        "  Scheduled,                              !- Fuel Temperature Modeling Mode",
        "  ,                                       !- Fuel Temperature Reference Node Name",
        "  Fuel Temperature,                       !- Fuel Temperature Schedule Name",
        "  Compressor Power Multiplier Function of FuelRate Curve, !- Compressor Power Multiplier Function of Fuel Rate Curve Name",
        "  1,                                      !- Compressor Heat Loss Factor",
        "  GaseousConstituents,                    !- Fuel Type",
        "  ,                                       !- Liquid Generic Fuel Lower Heating Value {kJ/kg}",
        "  ,                                       !- Liquid Generic Fuel Higher Heating Value {kJ/kg}",
        "  ,                                       !- Liquid Generic Fuel Molecular Weight {g/mol}",
        "  ,                                       !- Liquid Generic Fuel CO2 Emission Factor",
        "  8,                                      !- Number of Constituents in Gaseous Constituent Fuel Supply",
        "  METHANE,                                !- Constituent Name 1",
        "  0.949,                                  !- Constituent Molar Fraction 1",
        "  CarbonDioxide,                          !- Constituent Name 2",
        "  0.007,                                  !- Constituent Molar Fraction 2",
        "  NITROGEN,                               !- Constituent Name 3",
        "  0.016,                                  !- Constituent Molar Fraction 3",
        "  ETHANE,                                 !- Constituent Name 4",
        "  0.025,                                  !- Constituent Molar Fraction 4",
        "  PROPANE,                                !- Constituent Name 5",
        "  0.002,                                  !- Constituent Molar Fraction 5",
        "  BUTANE,                                 !- Constituent Name 6",
        "  0.0006,                                 !- Constituent Molar Fraction 6",
        "  PENTANE,                                !- Constituent Name 7",
        "  0.0002,                                 !- Constituent Molar Fraction 7",
        "  OXYGEN,                                 !- Constituent Name 8",
        "  0.0002;                                 !- Constituent Molar Fraction 8",

        "Generator:FuelCell:WaterSupply,",
        "  Generator Fuel Cell Water Supply 1,     !- Name",
        "  Reformer Water FlowRate Function of FuelRate Curve, !- Reformer Water Flow Rate Function of Fuel Rate Curve Name",
        "  Reformer Water Pump Power Function of FuelRate Curve, !- Reformer Water Pump Power Function of Fuel Rate Curve Name",
        "  0,                                      !- Pump Heat Loss Factor",
        "  TemperatureFromSchedule,                !- Water Temperature Modeling Mode",
        "  ,                                       !- Water Temperature Reference Node Name",
        "  Water Temperature;                      !- Water Temperature Schedule Name",

        "Generator:FuelCell:AuxiliaryHeater,",
        "  Generator Fuel Cell Auxiliary Heater 1, !- Name",
        "  0,                                      !- Excess Air Ratio",
        "  0,                                      !- Ancillary Power Constant Term",
        "  0,                                      !- Ancillary Power Linear Term",
        "  0.5,                                    !- Skin Loss U-Factor Times Area Value {W/K}",
        "  AirInletForFuelCell,                    !- Skin Loss Destination",
        "  ,                                       !- Zone Name to Receive Skin Losses",
        "  Watts,                                  !- Heating Capacity Units",
        "  0,                                      !- Maximum Heating Capacity in Watts {W}",
        "  0,                                      !- Minimum Heating Capacity in Watts {W}",
        "  0,                                      !- Maximum Heating Capacity in Kmol per Second {kmol/s}",
        "  0;                                      !- Minimum Heating Capacity in Kmol per Second {kmol/s}",

        "Generator:FuelCell:ElectricalStorage,",
        "  Generator Fuel Cell Electrical Storage 1, !- Name",
        "  SimpleEfficiencyWithConstraints,        !- Choice of Model",
        "  1,                                      !- Nominal Charging Energetic Efficiency",
        "  1,                                      !- Nominal Discharging Energetic Efficiency",
        "  0,                                      !- Simple Maximum Capacity {J}",
        "  0,                                      !- Simple Maximum Power Draw {W}",
        "  0,                                      !- Simple Maximum Power Store {W}",
        "  0;                                      !- Initial Charge State {J}",

        "Generator:FuelCell:Inverter,",
        "  Generator Fuel Cell Inverter 1,         !- Name",
        "  Constant,                               !- Inverter Efficiency Calculation Mode",
        "  1,                                      !- Inverter Efficiency",
        "  Efficiency Function of DC Power Curve;  !- Efficiency Function of DC Power Curve Name",

        "Curve:Cubic,",
        "  Blower Power Curve,                     !- Name",
        "  0,                                      !- Coefficient1 Constant",
        "  0,                                      !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0,                                      !- Coefficient4 x**3",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Curve:Cubic,",
        "  Compressor Power Multiplier Function of FuelRate Curve, !- Name",
        "  0,                                      !- Coefficient1 Constant",
        "  0,                                      !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0,                                      !- Coefficient4 x**3",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Curve:Cubic,",
        "  Reformer Water Pump Power Function of FuelRate Curve, !- Name",
        "  0,                                      !- Coefficient1 Constant",
        "  0,                                      !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  0,                                      !- Coefficient4 x**3",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Curve:Quadratic,",
        "  Air Rate Function of Electric Power Curve, !- Name",
        "  0.00150976,                             !- Coefficient1 Constant",
        "  -7.76656e-07,                           !- Coefficient2 x",
        "  1.30317e-10,                            !- Coefficient3 x**2",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Curve:Quadratic,",
        "  Air Rate Function of Fuel Rate Curve,   !- Name",
        "  0,                                      !- Coefficient1 Constant",
        "  0,                                      !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Curve:Quadratic,",
        "  Efficiency Function of DC Power Curve,  !- Name",
        "  0.560717,                               !- Coefficient1 Constant",
        "  0.00012401,                             !- Coefficient2 x",
        "  -2.01648e-08,                           !- Coefficient3 x**2",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Curve:Quadratic,",
        "  Power Module Efficiency Curve,          !- Name",
        "  0.642388,                               !- Coefficient1 Constant",
        "  -0.0001619,                             !- Coefficient2 x",
        "  2.26e-08,                               !- Coefficient3 x**2",
        "  0,                                      !- Minimum Value of x {BasedOnField A2}",
        "  10000;                                  !- Maximum Value of x {BasedOnField A2}",

        "Curve:Quadratic,",
        "  Reformer Water FlowRate Function of FuelRate Curve, !- Name",
        "  0,                                      !- Coefficient1 Constant",
        "  0,                                      !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Curve:Quadratic,",
        "  Skin Loss Curve,                        !- Name",
        "  0,                                      !- Coefficient1 Constant",
        "  0,                                      !- Coefficient2 x",
        "  0,                                      !- Coefficient3 x**2",
        "  -10000000000,                           !- Minimum Value of x {BasedOnField A2}",
        "  10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "Branch,",
        "  SHW Loop Supply Outlet Branch,          !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  Pipe:Adiabatic,                         !- Component Object Type 1",
        "  SHW Loop Supply Outlet Pipe,            !- Component Name 1",
        "  SHW Loop Supply Outlet Pipe Node,       !- Component Inlet Node Name 1",
        "  Node 3;                                 !- Component Outlet Node Name 1",

        "Pipe:Adiabatic,",
        "  SHW Loop Supply Outlet Pipe,            !- Name",
        "  SHW Loop Supply Outlet Pipe Node,       !- Inlet Node Name",
        "  Node 3;                                 !- Outlet Node Name",

        "BranchList,",
        "  SHW Loop Demand Branches,               !- Name",
        "  SHW Loop Demand Inlet Branch,           !- Branch Name 1",
        "  SHW Loop Demand Branch 1,               !- Branch Name 2",
        "  SHW Loop Demand Bypass Branch,          !- Branch Name 3",
        "  SHW Loop Demand Outlet Branch;          !- Branch Name 4",

        "ConnectorList,",
        "  SHW Loop Demand Connector List,         !- Name",
        "  Connector:Splitter,                     !- Connector Object Type 1",
        "  SHW Loop Demand Splitter,               !- Connector Name 1",
        "  Connector:Mixer,                        !- Connector Object Type 2",
        "  SHW Loop Demand Mixer;                  !- Connector Name 2",

        "Connector:Splitter,",
        "  SHW Loop Demand Splitter,               !- Name",
        "  SHW Loop Demand Inlet Branch,           !- Inlet Branch Name",
        "  SHW Loop Demand Branch 1,               !- Outlet Branch Name 1",
        "  SHW Loop Demand Bypass Branch;          !- Outlet Branch Name 2",

        "Connector:Mixer,",
        "  SHW Loop Demand Mixer,                  !- Name",
        "  SHW Loop Demand Outlet Branch,          !- Outlet Branch Name",
        "  SHW Loop Demand Branch 1,               !- Inlet Branch Name 1",
        "  SHW Loop Demand Bypass Branch;          !- Inlet Branch Name 2",

        "Branch,",
        "  SHW Loop Demand Inlet Branch,           !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  Pipe:Adiabatic,                         !- Component Object Type 1",
        "  SHW Loop Demand Inlet Pipe,             !- Component Name 1",
        "  Node 5,                                 !- Component Inlet Node Name 1",
        "  SHW Loop Demand Inlet Pipe Node;        !- Component Outlet Node Name 1",

        "Pipe:Adiabatic,",
        "  SHW Loop Demand Inlet Pipe,             !- Name",
        "  Node 5,                                 !- Inlet Node Name",
        "  SHW Loop Demand Inlet Pipe Node;        !- Outlet Node Name",

        "Branch,",
        "  SHW Loop Demand Branch 1,               !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  WaterUse:Connections,                   !- Component Object Type 1",
        "  Water Use Connections 1,                !- Component Name 1",
        "  Node 7,                                 !- Component Inlet Node Name 1",
        "  Node 12;                                !- Component Outlet Node Name 1",

        "WaterUse:Connections,",
        "  Water Use Connections 1,                !- Name",
        "  Node 7,                                 !- Inlet Node Name",
        "  Node 12,                                !- Outlet Node Name",
        "  ,                                       !- Supply Water Storage Tank Name",
        "  ,                                       !- Reclamation Water Storage Tank Name",
        "  ,                                       !- Hot Water Supply Temperature Schedule Name",
        "  ,                                       !- Cold Water Supply Temperature Schedule Name",
        "  None,                                   !- Drain Water Heat Exchanger Type",
        "  Plant,                                  !- Drain Water Heat Exchanger Destination",
        "  ,                                       !- Drain Water Heat Exchanger U-Factor Times Area {W/K}",
        "  Water Use Equipment 1;                  !- Water Use Equipment Name 1",

        "WaterUse:Equipment,",
        "  Water Use Equipment 1,                  !- Name",
        "  General,                                !- End-Use Subcategory",
        "  0,                                      !- Peak Flow Rate {m3/s}",
        "  Always On Discrete;                     !- Flow Rate Fraction Schedule Name",

        "Branch,",
        "  SHW Loop Demand Bypass Branch,          !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  Pipe:Adiabatic,                         !- Component Object Type 1",
        "  SHW Loop Demand Bypass Pipe,            !- Component Name 1",
        "  SHW Loop Demand Bypass Pipe Inlet Node, !- Component Inlet Node Name 1",
        "  SHW Loop Demand Bypass Pipe Outlet Node; !- Component Outlet Node Name 1",

        "Pipe:Adiabatic,",
        "  SHW Loop Demand Bypass Pipe,            !- Name",
        "  SHW Loop Demand Bypass Pipe Inlet Node, !- Inlet Node Name",
        "  SHW Loop Demand Bypass Pipe Outlet Node; !- Outlet Node Name",

        "Branch,",
        "  SHW Loop Demand Outlet Branch,          !- Name",
        "  ,                                       !- Pressure Drop Curve Name",
        "  Pipe:Adiabatic,                         !- Component Object Type 1",
        "  SHW Loop Demand Outlet Pipe,            !- Component Name 1",
        "  SHW Loop Demand Outlet Pipe Node,       !- Component Inlet Node Name 1",
        "  Node 6;                                 !- Component Outlet Node Name 1",

        "Pipe:Adiabatic,",
        "  SHW Loop Demand Outlet Pipe,            !- Name",
        "  SHW Loop Demand Outlet Pipe Node,       !- Inlet Node Name",
        "  Node 6;                                 !- Outlet Node Name",

        "PlantEquipmentOperationSchemes,",
        "  SHW Loop Operation Schemes,             !- Name",
        "  PlantEquipmentOperation:HeatingLoad,    !- Control Scheme Object Type 1",
        "  SHW Loop Heating Operation Scheme,      !- Control Scheme Name 1",
        "  Always On Discrete;                     !- Control Scheme Schedule Name 1",

        "PlantEquipmentOperation:HeatingLoad,",
        "  SHW Loop Heating Operation Scheme,      !- Name",
        "  0,                                      !- Load Range Lower Limit 1 {W}",
        "  1000000000,                             !- Load Range Upper Limit 1 {W}",
        "  SHW Loop Heating Equipment List;        !- Range Equipment List Name 1",

        "PlantEquipmentList,",
        "  SHW Loop Heating Equipment List,        !- Name",
        "  WaterHeater:Mixed,                      !- Equipment Object Type 1",
        "  Water Heater Mixed 1,                   !- Equipment Name 1",
        "  Generator:FuelCell:ExhaustGasToWaterHeatExchanger, !- Equipment Object Type 2",
        "  Generator Fuel Cell Exhaust Gas To Water Heat Exchanger 1; !- Equipment Name 2",

        "SetpointManager:Scheduled,",
        "  SHW LWT SPM,                            !- Name",
        "  Temperature,                            !- Control Variable",
        "  Hot_Water_Temperature,                  !- Schedule Name",
        "  Node 3;                                 !- Setpoint Node or NodeList Name",

        "ElectricLoadCenter:Distribution,",
        "  Electric Load Center Distribution 1,    !- Name",
        "  Electric Load Center Distribution 1 Generators, !- Generator List Name",
        "  Baseload,                               !- Generator Operation Scheme Type",
        "  ,                                       !- Generator Demand Limit Scheme Purchased Electric Demand Limit {W}",
        "  ,                                       !- Generator Track Schedule Name Scheme Schedule Name",
        "  ,                                       !- Generator Track Meter Scheme Meter Name",
        "  AlternatingCurrent;                     !- Electrical Buss Type",

        "ElectricLoadCenter:Generators,",
        "  Electric Load Center Distribution 1 Generators, !- Name",
        "  Generator Fuel Cell 1,                  !- Generator Name 1",
        "  Generator:FuelCell,                     !- Generator Object Type 1",
        "  3400,                                   !- Generator Rated Electric Power Output 1 {W}",
        "  ,                                       !- Generator Availability Schedule Name 1",
        "  ;                                       !- Generator Rated Thermal to Electrical Power Ratio 1",

        "  Site:Location,",
        "    CHICAGO_IL_USA TMY2-94846,  !- Name",
        "    42.00,                   !- Latitude {deg}",
        "    -87.88,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190.00;                  !- Elevation {m}",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    4.9,                     !- Wind Speed {m/s}",
        "    270,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    bool ErrorsFound(false);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    InternalHeatGains::GetInternalHeatGainsInput(*state);
    EXPECT_FALSE(InternalHeatGains::ErrorsFound);

    state->dataGlobal->BeginSimFlag = true;
    OutputReportPredefined::SetPredefinedTables(*state);

    OutputProcessor::SetupTimePointers(*state, "Zone", state->dataGlobal->TimeStepZone); // Set up Time pointer for HB/Zone Simulation
    OutputProcessor::SetupTimePointers(*state, "HVAC", DataHVACGlobals::TimeStepSys);

    createFacilityElectricPowerServiceObject();
    OutputProcessor::GetReportVariableInput(*state);
    PlantManager::CheckIfAnyPlant(*state);

    BranchInputManager::ManageBranchInput(*state); // just gets input and returns.
    // Get plant loop data
    PlantManager::GetPlantLoopData(*state);
    PlantManager::GetPlantInput(*state);
    SizingManager::GetPlantSizingInput(*state);
    PlantManager::InitOneTimePlantSizingInfo(*state, 1);
    EXPECT_TRUE(compare_err_stream(""));
    PlantManager::SizePlantLoop(*state, 1, true);
    // SizePlantLoop throws a "   ** Warning ** Water heater = WATER HEATER MIXED 1:  Recovery Efficiency and Energy Factor could not be calculated during the test for standard ratings\n   **   ~~~   ** Setpoint was never recovered and/or heater never turned on\n"
    EXPECT_TRUE(has_err_output(true));


    bool SimElecCircuitsFlag = false;
    // GetInput and other code will be executed and SimElectricCircuits will be true
    facilityElectricServiceObj->manageElectricPowerService(*state, true, SimElecCircuitsFlag, false);
    EXPECT_TRUE(SimElecCircuitsFlag);


    ASSERT_EQ(1u, facilityElectricServiceObj->elecLoadCenterObjs.size());
    EXPECT_EQ(facilityElectricServiceObj->elecLoadCenterObjs[0]->numGenerators,
              1);

    EXPECT_TRUE(compare_err_stream(""));
}
