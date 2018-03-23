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
#include "Fixtures/EnergyPlusFixture.hh"

#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HVACUnitaryBypassVAV.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;

TEST_F( EnergyPlusFixture, UnitaryBypassVAV_GetInputZoneEquipment ) {

	std::string const idf_objects = delimited_string({

	"Version, 8.9;                             !- Version Identifier",
	"Zone,",
	"  Zone 1;                                 !- Name",
	"BuildingSurface:Detailed,",
	"  Surface_1,               !- Name",
	"  WALL,                    !- Surface Type",
	"  EXTWALL80,               !- Construction Name",
	"  Zone 1,                  !- Zone Name",
	"  Outdoors,                !- Outside Boundary Condition",
	"  ,                        !- Outside Boundary Condition Object",
	"  SunExposed,              !- Sun Exposure",
	"  WindExposed,             !- Wind Exposure",
	"  0.5000000,               !- View Factor to Ground",
	"  4,                       !- Number of Vertices",
	"  0,6.096000,3.048000,     !- X,Y,Z ==> Vertex 1 {m}",
	"  0,6.096000,0,            !- X,Y,Z ==> Vertex 2 {m}",
	"  0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
	"  0,0,3.048000;            !- X,Y,Z ==> Vertex 4 {m}",
	"Construction,",
	"  EXTWALL80,               !- Name",
	"  C10 - 8 IN HW CONCRETE;        !- Outside Layer",
	"Material,",
	"  C10 - 8 IN HW CONCRETE,  !- Name",
	"  MediumRough,             !- Roughness",
	"  0.2033016,               !- Thickness {m}",
	"  1.729577,                !- Conductivity {W/m-K}",
	"  2242.585,                !- Density {kg/m3}",
	"  836.8000,                !- Specific Heat {J/kg-K}",
	"  0.9000000,               !- Thermal Absorptance",
	"  0.6500000,               !- Solar Absorptance",
	"  0.6500000;               !- Visible Absorptance",
	"  ZoneControl:Thermostat,",
	"    Zone Thermostat,         !- Name",
	"    Zone 1,           !- Zone or ZoneList Name",
	"    Dual Zone Control Type Sched,  !- Control Type Schedule Name",
	"    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
	"    Setpoints;               !- Control 1 Name",

	"  ThermostatSetpoint:DualSetpoint,",
	"    Setpoints,               !- Name",
	"    Dual Heating Setpoints,  !- Heating Setpoint Temperature Schedule Name",
	"    Dual Cooling Setpoints;  !- Cooling Setpoint Temperature Schedule Name",
	"  Schedule:Compact,",
	"    Dual Heating Setpoints,  !- Name",
	"    Temperature,             !- Schedule Type Limits Name",
	"    Through: 12/31,          !- Field 1",
	"    For: AllDays,            !- Field 2",
	"    Until: 24:00,            !- Field 3",
	"    23.0;                    !- Field 4",

	"  Schedule:Compact,",
	"    Dual Cooling Setpoints,  !- Name",
	"    Temperature,             !- Schedule Type Limits Name",
	"    Through: 12/31,          !- Field 1",
	"    For: AllDays,            !- Field 2",
	"    Until: 24:00,            !- Field 3",
	"    23.0;                    !- Field 4",

	"  Schedule:Compact,",
	"    Dual Zone Control Type Sched,  !- Name",
	"    Control Type,            !- Schedule Type Limits Name",
	"    Through: 12/31,          !- Field 1",
	"    For: AllDays,            !- Field 2",
	"    Until: 24:00,            !- Field 3",
	"    4;                       !- Field 4",
	"ZoneHVAC:EquipmentConnections,",
	"  Zone 1,                                 !- Zone Name",
	"  Zone 1 Equipment List,                  !- Zone Conditioning Equipment List Name",
	"  Zone 1 Inlet Node List,                 !- Zone Air Inlet Node or NodeList Name",
	"  ,                                       !- Zone Air Exhaust Node or NodeList Name",
	"  Zone 1 Zone Air Node,                   !- Zone Air Node Name",
	"  Zone 1 Zone Return Air Node;            !- Zone Return Air Node or NodeList Name",
	"",
	"NodeList,",
	"  Zone 1 Inlet Node List,                 !- Name",
	"  Zone 1 Dummy Inlet Node,",
	"  Zone 1 ATU VAVHeatAndCoolNoReheat Outlet Node; !- Node Name 1",
	"",
	"ZoneHVAC:AirDistributionUnit,",
	"  ADU Zone 1 ATU VAVHeatAndCoolNoReheat,  !- Name",
	"  Zone 1 ATU VAVHeatAndCoolNoReheat Outlet Node, !- Air Distribution Unit Outlet Node Name",
	"  AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat, !- Air Terminal Object Type",
	"  Zone 1 ATU VAVHeatAndCoolNoReheat;      !- Air Terminal Name",
	"",
	"AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat,",
	"  Zone 1 ATU VAVHeatAndCoolNoReheat,      !- Name",
	"  ,                                       !- Availability Schedule Name",
	"  Zone 1 ATU VAVHeatAndCoolNoReheat Outlet Node, !- Air Outlet Node Name",
	"  Zone 1 ATU VAVHeatAndCoolNoReheat Inlet Node, !- Air Inlet Node Name",
	"  0.02,                               !- Maximum Air Flow Rate {m3/s}",
	"  0;                                      !- Zone Minimum Air Flow Fraction",
	"",
	"ZoneHVAC:EquipmentList,",
	"  Zone 1 Equipment List,                  !- Name",
	"  ,                                       !- Load Distribution Scheme",
	"  ZoneHVAC:AirDistributionUnit,           !- Zone Equipment Object Type 1",
	"  ADU Zone 1 ATU VAVHeatAndCoolNoReheat,  !- Zone Equipment Name 1",
	"  1,                                      !- Zone Equipment Cooling Sequence 1",
	"  1;                                      !- Zone Equipment Heating or No-Load Sequence 1",
	"OutdoorAir:Node,",
	"  Model Outdoor Air Node;                 !- Name",
	"",
	"AirLoopHVAC,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop, !- Name",
	"  ,                                       !- Controller List Name",
	"  , !- Availability Manager List Name",
	"  0.02,                               !- Design Supply Air Flow Rate {m3/s}",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Branches, !- Branch List Name",
	"  ,                                       !- Connector List Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Inlet Node, !- Supply Side Inlet Node Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Outlet Node, !- Demand Side Outlet Node Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Nodes, !- Demand Side Inlet Node Names",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Nodes; !- Supply Side Outlet Node Names",
	"",
	"NodeList,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Nodes, !- Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Node; !- Node Name 1",
	"",
	"NodeList,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Nodes, !- Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node; !- Node Name 1",
	"",
	"BranchList,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Branches, !- Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Main Branch; !- Branch Name 1",
	"",
	"Branch,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Main Branch, !- Name",
	"  ,                                       !- Pressure Drop Curve Name",
	"  AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass, !- Component Object Type 1",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1, !- Component Name 1",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Inlet Node, !- Component Inlet Node Name 1",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Node; !- Component Outlet Node Name 1",
	"",
	"AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass,",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1, !- Name",
	"  ,                                       !- Availability Schedule Name",
	"  0.02,                               !- Cooling Supply Air Flow Rate {m3/s}",
	"  0.02,                               !- Heating Supply Air Flow Rate {m3/s}",
	"  0.02,                               !- No Load Supply Air Flow Rate {m3/s}",
	"  0.01,                               !- Cooling Outdoor Air Flow Rate {m3/s}",
	"  0.01,                               !- Heating Outdoor Air Flow Rate {m3/s}",
	"  0.01,                               !- No Load Outdoor Air Flow Rate {m3/s}",
	"  ,                                       !- Outdoor Air Flow Rate Multiplier Schedule Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Inlet Node, !- Air Inlet Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Mixer Node, !- Bypass Duct Mixer Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Splitter Node, !- Bypass Duct Splitter Node Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Supply Outlet Node, !- Air Outlet Node Name",
	"  OutdoorAir:Mixer,                       !- Outdoor Air Mixer Object Type",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Outdoor Air Mixer, !- Outdoor Air Mixer Name",
	"  Fan:ConstantVolume,                     !- Supply Air Fan Object Type",
	"  Fan Constant Volume 1,                  !- Supply Air Fan Name",
	"  DrawThrough,                            !- Supply Air Fan Placement",
	"  ,                                       !- Supply Air Fan Operating Mode Schedule Name",
	"  Coil:Cooling:DX:SingleSpeed,            !- Cooling Coil Object Type",
	"  Coil Cooling DX Single Speed 1,         !- Cooling Coil Name",
	"  Coil:Heating:Fuel,                      !- Heating Coil Object Type",
	"  Coil Heating Gas 1,                     !- Heating Coil Name",
	"  ZonePriority,                           !- Priority Control Mode",
	"  8,                                      !- Minimum Outlet Air Temperature During Cooling Operation {C}",
	"  50,                                     !- Maximum Outlet Air Temperature During Heating Operation {C}",
	"  None;                                   !- Dehumidification Control Type",
	"",
	"Fan:ConstantVolume,",
	"  Fan Constant Volume 1,                  !- Name",
	"  ,                                       !- Availability Schedule Name",
	"  0.7,                                    !- Fan Total Efficiency",
	"  250,                                    !- Pressure Rise {Pa}",
	"  0.02,                               !- Maximum Flow Rate {m3/s}",
	"  0.9,                                    !- Motor Efficiency",
	"  1,                                      !- Motor In Airstream Fraction",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Heating Coil Outlet Node, !- Air Inlet Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Splitter Node; !- Air Outlet Node Name",
	"",
	"Coil:Heating:Fuel,",
	"  Coil Heating Gas 1,                     !- Name",
	"  ,                                       !- Availability Schedule Name",
	"  NaturalGas,                             !- Fuel Type",
	"  0.8,                                    !- Burner Efficiency",
	"  1000.0,                               !- Nominal Capacity {W}",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Cooling Coil Outlet Node, !- Air Inlet Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Heating Coil Outlet Node, !- Air Outlet Node Name",
	"  ,                                       !- Temperature Setpoint Node Name",
	"  0,                                      !- Parasitic Electric Load {W}",
	"  ,                                       !- Part Load Fraction Correlation Curve Name",
	"  0;                                      !- Parasitic Fuel Load {W}",
	"",
	"Curve:Biquadratic,",
	"  Curve Biquadratic 1,                    !- Name",
	"  0.942587793,                            !- Coefficient1 Constant",
	"  0.009543347,                            !- Coefficient2 x",
	"  0.00068377,                             !- Coefficient3 x**2",
	"  -0.011042676,                           !- Coefficient4 y",
	"  5.249e-06,                              !- Coefficient5 y**2",
	"  -9.72e-06,                              !- Coefficient6 x*y",
	"  17,                                     !- Minimum Value of x {BasedOnField A2}",
	"  22,                                     !- Maximum Value of x {BasedOnField A2}",
	"  13,                                     !- Minimum Value of y {BasedOnField A3}",
	"  46;                                     !- Maximum Value of y {BasedOnField A3}",
	"",
	"Curve:Quadratic,",
	"  Curve Quadratic 1,                      !- Name",
	"  0.8,                                    !- Coefficient1 Constant",
	"  0.2,                                    !- Coefficient2 x",
	"  0,                                      !- Coefficient3 x**2",
	"  0.5,                                    !- Minimum Value of x {BasedOnField A2}",
	"  1.5;                                    !- Maximum Value of x {BasedOnField A2}",
	"",
	"Curve:Biquadratic,",
	"  Curve Biquadratic 2,                    !- Name",
	"  0.342414409,                            !- Coefficient1 Constant",
	"  0.034885008,                            !- Coefficient2 x",
	"  -0.0006237,                             !- Coefficient3 x**2",
	"  0.004977216,                            !- Coefficient4 y",
	"  0.000437951,                            !- Coefficient5 y**2",
	"  -0.000728028,                           !- Coefficient6 x*y",
	"  17,                                     !- Minimum Value of x {BasedOnField A2}",
	"  22,                                     !- Maximum Value of x {BasedOnField A2}",
	"  13,                                     !- Minimum Value of y {BasedOnField A3}",
	"  46;                                     !- Maximum Value of y {BasedOnField A3}",
	"",
	"Curve:Quadratic,",
	"  Curve Quadratic 2,                      !- Name",
	"  1.1552,                                 !- Coefficient1 Constant",
	"  -0.1808,                                !- Coefficient2 x",
	"  0.0256,                                 !- Coefficient3 x**2",
	"  0.5,                                    !- Minimum Value of x {BasedOnField A2}",
	"  1.5;                                    !- Maximum Value of x {BasedOnField A2}",
	"",
	"Curve:Quadratic,",
	"  Curve Quadratic 3,                      !- Name",
	"  0.85,                                   !- Coefficient1 Constant",
	"  0.15,                                   !- Coefficient2 x",
	"  0,                                      !- Coefficient3 x**2",
	"  0,                                      !- Minimum Value of x {BasedOnField A2}",
	"  1;                                      !- Maximum Value of x {BasedOnField A2}",
	"",
	"Coil:Cooling:DX:SingleSpeed,",
	"  Coil Cooling DX Single Speed 1,         !- Name",
	"  ,                                       !- Availability Schedule Name",
	"  1000,                               !- Gross Rated Total Cooling Capacity {W}",
	"  0.7,                               !- Gross Rated Sensible Heat Ratio",
	"  3,                                      !- Gross Rated Cooling COP {W/W}",
	"  0.02,                               !- Rated Air Flow Rate {m3/s}",
	"  773.3,                                  !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Mixed Air Node, !- Air Inlet Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Cooling Coil Outlet Node, !- Air Outlet Node Name",
	"  Curve Biquadratic 1,                    !- Total Cooling Capacity Function of Temperature Curve Name",
	"  Curve Quadratic 1,                      !- Total Cooling Capacity Function of Flow Fraction Curve Name",
	"  Curve Biquadratic 2,                    !- Energy Input Ratio Function of Temperature Curve Name",
	"  Curve Quadratic 2,                      !- Energy Input Ratio Function of Flow Fraction Curve Name",
	"  Curve Quadratic 3,                      !- Part Load Fraction Correlation Curve Name",
	"  ,                                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
	"  ,                                       !- Nominal Time for Condensate Removal to Begin {s}",
	"  ,                                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
	"  ,                                       !- Maximum Cycling Rate {cycles/hr}",
	"  ,                                       !- Latent Capacity Time Constant {s}",
	"  ,                                       !- Condenser Air Inlet Node Name",
	"  EvaporativelyCooled,                    !- Condenser Type",
	"  0,                                      !- Evaporative Condenser Effectiveness {dimensionless}",
	"  Autosize,                               !- Evaporative Condenser Air Flow Rate {m3/s}",
	"  Autosize,                               !- Evaporative Condenser Pump Rated Power Consumption {W}",
	"  0,                                      !- Crankcase Heater Capacity {W}",
	"  0,                                      !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
	"  ,                                       !- Supply Water Storage Tank Name",
	"  ,                                       !- Condensate Collection Water Storage Tank Name",
	"  0,                                      !- Basin Heater Capacity {W/K}",
	"  10;                                     !- Basin Heater Setpoint Temperature {C}",
	"",
	"OutdoorAir:Mixer,",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Outdoor Air Mixer, !- Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Mixed Air Node, !- Mixed Air Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 OA Node, !- Outdoor Air Stream Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Relief Air Node, !- Relief Air Stream Node Name",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 Bypass Duct Mixer Node; !- Return Air Stream Node Name",
	"",
	"OutdoorAir:NodeList,",
	"  Air Loop HVAC Unitary Heat Cool VAVChangeover Bypass 1 OA Node; !- Node or NodeList Name 1",
	"",
	"AirLoopHVAC:SupplyPath,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node Supply Path, !- Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node, !- Supply Air Path Inlet Node Name",
	"  AirLoopHVAC:ZoneSplitter,               !- Component Object Type 1",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Splitter; !- Component Name 1",
	"",
	"AirLoopHVAC:ZoneSplitter,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Splitter, !- Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Inlet Node, !- Inlet Node Name",
	"  Zone 1 ATU VAVHeatAndCoolNoReheat Inlet Node; !- Outlet Node Name 1",
	"",
	"AirLoopHVAC:ReturnPath,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Return Path, !- Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Outlet Node, !- Return Air Path Outlet Node Name",
	"  AirLoopHVAC:ZoneMixer,                  !- Component Object Type 1",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Mixer; !- Component Name 1",
	"",
	"AirLoopHVAC:ZoneMixer,",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Mixer, !- Name",
	"  UnitaryHeatCoolVAVChangeoverBypass Loop Demand Outlet Node, !- Outlet Node Name",
	"  Zone 1 Zone Return Air Node;            !- Inlet Node Name 1",
	} );

	ASSERT_TRUE( process_idf( idf_objects ) ); // read idf objects

	bool ErrorsFound = false;
	bool firstHVACIteration = true;
	// Read objects
	SimulationManager::GetProjectData();
	HeatBalanceManager::GetProjectControlData(ErrorsFound);
	EXPECT_FALSE(ErrorsFound);
	HeatBalanceManager::GetZoneData(ErrorsFound);
	EXPECT_FALSE(ErrorsFound);
	HeatBalanceManager::GetMaterialData(ErrorsFound);
	EXPECT_FALSE(ErrorsFound);
	HeatBalanceManager::GetConstructData(ErrorsFound);
	EXPECT_FALSE(ErrorsFound);
	HeatBalanceManager::GetHeatBalanceInput();
	HeatBalanceManager::AllocateHeatBalArrays();
	HeatBalanceManager::GetZoneData( ErrorsFound );
	ASSERT_FALSE( ErrorsFound );
	HeatBalanceManager::AllocateHeatBalArrays();
	ZoneTempPredictorCorrector::InitZoneAirSetPoints();
	bool simZone = false;
	bool simAir = false;
	DataHeatBalance::MassConservation.allocate( DataGlobals::NumOfZones);
	ZoneEquipmentManager::ManageZoneEquipment( firstHVACIteration, simZone, simAir );
	SimAirServingZones::GetAirPathData();
	SplitterComponent::GetSplitterInput();
	SimAirServingZones::InitAirLoops( firstHVACIteration );


	HVACUnitaryBypassVAV::GetCBVAV(); // get UnitarySystem input from object above
	auto & cbvav ( HVACUnitaryBypassVAV::CBVAV( 1 ) );
	EXPECT_EQ( 1, cbvav.ControlledZoneNum( 1 ) );
	EXPECT_EQ( 1, cbvav.ActualZoneNum( 1 ) );
	EXPECT_EQ( 1, cbvav.ZoneSequenceCoolingNum( 1 ) );
	EXPECT_EQ( 1, cbvav.ZoneSequenceHeatingNum( 1 ) );

}
