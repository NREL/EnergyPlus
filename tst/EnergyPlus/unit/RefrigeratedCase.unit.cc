// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataZoneEquipment.hh>

// I should not have to import these, but I do, the headers don't declare them
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/RefrigeratedCase.hh>

// Standard headers
#include <fmt/format.h>
#include <string>
#include <string_view>

using namespace EnergyPlus;

static std::string_view constexpr oneZoneBuildingWithIdealLoads = R"IDF(
ScheduleTypeLimits,
  Any Number;                             !- Name

Schedule:Constant,
  Always On,                              !- Name
  Any Number,                             !- Schedule Type Limits Name
  1;                                      !- Hourly Value

Schedule:Constant,
  Always Off,                             !- Name
  Any Number,                             !- Schedule Type Limits Name
  0;                                      !- Hourly Value

Zone,
  Zone_1,                                 !- Name
  ,                                       !- Direction of Relative North {deg}
  0,                                      !- X Origin {m}
  0,                                      !- Y Origin {m}
  0,                                      !- Z Origin {m}
  ,                                       !- Type
  1,                                      !- Multiplier
  ,                                       !- Ceiling Height {m}
  ,                                       !- Volume {m3}
  ,                                       !- Floor Area {m2}
  ,                                       !- Zone Inside Convection Algorithm
  ,                                       !- Zone Outside Convection Algorithm
  Yes;                                    !- Part of Total Floor Area

Space,
  Space_1,                                !- Name
  Zone_1,                                 !- Zone Name
  ,                                       !- Ceiling Height {m}
  ,                                       !- Volume {m3}
  ;                                       !- Floor Area {m2}

BuildingSurface:Detailed,
  Space_1 Floor,                          !- Name
  Floor,                                  !- Surface Type
  Construction 1,                         !- Construction Name
  Zone_1,                                 !- Zone Name
  Space_1,                                !- Space Name
  Ground,                                 !- Outside Boundary Condition
  ,                                       !- Outside Boundary Condition Object
  NoSun,                                  !- Sun Exposure
  NoWind,                                 !- Wind Exposure
  ,                                       !- View Factor to Ground
  ,                                       !- Number of Vertices
  10, 5, 0,                               !- X,Y,Z Vertex 1 {m}
  10, -5, 0,                              !- X,Y,Z Vertex 2 {m}
  -10, -5, 0,                             !- X,Y,Z Vertex 3 {m}
  -10, 5, 0;                              !- X,Y,Z Vertex 4 {m}

BuildingSurface:Detailed,
  Space_1 RoofCeiling,                    !- Name
  Roof,                                   !- Surface Type
  Construction 1,                         !- Construction Name
  Zone_1,                                 !- Zone Name
  Space_1,                                !- Space Name
  Outdoors,                               !- Outside Boundary Condition
  ,                                       !- Outside Boundary Condition Object
  SunExposed,                             !- Sun Exposure
  WindExposed,                            !- Wind Exposure
  ,                                       !- View Factor to Ground
  ,                                       !- Number of Vertices
  -10, 5, 3,                              !- X,Y,Z Vertex 1 {m}
  -10, -5, 3,                             !- X,Y,Z Vertex 2 {m}
  10, -5, 3,                              !- X,Y,Z Vertex 3 {m}
  10, 5, 3;                               !- X,Y,Z Vertex 4 {m}

BuildingSurface:Detailed,
  Space_1 Wall 1,                         !- Name
  Wall,                                   !- Surface Type
  Construction 1,                         !- Construction Name
  Zone_1,                                 !- Zone Name
  Space_1,                                !- Space Name
  Outdoors,                               !- Outside Boundary Condition
  ,                                       !- Outside Boundary Condition Object
  SunExposed,                             !- Sun Exposure
  WindExposed,                            !- Wind Exposure
  ,                                       !- View Factor to Ground
  ,                                       !- Number of Vertices
  10, 5, 3,                               !- X,Y,Z Vertex 1 {m}
  10, -5, 3,                              !- X,Y,Z Vertex 2 {m}
  10, -5, 0,                              !- X,Y,Z Vertex 3 {m}
  10, 5, 0;                               !- X,Y,Z Vertex 4 {m}

BuildingSurface:Detailed,
  Space_1 Wall 2,                         !- Name
  Wall,                                   !- Surface Type
  Construction 1,                         !- Construction Name
  Zone_1,                                 !- Zone Name
  Space_1,                                !- Space Name
  Outdoors,                               !- Outside Boundary Condition
  ,                                       !- Outside Boundary Condition Object
  SunExposed,                             !- Sun Exposure
  WindExposed,                            !- Wind Exposure
  ,                                       !- View Factor to Ground
  ,                                       !- Number of Vertices
  10, -5, 3,                              !- X,Y,Z Vertex 1 {m}
  -10, -5, 3,                             !- X,Y,Z Vertex 2 {m}
  -10, -5, 0,                             !- X,Y,Z Vertex 3 {m}
  10, -5, 0;                              !- X,Y,Z Vertex 4 {m}

BuildingSurface:Detailed,
  Space_1 Wall 3,                         !- Name
  Wall,                                   !- Surface Type
  Construction 1,                         !- Construction Name
  Zone_1,                                 !- Zone Name
  Space_1,                                !- Space Name
  Outdoors,                               !- Outside Boundary Condition
  ,                                       !- Outside Boundary Condition Object
  SunExposed,                             !- Sun Exposure
  WindExposed,                            !- Wind Exposure
  ,                                       !- View Factor to Ground
  ,                                       !- Number of Vertices
  -10, -5, 3,                             !- X,Y,Z Vertex 1 {m}
  -10, 5, 3,                              !- X,Y,Z Vertex 2 {m}
  -10, 5, 0,                              !- X,Y,Z Vertex 3 {m}
  -10, -5, 0;                             !- X,Y,Z Vertex 4 {m}

BuildingSurface:Detailed,
  Space_1 Wall 4,                         !- Name
  Wall,                                   !- Surface Type
  Construction 1,                         !- Construction Name
  Zone_1,                                 !- Zone Name
  Space_1,                                !- Space Name
  Outdoors,                               !- Outside Boundary Condition
  ,                                       !- Outside Boundary Condition Object
  SunExposed,                             !- Sun Exposure
  WindExposed,                            !- Wind Exposure
  ,                                       !- View Factor to Ground
  ,                                       !- Number of Vertices
  -10, 5, 3,                              !- X,Y,Z Vertex 1 {m}
  10, 5, 3,                               !- X,Y,Z Vertex 2 {m}
  10, 5, 0,                               !- X,Y,Z Vertex 3 {m}
  -10, 5, 0;                              !- X,Y,Z Vertex 4 {m}

Material,
  Material 1,                             !- Name
  Smooth,                                 !- Roughness
  0.1,                                    !- Thickness {m}
  0.1,                                    !- Conductivity {W/m-K}
  0.1,                                    !- Density {kg/m3}
  1400,                                   !- Specific Heat {J/kg-K}
  0.9,                                    !- Thermal Absorptance
  0.7,                                    !- Solar Absorptance
  0.7;                                    !- Visible Absorptance

Construction,
  Construction 1,                         !- Name
  Material 1;                             !- Layer 1

ZoneHVAC:EquipmentConnections,
  Zone_1,                                 !- Zone Name
  Zone_1 Equipment List,                  !- Zone Conditioning Equipment List Name
  Zone_1 Inlet Node List,                 !- Zone Air Inlet Node or NodeList Name
  Zone_1 Exhaust Node List,               !- Zone Air Exhaust Node or NodeList Name
  Node 1;                                 !- Zone Air Node Name

NodeList,
  Zone_1 Inlet Node List,                 !- Name
  Node 3;                                 !- Node Name 1

NodeList,
  Zone_1 Exhaust Node List,               !- Name
  Node 2;                                 !- Node Name 1

ZoneHVAC:IdealLoadsAirSystem,
  Zone HVAC Ideal Loads Air System 1,     !- Name
  ,                                       !- Availability Schedule Name
  Node 3,                                 !- Zone Supply Air Node Name
  Node 2,                                 !- Zone Exhaust Air Node Name
  ,                                       !- System Inlet Air Node Name
  ,                                       !- Maximum Heating Supply Air Temperature {C}
  ,                                       !- Minimum Cooling Supply Air Temperature {C}
  ,                                       !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}
  ,                                       !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}
  ,                                       !- Heating Limit
  ,                                       !- Maximum Heating Air Flow Rate {m3/s}
  ,                                       !- Maximum Sensible Heating Capacity {W}
  ,                                       !- Cooling Limit
  ,                                       !- Maximum Cooling Air Flow Rate {m3/s}
  ,                                       !- Maximum Total Cooling Capacity {W}
  ,                                       !- Heating Availability Schedule Name
  ,                                       !- Cooling Availability Schedule Name
  ,                                       !- Dehumidification Control Type
  ,                                       !- Cooling Sensible Heat Ratio {dimensionless}
  ,                                       !- Humidification Control Type
  ,                                       !- Design Specification Outdoor Air Object Name
  ,                                       !- Outdoor Air Inlet Node Name
  ,                                       !- Demand Controlled Ventilation Type
  ,                                       !- Outdoor Air Economizer Type
  ,                                       !- Heat Recovery Type
  ,                                       !- Sensible Heat Recovery Effectiveness {dimensionless}
  ;                                       !- Latent Heat Recovery Effectiveness {dimensionless}

ZoneHVAC:EquipmentList,
  Zone_1 Equipment List,                  !- Name
  SequentialLoad,                         !- Load Distribution Scheme
  ZoneHVAC:IdealLoadsAirSystem,           !- Zone Equipment Object Type 1
  Zone HVAC Ideal Loads Air System 1,     !- Zone Equipment Name 1
  1,                                      !- Zone Equipment Cooling Sequence 1
  1,                                      !- Zone Equipment Heating or No-Load Sequence 1
  ,                                       !- Zone Equipment Sequential Cooling Fraction Schedule Name 1
  ;                                       !- Zone Equipment Sequential Heating Fraction Schedule Name 1
)IDF";

TEST_F(EnergyPlusFixture, RefrigeratedRackWithCaseInZone)
{
    std::string_view constexpr idf_objects = R"IDF(
Refrigeration:CompressorRack,
  SelfContainedDisplay,    !- Name
  Zone,                    !- Heat Rejection Location
  4.0,                     !- Design Compressor Rack COP {W/W}
  RackCOPfTCurve2,         !- Compressor Rack COP Function of Temperature Curve Name
  175.0,                   !- Design Condenser Fan Power {W}
  ,                        !- Condenser Fan Power Function of Temperature Curve Name
  AirCooled,               !- Condenser Type
  ,                        !- Water-Cooled Condenser Inlet Node Name
  ,                        !- Water-Cooled Condenser Outlet Node Name
  ,                        !- Water-Cooled Loop Flow Type
  ,                        !- Water-Cooled Condenser Outlet Temperature Schedule Name
  ,                        !- Water-Cooled Condenser Design Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Water Outlet Temperature {C}
  ,                        !- Water-Cooled Condenser Minimum Water Inlet Temperature {C}
  ,                        !- Evaporative Condenser Availability Schedule Name
  ,                        !- Evaporative Condenser Effectiveness {dimensionless}
  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}
  ,                        !- Basin Heater Capacity {W/K}
  ,                        !- Basin Heater Setpoint Temperature {C}
  ,                        !- Design Evaporative Condenser Water Pump Power {W}
  ,                        !- Evaporative Water Supply Tank Name
  ,                        !- Condenser Air Inlet Node Name
  ,                        !- End-Use Subcategory
  SelfContainedDisplayCase,!- Refrigeration Case Name or WalkIn Name or CaseAndWalkInList Name
  ZONE_1;                  !- Heat Rejection Zone Name


Curve:Quadratic,
  RackCOPfTCurve2,         !- Name
  1.0,                     !- Coefficient1 Constant
  0.0,                     !- Coefficient2 x
  0.0,                     !- Coefficient3 x**2
  0.0,                     !- Minimum Value of x
  50.0,                    !- Maximum Value of x
  ,                        !- Minimum Curve Output
  ,                        !- Maximum Curve Output
  Temperature,             !- Input Unit Type for X
  Dimensionless;           !- Output Unit Type

Refrigeration:Case,
  SelfContainedDisplayCase,!- Name
  ,                        !- Availability Schedule Name
  ZONE_1,                  !- Zone Name
  23.88,                   !- Rated Ambient Temperature {C}
  55.0,                    !- Rated Ambient Relative Humidity {percent}
  1000.0,                  !- Rated Total Cooling Capacity per Unit Length {W/m}
  0.08,                    !- Rated Latent Heat Ratio
  0.85,                    !- Rated Runtime Fraction
  10.0,                    !- Case Length {m}
  13.0,                    !- Case Operating Temperature {C}
  CaseTemperatureMethod,   !- Latent Case Credit Curve Type
  MultiShelfVertical_LatentEnergyMult,  !- Latent Case Credit Curve Name
  40.0,                    !- Standard Case Fan Power per Unit Length {W/m}
  40.0,                    !- Operating Case Fan Power per Unit Length {W/m}
  75.0,                    !- Standard Case Lighting Power per Unit Length {W/m}
  ,                        !- Installed Case Lighting Power per Unit Length {W/m}
  Always On,               !- Case Lighting Schedule Name
  0.9,                     !- Fraction of Lighting Energy to Case
  0.0,                     !- Case Anti-Sweat Heater Power per Unit Length {W/m}
  ,                        !- Minimum Anti-Sweat Heater Power per Unit Length {W/m}
  None,                    !- Anti-Sweat Heater Control Type
  0.0,                     !- Humidity at Zero Anti-Sweat Heater Energy {percent}
  0.0,                     !- Case Height {m}
  0.0,                     !- Fraction of Anti-Sweat Heater Energy to Case
  0.0,                     !- Case Defrost Power per Unit Length {W/m}
  None,                    !- Case Defrost Type
  ,                        !- Case Defrost Schedule Name
  ,                        !- Case Defrost Drip-Down Schedule Name
  ,                        !- Defrost Energy Correction Curve Type
  ,                        !- Defrost Energy Correction Curve Name
  0.0,                     !- Under Case HVAC Return Air Fraction
  SelfContainedCaseStockingSched;  !- Refrigerated Case Restocking Schedule Name

Curve:Cubic,
  MultiShelfVertical_LatentEnergyMult,  !- Name
  0.026526281,             !- Coefficient1 Constant
  0.001078032,             !- Coefficient2 x
  -0.0000602558,           !- Coefficient3 x**2
  0.00000123732,           !- Coefficient4 x**3
  -35.0,                   !- Minimum Value of x
  20.0;                    !- Maximum Value of x

Schedule:Compact,
  SelfContainedCaseStockingSched,  !- Name
  AnyNumber,               !- Schedule Type Limits Name
  Through: 12/31,          !- Field 1
  For: AllDays,            !- Field 2
  Until: 6:00,0.0,         !- Field 3
  Until: 7:00,50.0,        !- Field 5
  Until: 9:00,70.0,        !- Field 7
  Until: 10:00,80.0,       !- Field 9
  Until: 11:00,70.0,       !- Field 11
  Until: 13:00,50.0,       !- Field 13
  Until: 14:00,80.0,       !- Field 15
  Until: 15:00,90.0,       !- Field 17
  Until: 16:00,80.0,       !- Field 19
  Until: 24:00,0.0;        !- Field 21
)IDF";

    ASSERT_TRUE(process_idf(fmt::format("{}\n{}", oneZoneBuildingWithIdealLoads, idf_objects))); // read idf objects

    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataEnvrn->OutBaroPress = 101325.0;
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ScheduleManager::ProcessScheduleInput(*state);
    // ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    InternalHeatGains::ManageInternalHeatGains(*state, true);
    RefrigeratedCase::ManageRefrigeratedCaseRacks(*state);
}

TEST_F(EnergyPlusFixture, RefrigeratedRackWithWalkInInZone)
{

    std::string_view constexpr idf_objects = R"IDF(
Refrigeration:CompressorRack,
  SelfContainedDisplay,    !- Name
  Zone,                    !- Heat Rejection Location
  4.0,                     !- Design Compressor Rack COP {W/W}
  RackCOPfTCurve2,         !- Compressor Rack COP Function of Temperature Curve Name
  175.0,                   !- Design Condenser Fan Power {W}
  ,                        !- Condenser Fan Power Function of Temperature Curve Name
  AirCooled,               !- Condenser Type
  ,                        !- Water-Cooled Condenser Inlet Node Name
  ,                        !- Water-Cooled Condenser Outlet Node Name
  ,                        !- Water-Cooled Loop Flow Type
  ,                        !- Water-Cooled Condenser Outlet Temperature Schedule Name
  ,                        !- Water-Cooled Condenser Design Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Water Outlet Temperature {C}
  ,                        !- Water-Cooled Condenser Minimum Water Inlet Temperature {C}
  ,                        !- Evaporative Condenser Availability Schedule Name
  ,                        !- Evaporative Condenser Effectiveness {dimensionless}
  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}
  ,                        !- Basin Heater Capacity {W/K}
  ,                        !- Basin Heater Setpoint Temperature {C}
  ,                        !- Design Evaporative Condenser Water Pump Power {W}
  ,                        !- Evaporative Water Supply Tank Name
  ,                        !- Condenser Air Inlet Node Name
  ,                        !- End-Use Subcategory
  RefrigerationWalkIn,     !- Refrigeration Case Name or WalkIn Name or CaseAndWalkInList Name
  ZONE_1;                  !- Heat Rejection Zone Name


Curve:Quadratic,
  RackCOPfTCurve2,         !- Name
  1.0,                     !- Coefficient1 Constant
  0.0,                     !- Coefficient2 x
  0.0,                     !- Coefficient3 x**2
  0.0,                     !- Minimum Value of x
  50.0,                    !- Maximum Value of x
  ,                        !- Minimum Curve Output
  ,                        !- Maximum Curve Output
  Temperature,             !- Input Unit Type for X
  Dimensionless;           !- Output Unit Type

Refrigeration:WalkIn,
  RefrigerationWalkIn,     !- Name
  Always On,               !- Availability Schedule Name
  5,                       !- Rated Coil Cooling Capacity {W}
  1.66666666666667,        !- Operating Temperature {C}
  -6.11111111111111,       !- Rated Cooling Source Temperature {C}
  0.0,                     !- Rated Total Heating Power {W}
  Always On,               !- Heating Power Schedule Name
  5,                       !- Rated Cooling Coil Fan Power {W}
  0.0,                     !- Rated Circulation Fan Power {W}
  5,                       !- Rated Total Lighting Power {W}
  Always On,               !- Lighting Schedule Name
  Electric,                !- Defrost Type
  TimeSchedule,            !- Defrost Control Type
  Always Off,              !- Defrost Schedule Name
  ,                        !- Defrost Drip-Down Schedule Name
  0.001,                   !- Defrost Power {W}
  ,                        !- Temperature Termination Defrost Fraction to Ice {dimensionless}
  ,                        !- Restocking Schedule Name
  ,                        !- Average Refrigerant Charge Inventory {kg}
  1,                       !- Insulated Floor Surface Area {m2}
  0.17744571875,           !- Insulated Floor U-Value {W/m2-K}
  ZONE_1,                  !- Zone 1 Name
  6.504,                   !- Total Insulated Surface Area Facing Zone 1 {m2}
  0.17744571875,           !- Insulated Surface U-Value Facing Zone 1 {W/m2-K}
  ,                        !- Area of Glass Reach In Doors Facing Zone 1 {m2}
  ,                        !- Height of Glass Reach In Doors Facing Zone 1 {m}
  ,                        !- Glass Reach In Door U Value Facing Zone 1 {W/m2-K}
  ,                        !- Glass Reach In Door Opening Schedule Name Facing Zone 1
  1.216,                   !- Area of Stocking Doors Facing Zone 1 {m2}
  1.651,                   !- Height of Stocking Doors Facing Zone 1 {m}
  ,                        !- Stocking Door U Value Facing Zone 1 {W/m2-K}
  ,                        !- Stocking Door Opening Schedule Name Facing Zone 1
  None;                    !- Stocking Door Opening Protection Type Facing Zone 1
)IDF";

    ASSERT_TRUE(process_idf(fmt::format("{}\n{}", oneZoneBuildingWithIdealLoads, idf_objects))); // read idf objects

    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataEnvrn->OutBaroPress = 101325.0;
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ScheduleManager::ProcessScheduleInput(*state);
    // ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    InternalHeatGains::ManageInternalHeatGains(*state, true);
    RefrigeratedCase::ManageRefrigeratedCaseRacks(*state);
}

TEST_F(EnergyPlusFixture, RefrigeratedRackWithWalkInInZone_CaseAndWalkinList)
{

    std::string_view constexpr idf_objects = R"IDF(
Refrigeration:CompressorRack,
  SelfContainedDisplay,    !- Name
  Zone,                    !- Heat Rejection Location
  4.0,                     !- Design Compressor Rack COP {W/W}
  RackCOPfTCurve2,         !- Compressor Rack COP Function of Temperature Curve Name
  175.0,                   !- Design Condenser Fan Power {W}
  ,                        !- Condenser Fan Power Function of Temperature Curve Name
  AirCooled,               !- Condenser Type
  ,                        !- Water-Cooled Condenser Inlet Node Name
  ,                        !- Water-Cooled Condenser Outlet Node Name
  ,                        !- Water-Cooled Loop Flow Type
  ,                        !- Water-Cooled Condenser Outlet Temperature Schedule Name
  ,                        !- Water-Cooled Condenser Design Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Water Outlet Temperature {C}
  ,                        !- Water-Cooled Condenser Minimum Water Inlet Temperature {C}
  ,                        !- Evaporative Condenser Availability Schedule Name
  ,                        !- Evaporative Condenser Effectiveness {dimensionless}
  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}
  ,                        !- Basin Heater Capacity {W/K}
  ,                        !- Basin Heater Setpoint Temperature {C}
  ,                        !- Design Evaporative Condenser Water Pump Power {W}
  ,                        !- Evaporative Water Supply Tank Name
  ,                        !- Condenser Air Inlet Node Name
  ,                        !- End-Use Subcategory
  CompressorRack with Case and Walkin List,     !- Refrigeration Case Name or WalkIn Name or CaseAndWalkInList Name
  ZONE_1;                  !- Heat Rejection Zone Name

Curve:Quadratic,
  RackCOPfTCurve2,         !- Name
  1.0,                     !- Coefficient1 Constant
  0.0,                     !- Coefficient2 x
  0.0,                     !- Coefficient3 x**2
  0.0,                     !- Minimum Value of x
  50.0,                    !- Maximum Value of x
  ,                        !- Minimum Curve Output
  ,                        !- Maximum Curve Output
  Temperature,             !- Input Unit Type for X
  Dimensionless;           !- Output Unit Type

Refrigeration:CaseAndWalkInList,
  CompressorRack with Case and Walkin List, !- Name
  RefrigerationWalkIn;                      !- Case or WalkIn Name 1

Refrigeration:WalkIn,
  RefrigerationWalkIn,     !- Name
  Always On,               !- Availability Schedule Name
  5,                       !- Rated Coil Cooling Capacity {W}
  1.66666666666667,        !- Operating Temperature {C}
  -6.11111111111111,       !- Rated Cooling Source Temperature {C}
  0.0,                     !- Rated Total Heating Power {W}
  Always On,               !- Heating Power Schedule Name
  5,                       !- Rated Cooling Coil Fan Power {W}
  0.0,                     !- Rated Circulation Fan Power {W}
  5,                       !- Rated Total Lighting Power {W}
  Always On,               !- Lighting Schedule Name
  Electric,                !- Defrost Type
  TimeSchedule,            !- Defrost Control Type
  Always Off,              !- Defrost Schedule Name
  ,                        !- Defrost Drip-Down Schedule Name
  0.001,                   !- Defrost Power {W}
  ,                        !- Temperature Termination Defrost Fraction to Ice {dimensionless}
  ,                        !- Restocking Schedule Name
  ,                        !- Average Refrigerant Charge Inventory {kg}
  1,                       !- Insulated Floor Surface Area {m2}
  0.17744571875,           !- Insulated Floor U-Value {W/m2-K}
  ZONE_1,                  !- Zone 1 Name
  6.504,                   !- Total Insulated Surface Area Facing Zone 1 {m2}
  0.17744571875,           !- Insulated Surface U-Value Facing Zone 1 {W/m2-K}
  ,                        !- Area of Glass Reach In Doors Facing Zone 1 {m2}
  ,                        !- Height of Glass Reach In Doors Facing Zone 1 {m}
  ,                        !- Glass Reach In Door U Value Facing Zone 1 {W/m2-K}
  ,                        !- Glass Reach In Door Opening Schedule Name Facing Zone 1
  1.216,                   !- Area of Stocking Doors Facing Zone 1 {m2}
  1.651,                   !- Height of Stocking Doors Facing Zone 1 {m}
  ,                        !- Stocking Door U Value Facing Zone 1 {W/m2-K}
  ,                        !- Stocking Door Opening Schedule Name Facing Zone 1
  None;                    !- Stocking Door Opening Protection Type Facing Zone 1
)IDF";

    ASSERT_TRUE(process_idf(fmt::format("{}\n{}", oneZoneBuildingWithIdealLoads, idf_objects))); // read idf objects

    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataEnvrn->OutBaroPress = 101325.0;
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ScheduleManager::ProcessScheduleInput(*state);
    // ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    InternalHeatGains::ManageInternalHeatGains(*state, true);
    RefrigeratedCase::ManageRefrigeratedCaseRacks(*state);
}

TEST_F(EnergyPlusFixture, RefrigeratedRackWithBothInZone_CaseAndWalkinList)
{

    std::string_view constexpr idf_objects = R"IDF(
Refrigeration:CompressorRack,
  SelfContainedDisplay,    !- Name
  Zone,                    !- Heat Rejection Location
  4.0,                     !- Design Compressor Rack COP {W/W}
  RackCOPfTCurve2,         !- Compressor Rack COP Function of Temperature Curve Name
  175.0,                   !- Design Condenser Fan Power {W}
  ,                        !- Condenser Fan Power Function of Temperature Curve Name
  AirCooled,               !- Condenser Type
  ,                        !- Water-Cooled Condenser Inlet Node Name
  ,                        !- Water-Cooled Condenser Outlet Node Name
  ,                        !- Water-Cooled Loop Flow Type
  ,                        !- Water-Cooled Condenser Outlet Temperature Schedule Name
  ,                        !- Water-Cooled Condenser Design Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Flow Rate {m3/s}
  ,                        !- Water-Cooled Condenser Maximum Water Outlet Temperature {C}
  ,                        !- Water-Cooled Condenser Minimum Water Inlet Temperature {C}
  ,                        !- Evaporative Condenser Availability Schedule Name
  ,                        !- Evaporative Condenser Effectiveness {dimensionless}
  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}
  ,                        !- Basin Heater Capacity {W/K}
  ,                        !- Basin Heater Setpoint Temperature {C}
  ,                        !- Design Evaporative Condenser Water Pump Power {W}
  ,                        !- Evaporative Water Supply Tank Name
  ,                        !- Condenser Air Inlet Node Name
  ,                        !- End-Use Subcategory
  CompressorRack with Case and Walkin List,     !- Refrigeration Case Name or WalkIn Name or CaseAndWalkInList Name
  ZONE_1;                  !- Heat Rejection Zone Name

Curve:Quadratic,
  RackCOPfTCurve2,         !- Name
  1.0,                     !- Coefficient1 Constant
  0.0,                     !- Coefficient2 x
  0.0,                     !- Coefficient3 x**2
  0.0,                     !- Minimum Value of x
  50.0,                    !- Maximum Value of x
  ,                        !- Minimum Curve Output
  ,                        !- Maximum Curve Output
  Temperature,             !- Input Unit Type for X
  Dimensionless;           !- Output Unit Type

Refrigeration:CaseAndWalkInList,
  CompressorRack with Case and Walkin List, !- Name
  RefrigerationWalkIn,                      !- Case or WalkIn Name 1
  SelfContainedDisplayCase;                 !- Case or WalkIn Name 2

Refrigeration:WalkIn,
  RefrigerationWalkIn,     !- Name
  Always On,               !- Availability Schedule Name
  5,                       !- Rated Coil Cooling Capacity {W}
  1.66666666666667,        !- Operating Temperature {C}
  -6.11111111111111,       !- Rated Cooling Source Temperature {C}
  0.0,                     !- Rated Total Heating Power {W}
  Always On,               !- Heating Power Schedule Name
  5,                       !- Rated Cooling Coil Fan Power {W}
  0.0,                     !- Rated Circulation Fan Power {W}
  5,                       !- Rated Total Lighting Power {W}
  Always On,               !- Lighting Schedule Name
  Electric,                !- Defrost Type
  TimeSchedule,            !- Defrost Control Type
  Always Off,              !- Defrost Schedule Name
  ,                        !- Defrost Drip-Down Schedule Name
  0.001,                   !- Defrost Power {W}
  ,                        !- Temperature Termination Defrost Fraction to Ice {dimensionless}
  ,                        !- Restocking Schedule Name
  ,                        !- Average Refrigerant Charge Inventory {kg}
  1,                       !- Insulated Floor Surface Area {m2}
  0.17744571875,           !- Insulated Floor U-Value {W/m2-K}
  ZONE_1,                  !- Zone 1 Name
  6.504,                   !- Total Insulated Surface Area Facing Zone 1 {m2}
  0.17744571875,           !- Insulated Surface U-Value Facing Zone 1 {W/m2-K}
  ,                        !- Area of Glass Reach In Doors Facing Zone 1 {m2}
  ,                        !- Height of Glass Reach In Doors Facing Zone 1 {m}
  ,                        !- Glass Reach In Door U Value Facing Zone 1 {W/m2-K}
  ,                        !- Glass Reach In Door Opening Schedule Name Facing Zone 1
  1.216,                   !- Area of Stocking Doors Facing Zone 1 {m2}
  1.651,                   !- Height of Stocking Doors Facing Zone 1 {m}
  ,                        !- Stocking Door U Value Facing Zone 1 {W/m2-K}
  ,                        !- Stocking Door Opening Schedule Name Facing Zone 1
  None;                    !- Stocking Door Opening Protection Type Facing Zone 1

Refrigeration:Case,
  SelfContainedDisplayCase,!- Name
  ,                        !- Availability Schedule Name
  ZONE_1,                  !- Zone Name
  23.88,                   !- Rated Ambient Temperature {C}
  55.0,                    !- Rated Ambient Relative Humidity {percent}
  1000.0,                  !- Rated Total Cooling Capacity per Unit Length {W/m}
  0.08,                    !- Rated Latent Heat Ratio
  0.85,                    !- Rated Runtime Fraction
  10.0,                    !- Case Length {m}
  13.0,                    !- Case Operating Temperature {C}
  CaseTemperatureMethod,   !- Latent Case Credit Curve Type
  MultiShelfVertical_LatentEnergyMult,  !- Latent Case Credit Curve Name
  40.0,                    !- Standard Case Fan Power per Unit Length {W/m}
  40.0,                    !- Operating Case Fan Power per Unit Length {W/m}
  75.0,                    !- Standard Case Lighting Power per Unit Length {W/m}
  ,                        !- Installed Case Lighting Power per Unit Length {W/m}
  Always On,               !- Case Lighting Schedule Name
  0.9,                     !- Fraction of Lighting Energy to Case
  0.0,                     !- Case Anti-Sweat Heater Power per Unit Length {W/m}
  ,                        !- Minimum Anti-Sweat Heater Power per Unit Length {W/m}
  None,                    !- Anti-Sweat Heater Control Type
  0.0,                     !- Humidity at Zero Anti-Sweat Heater Energy {percent}
  0.0,                     !- Case Height {m}
  0.0,                     !- Fraction of Anti-Sweat Heater Energy to Case
  0.0,                     !- Case Defrost Power per Unit Length {W/m}
  None,                    !- Case Defrost Type
  ,                        !- Case Defrost Schedule Name
  ,                        !- Case Defrost Drip-Down Schedule Name
  ,                        !- Defrost Energy Correction Curve Type
  ,                        !- Defrost Energy Correction Curve Name
  0.0,                     !- Under Case HVAC Return Air Fraction
  SelfContainedCaseStockingSched;  !- Refrigerated Case Restocking Schedule Name

Curve:Cubic,
  MultiShelfVertical_LatentEnergyMult,  !- Name
  0.026526281,             !- Coefficient1 Constant
  0.001078032,             !- Coefficient2 x
  -0.0000602558,           !- Coefficient3 x**2
  0.00000123732,           !- Coefficient4 x**3
  -35.0,                   !- Minimum Value of x
  20.0;                    !- Maximum Value of x

Schedule:Compact,
  SelfContainedCaseStockingSched,  !- Name
  AnyNumber,               !- Schedule Type Limits Name
  Through: 12/31,          !- Field 1
  For: AllDays,            !- Field 2
  Until: 6:00,0.0,         !- Field 3
  Until: 7:00,50.0,        !- Field 5
  Until: 9:00,70.0,        !- Field 7
  Until: 10:00,80.0,       !- Field 9
  Until: 11:00,70.0,       !- Field 11
  Until: 13:00,50.0,       !- Field 13
  Until: 14:00,80.0,       !- Field 15
  Until: 15:00,90.0,       !- Field 17
  Until: 16:00,80.0,       !- Field 19
  Until: 24:00,0.0;        !- Field 21
)IDF";

    ASSERT_TRUE(process_idf(fmt::format("{}\n{}", oneZoneBuildingWithIdealLoads, idf_objects))); // read idf objects

    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataEnvrn->OutBaroPress = 101325.0;
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ScheduleManager::ProcessScheduleInput(*state);
    // ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    InternalHeatGains::ManageInternalHeatGains(*state, true);
    RefrigeratedCase::ManageRefrigeratedCaseRacks(*state);
}
