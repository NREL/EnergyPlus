// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::HeatBalanceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include <nlohmann/json_literals.hpp>

#include "Fixtures/EnergyPlusFixture.hh"

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatBalanceAirManager_RoomAirModelType_Test)
{
    // Issue User file with RoomAirSettings:AirflowNetwork is failing with fatal error #6086

    std::string const idf_objects = delimited_string({
        "  RoomAirModelType,",
        "  Skinny_Model,            !- Name",
        "  South Skin,              !- Zone Name",
        "  AirflowNetwork,          !- Room - Air Modeling Type",
        "  Direct;                  !- Air Temperature Coupling Strategy",

        "  RoomAirModelType,",
        "  Phat_Model,              !- Name",
        "  Thermal Zone,            !- Zone Name",
        "  AirflowNetwork,          !- Room - Air Modeling Type",
        "  Direct;                  !- Air Temperature Coupling Strategy",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfZones = 2;

    state->dataHeatBal->Zone.allocate(2);
    state->dataHeatBal->Zone(1).Name = "SOUTH SKIN";
    state->dataHeatBal->Zone(2).Name = "THERMAL ZONE";

    bool ErrorsFound(false);

    HeatBalanceAirManager::GetRoomAirModelParameters(*state, ErrorsFound);

    EXPECT_TRUE(ErrorsFound);

    std::string const error_string = delimited_string({
        "   ** Severe  ** In RoomAirModelType = SKINNY_MODEL: Room-Air Modeling Type = AIRFLOWNETWORK.",
        "   **   ~~~   ** This model requires AirflowNetwork:* objects to form a complete network, including AirflowNetwork:Intrazone:Node and "
        "AirflowNetwork:Intrazone:Linkage.",
        "   **   ~~~   ** AirflowNetwork:SimulationControl not found.",
        "   ** Severe  ** In RoomAirModelType = PHAT_MODEL: Room-Air Modeling Type = AIRFLOWNETWORK.",
        "   **   ~~~   ** This model requires AirflowNetwork:* objects to form a complete network, including AirflowNetwork:Intrazone:Node and "
        "AirflowNetwork:Intrazone:Linkage.",
        "   **   ~~~   ** AirflowNetwork:SimulationControl not found.",
        "   ** Severe  ** Errors found in processing input for RoomAirModelType",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));
}
TEST_F(EnergyPlusFixture, HeatBalanceAirManager_GetInfiltrationAndVentilation)
{
    // Test input processing of Infiltration objects with spaces

    state->dataInputProcessing->inputProcessor->epJSON = R"(
    {
        "Zone": {
            "Zone 1" : {
                "volume": 100.0
            },
            "Zone 2" : {
                "volume": 2000.0,
                 "floor_area": 1000.0
            }
        },
        "Space": {
            "Space 1a" : {
                 "zone_name": "Zone 1",
                 "floor_area": 10.0
            },
            "Space 1b" : {
                 "zone_name": "Zone 1",
                 "floor_area": 100.0
            }
        },
        "SpaceList": {
            "SomeSpaces" : {
                 "spaces": [
                    {
                        "space_name": "Space 1a"
                    },
                    {
                        "space_name": "Space 1b"
                    }
                ]
            }
        },
        "ZoneList": {
            "AllZones" : {
                 "zones": [
                    {
                        "zone_name": "Zone 1"
                    },
                    {
                        "zone_name": "Zone 2"
                    }
                ]
            }
        },
        "Building": {
            "Some building somewhere": {
            }
        },
        "GlobalGeometryRules": {
            "GlobalGeometryRules 1": {
                "coordinate_system": "Relative",
                "starting_vertex_position": "UpperLeftCorner",
                "vertex_entry_direction": "Counterclockwise"
            }
        },
        "Construction": {
            "ext-slab": {
                "outside_layer": "HW CONCRETE"
            }
        },
        "Material": {
            "HW CONCRETE": {
                "conductivity": 1.311,
                "density": 2240.0,
                "roughness": "Rough",
                "solar_absorptance": 0.7,
                "specific_heat": 836.8,
                "thermal_absorptance": 0.9,
                "thickness": 0.1016,
                "visible_absorptance": 0.7
            }
        },
        "BuildingSurface:Detailed": {
            "Dummy Space 1a Floor": {
                "zone_name": "Zone 1",
                "space_name": "Space 1a",
                "surface_type": "Floor",
                "construction_name": "ext-slab",
                "number_of_vertices": 4,
                "outside_boundary_condition": "adiabatic",
                "sun_exposure": "nosun",
                "vertices": [
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    }
                ]
            },
            "Dummy Space 1b Floor": {
                "zone_name": "Zone 1",
                "space_name": "Space 1b",
                "surface_type": "Floor",
                "construction_name": "ext-slab",
                "number_of_vertices": 4,
                "outside_boundary_condition": "adiabatic",
                "sun_exposure": "nosun",
                "vertices": [
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    }
                ]
            },
            "Dummy Zone 2 Floor": {
                "zone_name": "Zone 2",
                "surface_type": "Floor",
                "construction_name": "ext-slab",
                "number_of_vertices": 4,
                "outside_boundary_condition": "adiabatic",
                "sun_exposure": "nosun",
                "vertices": [
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    }
                ]
            }
        },
        "ZoneInfiltration:DesignFlowRate": {
            "Zone1Infiltration": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 1.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Zone 1"
            },
            "Zone2Infiltration": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 2.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Zone 2"
            },
            "Space1aInfiltration": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 3.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Space 1a"
            },
            "Space1bInfiltration": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 4.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Space 1b"
            },
            "SomeSpacesInfiltration": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 5.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "SomeSpaces"
            },
            "AllZonesInfiltration": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 6.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "AllZones"
            }
        },
        "ZoneVentilation:DesignFlowRate": {
            "Zone1Ventilation": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 1.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Zone 1"
            },
            "Zone2Ventilation": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 2.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Zone 2"
            },
            "Space1aVentilation": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 3.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Space 1a"
            },
            "Space1bVentilation": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 4.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "Space 1b"
            },
            "SomeSpacesVentilation": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 5.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "SomeSpaces"
            },
            "AllZonesVentilation": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 6.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "AllZones"
            }
        }
    }
    )"_json;

    state->dataGlobal->isEpJSON = true;
    state->dataInputProcessing->inputProcessor->initializeMaps();

    // GetZoneData and others use GetObjectItem with ipshortcuts so these need to be allocated
    // copied from InputProcessor::processInput (which expects an input file to be present)
    int MaxArgs = 0;
    int MaxAlpha = 0;
    int MaxNumeric = 0;
    state->dataInputProcessing->inputProcessor->getMaxSchemaArgs(MaxArgs, MaxAlpha, MaxNumeric);

    state->dataIPShortCut->cAlphaFieldNames.allocate(MaxAlpha);
    state->dataIPShortCut->cAlphaArgs.allocate(MaxAlpha);
    state->dataIPShortCut->lAlphaFieldBlanks.dimension(MaxAlpha, false);
    state->dataIPShortCut->cNumericFieldNames.allocate(MaxNumeric);
    state->dataIPShortCut->rNumericArgs.dimension(MaxNumeric, 0.0);
    state->dataIPShortCut->lNumericFieldBlanks.dimension(MaxNumeric, false);

    bool ErrorsFound = false;
    HeatBalanceManager::GetHeatBalanceInput(*state);
    std::string const error_string = delimited_string(
        {"   ** Warning ** GetSurfaceData: Entered Space Floor Area(s) differ more than 5% from calculated Space Floor Area(s).",
         "   **   ~~~   ** ...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual Spaces.",
         "   ** Warning ** CalculateZoneVolume: 1 zone is not fully enclosed. For more details use:  Output:Diagnostics,DisplayExtrawarnings; "});

    compare_err_stream(error_string, true);
    EXPECT_FALSE(ErrorsFound);

    state->dataHeatBalFanSys->ZoneReOrder.allocate(state->dataGlobal->NumOfZones);
    ErrorsFound = false;
    HeatBalanceAirManager::GetSimpleAirModelInputs(*state, ErrorsFound);
    compare_err_stream("", true);
    EXPECT_FALSE(ErrorsFound);

    // Expected floor areas
    Real64 constexpr Space1aFloorArea = 10.0;
    Real64 constexpr Space1bFloorArea = 100.0;
    Real64 constexpr Zone2FloorArea = 1000.0;

    // Zone and space nums
    int const zone1 = UtilityRoutines::FindItemInList("ZONE 1", state->dataHeatBal->Zone);
    int const zone2 = UtilityRoutines::FindItemInList("ZONE 2", state->dataHeatBal->Zone);
    int const space1a = UtilityRoutines::FindItemInList("SPACE 1A", state->dataHeatBal->space);
    int const space1b = UtilityRoutines::FindItemInList("SPACE 1B", state->dataHeatBal->space);
    int const spaceZone2 = UtilityRoutines::FindItemInList("ZONE 2", state->dataHeatBal->space);

    int zoneNum = zone1;
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(zoneNum).Name);
    EXPECT_EQ(2, state->dataHeatBal->Zone(zoneNum).numSpaces);
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[0]).Name);
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[1]).Name);
    zoneNum = zone2;
    EXPECT_EQ("ZONE 2", state->dataHeatBal->Zone(zoneNum).Name);
    EXPECT_EQ(1, state->dataHeatBal->Zone(zoneNum).numSpaces);
    EXPECT_EQ("ZONE 2", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[0]).Name);

    int spaceNum = space1a;
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    EXPECT_EQ(Space1aFloorArea, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_EQ(Space1aFloorArea, state->dataHeatBal->space(spaceNum).floorArea);

    spaceNum = space1b;
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    EXPECT_EQ(Space1bFloorArea, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_EQ(Space1bFloorArea, state->dataHeatBal->space(spaceNum).floorArea);

    EXPECT_EQ("SOMESPACES", state->dataHeatBal->spaceList(1).Name);
    EXPECT_EQ(2, state->dataHeatBal->spaceList(1).spaces.size());
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(state->dataHeatBal->spaceList(1).spaces[0]).Name);
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(state->dataHeatBal->spaceList(1).spaces[1]).Name);

    // This space is auto-generated
    spaceNum = spaceZone2;
    EXPECT_EQ("ZONE 2", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("ZONE 2", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    // Defaults
    EXPECT_EQ(-99999, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_EQ(Zone2FloorArea, state->dataHeatBal->space(spaceNum).floorArea);

    // Now get to the point and check the infiltration and ventilation setup
    // Expected number of infiltration and ventilation instances:
    // AllZonesInfiltration - 3, 3 spaces total in the zones in the zonelist, flow/area =
    // SomeSpacesInfiltration - 2, 2 spaces in the spacelist
    // Space1aInfiltration - 1
    // Space1bInfiltration - 1
    // Zone1Infiltration - 2, 2 spaces in the zone
    // Zone2Infiltration - 1, 1 space in the zone
    constexpr int numInstances = 10;

    constexpr Real64 AllZonesFlowPerArea = 6.0;
    constexpr Real64 SomeSpacesFlowPerArea = 5.0;
    constexpr Real64 Space1aFlowPerArea = 3.0;
    constexpr Real64 Space1bFlowPerArea = 4.0;
    constexpr Real64 Zone1FlowPerArea = 1.0;
    constexpr Real64 Zone2FlowPerArea = 2.0;

    // Note that the epJSON input will sort the objects alphabetically, so AllZoneInfiltration comes first
    const std::array<int, numInstances> spaceNums = {space1a, space1b, spaceZone2, space1a, space1b, space1a, space1b, space1a, space1b, spaceZone2};

    const std::array<int, numInstances> zoneNums = {zone1, zone1, zone2, zone1, zone1, zone1, zone1, zone1, zone1, zone2};

    constexpr std::array<std::string_view, numInstances> infilNames = {"Space 1a AllZonesInfiltration",
                                                                       "Space 1b AllZonesInfiltration",
                                                                       "Zone 2 AllZonesInfiltration",
                                                                       "Space 1a SomeSpacesInfiltration",
                                                                       "Space 1b SomeSpacesInfiltration",
                                                                       "Space1aInfiltration",
                                                                       "Space1bInfiltration",
                                                                       "Space 1a Zone1Infiltration",
                                                                       "Space 1b Zone1Infiltration",
                                                                       "Zone2Infiltration"};

    constexpr std::array<std::string_view, numInstances> ventNames = {"Space 1a AllZonesVentilation",
                                                                      "Space 1b AllZonesVentilation",
                                                                      "Zone 2 AllZonesVentilation",
                                                                      "Space 1a SomeSpacesVentilation",
                                                                      "Space 1b SomeSpacesVentilation",
                                                                      "Space1aVentilation",
                                                                      "Space1bVentilation",
                                                                      "Space 1a Zone1Ventilation",
                                                                      "Space 1b Zone1Ventilation",
                                                                      "Zone2Ventilation"};

    // Same flow rates for both infiltration and ventilation
    constexpr std::array<Real64, numInstances> flows = {Space1aFloorArea * AllZonesFlowPerArea,
                                                        Space1bFloorArea * AllZonesFlowPerArea,
                                                        Zone2FloorArea * AllZonesFlowPerArea,
                                                        Space1aFloorArea * SomeSpacesFlowPerArea,
                                                        Space1bFloorArea * SomeSpacesFlowPerArea,
                                                        Space1aFloorArea * Space1aFlowPerArea,
                                                        Space1bFloorArea * Space1bFlowPerArea,
                                                        Space1aFloorArea * Zone1FlowPerArea,
                                                        Space1bFloorArea * Zone1FlowPerArea,
                                                        Zone2FloorArea * Zone2FlowPerArea};

    for (int itemNum = 0; itemNum <= numInstances - 1; ++itemNum) {
        auto &thisInfiltration = state->dataHeatBal->Infiltration[itemNum];
        auto &thisVentilation = state->dataHeatBal->Ventilation[itemNum];
        EXPECT_TRUE(UtilityRoutines::SameString(infilNames[itemNum], thisInfiltration.Name));
        EXPECT_EQ(thisInfiltration.DesignLevel, flows[itemNum]);
        EXPECT_TRUE(UtilityRoutines::SameString(ventNames[itemNum], thisVentilation.Name));
        EXPECT_EQ(thisVentilation.DesignLevel, flows[itemNum]);
        EXPECT_EQ(thisInfiltration.ZonePtr, zoneNums[itemNum]);
        EXPECT_EQ(thisVentilation.ZonePtr, zoneNums[itemNum]);
        EXPECT_EQ(thisInfiltration.spaceIndex, spaceNums[itemNum]);
        EXPECT_EQ(thisVentilation.spaceIndex, spaceNums[itemNum]);
    }
}
TEST_F(EnergyPlusFixture, HeatBalanceAirManager_GetMixingAndCrossMixing)
{

    // Test input processing of Infiltration objects with spaces

    state->dataInputProcessing->inputProcessor->epJSON = R"(
    {
        "Zone": {
            "Zone 1" : {
                "volume": 100.0
            },
            "Zone 2" : {
                "volume": 2000.0,
                 "floor_area": 1000.0
            }
        },
        "Space": {
            "Space 1a" : {
                 "zone_name": "Zone 1",
                 "floor_area": 10.0
            },
            "Space 1b" : {
                 "zone_name": "Zone 1",
                 "floor_area": 100.0
            }
        },
        "SpaceList": {
            "SomeSpaces" : {
                 "spaces": [
                    {
                        "space_name": "Space 1a"
                    },
                    {
                        "space_name": "Space 1b"
                    }
                ]
            }
        },
        "ZoneList": {
            "AllZones" : {
                 "zones": [
                    {
                        "zone_name": "Zone 1"
                    },
                    {
                        "zone_name": "Zone 2"
                    }
                ]
            }
        },
        "Building": {
            "Some building somewhere": {
            }
        },
        "GlobalGeometryRules": {
            "GlobalGeometryRules 1": {
                "coordinate_system": "Relative",
                "starting_vertex_position": "UpperLeftCorner",
                "vertex_entry_direction": "Counterclockwise"
            }
        },
        "Construction": {
            "ext-slab": {
                "outside_layer": "HW CONCRETE"
            }
        },
        "Material": {
            "HW CONCRETE": {
                "conductivity": 1.311,
                "density": 2240.0,
                "roughness": "Rough",
                "solar_absorptance": 0.7,
                "specific_heat": 836.8,
                "thermal_absorptance": 0.9,
                "thickness": 0.1016,
                "visible_absorptance": 0.7
            }
        },
        "BuildingSurface:Detailed": {
            "Dummy Space 1a Floor": {
                "zone_name": "Zone 1",
                "space_name": "Space 1a",
                "surface_type": "Floor",
                "construction_name": "ext-slab",
                "number_of_vertices": 4,
                "outside_boundary_condition": "adiabatic",
                "sun_exposure": "nosun",
                "vertices": [
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    }
                ]
            },
            "Dummy Space 1b Floor": {
                "zone_name": "Zone 1",
                "space_name": "Space 1b",
                "surface_type": "Floor",
                "construction_name": "ext-slab",
                "number_of_vertices": 4,
                "outside_boundary_condition": "adiabatic",
                "sun_exposure": "nosun",
                "vertices": [
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    }
                ]
            },
            "Dummy Zone 2 Floor": {
                "zone_name": "Zone 2",
                "surface_type": "Floor",
                "construction_name": "ext-slab",
                "number_of_vertices": 4,
                "outside_boundary_condition": "adiabatic",
                "sun_exposure": "nosun",
                "vertices": [
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 45.3375,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 4.5732,
                        "vertex_z_coordinate": 0.0
                    },
                    {
                        "vertex_x_coordinate": 4.5732,
                        "vertex_y_coordinate": 28.7006,
                        "vertex_z_coordinate": 0.0
                    }
                ]
            }
        },
        "ZoneMixing": {
            "Zone1Mixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 1.0,
                "zone_or_space_name": "Zone 1",
                "source_zone_or_space_name": "Zone 2"
            },
            "Zone2Mixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 2.0,
                "zone_or_space_name": "Zone 2",
                "source_zone_or_space_name": "Zone 1"
            },
            "Space1aMixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 3.0,
                "zone_or_space_name": "Space 1a",
                "source_zone_or_space_name": "Space 1b"
            },
            "Space1bMixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 4.0,
                "zone_or_space_name": "Space 1b",
                "source_zone_or_space_name": "Zone 2"
            }
        },
        "ZoneCrossMixing": {
            "Zone1CrossMixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 1.0,
                "zone_or_space_name": "Zone 1",
                "source_zone_or_space_name": "Zone 2"
            },
            "Zone2CrossMixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 2.0,
                "zone_or_space_name": "Zone 2",
                "source_zone_or_space_name": "Zone 1"
            },
            "Space1aCrossMixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 3.0,
                "zone_or_space_name": "Space 1a",
                "source_zone_or_space_name": "Space 1b"
            },
            "Space1bCrossMixing": {
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_rate_per_floor_area": 4.0,
                "zone_or_space_name": "Space 1b",
                "source_zone_or_space_name": "Zone 2"
            }
        }
    }
    )"_json;

    state->dataGlobal->isEpJSON = true;
    state->dataInputProcessing->inputProcessor->initializeMaps();

    // GetZoneData and others use GetObjectItem with ipshortcuts so these need to be allocated
    // copied from InputProcessor::processInput (which expects an input file to be present)
    int MaxArgs = 0;
    int MaxAlpha = 0;
    int MaxNumeric = 0;
    state->dataInputProcessing->inputProcessor->getMaxSchemaArgs(MaxArgs, MaxAlpha, MaxNumeric);

    state->dataIPShortCut->cAlphaFieldNames.allocate(MaxAlpha);
    state->dataIPShortCut->cAlphaArgs.allocate(MaxAlpha);
    state->dataIPShortCut->lAlphaFieldBlanks.dimension(MaxAlpha, false);
    state->dataIPShortCut->cNumericFieldNames.allocate(MaxNumeric);
    state->dataIPShortCut->rNumericArgs.dimension(MaxNumeric, 0.0);
    state->dataIPShortCut->lNumericFieldBlanks.dimension(MaxNumeric, false);

    bool ErrorsFound = false;
    HeatBalanceManager::GetHeatBalanceInput(*state);
    std::string const error_string = delimited_string(
        {"   ** Warning ** GetSurfaceData: Entered Space Floor Area(s) differ more than 5% from calculated Space Floor Area(s).",
         "   **   ~~~   ** ...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual Spaces.",
         "   ** Warning ** CalculateZoneVolume: 1 zone is not fully enclosed. For more details use:  Output:Diagnostics,DisplayExtrawarnings; "});

    compare_err_stream(error_string, true);
    EXPECT_FALSE(ErrorsFound);

    state->dataHeatBalFanSys->ZoneReOrder.allocate(state->dataGlobal->NumOfZones);
    ErrorsFound = false;
    HeatBalanceAirManager::GetSimpleAirModelInputs(*state, ErrorsFound);
    compare_err_stream("", true);
    EXPECT_FALSE(ErrorsFound);

    // Expected floor areas
    Real64 constexpr Space1aFloorArea = 10.0;
    Real64 constexpr Space1bFloorArea = 100.0;
    Real64 constexpr Zone2FloorArea = 1000.0;

    // Zone  and space nums
    int const zone1 = UtilityRoutines::FindItemInList("ZONE 1", state->dataHeatBal->Zone);
    int const zone2 = UtilityRoutines::FindItemInList("ZONE 2", state->dataHeatBal->Zone);
    int const space1a = UtilityRoutines::FindItemInList("SPACE 1A", state->dataHeatBal->space);
    int const space1b = UtilityRoutines::FindItemInList("SPACE 1B", state->dataHeatBal->space);
    int const spaceZone2 = UtilityRoutines::FindItemInList("ZONE 2", state->dataHeatBal->space);

    int zoneNum = zone1;
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(zoneNum).Name);
    EXPECT_EQ(2, state->dataHeatBal->Zone(zoneNum).numSpaces);
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[0]).Name);
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[1]).Name);
    zoneNum = zone2;
    EXPECT_EQ("ZONE 2", state->dataHeatBal->Zone(zoneNum).Name);
    EXPECT_EQ(1, state->dataHeatBal->Zone(zoneNum).numSpaces);
    EXPECT_EQ("ZONE 2", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[0]).Name);

    int spaceNum = space1a;
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    EXPECT_EQ(Space1aFloorArea, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_EQ(Space1aFloorArea, state->dataHeatBal->space(spaceNum).floorArea);

    spaceNum = space1b;
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    EXPECT_EQ(Space1bFloorArea, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_EQ(Space1bFloorArea, state->dataHeatBal->space(spaceNum).floorArea);

    EXPECT_EQ("SOMESPACES", state->dataHeatBal->spaceList(1).Name);
    EXPECT_EQ(2, state->dataHeatBal->spaceList(1).spaces.size());
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(state->dataHeatBal->spaceList(1).spaces[0]).Name);
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(state->dataHeatBal->spaceList(1).spaces[1]).Name);

    // This space is auto-generated
    spaceNum = spaceZone2;
    EXPECT_EQ("ZONE 2", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("ZONE 2", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    // Defaults
    EXPECT_EQ(-99999, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_EQ(Zone2FloorArea, state->dataHeatBal->space(spaceNum).floorArea);

    // Now get to the point and check the mixing setup
    // Expected number of mixing and crossmixing instances:
    // Space1aMixing - 1
    // Space1bMixing - 1
    // Zone1Mixing - 2, 2 spaces in the zone
    // Zone2Mixing - 1, 1 space in the zone
    constexpr int numInstances = 5;

    constexpr Real64 Space1aFlowPerArea = 3.0;
    constexpr Real64 Space1bFlowPerArea = 4.0;
    constexpr Real64 Zone1FlowPerArea = 1.0;
    constexpr Real64 Zone2FlowPerArea = 2.0;

    // Note that the epJSON input will sort the objects alphabetically, so Space1aMixing comes first
    const std::array<int, numInstances> spaceNums = {space1a, space1b, space1a, space1b, spaceZone2};

    const std::array<int, numInstances> zoneNums = {zone1, zone1, zone1, zone1, zone2};

    const std::array<int, numInstances> fromSpaceNums = {space1b, spaceZone2, spaceZone2, spaceZone2, 0};

    const std::array<int, numInstances> fromZoneNums = {zone1, zone2, zone2, zone2, zone1};

    constexpr std::array<std::string_view, numInstances> mixNames = {
        "Space1aMixing", "Space1bMixing", "Space 1a Zone1Mixing", "Space 1b Zone1Mixing", "Zone2Mixing"};

    constexpr std::array<std::string_view, numInstances> crossMixNames = {
        "Space1aCrossMixing", "Space1bCrossMixing", "Space 1a Zone1CrossMixing", "Space 1b Zone1CrossMixing", "Zone2CrossMixing"};

    // Same flow rates for both infiltration and ventilation
    constexpr std::array<Real64, numInstances> flows = {Space1aFloorArea * Space1aFlowPerArea,
                                                        Space1bFloorArea * Space1bFlowPerArea,
                                                        Space1aFloorArea * Zone1FlowPerArea,
                                                        Space1bFloorArea * Zone1FlowPerArea,
                                                        Zone2FloorArea * Zone2FlowPerArea};

    for (int itemNum = 0; itemNum <= numInstances - 1; ++itemNum) {
        auto &thisMixing = state->dataHeatBal->Mixing[itemNum];
        auto &thisCrossMixing = state->dataHeatBal->CrossMixing[itemNum];
        EXPECT_TRUE(UtilityRoutines::SameString(mixNames[itemNum], thisMixing.Name));
        EXPECT_EQ(thisMixing.DesignLevel, flows[itemNum]);
        EXPECT_TRUE(UtilityRoutines::SameString(crossMixNames[itemNum], thisCrossMixing.Name));
        EXPECT_EQ(thisCrossMixing.DesignLevel, flows[itemNum]);
        EXPECT_EQ(thisMixing.ZonePtr, zoneNums[itemNum]);
        EXPECT_EQ(thisCrossMixing.ZonePtr, zoneNums[itemNum]);
        EXPECT_EQ(thisMixing.spaceIndex, spaceNums[itemNum]);
        EXPECT_EQ(thisCrossMixing.spaceIndex, spaceNums[itemNum]);
        EXPECT_EQ(thisMixing.fromSpaceIndex, fromSpaceNums[itemNum]);
        EXPECT_EQ(thisCrossMixing.fromSpaceIndex, fromSpaceNums[itemNum]);
        EXPECT_EQ(thisMixing.FromZone, fromZoneNums[itemNum]);
        EXPECT_EQ(thisCrossMixing.FromZone, fromZoneNums[itemNum]);
    }
}

} // namespace EnergyPlus
