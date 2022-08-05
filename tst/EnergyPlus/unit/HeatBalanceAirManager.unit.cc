// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>

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
TEST_F(EnergyPlusFixture, HeatBalanceAirManager_GetInfiltration)
{
    // Test input processing of Infiltration objects with spaces

    // GetZoneData uses GetObjectItem with ipshortcuts so these need to be allocated
    int NumAlphas = 4;
    int NumNumbers = 9;
    state->dataIPShortCut->lNumericFieldBlanks.allocate(NumNumbers);
    state->dataIPShortCut->lAlphaFieldBlanks.allocate(NumAlphas);
    state->dataIPShortCut->cAlphaFieldNames.allocate(NumAlphas);
    state->dataIPShortCut->cNumericFieldNames.allocate(NumNumbers);
    state->dataIPShortCut->cAlphaArgs.allocate(NumAlphas);
    state->dataIPShortCut->rNumericArgs.allocate(NumNumbers);
    state->dataIPShortCut->lNumericFieldBlanks = false;
    state->dataIPShortCut->lAlphaFieldBlanks = false;
    state->dataIPShortCut->cAlphaFieldNames = " ";
    state->dataIPShortCut->cNumericFieldNames = " ";
    state->dataIPShortCut->cAlphaArgs = " ";
    state->dataIPShortCut->rNumericArgs = 0.0;

    state->dataInputProcessing->inputProcessor->epJSON = R"(
    {
        "Zone": {
            "Zone 1" : {
            },
            "Zone 2" : {
                 "floor_area": 1000.0,
            }
        },
        "Space": {
            "Space 1a" : {
                 "zone_name": "Zone 1"
                 "floor_area": 10.0,
            },
            "Space 1b" : {
                 "zone_name": "Zone 1",
                 "floor_area": 100.0,
                 "space_type": "Office",
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
        }
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
        }
        "ZoneInfiltration:DesignFlowRate": {
            "FirstFloor_Plenum_Infiltration": {
                "constant_term_coefficient": 1.0,
                "design_flow_rate_calculation_method": "Flow/Area",
                "flow_per_floor_area": 1.0,
                "schedule_name": "INFIL_QUARTER_ON_SCH",
                "temperature_term_coefficient": 0.0,
                "velocity_squared_term_coefficient": 0.0,
                "velocity_term_coefficient": 0.0,
                "zone_or_zonelist_or_space_or_spacelist_name": "FirstFloor_Plenum"
            },
        }
    )"_json;

    state->dataGlobal->isEpJSON = true;
    state->dataInputProcessing->inputProcessor->initializeMaps();

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    compare_err_stream("");
    EXPECT_FALSE(ErrorsFound);

    int zoneNum = 1;
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(zoneNum).Name);
    EXPECT_EQ(2, state->dataHeatBal->Zone(zoneNum).numSpaces);
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[0]).Name);
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[1]).Name);
    zoneNum = 2;
    EXPECT_EQ("ZONE 2", state->dataHeatBal->Zone(zoneNum).Name);
    EXPECT_EQ(1, state->dataHeatBal->Zone(zoneNum).numSpaces);
    EXPECT_EQ("ZONE 2", state->dataHeatBal->space(state->dataHeatBal->Zone(zoneNum).spaceIndexes[0]).Name);

    int spaceNum = 1;
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("GENERAL", state->dataHeatBal->space(spaceNum).spaceType);
    EXPECT_EQ("GENERAL", state->dataHeatBal->spaceTypes(state->dataHeatBal->space(spaceNum).spaceTypeNum));
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    // Defaults
    EXPECT_EQ(-99999, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_TRUE(state->dataHeatBal->space(spaceNum).tags.empty());

    spaceNum = 2;
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("OFFICE", state->dataHeatBal->space(spaceNum).spaceType);
    EXPECT_EQ("OFFICE", state->dataHeatBal->spaceTypes(state->dataHeatBal->space(spaceNum).spaceTypeNum));
    EXPECT_EQ("ZONE 1", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    EXPECT_EQ(100, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_EQ("TAG1", state->dataHeatBal->space(spaceNum).tags(1));
    EXPECT_EQ("TAG2", state->dataHeatBal->space(spaceNum).tags(2));

    EXPECT_EQ("SOMESPACES", state->dataHeatBal->spaceList(1).Name);
    EXPECT_EQ(2, state->dataHeatBal->spaceList(1).spaces.size());
    EXPECT_EQ("SPACE 1A", state->dataHeatBal->space(state->dataHeatBal->spaceList(1).spaces[0]).Name);
    EXPECT_EQ("SPACE 1B", state->dataHeatBal->space(state->dataHeatBal->spaceList(1).spaces[1]).Name);

    // This space is auto-generated
    spaceNum = 3;
    EXPECT_EQ("ZONE 2", state->dataHeatBal->space(spaceNum).Name);
    EXPECT_EQ("GENERAL", state->dataHeatBal->space(spaceNum).spaceType);
    EXPECT_EQ("GENERAL", state->dataHeatBal->spaceTypes(state->dataHeatBal->space(spaceNum).spaceTypeNum));
    EXPECT_EQ("ZONE 2", state->dataHeatBal->Zone(state->dataHeatBal->space(spaceNum).zoneNum).Name);
    // Defaults
    EXPECT_EQ(-99999, state->dataHeatBal->space(spaceNum).userEnteredFloorArea);
    EXPECT_TRUE(state->dataHeatBal->space(spaceNum).tags.empty());
}

} // namespace EnergyPlus
