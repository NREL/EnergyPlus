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

// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

#include <nlohmann/json_literals.hpp>

using namespace EnergyPlus;
using namespace EnergyPlus::DataZoneEquipment;

TEST_F(EnergyPlusFixture, DataZoneEquipment_TestGetSystemNodeNumberForZone)
{

    state->dataGlobal->NumOfZones = 2;
    state->dataZoneEquip->ZoneEquipConfig.allocate(state->dataGlobal->NumOfZones);

    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone1";
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).IsControlled = true;

    state->dataZoneEquip->ZoneEquipConfig(2).ZoneName = "Zone2";
    state->dataZoneEquip->ZoneEquipConfig(2).ZoneNode = 2;
    state->dataZoneEquip->ZoneEquipConfig(2).IsControlled = true;

    state->dataZoneEquip->ZoneEquipInputsFilled = true;

    EXPECT_EQ(0, GetSystemNodeNumberForZone(*state, 0));
    EXPECT_EQ(1, GetSystemNodeNumberForZone(*state, 1));

    state->dataZoneEquip->ZoneEquipConfig.deallocate();
}

TEST_F(EnergyPlusFixture, DataZoneEquipment_TestCalcDesignSpecificationOutdoorAir)
{
    // #6225

    state->dataHeatBal->Zone.allocate(1);
    state->dataSize->OARequirements.allocate(1);
    state->dataHeatBal->ZoneIntGain.allocate(1);
    state->dataHeatBal->People.allocate(1);
    state->dataScheduleMgr->Schedule.allocate(2);
    state->dataContaminantBalance->ZoneCO2GainFromPeople.allocate(1);
    state->dataContaminantBalance->ZoneAirCO2.allocate(1);
    state->dataContaminantBalance->ZoneSysContDemand.allocate(1);

    state->dataEnvrn->StdRhoAir = 1.20;

    state->dataHeatBal->Zone(1).FloorArea = 10.0;
    state->dataHeatBal->Zone(1).TotOccupants = 5.0;
    state->dataHeatBal->Zone(1).ZoneContamControllerSchedIndex = 1;
    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->TotPeople = 1;
    state->dataHeatBal->People(1).ActivityLevelPtr = 2;
    state->dataHeatBal->People(1).CO2RateFactor = 3.82e-8;
    state->dataHeatBal->People(1).NumberOfPeople = state->dataHeatBal->Zone(1).TotOccupants;

    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataContaminantBalance->OutdoorCO2 = 400.0;
    state->dataContaminantBalance->ZoneCO2GainFromPeople(1) = 3.82E-8 * 5.0;

    state->dataSize->NumOARequirements = 1;
    state->dataSize->OARequirements(1).Name = "ZONE OA";
    state->dataSize->OARequirements(1).OAFlowMethod = DataSizing::OAFlowCalcMethod::PCOccSch;
    state->dataSize->OARequirements(1).OAFlowPerPerson = 0.002;
    state->dataSize->OARequirements(1).OAFlowPerArea = 0.003;
    state->dataHeatBal->ZoneIntGain(1).NOFOCC = 0.5;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1.0;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 131.881995;

    Real64 OAVolumeFlowRate;
    // Test ZOAM_ProportionalControlSchOcc
    state->dataContaminantBalance->ZoneAirCO2(1) = 500.0;
    OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.031, OAVolumeFlowRate, 0.00001);

    state->dataContaminantBalance->ZoneAirCO2(1) = 405.0;
    OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.0308115, OAVolumeFlowRate, 0.00001);

    // Test ZOAM_ProportionalControlDesOcc
    state->dataContaminantBalance->ZoneAirCO2(1) = 500.0;
    state->dataSize->OARequirements(1).OAFlowMethod = DataSizing::OAFlowCalcMethod::PCDesOcc;
    OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.0315879, OAVolumeFlowRate, 0.00001);

    // Test ZOAM_IAQP
    state->dataSize->OARequirements(1).OAFlowMethod = DataSizing::OAFlowCalcMethod::IAQProcedure;
    state->dataContaminantBalance->ZoneSysContDemand(1).OutputRequiredToCO2SP = 0.2 * state->dataEnvrn->StdRhoAir;
    OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.2, OAVolumeFlowRate, 0.00001);

    // Cleanup
    state->dataHeatBal->Zone.deallocate();
    state->dataSize->OARequirements.deallocate();
    state->dataHeatBal->ZoneIntGain.deallocate();
    state->dataScheduleMgr->Schedule.deallocate();
    state->dataHeatBal->People.deallocate();
    state->dataContaminantBalance->ZoneCO2GainFromPeople.deallocate();
    state->dataContaminantBalance->ZoneAirCO2.deallocate();
    state->dataContaminantBalance->ZoneSysContDemand.deallocate();
}

TEST_F(EnergyPlusFixture, GetZoneEquipmentData_epJSON)
{

    state->dataInputProcessing->inputProcessor->epJSON = R"(
{
    "AirLoopHVAC": {
        "Packaged Rooftop Air Conditioner": {
            "availability_manager_list_name": "Packaged Rooftop Air ConditionerAvailability Manager List",
            "branch_list_name": "Packaged Rooftop Air Conditioner Supply Branches",
            "demand_side_inlet_node_names": "Packaged Rooftop Air Conditioner Demand Inlet Nodes",
            "demand_side_outlet_node_name": "Node 7",
            "design_return_air_flow_fraction_of_supply_air_flow": 1,
            "design_supply_air_flow_rate": "Autosize",
            "supply_side_inlet_node_name": "Node 4",
            "supply_side_outlet_node_names": "Packaged Rooftop Air Conditioner Supply Outlet Nodes"
        },
        "VAV with Reheat": {
            "availability_manager_list_name": "VAV with ReheatAvailability Manager List",
            "branch_list_name": "VAV with Reheat Supply Branches",
            "controller_list_name": "VAV with Reheat Controllers",
            "demand_side_inlet_node_names": "VAV with Reheat Demand Inlet Nodes",
            "demand_side_outlet_node_name": "Node 19",
            "design_return_air_flow_fraction_of_supply_air_flow": 1,
            "design_supply_air_flow_rate": "Autosize",
            "supply_side_inlet_node_name": "Node 16",
            "supply_side_outlet_node_names": "VAV with Reheat Supply Outlet Nodes"
        }
    },
    "AirLoopHVAC:ControllerList": {
        "Air Loop HVAC Outdoor Air System 1 Controller List": {
            "controller_1_name": "Controller Outdoor Air 1",
            "controller_1_object_type": "Controller:OutdoorAir"
        },
        "Air Loop HVAC Outdoor Air System 2 Controller List": {
            "controller_1_name": "Controller Outdoor Air 2",
            "controller_1_object_type": "Controller:OutdoorAir"
        },
        "VAV with Reheat Controllers": {
            "controller_1_name": "Controller Water Coil 2",
            "controller_1_object_type": "Controller:WaterCoil",
            "controller_2_name": "Controller Water Coil 1",
            "controller_2_object_type": "Controller:WaterCoil"
        }
    },
    "AirLoopHVAC:OutdoorAirSystem": {
        "Air Loop HVAC Outdoor Air System 1": {
            "controller_list_name": "Air Loop HVAC Outdoor Air System 1 Controller List",
            "outdoor_air_equipment_list_name": "Air Loop HVAC Outdoor Air System 1 Equipment List"
        },
        "Air Loop HVAC Outdoor Air System 2": {
            "controller_list_name": "Air Loop HVAC Outdoor Air System 2 Controller List",
            "outdoor_air_equipment_list_name": "Air Loop HVAC Outdoor Air System 2 Equipment List"
        }
    },
    "AirLoopHVAC:OutdoorAirSystem:EquipmentList": {
        "Air Loop HVAC Outdoor Air System 1 Equipment List": {
            "component_1_name": "Air Loop HVAC Outdoor Air System 1 Outdoor Air Mixer",
            "component_1_object_type": "OutdoorAir:Mixer"
        },
        "Air Loop HVAC Outdoor Air System 2 Equipment List": {
            "component_1_name": "Air Loop HVAC Outdoor Air System 2 Outdoor Air Mixer",
            "component_1_object_type": "OutdoorAir:Mixer"
        }
    },
    "AirLoopHVAC:ReturnPath": {
        "Packaged Rooftop Air Conditioner Return Path": {
            "components": [
                {
                    "component_name": "Air Loop HVAC Zone Mixer 1",
                    "component_object_type": "AirLoopHVAC:ZoneMixer"
                }
            ],
            "return_air_path_outlet_node_name": "Node 7"
        },
        "VAV with Reheat Return Path": {
            "components": [
                {
                    "component_name": "Air Loop HVAC Zone Mixer 2",
                    "component_object_type": "AirLoopHVAC:ZoneMixer"
                }
            ],
            "return_air_path_outlet_node_name": "Node 19"
        }
    },
    "AirLoopHVAC:SupplyPath": {
        "Packaged Rooftop Air Conditioner Node 6 Supply Path": {
            "components": [
                {
                    "component_name": "Air Loop HVAC Zone Splitter 1",
                    "component_object_type": "AirLoopHVAC:ZoneSplitter"
                }
            ],
            "supply_air_path_inlet_node_name": "Node 6"
        },
        "VAV with Reheat Node 18 Supply Path": {
            "components": [
                {
                    "component_name": "Air Loop HVAC Zone Splitter 2",
                    "component_object_type": "AirLoopHVAC:ZoneSplitter"
                }
            ],
            "supply_air_path_inlet_node_name": "Node 18"
        }
    },
    "AirLoopHVAC:ZoneMixer": {
        "Air Loop HVAC Zone Mixer 1": {
            "nodes": [
                {
                    "inlet_node_name": "Node 15"
                }
            ],
            "outlet_node_name": "Node 7"
        },
        "Air Loop HVAC Zone Mixer 2": {
            "nodes": [
                {
                    "inlet_node_name": "Node 77"
                }
            ],
            "outlet_node_name": "Node 19"
        }
    },
    "AirLoopHVAC:ZoneSplitter": {
        "Air Loop HVAC Zone Splitter 1": {
            "inlet_node_name": "Node 6",
            "nodes": [
                {
                    "outlet_node_name": "Node 14"
                }
            ]
        },
        "Air Loop HVAC Zone Splitter 2": {
            "inlet_node_name": "Node 18",
            "nodes": [
                {
                    "outlet_node_name": "Node 54"
                }
            ]
        }
    },
    "AirTerminal:SingleDuct:ConstantVolume:NoReheat": {
        "Air Terminal Single Duct Constant Volume No Reheat 1": {
            "air_inlet_node_name": "Node 14",
            "air_outlet_node_name": "Node 8",
            "availability_schedule_name": "Always On Discrete",
            "maximum_air_flow_rate": "Autosize"
        }
    },
    "AirTerminal:SingleDuct:VAV:Reheat": {
        "Air Terminal Single Duct VAV Reheat 1": {
            "air_inlet_node_name": "Node 54",
            "air_outlet_node_name": "Node 20",
            "availability_schedule_name": "Always On Discrete",
            "constant_minimum_air_flow_fraction": 0.3,
            "convergence_tolerance": 0.001,
            "damper_air_outlet_node_name": "Air Terminal Single Duct VAV Reheat 1 Damper Outlet",
            "damper_heating_action": "Normal",
            "fixed_minimum_air_flow_rate": 0,
            "maximum_air_flow_rate": "Autosize",
            "maximum_flow_fraction_during_reheat": "Autosize",
            "maximum_flow_per_zone_floor_area_during_reheat": "Autosize",
            "maximum_hot_water_or_steam_flow_rate": "Autosize",
            "maximum_reheat_air_temperature": 35,
            "minimum_hot_water_or_steam_flow_rate": 0,
            "reheat_coil_name": "Coil Heating Water 2",
            "reheat_coil_object_type": "Coil:Heating:Water",
            "zone_minimum_air_flow_input_method": "Constant"
        }
    },
    "AvailabilityManager:Scheduled": {
        "Air Loop HVAC Outdoor Air System 1 Availability Manager": {
            "schedule_name": "Always On Discrete"
        },
        "Air Loop HVAC Outdoor Air System 2 Availability Manager": {
            "schedule_name": "Always On Discrete"
        },
        "Packaged Rooftop Air Conditioner Availability Manager": {
            "schedule_name": "Always On Discrete"
        },
        "VAV with Reheat Availability Manager": {
            "schedule_name": "Always On Discrete"
        }
    },
    "AvailabilityManagerAssignmentList": {
        "Air Loop HVAC Outdoor Air System 1 Availability Manager List": {
            "managers": [
                {
                    "availability_manager_name": "Air Loop HVAC Outdoor Air System 1 Availability Manager",
                    "availability_manager_object_type": "AvailabilityManager:Scheduled"
                }
            ]
        },
        "Air Loop HVAC Outdoor Air System 2 Availability Manager List": {
            "managers": [
                {
                    "availability_manager_name": "Air Loop HVAC Outdoor Air System 2 Availability Manager",
                    "availability_manager_object_type": "AvailabilityManager:Scheduled"
                }
            ]
        },
        "Packaged Rooftop Air ConditionerAvailability Manager List": {
            "managers": [
                {
                    "availability_manager_name": "Packaged Rooftop Air Conditioner Availability Manager",
                    "availability_manager_object_type": "AvailabilityManager:Scheduled"
                }
            ]
        },
        "VAV with ReheatAvailability Manager List": {
            "managers": [
                {
                    "availability_manager_name": "VAV with Reheat Availability Manager",
                    "availability_manager_object_type": "AvailabilityManager:Scheduled"
                }
            ]
        }
    },
    "Boiler:HotWater": {
        "Boiler Hot Water 1": {
            "boiler_flow_mode": "ConstantFlow",
            "boiler_water_inlet_node_name": "Node 28",
            "boiler_water_outlet_node_name": "Node 33",
            "design_water_flow_rate": "Autosize",
            "efficiency_curve_temperature_evaluation_variable": "LeavingBoiler",
            "end_use_subcategory": "General",
            "fuel_type": "NaturalGas",
            "maximum_part_load_ratio": 1,
            "minimum_part_load_ratio": 0,
            "nominal_capacity": "Autosize",
            "nominal_thermal_efficiency": 0.8,
            "normalized_boiler_efficiency_curve_name": "Boiler Efficiency",
            "optimum_part_load_ratio": 1,
            "parasitic_electric_load": 0,
            "sizing_factor": 1,
            "water_outlet_upper_temperature_limit": 99
        }
    },
    "Branch": {
        "Chilled Water Loop Demand Branch 1": {
            "components": [
                {
                    "component_inlet_node_name": "Node 47",
                    "component_name": "Coil Cooling Water 1",
                    "component_object_type": "Coil:Cooling:Water",
                    "component_outlet_node_name": "Node 53"
                }
            ]
        },
        "Chilled Water Loop Demand Branch 2": {
            "components": [
                {
                    "component_inlet_node_name": "Node 55",
                    "component_name": "Pipe Adiabatic 8",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 56"
                }
            ]
        },
        "Chilled Water Loop Demand Bypass Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Chilled Water Loop Demand Bypass Pipe Inlet Node",
                    "component_name": "Chilled Water Loop Demand Bypass Pipe",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Chilled Water Loop Demand Bypass Pipe Outlet Node"
                }
            ]
        },
        "Chilled Water Loop Demand Inlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 45",
                    "component_name": "Pipe Adiabatic 9",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 59"
                }
            ]
        },
        "Chilled Water Loop Demand Outlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 60",
                    "component_name": "Pipe Adiabatic 10",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 46"
                }
            ]
        },
        "Chilled Water Loop Supply Branch 1": {
            "components": [
                {
                    "component_inlet_node_name": "Node 44",
                    "component_name": "Chiller Electric EIR 1",
                    "component_object_type": "Chiller:Electric:EIR",
                    "component_outlet_node_name": "Node 49"
                }
            ]
        },
        "Chilled Water Loop Supply Branch 2": {
            "components": [
                {
                    "component_inlet_node_name": "Node 50",
                    "component_name": "Pipe Adiabatic 6",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 51"
                }
            ]
        },
        "Chilled Water Loop Supply Inlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 42",
                    "component_name": "Pump Variable Speed 2",
                    "component_object_type": "Pump:VariableSpeed",
                    "component_outlet_node_name": "Node 48"
                }
            ]
        },
        "Chilled Water Loop Supply Outlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 52",
                    "component_name": "Pipe Adiabatic 7",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 43"
                }
            ]
        },
        "Condenser Water Loop Demand Branch 1": {
            "components": [
                {
                    "component_inlet_node_name": "Node 66",
                    "component_name": "Chiller Electric EIR 1",
                    "component_object_type": "Chiller:Electric:EIR",
                    "component_outlet_node_name": "Node 72"
                }
            ]
        },
        "Condenser Water Loop Demand Branch 2": {
            "components": [
                {
                    "component_inlet_node_name": "Node 73",
                    "component_name": "Pipe Adiabatic 13",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 74"
                }
            ]
        },
        "Condenser Water Loop Demand Bypass Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Condenser Water Loop Demand Bypass Pipe Inlet Node",
                    "component_name": "Condenser Water Loop Demand Bypass Pipe",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Condenser Water Loop Demand Bypass Pipe Outlet Node"
                }
            ]
        },
        "Condenser Water Loop Demand Inlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 64",
                    "component_name": "Pipe Adiabatic 14",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 75"
                }
            ]
        },
        "Condenser Water Loop Demand Outlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 76",
                    "component_name": "Pipe Adiabatic 15",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 65"
                }
            ]
        },
        "Condenser Water Loop Supply Branch 1": {
            "components": [
                {
                    "component_inlet_node_name": "Node 63",
                    "component_name": "Cooling Tower Single Speed 1",
                    "component_object_type": "CoolingTower:SingleSpeed",
                    "component_outlet_node_name": "Node 67"
                }
            ]
        },
        "Condenser Water Loop Supply Branch 2": {
            "components": [
                {
                    "component_inlet_node_name": "Node 68",
                    "component_name": "Pipe Adiabatic 11",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 69"
                }
            ]
        },
        "Condenser Water Loop Supply Inlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 61",
                    "component_name": "Pump Variable Speed 3",
                    "component_object_type": "Pump:VariableSpeed",
                    "component_outlet_node_name": "Node 71"
                }
            ]
        },
        "Condenser Water Loop Supply Outlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 70",
                    "component_name": "Pipe Adiabatic 12",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 62"
                }
            ]
        },
        "Hot Water Loop Demand Branch 1": {
            "components": [
                {
                    "component_inlet_node_name": "Node 31",
                    "component_name": "Pipe Adiabatic 2",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 36"
                }
            ]
        },
        "Hot Water Loop Demand Branch 2": {
            "components": [
                {
                    "component_inlet_node_name": "Node 37",
                    "component_name": "Coil Heating Water 1",
                    "component_object_type": "Coil:Heating:Water",
                    "component_outlet_node_name": "Node 38"
                }
            ]
        },
        "Hot Water Loop Demand Branch 3": {
            "components": [
                {
                    "component_inlet_node_name": "Node 57",
                    "component_name": "Coil Heating Water 2",
                    "component_object_type": "Coil:Heating:Water",
                    "component_outlet_node_name": "Node 58"
                }
            ]
        },
        "Hot Water Loop Demand Bypass Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Hot Water Loop Demand Bypass Pipe Inlet Node",
                    "component_name": "Hot Water Loop Demand Bypass Pipe",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Hot Water Loop Demand Bypass Pipe Outlet Node"
                }
            ]
        },
        "Hot Water Loop Demand Inlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 29",
                    "component_name": "Pipe Adiabatic 3",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 40"
                }
            ]
        },
        "Hot Water Loop Demand Outlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 39",
                    "component_name": "Pipe Adiabatic 4",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 30"
                }
            ]
        },
        "Hot Water Loop Supply Branch 1": {
            "components": [
                {
                    "component_inlet_node_name": "Node 28",
                    "component_name": "Boiler Hot Water 1",
                    "component_object_type": "Boiler:HotWater",
                    "component_outlet_node_name": "Node 33"
                }
            ]
        },
        "Hot Water Loop Supply Branch 2": {
            "components": [
                {
                    "component_inlet_node_name": "Node 34",
                    "component_name": "Pipe Adiabatic 1",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 35"
                }
            ]
        },
        "Hot Water Loop Supply Inlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 26",
                    "component_name": "Pump Variable Speed 1",
                    "component_object_type": "Pump:VariableSpeed",
                    "component_outlet_node_name": "Node 32"
                }
            ]
        },
        "Hot Water Loop Supply Outlet Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 41",
                    "component_name": "Pipe Adiabatic 5",
                    "component_object_type": "Pipe:Adiabatic",
                    "component_outlet_node_name": "Node 27"
                }
            ]
        },
        "Packaged Rooftop Air Conditioner Main Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 4",
                    "component_name": "Air Loop HVAC Outdoor Air System 1",
                    "component_object_type": "AirLoopHVAC:OutdoorAirSystem",
                    "component_outlet_node_name": "Node 11"
                },
                {
                    "component_inlet_node_name": "Node 11",
                    "component_name": "Coil Cooling DX Single Speed 1 CoilSystem",
                    "component_object_type": "CoilSystem:Cooling:DX",
                    "component_outlet_node_name": "Node 12"
                },
                {
                    "component_inlet_node_name": "Node 12",
                    "component_name": "Coil Heating Gas 1",
                    "component_object_type": "Coil:Heating:Fuel",
                    "component_outlet_node_name": "Node 13"
                },
                {
                    "component_inlet_node_name": "Node 13",
                    "component_name": "Fan Constant Volume 1",
                    "component_object_type": "Fan:ConstantVolume",
                    "component_outlet_node_name": "Node 5"
                }
            ]
        },
        "VAV with Reheat Main Branch": {
            "components": [
                {
                    "component_inlet_node_name": "Node 16",
                    "component_name": "Air Loop HVAC Outdoor Air System 2",
                    "component_object_type": "AirLoopHVAC:OutdoorAirSystem",
                    "component_outlet_node_name": "Node 23"
                },
                {
                    "component_inlet_node_name": "Node 23",
                    "component_name": "Coil Cooling Water 1",
                    "component_object_type": "Coil:Cooling:Water",
                    "component_outlet_node_name": "Node 24"
                },
                {
                    "component_inlet_node_name": "Node 24",
                    "component_name": "Coil Heating Water 1",
                    "component_object_type": "Coil:Heating:Water",
                    "component_outlet_node_name": "Node 25"
                },
                {
                    "component_inlet_node_name": "Node 25",
                    "component_name": "Fan Variable Volume 1",
                    "component_object_type": "Fan:VariableVolume",
                    "component_outlet_node_name": "Node 17"
                }
            ]
        }
    },
    "BranchList": {
        "Chilled Water Loop Demand Branches": {
            "branches": [
                {
                    "branch_name": "Chilled Water Loop Demand Inlet Branch"
                },
                {
                    "branch_name": "Chilled Water Loop Demand Branch 1"
                },
                {
                    "branch_name": "Chilled Water Loop Demand Branch 2"
                },
                {
                    "branch_name": "Chilled Water Loop Demand Bypass Branch"
                },
                {
                    "branch_name": "Chilled Water Loop Demand Outlet Branch"
                }
            ]
        },
        "Chilled Water Loop Supply Branches": {
            "branches": [
                {
                    "branch_name": "Chilled Water Loop Supply Inlet Branch"
                },
                {
                    "branch_name": "Chilled Water Loop Supply Branch 1"
                },
                {
                    "branch_name": "Chilled Water Loop Supply Branch 2"
                },
                {
                    "branch_name": "Chilled Water Loop Supply Outlet Branch"
                }
            ]
        },
        "Condenser Water Loop Demand Branches": {
            "branches": [
                {
                    "branch_name": "Condenser Water Loop Demand Inlet Branch"
                },
                {
                    "branch_name": "Condenser Water Loop Demand Branch 1"
                },
                {
                    "branch_name": "Condenser Water Loop Demand Branch 2"
                },
                {
                    "branch_name": "Condenser Water Loop Demand Bypass Branch"
                },
                {
                    "branch_name": "Condenser Water Loop Demand Outlet Branch"
                }
            ]
        },
        "Condenser Water Loop Supply Branches": {
            "branches": [
                {
                    "branch_name": "Condenser Water Loop Supply Inlet Branch"
                },
                {
                    "branch_name": "Condenser Water Loop Supply Branch 1"
                },
                {
                    "branch_name": "Condenser Water Loop Supply Branch 2"
                },
                {
                    "branch_name": "Condenser Water Loop Supply Outlet Branch"
                }
            ]
        },
        "Hot Water Loop Demand Branches": {
            "branches": [
                {
                    "branch_name": "Hot Water Loop Demand Inlet Branch"
                },
                {
                    "branch_name": "Hot Water Loop Demand Branch 1"
                },
                {
                    "branch_name": "Hot Water Loop Demand Branch 2"
                },
                {
                    "branch_name": "Hot Water Loop Demand Branch 3"
                },
                {
                    "branch_name": "Hot Water Loop Demand Bypass Branch"
                },
                {
                    "branch_name": "Hot Water Loop Demand Outlet Branch"
                }
            ]
        },
        "Hot Water Loop Supply Branches": {
            "branches": [
                {
                    "branch_name": "Hot Water Loop Supply Inlet Branch"
                },
                {
                    "branch_name": "Hot Water Loop Supply Branch 1"
                },
                {
                    "branch_name": "Hot Water Loop Supply Branch 2"
                },
                {
                    "branch_name": "Hot Water Loop Supply Outlet Branch"
                }
            ]
        },
        "Packaged Rooftop Air Conditioner Supply Branches": {
            "branches": [
                {
                    "branch_name": "Packaged Rooftop Air Conditioner Main Branch"
                }
            ]
        },
        "VAV with Reheat Supply Branches": {
            "branches": [
                {
                    "branch_name": "VAV with Reheat Main Branch"
                }
            ]
        }
    },
    "Building": {
        "Building 1": {
            "north_axis": 0
        }
    },
    "BuildingSurface:Detailed": {
        "1-SOUTH": {
            "construction_name": "R13 Construction",
            "outside_boundary_condition": "Outdoors",
            "sun_exposure": "SunExposed",
            "surface_type": "Wall",
            "vertices": [
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0.3
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0.3
                }
            ],
            "wind_exposure": "WindExposed",
            "zone_name": "Zone1"
        },
        "2-WEST": {
            "construction_name": "R13 Construction",
            "outside_boundary_condition": "Outdoors",
            "sun_exposure": "SunExposed",
            "surface_type": "Wall",
            "vertices": [
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0.3
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0.3
                }
            ],
            "wind_exposure": "WindExposed",
            "zone_name": "Zone1"
        },
        "3-EAST": {
            "construction_name": "R13 Construction",
            "outside_boundary_condition": "Outdoors",
            "sun_exposure": "SunExposed",
            "surface_type": "Wall",
            "vertices": [
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0.3
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0.3
                }
            ],
            "wind_exposure": "WindExposed",
            "zone_name": "Zone1"
        },
        "4-NORTH": {
            "construction_name": "R13 Construction",
            "outside_boundary_condition": "Outdoors",
            "sun_exposure": "SunExposed",
            "surface_type": "Wall",
            "vertices": [
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0.3
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0.3
                }
            ],
            "wind_exposure": "WindExposed",
            "zone_name": "Zone1"
        },
        "FLOOR": {
            "construction_name": "R13 Construction",
            "outside_boundary_condition": "Ground",
            "sun_exposure": "NoSun",
            "surface_type": "Floor",
            "vertices": [
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0
                }
            ],
            "wind_exposure": "NoWind",
            "zone_name": "Zone1"
        },
        "ROOF": {
            "construction_name": "R13 Construction",
            "outside_boundary_condition": "Outdoors",
            "sun_exposure": "SunExposed",
            "surface_type": "Roof",
            "vertices": [
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0.3
                },
                {
                    "vertex_x_coordinate": 30,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0.3
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 10,
                    "vertex_z_coordinate": 0.3
                },
                {
                    "vertex_x_coordinate": 0,
                    "vertex_y_coordinate": 0,
                    "vertex_z_coordinate": 0.3
                }
            ],
            "wind_exposure": "WindExposed",
            "zone_name": "Zone1"
        }
    },
    "Chiller:Electric:EIR": {
        "Chiller Electric EIR 1": {
            "basin_heater_capacity": 0,
            "basin_heater_setpoint_temperature": 10,
            "chilled_water_inlet_node_name": "Node 44",
            "chilled_water_outlet_node_name": "Node 49",
            "chiller_flow_mode": "NotModulated",
            "condenser_fan_power_ratio": 0,
            "condenser_heat_recovery_relative_capacity_fraction": 1,
            "condenser_inlet_node_name": "Node 66",
            "condenser_outlet_node_name": "Node 72",
            "condenser_type": "WaterCooled",
            "cooling_capacity_function_of_temperature_curve_name": "Curve Biquadratic 3",
            "electric_input_to_cooling_output_ratio_function_of_part_load_ratio_curve_name": "Curve Quadratic 4",
            "electric_input_to_cooling_output_ratio_function_of_temperature_curve_name": "Curve Biquadratic 4",
            "end_use_subcategory": "General",
            "fraction_of_compressor_electric_consumption_rejected_by_condenser": 1,
            "leaving_chilled_water_lower_temperature_limit": 2,
            "maximum_part_load_ratio": 1,
            "minimum_part_load_ratio": 0.1,
            "minimum_unloading_ratio": 0.2,
            "optimum_part_load_ratio": 1,
            "reference_capacity": "Autosize",
            "reference_chilled_water_flow_rate": "Autosize",
            "reference_condenser_fluid_flow_rate": "Autosize",
            "reference_cop": 5.5,
            "reference_entering_condenser_fluid_temperature": 29.4,
            "reference_leaving_chilled_water_temperature": 6.67,
            "sizing_factor": 1
        }
    },
    "Coil:Cooling:DX:SingleSpeed": {
        "Coil Cooling DX Single Speed 1": {
            "2017_rated_evaporator_fan_power_per_volume_flow_rate": 773.3,
            "2023_rated_evaporator_fan_power_per_volume_flow_rate": 934.4,
            "air_inlet_node_name": "Node 11",
            "air_outlet_node_name": "Node 12",
            "availability_schedule_name": "Always On Discrete",
            "basin_heater_capacity": 0,
            "basin_heater_setpoint_temperature": 2,
            "condenser_type": "AirCooled",
            "crankcase_heater_capacity": 0,
            "energy_input_ratio_function_of_flow_fraction_curve_name": "Curve Quadratic 2",
            "energy_input_ratio_function_of_temperature_curve_name": "Curve Biquadratic 2",
            "evaporative_condenser_air_flow_rate": "Autosize",
            "evaporative_condenser_effectiveness": 0.9,
            "evaporative_condenser_pump_rated_power_consumption": "Autosize",
            "gross_rated_cooling_cop": 3,
            "gross_rated_sensible_heat_ratio": "Autosize",
            "gross_rated_total_cooling_capacity": "Autosize",
            "latent_capacity_time_constant": 0,
            "maximum_cycling_rate": 0,
            "maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation": 10,
            "minimum_outdoor_dry_bulb_temperature_for_compressor_operation": -25,
            "nominal_time_for_condensate_removal_to_begin": 0,
            "part_load_fraction_correlation_curve_name": "Curve Quadratic 3",
            "rated_air_flow_rate": "Autosize",
            "ratio_of_initial_moisture_evaporation_rate_and_steady_state_latent_capacity": 0,
            "total_cooling_capacity_function_of_flow_fraction_curve_name": "Curve Quadratic 1",
            "total_cooling_capacity_function_of_temperature_curve_name": "Curve Biquadratic 1"
        }
    },
    "Coil:Cooling:Water": {
        "Coil Cooling Water 1": {
            "air_inlet_node_name": "Node 23",
            "air_outlet_node_name": "Node 24",
            "availability_schedule_name": "Always On Discrete",
            "design_air_flow_rate": "Autosize",
            "design_inlet_air_humidity_ratio": "Autosize",
            "design_inlet_air_temperature": "Autosize",
            "design_inlet_water_temperature": "Autosize",
            "design_outlet_air_humidity_ratio": "Autosize",
            "design_outlet_air_temperature": "Autosize",
            "design_water_flow_rate": "Autosize",
            "heat_exchanger_configuration": "CrossFlow",
            "type_of_analysis": "SimpleAnalysis",
            "water_inlet_node_name": "Node 47",
            "water_outlet_node_name": "Node 53"
        }
    },
    "Coil:Heating:Fuel": {
        "Coil Heating Gas 1": {
            "air_inlet_node_name": "Node 12",
            "air_outlet_node_name": "Node 13",
            "availability_schedule_name": "Always On Discrete",
            "burner_efficiency": 0.8,
            "fuel_type": "NaturalGas",
            "nominal_capacity": "Autosize",
            "parasitic_electric_load": 0,
            "parasitic_fuel_load": 0,
            "temperature_setpoint_node_name": "Node 13"
        }
    },
    "Coil:Heating:Water": {
        "Coil Heating Water 1": {
            "air_inlet_node_name": "Node 24",
            "air_outlet_node_name": "Node 25",
            "availability_schedule_name": "Always On Discrete",
            "maximum_water_flow_rate": "Autosize",
            "performance_input_method": "UFactorTimesAreaAndDesignWaterFlowRate",
            "rated_capacity": "Autosize",
            "rated_inlet_air_temperature": 16.6,
            "rated_inlet_water_temperature": 82.2,
            "rated_outlet_air_temperature": 32.2,
            "rated_outlet_water_temperature": 71.1,
            "rated_ratio_for_air_and_water_convection": 0.5,
            "u_factor_times_area_value": "Autosize",
            "water_inlet_node_name": "Node 37",
            "water_outlet_node_name": "Node 38"
        },
        "Coil Heating Water 2": {
            "air_inlet_node_name": "Air Terminal Single Duct VAV Reheat 1 Damper Outlet",
            "air_outlet_node_name": "Node 20",
            "availability_schedule_name": "Always On Discrete",
            "maximum_water_flow_rate": "Autosize",
            "performance_input_method": "UFactorTimesAreaAndDesignWaterFlowRate",
            "rated_capacity": "Autosize",
            "rated_inlet_air_temperature": 16.6,
            "rated_inlet_water_temperature": 82.2,
            "rated_outlet_air_temperature": 32.2,
            "rated_outlet_water_temperature": 71.1,
            "rated_ratio_for_air_and_water_convection": 0.5,
            "u_factor_times_area_value": "Autosize",
            "water_inlet_node_name": "Node 57",
            "water_outlet_node_name": "Node 58"
        }
    },
    "CoilSystem:Cooling:DX": {
        "Coil Cooling DX Single Speed 1 CoilSystem": {
            "availability_schedule_name": "Always On Discrete",
            "cooling_coil_name": "Coil Cooling DX Single Speed 1",
            "cooling_coil_object_type": "Coil:Cooling:DX:SingleSpeed",
            "dx_cooling_coil_system_inlet_node_name": "Node 11",
            "dx_cooling_coil_system_outlet_node_name": "Node 12",
            "dx_cooling_coil_system_sensor_node_name": "Node 12"
        }
    },
    "Connector:Mixer": {
        "Chilled Water Loop Demand Mixer": {
            "branches": [
                {
                    "inlet_branch_name": "Chilled Water Loop Demand Branch 1"
                },
                {
                    "inlet_branch_name": "Chilled Water Loop Demand Branch 2"
                },
                {
                    "inlet_branch_name": "Chilled Water Loop Demand Bypass Branch"
                }
            ],
            "outlet_branch_name": "Chilled Water Loop Demand Outlet Branch"
        },
        "Chilled Water Loop Supply Mixer": {
            "branches": [
                {
                    "inlet_branch_name": "Chilled Water Loop Supply Branch 1"
                },
                {
                    "inlet_branch_name": "Chilled Water Loop Supply Branch 2"
                }
            ],
            "outlet_branch_name": "Chilled Water Loop Supply Outlet Branch"
        },
        "Condenser Water Loop Demand Mixer": {
            "branches": [
                {
                    "inlet_branch_name": "Condenser Water Loop Demand Branch 1"
                },
                {
                    "inlet_branch_name": "Condenser Water Loop Demand Branch 2"
                },
                {
                    "inlet_branch_name": "Condenser Water Loop Demand Bypass Branch"
                }
            ],
            "outlet_branch_name": "Condenser Water Loop Demand Outlet Branch"
        },
        "Condenser Water Loop Supply Mixer": {
            "branches": [
                {
                    "inlet_branch_name": "Condenser Water Loop Supply Branch 1"
                },
                {
                    "inlet_branch_name": "Condenser Water Loop Supply Branch 2"
                }
            ],
            "outlet_branch_name": "Condenser Water Loop Supply Outlet Branch"
        },
        "Hot Water Loop Demand Mixer": {
            "branches": [
                {
                    "inlet_branch_name": "Hot Water Loop Demand Branch 1"
                },
                {
                    "inlet_branch_name": "Hot Water Loop Demand Branch 2"
                },
                {
                    "inlet_branch_name": "Hot Water Loop Demand Branch 3"
                },
                {
                    "inlet_branch_name": "Hot Water Loop Demand Bypass Branch"
                }
            ],
            "outlet_branch_name": "Hot Water Loop Demand Outlet Branch"
        },
        "Hot Water Loop Supply Mixer": {
            "branches": [
                {
                    "inlet_branch_name": "Hot Water Loop Supply Branch 1"
                },
                {
                    "inlet_branch_name": "Hot Water Loop Supply Branch 2"
                }
            ],
            "outlet_branch_name": "Hot Water Loop Supply Outlet Branch"
        }
    },
    "Connector:Splitter": {
        "Chilled Water Loop Demand Splitter": {
            "branches": [
                {
                    "outlet_branch_name": "Chilled Water Loop Demand Branch 1"
                },
                {
                    "outlet_branch_name": "Chilled Water Loop Demand Branch 2"
                },
                {
                    "outlet_branch_name": "Chilled Water Loop Demand Bypass Branch"
                }
            ],
            "inlet_branch_name": "Chilled Water Loop Demand Inlet Branch"
        },
        "Chilled Water Loop Supply Splitter": {
            "branches": [
                {
                    "outlet_branch_name": "Chilled Water Loop Supply Branch 1"
                },
                {
                    "outlet_branch_name": "Chilled Water Loop Supply Branch 2"
                }
            ],
            "inlet_branch_name": "Chilled Water Loop Supply Inlet Branch"
        },
        "Condenser Water Loop Demand Splitter": {
            "branches": [
                {
                    "outlet_branch_name": "Condenser Water Loop Demand Branch 1"
                },
                {
                    "outlet_branch_name": "Condenser Water Loop Demand Branch 2"
                },
                {
                    "outlet_branch_name": "Condenser Water Loop Demand Bypass Branch"
                }
            ],
            "inlet_branch_name": "Condenser Water Loop Demand Inlet Branch"
        },
        "Condenser Water Loop Supply Splitter": {
            "branches": [
                {
                    "outlet_branch_name": "Condenser Water Loop Supply Branch 1"
                },
                {
                    "outlet_branch_name": "Condenser Water Loop Supply Branch 2"
                }
            ],
            "inlet_branch_name": "Condenser Water Loop Supply Inlet Branch"
        },
        "Hot Water Loop Demand Splitter": {
            "branches": [
                {
                    "outlet_branch_name": "Hot Water Loop Demand Branch 1"
                },
                {
                    "outlet_branch_name": "Hot Water Loop Demand Branch 2"
                },
                {
                    "outlet_branch_name": "Hot Water Loop Demand Branch 3"
                },
                {
                    "outlet_branch_name": "Hot Water Loop Demand Bypass Branch"
                }
            ],
            "inlet_branch_name": "Hot Water Loop Demand Inlet Branch"
        },
        "Hot Water Loop Supply Splitter": {
            "branches": [
                {
                    "outlet_branch_name": "Hot Water Loop Supply Branch 1"
                },
                {
                    "outlet_branch_name": "Hot Water Loop Supply Branch 2"
                }
            ],
            "inlet_branch_name": "Hot Water Loop Supply Inlet Branch"
        }
    },
    "ConnectorList": {
        "Chilled Water Loop Demand Connector List": {
            "connector_1_name": "Chilled Water Loop Demand Splitter",
            "connector_1_object_type": "Connector:Splitter",
            "connector_2_name": "Chilled Water Loop Demand Mixer",
            "connector_2_object_type": "Connector:Mixer"
        },
        "Chilled Water Loop Supply Connector List": {
            "connector_1_name": "Chilled Water Loop Supply Splitter",
            "connector_1_object_type": "Connector:Splitter",
            "connector_2_name": "Chilled Water Loop Supply Mixer",
            "connector_2_object_type": "Connector:Mixer"
        },
        "Condenser Water Loop Demand Connector List": {
            "connector_1_name": "Condenser Water Loop Demand Splitter",
            "connector_1_object_type": "Connector:Splitter",
            "connector_2_name": "Condenser Water Loop Demand Mixer",
            "connector_2_object_type": "Connector:Mixer"
        },
        "Condenser Water Loop Supply Connector List": {
            "connector_1_name": "Condenser Water Loop Supply Splitter",
            "connector_1_object_type": "Connector:Splitter",
            "connector_2_name": "Condenser Water Loop Supply Mixer",
            "connector_2_object_type": "Connector:Mixer"
        },
        "Hot Water Loop Demand Connector List": {
            "connector_1_name": "Hot Water Loop Demand Splitter",
            "connector_1_object_type": "Connector:Splitter",
            "connector_2_name": "Hot Water Loop Demand Mixer",
            "connector_2_object_type": "Connector:Mixer"
        },
        "Hot Water Loop Supply Connector List": {
            "connector_1_name": "Hot Water Loop Supply Splitter",
            "connector_1_object_type": "Connector:Splitter",
            "connector_2_name": "Hot Water Loop Supply Mixer",
            "connector_2_object_type": "Connector:Mixer"
        }
    },
    "Construction": {
        "R13 Construction": {
            "outside_layer": "R13-IP"
        }
    },
    "Controller:MechanicalVentilation": {
        "Controller Mechanical Ventilation 1": {
            "availability_schedule_name": "Always On Discrete",
            "demand_controlled_ventilation": "No",
            "system_outdoor_air_method": "ZoneSum",
            "zone_specifications": [
                {
                    "zone_or_zonelist_name": "Zone1"
                }
            ]
        },
        "Controller Mechanical Ventilation 2": {
            "availability_schedule_name": "Always On Discrete",
            "demand_controlled_ventilation": "No",
            "system_outdoor_air_method": "ZoneSum",
            "zone_specifications": [
                {
                    "zone_or_zonelist_name": "Zone1"
                }
            ]
        }
    },
    "Controller:OutdoorAir": {
        "Controller Outdoor Air 1": {
            "actuator_node_name": "Node 9",
            "control_high_indoor_humidity_based_on_outdoor_humidity_ratio": "Yes",
            "economizer_control_action_type": "ModulateFlow",
            "economizer_control_type": "NoEconomizer",
            "economizer_maximum_limit_dry_bulb_temperature": 28,
            "economizer_maximum_limit_enthalpy": 64000,
            "economizer_minimum_limit_dry_bulb_temperature": -100,
            "heat_recovery_bypass_control_type": "BypassWhenWithinEconomizerLimits",
            "high_humidity_control": "No",
            "lockout_type": "NoLockout",
            "maximum_outdoor_air_flow_rate": "Autosize",
            "mechanical_ventilation_controller_name": "Controller Mechanical Ventilation 1",
            "minimum_limit_type": "FixedMinimum",
            "minimum_outdoor_air_flow_rate": 0,
            "mixed_air_node_name": "Node 11",
            "relief_air_outlet_node_name": "Node 10",
            "return_air_node_name": "Node 4"
        },
        "Controller Outdoor Air 2": {
            "actuator_node_name": "Node 21",
            "control_high_indoor_humidity_based_on_outdoor_humidity_ratio": "Yes",
            "economizer_control_action_type": "ModulateFlow",
            "economizer_control_type": "NoEconomizer",
            "economizer_maximum_limit_dry_bulb_temperature": 28,
            "economizer_maximum_limit_enthalpy": 64000,
            "economizer_minimum_limit_dry_bulb_temperature": -100,
            "heat_recovery_bypass_control_type": "BypassWhenWithinEconomizerLimits",
            "high_humidity_control": "No",
            "lockout_type": "NoLockout",
            "maximum_outdoor_air_flow_rate": "Autosize",
            "mechanical_ventilation_controller_name": "Controller Mechanical Ventilation 2",
            "minimum_limit_type": "FixedMinimum",
            "minimum_outdoor_air_flow_rate": 0,
            "mixed_air_node_name": "Node 23",
            "relief_air_outlet_node_name": "Node 22",
            "return_air_node_name": "Node 16"
        }
    },
    "Controller:WaterCoil": {
        "Controller Water Coil 1": {
            "action": "Normal",
            "actuator_node_name": "Node 37",
            "actuator_variable": "Flow",
            "control_variable": "Temperature",
            "controller_convergence_tolerance": "Autosize",
            "maximum_actuated_flow": "Autosize",
            "minimum_actuated_flow": 0,
            "sensor_node_name": "Node 25"
        },
        "Controller Water Coil 2": {
            "action": "Reverse",
            "actuator_node_name": "Node 47",
            "actuator_variable": "Flow",
            "control_variable": "Temperature",
            "controller_convergence_tolerance": "Autosize",
            "maximum_actuated_flow": "Autosize",
            "minimum_actuated_flow": 0,
            "sensor_node_name": "Node 24"
        }
    },
    "CoolingTower:SingleSpeed": {
        "Cooling Tower Single Speed 1": {
            "basin_heater_capacity": 0,
            "basin_heater_setpoint_temperature": 2,
            "blowdown_calculation_mode": "ConcentrationRatio",
            "blowdown_concentration_ratio": 3,
            "capacity_control": "FanCycling",
            "cell_control": "MinimalCell",
            "cell_maximum_water_flow_rate_fraction": 2.5,
            "cell_minimum_water_flow_rate_fraction": 0.33,
            "design_air_flow_rate": "Autosize",
            "design_approach_temperature": "Autosize",
            "design_fan_power": "Autosize",
            "design_inlet_air_dry_bulb_temperature": 35,
            "design_inlet_air_wet_bulb_temperature": 25.6,
            "design_range_temperature": "Autosize",
            "design_u_factor_times_area_value": "Autosize",
            "design_water_flow_rate": "Autosize",
            "drift_loss_percent": 0.008,
            "end_use_subcategory": "General",
            "evaporation_loss_factor": 0.2,
            "evaporation_loss_mode": "LossFactor",
            "free_convection_capacity": 0,
            "free_convection_nominal_capacity_sizing_factor": 0.1,
            "free_convection_regime_air_flow_rate": "Autocalculate",
            "free_convection_regime_air_flow_rate_sizing_factor": 0.1,
            "free_convection_regime_u_factor_times_area_value": "Autocalculate",
            "free_convection_u_factor_times_area_value_sizing_factor": 0.1,
            "heat_rejection_capacity_and_nominal_capacity_sizing_ratio": 1.25,
            "number_of_cells": 1,
            "performance_input_method": "UFactorTimesAreaAndDesignWaterFlowRate",
            "sizing_factor": 1,
            "water_inlet_node_name": "Node 63",
            "water_outlet_node_name": "Node 67"
        }
    },
    "Curve:Biquadratic": {
        "Boiler Efficiency": {
            "coefficient1_constant": 1,
            "coefficient2_x": 0,
            "coefficient3_x_2": 0,
            "coefficient4_y": 0,
            "coefficient5_y_2": 0,
            "coefficient6_x_y": 0,
            "input_unit_type_for_x": "Dimensionless",
            "input_unit_type_for_y": "Dimensionless",
            "maximum_value_of_x": 1,
            "maximum_value_of_y": 1,
            "minimum_value_of_x": 0,
            "minimum_value_of_y": 0,
            "output_unit_type": "Dimensionless"
        },
        "Curve Biquadratic 1": {
            "coefficient1_constant": 0.942587793,
            "coefficient2_x": 0.009543347,
            "coefficient3_x_2": 0.00068377,
            "coefficient4_y": -0.011042676,
            "coefficient5_y_2": 5.249e-06,
            "coefficient6_x_y": -9.72e-06,
            "maximum_value_of_x": 22,
            "maximum_value_of_y": 46,
            "minimum_value_of_x": 17,
            "minimum_value_of_y": 13
        },
        "Curve Biquadratic 2": {
            "coefficient1_constant": 0.342414409,
            "coefficient2_x": 0.034885008,
            "coefficient3_x_2": -0.0006237,
            "coefficient4_y": 0.004977216,
            "coefficient5_y_2": 0.000437951,
            "coefficient6_x_y": -0.000728028,
            "maximum_value_of_x": 22,
            "maximum_value_of_y": 46,
            "minimum_value_of_x": 17,
            "minimum_value_of_y": 13
        },
        "Curve Biquadratic 3": {
            "coefficient1_constant": 0.258,
            "coefficient2_x": 0.0389,
            "coefficient3_x_2": -0.000217,
            "coefficient4_y": 0.0469,
            "coefficient5_y_2": -0.000943,
            "coefficient6_x_y": -0.000343,
            "maximum_value_of_x": 10,
            "maximum_value_of_y": 35,
            "minimum_value_of_x": 5,
            "minimum_value_of_y": 24
        },
        "Curve Biquadratic 4": {
            "coefficient1_constant": 0.934,
            "coefficient2_x": -0.0582,
            "coefficient3_x_2": 0.0045,
            "coefficient4_y": 0.00243,
            "coefficient5_y_2": 0.000486,
            "coefficient6_x_y": -0.00122,
            "maximum_value_of_x": 10,
            "maximum_value_of_y": 35,
            "minimum_value_of_x": 5,
            "minimum_value_of_y": 24
        }
    },
    "Curve:Quadratic": {
        "Curve Quadratic 1": {
            "coefficient1_constant": 0.8,
            "coefficient2_x": 0.2,
            "coefficient3_x_2": 0,
            "maximum_value_of_x": 1.5,
            "minimum_value_of_x": 0.5
        },
        "Curve Quadratic 2": {
            "coefficient1_constant": 1.1552,
            "coefficient2_x": -0.1808,
            "coefficient3_x_2": 0.0256,
            "maximum_value_of_x": 1.5,
            "minimum_value_of_x": 0.5
        },
        "Curve Quadratic 3": {
            "coefficient1_constant": 0.85,
            "coefficient2_x": 0.15,
            "coefficient3_x_2": 0,
            "maximum_value_of_x": 1,
            "minimum_value_of_x": 0
        },
        "Curve Quadratic 4": {
            "coefficient1_constant": 0.222903,
            "coefficient2_x": 0.313387,
            "coefficient3_x_2": 0.46371,
            "maximum_value_of_x": 1,
            "minimum_value_of_x": 0
        }
    },
    "Fan:ConstantVolume": {
        "Fan Constant Volume 1": {
            "air_inlet_node_name": "Node 13",
            "air_outlet_node_name": "Node 5",
            "availability_schedule_name": "Always On Discrete",
            "fan_total_efficiency": 0.7,
            "maximum_flow_rate": "Autosize",
            "motor_efficiency": 0.9,
            "motor_in_airstream_fraction": 1,
            "pressure_rise": 500
        }
    },
    "Fan:VariableVolume": {
        "Fan Variable Volume 1": {
            "air_inlet_node_name": "Node 25",
            "air_outlet_node_name": "Node 17",
            "availability_schedule_name": "Always On Discrete",
            "end_use_subcategory": "General",
            "fan_power_coefficient_1": 0.040759894,
            "fan_power_coefficient_2": 0.08804497,
            "fan_power_coefficient_3": -0.07292612,
            "fan_power_coefficient_4": 0.943739823,
            "fan_power_coefficient_5": 0,
            "fan_power_minimum_air_flow_rate": 0,
            "fan_power_minimum_flow_fraction": 0,
            "fan_power_minimum_flow_rate_input_method": "FixedFlowRate",
            "fan_total_efficiency": 0.6045,
            "maximum_flow_rate": "Autosize",
            "motor_efficiency": 0.93,
            "motor_in_airstream_fraction": 1,
            "pressure_rise": 500
        }
    },
    "Fan:ZoneExhaust": {
        "Fan Zone Exhaust 1": {
            "air_inlet_node_name": "Node 2",
            "air_outlet_node_name": "Node 3",
            "end_use_subcategory": "General",
            "fan_total_efficiency": 0.6,
            "pressure_rise": 0,
            "system_availability_manager_coupling_mode": "Decoupled"
        }
    },
    "GlobalGeometryRules": {
        "GlobalGeometryRules 1": {
            "coordinate_system": "Relative",
            "daylighting_reference_point_coordinate_system": "Relative",
            "rectangular_surface_coordinate_system": "Relative",
            "starting_vertex_position": "UpperLeftCorner",
            "vertex_entry_direction": "Counterclockwise"
        }
    },
    "Material:NoMass": {
        "R13-IP": {
            "roughness": "Smooth",
            "solar_absorptance": 0.7,
            "thermal_absorptance": 0.9,
            "thermal_resistance": 2.28943238786998,
            "visible_absorptance": 0.7
        }
    },
    "NodeList": {
        "Packaged Rooftop Air Conditioner Demand Inlet Nodes": {
            "nodes": [
                {
                    "node_name": "Node 6"
                }
            ]
        },
        "Packaged Rooftop Air Conditioner Supply Outlet Nodes": {
            "nodes": [
                {
                    "node_name": "Node 5"
                }
            ]
        },
        "VAV with Reheat Demand Inlet Nodes": {
            "nodes": [
                {
                    "node_name": "Node 18"
                }
            ]
        },
        "VAV with Reheat Supply Outlet Nodes": {
            "nodes": [
                {
                    "node_name": "Node 17"
                }
            ]
        },
        "Zone1 Exhaust Node List": {
            "nodes": [
                {
                    "node_name": "Node 2"
                }
            ]
        },
        "Zone1 Inlet Node List": {
            "nodes": [
                {
                    "node_name": "Node 8"
                },
                {
                    "node_name": "Node 20"
                }
            ]
        },
        "Zone1 Return Node List": {
            "nodes": [
                {
                    "node_name": "Node 15"
                },
                {
                    "node_name": "Node 77"
                }
            ]
        }
    },
    "OutdoorAir:Mixer": {
        "Air Loop HVAC Outdoor Air System 1 Outdoor Air Mixer": {
            "mixed_air_node_name": "Node 11",
            "outdoor_air_stream_node_name": "Node 9",
            "relief_air_stream_node_name": "Node 10",
            "return_air_stream_node_name": "Node 4"
        },
        "Air Loop HVAC Outdoor Air System 2 Outdoor Air Mixer": {
            "mixed_air_node_name": "Node 23",
            "outdoor_air_stream_node_name": "Node 21",
            "relief_air_stream_node_name": "Node 22",
            "return_air_stream_node_name": "Node 16"
        }
    },
    "OutdoorAir:Node": {
        "Model Outdoor Air Node": {}
    },
    "OutdoorAir:NodeList": {
        "OutdoorAir:NodeList 1": {
            "nodes": [
                {
                    "node_or_nodelist_name": "Node 9"
                }
            ]
        },
        "OutdoorAir:NodeList 2": {
            "nodes": [
                {
                    "node_or_nodelist_name": "Node 21"
                }
            ]
        }
    },
    "Output:Table:SummaryReports": {
        "Output:Table:SummaryReports 1": {
            "reports": [
                {
                    "report_name": "AllSummary"
                }
            ]
        }
    },
    "OutputControl:Table:Style": {
        "OutputControl:Table:Style 1": {
            "column_separator": "HTML"
        }
    },
    "Pipe:Adiabatic": {
        "Chilled Water Loop Demand Bypass Pipe": {
            "inlet_node_name": "Chilled Water Loop Demand Bypass Pipe Inlet Node",
            "outlet_node_name": "Chilled Water Loop Demand Bypass Pipe Outlet Node"
        },
        "Condenser Water Loop Demand Bypass Pipe": {
            "inlet_node_name": "Condenser Water Loop Demand Bypass Pipe Inlet Node",
            "outlet_node_name": "Condenser Water Loop Demand Bypass Pipe Outlet Node"
        },
        "Hot Water Loop Demand Bypass Pipe": {
            "inlet_node_name": "Hot Water Loop Demand Bypass Pipe Inlet Node",
            "outlet_node_name": "Hot Water Loop Demand Bypass Pipe Outlet Node"
        },
        "Pipe Adiabatic 1": {
            "inlet_node_name": "Node 34",
            "outlet_node_name": "Node 35"
        },
        "Pipe Adiabatic 2": {
            "inlet_node_name": "Node 31",
            "outlet_node_name": "Node 36"
        },
        "Pipe Adiabatic 3": {
            "inlet_node_name": "Node 29",
            "outlet_node_name": "Node 40"
        },
        "Pipe Adiabatic 4": {
            "inlet_node_name": "Node 39",
            "outlet_node_name": "Node 30"
        },
        "Pipe Adiabatic 5": {
            "inlet_node_name": "Node 41",
            "outlet_node_name": "Node 27"
        },
        "Pipe Adiabatic 6": {
            "inlet_node_name": "Node 50",
            "outlet_node_name": "Node 51"
        },
        "Pipe Adiabatic 7": {
            "inlet_node_name": "Node 52",
            "outlet_node_name": "Node 43"
        },
        "Pipe Adiabatic 8": {
            "inlet_node_name": "Node 55",
            "outlet_node_name": "Node 56"
        },
        "Pipe Adiabatic 9": {
            "inlet_node_name": "Node 45",
            "outlet_node_name": "Node 59"
        },
        "Pipe Adiabatic 10": {
            "inlet_node_name": "Node 60",
            "outlet_node_name": "Node 46"
        },
        "Pipe Adiabatic 11": {
            "inlet_node_name": "Node 68",
            "outlet_node_name": "Node 69"
        },
        "Pipe Adiabatic 12": {
            "inlet_node_name": "Node 70",
            "outlet_node_name": "Node 62"
        },
        "Pipe Adiabatic 13": {
            "inlet_node_name": "Node 73",
            "outlet_node_name": "Node 74"
        },
        "Pipe Adiabatic 14": {
            "inlet_node_name": "Node 64",
            "outlet_node_name": "Node 75"
        },
        "Pipe Adiabatic 15": {
            "inlet_node_name": "Node 76",
            "outlet_node_name": "Node 65"
        }
    },
    "PlantEquipmentList": {
        "Chilled Water Loop Cooling Equipment List": {
            "equipment": [
                {
                    "equipment_name": "Chiller Electric EIR 1",
                    "equipment_object_type": "Chiller:Electric:EIR"
                }
            ]
        },
        "Condenser Water Loop Cooling Equipment List": {
            "equipment": [
                {
                    "equipment_name": "Cooling Tower Single Speed 1",
                    "equipment_object_type": "CoolingTower:SingleSpeed"
                }
            ]
        },
        "Hot Water Loop Heating Equipment List": {
            "equipment": [
                {
                    "equipment_name": "Boiler Hot Water 1",
                    "equipment_object_type": "Boiler:HotWater"
                }
            ]
        }
    },
    "PlantEquipmentOperation:CoolingLoad": {
        "Chilled Water Loop Cooling Operation Scheme": {
            "load_range_1_lower_limit": 0,
            "load_range_1_upper_limit": 1000000000,
            "range_1_equipment_list_name": "Chilled Water Loop Cooling Equipment List"
        },
        "Condenser Water Loop Cooling Operation Scheme": {
            "load_range_1_lower_limit": 0,
            "load_range_1_upper_limit": 1000000000,
            "range_1_equipment_list_name": "Condenser Water Loop Cooling Equipment List"
        }
    },
    "PlantEquipmentOperation:HeatingLoad": {
        "Hot Water Loop Heating Operation Scheme": {
            "load_range_1_lower_limit": 0,
            "load_range_1_upper_limit": 1000000000,
            "range_1_equipment_list_name": "Hot Water Loop Heating Equipment List"
        }
    },
    "PlantEquipmentOperationSchemes": {
        "Chilled Water Loop Operation Schemes": {
            "control_scheme_1_name": "Chilled Water Loop Cooling Operation Scheme",
            "control_scheme_1_object_type": "PlantEquipmentOperation:CoolingLoad",
            "control_scheme_1_schedule_name": "Always On Discrete"
        },
        "Condenser Water Loop Operation Schemes": {
            "control_scheme_1_name": "Condenser Water Loop Cooling Operation Scheme",
            "control_scheme_1_object_type": "PlantEquipmentOperation:CoolingLoad",
            "control_scheme_1_schedule_name": "Always On Discrete"
        },
        "Hot Water Loop Operation Schemes": {
            "control_scheme_1_name": "Hot Water Loop Heating Operation Scheme",
            "control_scheme_1_object_type": "PlantEquipmentOperation:HeatingLoad",
            "control_scheme_1_schedule_name": "Always On Discrete"
        }
    },
    "PlantLoop": {
        "Chilled Water Loop": {
            "common_pipe_simulation": "None",
            "demand_side_branch_list_name": "Chilled Water Loop Demand Branches",
            "demand_side_connector_list_name": "Chilled Water Loop Demand Connector List",
            "demand_side_inlet_node_name": "Node 45",
            "demand_side_outlet_node_name": "Node 46",
            "fluid_type": "Water",
            "load_distribution_scheme": "Optimal",
            "loop_temperature_setpoint_node_name": "Node 43",
            "maximum_loop_flow_rate": "Autosize",
            "maximum_loop_temperature": 100,
            "minimum_loop_flow_rate": 0,
            "minimum_loop_temperature": 0,
            "plant_equipment_operation_scheme_name": "Chilled Water Loop Operation Schemes",
            "plant_loop_demand_calculation_scheme": "SingleSetpoint",
            "plant_loop_volume": "Autocalculate",
            "plant_side_branch_list_name": "Chilled Water Loop Supply Branches",
            "plant_side_connector_list_name": "Chilled Water Loop Supply Connector List",
            "plant_side_inlet_node_name": "Node 42",
            "plant_side_outlet_node_name": "Node 43"
        },
        "Condenser Water Loop": {
            "common_pipe_simulation": "None",
            "demand_side_branch_list_name": "Condenser Water Loop Demand Branches",
            "demand_side_connector_list_name": "Condenser Water Loop Demand Connector List",
            "demand_side_inlet_node_name": "Node 64",
            "demand_side_outlet_node_name": "Node 65",
            "fluid_type": "Water",
            "load_distribution_scheme": "Optimal",
            "loop_temperature_setpoint_node_name": "Node 62",
            "maximum_loop_flow_rate": "Autosize",
            "maximum_loop_temperature": 100,
            "minimum_loop_flow_rate": 0,
            "minimum_loop_temperature": 0,
            "plant_equipment_operation_scheme_name": "Condenser Water Loop Operation Schemes",
            "plant_loop_demand_calculation_scheme": "SingleSetpoint",
            "plant_loop_volume": "Autocalculate",
            "plant_side_branch_list_name": "Condenser Water Loop Supply Branches",
            "plant_side_connector_list_name": "Condenser Water Loop Supply Connector List",
            "plant_side_inlet_node_name": "Node 61",
            "plant_side_outlet_node_name": "Node 62"
        },
        "Hot Water Loop": {
            "common_pipe_simulation": "None",
            "demand_side_branch_list_name": "Hot Water Loop Demand Branches",
            "demand_side_connector_list_name": "Hot Water Loop Demand Connector List",
            "demand_side_inlet_node_name": "Node 29",
            "demand_side_outlet_node_name": "Node 30",
            "fluid_type": "Water",
            "load_distribution_scheme": "Optimal",
            "loop_temperature_setpoint_node_name": "Node 27",
            "maximum_loop_flow_rate": "Autosize",
            "maximum_loop_temperature": 100,
            "minimum_loop_flow_rate": 0,
            "minimum_loop_temperature": 0,
            "plant_equipment_operation_scheme_name": "Hot Water Loop Operation Schemes",
            "plant_loop_demand_calculation_scheme": "SingleSetpoint",
            "plant_loop_volume": "Autocalculate",
            "plant_side_branch_list_name": "Hot Water Loop Supply Branches",
            "plant_side_connector_list_name": "Hot Water Loop Supply Connector List",
            "plant_side_inlet_node_name": "Node 26",
            "plant_side_outlet_node_name": "Node 27"
        }
    },
    "Pump:VariableSpeed": {
        "Pump Variable Speed 1": {
            "coefficient_1_of_the_part_load_performance_curve": 0,
            "coefficient_2_of_the_part_load_performance_curve": 1,
            "coefficient_3_of_the_part_load_performance_curve": 0,
            "coefficient_4_of_the_part_load_performance_curve": 0,
            "design_electric_power_per_unit_flow_rate": 348701.1,
            "design_maximum_flow_rate": "Autosize",
            "design_minimum_flow_rate": 0,
            "design_minimum_flow_rate_fraction": 0,
            "design_power_consumption": "Autosize",
            "design_power_sizing_method": "PowerPerFlowPerPressure",
            "design_pump_head": 179352,
            "design_shaft_power_per_unit_flow_rate_per_unit_head": 1.282051282,
            "end_use_subcategory": "General",
            "fraction_of_motor_inefficiencies_to_fluid_stream": 0,
            "inlet_node_name": "Node 26",
            "motor_efficiency": 0.9,
            "outlet_node_name": "Node 32",
            "pump_control_type": "Intermittent",
            "skin_loss_radiative_fraction": 0.5
        },
        "Pump Variable Speed 2": {
            "coefficient_1_of_the_part_load_performance_curve": 0,
            "coefficient_2_of_the_part_load_performance_curve": 1,
            "coefficient_3_of_the_part_load_performance_curve": 0,
            "coefficient_4_of_the_part_load_performance_curve": 0,
            "design_electric_power_per_unit_flow_rate": 348701.1,
            "design_maximum_flow_rate": "Autosize",
            "design_minimum_flow_rate": 0,
            "design_minimum_flow_rate_fraction": 0,
            "design_power_consumption": "Autosize",
            "design_power_sizing_method": "PowerPerFlowPerPressure",
            "design_pump_head": 179352,
            "design_shaft_power_per_unit_flow_rate_per_unit_head": 1.282051282,
            "end_use_subcategory": "General",
            "fraction_of_motor_inefficiencies_to_fluid_stream": 0,
            "inlet_node_name": "Node 42",
            "motor_efficiency": 0.9,
            "outlet_node_name": "Node 48",
            "pump_control_type": "Intermittent",
            "skin_loss_radiative_fraction": 0.5
        },
        "Pump Variable Speed 3": {
            "coefficient_1_of_the_part_load_performance_curve": 0,
            "coefficient_2_of_the_part_load_performance_curve": 1,
            "coefficient_3_of_the_part_load_performance_curve": 0,
            "coefficient_4_of_the_part_load_performance_curve": 0,
            "design_electric_power_per_unit_flow_rate": 348701.1,
            "design_maximum_flow_rate": "Autosize",
            "design_minimum_flow_rate": 0,
            "design_minimum_flow_rate_fraction": 0,
            "design_power_consumption": "Autosize",
            "design_power_sizing_method": "PowerPerFlowPerPressure",
            "design_pump_head": 179352,
            "design_shaft_power_per_unit_flow_rate_per_unit_head": 1.282051282,
            "end_use_subcategory": "General",
            "fraction_of_motor_inefficiencies_to_fluid_stream": 0,
            "inlet_node_name": "Node 61",
            "motor_efficiency": 0.9,
            "outlet_node_name": "Node 71",
            "pump_control_type": "Intermittent",
            "skin_loss_radiative_fraction": 0.5
        }
    },
    "RunPeriod": {
        "Run Period 1": {
            "apply_weekend_holiday_rule": "No",
            "begin_day_of_month": 1,
            "begin_month": 1,
            "begin_year": 2009,
            "day_of_week_for_start_day": "Thursday",
            "end_day_of_month": 31,
            "end_month": 12,
            "end_year": 2009,
            "use_weather_file_daylight_saving_period": "No",
            "use_weather_file_holidays_and_special_days": "No",
            "use_weather_file_rain_indicators": "Yes",
            "use_weather_file_snow_indicators": "Yes"
        }
    },
    "Schedule:Constant": {
        "Always Off Discrete": {
            "hourly_value": 0
        },
        "Always On Continuous": {
            "hourly_value": 1
        },
        "Always On Discrete": {
            "hourly_value": 1,
            "schedule_type_limits_name": "OnOff"
        }
    },
    "Schedule:Day:Interval": {
        "Chilled_Water_Temperature_Default": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 6.7
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Chilled_Water_Temperature_Summer_Design_Day": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 6.7
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Chilled_Water_Temperature_Winter_Design_Day": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 6.7
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Deck_Temperature_Default": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 12.8
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Deck_Temperature_Summer_Design_Day": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 12.8
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Deck_Temperature_Winter_Design_Day": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 12.8
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Hot_Water_Temperature_Default": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 67
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Hot_Water_Temperature_Summer_Design_Day": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 67
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        },
        "Hot_Water_Temperature_Winter_Design_Day": {
            "data": [
                {
                    "time": "24:00",
                    "value_until_time": 67
                }
            ],
            "interpolate_to_timestep": "No",
            "schedule_type_limits_name": "Temperature"
        }
    },
    "Schedule:Week:Daily": {
        "Chilled_Water_Temperature Week Rule - Jan1-Dec31": {
            "customday1_schedule_day_name": "Chilled_Water_Temperature_Default",
            "customday2_schedule_day_name": "Chilled_Water_Temperature_Default",
            "friday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "holiday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "monday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "saturday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "summerdesignday_schedule_day_name": "Chilled_Water_Temperature_Summer_Design_Day",
            "sunday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "thursday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "tuesday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "wednesday_schedule_day_name": "Chilled_Water_Temperature_Default",
            "winterdesignday_schedule_day_name": "Chilled_Water_Temperature_Winter_Design_Day"
        },
        "Deck_Temperature Week Rule - Jan1-Dec31": {
            "customday1_schedule_day_name": "Deck_Temperature_Default",
            "customday2_schedule_day_name": "Deck_Temperature_Default",
            "friday_schedule_day_name": "Deck_Temperature_Default",
            "holiday_schedule_day_name": "Deck_Temperature_Default",
            "monday_schedule_day_name": "Deck_Temperature_Default",
            "saturday_schedule_day_name": "Deck_Temperature_Default",
            "summerdesignday_schedule_day_name": "Deck_Temperature_Summer_Design_Day",
            "sunday_schedule_day_name": "Deck_Temperature_Default",
            "thursday_schedule_day_name": "Deck_Temperature_Default",
            "tuesday_schedule_day_name": "Deck_Temperature_Default",
            "wednesday_schedule_day_name": "Deck_Temperature_Default",
            "winterdesignday_schedule_day_name": "Deck_Temperature_Winter_Design_Day"
        },
        "Hot_Water_Temperature Week Rule - Jan1-Dec31": {
            "customday1_schedule_day_name": "Hot_Water_Temperature_Default",
            "customday2_schedule_day_name": "Hot_Water_Temperature_Default",
            "friday_schedule_day_name": "Hot_Water_Temperature_Default",
            "holiday_schedule_day_name": "Hot_Water_Temperature_Default",
            "monday_schedule_day_name": "Hot_Water_Temperature_Default",
            "saturday_schedule_day_name": "Hot_Water_Temperature_Default",
            "summerdesignday_schedule_day_name": "Hot_Water_Temperature_Summer_Design_Day",
            "sunday_schedule_day_name": "Hot_Water_Temperature_Default",
            "thursday_schedule_day_name": "Hot_Water_Temperature_Default",
            "tuesday_schedule_day_name": "Hot_Water_Temperature_Default",
            "wednesday_schedule_day_name": "Hot_Water_Temperature_Default",
            "winterdesignday_schedule_day_name": "Hot_Water_Temperature_Winter_Design_Day"
        }
    },
    "Schedule:Year": {
        "Chilled_Water_Temperature": {
            "schedule_type_limits_name": "Temperature",
            "schedule_weeks": [
                {
                    "end_day": 31,
                    "end_month": 12,
                    "schedule_week_name": "Chilled_Water_Temperature Week Rule - Jan1-Dec31",
                    "start_day": 1,
                    "start_month": 1
                }
            ]
        },
        "Deck_Temperature": {
            "schedule_type_limits_name": "Temperature",
            "schedule_weeks": [
                {
                    "end_day": 31,
                    "end_month": 12,
                    "schedule_week_name": "Deck_Temperature Week Rule - Jan1-Dec31",
                    "start_day": 1,
                    "start_month": 1
                }
            ]
        },
        "Hot_Water_Temperature": {
            "schedule_type_limits_name": "Temperature",
            "schedule_weeks": [
                {
                    "end_day": 31,
                    "end_month": 12,
                    "schedule_week_name": "Hot_Water_Temperature Week Rule - Jan1-Dec31",
                    "start_day": 1,
                    "start_month": 1
                }
            ]
        }
    },
    "ScheduleTypeLimits": {
        "OnOff": {
            "lower_limit_value": 0,
            "numeric_type": "Discrete",
            "unit_type": "Availability",
            "upper_limit_value": 1
        },
        "Temperature": {
            "numeric_type": "Continuous",
            "unit_type": "Temperature"
        }
    },
    "SetpointManager:FollowOutdoorAirTemperature": {
        "Setpoint Manager Follow Outdoor Air Temperature 1": {
            "control_variable": "Temperature",
            "maximum_setpoint_temperature": 80,
            "minimum_setpoint_temperature": 6,
            "offset_temperature_difference": 1.5,
            "reference_temperature_type": "OutdoorAirWetBulb",
            "setpoint_node_or_nodelist_name": "Node 62"
        }
    },
    "SetpointManager:MixedAir": {
        "Node 11 OS Default SPM": {
            "control_variable": "Temperature",
            "fan_inlet_node_name": "Node 13",
            "fan_outlet_node_name": "Node 5",
            "reference_setpoint_node_name": "Node 5",
            "setpoint_node_or_nodelist_name": "Node 11"
        },
        "Node 12 OS Default SPM": {
            "control_variable": "Temperature",
            "fan_inlet_node_name": "Node 13",
            "fan_outlet_node_name": "Node 5",
            "reference_setpoint_node_name": "Node 5",
            "setpoint_node_or_nodelist_name": "Node 12"
        },
        "Node 13 OS Default SPM": {
            "control_variable": "Temperature",
            "fan_inlet_node_name": "Node 13",
            "fan_outlet_node_name": "Node 5",
            "reference_setpoint_node_name": "Node 5",
            "setpoint_node_or_nodelist_name": "Node 13"
        },
        "Node 23 OS Default SPM": {
            "control_variable": "Temperature",
            "fan_inlet_node_name": "Node 25",
            "fan_outlet_node_name": "Node 17",
            "reference_setpoint_node_name": "Node 17",
            "setpoint_node_or_nodelist_name": "Node 23"
        },
        "Node 24 OS Default SPM": {
            "control_variable": "Temperature",
            "fan_inlet_node_name": "Node 25",
            "fan_outlet_node_name": "Node 17",
            "reference_setpoint_node_name": "Node 17",
            "setpoint_node_or_nodelist_name": "Node 24"
        },
        "Node 25 OS Default SPM": {
            "control_variable": "Temperature",
            "fan_inlet_node_name": "Node 25",
            "fan_outlet_node_name": "Node 17",
            "reference_setpoint_node_name": "Node 17",
            "setpoint_node_or_nodelist_name": "Node 25"
        }
    },
    "SetpointManager:Scheduled": {
        "Setpoint Manager Scheduled 1": {
            "control_variable": "Temperature",
            "schedule_name": "Deck_Temperature",
            "setpoint_node_or_nodelist_name": "Node 17"
        },
        "Setpoint Manager Scheduled 2": {
            "control_variable": "Temperature",
            "schedule_name": "Hot_Water_Temperature",
            "setpoint_node_or_nodelist_name": "Node 27"
        },
        "Setpoint Manager Scheduled 3": {
            "control_variable": "Temperature",
            "schedule_name": "Chilled_Water_Temperature",
            "setpoint_node_or_nodelist_name": "Node 43"
        }
    },
    "SetpointManager:SingleZone:Reheat": {
        "Setpoint Manager Single Zone Reheat 1": {
            "control_variable": "Temperature",
            "control_zone_name": "Zone1",
            "maximum_supply_air_temperature": 99,
            "minimum_supply_air_temperature": -99,
            "setpoint_node_or_nodelist_name": "Node 5",
            "zone_inlet_node_name": "Node 20",
            "zone_node_name": "Node 1"
        }
    },
    "SimulationControl": {
        "SimulationControl 1": {
            "do_plant_sizing_calculation": "Yes",
            "do_system_sizing_calculation": "Yes",
            "do_zone_sizing_calculation": "Yes",
            "run_simulation_for_sizing_periods": "Yes",
            "run_simulation_for_weather_file_run_periods": "No"
        }
    },
    "Sizing:Parameters": {
        "Sizing:Parameters 1": {
            "cooling_sizing_factor": 1.15,
            "heating_sizing_factor": 1.25
        }
    },
    "Sizing:Plant": {
        "Sizing:Plant 1": {
            "coincident_sizing_factor_mode": "None",
            "design_loop_exit_temperature": 7.22,
            "loop_design_temperature_difference": 6.67,
            "loop_type": "Cooling",
            "plant_or_condenser_loop_name": "Chilled Water Loop",
            "sizing_option": "NonCoincident",
            "zone_timesteps_in_averaging_window": 1
        },
        "Sizing:Plant 2": {
            "coincident_sizing_factor_mode": "None",
            "design_loop_exit_temperature": 29.4,
            "loop_design_temperature_difference": 5.6,
            "loop_type": "Condenser",
            "plant_or_condenser_loop_name": "Condenser Water Loop",
            "sizing_option": "NonCoincident",
            "zone_timesteps_in_averaging_window": 1
        },
        "Sizing:Plant 3": {
            "coincident_sizing_factor_mode": "None",
            "design_loop_exit_temperature": 82,
            "loop_design_temperature_difference": 11,
            "loop_type": "Heating",
            "plant_or_condenser_loop_name": "Hot Water Loop",
            "sizing_option": "NonCoincident",
            "zone_timesteps_in_averaging_window": 1
        }
    },
    "Sizing:System": {
        "Sizing:System 1": {
            "100_outdoor_air_in_cooling": "Yes",
            "100_outdoor_air_in_heating": "Yes",
            "airloop_name": "Packaged Rooftop Air Conditioner",
            "central_cooling_capacity_control_method": "OnOff",
            "central_cooling_design_supply_air_humidity_ratio": 0.0085,
            "central_cooling_design_supply_air_temperature": 12.8,
            "central_heating_design_supply_air_humidity_ratio": 0.008,
            "central_heating_design_supply_air_temperature": 40,
            "central_heating_maximum_system_air_flow_ratio": 1,
            "cooling_design_capacity": "Autosize",
            "cooling_design_capacity_method": "CoolingDesignCapacity",
            "cooling_design_capacity_per_floor_area": 234.7,
            "cooling_fraction_of_autosized_cooling_supply_air_flow_rate": 1,
            "cooling_supply_air_flow_rate": 0,
            "cooling_supply_air_flow_rate_method": "DesignDay",
            "cooling_supply_air_flow_rate_per_floor_area": 0.0099676501,
            "cooling_supply_air_flow_rate_per_unit_cooling_capacity": 3.9475456e-05,
            "design_outdoor_air_flow_rate": "Autosize",
            "fraction_of_autosized_cooling_design_capacity": 1,
            "fraction_of_autosized_heating_design_capacity": 1,
            "heating_design_capacity": "Autosize",
            "heating_design_capacity_method": "HeatingDesignCapacity",
            "heating_design_capacity_per_floor_area": 157,
            "heating_fraction_of_autosized_cooling_supply_air_flow_rate": 1,
            "heating_fraction_of_autosized_heating_supply_air_flow_rate": 1,
            "heating_supply_air_flow_rate": 0,
            "heating_supply_air_flow_rate_method": "DesignDay",
            "heating_supply_air_flow_rate_per_floor_area": 0.0099676501,
            "heating_supply_air_flow_rate_per_unit_heating_capacity": 3.1588213e-05,
            "occupant_diversity": "Autosize",
            "precool_design_humidity_ratio": 0.008,
            "precool_design_temperature": 12.8,
            "preheat_design_humidity_ratio": 0.008,
            "preheat_design_temperature": 7,
            "system_outdoor_air_method": "ZoneSum",
            "type_of_load_to_size_on": "Sensible",
            "type_of_zone_sum_to_use": "NonCoincident",
            "zone_maximum_outdoor_air_fraction": 1
        },
        "Sizing:System 2": {
            "100_outdoor_air_in_cooling": "Yes",
            "100_outdoor_air_in_heating": "Yes",
            "airloop_name": "VAV with Reheat",
            "central_cooling_capacity_control_method": "OnOff",
            "central_cooling_design_supply_air_humidity_ratio": 0.0085,
            "central_cooling_design_supply_air_temperature": 12.8,
            "central_heating_design_supply_air_humidity_ratio": 0.008,
            "central_heating_design_supply_air_temperature": 12.8,
            "central_heating_maximum_system_air_flow_ratio": 0.3,
            "cooling_design_capacity": "Autosize",
            "cooling_design_capacity_method": "CoolingDesignCapacity",
            "cooling_design_capacity_per_floor_area": 234.7,
            "cooling_fraction_of_autosized_cooling_supply_air_flow_rate": 1,
            "cooling_supply_air_flow_rate": 0,
            "cooling_supply_air_flow_rate_method": "DesignDay",
            "cooling_supply_air_flow_rate_per_floor_area": 0.0099676501,
            "cooling_supply_air_flow_rate_per_unit_cooling_capacity": 3.9475456e-05,
            "design_outdoor_air_flow_rate": "Autosize",
            "fraction_of_autosized_cooling_design_capacity": 1,
            "fraction_of_autosized_heating_design_capacity": 1,
            "heating_design_capacity": "Autosize",
            "heating_design_capacity_method": "HeatingDesignCapacity",
            "heating_design_capacity_per_floor_area": 157,
            "heating_fraction_of_autosized_cooling_supply_air_flow_rate": 1,
            "heating_fraction_of_autosized_heating_supply_air_flow_rate": 1,
            "heating_supply_air_flow_rate": 0,
            "heating_supply_air_flow_rate_method": "DesignDay",
            "heating_supply_air_flow_rate_per_floor_area": 0.0099676501,
            "heating_supply_air_flow_rate_per_unit_heating_capacity": 3.1588213e-05,
            "occupant_diversity": "Autosize",
            "precool_design_humidity_ratio": 0.008,
            "precool_design_temperature": 12.8,
            "preheat_design_humidity_ratio": 0.008,
            "preheat_design_temperature": 7,
            "system_outdoor_air_method": "ZoneSum",
            "type_of_load_to_size_on": "Sensible",
            "type_of_zone_sum_to_use": "NonCoincident",
            "zone_maximum_outdoor_air_fraction": 1
        }
    },
    "Sizing:Zone": {
        "Sizing:Zone 1": {
            "account_for_dedicated_outdoor_air_system": "No",
            "cooling_design_air_flow_method": "DesignDay",
            "cooling_design_air_flow_rate": 0,
            "cooling_minimum_air_flow": 0,
            "cooling_minimum_air_flow_fraction": 0,
            "cooling_minimum_air_flow_per_zone_floor_area": 0.000762,
            "heating_design_air_flow_method": "DesignDay",
            "heating_design_air_flow_rate": 0,
            "heating_maximum_air_flow": 0.1415762,
            "heating_maximum_air_flow_fraction": 0.3,
            "heating_maximum_air_flow_per_zone_floor_area": 0.002032,
            "zone_cooling_design_supply_air_humidity_ratio": 0.0085,
            "zone_cooling_design_supply_air_humidity_ratio_difference": 0.005,
            "zone_cooling_design_supply_air_temperature": 14,
            "zone_cooling_design_supply_air_temperature_difference": 11.11,
            "zone_cooling_design_supply_air_temperature_input_method": "SupplyAirTemperature",
            "zone_heating_design_supply_air_humidity_ratio": 0.008,
            "zone_heating_design_supply_air_temperature": 40,
            "zone_heating_design_supply_air_temperature_difference": 11.11,
            "zone_heating_design_supply_air_temperature_input_method": "SupplyAirTemperature",
            "zone_humidification_design_supply_air_humidity_ratio_difference": 0.005,
            "zone_latent_cooling_design_supply_air_humidity_ratio_input_method": "HumidityRatioDifference",
            "zone_latent_heating_design_supply_air_humidity_ratio_input_method": "HumidityRatioDifference",
            "zone_load_sizing_method": "Sensible Load Only No Latent Load",
            "zone_or_zonelist_name": "Zone1"
        }
    },
    "SizingPeriod:DesignDay": {
        "Chicago Ohare Intl Ap Ann Clg .4% Condns DB=>MWB": {
            "ashrae_clear_sky_optical_depth_for_beam_irradiance_taub_": 0.455,
            "ashrae_clear_sky_optical_depth_for_diffuse_irradiance_taud_": 2.05,
            "barometric_pressure": 98934,
            "begin_environment_reset_mode": "FullResetAtBeginEnvironment",
            "daily_dry_bulb_temperature_range": 10.5,
            "day_of_month": 21,
            "day_type": "SummerDesignDay",
            "daylight_saving_time_indicator": "No",
            "dry_bulb_temperature_range_modifier_type": "DefaultMultipliers",
            "humidity_condition_type": "WetBulb",
            "maximum_dry_bulb_temperature": 33.3,
            "month": 7,
            "rain_indicator": "No",
            "snow_indicator": "No",
            "solar_model_indicator": "ASHRAETau",
            "wetbulb_or_dewpoint_at_maximum_dry_bulb": 23.7,
            "wind_direction": 230,
            "wind_speed": 5.2
        },
        "Chicago Ohare Intl Ap Ann Htg 99.6% Condns DB": {
            "barometric_pressure": 98934,
            "begin_environment_reset_mode": "FullResetAtBeginEnvironment",
            "daily_dry_bulb_temperature_range": 0,
            "day_of_month": 21,
            "day_type": "WinterDesignDay",
            "daylight_saving_time_indicator": "No",
            "dry_bulb_temperature_range_modifier_type": "DefaultMultipliers",
            "humidity_condition_type": "WetBulb",
            "maximum_dry_bulb_temperature": -20,
            "month": 1,
            "rain_indicator": "No",
            "sky_clearness": 0,
            "snow_indicator": "No",
            "solar_model_indicator": "ASHRAEClearSky",
            "wetbulb_or_dewpoint_at_maximum_dry_bulb": -20,
            "wind_direction": 270,
            "wind_speed": 4.9
        }
    },
    "Timestep": {
        "Timestep 1": {
            "number_of_timesteps_per_hour": 6
        }
    },
    "Version": {
        "Version 1": {
            "version_identifier": "22.2"
        }
    },
    "Zone": {
        "Zone1": {
            "direction_of_relative_north": 0,
            "multiplier": 1,
            "part_of_total_floor_area": "Yes",
            "x_origin": 0,
            "y_origin": 0,
            "z_origin": 0
        }
    },
    "ZoneHVAC:AirDistributionUnit": {
        "ADU Air Terminal Single Duct Constant Volume No Reheat 1": {
            "air_distribution_unit_outlet_node_name": "Node 8",
            "air_terminal_name": "Air Terminal Single Duct Constant Volume No Reheat 1",
            "air_terminal_object_type": "AirTerminal:SingleDuct:ConstantVolume:NoReheat"
        },
        "ADU Air Terminal Single Duct VAV Reheat 1": {
            "air_distribution_unit_outlet_node_name": "Node 20",
            "air_terminal_name": "Air Terminal Single Duct VAV Reheat 1",
            "air_terminal_object_type": "AirTerminal:SingleDuct:VAV:Reheat"
        }
    },
    "ZoneHVAC:EquipmentConnections": {
        "ZoneHVAC:EquipmentConnections 1": {
            "zone_air_exhaust_node_or_nodelist_name": "Zone1 Exhaust Node List",
            "zone_air_inlet_node_or_nodelist_name": "Zone1 Inlet Node List",
            "zone_air_node_name": "Node 1",
            "zone_conditioning_equipment_list_name": "Zone1 Equipment List",
            "zone_name": "Zone1",
            "zone_return_air_node_or_nodelist_name": "Zone1 Return Node List"
        }
    },
    "ZoneHVAC:EquipmentList": {
        "Zone1 Equipment List": {
            "equipment": [
                {
                    "zone_equipment_cooling_sequence": 1,
                    "zone_equipment_heating_or_no_load_sequence": 1,
                    "zone_equipment_name": "Fan Zone Exhaust 1",
                    "zone_equipment_object_type": "Fan:ZoneExhaust"
                },
                {
                    "zone_equipment_cooling_sequence": 2,
                    "zone_equipment_heating_or_no_load_sequence": 2,
                    "zone_equipment_name": "ADU Air Terminal Single Duct Constant Volume No Reheat 1",
                    "zone_equipment_object_type": "ZoneHVAC:AirDistributionUnit"
                },
                {
                    "zone_equipment_cooling_sequence": 3,
                    "zone_equipment_heating_or_no_load_sequence": 3,
                    "zone_equipment_name": "ADU Air Terminal Single Duct VAV Reheat 1",
                    "zone_equipment_object_type": "ZoneHVAC:AirDistributionUnit"
                }
            ],
            "load_distribution_scheme": "SequentialLoad"
        }
    }
}
    )"_json;

    state->dataGlobal->isEpJSON = true;
    state->dataInputProcessing->inputProcessor->initializeMaps();

    // GetZoneData and others use GetObjectItem with ipshortcuts so these need to be allocated
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

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    DataZoneEquipment::GetZoneEquipmentData(*state);

    EXPECT_EQ(1, state->dataZoneEquip->ZoneEquipList.size());
    EXPECT_EQ(3, state->dataZoneEquip->ZoneEquipList(1).NumOfEquipTypes);
}
