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

// EnergyPlus::PlantManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>
// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {
namespace PlantManager {
    using namespace DataPlant;
    using namespace DataLoopNode;
    using namespace ObjexxFCL;
    using namespace DataSizing;
    using namespace ScheduleManager;
    using namespace SetPointManager;

    TEST_F(EnergyPlusFixture, PlantManager_SizePlantLoopTest)
    {
        PlantLoop.allocate(1);
        PlantLoop(1).VolumeWasAutoSized = true;
        PlantLoop(1).MaxVolFlowRate = 5;
        PlantLoop(1).CirculationTime = 2;
        PlantLoop(1).FluidType = NodeType_Water;
        PlantLoop(1).FluidIndex = 1;
        SizePlantLoop(1, true);
        int TestVolume = 600;
        EXPECT_EQ(TestVolume, PlantLoop(1).Volume);
    }

    TEST_F(EnergyPlusFixture, PlantManager_TwoWayCommonPipeSetPointManagerTest)
    {
        // issue 6069
        bool ErrorsFound = false;

        std::string const idf_objects = delimited_string({

            "  Version,9.0;",

            "  PlantLoop,",
            "    Chilled Water Loop,      !- Name",
            "    Water,                   !- Fluid Type",
            "    ,                        !- User Defined Fluid Type",
            "    Chilled Water Loop Operation,  !- Plant Equipment Operation Scheme Name",
            "    Chilled Water Loop Supply Outlet,  !- Loop Temperature Setpoint Node Name",
            "    98,                      !- Maximum Loop Temperature {C}",
            "    1,                       !- Minimum Loop Temperature {C}",
            "    0.12396E-02,             !- Maximum Loop Flow Rate {m3/s}",
            "    0,                       !- Minimum Loop Flow Rate {m3/s}",
            "    autocalculate,           !- Plant Loop Volume {m3}",
            "    Chilled Water Loop Supply Inlet,  !- Plant Side Inlet Node Name",
            "    Chilled Water Loop Supply Outlet,  !- Plant Side Outlet Node Name",
            "    Chilled Water Loop Supply Side Branches,  !- Plant Side Branch List Name",
            "    Chilled Water Loop Supply Side Connectors,  !- Plant Side Connector List Name",
            "    Chilled Water Loop Demand Inlet,  !- Demand Side Inlet Node Name",
            "    Chilled Water Loop Demand Outlet,  !- Demand Side Outlet Node Name",
            "    Chilled Water Loop Demand Side Branches,  !- Demand Side Branch List Name",
            "    Chilled Water Loop Demand Side Connectors,  !- Demand Side Connector List Name",
            "    SequentialLoad,          !- Load Distribution Scheme",
            "    ,                        !- Availability Manager List Name",
            "    SingleSetPoint,          !- Plant Loop Demand Calculation Scheme",
            "    TwoWayCommonPipe;        !- Common Pipe Simulation",

            "  BranchList,",
            "    Chilled Water Loop Supply Side Branches,  !- Name",
            "    Chilled Water Loop Supply Inlet Branch,  !- Branch 1 Name",
            "    Main Chiller ChW Branch, !- Branch 2 Name",
            "    Chilled Water Loop Supply Bypass Branch,  !- Branch 3 Name",
            "    Chilled Water Loop Supply Outlet Branch;  !- Branch 4 Name",

            "  Branch,",
            "    Chilled Water Loop Supply Inlet Branch,  !- Name",
            "    ,                        !- Pressure Drop Curve Name",
            "    Pump:ConstantSpeed,      !- Component 1 Object Type",
            "    Chilled Water Loop Pri Supply Pump,  !- Component 1 Name",
            "    Chilled Water Loop Supply Inlet,  !- Component 1 Inlet Node Name",
            "    Chilled Water Loop Pri Pump Outlet;  !- Component 1 Outlet Node Name",

            "  Branch,",
            "    Chilled Water Loop Supply Outlet Branch,  !- Name",
            "    ,                        !- Pressure Drop Curve Name",
            "    Pipe:Adiabatic,          !- Component 1 Object Type",
            "    Chilled Water Loop Supply Outlet Pipe,  !- Component 1 Name",
            "    Chilled Water Loop Supply Outlet Pipe Inlet,  !- Component 1 Inlet Node Name",
            "    Chilled Water Loop Supply Outlet;  !- Component 1 Outlet Node Name",

            "  Pipe:Adiabatic,",
            "    Chilled Water Loop Supply Outlet Pipe,  !- Name",
            "    Chilled Water Loop Supply Outlet Pipe Inlet,  !- Inlet Node Name",
            "    Chilled Water Loop Supply Outlet;  !- Outlet Node Name",

            "  BranchList,",
            "    Chilled Water Loop Demand Side Branches,  !- Name",
            "    Chilled Water Loop Demand Inlet Branch,  !- Branch 1 Name",
            "    VAV Sys 1 Cooling Coil ChW Branch,  !- Branch 2 Name",
            "    Chilled Water Loop Demand Bypass Branch,  !- Branch 3 Name",
            "    Chilled Water Loop Demand Outlet Branch;  !- Branch 4 Name",

            "  Branch,",
            "    Chilled Water Loop Demand Inlet Branch,  !- Name",
            "    ,                        !- Pressure Drop Curve Name",
            "    Pump:VariableSpeed,      !- Component 1 Object Type",
            "    Chilled Water Loop Demand Pump,  !- Component 1 Name",
            "    Chilled Water Loop Demand Inlet,  !- Component 1 Inlet Node Name",
            "    Chilled Water Loop Demand Pump Outlet;  !- Component 1 Outlet Node Name",

            "  Branch,",
            "    Chilled Water Loop Demand Outlet Branch,  !- Name",
            "    ,                        !- Pressure Drop Curve Name",
            "    Pipe:Adiabatic,          !- Component 1 Object Type",
            "    Chilled Water Loop Demand Outlet Pipe,  !- Component 1 Name",
            "    Chilled Water Loop Demand Outlet Pipe Inlet,  !- Component 1 Inlet Node Name",
            "    Chilled Water Loop Demand Outlet;  !- Component 1 Outlet Node Name",

            "  Pipe:Adiabatic,",
            "    Chilled Water Loop Demand Outlet Pipe,  !- Name",
            "    Chilled Water Loop Demand Outlet Pipe Inlet,  !- Inlet Node Name",
            "    Chilled Water Loop Demand Outlet;  !- Outlet Node Name",

            "  Schedule:Compact,",
            "    COMPACT HVAC-ALWAYS 5.0, !- Name",
            "    COMPACT HVAC Any Number, !- Schedule Type Limits Name",
            "    Through: 12/31,          !- Field 1",
            "    For: AllDays,            !- Field 2",
            "    Until: 24:00,5.0;        !- Field 3",

            "  SetpointManager:Scheduled,",
            "    Chilled Water Primary Loop Setpoint Manager,  !- Name",
            "    Temperature,             !- Control Variable",
            "    COMPACT HVAC-ALWAYS 5.0, !- Schedule Name",
            "    Chilled Water Loop Supply Outlet;  !- Setpoint Node or NodeList Name",

            "  Schedule:Compact,",
            "    COMPACT HVAC-ALWAYS 8.00,!- Name",
            "    COMPACT HVAC Any Number, !- Schedule Type Limits Name",
            "    Through: 12/31,          !- Field 1",
            "    For: AllDays,            !- Field 2",
            "    Until: 24:00,8.00;       !- Field 3",

            " SetpointManager:OutdoorAirReset,",
            "    Chilled Water Secondary Loop Setpoint Manager,  !- Name",
            "    Temperature,             !- Control Variable",
            "    11.11,                   !- Setpoint at Outdoor Low Temperature {C}",
            "    7.22,                    !- Outdoor Low Temperature {C}",
            "    7.22,                    !- Setpoint at Outdoor High Temperature {C}",
            "    29.44,                   !- Outdoor High Temperature {C}",
            "    Chilled Water Loop Supply inlet;  !- Setpoint Node or NodeList Name",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        // get input and checks if there are two setpointmanagers
        // for a TwoWayCommonPipe and one of them setpoints can be
        // a SetpointManager:OutdoorAirReset type.
        GetPlantLoopData();
        ASSERT_FALSE(ErrorsFound);
        // there two setpoint amanegrs in the loop
        EXPECT_EQ(1, NumSchSetPtMgrs);    // SetpointManager:Scheduled
        EXPECT_EQ(1, NumOutAirSetPtMgrs); // SetpointManager:OutdoorAirReset
        EXPECT_EQ(2, NumAllSetPtMgrs);
        // Schedule Setpoint Manager assigned at a plant loop supply outlet node
        EXPECT_EQ(SchSetPtMgr(1).CtrlVarType, "TEMPERATURE");
        EXPECT_EQ(SchSetPtMgr(1).CtrlNodeListName, "CHILLED WATER LOOP SUPPLY OUTLET");
        // OAReset Setpoint Manager assigned at a plant loop supply inlet node
        EXPECT_EQ(OutAirSetPtMgr(1).CtrlVarType, "TEMPERATURE");
        EXPECT_EQ(OutAirSetPtMgr(1).CtrlNodeListName, "CHILLED WATER LOOP SUPPLY INLET");
    }
} // namespace PlantManager
} // namespace EnergyPlus
